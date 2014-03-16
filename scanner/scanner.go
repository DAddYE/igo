// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package scanner implements a scanner for Go source text.
// It takes a []byte as source which can then be tokenized
// through repeated calls to the Scan method.
//
package scanner

import (
	"bytes"
	"fmt"
	"path/filepath"
	"strconv"
	"unicode"
	"unicode/utf8"

	"github.com/daddye/igo/token"
)

// An ErrorHandler may be provided to Scanner.Init. If a syntax error is
// encountered and a handler was installed, the handler is called with a
// position and an error message. The position points to the beginning of
// the offending token.
//
type ErrorHandler func(pos token.Position, msg string)

// A Scanner holds the scanner's internal state while processing
// a given text.  It can be allocated as part of another data
// structure but must be initialized via Init before use.
//
type Scanner struct {
	// immutable state
	file *token.File  // source file handle
	dir  string       // directory portion of file.Name()
	src  []byte       // source
	err  ErrorHandler // error reporting; or nil
	mode Mode         // scanning mode

	// scanning state
	ch         rune // current character
	offset     int  // character offset
	rdOffset   int  // reading offset (position after current character)
	lineOffset int  // current line offset
	noSemi     bool // avoid terminator (will wait '\n' before reset)
	unfinished bool // avoid terminator on unfinished expressions (will wait next token before reset)

	// indent state
	indent     indent // stacks of indentation levels
	whiteWidth int    // number of consecutive white spaces at beginning of line

	// public state - ok to modify
	ErrorCount int // number of errors encountered
}

const (
	MaxIndent = 100
)

type indent struct {
	idx    int            // current indentation index
	pendin int            // track of indent/dedent
	stack  [MaxIndent]int // indent stack
	level  int            // () [] {} Parentheses nesting level, used to allow free continuations inside them
}

const bom = 0xFEFF // byte order mark, only permitted as very first character

// Read the next Unicode char into s.ch.
// s.ch < 0 means end-of-file.
//
func (s *Scanner) next() {
	if s.rdOffset < len(s.src) {
		s.offset = s.rdOffset
		if s.ch == '\n' {
			s.lineOffset = s.offset
			s.file.AddLine(s.offset)
			s.whiteWidth = 0
		}
		r, w := rune(s.src[s.rdOffset]), 1
		switch {
		case r == 0:
			s.error(s.offset, "illegal character NUL")
		case r >= 0x80:
			// not ASCII
			r, w = utf8.DecodeRune(s.src[s.rdOffset:])
			if r == utf8.RuneError && w == 1 {
				s.error(s.offset, "illegal UTF-8 encoding")
			} else if r == bom && s.offset > 0 {
				s.error(s.offset, "illegal byte order mark")
			}
		}
		s.rdOffset += w
		s.ch = r
	} else {
		s.offset = len(s.src)
		if s.ch == '\n' {
			s.lineOffset = s.offset
			s.file.AddLine(s.offset)
		}
		s.ch = -1 // eof
	}
}

// A mode value is a set of flags (or 0).
// They control scanner behavior.
//
type Mode uint

const (
	ScanComments Mode = 1 << iota // return comments as COMMENT tokens
)

// Init prepares the scanner s to tokenize the text src by setting the
// scanner at the beginning of src. The scanner uses the file set file
// for position information and it adds line information for each line.
// It is ok to re-use the same file when re-scanning the same file as
// line information which is already present is ignored. Init causes a
// panic if the file size does not match the src size.
//
// Calls to Scan will invoke the error handler err if they encounter a
// syntax error and err is not nil. Also, for each error encountered,
// the Scanner field ErrorCount is incremented by one. The mode parameter
// determines how comments are handled.
//
// Note that Init may call err if there is an error in the first character
// of the file.
//
func (s *Scanner) Init(file *token.File, src []byte, err ErrorHandler, mode Mode) {
	// Explicitly initialize all fields since a scanner may be reused.
	if file.Size() != len(src) {
		panic(fmt.Sprintf("file size (%d) does not match src len (%d)", file.Size(), len(src)))
	}
	s.file = file
	s.dir, _ = filepath.Split(file.Name())
	s.src = src
	s.err = err
	s.mode = mode

	s.ch = ' '
	s.offset = 0
	s.rdOffset = 0
	s.lineOffset = 0
	s.ErrorCount = 0

	s.next()
	if s.ch == bom {
		s.next() // ignore BOM at file beginning
	}
}

func (s *Scanner) error(offs int, msg string) {
	if s.err != nil {
		s.err(s.file.Position(s.file.Pos(offs)), msg)
	}
	s.ErrorCount++
}

var prefix = []byte("#line ")

func (s *Scanner) interpretLineComment(text []byte) {
	if bytes.HasPrefix(text, prefix) {
		// get filename and line number, if any
		if i := bytes.LastIndex(text, []byte{':'}); i > 0 {
			if line, err := strconv.Atoi(string(text[i+1:])); err == nil && line > 0 {
				// valid #line filename:line comment;
				filename := filepath.Clean(string(text[len(prefix):i]))
				if !filepath.IsAbs(filename) {
					// make filename relative to current directory
					filename = filepath.Join(s.dir, filename)
				}
				// update scanner position
				s.file.AddLineInfo(s.lineOffset+len(text)+1, filename, line) // +len(text)+1 since comment applies to next line
			}
		}
	}
}

func (s *Scanner) scanComment() string {
	// initial '#' already consumed
	offs := s.offset - 1 // position of '#'
	hasCR := false

	if s.ch == '\r' {
		hasCR = true
	}

	for s.ch != '\n' && s.ch >= 0 {
		if s.ch == '\r' {
			hasCR = true
		}
		s.next()
	}

	if offs == s.lineOffset {
		// comment starts at the beginning of the current line
		s.interpretLineComment(s.src[offs:s.offset])
	}

	lit := s.src[offs:s.offset]

	if hasCR {
		lit = stripCR(lit)
	}

	return string(lit)
}

func isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_' || ch >= 0x80 && unicode.IsLetter(ch)
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9' || ch >= 0x80 && unicode.IsDigit(ch)
}

func (s *Scanner) scanIdentifier() string {
	offs := s.offset
	for isLetter(s.ch) || isDigit(s.ch) {
		s.next()
	}
	return string(s.src[offs:s.offset])
}

func digitVal(ch rune) int {
	switch {
	case '0' <= ch && ch <= '9':
		return int(ch - '0')
	case 'a' <= ch && ch <= 'f':
		return int(ch - 'a' + 10)
	case 'A' <= ch && ch <= 'F':
		return int(ch - 'A' + 10)
	}
	return 16 // larger than any legal digit val
}

func (s *Scanner) scanMantissa(base int) {
	for digitVal(s.ch) < base {
		s.next()
	}
}

func (s *Scanner) scanNumber(seenDecimalPoint bool) (token.Token, string) {
	// digitVal(s.ch) < 10
	offs := s.offset
	tok := token.INT

	if seenDecimalPoint {
		offs--
		tok = token.FLOAT
		s.scanMantissa(10)
		goto exponent
	}

	if s.ch == '0' {
		// int or float
		offs := s.offset
		s.next()
		if s.ch == 'x' || s.ch == 'X' {
			// hexadecimal int
			s.next()
			s.scanMantissa(16)
			if s.offset-offs <= 2 {
				// only scanned "0x" or "0X"
				s.error(offs, "illegal hexadecimal number")
			}
		} else {
			// octal int or float
			seenDecimalDigit := false
			s.scanMantissa(8)
			if s.ch == '8' || s.ch == '9' {
				// illegal octal int or float
				seenDecimalDigit = true
				s.scanMantissa(10)
			}
			if s.ch == '.' || s.ch == 'e' || s.ch == 'E' || s.ch == 'i' {
				goto fraction
			}
			// octal int
			if seenDecimalDigit {
				s.error(offs, "illegal octal number")
			}
		}
		goto exit
	}

	// decimal int or float
	s.scanMantissa(10)

fraction:
	if s.ch == '.' {
		tok = token.FLOAT
		s.next()
		s.scanMantissa(10)
	}

exponent:
	if s.ch == 'e' || s.ch == 'E' {
		tok = token.FLOAT
		s.next()
		if s.ch == '-' || s.ch == '+' {
			s.next()
		}
		s.scanMantissa(10)
	}

	if s.ch == 'i' {
		tok = token.IMAG
		s.next()
	}

exit:
	return tok, string(s.src[offs:s.offset])
}

func (s *Scanner) scanEscape(quote rune) {
	offs := s.offset

	var i, base, max uint32
	switch s.ch {
	case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', quote:
		s.next()
		return
	case '0', '1', '2', '3', '4', '5', '6', '7':
		i, base, max = 3, 8, 255
	case 'x':
		s.next()
		i, base, max = 2, 16, 255
	case 'u':
		s.next()
		i, base, max = 4, 16, unicode.MaxRune
	case 'U':
		s.next()
		i, base, max = 8, 16, unicode.MaxRune
	default:
		s.next() // always make progress
		s.error(offs, "unknown escape sequence")
		return
	}

	var x uint32
	for ; i > 0 && s.ch != quote && s.ch >= 0; i-- {
		d := uint32(digitVal(s.ch))
		if d >= base {
			s.error(s.offset, "illegal character in escape sequence")
			break
		}
		x = x*base + d
		s.next()
	}
	// in case of an error, consume remaining chars
	for ; i > 0 && s.ch != quote && s.ch >= 0; i-- {
		s.next()
	}
	if x > max || 0xD800 <= x && x < 0xE000 {
		s.error(offs, "escape sequence is invalid Unicode code point")
	}
}

func (s *Scanner) scanChar() string {
	// '\'' opening already consumed
	offs := s.offset - 1

	n := 0
	for s.ch != '\'' {
		ch := s.ch
		n++
		s.next()
		if ch == '\n' || ch < 0 {
			s.error(offs, "character literal not terminated")
			n = 1
			break
		}
		if ch == '\\' {
			s.scanEscape('\'')
		}
	}

	s.next()

	if n != 1 {
		s.error(offs, "illegal character literal")
	}

	return string(s.src[offs:s.offset])
}

func (s *Scanner) scanString() string {
	// '"' opening already consumed
	offs := s.offset - 1

	for s.ch != '"' {
		ch := s.ch
		s.next()
		if ch == '\n' || ch < 0 {
			s.error(offs, "string not terminated")
			break
		}
		if ch == '\\' {
			s.scanEscape('"')
		}
	}

	s.next()

	return string(s.src[offs:s.offset])
}

func stripCR(b []byte) []byte {
	c := make([]byte, len(b))
	i := 0
	for _, ch := range b {
		if ch != '\r' {
			c[i] = ch
			i++
		}
	}
	return c[:i]
}

func (s *Scanner) scanRawString() string {
	// '`' opening already consumed
	offs := s.offset - 1

	hasCR := false
	for s.ch != '`' {
		ch := s.ch
		s.next()
		if ch == '\r' {
			hasCR = true
		}
		if ch < 0 {
			s.error(offs, "string not terminated")
			break
		}
	}

	s.next()

	lit := s.src[offs:s.offset]
	if hasCR {
		lit = stripCR(lit)
	}

	return string(lit)
}

func (s *Scanner) scanRawString2() string {
	// '"' opening already consumed
	offs := s.offset - 1
	hasCR := false
	terminated := false

	// we don't need an if here since the `""` condition was
	// made in the switch of Scan()
	s.next() // ""

	// If are 3 `"`, `"""` it's really a raw string
	if s.ch == '"' {
		s.next()
		for s.ch >= 0 {
			ch := s.ch
			if ch == '\r' {
				hasCR = true
			}
			s.next()
			// check if we are at end of comment
			if ch == '"' && s.ch == '"' {
				s.next()
				if s.ch == '"' {
					terminated = true
					s.next()
					goto exit
				}
			}
		}
	} else {
		// we are already good with an empty string
		terminated = true
	}

	if !terminated {
		s.error(offs, "string not terminated")
	}

exit:
	lit := s.src[offs:s.offset]
	if hasCR {
		lit = stripCR(lit)
	}
	return string(lit)
}

// This allows '\n' since is needed for indenting tracks
func (s *Scanner) cleanCRLF() {
	for s.ch == '\n' || s.ch == '\r' {
		s.next()
	}
}

func (s *Scanner) skipWhitespace() {
	bol := s.offset == s.lineOffset
	for s.ch == ' ' || s.ch == '\t' {
		s.next()
		if bol {
			s.whiteWidth++
		}
	}
}

// Helper functions for scanning multi-byte tokens such as >> += >>= .
// Different routines recognize different length tok_i based on matches
// of ch_i. If a token ends in '=', the result is tok1 or tok3
// respectively. Otherwise, the result is tok0 if there was no other
// matching character, or tok2 if the matching character was ch2.

func (s *Scanner) switch2(tok0, tok1 token.Token) token.Token {
	if s.ch == '=' {
		s.next()
		return tok1
	}
	return tok0
}

func (s *Scanner) switch3(tok0, tok1 token.Token, ch2 rune, tok2 token.Token) token.Token {
	if s.ch == '=' {
		s.next()
		return tok1
	}
	if s.ch == ch2 {
		s.next()
		return tok2
	}
	return tok0
}

func (s *Scanner) switch4(tok0, tok1 token.Token, ch2 rune, tok2, tok3 token.Token) token.Token {
	if s.ch == '=' {
		s.next()
		return tok1
	}
	if s.ch == ch2 {
		s.next()
		if s.ch == '=' {
			s.next()
			return tok3
		}
		return tok2
	}
	return tok0
}

// Scan scans the next token and returns the token position, the token,
// and its literal string if applicable. The source end is indicated by
// token.EOF.
//
// If the returned token is a literal (token.IDENT, token.INT, token.FLOAT,
// token.IMAG, token.CHAR, token.STRING) or token.COMMENT, the literal string
// has the corresponding value.
//
// If the returned token is a keyword, the literal string is the keyword.
//
// If the returned token is token.SEMICOLON, the corresponding
// literal string is ";" if the semicolon was present in the source,
// and "\n" if the semicolon was inserted because of a newline or
// at EOF.
//
// If the returned token is token.ILLEGAL, the literal string is the
// offending character.
//
// In all other cases, Scan returns an empty literal string.
//
// For more tolerant parsing, Scan will return a valid token if
// possible even if a syntax error was encountered. Thus, even
// if the resulting token sequence contains no illegal tokens,
// a client may not assume that no error occurred. Instead it
// must check the scanner's ErrorCount or the number of calls
// of the error handler, if there was one installed.
//
// Scan adds line information to the file added to the file
// set with Init. Token positions are relative to that file
// and thus relative to the file set.
//
func (s *Scanner) Scan() (pos token.Pos, tok token.Token, lit string) {
newLine:
	blankLine := false

	if s.offset == s.lineOffset {

		cl := 0 // current level

		for {
			if s.ch == '\t' {
				cl += 2 // TODO: use (level/tabsize + 1) * tabsize
			} else if s.ch == ' ' {
				cl++
			} else {
				break
			}
			s.whiteWidth++
			s.next()
		}

		blankLine = s.ch == '#' || s.ch == '\n'

		// If we are not inside [](){}
		// Comments '#' or empty lines, should not affect indentation
		if s.indent.level == 0 && !blankLine && !s.unfinished {
			switch {
			case cl == s.indent.stack[s.indent.idx]:
				// noting to do
			case cl > s.indent.stack[s.indent.idx]:
				s.indent.idx++
				s.indent.pendin++
				s.indent.stack[s.indent.idx] = cl
			default:
				for s.indent.idx > 0 && cl < s.indent.stack[s.indent.idx] {
					s.indent.pendin--
					s.indent.idx--
				}
				if cl != s.indent.stack[s.indent.idx] {
					s.error(s.offset, "incosistent indentation")
				}
			}
		}
	}

	// current token start
	pos = s.file.Pos(s.offset)

	switch {
	case s.indent.pendin < 0:
		s.indent.pendin++
		return pos - 1, token.DEDENT, "}"
	case s.indent.pendin > 0:
		s.indent.pendin--
		return pos, token.INDENT, "{"
	}

scanAgain:

	s.skipWhitespace()

	// determine token value
	switch ch := s.ch; {
	case isLetter(ch):
		lit = s.scanIdentifier()
		if len(lit) > 1 {
			// keywords are longer than one letter - avoid lookup otherwise
			tok = token.Lookup(lit)
			switch tok {
			case token.CASE, token.DEFAULT:
				s.unfinished = true
				return
			}
		} else {
			tok = token.IDENT
		}
	case '0' <= ch && ch <= '9':
		tok, lit = s.scanNumber(false)
	default:
		s.next() // always make progress
		switch ch {
		case -1:
			for s.indent.idx > 0 {
				s.indent.idx--
				return pos, token.DEDENT, "}"
			}
			tok = token.EOF
		case '\n':
			if blankLine || s.indent.level > 0 {
				goto newLine
			}
			if s.noSemi {
				s.noSemi = false
				goto newLine
			}
			if s.unfinished {
				goto newLine
			}
			return pos, token.SEMICOLON, "\n"
		case '"':
			if s.ch == '"' {
				tok = token.STRING
				lit = s.scanRawString2()
			} else {
				tok = token.STRING
				lit = s.scanString()
			}
		case '\'':
			tok = token.CHAR
			lit = s.scanChar()
		case '`':
			tok = token.STRING
			lit = s.scanRawString()
		case ':':
			tok = s.switch2(token.COLON, token.DEFINE)
			if tok == token.DEFINE {
				s.unfinished = true
				return
			}
		case '.':
			if '0' <= s.ch && s.ch <= '9' {
				tok, lit = s.scanNumber(true)
			} else if s.ch == '.' {
				s.next()
				if s.ch == '.' {
					s.next()
					tok = token.ELLIPSIS
				}
			} else {
				tok = token.PERIOD
			}
		case ',':
			tok = token.COMMA
			s.unfinished = true
			return
		case ';':
			tok = token.SEMICOLON
			lit = ";"
		case '(':
			s.indent.level++
			tok = token.LPAREN
		case ')':
			s.indent.level--
			tok = token.RPAREN
		case '[':
			s.indent.level++
			tok = token.LBRACK
		case ']':
			s.indent.level--
			tok = token.RBRACK
		case '{':
			s.indent.level++
			tok = token.LBRACE
		case '}':
			s.indent.level--
			tok = token.RBRACE
		case '+':
			tok = s.switch3(token.ADD, token.ADD_ASSIGN, '+', token.INC)
			s.unfinished = tok != token.INC
			return
		case '-':
			tok = s.switch3(token.SUB, token.SUB_ASSIGN, '-', token.DEC)
			s.unfinished = tok != token.DEC
			return
		case '*':
			tok = s.switch2(token.MUL, token.MUL_ASSIGN)
			s.unfinished = true
			return
		case '#':
			// comment
			s.noSemi = s.file.Offset(pos)-s.whiteWidth == s.lineOffset // start a beginning of line
			lit = s.scanComment()
			if s.mode&ScanComments == 0 {
				// skip comment
				goto scanAgain
			}
			tok = token.COMMENT
			// Should not affect the status of the 'line'
			if s.unfinished {
				return
			}
		case '/':
			tok = s.switch2(token.QUO, token.QUO_ASSIGN)
			s.unfinished = true
			return
		case '%':
			tok = s.switch2(token.REM, token.REM_ASSIGN)
			s.unfinished = true
			return
		case '^':
			tok = s.switch2(token.XOR, token.XOR_ASSIGN)
			s.unfinished = true
			return
		case '<':
			if s.ch == '-' {
				s.next()
				tok = token.ARROW
			} else {
				tok = s.switch4(token.LSS, token.LEQ, '<', token.SHL, token.SHL_ASSIGN)
			}
			s.unfinished = true
			return
		case '>':
			tok = s.switch4(token.GTR, token.GEQ, '>', token.SHR, token.SHR_ASSIGN)
			s.unfinished = true
			return
		case '=':
			tok = s.switch2(token.ASSIGN, token.EQL)
			s.unfinished = true
			return
		case '!':
			tok = s.switch2(token.NOT, token.NEQ)
			s.unfinished = true
			return
		case '&':
			if s.ch == '^' {
				s.next()
				tok = s.switch2(token.AND_NOT, token.AND_NOT_ASSIGN)
			} else {
				tok = s.switch3(token.AND, token.AND_ASSIGN, '&', token.LAND)
			}
			s.unfinished = true
			return
		case '|':
			tok = s.switch3(token.OR, token.OR_ASSIGN, '|', token.LOR)
			s.unfinished = true
			return
		default:
			// next reports unexpected BOMs - don't repeat
			if ch != bom {
				s.error(s.file.Offset(pos), fmt.Sprintf("illegal character %#U", ch))
			}
			tok = token.ILLEGAL
			lit = string(ch)
		}
	}
	s.unfinished = false
	return
}
