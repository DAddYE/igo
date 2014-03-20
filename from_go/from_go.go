// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package printer implements printing of AST nodes.
package from_go

import (
	"fmt"
	iToken "github.com/DAddYE/igo/token"
	"go/ast"
	"go/token"
	"io"
	"os"
	"strconv"
	"strings"
	"text/tabwriter"
	"unicode"
)

const (
	maxNewlines = 2     // max. number of newlines between source text
	debug       = false // enable for debugging
	infinity    = 1 << 30
)

type whiteSpace byte

const (
	ignore   = whiteSpace(0)
	blank    = whiteSpace(' ')
	vtab     = whiteSpace('\v')
	newline  = whiteSpace('\n')
	formfeed = whiteSpace('\f')
	indent   = whiteSpace('>')
	unindent = whiteSpace('<')
)

// A pmode value represents the current printer mode.
type pmode int

const (
	noExtraLinebreak pmode = 1 << iota
)

type printer struct /* Configuration (does not change after initialization)  */ 
{
	Config
	fset *token.FileSet

	// Current state
	output      []byte       // raw printer result
	indent      int          // current indentation
	mode        pmode        // current printer mode
	impliedSemi bool         // if set, a linebreak implies a semicolon
	lastTok     token.Token  // the last token printed (token.ILLEGAL if it's whitespace)
	wsbuf       []whiteSpace // delayed white space
	findent     int          // function indentation idx
	inFunc      bool         // track if we are in a function call

	// Positions
	// The out position differs from the pos position when the result
	// formatting differs from the source formatting (in the amount of
	// white space). If there's a difference and SourcePos is set in
	// ConfigMode, //line comments are used in the output to restore
	// original source positions for a reader.
	pos  token.Position // current position in AST (source) space
	out  token.Position // current position in output space
	last token.Position // value of pos after calling writeString

	// The list of all source comments, in order of appearance.
	comments        []*ast.CommentGroup // may be nil
	cindex          int                 // current comment index
	useNodeComments bool                // if not set, ignore lead and line comments of nodes

	// Information about p.comments[p.cindex]; set up by nextComment.
	comment        *ast.CommentGroup // = p.comments[p.cindex]; or nil
	commentOffset  int               // = p.posFor(p.comments[p.cindex].List[0].Pos()).Offset; or infinity
	commentNewline bool              // true if the comment group contains newlines

	// Cache of already computed node sizes.
	nodeSizes map[ast.Node]int

	// Cache of most recently computed line position.
	cachedPos  token.Pos
	cachedLine int // line corresponding to cachedPos

	// Set the function scope to allow identifier change
	rcvName *ast.Ident // the name of the receiver
}

func (self *printer) init(cfg *Config, fset *token.FileSet, nodeSizes map[ast.Node]int) {
	self.Config = *cfg
	self.fset = fset
	self.pos = token.Position{Line: 1, Column: 1}
	self.out = token.Position{Line: 1, Column: 1}
	self.wsbuf = make([]whiteSpace, 0, 16) // whitespace sequences are short
	self.nodeSizes = nodeSizes
	self.cachedPos = -1
}

// commentsHaveNewline reports whether a list of comments belonging to
// an *ast.CommentGroup contains newlines. Because the position information
// may only be partially correct, we also have to read the comment text.
func (self *printer) commentsHaveNewline(list []*ast.Comment) bool { // len(list) > 0
	line := self.lineFor(list[0].Pos())
	for i, c := range list {
		if i > 0 && self.lineFor(list[i].Pos()) != line { // not all comments on the same line
			return true

		}
		if t := c.Text; len(t) >= 2 && (t[1] == '/' || strings.Contains(t, "\n")) {
			return true

		}
	}
	_ = line
	return false

}
func (self *printer) nextComment() {
	for self.cindex < len(self.comments) {
		c := self.comments[self.cindex]
		self.cindex++
		if list := c.List; len(list) > 0 {
			self.comment = c
			self.commentOffset = self.posFor(list[0].Pos()).Offset
			self.commentNewline = self.commentsHaveNewline(list)
			return

			// we should not reach here (correct ASTs don't have empty
			// ast.CommentGroup nodes), but be conservative and try again

		} // no more comments
	}
	self.commentOffset = infinity

}
func (self *printer) internalError(msg ...interface{}) {
	if debug {
		fmt.Print(self.pos.String() + ": ")
		fmt.Println(msg...)
		panic("github.com/DAddYE/igo/from_go")

	}
}
func (self *printer) posFor(pos token.Pos) token.Position { // not used frequently enough to cache entire token.Position
	return self.fset.Position(pos)

}
func (self *printer) lineFor(pos token.Pos) int {
	if pos != self.cachedPos {
		self.cachedPos = pos
		self.cachedLine = self.fset.Position(pos).Line

	}
	return self.cachedLine
}

// atLineBegin emits a //line comment if necessary and prints indentation.
func (self *printer) atLineBegin(pos token.Position) { // write a //line comment if necessary
	if self.Config.Mode&SourcePos != 0 && pos.IsValid() && (self.out.Line != pos.Line || self.out.Filename != pos.Filename) {
		self.output = append(self.output, tabwriter.Escape) // protect '\n' in //line from tabwriter interpretation
		self.output = append(self.output, fmt.Sprintf("#line %s:%d\n", pos.Filename, pos.Line)...)
		self.output = append(self.output, tabwriter.Escape)
		// p.out must match the //line comment
		self.out.Filename = pos.Filename
		self.out.Line = pos.Line

	} // write indentation
	// use "hard" htabs - indentation columns
	// must not be discarded by the tabwriter
	n := self.Config.Indent + self.indent // include base indentation
	for i := 0; i < n; i++ {
		self.output = append(self.output, '\t')

	} // update positions
	self.pos.Offset += n
	self.pos.Column += n
	self.out.Column += n
}

// writeByte writes ch n times to p.output and updates p.pos.
func (self *printer) writeByte(ch byte, n int) {
	if self.out.Column == 1 {
		self.atLineBegin(self.pos)

	}
	for i := 0; i < n; i++ {
		self.output = append(self.output, ch)

	} // update positions
	self.pos.Offset += n
	if ch == '\n' || ch == '\f' {
		self.pos.Line += n
		self.out.Line += n
		self.pos.Column = 1
		self.out.Column = 1
		return

	}
	self.pos.Column += n
	self.out.Column += n
}

// writeString writes the string s to p.output and updates p.pos, p.out,
// and p.last. If isLit is set, s is escaped w/ tabwriter.Escape characters
// to protect s from being interpreted by the tabwriter.
//
// Note: writeString is only used to write Go tokens, literals, and
// comments, all of which must be written literally. Thus, it is correct
// to always set isLit = true. However, setting it explicitly only when
// needed (i.e., when we don't know that s contains no tabs or line breaks)
// avoids processing extra escape characters and reduces run time of the
// printer benchmark by up to 10%.
//
func (self *printer) writeString(pos token.Position, s string, isLit bool) {
	if self.out.Column == 1 {
		self.atLineBegin(pos)

	}
	if pos.IsValid() { // update p.pos (if pos is invalid, continue with existing p.pos)
		// Note: Must do this after handling line beginnings because
		// atLineBegin updates p.pos if there's indentation, but p.pos
		// is the position of s.
		self.pos = pos

	}
	if isLit { // Protect s such that is passes through the tabwriter
		// unchanged. Note that valid Go programs cannot contain
		// tabwriter.Escape bytes since they do not appear in legal
		// UTF-8 sequences.
		self.output = append(self.output, tabwriter.Escape)

	} // if debug {
	// 	p.output = append(p.output, fmt.Sprintf("/*%s*/", pos)...) // do not update p.pos!
	// }
	self.output = append(self.output, s...)

	// update positions
	nlines := 0
	var li int                    // index of last newline; valid if nlines > 0
	for i := 0; i < len(s); i++ { // Go tokens cannot contain '\f' - no need to look for it
		if s[i] == '\n' {
			nlines++
			li = i

		}
	}
	self.pos.Offset += len(s)
	if nlines > 0 {
		self.pos.Line += nlines
		self.out.Line += nlines
		c := len(s) - li
		self.pos.Column = c
		self.out.Column = c
	} else {
		self.pos.Column += len(s)
		self.out.Column += len(s)

	}
	if isLit {
		self.output = append(self.output, tabwriter.Escape)

	}
	self.last = self.pos
}

// writeCommentPrefix writes the whitespace before a comment.
// If there is any pending whitespace, it consumes as much of
// it as is likely to help position the comment nicely.
// pos is the comment position, next the position of the item
// after all pending comments, prev is the previous comment in
// a group of comments (or nil), and tok is the next token.
//
func (self *printer) writeCommentPrefix(pos, next token.Position, prev, comment *ast.Comment, tok token.Token) {
	if len(self.output) == 0 { // the comment is the first item to be printed - don't write any whitespace
		return

	}
	if pos.IsValid() && pos.Filename != self.last.Filename { // comment in a different file - separate with newlines
		self.writeByte('\f', maxNewlines)
		return

	}
	if pos.Line == self.last.Line && (prev == nil || prev.Text[1] != '/') { // comment on the same line as last item:
		// separate with at least one separator
		hasSep := false
		if prev == nil { // first comment of a comment group
			j := 0
			for i, ch := range self.wsbuf {
				switch ch {
				case blank:
					// ignore any blanks before a comment
					self.wsbuf[i] = ignore
					continue

				case vtab:
					// respect existing tabs - important
					// for proper formatting of commented structs
					hasSep = true
					continue

				case indent:
					// apply pending indentation
					continue



				}
				j = i
				break

			}
			self.writeWhitespace(j)

		} // make sure there is at least one separator
		if !hasSep {
			sep := byte('\t')
			if pos.Line == next.Line { // next item is on the same line as the comment
				// (which must be a /*-style comment): separate
				// with a blank instead of a tab
				sep = ' '

			}
			self.writeByte(sep, 1)

		}
	} else { // comment on a different line:
		// separate with at least one line break
		droppedLinebreak := false
		j := 0
		for i, ch := range self.wsbuf {
			switch ch {
			case blank, vtab:
				// ignore any horizontal whitespace before line breaks
				self.wsbuf[i] = ignore
				continue

			case indent:
				// apply pending indentation
				continue

			case unindent:
				// if this is not the last unindent, apply it
				// as it is (likely) belonging to the last
				// construct (e.g., a multi-line expression list)
				// and is not part of closing a block
				if i+1 < len(self.wsbuf) && self.wsbuf[i+1] == unindent {
					continue

				} // if the next token is not a closing }, apply the unindent
				// if it appears that the comment is aligned with the
				// token; otherwise assume the unindent is part of a
				// closing block and stop (this scenario appears with
				// comments before a case label where the comments
				// apply to the next case instead of the current one)
				if tok != token.RBRACE && pos.Column == next.Column {
					continue

				}

			case newline, formfeed:

				self.wsbuf[i] = ignore
				droppedLinebreak = prev == nil // record only if first comment of a group

			}
			j = i
			break

		}
		self.writeWhitespace(j)

		// determine number of linebreaks before the comment
		n := 0
		if pos.IsValid() && self.last.IsValid() {
			n = pos.Line - self.last.Line
			if n < 0 /* should never happen  */ {
				n = 0

			} // at the package scope level only (p.indent == 0),
			// add an extra newline if we dropped one before:
			// this preserves a blank line before documentation
			// comments at the package scope level (issue 2570)
		}
		if self.indent == 0 && droppedLinebreak {
			n++

		} // make sure there is at least one line break
		// if the previous comment was a line comment
		if n == 0 && prev != nil && prev.Text[1] == '/' {
			n = 1

		}
		if n > 0 { // use formfeeds to break columns before a comment;
			// this is analogous to using formfeeds to separate
			// individual lines of /*-style comments
			self.writeByte('\f', nlimit(n))

		} // Returns true if s contains only white space
		// (only tabs and blanks can appear in the printer's context).
		//
	}
}
func isBlank(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] > ' ' {
			return false

		}
	}
	return true
}

// trimRight returns s with trailing whitespace removed.
func trimRight(s string) string {
	return strings.TrimRightFunc(s, unicode.IsSpace)

}
func trimSuffix(s string) string {
	return strings.TrimRightFunc(s, func(r rune) bool {
		switch r {
		case '*', '/':

			return true

		default:

			return false



		}
	})

}
func trimPrefix(s string) string {
	return strings.TrimLeftFunc(s, func(r rune) bool {
		switch r {
		case '/', '*':

			return true

		default:

			return false

			// stripCommonPrefix removes a common prefix from /*-style comment lines (unless no
			// comment line is indented, all but the first line have some form of space prefix).
			// The prefix is computed using heuristics such that is likely that the comment
			// contents are nicely laid out after re-printing each line using the printer's
			// current indentation.
			//
		}
	})

}
func stripCommonPrefix(lines []string) {
	for i, line := range lines {
		line = trimPrefix(line)
		line = trimRight(line)
		line = trimSuffix(line)
		lines[i] = "#" + line

	}
}
func (self *printer) writeComment(comment *ast.Comment) {
	text := comment.Text
	pos := self.posFor(comment.Pos())

	const linePrefix = "#line "
	if strings.HasPrefix(text, linePrefix) && (!pos.IsValid() || pos.Column == 1) { // possibly a line directive
		ldir := strings.TrimSpace(text[len(linePrefix):])
		if i := strings.LastIndex(ldir, ":"); i >= 0 {
			if line, err := strconv.Atoi(ldir[i+1:]); err == nil && line > 0 { // The line directive we are about to print changed
				// the Filename and Line number used for subsequent
				// tokens. We have to update our AST-space position
				// accordingly and suspend indentation temporarily.
				indent := self.indent
				self.indent = 0
				defer func() {
					self.pos.Filename = ldir[:i]
					self.pos.Line = line
					self.pos.Column = 1
					self.indent = indent
				}()

			} // shortcut common case of //-style comments
		}
	}
	if text[1] == '/' {
		text := "#" + text[2:]
		self.writeString(pos, trimRight(text), true)
		return

	} // for /*-style comments, print line by line and let the
	// write function take care of the proper indentation
	lines := strings.Split(text, "\n")

	// The comment started in the first column but is going
	// to be indented. For an idempotent result, add indentation
	// to all lines such that they look like they were indented
	// before - this will make sure the common prefix computation
	// is the same independent of how many times formatting is
	// applied (was issue 1835).
	if pos.IsValid() && pos.Column == 1 && self.indent > 0 {
		for i, line := range lines[1:] {
			lines[1+i] = "   " + line

		}
	}
	stripCommonPrefix(lines)

	// write comment lines, separated by formfeed,
	// without a line break after the last line
	for i, line := range lines {
		if i > 0 {
			self.writeByte('\f', 1)
			pos = self.pos

		}
		if len(line) > 0 {
			self.writeString(pos, trimRight(line), true)

		} // writeCommentSuffix writes a line break after a comment if indicated
		// and processes any leftover indentation information. If a line break
		// is needed, the kind of break (newline vs formfeed) depends on the
		// pending whitespace. The writeCommentSuffix result indicates if a
		// newline was written or if a formfeed was dropped from the whitespace
		// buffer.
		//
	}
}
func (self *printer) writeCommentSuffix(needsLinebreak bool) (wroteNewline, droppedFF bool) {
	for i, ch := range self.wsbuf {
		switch ch {
		case blank, vtab:
			// ignore trailing whitespace
			self.wsbuf[i] = ignore

		case indent, unindent:
			// don't lose indentation information
		case newline, formfeed:
			// if we need a line break, keep exactly one
			// but remember if we dropped any formfeeds
			if needsLinebreak {
				needsLinebreak = false
				wroteNewline = true
			} else {
				if ch == formfeed {
					droppedFF = true

				}
				self.wsbuf[i] = ignore

			}

		}
	}
	self.writeWhitespace(len(self.wsbuf))

	// make sure we have a line break
	if needsLinebreak {
		self.writeByte('\n', 1)
		wroteNewline = true

	}
	return
}

// intersperseComments consumes all comments that appear before the next token
// tok and prints it together with the buffered whitespace (i.e., the whitespace
// that needs to be written before the next token). A heuristic is used to mix
// the comments and whitespace. The intersperseComments result indicates if a
// newline was written or if a formfeed was dropped from the whitespace buffer.
//
func (self *printer) intersperseComments(next token.Position, tok token.Token) (wroteNewline, droppedFF bool) {
	var last *ast.Comment
	for self.commentBefore(next) {
		for _, c := range self.comment.List { // if the last comment is a /*-style comment and the next item
			// follows on the same line but is not a comma or a "closing"
			// token, drop it
			if c.Text[1] == '*' && self.lineFor(c.Pos()) == next.Line {
				self.writeByte(' ', 1)
				continue

			}
			self.writeCommentPrefix(self.posFor(c.Pos()), next, last, c, tok)
			self.writeComment(c)
			last = c

		}
		self.nextComment()

	}
	if last != nil { // ensure that there is a line break after a //-style comment,
		// before a closing '}' unless explicitly disabled, or at eof
		needsLinebreak :=
			last.Text[1] == '/' ||
				tok == token.RBRACE && self.mode&noExtraLinebreak == 0 ||
				tok == token.EOF
		return self.writeCommentSuffix(needsLinebreak)

	} // no comment was written - we should never reach here since
	// intersperseComments should not be called in that case
	self.internalError("intersperseComments called without pending comments")
	return
}

// whiteWhitespace writes the first n whitespace entries.
func (self *printer) writeWhitespace(n int) { // write entries
	for i := 0; i < n; i++ {
		switch ch := self.wsbuf[i]; ch {
		case ignore:
			// ignore!
		case indent:

			self.indent++

		case unindent:

			self.indent--
			if self.indent < 0 {
				self.internalError("negative indentation:", self.indent)
				self.indent = 0

			}

		case newline, formfeed:
			// A line break immediately followed by a "correcting"
			// unindent is swapped with the unindent - this permits
			// proper label positioning. If a comment is between
			// the line break and the label, the unindent is not
			// part of the comment whitespace prefix and the comment
			// will be positioned correctly indented.
			if i+1 < n && self.wsbuf[i+1] == unindent { // Use a formfeed to terminate the current section.
				// Otherwise, a long label name on the next line leading
				// to a wide column may increase the indentation column
				// of lines before the label; effectively leading to wrong
				// indentation.
				self.wsbuf[i], self.wsbuf[i+1] = unindent, formfeed
				i-- // do it again
				continue

			}
			fallthrough

		default:

			self.writeByte(byte(ch), 1)

			// shift remaining entries down
		}
	}
	i := 0
	for ; n < len(self.wsbuf); n++ {
		self.wsbuf[i] = self.wsbuf[n]
		i++

	}
	self.wsbuf = self.wsbuf[0:i]
}

// ----------------------------------------------------------------------------
// Printing interface

// nlines limits n to maxNewlines.
func nlimit(n int) int {
	if n > maxNewlines {
		n = maxNewlines

	}
	return n

}
func mayCombine(prev token.Token, next byte) (b bool) {
	switch prev {
	case token.INT:

		b = next == '.' // 1.
	case token.ADD:

		b = next == '+' // ++
	case token.SUB:

		b = next == '-' // --
	case token.QUO:

		b = next == '*' // /*
	case token.LSS:

		b = next == '-' || next == '<' // <- or <<
	case token.AND:

		b = next == '&' || next == '^' // && or &^

	}
	return
}

// print prints a list of "items" (roughly corresponding to syntactic
// tokens, but also including whitespace and formatting information).
// It is the only print function that should be called directly from
// any of the AST printing functions in nodes.go.
//
// Whitespace is accumulated until a non-whitespace token appears. Any
// comments that need to appear before that token are printed first,
// taking into account the amount and structure of any pending white-
// space for best comment placement. Then, any leftover whitespace is
// printed, followed by the actual token.
//
func (self *printer) print(args ...interface{}) {
	for _, arg := range args { // information about the current arg
		var data string
		var isLit bool
		var impliedSemi bool // value for p.impliedSemi after this arg

		switch x := arg.(type) {
		case pmode:
			// toggle printer mode
			self.mode ^= x
			continue



		case whiteSpace:

			if x == ignore { // don't add ignore's to the buffer; they
				// may screw up "correcting" unindents (see
				// LabeledStmt)
				continue

			}
			i := len(self.wsbuf)
			if i == cap(self.wsbuf) { // Whitespace sequences are very short so this should
				// never happen. Handle gracefully (but possibly with
				// bad comment placement) if it does happen.
				self.writeWhitespace(i)
				i = 0

			}
			self.wsbuf = self.wsbuf[0 : i+1]
			self.wsbuf[i] = x
			if x == newline || x == formfeed { // newlines affect the current state (p.impliedSemi)
				// and not the state after printing arg (impliedSemi)
				// because comments can be interspersed before the arg
				// in this case
				self.impliedSemi = false

			}
			self.lastTok = token.ILLEGAL
			continue



		case *ast.Ident:

			data = x.Name
			impliedSemi = true
			self.lastTok = token.IDENT



		case *ast.BasicLit:

			data = x.Value
			isLit = true
			impliedSemi = true
			self.lastTok = x.Kind



		case iToken.Token:

			s := x.String()
			data = s



		case token.Token:

			s := x.String()
			if mayCombine(self.lastTok, s[0]) { // the previous and the current token must be
				// separated by a blank otherwise they combine
				// into a different incorrect token sequence
				// (except for token.INT followed by a '.' this
				// should never happen because it is taken care
				// of via binary expression formatting)
				if len(self.wsbuf) != 0 {
					self.internalError("whitespace buffer not empty")

				}
				self.wsbuf = self.wsbuf[0:1]
				self.wsbuf[0] = ' '

			}
			data = s
			// some keywords followed by a newline imply a semicolon
			switch x {
			case token.BREAK, token.CONTINUE, token.FALLTHROUGH, token.RETURN,
				token.INC, token.DEC, token.RPAREN, token.RBRACK, token.RBRACE:

				impliedSemi = true



			}
			self.lastTok = x



		case token.Pos:

			if x.IsValid() {
				self.pos = self.posFor(x) // accurate position of next item

			}
			continue



		case string:
			// incorrect AST - print error message
			data = x
			isLit = true
			impliedSemi = true
			self.lastTok = token.STRING



		default:

			fmt.Fprintf(os.Stderr, "print: unsupported argument %v (%T)\n", arg, arg)
			panic("github.com/DAddYE/igo/from_go printer type")

			// data != ""

		}
		next := self.pos // estimated/accurate position of next item
		wroteNewline, droppedFF := self.flush(next, self.lastTok)

		// intersperse extra newlines if present in the source and
		// if they don't cause extra semicolons (don't do this in
		// flush as it will cause extra newlines at the end of a file)
		if !self.impliedSemi {
			n := nlimit(next.Line - self.pos.Line)
			// don't exceed maxNewlines if we already wrote one
			if wroteNewline && n == maxNewlines {
				n = maxNewlines - 1

			}
			if n > 0 {
				ch := byte('\n')
				if droppedFF {
					ch = '\f' // use formfeed since we dropped one before

				}
				self.writeByte(ch, n)
				impliedSemi = false

			}
		}
		self.writeString(next, data, isLit)
		self.impliedSemi = impliedSemi

	} // commentBefore returns true iff the current comment group occurs
	// before the next position in the source code and printing it does
	// not introduce implicit semicolons.
	//
}
func (self *printer) commentBefore(next token.Position) (result bool) {
	return self.commentOffset < next.Offset && (!self.impliedSemi || !self.commentNewline)
}

// flush prints any pending comments and whitespace occurring textually
// before the position of the next token tok. The flush result indicates
// if a newline was written or if a formfeed was dropped from the whitespace
// buffer.
//
func (self *printer) flush(next token.Position, tok token.Token) (wroteNewline, droppedFF bool) {
	if self.commentBefore(next) { // if there are comments before the next item, intersperse them
		wroteNewline, droppedFF = self.intersperseComments(next, tok)
	} else { // otherwise, write any leftover whitespace
		self.writeWhitespace(len(self.wsbuf))

	}
	return
}

// getNode returns the ast.CommentGroup associated with n, if any.
func getDoc(n ast.Node) *ast.CommentGroup {
	switch n := n.(type) {
	case *ast.Field:

		return n.Doc

	case *ast.ImportSpec:

		return n.Doc

	case *ast.ValueSpec:

		return n.Doc

	case *ast.TypeSpec:

		return n.Doc

	case *ast.GenDecl:

		return n.Doc

	case *ast.FuncDecl:

		return n.Doc

	case *ast.File:

		return n.Doc



	}
	return nil

}
func (self *printer) printNode(node interface{}) error { // unpack *CommentedNode, if any
	var comments []*ast.CommentGroup
	if cnode, ok := node.(*CommentedNode); ok {
		node = cnode.Node
		comments = cnode.Comments

	}
	if comments != nil { // commented node - restrict comment list to relevant range
		n, ok := node.(ast.Node)
		if !ok {
			goto unsupported

		}
		beg := n.Pos()
		end := n.End()
		// if the node has associated documentation,
		// include that commentgroup in the range
		// (the comment list is sorted in the order
		// of the comment appearance in the source code)
		if doc := getDoc(n); doc != nil {
			beg = doc.Pos()

		} // token.Pos values are global offsets, we can
		// compare them directly
		i := 0
		for i < len(comments) && comments[i].End() < beg {
			i++

		}
		j := i
		for j < len(comments) && comments[j].Pos() < end {
			j++

		}
		if i < j {
			self.comments = comments[i:j]

		}
	} else if n, ok := node.(*ast.File); ok { // use ast.File comments, if any
		self.comments = n.Comments

	} // if there are no comments, use node comments
	self.useNodeComments = self.comments == nil

	// get comments ready for use
	self.nextComment()

	// format node
	switch n := node.(type) {
	case ast.Expr:

		self.expr(n)

	case ast.Stmt:

		self.stmt(n, false)

	case ast.Decl:

		self.decl(n)

	case ast.Spec:

		self.spec(n, 1, false)

	case []ast.Stmt:

		self.stmtList(n, 0, false)

	case []ast.Decl:

		self.declList(n)

	case *ast.File:

		self.file(n)

	default:

		goto unsupported



	}
	return nil

unsupported:

	return fmt.Errorf("github.com/DAddYE/igo/printer: unsupported node type %T", node)

	// ----------------------------------------------------------------------------
	// Trimmer

	// A trimmer is an io.Writer filter for stripping tabwriter.Escape
	// characters, trailing blanks and tabs, and for converting formfeed
	// and vtab characters into newlines and htabs (in case no tabwriter
	// is used). Text bracketed by tabwriter.Escape characters is passed
	// through unchanged.
	//
}

type trimmer struct {
	output io.Writer
	state  int
	space  []byte
}

// trimmer is implemented as a state machine.
// It can be in one of the following states:
const (
	inSpace  = iota // inside space
	inEscape // inside text bracketed by tabwriter.Escapes
	inText   // inside text
)

func (self *trimmer) resetSpace() {
	self.state = inSpace
	self.space = self.space[0:0]
}

// Design note: It is tempting to eliminate extra blanks occurring in
//              whitespace in this function as it could simplify some
//              of the blanks logic in the node printing functions.
//              However, this would mess up any formatting done by
//              the tabwriter.

var aNewline = []byte("\n")

func (self *trimmer) Write(data []byte) (n int, err error) { // invariants:
	// p.state == inSpace:
	//	p.space is unwritten
	// p.state == inEscape, inText:
	//	data[m:n] is unwritten
	m := 0
	var b byte
	for n, b = range data {
		if b == '\v' {
			b = '\t' // convert to htab

		}
		switch self.state {
		case inSpace:

			switch b {
			case '\t', ' ':

				self.space = append(self.space, b)

			case '\n', '\f':

				self.resetSpace() // discard trailing space
				_, err = self.output.Write(aNewline)

			case tabwriter.Escape:

				_, err = self.output.Write(self.space)
				self.state = inEscape
				m = n + 1 // +1: skip tabwriter.Escape
			default:

				_, err = self.output.Write(self.space)
				self.state = inText
				m = n



			}

		case inEscape:

			if b == tabwriter.Escape {
				_, err = self.output.Write(data[m:n])
				self.resetSpace()

			}

		case inText:

			switch b {
			case '\t', ' ':

				_, err = self.output.Write(data[m:n])
				self.resetSpace()
				self.space = append(self.space, b)

			case '\n', '\f':

				_, err = self.output.Write(data[m:n])
				self.resetSpace()
				_, err = self.output.Write(aNewline)

			case tabwriter.Escape:

				_, err = self.output.Write(data[m:n])
				self.state = inEscape
				m = n + 1 // +1: skip tabwriter.Escape

			}

		default:

			panic("unreachable")



		}
		if err != nil {
			return

		}
	}
	n = len(data)

	switch self.state {
	case inEscape, inText:

		_, err = self.output.Write(data[m:n])
		self.resetSpace()



	}
	return
}

// ----------------------------------------------------------------------------
// Public interface

// A Mode value is a set of flags (or 0). They control printing.
type Mode uint

const (
	RawFormat Mode = 1 << iota // do not use a tabwriter; if set, UseSpaces is ignored
	TabIndent // use tabs for indentation independent of UseSpaces
	UseSpaces // use spaces instead of tabs for alignment
	SourcePos // emit //line comments to preserve original source positions
)

// A Config node controls the output of Fprint.
type Config struct {
	Mode     Mode // default: 0
	Tabwidth int  // default: 8
	Indent   int  // default: 0 (all code is indented at least by this much)
}

// fprint implements Fprint and takes a nodesSizes map for setting up the printer state.
func (self *Config) fprint(output io.Writer, fset *token.FileSet, node interface{}, nodeSizes map[ast.Node]int) (err error) { // print node
	var p printer
	p.init(self, fset, nodeSizes)
	if err = p.printNode(node); err != nil {
		return

	} // print outstanding comments
	p.impliedSemi = false // EOF acts like a newline
	p.flush(token.Position{Offset: infinity, Line: infinity}, token.EOF)

	// redirect output through a trimmer to eliminate trailing whitespace
	// (Input to a tabwriter must be untrimmed since trailing tabs provide
	// formatting information. The tabwriter could provide trimming
	// functionality but no tabwriter is used when RawFormat is set.)
	output = &trimmer{output: output}

	// redirect output through a tabwriter if necessary
	if self.Mode&RawFormat == 0 {
		minwidth := self.Tabwidth

		padchar := byte('\t')
		if self.Mode&UseSpaces != 0 {
			padchar = ' '

		}
		twmode := tabwriter.DiscardEmptyColumns
		if self.Mode&TabIndent != 0 {
			minwidth = 0
			twmode |= tabwriter.TabIndent

		}
		output = tabwriter.NewWriter(output, minwidth, self.Tabwidth, 1, padchar, twmode)

	} // write printer result via tabwriter/trimmer to output
	if _, err = output.Write(p.output); err != nil {
		return

	} // flush tabwriter, if any
	if tw, _ := output.(*tabwriter.Writer); tw != nil {
		err = tw.Flush()

	}
	return
}

// A CommentedNode bundles an AST node and corresponding comments.
// It may be provided as argument to any of the Fprint functions.
//
type CommentedNode struct {
	Node     interface{} // *ast.File, or ast.Expr, ast.Decl, ast.Spec, or ast.Stmt
	Comments []*ast.CommentGroup
}

// Fprint "pretty-prints" an AST node to output for a given configuration cfg.
// Position information is interpreted relative to the file set fset.
// The node type must be *ast.File, *CommentedNode, []ast.Decl, []ast.Stmt,
// or assignment-compatible to ast.Expr, ast.Decl, ast.Spec, or ast.Stmt.
//
func (self *Config) Fprint(output io.Writer, fset *token.FileSet, node interface{}) error {
	return self.fprint(output, fset, node, make(map[ast.Node]int))
}

// Fprint "pretty-prints" an AST node to output.
// It calls Config.Fprint with default settings.
//
func Fprint(output io.Writer, fset *token.FileSet, node interface{}) error {
	return (&Config{Tabwidth: 8}).Fprint(output, fset, node)
}
