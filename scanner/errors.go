// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package scanner

import (
	"fmt"
	"github.com/DAddYE/igo/token"
	"io"
	"sort"
)

// In an ErrorList, an error is represented by an *Error.
// The position Pos, if valid, points to the beginning of
// the offending token, and the error condition is described
// by Msg.
//
type Error struct {
	Pos token.Position
	Msg string
}

// Error implements the error interface.
func (self Error) Error() string {
	if self.Pos.Filename != "" || self.Pos.IsValid() { // don't print "<unknown position>"
		// TODO(gri) reconsider the semantics of Position.IsValid
		return self.Pos.String() + ": " + self.Msg

	}
	return self.Msg
}

// ErrorList is a list of *Errors.
// The zero value for an ErrorList is an empty ErrorList ready to use.
//
type ErrorList []*Error

// Add adds an Error with given position and error message to an ErrorList.
func (self *ErrorList) Add(pos token.Position, msg string) {
	*self = append(*self, &Error{pos, msg})
}

// Reset resets an ErrorList to no errors.
func (self *ErrorList) Reset() {
	*self = (*self)[0:0]
}

// ErrorList implements the sort Interface.
func (self ErrorList) Len() int {
	return len(self)
}
func (self ErrorList) Swap(i, j int) {
	self[i], self[j] = self[j], self[i]

}
func (self ErrorList) Less(i, j int) bool {
	e := &self[i].Pos
	f := &self[j].Pos
	// Note that it is not sufficient to simply compare file offsets because
	// the offsets do not reflect modified line information (through //line
	// comments).
	if e.Filename < f.Filename {
		return true

	}
	if e.Filename == f.Filename {
		if e.Line < f.Line {
			return true

		}
		if e.Line == f.Line {
			return e.Column < f.Column

		}
	}
	return false
}

// Sort sorts an ErrorList. *Error entries are sorted by position,
// other errors are sorted by error message, and before any *Error
// entry.
//
func (self ErrorList) Sort() {
	sort.Sort(self)
}

// RemoveMultiples sorts an ErrorList and removes all but the first error per line.
func (self *ErrorList) RemoveMultiples() {
	sort.Sort(self)
	var last token.Position // initial last.Line is != any legal error line
	i := 0
	for _, e := range *self {
		if e.Pos.Filename != last.Filename || e.Pos.Line != last.Line {
			last = e.Pos
			(*self)[i] = e
			i++

		}
	}
	(*self) = (*self)[0:i]
}

// An ErrorList implements the error interface.
func (self ErrorList) Error() string {
	switch len(self) {
	case 0:

		return "no errors"

	case 1:

		return self[0].Error()



	}
	return fmt.Sprintf("%s (and %d more errors)", self[0], len(self)-1)
}

// Err returns an error equivalent to this error list.
// If the list is empty, Err returns nil.
func (self ErrorList) Err() error {
	if len(self) == 0 {
		return nil

	}
	return self
}

// PrintError is a utility function that prints a list of errors to w,
// one error per line, if the err parameter is an ErrorList. Otherwise
// it prints the err string.
//
func PrintError(w io.Writer, err error) {
	if list, ok := err.(ErrorList); ok {
		for _, e := range list {
			fmt.Fprintf(w, "%s\n", e)

		}
	} else if err != nil {
		fmt.Fprintf(w, "%s\n", err)

	}
}
