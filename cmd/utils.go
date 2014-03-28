package cmd

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
)

type Mode int

const (
	GO Mode = iota
	IGO
)

var (
	// layout control
	comments  = flag.Bool("comments", true, "print comments")
	tabWidth  = flag.Int("tabwidth", 8, "tab width")
	tabIndent = flag.Bool("tabs", true, "indent with tabs")
	DestDir   = flag.String("dest", "./", "destination directory")

	// ExitCode
	exitCode = 0
)

func To(m Mode, paths []string) int {
	flag.Parse()

	if *tabWidth < 0 {
		fmt.Fprintf(os.Stderr, "negative tabwidth %d\n", *tabWidth)
		exitCode = 2
	}

	if m == IGO {
		goInitParserMode()
		goInitPrinterMode()
	} else {
		igoInit()
	}

	// If we don't want to process a single file or directory,
	// preocess the current dir.
	if len(paths) == 0 {
		paths = append(paths, ".")
	}

	for _, path := range paths {
		if m == IGO {
			goWalkPath(path)
		} else {
			igoWalkPath(path)
		}
	}

	return exitCode
}

func createDir(file string) {
	dir := filepath.Dir(file)
	err := os.MkdirAll(dir, 0700)
	if err != nil && !os.IsExist(err) {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
}

func cutSpace(b []byte) (before, middle, after []byte) {
	i := 0
	for i < len(b) && (b[i] == ' ' || b[i] == '\t' || b[i] == '\n') {
		i++
	}
	j := len(b)
	for j > 0 && (b[j-1] == ' ' || b[j-1] == '\t' || b[j-1] == '\n') {
		j--
	}
	if i <= j {
		return b[:i], b[i:j], b[j:]
	}
	return nil, nil, b[j:]
}

// matchSpace reformats src to use the same space context as orig.
// 1) If orig begins with blank lines, matchSpace inserts them at the beginning of src.
// 2) matchSpace copies the indentation of the first non-blank line in orig
//    to every non-blank line in src.
// 3) matchSpace copies the trailing space from orig and uses it in place
//   of src's trailing space.
func matchSpace(orig []byte, src []byte) []byte {
	before, _, after := cutSpace(orig)
	i := bytes.LastIndex(before, []byte{'\n'})
	before, indent := before[:i+1], before[i+1:]

	_, src, _ = cutSpace(src)

	var b bytes.Buffer
	b.Write(before)
	for len(src) > 0 {
		line := src
		if i := bytes.IndexByte(line, '\n'); i >= 0 {
			line, src = line[:i+1], line[i+1:]
		} else {
			src = nil
		}
		if len(line) > 0 && line[0] != '\n' { // not blank
			b.Write(indent)
		}
		b.Write(line)
	}
	b.Write(after)
	return b.Bytes()
}
