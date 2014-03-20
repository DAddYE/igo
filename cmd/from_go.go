package cmd

import (
	"bytes"
	"path/filepath"

	printer "github.com/DAddYE/igo/from_go"

	"go/ast"
	"go/parser"
	"go/scanner"
	"go/token"

	"io"
	"io/ioutil"
	"os"
	"strings"
)

var (
	goFileSet     = token.NewFileSet() // per process FileSet
	goParserMode  parser.Mode
	goPrinterMode printer.Mode
)

func goReport(err error) {
	scanner.PrintError(os.Stderr, err)
	exitCode = 2

}
func goInitParserMode() {
	goParserMode = parser.Mode(0)
	if *comments {
		goParserMode |= parser.ParseComments

	}
	goParserMode |= parser.AllErrors

}
func goInitPrinterMode() {
	goPrinterMode = printer.UseSpaces
	if *tabIndent {
		goPrinterMode |= printer.TabIndent

	}
}
func goProcessFile(filename string, in io.Reader, out io.Writer) error {
	dest := strings.TrimSuffix(filename, ".go") + ".igo"

	f, err := os.Open(filename)
	if err != nil {
		return err

	}
	defer f.Close()

	src, err := ioutil.ReadAll(f)
	if err != nil {
		return err

	}
	file, adjust, err := goParse(goFileSet, filename, src)
	if err != nil {
		return err

	}
	ast.SortImports(goFileSet, file)

	var buf bytes.Buffer
	err = (&printer.Config{Mode: goPrinterMode, Tabwidth: *tabWidth}).Fprint(&buf, goFileSet, file)
	if err != nil {
		return err

	}
	res := buf.Bytes()
	if adjust != nil {
		res = adjust(src, res)

	}
	if *destDir != "" {
		dest = filepath.Join(*destDir, dest)
		createDir(dest)

	}
	err = ioutil.WriteFile(dest, res, 0644)
	if err != nil {
		return err

	}
	return err

}
func goFile(f os.FileInfo) bool { // ignore non-Go files
	name := f.Name()
	return !f.IsDir() && !strings.HasPrefix(name, ".") && strings.HasSuffix(name, ".go")

}
func goVisitFile(path string, f os.FileInfo, err error) error {
	if err == nil && goFile(f) {
		err = goProcessFile(path, nil, os.Stdout)

	}
	if err != nil {
		goReport(err)

	}
	return nil

}
func goWalkPath(path string) {
	switch dir, err := os.Stat(path); {
	case err != nil:

		goReport(err)

	case dir.IsDir():

		filepath.Walk(path, goVisitFile)

	default:

		if err := goProcessFile(path, nil, os.Stdout); err != nil {
			goReport(err)

		} // parse parses src, which was read from filename,
		// as a Go source file or statement list.
	}
}
func goParse(fset *token.FileSet, filename string, src []byte) (*ast.File, func(orig, src []byte) []byte, error) { // Try as whole source file.
	file, err := parser.ParseFile(fset, filename, src, goParserMode)
	if err == nil {
		return file, nil, nil

	} // If the error is that the source file didn't begin with a
	// package line and this is standard input, fall through to
	// try as a source fragment.  Stop and return on any other error.
	if !strings.Contains(err.Error(), "expected 'package'") {
		return nil, nil, err

	} // If this is a declaration list, make it a source file
	// by inserting a package clause.
	// Insert using a ;, not a newline, so that the line numbers
	// in psrc match the ones in src.
	psrc := append([]byte("package p;"), src...)
	file, err = parser.ParseFile(fset, filename, psrc, goParserMode)
	if err == nil {
		adjust := func(orig, src []byte) []byte { // Remove the package clause.
			// Gofmt has turned the ; into a \n.
			src = src[len("package p\n"):]
			return matchSpace(orig, src)

		}
		return file, adjust, nil

	} // If the error is that the source file didn't begin with a
	// declaration, fall through to try as a statement list.
	// Stop and return on any other error.
	if !strings.Contains(err.Error(), "expected declaration") {
		return nil, nil, err

	} // If this is a statement list, make it a source file
	// by inserting a package clause and turning the list
	// into a function body.  This handles expressions too.
	// Insert using a ;, not a newline, so that the line numbers
	// in fsrc match the ones in src.
	fsrc := append(append([]byte("package p; func _() {"), src...), '}')
	file, err = parser.ParseFile(fset, filename, fsrc, goParserMode)
	if err == nil {
		adjust := func(orig, src []byte) []byte { // Remove the wrapping.
			// Gofmt has turned the ; into a \n\n.
			src = src[len("package p\n\nfunc _() {"):]
			src = src[:len(src)-len("}\n")]
			// Gofmt has also indented the function body one level.
			// Remove that indent.
			src = bytes.Replace(src, []byte("\n\t"), []byte("\n"), -1)
			return matchSpace(orig, src)

		}
		return file, adjust, nil

	} // Failed, and out of options.
	return nil, nil, err
}
