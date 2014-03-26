package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"

	"github.com/DAddYE/igo/cmd"
)

type Cmd int

const (
	INVALID Cmd = iota
	COMPILE
	PARSE
	BUILD
)

var commands = []string{
	COMPILE: "compile",
	PARSE:   "parse",
	BUILD:   "build",
}

func usage() {
	fmt.Fprintf(os.Stderr, "usage: igo ["+strings.Join(commands[1:], "|")+"] [flags] [path ...]\n")
	flag.PrintDefaults()
	os.Exit(2)
}

func toCmd(c string) Cmd {
	for i, command := range commands {
		if c == command {
			return Cmd(i)
		}
	}
	return Cmd(0)
}

func abs(i int) int {
	switch {
	case i < 0:
		return -i
	default:
		return i
	}
}

func parseError(err []byte) {
	const regex = `^(.*):(\d+):(\d+)?\s*(.*)`
	re := regexp.MustCompile(regex)

	// Iterate over each error message.
	for _, line := range bytes.Split(err, []byte{'\n'}) {

		// if the error message is kind of:
		//		./path/name.go:line:col error message
		//
		match := re.FindAllStringSubmatch(string(line), 4)
		if len(match) == 1 {
			file, _ := filepath.Rel("./", match[0][1]) // TODO: check this under chdir
			line, _ := strconv.Atoi(match[0][2])
			col, _ := strconv.Atoi(match[0][3])
			message := match[0][4]
			igoFile := strings.TrimSuffix(file, ".go") + ".igo"
			if pos := cmd.IgoPositions[igoFile]; pos != nil {
				var cols []int
				for in, out := range *pos {
					if out.Line == line {
						cols = append(cols, in.Column)
					}
				}
				if len(cols) > 0 {
					closest := abs(cols[0] - col)
					for _, c := range cols {
						if cd := abs(c - col); cd < closest {
							closest = cd
						}
					}
					fmt.Printf("%s:%d:%d %s\n", igoFile, line, closest, message)
				} else {
					fmt.Printf("%s:%d:%d %s\n", igoFile, line, col, message)
				}
			}
		}
	}
}

func main() {
	flag.Usage = usage
	flag.Parse()

	var (
		command  Cmd
		paths    []string
		exitCode = 0
	)

	for i := 0; i < flag.NArg(); i++ {
		s := flag.Arg(i)
		if cmd := toCmd(s); cmd > 0 {
			command = cmd
		} else {
			// Could be a path
			paths = append(paths, s)
		}
	}

	switch command {
	case PARSE:
		exitCode = cmd.To(cmd.IGO, paths)
	case COMPILE:
		exitCode = cmd.To(cmd.GO, paths)
	case BUILD:
		exitCode = cmd.To(cmd.GO, paths)
		if exitCode == 0 {
			gocmd := path.Join(runtime.GOROOT(), "bin", "go")
			out, err := exec.Command(gocmd, "build").CombinedOutput()
			if err != nil {
				// os.Stderr.Write(out)
				parseError(out)
				exitCode = 1
			}
		}
	default:
		fmt.Fprintln(os.Stderr, "Invalid command")
		usage()
	}

	os.Exit(exitCode)
}
