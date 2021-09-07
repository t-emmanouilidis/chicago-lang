package repl

import (
	"bufio"
	"fmt"
	"io"
	"io/aegis/chicago/lexer"
	"io/aegis/chicago/parser"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Fprintf(out, PROMPT)
		scanned := scanner.Scan()

		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)
		p := parser.New(l)

		program := p.ParseProgram()
		if p.AnyErrors() {
			io.WriteString(out, p.ErrorsAsString())
			continue
		}

		io.WriteString(out, program.String())
		io.WriteString(out, "\n")
	}
}
