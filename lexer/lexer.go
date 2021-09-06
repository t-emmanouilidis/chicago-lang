package lexer

import "io/aegis/chicago/token"

type Lexer struct {
	input           string
	currentPosition int
	nextPosition    int
	currentByte     byte
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.nextPosition >= len(l.input) {
		l.currentByte = 0
	} else {
		l.currentByte = l.input[l.nextPosition]
	}
	l.currentPosition = l.nextPosition
	l.nextPosition += 1
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()

	switch l.currentByte {
	case '=':
		if l.peekChar() == '=' {
			ch := l.currentByte
			l.readChar()
			literal := string(ch) + string(l.currentByte)
			tok = newTokenWithLiteral(token.EQUAL, literal)
		} else {
			tok = newToken(token.ASSIGN, l.currentByte)
		}
	case ';':
		tok = newToken(token.SEMICOLON, l.currentByte)
	case '(':
		tok = newToken(token.LPAREN, l.currentByte)
	case ')':
		tok = newToken(token.RPAREN, l.currentByte)
	case ',':
		tok = newToken(token.COMMA, l.currentByte)
	case '+':
		tok = newToken(token.PLUS, l.currentByte)
	case '/':
		tok = newToken(token.DIVIDE, l.currentByte)
	case '{':
		tok = newToken(token.LBRACE, l.currentByte)
	case '}':
		tok = newToken(token.RBRACE, l.currentByte)
	case '*':
		tok = newToken(token.MULTIPLY, l.currentByte)
	case '-':
		tok = newToken(token.MINUS, l.currentByte)
	case '!':
		if l.peekChar() == '=' {
			ch := l.currentByte
			l.readChar()
			literal := string(ch) + string(l.currentByte)
			tok = newTokenWithLiteral(token.UNEQUAL, literal)
		} else {
			tok = newToken(token.BANG, l.currentByte)
		}
	case '>':
		tok = newToken(token.GREATER, l.currentByte)
	case '<':
		tok = newToken(token.LESS, l.currentByte)
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.currentByte) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdentifier(tok.Literal)
			return tok
		} else if isDigit(l.currentByte) {
			tok.Literal = l.readNumber()
			tok.Type = token.INT
			return tok
		} else {
			tok = newToken(token.ILLEGAL, l.currentByte)
		}
	}

	l.readChar()
	return tok
}

func newToken(tokenType token.TokenType, currentByte byte) token.Token {
	return newTokenWithLiteral(tokenType, string(currentByte))
}

func newTokenWithLiteral(tokenType token.TokenType, literal string) token.Token {
	return token.Token{Type: tokenType, Literal: literal}
}

func (l *Lexer) readIdentifier() string {
	startingChar := l.currentPosition
	for isLetter(l.currentByte) {
		l.readChar()
	}
	return l.input[startingChar:l.currentPosition]
}

func (l *Lexer) readNumber() string {
	startingChar := l.currentPosition
	for isDigit(l.currentByte) {
		l.readChar()
	}
	return l.input[startingChar:l.currentPosition]
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func (l *Lexer) skipWhitespace() {
	for l.currentByte == ' ' || l.currentByte == '\n' || l.currentByte == '\t' || l.currentByte == '\r' {
		l.readChar()
	}
}

func (l *Lexer) inputLength() int {
	return len(l.input)
}

func (l *Lexer) peekChar() byte {
	if l.nextPosition >= l.inputLength() {
		return 0
	} else {
		return l.input[l.nextPosition]
	}
}
