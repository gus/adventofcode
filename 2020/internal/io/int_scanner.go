package io

import (
	"bufio"
	"io"
	"strconv"
)

// Int16Scanner provides support for scanning a token as an int16.
type Int16Scanner struct {
	*bufio.Scanner
	err error
	num int16
}

// NewInt16Scanner creates a new Int16Scanner.
func NewInt16Scanner(r io.Reader) *Int16Scanner {
	return &Int16Scanner{bufio.NewScanner(r), nil, 0}
}

// Err first returns any error from scanning, else it returns any error from parsing a token as an int16.
func (s *Int16Scanner) Err() error {
	if s.Scanner.Err() != nil {
		return s.Scanner.Err()
	}
	return s.err
}

// Scan will perform a normal Scan, then it will attempt to convert any scanned token to an int16. If this
// fails, an error is recorcded and false is returned. A false is returned if the Scan itself failed.
func (s *Int16Scanner) Scan() bool {
	if s.Scanner.Scan() {
		i, err := strconv.ParseInt(s.Text(), 10, 16)
		s.num = int16(i)
		s.err = err
		return s.err == nil
	}
	return false
}

// Int16 returns the last scanned int16 value.
func (s *Int16Scanner) Int16() int16 {
	return s.num
}
