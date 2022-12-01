package io

import (
	"bufio"
	"io"
	"strconv"
)

// IntScanner provides support for scanning a token as an int at various sizes.
// The base of the inputs defaults to 10, but can be adjusted with the Base()
// function.
type IntScanner struct {
	*bufio.Scanner
	err  error
	num  int64
	base int
}

// NewIntScanner creates a new Int16Scanner.
func NewIntScanner(r io.Reader) *IntScanner {
	return &IntScanner{bufio.NewScanner(r), nil, 0, 10}
}

// Base changes the default base to use when parsing each input.
func (s *IntScanner) Base(base int) {
	s.base = base
}

// Err first returns any error from scanning, else it returns any error from parsing a token as an int16.
func (s *IntScanner) Err() error {
	if s.Scanner.Err() != nil {
		return s.Scanner.Err()
	}
	return s.err
}

// Scan will perform a normal Scan, then it will attempt to convert any scanned token to an int64. If this
// fails, an error is recorcded and false is returned. A false is returned if the Scan itself failed.
func (s *IntScanner) Scan() bool {
	if s.Scanner.Scan() {
		i, err := strconv.ParseInt(s.Text(), s.base, 64)
		s.num = int64(i)
		s.err = err
		return s.err == nil
	}
	return false
}

// Int returns the last scanned int value.
func (s *IntScanner) Int() int {
	return int(s.num)
}

// Int8 returns the last scanned int8 value.
func (s *IntScanner) Int8() int8 {
	return int8(s.num)
}

// Int16 returns the last scanned int16 value.
func (s *IntScanner) Int16() int16 {
	return int16(s.num)
}

// Int32 returns the last scanned int32 value.
func (s *IntScanner) Int32() int32 {
	return int32(s.num)
}

// Int64 returns the last scanned int16 value.
func (s *IntScanner) Int64() int64 {
	return s.num
}
