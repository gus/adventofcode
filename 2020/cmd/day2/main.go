package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

func usage(errMsg string) {
	fmt.Printf("! %s\n\nusage: %s [--positional]", errMsg, os.Args[0])
	os.Exit(1)
}

var policySplitter = regexp.MustCompile("[: -]+")

type policy struct {
	min int8
	max int8
	c   rune
}

func (pol *policy) validAsCount(pwd string) bool {
	ctr := int8(0)
	for _, c := range pwd {
		if c == pol.c {
			ctr++
		}
	}
	return ctr >= pol.min && ctr <= pol.max
}

func (pol *policy) validAsPositional(pwd string) bool {
	pwr := []rune(pwd)
	// if pol.min > len(pwr) || pol.max > len(pwr)
	ra, rb := pwr[pol.min-1], pwr[pol.max-1]
	return (ra == pol.c || rb == pol.c) && (ra != rb)
}

func parsePolicy(txt string) (*policy, string, error) {
	polPwdParts := policySplitter.Split(txt, -1)

	if len(polPwdParts) != 4 {
		return nil, "", errors.New("invalid password policy format: too many fields")
	}

	min64, err := strconv.ParseInt(polPwdParts[0], 10, 8)
	if err != nil {
		return nil, "", err
	}
	max64, err := strconv.ParseInt(polPwdParts[1], 10, 8)
	if err != nil {
		return nil, "", err
	}

	charRunes := []rune(polPwdParts[2])
	if len(charRunes) != 1 {
		return nil, "", errors.New("invalid password policy: expected only one character to match")
	}

	return &policy{int8(min64), int8(max64), charRunes[0]}, polPwdParts[3], nil
}

func main() {
	positional := false
	args := os.Args[1:]
	if len(args) > 1 {
		usage("too many arguments")
	} else if len(args) == 1 {
		if args[0] != "--positional" {
			usage(fmt.Sprintf("unexpected argument: %s", args[0]))
		}
		positional = true
	}

	ctr := 0
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		pol, pwd, err := parsePolicy(scanner.Text())
		if err != nil {
			log.Fatalf("'%s' - %v", scanner.Text(), err)
		} else if (positional && pol.validAsPositional(pwd)) || (!positional && pol.validAsCount(pwd)) {
			ctr++
		} else {
			log.Printf("invalid password: %s", scanner.Text())
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("correct passwords: %d", ctr)
}
