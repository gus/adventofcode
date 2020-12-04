package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// validators

type validator func(string) bool

func intRangeValidator(min, max int) validator {
	return func(input string) bool {
		num, err := strconv.Atoi(input)
		if err != nil {
			return false
		}
		return num >= min && num <= max
	}
}

func regexpValidator(exp string) validator {
	re := regexp.MustCompile(exp)
	return func(input string) bool {
		return re.MatchString(input)
	}
}

func valueValidator(values []string) validator {
	vmap := map[string]struct{}{}
	for _, v := range values {
		vmap[v] = struct{}{}
	}
	return func(input string) bool {
		_, ok := vmap[input]
		return ok
	}
}

func heightValidator() validator {
	// a number followed by either cm or in:
	// - If cm, the number must be at least 150 and at most 193.
	// - If in, the number must be at least 59 and at most 76.
	cmRangeValidator := intRangeValidator(150, 193)
	inRangeValidator := intRangeValidator(59, 76)
	hgtRe := regexp.MustCompile("^([0-9]{2,3})(cm|in)$")

	return func(input string) bool {
		matches := hgtRe.FindSubmatch([]byte(input))
		if len(matches) != 3 {
			return false
		}

		if string(matches[2]) == "in" {
			return inRangeValidator(string(matches[1]))
		}
		return cmRangeValidator(string(matches[1]))
	}
}

// rules

var rules = map[string]validator{
	"byr": intRangeValidator(1920, 2002),
	"ecl": valueValidator(strings.Split("amb blu brn gry grn hzl oth", " ")),
	"eyr": intRangeValidator(2020, 2030),
	"hcl": regexpValidator("^#[0-9a-f]{6}$"),
	"hgt": heightValidator(),
	"iyr": intRangeValidator(2010, 2020),
	"pid": regexpValidator("^[0-9]{9}$"),
}

// document

type document map[string]string

func (d document) Scan(line string) {
	for _, field := range strings.Split(line, " ") {
		kv := strings.Split(field, ":")
		d[kv[0]] = kv[1]
	}
}

func (d document) Valid() bool {
	for k := range rules {
		if _, ok := d[k]; !ok {
			return false
		}
	}
	return true
}

func (d document) ValidStrict() bool {
	for k, vfunc := range rules {
		if input, ok := d[k]; !ok || !vfunc(input) {
			return false
		}
	}
	return true
}

// main

func countValidDocs(docs []document, strict bool) int {
	ctr := 0
	for _, doc := range docs {
		if (strict && doc.ValidStrict()) || (!strict && doc.Valid()) {
			ctr++
		}
	}
	return ctr
}

func main() {
	docs := make([]document, 0)

	doc := document{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			docs = append(docs, doc)
			doc = document{}
			continue
		}
		doc.Scan(line)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}
	docs = append(docs, doc)

	fmt.Printf("part 1: %d\n", countValidDocs(docs, false))
	fmt.Printf("part 2: %d\n", countValidDocs(docs, true))
}
