package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func apply(op byte, left, right int64) int64 {
	if op == '+' {
		return left + right
	}
	return left * right
}

func readNum(expr []byte) (int64, int) {
	i := 0
	for ; i < len(expr); i++ {
		if expr[i] < '0' || expr[i] > '9' {
			break
		}
	}
	num, err := strconv.ParseInt(string(expr[0:i]), 10, 64)
	if err != nil {
		panic(err)
	}
	return num, i - 1
}

func equivEval(expr []byte) (int64, int) {
	var acc int64 = 0
	var op byte = '+'

	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '+', '*':
			op = expr[i]
		case '(':
			subacc, newi := equivEval(expr[i+1:])
			i += newi + 1
			acc = apply(op, acc, subacc)
		case ')':
			return acc, i
		case ' ':
			continue
		default:
			num, newi := readNum(expr[i:])
			i += newi
			acc = apply(op, acc, num)
		}
	}
	return acc, len(expr)
}

func addFirstEval(expr []byte) (int64, int) {
	var acc int64 = 0
	var op byte = '+'

	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '+':
			op = expr[i]
		case '*':
			op = expr[i]
			subacc, newi := addFirstEval(expr[i+1:])
			i += newi
			acc = apply(op, acc, subacc)
		case '(':
			subacc, newi := addFirstEval(expr[i+1:])
			i += newi + 1
			acc = apply(op, acc, subacc)
		case ')':
			return acc, i
		case ' ':
			continue
		default:
			num, newi := readNum(expr[i:])
			i += newi
			acc = apply(op, acc, num)
		}
	}
	return acc, len(expr)
}

func main() {
	var acc1, acc2 int64 = 0, 0
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		val1, _ := equivEval(scanner.Bytes())
		acc1 += val1
		val2, _ := addFirstEval(scanner.Bytes())
		acc2 += val2
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1: %d\n", acc1)
	fmt.Printf("part 2: %d\n", acc2)
}
