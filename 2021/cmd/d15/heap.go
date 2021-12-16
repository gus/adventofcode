package main

// See https://pkg.go.dev/container/heap?utm_source=gopls#example-package-PriorityQueue

import (
	"container/heap"

	"github.com/gus/adventofcode/2021/internal/geom"
)

// An RiskState is something we manage in a priority queue.
type RiskState struct {
	pt geom.P2

	// risk represents the inverse priority of the pt
	risk int

	// path is the sequence of pts to get to this one
	path []geom.P2

	// index is needed by update and is maintained by the heap.Interface methods
	// and notes the index of this item in the heap.
	index int
}

type PriorityQueue []*RiskState

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	// we want the lowest risk/priority pt, or the pt with the shortest path if
	// risk is equivalent
	if pq[i].risk == pq[j].risk {
		return len(pq[i].path) < len(pq[j].path)
	}
	return pq[i].risk < pq[j].risk
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PriorityQueue) Push(x interface{}) {
	n := len(*pq)
	item := x.(*RiskState)
	item.index = n
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil  // avoid memory leak
	item.index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}

// update modifies the priority and value of an P2RiskState in the queue.
func (pq *PriorityQueue) update(item *RiskState, pt geom.P2, risk int) {
	item.pt = pt
	item.risk = risk
	heap.Fix(pq, item.index)
}
