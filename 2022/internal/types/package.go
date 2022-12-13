package types

// OrderedNumeric matches numeric types that support the < operator.
type OrderedNumeric interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 |
		~float32 | ~float64
}

// AnyInt matches any int type.
type AnyInt interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64
}

func Zero[T any]() T {
	var zero T
	return zero
}

// IsZero returns true if a's value equals the zero-value for the type.
func IsZero[T comparable](z T) bool {
	return z == Zero[T]()
}

// IsZero returns [a] if [z] is the type's zero-value; else it returns [b].
func IfZero[T comparable, D any](z T, a D, b D) D {
	if IsZero(z) {
		return a
	}
	return b
}
