module Lesson4

// this defines a list type that is not the real list type in the F# stdlib, but it is equivalent
type 'a mylist = Empty | NonEmpty of 'a * 'a mylist
// we have one constant case for the empty list; and another case for the list construction
// these 2 cases are like the base case of a recursion and the inductive case: that's a recursive data type

//type 'a list = [] | (::) of 'a * 'a list


// this is a list made of 2 elements: 2 and 3
// that's pretty uncomfortable to write, but it is perfectly integrated within the language: no special syntax or constructs are needed
let l1 : int mylist = NonEmpty (2, NonEmpty (3, Empty))

// the REAL list type in the F# stdlib is defined as such (do not uncomment it or it will clash with the F# stdlib):
// type 'a list = [] | :: of 'a * 'a list
// [] is the empty list, while the (::) operator is the constructor

let l2 = 2 :: 3 :: []   // this is a list made of 2 elements: 2 and 3
let l3 = [2; 3]         // this is the exact same list but the syntax with square brackets is more comfortable
let l4 = []             // this is an empty list
