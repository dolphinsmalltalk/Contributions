"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #VectorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VectorTest guid: (GUID fromString: '{728bdae0-4e08-11d5-be18-00010240d5e2}')!
VectorTest comment: ''!
!VectorTest categoriesForClass!Unclassified! !
!VectorTest methodsFor!

testAddition
	| v1 v2 v3 |

	v1 := Vector from: #(9 8 7).
	v2 := Vector from: #(3 2 1).
	v3 := Vector from: #(1 2 3 4 5).

	self should: [ v1 + v2 = (Vector from: #(12 10 8)) ].
	self should: [ v1 + v3 ] raise: IncompatibleVectorError!

testAngleFrom
	| v1 v2 v3 v4 v5 |

	v1 := Vector from: #(3 1 1 1).
	v2 := Vector from: #(2 2 0 1).
	v3 := Vector from: #(1 2 3).
	v4 := Vector from: #(0 10).
	v5 := Vector from: #(10 0).

	self shouldnt: [ v1 angleFrom: v2 ] raise: Error.
	self should: [ (v1 angleFrom: v2) - (Float pi / 6) < 0.000000000000001].
	self should: [ v1 angleFrom: v3 ] raise: IncompatibleVectorError.
	self should: [ (v4 angleFrom: v5) - (Float pi / 2) < 0.000000000000001]!

testAsColumn
	| v c m |

	v := Vector from: #(9 8 7 6 5).
	c := v asColumn.
	m := Matrix new: 5@1 from: #(9 8 7 6 5).
	self should: [ c = m ]!

testAsRow
	| v r m |

	v := Vector from: #(9 8 7 6 5).
	r := v asRow.
	m := Matrix new: 1@5 from: #(9 8 7 6 5).
	self should: [ r = m ]!

testAsVector
	| v1 v2 |

	v1 := Vector from: #(0 9 8 7 6 1 2 3 4).
	v2 := v1 asVector.
	self should: [ v1 = v2 ]!

testAt
	| v |

	v := Vector from: #(0 10 1 9 2 8 3 7 4 7 5).
	self should: [ (v at: 4) = 9 ].
	self should: [ (v at: 11) = 5 ]!

testAtPut
	| v |

	v := Vector from: #(0 10 1 9 2 8 3 7 4 7 5).
	v at: 7 put: 111.
	self should: [ (v at: 7) = 111 ]!

testDisplayString
	| v |

	v := Vector from: #(1 2 3 4 5).
	self should: [ v displayString = '(1 2 3 4 5)' ]!

testDistance
	| v1  v2 |

	v1 := Vector from: #(1 2 3).
	v2 := Vector from: #(4 5 6).

	self should: [ (v1 distanceFrom: v2) = (9 + 9 + 9) sqrt ]!

testDivision
	| v1 v2 |

	v1 := Vector from: #(1 2 3).
	v2 := Vector from: #(4 5 6).
	self should: [ (v1 / 2) = (Vector from: #(##(1/2) 1 ##(3/2))) ].
	self should: [ (v1 / (3/2)) = (Vector from: #(##(2/3) ##(4/3)  2)) ].
	self should: [ (v1 / 2.5) equals: (Vector from: #(0.4 0.8 1.2)) ].
	self should: [ (v1 / v2) = (23/20) ].
!

testDotProduct
	| v1 v2 dotProduct |

	v1 := Vector from: #(9 8 7 6).
	v2 := Vector from: #(1 2 3 4).
	self should: [ (dotProduct := v1 dotProduct: v2) = 70 ]!

testEquivalence
	| v1 v2 |

	v1 := Vector from: #(1 2 3 4 5 6).
	v2 := Vector from: #(1 2 3 4 5 6).

	self should: [ v1 = v2 ]!

testLength
	| v |

	v := Vector from: #(1 2 3 4).
	self should: [ v length = (1 + 4 + 9 + 16) sqrt ]!

testMultiplication
	| v1 v2 |

	v1 := Vector from: #(1 2 3).
	v2 := Vector from: #(4 5 6).
	self should: [ (v1 * 1.5) = (Vector from: #(1.5 3.0 4.5)) ].
	self should: [ (v1 * (3/2)) = (Vector from: #(##(3/2) 3 ##(9/2))) ].
	self should: [ (v1 * 4) = (Vector from: #(4 8 12)) ].
	self should: [ (v1 * 1.234s) = (Vector from: #(1.234s 2.468s 3.702s)) ].
	self should: [ (v1 * v2) = 32 ].
!

testNegation
	| v1 |

	v1 := Vector from: #(9 7 5 3 1).
	self should: [ v1 negated = (Vector from: #(-9 -7 -5 -3 -1)) ]!

testOrder
	| v1 |

	v1 := Vector from: #(2 4 6 8 10).
	self should: [ v1 order = 5 ]!

testOrthogonalWith
	| v1 v2 v3 v4 |

	v1 := Vector from: #(1 -1 2 3).
	v2 := Vector from: #(2 1 1 0).
	v3 := Vector from: #(1 1 2 -2).
	v4 := Vector from: #(2 -4 3 2).

	self shouldnt: [ v1 orthogonalWith: v2 ].
	self should: [ v3 orthogonalWith: v4 ]!

testPerpendicularTo
	| v1 v2 v3 v4 |

	v1 := Vector from: #(1 -1 2 3).
	v2 := Vector from: #(2 1 1 0).
	v3 := Vector from: #(1 1 2 -2).
	v4 := Vector from: #(2 -4 3 2).

	self shouldnt: [ v1 perpendicularTo: v2 ].
	self should: [ v3 perpendicularTo: v4 ]!

testPrintString
	| v |

	v := Vector from: #(1 2 3 4 5).
	self should: [ v printString = 'a Vector(1 2 3 4 5)' ]!

testSubtraction
	| v1 v2 v3 |

	v1 := Vector from: #(9 8 7).
	v2 := Vector from: #(3 2 1).
	v3 := Vector from: #(1 2 3 4 5).

	self should: [ v1 - v2 = (Vector from: #(6 6 6)) ].
	self should: [ v1 - v3 ] raise: IncompatibleVectorError!

testValues
	| v1 testValues |

	testValues := #(2 9 4 7 5 6 3 8 1 0).
	v1 := Vector from: testValues.
	self should: [ v1 values = testValues ]! !
!VectorTest categoriesFor: #testAddition!public!testing! !
!VectorTest categoriesFor: #testAngleFrom!public!testing! !
!VectorTest categoriesFor: #testAsColumn!public!testing! !
!VectorTest categoriesFor: #testAsRow!public!testing! !
!VectorTest categoriesFor: #testAsVector!public!testing! !
!VectorTest categoriesFor: #testAt!public!testing! !
!VectorTest categoriesFor: #testAtPut!public!testing! !
!VectorTest categoriesFor: #testDisplayString!public!testing! !
!VectorTest categoriesFor: #testDistance!public!testing! !
!VectorTest categoriesFor: #testDivision!public!testing! !
!VectorTest categoriesFor: #testDotProduct!public!testing! !
!VectorTest categoriesFor: #testEquivalence!public!testing! !
!VectorTest categoriesFor: #testLength!public!testing! !
!VectorTest categoriesFor: #testMultiplication!public!testing! !
!VectorTest categoriesFor: #testNegation!public!testing! !
!VectorTest categoriesFor: #testOrder!public!testing! !
!VectorTest categoriesFor: #testOrthogonalWith!public!testing! !
!VectorTest categoriesFor: #testPerpendicularTo!public!testing! !
!VectorTest categoriesFor: #testPrintString!public!testing! !
!VectorTest categoriesFor: #testSubtraction!public!testing! !
!VectorTest categoriesFor: #testValues!public!testing! !

