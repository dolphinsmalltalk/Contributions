"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #ComplexTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ComplexTest guid: (GUID fromString: '{8903E117-E0FE-4306-AB83-C21F46A34A38}')!
ComplexTest comment: ''!
!ComplexTest categoriesForClass!Unclassified! !
!ComplexTest methodsFor!

testAddition
	| c1 c2 |

	self shouldnt: [ c1 := Complex real: 1 imaginary: 2 ] raise: Exception.
	self shouldnt: [ c2 := Complex real: 3 imaginary: 4 ] raise: Exception.

	self should: [ (c1 + c2) = (Complex real: 4 imaginary: 6) ].!

testCreation
	| c |

	self should: [ (c := Complex new) realPart = 0 ].
	self should: [ c imaginaryPart = 0 ].
	self should: [ (c := Complex real: 1 imaginary: 2) realPart = 1 ].
	self should: [ c imaginaryPart = 2 ]!

testDivision
	| c1 c2 |

	self shouldnt: [ c1 := Complex real: 2 imaginary: 2 ] raise: Exception.
	self shouldnt: [ c2 := Complex real: 3 imaginary: 6 ] raise: Exception.

	self should: [ (c1 / c1) = (Complex real: 1 imaginary: 0) ].
	self should: [ (c1 / c2) = (Complex real: 2/5 imaginary: (-2/15)) ].
	self should: [ (c2 / c1) = (Complex real: 9/4 imaginary: 3/4) ].

	self should: [ c2 / 3 = (Complex real: 1 imaginary: 2) ].
	self should: [ c1 / 2 = (Complex real: 1 imaginary: 1) ].!

testEquality
	self should: [ 3 = (Complex real: 3 imaginary: 0) ].
	self should: [ (Complex real: 3 imaginary: 0) = 3 ].

	self should: [ 3.0 = (Complex real: 3 imaginary: 0) ].
	self should: [ (Complex real: 3 imaginary: 0) = 3.0 ].

	self shouldnt: [ 3 = (Complex real: 3 imaginary: 1) ].
	self shouldnt: [ (Complex real: 3 imaginary: 1) = 3 ].

	self shouldnt: [ 3.0 = (Complex real: 3 imaginary: 1) ].
	self shouldnt: [ (Complex real: 3 imaginary: 1) = 3.0 ].

	self should: [ 3 ~= (Complex real: 3 imaginary: 1) ].
	self should: [ (Complex real: 3 imaginary: 1) ~= 3 ].

	self should: [ 3.0 ~= (Complex real: 3 imaginary: 1) ].
	self should: [ (Complex real: 3 imaginary: 1) ~= 3.0 ].
!

testModulus
	| c1 c2 |

	"Test case where |a| < |b| in complex number (a + ib)."

	self shouldnt: [ c1 := Complex real: 2 imaginary: 3 ] raise: Exception.
	self should: [ c1 modulus = (3 * ((1 + ((2 / 3) * (2 / 3))) sqrt)) ].

	"Test case where |a| >= |b| in complex number (a + ib)."

	self shouldnt: [ c2 := Complex real: 4 imaginary: -2 ] raise: Exception.
	self should: [ c2 modulus = (4 * ((1 + ((-2 / 4) * (-2 / 4))) sqrt)) ].!

testMultiplication
	| c1 c2 |

	self shouldnt: [ c1 := Complex real: 1 imaginary: 2 ] raise: Exception.
	self shouldnt: [ c2 := Complex real: 3 imaginary: 4 ] raise: Exception.

	self should: [ (c1 * c2) = (Complex real: -5 imaginary: 10) ].
	self should: [ (c1 * Complex zero) = Complex zero ].

	self should: [ c1 * 5 = (Complex real: 5 imaginary: 10) ].
	self should: [ c1 * 1.1 = (Complex real: 1.1 imaginary: 2.2) ].
	self should: [ c1 * (2/3) = (Complex real: 2/3 imaginary: 4/3) ].!

testRaisedTo
	| c |

	self shouldnt: [ c := Complex real: 3 imaginary: 2 ] raise: Exception.
	self should: [ (c raisedTo: 2) = (c * c) ].
	self should: [ (c raisedTo: 3) = (c * c * c) ].
	self should: [ (c raisedTo: 4) = (c * c * c * c) ].!

testSqrt
	| c w |

	self shouldnt: [ c := Complex real: 0 imaginary: 0 ] raise: Exception.
	self should: [ c sqrt = 0 ].

	self shouldnt: [ c := Complex real: 9 imaginary: 4 ] raise: Exception.
	self should: [ w := 3 * (((1 + (1 + ((4/9) * (4/9))) sqrt) / 2) sqrt).
			c sqrt = (Complex real: w imaginary: 4 / (2 * w)) ].

	self shouldnt: [ c := Complex imaginary: -2 ] raise: Exception.
	"self should: [ c sqrt = (Complex real: 1 imaginary: -1) ].  Should be true, but rounding bites us..."
	self should: [ (c sqrt realPart - 1) abs < 1.0e-10 ].
	self should: [ (c sqrt imaginaryPart + 1) abs < 1.0e-10 ].!

testSubtraction
	| c1 c2 |

	self shouldnt: [ c1 := Complex real: 1 imaginary: 2 ] raise: Exception.
	self shouldnt: [ c2 := Complex real: 3 imaginary: 4 ] raise: Exception.

	self should: [ (c1 - c2) = (Complex real: -2 imaginary: -2) ].! !
!ComplexTest categoriesFor: #testAddition!public!testing! !
!ComplexTest categoriesFor: #testCreation!public!testing! !
!ComplexTest categoriesFor: #testDivision!public!testing! !
!ComplexTest categoriesFor: #testEquality!public!testing! !
!ComplexTest categoriesFor: #testModulus!public!testing! !
!ComplexTest categoriesFor: #testMultiplication!public!testing! !
!ComplexTest categoriesFor: #testRaisedTo!public!testing! !
!ComplexTest categoriesFor: #testSqrt!public!testing! !
!ComplexTest categoriesFor: #testSubtraction!public!testing! !

