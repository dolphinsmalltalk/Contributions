| package |
package := Package name: 'LinearAlgebraTests'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #FloatMatrixTest;
	add: #MatrixTest;
	add: #VectorTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'LinearAlgebra';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #MatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #VectorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MatrixTest subclass: #FloatMatrixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

MatrixTest guid: (GUID fromString: '{728BDAE1-4E08-11D5-BE18-00010240D5E2}')!
MatrixTest comment: ''!
!MatrixTest categoriesForClass!Unclassified! !
!MatrixTest methodsFor!

matrixClassToTest
	^Matrix!

testAddition
	| m1 m2 m3 m4 |

	m1 := self matrixClassToTest new: 4@4 from: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16).
	m2 := self matrixClassToTest new: 4@4 from: #(32 30 28 26 24 22 20 18 16 14 12 10 8 6 4 2).

	self shouldnt: [ m3 := m1 + m2 ] raise: MatrixError.
	self should: [ m3 values = #(33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18) ].

	m4 := self matrixClassToTest identity: 3.
	self should: [ m1 + m4 ] raise: IncompatibleMatrixError!

testAsVector
	| m1 v1 m2 v2 m3 val |

	m1 := self matrixClassToTest new: 1@4 from: #(8 7 6 5).
	self shouldnt: [ v1 := m1 asVector ] raise: Exception.
	self should: [ v1 values = m1 values ].
	self shouldnt: [ v1 values == m1 values ].

	"Ensure values are deep-copied properly"

	m1 at: 1 at: 1 put: 99.
	self should: [ (m1 values at: 1) = 99 ].
	self shouldnt: [ (v1 at: 1) = 99 ].

	m2 := self matrixClassToTest new: 5@1 from: #(3 4 5 6 7).
	self shouldnt: [ v2 := m2 asVector ] raise: Exception.
	self should: [ v2 values = m2 values ].

	m3 := self matrixClassToTest new: 3@3 from: #(9 8 7 6 5 4 3 2 1).
	self should: [ m3 asVector ] raise: IncompatibleMatrixError!

testAtAt
	| m1 |

	m1 := self matrixClassToTest identity: 4.

	self should: [ (m1 at: 1 at: 1) = 1 ].
	self should: [ (m1 at: 2 at: 3) = 0 ].
	self should: [ m1 at: 2 at: 5 ] raise: MatrixError.!

testAtAtPut
	| m1 |

	m1 := self matrixClassToTest identity: 4.

	self should: [ m1 at: 1 at: 2 put: 3.  (m1 at: 1 at: 2) = 3 ].!

testColumn
	| m |

	m := self matrixClassToTest new: 3@3 from: #(2 4 6 8 10 12 14 16 18).
	self should: [ (m column: 2) = (self matrixClassToTest new: 3@1 from: #(4 10 16)) ]!

testColumnPut
	| m v |

	m := self matrixClassToTest new: 3@3 from: #(18 17 16 15 14 13 12 11 10).

	m column: 2 put: (Vector from: #(4 10 16)).
	self should: [ (m column: 2) = (self matrixClassToTest new: 3@1 from: #(4 10 16)) ]!

testColumns
	| m |

	m := self matrixClassToTest new: 4@4 from: #(10 9 8 7 6 5 4 3 2 1 10 9 8 7 6 5).
	self should: [ m columns = 4 ]!

testColumnVector
	| m |

	m := self matrixClassToTest new: 3@3 from: #(2 4 6 8 10 12 14 16 18).
	self should: [ (m columnVector: 2) = (Vector from: #(4 10 16)) ]!

testComputeLuForm
	| m1 mSingular lu |

	m1 := self matrixClassToTest new: 4@4 from: #(1 0 2 3 2 3 0 4 3 4 5 0 0 6 7 8).
	self shouldnt: [ lu := m1 luForm ] raise: MatrixError.
	self should: [ lu = (self matrixClassToTest new: 4@4 from: #(3 4 5 0
									    0 6 7 8
									    ##(2/3) ##(1/18) ##(-67/18) ##(32/9)
									    ##(1/3) ##(-2/9) ##(-34/67) ##(441/67))) ].
	self should: [ (m1 invertByCofactors * m1) = (self matrixClassToTest identity: 4) ].
	self should: [ (m1 invertByDecompositionAndBackSubstitution * m1) = (self matrixClassToTest identity: 4) ].

	mSingular := self matrixClassToTest new: 3@3 from: #(1 2 3 4 5 6 7 8 9).
	"self should: [ mSingular luForm ] raise: SingularMatrixError."
	self should: [ mSingular determinantByCofactors = 0 ].
	self should: [ mSingular determinantByLuDecomposition <= mSingular comparisonTolerance ]!

testDeterminant
	| m1 mSingular |

	m1 := self matrixClassToTest new: 4@4 from: #(6 0 1 3 4 7 9 2 8 1 1 6 7 9 8 4).
	self should: [ m1 isSingular = false ].
	self should: [ (m1 determinant - 294) abs <= m1 comparisonTolerance ].
	self should: [ (m1 determinantByCofactors - 294) abs <= m1 comparisonTolerance ].
	self should: [ (m1 determinantByLuDecomposition - 294) abs <= m1 comparisonTolerance ].

	mSingular := self matrixClassToTest new: 3@3 from: #(1 2 3 4 5 6 7 8 9).
	self should: [ mSingular isSingular = true ].
	self should: [ mSingular determinant = 0 ].
	self should: [ mSingular determinantByCofactors = 0 ].
	self should: [ mSingular determinantByLuDecomposition = 0 ].
!

testEquivalence
	| m1 m2 m3 |

	m1 := self matrixClassToTest new: 3@2 from: #(6 4 2 5 3 1).
	m2 := (m1 * 2) * (1/2).
	self should: [ m1 = m2 ].

	m3 := self matrixClassToTest new: 3@2 from: #(1 2 3 4 5 6).
	self shouldnt: [ m1 = m3 ]!

testIdentity
	| m |

	m := self matrixClassToTest identity: 3.
	self should: [ m values = #(1 0 0 0 1 0 0 0 1) ].
	self should: [ m order = (3@3) ]!

testInverse
	| m1 mSingular |

	m1 := self matrixClassToTest new: 4@4 from: #(6 0 1 3 4 7 9 2 8 1 1 6 7 9 8 4).
	self should: [ m1 inverse = (self matrixClassToTest new: 4@4 from: #(##(10/21) ##(-47/294) ##(-85/294) ##(23/147)
										##(-2/21) ##(-83/294) ##(-25/294) ##(50/147)
										0 ##(5/14) ##(1/14) ##(-2/7)
										##(-13/21) ##(59/294) ##(163/294) ##(-32/147))) ].

	mSingular := self matrixClassToTest new: 3@3 from: #(1 2 3 4 5 6 7 8 9).
	self should: [ mSingular inverse ] raise: SingularMatrixError
!

testMatrixMultiplication
	| m1 m2 m3 m4 |

	m1 := self matrixClassToTest new: 3@4 from: #(1 2 3 4 5 6 7 8 9 10 11 12).
	m2 := self matrixClassToTest new: 4@2 from: #(8 7 6 5 2 4 3 1).

	self shouldnt: [ m3 := m1 * m2 ] raise: MatrixError.
	self should: [ m3 order = (3@2) ].
	self should: [ m3 values = #(38 33 114 101 190 169) ].

	m4 := self matrixClassToTest identity: 5.
	self should: [ m1 * m4 ] raise: IncompatibleMatrixError.!

testNew
	| m |

	m := self matrixClassToTest new: 3@4.
	self should: [ m values = #(nil nil nil nil nil nil nil nil nil nil nil nil) ].
	self should: [ m rows = 3 ].
	self should: [ m columns = 4 ]!

testNewFrom
	| m |

	m := self matrixClassToTest new: 5@2 from: #(10 9 8 7 6 1 2 3 4 5).
	self should: [ m values = #(10 9 8 7 6 1 2 3 4 5) ].
	self should: [ m rows = 5 ].
	self should: [ m columns = 2 ]
!

testScalarMultiplication
	| m m2 result desired |

	m := self matrixClassToTest new: 4@3 from: #(1 2 3 4 5 6 7 8 9 10 11 12).
	m2 := m * 1.75.		"Float"
	self should: [ m2 values = #(1.75 3.5 5.25 7.0 8.75 10.5 12.25 14.0 15.75 17.5 19.25 21.0) ].
	m2 := m * (7/5).		"Fraction"
	self should: [ result := true.
			desired := #(##(7/5) ##(14/5) ##(21/5) ##(28/5) 7 ##(42/5) ##(49/5) ##(56/5) ##(63/5)
						14 ##(77/5) ##(84/5)).
			1 to: m2 values size do: [ :i |
				((desired at: i) - (m2 values at: i)) abs <= m2 comparisonTolerance ifFalse: [
					Transcript show: i printString; cr.  result := false ] ].
			result ].
	m2 := m * 2.		"Integer"
	self should: [ m2 values = #(2 4 6 8 10 12 14 16 18 20 22 24) ].
	m2 := m * 1.23s.		"Scaled decimal"
	self should: [ m2 values = #(1.23s 2.46s 3.69s 4.92s 6.15s 7.38s 8.61s 9.84s 11.07s 12.3s 13.53s 14.76s) ].
!

testSolveFor
	| m mSingular |

	m := Matrix new: 3@3 from: #(3 2 4 2 3 1 1 4 ##(1/4)).

	self should: [ (m solveFor: (Vector from: #(16 10 4))) = (Vector from: #(##(28/5) ##(-2/5) 0)) ].
	self should: [ (m solveFor: (Vector from: #(1 3))) ] raise: IncompatibleVectorError.

	mSingular := Matrix new: 3@3 from: #(1 2 3 4 5 6 7 8 9).
	self should: [ (mSingular solveFor: (Vector from: #(1 2 3))) ] raise: SingularMatrixError.!

testSubtraction
	| m1 m2 m3 m4 |

	m1 := self matrixClassToTest new: 4@4 from: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16).
	m2 := self matrixClassToTest new: 4@4 from: #(32 30 28 26 24 22 20 18 16 14 12 10 8 6 4 2).

	self shouldnt: [ m3 := m2 - m1 ] raise: IncompatibleMatrixError.
	self should: [ m3 values = #(31 28 25 22 19 16 13 10 7 4 1 -2 -5 -8 -11 -14) ].

	m4 := self matrixClassToTest identity: 3.
	self should: [ m1 - m4 ] raise: IncompatibleMatrixError!

testZero
	| m |

	m := self matrixClassToTest zeroSquare: 4.
	self should: [ m values = #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) ].
	self should: [ m rows = 4 ].
	self should: [ m columns = 4 ]! !
!MatrixTest categoriesFor: #matrixClassToTest!accessing!public! !
!MatrixTest categoriesFor: #testAddition!public!testing! !
!MatrixTest categoriesFor: #testAsVector!public!testing! !
!MatrixTest categoriesFor: #testAtAt!public!testing! !
!MatrixTest categoriesFor: #testAtAtPut!public!testing! !
!MatrixTest categoriesFor: #testColumn!public!testing! !
!MatrixTest categoriesFor: #testColumnPut!public!testing! !
!MatrixTest categoriesFor: #testColumns!public!testing! !
!MatrixTest categoriesFor: #testColumnVector!public!testing! !
!MatrixTest categoriesFor: #testComputeLuForm!public!testing! !
!MatrixTest categoriesFor: #testDeterminant!public!testing! !
!MatrixTest categoriesFor: #testEquivalence!public!testing! !
!MatrixTest categoriesFor: #testIdentity!public!testing! !
!MatrixTest categoriesFor: #testInverse!public!testing! !
!MatrixTest categoriesFor: #testMatrixMultiplication!public!testing! !
!MatrixTest categoriesFor: #testNew!public!testing! !
!MatrixTest categoriesFor: #testNewFrom!public!testing! !
!MatrixTest categoriesFor: #testScalarMultiplication!public!testing! !
!MatrixTest categoriesFor: #testSolveFor!public!testing! !
!MatrixTest categoriesFor: #testSubtraction!public!testing! !
!MatrixTest categoriesFor: #testZero!public!testing! !

VectorTest guid: (GUID fromString: '{728BDAE0-4E08-11D5-BE18-00010240D5E2}')!
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

FloatMatrixTest guid: (GUID fromString: '{1EBB4965-0174-4958-8ADE-BE3E9CF3F6A0}')!
FloatMatrixTest comment: ''!
!FloatMatrixTest categoriesForClass!Unclassified! !
!FloatMatrixTest methodsFor!

matrixClassToTest
	^FloatMatrix! !
!FloatMatrixTest categoriesFor: #matrixClassToTest!accessing!public! !

"Binary Globals"!

"Resources"!

