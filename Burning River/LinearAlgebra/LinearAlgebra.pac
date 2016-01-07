| package |
package := Package name: 'LinearAlgebra'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #FloatMatrix;
	add: #IncompatibleMatrixError;
	add: #IncompatibleVectorError;
	add: #LuMatrix;
	add: #Matrix;
	add: #MatrixError;
	add: #SingularMatrixError;
	add: #Vector;
	add: #VectorError;
	yourself.

package methodNames
	add: #Number -> #divideVector:;
	add: #Number -> #multiplyMatrix:;
	add: #Number -> #multiplyVector:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Matrix
	instanceVariableNames: 'order values luForm comparisonTolerance'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Vector
	instanceVariableNames: 'values'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #MatrixError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #VectorError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MatrixError subclass: #IncompatibleMatrixError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MatrixError subclass: #SingularMatrixError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VectorError subclass: #IncompatibleVectorError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Matrix subclass: #FloatMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Matrix subclass: #LuMatrix
	instanceVariableNames: 'sourceMatrix rowInterchangeSign rowPermutations'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Number methodsFor!

divideVector: aVector
	^aVector multipliedByScalar: (1 / self)!

multiplyMatrix: aMatrix
	^aMatrix multipliedByScalar: self!

multiplyVector: aVector
	^aVector multipliedByScalar: self! !
!Number categoriesFor: #divideVector:!double dispatch!public! !
!Number categoriesFor: #multiplyMatrix:!double dispatch!private! !
!Number categoriesFor: #multiplyVector:!double dispatch!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

Matrix guid: (GUID fromString: '{9F5AF640-40C9-11D5-BE18-00010240D5E2}')!
Matrix comment: 'Defines a matrix.  Some notes:
	1.  The order of the matrix is defined by a Point.  The x coordinate of the Point defines the number of
	     rows in the matrix, while the y coordinate defines the number of columns.
	2.  Similarly, a number of methods use Points to specify an element within the matrix.  Here again the
	     x coordinate of the Point defines the row while the y coordinate defines column.

	luForm is this matrix in LU-decomposed form.  Because this is relatively expensive to compute we determine this
		once and then use it when computing inverses, calculating determinants, and doing back/forward
		substitution.'!
!Matrix categoriesForClass!Unclassified! !
!Matrix methodsFor!

- aMatrix
	"Answer the result of subtracting 'aMatrix' from this matrix."

	| resultData |

	self order = aMatrix order ifFalse: [ IncompatibleMatrixError signal ].

	resultData := Array new: (self values size).

	1 to: self values size do: [ :i | resultData at: i put: (self values at: i) - (aMatrix values at: i) ].
	^self class new: self order from: resultData
	!

* aMatrixOrScalar
	^aMatrixOrScalar multiplyMatrix: self!

+ aMatrix
	"Answer the result of adding this matrix to 'aMatrix'."

	| resultData |

	self order = aMatrix order ifFalse: [ IncompatibleMatrixError signal ].

	resultData := Array new: (self values size).

	1 to: self values size do: [ :i | resultData at: i put: (self values at: i) + (aMatrix values at: i) ].
	^self class new: self order from: resultData
	!

= aMatrix
	self order = aMatrix order ifFalse: [ ^ false ].
	1 to: self rows do: [ :r |
		1 to: self columns do: [ :c |
			(((self at: r at: c)  - (aMatrix at: r at: c)) abs <= self comparisonTolerance) ifFalse: [ ^false ] ] ].
	^true!

adjoint
	| mat |

	mat := self class new: self order.

	"Note that we perform the transpose operation below rather than
	 sending #transpose.  This is done to improve performance"

	1 to: self rows do: [ :r |
		1 to: self columns do: [ :c |
			mat basicAt: c at: r put: (self cofactor: r@c) ] ].

	^mat!

asVector
	"Answer the receiver as a vector.  One of the dimensions of this matrix must be 1 for this to be valid."

	(self rows = 1) | (self columns = 1) ifFalse: [
		IncompatibleMatrixError signal: 'Only matrices with a single row or column can be converted to vectors' ].

	^Vector from: self values!

at: aPoint
	^self at: aPoint x at: aPoint y!

at: anIntegerRow at: anIntegerColumn
	self validate: anIntegerRow@anIntegerColumn.
	^self basicAt: anIntegerRow at: anIntegerColumn
!

at: anIntegerRow at: anIntegerColumn put: aValue
	self validate: anIntegerRow@anIntegerColumn.
	self basicAt: anIntegerRow at: anIntegerColumn put: aValue
!

at: aPoint put: aValue
	^self at: aPoint x at: aPoint y put: aValue!

basicAt: anIntegerRow at: anIntegerColumn
	^self values at: (anIntegerRow-1 * self columns) + anIntegerColumn!

basicAt: anIntegerRow at: anIntegerColumn put: aValue
	self values at: (anIntegerRow-1 * self columns) + anIntegerColumn put: aValue.
	self luForm: nil!

cofactor: aPoint
	(aPoint x + aPoint y) odd
		ifTrue: [ ^-1 * (self minor: aPoint) ]
		ifFalse: [ ^self minor: aPoint ]!

column: anInteger
	"Answer a single-column Matrix containing only the data from the 'anInteger'th column of this Matrix"

	| data |

	(anInteger < 0) | (anInteger > self columns) ifTrue: [ MatrixError signal: 'Index out of range' ].

	data := Array new: self rows.
	1 to: self rows do: [ :i | data at: i put: (self basicAt: i at: anInteger) ].
	^self class new: self rows @ 1 from: data
!

column: anInteger put: aVector
	"Replace the data in column 'anInteger' with values from aVector."

	aVector order ~= self rows ifTrue: [ MatrixError signal: 'Vector not compatible with this matrix' ].

	1 to: aVector order do: [ :i | self basicAt: i at: anInteger put: (aVector at: i) ]!

columns
	^self order y!

columnVector: anInteger
	^(self column: anInteger) asVector!

comparisonTolerance
	comparisonTolerance isNil ifTrue: [ comparisonTolerance := self class defaultComparisonTolerance ].
	^comparisonTolerance!

comparisonTolerance: aNumber
	comparisonTolerance := aNumber!

determinant
	"Answer the determinant of this matrix.

	Note that if your only interest in the determinant is to decide whether or not the matrix is singular you're
	better off using #isSingular which is not subject to numeric overflow."

	^self determinantByLuDecomposition!

determinantByCofactors
	"Answer the determinant of this matrix."

	| retval |

	self isSquare ifFalse: [ MatrixError signal: 'Matrix must be square' ].

	self order = (3@3) ifTrue: [
		^((self basicAt: 1 at: 1) * (((self basicAt: 2 at: 2)*(self basicAt: 3 at: 3)) - ((self basicAt: 3 at: 2)*(self basicAt: 2 at: 3)))) -
		  ((self basicAt: 1 at: 2) * (((self basicAt: 2 at: 1)*(self basicAt: 3 at: 3)) - ((self basicAt: 3 at: 1)*(self basicAt: 2 at: 3)))) +
		  ((self basicAt: 1 at: 3) * (((self basicAt: 2 at: 1)*(self basicAt: 3 at: 2)) - ((self basicAt: 3 at: 1)*(self basicAt: 2 at: 2)))) ].

	self order = (2@2) ifTrue: [
		^((self basicAt: 1 at: 1)*(self basicAt: 2 at: 2)) - ((self basicAt: 1 at: 2)*(self basicAt: 2 at: 1)) ].

	self order = (1@1) ifTrue: [ ^self basicAt: 1 at: 1 ].

	"General case"

	^(1 to: self columns) inject: 0 into: [ :tot :c | tot + ((self basicAt: 1 at: c) * (self cofactor: 1@c)) ]!

determinantByLuDecomposition
	"Answer the determinant of a matrix."

	| lu prod |

	lu := self luForm.
	prod := self luForm rowInterchangeSign.

	1 to: lu rows do: [ :i |
		(lu at: i at: i) <= lu comparisonTolerance ifTrue: [ ^0 ].
		prod := prod * (lu at: i at: i) asFraction ].

	^prod!

hash
	^self rows * self columns!

initialize: aPoint
	self order: aPoint!

initializeWith: aDiadicBlock
	1 to: self rows do: [ :r |
		1 to: self columns do: [ :c |
			self basicAt: r at: c put: (aDiadicBlock value: r value: c) ] ]!

inverse
	^self invertByDecompositionAndBackSubstitution!

invertByCofactors
	| determinant |
	(determinant := self determinantByCofactors) = 0 ifTrue: [ SingularMatrixError signal ].
	^self adjoint * (1 / determinant)!

invertByDecompositionAndBackSubstitution
	| lu inverse col |

	self isSingular ifTrue: [ SingularMatrixError signal ].
	lu := self luForm.
	inverse := self class identity: self rows.
	1 to: lu columns do: [ :c |
		inverse column: c put: (lu backSubstitute: (inverse columnVector: c)) ].
	^inverse!

isSingular
	"Determine if this matrix is singular.  Commonly this would be done by testing to see if the determinant is
	zero, but this presents problems if the determinant calculation overflows the numeric type used (floating-
	point numbers are prone to this for large matrices).  To get around this we note that the determinant of a
	matrix can be computed by multiplying the values on the major diagonal of the LU decomposition of the
	original matrix, and that the results of this multiplication can only be zero (and thus the original matrix
	can only be singular) if one of the elements on the diagonal of the LU decomposed matrix is zero.  Although
	we can (and in fact do) work around the floating-point overflow problem, this method of determining if a
	matrix is singular replaces multiplications with comparisons, which should be less time-consuming."

	| lu |

	[ lu := self luForm ] on: SingularMatrixError do: [ :e | ^true ].
	1 to: lu rows do: [ :i |
		(lu basicAt: i at: i) abs <= self comparisonTolerance
			ifTrue: [ ^true ] ].
	^false!

isSquare
	^self rows = self columns!

luForm
	luForm isNil ifTrue: [ luForm := LuMatrix from: self ].
	^luForm!

luForm: aMatrix
	luForm := aMatrix!

minor: aPoint
	^(self subMatrix: aPoint) determinantByCofactors!

multipliedByScalar: aNumber
	| result |

	result := self class new: self order.
	1 to: self values size do: [ :i | result values at: i put: (self values at: i) * aNumber ].
	^result
!

multiplyByFloat: aFloat
	^self multipliedByScalar: aFloat!

multiplyByFraction: aFraction
	^self multipliedByScalar: aFraction!

multiplyByInteger: aInteger
	^self multipliedByScalar: aInteger!

multiplyByScaledDecimal: aScaledDecimal
	^self multipliedByScalar: aScaledDecimal!

multiplyMatrix: aMatrix
	"Private - Answer the result of multiplying aMatrix by self."

	| result |

	aMatrix columns = self rows ifFalse: [ IncompatibleMatrixError signal ].

	result := self class new: aMatrix rows @ self columns.

	1 to: aMatrix rows do: [ :r |
		1 to: self columns do: [ :c |
			result basicAt: r at: c put: ((aMatrix rowVector: r) dotProduct: (self columnVector: c)) ] ].
	^result!

multiplyRow: anInteger by: aScalar
	"Type 2 elementary row op- multiply a row by a scalar."

	aScalar = 0 ifTrue: [ MatrixError signal: 'Scalar must not be zero' ].
	1 to: self columns do: [ :c | self basicAt: anInteger at: c put: (self basicAt: anInteger at: c) * aScalar ]!

multiplyRow: firstRowIndex by: aScalar andAddTo: secondRowIndex
	"Type3 elementary row operation."

	| vec |

	vec := (((self row: firstRowIndex) * aScalar) + (self row: secondRowIndex)) asVector.
	self row: secondRowIndex put: vec!

negated
	^self * -1!

order
	^order!

order: aPoint
	order := aPoint.
	self values: (Array new: aPoint x * aPoint y)!

row: anInteger
	"Answer a single-row Matrix containing only the data from the 'anInteger'th row of this Matrix"

	| data |

	(anInteger < 0) | (anInteger > self rows) ifTrue: [ MatrixError signal: 'Index out of range' ].

	data := Array new: self columns.
	1 to: self columns do: [ :i | data at: i put: (self basicAt: anInteger at: i) ].
	^self class new: 1 @ self columns from: data!

row: anInteger put: aVector
	"Replace the data in row 'anInteger' with values from aVector."

	aVector order ~= self columns ifTrue: [ MatrixError signal: 'Vector not compatible with this matrix' ].

	1 to: aVector order do: [ :i | self basicAt: anInteger at: i put: (aVector at: i) ]!

rows
	^self order x!

rowVector: anInteger
	^(self row: anInteger) asVector!

solveFor: aVector
	"Assuming that this matrix is a matrix of coefficients (A) for a system of linear equations of the form

			A * X = B

	and aVector is the solution vector B, solve for X.  Answers the vector X which satisfies the
	satisfies the given system of linear equations."

	^self luForm backSubstitute: aVector!

subMatrix: aPoint
	| retval retRow retCol |

	retval := self class new: (self rows-1)@(self columns-1).
	retRow := 1.
	retCol := 1.

	1 to: self rows do: [ :r |
		r ~= aPoint x ifTrue: [
			1 to: self columns do: [ :c |
				c ~= aPoint y ifTrue: [
					retval basicAt: retRow at: retCol put: (self basicAt: r at: c).
					retCol := retCol + 1 ] ].
			retRow := retRow + 1 ].
		retCol := 1 ].
	^retval!

swapRow: firstRowIndex withRow: secondRowIndex
	"Type 1 elementary row op - swap rows."

	| firstRow |

	(firstRowIndex > 0) & (firstRowIndex <= self rows) & (secondRowIndex <= self rows) & (secondRowIndex > 0)
		ifFalse: [ BoundsError signal ].

	firstRow := self rowVector: firstRowIndex.
	1 to: self columns do: [ :c |
		self basicAt: firstRowIndex at: c put: (self basicAt: secondRowIndex at: c).
		self basicAt: secondRowIndex at: c put: (firstRow at: c) ]!

transpose
	| mat |

	mat := self class new: self columns@self rows.

	1 to: self rows do: [ :r |
		1 to: self columns do: [ :c |
			mat basicAt: c at: r put: (self basicAt: r at: c) ] ].
	^mat!

validate: aPoint
	(aPoint x <= 0) | (aPoint x > self rows) | (aPoint y <= 0) | (aPoint y > self columns) ifTrue: [
		MatrixError signal: 'Index out of range' ]!

values
	^values!

values: anArray
	values := anArray.
	self luForm: nil! !
!Matrix categoriesFor: #-!arithmetic!public! !
!Matrix categoriesFor: #*!arithmetic!public! !
!Matrix categoriesFor: #+!arithmetic!public! !
!Matrix categoriesFor: #=!comparing!public! !
!Matrix categoriesFor: #adjoint!cofactors!operations!public! !
!Matrix categoriesFor: #asVector!converting!public! !
!Matrix categoriesFor: #at:!accessing!public! !
!Matrix categoriesFor: #at:at:!accessing!public! !
!Matrix categoriesFor: #at:at:put:!accessing!public! !
!Matrix categoriesFor: #at:put:!accessing!public! !
!Matrix categoriesFor: #basicAt:at:!helpers!private! !
!Matrix categoriesFor: #basicAt:at:put:!helpers!private! !
!Matrix categoriesFor: #cofactor:!cofactors!operations!public! !
!Matrix categoriesFor: #column:!accessing!public! !
!Matrix categoriesFor: #column:put:!accessing!public! !
!Matrix categoriesFor: #columns!accessing!public! !
!Matrix categoriesFor: #columnVector:!accessing!public! !
!Matrix categoriesFor: #comparisonTolerance!accessing!public! !
!Matrix categoriesFor: #comparisonTolerance:!accessing!public! !
!Matrix categoriesFor: #determinant!operations!public! !
!Matrix categoriesFor: #determinantByCofactors!helpers!private! !
!Matrix categoriesFor: #determinantByLuDecomposition!helpers!private! !
!Matrix categoriesFor: #hash!comparing!public! !
!Matrix categoriesFor: #initialize:!initializing!private! !
!Matrix categoriesFor: #initializeWith:!initializing!private! !
!Matrix categoriesFor: #inverse!operations!public! !
!Matrix categoriesFor: #invertByCofactors!helpers!private! !
!Matrix categoriesFor: #invertByDecompositionAndBackSubstitution!helpers!private! !
!Matrix categoriesFor: #isSingular!public!testing! !
!Matrix categoriesFor: #isSquare!public!testing! !
!Matrix categoriesFor: #luForm!accessing!public! !
!Matrix categoriesFor: #luForm:!accessing!private! !
!Matrix categoriesFor: #minor:!cofactors!public! !
!Matrix categoriesFor: #multipliedByScalar:!double dispatch!private! !
!Matrix categoriesFor: #multiplyByFloat:!double dispatch!private! !
!Matrix categoriesFor: #multiplyByFraction:!double dispatch!private! !
!Matrix categoriesFor: #multiplyByInteger:!double dispatch!private! !
!Matrix categoriesFor: #multiplyByScaledDecimal:!double dispatch!private! !
!Matrix categoriesFor: #multiplyMatrix:!double dispatch!private! !
!Matrix categoriesFor: #multiplyRow:by:!elementary row operations!public! !
!Matrix categoriesFor: #multiplyRow:by:andAddTo:!elementary row operations!public! !
!Matrix categoriesFor: #negated!arithmetic!operations!public! !
!Matrix categoriesFor: #order!accessing!public! !
!Matrix categoriesFor: #order:!accessing!private! !
!Matrix categoriesFor: #row:!accessing!public! !
!Matrix categoriesFor: #row:put:!accessing!public! !
!Matrix categoriesFor: #rows!accessing!public! !
!Matrix categoriesFor: #rowVector:!accessing!public! !
!Matrix categoriesFor: #solveFor:!operations!public! !
!Matrix categoriesFor: #subMatrix:!cofactors!public! !
!Matrix categoriesFor: #swapRow:withRow:!elementary row operations!public! !
!Matrix categoriesFor: #transpose!operations!public! !
!Matrix categoriesFor: #validate:!helpers!private! !
!Matrix categoriesFor: #values!accessing!private! !
!Matrix categoriesFor: #values:!accessing!private! !

!Matrix class methodsFor!

defaultComparisonTolerance
	^0!

hilbert: anInteger
	"Answer a Hilbert matrix of order 'anInteger@anInteger'."

	^self new: anInteger@anInteger initializeWith: [ :r :c | 1 / (r + c - 1) ]!

identity: anInteger
	"Answer an identity matrix of order 'anInteger@anInteger'."

	| mat |

	mat := self zero: anInteger@anInteger.
	1 to: anInteger do: [ :i | mat at: i at: i put: 1 ].
	^mat!

new: aPoint
	^self new initialize: aPoint!

new: aPoint from: anArray
	| result |

	"Verify that enough data is supplied to initialize the matrix."

	anArray size = (aPoint x * aPoint y) ifFalse: [ MatrixError signal: 'Initialization array wrong size' ].

	result := self new: aPoint.
	result values: anArray deepCopy.
	^result!

new: aPoint initializeWith: aDiadicBlock
	| result |

	result := self new: aPoint.
	result initializeWith: aDiadicBlock.
	^result!

newSquare: anInteger
	"Answer a square matrix of order 'anInteger@anInteger'."

	^self new: anInteger@anInteger!

ones: aPoint
	"Answer a matrix of order 'aPoint' filled with ones."

	^self new: aPoint from: ((Array new: aPoint x * aPoint y) atAllPut: 1)!

zero: aPoint
	"Answer a zero matrix of order 'aPoint'."

	^self new: aPoint from: ((Array new: aPoint x * aPoint y) atAllPut: 0)!

zeroSquare: anInteger
	"Answer a square matrix of order 'anInteger@anInteger' filled with zeros."

	^self zero: anInteger@anInteger! !
!Matrix class categoriesFor: #defaultComparisonTolerance!accessing!private! !
!Matrix class categoriesFor: #hilbert:!instance creation!public! !
!Matrix class categoriesFor: #identity:!instance creation!public! !
!Matrix class categoriesFor: #new:!instance creation!public! !
!Matrix class categoriesFor: #new:from:!instance creation!public! !
!Matrix class categoriesFor: #new:initializeWith:!instance creation!public! !
!Matrix class categoriesFor: #newSquare:!instance creation!public! !
!Matrix class categoriesFor: #ones:!instance creation!public! !
!Matrix class categoriesFor: #zero:!instance creation!public! !
!Matrix class categoriesFor: #zeroSquare:!instance creation!public! !

Vector guid: (GUID fromString: '{9F5AF641-40C9-11D5-BE18-00010240D5E2}')!
Vector comment: 'A vector is an ordered group of numbers which behaves in mathematically interesting ways.  '!
!Vector categoriesForClass!Unclassified! !
!Vector methodsFor!

- aVector
	| result |

	self order ~= aVector order ifTrue: [ IncompatibleVectorError signal ].

	result := Vector new: self order.
	1 to: self order do: [ :i | result at: i put: (self at: i) - (aVector at: i) ].
	^result!

* aNumberOrVector
	^aNumberOrVector multiplyVector: self!

/ aNumberOrVector
	^aNumberOrVector divideVector: self!

\\ aNumber
	^self collect: [ :each | each \\ aNumber ]!

+ aVector
	| result |

	self order ~= aVector order ifTrue: [ IncompatibleVectorError signal ].

	result := Vector new: self order.
	1 to: self order do: [ :i | result at: i put: (self at: i) + (aVector at: i) ].
	^result!

= aVector
	self order = aVector order ifFalse: [ ^false ].
	1 to: self order do: [ :i | (self at: i) = (aVector at: i) ifFalse: [ ^false ] ].
	^true!

angleFrom: aVector
	"Answer the angle between this vector and aVector."

	^((self dotProduct: aVector) / (self length * aVector length)) arcCos!

asColumn
	"Answer a column matrix (an Nx1 matrix, N=self order) containing the same values as this Vector."

	^Matrix new: self order@1 from: self values!

asRow
	"Answer a row matrix (a 1xN matrix, N=self order) containing the same values as this Vector."

	^Matrix new: 1@self order from: self values!

asVector
	^self!

at: anInteger
	^values at: anInteger!

at: anInteger put: anArithmeticValue
	self values at: anInteger put: anArithmeticValue!

collect: aMonadicValuable
	^self class from: (self values collect: aMonadicValuable)!

displayOn: aStream
	aStream nextPut: $(.
	1 to: self values size do: [ :i |
			(self values at: i) printOn: aStream.
			i < self values size ifTrue: [ aStream nextPut: $  ] ].
	aStream nextPut: $)!

distanceFrom: aVector
	^(self - aVector) length!

divideVector: aVector
	^self reciprocal dotProduct: aVector!

dotProduct: aVector
	"Answer the dot product of this vector with aVector"

	self order ~= aVector order ifTrue: [ 
		IncompatibleVectorError signal: 'Vectors must be the same size to be multiplied' ].

	^((1 to: self order) collect: [ :i | (self at: i) * (aVector at: i) ]) inject: 0 into: [ :tot :val | tot + val ]!

equals: aVector
	self order = aVector order ifFalse: [ ^false ].
	1 to: self order do: [ :i | ((self at: i) equals: (aVector at: i)) ifFalse: [ ^false ] ].
	^true!

initialize: anInteger
	values := Array new: anInteger!

length
	^(self dotProduct: self) sqrt!

multipliedByScalar: aNumber
	| result |

	result := Vector new: self order.
	1 to: self order do: [ :i | result at: i put: (self at: i) * aNumber ].
	^result!

multiplyByFloat: aFloat
	^self multipliedByScalar: aFloat!

multiplyByFraction: aFraction
	^self multipliedByScalar: aFraction!

multiplyByInteger: aInteger
	^self multipliedByScalar: aInteger!

multiplyByScaledDecimal: aScaledDecimal
	^self multipliedByScalar: aScaledDecimal!

multiplyVector: aVector
	^self dotProduct: aVector!

negated
	^self * -1!

order
	^self values size!

orthogonalWith: aVector
	^(self dotProduct: aVector) = 0!

perpendicularTo: aVector
	^self orthogonalWith: aVector!

printOn: aStream
	super printOn: aStream.
	self displayOn: aStream!

reciprocal
	^self collect: [ :each | 1 / each ]!

values
	^values!

values: anArray
	values := anArray! !
!Vector categoriesFor: #-!arithmetic!public! !
!Vector categoriesFor: #*!arithmetic!public! !
!Vector categoriesFor: #/!arithmetic!public! !
!Vector categoriesFor: #\\!arithmetic!public! !
!Vector categoriesFor: #+!arithmetic!public! !
!Vector categoriesFor: #=!comparing!public! !
!Vector categoriesFor: #angleFrom:!operations!public! !
!Vector categoriesFor: #asColumn!converting!public! !
!Vector categoriesFor: #asRow!converting!public! !
!Vector categoriesFor: #asVector!converting!public! !
!Vector categoriesFor: #at:!accessing!public! !
!Vector categoriesFor: #at:put:!accessing!public! !
!Vector categoriesFor: #collect:!enumerating!public! !
!Vector categoriesFor: #displayOn:!printing!public! !
!Vector categoriesFor: #distanceFrom:!operations!public! !
!Vector categoriesFor: #divideVector:!double dispatch!private! !
!Vector categoriesFor: #dotProduct:!operations!public! !
!Vector categoriesFor: #equals:!comparing!public! !
!Vector categoriesFor: #initialize:!initializing!public! !
!Vector categoriesFor: #length!operations!public! !
!Vector categoriesFor: #multipliedByScalar:!double dispatch!private! !
!Vector categoriesFor: #multiplyByFloat:!double dispatch!private! !
!Vector categoriesFor: #multiplyByFraction:!double dispatch!private! !
!Vector categoriesFor: #multiplyByInteger:!double dispatch!private! !
!Vector categoriesFor: #multiplyByScaledDecimal:!double dispatch!private! !
!Vector categoriesFor: #multiplyVector:!double dispatch!private! !
!Vector categoriesFor: #negated!arithmetic!public! !
!Vector categoriesFor: #order!accessing!public! !
!Vector categoriesFor: #orthogonalWith:!operations!public! !
!Vector categoriesFor: #perpendicularTo:!operations!public! !
!Vector categoriesFor: #printOn:!printing!public! !
!Vector categoriesFor: #reciprocal!arithmetic!public! !
!Vector categoriesFor: #values!accessing!public! !
!Vector categoriesFor: #values:!accessing!private! !

!Vector class methodsFor!

from: aCollection
	^self new values: aCollection deepCopy!

new: anInteger
	^self new initialize: anInteger! !
!Vector class categoriesFor: #from:!instance creation!public! !
!Vector class categoriesFor: #new:!instance creation!public! !

MatrixError guid: (GUID fromString: '{BFBA6A02-4968-11D5-BE18-00010240D5E2}')!
MatrixError comment: ''!
!MatrixError categoriesForClass!Unclassified! !
VectorError guid: (GUID fromString: '{AB14EC62-4A0B-11D5-BE18-00010240D5E2}')!
VectorError comment: ''!
!VectorError categoriesForClass!Unclassified! !
IncompatibleMatrixError guid: (GUID fromString: '{AB14EC61-4A0B-11D5-BE18-00010240D5E2}')!
IncompatibleMatrixError comment: ''!
!IncompatibleMatrixError categoriesForClass!Unclassified! !
SingularMatrixError guid: (GUID fromString: '{3913C841-4BAC-11D5-BE18-00010240D5E2}')!
SingularMatrixError comment: ''!
!SingularMatrixError categoriesForClass!Unclassified! !
IncompatibleVectorError guid: (GUID fromString: '{BFBA6A01-4968-11D5-BE18-00010240D5E2}')!
IncompatibleVectorError comment: ''!
!IncompatibleVectorError categoriesForClass!Unclassified! !
FloatMatrix guid: (GUID fromString: '{35A64845-0F44-4768-BF05-84491A3A4E98}')!
FloatMatrix comment: 'A matrix which only stores Floats.  This can provide a performance improvement at the loss of some accuracy.'!
!FloatMatrix categoriesForClass!Unclassified! !
!FloatMatrix methodsFor!

basicAt: anIntegerRow at: anIntegerColumn put: aValue
	super basicAt: anIntegerRow at: anIntegerColumn put: aValue asFloat! !
!FloatMatrix categoriesFor: #basicAt:at:put:!accessing!private! !

!FloatMatrix class methodsFor!

defaultComparisonTolerance
	^1e-14!

new: aPoint from: anArray
	| result |

	"Verify that enough data is supplied to initialize the matrix."

	anArray size = (aPoint x * aPoint y) ifFalse: [ MatrixError signal: 'Initialization array wrong size' ].

	result := self new: aPoint.
	result values: (anArray collect: [ :aNumber | aNumber asFloat ]).
	^result!

ones: aPoint
	"Answer a matrix of order 'aPoint' filled with ones."

	^self new: aPoint from: ((Array new: aPoint x * aPoint y) atAllPut: 1.0)!

zero: aPoint
	"Answer a zero matrix of order 'aPoint'."

	^self new: aPoint from: ((Array new: aPoint x * aPoint y) atAllPut: 0.0)! !
!FloatMatrix class categoriesFor: #defaultComparisonTolerance!accessing!private! !
!FloatMatrix class categoriesFor: #new:from:!instance creation!public! !
!FloatMatrix class categoriesFor: #ones:!instance creation!public! !
!FloatMatrix class categoriesFor: #zero:!instance creation!public! !

LuMatrix guid: (GUID fromString: '{EE9007B2-381A-40B2-848E-CF848B757BAD}')!
LuMatrix comment: 'A matrix produced by the lower-upper decomposition of another matrix.'!
!LuMatrix categoriesForClass!Unclassified! !
!LuMatrix methodsFor!

backSubstitute: aColumnVector
	"Perform forward and backward substitution to solve a series of linear equations of the form

		A * X = B

	This routine assumes that this matrix is in LU decomposed form and that 'aColumnVector'
	contains the right-hand side vector of the above equation.  Answers the resulting column
	vector.  NOTE:  this algorithm is from 'Numerical Recipes' by Press, Flannery, Teukolsky,
	and Vetterling ((c) 1986 Cambridge University Press).  Note that in this version the input
	column vector (aColumnVector) is not destroyed."

	| b n ip ii sum |

	self isSquare ifFalse: [ MatrixError signal: 'Matrix must be square' ].
	self sourceMatrix isSingular ifTrue: [ SingularMatrixError signal: 'Matrix is singular' ].
	aColumnVector order = self rows ifFalse: [ IncompatibleVectorError signal: 'Vector size must match matrix' ].

	b := aColumnVector deepCopy.
	n := self rows.

	ii := 0.
	1 to: n do: [ :i |
		ip := (rowPermutations at: i).
		sum := b at: ip.
		b at: ip put: (b at: i).
		ii ~= 0
			ifTrue: [
				ii to: i-1 do: [ :j |
					sum := sum - ((self basicAt: i at: j) * (b at: j)) ] ]
			ifFalse: [
				sum ~= 0 ifTrue: [
					ii := i ] ].
		b at: i put: sum ].

	n to: 1 by: -1 do: [ :i |
		sum := b at: i.
		i < n ifTrue: [
			i+1 to: n do: [ :j |
				sum := sum - ((self basicAt: i at: j) * (b at: j)) ] ].
		b at: i put: sum / (self basicAt: i at: i) ].

	^b!

basicLuDecomposition
	"Replace this matrix with its LU decomposition.  NOTE:  this algorithm is from 'Numerical Recipes'
	by Press, Flannery, Teukolsky, and Vetterling ((c) 1986 Cambridge University Press).  In this version
	we handle singular matrices by simply returning that we've computed so far, with a zero placed
	strategically upon the major diagonal to ensure that the product of the major diagonal (and thus the
	determinant of the source matrix) is zero."

	| big temp vv sum n dum imax |

	self isSquare ifFalse: [ MatrixError signal: 'Matrix must be square' ].

	n := self rows.
	vv := Array new: n.						"Stores the implicit scaling of each row"
	rowPermutations := Array new: n.

	rowInterchangeSign := 1.					"No row interchanges yet"

	1 to: n do: [ :i |							"Loop over rows to get the implicit scaling information"
		big := 0.
		1 to: n do: [ :j |
			(temp := (self basicAt: i at: j) abs) > big ifTrue: [ big := temp ] ].
		big abs <= self comparisonTolerance ifTrue: [	"No non-zero largest element"
			self at: 1 at: 1 put: 0.				"Make sure we've got an element on the major diagonal"
			^self	].						"which is exactly zero, for computational reasons."
		vv at: i put: 1 / big ].					"Save the scaling"

	1 to: n do: [ :j |							"This is the loop over columns of Crout's method"
		1 to: j-1 do: [ :i |
			sum := self basicAt: i at: j.
			1 to: i-1 do: [ :k |
				sum := sum - ((self basicAt: i at: k) * (self basicAt: k at: j)) ].
			self basicAt: i at: j put: sum ].
		big := 0.							"Initialize for the search for largest pivot element"
		j to: n do: [ :i |
			sum := self basicAt: i at: j.
			1 to: j-1 do: [ :k |
				sum := sum - ((self basicAt: i at: k) * (self basicAt: k at: j)) ].
			self basicAt: i at: j put: sum.
			dum := (vv at: i) * sum abs.			"Figure of merit for the pivot"
			dum > big ifTrue: [				"Is it better than the best so far?"
				big := dum.
				imax := i ] ].
		j ~= imax ifTrue: [					"Do we need to interchange rows?"
			1 to: n do: [ :k |					"Yes, do so..."
				dum := self basicAt: imax at: k.
				self basicAt: imax at: k put: (self basicAt: j at: k).
				self basicAt: j at: k put: dum ].
			rowInterchangeSign := rowInterchangeSign * -1.  "...and change the parity of rowInterchangeSign."
			vv at: imax put: (vv at: j) ].			    "Also interchange the scale factor."
		rowPermutations at: j put: imax.
		(self basicAt: j at: j) abs <= self comparisonTolerance ifTrue: [	"If the pivot element is zero the"
			self basicAt: 1 at:1 put: 0.						"matrix is singular."
			^self ].
		j ~= n ifTrue: [						"Now, finally divide by the pivot element"
			dum := 1 / (self basicAt: j at: j).
			j+1 to: n do: [ :i |
				self basicAt: i at: j put: (self basicAt: i at: j) * dum ] ] ]!

rowInterchangeSign
	^rowInterchangeSign!

sourceMatrix
	^sourceMatrix!

sourceMatrix: aMatrix
	sourceMatrix := aMatrix! !
!LuMatrix categoriesFor: #backSubstitute:!operations!public! !
!LuMatrix categoriesFor: #basicLuDecomposition!operations!private! !
!LuMatrix categoriesFor: #rowInterchangeSign!accessing!public! !
!LuMatrix categoriesFor: #sourceMatrix!accessing!public! !
!LuMatrix categoriesFor: #sourceMatrix:!accessing!public! !

!LuMatrix class methodsFor!

from: aMatrix
	"Create and answer the LU-decomposed form of the source matrix."

	| mat |
	mat := self new: aMatrix order from: aMatrix values.
	mat sourceMatrix: aMatrix.
	mat comparisonTolerance: aMatrix comparisonTolerance.
	^mat basicLuDecomposition! !
!LuMatrix class categoriesFor: #from:!instance creation!public! !

"Binary Globals"!

"Resources"!

