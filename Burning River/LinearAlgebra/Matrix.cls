"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Matrix
	instanceVariableNames: 'order values luForm comparisonTolerance'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Matrix guid: (GUID fromString: '{9f5af640-40c9-11d5-be18-00010240d5e2}')!
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

