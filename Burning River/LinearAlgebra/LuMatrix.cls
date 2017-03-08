"Filed out from Dolphin Smalltalk 7"!

Matrix subclass: #LuMatrix
	instanceVariableNames: 'sourceMatrix rowInterchangeSign rowPermutations'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LuMatrix guid: (GUID fromString: '{ee9007b2-381a-40b2-848e-cf848b757bad}')!
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

