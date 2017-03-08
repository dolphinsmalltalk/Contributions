"Filed out from Dolphin Smalltalk 7"!

Matrix subclass: #FloatMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FloatMatrix guid: (GUID fromString: '{35a64845-0f44-4768-bf05-84491a3a4e98}')!
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

