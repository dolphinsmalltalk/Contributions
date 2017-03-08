"Filed out from Dolphin Smalltalk 7"!

Matrix subclass: #ComplexMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ComplexMatrix guid: (GUID fromString: '{70ac856d-dcbb-4895-af89-0d74e23bafbb}')!
ComplexMatrix comment: ''!
!ComplexMatrix categoriesForClass!Unclassified! !
!ComplexMatrix methodsFor!

= aMatrix
	^aMatrix compareToComplexMatrix: self!

basicAt: anIntegerRow at: anIntegerColumn put: aValue
	super basicAt: anIntegerRow at: anIntegerColumn put: aValue asComplex! !
!ComplexMatrix categoriesFor: #=!public! !
!ComplexMatrix categoriesFor: #basicAt:at:put:!private! !

!ComplexMatrix class methodsFor!

defaultComparisonTolerance
	^1e-14!

new: aPoint from: anArray
	| result |

	"Verify that enough data is supplied to initialize the matrix."

	anArray size = (aPoint x * aPoint y) ifFalse: [ MatrixError signal: 'Initialization array wrong size' ].

	result := self new: aPoint.
	result values: (anArray collect: [ :aNumber | aNumber asComplex deepCopy ]).
	^result! !
!ComplexMatrix class categoriesFor: #defaultComparisonTolerance!private! !
!ComplexMatrix class categoriesFor: #new:from:!public! !

