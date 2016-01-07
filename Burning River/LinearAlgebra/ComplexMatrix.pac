| package |
package := Package name: 'ComplexMatrix'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #ComplexMatrix;
	yourself.

package methodNames
	add: #Matrix -> #compareToComplexMatrix:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'LinearAlgebra';
	yourself).

package!

"Class Definitions"!

Matrix subclass: #ComplexMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Matrix methodsFor!

compareToComplexMatrix: aComplexMatrix
	self order = aComplexMatrix order ifFalse: [ ^ false ].
	1 to: self rows do:
		[ :r |
		1 to: self columns do:
			[ :c |
			(((self at: r at: c) asComplex - (aComplexMatrix at: r at: c)) modulus <= self comparisonTolerance)
				ifFalse: [ ^false ] ] ].
	^true
! !
!Matrix categoriesFor: #compareToComplexMatrix:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ComplexMatrix guid: (GUID fromString: '{70AC856D-DCBB-4895-AF89-0D74E23BAFBB}')!
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

"Binary Globals"!

"Resources"!

