"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Vector
	instanceVariableNames: 'values'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Vector guid: (GUID fromString: '{9f5af641-40c9-11d5-be18-00010240d5e2}')!
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

