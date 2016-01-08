"Filed out from Dolphin Smalltalk 7"!

ArithmeticValue subclass: #Complex
	instanceVariableNames: 'real imag'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Complex guid: (GUID fromString: '{2C1BFF60-9BA8-471F-857A-77B91B6FA516}')!
Complex comment: ''!
!Complex categoriesForClass!Unclassified! !
!Complex methodsFor!

* operand
	"Answer the result of multiplying the receiver by the <number>
	argument, operand."

	^operand multiplyComplex: self!

/ operand
	"Answer the result of dividing the receiver by <number> argument, 
	operand. Raise a <ZeroDivide> exception if the operand is zero."

	^operand divideIntoComplex: self!

+ operand
	"Answer the sum of the receiver and the <number> argument, 
	operand."

	^operand addToComplex: self!

= aNumber
	^aNumber compareWithComplex: self!

addToComplex: aComplex
	^Complex real: (aComplex realPart + self realPart) imaginary: (aComplex imaginaryPart + self imaginaryPart)!

asComplex
	^self!

ceiling
	^self shouldNotImplement!

coerce: anArithmeticValue
	"Private - Coerce anArithmeticValue (which must be of a lower generality than the receiver) to be the 
	same type as a anArithmeticValue."

	^anArithmeticValue asComplex
	!

compareWithComplex: aComplex
	^(self realPart = aComplex realPart) and: [ self imaginaryPart = aComplex imaginaryPart ]!

displayOn: aStream
	aStream nextPut: $(.
	self realPart printOn: aStream.
	self imaginaryPart >= 0
		ifTrue: [ aStream nextPut: $+ ]
		ifFalse: [ aStream nextPut: $- ].
	self imaginaryPart abs printOn: aStream.
	aStream nextPutAll: 'i)'!

divideIntoComplex: aComplex
	"Implement complex division (a + ib) / (c + id).  Due to double dispatch, in this routine
	self = (c + id) and aComplex = (a + ib)."

	| quotient denominator |

	self realPart abs >= (self imaginaryPart abs)
		ifTrue: [ quotient := self imaginaryPart / self realPart.
			denominator := self realPart + (self imaginaryPart * quotient).
			^Complex real: (aComplex realPart + (aComplex imaginaryPart * quotient)) / denominator
					imaginary: (aComplex imaginaryPart - (aComplex realPart * quotient)) / denominator ]
		ifFalse: [ quotient := self realPart / self imaginaryPart.
			denominator := (self realPart * quotient) + self imaginaryPart.
			^Complex real: ((aComplex realPart * quotient) + aComplex imaginaryPart) / denominator
					imaginary: ((aComplex imaginaryPart * quotient) - aComplex realPart) / denominator ]!

floor
	^self shouldNotImplement!

generality
	"Private - Answer the Smalltalk generality of the receiver, used for performing type conversions"

	^55!

imaginaryPart
	^imag!

imaginaryPart: aNumber
	imag := aNumber!

initialize
	super initialize.
	self realPart: 0.
	self imaginaryPart: 0.!

isReal
	"Answer whether or not this complex number represents a real number, i.e. has no imaginary part."

	^self imaginaryPart = 0!

modulus
	| multiplicand quotient |

	(self realPart abs) >= (self imaginaryPart abs)
		ifTrue: [ multiplicand := self realPart abs.  quotient := (self imaginaryPart) / (self realPart) ]
		ifFalse: [ multiplicand := self imaginaryPart abs.  quotient :=   (self realPart) / (self imaginaryPart) ].
	^multiplicand * ((1 + (quotient * quotient)) sqrt)
!

multiplyComplex: aComplex
	"Private - Implements simple complex multiplication.
	Assumes that the operands have been switched during double dispatch; thus in the multiplication

		(a + bi)(c + di)

	aComplex represents (a + bi) and self represents (c + di)."

	^Complex real: ((aComplex realPart * self realPart) - (aComplex imaginaryPart * self imaginaryPart))
			imaginary: ((aComplex imaginaryPart * self realPart) + (aComplex realPart * self imaginaryPart))!

negated
	^Complex real: self realPart negated imaginary: self imaginaryPart negated!

printOn: aStream
	super printOn: aStream.
	self displayOn: aStream!

raisedTo: operand
	| result |

	result := self.
	2 to: operand do: [ :n | result := result * self ].
	^result!

realPart
	^real!

realPart: aNumber
	real := aNumber!

sqrt
	| w quotient |

	((self realPart = 0) and: [ self imaginaryPart = 0 ]) ifTrue: [ ^Complex zero ].
	self realPart abs >= self imaginaryPart abs
		ifTrue:
			[ quotient := self imaginaryPart / self realPart.
			w := ((self realPart abs) sqrt) * (((1 + (1 + (quotient * quotient)) sqrt) / 2) sqrt) ]
		ifFalse:
			[ quotient := self realPart / self imaginaryPart.
			w := ((self imaginaryPart abs) sqrt) * (((quotient abs + (1 + (quotient * quotient)) sqrt) / 2) sqrt) ].

	self realPart >= 0
		ifTrue:
			[ ^Complex real: w imaginary: (self imaginaryPart / (2 * w)) ]
		ifFalse:
			[ self imaginaryPart >= 0
				ifTrue: [ ^Complex real: (self imaginaryPart abs) / (2 * w) imaginary: w ]
				ifFalse: [ ^Complex real: (self imaginaryPart abs) / (2 * w) imaginary: -1 * w ] ]! !
!Complex categoriesFor: #*!arithmetic!public! !
!Complex categoriesFor: #/!arithmetic!public! !
!Complex categoriesFor: #+!arithmetic!public! !
!Complex categoriesFor: #=!comparing!public! !
!Complex categoriesFor: #addToComplex:!double dispatch!private! !
!Complex categoriesFor: #asComplex!converting!public! !
!Complex categoriesFor: #ceiling!public!rounding! !
!Complex categoriesFor: #coerce:!coercing!private! !
!Complex categoriesFor: #compareWithComplex:!double dispatch!private! !
!Complex categoriesFor: #displayOn:!displaying!public! !
!Complex categoriesFor: #divideIntoComplex:!double dispatch!public! !
!Complex categoriesFor: #floor!public!rounding! !
!Complex categoriesFor: #generality!coercing!private! !
!Complex categoriesFor: #imaginaryPart!accessing!public! !
!Complex categoriesFor: #imaginaryPart:!accessing!public! !
!Complex categoriesFor: #initialize!initialize/release!public! !
!Complex categoriesFor: #isReal!public!testing! !
!Complex categoriesFor: #modulus!accessing!public! !
!Complex categoriesFor: #multiplyComplex:!double dispatch!private! !
!Complex categoriesFor: #negated!arithmetic!public! !
!Complex categoriesFor: #printOn:!printing!public! !
!Complex categoriesFor: #raisedTo:!mathematical!public! !
!Complex categoriesFor: #realPart!accessing!public! !
!Complex categoriesFor: #realPart:!accessing!public! !
!Complex categoriesFor: #sqrt!mathematical!public! !

!Complex class methodsFor!

imaginary: imaginaryNumber
	^self new
		realPart: 0;
		imaginaryPart: imaginaryNumber!

new
	^super new initialize!

one
	^self real: 1!

real: realNumber
	^self new
		realPart: realNumber;
		imaginaryPart: 0!

real: realNumber imaginary: imaginaryNumber
	^self new
		realPart: realNumber;
		imaginaryPart: imaginaryNumber!

zero
	^self new		"By default Complex new produces a zero-valued instance"! !
!Complex class categoriesFor: #imaginary:!object creation!public! !
!Complex class categoriesFor: #new!object creation!public! !
!Complex class categoriesFor: #one!object creation!public! !
!Complex class categoriesFor: #real:!object creation!public! !
!Complex class categoriesFor: #real:imaginary:!object creation!public! !
!Complex class categoriesFor: #zero!object creation!public! !

