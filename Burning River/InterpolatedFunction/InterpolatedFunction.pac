| package |
package := Package name: 'InterpolatedFunction'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #InterpolatedFunction;
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

Object subclass: #InterpolatedFunction
	instanceVariableNames: 'values derivative1 derivativeN secondDerivatives'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

InterpolatedFunction guid: (GUID fromString: '{8F8E1F10-0A42-4C09-B665-8CB9FFC5E9B3}')!
InterpolatedFunction comment: 'This class encapsulates cubic spline interpolation of tabulated functions.  Use InterpolatedFunction class>>for: and InterpolatedFunction class>>for:withEndpointDerivatives to create instances of this class easily.'!
!InterpolatedFunction categoriesForClass!Unclassified! !
!InterpolatedFunction methodsFor!

computeSecondDerivatives
	| n temp sig p t qn un |

	n := self values size.
	temp := Array new: n - 1.
	secondDerivatives := Array new: n.

	derivative1 isNil
		ifTrue: [ secondDerivatives at: 1 put: 0.
			  temp at: 1 put: 0 ]
		ifFalse: [ secondDerivatives at: 1 put: -0.5.
			   temp at: 1 put: 
				(3.0 / ((self x: 2) - (self x: 1))) *
					((((self y: 2) - (self y: 1)) / ((self x: 2) - (self x: 1))) - self derivative1) ].

	2 to: n - 1 do:
		[ :i |
		sig := ((self x: i) - (self x: i-1)) / ((self x: i+1) - (self x: i-1)).
		p := (sig * (secondDerivatives at: i-1)) + 2.
		secondDerivatives at: i put: (sig - 1) / p.
		t := (((self y: i+1) - (self y: i)) / ((self x: i+1) - (self x: i))) -
					(((self y: i) - (self y: i-1)) / ((self x: i) - (self x: i-1))).
		temp at: i put: (((6 * t) / ((self x: i+1) - (self x: i-1))) - (sig * (temp at: i-1))) / p ].

	derivativeN isNil
		ifTrue: [ qn := 0.
			  un := 0 ]
		ifFalse: [ qn := 0.5.
			   un := (3.0 / ((self x: n) - (self x: n-1))) *
				(derivativeN - (((self y: n) - (self y: n-1)) / ((self x: n) - (self x: n-1)))) ].

	secondDerivatives at: n put: (un - (qn * (temp at: n-1))) / ((qn * (secondDerivatives at: n-1)) + 1).

	n-1 to: 1 by: -1 do:
		[ :k |
		secondDerivatives at: k
				put: ((secondDerivatives at: k) * (secondDerivatives at: k+1)) + (temp at: k) ]!

derivative1
	^derivative1!

derivative1: aNumber
	derivative1 := aNumber!

derivativeN
	^derivativeN!

derivativeN: aNumber
	derivativeN := aNumber!

interpolateFor: aNumber
	| lowIndex highIndex i width lowRatio highRatio |

	lowIndex := 1.
	highIndex := self values size.

	[ highIndex - lowIndex > 1 ] whileTrue:
		[ i := (highIndex + lowIndex) // 2.
		(self x: i) > aNumber
			ifTrue: [ highIndex := i ]
			ifFalse: [ lowIndex := i ] ].

	width := (self x: highIndex) - (self x: lowIndex).
	highRatio := ((self x: highIndex) - aNumber) / width.
	lowRatio := (aNumber - (self x: lowIndex)) / width.

	^(highRatio * (self y: lowIndex)) +
		(lowRatio * (self y: highIndex)) +
			(((((highRatio * highRatio * highRatio) - highRatio) * (secondDerivatives at: lowIndex)) +
				(((lowRatio * lowRatio * lowRatio) - lowRatio) * (secondDerivatives at: highIndex))) *
					(width * width) / 6)!

value: aNumber
	"Answer the interpolated y value for the x value specified by aNumber."

	secondDerivatives isNil ifTrue: [ self computeSecondDerivatives ].
	^self interpolateFor: aNumber!

values
	^values!

values: anArrayOfPoints
	values := anArrayOfPoints!

x: aNumber
	"Private - Answer the x value for the given index into values."

	^(self values at: aNumber) x!

y: aNumber
	"Private - Answer the y value for the given index into values."

	^(self values at: aNumber) y! !
!InterpolatedFunction categoriesFor: #computeSecondDerivatives!helpers!private! !
!InterpolatedFunction categoriesFor: #derivative1!accessing!private! !
!InterpolatedFunction categoriesFor: #derivative1:!accessing!private! !
!InterpolatedFunction categoriesFor: #derivativeN!accessing!private! !
!InterpolatedFunction categoriesFor: #derivativeN:!accessing!private! !
!InterpolatedFunction categoriesFor: #interpolateFor:!helpers!private! !
!InterpolatedFunction categoriesFor: #value:!accessing!public! !
!InterpolatedFunction categoriesFor: #values!accessing!private! !
!InterpolatedFunction categoriesFor: #values:!accessing!private! !
!InterpolatedFunction categoriesFor: #x:!private!private helpers! !
!InterpolatedFunction categoriesFor: #y:!private!private helpers! !

!InterpolatedFunction class methodsFor!

for: valueArray
	"valueArray should contain an array of Points which specify the values of the tabulated function."

	^self for: valueArray withEndpointDerivatives: #(nil nil)!

for: valueArray withEndpointDerivatives: derivativeArray
	"valueArray should contain an array of Points which specify the values of the tabulated function.
	derivativeArray should contain a 2-element array containing the first derivatives of the tabulated function at its
	endpoints.  Either of the elements of derivativeArray may be nil, indicating that a 'natural' spline be
	calculated at that endpoint."

	self assert: [ derivativeArray size = 2 ].

	^self new
		values: valueArray;
		derivative1: (derivativeArray at: 1);
		derivativeN: (derivativeArray at: 2);
		yourself! !
!InterpolatedFunction class categoriesFor: #for:!instance creation!public! !
!InterpolatedFunction class categoriesFor: #for:withEndpointDerivatives:!instance creation!public! !

"Binary Globals"!

"Resources"!

