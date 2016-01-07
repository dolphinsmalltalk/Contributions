| package |
package := Package name: 'SpokenNumberFormatter'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicScriptAt: #postinstall put: 'SpokenNumberFormatter defaultToAmerican'.

package classNames
	add: #SpokenNumberFormatter;
	yourself.

package methodNames
	add: #Fraction -> #asSpoken;
	add: #Number -> #asSpoken;
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

Object subclass: #SpokenNumberFormatter
	instanceVariableNames: 'groupNames'
	classVariableNames: 'DefaultGroupNames'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Fraction methodsFor!

asSpoken
	^self numerator asSpoken, ' over ', self denominator asSpoken! !
!Fraction categoriesFor: #asSpoken!converting!public! !

!Number methodsFor!

asSpoken
	"Answers a String containing the text for this number in American English."

	^SpokenNumberFormatter new textFor: self! !
!Number categoriesFor: #asSpoken!converting!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

SpokenNumberFormatter guid: (GUID fromString: '{DD7238CE-9F85-4FAA-9B4C-9ACABCC9D3A9}')!
SpokenNumberFormatter comment: ''!
!SpokenNumberFormatter categoriesForClass!Unclassified! !
!SpokenNumberFormatter methodsFor!

beAmerican
	self groupNames: self class americanGroupNames!

beDefault
	self groupNames: self class defaultGroupNames!

beEuropean
	self groupNames: self class europeanGroupNames!

groupNames
	^groupNames!

groupNames: anArray
	groupNames := anArray!

initialize
	super initialize.
	self beDefault!

isAmerican
	^self groupNames == self class americanGroupNames!

isEuropean
	^self groupNames == self class europeanGroupNames!

spokenDigit: aCharacter
	| digitText |

	digitText := ##(Array withAll: #('zero' 'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine')).
	^digitText at: aCharacter asString asNumber + 1!

spokenExponentDigit: aCharacter
	| digitText |

	digitText := ##(Array withAll: #('zeroth' 'first' 'second' 'third' 'fourth' 'fifth' 'sixth' 'seventh' 'eighth' 'ninth')).
	^digitText at: aCharacter asString asNumber + 1!

spokenExponentFor: aNumber
	"Answers a String containing the text for this number as an exponent in American English."

	| text result hundredsDigitFound tensDigitFound unitsDigitFound |

	hundredsDigitFound := tensDigitFound := unitsDigitFound := false.
	result := ''.

	text := aNumber printString.
	text last isDigit ifFalse: [ text := text leftString: text size - 1 ].

	(text at: 1) = $- ifTrue: [ result := 'negative '.  text := text rightString: text size - 1 ].

	text size = 3 ifTrue: [
		(text at: 1) ~= $0 ifTrue: [
			result := result, (self spokenDigit: (text at: 1)), ' hundred '.
			hundredsDigitFound := true ].
		text := text rightString: 2 ].

	text size = 2 ifTrue: [
		(text at: 1) ~= $0	"tens digit is not zero"
			ifTrue: [
				(text at: 1) ~= $1	"tens digit is not one"
					ifTrue: [
						(text at: 2) ~= $0	"units digit is not zero"
							ifTrue: [ result := result, (self spokenTensDigit: (text at: 1)), '-'.
								    text := text rightString: 1 ]
							ifFalse: [ result := result, (self spokenTensExponentDigit: (text at: 1)), ' '.
									text := text rightString: 1 ] ]
					ifFalse: [ result := result, (self spokenTeensExponentDigit: (text at: 2)).
						     text := '' ].
				tensDigitFound := true ]
			ifFalse: [ 
				(text at: 2) ~= $0 ifTrue: [ result := result, 'and ' ].
				text := text rightString: 1 ] ].

	text size = 1 ifTrue: [
		(text at: 1) ~= $0 ifTrue: [
			result := result, (self spokenExponentDigit: (text at: 1)).
			unitsDigitFound := true ] ].

	hundredsDigitFound & (tensDigitFound not) & (unitsDigitFound not) ifTrue: [ result := result trimBlanks, 'th' ].

	^result trimBlanks!

spokenTeensDigit: aCharacter
	| digitText |

	digitText := ##(Array withAll: #('ten' 'eleven' 'twelve' 'thirteen' 'fourteen' 'fifteen'
							'sixteen' 'seventeen' 'eighteen' 'nineteen')).
	^digitText at: aCharacter asString asNumber + 1!

spokenTeensExponentDigit: aCharacter
	| digitText |

	digitText := ##(Array withAll: #('tenth' 'eleventh' 'twelfth' 'thirteenth' 'fourteenth'
							'fifteenth' 'sixteenth' 'seventeenth' 'eighteenth' 'nineteenth')).
	^digitText at: aCharacter asString asNumber + 1!

spokenTensDigit: aCharacter
	| digitText |

	digitText := ##(Array withAll: #('' '' 'twenty' 'thirty' 'forty' 'fifty' 'sixty' 'seventy' 'eighty' 'ninety')).
	^digitText at: aCharacter asString asNumber + 1!

spokenTensExponentDigit: aCharacter
	| digitText |

	digitText := ##(Array withAll: #('' '' 'twentieth' 'thirtieth' 'fortieth' 'fiftieth' 'sixtieth' 'seventieth' 'eightieth' 'ninetieth')).
	^digitText at: aCharacter asString asNumber + 1!

textFor: aNumber
	"Answers a String containing the text for this number in American English."

	| text subStrings groups leftOfDecimal rightOfDecimal exponent groupText
	  groupCount groupSize result |

	groupText := self groupNames.

	leftOfDecimal := ''.
	rightOfDecimal := ''.
	exponent := ''.
	result := String new.

	text := aNumber printString.
	text last isDigit ifFalse: [ text := text leftString: text size - 1 ].

	(text at: 1) = $+ ifTrue: [ result := 'positive '.  text := text rightString: text size - 1 ].
	(text at: 1) = $- ifTrue: [ result := 'negative '.  text := text rightString: text size - 1 ].

	subStrings := text subStrings: '.'.
	leftOfDecimal := subStrings at: 1.
	subStrings size > 1 ifTrue: [
		subStrings := (subStrings at: 2) subStrings: 'e'.
		rightOfDecimal := subStrings at: 1.
		subStrings size > 1
			ifTrue: [ exponent := subStrings at: 2 ] ].

	(aNumber > self class largestFormattableNumber) & (exponent size = 0)  ifTrue: [ ^'a really big number' ].

	groupCount := leftOfDecimal size // 3.
	leftOfDecimal size \\ 3 ~= 0 ifTrue: [ groupCount := groupCount + 1 ].

	groups := Array new: groupCount.
	1 to: groupCount do: [ :i |
		i = 1
			ifTrue: [
				leftOfDecimal size \\ 3 = 0
					ifTrue: [ groupSize := 3 ]
					ifFalse: [ groupSize := leftOfDecimal size \\ 3 ] ]
			ifFalse: [ groupSize := 3 ].
		groups at: i put: (leftOfDecimal leftString: groupSize).
		leftOfDecimal := leftOfDecimal rightString: leftOfDecimal size - groupSize ].

	groups := groups reverse.

	groups size = 1
		ifTrue:
			[ text := (groups at: 1).
			text size = 3 ifTrue: [
				(text at: 1) ~= $0 ifTrue: [
					result := result, (self spokenDigit: (text at: 1)), ' hundred ' ].
				text := text rightString: 2 ].
			text size = 2 ifTrue: [
				(text at: 1) ~= $0
					ifTrue: [
						(text at: 1) ~= $1
							ifTrue: [ result := result, (self spokenTensDigit: (text at: 1)).
								    text := text rightString: 1.
								    text ~= '0' ifTrue: [ result := result, '-'] ]
							ifFalse: [aNumber >= 100 ifTrue: [ result := result trimBlanks, ' and ' ].
								     result := result, (self spokenTeensDigit: (text at: 2)).
								     text := '' ] ]
					ifFalse: [ 
						(text at: 2) ~= $0 ifTrue: [ result := result, 'and ' ].
						text := text rightString: 1 ] ].
			text size = 1 ifTrue: [
				(aNumber abs < 1) | ((text at: 1) ~= $0) ifTrue: [
					result := result, (self spokenDigit: (text at: 1)) ] ] ]
		ifFalse: [
			groups size to: 1 by: -1 do: [ :i |
				(i = 1) & ((groups at: i) asNumber < 100) & ((groups at: i) asNumber > 0) & (aNumber > 1000)
					ifTrue: [
						result := result trimBlanks.
						result last = $, ifTrue: [
							result := result leftString: (result size - 1) ].
						result := result, ' and ' ].
				(groups at: i) asNumber ~= 0 ifTrue: [
					result := result, (groups at: i) asNumber asSpoken, ' '.
					result := result, (groupText at: i) ].
				(i > 1) & ((groups at: i) asNumber ~= 0) ifTrue: [ result := result,  ', ' ] ] ].

	rightOfDecimal size > 0 ifTrue: [
		result last ~= $  ifTrue: [ result := result, ' ' ].
		result := result, 'point '.
		1 to: rightOfDecimal size do: [ :i |
			result := result, (rightOfDecimal at: i) asString asNumber asSpoken, ' ' ] ].

	result := result trimBlanks.
	result size > 0 ifTrue: [
		result last = $, ifTrue: [ result := result leftString: (result size - 1) ] ].

	exponent size > 0 ifTrue: [
		result := result, ' times ten to the ', (self spokenExponentFor: ((exponent rightString: exponent size - 1) asNumber)) ].

	^result trimBlanks
! !
!SpokenNumberFormatter categoriesFor: #beAmerican!public! !
!SpokenNumberFormatter categoriesFor: #beDefault!public! !
!SpokenNumberFormatter categoriesFor: #beEuropean!public! !
!SpokenNumberFormatter categoriesFor: #groupNames!public! !
!SpokenNumberFormatter categoriesFor: #groupNames:!public! !
!SpokenNumberFormatter categoriesFor: #initialize!public! !
!SpokenNumberFormatter categoriesFor: #isAmerican!public! !
!SpokenNumberFormatter categoriesFor: #isEuropean!public! !
!SpokenNumberFormatter categoriesFor: #spokenDigit:!private! !
!SpokenNumberFormatter categoriesFor: #spokenExponentDigit:!private! !
!SpokenNumberFormatter categoriesFor: #spokenExponentFor:!private! !
!SpokenNumberFormatter categoriesFor: #spokenTeensDigit:!private! !
!SpokenNumberFormatter categoriesFor: #spokenTeensExponentDigit:!private! !
!SpokenNumberFormatter categoriesFor: #spokenTensDigit:!private! !
!SpokenNumberFormatter categoriesFor: #spokenTensExponentDigit:!private! !
!SpokenNumberFormatter categoriesFor: #textFor:!public! !

!SpokenNumberFormatter class methodsFor!

americanGroupNames
	^##(Array withAll: #('' 'thousand' 'million' 'billion' 'trillion' 'quadrillion'
					'quintillion' 'sextillion' 'septillion' 'octillion'
					'nonillion' 'decillion' 'undecillion' 'duodecillion'
					'tredecillion' 'quattrodecillion' 'quindecillion'
					'sexdecillion' 'septendecillion' 'octodecillion'
					'novemdecillion' 'vigintillion'))!

defaultGroupNames
	^DefaultGroupNames!

defaultToAmerican
	DefaultGroupNames := self americanGroupNames!

defaultToEuropean
	DefaultGroupNames := self europeanGroupNames!

europeanGroupNames
	^##(Array withAll: #('' 'thousand' 'million' 'milliard' 'billion'
					'billiard' 'trillion' 'trilliard' 'quadrillion'
					'quadrilliard' 'quintillion' 'quintilliard'
					'sextillion' 'sextilliard' 'septillion' 'septilliard'
					'octillion' 'octilliard' 'nonillion' 'nonilliard'
					'decillion' 'decilliard'))!

largestFormattableNumber
	^##((10 raisedTo: 66) - 1)!

new
	^super new initialize! !
!SpokenNumberFormatter class categoriesFor: #americanGroupNames!accessing!private! !
!SpokenNumberFormatter class categoriesFor: #defaultGroupNames!accessing!public! !
!SpokenNumberFormatter class categoriesFor: #defaultToAmerican!operations!public! !
!SpokenNumberFormatter class categoriesFor: #defaultToEuropean!operations!public! !
!SpokenNumberFormatter class categoriesFor: #europeanGroupNames!accessing!private! !
!SpokenNumberFormatter class categoriesFor: #largestFormattableNumber!accessing!public! !
!SpokenNumberFormatter class categoriesFor: #new!instance creation!public! !

"Binary Globals"!

"Resources"!

