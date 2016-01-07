| package |
package := Package name: 'US Collection Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Collection Extensions 0.015$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.015'.


package methodNames
	add: #Character -> #isCharacter;
	add: #Collection -> #any;
	add: #Collection -> #ifEmpty:;
	add: #Collection -> #ifEmpty:ifNotEmpty:;
	add: #Collection -> #ifNotEmpty:;
	add: #Collection -> #ifNotEmpty:ifEmpty:;
	add: #Collection -> #ifNotEmptyDo:;
	add: #Collection -> #isEmptyOrNil;
	add: #Collection -> #isNilOrEmpty;
	add: #Collection -> #notNilOrEmpty;
	add: #Dictionary -> #keysAndValuesInverted;
	add: #Dictionary -> #keysAt:prefix:;
	add: #Random -> #choice:;
	add: #SequenceableCollection -> #keysAndValuesInverted;
	add: #SequenceableCollection -> #pairsDo:;
	add: #String -> #findDelimiters:startingAt:;
	add: #String -> #findTokens:;
	add: #String -> #includesSubString:;
	add: #String -> #isCharacter;
	add: #String -> #matchesRegex:;
	add: #String -> #skipDelimiters:startingAt:;
	add: #UndefinedObject -> #isEmptyOrNil;
	add: #UndefinedObject -> #isNilOrEmpty;
	add: #UndefinedObject -> #notNilOrEmpty;
	add: 'SequenceableCollection class' -> #streamContents:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\ActiveX\Components\VBScript\VBScript Regular Expressions';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Character methodsFor!

isCharacter
	^false! !
!Character categoriesFor: #isCharacter!public! !

!Collection methodsFor!

any
^self detect: [:each | true]!

ifEmpty: aBlock 
	^self isEmpty ifTrue: [aBlock value]!

ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock 
	^self isEmpty ifTrue: [emptyBlock value] ifFalse: [notEmptyBlock value]!

ifNotEmpty: aBlock 
	^self isEmpty ifFalse: [aBlock value]!

ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock 
	^self ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock!

ifNotEmptyDo: operation 
	^self isEmpty 
		ifTrue: [nil]
		ifFalse: 
			[self do: operation.
			]!

isEmptyOrNil
	^self isEmpty!

isNilOrEmpty
	^self isEmpty!

notNilOrEmpty
	^self notEmpty! !
!Collection categoriesFor: #any!public! !
!Collection categoriesFor: #ifEmpty:!public! !
!Collection categoriesFor: #ifEmpty:ifNotEmpty:!public! !
!Collection categoriesFor: #ifNotEmpty:!public! !
!Collection categoriesFor: #ifNotEmpty:ifEmpty:!public! !
!Collection categoriesFor: #ifNotEmptyDo:!public! !
!Collection categoriesFor: #isEmptyOrNil!public!testing! !
!Collection categoriesFor: #isNilOrEmpty!public!testing! !
!Collection categoriesFor: #notNilOrEmpty!public!testing! !

!Dictionary methodsFor!

keysAndValuesInverted
	| dic |
	dic := Dictionary new: self size.
	self keysAndValuesDo: [:eachKey :eachValue | dic at: eachValue put: eachKey].
	^dic!

keysAt: aValue prefix: aString 
	| matchingKeys |
	matchingKeys := OrderedCollection new.
	self keysAndValuesDo: 
			[:eachKey :eachValue | 
			(eachValue = aValue and: [eachKey beginsWith: aString]) ifTrue: [matchingKeys add: eachKey]].
	^matchingKeys! !
!Dictionary categoriesFor: #keysAndValuesInverted!public! !
!Dictionary categoriesFor: #keysAt:prefix:!public! !

!Random methodsFor!

choice: aCollection
^aCollection at: (self next * aCollection size + 0.5) rounded! !
!Random categoriesFor: #choice:!public! !

!SequenceableCollection methodsFor!

keysAndValuesInverted
	| dic |
	dic := Dictionary new: self size.
	self keysAndValuesDo: [:eachKey :eachValue | dic at: eachValue  put: eachKey ].
	^dic!

pairsDo: aBlock 
	"Evaluate aBlock with my elements taken two at a time.  If there's an odd number of items, ignore the last one.  Allows use of a flattened array for things that naturally group into pairs.  See also pairsCollect:"

	1 to: self size // 2 do:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsDo:
	[:a :b | Transcript cr; show: b, ' is number ', a printString]
"! !
!SequenceableCollection categoriesFor: #keysAndValuesInverted!public! !
!SequenceableCollection categoriesFor: #pairsDo:!public! !

!SequenceableCollection class methodsFor!

streamContents: blockWithArg 
	| stream |
	stream := WriteStream on: (self new: 100).
	blockWithArg value: stream.
	^stream contents! !
!SequenceableCollection class categoriesFor: #streamContents:!public! !

!String methodsFor!

findDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | delim = (self at: i) ifTrue: [^ i]]].
	^ self size + 1!

findTokens: delimiters
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens := OrderedCollection new.
	separators := delimiters isCharacter 
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipDelimiters: separators startingAt: keyStop.
		keyStop := self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens!

includesSubString: aString
^(self findString: aString)> 0!

isCharacter
^false!

matchesRegex: aString 
	| regex |
	regex := IRegExp2 new.
	regex pattern: aString.
	regex global: true.
	^regex test: self!

skipDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.  Assumes the delimiters to be a non-empty string."

	start to: self size do: [:i |
		delimiters detect: [:delim | delim = (self at: i)]
				ifNone: [^ i]].
	^ self size + 1! !
!String categoriesFor: #findDelimiters:startingAt:!public! !
!String categoriesFor: #findTokens:!public! !
!String categoriesFor: #includesSubString:!public! !
!String categoriesFor: #isCharacter!public! !
!String categoriesFor: #matchesRegex:!public! !
!String categoriesFor: #skipDelimiters:startingAt:!public! !

!UndefinedObject methodsFor!

isEmptyOrNil
	^true!

isNilOrEmpty
	^true!

notNilOrEmpty
	^false! !
!UndefinedObject categoriesFor: #isEmptyOrNil!public!testing! !
!UndefinedObject categoriesFor: #isNilOrEmpty!public!testing! !
!UndefinedObject categoriesFor: #notNilOrEmpty!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

