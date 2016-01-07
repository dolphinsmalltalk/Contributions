| package |
package := Package name: 'ANSI Substrings'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicScriptAt: #postuninstall put: '"Replace the original implementation of String>>subStrings: and associated methods"

String compile:
	''subStrings: separator
	"Answer an array containing the substrings of the receiver separated by occurrences
	of the <Character> or <readableString> argument, separator.
	Repeated separators produce empty strings in the array (cf. #subStrings).
	The separators are removed."

	#todo "This does not comply with the current ANSI definition, which requires an Array of separators,
		each of which may individually be used as a separator".
	^separator _separateSubStringsIn: self''
	categories: (Array with: (MethodCategory name: ''copying'')).

String compile:
	''lines
	"Answer a SequenceableCollection containing the lines of the receiver (sequences of Characters
	separated by line delimiters. Blank lines are included.
	N.B. It is assumed that a line delimiter consists of two characters."

	^self subStrings: String lineDelimiter''
	categories: (Array with: (MethodCategory name: ''copying'')).
'.
package basicScriptAt: #preuninstall put: '"Replace the original implementation of String>>subStrings: and associated methods"

String compile:
	''subStrings: separator
	"Answer an array containing the substrings of the receiver separated by occurrences
	of the <Character> or <readableString> argument, separator.
	Repeated separators produce empty strings in the array (cf. #subStrings).
	The separators are removed."

	#todo "This does not comply with the current ANSI definition, which requires an Array of separators,
		each of which may individually be used as a separator".
	^separator _separateSubStringsIn: self''
	categories: (Array with: (MethodCategory name: ''copying'')).

String compile:
	''lines
	"Answer a SequenceableCollection containing the lines of the receiver (sequences of Characters
	separated by line delimiters. Blank lines are included.
	N.B. It is assumed that a line delimiter consists of two characters."

	^self subStrings: String lineDelimiter''
	categories: (Array with: (MethodCategory name: ''copying'')).
'.

package classNames
	add: #SubstringExtractor;
	yourself.

package methodNames
	add: #Character -> #separateSubStringsIn:;
	add: #SequenceableCollection -> #separateSubStringsIn:;
	add: #String -> #lines;
	add: #String -> #subStrings:;
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

Object subclass: #SubstringExtractor
	instanceVariableNames: 'ignoreLeadingSeparators ignoreTrailingSeparators compressConsecutiveSeparators trailingSeparatorsToRemove'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Character methodsFor!

separateSubStringsIn: tokens
	^(SubstringExtractor new
		ignoreTrailingSeparators: true;
		trailingSeparatorsToRemove: 1) extractSubstringsFrom: tokens separators: (Array with: self)! !
!Character categoriesFor: #separateSubStringsIn:!double dispatch!private! !

!SequenceableCollection methodsFor!

separateSubStringsIn: aReadableString
	^SubstringExtractor new extractSubstringsFrom: aReadableString separators: self! !
!SequenceableCollection categoriesFor: #separateSubStringsIn:!double dispatch!private! !

!String methodsFor!

lines
	"Answer a SequenceableCollection containing the lines of the receiver (sequences of Characters
	separated by line delimiters. Blank lines are included.
	N.B. It is assumed that a line delimiter consists of two characters."

	^String lineDelimiter _separateSubStringsIn: self!

subStrings: separator
	"Answer an array containing the substrings of the receiver separated by occurrences
	of the contents of the argument, separator. Separator can be either a Character (non-ANSI behavior) or
	a SequenceableCollection.  Adjacent separators produce empty strings in the result collection.  The
	separators are removed."

	^separator separateSubStringsIn: self! !
!String categoriesFor: #lines!accessing!copying!public! !
!String categoriesFor: #subStrings:!copying!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

SubstringExtractor guid: (GUID fromString: '{5E2F38C1-C01A-4FA8-8B15-F5EA75AB1125}')!
SubstringExtractor comment: 'I know how to break strings into sub-strings based on some simple rules.'!
!SubstringExtractor categoriesForClass!Unclassified! !
!SubstringExtractor methodsFor!

compressConsecutiveSeparators
	^compressConsecutiveSeparators!

compressConsecutiveSeparators: aBoolean
	compressConsecutiveSeparators := aBoolean!

compressConsecutiveSeparatorsIn: col
	"| stream |

	stream := (Array new: col size + 2) writeStream.
	(col at: 1) size = 0 ifTrue: [ stream nextPut: '' ].
	col do: [ :aString | aString size > 0 ifTrue: [ stream nextPut: aString ] ].
	(col at: col size) size = 0 ifTrue: [ stream nextPut: '' ].
	^stream contents"

	| newCol |

	newCol := OrderedCollection new: col size.
	(col at: 1) size = 0 ifTrue: [ newCol add: '' ].
	col do: [ :aString | aString size > 0 ifTrue: [ newCol add: aString ] ].
	(col at: col size) size = 0 ifTrue: [ newCol add: '' ].
	^newCol asArray!

extractSubstringsFrom: aReadableString separators: aSequenceableCollection
	| separators start end stream runLength aString |

	stream := (Array new: 10) writeStream.
	separators := String withAll: aSequenceableCollection.
	start := 1.
	runLength := 1.

	"Reduce the string to be processed appropriately if any processing options have been selected."

	aString := aReadableString copy.

	end := aString indexOfAnyOf: separators startingAt: start.
	[ end ~= 0 ] whileTrue:
		[ ((end > (start + 1)) or: [ (separators includes: (aString at: start)) = false ])
			ifTrue: [ stream nextPut: (aString midString: end - start from: start).
				   runLength := 1. ]
			ifFalse: [ ((runLength := runLength + 1) > 1) | (start = 1) ifTrue: [ stream nextPut: '' ] ].
		start := end + 1.
		end := aString indexOfAnyOf: separators startingAt: start ].

	start <= aString size ifTrue:
		[ stream nextPut: (aString rightString: aString size - start + 1) ].

	aString notEmpty ifTrue:
		[ (separators includes: (aString last)) ifTrue: [ stream nextPut: '' ] ].

	^self postApplyOptions: stream contents!

ignoreLeadingSeparators
	^ignoreLeadingSeparators!

ignoreLeadingSeparators: aBoolean
	ignoreLeadingSeparators := aBoolean!

ignoreTrailingSeparators
	^ignoreTrailingSeparators!

ignoreTrailingSeparators: aBoolean
	ignoreTrailingSeparators := aBoolean!

initialize
	super initialize.
	self
		ignoreLeadingSeparators: false;
		ignoreTrailingSeparators: false;
		compressConsecutiveSeparators: false;
		trailingSeparatorsToRemove: nil!

postApplyOptions: aCollection
	| col stream |

	col := aCollection.

	self ignoreLeadingSeparators ifTrue:
		[ col := (self removeTrailingEmptyElementsFrom: col reverse maxSeparatorsToRemove: nil) reverse ].

	self ignoreTrailingSeparators ifTrue:
		[ col := self removeTrailingEmptyElementsFrom: col
					maxSeparatorsToRemove: self trailingSeparatorsToRemove ].

	"Private - The following should leave leading and trailing empty elements alone, but remove internal 
	empty elements."

	self compressConsecutiveSeparators ifTrue:
		[ col := self compressConsecutiveSeparatorsIn: col ].

	^col!

removeTrailingEmptyElementsFrom: aCollection maxSeparatorsToRemove: count
	| removedCount index |

	removedCount := 0.
	index := aCollection size.

	[ index > 0 and: [ (aCollection at: index) size = 0 and: [ count isNil or: [ removedCount < count ] ] ] ]
		whileTrue: [ removedCount := removedCount + 1.
				index := index - 1 ].
	^aCollection copyFrom: 1 to: (aCollection size - removedCount)!

trailingSeparatorsToRemove
	^trailingSeparatorsToRemove!

trailingSeparatorsToRemove: anIntegerOrNil
	trailingSeparatorsToRemove := anIntegerOrNil! !
!SubstringExtractor categoriesFor: #compressConsecutiveSeparators!accessing!public! !
!SubstringExtractor categoriesFor: #compressConsecutiveSeparators:!accessing!public! !
!SubstringExtractor categoriesFor: #compressConsecutiveSeparatorsIn:!private! !
!SubstringExtractor categoriesFor: #extractSubstringsFrom:separators:!operations!public! !
!SubstringExtractor categoriesFor: #ignoreLeadingSeparators!accessing!public! !
!SubstringExtractor categoriesFor: #ignoreLeadingSeparators:!accessing!public! !
!SubstringExtractor categoriesFor: #ignoreTrailingSeparators!accessing!public! !
!SubstringExtractor categoriesFor: #ignoreTrailingSeparators:!accessing!public! !
!SubstringExtractor categoriesFor: #initialize!initialize/release!public! !
!SubstringExtractor categoriesFor: #postApplyOptions:!helpers!private! !
!SubstringExtractor categoriesFor: #removeTrailingEmptyElementsFrom:maxSeparatorsToRemove:!helpers!private! !
!SubstringExtractor categoriesFor: #trailingSeparatorsToRemove!accessing!public! !
!SubstringExtractor categoriesFor: #trailingSeparatorsToRemove:!accessing!public! !

!SubstringExtractor class methodsFor!

new
	^super new initialize! !
!SubstringExtractor class categoriesFor: #new!public! !

"Binary Globals"!

"Resources"!

