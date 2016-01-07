| package |
package := Package name: 'US JSON'.
package paxVersion: 1;
	basicComment: '$id: US JSON 0.018$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.018'.


package classNames
	add: #JsonError;
	add: #JsonFiler;
	add: #JsonInFiler;
	add: #JsonOutFiler;
	yourself.

package methodNames
	add: #Boolean -> #jsonSaveOn:;
	add: #Color -> #jsonSaveOn:;
	add: #Dictionary -> #jsonSaveOn:;
	add: #Float -> #jsonSaveOn:;
	add: #Fraction -> #jsonSaveOn:;
	add: #Number -> #jsonSaveOn:;
	add: #Object -> #jsonSaveOn:;
	add: #Object -> #jsonStoreOn:;
	add: #Object -> #jsonString;
	add: #Point -> #jsonSaveOn:;
	add: #SequenceableCollection -> #jsonSaveOn:;
	add: #SequencedStream -> #peek:;
	add: #String -> #jsonSaveOn:;
	add: #UndefinedObject -> #jsonSaveOn:;
	add: 'Character class' -> #ff;
	add: 'Object class' -> #fromJsonString:;
	add: 'Object class' -> #jsonReadFrom:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Unicode';
	yourself).

package setManualPrerequisites: #(
	'US Unicode').

package!

"Class Definitions"!

Object subclass: #JsonFiler
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #JsonError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JsonFiler subclass: #JsonInFiler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JsonFiler subclass: #JsonOutFiler
	instanceVariableNames: ''
	classVariableNames: 'CharacterEscapeMap'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Boolean methodsFor!

jsonSaveOn: aJsonOutFiler
aJsonOutFiler saveBoolean: self! !
!Boolean categoriesFor: #jsonSaveOn:!public! !

!Character class methodsFor!

ff
	"Answer the formfeed <Character>."

	^##(self value: 12)! !
!Character class categoriesFor: #ff!instance creation!public! !

!Color methodsFor!

jsonSaveOn: aJsonOutFiler 
	aJsonOutFiler saveColor: self! !
!Color categoriesFor: #jsonSaveOn:!public! !

!Dictionary methodsFor!

jsonSaveOn: aJsonOutFiler 
	aJsonOutFiler saveDictionary: self! !
!Dictionary categoriesFor: #jsonSaveOn:!public! !

!Float methodsFor!

jsonSaveOn: aJsonOutFiler 
	aJsonOutFiler saveFloat: self! !
!Float categoriesFor: #jsonSaveOn:!public! !

!Fraction methodsFor!

jsonSaveOn: aJsonOutFiler 
	aJsonOutFiler saveNumber: self asFloat! !
!Fraction categoriesFor: #jsonSaveOn:!public! !

!Number methodsFor!

jsonSaveOn: aJsonOutFiler
aJsonOutFiler saveNumber: self! !
!Number categoriesFor: #jsonSaveOn:!public! !

!Object methodsFor!

jsonSaveOn: aJsonOutFiler
Error notYetImplemented!

jsonStoreOn: aStream 


	(JsonOutFiler on: aStream) nextPut: self!

jsonString
	| stream |
	stream := UnicodeString writeStream: 128.
	self jsonStoreOn: stream.
	^stream contents! !
!Object categoriesFor: #jsonSaveOn:!public! !
!Object categoriesFor: #jsonStoreOn:!binary filing!public! !
!Object categoriesFor: #jsonString!binary filing!public! !

!Object class methodsFor!

fromJsonString: aString 
	| stream |
	stream := aString readStream.
	^self jsonReadFrom: stream!

jsonReadFrom: aStream 

	^(JsonInFiler on: aStream)
		
		next! !
!Object class categoriesFor: #fromJsonString:!binary filing!public! !
!Object class categoriesFor: #jsonReadFrom:!binary filing!public! !

!Point methodsFor!

jsonSaveOn: aJsonOutFiler 
	aJsonOutFiler savePoint: self! !
!Point categoriesFor: #jsonSaveOn:!public! !

!SequenceableCollection methodsFor!

jsonSaveOn: aJsonOutFiler
aJsonOutFiler saveArray: self! !
!SequenceableCollection categoriesFor: #jsonSaveOn:!public! !

!SequencedStream methodsFor!

peek: size 
	^(self atEnd or: [self position + size > self size])
		ifFalse: 
			[| anObject |
			anObject := self next: size.
			self position: self position - size.
			anObject]! !
!SequencedStream categoriesFor: #peek:!accessing!public! !

!String methodsFor!

jsonSaveOn: aJsonOutFiler
aJsonOutFiler saveString: self! !
!String categoriesFor: #jsonSaveOn:!public! !

!UndefinedObject methodsFor!

jsonSaveOn: aJsonOutFiler
aJsonOutFiler saveNil! !
!UndefinedObject categoriesFor: #jsonSaveOn:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

JsonFiler guid: (GUID fromString: '{0A6ABB10-D5FE-46A6-90E4-547D6D3506F4}')!
JsonFiler comment: ''!
!JsonFiler categoriesForClass!Unclassified! !
!JsonFiler methodsFor!

close
stream close!

stream
	^stream!

stream: anObject
	stream := anObject! !
!JsonFiler categoriesFor: #close!public! !
!JsonFiler categoriesFor: #stream!accessing!public! !
!JsonFiler categoriesFor: #stream:!accessing!private! !

!JsonFiler class methodsFor!

icon
^Icon fromFile: 'Udo Schneider\Goodies\Resources\json.ico'!

on: aStream
^self new stream: aStream; yourself! !
!JsonFiler class categoriesFor: #icon!public! !
!JsonFiler class categoriesFor: #on:!instance/creation!public! !

JsonError guid: (GUID fromString: '{2A5CA8E6-BBC1-4E7A-BAF3-B8A9591506CB}')!
JsonError comment: ''!
!JsonError categoriesForClass!Unclassified! !
!JsonError methodsFor!

_descriptionArguments
	^Array with: messageText with: tag displayString!

_descriptionFormat
	"Answer the Win32 format String to be used to format the description for the receiver."

	^'JSON Error: %1'! !
!JsonError categoriesFor: #_descriptionArguments!*-in class package!displaying!public! !
!JsonError categoriesFor: #_descriptionFormat!*-in class package!displaying!public! !

!JsonError class methodsFor!

icon
^Icon fromFile: 'Udo Schneider\Goodies\Resources\json.ico'! !
!JsonError class categoriesFor: #icon!public! !

JsonInFiler guid: (GUID fromString: '{7D5A0C0F-5B59-4370-B39C-6F27B19F3F2A}')!
JsonInFiler comment: ''!
!JsonInFiler categoriesForClass!Unclassified! !
!JsonInFiler methodsFor!

next
	| char |
	self skipCommentsAndWhitespace.
	char := stream peek.
	char = $" ifTrue: [^self parseString].
	('-0123456789' includes: char) ifTrue: [^self parseNumber].
	char = $[ ifTrue: [^self parseArray].
	char = $t ifTrue: [^self parseTrue].
	char = $f ifTrue: [^self parseFalse].
	char = $n ifTrue: [^self parseNull].
	char = ${ ifTrue: [^self parseObject].
	self parserError: 'Unknown character sequence found'!

parseArray
	| array |
	array := OrderedCollection new.
	stream next.	"Consume initial ["
	self skipCommentsAndWhitespace.
	[stream atEnd or: [stream peek = $]]] whileFalse: 
			[array add: self next.
			stream peek = $, ifTrue: [stream next].
			self skipCommentsAndWhitespace].
	stream next.
	^array asArray!

parseEscapeCharacter
	| char |
	char := stream next.
	char = $" ifTrue: [^$"].
	char = $\ ifTrue: [^$\].
	char = $/ ifTrue: [^$/].
	char = $b ifTrue: [^Character backspace].
	char = $f ifTrue: [^Character ff].
	char = $n ifTrue: [^Character nl].
	char = $r ifTrue: [^Character cr].
	char = $t ifTrue: [^Character tab].
	char = $u 
		ifTrue: 
			[| hexValue |
			hexValue := Number fromString: '16r' , (stream next: 4) asUppercase.
			^Character value: hexValue.
			].
	Error notYetImplemented!

parseFalse
	stream next: 5.
	^false!

parseNull
stream next: 4.
^nil!

parseNumber
	^Number readSmalltalkSyntaxFrom: stream!

parseObject
	| object |
	object := Dictionary new.
	stream next.	"Consume initial {"
	self skipCommentsAndWhitespace.
	[stream atEnd or: [stream peek = $}]] whileFalse: 
			[| key value |
			key := self next.
			self skipCommentsAndWhitespace.
			stream next = $: ifFalse: [self parserError: 'Colon $: expected but not found'].
			value := self next.
			object at: key put: value.
			self skipCommentsAndWhitespace.
			stream peek = $, ifTrue: [stream next].
			self skipCommentsAndWhitespace].
	self skipCommentsAndWhitespace.
	stream next.
	^object!

parserError: aString 
	JsonError signal: 'Parser Error: ' , aString!

parseString
	| stringStream char |
	stringStream := ReadWriteStream on: String new.
	stream next.	"Consume start double quote"
	[stream atEnd or: [(char := stream next) = $"]] whileFalse: 
			[char = $\ 
				ifFalse: [stringStream nextPut: char]
				ifTrue: [stringStream nextPut: self parseEscapeCharacter]].
	^stringStream contents!

parseTrue
	stream next: 4.
	^true!

skipCommentsAndWhitespace
	| abort |
	abort := false.
	[abort or: [stream atEnd]] whileFalse: 
			[stream peek isWhitespace 
				ifTrue: [stream next]
				ifFalse: [(stream peek: 2) = '/*' ifTrue: [
					stream upToAll: '*/'
					] ifFalse: [abort := true]]]! !
!JsonInFiler categoriesFor: #next!public! !
!JsonInFiler categoriesFor: #parseArray!public! !
!JsonInFiler categoriesFor: #parseEscapeCharacter!public! !
!JsonInFiler categoriesFor: #parseFalse!public! !
!JsonInFiler categoriesFor: #parseNull!public! !
!JsonInFiler categoriesFor: #parseNumber!public! !
!JsonInFiler categoriesFor: #parseObject!public! !
!JsonInFiler categoriesFor: #parserError:!public! !
!JsonInFiler categoriesFor: #parseString!public! !
!JsonInFiler categoriesFor: #parseTrue!public! !
!JsonInFiler categoriesFor: #skipCommentsAndWhitespace!public! !

JsonOutFiler guid: (GUID fromString: '{F467BEA0-3FA5-4D82-BEDF-855BFC4AF696}')!
JsonOutFiler comment: ''!
!JsonOutFiler categoriesForClass!Unclassified! !
!JsonOutFiler methodsFor!

jsonSaveOn: aJsonOutFiler
aJsonOutFiler saveNil!

nextPut: anObject
anObject jsonSaveOn: self!

saveArray: anArray 
	stream nextPut: $[.
	anArray do: [:each | each jsonSaveOn: self]
		separatedBy: 
			[stream
				nextPut: $,].
	stream nextPut: $]!

saveBoolean: aBoolean
stream nextPutAll: (aBoolean  ifTrue: ['true'] ifFalse: ['false'] )!

saveColor: aColor 
	| colorString |
	colorString := '#' , ('0' , (aColor red printStringRadix: 16 showRadix: false) last: 2) 
				, ('0' , (aColor green printStringRadix: 16 showRadix: false) last: 2) 
					, ('0' , (aColor blue printStringRadix: 16 showRadix: false) last: 2).
					self saveString: colorString!

saveDictionary: aDictionary 
	stream nextPut: ${.
	aDictionary associationsDo: 
			[:association | 
			association key jsonSaveOn: self.
			stream nextPut: $:.
			association value jsonSaveOn: self]
		separatedBy: 
			[stream
				nextPut: $,].
	stream nextPut: $}!

saveFloat: aFloat 
	aFloat printOn: stream decimalPlaces: 2!

saveNil
stream nextPutAll: 'null'!

saveNumber: aNumber 
	stream nextPutAll: aNumber  displayString!

savePoint: aPoint


	self saveDictionary: ( (Dictionary new
		at: #x
			put: aPoint x;
		at: #y
			put: aPoint y;
		yourself) )!

saveString: aString 
	stream nextPut: $".
	aString  do: 
			[:each | 
			(CharacterEscapeMap at: each ifAbsent: [nil]) 
				ifNotNil: [:value | stream nextPutAll: value]
				ifNil: [stream nextPut: each]].
	stream nextPut: $"! !
!JsonOutFiler categoriesFor: #jsonSaveOn:!public! !
!JsonOutFiler categoriesFor: #nextPut:!public! !
!JsonOutFiler categoriesFor: #saveArray:!private! !
!JsonOutFiler categoriesFor: #saveBoolean:!private! !
!JsonOutFiler categoriesFor: #saveColor:!private! !
!JsonOutFiler categoriesFor: #saveDictionary:!private! !
!JsonOutFiler categoriesFor: #saveFloat:!private! !
!JsonOutFiler categoriesFor: #saveNil!private! !
!JsonOutFiler categoriesFor: #saveNumber:!private! !
!JsonOutFiler categoriesFor: #savePoint:!private! !
!JsonOutFiler categoriesFor: #saveString:!private! !

!JsonOutFiler class methodsFor!

initialize
	"
	self initialize
	"

	CharacterEscapeMap := Dictionary new.
	CharacterEscapeMap
		at: $" put: '\"';
		at: $\ put: '\\';
		at: Character backspace put: '\b';
		at: Character ff put: '\f';
		at: Character nl put: '\n';
		at: Character cr put: '\r';
		at: Character tab put: '\t'.
	"at: $/ put: '\/';"
	^super initialize! !
!JsonOutFiler class categoriesFor: #initialize!public! !

"Binary Globals"!

