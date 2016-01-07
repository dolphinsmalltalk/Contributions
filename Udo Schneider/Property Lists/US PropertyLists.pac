| package |
package := Package name: 'US PropertyLists'.
package paxVersion: 1;
	basicComment: '$id: US PropertyLists 1.005$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Read Apple Mac OS X XML and Binary Property Lists

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.005'.


package classNames
	add: #CFBinaryPlistHeader;
	add: #CFBinaryPlistTrailer;
	add: #NSBinaryPropertyListSerialization;
	add: #NSPropertyListSerialization;
	add: #NSXMLPropertyListSerialization;
	yourself.

package methodNames
	add: #ByteArray -> #arraySized:atOffset:length:;
	add: #SequenceableCollection -> #copyFromOffset:;
	add: #SequenceableCollection -> #copyFromOffset:length:;
	add: #Set -> #=;
	add: 'TimeStamp class' -> #coreFoundationReferenceTimeStamp;
	add: 'TimeStamp class' -> #fromCoreFoundationDouble:;
	yourself.

package globalNames
	add: #PropertyListConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\System\Base64\Dolphin Base64';
	add: '..\Big Endian Conversion\US BigEndian';
	add: '..\RFC3339\US RFC3339';
	add: '..\..\..\Object Arts\Dolphin\ActiveX\Components\XML DOM\XML DOM';
	yourself).

package setManualPrerequisites: #(
	'US RFC3339').

package!

"Class Definitions"!

Object subclass: #NSPropertyListSerialization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'PropertyListConstants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #CFBinaryPlistHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #CFBinaryPlistTrailer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NSPropertyListSerialization subclass: #NSBinaryPropertyListSerialization
	instanceVariableNames: 'data header trailer offsetTable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NSPropertyListSerialization subclass: #NSXMLPropertyListSerialization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ByteArray methodsFor!

arraySized: aByteSize atOffset: anOffset length: length	
|array|
aByteSize  = 1 
		ifTrue: [array := self copyFromOffset: anOffset  length: length]
		ifFalse: 
			[aByteSize  = 2 ifTrue: [array := WORDArray new: length].
			aByteSize  = 4 ifTrue: [array := DWORDArray new: length].
			array copyBytes: (self copyFromOffset: anOffset )].
^array
! !
!ByteArray categoriesFor: #arraySized:atOffset:length:!public! !

!SequenceableCollection methodsFor!

copyFromOffset: start 


	^self copyFrom: start+1 !

copyFromOffset: start length: length 
	^self copyFrom: start + 1 to: start + length! !
!SequenceableCollection categoriesFor: #copyFromOffset:!public! !
!SequenceableCollection categoriesFor: #copyFromOffset:length:!public! !

!Set methodsFor!

= aSet
^self equals: aSet! !
!Set categoriesFor: #=!comparing!public! !

!TimeStamp class methodsFor!

coreFoundationReferenceTimeStamp
^self date: (Date newDay: 1 monthIndex: 1 year: 2001) time: (Time fromUtcHours: 0 minutes: 0 seconds: 0 milliseconds: 0 ).!

fromCoreFoundationDouble: aDouble 
	^TimeStamp fromSeconds: aDouble + self coreFoundationReferenceTimeStamp asSeconds! !
!TimeStamp class categoriesFor: #coreFoundationReferenceTimeStamp!public! !
!TimeStamp class categoriesFor: #fromCoreFoundationDouble:!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #PropertyListConstants put: (PoolConstantsDictionary named: #PropertyListConstants)!
PropertyListConstants at: 'kCFBinaryPlistMarkerArray' put: 16rA0!
PropertyListConstants at: 'kCFBinaryPlistMarkerASCIIString' put: 16r50!
PropertyListConstants at: 'kCFBinaryPlistMarkerData' put: 16r40!
PropertyListConstants at: 'kCFBinaryPlistMarkerDate' put: 16r33!
PropertyListConstants at: 'kCFBinaryPlistMarkerDict' put: 16rD0!
PropertyListConstants at: 'kCFBinaryPlistMarkerFalse' put: 16r8!
PropertyListConstants at: 'kCFBinaryPlistMarkerFill' put: 16rF!
PropertyListConstants at: 'kCFBinaryPlistMarkerInt' put: 16r10!
PropertyListConstants at: 'kCFBinaryPlistMarkerNull' put: 16r0!
PropertyListConstants at: 'kCFBinaryPlistMarkerReal' put: 16r20!
PropertyListConstants at: 'kCFBinaryPlistMarkerTrue' put: 16r9!
PropertyListConstants at: 'kCFBinaryPlistMarkerUID' put: 16r80!
PropertyListConstants at: 'kCFBinaryPlistMarkerUnicode16String' put: 16r60!
PropertyListConstants at: 'NSPropertyListBinaryFormat_v1_0' put: 16rC8!
PropertyListConstants at: 'NSPropertyListImmutable' put: 16r0!
PropertyListConstants at: 'NSPropertyListMutableContainers' put: 16r1!
PropertyListConstants at: 'NSPropertyListMutableContainersAndLeaves' put: 16r2!
PropertyListConstants at: 'NSPropertyListOpenStepFormat' put: 16r1!
PropertyListConstants at: 'NSPropertyListXMLFormat_v1_0' put: 16r64!
PropertyListConstants shrink!

"Classes"!

NSPropertyListSerialization guid: (GUID fromString: '{CFB55EE4-BC84-449E-BB81-CAE69F0B1BF5}')!
NSPropertyListSerialization comment: ''!
!NSPropertyListSerialization categoriesForClass!Unclassified! !
!NSPropertyListSerialization class methodsFor!

errorNotAPropertyList
	self error: 'This is not an plist!!'!

errorWrongVersion
	self error: 'Unknown plist version'!

propertyListFromData: aString format: aFormat 


	aFormat = NSPropertyListXMLFormat_v1_0 
		ifTrue: [^NSXMLPropertyListSerialization propertyListFromData: aString].
	aFormat = NSPropertyListBinaryFormat_v1_0 
		ifTrue: [^NSBinaryPropertyListSerialization propertyListFromData: aString].
	self error: 'Unknown plist format'! !
!NSPropertyListSerialization class categoriesFor: #errorNotAPropertyList!public! !
!NSPropertyListSerialization class categoriesFor: #errorWrongVersion!public! !
!NSPropertyListSerialization class categoriesFor: #propertyListFromData:format:!public! !

CFBinaryPlistHeader guid: (GUID fromString: '{5765F8E2-1898-4061-AA2C-057B519B6157}')!
CFBinaryPlistHeader comment: ''!
!CFBinaryPlistHeader categoriesForClass!Unclassified! !
!CFBinaryPlistHeader methodsFor!

magic
	"Answer the receiver's magic field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress) length: 6!

version
	"Answer the receiver's version field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 6) length: 2! !
!CFBinaryPlistHeader categoriesFor: #magic!**compiled accessors**!public! !
!CFBinaryPlistHeader categoriesFor: #version!**compiled accessors**!public! !

!CFBinaryPlistHeader class methodsFor!

defineFields
	"Define CFBinaryPlistHeader structure
		CFBinaryPlistHeader compileDefinition

	typedef struct {
		uint8_t	_magic[6];
		uint8_t	_version[2];
	} CFBinaryPlistHeader;"

	self
		defineField: #magic type: (ArrayField type: String length: 6) beReadOnly;
		defineField: #version type:  (ArrayField type: String length: 2) beReadOnly! !
!CFBinaryPlistHeader class categoriesFor: #defineFields!public! !

CFBinaryPlistTrailer guid: (GUID fromString: '{5B3E39E6-B4A0-4A7E-BC01-B43CFFB058BF}')!
CFBinaryPlistTrailer comment: ''!
!CFBinaryPlistTrailer categoriesForClass!Unclassified! !
!CFBinaryPlistTrailer methodsFor!

numObjects
	"Answer the receiver's numObjects field as a Smalltalk object."

	^(bytes bigEndianQwordAtOffset: 8)!

numObjects: anObject
	"Set the receiver's numObjects field to the value of anObject."

	bytes bigEndianQwordAtOffset: 8 put: anObject!

objectRefSize
	"Answer the receiver's objectRefSize field as a Smalltalk object."

	^(bytes byteAtOffset: 7)!

objectRefSize: anObject
	"Set the receiver's objectRefSize field to the value of anObject."

	bytes byteAtOffset: 7 put: anObject!

offsetIntSize
	"Answer the receiver's offsetIntSize field as a Smalltalk object."

	^(bytes byteAtOffset: 6)!

offsetIntSize: anObject
	"Set the receiver's offsetIntSize field to the value of anObject."

	bytes byteAtOffset: 6 put: anObject!

offsetTableOffset
	"Answer the receiver's offsetTableOffset field as a Smalltalk object."

	^(bytes bigEndianQwordAtOffset: 24)!

offsetTableOffset: anObject
	"Set the receiver's offsetTableOffset field to the value of anObject."

	bytes bigEndianQwordAtOffset: 24 put: anObject!

topObject
	"Answer the receiver's topObject field as a Smalltalk object."

	^(bytes bigEndianQwordAtOffset: 16)!

topObject: anObject
	"Set the receiver's topObject field to the value of anObject."

	bytes bigEndianQwordAtOffset: 16 put: anObject! !
!CFBinaryPlistTrailer categoriesFor: #numObjects!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #numObjects:!**compiled accessors**!*-unreferenced selectors!public! !
!CFBinaryPlistTrailer categoriesFor: #objectRefSize!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #objectRefSize:!**compiled accessors**!*-unreferenced selectors!public! !
!CFBinaryPlistTrailer categoriesFor: #offsetIntSize!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #offsetIntSize:!**compiled accessors**!*-unreferenced selectors!public! !
!CFBinaryPlistTrailer categoriesFor: #offsetTableOffset!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #offsetTableOffset:!**compiled accessors**!*-unreferenced selectors!public! !
!CFBinaryPlistTrailer categoriesFor: #topObject!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #topObject:!**compiled accessors**!*-unreferenced selectors!public! !

!CFBinaryPlistTrailer class methodsFor!

defineFields
	"Define CFBinaryPlistTrailer structure
		CFBinaryPlistTrailer compileDefinition

	typedef struct {
		uint8_t	_unused[6];
		uint8_t	_offsetIntSize;
		uint8_t	_objectRefSize;
		uint64_t	_numObjects;
		uint64_t	_topObject;
		uint64_t	_offsetTableOffset;
	} CFBinaryPlistTrailer;"

	self
		defineField: #unused type: (FillerField byteSize: 6);
		defineField: #offsetIntSize type: BYTEField new;
		defineField: #objectRefSize type: BYTEField new;
		defineField: #numObjects type: BigEndianQWORDField new;
		defineField: #topObject type: BigEndianQWORDField new;
		defineField: #offsetTableOffset type: BigEndianQWORDField new! !
!CFBinaryPlistTrailer class categoriesFor: #defineFields!public! !

NSBinaryPropertyListSerialization guid: (GUID fromString: '{5ED2E2A4-AB9C-4DD8-A3FD-CD1591A44493}')!
NSBinaryPropertyListSerialization comment: ''!
!NSBinaryPropertyListSerialization categoriesForClass!Unclassified! !
!NSBinaryPropertyListSerialization methodsFor!

objectIsOversized: objectType 
	^(objectType bitAnd: 16rF) = 15!

parse
	|topObject |
	topObject := trailer topObject.
	^self parse: topObject!

parse: anObjectIndex 
	| objectOffset objectType |
	objectOffset := offsetTable at: anObjectIndex + 1.
	objectType := data byteAtOffset: objectOffset.
	objectType = kCFBinaryPlistMarkerNull ifTrue: [^nil].
	objectType = kCFBinaryPlistMarkerFalse ifTrue: [^false].
	objectType = kCFBinaryPlistMarkerTrue ifTrue: [^true].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerInt 
		ifTrue: [^self parseIntegerAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerReal 
		ifTrue: [^self parseRealAt: objectOffset].
	objectType = kCFBinaryPlistMarkerDate ifTrue: [^self parseDateAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerData 
		ifTrue: [^self parseDataAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerASCIIString 
		ifTrue: [^self parseStringAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerUnicode16String 
		ifTrue: [^self parseUnicodeStringAt: objectOffset].
	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerArray 
		ifTrue: [^self parseArrayAt: objectOffset].

	(objectType bitAnd: 16rF0) = kCFBinaryPlistMarkerDict 
		ifTrue: [^self parseDictionaryAt: objectOffset].




	self error: 'Unknown data type'!

parseArrayAt: objectOffset 
	| objectType length offset array |
	objectType := data byteAtOffset: objectOffset.
	(self objectIsOversized: objectType) 
		ifFalse: 
			[offset := 1.
			length := self parseLengthAt: objectOffset]
		ifTrue: 
			[length := self parseIntegerAt: objectOffset + 1.
			offset := (self parsePower2LengthAt: objectOffset + 1) + 2].
	array := data 
				arraySized: trailer objectRefSize
				atOffset: objectOffset + offset
				length: length.


	^array asArray collect: [:each | self parse: each]!

parseDataAt: objectOffset 
	| objectType length offset |
	objectType := data byteAtOffset: objectOffset.
	(self objectIsOversized: objectType) 
		ifFalse: 
			[offset := 1.
			length := self parseLengthAt: objectOffset]
		ifTrue: 
			[length := self parseIntegerAt: objectOffset + 1.
			offset := (self parsePower2LengthAt: objectOffset + 1) + 2].
	^data copyFromOffset: objectOffset + offset length: length!

parseDateAt: objectOffset 
	^TimeStamp fromCoreFoundationDouble: (data bigEndianDoubleAtOffset: objectOffset+1)!

parseDictionaryAt: objectOffset 
	| objectType length offset array dictionary |
	objectType := data byteAtOffset: objectOffset.
	(self objectIsOversized: objectType) 
		ifFalse: 
			[offset := 1.
			length := self parseLengthAt: objectOffset]
		ifTrue: 
			[length := self parseIntegerAt: objectOffset + 1.
			offset := (self parsePower2LengthAt: objectOffset + 1) + 2].
	
	array := data 
				arraySized: trailer objectRefSize
				atOffset: objectOffset + offset
				length: length * 2.
	dictionary := Dictionary new.
	1 to: length
		do: 
			[:index | 
			dictionary at: (self parse: (array at: index))
				put: (self parse: (array at: index + length))].
	^dictionary!

parseIntegerAt: objectOffset 
	| length |
	length := self parsePower2LengthAt: objectOffset.
	length = 1 ifTrue: [^data byteAtOffset: objectOffset +1].
	length = 2 ifTrue: [^data bigEndianWordAtOffset: objectOffset+1].
	length = 4 ifTrue: [^data bigEndianDwordAtOffset: objectOffset+1].
	length = 8 ifTrue: [^data bigEndianQwordAtOffset: objectOffset+1].
	self error: 'Unknown Integer format'!

parseLengthAt: objectOffset 
	^(data byteAtOffset: objectOffset) bitAnd: 16r0F!

parsePower2LengthAt: objectOffset 
	^2 raisedTo: (self parseLengthAt: objectOffset )!

parseRealAt: objectOffset 
	| length |
	length := self parsePower2LengthAt: objectOffset.
	length = 4 ifTrue: [^data bigEndianFloatAtOffset: objectOffset +1].
	length = 8 ifTrue: [^data bigEndianDoubleAtOffset: objectOffset+1 ].
	self error: 'Unknown Float format'!

parseStringAt: objectOffset 	^(self parseDataAt: objectOffset ) asString!

parseUnicodeStringAt: objectOffset 
	^(self parseStringAt: objectOffset )asUnicodeString!

setData: aByteArray header: aCFBinaryPlistHeader trailer: aCFBinaryPlistTrailer 
	| array |
	data := aByteArray.
	header := aCFBinaryPlistHeader.
	trailer := aCFBinaryPlistTrailer.
	offsetTable := data 
				arraySized: trailer offsetIntSize
				atOffset: trailer offsetTableOffset
				length: trailer numObjects

	! !
!NSBinaryPropertyListSerialization categoriesFor: #objectIsOversized:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parse!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parse:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseArrayAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseDataAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseDateAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseDictionaryAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseIntegerAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseLengthAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parsePower2LengthAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseRealAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseStringAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #parseUnicodeStringAt:!public! !
!NSBinaryPropertyListSerialization categoriesFor: #setData:header:trailer:!public! !

!NSBinaryPropertyListSerialization class methodsFor!

propertyListFromData: aByteArray 
	| header trailer |
	header := CFBinaryPlistHeader fromBytes: (aByteArray first: CFBinaryPlistHeader byteSize).
	trailer := CFBinaryPlistTrailer 
				fromBytes: (aByteArray last: CFBinaryPlistTrailer byteSize).
	header magic ~= 'bplist' ifTrue: [self errorNotAPropertyList].
	header version ~= '00' ifTrue: [self errorWrongVersion].
	^(self new)
		setData: aByteArray
			header: header
			trailer: trailer;
		parse! !
!NSBinaryPropertyListSerialization class categoriesFor: #propertyListFromData:!public! !

NSXMLPropertyListSerialization guid: (GUID fromString: '{94A9D591-FE44-43E5-B926-0742FF909538}')!
NSXMLPropertyListSerialization comment: ''!
!NSXMLPropertyListSerialization categoriesForClass!Unclassified! !
!NSXMLPropertyListSerialization methodsFor!

parse: anXMLElement 
	anXMLElement tagName = 'true' ifTrue: [^true].
	anXMLElement tagName = 'false' ifTrue: [^false].
	anXMLElement tagName = 'integer' ifTrue: [^self parseNumber: anXMLElement].
	anXMLElement tagName = 'real' ifTrue: [^self parseNumber: anXMLElement].
	anXMLElement tagName = 'data' ifTrue: [^self parseData: anXMLElement].
	anXMLElement tagName = 'date' ifTrue: [^TimeStamp fromRfc3339String: anXMLElement text].
	anXMLElement tagName = 'string' ifTrue: [^anXMLElement text].
	anXMLElement tagName = 'array' ifTrue: [^self parseArray: anXMLElement].
	anXMLElement tagName = 'dict' ifTrue: [^self parseDictionary: anXMLElement].
	self error: 'Unknown property found (' , anXMLElement tagName , ')'!

parseArray: anXMLElement 
	^anXMLElement childNodes collect: [:eachElement | self parse: eachElement]!

parseData: anXMLElement 
	| stream |
	stream := ReadWriteStream on: ByteArray new.
	Base64Codec decodeFrom: (ReadStream on: anXMLElement text) onto: stream.
	^stream contents!

parseDictionary: anXMLElement 
	| dic |
	dic := Dictionary new.
	1 to: anXMLElement childNodes size
		by: 2
		do: 
			[:index | 
			| key value |
			key := (anXMLElement childNodes at: index) text.
			value := self parse: (anXMLElement childNodes at: index + 1).
			dic at: key put: value].
	^dic!

parseNumber: anXMLElement 
	^Number fromString: anXMLElement text! !
!NSXMLPropertyListSerialization categoriesFor: #parse:!public! !
!NSXMLPropertyListSerialization categoriesFor: #parseArray:!public! !
!NSXMLPropertyListSerialization categoriesFor: #parseData:!public! !
!NSXMLPropertyListSerialization categoriesFor: #parseDictionary:!public! !
!NSXMLPropertyListSerialization categoriesFor: #parseNumber:!public! !

!NSXMLPropertyListSerialization class methodsFor!

errorXMLParsing
	self error: 'Error parsing XML plist'!

propertyListFromData: aByteArray 
	| doc root |
	doc := (IXMLDOMDocument new)
				resolveExternals: false;
				validateOnParse: false;
				yourself.
	(doc loadXML: aByteArray asString) ifFalse: [self errorXMLParsing].
	root := doc documentElement.
	root tagName ~= 'plist' ifTrue: [self errorNotAPropertyList].
	(root getAttribute: 'version') ~= '1.0' ifTrue: [self errorWrongVersion].
	^self new parse: root firstChild! !
!NSXMLPropertyListSerialization class categoriesFor: #errorXMLParsing!public! !
!NSXMLPropertyListSerialization class categoriesFor: #propertyListFromData:!public! !

"Binary Globals"!

