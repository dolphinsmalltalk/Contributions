| package |
package := Package name: 'US Font Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Font Extensions 0.004$, $date: 24.07.2009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) 2007, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.004'.


package classNames
	add: #TT_NAME_RECORD;
	add: #TT_NAME_TABLE_HEADER;
	add: #TT_OFFSET_TABLE;
	add: #TT_TABLE_DIRECTORY;
	yourself.

package methodNames
	add: 'Font class' -> #fromFile:;
	add: 'Font class' -> #fromFile:pixelSize:;
	add: 'Font class' -> #fromFile:pointSize:;
	add: 'Font class' -> #loadFont:;
	add: 'Font class' -> #nameFromFile:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Big Endian Conversion\US BigEndian';
	yourself).

package!

"Class Definitions"!

ExternalStructure subclass: #TT_NAME_RECORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #TT_NAME_TABLE_HEADER
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #TT_OFFSET_TABLE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #TT_TABLE_DIRECTORY
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Font class methodsFor!

fromFile: aFilename 
	^self name: (self loadFont: aFilename)!

fromFile: aFilename pixelSize: anInteger
	^self name: (self loadFont: aFilename) pixelSize: anInteger!

fromFile: aFilename pointSize: anInteger 
	^self name: (self loadFont: aFilename) pointSize: anInteger!

loadFont: aFilename 
	"Loads font from a file if not already installed.

	Returns the font name"

	| fontName |
	fontName := self nameFromFile: aFilename.
	(View desktop canvas fontNames includes: fontName) 
		ifFalse: 
			[(GDILibrary default addFontResource: aFilename) = 0 
				ifTrue: [self error: 'Font ' , aFilename , ' could not be loaded']].
	^fontName!

nameFromFile: aFilename 
	"Returns the name of a given (True Type) font.

	See http://www.codeproject.com/KB/GDI/fontnamefromfile.aspx"

	| stream offsetTable tableDirectory nameTable nameTableHeader records nameRecord ttfName |
	stream := FileStream read: aFilename text: false.
	
	[offsetTable := stream nextStructure: TT_OFFSET_TABLE.
	(offsetTable uMajorVersion = 1 and: [offsetTable uMinorVersion = 0]) 
		ifFalse: [self error: 'Only TT 1.0 supported'].
	tableDirectory := (1 to: offsetTable uNumOfTables) 
				collect: [:index | stream nextStructure: TT_TABLE_DIRECTORY].
	nameTable := tableDirectory detect: [:each | each szTag = 'name']
				ifNone: [self error: 'Name Directory entry not found'].
	stream position: nameTable uOffset.
	nameTableHeader := stream nextStructure: TT_NAME_TABLE_HEADER.
	records := (1 to: nameTableHeader uNRCount) 
				collect: [:index | stream nextStructure: TT_NAME_RECORD].
	nameRecord := records detect: [:each | each uNameID = 1]
				ifNone: [self error: 'Name record not found'].
	stream position: nameTable uOffset + nameTableHeader uStorageOffset + nameRecord uStringOffset.
	ttfName := stream next: nameRecord uStringLength] 
			ensure: [stream close].
	^ttfName asString! !
!Font class categoriesFor: #fromFile:!instance creation!public! !
!Font class categoriesFor: #fromFile:pixelSize:!instance creation!public! !
!Font class categoriesFor: #fromFile:pointSize:!instance creation!public! !
!Font class categoriesFor: #loadFont:!helpers!instance creation!private! !
!Font class categoriesFor: #nameFromFile:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

TT_NAME_RECORD guid: (GUID fromString: '{E6AA3468-5148-46F1-898D-407CE2408C7A}')!
TT_NAME_RECORD comment: ''!
!TT_NAME_RECORD categoriesForClass!Unclassified! !
!TT_NAME_RECORD methodsFor!

uEncodingID
	"Answer the receiver's uEncodingID field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 2)!

uEncodingID: anObject
	"Set the receiver's uEncodingID field to the value of anObject."

	bytes bigEndianWordAtOffset: 2 put: anObject!

uLanguageID
	"Answer the receiver's uLanguageID field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 4)!

uLanguageID: anObject
	"Set the receiver's uLanguageID field to the value of anObject."

	bytes bigEndianWordAtOffset: 4 put: anObject!

uNameID
	"Answer the receiver's uNameID field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 6)!

uNameID: anObject
	"Set the receiver's uNameID field to the value of anObject."

	bytes bigEndianWordAtOffset: 6 put: anObject!

uPlatformID
	"Answer the receiver's uPlatformID field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 0)!

uPlatformID: anObject
	"Set the receiver's uPlatformID field to the value of anObject."

	bytes bigEndianWordAtOffset: 0 put: anObject!

uStringLength
	"Answer the receiver's uStringLength field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 8)!

uStringLength: anObject
	"Set the receiver's uStringLength field to the value of anObject."

	bytes bigEndianWordAtOffset: 8 put: anObject!

uStringOffset
	"Answer the receiver's uStringOffset field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 10)!

uStringOffset: anObject
	"Set the receiver's uStringOffset field to the value of anObject."

	bytes bigEndianWordAtOffset: 10 put: anObject! !
!TT_NAME_RECORD categoriesFor: #uEncodingID!public! !
!TT_NAME_RECORD categoriesFor: #uEncodingID:!public! !
!TT_NAME_RECORD categoriesFor: #uLanguageID!public! !
!TT_NAME_RECORD categoriesFor: #uLanguageID:!public! !
!TT_NAME_RECORD categoriesFor: #uNameID!public! !
!TT_NAME_RECORD categoriesFor: #uNameID:!public! !
!TT_NAME_RECORD categoriesFor: #uPlatformID!public! !
!TT_NAME_RECORD categoriesFor: #uPlatformID:!public! !
!TT_NAME_RECORD categoriesFor: #uStringLength!public! !
!TT_NAME_RECORD categoriesFor: #uStringLength:!public! !
!TT_NAME_RECORD categoriesFor: #uStringOffset!public! !
!TT_NAME_RECORD categoriesFor: #uStringOffset:!public! !

!TT_NAME_RECORD class methodsFor!

defineFields
	"Define the fields of the TT_NAME_RECORD structure.
		TT_NAME_RECORD compileDefinition
	
		typedef struct _tagTT_NAME_RECORD{
    USHORT uPlatformID;
    USHORT uEncodingID;
    USHORT uLanguageID;
    USHORT uNameID;
    USHORT uStringLength;
    USHORT uStringOffset; //from start of storage area

}TT_NAME_RECORD;
"

	self
		defineField: #uPlatformID type: BigEndianWORDField new;
		defineField: #uEncodingID type: BigEndianWORDField new;
		defineField: #uLanguageID type: BigEndianWORDField new;
		defineField: #uNameID type: BigEndianWORDField new;
		defineField: #uStringLength type: BigEndianWORDField new;
		defineField: #uStringOffset type: BigEndianWORDField new! !
!TT_NAME_RECORD class categoriesFor: #defineFields!public! !

TT_NAME_TABLE_HEADER guid: (GUID fromString: '{1FCF03B7-EBC9-4532-9CFC-CDB4370D4DE3}')!
TT_NAME_TABLE_HEADER comment: ''!
!TT_NAME_TABLE_HEADER categoriesForClass!Unclassified! !
!TT_NAME_TABLE_HEADER methodsFor!

uFSelector
	"Answer the receiver's uFSelector field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 0)!

uFSelector: anObject
	"Set the receiver's uFSelector field to the value of anObject."

	bytes bigEndianWordAtOffset: 0 put: anObject!

uNRCount
	"Answer the receiver's uNRCount field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 2)!

uNRCount: anObject
	"Set the receiver's uNRCount field to the value of anObject."

	bytes bigEndianWordAtOffset: 2 put: anObject!

uStorageOffset
	"Answer the receiver's uStorageOffset field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 4)!

uStorageOffset: anObject
	"Set the receiver's uStorageOffset field to the value of anObject."

	bytes bigEndianWordAtOffset: 4 put: anObject! !
!TT_NAME_TABLE_HEADER categoriesFor: #uFSelector!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uFSelector:!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uNRCount!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uNRCount:!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uStorageOffset!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uStorageOffset:!public! !

!TT_NAME_TABLE_HEADER class methodsFor!

defineFields
	"Define the fields of the TT_NAME_TABLE_HEADER structure.
		TT_NAME_TABLE_HEADER compileDefinition
	
typedef struct _tagTT_NAME_TABLE_HEADER{
    USHORT uFSelector; //format selector. Always 0

    USHORT uNRCount; //Name Records count

    USHORT uStorageOffset; //Offset for strings storage, 

                           //from start of the table

}TT_NAME_TABLE_HEADER;
"

	self
		defineField: #uFSelector type: BigEndianWORDField new;
		defineField: #uNRCount type: BigEndianWORDField new;
		defineField: #uStorageOffset type: BigEndianWORDField new! !
!TT_NAME_TABLE_HEADER class categoriesFor: #defineFields!public! !

TT_OFFSET_TABLE guid: (GUID fromString: '{CC003CBB-20E1-46C6-8191-2BB134185A1F}')!
TT_OFFSET_TABLE comment: ''!
!TT_OFFSET_TABLE categoriesForClass!Unclassified! !
!TT_OFFSET_TABLE methodsFor!

uEntrySelector
	"Answer the receiver's uEntrySelector field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 8)!

uEntrySelector: anObject
	"Set the receiver's uEntrySelector field to the value of anObject."

	bytes bigEndianWordAtOffset: 8 put: anObject!

uMajorVersion
	"Answer the receiver's uMajorVersion field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 0)!

uMajorVersion: anObject
	"Set the receiver's uMajorVersion field to the value of anObject."

	bytes bigEndianWordAtOffset: 0 put: anObject!

uMinorVersion
	"Answer the receiver's uMinorVersion field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 2)!

uMinorVersion: anObject
	"Set the receiver's uMinorVersion field to the value of anObject."

	bytes bigEndianWordAtOffset: 2 put: anObject!

uNumOfTables
	"Answer the receiver's uNumOfTables field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 4)!

uNumOfTables: anObject
	"Set the receiver's uNumOfTables field to the value of anObject."

	bytes bigEndianWordAtOffset: 4 put: anObject!

uRangeShift
	"Answer the receiver's uRangeShift field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 10)!

uRangeShift: anObject
	"Set the receiver's uRangeShift field to the value of anObject."

	bytes bigEndianWordAtOffset: 10 put: anObject!

uSearchRange
	"Answer the receiver's uSearchRange field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 6)!

uSearchRange: anObject
	"Set the receiver's uSearchRange field to the value of anObject."

	bytes bigEndianWordAtOffset: 6 put: anObject! !
!TT_OFFSET_TABLE categoriesFor: #uEntrySelector!public! !
!TT_OFFSET_TABLE categoriesFor: #uEntrySelector:!public! !
!TT_OFFSET_TABLE categoriesFor: #uMajorVersion!public! !
!TT_OFFSET_TABLE categoriesFor: #uMajorVersion:!public! !
!TT_OFFSET_TABLE categoriesFor: #uMinorVersion!public! !
!TT_OFFSET_TABLE categoriesFor: #uMinorVersion:!public! !
!TT_OFFSET_TABLE categoriesFor: #uNumOfTables!public! !
!TT_OFFSET_TABLE categoriesFor: #uNumOfTables:!public! !
!TT_OFFSET_TABLE categoriesFor: #uRangeShift!public! !
!TT_OFFSET_TABLE categoriesFor: #uRangeShift:!public! !
!TT_OFFSET_TABLE categoriesFor: #uSearchRange!public! !
!TT_OFFSET_TABLE categoriesFor: #uSearchRange:!public! !

!TT_OFFSET_TABLE class methodsFor!

defineFields
	"Define the fields of the TT_OFFSET_TABLE structure.
		TT_OFFSET_TABLE compileDefinition
	
typedef struct _tagTT_OFFSET_TABLE{
    USHORT uMajorVersion;
    USHORT uMinorVersion;
    USHORT uNumOfTables;
    USHORT uSearchRange;
    USHORT uEntrySelector;
    USHORT uRangeShift;
}TT_OFFSET_TABLE;
"

	self
		defineField: #uMajorVersion type: BigEndianWORDField new;
		defineField: #uMinorVersion type: BigEndianWORDField new;
		defineField: #uNumOfTables type: BigEndianWORDField new;
		defineField: #uSearchRange type: BigEndianWORDField new;
		defineField: #uEntrySelector type: BigEndianWORDField new;
		defineField: #uRangeShift type: BigEndianWORDField new! !
!TT_OFFSET_TABLE class categoriesFor: #defineFields!public! !

TT_TABLE_DIRECTORY guid: (GUID fromString: '{4A277749-634A-4910-96C1-D619FA9A55FA}')!
TT_TABLE_DIRECTORY comment: ''!
!TT_TABLE_DIRECTORY categoriesForClass!Unclassified! !
!TT_TABLE_DIRECTORY methodsFor!

szTag
	"Answer the receiver's szTag field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress) length: 4!

szTag: anObject
	"Set the receiver's szTag field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (4 * 1).
	anObject replaceBytesOf: bytes from: 1 to: size startingAt: 1!

uCheckSum
	"Answer the receiver's uCheckSum field as a Smalltalk object."

	^(bytes bigEndianDwordAtOffset: 4)!

uCheckSum: anObject
	"Set the receiver's uCheckSum field to the value of anObject."

	bytes bigEndianDwordAtOffset: 4 put: anObject!

uLength
	"Answer the receiver's uLength field as a Smalltalk object."

	^(bytes bigEndianDwordAtOffset: 12)!

uLength: anObject
	"Set the receiver's uLength field to the value of anObject."

	bytes bigEndianDwordAtOffset: 12 put: anObject!

uOffset
	"Answer the receiver's uOffset field as a Smalltalk object."

	^(bytes bigEndianDwordAtOffset: 8)!

uOffset: anObject
	"Set the receiver's uOffset field to the value of anObject."

	bytes bigEndianDwordAtOffset: 8 put: anObject! !
!TT_TABLE_DIRECTORY categoriesFor: #szTag!public! !
!TT_TABLE_DIRECTORY categoriesFor: #szTag:!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uCheckSum!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uCheckSum:!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uLength!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uLength:!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uOffset!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uOffset:!public! !

!TT_TABLE_DIRECTORY class methodsFor!

defineFields
	"Define the fields of the TT_TABLE_DIRECTORY structure.
		TT_TABLE_DIRECTORY compileDefinition
	
typedef struct _tagTT_TABLE_DIRECTORY{
    char szTag[4]; //table name

    ULONG uCheckSum; //Check sum

    ULONG uOffset; //Offset from beginning of file

    ULONG uLength; //length of the table in bytes

}TT_TABLE_DIRECTORY;
"

	self
		defineField: #szTag type: (ArrayField type: String length: 4);
		defineField: #uCheckSum type: BigEndianDWORDField new;
		defineField: #uOffset type: BigEndianDWORDField new;
		defineField: #uLength type: BigEndianDWORDField new! !
!TT_TABLE_DIRECTORY class categoriesFor: #defineFields!public! !

"Binary Globals"!

