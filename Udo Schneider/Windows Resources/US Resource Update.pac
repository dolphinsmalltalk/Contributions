| package |
package := Package name: 'US Resource Update'.
package paxVersion: 1;
	basicComment: '$id: US Resource Update 0.038$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 03.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.038'.


package classNames
	add: #AbstractWindowsResource;
	add: #BinaryResource;
	add: #BINRESDIR;
	add: #BitmapResource;
	add: #CURSORDIR;
	add: #CURSORIMAGE;
	add: #CursorResource;
	add: #FILERESDIR;
	add: #FlatResource;
	add: #GroupCursorResource;
	add: #GroupIconResource;
	add: #ICONIMAGE;
	add: #ICONRESDIR;
	add: #IconResource;
	add: #NEWHEADER;
	add: #RESDIR;
	add: #StringTableResource;
	add: #UserResource;
	add: #WindowsResourceUpdater;
	yourself.

package methodNames
	add: #Bitmap -> #asDIBSection;
	add: #Bitmap -> #resourceBytes;
	add: #ByteArray -> #asUnicodeString;
	add: #DIBSection -> #asByteArray;
	add: #DIBSection -> #asDIBSection;
	add: #DIBSection -> #getBITMAPINFOHEADER;
	add: #DIBSection -> #resourceBytes;
	add: #GdiplusBitmap -> #asIcon;
	add: #Image -> #asWindowsResource;
	add: #Image -> #resourceBytes;
	add: #KernelLibrary -> #deleteResource:lpType:lpName:wLanguage:;
	add: #Number -> #baseOffsetFromResourceId;
	add: #Number -> #offsetInStringTable;
	add: #Number -> #resourceIdFromOffset;
	add: #Object -> #asWindowsResource;
	add: #Object -> #defaultResourceClass;
	add: #Object -> #resourceBytes;
	add: #OLEPicture -> #asByteArray;
	add: #OLEPicture -> #asWindowsResource;
	add: #OLEPicture -> #resourceBytes;
	add: 'Bitmap class' -> #defaultResourceClass;
	add: 'Cursor class' -> #defaultResourceClass;
	add: 'Icon class' -> #defaultResourceClass;
	add: 'Object class' -> #defaultResourceClass;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	add: '..\..\..\Object Arts\Dolphin\ActiveX\Components\Picture\OLE Picture';
	add: '..\..\..\Object Arts\Dolphin\ActiveX\Structured Storage\OLE Structured Storage';
	add: 'US Resource Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Resource Extensions').

package!

"Class Definitions"!

Object subclass: #AbstractWindowsResource
	instanceVariableNames: 'identifier locale'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Object subclass: #WindowsResourceUpdater
	instanceVariableNames: 'filename'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
AbstractWindowsResource subclass: #FlatResource
	instanceVariableNames: 'bytes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractWindowsResource subclass: #GroupIconResource
	instanceVariableNames: 'entries offset'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
AbstractWindowsResource variableSubclass: #StringTableResource
	instanceVariableNames: 'entries'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
FlatResource subclass: #BinaryResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FlatResource subclass: #BitmapResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FlatResource subclass: #IconResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BinaryResource subclass: #UserResource
	instanceVariableNames: 'type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IconResource subclass: #CursorResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GroupIconResource subclass: #GroupCursorResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #CURSORDIR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #CURSORIMAGE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #ICONIMAGE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #ICONRESDIR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #NEWHEADER
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #RESDIR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RESDIR subclass: #BINRESDIR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RESDIR subclass: #FILERESDIR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Bitmap methodsFor!

asDIBSection
		| bitmap |
	bitmap := DIBSection  width: self extent x height: self extent y depth: self depth .
	self drawOn: bitmap canvas.
	^bitmap!

resourceBytes
 ^self asDIBSection resourceBytes! !
!Bitmap categoriesFor: #asDIBSection!public! !
!Bitmap categoriesFor: #resourceBytes!public! !

!Bitmap class methodsFor!

defaultResourceClass
	^BitmapResource! !
!Bitmap class categoriesFor: #defaultResourceClass!public! !

!ByteArray methodsFor!

asUnicodeString
	"Answer a UnicodeString composed of the characters of the receiver, or
	the empty string if the receiver is a null pointer"

	^UnicodeString fromAddress: self yourAddress  length: self size //2! !
!ByteArray categoriesFor: #asUnicodeString!comparing!public! !

!Cursor class methodsFor!

defaultResourceClass
	^GroupCursorResource ! !
!Cursor class categoriesFor: #defaultResourceClass!public! !

!DIBSection methodsFor!

asByteArray
	| stream |
	stream := ReadWriteStream on: ByteArray new.
	self saveToStream: stream.
	^stream contents.!

asDIBSection
^self!

getBITMAPINFOHEADER
^ self getDIBSECTION dsBmih !

resourceBytes
	| bitmapInfo infoHeader colors stream |
	self cacheInfo.
	infoHeader := self getBITMAPINFOHEADER asByteArray.
	colors := self getColorTable ifNil: [#[]] ifNotNil: [:value | value asByteArray].
	bitmapInfo := self getInfo.
	imageBits := ByteArray fromAddress: self imageBits
				length: bitmapInfo bmHeight * bitmapInfo bmWidthBytes.
	stream := ReadWriteStream on: ByteArray new.
	stream
		nextPutAll: infoHeader;
		nextPutAll: colors;
		nextPutAll: imageBits.
	^stream contents! !
!DIBSection categoriesFor: #asByteArray!public! !
!DIBSection categoriesFor: #asDIBSection!public! !
!DIBSection categoriesFor: #getBITMAPINFOHEADER!public! !
!DIBSection categoriesFor: #resourceBytes!public! !

!GdiplusBitmap methodsFor!

asIcon
	| xor mask canvas |
	xor := DIBSection 
				width: self width
				height: self height
				depth: 32.
	mask := DIBSection 
				width: self width
				height: self height
				depth: 1.
	mask setColors: (Array with: Color white with: Color black).
	canvas := mask canvas.
	0 to: self height - 1
		do: 
			[:y | 
			0 to: self width - 1
				do: 
					[:x | 
					| col |
					col := self pixelAt: x @ y.
					canvas pixelAt: x @ y put: (col alpha > 128 ifTrue: [Color black] ifFalse: [Color white]).
					xor imageBits dwordAtOffset: y * xor getInfo bmWidthBytes + (x * 4) put: col asParameter]].
	^(ICONIMAGE xor: xor and: mask) asIcon! !
!GdiplusBitmap categoriesFor: #asIcon!public! !

!Icon class methodsFor!

defaultResourceClass
	^GroupIconResource! !
!Icon class categoriesFor: #defaultResourceClass!public! !

!Image methodsFor!

asWindowsResource

	^self defaultResourceClass fromImage: self!

resourceBytes
	self shouldNotImplement! !
!Image categoriesFor: #asWindowsResource!public! !
!Image categoriesFor: #resourceBytes!public! !

!KernelLibrary methodsFor!

deleteResource: hUpdate lpType: resTypeString lpName: resName wLanguage: langId 
^self updateResource: hUpdate lpType: resTypeString lpName: resName wLanguage: langId lpData: nil cbData: nil ! !
!KernelLibrary categoriesFor: #deleteResource:lpType:lpName:wLanguage:!public! !

!Number methodsFor!

baseOffsetFromResourceId
	^(self - 1) * 16!

offsetInStringTable
^self \\ 16!

resourceIdFromOffset
	^self // 16 + 1! !
!Number categoriesFor: #baseOffsetFromResourceId!public! !
!Number categoriesFor: #offsetInStringTable!public! !
!Number categoriesFor: #resourceIdFromOffset!public! !

!Object methodsFor!

asWindowsResource
	
	^(self defaultResourceClass 
		fromResourceBytes: self resourceBytes
		id: nil
		locale: nil
		in: nil)
		type: self resourceType;
		yourself!

defaultResourceClass
	^self class defaultResourceClass!

resourceBytes
^self binaryStoreBytes! !
!Object categoriesFor: #asWindowsResource!public! !
!Object categoriesFor: #defaultResourceClass!public! !
!Object categoriesFor: #resourceBytes!public! !

!Object class methodsFor!

defaultResourceClass
	^UserResource! !
!Object class categoriesFor: #defaultResourceClass!public! !

!OLEPicture methodsFor!

asByteArray
^self resourceBytes!

asWindowsResource

	^super asWindowsResource type: self resourceType; yourself!

resourceBytes
	| stream iStream |
	stream := ReadWriteStream on: ByteArray new.
	iStream := stream queryInterface: IStream.
	self picture saveAsFile: iStream fSaveMemCopy: true.
	iStream free.
	^stream contents! !
!OLEPicture categoriesFor: #asByteArray!public! !
!OLEPicture categoriesFor: #asWindowsResource!*-not in class package!public! !
!OLEPicture categoriesFor: #resourceBytes!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

AbstractWindowsResource guid: (GUID fromString: '{B0524098-1DD4-47CD-8CBA-CA51C72C94BD}')!
AbstractWindowsResource comment: ''!
!AbstractWindowsResource categoriesForClass!Unclassified! !
!AbstractWindowsResource methodsFor!

identifier
	^identifier!

identifier: aResourceId 
	identifier := aResourceId!

initialize
^self!

locale
	locale isNil ifTrue: [locale  := Locale dolphinResourceDefault].
^locale!

locale: aLocale
locale := aLocale!

packedResourceBytes
	self subclassResponsibility!

resourceType ^ self class resourceType!

updateResource: hUpdate 
	| resourceBytes |
	
	resourceBytes := self packedResourceBytes.
	(KernelLibrary default 
		updateResource: hUpdate
		lpType: self resourceType asParameter
		lpName: (self identifier isString ifTrue: [self identifier asUppercase] ifFalse: [self identifier]) asParameter
		wLanguage: self locale asParameter
		lpData: resourceBytes asParameter
		cbData: resourceBytes size) ifFalse: [KernelLibrary default systemError]! !
!AbstractWindowsResource categoriesFor: #identifier!accessing!public! !
!AbstractWindowsResource categoriesFor: #identifier:!*-in class package!accessing!public! !
!AbstractWindowsResource categoriesFor: #initialize!public! !
!AbstractWindowsResource categoriesFor: #locale!accessing!public! !
!AbstractWindowsResource categoriesFor: #locale:!accessing!public! !
!AbstractWindowsResource categoriesFor: #packedResourceBytes!private! !
!AbstractWindowsResource categoriesFor: #resourceType!public! !
!AbstractWindowsResource categoriesFor: #updateResource:!*-in class package!public! !

!AbstractWindowsResource class methodsFor!

extractResourceBytesType: aResourceType id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	^aLocale isNil 
		ifTrue: 
			[ByteArray 
				fromId: aResourceId
				in: anExternalLibraryOrHandle
				type: aResourceType]
		ifFalse: 
			[ByteArray 
				fromId: aResourceId
				locale: aLocale
				in: anExternalLibraryOrHandle
				type: aResourceType]!

fromId: aResourceId in: anExternalLibraryOrHandle 
	^self 
		fromId: aResourceId locale: nil in: anExternalLibraryOrHandle!

fromId: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	^self 
		fromResourceBytes: (self 
				extractResourceBytesType: self resourceType
				id: aResourceId
				locale: aLocale
				in: anExternalLibraryOrHandle)
		id: aResourceId
		locale: aLocale
		in: anExternalLibraryOrHandle!

fromImage: anImage 
	^anImage instanceHandle isNil 
		ifTrue: 
			[anImage identifier notNil 
				ifTrue: 
					["File"
					self fromResourceFile: (anImage fileLocator localFileSpecFor: anImage identifier)]
				ifFalse: 
					["Memory"
					self fromResourceBytes: anImage resourceBytes id: anImage identifier locale: nil in: anImage instanceHandle
				]]
		ifFalse: 
			["Resource"
			self fromId: anImage identifier in: anImage instanceHandle]!

fromResourceBytes: bytes id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	self subclassResponsibility!

fromResourceFile: aFilename 
	| stream bytes |
	stream := FileStream read: aFilename type: #binary.
	bytes := stream contents.
	stream close.
	^self fromResourceFileBytes: bytes in: aFilename!

fromResourceFileBytes: bytes in: aFilename 
	self subclassResponsibility!

id: aResourceId locale: aLocale 
	^(self new)
		identifier: aResourceId;
		locale: aLocale;
		yourself!

new
^super new initialize!

resourceType
^self subclassResponsibility! !
!AbstractWindowsResource class categoriesFor: #extractResourceBytesType:id:locale:in:!private! !
!AbstractWindowsResource class categoriesFor: #fromId:in:!public! !
!AbstractWindowsResource class categoriesFor: #fromId:locale:in:!public! !
!AbstractWindowsResource class categoriesFor: #fromImage:!public! !
!AbstractWindowsResource class categoriesFor: #fromResourceBytes:id:locale:in:!private! !
!AbstractWindowsResource class categoriesFor: #fromResourceFile:!public! !
!AbstractWindowsResource class categoriesFor: #fromResourceFileBytes:in:!private! !
!AbstractWindowsResource class categoriesFor: #id:locale:!public! !
!AbstractWindowsResource class categoriesFor: #new!public! !
!AbstractWindowsResource class categoriesFor: #resourceType!private! !

WindowsResourceUpdater guid: (GUID fromString: '{69889CC6-33EF-4C78-8D7A-790DC8D64763}')!
WindowsResourceUpdater comment: ''!
!WindowsResourceUpdater categoriesForClass!Unclassified! !
!WindowsResourceUpdater methodsFor!

closeLibraries
	(ExternalResourceLibrary allInstances select: [:each | each fileName = filename asLowercase]) 
		do: [:each | each close]!

filename
	^filename!

filename: anObject
	filename := anObject!

initialize!

openLibraryWhile: aBlock 
	| lib |
	lib := ExternalResourceLibrary open: filename.
	aBlock value: lib  . lib close!

removeResources
	| toDelete |
	self
		openLibraryWhile: 
				[:lib | 
				toDelete := Set new.
				self supportedResourceTypes 
					do: [:resourceType | toDelete addAll: (lib resourceIdentifiersOfType: resourceType)]];
		closeLibraries;
		removeResources: toDelete.
!

removeResources: aCollectionOfResourceIdentifiers
	self updateResourcesWhile: 
			[:hUpdate | 
			aCollectionOfResourceIdentifiers do: 
					[:each | 
					KernelLibrary default 
						deleteResource: hUpdate
						lpType: each type
						lpName: each id
						wLanguage: each locale asParameter]]!

supportedResourceTypes
	^Array 
		with: RT_ICON
		with: RT_GROUP_ICON
		with: RT_CURSOR
		with: RT_GROUP_CURSOR
		with: RT_BITMAP!

updateResourcesWhile: aBlock

	KernelLibrary default updateResourcesOf: filename
		do: 
			aBlock! !
!WindowsResourceUpdater categoriesFor: #closeLibraries!accessing!public! !
!WindowsResourceUpdater categoriesFor: #filename!accessing!public! !
!WindowsResourceUpdater categoriesFor: #filename:!accessing!private! !
!WindowsResourceUpdater categoriesFor: #initialize!public! !
!WindowsResourceUpdater categoriesFor: #openLibraryWhile:!accessing!public! !
!WindowsResourceUpdater categoriesFor: #removeResources!accessing!public! !
!WindowsResourceUpdater categoriesFor: #removeResources:!accessing!public! !
!WindowsResourceUpdater categoriesFor: #supportedResourceTypes!accessing!public! !
!WindowsResourceUpdater categoriesFor: #updateResourcesWhile:!accessing!public! !

!WindowsResourceUpdater class methodsFor!

new
^super new initialize!

onFile: aFilename
^self new filename: aFilename! !
!WindowsResourceUpdater class categoriesFor: #new!public! !
!WindowsResourceUpdater class categoriesFor: #onFile:!public! !

FlatResource guid: (GUID fromString: '{1B940B5E-37F7-4227-AAB2-FF7F92EA75F9}')!
FlatResource comment: 'Flat resources which exist as a ByteStream. The bytes that got in are the bytes to bw written.'!
!FlatResource categoriesForClass!Unclassified! !
!FlatResource methodsFor!

packedResourceBytes
	^bytes!

setIdentifier: aResourceId locale: aLocale bytes: aByteArray 
	identifier := aResourceId.
	locale := aLocale.
	bytes := aByteArray! !
!FlatResource categoriesFor: #packedResourceBytes!*-in class package!private! !
!FlatResource categoriesFor: #setIdentifier:locale:bytes:!*-in class package!private! !

!FlatResource class methodsFor!

fromResourceBytes: bytes id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	^(self new)
		setIdentifier: aResourceId
			locale: aLocale
			bytes: bytes;
		yourself!

fromResourceFileBytes: bytes in: aFilename 
	^self new 
		setIdentifier: (File splitFilenameFrom: aFilename)
		locale: nil
		bytes: bytes; yourself! !
!FlatResource class categoriesFor: #fromResourceBytes:id:locale:in:!*-in class package!private! !
!FlatResource class categoriesFor: #fromResourceFileBytes:in:!*-in class package!private! !

GroupIconResource guid: (GUID fromString: '{67D915DD-A91B-4AF1-9201-E95B6536A222}')!
GroupIconResource comment: ''!
!GroupIconResource categoriesForClass!Unclassified! !
!GroupIconResource methodsFor!

addEntry: aRESDIR image: anIconOrCursorResource 
	entries add: aRESDIR -> anIconOrCursorResource!

entriesAndImagesDo: aBlock 
	
	
	(entries asSortedCollection: [ :x :y | x key <= y key]) do: [:each | aBlock value: each key value: each value]!

initialize
	entries := OrderedCollection new.
	^super initialize!

newheaderEntryType 
^RES_ICON
!

offset
	^offset!

offset: anInteger 
	offset := anInteger!

packedResourceBytes
	self shouldNotImplement!

updateEntryResources: hUpdate 
	| bytes  |
	bytes := (NEWHEADER new)
				ResType: self newheaderEntryType;
				ResCount: entries size;
				bytes.
	self entriesAndImagesDo: 
			[:entry :icon | 
			icon
				identifier: offset;
				updateResource: hUpdate.
			bytes := bytes , ((entry asBINRESDIR)
								IconCursorId: offset;
								bytes).
			offset := offset + 1].
	^bytes!

updateGroupResourceIn: hUpdate withGroup: groupBytes 
	(KernelLibrary default 
		updateResource: hUpdate
		lpType: self class resourceType asParameter
		lpName: (self identifier isString ifTrue: [self identifier asUppercase] ifFalse: [self identifier])
		wLanguage: self locale asParameter
		lpData: groupBytes asParameter
		cbData: groupBytes size) ifFalse: [KernelLibrary default systemError]!

updateResource: hUpdate 
	| group |
	group := self updateEntryResources: hUpdate.
	self updateGroupResourceIn: hUpdate withGroup: group! !
!GroupIconResource categoriesFor: #addEntry:image:!public! !
!GroupIconResource categoriesFor: #entriesAndImagesDo:!public! !
!GroupIconResource categoriesFor: #initialize!public! !
!GroupIconResource categoriesFor: #newheaderEntryType!*-in class package!private! !
!GroupIconResource categoriesFor: #offset!accessing!public! !
!GroupIconResource categoriesFor: #offset:!accessing!public! !
!GroupIconResource categoriesFor: #packedResourceBytes!private! !
!GroupIconResource categoriesFor: #updateEntryResources:!*-in class package!private! !
!GroupIconResource categoriesFor: #updateGroupResourceIn:withGroup:!*-in class package!private! !
!GroupIconResource categoriesFor: #updateResource:!*-in class package!public! !

!GroupIconResource class methodsFor!

fromResourceBytes: bytes id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	| header resource |
	header := NEWHEADER fromAddress: bytes yourAddress.
	resource := self id: aResourceId locale: aLocale.
	header binresdirEntries do: 
			[:entry | 
			resource addEntry: entry copy
				image: (self resourceEntryClass 
						fromId: entry IconCursorId
						locale: aLocale
						in: anExternalLibraryOrHandle)].
	^resource!

fromResourceFileBytes: bytes in: aFilename 
	| header resource |
	header := NEWHEADER fromAddress: bytes yourAddress.
	resource := self id: (File splitFilenameFrom: aFilename) locale: nil.
	header fileresdirEntries do: 
			[:entry | 
			| resourceBytes |
			resourceBytes := header resEntryBytes: entry.
			resource addEntry: entry copy
				image: (self resourceEntryClass fromResourceFileBytes: resourceBytes in: aFilename)].
	^resource!

resourceEntryClass 
^IconResource 
							!

resourceType
	^RT_GROUP_ICON! !
!GroupIconResource class categoriesFor: #fromResourceBytes:id:locale:in:!*-in class package!private! !
!GroupIconResource class categoriesFor: #fromResourceFileBytes:in:!*-in class package!private! !
!GroupIconResource class categoriesFor: #resourceEntryClass!*-in class package!constants!private! !
!GroupIconResource class categoriesFor: #resourceType!*-in class package!private! !

StringTableResource guid: (GUID fromString: '{64AF8626-A821-4524-9079-030F90362FBD}')!
StringTableResource comment: ''!
!StringTableResource categoriesForClass!Unclassified! !
!StringTableResource methodsFor!

at: anInteger 
	^entries at: anInteger + 1!

at: anInteger put: aString 
	^entries at: anInteger + 1 put: aString!

packedResourceBytes
	| resourceBytesStream |
	resourceBytesStream := ReadWriteStream on: ByteArray new.
	entries do: 
			[:entry | 
			| bytes |
			bytes := entry asUnicodeString asByteArray.
			resourceBytesStream
				nextPutAll: ((ByteArray new: 2)
							wordAtOffset: 0 put: bytes size // 2;
							yourself);
				nextPutAll: bytes	"The ByteArray construct above could be replaced with #nextDWORDPut:"].
	^resourceBytesStream contents!

setIdentifier: aResourceId locale: aLocale entries: anArray 
	identifier := aResourceId.
	locale := aLocale.
	entries := anArray! !
!StringTableResource categoriesFor: #at:!public! !
!StringTableResource categoriesFor: #at:put:!*-in class package!public! !
!StringTableResource categoriesFor: #packedResourceBytes!*-in class package!private! !
!StringTableResource categoriesFor: #setIdentifier:locale:entries:!private! !

!StringTableResource class methodsFor!

empty
	^self new 
		setIdentifier: nil
		locale: nil
		entries: (Array new: 16 withAll: '')!

fromResourceBytes: bytes id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	| entryBytesStream resourceByteStream entries |
	entryBytesStream := ReadWriteStream on: Array new.
	resourceByteStream := ReadStream on: bytes.
	[resourceByteStream atEnd] 
		whileFalse: [entryBytesStream nextPut: (resourceByteStream next: resourceByteStream nextWORD * 2)].
	entries := entryBytesStream contents 
				collect: [:each | (UnicodeString fromAddress: each yourAddress length: each size / 2) asString].
	^self new 
		setIdentifier: aResourceId
		locale: aLocale
		entries: entries !

fromResourceFileBytes: bytes in: aFilename 
	self shouldNotImplement!

resourceType
	^RT_STRING! !
!StringTableResource class categoriesFor: #empty!*-in class package!public! !
!StringTableResource class categoriesFor: #fromResourceBytes:id:locale:in:!*-in class package!private! !
!StringTableResource class categoriesFor: #fromResourceFileBytes:in:!private! !
!StringTableResource class categoriesFor: #resourceType!private! !

BinaryResource guid: (GUID fromString: '{8C6D8393-41EF-4208-B372-103A48F67FEE}')!
BinaryResource comment: ''!
!BinaryResource categoriesForClass!Unclassified! !
!BinaryResource class methodsFor!

resourceType
	^RT_RCDATA! !
!BinaryResource class categoriesFor: #resourceType!private! !

BitmapResource guid: (GUID fromString: '{1EF56875-D021-457D-8930-7DC09C1028BC}')!
BitmapResource comment: ''!
!BitmapResource categoriesForClass!Unclassified! !
!BitmapResource class methodsFor!

fromResourceFileBytes: bytes in: aFilename 
	^self new 
		setIdentifier: (File splitFilenameFrom: aFilename)
		locale: nil
		bytes: (bytes copyFrom: 15)!

resourceType
	^RT_BITMAP! !
!BitmapResource class categoriesFor: #fromResourceFileBytes:in:!private! !
!BitmapResource class categoriesFor: #resourceType!private! !

IconResource guid: (GUID fromString: '{51F58F25-F30A-4105-887F-6EE444F21458}')!
IconResource comment: ''!
!IconResource categoriesForClass!Unclassified! !
!IconResource class methodsFor!

resourceType
	^RT_ICON! !
!IconResource class categoriesFor: #resourceType!*-in class package!private! !

UserResource guid: (GUID fromString: '{8A7CBA4A-8635-416B-8C09-A5D98AAD658A}')!
UserResource comment: ''!
!UserResource categoriesForClass!Unclassified! !
!UserResource methodsFor!

resourceType
	^self type!

setIdentifier: aResourceId locale: aLocale bytes: aByteArray 
^self setType: self class resourceType identifier: aResourceId locale: aLocale bytes: aByteArray !

setType: aResourceType identifier: aResourceId locale: aLocale bytes: aByteArray 
	type := aResourceType.
	identifier := aResourceId.
	locale := aLocale.
	bytes := aByteArray!

type
	^type!

type: anObject 

	type := anObject! !
!UserResource categoriesFor: #resourceType!public! !
!UserResource categoriesFor: #setIdentifier:locale:bytes:!private! !
!UserResource categoriesFor: #setType:identifier:locale:bytes:!private! !
!UserResource categoriesFor: #type!accessing!public! !
!UserResource categoriesFor: #type:!*-in class package!accessing!public! !

CursorResource guid: (GUID fromString: '{BF354619-CAF3-4052-A0D8-846C176EC041}')!
CursorResource comment: ''!
!CursorResource categoriesForClass!Unclassified! !
!CursorResource methodsFor!

setIdentifier: aResourceId locale: aLocale bytes: aByteArray 

	^super 
		setIdentifier: aResourceId
		locale: aLocale
		bytes: aByteArray! !
!CursorResource categoriesFor: #setIdentifier:locale:bytes:!*-in class package!private! !

!CursorResource class methodsFor!

resourceType
	^RT_CURSOR! !
!CursorResource class categoriesFor: #resourceType!*-in class package!private! !

GroupCursorResource guid: (GUID fromString: '{C0647204-4A42-4A46-B26A-AE6A461294F0}')!
GroupCursorResource comment: ''!
!GroupCursorResource categoriesForClass!Unclassified! !
!GroupCursorResource methodsFor!

newheaderEntryType
	^RES_CURSOR! !
!GroupCursorResource categoriesFor: #newheaderEntryType!*-in class package!private! !

!GroupCursorResource class methodsFor!

fixedEntry: aFILERESDIR withFileBytes: aByteArray 
^aFILERESDIR fixedUsing: aByteArray!

fixedFileBytes: aByteArray withEntry: aFILERESDIR 
	| fixedBytes |
	
	fixedBytes := #[0 0 0 0] , aByteArray.
	fixedBytes
		wordAtOffset: 0 put: aFILERESDIR xHotSpot;
		wordAtOffset: 2 put: aFILERESDIR yHotSpot.
	^fixedBytes!

fromResourceFileBytes: bytes in: aFilename 
	| header resource |
	header := NEWHEADER fromAddress: bytes yourAddress.
	resource := self id: (File splitFilenameFrom: aFilename) locale: nil.
	header fileresdirEntries do: 
			[:entry | 
			| resourceBytes fixedEntry fixedResourceBytes |
			resourceBytes := header resEntryBytes: entry.
			fixedResourceBytes := self fixedFileBytes: resourceBytes withEntry: entry.
			fixedEntry := self fixedEntry: entry withFileBytes: resourceBytes.
			resource addEntry: fixedEntry
				image: (self resourceEntryClass fromResourceFileBytes: fixedResourceBytes in: aFilename)].
	^resource!

resourceEntryClass
	^CursorResource!

resourceType
	^RT_GROUP_CURSOR! !
!GroupCursorResource class categoriesFor: #fixedEntry:withFileBytes:!private! !
!GroupCursorResource class categoriesFor: #fixedFileBytes:withEntry:!*-in class package!private! !
!GroupCursorResource class categoriesFor: #fromResourceFileBytes:in:!*-in class package!private! !
!GroupCursorResource class categoriesFor: #resourceEntryClass!*-in class package!constants!private! !
!GroupCursorResource class categoriesFor: #resourceType!*-in class package!private! !

CURSORDIR guid: (GUID fromString: '{7940710A-59D7-48D0-A46C-6B8E09836C90}')!
CURSORDIR comment: ''!
!CURSORDIR categoriesForClass!Unclassified! !
!CURSORDIR methodsFor!

extent
^self Width@self Height!

extent: aPoint

	self Width: aPoint x; Height: aPoint y.
	^aPoint!

Height
	"Answer the receiver's Height field as a Smalltalk object."

	^(bytes wordAtOffset: 2)!

Height: anObject
	"Set the receiver's Height field to the value of anObject."

	bytes wordAtOffset: 2 put: anObject!

Width
	"Answer the receiver's Width field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

Width: anObject
	"Set the receiver's Width field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject! !
!CURSORDIR categoriesFor: #extent!public! !
!CURSORDIR categoriesFor: #extent:!public! !
!CURSORDIR categoriesFor: #Height!**compiled accessors**!public! !
!CURSORDIR categoriesFor: #Height:!**compiled accessors**!public! !
!CURSORDIR categoriesFor: #Width!**compiled accessors**!public! !
!CURSORDIR categoriesFor: #Width:!**compiled accessors**!public! !

!CURSORDIR class methodsFor!

defineFields
	"Define the fields of the CURSORDIR structure (see http://msdn2.microsoft.com/en-us/library/ms648011(VS.85).aspx).
	
	CURSORDIR compileDefinition

	struct CURSORDIR {
		WORD Width;
		WORD Height;
	} CURSORDIR;
	"

	self
		defineField: #Width type: WORDField new;
		defineField: #Height type: WORDField new.
	! !
!CURSORDIR class categoriesFor: #defineFields!constants!public! !

CURSORIMAGE guid: (GUID fromString: '{32D499B3-520D-4226-9DC9-EE5E11C5CF5C}')!
CURSORIMAGE comment: ''!
!CURSORIMAGE categoriesForClass!Unclassified! !
!CURSORIMAGE methodsFor!

asIcon
	| handle |
	#USToDo.
	"Switch to CreateIcon() here. This will allow more flexible mask settings where mask,dib and colors are references and not part of the bytes"
	handle := UserLibrary default 
				createIconFromResource: bytes
				dwResSize: bytes size
				fIcon: true
				dwVer: 16r00030000.
	^Icon fromOwnedHandle: handle!

icAND
	| start size |
	start := self icHeader byteSize + self icColors  byteSize + self icXOR byteSize.
	size := (self widthInBytes: self icHeader biWidth) * (self icHeader biHeight / 2).
	^bytes copyFrom: start + 1 to: start + size!

icANDDIB
	| infoHeader info hBitmap |
	infoHeader := self icHeader copy.
	infoHeader
		biHeight: infoHeader biHeight / 2;
		biBitCount: 1.
	info := BITMAPINFO colorDepth: 1.
	info bmiHeader: infoHeader.
	info bmiColors at: 1 put: (RGBQUAD fromColor: Color white). #USToDo. "Which color is white? 0 or 1?"
	hBitmap := GDILibrary default 
				createDIBitmap: UserLibrary default getDC
				lpbmih: infoHeader
				fdwInit: CBM_INIT
				lpbInit: self icAND asParameter
				lpbmi: info
				fuUsage: DIB_RGB_COLORS.
	^DIBSection fromOwnedHandle: hBitmap!

icColors
	"Answer the receiver's icColors field as a Smalltalk object."

	^StructureArray fromAddress: (bytes yourAddress + 44) length: self numColors elementClass: RGBQUAD!

icColors: anObject
	"Set the receiver's icColors field to the value of anObject."

	| size |
	size := anObject byteSize min: (self numColors * 4).
	anObject replaceBytesOf: bytes from: 45 to: 44 + size startingAt: 1!

icHeader
	"Answer the receiver's icHeader field as a Smalltalk object."

	^BITMAPINFOHEADER fromAddress: (bytes yourAddress + 4)!

icHeader: anObject
	"Set the receiver's icHeader field to the value of anObject."

	anObject replaceBytesOf: bytes from: 5 to: 44 startingAt: 1!

icXOR
	| start size |
	start := self icHeader byteSize + self icColors byteSize.
	size := (self widthInBytes: self icHeader biWidth * self icHeader biBitCount) 
				* (self icHeader biHeight / 2).
	"size := self icHeader biSizeImage."
	^bytes copyFrom: start + 1 to: start + size!

icXORDIB
	| infoHeader info hBitmap |
	infoHeader := self icHeader copy.
	infoHeader biHeight: infoHeader biHeight / 2.
	info := BITMAPINFO colorDepth: infoHeader biBitCount.
	info
		bmiHeader: infoHeader.
		self numColors >0 ifTrue: [
		info bmiColors: self icColors].
	hBitmap := GDILibrary default 
				createDIBitmap: UserLibrary default getDC
				lpbmih: infoHeader
				fdwInit: CBM_INIT
				lpbInit: self icXOR asParameter
				lpbmi: info
				fuUsage: DIB_RGB_COLORS.
	^DIBSection fromOwnedHandle: hBitmap!

lengthAndMask
	| ich |
	ich := self icHeader.
	^ich biHeight * (self widthInBytes: ich biWidth)!

lengthXorMask
	| ich |
	ich := self icHeader.
	
	^ich biHeight * (self widthInBytes: ich biWidth * ich biPlanes * ich biBitCount)!

numColors
	^self icHeader biBitCount < 16 
		ifTrue: [2 raisedTo: self icHeader biBitCount]
		ifFalse: 
			[
			0]!

widthInBytes: anInteger 
	"How wide, in bytes, would this many bits be, DWORD aligned?
	
	#define WIDTHBYTES(bits)      ((((bits) + 31)>>5)<<2)"
^anInteger + 31 >> 5 <<2
	!

xHotSpot
	"Answer the receiver's xHotSpot field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

xHotSpot: anObject
	"Set the receiver's xHotSpot field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject!

yHotSpot
	"Answer the receiver's yHotSpot field as a Smalltalk object."

	^(bytes wordAtOffset: 2)!

yHotSpot: anObject
	"Set the receiver's yHotSpot field to the value of anObject."

	bytes wordAtOffset: 2 put: anObject! !
!CURSORIMAGE categoriesFor: #asIcon!*-in class package!public! !
!CURSORIMAGE categoriesFor: #icAND!*-in class package!public! !
!CURSORIMAGE categoriesFor: #icANDDIB!*-in class package!public! !
!CURSORIMAGE categoriesFor: #icColors!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #icColors:!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #icHeader!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #icHeader:!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #icXOR!*-in class package!public! !
!CURSORIMAGE categoriesFor: #icXORDIB!*-in class package!public! !
!CURSORIMAGE categoriesFor: #lengthAndMask!private! !
!CURSORIMAGE categoriesFor: #lengthXorMask!private! !
!CURSORIMAGE categoriesFor: #numColors!*-in class package!private! !
!CURSORIMAGE categoriesFor: #widthInBytes:!private! !
!CURSORIMAGE categoriesFor: #xHotSpot!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #xHotSpot:!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #yHotSpot!**compiled accessors**!public! !
!CURSORIMAGE categoriesFor: #yHotSpot:!**compiled accessors**!public! !

!CURSORIMAGE class methodsFor!

defineFields
	"Define the fields of the CURSORIMAGE structure.
		CURSORIMAGE compileDefinition

	typdef struct
	{
		DWORD xHotSpot;
		DWORD yHotSpot;
		BITMAPINFOHEADER   icHeader;      // DIB header
		RGBQUAD         icColors[1];   // Color table
		BYTE            icXOR[1];      // DIB bits for XOR mask
		BYTE            icAND[1];      // DIB bits for AND mask
	} CURSORIMAGE, *LPCURSORIMAGE;"

	self
		defineField: #xHotSpot type: WORDField new;
		defineField: #yHotSpot type: WORDField new;
		defineField: #icHeader type: (StructureField type: BITMAPINFOHEADER);
		defineField: #icColors type: (VariableStructureArrayField type: RGBQUAD length: #numColors);
		defineField: #icXOR type: (FillerField byteSize: 4);
		defineField: #icAND type: (FillerField byteSize: 4)!

xor: xorDIB and: andDIB 
	| header xorBytes andBytes iconBytes colorBytes |
	self assert: [xorDIB extent = andDIB extent].
	self assert: [andDIB depth = 1].
	header := xorDIB getBITMAPINFOHEADER copy.
	header biHeight: header biHeight * 2.
	xorBytes := ByteArray fromAddress: xorDIB imageBits
				length: xorDIB getInfo bmHeight * xorDIB getInfo bmWidthBytes.
	colorBytes := xorDIB getColorTable ifNil: [#[]] ifNotNil: [:value | value bytes].
	andBytes := ByteArray fromAddress: andDIB imageBits
				length: andDIB getInfo bmHeight * andDIB getInfo bmWidthBytes.
	iconBytes := header bytes , colorBytes  , xorBytes , andBytes.
	^(self new: iconBytes size) bytes: iconBytes! !
!CURSORIMAGE class categoriesFor: #defineFields!*-in class package!constants!public! !
!CURSORIMAGE class categoriesFor: #xor:and:!*-in class package!public! !

ICONIMAGE guid: (GUID fromString: '{52CF236C-7E03-42C9-BB03-85DE74CCF8FD}')!
ICONIMAGE comment: ''!
!ICONIMAGE categoriesForClass!Unclassified! !
!ICONIMAGE methodsFor!

asIcon
	| handle |
	#USToDo.
	"Switch to CreateIcon() here. This will allow more flexible mask settings where mask,dib and colors are references and not part of the bytes"
	handle := UserLibrary default 
				createIconFromResource: bytes
				dwResSize: bytes size
				fIcon: true
				dwVer: 16r00030000.
	^Icon fromOwnedHandle: handle!

icAND
	| start size |
	start := self icHeader byteSize + self icColors  byteSize + self icXOR byteSize.
	size := (self widthInBytes: self icHeader biWidth) * (self icHeader biHeight / 2).
	^bytes copyFrom: start + 1 to: start + size!

icANDDIB
	| infoHeader info hBitmap |
	infoHeader := self icHeader copy.
	infoHeader
		biHeight: infoHeader biHeight / 2;
		biBitCount: 1.
	info := BITMAPINFO colorDepth: 1.
	info bmiHeader: infoHeader.
	info bmiColors at: 1 put: (RGBQUAD fromColor: Color white). #USToDo. "Which color is white? 0 or 1?"
	hBitmap := GDILibrary default 
				createDIBitmap: UserLibrary default getDC
				lpbmih: infoHeader
				fdwInit: CBM_INIT
				lpbInit: self icAND asParameter
				lpbmi: info
				fuUsage: DIB_RGB_COLORS.
	^DIBSection fromOwnedHandle: hBitmap!

icColors
	"Answer the receiver's icColors field as a Smalltalk object."

	
	^StructureArray 
		fromAddress: bytes yourAddress + 40
		length: self numColors
		elementClass: RGBQUAD!

icColors: anObject
	"Set the receiver's icColors field to the value of anObject."

	| size |
	size := anObject byteSize min: (self numColors * 4).
	anObject replaceBytesOf: bytes from: 41 to: 40 + size startingAt: 1!

icHeader
	"Answer the receiver's icHeader field as a Smalltalk object."

	^BITMAPINFOHEADER fromAddress: (bytes yourAddress)!

icHeader: anObject
	"Set the receiver's icHeader field to the value of anObject."

	anObject replaceBytesOf: bytes from: 1 to: 40 startingAt: 1!

icXOR
	| start size |
	start := self icHeader byteSize + self icColors byteSize.
	size := (self widthInBytes: self icHeader biWidth * self icHeader biBitCount) 
				* (self icHeader biHeight / 2).
	"size := self icHeader biSizeImage."
	^bytes copyFrom: start + 1 to: start + size!

icXORDIB
	| infoHeader info hBitmap |
	infoHeader := self icHeader copy.
	infoHeader biHeight: infoHeader biHeight / 2.
	info := BITMAPINFO colorDepth: infoHeader biBitCount.
	info
		bmiHeader: infoHeader.
		self numColors >0 ifTrue: [
		info bmiColors: self icColors].
	hBitmap := GDILibrary default 
				createDIBitmap: UserLibrary default getDC
				lpbmih: infoHeader
				fdwInit: CBM_INIT
				lpbInit: self icXOR asParameter
				lpbmi: info
				fuUsage: DIB_RGB_COLORS.
	^DIBSection fromOwnedHandle: hBitmap!

lengthAndMask
	| ich |
	ich := self icHeader.
	^ich biHeight * (self widthInBytes: ich biWidth)!

lengthXorMask
	| ich |
	ich := self icHeader.
	
	^ich biHeight * (self widthInBytes: ich biWidth * ich biPlanes * ich biBitCount)!

numColors
	^self icHeader biBitCount < 16 
		ifTrue: [2 raisedTo: self icHeader biBitCount]
		ifFalse: 
			[
			0]!

widthInBytes: anInteger 
	"How wide, in bytes, would this many bits be, DWORD aligned?
	
	#define WIDTHBYTES(bits)      ((((bits) + 31)>>5)<<2)"
^anInteger + 31 >> 5 <<2
	! !
!ICONIMAGE categoriesFor: #asIcon!*-in class package!public! !
!ICONIMAGE categoriesFor: #icAND!*-in class package!public! !
!ICONIMAGE categoriesFor: #icANDDIB!*-in class package!public! !
!ICONIMAGE categoriesFor: #icColors!**compiled accessors**!*-in class package!public! !
!ICONIMAGE categoriesFor: #icColors:!**compiled accessors**!public! !
!ICONIMAGE categoriesFor: #icHeader!**compiled accessors**!public! !
!ICONIMAGE categoriesFor: #icHeader:!**compiled accessors**!public! !
!ICONIMAGE categoriesFor: #icXOR!*-in class package!public! !
!ICONIMAGE categoriesFor: #icXORDIB!*-in class package!public! !
!ICONIMAGE categoriesFor: #lengthAndMask!private! !
!ICONIMAGE categoriesFor: #lengthXorMask!private! !
!ICONIMAGE categoriesFor: #numColors!*-in class package!private! !
!ICONIMAGE categoriesFor: #widthInBytes:!private! !

!ICONIMAGE class methodsFor!

defineFields
	"Define the fields of the ICONIMAGE structure.
		ICONIMAGE compileDefinition

	typdef struct
	{
		BITMAPINFOHEADER   icHeader;      // DIB header
		RGBQUAD         icColors[1];   // Color table
		BYTE            icXOR[1];      // DIB bits for XOR mask
		BYTE            icAND[1];      // DIB bits for AND mask
	} ICONIMAGE, *LPICONIMAGE;"

	self
		defineField: #icHeader type: (StructureField type: BITMAPINFOHEADER);
		defineField: #icColors type: (VariableStructureArrayField type: RGBQUAD length: #numColors);
		
			defineField: #icXOR type: (FillerField byteSize: 4);
			defineField: #icAND type: (FillerField byteSize: 4)!

xor: xorDIB and: andDIB 
	| header xorBytes andBytes iconBytes colorBytes |
	self assert: [xorDIB extent = andDIB extent].
	self assert: [andDIB depth = 1].
	header := xorDIB getBITMAPINFOHEADER copy.
	header biHeight: header biHeight * 2.
	xorBytes := ByteArray fromAddress: xorDIB imageBits
				length: xorDIB getInfo bmHeight * xorDIB getInfo bmWidthBytes.
	colorBytes := xorDIB getColorTable ifNil: [#[]] ifNotNil: [:value | value bytes].
	andBytes := ByteArray fromAddress: andDIB imageBits
				length: andDIB getInfo bmHeight * andDIB getInfo bmWidthBytes.
	iconBytes := header bytes , colorBytes  , xorBytes , andBytes.
	^(self new: iconBytes size) bytes: iconBytes! !
!ICONIMAGE class categoriesFor: #defineFields!*-in class package!constants!public! !
!ICONIMAGE class categoriesFor: #xor:and:!*-in class package!public! !

ICONRESDIR guid: (GUID fromString: '{F3F3AE91-4E08-4535-B553-06B5BF11BE53}')!
ICONRESDIR comment: ''!
!ICONRESDIR categoriesForClass!Unclassified! !
!ICONRESDIR methodsFor!

ColorCount
	"Answer the receiver's ColorCount field as a Smalltalk object."

	^(bytes byteAtOffset: 2)!

ColorCount: anObject
	"Set the receiver's ColorCount field to the value of anObject."

	bytes byteAtOffset: 2 put: anObject!

extent
^self Width@self Height!

extent: aPoint

	self Width: aPoint x; Height: aPoint y.
	^aPoint!

Height
	"Answer the receiver's Height field as a Smalltalk object."

	^(bytes byteAtOffset: 1)!

Height: anObject
	"Set the receiver's Height field to the value of anObject."

	bytes byteAtOffset: 1 put: anObject!

Width
	"Answer the receiver's Width field as a Smalltalk object."

	^(bytes byteAtOffset: 0)!

Width: anObject
	"Set the receiver's Width field to the value of anObject."

	bytes byteAtOffset: 0 put: anObject! !
!ICONRESDIR categoriesFor: #ColorCount!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #ColorCount:!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #extent!public! !
!ICONRESDIR categoriesFor: #extent:!public! !
!ICONRESDIR categoriesFor: #Height!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #Height:!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #Width!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #Width:!**compiled accessors**!public! !

!ICONRESDIR class methodsFor!

defineFields
	"Define the fields of the ICONRESDIR structure (see http://msdn2.microsoft.com/en-us/library/ms648016(VS.85).aspx).
	
	ICONRESDIR compileDefinition

	struct ICONRESDIR { 
		BYTE Width;
		BYTE Height;
		BYTE ColorCount;
		BYTE reserved;
	} ICONRESDIR; 
	"

	self
		defineField: #Width type: BYTEField new;
		defineField: #Height type: BYTEField new;
		defineField: #ColorCount type: BYTEField new;
		defineField: #reserved type: (FillerField byteSize: 1)! !
!ICONRESDIR class categoriesFor: #defineFields!constants!public! !

NEWHEADER guid: (GUID fromString: '{7F441582-BEC8-4358-BCCB-3626975E75BE}')!
NEWHEADER comment: ''!
!NEWHEADER categoriesForClass!Unclassified! !
!NEWHEADER methodsFor!

binresdirEntries
^self ResEntries: BINRESDIR!

fileresdirEntries
	^self ResEntries: FILERESDIR!

ResCount
	"Answer the receiver's ResCount field as a Smalltalk object."

	^(bytes wordAtOffset: 4)!

ResCount: anObject
	"Set the receiver's ResCount field to the value of anObject."

	bytes wordAtOffset: 4 put: anObject!

ResEntries: anEntryClass
^(StructureArray 
		fromAddress: self bytes yourAddress + self byteSize
		length: self ResCount
		elementClass: anEntryClass)!

resEntryBytes: aFILERESDIR 
	^ByteArray fromAddress: self bytes yourAddress + aFILERESDIR IconCursorOffset
		length: aFILERESDIR BytesInRes!

ResType
	"Answer the receiver's ResType field as a Smalltalk object."

	^(bytes wordAtOffset: 2)!

ResType: anObject
	"Set the receiver's ResType field to the value of anObject."

	bytes wordAtOffset: 2 put: anObject! !
!NEWHEADER categoriesFor: #binresdirEntries!public! !
!NEWHEADER categoriesFor: #fileresdirEntries!public! !
!NEWHEADER categoriesFor: #ResCount!**compiled accessors**!public! !
!NEWHEADER categoriesFor: #ResCount:!**compiled accessors**!public! !
!NEWHEADER categoriesFor: #ResEntries:!public! !
!NEWHEADER categoriesFor: #resEntryBytes:!public! !
!NEWHEADER categoriesFor: #ResType!**compiled accessors**!public! !
!NEWHEADER categoriesFor: #ResType:!**compiled accessors**!public! !

!NEWHEADER class methodsFor!

defineFields
	"Define the fields of the NEWHEADER structure (see http://msdn2.microsoft.com/en-us/library/ms648026(VS.85).aspx).
	
	NEWHEADER compileDefinition

	struct NEWHEADER { 
		WORD Reserved;
		WORD ResType;
		WORD ResCount;
	} NEWHEADER, *PNEWHEADER;
	"

	self
		defineField: #Reserved type: (FillerField byteSize: 2);
		defineField: #ResType type: WORDField new;
		defineField: #ResCount type: WORDField new! !
!NEWHEADER class categoriesFor: #defineFields!constants!public! !

RESDIR guid: (GUID fromString: '{E41F6B74-0391-4600-B8A1-6895390BA0FE}')!
RESDIR comment: ''!
!RESDIR categoriesForClass!Unclassified! !
!RESDIR methodsFor!

<= aRESDIR 
self BitCount = aRESDIR BitCount ifFalse: [^self BitCount <= aRESDIR BitCount].
	^self Icon Width > aRESDIR Icon Width
	!

asBINRESDIR
	self subclassResponsibility!

asFILERESDIR
	self subclassResponsibility!

BitCount
	"Answer the receiver's BitCount field as a Smalltalk object."

	^(bytes wordAtOffset: 6)!

BitCount: anObject
	"Set the receiver's BitCount field to the value of anObject."

	bytes wordAtOffset: 6 put: anObject!

BytesInRes
	"Answer the receiver's BytesInRes field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

BytesInRes: anObject
	"Set the receiver's BytesInRes field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

Cursor
	"Answer the receiver's Cursor field as a Smalltalk object."

	^CURSORDIR fromAddress: (bytes yourAddress)!

Cursor: anObject
	"Set the receiver's Cursor field to the value of anObject."

	anObject replaceBytesOf: bytes from: 1 to: 4 startingAt: 1!

hotspot
^self xHotSpot @ self yHotSpot!

hotspot: aPoint
self xHotSpot:  aPoint x; yHotSpot: aPoint y.
^aPoint!

Icon
	"Answer the receiver's Icon field as a Smalltalk object."

	^ICONRESDIR fromAddress: (bytes yourAddress)!

Icon: anObject
	"Set the receiver's Icon field to the value of anObject."

	anObject replaceBytesOf: bytes from: 1 to: 4 startingAt: 1!

Planes
	"Answer the receiver's Planes field as a Smalltalk object."

	^(bytes wordAtOffset: 4)!

Planes: anObject
	"Set the receiver's Planes field to the value of anObject."

	bytes wordAtOffset: 4 put: anObject!

xHotSpot
	"Answer the receiver's xHotSpot field as a Smalltalk object."

	^(bytes wordAtOffset: 4)!

xHotSpot: anObject
	"Set the receiver's xHotSpot field to the value of anObject."

	bytes wordAtOffset: 4 put: anObject!

yHotSpot
	"Answer the receiver's yHotSpot field as a Smalltalk object."

	^(bytes wordAtOffset: 6)!

yHotSpot: anObject
	"Set the receiver's yHotSpot field to the value of anObject."

	bytes wordAtOffset: 6 put: anObject! !
!RESDIR categoriesFor: #<=!public! !
!RESDIR categoriesFor: #asBINRESDIR!public! !
!RESDIR categoriesFor: #asFILERESDIR!public! !
!RESDIR categoriesFor: #BitCount!**compiled accessors**!public! !
!RESDIR categoriesFor: #BitCount:!**compiled accessors**!public! !
!RESDIR categoriesFor: #BytesInRes!**compiled accessors**!public! !
!RESDIR categoriesFor: #BytesInRes:!**compiled accessors**!public! !
!RESDIR categoriesFor: #Cursor!**compiled accessors**!public! !
!RESDIR categoriesFor: #Cursor:!**compiled accessors**!public! !
!RESDIR categoriesFor: #hotspot!public! !
!RESDIR categoriesFor: #hotspot:!public! !
!RESDIR categoriesFor: #Icon!**compiled accessors**!public! !
!RESDIR categoriesFor: #Icon:!**compiled accessors**!public! !
!RESDIR categoriesFor: #Planes!**compiled accessors**!public! !
!RESDIR categoriesFor: #Planes:!**compiled accessors**!public! !
!RESDIR categoriesFor: #xHotSpot!**compiled accessors**!public! !
!RESDIR categoriesFor: #xHotSpot:!**compiled accessors**!public! !
!RESDIR categoriesFor: #yHotSpot!**compiled accessors**!public! !
!RESDIR categoriesFor: #yHotSpot:!**compiled accessors**!public! !

!RESDIR class methodsFor!

alignment
	"Answer the natural alignment for instances of the receiver when 
	embedded in arrays or other structures. This is the natural alignment 
	of the largest field in the receiver. The actual alignment used may
	be different if the structure packing overrides it."

^2!

defineFields
	"Define the fields of the RESDIR structure.
	
	RESDIR compileDefinition
	"

	self
		defineField: #Icon
			type: (StructureField type: ICONRESDIR)
			offset: 0;
		defineField: #Cursor
			type: (StructureField type: CURSORDIR)
			offset: 0;
		defineField: #Planes
			type: WORDField new
			offset: 4; defineField: #xHotSpot
			type: WORDField new
			offset: 4;
		defineField: #BitCount
			type: WORDField new
			offset: 6; 	defineField: #yHotSpot
			type: WORDField new
			offset: 6;
		defineField: #BytesInRes
			type: DWORDField new
			offset: 8!

packing
	"Answer the default packing for instances of the receiver."

	^2! !
!RESDIR class categoriesFor: #alignment!constants!public! !
!RESDIR class categoriesFor: #defineFields!constants!public! !
!RESDIR class categoriesFor: #packing!public! !

BINRESDIR guid: (GUID fromString: '{216201B6-B087-43C6-9286-44FA9C25FD1C}')!
BINRESDIR comment: ''!
!BINRESDIR categoriesForClass!Unclassified! !
!BINRESDIR methodsFor!

asBINRESDIR
	^self!

asFILERESDIR
	^FILERESDIR fromBytes: self bytes , #[0 0]!

IconCursorId
	"Answer the receiver's IconCursorId field as a Smalltalk object."

	^(bytes wordAtOffset: 12)!

IconCursorId: anObject
	"Set the receiver's IconCursorId field to the value of anObject."

	bytes wordAtOffset: 12 put: anObject! !
!BINRESDIR categoriesFor: #asBINRESDIR!public! !
!BINRESDIR categoriesFor: #asFILERESDIR!public! !
!BINRESDIR categoriesFor: #IconCursorId!**compiled accessors**!public! !
!BINRESDIR categoriesFor: #IconCursorId:!**compiled accessors**!public! !

!BINRESDIR class methodsFor!

defineFields
	"Define the fields of the BINRESDIR structure (see http://msdn2.microsoft.com/en-us/library/ms648026(VS.85).aspx).
	
	BINRESDIR compileDefinition

	typedef struct tagBINRESDIR {
		union {
			ICONRESDIR   Icon;
			CURSORDIR    Cursor;
		} ResInfo;
		WORD    Planes;
		WORD    BitCount;
		DWORD   BytesInRes;
		WORD  IconCursorId;
	} BINRESDIR;
	"

	super defineFields.
	self 
		defineField: #IconCursorId
		type: WORDField new
		offset: 12! !
!BINRESDIR class categoriesFor: #defineFields!constants!public! !

FILERESDIR guid: (GUID fromString: '{191FF8B3-BF24-45B0-B789-38F672C7FB9B}')!
FILERESDIR comment: ''!
!FILERESDIR categoriesForClass!Unclassified! !
!FILERESDIR methodsFor!

asBINRESDIR
	^BINRESDIR fromBytes: self bytes!

asFILERESDIR
^self!

IconCursorOffset
	"Answer the receiver's IconCursorOffset field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

IconCursorOffset: anObject
	"Set the receiver's IconCursorOffset field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject! !
!FILERESDIR categoriesFor: #asBINRESDIR!public! !
!FILERESDIR categoriesFor: #asFILERESDIR!public! !
!FILERESDIR categoriesFor: #IconCursorOffset!**compiled accessors**!public! !
!FILERESDIR categoriesFor: #IconCursorOffset:!**compiled accessors**!public! !

!FILERESDIR class methodsFor!

defineFields
	"Define the fields of the FILERESDIR structure.
	
	FILERESDIR compileDefinition

	typedef struct tagFILERESDIR {
		union {
			ICONRESDIR   Icon;
			CURSORDIR    Cursor;
		} ResInfo;
		WORD    Planes;
		WORD    BitCount;
		DWORD   BytesInRes;
		DWORD  IconCursorOffset;
	} FILERESDIR;
	"

	super defineFields.
	self 
		defineField: #IconCursorOffset
		type: DWORDField new
		offset: 12! !
!FILERESDIR class categoriesFor: #defineFields!constants!public! !

"Binary Globals"!

