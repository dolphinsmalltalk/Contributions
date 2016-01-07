| package |
package := Package name: 'US Resource Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Resource Extensions 0.042$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.042'.

package basicScriptAt: #postuninstall put: 'Win32Constants removeKey: ''RT_GROUPICON'''.
package basicScriptAt: #preinstall put: 'Win32Constants
	at: ''RT_CURSOR'' put: 1;
	at: ''RT_GROUP_CURSOR'' put: 12;
	at: ''RT_GROUP_ICON'' put: 14;
	at: ''RT_BITMAP'' put: 2;
	at: ''RES_ICON'' put: 1;
	at: ''RES_CURSOR'' put: 2'.

package classNames
	add: #WindowsResourceIdentifier;
	yourself.

package methodNames
	add: #BITMAPINFOHEADER -> #negateBiHeight;
	add: #DIBSection -> #saveToFile:;
	add: #DIBSection -> #saveToStream:;
	add: #ExternalAddress -> #asDecodedResourceId;
	add: #ExternalAddress -> #isInternalResource;
	add: #ExternalLibrary -> #asExternalResourceLibrary;
	add: #ExternalResourceLibrary -> #allBitmaps;
	add: #ExternalResourceLibrary -> #allGroupCursors;
	add: #ExternalResourceLibrary -> #allGroupIcons;
	add: #ExternalResourceLibrary -> #allOfClass:;
	add: #ExternalResourceLibrary -> #allStrings;
	add: #ExternalResourceLibrary -> #resourceIdentifiersOfName:ofType:;
	add: #ExternalResourceLibrary -> #resourceIdentifiersOfType:;
	add: #ExternalResourceLibrary -> #resourceLanguagesOfName:ofType:do:;
	add: #ExternalResourceLibrary -> #resourceLocalesOfName:ofType:;
	add: #ExternalResourceLibrary -> #resourceNamesOfType:;
	add: #ExternalResourceLibrary -> #resourceNamesOfType:do:;
	add: #ExternalResourceLibrary -> #resourceTypes;
	add: #ExternalResourceLibrary -> #resourceTypesDo:;
	add: #KernelLibrary -> #enumResourceLanguages:lpszType:lpName:lpEnumFunc:lParam:;
	add: #KernelLibrary -> #enumResourceNames:lpszType:lpEnumFunc:lParam:;
	add: #KernelLibrary -> #enumResourceTypes:lpEnumFunc:lParam:;
	add: #KernelLibrary -> #findResourceEx:lpName:lpType:wLanguage:;
	add: #Object -> #resourceType;
	add: #OLEPicture -> #loadFromFile:extent:;
	add: #OLEPicture -> #loadFromInstance:;
	add: #UserLibrary -> #createIconFromResource:dwResSize:fIcon:dwVer:;
	add: #UserLibrary -> #lookupIconIdFromDirectory:ficon:;
	add: 'Bitmap class' -> #defaultResourceType;
	add: 'Bitmap class' -> #fromByteArray:;
	add: 'Bitmap class' -> #fromResourceBytes:;
	add: 'ByteArray class' -> #defaultResourceType;
	add: 'ByteArray class' -> #fromResourceBytes:id:locale:in:usingLocator:type:;
	add: 'Cursor class' -> #defaultResourceEntryType;
	add: 'Cursor class' -> #defaultResourceType;
	add: 'Cursor class' -> #isIcon;
	add: 'GdiplusImage class' -> #defaultResourceType;
	add: 'GdiplusImage class' -> #fromResourceBytes:id:locale:in:usingLocator:type:;
	add: 'Icon class' -> #defaultResourceEntryType;
	add: 'Icon class' -> #defaultResourceType;
	add: 'Icon class' -> #fromResourceBytes:id:locale:in:usingLocator:type:;
	add: 'Icon class' -> #isIcon;
	add: 'Image class' -> #fromId:in:usingLocator:;
	add: 'Image class' -> #fromResourceBytes:;
	add: 'Image class' -> #fromResourceBytes:id:locale:in:usingLocator:type:;
	add: 'Locale class' -> #dolphinResourceDefault;
	add: 'Object class' -> #basicFromId:locale:in:fileLocator:type:;
	add: 'Object class' -> #basicFromResourceFindHandle:id:locale:in:usingLocator:type:;
	add: 'Object class' -> #defaultResourceType;
	add: 'Object class' -> #fromFile:;
	add: 'Object class' -> #fromFile:usingLocator:;
	add: 'Object class' -> #fromFile:usingLocator:type:;
	add: 'Object class' -> #fromId:;
	add: 'Object class' -> #fromId:in:;
	add: 'Object class' -> #fromId:in:type:;
	add: 'Object class' -> #fromId:in:usingLocator:;
	add: 'Object class' -> #fromId:in:usingLocator:type:;
	add: 'Object class' -> #fromId:locale:;
	add: 'Object class' -> #fromId:locale:in:;
	add: 'Object class' -> #fromId:locale:in:type:;
	add: 'Object class' -> #fromId:locale:in:usingLocator:type:;
	add: 'Object class' -> #fromId:type:;
	add: 'Object class' -> #fromResourceBytes:id:locale:in:usingLocator:type:;
	add: 'Object class' -> #fromSystemId:;
	add: 'Object class' -> #loadFromFile:id:locale:in:usingLocator:type:;
	add: 'Object class' -> #loadFromInstanceId:locale:in:usingLocator:type:;
	add: 'OLEPicture class' -> #defaultResourceType;
	add: 'OLEPicture class' -> #fromByteArray:;
	add: 'OLEPicture class' -> #fromIStream:;
	add: 'OLEPicture class' -> #fromResourceBytes:;
	add: 'String class' -> #defaultResourceType;
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
	yourself).

package!

"Class Definitions"!

Object subclass: #WindowsResourceIdentifier
	instanceVariableNames: 'type id locale'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Bitmap class methodsFor!

defaultResourceType
	^##(Win32Constants at: 'RT_BITMAP')!

fromByteArray: aByteArray 
"Skip the first 14 bytes (BITMAPFILEHEADER)"
	^self fromResourceBytes: (aByteArray copyFrom: 15)!

fromResourceBytes: aByteArray 
	| infoHeader info colors hBitmap |
	infoHeader := BITMAPINFOHEADER fromBytes: aByteArray.
	info := BITMAPINFO colorDepth: infoHeader biBitCount.
	info bmiHeader: infoHeader.
	colors := infoHeader biBitCount < 16 
				ifTrue: 
					[StructureArray 
						fromAddress: aByteArray yourAddress + infoHeader byteSize
						length: (2 raisedTo: infoHeader biBitCount)
						elementClass: RGBQUAD]
				ifFalse: [StructureArray length: 0 elementClass: RGBQUAD].
	info bmiColors: colors.
	hBitmap := GDILibrary default 
				createDIBitmap: UserLibrary default getDC
				lpbmih: infoHeader
				fdwInit: CBM_INIT
				lpbInit: aByteArray yourAddress + infoHeader byteSize + colors byteSize
				lpbmi: info
				fuUsage: DIB_RGB_COLORS.
	^self fromOwnedHandle: hBitmap! !
!Bitmap class categoriesFor: #defaultResourceType!*-not in class package!public! !
!Bitmap class categoriesFor: #fromByteArray:!*-not in class package!instance creation!public! !
!Bitmap class categoriesFor: #fromResourceBytes:!*-not in class package!instance creation!public! !

!BITMAPINFOHEADER methodsFor!

negateBiHeight
self biHeight: self biHeight negated! !
!BITMAPINFOHEADER categoriesFor: #negateBiHeight!public! !

!ByteArray class methodsFor!

defaultResourceType
	^##(Win32Constants at: 'RT_RCDATA')!

fromResourceBytes: aByteArray id: anIntegerOrStringResourceId locale: aLocale in: anInstanceHandle usingLocator: aFileLocator type: aResourceType 
	^aByteArray! !
!ByteArray class categoriesFor: #defaultResourceType!public! !
!ByteArray class categoriesFor: #fromResourceBytes:id:locale:in:usingLocator:type:!*-not in class package!instance creation!public! !

!Cursor class methodsFor!

defaultResourceEntryType
	^##(Win32Constants at: 'RT_CURSOR')!

defaultResourceType
	^##(Win32Constants at: 'RT_GROUP_CURSOR')!

isIcon
	^false! !
!Cursor class categoriesFor: #defaultResourceEntryType!public! !
!Cursor class categoriesFor: #defaultResourceType!*-not in class package!public! !
!Cursor class categoriesFor: #isIcon!public!testing! !

!DIBSection methodsFor!

saveToFile: aFilename 

	| stream |
	stream := (FileStream write: aFilename text: false).
	self saveToStream: stream.
	stream close.
	!

saveToStream: aStream 
	| bitmapInfo infoHeader colors fileheader |
	self cacheInfo.
infoHeader := self getBITMAPINFOHEADER negateBiHeight asByteArray.
	colors := self getColorTable ifNil: [#[]] ifNotNil: [:value | value asByteArray].
	bitmapInfo := self getInfo.
	imageBits := ByteArray fromAddress: self imageBits
				length: bitmapInfo bmHeight * bitmapInfo bmWidthBytes.
	"fileheader := BITMAPFILEHEADER new."
	fileheader := ByteArray new: 14 withAll: 0.
	"fileheader bfType: 19778."
	fileheader wordAtOffset: 0 put: 19778.
	"fileheader bfSize: fileheader size + infoHeader size + colors size + imageBits size."
	fileheader dwordAtOffset: 2 put: fileheader size + infoHeader size + colors size + imageBits size.
	"fileheader bfOffBits: fileheader size + infoHeader size + colors size."
	fileheader dwordAtOffset: 10 put: fileheader size + infoHeader size + colors size.
	aStream
		nextPutAll: fileheader;
		nextPutAll: infoHeader;
		nextPutAll: colors;
		nextPutAll: imageBits! !
!DIBSection categoriesFor: #saveToFile:!public! !
!DIBSection categoriesFor: #saveToStream:!public! !

!ExternalAddress methodsFor!

asDecodedResourceId
	^self isInternalResource ifTrue: [String fromAddress: self] ifFalse: [self asInteger]!

isInternalResource
	^self yourAddress >> 16 > 0! !
!ExternalAddress categoriesFor: #asDecodedResourceId!public! !
!ExternalAddress categoriesFor: #isInternalResource!public! !

!ExternalLibrary methodsFor!

asExternalResourceLibrary
	^ExternalResourceLibrary fromHandle:  self handle! !
!ExternalLibrary categoriesFor: #asExternalResourceLibrary!public! !

!ExternalResourceLibrary methodsFor!

allBitmaps
	^self allOfClass: Bitmap!

allGroupCursors
	^self allOfClass: Cursor!

allGroupIcons
	^self allOfClass: Icon!

allOfClass: aClass
	^(self resourceNamesOfType: aClass defaultResourceType) 
		collect: [:eachId | aClass fromId: eachId in: self]!

allStrings
	| stringTables dic |
	stringTables := (self resourceNamesOfType: String defaultResourceType) 
				collect: [:eachId | (Smalltalk at: #StringTableResource) fromId: eachId in: self].
	dic := LookupTable new.
	stringTables do: 
			[:each | 
			0 to: 15
				do: 
					[:index | 
					| bytes |
					bytes := each at: index.
					bytes notEmpty ifTrue: [dic at: (each identifier - 1) * 16 + index put: bytes]]].
	^dic!

resourceIdentifiersOfName: id ofType: type 
	^(self resourceLocalesOfName: id ofType: type) collect: 
			[:locale | 
			WindowsResourceIdentifier 
				type: type
				id: id
				locale: locale]!

resourceIdentifiersOfType: resourceType 
	| identifiers |
	identifiers := Set new.
	self resourceNamesOfType: resourceType
		do: [:type :id | identifiers addAll: (self resourceIdentifiersOfName: id ofType: type)].
	^identifiers!

resourceLanguagesOfName: anIntegerOrStringResourceId ofType: anIntegerOrStringResourceType do: aBlock 
	"The EnumResLangProc function is an application-defined callback function used with
	the EnumResourceLanguages function. It receives resource languages. The
	ENUMRESLANGPROC type defines a pointer to this callback function. EnumResLangProc
	is a placeholder for the application-defined function name.

	BOOL CALLBACK EnumResLangProc(
		HANDLE hModule,
		LPCTSTR lpszType,
		LPCTSTR lpszName,
		WORD wIDLanguage,
		LONG_PTR lParam
		);"

	| callback result |
	callback := ExternalCallback block: 
					[:hModule :lpszType :lpszName :wIDLanguage :lParam | 
					aBlock 
						value: lpszType asDecodedResourceId
						value: lpszName asDecodedResourceId
						value: wIDLanguage.
					true]
				descriptor: (ExternalDescriptor returnType: 'bool' argumentTypes: 'handle lpvoid lpvoid word sdword').
	result := KernelLibrary default 
				enumResourceLanguages: self asParameter
				lpszType: anIntegerOrStringResourceType
				lpName: anIntegerOrStringResourceId
				lpEnumFunc: callback asParameter
				lParam: 0.
	result ifFalse: [self systemError]!

resourceLocalesOfName: aResourceId ofType: aResourceType 
	| locales |
	locales := OrderedCollection new.
	(self resourceTypes includes: aResourceType) 
		ifTrue: 
			[
				
				((self resourceNamesOfType: aResourceType) includes: aResourceId) ifTrue: [
				self 
				resourceLanguagesOfName: aResourceId
				ofType: aResourceType
				do: [:type :id :lcid | locales add: (Locale lcid: lcid)]]].
	^locales!

resourceNamesOfType: aResourceType 
	| names |
	names := OrderedCollection new.
	(self resourceTypes includes: aResourceType)
		ifTrue: [self resourceNamesOfType: aResourceType do: [:type :id | names add: id]].
	^names!

resourceNamesOfType: aResourceType do: aBlock 
	"The EnumResNameProc function is an application-defined callback function
	used with the EnumResourceNames function. It receives resource names. The
	ENUMRESNAMEPROC type defines a pointer to this callback function.
	EnumResNameProc is a placeholder for the application-defined function name. 

	BOOL CALLBACK EnumResNameProc(
		HMODULE hModule,
		LPCTSTR lpszType,
		LPTSTR lpszName,
		LONG_PTR lParam
	);"

	| callback result |
	callback := ExternalCallback block: 
					[:hModule :lpszType :lpszName :lParam | 
					aBlock value: lpszType asDecodedResourceId value: lpszName asDecodedResourceId.
					true]
				descriptor: (ExternalDescriptor returnType: 'bool' argumentTypes: 'handle lpvoid lpvoid sdword').
	result := KernelLibrary default 
				enumResourceNames: self asParameter
				lpszType: aResourceType
				lpEnumFunc: callback asParameter
				lParam: 0.
	result ifFalse: [Notification   signal: 'No resource names found' with: self ]!

resourceTypes
	| types |
	types := OrderedCollection new.
	self resourceTypesDo: [:type | types add: type].
	^types!

resourceTypesDo: aBlock 
	"The EnumResTypeProc function is an application-defined callback
	function used with the EnumResourceTypes function. It receives resource types.
	The ENUMRESTYPEPROC type defines a pointer to this callback function.
	EnumResTypeProc is a placeholder for the application-defined function name. 

	BOOL CALLBACK EnumResTypeProc(
		HMODULE hModule,
		LPTSTR lpszType,
		LONG_PTR lParam
	);"

	| callback result |
	callback := ExternalCallback block: 
					[:hModule :lpszType :lParam | 
					aBlock value: lpszType asDecodedResourceId.
					true]
				descriptor: (ExternalDescriptor returnType: 'bool' argumentTypes: 'handle  lpvoid sdword').
	result := KernelLibrary default 
				enumResourceTypes: self asParameter
				lpEnumFunc: callback asParameter
				lParam: 0.
	result ifFalse: [self systemError]! !
!ExternalResourceLibrary categoriesFor: #allBitmaps!*-not in class package!public! !
!ExternalResourceLibrary categoriesFor: #allGroupCursors!*-not in class package!public! !
!ExternalResourceLibrary categoriesFor: #allGroupIcons!*-not in class package!public! !
!ExternalResourceLibrary categoriesFor: #allOfClass:!*-not in class package!public! !
!ExternalResourceLibrary categoriesFor: #allStrings!*-not in class package!public! !
!ExternalResourceLibrary categoriesFor: #resourceIdentifiersOfName:ofType:!accessing!public! !
!ExternalResourceLibrary categoriesFor: #resourceIdentifiersOfType:!accessing!public! !
!ExternalResourceLibrary categoriesFor: #resourceLanguagesOfName:ofType:do:!private! !
!ExternalResourceLibrary categoriesFor: #resourceLocalesOfName:ofType:!*-not in class package!public! !
!ExternalResourceLibrary categoriesFor: #resourceNamesOfType:!*-not in class package!public! !
!ExternalResourceLibrary categoriesFor: #resourceNamesOfType:do:!private! !
!ExternalResourceLibrary categoriesFor: #resourceTypes!public! !
!ExternalResourceLibrary categoriesFor: #resourceTypesDo:!private! !

!GdiplusImage class methodsFor!

defaultResourceType
	^'GDIPLUSIMAGE'!

fromResourceBytes: aByteArray id: anIntegerOrStringResourceId locale: aLocale in: anInstanceHandle usingLocator: aFileLocator type: aResourceType 
	^self fromByteArray: aByteArray! !
!GdiplusImage class categoriesFor: #defaultResourceType!public! !
!GdiplusImage class categoriesFor: #fromResourceBytes:id:locale:in:usingLocator:type:!*-not in class package!instance creation!public! !

!Icon class methodsFor!

defaultResourceEntryType
	^##(Win32Constants at: 'RT_ICON')!

defaultResourceType
	^##(Win32Constants at: 'RT_GROUP_ICON') !

fromResourceBytes: aByteArray id: anIntegerOrStringResourceId locale: aLocale in: anInstanceHandle usingLocator: aFileLocator type: aResourceType 
	| iconId iconBytes handle |
	iconId := UserLibrary default lookupIconIdFromDirectory: aByteArray ficon: self isIcon.
	iconBytes := ByteArray 
				fromId: iconId
				in: anInstanceHandle
				type: self defaultResourceEntryType.
	handle := UserLibrary default 
				createIconFromResource: iconBytes
				dwResSize: iconBytes size
				fIcon: self isIcon
				dwVer: 16r00030000.
	^(self fromOwnedHandle: handle)
		identifier: anIntegerOrStringResourceId;
		instanceHandle: anInstanceHandle;
		fileLocator: aFileLocator;
		yourself!

isIcon
^true! !
!Icon class categoriesFor: #defaultResourceEntryType!public! !
!Icon class categoriesFor: #defaultResourceType!*-not in class package!public! !
!Icon class categoriesFor: #fromResourceBytes:id:locale:in:usingLocator:type:!*-not in class package!instance creation!public! !
!Icon class categoriesFor: #isIcon!public!testing! !

!Image class methodsFor!

fromId: aFilename in: anInstanceHandle usingLocator: aFileLocator 
	"Answer an instance loaded from the file aFilename by using aFileLocator."

	| relativeFilename |
	relativeFilename := (aFileLocator isNil or: [File isRelativePath: aFilename]) 
				ifTrue: [aFilename]
				ifFalse: [aFileLocator relativePathTo: aFilename].
	^(self fromId: relativeFilename in: anInstanceHandle)
		fileLocator: aFileLocator;
		yourself!

fromResourceBytes: aByteArray 
	self subclassResponsibility!

fromResourceBytes: aByteArray id: anIntegerOrStringResourceId locale: aLocale in: anInstanceHandle usingLocator: aFileLocator type: aResourceType 
	^(self fromResourceBytes: aByteArray)
		identifier: anIntegerOrStringResourceId;
		instanceHandle: anInstanceHandle;
		fileLocator: aFileLocator; yourself! !
!Image class categoriesFor: #fromId:in:usingLocator:!public! !
!Image class categoriesFor: #fromResourceBytes:!*-not in class package!instance creation!public! !
!Image class categoriesFor: #fromResourceBytes:id:locale:in:usingLocator:type:!*-not in class package!instance creation!public! !

!KernelLibrary methodsFor!

enumResourceLanguages: hModule lpszType: lpszType lpName: lpName lpEnumFunc: lpEnumFunc lParam: lParam 
	"The EnumResourceLanguages function searches a module for each resource of the
	specified type and name and passes the language of each resource it locates to a
	defined callback function. 

	BOOL EnumResourceLanguages(
		HMODULE hModule,
		LPCTSTR lpType,
		LPCTSTR lpName,
		ENUMRESLANGPROC lpEnumFunc,
		LONG_PTR lParam
	);"

	<stdcall: bool EnumResourceLanguagesA handle lpvoid lpvoid lpvoid sdword>
	^self invalidCall!

enumResourceNames: hModule lpszType: lpszType lpEnumFunc: lpEnumFunc lParam: lParam 
	"The EnumResourceNames function searches a module for each resource of the specified type
	and passes either the name or the identifier (ID) of each resource it locates to an application-defined
	callback function.
	
	BOOL EnumResourceNames(          HMODULE hModule,
		LPCTSTR lpszType,
		ENUMRESNAMEPROC lpEnumFunc,
		LONG_PTR lParam
	);"

	<stdcall: bool EnumResourceNamesA handle lpvoid lpvoid sdword>
	^self invalidCall!

enumResourceTypes: hModule lpEnumFunc: lpEnumFunc lParam: lParam 
	"The EnumResourceTypes function searches a module for resources and passes
	each resource type it finds to an application-defined callback function. 

	BOOL EnumResourceTypes(
		HMODULE hModule,
		ENUMRESTYPEPROC lpEnumFunc,
		LONG_PTR lParam
	);"

	<stdcall: bool EnumResourceTypesA handle lpvoid sdword>
	^self invalidCall!

findResourceEx: anInstanceHandle lpName: anIntegerId lpType: anIntegerType wLanguage: wLanguage 
	"Determines the location of the resource with the specified type, name, and language in the specified module.
		HRSRC FindResourceEx(
			HMODULE hModule,
			LPCTSTR lpType,
			LPCTSTR lpName,
			WORD wLanguage
		);"

	<stdcall: handle FindResourceExA handle lpvoid lpvoid word>
	^self invalidCall! !
!KernelLibrary categoriesFor: #enumResourceLanguages:lpszType:lpName:lpEnumFunc:lParam:!*-not in class package!public!win32 functions-resource! !
!KernelLibrary categoriesFor: #enumResourceNames:lpszType:lpEnumFunc:lParam:!*-not in class package!public!win32 functions-resource! !
!KernelLibrary categoriesFor: #enumResourceTypes:lpEnumFunc:lParam:!*-not in class package!public!win32 functions-resource! !
!KernelLibrary categoriesFor: #findResourceEx:lpName:lpType:wLanguage:!public!win32 functions-resource! !

!Locale class methodsFor!

dolphinResourceDefault
^self lcid: 2057! !
!Locale class categoriesFor: #dolphinResourceDefault!public! !

!Object methodsFor!

resourceType
	^self class defaultResourceType! !
!Object categoriesFor: #resourceType!public! !

!Object class methodsFor!

basicFromId: anIntegerOrStringId locale: aLocale in: anInstanceHandle fileLocator: aFileLocator type: aResourceType 
	| spec |
	anInstanceHandle notNil 
		ifTrue: 
			[(self 
				loadFromInstanceId: anIntegerOrStringId
				locale: aLocale
				in: anInstanceHandle
				usingLocator: aFileLocator
				type: aResourceType) ifNotNil: [:bytes | ^bytes]].
	spec := (anIntegerOrStringId isInteger or: [aFileLocator isNil]) 
				ifTrue: [anIntegerOrStringId]
				ifFalse: [aFileLocator localFileSpecFor: anIntegerOrStringId].
	spec isInteger 
		ifFalse: 
			[(self 
				loadFromFile: spec
				id: anIntegerOrStringId
				locale: aLocale
				in: anInstanceHandle
				usingLocator: aFileLocator
				type: aResourceType ) ifNotNil: [:bytes | ^bytes]].
	^self 
		loadFromInstanceId: anIntegerOrStringId
		locale: aLocale
		in: SessionManager current defaultResourceLibrary
		usingLocator: aFileLocator
		type: aResourceType!

basicFromResourceFindHandle: resourceFindHandle id: anIntegerOrStringResourceId locale: aLocale in: anExternalLibraryOrHandle usingLocator: aFileLocator type: aResourceType 
	| resourceAddress resourceLoadHandle resourceLength lib |
	lib := KernelLibrary default.
	(resourceLoadHandle := lib loadResource: anExternalLibraryOrHandle asParameter
				hResInfo: resourceFindHandle) isNull 
		ifTrue: [self error: 'Unable to load resource from module: ' , self printString].
	(resourceAddress := (lib lockResource: resourceLoadHandle) asExternalAddress) isNull 
		ifTrue: [self error: 'Unable to lock resource from module: ' , self printString].
	(resourceLength := lib sizeOfResource: anExternalLibraryOrHandle asParameter
				hResInfo: resourceFindHandle) isNull 
		ifTrue: [self error: 'Unable get size of resource from module: ' , self printString].
	^self 
		fromResourceBytes: (ByteArray fromAddress: resourceAddress length: resourceLength)
		id: anIntegerOrStringResourceId
		locale: aLocale
		in: anExternalLibraryOrHandle
		usingLocator: aFileLocator
		type: aResourceType!

defaultResourceType
	^'STB'!

fromFile: aFilename
	"Answer an instance loaded from the file aFilename.
	Note that aFilename will be converted to an installation relative path. If you want 
	to access an image at an absolute path, you should set it up using #fromFile:usingLocator:
	in conjuction with an AbsoluteFileLocator. e.g:

		ImagePresenter show: 'Basic image' on: 
			(Bitmap
				fromFile: (File composePath: SessionManager current windowsDirectory 
							subPath: 'Soap Bubbles.bmp')
				usingLocator: FileLocator absolute)
	"

	^self  fromFile: aFilename usingLocator: FileLocator default
!

fromFile: aFilename usingLocator: aFileLocator 
	^self fromFile: aFilename usingLocator: aFileLocator type: self defaultResourceType!

fromFile: aFilename usingLocator: aFileLocator type: aResourceType 
	"Answer an instance loaded from the file aFilename by using aFileLocator."

	| relativeFilename |
	relativeFilename := (aFileLocator isNil or: [File isRelativePath: aFilename]) 
				ifTrue: [aFilename]
				ifFalse: [aFileLocator relativePathTo: aFilename].
	^self 
		fromId: relativeFilename
		in: nil
		usingLocator: aFileLocator
		type: aResourceType!

fromId: anIntegerOrStringId
	"Answer a new instance with a resource Id of anIntegerOrStringId."

	^self fromId: anIntegerOrStringId in: SessionManager current defaultResourceLibrary
!

fromId: anIntegerOrStringId in: anInstanceHandle 
	"Answer a new instance with a resourceID of anIntegerOrStringId
	from the application instance anInstanceHandle"

	^self 
		fromId: anIntegerOrStringId
		in: anInstanceHandle
		type: self defaultResourceType!

fromId: anIntegerOrStringResourceId in: anExternalLibraryOrHandle type: anIntegerOrStringIResourceType 
^self fromId: anIntegerOrStringResourceId locale: nil in: anExternalLibraryOrHandle type: anIntegerOrStringIResourceType!

fromId: aFilename in: anInstanceHandle usingLocator: aFileLocator 
	^self 
		fromId: aFilename
		in: anInstanceHandle
		usingLocator: aFileLocator
		type: self defaultResourceType!

fromId: aFilename in: anInstanceHandle usingLocator: aFileLocator type: aResourceType 
	"Answer an instance loaded from the file aFilename by using aFileLocator."

^self fromId: aFilename locale: nil in: anInstanceHandle usingLocator: aFileLocator type: aResourceType!

fromId: anIntegerOrStringId  locale: aLocale 
	"Answer a new instance with a resource Id of anIntegerOrStringId."

^self fromId: anIntegerOrStringId locale: aLocale in: SessionManager current defaultResourceLibrary
	!

fromId: anIntegerOrStringId locale: aLocale in: anInstanceHandle 
	"Answer a new instance with a resourceID of anIntegerOrStringId
	from the application instance anInstanceHandle"

	^self  fromId: anIntegerOrStringId locale: aLocale in: anInstanceHandle type: self defaultResourceType
!

fromId: anIntegerOrStringResourceId locale: aLocale in: anExternalLibraryOrHandle type: anIntegerOrStringIResourceType 
	^self fromId: anIntegerOrStringResourceId locale: aLocale in: anExternalLibraryOrHandle usingLocator: nil type: anIntegerOrStringIResourceType !

fromId: anIntegerOrStringResourceId locale: aLocale in: anExternalLibraryOrHandle usingLocator: aFileLocator type: anIntegerOrStringIResourceType 
	^self basicFromId: anIntegerOrStringResourceId locale: aLocale in: anExternalLibraryOrHandle fileLocator: aFileLocator type: anIntegerOrStringIResourceType!

fromId: anIntegerOrStringResourceId type: anIntegerOrStringIResourceType 
	^self 
		fromId: anIntegerOrStringResourceId
		in: SessionManager current defaultResourceLibrary
		type: anIntegerOrStringIResourceType!

fromResourceBytes: aByteArray id: anIntegerOrStringResourceId locale: aLocale in: anInstanceHandle usingLocator: aFileLocator type: aResourceType 
	^Object fromBinaryStoreBytes: aByteArray!

fromSystemId: anIntegerId
	"Answer a new instance of the Windows pre-defined icon specified by anIntegerID."

	^self fromId: anIntegerId in: 0.
!

loadFromFile: aFilename id: anIntegerOrStringResourceId locale: aLocale in: anInstanceHandle usingLocator: aFileLocator type: aResourceType 
	| stream contents |
	^(File exists: aFilename) 
		ifTrue: 
			[stream := FileStream read: aFilename text: false.
			contents := stream contents.
			stream close.
			self 
				fromResourceBytes: contents
				id: anIntegerOrStringResourceId
				locale: aLocale
				in: anInstanceHandle
				usingLocator: aFileLocator
				type: aResourceType ]
		ifFalse: [nil]!

loadFromInstanceId: anIntegerOrStringResourceId locale: aLocale in: anExternalLibraryOrHandle usingLocator: aFileLocator type: anIntegerOrStringIResourceType 
	| hFind |
	hFind := aLocale isNil 
				ifTrue: 
					[KernelLibrary default 
						findResource: anExternalLibraryOrHandle asParameter
						lpName: anIntegerOrStringResourceId
						lpType: anIntegerOrStringIResourceType]
				ifFalse: 
					[KernelLibrary default 
						findResourceEx: anExternalLibraryOrHandle asParameter
						lpName: anIntegerOrStringResourceId
						lpType: anIntegerOrStringIResourceType
						wLanguage: aLocale asParameter].
	hFind isNull ifTrue: [^nil].
	^self 
		basicFromResourceFindHandle: hFind
		id: anIntegerOrStringResourceId
		locale: aLocale
		in: anExternalLibraryOrHandle
		usingLocator:aFileLocator
		type: anIntegerOrStringIResourceType! !
!Object class categoriesFor: #basicFromId:locale:in:fileLocator:type:!*-not in class package!private! !
!Object class categoriesFor: #basicFromResourceFindHandle:id:locale:in:usingLocator:type:!*-not in class package!instance creation!private! !
!Object class categoriesFor: #defaultResourceType!public! !
!Object class categoriesFor: #fromFile:!instance creation!public! !
!Object class categoriesFor: #fromFile:usingLocator:!instance creation!public! !
!Object class categoriesFor: #fromFile:usingLocator:type:!instance creation!public! !
!Object class categoriesFor: #fromId:!instance creation!public! !
!Object class categoriesFor: #fromId:in:!*-not in class package!instance creation!public! !
!Object class categoriesFor: #fromId:in:type:!*-not in class package!instance creation!public! !
!Object class categoriesFor: #fromId:in:usingLocator:!public! !
!Object class categoriesFor: #fromId:in:usingLocator:type:!*-not in class package!public! !
!Object class categoriesFor: #fromId:locale:!*-not in class package!instance creation!public! !
!Object class categoriesFor: #fromId:locale:in:!*-not in class package!instance creation!public! !
!Object class categoriesFor: #fromId:locale:in:type:!*-not in class package!instance creation!public! !
!Object class categoriesFor: #fromId:locale:in:usingLocator:type:!*-not in class package!instance creation!public! !
!Object class categoriesFor: #fromId:type:!instance creation!public! !
!Object class categoriesFor: #fromResourceBytes:id:locale:in:usingLocator:type:!*-not in class package!instance creation!public! !
!Object class categoriesFor: #fromSystemId:!instance creation!public! !
!Object class categoriesFor: #loadFromFile:id:locale:in:usingLocator:type:!*-not in class package!private! !
!Object class categoriesFor: #loadFromInstanceId:locale:in:usingLocator:type:!*-not in class package!instance creation!private! !

!OLEPicture methodsFor!

loadFromFile: pathString extent: anObject 
	"Private - Attempts to load the receiver from a disk file/url. If the image
	could not be loaded (e.g. the file was not found) then a picture 
	wrapping the standard question mark icon is created. Answers the GDI
	handle of the picture."

	identifier isNil 
		ifFalse: 
			[[self picture: (IPicture fromFile: pathString asParameter)] on: HRESULTError
				do: 
					[:ex | 
					Notification 
						signal: ('Failed to load OLE Picture from file <1p>: <2s>' expandMacrosWith: pathString
								with: ex messageText)]].
	^picture isNull 
		ifTrue: 
			[nil] ifFalse: [
	picture handle]!

loadFromInstance: hModule 
	"Private - Attempts to load the receiver as a resource from an instance.
	Answers nil if the load failed, or the image handle if successful."

	| byteArray pic |
	
	byteArray := ByteArray 
				fromId: identifier
				in: hModule
				type: self resourceType.
	^byteArray 
		ifNotNil: 
			[:value | 
			pic := IPicture readFromIStream: ((ReadStream on: value) queryInterface: IStream).
			self picture: pic.
			picture handle]! !
!OLEPicture categoriesFor: #loadFromFile:extent:!must not strip!private!realizing/unrealizing! !
!OLEPicture categoriesFor: #loadFromInstance:!*-not in class package!must not strip!private!realizing/unrealizing! !

!OLEPicture class methodsFor!

defaultResourceType
	^'OLEPICTURE'!

fromByteArray: aByteArray 
	"Answer an instance of the receiver created from data in aByteArray."

	| pStream |
	pStream := IStream onHGLOBAL.
	pStream nextPutAll: aByteArray.
	pStream reset.
	^self fromIStream: pStream

!

fromIStream: pStream 
	^self fromIPicture: (IPicture readFromIStream: pStream)


!

fromResourceBytes: aByteArray
	^self fromByteArray: aByteArray! !
!OLEPicture class categoriesFor: #defaultResourceType!public! !
!OLEPicture class categoriesFor: #fromByteArray:!public! !
!OLEPicture class categoriesFor: #fromIStream:!public! !
!OLEPicture class categoriesFor: #fromResourceBytes:!*-not in class package!instance creation!public! !

!String class methodsFor!

defaultResourceType
	^##(Win32Constants at: 'RT_STRING')! !
!String class categoriesFor: #defaultResourceType!*-not in class package!public! !

!UserLibrary methodsFor!

createIconFromResource: presbits dwResSize: dwResSize fIcon: fIcon dwVer: dwVer 
	"The CreateIconFromResource function creates an icon or cursor from resource bits describing the icon.

	HICON CreateIconFromResource(
		PBYTE presbits,
		DWORD dwResSize,
		BOOL fIcon,
		DWORD dwVer
	);"

<stdcall: handle CreateIconFromResource lpvoid dword bool dword>
^self invalidCall
	!

lookupIconIdFromDirectory: presbits ficon: fIcon 
	"The LookupIconIdFromDirectory function searches through icon or cursor data for the icon or cursor that best fits the current display device.

	int LookupIconIdFromDirectory
		PBYTE presbits,
		BOOL fIcon
	);"

	<stdcall: sdword LookupIconIdFromDirectory lpvoid bool>
	^self invalidCall! !
!UserLibrary categoriesFor: #createIconFromResource:dwResSize:fIcon:dwVer:!*-not in class package!public! !
!UserLibrary categoriesFor: #lookupIconIdFromDirectory:ficon:!*-not in class package!public!win32 functions-resource! !

"End of package definition"!

"Source Globals"!

"Classes"!

WindowsResourceIdentifier guid: (GUID fromString: '{D77A89E0-57EF-4C1B-B9B7-104FE015A8DA}')!
WindowsResourceIdentifier comment: ''!
!WindowsResourceIdentifier categoriesForClass!Unclassified! !
!WindowsResourceIdentifier methodsFor!

id
	^id!

id: anObject
	id := anObject!

locale
	^locale!

locale: anObject
	locale := anObject!

setType: aResourceType
		id: aResourceId
		locale: aLocale
		type := aResourceType.
		id := aResourceId.
		locale := aLocale!

type
	^type!

type: anObject
	type := anObject! !
!WindowsResourceIdentifier categoriesFor: #id!accessing!public! !
!WindowsResourceIdentifier categoriesFor: #id:!accessing!private! !
!WindowsResourceIdentifier categoriesFor: #locale!accessing!public! !
!WindowsResourceIdentifier categoriesFor: #locale:!accessing!private! !
!WindowsResourceIdentifier categoriesFor: #setType:id:locale:!public! !
!WindowsResourceIdentifier categoriesFor: #type!accessing!public! !
!WindowsResourceIdentifier categoriesFor: #type:!accessing!private! !

!WindowsResourceIdentifier class methodsFor!

new
"Use #type:id:locale: instead"
	self shouldNotImplement!

type: aResourceType id: aResourceId locale: aLocale 
	^super new setType: aResourceType id: aResourceId locale: aLocale ! !
!WindowsResourceIdentifier class categoriesFor: #new!public! !
!WindowsResourceIdentifier class categoriesFor: #type:id:locale:!public! !

"Binary Globals"!

