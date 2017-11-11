"Filed out from Dolphin Smalltalk 7"!

GDILibrary subclass: #GDILibraryW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GDILibraryW guid: (GUID fromString: '{80e1b07e-46ed-40eb-bf92-98d4dd955556}')!
GDILibraryW comment: ''!
!GDILibraryW categoriesForClass!External-Data-Structured-Win32! !
!GDILibraryW methodsFor!

addFontResource: fontFile

^self addFontResourceW: fontFile!

addFontResourceW: fontFile 
	"Adds the font resource from the specified file to the system font table.
	The font can subsequently be used for text output by any application. 

	int AddFontResource(
		LPCTSTR lpszFilename   // font file name
	);"

	<stdcall: sdword AddFontResourceW lpstr>
	^self invalidCall!

copyEnhMetaFile: hemf lpszFile: lpszFile

^self copyEnhMetaFileW: hemf lpszFile: lpszFile!

copyEnhMetaFileW: hemf lpszFile: lpszFile
	"Invoke the CopyEnhMetaFile() function of the module wrapped by the receiver.
	Helpstring: Copies contents of enhanced-format metafile to specified file.

		HENHMETAFILE __stdcall CopyEnhMetaFile(
			HENHMETAFILE hemf,
			LPCSTR lpszFile);"

	<stdcall: handle CopyEnhMetaFileW handle lpstr>
	^self invalidCall!

createDC: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpInitData: lpInitData

^self createDCW: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpInitData: lpInitData!

createDCW: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpInitData: lpInitData
	"Invoke the CreateDC() function of the module wrapped by the receiver.
	Helpstring: Creates a device context (DC) for a device by using the specified name

		HDC __stdcall CreateDC(
			LPCSTR lpszDriver,
			LPCSTR lpszDevice,
			LPCSTR lpszOutput,
			DEVMODE* lpInitData);"

	<stdcall: handle CreateDCW lpstr lpstr lpstr DEVMODE*>
	^self invalidCall!

createEnhMetaFile: hdc lpFileName: lpFileName lpRect: lpRect lpDescription: lpDescription

^self createEnhMetaFileW: hdc lpFileName: lpFileName lpRect: lpRect lpDescription: lpDescription!

createEnhMetaFileW: hdc lpFileName: lpFileName lpRect: lpRect lpDescription: lpDescription
	"Invoke the CreateEnhMetaFile() function of the module wrapped by the receiver.
	Helpstring: Creates device context for enhanced-format metafile.

		HDC __stdcall CreateEnhMetaFile(
			HDC HDC,
			LPCSTR lpFileName,
			int* lpRect,
			LPCSTR lpDescription);"

	<stdcall: handle CreateEnhMetaFileW handle lpstr sdword* lpstr>
	^self invalidCall!

createFontIndirect: lplf

^self createFontIndirectW: lplf!

createFontIndirectW: lplf
	"Invoke the CreateFontIndirect() function of the module wrapped by the receiver.
	Helpstring: Creates a logical font that has the characteristics specified in the specified structure

		HFONT __stdcall CreateFontIndirect(
			LOGFONT* lplf);"

	<stdcall: handle CreateFontIndirectW LOGFONT*>
	^self invalidCall!

createIC: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpdvminit: lpdvminit

^self createICW: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpdvminit: lpdvminit!

createICW: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpdvminit: lpdvminit
	"Invoke the CreateIC() function of the module wrapped by the receiver.
	Helpstring: Creates an information context for the specified device

		HDC __stdcall CreateIC(
			LPCSTR lpszDriver,
			LPCSTR lpszDevice,
			LPCSTR lpszOutput,
			DEVMODE* lpdvminit);"

	<stdcall: handle CreateICW lpstr lpstr lpstr DEVMODE*>
	^self invalidCall!

enumFonts: dcHandle lpFaceName: aString lpFontFunc: aCallbackThunk lParam: anInteger

^self enumFontsW: dcHandle lpFaceName: aString lpFontFunc: aCallbackThunk lParam: anInteger!

enumFontsW: dcHandle lpFaceName: aString lpFontFunc: aCallbackThunk lParam: anInteger
	"Enumerate the fonts with the specified face name on the specified device throught the
	specified callback with the specified extra parameter.

		int EnumFonts(
			HDC hdc,					// handle of device context 
			LPCTSTR lpFaceName,		// font typeface name string 
			FONTENUMPROC lpFontFunc,	// callback function 
			LPARAM lParam 				// application-supplied data
		);"

	<stdcall: sdword EnumFontsW handle lpstr lpvoid uintptr>
	^self invalidCall!

extTextOut: hdc x: x y: y fuOptions: fuOptions lprc: lprc lpString: lpString cbCount: cbCount lpDx: lpDx

^self extTextOutW: hdc x: x y: y fuOptions: fuOptions lprc: lprc lpString: lpString cbCount: cbCount lpDx: lpDx!

extTextOutW: hdc x: x y: y fuOptions: fuOptions lprc: lprc lpString: lpString cbCount: cbCount lpDx: lpDx
	"Invoke the ExtTextOut() function of the module wrapped by the receiver.
	Helpstring: Draws a character string, optionally using a rectangle to specify clipping, opaquing, or both

		BOOL __stdcall ExtTextOut(
			HDC HDC,
			int x,
			int y,
			UINT fuOptions,
			RECT* lprc,
			LPCSTR lpString,
			UINT cbCount,
			int* lpDx);"

	<stdcall: bool ExtTextOutW handle sdword sdword dword RECT* lpstr dword sdword*>
	^self invalidCall!

getEnhMetaFile: lpszMetaFile

^self getEnhMetaFileW: lpszMetaFile!

getEnhMetaFileDescription: hemf cchBuffer: cchBuffer lpszDescription: lpszDescription

^self getEnhMetaFileDescriptionW: hemf cchBuffer: cchBuffer lpszDescription: lpszDescription!

getEnhMetaFileDescriptionW: hemf cchBuffer: cchBuffer lpszDescription: lpszDescription
	"Invoke the GetEnhMetaFileDescription() function of the module wrapped by the receiver.
	Helpstring: Copies optional text description from an enhanced-format metafile to a specified buffer.

		UINT __stdcall GetEnhMetaFileDescription(
			HENHMETAFILE hemf,
			UINT cchBuffer,
			LPSTR lpszDescription);"

	<stdcall: dword GetEnhMetaFileDescriptionW handle dword lpstr>
	^self invalidCall!

getEnhMetaFileW: lpszMetaFile
	"Invoke the GetEnhMetaFile() function of the module wrapped by the receiver.
	Helpstring: Creates handle for given file-based enhanced-format metafile

		HENHMETAFILE __stdcall GetEnhMetaFile(
			LPCSTR lpszMetaFile);"

	<stdcall: handle GetEnhMetaFileW lpstr>
	^self invalidCall!

getObject: objectHandle cbBuffer: count lpvObject: buffer

^self getObjectW: objectHandle cbBuffer: count lpvObject: buffer!

getObjectW: objectHandle cbBuffer: count lpvObject: buffer
	"Obtains information about a specified graphics object. Depending on 
	the graphics object, the function places a filled-in BITMAP, DIBSECTION, 
	EXTLOGPEN, LOGBRUSH, LOGFONT, or LOGPEN structure, or a count of table 
	entries (for a logical palette), into a specified buffer. 
		int GetObject(
  			HGDIOBJ hgdiobj,	// handle to graphics object of interest
			int cbBuffer,	// size of buffer for object information 
			LPVOID lpvObject 	// pointer to buffer for object information  
		);"

	<stdcall: sdword GetObjectW handle sdword lpvoid>
	^self invalidCall!

getOutlineTextMetrics: anExternalHandle cbData: anInteger lpotm: anOUTLINETEXTMETRIC

^self getOutlineTextMetricsW: anExternalHandle cbData: anInteger lpotm: anOUTLINETEXTMETRIC!

getOutlineTextMetricsW: anExternalHandle cbData: anInteger lpotm: anOUTLINETEXTMETRIC 
	<stdcall: dword GetOutlineTextMetricsW handle dword OUTLINETEXTMETRIC*>
	^self invalidCall!

getTextExtentPoint32: hdc lpString: lpString cbString: cbString lpSize: lpSize

^self getTextExtentPoint32W: hdc lpString: lpString cbString: cbString lpSize: lpSize!

getTextExtentPoint32W: hdc lpString: lpString cbString: cbString lpSize: lpSize
	"Invoke the GetTextExtentPoint32() function of the module wrapped by the receiver.
	Helpstring: Computes the width and height of the specified string of text

		BOOL __stdcall GetTextExtentPoint32(
			HDC HDC,
			LPCSTR lpString,
			int cbString,
			SIZEL* lpSize);"

	<stdcall: bool GetTextExtentPoint32W handle lpstr sdword SIZE*>
	^self invalidCall!

getTextMetrics: hdc lptm: lptm

^self getTextMetricsW: hdc lptm: lptm!

getTextMetricsW: hdc lptm: lptm
	"Invoke the GetTextMetrics() GDI call to populate a TEXTMETRIC structure, lptm, with
	metrics for the font currently selected into the device context with handle, hdc."

	<stdcall: bool GetTextMetricsW handle TEXTMETRIC*>
	^self invalidCall!

open: aString

	handle  := self class superclass default handle.!

removeFontResource: fontFile

^self removeFontResourceW: fontFile!

removeFontResourceW: fontFile 
	"The RemoveFontResource function removes the fonts in the specified file from the system font table. 
	If the font was added using the AddFontResourceEx function, you must use the RemoveFontResourceEx function. 

	BOOL RemoveFontResource(
		LPCTSTR lpFileName   // name of font file
		);
	"

	<stdcall: bool RemoveFontResourceW lpstr>
	^self invalidCall!

startDoc: hdc lpdi: info

^self startDocW: hdc lpdi: info!

startDocW: hdc lpdi: info
	"Start a print job.
		int StartDoc(
  			HDC hdc,				// handle of device context 
			CONST DOCINFO *lpdi 	// address of structure with file names  
		);"

	<stdcall: sdword StartDocW handle DOCINFO* >
	^self invalidCall

!

textOut: hdc nXStart: nXStart nYStart: nYStart lpString: lpString cbString: cbString

^self textOutW: hdc nXStart: nXStart nYStart: nYStart lpString: lpString cbString: cbString!

textOutW: hdc nXStart: nXStart nYStart: nYStart lpString: lpString cbString: cbString
	"Invoke the TextOut() function of the module wrapped by the receiver.
	Helpstring: Writes a character string at the specified location, using the currently selected font

		BOOL __stdcall TextOut(
			HDC HDC,
			int nXStart,
			int nYStart,
			LPCSTR lpString,
			int cbString);"

	<stdcall: bool TextOutW handle sdword sdword lpstr sdword>
	^self invalidCall! !
!GDILibraryW categoriesFor: #addFontResource:!public! !
!GDILibraryW categoriesFor: #addFontResourceW:!public! !
!GDILibraryW categoriesFor: #copyEnhMetaFile:lpszFile:!public! !
!GDILibraryW categoriesFor: #copyEnhMetaFileW:lpszFile:!public! !
!GDILibraryW categoriesFor: #createDC:lpszDevice:lpszOutput:lpInitData:!public! !
!GDILibraryW categoriesFor: #createDCW:lpszDevice:lpszOutput:lpInitData:!public! !
!GDILibraryW categoriesFor: #createEnhMetaFile:lpFileName:lpRect:lpDescription:!public! !
!GDILibraryW categoriesFor: #createEnhMetaFileW:lpFileName:lpRect:lpDescription:!public! !
!GDILibraryW categoriesFor: #createFontIndirect:!public! !
!GDILibraryW categoriesFor: #createFontIndirectW:!public! !
!GDILibraryW categoriesFor: #createIC:lpszDevice:lpszOutput:lpdvminit:!public! !
!GDILibraryW categoriesFor: #createICW:lpszDevice:lpszOutput:lpdvminit:!public! !
!GDILibraryW categoriesFor: #enumFonts:lpFaceName:lpFontFunc:lParam:!public! !
!GDILibraryW categoriesFor: #enumFontsW:lpFaceName:lpFontFunc:lParam:!public! !
!GDILibraryW categoriesFor: #extTextOut:x:y:fuOptions:lprc:lpString:cbCount:lpDx:!public! !
!GDILibraryW categoriesFor: #extTextOutW:x:y:fuOptions:lprc:lpString:cbCount:lpDx:!public! !
!GDILibraryW categoriesFor: #getEnhMetaFile:!public! !
!GDILibraryW categoriesFor: #getEnhMetaFileDescription:cchBuffer:lpszDescription:!public! !
!GDILibraryW categoriesFor: #getEnhMetaFileDescriptionW:cchBuffer:lpszDescription:!public! !
!GDILibraryW categoriesFor: #getEnhMetaFileW:!public! !
!GDILibraryW categoriesFor: #getObject:cbBuffer:lpvObject:!public! !
!GDILibraryW categoriesFor: #getObjectW:cbBuffer:lpvObject:!public! !
!GDILibraryW categoriesFor: #getOutlineTextMetrics:cbData:lpotm:!public! !
!GDILibraryW categoriesFor: #getOutlineTextMetricsW:cbData:lpotm:!public! !
!GDILibraryW categoriesFor: #getTextExtentPoint32:lpString:cbString:lpSize:!public! !
!GDILibraryW categoriesFor: #getTextExtentPoint32W:lpString:cbString:lpSize:!public! !
!GDILibraryW categoriesFor: #getTextMetrics:lptm:!public! !
!GDILibraryW categoriesFor: #getTextMetricsW:lptm:!public! !
!GDILibraryW categoriesFor: #open:!public! !
!GDILibraryW categoriesFor: #removeFontResource:!public! !
!GDILibraryW categoriesFor: #removeFontResourceW:!public! !
!GDILibraryW categoriesFor: #startDoc:lpdi:!public! !
!GDILibraryW categoriesFor: #startDocW:lpdi:!public! !
!GDILibraryW categoriesFor: #textOut:nXStart:nYStart:lpString:cbString:!public! !
!GDILibraryW categoriesFor: #textOutW:nXStart:nYStart:lpString:cbString:!public! !

