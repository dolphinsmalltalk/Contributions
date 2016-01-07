| package |
package := Package name: 'US GDI+ Extensions'.
package paxVersion: 1;
	basicComment: '$id: US GDI+ Extensions 0.011$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 21.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.011'.


package classNames
	add: #GdiplusBitmapFromRawInitializer;
	add: #GdiplusRegionFromRegionInitializer;
	yourself.

package methodNames
	add: #GdiplusBitmap -> #setResolution:;
	add: #GdiplusImage -> #pixelFormatSize;
	add: #GdiplusLibrary -> #gdipCreateRegionHrgn:region:;
	add: 'GdiplusBitmap class' -> #fromIconEx:;
	add: 'GdiplusBitmap class' -> #fromRegionView:;
	add: 'GdiplusBitmap class' -> #width:height:stride:format:scan:;
	add: 'GdiplusRegion class' -> #fromRegion:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	yourself).

package!

"Class Definitions"!

GdiplusInitializer subclass: #GdiplusBitmapFromRawInitializer
	instanceVariableNames: 'width height stride format scan'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GdiplusInitializer subclass: #GdiplusRegionFromRegionInitializer
	instanceVariableNames: 'region'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!GdiplusBitmap methodsFor!

setResolution: aDPIPoint 
	"Answer an ARGB that is the color of the receiver at aPoint."

	| status  |
	status := GdiplusLibrary default 
				gdipBitmapSetResolution: self asParameter
				xdpi: aDPIPoint x
				ydpi: aDPIPoint y.
	status = Ok ifFalse: [GdiplusError signal: 'GdipBitmapSetResolution:  failed' with: status]! !
!GdiplusBitmap categoriesFor: #setResolution:!accessing!public! !

!GdiplusBitmap class methodsFor!

fromIconEx: anIcon 
	"Creates a Bitmap object based on an icon.
	This method has special handling for icons with alpha transparency."

	| hbmColor hbmColorCopy bitmap bitmapHeader |
	hbmColor := DIBSection fromHandle: anIcon getIconInfo hbmColor.
	"Note: Checking the depth of hbmColor doesn't work as this is a Bitmap and thus always equal to screen depth"
	(GdiplusBitmap fromBitmap: hbmColor) pixelFormatSize = 32 
		ifFalse: 
			["We only know how to handle 32bit images"
			^self fromIcon: anIcon].
		"Create new dib with size of icon"
	hbmColorCopy := DIBSection 
				width: anIcon extent x
				height: anIcon extent y
				depth: 32.
	bitmap := hbmColorCopy getDIBSECTION dsBm.
	bitmapHeader := hbmColorCopy getDIBSECTION dsBmih.
	bitmapHeader biHeight: bitmapHeader biHeight negated.
	"Copy bits from icon to new dib as the icon does not allow bits access"
	GDILibrary default 
		getDIBits: UserLibrary default getDC
		hbm: hbmColor asParameter
		uStartScan: 0
		cScanLines: hbmColorCopy extent y
		lpvBits: bitmap bmBits asParameter
		lpbi: bitmapHeader
		uUsage: DIB_RGB_COLORS.
	^GdiplusBitmap 
		width: bitmap bmWidth
		height: bitmap bmHeight
		stride: bitmap bmWidthBytes
		format: PixelFormat32bppARGB
		scan: bitmap bmBits asParameter!

fromRegionView: aView 
	| image |
	image := self 
				width: aView width
				height: aView height
				format: PixelFormat32bppARGB.
	(image graphics)
		clear: (ARGB 
					a: 0
					r: 0
					g: 0
					b: 0);
		clipRegion: (GdiplusRegion fromRegion: aView region) combineMode: nil;
		drawImage: (GdiplusBitmap fromBitmap: (aView printWindow )).
	^image!

width: widthInteger height: heightInteger stride: strideInteger format: aPixelFormat scan: anExternalAddress 
	^self fromInitializer: (GdiplusBitmapFromRawInitializer 
				width: widthInteger
				height: heightInteger
				stride: strideInteger
				format: aPixelFormat
				scan: anExternalAddress)! !
!GdiplusBitmap class categoriesFor: #fromIconEx:!instance creation!public! !
!GdiplusBitmap class categoriesFor: #fromRegionView:!public! !
!GdiplusBitmap class categoriesFor: #width:height:stride:format:scan:!instance creation!public! !

!GdiplusImage methodsFor!

pixelFormatSize
"Return the bit depth of the bitmap"
	^(self pixelFormat bitShift: -8) bitAnd: 16rFF! !
!GdiplusImage categoriesFor: #pixelFormatSize!accessing!public! !

!GdiplusLibrary methodsFor!

gdipCreateRegionHrgn: hRgn region: region 
	"Invoke the GdipCreateRegionHrgn() function of the module wrapped by the receiver.

		GpStatus __stdcall GdipCreateRegionHrgn(
			void* hRgn,
			[out, retval]PGpRegion* region);"

	<stdcall: sdword GdipCreateRegionHrgn handle handle*>
	^self invalidCall! !
!GdiplusLibrary categoriesFor: #gdipCreateRegionHrgn:region:!**auto generated**!public! !

!GdiplusRegion class methodsFor!

fromRegion: aRegion
	^self fromInitializer: (GdiplusRegionFromRegionInitializer fromRegion: aRegion )! !
!GdiplusRegion class categoriesFor: #fromRegion:!instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

GdiplusBitmapFromRawInitializer guid: (GUID fromString: '{9368EE40-C3A0-4A20-BA45-0F4419DE569D}')!
GdiplusBitmapFromRawInitializer comment: ''!
!GdiplusBitmapFromRawInitializer categoriesForClass!Unclassified! !
!GdiplusBitmapFromRawInitializer methodsFor!

createHandle
	" Bitmap::Bitmap(
	    IN INT width,
	    IN INT height,
	    IN PixelFormat format
	    )
	{
	    GpBitmap *bitmap = NULL;

	    lastResult = DllExports::GdipCreateBitmapFromScan0(width,
                                                       height,
                                                       stride,
                                                       format,
                                                       scan0,
                                                       &bitmap);"

	| gpHandle status |
	gpHandle := ExternalHandle new.
	status := self library 
				gdipCreateBitmapFromScan0: width
				height: height
				stride: stride
				format: format
				scan0: scan asParameter
				bitmap: gpHandle.
	self assertStatusOk: status.
	^gpHandle!

gdiplusConstructorErrorDescription
	^'Error creating Bitmap'!

width: widthInteger height: heightInteger stride: strideInteger format: aPixelFormat scan: anExternalAddress 
	
	width := widthInteger.
	height := heightInteger.
	stride := strideInteger.
	format := aPixelFormat.
	scan := anExternalAddress! !
!GdiplusBitmapFromRawInitializer categoriesFor: #createHandle!public!realizing/unrealizing! !
!GdiplusBitmapFromRawInitializer categoriesFor: #gdiplusConstructorErrorDescription!constants!private! !
!GdiplusBitmapFromRawInitializer categoriesFor: #width:height:stride:format:scan:!accessing!public! !

!GdiplusBitmapFromRawInitializer class methodsFor!

width: widthInteger height: heightInteger stride: strideInteger format: aPixelFormat scan: anExternalAddress 
	^self new 
	width: widthInteger height: heightInteger stride: strideInteger format: aPixelFormat scan: anExternalAddress ! !
!GdiplusBitmapFromRawInitializer class categoriesFor: #width:height:stride:format:scan:!instance creation!public! !

GdiplusRegionFromRegionInitializer guid: (GUID fromString: '{1F63EE28-03BA-4BC5-98EF-122781F6F109}')!
GdiplusRegionFromRegionInitializer comment: ''!
!GdiplusRegionFromRegionInitializer categoriesForClass!Unclassified! !
!GdiplusRegionFromRegionInitializer methodsFor!

createHandle
	| gpHandle status |
	gpHandle := ExternalHandle new.
	status := self library gdipCreateRegionHrgn: region asParameter region: gpHandle.
	
	self assertStatusOk: status.
	^gpHandle!

gdiplusConstructorErrorDescription
	^'Error creating Region'!

region: aRegion 
	region := aRegion! !
!GdiplusRegionFromRegionInitializer categoriesFor: #createHandle!public!realizing/unrealizing! !
!GdiplusRegionFromRegionInitializer categoriesFor: #gdiplusConstructorErrorDescription!constants!private! !
!GdiplusRegionFromRegionInitializer categoriesFor: #region:!accessing!public! !

!GdiplusRegionFromRegionInitializer class methodsFor!

fromRegion: aRegion 
	^self new region: aRegion! !
!GdiplusRegionFromRegionInitializer class categoriesFor: #fromRegion:!instance creation!public! !

"Binary Globals"!

