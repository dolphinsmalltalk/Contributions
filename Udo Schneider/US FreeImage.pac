| package |
package := Package name: 'US FreeImage'.
package paxVersion: 1;
	basicComment: '$id: US FreeImage 0.030$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.030'.


package classNames
	add: #FIBITMAP;
	add: #FICOMPLEX;
	add: #FIICCPROFILE;
	add: #FIMEMORY;
	add: #FIMETADATA;
	add: #FIMULTIBITMAP;
	add: #FIRGB16;
	add: #FIRGBA16;
	add: #FIRGBAF;
	add: #FIRGBF;
	add: #FITAG;
	add: #FreeImageAbstractBitmapOperation;
	add: #FreeImageAbstractJpegOperation;
	add: #FreeImageAbstractJpegTransformationOperation;
	add: #FreeImageAdaptiveLogarithmicToneMappingOperation;
	add: #FreeImageAdjustBrightnessOperation;
	add: #FreeImageAdjustContrastOperation;
	add: #FreeImageAdjustCurveOperation;
	add: #FreeImageAdjustGammaOperation;
	add: #FreeImageBitmap;
	add: #FreeImageColorQuantizeOperation;
	add: #FreeImageCompositeOperation;
	add: #FreeImageConvertTo16BitsOperation;
	add: #FreeImageConvertTo24BitsOperation;
	add: #FreeImageConvertTo32BitsOperation;
	add: #FreeImageConvertTo4BitsOperation;
	add: #FreeImageConvertTo8BitsOperation;
	add: #FreeImageConvertToGreyscaleOperation;
	add: #FreeImageConvertToMonochromeOperation;
	add: #FreeImageConvertToRGBFOperation;
	add: #FreeImageConvertToStandardTypeOperation;
	add: #FreeImageConvertToTypeOperation;
	add: #FreeImageCopyOperation;
	add: #FreeImageDestructiveBitmapOperation;
	add: #FreeImageDitherOperation;
	add: #FreeImageDynamicRangeReductionToneMapping;
	add: #FreeImageError;
	add: #FreeImageExtendedColorQuantizeOperation;
	add: #FreeImageFlipHorizontalOperation;
	add: #FreeImageFlipVerticalOperation;
	add: #FreeImageFormat;
	add: #FreeImageGetChannelOperation;
	add: #FreeImageHorizontalFlipJpegOperation;
	add: #FreeImageICCProfile;
	add: #FreeImageInvertOperation;
	add: #FreeImageIO;
	add: #FreeImageJpegCropOperation;
	add: #FreeImageLibrary;
	add: #FreeImageMemoryStream;
	add: #FreeImageMetadata;
	add: #FreeImageNonDestructiveOperation;
	add: #FreeImagePasteOperation;
	add: #FreeImageRescaleOperation;
	add: #FreeImageRotate180JpegOperation;
	add: #FreeImageRotate270JpegOperation;
	add: #FreeImageRotate90JpegOperation;
	add: #FreeImageRotateClassicOperation;
	add: #FreeImageRotateExOperation;
	add: #FreeImageSetChannelOperation;
	add: #FreeImageTag;
	add: #FreeImageThumbnailOperation;
	add: #FreeImageTransposeJpegOperation;
	add: #FreeImageTransverseJpegOperation;
	add: #FreeImageVerticalFlipJpegOperation;
	add: #Plugin;
	add: #RGBTRIPLE;
	yourself.

package methodNames
	add: 'Color class' -> #fromSVGSpec:;
	add: 'Color class' -> #fromX11Spec:;
	add: 'Color class' -> #primLookupSVGColor:;
	add: 'Color class' -> #primLookupX11Color:;
	yourself.

package globalNames
	add: #FreeImageConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	add: 'Graphics\US GDI Extensions';
	yourself).

package setManualPrerequisites: #(
	'US GDI Extensions').

package!

"Class Definitions"!

Object subclass: #FreeImageAbstractBitmapOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
Object subclass: #FreeImageAbstractJpegOperation
	instanceVariableNames: 'source destination'
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
Object subclass: #FreeImageBitmap
	instanceVariableNames: 'handle ownsHandle loadFormat'
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants GdiplusConstants Win32Constants'
	classInstanceVariableNames: ''!
Object subclass: #FreeImageFormat
	instanceVariableNames: 'fif flags'
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
Object subclass: #FreeImageICCProfile
	instanceVariableNames: 'profile'
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
Object subclass: #FreeImageMemoryStream
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
Object subclass: #FreeImageMetadata
	instanceVariableNames: 'bitmap fimd'
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
Object subclass: #FreeImageTag
	instanceVariableNames: 'handle ownsHandle metadata'
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants Win32Constants'
	classInstanceVariableNames: ''!
Error subclass: #FreeImageError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #FreeImageLibrary
	instanceVariableNames: ''
	classVariableNames: 'Callback LastErrorMessage'
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIBITMAP
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FICOMPLEX
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIICCPROFILE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIMEMORY
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIMETADATA
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIMULTIBITMAP
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIRGB16
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIRGBA16
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIRGBAF
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FIRGBF
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FITAG
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FreeImageIO
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #Plugin
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #RGBTRIPLE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractBitmapOperation subclass: #FreeImageDestructiveBitmapOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractBitmapOperation subclass: #FreeImageNonDestructiveOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
FreeImageDestructiveBitmapOperation subclass: #FreeImageAdjustBrightnessOperation
	instanceVariableNames: 'contrast'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageDestructiveBitmapOperation subclass: #FreeImageAdjustContrastOperation
	instanceVariableNames: 'brightness'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageDestructiveBitmapOperation subclass: #FreeImageAdjustCurveOperation
	instanceVariableNames: 'channel lut'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageDestructiveBitmapOperation subclass: #FreeImageAdjustGammaOperation
	instanceVariableNames: 'gamma'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageDestructiveBitmapOperation subclass: #FreeImageInvertOperation
	instanceVariableNames: 'contrast'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageDestructiveBitmapOperation subclass: #FreeImagePasteOperation
	instanceVariableNames: 'origin alpha bitmap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageDestructiveBitmapOperation subclass: #FreeImageSetChannelOperation
	instanceVariableNames: 'channel bitmap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageAdaptiveLogarithmicToneMappingOperation
	instanceVariableNames: 'gamma exposure'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageColorQuantizeOperation
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageCompositeOperation
	instanceVariableNames: 'bitmap appBgkColor useFileBgk'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertTo16BitsOperation
	instanceVariableNames: 'highDefGreen'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertTo24BitsOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertTo32BitsOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertTo4BitsOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertTo8BitsOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertToGreyscaleOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertToMonochromeOperation
	instanceVariableNames: 'threshold'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertToRGBFOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageConvertToStandardTypeOperation
	instanceVariableNames: 'scaleType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageCopyOperation
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageDitherOperation
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageDynamicRangeReductionToneMapping
	instanceVariableNames: 'intensity contrast'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageFlipHorizontalOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageFlipVerticalOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageGetChannelOperation
	instanceVariableNames: 'channel'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageRescaleOperation
	instanceVariableNames: 'extent filter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageRotateClassicOperation
	instanceVariableNames: 'angle'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageNonDestructiveOperation subclass: #FreeImageThumbnailOperation
	instanceVariableNames: 'size convert'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageColorQuantizeOperation subclass: #FreeImageExtendedColorQuantizeOperation
	instanceVariableNames: 'paletteSize reservedPalette'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageConvertToStandardTypeOperation subclass: #FreeImageConvertToTypeOperation
	instanceVariableNames: 'type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageRotateClassicOperation subclass: #FreeImageRotateExOperation
	instanceVariableNames: 'shift origin mask'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractJpegOperation subclass: #FreeImageAbstractJpegTransformationOperation
	instanceVariableNames: 'perfect'
	classVariableNames: ''
	poolDictionaries: 'FreeImageConstants'
	classInstanceVariableNames: ''!
FreeImageAbstractJpegOperation subclass: #FreeImageJpegCropOperation
	instanceVariableNames: 'rectangle'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractJpegTransformationOperation subclass: #FreeImageHorizontalFlipJpegOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractJpegTransformationOperation subclass: #FreeImageRotate180JpegOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractJpegTransformationOperation subclass: #FreeImageRotate270JpegOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractJpegTransformationOperation subclass: #FreeImageRotate90JpegOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractJpegTransformationOperation subclass: #FreeImageTransposeJpegOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractJpegTransformationOperation subclass: #FreeImageTransverseJpegOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FreeImageAbstractJpegTransformationOperation subclass: #FreeImageVerticalFlipJpegOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Color class methodsFor!

fromSVGSpec: aString 
| result |
	result := self primLookupSVGColor: aString.
	^self 
		red: result first
		green: result second
		blue: result third!

fromX11Spec: aString 
| result |
	result := self primLookupX11Color: aString.
	^self 
		red: result first
		green: result second
		blue: result third!

primLookupSVGColor: szColor 
	| nRed nGreen nBlue |
	nRed := BYTE new.
	nGreen := BYTE new.
	nBlue := BYTE new.
	FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_LookupSVGColor: szColor
				nRed: nRed
				nGreen: nGreen
				nBlue: nBlue].
	^Array 
		with: nRed value
		with: nGreen value
		with: nBlue value!

primLookupX11Color: szColor

	| nRed nGreen nBlue |
	nRed := BYTE new.
	nGreen := BYTE new.
	nBlue := BYTE new.
	
	FreeImageLibrary errorCheck: [ :lib | lib freeImage_LookupX11Color: szColor nRed: nRed nGreen: nGreen nBlue: nBlue].

	^Array with: 
		 nRed value
		with: nGreen value
		with:  nBlue value! !
!Color class categoriesFor: #fromSVGSpec:!instance creation!public! !
!Color class categoriesFor: #fromX11Spec:!instance creation!public! !
!Color class categoriesFor: #primLookupSVGColor:!instance creation!primitives!private! !
!Color class categoriesFor: #primLookupX11Color:!instance creation!primitives!private! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #FreeImageConstants put: (PoolConstantsDictionary named: #FreeImageConstants)!
FreeImageConstants at: 'BMP_DEFAULT' put: 16r0!
FreeImageConstants at: 'BMP_SAVE_RLE' put: 16r1!
FreeImageConstants at: 'CUT_DEFAULT' put: 16r0!
FreeImageConstants at: 'DDS_DEFAULT' put: 16r0!
FreeImageConstants at: 'FAXG3_DEFAULT' put: 16r0!
FreeImageConstants at: 'FI_RGBA_ALPHA' put: 16r3!
FreeImageConstants at: 'FI_RGBA_ALPHA_MASK' put: -16r1000000!
FreeImageConstants at: 'FI_RGBA_ALPHA_SHIFT' put: 16r18!
FreeImageConstants at: 'FI_RGBA_BLUE' put: 16r0!
FreeImageConstants at: 'FI_RGBA_BLUE_MASK' put: 16rFF!
FreeImageConstants at: 'FI_RGBA_BLUE_SHIFT' put: 16r0!
FreeImageConstants at: 'FI_RGBA_GREEN' put: 16r1!
FreeImageConstants at: 'FI_RGBA_GREEN_MASK' put: 16rFF00!
FreeImageConstants at: 'FI_RGBA_GREEN_SHIFT' put: 16r8!
FreeImageConstants at: 'FI_RGBA_RED' put: 16r2!
FreeImageConstants at: 'FI_RGBA_RED_MASK' put: 16rFF0000!
FreeImageConstants at: 'FI_RGBA_RED_SHIFT' put: 16r10!
FreeImageConstants at: 'FI_RGBA_RGB_MASK' put: 16rFFFFFF!
FreeImageConstants at: 'FI16_555_BLUE_MASK' put: 16r1F!
FreeImageConstants at: 'FI16_555_BLUE_SHIFT' put: 16r0!
FreeImageConstants at: 'FI16_555_GREEN_MASK' put: 16r3E0!
FreeImageConstants at: 'FI16_555_GREEN_SHIFT' put: 16r5!
FreeImageConstants at: 'FI16_555_RED_MASK' put: 16r7C00!
FreeImageConstants at: 'FI16_555_RED_SHIFT' put: 16rA!
FreeImageConstants at: 'FI16_565_BLUE_MASK' put: 16r1F!
FreeImageConstants at: 'FI16_565_BLUE_SHIFT' put: 16r0!
FreeImageConstants at: 'FI16_565_GREEN_MASK' put: 16r7E0!
FreeImageConstants at: 'FI16_565_GREEN_SHIFT' put: 16r5!
FreeImageConstants at: 'FI16_565_RED_MASK' put: 16rF800!
FreeImageConstants at: 'FI16_565_RED_SHIFT' put: 16rB!
FreeImageConstants at: 'FIC_CMYK' put: 16r5!
FreeImageConstants at: 'FIC_MINISBLACK' put: 16r1!
FreeImageConstants at: 'FIC_MINISWHITE' put: 16r0!
FreeImageConstants at: 'FIC_PALETTE' put: 16r3!
FreeImageConstants at: 'FIC_RGB' put: 16r2!
FreeImageConstants at: 'FIC_RGBALPHA' put: 16r4!
FreeImageConstants at: 'FICC_ALPHA' put: 16r4!
FreeImageConstants at: 'FICC_BLACK' put: 16r5!
FreeImageConstants at: 'FICC_BLUE' put: 16r3!
FreeImageConstants at: 'FICC_GREEN' put: 16r2!
FreeImageConstants at: 'FICC_IMAG' put: 16r7!
FreeImageConstants at: 'FICC_MAG' put: 16r8!
FreeImageConstants at: 'FICC_PHASE' put: 16r9!
FreeImageConstants at: 'FICC_REAL' put: 16r6!
FreeImageConstants at: 'FICC_RED' put: 16r1!
FreeImageConstants at: 'FICC_RGB' put: 16r0!
FreeImageConstants at: 'FID_BAYER4x4' put: 16r1!
FreeImageConstants at: 'FID_BAYER8x8' put: 16r2!
FreeImageConstants at: 'FID_CLUSTER16x16' put: 16r5!
FreeImageConstants at: 'FID_CLUSTER6x6' put: 16r3!
FreeImageConstants at: 'FID_CLUSTER8x8' put: 16r4!
FreeImageConstants at: 'FID_FS' put: 16r0!
FreeImageConstants at: 'FIDT_ASCII' put: 16r2!
FreeImageConstants at: 'FIDT_BYTE' put: 16r1!
FreeImageConstants at: 'FIDT_DOUBLE' put: 16rC!
FreeImageConstants at: 'FIDT_FLOAT' put: 16rB!
FreeImageConstants at: 'FIDT_IFD' put: 16rD!
FreeImageConstants at: 'FIDT_LONG' put: 16r4!
FreeImageConstants at: 'FIDT_NOTYPE' put: 16r0!
FreeImageConstants at: 'FIDT_PALETTE' put: 16rE!
FreeImageConstants at: 'FIDT_RATIONAL' put: 16r5!
FreeImageConstants at: 'FIDT_SBYTE' put: 16r6!
FreeImageConstants at: 'FIDT_SHORT' put: 16r3!
FreeImageConstants at: 'FIDT_SLONG' put: 16r9!
FreeImageConstants at: 'FIDT_SRATIONAL' put: 16rA!
FreeImageConstants at: 'FIDT_SSHORT' put: 16r8!
FreeImageConstants at: 'FIDT_UNDEFINED' put: 16r7!
FreeImageConstants at: 'FIF_BMP' put: 16r0!
FreeImageConstants at: 'FIF_CUT' put: 16r15!
FreeImageConstants at: 'FIF_DDS' put: 16r18!
FreeImageConstants at: 'FIF_FAXG3' put: 16r1B!
FreeImageConstants at: 'FIF_GIF' put: 16r19!
FreeImageConstants at: 'FIF_HDR' put: 16r1A!
FreeImageConstants at: 'FIF_ICO' put: 16r1!
FreeImageConstants at: 'FIF_IFF' put: 16r5!
FreeImageConstants at: 'FIF_JNG' put: 16r3!
FreeImageConstants at: 'FIF_JPEG' put: 16r2!
FreeImageConstants at: 'FIF_KOALA' put: 16r4!
FreeImageConstants at: 'FIF_LBM' put: 16r5!
FreeImageConstants at: 'FIF_MNG' put: 16r6!
FreeImageConstants at: 'FIF_PBM' put: 16r7!
FreeImageConstants at: 'FIF_PBMRAW' put: 16r8!
FreeImageConstants at: 'FIF_PCD' put: 16r9!
FreeImageConstants at: 'FIF_PCX' put: 16rA!
FreeImageConstants at: 'FIF_PGM' put: 16rB!
FreeImageConstants at: 'FIF_PGMRAW' put: 16rC!
FreeImageConstants at: 'FIF_PNG' put: 16rD!
FreeImageConstants at: 'FIF_PPM' put: 16rE!
FreeImageConstants at: 'FIF_PPMRAW' put: 16rF!
FreeImageConstants at: 'FIF_PSD' put: 16r14!
FreeImageConstants at: 'FIF_RAS' put: 16r10!
FreeImageConstants at: 'FIF_SGI' put: 16r1C!
FreeImageConstants at: 'FIF_TARGA' put: 16r11!
FreeImageConstants at: 'FIF_TIFF' put: 16r12!
FreeImageConstants at: 'FIF_UNKNOWN' put: -16r1!
FreeImageConstants at: 'FIF_WBMP' put: 16r13!
FreeImageConstants at: 'FIF_XBM' put: 16r16!
FreeImageConstants at: 'FIF_XPM' put: 16r17!
FreeImageConstants at: 'FIICC_COLOR_IS_CMYK' put: 16r1!
FreeImageConstants at: 'FIICC_DEFAULT' put: 16r0!
FreeImageConstants at: 'FIJPEG_OP_FLIP_H' put: 16r1!
FreeImageConstants at: 'FIJPEG_OP_FLIP_V' put: 16r2!
FreeImageConstants at: 'FIJPEG_OP_NONE' put: 16r0!
FreeImageConstants at: 'FIJPEG_OP_ROTATE_180' put: 16r6!
FreeImageConstants at: 'FIJPEG_OP_ROTATE_270' put: 16r7!
FreeImageConstants at: 'FIJPEG_OP_ROTATE_90' put: 16r5!
FreeImageConstants at: 'FIJPEG_OP_TRANSPOSE' put: 16r3!
FreeImageConstants at: 'FIJPEG_OP_TRANSVERSE' put: 16r4!
FreeImageConstants at: 'FILTER_BICUBIC' put: 16r1!
FreeImageConstants at: 'FILTER_BILINEAR' put: 16r2!
FreeImageConstants at: 'FILTER_BOX' put: 16r0!
FreeImageConstants at: 'FILTER_BSPLINE' put: 16r3!
FreeImageConstants at: 'FILTER_CATMULLROM' put: 16r4!
FreeImageConstants at: 'FILTER_LANCZOS3' put: 16r5!
FreeImageConstants at: 'FIMD_ANIMATION' put: 16r9!
FreeImageConstants at: 'FIMD_COMMENTS' put: 16r0!
FreeImageConstants at: 'FIMD_CUSTOM' put: 16rA!
FreeImageConstants at: 'FIMD_EXIF_EXIF' put: 16r2!
FreeImageConstants at: 'FIMD_EXIF_GPS' put: 16r3!
FreeImageConstants at: 'FIMD_EXIF_INTEROP' put: 16r5!
FreeImageConstants at: 'FIMD_EXIF_MAIN' put: 16r1!
FreeImageConstants at: 'FIMD_EXIF_MAKERNOTE' put: 16r4!
FreeImageConstants at: 'FIMD_GEOTIFF' put: 16r8!
FreeImageConstants at: 'FIMD_IPTC' put: 16r6!
FreeImageConstants at: 'FIMD_NODATA' put: -16r1!
FreeImageConstants at: 'FIMD_XMP' put: 16r7!
FreeImageConstants at: 'FIQ_NNQUANT' put: 16r1!
FreeImageConstants at: 'FIQ_WUQUANT' put: 16r0!
FreeImageConstants at: 'FIT_BITMAP' put: 16r1!
FreeImageConstants at: 'FIT_COMPLEX' put: 16r8!
FreeImageConstants at: 'FIT_DOUBLE' put: 16r7!
FreeImageConstants at: 'FIT_FLOAT' put: 16r6!
FreeImageConstants at: 'FIT_INT16' put: 16r3!
FreeImageConstants at: 'FIT_INT32' put: 16r5!
FreeImageConstants at: 'FIT_RGB16' put: 16r9!
FreeImageConstants at: 'FIT_RGBA16' put: 16rA!
FreeImageConstants at: 'FIT_RGBAF' put: 16rC!
FreeImageConstants at: 'FIT_RGBF' put: 16rB!
FreeImageConstants at: 'FIT_UINT16' put: 16r2!
FreeImageConstants at: 'FIT_UINT32' put: 16r4!
FreeImageConstants at: 'FIT_UNKNOWN' put: 16r0!
FreeImageConstants at: 'FITMO_DRAGO03' put: 16r0!
FreeImageConstants at: 'FITMO_REINHARD05' put: 16r1!
FreeImageConstants at: 'GIF_DEFAULT' put: 16r0!
FreeImageConstants at: 'GIF_LOAD256' put: 16r1!
FreeImageConstants at: 'GIF_PLAYBACK' put: 16r2!
FreeImageConstants at: 'HDR_DEFAULT' put: 16r0!
FreeImageConstants at: 'ICO_DEFAULT' put: 16r0!
FreeImageConstants at: 'ICO_MAKEALPHA' put: 16r1!
FreeImageConstants at: 'IFF_DEFAULT' put: 16r0!
FreeImageConstants at: 'JPEG_ACCURATE' put: 16r2!
FreeImageConstants at: 'JPEG_CMYK' put: 16r1000!
FreeImageConstants at: 'JPEG_DEFAULT' put: 16r0!
FreeImageConstants at: 'JPEG_FAST' put: 16r1!
FreeImageConstants at: 'JPEG_PROGRESSIVE' put: 16r2000!
FreeImageConstants at: 'JPEG_QUALITYAVERAGE' put: 16r400!
FreeImageConstants at: 'JPEG_QUALITYBAD' put: 16r800!
FreeImageConstants at: 'JPEG_QUALITYGOOD' put: 16r100!
FreeImageConstants at: 'JPEG_QUALITYNORMAL' put: 16r200!
FreeImageConstants at: 'JPEG_QUALITYSUPERB' put: 16r80!
FreeImageConstants at: 'KOALA_DEFAULT' put: 16r0!
FreeImageConstants at: 'LBM_DEFAULT' put: 16r0!
FreeImageConstants at: 'MNG_DEFAULT' put: 16r0!
FreeImageConstants at: 'PCD_BASE' put: 16r1!
FreeImageConstants at: 'PCD_BASEDIV16' put: 16r3!
FreeImageConstants at: 'PCD_BASEDIV4' put: 16r2!
FreeImageConstants at: 'PCD_DEFAULT' put: 16r0!
FreeImageConstants at: 'PCX_DEFAULT' put: 16r0!
FreeImageConstants at: 'PNG_DEFAULT' put: 16r0!
FreeImageConstants at: 'PNG_IGNOREGAMMA' put: 16r1!
FreeImageConstants at: 'PNM_DEFAULT' put: 16r0!
FreeImageConstants at: 'PNM_SAVE_ASCII' put: 16r1!
FreeImageConstants at: 'PNM_SAVE_RAW' put: 16r0!
FreeImageConstants at: 'PSD_DEFAULT' put: 16r0!
FreeImageConstants at: 'RAS_DEFAULT' put: 16r0!
FreeImageConstants at: 'SEEK_CUR' put: 16r1!
FreeImageConstants at: 'SEEK_END' put: 16r2!
FreeImageConstants at: 'SEEK_SET' put: 16r0!
FreeImageConstants at: 'SGI_DEFAULT' put: 16r0!
FreeImageConstants at: 'TARGA_DEFAULT' put: 16r0!
FreeImageConstants at: 'TARGA_LOAD_RGB888' put: 16r1!
FreeImageConstants at: 'TIFF_ADOBE_DEFLATE' put: 16r400!
FreeImageConstants at: 'TIFF_CCITTFAX3' put: 16r1000!
FreeImageConstants at: 'TIFF_CCITTFAX4' put: 16r2000!
FreeImageConstants at: 'TIFF_CMYK' put: 16r1!
FreeImageConstants at: 'TIFF_DEFAULT' put: 16r0!
FreeImageConstants at: 'TIFF_DEFLATE' put: 16r200!
FreeImageConstants at: 'TIFF_JPEG' put: 16r8000!
FreeImageConstants at: 'TIFF_LZW' put: 16r4000!
FreeImageConstants at: 'TIFF_NONE' put: 16r800!
FreeImageConstants at: 'TIFF_PACKBITS' put: 16r100!
FreeImageConstants at: 'WBMP_DEFAULT' put: 16r0!
FreeImageConstants at: 'XBM_DEFAULT' put: 16r0!
FreeImageConstants at: 'XPM_DEFAULT' put: 16r0!
FreeImageConstants shrink!

"Classes"!

FreeImageAbstractBitmapOperation guid: (GUID fromString: '{414A7193-7425-4F71-B6CA-34DE11633BC3}')!
FreeImageAbstractBitmapOperation comment: ''!
!FreeImageAbstractBitmapOperation categoriesForClass!Unclassified! !
!FreeImageAbstractBitmapOperation methodsFor!

basicProcess: aFreeImageBitmap 
	"Returns an FIBITMAP"

	^self subclassResponsibility!

initialize
!

process: aFreeImageBitmap 
	^self processNonDestructive: aFreeImageBitmap!

processDestructive: aFreeImageBitmap 
	self subclassResponsibility!

processNonDestructive: aFreeImageBitmap 
	self subclassResponsibility! !
!FreeImageAbstractBitmapOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageAbstractBitmapOperation categoriesFor: #initialize!initializing!private! !
!FreeImageAbstractBitmapOperation categoriesFor: #process:!actions!conversion!public! !
!FreeImageAbstractBitmapOperation categoriesFor: #processDestructive:!actions!conversion!public! !
!FreeImageAbstractBitmapOperation categoriesFor: #processNonDestructive:!actions!conversion!public! !

!FreeImageAbstractBitmapOperation class methodsFor!

new
^super new initialize!

process: aFreeImageBitmap
^self new process: aFreeImageBitmap! !
!FreeImageAbstractBitmapOperation class categoriesFor: #new!instance creation!public! !
!FreeImageAbstractBitmapOperation class categoriesFor: #process:!instance creation!public! !

FreeImageAbstractJpegOperation guid: (GUID fromString: '{86D0ED68-DE96-411D-A7EF-CB65B99AFB9B}')!
FreeImageAbstractJpegOperation comment: ''!
!FreeImageAbstractJpegOperation categoriesForClass!Unclassified! !
!FreeImageAbstractJpegOperation methodsFor!

destination
	^destination!

destination: aFilename 
	destination := aFilename!

initialize
!

process
	self subclassResponsibility!

source
	^source!

source: aFilename 
	source := aFilename! !
!FreeImageAbstractJpegOperation categoriesFor: #destination!accessing!public! !
!FreeImageAbstractJpegOperation categoriesFor: #destination:!accessing!public! !
!FreeImageAbstractJpegOperation categoriesFor: #initialize!initializing!private! !
!FreeImageAbstractJpegOperation categoriesFor: #process!actions!conversion!public! !
!FreeImageAbstractJpegOperation categoriesFor: #source!accessing!public! !
!FreeImageAbstractJpegOperation categoriesFor: #source:!accessing!public! !

!FreeImageAbstractJpegOperation class methodsFor!

icon
	^##(TypeConverter icon)!

new
^super new initialize!

processSource: sourceFilename destination: destinatioFilename 
^(self source: sourceFilename destination: destinatioFilename) process; yourself!

source: sourceFilename destination: destinatioFilename 
	^(self new)
		source: sourceFilename;
		destination: destinatioFilename;
		yourself! !
!FreeImageAbstractJpegOperation class categoriesFor: #icon!development!public! !
!FreeImageAbstractJpegOperation class categoriesFor: #new!instance creation!public! !
!FreeImageAbstractJpegOperation class categoriesFor: #processSource:destination:!instance creation!public! !
!FreeImageAbstractJpegOperation class categoriesFor: #source:destination:!instance creation!public! !

FreeImageBitmap guid: (GUID fromString: '{E890EF87-70E1-47CA-8CBB-D9E2B65DD2FF}')!
FreeImageBitmap comment: ''!
!FreeImageBitmap categoriesForClass!Unclassified! !
!FreeImageBitmap methodsFor!

asBitmap
	| hBitmap |
	hBitmap := GDILibrary default 
				createDIBitmap: UserLibrary default getDC
				lpbmih: self infoHeader
				fdwInit: CBM_INIT
				lpbInit: self bits
				lpbmi: self info
				fuUsage: DIB_RGB_COLORS.
	^Bitmap fromOwnedHandle: hBitmap!

asByteArray
	^self asByteArray: loadFormat!

asByteArray: aFreeImageFormat
	| stream |
	stream := FreeImageMemoryStream open.
	self saveToMemory: stream format: aFreeImageFormat.
	^stream contents!

asGdiplusBitmap
	| image data |
	image := GdiplusBitmap extent: self extent.
	data := GpBitmapData new.
	image 
		lockBits: (RECT fromRectangle: (0 @ 0 extent: image extent))
		flags: 1
		format: PixelFormat32bppARGB
		lockedBitmapData: data.
	self 
		toRawBits: data Scan0
		extent: self extent
		pitch: data Stride
		depth: self depth
		redMask: FI_RGBA_RED
		greenMask: FI_RGBA_GREEN
		blueMask: FI_RGBA_BLUE.
	image unlockBits: data.
	^image!

asParameter
	"Answer the receiver in a form suitable for an API call."

	^self handle!

backgroundColor
	^self primGetBackgroundColor asColor!

backgroundColor: aColor 
	self primSetBackgroundColor: aColor!

basicFree
	"Private - Free up external resources held by the receiver.
	Ignore any errors which might arise."

	self primUnload!

beOpaque
	^self hasTransparency: false!

beTransparent
	^self hasTransparency: true!

bits
	^self primGetBits!

blueMask
	^self primGetBlueMask!

bpp
	"Returns the size of one pixel in the bitmap in bits. For example when each pixel takes 32-bits
	of space in the bitmap, this function returns 32. Possible bit depths are 1, 4, 8, 16, 24, 32 for
	standard bitmaps and 16-, 32-, 48-, 64-, 96- and 128-bit for non standard bitmaps."

	^self primGetBPP!

clearCached
	"Private - Clear down the receiver's cached information and handles.
	Should be overridden by subclasses which wish to clear down other
	handles and cached information."

	handle := nil!

colorsCount
	^self primGetColorsUsed!

colorType
	^self primGetColorType!

comments
^FreeImageMetadata commentsFromFreeImageBitmap: self.!

copy
	^(super copy)
		ownedHandle: self copyHandle;
		yourself!

copyHandle
	"Private - Answer an external handle to the image which is a copy of that represented by the receiver."

	^self primClone!

depth
^self bpp!

detachHandle
	"Answer the receiver's handle, and it is owned, relinquish ownership
	to the caller."

	| hTool |
	hTool := self handle.	"May realize the object"
	ownsHandle ifTrue: [self beUnfinalizable].
	self clearCached.
	^hTool!

dibSize
	"Returns the size of the DIB-element of a FIBITMAP in memory, i.e. the
	BITMAPINFOHEADER + palette + data bits 
	(note that this is not the real size of a FIBITMAP, only the size of its DIB-element)."

	^self primGetDIBSize!

dotsPerInch
^(self dotsPerMeter  / 100 * 2.54) truncated!

dotsPerInch: aPoint 
	^self dotsPerMeter: (aPoint / 2.54 * 100) ceiling!

dotsPerMeter
	^(self primGetDotsPerMeterX @ self) primGetDotsPerMeterY!

dotsPerMeter: aPoint 
self primSetDotsPerMeterX: aPoint x; primSetDotsPerMeterY: aPoint y!

dpi
^self dotsPerInch!

dpi: aPoint
	^self dotsPerInch: aPoint!

drawOn: aCanvas
	"Draw the receiver to aCanvas at position aPoint (no stretching).
	Answer whether the operation succeeded."

	^self drawOn: aCanvas at: Point zero!

drawOn: aCanvas at: aPoint
	"Draw the receiver to aCanvas at position aPoint (no stretching).
	Answer whether the operation succeeded."

	^self drawOn: aCanvas at: aPoint extent: self extent!

drawOn: aCanvas at: dstOrigin extent: dstExtent 
	"Copies the receiver to aCanvas at the position specified by the Point, dstOrigin,
	and the with size specified by the Point, dstExtent. The receiver is stretched
	to fit the destination rectangle."

	GDILibrary default 
		stretchDIBits: aCanvas asParameter
		xDest: dstOrigin x
		yDest: dstOrigin y
		nDestWidth: dstExtent x
		nDestHeight: dstExtent y
		xSrc: 0
		ySrc: 0
		nSrcWidth: self width
		nSrcHeight: self height
		lpBits: self bits
		lpBitsInfo: self info
		iUsage: DIB_RGB_COLORS
		dwRop: SRCCOPY!

extent
	^self width @self height!

free
	"Free external resources held by the receiver, and leave in a state such
	that the receiver will be re-realized the next time it is accessed."

	(self ownsHandle) 
		ifTrue: 
			[self beUnfinalizable.
			self basicFree].
	handle := nil!

freeDC
	"Private - Free the receiver's cached memory DC, if any."

	!

greenMask
	^self primGetGreenMask!

handle
	^handle!

handle: aHandle 
handle := aHandle!

hasAlpha
^self isRGBA!

hasBackgroundColor
	^self primHasBackgroundColor !

hasPalette
^self colorType = FIC_PALETTE!

hasRGB
^self colorType = FIC_RGB or: [self colorType = FIC_RGBALPHA].!

hasTransparency
	^self primIsTransparent !

hasTransparency: aBoolean 
	^self primSetTransparent: aBoolean!

height
	^self primGetHeight!

histogram
^self histogram: FICC_BLACK!

histogram: aFICHANNEL 
^self primGetHistogram: aFICHANNEL!

iccProfile
	^FreeImageICCProfile fromFreeImageBitmap: self!

iccProfile: aFreeImageICCProfile 
	aFreeImageICCProfile createIn: self!

info
	^self primGetInfo!

infoHeader
	^self primGetInfoHeader!

initialize
	"Initialize the receiver's instance variables."

	ownsHandle := true.	"By default instances will free their handles on finalization"
	loadFormat := FreeImageFormat unknown!

isCMYK
^self colorType = FIC_CMYK!

isGreyscale
^self colorType = FIC_MINISBLACK or: [self colorType = FIC_MINISWHITE]!

isRGB
^self colorType = FIC_RGB!

isRGBA
	^self colorType = FIC_RGBALPHA!

line
	"Returns the width of the bitmap in bytes.
	See also: #pitch.

	There has been some criticism on the name of this function. Some people expect it to
	return a scanline in the pixel data, while it actually returns the width of the bitmap in
	bytes. As far as I know the term Line is common terminology for the width of a bitmap
	in bytes. It is at least used by Microsoft DirectX."

	^self primGetLine!

loadFormat
	^loadFormat!

loadFormat: aFreeImageFormat 
	loadFormat := aFreeImageFormat!

onStartup
	handle := nil.
	loadFormat := FreeImageFormat unknown!

ownedHandle: aHandle
	"Private - Set the handle of the external graphics's tool object represented and owned by
	the receiver to be the argument."

	self handle: aHandle.
	ownsHandle := true.
	self beFinalizable!

ownsHandle
	"Answer whether the receiver owns the handle it is holding"

	^ownsHandle!

ownsHandle: aBoolean
	"Private - Record whether the receiver owns the handle it is holding, and will
	therefore release it when finalized. Answer the receiver."

	ownsHandle := aBoolean!

palette
	|  |
	^StructureArray 
		fromAddress: self primGetPalette
		length: self primGetColorsUsed
		elementClass: RGBQUAD!

pitch
	"Returns the width of the bitmap in bytes, rounded to the next 32-bit boundary, also known as
	pitch or stride or scan width.
	In FreeImage each scanline starts at a 32-bit boundary for performance reasons.
	This accessor in essential when using low level pixel manipulation functions (see also
	the chapter on Pixel access functions)."

	^self primGetPitch!

pixelColor: aPoint 
	^(self primGetPixelColor: aPoint) !

pixelColorAt: aPoint put: aColor 
	^FreeImageLibrary default 
		freeImage_SetPixelColor: self asParameter
		x: aPoint x
		y: aPoint y
		value: (RGBQUAD fromColor: aColor)!

pixelIndexAt: aPoint 
^self primGetPixelIndex: aPoint!

pixelIndexAt: aPoint put: anInteger 
	^FreeImageLibrary default 
		freeImage_SetPixelColor: self asParameter
		x: aPoint x
		y: aPoint y
		value: anInteger!

primClone
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_Clone: self asParameter ]!

primConvertToRawBits: anExternalAddress
		
		pitch: pitch
		bpp: bpp
		red_mask: redMask
		green_mask: greenMask
		blue_mask: blueMask
		topdown: topdown
		
	^FreeImageLibrary errorCheck: [ :lib | lib  
		freeImage_ConvertToRawBits: anExternalAddress
		dib: self asParameter
		pitch: pitch
		bpp: bpp
		red_mask: redMask
		green_mask: greenMask
		blue_mask: blueMask
		topdown: topdown]!

primDestroyICCProfile
	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_DestroyICCProfile: self asParameter]!

primGetBackgroundColor
	| bkcolor |
	bkcolor := RGBQUAD new.
	FreeImageLibrary 
		errorCheck: [:lib | lib freeImage_GetBackgroundColor: self asParameter bkcolor: bkcolor].
		^bkcolor!

primGetBits
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetBits: self asParameter].!

primGetBlueMask

	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetBlueMask: self asParameter]!

primGetBPP


	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetBPP: self asParameter]!

primGetColorsUsed
	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetColorsUsed: self asParameter]!

primGetColorType
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetColorType: self asParameter]!

primGetDIBSize


	^FreeImageLibrary errorCheck: [ :lib | lib   freeImage_GetDIBSize: self asParameter]!

primGetDotsPerMeterX
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetDotsPerMeterX: self asParameter]!

primGetDotsPerMeterY
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetDotsPerMeterY: self asParameter]!

primGetGreenMask
	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetGreenMask: self asParameter]!

primGetHeight

	^FreeImageLibrary errorCheck: [:lib | lib  freeImage_GetHeight: self asParameter]!

primGetHistogram: channel
	| histo |
	histo := StructureArray length: 256 elementClass: DWORD.
FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetHistogram: self asParameter histo: histo channel: channel].

^histo!

primGetImageType
	^FreeImageLibrary errorCheck:  [ :lib | lib freeImage_GetImageType: self asParameter]!

primGetInfo

	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetInfo: self asParameter]!

primGetInfoHeader
	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetInfoHeader: self asParameter]!

primGetLine


	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetLine: self asParameter]!

primGetPalette
	^ FreeImageLibrary errorCheck: [ :lib | lib  freeImage_GetPalette: self asParameter].
	!

primGetPitch

	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetPitch: self asParameter]!

primGetPixelColor: pos 
	| rgbQuad |
	rgbQuad := RGBQUAD new.
	FreeImageLibrary 
		errorCheck: 
			[:lib | 
			lib 
				freeImage_GetPixelColor: self asParameter
				x: pos x
				y: pos y
				value: rgbQuad] 
	.
	^rgbQuad asColor!

primGetPixelIndex: pos 
	| byte  |
	byte := BYTE new.
	FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_GetPixelIndex: self asParameter
				x: pos x
				y: pos y
				value: byte].
	^byte value!

primGetRedMask
	^FreeImageLibrary errorCheck: [ :lib | lib  freeImage_GetRedMask: self asParameter]!

primGetScanLine: scanline
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetScanLine: self asParameter scanline: scanline]!

primGetTransparencyCount
	^FreeImageLibrary errorCheck: [ :lib | lib  freeImage_GetTransparencyCount: self asParameter]!

primGetTransparencyTable
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetTransparencyTable: self asParameter]!

primGetWidth
	^FreeImageLibrary errorCheck: [ :lib | lib  freeImage_GetWidth: self asParameter]!

primHasBackgroundColor
	^(FreeImageLibrary errorCheck: [:lib | lib freeImage_HasBackgroundColor: self asParameter]) asBoolean!

primIsTransparent
	^(FreeImageLibrary errorCheck: [:lib | lib freeImage_IsTransparent: self asParameter]) asBoolean!

primSave: aFreeImageFormat filename: aFilename flags: flags 
	FreeImageLibrary errorCheck:  
			[:lib | 
			lib 
				freeImage_Save: aFreeImageFormat asParameter
				dib: self asParameter
				filename: aFilename
				flags: flags]!

primSaveToMemory: fif stream: stream flags: flags
	^FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_SaveToMemory: fif asParameter
				dib: self asParameter
				stream: stream asParameter
				flags: flags
				 ]!

primSetBackgroundColor: bkcolor 
	^FreeImageLibrary 
		errorCheck: [:lib | lib freeImage_SetBackgroundColor: self asParameter bkcolor: ( RGBQUAD fromColor: bkcolor)]!

primSetDotsPerMeterX: res
FreeImageLibrary default freeImage_SetDotsPerMeterX: self asParameter res: res!

primSetDotsPerMeterY: res 
	FreeImageLibrary default  freeImage_SetDotsPerMeterY: self asParameter res: res!

primSetTransparencyTable: table 
	^FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_SetTransparencyTable: self asParameter
				table: table
				count: table size]!

primSetTransparent: enabled


	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_SetTransparent: self asParameter enabled: enabled asParameter ]!

primUnload


	FreeImageLibrary default freeImage_Unload: self asParameter!

redMask
	^self primGetRedMask!

removeIccProfile
	self primDestroyICCProfile!

saveToFile: aFilename 
	^self saveToFile: aFilename format: loadFormat!

saveToFile: aFilename format: aFreeImageFormat 
	^self 
		saveToFile: aFilename
		format: aFreeImageFormat
		flags: aFreeImageFormat flags!

saveToFile: aFilename format: aFreeImageFormat flags: flags 
	^(aFreeImageFormat isUnknown not and: [aFreeImageFormat supportsWriting]) 
		ifTrue: 
			[self 
				primSave: aFreeImageFormat
				filename: aFilename
				flags: flags]
		ifFalse: [FreeImageError signal: 'File ' , aFilename , ' cannot be saved (format)']!

saveToMemory: aFreeImageBitmapStream 
	^self saveToMemory: aFreeImageBitmapStream format: loadFormat!

saveToMemory: aFreeImageBitmapStream format: aFreeImageFormat 
	^self 
		saveToMemory: aFreeImageBitmapStream
		format: aFreeImageFormat
		flags: aFreeImageFormat flags!

saveToMemory: aFreeImageBitmapStream format: aFreeImageFormat flags: flags 
	^(aFreeImageFormat isUnknown not and: [aFreeImageFormat supportsWriting]) 
		ifTrue: 
			[
			self 
				primSaveToMemory: aFreeImageFormat
				stream: aFreeImageBitmapStream
				flags: flags
					]
		ifFalse: [FreeImageError signal: 'Could not write to memory (format)']!

scanLine: anInteger 
	^self primGetScanLine: anInteger!

toRawBits: anExternalAddress extent: extent pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask 
	^self 
		toRawBits: anExternalAddress
		width: extent x
		height: extent y pitch: pitch
		depth: bpp
		redMask: redMask
		greenMask: greenMask
		blueMask: blueMask!

toRawBits: anExternalAddress width: width height: height pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask 
^self toRawBits: anExternalAddress width: width height: height pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask topdown: false!

toRawBits: anExternalAddress width: width height: height pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask topdown: topdown 
	^self 
		primConvertToRawBits: anExternalAddress
		pitch: pitch
		bpp: bpp
		red_mask: redMask
		green_mask: greenMask
		blue_mask: blueMask
		topdown: topdown!

transparencyCount
	^self primGetTransparencyCount!

transparencyTable
	^ByteArray fromAddress: self primGetTransparencyTable length: self primGetTransparencyCount!

transparencyTable: aByteArray 
	self primSetTransparencyTable: aByteArray!

type
	^self primGetImageType!

width
	^self primGetWidth! !
!FreeImageBitmap categoriesFor: #asBitmap!converting!public! !
!FreeImageBitmap categoriesFor: #asByteArray!conversion!public! !
!FreeImageBitmap categoriesFor: #asByteArray:!conversion!public! !
!FreeImageBitmap categoriesFor: #asGdiplusBitmap!converting!public! !
!FreeImageBitmap categoriesFor: #asParameter!converting!public! !
!FreeImageBitmap categoriesFor: #backgroundColor!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #backgroundColor:!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #basicFree!bitmap management!private!realizing/unrealizing! !
!FreeImageBitmap categoriesFor: #beOpaque!bitmap information!modes!public! !
!FreeImageBitmap categoriesFor: #beTransparent!bitmap information!modes!public! !
!FreeImageBitmap categoriesFor: #bits!accessing!pixel access!public! !
!FreeImageBitmap categoriesFor: #blueMask!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #bpp!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #clearCached!private!realizing/unrealizing! !
!FreeImageBitmap categoriesFor: #colorsCount!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #colorType!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #comments!*-in class package!metadata!public! !
!FreeImageBitmap categoriesFor: #copy!bitmap management!copying!public! !
!FreeImageBitmap categoriesFor: #copyHandle!bitmap management!private!realizing/unrealizing! !
!FreeImageBitmap categoriesFor: #depth!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #detachHandle!accessing!public! !
!FreeImageBitmap categoriesFor: #dibSize!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #dotsPerInch!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #dotsPerInch:!*-in class package!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #dotsPerMeter!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #dotsPerMeter:!*-in class package!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #dpi!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #dpi:!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #drawOn:!drawing-bitmaps!public! !
!FreeImageBitmap categoriesFor: #drawOn:at:!drawing-bitmaps!public! !
!FreeImageBitmap categoriesFor: #drawOn:at:extent:!drawing-bitmaps!public! !
!FreeImageBitmap categoriesFor: #extent!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #free!public!realizing/unrealizing! !
!FreeImageBitmap categoriesFor: #freeDC!private!realizing/unrealizing! !
!FreeImageBitmap categoriesFor: #greenMask!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #handle!accessing!public! !
!FreeImageBitmap categoriesFor: #handle:!accessing!private! !
!FreeImageBitmap categoriesFor: #hasAlpha!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #hasBackgroundColor!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #hasPalette!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #hasRGB!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #hasTransparency!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #hasTransparency:!accessing!bitmap information!modes!public! !
!FreeImageBitmap categoriesFor: #height!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #histogram!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #histogram:!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #iccProfile!ICC profile!public! !
!FreeImageBitmap categoriesFor: #iccProfile:!ICC profile!public! !
!FreeImageBitmap categoriesFor: #info!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #infoHeader!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #initialize!initializing!private! !
!FreeImageBitmap categoriesFor: #isCMYK!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #isGreyscale!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #isRGB!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #isRGBA!bitmap information!public!testing! !
!FreeImageBitmap categoriesFor: #line!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #loadFormat!accessing!initializing!public! !
!FreeImageBitmap categoriesFor: #loadFormat:!accessing!initializing!private! !
!FreeImageBitmap categoriesFor: #onStartup!event handling!private! !
!FreeImageBitmap categoriesFor: #ownedHandle:!accessing!private! !
!FreeImageBitmap categoriesFor: #ownsHandle!public!testing! !
!FreeImageBitmap categoriesFor: #ownsHandle:!private!testing! !
!FreeImageBitmap categoriesFor: #palette!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #pitch!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #pixelColor:!accessing!pixel access!public! !
!FreeImageBitmap categoriesFor: #pixelColorAt:put:!accessing!pixel access!public! !
!FreeImageBitmap categoriesFor: #pixelIndexAt:!accessing!pixel access!public! !
!FreeImageBitmap categoriesFor: #pixelIndexAt:put:!accessing!pixel access!public! !
!FreeImageBitmap categoriesFor: #primClone!bitmap management!primitives!private!realizing/unrealizing! !
!FreeImageBitmap categoriesFor: #primConvertToRawBits:pitch:bpp:red_mask:green_mask:blue_mask:topdown:!conversion!public! !
!FreeImageBitmap categoriesFor: #primDestroyICCProfile!ICC profile!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetBackgroundColor!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetBits!accessing!pixel access!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetBlueMask!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetBPP!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetColorsUsed!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetColorType!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetDIBSize!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetDotsPerMeterX!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetDotsPerMeterY!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetGreenMask!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetHeight!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetHistogram:!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetImageType!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetInfo!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetInfoHeader!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetLine!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetPalette!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetPitch!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetPixelColor:!accessing!pixel access!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetPixelIndex:!accessing!pixel access!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetRedMask!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetScanLine:!accessing!pixel access!public! !
!FreeImageBitmap categoriesFor: #primGetTransparencyCount!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetTransparencyTable!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primGetWidth!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primHasBackgroundColor!bitmap information!primitives!private!testing! !
!FreeImageBitmap categoriesFor: #primIsTransparent!bitmap information!primitives!private!testing! !
!FreeImageBitmap categoriesFor: #primSave:filename:flags:!bitmap management!file operations!primitives!private! !
!FreeImageBitmap categoriesFor: #primSaveToMemory:stream:flags:!bitmap management!public! !
!FreeImageBitmap categoriesFor: #primSetBackgroundColor:!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primSetDotsPerMeterX:!*-in class package!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primSetDotsPerMeterY:!*-in class package!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primSetTransparencyTable:!accessing!bitmap information!primitives!private! !
!FreeImageBitmap categoriesFor: #primSetTransparent:!accessing!bitmap information!modes!primitives!private! !
!FreeImageBitmap categoriesFor: #primUnload!bitmap management!primitives!private!realizing/unrealizing! !
!FreeImageBitmap categoriesFor: #redMask!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #removeIccProfile!ICC profile!public! !
!FreeImageBitmap categoriesFor: #saveToFile:!bitmap management!file operations!public! !
!FreeImageBitmap categoriesFor: #saveToFile:format:!bitmap management!file operations!public! !
!FreeImageBitmap categoriesFor: #saveToFile:format:flags:!bitmap management!file operations!public! !
!FreeImageBitmap categoriesFor: #saveToMemory:!bitmap management!public! !
!FreeImageBitmap categoriesFor: #saveToMemory:format:!bitmap management!public! !
!FreeImageBitmap categoriesFor: #saveToMemory:format:flags:!bitmap management!public! !
!FreeImageBitmap categoriesFor: #scanLine:!accessing!pixel access!public! !
!FreeImageBitmap categoriesFor: #toRawBits:extent:pitch:depth:redMask:greenMask:blueMask:!conversion!public! !
!FreeImageBitmap categoriesFor: #toRawBits:width:height:pitch:depth:redMask:greenMask:blueMask:!conversion!public! !
!FreeImageBitmap categoriesFor: #toRawBits:width:height:pitch:depth:redMask:greenMask:blueMask:topdown:!conversion!public! !
!FreeImageBitmap categoriesFor: #transparencyCount!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #transparencyTable!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #transparencyTable:!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #type!accessing!bitmap information!public! !
!FreeImageBitmap categoriesFor: #width!accessing!bitmap information!public! !

!FreeImageBitmap class methodsFor!

extent: extent depth: bpp 
	^self 
		width: extent x
		height: extent y
		depth: bpp!

fromBitmap: aBitmap 
	| fi dib |
	fi := aBitmap getInfo.
	dib := self 
				width: fi bmWidth
				height: fi bmHeight
				depth: fi bmBitsPixel.
	GDILibrary default 
		getDIBits: UserLibrary default getDC
		hbm: aBitmap asParameter
		uStartScan: 0
		cScanLines: dib height
		lpvBits: dib bits
		lpbi: dib info
		uUsage: DIB_RGB_COLORS.
	^dib!

fromByteArray: aByteArray

^self fromMemory: (FreeImageMemoryStream  open: aByteArray yourAddress size: aByteArray size)!

fromFile: aFilename 
	^self fromFile: aFilename format: (FreeImageFormat fromFile: aFilename)!

fromFile: aFilename format: aFreeImageFormat 
	^self 
		fromFile: aFilename
		format: aFreeImageFormat
		flags: aFreeImageFormat flags!

fromFile: aFilename format: aFreeImageFormat flags: flags 
	^(aFreeImageFormat isUnknown not and: [aFreeImageFormat supportsReading]) 
		ifTrue: 
			[self fromOwnedHandle: (self 
						primLoad: aFreeImageFormat
						filename: aFilename
						flags: flags)
				loadFormat: aFreeImageFormat]
		ifFalse: [FreeImageError signal: 'File ' , aFilename , ' cannot be loaded (format)']!

fromHandle: aHandle 
	"Answers an instance of the receiver with aHandle. The handle is not
	owned by the instance and will not therefore be freed by it."

	^(self new)
		ownsHandle: false;
		handle: aHandle; yourself!

fromMemory: aFreeImageMemoryStream 
	^self fromMemory: aFreeImageMemoryStream
		format: (FreeImageFormat fromMemory: aFreeImageMemoryStream)!

fromMemory: aFreeImageMemoryStream format: aFreeImageFormat 
	^self 
		fromMemory: aFreeImageMemoryStream
		format: aFreeImageFormat
		flags: aFreeImageFormat flags!

fromMemory: aFreeImageMemoryStream format: aFreeImageFormat flags: flags 
	^(aFreeImageFormat isUnknown not and: [aFreeImageFormat supportsReading]) 
		ifTrue: 
			[self fromOwnedHandle: (FreeImageLibrary default 
						freeImage_LoadFromMemory: aFreeImageFormat asParameter
						stream: aFreeImageMemoryStream asParameter
						flags: flags)
				loadFormat: aFreeImageFormat]
		ifFalse: [FreeImageError signal: 'Could not load from memory (format)']!

fromOwnedHandle: aFIBITMAP 
	"Answers an instance of the receiver with aHandle. The handle is owned by the instance and
	will therefore be freed by it."

	^(self new)
		ownedHandle: aFIBITMAP;
		yourself!

fromOwnedHandle: aFIBITMAP loadFormat: aFreeImageFormat 
	"Answers an instance of the receiver with aHandle. The handle is owned by the instance and
	will therefore be freed by it."

	^(self new)
		ownedHandle: aFIBITMAP;
		loadFormat: aFreeImageFormat;
		yourself!

fromRawBits: anExternalAddress extent: extent pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask 

^self fromRawBits: anExternalAddress width: extent x height: extent y pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask !

fromRawBits: anExternalAddress width: width height: height pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask 
^self fromRawBits: anExternalAddress width: width height: height pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask topdown: false 
!

fromRawBits: anExternalAddress width: width height: height pitch: pitch depth: bpp redMask: redMask greenMask: greenMask blueMask: blueMask topdown: topdown 
	^self fromOwnedHandle: (FreeImageLibrary default 
				freeImage_ConvertFromRawBits: anExternalAddress
				width: width
				height: height
				pitch: pitch
				bpp: bpp
				red_mask: redMask
				green_mask: greenMask
				blue_mask: blueMask
				topdown: topdown)!

icon
	^##(Bitmap icon)!

initialize
	"Private - Register the receiver with the #onStartup event"

	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self.
	!

new
	"Answer a new, properly initialized, instance of the receiver."

	^super new initialize!

newInstanceAspect: aSymbol class: aspectClass 
	"Private - Answer a new <Aspect> of the class, aspectClass, and with name, aSymbol, 
    	which is appropriate for representing aspects of the receiver's type."

	^aspectClass bitmap: aSymbol!

onStartup
	self allInstances do: [ :each | each onStartup]!

primAllocate: width height: height bpp: bpp 
	^FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_Allocate: width
				height: height
				bpp: bpp]!

primAllocateT: fif width: width height: height bpp: bpp 
	^FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_AllocateT: fif asParameter
				width: width
				height: height
				bpp: bpp]!

primLoad: aFreeImageFormat filename: aFilename flags: flags 
	^FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_Load: aFreeImageFormat asParameter
				filename: aFilename
				flags: flags]!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	"	^(super publishedAspectsOfInstances)
		add: (Aspect autostring: #text);
		add: (Aspect integer: #width);
		add: (Aspect block: #getContentsBlock);
		add: (Aspect block: #getImageBlock);
		add: (Aspect block: #getTextBlock);
		add: (Aspect block: #sortBlock);
		add: (Aspect choice: #alignment from: #(#left #center #right));
		add: (Aspect boolean: #isAutoResize);
		add: (Aspect block: #getInfoTipBlock);
		add: (Aspect block: #customDrawBlock);
		yourself"

	^(super publishedAspectsOfInstances)
		add: (Aspect 
					choice: #type
					fromMap: FreeImageLibrary typeConstants
					nullValue: nil);
		add: (Aspect 
					choice: #colorType
					fromMap: FreeImageLibrary colorTypeConstants
					nullValue: nil);
		add: (Aspect name: #loadFormat);
		yourself!

type: fif extent: extent depth: bpp 
	^self type: fif
		width: extent x
		height: extent y
		depth: bpp!

type: fif width: width height: height depth: bpp 
	^self fromOwnedHandle: (self 
				primAllocateT: fif
				width: width
				height: height
				bpp: bpp)!

width: width height: height depth: bpp 
	^self fromOwnedHandle: (self 
				primAllocate: width
				height: height
				bpp: bpp)! !
!FreeImageBitmap class categoriesFor: #extent:depth:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromBitmap:!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromByteArray:!public! !
!FreeImageBitmap class categoriesFor: #fromFile:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromFile:format:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromFile:format:flags:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromHandle:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromMemory:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromMemory:format:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromMemory:format:flags:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromOwnedHandle:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromOwnedHandle:loadFormat:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromRawBits:extent:pitch:depth:redMask:greenMask:blueMask:!conversion!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromRawBits:width:height:pitch:depth:redMask:greenMask:blueMask:!conversion!instance creation!public! !
!FreeImageBitmap class categoriesFor: #fromRawBits:width:height:pitch:depth:redMask:greenMask:blueMask:topdown:!conversion!instance creation!public! !
!FreeImageBitmap class categoriesFor: #icon!development!public! !
!FreeImageBitmap class categoriesFor: #initialize!initializing!private! !
!FreeImageBitmap class categoriesFor: #new!instance creation!private! !
!FreeImageBitmap class categoriesFor: #newInstanceAspect:class:!adapters!development!private! !
!FreeImageBitmap class categoriesFor: #onStartup!event handling!private! !
!FreeImageBitmap class categoriesFor: #primAllocate:height:bpp:!bitmap management!instance creation!primitives!private! !
!FreeImageBitmap class categoriesFor: #primAllocateT:width:height:bpp:!bitmap management!instance creation!primitives!private! !
!FreeImageBitmap class categoriesFor: #primLoad:filename:flags:!bitmap management!instance creation!primitives!private! !
!FreeImageBitmap class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!FreeImageBitmap class categoriesFor: #type:extent:depth:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #type:width:height:depth:!bitmap management!instance creation!public! !
!FreeImageBitmap class categoriesFor: #width:height:depth:!bitmap management!instance creation!public! !

FreeImageFormat guid: (GUID fromString: '{99A2F568-07DA-4771-8189-5C0421081D35}')!
FreeImageFormat comment: ''!
!FreeImageFormat categoriesForClass!Unclassified! !
!FreeImageFormat methodsFor!

asParameter
	"Answer the receiver in a form suitable for an API call."

	^fif!

beDisabled
	^self isEnabled: false!

beEnabled
^self isEnabled: true!

description
	^self primGetFIFDescription!

displayExtensions
| stream contents |
	stream := ReadWriteStream on: String new.
	self extensions do: [ :each |
	stream nextPutAll: '*.' ; nextPutAll: each; nextPutAll: ', '.
	].
contents := stream contents.
^contents copyFrom: 1 to: (contents size -2)!

extensions
	^(self primGetFIFExtensionList ) subStrings: $,!

fif
	"Answer the receiver in a form suitable for an API call."

	^fif!

fif: aFreeImageFormat 
	fif := aFreeImageFormat!

flags
	^flags!

flags: anObject
	flags := anObject!

format
	^self primGetFormatFromFIF!

initialize
	fif := FIF_UNKNOWN.
	flags := nil!

isEnabled
	^self primIsPluginEnabled!

isEnabled: aBoolean 
	^self primSetPluginEnabled: aBoolean!

isUnknown
	^fif = FIF_UNKNOWN!

mimeType
	^self primGetFIFMimeType!

primFIFSupportsExportBPP: bpp 
	^(FreeImageLibrary 
		errorCheck: [:lib | lib freeImage_FIFSupportsExportBPP: self asParameter bpp: bpp]) asBoolean!

primFIFSupportsExportType: type 
	^(FreeImageLibrary 
		errorCheck: [:lib | lib freeImage_FIFSupportsExportType: self asParameter type: type]) asBoolean!

primFIFSupportsICCProfiles
	^(FreeImageLibrary errorCheck: [ :lib | lib freeImage_FIFSupportsICCProfiles: self asParameter]) asBoolean!

primFIFSupportsReading
	^(FreeImageLibrary errorCheck: [:lib | lib freeImage_FIFSupportsReading: self asParameter]) asBoolean!

primFIFSupportsWriting
	^(FreeImageLibrary errorCheck: [:lib | lib freeImage_FIFSupportsWriting: self asParameter]) asBoolean!

primGetFIFDescription
^FreeImageLibrary default  freeImage_GetFIFDescription: self asParameter.!

primGetFIFExtensionList

^FreeImageLibrary default freeImage_GetFIFExtensionList: self asParameter
!

primGetFIFMimeType
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetFIFMimeType: self asParameter].
!

primGetFIFRegExpr

^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetFIFRegExpr: self asParameter].
!

primGetFormatFromFIF
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetFormatFromFIF: self asParameter].
!

primIsPluginEnabled
^(FreeImageLibrary errorCheck: [ :lib | lib freeImage_IsPluginEnabled: self asParameter]) asBoolean!

primSetPluginEnabled: enable
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_SetPluginEnabled: self asParameter enable: enable asParameter].
!

printOn: target 
	super printOn: target.
	self isUnknown ifFalse: [
	target
		space;
		nextPut: $(;
		nextPutAll: self description;
		space;
		nextPut: $(;
		nextPutAll: self displayExtensions;
		nextPut: $);
		nextPut: $)] ifTrue: [target  nextPutAll: ' (Unknown format (returned value only, never use it as input value))']!

regularExpression
	^self primGetFIFRegExpr!

supportsExportBpp: bpp 
	^self primFIFSupportsExportBPP: bpp!

supportsExportType: aFreeImageType 
	^self primFIFSupportsExportType: aFreeImageType!

supportsICCProfiles
	^self primFIFSupportsICCProfiles!

supportsReading
	^self primFIFSupportsReading!

supportsWriting
	^self primFIFSupportsWriting! !
!FreeImageFormat categoriesFor: #asParameter!converting!public! !
!FreeImageFormat categoriesFor: #beDisabled!accessing!modes!public! !
!FreeImageFormat categoriesFor: #beEnabled!accessing!modes!public! !
!FreeImageFormat categoriesFor: #description!accessing!public! !
!FreeImageFormat categoriesFor: #displayExtensions!printing!public! !
!FreeImageFormat categoriesFor: #extensions!accessing!public! !
!FreeImageFormat categoriesFor: #fif!converting!public! !
!FreeImageFormat categoriesFor: #fif:!accessing!private! !
!FreeImageFormat categoriesFor: #flags!accessing!public! !
!FreeImageFormat categoriesFor: #flags:!accessing!public! !
!FreeImageFormat categoriesFor: #format!accessing!public! !
!FreeImageFormat categoriesFor: #initialize!initializing!private! !
!FreeImageFormat categoriesFor: #isEnabled!accessing!public!testing! !
!FreeImageFormat categoriesFor: #isEnabled:!accessing!modes!public! !
!FreeImageFormat categoriesFor: #isUnknown!accessing!public!testing! !
!FreeImageFormat categoriesFor: #mimeType!accessing!public! !
!FreeImageFormat categoriesFor: #primFIFSupportsExportBPP:!accessing!primitives!private!testing! !
!FreeImageFormat categoriesFor: #primFIFSupportsExportType:!accessing!primitives!private!testing! !
!FreeImageFormat categoriesFor: #primFIFSupportsICCProfiles!accessing!primitives!private!testing! !
!FreeImageFormat categoriesFor: #primFIFSupportsReading!accessing!primitives!private!testing! !
!FreeImageFormat categoriesFor: #primFIFSupportsWriting!accessing!primitives!private!testing! !
!FreeImageFormat categoriesFor: #primGetFIFDescription!accessing!primitives!private! !
!FreeImageFormat categoriesFor: #primGetFIFExtensionList!accessing!primitives!private! !
!FreeImageFormat categoriesFor: #primGetFIFMimeType!accessing!primitives!private! !
!FreeImageFormat categoriesFor: #primGetFIFRegExpr!accessing!primitives!private! !
!FreeImageFormat categoriesFor: #primGetFormatFromFIF!accessing!primitives!private! !
!FreeImageFormat categoriesFor: #primIsPluginEnabled!accessing!primitives!private!testing! !
!FreeImageFormat categoriesFor: #primSetPluginEnabled:!accessing!modes!primitives!private! !
!FreeImageFormat categoriesFor: #printOn:!printing!public! !
!FreeImageFormat categoriesFor: #regularExpression!accessing!public! !
!FreeImageFormat categoriesFor: #supportsExportBpp:!accessing!public!testing! !
!FreeImageFormat categoriesFor: #supportsExportType:!accessing!public!testing! !
!FreeImageFormat categoriesFor: #supportsICCProfiles!accessing!public!testing! !
!FreeImageFormat categoriesFor: #supportsReading!accessing!public!testing! !
!FreeImageFormat categoriesFor: #supportsWriting!accessing!public!testing! !

!FreeImageFormat class methodsFor!

allFormats
^(0 to: self count -1 ) collect: [:each | self  fromFif: each ].!

bmp
	^self fromFif: FIF_BMP!

count
	^self primGetFIFCount!

fromFif: aFreeImageFormat 
	^(self new)
		fif: aFreeImageFormat;
		yourself!

fromFile: aFilename 
| plugin |
	plugin := self fromFileContent: aFilename.
	plugin isUnknown ifFalse: [^plugin].
	^self fromFileName: aFilename
	!

fromFileContent: aFilename 
	^self fromFif: (self primGetFileType: aFilename size: 0 )!

fromFileName: aFilename 
	^self fromFif: (self primGetFIFFromFilename: aFilename)!

fromMemory: aFreeImageMemoryStream 
	^self fromFif: (
		self primGetFileTypeFromMemory: aFreeImageMemoryStream size: 0)!

fromMimeType: aString 
	^self fromFif: (self primGetFIFFromMime: aString )!

fromName: aString 
	^self fromFif: (self primGetFIFFromFormat: aString )!

icon
	^##(Icon fromId: 'COMINTERFACE.ICO')!

jpeg
	^self fromFif: FIF_JPEG!

new
^super new initialize!

png
^self fromFif: FIF_PNG!

primGetFIFCount
	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetFIFCount]!

primGetFIFFromFilename: filename
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetFIFFromFilename: filename]
!

primGetFIFFromFormat: format
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetFIFFromFormat: format]!

primGetFIFFromMime: mime
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetFIFFromMime: mime]!

primGetFileType:  filename size: size
^FreeImageLibrary default freeImage_GetFileType: filename size: size!

primGetFileTypeFromMemory: stream size: size

^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetFileTypeFromMemory: stream asParameter size: size]

!

readFormats
	^self allFormats select: [:each | each supportsReading ]!

tiff
	^self fromFif: FIF_TIFF!

unknown
^self fromFif: FIF_UNKNOWN!

writeFormats
	^self allFormats select: [:each | each supportsWriting ]! !
!FreeImageFormat class categoriesFor: #allFormats!helpers!public! !
!FreeImageFormat class categoriesFor: #bmp!helpers!instance creation!public! !
!FreeImageFormat class categoriesFor: #count!helpers!public! !
!FreeImageFormat class categoriesFor: #fromFif:!instance creation!public! !
!FreeImageFormat class categoriesFor: #fromFile:!instance creation!public! !
!FreeImageFormat class categoriesFor: #fromFileContent:!instance creation!public! !
!FreeImageFormat class categoriesFor: #fromFileName:!instance creation!public! !
!FreeImageFormat class categoriesFor: #fromMemory:!instance creation!public! !
!FreeImageFormat class categoriesFor: #fromMimeType:!instance creation!public! !
!FreeImageFormat class categoriesFor: #fromName:!instance creation!public! !
!FreeImageFormat class categoriesFor: #icon!development!public! !
!FreeImageFormat class categoriesFor: #jpeg!helpers!instance creation!public! !
!FreeImageFormat class categoriesFor: #new!instance creation!public! !
!FreeImageFormat class categoriesFor: #png!helpers!instance creation!public! !
!FreeImageFormat class categoriesFor: #primGetFIFCount!helpers!primitives!private! !
!FreeImageFormat class categoriesFor: #primGetFIFFromFilename:!instance creation!primitives!private! !
!FreeImageFormat class categoriesFor: #primGetFIFFromFormat:!instance creation!primitives!private! !
!FreeImageFormat class categoriesFor: #primGetFIFFromMime:!instance creation!primitives!private! !
!FreeImageFormat class categoriesFor: #primGetFileType:size:!instance creation!primitives!private! !
!FreeImageFormat class categoriesFor: #primGetFileTypeFromMemory:size:!instance creation!primitives!private! !
!FreeImageFormat class categoriesFor: #readFormats!helpers!public! !
!FreeImageFormat class categoriesFor: #tiff!helpers!instance creation!public! !
!FreeImageFormat class categoriesFor: #unknown!instance creation!public! !
!FreeImageFormat class categoriesFor: #writeFormats!helpers!public! !

FreeImageICCProfile guid: (GUID fromString: '{9285D040-461C-41A8-A13E-BECCD74D1193}')!
FreeImageICCProfile comment: ''!
!FreeImageICCProfile categoriesForClass!Unclassified! !
!FreeImageICCProfile methodsFor!

asParameter
	"Answer the receiver in a form suitable for an API call."

	^profile!

createIn: aFreeImageBitmap 
	^FreeImageLibrary default 
		freeImage_CreateICCProfile: aFreeImageBitmap asParameter
		data: self data
		size: self size!

data
	^self profile data!

flags
^self profile flags!

isCMYK
	^(self flags allMask:  FIICC_COLOR_IS_CMYK) !

onStartup
!

profile
^profile!

profile: aFIICCPROFILE
	profile := aFIICCPROFILE!

size
	^self profile size! !
!FreeImageICCProfile categoriesFor: #asParameter!converting!public! !
!FreeImageICCProfile categoriesFor: #createIn:!operations!public! !
!FreeImageICCProfile categoriesFor: #data!accessing!public! !
!FreeImageICCProfile categoriesFor: #flags!accessing!public! !
!FreeImageICCProfile categoriesFor: #isCMYK!accessing!public!testing! !
!FreeImageICCProfile categoriesFor: #onStartup!event handling!private! !
!FreeImageICCProfile categoriesFor: #profile!accessing!private! !
!FreeImageICCProfile categoriesFor: #profile:!accessing!private! !
!FreeImageICCProfile categoriesFor: #size!accessing!public! !

!FreeImageICCProfile class methodsFor!

fromFreeImageBitmap: aFreeImageBitmap 
	^self fromProfile: (FreeImageLibrary default freeImage_GetICCProfile: aFreeImageBitmap asParameter)!

fromProfile: aFIICCPROFILE 
	^(self basicNew)
		profile: aFIICCPROFILE;
		yourself!

icon
	^##(Color icon)!

new
	self shouldNotImplement!

onStartup
	self allInstances do: [:each | each onStartup]! !
!FreeImageICCProfile class categoriesFor: #fromFreeImageBitmap:!instance creation!public! !
!FreeImageICCProfile class categoriesFor: #fromProfile:!instance creation!private! !
!FreeImageICCProfile class categoriesFor: #icon!development!public! !
!FreeImageICCProfile class categoriesFor: #new!instance creation!private! !
!FreeImageICCProfile class categoriesFor: #onStartup!public! !

FreeImageMemoryStream guid: (GUID fromString: '{6F56B443-FAFB-4530-9456-F9379BA0EBB0}')!
FreeImageMemoryStream comment: ''!
!FreeImageMemoryStream categoriesForClass!Unclassified! !
!FreeImageMemoryStream methodsFor!

asParameter
	"Answer the receiver in a form suitable for an API call."

	^self stream!

basicFree
	"Private - Free up external resources held by the receiver.
	Ignore any errors which might arise."

	self primCloseMemory!

clearCached
	"Private - Clear down the receiver's cached information and handles.
	Should be overridden by subclasses which wish to clear down other
	handles and cached information."

	stream := nil!

contents
	| aquired |
	aquired := self primAquireMemory.
	^aquired first 
		ifFalse: [nil]
		ifTrue: [ByteArray fromAddress: (aquired at: 2) length: (aquired at: 3)]!

copy
	self error: 'FreeImageMemoryStreams cannot be copied'!

free
	"Free external resources held by the receiver, and leave in a state such
	that the receiver will be re-realized the next time it is accessed."

	self beUnfinalizable.
	self basicFree.
	stream := nil!

next: anInteger 
	| bytes |
	bytes := self nextOrLess: anInteger.
	bytes size < anInteger ifTrue: [self error: 'Not enough bytes to read'].
	^bytes!

nextOrLess: anInteger 
	| buffer readBytes |
	buffer := ByteArray new: anInteger.
	readBytes := FreeImageLibrary default 
				freeImage_ReadMemory: buffer
				size: 1
				count: buffer size
				stream: self asParameter.
	^buffer copyFrom: 1 to: readBytes!

nextPutAll: aByteArray 
	| bytesWritten |
	bytesWritten := FreeImageLibrary default 
				freeImage_WriteMemory: aByteArray
				size: 1
				count: aByteArray size
				stream: self asParameter.
	bytesWritten < aByteArray size ifTrue: [self error: 'Could not write all bytes to memory'].
	^ByteArray!

onStartup
	stream := nil!

origin: origin offset: offset
	^FreeImageLibrary default 
		freeImage_SeekMemory: self asParameter
		offset: offset
		origin: origin!

position
^FreeImageLibrary default freeImage_TellMemory: self asParameter!

position: anInteger 
^self origin: SEEK_SET offset: anInteger!

primAquireMemory
	| address length result |
	address := DWORD new.
	length := DWORD new.
	result := (FreeImageLibrary errorCheck: 
					[:lib | 
					lib 
						freeImage_AcquireMemory: self asParameter
						data: address
						size_in_bytes: length]) 
				.
	^Array 
		with: result asBoolean
		with: address value
		with: length value!

primCloseMemory
	^(FreeImageLibrary default freeImage_CloseMemory: self asParameter)!

setToEnd
^self origin: SEEK_END offset: 0!

setToStart
	^self origin: SEEK_SET offset: 0!

stream
	^stream!

stream: aFIMEMORY 
	stream := aFIMEMORY.
	self beFinalizable! !
!FreeImageMemoryStream categoriesFor: #asParameter!converting!public! !
!FreeImageMemoryStream categoriesFor: #basicFree!private!realizing/unrealizing! !
!FreeImageMemoryStream categoriesFor: #clearCached!private!realizing/unrealizing! !
!FreeImageMemoryStream categoriesFor: #contents!helpers!public! !
!FreeImageMemoryStream categoriesFor: #copy!copying!public! !
!FreeImageMemoryStream categoriesFor: #free!public!realizing/unrealizing! !
!FreeImageMemoryStream categoriesFor: #next:!accessing!public! !
!FreeImageMemoryStream categoriesFor: #nextOrLess:!accessing!public! !
!FreeImageMemoryStream categoriesFor: #nextPutAll:!accessing!public! !
!FreeImageMemoryStream categoriesFor: #onStartup!event handling!private! !
!FreeImageMemoryStream categoriesFor: #origin:offset:!positioning!public! !
!FreeImageMemoryStream categoriesFor: #position!positioning!public! !
!FreeImageMemoryStream categoriesFor: #position:!positioning!public! !
!FreeImageMemoryStream categoriesFor: #primAquireMemory!helpers!primitives!private! !
!FreeImageMemoryStream categoriesFor: #primCloseMemory!primitives!private!realizing/unrealizing! !
!FreeImageMemoryStream categoriesFor: #setToEnd!positioning!public! !
!FreeImageMemoryStream categoriesFor: #setToStart!positioning!public! !
!FreeImageMemoryStream categoriesFor: #stream!accessing!public! !
!FreeImageMemoryStream categoriesFor: #stream:!accessing!private! !

!FreeImageMemoryStream class methodsFor!

fromByteArray: aByteArray
^self open: aByteArray yourAddress size: aByteArray size!

fromMemory: aFIMEMORY 
	"Answers an instance of the receiver with aHandle. The handle is not
	owned by the instance and will not therefore be freed by it."

	^(self basicNew )
		stream: aFIMEMORY;
		yourself!

icon
	^##(Icon fromId: 'MEMORYMANAGER.ICO')!

initialize
	"Private - Register the receiver with the #onStartup event"

	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self.
	!

new
	self shouldNotImplement!

onStartup
	self allInstances do: [ :each | each onStartup]!

open
	^self open: 0 size: 0!

open: anExternalAddress size: anInteger
	^self fromMemory: (FreeImageLibrary default freeImage_OpenMemory: anExternalAddress size_in_bytes:anInteger )! !
!FreeImageMemoryStream class categoriesFor: #fromByteArray:!bitmap management!instance creation!public! !
!FreeImageMemoryStream class categoriesFor: #fromMemory:!bitmap management!instance creation!private! !
!FreeImageMemoryStream class categoriesFor: #icon!development!public! !
!FreeImageMemoryStream class categoriesFor: #initialize!initializing!private! !
!FreeImageMemoryStream class categoriesFor: #new!instance creation!private! !
!FreeImageMemoryStream class categoriesFor: #onStartup!event handling!private! !
!FreeImageMemoryStream class categoriesFor: #open!bitmap management!instance creation!public! !
!FreeImageMemoryStream class categoriesFor: #open:size:!bitmap management!instance creation!public! !

FreeImageMetadata guid: (GUID fromString: '{42BC22CD-681A-405F-AA29-3945F068EC68}')!
FreeImageMetadata comment: ''!
!FreeImageMetadata categoriesForClass!Unclassified! !
!FreeImageMetadata methodsFor!

asParameter
^fimd!

at: aString 
	|  |
	^FreeImageTag fromHandle: (self primGetMetadata: aString)!

at: aString put: aFreeImageTag 
self primSetMetadata: aString tag: aFreeImageTag!

primFindCloseMetadata: mdhandle

			FreeImageLibrary errorCheck: [ :lib | lib freeImage_FindCloseMetadata: mdhandle]!

primFindFirstMetadata
	| tag mdhandle |
	tag := FITAG newPointer.
	mdhandle := FreeImageLibrary default 
						freeImage_FindFirstMetadata: self asParameter
						dib: bitmap asParameter
						tag: tag.
	^Array with: mdhandle with: tag!

primFindNextMetadata: mdhandle 
	| tag |
	tag := FITAG newPointer.
	^(FreeImageLibrary default freeImage_FindNextMetadata: mdhandle tag: tag) notNull ifTrue: [tag] ifFalse: [nil]!

primGetMetadata: key 
	| tag  |
	tag := FITAG newPointer.
	FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_GetMetadata: self asParameter
				dib: bitmap asParameter
				key: key
				tag: tag].
	^tag!

primGetMetadataCount
	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_GetMetadataCount: self asParameter dib: bitmap asParameter]!

primSetMetadata: key tag: tag

^FreeImageLibrary errorCheck: [ :lib | lib freeImage_SetMetadata: self asParameter dib: bitmap asParameter key: key tag: tag asParameter].
!

removeAll
	^self at: nil putTag: nil!

removeAtKey: aString 
	^self at: aString putTag: nil!

setBitmap: aFreeImageBitmap fimd: aFIMD 
	bitmap := aFreeImageBitmap.
	fimd := aFIMD!

tagCount
	^self primGetMetadataCount!

tags
	| tags tag mdhandle result |
	result := self primFindFirstMetadata.
	mdhandle := result first.
	tag := result second.
	tags := Set new.
	mdhandle notNull 
		ifTrue: 
			[[tag notNil] whileTrue: 
					[tags add: (FreeImageTag fromHandle: tag).
					tag := self primFindNextMetadata: mdhandle].
			
			
			self primFindCloseMetadata: mdhandle].
	^tags! !
!FreeImageMetadata categoriesFor: #asParameter!accessing!converting!public! !
!FreeImageMetadata categoriesFor: #at:!accessing!public! !
!FreeImageMetadata categoriesFor: #at:put:!accessing!public! !
!FreeImageMetadata categoriesFor: #primFindCloseMetadata:!accessing!public! !
!FreeImageMetadata categoriesFor: #primFindFirstMetadata!accessing!public! !
!FreeImageMetadata categoriesFor: #primFindNextMetadata:!accessing!public! !
!FreeImageMetadata categoriesFor: #primGetMetadata:!accessing!primitives!private! !
!FreeImageMetadata categoriesFor: #primGetMetadataCount!accessing!primitives!private! !
!FreeImageMetadata categoriesFor: #primSetMetadata:tag:!accessing!primitives!private! !
!FreeImageMetadata categoriesFor: #removeAll!accessing!public! !
!FreeImageMetadata categoriesFor: #removeAtKey:!accessing!public! !
!FreeImageMetadata categoriesFor: #setBitmap:fimd:!initialize!private! !
!FreeImageMetadata categoriesFor: #tagCount!accessing!public! !
!FreeImageMetadata categoriesFor: #tags!accessing!public! !

!FreeImageMetadata class methodsFor!

animationFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_ANIMATION;
		yourself!

commentsFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_COMMENTS ;
		yourself!

exifExifFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_EXIF_EXIF;
		yourself!

exifGpsFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_EXIF_GPS;
		yourself!

exifInteropFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_EXIF_MAKERNOTE;
		yourself!

exifMainFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_EXIF_MAIN ;
		yourself!

exifMakerNoteFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_EXIF_MAKERNOTE;
		yourself!

fromFreeImageBitmap: aFreeImageBitmap fimd: aFIMD 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: aFIMD ;
		yourself!

geotiffFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_GEOTIFF;
		yourself!

icon
	^##(Icon fromId: 'METACLASS.ICO')!

iptcFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_IPTC;
		yourself!

xmpFromFreeImageBitmap: aFreeImageBitmap 
	^(self new)
		setBitmap: aFreeImageBitmap fimd: FIMD_XMP;
		yourself! !
!FreeImageMetadata class categoriesFor: #animationFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #commentsFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #exifExifFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #exifGpsFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #exifInteropFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #exifMainFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #exifMakerNoteFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #fromFreeImageBitmap:fimd:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #geotiffFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #icon!public! !
!FreeImageMetadata class categoriesFor: #iptcFromFreeImageBitmap:!instance creation!public! !
!FreeImageMetadata class categoriesFor: #xmpFromFreeImageBitmap:!instance creation!public! !

FreeImageTag guid: (GUID fromString: '{66935647-7DF5-441D-9392-FA9BF6642B62}')!
FreeImageTag comment: ''!
!FreeImageTag categoriesForClass!Unclassified! !
!FreeImageTag methodsFor!

asParameter
	"Answer the receiver in a form suitable for an API call."

	^self handle!

basicByteValue: address count: count 
	^StructureArray 
		fromAddress: address
		length: count
		elementClass: BYTE!

basicDoubleValue: address count: count 
	^StructureArray 
		fromAddress: address
		length: count
		elementClass: DOUBLE!

basicDwordValue: address length: count 
	^StructureArray 
		fromAddress: address
		length: count
		elementClass: DWORD!

basicFloatValue: address count: count 
	^StructureArray 
		fromAddress: address
		length: count
		elementClass: FLOAT!

basicFree
	"Private - Free up external resources held by the receiver.
	Ignore any errors which might arise."

	self primDeleteTag!

basicPaletteValue: address count: count 
	^StructureArray 
		fromAddress: address
		length: count
		elementClass: RGBQUAD!

basicRationalValue: address count: count 
	| rationals |
	rationals := StructureArray 
				fromAddress: address
				length: count * 2
				elementClass: DWORD.
	^(1 to: rationals size by: 2) 
		collect: [:index | Fraction numerator: (rationals at: index) value denominator: (rationals at: index + 1) value]!

basicSdwordValue: address length: count 
	^StructureArray 
		fromAddress: address
		length: count
		elementClass: SDWORD!

basicSrationalValue: address count: count 
	| rationals |
	rationals := StructureArray 
				fromAddress: address
				length: count * 2
				elementClass: SDWORD.
	^(1 to: rationals size by: 2) 
		collect: [:index | Fraction numerator: (rationals at: index) value denominator: (rationals at: index + 1) value]!

basicStringValue: address count: count 
	^String fromAddress: address length: count - 1!

basicSwordValue: address count: count 
	^StructureArray 
		fromAddress: address
		length: count
		elementClass: SWORD!

basicWordValue: address count: count 
	^StructureArray 
		fromAddress: address
		length: count
		elementClass: WORD!

clearCached
	"Private - Clear down the receiver's cached information and handles.
	Should be overridden by subclasses which wish to clear down other
	handles and cached information."

	handle := nil.
	metadata := nil!

copy
	^(super copy)
		ownedHandle: self copyHandle;
		yourself!

copyHandle
	"Private - Answer an external handle to the image which is a copy of that represented by the receiver."

	^self primCloneTag!

description
	^FreeImageLibrary default freeImage_GetTagDescription: self asParameter!

description: aString
	^FreeImageLibrary default freeImage_SetTagDescription: self asParameter description: aString!

free
	"Free external resources held by the receiver, and leave in a state such
	that the receiver will be re-realized the next time it is accessed."

	self ownsHandle 
		ifTrue: 
			[self beUnfinalizable.
			self basicFree].
	handle := nil.
	metadata := nil.!

handle
	^handle!

handle: aHandle 
handle := aHandle!

id
	^FreeImageLibrary default freeImage_GetTagID: self asParameter!

id: anInteger
	^FreeImageLibrary default freeImage_SetTagID: self asParameter id: anInteger!

initialize
	"Initialize the receiver's instance variables."

	ownsHandle := true	"By default instances will free their handles on finalization".
	metadata := nil!

key
^FreeImageLibrary default freeImage_GetTagKey: self asParameter!

key: aString
	^FreeImageLibrary default freeImage_SetTagKey: self asParameter key: aString!

onStartup
	handle := nil.
!

ownedHandle: aHandle
	"Private - Set the handle of the external graphics's tool object represented and owned by
	the receiver to be the argument."

	self handle: aHandle.
	ownsHandle := true.
	self beFinalizable!

ownsHandle
	"Answer whether the receiver owns the handle it is holding"

	^ownsHandle!

ownsHandle: aBoolean
	"Private - Record whether the receiver owns the handle it is holding, and will
	therefore release it when finalized. Answer the receiver."

	ownsHandle := aBoolean!

primCloneTag
	

	^FreeImageLibrary errorCheck: [:lib | lib freeImage_CloneTag: self asParameter]!

primDeleteTag

	^FreeImageLibrary errorCheck: [ :lib | lib  freeImage_DeleteTag: self asParameter]!

primGetTagCount
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetTagCount: self asParameter]!

primGetTagLength
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetTagLength: self asParameter]!

primGetTagType
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetTagType: self asParameter]!

primGetTagValue
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetTagValue: self asParameter]!

primSetTagCount: count 
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_SetTagCount: self asParameter count: count]!

primSetTagLength: length 
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_SetTagLength: self asParameter length: length]!

primSetTagType: type 
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_SetTagType: self asParameter type: type]!

primSetTagValue: value 
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_SetTagValue: self asParameter value: value]!

primTagToString
	^FreeImageLibrary errorCheck: [ :lib | lib 
		freeImage_TagToString: metadata asParameter
		tag: self asParameter
		make: nil]!

value
	| type address count |
	type := self primGetTagType.
	address := self primGetTagValue.
	count := self primGetTagCount.
	(type = FIDT_BYTE or: [type = FIDT_UNDEFINED]) ifTrue: [^self basicByteValue: address count: count].
	type = FIDT_ASCII ifTrue: [^self basicStringValue: address count: count].
	type = FIDT_SHORT ifTrue: [^self basicWordValue: address count: count].
	(type = FIDT_LONG or: [type = FIDT_IFD]) ifTrue: [^self basicDwordValue: address length: count].
	type = FIDT_RATIONAL ifTrue: [^self basicRationalValue: address count: count].
	type = FIDT_SSHORT ifTrue: [^self basicSwordValue: address count: count].
	(type = FIDT_SLONG or: [type = FIDT_IFD]) ifTrue: [^self basicSdwordValue: address length: count].
	type = FIDT_SRATIONAL ifTrue: [^self basicSrationalValue: address count: count].
	type = FIDT_FLOAT ifTrue: [^self basicFloatValue: address count: count].
	type = FIDT_DOUBLE ifTrue: [^self basicFloatValue: address count: count].
	type = FIDT_PALETTE ifTrue: [^self basicPaletteValue: address count: count].
	^self basicByteValue: address count: count!

value: aValue
^Error notYetImplemented!

valueString
	^self primTagToString! !
!FreeImageTag categoriesFor: #asParameter!converting!public! !
!FreeImageTag categoriesFor: #basicByteValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #basicDoubleValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #basicDwordValue:length:!accessing!private! !
!FreeImageTag categoriesFor: #basicFloatValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #basicFree!bitmap management!private!realizing/unrealizing! !
!FreeImageTag categoriesFor: #basicPaletteValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #basicRationalValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #basicSdwordValue:length:!accessing!private! !
!FreeImageTag categoriesFor: #basicSrationalValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #basicStringValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #basicSwordValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #basicWordValue:count:!accessing!private! !
!FreeImageTag categoriesFor: #clearCached!private!realizing/unrealizing! !
!FreeImageTag categoriesFor: #copy!bitmap management!copying!public! !
!FreeImageTag categoriesFor: #copyHandle!bitmap management!private!realizing/unrealizing! !
!FreeImageTag categoriesFor: #description!accessing!public! !
!FreeImageTag categoriesFor: #description:!accessing!public! !
!FreeImageTag categoriesFor: #free!public!realizing/unrealizing! !
!FreeImageTag categoriesFor: #handle!accessing!public! !
!FreeImageTag categoriesFor: #handle:!accessing!private! !
!FreeImageTag categoriesFor: #id!accessing!public! !
!FreeImageTag categoriesFor: #id:!accessing!public! !
!FreeImageTag categoriesFor: #initialize!initializing!private! !
!FreeImageTag categoriesFor: #key!accessing!public! !
!FreeImageTag categoriesFor: #key:!accessing!public! !
!FreeImageTag categoriesFor: #onStartup!event handling!private! !
!FreeImageTag categoriesFor: #ownedHandle:!accessing!private! !
!FreeImageTag categoriesFor: #ownsHandle!public!testing! !
!FreeImageTag categoriesFor: #ownsHandle:!private!testing! !
!FreeImageTag categoriesFor: #primCloneTag!bitmap management!primitives!private!realizing/unrealizing! !
!FreeImageTag categoriesFor: #primDeleteTag!bitmap management!primitives!private!realizing/unrealizing! !
!FreeImageTag categoriesFor: #primGetTagCount!accessing!primitives!private! !
!FreeImageTag categoriesFor: #primGetTagLength!accessing!primitives!private! !
!FreeImageTag categoriesFor: #primGetTagType!accessing!primitives!private! !
!FreeImageTag categoriesFor: #primGetTagValue!accessing!primitives!private! !
!FreeImageTag categoriesFor: #primSetTagCount:!accessing!primitives!private! !
!FreeImageTag categoriesFor: #primSetTagLength:!accessing!primitives!private! !
!FreeImageTag categoriesFor: #primSetTagType:!accessing!primitives!private! !
!FreeImageTag categoriesFor: #primSetTagValue:!accessing!primitives!private! !
!FreeImageTag categoriesFor: #primTagToString!accessing!public! !
!FreeImageTag categoriesFor: #value!accessing!public! !
!FreeImageTag categoriesFor: #value:!accessing!public! !
!FreeImageTag categoriesFor: #valueString!accessing!public! !

!FreeImageTag class methodsFor!

create
	^self fromOwnedHandle: FreeImageLibrary default freeImage_CreateTag.
	!

fromHandle: aHandle 
	"Answers an instance of the receiver with aHandle. The handle is not
	owned by the instance and will not therefore be freed by it."

	^(self new)
		ownsHandle: false;
		handle: aHandle; yourself!

fromOwnedHandle: aHandle 
	"Answers an instance of the receiver with aHandle. The handle is owned by the instance and
	will therefore be freed by it."

	^(self new)
		ownedHandle: aHandle;
		yourself!

icon
	^##(Icon fromId: 'CATEGORY.ICO')!

initialize
	"Private - Register the receiver with the #onStartup event"

	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self.
	!

new
	"Answer a new, properly initialized, instance of the receiver."

	^super new initialize!

onStartup
	self allInstances do: [ :each | each onStartup]!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect string:  #description) beReadOnly;
		add: (Aspect string: #key);
		add: (Aspect name: #id);
		add: (Aspect name: #value) beReadOnly;
		add: (Aspect string:  #valueString) beReadOnly;
		yourself! !
!FreeImageTag class categoriesFor: #create!public! !
!FreeImageTag class categoriesFor: #fromHandle:!bitmap management!instance creation!public! !
!FreeImageTag class categoriesFor: #fromOwnedHandle:!bitmap management!instance creation!public! !
!FreeImageTag class categoriesFor: #icon!development!public! !
!FreeImageTag class categoriesFor: #initialize!initializing!private! !
!FreeImageTag class categoriesFor: #new!instance creation!private! !
!FreeImageTag class categoriesFor: #onStartup!event handling!private! !
!FreeImageTag class categoriesFor: #publishedAspectsOfInstances!development!must strip!public! !

FreeImageError guid: (GUID fromString: '{4F75571D-EA9A-4260-BB17-4CD9AB74DBB6}')!
FreeImageError comment: ''!
!FreeImageError categoriesForClass!Unclassified! !
!FreeImageError methodsFor!

_descriptionArguments
	^Array with: messageText with: tag displayString!

_descriptionFormat
	"Answer the Win32 format String to be used to format the description for the receiver."

	^'FreeImage Error: %1'! !
!FreeImageError categoriesFor: #_descriptionArguments!*-in class package!displaying!public! !
!FreeImageError categoriesFor: #_descriptionFormat!*-in class package!displaying!public! !

FreeImageLibrary guid: (GUID fromString: '{44F1F4AF-6EBA-4890-B313-14601E3D3BE2}')!
FreeImageLibrary comment: 'FreeImageLibrary is the <ExternalLibrary> class to represent the dynamic link library, ''FreeImage.dll''.It was generated generated from type information in the ''FreeImage Type Library'' library. It contains methods for each of the functions defined by the corresponding module in that type library.

The type library contains no documentation for this module

Warning: This comment was automatically generated from the module''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

[
	dllname("FreeImage.dll"), 
	uuid(44F1F4AF-6EBA-4890-B313-14601E3D3BE2)
]
module FreeImage
{
	[entry(0x60000000)]
	void __stdcall FreeImage_Initialise(
		long load_local_plugins_only);
	[entry(0x60000001)]
	void __stdcall FreeImage_DeInitialise();
	[entry(0x60000002)]
	CHAR* __stdcall FreeImage_GetVersion();
	[entry(0x60000003)]
	CHAR* __stdcall FreeImage_GetCopyrightMessage();
	[entry(0x60000004)]
	void __stdcall FreeImage_SetOutputMessage(
		void* omf);
	[entry(0x60000005)]
	FIBITMAP* __stdcall FreeImage_Allocate(
		int width,
		int height,
		int bpp,
		unsigned int red_mask,
		unsigned int green_mask,
		unsigned int blue_mask);
	[entry(0x60000006)]
	FIBITMAP* __stdcall FreeImage_AllocateT(
		int type,
		int width,
		int height,
		int bpp,
		unsigned int red_mask,
		unsigned int green_mask,
		unsigned int blue_mask);
	[entry(0x60000007)]
	FIBITMAP* __stdcall FreeImage_Clone(
		FIBITMAP* dib);
	[entry(0x60000008)]
	void __stdcall FreeImage_Unload(
		FIBITMAP* dib);
	[entry(0x60000009)]
	FIBITMAP* __stdcall FreeImage_Load(
		int fif,
		CHAR* filename,
		int flags);
	[entry(0x6000000a)]
	FIBITMAP* __stdcall FreeImage_LoadU(
		int fif,
		unsigned short* filename,
		int flags);
	[entry(0x6000000b)]
	FIBITMAP* __stdcall FreeImage_LoadFromHandle(
		int fif,
		FreeImageIO* io,
		void* handle,
		int flags);
	[entry(0x6000000c)]
	long __stdcall FreeImage_Save(
		int fif,
		FIBITMAP* dib,
		CHAR* filename,
		int flags);
	[entry(0x6000000d)]
	long __stdcall FreeImage_SaveU(
		int fif,
		FIBITMAP* dib,
		unsigned short* filename,
		int flags);
	[entry(0x6000000e)]
	long __stdcall FreeImage_SaveToHandle(
		int fif,
		FIBITMAP* dib,
		FreeImageIO* io,
		void* handle,
		int flags);
	[entry(0x6000000f)]
	FIMEMORY* __stdcall FreeImage_OpenMemory(
		BYTE* data,
		unsigned long size_in_bytes);
	[entry(0x60000010)]
	void __stdcall FreeImage_CloseMemory(
		FIMEMORY* stream);
	[entry(0x60000011)]
	FIBITMAP* __stdcall FreeImage_LoadFromMemory(
		int fif,
		FIMEMORY* stream,
		int flags);
	[entry(0x60000012)]
	long __stdcall FreeImage_SaveToMemory(
		int fif,
		FIBITMAP* dib,
		FIMEMORY* stream,
		int flags);
	[entry(0x60000013)]
	long __stdcall FreeImage_TellMemory(
		FIMEMORY* stream);
	[entry(0x60000014)]
	long __stdcall FreeImage_SeekMemory(
		FIMEMORY* stream,
		long offset,
		int origin);
	[entry(0x60000015)]
	long __stdcall FreeImage_AcquireMemory(
		FIMEMORY* stream,
		BYTE** data,
		unsigned long* size_in_bytes);
	[entry(0x60000016)]
	unsigned int __stdcall FreeImage_ReadMemory(
		void* buffer,
		unsigned int size,
		unsigned int count,
		FIMEMORY* stream);
	[entry(0x60000017)]
	unsigned int __stdcall FreeImage_WriteMemory(
		void* buffer,
		unsigned int size,
		unsigned int count,
		FIMEMORY* stream);
	[entry(0x60000018)]
	FIMULTIBITMAP* __stdcall FreeImage_LoadMultiBitmapFromMemory(
		int fif,
		FIMEMORY* stream,
		int flags);
	[entry(0x60000019)]
	int __stdcall FreeImage_RegisterLocalPlugin(
		void* proc_address,
		CHAR* format,
		CHAR* description,
		CHAR* extension,
		CHAR* regexpr);
	[entry(0x6000001a)]
	int __stdcall FreeImage_RegisterExternalPlugin(
		CHAR* path,
		CHAR* format,
		CHAR* description,
		CHAR* extension,
		CHAR* regexpr);
	[entry(0x6000001b)]
	int __stdcall FreeImage_GetFIFCount();
	[entry(0x6000001c)]
	int __stdcall FreeImage_SetPluginEnabled(
		int fif,
		long enable);
	[entry(0x6000001d)]
	int __stdcall FreeImage_IsPluginEnabled(
		int fif);
	[entry(0x6000001e)]
	int __stdcall FreeImage_GetFIFFromFormat(
		CHAR* format);
	[entry(0x6000001f)]
	int __stdcall FreeImage_GetFIFFromMime(
		CHAR* mime);
	[entry(0x60000020)]
	CHAR* __stdcall FreeImage_GetFormatFromFIF(
		int fif);
	[entry(0x60000021)]
	CHAR* __stdcall FreeImage_GetFIFExtensionList(
		int fif);
	[entry(0x60000022)]
	CHAR* __stdcall FreeImage_GetFIFDescription(
		int fif);
	[entry(0x60000023)]
	CHAR* __stdcall FreeImage_GetFIFRegExpr(
		int fif);
	[entry(0x60000024)]
	CHAR* __stdcall FreeImage_GetFIFMimeType(
		int fif);
	[entry(0x60000025)]
	int __stdcall FreeImage_GetFIFFromFilename(
		CHAR* filename);
	[entry(0x60000026)]
	int __stdcall FreeImage_GetFIFFromFilenameU(
		unsigned short* filename);
	[entry(0x60000027)]
	long __stdcall FreeImage_FIFSupportsReading(
		int fif);
	[entry(0x60000028)]
	long __stdcall FreeImage_FIFSupportsWriting(
		int fif);
	[entry(0x60000029)]
	long __stdcall FreeImage_FIFSupportsExportBPP(
		int fif,
		int bpp);
	[entry(0x6000002a)]
	long __stdcall FreeImage_FIFSupportsExportType(
		int fif,
		int type);
	[entry(0x6000002b)]
	long __stdcall FreeImage_FIFSupportsICCProfiles(
		int fif);
	[entry(0x6000002c)]
	FIMULTIBITMAP* __stdcall FreeImage_OpenMultiBitmap(
		int fif,
		CHAR* filename,
		long create_new,
		long read_only,
		long keep_cache_in_memory,
		int flags);
	[entry(0x6000002d)]
	long __stdcall FreeImage_CloseMultiBitmap(
		FIMULTIBITMAP* bitmap,
		int flags);
	[entry(0x6000002e)]
	int __stdcall FreeImage_GetPageCount(
		FIMULTIBITMAP* bitmap);
	[entry(0x6000002f)]
	void __stdcall FreeImage_AppendPage(
		FIMULTIBITMAP* bitmap,
		FIBITMAP* data);
	[entry(0x60000030)]
	void __stdcall FreeImage_InsertPage(
		FIMULTIBITMAP* bitmap,
		int page,
		FIBITMAP* data);
	[entry(0x60000031)]
	void __stdcall FreeImage_DeletePage(
		FIMULTIBITMAP* bitmap,
		int page);
	[entry(0x60000032)]
	FIBITMAP* __stdcall FreeImage_LockPage(
		FIMULTIBITMAP* bitmap,
		int page);
	[entry(0x60000033)]
	void __stdcall FreeImage_UnlockPage(
		FIMULTIBITMAP* bitmap,
		FIBITMAP* data,
		long changed);
	[entry(0x60000034)]
	long __stdcall FreeImage_MovePage(
		FIMULTIBITMAP* bitmap,
		int target,
		int source);
	[entry(0x60000035)]
	long __stdcall FreeImage_GetLockedPageNumbers(
		FIMULTIBITMAP* bitmap,
		int* pages,
		int* count);
	[entry(0x60000036)]
	int __stdcall FreeImage_GetFileType(
		CHAR* filename,
		int size);
	[entry(0x60000037)]
	int __stdcall FreeImage_GetFileTypeU(
		unsigned short* filename,
		int size);
	[entry(0x60000038)]
	int __stdcall FreeImage_GetFileTypeFromHandle(
		FreeImageIO* io,
		void* handle,
		int size);
	[entry(0x60000039)]
	int __stdcall FreeImage_GetFileTypeFromMemory(
		FIMEMORY* stream,
		int size);
	[entry(0x6000003a)]
	int __stdcall FreeImage_GetImageType(
		FIBITMAP* dib);
	[entry(0x6000003b)]
	long __stdcall FreeImage_IsLittleEndian();
	[entry(0x6000003c)]
	long __stdcall FreeImage_LookupX11Color(
		CHAR* szColor,
		BYTE* nRed,
		BYTE* nGreen,
		BYTE* nBlue);
	[entry(0x6000003d)]
	long __stdcall FreeImage_LookupSVGColor(
		CHAR* szColor,
		BYTE* nRed,
		BYTE* nGreen,
		BYTE* nBlue);
	[entry(0x6000003e)]
	BYTE* __stdcall FreeImage_GetBits(
		FIBITMAP* dib);
	[entry(0x6000003f)]
	BYTE* __stdcall FreeImage_GetScanLine(
		FIBITMAP* dib,
		int scanline);
	[entry(0x60000040)]
	long __stdcall FreeImage_GetPixelIndex(
		FIBITMAP* dib,
		unsigned int x,
		unsigned int y,
		BYTE* value);
	[entry(0x60000041)]
	long __stdcall FreeImage_GetPixelColor(
		FIBITMAP* dib,
		unsigned int x,
		unsigned int y,
		RGBQUAD* value);
	[entry(0x60000042)]
	long __stdcall FreeImage_SetPixelIndex(
		FIBITMAP* dib,
		unsigned int x,
		unsigned int y,
		BYTE* value);
	[entry(0x60000043)]
	long __stdcall FreeImage_SetPixelColor(
		FIBITMAP* dib,
		unsigned int x,
		unsigned int y,
		RGBQUAD* value);
	[entry(0x60000044)]
	unsigned int __stdcall FreeImage_GetColorsUsed(
		FIBITMAP* dib);
	[entry(0x60000045)]
	unsigned int __stdcall FreeImage_GetBPP(
		FIBITMAP* dib);
	[entry(0x60000046)]
	unsigned int __stdcall FreeImage_GetWidth(
		FIBITMAP* dib);
	[entry(0x60000047)]
	unsigned int __stdcall FreeImage_GetHeight(
		FIBITMAP* dib);
	[entry(0x60000048)]
	unsigned int __stdcall FreeImage_GetLine(
		FIBITMAP* dib);
	[entry(0x60000049)]
	unsigned int __stdcall FreeImage_GetPitch(
		FIBITMAP* dib);
	[entry(0x6000004a)]
	unsigned int __stdcall FreeImage_GetDIBSize(
		FIBITMAP* dib);
	[entry(0x6000004b)]
	RGBQUAD* __stdcall FreeImage_GetPalette(
		FIBITMAP* dib);
	[entry(0x6000004c)]
	unsigned int __stdcall FreeImage_GetDotsPerMeterX(
		FIBITMAP* dib);
	[entry(0x6000004d)]
	unsigned int __stdcall FreeImage_GetDotsPerMeterY(
		FIBITMAP* dib);
	[entry(0x6000004e)]
	void __stdcall FreeImage_SetDotsPerMeterX(
		FIBITMAP* dib,
		unsigned int res);
	[entry(0x6000004f)]
	void __stdcall FreeImage_SetDotsPerMeterY(
		FIBITMAP* dib,
		unsigned int res);
	[entry(0x60000050)]
	BITMAPINFOHEADER* __stdcall FreeImage_GetInfoHeader(
		FIBITMAP* dib);
	[entry(0x60000051)]
	BITMAPINFO* __stdcall FreeImage_GetInfo(
		FIBITMAP* dib);
	[entry(0x60000052)]
	int __stdcall FreeImage_GetColorType(
		FIBITMAP* dib);
	[entry(0x60000053)]
	unsigned int __stdcall FreeImage_GetRedMask(
		FIBITMAP* dib);
	[entry(0x60000054)]
	unsigned int __stdcall FreeImage_GetGreenMask(
		FIBITMAP* dib);
	[entry(0x60000055)]
	unsigned int __stdcall FreeImage_GetBlueMask(
		FIBITMAP* dib);
	[entry(0x60000056)]
	unsigned int __stdcall FreeImage_GetTransparencyCount(
		FIBITMAP* dib);
	[entry(0x60000057)]
	BYTE* __stdcall FreeImage_GetTransparencyTable(
		FIBITMAP* dib);
	[entry(0x60000058)]
	void __stdcall FreeImage_SetTransparent(
		FIBITMAP* dib,
		long enabled);
	[entry(0x60000059)]
	void __stdcall FreeImage_SetTransparencyTable(
		FIBITMAP* dib,
		BYTE* table,
		int count);
	[entry(0x6000005a)]
	long __stdcall FreeImage_IsTransparent(
		FIBITMAP* dib);
	[entry(0x6000005b)]
	long __stdcall FreeImage_HasBackgroundColor(
		FIBITMAP* dib);
	[entry(0x6000005c)]
	long __stdcall FreeImage_GetBackgroundColor(
		FIBITMAP* dib,
		RGBQUAD* bkcolor);
	[entry(0x6000005d)]
	long __stdcall FreeImage_SetBackgroundColor(
		FIBITMAP* dib,
		RGBQUAD* bkcolor);
	[entry(0x6000005e)]
	FIICCPROFILE* __stdcall FreeImage_GetICCProfile(
		FIBITMAP* dib);
	[entry(0x6000005f)]
	FIICCPROFILE* __stdcall FreeImage_CreateICCProfile(
		FIBITMAP* dib,
		void* data,
		long size);
	[entry(0x60000060)]
	void __stdcall FreeImage_DestroyICCProfile(
		FIBITMAP* dib);
	[entry(0x60000061)]
	void __stdcall FreeImage_ConvertLine1To4(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000062)]
	void __stdcall FreeImage_ConvertLine8To4(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x60000063)]
	void __stdcall FreeImage_ConvertLine16To4_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000064)]
	void __stdcall FreeImage_ConvertLine16To4_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000065)]
	void __stdcall FreeImage_ConvertLine24To4(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000066)]
	void __stdcall FreeImage_ConvertLine32To4(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000067)]
	void __stdcall FreeImage_ConvertLine1To8(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000068)]
	void __stdcall FreeImage_ConvertLine4To8(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000069)]
	void __stdcall FreeImage_ConvertLine16To8_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x6000006a)]
	void __stdcall FreeImage_ConvertLine16To8_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x6000006b)]
	void __stdcall FreeImage_ConvertLine24To8(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x6000006c)]
	void __stdcall FreeImage_ConvertLine32To8(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x6000006d)]
	void __stdcall FreeImage_ConvertLine1To16_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x6000006e)]
	void __stdcall FreeImage_ConvertLine4To16_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x6000006f)]
	void __stdcall FreeImage_ConvertLine8To16_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x60000070)]
	void __stdcall FreeImage_ConvertLine16_565_To16_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000071)]
	void __stdcall FreeImage_ConvertLine24To16_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000072)]
	void __stdcall FreeImage_ConvertLine32To16_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000073)]
	void __stdcall FreeImage_ConvertLine1To16_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x60000074)]
	void __stdcall FreeImage_ConvertLine4To16_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x60000075)]
	void __stdcall FreeImage_ConvertLine8To16_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x60000076)]
	void __stdcall FreeImage_ConvertLine16_555_To16_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000077)]
	void __stdcall FreeImage_ConvertLine24To16_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000078)]
	void __stdcall FreeImage_ConvertLine32To16_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000079)]
	void __stdcall FreeImage_ConvertLine1To24(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x6000007a)]
	void __stdcall FreeImage_ConvertLine4To24(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x6000007b)]
	void __stdcall FreeImage_ConvertLine8To24(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x6000007c)]
	void __stdcall FreeImage_ConvertLine16To24_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x6000007d)]
	void __stdcall FreeImage_ConvertLine16To24_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x6000007e)]
	void __stdcall FreeImage_ConvertLine32To24(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x6000007f)]
	void __stdcall FreeImage_ConvertLine1To32(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x60000080)]
	void __stdcall FreeImage_ConvertLine4To32(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x60000081)]
	void __stdcall FreeImage_ConvertLine8To32(
		BYTE* target,
		BYTE* source,
		int width_in_pixels,
		RGBQUAD* palette);
	[entry(0x60000082)]
	void __stdcall FreeImage_ConvertLine16To32_555(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000083)]
	void __stdcall FreeImage_ConvertLine16To32_565(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000084)]
	void __stdcall FreeImage_ConvertLine24To32(
		BYTE* target,
		BYTE* source,
		int width_in_pixels);
	[entry(0x60000085)]
	FIBITMAP* __stdcall FreeImage_ConvertTo4Bits(
		FIBITMAP* dib);
	[entry(0x60000086)]
	FIBITMAP* __stdcall FreeImage_ConvertTo8Bits(
		FIBITMAP* dib);
	[entry(0x60000087)]
	FIBITMAP* __stdcall FreeImage_ConvertToGreyscale(
		FIBITMAP* dib);
	[entry(0x60000088)]
	FIBITMAP* __stdcall FreeImage_ConvertTo16Bits555(
		FIBITMAP* dib);
	[entry(0x60000089)]
	FIBITMAP* __stdcall FreeImage_ConvertTo16Bits565(
		FIBITMAP* dib);
	[entry(0x6000008a)]
	FIBITMAP* __stdcall FreeImage_ConvertTo24Bits(
		FIBITMAP* dib);
	[entry(0x6000008b)]
	FIBITMAP* __stdcall FreeImage_ConvertTo32Bits(
		FIBITMAP* dib);
	[entry(0x6000008c)]
	FIBITMAP* __stdcall FreeImage_ColorQuantize(
		FIBITMAP* dib,
		int quantize);
	[entry(0x6000008d)]
	FIBITMAP* __stdcall FreeImage_ColorQuantizeEx(
		FIBITMAP* dib,
		int quantize,
		int PaletteSize,
		int ReserveSize,
		RGBQUAD* ReservePalette);
	[entry(0x6000008e)]
	FIBITMAP* __stdcall FreeImage_Threshold(
		FIBITMAP* dib,
		BYTE T);
	[entry(0x6000008f)]
	FIBITMAP* __stdcall FreeImage_Dither(
		FIBITMAP* dib,
		int algorithm);
	[entry(0x60000090)]
	FIBITMAP* __stdcall FreeImage_ConvertFromRawBits(
		BYTE* bits,
		int width,
		int height,
		int pitch,
		unsigned int bpp,
		unsigned int red_mask,
		unsigned int green_mask,
		unsigned int blue_mask,
		long topdown);
	[entry(0x60000091)]
	void __stdcall FreeImage_ConvertToRawBits(
		BYTE* bits,
		FIBITMAP* dib,
		int pitch,
		unsigned int bpp,
		unsigned int red_mask,
		unsigned int green_mask,
		unsigned int blue_mask,
		long topdown);
	[entry(0x60000092)]
	FIBITMAP* __stdcall FreeImage_ConvertToRGBF(
		FIBITMAP* dib);
	[entry(0x60000093)]
	FIBITMAP* __stdcall FreeImage_ConvertToStandardType(
		FIBITMAP* src,
		long scale_linear);
	[entry(0x60000094)]
	FIBITMAP* __stdcall FreeImage_ConvertToType(
		FIBITMAP* src,
		int dst_type,
		long scale_linear);
	[entry(0x60000095)]
	FIBITMAP* __stdcall FreeImage_ToneMapping(
		FIBITMAP* dib,
		int tmo,
		double first_param,
		double second_param);
	[entry(0x60000096)]
	FIBITMAP* __stdcall FreeImage_TmoDrago03(
		FIBITMAP* src,
		double gamma,
		double exposure);
	[entry(0x60000097)]
	FIBITMAP* __stdcall FreeImage_TmoReinhard05(
		FIBITMAP* src,
		double intensity,
		double contrast);
	[entry(0x60000098)]
	unsigned long __stdcall FreeImage_ZLibCompress(
		BYTE* target,
		unsigned long target_size,
		BYTE* source,
		unsigned long source_size);
	[entry(0x60000099)]
	unsigned long __stdcall FreeImage_ZLibUncompress(
		BYTE* target,
		unsigned long target_size,
		BYTE* source,
		unsigned long source_size);
	[entry(0x6000009a)]
	unsigned long __stdcall FreeImage_ZLibGZip(
		BYTE* target,
		unsigned long target_size,
		BYTE* source,
		unsigned long source_size);
	[entry(0x6000009b)]
	unsigned long __stdcall FreeImage_ZLibGUnzip(
		BYTE* target,
		unsigned long target_size,
		BYTE* source,
		unsigned long source_size);
	[entry(0x6000009c)]
	unsigned long __stdcall FreeImage_ZLibCRC32(
		unsigned long crc,
		BYTE* source,
		unsigned long source_size);
	[entry(0x6000009d)]
	FITAG* __stdcall FreeImage_CreateTag();
	[entry(0x6000009e)]
	void __stdcall FreeImage_DeleteTag(
		FITAG* tag);
	[entry(0x6000009f)]
	FITAG* __stdcall FreeImage_CloneTag(
		FITAG* tag);
	[entry(0x600000a0)]
	CHAR* __stdcall FreeImage_GetTagKey(
		FITAG* tag);
	[entry(0x600000a1)]
	CHAR* __stdcall FreeImage_GetTagDescription(
		FITAG* tag);
	[entry(0x600000a2)]
	unsigned short __stdcall FreeImage_GetTagID(
		FITAG* tag);
	[entry(0x600000a3)]
	int __stdcall FreeImage_GetTagType(
		FITAG* tag);
	[entry(0x600000a4)]
	unsigned long __stdcall FreeImage_GetTagCount(
		FITAG* tag);
	[entry(0x600000a5)]
	unsigned long __stdcall FreeImage_GetTagLength(
		FITAG* tag);
	[entry(0x600000a6)]
	void* __stdcall FreeImage_GetTagValue(
		FITAG* tag);
	[entry(0x600000a7)]
	long __stdcall FreeImage_SetTagKey(
		FITAG* tag,
		CHAR* key);
	[entry(0x600000a8)]
	long __stdcall FreeImage_SetTagDescription(
		FITAG* tag,
		CHAR* description);
	[entry(0x600000a9)]
	long __stdcall FreeImage_SetTagID(
		FITAG* tag,
		unsigned short id);
	[entry(0x600000aa)]
	long __stdcall FreeImage_SetTagType(
		FITAG* tag,
		int type);
	[entry(0x600000ab)]
	long __stdcall FreeImage_SetTagCount(
		FITAG* tag,
		unsigned long count);
	[entry(0x600000ac)]
	long __stdcall FreeImage_SetTagLength(
		FITAG* tag,
		unsigned long length);
	[entry(0x600000ad)]
	long __stdcall FreeImage_SetTagValue(
		FITAG* tag,
		void* value);
	[entry(0x600000ae)]
	FIMETADATA* __stdcall FreeImage_FindFirstMetadata(
		int model,
		FIBITMAP* dib,
		FITAG** tag);
	[entry(0x600000af)]
	long __stdcall FreeImage_FindNextMetadata(
		FIMETADATA* mdhandle,
		FITAG** tag);
	[entry(0x600000b0)]
	void __stdcall FreeImage_FindCloseMetadata(
		FIMETADATA* mdhandle);
	[entry(0x600000b1)]
	long __stdcall FreeImage_SetMetadata(
		int model,
		FIBITMAP* dib,
		CHAR* key,
		FITAG* tag);
	[entry(0x600000b2)]
	long __stdcall FreeImage_GetMetadata(
		int model,
		FIBITMAP* dib,
		CHAR* key,
		FITAG** tag);
	[entry(0x600000b3)]
	unsigned int __stdcall FreeImage_GetMetadataCount(
		int model,
		FIBITMAP* dib);
	[entry(0x600000b4)]
	CHAR* __stdcall FreeImage_TagToString(
		int model,
		FITAG* tag,
		CHAR* Make);
	[entry(0x600000b5)]
	FIBITMAP* __stdcall FreeImage_RotateClassic(
		FIBITMAP* dib,
		double angle);
	[entry(0x600000b6)]
	FIBITMAP* __stdcall FreeImage_RotateEx(
		FIBITMAP* dib,
		double angle,
		double x_shift,
		double y_shift,
		double x_origin,
		double y_origin,
		long use_mask);
	[entry(0x600000b7)]
	long __stdcall FreeImage_FlipHorizontal(
		FIBITMAP* dib);
	[entry(0x600000b8)]
	long __stdcall FreeImage_FlipVertical(
		FIBITMAP* dib);
	[entry(0x600000b9)]
	long __stdcall FreeImage_JPEGTransform(
		CHAR* src_file,
		CHAR* dst_file,
		int operation,
		long perfect);
	[entry(0x600000ba)]
	FIBITMAP* __stdcall FreeImage_Rescale(
		FIBITMAP* dib,
		int dst_width,
		int dst_height,
		int filter);
	[entry(0x600000bb)]
	FIBITMAP* __stdcall FreeImage_MakeThumbnail(
		FIBITMAP* dib,
		int max_pixel_size,
		long convert);
	[entry(0x600000bc)]
	long __stdcall FreeImage_AdjustCurve(
		FIBITMAP* dib,
		BYTE* LUT,
		int channel);
	[entry(0x600000bd)]
	long __stdcall FreeImage_AdjustGamma(
		FIBITMAP* dib,
		double gamma);
	[entry(0x600000be)]
	long __stdcall FreeImage_AdjustBrightness(
		FIBITMAP* dib,
		double percentage);
	[entry(0x600000bf)]
	long __stdcall FreeImage_AdjustContrast(
		FIBITMAP* dib,
		double percentage);
	[entry(0x600000c0)]
	long __stdcall FreeImage_Invert(
		FIBITMAP* dib);
	[entry(0x600000c1)]
	long __stdcall FreeImage_GetHistogram(
		FIBITMAP* dib,
		unsigned long* histo,
		int channel);
	[entry(0x600000c2)]
	FIBITMAP* __stdcall FreeImage_GetChannel(
		FIBITMAP* dib,
		int channel);
	[entry(0x600000c3)]
	long __stdcall FreeImage_SetChannel(
		FIBITMAP* dib,
		FIBITMAP* dib8,
		int channel);
	[entry(0x600000c4)]
	FIBITMAP* __stdcall FreeImage_GetComplexChannel(
		FIBITMAP* src,
		int channel);
	[entry(0x600000c5)]
	long __stdcall FreeImage_SetComplexChannel(
		FIBITMAP* dst,
		FIBITMAP* src,
		int channel);
	[entry(0x600000c6)]
	FIBITMAP* __stdcall FreeImage_Copy(
		FIBITMAP* dib,
		int left,
		int top,
		int right,
		int bottom);
	[entry(0x600000c7)]
	long __stdcall FreeImage_Paste(
		FIBITMAP* dst,
		FIBITMAP* src,
		int left,
		int top,
		int alpha);
	[entry(0x600000c8)]
	FIBITMAP* __stdcall FreeImage_Composite(
		FIBITMAP* fg,
		long useFileBkg,
		RGBQUAD* appBkColor,
		FIBITMAP* bg);
	[entry(0x600000c9)]
	long __stdcall FreeImage_JPEGCrop(
		CHAR* src_file,
		CHAR* dst_file,
		int left,
		int top,
		int right,
		int bottom);
};
'!
!FreeImageLibrary categoriesForClass!Unclassified! !
!FreeImageLibrary methodsFor!

freeImage_AcquireMemory: stream data: data size_in_bytes: size_in_bytes
	"Private - Invoke the FreeImage_AcquireMemory() function of the module wrapped by the receiver.

		long __stdcall FreeImage_AcquireMemory(
			FIMEMORY* stream,
			BYTE** data,
			unsigned long* size_in_bytes);"

	<stdcall: sdword '_FreeImage_AcquireMemory@12' FIMEMORY* byte** dword*>
	^self invalidCall!

freeImage_AdjustBrightness: dib percentage: percentage 
	"Private - Invoke the FreeImage_AdjustBrightness() function of the module wrapped by the receiver.

		long __stdcall FreeImage_AdjustBrightness(
			FIBITMAP* dib,
			double percentage);"

	<stdcall: sdword '_FreeImage_AdjustBrightness@12' FIBITMAP* double>
	^self invalidCall!

freeImage_AdjustContrast: dib percentage: percentage 
	"Private - Invoke the FreeImage_AdjustContrast() function of the module wrapped by the receiver.

		long __stdcall FreeImage_AdjustContrast(
			FIBITMAP* dib,
			double percentage);"

	<stdcall: sdword  '_FreeImage_AdjustContrast@12' FIBITMAP* double>
	^self invalidCall!

freeImage_AdjustCurve: dib lut: lut channel: channel 
	"Private - Invoke the FreeImage_AdjustCurve() function of the module wrapped by the receiver.

		long __stdcall FreeImage_AdjustCurve(
			FIBITMAP* dib,
			BYTE* LUT,
			int channel);"

	<stdcall: bool  '_FreeImage_AdjustCurve@12' FIBITMAP* byte* sdword>
	^self invalidCall!

freeImage_AdjustGamma: dib gamma: gamma 
	"Private - Invoke the FreeImage_AdjustGamma() function of the module wrapped by the receiver.

		long __stdcall FreeImage_AdjustGamma(
			FIBITMAP* dib,
			double gamma);"

	<stdcall: bool  '_FreeImage_AdjustGamma@12' FIBITMAP* double>
	^self invalidCall!

freeImage_Allocate: width height: height bpp: bpp 
^self freeImage_Allocate: width height: height bpp: bpp red_mask: 0 green_mask: 0 blue_mask: 0 
!

freeImage_Allocate: width height: height bpp: bpp red_mask: red_mask green_mask: green_mask blue_mask: blue_mask
	"Private - Invoke the FreeImage_Allocate() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_Allocate(
			int width,
			int height,
			int bpp,
			unsigned int red_mask,
			unsigned int green_mask,
			unsigned int blue_mask);"

	<stdcall: FIBITMAP* '_FreeImage_Allocate@24' sdword sdword sdword dword dword dword>
	^self invalidCall!

freeImage_AllocateT: type width: width height: height bpp: bpp
^self freeImage_AllocateT: type width: width height: height bpp: bpp red_mask: 0 green_mask: 0 blue_mask: 0 !

freeImage_AllocateT: type width: width height: height bpp: bpp red_mask: red_mask green_mask: green_mask blue_mask: blue_mask
	"Private - Invoke the FreeImage_AllocateT() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_AllocateT(
			int type,
			int width,
			int height,
			int bpp,
			unsigned int red_mask,
			unsigned int green_mask,
			unsigned int blue_mask);"

	<stdcall: FIBITMAP* '_FreeImage_AllocateT@28' sdword sdword sdword sdword dword dword dword>
	^self invalidCall!

freeImage_AppendPage: bitmap data: data
	"Private - Invoke the FreeImage_AppendPage() function of the module wrapped by the receiver.

		void __stdcall FreeImage_AppendPage(
			FIMULTIBITMAP* bitmap,
			FIBITMAP* data);"

	<stdcall: void '_FreeImage_AppendPage@8' FIMULTIBITMAP* FIBITMAP*>
	^self invalidCall!

freeImage_Clone: dib
	"Private - Invoke the FreeImage_Clone() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_Clone(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_Clone@4' FIBITMAP*>
	^self invalidCall!

freeImage_CloneTag: tag
	"Private - Invoke the FreeImage_CloneTag() function of the module wrapped by the receiver.

		FITAG* __stdcall FreeImage_CloneTag(
			FITAG* tag);"

	<stdcall: FITAG* '_FreeImage_CloneTag@4' FITAG*>
	^self invalidCall!

freeImage_CloseMemory: stream
	"Private - Invoke the FreeImage_CloseMemory() function of the module wrapped by the receiver.

		void __stdcall FreeImage_CloseMemory(
			FIMEMORY* stream);"

	<stdcall: void '_FreeImage_CloseMemory@4' FIMEMORY*>
	^self invalidCall!

freeImage_CloseMultiBitmap: bitmap flags: flags
	"Private - Invoke the FreeImage_CloseMultiBitmap() function of the module wrapped by the receiver.

		long __stdcall FreeImage_CloseMultiBitmap(
			FIMULTIBITMAP* bitmap,
			int flags);"

	<stdcall: sdword '_FreeImage_CloseMultiBitmap@8' FIMULTIBITMAP* sdword>
	^self invalidCall!

freeImage_ColorQuantize: dib quantize: quantize
	"Private - Invoke the FreeImage_ColorQuantize() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ColorQuantize(
			FIBITMAP* dib,
			int quantize);"

	<stdcall: FIBITMAP* '_FreeImage_ColorQuantize@8' FIBITMAP* sdword>
	^self invalidCall!

freeImage_ColorQuantizeEx: dib quantize: quantize paletteSize: paletteSize reserveSize: reserveSize reservePalette: reservePalette
	"Private - Invoke the FreeImage_ColorQuantizeEx() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ColorQuantizeEx(
			FIBITMAP* dib,
			int quantize,
			int PaletteSize,
			int ReserveSize,
			RGBQUAD* ReservePalette);"

	<stdcall: FIBITMAP* '_FreeImage_ColorQuantizeEx@20' FIBITMAP* sdword sdword sdword RGBQUAD*>
	^self invalidCall!

freeImage_Composite: fg useFileBkg: useFileBkg appBkColor: appBkColor bg: bg 
	"Private - Invoke the FreeImage_Composite() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_Composite(
			FIBITMAP* fg,
			long useFileBkg,
			RGBQUAD* appBkColor,
			FIBITMAP* bg);"

	<stdcall: FIBITMAP* '_FreeImage_Composite@16' FIBITMAP* bool RGBQUAD* FIBITMAP*>
	^self invalidCall!

freeImage_ConvertFromRawBits: bits width: width height: height pitch: pitch bpp: bpp red_mask: red_mask green_mask: green_mask blue_mask: blue_mask topdown: topdown
	"Private - Invoke the FreeImage_ConvertFromRawBits() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertFromRawBits(
			BYTE* bits,
			int width,
			int height,
			int pitch,
			unsigned int bpp,
			unsigned int red_mask,
			unsigned int green_mask,
			unsigned int blue_mask,
			long topdown);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertFromRawBits@36' byte* sdword sdword sdword dword dword dword dword sdword>
	^self invalidCall!

freeImage_ConvertLine16_555_To16_565: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16_555_To16_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16_555_To16_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16_555_To16_565@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16_565_To16_555: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16_565_To16_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16_565_To16_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16_565_To16_555@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16To24_555: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16To24_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16To24_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16To24_555@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16To24_565: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16To24_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16To24_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16To24_565@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16To32_555: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16To32_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16To32_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16To32_555@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16To32_565: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16To32_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16To32_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16To32_565@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16To4_555: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16To4_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16To4_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16To4_555@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16To4_565: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16To4_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16To4_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16To4_565@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16To8_555: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16To8_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16To8_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16To8_555@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine16To8_565: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine16To8_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine16To8_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine16To8_565@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine1To16_555: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine1To16_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine1To16_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine1To16_555@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine1To16_565: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine1To16_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine1To16_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine1To16_565@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine1To24: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine1To24() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine1To24(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine1To24@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine1To32: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine1To32() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine1To32(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine1To32@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine1To4: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine1To4() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine1To4(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine1To4@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine1To8: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine1To8() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine1To8(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine1To8@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine24To16_555: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine24To16_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine24To16_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine24To16_555@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine24To16_565: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine24To16_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine24To16_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine24To16_565@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine24To32: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine24To32() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine24To32(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine24To32@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine24To4: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine24To4() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine24To4(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine24To4@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine24To8: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine24To8() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine24To8(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine24To8@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine32To16_555: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine32To16_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine32To16_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine32To16_555@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine32To16_565: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine32To16_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine32To16_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine32To16_565@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine32To24: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine32To24() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine32To24(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine32To24@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine32To4: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine32To4() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine32To4(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine32To4@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine32To8: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine32To8() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine32To8(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine32To8@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine4To16_555: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine4To16_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine4To16_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine4To16_555@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine4To16_565: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine4To16_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine4To16_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine4To16_565@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine4To24: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine4To24() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine4To24(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine4To24@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine4To32: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine4To32() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine4To32(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine4To32@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine4To8: target source: source width_in_pixels: width_in_pixels
	"Private - Invoke the FreeImage_ConvertLine4To8() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine4To8(
			BYTE* target,
			BYTE* source,
			int width_in_pixels);"

	<stdcall: void '_FreeImage_ConvertLine4To8@12' byte* byte* sdword>
	^self invalidCall!

freeImage_ConvertLine8To16_555: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine8To16_555() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine8To16_555(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine8To16_555@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine8To16_565: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine8To16_565() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine8To16_565(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine8To16_565@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine8To24: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine8To24() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine8To24(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine8To24@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine8To32: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine8To32() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine8To32(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine8To32@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertLine8To4: target source: source width_in_pixels: width_in_pixels palette: palette
	"Private - Invoke the FreeImage_ConvertLine8To4() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertLine8To4(
			BYTE* target,
			BYTE* source,
			int width_in_pixels,
			RGBQUAD* palette);"

	<stdcall: void '_FreeImage_ConvertLine8To4@16' byte* byte* sdword RGBQUAD*>
	^self invalidCall!

freeImage_ConvertTo16Bits555: dib
	"Private - Invoke the FreeImage_ConvertTo16Bits555() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertTo16Bits555(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertTo16Bits555@4' FIBITMAP*>
	^self invalidCall!

freeImage_ConvertTo16Bits565: dib
	"Private - Invoke the FreeImage_ConvertTo16Bits565() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertTo16Bits565(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertTo16Bits565@4' FIBITMAP*>
	^self invalidCall!

freeImage_ConvertTo24Bits: dib
	"Private - Invoke the FreeImage_ConvertTo24Bits() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertTo24Bits(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertTo24Bits@4' FIBITMAP*>
	^self invalidCall!

freeImage_ConvertTo32Bits: dib
	"Private - Invoke the FreeImage_ConvertTo32Bits() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertTo32Bits(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertTo32Bits@4' FIBITMAP*>
	^self invalidCall!

freeImage_ConvertTo4Bits: dib
	"Private - Invoke the FreeImage_ConvertTo4Bits() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertTo4Bits(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertTo4Bits@4' FIBITMAP*>
	^self invalidCall!

freeImage_ConvertTo8Bits: dib
	"Private - Invoke the FreeImage_ConvertTo8Bits() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertTo8Bits(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertTo8Bits@4' FIBITMAP*>
	^self invalidCall!

freeImage_ConvertToGreyscale: dib
	"Private - Invoke the FreeImage_ConvertToGreyscale() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertToGreyscale(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertToGreyscale@4' FIBITMAP*>
	^self invalidCall!

freeImage_ConvertToRawBits: bits dib: dib pitch: pitch bpp: bpp red_mask: red_mask green_mask: green_mask blue_mask: blue_mask topdown: topdown 
	"Private - Invoke the FreeImage_ConvertToRawBits() function of the module wrapped by the receiver.

		void __stdcall FreeImage_ConvertToRawBits(
			BYTE* bits,
			FIBITMAP* dib,
			int pitch,
			unsigned int bpp,
			unsigned int red_mask,
			unsigned int green_mask,
			unsigned int blue_mask,
			long topdown);"

	<stdcall: void '_FreeImage_ConvertToRawBits@32' byte* FIBITMAP* sdword dword dword dword dword bool>
	^self invalidCall!

freeImage_ConvertToRGBF: dib
	"Private - Invoke the FreeImage_ConvertToRGBF() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertToRGBF(
			FIBITMAP* dib);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertToRGBF@4' FIBITMAP*>
	^self invalidCall!

freeImage_ConvertToStandardType: src scale_linear: scale_linear 
	"Private - Invoke the FreeImage_ConvertToStandardType() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertToStandardType(
			FIBITMAP* src,
			long scale_linear);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertToStandardType@8' FIBITMAP* bool>
	^self invalidCall!

freeImage_ConvertToType: src dst_type: dst_type scale_linear: scale_linear
	"Private - Invoke the FreeImage_ConvertToType() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ConvertToType(
			FIBITMAP* src,
			int dst_type,
			long scale_linear);"

	<stdcall: FIBITMAP* '_FreeImage_ConvertToType@12' FIBITMAP* sdword sdword>
	^self invalidCall!

freeImage_Copy: dib left: left top: top right: right bottom: bottom
	"Private - Invoke the FreeImage_Copy() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_Copy(
			FIBITMAP* dib,
			int left,
			int top,
			int right,
			int bottom);"

	<stdcall: FIBITMAP* '_FreeImage_Copy@20' FIBITMAP* sdword sdword sdword sdword>
	^self invalidCall!

freeImage_CreateICCProfile: dib data: data size: size
	"Private - Invoke the FreeImage_CreateICCProfile() function of the module wrapped by the receiver.

		FIICCPROFILE* __stdcall FreeImage_CreateICCProfile(
			FIBITMAP* dib,
			void* data,
			long size);"

	<stdcall: FIICCPROFILE* '_FreeImage_CreateICCProfile@12' FIBITMAP* void* sdword>
	^self invalidCall!

freeImage_CreateTag
	"Private - Invoke the FreeImage_CreateTag() function of the module wrapped by the receiver.

		FITAG* __stdcall FreeImage_CreateTag();"

	<stdcall: FITAG* '_FreeImage_CreateTag@0'>
	^self invalidCall!

freeImage_DeInitialise
	"Private - Invoke the FreeImage_DeInitialise() function of the module wrapped by the receiver.

		void __stdcall FreeImage_DeInitialise();"

	<stdcall: void '_FreeImage_DeInitialise@0'>
	^self invalidCall!

freeImage_DeletePage: bitmap page: page
	"Private - Invoke the FreeImage_DeletePage() function of the module wrapped by the receiver.

		void __stdcall FreeImage_DeletePage(
			FIMULTIBITMAP* bitmap,
			int page);"

	<stdcall: void '_FreeImage_DeletePage@8' FIMULTIBITMAP* sdword>
	^self invalidCall!

freeImage_DeleteTag: tag
	"Private - Invoke the FreeImage_DeleteTag() function of the module wrapped by the receiver.

		void __stdcall FreeImage_DeleteTag(
			FITAG* tag);"

	<stdcall: void '_FreeImage_DeleteTag@4' FITAG*>
	^self invalidCall!

freeImage_DestroyICCProfile: dib
	"Private - Invoke the FreeImage_DestroyICCProfile() function of the module wrapped by the receiver.

		void __stdcall FreeImage_DestroyICCProfile(
			FIBITMAP* dib);"

	<stdcall: void '_FreeImage_DestroyICCProfile@4' FIBITMAP*>
	^self invalidCall!

freeImage_Dither: dib algorithm: algorithm
	"Private - Invoke the FreeImage_Dither() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_Dither(
			FIBITMAP* dib,
			int algorithm);"

	<stdcall: FIBITMAP* '_FreeImage_Dither@8' FIBITMAP* sdword>
	^self invalidCall!

freeImage_FIFSupportsExportBPP: fif bpp: bpp 
	"Private - Invoke the FreeImage_FIFSupportsExportBPP() function of the module wrapped by the receiver.

		long __stdcall FreeImage_FIFSupportsExportBPP(
			int fif,
			int bpp);"

	<stdcall: sdword  '_FreeImage_FIFSupportsExportBPP@8' sdword sdword>
	^self invalidCall!

freeImage_FIFSupportsExportType: fif type: type
	"Private - Invoke the FreeImage_FIFSupportsExportType() function of the module wrapped by the receiver.

		long __stdcall FreeImage_FIFSupportsExportType(
			int fif,
			int type);"

	<stdcall: sdword '_FreeImage_FIFSupportsExportType@8' sdword sdword>
	^self invalidCall!

freeImage_FIFSupportsICCProfiles: fif 
	"Private - Invoke the FreeImage_FIFSupportsICCProfiles() function of the module wrapped by the receiver.

		long __stdcall FreeImage_FIFSupportsICCProfiles(
			int fif);"

	<stdcall: sdword '_FreeImage_FIFSupportsICCProfiles@4' sdword>
	^self invalidCall!

freeImage_FIFSupportsReading: fif 
	"Private - Invoke the FreeImage_FIFSupportsReading() function of the module wrapped by the receiver.

		long __stdcall FreeImage_FIFSupportsReading(
			int fif);"

	<stdcall: sdword '_FreeImage_FIFSupportsReading@4' sdword>
	^self invalidCall!

freeImage_FIFSupportsWriting: fif 
	"Private - Invoke the FreeImage_FIFSupportsWriting() function of the module wrapped by the receiver.

		long __stdcall FreeImage_FIFSupportsWriting(
			int fif);"

	<stdcall: sdword '_FreeImage_FIFSupportsWriting@4' sdword>
	^self invalidCall!

freeImage_FindCloseMetadata: mdhandle
	"Private - Invoke the FreeImage_FindCloseMetadata() function of the module wrapped by the receiver.

		void __stdcall FreeImage_FindCloseMetadata(
			FIMETADATA* mdhandle);"

	<stdcall: void '_FreeImage_FindCloseMetadata@4' FIMETADATA*>
	^self invalidCall!

freeImage_FindFirstMetadata: model dib: dib tag: tag
	"Private - Invoke the FreeImage_FindFirstMetadata() function of the module wrapped by the receiver.

		FIMETADATA* __stdcall FreeImage_FindFirstMetadata(
			int model,
			FIBITMAP* dib,
			FITAG** tag);"

	<stdcall: FIMETADATA* '_FreeImage_FindFirstMetadata@12' sdword FIBITMAP* FITAG**>
	^self invalidCall!

freeImage_FindNextMetadata: mdhandle tag: tag 
	"Private - Invoke the FreeImage_FindNextMetadata() function of the module wrapped by the receiver.

		long __stdcall FreeImage_FindNextMetadata(
			FIMETADATA* mdhandle,
			FITAG** tag);"

	<stdcall: bool '_FreeImage_FindNextMetadata@8' FIMETADATA* FITAG**>
	^self invalidCall!

freeImage_FlipHorizontal: dib
	"Private - Invoke the FreeImage_FlipHorizontal() function of the module wrapped by the receiver.

		long __stdcall FreeImage_FlipHorizontal(
			FIBITMAP* dib);"

	<stdcall: sdword '_FreeImage_FlipHorizontal@4' FIBITMAP*>
	^self invalidCall!

freeImage_FlipVertical: dib
	"Private - Invoke the FreeImage_FlipVertical() function of the module wrapped by the receiver.

		long __stdcall FreeImage_FlipVertical(
			FIBITMAP* dib);"

	<stdcall: sdword '_FreeImage_FlipVertical@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetBackgroundColor: dib bkcolor: bkcolor 
	"Private - Invoke the FreeImage_GetBackgroundColor() function of the module wrapped by the receiver.

		long __stdcall FreeImage_GetBackgroundColor(
			FIBITMAP* dib,
			RGBQUAD* bkcolor);"

	<stdcall: sdword '_FreeImage_GetBackgroundColor@8' FIBITMAP* RGBQUAD*>
	^self invalidCall!

freeImage_GetBits: dib
	"Private - Invoke the FreeImage_GetBits() function of the module wrapped by the receiver.

		BYTE* __stdcall FreeImage_GetBits(
			FIBITMAP* dib);"

	<stdcall: byte* '_FreeImage_GetBits@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetBlueMask: dib
	"Private - Invoke the FreeImage_GetBlueMask() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetBlueMask(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetBlueMask@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetBPP: dib
	"Private - Invoke the FreeImage_GetBPP() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetBPP(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetBPP@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetChannel: dib channel: channel
	"Private - Invoke the FreeImage_GetChannel() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_GetChannel(
			FIBITMAP* dib,
			int channel);"

	<stdcall: FIBITMAP* '_FreeImage_GetChannel@8' FIBITMAP* sdword>
	^self invalidCall!

freeImage_GetColorsUsed: dib
	"Private - Invoke the FreeImage_GetColorsUsed() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetColorsUsed(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetColorsUsed@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetColorType: dib
	"Private - Invoke the FreeImage_GetColorType() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetColorType(
			FIBITMAP* dib);"

	<stdcall: sdword '_FreeImage_GetColorType@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetComplexChannel: src channel: channel
	"Private - Invoke the FreeImage_GetComplexChannel() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_GetComplexChannel(
			FIBITMAP* src,
			int channel);"

	<stdcall: FIBITMAP* '_FreeImage_GetComplexChannel@8' FIBITMAP* sdword>
	^self invalidCall!

freeImage_GetCopyrightMessage
	"Private - Invoke the FreeImage_GetCopyrightMessage() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetCopyrightMessage();"

	<stdcall: char* '_FreeImage_GetCopyrightMessage@0'>
	^self invalidCall!

freeImage_GetDIBSize: dib
	"Private - Invoke the FreeImage_GetDIBSize() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetDIBSize(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetDIBSize@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetDotsPerMeterX: dib
	"Private - Invoke the FreeImage_GetDotsPerMeterX() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetDotsPerMeterX(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetDotsPerMeterX@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetDotsPerMeterY: dib
	"Private - Invoke the FreeImage_GetDotsPerMeterY() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetDotsPerMeterY(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetDotsPerMeterY@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetFIFCount
	"Private - Invoke the FreeImage_GetFIFCount() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFIFCount();"

	<stdcall: sdword '_FreeImage_GetFIFCount@0'>
	^self invalidCall!

freeImage_GetFIFDescription: fif 
	"Private - Invoke the FreeImage_GetFIFDescription() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetFIFDescription(
			int fif);"

	<stdcall: char* '_FreeImage_GetFIFDescription@4' sdword>
	^self invalidCall!

freeImage_GetFIFExtensionList: fif 
	"Private - Invoke the FreeImage_GetFIFExtensionList() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetFIFExtensionList(
			int fif);"

	<stdcall: char* '_FreeImage_GetFIFExtensionList@4' sdword>
	^self invalidCall!

freeImage_GetFIFFromFilename: filename 
	"Private - Invoke the FreeImage_GetFIFFromFilename() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFIFFromFilename(
			CHAR* filename);"

	<stdcall: sdword '_FreeImage_GetFIFFromFilename@4' char*>
	^self invalidCall!

freeImage_GetFIFFromFilenameU: filename
	"Private - Invoke the FreeImage_GetFIFFromFilenameU() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFIFFromFilenameU(
			unsigned short* filename);"

	<stdcall: sdword '_FreeImage_GetFIFFromFilenameU@4' word*>
	^self invalidCall!

freeImage_GetFIFFromFormat: format 
	"Private - Invoke the FreeImage_GetFIFFromFormat() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFIFFromFormat(
			CHAR* format);"

	<stdcall: sdword '_FreeImage_GetFIFFromFormat@4' char*>
	^self invalidCall!

freeImage_GetFIFFromMime: mime 
	"Private - Invoke the FreeImage_GetFIFFromMime() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFIFFromMime(
			CHAR* mime);"

	<stdcall: sdword '_FreeImage_GetFIFFromMime@4' char*>
	^self invalidCall!

freeImage_GetFIFMimeType: fif 
	"Private - Invoke the FreeImage_GetFIFMimeType() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetFIFMimeType(
			int fif);"

	<stdcall: char* '_FreeImage_GetFIFMimeType@4' sdword>
	^self invalidCall!

freeImage_GetFIFRegExpr: fif 
	"Private - Invoke the FreeImage_GetFIFRegExpr() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetFIFRegExpr(
			int fif);"

	<stdcall: char* '_FreeImage_GetFIFRegExpr@4' sdword>
	^self invalidCall!

freeImage_GetFileType: filename size: size 
	"Private - Invoke the FreeImage_GetFileType() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFileType(
			CHAR* filename,
			int size);"

	<stdcall: sdword '_FreeImage_GetFileType@8' char* sdword>
	^self invalidCall!

freeImage_GetFileTypeFromHandle: io handle: handle1 size: size
	"Private - Invoke the FreeImage_GetFileTypeFromHandle() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFileTypeFromHandle(
			FreeImageIO* io,
			void* handle,
			int size);"

	<stdcall: sdword '_FreeImage_GetFileTypeFromHandle@12' FreeImageIO* void* sdword>
	^self invalidCall!

freeImage_GetFileTypeFromMemory: stream size: size
	"Private - Invoke the FreeImage_GetFileTypeFromMemory() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFileTypeFromMemory(
			FIMEMORY* stream,
			int size);"

	<stdcall: sdword '_FreeImage_GetFileTypeFromMemory@8' FIMEMORY* sdword>
	^self invalidCall!

freeImage_GetFileTypeU: filename size: size
	"Private - Invoke the FreeImage_GetFileTypeU() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetFileTypeU(
			unsigned short* filename,
			int size);"

	<stdcall: sdword '_FreeImage_GetFileTypeU@8' word* sdword>
	^self invalidCall!

freeImage_GetFormatFromFIF: fif 
	"Private - Invoke the FreeImage_GetFormatFromFIF() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetFormatFromFIF(
			int fif);"

	<stdcall: char* '_FreeImage_GetFormatFromFIF@4' sdword>
	^self invalidCall!

freeImage_GetGreenMask: dib
	"Private - Invoke the FreeImage_GetGreenMask() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetGreenMask(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetGreenMask@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetHeight: dib
	"Private - Invoke the FreeImage_GetHeight() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetHeight(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetHeight@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetHistogram: dib histo: histo channel: channel 
	"Private - Invoke the FreeImage_GetHistogram() function of the module wrapped by the receiver.

		long __stdcall FreeImage_GetHistogram(
			FIBITMAP* dib,
			unsigned long* histo,
			int channel);"

	<stdcall: bool '_FreeImage_GetHistogram@12' FIBITMAP* dword* sdword>
	^self invalidCall!

freeImage_GetICCProfile: dib
	"Private - Invoke the FreeImage_GetICCProfile() function of the module wrapped by the receiver.

		FIICCPROFILE* __stdcall FreeImage_GetICCProfile(
			FIBITMAP* dib);"

	<stdcall: FIICCPROFILE* '_FreeImage_GetICCProfile@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetImageType: dib
	"Private - Invoke the FreeImage_GetImageType() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetImageType(
			FIBITMAP* dib);"

	<stdcall: sdword '_FreeImage_GetImageType@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetInfo: dib
	"Private - Invoke the FreeImage_GetInfo() function of the module wrapped by the receiver.

		BITMAPINFO* __stdcall FreeImage_GetInfo(
			FIBITMAP* dib);"

	<stdcall: BITMAPINFO* '_FreeImage_GetInfo@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetInfoHeader: dib
	"Private - Invoke the FreeImage_GetInfoHeader() function of the module wrapped by the receiver.

		BITMAPINFOHEADER* __stdcall FreeImage_GetInfoHeader(
			FIBITMAP* dib);"

	<stdcall: BITMAPINFOHEADER* '_FreeImage_GetInfoHeader@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetLine: dib
	"Private - Invoke the FreeImage_GetLine() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetLine(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetLine@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetLockedPageNumbers: bitmap pages: pages count: count
	"Private - Invoke the FreeImage_GetLockedPageNumbers() function of the module wrapped by the receiver.

		long __stdcall FreeImage_GetLockedPageNumbers(
			FIMULTIBITMAP* bitmap,
			int* pages,
			int* count);"

	<stdcall: sdword '_FreeImage_GetLockedPageNumbers@12' FIMULTIBITMAP* sdword* sdword*>
	^self invalidCall!

freeImage_GetMetadata: model dib: dib key: key tag: tag 
	"Private - Invoke the FreeImage_GetMetadata() function of the module wrapped by the receiver.

		long __stdcall FreeImage_GetMetadata(
			int model,
			FIBITMAP* dib,
			CHAR* key,
			FITAG** tag);"

	<stdcall: bool '_FreeImage_GetMetadata@16' sdword FIBITMAP* char* FITAG**>
	^self invalidCall!

freeImage_GetMetadataCount: model dib: dib
	"Private - Invoke the FreeImage_GetMetadataCount() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetMetadataCount(
			int model,
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetMetadataCount@8' sdword FIBITMAP*>
	^self invalidCall!

freeImage_GetPageCount: bitmap
	"Private - Invoke the FreeImage_GetPageCount() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetPageCount(
			FIMULTIBITMAP* bitmap);"

	<stdcall: sdword '_FreeImage_GetPageCount@4' FIMULTIBITMAP*>
	^self invalidCall!

freeImage_GetPalette: dib 
	"Private - Invoke the FreeImage_GetPalette() function of the module wrapped by the receiver.

		RGBQUAD* __stdcall FreeImage_GetPalette(
			FIBITMAP* dib);"

	<stdcall: lpvoid '_FreeImage_GetPalette@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetPitch: dib
	"Private - Invoke the FreeImage_GetPitch() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetPitch(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetPitch@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetPixelColor: dib x: x y: y value: value 
	"Private - Invoke the FreeImage_GetPixelColor() function of the module wrapped by the receiver.

		long __stdcall FreeImage_GetPixelColor(
			FIBITMAP* dib,
			unsigned int x,
			unsigned int y,
			RGBQUAD* value);"

	<stdcall: bool '_FreeImage_GetPixelColor@16' FIBITMAP* dword dword RGBQUAD*>
	^self invalidCall!

freeImage_GetPixelIndex: dib x: x y: y value: value 
	"Private - Invoke the FreeImage_GetPixelIndex() function of the module wrapped by the receiver.

		long __stdcall FreeImage_GetPixelIndex(
			FIBITMAP* dib,
			unsigned int x,
			unsigned int y,
			BYTE* value);"

	<stdcall: bool '_FreeImage_GetPixelIndex@16' FIBITMAP* dword dword byte*>
	^self invalidCall!

freeImage_GetRedMask: dib
	"Private - Invoke the FreeImage_GetRedMask() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetRedMask(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetRedMask@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetScanLine: dib scanline: scanline
	"Private - Invoke the FreeImage_GetScanLine() function of the module wrapped by the receiver.

		BYTE* __stdcall FreeImage_GetScanLine(
			FIBITMAP* dib,
			int scanline);"

	<stdcall: byte* '_FreeImage_GetScanLine@8' FIBITMAP* sdword>
	^self invalidCall!

freeImage_GetTagCount: tag
	"Private - Invoke the FreeImage_GetTagCount() function of the module wrapped by the receiver.

		unsigned long __stdcall FreeImage_GetTagCount(
			FITAG* tag);"

	<stdcall: dword '_FreeImage_GetTagCount@4' FITAG*>
	^self invalidCall!

freeImage_GetTagDescription: tag 
	"Private - Invoke the FreeImage_GetTagDescription() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetTagDescription(
			FITAG* tag);"

	<stdcall: char* '_FreeImage_GetTagDescription@4' FITAG*>
	^self invalidCall!

freeImage_GetTagID: tag
	"Private - Invoke the FreeImage_GetTagID() function of the module wrapped by the receiver.

		unsigned short __stdcall FreeImage_GetTagID(
			FITAG* tag);"

	<stdcall: word '_FreeImage_GetTagID@4' FITAG*>
	^self invalidCall!

freeImage_GetTagKey: tag 
	"Private - Invoke the FreeImage_GetTagKey() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetTagKey(
			FITAG* tag);"

	<stdcall: char* '_FreeImage_GetTagKey@4' FITAG*>
	^self invalidCall!

freeImage_GetTagLength: tag
	"Private - Invoke the FreeImage_GetTagLength() function of the module wrapped by the receiver.

		unsigned long __stdcall FreeImage_GetTagLength(
			FITAG* tag);"

	<stdcall: dword '_FreeImage_GetTagLength@4' FITAG*>
	^self invalidCall!

freeImage_GetTagType: tag
	"Private - Invoke the FreeImage_GetTagType() function of the module wrapped by the receiver.

		int __stdcall FreeImage_GetTagType(
			FITAG* tag);"

	<stdcall: sdword '_FreeImage_GetTagType@4' FITAG*>
	^self invalidCall!

freeImage_GetTagValue: tag
	"Private - Invoke the FreeImage_GetTagValue() function of the module wrapped by the receiver.

		void* __stdcall FreeImage_GetTagValue(
			FITAG* tag);"

	<stdcall: void* '_FreeImage_GetTagValue@4' FITAG*>
	^self invalidCall!

freeImage_GetTransparencyCount: dib
	"Private - Invoke the FreeImage_GetTransparencyCount() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetTransparencyCount(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetTransparencyCount@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetTransparencyTable: dib
	"Private - Invoke the FreeImage_GetTransparencyTable() function of the module wrapped by the receiver.

		BYTE* __stdcall FreeImage_GetTransparencyTable(
			FIBITMAP* dib);"

	<stdcall: byte* '_FreeImage_GetTransparencyTable@4' FIBITMAP*>
	^self invalidCall!

freeImage_GetVersion
	"Private - Invoke the FreeImage_GetVersion() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_GetVersion();"

	<stdcall: char* '_FreeImage_GetVersion@0'>
	^self invalidCall!

freeImage_GetWidth: dib
	"Private - Invoke the FreeImage_GetWidth() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_GetWidth(
			FIBITMAP* dib);"

	<stdcall: dword '_FreeImage_GetWidth@4' FIBITMAP*>
	^self invalidCall!

freeImage_HasBackgroundColor: dib 
	"Private - Invoke the FreeImage_HasBackgroundColor() function of the module wrapped by the receiver.

		long __stdcall FreeImage_HasBackgroundColor(
			FIBITMAP* dib);"

	<stdcall: sdword '_FreeImage_HasBackgroundColor@4' FIBITMAP*>
	^self invalidCall!

freeImage_Initialise: load_local_plugins_only
	"Private - Invoke the FreeImage_Initialise() function of the module wrapped by the receiver.

		void __stdcall FreeImage_Initialise(
			long load_local_plugins_only);"

	<stdcall: void '_FreeImage_Initialise@4' sdword>
	^self invalidCall!

freeImage_InsertPage: bitmap page: page data: data
	"Private - Invoke the FreeImage_InsertPage() function of the module wrapped by the receiver.

		void __stdcall FreeImage_InsertPage(
			FIMULTIBITMAP* bitmap,
			int page,
			FIBITMAP* data);"

	<stdcall: void '_FreeImage_InsertPage@12' FIMULTIBITMAP* sdword FIBITMAP*>
	^self invalidCall!

freeImage_Invert: dib 
	"Private - Invoke the FreeImage_Invert() function of the module wrapped by the receiver.

		long __stdcall FreeImage_Invert(
			FIBITMAP* dib);"

	<stdcall: sdword  '_FreeImage_Invert@4' FIBITMAP*>
	^self invalidCall!

freeImage_IsLittleEndian
	"Private - Invoke the FreeImage_IsLittleEndian() function of the module wrapped by the receiver.

		long __stdcall FreeImage_IsLittleEndian();"

	<stdcall: sdword '_FreeImage_IsLittleEndian@0'>
	^self invalidCall!

freeImage_IsPluginEnabled: fif
	"Private - Invoke the FreeImage_IsPluginEnabled() function of the module wrapped by the receiver.

		int __stdcall FreeImage_IsPluginEnabled(
			int fif);"

	<stdcall: sdword '_FreeImage_IsPluginEnabled@4' sdword>
	^self invalidCall!

freeImage_IsTransparent: dib 
	"Private - Invoke the FreeImage_IsTransparent() function of the module wrapped by the receiver.

		long __stdcall FreeImage_IsTransparent(
			FIBITMAP* dib);"

	<stdcall: sdword '_FreeImage_IsTransparent@4' FIBITMAP*>
	^self invalidCall!

freeImage_JPEGCrop: src_file dst_file: dst_file left: left top: top right: right bottom: bottom 
	"Private - Invoke the FreeImage_JPEGCrop() function of the module wrapped by the receiver.

		long __stdcall FreeImage_JPEGCrop(
			CHAR* src_file,
			CHAR* dst_file,
			int left,
			int top,
			int right,
			int bottom);"

	<stdcall: bool '_FreeImage_JPEGCrop@24' char* char* sdword sdword sdword sdword>
	^self invalidCall!

freeImage_JPEGTransform: src_file dst_file: dst_file operation: operation perfect: perfect 
	"Private - Invoke the FreeImage_JPEGTransform() function of the module wrapped by the receiver.

		long __stdcall FreeImage_JPEGTransform(
			CHAR* src_file,
			CHAR* dst_file,
			int operation,
			long perfect);"

	<stdcall: sdword '_FreeImage_JPEGTransform@16' char* char* sdword bool>
	^self invalidCall!

freeImage_Load: fif filename: filename flags: flags
	"Private - Invoke the FreeImage_Load() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_Load(
			int fif,
			CHAR* filename,
			int flags);"

	<stdcall: FIBITMAP* '_FreeImage_Load@12' sdword sbyte* sdword>
	^self invalidCall!

freeImage_LoadFromHandle: fif io: io handle: handle1 flags: flags
	"Private - Invoke the FreeImage_LoadFromHandle() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_LoadFromHandle(
			int fif,
			FreeImageIO* io,
			void* handle,
			int flags);"

	<stdcall: FIBITMAP* '_FreeImage_LoadFromHandle@16' sdword FreeImageIO* void* sdword>
	^self invalidCall!

freeImage_LoadFromMemory: fif stream: stream flags: flags
	"Private - Invoke the FreeImage_LoadFromMemory() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_LoadFromMemory(
			int fif,
			FIMEMORY* stream,
			int flags);"

	<stdcall: FIBITMAP* '_FreeImage_LoadFromMemory@12' sdword FIMEMORY* sdword>
	^self invalidCall!

freeImage_LoadMultiBitmapFromMemory: fif stream: stream flags: flags
	"Private - Invoke the FreeImage_LoadMultiBitmapFromMemory() function of the module wrapped by the receiver.

		FIMULTIBITMAP* __stdcall FreeImage_LoadMultiBitmapFromMemory(
			int fif,
			FIMEMORY* stream,
			int flags);"

	<stdcall: FIMULTIBITMAP* '_FreeImage_LoadMultiBitmapFromMemory@12' sdword FIMEMORY* sdword>
	^self invalidCall!

freeImage_LoadU: fif filename: filename flags: flags
	"Private - Invoke the FreeImage_LoadU() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_LoadU(
			int fif,
			unsigned short* filename,
			int flags);"

	<stdcall: FIBITMAP* '_FreeImage_LoadU@12' sdword word* sdword>
	^self invalidCall!

freeImage_LockPage: bitmap page: page
	"Private - Invoke the FreeImage_LockPage() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_LockPage(
			FIMULTIBITMAP* bitmap,
			int page);"

	<stdcall: FIBITMAP* '_FreeImage_LockPage@8' FIMULTIBITMAP* sdword>
	^self invalidCall!

freeImage_LookupSVGColor: szColor nRed: nRed nGreen: nGreen nBlue: nBlue 
	"Private - Invoke the FreeImage_LookupSVGColor() function of the module wrapped by the receiver.

		long __stdcall FreeImage_LookupSVGColor(
			CHAR* szColor,
			BYTE* nRed,
			BYTE* nGreen,
			BYTE* nBlue);"

	<stdcall: bool '_FreeImage_LookupSVGColor@16' char* byte* byte* byte*>
	^self invalidCall!

freeImage_LookupX11Color: szColor nRed: nRed nGreen: nGreen nBlue: nBlue 
	"Private - Invoke the FreeImage_LookupX11Color() function of the module wrapped by the receiver.

		long __stdcall FreeImage_LookupX11Color(
			CHAR* szColor,
			BYTE* nRed,
			BYTE* nGreen,
			BYTE* nBlue);"

	<stdcall: bool '_FreeImage_LookupX11Color@16' char* byte* byte* byte*>
	^self invalidCall!

freeImage_MakeThumbnail: dib max_pixel_size: max_pixel_size convert: convert 
	"Private - Invoke the FreeImage_MakeThumbnail() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_MakeThumbnail(
			FIBITMAP* dib,
			int max_pixel_size,
			long convert);"

	<stdcall: FIBITMAP* '_FreeImage_MakeThumbnail@12' FIBITMAP* sdword bool>
	^self invalidCall!

freeImage_MovePage: bitmap target: target source: source
	"Private - Invoke the FreeImage_MovePage() function of the module wrapped by the receiver.

		long __stdcall FreeImage_MovePage(
			FIMULTIBITMAP* bitmap,
			int target,
			int source);"

	<stdcall: sdword '_FreeImage_MovePage@12' FIMULTIBITMAP* sdword sdword>
	^self invalidCall!

freeImage_OpenMemory: data size_in_bytes: size_in_bytes
	"Private - Invoke the FreeImage_OpenMemory() function of the module wrapped by the receiver.

		FIMEMORY* __stdcall FreeImage_OpenMemory(
			BYTE* data,
			unsigned long size_in_bytes);"

	<stdcall: FIMEMORY* '_FreeImage_OpenMemory@8' byte* dword>
	^self invalidCall!

freeImage_OpenMultiBitmap: fif filename: filename create_new: create_new read_only: read_only keep_cache_in_memory: keep_cache_in_memory flags: flags
	"Private - Invoke the FreeImage_OpenMultiBitmap() function of the module wrapped by the receiver.

		FIMULTIBITMAP* __stdcall FreeImage_OpenMultiBitmap(
			int fif,
			CHAR* filename,
			long create_new,
			long read_only,
			long keep_cache_in_memory,
			int flags);"

	<stdcall: FIMULTIBITMAP* '_FreeImage_OpenMultiBitmap@24' sdword sbyte* sdword sdword sdword sdword>
	^self invalidCall!

freeImage_Paste: dst src: src left: left top: top alpha: alpha 
	"Private - Invoke the FreeImage_Paste() function of the module wrapped by the receiver.

		long __stdcall FreeImage_Paste(
			FIBITMAP* dst,
			FIBITMAP* src,
			int left,
			int top,
			int alpha);"

	<stdcall: bool '_FreeImage_Paste@20' FIBITMAP* FIBITMAP* sdword sdword sdword>
	^self invalidCall!

freeImage_ReadMemory: buffer size: size count: count stream: stream
	"Private - Invoke the FreeImage_ReadMemory() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_ReadMemory(
			void* buffer,
			unsigned int size,
			unsigned int count,
			FIMEMORY* stream);"

	<stdcall: dword '_FreeImage_ReadMemory@16' void* dword dword FIMEMORY*>
	^self invalidCall!

freeImage_RegisterExternalPlugin: path format: format description: description extension: extension regexpr: regexpr
	"Private - Invoke the FreeImage_RegisterExternalPlugin() function of the module wrapped by the receiver.

		int __stdcall FreeImage_RegisterExternalPlugin(
			CHAR* path,
			CHAR* format,
			CHAR* description,
			CHAR* extension,
			CHAR* regexpr);"

	<stdcall: sdword '_FreeImage_RegisterExternalPlugin@20' sbyte* sbyte* sbyte* sbyte* sbyte*>
	^self invalidCall!

freeImage_RegisterLocalPlugin: proc_address format: format description: description extension: extension regexpr: regexpr
	"Private - Invoke the FreeImage_RegisterLocalPlugin() function of the module wrapped by the receiver.

		int __stdcall FreeImage_RegisterLocalPlugin(
			void* proc_address,
			CHAR* format,
			CHAR* description,
			CHAR* extension,
			CHAR* regexpr);"

	<stdcall: sdword '_FreeImage_RegisterLocalPlugin@20' void* sbyte* sbyte* sbyte* sbyte*>
	^self invalidCall!

freeImage_Rescale: dib dst_width: dst_width dst_height: dst_height filter: filter
	"Private - Invoke the FreeImage_Rescale() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_Rescale(
			FIBITMAP* dib,
			int dst_width,
			int dst_height,
			int filter);"

	<stdcall: FIBITMAP* '_FreeImage_Rescale@16' FIBITMAP* sdword sdword sdword>
	^self invalidCall!

freeImage_RotateClassic: dib angle: angle
	"Private - Invoke the FreeImage_RotateClassic() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_RotateClassic(
			FIBITMAP* dib,
			double angle);"

	<stdcall: FIBITMAP* '_FreeImage_RotateClassic@12' FIBITMAP* double>
	^self invalidCall!

freeImage_RotateEx: dib angle: angle x_shift: x_shift y_shift: y_shift x_origin: x_origin y_origin: y_origin use_mask: use_mask 
	"Private - Invoke the FreeImage_RotateEx() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_RotateEx(
			FIBITMAP* dib,
			double angle,
			double x_shift,
			double y_shift,
			double x_origin,
			double y_origin,
			long use_mask);"

	<stdcall: FIBITMAP* '_FreeImage_RotateEx@48' FIBITMAP* double double double double double bool>
	^self invalidCall!

freeImage_Save: fif dib: dib filename: filename flags: flags
	"Private - Invoke the FreeImage_Save() function of the module wrapped by the receiver.

		long __stdcall FreeImage_Save(
			int fif,
			FIBITMAP* dib,
			CHAR* filename,
			int flags);"

	<stdcall: sdword '_FreeImage_Save@16' sdword FIBITMAP* sbyte* sdword>
	^self invalidCall!

freeImage_SaveToHandle: fif dib: dib io: io handle: handle1 flags: flags
	"Private - Invoke the FreeImage_SaveToHandle() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SaveToHandle(
			int fif,
			FIBITMAP* dib,
			FreeImageIO* io,
			void* handle,
			int flags);"

	<stdcall: sdword '_FreeImage_SaveToHandle@20' sdword FIBITMAP* FreeImageIO* void* sdword>
	^self invalidCall!

freeImage_SaveToMemory: fif dib: dib stream: stream flags: flags
	"Private - Invoke the FreeImage_SaveToMemory() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SaveToMemory(
			int fif,
			FIBITMAP* dib,
			FIMEMORY* stream,
			int flags);"

	<stdcall: sdword '_FreeImage_SaveToMemory@16' sdword FIBITMAP* FIMEMORY* sdword>
	^self invalidCall!

freeImage_SaveU: fif dib: dib filename: filename flags: flags
	"Private - Invoke the FreeImage_SaveU() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SaveU(
			int fif,
			FIBITMAP* dib,
			unsigned short* filename,
			int flags);"

	<stdcall: sdword '_FreeImage_SaveU@16' sdword FIBITMAP* word* sdword>
	^self invalidCall!

freeImage_SeekMemory: stream offset: offset origin: origin
	"Private - Invoke the FreeImage_SeekMemory() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SeekMemory(
			FIMEMORY* stream,
			long offset,
			int origin);"

	<stdcall: sdword '_FreeImage_SeekMemory@12' FIMEMORY* sdword sdword>
	^self invalidCall!

freeImage_SetBackgroundColor: dib bkcolor: bkcolor 
	"Private - Invoke the FreeImage_SetBackgroundColor() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetBackgroundColor(
			FIBITMAP* dib,
			RGBQUAD* bkcolor);"

	<stdcall: sdword '_FreeImage_SetBackgroundColor@8' FIBITMAP* RGBQUAD*>
	^self invalidCall!

freeImage_SetChannel: dib dib8: dib8 channel: channel 
	"Private - Invoke the FreeImage_SetChannel() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetChannel(
			FIBITMAP* dib,
			FIBITMAP* dib8,
			int channel);"

	<stdcall: sdword '_FreeImage_SetChannel@12' FIBITMAP* FIBITMAP* sdword>
	^self invalidCall!

freeImage_SetComplexChannel: dst src: src channel: channel 
	"Private - Invoke the FreeImage_SetComplexChannel() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetComplexChannel(
			FIBITMAP* dst,
			FIBITMAP* src,
			int channel);"

	<stdcall: bool '_FreeImage_SetComplexChannel@12' FIBITMAP* FIBITMAP* sdword>
	^self invalidCall!

freeImage_SetDotsPerMeterX: dib res: res
	"Private - Invoke the FreeImage_SetDotsPerMeterX() function of the module wrapped by the receiver.

		void __stdcall FreeImage_SetDotsPerMeterX(
			FIBITMAP* dib,
			unsigned int res);"

	<stdcall: void '_FreeImage_SetDotsPerMeterX@8' FIBITMAP* dword>
	^self invalidCall!

freeImage_SetDotsPerMeterY: dib res: res
	"Private - Invoke the FreeImage_SetDotsPerMeterY() function of the module wrapped by the receiver.

		void __stdcall FreeImage_SetDotsPerMeterY(
			FIBITMAP* dib,
			unsigned int res);"

	<stdcall: void '_FreeImage_SetDotsPerMeterY@8' FIBITMAP* dword>
	^self invalidCall!

freeImage_SetMetadata: model dib: dib key: key tag: tag 
	"Private - Invoke the FreeImage_SetMetadata() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetMetadata(
			int model,
			FIBITMAP* dib,
			CHAR* key,
			FITAG* tag);"

	<stdcall: bool '_FreeImage_SetMetadata@16' sdword FIBITMAP* char* FITAG*>
	^self invalidCall!

freeImage_SetOutputMessage: omf
	"Private - Invoke the FreeImage_SetOutputMessage() function of the module wrapped by the receiver.

		void __stdcall FreeImage_SetOutputMessage(
			void* omf);"

	<stdcall: void '_FreeImage_SetOutputMessage@4' void*>
	^self invalidCall!

freeImage_SetPixelColor: dib x: x y: y value: value 
	"Private - Invoke the FreeImage_SetPixelColor() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetPixelColor(
			FIBITMAP* dib,
			unsigned int x,
			unsigned int y,
			RGBQUAD* value);"

	<stdcall: bool '_FreeImage_SetPixelColor@16' FIBITMAP* dword dword RGBQUAD*>
	^self invalidCall!

freeImage_SetPixelIndex: dib x: x y: y value: value 
	"Private - Invoke the FreeImage_SetPixelIndex() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetPixelIndex(
			FIBITMAP* dib,
			unsigned int x,
			unsigned int y,
			BYTE* value);"

	<stdcall: bool '_FreeImage_SetPixelIndex@16' FIBITMAP* dword dword byte*>
	^self invalidCall!

freeImage_SetPluginEnabled: fif enable: enable
	"Private - Invoke the FreeImage_SetPluginEnabled() function of the module wrapped by the receiver.

		int __stdcall FreeImage_SetPluginEnabled(
			int fif,
			long enable);"

	<stdcall: sdword '_FreeImage_SetPluginEnabled@8' sdword sdword>
	^self invalidCall!

freeImage_SetTagCount: tag count: count
	"Private - Invoke the FreeImage_SetTagCount() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetTagCount(
			FITAG* tag,
			unsigned long count);"

	<stdcall: sdword '_FreeImage_SetTagCount@8' FITAG* dword>
	^self invalidCall!

freeImage_SetTagDescription: tag description: description 
	"Private - Invoke the FreeImage_SetTagDescription() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetTagDescription(
			FITAG* tag,
			CHAR* description);"

	<stdcall: sdword '_FreeImage_SetTagDescription@8' FITAG* char*>
	^self invalidCall!

freeImage_SetTagID: tag id: id
	"Private - Invoke the FreeImage_SetTagID() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetTagID(
			FITAG* tag,
			unsigned short id);"

	<stdcall: sdword '_FreeImage_SetTagID@8' FITAG* word>
	^self invalidCall!

freeImage_SetTagKey: tag key: key 
	"Private - Invoke the FreeImage_SetTagKey() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetTagKey(
			FITAG* tag,
			CHAR* key);"

	<stdcall: sdword '_FreeImage_SetTagKey@8' FITAG* char*>
	^self invalidCall!

freeImage_SetTagLength: tag length: length
	"Private - Invoke the FreeImage_SetTagLength() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetTagLength(
			FITAG* tag,
			unsigned long length);"

	<stdcall: sdword '_FreeImage_SetTagLength@8' FITAG* dword>
	^self invalidCall!

freeImage_SetTagType: tag type: type
	"Private - Invoke the FreeImage_SetTagType() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetTagType(
			FITAG* tag,
			int type);"

	<stdcall: sdword '_FreeImage_SetTagType@8' FITAG* sdword>
	^self invalidCall!

freeImage_SetTagValue: tag value: value
	"Private - Invoke the FreeImage_SetTagValue() function of the module wrapped by the receiver.

		long __stdcall FreeImage_SetTagValue(
			FITAG* tag,
			void* value);"

	<stdcall: sdword '_FreeImage_SetTagValue@8' FITAG* void*>
	^self invalidCall!

freeImage_SetTransparencyTable: dib table: table count: count
	"Private - Invoke the FreeImage_SetTransparencyTable() function of the module wrapped by the receiver.

		void __stdcall FreeImage_SetTransparencyTable(
			FIBITMAP* dib,
			BYTE* table,
			int count);"

	<stdcall: void '_FreeImage_SetTransparencyTable@12' FIBITMAP* byte* sdword>
	^self invalidCall!

freeImage_SetTransparent: dib enabled: enabled 
	"Private - Invoke the FreeImage_SetTransparent() function of the module wrapped by the receiver.

		void __stdcall FreeImage_SetTransparent(
			FIBITMAP* dib,
			long enabled);"

	<stdcall: void '_FreeImage_SetTransparent@8' FIBITMAP* sdword>
	^self invalidCall!

freeImage_TagToString: model tag: tag make: make 
	"Private - Invoke the FreeImage_TagToString() function of the module wrapped by the receiver.

		CHAR* __stdcall FreeImage_TagToString(
			int model,
			FITAG* tag,
			CHAR* Make);"

	<stdcall: char* '_FreeImage_TagToString@12' sdword FITAG* char*>
	^self invalidCall!

freeImage_TellMemory: stream
	"Private - Invoke the FreeImage_TellMemory() function of the module wrapped by the receiver.

		long __stdcall FreeImage_TellMemory(
			FIMEMORY* stream);"

	<stdcall: sdword '_FreeImage_TellMemory@4' FIMEMORY*>
	^self invalidCall!

freeImage_Threshold: dib t: t
	"Private - Invoke the FreeImage_Threshold() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_Threshold(
			FIBITMAP* dib,
			BYTE T);"

	<stdcall: FIBITMAP* '_FreeImage_Threshold@8' FIBITMAP* byte>
	^self invalidCall!

freeImage_TmoDrago03: src gamma: gamma exposure: exposure
	"Private - Invoke the FreeImage_TmoDrago03() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_TmoDrago03(
			FIBITMAP* src,
			double gamma,
			double exposure);"

	<stdcall: FIBITMAP* '_FreeImage_TmoDrago03@20' FIBITMAP* double double>
	^self invalidCall!

freeImage_TmoReinhard05: src intensity: intensity contrast: contrast
	"Private - Invoke the FreeImage_TmoReinhard05() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_TmoReinhard05(
			FIBITMAP* src,
			double intensity,
			double contrast);"

	<stdcall: FIBITMAP* '_FreeImage_TmoReinhard05@20' FIBITMAP* double double>
	^self invalidCall!

freeImage_ToneMapping: dib tmo: tmo first_param: first_param second_param: second_param
	"Private - Invoke the FreeImage_ToneMapping() function of the module wrapped by the receiver.

		FIBITMAP* __stdcall FreeImage_ToneMapping(
			FIBITMAP* dib,
			int tmo,
			double first_param,
			double second_param);"

	<stdcall: FIBITMAP* '_FreeImage_ToneMapping@24' FIBITMAP* sdword double double>
	^self invalidCall!

freeImage_Unload: dib
	"Private - Invoke the FreeImage_Unload() function of the module wrapped by the receiver.

		void __stdcall FreeImage_Unload(
			FIBITMAP* dib);"

	<stdcall: void '_FreeImage_Unload@4' FIBITMAP*>
	^self invalidCall!

freeImage_UnlockPage: bitmap data: data changed: changed
	"Private - Invoke the FreeImage_UnlockPage() function of the module wrapped by the receiver.

		void __stdcall FreeImage_UnlockPage(
			FIMULTIBITMAP* bitmap,
			FIBITMAP* data,
			long changed);"

	<stdcall: void '_FreeImage_UnlockPage@12' FIMULTIBITMAP* FIBITMAP* sdword>
	^self invalidCall!

freeImage_WriteMemory: buffer size: size count: count stream: stream
	"Private - Invoke the FreeImage_WriteMemory() function of the module wrapped by the receiver.

		unsigned int __stdcall FreeImage_WriteMemory(
			void* buffer,
			unsigned int size,
			unsigned int count,
			FIMEMORY* stream);"

	<stdcall: dword '_FreeImage_WriteMemory@16' void* dword dword FIMEMORY*>
	^self invalidCall!

freeImage_ZLibCompress: target target_size: target_size source: source source_size: source_size
	"Private - Invoke the FreeImage_ZLibCompress() function of the module wrapped by the receiver.

		unsigned long __stdcall FreeImage_ZLibCompress(
			BYTE* target,
			unsigned long target_size,
			BYTE* source,
			unsigned long source_size);"

	<stdcall: dword '_FreeImage_ZLibCompress@16' byte* dword byte* dword>
	^self invalidCall!

freeImage_ZLibCRC32: crc source: source source_size: source_size
	"Private - Invoke the FreeImage_ZLibCRC32() function of the module wrapped by the receiver.

		unsigned long __stdcall FreeImage_ZLibCRC32(
			unsigned long crc,
			BYTE* source,
			unsigned long source_size);"

	<stdcall: dword '_FreeImage_ZLibCRC32@12' dword byte* dword>
	^self invalidCall!

freeImage_ZLibGUnzip: target target_size: target_size source: source source_size: source_size
	"Private - Invoke the FreeImage_ZLibGUnzip() function of the module wrapped by the receiver.

		unsigned long __stdcall FreeImage_ZLibGUnzip(
			BYTE* target,
			unsigned long target_size,
			BYTE* source,
			unsigned long source_size);"

	<stdcall: dword '_FreeImage_ZLibGUnzip@16' byte* dword byte* dword>
	^self invalidCall!

freeImage_ZLibGZip: target target_size: target_size source: source source_size: source_size
	"Private - Invoke the FreeImage_ZLibGZip() function of the module wrapped by the receiver.

		unsigned long __stdcall FreeImage_ZLibGZip(
			BYTE* target,
			unsigned long target_size,
			BYTE* source,
			unsigned long source_size);"

	<stdcall: dword '_FreeImage_ZLibGZip@16' byte* dword byte* dword>
	^self invalidCall!

freeImage_ZLibUncompress: target target_size: target_size source: source source_size: source_size
	"Private - Invoke the FreeImage_ZLibUncompress() function of the module wrapped by the receiver.

		unsigned long __stdcall FreeImage_ZLibUncompress(
			BYTE* target,
			unsigned long target_size,
			BYTE* source,
			unsigned long source_size);"

	<stdcall: dword '_FreeImage_ZLibUncompress@16' byte* dword byte* dword>
	^self invalidCall! !
!FreeImageLibrary categoriesFor: #freeImage_AcquireMemory:data:size_in_bytes:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_AdjustBrightness:percentage:!**auto generated**!color manipulation!private! !
!FreeImageLibrary categoriesFor: #freeImage_AdjustContrast:percentage:!**auto generated**!color manipulation!private! !
!FreeImageLibrary categoriesFor: #freeImage_AdjustCurve:lut:channel:!**manually changed**!color manipulation!private! !
!FreeImageLibrary categoriesFor: #freeImage_AdjustGamma:gamma:!**manually changed**!color manipulation!private! !
!FreeImageLibrary categoriesFor: #freeImage_Allocate:height:bpp:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_Allocate:height:bpp:red_mask:green_mask:blue_mask:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_AllocateT:width:height:bpp:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_AllocateT:width:height:bpp:red_mask:green_mask:blue_mask:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_AppendPage:data:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_Clone:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_CloneTag:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_CloseMemory:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_CloseMultiBitmap:flags:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ColorQuantize:quantize:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ColorQuantizeEx:quantize:paletteSize:reserveSize:reservePalette:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_Composite:useFileBkg:appBkColor:bg:!**manually changed**!composing functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertFromRawBits:width:height:pitch:bpp:red_mask:green_mask:blue_mask:topdown:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16_555_To16_565:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16_565_To16_555:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16To24_555:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16To24_565:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16To32_555:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16To32_565:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16To4_555:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16To4_565:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16To8_555:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine16To8_565:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine1To16_555:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine1To16_565:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine1To24:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine1To32:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine1To4:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine1To8:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine24To16_555:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine24To16_565:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine24To32:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine24To4:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine24To8:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine32To16_555:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine32To16_565:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine32To24:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine32To4:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine32To8:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine4To16_555:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine4To16_565:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine4To24:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine4To32:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine4To8:source:width_in_pixels:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine8To16_555:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine8To16_565:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine8To24:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine8To32:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertLine8To4:source:width_in_pixels:palette:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertTo16Bits555:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertTo16Bits565:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertTo24Bits:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertTo32Bits:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertTo4Bits:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertTo8Bits:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertToGreyscale:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertToRawBits:dib:pitch:bpp:red_mask:green_mask:blue_mask:topdown:!**manually changed**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertToRGBF:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertToStandardType:scale_linear:!**manually changed**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ConvertToType:dst_type:scale_linear:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_Copy:left:top:right:bottom:!**auto generated**!composing functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_CreateICCProfile:data:size:!**auto generated**!ICC Profile functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_CreateTag!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_DeInitialise!**auto generated**!general functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_DeletePage:page:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_DeleteTag:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_DestroyICCProfile:!**auto generated**!ICC Profile functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_Dither:algorithm:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_FIFSupportsExportBPP:bpp:!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_FIFSupportsExportType:type:!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_FIFSupportsICCProfiles:!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_FIFSupportsReading:!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_FIFSupportsWriting:!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_FindCloseMetadata:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_FindFirstMetadata:dib:tag:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_FindNextMetadata:tag:!**manually changed**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_FlipHorizontal:!**auto generated**!private!toolkit functions! !
!FreeImageLibrary categoriesFor: #freeImage_FlipVertical:!**auto generated**!private!toolkit functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetBackgroundColor:bkcolor:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetBits:!**auto generated**!pixel access functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetBlueMask:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetBPP:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetChannel:channel:!**auto generated**!channel processing!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetColorsUsed:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetColorType:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetComplexChannel:channel:!**auto generated**!channel processing!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetCopyrightMessage!**manually changed**!general functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetDIBSize:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetDotsPerMeterX:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetDotsPerMeterY:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFCount!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFDescription:!**manually changed**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFExtensionList:!**manually changed**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFFromFilename:!**manually changed**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFFromFilenameU:!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFFromFormat:!**manually changed**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFFromMime:!**manually changed**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFMimeType:!**manually changed**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFIFRegExpr:!**manually changed**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFileType:size:!**manually changed**!filetype functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFileTypeFromHandle:handle:size:!**auto generated**!filetype functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFileTypeFromMemory:size:!**auto generated**!filetype functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFileTypeU:size:!**auto generated**!filetype functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetFormatFromFIF:!**manually changed**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetGreenMask:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetHeight:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetHistogram:histo:channel:!**manually changed**!color manipulation!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetICCProfile:!**auto generated**!ICC Profile functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetImageType:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetInfo:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetInfoHeader:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetLine:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetLockedPageNumbers:pages:count:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetMetadata:dib:key:tag:!**manually changed**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetMetadataCount:dib:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetPageCount:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetPalette:!**manually changed**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetPitch:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetPixelColor:x:y:value:!**manually changed**!pixel access functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetPixelIndex:x:y:value:!**manually changed**!pixel access functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetRedMask:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetScanLine:scanline:!**auto generated**!pixel access functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetTagCount:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetTagDescription:!**manually changed**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetTagID:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetTagKey:!**manually changed**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetTagLength:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetTagType:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetTagValue:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_GetTransparencyCount:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetTransparencyTable:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetVersion!**manually changed**!general functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_GetWidth:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_HasBackgroundColor:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_Initialise:!**auto generated**!general functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_InsertPage:page:data:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_Invert:!**auto generated**!color manipulation!private! !
!FreeImageLibrary categoriesFor: #freeImage_IsLittleEndian!**auto generated**!helper functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_IsPluginEnabled:!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_IsTransparent:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_JPEGCrop:dst_file:left:top:right:bottom:!**manually changed**!composing functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_JPEGTransform:dst_file:operation:perfect:!**manually changed**!private!toolkit functions! !
!FreeImageLibrary categoriesFor: #freeImage_Load:filename:flags:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_LoadFromHandle:io:handle:flags:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_LoadFromMemory:stream:flags:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_LoadMultiBitmapFromMemory:stream:flags:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_LoadU:filename:flags:!**auto generated**!bitmap management!private!unicode functions! !
!FreeImageLibrary categoriesFor: #freeImage_LockPage:page:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_LookupSVGColor:nRed:nGreen:nBlue:!**manually changed**!helper functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_LookupX11Color:nRed:nGreen:nBlue:!**manually changed**!helper functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_MakeThumbnail:max_pixel_size:convert:!**manually changed**!private!toolkit functions! !
!FreeImageLibrary categoriesFor: #freeImage_MovePage:target:source:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_OpenMemory:size_in_bytes:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_OpenMultiBitmap:filename:create_new:read_only:keep_cache_in_memory:flags:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_Paste:src:left:top:alpha:!**manually changed**!composing functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_ReadMemory:size:count:stream:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_RegisterExternalPlugin:format:description:extension:regexpr:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_RegisterLocalPlugin:format:description:extension:regexpr:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_Rescale:dst_width:dst_height:filter:!**auto generated**!private!toolkit functions! !
!FreeImageLibrary categoriesFor: #freeImage_RotateClassic:angle:!**auto generated**!private!toolkit functions! !
!FreeImageLibrary categoriesFor: #freeImage_RotateEx:angle:x_shift:y_shift:x_origin:y_origin:use_mask:!**manually changed**!private!toolkit functions! !
!FreeImageLibrary categoriesFor: #freeImage_Save:dib:filename:flags:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_SaveToHandle:dib:io:handle:flags:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_SaveToMemory:dib:stream:flags:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_SaveU:dib:filename:flags:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_SeekMemory:offset:origin:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetBackgroundColor:bkcolor:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetChannel:dib8:channel:!**auto generated**!channel processing!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetComplexChannel:src:channel:!**manually changed**!channel processing!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetDotsPerMeterX:res:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetDotsPerMeterY:res:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetMetadata:dib:key:tag:!**manually changed**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_SetOutputMessage:!**auto generated**!general functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetPixelColor:x:y:value:!**manually changed**!pixel access functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetPixelIndex:x:y:value:!**manually changed**!pixel access functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetPluginEnabled:enable:!**auto generated**!plugin functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetTagCount:count:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_SetTagDescription:description:!**manually changed**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_SetTagID:id:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_SetTagKey:key:!**manually changed**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_SetTagLength:length:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_SetTagType:type:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_SetTagValue:value:!**auto generated**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_SetTransparencyTable:table:count:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_SetTransparent:enabled:!**auto generated**!bitmap information functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_TagToString:tag:make:!**manually changed**!private!tag functions! !
!FreeImageLibrary categoriesFor: #freeImage_TellMemory:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_Threshold:t:!**auto generated**!conversion functions!private! !
!FreeImageLibrary categoriesFor: #freeImage_TmoDrago03:gamma:exposure:!**auto generated**!private!tone mapping operators! !
!FreeImageLibrary categoriesFor: #freeImage_TmoReinhard05:intensity:contrast:!**auto generated**!private!tone mapping operators! !
!FreeImageLibrary categoriesFor: #freeImage_ToneMapping:tmo:first_param:second_param:!**auto generated**!private!tone mapping operators! !
!FreeImageLibrary categoriesFor: #freeImage_Unload:!**auto generated**!bitmap management!private! !
!FreeImageLibrary categoriesFor: #freeImage_UnlockPage:data:changed:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_WriteMemory:size:count:stream:!**auto generated**!memory I/O streams!private! !
!FreeImageLibrary categoriesFor: #freeImage_ZLibCompress:target_size:source:source_size:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ZLibCRC32:source:source_size:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ZLibGUnzip:target_size:source:source_size:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ZLibGZip:target_size:source:source_size:!**auto generated**!private! !
!FreeImageLibrary categoriesFor: #freeImage_ZLibUncompress:target_size:source:source_size:!**auto generated**!private! !

!FreeImageLibrary class methodsFor!

callback
^Callback!

colorTypeConstants
	^##(| table |
	table := LookupTable new.
	table
		at: 'Monochrome bitmap (1-bit) : first palette entry is black. Palletised bitmap (4 or 8-bit) : the bitmap has a greyscale palette' put: FIC_MINISBLACK;
at: 'Monochrome bitmap (1-bit) : first palette entry is white. Palletised bitmap (4 or 8-bit) : the bitmap has an inverted greyscale palette' put: FIC_MINISWHITE;
at: 'Palettized bitmap (1, 4 or 8 bit)' put: FIC_PALETTE;
at: 'High-color bitmap (16, 24 or 32 bit)' put: FIC_RGB;
at: 'High-color bitmap with an alpha channel (32 bit only)' put: FIC_RGBALPHA;
at: 'CMYK bitmap (32 bit only)' put: FIC_CMYK.
	table)!

copyright
	^self default freeImage_GetCopyrightMessage!

decoderLoadConstants
	^##(| table |
	table := LookupTable new.
	table
		at: GIF_DEFAULT put: 'GIF default settings';
		at: GIF_LOAD256
			put: 'Load the image as a 256 color image with unused palette entries, if it''s 16 or 2 color';
		at: GIF_PLAYBACK
			put: '''Play'' the GIF to generate each frame (as 32bpp) instead of returning raw frame data when loading';
		at: ICO_MAKEALPHA
			put: 'Convert to 32-bit and create an alpha channel from the AND-mask when loading';
		at: JPEG_DEFAULT put: 'Loads the file as fast as possible, sacrificing some quality';
		at: JPEG_FAST put: 'Loads the file as fast as possible, sacrificing some quality';
		at: JPEG_ACCURATE put: 'Loads the file with the best quality, sacrificing some speed';
		at: JPEG_CMYK put: 'This flag will load CMYK bitmaps as 32-bit separated CMYK';
		at: PCD_DEFAULT
			put: 'A PhotoCD picture comes in many sizes. This flag will load the one sized 768 x 512';
		at: PCD_BASE put: 'This flag will load the one sized 768 x 512';
		at: PCD_BASEDIV4 put: 'This flag will load the bitmap sized 384 x 256';
		at: PCD_BASEDIV16 put: 'This flag will load the bitmap sized 192 x 128';
		at: PNG_IGNOREGAMMA put: 'Avoid gamma correction';
		at: TARGA_LOAD_RGB888 put: 'If set the loader converts RGB555 and ARGB8888 -> RGB888';
		at: TIFF_CMYK put: 'This flag will load CMYK bitmaps as 32-bit separated CMYK'.
	table)!

decoderSaveConstants
	^##(| table |
	table := LookupTable new.
	table
		at: BMP_DEFAULT put: 'Save without any compression';
		at: BMP_SAVE_RLE put: 'Compress the bitmap using RLE when saving';
		at: JPEG_DEFAULT put: 'Saves with good quality (75:1)';
		at: JPEG_QUALITYSUPERB put: 'Saves with superb quality (100:1)';
		at: JPEG_QUALITYGOOD put: 'Saves with good quality (75:1)';
		at: JPEG_QUALITYNORMAL put: 'Saves with normal quality (50:1)';
		at: JPEG_QUALITYAVERAGE put: 'Saves with average quality (25:1)';
		at: JPEG_QUALITYBAD put: 'Saves with bad quality (10:1)';
		at: JPEG_PROGRESSIVE
			put: 'Saves as a progressive JPEG file (use | to combine with JPEG quality flags)';
		at: PNM_DEFAULT put: 'Saves the bitmap as a binary file';
		at: PNM_SAVE_RAW put: 'Saves the bitmap as a binary file';
		at: PNM_SAVE_ASCII put: 'Saves the bitmap as an ASCII file';
		at: TIFF_DEFAULT
			put: 'Save using CCITTFAX4 compression for 1-bit bitmaps and LZW compression for any other bitmaps';
		at: TIFF_CMYK put: 'Stores tags for separated CMYK (use | to combine with TIFF compression flags)';
		at: TIFF_PACKBITS put: 'Save using PACKBITS compression.';
		at: TIFF_DEFLATE put: 'Save using DEFLATE compression (also known as ZLIB compression)';
		at: TIFF_ADOBE_DEFLATE put: 'Save using ADOBE DEFLATE compression';
		at: TIFF_NONE put: 'Save without any compression';
		at: TIFF_CCITTFAX3 put: 'Save using CCITT Group 3 fax encoding';
		at: TIFF_CCITTFAX4 put: 'Save using CCITT Group 4 fax encoding';
		at: TIFF_LZW put: 'Save using LZW compression';
		at: TIFF_JPEG
			put: 'Save using JPEG compression (8-bit greyscale and 24-bit only. Default to LZW for other bitdepths).'.
	table)!

errorCheck: aMonadicValuable 
	| returnValue |
	self resetLastMessage.
	returnValue := aMonadicValuable value: self default.
	returnValue isNull 
		ifTrue: 
			[self lastMessage ifNotNil: [:value | FreeImageError signal: value] ifNil: [FreeImageError signal]].
	^returnValue!

fileName
	"Answer the host system file name for the library."

	^'FreeImage'
!

initialize
	"Private - Register the receiver with the #onStartup event"

	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self.
	self resetLastMessage.
	self initializeCallback!

initializeCallback
	[Callback := ExternalCallback block: 
					[:fif :message | 
					LastErrorMessage := message.
					Transcript
						show: message;
						cr.
					0]
				descriptor: (ExternalDescriptor 
						callingConvention: 'cdecl:'
						returnType: 'void'
						argumentTypes: 'sdword char*').
	self default freeImage_SetOutputMessage: Callback asParameter yourAddress] on: Error do: [:ex | Callback := nil]!

lastMessage
	^LastErrorMessage!

onStartup
	self resetLastMessage.
	self initializeCallback!

resetCallback
	self default freeImage_SetOutputMessage: nil.
	Callback := nil!

resetLastMessage
LastErrorMessage := nil!

typeConstants
	^##(| table |
	table := LookupTable new.
	table
at: 'Unknown format (returned value only, never use it as input value)' put: FIT_UNKNOWN;
at: 'Standard image: 1-, 4-, 8-, 16-, 24-, 32-bit' put: FIT_BITMAP;
at: 'Array of unsigned short: unsigned 16-bit' put: FIT_UINT16;
at: 'Array of short: signed 16-bit' put: FIT_INT16;
at: 'Array of unsigned long: unsigned 32-bit' put: FIT_UINT32;
at: 'Array of long: signed 32-bit' put: FIT_INT32;
at: 'Array of float:: 32-bit IEEE floating point' put: FIT_FLOAT;
at: 'Array of double: 64-bit IEEE floating point' put: FIT_DOUBLE;
at: 'Array of FICOMPLEX: 2 x 64-bit IEEE floating point' put: FIT_COMPLEX;
at: '48-bit RGB image: 3 x 16-bit' put: FIT_RGB16;
at: '64-bit RGBA image: 4 x 16-bit' put: FIT_RGBA16;
at: '96-bit RGB float image: 3 x 32-bit IEEE floating point' put: FIT_RGBF;
at: '128-bit RGBA float image: 4 x 32-bit IEEE floating point' put: FIT_RGBAF.

table)!

version ^self default freeImage_GetVersion! !
!FreeImageLibrary class categoriesFor: #callback!class maintenance!private! !
!FreeImageLibrary class categoriesFor: #colorTypeConstants!development!public! !
!FreeImageLibrary class categoriesFor: #copyright!public! !
!FreeImageLibrary class categoriesFor: #decoderLoadConstants!development!public! !
!FreeImageLibrary class categoriesFor: #decoderSaveConstants!development!public! !
!FreeImageLibrary class categoriesFor: #errorCheck:!helpers!private! !
!FreeImageLibrary class categoriesFor: #fileName!**auto generated**!constants!public! !
!FreeImageLibrary class categoriesFor: #initialize!initializing!private! !
!FreeImageLibrary class categoriesFor: #initializeCallback!initializing!private! !
!FreeImageLibrary class categoriesFor: #lastMessage!accessing!public! !
!FreeImageLibrary class categoriesFor: #onStartup!event handling!private! !
!FreeImageLibrary class categoriesFor: #resetCallback!class maintenance!private! !
!FreeImageLibrary class categoriesFor: #resetLastMessage!accessing!public! !
!FreeImageLibrary class categoriesFor: #typeConstants!development!public! !
!FreeImageLibrary class categoriesFor: #version!public! !

FIBITMAP guid: (GUID fromString: '{D094FA37-7EE9-49D2-B4EA-6DD140D9B65A}')!
FIBITMAP comment: '<FIBITMAP> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIBITMAP'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIBITMAP {
	void* data;
} FIBITMAP;
'!
!FIBITMAP categoriesForClass!Unclassified! !
!FIBITMAP methodsFor!

data
	"Answer the receiver's data field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

data: anObject
	"Set the receiver's data field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!FIBITMAP categoriesFor: #data!**compiled accessors**!public! !
!FIBITMAP categoriesFor: #data:!**compiled accessors**!public! !

!FIBITMAP class methodsFor!

defineFields
	"Define the fields of the FIBITMAP structure.

	FIBITMAP  compileDefinition

		typedef 
		struct tagFIBITMAP {
			void* data;
		} FIBITMAP;
"

	self
		defineField: #data type: LPVOIDField new offset: 0.
	self byteSize: 4! !
!FIBITMAP class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FICOMPLEX guid: (GUID fromString: '{10FB34DD-9B53-4693-BB3E-567B6AFFAEC6}')!
FICOMPLEX comment: '<FICOMPLEX> is an <ExternalStructure> class to wrap the struct ''FreeImage.FICOMPLEX'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFICOMPLEX {
	double r;
	double i;
} FICOMPLEX;
'!
!FICOMPLEX categoriesForClass!Unclassified! !
!FICOMPLEX methodsFor!

i
	"Answer the receiver's i field as a Smalltalk object."

	^(bytes doubleAtOffset: 8)!

i: anObject
	"Set the receiver's i field to the value of anObject."

	bytes doubleAtOffset: 8 put: anObject!

r
	"Answer the receiver's r field as a Smalltalk object."

	^(bytes doubleAtOffset: 0)!

r: anObject
	"Set the receiver's r field to the value of anObject."

	bytes doubleAtOffset: 0 put: anObject! !
!FICOMPLEX categoriesFor: #i!**compiled accessors**!public! !
!FICOMPLEX categoriesFor: #i:!**compiled accessors**!public! !
!FICOMPLEX categoriesFor: #r!**compiled accessors**!public! !
!FICOMPLEX categoriesFor: #r:!**compiled accessors**!public! !

!FICOMPLEX class methodsFor!

defineFields
	"Define the fields of the FICOMPLEX structure.

	FICOMPLEX  compileDefinition

		typedef 
		struct tagFICOMPLEX {
			double r;
			double i;
		} FICOMPLEX;
"

	self
		defineField: #r type: DOUBLEField new offset: 0;
		defineField: #i type: DOUBLEField new offset: 8.
	self byteSize: 16! !
!FICOMPLEX class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FIICCPROFILE guid: (GUID fromString: '{0610A6FE-8972-4A2C-ABDD-2BCE3F90D797}')!
FIICCPROFILE comment: '<FIICCPROFILE> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIICCPROFILE'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIICCPROFILE {
	unsigned short flags;
	unsigned long size;
	void* data;
} FIICCPROFILE;
'!
!FIICCPROFILE categoriesForClass!Unclassified! !
!FIICCPROFILE methodsFor!

data
	"Answer the receiver's data field as a Smalltalk object."

	^(bytes dwordAtOffset: 8) asExternalAddress!

data: anObject
	"Set the receiver's data field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

flags
	"Answer the receiver's flags field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

flags: anObject
	"Set the receiver's flags field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject!

size
	"Answer the receiver's size field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

size: anObject
	"Set the receiver's size field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject! !
!FIICCPROFILE categoriesFor: #data!**compiled accessors**!public! !
!FIICCPROFILE categoriesFor: #data:!**compiled accessors**!public! !
!FIICCPROFILE categoriesFor: #flags!**compiled accessors**!public! !
!FIICCPROFILE categoriesFor: #flags:!**compiled accessors**!public! !
!FIICCPROFILE categoriesFor: #size!**compiled accessors**!public! !
!FIICCPROFILE categoriesFor: #size:!**compiled accessors**!public! !

!FIICCPROFILE class methodsFor!

defineFields
	"Define the fields of the FIICCPROFILE structure.

	FIICCPROFILE  compileDefinition

		typedef 
		struct tagFIICCPROFILE {
			unsigned short flags;
			unsigned long size;
			void* data;
		} FIICCPROFILE;
"

	self
		defineField: #flags type: WORDField new offset: 0;
		defineField: #size type: DWORDField new offset: 4;
		defineField: #data type: LPVOIDField new offset: 8.
	self byteSize: 12! !
!FIICCPROFILE class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FIMEMORY guid: (GUID fromString: '{64DFDF53-389F-478F-A10E-C2A383D098CE}')!
FIMEMORY comment: '<FIMEMORY> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIMEMORY'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIMEMORY {
	void* data;
} FIMEMORY;
'!
!FIMEMORY categoriesForClass!Unclassified! !
!FIMEMORY methodsFor!

data
	"Answer the receiver's data field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

data: anObject
	"Set the receiver's data field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!FIMEMORY categoriesFor: #data!**compiled accessors**!public! !
!FIMEMORY categoriesFor: #data:!**compiled accessors**!public! !

!FIMEMORY class methodsFor!

defineFields
	"Define the fields of the FIMEMORY structure.

	FIMEMORY  compileDefinition

		typedef 
		struct tagFIMEMORY {
			void* data;
		} FIMEMORY;
"

	self
		defineField: #data type: LPVOIDField new offset: 0.
	self byteSize: 4! !
!FIMEMORY class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FIMETADATA guid: (GUID fromString: '{BE9ED22A-52A1-4A4F-8388-344F69C185B1}')!
FIMETADATA comment: '<FIMETADATA> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIMETADATA'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIMETADATA {
	void* data;
} FIMETADATA;
'!
!FIMETADATA categoriesForClass!Unclassified! !
!FIMETADATA methodsFor!

data
	"Answer the receiver's data field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

data: anObject
	"Set the receiver's data field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!FIMETADATA categoriesFor: #data!**compiled accessors**!public! !
!FIMETADATA categoriesFor: #data:!**compiled accessors**!public! !

!FIMETADATA class methodsFor!

defineFields
	"Define the fields of the FIMETADATA structure.

	FIMETADATA  compileDefinition

		typedef 
		struct tagFIMETADATA {
			void* data;
		} FIMETADATA;
"

	self
		defineField: #data type: LPVOIDField new offset: 0.
	self byteSize: 4! !
!FIMETADATA class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FIMULTIBITMAP guid: (GUID fromString: '{A2EBA47B-5D47-4498-A46B-796E410F3BDA}')!
FIMULTIBITMAP comment: '<FIMULTIBITMAP> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIMULTIBITMAP'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIMULTIBITMAP {
	void* data;
} FIMULTIBITMAP;
'!
!FIMULTIBITMAP categoriesForClass!Unclassified! !
!FIMULTIBITMAP methodsFor!

data
	"Answer the receiver's data field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

data: anObject
	"Set the receiver's data field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!FIMULTIBITMAP categoriesFor: #data!**compiled accessors**!public! !
!FIMULTIBITMAP categoriesFor: #data:!**compiled accessors**!public! !

!FIMULTIBITMAP class methodsFor!

defineFields
	"Define the fields of the FIMULTIBITMAP structure.

	FIMULTIBITMAP  compileDefinition

		typedef 
		struct tagFIMULTIBITMAP {
			void* data;
		} FIMULTIBITMAP;
"

	self
		defineField: #data type: LPVOIDField new offset: 0.
	self byteSize: 4! !
!FIMULTIBITMAP class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FIRGB16 guid: (GUID fromString: '{6843E939-2D6E-4CF8-9498-196DFC1A46EF}')!
FIRGB16 comment: '<FIRGB16> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIRGB16'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIRGB16 {
	unsigned short red;
	unsigned short green;
	unsigned short blue;
} FIRGB16;
'!
!FIRGB16 categoriesForClass!Unclassified! !
!FIRGB16 methodsFor!

blue
	"Answer the receiver's blue field as a Smalltalk object."

	^(bytes wordAtOffset: 4)!

blue: anObject
	"Set the receiver's blue field to the value of anObject."

	bytes wordAtOffset: 4 put: anObject!

green
	"Answer the receiver's green field as a Smalltalk object."

	^(bytes wordAtOffset: 2)!

green: anObject
	"Set the receiver's green field to the value of anObject."

	bytes wordAtOffset: 2 put: anObject!

red
	"Answer the receiver's red field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

red: anObject
	"Set the receiver's red field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject! !
!FIRGB16 categoriesFor: #blue!**compiled accessors**!public! !
!FIRGB16 categoriesFor: #blue:!**compiled accessors**!public! !
!FIRGB16 categoriesFor: #green!**compiled accessors**!public! !
!FIRGB16 categoriesFor: #green:!**compiled accessors**!public! !
!FIRGB16 categoriesFor: #red!**compiled accessors**!public! !
!FIRGB16 categoriesFor: #red:!**compiled accessors**!public! !

!FIRGB16 class methodsFor!

defineFields
	"Define the fields of the FIRGB16 structure.

	FIRGB16  compileDefinition

		typedef 
		struct tagFIRGB16 {
			unsigned short red;
			unsigned short green;
			unsigned short blue;
		} FIRGB16;
"

	self
		defineField: #red type: WORDField new offset: 0;
		defineField: #green type: WORDField new offset: 2;
		defineField: #blue type: WORDField new offset: 4.
	self byteSize: 6! !
!FIRGB16 class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FIRGBA16 guid: (GUID fromString: '{308BD5BD-33E0-4DCC-908D-34057F474F44}')!
FIRGBA16 comment: '<FIRGBA16> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIRGBA16'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIRGBA16 {
	unsigned short red;
	unsigned short green;
	unsigned short blue;
	unsigned short alpha;
} FIRGBA16;
'!
!FIRGBA16 categoriesForClass!Unclassified! !
!FIRGBA16 methodsFor!

alpha
	"Answer the receiver's alpha field as a Smalltalk object."

	^(bytes wordAtOffset: 6)!

alpha: anObject
	"Set the receiver's alpha field to the value of anObject."

	bytes wordAtOffset: 6 put: anObject!

blue
	"Answer the receiver's blue field as a Smalltalk object."

	^(bytes wordAtOffset: 4)!

blue: anObject
	"Set the receiver's blue field to the value of anObject."

	bytes wordAtOffset: 4 put: anObject!

green
	"Answer the receiver's green field as a Smalltalk object."

	^(bytes wordAtOffset: 2)!

green: anObject
	"Set the receiver's green field to the value of anObject."

	bytes wordAtOffset: 2 put: anObject!

red
	"Answer the receiver's red field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

red: anObject
	"Set the receiver's red field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject! !
!FIRGBA16 categoriesFor: #alpha!**compiled accessors**!public! !
!FIRGBA16 categoriesFor: #alpha:!**compiled accessors**!public! !
!FIRGBA16 categoriesFor: #blue!**compiled accessors**!public! !
!FIRGBA16 categoriesFor: #blue:!**compiled accessors**!public! !
!FIRGBA16 categoriesFor: #green!**compiled accessors**!public! !
!FIRGBA16 categoriesFor: #green:!**compiled accessors**!public! !
!FIRGBA16 categoriesFor: #red!**compiled accessors**!public! !
!FIRGBA16 categoriesFor: #red:!**compiled accessors**!public! !

!FIRGBA16 class methodsFor!

defineFields
	"Define the fields of the FIRGBA16 structure.

	FIRGBA16  compileDefinition

		typedef 
		struct tagFIRGBA16 {
			unsigned short red;
			unsigned short green;
			unsigned short blue;
			unsigned short alpha;
		} FIRGBA16;
"

	self
		defineField: #red type: WORDField new offset: 0;
		defineField: #green type: WORDField new offset: 2;
		defineField: #blue type: WORDField new offset: 4;
		defineField: #alpha type: WORDField new offset: 6.
	self byteSize: 8! !
!FIRGBA16 class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FIRGBAF guid: (GUID fromString: '{6C0F4A8F-F4FE-4FD4-AF30-A836317F0512}')!
FIRGBAF comment: '<FIRGBAF> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIRGBAF'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIRGBAF {
	float red;
	float green;
	float blue;
	float alpha;
} FIRGBAF;
'!
!FIRGBAF categoriesForClass!Unclassified! !
!FIRGBAF methodsFor!

alpha
	"Answer the receiver's alpha field as a Smalltalk object."

	^(bytes floatAtOffset: 12)!

alpha: anObject
	"Set the receiver's alpha field to the value of anObject."

	bytes floatAtOffset: 12 put: anObject!

blue
	"Answer the receiver's blue field as a Smalltalk object."

	^(bytes floatAtOffset: 8)!

blue: anObject
	"Set the receiver's blue field to the value of anObject."

	bytes floatAtOffset: 8 put: anObject!

green
	"Answer the receiver's green field as a Smalltalk object."

	^(bytes floatAtOffset: 4)!

green: anObject
	"Set the receiver's green field to the value of anObject."

	bytes floatAtOffset: 4 put: anObject!

red
	"Answer the receiver's red field as a Smalltalk object."

	^(bytes floatAtOffset: 0)!

red: anObject
	"Set the receiver's red field to the value of anObject."

	bytes floatAtOffset: 0 put: anObject! !
!FIRGBAF categoriesFor: #alpha!**compiled accessors**!public! !
!FIRGBAF categoriesFor: #alpha:!**compiled accessors**!public! !
!FIRGBAF categoriesFor: #blue!**compiled accessors**!public! !
!FIRGBAF categoriesFor: #blue:!**compiled accessors**!public! !
!FIRGBAF categoriesFor: #green!**compiled accessors**!public! !
!FIRGBAF categoriesFor: #green:!**compiled accessors**!public! !
!FIRGBAF categoriesFor: #red!**compiled accessors**!public! !
!FIRGBAF categoriesFor: #red:!**compiled accessors**!public! !

!FIRGBAF class methodsFor!

defineFields
	"Define the fields of the FIRGBAF structure.

	FIRGBAF  compileDefinition

		typedef 
		struct tagFIRGBAF {
			float red;
			float green;
			float blue;
			float alpha;
		} FIRGBAF;
"

	self
		defineField: #red type: FLOATField new offset: 0;
		defineField: #green type: FLOATField new offset: 4;
		defineField: #blue type: FLOATField new offset: 8;
		defineField: #alpha type: FLOATField new offset: 12.
	self byteSize: 16! !
!FIRGBAF class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FIRGBF guid: (GUID fromString: '{9B48521A-D23A-4F79-B5ED-CF929084A772}')!
FIRGBF comment: '<FIRGBF> is an <ExternalStructure> class to wrap the struct ''FreeImage.FIRGBF'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFIRGBF {
	float red;
	float green;
	float blue;
} FIRGBF;
'!
!FIRGBF categoriesForClass!Unclassified! !
!FIRGBF methodsFor!

blue
	"Answer the receiver's blue field as a Smalltalk object."

	^(bytes floatAtOffset: 8)!

blue: anObject
	"Set the receiver's blue field to the value of anObject."

	bytes floatAtOffset: 8 put: anObject!

green
	"Answer the receiver's green field as a Smalltalk object."

	^(bytes floatAtOffset: 4)!

green: anObject
	"Set the receiver's green field to the value of anObject."

	bytes floatAtOffset: 4 put: anObject!

red
	"Answer the receiver's red field as a Smalltalk object."

	^(bytes floatAtOffset: 0)!

red: anObject
	"Set the receiver's red field to the value of anObject."

	bytes floatAtOffset: 0 put: anObject! !
!FIRGBF categoriesFor: #blue!**compiled accessors**!public! !
!FIRGBF categoriesFor: #blue:!**compiled accessors**!public! !
!FIRGBF categoriesFor: #green!**compiled accessors**!public! !
!FIRGBF categoriesFor: #green:!**compiled accessors**!public! !
!FIRGBF categoriesFor: #red!**compiled accessors**!public! !
!FIRGBF categoriesFor: #red:!**compiled accessors**!public! !

!FIRGBF class methodsFor!

defineFields
	"Define the fields of the FIRGBF structure.

	FIRGBF  compileDefinition

		typedef 
		struct tagFIRGBF {
			float red;
			float green;
			float blue;
		} FIRGBF;
"

	self
		defineField: #red type: FLOATField new offset: 0;
		defineField: #green type: FLOATField new offset: 4;
		defineField: #blue type: FLOATField new offset: 8.
	self byteSize: 12! !
!FIRGBF class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FITAG guid: (GUID fromString: '{B145AEC8-08A1-4C07-A126-157E955024E4}')!
FITAG comment: '<FITAG> is an <ExternalStructure> class to wrap the struct ''FreeImage.FITAG'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFITAG {
	void* data;
} FITAG;
'!
!FITAG categoriesForClass!Unclassified! !
!FITAG methodsFor!

data
	"Answer the receiver's data field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

data: anObject
	"Set the receiver's data field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!FITAG categoriesFor: #data!**compiled accessors**!public! !
!FITAG categoriesFor: #data:!**compiled accessors**!public! !

!FITAG class methodsFor!

defineFields
	"Define the fields of the FITAG structure.

	FITAG  compileDefinition

		typedef 
		struct tagFITAG {
			void* data;
		} FITAG;
"

	self
		defineField: #data type: LPVOIDField new offset: 0.
	self byteSize: 4! !
!FITAG class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FreeImageIO guid: (GUID fromString: '{E6E84163-7EDA-424B-8DF9-3E7DC6507603}')!
FreeImageIO comment: '<FreeImageIO> is an <ExternalStructure> class to wrap the struct ''FreeImage.FreeImageIO'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagFreeImageIO {
	void* read_proc;
	void* write_proc;
	void* seek_proc;
	void* tell_proc;
} FreeImageIO;
'!
!FreeImageIO categoriesForClass!Unclassified! !
!FreeImageIO methodsFor!

read_proc
	"Answer the receiver's read_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

read_proc: anObject
	"Set the receiver's read_proc field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

seek_proc
	"Answer the receiver's seek_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 8) asExternalAddress!

seek_proc: anObject
	"Set the receiver's seek_proc field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

tell_proc
	"Answer the receiver's tell_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 12) asExternalAddress!

tell_proc: anObject
	"Set the receiver's tell_proc field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

write_proc
	"Answer the receiver's write_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asExternalAddress!

write_proc: anObject
	"Set the receiver's write_proc field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject! !
!FreeImageIO categoriesFor: #read_proc!**compiled accessors**!public! !
!FreeImageIO categoriesFor: #read_proc:!**compiled accessors**!public! !
!FreeImageIO categoriesFor: #seek_proc!**compiled accessors**!public! !
!FreeImageIO categoriesFor: #seek_proc:!**compiled accessors**!public! !
!FreeImageIO categoriesFor: #tell_proc!**compiled accessors**!public! !
!FreeImageIO categoriesFor: #tell_proc:!**compiled accessors**!public! !
!FreeImageIO categoriesFor: #write_proc!**compiled accessors**!public! !
!FreeImageIO categoriesFor: #write_proc:!**compiled accessors**!public! !

!FreeImageIO class methodsFor!

defineFields
	"Define the fields of the FreeImageIO structure.

	FreeImageIO  compileDefinition

		typedef 
		struct tagFreeImageIO {
			void* read_proc;
			void* write_proc;
			void* seek_proc;
			void* tell_proc;
		} FreeImageIO;
"

	self
		defineField: #read_proc type: LPVOIDField new offset: 0;
		defineField: #write_proc type: LPVOIDField new offset: 4;
		defineField: #seek_proc type: LPVOIDField new offset: 8;
		defineField: #tell_proc type: LPVOIDField new offset: 12.
	self byteSize: 16! !
!FreeImageIO class categoriesFor: #defineFields!**auto generated**!initializing!public! !

Plugin guid: (GUID fromString: '{2F516407-4BB5-48CA-B8F0-829A29890AE8}')!
Plugin comment: '<Plugin> is an <ExternalStructure> class to wrap the struct ''FreeImage.Plugin'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagPlugin {
	void* format_proc;
	void* description_proc;
	void* extension_proc;
	void* regexpr_proc;
	void* open_proc;
	void* close_proc;
	void* pagecount_proc;
	void* pagecapability_proc;
	void* load_proc;
	void* save_proc;
	void* validate_proc;
	void* mime_proc;
	void* supports_export_bpp_proc;
	void* supports_export_type_proc;
	void* supports_icc_profiles_proc;
} Plugin;
'!
!Plugin categoriesForClass!Unclassified! !
!Plugin methodsFor!

close_proc
	"Answer the receiver's close_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 20) asExternalAddress!

close_proc: anObject
	"Set the receiver's close_proc field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

description_proc
	"Answer the receiver's description_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asExternalAddress!

description_proc: anObject
	"Set the receiver's description_proc field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

extension_proc
	"Answer the receiver's extension_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 8) asExternalAddress!

extension_proc: anObject
	"Set the receiver's extension_proc field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

format_proc
	"Answer the receiver's format_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

format_proc: anObject
	"Set the receiver's format_proc field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

load_proc
	"Answer the receiver's load_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

load_proc: anObject
	"Set the receiver's load_proc field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

mime_proc
	"Answer the receiver's mime_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

mime_proc: anObject
	"Set the receiver's mime_proc field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

open_proc
	"Answer the receiver's open_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

open_proc: anObject
	"Set the receiver's open_proc field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

pagecapability_proc
	"Answer the receiver's pagecapability_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asExternalAddress!

pagecapability_proc: anObject
	"Set the receiver's pagecapability_proc field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject!

pagecount_proc
	"Answer the receiver's pagecount_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 24) asExternalAddress!

pagecount_proc: anObject
	"Set the receiver's pagecount_proc field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject!

regexpr_proc
	"Answer the receiver's regexpr_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 12) asExternalAddress!

regexpr_proc: anObject
	"Set the receiver's regexpr_proc field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

save_proc
	"Answer the receiver's save_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

save_proc: anObject
	"Set the receiver's save_proc field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

supports_export_bpp_proc
	"Answer the receiver's supports_export_bpp_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 48) asExternalAddress!

supports_export_bpp_proc: anObject
	"Set the receiver's supports_export_bpp_proc field to the value of anObject."

	bytes dwordAtOffset: 48 put: anObject!

supports_export_type_proc
	"Answer the receiver's supports_export_type_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 52) asExternalAddress!

supports_export_type_proc: anObject
	"Set the receiver's supports_export_type_proc field to the value of anObject."

	bytes dwordAtOffset: 52 put: anObject!

supports_icc_profiles_proc
	"Answer the receiver's supports_icc_profiles_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 56) asExternalAddress!

supports_icc_profiles_proc: anObject
	"Set the receiver's supports_icc_profiles_proc field to the value of anObject."

	bytes dwordAtOffset: 56 put: anObject!

validate_proc
	"Answer the receiver's validate_proc field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

validate_proc: anObject
	"Set the receiver's validate_proc field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject! !
!Plugin categoriesFor: #close_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #close_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #description_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #description_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #extension_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #extension_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #format_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #format_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #load_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #load_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #mime_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #mime_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #open_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #open_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #pagecapability_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #pagecapability_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #pagecount_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #pagecount_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #regexpr_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #regexpr_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #save_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #save_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #supports_export_bpp_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #supports_export_bpp_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #supports_export_type_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #supports_export_type_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #supports_icc_profiles_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #supports_icc_profiles_proc:!**compiled accessors**!public! !
!Plugin categoriesFor: #validate_proc!**compiled accessors**!public! !
!Plugin categoriesFor: #validate_proc:!**compiled accessors**!public! !

!Plugin class methodsFor!

defineFields
	"Define the fields of the Plugin structure.

	Plugin  compileDefinition

		typedef 
		struct tagPlugin {
			void* format_proc;
			void* description_proc;
			void* extension_proc;
			void* regexpr_proc;
			void* open_proc;
			void* close_proc;
			void* pagecount_proc;
			void* pagecapability_proc;
			void* load_proc;
			void* save_proc;
			void* validate_proc;
			void* mime_proc;
			void* supports_export_bpp_proc;
			void* supports_export_type_proc;
			void* supports_icc_profiles_proc;
		} Plugin;
"

	self
		defineField: #format_proc type: LPVOIDField new offset: 0;
		defineField: #description_proc type: LPVOIDField new offset: 4;
		defineField: #extension_proc type: LPVOIDField new offset: 8;
		defineField: #regexpr_proc type: LPVOIDField new offset: 12;
		defineField: #open_proc type: LPVOIDField new offset: 16;
		defineField: #close_proc type: LPVOIDField new offset: 20;
		defineField: #pagecount_proc type: LPVOIDField new offset: 24;
		defineField: #pagecapability_proc type: LPVOIDField new offset: 28;
		defineField: #load_proc type: LPVOIDField new offset: 32;
		defineField: #save_proc type: LPVOIDField new offset: 36;
		defineField: #validate_proc type: LPVOIDField new offset: 40;
		defineField: #mime_proc type: LPVOIDField new offset: 44;
		defineField: #supports_export_bpp_proc type: LPVOIDField new offset: 48;
		defineField: #supports_export_type_proc type: LPVOIDField new offset: 52;
		defineField: #supports_icc_profiles_proc type: LPVOIDField new offset: 56.
	self byteSize: 60! !
!Plugin class categoriesFor: #defineFields!**auto generated**!initializing!public! !

RGBTRIPLE guid: (GUID fromString: '{4C936BC4-DCDD-4A7E-8464-711CA30FC95D}')!
RGBTRIPLE comment: '<RGBTRIPLE> is an <ExternalStructure> class to wrap the struct ''FreeImage.RGBTRIPLE'' from type information in the ''FreeImage Type Library'' library.

The type library contains no documentation for this struct

Warning: This comment was automatically generated from the struct''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

typedef 
struct tagRGBTRIPLE {
	BYTE rgbtBlue;
	BYTE rgbtGreen;
	BYTE rgbtRed;
} RGBTRIPLE;
'!
!RGBTRIPLE categoriesForClass!Unclassified! !
!RGBTRIPLE methodsFor!

rgbtBlue
	"Answer the receiver's rgbtBlue field as a Smalltalk object."

	^(bytes byteAtOffset: 0)!

rgbtBlue: anObject
	"Set the receiver's rgbtBlue field to the value of anObject."

	bytes byteAtOffset: 0 put: anObject!

rgbtGreen
	"Answer the receiver's rgbtGreen field as a Smalltalk object."

	^(bytes byteAtOffset: 1)!

rgbtGreen: anObject
	"Set the receiver's rgbtGreen field to the value of anObject."

	bytes byteAtOffset: 1 put: anObject!

rgbtRed
	"Answer the receiver's rgbtRed field as a Smalltalk object."

	^(bytes byteAtOffset: 2)!

rgbtRed: anObject
	"Set the receiver's rgbtRed field to the value of anObject."

	bytes byteAtOffset: 2 put: anObject! !
!RGBTRIPLE categoriesFor: #rgbtBlue!**compiled accessors**!public! !
!RGBTRIPLE categoriesFor: #rgbtBlue:!**compiled accessors**!public! !
!RGBTRIPLE categoriesFor: #rgbtGreen!**compiled accessors**!public! !
!RGBTRIPLE categoriesFor: #rgbtGreen:!**compiled accessors**!public! !
!RGBTRIPLE categoriesFor: #rgbtRed!**compiled accessors**!public! !
!RGBTRIPLE categoriesFor: #rgbtRed:!**compiled accessors**!public! !

!RGBTRIPLE class methodsFor!

defineFields
	"Define the fields of the RGBTRIPLE structure.

	RGBTRIPLE  compileDefinition

		typedef 
		struct tagRGBTRIPLE {
			BYTE rgbtBlue;
			BYTE rgbtGreen;
			BYTE rgbtRed;
		} RGBTRIPLE;
"

	self
		defineField: #rgbtBlue type: BYTEField new offset: 0;
		defineField: #rgbtGreen type: BYTEField new offset: 1;
		defineField: #rgbtRed type: BYTEField new offset: 2.
	self byteSize: 3! !
!RGBTRIPLE class categoriesFor: #defineFields!**auto generated**!initializing!public! !

FreeImageDestructiveBitmapOperation guid: (GUID fromString: '{10DA370E-C153-4EA3-AA55-5B5B6BE29D9F}')!
FreeImageDestructiveBitmapOperation comment: ''!
!FreeImageDestructiveBitmapOperation categoriesForClass!Unclassified! !
!FreeImageDestructiveBitmapOperation methodsFor!

processDestructive: aFreeImageBitmap 
	self basicProcess: aFreeImageBitmap .
	^aFreeImageBitmap!

processNonDestructive: aFreeImageBitmap 
	| newBitmap |
	newBitmap := aFreeImageBitmap copy.
	self basicProcess: newBitmap.

	^newBitmap! !
!FreeImageDestructiveBitmapOperation categoriesFor: #processDestructive:!actions!conversion!public! !
!FreeImageDestructiveBitmapOperation categoriesFor: #processNonDestructive:!actions!conversion!public! !

!FreeImageDestructiveBitmapOperation class methodsFor!

icon
	^##(ValueAdaptor icon)! !
!FreeImageDestructiveBitmapOperation class categoriesFor: #icon!development!public! !

FreeImageNonDestructiveOperation guid: (GUID fromString: '{116A0AAC-0D95-43A2-B3E6-54AE3037BBE2}')!
FreeImageNonDestructiveOperation comment: ''!
!FreeImageNonDestructiveOperation categoriesForClass!Unclassified! !
!FreeImageNonDestructiveOperation methodsFor!

processDestructive: aFreeImageBitmap 
	^aFreeImageBitmap
		free;
		ownedHandle: (self basicProcess: aFreeImageBitmap ) asParameter ;
		yourself!

processNonDestructive: aFreeImageBitmap 
	^FreeImageBitmap fromOwnedHandle: (self basicProcess: aFreeImageBitmap )! !
!FreeImageNonDestructiveOperation categoriesFor: #processDestructive:!actions!conversion!public! !
!FreeImageNonDestructiveOperation categoriesFor: #processNonDestructive:!actions!conversion!public! !

!FreeImageNonDestructiveOperation class methodsFor!

icon
	^##(ValueConverter icon)! !
!FreeImageNonDestructiveOperation class categoriesFor: #icon!development!public! !

FreeImageAdjustBrightnessOperation guid: (GUID fromString: '{81D6D4B6-7CEC-4470-8FA2-0C4E7A68C3E2}')!
FreeImageAdjustBrightnessOperation comment: ''!
!FreeImageAdjustBrightnessOperation categoriesForClass!Unclassified! !
!FreeImageAdjustBrightnessOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^self primAdjustContrast: aFreeImageBitmap percentage: contrast!

contrast
	^contrast!

contrast: anObject 
	contrast := anObject!

initialize
	super initialize.
	contrast := 0!

primAdjustContrast: aFreeImageBitmap percentage: percentage 
	^FreeImageLibrary errorCheck: [ :lib | lib freeImage_AdjustContrast: aFreeImageBitmap asParameter
		percentage: percentage]! !
!FreeImageAdjustBrightnessOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageAdjustBrightnessOperation categoriesFor: #contrast!accessing!public! !
!FreeImageAdjustBrightnessOperation categoriesFor: #contrast:!accessing!public! !
!FreeImageAdjustBrightnessOperation categoriesFor: #initialize!initializing!private! !
!FreeImageAdjustBrightnessOperation categoriesFor: #primAdjustContrast:percentage:!actions!conversion!primitives!private! !

!FreeImageAdjustBrightnessOperation class methodsFor!

contrast: anInteger 
	^(self new)
		contrast: anInteger;
		yourself! !
!FreeImageAdjustBrightnessOperation class categoriesFor: #contrast:!instance creation!public! !

FreeImageAdjustContrastOperation guid: (GUID fromString: '{8D331819-CE41-4B8B-8F2B-D8C0A347DA85}')!
FreeImageAdjustContrastOperation comment: ''!
!FreeImageAdjustContrastOperation categoriesForClass!Unclassified! !
!FreeImageAdjustContrastOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^self primAdjustBrightness: aFreeImageBitmap percentage: brightness!

brightness
	^brightness!

brightness: anObject 
	brightness := anObject!

initialize
	super initialize.
	brightness := 0!

primAdjustBrightness: aFreeImageBitmap percentage: percentage 
	^FreeImageLibrary 
		errorCheck: [:lib | lib freeImage_AdjustBrightness: aFreeImageBitmap asParameter percentage: percentage]! !
!FreeImageAdjustContrastOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageAdjustContrastOperation categoriesFor: #brightness!accessing!public! !
!FreeImageAdjustContrastOperation categoriesFor: #brightness:!accessing!public! !
!FreeImageAdjustContrastOperation categoriesFor: #initialize!initializing!private! !
!FreeImageAdjustContrastOperation categoriesFor: #primAdjustBrightness:percentage:!actions!conversion!primitives!private! !

!FreeImageAdjustContrastOperation class methodsFor!

brightness: anInteger 
	^(self new)
		brightness: anInteger;
		yourself! !
!FreeImageAdjustContrastOperation class categoriesFor: #brightness:!instance creation!public! !

FreeImageAdjustCurveOperation guid: (GUID fromString: '{8CFD41F4-70FB-4956-9AE5-7430A2752CCD}')!
FreeImageAdjustCurveOperation comment: ''!
!FreeImageAdjustCurveOperation categoriesForClass!Unclassified! !
!FreeImageAdjustCurveOperation methodsFor!

basicProcess: aFreeImageBitmap 

^self primAdjustCurve: aFreeImageBitmap lut: lut channel: channel!

channel
	^channel!

channel: anObject
	channel := anObject!

initialize
	super initialize.
	channel := FICC_RGB.
	lut := (0 to: 255) asByteArray.!

lut
	^lut!

lut: anObject
	lut := anObject!

primAdjustCurve: dib lut: aByteArray channel: aChannel 
	^FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_AdjustCurve: dib asParameter
				lut: aByteArray
				channel: aChannel]!

process: aFreeImageBitmap 
| copy |
	copy := aFreeImageBitmap copy.

	self basicProcess:  copy asParameter. 
	^copy!

useAlpha
	channel := FICC_ALPHA!

useBlack
	channel := FICC_BLACK!

useBlue
	channel := FICC_BLUE!

useGreen
	channel := FICC_GREEN!

useImaginary
	channel := FICC_IMAG!

usePhase
	channel := FICC_PHASE!

useReal
	channel := FICC_REAL!

useRed
	channel := FICC_RED!

useRGB
channel := FICC_RGB! !
!FreeImageAdjustCurveOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageAdjustCurveOperation categoriesFor: #channel!accessing!public! !
!FreeImageAdjustCurveOperation categoriesFor: #channel:!accessing!public! !
!FreeImageAdjustCurveOperation categoriesFor: #initialize!initializing!private! !
!FreeImageAdjustCurveOperation categoriesFor: #lut!accessing!public! !
!FreeImageAdjustCurveOperation categoriesFor: #lut:!accessing!public! !
!FreeImageAdjustCurveOperation categoriesFor: #primAdjustCurve:lut:channel:!actions!conversion!primitives!private! !
!FreeImageAdjustCurveOperation categoriesFor: #process:!actions!conversion!public! !
!FreeImageAdjustCurveOperation categoriesFor: #useAlpha!public! !
!FreeImageAdjustCurveOperation categoriesFor: #useBlack!public! !
!FreeImageAdjustCurveOperation categoriesFor: #useBlue!public! !
!FreeImageAdjustCurveOperation categoriesFor: #useGreen!public! !
!FreeImageAdjustCurveOperation categoriesFor: #useImaginary!public! !
!FreeImageAdjustCurveOperation categoriesFor: #usePhase!public! !
!FreeImageAdjustCurveOperation categoriesFor: #useReal!public! !
!FreeImageAdjustCurveOperation categoriesFor: #useRed!public! !
!FreeImageAdjustCurveOperation categoriesFor: #useRGB!public! !

FreeImageAdjustGammaOperation guid: (GUID fromString: '{C83E451A-5B6F-4E61-9E08-4705F31B095D}')!
FreeImageAdjustGammaOperation comment: ''!
!FreeImageAdjustGammaOperation categoriesForClass!Unclassified! !
!FreeImageAdjustGammaOperation methodsFor!

basicProcess: aFreeImageBitmap 
^self primAdjustGamma: aFreeImageBitmap gamma: gamma!

gamma
	^gamma!

gamma: aFloat 
	gamma := aFloat!

initialize
	super initialize.
gamma := 1.!

primAdjustGamma: dib  gamma: aFloat 
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_AdjustGamma: dib  asParameter gamma: aFloat]! !
!FreeImageAdjustGammaOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageAdjustGammaOperation categoriesFor: #gamma!accessing!public! !
!FreeImageAdjustGammaOperation categoriesFor: #gamma:!accessing!public! !
!FreeImageAdjustGammaOperation categoriesFor: #initialize!initializing!private! !
!FreeImageAdjustGammaOperation categoriesFor: #primAdjustGamma:gamma:!actions!conversion!primitives!private! !

!FreeImageAdjustGammaOperation class methodsFor!

gamma: anInteger
^(self new) gamma: anInteger;yourself! !
!FreeImageAdjustGammaOperation class categoriesFor: #gamma:!instance creation!public! !

FreeImageInvertOperation guid: (GUID fromString: '{FEC21AE7-157B-4322-B1A5-2503EFAEFFBB}')!
FreeImageInvertOperation comment: ''!
!FreeImageInvertOperation categoriesForClass!Unclassified! !
!FreeImageInvertOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^self primInvert: aFreeImageBitmap!

primInvert: dib
^FreeImageLibrary errorCheck: [ :lib | lib freeImage_Invert: dib asParameter].
! !
!FreeImageInvertOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageInvertOperation categoriesFor: #primInvert:!actions!conversion!primitives!private! !

FreeImagePasteOperation guid: (GUID fromString: '{F05043C7-A13B-484C-A59A-81A5E5099179}')!
FreeImagePasteOperation comment: ''!
!FreeImagePasteOperation categoriesForClass!Unclassified! !
!FreeImagePasteOperation methodsFor!

alpha
	^alpha!

alpha: anObject
	alpha := anObject!

basicProcess: aFreeImageBitmap 
	| originPoint |
	originPoint := (origin isKindOf: Point) 
				ifTrue: [origin]
				ifFalse: [(Rectangle origin: Point zero extent: aFreeImageBitmap extent) perform: origin].
	^self primPaste: aFreeImageBitmap position: originPoint!

bitmap
	^bitmap!

bitmap: anObject
	bitmap := anObject!

initialize
	super initialize.
	origin := #topLeft.
	
	bitmap := nil!

origin
	^origin!

origin: anIntegerOrSymbol 
	origin := anIntegerOrSymbol!

primPaste: dib position: position


	^FreeImageLibrary errorCheck: [ :lib | lib  
		freeImage_Paste: dib asParameter
		src: bitmap asParameter
		left: position x
		top: position y
		alpha: alpha]! !
!FreeImagePasteOperation categoriesFor: #alpha!accessing!public! !
!FreeImagePasteOperation categoriesFor: #alpha:!accessing!public! !
!FreeImagePasteOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImagePasteOperation categoriesFor: #bitmap!accessing!public! !
!FreeImagePasteOperation categoriesFor: #bitmap:!accessing!public! !
!FreeImagePasteOperation categoriesFor: #initialize!initializing!private! !
!FreeImagePasteOperation categoriesFor: #origin!accessing!public! !
!FreeImagePasteOperation categoriesFor: #origin:!accessing!public! !
!FreeImagePasteOperation categoriesFor: #primPaste:position:!actions!conversion!primitives!private! !

FreeImageSetChannelOperation guid: (GUID fromString: '{9E760899-8A53-497D-A178-332E96527F40}')!
FreeImageSetChannelOperation comment: ''!
!FreeImageSetChannelOperation categoriesForClass!Unclassified! !
!FreeImageSetChannelOperation methodsFor!

basicProcess: aFreeImageBitmap 
	(##(Array 
		with: FICC_RED
		with: FICC_GREEN
		with: FICC_BLUE
		with: FICC_ALPHA) includes: channel) 
		ifTrue: 
			[^self 
				primSetChannel: aFreeImageBitmap
				dib8: bitmap
				channel: channel.
			].
	(##(Array 
		with: FICC_REAL
		with: FICC_IMAG
		with: FICC_MAG
		with: FICC_PHASE) includes: channel) 
		ifTrue: 
			[^self 
				primSetComplexChannel: aFreeImageBitmap
				src: bitmap
				channel: channel].
	FreeImageError signal: 'Not supported Channel'!

bitmap
	^bitmap!

bitmap: anObject
	bitmap := anObject!

channel
	^channel!

channel: anObject
	channel := anObject!

initialize
	super initialize.
	channel := nil.
	bitmap := nil.!

primSetChannel: dib dib8: dib8 channel: aChannel 
	^FreeImageLibrary errorCheck: 
			[:lib | 
			lib 
				freeImage_SetChannel: dib asParameter
				dib8: dib8 asParameter
				channel: aChannel]!

primSetComplexChannel: dst src: src channel: aChannel

^FreeImageLibrary errorCheck: [ :lib | lib freeImage_SetComplexChannel: dst asParameter  src: src asParameter channel: channel].
!

useAlpha
	channel := FICC_ALPHA!

useBlue
	channel := FICC_BLUE!

useGreen
	channel := FICC_GREEN!

useRed
	channel := FICC_RED! !
!FreeImageSetChannelOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageSetChannelOperation categoriesFor: #bitmap!accessing!private! !
!FreeImageSetChannelOperation categoriesFor: #bitmap:!accessing!private! !
!FreeImageSetChannelOperation categoriesFor: #channel!accessing!public! !
!FreeImageSetChannelOperation categoriesFor: #channel:!accessing!public! !
!FreeImageSetChannelOperation categoriesFor: #initialize!initializing!private! !
!FreeImageSetChannelOperation categoriesFor: #primSetChannel:dib8:channel:!actions!conversion!primitives!private! !
!FreeImageSetChannelOperation categoriesFor: #primSetComplexChannel:src:channel:!actions!conversion!primitives!private! !
!FreeImageSetChannelOperation categoriesFor: #useAlpha!public! !
!FreeImageSetChannelOperation categoriesFor: #useBlue!public! !
!FreeImageSetChannelOperation categoriesFor: #useGreen!public! !
!FreeImageSetChannelOperation categoriesFor: #useRed!public! !

FreeImageAdaptiveLogarithmicToneMappingOperation guid: (GUID fromString: '{1AC60D62-7E7A-4437-B15D-0C03375E312E}')!
FreeImageAdaptiveLogarithmicToneMappingOperation comment: ''!
!FreeImageAdaptiveLogarithmicToneMappingOperation categoriesForClass!Unclassified! !
!FreeImageAdaptiveLogarithmicToneMappingOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^self primTmoDrago03: aFreeImageBitmap!

exposure
	^exposure!

exposure: aFloat 
	exposure := aFloat!

gamma
	^gamma!

gamma: aFloat 
	gamma := aFloat!

initialize
	super initialize.
	gamma := 2.2.
	exposure := 0!

primTmoDrago03: dib
	^FreeImageLibrary errorCheck: [ :lib | lib 
		freeImage_TmoDrago03: dib asParameter
		gamma: gamma
		exposure: exposure]! !
!FreeImageAdaptiveLogarithmicToneMappingOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageAdaptiveLogarithmicToneMappingOperation categoriesFor: #exposure!accessing!public! !
!FreeImageAdaptiveLogarithmicToneMappingOperation categoriesFor: #exposure:!accessing!public! !
!FreeImageAdaptiveLogarithmicToneMappingOperation categoriesFor: #gamma!accessing!public! !
!FreeImageAdaptiveLogarithmicToneMappingOperation categoriesFor: #gamma:!accessing!public! !
!FreeImageAdaptiveLogarithmicToneMappingOperation categoriesFor: #initialize!initializing!private! !
!FreeImageAdaptiveLogarithmicToneMappingOperation categoriesFor: #primTmoDrago03:!actions!conversion!primitives!private! !

!FreeImageAdaptiveLogarithmicToneMappingOperation class methodsFor!

gamma: gamma exposure: exposure
^self new gamma: gamma; exposure: exposure;yourself! !
!FreeImageAdaptiveLogarithmicToneMappingOperation class categoriesFor: #gamma:exposure:!instance creation!public! !

FreeImageColorQuantizeOperation guid: (GUID fromString: '{6EEF7E68-5A86-480F-B1EE-619DDE2E171D}')!
FreeImageColorQuantizeOperation comment: ''!
!FreeImageColorQuantizeOperation categoriesForClass!Unclassified! !
!FreeImageColorQuantizeOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_ColorQuantize: aFreeImageBitmap asParameter quantize: method!

initialize
super initialize.
	method := FIQ_WUQUANT!

useNeuQuantAlgorithm
	method := FIQ_NNQUANT!

useXiaolinWoAlgorithm
	method := FIQ_WUQUANT! !
!FreeImageColorQuantizeOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageColorQuantizeOperation categoriesFor: #initialize!initializing!private! !
!FreeImageColorQuantizeOperation categoriesFor: #useNeuQuantAlgorithm!public! !
!FreeImageColorQuantizeOperation categoriesFor: #useXiaolinWoAlgorithm!public! !

!FreeImageColorQuantizeOperation class methodsFor!

neuQuant
	^(self new)
		useNeuQuantAlgorithm;
		yourself!

xiaolinWoAlgorithm
	^(self new)
		useXiaolinWoAlgorithm
	
	;
		yourself! !
!FreeImageColorQuantizeOperation class categoriesFor: #neuQuant!instance creation!public! !
!FreeImageColorQuantizeOperation class categoriesFor: #xiaolinWoAlgorithm!instance creation!public! !

FreeImageCompositeOperation guid: (GUID fromString: '{A6A9DBCD-9634-4CF1-A1A0-164FC09A4CA4}')!
FreeImageCompositeOperation comment: ''!
!FreeImageCompositeOperation categoriesForClass!Unclassified! !
!FreeImageCompositeOperation methodsFor!

appBgkColor
	^appBgkColor asColor!

appBgkColor: aColor 
	appBgkColor := RGBQUAD fromColor: aColor.
	self useFileBgk: false.!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default 
		freeImage_Composite: aFreeImageBitmap asParameter
		useFileBkg: useFileBgk
		appBkColor: appBgkColor
		bg: bitmap asParameter!

bitmap
	^bitmap!

bitmap: anObject 
	bitmap := anObject.
	self useFileBgk:  true.!

initialize
	super initialize.
	bitmap := nil.
	appBgkColor := nil.
	useFileBgk := nil!

useFileBgk
	^useFileBgk!

useFileBgk: aBoolean 
	useFileBgk := aBoolean! !
!FreeImageCompositeOperation categoriesFor: #appBgkColor!accessing!public! !
!FreeImageCompositeOperation categoriesFor: #appBgkColor:!accessing!public! !
!FreeImageCompositeOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageCompositeOperation categoriesFor: #bitmap!accessing!public! !
!FreeImageCompositeOperation categoriesFor: #bitmap:!accessing!public! !
!FreeImageCompositeOperation categoriesFor: #initialize!initializing!private! !
!FreeImageCompositeOperation categoriesFor: #useFileBgk!accessing!private! !
!FreeImageCompositeOperation categoriesFor: #useFileBgk:!accessing!private! !

FreeImageConvertTo16BitsOperation guid: (GUID fromString: '{6D684F3B-8882-4960-B145-D7A9DC726E8F}')!
FreeImageConvertTo16BitsOperation comment: ''!
!FreeImageConvertTo16BitsOperation categoriesForClass!Unclassified! !
!FreeImageConvertTo16BitsOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^highDefGreen 
		ifTrue: [FreeImageLibrary default freeImage_ConvertTo16Bits565: aFreeImageBitmap asParameter ]
		ifFalse: [FreeImageLibrary default freeImage_ConvertTo16Bits555: aFreeImageBitmap asParameter ]!

highDefGreen
	^highDefGreen!

highDefGreen: anObject
	highDefGreen := anObject!

initialize
	highDefGreen := false! !
!FreeImageConvertTo16BitsOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageConvertTo16BitsOperation categoriesFor: #highDefGreen!accessing!public! !
!FreeImageConvertTo16BitsOperation categoriesFor: #highDefGreen:!accessing!public! !
!FreeImageConvertTo16BitsOperation categoriesFor: #initialize!initializing!private! !

!FreeImageConvertTo16BitsOperation class methodsFor!

for555

^self new highDefGreen:  false; yourself!

for565
	^(self new)
		highDefGreen: true;
		yourself!

withHighDefGreen
	^(self new)
		highDefGreen: true;
		yourself!

withoutHighDefGreen
	^(self new)
		highDefGreen: false;
		yourself! !
!FreeImageConvertTo16BitsOperation class categoriesFor: #for555!instance creation!public! !
!FreeImageConvertTo16BitsOperation class categoriesFor: #for565!instance creation!public! !
!FreeImageConvertTo16BitsOperation class categoriesFor: #withHighDefGreen!instance creation!public! !
!FreeImageConvertTo16BitsOperation class categoriesFor: #withoutHighDefGreen!instance creation!public! !

FreeImageConvertTo24BitsOperation guid: (GUID fromString: '{4BCEB00C-EEAD-4737-9757-035D4D192C9F}')!
FreeImageConvertTo24BitsOperation comment: ''!
!FreeImageConvertTo24BitsOperation categoriesForClass!Unclassified! !
!FreeImageConvertTo24BitsOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_ConvertTo24Bits: aFreeImageBitmap asParameter! !
!FreeImageConvertTo24BitsOperation categoriesFor: #basicProcess:!actions!conversion!private! !

FreeImageConvertTo32BitsOperation guid: (GUID fromString: '{C8BF481F-BE6A-486C-8AC1-86713324219D}')!
FreeImageConvertTo32BitsOperation comment: ''!
!FreeImageConvertTo32BitsOperation categoriesForClass!Unclassified! !
!FreeImageConvertTo32BitsOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_ConvertTo32Bits: aFreeImageBitmap asParameter! !
!FreeImageConvertTo32BitsOperation categoriesFor: #basicProcess:!actions!conversion!private! !

FreeImageConvertTo4BitsOperation guid: (GUID fromString: '{2FDCDDA9-52D1-465F-AE31-57D5D1FE5E0A}')!
FreeImageConvertTo4BitsOperation comment: ''!
!FreeImageConvertTo4BitsOperation categoriesForClass!Unclassified! !
!FreeImageConvertTo4BitsOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_ConvertTo4Bits: aFreeImageBitmap asParameter! !
!FreeImageConvertTo4BitsOperation categoriesFor: #basicProcess:!actions!conversion!private! !

FreeImageConvertTo8BitsOperation guid: (GUID fromString: '{374ABAD3-2F2F-4D30-8F2C-6FEA356AFFD1}')!
FreeImageConvertTo8BitsOperation comment: ''!
!FreeImageConvertTo8BitsOperation categoriesForClass!Unclassified! !
!FreeImageConvertTo8BitsOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_ConvertTo8Bits: aFreeImageBitmap asParameter! !
!FreeImageConvertTo8BitsOperation categoriesFor: #basicProcess:!actions!conversion!private! !

FreeImageConvertToGreyscaleOperation guid: (GUID fromString: '{2F11A6FD-19F1-4284-9776-E15997708EC0}')!
FreeImageConvertToGreyscaleOperation comment: ''!
!FreeImageConvertToGreyscaleOperation categoriesForClass!Unclassified! !
!FreeImageConvertToGreyscaleOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_ConvertToGreyscale: aFreeImageBitmap asParameter! !
!FreeImageConvertToGreyscaleOperation categoriesFor: #basicProcess:!actions!conversion!private! !

FreeImageConvertToMonochromeOperation guid: (GUID fromString: '{A2F59287-0C80-4C4F-A5A2-E9D5FA48F0DB}')!
FreeImageConvertToMonochromeOperation comment: ''!
!FreeImageConvertToMonochromeOperation categoriesForClass!Unclassified! !
!FreeImageConvertToMonochromeOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_Threshold: aFreeImageBitmap asParameter t: threshold!

initialize
super initialize.
	threshold := 128!

threshold
	^threshold!

threshold: anInteger 
	self assert: [anInteger <= 255].
	threshold := anInteger! !
!FreeImageConvertToMonochromeOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageConvertToMonochromeOperation categoriesFor: #initialize!initialization!private! !
!FreeImageConvertToMonochromeOperation categoriesFor: #threshold!accessing!public! !
!FreeImageConvertToMonochromeOperation categoriesFor: #threshold:!accessing!public! !

!FreeImageConvertToMonochromeOperation class methodsFor!

threshold: anInteger
^self new threshold: anInteger; yourself! !
!FreeImageConvertToMonochromeOperation class categoriesFor: #threshold:!instance creation!public! !

FreeImageConvertToRGBFOperation guid: (GUID fromString: '{B1786766-3EA7-4972-B5B2-15B6ABECBA74}')!
FreeImageConvertToRGBFOperation comment: ''!
!FreeImageConvertToRGBFOperation categoriesForClass!Unclassified! !
!FreeImageConvertToRGBFOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_ConvertToRGBF: aFreeImageBitmap asParameter! !
!FreeImageConvertToRGBFOperation categoriesFor: #basicProcess:!actions!conversion!private! !

FreeImageConvertToStandardTypeOperation guid: (GUID fromString: '{E046B420-CCC8-4AB7-8ABC-BE83B64D7B7C}')!
FreeImageConvertToStandardTypeOperation comment: ''!
!FreeImageConvertToStandardTypeOperation categoriesForClass!Unclassified! !
!FreeImageConvertToStandardTypeOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_ConvertToStandardType: aFreeImageBitmap asParameter scale_linear: scaleType!

initialize
super initialize.
	scaleType := true!

scaleLinear
scaleType := true!

scaleRouding
	scaleType := false!

scaleType
	^scaleType!

scaleType: anObject 
	scaleType := anObject! !
!FreeImageConvertToStandardTypeOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageConvertToStandardTypeOperation categoriesFor: #initialize!initialization!private! !
!FreeImageConvertToStandardTypeOperation categoriesFor: #scaleLinear!accessing!public! !
!FreeImageConvertToStandardTypeOperation categoriesFor: #scaleRouding!accessing!public! !
!FreeImageConvertToStandardTypeOperation categoriesFor: #scaleType!accessing!public! !
!FreeImageConvertToStandardTypeOperation categoriesFor: #scaleType:!accessing!public! !

!FreeImageConvertToStandardTypeOperation class methodsFor!

scaleLinear
^(self new) scaleLinear;yourself!

scaleRounding
	^(self new)
		scaleRouding ;
		yourself! !
!FreeImageConvertToStandardTypeOperation class categoriesFor: #scaleLinear!public! !
!FreeImageConvertToStandardTypeOperation class categoriesFor: #scaleRounding!public! !

FreeImageCopyOperation guid: (GUID fromString: '{06118AAB-3013-466D-9DE7-A70D95A86668}')!
FreeImageCopyOperation comment: ''!
!FreeImageCopyOperation categoriesForClass!Unclassified! !
!FreeImageCopyOperation methodsFor!

basicProcess: aFreeImageBitmap 
	| originPoint cornerPoint |
	originPoint := (origin isKindOf: Point) 
				ifTrue: [origin]
				ifFalse: [(Rectangle origin: Point zero extent: aFreeImageBitmap extent) perform: origin].
	cornerPoint := (corner isKindOf: Point) 
				ifTrue: [corner]
				ifFalse: [(Rectangle origin: Point zero extent: aFreeImageBitmap extent) perform: corner].
	^(FreeImageLibrary default 
				freeImage_Copy: aFreeImageBitmap asParameter
				left: originPoint x
				top: originPoint y
				right: cornerPoint x
				bottom: cornerPoint y)!

corner
	^corner!

corner: anIntegerOrSymbol 
	corner := anIntegerOrSymbol!

initialize
	super initialize.
origin := #topLeft. corner := #bottomRight!

origin
	^origin!

origin: anIntegerOrSymbol 
	origin := anIntegerOrSymbol!

rectangle: aRectangle
origin := aRectangle origin.
corner := aRectangle corner! !
!FreeImageCopyOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageCopyOperation categoriesFor: #corner!accessing!public! !
!FreeImageCopyOperation categoriesFor: #corner:!accessing!public! !
!FreeImageCopyOperation categoriesFor: #initialize!initializing!private! !
!FreeImageCopyOperation categoriesFor: #origin!accessing!public! !
!FreeImageCopyOperation categoriesFor: #origin:!accessing!public! !
!FreeImageCopyOperation categoriesFor: #rectangle:!accessing!public! !

!FreeImageCopyOperation class methodsFor!

origin: origin corner: corner
^self new origin: origin; corner: corner; yourself!

rectangle: aRectangle
^self new rectangle: aRectangle; yourself! !
!FreeImageCopyOperation class categoriesFor: #origin:corner:!public! !
!FreeImageCopyOperation class categoriesFor: #rectangle:!public! !

FreeImageDitherOperation guid: (GUID fromString: '{BD444167-BE95-4196-9E37-67C975FA58FE}')!
FreeImageDitherOperation comment: ''!
!FreeImageDitherOperation categoriesForClass!Unclassified! !
!FreeImageDitherOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_Dither: aFreeImageBitmap asParameter  algorithm: method!

initialize
	method := FID_FS!

method
	^method!

method: anObject
	method := anObject!

useBayer4x4
	method := FID_BAYER4x4!

useBayer8x8
	method := FID_BAYER8x8!

useCluster16x16
	method := FID_CLUSTER16x16!

useCluster6x6
	method := FID_CLUSTER6x6!

useCluster8x8
	method := FID_CLUSTER8x8!

useFloydSteinberg
	method := FID_FS! !
!FreeImageDitherOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageDitherOperation categoriesFor: #initialize!initializing!private! !
!FreeImageDitherOperation categoriesFor: #method!accessing!public! !
!FreeImageDitherOperation categoriesFor: #method:!accessing!public! !
!FreeImageDitherOperation categoriesFor: #useBayer4x4!public! !
!FreeImageDitherOperation categoriesFor: #useBayer8x8!public! !
!FreeImageDitherOperation categoriesFor: #useCluster16x16!public! !
!FreeImageDitherOperation categoriesFor: #useCluster6x6!public! !
!FreeImageDitherOperation categoriesFor: #useCluster8x8!public! !
!FreeImageDitherOperation categoriesFor: #useFloydSteinberg!public! !

!FreeImageDitherOperation class methodsFor!

bayer4x4
	^(self new)
		useBayer4x4 ;
		yourself!

bayer8x8
	^(self new)
		useBayer8x8 ;
		yourself!

cluster16x16
	^(self new)
		useCluster16x16 ;
		yourself!

cluster6x6
	^(self new)
		useCluster6x6 ;
		yourself!

cluster8x8
	^(self new)
		useCluster8x8;
		yourself!

floydSteinberg
^self new useFloydSteinberg;yourself! !
!FreeImageDitherOperation class categoriesFor: #bayer4x4!instance creation!public! !
!FreeImageDitherOperation class categoriesFor: #bayer8x8!instance creation!public! !
!FreeImageDitherOperation class categoriesFor: #cluster16x16!instance creation!public! !
!FreeImageDitherOperation class categoriesFor: #cluster6x6!instance creation!public! !
!FreeImageDitherOperation class categoriesFor: #cluster8x8!instance creation!public! !
!FreeImageDitherOperation class categoriesFor: #floydSteinberg!instance creation!public! !

FreeImageDynamicRangeReductionToneMapping guid: (GUID fromString: '{46EB4426-6732-4DA5-8EC5-9F421B11584D}')!
FreeImageDynamicRangeReductionToneMapping comment: ''!
!FreeImageDynamicRangeReductionToneMapping categoriesForClass!Unclassified! !
!FreeImageDynamicRangeReductionToneMapping methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default 
		freeImage_TmoReinhard05: aFreeImageBitmap asParameter
		intensity: intensity
		contrast: contrast!

contrast
	^contrast!

contrast: aFloat 
	contrast := aFloat!

initialize
	super initialize.
	intensity := 0.
	contrast := 0!

intensity
	^intensity!

intensity: aFloat 
	intensity := aFloat! !
!FreeImageDynamicRangeReductionToneMapping categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageDynamicRangeReductionToneMapping categoriesFor: #contrast!accessing!public! !
!FreeImageDynamicRangeReductionToneMapping categoriesFor: #contrast:!accessing!public! !
!FreeImageDynamicRangeReductionToneMapping categoriesFor: #initialize!initializing!private! !
!FreeImageDynamicRangeReductionToneMapping categoriesFor: #intensity!accessing!public! !
!FreeImageDynamicRangeReductionToneMapping categoriesFor: #intensity:!accessing!public! !

!FreeImageDynamicRangeReductionToneMapping class methodsFor!

intensity: intensity contrast: contrast 
	^(self new)
		intensity: intensity;
		contrast: contrast;
		yourself! !
!FreeImageDynamicRangeReductionToneMapping class categoriesFor: #intensity:contrast:!instance creation!public! !

FreeImageFlipHorizontalOperation guid: (GUID fromString: '{9FB1D0FE-2F53-4E70-9E51-0856FDA48E74}')!
FreeImageFlipHorizontalOperation comment: ''!
!FreeImageFlipHorizontalOperation categoriesForClass!Unclassified! !
!FreeImageFlipHorizontalOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_FlipHorizontal: aFreeImageBitmap asParameter! !
!FreeImageFlipHorizontalOperation categoriesFor: #basicProcess:!actions!conversion!private! !

FreeImageFlipVerticalOperation guid: (GUID fromString: '{F2423FB7-11AF-4C3A-AD8B-6A4CAE6769E1}')!
FreeImageFlipVerticalOperation comment: ''!
!FreeImageFlipVerticalOperation categoriesForClass!Unclassified! !
!FreeImageFlipVerticalOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_FlipVertical: aFreeImageBitmap asParameter! !
!FreeImageFlipVerticalOperation categoriesFor: #basicProcess:!actions!conversion!private! !

FreeImageGetChannelOperation guid: (GUID fromString: '{3DDBCEA9-51C3-492A-961C-F3380103A712}')!
FreeImageGetChannelOperation comment: ''!
!FreeImageGetChannelOperation categoriesForClass!Unclassified! !
!FreeImageGetChannelOperation methodsFor!

basicProcess: aFreeImageBitmap 
	(##(Array 
		with: FICC_RED
		with: FICC_GREEN
		with: FICC_BLUE
		with: FICC_ALPHA) includes: channel) 
		ifTrue: [^self primGetChannel: aFreeImageBitmap asParameter channel: channel].
	(##(Array 
		with: FICC_REAL
		with: FICC_IMAG
		with: FICC_MAG
		with: FICC_PHASE) includes: channel) 
		ifTrue: [^self primGetComplexChannel: aFreeImageBitmap channel: channel].
	FreeImageError  signal: 'Not supported Channel'!

channel
	^channel!

channel: anObject
	channel := anObject!

initialize
	super initialize.
	channel := FICC_RGB.
	!

primGetChannel: dib channel: aChannel 
	^FreeImageLibrary errorCheck: [:lib | lib freeImage_GetChannel: dib asParameter channel: aChannel]!

primGetComplexChannel: src channel: aChannel 
	^FreeImageLibrary 
		errorCheck: [:lib | lib freeImage_GetComplexChannel: src asParameter channel: aChannel]!

useAlpha
	channel := FICC_ALPHA!

useBlack
	channel := FICC_BLACK!

useBlue
	channel := FICC_BLUE!

useGreen
	channel := FICC_GREEN!

useImaginary
	channel := FICC_IMAG!

usePhase
	channel := FICC_PHASE!

useReal
	channel := FICC_REAL!

useRed
	channel := FICC_RED! !
!FreeImageGetChannelOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageGetChannelOperation categoriesFor: #channel!accessing!public! !
!FreeImageGetChannelOperation categoriesFor: #channel:!accessing!public! !
!FreeImageGetChannelOperation categoriesFor: #initialize!initializing!private! !
!FreeImageGetChannelOperation categoriesFor: #primGetChannel:channel:!actions!conversion!primitives!private! !
!FreeImageGetChannelOperation categoriesFor: #primGetComplexChannel:channel:!actions!conversion!primitives!private! !
!FreeImageGetChannelOperation categoriesFor: #useAlpha!public! !
!FreeImageGetChannelOperation categoriesFor: #useBlack!public! !
!FreeImageGetChannelOperation categoriesFor: #useBlue!public! !
!FreeImageGetChannelOperation categoriesFor: #useGreen!public! !
!FreeImageGetChannelOperation categoriesFor: #useImaginary!public! !
!FreeImageGetChannelOperation categoriesFor: #usePhase!public! !
!FreeImageGetChannelOperation categoriesFor: #useReal!public! !
!FreeImageGetChannelOperation categoriesFor: #useRed!public! !

FreeImageRescaleOperation guid: (GUID fromString: '{79652B7C-3798-4BCB-918A-E6FB7CCCF750}')!
FreeImageRescaleOperation comment: ''!
!FreeImageRescaleOperation categoriesForClass!Unclassified! !
!FreeImageRescaleOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default 
		freeImage_Rescale: aFreeImageBitmap asParameter
		dst_width: extent x
		dst_height: extent y
		filter: filter!

extent
	^extent!

extent: anObject
	extent := anObject!

filter
	^filter!

filter: anObject
	filter := anObject!

initialize
	super initialize.
	extent := 100 @ 100.
	filter := FILTER_BOX!

useBicubic
	filter := FILTER_BICUBIC!

useBilinear
	filter := FILTER_BILINEAR!

useBox
filter := FILTER_BOX!

useBspline
	filter := FILTER_BSPLINE!

useCatmulRom
	filter := FILTER_CATMULLROM!

useLanczos
	filter := FILTER_LANCZOS3! !
!FreeImageRescaleOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageRescaleOperation categoriesFor: #extent!accessing!public! !
!FreeImageRescaleOperation categoriesFor: #extent:!accessing!public! !
!FreeImageRescaleOperation categoriesFor: #filter!accessing!public! !
!FreeImageRescaleOperation categoriesFor: #filter:!accessing!public! !
!FreeImageRescaleOperation categoriesFor: #initialize!initializing!private! !
!FreeImageRescaleOperation categoriesFor: #useBicubic!public! !
!FreeImageRescaleOperation categoriesFor: #useBilinear!public! !
!FreeImageRescaleOperation categoriesFor: #useBox!public! !
!FreeImageRescaleOperation categoriesFor: #useBspline!public! !
!FreeImageRescaleOperation categoriesFor: #useCatmulRom!public! !
!FreeImageRescaleOperation categoriesFor: #useLanczos!public! !

!FreeImageRescaleOperation class methodsFor!

bicubic
	^(self new)
		useBicubic ;
		yourself!

bilinear
	^self new useBilinear ;yourself!

box
^self new useBox; yourself!

bspline
	^(self new)
		useBspline ;
		yourself!

catmulRom 
	^(self new)
		useCatmulRom ;
		yourself!

extent: aPoint 
	^(self new)
		extent: aPoint;
	yourself!

lanczos
	^(self new)
		useLanczos ;
		yourself! !
!FreeImageRescaleOperation class categoriesFor: #bicubic!instance creation!public! !
!FreeImageRescaleOperation class categoriesFor: #bilinear!instance creation!public! !
!FreeImageRescaleOperation class categoriesFor: #box!instance creation!public! !
!FreeImageRescaleOperation class categoriesFor: #bspline!instance creation!public! !
!FreeImageRescaleOperation class categoriesFor: #catmulRom!instance creation!public! !
!FreeImageRescaleOperation class categoriesFor: #extent:!instance creation!public! !
!FreeImageRescaleOperation class categoriesFor: #lanczos!instance creation!public! !

FreeImageRotateClassicOperation guid: (GUID fromString: '{D46F0215-34BB-4388-BC3F-A4C69F1ED07A}')!
FreeImageRotateClassicOperation comment: ''!
!FreeImageRotateClassicOperation categoriesForClass!Unclassified! !
!FreeImageRotateClassicOperation methodsFor!

angle
	^angle!

angle: anInteger 
	angle := anInteger!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default freeImage_RotateClassic: aFreeImageBitmap asParameter angle: angle!

initialize
	super initialize.
	angle := 90! !
!FreeImageRotateClassicOperation categoriesFor: #angle!accessing!public! !
!FreeImageRotateClassicOperation categoriesFor: #angle:!accessing!public! !
!FreeImageRotateClassicOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageRotateClassicOperation categoriesFor: #initialize!initializing!private! !

!FreeImageRotateClassicOperation class methodsFor!

angle: anInteger
^self new angle: anInteger ;yourself! !
!FreeImageRotateClassicOperation class categoriesFor: #angle:!instance creation!public! !

FreeImageThumbnailOperation guid: (GUID fromString: '{0C40DBA7-6EC0-4D34-8AED-861E4078DE82}')!
FreeImageThumbnailOperation comment: ''!
!FreeImageThumbnailOperation categoriesForClass!Unclassified! !
!FreeImageThumbnailOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default 
		freeImage_MakeThumbnail: aFreeImageBitmap asParameter
		max_pixel_size: size
		convert: convert!

convert
	^convert!

convert: aBoolean 
	convert := aBoolean!

initialize
	super initialize.
	size := 120.
	convert := true!

size
	^size!

size: anInteger 
	size := anInteger! !
!FreeImageThumbnailOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageThumbnailOperation categoriesFor: #convert!accessing!public! !
!FreeImageThumbnailOperation categoriesFor: #convert:!accessing!public! !
!FreeImageThumbnailOperation categoriesFor: #initialize!initializing!private! !
!FreeImageThumbnailOperation categoriesFor: #size!accessing!public! !
!FreeImageThumbnailOperation categoriesFor: #size:!accessing!public! !

!FreeImageThumbnailOperation class methodsFor!

size: anInteger 
	^(self new)
		size: anInteger;
		yourself!

size: anInteger convert: aBoolean
	^(self new)
		size: anInteger; convert: aBoolean;
		yourself! !
!FreeImageThumbnailOperation class categoriesFor: #size:!public! !
!FreeImageThumbnailOperation class categoriesFor: #size:convert:!public! !

FreeImageExtendedColorQuantizeOperation guid: (GUID fromString: '{95989888-0E4C-4CC6-B1CC-DDB3A48FCC4A}')!
FreeImageExtendedColorQuantizeOperation comment: ''!
!FreeImageExtendedColorQuantizeOperation categoriesForClass!Unclassified! !
!FreeImageExtendedColorQuantizeOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default 
		freeImage_ColorQuantizeEx: aFreeImageBitmap asParameter
		quantize: method
		paletteSize: paletteSize
		reserveSize: reservedPalette size
		reservePalette: (StructureArray 
				withAll: (reservedPalette collect: [:each | RGBQUAD fromColor: each])
				elementClass: RGBQUAD)!

initialize
super initialize.
	paletteSize := 256.
	reservedPalette := OrderedCollection new!

paletteSize
	^paletteSize!

paletteSize: anInteger 
self assert: [anInteger <= 256].
	paletteSize := anInteger!

reservedPalette
	^reservedPalette!

reservedPalette: aCollectionOfColors 
self assert: [aCollectionOfColors size <= 256].
	reservedPalette := aCollectionOfColors! !
!FreeImageExtendedColorQuantizeOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageExtendedColorQuantizeOperation categoriesFor: #initialize!initializing!private! !
!FreeImageExtendedColorQuantizeOperation categoriesFor: #paletteSize!accessing!public! !
!FreeImageExtendedColorQuantizeOperation categoriesFor: #paletteSize:!accessing!public! !
!FreeImageExtendedColorQuantizeOperation categoriesFor: #reservedPalette!accessing!public! !
!FreeImageExtendedColorQuantizeOperation categoriesFor: #reservedPalette:!accessing!public! !

FreeImageConvertToTypeOperation guid: (GUID fromString: '{3E980A0E-08B6-49DD-B9B1-FB78CFBBEB80}')!
FreeImageConvertToTypeOperation comment: ''!
!FreeImageConvertToTypeOperation categoriesForClass!Unclassified! !
!FreeImageConvertToTypeOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^FreeImageLibrary default 
		freeImage_ConvertToType: aFreeImageBitmap asParameter
		dst_type: type
		scale_linear: scaleType!

initialize
	super initialize.
	type := FIT_BITMAP!

type
	^type!

type: anObject
	type := anObject! !
!FreeImageConvertToTypeOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageConvertToTypeOperation categoriesFor: #initialize!initialization!private! !
!FreeImageConvertToTypeOperation categoriesFor: #type!accessing!public! !
!FreeImageConvertToTypeOperation categoriesFor: #type:!accessing!public! !

FreeImageRotateExOperation guid: (GUID fromString: '{5DD71885-87FF-4CC7-817F-0268FF2AFD73}')!
FreeImageRotateExOperation comment: ''!
!FreeImageRotateExOperation categoriesForClass!Unclassified! !
!FreeImageRotateExOperation methodsFor!

basicProcess: aFreeImageBitmap 
	^self subclassResponsibility!

initialize
	super initialize.
	shift := 0@0.
	origin := #center.
	mask := true!

mask
	^mask!

mask: aBoolean 
	mask := aBoolean!

origin
	^origin!

origin: aPointOrSymbol 
	origin := aPointOrSymbol!

primRotateEx: dib origin: pos 
	^FreeImageLibrary errorCheck: [ :lib | lib 
		freeImage_RotateEx: dib asParameter
		angle: angle
		x_shift: shift x
		y_shift: shift y
		x_origin: pos x
		y_origin: pos y
		use_mask: mask]!

process: aFreeImageBitmap 
	| originPoint |
	originPoint := (origin isKindOf: Point) 
				ifTrue: [origin]
				ifFalse: [(Rectangle origin: Point zero extent: aFreeImageBitmap extent) perform: origin].
	^FreeImageBitmap fromOwnedHandle:( self primRotateEx: aFreeImageBitmap origin: originPoint)!

shift
	^shift!

shift: aPoint 
	shift := aPoint! !
!FreeImageRotateExOperation categoriesFor: #basicProcess:!actions!conversion!private! !
!FreeImageRotateExOperation categoriesFor: #initialize!initializing!private! !
!FreeImageRotateExOperation categoriesFor: #mask!accessing!public! !
!FreeImageRotateExOperation categoriesFor: #mask:!accessing!public! !
!FreeImageRotateExOperation categoriesFor: #origin!accessing!public! !
!FreeImageRotateExOperation categoriesFor: #origin:!accessing!public! !
!FreeImageRotateExOperation categoriesFor: #primRotateEx:origin:!actions!conversion!public! !
!FreeImageRotateExOperation categoriesFor: #process:!actions!conversion!public! !
!FreeImageRotateExOperation categoriesFor: #shift!accessing!public! !
!FreeImageRotateExOperation categoriesFor: #shift:!accessing!public! !

!FreeImageRotateExOperation class methodsFor!

angle: anInteger shift: aPoint origin: aPointOrSymbol mask: aBoolean
^self new angle: anInteger; shift: aPoint; origin: aPointOrSymbol; mask: aBoolean; yourself! !
!FreeImageRotateExOperation class categoriesFor: #angle:shift:origin:mask:!instance creation!public! !

FreeImageAbstractJpegTransformationOperation guid: (GUID fromString: '{83CFAD6F-0A4F-43EB-8D4F-B761BB247C23}')!
FreeImageAbstractJpegTransformationOperation comment: ''!
!FreeImageAbstractJpegTransformationOperation categoriesForClass!Unclassified! !
!FreeImageAbstractJpegTransformationOperation methodsFor!

initialize
super initialize.
	perfect := false!

operation
^self subclassResponsibility!

perfect
	^perfect!

perfect: anObject
	perfect := anObject!

primJPEGTransform

	^FreeImageLibrary errorCheck: [ :lib | lib 
		freeImage_JPEGTransform: source
		dst_file: destination
		operation: self operation
		perfect: perfect]!

process
	self primJPEGTransform! !
!FreeImageAbstractJpegTransformationOperation categoriesFor: #initialize!initializing!private! !
!FreeImageAbstractJpegTransformationOperation categoriesFor: #operation!constants!conversion!private! !
!FreeImageAbstractJpegTransformationOperation categoriesFor: #perfect!accessing!public! !
!FreeImageAbstractJpegTransformationOperation categoriesFor: #perfect:!accessing!public! !
!FreeImageAbstractJpegTransformationOperation categoriesFor: #primJPEGTransform!actions!conversion!public! !
!FreeImageAbstractJpegTransformationOperation categoriesFor: #process!actions!conversion!public! !

FreeImageJpegCropOperation guid: (GUID fromString: '{141E996A-9CF3-4C0F-83C9-5E1F6A6FE78A}')!
FreeImageJpegCropOperation comment: ''!
!FreeImageJpegCropOperation categoriesForClass!Unclassified! !
!FreeImageJpegCropOperation methodsFor!

primJPEGCrop
	^FreeImageLibrary errorCheck: [ :lib | lib 
		freeImage_JPEGCrop: source
		dst_file: destination
		left: rectangle left
		top: rectangle top
		right: rectangle right
		bottom: rectangle bottom]!

process
	^self primJPEGCrop!

rectangle: aRectangle 
	rectangle := aRectangle! !
!FreeImageJpegCropOperation categoriesFor: #primJPEGCrop!actions!conversion!public! !
!FreeImageJpegCropOperation categoriesFor: #process!actions!conversion!public! !
!FreeImageJpegCropOperation categoriesFor: #rectangle:!accessing!public! !

FreeImageHorizontalFlipJpegOperation guid: (GUID fromString: '{3F9BC417-7489-416F-B40B-F14663917F72}')!
FreeImageHorizontalFlipJpegOperation comment: ''!
!FreeImageHorizontalFlipJpegOperation categoriesForClass!Unclassified! !
!FreeImageHorizontalFlipJpegOperation methodsFor!

operation
	^FIJPEG_OP_FLIP_H! !
!FreeImageHorizontalFlipJpegOperation categoriesFor: #operation!constants!conversion!private! !

FreeImageRotate180JpegOperation guid: (GUID fromString: '{21C8E078-2A4E-4FDC-B19E-DE814EDBB480}')!
FreeImageRotate180JpegOperation comment: ''!
!FreeImageRotate180JpegOperation categoriesForClass!Unclassified! !
!FreeImageRotate180JpegOperation methodsFor!

operation
	^FIJPEG_OP_ROTATE_180! !
!FreeImageRotate180JpegOperation categoriesFor: #operation!constants!conversion!private! !

FreeImageRotate270JpegOperation guid: (GUID fromString: '{F1852380-1801-4D37-8F1D-6F08674D3479}')!
FreeImageRotate270JpegOperation comment: ''!
!FreeImageRotate270JpegOperation categoriesForClass!Unclassified! !
!FreeImageRotate270JpegOperation methodsFor!

operation
	^FIJPEG_OP_ROTATE_270! !
!FreeImageRotate270JpegOperation categoriesFor: #operation!constants!conversion!private! !

FreeImageRotate90JpegOperation guid: (GUID fromString: '{7454BE41-534A-4EEC-A63C-58FEE3754570}')!
FreeImageRotate90JpegOperation comment: ''!
!FreeImageRotate90JpegOperation categoriesForClass!Unclassified! !
!FreeImageRotate90JpegOperation methodsFor!

operation
	^FIJPEG_OP_ROTATE_90! !
!FreeImageRotate90JpegOperation categoriesFor: #operation!constants!conversion!private! !

FreeImageTransposeJpegOperation guid: (GUID fromString: '{AB02F44F-48EE-4E25-A73A-FFBF2AE1089A}')!
FreeImageTransposeJpegOperation comment: ''!
!FreeImageTransposeJpegOperation categoriesForClass!Unclassified! !
!FreeImageTransposeJpegOperation methodsFor!

operation
	^FIJPEG_OP_TRANSPOSE! !
!FreeImageTransposeJpegOperation categoriesFor: #operation!constants!conversion!private! !

FreeImageTransverseJpegOperation guid: (GUID fromString: '{64D0DC2B-BFCE-48BF-B26F-B0CDE5A63D9B}')!
FreeImageTransverseJpegOperation comment: ''!
!FreeImageTransverseJpegOperation categoriesForClass!Unclassified! !
!FreeImageTransverseJpegOperation methodsFor!

operation
	^FIJPEG_OP_TRANSVERSE! !
!FreeImageTransverseJpegOperation categoriesFor: #operation!constants!conversion!private! !

FreeImageVerticalFlipJpegOperation guid: (GUID fromString: '{90EDA35F-1C85-4B06-9710-615A8D167B61}')!
FreeImageVerticalFlipJpegOperation comment: ''!
!FreeImageVerticalFlipJpegOperation categoriesForClass!Unclassified! !
!FreeImageVerticalFlipJpegOperation methodsFor!

operation
	^FIJPEG_OP_FLIP_V! !
!FreeImageVerticalFlipJpegOperation categoriesFor: #operation!constants!conversion!private! !

"Binary Globals"!

