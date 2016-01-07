| package |
package := Package name: 'US Treemap - Render Cushion External'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Render Cushion External 0.013$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.013'.


package classNames
	add: #_CushionSurface;
	add: #ExternalCushionRenderStrategy;
	add: #TreemapLibrary;
	yourself.

package methodNames
	add: #CushionSurface -> #asParameter;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Treemap - Render Cushion Abstract';
	yourself).

package!

"Class Definitions"!

AbstractCushionRenderStrategy subclass: #ExternalCushionRenderStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #TreemapLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #_CushionSurface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!CushionSurface methodsFor!

asParameter
^_CushionSurface fromCushionSurface: self.! !
!CushionSurface categoriesFor: #asParameter!*-not in class package!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ExternalCushionRenderStrategy guid: (GUID fromString: '{1CCF6B36-05D9-4280-9A95-7BA9A6255C58}')!
ExternalCushionRenderStrategy comment: ''!
!ExternalCushionRenderStrategy categoriesForClass!Unclassified! !
!ExternalCushionRenderStrategy methodsFor!

createColorTableFor: node 
	| shadedBrushes basecolor |
	basecolor := renderer nodeColor: node object.
	shadedBrushes := DWORDArray new: 256.
	TreemapLibrary default 
		createColorTable: shadedBrushes yourAddress
		basecolor: basecolor asDWORD value
		ambientLightIntensity: self ambientLightIntensity
		directLightIntensity: self directLightIntensity.
	^shadedBrushes!

render: node using: aRenderState 
	| shadedBrushes surface |
	shadedBrushes := self createColorTableFor: node.
	surface := aRenderState surface.
	TreemapLibrary default 
		renderCushionOn: bitmap getInfo
		in: node layoutRectangle truncated asParameter
		surface: surface asParameter
		colors: shadedBrushes
		lightx: surface lightDirection x
		lighty: surface lightDirection y
		lightz: surface lightDirection z! !
!ExternalCushionRenderStrategy categoriesFor: #createColorTableFor:!*-in class package!public! !
!ExternalCushionRenderStrategy categoriesFor: #render:using:!*-in class package!public! !

!ExternalCushionRenderStrategy class methodsFor!

description
	^'Cushion - external DLL'! !
!ExternalCushionRenderStrategy class categoriesFor: #description!*-in class package!public! !

TreemapLibrary guid: (GUID fromString: '{F6CF0182-51CC-4D16-8EB2-936997409051}')!
TreemapLibrary comment: ''!
!TreemapLibrary categoriesForClass!Unclassified! !
!TreemapLibrary methodsFor!

createColorTable: shadedBrushes basecolor: basecolor ambientLightIntensity: ambientLightIntensity  directLightIntensity: directLightIntensity
	"void APIENTRY __declspec(dllexport) createColorTable (
		DWORD shadedBrushes[],
		DWORD basecolor,
		DWORD ambientLightIntensity,
		DWORD directLightIntensity)"

	<stdcall: void createColorTable lpvoid dword dword dword>
	^self invalidCall



!

renderCushionOn: dib in: layoutRectangle surface: surface colors: colors lightx: lightx lighty: lighty lightz: lightz 
	"void APIENTRY __declspec(dllexport) renderCushion(
		DIBSECTION dib,
		RECT layoutRectangle,
		CushionSurface surface,
		DWORD colors,
		float lightx,
		float lighty,
		float lightz)"

	<stdcall: void renderCushion BITMAP RECT _CushionSurface lpvoid double double double>
	^self invalidCall! !
!TreemapLibrary categoriesFor: #createColorTable:basecolor:ambientLightIntensity:directLightIntensity:!*-in class package!public! !
!TreemapLibrary categoriesFor: #renderCushionOn:in:surface:colors:lightx:lighty:lightz:!*-in class package!public! !

!TreemapLibrary class methodsFor!

fileName
	^'treemap.dll'! !
!TreemapLibrary class categoriesFor: #fileName!*-in class package!public! !

_CushionSurface guid: (GUID fromString: '{84277998-5DA6-479A-AB81-A5760C5402EE}')!
_CushionSurface comment: ''!
!_CushionSurface categoriesForClass!Unclassified! !
!_CushionSurface methodsFor!

bottom
	"Answer the receiver's bottom field as a Smalltalk object."

	^(bytes doubleAtOffset: 24)!

bottom: anObject
	"Set the receiver's bottom field to the value of anObject."

	bytes doubleAtOffset: 24 put: anObject!

left
	"Answer the receiver's left field as a Smalltalk object."

	^(bytes doubleAtOffset: 0)!

left: anObject
	"Set the receiver's left field to the value of anObject."

	bytes doubleAtOffset: 0 put: anObject!

right
	"Answer the receiver's right field as a Smalltalk object."

	^(bytes doubleAtOffset: 16)!

right: anObject
	"Set the receiver's right field to the value of anObject."

	bytes doubleAtOffset: 16 put: anObject!

top
	"Answer the receiver's top field as a Smalltalk object."

	^(bytes doubleAtOffset: 8)!

top: anObject
	"Set the receiver's top field to the value of anObject."

	bytes doubleAtOffset: 8 put: anObject! !
!_CushionSurface categoriesFor: #bottom!**compiled accessors**!*-in class package!public! !
!_CushionSurface categoriesFor: #bottom:!**compiled accessors**!*-in class package!public! !
!_CushionSurface categoriesFor: #left!**compiled accessors**!*-in class package!public! !
!_CushionSurface categoriesFor: #left:!**compiled accessors**!*-in class package!public! !
!_CushionSurface categoriesFor: #right!**compiled accessors**!*-in class package!public! !
!_CushionSurface categoriesFor: #right:!**compiled accessors**!*-in class package!public! !
!_CushionSurface categoriesFor: #top!**compiled accessors**!*-in class package!public! !
!_CushionSurface categoriesFor: #top:!**compiled accessors**!*-in class package!public! !

!_CushionSurface class methodsFor!

defineFields
	"Define the fields of the _CushionSurface structure.
		_CushionSurface compileDefinition
	
		typedef 		struct _CushionSurface {
			double left;
			double top;
			double right;
			double bottom;
		} CushionSurface;

	"

	self
		defineField: #left
			type: DOUBLEField new
			offset: 0;
		defineField: #top
			type: DOUBLEField new
			offset: 8;
		defineField: #right
			type: DOUBLEField new
			offset: 16;
		defineField: #bottom
			type: DOUBLEField new
			offset: 24.
	self byteSize: 32!

fromCushionSurface: aCushionSurface 
	^self 
		left: aCushionSurface left
		top: aCushionSurface top
		right: aCushionSurface right
		bottom: aCushionSurface bottom!

fromRectangle: aRectangle 
	"Answer a new instance of the receiver instantiated from the Smalltalk
	Rectangle, aRectangle."
#deprecated.

	^self 
		left: aRectangle left
		top: aRectangle top
		right: aRectangle right
		bottom: aRectangle bottom!

left: left top: top right: right bottom: bottom 
	"Answer a new instance of the receiver with the specified position
	corner positions.
	Implementation Note: For performance reasons do this all with low-level
	inline code."

	^self basicNew bytes: ((ByteArray newFixed: 32)
				doubleAtOffset: 0 put: left;
				doubleAtOffset: 8 put: top;
				doubleAtOffset: 16 put: right;
				doubleAtOffset: 24 put: bottom;
				yourself)! !
!_CushionSurface class categoriesFor: #defineFields!*-in class package!initializing!public! !
!_CushionSurface class categoriesFor: #fromCushionSurface:!*-in class package!instance creation!public! !
!_CushionSurface class categoriesFor: #fromRectangle:!*-in class package!instance creation!public! !
!_CushionSurface class categoriesFor: #left:top:right:bottom:!*-in class package!instance creation!public! !

"Binary Globals"!

