| package |
package := Package name: 'US Treemap - Render Cushion DirectToMemory'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Render Cushion DirectToMemory 0.015$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.015'.


package classNames
	add: #DirectCushionRenderStrategy;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Treemap - Render Cushion Abstract';
	yourself).

package!

"Class Definitions"!

AbstractCushionRenderStrategy subclass: #DirectCushionRenderStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

DirectCushionRenderStrategy guid: (GUID fromString: '{C0787843-E04D-48FF-B617-EFA736E010F4}')!
DirectCushionRenderStrategy comment: ''!
!DirectCushionRenderStrategy categoriesForClass!Unclassified! !
!DirectCushionRenderStrategy methodsFor!

createColorTableFor: node 
	"| shadedBrushes basecolor |
	basecolor := renderer nodeColor: node object.
	shadedBrushes := Array new: 256.
	1 to: 256
		do: 
			[:shade | 
			shadedBrushes at: shade
				put: (basecolor darkenBy: 255 
								- (self ambientLightIntensity + (self directLightIntensity * ((shade - 1) / 255)))) 
						asDWORD].
	^shadedBrushes"
	| basecolor |
	basecolor := renderer nodeColor: node object.

	^(1 to: 256) collect: [ :each |(basecolor darkenBy: 255 
								- (self ambientLightIntensity + (self directLightIntensity * ((each - 1) / 255)))) 
						asDWORD].
!

render: node using: aRenderState 
	| shadedBrushes surface |
	shadedBrushes := self createColorTableFor: node.
	surface := aRenderState surface.
	canvas
		selectClipRegion: nil;
		fillRectangle: node layoutRectangle truncated brush: (Brush color: Color white).
	node layoutRectangle top truncated to: node layoutRectangle bottom truncated - 1
		do: 
			[:iy | 
			| pixelAddress |
			pixelAddress := iy * bitmap getInfo bmWidthBytes 
						+ (node layoutRectangle left truncated * 4).
			node layoutRectangle left truncated to: node layoutRectangle right truncated
				do: 
					[:ix | 
					| lightIntensity |
					lightIntensity := surface lightIntensityAt: ix @ iy.
					bitmap imageBits dwordAtOffset: pixelAddress
						put: (shadedBrushes at: (0 max: (lightIntensity * 255) truncated) + 1).
					pixelAddress := pixelAddress + 4]]! !
!DirectCushionRenderStrategy categoriesFor: #createColorTableFor:!*-in class package!public! !
!DirectCushionRenderStrategy categoriesFor: #render:using:!*-in class package!public! !

!DirectCushionRenderStrategy class methodsFor!

description
	^'Cushion - direct memory access'! !
!DirectCushionRenderStrategy class categoriesFor: #description!*-in class package!public! !

"Binary Globals"!

