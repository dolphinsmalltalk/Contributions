| package |
package := Package name: 'US Treemap - Render Cushion Optimized'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Render Cushion Optimized 0.015$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.015'.


package classNames
	add: #OptimizedCushionRenderStrategy;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Treemap - Render Abstract';
	add: 'US Treemap - Render Cushion Abstract';
	yourself).

package!

"Class Definitions"!

AbstractCushionRenderStrategy subclass: #OptimizedCushionRenderStrategy
	instanceVariableNames: 'brushCache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

OptimizedCushionRenderStrategy guid: (GUID fromString: '{B73FF5B2-C498-4819-97D0-F1BD1057AB11}')!
OptimizedCushionRenderStrategy comment: ''!
!OptimizedCushionRenderStrategy categoriesForClass!Unclassified! !
!OptimizedCushionRenderStrategy methodsFor!

initialize
	brushCache := LightningColorCache fillBlock: 
					[:basecolor | 
					(0 to: self directLightIntensity) 
						collect: [:index | Brush color: (basecolor darkenBy: 255 - index - self ambientLightIntensity)]]!

render: node using: aRenderState 
	| surface brushes |
	brushes := brushCache at: (renderer nodeColor: node object).
	surface := aRenderState surface.
	canvas
		selectClipRegion: (Region rectangle: node layoutRectangle truncated);
		pen: Pen null.
	node layoutRectangle top truncated to: node layoutRectangle bottom truncated
		by: self renderStep
		do: 
			[:iy | 
			node layoutRectangle left truncated to: node layoutRectangle right truncated
				by: self renderStep
				do: 
					[:ix | 
					| lightIntensity |
					lightIntensity := (0 max: self directLightIntensity * (surface lightIntensityAt: ix @ iy)) 
								truncated.
					canvas fillRectangle: (ix @ iy extent: self renderStep@self renderStep)
						brush: (brushes at: lightIntensity + 1)]]! !
!OptimizedCushionRenderStrategy categoriesFor: #initialize!*-in class package!public! !
!OptimizedCushionRenderStrategy categoriesFor: #render:using:!*-in class package!public! !

!OptimizedCushionRenderStrategy class methodsFor!

description
	
	^'Cushion - GDI optimized'! !
!OptimizedCushionRenderStrategy class categoriesFor: #description!*-in class package!public! !

"Binary Globals"!

