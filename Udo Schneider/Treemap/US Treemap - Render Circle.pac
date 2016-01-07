| package |
package := Package name: 'US Treemap - Render Circle'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Render Circle 0.016$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.016'.


package classNames
	add: #CircleRenderState;
	add: #CircleRenderStrategy;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Treemap - Render Abstract';
	yourself).

package!

"Class Definitions"!

LightningRenderStrategy subclass: #CircleRenderStrategy
	instanceVariableNames: 'brushCache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreeMapRenderState subclass: #CircleRenderState
	instanceVariableNames: 'rectangle vector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

CircleRenderStrategy guid: (GUID fromString: '{5733B7EA-55B3-4EB1-A8B4-F2C9A6DB56E9}')!
CircleRenderStrategy comment: ''!
!CircleRenderStrategy categoriesForClass!Unclassified! !
!CircleRenderStrategy methodsFor!

initialize
	brushCache := LightningColorCache fillBlock: 
					[:basecolor | 
					(0 to: self directLightIntensity) 
						collect: [:index | Brush color: (basecolor darkenBy: 255 - index - self ambientLightIntensity)]]!

initialRenderState
	^CircleRenderState new!

render: node using: renderSettings 
	| nodeRectangle center radius r brushes vector ellipsenodeRectangle |
	nodeRectangle := node layoutRectangle.
	radius := ((nodeRectangle center - nodeRectangle topLeft) r * 1.2) truncated.
	brushes := brushCache at: (renderer nodeColor: node object).
	canvas
		selectClipRegion: (Region rectangle: nodeRectangle truncated);
		brush: (brushes first);
		fillRectangle: nodeRectangle truncated;
		pen: Pen null.
	vector := renderSettings vector / self directLightIntensity.
	center := nodeRectangle center.
	0 to: self directLightIntensity
		by: self renderStep
		do: 
			[:index | 
			r := radius * (1 - (index / self directLightIntensity)).
			ellipsenodeRectangle := ((center - r corner: center + r) moveBy: vector * index) 
						truncated.
			canvas
				brush: (brushes at: index + 1);
				ellipse: ellipsenodeRectangle]! !
!CircleRenderStrategy categoriesFor: #initialize!*-in class package!public! !
!CircleRenderStrategy categoriesFor: #initialRenderState!*-in class package!public! !
!CircleRenderStrategy categoriesFor: #render:using:!*-in class package!public! !

!CircleRenderStrategy class methodsFor!

description
	^'Circle'! !
!CircleRenderStrategy class categoriesFor: #description!*-in class package!public! !

CircleRenderState guid: (GUID fromString: '{C20B81B7-6778-4512-88A8-9D8F61300857}')!
CircleRenderState comment: ''!
!CircleRenderState categoriesForClass!Unclassified! !
!CircleRenderState methodsFor!

setRectangle: aRectangle vector: aVector 
	rectangle := aRectangle.
	vector := aVector.!

setRootRectangle: aRectangle 
	rectangle := aRectangle!

update: aNode horizontalIn: aRectangle 
^self update: aNode in: aRectangle !

update: aNode in: aRectangle 
	((aNode hasParent ) and: [aNode hasExtent])
		ifTrue: 
			[| newVector |
			newVector := (rectangle center - aRectangle center) / (rectangle area / aRectangle area) + self vector.
			^self class rectangle: aRectangle vector: newVector]
		ifFalse: [^self class rectangle: aRectangle vector: 0 @ 0]!

update: aNode verticalIn: aRectangle 
	^self update: aNode in: aRectangle !

vector
^vector! !
!CircleRenderState categoriesFor: #setRectangle:vector:!*-in class package!private! !
!CircleRenderState categoriesFor: #setRootRectangle:!*-in class package!public! !
!CircleRenderState categoriesFor: #update:horizontalIn:!*-in class package!public! !
!CircleRenderState categoriesFor: #update:in:!*-in class package!public! !
!CircleRenderState categoriesFor: #update:verticalIn:!*-in class package!public! !
!CircleRenderState categoriesFor: #vector!*-in class package!public! !

!CircleRenderState class methodsFor!

new
	^self rectangle: (0 @ 0 extent: 0 @ 0) vector: 0 @ 0!

rectangle: aRectangle vector: aVector 
	^self basicNew setRectangle: aRectangle vector: aVector; yourself! !
!CircleRenderState class categoriesFor: #new!*-in class package!public! !
!CircleRenderState class categoriesFor: #rectangle:vector:!*-in class package!public! !

"Binary Globals"!

