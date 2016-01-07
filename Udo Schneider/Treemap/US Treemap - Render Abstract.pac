| package |
package := Package name: 'US Treemap - Render Abstract'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Render Abstract 0.021$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.021'.


package classNames
	add: #AbstractTreeMapRenderStrategy;
	add: #LightningColorCache;
	add: #LightningRenderStrategy;
	add: #TreeMapRenderState;
	yourself.

package methodNames
	add: #Color -> #darkenBy:;
	add: 'Pen class' -> #null;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #AbstractTreeMapRenderStrategy
	instanceVariableNames: 'renderer bitmap canvas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #LightningColorCache
	instanceVariableNames: 'cache fillBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #TreeMapRenderState
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractTreeMapRenderStrategy subclass: #LightningRenderStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Color methodsFor!

darkenBy: aNumber 
	"Return the color darkened by aNumber (0-255).
		0: Don't darken at all
		255: Maximum darkening (Black)"

	| factor rgb |
	factor := (255 - aNumber) / 255.
	rgb := self asRGB.
	^Color 
		red: rgb red * factor
		green: rgb green * factor
		blue: rgb blue * factor! !
!Color categoriesFor: #darkenBy:!*-not in class package!converting!public! !

!Pen class methodsFor!

null


	^self fromLogPen: ((LOGPEN new)
				style: PS_NULL;
			
				yourself)! !
!Pen class categoriesFor: #null!*-not in class package!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

AbstractTreeMapRenderStrategy guid: (GUID fromString: '{DE4ED926-D19A-428A-A19B-4FB1B6AAB08B}')!
AbstractTreeMapRenderStrategy comment: ''!
!AbstractTreeMapRenderStrategy categoriesForClass!Unclassified! !
!AbstractTreeMapRenderStrategy methodsFor!

initialize!

initialRenderState
	^TreeMapRenderState new!

render: node using: renderState 
	self subclassResponsibility!

renderNode: node using: renderState 
	node isLeafNode ifTrue: [self render: node using: renderState]!

setBitmap: anObject 
	bitmap := anObject.
canvas := bitmap canvas.!

setCanvas: anObject 
bitmap := nil.
	canvas := anObject!

setRenderer: aTreeMapRenderer
	renderer := aTreeMapRenderer! !
!AbstractTreeMapRenderStrategy categoriesFor: #initialize!*-in class package!private! !
!AbstractTreeMapRenderStrategy categoriesFor: #initialRenderState!*-in class package!public! !
!AbstractTreeMapRenderStrategy categoriesFor: #render:using:!*-in class package!public! !
!AbstractTreeMapRenderStrategy categoriesFor: #renderNode:using:!*-in class package!public! !
!AbstractTreeMapRenderStrategy categoriesFor: #setBitmap:!*-in class package!accessing!private! !
!AbstractTreeMapRenderStrategy categoriesFor: #setCanvas:!*-in class package!accessing!private! !
!AbstractTreeMapRenderStrategy categoriesFor: #setRenderer:!*-in class package!accessing!public! !

!AbstractTreeMapRenderStrategy class methodsFor!

description
^self class displayString!

new
^super new initialize! !
!AbstractTreeMapRenderStrategy class categoriesFor: #description!*-in class package!public! !
!AbstractTreeMapRenderStrategy class categoriesFor: #new!*-in class package!public! !

LightningColorCache guid: (GUID fromString: '{7702B83E-C089-4788-90B8-1EC08A5B6FAE}')!
LightningColorCache comment: ''!
!LightningColorCache categoriesForClass!Unclassified! !
!LightningColorCache methodsFor!

at: aColor 
	^cache at: aColor ifAbsentPut: [ (fillBlock value: aColor)]!

setCacheSize: anInteger 
	cache := LookupTable new: anInteger!

setFillBlock: aBlock 
	fillBlock := aBlock! !
!LightningColorCache categoriesFor: #at:!*-in class package!private! !
!LightningColorCache categoriesFor: #setCacheSize:!*-in class package!private! !
!LightningColorCache categoriesFor: #setFillBlock:!*-in class package!private! !

!LightningColorCache class methodsFor!

cacheSize: anInteger fillBlock: aBlock 
	^(self basicNew)
		setCacheSize: anInteger;
		setFillBlock: aBlock;
		yourself!

defaultCacheSize
	^32!

fillBlock: aBlock 
^self cacheSize: self defaultCacheSize  fillBlock: aBlock !

new
	self shouldNotImplement! !
!LightningColorCache class categoriesFor: #cacheSize:fillBlock:!*-in class package!public! !
!LightningColorCache class categoriesFor: #defaultCacheSize!*-in class package!public! !
!LightningColorCache class categoriesFor: #fillBlock:!*-in class package!public! !
!LightningColorCache class categoriesFor: #new!*-in class package!public! !

TreeMapRenderState guid: (GUID fromString: '{86C9488C-CF7D-4416-8EEB-0D4EF4F9E763}')!
TreeMapRenderState comment: ''!
!TreeMapRenderState categoriesForClass!Unclassified! !
!TreeMapRenderState methodsFor!

setRootRectangle: aRectangle 
	!

update: aNode horizontalIn: aRectangle	^self!

update: aNode in: aRectangle 
	^self!

update: aNode verticalIn: aRectangle 	^self! !
!TreeMapRenderState categoriesFor: #setRootRectangle:!*-in class package!public! !
!TreeMapRenderState categoriesFor: #update:horizontalIn:!*-in class package!public! !
!TreeMapRenderState categoriesFor: #update:in:!*-in class package!public! !
!TreeMapRenderState categoriesFor: #update:verticalIn:!*-in class package!public! !

LightningRenderStrategy guid: (GUID fromString: '{3C41E114-2BCB-4255-AAF3-C3AF7BD0988A}')!
LightningRenderStrategy comment: ''!
!LightningRenderStrategy categoriesForClass!Unclassified! !
!LightningRenderStrategy methodsFor!

ambientLightIntensity
	^55!

directLightIntensity
	^200!

renderStep
	^4! !
!LightningRenderStrategy categoriesFor: #ambientLightIntensity!*-in class package!public! !
!LightningRenderStrategy categoriesFor: #directLightIntensity!*-in class package!public! !
!LightningRenderStrategy categoriesFor: #renderStep!*-in class package!public! !

"Binary Globals"!

