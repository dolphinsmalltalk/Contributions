| package |
package := Package name: 'US Treemap - Render Cushion Abstract'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Render Cushion Abstract 0.017$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.017'.


package classNames
	add: #AbstractCushionRenderStrategy;
	add: #CushionRenderState;
	add: #CushionSurface;
	add: #VanillaCushionRenderStrategy;
	yourself.

package methodNames
	add: #Color -> #asDWORD;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Treemap - Render Abstract';
	yourself).

package!

"Class Definitions"!

Object subclass: #CushionSurface
	instanceVariableNames: 'left top right bottom'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LightningRenderStrategy subclass: #AbstractCushionRenderStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractCushionRenderStrategy subclass: #VanillaCushionRenderStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreeMapRenderState subclass: #CushionRenderState
	instanceVariableNames: 'height factor surface'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Color methodsFor!

asDWORD
	| temp |
	temp := self asRGB.
	^DWORD fromInteger: temp red * 65536 + (temp green * 256) + temp blue! !
!Color categoriesFor: #asDWORD!*-not in class package!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

CushionSurface guid: (GUID fromString: '{02F037F7-E2E4-4768-8593-AF95595F27DD}')!
CushionSurface comment: ''!
!CushionSurface categoriesForClass!Unclassified! !
!CushionSurface methodsFor!

addRidge: aRectangle height: h 
	^self class 
		left: self left 
				+ (((4 * h) * (aRectangle right + aRectangle left)) / (aRectangle right - aRectangle left))
		top: self top 
				+ (((4 * h) * (aRectangle top + aRectangle bottom)) / (aRectangle top - aRectangle bottom))
		right: self right - ((4 * h) / (aRectangle right - aRectangle left))
		bottom: self bottom - ((4 * h) / (aRectangle top - aRectangle bottom))!

addRidgeHorizontalMin: x1 max: x2 height: h 
	^self class 
		left: self left + (((4 * h) * (x2 + x1)) / (x2 - x1))
		top: self top
		right: self right - ((4 * h) / (x2 - x1))
		bottom: self bottom!

addRidgeVerticalMin: x1 max: x2 height: h 
	^self class 
		left: self left
		top: self top + (((4 * h) * (x2 + x1)) / (x2 - x1))
		right: self right
		bottom: self bottom - ((4 * h) / (x2 - x1))!

bottom
	^bottom!

bottom: anObject
	bottom := anObject!

left
	^left!

left: anObject
	left := anObject!

lightDirection
	"^##(0.09759 @ 0.19518 @ 0.9759)"

	^##(| direction |
	direction := Point3D x: 1 y: 2 z: 10.
	direction / direction r)!

lightIntensityAt: aPoint 
	| nx ny |
	nx := -1 * (2 * self right * aPoint x + self left).
	ny := -1 * (2 * self bottom * aPoint y + self top).
	^(self lightDirection dotProduct: (Point3D x: nx y: ny z: 1)) / (nx squared + ny squared + 1) sqrt!

printOn: aStream 
	aStream
		nextPutAll: '( ';
		print: left @ top;
		nextPutAll: ' / ';
		print: right @ bottom;
		nextPutAll: ' )'!

right
	^right!

right: anObject
	right := anObject!

setLeft: x1 top: y1 right: x2 bottom: y2 
	left := x1.
	top := y1.
	right := x2.
	bottom := y2!

top
	^top!

top: anObject
	top := anObject! !
!CushionSurface categoriesFor: #addRidge:height:!*-in class package!public! !
!CushionSurface categoriesFor: #addRidgeHorizontalMin:max:height:!*-in class package!public! !
!CushionSurface categoriesFor: #addRidgeVerticalMin:max:height:!*-in class package!public! !
!CushionSurface categoriesFor: #bottom!*-in class package!accessing!private! !
!CushionSurface categoriesFor: #bottom:!*-in class package!accessing!private! !
!CushionSurface categoriesFor: #left!*-in class package!accessing!private! !
!CushionSurface categoriesFor: #left:!*-in class package!accessing!private! !
!CushionSurface categoriesFor: #lightDirection!*-in class package!public! !
!CushionSurface categoriesFor: #lightIntensityAt:!*-in class package!public! !
!CushionSurface categoriesFor: #printOn:!*-in class package!public! !
!CushionSurface categoriesFor: #right!*-in class package!accessing!private! !
!CushionSurface categoriesFor: #right:!*-in class package!accessing!private! !
!CushionSurface categoriesFor: #setLeft:top:right:bottom:!*-in class package!private! !
!CushionSurface categoriesFor: #top!*-in class package!accessing!private! !
!CushionSurface categoriesFor: #top:!*-in class package!accessing!private! !

!CushionSurface class methodsFor!

left: x1 top: y1 right: x2 bottom: y2 
	^self basicNew 
		setLeft: x1
		top: y1
		right: x2
		bottom: y2!

new
	^self 
		left: 0
		top: 0
		right: 0
		bottom: 0! !
!CushionSurface class categoriesFor: #left:top:right:bottom:!*-in class package!public! !
!CushionSurface class categoriesFor: #new!*-in class package!public! !

AbstractCushionRenderStrategy guid: (GUID fromString: '{6632B1E0-DFC8-4D7C-AA16-FBDB659F22A8}')!
AbstractCushionRenderStrategy comment: ''!
!AbstractCushionRenderStrategy categoriesForClass!Unclassified! !
!AbstractCushionRenderStrategy methodsFor!

initialRenderState
	^CushionRenderState new! !
!AbstractCushionRenderStrategy categoriesFor: #initialRenderState!*-in class package!public! !

VanillaCushionRenderStrategy guid: (GUID fromString: '{97A45B25-74A0-48D9-8F63-3D1ECA6F2888}')!
VanillaCushionRenderStrategy comment: ''!
!VanillaCushionRenderStrategy categoriesForClass!Unclassified! !
!VanillaCushionRenderStrategy methodsFor!

render: node using: aRenderState 
	| surface basecolor |
	basecolor := renderer nodeColor: node object.
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
					lightIntensity := surface lightIntensityAt: ix @ iy.
					self setPixelAt: ix @ iy
						brush: (basecolor darkenBy: 255 
										- (self ambientLightIntensity + (0 max: self directLightIntensity * lightIntensity)))]]!

setPixelAt: aPoint brush: aColor 
	canvas fillRectangle: (aPoint extent: self renderStep @ self renderStep) brush: (Brush color: aColor)! !
!VanillaCushionRenderStrategy categoriesFor: #render:using:!*-in class package!public! !
!VanillaCushionRenderStrategy categoriesFor: #setPixelAt:brush:!*-in class package!public! !

!VanillaCushionRenderStrategy class methodsFor!

description
	^'Cushion - out of the box'! !
!VanillaCushionRenderStrategy class categoriesFor: #description!*-in class package!public! !

CushionRenderState guid: (GUID fromString: '{52D4A45E-6D84-4C35-B6BE-06311FDEE304}')!
CushionRenderState comment: ''!
!CushionRenderState categoriesForClass!Unclassified! !
!CushionRenderState methodsFor!

factor
	^factor!

height
	^height!

height: anObject 
	height := anObject!

newHeight: aNode 
	^aNode hasParent ifTrue: [self height * self factor] ifFalse: [self height]!

setHeight: aFloat1 factor: aFloat2 surface: aCushionSurface 
	height := aFloat1.
	factor := aFloat2.
	surface := aCushionSurface!

surface
	^surface!

update: aNode horizontalIn: aRectangle 
	aNode hasParent 
		ifTrue: 
			[^self class 
				height: (self newHeight: aNode)
				factor: self factor
				surface: (self surface 
						addRidgeVerticalMin: aRectangle bottom
						max: aRectangle top
						height: (self newHeight: aNode))]ifFalse: 
			[^self class 
				height: (self newHeight: aNode)
				factor: self factor
				surface: self surface]!

update: aNode in: aRectangle 
	aNode hasParent 
		ifTrue: 
			[^self class 
				height: (self newHeight: aNode)
				factor: self factor
				surface: (self surface 
						addRidge:aRectangle
						height: (self newHeight: aNode))]
		ifFalse: 
			[^self class 
				height: (self newHeight: aNode)
				factor: self factor
				surface: self surface]!

update: aNode verticalIn: aRectangle 
	aNode hasParent 
		ifTrue: 
			[^self class 
				height: (self newHeight: aNode)
				factor: self factor
				surface: (self surface 
						addRidgeHorizontalMin: aRectangle left
						max: aRectangle right
						height: (self newHeight: aNode))]
		ifFalse: 
			[^self class 
				height: (self newHeight: aNode)
				factor: self factor
				surface: self surface 
						]! !
!CushionRenderState categoriesFor: #factor!*-in class package!accessing!private! !
!CushionRenderState categoriesFor: #height!*-in class package!accessing!private! !
!CushionRenderState categoriesFor: #height:!*-in class package!accessing!private! !
!CushionRenderState categoriesFor: #newHeight:!*-in class package!private! !
!CushionRenderState categoriesFor: #setHeight:factor:surface:!*-in class package!private! !
!CushionRenderState categoriesFor: #surface!*-in class package!accessing!private! !
!CushionRenderState categoriesFor: #update:horizontalIn:!*-in class package!public! !
!CushionRenderState categoriesFor: #update:in:!*-in class package!public! !
!CushionRenderState categoriesFor: #update:verticalIn:!*-in class package!public! !

!CushionRenderState class methodsFor!

defaultFactor
	^0.7!

defaultHeight
	^0.8!

height: aFloat1 factor: aFloat2 surface: aCushionSurface 
	^self new 
		setHeight: aFloat1
		factor: aFloat2
		surface: aCushionSurface!

new
	^super new 
		setHeight: self defaultHeight
		factor: self defaultFactor
		surface: CushionSurface new! !
!CushionRenderState class categoriesFor: #defaultFactor!*-in class package!private! !
!CushionRenderState class categoriesFor: #defaultHeight!*-in class package!private! !
!CushionRenderState class categoriesFor: #height:factor:surface:!*-in class package!public! !
!CushionRenderState class categoriesFor: #new!*-in class package!public! !

"Binary Globals"!

