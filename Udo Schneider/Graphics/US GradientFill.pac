| package |
package := Package name: 'US GradientFill'.
package paxVersion: 1;
	basicComment: '$id: US GradientFill 1.009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Provides a Dolphin canvas around the GDI gradient Fill Methods

Usage
	Look for examples here:
	Canvas gradientFillComplexExample.
	Canvas gradientFillVerticalRectangleExample.
	Canvas gradientFillHorizontalRectangleExample.
	See here for further information: http://msdn.microsoft.com/library/default.asp?url=/library/en-us/gdi/bitmaps_8oa4.asp

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.009'.


package methodNames
	add: #Canvas -> #fillComplexGradient:;
	add: #Canvas -> #fillComplexGradient:meshes:mode:;
	add: #Canvas -> #fillRectangle:blendFunction:steps:verticalGradient:;
	add: #Canvas -> #fillRectangle:blendFunction:verticalGradient:;
	add: #Canvas -> #fillRectangleHorizontal:blendFunction:;
	add: #Canvas -> #fillRectangleHorizontal:blendFunction:steps:;
	add: #Canvas -> #fillRectangleVertical:blendFunction:;
	add: #Canvas -> #fillRectangleVertical:blendFunction:steps:;
	add: #TRIVERTEX -> #color;
	add: #TRIVERTEX -> #position;
	add: 'BLENDFUNCTION class' -> #blendRainbow;
	add: 'BLENDFUNCTION class' -> #blendRainbowAnticlockwiseFrom:to:;
	add: 'BLENDFUNCTION class' -> #blendRainbowClockwiseFrom:to:;
	add: 'BLENDFUNCTION class' -> #blendRainbowInverse;
	add: 'BLENDFUNCTION class' -> #blendRainbowLongFrom:to:;
	add: 'BLENDFUNCTION class' -> #blendRainbowShortFrom:to:;
	add: 'Canvas class' -> #gradientFillComplexExample;
	add: 'Canvas class' -> #gradientFillHorizontalRectangleExample;
	add: 'Canvas class' -> #gradientFillVerticalRectangleExample;
	add: 'GRADIENT_TRIANGLE class' -> #vertex1:vertex2:vertex3:;
	add: 'GRADIENT_TRIANGLE class' -> #vertexs:;
	add: 'TRIVERTEX class' -> #position:color:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Image\Dolphin Image Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!BLENDFUNCTION class methodsFor!

blendRainbow
	^##(
	[:value | 
	Color 
		hue: value * 360
		saturation: 1
		value: 1])!

blendRainbowAnticlockwiseFrom: fromColor to: toColor 
	| hue1 hue2 |
	hue1 := fromColor hue \\ 360.
	hue2 := toColor hue \\ 360.
	hue2 > hue1 ifTrue: [hue1 := hue1 + 360].
	^
	[:value | 
	Color 
		hue: (hue1 - ((hue1 - hue2) * value)) \\ 360
		saturation: fromColor saturation + ((toColor saturation - fromColor saturation) * value)
		value: fromColor value + ((toColor value - fromColor value) * value)]!

blendRainbowClockwiseFrom: fromColor to: toColor 
	| hue1 hue2 |
	hue1 := fromColor hue \\ 360.
	hue2 := toColor hue \\ 360.
	hue2 < hue1 ifTrue: [hue2 := hue2 + 360].
	^
	[:value | 
	Color 
		hue: (hue1 + ((hue2 - hue1) * value)) \\ 360
		saturation: fromColor saturation + ((toColor saturation - fromColor saturation) * value)
		value: fromColor value + ((toColor value - fromColor value) * value)]!

blendRainbowInverse
	^##(
	[:value | 
	Color 
		hue: 360 - (value * 360)
		saturation: 1
		value: 1])!

blendRainbowLongFrom: fromColor to: toColor 
	| hue1 hue2 |
	hue1 := fromColor hue \\ 360.
	hue2 := toColor hue \\ 360.
	^(360 + hue2 - hue1) \\ 360 >= 180 
		ifTrue: [self blendRainbowClockwiseFrom: fromColor to: toColor]
		ifFalse: [self blendRainbowAnticlockwiseFrom: fromColor to: toColor]!

blendRainbowShortFrom: fromColor to: toColor 
	| hue1 hue2 |
	hue1 := fromColor hue \\ 360.
	hue2 := toColor hue \\ 360.
	^(360 + hue2 - hue1) \\ 360 < 180 
		ifTrue: [self blendRainbowClockwiseFrom: fromColor to: toColor]
		ifFalse: [self blendRainbowAnticlockwiseFrom: fromColor to: toColor]! !
!BLENDFUNCTION class categoriesFor: #blendRainbow!public! !
!BLENDFUNCTION class categoriesFor: #blendRainbowAnticlockwiseFrom:to:!public! !
!BLENDFUNCTION class categoriesFor: #blendRainbowClockwiseFrom:to:!public! !
!BLENDFUNCTION class categoriesFor: #blendRainbowInverse!public! !
!BLENDFUNCTION class categoriesFor: #blendRainbowLongFrom:to:!public! !
!BLENDFUNCTION class categoriesFor: #blendRainbowShortFrom:to:!public! !

!Canvas methodsFor!

fillComplexGradient: aCollection 
	"aCollection elements are collections containing three Associations. The Associations' key is a Point and the value the color. E.g.:
	#(
		#(
			100@0 -> Color red
			0@100 -> Color green
			200@100 -> Color blue
		)
		#(
			100@0 -> Color red
			200@0 -> Color yellow
			200@100 -> Color blue
		)
	)"

	| vertexs meshes |
	vertexs := OrderedCollection new.
	meshes := aCollection collect: 
					[:eachTria | 
					GRADIENT_TRIANGLE 
						vertexs: ((1 to: 3) collect: 
									[:pointIndex | 
									| currentVertex |
									currentVertex := TRIVERTEX position: (eachTria at: pointIndex) key
												color: (eachTria at: pointIndex) value.
									(vertexs includes: currentVertex) ifFalse: [vertexs add: currentVertex].
									(vertexs indexOf: currentVertex) - 1])].
	^self 
		fillComplexGradient: (StructureArray withAll: vertexs elementClass: TRIVERTEX)
		meshes: (StructureArray withAll: meshes elementClass: GRADIENT_TRIANGLE)
		mode: GRADIENT_FILL_TRIANGLE!

fillComplexGradient: vertexs meshes: meshes mode: dwMode 
	^Msimg32Library default 
		gradientFill: self asParameter
		pVertex: vertexs
		dwNumVertex: vertexs size
		pMesh: meshes
		dwNumMesh: meshes size
		dwMode: dwMode!

fillRectangle: aRectangle blendFunction: blendFunction steps: steps verticalGradient: aBoolean 
	aBoolean 
		ifTrue: 
			[self 
				fillRectangleVertical: aRectangle
				blendFunction: blendFunction
				steps: steps]
		ifFalse: 
			[self 
				fillRectangleHorizontal: aRectangle
				blendFunction: blendFunction
				steps: steps]!

fillRectangle: aRectangle blendFunction: blendFunction verticalGradient: aBoolean 
	aBoolean 
		ifTrue: [self fillRectangleVertical: aRectangle blendFunction: blendFunction]
		ifFalse: [self fillRectangleHorizontal: aRectangle blendFunction: blendFunction]!

fillRectangleHorizontal: aRectangle blendFunction: blendFunction 
	^self 
		fillRectangleHorizontal: aRectangle
		blendFunction: blendFunction
		steps: aRectangle width!

fillRectangleHorizontal: aRectangle blendFunction: blendFunction steps: steps 
	| stepSize |
	stepSize := aRectangle width / (steps min: aRectangle width).
	aRectangle left to: aRectangle right
		by: stepSize
		do: 
			[:apos | 
			| relativePosition currentStrip |
			relativePosition := (apos - aRectangle left) / aRectangle width.
			currentStrip := apos @ aRectangle top extent: stepSize @ aRectangle height.
			self fillRectangle: currentStrip truncated
				brush: (Brush color: (blendFunction value: relativePosition))]!

fillRectangleVertical: aRectangle blendFunction: blendFunction 
	^self 
		fillRectangleVertical: aRectangle
		blendFunction: blendFunction
		steps: aRectangle height!

fillRectangleVertical: aRectangle blendFunction: blendFunction steps: steps 
	| stepSize |
	stepSize := aRectangle height / (steps min: aRectangle height).
	aRectangle top to: aRectangle bottom
		by: stepSize
		do: 
			[:apos | 
			| relativePosition currentStrip |
			relativePosition := (apos - aRectangle top) / aRectangle height.
			currentStrip := aRectangle left @ apos extent: aRectangle right @ stepSize.
			self fillRectangle: currentStrip truncated
				brush: (Brush color: (blendFunction value: relativePosition))]! !
!Canvas categoriesFor: #fillComplexGradient:!public! !
!Canvas categoriesFor: #fillComplexGradient:meshes:mode:!private! !
!Canvas categoriesFor: #fillRectangle:blendFunction:steps:verticalGradient:!public! !
!Canvas categoriesFor: #fillRectangle:blendFunction:verticalGradient:!public! !
!Canvas categoriesFor: #fillRectangleHorizontal:blendFunction:!private! !
!Canvas categoriesFor: #fillRectangleHorizontal:blendFunction:steps:!private! !
!Canvas categoriesFor: #fillRectangleVertical:blendFunction:!private! !
!Canvas categoriesFor: #fillRectangleVertical:blendFunction:steps:!private! !

!Canvas class methodsFor!

gradientFillComplexExample
	"
	self gradientFillComplexExample
	"

	| dib p1 p2 p3 p4 p5 fills |
	dib := DIBSection 
				width: 160
				height: 120
				depth: 32.
	p1 := 0 @ 0 -> Color red.
	p2 := dib extent x @ 0 -> Color yellow.
	p3 := dib extent -> Color green.
	p4 := 0 @ dib extent y -> Color blue.
	p5 := dib extent / 2 -> Color cyan.
	fills := Array 
				with: (Array 
						with: p1
						with: p2
						with: p5)
				with: (Array 
						with: p2
						with: p3
						with: p5)
				with: (Array 
						with: p3
						with: p4
						with: p5)
				with: (Array 
						with: p4
						with: p1
						with: p5).
	dib canvas fillComplexGradient: fills.
	ImagePresenter showOn: dib!

gradientFillHorizontalRectangleExample
	"
	self gradientFillHorizontalRectangleExample
	"

	| dib  |
	dib := DIBSection 
				width: 160
				height: 120
				depth: 32.
	dib canvas 
		fillRectangle: (0 @ 0 extent: dib extent)
		startColor: Color green
		endColor: Color cyan
		verticalGradient: false.
	ImagePresenter showOn: dib!

gradientFillVerticalRectangleExample
	"
	self gradientFillVerticalRectangleExample
	"

	| dib|
	dib := DIBSection 
				width: 160
				height: 120
				depth: 32.
	dib canvas 
		fillRectangle: (0 @ 0 extent: dib extent)
		startColor: Color blue
		endColor: Color yellow
		verticalGradient: true.
	ImagePresenter showOn: dib! !
!Canvas class categoriesFor: #gradientFillComplexExample!*-not in class package!example!must strip!public! !
!Canvas class categoriesFor: #gradientFillHorizontalRectangleExample!*-not in class package!example!must strip!public! !
!Canvas class categoriesFor: #gradientFillVerticalRectangleExample!*-not in class package!example!must strip!public! !

!GRADIENT_TRIANGLE class methodsFor!

vertex1: vertex1 vertex2: vertex2 vertex3: vertex3 
	^(self new)
		Vertex1: vertex1;
		Vertex2: vertex2;
		Vertex3: vertex3;
		yourself!

vertexs: anArray
^self vertex1: (anArray at: 1) vertex2: (anArray at: 2)vertex3: (anArray at: 3)! !
!GRADIENT_TRIANGLE class categoriesFor: #vertex1:vertex2:vertex3:!public! !
!GRADIENT_TRIANGLE class categoriesFor: #vertexs:!public! !

!TRIVERTEX methodsFor!

color
^Color red: self red / 16rFF green: self green / 16rFF blue: self blue / 16rFF
!

position
	^self x@self y! !
!TRIVERTEX categoriesFor: #color!public! !
!TRIVERTEX categoriesFor: #position!public! !

!TRIVERTEX class methodsFor!

position: aPoint color: aColor
	^self new position: aPoint; color: aColor; yourself.! !
!TRIVERTEX class categoriesFor: #position:color:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

