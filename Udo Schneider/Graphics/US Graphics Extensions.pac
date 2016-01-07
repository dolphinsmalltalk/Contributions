| package |
package := Package name: 'US Graphics Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Graphics Extensions 0.022$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This package provides some (non-GDI/GDI+ specific) graphical additions.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.022'.


package methodNames
	add: #Bitmap -> #pixelColorsDo:;
	add: #Canvas -> #centerText:in:;
	add: #Canvas -> #smoothedPolygon:;
	add: #Canvas -> #smoothedPolygon:factor:;
	add: #Image -> #pixelValueDo:;
	add: #Number -> #gamma:;
	add: #Number -> #xaraBias:;
	add: #Number -> #xaraGain:;
	add: #Number -> #xaraMapping;
	add: #SequenceableCollection -> #calculatePointOnCatmullRomSpline:;
	add: #SequenceableCollection -> #smoothedPolygonPoints;
	add: #SequenceableCollection -> #smoothedPolygonPointsFactor:;
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


"Global Aliases"!


"Loose Methods"!

!Bitmap methodsFor!

pixelColorsDo: aBlock 
	| canvas |
	canvas := self canvas.
	0 to: self extent y - 1
		do: [:y | 0 to: self extent x - 1 do: [:x | aBlock value: (canvas pixelAt: x @ y)]]! !
!Bitmap categoriesFor: #pixelColorsDo:!public! !

!Canvas methodsFor!

centerText: string in: aRectangle 
	self text: string at: (aRectangle  origin +((aRectangle extent - (self textExtent: string)) / 2) )truncated!

smoothedPolygon: points 
	^self smoothedPolygon: points factor: 0.5!

smoothedPolygon: points factor: smootingFactor 
	^self polyBezier: ((points smoothedPolygonPointsFactor: smootingFactor) collect: [:each | each truncated])! !
!Canvas categoriesFor: #centerText:in:!operations!public! !
!Canvas categoriesFor: #smoothedPolygon:!public! !
!Canvas categoriesFor: #smoothedPolygon:factor:!public! !

!Image methodsFor!

pixelValueDo: aBlock 
	"Calls the aBlock with an Integer 32/24 bit Integer value (ARGB/RGB) of each pixel in the dib"

	"Ensure that dib points to a 32bit dib"

	| dib dsBm bmBits bmWidthBytes |
	((self isKindOf: DIBSection) and: [self depth = 32]) 
		ifTrue: [dib := self]
		ifFalse: 
			[dib := DIBSection 
						width: self extent x
						height: self extent y
						depth: 32.
			self drawOn: dib canvas].
	dsBm := dib getDIBSECTION dsBm.
	bmBits := dsBm bmBits.
	bmWidthBytes := dsBm bmWidthBytes.
	(dib getDIBSECTION dsBmih biHeight negative 
		ifFalse: [0 to: dib extent y - 1]
		ifTrue: [dib extent y - 1 to: 0 by: -1]) do: 
				[:y | 
				| lineOffset |
				lineOffset := y * bmWidthBytes.
				lineOffset to: lineOffset + bmWidthBytes - 1
					by: 4
					do: 
						[:rowOffset | 
						| colorInteger |
						colorInteger := bmBits dwordAtOffset: rowOffset.
						aBlock value: colorInteger]].
	dib = self ifFalse: [dib free]! !
!Image categoriesFor: #pixelValueDo:!public! !

!Number methodsFor!

gamma: aFloat
  ^self raisedTo: aFloat!

xaraBias: bias 
	"XaraX bias function as defined in http://www.xara.com/support/docs/webformat/spec/XARFormatDocument.pdf"

	^self / ((1 / bias - 2) * (1 - self) + 1)!

xaraGain: gain 
	"XaraX gain function as defined in http://www.xara.com/support/docs/webformat/spec/XARFormatDocument.pdf"

	^self < 0.5 
		ifTrue: [self / ((1 / gain - 2) * (1 - (2 * self)) + 1)]
		ifFalse: [((1 / gain - 2) * (1 - (2 * self)) - self) / ((1 / gain - 2) * (1 - (2 * self)) - 1)]!

xaraMapping
	"XaraX Mapping function as defined in http://www.xara.com/support/docs/webformat/spec/XARFormatDocument.pdf"

	^(self + 1) * 0.49999 + 0.00001! !
!Number categoriesFor: #gamma:!public! !
!Number categoriesFor: #xaraBias:!public! !
!Number categoriesFor: #xaraGain:!public! !
!Number categoriesFor: #xaraMapping!public! !

!SequenceableCollection methodsFor!

calculatePointOnCatmullRomSpline: t 
	| p0 p1 p2 p3 |
	p0 := self at: 1.
	p1 := self at: 2.
	p2 := self at: 3.
	p3 := self at: 4.
	^0.5 
		* (2.0 * p1 + ((p0 negated + p2) * t) 
				+ ((2.0 * p0 - (5.0 * p1) + (4 * p2) - p3) * (t raisedToInteger: 2)) 
					+ ((p0 negated + (3.0 * p1) - (3.0 * p2) + p3) * (t raisedToInteger: 3)))!

smoothedPolygonPoints
	^self smoothedPolygonPointsFactor: 0.5!

smoothedPolygonPointsFactor: smootingFactor 
	"Polygon Smoothing according to http://www.antigrain.com/research/bezier_interpolation/index.html"

	| beziers size |
	size := self size.
	beziers := ReadWriteStream on: Array new.
	beziers nextPut: self first.
	1 to: size
		do: 
			[:index | 
			| p0 p1 p2 p3 c1 c2 c3 len1 len2 len3 k1 k2 m1 m2 ctrl1 ctrl2 |
			p0 := self at: (index - 1 wrapArround: size).
			p1 := self at: (index wrapArround: size).
			p2 := self at: (index + 1 wrapArround: size).
			p3 := self at: (index + 2 wrapArround: size).
			c1 := (p0 + p1) / 2.
			c2 := (p1 + p2) / 2.
			c3 := (p2 + p3) / 2.
			len1 := p0 dist: p1.
			len2 := p1 dist: p2.
			len3 := p2 dist: p3.
			k1 := len1 / (len1 + len2).
			k2 := len2 / (len2 + len3).
			m1 := c1 + ((c2 - c1) * k1).
			m2 := c2 + ((c3 - c2) * k2).
			ctrl1 :=  ((c2 - m1) * smootingFactor) + p1 .
			ctrl2 :=  ((c2 - m2) * smootingFactor) + p2.
			beziers
				nextPut: ctrl1;
				nextPut: ctrl2;
				nextPut: p2].
	^beziers contents! !
!SequenceableCollection categoriesFor: #calculatePointOnCatmullRomSpline:!public! !
!SequenceableCollection categoriesFor: #smoothedPolygonPoints!public! !
!SequenceableCollection categoriesFor: #smoothedPolygonPointsFactor:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

