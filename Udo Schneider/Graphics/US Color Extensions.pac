| package |
package := Package name: 'US Color Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Color Extensions 0.015$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.015'.


package methodNames
	add: #Color -> #asPoint3D;
	add: #Color -> #blueCopy;
	add: #Color -> #cyanCopy;
	add: #Color -> #greenCopy;
	add: #Color -> #magentaCopy;
	add: #Color -> #max:;
	add: #Color -> #min:;
	add: #Color -> #redCopy;
	add: #Color -> #value;
	add: #Color -> #wheel:;
	add: #Color -> #yellowCopy;
	add: #RGB -> #hue;
	add: #RGB -> #saturation;
	add: #RGB -> #value;
	add: 'Color class' -> #default16ColorPalette;
	add: 'Color class' -> #fromPoint3D:;
	add: 'Color class' -> #orange;
	add: 'Color class' -> #purple;
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

!Color methodsFor!

asPoint3D
^self red @ self green  @ self blue!

blueCopy
	^Color 
		red: 0
		green: 0
		blue: self blue!

cyanCopy
	^Color 
		red: 0
		green: self green
		blue: self blue!

greenCopy
	^Color 
		red: 0
		green: self green
		blue: 0!

magentaCopy
	^Color 
		red: self red
		green: 0
		blue: self blue!

max: aColor 
	| col1 col2 |
	col1 := self asRGB.
	col2 := self asRGB.
	^Color 
		red: (col1 red max: col2 red)
		green: (col1 green max: col2 green)
		blue: (col1 blue max: col2 blue)!

min: aColor 
	| col1 col2 |
	col1 := self asRGB.
	col2 := self asRGB.
	^Color 
		red: (col1 red min: col2 red)
		green: (col1 green min: col2 green)
		blue: (col1 blue min: col2 blue)!

redCopy
^Color red: self red green: 0 blue: 0!

value
	^self asRGB value!

wheel: anInteger 
	| hue saturation step |
	anInteger <= 1 ifTrue: [^Array with: self].
	hue := self hue.
	saturation := self saturation.
	step := 360 / anInteger.
	^(step to: 360 by: step) collect: 
			[:hueOffset | 
			Color 
				hue: (hue + hueOffset) \\ 360
				saturation:saturation
				value: 0.5]!

yellowCopy
	^Color 
		red: self red
		green: self green
		blue: 0! !
!Color categoriesFor: #asPoint3D!public! !
!Color categoriesFor: #blueCopy!public! !
!Color categoriesFor: #cyanCopy!public! !
!Color categoriesFor: #greenCopy!public! !
!Color categoriesFor: #magentaCopy!public! !
!Color categoriesFor: #max:!public! !
!Color categoriesFor: #min:!public! !
!Color categoriesFor: #redCopy!public! !
!Color categoriesFor: #value!public! !
!Color categoriesFor: #wheel:!public! !
!Color categoriesFor: #yellowCopy!public! !

!Color class methodsFor!

default16ColorPalette
	^##(#(16r000000 16r0000AA 16r00AA00 16r00AAAA 16rAA0000 16rAA00AA 16rAAAA00 16rAAAAAA 16r555555 16r0000FF 16r00FF00 16r00FFFF 16rFF0000 16rFF00FF 16rFFFF00 16rFFFFFF) 
		collect: [:each | (Color fromInteger: each) asRGB])!

fromPoint3D: aPiont3D
^self red: aPiont3D x green: aPiont3D y blue: aPiont3D z!

orange
	^Color 
		red: 255
		green: 192
		blue: 0!

purple
	^Color 
		red: 192
		green: 0
		blue: 255! !
!Color class categoriesFor: #default16ColorPalette!constants!public! !
!Color class categoriesFor: #fromPoint3D:!public! !
!Color class categoriesFor: #orange!*-not in class package!instance creation!public! !
!Color class categoriesFor: #purple!*-not in class package!instance creation!public! !

!RGB methodsFor!

hue
	| min max |
	min := (self red min: self green) min: self blue.
	max := (self red max: self green) max: self blue.
	max = min ifTrue: [^0].
	self red = max ifTrue: [^((0 + ((self green - self blue) / (max - min))) * 60) +360 \\360].
	self green = max ifTrue: [^((2 + ((self blue - self red) / (max - min))) * 60) +360 \\360].
	self blue = max ifTrue: [^((4 + ((self red - self green) / (max - min))) * 60) +360 \\360]!

saturation
	| min max |
	min := (self red min: self green) min: self blue.
	max := (self red max: self green) max: self blue.
^	max isZero ifFalse: [(max - min) / max] ifTrue: [0]!

value
	| min max |
	min := (self red min: self green) min: self blue.
	max := (self red max: self green) max: self blue.
	^max / 255! !
!RGB categoriesFor: #hue!public! !
!RGB categoriesFor: #saturation!public! !
!RGB categoriesFor: #value!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

