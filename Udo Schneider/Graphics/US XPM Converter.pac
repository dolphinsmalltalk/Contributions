| package |
package := Package name: 'US XPM Converter'.
package paxVersion: 1;
	basicComment: '$id: US XPM Converter 0.005$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.005'.


package methodNames
	add: #DIBSection -> #xpmString;
	add: #Image -> #xpmString;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Octree Quantizer';
	yourself).

package setManualPrerequisites: #(
	'US Octree Quantizer').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!DIBSection methodsFor!

xpmString
	"Convert an 8bit DIB into an XPM String
	
	Please note that this method only accepts 8bit DIBs with 64 colors max.
	If it is not 8 bits deep refer to superclass implementation whch takes care of it"
	| stream colors dsBm bmBits bmWidthBytes encoding |
	self depth = 8 ifFalse: [^super xpmString].
	encoding := '0123456789abzdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-'.
	stream := ReadWriteStream on: String new.
	stream
		nextPutAll: '/* XPM */';
		cr;
		nextPutAll: 'static char *Pixmap[] = {';
		cr;
		nextPutAll: '/* width height num_colors chars_per_pixel */';
		cr;
		nextPutAll: ('" <1d> <2d> 64 1",' expandMacrosWith: self extent x with: self extent y);
		cr;
		nextPutAll: '/* colors */';
		cr.
	colors := self getColors copyFrom: 1 to: 64.
	colors keysAndValuesDo: 
			[:index :color | 
			stream
				nextPutAll: ('"<1d> c #<2d>",' expandMacrosWith: (encoding at: index)
							with: ('00000' , (color asParameter printStringRadix: 16 showRadix: false) rightString: 6));
				cr].
	stream
		nextPutAll: '/* pixels */';
		cr.
	dsBm := self getDIBSECTION dsBm.
	bmBits := dsBm bmBits.
	bmWidthBytes := dsBm bmWidthBytes.
	(self getDIBSECTION dsBmih biHeight negative 
		ifFalse: [0 to: self extent y - 1]
		ifTrue: [self extent y - 1 to: 0 by: -1]) do: 
				[:y | 
				| lineOffset |
				lineOffset := y * bmWidthBytes.
				stream nextPut: $".
				0 to: bmWidthBytes
					do: 
						[:rowOffset | 
						| byte |
						byte := (bmBits byteAtOffset: lineOffset + rowOffset) \\ 64.
						stream nextPut: (encoding at: byte + 1)].
				stream
					nextPut: $";
					cr].
	stream
		nextPutAll: '};';
		cr.
	^stream contents! !
!DIBSection categoriesFor: #xpmString!public! !

!Image methodsFor!

xpmString
	| extent dib xpmString |
	extent := self extent.
	dib := DIBSection 
				width: extent x
				height: extent y
				depth: 32.
	self drawOn: dib canvas.
	xpmString :=  dib quantizeTo64Colors xpmString.
	dib free.
	^xpmString! !
!Image categoriesFor: #xpmString!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

