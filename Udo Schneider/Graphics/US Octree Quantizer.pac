| package |
package := Package name: 'US Octree Quantizer'.
package paxVersion: 1;
	basicComment: '$id: US Octree Quantizer 0.016$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Usage:
This package provides a possibility to quantize pictures (color reduction). Please note that currently the resulting picture uses the "nearest" color from the quantization and is not dithered.

Example:

icon := Icon fromId: ''PANIC.ico''.		"Inspect it: This is the original image"
icon quantizeTo256Colors.			"Inspect it: Reduced to 256 colors"
icon quantizeTo64Colors.			"Inspect it: Reduced to 64 colors"
icon quantizeTo16Colors.			"Inspect it: Reduced to 16 colors"

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.016'.


package classNames
	add: #Octree;
	add: #OctreeNode;
	add: #OctreeQuantizer;
	yourself.

package methodNames
	add: #Image -> #quantizeTo:depth:bits:colors:;
	add: #Image -> #quantizeTo16Colors;
	add: #Image -> #quantizeTo256Colors;
	add: #Image -> #quantizeTo64Colors;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Graphics Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Graphics Extensions').

package!

"Class Definitions"!

Object subclass: #Octree
	instanceVariableNames: 'leafCount reducibleNodes previousColor previousNode maxColorBits root palette paletteTable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #OctreeNode
	instanceVariableNames: 'leaf red green blue pixelCount nextReducible children paletteIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #OctreeQuantizer
	instanceVariableNames: 'octree maxColors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Image methodsFor!

quantizeTo: maxColors depth: depth bits: bits colors: colors 
	"Return an quantized DIBSection of self
	
	maxColors: # of colors the OctreeQuantizer should return
	depth: bitdepth of final dib
	bits: How many bits of each color component (RGB) should be taken into considration. Maximum is 8. However 6 leads to good and fast results
	colors: A collection of colors to be appended to the colors from the quantizer"

	| palette dib quantizedPalette canvas |
	maxColors < 8 ifTrue: [self error: 'Number of colors must be 8 or higher!!'].
	quantizedPalette := (OctreeQuantizer maxColors: maxColors maxBits: bits) quantizeImage: self.
	palette := quantizedPalette , colors , (Array new: 2 ** depth withAll: quantizedPalette last) 
				copyFrom: 1
				to: 2 ** depth.
	dib := DIBSection 
				width: self extent x
				height: self extent y
				depth: depth.
				canvas := dib canvas.
	dib
		
		setColors: palette.
	self 
		drawOn:  canvas
		at: Point zero
		extent: self extent.
	^dib!

quantizeTo16Colors
	^self quantizeTo: 16 depth: 4 bits: 6 colors: #()!

quantizeTo256Colors
	^self quantizeTo: 256 depth: 8 bits: 6 colors: #()!

quantizeTo64Colors
	^self quantizeTo: 64 depth: 8 bits: 6 colors: #()! !
!Image categoriesFor: #quantizeTo:depth:bits:colors:!public! !
!Image categoriesFor: #quantizeTo16Colors!public! !
!Image categoriesFor: #quantizeTo256Colors!public! !
!Image categoriesFor: #quantizeTo64Colors!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

Octree guid: (GUID fromString: '{D52D0007-93CB-44BD-B727-AB7CA366A93F}')!
Octree comment: ''!
!Octree categoriesForClass!Unclassified! !
!Octree methodsFor!

addInteger: anInteger 
	previousColor = anInteger 
		ifTrue: 
			[previousNode isNil 
				ifTrue: 
					[previousColor := anInteger.
					root 
						addInteger: anInteger
						colorBits: maxColorBits
						level: 0
						octree: self]
				ifFalse: [previousNode increment: anInteger]]
		ifFalse: 
			[previousColor := anInteger.
			root 
				addInteger: anInteger
				colorBits: maxColorBits
				level: 0
				octree: self]!

initialize
	leafCount := 0.
	reducibleNodes := Array new: 9.
	previousColor := 0.
	previousNode := nil!

leaves
^leafCount!

leaves: anInteger
leafCount := anInteger!

palletize: colorCount 
	[self leaves > colorCount] whileTrue: [self reduce].
	
	^root constructPalette: OrderedCollection new paletteIndex: 0!

reduce
	| index node |
	index := maxColorBits - 1.
	#USToDo.	"Is thie whileFalse or whileTrue"
	[index > 0 and: [(self reducibleNodesAt: index) notNil]] whileFalse: [index := index - 1].
	node := self reducibleNodesAt: index.
	self reducibleNodesAt: index put: node nextReducible.
	leafCount := leafCount - node reduce.
	previousNode := nil!

reducibleNodes
^reducibleNodes!

reducibleNodesAt: level
	^reducibleNodes at: level +1!

reducibleNodesAt: level put: octreeNode
	^reducibleNodes at: level + 1 put: octreeNode!

setBits: bits 
	maxColorBits := bits.
	root := OctreeNode 
				level: 0
				colorBits: maxColorBits
				octree: self!

trackPrevious: node
previousNode := node! !
!Octree categoriesFor: #addInteger:!public! !
!Octree categoriesFor: #initialize!public! !
!Octree categoriesFor: #leaves!public! !
!Octree categoriesFor: #leaves:!public! !
!Octree categoriesFor: #palletize:!public! !
!Octree categoriesFor: #reduce!public! !
!Octree categoriesFor: #reducibleNodes!public! !
!Octree categoriesFor: #reducibleNodesAt:!public! !
!Octree categoriesFor: #reducibleNodesAt:put:!public! !
!Octree categoriesFor: #setBits:!public! !
!Octree categoriesFor: #trackPrevious:!public! !

!Octree class methodsFor!

bits: bits
^self new setBits: bits
!

new
^super new initialize! !
!Octree class categoriesFor: #bits:!public! !
!Octree class categoriesFor: #new!public! !

OctreeNode guid: (GUID fromString: '{E5D0EE7A-599B-4236-9736-98046249BB30}')!
OctreeNode comment: ''!
!OctreeNode categoriesForClass!Unclassified! !
!OctreeNode methodsFor!

addInteger: anInteger colorBits: colorBits level: level octree: octree 
	| index child |
	leaf 
		ifTrue: 
			[self increment: anInteger.
			octree trackPrevious: self]
		ifFalse: 
			[| shifted |
			shifted := anInteger bitShift: level - 8.
			index := ((shifted bitAnd: 16r010000) bitShift: -14) + ((shifted bitAnd: 16r0100) bitShift: -7) 
						+ (shifted bitAnd: 16r1).
			child := self childrenAt: index.
			child := child 
				ifNil: 
					[
					self childrenAt: index put: (OctreeNode 
								level: level + 1
								colorBits: colorBits
								octree: octree)].
			child 
				addInteger: anInteger
				colorBits: colorBits
				level: level + 1
				octree: octree]!

blue
	^blue!

children 
^children!

children: anArray 
	children := anArray!

childrenAt: anInteger 
	^children at: anInteger +1!

childrenAt: anInteger put: anOctreeNode 
	^children at: anInteger +1 put: anOctreeNode!

childrenDo: aMonadicValuable 
	children do: aMonadicValuable!

constructPalette: palette paletteIndex: anInteger 
	leaf 
		ifTrue: 
			[paletteIndex := anInteger + 1.
			palette add: (Color 
						red: red // pixelCount
						green: green // pixelCount
						blue: blue // pixelCount)]
		ifFalse: 
			[self childrenDo: [:eachChild | eachChild ifNotNil: [:value | value constructPalette: palette paletteIndex: anInteger]]]. ^palette!

green
	^green!

increment: anInteger 
	pixelCount := pixelCount + 1.
	red := red + ((anInteger bitAnd: 16rFF0000) >> 16).
	green := green + ((anInteger bitAnd: 16rFF00) >> 8).
	blue := blue + (anInteger bitAnd: 16rFF)!

maskAt: level
	^#(16r80 16r40 16r20 16r10 16r8 16r4 16r2 16r1) at: level +1!

nextReducible
	^nextReducible!

nextReducible: anObject
	nextReducible := anObject!

pixelCount
	^pixelCount!

red
	^red!

reduce
	| childrenC |

	childrenC := 0.
	red := 0.
	green := 0.
	blue := 0.
	self children do: 
			[:eachChild | 
			eachChild 
				ifNotNil: 
					[:value | 
					red := red + value red.
					green := green + value green.
					blue := blue + value blue.
					pixelCount := pixelCount + value pixelCount.
					childrenC := childrenC + 1]].
	self children: (Array new: 8).
	leaf := true.
	^childrenC - 1!

setLevel: level colorBits: colorBits octree: octree 
	leaf := level = colorBits.
	red := 0.
	green := 0.
	blue := 0.
	pixelCount := 0.
	leaf 
		ifTrue: 
			[octree leaves: octree leaves + 1.
			nextReducible := nil.
			children := nil]
		ifFalse: 
			[nextReducible := octree reducibleNodesAt: level.
			octree reducibleNodesAt: level put: self.
			children := Array new: 8]! !
!OctreeNode categoriesFor: #addInteger:colorBits:level:octree:!public! !
!OctreeNode categoriesFor: #blue!public! !
!OctreeNode categoriesFor: #children!public! !
!OctreeNode categoriesFor: #children:!public! !
!OctreeNode categoriesFor: #childrenAt:!public! !
!OctreeNode categoriesFor: #childrenAt:put:!public! !
!OctreeNode categoriesFor: #childrenDo:!public! !
!OctreeNode categoriesFor: #constructPalette:paletteIndex:!public! !
!OctreeNode categoriesFor: #green!public! !
!OctreeNode categoriesFor: #increment:!public! !
!OctreeNode categoriesFor: #maskAt:!public! !
!OctreeNode categoriesFor: #nextReducible!public! !
!OctreeNode categoriesFor: #nextReducible:!public! !
!OctreeNode categoriesFor: #pixelCount!public! !
!OctreeNode categoriesFor: #red!public! !
!OctreeNode categoriesFor: #reduce!public! !
!OctreeNode categoriesFor: #setLevel:colorBits:octree:!public! !

!OctreeNode class methodsFor!

level: level
				colorBits: colorBits
				octree: octree
				^self new setLevel: level
				colorBits: colorBits
				octree: octree! !
!OctreeNode class categoriesFor: #level:colorBits:octree:!public! !

OctreeQuantizer guid: (GUID fromString: '{80823C20-B759-4550-96D7-138FF7DE7FD2}')!
OctreeQuantizer comment: ''!
!OctreeQuantizer categoriesForClass!Unclassified! !
!OctreeQuantizer methodsFor!

quantizeColor: aColor 
	self quantizeInteger: aColor asRGB asParameter!

quantizedPalette
	^(octree palletize: maxColors)  asSortedCollection: [ :x :y | x luminance <= y luminance ].!

quantizeImage: aDIBSection 
	aDIBSection pixelValueDo: [:anInteger | self quantizeInteger: anInteger].
	^self quantizedPalette!

quantizeInteger: anInteger 
	octree addInteger: anInteger!

setMaxColors: anInteger maxBits: maxBits 
	maxColors := anInteger.
	octree := Octree bits: maxBits! !
!OctreeQuantizer categoriesFor: #quantizeColor:!public! !
!OctreeQuantizer categoriesFor: #quantizedPalette!public! !
!OctreeQuantizer categoriesFor: #quantizeImage:!public! !
!OctreeQuantizer categoriesFor: #quantizeInteger:!public! !
!OctreeQuantizer categoriesFor: #setMaxColors:maxBits:!private! !

!OctreeQuantizer class methodsFor!

maxColors: maxColors maxBits: maxBits 
	^(self basicNew )
		setMaxColors: maxColors maxBits: maxBits;
		yourself!

new
^self maxColors: 256 maxBits: 8! !
!OctreeQuantizer class categoriesFor: #maxColors:maxBits:!public! !
!OctreeQuantizer class categoriesFor: #new!public! !

"Binary Globals"!

