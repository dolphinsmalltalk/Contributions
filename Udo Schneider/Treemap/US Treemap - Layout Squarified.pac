| package |
package := Package name: 'US Treemap - Layout Squarified'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Layout Squarified 0.019$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.019'.


package classNames
	add: #OrderedSquarifiedTreeMapStrategy;
	add: #SortedSquarifiedTreeMapStrategy;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'US Treemap - Layout Abstract';
	add: 'US Treemap - Layout Slice and Dice';
	yourself).

package!

"Class Definitions"!

AbstractTreemapLayoutStrategy subclass: #OrderedSquarifiedTreeMapStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OrderedSquarifiedTreeMapStrategy subclass: #SortedSquarifiedTreeMapStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

OrderedSquarifiedTreeMapStrategy guid: (GUID fromString: '{B13E8D0F-CA7F-4317-BD1F-EAB9918FD016}')!
OrderedSquarifiedTreeMapStrategy comment: ''!
!OrderedSquarifiedTreeMapStrategy categoriesForClass!Unclassified! !
!OrderedSquarifiedTreeMapStrategy methodsFor!

aspectBig: big small: small a: a b: b 
	| x |
	x := big * b / (small * (a / b)).
^x<1 ifTrue: [1/x] ifFalse: [x]!

getChildren: aNode 
	^aNode childrenWithExtent!

layout: children horizontallyIn: aRectangle 
	| x y w h total a b mid q |
	#USToDo.	"the ^self is strange ..."
	children size = 2 
		ifTrue: 
			[SliceAndDiceLayoutStrategy new layout: children horizontallyIn: aRectangle.
			^self].
	x := aRectangle left.
	y := aRectangle top.
	w := aRectangle width.
	h := aRectangle height.
	total := children sum.
	a := children first size / total.
	b := a.
	mid := 1.
	
	[mid < children size and: 
			[q := (children at: mid) size / total.
			(self 
				aspectBig: w
				small: h
				a: a
				b: b + q) > (self 
							aspectBig: w
							small: h
							a: a
							b: b)]] 
			whileTrue: 
				[mid := mid + 1.
				b := b + q].
	SliceAndDiceLayoutStrategy new layout: (children copyFrom: 1 to: mid)
		verticallyIn: (aRectangle topLeft extent: (w * b) @ h).
	self layout: (children copyFrom: mid + 1) in: ((x + (w * b)) @ y extent: (w * (1 - b)) @ h)!

layout: children in: aRectangle 
children isEmpty ifTrue: [^self].
	children size = 1 
		ifTrue: [children first setLayoutRectangle: aRectangle].
	aRectangle width > aRectangle height 
				ifTrue: [self layout: children horizontallyIn: aRectangle]
				ifFalse: [self layout: children verticallyIn: aRectangle]!

layout: node using: renderSettings 
	| newRenderSettings children |

	children := self getChildren: node.
	children notEmpty ifTrue: [self layout: children in: node layoutRectangle].
	children do: 
			[:each | 
			newRenderSettings := renderSettings update: each in: each layoutRectangle.
			renderStrategy renderNode: each using: newRenderSettings.
			each isLeafNode ifFalse: [self layout: each using: newRenderSettings]]!

layout: children verticallyIn: aRectangle 
	| x y w h total a b mid q |
	#USToDo.	"the ^self is strange ..."
	children size = 2 
		ifTrue: 
			[SliceAndDiceLayoutStrategy new layout: children horizontallyIn: aRectangle.
			^self].
	x := aRectangle left.
	y := aRectangle top.
	w := aRectangle width.
	h := aRectangle height.
	total := children sum.
	a := children first size / total.
	b := a.
	mid := 1.
	
	[mid < children size and: 
			[q := (children at: mid) size / total.
			(self 
				aspectBig: h
				small: w
				a: a
				b: b + q) > (self 
							aspectBig: h
							small: w
							a: a
							b: b)]] 
			whileTrue: 
				[mid := mid + 1.
				b := b + q].
	SliceAndDiceLayoutStrategy new layout: (children copyFrom: 1 to: mid)
		horizontallyIn: (x @ (y + (h * (1 - b))) extent: w @ (h * b)).
	self layout: (children copyFrom: mid + 1) in: (x @ y extent: w @ (h * (1 - b)))! !
!OrderedSquarifiedTreeMapStrategy categoriesFor: #aspectBig:small:a:b:!*-in class package!private! !
!OrderedSquarifiedTreeMapStrategy categoriesFor: #getChildren:!*-in class package!private! !
!OrderedSquarifiedTreeMapStrategy categoriesFor: #layout:horizontallyIn:!*-in class package!private! !
!OrderedSquarifiedTreeMapStrategy categoriesFor: #layout:in:!*-in class package!private! !
!OrderedSquarifiedTreeMapStrategy categoriesFor: #layout:using:!*-in class package!private! !
!OrderedSquarifiedTreeMapStrategy categoriesFor: #layout:verticallyIn:!*-in class package!private! !

!OrderedSquarifiedTreeMapStrategy class methodsFor!

description
	^'Squarified - Ordered'! !
!OrderedSquarifiedTreeMapStrategy class categoriesFor: #description!*-in class package!public! !

SortedSquarifiedTreeMapStrategy guid: (GUID fromString: '{DB054BD9-373F-47D6-98AF-4E4F6AB3E643}')!
SortedSquarifiedTreeMapStrategy comment: ''!
!SortedSquarifiedTreeMapStrategy categoriesForClass!Unclassified! !
!SortedSquarifiedTreeMapStrategy methodsFor!

getChildren: aNode 
	^(super getChildren: aNode ) asSortedCollection: [:a :b | a size > b size]! !
!SortedSquarifiedTreeMapStrategy categoriesFor: #getChildren:!*-in class package!private! !

!SortedSquarifiedTreeMapStrategy class methodsFor!

description
^'Squarified - Sorted'! !
!SortedSquarifiedTreeMapStrategy class categoriesFor: #description!*-in class package!public! !

"Binary Globals"!

