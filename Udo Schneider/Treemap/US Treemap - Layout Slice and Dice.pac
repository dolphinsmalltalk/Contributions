| package |
package := Package name: 'US Treemap - Layout Slice and Dice'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Layout Slice and Dice 0.019$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.019'.


package classNames
	add: #SliceAndDiceLayoutStrategy;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'US Treemap - Layout Abstract';
	yourself).

package!

"Class Definitions"!

AbstractTreemapLayoutStrategy subclass: #SliceAndDiceLayoutStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SliceAndDiceLayoutStrategy guid: (GUID fromString: '{4214B749-32D7-477A-AF69-FDBF89F5BD86}')!
SliceAndDiceLayoutStrategy comment: 'AbstractTreemapLayoutStrategy implements the oldest Treemap Layout Strategy called "Slice and Dice".

The implementation is based on "Tree visualization with treemaps: a 2-d space-filling approach" by Shneiderman, B. (March 1991, ACM Transactions on Graphics, vol. 11, 1 (Jan. 1992) 92-99, HCIL-91-03 , CS-TR-2645 , CAR-TR-548). See http://www.cs.umd.edu/local-cgi-bin/hcil/rr.pl?number=91-03 for more information.
'!
!SliceAndDiceLayoutStrategy categoriesForClass!Unclassified! !
!SliceAndDiceLayoutStrategy methodsFor!

layout: treemapNodes horizontallyIn: aRectangle 
	| rectangleWidth nodeRectangle |
	nodeRectangle := aRectangle copy.
	rectangleWidth := nodeRectangle width / treemapNodes sum.
	treemapNodes do: 
			[:eachNode | 
			nodeRectangle right: nodeRectangle left + (eachNode  size * rectangleWidth).
			eachNode  setLayoutRectangle: nodeRectangle copy.
			nodeRectangle left: nodeRectangle right]!

layout: aTreemapNode horizontallyUsing: aTreemapRenderState 
	| newRenderSettings treemapNodes |
	treemapNodes := aTreemapNode childrenWithExtent.
	treemapNodes notEmpty ifTrue: [self layout: treemapNodes horizontallyIn: aTreemapNode layoutRectangle].
	treemapNodes do: 
			[:eachChildNode | 
			newRenderSettings := aTreemapRenderState update: eachChildNode
						horizontalIn: eachChildNode layoutRectangle.
			renderStrategy renderNode: eachChildNode using: newRenderSettings.
			eachChildNode isLeafNode 
				ifFalse: [self layout: eachChildNode verticallyUsing: newRenderSettings]]!

layout: aTreemapNode using: aTreemapRenderState 
	"Initial test to determine whether to layout the tree horizontally or vertically.
	This is just done once - afterwards the direction changes back and forth for every child node"

	aTreemapNode layoutRectangle width >= aTreemapNode layoutRectangle height 
		ifTrue: 	[self layout: aTreemapNode horizontallyUsing: aTreemapRenderState]
	ifFalse: [self layout: aTreemapNode verticallyUsing: aTreemapRenderState]
!

layout: treemapNodes verticallyIn: aRectangle 
	| rectangleHeight nodeRectangle |
	nodeRectangle := aRectangle copy.
	rectangleHeight  := nodeRectangle height / treemapNodes sum.
	treemapNodes do: 
			[:eachNode | 
			nodeRectangle bottom: nodeRectangle top + (eachNode size * rectangleHeight ).
			eachNode setLayoutRectangle: nodeRectangle copy.
			nodeRectangle top: nodeRectangle bottom]!

layout: aTreemapNode verticallyUsing: aTreemapRenderState 
	| newRenderSettings treemapNodes |

	treemapNodes := aTreemapNode childrenWithExtent.
	treemapNodes notEmpty ifTrue: [self layout: treemapNodes verticallyIn: aTreemapNode layoutRectangle].
	treemapNodes do: 
			[:eachChildNode | 
			newRenderSettings := aTreemapRenderState update: eachChildNode
						verticalIn: eachChildNode layoutRectangle.
			renderStrategy renderNode: eachChildNode using: newRenderSettings.
			eachChildNode isLeafNode 
				ifFalse: [self layout: eachChildNode horizontallyUsing: newRenderSettings]]! !
!SliceAndDiceLayoutStrategy categoriesFor: #layout:horizontallyIn:!*-in class package!private! !
!SliceAndDiceLayoutStrategy categoriesFor: #layout:horizontallyUsing:!*-in class package!private! !
!SliceAndDiceLayoutStrategy categoriesFor: #layout:using:!*-in class package!private! !
!SliceAndDiceLayoutStrategy categoriesFor: #layout:verticallyIn:!*-in class package!private! !
!SliceAndDiceLayoutStrategy categoriesFor: #layout:verticallyUsing:!*-in class package!private! !

!SliceAndDiceLayoutStrategy class methodsFor!

description
	^'Slice and Dice'! !
!SliceAndDiceLayoutStrategy class categoriesFor: #description!*-in class package!public! !

"Binary Globals"!

