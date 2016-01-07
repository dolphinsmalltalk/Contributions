| package |
package := Package name: 'US Treemap - Layout Abstract'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Layout Abstract 0.017$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.017'.


package classNames
	add: #AbstractTreemapLayoutStrategy;
	yourself.

package methodNames
	add: #Collection -> #sum;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #AbstractTreemapLayoutStrategy
	instanceVariableNames: 'renderStrategy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Collection methodsFor!

sum
	"Sumarizes all items' size"
	^self inject: 0 into: [:sum :each | sum + each size]! !
!Collection categoriesFor: #sum!*-not in class package!operations!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

AbstractTreemapLayoutStrategy guid: (GUID fromString: '{CC8B4888-70E3-44D2-B20E-F83BE7AD2F03}')!
AbstractTreemapLayoutStrategy comment: 'AbstractTreeMapLayoutStrategy implements the <Treemap Layout Strategy> protocol used by TreemapRenderer.

Subclasses shoul override #layout:using and class>>#description.

See SliceAndDiceLayoutStrategy for a simple implementation. Take special care for layouting only child nodes with extent and updating the renderState.

Instance Variables:
	renderStrategy		<kindOf AbstractTreemapRenderStrategy>

'!
!AbstractTreemapLayoutStrategy categoriesForClass!Unclassified! !
!AbstractTreemapLayoutStrategy methodsFor!

layout: aTreemapModel in: aRectangle using: aTreemapRenderState 
	| node |
	node := (aTreemapModel rootNode)
				setLayoutRectangle: aRectangle;
				yourself.
	self layout: node using: aTreemapRenderState !

layout: aTreemapNode using: aTreemapRenderState 
	"Layout the node (including child nodes) in a Rectangle specified by aNode>>layoutRectangle.
	Create new renderSettings per child node using #update:in:, #update:horizontalIn: or #update:verticalIn:"

	^self subclassResponsibility!

setRenderStrategy: aTreeMapRenderStrategy
	renderStrategy := aTreeMapRenderStrategy! !
!AbstractTreemapLayoutStrategy categoriesFor: #layout:in:using:!*-in class package!public! !
!AbstractTreemapLayoutStrategy categoriesFor: #layout:using:!*-in class package!private! !
!AbstractTreemapLayoutStrategy categoriesFor: #setRenderStrategy:!*-in class package!private! !

!AbstractTreemapLayoutStrategy class methodsFor!

description
^self class displayString! !
!AbstractTreemapLayoutStrategy class categoriesFor: #description!*-in class package!public! !

"Binary Globals"!

