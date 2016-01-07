| package |
package := Package name: 'US Treemap - Render Flat'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Render Flat 0.017$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.017'.


package classNames
	add: #FlatRenderStrategy;
	yourself.

package methodNames
	add: #Rectangle -> #gdiTruncated;
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

AbstractTreeMapRenderStrategy subclass: #FlatRenderStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Rectangle methodsFor!

gdiTruncated
	"Answer a new <Rectangle> whose origin and corner coordinates
	are truncated to the nearest integer."

	^self species origin: origin truncated  corner: corner ceiling ! !
!Rectangle categoriesFor: #gdiTruncated!*-not in class package!converting!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

FlatRenderStrategy guid: (GUID fromString: '{4AE50A5A-EC61-4055-AABE-EFA41B54F964}')!
FlatRenderStrategy comment: ''!
!FlatRenderStrategy categoriesForClass!Unclassified! !
!FlatRenderStrategy methodsFor!

render: node using: renderState 
	#USToDo.	"Make the border color changable"
	canvas
		selectClipRegion: (Region rectangle: node layoutRectangle truncated);
		brush: (Brush color: (renderer nodeColor: node object));
		pen: (Pen color: Color black);
		rectangle: node layoutRectangle gdiTruncated! !
!FlatRenderStrategy categoriesFor: #render:using:!*-in class package!public! !

!FlatRenderStrategy class methodsFor!

description 
^'Flat'! !
!FlatRenderStrategy class categoriesFor: #description!*-in class package!public! !

"Binary Globals"!

