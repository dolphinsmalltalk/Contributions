| package |
package := Package name: 'US DragAndDrop Extensions'.
package paxVersion: 1;
	basicComment: '$id: US DragAndDrop Extensions 0.005$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Get filenames from an native Drag&Drop Session (requires the Dolphin Harbor "DH Shell Data Transfer" package):
	filenames := aDragDropSession dragFilenames

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.005'.


package methodNames
	add: #DragDropSession -> #dragFilenames;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Dolphin Harbor\DH Shell Data Transfer';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package setManualPrerequisites: #(
	'DH Shell Data Transfer').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!DragDropSession methodsFor!

dragFilenames
	| filenames |

	filenames := OrderedCollection new.
	(self dragObjects select: [:each | each isFormatAvailable: #Filenames]) 
		do: [:each | filenames addAll: (each format: #Filenames)].
	^filenames! !
!DragDropSession categoriesFor: #dragFilenames!*-in class package!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

