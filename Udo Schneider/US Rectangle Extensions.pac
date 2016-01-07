| package |
package := Package name: 'US Rectangle Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Rectangle Extensions 0.004$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.004'.


package methodNames
	add: #Rectangle -> #fitIn:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Rectangle methodsFor!

fitIn: destinationRectangle 
"Resize self proportionaly to fit into a destination Rectangle. Return the posision within the desination Rectangle"
	| destinationExtent |
	destinationExtent := destinationRectangle width / self width 
				> (destinationRectangle height / self height) 
					ifTrue: [(destinationRectangle height / self height * self width) @ destinationRectangle height]
					ifFalse: [destinationRectangle width @ (destinationRectangle width / self width * self height)].
	^Rectangle center: destinationRectangle center rounded extent: destinationExtent truncated! !
!Rectangle categoriesFor: #fitIn:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

