| package |
package := Package name: 'US GoldenRatio'.
package paxVersion: 1;
	basicComment: '$id: US GoldenRatio 1.2$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Calculate the golden Ratio for a given Point


Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.2'.


package methodNames
	add: #Point -> #goldenRatioX;
	add: #Point -> #goldenRatioY;
	add: 'Float class' -> #goldenRatio;
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

!Float class methodsFor!

goldenRatio
	^##((5 sqrt + 1) / 2)! !
!Float class categoriesFor: #goldenRatio!public! !

!Point methodsFor!

goldenRatioX
	^self x >= self y ifTrue: [self x @ (self x / Float goldenRatio)] ifFalse: [self x @ (self x * Float goldenRatio)]
!

goldenRatioY
	^self x >= self y 
		ifFalse: [(self y / Float goldenRatio) @ self y]
		ifTrue: [(self y * Float goldenRatio) @ self y]! !
!Point categoriesFor: #goldenRatioX!*-unreferenced selectors!public! !
!Point categoriesFor: #goldenRatioY!*-unreferenced selectors!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

