| package |
package := Package name: 'US Locale Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Locale Extensions 0.004$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.004'.


package methodNames
	add: #Locale -> #evaluate:;
	add: 'Locale class' -> #englishUS;
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

!Locale methodsFor!

evaluate: aBlock 
	"Evaluate aBlock setting aLocale as the receiver default locale,
	and restoring the previous default locale after the evaluation of aBlock."

	| previousLocale |
	
	[previousLocale := self class default.
	UserDefault := self.
	aBlock value] 
			ensure: [UserDefault := previousLocale]! !
!Locale categoriesFor: #evaluate:!public! !

!Locale class methodsFor!

englishUS
	"Answer the English (United States) system locale."

	^Locale lcid: 1033! !
!Locale class categoriesFor: #englishUS!accessing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

