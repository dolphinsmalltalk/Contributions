| package |
package := Package name: 'US Soundex'.
package paxVersion: 1;
	basicComment: '$id: US Soundex 1.103$, $date: 24.07.2009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Calculate the Soundex of a String

Usage
	See Testcases
	See here for more information:
	http://www.geocities.com/Heartland/Hills/3916/soundex.html
	http://en.wikipedia.org/wiki/Soundex

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '1.103'.


package methodNames
	add: #String -> #soundex;
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


"Global Aliases"!


"Loose Methods"!

!String methodsFor!

soundex
	| input output soundexCodes lastChar |
	input := ReadStream on: (self select: [:each | each isAlphaNumeric]) asUppercase.
	input size < 1 ifTrue: [^''].
	output := ReadWriteStream on: String new.
	lastChar := input next.
	output nextPut: lastChar.
	soundexCodes := #('BFPV' 'CGJKQSXZ' 'DT' 'L' 'MN' 'R').

	[input atEnd] whileFalse: 
			[| char |
			char := input next.
			char ~= lastChar 
				ifTrue: 
					[1 to: 6
						do: 
							[:index | 
							((soundexCodes at: index) includes: char) 
								ifTrue: [output nextPut: (Character digitValue: index)]]].
			lastChar := char].
	output nextPutAll: '0000'.
	^output contents copyFrom: 1 to: 4! !
!String categoriesFor: #soundex!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

