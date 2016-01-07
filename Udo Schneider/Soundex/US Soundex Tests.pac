| package |
package := Package name: 'US Soundex Tests'.
package paxVersion: 1;
	basicComment: '$id: US Soundex Tests 1.204$, $date: 24.07.2009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Test Cases for Soundex

Usage
	See Testcases
	See here for more information:
	http://www.geocities.com/Heartland/Hills/3916/soundex.html
	http://en.wikipedia.org/wiki/Soundex

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '1.204'.


package classNames
	add: #SoundexTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Camp Smalltalk\SUnit\SUnit';
	add: 'US Soundex';
	yourself).

package setManualPrerequisites: #(
	'US Soundex').

package!

"Class Definitions"!

TestCase subclass: #SoundexTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SoundexTest guid: (GUID fromString: '{65CF15A9-5ABD-4B3F-8261-13AFAA7BFE2D}')!
SoundexTest comment: ''!
!SoundexTest categoriesForClass!Unclassified! !
!SoundexTest methodsFor!

testSoundex
	self
		assert: 'Euler' soundex = 'E460';
		assert: 'Ellery' soundex = 'E460';
		assert: 'Gauss' soundex = 'G200';
		assert: 'Gosh' soundex = 'G200';
		assert: 'Hilbert' soundex = 'H416';
		assert: 'Heilbronn' soundex = 'H416';
		assert: 'Knuth' soundex = 'K530';
		assert: 'Kant' soundex = 'K530';
		assert: 'Lloyd' soundex = 'L300';
		assert: 'Ladd' soundex = 'L300';
		assert: 'Lukasiewicz' soundex = 'L222';
		assert: 'Lissajous' soundex = 'L222'! !
!SoundexTest categoriesFor: #testSoundex!*-unreferenced selectors!public! !

"Binary Globals"!

