| package |
package := Package name: 'US LevenshteinDistance'.
package paxVersion: 1;
	basicComment: '$id: US LevenshteinDistance 1.006$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Calculate the Levenshtein Distance of  two SequencableCollections

Usage
	See Testcases
	See here for more information:
	http://www.merriampark.com/ld.htm#
	http://www-igm.univ-mlv.fr/~lecroq/seqcomp/node2.html
	http://en.wikipedia.org/wiki/Levenshtein_distance
	

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.006'.


package methodNames
	add: #Number -> #min:min:;
	add: #SequenceableCollection -> #levenshteinDistance:;
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

!Number methodsFor!

min: a min: b 
	| min |
	min := self.
	a < min ifTrue: [min := a].
	b < min ifTrue: [min := b].
	^min! !
!Number categoriesFor: #min:min:!public! !

!SequenceableCollection methodsFor!

levenshteinDistance: aSequenceableCollection 
	| matrix colChar rowChar cost stream |
	self isEmpty ifTrue: [^aSequenceableCollection  size].
	aSequenceableCollection  isEmpty ifTrue: [^self size].
	matrix := Dictionary new.
	0 to: self size do: [:col | 0 to: aSequenceableCollection  size do: [:row | matrix at: col @ row put: 0]].
	0 to: self size do: [:col | matrix at: col @ 0 put: col].
	0 to: aSequenceableCollection  size do: [:row | matrix at: 0 @ row put: row].
	1 to: self size
		do: 
			[:col | 
			colChar := self at: col.
			1 to: aSequenceableCollection  size
				do: 
					[:row | 
					rowChar := aSequenceableCollection  at: row.
					colChar = rowChar ifTrue: [cost := 0] ifFalse: [cost := 1].
					matrix at: col @ row
						put: ((matrix at: (col - 1) @ row) + 1 min: (matrix at: col @ (row - 1)) + 1
								min: (matrix at: (col - 1) @ (row - 1)) + cost)]].
	^matrix at: self size @ aSequenceableCollection  size! !
!SequenceableCollection categoriesFor: #levenshteinDistance:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

