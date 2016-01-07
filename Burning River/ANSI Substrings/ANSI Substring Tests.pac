| package |
package := Package name: 'ANSI Substring Tests'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #ANSISubstringTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'ANSI Substrings';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #ANSISubstringTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ANSISubstringTest guid: (GUID fromString: '{6E55D1D7-0C66-4334-A877-33DCBC209C18}')!
ANSISubstringTest comment: ''!
!ANSISubstringTest categoriesForClass!Unclassified! !
!ANSISubstringTest methodsFor!

testCharacterArgument
	"Note that the behavior of Character arguments is subtly different from that of SequenceableCollection
	arguments, in that when the argument to #subStrings is a Character the effect of the last trailing delimiter
	is ignored."

	self should: [ ('a,b,c,d,' subStrings: $,) = #('a' 'b' 'c' 'd') ].
	self should: [ ('a,b,c,d,,,,,' subStrings: $,) = #('a' 'b' 'c' 'd' '' '' '' '') ].
	self should: [ ('a,b,c,d' subStrings: $,) = #('a' 'b' 'c' 'd') ].
	self should: [ ('abc,def,ghi' subStrings: $,) =  #('abc' 'def' 'ghi') ].
	self should: [ (',abc,def,ghi' subStrings: $,) =  #('' 'abc' 'def' 'ghi') ].
	self should: [ (',,abc,,def-ghi,-jkl-mno,,-,,p--' subStrings: $-) = #(',,abc,,def' 'ghi,' 'jkl' 'mno,,' ',,p' '') ].!

testDefaultSubstringExtractor
	| extractor |

	extractor := SubstringExtractor new.

	self should: [ (extractor extractSubstringsFrom: '' separators: '') = #() ].
	self should: [ (extractor extractSubstringsFrom: '' separators: ',') = #() ].
	self should: [ (extractor extractSubstringsFrom: 'abc-def' separators: ',') = #('abc-def') ].
	self should: [ (extractor extractSubstringsFrom: ',,,,' separators: ',') = #('' '' '' '' '') ].
	self should: [ (extractor extractSubstringsFrom: 'abc,def,ghi' separators: '') = #('abc,def,ghi') ].
	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d,' separators: ',') = #('a' 'b' 'c' 'd' '') ].
	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d' separators: ',') = #('a' 'b' 'c' 'd') ].
	self should: [ (extractor extractSubstringsFrom: 'abc,def,ghi' separators: ',') =  #('abc' 'def' 'ghi') ].
	self should: [ (extractor extractSubstringsFrom: ',abc,def,ghi' separators: ',') =  #('' 'abc' 'def' 'ghi') ].
	self should: [ (extractor extractSubstringsFrom: ',,abc,,def-ghi,-jkl-mno,,-,,p--' separators: ',-') = #('' '' 'abc' '' 'def' 'ghi' '' 'jkl' 'mno' '' '' '' '' 'p' '' '') ].
!

testOldNonStandardBehavior
	"Ensure that the old behavior is available so that String>>lines still works properly."

	self should: [ (String lineDelimiter, 'a', String lineDelimiter, 'bc',
				String lineDelimiter, String lineDelimiter, 'def', String lineDelimiter) lines = #('' 'a' 'bc' '' 'def') ]
				!

testRemoveTrailingEmptyElements
	| col extractor |

	extractor := SubstringExtractor new.
	col := #('' '' 'a' 'b' 'c' 'd' '' '').

	self should: [ (extractor removeTrailingEmptyElementsFrom: col maxSeparatorsToRemove: nil) = #('' '' 'a' 'b' 'c' 'd') ].
	self should: [ (extractor removeTrailingEmptyElementsFrom: col maxSeparatorsToRemove: 2) = #('' '' 'a' 'b' 'c' 'd') ].
	self should: [ (extractor removeTrailingEmptyElementsFrom: col maxSeparatorsToRemove: 1) = #('' '' 'a' 'b' 'c' 'd' '') ].
	self should: [ (extractor removeTrailingEmptyElementsFrom: col maxSeparatorsToRemove: 0) = #('' '' 'a' 'b' 'c' 'd' '' '') ].!

testStringArgument
	self should: [ ('a,b,c,d,' subStrings: ',') = #('a' 'b' 'c' 'd' '') ].
	self should: [ ('a,b,c,d' subStrings: ',') = #('a' 'b' 'c' 'd') ].
	self should: [ ('abc,def,ghi' subStrings: ',') =  #('abc' 'def' 'ghi') ].
	self should: [ (',abc,def,ghi' subStrings: ',') =  #('' 'abc' 'def' 'ghi') ].
	self should: [ (',,abc,,def-ghi,-jkl-mno,,-,,p--' subStrings: ',-') = #('' '' 'abc' '' 'def' 'ghi' '' 'jkl' 'mno' '' '' '' '' 'p' '' '') ].!

testSubstringExtractorCompressingConsecutiveSeparators
	| extractor |

	extractor := SubstringExtractor new.
	extractor compressConsecutiveSeparators: true.

	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d,' separators: ',') = #('a' 'b' 'c' 'd' '') ].
	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d' separators: ',') = #('a' 'b' 'c' 'd') ].
	self should: [ (extractor extractSubstringsFrom: 'abc,def,ghi' separators: ',') =  #('abc' 'def' 'ghi') ].
	self should: [ (extractor extractSubstringsFrom: ',abc,def,ghi' separators: ',') =  #('' 'abc' 'def' 'ghi') ].
	self should: [ (extractor extractSubstringsFrom: ',,abc,,def-ghi,-jkl-mno,,-,,p--' separators: ',-') = #('' 'abc' 'def' 'ghi' 'jkl' 'mno' 'p' '') ].
!

testSubstringExtractorIgnoringLeadingSeparators
	| extractor |

	extractor := SubstringExtractor new.
	extractor ignoreLeadingSeparators: true.

	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d,' separators: ',') = #('a' 'b' 'c' 'd' '') ].
	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d' separators: ',') = #('a' 'b' 'c' 'd') ].
	self should: [ (extractor extractSubstringsFrom: 'abc,def,ghi' separators: ',') =  #('abc' 'def' 'ghi') ].
	self should: [ (extractor extractSubstringsFrom: ',abc,def,ghi' separators: ',') =  #('abc' 'def' 'ghi') ].
	self should: [ (extractor extractSubstringsFrom: ',,abc,,def-ghi,-jkl-mno,,-,,p--' separators: ',-') = #('abc' '' 'def' 'ghi' '' 'jkl' 'mno' '' '' '' '' 'p' '' '') ].
!

testSubstringExtractorIgnoringTrailingSeparators
	| extractor |

	extractor := SubstringExtractor new.
	extractor ignoreTrailingSeparators: true.

	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d,' separators: ',') = #('a' 'b' 'c' 'd') ].
	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d' separators: ',') = #('a' 'b' 'c' 'd') ].
	self should: [ (extractor extractSubstringsFrom: 'abc,def,ghi' separators: ',') =  #('abc' 'def' 'ghi') ].
	self should: [ (extractor extractSubstringsFrom: ',abc,def,ghi' separators: ',') =  #('' 'abc' 'def' 'ghi') ].
	self should: [ (extractor extractSubstringsFrom: ',,abc,,def-ghi,-jkl-mno,,-,,p--' separators: ',-') = #('' '' 'abc' '' 'def' 'ghi' '' 'jkl' 'mno' '' '' '' '' 'p') ].

	extractor trailingSeparatorsToRemove: 1.

	self should: [ (extractor extractSubstringsFrom: 'a,b,c,d,,,' separators: ',') = #('a' 'b' 'c' 'd' '' '') ].! !
!ANSISubstringTest categoriesFor: #testCharacterArgument!public!testing! !
!ANSISubstringTest categoriesFor: #testDefaultSubstringExtractor!public!testing! !
!ANSISubstringTest categoriesFor: #testOldNonStandardBehavior!public!testing! !
!ANSISubstringTest categoriesFor: #testRemoveTrailingEmptyElements!public!testing! !
!ANSISubstringTest categoriesFor: #testStringArgument!public!testing! !
!ANSISubstringTest categoriesFor: #testSubstringExtractorCompressingConsecutiveSeparators!public!testing! !
!ANSISubstringTest categoriesFor: #testSubstringExtractorIgnoringLeadingSeparators!public!testing! !
!ANSISubstringTest categoriesFor: #testSubstringExtractorIgnoringTrailingSeparators!public!testing! !

"Binary Globals"!

"Resources"!

