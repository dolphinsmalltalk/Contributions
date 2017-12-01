| package |
package := Package name: 'US Unicode Tests'.
package paxVersion: 1;
	basicComment: '$id: US Unicode Tests 0.027$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 20.09.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.027'.


package classNames
	add: #UnicodeBSTRTests;
	add: #UnicodeCharacterTests;
	add: #UnicodeStringTests;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	add: 'US Unicode';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #UnicodeBSTRTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #UnicodeCharacterTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #UnicodeStringTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

UnicodeBSTRTests guid: (GUID fromString: '{24ADE72B-73F3-4AB2-9173-326A0E5E13A0}')!
UnicodeBSTRTests comment: ''!
!UnicodeBSTRTests categoriesForClass!Unclassified! !
!UnicodeBSTRTests methodsFor!

testAtPutCodePointAbove255
	| string bstr char |
	string := 'TEST123' copy.
	bstr := BSTR fromString: string.
	char := Character value:65531.
	self 
		shouldnt: [bstr at: 1 put: char]
		raise: BoundsError
		description: 'BSTR does not store Characters with codePoint <= 255'.
	self assert: (bstr at: 1) = char
		description: 'BSTR does not return Characters with codePoint <= 255'!

testAtPutCodePointAbove65535
	| string bstr char |
	string := 'TEST123' copy.
	bstr := BSTR fromString: string.
	char := Character value: 65538.
	self 
		should: [bstr at: 1 put: char]
		raise: Error
		description: 'BSTR does not store Characters with codePoint >65535'.
	self deny: (bstr at: 1) = char description: 'BSTR does not return Characters with codePoint >65535'!

testAtPutCodePointBelow255
	| string bstr char |
	string := 'TEST123' copy.
	bstr := BSTR fromString: string.
	char := $a.
	self 
		shouldnt: [bstr at: 1 put:char]
		raise: BoundsError
		description: 'BSTR does not store Characters with codePoint <= 255'.
	self assert: (bstr at: 1) = char description: 'BSTR does not return Characters with codePoint <= 255'! !
!UnicodeBSTRTests categoriesFor: #testAtPutCodePointAbove255!public!tests! !
!UnicodeBSTRTests categoriesFor: #testAtPutCodePointAbove65535!public!tests! !
!UnicodeBSTRTests categoriesFor: #testAtPutCodePointBelow255!public!tests! !

UnicodeCharacterTests guid: (GUID fromString: '{99EF6A3B-A219-4257-BB5E-3C3D786C4DBE}')!
UnicodeCharacterTests comment: ''!
!UnicodeCharacterTests categoriesForClass!Unclassified! !
!UnicodeCharacterTests methodsFor!

asciiCharacterCodePoints
	^self asciiCharacters collect: [:each | each asInteger ]!

asciiCharacters
	^(0 to: 127) collect: [:codePoint | Character value: codePoint]!

lowercaseLetterCodePoints
	^self lowercaseLetters collect: [:each | each asInteger ]!

lowercaseLetters
	^($a asInteger to: $z asInteger) collect: [:codePoint | Character value: codePoint]!

numberCodePoints
^self numbers collect: [:each | each asInteger ]!

numbers
	^($0 asInteger to: $9 asInteger) collect: [:codePoint | Character value: codePoint]!

testAsLowercase
	self lowercaseLetterCodePoints do: 
			[:codePoint | 
			self 
				assert: (Character value: codePoint) asLowercase = (UnicodeCharacter value: codePoint) asLowercase]!

testAsUppercase
	self uppercaseLetterCodePoints
		do: 
			[:codePoint | 
			self 
				assert: (Character value: codePoint) asUppercase = (UnicodeCharacter value: codePoint) asUppercase]!

testEquality
	| char uchar |
	char := $a.
	uchar := char asUnicodeCharacter.
	self assert: char = char.
	self assert: uchar = uchar.
	self assert: char = uchar.
	self assert:  uchar = uchar.
	self deny: char = $b.
	self deny: uchar = $b.!

testHash
	| char uchar |
	char := $a.
	uchar := char asUnicodeCharacter.
	self assert: char hash = uchar hash!

testIsAlphaNumeric
	self asciiCharacterCodePoints
		do: 
			[:codePoint | 
			self assert: (Character value: codePoint) isAlphaNumeric 
						= (UnicodeCharacter value: codePoint) isAlphaNumeric]!

testIsLetter
	self asciiCharacterCodePoints do: 
			[:codePoint | 
			self assert: (Character value: codePoint) isLetter 
						= (UnicodeCharacter value: codePoint) isLetter ]!

testIsLowercase
	self asciiCharacterCodePoints 
		do: [:codePoint | self assert: (Character value: codePoint) isLowercase = (UnicodeCharacter value: codePoint) isLowercase ]!

testIsUppercase
	self asciiCharacterCodePoints do: 
			[:codePoint | 
			self 
				assert: (Character value: codePoint) isUppercase = (UnicodeCharacter value: codePoint) isUppercase ]!

testLargeCodePoints
	self shouldnt: [Character value: 256] raise: BoundsError.
	self assert: (Character value: 65536) codePoint = 65536.
	self deny: (Character value: 255) isUnicodeCharacter.
	self assert: (Character value: 65536) isUnicodeCharacter!

uppercaseLetterCodePoints
	^self uppercaseLetters collect: [:each | each asInteger ]!

uppercaseLetters
	^($A asInteger to: $Z asInteger) collect: [:codePoint | Character value: codePoint]! !
!UnicodeCharacterTests categoriesFor: #asciiCharacterCodePoints!constants!public! !
!UnicodeCharacterTests categoriesFor: #asciiCharacters!constants!public! !
!UnicodeCharacterTests categoriesFor: #lowercaseLetterCodePoints!constants!public! !
!UnicodeCharacterTests categoriesFor: #lowercaseLetters!constants!public! !
!UnicodeCharacterTests categoriesFor: #numberCodePoints!constants!public! !
!UnicodeCharacterTests categoriesFor: #numbers!constants!public! !
!UnicodeCharacterTests categoriesFor: #testAsLowercase!public!tests! !
!UnicodeCharacterTests categoriesFor: #testAsUppercase!public!tests! !
!UnicodeCharacterTests categoriesFor: #testEquality!public!tests! !
!UnicodeCharacterTests categoriesFor: #testHash!public!tests! !
!UnicodeCharacterTests categoriesFor: #testIsAlphaNumeric!public!tests! !
!UnicodeCharacterTests categoriesFor: #testIsLetter!public!tests! !
!UnicodeCharacterTests categoriesFor: #testIsLowercase!public!tests! !
!UnicodeCharacterTests categoriesFor: #testIsUppercase!public!tests! !
!UnicodeCharacterTests categoriesFor: #testLargeCodePoints!public!tests! !
!UnicodeCharacterTests categoriesFor: #uppercaseLetterCodePoints!constants!public! !
!UnicodeCharacterTests categoriesFor: #uppercaseLetters!constants!public! !

UnicodeStringTests guid: (GUID fromString: '{7D6B19D1-DA04-4C52-87ED-8FB39F85201B}')!
UnicodeStringTests comment: ''!
!UnicodeStringTests categoriesForClass!Unclassified! !
!UnicodeStringTests methodsFor!

testAsByteArray
	|string |
string := 'ABCD'.
self assert: string asByteArray = #[65 66 67 68].
self assert: string asUnicodeString asByteArray = #[65 0 66 0 67 0 68 0 ]!

testAsSymbol
	| string |
	string := 'Test'.
	self assert: string asSymbol = string asUnicodeString asSymbol.
	!

testByteStringEndsWithNull
| string bytes expectedResult |
	string := 'A'.
bytes := ByteArray fromAddress: string yourAddress length:2.
expectedResult := #[65 0].
self assert: bytes = expectedResult description: 'ByteString does not end with null char'!

testConcatenation
	| string1 string2 result |
	string1 := 'Udo'.
	string2 := 'Schneider'.
	result := 'UdoSchneider'.
	self assert: (string1 , string2) = result.
	self assert: (string1 asUnicodeString , string2) = result.
	self assert: (string1 , string2 asUnicodeString) = result.
	self assert: (string1 asUnicodeString , string2 asUnicodeString) = result.
	self assert: (string1 , string2) = result asUnicodeString.
	self assert: (string1 asUnicodeString , string2) = result asUnicodeString.
	self assert: (string1 , string2 asUnicodeString) = result asUnicodeString.
	self assert: (string1 asUnicodeString , string2 asUnicodeString) = result asUnicodeString!

testCopiedUnicodeStringEndsWithNullNull
	| string bytes expectedResult |
	string := 'A' asUnicodeString copy.
	bytes := ByteArray fromAddress: string yourAddress length: 4.
	expectedResult := #[65 0 0 0].
	self assert: bytes = expectedResult description: 'Copied UnicodeString does not end with double null char'!

testDeepCopiedUnicodeStringEndsWithNullNull
	| string bytes expectedResult |
	string := 'A' asUnicodeString deepCopy .
	bytes := ByteArray fromAddress: string yourAddress length: 4.
	expectedResult := #[65 0 0 0].
	self assert: bytes = expectedResult
		description: 'Deep Copied UnicodeString does not end with double null char'!

testEquality
	| string ustring |
	string := 'abc123'.
	ustring := string asUnicodeString.
	self assert: string = string.
	self assert: ustring = ustring.
	self assert: string = ustring.
	self assert: ustring = ustring.
	self deny: string = 'def446'.
	self deny: ustring =  'def446'!

testHash
	| string ustring |
	string := 'abc123'.
	ustring := string asUnicodeString.
	self assert: string hash = ustring hash!

testShallowCopiedUnicodeStringEndsWithNullNull
	| string bytes expectedResult |
	string := 'A' asUnicodeString shallowCopy .
	bytes := ByteArray fromAddress: string yourAddress length: 4.
	expectedResult := #[65 0 0 0].
	self assert: bytes = expectedResult
		description: 'Shallow Copied UnicodeString does not end with double null char'!

testStringBecomesUnicodeString
	| string |
	string := 'test' copy.
	self assert: string class = String description: 'Strings originating from literal strings are of class String by default.'.
	string at: 1 put: $A.
	self assert: string class = String  description: 'Strings remain Strings if non-Unicode characters are added'.
	string at: 2 put: (Character value: 345).
	self assert: string class = UnicodeString description: 'Strings become UnicodeStrings if Unicode characters are added.'!

testUnicodeStringEndsWithNullNull
	| string bytes expectedResult |
	string := 'A' asUnicodeString.
	bytes := ByteArray fromAddress: string yourAddress length: 4.
	expectedResult := #[ 65 0 0 0].
	self assert: bytes = expectedResult description: 'UnicodeString does not end with double null char'! !
!UnicodeStringTests categoriesFor: #testAsByteArray!public! !
!UnicodeStringTests categoriesFor: #testAsSymbol!public! !
!UnicodeStringTests categoriesFor: #testByteStringEndsWithNull!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testConcatenation!public!tests! !
!UnicodeStringTests categoriesFor: #testCopiedUnicodeStringEndsWithNullNull!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testDeepCopiedUnicodeStringEndsWithNullNull!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testEquality!public!tests! !
!UnicodeStringTests categoriesFor: #testHash!public!tests! !
!UnicodeStringTests categoriesFor: #testShallowCopiedUnicodeStringEndsWithNullNull!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testStringBecomesUnicodeString!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testUnicodeStringEndsWithNullNull!public!testing!tests! !

"Binary Globals"!

