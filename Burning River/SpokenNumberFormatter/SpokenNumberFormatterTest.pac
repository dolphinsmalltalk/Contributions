| package |
package := Package name: 'SpokenNumberFormatterTest'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #SpokenNumberFormatterTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'SpokenNumberFormatter';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #SpokenNumberFormatterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SpokenNumberFormatterTest guid: (GUID fromString: '{CDD250F6-1F15-41E1-B6A4-B3BA4A85B3E5}')!
SpokenNumberFormatterTest comment: ''!
!SpokenNumberFormatterTest categoriesForClass!Unclassified! !
!SpokenNumberFormatterTest methodsFor!

test0
	self should: [ 0 asSpoken = 'zero' ]!

test1
	self should: [ 1 asSpoken = 'one' ]!

test10
	self should: [ 10 asSpoken = 'ten' ]!

test1000001
	self should: [ 1000001 asSpoken = 'one million and one' ]!

test1001
	self should: [ 1001 asSpoken = 'one thousand and one' ]!

test11
	self should: [ 11 asSpoken = 'eleven' ]!

test12005078901
	| s v |
	s := SpokenNumberFormatter new beAmerican.
	v := 12005078901.
	self should: [ (s textFor: v) = 'twelve billion, five million, seventy-eight thousand, nine hundred and one' ].
	s beEuropean.
	self should: [ (s textFor: v) = 'twelve milliard, five million, seventy-eight thousand, nine hundred and one' ]!

test12345000000
	| s v |
	s := SpokenNumberFormatter new beAmerican.
	v := 12345000000.
	self should: [ (s textFor: v) = 'twelve billion, three hundred forty-five million' ].
	s beEuropean.
	self should: [ (s textFor: v) = 'twelve milliard, three hundred forty-five million' ].!

test12345000007
	| s v |
	s := SpokenNumberFormatter new beAmerican.
	v := 12345000007.
	self should: [ (s textFor: v) = 'twelve billion, three hundred forty-five million and seven' ].
	s beEuropean.
	self should: [ (s textFor: v) = 'twelve milliard, three hundred forty-five million and seven' ]!

test20
	self should: [ 20 asSpoken = 'twenty' ]!

test21
	self should: [ 21 asSpoken = 'twenty-one' ]!

test910
	self should: [ 910 asSpoken = 'nine hundred and ten' ]!

testBigNumber
	| s v |
	s := SpokenNumberFormatter new beAmerican.
	v := SpokenNumberFormatter largestFormattableNumber.
	self should: [ (s textFor: v) = 'nine hundred ninety-nine vigintillion, nine hundred ninety-nine novemdecillion, nine hundred ninety-nine octodecillion, nine hundred ninety-nine septendecillion, nine hundred ninety-nine sexdecillion, nine hundred ninety-nine quindecillion, nine hundred ninety-nine quattrodecillion, nine hundred ninety-nine tredecillion, nine hundred ninety-nine duodecillion, nine hundred ninety-nine undecillion, nine hundred ninety-nine decillion, nine hundred ninety-nine nonillion, nine hundred ninety-nine octillion, nine hundred ninety-nine septillion, nine hundred ninety-nine sextillion, nine hundred ninety-nine quintillion, nine hundred ninety-nine quadrillion, nine hundred ninety-nine trillion, nine hundred ninety-nine billion, nine hundred ninety-nine million, nine hundred ninety-nine thousand, nine hundred ninety-nine' ].
	s beEuropean.
	self should: [ (s textFor: v) = 'nine hundred ninety-nine decilliard, nine hundred ninety-nine decillion, nine hundred ninety-nine nonilliard, nine hundred ninety-nine nonillion, nine hundred ninety-nine octilliard, nine hundred ninety-nine octillion, nine hundred ninety-nine septilliard, nine hundred ninety-nine septillion, nine hundred ninety-nine sextilliard, nine hundred ninety-nine sextillion, nine hundred ninety-nine quintilliard, nine hundred ninety-nine quintillion, nine hundred ninety-nine quadrilliard, nine hundred ninety-nine quadrillion, nine hundred ninety-nine trilliard, nine hundred ninety-nine trillion, nine hundred ninety-nine billiard, nine hundred ninety-nine billion, nine hundred ninety-nine milliard, nine hundred ninety-nine million, nine hundred ninety-nine thousand, nine hundred ninety-nine' ]!

testDecimal
	self should: [ 0.123 asSpoken = 'zero point one two three' ]!

testFloat
	self should: [ 1.2345e99 asSpoken = 'one point two three four five times ten to the ninety-ninth' ]!

testFraction
	self should: [ (100 / 101) asSpoken = 'one hundred over one hundred and one' ]!

testTooBigNumber
	self should: [ (SpokenNumberFormatter largestFormattableNumber + 1) asSpoken = 'a really big number' ]! !
!SpokenNumberFormatterTest categoriesFor: #test0!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test1!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test10!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test1000001!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test1001!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test11!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test12005078901!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test12345000000!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test12345000007!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test20!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test21!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #test910!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #testBigNumber!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #testDecimal!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #testFloat!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #testFraction!public!testing! !
!SpokenNumberFormatterTest categoriesFor: #testTooBigNumber!public!testing! !

"Binary Globals"!

"Resources"!

