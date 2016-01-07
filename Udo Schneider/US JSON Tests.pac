| package |
package := Package name: 'US JSON Tests'.
package paxVersion: 1;
	basicComment: '$id: US JSON Tests 0.004$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.004'.


package classNames
	add: #JsonTestCase;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #JsonTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

JsonTestCase guid: (GUID fromString: '{D9F1CDC7-DAB5-42E8-B941-E382B36B3C56}')!
JsonTestCase comment: ''!
!JsonTestCase categoriesForClass!Unclassified! !
!JsonTestCase methodsFor!

json: aString equals: aValue 
	| jsonValue |
	jsonValue := Object fromJsonString: aString.
	self assert: jsonValue = aValue!

render: anObject equals: aString 
	| smalltalkString |
	smalltalkString := anObject jsonString.
	self assert: smalltalkString = aString!

testReadArray
self json: '[]' equals: #().
self json: ' [ ] ' equals: #().
self json: ' [ [ [ ] [ ] ] [ ] ]' equals: #( #( #() #() ) #() ).
self json: '[ 123 "Hello" true false null]' equals: (Array with: 123 with: 'Hello' with: true with: false with: nil)!

testReadAtomicValues

self json: 'true' equals: true.
self json: ' true ' equals: true.
self json: 'false' equals: false.
self json: ' false ' equals: false.
self json: 'null' equals: nil.
self json: ' null ' equals: nil!

testReadNumber
	self json: '123' equals: 123.
	self json: '-123' equals: -123.
	self json: '1.23' equals: 1.23.
	self json: '-1.23' equals: -1.23.
	self json: '123e456' equals: 123e456.
	self json: '-123e-456' equals: -123e-456!

testReadObject

self json: '{}' equals: Dictionary new.
self json: ' { } ' equals: Dictionary new.
self json: '{ "a" : "b" , true : false } ' equals: (Dictionary new at: 'a' put: 'b'; at: true put: false; yourself)!

testReadString
	self json: '"Hello World"' equals: 'Hello World'.
	self json: '"Quote \""' equals: 'Quote "'.
	self json: '"Reverse Solidus \\"' equals: 'Reverse Solidus \'.
	self json: '"Solidus \/"' equals: 'Solidus /'.
	self json: '"Backspace \b"' equals: 'Backspace ' , Character backspace asString.
	
	self json: '"Formfeed \f"' equals: 'Formfeed ' , Character ff asString.
	self json: '"Newline \n"' equals: 'Newline ' , Character nl asString.
	self json: '"Carriage Return \r"' equals: 'Carriage Return ' , Character cr asString.
	self json: '"Tab \t"' equals: 'Tab ' , Character tab asString.!

testWriteArray
	self render: (Array 
				with: 123
				with: 'Hello'
				with: true
				with: false
				with: nil)
		equals: '[123,"Hello",true,false,null]'!

testWriteAtomicValues
	self render: true equals: 'true'.
	self render: false equals: 'false'.
	self render: nil equals: 'null'!

testWriteNumber
	self render: 123 equals: '123'.
	self render: -123 equals: '-123'.
	self render: 1.23 equals: '1.23'.
	self render: -1.23 equals: '-1.23'.
!

testWriteObject
self render: ((Dictionary new)
				at: 'a' put: 'b';
				at: true put: false;
				yourself) equals: '{"a":"b",true:false}'!

testWriteString
	| stream |
	stream := ReadWriteStream on: String new.
	stream
		nextPutAll: 'Hello';
		nextPut: $";
		nextPut: $\;
		nextPut: $/;
		nextPut: Character backspace;
		nextPut: Character ff;
		nextPut: Character nl;
		nextPut: Character cr;
		nextPut: Character tab;
		nextPutAll: 'World'.
		self render: stream contents equals: '"Hello\"\\\/\b\f\n\r\tWorld"'! !
!JsonTestCase categoriesFor: #json:equals:!public! !
!JsonTestCase categoriesFor: #render:equals:!public! !
!JsonTestCase categoriesFor: #testReadArray!public! !
!JsonTestCase categoriesFor: #testReadAtomicValues!public! !
!JsonTestCase categoriesFor: #testReadNumber!public! !
!JsonTestCase categoriesFor: #testReadObject!public! !
!JsonTestCase categoriesFor: #testReadString!public! !
!JsonTestCase categoriesFor: #testWriteArray!public! !
!JsonTestCase categoriesFor: #testWriteAtomicValues!public! !
!JsonTestCase categoriesFor: #testWriteNumber!public! !
!JsonTestCase categoriesFor: #testWriteObject!public! !
!JsonTestCase categoriesFor: #testWriteString!public! !

"Binary Globals"!

