| package |
package := Package name: 'LogFileStreamTest'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #LogFileStreamTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'LogFileStream';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #LogFileStreamTest
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LogFileStreamTest guid: (GUID fromString: '{B978EB50-5C5E-4C47-AEFF-2DE154F2C8BA}')!
LogFileStreamTest comment: ''!
!LogFileStreamTest categoriesForClass!SUnit! !
!LogFileStreamTest methodsFor!

filename
	^'LogFileStreamTest.out'!

setUp
	stream := LogFileStream open: self filename!

stream
	^stream!

stream: anObject
	stream := anObject!

tearDown
	stream close.
	File delete: self filename!

testNextPut
	| s |

	self shouldnt: [ self stream nextPut: $X; cr ] raise: Error.
	self stream
		flush;
		position: 0.
	s := self stream nextLine.
	self should: [ s = ((self stream timeStampString: TimeStamp current), '  X') ]!

testNextPutAll
	| t s |

	t := '123ABC456def'.
	self shouldnt: [ self stream nextPutAll: t; cr ] raise: Error.
	self stream
		flush;
		position: 0.
	s := self stream nextLine.
	self should: [ s = ((self stream timeStampString: TimeStamp current), '  ', t) ]! !
!LogFileStreamTest categoriesFor: #filename!private! !
!LogFileStreamTest categoriesFor: #setUp!private! !
!LogFileStreamTest categoriesFor: #stream!accessing!private! !
!LogFileStreamTest categoriesFor: #stream:!accessing!private! !
!LogFileStreamTest categoriesFor: #tearDown!private! !
!LogFileStreamTest categoriesFor: #testNextPut!public! !
!LogFileStreamTest categoriesFor: #testNextPutAll!public! !

"Binary Globals"!

"Resources"!

