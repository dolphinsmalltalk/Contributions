| package |
package := Package name: 'StableSortedCollectionTest'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #SortTestData;
	add: #StableSortedCollectionTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'StableSortedCollection';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

Object subclass: #SortTestData
	instanceVariableNames: 'value order'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #StableSortedCollectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SortTestData guid: (GUID fromString: '{CE6B32F4-F5D6-42DB-AE5F-B6C00ED04648}')!
SortTestData comment: ''!
!SortTestData categoriesForClass!Unclassified! !
!SortTestData methodsFor!

order
	"Answer the value of the receiver's ''order'' instance variable."

	^order!

order: anObject
	"Set the value of the receiver's ''order'' instance variable to the argument, anObject."

	order := anObject!

printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: '(value: ';
		nextPutAll: value printString;
		nextPutAll: ' order: ';
		nextPutAll: order printString;
		nextPut: $)!

value
	"Answer the value of the receiver's ''value'' instance variable."

	^value!

value: anObject
	"Set the value of the receiver's ''value'' instance variable to the argument, anObject."

	value := anObject! !
!SortTestData categoriesFor: #order!accessing!public! !
!SortTestData categoriesFor: #order:!accessing!public! !
!SortTestData categoriesFor: #printOn:!printing!public! !
!SortTestData categoriesFor: #value!accessing!public! !
!SortTestData categoriesFor: #value:!accessing!public! !

StableSortedCollectionTest guid: (GUID fromString: '{E425DEC6-546A-4C91-8B09-BF8842CFA503}')!
StableSortedCollectionTest comment: ''!
!StableSortedCollectionTest categoriesForClass!Unclassified! !
!StableSortedCollectionTest methodsFor!

testBasic
	| col errs last |

	col := StableSortedCollection new addAll: (100 to: 1 by: -1); yourself.
	last := nil.
	errs := col inject: 0 into:
		[ :tot :n |
		last notNil ifTrue: [ last > n ifTrue: [ tot := tot + 1 ] ].
		last := n.
		tot ].
	self should: [ errs = 0 ]!

testStability1
	"Verify that the sort is stable"

	"wod - fix bug so that it is actually verifying the StableSortedCollection"

	| testData col lastTestData errs |

	testData := OrderedCollection new.
	testData addAll: ((100 to: 1 by: -1) collect: [:n | SortTestData new value: n]).
	testData addAll: ((100 to: 1 by: -1) collect: [:n | SortTestData new value: n]).
	testData addAll: ((100 to: 1 by: -1) collect: [:n | SortTestData new value: n]).
	1 to: testData size do:
		[:i |
		(testData at: i) order: i].
	col := StableSortedCollection new sortBlock: [:v1 :v2 | v1 value <= v2 value].
	col addAll: testData.
	lastTestData := nil.
	errs := col inject: 0 into:
		[:tot :aSortTestData |
		lastTestData notNil
			ifTrue:
				[ lastTestData value = aSortTestData value
					ifTrue:
						[ lastTestData order > aSortTestData order
							ifTrue:
								[tot := tot + 1]]].
		lastTestData := aSortTestData.
		tot ].

	self should: [errs = 0]!

testStability2
	"Verify that the sort is stable"

	"wod - fix bug so that it is actually verifying the StableSortedCollection"

	| testData col lastTestData errs |

	testData := OrderedCollection new.
	(25 to: 1 by: -1) do:
		[ :i |
		1 to: 100 do:
			[ :j |
			testData add: (SortTestData new value: i) ] ].

	1 to: testData size do:
		[ :i | (testData at: i) order: i ].

	col := StableSortedCollection new sortBlock: [:v1 :v2 | v1 value <= v2 value].
	col addAll: testData.
	lastTestData := nil.
	errs := col inject: 0 into:
		[:tot :aSortTestData |
		lastTestData notNil
			ifTrue:
				[ lastTestData value = aSortTestData value
					ifTrue:
						[ lastTestData order > aSortTestData order
							ifTrue:
								[ tot := tot + 1 ] ] ].
		lastTestData := aSortTestData.
		tot].

	self should: [errs = 0]!

testTempArrayLargeEnough
	| collection errors last |
	collection := StableSortedCollection new.
	collection addAll: (100 to: 200).
	60 timesRepeat: [collection removeFirst].
	collection addAll: (130 to: 200).
	last := nil.
	errors := 0.
	collection do:
		[ :each |
		(last notNil and: [last > each]) ifTrue: [ errors := errors + 1 ].
		last := each].
	self assert: errors = 0
! !
!StableSortedCollectionTest categoriesFor: #testBasic!public!testing! !
!StableSortedCollectionTest categoriesFor: #testStability1!public!testing!unit tests! !
!StableSortedCollectionTest categoriesFor: #testStability2!public!testing!unit tests! !
!StableSortedCollectionTest categoriesFor: #testTempArrayLargeEnough!public!unit tests! !

"Binary Globals"!

"Resources"!

