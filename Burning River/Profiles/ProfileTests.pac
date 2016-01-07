| package |
package := Package name: 'ProfileTests'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #InitializationFileTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'Profiles';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #InitializationFileTest
	instanceVariableNames: 'profile'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

InitializationFileTest guid: (GUID fromString: '{1489E8B2-0802-11D5-BE05-00010240D5E2}')!
InitializationFileTest comment: ''!
!InitializationFileTest categoriesForClass!Unclassified! !
!InitializationFileTest methodsFor!

profile
	^profile!

setUp
	profile := InitializationFile on: File workingDirectory, 'InitializationFileTest.ini'.

	"Initialize the data needed to test the 'read' functionality"

	KernelLibrary default
		writePrivateProfileString: 'ReadTestSection'
		keyName: 'ReadTestKey'
		string: 'ReadTestKey string value'
		profileName: self profile filename.

	KernelLibrary default
		writePrivateProfileString: 'ReadTestSection'
		keyName: 'ReadTestKey2'
		string: 'ReadTestKey2 string value'
		profileName: self profile filename.

	KernelLibrary default
		writePrivateProfileString: 'ReadTestSection'
		keyName: 'ReadTestKey3'
		string: 'ReadTestKey3 string value'
		profileName: self profile filename.

	"Add additional sections to test the ability to read all section names"

	KernelLibrary default
		writePrivateProfileString: 'AnotherSection'
		keyName: 'AnotherTestKey'
		string: 'AnotherTestKey string value'
		profileName: self profile filename.

	KernelLibrary default
		writePrivateProfileString: 'YetAnotherSection'
		keyName: 'YetAnotherTestKey'
		string: 'YetAnotherTestKey string value'
		profileName: self profile filename.!

tearDown
	(File exists: self profile filename) ifTrue: [ File delete: self profile filename ]!

testDeleteKey
	| dict ret |

	dict := self profile section: 'ReadTestSection'.
	self should: [ dict notNil ].
	self should: [ dict size = 3 ].
	ret := self profile deleteKey: 'ReadTestKey2' section: 'ReadTestSection'.
	self should: [ ret = true ].
	dict := self profile section: 'ReadTestSection'.
	self should: [ dict notNil ].
	self should: [ dict size = 2 ].
	self should: [ dict includesKey: 'ReadTestKey' ].
	self should: [ dict includesKey: 'ReadTestKey3' ].
!

testDeleteSection
	| dict ret |

	dict := self profile section: 'ReadTestSection'.
	self should: [ dict notNil ].
	self should: [ dict size = 3 ].
	ret := self profile deleteSection: 'ReadTestSection'.
	self should: [ ret = true ].
	dict := self profile section: 'ReadTestSection'.
	self should: [ dict isNil ].!

testKeyNames
	| keyNames |
	self should: [ (keyNames := self profile keyNames: 'ReadTestSection') size = 3 ].
	self should: [ keyNames includes: 'ReadTestKey' ].
	self should: [ keyNames includes: 'ReadTestKey2' ].
	self should: [ keyNames includes: 'ReadTestKey3' ]!

testRead
	self should: [
		(self profile
			section: 'ReadTestSection'
			key: 'ReadTestKey'
			default: 'default') = 'ReadTestKey string value' ].

	"Verify that reading in a non-existent value answers the default value."

	self should: [ (self profile section: 'Non-existent' key: 'Non-existent' default: 'The default non-existent value') = 'The default non-existent value' ]!

testSection
	| section |
	self should: [ (section := self profile section: 'ReadTestSection') size = 3 ].
	self should: [ section keys includes: 'ReadTestKey' ].
	self should: [ (section at: 'ReadTestKey') = 'ReadTestKey string value' ].
	self should: [ section keys includes: 'ReadTestKey2' ].
	self should: [ (section at: 'ReadTestKey2') = 'ReadTestKey2 string value' ].
	self should: [ section keys includes: 'ReadTestKey3' ].
	self should: [ (section at: 'ReadTestKey3') = 'ReadTestKey3 string value' ].
!

testSectionNames
	self should: [ self profile sectionNames size = 3 ].
	self should: [ self profile sectionNames includes: 'ReadTestSection' ].
	self should: [ self profile sectionNames includes: 'AnotherSection' ].
	self should: [ self profile sectionNames includes: 'YetAnotherSection' ]
!

testWrite
	"Write a string to a new section and key."

	self should: [
		(self profile
			section: 'WriteTestSection'
			key: 'WriteTestKey'
			value: 'WriteTestKey string value') ~= 0 ].

	"Verify that the written string can be read back in."

	self should: [ (self profile section: 'WriteTestSection' key: 'WriteTestKey' default: '') = 'WriteTestKey string value' ].
!

testWriteSection
	| ret dict |

	dict := Dictionary
			with: 'Key1' -> 'Value1'
			with: 'Key2' -> 'Value2'
			with: 'Key3' -> 'Value3'
			with: 'Key4' -> 'Value4'.
	ret := profile section: 'WriteTestSection' put: dict.
	self should: [ ret = true ].
	dict := profile section: 'WriteTestSection'.

	self should: [ dict size = 4 ].
	self should: [ (dict at: 'Key1') = 'Value1' ].
	self should: [ (dict at: 'Key2') = 'Value2' ].
	self should: [ (dict at: 'Key3') = 'Value3' ].
	self should: [ (dict at: 'Key4') = 'Value4' ]! !
!InitializationFileTest categoriesFor: #profile!accessing!public! !
!InitializationFileTest categoriesFor: #setUp!public!Running! !
!InitializationFileTest categoriesFor: #tearDown!public!Running! !
!InitializationFileTest categoriesFor: #testDeleteKey!public!testing! !
!InitializationFileTest categoriesFor: #testDeleteSection!public!testing! !
!InitializationFileTest categoriesFor: #testKeyNames!public!testing! !
!InitializationFileTest categoriesFor: #testRead!public!testing! !
!InitializationFileTest categoriesFor: #testSection!public!testing! !
!InitializationFileTest categoriesFor: #testSectionNames!public!testing! !
!InitializationFileTest categoriesFor: #testWrite!public!testing! !
!InitializationFileTest categoriesFor: #testWriteSection!public!testing! !

"Binary Globals"!

"Resources"!

