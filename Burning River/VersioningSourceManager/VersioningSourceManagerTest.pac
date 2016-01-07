| package |
package := Package name: 'VersioningSourceManagerTest'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #VersioningSourceManagerTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\ExternalProcess\ExternalProcess';
	add: '..\KernelLibraryExtensions\KernelLibraryExtensions';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #VersioningSourceManagerTest
	instanceVariableNames: 'testPackage testClass'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

VersioningSourceManagerTest guid: (GUID fromString: '{1C050A7D-AA24-4DA0-A97E-E4DEA61CB7AE}')!
VersioningSourceManagerTest comment: ''!
!VersioningSourceManagerTest categoriesForClass!SUnit! !
!VersioningSourceManagerTest methodsFor!

addTestClass
	self shouldnt: [ testClass := Object subclass: #VersioningSourceManagerTestingClass
				instanceVariableNames: 'ivar1 ivar2'
				classVariableNames: ''
				poolDictionaries: ''
				classInstanceVariableNames: '' ] raise: Error!

setUp
	"Create a new project, convert it to PAX format, and set it up as the default package."

	super setUp.

	testPackage := Package manager newPackage: 'VersioningSourceManagerTestPackage'.
	Package manager defaultPackage: testPackage.
	testPackage fileOutAll.	"Should perform conversion to PAX mode quietly"
!

tearDown
	| classFilename ret p |

	testClass notNil ifTrue: [ classFilename := testClass fileOutName ].

	Package manager defaultPackage: nil.
	Package manager uninstall: testPackage.

	classFilename notNil ifTrue: [
		[ File isWriteable: classFilename set: true.
		File delete: classFilename ] on: Win32Error do: [ :error | ] ].

	File isWriteable: testPackage fileOutName set: true.
	File delete: testPackage fileOutName.

	(File exists: 'rcs') ifTrue:
		[ Transcript show: 'VersioningSourceManagerTest>>tearDown : deleting ''rcs'' '.
		p := ExternalProcess new
				commandLine: 'cmd /c rd /s /q rcs';
				directory: File workingDirectory;
				stdinFilename: nil;
				stdoutFilename: 'rd.out';
				stderrFilename: 'rd.err';
				secondsToWait: 10;
				yourself.
		[ p executeSync ] on: Error do: [ :e | Transcript show: ' (', e class name, ': ', e description, ')' ].			
		Transcript cr ].

	super tearDown
!

testClassAdd
	"Test to see that when a class is added the appropriate files are checked out."

	self addTestClass.

	self should: [ testPackage isUsingPAX ].
	self should: [ testPackage == testClass owningPackage ].
	self should: [ testPackage includesClass: testClass ].
	self shouldnt: [ testPackage fileOutAll ] raise: Error.
	self should: [ File exists: testPackage fileOutName ].
	self should: [ File exists: testClass fileOutName ].
	self should: [ testPackage fileOutName ~= testClass fileOutName ].
	self should: [ SourceManager default isFileCheckedOut: testPackage fileOutName ].
	self should: [ SourceManager default isFileCheckedOut: testClass fileOutName ].!

testClassCategorization
	"Test to see that when a class is categorized the appropriate files are checked out."

	self addTestClass.
	self shouldnt: [ testPackage fileOutAll ] raise: Error.
	self shouldnt: [ SourceManager default checkIn: testPackage withComment: 'Class categorization test' ]
		raise: Exception.
	self shouldnt: [ SourceManager default checkIn: testClass withComment: 'Class categorization test' ]
		raise: Exception.
	self should: [ (SourceManager default isFileCheckedOut: testPackage fileOutName) = false ].
	self should: [ (SourceManager default isFileCheckedOut: testClass fileOutName) = false ].
	self shouldnt: [ testClass category: (ClassCategory name: 'Testing') ] raise: Error.
	self should: [ testClass categories includes: (ClassCategory name: 'Testing') ].
	self should: [ SourceManager default isFileCheckedOut: testClass fileOutName ].! !
!VersioningSourceManagerTest categoriesFor: #addTestClass!private!testing! !
!VersioningSourceManagerTest categoriesFor: #setUp!public!testing! !
!VersioningSourceManagerTest categoriesFor: #tearDown!public!testing! !
!VersioningSourceManagerTest categoriesFor: #testClassAdd!public!testing! !
!VersioningSourceManagerTest categoriesFor: #testClassCategorization!public!testing! !

!VersioningSourceManagerTest class methodsFor!

referencesToOtherPackages
	"Private - Force references to other packages."
	KernelLibraryExtension		"Global defined in the KernelLibraryExtension package"! !
!VersioningSourceManagerTest class categoriesFor: #referencesToOtherPackages!Dependencies!private! !

"Binary Globals"!

"Resources"!

