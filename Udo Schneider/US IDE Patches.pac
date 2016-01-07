| package |
package := Package name: 'US IDE Patches'.
package paxVersion: 1;
	basicComment: '$id: US IDE Patches 0.055$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.055'.


package methodNames
	add: #ClassSelector -> #buildTestSuite;
	add: #IdeaSpaceShell -> #additionalAccelerators;
	add: #IdeaSpaceShell -> #allAdditionalAccelerators;
	add: #PackagePrompter -> #createSchematicWiring;
	add: #PackageSelector -> #buildTestSuite;
	add: #PackageSelector -> #savePackages:toFolder:;
	add: #SmalltalkToolShell -> #loadPackageFromRepository;
	add: #SUnitBrowser -> #additionalAccelerators;
	add: #TestSuite -> #buildSuite;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\IDE\Professional\Dolphin Professional Tools';
	add: '..\..\ITC Gorisek\Source Tracking System';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	add: '..\..\odellsoft\SUnitBrowser\SUnitBrowser';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!ClassSelector methodsFor!

buildTestSuite
	| parentEnvironment testEnvironment suite classSelections |
	classSelections := self selectionEnvironment.
	suite := TestSuite named: ('Tests in <1p>' expandMacrosWith: classSelections).
	parentEnvironment := classSelections environment.
	testEnvironment := classSelections.
	classSelections classesDo: 
			[:eachClass | 
			| hierarchyEnvironment |
			hierarchyEnvironment := parentEnvironment forClassHierarchyFrom: eachClass.
			testEnvironment := testEnvironment | hierarchyEnvironment].
	testEnvironment := testEnvironment selectMethods: [:each | each methodClass isMeta not].
	testEnvironment classesDo: 
			[:eachClass | 
			| classSuite |
			classSuite := eachClass buildSuite.
			suite addTests: (classSuite allTests 
						select: [:eachTest | eachClass canUnderstand: eachTest selector])].
	^suite! !
!ClassSelector categoriesFor: #buildTestSuite!commands!public! !

!IdeaSpaceShell methodsFor!

additionalAccelerators
	^#(#(#closeCardIfOpen 'Ctrl+F4') )!

allAdditionalAccelerators
	| answer |
	answer := OrderedCollection withAll: self additionalAccelerators.
	self subPresenters do: 
			[:each | 
			each == cardsPresenter 
				ifFalse: 
					[answer addAll: each additionalAccelerators.
					each allSubPresentersDo: [:eachSub | answer addAll: eachSub additionalAccelerators]]].
	self currentCard ifNotNil: [:tool | answer addAll: tool allAdditionalAccelerators].
	^answer! !
!IdeaSpaceShell categoriesFor: #additionalAccelerators!public! !
!IdeaSpaceShell categoriesFor: #allAdditionalAccelerators!constants!private! !

!PackagePrompter methodsFor!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	packagesPresenter
		when: #selectionChanged
			send: #onPackageSelected
			to: self;
		when: #actionPerformed
			send: #ok
			to: self.
	notPackagedPresenter 
		when: #valueChanged
		send: #toggleSetPackage
		to: self! !
!PackagePrompter categoriesFor: #createSchematicWiring!initializing!public! !

!PackageSelector methodsFor!

buildTestSuite
	| packages suite |
	packages := self selectionEnvironment.
	suite := TestSuite named: ('Tests in <1p>' expandMacrosWith: packages).
	TestCase allSubclassesDo: 
			[:eachClass | 
			(eachClass isAbstract not and: [packages includesClass: eachClass]) 
				ifTrue: 
					[| classSuite |
					classSuite := eachClass buildSuite.
					suite addTest: classSuite.
					"suite addTests: (classSuite allTests 
								select: [:eachTest | packages includesSelector: eachTest selector in: eachTest class])"]].
	^suite!

savePackages: aCollection toFolder: aPackageFolder 
	| msg dir mb |
	msg := String writeStream.
	msg
		nextPutAll: 'The following packages will be moved to the folder ';
		print: aPackageFolder folderName;
		nextPut: $:;
		cr.
	(aCollection asSortedCollection: Package defaultSortBlock) do: 
			[:each | 
			msg
				crtab;
				display: each].
	msg
		cr;
		cr;
		nextPutAll: 'Are you sure that you would like to proceed?'.
	mb := MessageBox new.
	mb beTaskModal.
	(mb confirm: msg contents) ifFalse: [^self].
	dir := File fullPathOf: aPackageFolder pathname relativeTo: SessionManager current imageBase.
	aCollection do: 
			[:each | 
			| oldPackagePath |
			oldPackagePath := FileLocator imageRelative localFileSpecFor: each packagePathname.
			each saveAs: (File composePath: dir subPath: each name).
			(File exists: oldPackagePath) ifTrue: [File delete: oldPackagePath]]! !
!PackageSelector categoriesFor: #buildTestSuite!commands!public! !
!PackageSelector categoriesFor: #savePackages:toFolder:!helpers!private! !

!SmalltalkToolShell methodsFor!

loadPackageFromRepository
	| edition |
	
	self sourceControl checkIfConnected ifFalse: [^self].
	(edition := StsPackageEditionPrompter 
				chooseWithDefault: (self packages isEmpty ifFalse: [self packages first]) name) isNil 
		ifFalse: [edition load]! !
!SmalltalkToolShell categoriesFor: #loadPackageFromRepository!public! !

!SUnitBrowser methodsFor!

additionalAccelerators

	^super additionalAccelerators , #(#(#debug 'F11'))! !
!SUnitBrowser categoriesFor: #additionalAccelerators!public! !

!TestSuite methodsFor!

buildSuite
	| copy |
	^self.
	copy := self class named: self name.
	copy addTests: self allTests.
	^copy! !
!TestSuite categoriesFor: #buildSuite!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

