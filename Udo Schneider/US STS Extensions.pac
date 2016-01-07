| package |
package := Package name: 'US STS Extensions'.
package paxVersion: 1;
	basicComment: '$id: US STS Extensions 0.095$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.095'.

package basicScriptAt: #postinstall put: 'StsPackageEditionsBrowserShell canUseIdeaSpace: true.
StsProjectBrowserShell canUseIdeaSpace: true.'.

package methodNames
	add: #Package -> #currentProjectEdition:;
	add: #Package -> #isInSTS;
	add: #Package -> #isStsPathChanged;
	add: #Package -> #latestEdition;
	add: #Package -> #latestEditionInProject:;
	add: #Package -> #oldestEdition;
	add: #Package -> #oldestEditionInProject:;
	add: #Package -> #packageEditions;
	add: #Package -> #projectEditions:;
	add: #StsManager -> #connectToDatabase;
	add: #StsManager -> #getRepositoryPath;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\ITC Gorisek\Source Tracking System';
	add: 'US Package Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Package Extensions').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Package methodsFor!

currentProjectEdition: projectName 
	| packageEditon projects projectEdition |
	packageEditon := self packageEditions detect: [:each | each versionDescriptor = packageVersion]
				ifNone: [self packageEditions first].
	projects := self projectEditions: projectName.
	projectEdition := projects detect: [:each | each packageEditions includes: packageEditon]
				ifNone: [projects first].
	^projectEdition !

isInSTS
	^([self packageEditions] on: Error do: [:ex | #()]) notEmpty!

isStsPathChanged
	^self packageEditions first pathName = self packagePathname!

latestEdition
	^self packageEditions first!

latestEditionInProject: projectName 
	| allProjects packageProjects |
	allProjects := self projectEditions: projectName.
	packageProjects := allProjects select: 
					[:eachProjectEditon | 
					eachProjectEditon packageEditions 
						anySatisfy: [:eachPackageEdition | eachPackageEdition name = self name]].
	^packageProjects first!

oldestEdition
	^self packageEditions last!

oldestEditionInProject: projectName 
	| allProjects packageProjects |
	allProjects := self projectEditions: projectName.
	packageProjects := allProjects select: 
					[:eachProjectEditon | 
					eachProjectEditon packageEditions 
						anySatisfy: [:eachPackageEdition | eachPackageEdition name = self name]].
	^packageProjects last!

packageEditions
	^StsManager current getPackageEditionsFor: self name!

projectEditions: projectName 
	^StsManager current getProjectEditionsFor: projectName! !
!Package categoriesFor: #currentProjectEdition:!*-not in class package!development!must strip!public! !
!Package categoriesFor: #isInSTS!public!testing! !
!Package categoriesFor: #isStsPathChanged!private! !
!Package categoriesFor: #latestEdition!*-not in class package!development!must strip!public! !
!Package categoriesFor: #latestEditionInProject:!*-not in class package!development!must strip!public! !
!Package categoriesFor: #oldestEdition!*-not in class package!development!must strip!public! !
!Package categoriesFor: #oldestEditionInProject:!*-not in class package!development!must strip!public! !
!Package categoriesFor: #packageEditions!*-not in class package!development!must strip!public! !
!Package categoriesFor: #projectEditions:!*-not in class package!development!must strip!public! !

!StsManager methodsFor!

connectToDatabase
	| transaction |
	[db := self databaseClass openOn: repositoryPath] on: Error do: [:ex | db := nil].
	db isNil 
		ifTrue: 
			[self shutdown.
			MessageBox 
				errorMsg: 'Source Tracking System can not connect to repository.

Source Tracking System has been shutdown.'
				caption: 'Can not open OmniBase database'.
			^nil].
	
	[transaction := db newTransaction.
	methodDictOIDs := IdentityDictionary new: Smalltalk size * 2.
	Smalltalk allClasses do: 
			[:each | 
			self
				createMethodDictionaryFor: each in: transaction;
				createMethodDictionaryFor: each class in: transaction].
	transaction root at: self projectsKey
		ifAbsentPut: 
			["migrate repository to STS version 2.0"
			(transaction root at: self packagesKey) do: 
					[:eachCollection | 
					eachCollection do: [:eachPackageEdition | transaction store: eachPackageEdition].
					transaction markDirty: eachCollection].
			LookupTable new].
	transaction checkpoint.
	(transaction root at: self methodDictionariesKey) 
		associationsDo: [:each | methodDictOIDs at: each key put: (transaction getObjectID: each value)].
	transaction abort] 
			ifCurtailed: 
				[db close.
				db := nil]. 	StsPackageManager updatePackageStsStatus!

getRepositoryPath
	^repositoryPath! !
!StsManager categoriesFor: #connectToDatabase!must strip!private! !
!StsManager categoriesFor: #getRepositoryPath!accessing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

