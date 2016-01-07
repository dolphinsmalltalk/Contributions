| package |
package := Package name: 'US Package Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Package Extensions 0.014$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.014'.


package methodNames
	add: #Package -> #icon;
	add: #Package -> #isCleanImagePackage;
	add: #Package -> #isObjectArtsPackage;
	add: #Package -> #namePrefix;
	add: #Package -> #prerequisitesInLoadOrder;
	add: #Package -> #prerequisitesInLoadOrderWithoutCleanImagePackages;
	add: #Package -> #sortedRecursivePrerequisites;
	add: #PackageManager -> #cleanImagePackageNames;
	add: #PackageManager -> #cleanImagePackagePrefixes;
	add: #PackageManager -> #cleanImagePackages;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Package methodsFor!

icon
	^self isObjectArtsPackage ifTrue: [(Icon fromFile: 'OAPackage.ico'
							usingLocator: (FolderRelativeFileLocator 
									basePath: (FileLocator imageRelative localFileSpecFor: 'Udo Schneider\Goodies\Resources\')))] ifFalse: [super icon]!

isCleanImagePackage
	^Package manager cleanImagePackages includes: self!

isObjectArtsPackage
	^self namePrefix asLowercase = 'object arts'.
	"^(self packagePathname indexOfSubCollection: 'Object Arts') notNull"!

namePrefix
	^((ImageRelativeFileLocator new relativePathTo: self packageFileName) subStrings: $\) first!

prerequisitesInLoadOrder
	| prerequisitePackages currentPackageIndex |
	prerequisitePackages := OrderedCollection with: self.
	currentPackageIndex := 1.
	[currentPackageIndex > prerequisitePackages size] whileFalse: 
			[| currentPackage |
			currentPackage := prerequisitePackages at: currentPackageIndex.
			prerequisitePackages 
				addAll: (currentPackage prerequisites reject: [:each | prerequisitePackages includes: each]).
			currentPackageIndex := currentPackageIndex + 1].
	^prerequisitePackages reverse!

prerequisitesInLoadOrderWithoutCleanImagePackages
	| prerequisitePackages currentPackageIndex |
	prerequisitePackages := OrderedCollection with: self.
	currentPackageIndex := 1.
	[currentPackageIndex > prerequisitePackages size] whileFalse: 
			[| currentPackage |
			currentPackage := prerequisitePackages at: currentPackageIndex.
			prerequisitePackages 
				addAll: (currentPackage prerequisites reject: [:each | each isCleanImagePackage]).
			currentPackageIndex := currentPackageIndex + 1].
	^prerequisitePackages reverse copyWithoutDuplicates!

sortedRecursivePrerequisites
	| allPrerequisitePackages currentPrerequisitePackages currentPackage |
	allPrerequisitePackages := OrderedCollection with: self.
	currentPrerequisitePackages := Set new.
	currentPackage := self.
	
	[currentPrerequisitePackages addAll: (currentPackage prerequisites 
				reject: [:each | (allPrerequisitePackages includes: each) ]).
	currentPrerequisitePackages notEmpty] 
			whileTrue: 
				[currentPackage := currentPrerequisitePackages any.
				allPrerequisitePackages addFirst: currentPackage.
				currentPrerequisitePackages remove: currentPackage].
	^allPrerequisitePackages! !
!Package categoriesFor: #icon!public! !
!Package categoriesFor: #isCleanImagePackage!public! !
!Package categoriesFor: #isObjectArtsPackage!public! !
!Package categoriesFor: #namePrefix!public! !
!Package categoriesFor: #prerequisitesInLoadOrder!public! !
!Package categoriesFor: #prerequisitesInLoadOrderWithoutCleanImagePackages!public! !
!Package categoriesFor: #sortedRecursivePrerequisites!public! !

!PackageManager methodsFor!

cleanImagePackageNames
	^self cleanImagePackages collect: [:each | each name]!

cleanImagePackagePrefixes
	^#( 'itc gorisek' 'object arts' 'refactory'  'camp smalltalk' ).
	"A list of prefixes can be generated using:"
	(Package manager packages values collect: [:eachPackage | eachPackage namePrefix asLowercase]) 
		asSet!

cleanImagePackages
	^Package manager packages values 
		select: [:eachPackage | (self cleanImagePackagePrefixes includes: eachPackage namePrefix asLowercase) or: [eachPackage name = 'Source Tracking System']]! !
!PackageManager categoriesFor: #cleanImagePackageNames!public! !
!PackageManager categoriesFor: #cleanImagePackagePrefixes!public! !
!PackageManager categoriesFor: #cleanImagePackages!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

