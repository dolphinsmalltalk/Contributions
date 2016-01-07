| package |
package := Package name: 'US ImageStripper'.
package paxVersion: 1;
	basicComment: '$id: US ImageStripper 0.046$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

An image stripper which automatically embeds version, author and copyright information into an deployed exe.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.046'.


package classNames
	add: #USImageStripper;
	yourself.

package methodNames
	add: #ImageStripper -> #finishedWithAll:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	add: 'US Application Information';
	add: 'US Collection Extensions';
	add: 'Windows Resources\US ResourceChanging ImageStripper';
	add: 'US SessionManager Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Application Information'
	'US Collection Extensions'
	'US SessionManager Extensions').

package!

"Class Definitions"!

ResourceChangingImageStripper subclass: #USImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ImageStripper methodsFor!

finishedWithAll: aCollectionOfSymbols 
	"Private - The receiver has finished with its method identified by the selectors in
	the <collection> argument, so they can be stripped."

	aCollectionOfSymbols do: [:each | [self finishedWith: each] on: Error do: [:ex | ] ]! !
!ImageStripper categoriesFor: #finishedWithAll:!operations!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

USImageStripper guid: (GUID fromString: '{7E74CCCC-B81F-430A-B0E2-060F6C2A8E19}')!
USImageStripper comment: ''!
!USImageStripper categoriesForClass!Unclassified! !
!USImageStripper methodsFor!

stringTable: aStringTable at: key with: value 
	aStringTable at: key put: value!

stringTable: aStringTable replaceAt: key with: value 
	(aStringTable includesKey: key) ifTrue: [self stringTable: aStringTable at: key with: value ]!

versionResource
	| verString newVersionResource sessionManagerClass |
	newVersionResource := super versionResource.
	sessionManagerClass := self runtimeSessionManagerClass.
	verString := sessionManagerClass projectEditionVersionString.
	newVersionResource
		productVersion: verString;
		fileVersion: verString.
	newVersionResource stringTables do: 
			[:eachTable | 
			self
				stringTable: eachTable
					replaceAt: 'LegalCopyright'
					with: sessionManagerClass legalCopyright;
				stringTable: eachTable
					replaceAt: 'ProductName'
					with: sessionManagerClass productName;
				stringTable: eachTable
					replaceAt: 'FileDescription'
					with: sessionManagerClass fileDescription;
				stringTable: eachTable
					replaceAt: 'Comments'
					with: sessionManagerClass comments;
				stringTable: eachTable
				at: 'ProjectEditionVersion'
				with: verString].
	^newVersionResource! !
!USImageStripper categoriesFor: #stringTable:at:with:!*-in class package!accessing!private! !
!USImageStripper categoriesFor: #stringTable:replaceAt:with:!*-in class package!accessing!private! !
!USImageStripper categoriesFor: #versionResource!*-in class package!accessing!private! !

"Binary Globals"!

