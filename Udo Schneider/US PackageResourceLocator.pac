| package |
package := Package name: 'US PackageResourceLocator'.
package paxVersion: 1;
	basicComment: '$id: US PackageResourceLocator 0.008$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 31.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.008'.


package classNames
	add: #PackageResourcesLocator;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\ActiveX\Components\Picture\OLE Picture';
	add: 'US File Locators';
	yourself).

package!

"Class Definitions"!

Object subclass: #PackageResourcesLocator
	instanceVariableNames: 'package packageLocator'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

PackageResourcesLocator guid: (GUID fromString: '{008EF580-73CB-4D1E-87FC-FD43ABD1EC36}')!
PackageResourcesLocator comment: ''!
!PackageResourcesLocator categoriesForClass!Unclassified! !
!PackageResourcesLocator methodsFor!

bitmap: anIdentifier 
	^DIBSection fromFile: anIdentifier usingLocator: self packageLocator!

icon: anIdentifier 
	^Icon 
		fromId: anIdentifier
		in: nil
		usingLocator: self packageLocator!

localFileSpecFor: aString 
	^self packageLocator localFileSpecFor: aString!

olePicture: anIdentifier 
	^OLEPicture fromFile: anIdentifier usingLocator: self packageLocator!

package
^package!

packageLocator
	packageLocator isNil 
		ifTrue: [packageLocator := PackageFolderRelativeFileLocator package: self package folder: 'Resources'].
	^packageLocator!

setPackage: aPackage
package := aPackage! !
!PackageResourcesLocator categoriesFor: #bitmap:!public! !
!PackageResourcesLocator categoriesFor: #icon:!public! !
!PackageResourcesLocator categoriesFor: #localFileSpecFor:!public! !
!PackageResourcesLocator categoriesFor: #olePicture:!public! !
!PackageResourcesLocator categoriesFor: #package!accessing!public! !
!PackageResourcesLocator categoriesFor: #packageLocator!public! !
!PackageResourcesLocator categoriesFor: #setPackage:!accessing!private! !

!PackageResourcesLocator class methodsFor!

forPackage: aPackage
	^super new setPackage: aPackage; yourself!

forPackageNamed: aString
^self forPackage: (Package manager packageNamed: aString)!

new
	

	^self shouldNotImplement! !
!PackageResourcesLocator class categoriesFor: #forPackage:!instance creation!public! !
!PackageResourcesLocator class categoriesFor: #forPackageNamed:!instance creation!public! !
!PackageResourcesLocator class categoriesFor: #new!instance creation!public! !

"Binary Globals"!

