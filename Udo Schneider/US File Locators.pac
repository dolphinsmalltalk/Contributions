| package |
package := Package name: 'US File Locators'.
package paxVersion: 1;
	basicComment: '$id: US File Locators 0.015$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.015'.


package classNames
	add: #PackageFolderRelativeFileLocator;
	add: #SpecialFolderRelativeFileLocator;
	yourself.

package methodNames
	add: #SessionManager -> #desktopDirectory;
	add: 'FileLocator class' -> #desktopRelative;
	add: 'FileLocator class' -> #myDocumentsRelative;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Dolphin Harbor\DH Shell Core';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

RelativeFileLocator subclass: #PackageFolderRelativeFileLocator
	instanceVariableNames: 'packageName folder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RelativeFileLocator subclass: #SpecialFolderRelativeFileLocator
	instanceVariableNames: 'csidl'
	classVariableNames: 'Current'
	poolDictionaries: 'ShellCSIDLConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!FileLocator class methodsFor!

desktopRelative
	"Answers an instance of the receiver that can be used to locate external files
	relative to Desktop directory."

	^SpecialFolderRelativeFileLocator desktop!

myDocumentsRelative
	"Answers an instance of the receiver that can be used to locate external files
	relative to Desktop directory."

	^SpecialFolderRelativeFileLocator myDocuments! !
!FileLocator class categoriesFor: #desktopRelative!accessing!public! !
!FileLocator class categoriesFor: #myDocumentsRelative!accessing!public! !

!SessionManager methodsFor!

desktopDirectory
	"Answer the path of the Windows 'Desktop' directory on the host computer for the current user."

	^ShellLibrary default getSpecialFolderLocation: 0! !
!SessionManager categoriesFor: #desktopDirectory!accessing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

PackageFolderRelativeFileLocator guid: (GUID fromString: '{2FF3F6A0-964D-42B5-B8BA-21759A24C93C}')!
PackageFolderRelativeFileLocator comment: ''!
!PackageFolderRelativeFileLocator categoriesForClass!Unclassified! !
!PackageFolderRelativeFileLocator methodsFor!

basePath
	^(self package ifNil: [super basePath] ifNotNil: [:pkg | pkg path]) , folder
	
	
!

package
	"Answer the <Package> who's folder is used as the base path, or nil if the package is not loaded."

	^Smalltalk at: #Package ifPresent: [:class | class manager packageNamed: packageName ifNone: []]!

packageName
	"Answer the <readableString> name of the Package who's folder is used as the base path."

	^packageName!

setFolder: aFolder
folder := aFolder!

setPackageName: aString 
	packageName := aString! !
!PackageFolderRelativeFileLocator categoriesFor: #basePath!accessing!public! !
!PackageFolderRelativeFileLocator categoriesFor: #package!accessing!public! !
!PackageFolderRelativeFileLocator categoriesFor: #packageName!accessing!public! !
!PackageFolderRelativeFileLocator categoriesFor: #setFolder:!private! !
!PackageFolderRelativeFileLocator categoriesFor: #setPackageName:!initializing!private! !

!PackageFolderRelativeFileLocator class methodsFor!

package: aPackage folder: aFolder
	"Answer a new instance of the receiver for locating files relative to the folder containing
	the specified <Package>. Should the package be unloaded subsequently, then the instance will
	behave as if it were an ,ImageRelativeFileLocator>."

	^self packageNamed: aPackage name folder: aFolder!

packageNamed: aString folder: aFolder 
	"Answer a new instance of the receiver for locating files relative to the folder containing
	the named package. If the package is not loaded, then the instance will behave as if it were
	an <ImageRelativeFileLocator>."

	^(super new)
		setPackageName: aString;
		setFolder: aFolder;
		yourself! !
!PackageFolderRelativeFileLocator class categoriesFor: #package:folder:!instance creation!public! !
!PackageFolderRelativeFileLocator class categoriesFor: #packageNamed:folder:!instance creation!public! !

SpecialFolderRelativeFileLocator guid: (GUID fromString: '{0D08EFB6-61C6-4D37-A3F4-A3FD19766948}')!
SpecialFolderRelativeFileLocator comment: ''!
!SpecialFolderRelativeFileLocator categoriesForClass!Unclassified! !
!SpecialFolderRelativeFileLocator methodsFor!

basePath
^ShellFolderLibrary default pathFromCSIDL: csidl!

setCSIDL: anInteger
csidl := anInteger! !
!SpecialFolderRelativeFileLocator categoriesFor: #basePath!public! !
!SpecialFolderRelativeFileLocator categoriesFor: #setCSIDL:!private! !

!SpecialFolderRelativeFileLocator class methodsFor!

csidl: anInteger 
"See http://msdn2.microsoft.com/en-us/library/bb762494.aspx for a list of CSIDLs"
	^(self new)
		setCSIDL: anInteger;
		yourself!

desktop

^self csidl: CSIDL_DESKTOP!

favorites
	^self csidl: CSIDL_FAVORITES!

myDocuments
	^self csidl: CSIDL_PERSONAL!

myPictures
	^self csidl: CSIDL_MYPICTURES!

programFiles
	^self csidl: CSIDL_PROGRAM_FILES!

windows
	^self csidl: CSIDL_WINDOWS! !
!SpecialFolderRelativeFileLocator class categoriesFor: #csidl:!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #desktop!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #favorites!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #myDocuments!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #myPictures!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #programFiles!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #windows!*-in class package!instance creation!public! !

"Binary Globals"!

