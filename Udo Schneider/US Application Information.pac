| package |
package := Package name: 'US Application Information'.
package paxVersion: 1;
	basicComment: '$id: US Application Information 0.025$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 31.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.025'.

package basicScriptAt: #preinstall put: '| softPrerequisites |
softPrerequisites := #(''US Package Extensions'' ''US STS Extensions'').
softPrerequisites 
	do: [:eachPackageName | (StsManager current getPackageEditionsFor: eachPackageName) first load]'.

package methodNames
	add: #SessionManager -> #applicationIcon;
	add: #SessionManager -> #applicationName;
	add: 'SessionManager class' -> #applicationIcon;
	add: 'SessionManager class' -> #applicationIconId;
	add: 'SessionManager class' -> #author;
	add: 'SessionManager class' -> #comments;
	add: 'SessionManager class' -> #email;
	add: 'SessionManager class' -> #fileDescription;
	add: 'SessionManager class' -> #fullCopyright;
	add: 'SessionManager class' -> #fullDescription;
	add: 'SessionManager class' -> #homepage;
	add: 'SessionManager class' -> #legalCopyright;
	add: 'SessionManager class' -> #productName;
	add: 'SessionManager class' -> #projectEditionVersionString;
	add: 'SessionManager class' -> #projectEditonName;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!SessionManager methodsFor!

applicationIcon
^self class applicationIcon!

applicationName
	"Answer the application name. Use the applications executable name."

	^self class productName! !
!SessionManager categoriesFor: #applicationIcon!*-not in class package!public! !
!SessionManager categoriesFor: #applicationName!constants!public! !

!SessionManager class methodsFor!

applicationIcon
	^SessionManager current isRuntime 
		ifTrue: [Icon fromId: SessionManager applicationIconId]
		ifFalse: [self owningPackage imageStripper applicationIcon]!

applicationIconId ^ '!!APPLICATION'!

author
^'Udo Schneider'!

comments^ 'Powered by Dolphin Smalltalk'!

email
^nil!

fileDescription
^self productName!

fullCopyright
^self legalCopyright
	!

fullDescription
^self fileDescription
!

homepage
^nil!

legalCopyright
	^self author , ' ' ,Date today year displayString!

productName  ^'A Dolphin X6 ToGo Application'!

projectEditionVersionString
	SessionManager current isRuntime 
		ifTrue: 
			[(VersionResource forModule: 0) stringTables 
				do: [:each | (each includesKey: 'ProjectEditionVersion') ifTrue: [^each at: 'ProjectEditionVersion']].
			^'']
		ifFalse: [^(self owningPackage currentProjectEdition: self projectEditonName) versionDescriptor]!

projectEditonName
^self subclassResponsibility! !
!SessionManager class categoriesFor: #applicationIcon!*-not in class package!public! !
!SessionManager class categoriesFor: #applicationIconId!*-not in class package!public! !
!SessionManager class categoriesFor: #author!*-not in class package!public! !
!SessionManager class categoriesFor: #comments!public! !
!SessionManager class categoriesFor: #email!*-in class package!public! !
!SessionManager class categoriesFor: #fileDescription!*-not in class package!public! !
!SessionManager class categoriesFor: #fullCopyright!*-not in class package!public! !
!SessionManager class categoriesFor: #fullDescription!*-not in class package!public! !
!SessionManager class categoriesFor: #homepage!*-in class package!public! !
!SessionManager class categoriesFor: #legalCopyright!*-not in class package!public! !
!SessionManager class categoriesFor: #productName!public! !
!SessionManager class categoriesFor: #projectEditionVersionString!*-not in class package!public! !
!SessionManager class categoriesFor: #projectEditonName!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

