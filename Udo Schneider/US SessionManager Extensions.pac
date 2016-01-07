| package |
package := Package name: 'US SessionManager Extensions'.
package paxVersion: 1;
	basicComment: '$id: US SessionManager Extensions 0.008$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.008'.


package methodNames
	add: #GUISessionManager -> #defaultResLibPath;
	add: #Object -> #sessionManager;
	add: #SessionManager -> #isTaskManagerDisabled;
	add: #SessionManager -> #isTaskManagerDisabled:;
	add: 'GUISessionManager class' -> #logError:message:;
	add: 'GUISessionManager class' -> #logErrorMessage:;
	add: 'Object class' -> #sessionManager;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!GUISessionManager methodsFor!

defaultResLibPath
	"Answer the path of the development resource library. 'self argv first' may not 
	include the full path to the executable if started from the command line"


	^self imageFileName! !
!GUISessionManager categoriesFor: #defaultResLibPath!constants!must not strip!public! !

!GUISessionManager class methodsFor!

logError: anException message: aString 
	MessageBox errorMsg: aString caption: self productName.
	SessionManager current isRuntime 
		ifTrue: 
			[SessionManager current logError: anException.
			VMLibrary default crashDump: aString.
			SessionManager current quit]
		ifFalse: [anException signal]!

logErrorMessage: aString 
	MessageBox errorMsg: aString caption: self productName.
	SessionManager current isRuntime 
		ifTrue: 
			[VMLibrary default crashDump: aString.
			SessionManager current quit]
		ifFalse: [Error signal: aString]! !
!GUISessionManager class categoriesFor: #logError:message:!public! !
!GUISessionManager class categoriesFor: #logErrorMessage:!public! !

!Object methodsFor!

sessionManager
	^self class sessionManager! !
!Object categoriesFor: #sessionManager!public! !

!Object class methodsFor!

sessionManager
	^SessionManager current isRuntime 
		ifTrue: [SessionManager current class]
		ifFalse: [[self owningPackage imageStripper runtimeSessionManagerClass] on: Error do: [:ex | nil]]! !
!Object class categoriesFor: #sessionManager!public! !

!SessionManager methodsFor!

isTaskManagerDisabled
	| key |
	key := RegKey userRoot at: 'Software\Microsoft\Windows\CurrentVersion\Policies\System'.
	^key subValues includesKey: 'DisableTaskMgr'!

isTaskManagerDisabled: aBoolean
	| key |
	key := RegKey userRoot at: 'Software\Microsoft\Windows\CurrentVersion\Policies\System'.
	aBoolean ifTrue: [key subValues at: 'DisableTaskMgr' put: 1.] ifFalse: [key subValues removeKey: 'DisableTaskMgr' ifAbsent: [].]
	

! !
!SessionManager categoriesFor: #isTaskManagerDisabled!public! !
!SessionManager categoriesFor: #isTaskManagerDisabled:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

