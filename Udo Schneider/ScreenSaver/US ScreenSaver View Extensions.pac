| package |
package := Package name: 'US ScreenSaver View Extensions'.
package paxVersion: 1;
	basicComment: '$id: US ScreenSaver View Extensions 0.005$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.005'.

package basicScriptAt: #postuninstall put: 'Win32Constants removeKey: #SC_SCREENSAVE.
View 
	compile: ''wmSysCommand: message wParam: wParam lParam: lParam 
	"Private - Handles a WM_SYSCOMMAND message. "

	^nil'''.
package basicScriptAt: #preinstall put: 'Win32Constants at: #SC_SCREENSAVE put: 16rF140'.

package classNames
	add: #ScreensaverEvent;
	yourself.

package methodNames
	add: #DesktopView -> #startScreensaver;
	add: #View -> #onScreensaverStarting:;
	add: #View -> #wmSysCommand:wParam:lParam:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

WindowsEvent subclass: #ScreensaverEvent
	instanceVariableNames: 'stopScreensaver'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!DesktopView methodsFor!

startScreensaver
^View foreground sendMessage: WM_SYSCOMMAND wParam: SC_SCREENSAVE lParam: 0! !
!DesktopView categoriesFor: #startScreensaver!public! !

!View methodsFor!

onScreensaverStarting: anScreensaverEvent 
	"The screensaver is about to be started.
	This is our opportunity to prevent it from being started.
	Return non-nil/0 to prevent the screensaver from running."

	DesktopView current trigger: #onScreensaverStarting: with: anScreensaverEvent.
	^anScreensaverEvent getStopScreensaver ifTrue: [1] ifFalse: [nil]!

wmSysCommand: message wParam: wParam lParam: lParam 
	"Private - Handles a WM_SYSCOMMAND message. "

	^wParam = SC_SCREENSAVE ifTrue: [



self onScreensaverStarting: (ScreensaverEvent 
				handle: handle
				message: message
				wParam: wParam
				lParam: lParam)] ifFalse: [nil]! !
!View categoriesFor: #onScreensaverStarting:!public! !
!View categoriesFor: #wmSysCommand:wParam:lParam:!event handling-win32!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

ScreensaverEvent guid: (GUID fromString: '{CBE03C5B-512C-46F6-BB9B-48B2D7C74B19}')!
ScreensaverEvent comment: ''!
!ScreensaverEvent categoriesForClass!Unclassified! !
!ScreensaverEvent methodsFor!

getStopScreensaver
stopScreensaver isNil ifTrue: [stopScreensaver := false ].
	^stopScreensaver!

setStopScreensaver: aBoolean
stopScreensaver := aBoolean!

stopScreensaver
stopScreensaver := true.! !
!ScreensaverEvent categoriesFor: #getStopScreensaver!public! !
!ScreensaverEvent categoriesFor: #setStopScreensaver:!public! !
!ScreensaverEvent categoriesFor: #stopScreensaver!public! !

"Binary Globals"!

