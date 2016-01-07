| package |
package := Package name: 'US ScreenSaver'.
package paxVersion: 1;
	basicComment: '$id: US ScreenSaver 0.043$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.043'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAzIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAABSAAAADgAAAFVTIFNjcmVlblNhdmVyUgAA
AAAAAACaAAAAsAEAAFIAAAAZAAAAU2NyZWVuc2F2ZXJTZXNzaW9uTWFuYWdlcu+/JQAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA').

package classNames
	add: #ScreensaverImageStripper;
	add: #ScreensaverSessionManager;
	add: #ScreensaverShell;
	add: #ScreensaverShellView;
	add: #ScreensaverShield;
	add: #ScreensaverShieldView;
	yourself.

package methodNames
	add: #Presenter -> #bePreview;
	add: #Presenter -> #createPreviewView:;
	add: #View -> #isPreview;
	add: 'Presenter class' -> #createPreviewView:in:on:;
	add: 'Presenter class' -> #show:asScreensaverOn:;
	add: 'ShellView class' -> #makeResource:inClass:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\System\Trace\Debug Trace Stream';
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	add: '..\..\..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	add: '..\US ImageStripper';
	add: '..\Windows Resources\US Resource Update';
	add: '..\GUI\US View Extensions';
	yourself).

package setManualPrerequisites: #(
	'US View Extensions').

package!

"Class Definitions"!

USImageStripper subclass: #ScreensaverImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #ScreensaverShield
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #ScreensaverShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #ScreensaverSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #ScreensaverShieldView
	instanceVariableNames: 'oldMousePosition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShellView subclass: #ScreensaverShellView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Presenter methodsFor!

bePreview
	self view propertyAt: #isPreview put: true!

createPreviewView: aResourceNameString 
	"Private - Creates and connects a view for the receiver from the resource identified
	by aResourceNameString. Answers the new view created"

	| parentView surrogateShellView newView |
	parentView := self parentPresenter 
				ifNil: 
					["The receiver has no parent presenter so create a basic
					ShellView to hold its view"
					surrogateShellView := ShellView new create.
					parentView := surrogateShellView.
					parentView
						layoutManager: GridLayout new;
						caption: self printString;
						largeIcon: self icon;
						show.
					parentView]
				ifNotNil: [:parent | parent view].

	"Create the new view"
	newView := self class loadViewResource: aResourceNameString inContext: parentView.
	newView propertyAt: #isPreview put: true.
	self view: newView.
	surrogateShellView notNil ifTrue: [self onViewOpened].
	self show.
	^newView! !
!Presenter categoriesFor: #bePreview!public! !
!Presenter categoriesFor: #createPreviewView:!operations!private! !

!Presenter class methodsFor!

createPreviewView: aResourceNameString in: aCompositePresenter on: aModel 
	"Answers an instance of the receiver created as a sub-presenter of aCompositePresenter
	and wired up to a view identified by the resource name aResourceNameString. The new
	presenter is to be connected to aModel. It is assumed that, at this stage, aCompositePresenter
	is already opened in a view. For this reason we must force an #onViewOpened message to
	the newly created presenter"

	| newOne |
	newOne := aCompositePresenter add: (self on: aModel).
	newOne createPreviewView: aResourceNameString.
	^newOne
		onViewOpened;
		yourself!

show: aString asScreensaverOn: aModel 
	| shell screensaver shield |
	shell := ScreensaverShell create.
	shell view coverParent.
	screensaver := self 
				create: aString
				in: shell
				on: aModel.
	screensaver view coverParent.
	shield := ScreensaverShield createIn: shell.
	(shield view)
		"coverParent;"
		isTransparent: true;
		zOrderTop;
		setFocus.
	shell show.
	^screensaver! !
!Presenter class categoriesFor: #createPreviewView:in:on:!*-not in class package!instance creation!public! !
!Presenter class categoriesFor: #show:asScreensaverOn:!public! !

!ShellView class methodsFor!

makeResource: aStringName inClass: aClass 
	"Private - Save and instance of the receiver as a default writable ViewResource 
	called aString owned by aClass."

	| resID view |
	view := View desktop addSubView: self new.
	(resID := ResourceIdentifier class: aClass name: aStringName) assign: view literalStoreArray.
	View desktop destroy.
	^resID! !
!ShellView class categoriesFor: #makeResource:inClass:!private! !

!View methodsFor!

isPreview

	^self propertyAt: #isPreview ifAbsent: [false]! !
!View categoriesFor: #isPreview!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ScreensaverImageStripper guid: (GUID fromString: '{769CC154-5962-46A2-A4CA-93C42D28CA44}')!
ScreensaverImageStripper comment: ''!
!ScreensaverImageStripper categoriesForClass!Unclassified! !
!ScreensaverImageStripper methodsFor!

stringTable: offset 
	"Answer the <StringTable> describing the string table to be built into	
	the deployed application/dll."

	| resourceLibrary stringTable |
	resourceLibrary := ExternalResourceLibrary open: self executablePath.
	stringTable := StringTableResource fromId: offset in: resourceLibrary.
	resourceLibrary close.
	^stringTable!

updateScreenSaverDescription: stringTable 
	stringTable at: 1 put: self runtimeSessionManagerClass screensaverName asUnicodeString asByteArray!

updateStringTableResource: anExternalHandle 
	"Update the String Table resource of the exe/dll stub through the supplied resource update
	handle."

	| stringTable |
	stringTable := self stringTable: 1 resourceIdFromOffset.
	self updateScreenSaverDescription: stringTable.
	stringTable updateResource: anExternalHandle.
	^super updateStringTableResource: anExternalHandle! !
!ScreensaverImageStripper categoriesFor: #stringTable:!accessing!private! !
!ScreensaverImageStripper categoriesFor: #updateScreenSaverDescription:!operations!public! !
!ScreensaverImageStripper categoriesFor: #updateStringTableResource:!operations!public! !

ScreensaverShield guid: (GUID fromString: '{7E909D9E-AB70-48F1-A8C7-B14589718422}')!
ScreensaverShield comment: ''!
!ScreensaverShield categoriesForClass!Unclassified! !
!ScreensaverShield class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ScreensaverShieldView)  98 13 0 0 98 2 8 1140850688 65 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 416 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 674 201 201 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 100 0 0 0 100 0 0 0] 98 0 674 193 193 0 27 )! !
!ScreensaverShield class categoriesFor: #resource_Default_view!public!resources-views! !

ScreensaverShell guid: (GUID fromString: '{AC4BA448-9695-4B1D-B375-881ABD43C0E6}')!
ScreensaverShell comment: ''!
!ScreensaverShell categoriesForClass!Unclassified! !
!ScreensaverShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ScreensaverShellView)  98 27 0 0 98 2 8 2147483648 131329 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 39 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 0 234 256 592 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  3839 21 754 2561 1601 416 690 8 #updateMenuBar 592 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 127 12 0 0 42 3 0 0] 98 0 754 193 193 0 27 )! !
!ScreensaverShell class categoriesFor: #resource_Default_view!public!resources-views! !

ScreensaverSessionManager guid: (GUID fromString: '{2BE4A7F9-C057-416F-9289-22B99712D7E5}')!
ScreensaverSessionManager comment: ''!
!ScreensaverSessionManager categoriesForClass!Unclassified! !
!ScreensaverSessionManager methodsFor!

commandLineArguments
	"Returns argv without the binary name and tokens seperated by collon as independent tokens"

	| arguments |
	arguments := ReadWriteStream on: Array new.
	self argv  allButFirst do: [:each | arguments nextPutAll: (each asLowercase subStrings: $:)].
	^arguments contents!

configuration
	^self class configuration!

endScreensaver
	Trace
		nextPutAll: 'endScreensaver';
		flush.
	SessionManager current isRuntime ifTrue: [	SessionManager  current inputState queueDeferredAction: [ Trace
		nextPutAll: 'quit';
		flush. SessionManager current quit]]!

ensureUIIsstarted
	#USToDo.
	ScreensaverShellView show close!

main
	| arguments |
	arguments := self commandLineArguments.
	arguments isEmpty 
		ifTrue: [self showConfigurationDialog]
		ifFalse: [self parseArguments: arguments]!

notifyNoSettings

	MessageBox notify: 'This screensaver has no options' caption: self class screensaverName!

observe: aPresenter 
	
	aPresenter 
		when: #viewClosed
		send: #endScreensaver
		to: self.
	(aPresenter view)
		when: #endScreensaver
			send: #endScreensaver
			to: self;
		when: #viewClosed
			send: #endScreensaver
			to: self!

parseArguments: arguments 

	(#('c' '-c' '/c') includes: arguments first) ifTrue: [^self parseConfigurationArguments: arguments].
	(#('p' '-p' '/p') includes: arguments first) ifTrue: [^self parsePreviewArguments: arguments].
	self showFullscreen!

parseConfigurationArguments: arguments 
	
	
	arguments size > 1 
		ifTrue: [self showConfigurationDialogModalTo: (View fromHandle: arguments second asNumber)]
		ifFalse: [self showConfigurationDialog]!

parsePreviewArguments: arguments 
	self showPreviewIn: (Presenter new view: (View fromHandle: arguments second asNumber); yourself)!

showConfigurationDialog
^self showConfigurationDialogModalTo: nil!

showConfigurationDialogModalTo: aView 
	| configuration dialog |
		configuration := self configuration.
	dialog := self class configurationDialog.
	configuration notNil 
		ifTrue: 
			[dialog notNil 
				ifTrue: 
					[(configuration := aView 
								ifNil: [dialog showModalOn: configuration]
								ifNotNil: [:view | dialog showModalOn: configuration to: view topShellView]) notNil 
						ifTrue: [configuration  saveSettings ]]
				ifFalse: [self notifyNoSettings]]
		ifFalse: [self notifyNoSettings].
	self endScreensaver!

showFullscreen
	| screensaverPresenter |

	screensaverPresenter := self class fullscreenPresenter show: self class fullscreenViewName
				asScreensaverOn: self configuration.
	self observe: screensaverPresenter.
	^screensaverPresenter!

showPreviewIn: aPresenter 
	| screensaver |
	self ensureUIIsstarted.
	screensaver := (self class previewPresenter 
				createPreviewView: self class previewViewName
				in: aPresenter
				on: self configuration) yourself.
	screensaver view coverParent.
	self observe: screensaver! !
!ScreensaverSessionManager categoriesFor: #commandLineArguments!private! !
!ScreensaverSessionManager categoriesFor: #configuration!public! !
!ScreensaverSessionManager categoriesFor: #endScreensaver!private! !
!ScreensaverSessionManager categoriesFor: #ensureUIIsstarted!private! !
!ScreensaverSessionManager categoriesFor: #main!public! !
!ScreensaverSessionManager categoriesFor: #notifyNoSettings!private! !
!ScreensaverSessionManager categoriesFor: #observe:!private! !
!ScreensaverSessionManager categoriesFor: #parseArguments:!private! !
!ScreensaverSessionManager categoriesFor: #parseConfigurationArguments:!private! !
!ScreensaverSessionManager categoriesFor: #parsePreviewArguments:!private! !
!ScreensaverSessionManager categoriesFor: #showConfigurationDialog!*-in class package!public! !
!ScreensaverSessionManager categoriesFor: #showConfigurationDialogModalTo:!*-in class package!public! !
!ScreensaverSessionManager categoriesFor: #showFullscreen!*-in class package!public! !
!ScreensaverSessionManager categoriesFor: #showPreviewIn:!*-in class package!public! !

!ScreensaverSessionManager class methodsFor!

companyName
	^self subclassResponsibility!

configuration
	^nil!

configurationDialog
	^nil!

fullscreenPresenter
	^self screensaverPresenter!

fullscreenViewName
	^self screensaverViewName!

imageExtension
	"Answer the suffix for an executable image file"

	^'scr'!

installKeyName
	"Private - Answer the name of the base installation registry key."

	^'Software\' , self companyName!

previewPresenter
	^self screensaverPresenter!

previewViewName
	^self screensaverViewName!

screensaverName
	"All Screensavers are required to have a Description string of no more than 25 chars for display by the Control Panel's Desktop applet."

	^self fileDescription!

screensaverPresenter
	self subclassResponsibility!

screensaverViewName
	^View defaultView!

showConfigurationDialogModalTo: aShellView 
	^self basicNew showConfigurationDialogModalTo: aShellView !

showFullscreen
	^self basicNew showFullscreen!

showPreviewIn: aPresenter 
	^self basicNew showPreviewIn: aPresenter !

userSettingsKey
	"Private - Answer the <RegKey> under which user specific settings should be stored."

	^self userSettingsRootKey createKey: self screensaverName!

userSettingsRootKey
	^RegKey userRoot createKey: self installKeyName! !
!ScreensaverSessionManager class categoriesFor: #companyName!accessing!public! !
!ScreensaverSessionManager class categoriesFor: #configuration!public! !
!ScreensaverSessionManager class categoriesFor: #configurationDialog!public! !
!ScreensaverSessionManager class categoriesFor: #fullscreenPresenter!public! !
!ScreensaverSessionManager class categoriesFor: #fullscreenViewName!public! !
!ScreensaverSessionManager class categoriesFor: #imageExtension!public! !
!ScreensaverSessionManager class categoriesFor: #installKeyName!accessing!private! !
!ScreensaverSessionManager class categoriesFor: #previewPresenter!public! !
!ScreensaverSessionManager class categoriesFor: #previewViewName!public! !
!ScreensaverSessionManager class categoriesFor: #screensaverName!public! !
!ScreensaverSessionManager class categoriesFor: #screensaverPresenter!public! !
!ScreensaverSessionManager class categoriesFor: #screensaverViewName!public! !
!ScreensaverSessionManager class categoriesFor: #showConfigurationDialogModalTo:!public! !
!ScreensaverSessionManager class categoriesFor: #showFullscreen!public! !
!ScreensaverSessionManager class categoriesFor: #showPreviewIn:!public! !
!ScreensaverSessionManager class categoriesFor: #userSettingsKey!accessing!private! !
!ScreensaverSessionManager class categoriesFor: #userSettingsRootKey!accessing!private! !

ScreensaverShieldView guid: (GUID fromString: '{8412202E-1FAD-4042-99CD-94C21736062B}')!
ScreensaverShieldView comment: ''!
!ScreensaverShieldView categoriesForClass!Unclassified! !
!ScreensaverShieldView methodsFor!

defaultWindowExStyle
	"Private - Answer the default extended window creation style."

	^super defaultWindowExStyle bitOr: WS_EX_TRANSPARENT!

endScreensaver
self parentView close.
	self presenter trigger: #onEndScreensaver!

maxDelta
	^5@5!

onEraseRequired: aColorEvent 

	^true!

onKeyPressed: aKeyEvent 
	self endScreensaver.
	^super onKeyPressed: aKeyEvent!

onMouseMoved: aMouseEvent 
	oldMousePosition notNil 
		ifTrue: 
			[((Rectangle center: oldMousePosition extent: self maxDelta) containsPoint: aMouseEvent position) 
				ifFalse: [self endScreensaver]].
	oldMousePosition := aMouseEvent position.
	^super onMouseMoved: aMouseEvent! !
!ScreensaverShieldView categoriesFor: #defaultWindowExStyle!public! !
!ScreensaverShieldView categoriesFor: #endScreensaver!public! !
!ScreensaverShieldView categoriesFor: #maxDelta!public! !
!ScreensaverShieldView categoriesFor: #onEraseRequired:!public! !
!ScreensaverShieldView categoriesFor: #onKeyPressed:!public! !
!ScreensaverShieldView categoriesFor: #onMouseMoved:!public! !

!ScreensaverShieldView class methodsFor!

icon
^Icon fromId: 'SHIELD.ICO'! !
!ScreensaverShieldView class categoriesFor: #icon!public! !

ScreensaverShellView guid: (GUID fromString: '{545C96F8-1A24-4C15-967C-562403713431}')!
ScreensaverShellView comment: ''!
!ScreensaverShellView categoriesForClass!Unclassified! !
!ScreensaverShellView methodsFor!

defaultWindowExStyle

	^super defaultWindowExStyle bitOr: WS_EX_TOOLWINDOW!

defaultWindowStyle
	"Private - Answer the default basic window creation style"

	^##(WS_POPUP)!

onCreated: anEvent 

	SessionManager  current isRuntime ifTrue:[Cursor hide.].
	^super onCreated: anEvent!

onDestroyed

SessionManager current isRuntime ifTrue: [	Cursor show.].
	^super onDestroyed!

onEraseRequired: aColorEvent 

	^true! !
!ScreensaverShellView categoriesFor: #defaultWindowExStyle!constants!private! !
!ScreensaverShellView categoriesFor: #defaultWindowStyle!constants!private! !
!ScreensaverShellView categoriesFor: #onCreated:!public! !
!ScreensaverShellView categoriesFor: #onDestroyed!public! !
!ScreensaverShellView categoriesFor: #onEraseRequired:!public! !

"Binary Globals"!

