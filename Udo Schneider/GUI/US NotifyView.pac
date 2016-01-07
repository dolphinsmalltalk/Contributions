| package |
package := Package name: 'US NotifyView'.
package paxVersion: 1;
	basicComment: '$id: US NotifyView 1.128$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 01.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This package allows you to use the Windows Notification Area ("Tray Bar") from your Dolphin Applications.

Please note that the use of NotifyView is deprecated. Use NotifyAreaIcon instead.

"Create your instance. No need to hassle with Views. Simply create an instance"
| notifyIcon notifyIcons |
notifyIcon := NotifyAreaIcon icon: (Icon fromId: ''!!APPLICATION'') message: ''My Application''.
"If you want your icon to be displayed, simply send #show"
notifyIcon show.
"You can change the tip message and the icon any time by using #icon: and #message: . If the icon is currently visible it''s automatically updated."
notifyIcon icon: Icon defaultApplication.
notifyIcon message: ''Changed message''.
"You can use this i.e. to display animations"
Icon allInstances asSet do: 
		[:each | 
		notifyIcon
			icon: each;
			message: each identifier displayString].
"The icon triggers several messages which you can intercept using the regular #when:send:to: mechanism"
notifyIcon 
	when: #leftButtonPressed
	send: #value
	to: [Sound beep].
notifyIcon 
	when: #leftButtonDoubleClicked
	send: #value
	to: 
		[Sound beep.
		(Delay forMilliseconds: 500) wait.
		Sound beep].
notifyIcon 
	when: #rightButtonPressed
	send: #value
	to: [Sound bell].
notifyIcon 
	when: #rightButtonDoubleClicked
	send: #value
	to: 
		[Sound bell.
		(Delay forMilliseconds: 500) wait.
		Sound bell].
"If you don''t need the icon anymore (e.g. during #onViewClosed) you can simply hide it or nil the variable and let GC take care of it"
notifyIcon hide.
notifyIcon := nil.

""
"You can even generate a whole bunch of instances"
notifyIcons := Icon allInstances asSet 
			collect: [:each | NotifyAreaIcon icon: each message: each identifier displayString].
"If you want to stress test your system, show /all/ icons"
notifyIcons do: [:each | each show].
"Please note, that we don''t hide icons here. We simply nil the instance and let GC take care of it"
notifyIcons := nil.
"You can also force a GC"
MemoryManager current collectGarbage.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.128'.

package basicScriptAt: #preinstall put: '| wmu |
wmu := Win32Constants at: ''WM_USER''.
Win32Constants
	at: ''NIF_INFO'' put: 16r00000010;
	at: ''NIIF_NONE'' put: 16r00000000;
	at: ''NIIF_INFO'' put: 16r00000001;
	at: ''NIIF_WARNING'' put: 16r00000002;
	at: ''NIIF_ERROR'' put: 16r00000003;
	at: ''NIIF_USER'' put: 16r00000004;
	at: ''NIIF_NOSOUND'' put: 16r00000010;
	at: ''NOTIFYICON_VERSION'' put: 3;
	at: ''NOTIFYICON_VERSION4'' put: 4;
	at: ''NIM_SETVERSION'' put: 16r04;
	at: ''NIN_BALLOONSHOW'' put: wmu + 2;
	at: ''NIN_BALLOONHIDE'' put: wmu + 3;
	at: ''NIN_BALLOONTIMEOUT'' put: wmu + 4;
	at: ''NIN_BALLOONUSERCLICK'' put: wmu + 5;
	at: ''NIN_SELECT'' put: wmu + 0;
	at: ''NINF_KEY'' put: 1;
	at: ''NIN_KEYSELECT'' put: wmu + 1	"NIN_SELECT | NINF_KEY"'.

package classNames
	add: #NotifyAreaBalloon;
	add: #NotifyAreaIcon;
	add: #NotifyAreaView;
	add: #NOTIFYICONDATA2;
	yourself.

package methodNames
	add: #ShellLibrary -> #shell_NotifyIcon:pnid:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\Tooltips\Dolphin Tooltips';
	add: '..\US Runtime Patches';
	add: '..\..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package setManualPrerequisites: #(
	'US Runtime Patches').

package!

"Class Definitions"!

Object subclass: #NotifyAreaIcon
	instanceVariableNames: 'identifier message icon isVisible'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Win32Structure subclass: #NOTIFYICONDATA2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MessageBoxAbstract subclass: #NotifyAreaBalloon
	instanceVariableNames: 'iconId icon timeout'
	classVariableNames: 'IconIds'
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
View subclass: #NotifyAreaView
	instanceVariableNames: 'icons version'
	classVariableNames: 'DefaultInstance NotifyMessageMap WM_NOTIFYAREACALLBACK WM_TASKBARCREATED'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ShellLibrary methodsFor!

shell_NotifyIcon: dwMessage pnid: pnid 
	"WINSHELLAPI BOOL WINAPI Shell_NotifyIcon(
		DWORD dwMessage, 
		PNOTIFYICONDATA pnid 
	);"

	<stdcall: bool Shell_NotifyIcon dword NOTIFYICONDATA2*>
	^self invalidCall! !
!ShellLibrary categoriesFor: #shell_NotifyIcon:pnid:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

NotifyAreaIcon guid: (GUID fromString: '{6251907A-1962-47EC-B593-90D8664A2664}')!
NotifyAreaIcon comment: ''!
!NotifyAreaIcon categoriesForClass!Unclassified! !
!NotifyAreaIcon methodsFor!

balloon
^NotifyAreaBalloon new owner: self; yourself!

finalize

	self isVisible ifTrue: [self hide]!

hide
	self isVisible 
		ifTrue: 
			[NotifyAreaView default hide: self.
			isVisible := false. self beUnfinalizable ]!

icon
	^icon!

icon: anIcon 
	icon = anIcon 
		ifFalse: 
			[icon := anIcon.
			self update]!

identifier
	^identifier!

identifier: anObject
	identifier := anObject!

initialize
	identifier := nil.
	message := 'Notify message'.
	
	icon := Icon defaultApplication.
	isVisible := false!

isVisible
	^isVisible!

message
	^message!

message: aString 
message = aString ifFalse: [message  := aString . self update]!

show
	self isVisible 
		ifFalse: 
			[NotifyAreaView default add: self.
			isVisible := true. self beFinalizable ]!

showBalloonIcon: anIcon title: title message: aString flags: dwInfoFlags timeout: anInteger playSound: aBoolean 
	self isVisible 
		ifTrue: 
			[NotifyAreaView default 
				showBalloon: self
				icon: anIcon
				title: title
				message: aString
				flags: dwInfoFlags
				timeout: anInteger
				playSound: aBoolean]!

update
	self isVisible ifTrue: [NotifyAreaView default basicUpdate: self]! !
!NotifyAreaIcon categoriesFor: #balloon!public! !
!NotifyAreaIcon categoriesFor: #finalize!private! !
!NotifyAreaIcon categoriesFor: #hide!operations!public! !
!NotifyAreaIcon categoriesFor: #icon!accessing!public! !
!NotifyAreaIcon categoriesFor: #icon:!accessing!public! !
!NotifyAreaIcon categoriesFor: #identifier!accessing!public! !
!NotifyAreaIcon categoriesFor: #identifier:!accessing!private! !
!NotifyAreaIcon categoriesFor: #initialize!initialization!private! !
!NotifyAreaIcon categoriesFor: #isVisible!accessing!public!testing! !
!NotifyAreaIcon categoriesFor: #message!accessing!public! !
!NotifyAreaIcon categoriesFor: #message:!accessing!public! !
!NotifyAreaIcon categoriesFor: #show!operations!public! !
!NotifyAreaIcon categoriesFor: #showBalloonIcon:title:message:flags:timeout:playSound:!operations!public! !
!NotifyAreaIcon categoriesFor: #update!operations!private! !

!NotifyAreaIcon class methodsFor!

icon: anIcon message: aString 
	^(self new) icon: anIcon;
		message: aString;
		yourself!

new
 ^super new initialize! !
!NotifyAreaIcon class categoriesFor: #icon:message:!instance creation!public! !
!NotifyAreaIcon class categoriesFor: #new!instance creation!public! !

NOTIFYICONDATA2 guid: (GUID fromString: '{8EAA50AF-77A9-4544-B6B8-1A9EFFB83299}')!
NOTIFYICONDATA2 comment: ''!
!NOTIFYICONDATA2 categoriesForClass!Unclassified! !
!NOTIFYICONDATA2 methodsFor!

balloonFlags: anInteger 
	self dwInfoFlags: anInteger.
	self uFlags: (self uFlags bitOr: NIF_INFO)!

balloonMessage: aString 
	self szInfo: aString.
	self uFlags: (self uFlags bitOr: NIF_INFO)!

balloonTimeout: anInteger 
	self uTimeout: anInteger.
	self uFlags: (self uFlags bitOr: NIF_INFO)!

balloonTitle: aString 
	self szInfoTitle: aString.
	self uFlags: (self uFlags bitOr: NIF_INFO)!

dwInfoFlags: anObject
	"Set the receiver's dwInfoFlags field to the value of anObject."

	bytes dwordAtOffset: 484 put: anObject!

dwSize: anObject
	"Set the receiver's dwSize field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

dwState: anObject
	"Set the receiver's dwState field to the value of anObject."

	bytes dwordAtOffset: 152 put: anObject!

dwStateMask: anObject
	"Set the receiver's dwStateMask field to the value of anObject."

	bytes dwordAtOffset: 156 put: anObject!

hIcon: anObject
	"Set the receiver's hIcon field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

hWnd: anObject
	"Set the receiver's hWnd field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

icon: anIconOrHandle 
	"Set the receiver's hIcon field."

	self hIcon: anIconOrHandle asParameter.
	self uFlags: (self uFlags bitOr: NIF_ICON).
	!

message: anIntegerMessageNumber
	"Set the receiver's uCallbackMessage (message sent to window when mouse over
	the icon in the taskbar) field."

	self uCallbackMessage: anIntegerMessageNumber.
	self uFlags: (self uFlags bitOr: NIF_MESSAGE)!

szInfo
	"Answer the receiver's szInfo field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 160)!

szInfo: anObject
	"Set the receiver's szInfo field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (255 * 1).
	anObject replaceBytesOf: bytes from: 161 to: 160 + size startingAt: 1.
	bytes at: size+161 put: 0!

szInfoTitle
	"Answer the receiver's szInfoTitle field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 420)!

szInfoTitle: anObject
	"Set the receiver's szInfoTitle field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (63 * 1).
	anObject replaceBytesOf: bytes from: 421 to: 420 + size startingAt: 1.
	bytes at: size+421 put: 0!

szTip
	"Answer the receiver's szTip field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 24)!

szTip: anObject
	"Set the receiver's szTip field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (127 * 1).
	anObject replaceBytesOf: bytes from: 25 to: 24 + size startingAt: 1.
	bytes at: size+25 put: 0!

tipText: aString
	"Set the receiver's szTip (tip text) field."

	self szTip: aString.
	self uFlags: (self uFlags bitOr: NIF_TIP)!

uCallbackMessage: anObject
	"Set the receiver's uCallbackMessage field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

uFlags
	"Answer the receiver's uFlags field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

uFlags: anObject
	"Set the receiver's uFlags field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

uID: anObject
	"Set the receiver's uID field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

uTimeout: anObject
	"Set the receiver's uTimeout field to the value of anObject."

	bytes dwordAtOffset: 416 put: anObject!

uVersion: anObject
	"Set the receiver's uVersion field to the value of anObject."

	bytes dwordAtOffset: 416 put: anObject! !
!NOTIFYICONDATA2 categoriesFor: #balloonFlags:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #balloonMessage:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #balloonTimeout:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #balloonTitle:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #dwInfoFlags:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #dwSize:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #dwState:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #dwStateMask:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #hIcon:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #hWnd:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #icon:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #message:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #szInfo!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szInfo:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szInfoTitle!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szInfoTitle:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szTip!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szTip:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #tipText:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #uCallbackMessage:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uFlags!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uFlags:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uID:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uTimeout:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uVersion:!**compiled accessors**!public! !

!NOTIFYICONDATA2 class methodsFor!

defineFields
	"Define the fields of the NOTIFYICONDATA2 'structure'.
		self compileDefinition

		struct {
			DWORD cbSize; 
			HWND hWnd; 
			UINT uID; 
			UINT uFlags; 
			UINT uCallbackMessage; 
			HICON hIcon; 
			char szTip[64]; 
		} NOTIFYICONDATA;
	"

	self
		defineField: #dwSize type: DWORDField writeOnly beOverride;
		defineField: #hWnd type: HANDLEField writeOnly;
		defineField: #uID type: DWORDField writeOnly;
		defineField: #uFlags type: DWORDField new;
		defineField: #uCallbackMessage type: DWORDField writeOnly;
		defineField: #hIcon type: HANDLEField writeOnly;
		defineField: #szTip type: (StringField length: 128);
		defineField: #dwState type: DWORDField writeOnly;
		defineField: #dwStateMask type: DWORDField writeOnly;
		defineField: #szInfo type: (StringField length: 256);
		defineField: #uTimeout type: DWORDField writeOnly offset: 416;
		defineField: #uVersion type: DWORDField writeOnly offset: 416;
		defineField: #szInfoTitle type: (StringField length: 64);
		defineField: #dwInfoFlags type: DWORDField writeOnly! !
!NOTIFYICONDATA2 class categoriesFor: #defineFields!initializing!public! !

NotifyAreaBalloon guid: (GUID fromString: '{AC0EB94F-5B9E-4F18-8E9E-B2819D779D5C}')!
NotifyAreaBalloon comment: ''!
!NotifyAreaBalloon categoriesForClass!Kernel-Objects! !
!NotifyAreaBalloon methodsFor!

basicOpen
	owner 
		showBalloonIcon:  icon
		title: self caption
		message: self text
		flags: self iconStyleFlag
		timeout: self timeout
		playSound: false!

icon: anIcon 
	"Set the receiver's icon to be the argument, anIcon. The icon MUST have been loaded from
	resources since the message box does not use the icon directly, but rather it uses its ID
	and module handle to load it again.. Also the icon only seems to be displayed if the
	resource id is an integer."

	icon := anIcon.
	self iconStyle: (icon isNil ifTrue: [#notify] ifFalse: [#user])!

iconStyle
	"Answer the receiver's symbolic icon style name."

	^IconIds keyAtValue: self iconStyleFlag!

iconStyle: aSymbol 
	"Set the receiver's icon style to that named by the argument."

	
	self iconStyleFlag: (IconIds at: aSymbol)!

iconStyleFlag
	"Private - Answer the icon style bits from the receiver's style mask."


	^IconIds at: iconId ifAbsent: [0]!

iconStyleFlag: anInteger 
	"Private - Set the receiver's icon to be that named by the argument."

	iconId := IconIds keyAtValue: anInteger ifAbsent: [0]!

initialize
super initialize.
	iconId := 0.

	timeout := 0.
!

open
	self basicOpen.
	^#ok!

timeout
	"Answer the number of milliseconds for which the bubble will be displayed.
	If <= 0, then the bubble is displayed until clicked or otherwise closed explicitly."

	^timeout!

timeout: anInteger 
	timeout := anInteger! !
!NotifyAreaBalloon categoriesFor: #basicOpen!private! !
!NotifyAreaBalloon categoriesFor: #icon:!accessing!public! !
!NotifyAreaBalloon categoriesFor: #iconStyle!accessing-styles!public! !
!NotifyAreaBalloon categoriesFor: #iconStyle:!accessing-styles!public! !
!NotifyAreaBalloon categoriesFor: #iconStyleFlag!accessing-styles!private! !
!NotifyAreaBalloon categoriesFor: #iconStyleFlag:!accessing-styles!private! !
!NotifyAreaBalloon categoriesFor: #initialize!initializing!private! !
!NotifyAreaBalloon categoriesFor: #open!public!realizing/unrealizing! !
!NotifyAreaBalloon categoriesFor: #timeout!accessing!public! !
!NotifyAreaBalloon categoriesFor: #timeout:!accessing!public! !

!NotifyAreaBalloon class methodsFor!

icon
	^Tooltip icon!

initialize
	"Private - Initialize the class variables of the receiver:
		self initialize
	"

	IconIds := (IdentityDictionary new)
				at: #none put: NIIF_NONE ;
				at: #error put: NIIF_ERROR ;
				
				at: #warning put: NIIF_WARNING ;
			
				at: #notify put: NIIF_INFO ;
				at: #user put: NIIF_USER ;
				shrink;
				yourself! !
!NotifyAreaBalloon class categoriesFor: #icon!constants!development!public! !
!NotifyAreaBalloon class categoriesFor: #initialize!private! !

NotifyAreaView guid: (GUID fromString: '{AEA859AF-B752-4704-93C1-3D891C9E0F67}')!
NotifyAreaView comment: ''!
!NotifyAreaView categoriesForClass!Unclassified! !
!NotifyAreaView methodsFor!

add: aNotifyAreaIcon 
	aNotifyAreaIcon identifier: self nextIconIdentifier.
	icons add: aNotifyAreaIcon.
	self basicAdd: aNotifyAreaIcon!

basicAdd: aNotifyAreaIcon 
	| notifyStruct |
	notifyStruct := (NOTIFYICONDATA2 new)
				hWnd: self handle;
				uID: aNotifyAreaIcon identifier;
				message: WM_NOTIFYAREACALLBACK;
				tipText: aNotifyAreaIcon message;
				icon: aNotifyAreaIcon icon copyExtentSmall;
				yourself.
	ShellLibrary default shell_NotifyIcon: NIM_ADD pnid: notifyStruct!

basicHide: aNotifyAreaIcon 
	| notifyStruct |
	notifyStruct := (NOTIFYICONDATA2 new)
				hWnd: self handle;
				uID: aNotifyAreaIcon identifier;
				yourself.
	ShellLibrary default shell_NotifyIcon: NIM_DELETE pnid: notifyStruct!

basicReAddIcons
	icons do: [:each | each ifNotNil: [:value | self basicAdd: value]]!

basicRemoveIcons
	icons do: [:each | each ifNotNil: [:value | self basicHide: value]]!

basicUpdate: aNotifyAreaIcon 
	| notifyStruct |
	notifyStruct := (NOTIFYICONDATA2 new)
				hWnd: self handle;
				uID: aNotifyAreaIcon identifier;
				message: WM_NOTIFYAREACALLBACK;
				tipText: aNotifyAreaIcon message;
				icon: aNotifyAreaIcon icon copyExtentSmall;
				yourself.
	ShellLibrary default shell_NotifyIcon: NIM_MODIFY pnid: notifyStruct!

dispatchRegistered: registeredId wParam: wParam lParam: lParam 
	"Private - Dispatch the window message"

	registeredId = WM_NOTIFYAREACALLBACK 
		ifTrue: 
			[(NotifyMessageMap includesKey: lParam) 
				ifTrue: 
					["Transcript
						show: 'Known Message ' , (NotifyMessageMap at: lParam);
						cr."
					^self perform: (NotifyMessageMap at: lParam) with: (self iconFromIdentifier: wParam)]
				ifFalse: 
					["Transcript
						show: 'Unknown Message ' , lParam displayString;
						cr"]].
	registeredId = WM_TASKBARCREATED ifTrue: [self onTaskbarCreated].
	^super 
		dispatchRegistered: registeredId
		wParam: wParam
		lParam: lParam!

hide: aNotifyAreaIcon 
	self basicHide: aNotifyAreaIcon.
	icons remove: aNotifyAreaIcon ifAbsent: [].
	aNotifyAreaIcon identifier: nil!

iconFromIdentifier: identifier 
	
	^icons detect: [:each | each identifier = identifier] ifNone: [nil]!

initialize
	super initialize.
	icons := WeakSet new.
	self useWin2000Behavior.
!

nextIconIdentifier
	| iconIds iconId |
	iconIds := icons collect: [:each | each identifier ].
	iconId := 0.
	[iconIds includes: iconId] whileTrue: [iconId  := iconId  +1].
	^iconId!

onBalloonClick: aNotifyAreaIcon 
	aNotifyAreaIcon trigger: #ballonClick!

onBalloonHide
!

onBalloonShow: aNotifyAreaIcon 
	aNotifyAreaIcon trigger: #ballonShow!

onBalloonTimeout: aNotifyAreaIcon 
aNotifyAreaIcon trigger: #ballonTimeout!

onKeySelect: anUndefinedObject 
	!

onLeftButtonDoubleClicked: aNotifiyAreaIcon 
aNotifiyAreaIcon trigger: #leftButtonDoubleClicked!

onLeftButtonPressed: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #leftButtonPressed!

onLeftButtonReleased: aNotifiyAreaIcon 
aNotifiyAreaIcon trigger: #leftButtonReleased!

onMouseMoved: aNotifyAreaIcon 
	aNotifyAreaIcon trigger: #mouseMoved!

onRightButtonDoubleClicked: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #rightButtonDoubleClicked!

onRightButtonPressed: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #rightButtonPressed!

onRightButtonReleased: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #rightButtonReleased!

onSelect: anUndefinedObject 
!

onShowContextMenu: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #showContextMenu!

onStartup
	self
		parentView: View desktop;
		create.
		self setVersion: version.
	self basicReAddIcons!

onTaskbarCreated
	self basicReAddIcons!

onViewClosed
	self basicRemoveIcons.
	!

setVersion: anInteger 
	| notifyStruct |
	version := anInteger.
	notifyStruct := (NOTIFYICONDATA2 new)
				hWnd: self handle;
				uVersion: version;
				yourself.
	ShellLibrary default shell_NotifyIcon: NIM_SETVERSION pnid: notifyStruct!

showBalloon: aNotifyAreaIcon icon: anIcon title: title message: message flags: dwInfoFlags timeout: anInteger playSound: aBoolean 
	| notifyStruct |
	notifyStruct := (NOTIFYICONDATA2 new)
				hWnd: self handle;
				balloonTitle: title;
				balloonMessage: message;
				balloonTimeout: anInteger;
				balloonFlags: (dwInfoFlags bitOr: (aBoolean ifFalse: [NIIF_NOSOUND] ifTrue: [0]));
				yourself.
	(dwInfoFlags anyMask: NIIF_USER) ifTrue: [notifyStruct hIcon: anIcon asParameter].
	ShellLibrary default shell_NotifyIcon: NIM_MODIFY pnid: notifyStruct!

useWin2000Behavior
	"Use the Windows 2000 behavior. Use this value for applications designed for Windows 2000 and later."

	self setVersion: NOTIFYICON_VERSION
	

!

useWin95Behavior
	"Use the Windows 95 behavior. Use this value for applications designed for Windows versions prior to Windows 2000."

	self setVersion: 0!

useWinVistaBehavior
	"Use the Windows Vista behavior. Use this value for applications designed for Windows Vista and later."
Error notYetImplemented.
#USToDo. "reimplement #iconFromIdentifier"
	self setVersion: NOTIFYICON_VERSION4! !
!NotifyAreaView categoriesFor: #add:!public! !
!NotifyAreaView categoriesFor: #basicAdd:!helpers!private! !
!NotifyAreaView categoriesFor: #basicHide:!helpers!private! !
!NotifyAreaView categoriesFor: #basicReAddIcons!helpers!private! !
!NotifyAreaView categoriesFor: #basicRemoveIcons!helpers!private! !
!NotifyAreaView categoriesFor: #basicUpdate:!public! !
!NotifyAreaView categoriesFor: #dispatchRegistered:wParam:lParam:!dispatching!private! !
!NotifyAreaView categoriesFor: #hide:!public! !
!NotifyAreaView categoriesFor: #iconFromIdentifier:!helpers!private! !
!NotifyAreaView categoriesFor: #initialize!initializing!private! !
!NotifyAreaView categoriesFor: #nextIconIdentifier!helpers!private! !
!NotifyAreaView categoriesFor: #onBalloonClick:!public! !
!NotifyAreaView categoriesFor: #onBalloonHide!public! !
!NotifyAreaView categoriesFor: #onBalloonShow:!public! !
!NotifyAreaView categoriesFor: #onBalloonTimeout:!public! !
!NotifyAreaView categoriesFor: #onKeySelect:!public! !
!NotifyAreaView categoriesFor: #onLeftButtonDoubleClicked:!event handling!public! !
!NotifyAreaView categoriesFor: #onLeftButtonPressed:!event handling!public! !
!NotifyAreaView categoriesFor: #onLeftButtonReleased:!event handling!public! !
!NotifyAreaView categoriesFor: #onMouseMoved:!event handling!public! !
!NotifyAreaView categoriesFor: #onRightButtonDoubleClicked:!event handling!public! !
!NotifyAreaView categoriesFor: #onRightButtonPressed:!event handling!public! !
!NotifyAreaView categoriesFor: #onRightButtonReleased:!event handling!public! !
!NotifyAreaView categoriesFor: #onSelect:!public! !
!NotifyAreaView categoriesFor: #onShowContextMenu:!event handling!public! !
!NotifyAreaView categoriesFor: #onStartup!event handling!public! !
!NotifyAreaView categoriesFor: #onTaskbarCreated!event handling!public! !
!NotifyAreaView categoriesFor: #onViewClosed!event handling!public! !
!NotifyAreaView categoriesFor: #setVersion:!public! !
!NotifyAreaView categoriesFor: #showBalloon:icon:title:message:flags:timeout:playSound:!helpers!private! !
!NotifyAreaView categoriesFor: #useWin2000Behavior!public! !
!NotifyAreaView categoriesFor: #useWin95Behavior!public! !
!NotifyAreaView categoriesFor: #useWinVistaBehavior!public! !

!NotifyAreaView class methodsFor!

default
	"Answers the singleton instance."

	DefaultInstance isNil 
		ifTrue: 
			[DefaultInstance := (self new)
						parentView: View desktop;
						create;
						yourself].
	^DefaultInstance!

initialize
	self
		initializeNotificationMap;
		registerMessages.
		SessionManager current 
		when: #sessionStopped
		send: #onExit
		to: self.
	^super initialize!

initializeNotificationMap
	"Private - Initialise the map of  notification codes to selectors.
	N.B. This method must not be stripped in order to ensure that the notification event handler
	methods (which are looked up in the table) are preserved.
	
	self initializeNotificationMap
	"

	NotifyMessageMap := (IdentityDictionary new)
				at: WM_LBUTTONDOWN put: #onLeftButtonPressed:;
				at: WM_LBUTTONUP put: #onLeftButtonReleased:;
				at: WM_RBUTTONDOWN put: #onRightButtonPressed:;
				at: WM_RBUTTONUP put: #onRightButtonReleased:;
				at: WM_LBUTTONDBLCLK put: #onLeftButtonDoubleClicked:;
				at: WM_RBUTTONDBLCLK put: #onRightButtonDoubleClicked:;
				at: WM_MOUSEMOVE put: #onMouseMoved:;
				at: WM_CONTEXTMENU put: #onShowContextMenu:;
				at: NIN_BALLOONSHOW put: #onBalloonShow:;
				at: NIN_BALLOONHIDE put: #onBalloonHide;
				at: NIN_BALLOONTIMEOUT put: #onBalloonTimeout:;
				at: NIN_BALLOONUSERCLICK put: #onBalloonClick:;
				at: NIN_SELECT put: #onSelect:;
				at: NIN_KEYSELECT put: #onKeySelect:;
				shrink;
				yourself!

onExit
	DefaultInstance ifNotNil: [:value | value basicRemoveIcons ]!

onStartup
	"Private - Perform any startup operations."

	self registerMessages.
	!

registerMessages
	WM_NOTIFYAREACALLBACK := self registerMessage: 'NotifyAreaCallback'.
	WM_TASKBARCREATED := self registerMessage: 'TaskbarCreated'!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system."

	DefaultInstance notNil 
		ifTrue: 
			[DefaultInstance destroy.
			DefaultInstance := nil]! !
!NotifyAreaView class categoriesFor: #default!accessing!public! !
!NotifyAreaView class categoriesFor: #initialize!initializing!private! !
!NotifyAreaView class categoriesFor: #initializeNotificationMap!initializing!must not strip!private! !
!NotifyAreaView class categoriesFor: #onExit!initializing!private! !
!NotifyAreaView class categoriesFor: #onStartup!initializing!private! !
!NotifyAreaView class categoriesFor: #registerMessages!initializing!private! !
!NotifyAreaView class categoriesFor: #uninitialize!class hierarchy-removing!private! !

"Binary Globals"!

