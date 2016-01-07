| package |
package := Package name: 'US View Extensions'.
package paxVersion: 1;
	basicComment: '$id: US View Extensions 0.039$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 21.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.039'.

package basicScriptAt: #preinstall put: 'Win32Constants
	at: ''GCL_HICON'' put: -14;
	at: ''GCL_HICONSM'' put: -34;
	at: ''HWND_BROADCAST'' put: 16rFFFF;
	at: ''SPI_GETFONTSMOOTHING'' put: 74;
	at: ''SPI_SETFONTSMOOTHING'' put: 75;
	at: ''FE_FONTSMOOTHINGSTANDARD'' put: 1;
	at: ''FE_FONTSMOOTHINGCLEARTYPE'' put: 2;
	at: ''SPI_GETFONTSMOOTHINGTYPE'' put: 16r200A;
	at: ''SPI_SETFONTSMOOTHINGTYPE'' put: 16r200B;
	at: ''SPI_GETFONTSMOOTHINGCONTRAST'' put: 16r200C;
	at: ''SPI_SETFONTSMOOTHINGCONTRAST'' put: 16r200D;
	at: ''ERROR'' put: 0;
	at: ''NULLREGION'' put: 1;
	at: ''SIMPLEREGION'' put: 2;
	at: ''COMPLEXREGION'' put: 3'.

package methodNames
	add: #ChoicePresenter -> #basicBeSorted;
	add: #ChoicePresenter -> #beNotSorted;
	add: #ChoicePresenter -> #beSorted;
	add: #ChoicePresenter -> #beSorted:;
	add: #ChoicePresenter -> #choices;
	add: #ChoicePresenter -> #defaultSortBlock;
	add: #ChoicePresenter -> #isSorted;
	add: #ChoicePresenter -> #list;
	add: #ChoicePresenter -> #list:;
	add: #ChoicePresenter -> #sortAlgorithmClass;
	add: #ChoicePresenter -> #sortBlock;
	add: #ChoicePresenter -> #sortBlock:;
	add: #CommCtrlLibrary -> #imageListAdd:hbmImage:hbmMask:;
	add: #DesktopView -> #allDisplayMonitors;
	add: #DesktopView -> #displayMonitors:intersecting:do:;
	add: #DesktopView -> #freezeDuring:;
	add: #Dialog -> #showModalTo:;
	add: #Presenter -> #beFullscreen;
	add: #PushButton -> #image:;
	add: #Shell -> #fixApplicationIconInMenu:with:;
	add: #Shell -> #fixApplicationIcons:;
	add: #Shell -> #fixApplicationIconsInMenuBar:With:;
	add: #Shell -> #fixApplicationIconsInMenuBarWith:;
	add: #Shell -> #fixApplicationIconsInShellWith:;
	add: #Shell -> #onSysColorChange;
	add: #ShellView -> #maximize;
	add: #ShellView -> #onSysColorChange;
	add: #ShellView -> #wmSysColorChange:wParam:lParam:;
	add: #SystemMetrics -> #fontSmoothing;
	add: #SystemMetrics -> #fontSmoothing:;
	add: #SystemMetrics -> #fontSmoothingType;
	add: #SystemMetrics -> #hasCleartype;
	add: #SystemMetrics -> #hasCleartype:;
	add: #SystemMetrics -> #setSysParamBool:value:;
	add: #SystemMetrics -> #setSysParamInteger:value:;
	add: #UserLibrary -> #enumDisplayMonitors:lprcClip:lpfnEnum:dwData:;
	add: #UserLibrary -> #getMenu:;
	add: #UserLibrary -> #getWindowRgn:hRgn:;
	add: #UserLibrary -> #setWindowRgn:hRgn:bRedraw:;
	add: #View -> #attachThreadWhile:;
	add: #View -> #basicLargeIcon;
	add: #View -> #basicMenuHandle;
	add: #View -> #basicMenuHandle:;
	add: #View -> #basicSmallIcon;
	add: #View -> #beFullscreen;
	add: #View -> #captureBitmap;
	add: #View -> #coverParent;
	add: #View -> #isChild;
	add: #View -> #isChild:;
	add: #View -> #parentViewOrDesktop;
	add: #View -> #printWindow;
	add: #View -> #processId;
	add: #View -> #region;
	add: #View -> #reverseAllSubViewZOrder;
	add: #View -> #reverseSubViewZOrder;
	add: #View -> #threadId;
	add: #View -> #topShellView;
	add: #View -> #wndClassName;
	add: #WinImageList -> #addBitmap:bitmapMask:;
	add: 'Dialog class' -> #showModalOn:to:;
	add: 'Dialog class' -> #showModalTo:;
	add: 'View class' -> #programManager;
	add: 'View class' -> #showFullscreen;
	add: 'View class' -> #startButton;
	add: 'View class' -> #taskBar;
	add: 'View class' -> #topLevelWindows;
	add: 'View class' -> #trayBar;
	add: 'View class' -> #trayBarClock;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin Additional Sort Algorithms';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Choice\Dolphin Choice Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Image\Dolphin Image Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!ChoicePresenter methodsFor!

basicBeSorted
	"Private - Change the receiver's model sortblock to a default sort block.
	The sort is not actually performed"

	self sortBlock: self defaultSortBlock!

beNotSorted
	"Change the receiver's view model to be an OrderedCollection"

	self sortBlock: nil.
	self list: self list asOrderedCollection!

beSorted
	"Change the receiver's model to be a SortedCollection 
	using a default sort block"

	self basicBeSorted.
	self list: self list asOrderedCollection.!

beSorted: aSortBlock
	"Change the receiver's model to be a SortedCollection using aSortBlock to determine sort order"

	self sortBlock: aSortBlock.
	self list: self list!

choices
	"Sets the choice list to choose from to be aSequenceableCollection"

	^self list!

defaultSortBlock
	"Private - Answer a default sort block to use when the receiver is sorted"

	^Message selector: #<=!

isSorted
	"Answer true if the receiver is sorted by default"

	^self sortBlock notNil!

list
	"Answer the contents of the receiver"

	^choicesModel list!

list: aSequenceableCollection 
	"Set the contents of the receiver to be aSequenceableCollection"

	| list |
	list := self isSorted 
				ifTrue: 
					[aSequenceableCollection 
						asSortedCollectionUsing: (self sortAlgorithmClass sortBlock: self sortBlock)]
				ifFalse: [aSequenceableCollection].
	^choicesModel list: list!

sortAlgorithmClass
	"Answer the class of algorithm to be used for sorting the receiver's contents. Use a stable sorting algorithm."

	^MergesortAlgorithm!

sortBlock
	^self propertyAt: #sortBlock ifAbsent: [nil]!

sortBlock: anObject 
	self propertyAt: #sortBlock put: anObject! !
!ChoicePresenter categoriesFor: #basicBeSorted!private!sorting! !
!ChoicePresenter categoriesFor: #beNotSorted!public!sorting! !
!ChoicePresenter categoriesFor: #beSorted!public!sorting! !
!ChoicePresenter categoriesFor: #beSorted:!public!sorting! !
!ChoicePresenter categoriesFor: #choices!accessing!public! !
!ChoicePresenter categoriesFor: #defaultSortBlock!constants!private!sorting! !
!ChoicePresenter categoriesFor: #isSorted!public!sorting!testing! !
!ChoicePresenter categoriesFor: #list!accessing!public! !
!ChoicePresenter categoriesFor: #list:!accessing!public! !
!ChoicePresenter categoriesFor: #sortAlgorithmClass!accessing!public! !
!ChoicePresenter categoriesFor: #sortBlock!accessing!private!sorting! !
!ChoicePresenter categoriesFor: #sortBlock:!accessing!private!sorting! !

!CommCtrlLibrary methodsFor!

imageListAdd: hImageList hbmImage: bitmap hbmMask: mask
	"Adds an image or images to an image list. 

Syntax

int ImageList_Add(          HIMAGELIST himl,
    HBITMAP hbmImage,
    HBITMAP hbmMask
);"


	<stdcall: sdword ImageList_Add handle handle handle>
	^self invalidCall! !
!CommCtrlLibrary categoriesFor: #imageListAdd:hbmImage:hbmMask:!primitives!public!win32 functions-image list! !

!DesktopView methodsFor!

allDisplayMonitors
	| displayMonitors |
	displayMonitors := OrderedCollection new.
	self 
		displayMonitors: nil
		intersecting: nil
		do: [:monitor :canvas :rectangle | displayMonitors add: rectangle. true].
	^displayMonitors!

displayMonitors: hdc intersecting: aRectangle do: aBlock 
	| monitorEnumProc |
	monitorEnumProc := ExternalCallback block: 
					[:hMonitor :hdcMonitor :lprcMonitor :dwData | 
					aBlock value: hMonitor value: (Canvas withNonOwnedDC: hdcMonitor) value: lprcMonitor asRectangle.
				]
				descriptor: (ExternalDescriptor 
						callingConvention: 'stdcall:'
						returnType: 'bool'
						argumentTypes: 'handle handle RECT* lpvoid').
	UserLibrary default 
		enumDisplayMonitors: hdc
		lprcClip: aRectangle
		lpfnEnum: monitorEnumProc asParameter
		dwData: nil!

freezeDuring: aBlock 
	| imageView return bitmap |
	bitmap := View desktop captureBitmap.
	(imageView := View desktop addSubView: ImageView new)
		coverParent;
		model: bitmap asValue;
		show;
		beTopMost.
	[return := aBlock value] on: Error do: [:ex | ] .
	imageView close.
	^return! !
!DesktopView categoriesFor: #allDisplayMonitors!public! !
!DesktopView categoriesFor: #displayMonitors:intersecting:do:!public! !
!DesktopView categoriesFor: #freezeDuring:!public! !

!Dialog methodsFor!

showModalTo: aView 
	self view showModalTo: aView.^self answer! !
!Dialog categoriesFor: #showModalTo:!public!realizing/unrealizing! !

!Dialog class methodsFor!

showModalOn: aModel to: aView 
	"Creates a default instance of the receiver with a default view	and displays it modal 
	to the current active window. 
	Answers the result of the dialog if confirmed or nil otherwise"

	^(self createOn: aModel) showModalTo: aView!

showModalTo: aView 
	"Creates a default instance of the receiver with a default view	and displays it modal 
	to the current active window. 
	Answers the result of the dialog if confirmed or nil otherwise"

	^self create showModalTo: aView! !
!Dialog class categoriesFor: #showModalOn:to:!instance creation!public! !
!Dialog class categoriesFor: #showModalTo:!instance creation!public! !

!Presenter methodsFor!

beFullscreen
	^self view beFullscreen! !
!Presenter categoriesFor: #beFullscreen!public! !

!PushButton methodsFor!

image: anImageOrNil 
	"Sets the image displayed by the receiver to anImageOrNil"

	(image := anImageOrNil) isNil 
		ifTrue: 
			[self baseStyle: 0 maskedBy: ##(BS_BITMAP | BS_ICON).
			^self].
	self baseStyle: (self text isNilOrEmpty 
				ifTrue: [anImageOrNil imageType = IMAGE_ICON ifTrue: [BS_ICON] ifFalse: [BS_BITMAP]]
				ifFalse: [0])
		maskedBy: ##(BS_BITMAP | BS_ICON).
	self 
		sendMessage: BM_SETIMAGE
		wParam: anImageOrNil imageType
		lParam: anImageOrNil asParameter! !
!PushButton categoriesFor: #image:!accessing!public! !

!Shell methodsFor!

fixApplicationIconInMenu: aMenu with: anIcon 
	aMenu items do: 
			[:eachEntry | 
			eachEntry image 
				ifNotNil: 
					[:image | 
					image identifier = SessionManager applicationIconId 
						ifTrue: [eachEntry image: anIcon ]]]!

fixApplicationIcons: anIcon 
	"Replaces the standard Dolphin Application Icon with anIcon.
	This method should only be called during development time - it won't
	hurt to call it during runtime but it won't help either."

	
	self
		fixApplicationIconsInMenuBarWith: anIcon;
		fixApplicationIconsInShellWith: anIcon!

fixApplicationIconsInMenuBar: aMenuBar With: anIcon 
	aMenuBar items do: [:eachMenu | self fixApplicationIconInMenu: eachMenu with: anIcon]!

fixApplicationIconsInMenuBarWith: anIcon 
self view menuBar ifNotNil: [:menuBar| self fixApplicationIconsInMenuBar: menuBar With: anIcon]

	!

fixApplicationIconsInShellWith: anIcon

	(self view largeIcon notNil 
		and: [self view largeIcon identifier = SessionManager applicationIconId]) 
			ifTrue: [self view largeIcon: anIcon].
	(self view smallIcon notNil 
		and: [self view smallIcon identifier = SessionManager applicationIconId]) 
			ifTrue: [self view smallIcon: anIcon]!

onSysColorChange
	
	^self view onSysColorChange! !
!Shell categoriesFor: #fixApplicationIconInMenu:with:!*-in class package!private! !
!Shell categoriesFor: #fixApplicationIcons:!*-in class package!public! !
!Shell categoriesFor: #fixApplicationIconsInMenuBar:With:!*-in class package!event handling!private! !
!Shell categoriesFor: #fixApplicationIconsInMenuBarWith:!*-in class package!event handling!private! !
!Shell categoriesFor: #fixApplicationIconsInShellWith:!*-in class package!event handling!private! !
!Shell categoriesFor: #onSysColorChange!event handling!public! !

!ShellView methodsFor!

maximize
	"Show the receiver in a maximized state"

	self showWithStyle: SW_MAXIMIZE!

onSysColorChange
	
	self presenter trigger: #sysColorChanged.
	^nil!

wmSysColorChange: message wParam: wParam lParam: lParam 
	"Private - A system color has occurred. Just accept the default window processing as the
	control should handle it."

	super 
		wmSysColorChange: message
		wParam: wParam
		lParam: lParam.
	^self presenter onSysColorChange.
	! !
!ShellView categoriesFor: #maximize!operations!public! !
!ShellView categoriesFor: #onSysColorChange!event handling!public! !
!ShellView categoriesFor: #wmSysColorChange:wParam:lParam:!event handling-win32!private! !

!SystemMetrics methodsFor!

fontSmoothing
	^(self 
		getSysParam: SPI_GETFONTSMOOTHING
		type: BOOL
		ifError: [^true]) value!

fontSmoothing: aBoolean 
	| result |
	result := self setSysParamBool: SPI_SETFONTSMOOTHING value: aBoolean.
	UserLibrary default 
		invalidate: 0
		lpRect: 0
		bErase: true asParameter.
	SessionManager current inputState pumpMessages.
	^result!

fontSmoothingType
	^(self 
		getSysParam: SPI_GETFONTSMOOTHINGTYPE
		type: DWORD
		ifError: [^FE_FONTSMOOTHINGSTANDARD ]) value!

hasCleartype
^self fontSmoothingType = FE_FONTSMOOTHINGCLEARTYPE!

hasCleartype: aBoolean 
	| result |
	result := self setSysParamInteger: SPI_SETFONTSMOOTHINGTYPE
		value: (aBoolean ifTrue: [FE_FONTSMOOTHINGCLEARTYPE] ifFalse: [FE_FONTSMOOTHINGSTANDARD]).
			UserLibrary default 
		invalidate: 0
		lpRect: 0
		bErase: true asParameter.
	SessionManager current inputState pumpMessages.
	^result!

setSysParamBool: anInteger value: aBoolean 
	^UserLibrary default 
		systemParametersInfo: anInteger
		uiParam: aBoolean asParameter
		pvParam: 0
		fWinIni: 0!

setSysParamInteger: anInteger value: value 
	^UserLibrary default 
		systemParametersInfo: anInteger
		uiParam: 0
		pvParam: value
		fWinIni: 0! !
!SystemMetrics categoriesFor: #fontSmoothing!constants!public! !
!SystemMetrics categoriesFor: #fontSmoothing:!constants!public! !
!SystemMetrics categoriesFor: #fontSmoothingType!constants!public! !
!SystemMetrics categoriesFor: #hasCleartype!constants!public! !
!SystemMetrics categoriesFor: #hasCleartype:!constants!public! !
!SystemMetrics categoriesFor: #setSysParamBool:value:!helpers!private! !
!SystemMetrics categoriesFor: #setSysParamInteger:value:!helpers!private! !

!UserLibrary methodsFor!

enumDisplayMonitors: hdc lprcClip: lprcClip lpfnEnum: lpfnEnum dwData: dwData
	"The EnumDisplayMonitors function enumerates display monitors (including invisible pseudo-monitors
	associated with the mirroring drivers) that intersect a region formed by the intersection of a specified
	clipping rectangle and the visible region of a device context. EnumDisplayMonitors calls an
	application-defined MonitorEnumProc callback function once for each monitor that is enumerated.
	Note that GetSystemMetrics(SM_CMONITORS) counts only the display monitors.

	BOOL EnumDisplayMonitors(
		HDC hdc,                   // handle to display DC
		LPCRECT lprcClip,          // clipping rectangle
		MONITORENUMPROC lpfnEnum,  // callback function
		LPARAM dwData              // data for callback function
	);
	"
	<stdcall: bool EnumDisplayMonitors handle RECT* lpvoid lpvoid>
	^self invalidCall!

getMenu: hWnd
"The GetMenu function retrieves a handle to the menu assigned to the specified window.

    HMENU GetMenu(      
        HWND hWnd
    );"
<stdcall: handle GetMenu handle>
^self invalidCall!

getWindowRgn: hwnd hRgn: hRgn 
	"The GetWindowRgn function obtains a copy of the window region of a window.
	The window region of a window is set by calling the SetWindowRgn function.
	The window region determines the area within the window where the system permits
	drawing. The system does not display any portion of a window that lies outside of
	the window region
	
	int GetWindowRgn(
		__in  HWND hWnd,
		__in  HRGN hRgn
	);
	"
<stdcall: sdword GetWindowRgn handle handle>
^self invalidCall
	!

setWindowRgn: hWnd hRgn: hRgn bRedraw: bRedraw 
	"The SetWindowRgn function sets the window region of a window. The window
	region determines the area within the window where the system permits drawing.
	The system does not display any portion of a window that lies outside of the window
	region

	int SetWindowRgn(
		__in  HWND hWnd,
		__in  HRGN hRgn,
		__in  BOOL bRedraw
	);
	"

	<stdcall: sdword SetWindowRgn handle handle bool>
	^self invalidCall! !
!UserLibrary categoriesFor: #enumDisplayMonitors:lprcClip:lpfnEnum:dwData:!public! !
!UserLibrary categoriesFor: #getMenu:!public!win32 functions-menu! !
!UserLibrary categoriesFor: #getWindowRgn:hRgn:!public! !
!UserLibrary categoriesFor: #setWindowRgn:hRgn:bRedraw:!public! !

!View methodsFor!

attachThreadWhile: aBlock 
	| ul kl currentThreadId attachThreatId |
	ul := UserLibrary default.
	kl := KernelLibrary default.
	currentThreadId := kl getCurrentThreadId.
	attachThreatId := self threadId.
	attachThreatId = currentThreadId 
		ifFalse: 
			[(ul
				attachThreadInput: currentThreadId
				idAttachTo: attachThreatId
				fAttach: true) = 0 
				ifTrue: [ul invalidCall]].
	aBlock value.
	attachThreatId = currentThreadId 
		ifFalse: 
			[
			(ul 
				attachThreadInput: currentThreadId
				idAttachTo: attachThreatId
				fAttach: false) = 0 
				ifTrue: [ul invalidCall]]!

basicLargeIcon
	| hicon |
	(hicon := self 
				sendMessage: WM_GETICON
				wParam: true asParameter
				lParam: 0) notNull 
		ifTrue: [^(Icon fromHandle: hicon) copy].
	(hicon := UserLibrary default getClassLong: self handle asParameter offset: GCL_HICON) notNull 
		ifTrue: [^(Icon fromHandle: hicon) copy].
	^Icon defaultApplication!

basicMenuHandle
	^UserLibrary default getMenu: self handle asParameter!

basicMenuHandle: aMenuBarHandleOrNil 


	UserLibrary default setMenu: self asParameter hMenu: aMenuBarHandleOrNil asParameter!

basicSmallIcon
	
	| hicon |
	(hicon := self 
				sendMessage: WM_GETICON
				wParam: false asParameter
				lParam: 0) notNull 
		ifTrue: [^(Icon fromHandle: hicon) copy].
	(hicon := UserLibrary default getClassLong: self handle asParameter offset: GCL_HICONSM) notNull 
		ifTrue: [^(Icon fromHandle: hicon) copy].
	^Icon defaultApplication!

beFullscreen
	self rectangle: (0 @ 0 extent: SystemMetrics current virtualScreenExtent)!

captureBitmap
	| extent bitmap dcGraphics dcView |
	extent := self extent.
	bitmap := Bitmap compatible: self canvas extent: extent.
	dcGraphics := bitmap canvas handle.
	dcView := self getWindowDC.
	GDILibrary default 
		bitBlt: dcGraphics
		nXDest: 0
		nYDest: 0
		nWidth: extent x
		nHeight: extent y
		hdcSrc: dcView
		nXSrc: 0
		nYSrc: 0
		dwRop: SRCCOPY.^bitmap!

coverParent
self 	position: 0 @ 0; 
				extent: self parentViewOrDesktop extent.
	!

isChild	^self baseStyleAllMask: WS_CHILD!

isChild: aBoolean 
	self 
		baseStyleMask: WS_CHILD
		set: aBoolean
		recreateIfChanged: true!

parentViewOrDesktop
	^self parentView ifNil: [^DesktopView  current ]!

printWindow
	| bitmap |
	bitmap := Bitmap compatible: self canvas extent: self extent.
	UserLibrary default 
		printWindow: self asParameter
		hdcBlt: bitmap canvas asParameter
		nFlags: 0.
	^bitmap!

processId
	| processId |
	processId := DWORD new.
	UserLibrary default getWindowThreadProcessId: self handle asParameter lpdwProcessId: processId.
	^processId value!

region
	| region |
	region := Region empty.
	UserLibrary default getWindowRgn: self asParameter hRgn: region asParameter.
	region = Region empty ifTrue: [region  := Region  rectangle: (0@0 extent: self extent ) ].
	^region!

reverseAllSubViewZOrder
self reverseSubViewZOrder.
	self subViewsDo: [:each | each reverseSubViewZOrder]!

reverseSubViewZOrder
self subViews do: [:each | each  zOrderTop ]!

threadId
	
	^UserLibrary default getWindowThreadProcessId: self handle asParameter lpdwProcessId: nil
	!

topShellView

| topMost topMostShellView |
	topMost := topMostShellView := self.


[topMost isTopView] whileFalse: [(topMost isKindOf: ShellView) ifTrue: [ topMostShellView := topMost].
topMost := topMost parentView].
^topMostShellView!

wndClassName
	| buffer |
	buffer := String new: 256.
	UserLibrary default 
		getClassName: self handle
		lpClassName: buffer
		nMaxCount: buffer size.
	^buffer trimNulls! !
!View categoriesFor: #attachThreadWhile:!public! !
!View categoriesFor: #basicLargeIcon!public! !
!View categoriesFor: #basicMenuHandle!public! !
!View categoriesFor: #basicMenuHandle:!public! !
!View categoriesFor: #basicSmallIcon!public! !
!View categoriesFor: #beFullscreen!public! !
!View categoriesFor: #captureBitmap!public! !
!View categoriesFor: #coverParent!public! !
!View categoriesFor: #isChild!accessing-styles!public!testing! !
!View categoriesFor: #isChild:!accessing-styles!public! !
!View categoriesFor: #parentViewOrDesktop!public! !
!View categoriesFor: #printWindow!public! !
!View categoriesFor: #processId!public! !
!View categoriesFor: #region!public! !
!View categoriesFor: #reverseAllSubViewZOrder!public! !
!View categoriesFor: #reverseSubViewZOrder!public! !
!View categoriesFor: #threadId!public! !
!View categoriesFor: #topShellView!public! !
!View categoriesFor: #wndClassName!public! !

!View class methodsFor!

programManager
| handle |
	handle := UserLibrary default findWindow: nil lpWindowName: 'Program Manager'.
^ View fromHandle: handle.!

showFullscreen
	^self show beFullscreen!

startButton
	"ID_STARTBUTTON  0x130"

	| handle |
	handle := View taskBar getItemHandle: 16r130
				ifAbsent: [UserLibrary default findWindow: 'Button' lpWindowName: 'Start'].
	handle isNil ifTrue: [^nil].
	^View fromHandle: handle!

taskBar
	| handle |
	handle := UserLibrary default findWindow: 'Shell_TrayWnd' lpWindowName: nil.
	^View fromHandle: handle!

topLevelWindows
	"Answer a Collection containing all top level windows"

	| windows wndEnumProc |
	windows := OrderedCollection new.
	wndEnumProc := ExternalCallback block: 
					[:hwnd :lparam | 
					| view |
					view := View fromHandle: hwnd.
					windows add: view.
					true]
				argumentTypes: 'handle dword'.
	UserLibrary default enumWindows: wndEnumProc asParameter lParam: nil.
	^windows!

trayBar
	"ID_TRAY         0x12F"

	| handle |
	handle := View taskBar getItemHandle: 16r12F ifAbsent: [nil].
	handle isNil ifTrue: [^nil].
	^View fromHandle: handle!

trayBarClock
	"ID_CLOCK        0x12F"

	| handle |
	handle := View trayBar getItemHandle: 16r12F ifAbsent: [nil].
	handle isNil ifTrue: [^nil].
	^View fromHandle: handle! !
!View class categoriesFor: #programManager!public! !
!View class categoriesFor: #showFullscreen!public! !
!View class categoriesFor: #startButton!public! !
!View class categoriesFor: #taskBar!public! !
!View class categoriesFor: #topLevelWindows!public! !
!View class categoriesFor: #trayBar!public! !
!View class categoriesFor: #trayBarClock!public! !

!WinImageList methodsFor!

addBitmap: aBitmap bitmapMask: aMask 
	"Append the <Bitmap> argument to the list of images held by the receiver answering the
	zero-based <integer> index at which it was added. The argument, aColor, specifies the mask
	colour to be used; pixels in the bitmap of the specified colour are converted to black, and
	the corresponding pixel is set in the mask bitmap. This allows the bitmap to be drawn
	transparently from the image list to a canvas. Note, however, that this will only work for
	bitmaps with a colour depth <= 8bpp, which does significantly reduce the usefulness of this
	feature. If the return value is -1, then the bitmap could not be added for some reason (e.g.
	it could not be realized and hence has a null handle). This is not treated as an error, as
	the effect should only be cosmetic."

	"Implementation Note: The image list will modify the bitmap in-place if the ILC_MASK flag is
	set (poor isn't it) and hence we must make sure we pass it it's own private copy of the
	bitmap if masking is enabled."

	| bmp hImgList mask |
	hImgList := self asParameter.
	bmp := self hasMask ifTrue: [aBitmap copy] ifFalse: [aBitmap].
	mask := self hasMask ifTrue: [aMask copy] ifFalse: [aMask].
	"#305 - We must make sure the bitmap is not selected into a DC, or the result will be a blank image in the list
	 	this won't damage the Bitmap in any way, because the canvas can be lazily recreated."
	bmp freeDC.
	^CommCtrlLibrary default 
		imageListAdd: hImgList
		hbmImage: bmp asParameter
		hbmMask: mask asParameter! !
!WinImageList categoriesFor: #addBitmap:bitmapMask:!adding!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

