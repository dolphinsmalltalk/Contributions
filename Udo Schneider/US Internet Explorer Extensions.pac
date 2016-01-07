| package |
package := Package name: 'US Internet Explorer Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Internet Explorer Extensions 0.012$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

URLPresenterWithContextMenu allows you to control /which/ context menu is displayed in a WebControl:
 - If the presenters view #allowContextMenu aspect is false no context menu will be shown. Neither a custom one nor the default one.
 - If the presenters view #allowContextMenu aspect is true it depends on #contextMenu. If #contextMenu is not nil it will be displayed. If it is nil the default one will be used.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.012'.


package classNames
	add: #Favorite;
	add: #FavoriteFolder;
	add: #FavoritesTreeModel;
	add: #IAuthenticate;
	add: #IProfferService;
	add: #IProtectFocus;
	add: #RemotableHandle;
	add: #URLPresenterWithContextMenu;
	yourself.

package methodNames
	add: #URLPresenter -> #AllowFocusChange:;
	add: #URLPresenter -> #Authenticate:pszUsername:pszPassword:;
	add: #URLPresenter -> #finalRelease;
	add: #URLPresenter -> #getSelectedText;
	add: #URLPresenter -> #getSelectedTextOrNil;
	add: #URLPresenter -> #onViewOpened;
	add: #URLPresenter -> #profferService;
	add: #URLPresenter -> #profferService:;
	add: #URLPresenter -> #queryInterface:;
	add: #URLPresenter -> #queryInterface:ifNone:;
	add: #URLPresenter -> #serviceProvider;
	add: #URLPresenter -> #supportedInterfaces;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\ActiveX\OCX\ActiveX Control Hosting';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: '..\..\Object Arts\Dolphin\ActiveX\Components\SHDocVw\Internet Explorer';
	add: '..\..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	add: 'US File Locators';
	yourself).

package!

"Class Definitions"!

Object subclass: #Favorite
	instanceVariableNames: 'parent path'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #RemotableHandle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IUnknown subclass: #IAuthenticate
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IUnknown subclass: #IProfferService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IUnknown subclass: #IProtectFocus
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Favorite subclass: #FavoriteFolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VirtualTreeModel subclass: #FavoritesTreeModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
URLPresenter subclass: #URLPresenterWithContextMenu
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'Win32Errors'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!URLPresenter methodsFor!

AllowFocusChange: allowFocusChange 
	"Implement the IProtectFocus::AllowFocusChange() interface function."

	
	^##(Win32Errors at: 'E_NOTIMPL').
	#todo	"Implement me"!

Authenticate: authenticate pszUsername: pszUsername pszPassword: pszPassword 
	"Implement the IAuthenticate::Authenticate() interface function."


	^##(Win32Errors at: 'E_NOTIMPL').
	#todo	"Implement me"!

finalRelease
	"The last reference to the receiver (as a COM object) has been released.
	This is an opportunity to actively clean up, rather than passively waiting 
	for finalization which is asynchronous and may never happen if the 
	object doesn't become garbage."

	"Override as necessary"!

getSelectedText
	^[self view controlDispatch document selection createRange text trimBlanks] on: Error do: [:ex | String new]!

getSelectedTextOrNil
	| text |
	text  := self getSelectedText.
	^text notEmpty ifTrue: [text ] ifFalse: [nil]!

onViewOpened
	"Received when the receiver's view is been connected. "

	|  |
	self
		profferService: IAuthenticate;
		profferService: IProtectFocus.
	super onViewOpened!

profferService
	^self serviceProvider queryService: IProfferService guid riid: IProfferService guid!

profferService: interface 
	self profferService profferService: interface guid psp: (self queryInterface: interface)!

queryInterface: anInterfaceClass
	"Answer a new interface which supports the specified interface protocol
	(usually a class), or nil if the receiver does not support the interface."

	^self queryInterface: anInterfaceClass ifNone: []
!

queryInterface: anInterfaceClass ifNone: exceptionHandler 
	"Answer a new interface pointer which supports the specified interface protocol
	(usually a class). If the receiver does not support the interface, answer the
	result of evaluating the niladic valuable, exceptionHandler."

	^(self supportedInterfaces detect: [:ic | ic supportsInterface: anInterfaceClass] ifNone: []) 
		ifNil: [exceptionHandler value]
		ifNotNil: [:class | class on: self implementor: self]!

serviceProvider
	^self view controlDispatch queryInterface: IServiceProvider!

supportedInterfaces
	"Answer the set of interface classes supported by the receiver.
	We need only support the standard property notification sink interface since
	all other control container requirements are satisfied by the ATL host window
	that we are wrapping."

	^#(##(IAuthenticate ) ##(IProtectFocus))! !
!URLPresenter categoriesFor: #AllowFocusChange:!COM Interfaces-IProtectFocus!public! !
!URLPresenter categoriesFor: #Authenticate:pszUsername:pszPassword:!COM Interfaces-IAuthenticate!public! !
!URLPresenter categoriesFor: #finalRelease!public!realizing/unrealizing! !
!URLPresenter categoriesFor: #getSelectedText!public! !
!URLPresenter categoriesFor: #getSelectedTextOrNil!public! !
!URLPresenter categoriesFor: #onViewOpened!event handling!public! !
!URLPresenter categoriesFor: #profferService!event handling!public! !
!URLPresenter categoriesFor: #profferService:!event handling!public! !
!URLPresenter categoriesFor: #queryInterface:!accessing!public! !
!URLPresenter categoriesFor: #queryInterface:ifNone:!accessing!public! !
!URLPresenter categoriesFor: #serviceProvider!event handling!public! !
!URLPresenter categoriesFor: #supportedInterfaces!constants!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

Favorite guid: (GUID fromString: '{364804CD-4B2B-4021-AF7B-B45E87C0F967}')!
Favorite comment: ''!
!Favorite categoriesForClass!Unclassified! !
!Favorite methodsFor!

<= aFavorite

^self description <= aFavorite description!

= aFavorite
^path = aFavorite path!

description
	^File splitStemFrom:  path!

displayOn: aStream
aStream nextPutAll:  self description!

hash
^path hash!

isFolder
	^false!

parent
	^parent!

path
^path!

printOn: target
super printOn: target.
target nextPutAll: ' (' , self description , ')'!

setParent: aFavoriteFolder 
	parent := aFavoriteFolder!

setPath: aPath
path := aPath!

url
	| url stream |
	url := nil.
	
	stream := FileStream read: path text: true.
	
	[[stream atEnd and: [url notNil]] whileFalse: 
			[|line|line := stream nextLine.
			(line beginsWith: 'URL=') ifTrue: [url := line copyFrom: 5]]] 
			ensure: [stream close].
	^url! !
!Favorite categoriesFor: #<=!public! !
!Favorite categoriesFor: #=!public! !
!Favorite categoriesFor: #description!public! !
!Favorite categoriesFor: #displayOn:!printing!public! !
!Favorite categoriesFor: #hash!public! !
!Favorite categoriesFor: #isFolder!public!testing! !
!Favorite categoriesFor: #parent!public! !
!Favorite categoriesFor: #path!public! !
!Favorite categoriesFor: #printOn:!printing!public! !
!Favorite categoriesFor: #setParent:!private! !
!Favorite categoriesFor: #setPath:!private! !
!Favorite categoriesFor: #url!public! !

!Favorite class methodsFor!

path: aPath 
	^(super new)
		setPath: aPath;
		yourself!

path: aPath parent: aFavoriteFolder 
	^(super new)
		setPath: aPath;
		setParent: aFavoriteFolder;
		yourself! !
!Favorite class categoriesFor: #path:!public! !
!Favorite class categoriesFor: #path:parent:!public! !

RemotableHandle guid: (GUID fromString: '{F4AD9768-57CD-482A-9CA2-ED911D1CDEF9}')!
RemotableHandle comment: ''!
!RemotableHandle categoriesForClass!Unclassified! !
IAuthenticate guid: (IID fromString: '{79EAC9D0-BAF9-11CE-8C82-00AA004BA90B}')!
IAuthenticate comment: '<IAuthenticate> is a wrapper class for the COM interface ''ShellObjects.IAuthenticate'' generated from type information in the ''Microsoft Shell Objects'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains no documentation for this interface

Warning: This comment was automatically generated from the interface''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

[
	object, 
	uuid(79EAC9D0-BAF9-11CE-8C82-00AA004BA90B)
]
interface IAuthenticate : IUnknown
 {
	[id(0x60010000)]
	HRESULT __stdcall Authenticate(
		[out]wireHWND* phwnd,
		[out]LPWSTR* pszUsername,
		[out]LPWSTR* pszPassword);
};
'!
!IAuthenticate categoriesForClass!Unclassified! !
!IAuthenticate methodsFor!

authenticate
	"Invoke the Authenticate() method of the COM object."

	| answer |
	answer := (Array new: 3)
				basicAt: 1 put: (RemotableHandle newBufferClass: COMTaskMemory);
				basicAt: 2 put: (StructurePointer newNull: COMTaskMemory elementClass: UnicodeString);
				basicAt: 3 put: (StructurePointer newNull: COMTaskMemory elementClass: UnicodeString);
				yourself.
	self
		Authenticate: (answer basicAt: 1)
		pszUsername: (answer basicAt: 2)
		pszPassword: (answer basicAt: 3).
	^answer collect: [:each | each asObject]
!

Authenticate: phwnd pszUsername: pszUsername pszPassword: pszPassword
	"Private - Invoke the Authenticate() method of the COM object.

		HRESULT __stdcall Authenticate(
			[out]wireHWND* phwnd,
			[out]LPWSTR* pszUsername,
			[out]LPWSTR* pszPassword);"

	<virtual stdcall: hresult 4 RemotableHandle** lpwstr* lpwstr*>
	^self invalidCall! !
!IAuthenticate categoriesFor: #authenticate!**auto generated**!methods!public! !
!IAuthenticate categoriesFor: #Authenticate:pszUsername:pszPassword:!**auto generated**!COM Interfaces-IAuthenticate!private! !

!IAuthenticate class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'ShellObjects.IAuthenticate'
		IAuthenticate defineTemplate"

	self
		defineFunction: #Authenticate:pszUsername:pszPassword:
			argumentTypes: 'RemotableHandle** lpwstr* lpwstr*'
! !
!IAuthenticate class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !

IProfferService guid: (IID fromString: '{CB728B20-F786-11CE-92AD-00AA00A74CD0}')!
IProfferService comment: '<IProfferService> is a wrapper class for the COM interface ''ShellObjects.IProfferService'' generated from type information in the ''Microsoft Shell Objects'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains no documentation for this interface

Warning: This comment was automatically generated from the interface''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

[
	object, 
	uuid(CB728B20-F786-11CE-92AD-00AA00A74CD0)
]
interface IProfferService : IUnknown
 {
	[id(0x60010000)]
	HRESULT __stdcall ProfferService(
		[in]GUID* guidService,
		[in]IServiceProvider* psp,
		[out]unsigned long* pdwCookie);
	[id(0x60010001)]
	HRESULT __stdcall RevokeService(
		[in]unsigned long dwCookie);
};
'!
!IProfferService categoriesForClass!Unclassified! !
!IProfferService methodsFor!

profferService: guidService psp: psp
	"Answer the <DWORD> result of invoking the ProfferService() method of the COM object."

	| answer |
	answer := (DWORD new).
	self
		ProfferService: guidService
		psp: psp
		pdwCookie: answer.
	^answer asObject
!

ProfferService: guidService psp: psp pdwCookie: pdwCookie
	"Private - Invoke the ProfferService() method of the COM object.

		HRESULT __stdcall ProfferService(
			[in]GUID* guidService,
			[in]IServiceProvider* psp,
			[out]unsigned long* pdwCookie);"

	<virtual stdcall: hresult 4 GUID* IServiceProvider* dword*>
	^self invalidCall!

revokeService: dwCookie
	"Invoke the RevokeService() method of the COM object."

	^self RevokeService: dwCookie
!

RevokeService: dwCookie
	"Private - Invoke the RevokeService() method of the COM object.

		HRESULT __stdcall RevokeService(
			[in]unsigned long dwCookie);"

	<virtual stdcall: hresult 5 dword>
	^self invalidCall! !
!IProfferService categoriesFor: #profferService:psp:!**auto generated**!methods!public! !
!IProfferService categoriesFor: #ProfferService:psp:pdwCookie:!**auto generated**!COM Interfaces-IProfferService!private! !
!IProfferService categoriesFor: #revokeService:!**auto generated**!methods!public! !
!IProfferService categoriesFor: #RevokeService:!**auto generated**!COM Interfaces-IProfferService!private! !

!IProfferService class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'ShellObjects.IProfferService'
		IProfferService defineTemplate"

	self
		defineFunction: #ProfferService:psp:pdwCookie:
			argumentTypes: 'GUID* IServiceProvider* dword*';
		defineFunction: #RevokeService:
			argumentTypes: 'dword'
! !
!IProfferService class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !

IProtectFocus guid: (IID fromString: '{D81F90A3-8156-44F7-AD28-5ABB87003274}')!
IProtectFocus comment: '<IProtectFocus> is a wrapper class for the COM interface ''ShellObjects.IProtectFocus'' generated from type information in the ''Microsoft Shell Objects'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains no documentation for this interface

Warning: This comment was automatically generated from the interface''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

[
	object, 
	uuid(D81F90A3-8156-44F7-AD28-5ABB87003274)
]
interface IProtectFocus : IUnknown
 {
	[id(0x60010000)]
	HRESULT __stdcall AllowFocusChange(
		[out]long* pfAllow);
};
'!
!IProtectFocus categoriesForClass!Unclassified! !
!IProtectFocus methodsFor!

allowFocusChange
	"Answer the <SDWORD> result of invoking the AllowFocusChange() method of the COM object."

	| answer |
	answer := (SDWORD new).
	self AllowFocusChange: answer.
	^answer asObject
!

AllowFocusChange: pfAllow
	"Private - Invoke the AllowFocusChange() method of the COM object.

		HRESULT __stdcall AllowFocusChange(
			[out]long* pfAllow);"

	<virtual stdcall: hresult 4 sdword*>
	^self invalidCall! !
!IProtectFocus categoriesFor: #allowFocusChange!**auto generated**!methods!public! !
!IProtectFocus categoriesFor: #AllowFocusChange:!**auto generated**!COM Interfaces-IProtectFocus!private! !

!IProtectFocus class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'ShellObjects.IProtectFocus'
		IProtectFocus defineTemplate"

	self
		defineFunction: #AllowFocusChange:
			argumentTypes: 'sdword*'
! !
!IProtectFocus class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !

FavoriteFolder guid: (GUID fromString: '{359A72FE-2B76-423C-A6BE-80FC78ABB368}')!
FavoriteFolder comment: ''!
!FavoriteFolder categoriesForClass!Unclassified! !
!FavoriteFolder methodsFor!

children
	| children |
	children := OrderedCollection new.
	File
		forDirectoriesIn: path
			do: [:finddata | children add: (FavoriteFolder path: finddata path parent: self)];
		for: '*.url'
			in: path
			do: [:finddata | children add: (Favorite path: finddata path parent: self)].
	^children asSortedCollection!

isFolder
 ^true! !
!FavoriteFolder categoriesFor: #children!public! !
!FavoriteFolder categoriesFor: #isFolder!public!testing! !

!FavoriteFolder class methodsFor!

root
^self path: SpecialFolderRelativeFileLocator favorites basePath! !
!FavoriteFolder class categoriesFor: #root!public! !

FavoritesTreeModel guid: (GUID fromString: '{AE4EBFA5-79E2-4058-92E4-168F68A97343}')!
FavoritesTreeModel comment: ''!
!FavoritesTreeModel categoriesForClass!Unclassified! !
!FavoritesTreeModel methodsFor!

defaultGetChildrenBlock
	^[:aFavoriteFolder | aFavoriteFolder isFolder ifTrue: [aFavoriteFolder children] ifFalse: [Array new]]!

defaultHasChildrenBlock
	^[:aFavorite | aFavorite isFolder]! !
!FavoritesTreeModel categoriesFor: #defaultGetChildrenBlock!initializing!private! !
!FavoritesTreeModel categoriesFor: #defaultHasChildrenBlock!initializing!private! !

!FavoritesTreeModel class methodsFor!

withAllFavorites
	"Answer an instance of the receiver on the entire class hiearchy"

	^self withRoots: FavoriteFolder root children! !
!FavoritesTreeModel class categoriesFor: #withAllFavorites!instance creation!public! !

URLPresenterWithContextMenu guid: (GUID fromString: '{7009D7FB-46C4-4FC9-80E0-586A378F766D}')!
URLPresenterWithContextMenu comment: ''!
!URLPresenterWithContextMenu categoriesForClass!Unclassified! !
!URLPresenterWithContextMenu methodsFor!

EnableModeless: enableModeless 
	"Implement the IDocHostUIHandlerDispatch::EnableModeless() interface function."

	^E_NOTIMPL.
!

FilterDataObject: filterDataObject ppDORet: ppDORet 
	"Implement the IDocHostUIHandlerDispatch::FilterDataObject() interface function."

	^E_NOTIMPL.
	!

GetDropTarget: getDropTarget ppDropTarget: ppDropTarget 
	"Implement the IDocHostUIHandlerDispatch::GetDropTarget() interface function."

	^E_NOTIMPL.
!

GetExternal: getExternal 
	"Implement the IDocHostUIHandlerDispatch::GetExternal() interface function."

	^E_NOTIMPL.
	!

GetHostInfo: getHostInfo pdwDoubleClick: pdwDoubleClick 
	"Implement the IDocHostUIHandlerDispatch::GetHostInfo() interface function."

	^E_NOTIMPL.
	!

GetOptionKeyPath: getOptionKeyPath dw: dw 
	"Implement the IDocHostUIHandlerDispatch::GetOptionKeyPath() interface function."

	^E_NOTIMPL.
!

HideUI
	"Implement the IDocHostUIHandlerDispatch::HideUI() interface function."

	^E_NOTIMPL.
!

OnDocWindowActivate: onDocWindowActivate 
	"Implement the IDocHostUIHandlerDispatch::OnDocWindowActivate() interface function."

	^E_NOTIMPL.
!

OnFrameWindowActivate: onFrameWindowActivate 
	"Implement the IDocHostUIHandlerDispatch::OnFrameWindowActivate() interface function."

	^E_NOTIMPL.
!

onViewOpened
	"Received when the receiver's view is been connected. "

	self view hostInterface SetExternalUIHandler: (self queryInterface: IDocHostUIHandlerDispatch).
	
	super onViewOpened.
	!

ResizeBorder: resizeBorder top: top right: right bottom: bottom pUIWindow: pUIWindow fFrameWindow: fFrameWindow 
	"Implement the IDocHostUIHandlerDispatch::ResizeBorder() interface function."

	^E_NOTIMPL.
!

ShowContextMenu: showContextMenu x: x y: y pcmdtReserved: pcmdtReserved pdispReserved: pdispReserved dwRetVal: dwRetVal 
	"Implement the IDocHostUIHandlerDispatch::ShowContextMenu() interface function."

"Only display a context menu if #allowContextMenu is true"

	view allowContextMenu 
		ifFalse: 
			[dwRetVal value: false.
			^S_OK].
		"Either display the context menu #contextMenu or, if #contextMenu is nil, the default IE menu"
	view contextMenu 
		ifNil: [dwRetVal value: true]
		ifNotNil: 
			[:menu | 
			menu showIn: view position: x @ y.
			dwRetVal value: false].
	^S_OK!

ShowUI: showUI pActiveObject: pActiveObject pCommandTarget: pCommandTarget pFrame: pFrame pDoc: pDoc dwRetVal: dwRetVal 
	"Implement the IDocHostUIHandlerDispatch::ShowUI() interface function."

	^E_NOTIMPL.
	!

supportedInterfaces
	"Answer the set of interface classes supported by the receiver.
	We need only support the standard property notification sink interface since
	all other control container requirements are satisfied by the ATL host window
	that we are wrapping."

	^super supportedInterfaces copyWith: IDocHostUIHandlerDispatch!

TranslateAccelerator: translateAccelerator nMessage: nMessage wParam: wParam lParam: lParam bstrGuidCmdGroup: bstrGuidCmdGroup nCmdID: nCmdID dwRetVal: dwRetVal 
	"Implement the IDocHostUIHandlerDispatch::TranslateAccelerator() interface function."

	^E_NOTIMPL!

TranslateUrl: translateUrl bstrURLIn: bstrURLIn pbstrURLOut: pbstrURLOut 
	"Implement the IDocHostUIHandlerDispatch::TranslateUrl() interface function."

	^E_NOTIMPL.
!

UpdateUI
	"Implement the IDocHostUIHandlerDispatch::UpdateUI() interface function."

	^E_NOTIMPL.
! !
!URLPresenterWithContextMenu categoriesFor: #EnableModeless:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #FilterDataObject:ppDORet:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #GetDropTarget:ppDropTarget:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #GetExternal:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #GetHostInfo:pdwDoubleClick:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #GetOptionKeyPath:dw:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #HideUI!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #OnDocWindowActivate:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #OnFrameWindowActivate:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #onViewOpened!event handling!public! !
!URLPresenterWithContextMenu categoriesFor: #ResizeBorder:top:right:bottom:pUIWindow:fFrameWindow:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #ShowContextMenu:x:y:pcmdtReserved:pdispReserved:dwRetVal:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #ShowUI:pActiveObject:pCommandTarget:pFrame:pDoc:dwRetVal:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #supportedInterfaces!constants!public! !
!URLPresenterWithContextMenu categoriesFor: #TranslateAccelerator:nMessage:wParam:lParam:bstrGuidCmdGroup:nCmdID:dwRetVal:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #TranslateUrl:bstrURLIn:pbstrURLOut:!COM Interfaces-IDocHostUIHandlerDispatch!public! !
!URLPresenterWithContextMenu categoriesFor: #UpdateUI!COM Interfaces-IDocHostUIHandlerDispatch!public! !

"Binary Globals"!

