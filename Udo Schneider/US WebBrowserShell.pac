| package |
package := Package name: 'US WebBrowserShell'.
package paxVersion: 1;
	basicComment: '$id: US WebBrowserShell 0.003$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.003'.


package classNames
	add: #USWebBrowserShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\ActiveX\Automation\ActiveX Automation';
	add: '..\..\Object Arts\Dolphin\ActiveX\Connection Points\ActiveX Connection Points';
	add: '..\..\Object Arts\Dolphin\ActiveX\OCX\ActiveX Control Hosting';
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\..\Object Arts\Dolphin\ActiveX\Components\SHDocVw\Internet Explorer';
	add: '..\..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	add: 'US Internet Explorer Extensions';
	add: '..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

SmalltalkToolShell subclass: #USWebBrowserShell
	instanceVariableNames: 'address browser statusModel commandStates'
	classVariableNames: ''
	poolDictionaries: 'SHDocVwConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

USWebBrowserShell guid: (GUID fromString: '{EB412672-D927-4852-B0D4-C129846B7D3E}')!
USWebBrowserShell comment: ''!
!USWebBrowserShell categoriesForClass!Unclassified! !
!USWebBrowserShell methodsFor!

browseDefinitions
browser getSelectedTextOrNil ifNotNil: [:text |
	SmalltalkSystem current browseDefinitionsOf: text] !

browseIt
^self evaluateIt browse!

browseReferences
	browser getSelectedTextOrNil 
		ifNotNil: 
			[:text | 
			| object |
			object := Smalltalk at: text asSymbol ifAbsent: [text asSymbol ].
			
			SmalltalkSystem current browseReferencesTo: object]!

buildFavoritesMenu: aMenu 
	aMenu clear.
	FavoritesTreeModel withAllFavorites 
		addToMenu: aMenu
		description: [:each | each description]
		command: [:each | Message selector: #openFavorite: argument: each]
		image: [:each | nil]!

controlDispatch
^browser view controlDispatch!

createComponents
	"Create the presenters contained by the receiver"

	super createComponents.
	statusModel := ValueHolder new.
	browser := self add: URLPresenterWithContextMenu new name: 'browser'.
	address := self add: TextPresenter new name: 'address'.
	"Share the model to simplify matters, although independent models would allow more control."
	address model: browser model!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	address 
		when: #valueChanged
		send: #invalidateUserInterface
		to: self.

	"Automatically update the caption and status bar text on receipt of events from the browser control"
	browser
		when: #StatusTextChange:
			send: #value:
			to: statusModel;
		when: #TitleChange:
			send: #caption:
			to: self.

	"Wire up a few of the events for illustrative purposes - we don't do anything with most of them at present"
	browser
		when: #NavigateComplete2:URL:
			send: #OnNavigateComplete2:URL:
			to: self;
		when: #DocumentComplete:URL:
			send: #OnDocumentComplete:URL:
			to: self;
		when: #CommandStateChange:Enable:
			send: #OnCommandStateChange:Enable:
			to: self;
		when: #BeforeNavigate2:URL:Flags:TargetFrameName:PostData:Headers:Cancel:
			send: #OnBeforeNavigate2:URL:Flags:TargetFrameName:PostData:Headers:Cancel:
			to: self;
		when: #NewWindow2:Cancel:
			send: #OnNewWindow2:Cancel:
			to: self!

evaluateIt
	
	^browser getSelectedTextOrNil 
		ifNotNil: [:text | [Compiler evaluate: text] on: Error do: [:ex | self]]!

historyBack: delta 
	"Go back the number of positions in the history specified by the <integer> argument."

	self controlDispatch GoBack!

historyForward: delta 
	"Go forward the number of positions in the history specified by the <integer> argument."

	self controlDispatch GoForward!

home
	"Go to the Home page"

	self openUrl: self homePage!

homePage
	"Private - Answer the home page to use for the receiver"

	^SessionManager current objectArtsUrl!

initialize
	"Private - Initialize the receiver's instance variables."

	super initialize.
	commandStates := Array with: false with: false!

inspectIt
	^self evaluateIt inspect!

navigate
	"Navigate to the address in the address bar.
	We want to by-pass the normal check for the same value here."

	(browser model)
		setValue: address value;
		notifyChanged!

onAboutToDisplayMenu: aMenu 
	"The pop-up <Menu>, popup, is about to be displayed.
	This is our opportunity to update it, e.g. to add/remove items."

	| menuName |
	super onAboutToDisplayMenu: aMenu.
	menuName := aMenu name.
	menuName == #favoritesMenu ifTrue: [^self buildFavoritesMenu: aMenu]!

OnBeforeNavigate2: anIDispatch URL: urlVARIANT Flags: flagsVARIANT TargetFrameName: targetVARIANT PostData: postDataVARIANT Headers: headersVARIANT Cancel: aBooleanValue 
	"Can be a URL or a PIDL"

	"Pre-navigation event fired by the control. Can be used to take control of what happens when
	a link is clicked. Be aware that <IDispatch> argument is not necessarily on a WebBrowser
	object, since this event is also fired for frames. Note also that programmatic navigations
	also fire this event, so if you intercept and redirect, this event will be fired again."

	!

OnCommandStateChange: anInteger Enable: aBoolean 
	"Event fired by the control when the enablement of a command changes.
	This tells us whether the forward/back buttons need to be enabled, and also
	whether any other toolbar buttons enablement state may have changed."

	(anInteger between: 1 and: commandStates size) ifTrue: [commandStates at: anInteger put: aBoolean].
	self invalidateUserInterface!

OnDocumentComplete: anIDispatch URL: urlOrPIDL 
	"Event fired by the control when the document download and display has completed."

!

OnNavigateComplete2: anIDispatch URL: urlOrPIDL 
	"Event fired by the control when the document being navigated to becomes visible and enters the navigation
	stack. If we wanted to maintain a history list, this would be the place to do so."

	"If we had independent models for the address bar and URLPresenter, this would be the place to update
	 the address bar model."

	(anIDispatch queryInterface: IDispatch) = (self controlDispatch queryInterface: IDispatch) 
		ifTrue: 
			[| url sslStatusItem |
			url := urlOrPIDL value.
			address value: url.
			sslStatusItem := self view viewNamed: 'ssl' ifNone: [].
			sslStatusItem ifNotNil: [:item | item model: (url beginsWith: 'https') asValue]]!

OnNewWindow2: ppDispOut Cancel: aBooleanValue 
	"Event fired by the control when about to open a new window.
	N.B. Both parameters are output parameters. See MSDN."

	| shell |

	shell := USWebBrowserShell show.
	ppDispOut value: shell controlDispatch yourAddress!

onViewOpened
	"Received when the receiver's view is been connected. "

	| statusItem |
	super onViewOpened.
	statusItem := self view viewNamed: 'status' ifNone: [].
	statusItem model: statusModel!

openFavorite: aFavorite 
	self openUrl: aFavorite url!

openUrl: url 
	"Browse the specified URL."

	browser model value: url asString!

queryCommand: query 
	"Enters details about a potential command for the receiver into the 
	<CommandQuery>,  query."

	| cmd |
	cmd := query commandSymbol.
	
	cmd == #historyBack: 
		ifTrue: 
			[query isEnabled: (commandStates at: CSC_NAVIGATEBACK).
			^true].
	cmd == #historyForward: 
		ifTrue: 
			[query isEnabled: (commandStates at: CSC_NAVIGATEFORWARD).
			^true].
	(#(#browseIt #evaluateIt #inspectIt #browseReferences #browseDefinitions) includes: cmd) 
		ifTrue: 
			[query isEnabled: browser getSelectedTextOrNil notNil.
			^true].
	cmd == #favoritesMenu 
		ifTrue: 
			[query isEnabled: true.
			^true].
	^super queryCommand: query!

shortCaption
	^self caption size > 20 ifFalse: [self caption ] ifTrue: [(self caption  copyFrom: 1 to: 20) , '...' ]! !
!USWebBrowserShell categoriesFor: #browseDefinitions!commands!public! !
!USWebBrowserShell categoriesFor: #browseIt!commands!public! !
!USWebBrowserShell categoriesFor: #browseReferences!commands!public! !
!USWebBrowserShell categoriesFor: #buildFavoritesMenu:!helpers!private! !
!USWebBrowserShell categoriesFor: #controlDispatch!helpers!private! !
!USWebBrowserShell categoriesFor: #createComponents!initializing!public! !
!USWebBrowserShell categoriesFor: #createSchematicWiring!initializing!public! !
!USWebBrowserShell categoriesFor: #evaluateIt!commands!public! !
!USWebBrowserShell categoriesFor: #historyBack:!commands!public! !
!USWebBrowserShell categoriesFor: #historyForward:!commands!public! !
!USWebBrowserShell categoriesFor: #home!commands!public! !
!USWebBrowserShell categoriesFor: #homePage!commands!private! !
!USWebBrowserShell categoriesFor: #initialize!initializing!private! !
!USWebBrowserShell categoriesFor: #inspectIt!commands!public! !
!USWebBrowserShell categoriesFor: #navigate!commands!public! !
!USWebBrowserShell categoriesFor: #onAboutToDisplayMenu:!event handling!menus!public! !
!USWebBrowserShell categoriesFor: #OnBeforeNavigate2:URL:Flags:TargetFrameName:PostData:Headers:Cancel:!event handling!public! !
!USWebBrowserShell categoriesFor: #OnCommandStateChange:Enable:!event handling!public! !
!USWebBrowserShell categoriesFor: #OnDocumentComplete:URL:!event handling!public! !
!USWebBrowserShell categoriesFor: #OnNavigateComplete2:URL:!event handling!public! !
!USWebBrowserShell categoriesFor: #OnNewWindow2:Cancel:!event handling!public! !
!USWebBrowserShell categoriesFor: #onViewOpened!event handling!public! !
!USWebBrowserShell categoriesFor: #openFavorite:!commands!public! !
!USWebBrowserShell categoriesFor: #openUrl:!operations!public! !
!USWebBrowserShell categoriesFor: #queryCommand:!commands!public! !
!USWebBrowserShell categoriesFor: #shortCaption!accessing!public! !

!USWebBrowserShell class methodsFor!

defaultModel
	^'about:blank' asValue!

displayOn: aStream 
	aStream nextPutAll: 'Web Browser'!

icon
	"Answers an Icon that can be used to represent this class"

	^Icon fromId: 14 in: ShellLibrary default!

initialize
	"Private - Initialize the receiver. Register the tools folder icon with SmalltalkSystem.

		self initialize.
	"

	super initialize.
	self canUseIdeaSpace: true.
	(Smalltalk developmentSystem)
		addAdditionalToolsFolderIcon: (SmalltalkSystemIcon 
					show: self
					description: self displayString
					helpId: self toolsFolderHelpId);
		registerTool: self!

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by the receiver."

	| aspects |
	aspects := super publishedAspects.

	aspects add: (Aspect boolean: #canUseIdeaSpace).
	^aspects!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 551 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409289036 131137 560 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 560 482 656 8 4294902165 234 256 98 2 410 576 98 25 0 560 98 2 8 1409289036 131137 848 0 482 656 0 519 0 674 0 16 706 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 754 193 193 0 848 482 656 8 4294902165 234 256 98 0 234 256 1040 202 208 1040 234 240 1040 0 1 0 754 33 33 754 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 754 1 1 754 1915 51 848 1234 8 #updateSize 1040 848 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 189 3 0 0 25 0 0 0] 98 2 410 8 ##(Smalltalk.ReferenceView)  98 14 0 848 98 2 8 1140850688 131073 1424 0 482 8 4278190080 0 7 0 0 0 1424 1180166 ##(Smalltalk.ResourceIdentifier)  576 8 #resource_Image_tools 0 1170 202 208 98 1 1234 1264 98 2 754 1 1 754 63 51 1424 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 31 0 0 0 25 0 0 0] 1040 754 193 193 0 27 410 1440 98 14 0 848 98 2 8 1140850688 131073 1744 0 482 1520 0 7 0 0 0 1744 1538 576 8 #resource_Smalltalk_tools 0 1170 202 208 98 1 1234 1264 98 2 754 63 1 754 991 51 1744 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 31 0 0 0 0 0 0 0 14 2 0 0 25 0 0 0] 1040 1728 0 27 1728 0 27 8 'toolbar' 234 256 1040 202 208 1040 234 240 1040 0 1 0 754 33 33 754 45 45 0 530 1 1 848 410 576 98 25 0 560 98 2 8 1409289036 131137 2112 0 482 656 0 519 0 674 0 16 706 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 754 193 193 0 2112 482 656 8 4294902165 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 2112 98 2 8 1140916352 1025 2320 0 482 512 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 2 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #navigate 8 '&Go' 1 1 0 0 0 2466 2097153 2498 8 #accept 8 '&Accept' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 2320 0 8 4294903121 852486 ##(Smalltalk.NullConverter)  0 0 1 1170 202 208 98 4 1234 1264 98 2 754 367 5 754 1409 39 2320 1234 8 #contextMenu: 98 1 2432 2320 1234 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2320 1234 8 #isTextModified: 98 1 32 2320 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 183 0 0 0 2 0 0 0 119 3 0 0 21 0 0 0] 98 0 1728 0 27 8 'address' 410 8 ##(Smalltalk.PushButton)  98 17 0 2112 98 2 8 1140924416 1 3040 0 0 754 97 41 519 0 0 0 3040 0 8 4294902779 2498 2528 8 '&Go' 1 1 0 0 16 1170 202 208 98 3 1234 1264 98 2 754 1785 5 754 121 39 3040 1234 8 #isEnabled: 98 1 32 3040 1234 8 #text: 98 1 8 '&Go' 3040 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 124 3 0 0 2 0 0 0 184 3 0 0 21 0 0 0] 98 0 1728 0 27 8 'go' 234 256 1040 202 208 1040 234 240 1040 0 1 0 754 33 33 754 45 45 0 852230 ##(Smalltalk.FramingLayout)  234 240 98 8 2320 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedPreviousRight 1 3658 3680 8 #fixedParentRight -139 3658 3680 8 #fixedParentTop 5 3658 3680 8 #fixedParentBottom -7 410 576 98 25 0 2112 98 2 8 1140853580 131137 3808 0 482 512 0 519 0 0 0 3808 482 512 8 4294902165 234 256 1040 234 256 98 10 24777 1246982 ##(Smalltalk.ToolbarSystemButton)  24777 0 3808 1 2498 459270 ##(Smalltalk.Message)  8 #historyBack: 98 1 3 8 'Back' 1 1 0 17 1 24779 3970 24779 0 3808 1 2498 4018 8 #historyForward: 98 1 3 8 'Forward' 1 1 0 17 3 24781 1115910 ##(Smalltalk.ToolbarIconButton)  24781 0 3808 1 2498 8 #reload 8 'Reload' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 3658 8 ##(Smalltalk.ImageRelativeFileLocator)  8 #current 8 'Refresh.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 3 754 33 33 1 24783 4194 24783 0 3808 1 2498 8 #stopLoading 8 'Stop Loading' 1 1 4274 0 16 4304 8 'COMPILEFAILEDMETHOD.ICO' 4384 4418 0 16 0 0 0 0 3 754 33 33 1 24785 853766 ##(Smalltalk.ToolbarButton)  24785 0 3808 1 2498 8 #home 8 'Home page' 1 1 0 4418 0 16 4304 8 'Tools.bmp' 4384 0 71 754 1857 33 73 98 7 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 3808 3 0 1 3984 4096 4208 4464 4608 4738 0 0 3808 3 0 1 234 240 98 8 4560 13 4432 11 4672 15 17 1 0 1 0 754 33 33 754 45 45 0 0 1170 202 208 98 2 1234 1264 98 2 754 1 1 754 257 51 3808 1234 1344 1040 3808 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 0 0 0 25 0 0 0] 98 0 1728 0 27 3618 3658 3680 8 #fixedParentLeft 1 5040 257 3744 1 3776 1 3040 3618 3712 -129 3712 -9 3744 5 3776 -7 410 8 ##(Smalltalk.StaticText)  98 16 0 2112 98 2 8 1140850944 1 5088 0 0 0 7 0 0 0 5088 0 8 4294902543 2658 0 0 0 1170 202 208 98 2 1234 1264 98 2 754 257 5 754 111 39 5088 1234 3360 98 1 8 'Address:' 5088 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 128 0 0 0 2 0 0 0 183 0 0 0 21 0 0 0] 98 0 1728 0 27 3618 3664 1 3658 3680 8 #fixedViewLeft 111 3744 5 3776 -7 1170 202 208 98 2 1234 1264 98 2 754 1 51 754 1915 51 2112 1234 1344 1040 2112 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 189 3 0 0 50 0 0 0] 98 4 3808 5088 2320 3040 1728 0 27 0 0 0 1170 202 208 98 2 1234 1264 98 2 754 1 1 754 1915 101 560 1234 1344 1040 560 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 189 3 0 0 50 0 0 0] 98 2 848 2112 1728 0 27 410 8 ##(Smalltalk.StatusBar)  98 18 0 416 98 2 8 1140853004 1 5808 0 482 8 4278190080 0 7 0 674 0 16 706 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 754 193 193 0 5808 0 8 4294902447 234 256 98 4 853766 ##(Smalltalk.StatusBarItem)  1 -1 5808 0 4018 8 #displayString 98 0 0 3658 8 ##(Smalltalk.IconImageManager)  4336 8 'status' 6034 1 129 5808 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  3 1 6176 8 'doIt' 8 '[ :each | each ifTrue: [''SSL''] ifFalse: [''No SSL'']]' 8 #[31 105 17 119 29 106 30 106] 8 'SSL' 8 'No SSL' 6192 7 257 0 6178 0 0 6210 5 1 6176 8 'doIt' 8 '[ :each | (Icon fromId: (each ifTrue: [ ''MUTEX.ICO''] ifFalse: [ 428])) imageIndex]' 8 #[33 105 29 17 119 30 112 238 172 1 178 161 106] 4272 8 'MUTEX.ICO' 8 #fromId: 8 #imageIndex 6320 7 257 0 0 8 'ssl' 98 2 6048 6160 1115142 ##(Smalltalk.StatusBarNullItem)  513 1 5808 0 0 1170 202 208 98 1 1234 1264 98 2 754 1 1039 754 1915 45 5808 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 7 2 0 0 189 3 0 0 29 2 0 0] 98 0 1728 0 27 410 576 98 25 0 416 98 2 8 1140851660 131073 6672 0 482 8 4278190080 0 7 0 674 0 16 706 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 754 193 193 0 6672 482 6752 8 4294902165 234 256 98 8 410 1440 98 14 0 6672 98 2 8 1140850688 131073 6896 0 0 0 7 0 0 0 6896 1538 576 8 #resource_File_tools 0 1170 202 208 98 1 1234 1264 98 2 754 1 1 754 51 151 6896 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 25 0 0 0 75 0 0 0] 1040 1728 0 27 8 'file tools' 410 1440 98 14 0 6672 98 2 8 1140850688 131073 7152 0 0 0 7 0 0 0 7152 1538 576 8 #resource_Workspace_tools 0 1170 202 208 98 1 1234 1264 98 2 754 1 151 754 49 195 7152 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 75 0 0 0 24 0 0 0 172 0 0 0] 1040 1728 0 27 8 'workspace tools' 410 1440 98 14 0 6672 98 2 8 1140850688 131073 7408 0 0 0 7 0 0 0 7408 1538 576 8 #resource_Edit_tools 0 1170 202 208 98 1 1234 1264 98 2 754 1 449 754 49 237 7408 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 224 0 0 0 24 0 0 0 86 1 0 0] 1040 1728 0 27 8 'edit tools' 410 1440 98 14 0 6672 98 2 8 1140850688 131073 7664 0 0 0 7 0 0 0 7664 1538 576 8 #resource_Find_tools 0 1170 202 208 98 1 1234 1264 98 2 754 1 345 754 51 105 7664 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 172 0 0 0 25 0 0 0 224 0 0 0] 1040 1728 0 27 8 'find tools' 234 256 1040 98 0 234 240 1040 0 1 0 754 33 33 754 45 45 0 1138 1 1 1 1170 202 208 98 2 1234 1264 98 2 754 1865 101 754 51 939 6672 1234 1344 1040 6672 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 164 3 0 0 50 0 0 0 189 3 0 0 7 2 0 0] 98 4 6896 7152 7664 7408 1728 0 27 0 410 8 ##(Smalltalk.AXValueConvertingControlSite)  98 26 0 416 98 2 8 1140850688 1 8192 721990 2 ##(Smalltalk.ValueHolder)  0 32 1376774 ##(Smalltalk.PluggableSearchPolicy)  4018 8 #= 98 0 4018 8 #hash 98 0 0 482 512 0 7 2418 0 16 98 6 2466 1 2498 8 #browseIt 8 '&Browse It' 9349 1 4274 0 16 4304 8 'ClassBrowserShell.ico' 4384 0 0 2466 1 2498 8 #evaluateIt 8 '&Evaluate It' 9355 1 4274 0 16 4304 8 'EvaluateIt.ico' 4384 0 0 2466 1 2498 8 #inspectIt 8 '&Inspect It' 9363 1 4274 0 16 4304 8 'BasicInspector.ico' 4384 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 2466 1 2498 8 #browseDefinitions 8 'Defi&nitions...' 1271 1 0 0 0 2466 1 2498 8 #browseReferences 8 '&References...' 5367 1 0 0 0 8 '&Workspace' 0 134217729 0 0 0 0 0 0 0 8192 0 52175649 8 'Shell.Explorer' 787462 ##(Smalltalk.IWebBrowser2)  0 0 1444166 1 ##(Smalltalk.TKindInterfaceAnalyzer)  590598 ##(Smalltalk.ITypeInfo)  0 0 525062 ##(Smalltalk.TYPEATTR)  0 9040 9008 0 1378630 2 ##(Smalltalk.AXTypeLibraryAnalyzer)  590342 ##(Smalltalk.ITypeLib2)  0 0 257 524550 ##(Smalltalk.TLIBATTR)  8 #[192 42 178 234 193 48 207 17 167 235 0 0 192 91 174 11 0 0 0 0 1 0 0 0 1 0 1 0 8 0 0 0] 8 '' 8 'Internet Explorer' 8 #SHDocVwLib 8 #SHDocVwConstants 234 240 98 2 8 #GUID 9296 0 0 0 11 0 0 0 0 918022 ##(Smalltalk.IDolphinAxHost)  0 0 722438 ##(Smalltalk.AXEventSink)  234 240 98 70 565 8 #SetPhishingFilterStatus: 567 8 #WindowStateChanged:dwValidFlagsMask: 205 8 #StatusTextChange: 209 8 #DownloadComplete 211 8 #CommandStateChange:Enable: 213 8 #DownloadBegin 217 8 #ProgressChange:ProgressMax: 501 8 #BeforeNavigate2:URL:Flags:TargetFrameName:PostData:Headers:Cancel: 503 8 #NewWindow2:Cancel: 505 8 #NavigateComplete2:URL: 225 8 #PropertyChange: 227 8 #TitleChange: 507 8 #OnQuit 509 8 #OnVisible: 511 8 #OnToolBar: 513 8 #OnMenuBar: 519 8 #DocumentComplete:URL: 515 8 #OnStatusBar: 517 8 #OnFullScreen: 521 8 #OnTheaterMode: 525 8 #WindowSetResizable: 529 8 #WindowSetLeft: 531 8 #WindowSetTop: 533 8 #WindowSetWidth: 535 8 #WindowSetHeight: 527 8 #WindowClosing:Cancel: 537 8 #ClientToHostWindow:CY: 539 8 #SetSecureLockIcon: 541 8 #FileDownload:Cancel: 543 8 #NavigateError:URL:Frame:StatusCode:Cancel: 451 8 #PrintTemplateInstantiation: 453 8 #PrintTemplateTeardown: 455 8 #UpdatePageStatus:nPage:fDone: 545 8 #PrivacyImpactedStateChange: 547 8 #NewWindow3:Cancel:dwFlags:bstrUrlContext:bstrUrl: 8192 1049094 ##(Smalltalk.IConnectionPoint)  0 0 3 1378630 1 ##(Smalltalk.TKindDispatchAnalyzer)  9026 0 0 9058 0 10032 10016 0 9104 0 262198 ##(Smalltalk.GUID)  16 160 21 167 52 135 101 208 17 146 74 0 32 175 199 172 77 23 0 0 0 1 3787 234 240 1040 1508358 ##(Smalltalk.IAxWinAmbientDispatchEx)  0 0 0 0 524806 ##(Smalltalk.IUnknown)  0 0 0 0 0 2658 0 0 1170 202 208 98 5 1234 1264 98 2 754 1 101 754 1865 939 8192 1234 8 #restoreAmbientProperties 1040 8192 1234 8 #docHostFlags: 98 1 9 8192 1234 8 #controlBinaryStoreBytes: 98 1 8 #[97 249 86 136 10 52 208 17 169 107 0 192 79 215 5 162 76 0 0 0 83 96 0 0 121 48 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 76 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 224 208 87 0 115 53 207 17 174 105 8 0 43 46 18 98 8 0 0 0 0 0 0 0 76 0 0 0 1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70 128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 8192 1234 2816 98 1 8448 8192 1362 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 50 0 0 0 164 3 0 0 7 2 0 0] 98 0 1728 0 27 234 256 98 6 8192 8 'browser' 6672 8 'toolbar2' 5808 8 'statusbar' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 7 2418 0 16 98 8 2466 1 2498 8 #fileNew 8 '&New' 9373 1 4274 0 16 4304 8 'FileNew.ico' 4384 0 0 2466 1 2498 8 #fileOpen 8 '&Open...' 9375 1 4274 0 16 4304 8 'FileOpen.ico' 4384 0 0 2466 1 2498 8 #fileFileIn 8 '&File In...' 1 1 0 0 0 8770 4097 2466 1 2498 8 #saveImage 8 'Sa&ve Image' 1 1 4274 0 16 4304 8 'Snapshot.ico' 4384 0 0 2466 1 2498 8 #smalltalkExit 8 'E&xit Dolphin' 1 1 4274 0 16 4304 8 'PowerSwitch.ico' 4384 0 0 8770 4097 2466 1 2498 8 #exit 8 '&Close' 17639 1 4274 0 16 4304 8 'CloseWindow.ico' 4384 0 0 8 '&File' 0 134217729 0 0 24677 0 0 2418 0 16 98 8 2466 1 2498 8 #cutSelection 8 'Cu&t' 9393 1 4274 0 16 4304 8 'EditCut.ico' 4384 0 0 2466 1 2498 8 #copySelection 8 '&Copy' 9351 1 4274 0 16 4304 8 'EditCopy.ico' 4384 0 0 2466 1 2498 8 #pasteClipboard 8 '&Paste' 9389 1 4274 0 16 4304 8 'EditPaste.ico' 4384 0 0 2466 1 2498 8 #editDelete 8 '&Delete' 1 1 4274 0 16 4304 8 'EditClear.ico' 4384 0 0 2466 1 2498 8 #selectAll 8 'Select &All' 9347 1 0 0 0 8770 4097 2466 1 2498 8 #editFind 8 '&Find...' 9357 1 4274 0 16 4304 47 3658 8 ##(Smalltalk.ShellLibrary)  8 #default 0 0 2466 1 2498 8 #findNext 8 'Find &Next' 1253 1 4274 0 16 4304 8 'FindNext.ico' 4384 0 0 8 '&Edit' 0 134217729 0 0 24693 0 0 2418 0 16 98 7 2466 1 2498 8512 8 '&Browse It' 9349 1 4274 0 16 4304 8 'ClassBrowserShell.ico' 4384 0 0 2466 1 2498 8608 8 '&Evaluate It' 9355 1 4274 0 16 4304 8 'EvaluateIt.ico' 4384 0 0 2466 1 2498 8704 8 '&Inspect It' 9363 1 4274 0 16 4304 8 'InspectIt.ico' 4384 0 0 2466 1 2498 8 #debugIt 8 'Deb&ug It' 1269 1 4274 0 16 4304 8 'Debugger.ico' 4384 0 0 8770 4097 2466 1 2498 8832 8 'Defi&nitions' 1271 1 0 0 0 2466 1 2498 8896 8 '&References' 5367 1 0 0 0 8 '&Workspace' 0 134217729 0 0 24707 0 0 2418 0 16 98 0 8 '&Tools' 8 #toolsMenu 134217729 0 0 24709 0 0 2418 0 16 98 0 8 'Favorites' 8 #favoritesMenu 134217729 0 0 24711 0 0 2418 0 16 98 0 8 'Wi&ndow' 8 #windowMenu 134217729 0 0 24713 0 0 2418 0 16 98 19 2466 1 2498 8 #helpContents 8 '&Contents' 1025 1 4274 0 16 4304 49 11856 0 0 2466 1 2498 8 #help 8 'On this &Tool' 1249 1 0 0 0 2466 1 2498 8 #helpWhatsThis 8 'What''s This?' 5345 1 0 0 0 8770 4097 2466 1 2498 8 #helpFirstSplash 8 'First Splash!!' 1 1 0 0 0 8770 4097 2466 1 2498 8 #helpWhatsNew 8 'What''s &New' 1 1 0 0 0 2466 1 2498 8 #helpGuidedTour 8 '&Guided Tour' 1 1 0 0 0 2466 1 2498 8 #helpTutorials 8 'Tutorials' 1 1 0 0 0 2418 0 16 98 4 2466 2097153 2498 8 #tipOfTheDay 8 '&Next Tip of the Day' 9441 1 4274 0 16 4304 8 'TipOfTheDay.ico' 4384 0 0 2466 1 2498 8 #previousTipOfTheDay 8 '&Previous Tip of the Day' 13537 1 4274 0 16 4304 8 'TipOfTheDay.ico' 4384 0 0 8770 4097 2466 1 2498 8 #toggleShowTipsAtStartup 8 '&Show Tips at Startup' 1 1 0 0 0 8 'Tip of the &Day' 0 134217729 0 0 24735 0 0 8770 4097 2466 1 2498 8 #objectArtsHomePage 8 'Object Arts Homepage' 1 1 0 0 0 2466 1 2498 8 #dolphinNewsgroup 8 'Dolphin Newsgroup/Forum' 1 1 0 0 0 2466 1 2498 8 #dolphinWikiWeb 8 'Dolphin WikiWeb' 1 1 0 0 0 2466 1 2498 8 #myDolphinAccount 8 'My Dolphin Account' 1 1 0 0 0 8770 4097 2466 1 2498 8 #dolphinLiveUpdate 8 'Check for Live &Updates...' 1 1 4274 0 16 4304 8 'LiveUpdate.ico' 4384 0 0 8770 4097 2466 1 2498 8 #aboutDolphin 8 '&About Dolphin Smalltalk' 1 1 4274 0 16 4304 8 '!!APPLICATION' 4384 0 0 8 '&Help' 0 134217729 0 0 24749 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 11693 0 0 0 0 1 0 0 1170 202 208 98 2 1234 1264 98 2 754 3839 21 754 1931 1191 416 1234 8 #updateMenuBar 1040 416 1362 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 68 11 0 0 93 2 0 0] 98 4 560 6672 8192 5808 1728 0 27 )!

uninitialize
	"Private - Un-register the system tools folder icon for the receiver to allow clean
	removal of this class from the system.
		self uninitialize
	"

	Smalltalk developmentSystem
		removeSystemFolderIconNamed: self displayString;
		unregisterTool: self! !
!USWebBrowserShell class categoriesFor: #defaultModel!public! !
!USWebBrowserShell class categoriesFor: #displayOn:!operations!public! !
!USWebBrowserShell class categoriesFor: #icon!constants!public! !
!USWebBrowserShell class categoriesFor: #initialize!initializing!must strip!private! !
!USWebBrowserShell class categoriesFor: #publishedAspects!development!public! !
!USWebBrowserShell class categoriesFor: #resource_Default_view!public!resources-views! !
!USWebBrowserShell class categoriesFor: #uninitialize!initializing!private! !

"Binary Globals"!

