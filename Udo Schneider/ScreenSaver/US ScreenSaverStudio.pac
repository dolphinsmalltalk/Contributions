| package |
package := Package name: 'US ScreenSaverStudio'.
package paxVersion: 1;
	basicComment: '$id: US ScreenSaverStudio 0.019$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.019'.

package basicScriptAt: #postuninstall put: 'Win32Constants removeKey:  ''SPI_GETSCREENSAVEACTIVE'''.
package basicScriptAt: #preinstall put: 'Win32Constants at: ''SPI_GETSCREENSAVEACTIVE'' put: 16'.

package classNames
	add: #DolphinScreensaver;
	add: #Screensaver;
	add: #ScreensaverPresenter;
	add: #ScreensaverStudioShell;
	add: #ScreensaverView;
	add: #WindowsScreensaver;
	yourself.

package methodNames
	add: #DesktopView -> #installedScreensavers;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Dolphin Harbor\DH Shell Core';
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Choice\Dolphin Choice Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Image\Dolphin Image Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\SysLink\Dolphin SysLink Control';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\..\..\Burning River\ExternalProcess\ExternalProcess';
	add: '..\..\..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	add: '..\..\..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus ImageView';
	add: 'US ScreenSaver';
	add: '..\..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Object subclass: #Screensaver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmalltalkToolShell subclass: #ScreensaverStudioShell
	instanceVariableNames: 'selectedScreensaver screensaverPresenter iconPresenter pathPresenter screensaverPreviewPresenter fileLengthTextPresenter fileLengthIconPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValuePresenter subclass: #ScreensaverPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Screensaver subclass: #DolphinScreensaver
	instanceVariableNames: 'sessionManagerClass'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Screensaver subclass: #WindowsScreensaver
	instanceVariableNames: 'name path process'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
ContainerView subclass: #ScreensaverView
	instanceVariableNames: 'screensaverPreviewParent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!DesktopView methodsFor!

installedScreensavers
	^WindowsScreensaver installedScreensavers! !
!DesktopView categoriesFor: #installedScreensavers!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

Screensaver guid: (GUID fromString: '{E695E185-7005-4FBD-B13B-BEEB9C9DC264}')!
Screensaver comment: ''!
!Screensaver categoriesForClass!Unclassified! !
!Screensaver methodsFor!

configureScreensaver
	self subclassResponsibility!

icon
^Icon fromId: 'EvaluateIt.ico'!

isDolphinScreensaver
^false!

name
self subclassResponsibility!

path
self subclassResponsibility!

printOn: target 
	super printOn: target.
	target
		nextPutAll: ' (';
		nextPutAll: self name;
		nextPutAll: ' -> ';
		nextPutAll: self path;
		nextPutAll: ')'!

showConfigurationModalTo: aView 
self subclassResponsibility!

showFullscreen
	self subclassResponsibility!

showPreviewIn: aPresenter 
	self subclassResponsibility! !
!Screensaver categoriesFor: #configureScreensaver!public! !
!Screensaver categoriesFor: #icon!public! !
!Screensaver categoriesFor: #isDolphinScreensaver!public! !
!Screensaver categoriesFor: #name!public! !
!Screensaver categoriesFor: #path!public! !
!Screensaver categoriesFor: #printOn:!public! !
!Screensaver categoriesFor: #showConfigurationModalTo:!public! !
!Screensaver categoriesFor: #showFullscreen!public! !
!Screensaver categoriesFor: #showPreviewIn:!public! !

!Screensaver class methodsFor!

basicInstalledScreensavers
	| screensavers |
	screensavers := Set new.
	self subclasses do: [:each | screensavers addAll: each installedScreensavers].
	^screensavers asArray!

installedScreensavers
	
	^(self basicInstalledScreensavers asSortedCollection: [:x :y | x name <= y name]) asArray! !
!Screensaver class categoriesFor: #basicInstalledScreensavers!private! !
!Screensaver class categoriesFor: #installedScreensavers!public! !

ScreensaverStudioShell guid: (GUID fromString: '{B53CE397-156E-4B6D-9929-CD3116C560F7}')!
ScreensaverStudioShell comment: ''!
!ScreensaverStudioShell categoriesForClass!Unclassified! !
!ScreensaverStudioShell methodsFor!

browseFullscreenClass
	selectedScreensaver value sessionManagerClass fullscreenPresenter browse!

browsePreviewClass
	selectedScreensaver value sessionManagerClass previewPresenter browse!

browseSessionManager
	selectedScreensaver value sessionManagerClass browse!

configureScreensaver
	
	[screensaverPreviewPresenter view stopScreensaver.
	self view setTimer: 1 interval: 10.
	selectedScreensaver value showConfigurationModalTo: self view.
	self view killTimer: 1.
	screensaverPreviewPresenter view startScreensaver] 
			forkAt: Processor userInterruptPriority!

createComponents
	super createComponents.
	screensaverPreviewPresenter := self add: ScreensaverPresenter new name: 'screensaver'.
	screensaverPresenter := self add: ChoicePresenter new name: 'screensavers'.
	iconPresenter := self add: ImagePresenter new name: 'icon'.
	pathPresenter := self add: TextPresenter new name: 'path'.
	fileLengthTextPresenter := self add: TextPresenter new name: 'fileLengthText'.
fileLengthIconPresenter := self add: ImagePresenter new name: 'fileLengthIcon'!

createSchematicWiring
	pathPresenter 
		when: #linkClicked:
		send: #exploreScreensaver
		to: self!

exploreScreensaver
	ShellLibrary default shellOpen: 'explorer.exe'
		parameters: '/select,' , selectedScreensaver value path!

inspectModel

	selectedScreensaver value model inspect!

model: anObject 
	super model: anObject.
	screensaverPresenter
		choices: anObject;
		value: anObject first.
	selectedScreensaver := screensaverPresenter model.
	screensaverPreviewPresenter model: selectedScreensaver.
	iconPresenter model: selectedScreensaver.
	pathPresenter model: selectedScreensaver.
	fileLengthTextPresenter model: selectedScreensaver.
fileLengthIconPresenter model: selectedScreensaver!

previewScreensaver
	| screensaver |
	screensaverPreviewPresenter view stopScreensaver.
	screensaver := selectedScreensaver value showFullscreen.
	screensaver 
		when: #viewClosed
		send: #startScreensaver
		to: screensaverPreviewPresenter view!

queryCommand: aCommandQuery 
	(#(#inspectModel #browseSessionManager #browseFullscreenClass #browsePreviewClass) 
		includes: aCommandQuery command) 
			ifTrue: 
				[aCommandQuery isEnabled: selectedScreensaver value isDolphinScreensaver.
				^true].
	^super queryCommand: aCommandQuery! !
!ScreensaverStudioShell categoriesFor: #browseFullscreenClass!actions!public! !
!ScreensaverStudioShell categoriesFor: #browsePreviewClass!actions!public! !
!ScreensaverStudioShell categoriesFor: #browseSessionManager!actions!public! !
!ScreensaverStudioShell categoriesFor: #configureScreensaver!actions!public! !
!ScreensaverStudioShell categoriesFor: #createComponents!public! !
!ScreensaverStudioShell categoriesFor: #createSchematicWiring!public! !
!ScreensaverStudioShell categoriesFor: #exploreScreensaver!actions!public! !
!ScreensaverStudioShell categoriesFor: #inspectModel!actions!public! !
!ScreensaverStudioShell categoriesFor: #model:!public! !
!ScreensaverStudioShell categoriesFor: #previewScreensaver!actions!public! !
!ScreensaverStudioShell categoriesFor: #queryCommand:!public! !

!ScreensaverStudioShell class methodsFor!

defaultModel
^Screensaver installedScreensavers!

displayOn: aStream 
	"Append, to aStream, a String whose characters are a representation of the receiver as a user
	would want to see it."

	aStream nextPutAll: 'Screensaver Studio'!

icon
^
Icon fromId: 'EvaluateIt.ico'!

initializeAfterLoad
(Smalltalk developmentSystem)
				addAdditionalToolsFolderIcon: self toolsFolderIcon;
				registerTool: self.
	super initializeAfterLoad!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26214401 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 328198 ##(Smalltalk.Point)  851 1211 551 0 0 0 416 0 234 256 98 10 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1412498947 1025 576 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 576 0 8 4294902789 459270 ##(Smalltalk.Message)  8 #name 98 0 704 401 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 514 113 665 514 361 43 576 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 56 0 0 0 76 1 0 0 236 0 0 0 97 1 0 0] 98 0 514 193 193 0 27 8 'screensavers' 410 8 ##(Smalltalk.StaticIcon)  98 17 0 416 98 2 8 1140850947 1 1168 721990 2 ##(Smalltalk.ValueHolder)  0 32 1376774 ##(Smalltalk.PluggableSearchPolicy)  850 8 #== 98 0 850 8 #hash 98 0 0 0 0 7 0 0 0 1168 0 8 4294902797 1442822 ##(Smalltalk.PluggableTypeConverter)  0 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  5 1 1424 8 'doIt' 8 'PluggableTypeConverter leftToRight: [ :each | each icon] rightToLeft: [ :each | ]' 8 #[29 32 112 226 1 106 33 111 60 106 192 105] 1424 8 #icon 8 #leftToRight:rightToLeft: 1472 1458 0 0 1504 19 257 0 9 257 0 1600 32 0 914 202 208 98 1 978 1008 98 2 514 31 653 514 65 65 1168 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 70 1 0 0 47 0 0 0 102 1 0 0] 98 0 1136 0 27 8 'icon' 410 8 ##(Smalltalk.SysLinkView)  98 16 0 416 98 2 8 1409286400 1 1792 1250 0 32 730 752 8 #equality 0 0 0 7 0 0 0 1792 0 8 4294904495 1426 0 0 1458 0 0 1490 5 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[ :each | ''<a>'' , each path , ''</a>'']' 8 #[33 105 29 226 1 178 32 178 106] 8 '<a>' 8 #path 8 #, 8 '</a>' 1952 7 257 0 0 0 914 202 208 98 2 978 1008 98 2 514 31 731 514 781 31 1792 978 8 #text: 98 1 8 '<A HREF="http://www.object-arts.com">Object Arts Home</A>' 1792 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 109 1 0 0 149 1 0 0 124 1 0 0] 98 0 1136 0 27 8 'path' 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2352 0 0 0 7 0 0 0 2352 0 8 4294902797 1426 0 0 1458 0 0 1490 8 1 1456 8 'doIt' 8 '
[:each | 
(File splitStemFrom: each path) size > 8 
	ifTrue: 
		[''The filename is longer than eight characters!! The filename (instead of the desciption) will be used:

'' 
			, (File splitStemFrom: each path)]
	ifFalse: [''The filename is within the eight character limit. The desciption will be used:

'' , each name]]' 8 #[36 105 29 226 1 178 145 214 8 129 124 32 29 226 1 178 180 106 34 226 6 180 106] 8 ##(Smalltalk.File)  2064 8 #splitStemFrom: 8 'The filename is longer than eight characters!! The filename (instead of the desciption) will be used:

' 2080 8 'The filename is within the eight character limit. The desciption will be used:

' 880 2464 7 257 0 1458 0 0 1490 10 1 8 ##(Smalltalk.NullConverter)  8 'doIt' 8 'PluggableTypeConverter leftToRight: [ :each | ((File splitStemFrom:  each path) size > 8) ifTrue: [''Yes''] ifFalse:[''No'']]  rightToLeft: [:each | '''']' 8 #[29 37 219 13 30 226 2 179 145 214 8 129 119 33 106 34 106 38 111 35 106 197 105] 1424 2544 2064 2560 8 'Yes' 8 'No' 8 '' 1584 1458 0 0 2624 11 257 0 2608 41 257 0 0 914 202 208 98 2 978 1008 98 2 514 125 1003 514 671 111 2352 978 2240 98 1 8 '1
2
3
4' 2352 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 62 0 0 0 245 1 0 0 141 1 0 0 44 2 0 0] 98 0 1136 0 27 8 'fileLengthText' 410 1184 98 17 0 416 98 2 8 1140850947 1 2992 1250 0 0 1282 850 1328 98 0 850 1376 98 0 0 0 0 7 0 0 0 2992 0 8 4294902797 1426 0 0 1458 0 0 1490 6 1 1456 8 'doIt' 8 ' [:each | 
((File splitStemFrom: each path) size > 8 ) not icon
	] ' 8 #[34 105 29 226 1 178 145 214 8 129 161 162 106] 2544 2064 2560 8 #not 1568 3184 7 257 0 1458 0 0 1490 1 83886081 1456 8 'doIt' 8 ' [ :each | nil]
' 8 #[29 105 60 106] 3280 7 257 0 32 0 914 202 208 98 1 978 1008 98 2 514 41 1001 514 65 65 2992 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 244 1 0 0 52 0 0 0 20 2 0 0] 98 0 1136 0 27 8 'fileLengthIcon' 0 0 0 0 0 1 0 0 0 0 1 0 0 914 202 208 98 3 978 1008 98 2 514 3839 21 514 851 1211 416 978 2240 98 1 8 'Screensaver Studio' 416 978 8 #updateMenuBar 704 416 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 40 9 0 0 103 2 0 0] 98 12 410 8 ##(Smalltalk.GroupBox)  98 14 0 416 98 2 8 1140850695 65 3776 0 786 8 4294967295 0 7 0 0 0 3776 0 8 4294902847 914 202 208 98 2 978 1008 98 2 514 15 13 514 811 761 3776 978 2240 98 1 8 'Screensaver' 3776 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 6 0 0 0 156 1 0 0 130 1 0 0] 98 0 1136 0 27 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 4112 0 0 0 7 0 0 0 4112 0 234 256 98 4 410 8 ##(Smalltalk.ScreensaverView)  98 16 0 4112 98 2 8 1140850688 131137 4224 0 786 8 4278190080 0 7 0 0 0 4224 788230 ##(Smalltalk.BorderLayout)  1 1 0 0 0 0 0 234 256 704 0 0 914 202 208 98 1 978 1008 98 2 514 47 33 514 641 401 4224 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 0 0 0 16 0 0 0 87 1 0 0 216 0 0 0] 98 0 1136 0 27 8 'screensaver' 410 8 ##(Smalltalk.GdiplusImageView)  98 28 0 4112 98 2 8 1140850944 1 4560 1250 0 32 1888 787206 ##(Smalltalk.GdiplusImage)  0 16 2032390 ##(Smalltalk.GdiplusImageFromFileInitializer)  0 8 'Udo Schneider\Resources\screenx2.png' 730 8 ##(Smalltalk.ImageRelativeFileLocator)  8 #current 0 0 7 0 0 0 4560 0 8 4294902797 2642 0 0 0 0 0 1 4672 8 #normal 0 8 #none 1 0 0 0 0 914 202 208 98 1 978 1008 98 2 514 1 1 514 753 593 4560 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 1 0 0 40 1 0 0] 98 0 1136 0 27 8 'monitor' 0 914 202 208 98 1 978 1008 98 2 514 33 41 514 761 601 4112 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 16 0 0 0 20 0 0 0 140 1 0 0 64 1 0 0] 98 2 4224 4560 1136 0 27 1168 576 410 8 ##(Smalltalk.PushButton)  98 17 0 416 98 2 8 1140924416 1 5184 0 0 0 7 0 0 0 5184 0 8 4294902847 1180998 4 ##(Smalltalk.CommandDescription)  8 #configureScreensaver 8 '&Configuration' 1 1 0 0 32 914 202 208 98 3 978 1008 98 2 514 481 661 514 177 51 5184 978 8 #isEnabled: 98 1 32 5184 978 2240 98 1 8 '&Configuration' 5184 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 74 1 0 0 72 1 0 0 99 1 0 0] 98 0 1136 0 27 410 5200 98 17 0 416 98 2 8 1140924416 1 5600 0 0 0 7 0 0 0 5600 0 8 4294902847 5282 8 #previewScreensaver 8 '&Preview' 1 1 0 0 32 914 202 208 98 3 978 1008 98 2 514 657 661 514 141 51 5600 978 5472 98 1 32 5600 978 2240 98 1 8 '&Preview' 5600 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 72 1 0 0 74 1 0 0 142 1 0 0 99 1 0 0] 98 0 1136 0 27 410 3792 98 14 0 416 98 2 8 1140850695 65 5968 0 786 8 4294967295 0 7 0 0 0 5968 0 8 4294902847 914 202 208 98 2 978 1008 98 2 514 11 801 514 811 121 5968 978 2240 98 1 8 'Browse/Inspect' 5968 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 144 1 0 0 154 1 0 0 204 1 0 0] 98 0 1136 0 27 1792 410 4128 98 15 0 416 98 2 8 1140850688 131073 6288 0 0 0 7 0 0 0 6288 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 8 410 5200 98 17 0 6288 98 2 8 1140924416 1 6416 0 0 0 7 0 0 0 6416 0 8 4294902847 5282 8 #browseSessionManager 8 'Session&Manager' 1 1 0 0 32 914 202 208 98 3 978 1008 98 2 514 191 1 514 187 61 6416 978 5472 98 1 32 6416 978 2240 98 1 8 'Session&Manager' 6416 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 0 0 0 0 188 0 0 0 30 0 0 0] 98 0 1136 0 27 524806 ##(Smalltalk.Fraction)  377 387 410 5200 98 17 0 6288 98 2 8 1140924416 1 6816 0 0 0 7 0 0 0 6816 0 8 4294902847 5282 8 #browsePreviewClass 8 'Preview &Class' 1 1 0 0 32 914 202 208 98 3 978 1008 98 2 514 571 1 514 201 61 6816 978 5472 98 1 32 6816 978 2240 98 1 8 'Preview &Class' 6816 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 29 1 0 0 0 0 0 0 129 1 0 0 30 0 0 0] 98 0 1136 0 27 6786 397 387 410 5200 98 17 0 6288 98 2 8 1140924416 1 7200 0 0 0 7 0 0 0 7200 0 8 4294902847 5282 8 #browseFullscreenClass 8 '&Fullscreen Class' 1 1 0 0 32 914 202 208 98 3 978 1008 98 2 514 377 1 514 195 61 7200 978 5472 98 1 32 7200 978 2240 98 1 8 '&Fullscreen Class' 7200 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 188 0 0 0 0 0 0 0 29 1 0 0 30 0 0 0] 98 0 1136 0 27 6786 195 193 410 5200 98 17 0 6288 98 2 8 1140924416 1 7584 0 0 0 7 0 0 0 7584 0 8 4294902847 5282 8 #inspectModel 8 '&Model' 1 1 0 0 32 914 202 208 98 3 978 1008 98 2 514 1 1 514 191 61 7584 978 5472 98 1 32 7584 978 2240 98 1 8 '&Model' 7584 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 95 0 0 0 30 0 0 0] 98 0 1136 0 27 6786 191 193 32 234 256 704 0 914 202 208 98 1 978 1008 98 2 514 31 841 514 771 61 6288 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 164 1 0 0 144 1 0 0 194 1 0 0] 98 4 7584 6416 7200 6816 1136 0 27 410 3792 98 14 0 416 98 2 8 1140850695 65 8144 0 786 6048 0 7 0 0 0 8144 0 8 4294902847 914 202 208 98 2 978 1008 98 2 514 25 963 514 791 161 8144 978 2240 98 1 8 'Name in Dialog' 8144 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 12 0 0 0 225 1 0 0 151 1 0 0 49 2 0 0] 98 0 1136 0 27 2992 2352 1136 0 27 )!

uninitializeBeforeRemove
	(Smalltalk developmentSystem)
				removeSystemFolderIcon: self toolsFolderIcon;
				unregisterTool: self.
	super uninitializeBeforeRemove! !
!ScreensaverStudioShell class categoriesFor: #defaultModel!public! !
!ScreensaverStudioShell class categoriesFor: #displayOn:!public! !
!ScreensaverStudioShell class categoriesFor: #icon!public! !
!ScreensaverStudioShell class categoriesFor: #initializeAfterLoad!initializing!public! !
!ScreensaverStudioShell class categoriesFor: #resource_Default_view!*-in class package!public!resources-views! !
!ScreensaverStudioShell class categoriesFor: #uninitializeBeforeRemove!initializing!public! !

ScreensaverPresenter guid: (GUID fromString: '{945AD615-5DF8-4337-9E38-D56B45FE6383}')!
ScreensaverPresenter comment: ''!
!ScreensaverPresenter categoriesForClass!Unclassified! !
!ScreensaverPresenter class methodsFor!

defaultModel
	^nil!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ScreensaverView)  98 16 0 0 98 2 8 1140850688 131137 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 0 0 0 0 234 256 98 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 738 701 501 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 250 0 0 0] 98 0 738 193 193 0 27 )! !
!ScreensaverPresenter class categoriesFor: #defaultModel!public! !
!ScreensaverPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

DolphinScreensaver guid: (GUID fromString: '{89F88EB1-00E1-457A-BB63-A3208D33E368}')!
DolphinScreensaver comment: ''!
!DolphinScreensaver categoriesForClass!Unclassified! !
!DolphinScreensaver methodsFor!

configureScreensaver
	sessionManagerClass  showConfigurationDialog!

icon
^
(File exists: self path) ifTrue: [SHFILEINFO openIcon: self path] ifFalse: [super icon]!

isDolphinScreensaver
	^true!

model
	^sessionManagerClass  model!

name
	^sessionManagerClass screensaverName!

path
	^(Package manager packageOfClass: sessionManagerClass) imageStripper executablePath!

sessionManagerClass
	^sessionManagerClass!

setSessionManagerClass: aSessionManager 
	sessionManagerClass := aSessionManager!

showConfigurationModalTo: aView 
	sessionManagerClass  showConfigurationDialogModalTo: aView!

showFullscreen
	^sessionManagerClass  showFullscreen!

showPreviewIn: aPresenter 
	sessionManagerClass  showPreviewIn: aPresenter! !
!DolphinScreensaver categoriesFor: #configureScreensaver!public! !
!DolphinScreensaver categoriesFor: #icon!public! !
!DolphinScreensaver categoriesFor: #isDolphinScreensaver!public! !
!DolphinScreensaver categoriesFor: #model!public! !
!DolphinScreensaver categoriesFor: #name!public! !
!DolphinScreensaver categoriesFor: #path!public! !
!DolphinScreensaver categoriesFor: #sessionManagerClass!public! !
!DolphinScreensaver categoriesFor: #setSessionManagerClass:!private! !
!DolphinScreensaver categoriesFor: #showConfigurationModalTo:!public! !
!DolphinScreensaver categoriesFor: #showFullscreen!public! !
!DolphinScreensaver categoriesFor: #showPreviewIn:!public! !

!DolphinScreensaver class methodsFor!

basicInstalledScreensavers
	^ScreensaverSessionManager allSubclasses collect: [:each | self sessionManager: each]!

sessionManager: aSessionManager 
	^(super new)
		setSessionManagerClass: aSessionManager;
		yourself! !
!DolphinScreensaver class categoriesFor: #basicInstalledScreensavers!private! !
!DolphinScreensaver class categoriesFor: #sessionManager:!public! !

WindowsScreensaver guid: (GUID fromString: '{519A4009-525E-4D7D-B8A7-2B4E2977DB3D}')!
WindowsScreensaver comment: ''!
!WindowsScreensaver categoriesForClass!Unclassified! !
!WindowsScreensaver methodsFor!

configureScreensaver
	self startWithParametersSync: '/c'!

icon
	^SHFILEINFO openIcon: self path!

name
	^name!

path
	^path!

setName: aString path: aPath

name := aString. path := aPath!

showConfigurationModalTo: aView 
	self startWithParametersSync: '/c:' , aView handle value displayString.
	!

showFullscreen
	self startWithParametersSync: '/s'!

showPreviewIn: aPresenter 
	self startWithParametersAsync: '/p ' , aPresenter asParameter value displayString!

startWithParametersAsync: parameters 
	(process := ExternalProcess new)
		commandLine: path , ' ' , parameters;
		executeAsync!

startWithParametersSync: parameters 
	(process := ExternalProcess new)
		commandLine: path , ' ' , parameters;
		executeSync! !
!WindowsScreensaver categoriesFor: #configureScreensaver!public! !
!WindowsScreensaver categoriesFor: #icon!public! !
!WindowsScreensaver categoriesFor: #name!public! !
!WindowsScreensaver categoriesFor: #path!public! !
!WindowsScreensaver categoriesFor: #setName:path:!private! !
!WindowsScreensaver categoriesFor: #showConfigurationModalTo:!public! !
!WindowsScreensaver categoriesFor: #showFullscreen!public! !
!WindowsScreensaver categoriesFor: #showPreviewIn:!public! !
!WindowsScreensaver categoriesFor: #startWithParametersAsync:!private! !
!WindowsScreensaver categoriesFor: #startWithParametersSync:!private! !

!WindowsScreensaver class methodsFor!

basicInstalledScreensavers
	| screensavers |
	screensavers := OrderedCollection new.
	File 
		for: '*.scr'
		in: SessionManager current systemDirectory
		do: 
			[:each | 
			| path |
			path := each path.
			screensavers add: (WindowsScreensaver path: path)].
	^screensavers asArray!

isActive
	^SystemMetrics current getSysParamBool: SPI_GETSCREENSAVEACTIVE!

isEnabled
	"// In some Windows versions, or due to some faulty program, the ScreenSaverIsEnabledFlag()
        // may be set even when there is no selected screensaver. "

	#USToDo.
	^self isActive and: [self isSelected]!

isSelected
	self selectedScreenSaverPath size > 0!

new
	^self shouldNotImplement!

path: aPath 
	| lib screensaverName |
	lib := ExternalResourceLibrary open: aPath.
	screensaverName := [String fromId: 1 in: lib] on: Win32Error do: [:ex | File splitStemFrom: aPath].
	lib close.
	^super new setName: screensaverName path: aPath!

selectedScreensaver
^self path: self selectedScreenSaverPath!

selectedScreenSaverPath
	(RegKey userRoot at: 'Control Panel\\Desktop') subValues at: 'SCRNSAVE.EXE'! !
!WindowsScreensaver class categoriesFor: #basicInstalledScreensavers!private! !
!WindowsScreensaver class categoriesFor: #isActive!private! !
!WindowsScreensaver class categoriesFor: #isEnabled!*-in class package!public! !
!WindowsScreensaver class categoriesFor: #isSelected!*-in class package!private! !
!WindowsScreensaver class categoriesFor: #new!private! !
!WindowsScreensaver class categoriesFor: #path:!*-in class package!public! !
!WindowsScreensaver class categoriesFor: #selectedScreensaver!public! !
!WindowsScreensaver class categoriesFor: #selectedScreenSaverPath!public! !

ScreensaverView guid: (GUID fromString: '{AAEA4020-4FF0-4904-A94E-92FE63DA60A4}')!
ScreensaverView comment: ''!
!ScreensaverView categoriesForClass!Unclassified! !
!ScreensaverView methodsFor!

connectModel
	"Connect the receiver to its model, wiring events, etc.
	ValueConvertingControlViews expect to be connected to a model that obeys the
	ValueModel protocol; i.e. it must generate a #value event when the value it is wrapping 
	is replaced."

	self model 
		ifNotNil: 
			[:m | 
			m 
				when: #valueChanged
				send: #refreshContents
				to: self].
^super connectModel!

defaultBackcolor
^Color black!

defaultLayoutManager
	^BorderLayout new!

defaultWindowExStyle
	"Private - Answer the default extended window creation style.
	Implementation Note: We specify WS_EX_TRANSPARENT because
	we turn on WS_CLIPSIBLINGS by default, and in any case, this
	is more correct, so there!! However, this does upset the
	hit-testing."

	^super defaultWindowExStyle bitOr: WS_EX_TRANSPARENT!

onEraseRequired: aColorEvent
^true!

refreshContents
	self stopScreensaver.
	self startScreensaver!

startScreensaver
	self model notNil 
		ifTrue: 
			[screensaverPreviewParent := Presenter create: 'Container view' in: self presenter.
			(screensaverPreviewParent view)
				coverParent.
			self value showPreviewIn: screensaverPreviewParent]!

stopScreensaver
	screensaverPreviewParent notNil 
		ifTrue: 
			[screensaverPreviewParent destroy.
			self invalidate.
			screensaverPreviewParent := nil]!

value
	"Answer the receiver's model value"

	^self model value!

value: anObject 
	"Set the receiver's model value to anObject"

	self model value: anObject! !
!ScreensaverView categoriesFor: #connectModel!public! !
!ScreensaverView categoriesFor: #defaultBackcolor!public! !
!ScreensaverView categoriesFor: #defaultLayoutManager!private! !
!ScreensaverView categoriesFor: #defaultWindowExStyle!constants!private! !
!ScreensaverView categoriesFor: #onEraseRequired:!public! !
!ScreensaverView categoriesFor: #refreshContents!public! !
!ScreensaverView categoriesFor: #startScreensaver!private! !
!ScreensaverView categoriesFor: #stopScreensaver!private! !
!ScreensaverView categoriesFor: #value!public! !
!ScreensaverView categoriesFor: #value:!public! !

!ScreensaverView class methodsFor!

defaultModel
	^nil! !
!ScreensaverView class categoriesFor: #defaultModel!public! !

"Binary Globals"!

