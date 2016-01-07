| package |
package := Package name: 'US External Tool Shell'.
package paxVersion: 1;
	basicComment: '$id: US External Tool Shell 0.013$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.013'.


package classNames
	add: #ExternalTool;
	add: #ExternalToolEditor;
	add: #ExternalToolPresenter;
	add: #ExternalToolShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Collection\Dolphin Collection Presenters';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Sliding Tray\Dolphin Slidey-Inney-Outey Thing';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Burning River\ExternalProcess\ExternalProcess';
	add: 'US ExternalProcess View';
	add: '..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Object subclass: #ExternalTool
	instanceVariableNames: 'path parameters description icon'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValueDialog subclass: #ExternalToolEditor
	instanceVariableNames: 'detailsPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmalltalkToolShell subclass: #ExternalToolShell
	instanceVariableNames: 'externalProcessPresenter knownToolsPresenter'
	classVariableNames: 'ExternalTools'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValuePresenter subclass: #ExternalToolPresenter
	instanceVariableNames: 'pathPresenter parametersPresenter descriptionPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ExternalTool guid: (GUID fromString: '{2C09CCB3-00AC-40D8-87D0-87F934E476B8}')!
ExternalTool comment: ''!
!ExternalTool categoriesForClass!Unclassified! !
!ExternalTool methodsFor!

asExternalProcess
	^(ExternalProcess new)
		commandLine: '"' , self path , '" "', self parameters , '"';
		yourself!

description
	description 
		ifNil: 
			[description := File splitFilenameFrom: self path.
			[description  := description , (' (' , (VersionInfo forPath: self path) fileDescription , ')')] on: Error do: [:ex | ].
			].
	^description!

description: aString 
	^description := aString!

icon
	^(Smalltalk at: #SHFILEINFO ifAbsent: [nil]) 
		ifNotNil: [:shfileinfo | shfileinfo smallIcon: path]
		ifNil: [self class icon]!

parameters
^parameters!

parameters: anObject
	parameters := anObject!

path
^path!

path: anObject 
	path := anObject.
	icon := nil.
	description := nil!

printOn: target
target nextPutAll:  self description!

setCommand: aPath parameters: aString
path := aPath.
parameters := aString 
	! !
!ExternalTool categoriesFor: #asExternalProcess!public! !
!ExternalTool categoriesFor: #description!accessing!public! !
!ExternalTool categoriesFor: #description:!accessing!private! !
!ExternalTool categoriesFor: #icon!public! !
!ExternalTool categoriesFor: #parameters!accessing!public! !
!ExternalTool categoriesFor: #parameters:!accessing!public! !
!ExternalTool categoriesFor: #path!accessing!public! !
!ExternalTool categoriesFor: #path:!accessing!public! !
!ExternalTool categoriesFor: #printOn:!public! !
!ExternalTool categoriesFor: #setCommand:parameters:!private! !

!ExternalTool class methodsFor!

command: aPath parameters: aString 
	^(self basicNew)
		initialize;
		setCommand: aPath parameters: aString;
		yourself!

icon

^Icon defaultApplication!

new
	^self command: ('c:\WINDOWS\system32\sol.exe')
		parameters: ''! !
!ExternalTool class categoriesFor: #command:parameters:!instance creation!public! !
!ExternalTool class categoriesFor: #icon!public! !
!ExternalTool class categoriesFor: #new!instance creation!public! !

ExternalToolEditor guid: (GUID fromString: '{AF8160A6-4D9F-4B09-BB5F-335C3A570098}')!
ExternalToolEditor comment: ''!
!ExternalToolEditor categoriesForClass!Unclassified! !
!ExternalToolEditor methodsFor!

createComponents
	"Create the presenters contained by the receiver"

	super createComponents.
	detailsPresenter := self add: ExternalToolPresenter new name: 'details'!

model: anExternalTool 
	"Set the model associated with the receiver."

	super model: anExternalTool.
	detailsPresenter model: self model! !
!ExternalToolEditor categoriesFor: #createComponents!initializing!public! !
!ExternalToolEditor categoriesFor: #model:!accessing!public! !

!ExternalToolEditor class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1537 1025 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 592 0 0 0 7 0 0 0 592 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 21 223 530 915 71 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 111 0 0 0 211 1 0 0 146 0 0 0] 98 0 530 193 193 0 27 0 0 410 608 98 14 0 416 98 2 8 1140850688 131073 976 0 482 8 4278190080 0 7 0 0 0 976 674 8 ##(Smalltalk.ExternalToolPresenter)  8 #resource_Default_view 0 738 202 208 98 1 802 832 98 2 530 21 21 530 915 203 976 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 211 1 0 0 111 0 0 0] 944 960 0 27 234 256 98 2 976 8 'details' 590342 ##(Smalltalk.Rectangle)  530 21 21 530 21 21 0 0 0 0 60305 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2117657919 738 202 208 98 3 802 832 98 2 530 3839 21 530 971 381 416 802 8 #text: 98 1 8 'Edit External Tool' 416 802 8 #updateMenuBar 944 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 100 9 0 0 200 0 0 0] 98 2 976 592 960 0 27 )! !
!ExternalToolEditor class categoriesFor: #resource_Default_view!public!resources-views! !

ExternalToolShell guid: (GUID fromString: '{8FBF71A4-7A4D-47C9-804A-EC552C91F983}')!
ExternalToolShell comment: ''!
!ExternalToolShell categoriesForClass!Unclassified! !
!ExternalToolShell methodsFor!

createComponents
	externalProcessPresenter := self add: ExternalProcessPresenter new name: 'externalProcess'.
	knownToolsPresenter := self add: CollectionPresenter new name: 'knownTools'.
	knownToolsPresenter setAddItemBlock: [ExternalToolEditor showModalOn: ExternalTool new].
	knownToolsPresenter model: self class externalTools .super createComponents!

createSchematicWiring
	self 
		when: #timerTick:
		send: #onTimerTick:
		to: self.
	externalProcessPresenter 
		when: #applicationKilled
		send: #onApplicationKilled
		to: self.
	knownToolsPresenter listPresenter 
		when: #actionPerformed
		send: #startTool
		to: self.
	super createSchematicWiring!

hideSlideys
	self hideSlideyNamed: 'toolsSlidey'.!

icon
	^self processLargeIcon!

killUpdateTimer
	self view killTimer: 1!

longCaption
	^externalProcessPresenter view processCaptionOrNil 
		ifNotNil: [:value | self class displayString , ': ' , value]
		ifNil: [self class displayString ]!

model: aProcess 
	super model: aProcess.
	externalProcessPresenter model: aProcess.
	!

onApplicationKilled
	self model: nil.
	externalProcessPresenter view invalidate!

onTimerTick: anInteger 
	self updateCaption; updateIcon!

onViewClosed
	self killUpdateTimer.
	^super onViewClosed!

onViewOpened
	self startUpdateTimer.
	self updateCaption; updateIcon.
	^super onViewOpened!

processLargeIcon
	^externalProcessPresenter view processLargeIconOrNil ifNil: [Icon defaultApplication]!

processSmallIcon
	^externalProcessPresenter view processSmallIconOrNil ifNil: [Icon defaultApplication]!

shortCaption
	^externalProcessPresenter view processCaptionOrNil ifNil: [self class displayString ]!

startTool
	| selectedTool |
	selectedTool := knownToolsPresenter listPresenter selectionOrNil.
	selectedTool ifNotNil: [:value | self model: value asExternalProcess]!

startUpdateTimer
	self view setTimer: 1 interval: 1000!

updateCaption
	self caption: self longCaption!

updateIcon
	self view largeIcon: self processLargeIcon;
		smallIcon: self processSmallIcon! !
!ExternalToolShell categoriesFor: #createComponents!public! !
!ExternalToolShell categoriesFor: #createSchematicWiring!public! !
!ExternalToolShell categoriesFor: #hideSlideys!public! !
!ExternalToolShell categoriesFor: #icon!public! !
!ExternalToolShell categoriesFor: #killUpdateTimer!helpers!private! !
!ExternalToolShell categoriesFor: #longCaption!public! !
!ExternalToolShell categoriesFor: #model:!public! !
!ExternalToolShell categoriesFor: #onApplicationKilled!public! !
!ExternalToolShell categoriesFor: #onTimerTick:!event handling!private! !
!ExternalToolShell categoriesFor: #onViewClosed!public! !
!ExternalToolShell categoriesFor: #onViewOpened!public! !
!ExternalToolShell categoriesFor: #processLargeIcon!helpers!private! !
!ExternalToolShell categoriesFor: #processSmallIcon!helpers!private! !
!ExternalToolShell categoriesFor: #shortCaption!accessing!public! !
!ExternalToolShell categoriesFor: #startTool!operations!public! !
!ExternalToolShell categoriesFor: #startUpdateTimer!helpers!private! !
!ExternalToolShell categoriesFor: #updateCaption!public! !
!ExternalToolShell categoriesFor: #updateIcon!public! !

!ExternalToolShell class methodsFor!

defaultModel
^nil!

displayOn: aStream 
	aStream nextPutAll: 'External Tool'!

externalTools
	ExternalTools ifNil: [ExternalTools := OrderedCollection  new].
	^ExternalTools!

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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1601 1201 551 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140851532 131073 592 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 592 482 688 8 4294902167 234 256 98 8 410 8 ##(Smalltalk.ReferenceView)  98 14 0 592 98 2 8 1140850688 131073 864 0 0 0 7 0 0 0 864 1180166 ##(Smalltalk.ResourceIdentifier)  608 8 #resource_Workspace_tools 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1299 1 530 155 51 864 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 137 2 0 0 0 0 0 0 214 2 0 0 25 0 0 0] 98 0 530 193 193 0 27 8 'workspaceTools' 410 880 98 14 0 592 98 2 8 1140850688 131073 1248 0 0 0 7 0 0 0 1248 946 608 8 #resource_Image_tools 0 994 202 208 98 1 1058 1088 98 2 530 1 1 530 63 51 1248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 31 0 0 0 25 0 0 0] 1200 1216 0 27 8 'imageTools' 410 880 98 14 0 592 98 2 8 1140850688 131073 1504 0 0 0 7 0 0 0 1504 946 608 8 #resource_Edit_tools 0 994 202 208 98 1 1058 1088 98 2 530 1053 1 530 247 51 1504 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 2 0 0 0 0 0 0 137 2 0 0 25 0 0 0] 1200 1216 0 27 8 'editTools' 410 880 98 14 0 592 98 2 8 1140850688 131073 1760 0 0 0 7 0 0 0 1760 946 608 8 #resource_Smalltalk_tools 0 994 202 208 98 1 1058 1088 98 2 530 63 1 530 991 51 1760 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 31 0 0 0 0 0 0 0 14 2 0 0 25 0 0 0] 1200 1216 0 27 8 'smalltalkTools' 234 256 1200 202 208 1200 234 240 1200 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 994 202 208 98 2 1058 1088 98 2 530 1 1 530 1585 51 592 1058 8 #updateSize 1200 592 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 24 3 0 0 25 0 0 0] 98 4 1248 1760 1504 864 1216 0 27 0 0 0 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 2320 0 0 0 7 0 0 0 2320 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 2 410 880 98 14 0 2320 98 2 8 1140850688 131073 2464 0 482 8 4278190080 0 7 0 0 0 2464 946 8 ##(Smalltalk.ExternalProcessPresenter)  8 #resource_Default_view 0 994 202 208 98 1 1058 1088 98 2 530 403 1 530 1183 1043 2464 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 0 0 0 0 24 3 0 0 9 2 0 0] 1200 1216 0 27 7 32 234 256 98 4 2464 8 'externalProcess' 410 8 ##(Smalltalk.SlideyInneyOuteyThing)  98 23 0 2320 98 2 8 1409286144 131073 2800 0 482 8 4278190080 0 519 0 0 0 2800 655878 ##(Smalltalk.CardLayout)  202 208 98 1 721414 ##(Smalltalk.Association)  8 'Tools' 410 880 98 14 0 410 8 ##(Smalltalk.SlidingCardTray)  98 22 0 2800 98 2 8 1140850688 131073 3056 0 482 2896 0 7 0 0 0 3056 2928 234 256 98 2 3024 8 'knownTools' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 2800 98 2 8 1140916864 1 3200 590662 2 ##(Smalltalk.ListModel)  202 208 98 1 3008 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 2896 0 1 0 0 0 3200 0 8 4294902779 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  3354 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 8 #noIcons 0 0 0 0 0 994 202 208 98 3 1058 1088 98 2 530 1 1 530 393 1043 3200 1058 8 #selectionByIndex:ifAbsent: 98 2 3 1058 8 #yourself 1200 0 3200 1058 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 3200 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 196 0 0 0 9 2 0 0] 98 0 1216 0 27 0 2800 530 33 33 1049862 ##(Smalltalk.ButtonInteractor)  3056 0 590342 ##(Smalltalk.Rectangle)  530 301 3 530 333 35 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #togglePin 8 'Pin or Unpin the tray' 1 1 0 0 0 994 202 208 98 1 1058 1088 98 2 530 49 9 530 337 1027 3056 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 0 0 0 4 0 0 0 192 0 0 0 5 2 0 0] 98 1 3024 1216 0 27 98 2 8 1140850688 131073 3024 0 482 2544 0 7 0 0 0 3024 946 8 ##(Smalltalk.CollectionPresenter)  2592 0 994 202 208 98 1 1058 1088 98 2 530 1 37 530 337 991 3024 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 168 0 0 0 1 2 0 0] 1200 1216 0 27 3024 234 256 1200 0 3200 3056 530 201 201 401 1 31 0 0 994 202 208 98 1 1058 1088 98 2 530 1 1 530 393 1043 2800 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 196 0 0 0 9 2 0 0] 98 2 3056 3200 1216 0 27 8 'toolsSlidey' 0 994 202 208 98 1 1058 1088 98 2 530 1 51 530 1585 1043 2320 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 24 3 0 0 34 2 0 0] 98 3 2800 410 8 ##(Smalltalk.Splitter)  98 12 0 2320 98 2 8 1140850688 1 4752 0 482 8 4278190080 0 519 0 0 0 4752 994 202 208 98 1 1058 1088 98 2 530 393 1 530 11 1043 4752 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 196 0 0 0 0 0 0 0 201 0 0 0 9 2 0 0] 98 0 1216 0 27 2464 1216 0 27 234 256 98 2 592 8 'toolbar' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 5 265030 4 ##(Smalltalk.Menu)  0 16 98 9 984134 2 ##(Smalltalk.CommandMenuItem)  1 3938 8 #fileNew 8 '&New' 8349 1 0 0 0 5170 1 3938 8 #fileOpen 8 '&Open...' 8351 1 0 0 0 5170 1 3938 8 #fileFileIn 8 '&File In...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 5170 1 3938 8 #saveImage 8 'Sa&ve Image' 1 1 0 0 0 5170 1 3938 8 #saveImageAs 8 'Save Image As...' 1 1 0 0 0 5170 1 3938 8 #compressChanges 8 '&Compress Changes' 1 1 0 0 0 5378 4097 5170 1 3938 8 #smalltalkExit 8 'E&xit Dolphin' 1 1 0 0 0 8 '&File' 0 1 0 0 37879 0 0 5122 0 16 98 11 5170 1 3938 8 #undo 8 '&Undo' 8373 1 0 0 0 5378 4097 5170 1 3938 8 #cutSelection 8 'Cu&t' 8369 1 0 0 0 5170 1 3938 8 #copySelection 8 '&Copy' 8327 1 0 0 0 5170 1 3938 8 #pasteClipboard 8 '&Paste' 8365 1 0 0 0 5170 1 3938 8 #clearSelection 8 '&Delete' 1 1 0 0 0 5170 1 3938 8 #selectAll 8 'Select &All' 1 1 0 0 0 5378 4097 5170 1 3938 8 #find 8 '&Find...' 8333 1 0 0 0 5170 1 3938 8 #findNext 8 'Find &Next' 229 1 0 0 0 5170 1 3938 8 #findReplace 8 '&Replace...' 8337 1 0 0 0 8 '&Edit' 0 1 0 0 37899 0 0 5122 0 16 98 7 5170 1 3938 8 #browseIt 8 '&Browse It' 8325 1 0 0 0 5170 1 3938 8 #displayIt 8 '&Display It' 8329 1 0 0 0 5170 1 3938 8 #evaluateIt 8 '&Evaluate It' 8331 1 0 0 0 5170 1 3938 8 #inspectIt 8 '&Inspect It' 8339 1 0 0 0 5170 1 3938 8 #fileItIn 8 '&File It In' 1 1 0 0 0 5378 4097 5170 1 3938 8 #accept 8 '&Accept' 8359 1 0 0 0 8 '&Workspace' 0 1 0 0 37913 0 0 5122 0 16 98 0 8 '&Tools' 8 #toolsMenu 1 0 0 37915 0 0 5122 0 16 98 19 5170 1 3938 8 #helpContents 8 '&Contents' 1025 1 263494 3 ##(Smalltalk.Icon)  0 16 3354 8 ##(Smalltalk.ImageRelativeFileLocator)  3504 49 3354 8 ##(Smalltalk.ShellLibrary)  8 #default 0 0 5170 1 3938 8 #help 8 'On this &Tool' 1249 1 0 0 0 5170 1 3938 8 #helpWhatsThis 8 'What''s This?' 5345 1 0 0 0 5378 4097 5170 1 3938 8 #helpFirstSplash 8 'First Splash!!' 1 1 0 0 0 5378 4097 5170 1 3938 8 #helpWhatsNew 8 'What''s &New' 1 1 0 0 0 5170 1 3938 8 #helpGuidedTour 8 '&Guided Tour' 1 1 0 0 0 5170 1 3938 8 #helpTutorials 8 'Tutorials' 1 1 0 0 0 5122 0 16 98 4 5170 2097153 3938 8 #tipOfTheDay 8 '&Next Tip of the Day' 9441 1 6962 0 16 6992 8 'TipOfTheDay.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 0 5170 1 3938 8 #previousTipOfTheDay 8 '&Previous Tip of the Day' 13537 1 6962 0 16 6992 8 'TipOfTheDay.ico' 7632 0 0 5378 4097 5170 1 3938 8 #toggleShowTipsAtStartup 8 '&Show Tips at Startup' 1 1 0 0 0 8 'Tip of the &Day' 0 134217729 0 0 37937 0 0 5378 4097 5170 1 3938 8 #objectArtsHomePage 8 'Object Arts Homepage' 1 1 0 0 0 5170 1 3938 8 #dolphinNewsgroup 8 'Dolphin Newsgroup/Forum' 1 1 0 0 0 5170 1 3938 8 #dolphinWikiWeb 8 'Dolphin WikiWeb' 1 1 0 0 0 5170 1 3938 8 #myDolphinAccount 8 'My Dolphin Account' 1 1 0 0 0 5378 4097 5170 1 3938 8 #dolphinLiveUpdate 8 'Check for Live &Updates...' 1 1 6962 0 16 6992 8 'LiveUpdate.ico' 7632 0 0 5378 4097 5170 1 3938 8 #aboutDolphin 8 '&About Dolphin Smalltalk' 1 1 6962 0 16 6992 8 '!!APPLICATION' 7632 0 0 8 '&Help' 0 134217729 0 0 37951 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 994 202 208 98 3 1058 1088 98 2 530 3839 21 530 1601 1201 416 1058 8 #text: 98 1 8 'External Tool' 416 1058 8 #updateMenuBar 1200 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 159 10 0 0 98 2 0 0] 98 2 592 2320 1216 0 27 )!

uninitialize
	"Private - Un-register the system tools folder icon for the receiver to allow clean
	removal of this class from the system.
		self uninitialize
	"

	Smalltalk developmentSystem
		removeSystemFolderIconNamed: self displayString;
		unregisterTool: self! !
!ExternalToolShell class categoriesFor: #defaultModel!public! !
!ExternalToolShell class categoriesFor: #displayOn:!operations!public! !
!ExternalToolShell class categoriesFor: #externalTools!public! !
!ExternalToolShell class categoriesFor: #initialize!initializing!must strip!private! !
!ExternalToolShell class categoriesFor: #publishedAspects!development!public! !
!ExternalToolShell class categoriesFor: #resource_Default_view!public!resources-views! !
!ExternalToolShell class categoriesFor: #uninitialize!initializing!private! !

ExternalToolPresenter guid: (GUID fromString: '{54C79EE1-49DF-4681-8C5F-AD0517C4DA2E}')!
ExternalToolPresenter comment: ''!
!ExternalToolPresenter categoriesForClass!Unclassified! !
!ExternalToolPresenter methodsFor!

browsePath
	| newFilename |
	newFilename := (FileOpenDialog new)
				fileTypes: (Array with: #('Executables (*.exe)' '*.exe') with: FileDialog allFilesType);
				value: pathPresenter value;
				caption: 'Choose External Tool...' ;showModal.
	newFilename notNil 
		ifTrue: 
			[pathPresenter value: newFilename.
			self onValueChanged]!

createComponents
	super createComponents.
	pathPresenter := self add: TextPresenter new name: 'path'.
	parametersPresenter := self add: TextPresenter new name: 'parameters'.
	descriptionPresenter := self add: TextPresenter new name: 'description'.!

model: aValueModel 
	"Set the model of the receiver to be aValueModel. We intercept a change
	notification so that the list selection can track this value."

	super model: aValueModel.
	
	self onValueChanged!

onValueChanged
	self value 
		ifNotNil: 
			[:newValue | 
			pathPresenter model: (newValue aspectValue: #path).
			parametersPresenter model: (newValue aspectValue: #parameters).
			descriptionPresenter model: (newValue aspectValue: #description ).]
		ifNil: 
			[pathPresenter model: nil.
			parametersPresenter model: nil.
			descriptionPresenter model: nil].
	super onValueChanged! !
!ExternalToolPresenter categoriesFor: #browsePath!public! !
!ExternalToolPresenter categoriesFor: #createComponents!public! !
!ExternalToolPresenter categoriesFor: #model:!accessing!public! !
!ExternalToolPresenter categoriesFor: #onValueChanged!event handling!public! !

!ExternalToolPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 14 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 592 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 592 0 8 4294902559 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  171 141 898 511 39 592 834 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 592 834 8 #isTextModified: 98 1 32 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 0 0 0 70 0 0 0 84 1 0 0 89 0 0 0] 98 0 898 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedPreviousRight 21 1194 1216 8 #fixedParentRight -19 1194 1216 8 #fixedPreviousTop 1 1194 1216 8 #fixedViewTop 39 410 8 ##(Smalltalk.PushButton)  98 17 0 416 98 2 8 1140924416 1 1344 0 0 0 5 0 0 0 1344 0 8 4294902543 1180998 4 ##(Smalltalk.CommandDescription)  8 #browsePath 8 'Browse...' 1 1 0 0 32 770 202 208 98 3 834 864 98 2 898 561 21 898 121 39 1344 834 8 #isEnabled: 98 1 32 1344 834 8 #text: 98 1 8 'Browse...' 1344 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 10 0 0 0 84 1 0 0 29 0 0 0] 98 0 1136 0 27 1154 1200 21 1248 -19 1280 1 1312 39 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 1792 0 0 0 5 0 0 0 1792 0 8 4294902553 738 0 0 0 770 202 208 98 2 834 864 98 2 898 21 141 898 131 41 1792 834 1680 98 1 8 'Description:' 1792 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 70 0 0 0 75 0 0 0 90 0 0 0] 98 0 1136 0 27 1154 1194 1216 8 #fixedParentLeft 21 1194 1216 8 #fixedViewLeft 131 1194 1216 8 #fixedPreviousBottom 23 1312 41 410 608 98 16 0 416 98 2 8 1140916352 1025 2224 0 674 704 0 5 0 0 0 2224 0 8 4294902559 738 0 0 1 770 202 208 98 3 834 864 98 2 898 111 21 898 431 39 2224 834 960 98 1 994 3 1 3 2224 834 1040 98 1 32 2224 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 55 0 0 0 10 0 0 0 14 1 0 0 29 0 0 0] 98 0 1136 0 27 1154 1200 21 1248 -159 1280 1 1312 39 410 608 98 16 0 416 98 2 8 1140916352 1025 2592 0 674 704 0 5 0 0 0 2592 0 8 4294902559 738 0 0 1 770 202 208 98 3 834 864 98 2 898 171 81 898 511 39 2592 834 960 98 1 994 3 1 3 2592 834 1040 98 1 32 2592 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 0 0 0 40 0 0 0 84 1 0 0 59 0 0 0] 98 0 1136 0 27 1154 1200 21 1248 -19 1280 1 1312 39 410 1808 98 16 0 416 98 2 8 1140850944 1 2960 0 0 0 5 0 0 0 2960 0 8 4294902553 738 0 0 0 770 202 208 98 2 834 864 98 2 898 21 21 898 71 41 2960 834 1680 98 1 8 'Path:' 2960 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 45 0 0 0 30 0 0 0] 98 0 1136 0 27 1154 2128 21 2160 71 1194 1216 8 #fixedParentTop 21 1312 41 410 1808 98 16 0 416 98 2 8 1140850944 1 3312 0 0 0 5 0 0 0 3312 0 8 4294902553 738 0 0 0 770 202 208 98 2 834 864 98 2 898 21 81 898 131 41 3312 834 1680 98 1 8 'Parameters:' 3312 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 40 0 0 0 75 0 0 0 60 0 0 0] 98 0 1136 0 27 1154 2128 21 2160 131 2192 23 1312 41 234 256 98 6 592 8 'description' 2224 8 'path' 2592 8 'parameters' 0 770 202 208 98 1 834 864 98 2 898 3839 21 898 701 291 416 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 221 8 0 0 155 0 0 0] 98 7 2960 2224 1344 3312 2592 1792 592 1136 0 27 )! !
!ExternalToolPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

