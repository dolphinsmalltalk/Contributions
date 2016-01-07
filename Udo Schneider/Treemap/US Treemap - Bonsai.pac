| package |
package := Package name: 'US Treemap - Bonsai'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Bonsai 0.038$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.038'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAAAAAAAAUgAAAAYAAABCb25zYWlSAAAAGAAA
AFVkbyBTY2huZWlkZXJcQm9uc2FpLmV4ZZoAAAAAAAAAUgAAAAYAAABCb25zYWlSAAAAFAAAAEJv
bnNhaVNlc3Npb25NYW5hZ2Vy778lAAAAAAAGAw8AVmVyc2lvblJlc291cmNlAAAAAAYBEABWU19G
SVhFREZJTEVJTkZPAAAAAHIAAAA0AAAAvQTv/gAAAQAAAAEAAQAAAAAAAQABAAAAPwAAAAAAAAAE
AAAAAgAAAAAAAAAAAAAAAAAAAOoAAAAAAAAA8AAAAGIAAAACAAAAUgAAAAgAAAAwNDA5MDRiMOoA
AAAAAAAA8AAAAGIAAAAYAAAAUgAAAA4AAABQcm9kdWN0VmVyc2lvblIAAAAKAAAAMSwgMCwgMCwg
MVIAAAALAAAAQ29tcGFueU5hbWVSAAAAAAAAAFIAAAAMAAAAUHJpdmF0ZUJ1aWxkUgAAAAAAAABS
AAAADAAAAFNwZWNpYWxCdWlsZFIAAAAAAAAAUgAAAA8AAABGaWxlRGVzY3JpcHRpb25SAAAAGwAA
AERvbHBoaW4gWFAgVG9HbyBBcHBsaWNhdGlvblIAAAAPAAAATGVnYWxUcmFkZW1hcmtzUgAAADEA
AABEb2xwaGluIGlzIGEgdHJhZGVtYXJrIG9mIENHSSBHcm91cCAoRXVyb3BlKSBMdGQuUgAAAAwA
AABJbnRlcm5hbE5hbWVSAAAAAAAAAFIAAAAQAAAAT3JpZ2luYWxGaWxlbmFtZVIAAAAKAAAAQm9u
c2FpLmV4ZVIAAAAOAAAATGVnYWxDb3B5cmlnaHRSAAAAKwAAAFBvcnRpb25zIENvcHlyaWdodCCp
IE9iamVjdCBBcnRzIDE5OTctMjAwMy5SAAAACAAAAENvbW1lbnRzUgAAABwAAABQb3dlcmVkIGJ5
IERvbHBoaW4gU21hbGx0YWxrUgAAAAsAAABGaWxlVmVyc2lvblIAAAAKAAAAMSwgMCwgMCwgMVIA
AAALAAAAUHJvZHVjdE5hbWVSAAAAHQAAAEEgRG9scGhpbiBYUCBUb0dvIEFwcGxpY2F0aW9uygAA
AAAAAADQAAAAYgAAAAEAAAAGAgoARFdPUkRBcnJheQAAAAByAAAABAAAAAkEsAQDAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=').

package classNames
	add: #BonsaiFile;
	add: #BonsaiFolder;
	add: #BonsaiPath;
	add: #BonsaiSessionManager;
	add: #BonsaiShell;
	add: #OutlookMail;
	add: #OutlookMailFolder;
	add: #OutlookMailItem;
	yourself.

package methodNames
	add: #Tooltip -> #registerViewSubclass:;
	add: #WIN32_FIND_DATA -> #isDotFile;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Choice\Dolphin Choice Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\MoenTree\Dolphin MoenTree View';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\Scrollbars\Dolphin Scrollbars';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\Tooltips\Dolphin Tooltips';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Tree\Dolphin Tree Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	add: '..\..\Type Libraries\Outlook';
	add: '..\Graphics\US Color Extensions';
	add: 'US Treemap - Base';
	add: 'US Treemap - Layout Ordered';
	add: 'US Treemap - Layout Slice and Dice';
	add: 'US Treemap - Layout Squarified';
	add: 'US Treemap - Render Circle';
	add: 'US Treemap - Render Cushion Abstract';
	add: 'US Treemap - Render Cushion DirectToMemory';
	add: 'US Treemap - Render Cushion External';
	add: 'US Treemap - Render Cushion Optimized';
	add: 'US Treemap - Render Flat';
	add: 'US Treemap - View';
	add: '..\..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package setManualPrerequisites: #(
	'US Color Extensions').

package!

"Class Definitions"!

Object subclass: #BonsaiPath
	instanceVariableNames: 'path children parent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #OutlookMailItem
	instanceVariableNames: 'item children parent name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BonsaiPath subclass: #BonsaiFile
	instanceVariableNames: 'size'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BonsaiPath subclass: #BonsaiFolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OutlookMailItem subclass: #OutlookMail
	instanceVariableNames: 'size creationTime color'
	classVariableNames: ''
	poolDictionaries: 'OutlookConstants'
	classInstanceVariableNames: ''!
OutlookMailItem subclass: #OutlookMailFolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #BonsaiShell
	instanceVariableNames: 'pathPresenter renderStrategyPresenter layoutStrategyPresenter treemapPresenter treePresenter moenTreePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #BonsaiSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Tooltip methodsFor!

registerViewSubclass: aView 
	self ttmAddTool: ((TOOLINFOA new)
				uFlags: ##(TTF_IDISHWND | TTF_SUBCLASS );
				textCallback;
				hwnd: aView asParameter;
				uId: aView asParameter;
				yourself)! !
!Tooltip categoriesFor: #registerViewSubclass:!*-not in class package!operations!private! !

!WIN32_FIND_DATA methodsFor!

isDotFile
	^#('.' '..') includes: self fileName! !
!WIN32_FIND_DATA categoriesFor: #isDotFile!*-not in class package!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

BonsaiPath guid: (GUID fromString: '{11AA1B6D-9D26-43AE-9919-B5E30BB2C47F}')!
BonsaiPath comment: ''!
!BonsaiPath categoriesForClass!Unclassified! !
!BonsaiPath methodsFor!

<= aBonsaiPath 
		self subclassResponsibility!

children
	children isNil ifTrue: [children := self getChildren asSortedCollection].
	^children!

compareWithFile: aBonsaiPath 
	self subclassResponsibility!

compareWithFolder: aBonsaiPath
self subclassResponsibility!

getChildren
self subclassResponsibility!

isDirectory
	self subclassResponsibility!

parent^parent!

path
	^path!

pathComponent
^self path!

printOn: aStream 
	super printOn: aStream.
	aStream nextPutAll: ' (' , self path , ')'!

setParent: anObject
"Private"
parent := anObject!

setPath: aString 
	path := aString!

size
	^0! !
!BonsaiPath categoriesFor: #<=!*-in class package!public! !
!BonsaiPath categoriesFor: #children!*-in class package!public! !
!BonsaiPath categoriesFor: #compareWithFile:!*-in class package!private! !
!BonsaiPath categoriesFor: #compareWithFolder:!*-in class package!private! !
!BonsaiPath categoriesFor: #getChildren!*-in class package!private! !
!BonsaiPath categoriesFor: #isDirectory!*-in class package!public! !
!BonsaiPath categoriesFor: #parent!*-in class package!private! !
!BonsaiPath categoriesFor: #path!*-in class package!accessing!public! !
!BonsaiPath categoriesFor: #pathComponent!*-in class package!public! !
!BonsaiPath categoriesFor: #printOn:!*-in class package!public! !
!BonsaiPath categoriesFor: #setParent:!*-in class package!private! !
!BonsaiPath categoriesFor: #setPath:!*-in class package!private! !
!BonsaiPath categoriesFor: #size!*-in class package!accessing!public! !

!BonsaiPath class methodsFor!

fromWIN32_FIND_DATA: aWIN32_FIND_DATA parent: aBonsaiPath 
	self subclassResponsibility! !
!BonsaiPath class categoriesFor: #fromWIN32_FIND_DATA:parent:!*-in class package!public! !

OutlookMailItem guid: (GUID fromString: '{38A1AB82-3780-4BB0-B0F8-3973E5803994}')!
OutlookMailItem comment: ''!
!OutlookMailItem categoriesForClass!Unclassified! !
!OutlookMailItem methodsFor!

<= aBonsaiPath 
		self subclassResponsibility!

children
	children isNil ifTrue: [children := self getChildren ].
	^children!

color
^Color white!

getChildren
self subclassResponsibility!

isDirectory
	self subclassResponsibility!

name
self subclassResponsibility!

parent

	^parent!

printOn: aStream 
	super printOn: aStream.
	aStream nextPutAll: ' (' , self name , ')'!

setItem: anItem parent: aParent 
	item := anItem.
	parent := aParent!

size
	^0! !
!OutlookMailItem categoriesFor: #<=!*-in class package!public! !
!OutlookMailItem categoriesFor: #children!*-in class package!public! !
!OutlookMailItem categoriesFor: #color!*-in class package!public! !
!OutlookMailItem categoriesFor: #getChildren!*-in class package!private! !
!OutlookMailItem categoriesFor: #isDirectory!*-in class package!public! !
!OutlookMailItem categoriesFor: #name!*-in class package!public! !
!OutlookMailItem categoriesFor: #parent!*-in class package!private! !
!OutlookMailItem categoriesFor: #printOn:!*-in class package!public! !
!OutlookMailItem categoriesFor: #setItem:parent:!*-in class package!public! !
!OutlookMailItem categoriesFor: #size!*-in class package!accessing!public! !

!OutlookMailItem class methodsFor!

item: anItem parent: aParent
	^self new setItem: anItem parent: aParent! !
!OutlookMailItem class categoriesFor: #item:parent:!*-in class package!public! !

BonsaiFile guid: (GUID fromString: '{D8C70044-A077-4126-AC0A-09FCFB7C9A6D}')!
BonsaiFile comment: ''!
!BonsaiFile categoriesForClass!Unclassified! !
!BonsaiFile methodsFor!

<= aBonsaiPath 
	^aBonsaiPath compareWithFile: self!

compareWithFile: aBonsaiPath 
	^ aBonsaiPath path<=  self path!

compareWithFolder: aBonsaiPath 
	^true!

getChildren
	^#()!

isDirectory
	^false!

pathComponent
	^File titleOf: self  path!

setSize: anObject 
	size := anObject!

size
	^size! !
!BonsaiFile categoriesFor: #<=!*-in class package!public! !
!BonsaiFile categoriesFor: #compareWithFile:!*-in class package!private! !
!BonsaiFile categoriesFor: #compareWithFolder:!*-in class package!private! !
!BonsaiFile categoriesFor: #getChildren!*-in class package!private! !
!BonsaiFile categoriesFor: #isDirectory!*-in class package!public! !
!BonsaiFile categoriesFor: #pathComponent!*-in class package!public! !
!BonsaiFile categoriesFor: #setSize:!*-in class package!accessing!private! !
!BonsaiFile categoriesFor: #size!*-in class package!accessing!private! !

!BonsaiFile class methodsFor!

fromWIN32_FIND_DATA: aWIN32_FIND_DATA parent: anBonsaiFolder 
	^(self new)
		setPath: aWIN32_FIND_DATA path;
		setSize: aWIN32_FIND_DATA fileSize;
		setParent: anBonsaiFolder;
		yourself!

icon
	^File icon! !
!BonsaiFile class categoriesFor: #fromWIN32_FIND_DATA:parent:!*-in class package!public! !
!BonsaiFile class categoriesFor: #icon!*-in class package!public! !

BonsaiFolder guid: (GUID fromString: '{B2694A66-00CC-4106-8801-6392ADBEF1E2}')!
BonsaiFolder comment: ''!
!BonsaiFolder categoriesForClass!Unclassified! !
!BonsaiFolder methodsFor!

<= aBonsaiPath 
	^aBonsaiPath compareWithFolder: self!

compareWithFile: aBonsaiPath 
	^false!

compareWithFolder: aBonsaiPath 
	^aBonsaiPath path  <= self path!

getChildren
	children := OrderedCollection new.
	File 
		for: '*.*'
		in: self path
		do: 
			[:each | 
			each isDirectory 
				ifTrue: 
					[each isDotFile 
						ifFalse: 
							[children add: ((BonsaiFolder fromWIN32_FIND_DATA: each parent: self)
										setParent: self;
										yourself)]]
				ifFalse: [children add: (BonsaiFile fromWIN32_FIND_DATA: each parent: self)]].
	^children !

isDirectory
	^true!

pathComponent
	^(self  path subStrings: '\') last displayString! !
!BonsaiFolder categoriesFor: #<=!*-in class package!public! !
!BonsaiFolder categoriesFor: #compareWithFile:!*-in class package!private! !
!BonsaiFolder categoriesFor: #compareWithFolder:!*-in class package!private! !
!BonsaiFolder categoriesFor: #getChildren!*-in class package!private! !
!BonsaiFolder categoriesFor: #isDirectory!*-in class package!public! !
!BonsaiFolder categoriesFor: #pathComponent!*-in class package!public! !

!BonsaiFolder class methodsFor!

fromString: aString 
	^(self new)
		setPath: aString;
		yourself!

fromWIN32_FIND_DATA: aWIN32_FIND_DATA parent: anBonsaiFolder 
	^(self new)
		setPath: aWIN32_FIND_DATA path , '\';
		setParent: anBonsaiFolder;
		yourself!

icon
	^Folder icon! !
!BonsaiFolder class categoriesFor: #fromString:!*-in class package!public! !
!BonsaiFolder class categoriesFor: #fromWIN32_FIND_DATA:parent:!*-in class package!public! !
!BonsaiFolder class categoriesFor: #icon!*-in class package!public! !

OutlookMail guid: (GUID fromString: '{256CF5D7-7E8D-47B2-9E01-D971156B1AC9}')!
OutlookMail comment: ''!
!OutlookMail categoriesForClass!Unclassified! !
!OutlookMail methodsFor!

<= aBonsaiPath 
	^aBonsaiPath compareWithFile: self!

color
	color isNil 
		ifTrue: 
			[color := 
					[##(| colors |
					colors := Dictionary new.
					colors
						at: olRedFlagIcon
							put: (Color 
									red);
						at: olBlueFlagIcon put: Color blue;
						at: olYellowFlagIcon put: Color yellow;
						at: olGreenFlagIcon put: Color green;
						at: olOrangeFlagIcon put: Color orange;
						at: olPurpleFlagIcon put: Color purple.
					colors) at: item flagIcon ifAbsent: [Color white]] 
							on: Error
							do: [:ex | Color white]].
	^color!

compareWithFile: aBonsaiPath 
	^ aBonsaiPath creationTime <=  self creationTime!

compareWithFolder: aBonsaiPath 
	^true!

creationTime
creationTime isNil ifTrue: [creationTime  := item creationTime asTimeStamp].^creationTime!

displayMail
	item display!

getChildren
	^#()!

isDirectory
	^false!

name
	name isNil ifTrue: [name := item subject ]. ^name!

size
	size isNil ifTrue: [size := item Size].
	^size! !
!OutlookMail categoriesFor: #<=!*-in class package!public! !
!OutlookMail categoriesFor: #color!*-in class package!public! !
!OutlookMail categoriesFor: #compareWithFile:!*-in class package!private! !
!OutlookMail categoriesFor: #compareWithFolder:!*-in class package!private! !
!OutlookMail categoriesFor: #creationTime!*-in class package!private! !
!OutlookMail categoriesFor: #displayMail!*-in class package!public! !
!OutlookMail categoriesFor: #getChildren!*-in class package!private! !
!OutlookMail categoriesFor: #isDirectory!*-in class package!public! !
!OutlookMail categoriesFor: #name!*-in class package!public! !
!OutlookMail categoriesFor: #size!*-in class package!accessing!private! !

!OutlookMail class methodsFor!

icon
	^File icon! !
!OutlookMail class categoriesFor: #icon!*-in class package!public! !

OutlookMailFolder guid: (GUID fromString: '{AF3F8977-539D-4C97-9B06-D451D5B389D2}')!
OutlookMailFolder comment: ''!
!OutlookMailFolder categoriesForClass!Unclassified! !
!OutlookMailFolder methodsFor!

<= aBonsaiPath 
	^aBonsaiPath compareWithFolder: self!

compareWithFile: aBonsaiPath 
	^false!

compareWithFolder: aBonsaiPath 
	^aBonsaiPath name  <= self name!

getChildren
	| temp folders items |
	temp := OrderedCollection new.
	folders := item folders.
	1 to: folders count do: [:index | temp add: (OutlookMailFolder item: (folders item: index) parent: self)].
items := item items.
1 to: items count do: [ :index |temp add: (OutlookMail item: (items item: index) parent: self)].
	^temp!

isDirectory
	^true!

name
name isNil ifTrue: [name  := item name].
^name! !
!OutlookMailFolder categoriesFor: #<=!*-in class package!public! !
!OutlookMailFolder categoriesFor: #compareWithFile:!*-in class package!private! !
!OutlookMailFolder categoriesFor: #compareWithFolder:!*-in class package!private! !
!OutlookMailFolder categoriesFor: #getChildren!*-in class package!private! !
!OutlookMailFolder categoriesFor: #isDirectory!*-in class package!public! !
!OutlookMailFolder categoriesFor: #name!*-in class package!public! !

!OutlookMailFolder class methodsFor!

icon
	^Folder icon! !
!OutlookMailFolder class categoriesFor: #icon!*-in class package!public! !

BonsaiShell guid: (GUID fromString: '{C01B730C-CECF-4E29-B30E-370041C2BD53}')!
BonsaiShell comment: ''!
!BonsaiShell categoriesForClass!Unclassified! !
!BonsaiShell methodsFor!

availableLayoutStrategies 
^##(TreeMapRenderer availableLayoutStrategies )!

availableRenderStrategies
	^##(TreeMapRenderer availableRenderStrategies)!

basicCaption 
^'Bonsai - '!

browseFolder
 |folder|
	folder := BrowseFolderDialog showModalOn: pathPresenter value.
	folder notNil ifTrue: [ self updateFolder: folder]!

createComponents
	super createComponents.
	pathPresenter := self add: TextPresenter new name: 'path'.
	layoutStrategyPresenter := self add: ChoicePresenter new name: 'layoutStrategy'.
	renderStrategyPresenter := self add: ChoicePresenter new name: 'renderStrategy'.
	treemapPresenter := self add: TreePresenter new name: 'treemap'.
	treePresenter := self add: TreePresenter new name: 'tree'.
moenTreePresenter := self add: TreePresenter new name: 'moentree'!

createSchematicWiring
	super createSchematicWiring.
	layoutStrategyPresenter 
		when: #selectionChanged
		send: #updateLayoutStrategy
		to: self.
	renderStrategyPresenter 
		when: #selectionChanged
		send: #updateRenderStrategy
		to: self.
	treemapPresenter 
		when: #selectionChanged
		send: #treemapSelectionChanged
		to: self.
	treePresenter 
		when: #selectionChanged
		send: #treeSelectionChanged
		to: self.
	moenTreePresenter 
		when: #selectionChanged
		send: #moenTreeSelectionChanged
		to: self.
	pathPresenter 
		when: #valueChanged
		send: #pathChanged
		to: self!

model: aModel 
	| treeModel |
	super model: (BonsaiFolder fromString: aModel).
	self updateCaption.
	treeModel := self treeModel.
	treePresenter model: treeModel.
	moenTreePresenter model: treeModel.
	treemapPresenter model: treeModel.
	layoutStrategyPresenter
		choices: (self availableLayoutStrategies 
					asSortedCollection: [:x :y | x description <= y description]);
		value: self availableLayoutStrategies first.
	renderStrategyPresenter
		choices: (self availableRenderStrategies 
					asSortedCollection: [:x :y | x description <= y description]);
		value: self availableRenderStrategies first.
	pathPresenter value: self model path!

moenTreeSelectionChanged
	treePresenter selection: moenTreePresenter selection.
treemapPresenter selection: moenTreePresenter selection.
!

onViewClosed
	super onViewClosed.
	SessionManager current isRuntime ifTrue: [SessionManager current exit]!

onViewOpened
	self updateLayoutStrategy; updateRenderStrategy.^super onViewOpened!

pathChanged
self updateFolder: pathPresenter value!

treemapSelectionChanged
	treePresenter selection: treemapPresenter selection.
	moenTreePresenter selection: treemapPresenter selection.
!

treeModel
^ VirtualTreeModel withRoots: (Array with: self model).
!

treeSelectionChanged
	treemapPresenter selection: treePresenter selection.
moenTreePresenter selection: treePresenter selection.!

updateCaption
	self caption: self basicCaption , self model path!

updateFolder: aString 
	((File exists: aString) and: [File isDirectory: aString]) 
		ifTrue: [self model: ( aString )]
		ifFalse: [MessageBox warning: '''' , aString , ''' does not exist or is no directory']!

updateLayoutStrategy
	treemapPresenter view renderer layoutStrategy: layoutStrategyPresenter value new!

updateRenderStrategy
		treemapPresenter view renderer renderStrategy: renderStrategyPresenter value new! !
!BonsaiShell categoriesFor: #availableLayoutStrategies!*-in class package!private! !
!BonsaiShell categoriesFor: #availableRenderStrategies!*-in class package!private! !
!BonsaiShell categoriesFor: #basicCaption!*-in class package!private! !
!BonsaiShell categoriesFor: #browseFolder!*-in class package!public! !
!BonsaiShell categoriesFor: #createComponents!*-in class package!public! !
!BonsaiShell categoriesFor: #createSchematicWiring!*-in class package!public! !
!BonsaiShell categoriesFor: #model:!*-in class package!public! !
!BonsaiShell categoriesFor: #moenTreeSelectionChanged!*-in class package!public! !
!BonsaiShell categoriesFor: #onViewClosed!*-in class package!public! !
!BonsaiShell categoriesFor: #onViewOpened!*-in class package!public! !
!BonsaiShell categoriesFor: #pathChanged!*-in class package!public! !
!BonsaiShell categoriesFor: #treemapSelectionChanged!*-in class package!public! !
!BonsaiShell categoriesFor: #treeModel!*-in class package!public! !
!BonsaiShell categoriesFor: #treeSelectionChanged!*-in class package!public! !
!BonsaiShell categoriesFor: #updateCaption!*-in class package!private! !
!BonsaiShell categoriesFor: #updateFolder:!*-in class package!public! !
!BonsaiShell categoriesFor: #updateLayoutStrategy!*-in class package!public! !
!BonsaiShell categoriesFor: #updateRenderStrategy!*-in class package!public! !

!BonsaiShell class methodsFor!

defaultModel
	^SessionManager current imageBase!

referencedClasses
"stream := ReadWriteStream on: String new.
TreeMapRenderer availableLayoutStrategies , TreeMapRenderer availableRenderStrategies 
	do: 
		[:each | 
		stream
			nextPutAll: each displayString;
			nextPut: $.;
			cr].
stream contents
"

	OrderedSquarifiedTreeMapStrategy.
	SliceAndDiceLayoutStrategy.
	PivotByBiggestStrategy.
	PivotByMiddleStrategy.
	PivotBySplitSizeStrategy.
	SortedSquarifiedTreeMapStrategy.
	FlatRenderStrategy.
	CircleRenderStrategy.
	DirectCushionRenderStrategy.
	ExternalCushionRenderStrategy.
	OptimizedCushionRenderStrategy.
	VanillaCushionRenderStrategy!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 517 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 560 0 482 512 0 5 0 0 0 560 852230 ##(Smalltalk.FramingLayout)  234 240 98 6 410 8 ##(Smalltalk.ComboBox)  98 17 0 560 98 2 8 1144063491 1025 720 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 5 0 0 0 720 0 8 4294903933 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[ :each | each description]' 8 #[30 105 226 0 106] 8 #description 976 7 257 0 848 401 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  509 1 1234 321 43 720 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 0 0 0 0 0 0 0 158 1 0 0 21 0 0 0] 98 0 1234 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  874 8 ##(Smalltalk.FramingCalculation)  8 #fixedParentRight -639 1392 -319 874 1408 8 #fixedParentTop 1 874 1408 8 #fixedParentBottom 1 410 736 98 17 0 560 98 2 8 1144063491 1025 1504 802 202 208 848 0 880 482 512 0 5 0 0 0 1504 0 8 4294903933 962 0 0 994 2 1 1024 8 'doIt' 8 '[ :each | each description]' 8 #[30 105 226 0 106] 1088 1632 7 257 0 848 401 1106 202 208 98 1 1170 1200 98 2 1234 829 1 1234 321 43 1504 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 158 1 0 0 0 0 0 0 62 2 0 0 21 0 0 0] 98 0 1344 0 27 1362 1392 -319 1392 1 1440 1 1472 1 410 8 ##(Smalltalk.TextEdit)  98 16 0 560 98 2 8 1140916352 1025 1888 0 482 512 0 5 0 0 0 1888 0 8 4294904081 852486 ##(Smalltalk.NullConverter)  0 0 1 1106 202 208 98 3 1170 1200 98 2 1234 1 1 1234 509 41 1888 1170 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1888 1170 8 #isTextModified: 98 1 32 1888 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 0 0 0 20 0 0 0] 98 0 1344 0 27 1362 874 1408 8 #fixedParentLeft 1 1392 -639 1440 1 1472 1 234 256 98 6 720 8 'layoutStrategy' 1504 8 'renderStrategy' 1888 8 'path' 0 1106 202 208 98 1 1170 1200 98 2 1234 1 1 1234 1149 41 560 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 62 2 0 0 20 0 0 0] 98 3 1888 720 1504 1344 0 27 0 0 0 410 576 98 15 0 416 98 2 8 1140850688 131073 2608 0 482 512 0 5 0 0 0 2608 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 2 410 576 98 15 0 2608 98 2 8 1140850688 131073 2752 0 482 512 0 5 0 0 0 2752 2690 234 240 98 2 410 8 ##(Smalltalk.AsynchronousTreeMapView)  98 23 0 2752 98 2 8 1140850688 1 2880 590918 3 ##(Smalltalk.TreeModel)  0 880 525062 ##(Smalltalk.TreeNode)  0 0 0 234 256 848 786694 ##(Smalltalk.IndexedColor)  33554433 0 5 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 0 0 0 0 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1234 193 193 0 2880 787206 ##(Smalltalk.TreeMapModel)  2976 722438 ##(Smalltalk.TreeMapNode)  0 0 3184 202 208 848 1 590342 ##(Smalltalk.Rectangle)  1234 1 1 1234 859 647 984582 ##(Smalltalk.TreeMapRenderer)  3184 3264 962 0 0 994 1 83886081 1024 8 'doIt' 8 '[ :object | object size]' 8 #[29 105 17 145 106] 3344 7 257 0 962 0 0 994 3 1 1024 8 'doIt' 8 '[ :object |  Color white]' 8 #[31 105 29 159 106] 8 ##(Smalltalk.Color)  8 #white 3424 7 257 0 1704198 ##(Smalltalk.SliceAndDiceLayoutStrategy)  1180422 ##(Smalltalk.FlatRenderStrategy)  3328 657990 3 ##(Smalltalk.DIBSection)  0 16 0 0 0 0 1 1234 2801 2101 65 0 395270 ##(Smalltalk.Canvas)  0 0 32 0 0 0 0 0 3584 3328 3616 32 0 962 0 0 994 2 1 1024 8 'doIt' 8 '[ :each | each path]' 8 #[30 105 226 0 106] 8 #path 3680 7 257 0 0 0 0 0 8 'Rendering TreeMap' 1106 202 208 98 2 1170 1200 98 2 1234 291 1 1234 859 647 2880 1170 8 #text: 98 1 3776 2880 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 0 0 0 0 62 2 0 0 67 1 0 0] 98 0 1344 0 27 7 32 234 256 98 4 410 8 ##(Smalltalk.TreeView)  98 27 0 2752 98 2 8 1140916263 1025 4032 2962 0 880 2994 0 0 0 234 256 848 482 512 0 5 0 0 0 4032 0 8 4294904133 962 0 0 994 2 1 1024 8 'doIt' 8 '[ :object | object pathComponent]' 8 #[30 105 226 0 106] 8 #pathComponent 4192 7 257 0 8 ##(Smalltalk.IconicListAbstract)  874 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 234 240 848 17 8 #smallIcons 1 0 1106 202 208 98 1 1170 1200 98 2 1234 1 1 1234 285 647 4032 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 142 0 0 0 67 1 0 0] 98 0 1344 0 27 8 'tree' 2880 8 'treemap' 0 1106 202 208 98 1 1170 1200 98 2 1234 1 1 1234 1149 647 2752 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 62 2 0 0 67 1 0 0] 98 3 4032 410 8 ##(Smalltalk.Splitter)  98 12 0 2752 98 2 8 1140850688 1 4736 0 482 512 0 517 0 0 0 4736 1106 202 208 98 1 1170 1200 98 2 1234 285 1 1234 7 647 4736 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 142 0 0 0 0 0 0 0 145 0 0 0 67 1 0 0] 98 0 1344 0 27 2880 1344 0 27 5 16 234 256 848 0 1106 202 208 98 1 1170 1200 98 2 1234 1 41 1234 1149 977 2608 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 62 2 0 0 252 1 0 0] 98 3 2752 410 4752 98 12 0 2608 98 2 8 1140850688 1 5168 0 482 512 0 517 0 0 0 5168 1106 202 208 98 1 1170 1200 98 2 1234 1 647 1234 1149 7 5168 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 67 1 0 0 62 2 0 0 70 1 0 0] 98 0 1344 0 27 410 8 ##(Smalltalk.ScrollingDecorator)  98 18 0 2608 98 2 8 1143996416 131073 5408 0 482 512 0 5 0 0 0 5408 1573190 1 ##(Smalltalk.ScrollingDecoratorLayout)  16 234 256 98 2 410 8 ##(Smalltalk.MoenTreeView)  98 30 0 5408 98 2 8 1140850688 1 5568 2962 0 880 2994 0 0 0 234 256 848 482 512 0 517 0 0 0 5568 788998 ##(Smalltalk.MoenTreeNode)  0 721926 ##(Smalltalk.MoenContour)  0 0 0 0 1234 1 1 5776 0 0 0 0 0 7 962 0 0 994 2 1 1024 8 'doIt' 8 '[ :object | object pathComponent]' 8 #[30 105 226 0 106] 4272 5792 7 257 0 5584 3 1234 5 3 4304 1234 1 1 41 0 197382 ##(Smalltalk.Pen)  0 16 393478 ##(Smalltalk.LOGPEN)  8 #[0 0 0 0 1 0 0 0 0 0 0 0 192 192 192 0] 1234 33 33 1234 19999 19999 114721 5888 1234 35 1 0 0 0 1106 202 208 98 1 1170 1200 98 2 1234 1 1 1234 1149 325 5568 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 62 2 0 0 162 0 0 0] 98 0 1344 0 27 8 'moentree' 0 1234 1 1 16 1234 17 17 1106 202 208 98 1 1170 1200 98 2 1234 1 653 1234 1149 325 5408 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 70 1 0 0 62 2 0 0 232 1 0 0] 98 1 5568 1344 0 27 1344 0 27 234 256 848 0 0 0 0 0 1 0 0 0 0 1 0 0 1106 202 208 98 2 1170 1200 98 2 1234 309 407 1234 2101 1483 416 1170 8 #updateMenuBar 848 416 1282 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 154 0 0 0 203 0 0 0 180 4 0 0 176 3 0 0] 98 2 560 2608 1344 0 27 )! !
!BonsaiShell class categoriesFor: #defaultModel!*-in class package!public! !
!BonsaiShell class categoriesFor: #referencedClasses!*-in class package!public! !
!BonsaiShell class categoriesFor: #resource_Default_view!*-in class package!public!resources-views! !

BonsaiSessionManager guid: (GUID fromString: '{8D850ACB-A8BA-4610-BF9C-8543DCE5E6B2}')!
BonsaiSessionManager comment: ''!
!BonsaiSessionManager categoriesForClass!Unclassified! !
!BonsaiSessionManager methodsFor!

main
	self mainShellClass show! !
!BonsaiSessionManager categoriesFor: #main!*-in class package!public! !

!BonsaiSessionManager class methodsFor!

mainShellClass
	"Answer the class of the application's main window (a <Shell> presenter)."

	^BonsaiShell! !
!BonsaiSessionManager class categoriesFor: #mainShellClass!*-in class package!constants!public! !

"Binary Globals"!

