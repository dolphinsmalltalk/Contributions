| package |
package := Package name: 'US Treemap - View'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - View 0.069$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.069'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAAAAAAAAUgAAAA4AAABVUyBUcmVlTWFwVmll
d1IAAAAAAAAAmgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAAFQAAAFJ1bnRpbWVT
ZXNzaW9uTWFuYWdlcu+/JQAAAAAABgMPAFZlcnNpb25SZXNvdXJjZQAAAAAGARAAVlNfRklYRURG
SUxFSU5GTwAAAAByAAAANAAAAL0E7/4AAAEAAAABAAEAAAAAAAEAAQAAAD8AAAAAAAAABAAAAAIA
AAAAAAAAAAAAAAAAAADqAAAAAAAAAPAAAABiAAAAAgAAAFIAAAAIAAAAMDQwOTA0YjDqAAAAAAAA
APAAAABiAAAAGAAAAFIAAAAOAAAAUHJvZHVjdFZlcnNpb25SAAAACgAAADEsIDAsIDAsIDFSAAAA
CwAAAENvbXBhbnlOYW1lUgAAAAAAAABSAAAADAAAAFByaXZhdGVCdWlsZFIAAAAAAAAAUgAAAAwA
AABTcGVjaWFsQnVpbGRSAAAAAAAAAFIAAAAPAAAARmlsZURlc2NyaXB0aW9uUgAAABsAAABEb2xw
aGluIFhQIFRvR28gQXBwbGljYXRpb25SAAAADwAAAExlZ2FsVHJhZGVtYXJrc1IAAAAxAAAARG9s
cGhpbiBpcyBhIHRyYWRlbWFyayBvZiBDR0kgR3JvdXAgKEV1cm9wZSkgTHRkLlIAAAAMAAAASW50
ZXJuYWxOYW1lUgAAAAAAAABSAAAAEAAAAE9yaWdpbmFsRmlsZW5hbWVSAAAAAAAAAFIAAAAOAAAA
TGVnYWxDb3B5cmlnaHRSAAAAKwAAAFBvcnRpb25zIENvcHlyaWdodCCpIE9iamVjdCBBcnRzIDE5
OTctMjAwMy5SAAAACAAAAENvbW1lbnRzUgAAABwAAABQb3dlcmVkIGJ5IERvbHBoaW4gU21hbGx0
YWxrUgAAAAsAAABGaWxlVmVyc2lvblIAAAAKAAAAMSwgMCwgMCwgMVIAAAALAAAAUHJvZHVjdE5h
bWVSAAAAHQAAAEEgRG9scGhpbiBYUCBUb0dvIEFwcGxpY2F0aW9uygAAAAAAAADQAAAAYgAAAAEA
AAAGAgoARFdPUkRBcnJheQAAAAByAAAABAAAAAkEsAQDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAA=').

package classNames
	add: #AsynchronousTreeMapView;
	add: #TreeMapView;
	yourself.

package methodNames
	add: #Color -> #negative;
	add: 'Brush class' -> #checkerboard;
	add: 'Brush class' -> #hatchedDiagonalCross;
	add: 'ListPresenter class' -> #resource_Treemap_view;
	add: 'ListPresenter class' -> #resource_Treemap_view_asynchronous;
	add: 'TreePresenter class' -> #resource_Treemap_view;
	add: 'TreePresenter class' -> #resource_Treemap_view_asynchronous;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\Tooltips\Dolphin Tooltips';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Tree\Dolphin Tree Presenter';
	add: '..\..\..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	add: 'US Treemap - Base';
	add: 'US Treemap - Layout Slice and Dice';
	add: 'US Treemap - Render Flat';
	yourself).

package!

"Class Definitions"!

View subclass: #TreeMapView
	instanceVariableNames: 'treeMapModel renderer dib needsUpdate selection getTextBlock getImageBlock tooltip tooltipRectangle'
	classVariableNames: ''
	poolDictionaries: 'TooltipConstants'
	classInstanceVariableNames: ''!
TreeMapView subclass: #AsynchronousTreeMapView
	instanceVariableNames: 'renderProcess renderText'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Brush class methodsFor!

checkerboard
	^self 
		withStyle: BS_PATTERN
		color: 0
		hatch: (Bitmap 
				width: 8
				height: 8
				planes: 1
				bitsPerPixel: 1
				bits: #[16rAA 0 16r55 0 16rAA 0 16r55 0 16rAA 0 16r55 0 16rAA 0 16r55 0] yourAddress)!

hatchedDiagonalCross
	"Answer a diagonal crosshatched instance of the receiver"

	^self 
		withStyle: BS_HATCHED
		color: 0
		hatch: HS_DIAGCROSS
! !
!Brush class categoriesFor: #checkerboard!*-not in class package!public! !
!Brush class categoriesFor: #hatchedDiagonalCross!*-not in class package!public! !

!Color methodsFor!

negative
	"Return the color's negative"
	| temp |
	temp := self asRGB.
	^Color 
		red: 255 - temp red
		green: 255 - temp green
		blue: 255 - temp blue! !
!Color categoriesFor: #negative!*-not in class package!converting!public! !

!ListPresenter class methodsFor!

resource_Treemap_view
	"Answer the literal data from which the 'Treemap view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Treemap_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.TreeMapView)  98 21 0 0 98 2 8 1140850688 1 416 590918 3 ##(Smalltalk.TreeModel)  0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 525062 ##(Smalltalk.TreeNode)  0 0 0 234 256 98 0 786694 ##(Smalltalk.IndexedColor)  33554433 0 5 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 0 0 0 0 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 416 0 984582 ##(Smalltalk.TreeMapRenderer)  0 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[ :object | object size]' 8 #[29 105 17 145 106] 848 7 257 0 834 0 0 866 3 1 896 8 'doIt' 8 '[ :object |  Color  white]' 8 #[31 105 29 159 106] 8 ##(Smalltalk.Color)  8 #white 960 7 257 0 1704198 ##(Smalltalk.SliceAndDiceLayoutStrategy)  0 1180422 ##(Smalltalk.FlatRenderStrategy)  0 0 0 0 16 0 8 ##(Smalltalk.BasicListAbstract)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 770 1 1 770 201 201 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 100 0 0 0 100 0 0 0] 98 0 770 193 193 0 27 )!

resource_Treemap_view_asynchronous
	"Answer the literal data from which the 'Treemap view asynchronous' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Treemap_view_asynchronous)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.AsynchronousTreeMapView)  98 23 0 0 98 2 8 1140850688 1 416 590918 3 ##(Smalltalk.TreeModel)  0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 525062 ##(Smalltalk.TreeNode)  0 0 0 234 256 98 0 786694 ##(Smalltalk.IndexedColor)  33554433 0 5 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 0 0 0 0 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 416 0 984582 ##(Smalltalk.TreeMapRenderer)  0 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[ :object | object size]' 8 #[29 105 17 145 106] 848 7 257 0 834 0 0 866 3 1 896 8 'doIt' 8 '[ :object |  Color  white]' 8 #[31 105 29 159 106] 8 ##(Smalltalk.Color)  8 #white 960 7 257 0 1704198 ##(Smalltalk.SliceAndDiceLayoutStrategy)  0 1180422 ##(Smalltalk.FlatRenderStrategy)  0 0 0 0 16 0 8 ##(Smalltalk.BasicListAbstract)  0 0 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 770 1 1 770 201 201 416 1218 8 #text: 98 1 8 'Rendering TreeMap' 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 100 0 0 0 100 0 0 0] 98 0 770 193 193 0 27 )! !
!ListPresenter class categoriesFor: #resource_Treemap_view!*-not in class package!public!resources-views! !
!ListPresenter class categoriesFor: #resource_Treemap_view_asynchronous!*-not in class package!public!resources-views! !

!TreePresenter class methodsFor!

resource_Treemap_view
	"Answer the literal data from which the 'Treemap view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Treemap_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.TreeMapView)  98 21 0 0 98 2 8 1140850688 1 416 590918 3 ##(Smalltalk.TreeModel)  0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 525062 ##(Smalltalk.TreeNode)  0 0 0 234 256 98 0 786694 ##(Smalltalk.IndexedColor)  33554433 0 5 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 0 0 0 0 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 416 0 984582 ##(Smalltalk.TreeMapRenderer)  0 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[ :object | object size]' 8 #[29 105 17 145 106] 848 7 257 0 834 0 0 866 3 1 896 8 'doIt' 8 '[ :object |  Color  white]' 8 #[31 105 29 159 106] 8 ##(Smalltalk.Color)  8 #white 960 7 257 0 1704198 ##(Smalltalk.SliceAndDiceLayoutStrategy)  0 1180422 ##(Smalltalk.FlatRenderStrategy)  0 0 0 0 16 0 8 ##(Smalltalk.BasicListAbstract)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 770 1 1 770 201 201 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 100 0 0 0 100 0 0 0] 98 0 770 193 193 0 27 )!

resource_Treemap_view_asynchronous
	"Answer the literal data from which the 'Treemap view asynchronous' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Treemap_view_asynchronous)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.AsynchronousTreeMapView)  98 23 0 0 98 2 8 1140850688 1 416 590918 3 ##(Smalltalk.TreeModel)  0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 525062 ##(Smalltalk.TreeNode)  0 0 0 234 256 98 0 786694 ##(Smalltalk.IndexedColor)  33554433 0 5 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 0 0 0 0 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 416 0 984582 ##(Smalltalk.TreeMapRenderer)  0 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[ :object | object size]' 8 #[29 105 17 145 106] 848 7 257 0 834 0 0 866 3 1 896 8 'doIt' 8 '[ :object |  Color  white]' 8 #[31 105 29 159 106] 8 ##(Smalltalk.Color)  8 #white 960 7 257 0 1704198 ##(Smalltalk.SliceAndDiceLayoutStrategy)  0 1180422 ##(Smalltalk.FlatRenderStrategy)  0 0 0 0 16 0 8 ##(Smalltalk.BasicListAbstract)  0 0 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 770 1 1 770 201 201 416 1218 8 #text: 98 1 8 'Rendering TreeMap' 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 100 0 0 0 100 0 0 0] 98 0 770 193 193 0 27 )! !
!TreePresenter class categoriesFor: #resource_Treemap_view!*-not in class package!public!resources-views! !
!TreePresenter class categoriesFor: #resource_Treemap_view_asynchronous!*-not in class package!public!resources-views! !

"End of package definition"!

"Source Globals"!

"Classes"!

TreeMapView guid: (GUID fromString: '{FD92BE49-1F90-4073-8191-BBE2DBA17062}')!
TreeMapView comment: ''!
!TreeMapView categoriesForClass!Unclassified! !
!TreeMapView methodsFor!

autoResizeColumns
"Do nothing"!

basicSelection: aTreeMapNode cause: aSymbol 
	"Private - Set the selection to the <TreeMapNode> argument 
	and update the view. Answer the new selection."

	selection == aTreeMapNode 
		ifFalse: 
			[(aSymbol == #unknown or: [self onSelChanging: aTreeMapNode cause: aSymbol]) 
				ifTrue: 
					[| oldSelection |
					oldSelection := self selectedNode.
					oldSelection notNil 
						ifTrue: [self invalidateRect: (oldSelection layoutRectangle truncated ) erase: true].
					selection := aTreeMapNode == treeMapModel rootNode ifFalse: [aTreeMapNode].

					"ensure new selection is visible - might need to expand the tree"
					selection notNil 
						ifTrue: 
							[| rectangle |
					
							rectangle := selection layoutRectangle .
							self invalidateRect: rectangle truncated erase: false.
							self ensureRectangleVisible: rectangle truncated].
					self onSelChanged]].
	^selection!

connectModel
	"Connect the receiver to its model, wiring events, etc."

	(self model)
		when: #treeChanged:
			send: #settingsChanged
			to: self;
		when: #item:addedInParent:
			send: #settingsChanged
			to: self;
		when: #item:removedFromParent:
			send: #settingsChanged
			to: self;
		when: #item:movedToParent:
			send: #settingsChanged
			to: self;
		when: #itemUpdated:
			send: #settingsChanged
			to: self;
		when: #listChanged
			send: #settingsChanged
			to: self!

createDib
	dib := DIBSection 
				width: View desktop clientRectangle width
				height: View desktop  clientRectangle height
				depth: 32!

defaultBackColor
		^Color black!

defaultFont
^(Font name: 'Arial' pointSize: 16)!

defaultGetImageBlock
	"Private - Answer a <monadicValuable>, or nil, to use to find out the image index of an object 
	that will be placed in the receiver. Nil is treated as implying the column has no associated images.
	Implementation Note: Answer nil as by default we do not have per-column images."

	^nil!

defaultGetTextBlock
	"Private - Answer a default block to use to find out the children of an object
	that will be placed in the receiver."

	^BasicListAbstract!

dib
	dib isNil ifTrue: [ self createDib].^dib!

errorNoSelection
	"Private - Raise an error to the effect that there is no selection."

	^self error: 'No object selected'!

findNodeForObject: anObject 
	"Private - Answer the first node in the receiver located using a pre-order traversal from the roots,
	for which the receiver's model's search policy evaluates to true for the <Object> argument..
	If there is no matching node then answer nil. Note that no check is made to verify that the object 
	is actually part of the model."

	"We shouldn't have to special case nil since the anchorNode is associated with that in the tree, but
	 since the search policy might be user supplied it might not consider nil as comparing equal to nil"

	^anObject isNil 
		ifTrue: [treeMapModel rootNode]
		ifFalse: 
			[treeMapModel isNil ifFalse: [treeMapModel nodeForObject: anObject] ifTrue: [nil]]!

getImageBlock
	^nil!

getImageBlock: aBlock 
	!

getTextBlock
^getTextBlock !

getTextBlock: aBlock 
	getTextBlock := aBlock!

hasSelection
	"Answer true if the receiver has a selection."

	^self selectedNode notNil!

hideTooltip
	tooltipRectangle := nil.
	self toolTipWindow pop!

hitTest: position 
	^treeMapModel leafNodeAt: position!

initialize
	super initialize.
	needsUpdate := true.
	self backcolor: self defaultBackColor.
	self font: self defaultFont.
	self renderer: TreeMapRenderer new.
self getImageBlock: self defaultGetImageBlock.
	self getTextBlock: self defaultGetTextBlock!

nodeForObject: anObject ifAbsent: exceptionHandler 
	"Private - Answer the first node in the receiver located using a pre-order traversal from the roots,
	for which the receiver's model's search policy evaluates to true for the <Object>, comperand.
	If there is no matching node, then expand the parent chain and try again."

	| node |
	node := self findNodeForObject: anObject.

			node isNil ifTrue: [exceptionHandler value].
	^node!

onButtonDown: aMouseEvent
	"Private - Handle a mouse click from left or right mouse buttons."

	self setFocus.
	(self hitTest: aMouseEvent position) 
		ifNil: [aMouseEvent isRButtonDown ifTrue: [self basicSelection: nil cause: #mouse]]
		ifNotNil: 
			[:node | 
		
			
				node == self selectedNode 
						ifFalse: 
							[self basicSelection: node cause: #mouse.
							"The selection change may have been refused."
							self selectedNode == node ifFalse: [^self]].
					self isDragSource ifTrue: [self onBegin: aMouseEvent button drag: node object]]!

onLeftButtonDoubleClicked: aMouseEvent 
	"Default handler for a mouse left button double-click event."

	| answer |
	answer := super onLeftButtonDoubleClicked: aMouseEvent.
	self hasSelection ifTrue: [self presenter performAction].
	^answer!

onLeftButtonPressed: aMouseEvent
	"Change the selection on left mouse button. This may also start a drag operation."

	self onButtonDown: aMouseEvent.
	^super onLeftButtonPressed: aMouseEvent!

onModelChanged
	super onModelChanged.
	treeMapModel := selection := nil.
	needsUpdate := true.
	self invalidate!

onMouseMoved: aMouseEvent 
	(tooltipRectangle notNil 
		and: [(tooltipRectangle containsPoint: aMouseEvent position) not]) 
			ifTrue: 
				[self hideTooltip]!

onPaintRequired: aPaintEvent 
	| canvas |
	canvas := aPaintEvent canvas.
	needsUpdate ifTrue: [self updateCachedBitmap].
	self
		renderAllOn: canvas!

onPositionChanged: aPositionEvent 
	needsUpdate := true.
	self  invalidate.
	^super onPositionChanged: aPositionEvent!

onRightButtonPressed: aMouseEvent
	"Change the selection on right mouse button and pop the context menu.
	This may also start a right drag operation."

	self onButtonDown: aMouseEvent.
	^super onRightButtonPressed: aMouseEvent!

onSelChanged
	"Private - Handle a selection change event"

	
			self presenter onSelectionChanged!

onSelChanging: aTreeMapNode cause: aSymbol 
	"Private - Selection is changing in the receiver to the specified <TreeMapNode> as a
	direct result of user input. The user input type is #keyboard or #mouse, as determined 
	by the <Symbol> argument. A #selectionChanging: event is triggered to enquire as to 
	whether the selection change is permissible. If it is not then any observer that wishes to 
	veto the change must set the the value of the <SelectionChangingEvent> to false. Note 
	that selection notifications are not propagated during state restoral."

	^self isStateRestoring or: 
			[| event |
			event := SelectionChangingEvent forSource: self.
			event cause: aSymbol.
			aTreeMapNode notNil ifTrue: [event newSelection: aTreeMapNode object].
			selection notNil ifTrue: [event oldSelection: selection object].
			self presenter onSelectionChanging: event.
			event value]!

onSelectionChanged
	"Sent by #onSelChange when the receiver.
	The default is to trigger an #selectionChanged event of the presenter"

	self presenter trigger: #selectionChanged.
	self invalidateUserInterface.!

onSelectionChanging: aSelectionChangingEvent
	"Handler to indicate that the receiver's selection is about to change.
	To prevent the proposed change under cdertain circumstances a handler
	can set the value of aValueHolder to false.
	The default is to trigger an #selectionChanging: event off the presenter"

	self presenter trigger: #selectionChanging: with: aSelectionChangingEvent!

onTipDetailsRequired: aNMTTDISPINFO 
	aNMTTDISPINFO text: self toolTipText.
	^true!

onViewOpened
	renderer 
		when: #renderSettingsChanged
		send: #settingsChanged
		to: self.
self toolTipWindow: ((Tooltip new)
						create;
						registerViewSubclass: self;
						yourself)
			"isBalloon: true;".
	super onViewOpened!

renderAllOn: aCanvas
	self
		renderTreemapOn: aCanvas;
		renderSelectionOn: aCanvas!

renderer
	^renderer!

renderer: aTreeMapRenderer 
	renderer := aTreeMapRenderer.
!

renderSelectionOn: aCanvas 
	self hasSelection 
		ifTrue: 
			[| selectionDib dibRectangle |
			dibRectangle := 0 @ 0 
						extent: selection layoutRectangle truncated width @ selection layoutRectangle truncated height.
			selectionDib := DIBSection width: dibRectangle width height: dibRectangle height.
			selectionDib canvas fillRectangle: dibRectangle brush: Brush checkerboard.
			aCanvas 
				bitBlt: selectionDib canvas
				rectangle: dibRectangle
				to: selection layoutRectangle truncated topLeft
				rop: 16r8800C6.
			aCanvas invertRectangle: selection layoutRectangle truncated]!

renderStrategy
	^renderer renderStrategy!

renderStrategy: anObject 
renderer renderStrategy: anObject !

renderTreemapOn: canvas 
	self dib drawOn: canvas!

resetSelection
	"Set the receiver to have no selection."

	self basicSelection: nil cause: #unknown!

selectedNode
	"Private - Answer the selected node or nil if no selection."


	^selection!

selection
	"Answer a single selected object. 
	Raise an error if no objects are selected."

	^self selectionIfNone: [self errorNoSelection]
!

selection: newSelection
	"Select the first occurence of the <Object>, newSelection, in the receiver. 
	If not present then signal a NotFoundError.
	Answer anObject"

	^self selection: newSelection ifAbsent: [self errorNotFound: newSelection]!

selection: anObject ifAbsent: exceptionHandler
	"Select the first occurence of anObject in the receiver
	and answer anObject, or if there are no occurrences, the 
	result of evaluating the <niladicValuable>, exceptionHandler."

	| node |
	anObject isNil ifTrue: [^self resetSelection].
	node := self nodeForObject: anObject ifAbsent: [^exceptionHandler value].
	self basicSelection: node cause: #unknown.
	^anObject!

selectionIfNone: aBlock
	"Answer the selected object or the result of evaluating aBlock
	if there is no selection."

	| selNode |
	^(selNode := self selectedNode) isNil
		ifTrue: [aBlock value]
		ifFalse: [selNode object]!

selectionOrNil
	"Return the selected object or nil if no selection."

	^self selectionIfNone: []!

selectionOrNil: newSelection
	"Select the first occurence of the <Object>, newSelection, in the
	receiver. If newSelection is nil, then simply remove the existing
	selection. If newSelection is not present then signal a NotFoundError.
	Answer newSelection."

	newSelection isNil 
		ifTrue: [self resetSelection]
		ifFalse: [self selection: newSelection].
	^newSelection!

settingsChanged
	needsUpdate := true.
self hideTooltip.
	self invalidate!

toolTipText
	^treeMapModel notNil 
		ifTrue: 
			[| node |
			node := treeMapModel leafNodeAt: self cursorPosition.
			node notNil 
				ifTrue: 
					[tooltipRectangle := node layoutRectangle.
					getTextBlock value: node object]
				ifFalse: 
					[tooltipRectangle := nil.
					'']]
		ifFalse: ['']!

updateCachedBitmap
	| object |
	needsUpdate := false.
	selection notNil ifTrue: [object := selection object].
	selection := treeMapModel := nil.
	renderer
				setModel: self model;
				setLayoutRectangle: self clientRectangle;
				setBitmap: self dib;
				render.
	treeMapModel := renderer treeMapModel.
	object notNil ifTrue: [self selection: object].
	self invalidate! !
!TreeMapView categoriesFor: #autoResizeColumns!*-in class package!public! !
!TreeMapView categoriesFor: #basicSelection:cause:!*-in class package!private!selection! !
!TreeMapView categoriesFor: #connectModel!*-in class package!public! !
!TreeMapView categoriesFor: #createDib!*-in class package!initialize/release!public! !
!TreeMapView categoriesFor: #defaultBackColor!*-in class package!public! !
!TreeMapView categoriesFor: #defaultFont!*-in class package!public! !
!TreeMapView categoriesFor: #defaultGetImageBlock!*-in class package!adapters!constants!private! !
!TreeMapView categoriesFor: #defaultGetTextBlock!*-in class package!adapters!constants!private! !
!TreeMapView categoriesFor: #dib!*-in class package!initialize/release!public! !
!TreeMapView categoriesFor: #errorNoSelection!*-in class package!exceptions!private! !
!TreeMapView categoriesFor: #findNodeForObject:!*-in class package!private!searching! !
!TreeMapView categoriesFor: #getImageBlock!*-in class package!public! !
!TreeMapView categoriesFor: #getImageBlock:!*-in class package!public! !
!TreeMapView categoriesFor: #getTextBlock!*-in class package!public! !
!TreeMapView categoriesFor: #getTextBlock:!*-in class package!public! !
!TreeMapView categoriesFor: #hasSelection!*-in class package!public!testing! !
!TreeMapView categoriesFor: #hideTooltip!*-in class package!event handling!private! !
!TreeMapView categoriesFor: #hitTest:!*-in class package!helpers!private! !
!TreeMapView categoriesFor: #initialize!*-in class package!initialize/release!public! !
!TreeMapView categoriesFor: #nodeForObject:ifAbsent:!*-in class package!private!searching! !
!TreeMapView categoriesFor: #onButtonDown:!*-in class package!event handling!private! !
!TreeMapView categoriesFor: #onLeftButtonDoubleClicked:!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onLeftButtonPressed:!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onModelChanged!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onMouseMoved:!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onPaintRequired:!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onPositionChanged:!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onRightButtonPressed:!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onSelChanged!*-in class package!event handling!private! !
!TreeMapView categoriesFor: #onSelChanging:cause:!*-in class package!event handling!private! !
!TreeMapView categoriesFor: #onSelectionChanged!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onSelectionChanging:!*-in class package!event handling!public! !
!TreeMapView categoriesFor: #onTipDetailsRequired:!*-in class package!public! !
!TreeMapView categoriesFor: #onViewOpened!*-in class package!initialize/release!public! !
!TreeMapView categoriesFor: #renderAllOn:!*-in class package!public!rendering! !
!TreeMapView categoriesFor: #renderer!*-in class package!accessing!private! !
!TreeMapView categoriesFor: #renderer:!*-in class package!accessing!private! !
!TreeMapView categoriesFor: #renderSelectionOn:!*-in class package!public! !
!TreeMapView categoriesFor: #renderStrategy!*-in class package!accessing!public! !
!TreeMapView categoriesFor: #renderStrategy:!*-in class package!accessing!public! !
!TreeMapView categoriesFor: #renderTreemapOn:!*-in class package!public!rendering! !
!TreeMapView categoriesFor: #resetSelection!*-in class package!public!selection! !
!TreeMapView categoriesFor: #selectedNode!*-in class package!private!selection! !
!TreeMapView categoriesFor: #selection!*-in class package!public!selection! !
!TreeMapView categoriesFor: #selection:!*-in class package!public!selection! !
!TreeMapView categoriesFor: #selection:ifAbsent:!*-in class package!public!selection! !
!TreeMapView categoriesFor: #selectionIfNone:!*-in class package!public!selection! !
!TreeMapView categoriesFor: #selectionOrNil!*-in class package!public!selection! !
!TreeMapView categoriesFor: #selectionOrNil:!*-in class package!public!selection! !
!TreeMapView categoriesFor: #settingsChanged!*-in class package!accessing!public! !
!TreeMapView categoriesFor: #toolTipText!*-in class package!public! !
!TreeMapView categoriesFor: #updateCachedBitmap!*-in class package!public!updating! !

!TreeMapView class methodsFor!

defaultModel
	^TreeModel new! !
!TreeMapView class categoriesFor: #defaultModel!*-in class package!public! !

AsynchronousTreeMapView guid: (GUID fromString: '{A853B97E-2FF4-43E2-B63C-DB10E8003DA1}')!
AsynchronousTreeMapView comment: ''!
!AsynchronousTreeMapView categoriesForClass!Unclassified! !
!AsynchronousTreeMapView methodsFor!

defaultRenderText
^'Rendering TreeMap'!

onPaintRequired: aPaintEvent 
	| canvas |
	canvas := aPaintEvent canvas.
	needsUpdate 
		ifTrue: 
			[self startRenderProccess].
	(renderProcess notNil and: [renderProcess isTerminated]) 
		ifTrue: [self renderAllOn: canvas]
		ifFalse: [self renderInProgressOn: canvas]!

onViewClosed
	renderProcess notNil 
				ifTrue: [renderProcess isTerminated ifFalse: [renderProcess terminate]. ].super onViewClosed

	!

renderInProgressOn: canvas 
	canvas
		brush: (Brush color: self backcolor);
		fillRectangle: self clientRectangle;
		font: self font;
		setTextAlign: TA_CENTER | TA_BASELINE;
		setBkMode: TRANSPARENT;
		setTextColor: self backcolor negative;
		text: self text at: self clientRectangle center!

startRenderProccess
renderProcess notNil 
				ifTrue: [renderProcess isTerminated ifFalse: [renderProcess terminate]].
			renderProcess := [self updateCachedBitmap] forkAt: Processor systemBackgroundPriority.
			renderProcess name: 'TreeMap Renderer'!

text
renderText isNil ifTrue: [ renderText := self defaultRenderText].^renderText!

text: aString 
	renderText := aString! !
!AsynchronousTreeMapView categoriesFor: #defaultRenderText!*-in class package!public!rendering! !
!AsynchronousTreeMapView categoriesFor: #onPaintRequired:!*-in class package!event handling!public! !
!AsynchronousTreeMapView categoriesFor: #onViewClosed!*-in class package!event handling!public! !
!AsynchronousTreeMapView categoriesFor: #renderInProgressOn:!*-in class package!public!rendering! !
!AsynchronousTreeMapView categoriesFor: #startRenderProccess!*-in class package!event handling!public! !
!AsynchronousTreeMapView categoriesFor: #text!*-in class package!public!rendering! !
!AsynchronousTreeMapView categoriesFor: #text:!*-in class package!public!rendering! !

"Binary Globals"!

