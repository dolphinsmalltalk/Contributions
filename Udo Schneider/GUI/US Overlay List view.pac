| package |
package := Package name: 'US Overlay List view'.
package paxVersion: 1;
	basicComment: '$id: US Overlay List view 0.012$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Usage
	See OverlayListView class>>example1
	

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.012'.

package basicScriptAt: #preinstall put: 'ListViewConstants
	at: ''LVTVIF_AUTOSIZE'' put: 16r00000000;
	at: ''LVTVIF_FIXEDWIDTH'' put: 16r00000001;
	at: ''LVTVIF_FIXEDHEIGHT'' put: 16r00000002;
	at: ''LVTVIF_FIXEDSIZE'' put: 16r00000003;
	at: ''LVTVIF_EXTENDED'' put: 16r00000004;
	at: ''LVTVIM_TILESIZE'' put: 16r00000001;
	at: ''LVTVIM_COLUMNS'' put: 16r00000002;
	at: ''LVTVIM_LABELMARGIN'' put: 16r00000004;
	at: ''LVM_SETTILEVIEWINFO'' put: 16r1000 + 162;
	at: ''LVM_SETVIEW'' put: 16r1000 + 142;
	at: ''LV_VIEW_TILE'' put: 4'.

package classNames
	add: #LVTILEVIEWINFO;
	add: #OverlayImageManager;
	add: #OverlayListView;
	add: #OverlayListViewColumn;
	yourself.

package methodNames
	add: #CommCtrlLibrary -> #imageListSetOverlayImage:iImage:iOverlay:;
	add: #LVBKIMAGE -> #beTiled;
	add: #LVITEM -> #overlay;
	add: #LVITEM -> #overlay:;
	add: #LVITEM -> #row:text:imageIndex:overlayIndex:indent:;
	add: #WinImageList -> #printOn:;
	add: #WinImageList -> #use:asOverlay:;
	add: 'ListPresenter class' -> #resource_Overlay_list_view;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Win32Structure subclass: #LVTILEVIEWINFO
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ImageManager subclass: #OverlayImageManager
	instanceVariableNames: 'overlayImages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ListViewColumn subclass: #OverlayListViewColumn
	instanceVariableNames: 'getOverlayImageBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ListView subclass: #OverlayListView
	instanceVariableNames: 'getOverlayBlock overlayImageMapping'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!CommCtrlLibrary methodsFor!

imageListSetOverlayImage: hImageList iImage: iImage iOverlay: iOverlay 
	"Adds a specified image to the list of images to be used as overlay masks.
	An image list can have up to four overlay masks in version 4.70 and earlier
	and up to 15 in version 4.71. The function assigns an overlay mask index
	to the specified image.
	
	Syntax
	
	BOOL ImageList_SetOverlayImage(
		HIMAGELIST himl,
		int iImage,
		int iOverlay
	);"

	<stdcall: bool ImageList_SetOverlayImage handle sdword sdword>
	^self invalidCall! !
!CommCtrlLibrary categoriesFor: #imageListSetOverlayImage:iImage:iOverlay:!primitives!public!win32 functions-image list! !

!ListPresenter class methodsFor!

resource_Overlay_list_view
	"Answer the literal data from which the 'Overlay list view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Overlay_list_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.OverlayListView)  98 32 0 0 98 2 8 1140920397 1025 416 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 416 0 8 4294902339 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 382320355 8 #[30 105 226 0 106] 8 #displayString 704 7 257 0 0 1246534 1 ##(Smalltalk.OverlayImageManager)  234 240 544 234 240 544 0 0 202 208 544 0 0 0 0 0 0 202 208 98 1 1379654 5 ##(Smalltalk.OverlayListViewColumn)  8 'Column 1' 201 8 #left 459270 ##(Smalltalk.Message)  800 544 994 8 #<= 544 0 0 416 0 3 0 0 0 8 #report 544 0 2145 0 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 1202 401 701 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 200 0 0 0 94 1 0 0] 98 0 1202 193 193 0 27 )! !
!ListPresenter class categoriesFor: #resource_Overlay_list_view!public!resources-views! !

!LVBKIMAGE methodsFor!

beTiled
	self ulFlags: (self ulFlags bitOr: LVBKIF_STYLE_TILE)! !
!LVBKIMAGE categoriesFor: #beTiled!accessing!public! !

!LVITEM methodsFor!

overlay
	^(self dwState bitAnd: LVIS_OVERLAYMASK) >> 8!

overlay: anInteger 
	self
		maskIn: self stateValidMask;
		stateMask: (self stateMask bitOr: LVIS_OVERLAYMASK);
		dwState: anInteger << 8!

row: rowInteger text: aString imageIndex: imageInteger overlayIndex: overlayInteger indent: indentInteger 
	| mask iImage |
	self
		iItem: rowInteger - 1;
		pszText: (text := aString).
	mask := LVIF_TEXT.
	imageInteger notNil ifTrue: [iImage := imageInteger - 1].
	indentInteger notNil 
		ifTrue: 
			[self iIndent: indentInteger.
			mask := mask bitOr: LVIF_INDENT.
			indentInteger < 0 ifTrue: [iImage := -1]].
	iImage notNil 
		ifTrue: 
			[self iImage: iImage.
			mask := mask bitOr: LVIF_IMAGE].
	overlayInteger notNil 
		ifTrue: 
			[self
				stateMask: (self stateMask bitOr: LVIS_OVERLAYMASK);
				dwState: overlayInteger << 8.
			mask := mask bitOr: LVIF_STATE].
	self mask: mask


! !
!LVITEM categoriesFor: #overlay!public! !
!LVITEM categoriesFor: #overlay:!public! !
!LVITEM categoriesFor: #row:text:imageIndex:overlayIndex:indent:!accessing!private! !

!WinImageList methodsFor!

printOn: target
super printOn: target.
target nextPutAll: ' (' , self extent displayString , ')'!

use: imageIndex asOverlay: overlayIndex 
	^CommCtrlLibrary default 
		imageListSetOverlayImage: self asParameter
		iImage: imageIndex - 1
		iOverlay: overlayIndex! !
!WinImageList categoriesFor: #printOn:!public! !
!WinImageList categoriesFor: #use:asOverlay:!adding!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

LVTILEVIEWINFO guid: (GUID fromString: '{DFBB203D-1891-4E18-9B9D-1F8F94C6F941}')!
LVTILEVIEWINFO comment: ''!
!LVTILEVIEWINFO categoriesForClass!Unclassified! !
!LVTILEVIEWINFO methodsFor!

cbSize
	"Answer the receiver's cbSize field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

cbSize: anObject
	"Set the receiver's cbSize field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

cLines
	"Answer the receiver's cLines field as a Smalltalk object."

	^(bytes sdwordAtOffset: 20)!

cLines: anObject
	"Set the receiver's cLines field to the value of anObject."

	bytes sdwordAtOffset: 20 put: anObject!

dwFlags
	"Answer the receiver's dwFlags field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

dwFlags: anObject
	"Set the receiver's dwFlags field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

dwMask
	"Answer the receiver's dwMask field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

dwMask: anObject
	"Set the receiver's dwMask field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

rcLabelMargin
	"Answer the receiver's rcLabelMargin field as a Smalltalk object."

	^RECT fromAddress: (bytes yourAddress + 24)!

rcLabelMargin: anObject
	"Set the receiver's rcLabelMargin field to the value of anObject."

	anObject replaceBytesOf: bytes from: 25 to: 40 startingAt: 1!

sizeTile
	"Answer the receiver's sizeTile field as a Smalltalk object."

	^SIZE fromAddress: (bytes yourAddress + 12)!

sizeTile: anObject
	"Set the receiver's sizeTile field to the value of anObject."

	anObject replaceBytesOf: bytes from: 13 to: 20 startingAt: 1! !
!LVTILEVIEWINFO categoriesFor: #cbSize!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #cbSize:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #cLines!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #cLines:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #dwFlags!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #dwFlags:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #dwMask!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #dwMask:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #rcLabelMargin!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #rcLabelMargin:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #sizeTile!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #sizeTile:!**compiled accessors**!public! !

!LVTILEVIEWINFO class methodsFor!

defineFields
	"Define the fields of the Win32 LVTILEVIEWINFO structure
		self compileDefinition
		
		typedef struct LVTILEVIEWINFO {
    UINT cbSize;
    DWORD dwMask;
    DWORD dwFlags;
    SIZE sizeTile;
    int cLines;
    RECT rcLabelMargin;
} LVTILEVIEWINFO, *PLVTILEVIEWINFO;
	"

	self
		defineField: #cbSize type: DWORDField new;
		defineField: #dwMask type: DWORDField new;
		defineField: #dwFlags type: DWORDField new;
		defineField: #sizeTile type: (StructureField type: SIZE);
		defineField: #cLines type: SDWORDField new;
		defineField: #rcLabelMargin type: (StructureField type: RECT)! !
!LVTILEVIEWINFO class categoriesFor: #defineFields!initializing!public! !

OverlayImageManager guid: (GUID fromString: '{C0C00A3A-D476-4D94-B2E5-77DBF8F1384F}')!
OverlayImageManager comment: ''!
!OverlayImageManager categoriesForClass!Unclassified! !
!OverlayImageManager methodsFor!

addOverlayImage: anImage 
	(overlayImages includes: anImage) ifFalse: [self basicAddOverlayImage: anImage].
	^overlayImages indexOf: anImage!

basicAddOverlayImage: anImage 
	| imageIndex overlayIndex |
	imageIndex := self indexOfImage: anImage.
	overlayImages addLast: anImage.
	overlayIndex := overlayImages indexOf: anImage.
	imageLists do: [:hImageList | hImageList use: imageIndex asOverlay: overlayIndex]!

buildImageListWithExtent: aPoint 
	| newList |
	newList := super buildImageListWithExtent: aPoint.
	overlayImages 
		keysAndValuesDo: [:overlayIndex :overlayImage | newList use: (self indexOfImage: overlayImage) asOverlay: overlayIndex].
	^newList!

indexOfOverlayImage: anImage 
^anImage isNil ifTrue: [0] ifFalse: [self addOverlayImage: anImage]
	!

initialize
	overlayImages := OrderedCollection new.
	^super initialize!

newImageListWithExtent: aPoint 
	"Private - Answer a new <WinImageList> appropriately configured to hold images for the receiver
	of the specified extent."

	| answer |
	(aPoint x <= 0 or: [aPoint y <= 0]) ifTrue: [^self error: 'Image list extent must be at least 1@1'].
	answer := WinImageList 
				newExtent: aPoint
				initialSize: images size
				masked: true.
	answer backcolor: backcolor.
	^answer!

purgeImages
	"Private - Frees up all the image resources"


	self newImageLists! !
!OverlayImageManager categoriesFor: #addOverlayImage:!adding!public! !
!OverlayImageManager categoriesFor: #basicAddOverlayImage:!adding!private! !
!OverlayImageManager categoriesFor: #buildImageListWithExtent:!helpers!private! !
!OverlayImageManager categoriesFor: #indexOfOverlayImage:!public!searching! !
!OverlayImageManager categoriesFor: #initialize!initializing!private! !
!OverlayImageManager categoriesFor: #newImageListWithExtent:!helpers!private! !
!OverlayImageManager categoriesFor: #purgeImages!operations!private! !

OverlayListViewColumn guid: (GUID fromString: '{2CF01934-D4D0-494E-BF13-26638DBAC664}')!
OverlayListViewColumn comment: ''!
!OverlayListViewColumn categoriesForClass!Unclassified! !
!OverlayListViewColumn methodsFor!

defaultGetImageBlock
	"Private - Answer a monadic valuable to use to find out the image index of an object 
	that will be placed in the receiver."

	^nil!

getOverlayBlock
	^getOverlayImageBlock!

getOverlayBlock: anObject 
	getOverlayImageBlock := anObject!

initialize
	"Private - Initialize the receiver"

	super initialize.
	getOverlayImageBlock := nil.
!

overlayFromRow: item 


	^getOverlayImageBlock isNil ifFalse: [getOverlayImageBlock value: (self contentFromRow: item)]! !
!OverlayListViewColumn categoriesFor: #defaultGetImageBlock!adapters!constants!private! !
!OverlayListViewColumn categoriesFor: #getOverlayBlock!accessing!public! !
!OverlayListViewColumn categoriesFor: #getOverlayBlock:!accessing!public! !
!OverlayListViewColumn categoriesFor: #initialize!initializing!private! !
!OverlayListViewColumn categoriesFor: #overlayFromRow:!private! !

!OverlayListViewColumn class methodsFor!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect block: #getOverlayBlock);
		yourself! !
!OverlayListViewColumn class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !

OverlayListView guid: (GUID fromString: '{E8AD61D4-7107-4A85-B6A2-9997D54D5B7A}')!
OverlayListView comment: ''!
!OverlayListView categoriesForClass!Unclassified! !
!OverlayListView methodsFor!

addOrUpdate: aBoolean nonVirtualItems: aSequenceableCollection afterIndex: anInteger 
	"Private - Add/update row and column items for specified sequence of objects to the ListView,
	starting at the specified row index."

	| lvItem primaryImageSupplier primaryTextSupplier isMultiColumn colImageBlock colOverlayBlock msg |
	primaryImageSupplier := primaryTextSupplier := self.
	colImageBlock := [:eachRow :eachCol | nil].
	(isMultiColumn := self isReportMode) 
		ifTrue: 
			[primaryTextSupplier := self allColumns at: 1 ifAbsent: [self].
			self hasColumnImages 
				ifTrue: 
					[colImageBlock := [:eachRow :eachCol | eachCol imageFromRow: eachRow].
					colOverlayBlock := [:eachRow :eachCol | eachCol overlayFromRow: eachRow].
					primaryImageSupplier := primaryTextSupplier]].
	lvItem := LVITEM new.
	msg := aBoolean ifTrue: [LVM_SETITEM] ifFalse: [LVM_INSERTITEM].
	aSequenceableCollection keysAndValuesDo: 
			[:i :each | 
			lvItem 
				row: i + anInteger
				text: (primaryTextSupplier textFromRow: each)
				imageIndex: (self imageManager indexOfImage: (primaryImageSupplier imageFromRow: each))
				overlayIndex: (self imageManager indexOfOverlayImage: (primaryImageSupplier overlayFromRow: each))
				indent: (self indentFromRow: each).
			self 
				sendMessage: msg
				wParam: 0
				lpParam: lvItem].
	isMultiColumn ifFalse: [^self].
	self allColumns from: 2
		keysAndValuesDo: 
			[:j :eachCol | 
			lvItem column: j.
			aSequenceableCollection keysAndValuesDo: 
					[:i :eachRow | 
					lvItem 
						row: i + anInteger
						text: (eachCol textFromRow: eachRow)
						imageIndex: (self imageManager indexOfImage: (colImageBlock value: eachRow value: eachCol))
						overlayIndex: (self imageManager indexOfOverlayImage: (colOverlayBlock value: eachRow value: eachCol))
						indent: nil.
					self lvmSetItem: lvItem]]!

defaultGetImageBlock
	"Private - Answer a monadic valuable to use to find out the image index of an object 
	that will be placed in the receiver."

	^nil!

getOverlayBlock
	^getOverlayBlock!

getOverlayBlock: anObject 
	getOverlayBlock := anObject.
	self updateAll!

initialize
	"Private - Initialize the receiver"

	super initialize.
	getOverlayBlock := nil.
	imageManager := OverlayImageManager new!

onDisplayDetailsRequired: lvitem 
	"Private - Get the display info for the receiver's row identified by the <LVITEM>, lvitem."

	"N.B. This is a callback request from the ListView so setting a breakpoint in here may bring
	your image to its knees."

	"Implementation Note: If in report mode then the task of supplying the text/images is
	delegated to the particular column, otherwise the valuables local to the receiver are used.
	This may seem inconsistent, but it allows different text/images to be displayed for the
	primary column if the application requires that the view be dynamically switchable between
	#report mode and the other modes."

	| rowObject mask column columnIdx overlayIdx |
	rowObject := self objectFromHandle: lvitem handle ifAbsent: [].
	"List sometimes asks for lvitem we no longer have, answer nil to accept default processing"
	rowObject isNil ifTrue: [^nil].
	self isReportMode 
		ifTrue: 
			[columnIdx := lvitem iSubItem + 1.
			column := self columnAtIndex: columnIdx].
	mask := lvitem mask.

	"Image Request?"
	(mask allMask: LVIF_IMAGE) 
		ifTrue: 
			[| primaryImageSupplier imgIdx |
			primaryImageSupplier := (column notNil and: [self hasColumnImages]) ifTrue: [column] ifFalse: [self].
			(primaryImageSupplier imageFromRow: rowObject) 
				ifNotNil: 
					[:img | 
					imgIdx := self imageManager indexOfImage: img.
					imgIdx notNil ifTrue: [lvitem image: imgIdx - 1]].
			(primaryImageSupplier overlayFromRow: rowObject) 
				ifNotNil: 
					[:overlay | 
					overlayIdx := self imageManager indexOfOverlayImage: overlay.
					overlayIdx notNil ifTrue: [lvitem overlay: overlayIdx]]].

	"Text request?"
	(mask allMask: LVIF_TEXT) 
		ifTrue: 
			["If in report mode the column's get text block is used unless the request
			 is for the primary column and its text block is nil, in which case the view
			 level block is used"
			lvitem 
				textInBuffer: (((column notNil and: [columnIdx > 1 or: [column getTextBlock notNil]]) 
						ifTrue: [column]
						ifFalse: [self]) textFromRow: rowObject)].
	(mask allMask: LVIF_INDENT) 
		ifTrue: 
			["Indenting is only supported for the whole row, not on a per-column basis"
			lvitem indent: (self indentFromRow: rowObject)].
	^0	"suppress default processing"!

overlayFromRow: item 
	^getOverlayBlock isNil ifFalse: [getOverlayBlock value: item]! !
!OverlayListView categoriesFor: #addOrUpdate:nonVirtualItems:afterIndex:!private!updating! !
!OverlayListView categoriesFor: #defaultGetImageBlock!adapters!constants!private! !
!OverlayListView categoriesFor: #getOverlayBlock!accessing!public! !
!OverlayListView categoriesFor: #getOverlayBlock:!accessing!public! !
!OverlayListView categoriesFor: #initialize!initializing!private! !
!OverlayListView categoriesFor: #onDisplayDetailsRequired:!event handling!private! !
!OverlayListView categoriesFor: #overlayFromRow:!private! !

!OverlayListView class methodsFor!

columnClass
	"Answer the class of object used to represent the columns of the receiver."

	^OverlayListViewColumn!

example1
	| listModel presenter view |
	"
		self example1
	"
	listModel := ListModel on: (1 to: 100).
	presenter := ListPresenter show: 'Overlay list view' on: listModel.
	view := presenter view.
	view viewMode: #tileIcons.
	"You should now see a ListPresenter showing all number between 1-100 w/o any icons"
	view getImageBlock: [:each | each icon].
	"Now each entry has a number icon.
		IMPORTANT: Please note that OverlayListPresenters do expect the getImageBlock to return the Icon itself - not the imageIndex of it!!!!!!"
	view getOverlayBlock: [:each | each even icon]
	"Now all number will have an overlay icon depending on whether they are even or not.
		IMPORTANT: Here as well you have to provide the icon itself - not it's imageIndex"!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect block: #getOverlayBlock);
		yourself! !
!OverlayListView class categoriesFor: #columnClass!constants!private! !
!OverlayListView class categoriesFor: #example1!*-in class package!must strip!public! !
!OverlayListView class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !

"Binary Globals"!

