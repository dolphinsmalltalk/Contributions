| package |
package := Package name: 'OwnerDrawControls'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicScriptAt: #postinstall put: 'OwnerDrawListBox addMessageMapEntries'.
package basicScriptAt: #postuninstall put: '"Remove constants from Win32Constants that were added by the preinstall script"
Win32Constants
	removeKey: ''ODS_SELECTED'';
	removeKey: ''WM_MEASUREITEM'';
	removeKey: ''WM_DELETEITEM'';
	removeKey: ''WM_COMPAREITEM'';
	removeKey: ''LB_SETITEMHEIGHT'';
	removeKey: ''LB_GETITEMHEIGHT'';
	removeKey: ''LVS_OWNERDRAWFIXED'';
	removeKey: ''ODS_SELECTED'';
	removeKey: ''ODS_FOCUS'';
	removeKey: ''ODS_DEFAULT'''.
package basicScriptAt: #preinstall put: '"Add appropriate constants to Win32Constants"
Win32Constants
	at: ''ODS_SELECTED''		put: 1;
	at: ''WM_MEASUREITEM''	put: 16r2C;
	at: ''WM_DELETEITEM''		put: 16r2D;
	at: ''WM_COMPAREITEM''	put: 16r39;
	at: ''LB_SETITEMHEIGHT''	put: 16r1A0;
	at: ''LB_GETITEMHEIGHT''	put: 16r1A1;
	at: ''LVS_OWNERDRAWFIXED'' put: 16r400;
	at: ''ODS_SELECTED''		put: 16r1;
	at: ''ODS_FOCUS''			put: 16r10;
	at: ''ODS_DEFAULT''		put: 16r20'.
package basicScriptAt: #preuninstall put: 'OwnerDrawListBox removeMessageMapEntries'.

package classNames
	add: #COMPAREITEMSTRUCT;
	add: #DELETEITEMSTRUCT;
	add: #MEASUREITEMSTRUCT;
	add: #OwnerDrawListBox;
	yourself.

package methodNames
	add: #View -> #wmCompareItem:wParam:lParam:;
	add: #View -> #wmDeleteItem:wParam:lParam:;
	add: #View -> #wmMeasureItem:wParam:lParam:;
	yourself.

package resourceNames
	add: #ListPresenter -> 'Owner-draw list box';
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #ListPresenter -> 'Owner-draw list box';
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Win32Structure subclass: #COMPAREITEMSTRUCT
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Win32Structure subclass: #DELETEITEMSTRUCT
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Win32Structure subclass: #MEASUREITEMSTRUCT
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ListBox subclass: #OwnerDrawListBox
	instanceVariableNames: 'measureItemBlock drawItemBlock compareItemBlock deleteItemBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!View methodsFor!

wmCompareItem: message wParam: wParam lParam: lParam
	"Private - Default handler for a WM_COMPAREITEM."

	| destinationWindow |
	destinationWindow := self getItem: wParam ifAbsent: [ nil ].
	destinationWindow notNil
		ifTrue: [^destinationWindow wmCompareItem: message wParam: wParam lParam: lParam]
		ifFalse: [^0]

!

wmDeleteItem: message wParam: wParam lParam: lParam
	"Private - Default handler for a WM_DELETEITEM."

	| destinationWindow |
	destinationWindow := self getItem: wParam ifAbsent: [ nil ].
	^destinationWindow notNil
		ifTrue: [destinationWindow wmDeleteItem: message wParam: wParam lParam: lParam]
		ifFalse: ["accept the default processing"]

!

wmMeasureItem: message wParam: wParam lParam: lParam
	"Private - Default handler for a WM_MEASUREITEM."

	| destinationWindow |
	destinationWindow := self getItem: wParam ifAbsent: [ nil ].
	^destinationWindow notNil
		ifTrue: [destinationWindow wmMeasureItem: message wParam: wParam lParam: lParam]
		ifFalse: ["accept the default processing"]

! !
!View categoriesFor: #wmCompareItem:wParam:lParam:!event handling-win32!must not strip!private! !
!View categoriesFor: #wmDeleteItem:wParam:lParam:!event handling-win32!must not strip!private! !
!View categoriesFor: #wmMeasureItem:wParam:lParam:!event handling-win32!must not strip!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

COMPAREITEMSTRUCT guid: (GUID fromString: '{0A1AD990-BCCC-497F-9EB3-BCF43C201AA3}')!
COMPAREITEMSTRUCT comment: ''!
!COMPAREITEMSTRUCT categoriesForClass!Unclassified! !
!COMPAREITEMSTRUCT methodsFor!

ctlID
	"Answer the receiver's ctlID field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

ctlType
	"Answer the receiver's ctlType field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

dwLocaleId
	"Answer the receiver's dwLocaleId field as a Smalltalk object."

	^(bytes dwordAtOffset: 28)!

hwndItem
	"Answer the receiver's hwndItem field as a Smalltalk object."

	^(bytes dwordAtOffset: 8) asExternalHandle!

itemData1
	"Answer the receiver's itemData1 field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

itemData2
	"Answer the receiver's itemData2 field as a Smalltalk object."

	^(bytes dwordAtOffset: 24)!

itemID1
	"Answer the receiver's itemID1 field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

itemID2
	"Answer the receiver's itemID2 field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)! !
!COMPAREITEMSTRUCT categoriesFor: #ctlID!**compiled accessors**!public! !
!COMPAREITEMSTRUCT categoriesFor: #ctlType!**compiled accessors**!public! !
!COMPAREITEMSTRUCT categoriesFor: #dwLocaleId!**compiled accessors**!public! !
!COMPAREITEMSTRUCT categoriesFor: #hwndItem!**compiled accessors**!public! !
!COMPAREITEMSTRUCT categoriesFor: #itemData1!**compiled accessors**!public! !
!COMPAREITEMSTRUCT categoriesFor: #itemData2!**compiled accessors**!public! !
!COMPAREITEMSTRUCT categoriesFor: #itemID1!**compiled accessors**!public! !
!COMPAREITEMSTRUCT categoriesFor: #itemID2!**compiled accessors**!public! !

!COMPAREITEMSTRUCT class methodsFor!

defineFields
	"Define the fields of the Win32 COMPAREITEMSTRUCT structure.

		COMPAREITEMSTRUCT compileDefinition

	typedef struct tagCOMPAREITEMSTRUCT   // dis 
		UINT  CtlType; 
		UINT  CtlID; 
		HWND hwndItem;
		UINT  itemID1; 
		DWORD itemData1;
		UINT itemID2;
		DWORD itemData2;
		DWORD dwLocaleId;
	 COMPAREITEMSTRUCT; "

	self 
		defineField: #ctlType type: DWORDField readOnly;
		defineField: #ctlID type: DWORDField readOnly;
		defineField: #hwndItem type: HANDLEField readOnly;
		defineField: #itemID1 type: DWORDField readOnly;
		defineField: #itemData1 type: DWORDField readOnly;
		defineField: #itemID2 type: DWORDField readOnly;
		defineField: #itemData2 type: DWORDField readOnly;
		defineField: #dwLocaleId type: DWORDField readOnly
		! !
!COMPAREITEMSTRUCT class categoriesFor: #defineFields!development!public! !

DELETEITEMSTRUCT guid: (GUID fromString: '{5AA43890-3A73-4B7A-B2D6-8F98BC6B5E31}')!
DELETEITEMSTRUCT comment: ''!
!DELETEITEMSTRUCT categoriesForClass!Unclassified! !
!DELETEITEMSTRUCT methodsFor!

ctlID
	"Answer the receiver's ctlID field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

ctlType
	"Answer the receiver's ctlType field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

hwndItem
	"Answer the receiver's hwndItem field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalHandle!

itemAction
	"Answer the receiver's itemAction field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

itemData
	"Answer the receiver's itemData field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)!

itemID
	"Answer the receiver's itemID field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)! !
!DELETEITEMSTRUCT categoriesFor: #ctlID!**compiled accessors**!public! !
!DELETEITEMSTRUCT categoriesFor: #ctlType!**compiled accessors**!public! !
!DELETEITEMSTRUCT categoriesFor: #hwndItem!**compiled accessors**!public! !
!DELETEITEMSTRUCT categoriesFor: #itemAction!**compiled accessors**!public! !
!DELETEITEMSTRUCT categoriesFor: #itemData!**compiled accessors**!public! !
!DELETEITEMSTRUCT categoriesFor: #itemID!**compiled accessors**!public! !

!DELETEITEMSTRUCT class methodsFor!

defineFields
	"Define the fields of the Win32 DELETEITEMSTRUCT structure.

		DELETEITEMSTRUCT compileDefinition

	typedef struct tagDELETEITEMSTRUCT 
		UINT  CtlType; 
		UINT  CtlID; 
		UINT  itemID; 
		UINT  itemAction; 
		HWND  hwndItem; 
		DWORD itemData;
	 DELETEITEMSTRUCT; "

	self 
		defineField: #ctlType type: DWORDField readOnly;
		defineField: #ctlID type: DWORDField readOnly;
		defineField: #itemID type: DWORDField readOnly;
		defineField: #itemAction type: DWORDField readOnly;
		defineField: #hwndItem type: HANDLEField readOnly;
		defineField: #itemData type: DWORDField readOnly! !
!DELETEITEMSTRUCT class categoriesFor: #defineFields!development!public! !

MEASUREITEMSTRUCT guid: (GUID fromString: '{E39A8166-1280-4382-93AE-5D1F93B7F94F}')!
MEASUREITEMSTRUCT comment: ''!
!MEASUREITEMSTRUCT categoriesForClass!Unclassified! !
!MEASUREITEMSTRUCT methodsFor!

ctlID
	"Answer the receiver's ctlID field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

ctlType
	"Answer the receiver's ctlType field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

itemData
	"Answer the receiver's itemData field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)!

itemHeight
	"Answer the receiver's itemHeight field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

itemHeight: anObject
	"Set the receiver's itemHeight field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

itemID
	"Answer the receiver's itemID field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

itemWidth
	"Answer the receiver's itemWidth field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

itemWidth: anObject
	"Set the receiver's itemWidth field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject! !
!MEASUREITEMSTRUCT categoriesFor: #ctlID!**compiled accessors**!public! !
!MEASUREITEMSTRUCT categoriesFor: #ctlType!**compiled accessors**!public! !
!MEASUREITEMSTRUCT categoriesFor: #itemData!**compiled accessors**!public! !
!MEASUREITEMSTRUCT categoriesFor: #itemHeight!**compiled accessors**!public! !
!MEASUREITEMSTRUCT categoriesFor: #itemHeight:!**compiled accessors**!public! !
!MEASUREITEMSTRUCT categoriesFor: #itemID!**compiled accessors**!public! !
!MEASUREITEMSTRUCT categoriesFor: #itemWidth!**compiled accessors**!public! !
!MEASUREITEMSTRUCT categoriesFor: #itemWidth:!**compiled accessors**!public! !

!MEASUREITEMSTRUCT class methodsFor!

defineFields
	"Define the fields of the Win32 MEASUREITEMSTRUCT structure.

		MEASUREITEMSTRUCT compileDefinition

	typedef struct tagMEASUREITEMSTRUCT
		UINT  CtlType; 
		UINT  CtlID; 
		UINT  itemID; 
		UINT  itemWidth; 
		UINT  itemHeight; 
		DWORD itemData;
	 MEASUREITEMSTRUCT; "

	self 
		defineField: #ctlType type: DWORDField readOnly;
		defineField: #ctlID type: DWORDField readOnly;
		defineField: #itemID type: DWORDField readOnly;
		defineField: #itemWidth type: DWORDField new;
		defineField: #itemHeight type: DWORDField new;
		defineField: #itemData type: DWORDField readOnly! !
!MEASUREITEMSTRUCT class categoriesFor: #defineFields!development!public! !

OwnerDrawListBox guid: (GUID fromString: '{8300DA53-690A-49DD-81F3-5CAC01F127AE}')!
OwnerDrawListBox comment: 'Implements an owner-draw listbox.  Instance-specific functions for drawing, comparing, measuring, and deleting list entries are supplied by user code in the form of four blocks which can be specified either in code or in the View Composer:

	compareItemBlock - a monadic valuable which accepts as its argument a COMPAREITEMSTRUCT.

	deleteItemBlock - a monadic valuable which accepts as its argument a DELETEITEMSTRUCT.

	drawItemBlock - a dyadic valuable which accepts as its arguments a DRAWITEMSTRUCT and a Canvas.

	measureItemBlock - a monadic valuable which accepts as its argument a MEASUREITEMSTRUCT.
			This block must place a value in the itemHeight member of its argument.'!
!OwnerDrawListBox categoriesForClass!Unclassified! !
!OwnerDrawListBox methodsFor!

compareItemBlock
	^compareItemBlock!

compareItemBlock: aBlock
	compareItemBlock := aBlock!

defaultWindowStyle
	^super defaultWindowStyle bitOr: LBS_OWNERDRAWVARIABLE!

deleteItemBlock
	^deleteItemBlock!

deleteItemBlock: aBlock
	deleteItemBlock := aBlock!

drawItemBlock
	^drawItemBlock!

drawItemBlock: aBlock
	drawItemBlock := aBlock!

getItemDataMessage
	^LB_GETITEMDATA!

measureItemBlock
	^measureItemBlock!

measureItemBlock: aBlock
	measureItemBlock := aBlock!

setItemDataMessage
	^LB_SETITEMDATA!

wmCompareItem: message wParam: wParam lParam: lParam
	| compareItemStruct |
	self compareItemBlock notNil
		ifTrue: [ compareItemStruct := COMPAREITEMSTRUCT fromAddress: lParam.
			  ^self compareItemBlock value: compareItemStruct ]
		ifFalse: [ ^0 ]!

wmDeleteItem: message wParam: wParam lParam: lParam
	| deleteItemStruct |
	^self deleteItemBlock notNil ifTrue:
		[ deleteItemStruct := DELETEITEMSTRUCT fromAddress: lParam.
		self deleteItemBlock value: deleteItemStruct ]!

wmDrawItem: message wParam: wParam lParam: lParam
	| di canvas |
	^self drawItemBlock notNil ifTrue:
		[ di := DRAWITEMSTRUCT fromAddress: lParam.
		canvas := Canvas withNonOwnedDC: di hDC.
		self drawItemBlock value: di value: canvas.
		(di itemState bitAnd: ODS_FOCUS) ~= 0 ifTrue:
			[ UserLibrary default drawFocusRect: di hDC lprc: di rcItem ] ]!

wmMeasureItem: message wParam: wParam lParam: lParam
	| measureItemStruct |
	self measureItemBlock notNil ifTrue:
		[ measureItemStruct := MEASUREITEMSTRUCT fromAddress: lParam.
		self measureItemBlock value: measureItemStruct ]! !
!OwnerDrawListBox categoriesFor: #compareItemBlock!accessing!public! !
!OwnerDrawListBox categoriesFor: #compareItemBlock:!accessing!public! !
!OwnerDrawListBox categoriesFor: #defaultWindowStyle!accessing-styles!private! !
!OwnerDrawListBox categoriesFor: #deleteItemBlock!accessing!public! !
!OwnerDrawListBox categoriesFor: #deleteItemBlock:!accessing!public! !
!OwnerDrawListBox categoriesFor: #drawItemBlock!accessing!public! !
!OwnerDrawListBox categoriesFor: #drawItemBlock:!accessing!public! !
!OwnerDrawListBox categoriesFor: #getItemDataMessage!constants!private! !
!OwnerDrawListBox categoriesFor: #measureItemBlock!accessing!public! !
!OwnerDrawListBox categoriesFor: #measureItemBlock:!accessing!public! !
!OwnerDrawListBox categoriesFor: #setItemDataMessage!constants!private! !
!OwnerDrawListBox categoriesFor: #wmCompareItem:wParam:lParam:!event handling-win32!private! !
!OwnerDrawListBox categoriesFor: #wmDeleteItem:wParam:lParam:!event handling-win32!private! !
!OwnerDrawListBox categoriesFor: #wmDrawItem:wParam:lParam:!event handling-win32!private! !
!OwnerDrawListBox categoriesFor: #wmMeasureItem:wParam:lParam:!event handling-win32!private! !

!OwnerDrawListBox class methodsFor!

addMessageMapEntries
	self messageMap
		at: WM_DELETEITEM+1		put: #wmDeleteItem:wParam:lParam:;
		at: WM_MEASUREITEM+1	put: #wmMeasureItem:wParam:lParam:;
		at: WM_COMPAREITEM+1	put: #wmCompareItem:wParam:lParam:!

publishedAspectsOfInstances
	"Answer a Set of the aspects published by instances of the receiver"

	^super publishedAspectsOfInstances 
		add: (Aspect block: #measureItemBlock);	"Block that services WM_MEASUREITEM messages"
		add: (Aspect block: #drawItemBlock);	"Block that services WM_DRAWITEM messages"
		add: (Aspect block: #compareItemBlock);	"Block that services WM_COMPAREITEM messages"
		add: (Aspect block: #deleteItemBlock);	"Block that services WM_DELETEITEM messages"
		yourself!

removeMessageMapEntries
	self messageMap
		at: WM_DELETEITEM+1		put: nil;
		at: WM_MEASUREITEM+1	put: nil! !
!OwnerDrawListBox class categoriesFor: #addMessageMapEntries!installing!private! !
!OwnerDrawListBox class categoriesFor: #publishedAspectsOfInstances!must strip!public! !
!OwnerDrawListBox class categoriesFor: #removeMessageMapEntries!private!removing! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: ListPresenter name: 'Owner-draw list box') assign: (Object fromBinaryStoreBytes:
(ByteArray fromHexString: '2153544220312046020C0001000000566965775265736F75726365000000000E0124005354425265736F757263655354424279746541727261794163636573736F7250726F78790000000072000000ED020000215354422030204E080C000A0000005354425669657750726F7879000000004E020D0001000000535442436C61737350726F78790000000036000600537472696E67100000004F776E6572447261774C697374626F7892000000100000004F776E6572447261774C697374426F78260005004172726179150000000000000000000000C20000000200000036000C004C61726765496E74656765720400000021013144010400006000000046030900020000004C6973744D6F64656C000000000E021200535442436F6C6C656374696F6E50726F7879000000007A000000000000009200000007000000446F6C7068696E92000000110000004F726465726564436F6C6C656374696F6EC20000000000000000000000060014004964656E74697479536561726368506F6C696379000000000000000000000000070000000000000000000000000000000000000000000000F2000000040000007198E8777A0000000000000060010000920000001100000042617369634C697374416273747261637401000000000000000000000000000000000000000000000006010F004D65737361676553657175656E6365000000003A0100000000000050010000C20000000200000006030B004D65737361676553656E64000000000E010E0053544253796D626F6C50726F787900000000920000001000000063726561746541743A657874656E743AC20000000200000006020500506F696E74000000000B0000000B0000008202000000000000FB0000005F0100006000000022020000000000004A020000000000009200000011000000686F72697A6F6E74616C457874656E743AC200000001000000010000006000000006010F0057494E444F57504C4143454D454E5400000000360009004279746541727261792C0000002C0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF050000000500000082000000B40000003A0100000000000050010000800100008202000000000000C1000000C10000000000000013000000460504000300000049636F6E0000000000000000100000000E02110053544253696E676C65746F6E50726F7879000000009A000000000000005200000007000000446F6C7068696E5200000018000000496D61676552656C617469766546696C654C6F6361746F72BA00000000000000520000000700000063757272656E74520000001500000042617369634C69737441627374726163742E69636F0E021F0053544245787465726E616C5265736F757263654C69627261727950726F7879000000005200000010000000646F6C7068696E64723030352E646C6C00000000'))!

