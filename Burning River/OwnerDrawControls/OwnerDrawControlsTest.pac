| package |
package := Package name: 'OwnerDrawControlsTest'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicScriptAt: #postinstall put: 'OwnerDrawListBoxTestShell initialize'.

package classNames
	add: #OwnerDrawListBoxTestModel;
	add: #OwnerDrawListBoxTestShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #OwnerDrawListBoxTestShell -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'OwnerDrawControls';
	yourself).

package!

"Class Definitions"!

Model subclass: #OwnerDrawListBoxTestModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #OwnerDrawListBoxTestShell
	instanceVariableNames: 'presenter'
	classVariableNames: 'Bitmaps'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

OwnerDrawListBoxTestModel guid: (GUID fromString: '{1F933B04-77AD-465C-83B3-63D7A9B8BFBF}')!
OwnerDrawListBoxTestModel comment: ''!
!OwnerDrawListBoxTestModel categoriesForClass!Unclassified! !
OwnerDrawListBoxTestShell guid: (GUID fromString: '{AF9BAD9C-E385-4988-ABDE-151F139B29DF}')!
OwnerDrawListBoxTestShell comment: ''!
!OwnerDrawListBoxTestShell categoriesForClass!Unclassified! !
!OwnerDrawListBoxTestShell methodsFor!

createComponents
	"Private - Create the presenters for components in the view"

	super createComponents.
	self presenter: (self add: ListPresenter new name: 'ownerDrawBox').!

createSchematicWiring
	self presenter
		when: #selectionChanged send: #onSelectionChanged to: self!

onSelectionChanged
	MessageBox notify: 'You clicked on item ', self presenter selection displayString!

onViewOpened
	| v c |

	super onViewOpened.
	self presenter view
		drawItemBlock: self class drawItemBlock;
		measureItemBlock: self class measureItemBlock.
	self presenter list: #(1 2 3 4 5 6 7 8)!

presenter
	"Private - Answer the value of the receiver's ''presenter'' instance variable."

	^presenter!

presenter: anObject
	"Private - Set the value of the receiver's ''presenter'' instance variable to the argument, anObject."

	presenter := anObject! !
!OwnerDrawListBoxTestShell categoriesFor: #createComponents!initializing!private! !
!OwnerDrawListBoxTestShell categoriesFor: #createSchematicWiring!initializing!public! !
!OwnerDrawListBoxTestShell categoriesFor: #onSelectionChanged!event handling!public! !
!OwnerDrawListBoxTestShell categoriesFor: #onViewOpened!event handling!public! !
!OwnerDrawListBoxTestShell categoriesFor: #presenter!accessing!private! !
!OwnerDrawListBoxTestShell categoriesFor: #presenter:!accessing!private! !

!OwnerDrawListBoxTestShell class methodsFor!

bitmaps
	Bitmaps isNil ifTrue: [ Bitmaps := self initializeBitmaps ].
	^Bitmaps!

defaultModel
	^OwnerDrawListBoxTestModel new!

defaultView
	^'Default view'!

drawItemBlock
	^[ :aDrawItemStruct :aCanvas | | id bitmap |
	(id := aDrawItemStruct itemID) ~= 16rFFFFFFFF
		ifTrue:
			[ aCanvas brush: (Brush color: Color yellow).
			bitmap := OwnerDrawListBoxTestShell bitmaps at: 1.
			id \\ 2 = 0
				ifTrue: [ aCanvas brush: (Brush color: Color green).
		    			bitmap := OwnerDrawListBoxTestShell bitmaps at: 2 ].
			id \\ 3 = 0
				ifTrue: [ aCanvas brush: (Brush color: Color blue).
					    bitmap := OwnerDrawListBoxTestShell bitmaps at: 3 ].
			id \\ 4 = 0
				ifTrue: [ aCanvas brush: (Brush color: Color red).
					    bitmap := OwnerDrawListBoxTestShell bitmaps at: 4 ].
			aCanvas fillRectangle: (aDrawItemStruct rcItem asRectangle).
			bitmap drawOn: aCanvas
					at: aDrawItemStruct rcItem asRectangle topCenter - ((bitmap extent x / 2)@0)
					extent: bitmap extent ] ]!

initialize
	super initialize.
	self initializeBitmaps!

initializeBitmaps
	Bitmaps := OrderedCollection new.
	Bitmaps
		add: (Bitmap fromFile: 'Burning River\OwnerDrawControls\1.bmp');
		add: (Bitmap fromFile: 'Burning River\OwnerDrawControls\2.bmp');
		add: (Bitmap fromFile: 'Burning River\OwnerDrawControls\3.bmp');
		add: (Bitmap fromFile: 'Burning River\OwnerDrawControls\4.bmp')!

measureItemBlock
	^[ :aMeasureItemStruct  | | id bitmap |
	(id := aMeasureItemStruct itemID) >= 0
		ifTrue:
			[ bitmap := OwnerDrawListBoxTestShell bitmaps at: 1.
			id \\ 2 = 0
				ifTrue: [ bitmap := OwnerDrawListBoxTestShell bitmaps at: 2 ].
			id \\ 3 = 0
				ifTrue: [ bitmap := OwnerDrawListBoxTestShell bitmaps at: 3 ].
			id \\ 4 = 0
				ifTrue: [ bitmap := OwnerDrawListBoxTestShell bitmaps at: 4 ].
			aMeasureItemStruct itemHeight: bitmap extent y ] ]!

resetBitmaps
	Bitmaps := nil! !
!OwnerDrawListBoxTestShell class categoriesFor: #bitmaps!initializing!public! !
!OwnerDrawListBoxTestShell class categoriesFor: #defaultModel!models!public! !
!OwnerDrawListBoxTestShell class categoriesFor: #defaultView!public!views! !
!OwnerDrawListBoxTestShell class categoriesFor: #drawItemBlock!public! !
!OwnerDrawListBoxTestShell class categoriesFor: #initialize!initialize/release!public! !
!OwnerDrawListBoxTestShell class categoriesFor: #initializeBitmaps!initialize/release!public! !
!OwnerDrawListBoxTestShell class categoriesFor: #measureItemBlock!public! !
!OwnerDrawListBoxTestShell class categoriesFor: #resetBitmaps!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: OwnerDrawListBoxTestShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromHexString: '2153544220312046020C0001000000566965775265736F75726365000000000E0124005354425265736F757263655354424279746541727261794163636573736F7250726F78790000000072000000E2050000215354422031204E080C000A0000005354425669657750726F7879000000009A000000000000005200000010000000446F6C7068696E204D5650204261736552000000090000005368656C6C56696577620000001B0000000000000000000000620000000200000001009E0101000200A001000000000000000000000000000007020000000000000000000000000000A001000006070C00426F726465724C61796F7574000000000100000001000000000000000000000000000000000000009A010000000000009A00000000000000C0010000520000000D000000436F6E7461696E657256696577620000000F00000000000000A001000062000000020000008200000004000000000000440100020020020000000000000000000000000000070000000000000000000000000000002002000000000000EA000000000000000001000062000000020000009A010000000000009A0000000000000052000000110000004F776E657244726177436F6E74726F6C7352000000100000004F776E6572447261774C697374426F7862000000150000000000000020020000620000000200000082000000040000002101314401040000A002000046030900020000004C6973744D6F64656C00000000CA00000000000000D0000000620000000000000000000000060014004964656E74697479536561726368506F6C69637900000000000000000000000007000000000000000000000000000000A00200000000000082000000040000007198E8779A00000000000000C0010000520000001100000042617369634C69737441627374726163746200000000000000000000000000000000000000000000000000000006010F004D65737361676553657175656E636500000000CA00000000000000D0000000620000000300000006030B004D65737361676553656E6400000000BA00000000000000520000001000000063726561746541743A657874656E743A620000000200000006020500506F696E740000000015000000150000004204000000000000FB000000F1000000A0020000F203000000000000BA000000000000005200000017000000626173696353656C656374696F6E734279496E6465783A62000000010000006200000000000000A0020000F203000000000000BA000000000000005200000011000000686F72697A6F6E74616C457874656E743A620000000100000001000000A002000006010F0057494E444F57504C4143454D454E5400000000720000002C0000002C0000000000000001000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0A0000000A0000008700000082000000CA00000000000000D0000000400300004204000000000000C1000000C10000000000000013000000520000000C0000006F776E657244726177426F7800000000B203000000000000CA00000000000000D00000006200000001000000F2030000000000001004000062000000020000004204000000000000010000000100000042040000000000004F050000C5010000200200000205000000000000720000002C0000002C0000000000000001000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000A7020000E2000000CA00000000000000D00000006200000001000000A0020000400500000000000013000000EA00000000000000000100004003000000000000000000000000000000000000000000000100000000000000000000000000000000000000010000000000000000000000B203000000000000CA00000000000000D00000006200000002000000F20300000000000010040000620000000200000042040000000000000B0000000B00000042040000000000005F050000FB010000A0010000F203000000000000BA0000000000000052000000080000006D656E754261723A620000000100000000000000A00100000205000000000000720000002C0000002C0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0500000005000000B402000002010000CA00000000000000D0000000620000000100000020020000400500000000000015000000460504000300000049636F6E0000000000000000100000000E02110053544253696E676C65746F6E50726F7879000000009A000000000000005200000007000000446F6C7068696E5200000018000000496D61676552656C617469766546696C654C6F6361746F72BA00000000000000520000000700000063757272656E74520000000D0000005368656C6C566965772E69636F0E021F0053544245787465726E616C5265736F757263654C69627261727950726F7879000000005200000010000000646F6C7068696E64723030352E646C6C00000000'))!

