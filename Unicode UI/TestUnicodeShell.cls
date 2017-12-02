"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #TestUnicodeShell
	instanceVariableNames: 'listPresenter textPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestUnicodeShell guid: (GUID fromString: '{bc85183a-f405-4371-b7ce-8f0a93ebe59f}')!
TestUnicodeShell comment: 'self show'!
!TestUnicodeShell categoriesForClass!MVP-Presenters! !
!TestUnicodeShell methodsFor!

cmdAddTestToList

	| str |

	str := self class isUnicodeMode  ifTrue: [UnicodeString newTest] ifFalse: ['This is a test'].

	listPresenter model: (ListModel  on: (Array 
			with: (Array with: str with: str ) 
			with: (Array with: str  with: str)))!

cmdODBCTest

	| dbConn recSet dbPath|

	dbPath := (PackageManager current packageNamed: 'MitSciUnicodeUI') path , 'TestResources\' , 'UnicodeTestDB.accdb'.

	(File exists: dbPath) ifFalse: [^MessageBox  errorMsg: 'Unicode Test DB not present at ' , dbPath printString , '.' caption: 'Unicode Test DB Missing' ].
	dbConn := DBConnection new.
	dbConn isUnicode: true.
	dbConn connectString: ('DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=', dbPath , ';') asUnicodeString .
	dbConn open.
	recSet := dbConn  query: 'SELECT * FROM Table1 WHERE ID = 1;' asUnicodeString .
	MessageBox notify: ('Read from DB: ' asUnicodeString , (recSet first at: 'TestUCString' asUnicodeString)).
	recSet close.
	dbConn close.!

createComponents

	super createComponents.
	listPresenter := self add: ListPresenter new name: 'list'.
	textPresenter := self add: TextPresenter new name: 'text'.!

onViewOpened

	super onViewOpened.
	self class isUnicodeMode  ifTrue: [self translateText].!

translateMenu: aMenu
	"Private - cdemers - 10/31/2003 Revised to ensure menu text updates by removing and re-adding the menu."

	"aMenu isNil ifTrue: [^nil]."

	"val := (UserLibraryW default isWindowUnicode: SessionManager current inputState lastWindow asParameter ).
	self halt."
	| val |
	aMenu copy items do: 
			[:eachMenu |
			"self halt."
			aMenu removeItem: (aMenu find: (eachMenu text copyWithout: $&)).
			(eachMenu respondsTo: #text:) ifTrue: [eachMenu text: (self translateString: eachMenu text)].
			(eachMenu respondsTo: #description:)
				ifTrue: [eachMenu description: (self translateString: eachMenu description)].
			(eachMenu respondsTo: #items) ifTrue: [self translateMenu: eachMenu].
			aMenu addItem: eachMenu]!

translatePresenter: aPresenter
	"Private - cdemers - 10/30/2003 Translate the presenter."

	((aPresenter view respondsTo: #caption:) and: [aPresenter view caption notNil])
		ifTrue: [aPresenter view caption: (self translateString: aPresenter view caption)].
	"self halt."
	((aPresenter view respondsTo: #menuBar) and: [aPresenter view menuBar notNil])
		ifTrue: [self translateMenu: aPresenter view menuBar].
	aPresenter view subViews do: 
			[:eachView |
			(eachView isKindOf: ReferenceView) ifTrue: [self translatePresenter: eachView referee presenter].
			(eachView isKindOf: ListView)
				ifTrue: 
					[eachView font: nil.	"reset font"
					eachView columns do: 
							[:eachColumn |
							"self halt."
							eachColumn text: (self translateString: eachColumn text)]].
			(eachView isKindOf: TextEdit)
				ifFalse: 
					[((eachView respondsTo: #text:) and: [eachView text notEmpty])
						ifTrue: [eachView text: (self translateString: eachView text)]].
			eachView canAcceptSubViews ifTrue: [self translatePresenter: eachView presenter]]!

translateString: aString


	| str ucStr |
	"self shiftHalt."
	str := UnicodeString new.
	^ucStr := UnicodeString newTest.
	"(aString size / ucStr size) asInteger timesRepeat: [str := str , ucStr ].
	^str"
	!

translateText

	self translatePresenter: self.! !
!TestUnicodeShell categoriesFor: #cmdAddTestToList!public! !
!TestUnicodeShell categoriesFor: #cmdODBCTest!public! !
!TestUnicodeShell categoriesFor: #createComponents!public! !
!TestUnicodeShell categoriesFor: #onViewOpened!public! !
!TestUnicodeShell categoriesFor: #translateMenu:!private! !
!TestUnicodeShell categoriesFor: #translatePresenter:!private! !
!TestUnicodeShell categoriesFor: #translateString:!private! !
!TestUnicodeShell categoriesFor: #translateText!public! !

!TestUnicodeShell class methodsFor!

isUnicodeMode

	^true.!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 721158 ##(Smalltalk.SystemColor) 31 0 39 0 0 0 416 0 234 256 98 4 410 8 ##(Smalltalk.ListView) 98 30 0 416 98 2 8 1409355853 1025 544 590662 2 ##(Smalltalk.ListModel) 202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 7 0 0 0 544 0 8 4294902119 459270 ##(Smalltalk.Message) 8 #displayString 98 0 0 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 225 8 #left 786 816 832 8 ##(Smalltalk.SortedCollection) 786 8 #first 98 0 0 544 0 3 0 0 914 8 'Column 2' 251 960 786 816 98 0 786 8 #<= 1104 786 8 #last 98 0 0 544 0 1 0 0 8 #report 672 0 131169 0 98 4 0 0 328198 ##(Smalltalk.Point) 1 1 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 1234 91 337 1234 491 311 544 1330 8 #text: 98 1 8 'Column 1' 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 45 0 0 0 168 0 0 0 34 1 0 0 67 1 0 0] 98 0 1234 193 193 0 27 8 'list' 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140916352 1025 1584 0 722 8 4278190080 0 7 0 0 0 1584 0 8 4294902925 852486 ##(Smalltalk.NullConverter) 0 0 1 1266 202 208 98 4 1330 1360 98 2 1234 45 99 1234 391 41 1584 1330 1440 98 1 8 'Some Text' 1584 1330 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 1584 1330 8 #isTextModified: 98 1 32 1584 1490 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 22 0 0 0 49 0 0 0 217 0 0 0 69 0 0 0] 98 0 1552 0 27 8 'text' 0 461638 4 ##(Smalltalk.MenuBar) 0 16 98 2 265030 4 ##(Smalltalk.Menu) 0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #noCommand 8 'Test Command 1' 1 1 0 0 0 8 'Test Menu 1' 0 134217729 0 0 62115 0 0 2146 0 16 98 1 2194 1 2226 2256 8 'Test Command 2' 1 1 0 0 0 8 'Test Menu 2' 0 134217729 0 0 62119 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1266 202 208 98 2 1330 1360 98 2 1234 6719 21 1234 671 801 416 1330 8 #updateMenuBar 672 416 1490 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 31 13 0 0 10 0 0 0 110 14 0 0 154 1 0 0] 98 7 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 2608 0 0 0 7 0 0 0 2608 0 8 4294901967 2226 8 #cmdAddTestToList 8 'Button1' 1 1 0 0 32 0 0 0 1266 202 208 98 3 1330 1360 98 2 1234 41 191 1234 251 51 2608 1330 8 #isEnabled: 98 1 32 2608 1330 1440 98 1 8 'Button1' 2608 1490 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 95 0 0 0 145 0 0 0 120 0 0 0] 98 0 1552 0 29 410 2624 98 20 0 416 98 2 8 1140924416 1 3008 0 0 0 7 0 0 0 3008 0 8 4294901967 2226 8 #cmdODBCTest 8 'Button2' 1 1 0 0 32 0 0 0 1266 202 208 98 2 1330 1360 98 2 1234 321 191 1234 241 51 3008 1330 1440 98 1 8 'Button2' 3008 1490 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 95 0 0 0 24 1 0 0 120 0 0 0] 98 0 1552 0 29 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 65 3344 0 0 0 7 0 0 0 3344 0 8 4294902251 1714 0 0 0 1266 202 208 98 2 1330 1360 98 2 1234 85 39 1234 341 41 3344 1330 1440 98 1 8 'Test Text' 3344 1490 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 42 0 0 0 19 0 0 0 212 0 0 0 39 0 0 0] 98 0 1552 0 27 1584 544 410 8 ##(Smalltalk.CheckBox) 98 16 0 416 98 2 8 1409363203 1 3664 721990 2 ##(Smalltalk.ValueHolder) 0 0 1114118 ##(Smalltalk.NeverSearchPolicy) 32 0 0 7 0 0 0 3664 0 8 4294901967 1714 0 0 0 1266 202 208 98 2 1330 1360 98 2 1234 41 251 1234 227 43 3664 1330 1440 98 1 8 'Test Checkbox' 3664 1490 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 125 0 0 0 133 0 0 0 146 0 0 0] 98 0 1552 0 27 410 8 ##(Smalltalk.GroupBox) 98 14 0 416 98 2 8 1140850695 65 4048 0 722 8 4278190080 0 7 0 0 0 4048 0 8 4294901967 1266 202 208 98 2 1330 1360 98 2 1234 25 149 1234 281 161 4048 1330 1440 98 1 8 'Group Box' 4048 1490 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 12 0 0 0 74 0 0 0 152 0 0 0 154 0 0 0] 98 0 1552 0 27 1552 0 27 )! !
!TestUnicodeShell class categoriesFor: #isUnicodeMode!public! !
!TestUnicodeShell class categoriesFor: #resource_Default_view!public!resources-views! !

