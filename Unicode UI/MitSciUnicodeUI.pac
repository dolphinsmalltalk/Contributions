| package |
package := Package name: 'MitSciUnicodeUI'.
package paxVersion: 1;
	basicComment: 'This package adds experimental Unicode support to the Dolphin UI.

This is done in a two stages:
1. The package loads code as normal.
2. After a prompt, automated transforms are applied to the code base.  This approach was taken to facilitate the ease of applying Unicode UI support to a independently evolving Dolphin core.  The results of these transforms are not captured into new packages. See the package scripts for more information.

This may not be a complete or optimally efficient implementation.  This started as an experimental prototype to explore the concept of adding Unicode UI support to Dolphin Smalltalk.

Areas in Need of Improvement:
1. Expand coverage of Unicode support as needed.  Presently most of the UI, and ODBC Databases have decent Unicode support.
2. Some of the transforms are simple text replacements.  It may be desirable to have more sophisticated parse tree aware transforms.
3. I haven''t used this, "for real", if someone does then further areas in need of improvement will come up. 

See example UI by evaluating:
	TestUnicodeShell show.

Initially developed by Mitchell Scientific, Inc. in 2016 .

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of th??Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.005'.

package basicScriptAt: #postinstall put: 'UserLibraryW openDefault.
GDILibraryW openDefault.

(MessageBox confirm: ''Allowing this section of the post install script will transform system methods to add Unicode support.  This package should ONLY be loaded into a test image.  Allow system changes?'' caption: ''Allow System Hooks?'') 
	ifTrue: [| uct |
		uct := MitSciUnicodeTransformer new.
		uct applyAutomatedCodeTransformations
]'.
package basicScriptAt: #preinstall put: '"Add a class variable a Unicode view atom."
(View classVarNames includes: ''WndClassAtomW'') ifFalse: [View addClassVarName: ''WndClassAtomW''].
(View classVarNames includes: ''UnicodeMask'') ifFalse: [View addClassVarName: ''UnicodeMask''.
	View classPool at: ''UnicodeMask'' put: 1024 ].
(DBColAttr instVarNames includes: ''isUnicode'') ifFalse: [DBColAttr addInstVarName: ''isUnicode''].
'.

package classNames
	add: #FlipperInspectorW;
	add: #GDILibraryW;
	add: #MitSciUnicodeAPIScanner;
	add: #MitSciUnicodeTransformer;
	add: #ODBCLibraryW;
	add: #TestNonUnicodeShell;
	add: #TestUnicodeShell;
	add: #UserLibraryW;
	add: #WNDCLASSW;
	yourself.

package methodNames
	add: #ByteArray -> #asUnicodeString;
	add: #Canvas -> #gdiLibrary;
	add: #Canvas -> #gdiLibraryForString:;
	add: #Canvas -> #userLibrary;
	add: #Canvas -> #userLibraryForString:;
	add: #CCITEM -> #textInBuffer:;
	add: #Clipboard -> #getText;
	add: #Clipboard -> #getUnicodeTextifNone:;
	add: #ContainerView -> #isUnicodeView:;
	add: #CRTLibrary -> #wcsncpy:strSource:count:;
	add: #DBAbstractStatement -> #describeCols:;
	add: #DBAbstractStatement -> #isUnicode;
	add: #DBAbstractStatement -> #odbcLibrary;
	add: #DBAbstractStatement -> #odbcLibraryForString:;
	add: #DBAbstractStatement -> #stringClass;
	add: #DBBoundBuffer -> #bind:;
	add: #DBColAttr -> #cType;
	add: #DBColAttr -> #isCharType;
	add: #DBColAttr -> #isUnicode;
	add: #DBColAttr -> #isUnicode:;
	add: #DBColAttr -> #lengthC;
	add: #DBColumnsStatement -> #executeStatement;
	add: #DBConnection -> #columns:qualifier:owner:table:;
	add: #DBConnection -> #isUnicode;
	add: #DBConnection -> #isUnicode:;
	add: #DBConnection -> #odbcLibrary;
	add: #DBConnection -> #open;
	add: #DBConnection -> #stringClass;
	add: #DBField -> #asString;
	add: #DBField -> #fromString:;
	add: #DBField -> #getData:;
	add: #DBParameterizedStatement -> #prepare;
	add: #DBParameterizedStatement -> #setParams;
	add: #DBResultSet -> #fetchScroll:offset:;
	add: #DBResultSet -> #odbcLibrary;
	add: #DBResultSet -> #odbcLibraryForString:;
	add: #DBStatement -> #executeStatement;
	add: #DBTablesStatement -> #executeStatement;
	add: #GraphicsTool -> #userLibrary;
	add: #GraphicsTool -> #userLibraryForString:;
	add: #InputState -> #lastWindow;
	add: #KernelLibrary -> #lstrcmpiW:lpString2:;
	add: #KernelLibrary -> #lstrcmpW:lpString2:;
	add: #ListView -> #lvmGetItem:;
	add: #ListView -> #lvmGetStringWidth:;
	add: #ListView -> #lvmSetColumn:at:;
	add: #ListView -> #lvmSetItem:;
	add: #ListView -> #onDisplayDetailsRequired:;
	add: #Menu -> #userLibrary;
	add: #MenuItem -> #userLibrary;
	add: #SymbolStringSearchPolicy -> #compare:with:;
	add: #SymbolStringSearchPolicy -> #hash:;
	add: #TabView -> #getItem:;
	add: #TabView -> #tcmInsertItem:atOffset:;
	add: #TabView -> #updateItem:atIndex:;
	add: #UnicodeCharacter -> #isSeparator;
	add: #UnicodeString -> #_collate:;
	add: #UnicodeString -> #displayString;
	add: #UnicodeString -> #includes:;
	add: #UnicodeString -> #inspectorClass;
	add: #UnicodeString -> #show;
	add: #UnicodeString -> #trueCompare:;
	add: #UserLibrary -> #isWindowUnicode:;
	add: #View -> #basicCreateAt:extent:;
	add: #View -> #isUnicodeView;
	add: #View -> #isUnicodeView:;
	add: #View -> #parentView:;
	add: #View -> #stringClass;
	add: #View -> #userLibrary;
	add: #View -> #userLibraryForString:;
	add: 'ListView class' -> #initializeNotificationMap;
	add: 'Presenter class' -> #isUnicodeMode;
	add: 'Presenter class' -> #loadViewResource:inContext:;
	add: 'Prompter class' -> #isUnicodeMode;
	add: 'UnicodeString class' -> #newTest;
	add: 'View class' -> #onStartup;
	add: 'View class' -> #registerClassW;
	add: 'View class' -> #reregisterClassW;
	add: 'View class' -> #unregisterClassW;
	add: 'View class' -> #winClassNameW;
	add: 'View class' -> #wndClassNameW;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Core\Object Arts\Dolphin\Database\Database Connection Base';
	add: '..\..\Core\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Core\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Core\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\Core\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\..\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Core\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Core\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\..\Core\Contributions\Refactory\Refactoring Browser\Environments\RBEnvironments';
	add: '..\..\Core\Object Arts\Dolphin\System\Compiler\Smalltalk Parser';
	add: '..\Udo Schneider\US Unicode';
	yourself).

package!

"Class Definitions"!

Object subclass: #MitSciUnicodeAPIScanner
	instanceVariableNames: 'ansiAPIFunctionMethods'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ODBCLibrary subclass: #ODBCLibraryW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GDILibrary subclass: #GDILibraryW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UserLibrary subclass: #UserLibraryW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WNDCLASS subclass: #WNDCLASSW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MitSciUnicodeAPIScanner subclass: #MitSciUnicodeTransformer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #TestUnicodeShell
	instanceVariableNames: 'listPresenter textPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FlipperInspector subclass: #FlipperInspectorW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestUnicodeShell subclass: #TestNonUnicodeShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ByteArray methodsFor!

asUnicodeString
	"Answer a UnicodeString containing the same elements as the receiver."
	"cdemers 12/7/2016 Added for Unicode support."

	^UnicodeString fromAddress: self yourAddress length: self size / 2! !
!ByteArray categoriesFor: #asUnicodeString!converting!public! !

!Canvas methodsFor!

gdiLibrary

	"cdemers 12/6/2016  I don't presently have a good way to default this.  Going to try to override based on string type arguments wherever I can."
	^GDILibrary default
!

gdiLibraryForString: aStringOrUnicodeString 

	"cdemers 12/6/2016"
	^((aStringOrUnicodeString class == UnicodeString ) ifTrue: [GDILibraryW] ifFalse: [GDILibrary]) default
!

userLibrary

	"Private - cdemers 10/26/2016"
	"^((UserLibraryW default isWindowUnicode: self handle) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default"
	"This is an inelegant way to do this.  In most cases the active window can determin the Unicode sstatus. "
	^((UserLibraryW default isWindowUnicode:  SessionManager current inputState lastWindow asParameter) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default!

userLibraryForString: aStringOrUnicodeString

	"Private - cdemers 10/28/2016"
	^((aStringOrUnicodeString class == UnicodeString ) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default! !
!Canvas categoriesFor: #gdiLibrary!public! !
!Canvas categoriesFor: #gdiLibraryForString:!public! !
!Canvas categoriesFor: #userLibrary!private! !
!Canvas categoriesFor: #userLibraryForString:!private! !

!CCITEM methodsFor!

textInBuffer: aString
	"Writes aString into a system buffer pointed to by pszText
	and of size cchTextMax.

	cdeemrs 11/1/2016 Revised for Unicode support."

	aString class == UnicodeString 
		ifTrue: [CRTLibrary default 
			wcsncpy: self pszTextAddress	"Raw address of text buffer allocated by control"
			strSource: aString 
			count: self cchTextMax] 
		ifFalse: [CRTLibrary default 
			strncpy: self pszTextAddress	"Raw address of text buffer allocated by control"
			strSource: aString 
			count: self cchTextMax]! !
!CCITEM categoriesFor: #textInBuffer:!accessing!public! !

!Clipboard methodsFor!

getText
        "Answer a <readableString> containing the the CF_TEXT contents 
	of the clipboard. If no text is currently available, raise an exception."
	"cdemers 12/6/2016 Revised to return a Unicode string if available, otherwise see if there is an ANSI string."

	^(self isFormatIdAvailable: CF_UNICODETEXT) 
		ifTrue: [self getUnicodeTextifNone: [self errorFormatNotAvailable: #String] ]
		ifFalse: [self getTextIfNone: [self errorFormatNotAvailable: #String]]!

getUnicodeTextifNone: exceptionHandler 
	"cdemers 10/12/2016 Expiriment to see if we can get unicode text."
	"Private - Answer a <readableString> containing the text contents
	of the clipboard of the specified format. If the format is not currently 
	available, the answers the result of evaluating the <niladicValuable> 
	exceptionHandler.
	N.B. It is not checked that formatId is actually a text format."

	^self apply: 
			[| hText pText text |
			hText := UserLibrary default getClipboardData: CF_UNICODETEXT.
			hText isNull ifTrue: [^exceptionHandler value].
			pText := KernelLibrary default globalLock: hText.
			text := UnicodeString fromAddress: pText.
			KernelLibrary default globalUnlock: hText.
			text]! !
!Clipboard categoriesFor: #getText!accessing!public! !
!Clipboard categoriesFor: #getUnicodeTextifNone:!private! !

!ContainerView methodsFor!

isUnicodeView: aBoolean

	"cdemers 10/20/2016 Set the Unicode state deeply. This will not recreate windows, so to have a proper Unicode view it will need to be set before the view is opened."
	super isUnicodeView: aBoolean.
	self subViewsDo: [:eachSv | 
		eachSv isUnicodeView: aBoolean].! !
!ContainerView categoriesFor: #isUnicodeView:!public! !

!CRTLibrary methodsFor!

wcsncpy: strDest strSource: strSource count: count
	"Copy up to count characters of the <String> strSource to the <String> strDest 
	and answer strDest. A null appended if there is sufficient space in the destination 
	buffer. The source and destination must not overlap.

		char *strncpy( char *strDest, const char *strSource, size_t count );

	cdemers 11/1/2016 Added for Unicode support.
	"

	<cdecl: lpvoid wcsncpy lpvoid lpvoid intptr>
	^self invalidCall
! !
!CRTLibrary categoriesFor: #wcsncpy:strSource:count:!CRT functions-string manipulation!public! !

!DBAbstractStatement methodsFor!

describeCols: columnNumbers 
	"Answer an array of <DBColAttr>s describing the each of the columns
	of the receiver with indices in the <sequencedReadableCollection> argument."

	| answer i name columnSize colNameLen dataType decimalDigits nullable nameBufSize hStmt colLen lib |
	nameBufSize := self parent maxColumnNameLength+ 1.
	name := self stringClass newFixed: nameBufSize.
	colNameLen := SWORD new.
	dataType := SWORD new.
	columnSize := DWORD new.
	decimalDigits := SWORD new.
	nullable := SWORD new.
	colLen := DWORD new.
	hStmt := self executedHandle.
	lib := self odbcLibrary.
	answer := Array new: columnNumbers size.
	i := 1.
	columnNumbers do: 
			[:each | 
			| ret |
			ret := lib 
						sqlDescribeCol: hStmt
						columnNumber: each
						columnName: name
						bufferLength: nameBufSize
						nameLengthPtr: colNameLen
						dataTypePtr: dataType
						columnSizePtr: columnSize
						decimalDigitsPtr: decimalDigits
						nullablePtr: nullable.
			self dbCheckException: ret.
			ret := lib 
						sqlColAttribute: hStmt
						columnNumber: each
						fieldIdentifier: SQL_COLUMN_LENGTH
						characterAttributePtr: nil
						bufferLength: nil
						stringLengthPtr: nil
						numericAttributePtr: colLen.	"Note using ODBC 2.x definition of column length"
			self dbCheckException: ret.
			answer at: i
				put: ((DBColAttr new)
						columnNumber: each;
						name: (name leftString: colNameLen value);
						type: dataType value;
						length: colLen value;
						precision: columnSize value;
						scale: decimalDigits value;
						isUnicode: self parent isUnicode).
			i := i + 1].
	^answer!

isUnicode

	^self parent isUnicode!

odbcLibrary

	^(self isUnicode ifTrue: [ODBCLibraryW] ifFalse: [ODBCLibrary]) default !

odbcLibraryForString: aStringOrUnicodeString

	"cdemers 11/15/2016"
	^((aStringOrUnicodeString class == UnicodeString ) ifTrue: [ODBCLibraryW] ifFalse: [ODBCLibrary]) default
	!

stringClass

	"cdemers 11/15/2016 "
	^self isUnicode ifTrue: [UnicodeString] ifFalse: [String]! !
!DBAbstractStatement categoriesFor: #describeCols:!accessing!public! !
!DBAbstractStatement categoriesFor: #isUnicode!public! !
!DBAbstractStatement categoriesFor: #odbcLibrary!public! !
!DBAbstractStatement categoriesFor: #odbcLibraryForString:!public! !
!DBAbstractStatement categoriesFor: #stringClass!public! !

!DBBoundBuffer methodsFor!

bind: aDBStatement
	"Private - Bind the receiver's field buffers to columns in the result table."

	| hStmt |
	hStmt := super bind: aDBStatement.
	self contents with: columns
		do: 
			[:eachField :eachColumn | 
			aDBStatement dbCheckException: (aDBStatement odbcLibrary 
						sqlBindCol: hStmt
						columnNumber: eachColumn columnNumber
						targetType: eachColumn cType
						targetValuePtr: eachField fieldBuf
						bufferLength: (##(2 raisedTo: 16) min: eachField fieldSize)
						strLenOrInd: eachField lengthBuf)].
	^hStmt! !
!DBBoundBuffer categoriesFor: #bind:!operations!private! !

!DBColAttr methodsFor!

cType
	"Private - Answer the 'C' type to which the described column's values should be converted when loaded
	into Dolphin buffers (DBFields)."

	"cdemers 11/15/2016 Add Unicode support."
	| type |

	type := SQLToCTypes at: sqlType+TypeOffset.
	^(type = SQL_C_CHAR and:[ self isUnicode]) 
		ifTrue: ["SQL_C_WCHAR May be wrong, seems to be defined as -9, I think it should be -8" -8 ]
		ifFalse: [type]
	!

isCharType
	"Private - Answers true if the receiver represents a character based column"

	"cdemers 2016/11/22 Added support for SQL_C_WCHAR for Unicode."

	^self cType == SQL_C_CHAR or: [self cType ==  -8 ]!

isUnicode
	"cdemers 11/15/2016"
	^isUnicode ifNil: [false]!

isUnicode: aBoolean
	"cdemers 11/15/2016"
	isUnicode := aBoolean!

lengthC
	"Private - Answer the length of a field sufficient to hold column
	 entries when SQL_C_DEFAULT conversion is used (basically need
	 an extra byte for null terminator for string types)"

	"cdemers 2016/11/22 We need a larger buffer for Unicode Strings."
	^self isCharType ifTrue: [self isUnicode ifTrue: [(length+1) * 2] ifFalse: [length+1]] ifFalse: [length]! !
!DBColAttr categoriesFor: #cType!accessing!private! !
!DBColAttr categoriesFor: #isCharType!private!testing! !
!DBColAttr categoriesFor: #isUnicode!public!testing! !
!DBColAttr categoriesFor: #isUnicode:!accessing!private! !
!DBColAttr categoriesFor: #lengthC!accessing!private! !

!DBColumnsStatement methodsFor!

executeStatement
	"Private - Execute the database command that the receiver represents.
	Answer the <integer> return code."
	
	^(self odbcLibraryForString:  self tableName)
		sqlColumns: self allocatedHandle
		catalogName: self catalogName
		nameLength1: SQL_NTS
		schemaName: self schemaName
		nameLength2: SQL_NTS
		tableName: self tableName
		nameLength3: SQL_NTS
		columnName: self columnName
		nameLength4: SQL_NTS! !
!DBColumnsStatement categoriesFor: #executeStatement!operations!private! !

!DBConnection methodsFor!

columns: aStringColumn qualifier: aStringQualifier owner: aStringOwner table: aStringTable 
	"Answer the list of columns in the table matching the specified search criteria"

	| i stmt colAttrs |
	stmt := DBColumnsStatement parent: self.
	stmt
		catalogName: aStringQualifier;
		schemaName: aStringOwner;
		tableName: aStringTable;
		columnName: aStringColumn.
	i := 0.
	colAttrs := stmt results collect: 
					[:each | 
					i := i + 1.
					"at: 'column_name'"	"at: 'DATA_TYPE'"	"at: 'LENGTH'"	"at: 'PRECISION'"	"at: 'SCALE'"
					(DBColAttr new)
						columnNumber: i;
						name: (each atIndex: 4);
						type: (each atIndex: 5);
						length: (each atIndex: 8);
						precision: (each atIndex: 7);
						scale: (each atIndex: 9);
						isUnicode: self isUnicode
						yourself].
	stmt free.
	^colAttrs!

isUnicode
	^flags allMask: 2 "UnicodeMask"!

isUnicode: aBoolean
	"Enable/disable the user of the Unicode option when opening this connection."

	flags := flags mask: 2 "UnicodeMask" set: aBoolean!

odbcLibrary

	^(self isUnicode ifTrue: [ODBCLibraryW] ifFalse: [ODBCLibrary]) default !

open
	"Open the receiver after prompting for the connection details, but only
	if not already connected."

	| connSz lenConnSz |
	handle isNull ifFalse: [^self].
	
	[| ret |
	"#1306: From MSDN, 'Applications should allocate at least 1,024 bytes for [the connection string] buffer'"
	connSz := self stringClass newFixed: 2048.
	lenConnSz := SWORD new.
	ret := self odbcLibrary 
				sqlDriverConnect: self getHandle
				windowHandle: UserLibrary default getActiveWindow
				inConnectionString: self connectString
				stringLength1: SQL_NTS
				outConnectionString: connSz
				bufferLength: connSz size
				stringLength2Ptr: lenConnSz
				driverCompletion: (self useDriverCompletion 
						ifTrue: [SQL_DRIVER_COMPLETE]
						ifFalse: [SQL_DRIVER_NOPROMPT]).
	self dbCheckException: ret] 
			ifCurtailed: [self free].
	self connectString: (connSz copyFrom: 1 to: (lenConnSz value min: connSz size))!

stringClass

	"cdemers 11/15/2016 "
	^self isUnicode ifTrue: [UnicodeString] ifFalse: [String]! !
!DBConnection categoriesFor: #columns:qualifier:owner:table:!enquiries!public! !
!DBConnection categoriesFor: #isUnicode!accessing!public! !
!DBConnection categoriesFor: #isUnicode:!accessing!public! !
!DBConnection categoriesFor: #odbcLibrary!public! !
!DBConnection categoriesFor: #open!operations!public! !
!DBConnection categoriesFor: #stringClass!public! !

!DBField methodsFor!

asString
	"Private - Answer the receiver's buffer as a <String>."
	"cdemers 11/15/2016 Revised for Unicode support."
	"self halt."
	^self column isUnicode 
		ifTrue: ["(buffer copy: UnicodeString from: 1 to: self length)"
			buffer replaceBytesOf: (UnicodeString new: self length/2)
				from: 1
				to: self length
				startingAt: 1]
		ifFalse: [buffer copyStringFrom: 1 to: self length]
!

fromString: aString
	"Private - Set the receiver's buffer from aString."
	"cdemers 2016/12/1 Modified to support UnicodeStrings"

	| stringToSave|

	stringToSave := self column isUnicode ifTrue: [aString asUnicodeString ] ifFalse: [aString ].
	(self shouldTranslate: stringToSave  class: String) ifTrue: [ 	| byteCount |
		byteCount := (stringToSave  basicSize). 
		byteCount > buffer size
			ifTrue: [^self error: 'String too long. Max ', buffer size displayString, ' got ', byteCount displayString].
		self column isUnicode 
			ifTrue: [buffer replaceFrom: 1 to: byteCount with: stringToSave asByteArray.]
			ifFalse: [buffer replaceFrom: 1 to: byteCount with: stringToSave].
		
		self length: byteCount]!

getData: aDBStatement 
	"Private - Retrieve the receiver's associated column data from the ODBC result set
	following a fetch (into the receiver's buffer)."

	| ret |
	ret := aDBStatement odbcLibrary 
				sqlGetData: aDBStatement executedHandle
				columnNumber: column columnNumber
				targetType: SQL_C_DEFAULT
				targetValuePtr: buffer
				bufferLength: buffer size
				strLenOrIndPtr: lengthBuf.
	aDBStatement dbCheckException: ret! !
!DBField categoriesFor: #asString!converting!private! !
!DBField categoriesFor: #fromString:!converting!private! !
!DBField categoriesFor: #getData:!accessing!private! !

!DBParameterizedStatement methodsFor!

prepare
	"Private - Prepare the receiver for later execution when the parameter values have 
	been set. N.B. This should only be sent once unless closed in the interm."

	| ret |
	isPrepared := false.
	parameters := nil.
	ret := ( self odbcLibrary "self odbcLibraryForString: self sqlString" )
				sqlPrepare: self allocatedHandle
				statementText: (self isUnicode ifTrue: [self sqlString asUnicodeString ] ifFalse: [self sqlString])
				textLength: SQL_NTS.
	self dbCheckException: ret.
	isPrepared := true!

setParams
	"Private - Bind parameter columns for a prepared SQL statement so that when we 
	subsequently fill the buffer fields the statement is ready to exec. Normally only done 
	just before first exec (from #parameters)."

	paramCols keysAndValuesDo: 
			[:i :eachCol | 
			| buf eachField |
			eachField := parameters at: i.
			buf := eachField fieldBuf.
			
			self dbCheckException: (self odbcLibrary 
						sqlBindParameter: self allocatedHandle
						parameterNumber: i
						inputOutputType: eachCol parameterType
						valueType: eachCol cType "SQL_C_DEFAULT" "cdemers - For some reason SQL_C_DEFAULT does not work here for Unicode vlaues."
						parameterType: eachCol type
						columnSize: eachCol precision
						decimalDigits: eachCol scale
						parameterValuePtr: buf
						bufferLength: (##(2 raisedTo: 16) min: buf byteSize)
						strLenOrIndPtr: eachField lengthBuf)]! !
!DBParameterizedStatement categoriesFor: #prepare!operations!private! !
!DBParameterizedStatement categoriesFor: #setParams!operations!private! !

!DBResultSet methodsFor!

fetchScroll: orientationInteger offset: offsetInteger 
	"Private - Fetch the specified row from the receiver's
	result set. Answers true if a row was successfully
	fetched, false for end of result set, and throws an
	DBError exception if an error occurs"

	| ret |
	ret := self odbcLibrary 
				sqlFetchScroll: self statementHandle
				fetchOrientation: orientationInteger
				fetchOffset: offsetInteger.
	ret = SQL_NO_DATA_FOUND ifTrue: [^nil].
	statement dbCheckException: ret.

	"Unbound buffers must now retrieve all columns, bound
	 buffers must retrieve long unbound columns"
	buffer getData: statement.
	^buffer status!

odbcLibrary

	^self statement odbcLibrary !

odbcLibraryForString: aStringOrUnicodeString

	"cdemers 11/15/2016"
	^self statement odbcLibraryForString: aStringOrUnicodeString
	! !
!DBResultSet categoriesFor: #fetchScroll:offset:!positioning!private! !
!DBResultSet categoriesFor: #odbcLibrary!public! !
!DBResultSet categoriesFor: #odbcLibraryForString:!public! !

!DBStatement methodsFor!

executeStatement
	"cdemers 11/15/2016"
	^(self odbcLibraryForString: self sqlString)
		sqlExecDirect: self allocatedHandle
		statementText: self sqlString
		textLength: SQL_NTS! !
!DBStatement categoriesFor: #executeStatement!operations!private! !

!DBTablesStatement methodsFor!

executeStatement
	"Private - Execute the database command that the receiver represents.
	Answer the <integer> return code."

	^(self odbcLibraryForString: tableType) 
		sqlTables: self allocatedHandle
		szTableQualifier: catalogName
		cbTableQualifier: SQL_NTS
		szTableOwner: schemaName
		cbTableOwner: SQL_NTS
		szTableName: tableName
		cbTableName: SQL_NTS
		szTableType: tableType
		cbTableType: SQL_NTS! !
!DBTablesStatement categoriesFor: #executeStatement!operations!private! !

!GraphicsTool methodsFor!

userLibrary

	"Private - cdemers 10/26/2016"
	"^((UserLibraryW default isWindowUnicode: self handle) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default"
	"This is an inelegant way to do this.  In most cases the active window can determin the Unicode sstatus. "
	^((UserLibraryW default isWindowUnicode:  SessionManager current inputState lastWindow asParameter ) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default!

userLibraryForString: aStringOrUnicodeString

	"Private - cdemers 10/28/2016"
	^((aStringOrUnicodeString class == UnicodeString ) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default! !
!GraphicsTool categoriesFor: #userLibrary!private! !
!GraphicsTool categoriesFor: #userLibraryForString:!private! !

!InputState methodsFor!

lastWindow

	"cdemers 10/26/2016 We may be able to use this to determin if we are working on a Unicode window."
	^lastWindow! !
!InputState categoriesFor: #lastWindow!public! !

!KernelLibrary methodsFor!

lstrcmpiW: aString1 lpString2: aString2
  	"Answer -1, 0 or 1 depending on whether aString collates before, the same as, 
	or after aString2 ignoring case (respectively).
		int lstrcmpi(
  			LPCTSTR lpString1,	// address of first string 
			LPCTSTR lpString2 	// address of second string 
		);

	N.B. If you need more flexibility, implement the CompareStrings() call (which
	this probably just layers on top of anyway)."
	"cdemers 12/9/2016 Added for Unicode support." 

	<stdcall: sdword lstrcmpiW lpstr lpstr>
	^self invalidCall!

lstrcmpW: aString1 lpString2: aString2
  	"Answer -1, 0 or 1 depending on whether aString collates before, the same as, 
	or after aString2 sensitive to case (respectively).
		int lstrcmp(
  			LPCTSTR lpString1,	// address of first string 
			LPCTSTR lpString2 	// address of second string 
		);

	N.B. If you need more flexibility, implement the CompareStrings() call (which
	this probably just layers on top of anyway)."
	"cdemers 12/9/2016 Added for Unicode support." 

	<stdcall: sdword lstrcmpW lpstr lpstr>
	^self invalidCall! !
!KernelLibrary categoriesFor: #lstrcmpiW:lpString2:!public!win32 functions-string manipulation! !
!KernelLibrary categoriesFor: #lstrcmpW:lpString2:!public!win32 functions-string manipulation! !

!ListView methodsFor!

lvmGetItem: aLvItem
	"Private - Retrieve the requested items attributes into the argument, aLvItem."

	(self sendMessage: (self isUnicodeView ifTrue: [( LVM_GETITEM  - 5 + 75)] ifFalse: [  LVM_GETITEM ])wParam: 0 lpParam: aLvItem) == 0
		ifTrue: [^self errorInCommonControlCall]!

lvmGetStringWidth: aString
	"Private - Answer the width of aString when displayed in this view."

	| width |
	width := self sendMessage: (self isUnicodeView ifTrue: [(LVM_GETSTRINGWIDTH  - 17 + 87)] ifFalse: [ LVM_GETSTRINGWIDTH ]) wParam: 0 lpParam: aString.
	^width == 0
		ifTrue: [self errorInCommonControlCall]
		ifFalse: [width]!

lvmSetColumn: anLvColumn at: columnIndex
	"Private - Set the attributes of a column."
"Must use LVM_SETCOLUMNW if we are Unicode"
	(self sendMessage: ((self isUnicodeView and: [anLvColumn text class == UnicodeString ]) 
			ifTrue: [(LVM_SETCOLUMN - 26 + 96)] ifFalse: [ LVM_SETCOLUMN]) wParam: columnIndex lpParam: anLvColumn asParameter) == 0
				ifTrue: [^self errorInCommonControlCall]!

lvmSetItem: anLvItem
	"Private - Set some or all of the receivers default attributes as 
	specified in the argument, anLvItem.
	cdemers 11/1/2016 Revised for Unicode support."

	(self 
		sendMessage: (self isUnicodeView ifTrue: [(LVM_SETITEM - 6 + 76)] ifFalse: [ LVM_SETITEM])
		wParam: 0
		lpParam: anLvItem asParameter) == 0 
		ifTrue: [^self errorInCommonControlCall]!

onDisplayDetailsRequired: lvitem 
	"Private - Get the display info for the receiver's row identified by the <LVITEM>, lvitem."

	"N.B. This is a callback request from the ListView's paint handler so setting an
	unconditional breakpoint in here may bring your image to its knees as the LV repeatedly
	attempts to paint a damaged region."

	"Implementation Note: If in report mode then the task of supplying the text/images is
	delegated to the particular column, otherwise the valuables local to the receiver are used.
	This may seem inconsistent, but it allows different text/images to be displayed for the
	primary column if the application requires that the view be dynamically switchable between
	#report mode and the other modes."

	"cdemers 11/23/2016 Handle ANSI text in a Unicode view by converting if needed. "

	| rowObject mask column columnIdx str|
	rowObject := self objectFromHandle: lvitem handle ifAbsent: [UnknownItem].
	"List sometimes asks for lvitem we no longer have, answer nil to accept default processing"
	rowObject == UnknownItem ifTrue: [^nil].
	self isReportMode 
		ifTrue: 
			[columnIdx := lvitem iSubItem + 1.
			column := self columnAtIndex: columnIdx].
	mask := lvitem mask.

	"Image Request?"
	(mask allMask: LVIF_IMAGE) 
		ifTrue: 
			[| imgIdx |
			imgIdx := ((column notNil and: [self hasColumnImages]) ifTrue: [column] ifFalse: [self]) 
						imageFromRow: rowObject.
			imgIdx notNil ifTrue: [lvitem image: imgIdx - 1]].

	"Text request?"
	(mask allMask: LVIF_TEXT) 
		ifTrue: 
			["If in report mode the column's get text block is used unless the request
			 is for the primary column and its text block is nil, in which case the view
			 level block is used"
			str := (((column notNil and: [columnIdx > 1 or: [column getTextBlock notNil]]) 
						ifTrue: [column]
						ifFalse: [self]) textFromRow: rowObject).
			self isUnicodeView ifTrue: [str  := str asUnicodeString].
			lvitem 
				textInBuffer: str].
	(mask allMask: LVIF_INDENT) 
		ifTrue: 
			["Indenting is only supported for the whole row, not on a per-column basis"
			lvitem indent: (self indentFromRow: rowObject)].
	^0	"suppress default processing"! !
!ListView categoriesFor: #lvmGetItem:!accessing!private! !
!ListView categoriesFor: #lvmGetStringWidth:!accessing!private! !
!ListView categoriesFor: #lvmSetColumn:at:!columns!private! !
!ListView categoriesFor: #lvmSetItem:!accessing!private! !
!ListView categoriesFor: #onDisplayDetailsRequired:!event handling!private! !

!ListView class methodsFor!

initializeNotificationMap
	"Private - Initialise the map of ListView notification codes to selector/parameter-class pairs.
	N.B. This method must not be stripped in order to ensure that the notification event handler
	methods (which are looked up in the table) are preserved.
	"

	"Implementation Note: Use an Array for best lookup performance since the notification
	nodes are in a contiguous range, even if it is a little sparse"

	| lvnMap |
	lvnMap := (Array new: 78)
				at: LVN_FIRST - LVN_ITEMCHANGING + 1 put: #lvnItemChanging:;
				at: LVN_FIRST - LVN_ITEMCHANGED + 1 put: #lvnItemChanged:;
				at: LVN_FIRST - LVN_INSERTITEM + 1 put: #lvnInsertItem:;
				at: LVN_FIRST - LVN_DELETEITEM + 1 put: #nmDeleteItem:;
				at: LVN_FIRST - LVN_DELETEALLITEMS + 1 put: #lvnDeleteAllItems:;
				at: LVN_FIRST - LVN_BEGINLABELEDITA + 1 put: #nmBeginLabelEdit:;
				at: LVN_FIRST - LVN_ENDLABELEDITA + 1 put: #nmEndLabelEdit:;
				at: LVN_FIRST - LVN_COLUMNCLICK + 1 put: #lvnColumnClick:;
				at: LVN_FIRST - LVN_BEGINDRAG + 1 put: #nmBeginDrag:;
				at: LVN_FIRST - LVN_BEGINRDRAG + 1 put: #nmBeginRDrag:;
				at: LVN_FIRST - LVN_ODCACHEHINT + 1 put: #nmDummy:;
				at: LVN_FIRST - LVN_ITEMACTIVATE + 1 put: #lvnItemActivate:;
				at: LVN_FIRST - LVN_ODSTATECHANGED + 1 put: #lvnODStateChanged:;
				at: LVN_FIRST - LVN_HOTTRACK + 1 put: #nmDummy:;
				at: LVN_FIRST - LVN_GETDISPINFOA + 1 put: #nmGetDispInfo:;
				at: LVN_FIRST - LVN_GETDISPINFOW + 1 put: #nmGetDispInfo:; "Added for Unicode"
				at: LVN_FIRST - LVN_SETDISPINFOA + 1 put: #nmSetDispInfo:;
				at: LVN_FIRST - LVN_ODFINDITEMA + 1 put: #lvnFindItem:;
				at: LVN_FIRST - LVN_KEYDOWN + 1 put: #nmKeyDown:;
				at: LVN_FIRST - LVN_MARQUEEBEGIN + 1 put: #lvnMarqueeBegin:;
				at: LVN_FIRST - LVN_GETINFOTIPA + 1 put: #lvnGetInfoTip:;
				yourself.
	lvnMap := lvnMap collect: [:each | each isNil ifTrue: [#nmDummy:] ifFalse: [each]].
	self addClassConstant: 'LvnMap' value: lvnMap
! !
!ListView class categoriesFor: #initializeNotificationMap!initializing!must not strip!private! !

!Menu methodsFor!

userLibrary

	"Private - cdemers 10/26/2016"
	"cdemers 11/23/2016 Revised to use the class of self text to determin the UserLibrary to use."  
	^((self text class == UnicodeString ) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default! !
!Menu categoriesFor: #userLibrary!private! !

!MenuItem methodsFor!

userLibrary

	"Private - cdemers 10/26/2016"
	"cdemers 11/23/2016 Revised to use the class of self text to determin the UserLibrary to use."  
	^((self text class == UnicodeString ) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default! !
!MenuItem categoriesFor: #userLibrary!private!realizing/unrealizing! !

!Presenter class methodsFor!

isUnicodeMode

	"cdemers 12/2/2016 - Provide an event hook for translation managers to devide if we should open in Unicode mode."
	| boolValue |

	boolValue := self -> false.
	self trigger: #isUnicodeRequested: with: boolValue.
	^boolValue value!

loadViewResource: aString inContext: aParentView 
	"cdemers 10/20/2016 Use a creation block to set Unicode status."

	| resourceIdentifier  newView |

	self isUnicodeMode 
		ifTrue: [resourceIdentifier := ResourceIdentifier class: self name: aString.
			ShellView createHookBlock: 
					[:shellView :createBlock | 
					"| exStyle |
					exStyle := shellView extendedStyle."
					shellView isUnicodeView: true.
					createBlock value.
					"shellView extendedStyle: exStyle"].
			[newView  := resourceIdentifier loadWithContext: aParentView] ensure: 
				[ShellView createHookBlock: nil].
			^newView]
		ifFalse: [resourceIdentifier := ResourceIdentifier class: self name: aString.
			^resourceIdentifier loadWithContext: aParentView]! !
!Presenter class categoriesFor: #isUnicodeMode!public! !
!Presenter class categoriesFor: #loadViewResource:inContext:!public! !

!Prompter class methodsFor!

isUnicodeMode

	"cdemers 11/30/2016 decide the unicode type based on the active window."
	^View active isUnicodeView. ! !
!Prompter class categoriesFor: #isUnicodeMode!public! !

!SymbolStringSearchPolicy methodsFor!

compare: operand1 with: operand2
	"Answer whether the <Object>, operand1, is considered equivalent to the <Object> argument,
	operand2, by this search policy."
	"cdemers 11/23/2016 Revised for Unicode support.  This may be innefficient."

	^(operand1 asString trueCompare: operand2 asString) == 0
!

hash: operand
	"Answer a suitable hash value for the <Object>, operand, under this search policy."
	"cdemers 11/23/2016 Revised to accomodate UnicodeStrings"

	^operand hash! !
!SymbolStringSearchPolicy categoriesFor: #compare:with:!comparing!public! !
!SymbolStringSearchPolicy categoriesFor: #hash:!comparing!public! !

!TabView methodsFor!

getItem: aninteger 
	| tcItem |

	"cdemers 12/5/2016 Revised for Unicode support."
	tcItem := TCITEM new.
	tcItem
		text: (self stringClass newFixed: 256);
		maskIn: tcItem imageValidMask.
	self 
		sendMessage: (self isUnicodeView ifTrue: [TCM_GETITEM-5+60] ifFalse: [TCM_GETITEM  ])
		wParam: aninteger - 1
		lpParam: tcItem asParameter.
	^tcItem!

tcmInsertItem: tcItem atOffset: anInteger
	"Private - Insert a tab defined by tcItem to the receiver at offset anInteger."

	"cdemers 12/5/2016 Revised for Unicode support."

	^self sendMessage: ((tcItem text isKindOf: UnicodeString) ifTrue: [TCM_INSERTITEM -7+62] ifFalse: [TCM_INSERTITEM]) 
		wParam: anInteger lpParam: tcItem asParameter!

updateItem: anObject atIndex: aninteger 
	"Re-render the specified item, which is at the specified <integer> index in the list."

	"cdemers 12/5/2016 Revised for Unicdode."

	| tcItem |
	tcItem := self makeTcItemStruct: anObject.
	self 
		sendMessage: ((tcItem text isKindOf: UnicodeString) ifTrue: [TCM_SETITEM -6+61] ifFalse: [TCM_SETITEM])
		wParam: aninteger - 1
		lpParam: tcItem asParameter! !
!TabView categoriesFor: #getItem:!helpers!private! !
!TabView categoriesFor: #tcmInsertItem:atOffset:!adding!private! !
!TabView categoriesFor: #updateItem:atIndex:!event handling!public! !

!UnicodeCharacter methodsFor!

isSeparator
	"cdemers 11/30/2016 Taken and revised from Character.  Not sure if there may be a broader meaning in Unicode."
	"Answer whether the receiver is a separator character (i.e. whitespace)."

	^self codePoint == 32 or: [self codePoint >= 9 and: [self codePoint <= 13]]! !
!UnicodeCharacter categoriesFor: #isSeparator!public! !

!UnicodeString methodsFor!

_collate: comparand
	"Private - Answer the receiver's <integer> collation order with respect to 
	the <readableString> argument, comparand. The answer is < 0 if
	the receiver is lexically before the argument, 0 if lexically equivalent, or
	> 0 if lexically after the argument. The comparision is CASE INSENSITIVE.
	The comparison respects the currently configured default locale of the
	host operating system, and the performance may disappoint in some cases."

	"cdemers 12/9/2016 Added for Unicode support."

	^KernelLibrary default lstrcmpiW: self lpString2: comparand asUnicodeString

	"If you really don't care about Locale sensitive collation, then this is
	even faster (C collation), especially if the C locale is set."

"	^CRTLibrary default _stricmp: self string2: comparand."
!

displayString
	"Answer a String representation of the receiver in a form suitable for
	presentation to an end user.
	Implementation Note: This is implemented purely for performance reasons to
	avoid the Stream overhead when displaying strings because it is such a
	common operation."

	"cdemers 2017/11/10 Revised to return self due to Unicode UI support."

	^self!

includes: target
	"Answer whether the argument, target, is one of the elements of the receiver.
	Implementation Note: Override superclass to provide a more efficient implementation."
	"cdemers 12/7/2016 Added to support Unicode, use equality since UnicodeCharacters are not based on identity."

	1 to: self size do: [:i | target = (self at: i) ifTrue: [^true]].
	^false!

inspectorClass
	
	^super inspectorClass
	"We may want to use a unicode inspector, but this isn't working well enough yet."
	"^FlipperInspectorW"!

show
	"cdemers 11/17/2016 Show self in a Unicode MessageBox.  This is just for debugging, as a quick way of seeing a UnicodeString 
		as a Unicode UI would render it." 
	UserLibraryW default messageBoxW: View desktop asParameter lpText: self lpCaption: 'UnicodeString' asUnicodeString uType: 0!

trueCompare: comparand
	"Private - Answer the receiver's <integer> collation order with respect to 
	the <readableString> argument, comparand. The answer is < 0 if
	the receiver is lexically before the argument, 0 if lexically equivalent, or
	> 0 if lexically after the argument. The comparision is CASE SENSITIVE.
	Implementation Note: lstrcmp() is used, which is a word based comparison
	which keeps, for example, hyphenated words together with equivalent
	non-hyphenated words. This is useful, but slower than a basic string collation.
	Also the comparison respects the currently configured default locale of the
	host operating system, and the performance may disappoint in some cases."

	"cdemers 12/9/2016 Added for Unicode support"
	"The primitive simply invokes this OS case sensitive string collation function"
	^KernelLibrary default lstrcmpW: self lpString2: comparand asUnicodeString! !
!UnicodeString categoriesFor: #_collate:!private! !
!UnicodeString categoriesFor: #displayString!printing!public! !
!UnicodeString categoriesFor: #includes:!public! !
!UnicodeString categoriesFor: #inspectorClass!public! !
!UnicodeString categoriesFor: #show!public! !
!UnicodeString categoriesFor: #trueCompare:!comparing!private! !

!UnicodeString class methodsFor!

newTest
	
	| ucStrByteArray |

	ucStrByteArray := #[217 143 47 102 0 78 42 78 75 109 213 139]. " 'This is a test' - Google Translated to Chinese (simplified)."
	^self fromAddress: ucStrByteArray yourAddress length: ucStrByteArray size / 2 .! !
!UnicodeString class categoriesFor: #newTest!public! !

!UserLibrary methodsFor!

isWindowUnicode: hWnd
	"Answer whether the window is a native unicode window"

	<stdcall: bool IsWindowUnicode  handle>
	^self invalidCall ! !
!UserLibrary categoriesFor: #isWindowUnicode:!public! !

!View methodsFor!

basicCreateAt: position extent: extentPoint 
	"Private - Create the Win32 window for the receiver, and answer its handle.
	N.B. The window may not be properly subclassed - use #createAt:extent: instead."

	| dwStyle |
	dwStyle := self baseCreationStyle.
	^self userLibrary 
		createWindowEx: self extendedStyle
		lpClassName: (self isUnicodeView ifTrue: [self class winClassNameW] ifFalse: [self class winClassName])
		lpWindowName: self windowName
		dwStyle: dwStyle
		x: position x
		y: position y
		nWidth: extentPoint x
		nHeight: extentPoint y
		hWndParent: self creationParentView asParameter
		hMenu: ((dwStyle anyMask: WS_CHILD) ifTrue: [self defaultId])
		hInstance: VMLibrary default applicationHandle
		lpParam: nil!

isUnicodeView

	"cdemers 10/19/2016  "
	^flags anyMask: UnicodeMask!

isUnicodeView: aBoolean

	"cdemers 10/19/2016  "
	flags := flags mask: UnicodeMask set: aBoolean!

parentView: aView 
	"Private - Sets the parent of the receiver to aView. Can only be used when the
	receiver is not yet realized since Windows has a bug when reparenting"

	creationParent := aView.
	"cdemers 10/20/2016 Inherit the unicode status from our parent, except when the parent is the DesktopView."
	"cdemers 1/4/2017 We need to avoid causing a recursion too deep by setting isUnicodeView: on non Dolphin views.
		The TabViewXP asks for the curren focus view, if it is a non-Dolphin view then a view is created.  Normally that is fine,
		but isUnicodeView: causes it to try to traverse its children and is causing a recursion."
	(creationParent  class ~~ DesktopView and: [creationParent isDolphinWindow]) ifTrue: [self isUnicodeView:  creationParent isUnicodeView].
	creationStyle at: 1 put: (self baseStyle mask: WS_CHILD set: true)!

stringClass

	"cdemers 10/19/2016 "
	^self isUnicodeView ifTrue: [UnicodeString] ifFalse: [String]!

userLibrary

	"cdemers 10/19/2016 Retrun either the UserLibrary with either ANSI or Unicode support."
	^(self isUnicodeView
		ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default!

userLibraryForString: aStringOrUnicodeString

	"Private - cdemers 10/28/2016"
	^((aStringOrUnicodeString class == UnicodeString ) ifTrue: [UserLibraryW] ifFalse: [UserLibrary]) default! !
!View categoriesFor: #basicCreateAt:extent:!private!realizing/unrealizing! !
!View categoriesFor: #isUnicodeView!public! !
!View categoriesFor: #isUnicodeView:!public! !
!View categoriesFor: #parentView:!hierarchy!private! !
!View categoriesFor: #stringClass!public! !
!View categoriesFor: #userLibrary!public! !
!View categoriesFor: #userLibraryForString:!private! !

!View class methodsFor!

onStartup
	"Perform post startup processing to initialize the View system"

	WndClassAtom := nil.
	WndClassAtomW := nil.

	self allSubclassesDo: [:c |
		(c class includesSelector: #onStartup) ifTrue: [c onStartup]].

	"Load the Common Control Library."
	CommCtrlLibrary openDefault.

	"We start some distance away from the standard Windows IDs for command buttons
	such as IDOK, etc, to avoid any clashes now or in future."
	NextId := 4096.

	"Register a hot key so that Ctrl+Break does not cancel dialogs (closes user interrupt walkback)"
	UserLibrary default registerHotKey: nil id: 0 fsModifiers: MOD_CONTROL vk: VK_CANCEL.

	ThemeLibrary default onStartup!

registerClassW
	"Private - Register the receivers corresponding Win32 class - answer the class atom.
	Will work for subclasses that implement at least #winClassName, and 
	optionally #winClassStyle and #winClassBrush"

	| atom classStruct |
	classStruct := (WNDCLASSW new)
				className: self wndClassNameW;
				lpfnWndProc: VMLibrary default getWndProc;
				hInstance: VMLibrary default applicationHandle;
				style: self winClassStyle;
				hbrBackground: self winClassBrush asParameter;
				hIcon: self winClassIcon asParameter;
				hCursor: self winClassCursor asParameter;
				yourself.
	atom := UserLibraryW default registerClass: classStruct asParameter.
	^atom == 0 ifTrue: [UserLibrary default systemError] ifFalse: [atom]!

reregisterClassW
	"Private - Un-register then re-register receivers corresponding Win32 class."

	^self
		unregisterClassW;
		registerClassW
!

unregisterClassW
	"Private - Unregister the receivers corresponding Win32 class - answer whether it succeeds."

	^UserLibraryW default 
		unregisterClass: self wndClassName 
		hInstance: VMLibrary default applicationHandle!

winClassNameW
	"Private - Answer the Windows class name, or atom, to be used when creating Windows attached
	to instances of the receiver."

	"Use the string class name if one is set to allow for subclasses."
	self winClassName isInteger ifFalse: [^self winClassName asUnicodeString].
	WndClassAtomW isNil ifTrue: [WndClassAtomW := self reregisterClassW].
	^WndClassAtomW!

wndClassNameW
	"Private - Answer the Windows class name to be registered with Win32 for instances of the receiver.
	Typically it is not necessary to override this message when creating custom views, the exceptional
	cases being when one wants different attributes associated with the Window class than the Dolphin
	defaults."

	^'DolphinWindowW'! !
!View class categoriesFor: #onStartup!event handling!public! !
!View class categoriesFor: #registerClassW!operations!private! !
!View class categoriesFor: #reregisterClassW!operations!private! !
!View class categoriesFor: #unregisterClassW!operations!private! !
!View class categoriesFor: #winClassNameW!constants!private! !
!View class categoriesFor: #wndClassNameW!constants!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

MitSciUnicodeAPIScanner guid: (GUID fromString: '{181212e1-1b6b-4bb9-bc89-ebde93814724}')!
MitSciUnicodeAPIScanner comment: '
	muaScan := MitSciUnicodeAPIScanner new.
	muaScan scanForAnsiAPIFunctions.
	muaScan ansiFunctionInfo.
	muaScan ansiFunctionArgumentTypes.'!
!MitSciUnicodeAPIScanner categoriesForClass!Kernel-Objects! !
!MitSciUnicodeAPIScanner methodsFor!

ansiAPIFunctionMethods
	^ansiAPIFunctionMethods!

ansiAPIFunctionMethods: anObject
	ansiAPIFunctionMethods := anObject!

ansiFunctionArgumentTypes
	
	| argTypeSet |

	argTypeSet := Set new.
	self ansiFunctionInfo do: [:each | argTypeSet addAll: each last].
	^argTypeSet asSortedCollection.!

ansiFunctionInfo

	^ansiAPIFunctionMethods collect: [:each | Array with: each methodClass fileName with: each functionName with: each argumentTypes  ].!

scanForAnsiAPIFunctions

	ansiAPIFunctionMethods := OrderedCollection new.
	ExternalLibrary allSubclasses do: [:eachClass | 
		ansiAPIFunctionMethods addAll: (eachClass methodDictionary select: [:eachMethod |
		eachMethod isExternalCall and: [eachMethod functionName last = $A]])].
! !
!MitSciUnicodeAPIScanner categoriesFor: #ansiAPIFunctionMethods!accessing!private! !
!MitSciUnicodeAPIScanner categoriesFor: #ansiAPIFunctionMethods:!accessing!private! !
!MitSciUnicodeAPIScanner categoriesFor: #ansiFunctionArgumentTypes!public! !
!MitSciUnicodeAPIScanner categoriesFor: #ansiFunctionInfo!public! !
!MitSciUnicodeAPIScanner categoriesFor: #scanForAnsiAPIFunctions!public! !

ODBCLibraryW guid: (GUID fromString: '{24994228-aaf3-4f80-8c83-529c9bb8bcdd}')!
ODBCLibraryW comment: ''!
!ODBCLibraryW categoriesForClass!External-Libraries-ODBC! !
!ODBCLibraryW methodsFor!

sqlBrowseConnect: anExternalHandle inConnectionString: aString stringLength1: anIntegerLen outConnectionString: anExternalBuffer bufferLength: anIntegerMax stringLength2Ptr: anIntegerParm

^self sqlBrowseConnectW: anExternalHandle inConnectionString: aString stringLength1: anIntegerLen outConnectionString: anExternalBuffer bufferLength: anIntegerMax stringLength2Ptr: anIntegerParm!

sqlBrowseConnectW: anExternalHandle inConnectionString: aString stringLength1: anIntegerLen outConnectionString: anExternalBuffer bufferLength: anIntegerMax stringLength2Ptr: anIntegerParm 
	"For enumerating attributes of a data source.

	SQLRETURN SQLBrowseConnectW(
		SQLHDBC     ConnectionHandle,
		SQLCHAR *     InConnectionString,
		SQLSMALLINT     StringLength1,
		SQLCHAR *     OutConnectionString,
		SQLSMALLINT     BufferLength,
		SQLSMALLINT *     StringLength2Ptr);"

	<stdcall: sword SQLBrowseConnectW handle lpstr sword lpstr sword SWORD*>
	^self invalidCall!

sqlColAttribute: anExternalHandle columnNumber: columnInteger fieldIdentifier: idInteger characterAttributePtr: bytes bufferLength: sizeInteger stringLengthPtr: anSWORD numericAttributePtr: anExternalAddress

^self sqlColAttributeW: anExternalHandle columnNumber: columnInteger fieldIdentifier: idInteger characterAttributePtr: bytes bufferLength: sizeInteger stringLengthPtr: anSWORD numericAttributePtr: anExternalAddress!

sqlColAttributeW: anExternalHandle columnNumber: columnInteger fieldIdentifier: idInteger characterAttributePtr: bytes bufferLength: sizeInteger stringLengthPtr: anSWORD numericAttributePtr: anExternalAddress 
	<stdcall: sword SQLColAttributeW handle word word lpvoid sword SWORD* lpvoid>
	^self invalidCall!

sqlColumns: anExternalHandleSTMT catalogName: aStringParmQual nameLength1: anIntegerQualLen schemaName: aStringParmOwner nameLength2: anIntegerOwnerLen tableName: aStringParmName nameLength3: anIntegerNameLen columnName: aStringParmType nameLength4: anIntegerTypeLen

^self sqlColumnsW: anExternalHandleSTMT catalogName: aStringParmQual nameLength1: anIntegerQualLen schemaName: aStringParmOwner nameLength2: anIntegerOwnerLen tableName: aStringParmName nameLength3: anIntegerNameLen columnName: aStringParmType nameLength4: anIntegerTypeLen!

sqlColumnsW: anExternalHandleSTMT catalogName: aStringParmQual nameLength1: anIntegerQualLen schemaName: aStringParmOwner nameLength2: anIntegerOwnerLen tableName: aStringParmName nameLength3: anIntegerNameLen columnName: aStringParmType nameLength4: anIntegerTypeLen 
	"SQLRETURN SQLColumnsW(
		SQLHSTMT     StatementHandle,
		SQLCHAR *     CatalogName,
		SQLSMALLINT     NameLength1,
		SQLCHAR *     SchemaName,
		SQLSMALLINT     NameLength2,
		SQLCHAR *     TableName,
		SQLSMALLINT     NameLength3,
		SQLCHAR *     ColumnName,
		SQLSMALLINT     NameLength4);"

	<stdcall: sword SQLColumnsW handle lpstr sword lpstr sword lpstr sword lpstr sword>
	^self invalidCall!

sqlConnect: anExternalHandle serverName: dsnString nameLength1: dsnInteger userName: uidString nameLength2: uidIInteger authentication: authString nameLength3: authInteger

^self sqlConnectW: anExternalHandle serverName: dsnString nameLength1: dsnInteger userName: uidString nameLength2: uidIInteger authentication: authString nameLength3: authInteger!

sqlConnectW: anExternalHandle serverName: dsnString nameLength1: dsnInteger userName: uidString nameLength2: uidIInteger authentication: authString nameLength3: authInteger 
	"Load driver and establish connection to a data source.

	SQLRETURN SQLConnectW(
		SQLHDBC ConnectionHandle,
		SQLCHAR * ServerName,
		SQLSMALLINT NameLength1,
		SQLCHAR * UserName,
		SQLSMALLINT NameLength2,
		SQLCHAR * Authentication,
		SQLSMALLINT NameLength3);"

	<stdcall: sword SQLConnectW handle lpstr sword lpstr sword lpstr sword>
	^self invalidCall!

sqlDataSources: anExternalHandle direction: anIntegerConstant serverName: anExternalBuffer bufferLength1: anIntegerMax nameLength1Ptr: anIntegerParmLen description: anExternalBufferParm bufferLength2: anIntegerParmDMax nameLength2Ptr: anIntegerParmDLen

^self sqlDataSourcesW: anExternalHandle direction: anIntegerConstant serverName: anExternalBuffer bufferLength1: anIntegerMax nameLength1Ptr: anIntegerParmLen description: anExternalBufferParm bufferLength2: anIntegerParmDMax nameLength2Ptr: anIntegerParmDLen!

sqlDataSourcesW: anExternalHandle direction: anIntegerConstant serverName: anExternalBuffer bufferLength1: anIntegerMax nameLength1Ptr: anIntegerParmLen description: anExternalBufferParm bufferLength2: anIntegerParmDMax nameLength2Ptr: anIntegerParmDLen 
	"SQLRETURN SQLDataSourcesW(
		SQLHENV     EnvironmentHandle,
		SQLUSMALLINT     Direction,
		SQLCHAR *     ServerName,
		SQLSMALLINT     BufferLength1,
		SQLSMALLINT *     NameLength1Ptr,
		SQLCHAR *     Description,
		SQLSMALLINT     BufferLength2,
		SQLSMALLINT *     NameLength2Ptr);

	    Enumerates data source names.
	    Max length of szDSN (cbDSNMax) need not be longer than
	    SQL_MAX_DSN_LENGTH+1)."

	<stdcall: sword SQLDataSourcesW handle word lpstr sword SWORD* lpstr sword SWORD*>
	^self invalidCall!

sqlDescribeCol: anExternalHandle columnNumber: columnInteger columnName: aString bufferLength: sizeInteger nameLengthPtr: maxSWORD dataTypePtr: typeSWORD columnSizePtr: precisionDWORD decimalDigitsPtr: scaleSWORD nullablePtr: nullableSWORD

^self sqlDescribeColW: anExternalHandle columnNumber: columnInteger columnName: aString bufferLength: sizeInteger nameLengthPtr: maxSWORD dataTypePtr: typeSWORD columnSizePtr: precisionDWORD decimalDigitsPtr: scaleSWORD nullablePtr: nullableSWORD!

sqlDescribeColW: anExternalHandle columnNumber: columnInteger columnName: aString bufferLength: sizeInteger nameLengthPtr: maxSWORD dataTypePtr: typeSWORD columnSizePtr: precisionDWORD decimalDigitsPtr: scaleSWORD nullablePtr: nullableSWORD 
	"SQLRETURN SQLDescribeColW(
		SQLHSTMT     StatementHandle,
		SQLSMALLINT     ColumnNumber,
		SQLCHAR *     ColumnName,
		SQLSMALLINT     BufferLength,
		SQLSMALLINT *     NameLengthPtr,
		SQLSMALLINT *     DataTypePtr,
		SQLUINTEGER *     ColumnSizePtr,
		SQLSMALLINT *     DecimalDigitsPtr,
		SQLSMALLINT *     NullablePtr);"

	<stdcall: sword SQLDescribeColW handle sword lpstr sword SWORD* SWORD* DWORD* SWORD* SWORD*>
	^self invalidCall!

sqlDriverConnect: anExternalHandle windowHandle: aWinHandle inConnectionString: aStringConn stringLength1: anIntegerConn outConnectionString: aStringConnOut bufferLength: anIntegerConnOut stringLength2Ptr: anExternalShort driverCompletion: anIntegerOption

^self sqlDriverConnectW: anExternalHandle windowHandle: aWinHandle inConnectionString: aStringConn stringLength1: anIntegerConn outConnectionString: aStringConnOut bufferLength: anIntegerConnOut stringLength2Ptr: anExternalShort driverCompletion: anIntegerOption!

sqlDriverConnectW: anExternalHandle windowHandle: aWinHandle inConnectionString: aStringConn stringLength1: anIntegerConn outConnectionString: aStringConnOut bufferLength: anIntegerConnOut stringLength2Ptr: anExternalShort driverCompletion: anIntegerOption 
	"Load driver and establish connection to a data source.

	SQLRETURN SQLDriverConnectW(
		SQLHDBC     ConnectionHandle,
		SQLHWND     WindowHandle,
		SQLCHAR *     InConnectionString,
		SQLSMALLINT     StringLength1,
		SQLCHAR *     OutConnectionString,
		SQLSMALLINT     BufferLength,
		SQLSMALLINT *     StringLength2Ptr,
		SQLUSMALLINT     DriverCompletion);"

	<stdcall: sword SQLDriverConnectW handle handle lpstr sword lpstr sword SWORD* word>
	^self invalidCall!

sqlError: anExternalHandleENV hdbc: anExternalHandleDBC hstmt: anExternalHandleSTMT szSqlState: anSQLStateParm pfNativeError: anIntegerParmNErr szErrorMsg: anExternalBufferErrMsg cbErrorMsgMax: anInteger pcbErrorMsg: anIntegerParmLen

^self sqlErrorW: anExternalHandleENV hdbc: anExternalHandleDBC hstmt: anExternalHandleSTMT szSqlState: anSQLStateParm pfNativeError: anIntegerParmNErr szErrorMsg: anExternalBufferErrMsg cbErrorMsgMax: anInteger pcbErrorMsg: anIntegerParmLen!

sqlErrorW: anExternalHandleENV hdbc: anExternalHandleDBC hstmt: anExternalHandleSTMT szSqlState: anSQLStateParm pfNativeError: anIntegerParmNErr szErrorMsg: anExternalBufferErrMsg cbErrorMsgMax: anInteger pcbErrorMsg: anIntegerParmLen 
	"RETCODE SQL_API SQLErrorW(
		HENV        henv,
		HDBC        hdbc,
		HSTMT       hstmt,
		UCHAR  FAR *szSqlState,
		SDWORD FAR *pfNativeError,
		UCHAR  FAR *szErrorMsg,
		SWORD       cbErrorMsgMax,
		SWORD  FAR *pcbErrorMsg);

	Get error or status information"

	<stdcall: sword SQLErrorW handle handle handle lpvoid lpvoid lpvoid sword lpvoid>
	#todo.	"This API has been deprecated - replace with SQLGetDiagRec"
	^self invalidCall!

sqlExecDirect: anExternalHandle statementText: aString textLength: anInteger

^self sqlExecDirectW: anExternalHandle statementText: aString textLength: anInteger!

sqlExecDirectW: anExternalHandle statementText: aString textLength: anInteger 
	"SQLRETURN SQLExecDirectW(
		SQLHSTMT StatementHandle,
		SQLCHAR * StatementText,
		SQLINTEGER TextLength);"

	<stdcall: sword SQLExecDirectW handle lpstr sdword>
	^self invalidCall!

sqlForeignKeys: anExternalHandle pkCatalogName: aStringPkQual nameLength1: anIntegerPkQual pkSchemaName: aStringPkOwn nameLength2: anIntegerPkOwn pkTableName: aStringPkName nameLength3: anIntegerPkName fkCatalogName: aStringFkQual nameLength4: anIntegerFkQual fkSchemaName: aStringFkOwn nameLength5: anIntegerFkOwn fkTableName: aStringFkName nameLength6: anIntegerFkName

^self sqlForeignKeysW: anExternalHandle pkCatalogName: aStringPkQual nameLength1: anIntegerPkQual pkSchemaName: aStringPkOwn nameLength2: anIntegerPkOwn pkTableName: aStringPkName nameLength3: anIntegerPkName fkCatalogName: aStringFkQual nameLength4: anIntegerFkQual fkSchemaName: aStringFkOwn nameLength5: anIntegerFkOwn fkTableName: aStringFkName nameLength6: anIntegerFkName!

sqlForeignKeysW: anExternalHandle pkCatalogName: aStringPkQual nameLength1: anIntegerPkQual pkSchemaName: aStringPkOwn nameLength2: anIntegerPkOwn pkTableName: aStringPkName nameLength3: anIntegerPkName fkCatalogName: aStringFkQual nameLength4: anIntegerFkQual fkSchemaName: aStringFkOwn nameLength5: anIntegerFkOwn fkTableName: aStringFkName nameLength6: anIntegerFkName 
	"SQLRETURN SQLForeignKeysW(
		SQLHSTMT     StatementHandle,
		SQLCHAR *     PKCatalogName,
		SQLSMALLINT     NameLength1,
		SQLCHAR *     PKSchemaName,
		SQLSMALLINT     NameLength2,
		SQLCHAR *     PKTableName,
		SQLSMALLINT     NameLength3,
		SQLCHAR *     FKCatalogName,
		SQLSMALLINT     NameLength4,
		SQLCHAR *     FKSchemaName,
		SQLSMALLINT     NameLength5,
		SQLCHAR *     FKTableName,
		SQLSMALLINT     NameLength6);"

	<stdcall: sword SQLForeignKeysW handle lpstr sword lpstr sword lpstr sword lpstr sword lpstr sword lpstr sword>
	^self invalidCall!

sqlGetConnectAttr: anExternalHandle attribute: idInteger valuePtr: anIntegerOrStringOrBytes bufferLength: lengthInteger stringLengthPtr: anSDWORD

^self sqlGetConnectAttrW: anExternalHandle attribute: idInteger valuePtr: anIntegerOrStringOrBytes bufferLength: lengthInteger stringLengthPtr: anSDWORD!

sqlGetConnectAttrW: anExternalHandle attribute: idInteger valuePtr: anIntegerOrStringOrBytes bufferLength: lengthInteger stringLengthPtr: anSDWORD 
	<stdcall: sword SQLGetConnectOptionW handle sdword lpvoid sdword sdword*>
	^self invalidCall!

sqlGetInfo: anExternalHandle infoType: anIntegerType infoValuePtr: bytes bufferLength: anIntegerMax stringLengthPtr: anSWORD

^self sqlGetInfoW: anExternalHandle infoType: anIntegerType infoValuePtr: bytes bufferLength: anIntegerMax stringLengthPtr: anSWORD!

sqlGetInfoW: anExternalHandle infoType: anIntegerType infoValuePtr: bytes bufferLength: anIntegerMax stringLengthPtr: anSWORD 
	"Request specific information about a data source depending on
	fInfoType.

		SQLRETURN SQLGetInfoW(
			SQLHDBC     ConnectionHandle,
			SQLUSMALLINT     InfoType,
			SQLPOINTER     InfoValuePtr,
			SQLSMALLINT     BufferLength,
			SQLSMALLINT *     StringLengthPtr);"

	<stdcall: sword SQLGetInfoW handle word lpvoid sword SWORD*>
	^self invalidCall!

sqlPrepare: anExternalHandle statementText: aString textLength: anInteger

^self sqlPrepareW: anExternalHandle statementText: aString textLength: anInteger!

sqlPrepareW: anExternalHandle statementText: aString textLength: anInteger 
	"SQLRETURN SQLPrepareW(
		SQLHSTMT     StatementHandle,
		SQLCHAR *     StatementText,
		SQLINTEGER     TextLength);"

	<stdcall: sword SQLPrepareW handle lpstr sdword>
	^self invalidCall!

sqlPrimaryKeys: anExternalHandle catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName

^self sqlPrimaryKeysW: anExternalHandle catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName!

sqlPrimaryKeysW: anExternalHandle catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName 
	"SQLRETURN SQLPrimaryKeysW(
		SQLHSTMT     StatementHandle,
		SQLCHAR *     CatalogName,
		SQLSMALLINT     NameLength1,
		SQLCHAR *     SchemaName,
		SQLSMALLINT     NameLength2,
		SQLCHAR *     TableName,
		SQLSMALLINT     NameLength3);"

	<stdcall: sword SQLPrimaryKeysW handle lpstr sword lpstr sword lpstr sword>
	^self invalidCall!

sqlProcedures: anExternalHandle catalogName: aStringParmPQ nameLength1: anIntegerPQ schemaName: aStringParmPO nameLength1: anIntegerPO procName: aStringParmPN nameLength3: anIntegerPN

^self sqlProceduresW: anExternalHandle catalogName: aStringParmPQ nameLength1: anIntegerPQ schemaName: aStringParmPO nameLength1: anIntegerPO procName: aStringParmPN nameLength3: anIntegerPN!

sqlProceduresW: anExternalHandle catalogName: aStringParmPQ nameLength1: anIntegerPQ schemaName: aStringParmPO nameLength1: anIntegerPO procName: aStringParmPN nameLength3: anIntegerPN 
	"SQLRETURN SQLProceduresW(
		SQLHSTMT     StatementHandle,
		SQLCHAR *     CatalogName,
		SQLSMALLINT     NameLength1,
		SQLCHAR *     SchemaName,
		SQLSMALLINT     NameLength2,
		SQLCHAR *     ProcName,
		SQLSMALLINT     NameLength3);"

	<stdcall: sword SQLProceduresW handle lpstr sword lpstr sword lpstr sword>
	^self invalidCall!

sqlSetConnectAttr: anExternalHandle attribute: idInteger valuePtr: bytes stringLength: lengthInteger

^self sqlSetConnectAttrW: anExternalHandle attribute: idInteger valuePtr: bytes stringLength: lengthInteger!

sqlSetConnectAttrW: anExternalHandle attribute: idInteger valuePtr: bytes stringLength: lengthInteger 
	<stdcall: sword SQLSetConnectAttrW handle sdword lpvoid sdword>
	^self invalidCall!

sqlSetStmtAttr: anExternalHandle attribute: anInteger valuePtr: anExternalAddress stringLength: lengthInteger

^self sqlSetStmtAttrW: anExternalHandle attribute: anInteger valuePtr: anExternalAddress stringLength: lengthInteger!

sqlSetStmtAttrW: anExternalHandle attribute: anInteger valuePtr: anExternalAddress stringLength: lengthInteger 
	<stdcall: sword SQLSetStmtAttrW handle sdword lpvoid sdword>
	^self invalidCall!

sqlSpecialColumns: anExternalHandle identifierType: anInteger catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName scope: anIntegerScope nullable: anIntegerNullable

^self sqlSpecialColumnsW: anExternalHandle identifierType: anInteger catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName scope: anIntegerScope nullable: anIntegerNullable!

sqlSpecialColumnsW: anExternalHandle identifierType: anInteger catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName scope: anIntegerScope nullable: anIntegerNullable 
	"SQLRETURN SQLSpecialColumnsW(
		SQLHSTMT     StatementHandle,
		SQLSMALLINT     IdentifierType,
		SQLCHAR *     CatalogName,
		SQLSMALLINT     NameLength1,
		SQLCHAR *     SchemaName,
		SQLSMALLINT     NameLength2,
		SQLCHAR *     TableName,
		SQLSMALLINT     NameLength3,
		SQLSMALLINT     Scope,
		SQLSMALLINT     Nullable);"

	<stdcall: sword SQLSpecialColumnsW handle sword lpstr sword lpstr sword lpstr sword sword sword>
	^self invalidCall!

sqlStatistics: anExternalHandle catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName unique: anIntegerScope reserved: anIntegerNullable

^self sqlStatisticsW: anExternalHandle catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName unique: anIntegerScope reserved: anIntegerNullable!

sqlStatisticsW: anExternalHandle catalogName: aStringQual nameLength1: anIntegerQual schemaName: aStringOwn nameLength2: anIntegerOwn tableName: aStringName nameLength3: anIntegerName unique: anIntegerScope reserved: anIntegerNullable 
	"SQLRETURN SQLStatisticsW(
		SQLHSTMT     StatementHandle,
		SQLCHAR *     CatalogName,
		SQLSMALLINT     NameLength1,
		SQLCHAR *     SchemaName,
		SQLSMALLINT     NameLength2,
		SQLCHAR *     TableName,
		SQLSMALLINT     NameLength3,
		SQLUSMALLINT     Unique,
		SQLUSMALLINT     Reserved);"

	<stdcall: sword SQLStatisticsW handle lpstr sword lpstr sword lpstr sword word word>
	^self invalidCall!

sqlTables: anExternalHandleSTMT szTableQualifier: aStringParmQual cbTableQualifier: anIntegerQualLen szTableOwner: aStringParmOwner cbTableOwner: anIntegerOwnerLen szTableName: aStringParmName cbTableName: anIntegerNameLen szTableType: aStringParmType cbTableType: anIntegerTypeLen

^self sqlTablesW: anExternalHandleSTMT szTableQualifier: aStringParmQual cbTableQualifier: anIntegerQualLen szTableOwner: aStringParmOwner cbTableOwner: anIntegerOwnerLen szTableName: aStringParmName cbTableName: anIntegerNameLen szTableType: aStringParmType cbTableType: anIntegerTypeLen!

sqlTablesW: anExternalHandleSTMT
    szTableQualifier:   aStringParmQual
    cbTableQualifier:   anIntegerQualLen
    szTableOwner:       aStringParmOwner
    cbTableOwner:       anIntegerOwnerLen
    szTableName:        aStringParmName
    cbTableName:        anIntegerNameLen
    szTableType:        aStringParmType
    cbTableType:        anIntegerTypeLen

	"RETCODE SQL_API SQLTablesW(
		HSTMT       hstmt,
		UCHAR  FAR *szTableQualifier,
		SWORD       cbTableQualifier,
		UCHAR  FAR *szTableOwner,
		SWORD       cbTableOwner,
		UCHAR  FAR *szTableName,
		SWORD       cbTableName,
		UCHAR  FAR *szTableType,
		SWORD       cbTableType);"

    <stdcall: sword SQLTablesW handle lpvoid sword lpvoid sword lpvoid sword lpvoid sword>
    ^self invalidCall! !
!ODBCLibraryW categoriesFor: #sqlBrowseConnect:inConnectionString:stringLength1:outConnectionString:bufferLength:stringLength2Ptr:!public! !
!ODBCLibraryW categoriesFor: #sqlBrowseConnectW:inConnectionString:stringLength1:outConnectionString:bufferLength:stringLength2Ptr:!public! !
!ODBCLibraryW categoriesFor: #sqlColAttribute:columnNumber:fieldIdentifier:characterAttributePtr:bufferLength:stringLengthPtr:numericAttributePtr:!public! !
!ODBCLibraryW categoriesFor: #sqlColAttributeW:columnNumber:fieldIdentifier:characterAttributePtr:bufferLength:stringLengthPtr:numericAttributePtr:!public! !
!ODBCLibraryW categoriesFor: #sqlColumns:catalogName:nameLength1:schemaName:nameLength2:tableName:nameLength3:columnName:nameLength4:!public! !
!ODBCLibraryW categoriesFor: #sqlColumnsW:catalogName:nameLength1:schemaName:nameLength2:tableName:nameLength3:columnName:nameLength4:!public! !
!ODBCLibraryW categoriesFor: #sqlConnect:serverName:nameLength1:userName:nameLength2:authentication:nameLength3:!public! !
!ODBCLibraryW categoriesFor: #sqlConnectW:serverName:nameLength1:userName:nameLength2:authentication:nameLength3:!public! !
!ODBCLibraryW categoriesFor: #sqlDataSources:direction:serverName:bufferLength1:nameLength1Ptr:description:bufferLength2:nameLength2Ptr:!public! !
!ODBCLibraryW categoriesFor: #sqlDataSourcesW:direction:serverName:bufferLength1:nameLength1Ptr:description:bufferLength2:nameLength2Ptr:!public! !
!ODBCLibraryW categoriesFor: #sqlDescribeCol:columnNumber:columnName:bufferLength:nameLengthPtr:dataTypePtr:columnSizePtr:decimalDigitsPtr:nullablePtr:!public! !
!ODBCLibraryW categoriesFor: #sqlDescribeColW:columnNumber:columnName:bufferLength:nameLengthPtr:dataTypePtr:columnSizePtr:decimalDigitsPtr:nullablePtr:!public! !
!ODBCLibraryW categoriesFor: #sqlDriverConnect:windowHandle:inConnectionString:stringLength1:outConnectionString:bufferLength:stringLength2Ptr:driverCompletion:!public! !
!ODBCLibraryW categoriesFor: #sqlDriverConnectW:windowHandle:inConnectionString:stringLength1:outConnectionString:bufferLength:stringLength2Ptr:driverCompletion:!public! !
!ODBCLibraryW categoriesFor: #sqlError:hdbc:hstmt:szSqlState:pfNativeError:szErrorMsg:cbErrorMsgMax:pcbErrorMsg:!public! !
!ODBCLibraryW categoriesFor: #sqlErrorW:hdbc:hstmt:szSqlState:pfNativeError:szErrorMsg:cbErrorMsgMax:pcbErrorMsg:!public! !
!ODBCLibraryW categoriesFor: #sqlExecDirect:statementText:textLength:!public! !
!ODBCLibraryW categoriesFor: #sqlExecDirectW:statementText:textLength:!public! !
!ODBCLibraryW categoriesFor: #sqlForeignKeys:pkCatalogName:nameLength1:pkSchemaName:nameLength2:pkTableName:nameLength3:fkCatalogName:nameLength4:fkSchemaName:nameLength5:fkTableName:nameLength6:!public! !
!ODBCLibraryW categoriesFor: #sqlForeignKeysW:pkCatalogName:nameLength1:pkSchemaName:nameLength2:pkTableName:nameLength3:fkCatalogName:nameLength4:fkSchemaName:nameLength5:fkTableName:nameLength6:!public! !
!ODBCLibraryW categoriesFor: #sqlGetConnectAttr:attribute:valuePtr:bufferLength:stringLengthPtr:!public! !
!ODBCLibraryW categoriesFor: #sqlGetConnectAttrW:attribute:valuePtr:bufferLength:stringLengthPtr:!public! !
!ODBCLibraryW categoriesFor: #sqlGetInfo:infoType:infoValuePtr:bufferLength:stringLengthPtr:!public! !
!ODBCLibraryW categoriesFor: #sqlGetInfoW:infoType:infoValuePtr:bufferLength:stringLengthPtr:!public! !
!ODBCLibraryW categoriesFor: #sqlPrepare:statementText:textLength:!public! !
!ODBCLibraryW categoriesFor: #sqlPrepareW:statementText:textLength:!public! !
!ODBCLibraryW categoriesFor: #sqlPrimaryKeys:catalogName:nameLength1:schemaName:nameLength2:tableName:nameLength3:!public! !
!ODBCLibraryW categoriesFor: #sqlPrimaryKeysW:catalogName:nameLength1:schemaName:nameLength2:tableName:nameLength3:!public! !
!ODBCLibraryW categoriesFor: #sqlProcedures:catalogName:nameLength1:schemaName:nameLength1:procName:nameLength3:!public! !
!ODBCLibraryW categoriesFor: #sqlProceduresW:catalogName:nameLength1:schemaName:nameLength1:procName:nameLength3:!public! !
!ODBCLibraryW categoriesFor: #sqlSetConnectAttr:attribute:valuePtr:stringLength:!public! !
!ODBCLibraryW categoriesFor: #sqlSetConnectAttrW:attribute:valuePtr:stringLength:!public! !
!ODBCLibraryW categoriesFor: #sqlSetStmtAttr:attribute:valuePtr:stringLength:!public! !
!ODBCLibraryW categoriesFor: #sqlSetStmtAttrW:attribute:valuePtr:stringLength:!public! !
!ODBCLibraryW categoriesFor: #sqlSpecialColumns:identifierType:catalogName:nameLength1:schemaName:nameLength2:tableName:nameLength3:scope:nullable:!public! !
!ODBCLibraryW categoriesFor: #sqlSpecialColumnsW:identifierType:catalogName:nameLength1:schemaName:nameLength2:tableName:nameLength3:scope:nullable:!public! !
!ODBCLibraryW categoriesFor: #sqlStatistics:catalogName:nameLength1:schemaName:nameLength2:tableName:nameLength3:unique:reserved:!public! !
!ODBCLibraryW categoriesFor: #sqlStatisticsW:catalogName:nameLength1:schemaName:nameLength2:tableName:nameLength3:unique:reserved:!public! !
!ODBCLibraryW categoriesFor: #sqlTables:szTableQualifier:cbTableQualifier:szTableOwner:cbTableOwner:szTableName:cbTableName:szTableType:cbTableType:!public! !
!ODBCLibraryW categoriesFor: #sqlTablesW:szTableQualifier:cbTableQualifier:szTableOwner:cbTableOwner:szTableName:cbTableName:szTableType:cbTableType:!public! !

!ODBCLibraryW class methodsFor!

default

	"cdemers 2016/11/22 Use the same handle as ODBCLibrary so we don't open the libary twice."
	^(default isNil or: [default isOpen not]) ifTrue: [default := self fromHandle: ODBCLibrary default handle] ifFalse: [default]! !
!ODBCLibraryW class categoriesFor: #default!public! !

GDILibraryW guid: (GUID fromString: '{80e1b07e-46ed-40eb-bf92-98d4dd955556}')!
GDILibraryW comment: ''!
!GDILibraryW categoriesForClass!External-Data-Structured-Win32! !
!GDILibraryW methodsFor!

addFontResource: fontFile

^self addFontResourceW: fontFile!

addFontResourceW: fontFile 
	"Adds the font resource from the specified file to the system font table.
	The font can subsequently be used for text output by any application. 

	int AddFontResource(
		LPCTSTR lpszFilename   // font file name
	);"

	<stdcall: sdword AddFontResourceW lpstr>
	^self invalidCall!

copyEnhMetaFile: hemf lpszFile: lpszFile

^self copyEnhMetaFileW: hemf lpszFile: lpszFile!

copyEnhMetaFileW: hemf lpszFile: lpszFile
	"Invoke the CopyEnhMetaFile() function of the module wrapped by the receiver.
	Helpstring: Copies contents of enhanced-format metafile to specified file.

		HENHMETAFILE __stdcall CopyEnhMetaFile(
			HENHMETAFILE hemf,
			LPCSTR lpszFile);"

	<stdcall: handle CopyEnhMetaFileW handle lpstr>
	^self invalidCall!

createDC: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpInitData: lpInitData

^self createDCW: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpInitData: lpInitData!

createDCW: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpInitData: lpInitData
	"Invoke the CreateDC() function of the module wrapped by the receiver.
	Helpstring: Creates a device context (DC) for a device by using the specified name

		HDC __stdcall CreateDC(
			LPCSTR lpszDriver,
			LPCSTR lpszDevice,
			LPCSTR lpszOutput,
			DEVMODE* lpInitData);"

	<stdcall: handle CreateDCW lpstr lpstr lpstr DEVMODE*>
	^self invalidCall!

createEnhMetaFile: hdc lpFileName: lpFileName lpRect: lpRect lpDescription: lpDescription

^self createEnhMetaFileW: hdc lpFileName: lpFileName lpRect: lpRect lpDescription: lpDescription!

createEnhMetaFileW: hdc lpFileName: lpFileName lpRect: lpRect lpDescription: lpDescription
	"Invoke the CreateEnhMetaFile() function of the module wrapped by the receiver.
	Helpstring: Creates device context for enhanced-format metafile.

		HDC __stdcall CreateEnhMetaFile(
			HDC HDC,
			LPCSTR lpFileName,
			int* lpRect,
			LPCSTR lpDescription);"

	<stdcall: handle CreateEnhMetaFileW handle lpstr sdword* lpstr>
	^self invalidCall!

createFontIndirect: lplf

^self createFontIndirectW: lplf!

createFontIndirectW: lplf
	"Invoke the CreateFontIndirect() function of the module wrapped by the receiver.
	Helpstring: Creates a logical font that has the characteristics specified in the specified structure

		HFONT __stdcall CreateFontIndirect(
			LOGFONT* lplf);"

	<stdcall: handle CreateFontIndirectW LOGFONT*>
	^self invalidCall!

createIC: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpdvminit: lpdvminit

^self createICW: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpdvminit: lpdvminit!

createICW: lpszDriver lpszDevice: lpszDevice lpszOutput: lpszOutput lpdvminit: lpdvminit
	"Invoke the CreateIC() function of the module wrapped by the receiver.
	Helpstring: Creates an information context for the specified device

		HDC __stdcall CreateIC(
			LPCSTR lpszDriver,
			LPCSTR lpszDevice,
			LPCSTR lpszOutput,
			DEVMODE* lpdvminit);"

	<stdcall: handle CreateICW lpstr lpstr lpstr DEVMODE*>
	^self invalidCall!

enumFonts: dcHandle lpFaceName: aString lpFontFunc: aCallbackThunk lParam: anInteger

^self enumFontsW: dcHandle lpFaceName: aString lpFontFunc: aCallbackThunk lParam: anInteger!

enumFontsW: dcHandle lpFaceName: aString lpFontFunc: aCallbackThunk lParam: anInteger
	"Enumerate the fonts with the specified face name on the specified device throught the
	specified callback with the specified extra parameter.

		int EnumFonts(
			HDC hdc,					// handle of device context 
			LPCTSTR lpFaceName,		// font typeface name string 
			FONTENUMPROC lpFontFunc,	// callback function 
			LPARAM lParam 				// application-supplied data
		);"

	<stdcall: sdword EnumFontsW handle lpstr lpvoid uintptr>
	^self invalidCall!

extTextOut: hdc x: x y: y fuOptions: fuOptions lprc: lprc lpString: lpString cbCount: cbCount lpDx: lpDx

^self extTextOutW: hdc x: x y: y fuOptions: fuOptions lprc: lprc lpString: lpString cbCount: cbCount lpDx: lpDx!

extTextOutW: hdc x: x y: y fuOptions: fuOptions lprc: lprc lpString: lpString cbCount: cbCount lpDx: lpDx
	"Invoke the ExtTextOut() function of the module wrapped by the receiver.
	Helpstring: Draws a character string, optionally using a rectangle to specify clipping, opaquing, or both

		BOOL __stdcall ExtTextOut(
			HDC HDC,
			int x,
			int y,
			UINT fuOptions,
			RECT* lprc,
			LPCSTR lpString,
			UINT cbCount,
			int* lpDx);"

	<stdcall: bool ExtTextOutW handle sdword sdword dword RECT* lpstr dword sdword*>
	^self invalidCall!

getEnhMetaFile: lpszMetaFile

^self getEnhMetaFileW: lpszMetaFile!

getEnhMetaFileDescription: hemf cchBuffer: cchBuffer lpszDescription: lpszDescription

^self getEnhMetaFileDescriptionW: hemf cchBuffer: cchBuffer lpszDescription: lpszDescription!

getEnhMetaFileDescriptionW: hemf cchBuffer: cchBuffer lpszDescription: lpszDescription
	"Invoke the GetEnhMetaFileDescription() function of the module wrapped by the receiver.
	Helpstring: Copies optional text description from an enhanced-format metafile to a specified buffer.

		UINT __stdcall GetEnhMetaFileDescription(
			HENHMETAFILE hemf,
			UINT cchBuffer,
			LPSTR lpszDescription);"

	<stdcall: dword GetEnhMetaFileDescriptionW handle dword lpstr>
	^self invalidCall!

getEnhMetaFileW: lpszMetaFile
	"Invoke the GetEnhMetaFile() function of the module wrapped by the receiver.
	Helpstring: Creates handle for given file-based enhanced-format metafile

		HENHMETAFILE __stdcall GetEnhMetaFile(
			LPCSTR lpszMetaFile);"

	<stdcall: handle GetEnhMetaFileW lpstr>
	^self invalidCall!

getObject: objectHandle cbBuffer: count lpvObject: buffer

^self getObjectW: objectHandle cbBuffer: count lpvObject: buffer!

getObjectW: objectHandle cbBuffer: count lpvObject: buffer
	"Obtains information about a specified graphics object. Depending on 
	the graphics object, the function places a filled-in BITMAP, DIBSECTION, 
	EXTLOGPEN, LOGBRUSH, LOGFONT, or LOGPEN structure, or a count of table 
	entries (for a logical palette), into a specified buffer. 
		int GetObject(
  			HGDIOBJ hgdiobj,	// handle to graphics object of interest
			int cbBuffer,	// size of buffer for object information 
			LPVOID lpvObject 	// pointer to buffer for object information  
		);"

	<stdcall: sdword GetObjectW handle sdword lpvoid>
	^self invalidCall!

getOutlineTextMetrics: anExternalHandle cbData: anInteger lpotm: anOUTLINETEXTMETRIC

^self getOutlineTextMetricsW: anExternalHandle cbData: anInteger lpotm: anOUTLINETEXTMETRIC!

getOutlineTextMetricsW: anExternalHandle cbData: anInteger lpotm: anOUTLINETEXTMETRIC 
	<stdcall: dword GetOutlineTextMetricsW handle dword OUTLINETEXTMETRIC*>
	^self invalidCall!

getTextExtentPoint32: hdc lpString: lpString cbString: cbString lpSize: lpSize

^self getTextExtentPoint32W: hdc lpString: lpString cbString: cbString lpSize: lpSize!

getTextExtentPoint32W: hdc lpString: lpString cbString: cbString lpSize: lpSize
	"Invoke the GetTextExtentPoint32() function of the module wrapped by the receiver.
	Helpstring: Computes the width and height of the specified string of text

		BOOL __stdcall GetTextExtentPoint32(
			HDC HDC,
			LPCSTR lpString,
			int cbString,
			SIZEL* lpSize);"

	<stdcall: bool GetTextExtentPoint32W handle lpstr sdword SIZE*>
	^self invalidCall!

getTextMetrics: hdc lptm: lptm

^self getTextMetricsW: hdc lptm: lptm!

getTextMetricsW: hdc lptm: lptm
	"Invoke the GetTextMetrics() GDI call to populate a TEXTMETRIC structure, lptm, with
	metrics for the font currently selected into the device context with handle, hdc."

	<stdcall: bool GetTextMetricsW handle TEXTMETRIC*>
	^self invalidCall!

open: aString

	handle  := self class superclass default handle.!

removeFontResource: fontFile

^self removeFontResourceW: fontFile!

removeFontResourceW: fontFile 
	"The RemoveFontResource function removes the fonts in the specified file from the system font table. 
	If the font was added using the AddFontResourceEx function, you must use the RemoveFontResourceEx function. 

	BOOL RemoveFontResource(
		LPCTSTR lpFileName   // name of font file
		);
	"

	<stdcall: bool RemoveFontResourceW lpstr>
	^self invalidCall!

startDoc: hdc lpdi: info

^self startDocW: hdc lpdi: info!

startDocW: hdc lpdi: info
	"Start a print job.
		int StartDoc(
  			HDC hdc,				// handle of device context 
			CONST DOCINFO *lpdi 	// address of structure with file names  
		);"

	<stdcall: sdword StartDocW handle DOCINFO* >
	^self invalidCall

!

textOut: hdc nXStart: nXStart nYStart: nYStart lpString: lpString cbString: cbString

^self textOutW: hdc nXStart: nXStart nYStart: nYStart lpString: lpString cbString: cbString!

textOutW: hdc nXStart: nXStart nYStart: nYStart lpString: lpString cbString: cbString
	"Invoke the TextOut() function of the module wrapped by the receiver.
	Helpstring: Writes a character string at the specified location, using the currently selected font

		BOOL __stdcall TextOut(
			HDC HDC,
			int nXStart,
			int nYStart,
			LPCSTR lpString,
			int cbString);"

	<stdcall: bool TextOutW handle sdword sdword lpstr sdword>
	^self invalidCall! !
!GDILibraryW categoriesFor: #addFontResource:!public! !
!GDILibraryW categoriesFor: #addFontResourceW:!public! !
!GDILibraryW categoriesFor: #copyEnhMetaFile:lpszFile:!public! !
!GDILibraryW categoriesFor: #copyEnhMetaFileW:lpszFile:!public! !
!GDILibraryW categoriesFor: #createDC:lpszDevice:lpszOutput:lpInitData:!public! !
!GDILibraryW categoriesFor: #createDCW:lpszDevice:lpszOutput:lpInitData:!public! !
!GDILibraryW categoriesFor: #createEnhMetaFile:lpFileName:lpRect:lpDescription:!public! !
!GDILibraryW categoriesFor: #createEnhMetaFileW:lpFileName:lpRect:lpDescription:!public! !
!GDILibraryW categoriesFor: #createFontIndirect:!public! !
!GDILibraryW categoriesFor: #createFontIndirectW:!public! !
!GDILibraryW categoriesFor: #createIC:lpszDevice:lpszOutput:lpdvminit:!public! !
!GDILibraryW categoriesFor: #createICW:lpszDevice:lpszOutput:lpdvminit:!public! !
!GDILibraryW categoriesFor: #enumFonts:lpFaceName:lpFontFunc:lParam:!public! !
!GDILibraryW categoriesFor: #enumFontsW:lpFaceName:lpFontFunc:lParam:!public! !
!GDILibraryW categoriesFor: #extTextOut:x:y:fuOptions:lprc:lpString:cbCount:lpDx:!public! !
!GDILibraryW categoriesFor: #extTextOutW:x:y:fuOptions:lprc:lpString:cbCount:lpDx:!public! !
!GDILibraryW categoriesFor: #getEnhMetaFile:!public! !
!GDILibraryW categoriesFor: #getEnhMetaFileDescription:cchBuffer:lpszDescription:!public! !
!GDILibraryW categoriesFor: #getEnhMetaFileDescriptionW:cchBuffer:lpszDescription:!public! !
!GDILibraryW categoriesFor: #getEnhMetaFileW:!public! !
!GDILibraryW categoriesFor: #getObject:cbBuffer:lpvObject:!public! !
!GDILibraryW categoriesFor: #getObjectW:cbBuffer:lpvObject:!public! !
!GDILibraryW categoriesFor: #getOutlineTextMetrics:cbData:lpotm:!public! !
!GDILibraryW categoriesFor: #getOutlineTextMetricsW:cbData:lpotm:!public! !
!GDILibraryW categoriesFor: #getTextExtentPoint32:lpString:cbString:lpSize:!public! !
!GDILibraryW categoriesFor: #getTextExtentPoint32W:lpString:cbString:lpSize:!public! !
!GDILibraryW categoriesFor: #getTextMetrics:lptm:!public! !
!GDILibraryW categoriesFor: #getTextMetricsW:lptm:!public! !
!GDILibraryW categoriesFor: #open:!public! !
!GDILibraryW categoriesFor: #removeFontResource:!public! !
!GDILibraryW categoriesFor: #removeFontResourceW:!public! !
!GDILibraryW categoriesFor: #startDoc:lpdi:!public! !
!GDILibraryW categoriesFor: #startDocW:lpdi:!public! !
!GDILibraryW categoriesFor: #textOut:nXStart:nYStart:lpString:cbString:!public! !
!GDILibraryW categoriesFor: #textOutW:nXStart:nYStart:lpString:cbString:!public! !

UserLibraryW guid: (GUID fromString: '{eefe281e-06e5-4441-b216-28acf93afa3e}')!
UserLibraryW comment: ''!
!UserLibraryW categoriesForClass!External-Libraries-Win32! !
!UserLibraryW methodsFor!

callWindowProc: lpPrevWndFunc hWnd: hWnd msg: msg wParam: wParam lParam: lParam

^self callWindowProcW: lpPrevWndFunc hWnd: hWnd msg: msg wParam: wParam lParam: lParam!

callWindowProcW: lpPrevWndFunc hWnd: hWnd msg: msg wParam: wParam lParam: lParam
	"Pass a message to the specified window procedure. 
		LRESULT CallWindowProc(
  			WNDPROC lpPrevWndFunc,	// pointer to previous procedure
			HWND hWnd,			// handle to window
			UINT Msg,				// message
			WPARAM wParam,			// first message parameter
			LPARAM lParam 			// second message parameter
		);
	N.B. As we don't want to pass the address of a Smalltalk object containing
	the WNDPROC's address to the function, we specify the first parameter
	as being a DWORD as this allows us to pass a wider range of types."

	<stdcall: uintptr CallWindowProcW dword handle dword uintptr uintptr>
	^self invalidCall!

charLower: aCharacter

^self charLowerW: aCharacter!

charLowerW: aCharacter
	"Answer the lowercase equivalent of aCharacter. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharLower(LPTSTR  lpsz); 	// single character or pointer to string"

	<stdcall: char CharLowerW char>
	^self invalidCall!

charUpper: aCharacter

^self charUpperW: aCharacter!

charUpperW: aCharacter
	"Answer the uppercase equivalent of aCharacter. This will be dependent on the semantics 
	of the language selected by the user during setup or by using Control Panel.

		LPTSTR CharUpper(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: char CharUpperW char>
	^self invalidCall!

createAcceleratorTable: pTable cEntries: tableSize

^self createAcceleratorTableW: pTable cEntries: tableSize!

createAcceleratorTableW: pTable cEntries: tableSize
	"Create an accelerator table. 
		HACCEL CreateAcceleratorTable(
  			LPACCEL lpaccl,	// pointer to structure array with accelerator data
			int cEntries 		// number of structures in the array
		);"

	<stdcall: handle CreateAcceleratorTableW lpvoid sdword>
	^self invalidCall!

createDialog: hInstance lpTemplate: template hWndParent: hParent lpDialogFunc: dlgProc dwInitParam: lParam

^self createDialogW: hInstance lpTemplate: template hWndParent: hParent lpDialogFunc: dlgProc dwInitParam: lParam!

createDialogW: hInstance lpTemplate: template hWndParent: hParent lpDialogFunc: dlgProc dwInitParam: lParam
	"Create a modeless dialog box from a dialog box template resource. 
		HWND CreateDialog(
  			HINSTANCE hInstance,		// handle of module containing template
			LPCTSTR lpTemplate,		// resource identifier
			HWND hWndParent,			// handle of owner window
			DLGPROC lpDialogFunc, 		// address of dialog proc
			LPARAM dwInitParam 		// value passed to wmInitDialog:
		);"

	<stdcall: sdword CreateDialogParamW handle lpvoid handle lpvoid sdword>
	^self invalidCall!

createWindowEx: exstyle lpClassName: classname lpWindowName: windowname dwStyle: style x: x y: y nWidth: width nHeight: height hWndParent: parent hMenu: menu hInstance: instance lpParam: lpParam

^self createWindowExW: exstyle lpClassName: classname lpWindowName: windowname dwStyle: style x: x y: y nWidth: width nHeight: height hWndParent: parent hMenu: menu hInstance: instance lpParam: lpParam!

createWindowExW: exstyle lpClassName: classname lpWindowName: windowname dwStyle: style 
		x: x y: y nWidth: width nHeight: height 
		hWndParent: parent hMenu: menu hInstance: instance lpParam: lpParam
	"Creates an overlapped, pop-up, or child window with the specified the parent or owner (if any), class, 
	title, menu, style (and extended style), position and extent. Answer the handle of the
	new window, or nil if the create fails.
	N.B. In order to correctly subclass controls so that Dolphin receive's creation messages
	for those controls, View>>hookWindowCreate should be called before this function.
		HWND CreateWindowEx(
			DWORD dwExStyle,		// extended window style
			LPCTSTR lpClassName,	// pointer to registered class name
			LPCTSTR lpWindowName,	// pointer to window name
			DWORD dwStyle,			// window style
			int x,				// horizontal position of window
			int y,				// vertical position of window
			int nWidth,			// window width
			int nHeight,			// window height
			HWND hWndParent,		// handle to parent or owner window
			HMENU hMenu,			// handle to menu, or child-window identifier
			HINSTANCE hInstance,	// handle to application instance
			LPVOID lpParam 		// pointer to window-creation data
		);"

	<stdcall: handle CreateWindowExW dword lpvoid lpvoid dword sdword sdword sdword sdword handle handle handle lpvoid>
	^self invalidCall
!

defDlgProc: hWnd msg: msg wParam: wParam lParam: lParam

^self defDlgProcW: hWnd msg: msg wParam: wParam lParam: lParam!

defDlgProcW: hWnd msg: msg wParam: wParam lParam: lParam
	"Call the default dialog procedure to provide default processing for 
	any window messages that an application does not process. 

		LRESULT DefDlgProc(
			HWND hDlg,	// handle to dialog box
			UINT Msg,	// message
			WPARAM wParam,	// first message parameter
			LPARAM lParam 	// second message parameter
		);"


	<stdcall: uintptr DefDlgProcW handle dword uintptr uintptr>
	^self invalidCall!

defWindowProc: hWnd msg: msg wParam: wParam lParam: lParam

^self defWindowProcW: hWnd msg: msg wParam: wParam lParam: lParam!

defWindowProcW: hWnd msg: msg wParam: wParam lParam: lParam
	"Call the default window procedure to provide default processing for 
	any window messages that an application does not process. 
	
		LRESULT DefWindowProc(
			HWND hWnd,	// handle to window
			UINT Msg,	// message identifier
			WPARAM wParam,	// first message parameter
			LPARAM lParam 	// second message parameter
		);"

	<stdcall: uintptr DefWindowProcW handle dword uintptr uintptr>
	^self invalidCall!

dispatchMessage: aMSG

^self dispatchMessageW: aMSG!

dispatchMessageW: aMSG
	"Dispatch a message to a window procedure.
		LONG DispatchMessage(
  			CONST MSG *lpmsg 	// pointer to structure with message
		);"

	<stdcall: sdword DispatchMessageW MSG* >
	^self invalidCall!

drawState: hdc hbr: hbr lpOutputFunc: lpOutputFunc lData: lData wData: wData x: x y: y cx: cx cy: cy fuFlags: fuFlags

^self drawStateW: hdc hbr: hbr lpOutputFunc: lpOutputFunc lData: lData wData: wData x: x y: y cx: cx cy: cy fuFlags: fuFlags!

drawStateW: hdc hbr: hbr lpOutputFunc: lpOutputFunc lData: lData wData: wData x: x y: y cx: cx cy: cy fuFlags: fuFlags
	"Invoke the DrawState() function of the module wrapped by the receiver.
	Helpstring: Displays an image and applies a visual effect to indicate a state, such as a disabled or default state

		BOOL __stdcall DrawState(
			[in]HDC HDC,
			[in]HBRUSH hbr,
			[in]DRAWSTATEPROC lpOutputFunc,
			[in]LPARAM lData,
			[in]WPARAM wData,
			[in]int x,
			[in]int y,
			[in]int cx,
			[in]int cy,
			[in]unsigned int fuFlags);"

	<stdcall: bool DrawStateW handle handle void* intptr uintptr sdword sdword sdword sdword dword>
	^self invalidCall!

drawTextEx: hdc lpchText: lpchText cchText: cchText lprc: lprc dwDTFormat: dwDTFormat lpDTParams: lpdtParams

^self drawTextExW: hdc lpchText: lpchText cchText: cchText lprc: lprc dwDTFormat: dwDTFormat lpDTParams: lpdtParams!

drawTextExW: hdc lpchText: lpchText cchText: cchText lprc: lprc dwDTFormat: dwDTFormat lpDTParams: lpdtParams
	"Draw text in the specified rectange with the specified options (see Win32 docs)

		int DrawTextEx(
			HDC hdc,			 						// handle to device context
			LPTSTR lpchText,							// pointer to string to draw 
			int cchText,								// length of string to draw 
			LPRECT lprc,								// pointer to rectangle coordinates 
			UINT dwDTFormat,						// formatting options 
			LPDRAWTEXTPARAMS lpDTParams	// pointer to struct with options 
		);
	" 
	<stdcall: sdword DrawTextExW handle lpstr sdword RECT* dword lpvoid>
	^self invalidCall!

findWindow: lpClassName lpWindowName: lpWindowName

^self findWindowW: lpClassName lpWindowName: lpWindowName!

findWindowW: lpClassName lpWindowName: lpWindowName
	"Find and answer the handle of the top-level window with matching class name and caption."

	<stdcall: handle FindWindowW lpstr lpstr>
	^self invalidCall!

getClassName: aWindowHandle lpClassName: aByteBuffer nMaxCount: anInteger

^self getClassNameW: aWindowHandle lpClassName: aByteBuffer nMaxCount: anInteger!

getClassNameW: aWindowHandle lpClassName: aByteBuffer nMaxCount: anInteger
	"Retrieves the name of the Windows class to which the specified window belongs.
		int GetClassName(
			HWND hWnd,			// handle of window
			LPTSTR lpClassName,	// address of buffer for class name
			int nMaxCount 			// size of buffer, in characters
		);"

	<stdcall: sdword GetClassNameW handle lpvoid sdword>
	^self invalidCall!

getClipboardFormatName: format lpszFormatName: lpszFormatName cchMaxCount: cchMaxCount

^self getClipboardFormatNameW: format lpszFormatName: lpszFormatName cchMaxCount: cchMaxCount!

getClipboardFormatNameW: format lpszFormatName: lpszFormatName cchMaxCount: cchMaxCount
	"Copy the name of the specified registered clipboard format into the
	supplied buffer.

		int GetClipboardFormatName( 
			UINT format,				// clipboard format to retrieve 
			LPTSTR lpszFormatName, 	// address of buffer for name 
			int cchMaxCount  			// length of name string in characters 
		);"

	<stdcall: sdword GetClipboardFormatNameW dword lpstr sdword>
	^self invalidCall!

getKeyNameText: lParam lpString: aString nSize: anInteger

^self getKeyNameTextW: lParam lpString: aString nSize: anInteger!

getKeyNameTextW: lParam lpString: aString nSize: anInteger
	"Populate a string with the name of a virtual key code
		int GetKeyNameText( 
			LONG lParam,	// second parameter of keyboard message	
			LPTSTR lpString,	// address of buffer for key name	
			int nSize 	// maximum length of key-name string length	
		);"

	<stdcall: sdword GetKeyNameTextW dword lpstr sdword>
	^self invalidCall
!

getMessage: aMSG hWnd: aWindowHandle wMsgFilterMin: anIntFilterMin wMsgFilterMax: anIntFilterMax

^self getMessageW: aMSG hWnd: aWindowHandle wMsgFilterMin: anIntFilterMin wMsgFilterMax: anIntFilterMax!

getMessageW: aMSG hWnd: aWindowHandle wMsgFilterMin: anIntFilterMin wMsgFilterMax: anIntFilterMax
	"Retrieves the next message from the Win32 input queue in the range integerFilterMin
	integerFilterMax, for the window with handle aWindowHandle (if Null then for all windows)
	into the Win32 MSG structure, aMSG.

	BOOL GetMessage(
		LPMSG  lpMsg,	// address of structure with message
		HWND  hWnd,	// handle of window
		UINT  wMsgFilterMin,	// first message
		UINT  wMsgFilterMax 	// last message
	);	

	Answers 1 if a message was retrieved, 0 if WM_QUIT was retrieved, or -1 if some
	error occurred. N.B. Though Win32 declares GetMessage() as returning a BOOL,
	it doesn't really (because of the error return value) so to avoid errors, as
	suggested in the help), we have it return a signed integer)"

	<stdcall: sdword GetMessageW MSG* handle dword dword>
	^self invalidCall!

getProp: aWindowHandle lpString: name

^self getPropW: aWindowHandle lpString: name!

getPropW: aWindowHandle lpString: name
	"Answer a data handle which is the previously added (with #setProp:etc) named property 
	of the given window.
		HANDLE GetProp(
			HWND hWnd,	// handle of window
			LPCTSTR lpString 	// atom or address of string
		);.

	N.B. The return specification HANDLE here indicates that the value is an opaque 32-bit
	(DWORD) value, so we use #dword, as we reserve #handle for real handles."

	<stdcall: dword GetPropW handle lpvoid>
	^self invalidCall

!

getWindowLong: aWindowHandle nIndex: zeroBasedIntegerOffset

^self getWindowLongW: aWindowHandle nIndex: zeroBasedIntegerOffset!

getWindowLongPtr: aWindowHandle nIndex: zeroBasedIntegerOffset

^self getWindowLongPtrW: aWindowHandle nIndex: zeroBasedIntegerOffset!

getWindowLongPtrW: aWindowHandle nIndex: zeroBasedIntegerOffset 
	"Answer various signed intptr values retrieved from the window with handle, aWindowHandle.
	If an unsigned value is required, use getWindowULongPtr:nIndex:. Note that this function
	is only supported on 64-bit Windows.

		LONG_PTR GetWindowLongPtr(
			HWND hWnd, // handle of window 
			int nIndex // offset of value to retrieve 
		);

	Valid offsets are in the range 0 to the number of bytes of extra window memory, minus the
	size of an intptr, or one of the following values: 

		GWL_EXSTYLE Extended window styles (WS_EX_XXX) 
		GWL_STYLE Window styles (WS_XXX) 
		GWLP_WNDPROC Address of the Window Procedure
		GWLP_HINSTANCE Handle of the application instance which owns the window. 
		GWLP_HWNDPARENT Handle of the parent window, if any. 
		GWLP_ID Identifier of the window. GWLP_USERDATA An inptr value associated with the window by the owning application.

	The following values are also available for dialog windows: 

		DWLP_DLGPROC Address of the dialog box procedure. 
		DWLP_MSGRESULT The return value of a message processed in the dialog	box procedure. 
		DWLP_USER Extra information private to the application.

	Answers 0 if the function fails."

	<stdcall: intptr GetWindowLongPtrW handle sdword>
	^self invalidCall!

getWindowLongW: aWindowHandle nIndex: zeroBasedIntegerOffset 
	"Answer various signed 32-bit integer values retrieved from the window with handle, aWindowHandle.
	If an unsigned value is required, use getWindowULong:nIndex:

		LONG GetWindowLong(
			HWND hWnd, // handle of window 
			int nIndex // offset of value to retrieve 
		);

	Valid offsets are in the range 0 to the number of bytes of extra window memory, minus the
	size of an int (4), or one of the following values: 

		GWL_EXSTYLE Extended window styles (WS_EX_XXX) 
		GWL_STYLE Window styles (WS_XXX) 

	Answers 0 if the function fails."

	<stdcall: sdword GetWindowLongW handle sdword>
	^self invalidCall!

getWindowText: aWindow lpString: aBuffer nMaxCount: len

^self getWindowTextW: aWindow lpString: aBuffer nMaxCount: len!

getWindowTextLength: hWnd

^self getWindowTextLengthW: hWnd!

getWindowTextLengthW: hWnd
	"Answer the length, in characters, of the specified window's title 'text'
		int GetWindowTextLength(
			HWND hWnd 	// handle of window or control with text
		);"

	<stdcall: sdword GetWindowTextLengthW handle>
	^self invalidCall
!

getWindowTextW: aWindow lpString: aBuffer nMaxCount: len
	"Copy the text of the specified window (its title bar, or other appropriate text)
	into the supplied buffer, up to the specified maximum length
		int GetWindowText(
			HWND hWnd,		// handle of window or control with text
			LPTSTR lpString,	// address of buffer for text
			int nMaxCount 		// maximum number of characters to copy
		);"

	<stdcall: sdword GetWindowTextW handle lpvoid sdword>
	^self invalidCall
!

getWindowULong: aWindowHandle nIndex: zeroBasedIntegerOffset

^self getWindowULongW: aWindowHandle nIndex: zeroBasedIntegerOffset!

getWindowULongPtr: aWindowHandle nIndex: zeroBasedIntegerOffset

^self getWindowULongPtrW: aWindowHandle nIndex: zeroBasedIntegerOffset!

getWindowULongPtrW: aWindowHandle nIndex: zeroBasedIntegerOffset 
	"Answer various unsigned values retrieved from the window with handle, aWindowHandle. See
	#getWindowLongPtr:offset: for more details"

	<stdcall: uintptr GetWindowLongPtrW handle sdword>
	^self invalidCall!

getWindowULongW: aWindowHandle nIndex: zeroBasedIntegerOffset 
	"Answer various unsigned 32-bit integer values retrieved from the window with handle,
	aWindowHandle. See #getWindowLongPtr:offset: for more details"

	<stdcall: dword GetWindowLongW handle sdword>
	^self invalidCall!

insertMenuItem: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMENUITEMINFOA

^self insertMenuItemW: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMENUITEMINFOA!

insertMenuItemW: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMENUITEMINFOA
	"Insert a new item into a menu.

		BOOL InsertMenuItem(
			HMENU hMenu,
			UINT uItem,
			BOOL fByPosition,
			LPCMENUITEMINFO lpmii
		);"

	<stdcall: bool InsertMenuItemW handle dword bool MENUITEMINFOA*>
	^self invalidCall!

isCharAlpha: aCharacter

^self isCharAlphaW: aCharacter!

isCharAlphaNumeric: aCharacter

^self isCharAlphaNumericW: aCharacter!

isCharAlphaNumericW: aCharacter
	"Answer whether a character is an alphabetic character or a digit. This will dependent on the 
	semantics of the language selected by the user during setup or by using Control Panel.
		BOOL IsCharAlphaNumeric(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharAlphaNumericW char>
	^self invalidCall!

isCharAlphaW: aCharacter
	"Answer whether a character is an alphabetic character. This will dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
		BOOL IsCharAlpha(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharAlphaW char>
	^self invalidCall!

isCharLower: aCharacter

^self isCharLowerW: aCharacter!

isCharLowerW: aCharacter
	"Answer whether a character is a lowercase letter. This will dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
		BOOL IsCharLower(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharLowerW char>
	^self invalidCall!

isCharLowerWW: aCharacter 
	"Answer whether a character is a lowercase letter. This will dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
		BOOL IsCharLower(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharLowerW dword>
	^self invalidCall!

isCharUpper: aCharacter

^self isCharUpperW: aCharacter!

isCharUpperW: aCharacter
	"Answer whether a character is an uppercase letter. This will dependent on the semantics of 
	the language selected by the user during setup or by using Control Panel.
		BOOL IsCharUpper(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharUpperW char>
	^self invalidCall!

isCharUpperWW: aCharacter 
	"Answer whether a character is an uppercase letter. This will dependent on the semantics of 
	the language selected by the user during setup or by using Control Panel.
		BOOL IsCharUpper(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharUpperW dword>
	^self invalidCall!

isDialogMessage: aHandle lpMsg: aMSG

^self isDialogMessageW: aHandle lpMsg: aMSG!

isDialogMessageW: aHandle lpMsg: aMSG
	"Determine whether a message is intended for the specified dialog 
	box and, if it is, process the message. 
		BOOL IsDialogMessage(
  			HWND hDlg,	// handle of dialog box
			LPMSG lpMsg	// address of structure with message
		);"

	<stdcall: bool IsDialogMessageW handle MSG* >
	^self invalidCall!

loadAccelerators: anExternalHandle lpTableName: anIntegerOrString

^self loadAcceleratorsW: anExternalHandle lpTableName: anIntegerOrString!

loadAcceleratorsW: anExternalHandle lpTableName: anIntegerOrString
	"Load the specified accelerator table from the resources of the application identified
	by the instance handle, aHandle. The argument anIntegerOrString may be a 16-bit integer
	resource id, or a String name. Answer the handle of the resource, or nil if the function
	fails. Loaded accelerator tables will be freed automatically when Dolphin terminates.

		HACCEL LoadAccelerators(
				HINSTANCE  hInstance,	// handle of application instance
				LPCTSTR  lpTableName 	// address of table-name string
			);"

	<stdcall: handle LoadAcceleratorsW handle lpvoid>
	^self invalidCall!

loadCursor: anInstanceHandle lpCursorName: anIntegerID

^self loadCursorW: anInstanceHandle lpCursorName: anIntegerID!

loadCursorFromFile: aFilename

^self loadCursorFromFileW: aFilename!

loadCursorFromFileW: aFilename
	"Creates a cursor based on data contained in the specified file, and answer the handle, 
	or NULL if it fails. Can load standard (.CUR) or animated (.ANI) cursors.

		HCURSOR LoadCursorFromFile (
  			LPCTSTR  lpFileName	// pointer to name of cursor file, or system cursor identifier
	   );"

	<stdcall: handle LoadCursorFromFileW lpvoid>
	^self invalidCall!

loadCursorW: anInstanceHandle lpCursorName: anIntegerID
	"Loads the specified cursor resource from the module with the specified handle
	Under Win32 it is not necessary to destroy cursors loaded in this manner.

		HCURSOR LoadCursor(
  			HINSTANCE  hInstance,	// handle of application instance
			LPCTSTR  lpCursorName 	// name string or cursor resource identifier  
		);"

	<stdcall: handle LoadCursorW handle lpvoid>
	^self invalidCall!

loadIcon: anInstanceHandle lpIconName: anIntegerID

^self loadIconW: anInstanceHandle lpIconName: anIntegerID!

loadIconW: anInstanceHandle lpIconName: anIntegerID
	"Loads the specified icon resource from the specified module.
		HICON LoadIcon(
			HINSTANCE hInstance,	// handle of application instance
			LPCTSTR lpIconName 	// icon-name string or icon resource identifier
		);"

	<stdcall: handle LoadIconW handle lpvoid>
	^self invalidCall
!

loadImage: hInst lpszName: filename uType: type cxDesired: w cyDesired: h fuLoad: flags

^self loadImageW: hInst lpszName: filename uType: type cxDesired: w cyDesired: h fuLoad: flags!

loadImageW: hInst lpszName: filename uType: type cxDesired: w cyDesired: h fuLoad: flags
	"Load an icon, cursor, or bitmap.
		HANDLE LoadImage(
			HINSTANCE hinst, 	// handle of the instance that contains the image
			LPCTSTR lpszName,	// name or identifier of image
			UINT uType,		// type of image
			int cxDesired,		// desired width
			int cyDesired,		// desired height
			UINT fuLoad		// load flags
		);"

	<stdcall: handle LoadImageW handle lpvoid dword sdword sdword dword>
	^self invalidCall!

loadMenu: anExternalHandle lpMenuName: anIntegerOrString

^self loadMenuW: anExternalHandle lpMenuName: anIntegerOrString!

loadMenuW: anExternalHandle lpMenuName: anIntegerOrString
	"Load the specified menu from the resources of the application identified
	by the instance handle, aHandle. The argument anIntegerOrString may be a 16-bit integer
	resource id, or a String name. Answer the handle of the resource, or nil if the function
	fails.

		HMENU LoadMenu(
			HINSTANCE  hInstance,	// handle of application instance
			LPCTSTR  lpMenuName	// menu name string or menu-resource identifier  
			);"

	<stdcall: handle LoadMenuW handle lpvoid>
	^self invalidCall!

loadString: anExternalHandle uID: anIntegerID lpBuffer: aString nBufferMax: anIntegerLength

^self loadStringW: anExternalHandle uID: anIntegerID lpBuffer: aString nBufferMax: anIntegerLength!

loadStringW: anExternalHandle uID: anIntegerID lpBuffer: aString nBufferMax: anIntegerLength
	"Load a string resource from the executable file associated with 
	the specified module, into the specified buffer appending
	a terminating null character. Answer the length of the string, or 0 
	if it does not exist.
		int LoadString(
			HINSTANCE hInstance,	// handle of module containing string resource 
			UINT uID,				// resource identifier 
			LPTSTR lpBuffer,		// address of buffer for resource 
			int nBufferMax 		// size of buffer
		);"

	<stdcall: sdword LoadStringW handle dword lpstr sdword>
	^self invalidCall!

mapVirtualKey: keyCode uMapType: mapType

^self mapVirtualKeyW: keyCode uMapType: mapType!

mapVirtualKeyW: keyCode uMapType: mapType
	"Map a virtual-key code into a scan code or character value, or translate a scan code into a virtual-key code. 

	The Meaning of keycode and the result of the translation depends on the maptype:

		keyCode					Answer
	0	virtual key code		->	scan code
	1	scan code				->	virtual key code
	2	virtual key code		->	character value
	3	scan code				->	virtual key code	

		UINT	MapVirtualKey(
			UINT uCode,
			UINT uMapType);"

	<stdcall: dword MapVirtualKeyW dword dword>
	^self invalidCall!

messageBox: hWnd lpText: lpText lpCaption: lpCaption uType: uType

^self messageBoxW: hWnd lpText: lpText lpCaption: lpCaption uType: uType!

messageBoxIndirect: aMSGBOXPARAMS

^self messageBoxIndirectW: aMSGBOXPARAMS!

messageBoxIndirectW: aMSGBOXPARAMS
	"Open a message box with the details specified in the structure.

		int MessageBox(
			LPMSGBOXPARRAMS lpMsgBoxParams;
		);"

	<stdcall: sdword MessageBoxIndirectW MSGBOXPARAMS*>
	^self invalidCall!

messageBoxW: hWnd lpText: lpText lpCaption: lpCaption uType: uType
	<stdcall: sdword MessageBoxW handle lpstr lpstr dword>
	^self invalidCall!

modifyMenu: hMenuDrop uPosition: position uFlags: styleFlags uIDNewItem: menuId lpNewItem: menuText

^self modifyMenuW: hMenuDrop uPosition: position uFlags: styleFlags uIDNewItem: menuId lpNewItem: menuText!

modifyMenuW: hMenuDrop uPosition: position uFlags: styleFlags uIDNewItem: menuId lpNewItem: menuText
	"Changes an existing menu item the specified menu with the specified
	style, identifier and text.

		BOOL ModifyMenu(
			HMENU hMenu,	// handle to menu
			UINT uPosition,	// menu item to modify
			UINT uFlags,		// menu item flags
			UINT uIDNewItem,	// menu item identifier or pop-up menu handle 
			LPCTSTR lpNewItem	// menu item content
		);"

	<stdcall: bool ModifyMenuW handle dword dword dword lpstr>
	^self invalidCall!

open: aString

	handle  := self class superclass default handle.!

overlappedMsgBoxIndirect: aMSGBOXPARAMS

^self overlappedMsgBoxIndirectW: aMSGBOXPARAMS!

overlappedMsgBoxIndirectW: aMSGBOXPARAMS
	"Private - Open a message box with the details specified in the structure
	as an overlapped call (i.e. on a separate thread).
	N.B. Don't use for MB_TASKMODAL message boxes.

		int MessageBox(
			LPMSGBOXPARRAMS lpMsgBoxParams;
		);"

	<overlap stdcall: sdword MessageBoxIndirectW MSGBOXPARAMS* >
	^self invalidCall!

peekMessage: aMSG hWnd: aWindowHandle uMsgFilterMin: anIntFilterMin uMsgFilterMax: uMsgFilterMax wRemoveMsg: anIntFlags

^self peekMessageW: aMSG hWnd: aWindowHandle uMsgFilterMin: anIntFilterMin uMsgFilterMax: uMsgFilterMax wRemoveMsg: anIntFlags!

peekMessageW: aMSG hWnd: aWindowHandle uMsgFilterMin: anIntFilterMin uMsgFilterMax: uMsgFilterMax wRemoveMsg: anIntFlags
	"Check the threads message queue for messages for the Window with
	handle aWindowHandle (all windows if nil/0, app messages if -1) in the range
	anIntFilterMin..anIntFilterMax (all messages if 0..0), retrieving 
	the first (if any) of such messages into aMSG, answering whether 
	a message was found. The message is optionally removed from the 
	queue depending on the value of the argument, anIntFlags (PM_REMOVE/
	PM_NOREMOVE). The flag value PmNoYield is obsolete, and has no effect
	in Win32.

		BOOL PeekMessage(
			LPMSG  lpMsg,			// address of structure for message
			HWND  hWnd,			// handle of window
			UINT  uMsgFilterMin,	// first message
			UINT  uMsgFilterMax,	// last message
			UINT  wRemoveMsg 		// removal flags
		);"

	<stdcall: bool PeekMessageW MSG* handle dword dword dword>
	^self invalidCall!

postMessage: aWindowHandle msg: msg wParam: wParam lParam: lParam

^self postMessageW: aWindowHandle msg: msg wParam: wParam lParam: lParam!

postMessageW: aWindowHandle msg: msg wParam: wParam lParam: lParam
	"Post a message to aWindowHandle.

	LRESULT PostMessage(
		HWND hWnd, 		// handle of destination window
		UINT Msg, 		// message to send
		WPARAM wParam, 	// first message parameter
		LPARAM lParam 		// second message parameter
	   );"

	<stdcall: intptr PostMessageW handle dword uintptr intptr>
	^self invalidCall !

postThreadMessage: anIntegerId msg: msg wParam: wParam lParam: lParam

^self postThreadMessageW: anIntegerId msg: msg wParam: wParam lParam: lParam!

postThreadMessageW: anIntegerId msg: msg wParam: wParam lParam: lParam
	"Post a message to the message queue of the specified thread for asynchronous processing.
		BOOL PostThreadMessage(
  			DWORD  idThread,	// thread identifier
			UINT  Msg,		// message to post
			WPARAM  wParam,	// first message parameter
			LPARAM  lParam 	// second message parameter);"

	<stdcall: bool PostThreadMessageW handle dword uintptr uintptr>
	^self invalidCall !

registerClass: aWNDCLASS

^self registerClassW: aWNDCLASS!

registerClassW: aWNDCLASS
	"Registers a window class for subsequent use in calls to the CreateWindow or 
	CreateWindowEx functions. Answers the class atom, or zero if fails.
		ATOM RegisterClass(
			CONST WNDCLASS  *lpwc 	// address of structure with class data
		);"

	<stdcall: word RegisterClassW WNDCLASS* >
	^self invalidCall!

registerClipboardFormat: formatName

^self registerClipboardFormatW: formatName!

registerClipboardFormatW: formatName
	"Register a new clipboard format name, answering its system wide identifier.

		UINT RegisterClipboardFormat( 
			LPCTSTR lpszFormat  // address of name string 
		);"

	<stdcall: dword RegisterClipboardFormatW lpstr>
	^self invalidCall!

registerWindowMessage: aString

^self registerWindowMessageW: aString!

registerWindowMessageW: aString
	"Defines a new window message that is guaranteed to be unique system wide.

		UINT RegisterWindowMessage(
			LPCTSTR  lpString 	// address of message string
		);"

	<stdcall: dword RegisterWindowMessageW lpstr>
	^self invalidCall!

removeProp: aWindowHandle lpString: name

^self removePropW: aWindowHandle lpString: name!

removePropW: aWindowHandle lpString: name
	"Remove an the entry identified by lpString from the property list of 
	the specified window.
		HANDLE RemoveProp(
			HWND hWnd,	// handle to window
			LPCTSTR lpString 	// atom or address of string
		);"

	<stdcall: handle RemovePropW handle lpvoid>
	^self invalidCall

!

sendDlgItemMessage: aWindowHandle nIDDlgItem: anIntegerId msg: aString wParam: wParam lParam: lParam

^self sendDlgItemMessageW: aWindowHandle nIDDlgItem: anIntegerId msg: aString wParam: wParam lParam: lParam!

sendDlgItemMessageW: aWindowHandle nIDDlgItem: anIntegerId msg: aString wParam: wParam lParam: lParam
	"Send a message to the specified control in a dialog box. 
		LRESULT SendDlgItemMessage(
			HWND  hDlg,		// handle of dialog box
			int  nIDDlgItem,	// identifier of control
			UINT  Msg,		// message to send
			WPARAM  wParam,	// first message parameter
			LPARAM  lParam 	// second message parameter
		   );"

	<stdcall: uintptr SendDlgItemMessageW handle sdword dword uintptr intptr>
	^self invalidCall!

sendMessage: aWindowHandle msg: msg wParam: wParam lParam: lParam

^self sendMessageW: aWindowHandle msg: msg wParam: wParam lParam: lParam!

sendMessage: aWindowHandle msg: msg wParam: wParam lpParam: lParam


^self sendMessageW: aWindowHandle msg: msg wParam: wParam lpParam: (lParam class == String ifTrue: [lParam asUnicodeString ] ifFalse: [lParam ])!

sendMessage: aWindowHandle msg: msg wpParam: wParam lpParam: lParam

^self sendMessageW: aWindowHandle msg: msg wpParam: wParam lpParam: lParam!

sendMessageW: aWindowHandle msg: msg wParam: wParam lParam: lParam
	"The SendMessage function sends the specified message to a window or windows.
	The function calls the window procedure for the specified window and does not
	return until the window procedure has processed the message. The PostMessage
	function, in contrast, posts a message to a thread's message queue and returns
	immediately. 

	Implementation Note: Although LPARAM is defined as a signed parameter, more often
	than not it is used to pass an unsigned 32-bit value. As we have a strict definition
	of what can be passed to a signed integer parameter (i.e. only valid two's complement 
	values that fit within the required size), we used the slightly more relaxed unsigned 
	specification. This allows LargeIntegers > the maximum positive signed value for
	the machine word size to be passed.

	LRESULT SendMessage(
		HWND hWnd, 		// handle of destination window
		UINT Msg, 		// message to send
		WPARAM wParam, 	// first message parameter
		LPARAM lParam 		// second message parameter
	   );"

	<stdcall: intptr SendMessageW handle dword uintptr uintptr>
	^self invalidCall !

sendMessageW: aWindowHandle msg: msg wParam: wParam lpParam: lParam
	"As sendMessageW:msg:wParam:lParam, but implicit conversion of lParam
	to pointer."

	<stdcall: intptr SendMessageW handle dword uintptr lpvoid>
	^self invalidCall !

sendMessageW: aWindowHandle msg: msg wpParam: wParam lpParam: lParam
	"As sendMessageW:msg:wParam:lParam, but implicit conversion of lParam
	and wParam to pointers."

	<stdcall: intptr SendMessageW handle dword lpvoid lpvoid>
	^self invalidCall !

setClassLong: aWindowHandle nIndex: offset dwNewLong: value

^self setClassLongW: aWindowHandle nIndex: offset dwNewLong: value!

setClassLongW: aWindowHandle nIndex: offset dwNewLong: value
	"Change an attribute of the specified window class, setting 
	a signed 32-bit (long) value at the specified offset into 
	the extra window memory of a window.
	Note: This is declared as returning a DWORD.

		DWORD SetClassLong(
			HWND hWnd,		// handle of window
			int nIndex,		// offset of value to set
			LONG dwNewLong 	// new value
		);"

	<stdcall: dword SetClassLongW handle sdword sdword>
	^self invalidCall!

setDlgItemText: aWindowHandle nIDDlgItem: anIntegerId lpString: aString

^self setDlgItemTextW: aWindowHandle nIDDlgItem: anIntegerId lpString: aString!

setDlgItemTextW: aWindowHandle nIDDlgItem: anIntegerId lpString: aString
	"Sets the title or text of a control in a dialog box. 
		BOOL SetDlgItemText(
  			HWND  hDlg,		// handle of dialog box
			int  nIDDlgItem,	// identifier of control
			LPCTSTR  lpString 	// text to set
			);"

	<stdcall: bool SetDlgItemTextW handle sdword lpvoid>
	^self invalidCall!

setMenuItemInfo: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMenuItemInfo

^self setMenuItemInfoW: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMenuItemInfo!

setMenuItemInfoW: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMenuItemInfo
	"Set various information about a menu item.
		BOOL SetMenuItemInfo(
			HMENU hMenu,
			UINT uItem,
			BOOL fByPosition,
			LPMENUITEMINFO lpmii
		);"

	<stdcall: bool SetMenuItemInfoW handle dword bool MENUITEMINFOA*>
	^self invalidCall

!

setProp: aWindowHandle lpString: name hData: anObject

^self setPropW: aWindowHandle lpString: name hData: anObject!

setPropW: aWindowHandle lpString: name hData: anObject
	"Answer true if name and anObject are successfully added to the property list.
	The SetProp function adds a new entry or changes an existing entry in the property list of the specified window.
	The function adds a new entry to the list if the specified character string does not exist already in the list.
	The new entry contains the string and the handle. Otherwise, the function replaces the string's current handle with the specified handle. 
		
		BOOL SetProp(
			HWND  	hWnd,		// handle of window
			LPCTSTR  lpString,		// atom or address of string
			HANDLE  	hData 		// handle of data
		);

	Before destroying a window (that is, before processing the WM_DESTROY message), an application must remove all entries it has added
	to the property list. The application must use the RemoveProp function to remove the entries."

	<stdcall: bool SetPropW handle lpvoid handle>
	^self invalidCall

!

setWindowDWORD: aWindowHandle nIndex: offset dwNewDWORD: value

^self setWindowDWORDW: aWindowHandle nIndex: offset dwNewDWORD: value!

setWindowDWORDW: aWindowHandle nIndex: offset dwNewDWORD: value 
	"See #setWindowLong:nIndex:dwNewLong: value, but note this expects an unsigned 32-bit
	integer as its last argument, and always returns a positive integer constructed by treating
	the return value as unsigned."

	<stdcall: dword SetWindowLongW handle sdword dword>
	^self invalidCall!

setWindowLong: aWindowHandle nIndex: offset dwNewLong: value

^self setWindowLongW: aWindowHandle nIndex: offset dwNewLong: value!

setWindowLongW: aWindowHandle nIndex: offset dwNewLong: value
	"Change an attribute of the specified window, setting 
	a signed 32-bit (long) value at the specified offset into 
	the extra window memory of a window.
		LONG SetWindowLong(
			HWND hWnd,		// handle of window
			int nIndex,		// offset of value to set
			LONG dwNewLong 	// new value
		);"

	<stdcall: sdword SetWindowLongW handle sdword sdword>
	^self invalidCall!

setWindowText: aWindowHandle lpString: aString

^self setWindowTextW: aWindowHandle lpString: aString asUnicodeString!

setWindowTextW: aWindowHandle lpString: aString
	"Set the 'text' of the specified window.
		BOOL SetWindowText(
			HWND hWnd,	// handle of window or control
			LPCTSTR lpString 	// address of string
		);"

	<stdcall: bool SetWindowTextW handle lpvoid>
	^self invalidCall!

stringLower: aString

^self stringLowerW: aString!

stringLowerA: aString

^self stringLowerAW: aString!

stringLowerAW: aString 
	"Convert aString to lower case IN PLACE. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharLower(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: void CharLowerW lpvoid>
	^self invalidCall!

stringLowerW: aString
	"Convert aString to lower case IN PLACE. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharLower(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: void CharLowerW lpvoid>
	^self invalidCall!

stringUpper: aString

^self stringUpperW: aString!

stringUpperA: aString

^self stringUpperAW: aString!

stringUpperAW: aString 
	"Convert aString to uppercase IN PLACE. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharUpper(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: void CharUpperW lpvoid>
	^self invalidCall!

stringUpperW: aString
	"Convert aString to uppercase IN PLACE. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharUpper(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: void CharUpperW lpvoid>
	^self invalidCall!

systemParametersInfo: uiAction uiParam: param1 pvParam: param2 fWinIni: update

^self systemParametersInfoW: uiAction uiParam: param1 pvParam: param2 fWinIni: update!

systemParametersInfoW: uiAction uiParam: param1 pvParam: param2 fWinIni: update
	"
	BOOL SystemParametersInfo(
		UINT uiAction,	// system parameter to query or set
		UINT uiParam,	// depends on action to be taken
		PVOID pvParam,	// depends on action to be taken
		UINT fWinIni 	// user profile update flag
	);"

	<stdcall: bool SystemParametersInfoW dword dword lpvoid dword>
	^self invalidCall!

unregisterClass: classAtomOrStringName hInstance: instanceHandle

^self unregisterClassW: classAtomOrStringName hInstance: instanceHandle!

unregisterClassW: classAtomOrStringName hInstance: instanceHandle
	"Remove a window class.
		BOOL UnregisterClass(
			LPCTSTR lpClassName,	// address of class name string
			HINSTANCE hInstance 	// handle of application instance
		);"

	<stdcall: bool UnregisterClassW lpvoid handle>
	^self invalidCall!

vkKeyScan: aChar

^self vkKeyScanW: aChar!

vkKeyScanW: aChar
	"Translate a character to the corresponding virtual-key code 
	and shift state for the current keyboard.
		SHORT VkKeyScan(
			TCHAR ch 	// character to translate
	   );"

	<stdcall: sword VkKeyScanW char>
	^self invalidCall

!

winHelp: hwndMain lpszHelp: lpszHelp uCommand: uCommand dwData: dwData

^self winHelpW: hwndMain lpszHelp: lpszHelp uCommand: uCommand dwData: dwData!

winHelpW: hwndMain lpszHelp: lpszHelp uCommand: uCommand dwData: dwData
	"Invoke the WinHelp() function of the module wrapped by the receiver.
	Helpstring: Starts windows help and passes a request for help

		BOOL __stdcall WinHelp(
			HWND hwndMain,
			LPCSTR lpszHelp,
			unsigned int uCommand,
			ULONG_PTR dwData);"

	<stdcall: bool WinHelpW handle lpstr dword uintptr>
	^self invalidCall! !
!UserLibraryW categoriesFor: #callWindowProc:hWnd:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #callWindowProcW:hWnd:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #charLower:!public! !
!UserLibraryW categoriesFor: #charLowerW:!public! !
!UserLibraryW categoriesFor: #charUpper:!public! !
!UserLibraryW categoriesFor: #charUpperW:!public! !
!UserLibraryW categoriesFor: #createAcceleratorTable:cEntries:!public! !
!UserLibraryW categoriesFor: #createAcceleratorTableW:cEntries:!public! !
!UserLibraryW categoriesFor: #createDialog:lpTemplate:hWndParent:lpDialogFunc:dwInitParam:!public! !
!UserLibraryW categoriesFor: #createDialogW:lpTemplate:hWndParent:lpDialogFunc:dwInitParam:!public! !
!UserLibraryW categoriesFor: #createWindowEx:lpClassName:lpWindowName:dwStyle:x:y:nWidth:nHeight:hWndParent:hMenu:hInstance:lpParam:!public! !
!UserLibraryW categoriesFor: #createWindowExW:lpClassName:lpWindowName:dwStyle:x:y:nWidth:nHeight:hWndParent:hMenu:hInstance:lpParam:!public! !
!UserLibraryW categoriesFor: #defDlgProc:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #defDlgProcW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #defWindowProc:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #defWindowProcW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #dispatchMessage:!public! !
!UserLibraryW categoriesFor: #dispatchMessageW:!public! !
!UserLibraryW categoriesFor: #drawState:hbr:lpOutputFunc:lData:wData:x:y:cx:cy:fuFlags:!public! !
!UserLibraryW categoriesFor: #drawStateW:hbr:lpOutputFunc:lData:wData:x:y:cx:cy:fuFlags:!public! !
!UserLibraryW categoriesFor: #drawTextEx:lpchText:cchText:lprc:dwDTFormat:lpDTParams:!public! !
!UserLibraryW categoriesFor: #drawTextExW:lpchText:cchText:lprc:dwDTFormat:lpDTParams:!public! !
!UserLibraryW categoriesFor: #findWindow:lpWindowName:!public! !
!UserLibraryW categoriesFor: #findWindowW:lpWindowName:!public! !
!UserLibraryW categoriesFor: #getClassName:lpClassName:nMaxCount:!public! !
!UserLibraryW categoriesFor: #getClassNameW:lpClassName:nMaxCount:!public! !
!UserLibraryW categoriesFor: #getClipboardFormatName:lpszFormatName:cchMaxCount:!public! !
!UserLibraryW categoriesFor: #getClipboardFormatNameW:lpszFormatName:cchMaxCount:!public! !
!UserLibraryW categoriesFor: #getKeyNameText:lpString:nSize:!public! !
!UserLibraryW categoriesFor: #getKeyNameTextW:lpString:nSize:!public! !
!UserLibraryW categoriesFor: #getMessage:hWnd:wMsgFilterMin:wMsgFilterMax:!public! !
!UserLibraryW categoriesFor: #getMessageW:hWnd:wMsgFilterMin:wMsgFilterMax:!public! !
!UserLibraryW categoriesFor: #getProp:lpString:!public! !
!UserLibraryW categoriesFor: #getPropW:lpString:!public! !
!UserLibraryW categoriesFor: #getWindowLong:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowLongPtr:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowLongPtrW:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowLongW:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowText:lpString:nMaxCount:!public! !
!UserLibraryW categoriesFor: #getWindowTextLength:!public! !
!UserLibraryW categoriesFor: #getWindowTextLengthW:!public! !
!UserLibraryW categoriesFor: #getWindowTextW:lpString:nMaxCount:!public! !
!UserLibraryW categoriesFor: #getWindowULong:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowULongPtr:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowULongPtrW:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowULongW:nIndex:!public! !
!UserLibraryW categoriesFor: #insertMenuItem:uItem:fByPosition:lpmii:!public! !
!UserLibraryW categoriesFor: #insertMenuItemW:uItem:fByPosition:lpmii:!public! !
!UserLibraryW categoriesFor: #isCharAlpha:!public! !
!UserLibraryW categoriesFor: #isCharAlphaNumeric:!public! !
!UserLibraryW categoriesFor: #isCharAlphaNumericW:!public! !
!UserLibraryW categoriesFor: #isCharAlphaW:!public! !
!UserLibraryW categoriesFor: #isCharLower:!public! !
!UserLibraryW categoriesFor: #isCharLowerW:!public! !
!UserLibraryW categoriesFor: #isCharLowerWW:!public! !
!UserLibraryW categoriesFor: #isCharUpper:!public! !
!UserLibraryW categoriesFor: #isCharUpperW:!public! !
!UserLibraryW categoriesFor: #isCharUpperWW:!public! !
!UserLibraryW categoriesFor: #isDialogMessage:lpMsg:!public! !
!UserLibraryW categoriesFor: #isDialogMessageW:lpMsg:!public! !
!UserLibraryW categoriesFor: #loadAccelerators:lpTableName:!public! !
!UserLibraryW categoriesFor: #loadAcceleratorsW:lpTableName:!public! !
!UserLibraryW categoriesFor: #loadCursor:lpCursorName:!public! !
!UserLibraryW categoriesFor: #loadCursorFromFile:!public! !
!UserLibraryW categoriesFor: #loadCursorFromFileW:!public! !
!UserLibraryW categoriesFor: #loadCursorW:lpCursorName:!public! !
!UserLibraryW categoriesFor: #loadIcon:lpIconName:!public! !
!UserLibraryW categoriesFor: #loadIconW:lpIconName:!public! !
!UserLibraryW categoriesFor: #loadImage:lpszName:uType:cxDesired:cyDesired:fuLoad:!public! !
!UserLibraryW categoriesFor: #loadImageW:lpszName:uType:cxDesired:cyDesired:fuLoad:!public! !
!UserLibraryW categoriesFor: #loadMenu:lpMenuName:!public! !
!UserLibraryW categoriesFor: #loadMenuW:lpMenuName:!public! !
!UserLibraryW categoriesFor: #loadString:uID:lpBuffer:nBufferMax:!public! !
!UserLibraryW categoriesFor: #loadStringW:uID:lpBuffer:nBufferMax:!public! !
!UserLibraryW categoriesFor: #mapVirtualKey:uMapType:!public! !
!UserLibraryW categoriesFor: #mapVirtualKeyW:uMapType:!public! !
!UserLibraryW categoriesFor: #messageBox:lpText:lpCaption:uType:!public! !
!UserLibraryW categoriesFor: #messageBoxIndirect:!public! !
!UserLibraryW categoriesFor: #messageBoxIndirectW:!public! !
!UserLibraryW categoriesFor: #messageBoxW:lpText:lpCaption:uType:!public! !
!UserLibraryW categoriesFor: #modifyMenu:uPosition:uFlags:uIDNewItem:lpNewItem:!public! !
!UserLibraryW categoriesFor: #modifyMenuW:uPosition:uFlags:uIDNewItem:lpNewItem:!public! !
!UserLibraryW categoriesFor: #open:!public! !
!UserLibraryW categoriesFor: #overlappedMsgBoxIndirect:!public! !
!UserLibraryW categoriesFor: #overlappedMsgBoxIndirectW:!public! !
!UserLibraryW categoriesFor: #peekMessage:hWnd:uMsgFilterMin:uMsgFilterMax:wRemoveMsg:!public! !
!UserLibraryW categoriesFor: #peekMessageW:hWnd:uMsgFilterMin:uMsgFilterMax:wRemoveMsg:!public! !
!UserLibraryW categoriesFor: #postMessage:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #postMessageW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #postThreadMessage:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #postThreadMessageW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #registerClass:!public! !
!UserLibraryW categoriesFor: #registerClassW:!public! !
!UserLibraryW categoriesFor: #registerClipboardFormat:!public! !
!UserLibraryW categoriesFor: #registerClipboardFormatW:!public! !
!UserLibraryW categoriesFor: #registerWindowMessage:!public! !
!UserLibraryW categoriesFor: #registerWindowMessageW:!public! !
!UserLibraryW categoriesFor: #removeProp:lpString:!public! !
!UserLibraryW categoriesFor: #removePropW:lpString:!public! !
!UserLibraryW categoriesFor: #sendDlgItemMessage:nIDDlgItem:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #sendDlgItemMessageW:nIDDlgItem:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #sendMessage:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #sendMessage:msg:wParam:lpParam:!public! !
!UserLibraryW categoriesFor: #sendMessage:msg:wpParam:lpParam:!public! !
!UserLibraryW categoriesFor: #sendMessageW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #sendMessageW:msg:wParam:lpParam:!public! !
!UserLibraryW categoriesFor: #sendMessageW:msg:wpParam:lpParam:!public! !
!UserLibraryW categoriesFor: #setClassLong:nIndex:dwNewLong:!public! !
!UserLibraryW categoriesFor: #setClassLongW:nIndex:dwNewLong:!public! !
!UserLibraryW categoriesFor: #setDlgItemText:nIDDlgItem:lpString:!public! !
!UserLibraryW categoriesFor: #setDlgItemTextW:nIDDlgItem:lpString:!public! !
!UserLibraryW categoriesFor: #setMenuItemInfo:uItem:fByPosition:lpmii:!public! !
!UserLibraryW categoriesFor: #setMenuItemInfoW:uItem:fByPosition:lpmii:!public! !
!UserLibraryW categoriesFor: #setProp:lpString:hData:!public! !
!UserLibraryW categoriesFor: #setPropW:lpString:hData:!public! !
!UserLibraryW categoriesFor: #setWindowDWORD:nIndex:dwNewDWORD:!public! !
!UserLibraryW categoriesFor: #setWindowDWORDW:nIndex:dwNewDWORD:!public! !
!UserLibraryW categoriesFor: #setWindowLong:nIndex:dwNewLong:!public! !
!UserLibraryW categoriesFor: #setWindowLongW:nIndex:dwNewLong:!public! !
!UserLibraryW categoriesFor: #setWindowText:lpString:!public! !
!UserLibraryW categoriesFor: #setWindowTextW:lpString:!public! !
!UserLibraryW categoriesFor: #stringLower:!public! !
!UserLibraryW categoriesFor: #stringLowerA:!public! !
!UserLibraryW categoriesFor: #stringLowerAW:!public! !
!UserLibraryW categoriesFor: #stringLowerW:!public! !
!UserLibraryW categoriesFor: #stringUpper:!public! !
!UserLibraryW categoriesFor: #stringUpperA:!public! !
!UserLibraryW categoriesFor: #stringUpperAW:!public! !
!UserLibraryW categoriesFor: #stringUpperW:!public! !
!UserLibraryW categoriesFor: #systemParametersInfo:uiParam:pvParam:fWinIni:!public! !
!UserLibraryW categoriesFor: #systemParametersInfoW:uiParam:pvParam:fWinIni:!public! !
!UserLibraryW categoriesFor: #unregisterClass:hInstance:!public! !
!UserLibraryW categoriesFor: #unregisterClassW:hInstance:!public! !
!UserLibraryW categoriesFor: #vkKeyScan:!public! !
!UserLibraryW categoriesFor: #vkKeyScanW:!public! !
!UserLibraryW categoriesFor: #winHelp:lpszHelp:uCommand:dwData:!public! !
!UserLibraryW categoriesFor: #winHelpW:lpszHelp:uCommand:dwData:!public! !

WNDCLASSW guid: (GUID fromString: '{a3f259b2-8c4d-4965-91b1-7ee32e6c5e72}')!
WNDCLASSW comment: ''!
!WNDCLASSW categoriesForClass!External-Data-Structured-Win32! !
!WNDCLASSW class methodsFor!

defineFields
	"Define the layout of the Win32 WNDCLASS structure. Currently to avoid
	wasting space, the structure is set up for registering classes only
	(i.e. the fields are fillers or write only). Furthermore it is not
	compiled at present.

	WNDCLASS compileDefinition

		typedef struct _WNDCLASS {
			UINT		style; 
			WNDPROC	lpfnWndProc; 
			int		cbClsExtra; 
			int		cbWndExtra; 
			HANDLE	hInstance; 
			HICON	hIcon; 
			HCURSOR	hCursor; 
			HBRUSH	hbrBackground; 
			LPCTSTR	lpszMenuName; 
			LPCTSTR	lpszClassName; 
		} WNDCLASS; "

	self 
		defineField: #style type: DWORDField writeOnly;
		defineField: #lpfnWndProc type: DWORDField writeOnly;
		defineField: #cbClsExtra type: SDWORDField filler;
		defineField: #cbWndExtra type: SDWORDField filler;
		defineField: #hInstance type: DWORDField writeOnly;
		defineField: #hIcon type: DWORDField writeOnly;
		defineField: #hCursor type: DWORDField writeOnly;
		defineField: #hbrBackground type: DWORDField writeOnly;
		defineField: #lpszMenuName type: (PointerField type: UnicodeString ) beWriteOnly;
		defineField: #lpszClassName type: (PointerField type: UnicodeString) beWriteOnly! !
!WNDCLASSW class categoriesFor: #defineFields!initializing!public! !

MitSciUnicodeTransformer guid: (GUID fromString: '{19b0d1e3-81e5-4fbd-b47b-587c4430d89a}')!
MitSciUnicodeTransformer comment: 'uct := MitSciUnicodeTransformer new.
uct scanForAnsiAPIFunctions.


uct addUnicodeMethodsFrom: GDILibrary to: GDILibraryW.

uct addUnicodeMethodsFrom: UserLibrary   to: UserLibraryW.

uct transformClassRefsFromCls: UserLibrary msgStr: ''default'' to: ''self userLibrary'' inAndAllSubClassesOf: View.
uct transformClassRefsFromCls:  String msgStr: ''new:'' to: ''self stringClass new:'' inAndAllSubClassesOf: View.

uct transformClassRefsFromCls: GDILibrary msgStr: ''default'' to: ''self gdiLibrary'' inAndAllSubClassesOf: Canvas.
uct transformSelfMsgStr: ''gdiLibrary '' to: ''self gdiLibraryForString:'' whithStringArgVarNameInAndAllSubClassesOf: Canvas.'!
!MitSciUnicodeTransformer categoriesForClass!Kernel-Objects! !
!MitSciUnicodeTransformer methodsFor!

addUnicodeMethodsFrom: srcLibaryClass  to: destLibClass

	| methods  newMethodStrCol |

	newMethodStrCol := OrderedCollection new.
	methods  := self  ansiAPIFunctionMethods select: [:each | each methodClass = srcLibaryClass ].
	
	"I'm just doing some quick and dirty string replacemnts.  There is theoretical potential for ambiguity, but that may not be an issue in practice.
		A clearner solution would be to use a proper parser for the code transforms."
	methods  do: [:each | | origSrc firstKeyword origMethodSig newSrc newFuncName helperStrm parser|  
		origSrc := each getSource.
		"OK, now I am (ab)using the Parser a little bit."
		parser := SmalltalkParser parseMethod: origSrc.
		firstKeyword := parser selectorParts first value readStream upTo: $:.
		origMethodSig := parser formattedCode readStream nextLine.
		"Make a new Unicode method."
		newSrc := origSrc  copyReplaceAll: firstKeyword with: firstKeyword  , 'W'.
		newFuncName := each functionName copy.
		newFuncName at: newFuncName  size put: $W.
		newSrc := newSrc copyReplaceAll: each functionName with: newFuncName .
		newMethodStrCol  add: newSrc.
		destLibClass compile: newSrc.
		"Make a helper/redirector method"
		helperStrm := String writeStream.
		helperStrm nextPutAll: origMethodSig; cr; cr; nextPutAll: '^self '; nextPutAll: (origMethodSig copyReplaceAll: firstKeyword with: firstKeyword  , 'W').
		destLibClass compile: helperStrm contents.

		].

	^newMethodStrCol.!

addUnicodeMethodsFrom: srcLibaryClass  to: destLibClass forFunctionNames: functionNames

	| methods  newMethodStrCol |

	newMethodStrCol := OrderedCollection new.
	methods  := (srcLibaryClass methodDictionary select: [:eachMethod |
		eachMethod isExternalCall and: [functionNames includes: eachMethod functionName]]).
	
	"I'm just doing some quick and dirty string replacemnts.  There is theoretical potential for ambiguity, but that may not be an issue in practice.
		A clearner solution would be to use a proper parser for the code transforms."
	methods  do: [:each | | origSrc firstKeyword origMethodSig newSrc newFuncName helperStrm parser|  
		origSrc := each getSource.
		"OK, now I am (ab)using the Parser a little bit."
		parser := SmalltalkParser parseMethod: origSrc.
		firstKeyword := parser selectorParts first value readStream upTo: $:.
		origMethodSig := parser formattedCode readStream nextLine.
		"Make a new Unicode method."
		newSrc := origSrc  copyReplaceAll: firstKeyword with: firstKeyword  , 'W'.
		newFuncName := each functionName , 'W'.
		newSrc := newSrc copyReplaceAll: each functionName with: newFuncName .
		newMethodStrCol  add: newSrc.
		destLibClass compile: newSrc.
		"Make a helper/redirector method"
		helperStrm := String writeStream.
		helperStrm nextPutAll: origMethodSig; cr; cr; nextPutAll: '^self '; nextPutAll: (origMethodSig copyReplaceAll: firstKeyword with: firstKeyword  , 'W').
		destLibClass compile: helperStrm contents.

		].

	^newMethodStrCol.!

applyAutomatedCodeTransformations
	"cdemers 11/18/2016"

	self transformClassRefsFromCls: UserLibrary msgStr: 'default' to: 'self userLibrary' inAndAllSubClassesOf: View.
	self transformClassRefsFromCls: String msgStr: 'new:' to: 'self stringClass new:' inAndAllSubClassesOf: View.
	self transformClassRefsFromCls: UserLibrary msgStr: 'default' to: 'self userLibrary' inAndAllSubClassesOf: GraphicsTool.
	self transformClassRefsFromCls: UserLibrary msgStr: 'default' to: 'self userLibrary' inAndAllSubClassesOf: MenuItem.
	self transformClassRefsFromCls: UserLibrary msgStr: 'default' to: 'self userLibrary' inAndAllSubClassesOf: Canvas.

	self transformClassRefsFromCls: GDILibrary msgStr: 'default' to: 'self gdiLibrary' inAndAllSubClassesOf: Canvas.

	self transformSelfMsgStr: 'userLibrary ' to: 'self userLibraryForString:' whithStringArgVarNameInAndAllSubClassesOf: Canvas.
	self transformSelfMsgStr: 'userLibrary '  to: 'self userLibraryForString:' whithStringArgVarNameInAndAllSubClassesOf: View.
	"N\A self transformSelfMsgStr: 'userLibrary '  to: 'self userLibraryForString:' whithStringArgVarNameInAndAllSubClassesOf: MenuItem."
	self transformSelfMsgStr: 'userLibrary '  to: 'self userLibraryForString:' whithStringArgVarNameInAndAllSubClassesOf: GraphicsTool.

	self transformSelfMsgStr: 'gdiLibrary ' to: 'self gdiLibraryForString:' whithStringArgVarNameInAndAllSubClassesOf: Canvas.
	ListView initializeNotificationMap.

	"ODBC Support"
	self transformClassRefsFromCls: ODBCLibrary msgStr: 'default' to: 'self odbcLibrary' inAndAllSubClassesOf: DBAbstractStatement.
	self transformClassRefsFromCls: ODBCLibrary msgStr: 'default' to: 'self odbcLibrary' inAndAllSubClassesOf: DBConnection.
	self transformClassRefsFromCls: String msgStr: 'new:' to: 'self stringClass new:' inAndAllSubClassesOf: DBConnection.
!

odbcUnicodeFunctionNames

	^#('SQLBrowseConnect' 'SQLGetDiagField' 'SQLColAttribute' 'SQLGetDiagRec' 'SQLColAttributes' 'SQLGetInfo' 'SQLColumnPrivileges' 'SQLGetStmtAttr' 'SQLColumns' 'SQLNativeSQL' 'SQLConnect' 'SQLPrepare' 'SQLDataSources' 'SQLPrimaryKeys' 'SQLDescribeCol' 'SQLProcedureColumns' 'SQLDriverConnect' 'SQLProcedures' 'SQLDrivers' 'SQLSetConnectAttr' 'SQLError' 'SQLSetConnectOption' 'SQLExecDirect' 'SQLSetCursorName' 'SQLForeignKeys' 'SQLSetDescField' 'SQLGetConnectAttr' 'SQLSetStmtAttr' 'SQLGetConnectOption' 'SQLSpecialColumns' 'SQLGetCursorName' 'SQLStatistics' 'SQLGetDescField' 'SQLTablePrivileges' 'SQLGetDescRec' 'SQLTables').!

transformClassRefsFromCls: clsToFind msgStr: msgStr to: destMsgString inAndAllSubClassesOf: aClass

	"cdemers 10/19/2016 Transform all refferences to [clsToFind] [msgStr] to [destMsgString] in aClass and all of its subclasses.
		Methods without that patern will not be changed.
		This method _can_ ruin a system because it makes changes to system methods.  There is no easy way to undo the changes.  Recovery would be to use a new image.
		We use class reffereces to find candidates for change, but ultimatly the changes are string replacements.  Parser based transforms would be more elegant."
	"cdemers 02/10/2017 Added some protection against doing a recursive code revision."

	| srcCol |

	srcCol := OrderedCollection new.

(SmalltalkSystem current referencesToVariable: ( SmalltalkSystem current globalVariableNamed: clsToFind name) in: (ClassEnvironment onEnvironment: SmalltalkSystem current systemEnvironment classes: ((OrderedCollection withAll: aClass allSubclasses) add: aClass; yourself ))) classesAndSelectorsDo: [:cls :selector | | curSource newSource |
		curSource := (cls methodDictionary at: selector) getSource. 
		newSource := curSource copyReplaceAll: clsToFind displayString , ' ' , msgStr with: destMsgString.
		(newSource ~= curSource and: [(destMsgString  endsWith: selector) not ]) ifTrue: [

			srcCol  add: cls displayString , '<<' ,  newSource.
			cls compile:  newSource] ].

	^srcCol !

transformSelfMsgStr: msgStr to: destMsgString whithStringArgVarNameInAndAllSubClassesOf: aClass

	"cdemers 10/31/2016 Transform all refferences to 'self [msgStr]' to  '([destMsgString] [strArgName]) ' in aClass and all of its subclasses when there is a method argument ending with 'String'.
		Methods without that patern will not be changed.
Example:
	uct transformSelfMsgStr: 'userLibrary '  to: 'self userLibraryForString:' whithStringArgVarNameInAndAllSubClassesOf: View.

		This method _can_ ruin a system because it makes changes to system methods.  There is no easy way to undo the changes.  Recovery would be to use a new image.
		The changes are string replacements.  Parser based transforms would be more elegant."
	| srcCol |

	srcCol := OrderedCollection new.

 (ClassEnvironment onEnvironment: SmalltalkSystem current systemEnvironment classes: ((OrderedCollection withAll: aClass allSubclasses) add: aClass; yourself )) classesAndSelectorsDo: [:cls :selector | | curSource newSource parser  strArgName |
		curSource := (cls methodDictionary at: selector) getSource. 
		parser := SmalltalkParser parseMethod: curSource.
		(strArgName := parser argumentNames detect: [:each | (each endsWith: 'String') or: [each = 'string'] ] ifNone: []) notNil ifTrue: [
		
		"firstKeyword := parser selectorParts first value readStream upTo: $:."
		newSource := curSource.
		(curSource findString: destMsgString) isZero ifTrue: [
			newSource := curSource copyReplaceAll: 'self ' , msgStr trimBlanks with: '(', destMsgString , ' ' , strArgName , ') '].
		newSource ~= curSource ifTrue: [

			srcCol  add: cls displayString , '<<' ,  newSource.
			cls compile:  newSource]] ].

	^srcCol! !
!MitSciUnicodeTransformer categoriesFor: #addUnicodeMethodsFrom:to:!public! !
!MitSciUnicodeTransformer categoriesFor: #addUnicodeMethodsFrom:to:forFunctionNames:!public! !
!MitSciUnicodeTransformer categoriesFor: #applyAutomatedCodeTransformations!public! !
!MitSciUnicodeTransformer categoriesFor: #odbcUnicodeFunctionNames!private! !
!MitSciUnicodeTransformer categoriesFor: #transformClassRefsFromCls:msgStr:to:inAndAllSubClassesOf:!public! !
!MitSciUnicodeTransformer categoriesFor: #transformSelfMsgStr:to:whithStringArgVarNameInAndAllSubClassesOf:!public! !

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
	('Read from DB: ' asUnicodeString , (recSet first at: 'TestUCString' asUnicodeString)) show. "show will display a UnicodeString in a Unicode MessageBox."
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

FlipperInspectorW guid: (GUID fromString: '{dca0775e-ca20-4d34-b4ac-b498f72811c1}')!
FlipperInspectorW comment: ''!
!FlipperInspectorW categoriesForClass!Development! !
!FlipperInspectorW class methodsFor!

isUnicodeMode

	^true.! !
!FlipperInspectorW class categoriesFor: #isUnicodeMode!public! !

TestNonUnicodeShell guid: (GUID fromString: '{2eb17404-7c0f-4ad3-bacc-1b33aac5f559}')!
TestNonUnicodeShell comment: 'self show'!
!TestNonUnicodeShell categoriesForClass!MVP-Presenters! !
!TestNonUnicodeShell class methodsFor!

isUnicodeMode

	^false! !
!TestNonUnicodeShell class categoriesFor: #isUnicodeMode!public! !

"Binary Globals"!

