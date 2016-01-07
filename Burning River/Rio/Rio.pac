| package |
package := Package name: 'Rio'.
package paxVersion: 0;
	basicComment: 'Portable ODBC database access for Smalltalk.

Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package imageStripperBytes: (ByteArray fromHexString: '2153544220312046101200040000004158446C6C496D61676553747269707065720000000000000000520000000300000052696F52000000190000004275726E696E672052697665725C52696F5C72696F2E646C6C9A0000000000000052000000160000004163746976655820444C4C20536572766572204B697452000000130000004158446C6C53657373696F6E4D616E61676572EFBF25000000000006030F0056657273696F6E5265736F75726365000000000601100056535F464958454446494C45494E464F000000007200000034000000BD04EFFE00000100000001000100000000000100010000003F000000000000000400000002000000000000000000000000000000EA00000000000000F0000000620000000200000052000000080000003034303930346230EA00000000000000F00000006200000018000000520000000E00000050726F6475637456657273696F6E520000000A000000312C20302C20302C2031520000000B000000436F6D70616E794E616D655200000000000000520000000C000000507269766174654275696C645200000000000000520000000C0000005370656369616C4275696C645200000000000000520000000F00000046696C654465736372697074696F6E5200000025000000446F6C7068696E20546F20476F20496E2D50726F6320434F4D205365727665722053747562520000000F0000004C6567616C54726164656D61726B735200000031000000446F6C7068696E20697320612074726164656D61726B206F66204347492047726F757020284575726F706529204C74642E520000000C000000496E7465726E616C4E616D65520000000F0000004950446F6C7068696E20546F20476F52000000100000004F726967696E616C46696C656E616D6552000000110000004950446F6C7068696E546F476F2E444C4C520000000E0000004C6567616C436F70797269676874520000002B000000506F7274696F6E7320436F7079726967687420A9204F626A656374204172747320313939372D323030322E5200000008000000436F6D6D656E7473520000001C000000506F776572656420627920446F6C7068696E20536D616C6C74616C6B520000000B00000046696C6556657273696F6E520000000A000000312C20302C20302C2031520000000B00000050726F647563744E616D6552000000170000004120446F6C7068696E20434F4D20436F6D706F6E656E74CA00000000000000D0000000620000000100000006020A0044574F524441727261790000000072000000040000000904B00403000000000000000000000000000000000000000000000000000000000000000000000000000000').
package basicScriptAt: #postinstall put: 'RioDatabase install.
RioStatement install.
RioRowsetBuffer install.
RioParameter install.
RioColumnHeader install.'.
package basicScriptAt: #preuninstall put: 'RioDatabase uninstall.
RioStatement uninstall.
RioRowsetBuffer uninstall.
RioParameter uninstall.
RioColumnHeader uninstall.'.

package classNames
	add: #RioAutoInsertStatement;
	add: #RioAutoStatement;
	add: #RioAutoUpdateStatement;
	add: #RioBigintColumnHeader;
	add: #RioBitColumnHeader;
	add: #RioCharacterColumnHeader;
	add: #RioColumnHeader;
	add: #RioCursor;
	add: #RioDatabase;
	add: #RioDateColumnHeader;
	add: #RioDecimalColumnHeader;
	add: #RioDoubleBuffer;
	add: #RioDoubleColumnHeader;
	add: #RioDwordBuffer;
	add: #RioDynamicCursor;
	add: #RioError;
	add: #RioExternalBuffer;
	add: #RioFatalError;
	add: #RioForwardCursor;
	add: #RioHandleBuffer;
	add: #RioIntegerColumnHeader;
	add: #RioIntegerParameter;
	add: #RioInternalBuffer;
	add: #RioKeysetCursor;
	add: #RioMetadataCursor;
	add: #RioOdbcLibrary;
	add: #RioParameter;
	add: #RioReadOnlyTransaction;
	add: #RioReadWriteTransaction;
	add: #RioRestartableError;
	add: #RioResult;
	add: #RioRow;
	add: #RioRowset;
	add: #RioRowsetBuffer;
	add: #RioRowsetHeader;
	add: #RioSdwordBuffer;
	add: #RioSmallintColumnHeader;
	add: #RioSqlGenerator;
	add: #RioStatement;
	add: #RioStaticCursor;
	add: #RioStringBuffer;
	add: #RioStringParameter;
	add: #RioSwordBuffer;
	add: #RioTestTransaction;
	add: #RioTimeColumnHeader;
	add: #RioTimeStampColumnHeader;
	add: #RioTransaction;
	add: #RioVarcharColumnHeader;
	add: #RioWordBuffer;
	yourself.

package methodNames
	add: #Date -> #odbcString;
	add: #Integer -> #columnHeaderFrom:;
	add: #String -> #columnHeaderFrom:;
	add: #Time -> #odbcString;
	add: #TimeStamp -> #odbcString;
	add: 'Date class' -> #fromOdbcString:;
	add: 'Time class' -> #fromOdbcString:;
	add: 'TimeStamp class' -> #fromOdbcString:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\PasswordPrompter\PasswordPrompter';
	add: 'RioConstants';
	yourself).

package!

"Class Definitions"!

Object subclass: #RioColumnHeader
	instanceVariableNames: 'cType dataSize name nullable precision scale sqlType offset index'
	classVariableNames: 'HeaderCreators HeaderInitializers'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioDatabase
	instanceVariableNames: 'hdbc functions retcode blobSize transaction statements finalizing connected'
	classVariableNames: 'Henv'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioExternalBuffer
	instanceVariableNames: 'nativeBuffer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioInternalBuffer
	instanceVariableNames: 'nativeBuffer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioParameter
	instanceVariableNames: 'parameterType cType sqlType precision scale value valueSize valueBytesAvailable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioResult
	instanceVariableNames: 'errorMsg nativeError sqlState'
	classVariableNames: 'ResultCollection'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioRowset
	instanceVariableNames: 'rowsetBuffer startRow nRows'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioRowsetBuffer
	instanceVariableNames: 'rowsetHeader data rowStatusArray nRows dbDirtyFlags uiDirtyFlags'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioRowsetHeader
	instanceVariableNames: 'columnHeaders columnHeadersByName bufferSize'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioSqlGenerator
	instanceVariableNames: 'tableName columnNames whereClause orderByColumnNames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioStatement
	instanceVariableNames: 'database hstmt sql parameters retcode blobSize finalizing prepared executed'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RioTransaction
	instanceVariableNames: 'db'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #RioError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioError subclass: #RioFatalError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioError subclass: #RioRestartableError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #RioOdbcLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioBigintColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioBitColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioCharacterColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioDateColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioDecimalColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioDoubleColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioIntegerColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioSmallintColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioTimeColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioColumnHeader subclass: #RioTimeStampColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioCharacterColumnHeader subclass: #RioVarcharColumnHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioInternalBuffer subclass: #RioDoubleBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioInternalBuffer subclass: #RioDwordBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioInternalBuffer subclass: #RioSdwordBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioInternalBuffer subclass: #RioStringBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioInternalBuffer subclass: #RioSwordBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioInternalBuffer subclass: #RioWordBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioDwordBuffer subclass: #RioHandleBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioParameter subclass: #RioIntegerParameter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioParameter subclass: #RioStringParameter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioRowset subclass: #RioRow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioStatement subclass: #RioAutoStatement
	instanceVariableNames: 'row tableName ignoreColumns'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioStatement subclass: #RioCursor
	instanceVariableNames: 'currentRowset rowsetHeader rowsetSize lastBindType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioAutoStatement subclass: #RioAutoInsertStatement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioAutoStatement subclass: #RioAutoUpdateStatement
	instanceVariableNames: 'dataParameterCount whereClause'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioCursor subclass: #RioDynamicCursor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioCursor subclass: #RioForwardCursor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioCursor subclass: #RioKeysetCursor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioCursor subclass: #RioStaticCursor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioStaticCursor subclass: #RioMetadataCursor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioTransaction subclass: #RioReadOnlyTransaction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioTransaction subclass: #RioReadWriteTransaction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RioReadWriteTransaction subclass: #RioTestTransaction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Date methodsFor!

odbcString
	| month day |
	month := self monthIndex displayString.
	month size = 1 ifTrue: [ month := '0', month ].
	day := self dayOfMonth displayString.
	day size = 1 ifTrue: [ day := '0', day ].
	^self year displayString, '-', month, '-', day
! !
!Date categoriesFor: #odbcString!converting!public! !

!Date class methodsFor!

fromOdbcString: aString
	"Answer a date corresponding to the ODBC date string 'aString', which should have the format
		yyyy-mm-dd"

	^(DateToText new format: 'yyyy-MM-dd') rightToLeft: aString! !
!Date class categoriesFor: #fromOdbcString:!instance creation!public! !

!Integer methodsFor!

columnHeaderFrom: anRioRowsetHeader
	^anRioRowsetHeader columnHeaders at: self ifAbsent: [nil]! !
!Integer categoriesFor: #columnHeaderFrom:!public! !

!String methodsFor!

columnHeaderFrom: anRioRowsetHeader
	^anRioRowsetHeader columnHeadersByName at: self ifAbsent: [nil]
! !
!String categoriesFor: #columnHeaderFrom:!accessing!public! !

!Time methodsFor!

odbcString
	^TimeToText new format: 'HH:mm:ss'; convertFromLeftToRight: self! !
!Time categoriesFor: #odbcString!converting!public! !

!Time class methodsFor!

fromOdbcString: aString
	"Answer a Time corresponding to the ODBC time string 'aString', which should have the format
		hh:mm:ss"
	^TimeToText new convertFromRightToLeft: aString! !
!Time class categoriesFor: #fromOdbcString:!public! !

!TimeStamp methodsFor!

odbcString
	^self date odbcString, ' ', self time odbcString! !
!TimeStamp categoriesFor: #odbcString!converting!public! !

!TimeStamp class methodsFor!

fromOdbcString: aString
	"Answer a TimeStamp corresponding to the ODBC timestamp string 'aString', which should
	have the format
			yyyy-mm-dd hh:mm:ss"
	| subStrings |

	subStrings := aString subStrings.
	^self
		date: (Date fromOdbcString: subStrings first)
		time: (Time fromOdbcString: (subStrings at: 2))
! !
!TimeStamp class categoriesFor: #fromOdbcString:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

RioColumnHeader guid: (GUID fromString: '{6BBB8F90-559A-11D3-8269-00001D19F5C2}')!
RioColumnHeader comment: '17-Oct-2000  RPJ  Changed extractValueFromBuffer:row: to answer decimal or numeric fields as ScaledDecimals instead
		of Strings.'!
!RioColumnHeader categoriesForClass!Rio! !
!RioColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	self subclassResponsibility!

clearDbDirtyFlagInBuffer: aRowsetBuffer row: anInteger
	aRowsetBuffer clearDbDirtyFlagForRow: anInteger column: self index!

clearUiDirtyFlagInBuffer: aRowsetBuffer row: anInteger
	aRowsetBuffer clearUiDirtyFlagForRow: anInteger column: self index!

cType
	^cType asInteger!

cType: anInteger
	cType := anInteger!

dataSize
	^dataSize!

dataSize: anInteger
	dataSize := anInteger asInteger!

extractDbDirtyFlagFromBuffer: aRowsetBuffer row: anInteger
	^aRowsetBuffer dbDirtyFlagForRow: anInteger column: self index!

extractUiDirtyFlagFromBuffer: aRowsetBuffer row: anInteger
	^aRowsetBuffer uiDirtyFlagForRow: anInteger column: self index!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	self subclassResponsibility!

index
	^index!

index: anInteger
	index := anInteger!

indicatorOffset
	"Answer the offset of the indicator value for this column"
	^offset + dataSize!

isQuotable
	"Answer true if printed versions of fields in this column should be surrounded by quotes."
	^false!

name
    ^name!

name: nameIn sqlType: sqlTypeIn precision: precisionIn scale: scaleIn nullable: nullableIn blobSize: aBlobSize index: anIntegerIndex
	| iSqlType headerInitializerBlock |

	name := nameIn asString deepCopy.
	sqlType := RioSwordBuffer new value: sqlTypeIn.
	iSqlType := sqlType asInteger.
	precision := RioDwordBuffer new value: precisionIn.
	scale := RioSwordBuffer new value: scaleIn.
	nullable := RioSwordBuffer new value: nullableIn.

	headerInitializerBlock := self class headerInitializers at: iSqlType ifAbsent: nil.
	headerInitializerBlock isNil ifTrue:
		[RioFatalError signal: 'RioColumnHeader>>name:sqlType:precision:scale:nullable: - ',
						'unsupported sqlType=', iSqlType asString ].
	headerInitializerBlock value: self value: aBlobSize.

	index := anIntegerIndex!

nullable
	"Answer whether data for this column header can be NULL.  Should
	be one of:
		SqlNoNulls - the column does not allow NULL values
		SqlNullable - the column allows NULL values
		SqlNullableUnknown - the nullability status of this column is unknown."
	^nullable asInteger!

offset
	^offset!

offset: anInteger
	offset := anInteger!

precision
	^precision asInteger!

printOn: aWriteStream
	"Print the definition of the column on aWriteStream"

	super printOn: aWriteStream.

	aWriteStream
		nextPut: $(;
		nextPutAll: self name;
		nextPutAll: '  datasize= ';
		nextPutAll: self dataSize printString;
		nextPutAll: '  nullable=';
		nextPutAll: self nullable asInteger printString;
		nextPutAll: '   precision=';
		nextPutAll: self precision asInteger printString;
		nextPutAll: '   scale=';
		nextPutAll: self scale asInteger printString;
		nextPutAll: '   sqlType=';
		nextPutAll: self sqlType asInteger printString;
		nextPutAll: '   offset=';
		nextPutAll: self offset printString;
		nextPut: $)!

scale
	^scale asInteger!

setDbDirtyFlagInBuffer: aRowsetBuffer row: anInteger
	aRowsetBuffer setDbDirtyFlagForRow: anInteger column: self index!

setIndicatorInBuffer: aRowsetBuffer row: anInteger value: anIntegerValue
	| indicatorOffset |

	indicatorOffset := (aRowsetBuffer rowOffset: anInteger) + self indicatorOffset.
	aRowsetBuffer sdwordAtOffset: indicatorOffset put: anIntegerValue!

setUiDirtyFlagInBuffer: aRowsetBuffer row: anInteger
	aRowsetBuffer setUiDirtyFlagForRow: anInteger column: self index!

setValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	"Set the value of this column in the specified row to the given value,
	answering the new value."

	| indicatorValue |

	"If the new value is nil set the indicator appropriately.  If this column
	can't be NULL signal an error."

	indicatorValue := RioConstants current sqlNullData.

	anObject isNil
		ifTrue: [ (self nullable = RioConstants current sqlNoNulls)
				ifTrue: [ RioFatalError signal:
						'RioColumnHeader>>setValueInBuffer:row:value: - ',
						'attempted to set non-nullable column ', self name, ' to NULL' ] ]
		ifFalse: [ indicatorValue := self basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject ].

	self setIndicatorInBuffer: aRowsetBuffer row: anInteger value: indicatorValue.
	self setDbDirtyFlagInBuffer: aRowsetBuffer row: anInteger.
	self setUiDirtyFlagInBuffer: aRowsetBuffer row: anInteger.

	^self extractValueFromBuffer: aRowsetBuffer row: anInteger!

sqlType
	^sqlType asInteger! !
!RioColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioColumnHeader categoriesFor: #clearDbDirtyFlagInBuffer:row:!dirty flags!public! !
!RioColumnHeader categoriesFor: #clearUiDirtyFlagInBuffer:row:!dirty flags!public! !
!RioColumnHeader categoriesFor: #cType!accessing!public! !
!RioColumnHeader categoriesFor: #cType:!accessing!private! !
!RioColumnHeader categoriesFor: #dataSize!accessing!public! !
!RioColumnHeader categoriesFor: #dataSize:!accessing!public! !
!RioColumnHeader categoriesFor: #extractDbDirtyFlagFromBuffer:row:!dirty flags!public! !
!RioColumnHeader categoriesFor: #extractUiDirtyFlagFromBuffer:row:!dirty flags!public! !
!RioColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !
!RioColumnHeader categoriesFor: #index!accessing!public! !
!RioColumnHeader categoriesFor: #index:!accessing!public! !
!RioColumnHeader categoriesFor: #indicatorOffset!accessing!public! !
!RioColumnHeader categoriesFor: #isQuotable!public!testing! !
!RioColumnHeader categoriesFor: #name!accessing!public! !
!RioColumnHeader categoriesFor: #name:sqlType:precision:scale:nullable:blobSize:index:!initializing!public! !
!RioColumnHeader categoriesFor: #nullable!accessing!public! !
!RioColumnHeader categoriesFor: #offset!accessing!public! !
!RioColumnHeader categoriesFor: #offset:!accessing!public! !
!RioColumnHeader categoriesFor: #precision!accessing!public! !
!RioColumnHeader categoriesFor: #printOn:!ANSI protocols-Object!public! !
!RioColumnHeader categoriesFor: #scale!accessing!public! !
!RioColumnHeader categoriesFor: #setDbDirtyFlagInBuffer:row:!dirty flags!public! !
!RioColumnHeader categoriesFor: #setIndicatorInBuffer:row:value:!accessing!public! !
!RioColumnHeader categoriesFor: #setUiDirtyFlagInBuffer:row:!dirty flags!public! !
!RioColumnHeader categoriesFor: #setValueInBuffer:row:value:!accessing!public! !
!RioColumnHeader categoriesFor: #sqlType!accessing!public! !

!RioColumnHeader class methodsFor!

clearHeaderCreators
	HeaderCreators := nil!

clearHeaderInitializers
	HeaderInitializers := nil!

createHeaderCreators
	| characterHeaderBlock decimalHeaderBlock doubleHeaderBlock varcharHeaderBlock |

	HeaderCreators := Dictionary new.

	characterHeaderBlock := [ RioCharacterColumnHeader new ].
	decimalHeaderBlock := [ RioDecimalColumnHeader new ].
	doubleHeaderBlock := [ RioDoubleColumnHeader new ].
	varcharHeaderBlock := [ RioVarcharColumnHeader new ].

	HeaderCreators
		at: RioConstants current sqlChar put: characterHeaderBlock;
		at: RioConstants current sqlVarchar put: varcharHeaderBlock;
		at: RioConstants current sqlLongvarchar put: varcharHeaderBlock;
		at: RioConstants current sqlBigint put: [ RioBigintColumnHeader new ];
		at: RioConstants current sqlBit put: [ RioBitColumnHeader new ];
		at: RioConstants current sqlDate put: [ RioDateColumnHeader new ];
		at: RioConstants current sqlDecimal put: decimalHeaderBlock;
		at: RioConstants current sqlNumeric put: decimalHeaderBlock;
		at: RioConstants current sqlDouble put: doubleHeaderBlock;
		at: RioConstants current sqlReal put: doubleHeaderBlock;
		at: RioConstants current sqlFloat put: doubleHeaderBlock;
		at: RioConstants current sqlInteger put: [ RioIntegerColumnHeader new ];
		at: RioConstants current sqlSmallint put: [ RioSmallintColumnHeader new ];
		at: RioConstants current sqlTime put: [ RioTimeColumnHeader new ];
		at: RioConstants current sqlTimestamp put: [ RioTimeStampColumnHeader new ]!

createHeaderInitializers
	| block |

	HeaderInitializers := Dictionary new.

	block := [ 	:aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCChar;
				dataSize: aColumnHeader precision + 1 ].
	HeaderInitializers
		at: RioConstants current sqlChar put: block;
		at: RioConstants current sqlVarchar put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCChar;
				dataSize: blobSize + 1 ].
	HeaderInitializers at: RioConstants current sqlLongvarchar put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCChar;
				dataSize: aColumnHeader precision + 3 ].
	HeaderInitializers
		at: RioConstants current sqlDecimal put: block;
		at: RioConstants current sqlNumeric put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCBit;
				dataSize: 1 ].
	HeaderInitializers at: RioConstants current sqlBit put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCStinyint;
				dataSize: 1 ].
	HeaderInitializers at: RioConstants current sqlTinyint put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCSshort;
				dataSize: 2 ].
	HeaderInitializers at: RioConstants current sqlSmallint put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCSlong;
				dataSize: 4 ].
	HeaderInitializers at: RioConstants current sqlInteger put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCChar;
				dataSize: 21 ].
	HeaderInitializers at: RioConstants current sqlBigint put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCDouble;
				dataSize: 8 ].
	HeaderInitializers
		at: RioConstants current sqlReal put: block;
		at: RioConstants current sqlFloat put: block;
		at: RioConstants current sqlDouble put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCBinary;
				dataSize: aColumnHeader precision ].
	HeaderInitializers
		at: RioConstants current sqlBinary put: block;
		at: RioConstants current sqlVarbinary put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCBinary;
				dataSize: blobSize ].
	HeaderInitializers at: RioConstants current sqlLongvarbinary put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCChar;
				dataSize: 11 ].
	HeaderInitializers at: RioConstants current sqlDate put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader
				cType: RioConstants current sqlCChar;
				dataSize: 10 ].
	HeaderInitializers at: RioConstants current sqlTime put: block.

	block := [ :aColumnHeader :blobSize |
			aColumnHeader cType: RioConstants current sqlCChar.
			aColumnHeader scale = 0
				ifTrue: [ aColumnHeader dataSize: 20 ]
				ifFalse: [ aColumnHeader dataSize: 21 + aColumnHeader scale ] ].
	HeaderInitializers at: RioConstants current sqlTimestamp put: block!

headerCreators
	^HeaderCreators!

headerFor: anIntegerSqlType
	| creationBlock |

	creationBlock := self headerCreators at: anIntegerSqlType ifAbsent: [ nil ].
	creationBlock isNil ifTrue: [ RioFatalError signal: 'RioColumnHeader class>>headerFor: - ',
										'unsupported sqlType=', anIntegerSqlType displayString ].
	^creationBlock value!

headerForName: nameIn sqlType: sqlTypeIn precision: precisionIn scale: scaleIn nullable: nullableIn blobSize: aBlobSize index: anIntegerIndex
	| header |

	header := self headerFor: sqlTypeIn asInteger.

	header
		name: nameIn
		sqlType: sqlTypeIn
		precision: precisionIn
		scale: scaleIn
		nullable: nullableIn
		blobSize: aBlobSize
		index: anIntegerIndex.
	^header!

headerInitializers
	^HeaderInitializers!

install
	self
		createHeaderCreators;
		createHeaderInitializers!

uninstall
	self
		clearHeaderInitializers;
		clearHeaderCreators! !
!RioColumnHeader class categoriesFor: #clearHeaderCreators!public!removing! !
!RioColumnHeader class categoriesFor: #clearHeaderInitializers!public!removing! !
!RioColumnHeader class categoriesFor: #createHeaderCreators!installing!public! !
!RioColumnHeader class categoriesFor: #createHeaderInitializers!installing!public! !
!RioColumnHeader class categoriesFor: #headerCreators!accessing!public! !
!RioColumnHeader class categoriesFor: #headerFor:!operations!public! !
!RioColumnHeader class categoriesFor: #headerForName:sqlType:precision:scale:nullable:blobSize:index:!instance creation!public! !
!RioColumnHeader class categoriesFor: #headerInitializers!accessing!public! !
!RioColumnHeader class categoriesFor: #install!installing!public! !
!RioColumnHeader class categoriesFor: #uninstall!public!removing! !

RioDatabase guid: (GUID fromString: '{6BBB8F92-559A-11D3-8269-00001D19F5C2}')!
RioDatabase comment: ''!
!RioDatabase categoriesForClass!Rio! !
!RioDatabase methodsFor!

addStatement: aRioStatement
	"Add a statement to the collection of statements active on this database"

	statements add: aRioStatement!

allocateConnectionHandle
	"Private - allocate an ODBC connection handle"
	self check: (self class dll
				sqlAllocConnect: self henv asParameter
				hdbc: self rawHdbc asParameter)!

basicDriverConnect: connectionString window: aWindow completionFlag: anIntegerCompletionFlag
	"Private - Connect to a data source using a connection string."

	| hwnd connectionStringOut bytesAvailable |

	self isConnected
		ifTrue: [ self retcode: RioConstants current sqlSuccess.
			  connectionStringOut := connectionString. ]
		ifFalse: [ aWindow isNil
				ifTrue: [ hwnd := RioHandleBuffer new value: 0 ]
				ifFalse: [ hwnd := aWindow handle ].

			connectionStringOut := String new: 256.

			bytesAvailable := RioSwordBuffer new value: connectionStringOut size - 1.

			self check: (self class dll
						sqlDriverConnect: self hdbc asParameter
						hwnd: hwnd asParameter
						connStrIn: connectionString asParameter
						connStrInLen: connectionString size asParameter
						connStrOut: connectionStringOut asParameter
						connStrOutSize: connectionStringOut size asParameter
						connStrOutBytesAvailable: bytesAvailable asParameter
						driverCompletionFlag: anIntegerCompletionFlag asParameter).

			self retcode = RioConstants current sqlSuccess
				ifTrue: [ self isConnected: true ] ].

	^connectionStringOut trimNulls!

blobSize
	"Answer the default BLOB allocation size for this database"
	^blobSize!

blobSize: anInteger
	"Set the default BLOB allocation size for this database"
	blobSize := anInteger!

browseConnect: connectString
	| connectStringOut outLen |

	self isConnected
		ifTrue: [ connectStringOut := connectString ]
		ifFalse:
			[ connectStringOut := String new: 1024.
			outLen := RioSwordBuffer new value: 0.

			self check: (self class dll
						sqlBrowseConnect: self hdbc asParameter
						connectionString: connectString asParameter
						connectionStringLen: connectString size asParameter
						connectionStringOut: connectStringOut asParameter
						connectStringOutSize: connectStringOut size asParameter
						connectStringOutLen: outLen asParameter).

			self retcode = RioConstants current sqlSuccess
				ifTrue: [ self isConnected: true ] ].

	^connectStringOut!

catalogs
	"Answer a collection containing all valid catalogs in this database."

	| rows |

	rows := (RioMetadataCursor on: self) tablesInCatalog: '%' schema: '' table: '' type: ''.
	^rows collect: [ :aRow | aRow TABLE_CAT ]!

check: anInteger
	self retcode: anInteger.
	^self processRetcode!

commit: aTransaction
	"Commit the active transaction on this connection"

	| localRetcode |
	localRetcode := RioConstants current sqlSuccess.

	self transaction == aTransaction ifTrue:
		[ localRetcode := self endTransaction: RioConstants current sqlCommit.
		statements do: [ :aStmt | aStmt afterCommit ].
		self transaction: nil ].
	^localRetcode!

connectOption: anIntegerOptionId
	"Answer the connect option requested.  The answered value will
	 be either an Integer or String, depending on the option."

	(anIntegerOptionId = RioConstants current sqlCurrentQualifier) |
			(anIntegerOptionId = RioConstants current sqlOptTracefile) |
			(anIntegerOptionId = RioConstants current sqlTranslateDll)
		ifTrue: [ ^self stringOption: anIntegerOptionId ]
		ifFalse: [ ^self integerOption: anIntegerOptionId ]!

connectOption: anOption value: param
	"Set the given connection option to the value specified in 'param'"

	^self check: (self class dll
						sqlSetConnectOption: self hdbc asParameter
						option: anOption asParameter
						param: param)!

createAutoInsertStatement: tableName
	"Create a new RioAutoInsertStatement for use with this database"

	^(RioAutoInsertStatement on: self) tableName: tableName!

createAutoUpdateStatement: tableName whereClause: aString ignoreColumns: anArray
	"Create a new RioAutoUpdateStatement for use with this database"

	^(RioAutoUpdateStatement on: self)
		tableName: tableName;
		whereClause: aString;
		ignoreColumns: anArray!

createDynamicCursor: anSqlString
	"Create a new RioDynamicCursor with the given SQL string,
	 answering the new RioDynamicCursor."

	^RioDynamicCursor on: self sql: anSqlString!

createForwardCursor: anSqlString
	"Create a new RioForwardCursor with the given SQL string,
	 answering the new RioForwardCursor."

	^RioForwardCursor on: self sql: anSqlString!

createKeysetCursor: anSqlString
	"Create a new RioKeysetCursor with the given SQL string,
	 answering the new RioKeysetCursor."

	^RioKeysetCursor on: self sql: anSqlString!

createStatement: anSqlString
	"Create a new RioStatement with the given SQL string,
	 answering the new RioStatement."

	^RioStatement on: self sql: anSqlString!

createStaticCursor: anSqlString
	"Create a new RioStaticCursor with the given SQL string,
	 answering the new RioStaticCursor."

	^RioStaticCursor on: self sql: anSqlString!

disconnect
	"Disconnect from the current data source"

	self isConnected
		ifTrue:
			[ self transaction notNil ifTrue:
				[ self transaction failureAction.
				self transaction: nil ].
			self dropAllStatements.
			self check: (self class dll sqlDisconnect: self rawHdbc asParameter).
			(self retcode = RioConstants current sqlSuccess) |
					(self retcode = RioConstants current sqlSuccessWithInfo)
				ifTrue: [ self isConnected: false ] ]
		ifFalse: [ self retcode: RioConstants current sqlSuccess ].

	^self retcode!

driverConnect: connectionString
	"Connect to a data source using a connection string."

	^self driverConnect: connectionString window: nil!

driverConnect: connectionString window: aWindow
	"Connect to a data source using a connection string."

	^self driverConnect: connectionString window: aWindow completionFlag: RioConstants current sqlDriverComplete!

driverConnect: connectionString window: aWindow completionFlag: anIntegerCompletionFlag
	"Connect to a data source using a connection string."

	^self basicDriverConnect: connectionString window: aWindow completionFlag: anIntegerCompletionFlag!

dropAllStatements
	"Private - drop any statements which exist on this database"

	statements do: [ :aStmt | aStmt basicDrop ]!

endTransaction: terminationType
	"Private - end the active transaction using the given termination
	type, which should be one of SqlCommit or SqlRollback."

	^self check: (self class dll
				sqlTransact: self henv asParameter
				hdbc: self hdbc asParameter
				type: terminationType asParameter)!

execute: anSqlString
	"Create a RioStatement for the given SQL string and execute it,
	 answering the new RioStatement."

	 ^(RioStatement on: self sql: anSqlString)
		execute;
		yourself!

finalize
	"Perform cleanup processing when this object
	 is about to be garbage collected."

	self finalizing: true.
	self release!

finalizing
	^finalizing!

finalizing: anObject
	finalizing := anObject!

freeConnection
	"Private - free the connection handle "

	| allocatedDatabases |

	(self rawHdbc = RioConstants current sqlNullHdbc)
		ifTrue: [ self retcode: RioConstants current sqlSuccess ]
		ifFalse:
			[ self check: (self class dll sqlFreeConnect: self rawHdbc asParameter).

			(self retcode = RioConstants current sqlSuccess) |
					(self retcode = RioConstants current sqlSuccessWithInfo)
				ifTrue: [ self rawHdbc: RioConstants current sqlNullHdbc ]].

	"If all database handles have been freed drop the environment handle"

(self class withAllSubclasses inject: Bag new into:
	[ :bag :aClass |
	bag addAll: (aClass allInstances select:
		[ :aDatabase |
		aDatabase rawHdbc value = RioConstants current sqlNullHdbc ]) ]) isEmpty ifTrue:
	[ self class releaseEnvironmentHandle ].

	^self retcode!

getFunctions
	"Private - create an array of Boolean values indicating which ODBC
	functions are supported on this connection"

	| buffer short tf |

	buffer := RioInternalBuffer new: 200.
	self check: (self class dll
				sqlGetFunctions: self hdbc asParameter
				function: RioConstants current sqlApiAllFunctions
				exists: buffer asParameter).
	(self retcode = RioConstants current sqlSuccess) | (self retcode = RioConstants current sqlSuccessWithInfo)
		ifTrue: [ "Build the 'functions' array"
			functions := Array new: 100.
			0 to: 198 by: 2 do: [ :i |
				(buffer wordAtOffset: i) ~= 0
					ifTrue: [tf := true]
					ifFalse: [tf := false].
				functions at: (i // 2) + 1 put: tf ] ].
	^functions!

getOption: anOption param: param
	"Private - invoke SQLGetConnectOption to retrieve a connection option"

	self check: (self class dll
				sqlGetConnectOption: self hdbc asParameter
				option: anOption asParameter
				param: param).
	^param!

hdbc
	"Answer the ODBC connection handle.  Allocate a new handle if
	currently unallocated."

	self rawHdbc asInteger = RioConstants current sqlNullHdbc
		ifTrue: [ self allocateConnectionHandle ].
	^self rawHdbc!

henv
	^self class henv!

initialize
	super initialize.

	self beFinalizable.
	self initializeDatabaseHandle.
	self blobSize: 100.	"Default allocation for all BLOB columns retrieved"
	statements := OrderedCollection new.
	self finalizing: false.
	self isConnected: false.!

initializeDatabaseHandle
	"Private - initialize the ODBC database handle to its 'unallocated' state."

	hdbc := RioHandleBuffer new value: RioConstants current sqlNullHdbc!

integerInfo: infoType
	"Answer the information type requested as an Integer"

	| buffer bytesAvailable |

	buffer := RioSdwordBuffer new value: 0.
	bytesAvailable := RioSwordBuffer new value: 0.
	self check: (self class dll
				sqlGetInfo: self hdbc asParameter
				infoType: (RioWordBuffer new value: infoType) asParameter
				buffer: buffer asParameter
				bufferLen: buffer size asParameter
				bytesAvailable: bytesAvailable asParameter).
	^buffer asInteger!

integerOption: anOption
	"Answer the integer connect option requested."

	| param |
	param := RioSdwordBuffer new.
	^(self getOption: anOption param: param) asInteger!

integerOption: anOption value: anInteger
	"Set the given connection option to the value specified in 'param'"

	| anSdword |
	anSdword := RioSdwordBuffer new value: anInteger.
	^self setOption: anOption value: anSdword!

isConnected
	^connected!

isConnected: aBoolean
	"Private - set the 'connected' flag."

	connected := aBoolean!

isTransactionActive
	^self transaction notNil!

nativeSql: anSqlStatement
	"Answer the native SQL translation of the statement given"

	| outString outStringBytesAvailable |
	outString := String new: anSqlStatement size * 2.
	outStringBytesAvailable := RioSdwordBuffer new value: 0.
	self check: (self class dll
				sqlNativeSql: self hdbc asParameter
				sqlStringIn: anSqlStatement asParameter
				sqlStringInLen: anSqlStatement size asParameter
				sqlStringOut: outString asParameter
				sqlStringOutSize: outString size asParameter
				sqlStringOutBytesAvailable: outStringBytesAvailable asParameter).
	^outString trimNulls!

odbcMajorVersion
	^(self odbcVersion leftString: 2) asNumber!

odbcMinorVersion
	^(self odbcVersion rightString: 2) asNumber!

odbcVersion
	^self stringInfo: (RioConstants current sqlOdbcVer)!

on: dataSource userId: userId authorization: authorization
	"Connect this object to the data source specified"

	self isConnected
		ifTrue: [ self disconnect ].

	self check: (self class dll
				sqlConnect: self hdbc asParameter
				dataSource: dataSource asParameter
				dataSourceLen: dataSource size asParameter
				userId: userId asParameter
				userIdLen: userId size asParameter
				authorization: authorization asParameter
				authorizationLen: authorization size asParameter).
	self retcode = RioConstants current sqlSuccess
		ifTrue:
			[ self isConnected: true.
			[ self
				integerOption: RioConstants current sqlAutocommit
				value: RioConstants current sqlAutocommitOff ] on: RioError do: [ :e | "ignore" ] ]!

owners
	"Answer a collection containing all valid owners in this database."

	| rows |

	rows := (RioCursor on: self) tablesInCatalog: '' schema: '%' table: '' type: ''.
	^rows collect: [ :aRow | aRow TABLE_OWNER ]!

prepare: anSqlString
	"Create a new RioStatement with the given SQL string and prepare it,
	 answering the new RioStatement.  Useful for statements with
	 parameter markers that require additional work (e.g. binding
	 values to the parameter markers) before execution."

	^(RioStatement on: self sql: anSqlString) prepare; yourself!

prepareCursor: anSqlString
	"Create a new RioCursor with the given SQL string and prepare it,
	 answering the new RioCursor."

	^(RioCursor on: self sql: anSqlString) prepare; yourself!

primaryKeyColumnNames: aStringTableName
	^(self primaryKeysFor: aStringTableName) collect: [ :aRow | aRow at: 'COLUMN_NAME' ]!

primaryKeysFor: aStringTableName
	| stmt rows |

	self readOnlyTransaction try: [
		stmt := RioForwardCursor on: self.
		rows := stmt primaryKeysInCatalog: '' schema: '' table: aStringTableName.
		stmt drop ].

	^rows!

processRetcode
	"Private - process the return code from an ODBC function call"

	| results |

	RioResult reset.
	(self retcode = RioConstants current sqlError) | (self retcode = RioConstants current sqlSuccessWithInfo)
		ifTrue: [ RioResult fromEnv: self henv hdbc: self rawHdbc].

	"Signal an appropriate error if problems were encountered unless
	 we're in the midst of finalizing this object, in which case we just ignore
	 the error."

	((self retcode = RioConstants current sqlError) |
			(self retcode = RioConstants current sqlInvalidHandle)) &
			(self finalizing not)
		ifTrue:
			[ results := RioResult allResults.
			RioResult isRestartable
				ifTrue: [ RioRestartableError signal: 'RioDatabase>>processRetcode - ',
											'a restartable ODBC error occurred' ]
				ifFalse: [RioFatalError signal: 'RioDatabase>>processRetcode - ',
											'a fatal ODBC error occurred' ] ].

	^self retcode!

rawHdbc
	"answer hdbc without trying to establish a connection"
	^hdbc!

rawHdbc: anInteger
	self rawHdbc value: anInteger!

readOnlyTransaction
	^RioReadOnlyTransaction on: self!

readWriteTransaction
	^RioReadWriteTransaction on: self!

release
	"Private - release any system resources held by this object"

	self
		disconnect;
		freeConnection.
	SessionManager current
		removeEventsTriggeredFor: self.
	super release!

removeStatement: aStmt
	"Remove a statement from the collection of statements
	 active on this database."

	statements remove: aStmt ifAbsent: [ ]!

retcode
	^retcode!

retcode: anObject
	retcode := anObject!

rollback: aTransaction
	| localRetcode |
	localRetcode := RioConstants current sqlSuccess.

	self transaction == aTransaction
		ifTrue: [
			localRetcode := self endTransaction: RioConstants current sqlRollback.
			statements do: [ :aStmt | aStmt afterRollback ].
			self transaction: nil ].
	^localRetcode!

setOption: anOption value: param
	"Private - set the given connection option to the value specified
	 in 'param', which should be either an ExternalLong or a String."

	^self check: (self class dll
				sqlSetConnectOption: self hdbc asParameter
				option: anOption asParameter
				param: param asParameter)!

shutdown
	"Private - perform actions appropriate when the image is shutting down.  For
	 now this entails:
		1.  Shut down all statements associated with this database.
		2.  Release the database handle.
	 Invoked at image shutdown by RioDatabase class>>shutdown"

	| save |

	statements do: [ :aStmt | aStmt shutdown ].
	save := self finalizing.
	self finalizing: true.
	self disconnect;
		freeConnection.
	self finalizing: save.!

started
	"Private - invoked at image startup by RioDatabase class>>started"

	self
		initializeDatabaseHandle;
		isConnected: false;
		retcode: RioConstants current sqlSuccess!

startTransaction: aTransaction
	self transaction isNil ifTrue:
		[ self
			connectOption: RioConstants current sqlAccessMode value: aTransaction accessMode;
			transaction: aTransaction ].
	^self transaction!

stringInfo: infoType
	"Answer the information type requested as a String"
	| buffer bytesAvailable |

	buffer := String new: 1025.
	bytesAvailable := RioSwordBuffer new value: 0.
	self check: (self class dll
				sqlGetInfo: self hdbc asParameter
				infoType: (RioWordBuffer new value: infoType) asParameter
				buffer: buffer asParameter
				bufferLen: buffer size - 1 asParameter
				bytesAvailable: bytesAvailable asParameter).
	^buffer trimNulls!

stringOption: anOption
		"Answer the string connect option requested."
	| param |
	param := String new: RioConstants current sqlMaxOptionStringLength + 1.
	^(self getOption: anOption param: param) trimNulls!

stringOption: anOption value: param
		"Set the given connection option to the value specified in 'param'"
	^self setOption: anOption value: param asString!

supportsFunction: aFunction
		"Answer a Boolean indicating whether or not the specified
		 function is supported on this connection"

	functions isNil
		ifTrue: [self getFunctions].
	^functions at: (aFunction + 1)!

tables
	"Answer a collection of rows containing information on all tables in this database"

	^(RioMetadataCursor on: self) tablesInCatalog: nil schema: nil table: nil type: nil!

tableTypes
	"Answer a collection containing all valid table types in this database."

	| rows |

	rows := (RioMetadataCursor on: self) tablesInCatalog: '' schema: '' table: '' type: '%'.
	^rows collect: [ :aRow | aRow TABLE_TYPE ]!

transaction
	"Answer the value of the receiver's instance variable transaction.
	This method was automatically generated, but may be modified."

	^transaction!

transaction: anObject
	"Set the value of the receiver's instance variable transaction to anObject.
	This method was automatically generated, but may be modified."

	transaction := anObject! !
!RioDatabase categoriesFor: #addStatement:!public!statements! !
!RioDatabase categoriesFor: #allocateConnectionHandle!private!private helpers! !
!RioDatabase categoriesFor: #basicDriverConnect:window:completionFlag:!connecting!private! !
!RioDatabase categoriesFor: #blobSize!accessing!public! !
!RioDatabase categoriesFor: #blobSize:!accessing!public! !
!RioDatabase categoriesFor: #browseConnect:!connecting!public! !
!RioDatabase categoriesFor: #catalogs!information!public! !
!RioDatabase categoriesFor: #check:!operations!public! !
!RioDatabase categoriesFor: #commit:!public!transactions! !
!RioDatabase categoriesFor: #connectOption:!options!public! !
!RioDatabase categoriesFor: #connectOption:value:!options!public! !
!RioDatabase categoriesFor: #createAutoInsertStatement:!public!statement creation! !
!RioDatabase categoriesFor: #createAutoUpdateStatement:whereClause:ignoreColumns:!public!statement creation! !
!RioDatabase categoriesFor: #createDynamicCursor:!public!statement creation! !
!RioDatabase categoriesFor: #createForwardCursor:!public!statement creation! !
!RioDatabase categoriesFor: #createKeysetCursor:!public!statement creation! !
!RioDatabase categoriesFor: #createStatement:!public!statement creation! !
!RioDatabase categoriesFor: #createStaticCursor:!public!statement creation! !
!RioDatabase categoriesFor: #disconnect!connecting!public! !
!RioDatabase categoriesFor: #driverConnect:!connecting!public! !
!RioDatabase categoriesFor: #driverConnect:window:!connecting!public! !
!RioDatabase categoriesFor: #driverConnect:window:completionFlag:!connecting!public! !
!RioDatabase categoriesFor: #dropAllStatements!private!private helpers! !
!RioDatabase categoriesFor: #endTransaction:!private!private helpers! !
!RioDatabase categoriesFor: #execute:!public!statements! !
!RioDatabase categoriesFor: #finalize!finalizing!public! !
!RioDatabase categoriesFor: #finalizing!accessing!public! !
!RioDatabase categoriesFor: #finalizing:!accessing!private! !
!RioDatabase categoriesFor: #freeConnection!private!private helpers! !
!RioDatabase categoriesFor: #getFunctions!private!private helpers! !
!RioDatabase categoriesFor: #getOption:param:!private!private helpers! !
!RioDatabase categoriesFor: #hdbc!accessing!public! !
!RioDatabase categoriesFor: #henv!accessing!public! !
!RioDatabase categoriesFor: #initialize!initializing!public! !
!RioDatabase categoriesFor: #initializeDatabaseHandle!private!private helpers! !
!RioDatabase categoriesFor: #integerInfo:!information!public! !
!RioDatabase categoriesFor: #integerOption:!options!public! !
!RioDatabase categoriesFor: #integerOption:value:!options!public! !
!RioDatabase categoriesFor: #isConnected!accessing!public! !
!RioDatabase categoriesFor: #isConnected:!private!private helpers! !
!RioDatabase categoriesFor: #isTransactionActive!accessing!public! !
!RioDatabase categoriesFor: #nativeSql:!accessing!public! !
!RioDatabase categoriesFor: #odbcMajorVersion!accessing!public! !
!RioDatabase categoriesFor: #odbcMinorVersion!accessing!public! !
!RioDatabase categoriesFor: #odbcVersion!operations!public! !
!RioDatabase categoriesFor: #on:userId:authorization:!connecting!public! !
!RioDatabase categoriesFor: #owners!information!public! !
!RioDatabase categoriesFor: #prepare:!public!statement creation! !
!RioDatabase categoriesFor: #prepareCursor:!public!statement creation! !
!RioDatabase categoriesFor: #primaryKeyColumnNames:!accessing!public! !
!RioDatabase categoriesFor: #primaryKeysFor:!accessing!public! !
!RioDatabase categoriesFor: #processRetcode!private!private helpers! !
!RioDatabase categoriesFor: #rawHdbc!accessing!public! !
!RioDatabase categoriesFor: #rawHdbc:!accessing!private! !
!RioDatabase categoriesFor: #readOnlyTransaction!public!transactions! !
!RioDatabase categoriesFor: #readWriteTransaction!public!transactions! !
!RioDatabase categoriesFor: #release!private!private helpers! !
!RioDatabase categoriesFor: #removeStatement:!public!statements! !
!RioDatabase categoriesFor: #retcode!accessing!public! !
!RioDatabase categoriesFor: #retcode:!accessing!public! !
!RioDatabase categoriesFor: #rollback:!public!transactions! !
!RioDatabase categoriesFor: #setOption:value:!options!private! !
!RioDatabase categoriesFor: #shutdown!private!private helpers! !
!RioDatabase categoriesFor: #started!private!private helpers! !
!RioDatabase categoriesFor: #startTransaction:!public!transactions! !
!RioDatabase categoriesFor: #stringInfo:!information!public! !
!RioDatabase categoriesFor: #stringOption:!options!public! !
!RioDatabase categoriesFor: #stringOption:value:!options!public! !
!RioDatabase categoriesFor: #supportsFunction:!information!public! !
!RioDatabase categoriesFor: #tables!information!public! !
!RioDatabase categoriesFor: #tableTypes!information!public! !
!RioDatabase categoriesFor: #transaction!accessing!public! !
!RioDatabase categoriesFor: #transaction:!accessing!public! !

!RioDatabase class methodsFor!

allocateEnvironmentHandle
	"Private: allocates an ODBC environment handle"

	| retcode |
	RioResult reset.
	retcode := RioDatabase dll sqlAllocEnv: Henv asParameter.
	retcode = RioConstants current sqlError ifTrue:
		[ RioResult fromEnv: self henv.
		RioFatalError signal: 'RioDatabase class>>allocateEnvironmentHandle: handle allocation failed'].
	^retcode!

dll
	^RioOdbcLibrary default!

driverConnect: connectionString window: aWindow
	| db |

	db := self new.
	db driverConnect: connectionString window: aWindow.
	^db!

finalize
	SessionManager current removeEventsTriggeredFor: self.
	super finalize!

henv
	"Answer the ODBC environment handle, allocating it if needed"

	Henv asInteger = RioConstants current sqlNullHenv ifTrue: [ self allocateEnvironmentHandle ].
	^Henv!

install
	"Register for events of interest when the package with this class in it is installed"

	SessionManager current
		when: #sessionStarted send: #started to: self;
		when: #sessionStopped send: #shutdown to: self.

	self beFinalizable.
	self started!

new
	^super new initialize!

on: dataSource
	| userId |

	userId := Prompter prompt: 'Please enter your user ID'.
	^self on: dataSource userId: userId!

on: dataSource userId: userId
	| authorization |

	authorization := PasswordPrompter prompt: 'Please enter your password'.
	^self on: dataSource userId: userId authorization: authorization!

on: dataSource userId: userId authorization: authorization
	^self new on: dataSource userId: userId authorization: authorization; yourself!

rawHenv
	^Henv!

releaseEnvironmentHandle
	"Private - release the environment handle, if allocated"
	
	Henv value = RioConstants current sqlNullHenv ifFalse:
		[ RioDatabase dll sqlFreeEnv: Henv asParameter.
		Henv value: RioConstants current sqlNullHenv ]!

shutdown
	"Private - the image is about to shut down.  Notify all instances of this class and its subclasses."

	self withAllSubclassesDo: [ :aClass | aClass allInstances do: [ :db | db shutdown ] ].
	self releaseEnvironmentHandle!

started
	"Clear the class environment handle at image startup."

	Henv := RioHandleBuffer new value: RioConstants current sqlNullHenv.
	self withAllSubclassesDo: [ :aClass | aClass allInstances do: [ :db | db started ] ]!

uninstall
	Henv := nil.
	self finalize! !
!RioDatabase class categoriesFor: #allocateEnvironmentHandle!private!private helpers! !
!RioDatabase class categoriesFor: #dll!accessing!public! !
!RioDatabase class categoriesFor: #driverConnect:window:!instance creation!public! !
!RioDatabase class categoriesFor: #finalize!finalizing!public! !
!RioDatabase class categoriesFor: #henv!accessing!public! !
!RioDatabase class categoriesFor: #install!installing!public! !
!RioDatabase class categoriesFor: #new!instance creation!public! !
!RioDatabase class categoriesFor: #on:!instance creation!public! !
!RioDatabase class categoriesFor: #on:userId:!instance creation!public! !
!RioDatabase class categoriesFor: #on:userId:authorization:!instance creation!public! !
!RioDatabase class categoriesFor: #rawHenv!accessing!public! !
!RioDatabase class categoriesFor: #releaseEnvironmentHandle!private!private helpers! !
!RioDatabase class categoriesFor: #shutdown!private!private helpers! !
!RioDatabase class categoriesFor: #started!private helpers!public! !
!RioDatabase class categoriesFor: #uninstall!installing!public! !

RioExternalBuffer guid: (GUID fromString: '{6BBB8F93-559A-11D3-8269-00001D19F5C2}')!
RioExternalBuffer comment: 'Represents a data buffer which is external to the Smalltalk image, is not subject to being moved around by the garbage collector, and thus is safe to use with non-Smalltalk database functions which commonly take the address of a buffer as an argument and assume that this buffer will not move around in memory during program execution.'!
!RioExternalBuffer categoriesForClass!Rio! !
!RioExternalBuffer methodsFor!

address
	^self nativeBuffer yourAddress!

byteAtOffset: anIntegerOffset
	^self nativeBuffer byteAtOffset: anIntegerOffset!

byteAtOffset: anIntegerOffset put: anObject
	self nativeBuffer byteAtOffset: anIntegerOffset put: anObject!

bytesAtOffset: anIntegerOffset count: anIntegerCount
	^(ByteArray new: anIntegerCount)
		replaceFrom: 1
		to: anIntegerCount
		with: self nativeBuffer
		startingAt: anIntegerOffset+1!

bytesAtOffset: anIntegerStartOffset count: anIntegerCount put: anObject
	self nativeBuffer
		replaceFrom: anIntegerStartOffset + 1
		to: anIntegerStartOffset + anIntegerCount 
		with: anObject
		startingAt: 1!

clear
	"Clear the contents of the buffer - i.e. reset to an empty buffer"

	0 to: self size - 1 do: [ :i | self byteAtOffset: i put: 0 ]
!

copyInternalBuffer: anInternalBuffer startingAtOffset: internalBufferOffset toOffset: toOffset count: count
	self nativeBuffer
		replaceFrom: toOffset + 1
		to: toOffset + count
		with: (anInternalBuffer bytesAtOffset: internalBufferOffset count: count)
		startingAt: 1!

doubleAtOffset: anInteger
	^self nativeBuffer doubleAtOffset: anInteger
!

doubleAtOffset: anInteger put: aDouble
	self nativeBuffer doubleAtOffset: anInteger put: aDouble!

dwordAtOffset: anInteger
	^self nativeBuffer dwordAtOffset: anInteger!

dwordAtOffset: anInteger put: anObject
	self nativeBuffer dwordAtOffset: anInteger put: anObject!

nativeBuffer
	^nativeBuffer!

nativeBuffer: aByteArray
	nativeBuffer := aByteArray!

sdwordAtOffset: anInteger
	^self nativeBuffer sdwordAtOffset: anInteger!

sdwordAtOffset: anInteger put: anObject
	self nativeBuffer sdwordAtOffset: anInteger put: anObject!

size
	^self nativeBuffer size!

stringAtOffset: anInteger count: anInteger2
	^(self bytesAtOffset: anInteger count: anInteger2) asString!

stringAtOffset: anInteger count: anInteger2 put: anObject
	self bytesAtOffset: anInteger count: anInteger2 put: anObject!

swordAtOffset: anInteger
	^self nativeBuffer swordAtOffset: anInteger!

swordAtOffset: anInteger put: anObject
	self nativeBuffer swordAtOffset: anInteger put: anObject!

wordAtOffset: anInteger
	^self nativeBuffer wordAtOffset: anInteger!

wordAtOffset: anInteger put: anObject
	self nativeBuffer wordAtOffset: anInteger put: anObject! !
!RioExternalBuffer categoriesFor: #address!accessing!public! !
!RioExternalBuffer categoriesFor: #byteAtOffset:!accessing!public! !
!RioExternalBuffer categoriesFor: #byteAtOffset:put:!accessing!public! !
!RioExternalBuffer categoriesFor: #bytesAtOffset:count:!accessing!public! !
!RioExternalBuffer categoriesFor: #bytesAtOffset:count:put:!accessing!public! !
!RioExternalBuffer categoriesFor: #clear!operations!public! !
!RioExternalBuffer categoriesFor: #copyInternalBuffer:startingAtOffset:toOffset:count:!operations!public! !
!RioExternalBuffer categoriesFor: #doubleAtOffset:!accessing!public! !
!RioExternalBuffer categoriesFor: #doubleAtOffset:put:!accessing!public! !
!RioExternalBuffer categoriesFor: #dwordAtOffset:!accessing!public! !
!RioExternalBuffer categoriesFor: #dwordAtOffset:put:!accessing!public! !
!RioExternalBuffer categoriesFor: #nativeBuffer!accessing!private! !
!RioExternalBuffer categoriesFor: #nativeBuffer:!accessing!private! !
!RioExternalBuffer categoriesFor: #sdwordAtOffset:!accessing!public! !
!RioExternalBuffer categoriesFor: #sdwordAtOffset:put:!accessing!public! !
!RioExternalBuffer categoriesFor: #size!accessing!public! !
!RioExternalBuffer categoriesFor: #stringAtOffset:count:!accessing!public! !
!RioExternalBuffer categoriesFor: #stringAtOffset:count:put:!accessing!public! !
!RioExternalBuffer categoriesFor: #swordAtOffset:!accessing!public! !
!RioExternalBuffer categoriesFor: #swordAtOffset:put:!accessing!public! !
!RioExternalBuffer categoriesFor: #wordAtOffset:!accessing!public! !
!RioExternalBuffer categoriesFor: #wordAtOffset:put:!accessing!public! !

!RioExternalBuffer class methodsFor!

new: anInteger
	^super new initialize nativeBuffer: (ByteArray newFixed: anInteger)! !
!RioExternalBuffer class categoriesFor: #new:!instance creation!public! !

RioInternalBuffer guid: (GUID fromString: '{6BBB8F94-559A-11D3-8269-00001D19F5C2}')!
RioInternalBuffer comment: 'Represents a data buffer inside the Smalltalk image.  Instances of this class are used to hold copies of the external buffers (class RioExternalBuffer), which reside outside the image, when the image is saved, thus allowing the data from the database to be saved with the image, and to later be accessed.'!
!RioInternalBuffer categoriesForClass!Rio! !
!RioInternalBuffer methodsFor!

asParameter
	^self nativeBuffer asParameter!

byteAtOffset: anInteger
	^self nativeBuffer byteAtOffset: anInteger!

byteAtOffset: anInteger put: anObject
	self nativeBuffer byteAtOffset: anInteger put: anObject!

bytesAtOffset: anInteger count: anInteger2
	^(ByteArray new: anInteger2) replaceFrom: 1 to: anInteger2 with: self nativeBuffer startingAt: anInteger+1!

bytesAtOffset: anInteger count: anInteger2 put: anObject
	| aByteArray |

	aByteArray := ByteArray new: anInteger2.
	aByteArray size <= anObject size
		ifTrue: [ aByteArray replaceFrom: 1 to: anInteger2 with: anObject startingAt: 1]
		ifFalse: [ aByteArray replaceFrom: 1 to: anObject size with: anObject startingAt: 1].

	self nativeBuffer bytesAtOffset: anInteger put: aByteArray!

clear
	"Clear the contents of the buffer - i.e. reset to an empty buffer"

	0 to: self size - 1 do: [ :i | self byteAtOffset: i put: 0 ]!

copyExternalBuffer: anExternalBuffer startingAtOffset: externalBufferOffset toInternalOffset: toOffset count: count
	"We assume here that both RioExternalBuffer and RioInternalBuffer
	 use a ByteArray as the native buffer class."

	self nativeBuffer
		bytesAtOffset: toOffset
		put: (anExternalBuffer nativeBuffer
					copyFrom: externalBufferOffset+1
					to: externalBufferOffset + count)!

doubleAtOffset: anInteger
	^self nativeBuffer doubleAtOffset: anInteger!

doubleAtOffset: anInteger put: aDouble
	self nativeBuffer doubleAtOffset: anInteger put: aDouble!

dwordAtOffset: anInteger
	^self nativeBuffer dwordAtOffset: anInteger!

dwordAtOffset: anInteger put: anObject
	self nativeBuffer dwordAtOffset: anInteger put: anObject!

nativeBuffer
	"Private - answer the native-system buffer used to store data"
	^nativeBuffer!

nativeBuffer: aByteArray
	"Private - set the native-system buffer used to store data"
	nativeBuffer := aByteArray!

printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self value printString;
		nextPut: $)!

sdwordAtOffset: anInteger
	^self nativeBuffer sdwordAtOffset: anInteger!

sdwordAtOffset: anInteger put: anObject
	self nativeBuffer sdwordAtOffset: anInteger put: anObject!

size
	^self nativeBuffer size!

stringAtOffset: anIntegerOffset count: anIntegerCount
	^(self bytesAtOffset: anIntegerOffset count: anIntegerCount) asString!

stringAtOffset: anInteger count: anInteger2 put: aString
	self bytesAtOffset: anInteger count: anInteger2 put: aString!

swordAtOffset: anInteger
	^self nativeBuffer swordAtOffset: anInteger!

swordAtOffset: anInteger put: anObject
	self nativeBuffer swordAtOffset: anInteger put: anObject!

wordAtOffset: anInteger
	^self nativeBuffer wordAtOffset: anInteger!

wordAtOffset: anInteger put: anObject
	self nativeBuffer wordAtOffset: anInteger put: anObject! !
!RioInternalBuffer categoriesFor: #asParameter!converting!public! !
!RioInternalBuffer categoriesFor: #byteAtOffset:!accessing!public! !
!RioInternalBuffer categoriesFor: #byteAtOffset:put:!accessing!public! !
!RioInternalBuffer categoriesFor: #bytesAtOffset:count:!accessing!public! !
!RioInternalBuffer categoriesFor: #bytesAtOffset:count:put:!accessing!public! !
!RioInternalBuffer categoriesFor: #clear!operations!public! !
!RioInternalBuffer categoriesFor: #copyExternalBuffer:startingAtOffset:toInternalOffset:count:!operations!public! !
!RioInternalBuffer categoriesFor: #doubleAtOffset:!accessing!public! !
!RioInternalBuffer categoriesFor: #doubleAtOffset:put:!accessing!public! !
!RioInternalBuffer categoriesFor: #dwordAtOffset:!accessing!public! !
!RioInternalBuffer categoriesFor: #dwordAtOffset:put:!accessing!public! !
!RioInternalBuffer categoriesFor: #nativeBuffer!private!private helpers! !
!RioInternalBuffer categoriesFor: #nativeBuffer:!private!private helpers! !
!RioInternalBuffer categoriesFor: #printOn:!printing!public! !
!RioInternalBuffer categoriesFor: #sdwordAtOffset:!accessing!public! !
!RioInternalBuffer categoriesFor: #sdwordAtOffset:put:!accessing!public! !
!RioInternalBuffer categoriesFor: #size!accessing!public! !
!RioInternalBuffer categoriesFor: #stringAtOffset:count:!accessing!public! !
!RioInternalBuffer categoriesFor: #stringAtOffset:count:put:!accessing!public! !
!RioInternalBuffer categoriesFor: #swordAtOffset:!accessing!public! !
!RioInternalBuffer categoriesFor: #swordAtOffset:put:!accessing!public! !
!RioInternalBuffer categoriesFor: #wordAtOffset:!accessing!public! !
!RioInternalBuffer categoriesFor: #wordAtOffset:put:!accessing!public! !

!RioInternalBuffer class methodsFor!

new: anInteger
	^super new initialize nativeBuffer: (ByteArray new: anInteger)! !
!RioInternalBuffer class categoriesFor: #new:!instance creation!public! !

RioParameter guid: (GUID fromString: '{6BBB8F95-559A-11D3-8269-00001D19F5C2}')!
RioParameter comment: ''!
!RioParameter categoriesForClass!Rio! !
!RioParameter methodsFor!

aboutToSaveImage
	"Private"
	| anInternalBuffer |

	value notNil ifTrue:
		[ anInternalBuffer := RioInternalBuffer new: self valueSize.
		anInternalBuffer bytesAtOffset: 0 count: value size put: value nativeBuffer.
		value := anInternalBuffer ]!

cType
	^cType!

cType: anInteger
	cType := anInteger!

initialize
	super initialize.
	self parameterType: RioConstants current sqlParamInput.!

parameterType
	^parameterType!

parameterType: anInteger
	parameterType := anInteger!

precision
	^precision!

precision: anInteger
	precision := anInteger!

printOn: aStream
	super printOn: aStream.
	aStream
		cr;
		tab; nextPutAll: 'cType='; nextPutAll: self cType printString; cr;
		tab; nextPutAll: 'parameterType='; nextPutAll: self parameterType printString; cr;
		tab; nextPutAll: 'precision='; nextPutAll: self precision printString; cr;
		tab; nextPutAll: 'scale='; nextPutAll: self scale printString; cr;
		tab; nextPutAll: 'sqlType='; nextPutAll: self sqlType printString; cr;
		tab; nextPutAll: 'valueSize='; nextPutAll: self valueSize printString; cr;
		tab; nextPutAll: 'valueBytesAvailable='; nextPutAll: self valueBytesAvailable printString;
		cr.!

savedImage
	| anExternalBuffer |

	value notNil ifTrue:
		[ anExternalBuffer := RioExternalBuffer new: self valueSize.
		anExternalBuffer bytesAtOffset: 0 count: self valueSize put: value nativeBuffer.
		value := anExternalBuffer ]!

scale
	^scale!

scale: anInteger
	scale := anInteger!

sqlType
	^sqlType!

sqlType: anInteger
	sqlType := anInteger!

started
	self savedImage!

value
	^value!

value: anObject
	self subclassResponsibility!

valueBytesAvailable
	^valueBytesAvailable!

valueBytesAvailable: anInteger
	valueBytesAvailable isNil
		ifTrue: [ valueBytesAvailable := RioExternalBuffer new: 4 ].

	valueBytesAvailable sdwordAtOffset: 0 put: anInteger!

valueSize
	^valueSize!

valueSize: anInteger
	valueSize := anInteger! !
!RioParameter categoriesFor: #aboutToSaveImage!private!private helpers! !
!RioParameter categoriesFor: #cType!accessing!public! !
!RioParameter categoriesFor: #cType:!accessing!public! !
!RioParameter categoriesFor: #initialize!initializing!public! !
!RioParameter categoriesFor: #parameterType!accessing!public! !
!RioParameter categoriesFor: #parameterType:!accessing!public! !
!RioParameter categoriesFor: #precision!accessing!public! !
!RioParameter categoriesFor: #precision:!accessing!public! !
!RioParameter categoriesFor: #printOn:!ANSI protocols-Object!public! !
!RioParameter categoriesFor: #savedImage!private!private helpers! !
!RioParameter categoriesFor: #scale!accessing!public! !
!RioParameter categoriesFor: #scale:!accessing!public! !
!RioParameter categoriesFor: #sqlType!accessing!public! !
!RioParameter categoriesFor: #sqlType:!accessing!public! !
!RioParameter categoriesFor: #started!private!private helpers! !
!RioParameter categoriesFor: #value!accessing!public! !
!RioParameter categoriesFor: #value:!accessing!public! !
!RioParameter categoriesFor: #valueBytesAvailable!accessing!public! !
!RioParameter categoriesFor: #valueBytesAvailable:!accessing!public! !
!RioParameter categoriesFor: #valueSize!accessing!public! !
!RioParameter categoriesFor: #valueSize:!accessing!public! !

!RioParameter class methodsFor!

aboutToSaveImage
	self withAllSubclassesDo: [ :aClass | aClass allInstances do: [ :aParameter | aParameter aboutToSaveImage ] ]!

finalize
	SessionManager current removeEventsTriggeredFor: self.
	super finalize!

install
	SessionManager current
		when: #imageSaveStarting send: #aboutToSaveImage to: self;
		when: #imageSaveCompleted send: #savedImage to: self;
		when: #sessionStarted send: #started to: self.

	self beFinalizable!

new
	^super new initialize!

savedImage
	self withAllSubclassesDo: [ :aClass | aClass allInstances do: [ :aParameter | aParameter savedImage ] ]!

started
	self withAllSubclassesDo: [ :aClass | aClass allInstances do: [ :aParameter | aParameter started ] ]!

uninstall
	self finalize! !
!RioParameter class categoriesFor: #aboutToSaveImage!private!private helpers! !
!RioParameter class categoriesFor: #finalize!finalizing!public! !
!RioParameter class categoriesFor: #install!installing!public! !
!RioParameter class categoriesFor: #new!instance creation!public! !
!RioParameter class categoriesFor: #savedImage!private!private helpers! !
!RioParameter class categoriesFor: #started!private!private helpers! !
!RioParameter class categoriesFor: #uninstall!installing!public! !

RioResult guid: (GUID fromString: '{6BBB8F96-559A-11D3-8269-00001D19F5C2}')!
RioResult comment: ''!
!RioResult categoriesForClass!Rio! !
!RioResult methodsFor!

asString
	^self printString!

errorMsg
	^errorMsg!

errorMsg: errString nativeError: anExternalLong sqlState: stateString
	"Private : value initialization method"
	errorMsg := errString trimNulls deepCopy.
	nativeError := anExternalLong asInteger.
	sqlState := stateString trimNulls deepCopy!

initialize
	super initialize.
	errorMsg := ''.
	nativeError := 0.
	sqlState := '00000'!

nativeError
	^nativeError!

printOn: aStream
	aStream
		nextPutAll: self sqlState;
		nextPut: $:;
		nextPutAll: self errorMsg!

sqlState
	^sqlState! !
!RioResult categoriesFor: #asString!converting!public! !
!RioResult categoriesFor: #errorMsg!accessing!public! !
!RioResult categoriesFor: #errorMsg:nativeError:sqlState:!private!private helpers! !
!RioResult categoriesFor: #initialize!initializing!public! !
!RioResult categoriesFor: #nativeError!accessing!public! !
!RioResult categoriesFor: #printOn:!printing!public! !
!RioResult categoriesFor: #sqlState!accessing!public! !

!RioResult class methodsFor!

allResults
	| results |

	results := String new.
	self do: [ :each | results := results, (String tab), each asString, (String lineDelimiter) ].
	^results!

at: anInteger
	^ResultCollection at: anInteger!

createFromEnv: henvExternalHandle hdbc: hdbcExternalHandle hstmt: hstmtExternalHandle
	"Private - answers an OrderedCollection of RioResults that apply
	 to the environment, database, and statement handles passed"

	| collection retcode state native msg msgAvail result |

	collection := OrderedCollection new.
	state := String new: 6.
	native := RioSdwordBuffer new value: 0.
	msg := String new: 1024.
	msgAvail := RioSwordBuffer new value: 0.

	retcode := RioDatabase dll
				sqlError: henvExternalHandle asParameter
				hdbc: hdbcExternalHandle asParameter
				hstmt: hstmtExternalHandle asParameter
				sqlState: state asParameter
				nativeError: native asParameter
				errorMsg: msg asParameter
				errorMsgSize: msg size asParameter
				errorMsgBytesAvailable: msgAvail asParameter.
	(retcode = RioConstants current sqlError) | (retcode = RioConstants current sqlInvalidHandle)
		ifTrue: [RioFatalError signal: 'RioResult class>>createFromEnv:hdbc:hstmt'].
	[retcode = RioConstants current sqlSuccess] whileTrue: [
		result := RioResult new errorMsg: msg nativeError: native sqlState: state.
		collection add: result.
		retcode := RioDatabase dll
					sqlError: henvExternalHandle asParameter
					hdbc: hdbcExternalHandle asParameter
					hstmt: hstmtExternalHandle asParameter
					sqlState: state asParameter
					nativeError: native asParameter
					errorMsg: msg asParameter
					errorMsgSize: msg size asParameter
					errorMsgBytesAvailable: msgAvail asParameter.
		(retcode = RioConstants current sqlError) | (retcode = RioConstants current sqlInvalidHandle)
			ifTrue: [RioFatalError signal: 'RioResult class>>createFromEnv:hdbc:hstmt']].
	^collection!

do: aBlock
	"Iterate resultCollection over aBlock"
	^ResultCollection do: aBlock!

fromEnv: aRioHandleBuffer
		"Creates an OrderedCollection of RioResults that apply to the
		 environment  passed, assigning the collection
		 to the class variable resultCollection.  Individual errors can
		 be retrieved using the class message at:."
	ResultCollection := self
					createFromEnv: aRioHandleBuffer asParameter
					hdbc: (RioHandleBuffer new value: RioConstants current sqlNullHdbc) asParameter
					hstmt: (RioHandleBuffer new value: RioConstants current sqlNullHstmt) asParameter!

fromEnv: henvHandleBuffer hdbc: hdbcHandleBuffer
		"Creates an OrderedCollection of RioResults that apply to the
		 environment and database passed, assigning the collection
		 to the class variable resultCollection.  Individual errors can
		 be retrieved using the class message at:."
	ResultCollection := (self
					createFromEnv: henvHandleBuffer asParameter
					hdbc: hdbcHandleBuffer asParameter
					hstmt: (RioHandleBuffer new value: RioConstants current sqlNullHstmt) asParameter),
				    (self
					createFromEnv: henvHandleBuffer asParameter
					hdbc: (RioHandleBuffer new value: RioConstants current sqlNullHdbc) asParameter
					hstmt: (RioHandleBuffer new value: RioConstants current sqlNullHstmt) asParameter)!

fromEnv: henvHandleBuffer hdbc: hdbcHandleBuffer hstmt: hstmtHandleBuffer
		"Creates an OrderedCollection of RioResults that apply to the
		 environment, database, and statement handles passed, assigning
		 the collection to the class variable resultCollection.  Individual
		 errors can be retrieved using the class message at:."
	ResultCollection := (self
					createFromEnv: henvHandleBuffer asParameter
					hdbc: hdbcHandleBuffer asParameter
					hstmt: hstmtHandleBuffer asParameter) ,
				   (self
					createFromEnv: henvHandleBuffer asParameter
					hdbc: hdbcHandleBuffer asParameter
					hstmt: (RioHandleBuffer new value: RioConstants current sqlNullHstmt) asParameter),
				   (self
					createFromEnv: henvHandleBuffer
					hdbc: (RioHandleBuffer new value: RioConstants current sqlNullHdbc) asParameter
					hstmt: (RioHandleBuffer new value: RioConstants current sqlNullHstmt) asParameter)!

isRestartable
	"Answer whether resultCollection contains errors indicating that the
	 current transaction can be rolled back and retried.  The only
	 restartable SQLSTATEs are S1T00 (timeout, or lock conflict) and
	 40001 (serialization failure, or deadlock).  This method always
	 answers true or false."
	self size = 0
		ifTrue: [ ^true ].
	ResultCollection do: [ :err |
		(err sqlState = 'S1T00') | (err sqlState = '40001')
			ifTrue: [ ^true ]].
	^false!

reset
	"Resets the class collection of errors, resultCollection, to an empty
	 OrderedCollection"
	ResultCollection isNil
		ifTrue: [ResultCollection := OrderedCollection new]
		ifFalse: [ResultCollection copy do: [ :each | ResultCollection remove: each ] ]!

resultCollection
	^ResultCollection!

size
	^ResultCollection size! !
!RioResult class categoriesFor: #allResults!converting!public! !
!RioResult class categoriesFor: #at:!accessing!public! !
!RioResult class categoriesFor: #createFromEnv:hdbc:hstmt:!private!private helpers! !
!RioResult class categoriesFor: #do:!enumerating!public! !
!RioResult class categoriesFor: #fromEnv:!accessing!public! !
!RioResult class categoriesFor: #fromEnv:hdbc:!accessing!public! !
!RioResult class categoriesFor: #fromEnv:hdbc:hstmt:!accessing!public! !
!RioResult class categoriesFor: #isRestartable!public!testing! !
!RioResult class categoriesFor: #reset!operations!public! !
!RioResult class categoriesFor: #resultCollection!accessing!public! !
!RioResult class categoriesFor: #size!accessing!public! !

RioRowset guid: (GUID fromString: '{6BBB8F97-559A-11D3-8269-00001D19F5C2}')!
RioRowset comment: ''!
!RioRowset categoriesForClass!Rio! !
!RioRowset methodsFor!

asCollection
	"Answer a collection of RioRow's from this rowset"

	| col rowStatus |

	col := OrderedCollection new.
	1 to: self nRows do: [ :i |
		rowStatus := self rowStatus: i.
		(rowStatus = RioConstants current sqlRowSuccess) |
				(rowStatus = RioConstants current sqlRowUpdated) |
				(rowStatus = RioConstants current sqlRowAdded)
			ifTrue: [ col add: (self rowAt: i) ] ].

	^col!

at: anInteger
	"Answers the row with index anInteger in this rowset."

	self assert: [ (anInteger > 0) & (anInteger <= self nRows) ].
	^self rowAt: anInteger!

at: anInteger column: anObject
	"Answer the value of the column with name or index anObject in
	 row anInteger."

	^(self at: anInteger) at: anObject!

at: anInteger column: anObject put: valueObject
	"Set the value of the column with name or index anObject to valueObject
	 in the row with index anInteger."
	^(self at: anInteger) at: anObject put: valueObject!

clearDbDirty
	"Clear the DB dirty flag on all fields in this rowset"

	self rowsetBuffer setAllDbDirtyFlags: false
	"self do: [ :aRow | aRow clearDbDirty ]"!

clearUiDirty
	"Clear the UI dirty flag on all fields in this rowset"

	self rowsetBuffer setAllUiDirtyFlags: false
	"self do: [ :aRow | aRow clearUiDirty ]"!

containsColumnNamed: aColumnNameString
	^self rowsetBuffer containsColumnNamed: aColumnNameString!

createBuffer
	"Private - create a RioRowsetBuffer to hold the data for this rowset."

	self rowsetBuffer: RioRowsetBuffer new.
	self rowsetBuffer nRows: self nRows.!

do: aMonadicBlock
	"Perform aMonadicBlock on all valid rows in this rowset."

	| rowStatus |

	1 to: self nRows do:
		[ :i |
		rowStatus := self rowStatus: i.
		(((rowStatus = RioConstants current sqlRowSuccess) or:
				[ rowStatus = RioConstants current sqlRowUpdated ]) or:
				[rowStatus = RioConstants current sqlRowAdded ])
			ifTrue: [ aMonadicBlock value: (self at: i) ] ]!

initialize
	super initialize.
	self startRow: 1!

isDbDirty
	"Determine if any fields in this rowset has been changed.  Answer
	 true if any fields have been changed, otherwise answer false."

	startRow to: (startRow + nRows - 1) do: [ :i |
		(self at: i) isDbDirty
			ifTrue: [ ^true ] ].

	^false!

isUiDirty
	"Determine if any fields in this rowset has been changed.  Answer
	 true if any fields have been changed, otherwise answer false."

	startRow to: (startRow + nRows - 1) do: [ :i |
		(self at: i) isUiDirty
			ifTrue: [ ^true ] ].

	^false!

nRows
	^nRows!

nRows: anInteger
	nRows := anInteger!

printOn: aStream
	"Dump the contents of each row"

	super printOn: aStream.
	aStream cr.
	1 to: self nRows do: [ :i |
		aStream tab.
		(self at: i) printOn: aStream.
		aStream cr. ]!

rowAt: anInteger
	"Private - Answers the row with index anInteger in this rowset."

	^RioRow new
		rowsetBuffer: self rowsetBuffer;
		startRow: anInteger!

rowsetBuffer
	rowsetBuffer isNil ifTrue: [ self createBuffer ].
	^rowsetBuffer!

rowsetBuffer: aRowsetBuffer
	"Private - set the rowset buffer which stores data for this object"
	rowsetBuffer := aRowsetBuffer!

rowsetHeader
	^self rowsetBuffer rowsetHeader!

rowsetHeader: aRowsetHeader
	self rowsetBuffer rowsetHeader: aRowsetHeader!

rowStatus: anInteger
	"Answer the status of row 'anInteger' in this rowset."

	^self rowsetBuffer rowStatus: (self startRow + anInteger - 1)!

setDbDirty
	"Set the DB dirty flag on all fields in this rowset"

	self rowsetBuffer setAllDbDirtyFlags: true
	"self do: [ :aRow | aRow setDbDirty ]"!

setDirtyFlagsAfterFetch
	"Set all UI and DB dirty flags in this rowset to their appropriate state after
	 data has been fetched into the rowset, i.e. sets the UI dirty flag to 'true' and
	 the DB dirty flag to 'false'."

	self
		setUiDirty;
		clearDbDirty!

setUiDirty
	"Set the UI dirty flag on all fields in this rowset"

	self rowsetBuffer setAllUiDirtyFlags: true
	"self do: [ :aRow | aRow setUiDirty ]"!

startRow
	^startRow!

startRow: anInteger
	"Private - sets the row within rowsetBuffer where this subset begins."
	startRow := anInteger! !
!RioRowset categoriesFor: #asCollection!converting!public! !
!RioRowset categoriesFor: #at:!accessing!public! !
!RioRowset categoriesFor: #at:column:!accessing!public! !
!RioRowset categoriesFor: #at:column:put:!accessing!public! !
!RioRowset categoriesFor: #clearDbDirty!dirty flags!public! !
!RioRowset categoriesFor: #clearUiDirty!dirty flags!public! !
!RioRowset categoriesFor: #containsColumnNamed:!accessing!public! !
!RioRowset categoriesFor: #createBuffer!private!private helpers! !
!RioRowset categoriesFor: #do:!enumerating!public! !
!RioRowset categoriesFor: #initialize!initializing!public! !
!RioRowset categoriesFor: #isDbDirty!dirty flags!public! !
!RioRowset categoriesFor: #isUiDirty!dirty flags!public! !
!RioRowset categoriesFor: #nRows!accessing!public! !
!RioRowset categoriesFor: #nRows:!accessing!public! !
!RioRowset categoriesFor: #printOn:!ANSI protocols-Object!public! !
!RioRowset categoriesFor: #rowAt:!accessing!private! !
!RioRowset categoriesFor: #rowsetBuffer!accessing!public! !
!RioRowset categoriesFor: #rowsetBuffer:!accessing!private! !
!RioRowset categoriesFor: #rowsetHeader!accessing!public! !
!RioRowset categoriesFor: #rowsetHeader:!accessing!public! !
!RioRowset categoriesFor: #rowStatus:!accessing!public! !
!RioRowset categoriesFor: #setDbDirty!dirty flags!public! !
!RioRowset categoriesFor: #setDirtyFlagsAfterFetch!dirty flags!public! !
!RioRowset categoriesFor: #setUiDirty!dirty flags!public! !
!RioRowset categoriesFor: #startRow!accessing!public! !
!RioRowset categoriesFor: #startRow:!private!private helpers! !

!RioRowset class methodsFor!

new
	^super new initialize! !
!RioRowset class categoriesFor: #new!instance creation!public! !

RioRowsetBuffer guid: (GUID fromString: '{6BBB8F98-559A-11D3-8269-00001D19F5C2}')!
RioRowsetBuffer comment: ''!
!RioRowsetBuffer categoriesForClass!Rio! !
!RioRowsetBuffer methodsFor!

aboutToSaveImage
	"Private - copy the external (non-Smalltalk) data to an internal object which can be
	 stored with the image."

	| anInternalBuffer |

	data isNil not
		ifTrue: [
			anInternalBuffer := RioInternalBuffer new: self dataSize.
			anInternalBuffer copyExternalBuffer: data startingAtOffset: 0 toInternalOffset: 0 count: self dataSize.
			data := anInternalBuffer ]!

allocateData: anInteger
	"Private - allocate storage for nRows of data."

	self nRows: anInteger.

	self rowsetHeader isNil
		ifTrue: [ RioFatalError signal: 'RioRowsetBuffer>>allocate: rowsetHeader is nil' ].

	self releaseData.
	data := RioExternalBuffer new: self dataSize.

	"Create the row status array.  Each element in the status array is an
	 unsigned word (2 bytes)."

	rowStatusArray := RioExternalBuffer new: (self nRows * 2).

	"Set indicators for all columns in all rows to NULL"

	1 to: self nRows do: [ :rowIndex  |
		1 to: self nCols do: [ :colIndex |
			(self rowsetHeader at: colIndex)
				setIndicatorInBuffer: self
				row: rowIndex
				value: RioConstants current sqlNullData ] ].

	"Allocate the DB and UI dirty flags"

	dbDirtyFlags := Array new: self nRows * self nCols.
	uiDirtyFlags := Array new: self nRows * self nCols.

	"Clear all DB and UI dirty flags"

	self setAllDbDirtyFlags: false.
	self setAllUiDirtyFlags: false!

byteAtOffset: anInteger
	^data byteAtOffset: anInteger!

byteAtOffset: anInteger put: aValue
	data byteAtOffset: anInteger put: aValue!

clearDbDirtyFlagForRow: anIntegerRow column: anIntegerColumn
	dbDirtyFlags at: (anIntegerRow - 1 * self nCols) + anIntegerColumn put: false!

clearUiDirtyFlagForRow: anIntegerRow column: anIntegerColumn
	uiDirtyFlags at: (anIntegerRow - 1 * self nCols) + anIntegerColumn put: false!

containsColumnNamed: aColumnNameString
	^self rowsetHeader containsColumnNamed: aColumnNameString!

data
	data isNil ifTrue: [ self allocateData: self nRows ].
	^data!

dataSize
	"Answer the number of bytes which must be allocated by this object"

	^(self rowsetHeader bufferSize * self nRows)!

dbDirtyFlagForRow: anIntegerRow column: anIntegerColumn
	^dbDirtyFlags at: (anIntegerRow - 1 * self nCols) + anIntegerColumn!

doubleAtOffset: anInteger
	^self data doubleAtOffset: anInteger!

doubleAtOffset: anInteger put: aDouble
	self data doubleAtOffset: anInteger put: aDouble!

dwordAtOffset: anInteger
	^self data dwordAtOffset: anInteger!

dwordAtOffset: anInteger put: anObject
	self data dwordAtOffset: anInteger put: anObject!

nCols
	^self rowsetHeader nCols!

nRows
	^nRows!

nRows: anInteger
	nRows := anInteger!

releaseData
	"Private - release the data buffer"

	data notNil ifTrue:
		[ data free.
		data := nil ]!

rowOffset: anInteger
	^self rowsetHeader rowOffset: anInteger!

rowsetHeader
	^rowsetHeader!

rowsetHeader: aRowsetHeader
	rowsetHeader := aRowsetHeader!

rowStatus: anInteger
	"Answer the status variable for row 'anInteger' in this buffer."

	| anExternalBuffer |

	anInteger <= nRows
		ifTrue: [ ^self rowStatusArray wordAtOffset: ((anInteger - 1) * 2) ]
		ifFalse: [ RioFatalError signal: 'RioRowsetBuffer>>rowStatus: - index ', anInteger printString, ' out of bounds' ]!

rowStatusArray
	^rowStatusArray!

savedImage
	"Private - copy the internal data to an external (non-Smalltalk) data area"

	| anExternalBuffer |

	data notNil
		ifTrue:
			[ anExternalBuffer := RioExternalBuffer new: self dataSize.
			anExternalBuffer
				copyInternalBuffer: self data
				startingAtOffset: 0
				toOffset: 0
				count: self dataSize.
			data := anExternalBuffer ]!

sdwordAtOffset: anInteger
	^self data sdwordAtOffset: anInteger!

sdwordAtOffset: anInteger put: anObject
	self data sdwordAtOffset: anInteger put: anObject!

setAllDbDirtyFlags: aBoolean
	dbDirtyFlags atAllPut: aBoolean!

setAllUiDirtyFlags: aBoolean
	uiDirtyFlags atAllPut: aBoolean!

setDbDirtyFlagForRow: anIntegerRow column: anIntegerColumn
	dbDirtyFlags at: (anIntegerRow - 1 * self nCols) + anIntegerColumn put: true!

setUiDirtyFlagForRow: anIntegerRow column: anIntegerColumn
	uiDirtyFlags at: (anIntegerRow - 1 * self nCols) + anIntegerColumn put: true!

started
	"Private"
	^self savedImage!

stringAtOffset: anIntegerOffset count: anIntegerCount
	^self data stringAtOffset: anIntegerOffset count: anIntegerCount!

stringAtOffset: anIntegerOffset count: anIntegerCount put: aString
	^self data stringAtOffset: anIntegerOffset count: anIntegerCount put: aString!

swordAtOffset: anInteger
	^self data swordAtOffset: anInteger!

swordAtOffset: anInteger put: anObject
	self data swordAtOffset: anInteger put: anObject!

uiDirtyFlagForRow: anIntegerRow column: anIntegerColumn
	^uiDirtyFlags at: (anIntegerRow - 1 * self nCols) + anIntegerColumn!

wordAtOffset: anInteger
	^self data wordAtOffset: anInteger!

wordAtOffset: anInteger put: anObject
	self data wordAtOffset: anInteger put: anObject! !
!RioRowsetBuffer categoriesFor: #aboutToSaveImage!private!private helpers! !
!RioRowsetBuffer categoriesFor: #allocateData:!private!private helpers! !
!RioRowsetBuffer categoriesFor: #byteAtOffset:!accessing!public! !
!RioRowsetBuffer categoriesFor: #byteAtOffset:put:!accessing!public! !
!RioRowsetBuffer categoriesFor: #clearDbDirtyFlagForRow:column:!operations!public! !
!RioRowsetBuffer categoriesFor: #clearUiDirtyFlagForRow:column:!operations!public! !
!RioRowsetBuffer categoriesFor: #containsColumnNamed:!accessing!public! !
!RioRowsetBuffer categoriesFor: #data!accessing!public! !
!RioRowsetBuffer categoriesFor: #dataSize!accessing!public! !
!RioRowsetBuffer categoriesFor: #dbDirtyFlagForRow:column:!accessing!public! !
!RioRowsetBuffer categoriesFor: #doubleAtOffset:!accessing!public! !
!RioRowsetBuffer categoriesFor: #doubleAtOffset:put:!accessing!public! !
!RioRowsetBuffer categoriesFor: #dwordAtOffset:!accessing!public! !
!RioRowsetBuffer categoriesFor: #dwordAtOffset:put:!accessing!public! !
!RioRowsetBuffer categoriesFor: #nCols!accessing!public! !
!RioRowsetBuffer categoriesFor: #nRows!accessing!public! !
!RioRowsetBuffer categoriesFor: #nRows:!accessing!public! !
!RioRowsetBuffer categoriesFor: #releaseData!private!private helpers! !
!RioRowsetBuffer categoriesFor: #rowOffset:!accessing!public! !
!RioRowsetBuffer categoriesFor: #rowsetHeader!accessing!public! !
!RioRowsetBuffer categoriesFor: #rowsetHeader:!accessing!public! !
!RioRowsetBuffer categoriesFor: #rowStatus:!accessing!public! !
!RioRowsetBuffer categoriesFor: #rowStatusArray!accessing!public! !
!RioRowsetBuffer categoriesFor: #savedImage!private!private helpers! !
!RioRowsetBuffer categoriesFor: #sdwordAtOffset:!accessing!public! !
!RioRowsetBuffer categoriesFor: #sdwordAtOffset:put:!accessing!public! !
!RioRowsetBuffer categoriesFor: #setAllDbDirtyFlags:!operations!public! !
!RioRowsetBuffer categoriesFor: #setAllUiDirtyFlags:!operations!public! !
!RioRowsetBuffer categoriesFor: #setDbDirtyFlagForRow:column:!operations!public! !
!RioRowsetBuffer categoriesFor: #setUiDirtyFlagForRow:column:!operations!public! !
!RioRowsetBuffer categoriesFor: #started!private!private helpers! !
!RioRowsetBuffer categoriesFor: #stringAtOffset:count:!accessing!public! !
!RioRowsetBuffer categoriesFor: #stringAtOffset:count:put:!accessing!public! !
!RioRowsetBuffer categoriesFor: #swordAtOffset:!accessing!public! !
!RioRowsetBuffer categoriesFor: #swordAtOffset:put:!accessing!public! !
!RioRowsetBuffer categoriesFor: #uiDirtyFlagForRow:column:!accessing!public! !
!RioRowsetBuffer categoriesFor: #wordAtOffset:!accessing!public! !
!RioRowsetBuffer categoriesFor: #wordAtOffset:put:!accessing!public! !

!RioRowsetBuffer class methodsFor!

aboutToSaveImage
	"Private"
	self withAllSubclassesDo: [ :aClass | aClass allInstances do: [ :aBuffer | aBuffer aboutToSaveImage ] ].!

finalize
	SessionManager current
		removeEventsTriggeredFor: self.
	super finalize!

install
	"Register for events of interest when the library containing this class is
	 bound to the image."

	SessionManager current
		when: #imageSaveStarting send: #aboutToSaveImage to: self;
		when: #imageSaveCompleted send: #savedImage to: self;
		when: #sessionStarted send: #started to: self.

	self beFinalizable!

savedImage
	"Private"
	self withAllSubclassesDo: [ :aClass |
					aClass allInstances do: [ :aBuffer | aBuffer savedImage ] ]!

started
	"Private"
	self withAllSubclassesDo: [ :aClass |
					aClass allInstances do: [ :aBuffer | aBuffer started ] ].!

uninstall
	"Private"
	self finalize! !
!RioRowsetBuffer class categoriesFor: #aboutToSaveImage!private!private helpers! !
!RioRowsetBuffer class categoriesFor: #finalize!finalizing!public! !
!RioRowsetBuffer class categoriesFor: #install!installing!public! !
!RioRowsetBuffer class categoriesFor: #savedImage!private!private helpers! !
!RioRowsetBuffer class categoriesFor: #started!private!private helpers! !
!RioRowsetBuffer class categoriesFor: #uninstall!private!private helpers! !

RioRowsetHeader guid: (GUID fromString: '{6BBB8F99-559A-11D3-8269-00001D19F5C2}')!
RioRowsetHeader comment: ''!
!RioRowsetHeader categoriesForClass!Rio! !
!RioRowsetHeader methodsFor!

addHeader: aHeader at: anInteger
	"Adds the aHeader to the columnHeaders collection at the index specified
	 by anInteger.  Also adds aHeader to columnHeadersByName, indexed
	 by the name of the column."

	self columnHeaders at: anInteger put: aHeader.
	self columnHeadersByName at: aHeader name put: aHeader.!

at: anIntegerOrString
	"If anIntegerOrString is an Integer, answers the header at that index
	 in columnHeaders.  If anIntegerOrString is a String, answers the header
	 with that name in columnHeadersByName."

	^anIntegerOrString columnHeaderFrom: self!

basicBufferSize
	"Private - Answer the size of the buffer required to store a row based
	 on this header.  The addition of 4 extra bytes on the end is for
	 the indicator on the last column."

	| maxColumnHeader |

	maxColumnHeader := columnHeaders at: 1.

	self columnHeaders do:
		[ :aColumnHeader |
		aColumnHeader offset > maxColumnHeader offset
			ifTrue: [ maxColumnHeader := aColumnHeader ] ].

	maxColumnHeader notNil
		ifTrue: [ ^maxColumnHeader offset + maxColumnHeader dataSize + 4 ]
		ifFalse: [ ^0 ]!

bufferSize
	bufferSize isNil ifTrue: [ bufferSize := self basicBufferSize ].
	^bufferSize!

calculateOffsets
	"Calculate the offset of each column in rowsets based on this header."

	| nextOffset |

	nextOffset := 0.

	self columnHeaders do: [ :aColumnHeader |
		aColumnHeader offset: nextOffset.

		"4 = size of indicator"

		nextOffset := nextOffset + aColumnHeader dataSize + 4 ]!

columnHeaders
	"Answer the value of the receiver's instance variable columnHeaders.
	This method was automatically generated, but may be modified."

	^columnHeaders!

columnHeaders: anObject
	"Set the value of the receiver's instance variable columnHeaders to anObject.
	This method was automatically generated, but may be modified."

	columnHeaders := anObject!

columnHeadersByName
	"Answer the value of the receiver's instance variable columnHeadersByName.
	This method was automatically generated, but may be modified."

	^columnHeadersByName!

columnHeadersByName: anObject
	"Set the value of the receiver's instance variable columnHeadersByName to anObject.
	This method was automatically generated, but may be modified."

	columnHeadersByName := anObject!

containsColumnNamed: aColumnNameString
	^(self columnHeadersByName at: aColumnNameString ifAbsent: [ nil ]) ~= nil!

initialize
	super initialize.
	self columnHeaders: Dictionary new.
	self columnHeadersByName: Dictionary new.!

nCols
	"Answer the number of columns referenced by this rowset header."
	^self columnHeaders size!

printOn: aWriteStream
	super printOn: aWriteStream.
	aWriteStream cr.
	self columnHeaders do: [ :aColumnHeader |
		aWriteStream tab.
		aColumnHeader printOn: aWriteStream.
		aWriteStream cr ]!

rowOffset: anInteger
	^self bufferSize * (anInteger - 1)! !
!RioRowsetHeader categoriesFor: #addHeader:at:!accessing!public! !
!RioRowsetHeader categoriesFor: #at:!accessing!public! !
!RioRowsetHeader categoriesFor: #basicBufferSize!accessing!private! !
!RioRowsetHeader categoriesFor: #bufferSize!accessing!public! !
!RioRowsetHeader categoriesFor: #calculateOffsets!accessing!public! !
!RioRowsetHeader categoriesFor: #columnHeaders!accessing!public! !
!RioRowsetHeader categoriesFor: #columnHeaders:!accessing!public! !
!RioRowsetHeader categoriesFor: #columnHeadersByName!accessing!public! !
!RioRowsetHeader categoriesFor: #columnHeadersByName:!accessing!public! !
!RioRowsetHeader categoriesFor: #containsColumnNamed:!accessing!public! !
!RioRowsetHeader categoriesFor: #initialize!initializing!public! !
!RioRowsetHeader categoriesFor: #nCols!accessing!public! !
!RioRowsetHeader categoriesFor: #printOn:!ANSI protocols-Object!public! !
!RioRowsetHeader categoriesFor: #rowOffset:!accessing!public! !

!RioRowsetHeader class methodsFor!

new
	^super new initialize! !
!RioRowsetHeader class categoriesFor: #new!instance creation!public! !

RioSqlGenerator guid: (GUID fromString: '{A2F48634-FA46-4435-AEF0-5CB7D08CAF58}')!
RioSqlGenerator comment: 'I generate SQL statements.  I might need to be subclassed if a particular database wanted its SQL formatted in some non-standard manner.'!
!RioSqlGenerator categoriesForClass!Unclassified! !
!RioSqlGenerator methodsFor!

columnNames
	^columnNames!

columnNames: aCollection
	columnNames := aCollection!

generateDeleteStatement
	| s |

	(s := String new writeStream)
		nextPutAll: 'DELETE FROM ';
		nextPutAll: self tableName.

	self whereClause notNil ifTrue:
		[ s
			nextPutAll: ' WHERE ';
			nextPutAll: self whereClause ].

	^s contents!

generateInsertStatement
	| s |

	(s := String new writeStream)
		nextPutAll: 'INSERT INTO ';
		nextPutAll: self tableName;
		nextPutAll: ' ('.

	self columnNames do:
		[ :aColumnName |
		s
			nextPutAll: aColumnName;
			nextPut: $, ].
	s pop.

	s nextPutAll: ') VALUES ('.

	self columnNames do:
		[ :aColumnName |
		s nextPutAll: '?,' ].
	s pop.

	s nextPut: $).

	^s contents!

generateSelectStatement
	| s |

	(s := String new writeStream)
		nextPutAll: 'SELECT '.

	self columnNames notNil
		ifTrue: [ self columnNames do:
				[ :aColumnName |
				s
					nextPutAll: aColumnName;
					nextPut: $, ].
			  s pop ]
		ifFalse: [ s nextPut: $* ].

	s
		nextPutAll: ' FROM ';
		nextPutAll: self tableName.

	self whereClause notNil ifTrue:
		[ s
			nextPutAll: ' WHERE ';
			nextPutAll: self whereClause ].

	self orderByColumnNames notNil ifTrue:
		[ s nextPutAll: ' ORDER BY '.
		self orderByColumnNames do:
			[ :aColumnName |
			s
				nextPutAll: aColumnName;
				nextPut: $, ].
		s pop ].

	^s contents!

generateUpdateStatement
	| s |

	(s := String new writeStream)
		nextPutAll: 'UPDATE ';
		nextPutAll: self tableName;
		nextPutAll: ' SET '.

	self columnNames do:
		[ :aColumnName |
		s
			nextPutAll: aColumnName;
			nextPutAll: '=?,' ].
	s pop.

	self whereClause notNil ifTrue:
		[ s
			nextPutAll: ' WHERE ';
			nextPutAll: self whereClause ].

	^s contents!

orderByColumnNames
	^orderByColumnNames!

orderByColumnNames: aCollection
	orderByColumnNames := aCollection!

tableName
	^tableName!

tableName: aString
	tableName := aString!

whereClause
	^whereClause!

whereClause: aString
	whereClause := aString! !
!RioSqlGenerator categoriesFor: #columnNames!accessing!public! !
!RioSqlGenerator categoriesFor: #columnNames:!accessing!public! !
!RioSqlGenerator categoriesFor: #generateDeleteStatement!operations!public! !
!RioSqlGenerator categoriesFor: #generateInsertStatement!operations!public! !
!RioSqlGenerator categoriesFor: #generateSelectStatement!operations!public! !
!RioSqlGenerator categoriesFor: #generateUpdateStatement!operations!public! !
!RioSqlGenerator categoriesFor: #orderByColumnNames!accessing!public! !
!RioSqlGenerator categoriesFor: #orderByColumnNames:!accessing!public! !
!RioSqlGenerator categoriesFor: #tableName!accessing!public! !
!RioSqlGenerator categoriesFor: #tableName:!accessing!public! !
!RioSqlGenerator categoriesFor: #whereClause!accessing!public! !
!RioSqlGenerator categoriesFor: #whereClause:!accessing!public! !

RioStatement guid: (GUID fromString: '{6BBB8F9A-559A-11D3-8269-00001D19F5C2}')!
RioStatement comment: ''!
!RioStatement categoriesForClass!Rio! !
!RioStatement methodsFor!

afterCommit
	"Private - perform appropriate processing after a transaction on
	 the database this cursor is associated with has been committed."

	| commitBehavior |

	commitBehavior := database integerInfo: RioConstants current sqlCursorCommitBehavior.
	commitBehavior = RioConstants current sqlCbDelete
		ifTrue: [
			self prepared: false.
			self executed: false].
	commitBehavior = RioConstants current sqlCbClose
		ifTrue: [ self executed: false ].!

afterHandleAllocated
	"Private - perform any processing required after the statement handle
	 is allocated.  Default is to do nothing."!

afterRollback
	"Private - perform appropriate processing after a transaction has been
	rolled back on the database with which this statement is associated."

	| rollbackBehavior |

	rollbackBehavior := database integerInfo: RioConstants current sqlCursorRollbackBehavior.
	rollbackBehavior = RioConstants current sqlCbDelete
		ifTrue: [
			self prepared: false.
			self executed: false].
	rollbackBehavior = RioConstants current sqlCbClose
		ifTrue: [ self executed: false ].!

allocateStatementHandle
	"Private - allocate an ODBC statement handle, assigning it to hstmt"

	self check: (RioDatabase dll
				sqlAllocStmt: self database hdbc asParameter
				hstmt: self rawHstmt asParameter).
	self afterHandleAllocated.
	^self retcode!

basicDrop
	"Private - release the statement handle without processing errors"

	| save |

	save := self finalizing.
	self finalizing: true.
	self drop.
	self finalizing: save.
	self rawHstmt value: RioConstants current sqlNullHstmt.
	^self retcode!

blobSize
	"Answer the default BLOB allocation size for this statement"
	blobSize isNil ifTrue: [ self blobSize: database blobSize ].
	^blobSize!

blobSize: anInteger
	"Set the BLOB allocation size for this statement"
	blobSize := anInteger!

cancel
	"Cancel any outstanding processing on this statement"

	^self check: (RioDatabase dll sqlCancel: self hstmt)!

check: anInteger
	self retcode: anInteger.
	^self processRetcode!

database
	^database!

database: aRioDatabase
	"Private - set the database this instance is associated with."
	database := aRioDatabase.
	database addStatement: self!

drop
	"Release the statement handle."
	self rawHstmt value = RioConstants current sqlNullHstmt
		ifTrue: [ self retcode: RioConstants current sqlSuccess]
		ifFalse: [
			self check: (RioDatabase dll
						sqlFreeStmt: self rawHstmt asParameter
						option: RioConstants current sqlDrop asParameter).
			self rawHstmt value: RioConstants current sqlNullHstmt.
			self prepared: false.
			self executed: false.
			self initializeParameters ].
	^self retcode!

dumpParameters: aStream
	"Dump the parameters associated with this statement"

	self parameters do: [ :parm |
		parm printOn: aStream ].
!

execute
	"Execute the statement"

	self prepare.

	self executed
		ifTrue: [ self retcode: RioConstants current sqlSuccess]
		ifFalse: [
			self check: (RioDatabase dll sqlExecute: self hstmt asParameter).
			(self retcode = RioConstants current sqlSuccess) |
					(self retcode = RioConstants current sqlSuccessWithInfo)
				ifTrue: [ self executed: true ]].
	^self retcode!

executed
	^executed!

executed: aBoolean
	"Private - set the 'executed' flag"
	executed := aBoolean!

finalize
	"Perform any actions necessary before this object is garbage collected"
	self finalizing: true.
	self release!

finalizing
	"Answer the value of the receiver's ''finalizing'' instance variable."

	^finalizing!

finalizing: anObject
	"Private - Set the value of the receiver's ''finalizing'' instance variable to the argument, anObject."

	finalizing := anObject!

getOption: anOption param: param
	"Private - answer the value of the statement option requested.
	User code should use either #integerOption: or #stringOption:."

	self check: (RioDatabase dll
				sqlGetStmtOption: self hstmt asParameter
				option: anOption asParameter
				param: param asParameter).
	^param!

hstmt
	"Answer the statement handle for this statement, allocating
	 one if necessary"
	self rawHstmt value = RioConstants current sqlNullHstmt
		ifTrue: [self allocateStatementHandle].
	^hstmt!

initialize
	super initialize.

	blobSize := nil.
	database := nil.
	hstmt := RioHandleBuffer new value: RioConstants current sqlNullHstmt.
	retcode := nil.
	sql := nil.
	self finalizing: false.
	self prepared: false.
	self executed: false.
	self initializeParameters.
	self beFinalizable.!

initializeParameters
	"Private - create a dictionary to hold parameters"

	parameters := Dictionary new!

integerOption: anOption
	"Answer the value of the integer statement option requested."
	| param |

	param := RioSdwordBuffer new.
	^(self getOption: anOption param: param) asInteger!

integerOption: anOption value: param
	"Set the given statement option to the value specified in 'param'"

	^self
		setOption: anOption
		value: param asInteger
!

nativeSql
	"Answer the database's translation of the SQL for this statement"
	^database nativeSql: sql!

paramCount
	"Answer the number of parameters in the SQL associated with
	 this statement."

	| count |

	self prepare.

	count := RioSdwordBuffer new value: 0.

	self check: (RioDatabase dll
				sqlNumParams: self hstmt asParameter
				parameterCount: count asParameter).
	^count asInteger!

parameters
	^parameters!

prepare
	"Prepare this statement for execution."

	self prepared
		ifTrue: [ self retcode: RioConstants current sqlSuccess ]
		ifFalse: [
			self check: (RioDatabase dll
						sqlPrepare: self hstmt asParameter
						sqlString: self sql asParameter
						sqlStringLen: self sql size asParameter).
			(self retcode = RioConstants current sqlSuccess) |
					(self retcode = RioConstants current sqlSuccessWithInfo)
				ifTrue: [ self prepared: true ] ].
	^self retcode!

prepared
	^prepared!

prepared: aBoolean
	"Private - answer the 'prepared' flag"
	prepared := aBoolean!

printOn: aWriteStream
	super printOn: aWriteStream.
	aWriteStream 
		nextPutAll: ' (';
		nextPutAll: sql;
		nextPutAll: ')'.
	self dumpParameters: aWriteStream!

processRetcode
	"Private - process the return code from an ODBC function call"

	| results |

	RioResult reset.
	(self retcode = RioConstants current sqlError) | (self retcode = RioConstants current sqlSuccessWithInfo)
		ifTrue: [ RioResult
				fromEnv: database henv
				hdbc: database hdbc
				hstmt: self rawHstmt].

	"Signal an appropriate error if problems were encountered unless
	 we're in the midst of finalizing this object, in which case we just ignore
	 the error."

	((self retcode = RioConstants current sqlError) |
			(self retcode = RioConstants current sqlInvalidHandle)) &
			(self finalizing not)
		ifTrue: [  results := RioResult allResults.
				RioResult isRestartable
					ifTrue: [RioRestartableError signal: 'RioStatement>>processRetcode - ',
											'a restartable ODBC error occurred']
					ifFalse: [RioFatalError signal: 'RioStatement>>processRetcode - ',
											'a fatal ODBC error occurred', RioResult allResults ]].

	^self retcode!

rawHstmt
	"Private - answer the handle for this statement.  Do not create
	 a new handle if one has not already been allocated."
	^hstmt!

release
	"Free any resources held by this object"
	self basicDrop.
	database removeStatement: self.
	database := nil.
	parameters := nil.
	super release!

resetParameters
	"Release all parameter buffers set for this statement.."

	self rawHstmt value = RioConstants current sqlNullHstmt
		ifTrue: [ self retcode: RioConstants current sqlSuccess]
		ifFalse:
			[ self check: (RioDatabase dll
						sqlFreeStmt: self rawHstmt asParameter
						option: RioConstants current sqlResetParams asParameter).
			self initializeParameters ].

	^self retcode!

retcode
	^retcode!

retcode: anObject
	retcode := anObject!

rowCount
	"Answer the number of rows affected by the latest action performed
	 by this statement."

	| count |

	count := RioSdwordBuffer new value: 0.

	self check: (RioDatabase dll
				sqlRowCount: self hstmt asParameter
				rowsAffected: count asParameter).
	^count value!

setOption: anOption value: param
	"set the given statement option to the value specified
	 in 'param', which should be either an RioSdwordBuffer or an
	RioStringBuffer"

	^self check: (RioDatabase dll
				sqlSetStmtOption: self hstmt asParameter
				option: anOption asParameter
				param: param asParameter)!

setParameter: parmIndex cType: cType parameterType: parameterType sqlType: sqlType precision: precision scale: scale value: value valueSize: valueSize valueBytesAvailable: valueBytesAvailable
	"Private - invoke SQLBindParameter to bind the parameter value to
			a parameter marker in the current statement."

	^self check: (RioDatabase dll
				sqlBindParameter: self hstmt asParameter
				parameterNumber: parmIndex asParameter
				parameterType:  parameterType asParameter
				cType: cType asParameter
				sqlType: sqlType asParameter
				precision: precision asParameter
				scale: scale asParameter
				value: value asParameter
				valueSize: valueSize asParameter
				valueBytesAvailable: valueBytesAvailable asParameter)!

setParameter: anInteger value: anObject
	"Set the parameter value to be used with the given parameter index."

	self setParameter: anInteger with: (RioStringParameter new value: anObject)!

setParameter: anInteger value: anObject sqlType: sqlType
	"Set the parameter value to be used with the given parameter index."
	| aRioStringParameter |

	aRioStringParameter := RioStringParameter new.
	aRioStringParameter
		sqlType: sqlType;
		value: anObject.

	self
		setParameter: anInteger
		with: aRioStringParameter!

setParameter: parmIndex with: aRioParameter
	self setParameter: parmIndex
		cType: aRioParameter cType
		parameterType: aRioParameter parameterType
		sqlType: aRioParameter sqlType
		precision: aRioParameter precision
		scale: aRioParameter scale
		value: aRioParameter value
		valueSize: aRioParameter valueSize
		valueBytesAvailable: aRioParameter valueBytesAvailable.

	self parameters removeKey: parmIndex ifAbsent: [ ].
	self parameters at: parmIndex put: aRioParameter!

shutdown
	"Private - perform necessary actions when the image is being shut down.
	 Invoked at image shutdown by RioStatement class>>shutdown"

	self basicDrop!

sql
	"Answer the current SQL string for this statement"

	^sql!

sql: aString
	"Private - set the SQL string for this statement"
	sql := aString!

started
	"Private - invoked at image startup by RioStatement class>>started"

	hstmt := RioHandleBuffer new value: RioConstants current sqlNullHstmt.
	self initializeParameters.
	self prepared: false.
	self executed: false.!

stringOption: anOption
	"Answer the value of the string statement option requested."
	| param |

	param := String new: RioConstants current sqlMaxOptionStringLength + 1.
	^(self getOption: anOption param: param) trimNulls!

stringOption: anOption value: param
	"Set the given statement option to the value specified in 'param'"

	^self setOption: anOption value: param asString!

unbindColumns
	"Release all column buffers."
	self rawHstmt value = RioConstants current sqlNullHstmt
		ifTrue: [ self retcode: RioConstants current sqlSuccess]
		ifFalse:
			[ self check: (RioDatabase dll
						sqlFreeStmt: self rawHstmt asParameter
						option: RioConstants current sqlUnbind asParameter) ].
	^self retcode! !
!RioStatement categoriesFor: #afterCommit!private!private helpers! !
!RioStatement categoriesFor: #afterHandleAllocated!private!private helpers! !
!RioStatement categoriesFor: #afterRollback!private!private helpers! !
!RioStatement categoriesFor: #allocateStatementHandle!private!private helpers! !
!RioStatement categoriesFor: #basicDrop!private!private helpers! !
!RioStatement categoriesFor: #blobSize!accessing!public! !
!RioStatement categoriesFor: #blobSize:!accessing!public! !
!RioStatement categoriesFor: #cancel!execution control!public! !
!RioStatement categoriesFor: #check:!operations!private helpers!public! !
!RioStatement categoriesFor: #database!accessing!public! !
!RioStatement categoriesFor: #database:!private!private helpers! !
!RioStatement categoriesFor: #drop!execution control!public! !
!RioStatement categoriesFor: #dumpParameters:!public!streaming! !
!RioStatement categoriesFor: #execute!execution control!public! !
!RioStatement categoriesFor: #executed!execution control!public! !
!RioStatement categoriesFor: #executed:!private!private helpers! !
!RioStatement categoriesFor: #finalize!finalizing!public! !
!RioStatement categoriesFor: #finalizing!accessing!public! !
!RioStatement categoriesFor: #finalizing:!accessing!private! !
!RioStatement categoriesFor: #getOption:param:!options!private! !
!RioStatement categoriesFor: #hstmt!accessing!public! !
!RioStatement categoriesFor: #initialize!initializing!public! !
!RioStatement categoriesFor: #initializeParameters!private!private helpers! !
!RioStatement categoriesFor: #integerOption:!options!public! !
!RioStatement categoriesFor: #integerOption:value:!options!public! !
!RioStatement categoriesFor: #nativeSql!accessing!public! !
!RioStatement categoriesFor: #paramCount!parameters!public! !
!RioStatement categoriesFor: #parameters!parameters!public! !
!RioStatement categoriesFor: #prepare!execution control!public! !
!RioStatement categoriesFor: #prepared!execution control!public! !
!RioStatement categoriesFor: #prepared:!private!private helpers! !
!RioStatement categoriesFor: #printOn:!ANSI protocols-Object!public! !
!RioStatement categoriesFor: #processRetcode!private!private helpers! !
!RioStatement categoriesFor: #rawHstmt!private!private helpers! !
!RioStatement categoriesFor: #release!operations!public! !
!RioStatement categoriesFor: #resetParameters!execution control!public! !
!RioStatement categoriesFor: #retcode!accessing!public! !
!RioStatement categoriesFor: #retcode:!accessing!public! !
!RioStatement categoriesFor: #rowCount!accessing!public! !
!RioStatement categoriesFor: #setOption:value:!private helpers!public! !
!RioStatement categoriesFor: #setParameter:cType:parameterType:sqlType:precision:scale:value:valueSize:valueBytesAvailable:!private!private helpers! !
!RioStatement categoriesFor: #setParameter:value:!parameters!public! !
!RioStatement categoriesFor: #setParameter:value:sqlType:!parameters!public! !
!RioStatement categoriesFor: #setParameter:with:!parameters!public! !
!RioStatement categoriesFor: #shutdown!operations-shutdown!private! !
!RioStatement categoriesFor: #sql!accessing!public! !
!RioStatement categoriesFor: #sql:!accessing!private! !
!RioStatement categoriesFor: #started!operations-startup!private! !
!RioStatement categoriesFor: #stringOption:!options!public! !
!RioStatement categoriesFor: #stringOption:value:!options!public! !
!RioStatement categoriesFor: #unbindColumns!execution control!public! !

!RioStatement class methodsFor!

finalize
	SessionManager current
		removeEventsTriggeredFor: self.
	super finalize!

install
	"Perform actions necessary when the library containing this class is bound
	 to the image."

	SessionManager current
		when: #sessionStarted send: #started to: self;
		when: #sessionStopped send: #shutdown to: self.

	self beFinalizable.
	self started!

new
	^super new initialize!

on: aDatabase
	"Answer a new instance of RioStatement with its database reference set to aDatabase."

	^self new
		database: aDatabase;
		yourself.!

on: aDatabase sql: aString
	"Answer a new instance of RioStatement with its database reference set to aDatabase."

	^self new
		database: aDatabase;
		sql: aString;
		yourself.!

shutdown
	"Pass the #shutdown message to all instances of this class and 
	 derived subclasses"

	self withAllSubclassesDo: [ :aClass |
		aClass allInstances do: [ :aStmt | aStmt shutdown ] ]!

started
	"Send the #started message to all instances of this class and
	 derived subclasses"

	self withAllSubclassesDo: [ :aClass |
		aClass allInstances do: [ :aStmt | aStmt started ] ]!

uninstall
	self finalize! !
!RioStatement class categoriesFor: #finalize!finalizing!public! !
!RioStatement class categoriesFor: #install!installing!public! !
!RioStatement class categoriesFor: #new!instance creation!public! !
!RioStatement class categoriesFor: #on:!public!statement creation! !
!RioStatement class categoriesFor: #on:sql:!public!statement creation! !
!RioStatement class categoriesFor: #shutdown!operations-shutdown!public! !
!RioStatement class categoriesFor: #started!operations-startup!public! !
!RioStatement class categoriesFor: #uninstall!installing!public! !

RioTransaction guid: (GUID fromString: '{6BBB8F9B-559A-11D3-8269-00001D19F5C2}')!
RioTransaction comment: 'Represents a transaction on the database.  Applications will commonly use either RioReadOnlyTransaction or RioReadWriteTransaction.  Usage:

	db := RioDatabase on: <appropriate arguments>.

	(RioReadOnlyTransaction on: db) try: [
		results := (RioCursor on: db sql: ''select * from blah'') fetchAll ].

or

	db readOnlyTransaction try: [
		results := (RioCursor on: db sql: ''select * from blah'') fetchAll ].'!
!RioTransaction categoriesForClass!No category! !
!RioTransaction methodsFor!

accessMode
	self subclassResponsibility!

commit
	self db commit: self!

db
	"Answer the value of the receiver's instance variable db.
	This method was automatically generated, but may be modified."

	^db!

db: anObject
	"Set the value of the receiver's instance variable db to anObject.
	This method was automatically generated, but may be modified."

	db := anObject!

failureAction
	self rollback!

isCompatibleWith: aTransaction
	"Answers true if self can be a sub-transaction of aTransaction, otherwise answers false."
	self subclassResponsibility!

isReadOnly
	self subclassResponsibility!

rollback
	self db rollback: self!

startTransaction
	self db startTransaction: self!

successAction
	self subclassResponsibility!

try: aNiladicValuable
	self try: aNiladicValuable
		with: (ExceptionHandler new on: RioError do: [ :anException | self failureAction. anException signal ])!

try: aNiladicValuable ensure: anotherNiladicValuable
	[ self try: aNiladicValuable ] ensure: anotherNiladicValuable!

try: aNiladicValuable on: selector do: action
	| handler |

	handler := ExceptionHandlerSet new
				on: selector do: action.
	selector ~= RioError
		ifTrue: [ handler on: RioError do: [ :anException | self failureAction. anException signal ] ].

	self try: aNiladicValuable with: handler!

try: aNiladicValuable on: selector do: action ensure: anotherNiladicValuable
	[ self try: aNiladicValuable
		on: selector
		do: action ] ensure: anotherNiladicValuable!

try: aNiladicValuable on: selector1 do: action1 on: selector2 do: action2
	| handler |

	handler := ExceptionHandlerSet new
				on: selector1 do: action1;
				on: selector2 do: action2.
	(selector1 ~= RioError) & (selector2 ~= RioError)
		ifTrue: [ handler on: RioError do: [ :anException | self failureAction. anException signal ] ].

	self try: aNiladicValuable with: handler!

try: aNiladicValuable on: selector1 do: action1 on: selector2 do: action2 ensure: anotherNiladicValuable
	[ self try: aNiladicValuable on: selector1 do: action1 on: selector2 do: action2 ]
		ensure: anotherNiladicValuable!

try: aNiladicValuable on: selector1 do: action1 on: selector2 do: action2 on: selector3 do: action3
	| handler |

	handler := ExceptionHandlerSet new
				on: selector1 do: action1;
				on: selector2 do: action2;
				on: selector3 do: action3.
	(selector1 ~= RioError) & (selector2 ~= RioError) & (selector3 ~= RioError)
		ifTrue: [ handler on: RioError do: [ :anException | self failureAction. anException signal ] ].

	self try: aNiladicValuable with: handler!

try: aNiladicValuable on: selector1 do: action1 on: selector2 do: action2 on: selector3 do: action3 ensure: anotherNiladicValuable
	[ self try: aNiladicValuable on: selector1 do: action1 on: selector2 do: action2 on: selector3 do: action3 ]
		ensure: anotherNiladicValuable!

try: aNiladicValuable with: anExceptionHandler
	"Private - execute the given block in the context of the given exception handler"
	anExceptionHandler try: [ self startTransaction.
						aNiladicValuable value.
						self successAction ]! !
!RioTransaction categoriesFor: #accessMode!accessing!public! !
!RioTransaction categoriesFor: #commit!private!transactions! !
!RioTransaction categoriesFor: #db!accessing!public! !
!RioTransaction categoriesFor: #db:!accessing!private! !
!RioTransaction categoriesFor: #failureAction!public!transactions! !
!RioTransaction categoriesFor: #isCompatibleWith:!public!testing! !
!RioTransaction categoriesFor: #isReadOnly!public!testing! !
!RioTransaction categoriesFor: #rollback!private!transactions! !
!RioTransaction categoriesFor: #startTransaction!private!transactions! !
!RioTransaction categoriesFor: #successAction!public!transactions! !
!RioTransaction categoriesFor: #try:!evaluating!public! !
!RioTransaction categoriesFor: #try:ensure:!evaluating!public! !
!RioTransaction categoriesFor: #try:on:do:!evaluating!public! !
!RioTransaction categoriesFor: #try:on:do:ensure:!evaluating!public! !
!RioTransaction categoriesFor: #try:on:do:on:do:!evaluating!public! !
!RioTransaction categoriesFor: #try:on:do:on:do:ensure:!evaluating!public! !
!RioTransaction categoriesFor: #try:on:do:on:do:on:do:!evaluating!public! !
!RioTransaction categoriesFor: #try:on:do:on:do:on:do:ensure:!evaluating!public! !
!RioTransaction categoriesFor: #try:with:!evaluating!private! !

!RioTransaction class methodsFor!

on: anRioDatabase
	^self new db: anRioDatabase! !
!RioTransaction class categoriesFor: #on:!instance creation!public! !

RioError guid: (GUID fromString: '{6BBB8F9E-559A-11D3-8269-00001D19F5C2}')!
RioError comment: ''!
!RioError categoriesForClass!No category! !
!RioError methodsFor!

defaultAction
	"A RioError has been signaled with no handler."

	SessionManager current onUnhandledError: self!

isFatal
	^false!

isRestartable
	^false! !
!RioError categoriesFor: #defaultAction!handling!public! !
!RioError categoriesFor: #isFatal!public!testing! !
!RioError categoriesFor: #isRestartable!public!testing! !

RioFatalError guid: (GUID fromString: '{6BBB8F9F-559A-11D3-8269-00001D19F5C2}')!
RioFatalError comment: ''!
!RioFatalError categoriesForClass!No category! !
!RioFatalError methodsFor!

isFatal
	^true! !
!RioFatalError categoriesFor: #isFatal!public!testing! !

RioRestartableError guid: (GUID fromString: '{6BBB8FA0-559A-11D3-8269-00001D19F5C2}')!
RioRestartableError comment: ''!
!RioRestartableError categoriesForClass!No category! !
!RioRestartableError methodsFor!

isRestartable
	^true! !
!RioRestartableError categoriesFor: #isRestartable!public!testing! !

RioOdbcLibrary guid: (GUID fromString: '{6BBB8FA1-559A-11D3-8269-00001D19F5C2}')!
RioOdbcLibrary comment: ''!
!RioOdbcLibrary categoriesForClass!Rio! !
!RioOdbcLibrary methodsFor!

sqlAllocConnect: henv hdbc: phdbc
	<stdcall: sword SQLAllocConnect handle lpvoid>
	^self invalidCall!

sqlAllocEnv: phenv
	<stdcall: sword SQLAllocEnv lpvoid>
	^self invalidCall!

sqlAllocHandle: handleType inputHandle: inputHandle outputHandle: pOutputHandle
	<stdcall: sword SQLAllocHandle sword handle lpvoid>
	^self invalidCall!

sqlAllocStmt: hdbc hstmt: phstmt
	<stdcall: sword SQLAllocStmt handle lpvoid>
	^self invalidCall!

sqlBindCol: hstmt column: icol cType: fCType buffer: rgbValue bufferLen: cbValueMax bytesAvailable: pcbValue
	<stdcall: sword SQLBindCol handle word sword lpvoid sdword lpvoid>
	^self invalidCall!

sqlBindParam: stmtHandle parameterNumber: parmNumber valueType: valueType parameterType: parameterType lengthPrecision: lengthPrecision parameterScale: parameterScale parameterValue: parameterValue lengthOrIndicator: lengthOrIndicator
	<stdcall: sword SQLBindParam handle sword sword sword sdword sword lpvoid lpvoid>
	^self invalidCall!

sqlBindParameter: hstmt parameterNumber: ipar parameterType: fParamType cType: fCType sqlType: fSqlType precision: cbColDef scale: ibScale value: rgbValue valueSize: cbValueMax valueBytesAvailable: pcbValue
	<stdcall: sword SQLBindParameter handle word sword sword sword dword sword lpvoid sdword lpvoid>
	^self invalidCall!

sqlBrowseConnect: hdbc connectionString: szConnStrIn connectionStringLen: cbConnStrIn connectionStringOut: szConnStrOut connectStringOutSize: cbConnStrOut connectStringOutLen: pcbConnStrOut
	<stdcall: sword SQLBrowseConnect handle lpvoid sword lpvoid sword lpvoid>
	^self invalidCall!

sqlBulkOperations: hstmt operation: operation
	<stdcall: sword SQLBulkOperation handle sword>
	^self invalidCall!

sqlCancel: hstmt
	<stdcall: sword SQLCancel handle>
	^self invalidCall!

sqlCloseCursor: hstmt
	<stdcall: sword SQLCloseCursor handle>
	^self invalidCall!

sqlColAttributes: hstmt columnNumber: icol descriptorType: fDescType buffer: rgbDesc bufferLen: cbDescMax bytesAvailable: pcbDesc integerDescriptor: pfDesc
	<stdcall: sword SQLColAttributes handle word word lpvoid sword lpvoid lpvoid>
	^self invalidCall!

sqlColumnPrivileges: hstmt catalogName: szCatalogName catalogLen: cbCatalogName schemaName: szSchemaName schemaLen: cbSchemaName tableName: szTableName tableLen: cbTableName columnName: szColumnName columnLen: cbColumnName
	<stdcall: sword SQLColumnPrivileges handle lpvoid sword lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlColumns: hstmt catalogName: szCatalogName catalogLen: cbCatalogName schemaName: szSchemaName schemaNameLen: cbSchemaName tableName: szTableName tableNameLen: cbTableName columnName: szColumnName columnNameLen: cbColumnName
	<stdcall: sword SQLColumns handle lpvoid sword lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlConnect: hdbc dataSource: szDSN dataSourceLen: cbDSN userId: szUID userIdLen: cbUID authorization: szAuthStr authorizationLen: cbAuthStr
	<stdcall: sword SQLConnect handle lpstr sword lpstr sword lpstr sword>
	^self invalidCall!

sqlCopyDesc: sourceDescHandle toTarget: targetDescHandle
	<stdcall: sword SQLCopyDesc handle handle>
	^self invalidCall!

sqlDataSources: henv direction: fDirection dsn: szDSN dsnMax: cbDSNMax dsnBytesAvailable: pcbDSN description: szDescription descriptionLen: cbDescriptionMax descriptionBytesAvailable: pcbDescription
	<stdcall: sword SQLDataSources handle word lpvoid sword lpvoid lpvoid sword lpvoid>
	^self invalidCall!

sqlDescribeCol: hstmt columnNumber: icol columnName: szColName columnNameSize: cbColNameMax columnNameBytesAvailable: pcbColName sqlType: pfSqlType precision: pcbColDef scale: pibScale nullable: pfNullable
	<stdcall: sword SQLDescribeCol handle word lpvoid sword lpvoid lpvoid lpvoid lpvoid lpvoid>
	^self invalidCall!

sqlDescribeParam: hstmt parameterNumber: ipar sqlType: pfSqlType precision: pcbParamDef scale: pibScale nullable: pfNullable
	<stdcall: sword SQLDescribeParam handle word lpvoid lpvoid lpvoid lpvoid>
	^self invalidCall!

sqlDisconnect: hdbc
	<stdcall: sword SQLDisconnect handle>
	^self invalidCall!

sqlDriverConnect: hdbc hwnd: hwnd connStrIn: szConnStrIn connStrInLen: cbConnStrIn connStrOut: szConnStrOut connStrOutSize: cbConnStrOutMax connStrOutBytesAvailable: pcbConnStrOut driverCompletionFlag: fDriverCompletion
	<stdcall: sword SQLDriverConnect handle dword lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlDrivers: henv direction: fDirection driverDesc: szDriverDesc driverDescSize: cbDriverDescMax driverDescBytesAvailable: pcbDriverDesc driverAttributes: szDriverAttributes driverAttributesSize: cbDrvrAttrMax driverAttributesBytesAvailable: pcbDrvrAttr
	<stdcall: sword SQLDrivers handle word lpvoid sword lpvoid lpvoid sword lpvoid>
	^self invalidCall!

sqlEndTran: handleType handle: aHandle completionType: completionType
	<stdcall: sword SQLEndTran sword handle sword>
	^self invalidCall!

sqlError: henv hdbc: hdbc hstmt: hstmt sqlState: szSqlState nativeError: pfNativeError errorMsg: szErrorMsg errorMsgSize: cbErrorMsgMax errorMsgBytesAvailable: pcbErrorMsg
	<stdcall: sword SQLError handle handle handle lpvoid lpvoid lpvoid sword lpvoid>
	^self invalidCall!

sqlExecDirect: hstmt sqlStmt: szSqlStr sqlStmtLen: cbSqlStr
	<stdcall: sword SQLExecDirect handle lpvoid sword>
	^self invalidCall!

sqlExecute: hstmt
	<stdcall: sword SQLExecute handle>
	^self invalidCall!

sqlExtendedFetch: hstmt fetchType: fFetchType rowNumber: irow rowsFetched: pcrow rowStatusArray: rgfRowStatus
	<stdcall: sword SQLExtendedFetch handle word sdword lpvoid lpvoid>
	^self invalidCall!

sqlFetch: hstmt
	<stdcall: sword SQLFetch handle>
	^self invalidCall!

sqlFetchScroll: hstmt orientation: fetchOrientation offset: fetchOffset
	<stdcall: sword SQLFetchScroll handle sword sdword>
	^self invalidCall!

sqlForeignKeys: hstmt pkCatalogName: szPkCatalogName pkCatalogNameLen: cbPkCatalogName pkSchemaName: szPkSchemaName pkSchemaNameLen: cbPkSchemaName pkTableName: szPkTableName pkTableNameLen: cbPkTableName fkCatalogName: szFkCatalogName fkCatalogNameLen: cbFkCatalogName fkSchemaName: szFkSchemaName fkSchemaNameLen: cbFkSchemaName fkTableName: szFkTableName fkTableNameLen: cbFkTableName
	<stdcall: sword SQLForeignKeys handle lpvoid sword lpvoid sword lpvoid sword lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlFreeConnect: hdbc
	<stdcall: sword SQLFreeConnect handle>
	^self invalidCall!

sqlFreeEnv: henv
	<stdcall: sword SQLFreeEnv handle>
	^self invalidCall!

sqlFreeHandle: handleType handle: aHandle
	<stdcall: sword SQLFreeHandle sword handle>
	^self invalidCall!

sqlFreeStmt: hstmt option: fOption
	<stdcall: sword SQLFreeStmt handle word>
	^self invalidCall!

sqlGetConnectAttr: hdbc attribute: attribute value: value bufferLength: bufferLength stringLength: stringLength
	<stdcall: sword SQLGetConnectAttr handle sdword lpvoid sdword lpvoid>
	^self invalidCall!

sqlGetConnectAttr: hdbc attribute: attribute value: value stringLength: stringLength
	<stdcall: sword SQLGetConnectAttr handle sdword lpvoid sdword>
	^self invalidCall!

sqlGetConnectOption: hdbc option: fOption param: pvParam
	<stdcall: sword SQLGetConnectOption handle word lpvoid>
	^self invalidCall!

sqlGetCursorName: hstmt cursorName: szCursor cursorNameSize: cbCursorMax cursorNameBytesAvailable: pcbCursor
	<stdcall: sword SQLGetCursorName handle lpvoid sword lpvoid>
	^self invalidCall!

sqlGetData: hstmt columnNumber: icol cType: fCType buffer: rgbValue bufferLen: cbValueMax bytesAvailable: pcbValue
	<stdcall: sword SQLGetData handle word sword lpvoid dword lpvoid>
	^self invalidCall!

sqlGetDescRec: aHandle recNumber: recNumber name: name bufferLength: bufferLength stringLength: stringLength type: type subType: subType length: length precision: precision scale: scale nullable: nullable
	<stdcall: sword SQLGetDescRec handle sword lpvoid sword lpvoid lpvoid lpvoid lpvoid lpvoid lpvoid lpvoid>
	^self invalidCall!

sqlGetDiagField: handleType handle: aHandle recNumber: recNumber diagIdentifier: diagIdentifier diagInfo: diagInfo bufferLength: bufferLength stringLength: stringLength
	<stdcall: sword SQLGetDiagField sword handle sword sword lpvoid sword lpvoid>
	^self invalidCall!

sqlGetDiagRec: handleType handle: aHandle recNumber: recNumber sqlstate: sqlstate nativeError: nativeError messageText: messageText bufferLength: bufferLength textLength: textLength
	<stdcall: sword SQLGetDiagRec sword handle sword lpvoid lpvoid lpvoid sword lpvoid>
	^self invalidCall!

sqlGetEnvAttr: henv attribute: attribute value: value bufferLength: bufferLength stringLength: stringLength
	<stdcall: sword SQLGetEnvAttr handle sdword lpvoid sdword lpvoid>
	^self invalidCall!

sqlGetFunctions: hdbc function: fFunction exists: pfExists
	<stdcall: sword SQLGetFunctions handle word lpvoid>
	^self invalidCall!

sqlGetInfo: hdbc infoType: fInfoType buffer: rgbInfoValue bufferLen: cbInfoValueMax bytesAvailable: pcbInfoValue
	<stdcall: sword SQLGetInfo handle word lpvoid sword lpvoid>
	^self invalidCall!

sqlGetStmtAttr: hstmt attribute: attribute value: value bufferLength: bufferLength stringLength: stringLength
	<stdcall: sword SQLGetStmtAttr handle sdword lpvoid sdword lpvoid>
	^self invalidCall!

sqlGetStmtOption: hstmt option: fOption param: pvParam
	<stdcall: sword SQLGetStmtOption handle word lpvoid>
	^self invalidCall!

sqlGetTypeInfo: hstmt forType: fSqlType
	<stdcall: sword SQLGetTypeInfo handle sword>
	^self invalidCall!

sqlMoreResults: hstmt
	<stdcall: sword SQLMoreResults handle>
	^self invalidCall!

sqlNativeSql: hdbc sqlStringIn: szSqlStrIn sqlStringInLen: cbSqlStrIn sqlStringOut: szSqlStr sqlStringOutSize: cbSqlStrMax sqlStringOutBytesAvailable: pcbSqlStr
	<stdcall: sword SQLNativeSql handle lpvoid sdword lpvoid sdword lpvoid>
	^self invalidCall!

sqlNumParams: hstmt parameterCount: pcpar
	<stdcall: sword SQLNumParams handle lpvoid>
	^self invalidCall!

sqlNumResultCols: hstmt numResultCols: pccol
	<stdcall: sword SQLNumResultCols handle lpvoid>
	^self invalidCall!

sqlParamData: hstmt value: prgbValue
	<stdcall: sword SQLParamData handle lpvoid>
	^self invalidCall!

sqlParamOptions: hstmt valueCount: crow currentRow: pirow
	<stdcall: sword SQLParamOptions handle dword lpvoid>
	^self invalidCall!

sqlPrepare: hstmt sqlString: szSqlStr sqlStringLen: cbSqlStr
	<stdcall: sword SQLPrepare handle lpvoid sdword>
	^self invalidCall!

sqlPrimaryKeys: hstmt catalogName: szCatalogName catalogNameLen: cbCatalogName schemaName: szSchemaName schemaNameLen: cbSchemaName tableName: szTableName tableNameLen: cbTableName
	<stdcall: sword SQLPrimaryKeys handle lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlProcedureColumns: hstmt catalogName: szCatalogName catalogNameLen: cbCatalogName schemaName: szSchemaName schemaNameLen: cbSchemaName procName: szProcName procNameLen: cbProcName columnName: szColumnName columnNameLen: cbColumnName
	<stdcall: sword SQLProcedureColumns handle lpvoid sword lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlProcedures: hstmt catalogName: szCatalogName catalogNameLen: cbCatalogName schemaName: szSchemaName schemaNameLen: cbSchemaName procName: szProcName procNameLen: cbProcName
	<stdcall: sword SQLProcedures handle lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlPutData: hstmt value: rgbValue valueLen: cbValue
	<stdcall: sword SQLPutData handle lpvoid sdword>
	^self invalidCall!

sqlRowCount: hstmt rowsAffected: pcrow
	<stdcall: sword SQLRowCount handle lpvoid>
	^self invalidCall!

sqlSetConnectOption: hdbc option: fOption param: vParam
	<stdcall: sword SQLSetConnectOption handle word dword>
	^self invalidCall!

sqlSetCursorName: hstmt cursorName: szCursor cursorNameLen: cbCursor
	<stdcall: sword SQLSetCursorName handle lpvoid sword>
	^self invalidCall!

sqlSetDescField: hdesc recNumber: recNumber fieldIdentifier: fieldIdentifier value: value bufferLength: bufferLength
	<stdcall: sword SQLSetDescField handle sword sword lpvoid sdword>
	^self invalidCall!

sqlSetDescRec: hdesc recNumber: recNumber type: type subType: subType length: length precision: precision scale: scale data: data stringLength: stringLength indicator: indicator
	<stdcall: sword SQLSetDescRec handle sword sword sword sdword sword sword lpvoid lpvoid lpvoid>
	^self invalidCall!

sqlSetEnvAttr: henv attribute: attribute value: value stringLength: stringLength
	<stdcall: sword SQLSetEnvAttr handle sdword lpvoid sdword>
	^self invalidCall!

sqlSetParam: hstmt parameterNumber: parameterNumber valueType: valueType parameterType: parameterType lengthPrecision: lengthPrecision parameterScale: parameterScale parameterValue: parameterValue lengthOrIndicator: lengthOrIndicator
	<stdcall: sword SQLSetParam handle sword sword sword sdword sword lpvoid lpvoid>
	^self invalidCall!

sqlSetPos: hstmt row: irow option: fOption lockType: fLock
	<stdcall: sword SQLSetPos handle word word word>
	^self invalidCall!

sqlSetStmtAttr: hstmt attribute: attribute value: value stringLength: stringLength
	<stdcall: sword SQLSetStmtAttr handle sdword lpvoid sdword>
	^self invalidCall!

sqlSetStmtOption: hstmt option: fOption param: vParam
	<stdcall: sword SQLSetStmtOption handle word dword>
	^self invalidCall!

sqlSpecialColumns: hstmt colType: fColType catalogName: szCatalogName catalogNameLen: cbCatalogName schemaName: szSchemaName schemaNameLen: cbSchemaNameLen tableName: szTableName tableNameLen: cbTableName scope: fScope nullable: fNullable
	<stdcall: sword SQLSpecialColumns handle word lpvoid sword lpvoid sword lpvoid sword word word>
	^self invalidCall!

sqlStatistics: hstmt catalogName: szCatalogName catalogNameLen: cbCatalogName schemaName: szSchemaName schemaNameLen: cbSchemaName tableName: szTableName tableNameLen: cbTableName unique: fUnique accuracy: fAccuracy
	<stdcall: sword SQLStatistics handle lpvoid sword lpvoid sword lpvoid sword word word>
	^self invalidCall!

sqlTablePrivileges: hstmt catalogName: szCatalogName catalogNameLen: cbCatalogName schemaName: szSchemaName schemaNameLen: cbSchemaName tableName: szTableName tableNameLen: cbTableName
	<stdcall: sword SQLTablePrivileges handle lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlTables: hstmt catalogName: szCatalogName catalogLen: cbCatalogName schemaName: szSchemaName schemaNameLen: cbSchemaName tableName: szTableName tableNameLen: cbTableName tableType: szTableType tableTypeLen: cbTableType
	<stdcall: sword SQLTables handle lpvoid sword lpvoid sword lpvoid sword lpvoid sword>
	^self invalidCall!

sqlTransact: henv hdbc: hdbc type: fType
	<stdcall: sword SQLTransact handle handle word>
	^self invalidCall! !
!RioOdbcLibrary categoriesFor: #sqlAllocConnect:hdbc:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlAllocEnv:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlAllocHandle:inputHandle:outputHandle:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlAllocStmt:hstmt:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlBindCol:column:cType:buffer:bufferLen:bytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlBindParam:parameterNumber:valueType:parameterType:lengthPrecision:parameterScale:parameterValue:lengthOrIndicator:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlBindParameter:parameterNumber:parameterType:cType:sqlType:precision:scale:value:valueSize:valueBytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlBrowseConnect:connectionString:connectionStringLen:connectionStringOut:connectStringOutSize:connectStringOutLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlBulkOperations:operation:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlCancel:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlCloseCursor:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlColAttributes:columnNumber:descriptorType:buffer:bufferLen:bytesAvailable:integerDescriptor:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlColumnPrivileges:catalogName:catalogLen:schemaName:schemaLen:tableName:tableLen:columnName:columnLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlColumns:catalogName:catalogLen:schemaName:schemaNameLen:tableName:tableNameLen:columnName:columnNameLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlConnect:dataSource:dataSourceLen:userId:userIdLen:authorization:authorizationLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlCopyDesc:toTarget:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlDataSources:direction:dsn:dsnMax:dsnBytesAvailable:description:descriptionLen:descriptionBytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlDescribeCol:columnNumber:columnName:columnNameSize:columnNameBytesAvailable:sqlType:precision:scale:nullable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlDescribeParam:parameterNumber:sqlType:precision:scale:nullable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlDisconnect:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlDriverConnect:hwnd:connStrIn:connStrInLen:connStrOut:connStrOutSize:connStrOutBytesAvailable:driverCompletionFlag:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlDrivers:direction:driverDesc:driverDescSize:driverDescBytesAvailable:driverAttributes:driverAttributesSize:driverAttributesBytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlEndTran:handle:completionType:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlError:hdbc:hstmt:sqlState:nativeError:errorMsg:errorMsgSize:errorMsgBytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlExecDirect:sqlStmt:sqlStmtLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlExecute:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlExtendedFetch:fetchType:rowNumber:rowsFetched:rowStatusArray:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlFetch:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlFetchScroll:orientation:offset:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlForeignKeys:pkCatalogName:pkCatalogNameLen:pkSchemaName:pkSchemaNameLen:pkTableName:pkTableNameLen:fkCatalogName:fkCatalogNameLen:fkSchemaName:fkSchemaNameLen:fkTableName:fkTableNameLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlFreeConnect:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlFreeEnv:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlFreeHandle:handle:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlFreeStmt:option:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetConnectAttr:attribute:value:bufferLength:stringLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetConnectAttr:attribute:value:stringLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetConnectOption:option:param:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetCursorName:cursorName:cursorNameSize:cursorNameBytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetData:columnNumber:cType:buffer:bufferLen:bytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetDescRec:recNumber:name:bufferLength:stringLength:type:subType:length:precision:scale:nullable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetDiagField:handle:recNumber:diagIdentifier:diagInfo:bufferLength:stringLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetDiagRec:handle:recNumber:sqlstate:nativeError:messageText:bufferLength:textLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetEnvAttr:attribute:value:bufferLength:stringLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetFunctions:function:exists:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetInfo:infoType:buffer:bufferLen:bytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetStmtAttr:attribute:value:bufferLength:stringLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetStmtOption:option:param:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlGetTypeInfo:forType:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlMoreResults:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlNativeSql:sqlStringIn:sqlStringInLen:sqlStringOut:sqlStringOutSize:sqlStringOutBytesAvailable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlNumParams:parameterCount:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlNumResultCols:numResultCols:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlParamData:value:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlParamOptions:valueCount:currentRow:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlPrepare:sqlString:sqlStringLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlPrimaryKeys:catalogName:catalogNameLen:schemaName:schemaNameLen:tableName:tableNameLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlProcedureColumns:catalogName:catalogNameLen:schemaName:schemaNameLen:procName:procNameLen:columnName:columnNameLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlProcedures:catalogName:catalogNameLen:schemaName:schemaNameLen:procName:procNameLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlPutData:value:valueLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlRowCount:rowsAffected:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetConnectOption:option:param:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetCursorName:cursorName:cursorNameLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetDescField:recNumber:fieldIdentifier:value:bufferLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetDescRec:recNumber:type:subType:length:precision:scale:data:stringLength:indicator:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetEnvAttr:attribute:value:stringLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetParam:parameterNumber:valueType:parameterType:lengthPrecision:parameterScale:parameterValue:lengthOrIndicator:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetPos:row:option:lockType:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetStmtAttr:attribute:value:stringLength:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSetStmtOption:option:param:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlSpecialColumns:colType:catalogName:catalogNameLen:schemaName:schemaNameLen:tableName:tableNameLen:scope:nullable:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlStatistics:catalogName:catalogNameLen:schemaName:schemaNameLen:tableName:tableNameLen:unique:accuracy:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlTablePrivileges:catalogName:catalogNameLen:schemaName:schemaNameLen:tableName:tableNameLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlTables:catalogName:catalogLen:schemaName:schemaNameLen:tableName:tableNameLen:tableType:tableTypeLen:!public!win32 functions-odbc library! !
!RioOdbcLibrary categoriesFor: #sqlTransact:hdbc:type:!public!win32 functions-odbc library! !

!RioOdbcLibrary class methodsFor!

fileName
        "Answer the receiver's file name."
     ^'odbc32.dll'! !
!RioOdbcLibrary class categoriesFor: #fileName!accessing!public! !

RioBigintColumnHeader guid: (GUID fromString: '{A6E68941-A536-11D4-BDF4-00010240D5E2}')!
RioBigintColumnHeader comment: ''!
!RioBigintColumnHeader categoriesForClass!Unclassified! !
!RioBigintColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	"Set the value of this column in the specified row to the given value,
	answering the new value.  We assume at this point that anObject ~~ nil.
	Answers the number of bytes of data assigned to the buffer."

	| string count dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 

	string := anObject displayString, (String with: 0 asCharacter).
	self dataSize >= string size
		ifTrue: [ count := string size ]
		ifFalse:
			[ string at: self dataSize put: 0 asCharacter.
			count := self dataSize ].
	aRowsetBuffer stringAtOffset: dataOffset count: count put: string.
	^count - 1!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset stringValue rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	stringValue := (aRowsetBuffer stringAtOffset: dataOffset count: self precision) trimNulls trimBlanks.
	^Number fromString: stringValue! !
!RioBigintColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioBigintColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioBitColumnHeader guid: (GUID fromString: '{A6E6894A-A536-11D4-BDF4-00010240D5E2}')!
RioBitColumnHeader comment: ''!
!RioBitColumnHeader categoriesForClass!Unclassified! !
!RioBitColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	| dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 
	aRowsetBuffer sdwordAtOffset: dataOffset put: anObject asInteger.
	^self dataSize!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	^aRowsetBuffer byteAtOffset: dataOffset! !
!RioBitColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioBitColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioCharacterColumnHeader guid: (GUID fromString: '{A6E68940-A536-11D4-BDF4-00010240D5E2}')!
RioCharacterColumnHeader comment: ''!
!RioCharacterColumnHeader categoriesForClass!Unclassified! !
!RioCharacterColumnHeader methodsFor!

basicExtractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset rowOffset |

	"Private - First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	^aRowsetBuffer stringAtOffset: dataOffset count: self precision
!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	| string count dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 

	string := anObject displayString, (String with: 0 asCharacter).
	self dataSize >= string size
		ifTrue: [ count := string size ]
		ifFalse:
			[ string at: self dataSize put: 0 asCharacter.
			count := self dataSize ].
	aRowsetBuffer stringAtOffset: dataOffset count: count put: string.
	^count - 1!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| val |
	val := self basicExtractValueFromBuffer: aRowsetBuffer row: anInteger.
	val notNil ifTrue: [ val := val trimBlanks ].
	^val!

isQuotable
	"Answer true if printed versions of fields in this column should be surrounded by quotes."
	^true! !
!RioCharacterColumnHeader categoriesFor: #basicExtractValueFromBuffer:row:!accessing!private! !
!RioCharacterColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioCharacterColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !
!RioCharacterColumnHeader categoriesFor: #isQuotable!public!testing! !

RioDateColumnHeader guid: (GUID fromString: '{A6E68943-A536-11D4-BDF4-00010240D5E2}')!
RioDateColumnHeader comment: ''!
!RioDateColumnHeader categoriesForClass!Unclassified! !
!RioDateColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: aDate
	| string count dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 

	string := aDate odbcString, (String with: 0 asCharacter).
	self dataSize >= string size
		ifTrue: [ count := string size ]
		ifFalse: [
			string at: self dataSize put: 0 asCharacter.
			count := self dataSize ].
	aRowsetBuffer stringAtOffset: dataOffset count: count put: string.
	^count - 1!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset stringValue rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	stringValue := (aRowsetBuffer stringAtOffset: dataOffset count: self precision) trimNulls trimBlanks.
	^Date fromOdbcString: stringValue! !
!RioDateColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioDateColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioDecimalColumnHeader guid: (GUID fromString: '{A6E68942-A536-11D4-BDF4-00010240D5E2}')!
RioDecimalColumnHeader comment: ''!
!RioDecimalColumnHeader categoriesForClass!Unclassified! !
!RioDecimalColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	| string count dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 

	string := anObject displayString, (String with: 0 asCharacter).
	self dataSize >= string size
		ifTrue: [ count := string size ]
		ifFalse: [
			string at: self dataSize put: 0 asCharacter.
			count := self dataSize ].
	aRowsetBuffer stringAtOffset: dataOffset count: count put: string.
	^count - 1!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset stringValue rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	stringValue := (aRowsetBuffer stringAtOffset: dataOffset count: self precision) trimNulls trimBlanks.
	stringValue first = $. ifTrue: [ stringValue := '0', stringValue ].
	^Number fromString: stringValue, 's'! !
!RioDecimalColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioDecimalColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioDoubleColumnHeader guid: (GUID fromString: '{A6E68949-A536-11D4-BDF4-00010240D5E2}')!
RioDoubleColumnHeader comment: ''!
!RioDoubleColumnHeader categoriesForClass!Unclassified! !
!RioDoubleColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	| string count dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 
	aRowsetBuffer doubleAtOffset: dataOffset put: anObject asFloat.
	^self dataSize!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset stringValue rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	^aRowsetBuffer doubleAtOffset: dataOffset! !
!RioDoubleColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioDoubleColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioIntegerColumnHeader guid: (GUID fromString: '{A6E68947-A536-11D4-BDF4-00010240D5E2}')!
RioIntegerColumnHeader comment: ''!
!RioIntegerColumnHeader categoriesForClass!Unclassified! !
!RioIntegerColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	| dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 
	aRowsetBuffer sdwordAtOffset: dataOffset put: anObject asInteger.
	^self dataSize!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset stringValue rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	^aRowsetBuffer sdwordAtOffset: dataOffset! !
!RioIntegerColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioIntegerColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioSmallintColumnHeader guid: (GUID fromString: '{A6E68948-A536-11D4-BDF4-00010240D5E2}')!
RioSmallintColumnHeader comment: ''!
!RioSmallintColumnHeader categoriesForClass!Unclassified! !
!RioSmallintColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: anObject
	| dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 
	aRowsetBuffer swordAtOffset: dataOffset put: anObject asInteger.
	^self dataSize!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset stringValue rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	^aRowsetBuffer swordAtOffset: dataOffset! !
!RioSmallintColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioSmallintColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioTimeColumnHeader guid: (GUID fromString: '{A6E68944-A536-11D4-BDF4-00010240D5E2}')!
RioTimeColumnHeader comment: ''!
!RioTimeColumnHeader categoriesForClass!Unclassified! !
!RioTimeColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: aTime
	| string count dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 

	string := aTime odbcString, (String with: 0 asCharacter).
	self dataSize >= string size
		ifTrue: [ count := string size ]
		ifFalse: [
			string at: self dataSize put: 0 asCharacter.
			count := self dataSize ].
	aRowsetBuffer stringAtOffset: dataOffset count: count put: string.
	^count - 1!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset stringValue rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	stringValue := (aRowsetBuffer stringAtOffset: dataOffset count: self precision) trimNulls trimBlanks.
	^Time fromOdbcString: stringValue! !
!RioTimeColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioTimeColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioTimeStampColumnHeader guid: (GUID fromString: '{A6E68945-A536-11D4-BDF4-00010240D5E2}')!
RioTimeStampColumnHeader comment: ''!
!RioTimeStampColumnHeader categoriesForClass!Unclassified! !
!RioTimeStampColumnHeader methodsFor!

basicSetValueInBuffer: aRowsetBuffer row: anInteger value: aTimeStamp
	| string count dataOffset |

	dataOffset := (aRowsetBuffer rowOffset: anInteger) + self offset. 

	string := aTimeStamp odbcString, (String with: 0 asCharacter).
	self dataSize >= string size
		ifTrue: [ count := string size ]
		ifFalse: [
			string at: self dataSize put: 0 asCharacter.
			count := self dataSize ].
	aRowsetBuffer stringAtOffset: dataOffset count: count put: string.
	^count - 1!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| dataOffset stringValue rowOffset |

	"First check the indicator - if it's SQL_NULL_DATA answer nil."

	rowOffset := aRowsetBuffer rowOffset: anInteger.

	(aRowsetBuffer sdwordAtOffset: rowOffset + self indicatorOffset) = RioConstants current sqlNullData
		ifTrue: [ ^nil ].

	dataOffset := rowOffset + self offset.

	stringValue := (aRowsetBuffer stringAtOffset: dataOffset count: self precision) trimNulls trimBlanks.
	^TimeStamp fromOdbcString: stringValue! !
!RioTimeStampColumnHeader categoriesFor: #basicSetValueInBuffer:row:value:!operations!private! !
!RioTimeStampColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioVarcharColumnHeader guid: (GUID fromString: '{047424CD-A4F8-4B38-B364-69453421B8F3}')!
RioVarcharColumnHeader comment: ''!
!RioVarcharColumnHeader categoriesForClass!Unclassified! !
!RioVarcharColumnHeader methodsFor!

extractValueFromBuffer: aRowsetBuffer row: anInteger
	| val |
	val := self basicExtractValueFromBuffer: aRowsetBuffer row: anInteger.
	val notNil ifTrue: [ val := val trimNulls trimBlanks ].
	^val! !
!RioVarcharColumnHeader categoriesFor: #extractValueFromBuffer:row:!accessing!public! !

RioDoubleBuffer guid: (GUID fromString: '{6BBB8FA2-559A-11D3-8269-00001D19F5C2}')!
RioDoubleBuffer comment: ''!
!RioDoubleBuffer categoriesForClass!Rio! !
!RioDoubleBuffer methodsFor!

asFloat
	^self value!

value
	^self doubleAtOffset: 0!

value: aFloat
	self doubleAtOffset: 0 put: aFloat! !
!RioDoubleBuffer categoriesFor: #asFloat!converting!public! !
!RioDoubleBuffer categoriesFor: #value!accessing!public! !
!RioDoubleBuffer categoriesFor: #value:!accessing!public! !

!RioDoubleBuffer class methodsFor!

new
	^super new: 8! !
!RioDoubleBuffer class categoriesFor: #new!instance creation!public! !

RioDwordBuffer guid: (GUID fromString: '{6BBB8FA3-559A-11D3-8269-00001D19F5C2}')!
RioDwordBuffer comment: ''!
!RioDwordBuffer categoriesForClass!Rio! !
!RioDwordBuffer methodsFor!

asInteger
	^self value!

value
	^self dwordAtOffset: 0!

value: anInteger
	self dwordAtOffset: 0 put: anInteger! !
!RioDwordBuffer categoriesFor: #asInteger!converting!public! !
!RioDwordBuffer categoriesFor: #value!accessing!public! !
!RioDwordBuffer categoriesFor: #value:!accessing!public! !

!RioDwordBuffer class methodsFor!

new
	^super new: 4! !
!RioDwordBuffer class categoriesFor: #new!instance creation!public! !

RioSdwordBuffer guid: (GUID fromString: '{6BBB8FA4-559A-11D3-8269-00001D19F5C2}')!
RioSdwordBuffer comment: ''!
!RioSdwordBuffer categoriesForClass!Rio! !
!RioSdwordBuffer methodsFor!

asInteger
	^self value!

value
	^super sdwordAtOffset: 0!

value: anInteger
	super sdwordAtOffset: 0 put: anInteger! !
!RioSdwordBuffer categoriesFor: #asInteger!converting!public! !
!RioSdwordBuffer categoriesFor: #value!accessing!public! !
!RioSdwordBuffer categoriesFor: #value:!accessing!public! !

!RioSdwordBuffer class methodsFor!

new
	^super new: 4! !
!RioSdwordBuffer class categoriesFor: #new!instance creation!public! !

RioStringBuffer guid: (GUID fromString: '{6BBB8FA5-559A-11D3-8269-00001D19F5C2}')!
RioStringBuffer comment: ''!
!RioStringBuffer categoriesForClass!Rio! !
!RioStringBuffer methodsFor!

asString
	^self value!

value
	^super stringAtOffset: 0 count: self size!

value: aString
	| workString |

	aString size < self size
		ifTrue: [ workString := aString, (String new: (self size - aString size)) ]
		ifFalse: [ workString := aString copyFrom: 1 to: self size ].

	super stringAtOffset: 0 count: workString size put: workString! !
!RioStringBuffer categoriesFor: #asString!converting!public! !
!RioStringBuffer categoriesFor: #value!accessing!public! !
!RioStringBuffer categoriesFor: #value:!accessing!public! !

!RioStringBuffer class methodsFor!

new: anInteger
	^super new: anInteger + 1	"Leave room for a NUL character"! !
!RioStringBuffer class categoriesFor: #new:!instance creation!public! !

RioSwordBuffer guid: (GUID fromString: '{6BBB8FA6-559A-11D3-8269-00001D19F5C2}')!
RioSwordBuffer comment: ''!
!RioSwordBuffer categoriesForClass!Rio! !
!RioSwordBuffer methodsFor!

asInteger
	^self value!

value
	^super swordAtOffset: 0!

value: anInteger
	super swordAtOffset: 0 put: anInteger! !
!RioSwordBuffer categoriesFor: #asInteger!converting!public! !
!RioSwordBuffer categoriesFor: #value!accessing!public! !
!RioSwordBuffer categoriesFor: #value:!accessing!public! !

!RioSwordBuffer class methodsFor!

new
	^super new: 2! !
!RioSwordBuffer class categoriesFor: #new!instance creation!public! !

RioWordBuffer guid: (GUID fromString: '{6BBB8FA7-559A-11D3-8269-00001D19F5C2}')!
RioWordBuffer comment: ''!
!RioWordBuffer categoriesForClass!Rio! !
!RioWordBuffer methodsFor!

asInteger
	^self value!

value
	^super wordAtOffset: 0!

value: anInteger
	super wordAtOffset: 0 put: anInteger! !
!RioWordBuffer categoriesFor: #asInteger!converting!public! !
!RioWordBuffer categoriesFor: #value!accessing!public! !
!RioWordBuffer categoriesFor: #value:!accessing!public! !

!RioWordBuffer class methodsFor!

new
	^super new: 2! !
!RioWordBuffer class categoriesFor: #new!instance creation!public! !

RioHandleBuffer guid: (GUID fromString: '{6BBB8FA8-559A-11D3-8269-00001D19F5C2}')!
RioHandleBuffer comment: 'This class may seem superfluous (no methods, no data), but I think there''s some value in using a class name which indicates the domain being modeled (in this case, Win32 handles).'!
!RioHandleBuffer categoriesForClass!Rio! !
RioIntegerParameter guid: (GUID fromString: '{6BBB8FA9-559A-11D3-8269-00001D19F5C2}')!
RioIntegerParameter comment: ''!
!RioIntegerParameter categoriesForClass!Rio! !
!RioIntegerParameter methodsFor!

initialize
	super initialize.
	self cType: RioConstants current sqlCSlong.
	self sqlType: RioConstants current sqlInteger.
	self valueSize: 4.!

value: anObject
	self
		precision: 0;
		scale: 0;
		valueSize: 4.

	anObject isNil
		ifTrue: [
			value := (RioExternalBuffer new: 4) sdwordAtOffset: 0 put: 0.
			self valueBytesAvailable: RioConstants current sqlNullData ]
		ifFalse: [
			value := (RioExternalBuffer new: 4) sdwordAtOffset: 0 put: anObject asInteger.
			self valueBytesAvailable: self valueSize ]! !
!RioIntegerParameter categoriesFor: #initialize!initializing!public! !
!RioIntegerParameter categoriesFor: #value:!accessing!public! !

RioStringParameter guid: (GUID fromString: '{6BBB8FAA-559A-11D3-8269-00001D19F5C2}')!
RioStringParameter comment: ''!
!RioStringParameter categoriesForClass!Rio! !
!RioStringParameter methodsFor!

initialize
	super initialize.
	self cType: RioConstants current sqlCChar.
	self sqlType: RioConstants current sqlChar
	!

printOn: aStream
	| aString |

	aString := self value stringAtOffset: 0 count: self valueSize.

	super printOn: aStream.
	aStream
		tab; nextPutAll: 'value='; nextPutAll: self value printString;
			nextPutAll: '='''; nextPutAll: aString; nextPutAll: ''''; cr!

value: anObject
	anObject isNil
		ifTrue: [
			value := (RioExternalBuffer new: 1) byteAtOffset: 0 put: 0.
			self precision: 1.
			self scale: 0.
			self valueSize: 1.
			self valueBytesAvailable: RioConstants current sqlNullData ]
		ifFalse: [ | aString |
			aString := anObject displayString.

			value := (RioExternalBuffer new: aString size) stringAtOffset: 0 count: aString size put: aString.
			self precision: aString size.
			self scale: 0.
			self valueSize: aString size.
 			self valueBytesAvailable: aString size ]! !
!RioStringParameter categoriesFor: #initialize!initializing!public! !
!RioStringParameter categoriesFor: #printOn:!ANSI protocols-Object!public! !
!RioStringParameter categoriesFor: #value:!accessing!public! !

RioRow guid: (GUID fromString: '{6BBB8FAB-559A-11D3-8269-00001D19F5C2}')!
RioRow comment: ''!
!RioRow categoriesForClass!Rio! !
!RioRow methodsFor!

areColumnsDbDirty: anArray
	"Determine if any columns named in the array anArray are db-dirty.  Answer
	 true if any are dirty, otherwise false."

	^anArray detect: [ :aColumnName | self isColumnDbDirty: aColumnName] ifNone: [ false ]!

areColumnsNil: anArray
	"Determine if any columns named in the array anArray are nil (NULL in
	 SQL terms).  Answer true if any are nil, otherwise false."

	^anArray detect: [ :aColumnName | (self at: aColumnName) isNil ] ifNone: [ false ]!

areColumnsUiDirty: anArray
	"Determine if any columns named in the array anArray are ui-dirty.  Answer
	 true if any are dirty, otherwise false."

	^anArray detect: [ :aColumnName | self isColumnUiDirty: aColumnName ] ifNone: [ false ]!

at: anIntegerOrString
	"If anIntegerOrString is an Integer, answers the value of the column
	 at that index in rowsetBuffer>>rowsetHeader>>columnHeaders.  If
	 anIntegerOrString is a String, answers the value of the column
	 with that name in rowsetBuffer>>rowsetHeader>>columnHeadersByName."

	| aColumnHeader |

	aColumnHeader := self rowsetHeader at: anIntegerOrString.
	^aColumnHeader extractValueFromBuffer: self rowsetBuffer row: startRow!

at: anIntegerOrString put: anObject
	"If anIntegerOrString is an Integer, sets the value of the column
	 at that index in rowsetBuffer>>rowsetHeader>>columnHeaders to
	 anObject.  If anIntegerOrString is a String, sets the value of the column
	 with that name in rowsetBuffer>>rowsetHeader>>columnHeadersByName
	 to anObject."

	| aColumnHeader |

	aColumnHeader := self rowsetHeader at: anIntegerOrString.
	^aColumnHeader setValueInBuffer: self rowsetBuffer row: startRow value: anObject!

clearColumnDbDirty: anIntegerOrString
	| aColumnHeader |

	aColumnHeader := self rowsetHeader at: anIntegerOrString.
	aColumnHeader clearDbDirtyFlagInBuffer: self rowsetBuffer row: startRow!

clearColumnUiDirty: anIntegerOrString
	| aColumnHeader |

	aColumnHeader := self rowsetHeader at: anIntegerOrString.
	aColumnHeader clearUiDirtyFlagInBuffer: self rowsetBuffer row: startRow!

clearDbDirty
	"Clears the DB dirty flag on all fields in this row"

	1 to: self nCols do: [ :i | self clearColumnDbDirty: i ]!

clearUiDirty
	"Clears the UI dirty flag on all fields in this row"

	1 to: self nCols do: [ :i | self clearColumnUiDirty: i ]!

columnValues: anArray
	"Answer an array containing values for the columns named in anArray."

	^anArray collect: [ :each | self at: each ]!

displayOn: aStream
	"Display the contents of this row"

	1 to: self nCols do:
		[ :i |
		i > 1
			ifTrue: [ aStream nextPutAll: '   ' ].

		aStream
			nextPutAll: (rowsetBuffer rowsetHeader at: i) name;
			nextPutAll: '=';
			nextPutAll: (self at: i) printString.

		(self isColumnDbDirty: i) | (self isColumnUiDirty: i) ifTrue:
			[ aStream nextPutAll: ' [' ].

		(self isColumnDbDirty: i)
			ifTrue: [ aStream nextPutAll: 'D' ].

		(self isColumnUiDirty: i)
			ifTrue: [ aStream nextPutAll: 'U' ].

		(self isColumnDbDirty: i) | (self isColumnUiDirty: i) ifTrue:
			[ aStream nextPutAll: ' ]' ] ].

!

doesNotUnderstand: message
	"Determine if the message received is a valid column name in this row.  If
	 so, take appropriate action.  If not, defer to the superclass version of this
	 method.  Expected form is either
		row ColumnName
	to retrieve a column value or
		row ColumnName: value
	to set a column value."

	| selectorPieces columnName |

	message arguments size < 2 ifTrue:
		[ message selector last = $:
			ifFalse:
				[ "No $: so we assume the message selector is a column name"

				(self rowsetHeader at: message selector asString) notNil ifTrue:
					[ ^self at: message selector asString ] ]
			ifTrue:
				[ selectorPieces := message selector subStrings: $:.
				columnName := selectorPieces first.
				(self rowsetHeader at: columnName) notNil ifTrue:
					[ ^self at: columnName put: message arguments first ] ] ].

	^super doesNotUnderstand: message!

existsIn: aTableNameString in: anRioDatabase usingKeys: anArray
	"Check to see if this row exists in the given table with the names of key fields given in anArray."

	| sql stmt aRowSet s |

	sql := String new.
	s := sql writeStream.

	s
		nextPutAll: 'select count(*) as COUNTER from ';
		nextPutAll: aTableNameString;
		nextPutAll: ' where '.

	anArray do:
		[ :aFieldNameString | | quotable |
		s
			nextPutAll: aFieldNameString;
			nextPut: $=.
		quotable := (self rowsetHeader at: aFieldNameString) isQuotable.
		quotable ifTrue: [ s nextPut: '''' ].
		s nextPutAll: (self at: aFieldNameString).
		quotable ifTrue: [ s nextPut: '''' ].
		aFieldNameString ~~ anArray last ifTrue: [ s nextPutAll: ' AND ' ] ].

	s close.
	stmt := anRioDatabase createForwardCursor: sql trimNulls.
	aRowSet := stmt next.
	stmt drop.

	^(aRowSet first at: 'COUNTER') > 0!

initialize
	super initialize.
	self nRows: 1!

isColumnDbDirty: anIntegerOrString
	"If anIntegerOrString is an Integer, answers the 'dirty' status  of the
	 column at that index in rowsetBuffer>>rowsetHeader>>columnHeaders.  If
	 anIntegerOrString is a String, answers the 'dirty' status of the column
	 with that name in rowsetBuffer>>rowsetHeader>>columnHeadersByName."

	| aColumnHeader |

	aColumnHeader := self rowsetHeader at: anIntegerOrString.
	^aColumnHeader extractDbDirtyFlagFromBuffer: self rowsetBuffer row: startRow!

isColumnUiDirty: anIntegerOrString
	"If anIntegerOrString is an Integer, answers the 'dirty' status  of the
	 column at that index in rowsetBuffer>>rowsetHeader>>columnHeaders.  If
	 anIntegerOrString is a String, answers the 'dirty' status of the column
	 with that name in rowsetBuffer>>rowsetHeader>>columnHeadersByName."

	| aColumnHeader |

	aColumnHeader := self rowsetHeader at: anIntegerOrString.
	^aColumnHeader extractUiDirtyFlagFromBuffer: self rowsetBuffer row: startRow!

isDbDirty
	"Answer true if any fields in this row have been changed, otherwise
	 answer false."

	1 to: self nCols do: [ :i | (self isColumnDbDirty: i) ifTrue: [ ^true ] ].
	^false!

isUiDirty
	"Answer true if any fields in this row have been changed, otherwise
	 answer false."

	1 to: self nCols do: [ :i | (self isColumnUiDirty: i) ifTrue: [ ^true ] ].
	^false!

nCols
	^self rowsetBuffer nCols!

printOn: aStream
	"Dump the contents of this row"

	aStream nextPutAll: 'a RioRow ('.
	self displayOn: aStream.
	aStream nextPutAll: ')'!

rowStatus: anInteger
	"This message is not appropriate when sent to a RioRow."

	RioFatalError signal: '#rowStatus should not be sent to instances of RioRow'!

setColumnDbDirty: anIntegerOrString
	| aColumnHeader |

	aColumnHeader := self rowsetHeader at: anIntegerOrString.
	aColumnHeader setDbDirtyFlagInBuffer: self rowsetBuffer row: startRow!

setColumnUiDirty: anIntegerOrString
	| aColumnHeader |

	aColumnHeader := self rowsetHeader at: anIntegerOrString.
	aColumnHeader setUiDirtyFlagInBuffer: self rowsetBuffer row: startRow!

setDbDirty
	"Sets all fields in this row to be DB dirty"

	1 to: self nCols do: [ :i | self setColumnDbDirty: i ]!

setUiDirty
	"Sets all fields in this row to be UI dirty"

	1 to: self nCols do: [ :i | self setColumnUiDirty: i ]!

status
	^super rowStatus: 1! !
!RioRow categoriesFor: #areColumnsDbDirty:!dirty flags!public! !
!RioRow categoriesFor: #areColumnsNil:!public!testing! !
!RioRow categoriesFor: #areColumnsUiDirty:!dirty flags!public! !
!RioRow categoriesFor: #at:!accessing!public! !
!RioRow categoriesFor: #at:put:!accessing!public! !
!RioRow categoriesFor: #clearColumnDbDirty:!dirty flags!public! !
!RioRow categoriesFor: #clearColumnUiDirty:!dirty flags!public! !
!RioRow categoriesFor: #clearDbDirty!dirty flags!public! !
!RioRow categoriesFor: #clearUiDirty!dirty flags!public! !
!RioRow categoriesFor: #columnValues:!accessing!public! !
!RioRow categoriesFor: #displayOn:!ANSI protocols-Object!public! !
!RioRow categoriesFor: #doesNotUnderstand:!ANSI protocols-Object!public! !
!RioRow categoriesFor: #existsIn:in:usingKeys:!accessing!public! !
!RioRow categoriesFor: #initialize!initializing!public! !
!RioRow categoriesFor: #isColumnDbDirty:!dirty flags!public! !
!RioRow categoriesFor: #isColumnUiDirty:!dirty flags!public! !
!RioRow categoriesFor: #isDbDirty!dirty flags!public! !
!RioRow categoriesFor: #isUiDirty!dirty flags!public! !
!RioRow categoriesFor: #nCols!accessing!public! !
!RioRow categoriesFor: #printOn:!ANSI protocols-Object!public! !
!RioRow categoriesFor: #rowStatus:!accessing!public! !
!RioRow categoriesFor: #setColumnDbDirty:!dirty flags!public! !
!RioRow categoriesFor: #setColumnUiDirty:!dirty flags!public! !
!RioRow categoriesFor: #setDbDirty!dirty flags!public! !
!RioRow categoriesFor: #setUiDirty!dirty flags!public! !
!RioRow categoriesFor: #status!accessing!public! !

RioAutoStatement guid: (GUID fromString: '{6BBB8FAC-559A-11D3-8269-00001D19F5C2}')!
RioAutoStatement comment: ''!
!RioAutoStatement categoriesForClass!Rio! !
!RioAutoStatement methodsFor!

bindRow
	"Private - bind columns in row to parameter markers in the generated statement."

	| rowsetHeader columnHeader skip skipCount |

	rowsetHeader := row rowsetHeader.
	skipCount := 0.

	1 to: rowsetHeader nCols do:
		[ :i |
		columnHeader := rowsetHeader at: i.
		skip := false.
		self ignoreColumns notNil ifTrue:
			[ self ignoreColumns do: [ :aColumnName |
			columnHeader name = aColumnName ifTrue:
				[ skip := true.
				skipCount := skipCount + 1 ] ] ].

		skip ifFalse:
			[ self setParameter: i - skipCount
				value: (row at: i)
				sqlType: columnHeader sqlType ] ]!

generateSql
	"Private - generate an appropriate SQL statement.  This method must be
	 implemented by a subclass."

	^self subclassResponsibility!

ignoreColumns
	^ignoreColumns!

ignoreColumns: anArray
	ignoreColumns := anArray!

prepare
	"Generate the SQL statement to be used and prepare it."

	self prepared ifFalse:
		[ self assert: [ self row notNil ].
		self generateSql.
		super prepare.
		self bindRow ]!

row
	^row!

row: aRow
	row := aRow.
	self prepared
		ifTrue: [ self bindRow ].!

tableName
	^tableName!

tableName: aString
	"Set the name of the table this statement will operate opon."

	tableName := aString! !
!RioAutoStatement categoriesFor: #bindRow!private!private helpers! !
!RioAutoStatement categoriesFor: #generateSql!private!private helpers! !
!RioAutoStatement categoriesFor: #ignoreColumns!accessing!public! !
!RioAutoStatement categoriesFor: #ignoreColumns:!accessing!public! !
!RioAutoStatement categoriesFor: #prepare!execution control!public! !
!RioAutoStatement categoriesFor: #row!accessing!public! !
!RioAutoStatement categoriesFor: #row:!accessing!public! !
!RioAutoStatement categoriesFor: #tableName!accessing!public! !
!RioAutoStatement categoriesFor: #tableName:!accessing!public! !

RioCursor guid: (GUID fromString: '{6BBB8FAD-559A-11D3-8269-00001D19F5C2}')!
RioCursor comment: 'lastBindType - records the size of the last buffer bound to this statement.  Doing this prevents us having to
		set the SQL_BIND_TYPE statement option multiple times, which turns out to be a
		real performance killer.'!
!RioCursor categoriesForClass!Rio! !
!RioCursor methodsFor!

afterHandleAllocated
	"Private - perform any processing required after the statement handle
	 is allocated."

	self setCursorType!

atEnd
	^self retcode = RioConstants current sqlNoDataFound!

bindRowset: aRowset
	"Bind columns in aRowset to the columns returned by this
	 statement.  Binding is based on column name.  Set the
	 SqlRowsetSize option for this statement."
	| rowsetRows columnHeader |

	self assert: [ self rowsetHeader == aRowset rowsetBuffer rowsetHeader ].

	(self rowsetHeader at: 1) offset isNil ifTrue: [ self rowsetHeader calculateOffsets ].

	rowsetRows := aRowset nRows.
	rowsetRows notNil
		ifTrue: [self integerOption: RioConstants current sqlRowsetSize value: rowsetRows.
			   (self lastBindType = aRowset rowsetBuffer rowsetHeader bufferSize) not
				   ifTrue: [ self integerOption: RioConstants current sqlBindType
					 		value: aRowset rowsetBuffer rowsetHeader bufferSize.
						self lastBindType: aRowset rowsetBuffer rowsetHeader bufferSize ] ]
		ifFalse: [RioFatalError signal: 'RioStatement>>bindRowset - rowsetRows = nil'].

	1 to: self rowsetHeader nCols do: [ :i |
		columnHeader := self rowsetHeader at: i.

		self check: (RioDatabase dll
					sqlBindCol: self hstmt asParameter
					column: i asParameter
					cType: columnHeader cType asParameter
					buffer: (aRowset rowsetBuffer data address + columnHeader offset) asParameter
					bufferLen: columnHeader dataSize asParameter
					bytesAvailable: (aRowset rowsetBuffer data address +
									columnHeader indicatorOffset) asParameter) ].
	self currentRowset: aRowset!

close
	"Close the cursor, discarding pending results."

	self check: (RioDatabase dll
				sqlFreeStmt: self hstmt asParameter
				option: RioConstants current sqlClose).
	self
		executed: false;
		currentRowset: nil.
	^self retcode!

createRow
	"Create a new row which can hold values retrieved by this cursor."

	^RioRow new rowsetHeader: self rowsetHeader
!

createRowset
	"Create a rowset for this statement."

	^RioRowset new rowsetHeader: self rowsetHeader; yourself.!

createRowset: anInteger
	"Create a rowset which can contain 'anInteger' rows."

	| aRowset |

	aRowset := self createRowset nRows: anInteger.
	aRowset rowsetBuffer nRows: anInteger.
	^aRowset!

createRowsetHeader
	"Private - create a RioRowsetHeader for this cursor."
	| aRowsetHeader numResultCols maxColumnNameLen name nameBytesAvailable
	  sqlType precision scale nullable header currentOffset |

	numResultCols := self numResultCols.

	aRowsetHeader := RioRowsetHeader new.

	maxColumnNameLen := self database integerInfo: RioConstants current sqlMaxColumnNameLen.
	name := String new: maxColumnNameLen + 1.
	nameBytesAvailable := RioSwordBuffer new value: 0.
	sqlType := RioSwordBuffer new value: 0.
	precision := RioDwordBuffer new value: 0.
	scale := RioSwordBuffer new value: 0.
	nullable := RioSwordBuffer new value: 0.
	currentOffset := 0.

	1 to: (numResultCols asInteger) do:
		[ :i |
		self check: (RioDatabase dll
					sqlDescribeCol: self hstmt asParameter
					columnNumber: i asParameter
					columnName: name asParameter
					columnNameSize: name size
					columnNameBytesAvailable: nameBytesAvailable asParameter
					sqlType: sqlType asParameter
					precision: precision asParameter
					scale: scale asParameter
					nullable: nullable asParameter).
		header := RioColumnHeader
					headerForName: name trimNulls
					sqlType: sqlType
					precision: precision
					scale: scale
					nullable: nullable
					blobSize: self blobSize
					index: i.
		aRowsetHeader addHeader: header at: i ].
	^aRowsetHeader!

currentRowset
	currentRowset isNil ifTrue: [ self bindRowset: (self createRowset: self rowsetSize) ].
	^currentRowset!

currentRowset: aRowset
	currentRowset := aRowset!

cursorName
	"Answer the cursor name associated with this statement."

	| aString cbCursorName |

	aString := String new: RioConstants current sqlMaxCursorNameLen + 1.
	cbCursorName := RioSdwordBuffer new value: 0.

	self check: (RioDatabase dll
				sqlGetCursorName: self hstmt asParameter
				cursorName: aString asParameter
				cursorNameSize: aString size asParameter
				cursorNameBytesAvailable: cbCursorName asParameter).
	^aString trimNulls trimBlanks!

cursorName: aString
	"Set the cursor name associated with this statement."

	self check: (RioDatabase dll
				sqlSetCursorName: self hstmt asParameter
				cursorName: aString asParameter
				cursorNameLen: aString size)!

defaultRowsetSize
	^25!

drop
	super drop.
	self currentRowset: nil.!

fetch: aFetchType rowNumber: anInteger
	"Private - fetch rows from the statement into the current rowset
	 bound to this statement."

	| fetchType rowNumber rowsFetched |

	fetchType := RioWordBuffer new value: aFetchType asInteger.
	rowNumber := RioSdwordBuffer new value: anInteger.
	rowsFetched := RioDwordBuffer new.
	self execute.
	self check: (RioDatabase dll
				sqlExtendedFetch: self hstmt asParameter
				fetchType: fetchType asParameter
				rowNumber: rowNumber asParameter
				rowsFetched: rowsFetched asParameter
				rowStatusArray: self currentRowset rowsetBuffer rowStatusArray asParameter).
	self currentRowset setDirtyFlagsAfterFetch.
	^self currentRowset!

fetchAbsolute: aRowNumber
		"Fetch a rowset whose first row is row aRowNumber within
		 the current result set.  This message assumes that the
		 statement has already been executed and that all columns are
		 bound as desired."

	^self fetch: RioConstants current sqlFetchAbsolute rowNumber: aRowNumber asInteger!

fetchFirst
		"Fetch the first result set for this statement.  This message
		 assumes that the statement has already been executed and
		 that all columns are bound as desired."

	^self fetch: RioConstants current sqlFetchFirst rowNumber: 1!

fetchLast
		"Fetch the last result set for this statement.  This message
		 assumes that the statement has already been executed and
		 that all columns are bound as desired."

	^self fetch: RioConstants current sqlFetchLast rowNumber: 1!

fetchPrior
		"Fetch the first previous set for this statement.  This message
		 assumes that the statement has already been executed and
		 that all columns are bound as desired."

	^self fetch: RioConstants current sqlFetchPrior rowNumber: 1!

fetchRelative: anInteger
		"Fetch a rowset whose first row is anInteger rows from the
		 first row of the current rowset.  anInteger can be either positive,
		 negative, or zero.  This message assumes that the statement
		 has already been executed and that all columns are bound as
		 desired."

	^self fetch: RioConstants current sqlFetchRelative rowNumber: anInteger asInteger!

initialize
	super initialize.
	self rowsetHeader: nil.
	self rowsetSize: self defaultRowsetSize!

lastBindType
	^lastBindType!

lastBindType: aNumber
	lastBindType := aNumber!

next
	"Fetch the next result set for this statement.  This message
	 assumes that the statement has already been executed and
	 that all columns are bound as desired."

	^self fetch: RioConstants current sqlFetchNext rowNumber: 1!

numResultCols
		"Answer the number of result columns returned by this
		 statement as an Integer"
	| numResultCols |

	numResultCols := RioSdwordBuffer new value: 0.
	self prepare.
	self check: (RioDatabase dll
				sqlNumResultCols: self hstmt asParameter
				numResultCols: numResultCols asParameter).
	^numResultCols asInteger!

rowsetHeader
	rowsetHeader isNil ifTrue: [ rowsetHeader := self createRowsetHeader ].
	^rowsetHeader!

rowsetHeader: aRowsetHeader
	rowsetHeader := aRowsetHeader!

rowsetSize
	^rowsetSize!

rowsetSize: anInteger
	rowsetSize := anInteger!

setConcurrency: anInteger
	"Set the cursor concurrency.  anInteger should be one of
	 SqlConcurReadOnly, SqlConcurLock, SqlConcurRowver, or
	 SqlConcurValues."

	self assert: 
		[ (anInteger = RioConstants current sqlConcurReadOnly) |
		  (anInteger = RioConstants current sqlConcurLock) |
		  (anInteger = RioConstants current sqlConcurRowver) |
		  (anInteger = RioConstants current sqlConcurValues) ].

	self setOption: RioConstants current sqlConcurrency value: anInteger!

setCursorType
	"Private - By default do nothing"!

update: anInteger
	"Update the current rowset fetched by this statement.  anInteger
	specifies the row within the rowset which should be updated.  If
	anInteger = 0 all rows in the rowset are updated."

	^self check: (RioDatabase dll
				sqlSetPos: self hstmt asParameter
				row: anInteger asParameter
				option: RioConstants current sqlUpdate asParameter
				lockType: RioConstants current sqlLockNoChange asParameter)!

updateAll
	"Update all rows in the rowset"
	^self update: 0!

upToEnd
	"Answer an OrderedCollection containing all rows produced by this
	 cursor.  The cursor is assumed to have all parameters bound and to
	 have been executed; i.e. to be in a fetchable state."

	| col rowset |

	col := OrderedCollection new.
	rowset := self createRowset: self rowsetSize.
	self bindRowset: rowset.
	self next.

	[(self retcode = RioConstants current sqlSuccess) |
			(self retcode = RioConstants current sqlSuccessWithInfo)] whileTrue: [
		rowset do: [ :aRow | col add: aRow ].
		rowset := self createRowset: self rowsetSize.
		self bindRowset: rowset.
		self next ].

	^col
! !
!RioCursor categoriesFor: #afterHandleAllocated!private!private helpers! !
!RioCursor categoriesFor: #atEnd!public!streaming! !
!RioCursor categoriesFor: #bindRowset:!operations!public! !
!RioCursor categoriesFor: #close!execution control!public! !
!RioCursor categoriesFor: #createRow!operations!public! !
!RioCursor categoriesFor: #createRowset!operations!public! !
!RioCursor categoriesFor: #createRowset:!operations!public! !
!RioCursor categoriesFor: #createRowsetHeader!private!private helpers! !
!RioCursor categoriesFor: #currentRowset!accessing!public! !
!RioCursor categoriesFor: #currentRowset:!accessing!public! !
!RioCursor categoriesFor: #cursorName!accessing!public! !
!RioCursor categoriesFor: #cursorName:!accessing!public! !
!RioCursor categoriesFor: #defaultRowsetSize!accessing!private! !
!RioCursor categoriesFor: #drop!execution control!public! !
!RioCursor categoriesFor: #fetch:rowNumber:!private!private helpers! !
!RioCursor categoriesFor: #fetchAbsolute:!execution control!public! !
!RioCursor categoriesFor: #fetchFirst!execution control!public! !
!RioCursor categoriesFor: #fetchLast!execution control!public! !
!RioCursor categoriesFor: #fetchPrior!execution control!public! !
!RioCursor categoriesFor: #fetchRelative:!execution control!public! !
!RioCursor categoriesFor: #initialize!initializing!public! !
!RioCursor categoriesFor: #lastBindType!accessing!private! !
!RioCursor categoriesFor: #lastBindType:!accessing!private! !
!RioCursor categoriesFor: #next!public!streaming! !
!RioCursor categoriesFor: #numResultCols!information!public! !
!RioCursor categoriesFor: #rowsetHeader!accessing!public! !
!RioCursor categoriesFor: #rowsetHeader:!accessing!public! !
!RioCursor categoriesFor: #rowsetSize!accessing!public! !
!RioCursor categoriesFor: #rowsetSize:!accessing!public! !
!RioCursor categoriesFor: #setConcurrency:!execution control!public! !
!RioCursor categoriesFor: #setCursorType!execution control!private! !
!RioCursor categoriesFor: #update:!execution control!public! !
!RioCursor categoriesFor: #updateAll!execution control!public! !
!RioCursor categoriesFor: #upToEnd!public!streaming! !

!RioCursor class methodsFor!

on: anRioDatabase selectForColumns: columnNameArray fromTable: tableNameString withKeys: keyNameArray
	"Generate an SQL statement to retrieve the columns named in columnArray
	 from the given table using the columns in keyArray as keys.  All key
	 comparisons are assumed to be equality."

	| g whereClause |

	(keyNameArray notNil and: [ keyNameArray size > 0 ]) ifTrue:
		[ whereClause := keyNameArray inject: String new into: [ :str :keyName | str, keyName, '=? and ' ].
		whereClause := whereClause leftString: whereClause size - 5 ].		"Strip the last 'and'"

	(g := RioSqlGenerator new)
		tableName: tableNameString;
		columnNames: columnNameArray;
		whereClause: whereClause.

	^self on: anRioDatabase sql: g generateSelectStatement! !
!RioCursor class categoriesFor: #on:selectForColumns:fromTable:withKeys:!instance creation!public! !

RioAutoInsertStatement guid: (GUID fromString: '{6BBB8FAE-559A-11D3-8269-00001D19F5C2}')!
RioAutoInsertStatement comment: ''!
!RioAutoInsertStatement categoriesForClass!Rio! !
!RioAutoInsertStatement methodsFor!

generateSql
	"Private - generate an INSERT statement appropriate for inserting row into
	 the database."

	| aString aRowsetHeader aColumnHeader parameterMarkers skip s p |

	super sql isNil ifTrue:
		[ aRowsetHeader := self row rowsetHeader.

		aString := String new.
		s := aString writeStream.

		s
			nextPutAll: 'INSERT INTO ';
			nextPutAll: self tableName;
			nextPutAll: ' ('.

		parameterMarkers := String new.
		p := parameterMarkers writeStream.

		1 to: aRowsetHeader nCols do:
			[ :i |
			aColumnHeader := aRowsetHeader at: i.
			skip := false.
			self ignoreColumns isNil ifFalse:
				[ (self ignoreColumns includes: aColumnHeader name) ifTrue: [ skip := true ] ].

			skip ifFalse:
				[ s nextPutAll: aColumnHeader name.
				p nextPut: $?.
				i < aRowsetHeader nCols ifTrue:
					[ s nextPut: $,.
					p nextPut: $, ] ] ].

		p close.

		s
			nextPutAll: ') VALUES (';
			nextPutAll: p contents;
			nextPut: $);
			close.

		self sql: s contents ].

	^super sql!

insertAll: aCollection
	"aCollection is assumed to contain rows to be inserted into the table for which this statement has been created."

	aCollection do:
		[ :aRow |
		self row: aRow.
		self execute ]
!

sql
	"Answer the generated SQL for this statement."
	^self generateSql! !
!RioAutoInsertStatement categoriesFor: #generateSql!private!private helpers! !
!RioAutoInsertStatement categoriesFor: #insertAll:!operations!public! !
!RioAutoInsertStatement categoriesFor: #sql!accessing!public! !

RioAutoUpdateStatement guid: (GUID fromString: '{6BBB8FAF-559A-11D3-8269-00001D19F5C2}')!
RioAutoUpdateStatement comment: ''!
!RioAutoUpdateStatement categoriesForClass!Rio! !
!RioAutoUpdateStatement methodsFor!

dataParameterCount
	^dataParameterCount!

dataParameterCount: anInteger
	"Private - set the data parameter count"
	dataParameterCount := anInteger!

generateSql
	"Private - generate an UPDATE statement appropriate for updating the
	 specified row."

	| sqlString aRowsetHeader aColumnHeader skip s |

	super sql isNil ifTrue:
		[ aRowsetHeader := self row rowsetHeader.

		sqlString := String new.
		s := sqlString writeStream.

		s
			nextPutAll: 'UPDATE ';
			nextPutAll: self tableName;
			nextPutAll: ' SET '.

		1 to: aRowsetHeader nCols do:
			[ :i |
			aColumnHeader := aRowsetHeader at: i.
			skip := false.
			self ignoreColumns isNil ifFalse:
				[ (self ignoreColumns includes: aColumnHeader name) ifTrue: [ skip := true ] ].
				skip ifFalse:
					[ s
						nextPutAll: aColumnHeader name;
						nextPutAll: '=?'.
					self dataParameterCount: self dataParameterCount + 1.
					i < aRowsetHeader nCols ifTrue: [ s nextPut: $, ] ] ].

		s
			nextPutAll: ' WHERE ';
			nextPutAll: self whereClause;
			close.

		self sql: s contents ].

	^super sql!

initialize
	super initialize.
	self dataParameterCount: 0!

setWhereClauseParameter: anInteger value: anObject sqlType: anSqlType
	self generateSql.		"Make sure SQL statement has been generated"
	self setParameter: (anInteger + self dataParameterCount)
		value: anObject
		sqlType: anSqlType!

sql
	"Answer the generated SQL for this statement."
	^self generateSql!

whereClause
	^whereClause!

whereClause: aString
	whereClause := aString! !
!RioAutoUpdateStatement categoriesFor: #dataParameterCount!accessing!public! !
!RioAutoUpdateStatement categoriesFor: #dataParameterCount:!private!private helpers! !
!RioAutoUpdateStatement categoriesFor: #generateSql!private!private helpers! !
!RioAutoUpdateStatement categoriesFor: #initialize!initializing!public! !
!RioAutoUpdateStatement categoriesFor: #setWhereClauseParameter:value:sqlType:!accessing!public! !
!RioAutoUpdateStatement categoriesFor: #sql!accessing!public! !
!RioAutoUpdateStatement categoriesFor: #whereClause!accessing!public! !
!RioAutoUpdateStatement categoriesFor: #whereClause:!accessing!public! !

RioDynamicCursor guid: (GUID fromString: '{6BBB8FB0-559A-11D3-8269-00001D19F5C2}')!
RioDynamicCursor comment: 'A cursor which saves and uses the keys for the rows in the rowset.'!
!RioDynamicCursor categoriesForClass!Rio! !
!RioDynamicCursor methodsFor!

setCursorType
	self setOption: RioConstants current sqlCursorType value: RioConstants current sqlCursorDynamic! !
!RioDynamicCursor categoriesFor: #setCursorType!execution control!private! !

RioForwardCursor guid: (GUID fromString: '{6BBB8FB1-559A-11D3-8269-00001D19F5C2}')!
RioForwardCursor comment: 'A cursor which can only scroll forward.'!
!RioForwardCursor categoriesForClass!Rio! !
!RioForwardCursor methodsFor!

setCursorType
	self setOption: RioConstants current sqlCursorType value: RioConstants current sqlCursorForwardOnly! !
!RioForwardCursor categoriesFor: #setCursorType!execution control!private! !

RioKeysetCursor guid: (GUID fromString: '{6BBB8FB2-559A-11D3-8269-00001D19F5C2}')!
RioKeysetCursor comment: 'A cursor that saves and uses the keys for the number of rows specified in the SQL_KEYSET_SIZE statement option.  Note that the keyset size must be greater than the rowset size or an error will occur when the cursor is fetched.'!
!RioKeysetCursor categoriesForClass!Rio! !
!RioKeysetCursor methodsFor!

keysetSize
	^self integerOption: RioConstants current sqlKeysetSize!

keysetSize: anInteger
	^self integerOption: RioConstants current sqlKeysetSize value: anInteger!

setCursorType
	self setOption: RioConstants current sqlCursorType value: RioConstants current sqlCursorKeysetDriven! !
!RioKeysetCursor categoriesFor: #keysetSize!accessing!public! !
!RioKeysetCursor categoriesFor: #keysetSize:!accessing!public! !
!RioKeysetCursor categoriesFor: #setCursorType!execution control!private! !

RioStaticCursor guid: (GUID fromString: '{6BBB8FB3-559A-11D3-8269-00001D19F5C2}')!
RioStaticCursor comment: 'A cursor whose data is static.'!
!RioStaticCursor categoriesForClass!Rio! !
!RioStaticCursor methodsFor!

setCursorType
	self setOption: RioConstants current sqlCursorType value: RioConstants current sqlCursorStatic! !
!RioStaticCursor categoriesFor: #setCursorType!execution control!private! !

RioMetadataCursor guid: (GUID fromString: '{67825ADB-6E1E-41CB-B4E8-F810E741DF3E}')!
RioMetadataCursor comment: ''!
!RioMetadataCursor categoriesForClass!Unclassified! !
!RioMetadataCursor methodsFor!

columnsInCatalog: catalogName schema: schemaName table: tableName columnName: columnName
	"Answer a collection of rows containing information returned by the SQLColumns function."

	self check: (RioDatabase dll
				sqlColumns: self hstmt asParameter
				catalogName: catalogName asParameter
				catalogLen: catalogName size asParameter
				schemaName: schemaName asParameter
				schemaNameLen: schemaName size asParameter
				tableName: tableName asParameter
				tableNameLen: tableName size asParameter
				columnName: columnName asParameter
				columnNameLen: columnName size asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd!

foreignKeysWithPkCatalog: pkCatalogName pkSchema: pkSchemaName pkTable: pkTableName fkCatalog: fkCatalogName fkSchema: fkSchemaName fkTable: fkTableName
	"Answer a collection of rows containing information returned by the SQLForiegnKeys function."

	self check: (RioDatabase dll
				sqlForeignKeys: self hstmt asParameter
				pkCatalogName: pkCatalogName asParameter
				pkCatalogNameLen: pkCatalogName size asParameter
				pkSchemaName: pkSchemaName asParameter
				pkSchemaNameLen: pkSchemaName size asParameter
				pkTableName: pkTableName asParameter
				pkTableNameLen: pkTableName size asParameter
				fkCatalogName: fkCatalogName asParameter
				fkCatalogNameLen: fkCatalogName size asParameter
				fkSchemaName: fkSchemaName asParameter
				fkSchemaNameLen: fkSchemaName size asParameter
				fkTableName: fkTableName asParameter
				fkTableNameLen: fkTableName size asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd!

primaryKeysInCatalog: catalogName schema: schemaName table: tableName
	"Answer a collection of rows containing information returned by the SQLPrimaryKeys function."

	self check: (RioDatabase dll
				sqlPrimaryKeys: self hstmt asParameter
				catalogName: catalogName asParameter
				catalogNameLen: catalogName size asParameter
				schemaName: schemaName asParameter
				schemaNameLen: schemaName size asParameter
				tableName: tableName asParameter
				tableNameLen: tableName size asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd!

procedureColumnsInCatalog: catalogName schema: schemaName procedure: procedureName column: columnName
	"Answer a collection of rows containing information returned by the SQLProcedureColumns function."

	self check: (RioDatabase dll
				sqlProcedureColumns: self hstmt asParameter
				catalogName: catalogName asParameter
				catalogNameLen: catalogName size asParameter
				schemaName: schemaName asParameter
				schemaNameLen: schemaName size asParameter
				procName: procedureName asParameter
				procNameLen: procedureName size asParameter
				columnName: columnName asParameter
				columnNameLen: columnName size asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd!

specialColumnsOfType: columnType catalog: catalogName schema: schemaName table: tableName scope: scope nullable: nullable
	"Answer a collection of rows containing information returned by the SQLSpecialColumns function."

	self check: (RioDatabase dll
				sqlSpecialColumns: self hstmt asParameter
				colType: columnType asParameter
				catalogName: catalogName asParameter
				catalogNameLen: catalogName size asParameter
				schemaName: schemaName asParameter
				schemaNameLen: schemaName size asParameter
				tableName: tableName asParameter
				tableNameLen: tableName size asParameter
				scope: scope asParameter
				nullable: nullable asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd!

statisticsForCatalog: catalogName schema: schemaName table: tableName unique: unique accuracy: accuracy
	"Answer a collection of rows containing information returned by the SQLStatistics function."

	self check: (RioDatabase dll
				sqlStatistics: self hstmt asParameter
				catalogName: catalogName asParameter
				catalogNameLen: catalogName size asParameter
				schemaName: schemaName asParameter
				schemaNameLen: schemaName size asParameter
				tableName: tableName asParameter
				tableNameLen: tableName size asParameter
				unique: unique asParameter
				accuracy: accuracy asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd!

tablePrivilegesForCatalog: catalogName schema: schemaName table: tableName unique: unique accuracy: accuracy
	"Answer a collection of rows containing information returned by the SQLTablePrivileges function."

	self check: (RioDatabase dll
				sqlTablePrivileges: self hstmt asParameter
				catalogName: catalogName asParameter
				catalogNameLen: catalogName size asParameter
				schemaName: schemaName asParameter
				schemaNameLen: schemaName size asParameter
				tableName: tableName asParameter
				tableNameLen: tableName size asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd!

tablesInCatalog: catalogName schema: schemaName table: tableName type: tableType
	"Answer a collection of rows containing information returned by the SQLTables function."

	self check: (RioDatabase dll
				sqlTables: self hstmt asParameter
				catalogName: catalogName asParameter
				catalogLen: catalogName size asParameter
				schemaName: schemaName asParameter
				schemaNameLen: schemaName size asParameter
				tableName: tableName asParameter
				tableNameLen: tableName size asParameter
				tableType: tableType asParameter
				tableTypeLen: tableType size asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd!

typeInfoFor: anInteger
	"Return a collection of rows describing the data type specified by anInteger."

	self check: (RioDatabase dll
				sqlGetTypeInfo: self hstmt asParameter
				forType: anInteger asParameter).
	self
		prepared: true;
		executed: true.
	^self upToEnd! !
!RioMetadataCursor categoriesFor: #columnsInCatalog:schema:table:columnName:!information!public! !
!RioMetadataCursor categoriesFor: #foreignKeysWithPkCatalog:pkSchema:pkTable:fkCatalog:fkSchema:fkTable:!information!public! !
!RioMetadataCursor categoriesFor: #primaryKeysInCatalog:schema:table:!information!public! !
!RioMetadataCursor categoriesFor: #procedureColumnsInCatalog:schema:procedure:column:!information!public! !
!RioMetadataCursor categoriesFor: #specialColumnsOfType:catalog:schema:table:scope:nullable:!information!public! !
!RioMetadataCursor categoriesFor: #statisticsForCatalog:schema:table:unique:accuracy:!information!public! !
!RioMetadataCursor categoriesFor: #tablePrivilegesForCatalog:schema:table:unique:accuracy:!information!public! !
!RioMetadataCursor categoriesFor: #tablesInCatalog:schema:table:type:!information!public! !
!RioMetadataCursor categoriesFor: #typeInfoFor:!information!public! !

RioReadOnlyTransaction guid: (GUID fromString: '{6BBB8FB4-559A-11D3-8269-00001D19F5C2}')!
RioReadOnlyTransaction comment: ''!
!RioReadOnlyTransaction categoriesForClass!No category! !
!RioReadOnlyTransaction methodsFor!

accessMode
	^RioConstants current sqlModeReadOnly!

isCompatibleWith: aTransaction
	^true!

isReadOnly
	^true!

successAction
	self rollback! !
!RioReadOnlyTransaction categoriesFor: #accessMode!accessing!public! !
!RioReadOnlyTransaction categoriesFor: #isCompatibleWith:!public!testing! !
!RioReadOnlyTransaction categoriesFor: #isReadOnly!public!testing! !
!RioReadOnlyTransaction categoriesFor: #successAction!public!transactions! !

RioReadWriteTransaction guid: (GUID fromString: '{6BBB8FB5-559A-11D3-8269-00001D19F5C2}')!
RioReadWriteTransaction comment: ''!
!RioReadWriteTransaction categoriesForClass!No category! !
!RioReadWriteTransaction methodsFor!

accessMode
	^RioConstants current sqlModeReadWrite!

isCompatibleWith: aTransaction
	^aTransaction isReadOnly not!

isReadOnly
	^false!

successAction
	self commit! !
!RioReadWriteTransaction categoriesFor: #accessMode!accessing!public! !
!RioReadWriteTransaction categoriesFor: #isCompatibleWith:!public!testing! !
!RioReadWriteTransaction categoriesFor: #isReadOnly!public!testing! !
!RioReadWriteTransaction categoriesFor: #successAction!public!transactions! !

RioTestTransaction guid: (GUID fromString: '{6BBB8FB6-559A-11D3-8269-00001D19F5C2}')!
RioTestTransaction comment: ''!
!RioTestTransaction categoriesForClass!No category! !
!RioTestTransaction methodsFor!

successAction
	self rollback! !
!RioTestTransaction categoriesFor: #successAction!public!transactions! !

"Binary Globals"!

"Resources"!

