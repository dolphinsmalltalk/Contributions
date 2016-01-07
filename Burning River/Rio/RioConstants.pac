| package |
package := Package name: 'RioConstants'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicScriptAt: #postinstall put: 'RioConstants install.'.
package basicScriptAt: #preuninstall put: 'RioConstants uninstall.'.

package classNames
	add: #RioConstants;
	add: #RioConstantsClassGenerator;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\ConstantsClassGenerator\ConstantsClassGenerator';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #RioConstants
	instanceVariableNames: ''
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ConstantsClassGenerator subclass: #RioConstantsClassGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

RioConstants guid: (GUID fromString: '{6BBB8F91-559A-11D3-8269-00001D19F5C2}')!
RioConstants comment: 'This class provides definitions of constants used by the Rio package.'!
!RioConstants categoriesForClass!Rio! !
!RioConstants methodsFor!

odbcver
	^16r0300!

sqlAccessibleProcedures
	^20!

sqlAccessibleTables
	^19!

sqlAccessMode
	^101!

sqlActiveConnections
	^0!

sqlActiveEnvironments
	^116!

sqlActiveStatements
	^1!

sqlAdAddConstraintDeferrable
	^16r00000080!

sqlAdAddConstraintInitiallyDeferred
	^16r00000020!

sqlAdAddConstraintInitiallyImmediate
	^16r00000040!

sqlAdAddConstraintNonDeferrable
	^16r00000100!

sqlAdAddDomainConstraint
	^16r00000002!

sqlAdAddDomainDefault
	^16r00000008!

sqlAdConstraintNameDefinition
	^16r00000001!

sqlAdd
	^4!

sqlAdDropDomainConstraint
	^16r00000004!

sqlAdDropDomainDefault
	^16r00000010!

sqlAfAll
	^self sqlAfAvg | self sqlAfCount | self sqlAfMax | self sqlAfMin | self sqlAfSum | self sqlAfDistinct!

sqlAfAvg
	^16r00000001!

sqlAfCount
	^16r00000002!

sqlAfDistinct
	^16r00000020!

sqlAfMax
	^16r00000004!

sqlAfMin
	^16r00000008!

sqlAfSum
	^16r00000010!

sqlAggregateFunctions
	^169!

sqlAllCatalogs
	^'%'!

sqlAllExceptLike
	^2!

sqlAllSchemas
	^'%'!

sqlAllTableTypes
	^'%'!

sqlAllTypes
	^0!

sqlAlterDomain
	^117!

sqlAlterTable
	^86!

sqlAmConnection
	^1!

sqlAmNone
	^0!

sqlAmStatement
	^2!

sqlApiAllFunctions
	^0!

sqlApiLoadbyordinal
	^199!

sqlApiOdbc3AllFunctions
	^999!

sqlApiOdbc3AllFunctionsSize
	^250!

sqlApiSqlallocconnect
	^1!

sqlApiSqlallocenv
	^2!

sqlApiSqlallochandle
	^1001!

sqlApiSqlallochandlestd
	^73!

sqlApiSqlallocstmt
	^3!

sqlApiSqlbindcol
	^4!

sqlApiSqlbindparam
	^1002!

sqlApiSqlbindparameter
	^72!

sqlApiSqlbrowseconnect
	^55!

sqlApiSqlbulkoperations
	^24!

sqlApiSqlcancel
	^5!

sqlApiSqlclosecursor
	^1003!

sqlApiSqlcolattribute
	^6!

sqlApiSqlcolattributes
	^6!

sqlApiSqlcolumnprivileges
	^56!

sqlApiSqlcolumns
	^40!

sqlApiSqlconnect
	^7!

sqlApiSqlcopydesc
	^1004!

sqlApiSqldatasources
	^57!

sqlApiSqldescribecol
	^8!

sqlApiSqldescribeparam
	^58!

sqlApiSqldisconnect
	^9!

sqlApiSqldriverconnect
	^41!

sqlApiSqldrivers
	^71!

sqlApiSqlendtran
	^1005!

sqlApiSqlerror
	^10!

sqlApiSqlexecdirect
	^11!

sqlApiSqlexecute
	^12!

sqlApiSqlextendedfetch
	^59!

sqlApiSqlfetch
	^13!

sqlApiSqlfetchscroll
	^1021!

sqlApiSqlforeignkeys
	^60!

sqlApiSqlfreeconnect
	^14!

sqlApiSqlfreeenv
	^15!

sqlApiSqlfreehandle
	^1006!

sqlApiSqlfreestmt
	^16!

sqlApiSqlgetconnectattr
	^1007!

sqlApiSqlgetconnectoption
	^42!

sqlApiSqlgetcursorname
	^17!

sqlApiSqlgetdata
	^43!

sqlApiSqlgetdescfield
	^1008!

sqlApiSqlgetdescrec
	^1009!

sqlApiSqlgetdiagfield
	^1010!

sqlApiSqlgetdiagrec
	^1011!

sqlApiSqlgetenvattr
	^1012!

sqlApiSqlgetfunctions
	^44!

sqlApiSqlgetinfo
	^45!

sqlApiSqlgetstmtattr
	^1014!

sqlApiSqlgetstmtoption
	^46!

sqlApiSqlgettypeinfo
	^47!

sqlApiSqlmoreresults
	^61!

sqlApiSqlnativesql
	^62!

sqlApiSqlnumparams
	^63!

sqlApiSqlnumresultcols
	^18!

sqlApiSqlparamdata
	^48!

sqlApiSqlparamoptions
	^64!

sqlApiSqlprepare
	^19!

sqlApiSqlprimarykeys
	^65!

sqlApiSqlprocedurecolumns
	^66!

sqlApiSqlprocedures
	^67!

sqlApiSqlputdata
	^49!

sqlApiSqlrowcount
	^20!

sqlApiSqlsetconnectattr
	^1016!

sqlApiSqlsetconnectoption
	^50!

sqlApiSqlsetcursorname
	^21!

sqlApiSqlsetdescfield
	^1017!

sqlApiSqlsetdescrec
	^1018!

sqlApiSqlsetenvattr
	^1019!

sqlApiSqlsetparam
	^22!

sqlApiSqlsetpos
	^68!

sqlApiSqlsetscrolloptions
	^69!

sqlApiSqlsetstmtattr
	^1020!

sqlApiSqlsetstmtoption
	^51!

sqlApiSqlspecialcolumns
	^52!

sqlApiSqlstatistics
	^53!

sqlApiSqltableprivileges
	^70!

sqlApiSqltables
	^54!

sqlApiSqltransact
	^23!

sqlArdType
	^(-99)!

sqlAsyncEnable
	^4!

sqlAsyncEnableDefault
	^self sqlAsyncEnableOff!

sqlAsyncEnableOff
	^0!

sqlAsyncEnableOn
	^1!

sqlAsyncMode
	^10021!

sqlAtAddColumn
	^16r00000001!

sqlAtAddColumnCollation
	^16r00000080!

sqlAtAddColumnDefault
	^16r00000040!

sqlAtAddColumnSingle
	^16r00000020!

sqlAtAddConstraint
	^16r00000008!

sqlAtAddTableConstraint
	^16r00001000!

sqlAtConstraintDeferrable
	^16r00040000!

sqlAtConstraintInitiallyDeferred
	^16r00010000!

sqlAtConstraintInitiallyImmediate
	^16r00020000!

sqlAtConstraintNameDefinition
	^16r00008000!

sqlAtConstraintNonDeferrable
	^16r00080000!

sqlAtDropColumn
	^16r00000002!

sqlAtDropColumnCascade
	^16r00000400!

sqlAtDropColumnDefault
	^16r00000200!

sqlAtDropColumnRestrict
	^16r00000800!

sqlAtDropTableConstraintCascade
	^16r00002000!

sqlAtDropTableConstraintRestrict
	^16r00004000!

sqlAtSetColumnDefault
	^16r00000100!

sqlAttrAccessMode
	^self sqlAccessMode!

sqlAttrAppParamDesc
	^10011!

sqlAttrAppRowDesc
	^10010!

sqlAttrAsyncEnable
	^4!

sqlAttrAutocommit
	^self sqlAutocommit!

sqlAttrAutoIpd
	^10001!

sqlAttrConcurrency
	^self sqlConcurrency!

sqlAttrConnectionPooling
	^201!

sqlAttrConnectionTimeout
	^113!

sqlAttrCpMatch
	^202!

sqlAttrCurrentCatalog
	^self sqlCurrentQualifier!

sqlAttrCursorScrollable
	^(-1)!

sqlAttrCursorSensitivity
	^(-2)!

sqlAttrCursorType
	^self sqlCursorType!

sqlAttrDisconnectBehavior
	^114!

sqlAttrEnableAutoIpd
	^15!

sqlAttrEnlistInDtc
	^1207!

sqlAttrEnlistInXa
	^1208!

sqlAttrFetchBookmarkPtr
	^16!

sqlAttrImpParamDesc
	^10013!

sqlAttrImpRowDesc
	^10012!

sqlAttrKeysetSize
	^self sqlKeysetSize!

sqlAttrLoginTimeout
	^self sqlLoginTimeout!

sqlAttrMaxLength
	^self sqlMaxLength!

sqlAttrMaxRows
	^self sqlMaxRows!

sqlAttrMetadataId
	^10014!

sqlAttrNoscan
	^self sqlNoscan!

sqlAttrOdbcCursors
	^self sqlOdbcCursors!

sqlAttrOdbcVersion
	^200!

sqlAttrOutputNts
	^10001!

sqlAttrPacketSize
	^self sqlPacketSize!

sqlAttrParamBindOffsetPtr
	^17!

sqlAttrParamBindType
	^18!

sqlAttrParamOperationPtr
	^19!

sqlAttrParamsetSize
	^22!

sqlAttrParamsProcessedPtr
	^21!

sqlAttrParamStatusPtr
	^20!

sqlAttrQueryTimeout
	^self sqlQueryTimeout!

sqlAttrQuietMode
	^self sqlQuietMode!

sqlAttrReadonly
	^0!

sqlAttrReadwriteUnknown
	^2!

sqlAttrRetrieveData
	^self sqlRetrieveData!

sqlAttrRowArraySize
	^27!

sqlAttrRowBindOffsetPtr
	^23!

sqlAttrRowBindType
	^self sqlBindType!

sqlAttrRowNumber
	^self sqlRowNumber!

sqlAttrRowOperationPtr
	^24!

sqlAttrRowsFetchedPtr
	^26!

sqlAttrRowStatusPtr
	^25!

sqlAttrSimulateCursor
	^self sqlSimulateCursor!

sqlAttrTrace
	^self sqlOptTrace!

sqlAttrTracefile
	^self sqlOptTracefile!

sqlAttrTranslateLib
	^self sqlTranslateDll!

sqlAttrTranslateOption
	^self sqlTranslateOption!

sqlAttrTxnIsolation
	^self sqlTxnIsolation!

sqlAttrUseBookmarks
	^self sqlUseBookmarks!

sqlAttrWrite
	^1!

sqlAutocommit
	^102!

sqlAutocommitDefault
	^self sqlAutocommitOn!

sqlAutocommitOff
	^0!

sqlAutocommitOn
	^1!

sqlBatchRowCount
	^120!

sqlBatchSupport
	^121!

sqlBestRowid
	^1!

sqlBigint
	^(-5)!

sqlBinary
	^(-2)!

sqlBindByColumn
	^0!

sqlBindType
	^5!

sqlBindTypeDefault
	^self sqlBindByColumn!

sqlBit
	^(-7)!

sqlBookmarkPersistence
	^82!

sqlBpClose
	^16r00000001!

sqlBpDelete
	^16r00000002!

sqlBpDrop
	^16r00000004!

sqlBpOtherHstmt
	^16r00000020!

sqlBpScroll
	^16r00000040!

sqlBpTransaction
	^16r00000008!

sqlBpUpdate
	^16r00000010!

sqlBrcExplicit
	^16r0000002!

sqlBrcProcedures
	^16r0000001!

sqlBrcRolledUp
	^16r0000004!

sqlBsRowCountExplicit
	^16r00000002!

sqlBsRowCountProc
	^16r00000008!

sqlBsSelectExplicit
	^16r00000001!

sqlBsSelectProc
	^16r00000004!

sqlCa1Absolute
	^16r00000002!

sqlCa1Bookmark
	^16r00000008!

sqlCa1BulkAdd
	^16r00010000!

sqlCa1BulkDeleteByBookmark
	^16r00040000!

sqlCa1BulkFetchByBookmark
	^16r00080000!

sqlCa1BulkUpdateByBookmark
	^16r00020000!

sqlCa1LockExclusive
	^16r00000080!

sqlCa1LockNoChange
	^16r00000040!

sqlCa1LockUnlock
	^16r00000100!

sqlCa1Next
	^16r00000001!

sqlCa1PosDelete
	^16r00000800!

sqlCa1PositionedDelete
	^16r00004000!

sqlCa1PositionedUpdate
	^16r00002000!

sqlCa1PosPosition
	^16r00000200!

sqlCa1PosRefresh
	^16r00001000!

sqlCa1PosUpdate
	^16r00000400!

sqlCa1Relative
	^16r00000004!

sqlCa1SelectForUpdate
	^16r00008000!

sqlCa2CrcApproximate
	^16r00002000!

sqlCa2CrcExact
	^16r00001000!

sqlCa2LockConcurrency
	^16r00000002!

sqlCa2MaxRowsAffectsAll
	^(((self sqlCa2MaxRowsSelect bitOr:
	self sqlCa2MaxRowsInsert) bitOr:
	self sqlCa2MaxRowsDelete) bitOr:
	self sqlCa2MaxRowsUpdate) bitOr:
	self sqlCa2MaxRowsCatalog!

sqlCa2MaxRowsCatalog
	^16r00000800!

sqlCa2MaxRowsDelete
	^16r00000200!

sqlCa2MaxRowsInsert
	^16r00000100!

sqlCa2MaxRowsSelect
	^16r00000080!

sqlCa2MaxRowsUpdate
	^16r00000400!

sqlCa2OptRowverConcurrency
	^16r00000004!

sqlCa2OptValuesConcurrency
	^16r00000008!

sqlCa2ReadOnlyConcurrency
	^16r00000001!

sqlCa2SensitivityAdditions
	^16r00000010!

sqlCa2SensitivityDeletions
	^16r00000020!

sqlCa2SensitivityUpdates
	^16r00000040!

sqlCa2SimulateNonUnique
	^16r00004000!

sqlCa2SimulateTryUnique
	^16r00008000!

sqlCa2SimulateUnique
	^16r00010000!

sqlCaConstraintDeferrable
	^16r00000040!

sqlCaConstraintInitiallyDeferred
	^16r00000010!

sqlCaConstraintInitiallyImmediate
	^16r00000020!

sqlCaConstraintNonDeferrable
	^16r00000080!

sqlCaCreateAssertion
	^16r00000001!

sqlCascade
	^0!

sqlCatalogLocation
	^self sqlQualifierLocation!

sqlCatalogName
	^10003!

sqlCatalogNameSeparator
	^self sqlQualifierNameSeparator!

sqlCatalogTerm
	^self sqlQualifierTerm!

sqlCatalogUsage
	^self sqlQualifierUsage!

sqlCbClose
	^1!

sqlCbDelete
	^0!

sqlCBinary
	^self sqlBinary!

sqlCBit
	^self sqlBit!

sqlCbNonNull
	^16r0001!

sqlCbNull
	^16r0000!

sqlCBookmark
	^self sqlCUlong!

sqlCbPreserve
	^2!

sqlCcClose
	^self sqlCbClose!

sqlCcDelete
	^self sqlCbDelete!

sqlCChar
	^self sqlChar!

sqlCcolCreateCollation
	^16r00000001!

sqlCcPreserve
	^self sqlCbPreserve!

sqlCcsCollateClause
	^16r00000002!

sqlCcsCreateCharacterSet
	^16r00000001!

sqlCcsLimitedCollation
	^16r00000004!

sqlCDate
	^self sqlDate!

sqlCDefault
	^99!

sqlCdoCollation
	^16r00000008!

sqlCdoConstraint
	^16r00000004!

sqlCdoConstraintDeferrable
	^16r00000080!

sqlCdoConstraintInitiallyDeferred
	^16r00000020!

sqlCdoConstraintInitiallyImmediate
	^16r00000040!

sqlCdoConstraintNameDefinition
	^16r00000010!

sqlCdoConstraintNonDeferrable
	^16r00000100!

sqlCdoCreateDomain
	^16r00000001!

sqlCdoDefault
	^16r00000002!

sqlCDouble
	^self sqlDouble!

sqlCFloat
	^self sqlReal!

sqlChar
	^1!

sqlCIntervalDay
	^self sqlIntervalDay!

sqlCIntervalDayToHour
	^self sqlIntervalDayToHour!

sqlCIntervalDayToMinute
	^self sqlIntervalDayToMinute!

sqlCIntervalDayToSecond
	^self sqlIntervalDayToSecond!

sqlCIntervalHour
	^self sqlIntervalHour!

sqlCIntervalHourToMinute
	^self sqlIntervalHourToMinute!

sqlCIntervalHourToSecond
	^self sqlIntervalHourToSecond!

sqlCIntervalMinute
	^self sqlIntervalMinute!

sqlCIntervalMinuteToSecond
	^self sqlIntervalMinuteToSecond!

sqlCIntervalMonth
	^self sqlIntervalMonth!

sqlCIntervalSecond
	^self sqlIntervalSecond!

sqlCIntervalYear
	^self sqlIntervalYear!

sqlCIntervalYearToMonth
	^self sqlIntervalYearToMonth!

sqlClEnd
	^self sqlQlEnd!

sqlCLong
	^self sqlInteger!

sqlClose
	^0!

sqlClStart
	^self sqlQlStart!

sqlCnAny
	^16r0002!

sqlCnDifferent
	^16r0001!

sqlCnNone
	^16r0000!

sqlCNumeric
	^self sqlNumeric!

sqlCodeDate
	^1!

sqlCodeDay
	^3!

sqlCodeDayToHour
	^8!

sqlCodeDayToMinute
	^9!

sqlCodeDayToSecond
	^10!

sqlCodeHour
	^4!

sqlCodeHourToMinute
	^11!

sqlCodeHourToSecond
	^12!

sqlCodeMinute
	^5!

sqlCodeMinuteToSecond
	^13!

sqlCodeMonth
	^2!

sqlCodeSecond
	^6!

sqlCodeTime
	^2!

sqlCodeTimestamp
	^3!

sqlCodeYear
	^1!

sqlCodeYearToMonth
	^7!

sqlColattOptMax
	^self sqlColumnLabel!

sqlColattOptMin
	^self sqlColumnCount!

sqlCollationSeq
	^10004!

sqlColPredBasic
	^self sqlAllExceptLike!

sqlColPredChar
	^self sqlLikeOnly!

sqlColumnAlias
	^87!

sqlColumnAutoIncrement
	^11!

sqlColumnCaseSensitive
	^12!

sqlColumnCount
	^0!

sqlColumnDisplaySize
	^6!

sqlColumnDriverStart
	^1000!

sqlColumnIgnore
	^self sqlIgnore!

sqlColumnLabel
	^18!

sqlColumnLength
	^3!

sqlColumnMoney
	^9!

sqlColumnName
	^1!

sqlColumnNullable
	^7!

sqlColumnNumberUnknown
	^(-2)!

sqlColumnOwnerName
	^16!

sqlColumnPrecision
	^4!

sqlColumnQualifierName
	^17!

sqlColumnScale
	^5!

sqlColumnSearchable
	^13!

sqlColumnTableName
	^15!

sqlColumnType
	^2!

sqlColumnTypeName
	^14!

sqlColumnUnsigned
	^8!

sqlColumnUpdatable
	^10!

sqlCommit
	^0!

sqlConcatNullBehavior
	^22!

sqlConcurDefault
	^self sqlConcurReadOnly!

sqlConcurLock
	^2!

sqlConcurReadOnly
	^1!

sqlConcurrency
	^7!

sqlConcurRowver
	^3!

sqlConcurTimestamp
	^self sqlConcurRowver!

sqlConcurValues
	^4!

sqlConnectOptDrvrStart
	^1000!

sqlConnOptMax
	^self sqlPacketSize!

sqlConnOptMin
	^self sqlAccessMode!

sqlConvertBigint
	^53!

sqlConvertBinary
	^54!

sqlConvertBit
	^55!

sqlConvertChar
	^56!

sqlConvertDate
	^57!

sqlConvertDecimal
	^58!

sqlConvertDouble
	^59!

sqlConvertFloat
	^60!

sqlConvertFunctions
	^48!

sqlConvertInteger
	^61!

sqlConvertIntervalDayTime
	^123!

sqlConvertIntervalYearMonth
	^124!

sqlConvertLongvarbinary
	^71!

sqlConvertLongvarchar
	^62!

sqlConvertNumeric
	^63!

sqlConvertReal
	^64!

sqlConvertSmallint
	^65!

sqlConvertTime
	^66!

sqlConvertTimestamp
	^67!

sqlConvertTinyint
	^68!

sqlConvertVarbinary
	^69!

sqlConvertVarchar
	^70!

sqlConvertWchar
	^122!

sqlConvertWlongvarchar
	^125!

sqlConvertWvarchar
	^126!

sqlCorrelationName
	^74!

sqlCpDefault
	^self sqlCpOff!

sqlCpMatchDefault
	^self sqlCpStrictMatch!

sqlCpOff
	^0!

sqlCpOnePerDriver
	^1!

sqlCpOnePerHenv
	^2!

sqlCpRelaxedMatch
	^1!

sqlCpStrictMatch
	^0!

sqlCrClose
	^self sqlCbClose!

sqlCrDelete
	^self sqlCbDelete!

sqlCreateAssertion
	^127!

sqlCreateCharacterSet
	^128!

sqlCreateCollation
	^129!

sqlCreateDomain
	^130!

sqlCreateSchema
	^131!

sqlCreateTable
	^132!

sqlCreateTranslation
	^133!

sqlCreateView
	^134!

sqlCrPreserve
	^self sqlCbPreserve!

sqlCsAuthorization
	^16r00000002!

sqlCSbigint
	^self sqlBigint + self sqlSignedOffset!

sqlCsCreateSchema
	^16r00000001!

sqlCsDefaultCharacterSet
	^16r00000004!

sqlCShort
	^self sqlSmallint!

sqlCSlong
	^(self sqlCLong + self sqlSignedOffset)!

sqlCSshort
	^self sqlCShort + self sqlSignedOffset!

sqlCStinyint
	^self sqlTinyint + self sqlSignedOffset!

sqlCtColumnCollation
	^16r00000800!

sqlCtColumnConstraint
	^16r00000200!

sqlCtColumnDefault
	^16r00000400!

sqlCtCommitDelete
	^16r00000004!

sqlCtCommitPreserve
	^16r00000002!

sqlCtConstraintDeferrable
	^16r00000080!

sqlCtConstraintInitiallyDeferred
	^16r00000020!

sqlCtConstraintInitiallyImmediate
	^16r00000040!

sqlCtConstraintNameDefinition
	^16r00002000!

sqlCtConstraintNonDeferrable
	^16r00000100!

sqlCtCreateTable
	^16r00000001!

sqlCtGlobalTemporary
	^16r00000008!

sqlCTime
	^self sqlTime!

sqlCTimestamp
	^self sqlTimestamp!

sqlCTinyint
	^self sqlTinyint!

sqlCtLocalTemporary
	^16r00000010!

sqlCtrCreateTranslation
	^16r00000001!

sqlCtTableConstraint
	^16r00001000!

sqlCTypeDate
	^self sqlTypeDate!

sqlCTypeTime
	^self sqlTypeTime!

sqlCTypeTimestamp
	^self sqlTypeTimestamp!

sqlCUbigint
	^self sqlBigint + self sqlUnsignedOffset!

sqlCuDmlStatements
	^self sqlQuDmlStatements!

sqlCuIndexDefinition
	^self sqlQuIndexDefinition!

sqlCUlong
	^self sqlCLong  + self sqlUnsignedOffset!

sqlCuPrivilegeDefinition
	^self sqlQuPrivilegeDefinition!

sqlCuProcedureInvocation
	^self sqlQuProcedureInvocation!

sqlCurDefault
	^self sqlCurUseDriver!

sqlCurrentQualifier
	^109!

sqlCursorCommitBehavior
	^23!

sqlCursorDynamic
	^2!

sqlCursorForwardOnly
	^0!

sqlCursorKeysetDriven
	^1!

sqlCursorRollbackBehavior
	^24!

sqlCursorSensitivity
	^10001!

sqlCursorStatic
	^3!

sqlCursorType
	^6!

sqlCursorTypeDefault
	^self sqlCursorForwardOnly!

sqlCurUseDriver
	^2!

sqlCurUseIfNeeded
	^0!

sqlCurUseOdbc
	^1!

sqlCUshort
	^self sqlCShort + self sqlUnsignedOffset!

sqlCuTableDefinition
	^self sqlQuTableDefinition!

sqlCUtinyint
	^self sqlTinyint + self sqlUnsignedOffset!

sqlCVarbookmark
	^self sqlCBinary!

sqlCvCascaded
	^16r00000004!

sqlCvCheckOption
	^16r00000002!

sqlCvCreateView
	^16r00000001!

sqlCvLocal
	^16r00000008!

sqlCvtBigint
	^16r00004000!

sqlCvtBinary
	^16r00000400!

sqlCvtBit
	^16r00001000!

sqlCvtChar
	^16r00000001!

sqlCvtDate
	^16r00008000!

sqlCvtDecimal
	^16r00000004!

sqlCvtDouble
	^16r00000080!

sqlCvtFloat
	^16r00000020!

sqlCvtInteger
	^16r00000008!

sqlCvtIntervalDayTime
	^16r00100000!

sqlCvtIntervalYearMonth
	^16r00080000!

sqlCvtLongvarbinary
	^16r00040000!

sqlCvtLongvarchar
	^16r00000200!

sqlCvtNumeric
	^16r00000002!

sqlCvtReal
	^16r00000040!

sqlCvtSmallint
	^16r00000010!

sqlCvtTime
	^16r00010000!

sqlCvtTimestamp
	^16r00020000!

sqlCvtTinyint
	^16r00002000!

sqlCvtVarbinary
	^16r00000800!

sqlCvtVarchar
	^16r00000100!

sqlCvtWchar
	^16r00200000!

sqlCvtWlongvarchar
	^16r00400000!

sqlCvtWvarchar
	^16r00800000!

sqlDaDropAssertion
	^16r00000001!

sqlDataAtExec
	^(-2)!

sqlDatabaseName
	^16!

sqlDataSourceName
	^2!

sqlDataSourceReadOnly
	^25!

sqlDate
	^9!

sqlDateLen
	^10!

sqlDatetime
	^9!

sqlDatetimeLiterals
	^119!

sqlDay
	^self sqlCodeDay!

sqlDayToHour
	^self sqlCodeDayToHour!

sqlDayToMinute
	^self sqlCodeDayToMinute!

sqlDayToSecond
	^self sqlCodeDayToSecond!

sqlDbDefault
	^self sqlDbReturnToPool!

sqlDbDisconnect
	^1!

sqlDbmsName
	^17!

sqlDbmsVer
	^18!

sqlDbReturnToPool
	^0!

sqlDcDropCollation
	^16r00000001!

sqlDcsDropCharacterSet
	^16r00000001!

sqlDdCascade
	^16r00000004!

sqlDdDropDomain
	^16r00000001!

sqlDdlIndex
	^170!

sqlDdRestrict
	^16r00000002!

sqlDecimal
	^3!

sqlDefault
	^99!

sqlDefaultParam
	^(-5)!

sqlDefaultTxnIsolation
	^26!

sqlDelete
	^3!

sqlDeleteByBookmark
	^6!

sqlDescAllocAuto
	^1!

sqlDescAllocType
	^1099!

sqlDescAllocUser
	^2!

sqlDescArraySize
	^20!

sqlDescArrayStatusPtr
	^21!

sqlDescAutoUniqueValue
	^self sqlColumnAutoIncrement!

sqlDescBaseColumnName
	^22!

sqlDescBaseTableName
	^23!

sqlDescBindOffsetPtr
	^24!

sqlDescBindType
	^25!

sqlDescCaseSensitive
	^self sqlColumnCaseSensitive!

sqlDescCatalogName
	^self sqlColumnQualifierName!

sqlDescConciseType
	^self sqlColumnType!

sqlDescCount
	^1001!

sqlDescDataPtr
	^1010!

sqlDescDatetimeIntervalCode
	^1007!

sqlDescDatetimeIntervalPrecision
	^26!

sqlDescDisplaySize
	^self sqlColumnDisplaySize!

sqlDescFixedPrecScale
	^self sqlColumnMoney!

sqlDescIndicatorPtr
	^1009!

sqlDescLabel
	^self sqlColumnLabel!

sqlDescLength
	^1003!

sqlDescLiteralPrefix
	^27!

sqlDescLiteralSuffix
	^28!

sqlDescLocalTypeName
	^29!

sqlDescMaximumScale
	^30!

sqlDescMinimumScale
	^31!

sqlDescName
	^1011!

sqlDescNullable
	^1008!

sqlDescNumPrecRadix
	^32!

sqlDescOctetLength
	^1013!

sqlDescOctetLengthPtr
	^1004!

sqlDescParameterType
	^33!

sqlDescPrecision
	^1005!

sqlDescribeParameter
	^10002!

sqlDescRowsProcessedPtr
	^34!

sqlDescScale
	^1006!

sqlDescSchemaName
	^self sqlColumnOwnerName!

sqlDescSearchable
	^self sqlColumnSearchable!

sqlDescTableName
	^self sqlColumnTableName!

sqlDescType
	^1002!

sqlDescTypeName
	^self sqlColumnTypeName!

sqlDescUnnamed
	^1012!

sqlDescUnsigned
	^self sqlColumnUnsigned!

sqlDescUpdatable
	^self sqlColumnUpdatable!

sqlDiagAlterTable
	^4!

sqlDiagCall
	^7!

sqlDiagClassOrigin
	^8!

sqlDiagColumnNumber
	^(-1247)!

sqlDiagConnectionName
	^10!

sqlDiagCreateIndex
	^(-1)!

sqlDiagCreateTable
	^77!

sqlDiagCreateView
	^84!

sqlDiagCursorRowCount
	^(-1249)!

sqlDiagDeleteWhere
	^19!

sqlDiagDropIndex
	^(-2)!

sqlDiagDropTable
	^32!

sqlDiagDropView
	^36!

sqlDiagDynamicDeleteCursor
	^38!

sqlDiagDynamicFunction
	^7!

sqlDiagDynamicFunctionCode
	^12!

sqlDiagDynamicUpdateCursor
	^81!

sqlDiagGrant
	^48!

sqlDiagInsert
	^50!

sqlDiagMessageText
	^6!

sqlDiagNative
	^5!

sqlDiagNumber
	^2!

sqlDiagReturncode
	^1!

sqlDiagRevoke
	^59!

sqlDiagRowCount
	^3!

sqlDiagRowNumber
	^(-1248)!

sqlDiagSelectCursor
	^85!

sqlDiagServerName
	^11!

sqlDiagSqlstate
	^4!

sqlDiagSubclassOrigin
	^9!

sqlDiagUnknownStatement
	^0!

sqlDiagUpdateWhere
	^82!

sqlDiCreateIndex
	^16r00000001!

sqlDiDropIndex
	^16r00000002!

sqlDlSql92Date
	^16r00000001!

sqlDlSql92IntervalDay
	^16r00000020!

sqlDlSql92IntervalDayToHour
	^16r00000400!

sqlDlSql92IntervalDayToMinute
	^16r00000800!

sqlDlSql92IntervalDayToSecond
	^16r00001000!

sqlDlSql92IntervalHour
	^16r00000040!

sqlDlSql92IntervalHourToMinute
	^16r00002000!

sqlDlSql92IntervalHourToSecond
	^16r00004000!

sqlDlSql92IntervalMinute
	^16r00000080!

sqlDlSql92IntervalMinuteToSecond
	^16r00008000!

sqlDlSql92IntervalMonth
	^16r00000010!

sqlDlSql92IntervalSecond
	^16r00000100!

sqlDlSql92IntervalYear
	^16r00000008!

sqlDlSql92IntervalYearToMonth
	^16r00000200!

sqlDlSql92Time
	^16r00000002!

sqlDlSql92Timestamp
	^16r00000004!

sqlDmVer
	^171!

sqlDouble
	^8!

sqlDriverComplete
	^1!

sqlDriverCompleteRequired
	^3!

sqlDriverHdbc
	^3!

sqlDriverHdesc
	^135!

sqlDriverHenv
	^4!

sqlDriverHlib
	^76!

sqlDriverHstmt
	^5!

sqlDriverName
	^6!

sqlDriverNoprompt
	^0!

sqlDriverOdbcVer
	^77!

sqlDriverPrompt
	^2!

sqlDriverVer
	^7!

sqlDrop
	^1!

sqlDropAssertion
	^136!

sqlDropCharacterSet
	^137!

sqlDropCollation
	^138!

sqlDropDomain
	^139!

sqlDropSchema
	^140!

sqlDropTable
	^141!

sqlDropTranslation
	^142!

sqlDropView
	^143!

sqlDsCascade
	^16r00000004!

sqlDsDropSchema
	^16r00000001!

sqlDsRestrict
	^16r00000002!

sqlDtCascade
	^16r00000004!

sqlDtcDone
	^0!

sqlDtDropTable
	^16r00000001!

sqlDtrDropTranslation
	^16r00000001!

sqlDtRestrict
	^16r00000002!

sqlDvCascade
	^16r00000004!

sqlDvDropView
	^16r00000001!

sqlDvRestrict
	^16r00000002!

sqlDynamicCursorAttributes1
	^144!

sqlDynamicCursorAttributes2
	^145!

sqlEnsure
	^1!

sqlEntireRowset
	^0!

sqlError
	^(-1)!

sqlExpressionsInOrderby
	^27!

sqlExtApiLast
	^self sqlApiSqlbindparameter!

sqlExtApiStart
	^40!

sqlFalse
	^0!

sqlFdFetchAbsolute
	^16r00000010!

sqlFdFetchBookmark
	^16r00000080!

sqlFdFetchFirst
	^16r00000002!

sqlFdFetchLast
	^16r00000004!

sqlFdFetchNext
	^16r00000001!

sqlFdFetchPrev
	^self sqlFdFetchPrior!

sqlFdFetchPrior
	^16r00000008!

sqlFdFetchRelative
	^16r00000020!

sqlFdFetchResume
	^16r00000040!

sqlFetchAbsolute
	^5!

sqlFetchBookmark
	^8!

sqlFetchByBookmark
	^7!

sqlFetchDirection
	^8!

sqlFetchFirst
	^2!

sqlFetchFirstSystem
	^32!

sqlFetchFirstUser
	^31!

sqlFetchLast
	^3!

sqlFetchNext
	^1!

sqlFetchPrev
	^self sqlFetchPrior!

sqlFetchPrior
	^4!

sqlFetchRelative
	^6!

sqlFetchResume
	^7!

sqlFileCatalog
	^self sqlFileQualifier!

sqlFileNotSupported
	^16r0000!

sqlFileQualifier
	^16r0002!

sqlFileTable
	^16r0001!

sqlFileUsage
	^84!

sqlFloat
	^6!

sqlFnCvtCast
	^16r00000002!

sqlFnCvtConvert
	^16r00000001!

sqlFnNumAbs
	^16r00000001!

sqlFnNumAcos
	^16r00000002!

sqlFnNumAsin
	^16r00000004!

sqlFnNumAtan
	^16r00000008!

sqlFnNumAtan2
	^16r00000010!

sqlFnNumCeiling
	^16r00000020!

sqlFnNumCos
	^16r00000040!

sqlFnNumCot
	^16r00000080!

sqlFnNumDegrees
	^16r00040000!

sqlFnNumExp
	^16r00000100!

sqlFnNumFloor
	^16r00000200!

sqlFnNumLog
	^16r00000400!

sqlFnNumLog10
	^16r00080000!

sqlFnNumMod
	^16r00000800!

sqlFnNumPi
	^16r00010000!

sqlFnNumPower
	^16r00100000!

sqlFnNumRadians
	^16r00200000!

sqlFnNumRand
	^16r00020000!

sqlFnNumRound
	^16r00400000!

sqlFnNumSign
	^16r00001000!

sqlFnNumSin
	^16r00002000!

sqlFnNumSqrt
	^16r00004000!

sqlFnNumTan
	^16r00008000!

sqlFnNumTruncate
	^16r00800000!

sqlFnStrAscii
	^16r00002000!

sqlFnStrBitLength
	^16r00080000!

sqlFnStrChar
	^16r00004000!

sqlFnStrCharacterLength
	^16r00200000!

sqlFnStrCharLength
	^16r00100000!

sqlFnStrConcat
	^16r00000001!

sqlFnStrDifference
	^16r00008000!

sqlFnStrInsert
	^16r00000002!

sqlFnStrLcase
	^16r00000040!

sqlFnStrLeft
	^16r00000004!

sqlFnStrLength
	^16r00000010!

sqlFnStrLocate
	^16r00000020!

sqlFnStrLocate2
	^16r00010000!

sqlFnStrLtrim
	^16r00000008!

sqlFnStrOctetLength
	^16r00400000!

sqlFnStrPosition
	^16r00800000!

sqlFnStrRepeat
	^16r00000080!

sqlFnStrReplace
	^16r00000100!

sqlFnStrRight
	^16r00000200!

sqlFnStrRtrim
	^16r00000400!

sqlFnStrSoundex
	^16r00020000!

sqlFnStrSpace
	^16r00040000!

sqlFnStrSubstring
	^16r00000800!

sqlFnStrUcase
	^16r00001000!

sqlFnSysDbname
	^16r00000002!

sqlFnSysIfnull
	^16r00000004!

sqlFnSysUsername
	^16r00000001!

sqlFnTdCurdate
	^16r00000002!

sqlFnTdCurrentDate
	^16r00020000!

sqlFnTdCurrentTime
	^16r00040000!

sqlFnTdCurrentTimestamp
	^16r00080000!

sqlFnTdCurtime
	^16r00000200!

sqlFnTdDayname
	^16r00008000!

sqlFnTdDayofmonth
	^16r00000004!

sqlFnTdDayofweek
	^16r00000008!

sqlFnTdDayofyear
	^16r00000010!

sqlFnTdExtract
	^16r00100000!

sqlFnTdHour
	^16r00000400!

sqlFnTdMinute
	^16r00000800!

sqlFnTdMonth
	^16r00000020!

sqlFnTdMonthname
	^16r00010000!

sqlFnTdNow
	^16r00000001!

sqlFnTdQuarter
	^16r00000040!

sqlFnTdSecond
	^16r00001000!

sqlFnTdTimestampadd
	^16r00002000!

sqlFnTdTimestampdiff
	^16r00004000!

sqlFnTdWeek
	^16r00000080!

sqlFnTdYear
	^16r00000100!

sqlFnTsiDay
	^16r00000010!

sqlFnTsiFracSecond
	^16r00000001!

sqlFnTsiHour
	^16r00000008!

sqlFnTsiMinute
	^16r00000004!

sqlFnTsiMonth
	^16r00000040!

sqlFnTsiQuarter
	^16r00000080!

sqlFnTsiSecond
	^16r00000002!

sqlFnTsiWeek
	^16r00000020!

sqlFnTsiYear
	^16r00000100!

sqlForwardOnlyCursorAttributes1
	^146!

sqlForwardOnlyCursorAttributes2
	^147!

sqlGbCollate
	^16r0004!

sqlGbGroupByContainsSelect
	^16r0002!

sqlGbGroupByEqualsSelect
	^16r0001!

sqlGbNoRelation
	^16r0003!

sqlGbNotSupported
	^16r0000!

sqlGdAnyColumn
	^16r00000001!

sqlGdAnyOrder
	^16r00000002!

sqlGdBlock
	^16r00000004!

sqlGdBound
	^16r00000008!

sqlGetBookmark
	^13!

sqlGetdataExtensions
	^81!

sqlGroupBy
	^88!

sqlHandleDbc
	^2!

sqlHandleDesc
	^4!

sqlHandleEnv
	^1!

sqlHandleSenv
	^5!

sqlHandleStmt
	^3!

sqlHour
	^self sqlCodeHour!

sqlHourToMinute
	^self sqlCodeHourToMinute!

sqlHourToSecond
	^self sqlCodeHourToSecond!

sqlIcLower
	^2!

sqlIcMixed
	^4!

sqlIcSensitive
	^3!

sqlIcUpper
	^1!

sqlIdentifierCase
	^28!

sqlIdentifierQuoteChar
	^29!

sqlIgnore
	^(-6)!

sqlIkAll
	^self sqlIkAsc | self sqlIkDesc!

sqlIkAsc
	^16r00000001!

sqlIkDesc
	^16r00000002!

sqlIkNone
	^16r00000000!

sqlIndexAll
	^1!

sqlIndexClustered
	^1!

sqlIndexHashed
	^2!

sqlIndexKeywords
	^148!

sqlIndexOther
	^3!

sqlIndexUnique
	^0!

sqlInfoDriverStart
	^1000!

sqlInfoFirst
	^0!

sqlInfoLast
	^self sqlQualifierLocation!

sqlInfoSchemaViews
	^149!

sqlInitiallyDeferred
	^5!

sqlInitiallyImmediate
	^6!

sqlInsensitive
	^1!

sqlInsertStatement
	^172!

sqlInteger
	^4!

sqlIntegrity
	^73!

sqlInterval
	^10!

sqlIntervalDay
	^(-83)!

sqlIntervalDayToHour
	^(-87)!

sqlIntervalDayToMinute
	^(-88)!

sqlIntervalDayToSecond
	^(-89)!

sqlIntervalHour
	^(-84)!

sqlIntervalHourToMinute
	^(-90)!

sqlIntervalHourToSecond
	^(-91)!

sqlIntervalMinute
	^(-85)!

sqlIntervalMinuteToSecond
	^(-92)!

sqlIntervalMonth
	^(-81)!

sqlIntervalSecond
	^(-86)!

sqlIntervalYear
	^(-80)!

sqlIntervalYearToMonth
	^(-82)!

sqlInvalidHandle
	^(-2)!

sqlIsInsertLiterals
	^16r00000001!

sqlIsInsertSearched
	^16r00000002!

sqlIsInteger
	^(-6)!

sqlIsPointer
	^(-4)!

sqlIsSelectInto
	^16r00000004!

sqlIsSmallint
	^(-8)!

sqlIsUinteger
	^(-5)!

sqlIsUsmallint
	^(-7)!

sqlIsvAssertions
	^16r00000001!

sqlIsvCharacterSets
	^16r00000002!

sqlIsvCheckConstraints
	^16r00000004!

sqlIsvCollations
	^16r00000008!

sqlIsvColumnDomainUsage
	^16r00000010!

sqlIsvColumnPrivileges
	^16r00000020!

sqlIsvColumns
	^16r00000040!

sqlIsvConstraintColumnUsage
	^16r00000080!

sqlIsvConstraintTableUsage
	^16r00000100!

sqlIsvDomainConstraints
	^16r00000200!

sqlIsvDomains
	^16r00000400!

sqlIsvKeyColumnUsage
	^16r00000800!

sqlIsvReferentialConstraints
	^16r00001000!

sqlIsvSchemata
	^16r00002000!

sqlIsvSqlLanguages
	^16r00004000!

sqlIsvTableConstraints
	^16r00008000!

sqlIsvTablePrivileges
	^16r00010000!

sqlIsvTables
	^16r00020000!

sqlIsvTranslations
	^16r00040000!

sqlIsvUsagePrivileges
	^16r00080000!

sqlIsvViewColumnUsage
	^16r00100000!

sqlIsvViews
	^16r00400000!

sqlIsvViewTableUsage
	^16r00200000!

sqlKeysetCursorAttributes1
	^150!

sqlKeysetCursorAttributes2
	^151!

sqlKeysetSize
	^8!

sqlKeysetSizeDefault
	^0!

sqlKeywords
	^89!

sqlLckExclusive
	^16r00000002!

sqlLckNoChange
	^16r00000001!

sqlLckUnlock
	^16r00000004!

sqlLenBinaryAttr: length
	^length negated + self sqlLenBinaryAttrOffset!

sqlLenBinaryAttrOffset
	^(-100)!

sqlLenDataAtExec: length
	^length negated + self sqlLenDataAtExecOffset!

sqlLenDataAtExecOffset
	^(-100)!

sqlLikeEscapeClause
	^113!

sqlLikeOnly
	^1!

sqlLockExclusive
	^1!

sqlLockNoChange
	^0!

sqlLockTypes
	^78!

sqlLockUnlock
	^2!

sqlLoginTimeout
	^103!

sqlLoginTimeoutDefault
	^15!

sqlLongvarbinary
	^(-4)!

sqlLongvarchar
	^(-1)!

sqlMaxAsyncConcurrentStatements
	^10022!

sqlMaxBinaryLiteralLen
	^112!

sqlMaxCatalogNameLen
	^34!

sqlMaxCharLiteralLen
	^108!

sqlMaxColumnNameLen
	^30!

sqlMaxColumnsInGroupBy
	^97!

sqlMaxColumnsInIndex
	^98!

sqlMaxColumnsInOrderBy
	^99!

sqlMaxColumnsInSelect
	^100!

sqlMaxColumnsInTable
	^101!

sqlMaxConcurrentActivities
	^1!

sqlMaxCursorNameLen
	^31!

sqlMaxDriverConnections
	^0!

sqlMaxDsnLength
	^32!

sqlMaxIdentifierLen
	^10005!

sqlMaximumCatalogNameLength
	^self sqlMaxCatalogNameLen!

sqlMaximumColumnNameLength
	^self sqlMaxColumnNameLen!

sqlMaximumColumnsInGroupBy
	^self sqlMaxColumnsInGroupBy!

sqlMaximumColumnsInIndex
	^self sqlMaxColumnsInIndex!

sqlMaximumColumnsInOrderBy
	^self sqlMaxColumnsInOrderBy!

sqlMaximumColumnsInSelect
	^self sqlMaxColumnsInSelect!

sqlMaximumConcurrentActivities
	^self sqlMaxConcurrentActivities!

sqlMaximumCursorNameLength
	^self sqlMaxCursorNameLen!

sqlMaximumDriverConnections
	^self sqlMaxDriverConnections!

sqlMaximumIdentifierLength
	^self sqlMaxIdentifierLen!

sqlMaximumIndexSize
	^self sqlMaxIndexSize!

sqlMaximumRowSize
	^self sqlMaxRowSize!

sqlMaximumSchemaNameLength
	^self sqlMaxSchemaNameLen!

sqlMaximumStatementLength
	^self sqlMaxStatementLen!

sqlMaximumTablesInSelect
	^self sqlMaxTablesInSelect!

sqlMaximumUserNameLength
	^self sqlMaxUserNameLen!

sqlMaxIndexSize
	^102!

sqlMaxLength
	^3!

sqlMaxLengthDefault
	^0!

sqlMaxMessageLength
	^512!

sqlMaxOptionStringLength
	^256!

sqlMaxOwnerNameLen
	^32!

sqlMaxProcedureNameLen
	^33!

sqlMaxQualifierNameLen
	^34!

sqlMaxRows
	^1!

sqlMaxRowsDefault
	^0!

sqlMaxRowSize
	^104!

sqlMaxRowSizeIncludesLong
	^103!

sqlMaxSchemaNameLen
	^32!

sqlMaxStatementLen
	^105!

sqlMaxTableNameLen
	^35!

sqlMaxTablesInSelect
	^106!

sqlMaxUserNameLen
	^107!

sqlMinute
	^self sqlCodeMinute!

sqlMinuteToSecond
	^self sqlCodeMinuteToSecond!

sqlModeDefault
	^self sqlModeReadWrite!

sqlModeReadOnly
	^1!

sqlModeReadWrite
	^0!

sqlMonth
	^self sqlCodeMonth!

sqlMultipleActiveTxn
	^37!

sqlMultResultSets
	^36!

sqlNamed
	^0!

sqlNcEnd
	^16r0004!

sqlNcHigh
	^0!

sqlNcLow
	^1!

sqlNcStart
	^16r0002!

sqlNeedData
	^99!

sqlNeedLongDataLen
	^111!

sqlNncNonNull
	^16r0001!

sqlNncNull
	^16r0000!

sqlNoAction
	^3!

sqlNoColumnNumber
	^(-1)!

sqlNoData
	^100!

sqlNoDataFound
	^self sqlNoData!

sqlNonNullableColumns
	^75!

sqlNonscrollable
	^0!

sqlNoNulls
	^0!

sqlNoRowNumber
	^(-1)!

sqlNoscan
	^2!

sqlNoscanDefault
	^self sqlNoscanOff!

sqlNoscanOff
	^0!

sqlNoscanOn
	^1!

sqlNoTotal
	^(-4)!

sqlNts
	^(-3)!

sqlNtsl
	^(-3)!

sqlNullable
	^1!

sqlNullableUnknown
	^2!

sqlNullCollation
	^85!

sqlNullData
	^(-1)!

sqlNullHandle
	^0!

sqlNullHdbc
	^0!

sqlNullHdesc
	^0!

sqlNullHenv
	^0!

sqlNullHstmt
	^0!

sqlNumeric
	^2!

sqlNumericFunctions
	^49!

sqlNumExtensions
	^ self sqlExtApiLast - self sqlExtApiStart + 1!

sqlNumFunctions
	^23!

sqlOacLevel1
	^16r0001!

sqlOacLevel2
	^16r0002!

sqlOacNone
	^16r0000!

sqlOdbcApiConformance
	^9!

sqlOdbcCursors
	^110!

sqlOdbcInterfaceConformance
	^152!

sqlOdbcKeywords
	^'ABSOLUTE,ACTION,ADA,ADD,ALL,ALLOCATE,ALTER,AND,ANY,ARE,AS,',
	'ASC,ASSERTION,AT,AUTHORIZATION,AVG,',
	'BEGIN,BETWEEN,BIT,BIT_LENGTH,BOTH,BY,CASCADE,CASCADED,CASE,CAST,CATALOG,',
	'CHAR,CHAR_LENGTH,CHARACTER,CHARACTER_LENGTH,CHECK,CLOSE,COALESCE,',
	'COLLATE,COLLATION,COLUMN,COMMIT,CONNECT,CONNECTION,CONSTRAINT,',
	'CONSTRAINTS,CONTINUE,CONVERT,CORRESPONDING,COUNT,CREATE,CROSS,CURRENT,',
	'CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,CURSOR,',
	'DATE,DAY,DEALLOCATE,DEC,DECIMAL,DECLARE,DEFAULT,DEFERRABLE,',
	'DEFERRED,DELETE,DESC,DESCRIBE,DESCRIPTOR,DIAGNOSTICS,DISCONNECT,',
	'DISTINCT,DOMAIN,DOUBLE,DROP,',
	'ELSE,END,END-EXEC,ESCAPE,EXCEPT,EXCEPTION,EXEC,EXECUTE,',
	'EXISTS,EXTERNAL,EXTRACT,',
	'FALSE,FETCH,FIRST,FLOAT,FOR,FOREIGN,FORTRAN,FOUND,FROM,FULL,',
	'GET,GLOBAL,GO,GOTO,GRANT,GROUP,HAVING,HOUR,',
	'IDENTITY,IMMEDIATE,IN,INCLUDE,INDEX,INDICATOR,INITIALLY,INNER,',
	'INPUT,INSENSITIVE,INSERT,INT,INTEGER,INTERSECT,INTERVAL,INTO,IS,ISOLATION,',
	'JOIN,KEY,LANGUAGE,LAST,LEADING,LEFT,LEVEL,LIKE,LOCAL,LOWER,',
	'MATCH,MAX,MIN,MINUTE,MODULE,MONTH,',
	'NAMES,NATIONAL,NATURAL,NCHAR,NEXT,NO,NONE,NOT,NULL,NULLIF,NUMERIC,',
	'OCTET_LENGTH,OF,ON,ONLY,OPEN,OPTION,OR,ORDER,OUTER,OUTPUT,OVERLAPS,',
	'PAD,PARTIAL,PASCAL,PLI,POSITION,PRECISION,PREPARE,PRESERVE,',
	'PRIMARY,PRIOR,PRIVILEGES,PROCEDURE,PUBLIC,',
	'READ,REAL,REFERENCES,RELATIVE,RESTRICT,REVOKE,RIGHT,ROLLBACK,ROWS',
	'SCHEMA,SCROLL,SECOND,SECTION,SELECT,SESSION,SESSION_USER,SET,SIZE,',
	'SMALLINT,SOME,SPACE,SQL,SQLCA,SQLCODE,SQLERROR,SQLSTATE,SQLWARNING,',
	'SUBSTRING,SUM,SYSTEM_USER,',
	'TABLE,TEMPORARY,THEN,TIME,TIMESTAMP,TIMEZONE_HOUR,TIMEZONE_MINUTE,',
	'TO,TRAILING,TRANSACTION,TRANSLATE,TRANSLATION,TRIM,TRUE,',
	'UNION,UNIQUE,UNKNOWN,UPDATE,UPPER,USAGE,USER,USING,',
	'VALUE,VALUES,VARCHAR,VARYING,VIEW,WHEN,WHENEVER,WHERE,WITH,WORK,WRITE,',
	'YEAR,ZONE'!

sqlOdbcSagCliConformance
	^12!

sqlOdbcSqlConformance
	^15!

sqlOdbcSqlOptIef
	^73!

sqlOdbcVer
	^10!

sqlOicCore
	^1!

sqlOicLevel1
	^2!

sqlOicLevel2
	^3!

sqlOjAllComparisonOps
	^16r00000040!

sqlOjCapabilities
	^65003!

sqlOjFull
	^16r00000004!

sqlOjInner
	^16r00000020!

sqlOjLeft
	^16r00000001!

sqlOjNested
	^16r00000008!

sqlOjNotOrdered
	^16r00000010!

sqlOjRight
	^16r00000002!

sqlOptTrace
	^104!

sqlOptTraceDefault
	^self sqlOptTraceOff!

sqlOptTracefile
	^105!

sqlOptTraceFileDefault
	^'\\SQL.LOG'!

sqlOptTraceOff
	^0!

sqlOptTraceOn
	^1!

sqlOrderByColumnsInSelect
	^90!

sqlOsccCompliant
	^16r0001!

sqlOsccNotCompliant
	^16r0000!

sqlOscCore
	^16r0001!

sqlOscExtended
	^16r0002!

sqlOscMinimum
	^16r0000!

sqlOuDmlStatements
	^16r00000001!

sqlOuIndexDefinition
	^16r00000008!

sqlOuPrivilegeDefinition
	^16r00000010!

sqlOuProcedureInvocation
	^16r00000002!

sqlOuTableDefinition
	^16r00000004!

sqlOuterJoinCapabilities
	^self sqlOjCapabilities!

sqlOuterJoins
	^38!

sqlOvOdbc2
	^2!

sqlOvOdbc3
	^3!

sqlOwnerTerm
	^39!

sqlOwnerUsage
	^91!

sqlPacketSize
	^112!

sqlParamArrayRowCounts
	^153!

sqlParamArraySelects
	^154!

sqlParamBindByColumn
	^0!

sqlParamBindTypeDefault
	^self sqlParamBindByColumn!

sqlParamDiagUnavailable
	^1!

sqlParamError
	^5!

sqlParamIgnore
	^1!

sqlParamInput
	^1!

sqlParamInputOutput
	^2!

sqlParamOutput
	^4!

sqlParamProceed
	^0!

sqlParamSuccess
	^0!

sqlParamSuccessWithInfo
	^6!

sqlParamTypeDefault
	^self sqlParamInputOutput!

sqlParamTypeUnknown
	^0!

sqlParamUnused
	^7!

sqlParcBatch
	^1!

sqlParcNoBatch
	^2!

sqlPasBatch
	^1!

sqlPasNoBatch
	^2!

sqlPasNoSelect
	^3!

sqlPcNonPseudo
	^1!

sqlPcNotPseudo
	^1!

sqlPcPseudo
	^2!

sqlPcUnknown
	^0!

sqlPosAdd
	^16r00000010!

sqlPosDelete
	^16r00000008!

sqlPosition
	^0!

sqlPositionedStatements
	^80!

sqlPosOperations
	^79!

sqlPosPosition
	^16r00000001!

sqlPosRefresh
	^16r00000002!

sqlPosUpdate
	^16r00000004!

sqlPredBasic
	^2!

sqlPredChar
	^1!

sqlPredNone
	^0!

sqlPredSearchable
	^self sqlSearchable!

sqlProcedures
	^21!

sqlProcedureTerm
	^40!

sqlPsPositionedDelete
	^16r00000001!

sqlPsPositionedUpdate
	^16r00000002!

sqlPsSelectForUpdate
	^16r00000004!

sqlPtFunction
	^2!

sqlPtProcedure
	^1!

sqlPtUnknown
	^0!

sqlQlEnd
	^16r0002!

sqlQlStart
	^16r0001!

sqlQualifierLocation
	^114!

sqlQualifierNameSeparator
	^41!

sqlQualifierTerm
	^42!

sqlQualifierUsage
	^92!

sqlQuDmlStatements
	^16r00000001!

sqlQueryTimeout
	^0!

sqlQueryTimeoutDefault
	^0!

sqlQuick
	^0!

sqlQuietMode
	^111!

sqlQuIndexDefinition
	^16r00000008!

sqlQuotedIdentifierCase
	^93!

sqlQuPrivilegeDefinition
	^16r00000010!

sqlQuProcedureInvocation
	^16r00000002!

sqlQuTableDefinition
	^16r00000004!

sqlRdDefault
	^self sqlRdOn!

sqlRdOff
	^0!

sqlRdOn
	^1!

sqlReal
	^7!

sqlRefresh
	^1!

sqlResetParams
	^3!

sqlRestrict
	^1!

sqlResultCol
	^3!

sqlRetrieveData
	^11!

sqlReturnValue
	^5!

sqlRollback
	^1!

sqlRowAdded
	^4!

sqlRowDeleted
	^1!

sqlRowError
	^5!

sqlRowIdentifier
	^1!

sqlRowIgnore
	^1!

sqlRowNorow
	^3!

sqlRowNumber
	^14!

sqlRowNumberUnknown
	^(-2)!

sqlRowProceed
	^0!

sqlRowsetSize
	^9!

sqlRowsetSizeDefault
	^1!

sqlRowSuccess
	^0!

sqlRowSuccessWithInfo
	^6!

sqlRowUpdated
	^2!

sqlRowUpdates
	^11!

sqlRowver
	^2!

sqlSccIso92Cli
	^16r00000002!

sqlSccoLock
	^16r00000002!

sqlSccoOptRowver
	^16r00000004!

sqlSccoOptTimestamp
	^self sqlSccoOptRowver!

sqlSccoOptValues
	^16r00000008!

sqlSccoReadOnly
	^16r00000001!

sqlSccXopenCliVersion1
	^16r00000001!

sqlScFips1272Transitional
	^16r00000002!

sqlSchemaTerm
	^self sqlOwnerTerm!

sqlSchemaUsage
	^self sqlOwnerUsage!

sqlScNonUnique
	^0!

sqlScopeCurrow
	^0!

sqlScopeSession
	^2!

sqlScopeTransaction
	^1!

sqlScrollable
	^1!

sqlScrollConcurrency
	^43!

sqlScrollDynamic
	^(-2)!

sqlScrollForwardOnly
	^0!

sqlScrollKeysetDriven
	^(-1)!

sqlScrollOptions
	^44!

sqlScrollStatic
	^(-3)!

sqlScSql92Entry
	^16r00000001!

sqlScSql92Full
	^16r00000008!

sqlScSql92Intermediate
	^16r00000004!

sqlScTryUnique
	^1!

sqlScUnique
	^2!

sqlSdfCurrentDate
	^16r00000001!

sqlSdfCurrentTime
	^16r00000002!

sqlSdfCurrentTimestamp
	^16r00000004!

sqlSearchable
	^3!

sqlSearchPatternEscape
	^14!

sqlSecond
	^self sqlCodeSecond!

sqlSensitive
	^2!

sqlServerName
	^13!

sqlSetDefault
	^4!

sqlSetNull
	^2!

sqlSetparamValueMax
	^(-1)!

sqlSetposMaxLockValue
	^self sqlLockUnlock!

sqlSetposMaxOptionValue
	^self sqlAdd!

sqlSfkdCascade
	^16r00000001!

sqlSfkdNoAction
	^16r00000002!

sqlSfkdSetDefault
	^16r00000004!

sqlSfkdSetNull
	^16r00000008!

sqlSfkuCascade
	^16r00000001!

sqlSfkuNoAction
	^16r00000002!

sqlSfkuSetDefault
	^16r00000004!

sqlSfkuSetNull
	^16r00000008!

sqlSgDeleteTable
	^16r00000020!

sqlSgInsertColumn
	^16r00000080!

sqlSgInsertTable
	^16r00000040!

sqlSgReferencesColumn
	^16r00000200!

sqlSgReferencesTable
	^16r00000100!

sqlSgSelectTable
	^16r00000400!

sqlSgUpdateColumn
	^16r00001000!

sqlSgUpdateTable
	^16r00000800!

sqlSgUsageOnCharacterSet
	^16r00000002!

sqlSgUsageOnCollation
	^16r00000004!

sqlSgUsageOnDomain
	^16r00000001!

sqlSgUsageOnTranslation
	^16r00000008!

sqlSgWithGrantOption
	^16r00000010!

sqlSignedOffset
	^(-20)!

sqlSimulateCursor
	^10!

sqlSmallint
	^5!

sqlSnvfBitLength
	^16r00000001!

sqlSnvfCharacterLength
	^16r00000004!

sqlSnvfCharLength
	^16r00000002!

sqlSnvfExtract
	^16r00000008!

sqlSnvfOctetLength
	^16r00000010!

sqlSnvfPosition
	^16r00000020!

sqlSoDynamic
	^16r00000004!

sqlSoForwardOnly
	^16r00000001!

sqlSoKeysetDriven
	^16r00000002!

sqlSoMixed
	^16r00000008!

sqlSoStatic
	^16r00000010!

sqlSpBetween
	^16r00000800!

sqlSpComparison
	^16r00001000!

sqlSpecialCharacters
	^94!

sqlSpecMajor
	^3!

sqlSpecMinor
	^00!

sqlSpecString
	^'03.00'!

sqlSpExists
	^16r00000001!

sqlSpIn
	^16r00000400!

sqlSpIsnotnull
	^16r00000002!

sqlSpIsnull
	^16r00000004!

sqlSpLike
	^16r00000200!

sqlSpMatchFull
	^16r00000008!

sqlSpMatchPartial
	^16r00000010!

sqlSpMatchUniqueFull
	^16r00000020!

sqlSpMatchUniquePartial
	^16r00000040!

sqlSpOverlaps
	^16r00000080!

sqlSpQuantifiedComparison
	^16r00002000!

sqlSpUnique
	^16r00000100!

sqlSqComparison
	^16r00000001!

sqlSqCorrelatedSubqueries
	^16r00000010!

sqlSqExists
	^16r00000002!

sqlSqIn
	^16r00000004!

sqlSql92DatetimeFunctions
	^155!

sqlSql92ForeignKeyDeleteRule
	^156!

sqlSql92ForeignKeyUpdateRule
	^157!

sqlSql92Grant
	^158!

sqlSql92NumericValueFunctions
	^159!

sqlSql92Predicates
	^160!

sqlSql92RelationalJoinOperators
	^161!

sqlSql92Revoke
	^162!

sqlSql92RowValueConstructor
	^163!

sqlSql92StringFunctions
	^164!

sqlSql92ValueExpressions
	^165!

sqlSqlConformance
	^118!

sqlSqlstateSize
	^5!

sqlSqQuantified
	^16r00000008!

sqlSrCascade
	^16r00000020!

sqlSrDeleteTable
	^16r00000080!

sqlSrGrantOptionFor
	^16r00000010!

sqlSrInsertColumn
	^16r00000200!

sqlSrInsertTable
	^16r00000100!

sqlSrjoCorrespondingClause
	^16r00000001!

sqlSrjoCrossJoin
	^16r00000002!

sqlSrjoExceptJoin
	^16r00000004!

sqlSrjoFullOuterJoin
	^16r00000008!

sqlSrjoInnerJoin
	^16r00000010!

sqlSrjoIntersectJoin
	^16r00000020!

sqlSrjoLeftOuterJoin
	^16r00000040!

sqlSrjoNaturalJoin
	^16r00000080!

sqlSrjoRightOuterJoin
	^16r00000100!

sqlSrjoUnionJoin
	^16r00000200!

sqlSrReferencesColumn
	^16r00000800!

sqlSrReferencesTable
	^16r00000400!

sqlSrRestrict
	^16r00000040!

sqlSrSelectTable
	^16r00001000!

sqlSrUpdateColumn
	^16r00004000!

sqlSrUpdateTable
	^16r00002000!

sqlSrUsageOnCharacterSet
	^16r00000002!

sqlSrUsageOnCollation
	^16r00000004!

sqlSrUsageOnDomain
	^16r00000001!

sqlSrUsageOnTranslation
	^16r00000008!

sqlSrvcDefault
	^16r00000004!

sqlSrvcNull
	^16r00000002!

sqlSrvcRowSubquery
	^16r00000008!

sqlSrvcValueExpression
	^16r00000001!

sqlSsAdditions
	^16r00000001!

sqlSsDeletions
	^16r00000002!

sqlSsfConvert
	^16r00000001!

sqlSsfLower
	^16r00000002!

sqlSsfSubstring
	^16r00000008!

sqlSsfTranslate
	^16r00000010!

sqlSsfTrimBoth
	^16r00000020!

sqlSsfTrimLeading
	^16r00000040!

sqlSsfTrimTrailing
	^16r00000080!

sqlSsfUpper
	^16r00000004!

sqlSsUpdates
	^16r00000004!

sqlStandardCliConformance
	^166!

sqlStaticCursorAttributes1
	^167!

sqlStaticCursorAttributes2
	^168!

sqlStaticSensitivity
	^83!

sqlStillExecuting
	^2!

sqlStmtOptMax
	^self sqlRowNumber!

sqlStmtOptMin
	^self sqlQueryTimeout!

sqlStringFunctions
	^50!

sqlSubqueries
	^95!

sqlSucceeded: rc
	^ (rc bitAnd: 1 bitInvert) = 0!

sqlSuccess
	^0!

sqlSuccessWithInfo
	^1!

sqlSuDmlStatements
	^self sqlOuDmlStatements!

sqlSuIndexDefinition
	^self sqlOuIndexDefinition!

sqlSuPrivilegeDefinition
	^self sqlOuPrivilegeDefinition!

sqlSuProcedureInvocation
	^self sqlOuProcedureInvocation!

sqlSuTableDefinition
	^self sqlOuTableDefinition!

sqlSveCase
	^16r00000001!

sqlSveCast
	^16r00000002!

sqlSveCoalesce
	^16r00000004!

sqlSveNullif
	^16r00000008!

sqlSystemFunctions
	^51!

sqlTableStat
	^0!

sqlTableTerm
	^45!

sqlTcAll
	^2!

sqlTcDdlCommit
	^3!

sqlTcDdlIgnore
	^4!

sqlTcDml
	^1!

sqlTcNone
	^0!

sqlTime
	^10!

sqlTimedateAddIntervals
	^109!

sqlTimedateDiffIntervals
	^110!

sqlTimedateFunctions
	^52!

sqlTimeLen
	^8!

sqlTimestamp
	^11!

sqlTimestampLen
	^19!

sqlTinyint
	^(-6)!

sqlTransactionCapable
	^self sqlTxnCapable!

sqlTransactionIsolationOption
	^self sqlTxnIsolationOption!

sqlTransactionReadCommitted
	^self sqlTxnReadCommitted!

sqlTransactionReadUncommitted
	^self sqlTxnReadUncommitted!

sqlTransactionRepeatableRead
	^self sqlTxnRepeatableRead!

sqlTransactionSerializable
	^self sqlTxnSerializable!

sqlTranslateDll
	^106!

sqlTranslateOption
	^107!

sqlTrue
	^1!

sqlTxnCapable
	^46!

sqlTxnIsolation
	^108!

sqlTxnIsolationOption
	^72!

sqlTxnReadCommitted
	^16r00000002!

sqlTxnReadUncommitted
	^16r00000001!

sqlTxnRepeatableRead
	^16r00000004!

sqlTxnSerializable
	^16r00000008!

sqlTxnVersioning
	^16r00000010!

sqlTypeDate
	^91!

sqlTypeDriverEnd
	^self sqlUnicodeLongvarchar!

sqlTypeDriverStart
	^self sqlIntervalYear!

sqlTypeMax
	^self sqlVarchar!

sqlTypeMin
	^self sqlBit!

sqlTypeNull
	^0!

sqlTypeTime
	^92!

sqlTypeTimestamp
	^93!

sqlUbDefault
	^self sqlUbOff!

sqlUbFixed
	^self sqlUbOn!

sqlUbOff
	^0!

sqlUbOn
	^01!

sqlUbVariable
	^2!

sqlUnbind
	^2!

sqlUnicode
	^(-95)!

sqlUnicodeChar
	^self sqlUnicode!

sqlUnicodeLongvarchar
	^(-97)!

sqlUnicodeVarchar
	^(-96)!

sqlUnion
	^96!

sqlUnionStatement
	^self sqlUnion!

sqlUnknownType
	^0!

sqlUnnamed
	^1!

sqlUnsearchable
	^0!

sqlUnsignedOffset
	^(-22)!

sqlUnspecified
	^0!

sqlUpdate
	^2!

sqlUpdateByBookmark
	^5!

sqlUseBookmarks
	^12!

sqlUserName
	^47!

sqlUsUnion
	^self sqlUUnion!

sqlUsUnionAll
	^self sqlUUnionAll!

sqlUUnion
	^16r00000001!

sqlUUnionAll
	^16r00000002!

sqlVarbinary
	^(-3)!

sqlVarchar
	^12!

sqlXopenCliYear
	^10000!

sqlYear
	^self sqlCodeYear!

sqlYearToMonth
	^self sqlCodeYearToMonth!

traceVersion
	^1000! !
!RioConstants categoriesFor: #odbcver!constants!public! !
!RioConstants categoriesFor: #sqlAccessibleProcedures!constants!public! !
!RioConstants categoriesFor: #sqlAccessibleTables!constants!public! !
!RioConstants categoriesFor: #sqlAccessMode!constants!public! !
!RioConstants categoriesFor: #sqlActiveConnections!constants!public! !
!RioConstants categoriesFor: #sqlActiveEnvironments!constants!public! !
!RioConstants categoriesFor: #sqlActiveStatements!constants!public! !
!RioConstants categoriesFor: #sqlAdAddConstraintDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlAdAddConstraintInitiallyDeferred!constants!public! !
!RioConstants categoriesFor: #sqlAdAddConstraintInitiallyImmediate!constants!public! !
!RioConstants categoriesFor: #sqlAdAddConstraintNonDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlAdAddDomainConstraint!constants!public! !
!RioConstants categoriesFor: #sqlAdAddDomainDefault!constants!public! !
!RioConstants categoriesFor: #sqlAdConstraintNameDefinition!constants!public! !
!RioConstants categoriesFor: #sqlAdd!constants!public! !
!RioConstants categoriesFor: #sqlAdDropDomainConstraint!constants!public! !
!RioConstants categoriesFor: #sqlAdDropDomainDefault!constants!public! !
!RioConstants categoriesFor: #sqlAfAll!constants!public! !
!RioConstants categoriesFor: #sqlAfAvg!constants!public! !
!RioConstants categoriesFor: #sqlAfCount!constants!public! !
!RioConstants categoriesFor: #sqlAfDistinct!constants!public! !
!RioConstants categoriesFor: #sqlAfMax!constants!public! !
!RioConstants categoriesFor: #sqlAfMin!constants!public! !
!RioConstants categoriesFor: #sqlAfSum!constants!public! !
!RioConstants categoriesFor: #sqlAggregateFunctions!constants!public! !
!RioConstants categoriesFor: #sqlAllCatalogs!constants!public! !
!RioConstants categoriesFor: #sqlAllExceptLike!constants!public! !
!RioConstants categoriesFor: #sqlAllSchemas!constants!public! !
!RioConstants categoriesFor: #sqlAllTableTypes!constants!public! !
!RioConstants categoriesFor: #sqlAllTypes!constants!public! !
!RioConstants categoriesFor: #sqlAlterDomain!constants!public! !
!RioConstants categoriesFor: #sqlAlterTable!constants!public! !
!RioConstants categoriesFor: #sqlAmConnection!constants!public! !
!RioConstants categoriesFor: #sqlAmNone!constants!public! !
!RioConstants categoriesFor: #sqlAmStatement!constants!public! !
!RioConstants categoriesFor: #sqlApiAllFunctions!constants!public! !
!RioConstants categoriesFor: #sqlApiLoadbyordinal!constants!public! !
!RioConstants categoriesFor: #sqlApiOdbc3AllFunctions!constants!public! !
!RioConstants categoriesFor: #sqlApiOdbc3AllFunctionsSize!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlallocconnect!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlallocenv!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlallochandle!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlallochandlestd!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlallocstmt!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlbindcol!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlbindparam!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlbindparameter!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlbrowseconnect!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlbulkoperations!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlcancel!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlclosecursor!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlcolattribute!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlcolattributes!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlcolumnprivileges!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlcolumns!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlconnect!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlcopydesc!constants!public! !
!RioConstants categoriesFor: #sqlApiSqldatasources!constants!public! !
!RioConstants categoriesFor: #sqlApiSqldescribecol!constants!public! !
!RioConstants categoriesFor: #sqlApiSqldescribeparam!constants!public! !
!RioConstants categoriesFor: #sqlApiSqldisconnect!constants!public! !
!RioConstants categoriesFor: #sqlApiSqldriverconnect!constants!public! !
!RioConstants categoriesFor: #sqlApiSqldrivers!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlendtran!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlerror!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlexecdirect!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlexecute!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlextendedfetch!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlfetch!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlfetchscroll!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlforeignkeys!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlfreeconnect!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlfreeenv!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlfreehandle!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlfreestmt!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetconnectattr!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetconnectoption!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetcursorname!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetdata!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetdescfield!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetdescrec!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetdiagfield!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetdiagrec!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetenvattr!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetfunctions!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetinfo!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetstmtattr!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgetstmtoption!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlgettypeinfo!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlmoreresults!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlnativesql!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlnumparams!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlnumresultcols!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlparamdata!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlparamoptions!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlprepare!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlprimarykeys!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlprocedurecolumns!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlprocedures!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlputdata!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlrowcount!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetconnectattr!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetconnectoption!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetcursorname!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetdescfield!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetdescrec!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetenvattr!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetparam!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetpos!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetscrolloptions!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetstmtattr!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlsetstmtoption!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlspecialcolumns!constants!public! !
!RioConstants categoriesFor: #sqlApiSqlstatistics!constants!public! !
!RioConstants categoriesFor: #sqlApiSqltableprivileges!constants!public! !
!RioConstants categoriesFor: #sqlApiSqltables!constants!public! !
!RioConstants categoriesFor: #sqlApiSqltransact!constants!public! !
!RioConstants categoriesFor: #sqlArdType!constants!public! !
!RioConstants categoriesFor: #sqlAsyncEnable!constants!public! !
!RioConstants categoriesFor: #sqlAsyncEnableDefault!constants!public! !
!RioConstants categoriesFor: #sqlAsyncEnableOff!constants!public! !
!RioConstants categoriesFor: #sqlAsyncEnableOn!constants!public! !
!RioConstants categoriesFor: #sqlAsyncMode!constants!public! !
!RioConstants categoriesFor: #sqlAtAddColumn!constants!public! !
!RioConstants categoriesFor: #sqlAtAddColumnCollation!constants!public! !
!RioConstants categoriesFor: #sqlAtAddColumnDefault!constants!public! !
!RioConstants categoriesFor: #sqlAtAddColumnSingle!constants!public! !
!RioConstants categoriesFor: #sqlAtAddConstraint!constants!public! !
!RioConstants categoriesFor: #sqlAtAddTableConstraint!constants!public! !
!RioConstants categoriesFor: #sqlAtConstraintDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlAtConstraintInitiallyDeferred!constants!public! !
!RioConstants categoriesFor: #sqlAtConstraintInitiallyImmediate!constants!public! !
!RioConstants categoriesFor: #sqlAtConstraintNameDefinition!constants!public! !
!RioConstants categoriesFor: #sqlAtConstraintNonDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlAtDropColumn!constants!public! !
!RioConstants categoriesFor: #sqlAtDropColumnCascade!constants!public! !
!RioConstants categoriesFor: #sqlAtDropColumnDefault!constants!public! !
!RioConstants categoriesFor: #sqlAtDropColumnRestrict!constants!public! !
!RioConstants categoriesFor: #sqlAtDropTableConstraintCascade!constants!public! !
!RioConstants categoriesFor: #sqlAtDropTableConstraintRestrict!constants!public! !
!RioConstants categoriesFor: #sqlAtSetColumnDefault!constants!public! !
!RioConstants categoriesFor: #sqlAttrAccessMode!constants!public! !
!RioConstants categoriesFor: #sqlAttrAppParamDesc!constants!public! !
!RioConstants categoriesFor: #sqlAttrAppRowDesc!constants!public! !
!RioConstants categoriesFor: #sqlAttrAsyncEnable!constants!public! !
!RioConstants categoriesFor: #sqlAttrAutocommit!constants!public! !
!RioConstants categoriesFor: #sqlAttrAutoIpd!constants!public! !
!RioConstants categoriesFor: #sqlAttrConcurrency!constants!public! !
!RioConstants categoriesFor: #sqlAttrConnectionPooling!constants!public! !
!RioConstants categoriesFor: #sqlAttrConnectionTimeout!constants!public! !
!RioConstants categoriesFor: #sqlAttrCpMatch!constants!public! !
!RioConstants categoriesFor: #sqlAttrCurrentCatalog!constants!public! !
!RioConstants categoriesFor: #sqlAttrCursorScrollable!constants!public! !
!RioConstants categoriesFor: #sqlAttrCursorSensitivity!constants!public! !
!RioConstants categoriesFor: #sqlAttrCursorType!constants!public! !
!RioConstants categoriesFor: #sqlAttrDisconnectBehavior!constants!public! !
!RioConstants categoriesFor: #sqlAttrEnableAutoIpd!constants!public! !
!RioConstants categoriesFor: #sqlAttrEnlistInDtc!constants!public! !
!RioConstants categoriesFor: #sqlAttrEnlistInXa!constants!public! !
!RioConstants categoriesFor: #sqlAttrFetchBookmarkPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrImpParamDesc!constants!public! !
!RioConstants categoriesFor: #sqlAttrImpRowDesc!constants!public! !
!RioConstants categoriesFor: #sqlAttrKeysetSize!constants!public! !
!RioConstants categoriesFor: #sqlAttrLoginTimeout!constants!public! !
!RioConstants categoriesFor: #sqlAttrMaxLength!constants!public! !
!RioConstants categoriesFor: #sqlAttrMaxRows!constants!public! !
!RioConstants categoriesFor: #sqlAttrMetadataId!constants!public! !
!RioConstants categoriesFor: #sqlAttrNoscan!constants!public! !
!RioConstants categoriesFor: #sqlAttrOdbcCursors!constants!public! !
!RioConstants categoriesFor: #sqlAttrOdbcVersion!constants!public! !
!RioConstants categoriesFor: #sqlAttrOutputNts!constants!public! !
!RioConstants categoriesFor: #sqlAttrPacketSize!constants!public! !
!RioConstants categoriesFor: #sqlAttrParamBindOffsetPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrParamBindType!constants!public! !
!RioConstants categoriesFor: #sqlAttrParamOperationPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrParamsetSize!constants!public! !
!RioConstants categoriesFor: #sqlAttrParamsProcessedPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrParamStatusPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrQueryTimeout!constants!public! !
!RioConstants categoriesFor: #sqlAttrQuietMode!constants!public! !
!RioConstants categoriesFor: #sqlAttrReadonly!constants!public! !
!RioConstants categoriesFor: #sqlAttrReadwriteUnknown!constants!public! !
!RioConstants categoriesFor: #sqlAttrRetrieveData!constants!public! !
!RioConstants categoriesFor: #sqlAttrRowArraySize!constants!public! !
!RioConstants categoriesFor: #sqlAttrRowBindOffsetPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrRowBindType!constants!public! !
!RioConstants categoriesFor: #sqlAttrRowNumber!constants!public! !
!RioConstants categoriesFor: #sqlAttrRowOperationPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrRowsFetchedPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrRowStatusPtr!constants!public! !
!RioConstants categoriesFor: #sqlAttrSimulateCursor!constants!public! !
!RioConstants categoriesFor: #sqlAttrTrace!constants!public! !
!RioConstants categoriesFor: #sqlAttrTracefile!constants!public! !
!RioConstants categoriesFor: #sqlAttrTranslateLib!constants!public! !
!RioConstants categoriesFor: #sqlAttrTranslateOption!constants!public! !
!RioConstants categoriesFor: #sqlAttrTxnIsolation!constants!public! !
!RioConstants categoriesFor: #sqlAttrUseBookmarks!constants!public! !
!RioConstants categoriesFor: #sqlAttrWrite!constants!public! !
!RioConstants categoriesFor: #sqlAutocommit!constants!public! !
!RioConstants categoriesFor: #sqlAutocommitDefault!constants!public! !
!RioConstants categoriesFor: #sqlAutocommitOff!constants!public! !
!RioConstants categoriesFor: #sqlAutocommitOn!constants!public! !
!RioConstants categoriesFor: #sqlBatchRowCount!constants!public! !
!RioConstants categoriesFor: #sqlBatchSupport!constants!public! !
!RioConstants categoriesFor: #sqlBestRowid!constants!public! !
!RioConstants categoriesFor: #sqlBigint!constants!public! !
!RioConstants categoriesFor: #sqlBinary!constants!public! !
!RioConstants categoriesFor: #sqlBindByColumn!constants!public! !
!RioConstants categoriesFor: #sqlBindType!constants!public! !
!RioConstants categoriesFor: #sqlBindTypeDefault!constants!public! !
!RioConstants categoriesFor: #sqlBit!constants!public! !
!RioConstants categoriesFor: #sqlBookmarkPersistence!constants!public! !
!RioConstants categoriesFor: #sqlBpClose!constants!public! !
!RioConstants categoriesFor: #sqlBpDelete!constants!public! !
!RioConstants categoriesFor: #sqlBpDrop!constants!public! !
!RioConstants categoriesFor: #sqlBpOtherHstmt!constants!public! !
!RioConstants categoriesFor: #sqlBpScroll!constants!public! !
!RioConstants categoriesFor: #sqlBpTransaction!constants!public! !
!RioConstants categoriesFor: #sqlBpUpdate!constants!public! !
!RioConstants categoriesFor: #sqlBrcExplicit!constants!public! !
!RioConstants categoriesFor: #sqlBrcProcedures!constants!public! !
!RioConstants categoriesFor: #sqlBrcRolledUp!constants!public! !
!RioConstants categoriesFor: #sqlBsRowCountExplicit!constants!public! !
!RioConstants categoriesFor: #sqlBsRowCountProc!constants!public! !
!RioConstants categoriesFor: #sqlBsSelectExplicit!constants!public! !
!RioConstants categoriesFor: #sqlBsSelectProc!constants!public! !
!RioConstants categoriesFor: #sqlCa1Absolute!constants!public! !
!RioConstants categoriesFor: #sqlCa1Bookmark!constants!public! !
!RioConstants categoriesFor: #sqlCa1BulkAdd!constants!public! !
!RioConstants categoriesFor: #sqlCa1BulkDeleteByBookmark!constants!public! !
!RioConstants categoriesFor: #sqlCa1BulkFetchByBookmark!constants!public! !
!RioConstants categoriesFor: #sqlCa1BulkUpdateByBookmark!constants!public! !
!RioConstants categoriesFor: #sqlCa1LockExclusive!constants!public! !
!RioConstants categoriesFor: #sqlCa1LockNoChange!constants!public! !
!RioConstants categoriesFor: #sqlCa1LockUnlock!constants!public! !
!RioConstants categoriesFor: #sqlCa1Next!constants!public! !
!RioConstants categoriesFor: #sqlCa1PosDelete!constants!public! !
!RioConstants categoriesFor: #sqlCa1PositionedDelete!constants!public! !
!RioConstants categoriesFor: #sqlCa1PositionedUpdate!constants!public! !
!RioConstants categoriesFor: #sqlCa1PosPosition!constants!public! !
!RioConstants categoriesFor: #sqlCa1PosRefresh!constants!public! !
!RioConstants categoriesFor: #sqlCa1PosUpdate!constants!public! !
!RioConstants categoriesFor: #sqlCa1Relative!constants!public! !
!RioConstants categoriesFor: #sqlCa1SelectForUpdate!constants!public! !
!RioConstants categoriesFor: #sqlCa2CrcApproximate!constants!public! !
!RioConstants categoriesFor: #sqlCa2CrcExact!constants!public! !
!RioConstants categoriesFor: #sqlCa2LockConcurrency!constants!public! !
!RioConstants categoriesFor: #sqlCa2MaxRowsAffectsAll!constants!public! !
!RioConstants categoriesFor: #sqlCa2MaxRowsCatalog!constants!public! !
!RioConstants categoriesFor: #sqlCa2MaxRowsDelete!constants!public! !
!RioConstants categoriesFor: #sqlCa2MaxRowsInsert!constants!public! !
!RioConstants categoriesFor: #sqlCa2MaxRowsSelect!constants!public! !
!RioConstants categoriesFor: #sqlCa2MaxRowsUpdate!constants!public! !
!RioConstants categoriesFor: #sqlCa2OptRowverConcurrency!constants!public! !
!RioConstants categoriesFor: #sqlCa2OptValuesConcurrency!constants!public! !
!RioConstants categoriesFor: #sqlCa2ReadOnlyConcurrency!constants!public! !
!RioConstants categoriesFor: #sqlCa2SensitivityAdditions!constants!public! !
!RioConstants categoriesFor: #sqlCa2SensitivityDeletions!constants!public! !
!RioConstants categoriesFor: #sqlCa2SensitivityUpdates!constants!public! !
!RioConstants categoriesFor: #sqlCa2SimulateNonUnique!constants!public! !
!RioConstants categoriesFor: #sqlCa2SimulateTryUnique!constants!public! !
!RioConstants categoriesFor: #sqlCa2SimulateUnique!constants!public! !
!RioConstants categoriesFor: #sqlCaConstraintDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlCaConstraintInitiallyDeferred!constants!public! !
!RioConstants categoriesFor: #sqlCaConstraintInitiallyImmediate!constants!public! !
!RioConstants categoriesFor: #sqlCaConstraintNonDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlCaCreateAssertion!constants!public! !
!RioConstants categoriesFor: #sqlCascade!constants!public! !
!RioConstants categoriesFor: #sqlCatalogLocation!constants!public! !
!RioConstants categoriesFor: #sqlCatalogName!constants!public! !
!RioConstants categoriesFor: #sqlCatalogNameSeparator!constants!public! !
!RioConstants categoriesFor: #sqlCatalogTerm!constants!public! !
!RioConstants categoriesFor: #sqlCatalogUsage!constants!public! !
!RioConstants categoriesFor: #sqlCbClose!constants!public! !
!RioConstants categoriesFor: #sqlCbDelete!constants!public! !
!RioConstants categoriesFor: #sqlCBinary!constants!public! !
!RioConstants categoriesFor: #sqlCBit!constants!public! !
!RioConstants categoriesFor: #sqlCbNonNull!constants!public! !
!RioConstants categoriesFor: #sqlCbNull!constants!public! !
!RioConstants categoriesFor: #sqlCBookmark!constants!public! !
!RioConstants categoriesFor: #sqlCbPreserve!constants!public! !
!RioConstants categoriesFor: #sqlCcClose!constants!public! !
!RioConstants categoriesFor: #sqlCcDelete!constants!public! !
!RioConstants categoriesFor: #sqlCChar!constants!public! !
!RioConstants categoriesFor: #sqlCcolCreateCollation!constants!public! !
!RioConstants categoriesFor: #sqlCcPreserve!constants!public! !
!RioConstants categoriesFor: #sqlCcsCollateClause!constants!public! !
!RioConstants categoriesFor: #sqlCcsCreateCharacterSet!constants!public! !
!RioConstants categoriesFor: #sqlCcsLimitedCollation!constants!public! !
!RioConstants categoriesFor: #sqlCDate!constants!public! !
!RioConstants categoriesFor: #sqlCDefault!constants!public! !
!RioConstants categoriesFor: #sqlCdoCollation!constants!public! !
!RioConstants categoriesFor: #sqlCdoConstraint!constants!public! !
!RioConstants categoriesFor: #sqlCdoConstraintDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlCdoConstraintInitiallyDeferred!constants!public! !
!RioConstants categoriesFor: #sqlCdoConstraintInitiallyImmediate!constants!public! !
!RioConstants categoriesFor: #sqlCdoConstraintNameDefinition!constants!public! !
!RioConstants categoriesFor: #sqlCdoConstraintNonDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlCdoCreateDomain!constants!public! !
!RioConstants categoriesFor: #sqlCdoDefault!constants!public! !
!RioConstants categoriesFor: #sqlCDouble!constants!public! !
!RioConstants categoriesFor: #sqlCFloat!constants!public! !
!RioConstants categoriesFor: #sqlChar!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalDay!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalDayToHour!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalDayToMinute!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalDayToSecond!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalHour!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalHourToMinute!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalHourToSecond!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalMinute!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalMinuteToSecond!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalMonth!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalSecond!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalYear!constants!public! !
!RioConstants categoriesFor: #sqlCIntervalYearToMonth!constants!public! !
!RioConstants categoriesFor: #sqlClEnd!constants!public! !
!RioConstants categoriesFor: #sqlCLong!constants!public! !
!RioConstants categoriesFor: #sqlClose!constants!public! !
!RioConstants categoriesFor: #sqlClStart!constants!public! !
!RioConstants categoriesFor: #sqlCnAny!constants!public! !
!RioConstants categoriesFor: #sqlCnDifferent!constants!public! !
!RioConstants categoriesFor: #sqlCnNone!constants!public! !
!RioConstants categoriesFor: #sqlCNumeric!constants!public! !
!RioConstants categoriesFor: #sqlCodeDate!constants!public! !
!RioConstants categoriesFor: #sqlCodeDay!constants!public! !
!RioConstants categoriesFor: #sqlCodeDayToHour!constants!public! !
!RioConstants categoriesFor: #sqlCodeDayToMinute!constants!public! !
!RioConstants categoriesFor: #sqlCodeDayToSecond!constants!public! !
!RioConstants categoriesFor: #sqlCodeHour!constants!public! !
!RioConstants categoriesFor: #sqlCodeHourToMinute!constants!public! !
!RioConstants categoriesFor: #sqlCodeHourToSecond!constants!public! !
!RioConstants categoriesFor: #sqlCodeMinute!constants!public! !
!RioConstants categoriesFor: #sqlCodeMinuteToSecond!constants!public! !
!RioConstants categoriesFor: #sqlCodeMonth!constants!public! !
!RioConstants categoriesFor: #sqlCodeSecond!constants!public! !
!RioConstants categoriesFor: #sqlCodeTime!constants!public! !
!RioConstants categoriesFor: #sqlCodeTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlCodeYear!constants!public! !
!RioConstants categoriesFor: #sqlCodeYearToMonth!constants!public! !
!RioConstants categoriesFor: #sqlColattOptMax!constants!public! !
!RioConstants categoriesFor: #sqlColattOptMin!constants!public! !
!RioConstants categoriesFor: #sqlCollationSeq!constants!public! !
!RioConstants categoriesFor: #sqlColPredBasic!constants!public! !
!RioConstants categoriesFor: #sqlColPredChar!constants!public! !
!RioConstants categoriesFor: #sqlColumnAlias!constants!public! !
!RioConstants categoriesFor: #sqlColumnAutoIncrement!constants!public! !
!RioConstants categoriesFor: #sqlColumnCaseSensitive!constants!public! !
!RioConstants categoriesFor: #sqlColumnCount!constants!public! !
!RioConstants categoriesFor: #sqlColumnDisplaySize!constants!public! !
!RioConstants categoriesFor: #sqlColumnDriverStart!constants!public! !
!RioConstants categoriesFor: #sqlColumnIgnore!constants!public! !
!RioConstants categoriesFor: #sqlColumnLabel!constants!public! !
!RioConstants categoriesFor: #sqlColumnLength!constants!public! !
!RioConstants categoriesFor: #sqlColumnMoney!constants!public! !
!RioConstants categoriesFor: #sqlColumnName!constants!public! !
!RioConstants categoriesFor: #sqlColumnNullable!constants!public! !
!RioConstants categoriesFor: #sqlColumnNumberUnknown!constants!public! !
!RioConstants categoriesFor: #sqlColumnOwnerName!constants!public! !
!RioConstants categoriesFor: #sqlColumnPrecision!constants!public! !
!RioConstants categoriesFor: #sqlColumnQualifierName!constants!public! !
!RioConstants categoriesFor: #sqlColumnScale!constants!public! !
!RioConstants categoriesFor: #sqlColumnSearchable!constants!public! !
!RioConstants categoriesFor: #sqlColumnTableName!constants!public! !
!RioConstants categoriesFor: #sqlColumnType!constants!public! !
!RioConstants categoriesFor: #sqlColumnTypeName!constants!public! !
!RioConstants categoriesFor: #sqlColumnUnsigned!constants!public! !
!RioConstants categoriesFor: #sqlColumnUpdatable!constants!public! !
!RioConstants categoriesFor: #sqlCommit!constants!public! !
!RioConstants categoriesFor: #sqlConcatNullBehavior!constants!public! !
!RioConstants categoriesFor: #sqlConcurDefault!constants!public! !
!RioConstants categoriesFor: #sqlConcurLock!constants!public! !
!RioConstants categoriesFor: #sqlConcurReadOnly!constants!public! !
!RioConstants categoriesFor: #sqlConcurrency!constants!public! !
!RioConstants categoriesFor: #sqlConcurRowver!constants!public! !
!RioConstants categoriesFor: #sqlConcurTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlConcurValues!constants!public! !
!RioConstants categoriesFor: #sqlConnectOptDrvrStart!constants!public! !
!RioConstants categoriesFor: #sqlConnOptMax!constants!public! !
!RioConstants categoriesFor: #sqlConnOptMin!constants!public! !
!RioConstants categoriesFor: #sqlConvertBigint!constants!public! !
!RioConstants categoriesFor: #sqlConvertBinary!constants!public! !
!RioConstants categoriesFor: #sqlConvertBit!constants!public! !
!RioConstants categoriesFor: #sqlConvertChar!constants!public! !
!RioConstants categoriesFor: #sqlConvertDate!constants!public! !
!RioConstants categoriesFor: #sqlConvertDecimal!constants!public! !
!RioConstants categoriesFor: #sqlConvertDouble!constants!public! !
!RioConstants categoriesFor: #sqlConvertFloat!constants!public! !
!RioConstants categoriesFor: #sqlConvertFunctions!constants!public! !
!RioConstants categoriesFor: #sqlConvertInteger!constants!public! !
!RioConstants categoriesFor: #sqlConvertIntervalDayTime!constants!public! !
!RioConstants categoriesFor: #sqlConvertIntervalYearMonth!constants!public! !
!RioConstants categoriesFor: #sqlConvertLongvarbinary!constants!public! !
!RioConstants categoriesFor: #sqlConvertLongvarchar!constants!public! !
!RioConstants categoriesFor: #sqlConvertNumeric!constants!public! !
!RioConstants categoriesFor: #sqlConvertReal!constants!public! !
!RioConstants categoriesFor: #sqlConvertSmallint!constants!public! !
!RioConstants categoriesFor: #sqlConvertTime!constants!public! !
!RioConstants categoriesFor: #sqlConvertTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlConvertTinyint!constants!public! !
!RioConstants categoriesFor: #sqlConvertVarbinary!constants!public! !
!RioConstants categoriesFor: #sqlConvertVarchar!constants!public! !
!RioConstants categoriesFor: #sqlConvertWchar!constants!public! !
!RioConstants categoriesFor: #sqlConvertWlongvarchar!constants!public! !
!RioConstants categoriesFor: #sqlConvertWvarchar!constants!public! !
!RioConstants categoriesFor: #sqlCorrelationName!constants!public! !
!RioConstants categoriesFor: #sqlCpDefault!constants!public! !
!RioConstants categoriesFor: #sqlCpMatchDefault!constants!public! !
!RioConstants categoriesFor: #sqlCpOff!constants!public! !
!RioConstants categoriesFor: #sqlCpOnePerDriver!constants!public! !
!RioConstants categoriesFor: #sqlCpOnePerHenv!constants!public! !
!RioConstants categoriesFor: #sqlCpRelaxedMatch!constants!public! !
!RioConstants categoriesFor: #sqlCpStrictMatch!constants!public! !
!RioConstants categoriesFor: #sqlCrClose!constants!public! !
!RioConstants categoriesFor: #sqlCrDelete!constants!public! !
!RioConstants categoriesFor: #sqlCreateAssertion!constants!public! !
!RioConstants categoriesFor: #sqlCreateCharacterSet!constants!public! !
!RioConstants categoriesFor: #sqlCreateCollation!constants!public! !
!RioConstants categoriesFor: #sqlCreateDomain!constants!public! !
!RioConstants categoriesFor: #sqlCreateSchema!constants!public! !
!RioConstants categoriesFor: #sqlCreateTable!constants!public! !
!RioConstants categoriesFor: #sqlCreateTranslation!constants!public! !
!RioConstants categoriesFor: #sqlCreateView!constants!public! !
!RioConstants categoriesFor: #sqlCrPreserve!constants!public! !
!RioConstants categoriesFor: #sqlCsAuthorization!constants!public! !
!RioConstants categoriesFor: #sqlCSbigint!constants!public! !
!RioConstants categoriesFor: #sqlCsCreateSchema!constants!public! !
!RioConstants categoriesFor: #sqlCsDefaultCharacterSet!constants!public! !
!RioConstants categoriesFor: #sqlCShort!constants!public! !
!RioConstants categoriesFor: #sqlCSlong!constants!public! !
!RioConstants categoriesFor: #sqlCSshort!constants!public! !
!RioConstants categoriesFor: #sqlCStinyint!constants!public! !
!RioConstants categoriesFor: #sqlCtColumnCollation!constants!public! !
!RioConstants categoriesFor: #sqlCtColumnConstraint!constants!public! !
!RioConstants categoriesFor: #sqlCtColumnDefault!constants!public! !
!RioConstants categoriesFor: #sqlCtCommitDelete!constants!public! !
!RioConstants categoriesFor: #sqlCtCommitPreserve!constants!public! !
!RioConstants categoriesFor: #sqlCtConstraintDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlCtConstraintInitiallyDeferred!constants!public! !
!RioConstants categoriesFor: #sqlCtConstraintInitiallyImmediate!constants!public! !
!RioConstants categoriesFor: #sqlCtConstraintNameDefinition!constants!public! !
!RioConstants categoriesFor: #sqlCtConstraintNonDeferrable!constants!public! !
!RioConstants categoriesFor: #sqlCtCreateTable!constants!public! !
!RioConstants categoriesFor: #sqlCtGlobalTemporary!constants!public! !
!RioConstants categoriesFor: #sqlCTime!constants!public! !
!RioConstants categoriesFor: #sqlCTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlCTinyint!constants!public! !
!RioConstants categoriesFor: #sqlCtLocalTemporary!constants!public! !
!RioConstants categoriesFor: #sqlCtrCreateTranslation!constants!public! !
!RioConstants categoriesFor: #sqlCtTableConstraint!constants!public! !
!RioConstants categoriesFor: #sqlCTypeDate!constants!public! !
!RioConstants categoriesFor: #sqlCTypeTime!constants!public! !
!RioConstants categoriesFor: #sqlCTypeTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlCUbigint!constants!public! !
!RioConstants categoriesFor: #sqlCuDmlStatements!constants!public! !
!RioConstants categoriesFor: #sqlCuIndexDefinition!constants!public! !
!RioConstants categoriesFor: #sqlCUlong!constants!public! !
!RioConstants categoriesFor: #sqlCuPrivilegeDefinition!constants!public! !
!RioConstants categoriesFor: #sqlCuProcedureInvocation!constants!public! !
!RioConstants categoriesFor: #sqlCurDefault!constants!public! !
!RioConstants categoriesFor: #sqlCurrentQualifier!constants!public! !
!RioConstants categoriesFor: #sqlCursorCommitBehavior!constants!public! !
!RioConstants categoriesFor: #sqlCursorDynamic!constants!public! !
!RioConstants categoriesFor: #sqlCursorForwardOnly!constants!public! !
!RioConstants categoriesFor: #sqlCursorKeysetDriven!constants!public! !
!RioConstants categoriesFor: #sqlCursorRollbackBehavior!constants!public! !
!RioConstants categoriesFor: #sqlCursorSensitivity!constants!public! !
!RioConstants categoriesFor: #sqlCursorStatic!constants!public! !
!RioConstants categoriesFor: #sqlCursorType!constants!public! !
!RioConstants categoriesFor: #sqlCursorTypeDefault!constants!public! !
!RioConstants categoriesFor: #sqlCurUseDriver!constants!public! !
!RioConstants categoriesFor: #sqlCurUseIfNeeded!constants!public! !
!RioConstants categoriesFor: #sqlCurUseOdbc!constants!public! !
!RioConstants categoriesFor: #sqlCUshort!constants!public! !
!RioConstants categoriesFor: #sqlCuTableDefinition!constants!public! !
!RioConstants categoriesFor: #sqlCUtinyint!constants!public! !
!RioConstants categoriesFor: #sqlCVarbookmark!constants!public! !
!RioConstants categoriesFor: #sqlCvCascaded!constants!public! !
!RioConstants categoriesFor: #sqlCvCheckOption!constants!public! !
!RioConstants categoriesFor: #sqlCvCreateView!constants!public! !
!RioConstants categoriesFor: #sqlCvLocal!constants!public! !
!RioConstants categoriesFor: #sqlCvtBigint!constants!public! !
!RioConstants categoriesFor: #sqlCvtBinary!constants!public! !
!RioConstants categoriesFor: #sqlCvtBit!constants!public! !
!RioConstants categoriesFor: #sqlCvtChar!constants!public! !
!RioConstants categoriesFor: #sqlCvtDate!constants!public! !
!RioConstants categoriesFor: #sqlCvtDecimal!constants!public! !
!RioConstants categoriesFor: #sqlCvtDouble!constants!public! !
!RioConstants categoriesFor: #sqlCvtFloat!constants!public! !
!RioConstants categoriesFor: #sqlCvtInteger!constants!public! !
!RioConstants categoriesFor: #sqlCvtIntervalDayTime!constants!public! !
!RioConstants categoriesFor: #sqlCvtIntervalYearMonth!constants!public! !
!RioConstants categoriesFor: #sqlCvtLongvarbinary!constants!public! !
!RioConstants categoriesFor: #sqlCvtLongvarchar!constants!public! !
!RioConstants categoriesFor: #sqlCvtNumeric!constants!public! !
!RioConstants categoriesFor: #sqlCvtReal!constants!public! !
!RioConstants categoriesFor: #sqlCvtSmallint!constants!public! !
!RioConstants categoriesFor: #sqlCvtTime!constants!public! !
!RioConstants categoriesFor: #sqlCvtTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlCvtTinyint!constants!public! !
!RioConstants categoriesFor: #sqlCvtVarbinary!constants!public! !
!RioConstants categoriesFor: #sqlCvtVarchar!constants!public! !
!RioConstants categoriesFor: #sqlCvtWchar!constants!public! !
!RioConstants categoriesFor: #sqlCvtWlongvarchar!constants!public! !
!RioConstants categoriesFor: #sqlCvtWvarchar!constants!public! !
!RioConstants categoriesFor: #sqlDaDropAssertion!constants!public! !
!RioConstants categoriesFor: #sqlDataAtExec!constants!public! !
!RioConstants categoriesFor: #sqlDatabaseName!constants!public! !
!RioConstants categoriesFor: #sqlDataSourceName!constants!public! !
!RioConstants categoriesFor: #sqlDataSourceReadOnly!constants!public! !
!RioConstants categoriesFor: #sqlDate!constants!public! !
!RioConstants categoriesFor: #sqlDateLen!constants!public! !
!RioConstants categoriesFor: #sqlDatetime!constants!public! !
!RioConstants categoriesFor: #sqlDatetimeLiterals!constants!public! !
!RioConstants categoriesFor: #sqlDay!constants!public! !
!RioConstants categoriesFor: #sqlDayToHour!constants!public! !
!RioConstants categoriesFor: #sqlDayToMinute!constants!public! !
!RioConstants categoriesFor: #sqlDayToSecond!constants!public! !
!RioConstants categoriesFor: #sqlDbDefault!constants!public! !
!RioConstants categoriesFor: #sqlDbDisconnect!constants!public! !
!RioConstants categoriesFor: #sqlDbmsName!constants!public! !
!RioConstants categoriesFor: #sqlDbmsVer!constants!public! !
!RioConstants categoriesFor: #sqlDbReturnToPool!constants!public! !
!RioConstants categoriesFor: #sqlDcDropCollation!constants!public! !
!RioConstants categoriesFor: #sqlDcsDropCharacterSet!constants!public! !
!RioConstants categoriesFor: #sqlDdCascade!constants!public! !
!RioConstants categoriesFor: #sqlDdDropDomain!constants!public! !
!RioConstants categoriesFor: #sqlDdlIndex!constants!public! !
!RioConstants categoriesFor: #sqlDdRestrict!constants!public! !
!RioConstants categoriesFor: #sqlDecimal!constants!public! !
!RioConstants categoriesFor: #sqlDefault!constants!public! !
!RioConstants categoriesFor: #sqlDefaultParam!constants!public! !
!RioConstants categoriesFor: #sqlDefaultTxnIsolation!constants!public! !
!RioConstants categoriesFor: #sqlDelete!constants!public! !
!RioConstants categoriesFor: #sqlDeleteByBookmark!constants!public! !
!RioConstants categoriesFor: #sqlDescAllocAuto!constants!public! !
!RioConstants categoriesFor: #sqlDescAllocType!constants!public! !
!RioConstants categoriesFor: #sqlDescAllocUser!constants!public! !
!RioConstants categoriesFor: #sqlDescArraySize!constants!public! !
!RioConstants categoriesFor: #sqlDescArrayStatusPtr!constants!public! !
!RioConstants categoriesFor: #sqlDescAutoUniqueValue!constants!public! !
!RioConstants categoriesFor: #sqlDescBaseColumnName!constants!public! !
!RioConstants categoriesFor: #sqlDescBaseTableName!constants!public! !
!RioConstants categoriesFor: #sqlDescBindOffsetPtr!constants!public! !
!RioConstants categoriesFor: #sqlDescBindType!constants!public! !
!RioConstants categoriesFor: #sqlDescCaseSensitive!constants!public! !
!RioConstants categoriesFor: #sqlDescCatalogName!constants!public! !
!RioConstants categoriesFor: #sqlDescConciseType!constants!public! !
!RioConstants categoriesFor: #sqlDescCount!constants!public! !
!RioConstants categoriesFor: #sqlDescDataPtr!constants!public! !
!RioConstants categoriesFor: #sqlDescDatetimeIntervalCode!constants!public! !
!RioConstants categoriesFor: #sqlDescDatetimeIntervalPrecision!constants!public! !
!RioConstants categoriesFor: #sqlDescDisplaySize!constants!public! !
!RioConstants categoriesFor: #sqlDescFixedPrecScale!constants!public! !
!RioConstants categoriesFor: #sqlDescIndicatorPtr!constants!public! !
!RioConstants categoriesFor: #sqlDescLabel!constants!public! !
!RioConstants categoriesFor: #sqlDescLength!constants!public! !
!RioConstants categoriesFor: #sqlDescLiteralPrefix!constants!public! !
!RioConstants categoriesFor: #sqlDescLiteralSuffix!constants!public! !
!RioConstants categoriesFor: #sqlDescLocalTypeName!constants!public! !
!RioConstants categoriesFor: #sqlDescMaximumScale!constants!public! !
!RioConstants categoriesFor: #sqlDescMinimumScale!constants!public! !
!RioConstants categoriesFor: #sqlDescName!constants!public! !
!RioConstants categoriesFor: #sqlDescNullable!constants!public! !
!RioConstants categoriesFor: #sqlDescNumPrecRadix!constants!public! !
!RioConstants categoriesFor: #sqlDescOctetLength!constants!public! !
!RioConstants categoriesFor: #sqlDescOctetLengthPtr!constants!public! !
!RioConstants categoriesFor: #sqlDescParameterType!constants!public! !
!RioConstants categoriesFor: #sqlDescPrecision!constants!public! !
!RioConstants categoriesFor: #sqlDescribeParameter!constants!public! !
!RioConstants categoriesFor: #sqlDescRowsProcessedPtr!constants!public! !
!RioConstants categoriesFor: #sqlDescScale!constants!public! !
!RioConstants categoriesFor: #sqlDescSchemaName!constants!public! !
!RioConstants categoriesFor: #sqlDescSearchable!constants!public! !
!RioConstants categoriesFor: #sqlDescTableName!constants!public! !
!RioConstants categoriesFor: #sqlDescType!constants!public! !
!RioConstants categoriesFor: #sqlDescTypeName!constants!public! !
!RioConstants categoriesFor: #sqlDescUnnamed!constants!public! !
!RioConstants categoriesFor: #sqlDescUnsigned!constants!public! !
!RioConstants categoriesFor: #sqlDescUpdatable!constants!public! !
!RioConstants categoriesFor: #sqlDiagAlterTable!constants!public! !
!RioConstants categoriesFor: #sqlDiagCall!constants!public! !
!RioConstants categoriesFor: #sqlDiagClassOrigin!constants!public! !
!RioConstants categoriesFor: #sqlDiagColumnNumber!constants!public! !
!RioConstants categoriesFor: #sqlDiagConnectionName!constants!public! !
!RioConstants categoriesFor: #sqlDiagCreateIndex!constants!public! !
!RioConstants categoriesFor: #sqlDiagCreateTable!constants!public! !
!RioConstants categoriesFor: #sqlDiagCreateView!constants!public! !
!RioConstants categoriesFor: #sqlDiagCursorRowCount!constants!public! !
!RioConstants categoriesFor: #sqlDiagDeleteWhere!constants!public! !
!RioConstants categoriesFor: #sqlDiagDropIndex!constants!public! !
!RioConstants categoriesFor: #sqlDiagDropTable!constants!public! !
!RioConstants categoriesFor: #sqlDiagDropView!constants!public! !
!RioConstants categoriesFor: #sqlDiagDynamicDeleteCursor!constants!public! !
!RioConstants categoriesFor: #sqlDiagDynamicFunction!constants!public! !
!RioConstants categoriesFor: #sqlDiagDynamicFunctionCode!constants!public! !
!RioConstants categoriesFor: #sqlDiagDynamicUpdateCursor!constants!public! !
!RioConstants categoriesFor: #sqlDiagGrant!constants!public! !
!RioConstants categoriesFor: #sqlDiagInsert!constants!public! !
!RioConstants categoriesFor: #sqlDiagMessageText!constants!public! !
!RioConstants categoriesFor: #sqlDiagNative!constants!public! !
!RioConstants categoriesFor: #sqlDiagNumber!constants!public! !
!RioConstants categoriesFor: #sqlDiagReturncode!constants!public! !
!RioConstants categoriesFor: #sqlDiagRevoke!constants!public! !
!RioConstants categoriesFor: #sqlDiagRowCount!constants!public! !
!RioConstants categoriesFor: #sqlDiagRowNumber!constants!public! !
!RioConstants categoriesFor: #sqlDiagSelectCursor!constants!public! !
!RioConstants categoriesFor: #sqlDiagServerName!constants!public! !
!RioConstants categoriesFor: #sqlDiagSqlstate!constants!public! !
!RioConstants categoriesFor: #sqlDiagSubclassOrigin!constants!public! !
!RioConstants categoriesFor: #sqlDiagUnknownStatement!constants!public! !
!RioConstants categoriesFor: #sqlDiagUpdateWhere!constants!public! !
!RioConstants categoriesFor: #sqlDiCreateIndex!constants!public! !
!RioConstants categoriesFor: #sqlDiDropIndex!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92Date!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalDay!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalDayToHour!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalDayToMinute!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalDayToSecond!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalHour!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalHourToMinute!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalHourToSecond!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalMinute!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalMinuteToSecond!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalMonth!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalSecond!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalYear!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92IntervalYearToMonth!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92Time!constants!public! !
!RioConstants categoriesFor: #sqlDlSql92Timestamp!constants!public! !
!RioConstants categoriesFor: #sqlDmVer!constants!public! !
!RioConstants categoriesFor: #sqlDouble!constants!public! !
!RioConstants categoriesFor: #sqlDriverComplete!constants!public! !
!RioConstants categoriesFor: #sqlDriverCompleteRequired!constants!public! !
!RioConstants categoriesFor: #sqlDriverHdbc!constants!public! !
!RioConstants categoriesFor: #sqlDriverHdesc!constants!public! !
!RioConstants categoriesFor: #sqlDriverHenv!constants!public! !
!RioConstants categoriesFor: #sqlDriverHlib!constants!public! !
!RioConstants categoriesFor: #sqlDriverHstmt!constants!public! !
!RioConstants categoriesFor: #sqlDriverName!constants!public! !
!RioConstants categoriesFor: #sqlDriverNoprompt!constants!public! !
!RioConstants categoriesFor: #sqlDriverOdbcVer!constants!public! !
!RioConstants categoriesFor: #sqlDriverPrompt!constants!public! !
!RioConstants categoriesFor: #sqlDriverVer!constants!public! !
!RioConstants categoriesFor: #sqlDrop!constants!public! !
!RioConstants categoriesFor: #sqlDropAssertion!constants!public! !
!RioConstants categoriesFor: #sqlDropCharacterSet!constants!public! !
!RioConstants categoriesFor: #sqlDropCollation!constants!public! !
!RioConstants categoriesFor: #sqlDropDomain!constants!public! !
!RioConstants categoriesFor: #sqlDropSchema!constants!public! !
!RioConstants categoriesFor: #sqlDropTable!constants!public! !
!RioConstants categoriesFor: #sqlDropTranslation!constants!public! !
!RioConstants categoriesFor: #sqlDropView!constants!public! !
!RioConstants categoriesFor: #sqlDsCascade!constants!public! !
!RioConstants categoriesFor: #sqlDsDropSchema!constants!public! !
!RioConstants categoriesFor: #sqlDsRestrict!constants!public! !
!RioConstants categoriesFor: #sqlDtCascade!constants!public! !
!RioConstants categoriesFor: #sqlDtcDone!constants!public! !
!RioConstants categoriesFor: #sqlDtDropTable!constants!public! !
!RioConstants categoriesFor: #sqlDtrDropTranslation!constants!public! !
!RioConstants categoriesFor: #sqlDtRestrict!constants!public! !
!RioConstants categoriesFor: #sqlDvCascade!constants!public! !
!RioConstants categoriesFor: #sqlDvDropView!constants!public! !
!RioConstants categoriesFor: #sqlDvRestrict!constants!public! !
!RioConstants categoriesFor: #sqlDynamicCursorAttributes1!constants!public! !
!RioConstants categoriesFor: #sqlDynamicCursorAttributes2!constants!public! !
!RioConstants categoriesFor: #sqlEnsure!constants!public! !
!RioConstants categoriesFor: #sqlEntireRowset!constants!public! !
!RioConstants categoriesFor: #sqlError!constants!public! !
!RioConstants categoriesFor: #sqlExpressionsInOrderby!constants!public! !
!RioConstants categoriesFor: #sqlExtApiLast!constants!public! !
!RioConstants categoriesFor: #sqlExtApiStart!constants!public! !
!RioConstants categoriesFor: #sqlFalse!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchAbsolute!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchBookmark!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchFirst!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchLast!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchNext!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchPrev!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchPrior!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchRelative!constants!public! !
!RioConstants categoriesFor: #sqlFdFetchResume!constants!public! !
!RioConstants categoriesFor: #sqlFetchAbsolute!constants!public! !
!RioConstants categoriesFor: #sqlFetchBookmark!constants!public! !
!RioConstants categoriesFor: #sqlFetchByBookmark!constants!public! !
!RioConstants categoriesFor: #sqlFetchDirection!constants!public! !
!RioConstants categoriesFor: #sqlFetchFirst!constants!public! !
!RioConstants categoriesFor: #sqlFetchFirstSystem!constants!public! !
!RioConstants categoriesFor: #sqlFetchFirstUser!constants!public! !
!RioConstants categoriesFor: #sqlFetchLast!constants!public! !
!RioConstants categoriesFor: #sqlFetchNext!constants!public! !
!RioConstants categoriesFor: #sqlFetchPrev!constants!public! !
!RioConstants categoriesFor: #sqlFetchPrior!constants!public! !
!RioConstants categoriesFor: #sqlFetchRelative!constants!public! !
!RioConstants categoriesFor: #sqlFetchResume!constants!public! !
!RioConstants categoriesFor: #sqlFileCatalog!constants!public! !
!RioConstants categoriesFor: #sqlFileNotSupported!constants!public! !
!RioConstants categoriesFor: #sqlFileQualifier!constants!public! !
!RioConstants categoriesFor: #sqlFileTable!constants!public! !
!RioConstants categoriesFor: #sqlFileUsage!constants!public! !
!RioConstants categoriesFor: #sqlFloat!constants!public! !
!RioConstants categoriesFor: #sqlFnCvtCast!constants!public! !
!RioConstants categoriesFor: #sqlFnCvtConvert!constants!public! !
!RioConstants categoriesFor: #sqlFnNumAbs!constants!public! !
!RioConstants categoriesFor: #sqlFnNumAcos!constants!public! !
!RioConstants categoriesFor: #sqlFnNumAsin!constants!public! !
!RioConstants categoriesFor: #sqlFnNumAtan!constants!public! !
!RioConstants categoriesFor: #sqlFnNumAtan2!constants!public! !
!RioConstants categoriesFor: #sqlFnNumCeiling!constants!public! !
!RioConstants categoriesFor: #sqlFnNumCos!constants!public! !
!RioConstants categoriesFor: #sqlFnNumCot!constants!public! !
!RioConstants categoriesFor: #sqlFnNumDegrees!constants!public! !
!RioConstants categoriesFor: #sqlFnNumExp!constants!public! !
!RioConstants categoriesFor: #sqlFnNumFloor!constants!public! !
!RioConstants categoriesFor: #sqlFnNumLog!constants!public! !
!RioConstants categoriesFor: #sqlFnNumLog10!constants!public! !
!RioConstants categoriesFor: #sqlFnNumMod!constants!public! !
!RioConstants categoriesFor: #sqlFnNumPi!constants!public! !
!RioConstants categoriesFor: #sqlFnNumPower!constants!public! !
!RioConstants categoriesFor: #sqlFnNumRadians!constants!public! !
!RioConstants categoriesFor: #sqlFnNumRand!constants!public! !
!RioConstants categoriesFor: #sqlFnNumRound!constants!public! !
!RioConstants categoriesFor: #sqlFnNumSign!constants!public! !
!RioConstants categoriesFor: #sqlFnNumSin!constants!public! !
!RioConstants categoriesFor: #sqlFnNumSqrt!constants!public! !
!RioConstants categoriesFor: #sqlFnNumTan!constants!public! !
!RioConstants categoriesFor: #sqlFnNumTruncate!constants!public! !
!RioConstants categoriesFor: #sqlFnStrAscii!constants!public! !
!RioConstants categoriesFor: #sqlFnStrBitLength!constants!public! !
!RioConstants categoriesFor: #sqlFnStrChar!constants!public! !
!RioConstants categoriesFor: #sqlFnStrCharacterLength!constants!public! !
!RioConstants categoriesFor: #sqlFnStrCharLength!constants!public! !
!RioConstants categoriesFor: #sqlFnStrConcat!constants!public! !
!RioConstants categoriesFor: #sqlFnStrDifference!constants!public! !
!RioConstants categoriesFor: #sqlFnStrInsert!constants!public! !
!RioConstants categoriesFor: #sqlFnStrLcase!constants!public! !
!RioConstants categoriesFor: #sqlFnStrLeft!constants!public! !
!RioConstants categoriesFor: #sqlFnStrLength!constants!public! !
!RioConstants categoriesFor: #sqlFnStrLocate!constants!public! !
!RioConstants categoriesFor: #sqlFnStrLocate2!constants!public! !
!RioConstants categoriesFor: #sqlFnStrLtrim!constants!public! !
!RioConstants categoriesFor: #sqlFnStrOctetLength!constants!public! !
!RioConstants categoriesFor: #sqlFnStrPosition!constants!public! !
!RioConstants categoriesFor: #sqlFnStrRepeat!constants!public! !
!RioConstants categoriesFor: #sqlFnStrReplace!constants!public! !
!RioConstants categoriesFor: #sqlFnStrRight!constants!public! !
!RioConstants categoriesFor: #sqlFnStrRtrim!constants!public! !
!RioConstants categoriesFor: #sqlFnStrSoundex!constants!public! !
!RioConstants categoriesFor: #sqlFnStrSpace!constants!public! !
!RioConstants categoriesFor: #sqlFnStrSubstring!constants!public! !
!RioConstants categoriesFor: #sqlFnStrUcase!constants!public! !
!RioConstants categoriesFor: #sqlFnSysDbname!constants!public! !
!RioConstants categoriesFor: #sqlFnSysIfnull!constants!public! !
!RioConstants categoriesFor: #sqlFnSysUsername!constants!public! !
!RioConstants categoriesFor: #sqlFnTdCurdate!constants!public! !
!RioConstants categoriesFor: #sqlFnTdCurrentDate!constants!public! !
!RioConstants categoriesFor: #sqlFnTdCurrentTime!constants!public! !
!RioConstants categoriesFor: #sqlFnTdCurrentTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlFnTdCurtime!constants!public! !
!RioConstants categoriesFor: #sqlFnTdDayname!constants!public! !
!RioConstants categoriesFor: #sqlFnTdDayofmonth!constants!public! !
!RioConstants categoriesFor: #sqlFnTdDayofweek!constants!public! !
!RioConstants categoriesFor: #sqlFnTdDayofyear!constants!public! !
!RioConstants categoriesFor: #sqlFnTdExtract!constants!public! !
!RioConstants categoriesFor: #sqlFnTdHour!constants!public! !
!RioConstants categoriesFor: #sqlFnTdMinute!constants!public! !
!RioConstants categoriesFor: #sqlFnTdMonth!constants!public! !
!RioConstants categoriesFor: #sqlFnTdMonthname!constants!public! !
!RioConstants categoriesFor: #sqlFnTdNow!constants!public! !
!RioConstants categoriesFor: #sqlFnTdQuarter!constants!public! !
!RioConstants categoriesFor: #sqlFnTdSecond!constants!public! !
!RioConstants categoriesFor: #sqlFnTdTimestampadd!constants!public! !
!RioConstants categoriesFor: #sqlFnTdTimestampdiff!constants!public! !
!RioConstants categoriesFor: #sqlFnTdWeek!constants!public! !
!RioConstants categoriesFor: #sqlFnTdYear!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiDay!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiFracSecond!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiHour!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiMinute!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiMonth!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiQuarter!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiSecond!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiWeek!constants!public! !
!RioConstants categoriesFor: #sqlFnTsiYear!constants!public! !
!RioConstants categoriesFor: #sqlForwardOnlyCursorAttributes1!constants!public! !
!RioConstants categoriesFor: #sqlForwardOnlyCursorAttributes2!constants!public! !
!RioConstants categoriesFor: #sqlGbCollate!constants!public! !
!RioConstants categoriesFor: #sqlGbGroupByContainsSelect!constants!public! !
!RioConstants categoriesFor: #sqlGbGroupByEqualsSelect!constants!public! !
!RioConstants categoriesFor: #sqlGbNoRelation!constants!public! !
!RioConstants categoriesFor: #sqlGbNotSupported!constants!public! !
!RioConstants categoriesFor: #sqlGdAnyColumn!constants!public! !
!RioConstants categoriesFor: #sqlGdAnyOrder!constants!public! !
!RioConstants categoriesFor: #sqlGdBlock!constants!public! !
!RioConstants categoriesFor: #sqlGdBound!constants!public! !
!RioConstants categoriesFor: #sqlGetBookmark!constants!public! !
!RioConstants categoriesFor: #sqlGetdataExtensions!constants!public! !
!RioConstants categoriesFor: #sqlGroupBy!constants!public! !
!RioConstants categoriesFor: #sqlHandleDbc!constants!public! !
!RioConstants categoriesFor: #sqlHandleDesc!constants!public! !
!RioConstants categoriesFor: #sqlHandleEnv!constants!public! !
!RioConstants categoriesFor: #sqlHandleSenv!constants!public! !
!RioConstants categoriesFor: #sqlHandleStmt!constants!public! !
!RioConstants categoriesFor: #sqlHour!constants!public! !
!RioConstants categoriesFor: #sqlHourToMinute!constants!public! !
!RioConstants categoriesFor: #sqlHourToSecond!constants!public! !
!RioConstants categoriesFor: #sqlIcLower!constants!public! !
!RioConstants categoriesFor: #sqlIcMixed!constants!public! !
!RioConstants categoriesFor: #sqlIcSensitive!constants!public! !
!RioConstants categoriesFor: #sqlIcUpper!constants!public! !
!RioConstants categoriesFor: #sqlIdentifierCase!constants!public! !
!RioConstants categoriesFor: #sqlIdentifierQuoteChar!constants!public! !
!RioConstants categoriesFor: #sqlIgnore!constants!public! !
!RioConstants categoriesFor: #sqlIkAll!constants!public! !
!RioConstants categoriesFor: #sqlIkAsc!constants!public! !
!RioConstants categoriesFor: #sqlIkDesc!constants!public! !
!RioConstants categoriesFor: #sqlIkNone!constants!public! !
!RioConstants categoriesFor: #sqlIndexAll!constants!public! !
!RioConstants categoriesFor: #sqlIndexClustered!constants!public! !
!RioConstants categoriesFor: #sqlIndexHashed!constants!public! !
!RioConstants categoriesFor: #sqlIndexKeywords!constants!public! !
!RioConstants categoriesFor: #sqlIndexOther!constants!public! !
!RioConstants categoriesFor: #sqlIndexUnique!constants!public! !
!RioConstants categoriesFor: #sqlInfoDriverStart!constants!public! !
!RioConstants categoriesFor: #sqlInfoFirst!constants!public! !
!RioConstants categoriesFor: #sqlInfoLast!constants!public! !
!RioConstants categoriesFor: #sqlInfoSchemaViews!constants!public! !
!RioConstants categoriesFor: #sqlInitiallyDeferred!constants!public! !
!RioConstants categoriesFor: #sqlInitiallyImmediate!constants!public! !
!RioConstants categoriesFor: #sqlInsensitive!constants!public! !
!RioConstants categoriesFor: #sqlInsertStatement!constants!public! !
!RioConstants categoriesFor: #sqlInteger!constants!public! !
!RioConstants categoriesFor: #sqlIntegrity!constants!public! !
!RioConstants categoriesFor: #sqlInterval!constants!public! !
!RioConstants categoriesFor: #sqlIntervalDay!constants!public! !
!RioConstants categoriesFor: #sqlIntervalDayToHour!constants!public! !
!RioConstants categoriesFor: #sqlIntervalDayToMinute!constants!public! !
!RioConstants categoriesFor: #sqlIntervalDayToSecond!constants!public! !
!RioConstants categoriesFor: #sqlIntervalHour!constants!public! !
!RioConstants categoriesFor: #sqlIntervalHourToMinute!constants!public! !
!RioConstants categoriesFor: #sqlIntervalHourToSecond!constants!public! !
!RioConstants categoriesFor: #sqlIntervalMinute!constants!public! !
!RioConstants categoriesFor: #sqlIntervalMinuteToSecond!constants!public! !
!RioConstants categoriesFor: #sqlIntervalMonth!constants!public! !
!RioConstants categoriesFor: #sqlIntervalSecond!constants!public! !
!RioConstants categoriesFor: #sqlIntervalYear!constants!public! !
!RioConstants categoriesFor: #sqlIntervalYearToMonth!constants!public! !
!RioConstants categoriesFor: #sqlInvalidHandle!constants!public! !
!RioConstants categoriesFor: #sqlIsInsertLiterals!constants!public! !
!RioConstants categoriesFor: #sqlIsInsertSearched!constants!public! !
!RioConstants categoriesFor: #sqlIsInteger!constants!public! !
!RioConstants categoriesFor: #sqlIsPointer!constants!public! !
!RioConstants categoriesFor: #sqlIsSelectInto!constants!public! !
!RioConstants categoriesFor: #sqlIsSmallint!constants!public! !
!RioConstants categoriesFor: #sqlIsUinteger!constants!public! !
!RioConstants categoriesFor: #sqlIsUsmallint!constants!public! !
!RioConstants categoriesFor: #sqlIsvAssertions!constants!public! !
!RioConstants categoriesFor: #sqlIsvCharacterSets!constants!public! !
!RioConstants categoriesFor: #sqlIsvCheckConstraints!constants!public! !
!RioConstants categoriesFor: #sqlIsvCollations!constants!public! !
!RioConstants categoriesFor: #sqlIsvColumnDomainUsage!constants!public! !
!RioConstants categoriesFor: #sqlIsvColumnPrivileges!constants!public! !
!RioConstants categoriesFor: #sqlIsvColumns!constants!public! !
!RioConstants categoriesFor: #sqlIsvConstraintColumnUsage!constants!public! !
!RioConstants categoriesFor: #sqlIsvConstraintTableUsage!constants!public! !
!RioConstants categoriesFor: #sqlIsvDomainConstraints!constants!public! !
!RioConstants categoriesFor: #sqlIsvDomains!constants!public! !
!RioConstants categoriesFor: #sqlIsvKeyColumnUsage!constants!public! !
!RioConstants categoriesFor: #sqlIsvReferentialConstraints!constants!public! !
!RioConstants categoriesFor: #sqlIsvSchemata!constants!public! !
!RioConstants categoriesFor: #sqlIsvSqlLanguages!constants!public! !
!RioConstants categoriesFor: #sqlIsvTableConstraints!constants!public! !
!RioConstants categoriesFor: #sqlIsvTablePrivileges!constants!public! !
!RioConstants categoriesFor: #sqlIsvTables!constants!public! !
!RioConstants categoriesFor: #sqlIsvTranslations!constants!public! !
!RioConstants categoriesFor: #sqlIsvUsagePrivileges!constants!public! !
!RioConstants categoriesFor: #sqlIsvViewColumnUsage!constants!public! !
!RioConstants categoriesFor: #sqlIsvViews!constants!public! !
!RioConstants categoriesFor: #sqlIsvViewTableUsage!constants!public! !
!RioConstants categoriesFor: #sqlKeysetCursorAttributes1!constants!public! !
!RioConstants categoriesFor: #sqlKeysetCursorAttributes2!constants!public! !
!RioConstants categoriesFor: #sqlKeysetSize!constants!public! !
!RioConstants categoriesFor: #sqlKeysetSizeDefault!constants!public! !
!RioConstants categoriesFor: #sqlKeywords!constants!public! !
!RioConstants categoriesFor: #sqlLckExclusive!constants!public! !
!RioConstants categoriesFor: #sqlLckNoChange!constants!public! !
!RioConstants categoriesFor: #sqlLckUnlock!constants!public! !
!RioConstants categoriesFor: #sqlLenBinaryAttr:!constants!public! !
!RioConstants categoriesFor: #sqlLenBinaryAttrOffset!constants!public! !
!RioConstants categoriesFor: #sqlLenDataAtExec:!constants!public! !
!RioConstants categoriesFor: #sqlLenDataAtExecOffset!constants!public! !
!RioConstants categoriesFor: #sqlLikeEscapeClause!constants!public! !
!RioConstants categoriesFor: #sqlLikeOnly!constants!public! !
!RioConstants categoriesFor: #sqlLockExclusive!constants!public! !
!RioConstants categoriesFor: #sqlLockNoChange!constants!public! !
!RioConstants categoriesFor: #sqlLockTypes!constants!public! !
!RioConstants categoriesFor: #sqlLockUnlock!constants!public! !
!RioConstants categoriesFor: #sqlLoginTimeout!constants!public! !
!RioConstants categoriesFor: #sqlLoginTimeoutDefault!constants!public! !
!RioConstants categoriesFor: #sqlLongvarbinary!constants!public! !
!RioConstants categoriesFor: #sqlLongvarchar!constants!public! !
!RioConstants categoriesFor: #sqlMaxAsyncConcurrentStatements!constants!public! !
!RioConstants categoriesFor: #sqlMaxBinaryLiteralLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxCatalogNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxCharLiteralLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxColumnNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxColumnsInGroupBy!constants!public! !
!RioConstants categoriesFor: #sqlMaxColumnsInIndex!constants!public! !
!RioConstants categoriesFor: #sqlMaxColumnsInOrderBy!constants!public! !
!RioConstants categoriesFor: #sqlMaxColumnsInSelect!constants!public! !
!RioConstants categoriesFor: #sqlMaxColumnsInTable!constants!public! !
!RioConstants categoriesFor: #sqlMaxConcurrentActivities!constants!public! !
!RioConstants categoriesFor: #sqlMaxCursorNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxDriverConnections!constants!public! !
!RioConstants categoriesFor: #sqlMaxDsnLength!constants!public! !
!RioConstants categoriesFor: #sqlMaxIdentifierLen!constants!public! !
!RioConstants categoriesFor: #sqlMaximumCatalogNameLength!constants!public! !
!RioConstants categoriesFor: #sqlMaximumColumnNameLength!constants!public! !
!RioConstants categoriesFor: #sqlMaximumColumnsInGroupBy!constants!public! !
!RioConstants categoriesFor: #sqlMaximumColumnsInIndex!constants!public! !
!RioConstants categoriesFor: #sqlMaximumColumnsInOrderBy!constants!public! !
!RioConstants categoriesFor: #sqlMaximumColumnsInSelect!constants!public! !
!RioConstants categoriesFor: #sqlMaximumConcurrentActivities!constants!public! !
!RioConstants categoriesFor: #sqlMaximumCursorNameLength!constants!public! !
!RioConstants categoriesFor: #sqlMaximumDriverConnections!constants!public! !
!RioConstants categoriesFor: #sqlMaximumIdentifierLength!constants!public! !
!RioConstants categoriesFor: #sqlMaximumIndexSize!constants!public! !
!RioConstants categoriesFor: #sqlMaximumRowSize!constants!public! !
!RioConstants categoriesFor: #sqlMaximumSchemaNameLength!constants!public! !
!RioConstants categoriesFor: #sqlMaximumStatementLength!constants!public! !
!RioConstants categoriesFor: #sqlMaximumTablesInSelect!constants!public! !
!RioConstants categoriesFor: #sqlMaximumUserNameLength!constants!public! !
!RioConstants categoriesFor: #sqlMaxIndexSize!constants!public! !
!RioConstants categoriesFor: #sqlMaxLength!constants!public! !
!RioConstants categoriesFor: #sqlMaxLengthDefault!constants!public! !
!RioConstants categoriesFor: #sqlMaxMessageLength!constants!public! !
!RioConstants categoriesFor: #sqlMaxOptionStringLength!constants!public! !
!RioConstants categoriesFor: #sqlMaxOwnerNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxProcedureNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxQualifierNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxRows!constants!public! !
!RioConstants categoriesFor: #sqlMaxRowsDefault!constants!public! !
!RioConstants categoriesFor: #sqlMaxRowSize!constants!public! !
!RioConstants categoriesFor: #sqlMaxRowSizeIncludesLong!constants!public! !
!RioConstants categoriesFor: #sqlMaxSchemaNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxStatementLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxTableNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMaxTablesInSelect!constants!public! !
!RioConstants categoriesFor: #sqlMaxUserNameLen!constants!public! !
!RioConstants categoriesFor: #sqlMinute!constants!public! !
!RioConstants categoriesFor: #sqlMinuteToSecond!constants!public! !
!RioConstants categoriesFor: #sqlModeDefault!constants!public! !
!RioConstants categoriesFor: #sqlModeReadOnly!constants!public! !
!RioConstants categoriesFor: #sqlModeReadWrite!constants!public! !
!RioConstants categoriesFor: #sqlMonth!constants!public! !
!RioConstants categoriesFor: #sqlMultipleActiveTxn!constants!public! !
!RioConstants categoriesFor: #sqlMultResultSets!constants!public! !
!RioConstants categoriesFor: #sqlNamed!constants!public! !
!RioConstants categoriesFor: #sqlNcEnd!constants!public! !
!RioConstants categoriesFor: #sqlNcHigh!constants!public! !
!RioConstants categoriesFor: #sqlNcLow!constants!public! !
!RioConstants categoriesFor: #sqlNcStart!constants!public! !
!RioConstants categoriesFor: #sqlNeedData!constants!public! !
!RioConstants categoriesFor: #sqlNeedLongDataLen!constants!public! !
!RioConstants categoriesFor: #sqlNncNonNull!constants!public! !
!RioConstants categoriesFor: #sqlNncNull!constants!public! !
!RioConstants categoriesFor: #sqlNoAction!constants!public! !
!RioConstants categoriesFor: #sqlNoColumnNumber!constants!public! !
!RioConstants categoriesFor: #sqlNoData!constants!public! !
!RioConstants categoriesFor: #sqlNoDataFound!constants!public! !
!RioConstants categoriesFor: #sqlNonNullableColumns!constants!public! !
!RioConstants categoriesFor: #sqlNonscrollable!constants!public! !
!RioConstants categoriesFor: #sqlNoNulls!constants!public! !
!RioConstants categoriesFor: #sqlNoRowNumber!constants!public! !
!RioConstants categoriesFor: #sqlNoscan!constants!public! !
!RioConstants categoriesFor: #sqlNoscanDefault!constants!public! !
!RioConstants categoriesFor: #sqlNoscanOff!constants!public! !
!RioConstants categoriesFor: #sqlNoscanOn!constants!public! !
!RioConstants categoriesFor: #sqlNoTotal!constants!public! !
!RioConstants categoriesFor: #sqlNts!constants!public! !
!RioConstants categoriesFor: #sqlNtsl!constants!public! !
!RioConstants categoriesFor: #sqlNullable!constants!public! !
!RioConstants categoriesFor: #sqlNullableUnknown!constants!public! !
!RioConstants categoriesFor: #sqlNullCollation!constants!public! !
!RioConstants categoriesFor: #sqlNullData!constants!public! !
!RioConstants categoriesFor: #sqlNullHandle!constants!public! !
!RioConstants categoriesFor: #sqlNullHdbc!constants!public! !
!RioConstants categoriesFor: #sqlNullHdesc!constants!public! !
!RioConstants categoriesFor: #sqlNullHenv!constants!public! !
!RioConstants categoriesFor: #sqlNullHstmt!constants!public! !
!RioConstants categoriesFor: #sqlNumeric!constants!public! !
!RioConstants categoriesFor: #sqlNumericFunctions!constants!public! !
!RioConstants categoriesFor: #sqlNumExtensions!constants!public! !
!RioConstants categoriesFor: #sqlNumFunctions!constants!public! !
!RioConstants categoriesFor: #sqlOacLevel1!constants!public! !
!RioConstants categoriesFor: #sqlOacLevel2!constants!public! !
!RioConstants categoriesFor: #sqlOacNone!constants!public! !
!RioConstants categoriesFor: #sqlOdbcApiConformance!constants!public! !
!RioConstants categoriesFor: #sqlOdbcCursors!constants!public! !
!RioConstants categoriesFor: #sqlOdbcInterfaceConformance!constants!public! !
!RioConstants categoriesFor: #sqlOdbcKeywords!constants!public! !
!RioConstants categoriesFor: #sqlOdbcSagCliConformance!constants!public! !
!RioConstants categoriesFor: #sqlOdbcSqlConformance!constants!public! !
!RioConstants categoriesFor: #sqlOdbcSqlOptIef!constants!public! !
!RioConstants categoriesFor: #sqlOdbcVer!constants!public! !
!RioConstants categoriesFor: #sqlOicCore!constants!public! !
!RioConstants categoriesFor: #sqlOicLevel1!constants!public! !
!RioConstants categoriesFor: #sqlOicLevel2!constants!public! !
!RioConstants categoriesFor: #sqlOjAllComparisonOps!constants!public! !
!RioConstants categoriesFor: #sqlOjCapabilities!constants!public! !
!RioConstants categoriesFor: #sqlOjFull!constants!public! !
!RioConstants categoriesFor: #sqlOjInner!constants!public! !
!RioConstants categoriesFor: #sqlOjLeft!constants!public! !
!RioConstants categoriesFor: #sqlOjNested!constants!public! !
!RioConstants categoriesFor: #sqlOjNotOrdered!constants!public! !
!RioConstants categoriesFor: #sqlOjRight!constants!public! !
!RioConstants categoriesFor: #sqlOptTrace!constants!public! !
!RioConstants categoriesFor: #sqlOptTraceDefault!constants!public! !
!RioConstants categoriesFor: #sqlOptTracefile!constants!public! !
!RioConstants categoriesFor: #sqlOptTraceFileDefault!constants!public! !
!RioConstants categoriesFor: #sqlOptTraceOff!constants!public! !
!RioConstants categoriesFor: #sqlOptTraceOn!constants!public! !
!RioConstants categoriesFor: #sqlOrderByColumnsInSelect!constants!public! !
!RioConstants categoriesFor: #sqlOsccCompliant!constants!public! !
!RioConstants categoriesFor: #sqlOsccNotCompliant!constants!public! !
!RioConstants categoriesFor: #sqlOscCore!constants!public! !
!RioConstants categoriesFor: #sqlOscExtended!constants!public! !
!RioConstants categoriesFor: #sqlOscMinimum!constants!public! !
!RioConstants categoriesFor: #sqlOuDmlStatements!constants!public! !
!RioConstants categoriesFor: #sqlOuIndexDefinition!constants!public! !
!RioConstants categoriesFor: #sqlOuPrivilegeDefinition!constants!public! !
!RioConstants categoriesFor: #sqlOuProcedureInvocation!constants!public! !
!RioConstants categoriesFor: #sqlOuTableDefinition!constants!public! !
!RioConstants categoriesFor: #sqlOuterJoinCapabilities!constants!public! !
!RioConstants categoriesFor: #sqlOuterJoins!constants!public! !
!RioConstants categoriesFor: #sqlOvOdbc2!constants!public! !
!RioConstants categoriesFor: #sqlOvOdbc3!constants!public! !
!RioConstants categoriesFor: #sqlOwnerTerm!constants!public! !
!RioConstants categoriesFor: #sqlOwnerUsage!constants!public! !
!RioConstants categoriesFor: #sqlPacketSize!constants!public! !
!RioConstants categoriesFor: #sqlParamArrayRowCounts!constants!public! !
!RioConstants categoriesFor: #sqlParamArraySelects!constants!public! !
!RioConstants categoriesFor: #sqlParamBindByColumn!constants!public! !
!RioConstants categoriesFor: #sqlParamBindTypeDefault!constants!public! !
!RioConstants categoriesFor: #sqlParamDiagUnavailable!constants!public! !
!RioConstants categoriesFor: #sqlParamError!constants!public! !
!RioConstants categoriesFor: #sqlParamIgnore!constants!public! !
!RioConstants categoriesFor: #sqlParamInput!constants!public! !
!RioConstants categoriesFor: #sqlParamInputOutput!constants!public! !
!RioConstants categoriesFor: #sqlParamOutput!constants!public! !
!RioConstants categoriesFor: #sqlParamProceed!constants!public! !
!RioConstants categoriesFor: #sqlParamSuccess!constants!public! !
!RioConstants categoriesFor: #sqlParamSuccessWithInfo!constants!public! !
!RioConstants categoriesFor: #sqlParamTypeDefault!constants!public! !
!RioConstants categoriesFor: #sqlParamTypeUnknown!constants!public! !
!RioConstants categoriesFor: #sqlParamUnused!constants!public! !
!RioConstants categoriesFor: #sqlParcBatch!constants!public! !
!RioConstants categoriesFor: #sqlParcNoBatch!constants!public! !
!RioConstants categoriesFor: #sqlPasBatch!constants!public! !
!RioConstants categoriesFor: #sqlPasNoBatch!constants!public! !
!RioConstants categoriesFor: #sqlPasNoSelect!constants!public! !
!RioConstants categoriesFor: #sqlPcNonPseudo!constants!public! !
!RioConstants categoriesFor: #sqlPcNotPseudo!constants!public! !
!RioConstants categoriesFor: #sqlPcPseudo!constants!public! !
!RioConstants categoriesFor: #sqlPcUnknown!constants!public! !
!RioConstants categoriesFor: #sqlPosAdd!constants!public! !
!RioConstants categoriesFor: #sqlPosDelete!constants!public! !
!RioConstants categoriesFor: #sqlPosition!constants!public! !
!RioConstants categoriesFor: #sqlPositionedStatements!constants!public! !
!RioConstants categoriesFor: #sqlPosOperations!constants!public! !
!RioConstants categoriesFor: #sqlPosPosition!constants!public! !
!RioConstants categoriesFor: #sqlPosRefresh!constants!public! !
!RioConstants categoriesFor: #sqlPosUpdate!constants!public! !
!RioConstants categoriesFor: #sqlPredBasic!constants!public! !
!RioConstants categoriesFor: #sqlPredChar!constants!public! !
!RioConstants categoriesFor: #sqlPredNone!constants!public! !
!RioConstants categoriesFor: #sqlPredSearchable!constants!public! !
!RioConstants categoriesFor: #sqlProcedures!constants!public! !
!RioConstants categoriesFor: #sqlProcedureTerm!constants!public! !
!RioConstants categoriesFor: #sqlPsPositionedDelete!constants!public! !
!RioConstants categoriesFor: #sqlPsPositionedUpdate!constants!public! !
!RioConstants categoriesFor: #sqlPsSelectForUpdate!constants!public! !
!RioConstants categoriesFor: #sqlPtFunction!constants!public! !
!RioConstants categoriesFor: #sqlPtProcedure!constants!public! !
!RioConstants categoriesFor: #sqlPtUnknown!constants!public! !
!RioConstants categoriesFor: #sqlQlEnd!constants!public! !
!RioConstants categoriesFor: #sqlQlStart!constants!public! !
!RioConstants categoriesFor: #sqlQualifierLocation!constants!public! !
!RioConstants categoriesFor: #sqlQualifierNameSeparator!constants!public! !
!RioConstants categoriesFor: #sqlQualifierTerm!constants!public! !
!RioConstants categoriesFor: #sqlQualifierUsage!constants!public! !
!RioConstants categoriesFor: #sqlQuDmlStatements!constants!public! !
!RioConstants categoriesFor: #sqlQueryTimeout!constants!public! !
!RioConstants categoriesFor: #sqlQueryTimeoutDefault!constants!public! !
!RioConstants categoriesFor: #sqlQuick!constants!public! !
!RioConstants categoriesFor: #sqlQuietMode!constants!public! !
!RioConstants categoriesFor: #sqlQuIndexDefinition!constants!public! !
!RioConstants categoriesFor: #sqlQuotedIdentifierCase!constants!public! !
!RioConstants categoriesFor: #sqlQuPrivilegeDefinition!constants!public! !
!RioConstants categoriesFor: #sqlQuProcedureInvocation!constants!public! !
!RioConstants categoriesFor: #sqlQuTableDefinition!constants!public! !
!RioConstants categoriesFor: #sqlRdDefault!constants!public! !
!RioConstants categoriesFor: #sqlRdOff!constants!public! !
!RioConstants categoriesFor: #sqlRdOn!constants!public! !
!RioConstants categoriesFor: #sqlReal!constants!public! !
!RioConstants categoriesFor: #sqlRefresh!constants!public! !
!RioConstants categoriesFor: #sqlResetParams!constants!public! !
!RioConstants categoriesFor: #sqlRestrict!constants!public! !
!RioConstants categoriesFor: #sqlResultCol!constants!public! !
!RioConstants categoriesFor: #sqlRetrieveData!constants!public! !
!RioConstants categoriesFor: #sqlReturnValue!constants!public! !
!RioConstants categoriesFor: #sqlRollback!constants!public! !
!RioConstants categoriesFor: #sqlRowAdded!constants!public! !
!RioConstants categoriesFor: #sqlRowDeleted!constants!public! !
!RioConstants categoriesFor: #sqlRowError!constants!public! !
!RioConstants categoriesFor: #sqlRowIdentifier!constants!public! !
!RioConstants categoriesFor: #sqlRowIgnore!constants!public! !
!RioConstants categoriesFor: #sqlRowNorow!constants!public! !
!RioConstants categoriesFor: #sqlRowNumber!constants!public! !
!RioConstants categoriesFor: #sqlRowNumberUnknown!constants!public! !
!RioConstants categoriesFor: #sqlRowProceed!constants!public! !
!RioConstants categoriesFor: #sqlRowsetSize!constants!public! !
!RioConstants categoriesFor: #sqlRowsetSizeDefault!constants!public! !
!RioConstants categoriesFor: #sqlRowSuccess!constants!public! !
!RioConstants categoriesFor: #sqlRowSuccessWithInfo!constants!public! !
!RioConstants categoriesFor: #sqlRowUpdated!constants!public! !
!RioConstants categoriesFor: #sqlRowUpdates!constants!public! !
!RioConstants categoriesFor: #sqlRowver!constants!public! !
!RioConstants categoriesFor: #sqlSccIso92Cli!constants!public! !
!RioConstants categoriesFor: #sqlSccoLock!constants!public! !
!RioConstants categoriesFor: #sqlSccoOptRowver!constants!public! !
!RioConstants categoriesFor: #sqlSccoOptTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlSccoOptValues!constants!public! !
!RioConstants categoriesFor: #sqlSccoReadOnly!constants!public! !
!RioConstants categoriesFor: #sqlSccXopenCliVersion1!constants!public! !
!RioConstants categoriesFor: #sqlScFips1272Transitional!constants!public! !
!RioConstants categoriesFor: #sqlSchemaTerm!constants!public! !
!RioConstants categoriesFor: #sqlSchemaUsage!constants!public! !
!RioConstants categoriesFor: #sqlScNonUnique!constants!public! !
!RioConstants categoriesFor: #sqlScopeCurrow!constants!public! !
!RioConstants categoriesFor: #sqlScopeSession!constants!public! !
!RioConstants categoriesFor: #sqlScopeTransaction!constants!public! !
!RioConstants categoriesFor: #sqlScrollable!constants!public! !
!RioConstants categoriesFor: #sqlScrollConcurrency!constants!public! !
!RioConstants categoriesFor: #sqlScrollDynamic!constants!public! !
!RioConstants categoriesFor: #sqlScrollForwardOnly!constants!public! !
!RioConstants categoriesFor: #sqlScrollKeysetDriven!constants!public! !
!RioConstants categoriesFor: #sqlScrollOptions!constants!public! !
!RioConstants categoriesFor: #sqlScrollStatic!constants!public! !
!RioConstants categoriesFor: #sqlScSql92Entry!constants!public! !
!RioConstants categoriesFor: #sqlScSql92Full!constants!public! !
!RioConstants categoriesFor: #sqlScSql92Intermediate!constants!public! !
!RioConstants categoriesFor: #sqlScTryUnique!constants!public! !
!RioConstants categoriesFor: #sqlScUnique!constants!public! !
!RioConstants categoriesFor: #sqlSdfCurrentDate!constants!public! !
!RioConstants categoriesFor: #sqlSdfCurrentTime!constants!public! !
!RioConstants categoriesFor: #sqlSdfCurrentTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlSearchable!constants!public! !
!RioConstants categoriesFor: #sqlSearchPatternEscape!constants!public! !
!RioConstants categoriesFor: #sqlSecond!constants!public! !
!RioConstants categoriesFor: #sqlSensitive!constants!public! !
!RioConstants categoriesFor: #sqlServerName!constants!public! !
!RioConstants categoriesFor: #sqlSetDefault!constants!public! !
!RioConstants categoriesFor: #sqlSetNull!constants!public! !
!RioConstants categoriesFor: #sqlSetparamValueMax!constants!public! !
!RioConstants categoriesFor: #sqlSetposMaxLockValue!constants!public! !
!RioConstants categoriesFor: #sqlSetposMaxOptionValue!constants!public! !
!RioConstants categoriesFor: #sqlSfkdCascade!constants!public! !
!RioConstants categoriesFor: #sqlSfkdNoAction!constants!public! !
!RioConstants categoriesFor: #sqlSfkdSetDefault!constants!public! !
!RioConstants categoriesFor: #sqlSfkdSetNull!constants!public! !
!RioConstants categoriesFor: #sqlSfkuCascade!constants!public! !
!RioConstants categoriesFor: #sqlSfkuNoAction!constants!public! !
!RioConstants categoriesFor: #sqlSfkuSetDefault!constants!public! !
!RioConstants categoriesFor: #sqlSfkuSetNull!constants!public! !
!RioConstants categoriesFor: #sqlSgDeleteTable!constants!public! !
!RioConstants categoriesFor: #sqlSgInsertColumn!constants!public! !
!RioConstants categoriesFor: #sqlSgInsertTable!constants!public! !
!RioConstants categoriesFor: #sqlSgReferencesColumn!constants!public! !
!RioConstants categoriesFor: #sqlSgReferencesTable!constants!public! !
!RioConstants categoriesFor: #sqlSgSelectTable!constants!public! !
!RioConstants categoriesFor: #sqlSgUpdateColumn!constants!public! !
!RioConstants categoriesFor: #sqlSgUpdateTable!constants!public! !
!RioConstants categoriesFor: #sqlSgUsageOnCharacterSet!constants!public! !
!RioConstants categoriesFor: #sqlSgUsageOnCollation!constants!public! !
!RioConstants categoriesFor: #sqlSgUsageOnDomain!constants!public! !
!RioConstants categoriesFor: #sqlSgUsageOnTranslation!constants!public! !
!RioConstants categoriesFor: #sqlSgWithGrantOption!constants!public! !
!RioConstants categoriesFor: #sqlSignedOffset!constants!public! !
!RioConstants categoriesFor: #sqlSimulateCursor!constants!public! !
!RioConstants categoriesFor: #sqlSmallint!constants!public! !
!RioConstants categoriesFor: #sqlSnvfBitLength!constants!public! !
!RioConstants categoriesFor: #sqlSnvfCharacterLength!constants!public! !
!RioConstants categoriesFor: #sqlSnvfCharLength!constants!public! !
!RioConstants categoriesFor: #sqlSnvfExtract!constants!public! !
!RioConstants categoriesFor: #sqlSnvfOctetLength!constants!public! !
!RioConstants categoriesFor: #sqlSnvfPosition!constants!public! !
!RioConstants categoriesFor: #sqlSoDynamic!constants!public! !
!RioConstants categoriesFor: #sqlSoForwardOnly!constants!public! !
!RioConstants categoriesFor: #sqlSoKeysetDriven!constants!public! !
!RioConstants categoriesFor: #sqlSoMixed!constants!public! !
!RioConstants categoriesFor: #sqlSoStatic!constants!public! !
!RioConstants categoriesFor: #sqlSpBetween!constants!public! !
!RioConstants categoriesFor: #sqlSpComparison!constants!public! !
!RioConstants categoriesFor: #sqlSpecialCharacters!constants!public! !
!RioConstants categoriesFor: #sqlSpecMajor!constants!public! !
!RioConstants categoriesFor: #sqlSpecMinor!constants!public! !
!RioConstants categoriesFor: #sqlSpecString!constants!public! !
!RioConstants categoriesFor: #sqlSpExists!constants!public! !
!RioConstants categoriesFor: #sqlSpIn!constants!public! !
!RioConstants categoriesFor: #sqlSpIsnotnull!constants!public! !
!RioConstants categoriesFor: #sqlSpIsnull!constants!public! !
!RioConstants categoriesFor: #sqlSpLike!constants!public! !
!RioConstants categoriesFor: #sqlSpMatchFull!constants!public! !
!RioConstants categoriesFor: #sqlSpMatchPartial!constants!public! !
!RioConstants categoriesFor: #sqlSpMatchUniqueFull!constants!public! !
!RioConstants categoriesFor: #sqlSpMatchUniquePartial!constants!public! !
!RioConstants categoriesFor: #sqlSpOverlaps!constants!public! !
!RioConstants categoriesFor: #sqlSpQuantifiedComparison!constants!public! !
!RioConstants categoriesFor: #sqlSpUnique!constants!public! !
!RioConstants categoriesFor: #sqlSqComparison!constants!public! !
!RioConstants categoriesFor: #sqlSqCorrelatedSubqueries!constants!public! !
!RioConstants categoriesFor: #sqlSqExists!constants!public! !
!RioConstants categoriesFor: #sqlSqIn!constants!public! !
!RioConstants categoriesFor: #sqlSql92DatetimeFunctions!constants!public! !
!RioConstants categoriesFor: #sqlSql92ForeignKeyDeleteRule!constants!public! !
!RioConstants categoriesFor: #sqlSql92ForeignKeyUpdateRule!constants!public! !
!RioConstants categoriesFor: #sqlSql92Grant!constants!public! !
!RioConstants categoriesFor: #sqlSql92NumericValueFunctions!constants!public! !
!RioConstants categoriesFor: #sqlSql92Predicates!constants!public! !
!RioConstants categoriesFor: #sqlSql92RelationalJoinOperators!constants!public! !
!RioConstants categoriesFor: #sqlSql92Revoke!constants!public! !
!RioConstants categoriesFor: #sqlSql92RowValueConstructor!constants!public! !
!RioConstants categoriesFor: #sqlSql92StringFunctions!constants!public! !
!RioConstants categoriesFor: #sqlSql92ValueExpressions!constants!public! !
!RioConstants categoriesFor: #sqlSqlConformance!constants!public! !
!RioConstants categoriesFor: #sqlSqlstateSize!constants!public! !
!RioConstants categoriesFor: #sqlSqQuantified!constants!public! !
!RioConstants categoriesFor: #sqlSrCascade!constants!public! !
!RioConstants categoriesFor: #sqlSrDeleteTable!constants!public! !
!RioConstants categoriesFor: #sqlSrGrantOptionFor!constants!public! !
!RioConstants categoriesFor: #sqlSrInsertColumn!constants!public! !
!RioConstants categoriesFor: #sqlSrInsertTable!constants!public! !
!RioConstants categoriesFor: #sqlSrjoCorrespondingClause!constants!public! !
!RioConstants categoriesFor: #sqlSrjoCrossJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrjoExceptJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrjoFullOuterJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrjoInnerJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrjoIntersectJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrjoLeftOuterJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrjoNaturalJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrjoRightOuterJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrjoUnionJoin!constants!public! !
!RioConstants categoriesFor: #sqlSrReferencesColumn!constants!public! !
!RioConstants categoriesFor: #sqlSrReferencesTable!constants!public! !
!RioConstants categoriesFor: #sqlSrRestrict!constants!public! !
!RioConstants categoriesFor: #sqlSrSelectTable!constants!public! !
!RioConstants categoriesFor: #sqlSrUpdateColumn!constants!public! !
!RioConstants categoriesFor: #sqlSrUpdateTable!constants!public! !
!RioConstants categoriesFor: #sqlSrUsageOnCharacterSet!constants!public! !
!RioConstants categoriesFor: #sqlSrUsageOnCollation!constants!public! !
!RioConstants categoriesFor: #sqlSrUsageOnDomain!constants!public! !
!RioConstants categoriesFor: #sqlSrUsageOnTranslation!constants!public! !
!RioConstants categoriesFor: #sqlSrvcDefault!constants!public! !
!RioConstants categoriesFor: #sqlSrvcNull!constants!public! !
!RioConstants categoriesFor: #sqlSrvcRowSubquery!constants!public! !
!RioConstants categoriesFor: #sqlSrvcValueExpression!constants!public! !
!RioConstants categoriesFor: #sqlSsAdditions!constants!public! !
!RioConstants categoriesFor: #sqlSsDeletions!constants!public! !
!RioConstants categoriesFor: #sqlSsfConvert!constants!public! !
!RioConstants categoriesFor: #sqlSsfLower!constants!public! !
!RioConstants categoriesFor: #sqlSsfSubstring!constants!public! !
!RioConstants categoriesFor: #sqlSsfTranslate!constants!public! !
!RioConstants categoriesFor: #sqlSsfTrimBoth!constants!public! !
!RioConstants categoriesFor: #sqlSsfTrimLeading!constants!public! !
!RioConstants categoriesFor: #sqlSsfTrimTrailing!constants!public! !
!RioConstants categoriesFor: #sqlSsfUpper!constants!public! !
!RioConstants categoriesFor: #sqlSsUpdates!constants!public! !
!RioConstants categoriesFor: #sqlStandardCliConformance!constants!public! !
!RioConstants categoriesFor: #sqlStaticCursorAttributes1!constants!public! !
!RioConstants categoriesFor: #sqlStaticCursorAttributes2!constants!public! !
!RioConstants categoriesFor: #sqlStaticSensitivity!constants!public! !
!RioConstants categoriesFor: #sqlStillExecuting!constants!public! !
!RioConstants categoriesFor: #sqlStmtOptMax!constants!public! !
!RioConstants categoriesFor: #sqlStmtOptMin!constants!public! !
!RioConstants categoriesFor: #sqlStringFunctions!constants!public! !
!RioConstants categoriesFor: #sqlSubqueries!constants!public! !
!RioConstants categoriesFor: #sqlSucceeded:!constants!public! !
!RioConstants categoriesFor: #sqlSuccess!constants!public! !
!RioConstants categoriesFor: #sqlSuccessWithInfo!constants!public! !
!RioConstants categoriesFor: #sqlSuDmlStatements!constants!public! !
!RioConstants categoriesFor: #sqlSuIndexDefinition!constants!public! !
!RioConstants categoriesFor: #sqlSuPrivilegeDefinition!constants!public! !
!RioConstants categoriesFor: #sqlSuProcedureInvocation!constants!public! !
!RioConstants categoriesFor: #sqlSuTableDefinition!constants!public! !
!RioConstants categoriesFor: #sqlSveCase!constants!public! !
!RioConstants categoriesFor: #sqlSveCast!constants!public! !
!RioConstants categoriesFor: #sqlSveCoalesce!constants!public! !
!RioConstants categoriesFor: #sqlSveNullif!constants!public! !
!RioConstants categoriesFor: #sqlSystemFunctions!constants!public! !
!RioConstants categoriesFor: #sqlTableStat!constants!public! !
!RioConstants categoriesFor: #sqlTableTerm!constants!public! !
!RioConstants categoriesFor: #sqlTcAll!constants!public! !
!RioConstants categoriesFor: #sqlTcDdlCommit!constants!public! !
!RioConstants categoriesFor: #sqlTcDdlIgnore!constants!public! !
!RioConstants categoriesFor: #sqlTcDml!constants!public! !
!RioConstants categoriesFor: #sqlTcNone!constants!public! !
!RioConstants categoriesFor: #sqlTime!constants!public! !
!RioConstants categoriesFor: #sqlTimedateAddIntervals!constants!public! !
!RioConstants categoriesFor: #sqlTimedateDiffIntervals!constants!public! !
!RioConstants categoriesFor: #sqlTimedateFunctions!constants!public! !
!RioConstants categoriesFor: #sqlTimeLen!constants!public! !
!RioConstants categoriesFor: #sqlTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlTimestampLen!constants!public! !
!RioConstants categoriesFor: #sqlTinyint!constants!public! !
!RioConstants categoriesFor: #sqlTransactionCapable!constants!public! !
!RioConstants categoriesFor: #sqlTransactionIsolationOption!constants!public! !
!RioConstants categoriesFor: #sqlTransactionReadCommitted!constants!public! !
!RioConstants categoriesFor: #sqlTransactionReadUncommitted!constants!public! !
!RioConstants categoriesFor: #sqlTransactionRepeatableRead!constants!public! !
!RioConstants categoriesFor: #sqlTransactionSerializable!constants!public! !
!RioConstants categoriesFor: #sqlTranslateDll!constants!public! !
!RioConstants categoriesFor: #sqlTranslateOption!constants!public! !
!RioConstants categoriesFor: #sqlTrue!constants!public! !
!RioConstants categoriesFor: #sqlTxnCapable!constants!public! !
!RioConstants categoriesFor: #sqlTxnIsolation!constants!public! !
!RioConstants categoriesFor: #sqlTxnIsolationOption!constants!public! !
!RioConstants categoriesFor: #sqlTxnReadCommitted!constants!public! !
!RioConstants categoriesFor: #sqlTxnReadUncommitted!constants!public! !
!RioConstants categoriesFor: #sqlTxnRepeatableRead!constants!public! !
!RioConstants categoriesFor: #sqlTxnSerializable!constants!public! !
!RioConstants categoriesFor: #sqlTxnVersioning!constants!public! !
!RioConstants categoriesFor: #sqlTypeDate!constants!public! !
!RioConstants categoriesFor: #sqlTypeDriverEnd!constants!public! !
!RioConstants categoriesFor: #sqlTypeDriverStart!constants!public! !
!RioConstants categoriesFor: #sqlTypeMax!constants!public! !
!RioConstants categoriesFor: #sqlTypeMin!constants!public! !
!RioConstants categoriesFor: #sqlTypeNull!constants!public! !
!RioConstants categoriesFor: #sqlTypeTime!constants!public! !
!RioConstants categoriesFor: #sqlTypeTimestamp!constants!public! !
!RioConstants categoriesFor: #sqlUbDefault!constants!public! !
!RioConstants categoriesFor: #sqlUbFixed!constants!public! !
!RioConstants categoriesFor: #sqlUbOff!constants!public! !
!RioConstants categoriesFor: #sqlUbOn!constants!public! !
!RioConstants categoriesFor: #sqlUbVariable!constants!public! !
!RioConstants categoriesFor: #sqlUnbind!constants!public! !
!RioConstants categoriesFor: #sqlUnicode!constants!public! !
!RioConstants categoriesFor: #sqlUnicodeChar!constants!public! !
!RioConstants categoriesFor: #sqlUnicodeLongvarchar!constants!public! !
!RioConstants categoriesFor: #sqlUnicodeVarchar!constants!public! !
!RioConstants categoriesFor: #sqlUnion!constants!public! !
!RioConstants categoriesFor: #sqlUnionStatement!constants!public! !
!RioConstants categoriesFor: #sqlUnknownType!constants!public! !
!RioConstants categoriesFor: #sqlUnnamed!constants!public! !
!RioConstants categoriesFor: #sqlUnsearchable!constants!public! !
!RioConstants categoriesFor: #sqlUnsignedOffset!constants!public! !
!RioConstants categoriesFor: #sqlUnspecified!constants!public! !
!RioConstants categoriesFor: #sqlUpdate!constants!public! !
!RioConstants categoriesFor: #sqlUpdateByBookmark!constants!public! !
!RioConstants categoriesFor: #sqlUseBookmarks!constants!public! !
!RioConstants categoriesFor: #sqlUserName!constants!public! !
!RioConstants categoriesFor: #sqlUsUnion!constants!public! !
!RioConstants categoriesFor: #sqlUsUnionAll!constants!public! !
!RioConstants categoriesFor: #sqlUUnion!constants!public! !
!RioConstants categoriesFor: #sqlUUnionAll!constants!public! !
!RioConstants categoriesFor: #sqlVarbinary!constants!public! !
!RioConstants categoriesFor: #sqlVarchar!constants!public! !
!RioConstants categoriesFor: #sqlXopenCliYear!constants!public! !
!RioConstants categoriesFor: #sqlYear!constants!public! !
!RioConstants categoriesFor: #sqlYearToMonth!constants!public! !
!RioConstants categoriesFor: #traceVersion!constants!public! !

!RioConstants class methodsFor!

current
	"Current isNil ifTrue: [ Current := self new ]."
	^Current!

generateFrom: sqlHeaderPath extendedHeader: sqlExtendedHeaderPath
	"Generates a .ST file containing the class definition of RioConstants.  Note that the generated
	 file will likely require manual editing because the conversion is not 100% accurate."

	RioConstantsClassGenerator new
		generateClass: 'RioConstants'
		from: (Array with: sqlHeaderPath with: sqlExtendedHeaderPath)!

install
	Current := self new!

uninstall
	Current := nil! !
!RioConstants class categoriesFor: #current!instance creation!public! !
!RioConstants class categoriesFor: #generateFrom:extendedHeader:!public! !
!RioConstants class categoriesFor: #install!instance creation!public! !
!RioConstants class categoriesFor: #uninstall!public! !

RioConstantsClassGenerator guid: (GUID fromString: '{6BBB8F9D-559A-11D3-8269-00001D19F5C2}')!
RioConstantsClassGenerator comment: ''!
!RioConstantsClassGenerator categoriesForClass!Rio! !
!RioConstantsClassGenerator methodsFor!

translateValue: aString
	"Private - translate C-style values to Smalltalk-style values."

	| tempString |

	tempString := super translateValue: aString.
	(tempString leftString: 4) = 'SQL_' ifTrue: [ tempString := 'self ', (self translateName: tempString) ].

	^tempString

	! !
!RioConstantsClassGenerator categoriesFor: #translateValue:!helpers!private helpers!public! !

"Binary Globals"!

"Resources"!

