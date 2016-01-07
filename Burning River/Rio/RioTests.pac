| package |
package := Package name: 'RioTests'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #RioTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'Rio';
	add: 'RioConstants';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #RioTest
	instanceVariableNames: 'db'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

RioTest guid: (GUID fromString: '{943836A9-9BE9-4191-BDBF-6867B69D8A68}')!
RioTest comment: ''!
!RioTest categoriesForClass!Unclassified! !
!RioTest methodsFor!

basicDbTest
	"Private - Private"

	| typeResults employeeCursor rowCollection ordersCursor rowsetsFetched 
	  orderDetailsCursor |

	self shouldnt: [ typeResults := (RioMetadataCursor on: db) typeInfoFor: RioConstants current sqlAllTypes ] raise: RioError.
	self should: [ typeResults size > 0 ].

	self shouldnt: [ employeeCursor := RioStaticCursor
					on: db
					sql: 'Select EmployeeID, LastName, FirstName from Employees' ] raise: RioError.

	self shouldnt: [ rowCollection := employeeCursor upToEnd ] raise: RioError.
	self should: [ rowCollection size = 9 ].

	self should: [ #(##(RioConstants current sqlSuccess)
			      ##(RioConstants current sqlNoData)) includes: employeeCursor retcode ].

	self shouldnt: [ employeeCursor close ] raise: RioError.

	self shouldnt: [ ordersCursor := RioStaticCursor
					on: db
					sql: 'Select * from Orders' ] raise: RioError.

	self shouldnt: [ ordersCursor bindRowset: (ordersCursor createRowset: 10) ] raise: RioError.
	self shouldnt: [ ordersCursor next ] raise: RioError.

	rowsetsFetched := 1.

	self shouldnt: [
		[ (ordersCursor retcode = RioConstants current sqlSuccess) & (rowsetsFetched <= 5) ]
			whileTrue: [
				ordersCursor next.
				rowsetsFetched := rowsetsFetched + 1 ] ] raise: RioError.

	self shouldnt: [ ordersCursor close ] raise: RioError.

	self shouldnt: [ orderDetailsCursor := RioStaticCursor
						on: db
						sql: 'Select * from "Order Details"' ] raise: RioError.

	self shouldnt: [ rowCollection := orderDetailsCursor upToEnd ] raise: RioError.

	self should: [ #(##(RioConstants current sqlSuccess)
			      ##(RioConstants current sqlNoData)) includes: orderDetailsCursor retcode ].

	self shouldnt: [ orderDetailsCursor close ] raise: RioError.

	self should: [ rowCollection size = 2155 ].

	^false
!

setUp
	db := RioDatabase on: 'Northwind' userId: '' authorization: ''!

tearDown
	db disconnect!

testAutoInsertStatement
	| employeeRow stmt employeeColumnNames |

	employeeColumnNames := #('EmployeeID' 'LastName' 'FirstName' 'Title' 'TitleOfCourtesy' 'BirthDate'
							'HireDate' 'Address' 'City' 'Region' 'PostalCode' 'Country' 'HomePhone'
							'Extension' 'Notes' 'ReportsTo').

	self shouldnt: [ stmt := RioCursor
						on: db
						selectForColumns: employeeColumnNames
						fromTable: 'Employees'
						withKeys: nil ] raise: Exception.
	self shouldnt: [ employeeRow := stmt createRow ] raise: Exception.

	self shouldnt: [ stmt := (RioAutoInsertStatement on: db) tableName: 'Employees' ] raise: Exception.
	self shouldnt: [ stmt row: employeeRow ] raise: Exception.
	self should: [ stmt sql = ('INSERT INTO Employees ',
						'(EmployeeID,LastName,FirstName,Title,TitleOfCourtesy,BirthDate,',
						'HireDate,Address,City,Region,PostalCode,Country,HomePhone,',
						'Extension,Notes,ReportsTo) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)') ].!

testAutoUpdateStatement
	| employeeRow employeeColumnNames stmt |

	employeeColumnNames := #('EmployeeID' 'LastName' 'FirstName' 'Title' 'TitleOfCourtesy' 'BirthDate'
							'HireDate' 'Address' 'City' 'Region' 'PostalCode' 'Country' 'HomePhone'
							'Extension' 'Notes' 'ReportsTo').

	self shouldnt: [ stmt := RioCursor
						on: db
						selectForColumns: employeeColumnNames
						fromTable: 'Employees'
						withKeys: nil ] raise: Exception.
	self shouldnt: [ employeeRow := stmt createRow ] raise: Exception.

	self shouldnt: [ stmt := (RioAutoUpdateStatement on: db)
						tableName: 'Employees';
						whereClause: 'EmployeeID=?' ] raise: Exception.
	self shouldnt: [ stmt row: employeeRow ] raise: Exception.
	self should: [ stmt sql = ('UPDATE Employees SET ',
						'EmployeeID=?,LastName=?,FirstName=?,Title=?,TitleOfCourtesy=?,',
						'BirthDate=?,HireDate=?,Address=?,City=?,Region=?,PostalCode=?,',
						'Country=?,HomePhone=?,Extension=?,Notes=?,ReportsTo=? ',
						'WHERE EmployeeID=?') ].!

testCatalogs
	| catalogs |
	self should: [ (catalogs := db catalogs) size > 0 ]!

testColumnHeader
	| transaction orderDetailsCursor row rowset rowsetSize |

	rowsetSize := 25.

	self shouldnt: [ transaction := db readOnlyTransaction ] raise: Exception.

	transaction try:
		[ self shouldnt:
			[ orderDetailsCursor := db createStaticCursor: 'Select * from "Order Details"' ] raise: Exception.

		self shouldnt: [ row := orderDetailsCursor createRow ] raise: Exception.
		self shouldnt: [ orderDetailsCursor bindRowset: row ] raise: Exception.
		self shouldnt: [ row := orderDetailsCursor next ] raise: Exception.

		self shouldnt: [ rowset := orderDetailsCursor createRowset: rowsetSize ] raise: Exception.
		self shouldnt: [ orderDetailsCursor bindRowset: rowset ] raise: Exception.
		self shouldnt: [ rowset := orderDetailsCursor next ] raise: Exception ].

	self shouldnt: [ row setColumnDbDirty: 'OrderID' ] raise: Exception.
	self should: [ row isColumnDbDirty: 'OrderID' ].

	self shouldnt: [ row clearColumnDbDirty: 'OrderID' ] raise: Exception.
	self shouldnt: [ row isColumnDbDirty: 'OrderID' ].

	self shouldnt: [ row setDbDirty ] raise: Exception.
	1 to: row nCols do:
		[ :i |
		self should: [ (row isColumnDbDirty: i) = true ] ].

	self shouldnt: [ row clearDbDirty ] raise: Exception.
	1 to: row nCols do:
		[ :i |
		self should: [ (row isColumnDbDirty: i) = false ] ].

	self shouldnt: [ row setUiDirty ] raise: Exception.
	1 to: row nCols do:
		[ :i |
		self should: [ (row isColumnUiDirty: i) = true ] ].

	self shouldnt: [ row clearUiDirty ] raise: Exception.
	1 to: row nCols do:
		[ :i |
		self should: [ (row isColumnUiDirty: i) = false ] ].

	self should: [ rowset nRows = rowsetSize ].

	1 to: rowset nRows do:
		[ :n |
		self should: [ (rowset rowStatus: n) = RioConstants current sqlRowSuccess ] ].

	rowset setDbDirty.

	1 to: rowset nRows do:
		[ :n |
		row := rowset at: n.
		1 to: row nCols do:
			[ :c |
			self should: [ (row isColumnDbDirty: c) = true ] ] ].

	rowset clearDbDirty.

	1 to: rowset nRows do:
		[ :n |
		row := rowset at: n.
		1 to: row nCols do:
			[ :c |
			self should: [ (row isColumnDbDirty: c) = false ] ] ].

	rowset setUiDirty.

	1 to: rowset nRows do:
		[ :n |
		row := rowset at: n.
		1 to: row nCols do:
			[ :c |
			self should: [ (row isColumnUiDirty: c) = true ] ] ].

	rowset clearUiDirty.

	1 to: rowset nRows do:
		[ :n |
		row := rowset at: n.
		1 to: row nCols do:
			[ :c |
			self should: [ (row isColumnUiDirty: c) = false ] ] ].!

testColumnsInCatalog
	| rows aRow |
	self shouldnt: [ rows := (RioMetadataCursor on: db) columnsInCatalog: nil schema: nil table: 'Products' columnName: nil ] raise: Exception.
	self should: [ rows size = 10 ].
	self shouldnt: [ aRow := rows at: 1 ] raise: Exception.

	db odbcMajorVersion = 2 ifTrue:
		[ self should: [ aRow nCols >= 12 ].
		self should: [ aRow containsColumnNamed: 'TABLE_QUALIFIER' ].
		self should: [ aRow containsColumnNamed: 'TABLE_OWNER' ].
		self should: [ aRow containsColumnNamed: 'TABLE_NAME' ].
		self should: [ aRow containsColumnNamed: 'COLUMN_NAME' ].
		self should: [ aRow containsColumnNamed: 'DATA_TYPE' ].
		self should: [ aRow containsColumnNamed: 'TYPE_NAME' ].
		self should: [ aRow containsColumnNamed: 'PRECISION' ].
		self should: [ aRow containsColumnNamed: 'LENGTH' ].
		self should: [ aRow containsColumnNamed: 'SCALE' ].
		self should: [ aRow containsColumnNamed: 'RADIX' ].
		self should: [ aRow containsColumnNamed: 'NULLABLE' ].
		self should: [ aRow containsColumnNamed: 'REMARKS' ] ].

	db odbcMajorVersion = 3 ifTrue:
		[ self should: [ aRow nCols >= 18 ].
		self should: [ aRow containsColumnNamed: 'TABLE_CAT' ].
		self should: [ aRow containsColumnNamed: 'TABLE_SCHEM' ].
		self should: [ aRow containsColumnNamed: 'TABLE_NAME' ].
		self should: [ aRow containsColumnNamed: 'COLUMN_NAME' ].
		self should: [ aRow containsColumnNamed: 'DATA_TYPE' ].
		self should: [ aRow containsColumnNamed: 'TYPE_NAME' ].
		self should: [ aRow containsColumnNamed: 'COLUMN_SIZE' ].
		self should: [ aRow containsColumnNamed: 'BUFFER_LENGTH' ].
		self should: [ aRow containsColumnNamed: 'DECIMAL_DIGITS' ].
		self should: [ aRow containsColumnNamed: 'NUM_PREC_RADIX' ].
		self should: [ aRow containsColumnNamed: 'NULLABLE' ].
		self should: [ aRow containsColumnNamed: 'REMARKS' ].
		self should: [ aRow containsColumnNamed: 'COLUMN_DEF' ].
		self should: [ aRow containsColumnNamed: 'SQL_DATA_TYPE' ].
		self should: [ aRow containsColumnNamed: 'SQL_DATETIME_SUB' ].
		self should: [ aRow containsColumnNamed: 'CHAR_OCTET_LENGTH' ].
		self should: [ aRow containsColumnNamed: 'ORDINAL_POSITION' ].
		self should: [ aRow containsColumnNamed: 'IS_NULLABLE' ] ]!

testConnectionInformation
	"Retrieve information about the connection"

	| stringInfo integerInfo |

	stringInfo := #(#sqlAccessibleProcedures #sqlAccessibleTables
				#sqlColumnAlias #sqlDataSourceName
				#sqlDataSourceReadOnly "#sqlDatabaseName"
				#sqlDbmsName #sqlDbmsVer #sqlDriverName
				#sqlDriverOdbcVer #sqlDriverVer
				#sqlExpressionsInOrderby #sqlIdentifierQuoteChar
				#sqlKeywords #sqlLikeEscapeClause
				#sqlMaxRowSizeIncludesLong #sqlMultResultSets
				#sqlMultipleActiveTxn #sqlNeedLongDataLen
				#sqlOdbcSqlOptIef #sqlOdbcVer
				#sqlOrderByColumnsInSelect #sqlOuterJoins
				#sqlOwnerTerm #sqlProcedureTerm #sqlProcedures
				#sqlQualifierNameSeparator #sqlQualifierTerm
				#sqlRowUpdates #sqlSearchPatternEscape
				#sqlServerName #sqlSpecialCharacters
				#sqlTableTerm #sqlUserName).

	integerInfo := #(#sqlActiveConnections #sqlActiveStatements
				#sqlConcatNullBehavior 
				#sqlDriverHdbc
				#sqlDriverHenv #sqlDriverHlib
				#sqlMaxBinaryLiteralLen
				#sqlMaxCharLiteralLen #sqlMaxColumnNameLen
				#sqlMaxColumnsInGroupBy #sqlMaxColumnsInIndex
				#sqlMaxColumnsInOrderBy #sqlMaxColumnsInSelect
				#sqlMaxColumnsInTable #sqlMaxCursorNameLen
				#sqlMaxIndexSize #sqlMaxOwnerNameLen
				#sqlMaxProcedureNameLen #sqlMaxQualifierNameLen
				#sqlMaxRowSize #sqlMaxRowSizeIncludesLong
				#sqlMaxStatementLen #sqlMaxTableNameLen
				#sqlMaxTablesInSelect #sqlMaxUserNameLen).

	stringInfo do: [ :str | self should: [ (db stringInfo: (RioConstants current perform: str)) isKindOf: String ] ].
	integerInfo do: [ :str | self should: [ (db integerInfo: (RioConstants current perform: str)) isKindOf: Integer ] ]!

testConversions
	| conversionSymbols testSymbols |

	conversionSymbols := #(#sqlCvtBigint #sqlCvtBinary #sqlCvtBit #sqlCvtChar
					    #sqlCvtDate #sqlCvtDecimal #sqlCvtDouble
					    #sqlCvtFloat #sqlCvtInteger #sqlCvtLongvarbinary
					    #sqlCvtLongvarchar #sqlCvtNumeric #sqlCvtReal
					    #sqlCvtSmallint #sqlCvtTime #sqlCvtTimestamp
					    #sqlCvtTinyint #sqlCvtVarbinary #sqlCvtVarchar).

	testSymbols := #(#sqlConvertBigint #sqlConvertBinary #sqlConvertBit #sqlConvertChar 
				#sqlConvertDate #sqlConvertDecimal #sqlConvertDouble #sqlConvertFloat 
				#sqlConvertInteger #sqlConvertLongvarbinary #sqlConvertLongvarchar 
				#sqlConvertNumeric #sqlConvertReal #sqlConvertSmallint #sqlConvertTime 
				#sqlConvertTimestamp #sqlConvertTinyint #sqlConvertVarbinary 
				#sqlConvertVarchar).

	testSymbols do: [ :each | self translateBitmaskFor: each withSymbols: conversionSymbols ]!

testCursor
	| transaction orderDetailsCursor rows nestedTransaction countCursor countRows |

	self shouldnt: [ transaction := db readOnlyTransaction ] raise: Exception.

	transaction try:
		[ self shouldnt:
			[ orderDetailsCursor := db createStaticCursor: 'Select * from Products' ] raise: Exception.

		self shouldnt: [ rows := orderDetailsCursor upToEnd ] raise: Exception.

		self should: [ #(##(RioConstants current sqlSuccess)
				      ##(RioConstants current sqlNoData)) includes: orderDetailsCursor retcode ].

		self should: [ (rows at: 1) ProductName = 'Chai' ].

		self shouldnt: [ orderDetailsCursor close ] raise: Exception.

		self shouldnt:
			[ countRows := (db createStaticCursor: 'select count(*) as Counter ',
										  'from Products') upToEnd ] raise: Exception.

		self should: [ rows size = countRows first Counter ].

		self shouldnt: [ nestedTransaction := db readOnlyTransaction ] raise: Exception.

		self should: [ transaction ~= nestedTransaction ].

		nestedTransaction try:
			[ self should: [ db transaction ~= nestedTransaction ] ] ].

	self should: [ db transaction isNil ].!

testDate
	"Test to see if date values can be read from the database successfully."

	| cursor rows |

	self
		shouldnt:
			[ db readOnlyTransaction try:
				[ cursor := db createStaticCursor: 'select  EmployeeID, LastName, FirstName, BirthDate, HireDate ',
										'from Employees ',
										'where LastName = ''Davolio'''.
				rows := cursor upToEnd ] ]
		raise: Exception.

	self should: [ rows first BirthDate date = (Date fromString: '12/08/1948') ].
	self should: [ rows first BirthDate date odbcString = '1948-12-08' ].!

testErrors
	| r f |

	r := RioRestartableError new.
	self shouldnt: [ r isFatal ].
	self should: [ r isRestartable ].

	f := RioFatalError new.
	self should: [ f isFatal ].
	self shouldnt: [ f isRestartable ]!

testExternalBuffer
	"Test the functioning of the RioExternalBuffer class"

	| anInstance tempArray aString f |

	anInstance := RioExternalBuffer new: 20.

	"Test single byte access"

	0 to: anInstance size - 1 do: [ :i | self shouldnt: [ anInstance byteAtOffset: i put: i ] raise: Exception ].
	0 to: anInstance size - 1 do: [ :i | self should: [ (anInstance byteAtOffset: i) = i ] ].

	"Test buffer clearing"

	anInstance clear.

	0 to: anInstance size - 1 do: [ :i | self should: [ (anInstance byteAtOffset: i) = 0 ] ].

	"Test byte group access"

	anInstance clear.

	0 to: anInstance size - 3 do:
		[ :startOffset |
		startOffset to: anInstance size - 3 by: 3 do:
			[ :i |
			self shouldnt: [ tempArray := ByteArray with: i with: i+1 with: i+2.
					    anInstance bytesAtOffset: i count: tempArray size put: tempArray ] raise: Exception ].

		startOffset to: anInstance size - 3 by: 3 do:
			[ :i |
			self should: [ (anInstance bytesAtOffset: i count: 3) = (ByteArray with: i with: i+1 with: i+2) ] ] ].

	"Test double access"

	anInstance clear.
	f := Float new.

	0 to: anInstance size - f size do:
		[ :startOffset |
		startOffset to: anInstance size - f size by: f size do:
			[ :i |
			self shouldnt: [ anInstance doubleAtOffset: i put: i * 1.101 ] raise: Exception ].

		startOffset to: anInstance size - f size by: f size do:
			[ :i |
			self should: [ (anInstance doubleAtOffset: i) = (i * 1.101) ] ] ].

	"Test dword access"

	anInstance clear.

	0 to: anInstance size - 4 do:
		[ :startOffset |
		startOffset to: anInstance size - 4 by: 4 do:
			[ :i |
			self shouldnt: [ anInstance dwordAtOffset: i put: i * 12345678 ] raise: Exception ].

		startOffset to: anInstance size - 4 by: 4 do:
			[ :i |
			self should: [ (anInstance dwordAtOffset: i) = ( i * 12345678) ] ] ].

	"Test sdword access"

	anInstance clear.

	0 to: anInstance size - 4 do:
		[ :startOffset |
		startOffset to: anInstance size - 4 by: 4 do:
			[ :i |
			self shouldnt: [ anInstance sdwordAtOffset: i put: i * -12345678 ] raise: Exception ].

		startOffset to: anInstance size - 4 by: 4 do:
			[ :i |
			self should: [ (anInstance sdwordAtOffset: i) = ( i * -12345678) ] ] ].

	"Test string access"

	anInstance clear.

	aString := 'abcdefghijzyxwvutsrq'.

	0 to: anInstance size - aString size do:
		[ :startOffset |
		startOffset to: anInstance size - aString size by: aString size do:
			[ :i |
			self shouldnt: [ anInstance stringAtOffset: i count: aString size put: aString ] raise: Exception ].

		startOffset to: anInstance size - aString size by: aString size do:
			[ :i |
			self should: [ (anInstance stringAtOffset: i count: aString size) = aString ] ] ].

	"Test sword access"

	anInstance clear.

	0 to: anInstance size - 2 do:
		[ :startOffset |
		startOffset to: anInstance size - 2 by: 2 do:
			[ :i |
			self shouldnt: [ anInstance swordAtOffset: i put: i * -1234 ] raise: Exception ].

		startOffset to: anInstance size - 2 by: 2 do:
			[ :i |
			self should: [ (anInstance swordAtOffset: i) = ( i * -1234) ] ] ].

	"Test word access"

	anInstance clear.

	0 to: anInstance size - 2 do:
		[ :startOffset |
		startOffset to: anInstance size - 2 by: 2 do:
			[ :i |
			self shouldnt: [ anInstance wordAtOffset: i put: i * 1234 ] raise: Exception ].

		startOffset to: anInstance size - 2 by: 2 do:
			[ :i |
			self should: [ (anInstance wordAtOffset: i) = ( i * 1234) ] ] ]!

testForeignKeys
	| rows |

	(db supportsFunction: (RioConstants current sqlApiSqlforeignkeys)) ifTrue:
		[self shouldnt: [ rows := (RioMetadataCursor on: db) foreignKeysWithPkCatalog: nil pkSchema: nil pkTable: '%' fkCatalog: nil fkSchema: nil fkTable: '%' ] raise: Exception ].
!

testFunctions
	| functionSymbols |

	functionSymbols := #(sqlApiSqlallocconnect sqlApiSqlallocenv sqlApiSqlallocstmt sqlApiSqlbindcol
	   sqlApiSqlcancel sqlApiSqlcolattributes sqlApiSqlconnect sqlApiSqldescribecol
	   sqlApiSqldisconnect sqlApiSqlerror sqlApiSqlexecdirect sqlApiSqlexecute
	   sqlApiSqlfetch sqlApiSqlfreeconnect sqlApiSqlfreeenv sqlApiSqlfreestmt
	   sqlApiSqlgetcursorname sqlApiSqlnumresultcols sqlApiSqlprepare sqlApiSqlrowcount
	   sqlApiSqlsetcursorname sqlApiSqlsetparam sqlApiSqltransact sqlApiSqlbindparameter
	   sqlApiSqlcolumns sqlApiSqldriverconnect sqlApiSqlgetconnectoption sqlApiSqlgetdata
	   sqlApiSqlgetfunctions sqlApiSqlgetinfo sqlApiSqlgetstmtoption sqlApiSqlgettypeinfo
	   sqlApiSqlparamdata sqlApiSqlputdata sqlApiSqlsetconnectoption sqlApiSqlsetstmtoption
	   sqlApiSqlspecialcolumns sqlApiSqlstatistics sqlApiSqltables sqlApiSqlbrowseconnect
	   sqlApiSqlcolumnprivileges sqlApiSqldatasources sqlApiSqldescribeparam sqlApiSqldrivers
	   sqlApiSqlextendedfetch sqlApiSqlforeignkeys sqlApiSqlmoreresults sqlApiSqlnativesql
	   sqlApiSqlnumparams sqlApiSqlparamoptions sqlApiSqlprimarykeys sqlApiSqlprocedurecolumns
	   sqlApiSqlprocedures sqlApiSqlsetpos sqlApiSqlsetscrolloptions sqlApiSqltableprivileges).

	functionSymbols do: [ :func |
			self should: [ (db supportsFunction: (RioConstants current perform: func)) isKindOf: Boolean ] ]!

testInfo
	self
		translateBitmaskFor: #sqlAlterTable
			withSymbols: #(sqlAtAddColumn sqlAtDropColumn);
		translateBitmaskFor: #sqlBookmarkPersistence
			withSymbols: #(sqlBpClose sqlBpDelete sqlBpDrop sqlBpScroll
						sqlBpTransaction sqlBpUpdate sqlBpOtherHstmt);
		translateBitmaskFor: #sqlConvertFunctions withSymbols: #(sqlFnCvtConvert);
		translateValueFor: #sqlCorrelationName
			withSymbols: #(sqlCnNone sqlCnDifferent sqlCnAny);
		translateValueFor: #sqlCursorCommitBehavior
			withSymbols: #(sqlCbDelete sqlCbClose sqlCbPreserve);
		translateValueFor: #sqlCursorRollbackBehavior
			withSymbols: #(sqlCbDelete sqlCbClose sqlCbPreserve);
		translateValueFor: #sqlDefaultTxnIsolation
			withSymbols: #(sqlTxnReadUncommitted sqlTxnReadCommitted
						sqlTxnRepeatableRead sqlTxnSerializable sqlTxnVersioning);
		translateBitmaskFor: #sqlFetchDirection
			withSymbols: #(sqlFdFetchNext sqlFdFetchFirst sqlFdFetchLast sqlFdFetchPrior
						sqlFdFetchAbsolute sqlFdFetchRelative sqlFdFetchResume
						sqlFdFetchBookmark);
		translateValueFor: #sqlFileUsage
			withSymbols: #(sqlFileNotSupported sqlFileTable sqlFileQualifier);
		translateBitmaskFor: #sqlGetdataExtensions
			withSymbols: #(sqlGdAnyColumn sqlGdAnyOrder sqlGdAnyColumn
						sqlGdBlock sqlGdBound);
		translateValueFor: #sqlGroupBy
			withSymbols: #(sqlGbNotSupported sqlGbGroupByEqualsSelect
						sqlGbGroupByContainsSelect sqlGbNoRelation);
		translateValueFor: #sqlIdentifierCase
			withSymbols: #(sqlIcUpper sqlIcLower sqlIcSensitive sqlIcMixed);
		translateBitmaskFor: #sqlLockTypes
			withSymbols: #(sqlLckNoChange sqlLckExclusive sqlLckUnlock);
		translateValueFor: #sqlNonNullableColumns
			withSymbols: #(sqlNncNull sqlNncNonNull);
		translateValueFor: #sqlNullCollation
			withSymbols: #(sqlNcEnd sqlNcHigh sqlNcLow sqlNcStart);
		translateBitmaskFor: #sqlNumericFunctions
			withSymbols: #(sqlFnNumAbs sqlFnNumAcos sqlFnNumAsin
						sqlFnNumAtan sqlFnNumAtan2 sqlFnNumCeiling
						sqlFnNumCos sqlFnNumCot sqlFnNumDegrees
						sqlFnNumExp sqlFnNumFloor sqlFnNumLog
						sqlFnNumLog10 sqlFnNumMod sqlFnNumPi
						sqlFnNumPower sqlFnNumRadians sqlFnNumRand
						sqlFnNumRound	 sqlFnNumSign sqlFnNumSin
						sqlFnNumSqrt sqlFnNumTan sqlFnNumTruncate);
		translateValueFor: #sqlOdbcApiConformance
			withSymbols: #(sqlOacNone sqlOacLevel1 sqlOacLevel2);
		translateValueFor: #sqlOdbcSagCliConformance
			withSymbols: #(sqlOsccNotCompliant sqlOsccCompliant);
		translateValueFor: #sqlOdbcSqlConformance
			withSymbols: #(sqlOscMinimum sqlOscCore sqlOscExtended);
		translateBitmaskFor: #sqlOwnerUsage
			withSymbols: #(sqlOuDmlStatements sqlOuProcedureInvocation
						sqlOuTableDefinition sqlOuIndexDefinition);
		translateBitmaskFor: #sqlPosOperations
			withSymbols: #(sqlPosPosition sqlPosRefresh sqlPosUpdate
						sqlPosDelete sqlPosAdd);
		translateBitmaskFor: #sqlPositionedStatements
			withSymbols: #(sqlPsPositionedDelete sqlPsPositionedUpdate
						sqlPsSelectForUpdate);
		translateValueFor: #sqlQualifierLocation
			withSymbols: #(sqlQlStart sqlQlEnd);
		translateBitmaskFor: #sqlQualifierUsage
			withSymbols: #(sqlQuDmlStatements sqlQuProcedureInvocation
						sqlQuTableDefinition sqlQuIndexDefinition);
		translateBitmaskFor: #sqlQuotedIdentifierCase
			withSymbols: #(sqlIcUpper sqlIcLower sqlIcSensitive sqlIcMixed);
		translateBitmaskFor: #sqlScrollConcurrency
			withSymbols: #(sqlSccoReadOnly sqlSccoLock sqlSccoOptRowver
						sqlSccoOptValues);
		translateBitmaskFor: #sqlScrollOptions
			withSymbols: #(sqlSoForwardOnly sqlSoStatic sqlSoKeysetDriven
						sqlSoDynamic sqlSoMixed);
		translateBitmaskFor: #sqlStaticSensitivity
			withSymbols: #(sqlSsAdditions sqlSsDeletions sqlSsUpdates);
		translateBitmaskFor: #sqlStringFunctions
			withSymbols: #(sqlFnStrAscii sqlFnStrChar sqlFnStrConcat
						sqlFnStrDifference sqlFnStrInsert sqlFnStrLcase
						sqlFnStrLeft sqlFnStrLength sqlFnStrLocate
						sqlFnStrLocate2 sqlFnStrLtrim sqlFnStrRepeat
						sqlFnStrReplace sqlFnStrRight sqlFnStrRtrim
						sqlFnStrSoundex sqlFnStrSpace sqlFnStrSubstring
						sqlFnStrUcase);
		translateBitmaskFor: #sqlSubqueries
			withSymbols: #(sqlSqCorrelatedSubqueries sqlSqComparison
						sqlSqExists sqlSqIn sqlSqQuantified);
		translateBitmaskFor: #sqlSystemFunctions
			withSymbols: #(sqlFnSysDbname sqlFnSysIfnull sqlFnSysUsername);
		translateBitmaskFor: #sqlTimedateAddIntervals
			withSymbols: #(sqlFnTsiFracSecond sqlFnTsiSecond sqlFnTsiMinute
						sqlFnTsiHour sqlFnTsiDay sqlFnTsiWeek sqlFnTsiMonth
						sqlFnTsiQuarter sqlFnTsiYear);
		translateBitmaskFor: #sqlTimedateDiffIntervals
			withSymbols: #(sqlFnTsiFracSecond sqlFnTsiSecond sqlFnTsiMinute
						sqlFnTsiHour sqlFnTsiDay sqlFnTsiWeek sqlFnTsiMonth
						sqlFnTsiQuarter sqlFnTsiYear);
		translateBitmaskFor: #sqlTimedateFunctions
			withSymbols: #(sqlFnTdCurdate sqlFnTdCurtime sqlFnTdDayname
						sqlFnTdDayofmonth sqlFnTdDayofweek sqlFnTdDayofyear
						sqlFnTdHour sqlFnTdMinute sqlFnTdMonth
						sqlFnTdMonthname sqlFnTdNow sqlFnTdQuarter
						sqlFnTdSecond sqlFnTdTimestampadd sqlFnTdTimestampdiff
						sqlFnTdWeek sqlFnTdYear);
		translateValueFor: #sqlTxnCapable
			withSymbols: #(sqlTcNone sqlTcDml sqlTcDdlCommit sqlTcDdlIgnore sqlTcAll);
		translateBitmaskFor: #sqlTxnIsolationOption
			withSymbols: #(sqlTxnReadUncommitted sqlTxnReadCommitted
						sqlTxnRepeatableRead sqlTxnSerializable sqlTxnVersioning);
		translateBitmaskFor: #sqlUnion
			withSymbols: #(sqlUUnion sqlUUnionAll)
!

testIntegerParameter
	| stmt results |

	self shouldnt: [ stmt := db createStaticCursor: 'select * from "Order Details" where OrderId=?' ] raise: Exception.
	self shouldnt: [ stmt setParameter: 1 with: (RioIntegerParameter new value: 10255) ] raise: Exception.
	db readOnlyTransaction try: [
		self shouldnt: [ stmt execute ] raise: Exception.
		self shouldnt: [ results := stmt upToEnd ] raise: Exception ].
	self should: [ results size = 4 ].!

testInternalBuffer
	"Test the functioning of the RioInternalBuffer class"

	| anInstance tempArray aString f |

	anInstance := RioInternalBuffer new: 20.

	"Test single byte access"

	0 to: anInstance size - 1 do: [ :i | self shouldnt: [ anInstance byteAtOffset: i put: i ] raise: Exception ].
	0 to: anInstance size - 1 do: [ :i | self should: [ (anInstance byteAtOffset: i) = i ] ].

	"Test buffer clearing"

	anInstance clear.

	0 to: anInstance size - 1 do: [ :i | self should: [ (anInstance byteAtOffset: i) = 0 ] ].

	"Test byte group access"

	anInstance clear.

	0 to: anInstance size - 3 do:
		[ :startOffset |
		startOffset to: anInstance size - 3 by: 3 do:
			[ :i |
			self shouldnt: [ tempArray := ByteArray with: i with: i+1 with: i+2.
					    anInstance bytesAtOffset: i count: tempArray size put: tempArray ] raise: Exception ].

		startOffset to: anInstance size - 3 by: 3 do:
			[ :i |
			self should: [ (anInstance bytesAtOffset: i count: 3) = (ByteArray with: i with: i+1 with: i+2) ] ] ].

	"Test double access"

	anInstance clear.
	f := Float new.

	0 to: anInstance size - f size do:
		[ :startOffset |
		startOffset to: anInstance size - f size by: f size do:
			[ :i |
			self shouldnt: [ anInstance doubleAtOffset: i put: i * 1.101 ] raise: Exception ].

		startOffset to: anInstance size - f size by: f size do:
			[ :i |
			self should: [ (anInstance doubleAtOffset: i) = (i * 1.101) ] ] ].

	"Test dword access"

	anInstance clear.

	0 to: anInstance size - 4 do:
		[ :startOffset |
		startOffset to: anInstance size - 4 by: 4 do:
			[ :i |
			self shouldnt: [ anInstance dwordAtOffset: i put: i * 12345678 ] raise: Exception ].

		startOffset to: anInstance size - 4 by: 4 do:
			[ :i |
			self should: [ (anInstance dwordAtOffset: i) = ( i * 12345678) ] ] ].

	"Test sdword access"

	anInstance clear.

	0 to: anInstance size - 4 do:
		[ :startOffset |
		startOffset to: anInstance size - 4 by: 4 do:
			[ :i |
			self shouldnt: [ anInstance sdwordAtOffset: i put: i * -12345678 ] raise: Exception ].

		startOffset to: anInstance size - 4 by: 4 do:
			[ :i |
			self should: [ (anInstance sdwordAtOffset: i) = ( i * -12345678) ] ] ].

	"Test string access"

	anInstance clear.

	aString := 'abcdefghijzyxwvutsrq'.

	0 to: anInstance size - aString size do:
		[ :startOffset |
		startOffset to: anInstance size - aString size by: aString size do:
			[ :i |
			self shouldnt: [ anInstance stringAtOffset: i count: aString size put: aString ] raise: Exception ].

		startOffset to: anInstance size - aString size by: aString size do:
			[ :i |
			self should: [ (anInstance stringAtOffset: i count: aString size) = aString ] ] ].

	"Test sword access"

	anInstance clear.

	0 to: anInstance size - 2 do:
		[ :startOffset |
		startOffset to: anInstance size - 2 by: 2 do:
			[ :i |
			self shouldnt: [ anInstance swordAtOffset: i put: i * -1234 ] raise: Exception ].

		startOffset to: anInstance size - 2 by: 2 do:
			[ :i |
			self should: [ (anInstance swordAtOffset: i) = ( i * -1234) ] ] ].

	"Test word access"

	anInstance clear.

	0 to: anInstance size - 2 do:
		[ :startOffset |
		startOffset to: anInstance size - 2 by: 2 do:
			[ :i |
			self shouldnt: [ anInstance wordAtOffset: i put: i * 1234 ] raise: Exception ].

		startOffset to: anInstance size - 2 by: 2 do:
			[ :i |
			self should: [ (anInstance wordAtOffset: i) = ( i * 1234) ] ] ]
!

testOptions
	"Private"

	| retval |

	self shouldnt: [ retval := db connectOption: RioConstants current sqlAccessMode ] raise: RioError.
	self should: [ (RioConstants current sqlModeReadWrite) = retval ].

	self shouldnt: [ retval := db connectOption: RioConstants current sqlAutocommit ] raise: RioError.
	self should: [ #(##(RioConstants current sqlAutocommitOff)
			      ##(RioConstants current sqlAutocommitOn)) includes: retval ].

	"Try turning auto-commit mode off"

	self shouldnt: [ retval := db connectOption: RioConstants current sqlAutocommit
						value: RioConstants current sqlAutocommitOff ] raise: RioError.
	self should: [ db retcode = RioConstants current sqlSuccess ].

	"Check to see that auto-commit mode is now off"

	self shouldnt: [ retval := db connectOption: RioConstants current sqlAutocommit ] raise: RioError.
	self should: [ (RioConstants current sqlAutocommitOff) = retval ].

	self shouldnt: [ retval := db connectOption: RioConstants current sqlCurrentQualifier ] raise: RioError.

	"retval := db connectOption: SqlLoginTimeout.
	Transcript show: 'SqlLoginTimeout=';
			show: retval printString; cr."

	self shouldnt: [ retval := db connectOption: RioConstants current sqlOdbcCursors ] raise: RioError.
	self should: [ #(##(RioConstants current sqlCurUseIfNeeded)
			      ##(RioConstants current sqlCurUseOdbc)
			      ##(RioConstants current sqlCurUseDriver)) includes: retval ].

	self shouldnt: [ retval := db connectOption: RioConstants current sqlOptTrace ] raise: RioError.
	self should: [ #(##(RioConstants current sqlOptTraceOff)
			      ##(RioConstants current sqlOptTraceOn)) includes: retval ].

	self shouldnt: [ retval := db connectOption: RioConstants current sqlOptTracefile ] raise: RioError.

	"retval := db connectOption: SqlPacketSize.
	Transcript show: 'SqlPacketSize=';
			show: retval printString; cr."

	self shouldnt: [ retval := db connectOption: RioConstants current sqlQuietMode ] raise: RioError.

	"retval := db connectOption: SqlTranslateDll.
	Transcript show: 'SqlTranslateDll=';
			show: retval printString; cr."

	"retval := db connectOption: SqlTranslateOption.
	Transcript show: 'SqlTranslateOption=';
			show: retval printString; cr."

	self shouldnt: [ retval := db connectOption: RioConstants current sqlTxnIsolation ] raise: RioError.
	self should: [ #(##(RioConstants current sqlTxnReadUncommitted)
			      ##(RioConstants current sqlTxnReadCommitted)
			      ##(RioConstants current sqlTxnRepeatableRead)
			      ##(RioConstants current sqlTxnSerializable)
			      ##(RioConstants current sqlTxnVersioning)) includes: retval ].
!

testPrimaryKeys
	| rows |
	(db supportsFunction: (RioConstants current sqlApiSqlprimarykeys)) ifTrue:
		[ self shouldnt: [ rows := (RioMetadataCursor on: db) primaryKeysInCatalog: nil schema: nil table: '%' ] raise: Exception ]
!

testProcedureColumns
	| rows |
	(db supportsFunction: (RioConstants current sqlApiSqlprocedurecolumns)) ifTrue:
		[ self shouldnt: [ rows := (RioMetadataCursor on: db) procedureColumnsInCatalog: nil schema: nil procedure: '%'  column: nil ] raise: Exception ]
!

testSpecialColumnsBestRowid
	| rows aRow |
	(db supportsFunction: (RioConstants current sqlApiSqlspecialcolumns)) ifTrue:
		[ self shouldnt: [ rows := (RioMetadataCursor on: db) specialColumnsOfType: (RioConstants current sqlBestRowid) catalog: nil schema: nil table: 'Order Details'  scope: nil nullable: nil] raise: Exception.
		self should: [ rows size = 2 ].
		self shouldnt: [ aRow := rows at: 1 ] raise: Exception.
		self should: [ aRow containsColumnNamed: 'SCOPE' ].
		self should: [ aRow containsColumnNamed: 'COLUMN_NAME' ].
		self should: [ aRow containsColumnNamed: 'DATA_TYPE' ].
		self should: [ aRow containsColumnNamed: 'TYPE_NAME' ].

		db odbcMajorVersion = 2 ifTrue:
			[ self should: [ aRow containsColumnNamed: 'PRECISION' ].
			self should: [ aRow containsColumnNamed: 'LENGTH' ].
			self should: [ aRow containsColumnNamed: 'SCALE' ] ].

		db odbcMajorVersion = 3 ifTrue:
			[ self should: [ aRow containsColumnNamed: 'COLUMN_SIZE' ].
			self should: [ aRow containsColumnNamed: 'BUFFER_LENGTH' ].
			self should: [ aRow containsColumnNamed: 'DECIMAL_DIGITS' ] ].

		self should: [ aRow containsColumnNamed: 'PSEUDO_COLUMN' ] ]!

testSpecialColumnsRowver
	| rows aRow |
	(db supportsFunction: (RioConstants current sqlApiSqlspecialcolumns)) ifTrue:
		[ self shouldnt: [ rows := (RioMetadataCursor on: db) specialColumnsOfType: (RioConstants current sqlRowver) catalog: nil schema: nil table: 'Order Details'  scope: nil nullable: nil] raise: Exception.
		self should: [ rows size = 0 ] ]!

testSqlGenerator
	| g |

	self shouldnt:
		[ (g := RioSqlGenerator new)
			tableName: 'Employees';
			columnNames: #('LastName' 'FirstName' 'Title' 'Address' 'City' 'PostalCode' 'Country');
			whereClause: 'Region=?' ] raise: Exception.

	self should: [ g generateDeleteStatement = 'DELETE FROM Employees WHERE Region=?' ].
	self should: [ g generateInsertStatement = ('INSERT INTO Employees (LastName,FirstName,Title,Address,',
									'City,PostalCode,Country) VALUES (?,?,?,?,?,?,?)') ].
	self should: [ g generateUpdateStatement = ('UPDATE Employees SET LastName=?,FirstName=?,Title=?,',
									'Address=?,City=?,PostalCode=?,Country=? WHERE Region=?') ].
	self should: [ g generateSelectStatement = ('SELECT LastName,FirstName,Title,Address,City,PostalCode,',
									'Country FROM Employees WHERE Region=?') ].!

testStatistics
	| rows aRow |
	(db supportsFunction: (RioConstants current sqlApiSqlstatistics)) ifTrue:
		[ self shouldnt: [ rows := (RioMetadataCursor on: db) statisticsForCatalog: nil schema: nil table: 'Order Details' unique: (RioConstants current sqlIndexAll) accuracy: (RioConstants current sqlEnsure) ] raise: Exception.
		self should: [ rows size = 7 ].
		self shouldnt: [ aRow := rows at: 1 ] raise: Exception.

		db odbcMajorVersion = 2 ifTrue:
			[ aRow containsColumnNamed: 'TABLE_QUALIFIER'.
			aRow containsColumnNamed: 'TABLE_OWNER'.
			aRow containsColumnNamed: 'SEQ_IN_INDEX'.
			aRow containsColumnNamed: 'COLLATION' ].

		db odbcMajorVersion = 3 ifTrue:
			[ aRow containsColumnNamed: 'TABLE_CAT'.
			aRow containsColumnNamed: 'TABLE_SCHEM'.
			aRow containsColumnNamed: 'ORDINAL_POSITION'.
			aRow containsColumnNamed: 'ASC_OR_DESC' ].

		aRow containsColumnNamed: 'TABLE_NAME'.
		aRow containsColumnNamed: 'NON_UNIQUE'.
		aRow containsColumnNamed: 'INDEX_QUALIFIER'.
		aRow containsColumnNamed: 'INDEX_NAME'.
		aRow containsColumnNamed: 'TYPE'.
		aRow containsColumnNamed: 'COLUMN_NAME'.
		aRow containsColumnNamed: 'CARDINALITY'.
		aRow containsColumnNamed: 'PAGES'.
		aRow containsColumnNamed: 'FILTER_CONDITION' ]!

testStmtUpToEnd
	| rows countRows |

	db readOnlyTransaction try:
		[ self shouldnt: [ rows := (db createStaticCursor: 'select * from Orders') upToEnd ] raise: Exception.
		self shouldnt: [ countRows := (db createStaticCursor: 'select count(*) as Counter ',
											'from Orders') upToEnd ] raise: Exception.
		self should: [ rows size = countRows first Counter ] ]!

testStreaming
	| stmt results |

	self shouldnt: [ stmt := RioForwardCursor on: db selectForColumns: #('*') fromTable: '"order details"' withKeys: nil ]
		raise: Exception.

	self shouldnt: [ (RioReadOnlyTransaction on: db) try: [ results := stmt upToEnd ] ] raise: Exception.

	self should: [ results size > 0 ].!

testStringParameter
	| stmt results |

	self shouldnt: [ stmt := db createStaticCursor: 'select * from "Order Details" where OrderId=?' ] raise: Exception.
	self shouldnt: [ stmt setParameter: 1 value: 10255 ] raise: Exception.
	db readOnlyTransaction try: [
		self shouldnt: [ stmt execute ] raise: Exception.
		self shouldnt: [ results := stmt upToEnd ] raise: Exception ].
	self should: [ results size = 4 ].!

testTablePrivileges
	| rows aRow |
	(db supportsFunction: (RioConstants current sqlApiSqltableprivileges)) ifTrue:
		[ self shouldnt: [ rows := (RioMetadataCursor on: db) tablePrivilegesForCatalog: nil schema: nil table: 'Order Details' unique: (RioConstants current sqlIndexAll) accuracy: (RioConstants current sqlEnsure) ] raise: Exception ]!

testTablesInCatalog
	| rows aRow |
	self shouldnt: [ rows := (RioMetadataCursor on: db) tablesInCatalog: '' schema: '' table: '' type: '%' ] raise: Exception.
	self should: [ rows size = 4 ].
	self shouldnt: [ aRow := rows at: 1 ] raise: Exception.

	db odbcMajorVersion = 2 ifTrue:
		[ self should: [ aRow containsColumnNamed: 'TABLE_QUALIFIER' ].
		self should: [ aRow containsColumnNamed: 'TABLE_OWNER' ] ].

	db odbcMajorVersion = 3 ifTrue:
		[ self should: [ aRow containsColumnNamed: 'TABLE_CAT' ].
		self should: [ aRow containsColumnNamed: 'TABLE_SCHEM' ] ].

	self should: [ aRow containsColumnNamed: 'TABLE_NAME' ].
	self should: [ aRow containsColumnNamed: 'TABLE_TYPE' ].
	self should: [ aRow containsColumnNamed: 'REMARKS' ]!

testTableTypes
	| tableTypes |
	self should: [ (tableTypes := db tableTypes) size = 4 ]!

testTime
	"Test to see if time values can be read from the database successfully."

	| cursor rows |

	self
		shouldnt:
			[ db readOnlyTransaction try:
				[ cursor := db createStaticCursor: 'select  EmployeeID, LastName, FirstName, BirthDate, HireDate ',
										'from Employees ',
										'where LastName = ''Davolio'''.
				rows := cursor upToEnd ] ]
		raise: Exception.

	self should: [ rows first BirthDate time = (Time fromString: '00:00:00') ].
	self should: [ rows first BirthDate time odbcString = '00:00:00' ].!

testTimeStamp
	"Test to see if timestamp values can be read from the database successfully."

	| cursor rows |

	self
		shouldnt:
			[ db readOnlyTransaction try:
				[ cursor := db createStaticCursor: 'select  EmployeeID, LastName, FirstName, BirthDate, HireDate ',
										'from Employees ',
										'where LastName = ''Davolio'''.
				rows := cursor upToEnd ] ]
		raise: Exception.

	self should: [ rows first BirthDate = (TimeStamp fromOdbcString: '1948-12-08 00:00:00') ].
	self should: [ rows first BirthDate odbcString = '1948-12-08 00:00:00' ].!

testTransaction
	| transaction restartTransaction |

	restartTransaction := false.
	transaction := RioTestTransaction on: db.
	transaction try: [ restartTransaction := self basicDbTest ]	"End of transaction block"
		on: RioError do:
			[ :dbError |
			db transaction == transaction
				ifTrue:	"Error occurred - transaction started in the guarded block"
					[ dbError isFatal ifTrue:
						[ restartTransaction := false.
						Transcript show: 'A fatal database error occurred  text=' ].

					dbError isRestartable ifTrue:
						[ Transcript show: 'A restartable error occurred  text='].

					Transcript show: dbError messageText; cr.
					RioResult do: [ :aResult | Transcript show: aResult printString; cr].

					db rollback ]
				ifFalse:	"Error occurred - transaction started elsewhere - re-signal to outer handler"
					[ dbError class signal: dbError messageText] ].   "End of error handling block"

	^false | restartTransaction!

testTypeInfo
	| rows aRow |
	(db supportsFunction: (RioConstants current sqlApiSqlgettypeinfo)) ifTrue:
		[ self shouldnt: [ rows := (RioMetadataCursor on: db) typeInfoFor: (RioConstants current sqlAllTypes) ] raise: Exception.
		self should: [ rows size = 16 ].
		self shouldnt: [ aRow := rows at: 1 ] raise: Exception.

		db odbcMajorVersion = 2 ifTrue:
			[ self should: [ aRow containsColumnNamed: 'PRECISION' ].
			self should: [ aRow containsColumnNamed: 'MONEY' ].
			self should: [ aRow containsColumnNamed: 'AUTO_INCREMENT' ] ].

		db odbcMajorVersion = 3 ifTrue:
			[ self should: [ aRow containsColumnNamed: 'COLUMN_SIZE' ].
			self should: [ aRow containsColumnNamed: 'FIXED_PREC_SCALE' ].
			self should: [ aRow containsColumnNamed: 'AUTO_UNIQUE_VALUE' ].
			self should: [ aRow containsColumnNamed: 'SQL_DATA_TYPE' ].
			self should: [ aRow containsColumnNamed: 'SQL_DATETIME_SUB' ].
			self should: [ aRow containsColumnNamed: 'INTERVAL_PRECISION' ].
			self should: [ aRow containsColumnNamed: 'NUM_PREC_RADIX' ] ].

		self should: [ aRow containsColumnNamed: 'TYPE_NAME' ].
		self should: [ aRow containsColumnNamed: 'DATA_TYPE' ].
		self should: [ aRow containsColumnNamed: 'LITERAL_PREFIX' ].
		self should: [ aRow containsColumnNamed: 'LITERAL_SUFFIX' ].
		self should: [ aRow containsColumnNamed: 'CREATE_PARAMS' ].
		self should: [ aRow containsColumnNamed: 'NULLABLE' ].
		self should: [ aRow containsColumnNamed: 'CASE_SENSITIVE' ].
		self should: [ aRow containsColumnNamed: 'SEARCHABLE' ].
		self should: [ aRow containsColumnNamed: 'UNSIGNED_ATTRIBUTE' ].
		self should: [ aRow containsColumnNamed: 'LOCAL_TYPE_NAME' ].
		self should: [ aRow containsColumnNamed: 'MINIMUM_SCALE' ].
		self should: [ aRow containsColumnNamed: 'MAXIMUM_SCALE' ] ]!

translateBitmask: anInteger withSymbols: anArrayOfSymbols
	| aString s |

	aString := String new.
	s := aString writeStream.

	anArrayOfSymbols do:
		[ :each |
		(anInteger bitAnd: (RioConstants current perform: each)) ~= 0 ifTrue:
			[ s position > 0 ifTrue: [ s nextPutAll: ' | ' ].
			s nextPutAll: each asString ] ].

	s close.
	aString trimNulls.

	aString isEmpty ifTrue: [ aString := '(None)' ].

	^aString!

translateBitmaskFor: aSymbol withSymbols: anArrayOfSymbols
	| retval |

	self should: [ (retval := db integerInfo: (RioConstants current perform: aSymbol)) isKindOf: Integer ].
	self should: [ (self translateBitmask: retval withSymbols: anArrayOfSymbols) isKindOf: String ]!

translateValue: anInteger withSymbols: anArrayOfSymbols
	| aString |

	aString := String new.

	anArrayOfSymbols do: [ :each |
		anInteger = (RioConstants current perform: each)
			ifTrue: [ ^each asString ] ].
	^aString!

translateValueFor: aSymbol withSymbols: anArrayOfSymbols
	self should: [ (db integerInfo: (RioConstants current perform: aSymbol)) isKindOf: Integer ]! !
!RioTest categoriesFor: #basicDbTest!private!testing! !
!RioTest categoriesFor: #setUp!public!Running! !
!RioTest categoriesFor: #tearDown!public!Running! !
!RioTest categoriesFor: #testAutoInsertStatement!public!testing! !
!RioTest categoriesFor: #testAutoUpdateStatement!public!testing! !
!RioTest categoriesFor: #testCatalogs!public!testing! !
!RioTest categoriesFor: #testColumnHeader!public!testing! !
!RioTest categoriesFor: #testColumnsInCatalog!public!testing! !
!RioTest categoriesFor: #testConnectionInformation!public!testing! !
!RioTest categoriesFor: #testConversions!public!testing! !
!RioTest categoriesFor: #testCursor!public!testing! !
!RioTest categoriesFor: #testDate!public!testing! !
!RioTest categoriesFor: #testErrors!public!testing! !
!RioTest categoriesFor: #testExternalBuffer!public!testing! !
!RioTest categoriesFor: #testForeignKeys!public!testing! !
!RioTest categoriesFor: #testFunctions!public!testing! !
!RioTest categoriesFor: #testInfo!public!testing! !
!RioTest categoriesFor: #testIntegerParameter!public!testing! !
!RioTest categoriesFor: #testInternalBuffer!public!testing! !
!RioTest categoriesFor: #testOptions!public!testing! !
!RioTest categoriesFor: #testPrimaryKeys!public!testing! !
!RioTest categoriesFor: #testProcedureColumns!public!testing! !
!RioTest categoriesFor: #testSpecialColumnsBestRowid!public!testing! !
!RioTest categoriesFor: #testSpecialColumnsRowver!public!testing! !
!RioTest categoriesFor: #testSqlGenerator!public!testing! !
!RioTest categoriesFor: #testStatistics!public!testing! !
!RioTest categoriesFor: #testStmtUpToEnd!public!testing! !
!RioTest categoriesFor: #testStreaming!public!testing! !
!RioTest categoriesFor: #testStringParameter!public!testing! !
!RioTest categoriesFor: #testTablePrivileges!public!testing! !
!RioTest categoriesFor: #testTablesInCatalog!public!testing! !
!RioTest categoriesFor: #testTableTypes!public!testing! !
!RioTest categoriesFor: #testTime!public!testing! !
!RioTest categoriesFor: #testTimeStamp!public!testing! !
!RioTest categoriesFor: #testTransaction!public!testing! !
!RioTest categoriesFor: #testTypeInfo!public!testing! !
!RioTest categoriesFor: #translateBitmask:withSymbols:!helpers!private! !
!RioTest categoriesFor: #translateBitmaskFor:withSymbols:!helpers!private! !
!RioTest categoriesFor: #translateValue:withSymbols:!helpers!private! !
!RioTest categoriesFor: #translateValueFor:withSymbols:!helpers!private! !

"Binary Globals"!

"Resources"!

