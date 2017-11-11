"Filed out from Dolphin Smalltalk 7"!

ODBCLibrary subclass: #ODBCLibraryW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

