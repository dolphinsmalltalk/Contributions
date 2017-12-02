"Filed out from Dolphin Smalltalk 7"!

MitSciUnicodeAPIScanner subclass: #MitSciUnicodeTransformer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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
	
	self transformClassRefsFromCls: UserLibrary msgStr: 'default' to: 'self userLibrary' inAndAllSubClassesOf: MessageBoxAbstract.

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

