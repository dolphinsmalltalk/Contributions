"Filed out from Dolphin Smalltalk 7"!

Object subclass: #MitSciUnicodeAPIScanner
	instanceVariableNames: 'ansiAPIFunctionMethods'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

