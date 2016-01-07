| package |
package := Package name: 'US Dolphin COM Evaluator'.
package paxVersion: 1;
	basicComment: '$id: US Dolphin COM Evaluator 0.008$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This package allows you to use several Dolphin Smalltalk functions from PSPad (http://www.pspad.com/). Simply install this package and copy the DolphinSmalltalk.js file to the editors "Script\JScript" folder.

After (re-)starting PSPad or recompiling the scripts in PSPad you can now perform various actions with the selected Text right within PSPad.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.008'.

package basicScriptAt: #postinstall put: 'DolphinEvaluator
	register;
	registerClassFactory'.
package basicScriptAt: #postuninstall put: 'DolphinEvaluator
	unregister;
	unregisterClassFactory'.

package classNames
	add: #DolphinEvaluator;
	add: #IDolphinEval;
	yourself.

package globalNames
	add: #DolphinEvalLib;
	yourself.

package binaryGlobalNames: (Set new
	add: #DolphinEvalLib;
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\ActiveX\Automation\ActiveX Automation';
	add: '..\..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	add: '..\..\..\Refactory\Refactoring Browser\Environments\RBEnvironments';
	yourself).

package!

"Class Definitions"!

AXDualImp subclass: #DolphinEvaluator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IDispatch subclass: #IDolphinEval
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

DolphinEvaluator guid: (GUID fromString: '{17F98180-2F41-4F63-9FAC-10A8509DEB6A}')!
DolphinEvaluator comment: ''!
!DolphinEvaluator categoriesForClass!Unclassified! !
!DolphinEvaluator methodsFor!

Browse: browse 
	"Implement the IDolphinEval::Browse() interface function."

	(self evaluate: browse) browse .
	^S_OK!

cleanString: aString 
	^aString first = $' 
		ifFalse: [(aString reject: [:each | each = $#]) asSymbol]
		ifTrue: [String readFrom: aString readStream]!

Debug: debug 
	"Implement the IDolphinEval::Debug() interface function."
Compiler evaluate: 'self halt. ' , debug.
^S_OK.
	^E_NOTIMPL.
	#todo	"Implement me"!

Definitions: definitions 
	"Implement the IDolphinEval::Definitions() interface function."


	SmalltalkSystem current browseDefinitionsOf:  (self cleanString: definitions).
	^S_OK!

evaluate: aString
	^(self cleanString: aString)
		ifNotNil: [:text | [Compiler evaluate: text] on: Error do: [:ex | self]]!

Evaluate: evaluate result: result 
	"Implement the IDolphinEval::Evaluate() interface function."

	result value: (self evaluate: evaluate) displayString asBSTR asParameter.
	^S_OK!

interfaceClass
	"Answer the dual interface supported by the receiver."

	^IDolphinEval!

References: references 
	"Implement the IDolphinEval::References() interface function."

	

			| text object |
			text :=  (self cleanString: references).
			object := Smalltalk at: text asSymbol ifAbsent: [text asSymbol].
			SmalltalkSystem current browseReferencesTo: object.
				^S_OK!

Search: search 
	(self searchObject:  search ) searchForInTool: self.
	^S_OK!

searchForClass: aClass 
	^aClass browse!

searchForMethod: aCompiledMethod 
	aCompiledMethod browse!

searchForObject: anObject 
	^anObject inspect!

searchForPackage: aPackage 
	^aPackage browse!

searchForSymbol: aSymbol 
	SmalltalkSystem current browseDefinitionsMatching: (MethodSearch newSelector: aSymbol)
		in: BrowserEnvironment new!

SearchMethodContaining: searchMethodContaining 
	SmalltalkSystem current 
		browseContainingText: searchMethodContaining
		in: SmalltalkSystem current systemEnvironment
		prompt: false.
		^S_OK!

searchObject: aString 
	^
	[| evaluateText |
	aString first isLowerCase ifTrue: [^Symbol findInterned: aString].
	evaluateText := aString copyReplaceAll: '>>' with: '>>#'.
	Compiler evaluate: evaluateText] 
			on: Exception
			do: [:x | Package manager packages detect: [:each | each name = aString] ifNone: []]! !
!DolphinEvaluator categoriesFor: #Browse:!*-in class package!COM Interfaces-IDolphinEval!public! !
!DolphinEvaluator categoriesFor: #cleanString:!*-in class package!helpers!private! !
!DolphinEvaluator categoriesFor: #Debug:!*-in class package!COM Interfaces-IDolphinEval!public! !
!DolphinEvaluator categoriesFor: #Definitions:!*-in class package!COM Interfaces-IDolphinEval!public! !
!DolphinEvaluator categoriesFor: #evaluate:!commands!helpers!private! !
!DolphinEvaluator categoriesFor: #Evaluate:result:!*-in class package!COM Interfaces-IDolphinEval!public! !
!DolphinEvaluator categoriesFor: #interfaceClass!*-in class package!constants!private! !
!DolphinEvaluator categoriesFor: #References:!*-in class package!COM Interfaces-IDolphinEval!public! !
!DolphinEvaluator categoriesFor: #Search:!*-in class package!COM Interfaces-IDolphinEval!public! !
!DolphinEvaluator categoriesFor: #searchForClass:!*-in class package!private! !
!DolphinEvaluator categoriesFor: #searchForMethod:!*-in class package!private! !
!DolphinEvaluator categoriesFor: #searchForObject:!*-in class package!private! !
!DolphinEvaluator categoriesFor: #searchForPackage:!*-in class package!private! !
!DolphinEvaluator categoriesFor: #searchForSymbol:!*-in class package!private! !
!DolphinEvaluator categoriesFor: #SearchMethodContaining:!*-in class package!COM Interfaces-IDolphinEval!public! !
!DolphinEvaluator categoriesFor: #searchObject:!*-in class package!commands!private! !

!DolphinEvaluator class methodsFor!

clsid
	"Answer the receiver's CLSID."

	^CLSID fromString: '{8463EEE9-2C7A-4634-8F2D-D0F88EDFF0CB}'!

progID
	

	^'Dolphin.Evaluator.1'!

register
	"Make the necessary registry entries to expose the receiver as a COM object which can be used by
	other applications. Registry entries for both in-process and out-of-process use are created if the 
	necessary components are present. This (along with #unregister) provides the necessary mechanism 
	for self-registration."

	self register:CLSCTX_INPROC_HANDLER! !
!DolphinEvaluator class categoriesFor: #clsid!*-in class package!constants!private! !
!DolphinEvaluator class categoriesFor: #progID!*-in class package!constants!public! !
!DolphinEvaluator class categoriesFor: #register!*-in class package!operations!public! !

IDolphinEval guid: (IID fromString: '{BE10DD29-DFEC-472E-A8E6-83DE561E1B5D}')!
IDolphinEval comment: '<IDolphinEval> is a wrapper class for the COM interface ''DolphinEval.IDolphinEval'' generated from type information in the ''Dolphin Evalutaion Control 1.0'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"IDolphinEval Interface"

Warning: This comment was automatically generated from the interface''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

[
	object, 
	uuid(BE10DD29-DFEC-472E-A8E6-83DE561E1B5D), 
	helpstring("IDolphinEval Interface"), 
	dual
]
interface IDolphinEval : IDispatch
 {
	[id(0x60020000)]
	HRESULT __stdcall Browse(
		[in]BSTR SmalltalkExpression);
	[id(0x60020001)]
	HRESULT __stdcall Evaluate(
		[in]BSTR SmalltalkExpression,
		[out, retval]BSTR Result);
	[id(0x60020002)]
	HRESULT __stdcall Debug(
		[in]BSTR SmalltalkExpression);
	[id(0x60020003)]
	HRESULT __stdcall Reformat(
		[in]BSTR SmalltalkExpression,
		[out, retval]BSTR Result);
	[id(0x60020004)]
	HRESULT __stdcall Search(
		[in]BSTR SmalltalkExpression);
	[id(0x60020005)]
	HRESULT __stdcall SearchMethodContaining(
		[in]BSTR SmalltalkExpression);
	[id(0x60020006)]
	HRESULT __stdcall Definitions(
		[in]BSTR SmalltalkExpression);
	[id(0x60020007)]
	HRESULT __stdcall References(
		[in]BSTR SmalltalkExpression);
};
'!
!IDolphinEval categoriesForClass!Unclassified! !
!IDolphinEval methodsFor!

browse: smalltalkExpression
	"Invoke the Browse() method of the COM object."

	^self Browse: smalltalkExpression
!

Browse: smalltalkExpression
	"Private - Invoke the Browse() method of the COM object.

		HRESULT __stdcall Browse(
			[in]BSTR SmalltalkExpression);"

	<virtual stdcall: hresult 8 bstr>
	^self invalidCall!

debug: smalltalkExpression
	"Invoke the Debug() method of the COM object."

	^self Debug: smalltalkExpression
!

Debug: smalltalkExpression
	"Private - Invoke the Debug() method of the COM object.

		HRESULT __stdcall Debug(
			[in]BSTR SmalltalkExpression);"

	<virtual stdcall: hresult 10 bstr>
	^self invalidCall!

definitions: smalltalkExpression
	"Invoke the Definitions() method of the COM object."

	^self Definitions: smalltalkExpression
!

Definitions: smalltalkExpression
	"Private - Invoke the Definitions() method of the COM object.

		HRESULT __stdcall Definitions(
			[in]BSTR SmalltalkExpression);"

	<virtual stdcall: hresult 14 bstr>
	^self invalidCall!

evaluate: smalltalkExpression
	"Answer the <BSTR> result of invoking the Evaluate() method of the COM object."

	| answer |
	answer := BSTR new.
	self
		Evaluate: smalltalkExpression
		result: answer.
	^answer asObject
!

Evaluate: smalltalkExpression result: result
	"Private - Invoke the Evaluate() method of the COM object.

		HRESULT __stdcall Evaluate(
			[in]BSTR SmalltalkExpression,
			[out, retval]BSTR* Result);"

	<virtual stdcall: hresult 9 bstr bstr*>
	^self invalidCall!

references: smalltalkExpression
	"Invoke the References() method of the COM object."

	^self References: smalltalkExpression
!

References: smalltalkExpression
	"Private - Invoke the References() method of the COM object.

		HRESULT __stdcall References(
			[in]BSTR SmalltalkExpression);"

	<virtual stdcall: hresult 15 bstr>
	^self invalidCall!

reformat: smalltalkExpression
	"Answer the <BSTR> result of invoking the Reformat() method of the COM object."

	| answer |
	answer := BSTR new.
	self
		Reformat: smalltalkExpression
		result: answer.
	^answer asObject
!

Reformat: smalltalkExpression result: result
	"Private - Invoke the Reformat() method of the COM object.

		HRESULT __stdcall Reformat(
			[in]BSTR SmalltalkExpression,
			[out, retval]BSTR* Result);"

	<virtual stdcall: hresult 11 bstr bstr*>
	^self invalidCall!

search: smalltalkExpression
	"Invoke the Search() method of the COM object."

	^self Search: smalltalkExpression
!

Search: smalltalkExpression
	"Private - Invoke the Search() method of the COM object.

		HRESULT __stdcall Search(
			[in]BSTR SmalltalkExpression);"

	<virtual stdcall: hresult 12 bstr>
	^self invalidCall!

searchMethodContaining: smalltalkExpression
	"Invoke the SearchMethodContaining() method of the COM object."

	^self SearchMethodContaining: smalltalkExpression
!

SearchMethodContaining: smalltalkExpression
	"Private - Invoke the SearchMethodContaining() method of the COM object.

		HRESULT __stdcall SearchMethodContaining(
			[in]BSTR SmalltalkExpression);"

	<virtual stdcall: hresult 13 bstr>
	^self invalidCall! !
!IDolphinEval categoriesFor: #browse:!**auto generated**!*-in class package!methods!public! !
!IDolphinEval categoriesFor: #Browse:!**auto generated**!*-in class package!COM Interfaces-IDolphinEval!private! !
!IDolphinEval categoriesFor: #debug:!**auto generated**!*-in class package!methods!public! !
!IDolphinEval categoriesFor: #Debug:!**auto generated**!*-in class package!COM Interfaces-IDolphinEval!private! !
!IDolphinEval categoriesFor: #definitions:!**auto generated**!*-in class package!methods!public! !
!IDolphinEval categoriesFor: #Definitions:!**auto generated**!*-in class package!COM Interfaces-IDolphinEval!private! !
!IDolphinEval categoriesFor: #evaluate:!**auto generated**!*-in class package!methods!public! !
!IDolphinEval categoriesFor: #Evaluate:result:!**auto generated**!*-in class package!COM Interfaces-IDolphinEval!private! !
!IDolphinEval categoriesFor: #references:!**auto generated**!*-in class package!methods!public! !
!IDolphinEval categoriesFor: #References:!**auto generated**!*-in class package!COM Interfaces-IDolphinEval!private! !
!IDolphinEval categoriesFor: #reformat:!**auto generated**!*-in class package!methods!public! !
!IDolphinEval categoriesFor: #Reformat:result:!**auto generated**!*-in class package!COM Interfaces-IDolphinEval!private! !
!IDolphinEval categoriesFor: #search:!**auto generated**!*-in class package!methods!public! !
!IDolphinEval categoriesFor: #Search:!**auto generated**!*-in class package!COM Interfaces-IDolphinEval!private! !
!IDolphinEval categoriesFor: #searchMethodContaining:!**auto generated**!*-in class package!methods!public! !
!IDolphinEval categoriesFor: #SearchMethodContaining:!**auto generated**!*-in class package!COM Interfaces-IDolphinEval!private! !

!IDolphinEval class methodsFor!

clsid
	"Private - Answer the CLSID of the coclass (DolphinEvaluator) for which the receiver is the default interface."

	^CLSID fromString: '{D2A20729-3B7A-47BB-BF58-D98D5B524F36}'
!

defineFunctions
	"Declare the virtual function table for the COM interface 'DolphinEval.IDolphinEval'
		IDolphinEval defineTemplate"

	self
		defineFunction: #Browse:
			argumentTypes: 'bstr';
		defineFunction: #Evaluate:result:
			argumentTypes: 'bstr bstr*';
		defineFunction: #Debug:
			argumentTypes: 'bstr';
		defineFunction: #Reformat:result:
			argumentTypes: 'bstr bstr*';
		defineFunction: #Search:
			argumentTypes: 'bstr';
		defineFunction: #SearchMethodContaining:
			argumentTypes: 'bstr';
		defineFunction: #Definitions:
			argumentTypes: 'bstr';
		defineFunction: #References:
			argumentTypes: 'bstr'
!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		IDolphinEval initializeTypeLib"

	typeLib := DolphinEvalLib! !
!IDolphinEval class categoriesFor: #clsid!**auto generated**!*-in class package!constants!private! !
!IDolphinEval class categoriesFor: #defineFunctions!**auto generated**!*-in class package!initializing!public! !
!IDolphinEval class categoriesFor: #initializeTypeLib!**auto generated**!*-in class package!initializing!private! !

"Binary Globals"!

DolphinEvalLib := Object fromBinaryStoreBytes: 
(ByteArray fromBase64String: 'IVNUQiAzIEYJFQACAAAAQVhUeXBlTGlicmFyeUFuYWx5emVyBgIJAElUeXBlTGliMgAAAAAAAAAA
AQgAAAYBCABUTElCQVRUUnIAAAAgAAAAS76vkJDuLUKOXgSr5CD89AAAAAABAAAAAQAAAAgAAABS
AAAAAAAAAFIAAAALAAAARG9scGhpbkV2YWyyAAAADgAAAERvbHBoaW5FdmFsTGliAAIAAOoAAADw
AAAAYgAAAAIAAABSAAAABAAAAEdVSUSyAAAABAAAAEdVSUQAAAAA')!

