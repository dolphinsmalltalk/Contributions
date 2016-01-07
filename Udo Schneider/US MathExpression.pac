| package |
package := Package name: 'US MathExpression'.
package paxVersion: 1;
	basicComment: '$id: US MathExpression 0.008$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This package uses a SmaCC based Parser/Scanner to use regular math expressions in Smalltalk. You can either get the result of any given string (with variables) or convert it to the equivalent Smalltalk Expression. This also works for some constants (e.g. ''pi'' -> ''Float pi'') and functions (''sin(x)'' -> ''x sin''). This is especially usefull when porting complex math expressions to Smalltalk.

"Simple equations"
'' 2 + 3 * sin( 0.3 )**pi'' evaluate.	"Inspect-It"

"Equations with variables"
vars := Dictionary new.
vars at: ''x'' put: 0.3.
'' 2 + 3 * sin( x )**pi'' evaluateWithVariables: vars.	"Inspect-It"

"Convert to Smalltalk Expression"
'' 2 + 3 * sin ( x )**pi'' asSmalltalkExpression.	"Inspect-It"
	
NOTE: This version uses "**" instead of "^" for #raisedTo: operations. "^" is not the bitwise XOR operator!!

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.008'.


package classNames
	add: #MathParser;
	add: #MathScanner;
	add: #MathToSmalltalkParser;
	add: #MathToSmalltalkScanner;
	yourself.

package methodNames
	add: #String -> #asSmalltalkExpression;
	add: #String -> #mathEvaluate;
	add: #String -> #mathEvaluateWithVariables:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Refactory\SmaCC Runtime';
	add: '..\..\Object Arts\Dolphin\System\Compiler\Smalltalk Parser';
	yourself).

package!

"Class Definitions"!

SmaCCParser subclass: #MathParser
	instanceVariableNames: 'variables'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmaCCParser subclass: #MathToSmalltalkParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmaCCScanner subclass: #MathScanner
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmaCCScanner subclass: #MathToSmalltalkScanner
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!String methodsFor!

asSmalltalkExpression
	"Return the content of the string in regular C math expression style as Smalltalk code"

	^(SmalltalkParser parseExpression: (MathToSmalltalkParser parse: self)) formattedCode!

mathEvaluate
	^self mathEvaluateWithVariables: Dictionary new!

mathEvaluateWithVariables: aDictionary 
	"Parses the expression given by self using 'natural' mathimatical order.
	You can supply a dictionary of variables which will be used during this
	proccess. E.g.:

	vars := Dictionary new.
	vars
		at: 'x' put: 2;
		at: 'y' put: 3.
	'x+y' giveMeTheResultWithVariables: vars"

	^MathParser parse: self withVariables: aDictionary! !
!String categoriesFor: #asSmalltalkExpression!public! !
!String categoriesFor: #mathEvaluate!public! !
!String categoriesFor: #mathEvaluateWithVariables:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

MathParser guid: (GUID fromString: '{2548F4CC-0FBB-4C3A-9CA5-95F80A3D4D8F}')!
MathParser comment: ''!
!MathParser categoriesForClass!Unclassified! !
!MathParser methodsFor!

reduceActionForConstant1: nodes 
	^Float pi!

reduceActionForConstant2: nodes 
	^Float e!

reduceActionForExpression1: nodes 
	^(nodes at: 1) + (nodes at: 3)!

reduceActionForExpression10: nodes 
	^(nodes at: 1) bitOr: (nodes at: 3)!

reduceActionForExpression11: nodes 
	^(nodes at: 2) negated!

reduceActionForExpression12: nodes 
	^(nodes at: 3) perform: (nodes at: 1)!

reduceActionForExpression13: nodes 
	^(nodes at: 3) perform: (nodes at: 1) with: (nodes at: 5)!

reduceActionForExpression14: nodes 
	^nodes at: 2!

reduceActionForExpression15: nodes 
	^nodes at: 1!

reduceActionForExpression16: nodes 
	^nodes at: 1!

reduceActionForExpression17: nodes 
	^nodes at: 1!

reduceActionForExpression2: nodes 
	^(nodes at: 1) - (nodes at: 3)!

reduceActionForExpression3: nodes 
	^(nodes at: 1) * (nodes at: 3)!

reduceActionForExpression4: nodes 
	^(nodes at: 1) / (nodes at: 3)!

reduceActionForExpression5: nodes 
	^(nodes at: 1) raisedTo: (nodes at: 3)!

reduceActionForExpression6: nodes 
	^(nodes at: 1) >> (nodes at: 3)!

reduceActionForExpression7: nodes 
	^(nodes at: 1) << (nodes at: 3)!

reduceActionForExpression8: nodes 
	^(nodes at: 1) bitAnd: (nodes at: 3)!

reduceActionForExpression9: nodes 
	^(nodes at: 1) bitXor: (nodes at: 3)!

reduceActionForfunction1: nodes 
	^#sin!

reduceActionForFunction1: nodes 
	^#abs!

reduceActionForFunction10: nodes 
	^#sqrt!

reduceActionForFunction11: nodes 
	^#tan!

reduceActionForFunction2: nodes 
	^#arcCos!

reduceActionForFunction3: nodes 
	^#arcSin!

reduceActionForFunction4: nodes 
	^#arcTan!

reduceActionForFunction5: nodes 
	^#cos!

reduceActionForFunction6: nodes 
	^#exp!

reduceActionForFunction7: nodes 
	^#ln!

reduceActionForFunction8: nodes 
	^#log!

reduceActionForFunction9: nodes 
	^#sin!

reduceActionForNumber1: nodes 
	^(nodes at: 1) value asNumber!

reduceActionForNumber2: nodes 
	^Integer readFrom: (((nodes at: 1) value readStream)
				next: 2;
				yourself)
		radix: 16!

reduceActionForOneArgFunction1: nodes 
	^#abs!

reduceActionForOneArgFunction10: nodes 
	^#sqrt!

reduceActionForOneArgFunction11: nodes 
	^#tan!

reduceActionForOneArgFunction2: nodes 
	^#arcCos!

reduceActionForOneArgFunction3: nodes 
	^#arcSin!

reduceActionForOneArgFunction4: nodes 
	^#arcTan!

reduceActionForOneArgFunction5: nodes 
	^#cos!

reduceActionForOneArgFunction6: nodes 
	^#exp!

reduceActionForOneArgFunction7: nodes 
	^#ln!

reduceActionForOneArgFunction8: nodes 
	^#log!

reduceActionForOneArgFunction9: nodes 
	^#sin!

reduceActionForTwoArgFunction1: nodes 
	^#max:!

reduceActionForTwoArgFunction2: nodes 
	^#min:!

reduceActionForVariable1: nodes 
	^self variableAt: (nodes at: 1) value!

reduceTable
	^#(
#(37 1 #reduceActionForOneArgFunction1:)
#(37 1 #reduceActionForOneArgFunction2:)
#(37 1 #reduceActionForOneArgFunction3:)
#(37 1 #reduceActionForOneArgFunction4:)
#(37 1 #reduceActionForOneArgFunction5:)
#(37 1 #reduceActionForOneArgFunction6:)
#(37 1 #reduceActionForOneArgFunction7:)
#(37 1 #reduceActionForOneArgFunction8:)
#(37 1 #reduceActionForOneArgFunction9:)
#(37 1 #reduceActionForOneArgFunction10:)
#(37 1 #reduceActionForOneArgFunction11:)
#(38 1 #reduceFor:)
#(39 1 #reduceActionForNumber1:)
#(39 1 #reduceActionForNumber2:)
#(40 1 #reduceActionForVariable1:)
#(41 3 #reduceActionForExpression1:)
#(41 3 #reduceActionForExpression2:)
#(41 3 #reduceActionForExpression3:)
#(41 3 #reduceActionForExpression4:)
#(41 3 #reduceActionForExpression5:)
#(41 3 #reduceActionForExpression6:)
#(41 3 #reduceActionForExpression7:)
#(41 3 #reduceActionForExpression8:)
#(41 3 #reduceActionForExpression9:)
#(41 3 #reduceActionForExpression10:)
#(41 2 #reduceActionForExpression11:)
#(41 4 #reduceActionForExpression12:)
#(41 6 #reduceActionForExpression13:)
#(41 3 #reduceActionForExpression14:)
#(41 1 #reduceActionForExpression15:)
#(41 1 #reduceActionForExpression16:)
#(41 1 #reduceActionForExpression17:)
#(43 1 #reduceActionForTwoArgFunction1:)
#(43 1 #reduceActionForTwoArgFunction2:)
#(44 1 #reduceActionForConstant1:)
#(44 1 #reduceActionForConstant2:)
)!

setVariables: aDictionary
variables := aDictionary!

transitionTable
	^#(
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 101 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 113 0 41 0 105 0 43 0 109 0 44]
#[0 0 142 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 117 0 41 0 105 0 43 0 109 0 44]
#[0 0 146 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[0 0 38 0 8]
#[0 0 42 0 8]
#[0 0 46 0 8]
#[0 0 138 0 8]
#[0 0 134 0 8]
#[0 0 34 0 8]
#[0 0 6 0 8]
#[0 0 14 0 8]
#[0 0 10 0 8]
#[0 0 26 0 8]
#[0 0 22 0 8]
#[0 0 18 0 8]
#[0 0 30 0 8]
#[0 0 54 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[0 0 58 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[0 0 62 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[0 0 121 0 8]
#[0 0 122 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[0 0 130 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 137 0 6 0 141 0 7 0 145 0 9 0 149 0 12 0 153 0 13 0 157 0 14 0 161 0 15 0 0 0 42]
#[0 0 165 0 8]
#[0 0 126 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[1 0 106 0 1 0 106 0 2 0 133 0 4 0 106 0 6 0 106 0 7 0 145 0 9 0 106 0 12 0 106 0 13 0 106 0 14 0 161 0 15 0 106 0 20 0 106 0 24 0 106 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 137 0 6 0 141 0 7 0 145 0 9 0 149 0 12 0 153 0 13 0 157 0 14 0 161 0 15 0 169 0 24]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 173 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 177 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 181 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 185 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 189 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 193 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 197 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 201 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 205 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 209 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 213 0 41 0 105 0 43 0 109 0 44]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 217 0 41 0 105 0 43 0 109 0 44]
#[0 0 118 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 137 0 6 0 141 0 7 0 145 0 9 0 149 0 12 0 153 0 13 0 157 0 14 0 161 0 15 0 221 0 24]
#[1 0 66 0 1 0 66 0 2 0 133 0 4 0 66 0 6 0 66 0 7 0 145 0 9 0 66 0 12 0 66 0 13 0 66 0 14 0 161 0 15 0 66 0 20 0 66 0 24 0 66 0 42]
#[1 0 70 0 1 0 70 0 2 0 133 0 4 0 70 0 6 0 70 0 7 0 145 0 9 0 70 0 12 0 70 0 13 0 70 0 14 0 161 0 15 0 70 0 20 0 70 0 24 0 70 0 42]
#[1 0 78 0 1 0 78 0 2 0 78 0 4 0 78 0 6 0 78 0 7 0 145 0 9 0 78 0 12 0 78 0 13 0 78 0 14 0 78 0 15 0 78 0 20 0 78 0 24 0 78 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 86 0 6 0 86 0 7 0 145 0 9 0 86 0 12 0 86 0 13 0 86 0 14 0 161 0 15 0 86 0 20 0 86 0 24 0 86 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 90 0 6 0 90 0 7 0 145 0 9 0 90 0 12 0 90 0 13 0 90 0 14 0 161 0 15 0 90 0 20 0 90 0 24 0 90 0 42]
#[1 0 82 0 1 0 82 0 2 0 82 0 4 0 82 0 6 0 82 0 7 0 145 0 9 0 82 0 12 0 82 0 13 0 82 0 14 0 82 0 15 0 82 0 20 0 82 0 24 0 82 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 137 0 6 0 141 0 7 0 145 0 9 0 94 0 12 0 94 0 13 0 94 0 14 0 161 0 15 0 94 0 20 0 94 0 24 0 94 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 137 0 6 0 141 0 7 0 145 0 9 0 98 0 12 0 98 0 13 0 98 0 14 0 161 0 15 0 98 0 20 0 98 0 24 0 98 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 137 0 6 0 141 0 7 0 145 0 9 0 102 0 12 0 102 0 13 0 102 0 14 0 161 0 15 0 102 0 20 0 102 0 24 0 102 0 42]
#[1 0 74 0 1 0 74 0 2 0 74 0 4 0 74 0 6 0 74 0 7 0 145 0 9 0 74 0 12 0 74 0 13 0 74 0 14 0 74 0 15 0 74 0 20 0 74 0 24 0 74 0 42]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 137 0 6 0 141 0 7 0 145 0 9 0 149 0 12 0 153 0 13 0 157 0 14 0 161 0 15 0 225 0 20]
#[0 0 110 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
#[1 0 9 0 2 0 13 0 3 0 17 0 8 0 21 0 16 0 25 0 17 0 29 0 18 0 33 0 19 0 37 0 21 0 41 0 22 0 45 0 23 0 49 0 25 0 53 0 26 0 57 0 27 0 61 0 28 0 65 0 29 0 69 0 30 0 73 0 31 0 77 0 32 0 81 0 33 0 85 0 34 0 89 0 37 0 93 0 39 0 97 0 40 0 229 0 41 0 105 0 43 0 109 0 44]
#[1 0 125 0 1 0 129 0 2 0 133 0 4 0 137 0 6 0 141 0 7 0 145 0 9 0 149 0 12 0 153 0 13 0 157 0 14 0 161 0 15 0 233 0 24]
#[0 0 114 0 1 0 2 0 4 0 6 0 7 0 9 0 12 0 13 0 14 0 15 0 20 0 24 0 42]
)!

variableAt: aString 
	^variables at: aString ifAbsent: [self error: 'Undefined variable ' , aString]! !
!MathParser categoriesFor: #reduceActionForConstant1:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForConstant2:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression1:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression10:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression11:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression12:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression13:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression14:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression15:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression16:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression17:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression2:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression3:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression4:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression5:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression6:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression7:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression8:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForExpression9:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForfunction1:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction1:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction10:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction11:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction2:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction3:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction4:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction5:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction6:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction7:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction8:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForFunction9:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForNumber1:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForNumber2:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction1:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction10:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction11:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction2:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction3:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction4:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction5:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction6:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction7:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction8:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForOneArgFunction9:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForTwoArgFunction1:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForTwoArgFunction2:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceActionForVariable1:!generated-reduction actions!public! !
!MathParser categoriesFor: #reduceTable!generated-tables!public! !
!MathParser categoriesFor: #setVariables:!private! !
!MathParser categoriesFor: #transitionTable!generated-tables!public! !
!MathParser categoriesFor: #variableAt:!public! !

!MathParser class methodsFor!

itemSetsComment

	"
1	[Constant : .  ""e"";""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sqrt"";""(""]
	[Expression : .  Expression ""^"" Expression;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arctan"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[B e g i n : .  Expression;E O F]
	[Function : .  ""arccos"";""(""]
	[Expression : .  ""-"" Expression;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" E O F ""/"" ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

2	[Constant : ""pi"" . ;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

3	[Function : ""arccos"" . ;""(""]

4	[Function : ""abs"" . ;""(""]

5	[Constant : .  ""e"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sqrt"";""(""]
	[Expression : .  Expression ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arctan"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : ""-"" .  Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""arccos"";""(""]
	[Expression : .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

6	[Constant : .  ""e"";""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""sqrt"";""(""]
	[Expression : .  Expression ""^"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""arctan"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : ""("" .  Expression "")"";""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""arccos"";""(""]
	[Expression : .  ""-"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

7	[Constant : ""e"" . ;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

8	[Function : ""sqrt"" . ;""(""]

9	[Function : ""arctan"" . ;""(""]

10	[Function : ""tan"" . ;""(""]

11	[Function : ""sin"" . ;""(""]

12	[Function : ""arcsin"" . ;""(""]

13	[Function : ""cos"" . ;""(""]

14	[Function : ""exp"" . ;""(""]

15	[Function : ""log"" . ;""(""]

16	[Function : ""ln"" . ;""(""]

17	[Number : <number> . ;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

18	[Expression : Expression .  ""-"" Expression;""^"" E O F ""/"" ""+"" ""-"" ""*""]
	[B e g i n : Expression . ;E O F]
	[Expression : Expression .  ""+"" Expression;""^"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" E O F ""/"" ""+"" ""-"" ""*""]

19	[Expression : Constant . ;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

20	[Expression : Number . ;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

21	[Expression : Function .  ""("" Expression "")"";""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

22	[Expression : Expression .  ""-"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : ""-"" Expression . ;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

23	[Expression : Expression .  ""-"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""+"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : ""("" Expression .  "")"";""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]

24	[Constant : .  ""e"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sqrt"";""(""]
	[Function : .  ""arctan"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arccos"";""(""]
	[Expression : Expression ""*"" .  Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

25	[Constant : .  ""e"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sqrt"";""(""]
	[Expression : .  Expression ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arctan"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arccos"";""(""]
	[Expression : .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression ""^"" .  Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

26	[Constant : .  ""e"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sqrt"";""(""]
	[Expression : .  Expression ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arctan"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression ""+"" .  Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arccos"";""(""]
	[Expression : .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

27	[Constant : .  ""e"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sqrt"";""(""]
	[Expression : .  Expression ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arctan"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arccos"";""(""]
	[Expression : .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression ""-"" .  Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

28	[Constant : .  ""e"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression ""/"" .  Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arctan"";""(""]
	[Function : .  ""sqrt"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""arccos"";""(""]
	[Expression : .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

29	[Constant : .  ""e"";""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""sqrt"";""(""]
	[Expression : .  Expression ""^"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""arctan"";""(""]
	[Expression : .  Expression ""*"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""ln"";""(""]
	[Function : .  ""log"";""(""]
	[Expression : .  Number;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : .  Function ""("" Expression "")"";""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""abs"";""(""]
	[Expression : .  Expression ""-"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : .  ""("" Expression "")"";""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Constant : .  ""pi"";""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""arccos"";""(""]
	[Expression : .  ""-"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : Function ""("" .  Expression "")"";""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : .  Expression ""/"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : .  Expression ""+"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Number : .  <number>;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""sin"";""(""]
	[Function : .  ""arcsin"";""(""]
	[Function : .  ""cos"";""(""]
	[Function : .  ""exp"";""(""]
	[Expression : .  Constant;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Function : .  ""tan"";""(""]

30	[Expression : ""("" Expression "")"" . ;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

31	[Expression : Expression .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression ""*"" Expression . ;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]

32	[Expression : Expression .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression ""^"" Expression . ;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]

33	[Expression : Expression .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression ""+"" Expression . ;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""+"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]

34	[Expression : Expression ""-"" Expression . ;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]

35	[Expression : Expression .  ""-"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression ""/"" Expression . ;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" "")"" ""/"" E O F ""+"" ""-"" ""*""]
	[Expression : Expression .  ""+"" Expression;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

36	[Expression : Expression .  ""-"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""+"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""^"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""*"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]
	[Expression : Function ""("" Expression .  "")"";""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]
	[Expression : Expression .  ""/"" Expression;""^"" "")"" ""/"" ""+"" ""-"" ""*""]

37	[Expression : Function ""("" Expression "")"" . ;""^"" "")"" E O F ""/"" ""+"" ""-"" ""*""]

"!

parse: aString withVariables: aDictionary

	| parser |
	parser := self on: (ReadStream on: aString).
	parser setStartingState: (self defaultStartingState);
setVariables: aDictionary.
	^parser parse
!

parserDefinitionComment

	"%left ""&&"" ""||"";
%left ""&"" ""^"" ""|"";
%left "">>"" ""<<"";
%left ""+"" ""-"";
%left ""*"" ""/"" ""%"";
%right ""**"";

Expression : 
	  Expression 'exp1' ""+"" Expression 'exp2' {exp1 + exp2}
	| Expression 'exp1' ""-"" Expression 'exp2' {exp1 - exp2}
	| Expression 'exp1' ""*"" Expression 'exp2' {exp1 * exp2}
	| Expression 'exp1' ""/"" Expression 'exp2' {exp1 / exp2}
	| Expression 'exp1' ""**"" Expression 'exp2' {exp1 raisedTo: exp2}
	| Expression 'exp1' "">>"" Expression 'exp2' { exp1 >> exp2}
	| Expression 'exp1' ""<<"" Expression 'exp2' { exp1 << exp2}
	| Expression 'exp1' ""&"" Expression 'exp2' { exp1 bitAnd: exp2}
	| Expression 'exp1' ""^"" Expression 'exp2' { exp1 bitXor: exp2}
	| Expression 'exp1' ""|"" Expression 'exp2' { exp1 bitOr: exp2}
	| ""-"" Expression 'exp' { exp negated}
	| OneArgFunction 'function' ""("" Expression 'exp' "")"" { exp perform: function}
	| TwoArgFunction 'function' ""("" Expression 'exp1' "","" Expression 'exp2' "")"" { exp1 perform: function with: exp2 }
	| ""("" Expression 'exp' "")"" {exp}
	| Number 'number' {number}
	| Constant 'constant' {constant}
	| Variable 'variable' {variable} ;

OneArgFunction :
	  ""abs"" { #abs }
	| ""arccos"" { #arcCos }
	| ""arcsin"" { #arcSin }
	| ""arctan"" { #arcTan }
	| ""cos"" { #cos }
	| ""exp"" { #exp }
	| ""ln"" { #ln }
	| ""log"" { #log }
	| ""sin"" { #sin }
	| ""sqrt"" { #sqrt }
	| ""tan"" { #tan } ;

TwoArgFunction:
	  ""max"" { #max: }
	| ""min"" { #min: } ;


Constant	:
	  ""pi"" { Float pi}
	| ""e"" { Float e} ;

Variable	: <variable> 'variableName' { self variableAt: variableName value } ;

Number : <number> 'numberToken' {numberToken value asNumber}
	| <hexNumber> 'hexNumberToken' { Integer readFrom: ( hexNumberToken value readStream next: 2 ; yourself) radix: 16} ;"!

scannerClass
	^MathScanner!

startingStateForExpression
	^1!

symbolComment

	"
1. ""pi""
2. "")""
3. ""*""
4. ""^""
5. ""arccos""
6. ""abs""
7. ""+""
8. ""-""
9. ""(""
10. ""/""
11. ""e""
12. ""sqrt""
13. ""arctan""
14. ""tan""
15. ""sin""
16. ""arcsin""
17. ""cos""
18. ""exp""
19. ""log""
20. ""ln""
21. <number>
22. <whitespace>
23. error
24. Expression
25. B e g i n
26. Constant
27. E O F
28. Number
29. Function
"! !
!MathParser class categoriesFor: #itemSetsComment!generated-comments!public! !
!MathParser class categoriesFor: #parse:withVariables:!public! !
!MathParser class categoriesFor: #parserDefinitionComment!generated-comments!public! !
!MathParser class categoriesFor: #scannerClass!generated-accessing!public! !
!MathParser class categoriesFor: #startingStateForExpression!generated-starting states!public! !
!MathParser class categoriesFor: #symbolComment!generated-comments!public! !

MathToSmalltalkParser guid: (GUID fromString: '{E85F0912-FF5D-45A4-87D9-C15A3063583D}')!
MathToSmalltalkParser comment: ''!
!MathToSmalltalkParser categoriesForClass!Unclassified! !
!MathToSmalltalkParser methodsFor!

reduceActionForConstant1: nodes 
	^'Float pi'!

reduceActionForConstant2: nodes 
	^'Float e'!

reduceActionForExpression1: nodes 
	^'(' , (nodes at: 1) , '+' , (nodes at: 3) , ')'!

reduceActionForExpression10: nodes 
	^nodes at: 1!

reduceActionForExpression11: nodes 
	^nodes at: 1!

reduceActionForExpression12: nodes 
	^nodes at: 1!

reduceActionForExpression2: nodes 
	^'(' , (nodes at: 1) , '-' , (nodes at: 3) , ')'!

reduceActionForExpression3: nodes 
	^'(' , (nodes at: 1) , '*' , (nodes at: 3) , ')'!

reduceActionForExpression4: nodes 
	^'(' , (nodes at: 1) , '/' , (nodes at: 3) , ')'!

reduceActionForExpression5: nodes 
	^'(' , (nodes at: 1) , ' raisedTo:' , (nodes at: 3) , ')'!

reduceActionForExpression6: nodes 
	^'(' , (nodes at: 2) , ' negated)'!

reduceActionForExpression7: nodes 
	^'(' , (nodes at: 3) , ' ' , (nodes at: 1) , ')'!

reduceActionForExpression8: nodes 
	^(nodes at: 3) , ' ' , (nodes at: 1) , ' ' , (nodes at: 5)!

reduceActionForExpression9: nodes 
	^'(' , (nodes at: 2) , ')'!

reduceActionForNumber1: nodes 
	^(nodes at: 1) value!

reduceActionForOneArgFunction1: nodes 
	^#abs!

reduceActionForOneArgFunction10: nodes 
	^#sqrt!

reduceActionForOneArgFunction11: nodes 
	^#tan!

reduceActionForOneArgFunction2: nodes 
	^#arcCos!

reduceActionForOneArgFunction3: nodes 
	^#arcSin!

reduceActionForOneArgFunction4: nodes 
	^#arcTan!

reduceActionForOneArgFunction5: nodes 
	^#cos!

reduceActionForOneArgFunction6: nodes 
	^#exp!

reduceActionForOneArgFunction7: nodes 
	^#ln!

reduceActionForOneArgFunction8: nodes 
	^#log!

reduceActionForOneArgFunction9: nodes 
	^#sin!

reduceActionForTwoArgFunction1: nodes 
	^#max:!

reduceActionForTwoArgFunction2: nodes 
	^#min:!

reduceActionForVariable1: nodes 
	^(nodes at: 1) value!

reduceTable
	^#(
#(28 1 #reduceActionForTwoArgFunction1:)
#(28 1 #reduceActionForTwoArgFunction2:)
#(29 1 #reduceActionForVariable1:)
#(30 1 #reduceFor:)
#(31 1 #reduceActionForConstant1:)
#(31 1 #reduceActionForConstant2:)
#(32 1 #reduceActionForOneArgFunction1:)
#(32 1 #reduceActionForOneArgFunction2:)
#(32 1 #reduceActionForOneArgFunction3:)
#(32 1 #reduceActionForOneArgFunction4:)
#(32 1 #reduceActionForOneArgFunction5:)
#(32 1 #reduceActionForOneArgFunction6:)
#(32 1 #reduceActionForOneArgFunction7:)
#(32 1 #reduceActionForOneArgFunction8:)
#(32 1 #reduceActionForOneArgFunction9:)
#(32 1 #reduceActionForOneArgFunction10:)
#(32 1 #reduceActionForOneArgFunction11:)
#(34 3 #reduceActionForExpression1:)
#(34 3 #reduceActionForExpression2:)
#(34 3 #reduceActionForExpression3:)
#(34 3 #reduceActionForExpression4:)
#(34 3 #reduceActionForExpression5:)
#(34 2 #reduceActionForExpression6:)
#(34 4 #reduceActionForExpression7:)
#(34 6 #reduceActionForExpression8:)
#(34 3 #reduceActionForExpression9:)
#(34 1 #reduceActionForExpression10:)
#(34 1 #reduceActionForExpression11:)
#(34 1 #reduceActionForExpression12:)
#(35 1 #reduceActionForNumber1:)
)!

transitionTable
	^#(
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 101 0 34 0 105 0 35]
#[0 0 22 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[0 0 34 0 11]
#[0 0 30 0 11]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 109 0 34 0 105 0 35]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 113 0 34 0 105 0 35]
#[0 0 26 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[0 0 66 0 11]
#[0 0 70 0 11]
#[0 0 6 0 11]
#[0 0 58 0 11]
#[0 0 38 0 11]
#[0 0 42 0 11]
#[0 0 10 0 11]
#[0 0 46 0 11]
#[0 0 50 0 11]
#[0 0 54 0 11]
#[0 0 62 0 11]
#[0 0 122 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[0 0 14 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[0 0 117 0 11]
#[0 0 118 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[0 0 114 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[0 0 121 0 11]
#[1 0 125 0 4 0 129 0 5 0 133 0 8 0 137 0 9 0 141 0 10 0 0 0 33]
#[0 0 110 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[1 0 94 0 2 0 94 0 3 0 125 0 4 0 129 0 5 0 94 0 8 0 94 0 9 0 141 0 10 0 94 0 33]
#[1 0 145 0 2 0 125 0 4 0 129 0 5 0 133 0 8 0 137 0 9 0 141 0 10]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 149 0 34 0 105 0 35]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 153 0 34 0 105 0 35]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 157 0 34 0 105 0 35]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 161 0 34 0 105 0 35]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 165 0 34 0 105 0 35]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 169 0 34 0 105 0 35]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 173 0 34 0 105 0 35]
#[0 0 106 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[1 0 177 0 3 0 125 0 4 0 129 0 5 0 133 0 8 0 137 0 9 0 141 0 10]
#[1 0 181 0 2 0 125 0 4 0 129 0 5 0 133 0 8 0 137 0 9 0 141 0 10]
#[1 0 86 0 2 0 86 0 3 0 86 0 4 0 129 0 5 0 86 0 8 0 86 0 9 0 86 0 10 0 86 0 33]
#[1 0 90 0 2 0 90 0 3 0 90 0 4 0 129 0 5 0 90 0 8 0 90 0 9 0 90 0 10 0 90 0 33]
#[1 0 74 0 2 0 74 0 3 0 125 0 4 0 129 0 5 0 74 0 8 0 74 0 9 0 141 0 10 0 74 0 33]
#[1 0 78 0 2 0 78 0 3 0 125 0 4 0 129 0 5 0 78 0 8 0 78 0 9 0 141 0 10 0 78 0 33]
#[1 0 82 0 2 0 82 0 3 0 82 0 4 0 129 0 5 0 82 0 8 0 82 0 9 0 82 0 10 0 82 0 33]
#[1 0 9 0 1 0 13 0 6 0 17 0 7 0 21 0 9 0 25 0 11 0 29 0 12 0 33 0 13 0 37 0 14 0 41 0 15 0 45 0 16 0 49 0 17 0 53 0 18 0 57 0 19 0 61 0 20 0 65 0 21 0 69 0 22 0 73 0 23 0 77 0 24 0 81 0 25 0 85 0 28 0 89 0 29 0 93 0 31 0 97 0 32 0 185 0 34 0 105 0 35]
#[0 0 98 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
#[1 0 189 0 2 0 125 0 4 0 129 0 5 0 133 0 8 0 137 0 9 0 141 0 10]
#[0 0 102 0 2 0 3 0 4 0 5 0 8 0 9 0 10 0 33]
)! !
!MathToSmalltalkParser categoriesFor: #reduceActionForConstant1:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForConstant2:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression1:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression10:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression11:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression12:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression2:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression3:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression4:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression5:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression6:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression7:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression8:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForExpression9:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForNumber1:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction1:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction10:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction11:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction2:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction3:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction4:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction5:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction6:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction7:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction8:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForOneArgFunction9:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForTwoArgFunction1:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForTwoArgFunction2:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceActionForVariable1:!generated-reduction actions!public! !
!MathToSmalltalkParser categoriesFor: #reduceTable!generated-tables!public! !
!MathToSmalltalkParser categoriesFor: #transitionTable!generated-tables!public! !

!MathToSmalltalkParser class methodsFor!

parserDefinitionComment

	"%left ""+"" ""-"";
%left ""*"" ""/"";
%right ""^"";

Expression : 
	  Expression 'exp1' ""+"" Expression 'exp2' {'(' , exp1 , '+' , exp2 ,  ')'}
	| Expression 'exp1' ""-"" Expression 'exp2' {'(' , exp1 ,'-' , exp2,  ')'}
	| Expression 'exp1' ""*"" Expression 'exp2' {'(' , exp1 , '*' ,  exp2 , ')'}
	| Expression 'exp1' ""/"" Expression 'exp2' {'(' , exp1 , '/' ,  exp2 , ')'}
	| Expression 'exp1' ""^"" Expression 'exp2' {'(' , exp1 , ' raisedTo:',  exp2 , ')'}
	| ""-"" Expression 'exp' { '(' , exp , ' negated)'}
	| OneArgFunction 'function' ""("" Expression 'exp' "")"" { '(' , exp , ' ' , function , ')'}
	| TwoArgFunction 'function' ""("" Expression 'exp1' "","" Expression 'exp2' "")"" { exp1 , ' ' , function , ' ' ,  exp2 }
	| ""("" Expression 'exp' "")"" { '(' , exp , ')'}
	| Number 'number' {number}
	| Constant 'constant' {constant}
	| Variable 'variable' {variable} ;

OneArgFunction :
	  ""abs"" { #abs }
	| ""arccos"" { #arcCos }
	| ""arcsin"" { #arcSin }
	| ""arctan"" { #arcTan }
	| ""cos"" { #cos }
	| ""exp"" { #exp }
	| ""ln"" { #ln }
	| ""log"" { #log }
	| ""sin"" { #sin }
	| ""sqrt"" { #sqrt }
	| ""tan"" { #tan } ;

TwoArgFunction:
	  ""max"" { #max: }
	| ""min"" { #min: } ;


Constant	:
	  ""pi"" { 'Float pi'}
	| ""e"" { 'Float e'} ;

Variable	: <variable> 'variableName' { variableName value } ;

Number : <number> 'numberToken' {numberToken value };"!

scannerClass
	^MathToSmalltalkScanner!

startingStateForExpression
	^1! !
!MathToSmalltalkParser class categoriesFor: #parserDefinitionComment!generated-comments!public! !
!MathToSmalltalkParser class categoriesFor: #scannerClass!generated-accessing!public! !
!MathToSmalltalkParser class categoriesFor: #startingStateForExpression!generated-starting states!public! !

MathScanner guid: (GUID fromString: '{B845CE7E-B00D-4864-A3C5-704861D0080C}')!
MathScanner comment: ''!
!MathScanner categoriesForClass!Unclassified! !
!MathScanner methodsFor!

emptySymbolTokenId
	^42!

errorTokenId
	^36!

scan1
	
	[self recordMatch: #(32 34).
	self step.
	((currentCharacter between: $A and: $Z) 
		or: [currentCharacter == $_ or: [currentCharacter between: $a and: $z]]) 
			ifTrue: 
				[
				[self recordMatch: #(34).
				self step.
				currentCharacter isHexDigit or: 
						[(currentCharacter between: $G and: $Z) 
							or: [currentCharacter == $_ or: [currentCharacter between: $a and: $z]]]] 
						whileTrue.
				^self reportLastMatch].
	currentCharacter isDigit] 
			whileTrue.
	currentCharacter == $. 
		ifTrue: 
			[
			[self recordMatch: #(32).
			self step.
			currentCharacter isDigit] whileTrue.
			^self reportLastMatch].
	^self reportLastMatch!

scan2
	
	[self recordMatch: #(32).
	self step.
	currentCharacter isDigit] whileTrue.
	currentCharacter == $. 
		ifTrue: 
			[
			[self recordMatch: #(32).
			self step.
			currentCharacter isDigit] whileTrue.
			^self reportLastMatch].
	^self reportLastMatch!

scanForToken
	self step.
	((currentCharacter between: $A and: $Z) or: 
			[currentCharacter == $_ 
				or: [(currentCharacter between: $a and: $d) or: [currentCharacter between: $f and: $z]]]) 
		ifTrue: 
			[
			[self recordMatch: #(34).
			self step.
			currentCharacter isHexDigit or: 
					[(currentCharacter between: $G and: $Z) 
						or: [currentCharacter == $_ or: [currentCharacter between: $a and: $z]]]] 
					whileTrue.
			^self reportLastMatch].
	(currentCharacter between: $1 and: $9) ifTrue: [^self scan1].
	currentCharacter isWhitespace 
		ifTrue: 
			[
			[self recordMatch: #whitespace.
			self step.
			currentCharacter isWhitespace] whileTrue.
			^self reportLastMatch].
	currentCharacter == $- ifTrue: [^self recordAndReportMatch: #(2)].
	currentCharacter == $% ifTrue: [^self recordAndReportMatch: #(5)].
	currentCharacter == $& 
		ifTrue: 
			[self recordMatch: #(12).
			self step.
			currentCharacter == $& ifTrue: [^self recordAndReportMatch: #(10)].
			^self reportLastMatch].
	currentCharacter == $( ifTrue: [^self recordAndReportMatch: #(8)].
	currentCharacter == $) ifTrue: [^self recordAndReportMatch: #(24)].
	currentCharacter == $* 
		ifTrue: 
			[self recordMatch: #(15).
			self step.
			currentCharacter == $* ifTrue: [^self recordAndReportMatch: #(9)].
			^self reportLastMatch].
	currentCharacter == $, ifTrue: [^self recordAndReportMatch: #(20)].
	currentCharacter == $/ ifTrue: [^self recordAndReportMatch: #(4)].
	currentCharacter == $^ ifTrue: [^self recordAndReportMatch: #(13)].
	currentCharacter == $| 
		ifTrue: 
			[self recordMatch: #(14).
			self step.
			currentCharacter == $| ifTrue: [^self recordAndReportMatch: #(11)].
			^self reportLastMatch].
	currentCharacter == $+ ifTrue: [^self recordAndReportMatch: #(1)].
	currentCharacter == $< 
		ifTrue: 
			[self step.
			currentCharacter == $< ifTrue: [^self recordAndReportMatch: #(7)].
			^self reportLastMatch].
	currentCharacter == $> 
		ifTrue: 
			[self step.
			currentCharacter == $> ifTrue: [^self recordAndReportMatch: #(6)].
			^self reportLastMatch].
	currentCharacter == $0 
		ifTrue: 
			[self recordMatch: #(32 34).
			self step.
			((currentCharacter between: $A and: $Z) or: 
					[currentCharacter == $_ 
						or: [(currentCharacter between: $a and: $w) or: [currentCharacter between: $y and: $z]]]) 
				ifTrue: 
					[
					[self recordMatch: #(34).
					self step.
					currentCharacter isHexDigit or: 
							[(currentCharacter between: $G and: $Z) 
								or: [currentCharacter == $_ or: [currentCharacter between: $a and: $z]]]] 
							whileTrue.
					^self reportLastMatch].
			currentCharacter isDigit ifTrue: [^self scan1].
			currentCharacter == $. 
				ifTrue: 
					[
					[self recordMatch: #(32).
					self step.
					currentCharacter isDigit] whileTrue.
					^self reportLastMatch].
			currentCharacter == $x 
				ifTrue: 
					[
					[self recordMatch: #(33 34).
					self step.
					((currentCharacter between: $G and: $Z) 
						or: [currentCharacter == $_ or: [currentCharacter between: $g and: $z]]) 
							ifTrue: 
								[
								[self recordMatch: #(34).
								self step.
								currentCharacter isHexDigit or: 
										[(currentCharacter between: $G and: $Z) 
											or: [currentCharacter == $_ or: [currentCharacter between: $a and: $z]]]] 
										whileTrue.
								^self reportLastMatch].
					currentCharacter isHexDigit or: [currentCharacter between: $a and: $f]] 
							whileTrue.
					^self reportLastMatch].
			^self reportLastMatch].
	currentCharacter == $e 
		ifTrue: 
			[self recordMatch: #(16 34).
			self step.
			(currentCharacter isHexDigit or: 
					[(currentCharacter between: $G and: $Z) 
						or: [currentCharacter == $_ or: [currentCharacter between: $a and: $z]]]) 
				ifTrue: 
					[
					[self recordMatch: #(34).
					self step.
					currentCharacter isHexDigit or: 
							[(currentCharacter between: $G and: $Z) 
								or: [currentCharacter == $_ or: [currentCharacter between: $a and: $z]]]] 
							whileTrue.
					^self reportLastMatch].
			^self reportLastMatch].
	^self reportLastMatch! !
!MathScanner categoriesFor: #emptySymbolTokenId!generated-tokens!public! !
!MathScanner categoriesFor: #errorTokenId!generated-tokens!public! !
!MathScanner categoriesFor: #scan1!generated-scanner!public! !
!MathScanner categoriesFor: #scan2!generated-scanner!public! !
!MathScanner categoriesFor: #scanForToken!generated-scanner!public! !

!MathScanner class methodsFor!

initializeKeywordMap
	keywordMap := Dictionary new.
	#(#(34 'abs' 25) #(34 'arccos' 27) #(34 'arcsin' 26) #(34 'arctan' 30) #(34 'cos' 29) #(34 'exp' 28) #(34 'ln' 31) #(34 'log' 23) #(34 'max' 22) #(34 'min' 21) #(34 'pi' 3) #(34 'sin' 17) #(34 'sqrt' 18) #(34 'tan' 19)) 
		do: [:each | (keywordMap at: each first ifAbsentPut: [Dictionary new]) at: (each at: 2) put: each last].
	^keywordMap!

scannerDefinitionComment

	"<number>	:	[\d]+ (\. [\d]*)? ;
<hexNumber>	:	0x[\da-fA-F]+? ;
<variable>	:	[\w]+ ;
<whitespace>	:	\s+;"! !
!MathScanner class categoriesFor: #initializeKeywordMap!generated-initialization!public! !
!MathScanner class categoriesFor: #scannerDefinitionComment!generated-comments!public! !

MathToSmalltalkScanner guid: (GUID fromString: '{466B1A68-1812-4DBA-9BAD-4B19D0E7F94C}')!
MathToSmalltalkScanner comment: ''!
!MathToSmalltalkScanner categoriesForClass!Unclassified! !
!MathToSmalltalkScanner methodsFor!

emptySymbolTokenId
	^33!

errorTokenId
	^27!

scan1
	
	[self recordMatch: #(25).
	self step.
	currentCharacter between: $a and: $z] whileTrue.
	currentCharacter isDigit 
		ifTrue: 
			[
			[self recordMatch: #(25).
			self step.
			currentCharacter isDigit] whileTrue.
			^self reportLastMatch].
	^self reportLastMatch!

scanForToken
	self step.
	((currentCharacter between: $a and: $d) or: [currentCharacter between: $f and: $z]) 
		ifTrue: [^self scan1].
	currentCharacter isDigit 
		ifTrue: 
			[
			[self recordMatch: #(24).
			self step.
			currentCharacter isDigit] whileTrue.
			currentCharacter == $. 
				ifTrue: 
					[
					[self recordMatch: #(24).
					self step.
					currentCharacter isDigit] whileTrue.
					^self reportLastMatch].
			^self reportLastMatch].
	currentCharacter isWhitespace 
		ifTrue: 
			[
			[self recordMatch: #whitespace.
			self step.
			currentCharacter isWhitespace] whileTrue.
			^self reportLastMatch].
	currentCharacter == $- ifTrue: [^self recordAndReportMatch: #(9)].
	currentCharacter == $( ifTrue: [^self recordAndReportMatch: #(11)].
	currentCharacter == $) ifTrue: [^self recordAndReportMatch: #(2)].
	currentCharacter == $* ifTrue: [^self recordAndReportMatch: #(10)].
	currentCharacter == $, ifTrue: [^self recordAndReportMatch: #(3)].
	currentCharacter == $/ ifTrue: [^self recordAndReportMatch: #(4)].
	currentCharacter == $^ ifTrue: [^self recordAndReportMatch: #(5)].
	currentCharacter == $+ ifTrue: [^self recordAndReportMatch: #(8)].
	currentCharacter == $e 
		ifTrue: 
			[self recordMatch: #(12 25).
			self step.
			(currentCharacter between: $a and: $z) ifTrue: [^self scan1].
			currentCharacter isDigit 
				ifTrue: 
					[
					[self recordMatch: #(25).
					self step.
					currentCharacter isDigit] whileTrue.
					^self reportLastMatch].
			^self reportLastMatch].
	^self reportLastMatch! !
!MathToSmalltalkScanner categoriesFor: #emptySymbolTokenId!generated-tokens!public! !
!MathToSmalltalkScanner categoriesFor: #errorTokenId!generated-tokens!public! !
!MathToSmalltalkScanner categoriesFor: #scan1!generated-scanner!public! !
!MathToSmalltalkScanner categoriesFor: #scanForToken!generated-scanner!public! !

!MathToSmalltalkScanner class methodsFor!

initializeKeywordMap
	keywordMap := Dictionary new.
	#(#(25 'abs' 7) #(25 'arccos' 6) #(25 'arcsin' 17) #(25 'arctan' 18) #(25 'cos' 20) #(25 'exp' 21) #(25 'ln' 22) #(25 'log' 16) #(25 'max' 15) #(25 'min' 19) #(25 'pi' 1) #(25 'sin' 23) #(25 'sqrt' 13) #(25 'tan' 14)) 
		do: [:each | (keywordMap at: each first ifAbsentPut: [Dictionary new]) at: (each at: 2) put: each last].
	^keywordMap!

scannerDefinitionComment

	"<number>	:	[0-9]+ (\. [0-9]*)? ;
<variable>	:	[a-z]+[0-9]* ;
<whitespace>	:	\s+;"! !
!MathToSmalltalkScanner class categoriesFor: #initializeKeywordMap!generated-initialization!public! !
!MathToSmalltalkScanner class categoriesFor: #scannerDefinitionComment!generated-comments!public! !

"Binary Globals"!

