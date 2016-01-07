| package |
package := Package name: 'US Source HTML Formatter'.
package paxVersion: 1;
	basicComment: '$id: US Source HTML Formatter 0.010$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This package formats method and smalltalk expression as HTML.

Usage for methods:
RBHtmlFormatter new format: (SmalltalkParser parseMethod: methodString).

Or use CompiledCode>>htmlSource. E.g.:
(Object methodFor: #displayString) htmlSource.


Usage for expressions
RBHtmlFormatter new format: (SmalltalkParser parseExpression: expressionString).

Please note that you''ll need an CSS Stylesheet for correct display. An example is here:
RBHtmlFormatter class>>cssStyleSheet


Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


'.

package basicPackageVersion: '0.010'.


package classNames
	add: #RBHtmlFormatter;
	add: #RBLaTexFormatter;
	yourself.

package methodNames
	add: #CompiledCode -> #htmlSource;
	add: #CompiledCode -> #latexSource;
	add: #CompiledMethod -> #htmlSource;
	add: #CompiledMethod -> #latexSource;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Refactory\Refactoring Browser\Formatters\RBFormatters';
	add: '..\..\Object Arts\Dolphin\System\Compiler\Smalltalk Parser';
	yourself).

package!

"Class Definitions"!

RBFormatter subclass: #RBHtmlFormatter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RBFormatter subclass: #RBLaTexFormatter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!CompiledCode methodsFor!

htmlSource
	^self subclassResponsibility !

latexSource
	^self subclassResponsibility! !
!CompiledCode categoriesFor: #htmlSource!accessing!public! !
!CompiledCode categoriesFor: #latexSource!accessing!public! !

!CompiledMethod methodsFor!

htmlSource
	^RBHtmlFormatter new format: (SmalltalkParser parseMethod: self getSource ).!

latexSource
	^RBLaTexFormatter new format: (SmalltalkParser parseMethod: self getSource)! !
!CompiledMethod categoriesFor: #htmlSource!accessing!public! !
!CompiledMethod categoriesFor: #latexSource!accessing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

RBHtmlFormatter guid: (GUID fromString: '{C439FD6C-9E8C-4235-B3A9-BFAA61C0E380}')!
RBHtmlFormatter comment: ''!
!RBHtmlFormatter categoriesForClass!Unclassified! !
!RBHtmlFormatter methodsFor!

acceptLiteralNode: aLiteralNode 
	| literalStream literal |
	literalStream := ReadWriteStream on: String new.
	aLiteralNode token storeOn: literalStream.
	literal := literalStream contents.

	codeStream nextPutAll: (self span: 'literal' around: literal)!

acceptVariableNode: aVariableNode 
	codeStream nextPutAll: (self span: 'variable' around: aVariableNode name)!

cr
	codeStream nextPutAll: '</br>';cr.!

formatMessageSelector: selectorParts withArguments: formattedArgs multiline: multiLine 
	formattedArgs isEmpty 
		ifTrue: [codeStream nextPutAll: (self span: 'message' around: selectorParts first value)]
		ifFalse: 
			[1 to: formattedArgs size
				do: 
					[:i | 
					i ~~ 1 & multiLine not ifTrue: [self space].
					codeStream
						nextPutAll: (self span: 'message' around: (selectorParts at: i) value);
						nextPut: $ ;
						nextPutAll: ( (formattedArgs at: i)).
					(multiLine and: [i < formattedArgs size]) ifTrue: [self newLine]]]!

formatMethodCommentFor: aNode indentBefore: aBoolean 
	| source comments |
	source := aNode source.
	source isNil ifTrue: [^self].
	comments := aNode methodComments.
	comments isEmpty ifTrue: [^self].
	comments do: 
			[:each | 
			aBoolean ifTrue: [self newLine].
			codeStream 
				nextPutAll: (self span: 'comment' around: (aNode source copyFrom: each first to: each last))]
		separatedBy: 
			[self cr.
			aBoolean ifFalse: [self newLine]].
	aBoolean ifTrue: [self cr] ifFalse: [self newLine]!

formatMethodPatternFor: aMethodNode 
	| selectorParts arguments |
	selectorParts := aMethodNode selectorParts.
	arguments := aMethodNode arguments.
	arguments isEmpty 
		ifTrue: [codeStream nextPutAll: (self span: 'selector' around: selectorParts first value)]
		ifFalse: 
			[selectorParts with: arguments
				do: 
					[:selector :arg | 
					codeStream
						nextPutAll: (self span: 'selector' around: selector value);
						nextPut: $ .
					
					self visitArgument: arg.
					self space]]!

formatStatementCommentFor: aNode 
	| source |
	source := aNode source.
	source isNil ifTrue: [^self].
	aNode statementComments do: 
			[:each | 
			| crs |
			crs := self newLinesFor: source startingAt: each first.
			(crs - 1 max: 0) timesRepeat: [self newLine].
			crs == 0 ifTrue: [self tab] ifFalse: [self newLine].
			codeStream nextPutAll: (self span: 'comment' around: (source copyFrom: each first to: each last))]!

formatTemporariesFor: aSequenceNode 
	| temps |
	temps := aSequenceNode temporaries.
	temps isEmpty ifTrue: [^self].
	codeStream nextPutAll: '| '.
	temps do: 
			[:each | 
self span: 'temp' while: [
			self visitArgument: each.].
			self space].
	codeStream nextPut: $|.
	self newLine!

newLine
	firstLineLength isNil ifTrue: [firstLineLength := codeStream position].
	self cr.
	indent timesRepeat: [self tab].
	lineStart := codeStream position!

span: aCssClass around: aString
^'<span class="' , aCssClass , '">' , aString , '</span>'!

span: aCssClass while: aBlock
codeStream nextPutAll: '<span class="' , aCssClass , '">' .
aBlock value.
codeStream nextPutAll:  '</span>'!

tab
	4 timesRepeat: [codeStream nextPutAll: '&nbsp;']! !
!RBHtmlFormatter categoriesFor: #acceptLiteralNode:!public!visitor/double dispatching! !
!RBHtmlFormatter categoriesFor: #acceptVariableNode:!public!visitor/double dispatching! !
!RBHtmlFormatter categoriesFor: #cr!helpers!private! !
!RBHtmlFormatter categoriesFor: #formatMessageSelector:withArguments:multiline:!formatting!private! !
!RBHtmlFormatter categoriesFor: #formatMethodCommentFor:indentBefore:!formatting!private! !
!RBHtmlFormatter categoriesFor: #formatMethodPatternFor:!formatting!private! !
!RBHtmlFormatter categoriesFor: #formatStatementCommentFor:!formatting!private! !
!RBHtmlFormatter categoriesFor: #formatTemporariesFor:!formatting!private! !
!RBHtmlFormatter categoriesFor: #newLine!helpers!private! !
!RBHtmlFormatter categoriesFor: #span:around:!helpers!private! !
!RBHtmlFormatter categoriesFor: #span:while:!helpers!private! !
!RBHtmlFormatter categoriesFor: #tab!helpers!private! !

!RBHtmlFormatter class methodsFor!

cssStyleSheet
	^'
.selector {color: blue;
	font-weight: bold;}
.argument {font-weight: bold;}
.comment {color: green;
	font-style: italic;}
.message { color: blue; }
.literal { color: #990099;}
.temp	{font-style: italic;}
.stError	{color: black;
	font-weight: bold;
	font-size: Large;
	background: red;}
.classPrefix {	font-weight: bold; }

'! !
!RBHtmlFormatter class categoriesFor: #cssStyleSheet!public! !

RBLaTexFormatter guid: (GUID fromString: '{E7AFF33B-4551-4B11-832F-71AD42D8A980}')!
RBLaTexFormatter comment: ''!
!RBLaTexFormatter categoriesForClass!Unclassified! !
!RBLaTexFormatter methodsFor!

acceptLiteralNode: aLiteralNode 
	| literalStream literal |
	literalStream := ReadWriteStream on: String new.
	aLiteralNode token storeOn: literalStream.
	literal := literalStream contents.
	codeStream nextPutAll: (self span: 'stLiteral' around: literal)!

acceptMethodNode: aMethodNode 

self environment: 'verbatim'
		around: 
			[
	self formatMethodPatternFor: aMethodNode.
	self formatMethodBodyFor: aMethodNode]!

acceptReturnNode: aReturnNode 
	codeStream nextPutAll: '\^{}'.
	self visitNode: aReturnNode value!

acceptVariableNode: aVariableNode 
| name |
	name := aVariableNode name.
	name first isUppercase ifTrue: [codeStream nextPutAll: (self span: 'stClass' around: aVariableNode name)] ifFalse: [codeStream nextPutAll: (self span: 'stVariable' around: aVariableNode name)]
	!

cr
	codeStream
		nextPutAll: '~\\';
		cr!

environment: aString around: aBlock 
^aBlock value.
	codeStream
		nextPutAll: '\begin{';
		nextPutAll: aString;
		nextPut: $};
		cr.
	aBlock value.
	codeStream
		nextPutAll: '\end{';
		nextPutAll: aString;
		nextPut: $};
		cr!

formatMessageSelector: selectorParts withArguments: formattedArgs multiline: multiLine 
	formattedArgs isEmpty 
		ifTrue: [codeStream nextPutAll: (self span: 'stMessage' around: selectorParts first value)]
		ifFalse: 
			[1 to: formattedArgs size
				do: 
					[:i | 
					i ~~ 1 & multiLine not ifTrue: [self space].
					codeStream
						nextPutAll: (self span: 'stMessage' around: (selectorParts at: i) value);
						nextPut: $ ;
						nextPutAll: (formattedArgs at: i).
					(multiLine and: [i < formattedArgs size]) ifTrue: [self newLine]]]!

formatMethodCommentFor: aNode indentBefore: aBoolean 
	| source comments |
	source := aNode source.
	source isNil ifTrue: [^self].
	comments := aNode methodComments.
	comments isEmpty ifTrue: [^self].
	comments do: 
			[:each | 
			aBoolean ifTrue: [self newLine].
			codeStream 
				nextPutAll: (self span: 'stComment' around: (self texify: ((aNode source copyFrom: each first to: each last))) )]
		separatedBy: 
			[self cr.
			aBoolean ifFalse: [self newLine]].
	aBoolean ifTrue: [self cr] ifFalse: [self newLine]!

formatMethodPatternFor: aMethodNode 
	| selectorParts arguments |
	selectorParts := aMethodNode selectorParts.
	arguments := aMethodNode arguments.
	arguments isEmpty 
		ifTrue: [codeStream nextPutAll: (self span: 'stSelector' around: selectorParts first value)]
		ifFalse: 
			[selectorParts with: arguments
				do: 
					[:selector :arg | 
					codeStream
						nextPutAll: (self span: 'stSelector' around: selector value);
						nextPut: $ .
					self visitArgument: arg.
					self space]]!

formatStatementCommentFor: aNode 
	| source |
	source := aNode source.
	source isNil ifTrue: [^self].
	aNode statementComments do: 
			[:each | 
			| crs |
			crs := self newLinesFor: source startingAt: each first.
			(crs - 1 max: 0) timesRepeat: [self newLine].
			crs == 0 ifTrue: [self tab] ifFalse: [self newLine].
			codeStream nextPutAll: (self span: 'stComment' around: (source copyFrom: each first to: each last))]!

formatTemporariesFor: aSequenceNode 
	| temps |
	temps := aSequenceNode temporaries.
	temps isEmpty ifTrue: [^self].
	codeStream nextPutAll: '| '.
	temps do: 
			[:each | 
			self visitArgument: each.
			self space].
	codeStream nextPut: $|.
	self newLine!

newLine
	firstLineLength isNil ifTrue: [firstLineLength := codeStream position].
	self cr.
	indent timesRepeat: [self tab].
	lineStart := codeStream position!

span: aTexCommand around: aString 
	^'\' , aTexCommand, 
	'{' , aString , '}'!

span: aTexCommand while: aBlock 
	codeStream nextPutAll: '\' , aTexCommand . '{'.
	aBlock value.
	codeStream nextPutAll: '}'!

tab
	codeStream nextPutAll: '\stTab'!

texify: aString 
	| result source |
	result := ReadWriteStream on: String new.
	source := ReadStream on: aString.
	[source atEnd] whileFalse: 
			[| char |
			char := source next.
			('#^' includes: char) ifTrue: [result nextPut: $\].
			result nextPut: char].
	^result contents! !
!RBLaTexFormatter categoriesFor: #acceptLiteralNode:!public!visitor/double dispatching! !
!RBLaTexFormatter categoriesFor: #acceptMethodNode:!public!visitor/double dispatching! !
!RBLaTexFormatter categoriesFor: #acceptReturnNode:!public!visitor/double dispatching! !
!RBLaTexFormatter categoriesFor: #acceptVariableNode:!public!visitor/double dispatching! !
!RBLaTexFormatter categoriesFor: #cr!helpers!private! !
!RBLaTexFormatter categoriesFor: #environment:around:!helpers!private! !
!RBLaTexFormatter categoriesFor: #formatMessageSelector:withArguments:multiline:!formatting!private! !
!RBLaTexFormatter categoriesFor: #formatMethodCommentFor:indentBefore:!formatting!private! !
!RBLaTexFormatter categoriesFor: #formatMethodPatternFor:!formatting!private! !
!RBLaTexFormatter categoriesFor: #formatStatementCommentFor:!formatting!private! !
!RBLaTexFormatter categoriesFor: #formatTemporariesFor:!formatting!private! !
!RBLaTexFormatter categoriesFor: #newLine!helpers!private! !
!RBLaTexFormatter categoriesFor: #span:around:!helpers!private! !
!RBLaTexFormatter categoriesFor: #span:while:!helpers!private! !
!RBLaTexFormatter categoriesFor: #tab!helpers!private! !
!RBLaTexFormatter categoriesFor: #texify:!public! !

"Binary Globals"!

