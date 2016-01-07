| package |
package := Package name: 'US Writing API for XML Tests'.
package paxVersion: 1;
	basicComment: '$id: US Writing API for XML Tests 0.002$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Based on "WAX-RMV.7.mcz" (2008-10-22 03:23:01) from the "Writing API for XML" Project on Squeak Source:
http://www.squeaksource.com/@WKdv9M7Ta-2KxKvg/ds0z_a21

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.002'.


package classNames
	add: #WAXTest;
	add: #XMLUtilTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	add: 'US Writing API for XML';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #WAXTest
	instanceVariableNames: 'lineSeparator stream wax expected'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #XMLUtilTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

WAXTest guid: (GUID fromString: '{EA227DB7-528C-4A1C-B7B1-1427DD3E71A6}')!
WAXTest comment: ''!
!WAXTest categoriesForClass!Unclassified! !
!WAXTest methodsFor!

assert	"Private - assert that the stream contains the expected contents"	self assert: expected equals: stream contents!

assert: aString equals: aString2 
	self assert: aString = aString2!

setUp
	stream := WriteStream on: String new.	"to write to a String"
	wax := WAX stream: stream.
	lineSeparator := wax lineSeparator!

testAttributes	wax		start: 'root';		prefix: 'foo' namespace: 'http://www.ociweb.com/foo';		attr: 'a1' value: 'v1';		attr: 'a2' value: '2';		prefix: 'foo' attr: 'a3' value: 'bar';		prefix: 'foo' attr: 'a4' value: 'baz' newline: true;		close.	lineSeparator := wax lineSeparator.	expected :=		'<root' , lineSeparator ,		'  xmlns:foo="http://www.ociweb.com/foo"' , lineSeparator ,		'  a1="v1"' , lineSeparator ,		'  a2="2"' , lineSeparator ,		'  foo:a3="bar"' , lineSeparator ,		'  foo:a4="baz"/>'.	self assert!

testAttributeWithEscape	wax start: 'root';		 attr: 'a' value: '1&2';		 close.	expected := '<root a="1&amp;2"/>'.	self assert!

testAttributeWithoutEscape	wax start: 'root';		 unescapedAttr: 'a' value: '1&2';		 close.	expected := '<root a="1&2"/>'.	self assert!

testBadAttributeName		self should: [ wax start: 'root'; attr: '1a' value: 'value'; close ]		raise: IllegalArgumentException!

testBadAttributeTimingCaught	"Can't call attr:value: after calling text:"	self should: [ wax start: 'root'; text: 'text'; attr: '1a' value: 'v1' ]		raise: IllegalStateException!

testBadCDATA	"Can't call attr:value: after calling text:."	self should: [ wax cdata: 'text' ] raise: IllegalStateException!

testBadChild	"Can't call child:text: after root is closed."	self should: [ wax start: 'root'; end; child: 'child' text: 'text' ] raise: IllegalStateException!

testBadCloseAlreadyClosedByWAX	self should: [ wax start: 'root'; close; close ] raise: IllegalStateException!

testBadCloseThenWrite	self should: [ wax start: 'root'; close; start: 'more' ] raise: IllegalStateException!

testBadCloseWithoutRoot	"didn't write anything yet"	self should: [ wax close ] raise: IllegalStateException!

testBadComment	self should: [ wax start: 'root'; comment: 'foo--bar' ]		raise: IllegalArgumentException!

testBadDTDAfterRoot	"can't specify DTD after root element"	self should: [ wax start: 'root'; dtd: 'root.dtd' ] raise: IllegalStateException!

testBadDTDMultiple	"can't specify DTD after root element"	self should: [ wax start: 'root'; dtd: 'one.dtd'; dtd: 'two.dtd' ]		raise: IllegalStateException!

testBadElementName	self should: [ wax start: '1root' ] raise: IllegalArgumentException!

testBadEnd	"haven't called start: yet"	self should: [ wax end ] raise: IllegalStateException!

testBadEntityDef	"can't define an entity after the root element start tag"	self should: [ wax start: 'root'; entityDef: 'name' value: 'value' ] raise: IllegalStateException!

testBadExtraEnd	self should: [ wax start: 'root'; end; end ] raise: IllegalStateException!

testBadIndentBadChars	self should: [ wax indentChars: 'abc' ] raise: IllegalArgumentException!

testBadIndentMultipleTabs	self should: [ wax indentChars: '\t\t' ] raise: IllegalArgumentException!

testBadIndentNegative	self should: [ wax indentSize: 5 ] raise: IllegalArgumentException!

testBadIndentTooLarge	"must be <= 4"	self should: [ wax indentSize: 5 ] raise: IllegalArgumentException!

testBadLineSeparatorTiming	self should: [		wax start: 'root'.		"can't call after output has started"		wax lineSeparator: 'abc'	] raise: IllegalStateException!

testBadNamespaceDuplicatePrefix	"can't define same namespace prefix more than once on the same element"	self should: [		wax start: 'root';			prefix: 'tns' namespace: 'http://www.ociweb.com/tns';			prefix: 'tns' namespace: 'http://www.ociweb.com/tns'	] raise: IllegalArgumentException!

testBadNamespaceInElementContent	"can't define same default namespace more than once on the same element"	self should: [		wax start: 'root';			text: 'text';			prefix: 'tns' namespace: 'http://www.ociweb.com/tns'	] raise: IllegalStateException!

testBadNamespaceMultipleDefault	"can't define same default namespace more than once on the same element"	self should: [		wax start: 'root';			defaultNamespace: 'http://www.ociweb.com/tns';			defaultNamespace: 'http://www.ociweb.com/tns'	] raise: IllegalArgumentException!

testBadNoRoot	self should: [ wax close ] raise: IllegalStateException!

testBadPrefix	self should: [		wax start: 'root';			start: 'parent';			prefix: 'foo' namespace: 'http://www.ociweb.com/foo';			prefix: 'foo' child: 'child1' text: 'one';			end.		"The prefix 'foo' is out of scope now."		wax prefix: 'foo' child: 'child2' text: 'two';			close.	] raise: IllegalStateException!

testBadSetLineSeparator	"Since error checking is turned on, element names must be valid."	self should: [ wax lineSeparator: 'abc' ] raise: IllegalArgumentException!

testBadTextAfterRootEnd	self should: [		wax start: 'root'; end.		"Can't output more text after root element is terminated."		wax text: 'text'	] raise: IllegalStateException!

testBadTextInProlog	"haven't called start: yet"	self should: [ wax text: 'text' ] raise: IllegalStateException!

testBadTrustMeFalse	wax trustMe: false.	"Since error checking is turned on, element names must be valid."	self should: [ wax start: '123' ] raise: IllegalArgumentException!

testBadWrite	wax start: 'root'.	"closing stream instead of allowing WAX to do it"	stream close.	"Note that in Smalltalk, closing a stream doesn't do anything!!	That is why it is able to write to the stream after close was called."	wax close.	"No exception is raised, so there's really nothing to test."!

testBadWriteFile
	self should: 
			[| fs |
			fs := FileStream write:  '.'.	"the current directory, not a file"
			"This isn't a WAX issue. The line above signals the exception."
			wax := WAX stream: fs]
		raise: FileException!

testBadXSLTAfterRoot	"can't write this pi after root element"	self should: [ wax start: 'root'; xslt: 'root.xslt' ] raise: IllegalStateException!

testBadXSLTMultiple	"can't specify two XSLTs"	self should: [ wax xslt: 'one.xslt'; xslt: 'two.xslt' ] raise: IllegalStateException!

testBig	wax		start: 'root';		text: 'text #1' newline: true;		child: 'child1' text: 'text';		text: 'text #2' newline: true;		start: 'child2';		attr: 'a1' value: 'v1';		end;		text: 'text #3' newline: true;		close.	expected :=		'<root>' , lineSeparator ,		'  text #1' , lineSeparator ,		'  <child1>text</child1>' , lineSeparator ,		'  text #2' , lineSeparator ,		'  <child2 a1="v1"/>' , lineSeparator ,		'  text #3' , lineSeparator ,		'</root>'.	self assert!

testBlankLine	wax		start: 'root';		blankLine;		close.	expected :=		'<root>' , lineSeparator ,		lineSeparator ,		'</root>'.	self assert!

testCDATAWithNewlines	wax		start: 'root';		cdata: '1<2>3&4''5"6' newline: true;		close.	expected :=		'<root>' , lineSeparator ,		'  <!![CDATA[1<2>3&4''5"6]]>' , lineSeparator ,		'</root>'.	self assert!

testCDATAWithoutNewlines	wax		start: 'root';		cdata: '1<2>3&4''5"6';		close.	expected := '<root><!![CDATA[1<2>3&4''5"6]]></root>'.	self assert!

testCommentedStartWithContent	wax		start: 'root';		commentedStart: 'child';		child: 'grandchild' text: 'some text';		close.	expected :=		'<root>' , lineSeparator ,		'  <!!--child>' , lineSeparator ,		'    <grandchild>some text</grandchild>' , lineSeparator ,		'  </child-->' , lineSeparator ,		'</root>'.	self assert!

testCommentedStartWithNamespace	wax		start: 'root';		prefix: 'foo' namespace: 'http://www.ociweb.com/foo';		prefix: 'foo' commentedStart: 'child';		child: 'grandchild' text: 'some text';		close.	expected :=		'<root' , lineSeparator ,		'  xmlns:foo="http://www.ociweb.com/foo">' , lineSeparator ,		'  <!!--foo:child>' , lineSeparator ,		'    <grandchild>some text</grandchild>' , lineSeparator ,		'  </foo:child-->' , lineSeparator ,		'</root>'.	self assert!

testCommentedStartWithoutContent	wax		start: 'root';		commentedStart: 'child1';		end;		child: 'child2' text: 'some text';		close.	expected :=		'<root>' , lineSeparator ,		'  <!!--child1/-->' , lineSeparator ,		'  <child2>some text</child2>' , lineSeparator ,		'</root>'.	self assert!

testCommentWithNewlines	wax		comment: 'comment #1' newline: true;		comment: 'comment #2' newline: true;		start: 'root';		comment: 'comment #3' newline: true;		close.	expected :=		'<!!--' , lineSeparator ,		'  comment #1' , lineSeparator ,		'-->' , lineSeparator ,		'<!!--' , lineSeparator ,		'  comment #2' , lineSeparator ,		'-->' , lineSeparator ,		'<root>' , lineSeparator ,		'  <!!--' , lineSeparator ,		'    comment #3' , lineSeparator ,		'  -->' , lineSeparator ,		'</root>'.	self assert!

testCommentWithOutNewlines	wax		comment: 'comment #1';		comment: 'comment #2';		start: 'root';		comment: 'comment #3';		close.	expected :=		'<!!-- comment #1 -->' , lineSeparator ,		'<!!-- comment #2 -->' , lineSeparator ,		'<root>' , lineSeparator ,		'  <!!-- comment #3 -->' , lineSeparator ,		'</root>'.	self assert!

testDTDPublic	"testing with the ids for the strict form of XHTML"		| publicId systemId |	publicId := '-//W3C//DTD XHTML 1.0 Strict//EN'.	systemId := 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'.		wax		dtd: publicId systemId: systemId;		start: 'root';		close.	expected :=		'<!!DOCTYPE root PUBLIC "', publicId, '" "', systemId, '">', lineSeparator,		'<root/>'.	self assert!

testDTDSystem	| systemId |	systemId := 'http://www.ociweb.com/xml/root.dtd'.		wax		dtd: systemId;		start: 'root';		close.	expected :=		'<!!DOCTYPE root SYSTEM "', systemId, '">', lineSeparator,		'<root/>'.	self assert!

testEmpty	wax start: 'root'; close.	expected := '<root/>'.	self assert!

testEndVerbose	wax start: 'root'; end: true; close.	expected := '<root></root>'.	self assert!

testEntityDef	wax		noIndentsOrLineSeparators;		entityDef: 'name' value: 'value';		start: 'root';		close.	expected := '<!!DOCTYPE root [<!!ENTITY name "value">]><root/>'.	self assert!

testEscape	wax		noIndentsOrLineSeparators;		start: 'root';		text: 'abc<def>ghi''jkl"mno&pqr';		close.	expected := '<root>abc&lt;def&gt;ghi&apos;jkl&quot;mno&amp;pqr</root>'.	self assert!

testEscapeOffAndOn	wax		noIndentsOrLineSeparators;		start: 'root';		unescapedText: '&';		text: '&';		close.	expected := '<root>&&amp;</root>'.	self assert!

testExternalEntityDef	wax		noIndentsOrLineSeparators;		externalEntityDef: 'name' filePath: 'value';		start: 'root';		close.	expected := '<!!DOCTYPE root [<!!ENTITY name SYSTEM "value">]><root/>'.	self assert!

testGetIndent	self assert: '  ' equals: wax indent.	wax indentChars: '    '.	self assert: '    ' equals: wax indent.	wax indentChars: Character tab.	self assert: (Character tab asString) equals: wax indent.	wax noIndentsOrLineSeparators.	self assert: nil equals: wax indent.	wax indentChars: ''.	self assert: '' equals: wax indent!

testIndentByNum	wax		indentSize: 1;		start: 'root';		child: 'child' text: 'text';		close.	expected :=		'<root>', lineSeparator,		' <child>text</child>', lineSeparator,		'</root>'.	self assert!

testIndentByStringWeird	| chars indent |	indent := 'abc'.	wax		trustMe: true;		indentChars: indent;		trustMe: false;		start: 'root';		child: 'child' text: 'text';		close.			chars := wax indent.	self assert: indent equals: chars.		expected :=		'<root>', lineSeparator,		indent, '<child>text</child>', lineSeparator,		'</root>'.	self assert!

testNamespace	wax		noIndentsOrLineSeparators;		start: 'root';		defaultNamespace: 'http://www.ociweb.com/tns1';		prefix: 'tns2' namespace: 'http://www.ociweb.com/tns2';		prefix: 'tns3' namespace: 'http://www.ociweb.com/tns3';		close.	expected :=		'<root ',		'xmlns="http://www.ociweb.com/tns1"',		' xmlns:tns2="http://www.ociweb.com/tns2"',		' xmlns:tns3="http://www.ociweb.com/tns3"/>'.	self assert!

testNamespaceDuplicatePrefix	| prefix uri |	prefix := 'tns'.	uri := 'http://www.ociweb.com/tns'.	wax		noIndentsOrLineSeparators;		start: 'root';		prefix: prefix namespace: uri, '1';		start: 'child';		prefix: prefix namespace: uri, '2';		close.	expected :=		'<root xmlns:', prefix, '="', uri, '1">',		'<child xmlns:', prefix, '="', uri, '2"/>',		'</root>'.	self assert!

testNamespaceMultipleDefault	| uri |	uri := 'http://www.ociweb.com/tns'.	wax		noIndentsOrLineSeparators;		start: 'root';		defaultNamespace: uri, '1';		start: 'child';		defaultNamespace: uri, '2';		close.	expected :=		'<root xmlns="', uri, '1">',		'<child xmlns="', uri, '2"/>',		'</root>'.	self assert!

testNilComment	self should: [ wax comment: nil ] raise: IllegalArgumentException!

testNilCommentAfterClose	wax start: 'root'; close; trustMe: true.	self should: [ wax comment: nil ] raise: IllegalStateException!

testNilCommentWithTrustMe	wax		trustMe: true;		comment: nil;		start: 'root';		close.	expected :=		'<!!--  -->',		lineSeparator,		'<root/>'.	self assert!

testNoAmpersandOrLessThanQuotingInComment	wax		noIndentsOrLineSeparators;		start: 'root';		comment: '1&2<3';		close.	expected := '<root><!!-- 1&2<3 --></root>'.	self assert!

testNoAmpersandOrLessThanQuotingInProcessingInstruction	wax		noIndentsOrLineSeparators;		start: 'root';		trustMe: true;		processingInstruction: '1&2<3' data: '3&2<1';		close.	expected := '<root><?1&2<3 3&2<1?></root>'.	self assert!

testNoIndent	wax		noIndentsOrLineSeparators;		start: 'root';		child: 'child' text: 'text';		close.	expected := '<root><child>text</child></root>'.	self assert!

testNoIndentOrLineSeparators	wax noIndentsOrLineSeparators.	self assert: nil equals: wax indent!

testPrefix	wax		noIndentsOrLineSeparators;		prefix: 'foo' start: 'root';		attr: 'bar' value: 'baz';		prefix: 'foo' namespace: 'http://www.ociweb.com/foo';		close.	expected := '<foo:root bar="baz" xmlns:foo="http://www.ociweb.com/foo"/>'.	self assert!

testProcessingInstructionAfterProlog	wax		noIndentsOrLineSeparators;		start: 'root';		processingInstruction: 'target1' data: 'data1';		pi: 'target2' data: 'data2';		close.	expected := '<root><?target1 data1?><?target2 data2?></root>'.	self assert!

testProcessingInstructionInProlog	wax		processingInstruction: 'xml-stylesheet'			data: 'type="text/xsl" href="http://www.ociweb.com/foo.xslt"';		start: 'root';		close.	expected :=		'<?xml-stylesheet type="text/xsl" href="http://www.ociweb.com/foo.xslt"?>',		lineSeparator,		'<root/>'.	self assert!

testSchemasWithIndent	wax		start: 'root';		defaultNamespace: 'http://www.ociweb.com/tns1' schemaPath: 'tns1.xsd';		prefix: 'tns2' namespace: 'http://www.ociweb.com/tns2' schemaPath: 'tns2.xsd';		prefix: 'tns3' ns: 'http://www.ociweb.com/tns3' schemaPath: 'tns3.xsd';		close.	expected :=		'<root', lineSeparator,		'  xmlns="http://www.ociweb.com/tns1"', lineSeparator,		'  xmlns:tns2="http://www.ociweb.com/tns2"', lineSeparator,		'  xmlns:tns3="http://www.ociweb.com/tns3"', lineSeparator,		'  xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"', lineSeparator,		'  xsi:schemaLocation="http://www.ociweb.com/tns1 tns1.xsd', lineSeparator,		'    http://www.ociweb.com/tns2 tns2.xsd', lineSeparator,		'    http://www.ociweb.com/tns3 tns3.xsd"/>'.	self assert!

testSchemasWithoutIndent	wax		noIndentsOrLineSeparators;		start: 'root';		defaultNamespace: 'http://www.ociweb.com/tns1' schemaPath: 'tns1.xsd';		prefix: 'tns2' namespace: 'http://www.ociweb.com/tns2' schemaPath: 'tns2.xsd';		prefix: 'tns3' ns: 'http://www.ociweb.com/tns3' schemaPath: 'tns3.xsd';		close.	expected :=		'<root',		' xmlns="http://www.ociweb.com/tns1"',		' xmlns:tns2="http://www.ociweb.com/tns2"',		' xmlns:tns3="http://www.ociweb.com/tns3"',		' xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"',		' xsi:schemaLocation="http://www.ociweb.com/tns1 tns1.xsd',		' http://www.ociweb.com/tns2 tns2.xsd',		' http://www.ociweb.com/tns3 tns3.xsd"/>'.	self assert!

testSetLineSeparator	wax lineSeparator: (WAX unixLineSeparator).	self assert: WAX unixLineSeparator equals: wax lineSeparator		"Most of the other tests verify that	 this line separator is actually used in the output."!

testSpaceInEmptyElements	wax		noIndentsOrLineSeparators;		start: 'root';		start: 'child1'; end.			self deny: wax isSpaceInEmptyElements.	wax spaceInEmptyElements: true.	self assert: wax isSpaceInEmptyElements.		wax start: 'child2'; end.			wax spaceInEmptyElements: false.	self deny: wax isSpaceInEmptyElements.		wax start: 'child3'; close.		expected := '<root><child1/><child2 /><child3/></root>'.	self assert!

testText	wax		noIndentsOrLineSeparators;		start: 'root';		text: 'text';		close.		expected := '<root>text</root>'.	self assert!

testTrustMeTrue	self deny: wax trustMe.	wax trustMe: true.	self assert: wax trustMe.		wax		noIndentsOrLineSeparators;		start: '123';		unescapedText: '<>&''"';		close.		expected := '<123><>&''"</123>'.	self assert!

testUseWindowsLineSeparator	wax		lineSeparator: WAX windowsLineSeparator;		start: 'root';		child: 'child' text: 'text';		close.		lineSeparator := wax lineSeparator.	expected :=		'<root>', lineSeparator,		'  <child>text</child>', lineSeparator,		'</root>'.	self assert!

testXMLDeclaration	| myWAX |	myWAX := WAX stream: stream version: #'1.0'.	myWAX		noIndentsOrLineSeparators;		start: 'root';		close.		expected :=		'<?xml version="1.0" encoding="UTF-8"?>', lineSeparator,		'<root/>'.	self assert!

testXMLVersion11	| myWAX |	myWAX := WAX stream: stream version: #'1.1'.	myWAX start: 'root'; close.		expected :=		'<?xml version="1.1" encoding="UTF-8"?>', lineSeparator,		'<root/>'.	self assert!

testXMLVersionNil	"Currently a version of nil means that no XML declaration should be written.	 It's not an error."	"self should: [ WAX stream: stream version: nil ] raise: IllegalArgumentException"!

testXSLT	wax		xslt: 'root.xslt';		start: 'root';		close.		expected :=		'<?xml-stylesheet type="text/xsl" href="root.xslt"?>', lineSeparator,		'<root/>'.	self assert! !
!WAXTest categoriesFor: #assert!private! !
!WAXTest categoriesFor: #assert:equals:!public! !
!WAXTest categoriesFor: #setUp!public!running! !
!WAXTest categoriesFor: #testAttributes!public!tests! !
!WAXTest categoriesFor: #testAttributeWithEscape!public!tests! !
!WAXTest categoriesFor: #testAttributeWithoutEscape!public!tests! !
!WAXTest categoriesFor: #testBadAttributeName!public!tests! !
!WAXTest categoriesFor: #testBadAttributeTimingCaught!public!tests! !
!WAXTest categoriesFor: #testBadCDATA!public!tests! !
!WAXTest categoriesFor: #testBadChild!public!tests! !
!WAXTest categoriesFor: #testBadCloseAlreadyClosedByWAX!public!tests! !
!WAXTest categoriesFor: #testBadCloseThenWrite!public!tests! !
!WAXTest categoriesFor: #testBadCloseWithoutRoot!public!tests! !
!WAXTest categoriesFor: #testBadComment!public!tests! !
!WAXTest categoriesFor: #testBadDTDAfterRoot!public!tests! !
!WAXTest categoriesFor: #testBadDTDMultiple!public!tests! !
!WAXTest categoriesFor: #testBadElementName!public!tests! !
!WAXTest categoriesFor: #testBadEnd!public!tests! !
!WAXTest categoriesFor: #testBadEntityDef!public!tests! !
!WAXTest categoriesFor: #testBadExtraEnd!public!tests! !
!WAXTest categoriesFor: #testBadIndentBadChars!public!tests! !
!WAXTest categoriesFor: #testBadIndentMultipleTabs!public!tests! !
!WAXTest categoriesFor: #testBadIndentNegative!public!tests! !
!WAXTest categoriesFor: #testBadIndentTooLarge!public!tests! !
!WAXTest categoriesFor: #testBadLineSeparatorTiming!public!tests! !
!WAXTest categoriesFor: #testBadNamespaceDuplicatePrefix!public!tests! !
!WAXTest categoriesFor: #testBadNamespaceInElementContent!public!tests! !
!WAXTest categoriesFor: #testBadNamespaceMultipleDefault!public!tests! !
!WAXTest categoriesFor: #testBadNoRoot!public!tests! !
!WAXTest categoriesFor: #testBadPrefix!public!tests! !
!WAXTest categoriesFor: #testBadSetLineSeparator!public!tests! !
!WAXTest categoriesFor: #testBadTextAfterRootEnd!public!tests! !
!WAXTest categoriesFor: #testBadTextInProlog!public!tests! !
!WAXTest categoriesFor: #testBadTrustMeFalse!public!tests! !
!WAXTest categoriesFor: #testBadWrite!public!tests! !
!WAXTest categoriesFor: #testBadWriteFile!public!tests! !
!WAXTest categoriesFor: #testBadXSLTAfterRoot!public!tests! !
!WAXTest categoriesFor: #testBadXSLTMultiple!public!tests! !
!WAXTest categoriesFor: #testBig!public!tests! !
!WAXTest categoriesFor: #testBlankLine!public!tests! !
!WAXTest categoriesFor: #testCDATAWithNewlines!public!tests! !
!WAXTest categoriesFor: #testCDATAWithoutNewlines!public!tests! !
!WAXTest categoriesFor: #testCommentedStartWithContent!public!tests! !
!WAXTest categoriesFor: #testCommentedStartWithNamespace!public!tests! !
!WAXTest categoriesFor: #testCommentedStartWithoutContent!public!tests! !
!WAXTest categoriesFor: #testCommentWithNewlines!public!tests! !
!WAXTest categoriesFor: #testCommentWithOutNewlines!public!tests! !
!WAXTest categoriesFor: #testDTDPublic!public!tests! !
!WAXTest categoriesFor: #testDTDSystem!public!tests! !
!WAXTest categoriesFor: #testEmpty!public!tests! !
!WAXTest categoriesFor: #testEndVerbose!public!tests! !
!WAXTest categoriesFor: #testEntityDef!public!tests! !
!WAXTest categoriesFor: #testEscape!public!tests! !
!WAXTest categoriesFor: #testEscapeOffAndOn!public!tests! !
!WAXTest categoriesFor: #testExternalEntityDef!public!tests! !
!WAXTest categoriesFor: #testGetIndent!public!tests! !
!WAXTest categoriesFor: #testIndentByNum!public!tests! !
!WAXTest categoriesFor: #testIndentByStringWeird!public!tests! !
!WAXTest categoriesFor: #testNamespace!public!tests! !
!WAXTest categoriesFor: #testNamespaceDuplicatePrefix!public!tests! !
!WAXTest categoriesFor: #testNamespaceMultipleDefault!public!tests! !
!WAXTest categoriesFor: #testNilComment!public!tests! !
!WAXTest categoriesFor: #testNilCommentAfterClose!public!tests! !
!WAXTest categoriesFor: #testNilCommentWithTrustMe!public!tests! !
!WAXTest categoriesFor: #testNoAmpersandOrLessThanQuotingInComment!public!tests! !
!WAXTest categoriesFor: #testNoAmpersandOrLessThanQuotingInProcessingInstruction!public!tests! !
!WAXTest categoriesFor: #testNoIndent!public!tests! !
!WAXTest categoriesFor: #testNoIndentOrLineSeparators!public!tests! !
!WAXTest categoriesFor: #testPrefix!public!tests! !
!WAXTest categoriesFor: #testProcessingInstructionAfterProlog!public!tests! !
!WAXTest categoriesFor: #testProcessingInstructionInProlog!public!tests! !
!WAXTest categoriesFor: #testSchemasWithIndent!public!tests! !
!WAXTest categoriesFor: #testSchemasWithoutIndent!public!tests! !
!WAXTest categoriesFor: #testSetLineSeparator!public!tests! !
!WAXTest categoriesFor: #testSpaceInEmptyElements!public!tests! !
!WAXTest categoriesFor: #testText!public!tests! !
!WAXTest categoriesFor: #testTrustMeTrue!public!tests! !
!WAXTest categoriesFor: #testUseWindowsLineSeparator!public!tests! !
!WAXTest categoriesFor: #testXMLDeclaration!public!tests! !
!WAXTest categoriesFor: #testXMLVersion11!public!tests! !
!WAXTest categoriesFor: #testXMLVersionNil!public!tests! !
!WAXTest categoriesFor: #testXSLT!public!tests! !

XMLUtilTest guid: (GUID fromString: '{77FBDD2B-6AD6-4709-8DC8-2A88CB4BD32F}')!
XMLUtilTest comment: ''!
!XMLUtilTest categoriesForClass!Unclassified! !
!XMLUtilTest methodsFor!

assert: aString equals: aString2 
	self assert: aString = aString2!

flag: aSymbol 
	!

testBadComment	self should: [XMLUtil verifyComment: 'one -- two'] raise: IllegalArgumentException!

testBadName	self should: [XMLUtil verifyName: '1a'] raise: IllegalArgumentException!

testBadURI	self flag: #todo. "commented out since not working yet"	"self should: [XMLUtil verifyURI: ':junk'] raise: Error."!

testBadVersion	self should: [XMLUtil verifyVersion: '1.3'] raise: IllegalArgumentException!

testEscape	"test the escape method"	self assert: '&amp;' equals: (XMLUtil escape: '&').	self assert: '&lt;' equals: (XMLUtil escape: '<').	self assert: '&gt;' equals: (XMLUtil escape: '>').	self assert: '&apos;' equals: (XMLUtil escape: '''').	self assert: '&quot;' equals: (XMLUtil escape: '"').	self assert: '1&lt;2&gt;3&amp;4&apos;5&quot;6' equals: (XMLUtil escape: '1<2>3&4''5"6').!

testIsComment	"test the isComment method"	self assert: (XMLUtil isComment: 'one two').	self assert: (XMLUtil isComment: 'one - two').	self assert: (XMLUtil isComment: 'one -- two') not.	self assert: (XMLUtil isComment: '-- one two') not.	self assert: (XMLUtil isComment: 'one two --') not.!

testIsName	"test the isName method"		self assert: (XMLUtil isName: nil) not.	self assert: (XMLUtil isName: 'a1').	self assert: (XMLUtil isName: ':=a1').	self assert: (XMLUtil isName: '1a') not.		self assert: (XMLUtil isName: 'xmlFoo') not.	self assert: (XMLUtil isName: 'XMLFoo') not.	self assert: (XMLUtil isName: 'xMLFoo') not.		self flag: #todo. "Still need to test for non-Latin Unicode character."!

testIsURI	"test the isURI method"	self flag: #todo. "This isn't working yet!!"	"self assert: (XMLUtil isURI: 'http://www.ociweb.com/foo').	self assert: (XMLUtil isURI: ':junk') not."!

testIsVersion	"test the isVersion method"	self assert: (XMLUtil isVersion: #'1.0').	self assert: (XMLUtil isVersion: #'1.1').	self assert: (XMLUtil isVersion: #'1.2').	self assert: (XMLUtil isVersion: #'1.3') not.	! !
!XMLUtilTest categoriesFor: #assert:equals:!public! !
!XMLUtilTest categoriesFor: #flag:!public! !
!XMLUtilTest categoriesFor: #testBadComment!public!tests! !
!XMLUtilTest categoriesFor: #testBadName!public!tests! !
!XMLUtilTest categoriesFor: #testBadURI!public!tests! !
!XMLUtilTest categoriesFor: #testBadVersion!public!tests! !
!XMLUtilTest categoriesFor: #testEscape!public!tests! !
!XMLUtilTest categoriesFor: #testIsComment!public!tests! !
!XMLUtilTest categoriesFor: #testIsName!public!tests! !
!XMLUtilTest categoriesFor: #testIsURI!public!tests! !
!XMLUtilTest categoriesFor: #testIsVersion!public!tests! !

"Binary Globals"!

