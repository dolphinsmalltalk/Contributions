| package |
package := Package name: 'US Writing API for XML'.
package paxVersion: 1;
	basicComment: '$id: US Writing API for XML 0.008$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Based on "WAX-RMV.7.mcz" (2008-10-22 03:23:01) from the "Writing API for XML" Project on Squeak Source:
http://www.squeaksource.com/@WKdv9M7Ta-2KxKvg/ds0z_a21

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.008'.


package classNames
	add: #CDDemo;
	add: #IllegalArgumentException;
	add: #IllegalStateException;
	add: #State;
	add: #WAX;
	add: #XMLUtil;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'US Collection Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Collection Extensions').

package!

"Class Definitions"!

Object subclass: #CDDemo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #State
	instanceVariableNames: ''
	classVariableNames: 'AfterRoot InElement InProlog InStartTag'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WAX
	instanceVariableNames: 'attrOnNewLine checkMe closeStream defaultNSOnCurrentElement doctypePublicId doctypeSystemId dtdSpecified encoding entityDefs escape hasContent hasIndentedContent inCommentedStart indent lineSeparator namespaceURIToSchemaPathMap outputStarted parentStack pendingPrefixes prefixesStack spaceInEmptyElements state stream xsltSpecified'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #XMLUtil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #IllegalArgumentException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #IllegalStateException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

CDDemo guid: (GUID fromString: '{44BDD8E4-9BC6-41AA-B7F3-22822483FE98}')!
CDDemo comment: 'This class demonstrates use of the WAX library.'!
!CDDemo categoriesForClass!Unclassified! !
!CDDemo class methodsFor!

main	"demonstrates using the WAX API"	"Run this with CDDemo main"	| wax |	wax := WAX new.	wax start: 'foo';		 text: 'bar';		 end;		 close! !
!CDDemo class categoriesFor: #main!public! !

State guid: (GUID fromString: '{8B94676B-FCFE-406F-AB22-E19366F3934A}')!
State comment: 'This represents the state of a WAX object.Consider using an array of symbols instead of this class.Instance Variables'!
!State categoriesForClass!Unclassified! !
!State class methodsFor!

afterRoot	^AfterRoot!

inElement	^InElement!

initialize	"initializes class variables"	super initialize.	InProlog := State new.	InStartTag := State new.	InElement := State new.	AfterRoot := State new.!

inProlog	^InProlog!

inStartTag	^InStartTag! !
!State class categoriesFor: #afterRoot!accessing!public! !
!State class categoriesFor: #inElement!accessing!public! !
!State class categoriesFor: #initialize!initialize/release!public! !
!State class categoriesFor: #inProlog!accessing!public! !
!State class categoriesFor: #inStartTag!accessing!public! !

WAX guid: (GUID fromString: '{430E050E-DF44-4E24-B953-6E8EB22D14D8}')!
WAX comment: 'This is the main class of the Writing API for XML (WAX) implementation.'!
!WAX categoriesForClass!Unclassified! !
!WAX methodsFor!

allSpaces: aString	"answer whether all characters in aString are spaces"	aString do: [:c | c = Character space ifFalse: [^false]].	^true!

attr: aName value: aValue 	"add an attribute to the current element"		^ self prefix: '' attr: aName value: aValue!

badState: aMethodName	"raise an exception indicating that the given method was called in an invalid state"	IllegalStateException signal: 'can''t call ', aMethodName, ' when state is ', state!

blankLine	"write a blank line to the stream"	self text: '' newline: true!

cdata: aString 	"write a CDATA section in the content of the current element"	self cdata: aString newline: false!

cdata: aString newline: aBoolean	"write a CDATA section to the stream"	(state = #InProlog) | (state = #AfterRoot) ifTrue: [ self badState: 'cdata' ].	escape := false.	self text: '<!![CDATA[', aString, ']]>' newline: aBoolean.	escape := true!

child: anElementName text: aString	"a convenience method that is a shortcut for start: anElementName; text: aString; end"		self prefix: nil child: anElementName text: aString		!

close	"terminate all unterminated elements and close the stream"	stream ifNil: [IllegalStateException signal: 'already closed'].		"Verify that a root element has been written."	state = #InProlog ifTrue: [ self badState: 'close' ].		[ parentStack isEmpty] whileFalse: [ self end ].	closeStream ifTrue: [ stream close] ifFalse: [ stream flush ].	stream := nil!

comment: aString 	"write an XML comment"		self comment: aString newline: false!

comment: aString newline: aBoolean	"write an XML comment"	| text |		checkMe ifTrue: [ XMLUtil verifyComment: aString ].		hasContent := hasIndentedContent := true.	self terminateStart.	parentStack ifNotEmpty: [ self writeIndent ].		text := aString ifNil: [''] ifNotNil: [aString].	aBoolean		ifTrue: [			self write: '<!!--'.			self writeIndent.			self write: indent, text.			self writeIndent.			self write: '-->'		]		ifFalse: [ self write: '<!!-- ', text, ' -->' ].			(self willIndent and: [parentStack isEmpty]) ifTrue: [ self write: lineSeparator ]!

commentedStart: anElementName 	"write a commented start tag"		self prefix: nil commentedStart: anElementName!

defaultNamespace: aURI	"write a namespace declaration for the default namespace	in the start tag of the current element."	self prefix: '' namespace: aURI!

defaultNamespace: aURI schemaPath: aPath	"write a namespace declaration for the default namespace	in the start tag of the current element."	self prefix: '' namespace: aURI schemaPath: aPath!

dtd: systemId	"write a DOCTYPE that associates a DTD with the XML document"	self dtd: nil systemId: systemId!

dtd: publicId systemId: systemId	"write a DOCTYPE that associates a DTD with the XML document"	dtdSpecified ifTrue: [ IllegalStateException signal: 'can''t specify more than one DTD' ].	state = #InProlog ifFalse: [ self badState: 'dtd' ].	checkMe ifTrue: [ XMLUtil verifyURI: systemId ].		doctypePublicId := publicId.	doctypeSystemId := systemId.	dtdSpecified := true!

end	"end an element"	self end: false!

end: verbose	"end an element ...	if no content and aBoolean is false then terminates in the shorthand way"	| name wasCommentedStart |	(state = #InProlog) | (state = #AfterRoot) ifTrue: [ self badState: 'end' ].	checkMe ifTrue: [ self verifyPrefixes ].		self writeSchemaLocations.		name := parentStack removeFirst.		"Namespace prefixes that were in scope for this element are no longer in scope."	prefixesStack removeFirst.		"Check for hypen at beginning of element name	which indicates that the commentedStart method was used."	wasCommentedStart := (name at: 1) = $-.	hasContent | verbose		ifTrue: [			verbose ifTrue: [ self write: '>' ].			hasIndentedContent ifTrue: [ self writeIndent ].			self write: '</'.			wasCommentedStart				ifTrue: [ self write: (name allButFirst), '-->' ]				ifFalse: [ self write: name, '>' ].		]		ifFalse: [			spaceInEmptyElements ifTrue: [ self write: ' ' ].			wasCommentedStart				ifTrue: [ self write: '/-->' ]				ifFalse: [ self write: '/>' ].		].	hasContent := hasIndentedContent := true. "new setting for parent"	state := parentStack isEmpty ifTrue: [#AfterRoot] ifFalse: [#InElement].!

entityDef: aName value: aString	"write an entity definition to the internal subset of the DOCTYPE"	state = #InProlog ifFalse: [ self badState: 'entityDef' ].		entityDefs add: aName, ' "', aString, '"'!

externalEntityDef: aName filePath: aFilePath	"write an external entity definition to the internal subset of the DOCTYPE"	self entityDef: aName, ' SYSTEM' value: aFilePath!

indent	^indent!

indentChars: aStringOrChar 
	"Sets the indentation characters to use.	This defaults to two spaces.	Unless 'trust me' is set to true, the only valid string values are	a single tab, one or more spaces, an empty string, or null.	Passing "

	" causes elements to be output on separate lines, but not indented.	Passing null causes all output to be on a single line."

	| string |
	string := aStringOrChar asString.
	checkMe 
		ifTrue: 
			[| valid |
			valid := string isEmptyOrNil or: [(aStringOrChar = Character tab) or: [self allSpaces: string]].
			(valid not or: [string notNil and: [string size > 4]]) 
				ifTrue: [IllegalArgumentException signal: 'invalid indent value']].
	indent := string!

indentSize: anInteger	"Sets the number of spaces to use for indentation.	The number must be >= 0 and <= 4 if 'trust me' is false.	This defaults to 2."		anInteger < 0 ifTrue: [		IllegalArgumentException signal: 'can''t indent a negative number of spaces'	].	(checkMe and: [ anInteger > 4]) ifTrue: [		IllegalArgumentException signal: anInteger displayString, ' is an unreasonable indentation'	].	indent := ''.	anInteger timesRepeat: [ indent := indent, ' ' ]!

initialize	"initialize a newly created WAX object"	attrOnNewLine := false.	checkMe := true.	closeStream := true.	defaultNSOnCurrentElement := false.	dtdSpecified := false.      encoding := 'UTF-8'.	entityDefs := OrderedCollection new.	escape := true.	hasContent := false.	hasIndentedContent := false.	indent := '  '.	inCommentedStart := false.	namespaceURIToSchemaPathMap := Dictionary new.	outputStarted := false.	parentStack := OrderedCollection new.	pendingPrefixes := OrderedCollection new.	prefixesStack := OrderedCollection new.	spaceInEmptyElements := false.	state := #InProlog.	stream := Transcript.	xsltSpecified := false.		self lineSeparator: WAX unixLineSeparator!

isInScopePrefix: aPrefix	"Private - answer whether a given namespace prefix is currently in scope"	prefixesStack do: [ :prefixes |		prefixes ifNotNil: [			"Check for the special case where we are testing for the			default namespace and that's the only namespace in scope."			(aPrefix size = 0 and: [prefixes size = 0]) ifTrue: [^true].						"Check for matching any in prefixes."			((prefixes findTokens: ' ') includes: aPrefix) ifTrue: [^true].		]	].	^false!

isSpaceInEmptyElements	"answer whether a space is added before the slash in empty elements"		^spaceInEmptyElements!

lineSeparator	"answer the string of characters used for line separators"	^lineSeparator!

lineSeparator: aLineSeparator	"set the line separator characters to be used"		outputStarted ifTrue: [		IllegalStateException signal: 'can''t change line separator characters after output has started'	].		(WAX macLineSeparator = aLineSeparator) |	(WAX unixLineSeparator = aLineSeparator) |	(WAX windowsLineSeparator = aLineSeparator) ifFalse: [		IllegalArgumentException signal: 'invalid line separator characters'	].		lineSeparator := aLineSeparator!

noIndentsOrLineSeparators	"don't indent output or write line separators"	indent := nil!

pi: target data: data	"alias for processingInstruction:data:"	self processingInstruction: target data: data!

prefix: aPrefix attr: aName value: aValue 	"add an attribute to the current element"		^ self prefix: aPrefix attr: aName value: aValue newline: attrOnNewLine!

prefix: aPrefix attr: aName value: aValue newline: aBoolean 
	"add an attribute to the current element"

	| hasPrefix qName value |
	state = #InStartTag ifFalse: [self badState: 'attr'].
	hasPrefix := aPrefix ~= nil and: [aPrefix size > 0].
	checkMe 
		ifTrue: 
			[hasPrefix 
				ifTrue: 
					[XMLUtil verifyName: aPrefix.
					pendingPrefixes add: aPrefix].
			XMLUtil verifyName: aName].
	qName := hasPrefix ifFalse: [aName] ifTrue: [aPrefix , ':' , aName]  .
	aBoolean ifTrue: [self writeIndent] ifFalse: [self write: ' '].
	value := escape ifTrue: [XMLUtil escape: aValue] ifFalse: [aValue].
	self write: qName , '="' , value , '"'!

prefix: aPrefix child: anElementName text: aString	"a convenience method that is a shortcut for start: anElementName; text: aString; end"		state = #AfterRoot ifTrue: [ self badState: 'child' ].		self prefix: aPrefix start: anElementName; text: aString; end		!

prefix: aPrefix commentedStart: anElementName 	"write a commented start tag"		inCommentedStart := true.	self prefix: aPrefix start: anElementName.	inCommentedStart := false!

prefix: aPrefix namespace: aURI	"write a namespace declaration for a namespace that doesn't have an XML Schema"		^self prefix: aPrefix namespace: aURI schemaPath: nil.!

prefix: aPrefix namespace: aURI schemaPath: aPath	"write a namespace declaration for a namespace that has an XML Schema"		| hasPrefix prefix prefixesOnCurrentElement |	state = #InStartTag ifFalse: [ self badState: 'namespace' ].		prefix := aPrefix ifNil: [ ' ' ] ifNotNil: [ aPrefix ].	hasPrefix := prefix notEmpty.	prefixesOnCurrentElement := prefixesStack removeFirst.		checkMe ifTrue: [		hasPrefix ifTrue: [ XMLUtil verifyName: prefix ].		XMLUtil verifyURI: aURI.		aPath ifNotNil: [ XMLUtil verifyURI: aPath ].				"Verify that the prefix isn't already defined on the current element."		hasPrefix			ifTrue: [				prefixesOnCurrentElement ifNotNil: [					(prefixesOnCurrentElement findTokens: ' ') do: [:definedPrefix |						prefix = definedPrefix ifTrue: [							IllegalArgumentException signal: 'The namespace prefix "', prefix,								'" is already defined on the current element'						]					]				]			]			ifFalse: [				defaultNSOnCurrentElement ifTrue: [					IllegalArgumentException signal:						'The default namespace is already defined on the current element.'				]			]	].	self willIndent ifTrue: [ self writeIndent ] ifFalse: [ self write: ' ' ].		self write: 'xmlns'.	hasPrefix ifTrue: [ self write: ':', prefix ].	self write: '="', aURI, '"'.		aPath ifNotNil: [		aPath ifNotEmpty: [ namespaceURIToSchemaPathMap at: aURI put: aPath ]	].		hasPrefix		ifTrue: [			prefixesOnCurrentElement				ifNil: [ prefixesOnCurrentElement := prefix ]				ifNotNil: [ prefixesOnCurrentElement := prefixesOnCurrentElement, ',', prefix ].		]		ifFalse: [ defaultNSOnCurrentElement := true ].			"Note that the entry for the current element was popped off	near the beginning of this method, so we're pushing it back on."	prefixesStack addFirst: prefixesOnCurrentElement.		attrOnNewLine := true. "for the next attribute"!

prefix: aPrefix ns: aURI	"write a namespace declaration for a namespace that doesn't have an XML Schema"		^self prefix: aPrefix namespace: aURI.!

prefix: aPrefix ns: aURI schemaPath: aPath	"write a namespace declaration for a namespace that has an XML Schema"		self prefix: aPrefix namespace: aURI schemaPath: aPath!

prefix: aPrefix start: anElementName 	"start a new element"		| hasPrefix qName |		hasContent := hasIndentedContent := true.	self terminateStart.	hasContent := hasIndentedContent := false.		state = #AfterRoot ifTrue: [ self badState: 'start' ].		hasPrefix := aPrefix isEmptyOrNil not.		checkMe ifTrue: [		hasPrefix ifTrue: [			XMLUtil verifyName: aPrefix.			pendingPrefixes add: aPrefix		].		XMLUtil verifyName: anElementName	].		"If this is the root element ..."	state = #InProlog ifTrue: [ self writeDocType: anElementName ].		"Can't add to pendingPrefixes until previous start tag has been terminated."	checkMe & hasPrefix ifTrue: [ pendingPrefixes add: aPrefix ].		parentStack isEmpty ifFalse: [ self writeIndent ].		qName := hasPrefix ifTrue: [ aPrefix, ':', anElementName ] ifFalse: [ anElementName ].		inCommentedStart		ifTrue: [			self write: '<!!--', qName.			parentStack addFirst: '-', qName		]		ifFalse: [			self write: '<', qName.			parentStack addFirst: qName		].		defaultNSOnCurrentElement := false.		"No namespace prefixes have been associated with this element yet."	prefixesStack addFirst: nil.		state := #InStartTag.!

prefix: aPrefix unescapedAttr: aName value: aValue	"same as the attr method, but special characters in the value	aren't escaped.  This allows entity references to be embedded."		self prefix: '' unescapedAttr: aName value: aValue newline: false!

prefix: aPrefix unescapedAttr: aName value: aValue newline: aBoolean	"same as the attr method, but special characters in the value	aren't escaped.  This allows entity references to be embedded."		escape := false.	self prefix: aPrefix attr: aName value: aValue newline: aBoolean.	escape := true.!

processingInstruction: target data: data	"write a processing instruction"	checkMe ifTrue: [		"Processing instructions can go anywhere		except inside element start tags and attribute values."		"Provide special handling for the 'xml-stylesheet' processing instruction		since starting with 'xml' is reserved."		target = 'xml-stylesheet' ifFalse: [ XMLUtil verifyName: target ].	].	hasContent := hasIndentedContent := true.	self terminateStart.	parentStack ifNotEmptyDo: [ :stack | self writeIndent ].		self write: '<?', target, ' ', data, '?>'.	(self willIndent and: [parentStack isEmpty]) ifTrue: [ self write: lineSeparator]!

spaceInEmptyElements: aBoolean	"sets whether a space is added before the closing slash in empty elements"		spaceInEmptyElements := aBoolean!

start: anElementName 	"start a new element"		self prefix: '' start: anElementName!

stream	"answer the stream used for writing"	^stream!

stream: aStream	"set the stream used for writing"	stream := aStream!

terminateStart	"Private - close the start tag, with > or />, that had been kept open waiting for more namespace declarations and attributes"	checkMe ifTrue: [ self verifyPrefixes ].	state = #InStartTag ifFalse: [^self].	self writeSchemaLocations.	self write: '>'.	attrOnNewLine := false. "reset"	state := #InElement!

text: aString 	"add text to an element"		self text: aString newline: false!

text: aString newline: newline	"add text to an element"		(state = #InProlog) | (state = #AfterRoot) ifTrue: [ self badState: 'text' ].		hasContent := true.	hasIndentedContent := newline.	self terminateStart.		aString		ifEmpty: [ self write: lineSeparator ]		ifNotEmpty: [			| text |			newline ifTrue: [ self writeIndent ].			text := escape ifTrue: [XMLUtil escape: aString] ifFalse: [aString].			self write: text		]!

trustMe	"get 'trust me' mode setting"		^checkMe not!

trustMe: aBoolean	"set 'trust me' mode"		checkMe := aBoolean not!

unescapedAttr: aName value: aValue	"same as the attr method, but special characters in the value	aren't escaped.  This allows entity references to be embedded."		self prefix: '' unescapedAttr: aName value: aValue!

unescapedText: text	"same as the text method, but special characters in the value aren't escaped.	This allows entity references to be embedded."		self unescapedText: text newline: false!

unescapedText: text newline: aBoolean	"same as the text method, but special characters in the value aren't escaped.	This allows entity references to be embedded."		escape := false.	self text: text newline: aBoolean.	escape := true !

verifyPrefixes	"Private - verify that all the pending namespace prefix are currently in scope"	pendingPrefixes do: [:prefix |		(self isInScopePrefix: prefix) ifFalse: [			IllegalStateException signal: 'The namespace prefix "', prefix, '" isn''t in scope.'		]	].	"Note there is no method in Smalltalk for removing all elements from a collection.	This is normally handled by just creating a new collection."	pendingPrefixes := OrderedCollection new.!

willIndent	"Private - answer whether XML should be indented"	^indent notNil!

write: aString 	"Private - write aString to the stream"		stream ifNil: 		[ IllegalStateException signal: 'attempting to write XML after close has been called' ].		aString ifNotNil: [		stream nextPutAll: aString.		outputStarted := true	]!

writeDocType: rootElementName 	"Private - write a DOCTYPE"		(doctypeSystemId isNil and: [ entityDefs isEmpty ]) ifTrue: [ ^ self ].	self write: '<!!DOCTYPE ' , rootElementName.	doctypePublicId 		ifNotNil: 			[ self write: ' PUBLIC "' , doctypePublicId , '" "' , doctypeSystemId , '"' ]		ifNil: 			[ doctypeSystemId ifNotNil: [ self write: ' SYSTEM "' , doctypeSystemId , '"' ] ].	entityDefs ifNotEmpty: [		self write: ' ['.		entityDefs do: [ :entityDef | 			self willIndent ifTrue: [ self write: lineSeparator, indent ].			self write: '<!!ENTITY ' , entityDef , '>'		].		self willIndent ifTrue: [ self write: lineSeparator ].		self write: ']'.		entityDefs := OrderedCollection new	].	self write: '>'.	self willIndent ifTrue: [ self write: lineSeparator ]!

writeIndent	"Private - write the proper amount of indentation given the current nesting of elements"		self willIndent ifFalse: [ ^ self ].	self write: lineSeparator.	(1 to: parentStack size) do: [:i | self write: indent ]!

writeSchemaLocations
	"Private - write the namespace declaration for the XMLSchema-instance namespace	and writes the schemaLocation attribute	which associates namespace URIs with schema locations."

	| nsStream |
	
	namespaceURIToSchemaPathMap ifEmpty: [^self].

	"Write the attributes needed to associate XML Schemas with this XML."
	nsStream := WriteStream on: String new.
	namespaceURIToSchemaPathMap keysAndValuesDo: 
			[:uri :path | 
			"If not the first pair output ..."
			nsStream isEmpty 
				ifFalse: 
					[self willIndent 
						ifTrue: 
							[nsStream nextPutAll: lineSeparator.
							parentStack size + 1 timesRepeat: [nsStream nextPutAll: indent]]
						ifFalse: [nsStream nextPut: Character space]].
			nsStream nextPutAll: uri , ' ' , path].
	self
		prefix: 'xsi' namespace: XMLUtil xmlSchemaInstanceNS;
		prefix: 'xsi'
			attr: 'schemaLocation'
			value: nsStream contents
			newline: self willIndent.
	attrOnNewLine := true.	"for the next attribute"
	namespaceURIToSchemaPathMap := Dictionary new!

writeXMLDeclaration: aVersion 	"Private - write an XML declaration"		"Don't write an XML declaration if no version is specified."	aVersion ifNil: [ ^ self ].		XMLUtil verifyVersion: aVersion.	self write:		'<?xml version="' , aVersion asString , '" encoding="' , encoding , '"?>' , lineSeparator!

xslt: filePath	"write an 'xml-stylesheet' processing instruction"	xsltSpecified ifTrue: [ IllegalStateException signal: 'can''t specify more than one XSLT' ].	state = #InProlog ifFalse: [ self badState: 'xslt' ].	checkMe ifTrue: [ XMLUtil verifyURI: filePath ].		xsltSpecified := true.	self processingInstruction: 'xml-stylesheet' data: 'type="text/xsl" href="', filePath, '"'! !
!WAX categoriesFor: #allSpaces:!private! !
!WAX categoriesFor: #attr:value:!public!writing! !
!WAX categoriesFor: #badState:!error/handling!public! !
!WAX categoriesFor: #blankLine!public!writing! !
!WAX categoriesFor: #cdata:!public!writing! !
!WAX categoriesFor: #cdata:newline:!public!writing! !
!WAX categoriesFor: #child:text:!public!writing! !
!WAX categoriesFor: #close!public!writing! !
!WAX categoriesFor: #comment:!public!writing! !
!WAX categoriesFor: #comment:newline:!public!writing! !
!WAX categoriesFor: #commentedStart:!public!writing! !
!WAX categoriesFor: #defaultNamespace:!public!writing! !
!WAX categoriesFor: #defaultNamespace:schemaPath:!public!writing! !
!WAX categoriesFor: #dtd:!public!writing! !
!WAX categoriesFor: #dtd:systemId:!public!writing! !
!WAX categoriesFor: #end!public!writing! !
!WAX categoriesFor: #end:!public!writing! !
!WAX categoriesFor: #entityDef:value:!public!writing! !
!WAX categoriesFor: #externalEntityDef:filePath:!public!writing! !
!WAX categoriesFor: #indent!configuring!public! !
!WAX categoriesFor: #indentChars:!configuring!public! !
!WAX categoriesFor: #indentSize:!configuring!public! !
!WAX categoriesFor: #initialize!initialize/release!public! !
!WAX categoriesFor: #isInScopePrefix:!private! !
!WAX categoriesFor: #isSpaceInEmptyElements!private! !
!WAX categoriesFor: #lineSeparator!accessing!public! !
!WAX categoriesFor: #lineSeparator:!configuring!public! !
!WAX categoriesFor: #noIndentsOrLineSeparators!configuring!public! !
!WAX categoriesFor: #pi:data:!public!writing! !
!WAX categoriesFor: #prefix:attr:value:!public!writing! !
!WAX categoriesFor: #prefix:attr:value:newline:!public!writing! !
!WAX categoriesFor: #prefix:child:text:!public!writing! !
!WAX categoriesFor: #prefix:commentedStart:!public!writing! !
!WAX categoriesFor: #prefix:namespace:!public!writing! !
!WAX categoriesFor: #prefix:namespace:schemaPath:!public!writing! !
!WAX categoriesFor: #prefix:ns:!public!writing! !
!WAX categoriesFor: #prefix:ns:schemaPath:!public!writing! !
!WAX categoriesFor: #prefix:start:!public!writing! !
!WAX categoriesFor: #prefix:unescapedAttr:value:!public!writing! !
!WAX categoriesFor: #prefix:unescapedAttr:value:newline:!public!writing! !
!WAX categoriesFor: #processingInstruction:data:!public!writing! !
!WAX categoriesFor: #spaceInEmptyElements:!private! !
!WAX categoriesFor: #start:!public!writing! !
!WAX categoriesFor: #stream!accessing!public! !
!WAX categoriesFor: #stream:!accessing!public! !
!WAX categoriesFor: #terminateStart!private! !
!WAX categoriesFor: #text:!public!writing! !
!WAX categoriesFor: #text:newline:!public!writing! !
!WAX categoriesFor: #trustMe!configuring!public! !
!WAX categoriesFor: #trustMe:!configuring!public! !
!WAX categoriesFor: #unescapedAttr:value:!public!writing! !
!WAX categoriesFor: #unescapedText:!public!writing! !
!WAX categoriesFor: #unescapedText:newline:!public!writing! !
!WAX categoriesFor: #verifyPrefixes!private! !
!WAX categoriesFor: #willIndent!private! !
!WAX categoriesFor: #write:!private! !
!WAX categoriesFor: #writeDocType:!private! !
!WAX categoriesFor: #writeIndent!private! !
!WAX categoriesFor: #writeSchemaLocations!private! !
!WAX categoriesFor: #writeXMLDeclaration:!private! !
!WAX categoriesFor: #xslt:!public!writing! !

!WAX class methodsFor!

macLineSeparator
	^String with: Character nl!

new
^super new initialize!

stream: aStream	"creates a WAX object using a given stream"	^self stream: aStream version: nil!

stream: aStream version: aSymbol	"creates a WAX object using a given stream and version"	| wax |		wax := WAX new.	wax stream: aStream.	wax writeXMLDeclaration: aSymbol.	^wax!

unixLineSeparator
^String with: Character nl!

windowsLineSeparator
	^String with: Character cr with: Character nl! !
!WAX class categoriesFor: #macLineSeparator!accessing!public! !
!WAX class categoriesFor: #new!public! !
!WAX class categoriesFor: #stream:!instance creation!public! !
!WAX class categoriesFor: #stream:version:!instance creation!public! !
!WAX class categoriesFor: #unixLineSeparator!accessing!public! !
!WAX class categoriesFor: #windowsLineSeparator!accessing!public! !

XMLUtil guid: (GUID fromString: '{C234F5E7-C472-42BA-B139-1FA529CCAFB6}')!
XMLUtil comment: 'This class provides utility methods for working with XML.'!
!XMLUtil categoriesForClass!Unclassified! !
!XMLUtil class methodsFor!

defaultEncoding	"answer the default character encoding"		^ 'UTF-8'!

escape: aString 	"answer a version of aString where special characters in XML text are escaped"		| result |	result := aString.		"The next line was suggested by Bert Freudenberg.	It is essential that ampersands are processed first	so other entity references don't have their & escaped."	#($& '&amp;' $< '&lt;' $> '&gt;' $' '&apos;' $" '&quot;' )		pairsDo: [:char :entity | result := result copyReplaceAll: (char asString) with: entity].			^result!

flag: aSymbol 
	!

isComment: aString 	"answer whether aString is a valid XML comment"		(aString = nil) ifTrue: [ IllegalArgumentException signal: '"nil" is an invalid comment' ].	^(aString includesSubString: '--') not!

isName: aString 	"answer whether aString is a valid XML name"		aString ifNil: [^ false].	(aString asLowercase beginsWith: 'xml') ifTrue: [^ false].	^aString matchesRegex: self latinNameRegex!

isURI: aString 
	"answer whether aString is a URI"

	| alpha authority digit fragment hierPart pathAbempty query scheme uri |
	self flag: #todo.	"This isn't working yet."
	"^URI fromString: aString"

	"See http://gbiv.com/protocols/uri/rfc/rfc3986.html#generic-syntax."
	alpha := ''.
	authority := ''.
	digit := ''.
	fragment := ''.
	pathAbempty := ''.
	hierPart := '//' , authority , pathAbempty.
	query := ''.
	scheme := alpha , '(' , alpha , '|' , digit , '|\+|-|\.)*'.
	uri := scheme , ':' , hierPart , '(' , query , ')?(#' , fragment , ')?'.
	^true!

isVersion: aSymbol	"answer whether aString is a valid XML version"	^#(#'1.0' #'1.1' #'1.2') includes: aSymbol!

latinNameRegex	"answer the regular expression for latin XML names"	"Why can't '\.' be used in place of '.' in the regex below!!"		^'^[A-Za-z:=][A-Za-z0-9\-:=.]*$'!

verifyComment: aString	"verify that aString is a valid XML comment and signals if not"	(XMLUtil isComment: aString) ifFalse: [		IllegalArgumentException signal: '"', aString, ' is an invalid XML comment'	]!

verifyName: aString	"verify that aString is a valid XML comment and signals if not"	(XMLUtil isName: aString) ifFalse: [		IllegalArgumentException signal: '"', aString, '" is an invalid XML name'	]!

verifyURI: aString	"verify that aString is a valid URI and signals if not"	(XMLUtil isURI: aString) ifFalse: [		IllegalArgumentException signal: '"', aString, ' is an invalid URI'	]!

verifyVersion: aSymbol	"verify that aSymbol is a valid XML version and signals if not"	(XMLUtil isVersion: aSymbol) ifFalse: [		IllegalArgumentException signal: '"', aSymbol asString, ' is an invalid XML version'	]!

xmlSchemaInstanceNS	"answer the namespace URI for XML Schema instances"	^'http://www.w3.org/1999/XMLSchema-instance'! !
!XMLUtil class categoriesFor: #defaultEncoding!constants!public! !
!XMLUtil class categoriesFor: #escape:!public!transforming! !
!XMLUtil class categoriesFor: #flag:!public! !
!XMLUtil class categoriesFor: #isComment:!public!testing! !
!XMLUtil class categoriesFor: #isName:!public!testing! !
!XMLUtil class categoriesFor: #isURI:!public!testing! !
!XMLUtil class categoriesFor: #isVersion:!public!testing! !
!XMLUtil class categoriesFor: #latinNameRegex!private! !
!XMLUtil class categoriesFor: #verifyComment:!public!testing! !
!XMLUtil class categoriesFor: #verifyName:!public!testing! !
!XMLUtil class categoriesFor: #verifyURI:!public!testing! !
!XMLUtil class categoriesFor: #verifyVersion:!public!testing! !
!XMLUtil class categoriesFor: #xmlSchemaInstanceNS!constants!public! !

IllegalArgumentException guid: (GUID fromString: '{CDB9D3D8-978A-412B-A35E-CD2ABE742F53}')!
IllegalArgumentException comment: ''!
!IllegalArgumentException categoriesForClass!Unclassified! !
IllegalStateException guid: (GUID fromString: '{114B33CF-2B87-46C8-BAF2-432058B1A47E}')!
IllegalStateException comment: ''!
!IllegalStateException categoriesForClass!Unclassified! !
"Binary Globals"!

