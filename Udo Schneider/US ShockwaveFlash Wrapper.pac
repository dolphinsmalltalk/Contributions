| package |
package := Package name: 'US ShockwaveFlash Wrapper'.
package paxVersion: 1;
	basicComment: '$id: US ShockwaveFlash Wrapper 0.017$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.017'.


package classNames
	add: #ActionScriptBridge;
	add: #ActionScriptInvokationParser;
	add: #IShockwaveFlash;
	add: #ShockwaveFlashView;
	yourself.

package methodNames
	add: #Dictionary -> #asASXML;
	add: #False -> #asASXML;
	add: #Number -> #asASXML;
	add: #Object -> #asASXML;
	add: #SequenceableCollection -> #asASXML;
	add: #String -> #asASXML;
	add: #True -> #asASXML;
	add: #UndefinedObject -> #asASXML;
	yourself.

package globalNames
	add: #ShockwaveFlashObjectsLib;
	yourself.

package binaryGlobalNames: (Set new
	add: #ShockwaveFlashObjectsLib;
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\ActiveX\Automation\ActiveX Automation';
	add: '..\..\Object Arts\Dolphin\ActiveX\Connection Points\ActiveX Connection Points';
	add: '..\..\Object Arts\Dolphin\ActiveX\OCX\ActiveX Control Hosting';
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	add: '..\..\Object Arts\Dolphin\ActiveX\Components\XML DOM\XML DOM';
	yourself).

package!

"Class Definitions"!

Object subclass: #ActionScriptBridge
	instanceVariableNames: 'view isTracingFlashEvents'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ActionScriptInvokationParser
	instanceVariableNames: 'document'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IDispatch subclass: #IShockwaveFlash
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AXValueConvertingControlSite subclass: #ShockwaveFlashView
	instanceVariableNames: 'bridgeClass bridge'
	classVariableNames: ''
	poolDictionaries: 'CRTConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Dictionary methodsFor!

asASXML
	| stream |
	stream := ReadWriteStream on: String new.
	stream nextPutAll: '<object>'.
	self keysAndValuesDo: 
			[:eachKey :eachValue | 
			stream
				nextPutAll: '<property id="';
				nextPutAll: eachKey displayString;
				nextPutAll: '">';
				nextPutAll: eachValue asASXML;
				nextPutAll: '</property>'].
	stream nextPutAll: '</object>'.
	^stream contents! !
!Dictionary categoriesFor: #asASXML!public! !

!False methodsFor!

asASXML
	^'<false/>'! !
!False categoriesFor: #asASXML!public! !

!Number methodsFor!

asASXML
	^'<number>'  , self displayString , '</number>'! !
!Number categoriesFor: #asASXML!public! !

!Object methodsFor!

asASXML
^'<object/>'! !
!Object categoriesFor: #asASXML!public! !

!SequenceableCollection methodsFor!

asASXML
	| stream |
	stream := ReadWriteStream on: String new.
	stream nextPutAll: '<array>'.
	self keysAndValuesDo: 
			[:eachKey :eachValue | 
			stream
				nextPutAll: '<property id="';
				nextPutAll: eachKey displayString;
				nextPutAll: '">'; nextPutAll: eachValue asASXML;
			nextPutAll: '</property>'].
	stream nextPutAll: '</array>'.
	^stream contents! !
!SequenceableCollection categoriesFor: #asASXML!public! !

!String methodsFor!

asASXML
^'<string>' , self , '</string>'! !
!String categoriesFor: #asASXML!public! !

!True methodsFor!

asASXML
^'<true/>'! !
!True categoriesFor: #asASXML!public! !

!UndefinedObject methodsFor!

asASXML
^'<null/>'! !
!UndefinedObject categoriesFor: #asASXML!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ActionScriptBridge guid: (GUID fromString: '{05AC057D-1DF6-4948-B2B0-8B0C64767326}')!
ActionScriptBridge comment: ''!
!ActionScriptBridge categoriesForClass!MVP-Views! !
!ActionScriptBridge methodsFor!

doesNotUnderstand: failedMessage 
	| functionName arguments |
	functionName := (failedMessage selector subStrings: $:) first.
	arguments := failedMessage arguments.
	^[self invokeFlash: functionName withArguments: arguments] on: MessageNotUnderstood do: [:ex | ^super doesNotUnderstand: failedMessage] !

flashEventDispatch: function arguments: arguments 
	self isTracingFlashEvents 
		ifTrue: 
			[Transcript
				show: ' (Dispatching as ' 
							, (self selectorOrNilMatching: function argumentCount: arguments size) printString , ')';
				cr].
	^self perform: (self selectorOrNilMatching: function argumentCount: arguments size)
		withArguments: arguments!

flashEventTrigger: function arguments: arguments 
	| returnValue |
	self isTracingFlashEvents 
		ifTrue: 
			[Transcript
				show: ' (Triggering as ' , function printString , ')';
				cr].
	returnValue := nil asValue.
	(self )
		trigger: function withArguments: arguments;
		trigger: function
			with: arguments
			with: returnValue.
	^returnValue value!

initialize
	super initialize.
	isTracingFlashEvents := false!

invokeFlash: function 
	^self invokeFlash: function withArguments: #()!

invokeFlash: function with: arg1 
	^self invokeFlash: function withArguments: (Array with: arg1)!

invokeFlash: function with: arg1  with: arg2
	^self invokeFlash: function withArguments: (Array with: arg1 with: arg2)!

invokeFlash: function with: arg1 with: arg2  with: arg3
	^self invokeFlash: function withArguments: (Array with: arg1 with: arg2 with: arg3)!

invokeFlash: function with: arg1 with: arg2 with: arg3  with: arg4
	^self invokeFlash: function
		withArguments: (Array 
				with: arg1
				with: arg2
				with: arg3 with: arg4)!

invokeFlash: function withArguments: arguments 
	| invokeStream return |
	invokeStream := ReadWriteStream on: String new.
	invokeStream
		nextPutAll: '<invoke name="';
		nextPutAll: function;
		nextPutAll: '" returntype="xml"><arguments>'.
	arguments do: [:each | invokeStream nextPutAll: each asASXML].
	invokeStream nextPutAll: '</arguments></invoke>'.
	view maskFpeWhile: [
	return := view controlDispatch callFunction: invokeStream contents].
	^(ActionScriptInvokationParser parse: return) first!

isTracingFlashEvents
	"Answer whether the receiver is currently tracing incoming events from the flash movie.
	This is a debugging mode and should be turned off in a runtime system."

	^isTracingFlashEvents!

isTracingFlashEvents: aBoolean 
	"Answer whether the receiver is currently tracing incoming events from the flash movie.
	This is a debugging mode and should be turned off in a runtime system."

	isTracingFlashEvents := aBoolean!

onFlashCall: aFlashCall 
	| invoke function arguments return |
	invoke := (ActionScriptInvokationParser parse: aFlashCall) first.
	function := invoke key asSymbol.
	arguments := invoke value.
	self isTracingFlashEvents 
		ifTrue: 
			[Transcript show: ((String writeStream: 80)
						nextPutAll: 'Flash Event/Callback: ';
						print: function;
						space;
						print: arguments;
						contents)].
	return := (self selectorOrNilMatching: function argumentCount: arguments size) 
				ifNotNil: [:selector | self flashEventDispatch: function arguments: arguments]
				ifNil: [self flashEventTrigger: function arguments: arguments].
	view controlDispatch setReturnValue: return asASXML!

selectorOrNilMatching: aSymbol argumentCount: anInteger 
	^self class allSelectors 
		detect: [:each | (each beginsWith: aSymbol) and: [each argumentCount = anInteger]]
		ifNone: [nil]!

setView: aShockwaveFlashView 

	view := aShockwaveFlashView.
	aShockwaveFlashView 
		when: #FlashCall:
		send: #onFlashCall:
		to: self!

view
^view! !
!ActionScriptBridge categoriesFor: #doesNotUnderstand:!exceptions!ExternalInterface-Invocation!message dispatching!public!vm entry points! !
!ActionScriptBridge categoriesFor: #flashEventDispatch:arguments:!event handling!ExternalInterface-Callbacks!helpers!private! !
!ActionScriptBridge categoriesFor: #flashEventTrigger:arguments:!event handling!ExternalInterface-Callbacks!helpers!private! !
!ActionScriptBridge categoriesFor: #initialize!initialize/release!private! !
!ActionScriptBridge categoriesFor: #invokeFlash:!ExternalInterface-Invocation!public! !
!ActionScriptBridge categoriesFor: #invokeFlash:with:!ExternalInterface-Invocation!public! !
!ActionScriptBridge categoriesFor: #invokeFlash:with:with:!ExternalInterface-Invocation!public! !
!ActionScriptBridge categoriesFor: #invokeFlash:with:with:with:!ExternalInterface-Invocation!public! !
!ActionScriptBridge categoriesFor: #invokeFlash:with:with:with:with:!ExternalInterface-Invocation!public! !
!ActionScriptBridge categoriesFor: #invokeFlash:withArguments:!ExternalInterface-Invocation!public! !
!ActionScriptBridge categoriesFor: #isTracingFlashEvents!accessing!public!testing! !
!ActionScriptBridge categoriesFor: #isTracingFlashEvents:!accessing!public! !
!ActionScriptBridge categoriesFor: #onFlashCall:!event handling!ExternalInterface-Callbacks!public! !
!ActionScriptBridge categoriesFor: #selectorOrNilMatching:argumentCount:!helpers!private! !
!ActionScriptBridge categoriesFor: #setView:!accessing!private! !
!ActionScriptBridge categoriesFor: #view!accessing!public! !

!ActionScriptBridge class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\actionscript.ico'!

new
^super new initialize!

onView: aShockwaveFlashView 
	^(self new)
		setView: aShockwaveFlashView;
		yourself! !
!ActionScriptBridge class categoriesFor: #icon!development!public! !
!ActionScriptBridge class categoriesFor: #new!instance creation!public! !
!ActionScriptBridge class categoriesFor: #onView:!instance creation!public! !

ActionScriptInvokationParser guid: (GUID fromString: '{13C43DCB-2D47-4287-8AD4-8B5E9020E627}')!
ActionScriptInvokationParser comment: ''!
!ActionScriptInvokationParser categoriesForClass!Kernel-Objects! !
!ActionScriptInvokationParser methodsFor!

parse
^
document childNodes collect: [:each | self parseNode: each ]!

parseArray: anIXMLDOMElement 
	| result |
	result := anIXMLDOMElement childNodes collect: [:each | self parseNode: each].
	^result collect: [:each | each value]!

parseInvoke: anIXMLDOMElement 
	^(anIXMLDOMElement getAttribute: 'name') 
		-> (anIXMLDOMElement childNodes first childNodes collect: [:each | self parseNode: each])!

parseNode: anIXMLDOMElement 
	| tagName |
	tagName := anIXMLDOMElement tagName.
	tagName = 'null' ifTrue: [^nil].
	tagName = 'undefined' ifTrue: [^nil].
	tagName = 'true' ifTrue: [^true].
	tagName = 'false' ifTrue: [^false].
	tagName = 'number' ifTrue: [^self parseNumber: anIXMLDOMElement].
	tagName = 'string' ifTrue: [^self parseString: anIXMLDOMElement].
	tagName = 'property' ifTrue: [^self parseProperty: anIXMLDOMElement].
	tagName = 'array' ifTrue: [^self parseArray: anIXMLDOMElement].
	tagName = 'object' ifTrue: [^self parseObject: anIXMLDOMElement].
	tagName = 'invoke' ifTrue: [^self parseInvoke: anIXMLDOMElement].
	Error notYetImplemented!

parseNumber: anIXMLDOMElement 
	^Number fromString: anIXMLDOMElement text!

parseObject: anIXMLDOMElement 
	| result dic |
	result := anIXMLDOMElement childNodes collect: [:each | self parseNode: each].
	dic := Dictionary new.
	result do: [:each | dic at: each key put: each value].
	^dic!

parseProperty: anIXMLDOMElement 
	
	^(anIXMLDOMElement getAttribute: 'id') -> (self parseNode: anIXMLDOMElement childNodes first)!

parseString: anIXMLDOMElement 
	^ anIXMLDOMElement text!

setDocument: anIXMLDOMDocument 
document := anIXMLDOMDocument! !
!ActionScriptInvokationParser categoriesFor: #parse!public! !
!ActionScriptInvokationParser categoriesFor: #parseArray:!private! !
!ActionScriptInvokationParser categoriesFor: #parseInvoke:!private! !
!ActionScriptInvokationParser categoriesFor: #parseNode:!private! !
!ActionScriptInvokationParser categoriesFor: #parseNumber:!private! !
!ActionScriptInvokationParser categoriesFor: #parseObject:!private! !
!ActionScriptInvokationParser categoriesFor: #parseProperty:!private! !
!ActionScriptInvokationParser categoriesFor: #parseString:!private! !
!ActionScriptInvokationParser categoriesFor: #setDocument:!private! !

!ActionScriptInvokationParser class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\actionscript.ico'!

on: aString 
	^(self new)
		setDocument: ((IXMLDOMDocument new)
					loadXML: aString;
					yourself);
		yourself!

parse: aString 
	^(self on: aString)
		parse! !
!ActionScriptInvokationParser class categoriesFor: #icon!development!public! !
!ActionScriptInvokationParser class categoriesFor: #on:!public! !
!ActionScriptInvokationParser class categoriesFor: #parse:!public! !

IShockwaveFlash guid: (IID fromString: '{D27CDB6C-AE6D-11CF-96B8-444553540000}')!
IShockwaveFlash comment: '<IShockwaveFlash> is a wrapper class for the COM interface ''ShockwaveFlashObjects.IShockwaveFlash'' generated from type information in the ''Shockwave Flash'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Shockwave Flash"

Warning: This comment was automatically generated from the interface''s type information, but any changes made here will not be overwritten if the wrapper class is regenerated.

IDL definition follows:

[
	object, 
	uuid(D27CDB6C-AE6D-11CF-96B8-444553540000), 
	helpstring("Shockwave Flash"), 
	dual
]
interface IShockwaveFlash : IDispatch
 {
	[id(0xfffffdf3), propget, helpstring("property ReadyState")]
	HRESULT __stdcall ReadyState(
		[out, retval]long* pVal);
	[id(0x0000007c), propget, helpstring("property TotalFrames")]
	HRESULT __stdcall TotalFrames(
		[out, retval]long* pVal);
	[id(0x0000007d), propget, helpstring("property Playing")]
	HRESULT __stdcall Playing(
		[out, retval]VARIANT_BOOL* pVal);
	[id(0x0000007d), propput, helpstring("property Playing")]
	HRESULT __stdcall Playing(
		[in]VARIANT_BOOL pVal);
	[id(0x00000069), propget, helpstring("property Quality")]
	HRESULT __stdcall Quality(
		[out, retval]int* pVal);
	[id(0x00000069), propput, helpstring("property Quality")]
	HRESULT __stdcall Quality(
		[in]int pVal);
	[id(0x00000078), propget, helpstring("property ScaleMode")]
	HRESULT __stdcall ScaleMode(
		[out, retval]int* pVal);
	[id(0x00000078), propput, helpstring("property ScaleMode")]
	HRESULT __stdcall ScaleMode(
		[in]int pVal);
	[id(0x00000079), propget, helpstring("property AlignMode")]
	HRESULT __stdcall AlignMode(
		[out, retval]int* pVal);
	[id(0x00000079), propput, helpstring("property AlignMode")]
	HRESULT __stdcall AlignMode(
		[in]int pVal);
	[id(0x0000007b), propget, helpstring("property BackgroundColor")]
	HRESULT __stdcall BackgroundColor(
		[out, retval]long* pVal);
	[id(0x0000007b), propput, helpstring("property BackgroundColor")]
	HRESULT __stdcall BackgroundColor(
		[in]long pVal);
	[id(0x0000006a), propget, helpstring("property Loop")]
	HRESULT __stdcall Loop(
		[out, retval]VARIANT_BOOL* pVal);
	[id(0x0000006a), propput, helpstring("property Loop")]
	HRESULT __stdcall Loop(
		[in]VARIANT_BOOL pVal);
	[id(0x00000066), propget, helpstring("property Movie")]
	HRESULT __stdcall Movie(
		[out, retval]BSTR* pVal);
	[id(0x00000066), propput, helpstring("property Movie")]
	HRESULT __stdcall Movie(
		[in]BSTR pVal);
	[id(0x0000006b), propget, helpstring("property FrameNum")]
	HRESULT __stdcall FrameNum(
		[out, retval]long* pVal);
	[id(0x0000006b), propput, helpstring("property FrameNum")]
	HRESULT __stdcall FrameNum(
		[in]long pVal);
	[id(0x0000006d), helpstring("method SetZoomRect")]
	HRESULT __stdcall SetZoomRect(
		[in]long left,
		[in]long top,
		[in]long right,
		[in]long bottom);
	[id(0x00000076), helpstring("method Zoom")]
	HRESULT __stdcall Zoom(
		[in]int factor);
	[id(0x00000077), helpstring("method Pan")]
	HRESULT __stdcall Pan(
		[in]long x,
		[in]long y,
		[in]int mode);
	[id(0x00000070), helpstring("method Play")]
	HRESULT __stdcall Play();
	[id(0x00000071), helpstring("method Stop")]
	HRESULT __stdcall Stop();
	[id(0x00000072), helpstring("method Back")]
	HRESULT __stdcall Back();
	[id(0x00000073), helpstring("method Forward")]
	HRESULT __stdcall Forward();
	[id(0x00000074), helpstring("method Rewind")]
	HRESULT __stdcall Rewind();
	[id(0x0000007e), helpstring("method StopPlay")]
	HRESULT __stdcall StopPlay();
	[id(0x0000007f), helpstring("method GotoFrame")]
	HRESULT __stdcall GotoFrame(
		[in]long FrameNum);
	[id(0x00000080), helpstring("method CurrentFrame")]
	HRESULT __stdcall CurrentFrame(
		[out, retval]long* FrameNum);
	[id(0x00000081), helpstring("method IsPlaying")]
	HRESULT __stdcall IsPlaying(
		[out, retval]VARIANT_BOOL* Playing);
	[id(0x00000082), helpstring("method PercentLoaded")]
	HRESULT __stdcall PercentLoaded(
		[out, retval]long* percent);
	[id(0x00000083), helpstring("method FrameLoaded")]
	HRESULT __stdcall FrameLoaded(
		[in]long FrameNum,
		[out, retval]VARIANT_BOOL* loaded);
	[id(0x00000084), helpstring("method FlashVersion")]
	HRESULT __stdcall FlashVersion(
		[out, retval]long* version);
	[id(0x00000085), propget, helpstring("property WMode")]
	HRESULT __stdcall WMode(
		[out, retval]BSTR* pVal);
	[id(0x00000085), propput, helpstring("property WMode")]
	HRESULT __stdcall WMode(
		[in]BSTR pVal);
	[id(0x00000086), propget, helpstring("property SAlign")]
	HRESULT __stdcall SAlign(
		[out, retval]BSTR* pVal);
	[id(0x00000086), propput, helpstring("property SAlign")]
	HRESULT __stdcall SAlign(
		[in]BSTR pVal);
	[id(0x00000087), propget, helpstring("property Menu")]
	HRESULT __stdcall Menu(
		[out, retval]VARIANT_BOOL* pVal);
	[id(0x00000087), propput, helpstring("property Menu")]
	HRESULT __stdcall Menu(
		[in]VARIANT_BOOL pVal);
	[id(0x00000088), propget, helpstring("property Base")]
	HRESULT __stdcall Base(
		[out, retval]BSTR* pVal);
	[id(0x00000088), propput, helpstring("property Base")]
	HRESULT __stdcall Base(
		[in]BSTR pVal);
	[id(0x00000089), propget, helpstring("property Scale")]
	HRESULT __stdcall Scale(
		[out, retval]BSTR* pVal);
	[id(0x00000089), propput, helpstring("property Scale")]
	HRESULT __stdcall Scale(
		[in]BSTR pVal);
	[id(0x0000008a), propget, helpstring("property DeviceFont")]
	HRESULT __stdcall DeviceFont(
		[out, retval]VARIANT_BOOL* pVal);
	[id(0x0000008a), propput, helpstring("property DeviceFont")]
	HRESULT __stdcall DeviceFont(
		[in]VARIANT_BOOL pVal);
	[id(0x0000008b), propget, helpstring("property EmbedMovie")]
	HRESULT __stdcall EmbedMovie(
		[out, retval]VARIANT_BOOL* pVal);
	[id(0x0000008b), propput, helpstring("property EmbedMovie")]
	HRESULT __stdcall EmbedMovie(
		[in]VARIANT_BOOL pVal);
	[id(0x0000008c), propget, helpstring("property BGColor")]
	HRESULT __stdcall BGColor(
		[out, retval]BSTR* pVal);
	[id(0x0000008c), propput, helpstring("property BGColor")]
	HRESULT __stdcall BGColor(
		[in]BSTR pVal);
	[id(0x0000008d), propget, helpstring("property Quality2")]
	HRESULT __stdcall Quality2(
		[out, retval]BSTR* pVal);
	[id(0x0000008d), propput, helpstring("property Quality2")]
	HRESULT __stdcall Quality2(
		[in]BSTR pVal);
	[id(0x0000008e), helpstring("method LoadMovie")]
	HRESULT __stdcall LoadMovie(
		[in]int layer,
		[in]BSTR url);
	[id(0x0000008f), helpstring("method TGotoFrame")]
	HRESULT __stdcall TGotoFrame(
		[in]BSTR target,
		[in]long FrameNum);
	[id(0x00000090), helpstring("method TGotoLabel")]
	HRESULT __stdcall TGotoLabel(
		[in]BSTR target,
		[in]BSTR label);
	[id(0x00000091), helpstring("method TCurrentFrame")]
	HRESULT __stdcall TCurrentFrame(
		[in]BSTR target,
		[out, retval]long* FrameNum);
	[id(0x00000092), helpstring("method TCurrentLabel")]
	HRESULT __stdcall TCurrentLabel(
		[in]BSTR target,
		[out, retval]BSTR* pVal);
	[id(0x00000093), helpstring("method TPlay")]
	HRESULT __stdcall TPlay(
		[in]BSTR target);
	[id(0x00000094), helpstring("method TStopPlay")]
	HRESULT __stdcall TStopPlay(
		[in]BSTR target);
	[id(0x00000097), helpstring("method SetVariable")]
	HRESULT __stdcall SetVariable(
		[in]BSTR name,
		[in]BSTR value);
	[id(0x00000098), helpstring("method GetVariable")]
	HRESULT __stdcall GetVariable(
		[in]BSTR name,
		[out, retval]BSTR* pVal);
	[id(0x00000099), helpstring("method TSetProperty")]
	HRESULT __stdcall TSetProperty(
		[in]BSTR target,
		[in]int property,
		[in]BSTR value);
	[id(0x0000009a), helpstring("method TGetProperty")]
	HRESULT __stdcall TGetProperty(
		[in]BSTR target,
		[in]int property,
		[out, retval]BSTR* pVal);
	[id(0x0000009b), helpstring("method TCallFrame")]
	HRESULT __stdcall TCallFrame(
		[in]BSTR target,
		[in]int FrameNum);
	[id(0x0000009c), helpstring("method TCallLabel")]
	HRESULT __stdcall TCallLabel(
		[in]BSTR target,
		[in]BSTR label);
	[id(0x0000009d), helpstring("method TSetPropertyNum")]
	HRESULT __stdcall TSetPropertyNum(
		[in]BSTR target,
		[in]int property,
		[in]double value);
	[id(0x0000009e), helpstring("method TGetPropertyNum")]
	HRESULT __stdcall TGetPropertyNum(
		[in]BSTR target,
		[in]int property,
		[out, retval]double* pVal);
	[id(0x000000ac), helpstring("method TGetPropertyAsNumber")]
	HRESULT __stdcall TGetPropertyAsNumber(
		[in]BSTR target,
		[in]int property,
		[out, retval]double* pVal);
	[id(0x0000009f), propget, helpstring("property SWRemote")]
	HRESULT __stdcall SWRemote(
		[out, retval]BSTR* pVal);
	[id(0x0000009f), propput, helpstring("property SWRemote")]
	HRESULT __stdcall SWRemote(
		[in]BSTR pVal);
	[id(0x000000aa), propget, helpstring("property FlashVars")]
	HRESULT __stdcall FlashVars(
		[out, retval]BSTR* pVal);
	[id(0x000000aa), propput, helpstring("property FlashVars")]
	HRESULT __stdcall FlashVars(
		[in]BSTR pVal);
	[id(0x000000ab), propget, helpstring("property AllowScriptAccess")]
	HRESULT __stdcall AllowScriptAccess(
		[out, retval]BSTR* pVal);
	[id(0x000000ab), propput, helpstring("property AllowScriptAccess")]
	HRESULT __stdcall AllowScriptAccess(
		[in]BSTR pVal);
	[id(0x000000be), propget, helpstring("property MovieData")]
	HRESULT __stdcall MovieData(
		[out, retval]BSTR* pVal);
	[id(0x000000be), propput, helpstring("property MovieData")]
	HRESULT __stdcall MovieData(
		[in]BSTR pVal);
	[id(0x000000bf), propget, helpstring("property inline-data")]
	HRESULT __stdcall InlineData(
		[out, retval]IUnknown** ppIUnknown);
	[id(0x000000bf), propput, helpstring("property inline-data")]
	HRESULT __stdcall InlineData(
		[in]IUnknown* ppIUnknown);
	[id(0x000000c0), propget, helpstring("property SeamlessTabbing")]
	HRESULT __stdcall SeamlessTabbing(
		[out, retval]VARIANT_BOOL* pVal);
	[id(0x000000c0), propput, helpstring("property SeamlessTabbing")]
	HRESULT __stdcall SeamlessTabbing(
		[in]VARIANT_BOOL pVal);
	[id(0x000000c1), helpstring("method EnforceLocalSecurity")]
	HRESULT __stdcall EnforceLocalSecurity();
	[id(0x000000c2), propget, helpstring("property Profile")]
	HRESULT __stdcall Profile(
		[out, retval]VARIANT_BOOL* pVal);
	[id(0x000000c2), propput, helpstring("property Profile")]
	HRESULT __stdcall Profile(
		[in]VARIANT_BOOL pVal);
	[id(0x000000c3), propget, helpstring("property ProfileAddress")]
	HRESULT __stdcall ProfileAddress(
		[out, retval]BSTR* pVal);
	[id(0x000000c3), propput, helpstring("property ProfileAddress")]
	HRESULT __stdcall ProfileAddress(
		[in]BSTR pVal);
	[id(0x000000c4), propget, helpstring("property ProfilePort")]
	HRESULT __stdcall ProfilePort(
		[out, retval]long* pVal);
	[id(0x000000c4), propput, helpstring("property ProfilePort")]
	HRESULT __stdcall ProfilePort(
		[in]long pVal);
	[id(0x000000c6), helpstring("method Call")]
	HRESULT __stdcall CallFunction(
		[in]BSTR request,
		[out, retval]BSTR* response);
	[id(0x000000c7), helpstring("method SetReturnValue")]
	HRESULT __stdcall SetReturnValue(
		[in]BSTR returnValue);
	[id(0x000000c8), helpstring("method DisableLocalSecurity")]
	HRESULT __stdcall DisableLocalSecurity();
	[id(0x000000c9), propget, helpstring("property AllowNetworking")]
	HRESULT __stdcall AllowNetworking(
		[out, retval]BSTR* pVal);
	[id(0x000000c9), propput, helpstring("property AllowNetworking")]
	HRESULT __stdcall AllowNetworking(
		[in]BSTR pVal);
	[id(0x000000ca), propget, helpstring("property AllowFullScreen")]
	HRESULT __stdcall AllowFullScreen(
		[out, retval]BSTR* pVal);
	[id(0x000000ca), propput, helpstring("property AllowFullScreen")]
	HRESULT __stdcall AllowFullScreen(
		[in]BSTR pVal);
};
'!
!IShockwaveFlash categoriesForClass!COM-Interfaces!ShockwaveFlashObjects-Interfaces! !
!IShockwaveFlash methodsFor!

alignMode
	"Answer the <sdword> value of the 'AlignMode' property of the receiver.
	Helpstring: property AlignMode"

	| answer |
	answer := (SDWORD new).
	self get_AlignMode: answer.
	^answer asObject
!

alignMode: pVal
	"Set the 'AlignMode' property of the receiver to the <sdword> value of the argument.
	Helpstring: property AlignMode"

	self put_AlignMode: pVal
!

allowFullScreen
	"Answer the <bstr> value of the 'AllowFullScreen' property of the receiver.
	Helpstring: property AllowFullScreen"

	| answer |
	answer := BSTR new.
	self get_AllowFullScreen: answer.
	^answer asObject
!

allowFullScreen: pVal
	"Set the 'AllowFullScreen' property of the receiver to the <bstr> value of the argument.
	Helpstring: property AllowFullScreen"

	self put_AllowFullScreen: pVal
!

allowNetworking
	"Answer the <bstr> value of the 'AllowNetworking' property of the receiver.
	Helpstring: property AllowNetworking"

	| answer |
	answer := BSTR new.
	self get_AllowNetworking: answer.
	^answer asObject
!

allowNetworking: pVal
	"Set the 'AllowNetworking' property of the receiver to the <bstr> value of the argument.
	Helpstring: property AllowNetworking"

	self put_AllowNetworking: pVal
!

allowScriptAccess
	"Answer the <bstr> value of the 'AllowScriptAccess' property of the receiver.
	Helpstring: property AllowScriptAccess"

	| answer |
	answer := BSTR new.
	self get_AllowScriptAccess: answer.
	^answer asObject
!

allowScriptAccess: pVal
	"Set the 'AllowScriptAccess' property of the receiver to the <bstr> value of the argument.
	Helpstring: property AllowScriptAccess"

	self put_AllowScriptAccess: pVal
!

back
	"Invoke the Back() method of the COM object.
	Helpstring: method Back"

	^self Back
!

Back
	"Private - Invoke the Back() method of the COM object.
	Helpstring: method Back

		HRESULT __stdcall Back();"

	<virtual stdcall: hresult 31>
	^self invalidCall!

backgroundColor
	"Answer the <sdword> value of the 'BackgroundColor' property of the receiver.
	Helpstring: property BackgroundColor"

	| answer |
	answer := (SDWORD new).
	self get_BackgroundColor: answer.
	^answer asObject
!

backgroundColor: pVal
	"Set the 'BackgroundColor' property of the receiver to the <sdword> value of the argument.
	Helpstring: property BackgroundColor"

	self put_BackgroundColor: pVal
!

base
	"Answer the <bstr> value of the 'Base' property of the receiver.
	Helpstring: property Base"

	| answer |
	answer := BSTR new.
	self get_Base: answer.
	^answer asObject
!

base: pVal
	"Set the 'Base' property of the receiver to the <bstr> value of the argument.
	Helpstring: property Base"

	self put_Base: pVal
!

bgColor
	"Answer the <bstr> value of the 'BGColor' property of the receiver.
	Helpstring: property BGColor"

	| answer |
	answer := BSTR new.
	self get_BGColor: answer.
	^answer asObject
!

bgColor: pVal
	"Set the 'BGColor' property of the receiver to the <bstr> value of the argument.
	Helpstring: property BGColor"

	self put_BGColor: pVal
!

callFunction: request
	"Answer the <BSTR> result of invoking the CallFunction() method of the COM object.
	Helpstring: method Call"

	| answer |
	answer := BSTR new.
	self
		CallFunction: request
		response: answer.
	^answer asObject
!

CallFunction: request response: response
	"Private - Invoke the CallFunction() method of the COM object.
	Helpstring: method Call

		HRESULT __stdcall CallFunction(
			[in]BSTR request,
			[out, retval]BSTR* response);"

	<virtual stdcall: hresult 94 bstr bstr*>
	^self invalidCall!

currentFrame
	"Answer the <SDWORD> result of invoking the CurrentFrame() method of the COM object.
	Helpstring: method CurrentFrame"

	| answer |
	answer := (SDWORD new).
	self CurrentFrame: answer.
	^answer asObject
!

CurrentFrame: frameNum
	"Private - Invoke the CurrentFrame() method of the COM object.
	Helpstring: method CurrentFrame

		HRESULT __stdcall CurrentFrame(
			[out, retval]long* FrameNum);"

	<virtual stdcall: hresult 36 sdword*>
	^self invalidCall!

deviceFont
	"Answer the <varbool> value of the 'DeviceFont' property of the receiver.
	Helpstring: property DeviceFont"

	| answer |
	answer := (VARIANT_BOOL new).
	self get_DeviceFont: answer.
	^answer asObject
!

deviceFont: pVal
	"Set the 'DeviceFont' property of the receiver to the <varbool> value of the argument.
	Helpstring: property DeviceFont"

	self put_DeviceFont: pVal
!

disableLocalSecurity
	"Invoke the DisableLocalSecurity() method of the COM object.
	Helpstring: method DisableLocalSecurity"

	^self DisableLocalSecurity
!

DisableLocalSecurity
	"Private - Invoke the DisableLocalSecurity() method of the COM object.
	Helpstring: method DisableLocalSecurity

		HRESULT __stdcall DisableLocalSecurity();"

	<virtual stdcall: hresult 96>
	^self invalidCall!

embedMovie
	"Answer the <varbool> value of the 'EmbedMovie' property of the receiver.
	Helpstring: property EmbedMovie"

	| answer |
	answer := (VARIANT_BOOL new).
	self get_EmbedMovie: answer.
	^answer asObject
!

embedMovie: pVal
	"Set the 'EmbedMovie' property of the receiver to the <varbool> value of the argument.
	Helpstring: property EmbedMovie"

	self put_EmbedMovie: pVal
!

enforceLocalSecurity
	"Invoke the EnforceLocalSecurity() method of the COM object.
	Helpstring: method EnforceLocalSecurity"

	^self EnforceLocalSecurity
!

EnforceLocalSecurity
	"Private - Invoke the EnforceLocalSecurity() method of the COM object.
	Helpstring: method EnforceLocalSecurity

		HRESULT __stdcall EnforceLocalSecurity();"

	<virtual stdcall: hresult 87>
	^self invalidCall!

flashVars
	"Answer the <bstr> value of the 'FlashVars' property of the receiver.
	Helpstring: property FlashVars"

	| answer |
	answer := BSTR new.
	self get_FlashVars: answer.
	^answer asObject
!

flashVars: pVal
	"Set the 'FlashVars' property of the receiver to the <bstr> value of the argument.
	Helpstring: property FlashVars"

	self put_FlashVars: pVal
!

flashVersion
	"Answer the <SDWORD> result of invoking the FlashVersion() method of the COM object.
	Helpstring: method FlashVersion"

	| answer |
	answer := (SDWORD new).
	self FlashVersion: answer.
	^answer asObject
!

FlashVersion: version
	"Private - Invoke the FlashVersion() method of the COM object.
	Helpstring: method FlashVersion

		HRESULT __stdcall FlashVersion(
			[out, retval]long* version);"

	<virtual stdcall: hresult 40 sdword*>
	^self invalidCall!

forward
	"Invoke the Forward() method of the COM object.
	Helpstring: method Forward"

	^self Forward
!

Forward
	"Private - Invoke the Forward() method of the COM object.
	Helpstring: method Forward

		HRESULT __stdcall Forward();"

	<virtual stdcall: hresult 32>
	^self invalidCall!

frameLoaded: frameNum
	"Answer the <VARIANT_BOOL> result of invoking the FrameLoaded() method of the COM object.
	Helpstring: method FrameLoaded"

	| answer |
	answer := (VARIANT_BOOL new).
	self
		FrameLoaded: frameNum
		loaded: answer.
	^answer asObject
!

FrameLoaded: frameNum loaded: loaded
	"Private - Invoke the FrameLoaded() method of the COM object.
	Helpstring: method FrameLoaded

		HRESULT __stdcall FrameLoaded(
			[in]long FrameNum,
			[out, retval]VARIANT_BOOL* loaded);"

	<virtual stdcall: hresult 39 sdword varbool*>
	^self invalidCall!

frameNum
	"Answer the <sdword> value of the 'FrameNum' property of the receiver.
	Helpstring: property FrameNum"

	| answer |
	answer := (SDWORD new).
	self get_FrameNum: answer.
	^answer asObject
!

frameNum: pVal
	"Set the 'FrameNum' property of the receiver to the <sdword> value of the argument.
	Helpstring: property FrameNum"

	self put_FrameNum: pVal
!

get_AlignMode: pVal
	"Private - Get the value of the 'AlignMode' property of the receiver.

		HRESULT __stdcall AlignMode(
			[out, retval]int* pVal);"

	<virtual stdcall: hresult 16 sdword*>
	^self invalidCall!

get_AllowFullScreen: pVal
	"Private - Get the value of the 'AllowFullScreen' property of the receiver.

		HRESULT __stdcall AllowFullScreen(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 99 bstr*>
	^self invalidCall!

get_AllowNetworking: pVal
	"Private - Get the value of the 'AllowNetworking' property of the receiver.

		HRESULT __stdcall AllowNetworking(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 97 bstr*>
	^self invalidCall!

get_AllowScriptAccess: pVal
	"Private - Get the value of the 'AllowScriptAccess' property of the receiver.

		HRESULT __stdcall AllowScriptAccess(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 79 bstr*>
	^self invalidCall!

get_BackgroundColor: pVal
	"Private - Get the value of the 'BackgroundColor' property of the receiver.

		HRESULT __stdcall BackgroundColor(
			[out, retval]long* pVal);"

	<virtual stdcall: hresult 18 sdword*>
	^self invalidCall!

get_Base: pVal
	"Private - Get the value of the 'Base' property of the receiver.

		HRESULT __stdcall Base(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 47 bstr*>
	^self invalidCall!

get_BGColor: pVal
	"Private - Get the value of the 'BGColor' property of the receiver.

		HRESULT __stdcall BGColor(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 55 bstr*>
	^self invalidCall!

get_DeviceFont: pVal
	"Private - Get the value of the 'DeviceFont' property of the receiver.

		HRESULT __stdcall DeviceFont(
			[out, retval]VARIANT_BOOL* pVal);"

	<virtual stdcall: hresult 51 varbool*>
	^self invalidCall!

get_EmbedMovie: pVal
	"Private - Get the value of the 'EmbedMovie' property of the receiver.

		HRESULT __stdcall EmbedMovie(
			[out, retval]VARIANT_BOOL* pVal);"

	<virtual stdcall: hresult 53 varbool*>
	^self invalidCall!

get_FlashVars: pVal
	"Private - Get the value of the 'FlashVars' property of the receiver.

		HRESULT __stdcall FlashVars(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 77 bstr*>
	^self invalidCall!

get_FrameNum: pVal
	"Private - Get the value of the 'FrameNum' property of the receiver.

		HRESULT __stdcall FrameNum(
			[out, retval]long* pVal);"

	<virtual stdcall: hresult 24 sdword*>
	^self invalidCall!

get_InlineData: ppIUnknown
	"Private - Get the value of the 'InlineData' property of the receiver.

		HRESULT __stdcall InlineData(
			[out, retval]IUnknown** ppIUnknown);"

	<virtual stdcall: hresult 83 IUnknown**>
	^self invalidCall!

get_Loop: pVal
	"Private - Get the value of the 'Loop' property of the receiver.

		HRESULT __stdcall Loop(
			[out, retval]VARIANT_BOOL* pVal);"

	<virtual stdcall: hresult 20 varbool*>
	^self invalidCall!

get_Menu: pVal
	"Private - Get the value of the 'Menu' property of the receiver.

		HRESULT __stdcall Menu(
			[out, retval]VARIANT_BOOL* pVal);"

	<virtual stdcall: hresult 45 varbool*>
	^self invalidCall!

get_Movie: pVal
	"Private - Get the value of the 'Movie' property of the receiver.

		HRESULT __stdcall Movie(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 22 bstr*>
	^self invalidCall!

get_MovieData: pVal
	"Private - Get the value of the 'MovieData' property of the receiver.

		HRESULT __stdcall MovieData(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 81 bstr*>
	^self invalidCall!

get_Playing: pVal
	"Private - Get the value of the 'Playing' property of the receiver.

		HRESULT __stdcall Playing(
			[out, retval]VARIANT_BOOL* pVal);"

	<virtual stdcall: hresult 10 varbool*>
	^self invalidCall!

get_Profile: pVal
	"Private - Get the value of the 'Profile' property of the receiver.

		HRESULT __stdcall Profile(
			[out, retval]VARIANT_BOOL* pVal);"

	<virtual stdcall: hresult 88 varbool*>
	^self invalidCall!

get_ProfileAddress: pVal
	"Private - Get the value of the 'ProfileAddress' property of the receiver.

		HRESULT __stdcall ProfileAddress(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 90 bstr*>
	^self invalidCall!

get_ProfilePort: pVal
	"Private - Get the value of the 'ProfilePort' property of the receiver.

		HRESULT __stdcall ProfilePort(
			[out, retval]long* pVal);"

	<virtual stdcall: hresult 92 sdword*>
	^self invalidCall!

get_Quality: pVal
	"Private - Get the value of the 'Quality' property of the receiver.

		HRESULT __stdcall Quality(
			[out, retval]int* pVal);"

	<virtual stdcall: hresult 12 sdword*>
	^self invalidCall!

get_Quality2: pVal
	"Private - Get the value of the 'Quality2' property of the receiver.

		HRESULT __stdcall Quality2(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 57 bstr*>
	^self invalidCall!

get_ReadyState: pVal
	"Private - Get the value of the 'ReadyState' property of the receiver.

		HRESULT __stdcall ReadyState(
			[out, retval]long* pVal);"

	<virtual stdcall: hresult 8 sdword*>
	^self invalidCall!

get_SAlign: pVal
	"Private - Get the value of the 'SAlign' property of the receiver.

		HRESULT __stdcall SAlign(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 43 bstr*>
	^self invalidCall!

get_Scale: pVal
	"Private - Get the value of the 'Scale' property of the receiver.

		HRESULT __stdcall Scale(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 49 bstr*>
	^self invalidCall!

get_ScaleMode: pVal
	"Private - Get the value of the 'ScaleMode' property of the receiver.

		HRESULT __stdcall ScaleMode(
			[out, retval]int* pVal);"

	<virtual stdcall: hresult 14 sdword*>
	^self invalidCall!

get_SeamlessTabbing: pVal
	"Private - Get the value of the 'SeamlessTabbing' property of the receiver.

		HRESULT __stdcall SeamlessTabbing(
			[out, retval]VARIANT_BOOL* pVal);"

	<virtual stdcall: hresult 85 varbool*>
	^self invalidCall!

get_SWRemote: pVal
	"Private - Get the value of the 'SWRemote' property of the receiver.

		HRESULT __stdcall SWRemote(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 75 bstr*>
	^self invalidCall!

get_TotalFrames: pVal
	"Private - Get the value of the 'TotalFrames' property of the receiver.

		HRESULT __stdcall TotalFrames(
			[out, retval]long* pVal);"

	<virtual stdcall: hresult 9 sdword*>
	^self invalidCall!

get_WMode: pVal
	"Private - Get the value of the 'WMode' property of the receiver.

		HRESULT __stdcall WMode(
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 41 bstr*>
	^self invalidCall!

getVariable: name
	"Answer the <BSTR> result of invoking the GetVariable() method of the COM object.
	Helpstring: method GetVariable"

	| answer |
	answer := BSTR new.
	self
		GetVariable: name
		pVal: answer.
	^answer asObject
!

GetVariable: name pVal: pVal
	"Private - Invoke the GetVariable() method of the COM object.
	Helpstring: method GetVariable

		HRESULT __stdcall GetVariable(
			[in]BSTR name,
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 67 bstr bstr*>
	^self invalidCall!

gotoFrame: frameNum
	"Invoke the GotoFrame() method of the COM object.
	Helpstring: method GotoFrame"

	^self GotoFrame: frameNum
!

GotoFrame: frameNum
	"Private - Invoke the GotoFrame() method of the COM object.
	Helpstring: method GotoFrame

		HRESULT __stdcall GotoFrame(
			[in]long FrameNum);"

	<virtual stdcall: hresult 35 sdword>
	^self invalidCall!

inlineData
	"Answer the <IUnknown> value of the 'InlineData' property of the receiver.
	Helpstring: property inline-data"

	| answer |
	answer := IUnknown newPointer.
	self get_InlineData: answer.
	^answer asObject
!

inlineData: ppIUnknown
	"Set the 'InlineData' property of the receiver to the <IUnknown*> value of the argument.
	Helpstring: property inline-data"

	self put_InlineData: ppIUnknown
!

isPlaying
	"Answer the <VARIANT_BOOL> result of invoking the IsPlaying() method of the COM object.
	Helpstring: method IsPlaying"

	| answer |
	answer := (VARIANT_BOOL new).
	self IsPlaying: answer.
	^answer asObject
!

IsPlaying: playing
	"Private - Invoke the IsPlaying() method of the COM object.
	Helpstring: method IsPlaying

		HRESULT __stdcall IsPlaying(
			[out, retval]VARIANT_BOOL* Playing);"

	<virtual stdcall: hresult 37 varbool*>
	^self invalidCall!

loadMovie: layer url: url
	"Invoke the LoadMovie() method of the COM object.
	Helpstring: method LoadMovie"

	^self
		LoadMovie: layer
		url: url
!

LoadMovie: layer url: url
	"Private - Invoke the LoadMovie() method of the COM object.
	Helpstring: method LoadMovie

		HRESULT __stdcall LoadMovie(
			[in]int layer,
			[in]BSTR url);"

	<virtual stdcall: hresult 59 sdword bstr>
	^self invalidCall!

loop
	"Answer the <varbool> value of the 'Loop' property of the receiver.
	Helpstring: property Loop"

	| answer |
	answer := (VARIANT_BOOL new).
	self get_Loop: answer.
	^answer asObject
!

loop: pVal
	"Set the 'Loop' property of the receiver to the <varbool> value of the argument.
	Helpstring: property Loop"

	self put_Loop: pVal
!

menu
	"Answer the <varbool> value of the 'Menu' property of the receiver.
	Helpstring: property Menu"

	| answer |
	answer := (VARIANT_BOOL new).
	self get_Menu: answer.
	^answer asObject
!

menu: pVal
	"Set the 'Menu' property of the receiver to the <varbool> value of the argument.
	Helpstring: property Menu"

	self put_Menu: pVal
!

movie
	"Answer the <bstr> value of the 'Movie' property of the receiver.
	Helpstring: property Movie"

	| answer |
	answer := BSTR new.
	self get_Movie: answer.
	^answer asObject
!

movie: pVal
	"Set the 'Movie' property of the receiver to the <bstr> value of the argument.
	Helpstring: property Movie"

	self put_Movie: pVal
!

movieData
	"Answer the <bstr> value of the 'MovieData' property of the receiver.
	Helpstring: property MovieData"

	| answer |
	answer := BSTR new.
	self get_MovieData: answer.
	^answer asObject
!

movieData: pVal
	"Set the 'MovieData' property of the receiver to the <bstr> value of the argument.
	Helpstring: property MovieData"

	self put_MovieData: pVal
!

pan: x y: y mode: mode
	"Invoke the Pan() method of the COM object.
	Helpstring: method Pan"

	^self
		Pan: x
		y: y
		mode: mode
!

Pan: x y: y mode: mode
	"Private - Invoke the Pan() method of the COM object.
	Helpstring: method Pan

		HRESULT __stdcall Pan(
			[in]long x,
			[in]long y,
			[in]int mode);"

	<virtual stdcall: hresult 28 sdword sdword sdword>
	^self invalidCall!

percentLoaded
	"Answer the <SDWORD> result of invoking the PercentLoaded() method of the COM object.
	Helpstring: method PercentLoaded"

	| answer |
	answer := (SDWORD new).
	self PercentLoaded: answer.
	^answer asObject
!

PercentLoaded: percent
	"Private - Invoke the PercentLoaded() method of the COM object.
	Helpstring: method PercentLoaded

		HRESULT __stdcall PercentLoaded(
			[out, retval]long* percent);"

	<virtual stdcall: hresult 38 sdword*>
	^self invalidCall!

play
	"Invoke the Play() method of the COM object.
	Helpstring: method Play"

	^self Play
!

Play
	"Private - Invoke the Play() method of the COM object.
	Helpstring: method Play

		HRESULT __stdcall Play();"

	<virtual stdcall: hresult 29>
	^self invalidCall!

playing
	"Answer the <varbool> value of the 'Playing' property of the receiver.
	Helpstring: property Playing"

	| answer |
	answer := (VARIANT_BOOL new).
	self get_Playing: answer.
	^answer asObject
!

playing: pVal
	"Set the 'Playing' property of the receiver to the <varbool> value of the argument.
	Helpstring: property Playing"

	self put_Playing: pVal
!

profile
	"Answer the <varbool> value of the 'Profile' property of the receiver.
	Helpstring: property Profile"

	| answer |
	answer := (VARIANT_BOOL new).
	self get_Profile: answer.
	^answer asObject
!

profile: pVal
	"Set the 'Profile' property of the receiver to the <varbool> value of the argument.
	Helpstring: property Profile"

	self put_Profile: pVal
!

profileAddress
	"Answer the <bstr> value of the 'ProfileAddress' property of the receiver.
	Helpstring: property ProfileAddress"

	| answer |
	answer := BSTR new.
	self get_ProfileAddress: answer.
	^answer asObject
!

profileAddress: pVal
	"Set the 'ProfileAddress' property of the receiver to the <bstr> value of the argument.
	Helpstring: property ProfileAddress"

	self put_ProfileAddress: pVal
!

profilePort
	"Answer the <sdword> value of the 'ProfilePort' property of the receiver.
	Helpstring: property ProfilePort"

	| answer |
	answer := (SDWORD new).
	self get_ProfilePort: answer.
	^answer asObject
!

profilePort: pVal
	"Set the 'ProfilePort' property of the receiver to the <sdword> value of the argument.
	Helpstring: property ProfilePort"

	self put_ProfilePort: pVal
!

put_AlignMode: pVal
	"Private - Set the value of the 'AlignMode' property of the object wrapped by the 
	 receiver to the <sdword> argument, pVal.

		HRESULT __stdcall AlignMode(
			[in]int pVal);"

	<virtual stdcall: hresult 17 sdword>
	^self invalidCall!

put_AllowFullScreen: pVal
	"Private - Set the value of the 'AllowFullScreen' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall AllowFullScreen(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 100 bstr>
	^self invalidCall!

put_AllowNetworking: pVal
	"Private - Set the value of the 'AllowNetworking' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall AllowNetworking(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 98 bstr>
	^self invalidCall!

put_AllowScriptAccess: pVal
	"Private - Set the value of the 'AllowScriptAccess' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall AllowScriptAccess(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 80 bstr>
	^self invalidCall!

put_BackgroundColor: pVal
	"Private - Set the value of the 'BackgroundColor' property of the object wrapped by the 
	 receiver to the <sdword> argument, pVal.

		HRESULT __stdcall BackgroundColor(
			[in]long pVal);"

	<virtual stdcall: hresult 19 sdword>
	^self invalidCall!

put_Base: pVal
	"Private - Set the value of the 'Base' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall Base(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 48 bstr>
	^self invalidCall!

put_BGColor: pVal
	"Private - Set the value of the 'BGColor' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall BGColor(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 56 bstr>
	^self invalidCall!

put_DeviceFont: pVal
	"Private - Set the value of the 'DeviceFont' property of the object wrapped by the 
	 receiver to the <varbool> argument, pVal.

		HRESULT __stdcall DeviceFont(
			[in]VARIANT_BOOL pVal);"

	<virtual stdcall: hresult 52 varbool>
	^self invalidCall!

put_EmbedMovie: pVal
	"Private - Set the value of the 'EmbedMovie' property of the object wrapped by the 
	 receiver to the <varbool> argument, pVal.

		HRESULT __stdcall EmbedMovie(
			[in]VARIANT_BOOL pVal);"

	<virtual stdcall: hresult 54 varbool>
	^self invalidCall!

put_FlashVars: pVal
	"Private - Set the value of the 'FlashVars' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall FlashVars(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 78 bstr>
	^self invalidCall!

put_FrameNum: pVal
	"Private - Set the value of the 'FrameNum' property of the object wrapped by the 
	 receiver to the <sdword> argument, pVal.

		HRESULT __stdcall FrameNum(
			[in]long pVal);"

	<virtual stdcall: hresult 25 sdword>
	^self invalidCall!

put_InlineData: ppIUnknown
	"Private - Set the value of the 'InlineData' property of the object wrapped by the 
	 receiver to the <IUnknown*> argument, ppIUnknown.

		HRESULT __stdcall InlineData(
			[in]IUnknown* ppIUnknown);"

	<virtual stdcall: hresult 84 IUnknown*>
	^self invalidCall!

put_Loop: pVal
	"Private - Set the value of the 'Loop' property of the object wrapped by the 
	 receiver to the <varbool> argument, pVal.

		HRESULT __stdcall Loop(
			[in]VARIANT_BOOL pVal);"

	<virtual stdcall: hresult 21 varbool>
	^self invalidCall!

put_Menu: pVal
	"Private - Set the value of the 'Menu' property of the object wrapped by the 
	 receiver to the <varbool> argument, pVal.

		HRESULT __stdcall Menu(
			[in]VARIANT_BOOL pVal);"

	<virtual stdcall: hresult 46 varbool>
	^self invalidCall!

put_Movie: pVal 
	"Private - Set the value of the 'Movie' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall Movie(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 23 bstr>
	^self invalidCall!

put_MovieData: pVal
	"Private - Set the value of the 'MovieData' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall MovieData(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 82 bstr>
	^self invalidCall!

put_Playing: pVal
	"Private - Set the value of the 'Playing' property of the object wrapped by the 
	 receiver to the <varbool> argument, pVal.

		HRESULT __stdcall Playing(
			[in]VARIANT_BOOL pVal);"

	<virtual stdcall: hresult 11 varbool>
	^self invalidCall!

put_Profile: pVal
	"Private - Set the value of the 'Profile' property of the object wrapped by the 
	 receiver to the <varbool> argument, pVal.

		HRESULT __stdcall Profile(
			[in]VARIANT_BOOL pVal);"

	<virtual stdcall: hresult 89 varbool>
	^self invalidCall!

put_ProfileAddress: pVal
	"Private - Set the value of the 'ProfileAddress' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall ProfileAddress(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 91 bstr>
	^self invalidCall!

put_ProfilePort: pVal
	"Private - Set the value of the 'ProfilePort' property of the object wrapped by the 
	 receiver to the <sdword> argument, pVal.

		HRESULT __stdcall ProfilePort(
			[in]long pVal);"

	<virtual stdcall: hresult 93 sdword>
	^self invalidCall!

put_Quality: pVal
	"Private - Set the value of the 'Quality' property of the object wrapped by the 
	 receiver to the <sdword> argument, pVal.

		HRESULT __stdcall Quality(
			[in]int pVal);"

	<virtual stdcall: hresult 13 sdword>
	^self invalidCall!

put_Quality2: pVal
	"Private - Set the value of the 'Quality2' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall Quality2(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 58 bstr>
	^self invalidCall!

put_SAlign: pVal
	"Private - Set the value of the 'SAlign' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall SAlign(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 44 bstr>
	^self invalidCall!

put_Scale: pVal
	"Private - Set the value of the 'Scale' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall Scale(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 50 bstr>
	^self invalidCall!

put_ScaleMode: pVal
	"Private - Set the value of the 'ScaleMode' property of the object wrapped by the 
	 receiver to the <sdword> argument, pVal.

		HRESULT __stdcall ScaleMode(
			[in]int pVal);"

	<virtual stdcall: hresult 15 sdword>
	^self invalidCall!

put_SeamlessTabbing: pVal
	"Private - Set the value of the 'SeamlessTabbing' property of the object wrapped by the 
	 receiver to the <varbool> argument, pVal.

		HRESULT __stdcall SeamlessTabbing(
			[in]VARIANT_BOOL pVal);"

	<virtual stdcall: hresult 86 varbool>
	^self invalidCall!

put_SWRemote: pVal
	"Private - Set the value of the 'SWRemote' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall SWRemote(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 76 bstr>
	^self invalidCall!

put_WMode: pVal
	"Private - Set the value of the 'WMode' property of the object wrapped by the 
	 receiver to the <bstr> argument, pVal.

		HRESULT __stdcall WMode(
			[in]BSTR pVal);"

	<virtual stdcall: hresult 42 bstr>
	^self invalidCall!

quality
	"Answer the <sdword> value of the 'Quality' property of the receiver.
	Helpstring: property Quality"

	| answer |
	answer := (SDWORD new).
	self get_Quality: answer.
	^answer asObject
!

quality: pVal
	"Set the 'Quality' property of the receiver to the <sdword> value of the argument.
	Helpstring: property Quality"

	self put_Quality: pVal
!

quality2
	"Answer the <bstr> value of the 'Quality2' property of the receiver.
	Helpstring: property Quality2"

	| answer |
	answer := BSTR new.
	self get_Quality2: answer.
	^answer asObject
!

quality2: pVal
	"Set the 'Quality2' property of the receiver to the <bstr> value of the argument.
	Helpstring: property Quality2"

	self put_Quality2: pVal
!

readyState
	"Answer the <sdword> value of the 'ReadyState' property of the receiver.
	Helpstring: property ReadyState"

	| answer |
	answer := (SDWORD new).
	self get_ReadyState: answer.
	^answer asObject
!

rewind
	"Invoke the Rewind() method of the COM object.
	Helpstring: method Rewind"

	^self Rewind
!

Rewind
	"Private - Invoke the Rewind() method of the COM object.
	Helpstring: method Rewind

		HRESULT __stdcall Rewind();"

	<virtual stdcall: hresult 33>
	^self invalidCall!

sAlign
	"Answer the <bstr> value of the 'SAlign' property of the receiver.
	Helpstring: property SAlign"

	| answer |
	answer := BSTR new.
	self get_SAlign: answer.
	^answer asObject
!

sAlign: pVal
	"Set the 'SAlign' property of the receiver to the <bstr> value of the argument.
	Helpstring: property SAlign"

	self put_SAlign: pVal
!

scale
	"Answer the <bstr> value of the 'Scale' property of the receiver.
	Helpstring: property Scale"

	| answer |
	answer := BSTR new.
	self get_Scale: answer.
	^answer asObject
!

scale: pVal
	"Set the 'Scale' property of the receiver to the <bstr> value of the argument.
	Helpstring: property Scale"

	self put_Scale: pVal
!

scaleMode
	"Answer the <sdword> value of the 'ScaleMode' property of the receiver.
	Helpstring: property ScaleMode"

	| answer |
	answer := (SDWORD new).
	self get_ScaleMode: answer.
	^answer asObject
!

scaleMode: pVal
	"Set the 'ScaleMode' property of the receiver to the <sdword> value of the argument.
	Helpstring: property ScaleMode"

	self put_ScaleMode: pVal
!

seamlessTabbing
	"Answer the <varbool> value of the 'SeamlessTabbing' property of the receiver.
	Helpstring: property SeamlessTabbing"

	| answer |
	answer := (VARIANT_BOOL new).
	self get_SeamlessTabbing: answer.
	^answer asObject
!

seamlessTabbing: pVal
	"Set the 'SeamlessTabbing' property of the receiver to the <varbool> value of the argument.
	Helpstring: property SeamlessTabbing"

	self put_SeamlessTabbing: pVal
!

setReturnValue: returnValue
	"Invoke the SetReturnValue() method of the COM object.
	Helpstring: method SetReturnValue"

	^self SetReturnValue: returnValue
!

SetReturnValue: returnValue
	"Private - Invoke the SetReturnValue() method of the COM object.
	Helpstring: method SetReturnValue

		HRESULT __stdcall SetReturnValue(
			[in]BSTR returnValue);"

	<virtual stdcall: hresult 95 bstr>
	^self invalidCall!

setVariable: name value: value
	"Invoke the SetVariable() method of the COM object.
	Helpstring: method SetVariable"

	^self
		SetVariable: name
		value: value
!

SetVariable: name value: value
	"Private - Invoke the SetVariable() method of the COM object.
	Helpstring: method SetVariable

		HRESULT __stdcall SetVariable(
			[in]BSTR name,
			[in]BSTR value);"

	<virtual stdcall: hresult 66 bstr bstr>
	^self invalidCall!

setZoomRect: left top: top right: right bottom: bottom
	"Invoke the SetZoomRect() method of the COM object.
	Helpstring: method SetZoomRect"

	^self
		SetZoomRect: left
		top: top
		right: right
		bottom: bottom
!

SetZoomRect: left top: top right: right bottom: bottom
	"Private - Invoke the SetZoomRect() method of the COM object.
	Helpstring: method SetZoomRect

		HRESULT __stdcall SetZoomRect(
			[in]long left,
			[in]long top,
			[in]long right,
			[in]long bottom);"

	<virtual stdcall: hresult 26 sdword sdword sdword sdword>
	^self invalidCall!

stop
	"Invoke the Stop() method of the COM object.
	Helpstring: method Stop"

	^self Stop
!

Stop
	"Private - Invoke the Stop() method of the COM object.
	Helpstring: method Stop

		HRESULT __stdcall Stop();"

	<virtual stdcall: hresult 30>
	^self invalidCall!

stopPlay
	"Invoke the StopPlay() method of the COM object.
	Helpstring: method StopPlay"

	^self StopPlay
!

StopPlay
	"Private - Invoke the StopPlay() method of the COM object.
	Helpstring: method StopPlay

		HRESULT __stdcall StopPlay();"

	<virtual stdcall: hresult 34>
	^self invalidCall!

swRemote
	"Answer the <bstr> value of the 'SWRemote' property of the receiver.
	Helpstring: property SWRemote"

	| answer |
	answer := BSTR new.
	self get_SWRemote: answer.
	^answer asObject
!

swRemote: pVal
	"Set the 'SWRemote' property of the receiver to the <bstr> value of the argument.
	Helpstring: property SWRemote"

	self put_SWRemote: pVal
!

tCallFrame: target frameNum: frameNum
	"Invoke the TCallFrame() method of the COM object.
	Helpstring: method TCallFrame"

	^self
		TCallFrame: target
		frameNum: frameNum
!

TCallFrame: target frameNum: frameNum
	"Private - Invoke the TCallFrame() method of the COM object.
	Helpstring: method TCallFrame

		HRESULT __stdcall TCallFrame(
			[in]BSTR target,
			[in]int FrameNum);"

	<virtual stdcall: hresult 70 bstr sdword>
	^self invalidCall!

tCallLabel: target label: label
	"Invoke the TCallLabel() method of the COM object.
	Helpstring: method TCallLabel"

	^self
		TCallLabel: target
		label: label
!

TCallLabel: target label: label
	"Private - Invoke the TCallLabel() method of the COM object.
	Helpstring: method TCallLabel

		HRESULT __stdcall TCallLabel(
			[in]BSTR target,
			[in]BSTR label);"

	<virtual stdcall: hresult 71 bstr bstr>
	^self invalidCall!

tCurrentFrame: target
	"Answer the <SDWORD> result of invoking the TCurrentFrame() method of the COM object.
	Helpstring: method TCurrentFrame"

	| answer |
	answer := (SDWORD new).
	self
		TCurrentFrame: target
		frameNum: answer.
	^answer asObject
!

TCurrentFrame: target frameNum: frameNum
	"Private - Invoke the TCurrentFrame() method of the COM object.
	Helpstring: method TCurrentFrame

		HRESULT __stdcall TCurrentFrame(
			[in]BSTR target,
			[out, retval]long* FrameNum);"

	<virtual stdcall: hresult 62 bstr sdword*>
	^self invalidCall!

tCurrentLabel: target
	"Answer the <BSTR> result of invoking the TCurrentLabel() method of the COM object.
	Helpstring: method TCurrentLabel"

	| answer |
	answer := BSTR new.
	self
		TCurrentLabel: target
		pVal: answer.
	^answer asObject
!

TCurrentLabel: target pVal: pVal
	"Private - Invoke the TCurrentLabel() method of the COM object.
	Helpstring: method TCurrentLabel

		HRESULT __stdcall TCurrentLabel(
			[in]BSTR target,
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 63 bstr bstr*>
	^self invalidCall!

tGetProperty: target property: property
	"Answer the <BSTR> result of invoking the TGetProperty() method of the COM object.
	Helpstring: method TGetProperty"

	| answer |
	answer := BSTR new.
	self
		TGetProperty: target
		property: property
		pVal: answer.
	^answer asObject
!

TGetProperty: target property: property pVal: pVal
	"Private - Invoke the TGetProperty() method of the COM object.
	Helpstring: method TGetProperty

		HRESULT __stdcall TGetProperty(
			[in]BSTR target,
			[in]int property,
			[out, retval]BSTR* pVal);"

	<virtual stdcall: hresult 69 bstr sdword bstr*>
	^self invalidCall!

tGetPropertyAsNumber: target property: property
	"Answer the <DOUBLE> result of invoking the TGetPropertyAsNumber() method of the COM object.
	Helpstring: method TGetPropertyAsNumber"

	| answer |
	answer := (DOUBLE new).
	self
		TGetPropertyAsNumber: target
		property: property
		pVal: answer.
	^answer asObject
!

TGetPropertyAsNumber: target property: property pVal: pVal
	"Private - Invoke the TGetPropertyAsNumber() method of the COM object.
	Helpstring: method TGetPropertyAsNumber

		HRESULT __stdcall TGetPropertyAsNumber(
			[in]BSTR target,
			[in]int property,
			[out, retval]double* pVal);"

	<virtual stdcall: hresult 74 bstr sdword double*>
	^self invalidCall!

tGetPropertyNum: target property: property
	"Answer the <DOUBLE> result of invoking the TGetPropertyNum() method of the COM object.
	Helpstring: method TGetPropertyNum"

	| answer |
	answer := (DOUBLE new).
	self
		TGetPropertyNum: target
		property: property
		pVal: answer.
	^answer asObject
!

TGetPropertyNum: target property: property pVal: pVal
	"Private - Invoke the TGetPropertyNum() method of the COM object.
	Helpstring: method TGetPropertyNum

		HRESULT __stdcall TGetPropertyNum(
			[in]BSTR target,
			[in]int property,
			[out, retval]double* pVal);"

	<virtual stdcall: hresult 73 bstr sdword double*>
	^self invalidCall!

tGotoFrame: target frameNum: frameNum
	"Invoke the TGotoFrame() method of the COM object.
	Helpstring: method TGotoFrame"

	^self
		TGotoFrame: target
		frameNum: frameNum
!

TGotoFrame: target frameNum: frameNum
	"Private - Invoke the TGotoFrame() method of the COM object.
	Helpstring: method TGotoFrame

		HRESULT __stdcall TGotoFrame(
			[in]BSTR target,
			[in]long FrameNum);"

	<virtual stdcall: hresult 60 bstr sdword>
	^self invalidCall!

tGotoLabel: target label: label
	"Invoke the TGotoLabel() method of the COM object.
	Helpstring: method TGotoLabel"

	^self
		TGotoLabel: target
		label: label
!

TGotoLabel: target label: label
	"Private - Invoke the TGotoLabel() method of the COM object.
	Helpstring: method TGotoLabel

		HRESULT __stdcall TGotoLabel(
			[in]BSTR target,
			[in]BSTR label);"

	<virtual stdcall: hresult 61 bstr bstr>
	^self invalidCall!

totalFrames
	"Answer the <sdword> value of the 'TotalFrames' property of the receiver.
	Helpstring: property TotalFrames"

	| answer |
	answer := (SDWORD new).
	self get_TotalFrames: answer.
	^answer asObject
!

tPlay: target
	"Invoke the TPlay() method of the COM object.
	Helpstring: method TPlay"

	^self TPlay: target
!

TPlay: target
	"Private - Invoke the TPlay() method of the COM object.
	Helpstring: method TPlay

		HRESULT __stdcall TPlay(
			[in]BSTR target);"

	<virtual stdcall: hresult 64 bstr>
	^self invalidCall!

tSetProperty: target property: property value: value
	"Invoke the TSetProperty() method of the COM object.
	Helpstring: method TSetProperty"

	^self
		TSetProperty: target
		property: property
		value: value
!

TSetProperty: target property: property value: value
	"Private - Invoke the TSetProperty() method of the COM object.
	Helpstring: method TSetProperty

		HRESULT __stdcall TSetProperty(
			[in]BSTR target,
			[in]int property,
			[in]BSTR value);"

	<virtual stdcall: hresult 68 bstr sdword bstr>
	^self invalidCall!

tSetPropertyNum: target property: property value: value
	"Invoke the TSetPropertyNum() method of the COM object.
	Helpstring: method TSetPropertyNum"

	^self
		TSetPropertyNum: target
		property: property
		value: value
!

TSetPropertyNum: target property: property value: value
	"Private - Invoke the TSetPropertyNum() method of the COM object.
	Helpstring: method TSetPropertyNum

		HRESULT __stdcall TSetPropertyNum(
			[in]BSTR target,
			[in]int property,
			[in]double value);"

	<virtual stdcall: hresult 72 bstr sdword double>
	^self invalidCall!

tStopPlay: target
	"Invoke the TStopPlay() method of the COM object.
	Helpstring: method TStopPlay"

	^self TStopPlay: target
!

TStopPlay: target
	"Private - Invoke the TStopPlay() method of the COM object.
	Helpstring: method TStopPlay

		HRESULT __stdcall TStopPlay(
			[in]BSTR target);"

	<virtual stdcall: hresult 65 bstr>
	^self invalidCall!

wMode
	"Answer the <bstr> value of the 'WMode' property of the receiver.
	Helpstring: property WMode"

	| answer |
	answer := BSTR new.
	self get_WMode: answer.
	^answer asObject
!

wMode: pVal
	"Set the 'WMode' property of the receiver to the <bstr> value of the argument.
	Helpstring: property WMode"

	self put_WMode: pVal
!

zoom: factor
	"Invoke the Zoom() method of the COM object.
	Helpstring: method Zoom"

	^self Zoom: factor
!

Zoom: factor
	"Private - Invoke the Zoom() method of the COM object.
	Helpstring: method Zoom

		HRESULT __stdcall Zoom(
			[in]int factor);"

	<virtual stdcall: hresult 27 sdword>
	^self invalidCall! !
!IShockwaveFlash categoriesFor: #alignMode!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #alignMode:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #allowFullScreen!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #allowFullScreen:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #allowNetworking!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #allowNetworking:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #allowScriptAccess!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #allowScriptAccess:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #back!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #Back!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #backgroundColor!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #backgroundColor:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #base!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #base:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #bgColor!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #bgColor:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #callFunction:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #CallFunction:response:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #currentFrame!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #CurrentFrame:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #deviceFont!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #deviceFont:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #disableLocalSecurity!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #DisableLocalSecurity!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #embedMovie!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #embedMovie:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #enforceLocalSecurity!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #EnforceLocalSecurity!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #flashVars!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #flashVars:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #flashVersion!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #FlashVersion:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #forward!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #Forward!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #frameLoaded:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #FrameLoaded:loaded:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #frameNum!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #frameNum:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #get_AlignMode:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_AllowFullScreen:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_AllowNetworking:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_AllowScriptAccess:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_BackgroundColor:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Base:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_BGColor:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_DeviceFont:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_EmbedMovie:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_FlashVars:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_FrameNum:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_InlineData:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Loop:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Menu:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Movie:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_MovieData:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Playing:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Profile:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_ProfileAddress:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_ProfilePort:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Quality:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Quality2:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_ReadyState:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_SAlign:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_Scale:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_ScaleMode:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_SeamlessTabbing:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_SWRemote:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_TotalFrames:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #get_WMode:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #getVariable:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #GetVariable:pVal:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #gotoFrame:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #GotoFrame:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #inlineData!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #inlineData:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #isPlaying!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #IsPlaying:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #loadMovie:url:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #LoadMovie:url:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #loop!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #loop:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #menu!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #menu:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #movie!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #movie:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #movieData!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #movieData:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #pan:y:mode:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #Pan:y:mode:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #percentLoaded!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #PercentLoaded:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #play!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #Play!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #playing!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #playing:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #profile!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #profile:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #profileAddress!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #profileAddress:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #profilePort!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #profilePort:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #put_AlignMode:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_AllowFullScreen:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_AllowNetworking:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_AllowScriptAccess:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_BackgroundColor:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Base:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_BGColor:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_DeviceFont:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_EmbedMovie:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_FlashVars:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_FrameNum:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_InlineData:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Loop:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Menu:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Movie:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_MovieData:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Playing:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Profile:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_ProfileAddress:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_ProfilePort:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Quality:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Quality2:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_SAlign:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_Scale:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_ScaleMode:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_SeamlessTabbing:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_SWRemote:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #put_WMode:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #quality!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #quality:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #quality2!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #quality2:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #readyState!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #rewind!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #Rewind!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #sAlign!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #sAlign:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #scale!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #scale:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #scaleMode!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #scaleMode:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #seamlessTabbing!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #seamlessTabbing:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #setReturnValue:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #SetReturnValue:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #setVariable:value:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #SetVariable:value:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #setZoomRect:top:right:bottom:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #SetZoomRect:top:right:bottom:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #stop!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #Stop!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #stopPlay!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #StopPlay!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #swRemote!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #swRemote:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #tCallFrame:frameNum:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TCallFrame:frameNum:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tCallLabel:label:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TCallLabel:label:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tCurrentFrame:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TCurrentFrame:frameNum:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tCurrentLabel:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TCurrentLabel:pVal:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tGetProperty:property:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TGetProperty:property:pVal:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tGetPropertyAsNumber:property:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TGetPropertyAsNumber:property:pVal:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tGetPropertyNum:property:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TGetPropertyNum:property:pVal:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tGotoFrame:frameNum:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TGotoFrame:frameNum:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tGotoLabel:label:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TGotoLabel:label:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #totalFrames!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #tPlay:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TPlay:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tSetProperty:property:value:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TSetProperty:property:value:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tSetPropertyNum:property:value:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TSetPropertyNum:property:value:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #tStopPlay:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #TStopPlay:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !
!IShockwaveFlash categoriesFor: #wMode!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #wMode:!**auto generated**!properties!public! !
!IShockwaveFlash categoriesFor: #zoom:!**auto generated**!methods!public! !
!IShockwaveFlash categoriesFor: #Zoom:!**auto generated**!COM Interfaces-IShockwaveFlash!private! !

!IShockwaveFlash class methodsFor!

clsid
	"Private - Answer the CLSID of the coclass (ShockwaveFlash) for which the receiver is the default interface."

	^CLSID fromString: '{D27CDB6E-AE6D-11CF-96B8-444553540000}'
!

defineFunctions
	"Declare the virtual function table for the COM interface 'ShockwaveFlashObjects.IShockwaveFlash'
		IShockwaveFlash defineTemplate"

	self
		defineFunction: #get_ReadyState:
			argumentTypes: 'sdword*';
		defineFunction: #get_TotalFrames:
			argumentTypes: 'sdword*';
		defineFunction: #get_Playing:
			argumentTypes: 'varbool*';
		defineFunction: #put_Playing:
			argumentTypes: 'varbool';
		defineFunction: #get_Quality:
			argumentTypes: 'sdword*';
		defineFunction: #put_Quality:
			argumentTypes: 'sdword';
		defineFunction: #get_ScaleMode:
			argumentTypes: 'sdword*';
		defineFunction: #put_ScaleMode:
			argumentTypes: 'sdword';
		defineFunction: #get_AlignMode:
			argumentTypes: 'sdword*';
		defineFunction: #put_AlignMode:
			argumentTypes: 'sdword';
		defineFunction: #get_BackgroundColor:
			argumentTypes: 'sdword*';
		defineFunction: #put_BackgroundColor:
			argumentTypes: 'sdword';
		defineFunction: #get_Loop:
			argumentTypes: 'varbool*';
		defineFunction: #put_Loop:
			argumentTypes: 'varbool';
		defineFunction: #get_Movie:
			argumentTypes: 'bstr*';
		defineFunction: #put_Movie:
			argumentTypes: 'bstr';
		defineFunction: #get_FrameNum:
			argumentTypes: 'sdword*';
		defineFunction: #put_FrameNum:
			argumentTypes: 'sdword';
		defineFunction: #SetZoomRect:top:right:bottom:
			argumentTypes: 'sdword sdword sdword sdword';
		defineFunction: #Zoom:
			argumentTypes: 'sdword';
		defineFunction: #Pan:y:mode:
			argumentTypes: 'sdword sdword sdword';
		defineFunction: #Play;
		defineFunction: #Stop;
		defineFunction: #Back;
		defineFunction: #Forward;
		defineFunction: #Rewind;
		defineFunction: #StopPlay;
		defineFunction: #GotoFrame:
			argumentTypes: 'sdword';
		defineFunction: #CurrentFrame:
			argumentTypes: 'sdword*';
		defineFunction: #IsPlaying:
			argumentTypes: 'varbool*';
		defineFunction: #PercentLoaded:
			argumentTypes: 'sdword*';
		defineFunction: #FrameLoaded:loaded:
			argumentTypes: 'sdword varbool*';
		defineFunction: #FlashVersion:
			argumentTypes: 'sdword*';
		defineFunction: #get_WMode:
			argumentTypes: 'bstr*';
		defineFunction: #put_WMode:
			argumentTypes: 'bstr';
		defineFunction: #get_SAlign:
			argumentTypes: 'bstr*';
		defineFunction: #put_SAlign:
			argumentTypes: 'bstr';
		defineFunction: #get_Menu:
			argumentTypes: 'varbool*';
		defineFunction: #put_Menu:
			argumentTypes: 'varbool';
		defineFunction: #get_Base:
			argumentTypes: 'bstr*';
		defineFunction: #put_Base:
			argumentTypes: 'bstr';
		defineFunction: #get_Scale:
			argumentTypes: 'bstr*';
		defineFunction: #put_Scale:
			argumentTypes: 'bstr';
		defineFunction: #get_DeviceFont:
			argumentTypes: 'varbool*';
		defineFunction: #put_DeviceFont:
			argumentTypes: 'varbool';
		defineFunction: #get_EmbedMovie:
			argumentTypes: 'varbool*';
		defineFunction: #put_EmbedMovie:
			argumentTypes: 'varbool';
		defineFunction: #get_BGColor:
			argumentTypes: 'bstr*';
		defineFunction: #put_BGColor:
			argumentTypes: 'bstr';
		defineFunction: #get_Quality2:
			argumentTypes: 'bstr*';
		defineFunction: #put_Quality2:
			argumentTypes: 'bstr';
		defineFunction: #LoadMovie:url:
			argumentTypes: 'sdword bstr';
		defineFunction: #TGotoFrame:frameNum:
			argumentTypes: 'bstr sdword';
		defineFunction: #TGotoLabel:label:
			argumentTypes: 'bstr bstr';
		defineFunction: #TCurrentFrame:frameNum:
			argumentTypes: 'bstr sdword*';
		defineFunction: #TCurrentLabel:pVal:
			argumentTypes: 'bstr bstr*';
		defineFunction: #TPlay:
			argumentTypes: 'bstr';
		defineFunction: #TStopPlay:
			argumentTypes: 'bstr';
		defineFunction: #SetVariable:value:
			argumentTypes: 'bstr bstr';
		defineFunction: #GetVariable:pVal:
			argumentTypes: 'bstr bstr*';
		defineFunction: #TSetProperty:property:value:
			argumentTypes: 'bstr sdword bstr';
		defineFunction: #TGetProperty:property:pVal:
			argumentTypes: 'bstr sdword bstr*';
		defineFunction: #TCallFrame:frameNum:
			argumentTypes: 'bstr sdword';
		defineFunction: #TCallLabel:label:
			argumentTypes: 'bstr bstr';
		defineFunction: #TSetPropertyNum:property:value:
			argumentTypes: 'bstr sdword double';
		defineFunction: #TGetPropertyNum:property:pVal:
			argumentTypes: 'bstr sdword double*';
		defineFunction: #TGetPropertyAsNumber:property:pVal:
			argumentTypes: 'bstr sdword double*';
		defineFunction: #get_SWRemote:
			argumentTypes: 'bstr*';
		defineFunction: #put_SWRemote:
			argumentTypes: 'bstr';
		defineFunction: #get_FlashVars:
			argumentTypes: 'bstr*';
		defineFunction: #put_FlashVars:
			argumentTypes: 'bstr';
		defineFunction: #get_AllowScriptAccess:
			argumentTypes: 'bstr*';
		defineFunction: #put_AllowScriptAccess:
			argumentTypes: 'bstr';
		defineFunction: #get_MovieData:
			argumentTypes: 'bstr*';
		defineFunction: #put_MovieData:
			argumentTypes: 'bstr';
		defineFunction: #get_InlineData:
			argumentTypes: 'IUnknown**';
		defineFunction: #put_InlineData:
			argumentTypes: 'IUnknown*';
		defineFunction: #get_SeamlessTabbing:
			argumentTypes: 'varbool*';
		defineFunction: #put_SeamlessTabbing:
			argumentTypes: 'varbool';
		defineFunction: #EnforceLocalSecurity;
		defineFunction: #get_Profile:
			argumentTypes: 'varbool*';
		defineFunction: #put_Profile:
			argumentTypes: 'varbool';
		defineFunction: #get_ProfileAddress:
			argumentTypes: 'bstr*';
		defineFunction: #put_ProfileAddress:
			argumentTypes: 'bstr';
		defineFunction: #get_ProfilePort:
			argumentTypes: 'sdword*';
		defineFunction: #put_ProfilePort:
			argumentTypes: 'sdword';
		defineFunction: #CallFunction:response:
			argumentTypes: 'bstr bstr*';
		defineFunction: #SetReturnValue:
			argumentTypes: 'bstr';
		defineFunction: #DisableLocalSecurity;
		defineFunction: #get_AllowNetworking:
			argumentTypes: 'bstr*';
		defineFunction: #put_AllowNetworking:
			argumentTypes: 'bstr';
		defineFunction: #get_AllowFullScreen:
			argumentTypes: 'bstr*';
		defineFunction: #put_AllowFullScreen:
			argumentTypes: 'bstr'
!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		IShockwaveFlash initializeTypeLib"

	typeLib := ShockwaveFlashObjectsLib! !
!IShockwaveFlash class categoriesFor: #clsid!**auto generated**!constants!private! !
!IShockwaveFlash class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!IShockwaveFlash class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

ShockwaveFlashView guid: (GUID fromString: '{DE288559-9D2C-4DD8-88D0-B9D534F16E4F}')!
ShockwaveFlashView comment: ''!
!ShockwaveFlashView categoriesForClass!MVP-Views! !
!ShockwaveFlashView methodsFor!

binaryStoreControlOnIStream: target 

	self
		displayValue: '#';
		displayValue: ''.
	^super binaryStoreControlOnIStream: target!

bridge

	bridge isNil ifTrue: [self createBridge].
	^bridge!

bridgeClass
bridgeClass isNil ifTrue: [bridgeClass  := self class defaultBridgeClass ].
	^bridgeClass!

bridgeClass: anActionScriptBridgeOrSubclass 
	bridgeClass := anActionScriptBridgeOrSubclass.
	bridge := nil!

createBridge

	bridge := self bridgeClass onView: self!

createControl

	super createControl.
	self displayValue: self value.
	"Ensure that bridge is created/updated"
	self updateBridge!

defaultProgId
	"Answer the 'prog id' of the Active-X control to be hosted in the receiver by default (i.e. initially
	and when no other prog id is specified)."

	^'ShockwaveFlash.ShockwaveFlash'!

displayValue
	Transcript
		show: self displayString , '>>#displayValue';
		cr.
	"Private - Answers the displayed contents of the receiver"

	^self controlDispatch movie!

displayValue: anObject 
	"Private - Set the displayed contents of the receiver from anObject."

[self maskFpeWhile: [self controlDispatch movie: anObject]]
	
			on: HRESULTError
			do: 
				[:e | 
				e hresult = DISP_E_MEMBERNOTFOUND 
					ifTrue: 
						["Suppress the error, the value is read-only and cannot be set"
						]
					ifFalse: [e pass]]!

maskFpeWhile: aBlock 
	| oldMask |
	[[
	oldMask := Processor activeProcess fpeMask: (Processor activeProcess fpeMask bitOr: _EM_INVALID).
	aBlock value] ensure: [Processor activeProcess fpeMask: oldMask]] 
			on: FloatingPointException
			do: [:ex | ]!

recreateControl

	super recreateControl.	"Ensure that bridge is created/updated"
	self updateBridge!

safeCreateControlFromStream: pStream 
	| moviePath |
	super safeCreateControlFromStream: pStream.	"Ensure that bridge is created/updated"
	self updateBridge.
	moviePath := self value.
	(moviePath beginsWith: '.\') 
		ifTrue: 
			[| path |
			path := File splitPathFrom: SessionManager current installationDirectory.
			moviePath := File composePath: path subPath: moviePath].
	self displayValue: moviePath!

updateBridge

	self bridge setView: self! !
!ShockwaveFlashView categoriesFor: #binaryStoreControlOnIStream:!binary filing!private! !
!ShockwaveFlashView categoriesFor: #bridge!accessing!public! !
!ShockwaveFlashView categoriesFor: #bridgeClass!accessing!private! !
!ShockwaveFlashView categoriesFor: #bridgeClass:!accessing!private! !
!ShockwaveFlashView categoriesFor: #createBridge!accessing!helpers!private! !
!ShockwaveFlashView categoriesFor: #createControl!private!realizing/unrealizing! !
!ShockwaveFlashView categoriesFor: #defaultProgId!constants!public! !
!ShockwaveFlashView categoriesFor: #displayValue!private!updating! !
!ShockwaveFlashView categoriesFor: #displayValue:!private!updating! !
!ShockwaveFlashView categoriesFor: #maskFpeWhile:!private!updating! !
!ShockwaveFlashView categoriesFor: #recreateControl!private!realizing/unrealizing! !
!ShockwaveFlashView categoriesFor: #safeCreateControlFromStream:!helpers!private! !
!ShockwaveFlashView categoriesFor: #updateBridge!accessing!helpers!private! !

!ShockwaveFlashView class methodsFor!

defaultBridgeClass
^ActionScriptBridge!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\flash.ico'!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| bridgeClasses |
	bridgeClasses := (Array with: ActionScriptBridge) , ActionScriptBridge allSubclasses.
	^(super publishedAspectsOfInstances)
		add: (Aspect name: #bridgeClass chooseFrom: (bridgeClasses collect: [:each | each name displayString]));
		add: (Aspect name: #bridge);
		yourself!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShockwaveFlashView)  98 28 0 0 98 2 8 1140916224 1 416 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 416 0 3695393 8 'ShockwaveFlash.ShockwaveFlash' 984070 ##(Smalltalk.IShockwaveFlash)  0 0 0 0 918022 ##(Smalltalk.IDolphinAxHost)  0 0 722438 ##(Smalltalk.AXEventSink)  234 240 98 8 3917 8 #OnProgress: 301 8 #FSCommand:args: -1217 8 #OnReadyStateChange: 395 8 #FlashCall: 416 1049094 ##(Smalltalk.IConnectionPoint)  0 0 3 1378630 1 ##(Smalltalk.TKindDispatchAnalyzer)  590598 ##(Smalltalk.ITypeInfo)  0 0 525062 ##(Smalltalk.TYPEATTR)  0 896 864 0 1378630 2 ##(Smalltalk.AXTypeLibraryAnalyzer)  590342 ##(Smalltalk.ITypeLib2)  0 0 2049 524550 ##(Smalltalk.TLIBATTR)  8 #[107 219 124 210 109 174 207 17 150 184 68 69 83 84 0 0 0 0 0 0 1 0 0 0 1 0 0 0 8 0 0 0] 8 '' 8 'ShockwaveFlashObjects' 8 #ShockwaveFlashObjectsLib 1056 234 240 98 2 8 'GUID' 8 #GUID 0 0 262198 ##(Smalltalk.GUID)  16 109 219 124 210 109 174 207 17 150 184 68 69 83 84 0 0 5 0 0 0 1 2761 234 240 98 0 0 524806 ##(Smalltalk.IUnknown)  0 0 0 0 0 852486 ##(Smalltalk.NullConverter)  0 0 8 ##(Smalltalk.ActionScriptBridge)  1298 416 32 983302 ##(Smalltalk.MessageSequence)  202 208 98 4 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 1458 201 201 416 1394 8 #restoreAmbientProperties 1216 416 1394 8 #docHostFlags: 98 1 9 416 1394 8 #controlBinaryStoreBytes: 98 1 8 #[110 219 124 210 109 174 207 17 150 184 68 69 83 84 0 0 103 85 102 85 16 7 0 0 86 10 0 0 86 10 0 0 8 0 2 0 0 0 0 0 8 0 0 0 0 0 8 0 0 0 0 0 8 0 14 0 0 0 87 0 105 0 110 0 100 0 111 0 119 0 0 0 11 0 255 255 11 0 255 255 8 0 10 0 0 0 72 0 105 0 103 0 104 0 0 0 8 0 2 0 0 0 0 0 11 0 255 255 8 0 0 0 0 0 8 0 2 0 0 0 0 0 8 0 16 0 0 0 83 0 104 0 111 0 119 0 65 0 108 0 108 0 0 0 11 0 0 0 11 0 0 0 8 0 2 0 0 0 0 0 8 0 0 0 0 0 8 0 2 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 11 0 1 0 11 0 0 0 8 0 0 0 0 0 3 0 0 0 0 0 8 0 8 0 0 0 97 0 108 0 108 0 0 0 8 0 12 0 0 0 102 0 97 0 108 0 115 0 101 0 0 0] 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 100 0 0 0 100 0 0 0] 98 0 1458 193 193 0 27 )!

youTube
	^self new value: 'http://www.youtube.com/apiplayer?enablejsapi=1'! !
!ShockwaveFlashView class categoriesFor: #defaultBridgeClass!constants!public! !
!ShockwaveFlashView class categoriesFor: #icon!development!public! !
!ShockwaveFlashView class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!ShockwaveFlashView class categoriesFor: #resource_Default_view!public!resources-views! !
!ShockwaveFlashView class categoriesFor: #youTube!instance creation!public! !

"Binary Globals"!

ShockwaveFlashObjectsLib := Object fromBinaryStoreBytes: 
(ByteArray fromBase64String: 'IVNUQiAzIEYJFQACAAAAQVhUeXBlTGlicmFyeUFuYWx5emVyBgIJAElUeXBlTGliMgAAAAAAAAAA
AQgAAAYBCABUTElCQVRUUnIAAAAgAAAAa9t80m2uzxGWuERFU1QAAAAAAAABAAAAAQAAAAgAAABS
AAAAAAAAAFIAAAAVAAAAU2hvY2t3YXZlRmxhc2hPYmplY3RzsgAAABgAAABTaG9ja3dhdmVGbGFz
aE9iamVjdHNMaWIAAgAA6gAAAPAAAABiAAAAAgAAAFIAAAAEAAAAR1VJRLIAAAAEAAAAR1VJRAAA
AAA=')!

