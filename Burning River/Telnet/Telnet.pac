| package |
package := Package name: 'Telnet'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #TelnetConstants;
	add: #TelnetNegativeOption;
	add: #TelnetNVT;
	add: #TelnetOption;
	add: #TelnetRFC;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\Sockets\Sockets Connection';
	yourself).

package!

"Class Definitions"!

Object subclass: #TelnetConstants
	instanceVariableNames: ''
	classVariableNames: 'Current OptionDictionary'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #TelnetOption
	instanceVariableNames: 'id us usq him himq nvt'
	classVariableNames: 'Names'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #TelnetRFC
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #TelnetNVT
	instanceVariableNames: 'host socket options supportedOptions readProcess commandActions isDebugging lineTerminator captureStream waitForString waitForScore waitForSemaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TelnetOption subclass: #TelnetNegativeOption
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

TelnetConstants guid: (GUID fromString: '{2F1ED001-FF5C-11D3-82A2-00001D19F5C2}')!
TelnetConstants comment: ''!
!TelnetConstants categoriesForClass!Unclassified! !
!TelnetConstants methodsFor!

AO
	^245!

AYT
	^246!

BEL
	^7!

BRK
	^243!

BS
	^8!

CR
	^13!

defaultPort
	^23!

DM
	^242!

DO
	^253!

DONT
	^254!

EC
	^247!

ECHO
	^1!

EL
	^248!

EXTENDED_OPTIONS_LIST
	^255!

FF
	^12!

GA
	^249!

HT
	^9!

IAC
	^255!

IP
	^244!

LF
	^10!

NOP
	^241!

NUL
	^0!

SB
	^250!

SE
	^240!

STATUS
	^5!

SUPPRESS_GO_AHEAD
	^3!

TIMING_MARK
	^6!

TRANSMIT_BINARY
	^0!

VT
	^11!

WILL
	^251!

WONT
	^252! !
!TelnetConstants categoriesFor: #AO!constants!public! !
!TelnetConstants categoriesFor: #AYT!constants!public! !
!TelnetConstants categoriesFor: #BEL!constants!public! !
!TelnetConstants categoriesFor: #BRK!constants!public! !
!TelnetConstants categoriesFor: #BS!constants!public! !
!TelnetConstants categoriesFor: #CR!constants!public! !
!TelnetConstants categoriesFor: #defaultPort!constants!public! !
!TelnetConstants categoriesFor: #DM!constants!public! !
!TelnetConstants categoriesFor: #DO!constants!public! !
!TelnetConstants categoriesFor: #DONT!constants!public! !
!TelnetConstants categoriesFor: #EC!constants!public! !
!TelnetConstants categoriesFor: #ECHO!constants!public! !
!TelnetConstants categoriesFor: #EL!constants!public! !
!TelnetConstants categoriesFor: #EXTENDED_OPTIONS_LIST!constants!public! !
!TelnetConstants categoriesFor: #FF!constants!public! !
!TelnetConstants categoriesFor: #GA!constants!public! !
!TelnetConstants categoriesFor: #HT!constants!public! !
!TelnetConstants categoriesFor: #IAC!constants!public! !
!TelnetConstants categoriesFor: #IP!constants!public! !
!TelnetConstants categoriesFor: #LF!constants!public! !
!TelnetConstants categoriesFor: #NOP!constants!public! !
!TelnetConstants categoriesFor: #NUL!constants!public! !
!TelnetConstants categoriesFor: #SB!constants!public! !
!TelnetConstants categoriesFor: #SE!constants!public! !
!TelnetConstants categoriesFor: #STATUS!constants!public! !
!TelnetConstants categoriesFor: #SUPPRESS_GO_AHEAD!constants!public! !
!TelnetConstants categoriesFor: #TIMING_MARK!constants!public! !
!TelnetConstants categoriesFor: #TRANSMIT_BINARY!constants!public! !
!TelnetConstants categoriesFor: #VT!constants!public! !
!TelnetConstants categoriesFor: #WILL!constants!public! !
!TelnetConstants categoriesFor: #WONT!constants!public! !

!TelnetConstants class methodsFor!

current
	Current isNil ifTrue: [ Current := self new ].
	^Current!

initializeOptionDictionary
	OptionDictionary := Dictionary new.
	OptionDictionary
		at: 240 put: 'SE';
		at: 241 put: 'NOP';
		at: 242 put: 'Data Mark';
		at: 243 put: 'Break';
		at: 244 put: 'Interrupt Process';
		at: 245 put: 'Abort output';
		at: 246 put: 'Are you there?';
		at: 247 put: 'Erase character';
		at: 248 put: 'Erase Line';
		at: 249 put: 'Go ahead';
		at: 250 put: 'SB';
		at: 251 put: 'WILL';
		at: 252 put: 'WONT';
		at: 253 put: 'DO';
		at: 254 put: 'DONT';
		at: 255 put: 'IAC'!

optionDictionary
	OptionDictionary isNil ifTrue: [ self initializeOptionDictionary ].
	^OptionDictionary!

printStringFor: anInteger
	"Answer the string name of the given value, if any.  If none, answer the argument as a printable string."

	^self optionDictionary at: anInteger ifAbsent: [ anInteger printString ]! !
!TelnetConstants class categoriesFor: #current!accessing!public! !
!TelnetConstants class categoriesFor: #initializeOptionDictionary!initialization!private! !
!TelnetConstants class categoriesFor: #optionDictionary!accessing!public! !
!TelnetConstants class categoriesFor: #printStringFor:!printing!public! !

TelnetOption guid: (GUID fromString: '{93B785D1-00E7-11D4-82A2-00001D19F5C2}')!
TelnetOption comment: 'This class represents the state of a Telnet option, and provides methods for negotiating changes in state.  The ''got'' series of public messages (#gotDo, #gotDont, #gotWill, #gotWont) are intended to be invoked by a TelnetNVT or its subclasses when a DO, DONT, WILL, or WONT message is received from the opposite end of a Telnet connection.  The #sendDo, #sendDont, #sendWill, and #sendWont messages are used to send appropriate requests to the opposite end of the connection.'!
!TelnetOption categoriesForClass!Unclassified! !
!TelnetOption methodsFor!

gotDo
	"Invokes #gotDono, #gotDoyes, #gotDowantno, and #gotDowantyes"
	self perform: ('gotDo', us asString) asSymbol!

gotDono
	(self nvt will: self)
		ifTrue: [ us := #yes.  usq := #none.  self nvt sendWill: self ]
		ifFalse: [ self nvt sendWont: self ]!

gotDont
	"Invokes #gotDontno, #gotDontyes, #gotDontwantno, and #gotDontwantyes"
	self perform: ('gotDont', us asString) asSymbol!

gotDontno
	us == #yes ifTrue: [ us := #no.  usq := #none. self nvt sendWont: self ].
	us == #wantyes ifTrue: [
		usq == #empty ifTrue: [ us := #no. usq := #none ].
		usq == #opposite ifTrue: [
			us := #wantyes.  usq := #empty.  self nvt sendWill: self ] ].
	us == #wantno ifTrue: [
		usq == #empty ifTrue: [ us := #no. usq := #none ].
		usq == #opposite ifTrue: [ us := #no.  usq := #none ] ]!

gotDontwantno
	usq == #empty ifTrue: [ us := #no. usq := #none ].
	usq == #opposite ifTrue: [ us := #wantyes.  usq := #empty.  self nvt sendWill: self ]!

gotDontwantyes
	usq == #empty ifTrue: [ us := #no.  usq := #none ].
	usq == #opposite ifTrue: [ us := #no.  usq := #none ]!

gotDontyes
	usq == #empty ifTrue: [ us := #no. usq := #none ].
	usq == #opposite ifTrue: [
		us := #wantyes.  usq := #empty.  self nvt sendWill: self ]!

gotDowantno
	usq == #empty ifTrue: [ us := #no.  usq := #none ].
	usq == #opposite ifTrue: [ us := #yes.  usq := #empty ]!

gotDowantyes
	usq == #empty ifTrue: [ us := #yes. usq := #none ].
	usq == #opposite ifTrue: [
		us := #wantno.  usq := #empty.  self nvt sendWont: self ]!

gotDoyes
	"Ignore"!

gotWill
	"Invokes #gotWillno, #gotWillyes, #gotWillwantno, and #gotWillwantyes"
	self perform: ('gotWill', him asString) asSymbol!

gotWillno
	(self nvt will: self)
		ifTrue: [ him := #yes.  himq := #none.  self nvt sendDo: self ]
		ifFalse: [ self nvt sendDont: self ]!

gotWillwantno
	himq == #empty ifTrue: [ him := #no.  himq := #none ].
	himq == #opposite ifTrue: [ him := #yes.  himq := #empty ]!

gotWillwantyes
	himq == #empty ifTrue: [ him := #yes. himq := #none ].
	himq == #opposite ifTrue: [
		him := #wantno.  himq := #empty.  self nvt sendDont: self ]!

gotWillyes
	"Ignore"!

gotWont
	"Invokes #gotWontno, #gotWontyes, #gotWontwantno, and #gotWontwantyes"
	self perform: ('gotWont', him asString) asSymbol!

gotWontno
	him == #yes ifTrue: [ him := #no. himq := #none.  self nvt sendDont: self ].
	him == #wantyes ifTrue: [
		himq == #empty ifTrue: [ him := #no. himq := #none ].
		himq == #opposite ifTrue: [
			him := #wantyes.  himq := #empty.  self nvt sendDo: self ] ].
	him == #wantno ifTrue: [
		himq == #empty ifTrue: [ him := #no. himq := #none ].
		himq == #opposite ifTrue: [ him := #no.  himq := #none ] ]!

gotWontwantno
	himq == #empty ifTrue: [ him := #no. himq := #none ].
	himq == #opposite ifTrue: [ him := #wantyes.  himq := #empty.  self nvt sendDo: self ]!

gotWontwantyes
	himq == #empty ifTrue: [ him := #no.  himq := #none ].
	himq == #opposite ifTrue: [ him := #no.  himq := #none ]!

gotWontyes
	himq == #empty ifTrue: [ him := #no. himq := #none ].
	himq == #opposite ifTrue: [
		him := #wantyes.  himq := #empty.  self nvt sendDo: self ]!

id
	^id!

id: anInteger
	id := anInteger!

initialize
	super initialize.
	us := #no.
	usq := #none.
	him := #no.
	himq := #none.!

name
	^TelnetOption nameFor: self id!

nvt
	^nvt!

nvt: anNvt
	nvt := anNvt!

sendDo
	"Invokes #sendDono, #sendDoyes, #sendDowantno, and #sendDowantyes"
	self perform: ('sendDo', him asString) asSymbol!

sendDono
	him := #wantyes.
	himq := #empty.
	self nvt sendDo: self!

sendDont
	"Invokes #sendDontno, #sendDontyes, #sendDontwantno, and #sendDontwantyes"
	self perform: ('sendDont', him asString) asSymbol!

sendDontno
	self error: 'Already disabled'!

sendDontwantno
	himq == #empty ifTrue: [ self error: 'Already negotiating for disable' ].
	himq == #opposite ifTrue: [ himq == #empty ]!

sendDontwantyes
	himq == #empty ifTrue: [ self error: 'Cannot initiate new request in the middle of negotiation' ].
	himq == #opposite ifTrue: [ self error: 'Already queued a disable request' ]!

sendDontyes
	him := #wantno.
	himq := #empty.
	self nvt sendDont: self!

sendDowantno
	himq == #empty ifTrue: [ self error: 'Cannot initiate new request in the middle of negotiation' ].
	himq == #opposite ifTrue: [self error: 'Already queued an enable request' ]!

sendDowantyes
	himq == #empty ifTrue: [ self error: 'Already negotiating for enable' ].
	himq == #opposite ifTrue: [ himq := #empty ]!

sendDoyes
	self error: 'Already enabled'!

sendWill
	"Invokes #sendWillno, #sendWillyes, #sendWillwantno, and #sendWillwantyes"
	self perform: ('sendWill', us asString) asSymbol!

sendWillno
	us := #wantyes.
	usq := #empty.
	self nvt sendWill: self!

sendWillwantno
	usq == #empty ifTrue: [ self error: 'Cannot initiate new request in the middle of negotiation' ].
	usq == #opposite ifTrue: [self error: 'Already queued an enable request' ]!

sendWillwantyes
	usq == #empty ifTrue: [ self error: 'Already negotiating for enable' ].
	usq == #opposite ifTrue: [ usq := #empty ]!

sendWillyes
	self error: 'Already enabled'!

sendWont
	"Invokes #sendWontno, #sendWontyes, #sendWontwantno, and #sendWontwantyes"
	self perform: ('sendWont', us asString) asSymbol!

sendWontno
	self error: 'Already disabled'!

sendWontwantno
	usq == #empty ifTrue: [ self error: 'Already negotiating for disable' ].
	usq == #opposite ifTrue: [ usq == #empty ]!

sendWontwantyes
	usq == #empty ifTrue: [ self error: 'Cannot initiate new request in the middle of negotiation' ].
	usq == #opposite ifTrue: [ self error: 'Already queued a disable request' ]!

sendWontyes
	usq := #wantno.
	usq := #empty.
	self nvt sendWont: self!

theyAre
	"Answer true if the other end is currently performing this option (us = #yes).  Otherwise answer false."

	^him = #yes!

weAre
	"Answer true if we are currently performing this option (us = #yes).  Otherwise answer false."

	^us = #yes! !
!TelnetOption categoriesFor: #gotDo!public! !
!TelnetOption categoriesFor: #gotDono!private! !
!TelnetOption categoriesFor: #gotDont!public! !
!TelnetOption categoriesFor: #gotDontno!private! !
!TelnetOption categoriesFor: #gotDontwantno!private! !
!TelnetOption categoriesFor: #gotDontwantyes!private! !
!TelnetOption categoriesFor: #gotDontyes!private! !
!TelnetOption categoriesFor: #gotDowantno!private! !
!TelnetOption categoriesFor: #gotDowantyes!private! !
!TelnetOption categoriesFor: #gotDoyes!private! !
!TelnetOption categoriesFor: #gotWill!public! !
!TelnetOption categoriesFor: #gotWillno!private! !
!TelnetOption categoriesFor: #gotWillwantno!private! !
!TelnetOption categoriesFor: #gotWillwantyes!private! !
!TelnetOption categoriesFor: #gotWillyes!private! !
!TelnetOption categoriesFor: #gotWont!public! !
!TelnetOption categoriesFor: #gotWontno!private! !
!TelnetOption categoriesFor: #gotWontwantno!private! !
!TelnetOption categoriesFor: #gotWontwantyes!private! !
!TelnetOption categoriesFor: #gotWontyes!private! !
!TelnetOption categoriesFor: #id!public! !
!TelnetOption categoriesFor: #id:!public! !
!TelnetOption categoriesFor: #initialize!public! !
!TelnetOption categoriesFor: #name!public! !
!TelnetOption categoriesFor: #nvt!public! !
!TelnetOption categoriesFor: #nvt:!public! !
!TelnetOption categoriesFor: #sendDo!public! !
!TelnetOption categoriesFor: #sendDono!private! !
!TelnetOption categoriesFor: #sendDont!public! !
!TelnetOption categoriesFor: #sendDontno!private! !
!TelnetOption categoriesFor: #sendDontwantno!private! !
!TelnetOption categoriesFor: #sendDontwantyes!private! !
!TelnetOption categoriesFor: #sendDontyes!private! !
!TelnetOption categoriesFor: #sendDowantno!private! !
!TelnetOption categoriesFor: #sendDowantyes!private! !
!TelnetOption categoriesFor: #sendDoyes!private! !
!TelnetOption categoriesFor: #sendWill!public! !
!TelnetOption categoriesFor: #sendWillno!private! !
!TelnetOption categoriesFor: #sendWillwantno!private! !
!TelnetOption categoriesFor: #sendWillwantyes!private! !
!TelnetOption categoriesFor: #sendWillyes!private! !
!TelnetOption categoriesFor: #sendWont!public! !
!TelnetOption categoriesFor: #sendWontno!private! !
!TelnetOption categoriesFor: #sendWontwantno!private! !
!TelnetOption categoriesFor: #sendWontwantyes!private! !
!TelnetOption categoriesFor: #sendWontyes!private! !
!TelnetOption categoriesFor: #theyAre!public! !
!TelnetOption categoriesFor: #weAre!public! !

!TelnetOption class methodsFor!

initializeNames
	Names := Dictionary new.
	Names
		at: 0 put: 'TRANSMIT-BINARY';
		at: 1 put: 'ECHO';
		at: 3 put: 'SUPPRESS-GO-AHEAD';
		at: 5 put: 'STATUS';
		at: 6 put: 'TIMING-MARK';
		at: 255 put: 'EXTENDED-OPTIONS-LIST';
		at: 25 put: 'END-OF-RECORD';
		at: 26 put: 'TUID';
		at: 27 put: 'OUTMRK';
		at: 28 put: 'TTYLOC';
		at: 29 put: '3270-REGIME';
		at: 31 put: 'NAWS';
		at: 32 put: 'TERMINAL-SPEED';
		at: 24 put: 'TERMINAL-TYPE';
		at: 35 put: 'X-DISPLAY-LOCATION';
		at: 257 put: 'SUBLIMINAL-MESSAGE';
		at: 33 put: 'TOGGLE-FLOW-CONTROL';
		at: 36 put: 'ENVIRON';
		at: 37 put: 'AUTHENTICATION';
		at: 39 put: 'NEW-ENVIRON';
		at: 42 put: 'CHARSET';
		at: 44 put: 'COM-PORT-OPTION'!

nameFor: anInteger
	"Answer the printable string name for the given option ID"

	^self names at: anInteger ifAbsent: [ anInteger printString ]!

names
	Names isNil ifTrue: [ self initializeNames ].
	^Names!

nvt: anNvt id: anInteger
	^super new initialize
		nvt: anNvt;
		id: anInteger! !
!TelnetOption class categoriesFor: #initializeNames!public! !
!TelnetOption class categoriesFor: #nameFor:!public! !
!TelnetOption class categoriesFor: #names!public! !
!TelnetOption class categoriesFor: #nvt:id:!public! !

TelnetRFC guid: (GUID fromString: '{0B6BE5B1-7E9C-11D4-BDF2-00010240D5E2}')!
TelnetRFC comment: 'Defines the message transformation(s) required by one of the Telnet RFC''s.'!
!TelnetRFC categoriesForClass!Unclassified! !
TelnetNVT guid: (GUID fromString: '{2F1ED000-FF5C-11D3-82A2-00001D19F5C2}')!
TelnetNVT comment: 'This class implements the functionality of a basic Telnet device, known as a Network Virtual Terminal or NVT.  See Internet RFC 854 (Telnet protocol specification) and associated RFC''s (including but not limited to 855, 856, 857, 858, 859, 860, 861, 885, 927, 933, 946, 1041, 1073, 1079, 1091, 1096, 1097, 1143, 1372, 1408, 1411, 1412, 1416, 1571, 1572, 2066, and 2217) and/or any RFC''s which obsolete any of the above for more complete specifications.

At present this class and associated classes use the Dolphin socket classes directly.  At some point I intend to create an wrapper class (e.g. TelnetSocket) to wrap the vendor-supplied socket classes in order to simplify porting.'!
!TelnetNVT categoriesForClass!Unclassified! !
!TelnetNVT methodsFor!

basicPrintCharacter: aCharacter
	((aCharacter isPrintable or: [ aCharacter = Character cr ]) or: [ aCharacter = Character nl ])
		ifTrue: [ Transcript nextPut: aCharacter ]
		ifFalse: [ Transcript show: '(', aCharacter asInteger printString, ')' ]!

captureStream
	"Private - Answer the value of the receiver's ''captureStream'' instance variable."

	^captureStream!

captureStream: aStream
	"Private - initialize the stream to which incoming data should be written."

	self captureStream notNil ifTrue: [ self captureStream close ].
	captureStream := aStream!

captureTo: aStream
	"Begin capturing all data received from the server to the given stream."

	self captureStream: aStream!

commandActions
	"Private"
	commandActions isNil ifTrue: [ self initializeCommandActions ].
	^commandActions!

commandAo: aStream
	"Private"
	self printDebugString: 'AO|'.
	#todo!

commandAyt: aStream
	"Private"
	self printDebugString: 'AYT|'.
	#todo!

commandBrk: aStream
	"Private"
	self printDebugString: 'BRK|'.
	#todo!

commandDm: aStream
	"Private"
	self printDebugString: 'DM|'.
	#todo!

commandDo: aStream
	"Private"
	| char opt |

	char := aStream next.
	(opt := self supportedOptions at: char ifAbsent: [ nil ]) isNil ifTrue: [ 
		"Create a negative option and add it to supportedOptions"
		opt := TelnetNegativeOption nvt: self id: char.
		self supportedOptions at: char put: opt ].

	self printDebugString: 'DO|', opt name, '|'.
	opt gotDo!

commandDont: aStream
	"Private"
	| char opt |

	char := aStream next.
	(opt := self supportedOptions at: char ifAbsent: [ nil ]) isNil ifTrue: [ 
		"Create a negative option and add it to supportedOptions"
		opt := TelnetNegativeOption nvt: self id: char.
		self supportedOptions at: char put: opt ].

	self printDebugString: 'DONT|', opt name, '|'.
	opt gotDont
!

commandEc: aStream
	"Private"
	self printDebugString: 'EC|'.
	#todo!

commandEl: aStream
	"Private"
	self printDebugString: 'EL|'.
	#todo!

commandGa: aStream
	"Private"

	self printDebugString: 'GA|'.

	"Since we're not locking the keyboard until a go-ahead
	 is received we really don't need to do anything here."!

commandIp: aStream
	"Private"
	self printDebugString: 'IP|'.
	#todo!

commandNop: aStream
	"Private"
	self printDebugString: 'NOP|'.
	#todo!

commandSb: aStream
	"Private"
	self printDebugString: 'SB|'.
	#todo!

commandSe: aStream
	"Private"
	self printDebugString: 'SE|'.
	#todo!

commandWill: aStream
	"Private"
	| char opt |

	char := aStream next.
	(opt := self supportedOptions at: char ifAbsent: [ nil ]) isNil ifTrue: [ 
		"Create a negative option and add it to supportedOptions"
		opt := TelnetNegativeOption nvt: self id: char.
		self supportedOptions at: char put: opt ].

	self printDebugString: 'WILL|', opt name, '|'.
	opt gotWill

!

commandWont: aStream
	"Private"
	| char opt |

	char := aStream next.
	(opt := self supportedOptions at: char ifAbsent: [ nil ]) isNil ifTrue: [ 
		"Create a negative option and add it to supportedOptions"
		opt := TelnetNegativeOption nvt: self id: char.
		self supportedOptions at: char put: opt ].

	self printDebugString: 'WONT|', opt name, '|'.
	opt gotWont
!

connect
	"Connect to the host specified."

	self socket connect.
	readProcess := [ self processData: self socket readStream ] fork.
	self postConnect.
!

disconnect
	readProcess notNil ifTrue: [
		readProcess kill.
		readProcess := nil ].
	self socket close!

flushPrinter
	Transcript flush!

goAheadSuppressed
	"Answer if we are suppressing go-aheads."
	| option |
	(option := self supportedOptions at: (TelnetConstants current SUPPRESS_GO_AHEAD) ifAbsent: [ nil ]) notNil
		ifTrue: [ ^option weAre ]
		ifFalse: [ ^false ]!

host
	^host!

host: aString
	host := aString!

initializeCommandActions
	"Private"

	| tc |

	tc := TelnetConstants current.

	commandActions := Dictionary new.
	commandActions
		add: (tc SE -> #commandSe:);
		add: (tc NOP -> #commandNop:);
		add: (tc DM -> #commandDm:);
		add: (tc BRK -> #commandBrk:);
		add: (tc IP -> #commandIp:);
		add: (tc AO -> #commandAo:);
		add: (tc AYT -> #commandAyt:);
		add: (tc EC -> #commandEc:);
		add: (tc EL -> #commandEl:);
		add: (tc GA -> #commandGa:);
		add: (tc SB -> #commandSb:);
		add: (tc WILL -> #commandWill:);
		add: (tc WONT -> #commandWont:);
		add: (tc DO -> #commandDo:);
		add: (tc DONT -> #commandDont:)!

initializeSupportedOptions
	"Private"
	| tc |

	tc := TelnetConstants current.

	self supportedOptions:
		(Dictionary new 
			add: (tc TRANSMIT_BINARY -> (TelnetOption nvt: self id: tc TRANSMIT_BINARY));
			add: (tc ECHO -> (TelnetOption nvt: self id: tc ECHO));
			add: (tc SUPPRESS_GO_AHEAD -> (TelnetOption nvt: self id: tc SUPPRESS_GO_AHEAD));
			add: (tc STATUS -> (TelnetOption nvt: self id: tc STATUS));
			add: (tc TIMING_MARK -> (TelnetOption nvt: self id: tc TIMING_MARK));
			yourself)!

isDebugging
	"Answer the value of the receiver's ''isDebugging'' instance variable."

	^isDebugging!

isDebugging: aBoolean
	"Set the value of the receiver's ''isDebugging'' instance variable to the argument, anObject."

	isDebugging := aBoolean!

lineTerminator
	"Answers the line termination string currently in use.  Lazy initializes to String lineDelimiter."

	lineTerminator isNil ifTrue: [ self lineTerminator: String lineDelimiter ].
	^lineTerminator!

lineTerminator: aString
	"Sets the line termination string to use."

	lineTerminator := aString!

options
	options isNil ifTrue: [ options := Dictionary new ].
	^options!

postConnect
	"A series of actions to perform following a successful connection to a host."

	| tc option |

	tc := TelnetConstants current.

	"Always offer to suppress go-aheads."

	(option := self supportedOptions at: (tc SUPPRESS_GO_AHEAD) ifAbsent: [ nil ]) notNil ifTrue: [
		option sendWill ]!

printCharacter: aCharacter
	self
		basicPrintCharacter: aCharacter;
		flushPrinter!

printDebugCharacter: aCharacter
	self isDebugging ifTrue: [ self basicPrintCharacter: aCharacter; flushPrinter ]!

printDebugString: aString
	self isDebugging ifTrue: [
		aString do: [ :aCharacter | self basicPrintCharacter: aCharacter ].
		self flushPrinter ]!

printString: aString
	aString do: [ :aCharacter | self basicPrintCharacter: aCharacter ].
	self flushPrinter !

processByte: aByte
	"This is the main data processing routine.  Override to do something useful with the data.  To
	 ensure proper functioning overrides should probably invoke this version (i.e. super processByte: )."

	| c |

	c := Character value: aByte.
	self captureStream notNil ifTrue: [ self captureStream nextPut: c ].
	self printDebugCharacter: c.

	"Handle waiting for a particular string to be received, if such has been requested."

	waitForString notNil ifTrue: [
		c = (waitForString at: waitForScore + 1)
			ifTrue: [
				waitForScore := waitForScore + 1.
				waitForScore = waitForString size ifTrue: [
					self waitForString: nil.
					self waitForSemaphore pulse ] ]
			ifFalse: [ waitForScore := 0 ] ]!

processCommand: aStream
	"Private - the next element in the stream is an IAC.  Process the command appropriately"

	| char opt selector |

	char := aStream next.
	char = TelnetConstants current IAC
		ifTrue: [
			self printDebugString: 'IAC|'.

			char := aStream next.

			selector := self commandActions at: char ifAbsent: [ nil ].
			selector notNil
				ifTrue: [ self perform: selector with: aStream ]
				ifFalse: [ self
						printDebugCharacter: char;
						printDebugCharacter: $| ] ]
		ifFalse: [ self
				printDebugCharacter: char;
				printDebugCharacter: $| ]!

processData: aStream
	"Private.  This method is forked as a separate process by #connect."
	[ [ aStream atEnd ] whileFalse: [
		aStream peek = TelnetConstants current IAC
			ifTrue: [ self processCommand: aStream ]
			ifFalse: [ self processByte: aStream next ] ] ] on: SocketClosed do: [ :e | "drop out gracelessly" ]!

send: aCollection
	"If the other end isn't going to echo back to us process this data locally now."

	(self theyAre: TelnetConstants current ECHO) ifFalse: [ aCollection do: [ :each | self processByte: each ] ].

	"Send the data in the collection given.  Note that elements of the
	collection must be values in the range 0-255 or #asByteArray may fail."

	self socket sendByteArray: aCollection asByteArray!

sendDo: aTelnetOption
	self sendIac: (ByteArray with: TelnetConstants current DO with: aTelnetOption id)!

sendDont: aTelnetOption
	self sendIac: (ByteArray with: TelnetConstants current DONT with: aTelnetOption id)!

sendGoAhead
	self goAheadSuppressed ifFalse: [ self sendIac: (ByteArray with: TelnetConstants current GA) ]!

sendIac: aByteArray
	"Send an IAC command suffixed with the contents of aByteArray"

	| command option |

	self isDebugging ifTrue: [
		self printDebugString: '<sent: IAC'.

		"If the outbound byte array is only one or two bytes (the common case of a command or a
		 command-option pair) try to decode them.  Otherwise just dump the values."

		aByteArray size = 2 ifTrue: [
			command := aByteArray at: 1.
			option := aByteArray at: 2.
			self printDebugCharacter: $|.
			self printDebugString: (TelnetConstants optionDictionary
								at: command ifAbsent: [ command printString ]).
			self printDebugCharacter: $|.
			self printDebugString: (TelnetOption nameFor: option) ].

		aByteArray size = 1 ifTrue: [
			command := aByteArray at: 1.
			self printDebugCharacter: $|.
			self printDebugString: (TelnetConstants optionDictionary
								at: command ifAbsent: [ command printString ]) ].

		(aByteArray size ~= 1) & (aByteArray size ~= 2) ifTrue: [
			aByteArray do: [ :aByte | self printDebugString: '|', aByte printString ] ].

		self printDebugCharacter: $> ].

	self send: (ByteArray with: TelnetConstants current IAC), aByteArray!

sendLine: aCollection
	"Sends the collection given, terminated with the line termination sequence currently in effect."

	self
		send: aCollection, self lineTerminator;
		sendGoAhead!

sendWill: aTelnetOption
	self sendIac: (ByteArray with: TelnetConstants current WILL with: aTelnetOption id)
!

sendWont: aTelnetOption
	self sendIac: (ByteArray with: TelnetConstants current WONT with: aTelnetOption id)!

socket
	socket isNil ifTrue: [ socket := Socket
							port: (TelnetConstants current defaultPort)
							address: (InternetAddress host: self host) ].
	^socket!

supportedOptions
	supportedOptions isNil ifTrue: [ self initializeSupportedOptions ].
	^supportedOptions!

supportedOptions: aDictionary
	supportedOptions := aDictionary!

theyAre: anInteger
	"Answer true if the other computer ise currently performing the given option, otherwise answer false"

	^(self will: (TelnetOption nvt: self id: anInteger)) and: [ (self supportedOptions at: anInteger) theyAre ]!

waitFor: aString
	"Wait until aString has been received."

	self waitForString: aString.
	self waitForSemaphore wait!

waitForSemaphore
	"Private - Answer the value of the receiver's ''waitForSemaphore'' instance variable."

	waitForSemaphore isNil ifTrue: [ waitForSemaphore := Semaphore new ].
	^waitForSemaphore!

waitForString: aString
	"Private - Set the string for which to wait.  Implies we should clear the current score, if any."

	waitForString := aString.
	waitForScore := 0!

weAre: anInteger
	"Answer true if we are currently performing the given option, otherwise answer false"

	^(self will: (TelnetOption nvt: self id: anInteger)) and: [ (self supportedOptions at: anInteger) weAre ]!

will: aTelnetOption
	"Answer true if we are willing to perform the given option, otherwise answer false"
	^self supportedOptions includesKey: aTelnetOption id! !
!TelnetNVT categoriesFor: #basicPrintCharacter:!printing!private! !
!TelnetNVT categoriesFor: #captureStream!accessing!private! !
!TelnetNVT categoriesFor: #captureStream:!accessing!private! !
!TelnetNVT categoriesFor: #captureTo:!file operations!public! !
!TelnetNVT categoriesFor: #commandActions!command registry!private! !
!TelnetNVT categoriesFor: #commandAo:!commands!private! !
!TelnetNVT categoriesFor: #commandAyt:!commands!private! !
!TelnetNVT categoriesFor: #commandBrk:!commands!private! !
!TelnetNVT categoriesFor: #commandDm:!commands!private! !
!TelnetNVT categoriesFor: #commandDo:!commands!private! !
!TelnetNVT categoriesFor: #commandDont:!commands!private! !
!TelnetNVT categoriesFor: #commandEc:!commands!private! !
!TelnetNVT categoriesFor: #commandEl:!commands!private! !
!TelnetNVT categoriesFor: #commandGa:!commands!private! !
!TelnetNVT categoriesFor: #commandIp:!commands!private! !
!TelnetNVT categoriesFor: #commandNop:!commands!private! !
!TelnetNVT categoriesFor: #commandSb:!commands!private! !
!TelnetNVT categoriesFor: #commandSe:!commands!private! !
!TelnetNVT categoriesFor: #commandWill:!commands!private! !
!TelnetNVT categoriesFor: #commandWont:!commands!private! !
!TelnetNVT categoriesFor: #connect!accessing!public! !
!TelnetNVT categoriesFor: #disconnect!accessing!public! !
!TelnetNVT categoriesFor: #flushPrinter!printing!private! !
!TelnetNVT categoriesFor: #goAheadSuppressed!accessing!public! !
!TelnetNVT categoriesFor: #host!accessing!public! !
!TelnetNVT categoriesFor: #host:!accessing!public! !
!TelnetNVT categoriesFor: #initializeCommandActions!command registry!private! !
!TelnetNVT categoriesFor: #initializeSupportedOptions!initialization!private! !
!TelnetNVT categoriesFor: #isDebugging!accessing!public! !
!TelnetNVT categoriesFor: #isDebugging:!accessing!public! !
!TelnetNVT categoriesFor: #lineTerminator!accessing!public! !
!TelnetNVT categoriesFor: #lineTerminator:!accessing!public! !
!TelnetNVT categoriesFor: #options!accessing!public! !
!TelnetNVT categoriesFor: #postConnect!operations!public! !
!TelnetNVT categoriesFor: #printCharacter:!printing!private! !
!TelnetNVT categoriesFor: #printDebugCharacter:!printing!private! !
!TelnetNVT categoriesFor: #printDebugString:!printing!private! !
!TelnetNVT categoriesFor: #printString:!printing!private! !
!TelnetNVT categoriesFor: #processByte:!operations!private! !
!TelnetNVT categoriesFor: #processCommand:!operations!private! !
!TelnetNVT categoriesFor: #processData:!operations!private! !
!TelnetNVT categoriesFor: #send:!operations!public! !
!TelnetNVT categoriesFor: #sendDo:!operations!public! !
!TelnetNVT categoriesFor: #sendDont:!operations!public! !
!TelnetNVT categoriesFor: #sendGoAhead!operations!public! !
!TelnetNVT categoriesFor: #sendIac:!operations!public! !
!TelnetNVT categoriesFor: #sendLine:!operations!public! !
!TelnetNVT categoriesFor: #sendWill:!operations!public! !
!TelnetNVT categoriesFor: #sendWont:!operations!public! !
!TelnetNVT categoriesFor: #socket!accessing!private! !
!TelnetNVT categoriesFor: #supportedOptions!accessing!public! !
!TelnetNVT categoriesFor: #supportedOptions:!accessing!public! !
!TelnetNVT categoriesFor: #theyAre:!operations!public!testing! !
!TelnetNVT categoriesFor: #waitFor:!operations!public! !
!TelnetNVT categoriesFor: #waitForSemaphore!accessing!private! !
!TelnetNVT categoriesFor: #waitForString:!accessing!private! !
!TelnetNVT categoriesFor: #weAre:!operations!public!testing! !
!TelnetNVT categoriesFor: #will:!operations!public! !

TelnetNegativeOption guid: (GUID fromString: '{8B677371-300F-11D4-82A9-00001D19F5C2}')!
TelnetNegativeOption comment: 'This class represents a "negative" option - i.e. one which will always respond in the negative.  If asked if it WILL do something, it responds DONT.  If told to DO something, it responds WONT.'!
!TelnetNegativeOption categoriesForClass!Unclassified! !
!TelnetNegativeOption methodsFor!

gotDo
	"Respond negatively (WONT)"
	self nvt sendWont: self.!

gotDont
	"Respond affirmatively (WONT)"
	self nvt sendWont: self!

gotWill
	self nvt sendDont: self!

gotWont
	self nvt sendDont: self! !
!TelnetNegativeOption categoriesFor: #gotDo!public! !
!TelnetNegativeOption categoriesFor: #gotDont!public! !
!TelnetNegativeOption categoriesFor: #gotWill!public! !
!TelnetNegativeOption categoriesFor: #gotWont!public! !

"Binary Globals"!

"Resources"!

