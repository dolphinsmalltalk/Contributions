| package |
package := Package name: 'US MessageLogger'.
package paxVersion: 1;
	basicComment: '$id: US MessageLogger 0.002$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.002'.


package classNames
	add: #MessageLogger;
	yourself.

package methodNames
	add: #Object -> #__messageSend:;
	add: #Object -> #logSender;
	add: #Object -> #logSender:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

ProtoObject subclass: #MessageLogger
	instanceVariableNames: 'subject messages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Object methodsFor!

__messageSend: anInteger 
	| frame |
	frame := Processor activeProcess topFrame sender.
	anInteger timesRepeat: [frame := frame sender].
	^MessageSend 
		receiver: frame receiver
		selector: frame method selector
		arguments: frame arguments!

logSender
	| messageSendSelf messageSendSuper |
	messageSendSelf := self __messageSend: 1.
	messageSendSuper := self __messageSend: 2.
	Transcript
		show: messageSendSelf receiver class name , '>>' , messageSendSelf selector , ' called from ' 
					, messageSendSuper receiver class name , '>>' 
					, messageSendSuper selector;
		cr!

logSender: prefix 
	| messageSendSelf messageSendSuper |
	messageSendSelf := self __messageSend: 1.
	messageSendSuper := self __messageSend: 2.
	Transcript
		show: prefix , ': ' , messageSendSelf receiver class name , '>>' , messageSendSelf selector 
					, ' called from ' , messageSendSuper receiver class name 
					, '>>' , messageSendSuper selector;
		cr! !
!Object categoriesFor: #__messageSend:!public! !
!Object categoriesFor: #logSender!public! !
!Object categoriesFor: #logSender:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

MessageLogger guid: (GUID fromString: '{46E0B72B-20D8-4203-856A-9C350BA9B201}')!
MessageLogger comment: ''!
!MessageLogger categoriesForClass!Unclassified! !
!MessageLogger methodsFor!

doesNotUnderstand: failedMessage 
	messages add: ((Time now) -> failedMessage).
	^self subject perform: failedMessage selector  withArguments: failedMessage  arguments!

initialize
	
	messages := OrderedCollection new.
	^self!

messages
^messages!

subject
^subject!

subject: anObject
subject := anObject! !
!MessageLogger categoriesFor: #doesNotUnderstand:!public! !
!MessageLogger categoriesFor: #initialize!public! !
!MessageLogger categoriesFor: #messages!public! !
!MessageLogger categoriesFor: #subject!public! !
!MessageLogger categoriesFor: #subject:!public! !

!MessageLogger class methodsFor!

new
^super new initialize!

on: subject 
	^(self new )
		subject: subject;
		yourself! !
!MessageLogger class categoriesFor: #new!public! !
!MessageLogger class categoriesFor: #on:!public! !

"Binary Globals"!

