| package |
package := Package name: 'US RFC3339'.
package paxVersion: 1;
	basicComment: '$id: US RFC3339 1.305$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Convert from and to RFC 3339 Times and Dates

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.305'.


package methodNames
	add: #Date -> #rfc3339String;
	add: #Time -> #rfc3339String;
	add: #TimeStamp -> #rfc3339String;
	add: 'Date class' -> #fromRfc3339String:;
	add: 'Time class' -> #fromRfc3339String:;
	add: 'TimeStamp class' -> #fromRfc3339String:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Time\US Time Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Time Extensions').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Date methodsFor!

rfc3339String
	| stream |
	stream := ReadWriteStream on: String new.
	self printOn: stream format: 'yyyy-MM-dd'.
	^stream contents! !
!Date categoriesFor: #rfc3339String!public! !

!Date class methodsFor!

fromRfc3339String: aString
^self fromString: aString format: 'YYYY-MM-DD'! !
!Date class categoriesFor: #fromRfc3339String:!public! !

!Time methodsFor!

rfc3339String
	| stream bias |
	stream := ReadWriteStream on: String new.
	self printOn: stream format: 'HH:mm'.
	bias := Locale userDefault bias.
	bias isNull ifTrue: [stream nextPut: $Z].
	bias positive ifTrue: [stream nextPut: $+].
	bias negative ifTrue: [stream nextPut: $-].
	(Time fromSeconds: bias abs * 60) printOn: stream format: 'HH.mm'.
	^stream contents! !
!Time categoriesFor: #rfc3339String!public! !

!Time class methodsFor!

fromRfc3339String: aString 
	| time offset |
	(aString includes: $-) 
		ifTrue: 
			[time := aString copyFrom: 1 to: (aString indexOf: $-) - 1.
			offset := aString copyFrom: (aString indexOf: $-) + 1.
			time := Time fromUtcTime: (Time fromString: time).
			time := time addTime: (Time fromString: offset).
			^time].
	(aString includes: $+) 
		ifTrue: 
			[time := aString copyFrom: 1 to: (aString indexOf: $+) - 1.
			offset := aString copyFrom: (aString indexOf: $+) + 1.
			time := Time fromUtcTime: (Time fromString: time).
			time := time subtractTime: (Time fromString: offset).
			^time].
	time := aString copyFrom: 1 to: aString size - 1.
	offset := '00:00'.
	^Time fromUtcTime: (Time fromString: time)! !
!Time class categoriesFor: #fromRfc3339String:!public! !

!TimeStamp methodsFor!

rfc3339String
	 ^self date rfc3339String , 'T' , self time rfc3339String! !
!TimeStamp categoriesFor: #rfc3339String!public! !

!TimeStamp class methodsFor!

fromRfc3339String: aString

|tokens|
tokens := aString subStrings: 'T'.
^TimeStamp date: (Date fromRfc3339String: (tokens at: 1)) time: (Time fromRfc3339String: (tokens at: 2))! !
!TimeStamp class categoriesFor: #fromRfc3339String:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

