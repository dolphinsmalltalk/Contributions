| package |
package := Package name: 'US Time Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Time Extensions 0.010$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '0.010'.


package methodNames
	add: #Date -> #asDATE;
	add: #Locale -> #bias;
	add: #Time -> #asUTC;
	add: #TimeStamp -> #asDATE;
	add: #TimeStamp -> #asUnixEpocheSeconds;
	add: 'Time class' -> #fromHours:minutes:seconds:milliseconds:;
	add: 'Time class' -> #fromHours:minutes:seconds:millisecondsFraction:;
	add: 'Time class' -> #fromUtcHours:minutes:seconds:milliseconds:;
	add: 'Time class' -> #fromUtcHours:minutes:seconds:millisecondsFraction:;
	add: 'Time class' -> #fromUtcTime:;
	add: 'TimeStamp class' -> #currentUnixEpochSeconds;
	add: 'TimeStamp class' -> #fromUnixEpocheSeconds:;
	add: 'TimeStamp class' -> #unixEpoche;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Date methodsFor!

asDATE
^self asParameter asFloat! !
!Date categoriesFor: #asDATE!public! !

!Locale methodsFor!

bias
	^(self timeZoneInformation at: 2) bias! !
!Locale categoriesFor: #bias!public! !

!Time methodsFor!

asUTC

	^self addTime: (Time fromSeconds: (Locale userDefault bias) * 60)! !
!Time categoriesFor: #asUTC!public! !

!Time class methodsFor!

fromHours: hours minutes: minutes seconds: seconds milliseconds: milliseconds 
	^Time 
		fromMilliseconds: (((hours * 60 + minutes) * 60 + seconds) * 1000 + milliseconds) asInteger!

fromHours: hours minutes: minutes seconds: seconds millisecondsFraction: millisecondsFraction 
	^self 
		fromHours: hours
		minutes: minutes
		seconds: seconds
		milliseconds: (millisecondsFraction notNull 
				ifTrue: [millisecondsFraction / (10 raisedTo: millisecondsFraction log ceiling)]
				ifFalse: [0]) * 1000!

fromUtcHours: hours minutes: minutes seconds: seconds milliseconds: milliseconds 
	^Time fromUtcTime: (Time 
				fromMilliseconds: (((hours * 60 + minutes) * 60 + seconds) * 1000 + milliseconds )asInteger)!

fromUtcHours: hours minutes: minutes seconds: seconds millisecondsFraction: millisecondsFraction 
	^self 
		fromUtcHours: hours
		minutes: minutes
		seconds: seconds
		milliseconds: (millisecondsFraction notNull 
				ifTrue: [millisecondsFraction / (10 raisedTo: millisecondsFraction log ceiling)]
				ifFalse: [0]) * 1000!

fromUtcTime: aTime 
	^aTime subtractTime: (Time fromSeconds: Locale userDefault bias * 60)! !
!Time class categoriesFor: #fromHours:minutes:seconds:milliseconds:!public! !
!Time class categoriesFor: #fromHours:minutes:seconds:millisecondsFraction:!public! !
!Time class categoriesFor: #fromUtcHours:minutes:seconds:milliseconds:!public! !
!Time class categoriesFor: #fromUtcHours:minutes:seconds:millisecondsFraction:!public! !
!Time class categoriesFor: #fromUtcTime:!public! !

!TimeStamp methodsFor!

asDATE
^self asParameter asFloat!

asUnixEpocheSeconds
^self asSeconds - self class unixEpoche asSeconds.! !
!TimeStamp categoriesFor: #asDATE!public! !
!TimeStamp categoriesFor: #asUnixEpocheSeconds!public! !

!TimeStamp class methodsFor!

currentUnixEpochSeconds
	^self currentUTC asUnixEpocheSeconds!

fromUnixEpocheSeconds: seconds 
	^self fromSeconds: self unixEpoche asSeconds + seconds!

unixEpoche
	^##(self date: (Date 
				newDay: 1
				monthIndex: 1
				year: 1970)
		time: (Time 
				hours: 0
				minutes: 0
				seconds: 0))! !
!TimeStamp class categoriesFor: #currentUnixEpochSeconds!public! !
!TimeStamp class categoriesFor: #fromUnixEpocheSeconds:!public! !
!TimeStamp class categoriesFor: #unixEpoche!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

