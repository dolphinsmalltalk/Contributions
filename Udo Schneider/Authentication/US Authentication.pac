| package |
package := Package name: 'US Authentication'.
package paxVersion: 1;
	basicComment: '$id: US Authentication 0.002$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.002'.


package classNames
	add: #Authenticator;
	add: #OneTimePassword;
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

Object subclass: #Authenticator
	instanceVariableNames: 'failedLogins isLocked'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Authenticator subclass: #OneTimePassword
	instanceVariableNames: 'usedOtps'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Authenticator guid: (GUID fromString: '{6E68DEB6-6FA9-408E-825D-F4534E135AD3}')!
Authenticator comment: ''!
!Authenticator categoriesForClass!Kernel-Objects! !
!Authenticator methodsFor!

basicVerify: password 
self subclassResponsibility!

cacheCheck: password 
	^false!

currentTime
	^TimeStamp currentUTC!

initialize
	super initialize.
	isLocked := false.
	failedLogins := 0!

isLocked
	^isLocked!

lock
	isLocked := true!

onAuthenticationFailed: password timestamp: aTimeStamp 
	failedLogins := failedLogins + 1.
	failedLogins > 7 ifTrue: [self lock]. ^false!

onOuthenticationSuccess: password timestamp: aTimeStamp 
	failedLogins := 0.
	^true!

unlock
	isLocked := false!

verify: password 
	| result |
	
	[isLocked ifTrue: [^#locked -> (self onAuthenticationFailed: password timestamp: self currentTime)].
	(self cacheCheck: password) 
		ifTrue: [^#replay -> (self onAuthenticationFailed: password timestamp: self currentTime)].
		result := self basicVerify: password.
	(result ) value 
		ifFalse: [^#failed -> self onAuthenticationFailed: password timestamp: result key]
		ifTrue: [^#success -> self onOuthenticationSuccess: password timestamp: result key]] 
			on: Error
			do: [:ex | ^#error -> false]! !
!Authenticator categoriesFor: #basicVerify:!helpers!private! !
!Authenticator categoriesFor: #cacheCheck:!helpers!private! !
!Authenticator categoriesFor: #currentTime!helpers!private! !
!Authenticator categoriesFor: #initialize!initialization!private! !
!Authenticator categoriesFor: #isLocked!public!testing! !
!Authenticator categoriesFor: #lock!public! !
!Authenticator categoriesFor: #onAuthenticationFailed:timestamp:!helpers!private! !
!Authenticator categoriesFor: #onOuthenticationSuccess:timestamp:!helpers!private! !
!Authenticator categoriesFor: #unlock!public! !
!Authenticator categoriesFor: #verify:!public! !

!Authenticator class methodsFor!

new
	^super new initialize! !
!Authenticator class categoriesFor: #new!instance creation!public! !

OneTimePassword guid: (GUID fromString: '{FF42CDF2-0D48-47B9-9FBA-5BC5C6CC2A90}')!
OneTimePassword comment: ''!
!OneTimePassword categoriesForClass!Kernel-Objects! !
!OneTimePassword methodsFor!

allowedDeviation
	"+/- Deviation in minutes of allowed Password"

	self subclassResponsibility!

basicVerify: password 
	self subclassResponsibility!

basicVerify: aString time: otpTime 
	self subclassResponsibility!

cacheCheck: password 
	self cacheClean.
	^usedOtps anySatisfy: [:each | each value = password]!

cacheClean
	| timestamp |
	timestamp := TimeStamp fromSeconds: self currentTime asSeconds - (self cacheTimeout * 60).
	usedOtps := usedOtps reject: [:each | each key < timestamp]!

cacheTimeout
	"in minutes"

	^5!

calculateOtp
	^self calculateOtp: self otpTime!

calculateOtp: otpTime 
	self subclassResponsibility!

initialize
	super initialize.
	usedOtps := Set new.
!

onAuthenticationFailed: password timestamp: aTimeStamp 
	usedOtps add: aTimeStamp -> password.
	^super onAuthenticationFailed: password timestamp: aTimeStamp!

onOuthenticationSuccess: password timestamp: aTimeStamp 
	usedOtps add: aTimeStamp -> password.
	^super onOuthenticationSuccess: password timestamp: aTimeStamp!

otpTime
	self subclassResponsibility!

otpTime: otpTime generates: password 
	^(self calculateOtp: otpTime) = password!

otpTimeFromTimeStamp: aTimeStamp 
	self subclassResponsibility!

timeStampFromOtpTime: ticks 
	self subclassResponsibility! !
!OneTimePassword categoriesFor: #allowedDeviation!constants!public! !
!OneTimePassword categoriesFor: #basicVerify:!helpers!private! !
!OneTimePassword categoriesFor: #basicVerify:time:!helpers!private! !
!OneTimePassword categoriesFor: #cacheCheck:!helpers!private! !
!OneTimePassword categoriesFor: #cacheClean!helpers!private! !
!OneTimePassword categoriesFor: #cacheTimeout!constants!public! !
!OneTimePassword categoriesFor: #calculateOtp!public! !
!OneTimePassword categoriesFor: #calculateOtp:!helpers!private! !
!OneTimePassword categoriesFor: #initialize!initialization!private! !
!OneTimePassword categoriesFor: #onAuthenticationFailed:timestamp:!helpers!private! !
!OneTimePassword categoriesFor: #onOuthenticationSuccess:timestamp:!helpers!private! !
!OneTimePassword categoriesFor: #otpTime!helpers!private! !
!OneTimePassword categoriesFor: #otpTime:generates:!helpers!private! !
!OneTimePassword categoriesFor: #otpTimeFromTimeStamp:!helpers!private! !
!OneTimePassword categoriesFor: #timeStampFromOtpTime:!helpers!private! !

"Binary Globals"!

