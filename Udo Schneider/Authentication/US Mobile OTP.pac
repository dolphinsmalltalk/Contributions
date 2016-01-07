| package |
package := Package name: 'US Mobile OTP'.
package paxVersion: 1;
	basicComment: '$id: US Mobile OTP 0.009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

See http://motp.sourceforge.net for details.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.009'.


package classNames
	add: #MobileOneTimePassword;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Steve Waring\Utilities\MD5\MD5';
	add: 'US Authentication';
	add: '..\Time\US Time Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Time Extensions').

package!

"Class Definitions"!

OneTimePassword subclass: #MobileOneTimePassword
	instanceVariableNames: 'secret pin tokenTimeDeviation'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

MobileOneTimePassword guid: (GUID fromString: '{332974A3-F52D-4B4A-A689-3F51BB62035D}')!
MobileOneTimePassword comment: ''!
!MobileOneTimePassword categoriesForClass!Kernel-Objects! !
!MobileOneTimePassword methodsFor!

allowedDeviation
	"+/- Deviation in minutes of allowed Password"

	^3!

basicVerify: password 
	^self basicVerify: password time: self otpTime + tokenTimeDeviation!

basicVerify: aString time: otpTime 
	| password deviationTime result |
	password := aString asLowercase.
	deviationTime := self allowedDeviation * 6.
	result := (self timeStampFromOtpTime: otpTime) -> false.
	(otpTime - deviationTime to: otpTime + deviationTime) detect: 
			[:time | 
			(result := (self timeStampFromOtpTime: time) -> (self otpTime: time generates: password)) value]
		ifNone: [].
	^result!

cacheTimeout
	"in minutes"

	^5!

calculateOtp: otpTime 
	| md5 hashData |
	hashData := otpTime displayString , secret , pin.
	md5 := self md5: hashData.
	^md5 copyFrom: 1 to: 6!

initialize
	super initialize.
	tokenTimeDeviation := 0!

md5: hashData 
	^((MD5 hashMessage: hashData) printStringRadix: 16 showRadix: false) asLowercase!

otpTime
	^self otpTimeFromTimeStamp: self currentTime!

otpTimeFromTimeStamp: aTimeStamp 
	^aTimeStamp asUnixEpocheSeconds // 10!

setPassphrase: aPassphraseString pin: aPinString 
	self setSecret: ((self md5: aPassphraseString) copyFrom: 1 to: 16) pin: aPinString!

setSecret: aSecretString pin: aPinString
secret := aSecretString.
pin := aPinString!

sync: password otpTime: otpTokenTime 
	| result |
	result := self basicVerify: password time: otpTokenTime.
	result value ifTrue: [tokenTimeDeviation := otpTokenTime - self otpTime].
	^result value!

timeStampFromOtpTime: ticks 
	^TimeStamp fromUnixEpocheSeconds: ticks * 10! !
!MobileOneTimePassword categoriesFor: #allowedDeviation!constants!public! !
!MobileOneTimePassword categoriesFor: #basicVerify:!helpers!private! !
!MobileOneTimePassword categoriesFor: #basicVerify:time:!helpers!private! !
!MobileOneTimePassword categoriesFor: #cacheTimeout!constants!public! !
!MobileOneTimePassword categoriesFor: #calculateOtp:!helpers!private! !
!MobileOneTimePassword categoriesFor: #initialize!initialization!private! !
!MobileOneTimePassword categoriesFor: #md5:!helpers!private! !
!MobileOneTimePassword categoriesFor: #otpTime!helpers!private! !
!MobileOneTimePassword categoriesFor: #otpTimeFromTimeStamp:!helpers!private! !
!MobileOneTimePassword categoriesFor: #setPassphrase:pin:!initialization!private! !
!MobileOneTimePassword categoriesFor: #setSecret:pin:!initialization!private! !
!MobileOneTimePassword categoriesFor: #sync:otpTime:!public! !
!MobileOneTimePassword categoriesFor: #timeStampFromOtpTime:!helpers!private! !

!MobileOneTimePassword class methodsFor!

passphrase: passphrase pin: pin 
	^(self new )
		setPassphrase: passphrase pin: pin;
		yourself!

secret: secret pin: pin 
	"secret: init-secred from token (to init token: #**#)
	pin: User Pin"

	^(self  new )
		setSecret: secret pin: pin;
		yourself! !
!MobileOneTimePassword class categoriesFor: #passphrase:pin:!instance creation!public! !
!MobileOneTimePassword class categoriesFor: #secret:pin:!instance creation!public! !

"Binary Globals"!

