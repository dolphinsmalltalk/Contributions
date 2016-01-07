| package |
package := Package name: 'US Certification Authority Creator'.
package paxVersion: 1;
	basicComment: '$id: US Certification Authority Creator 0.012$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Usage:
creator := DolphinSureCertificationAuthorityCreator owner: ''Dolphin Map''.
creator create.

DolphinMapCertificate serial: 2 owner: ''Object Arts'' details: ''London UK''.

ret := DolphinMapPersonalCertificate serial: 3 owner: ''Udo Schneider'' details: ''Udo.Schneider@homeaddress.de''.
cert := ret first.
privKey := ret second.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.012'.


package classNames
	add: #DolphinSureCertificationAuthorityCreator;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\DolphinSure\DolphinSure';
	yourself).

package!

"Class Definitions"!

Object subclass: #DolphinSureCertificationAuthorityCreator
	instanceVariableNames: 'owner publicPackage privatePackage classNamePrefix caClassName caClass caKeyPair serialPrefix certClassName certClass certPersonalClassName certPersonalClass'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

DolphinSureCertificationAuthorityCreator guid: (GUID fromString: '{B762BD23-A22A-4CD9-B03D-3897C96D7FFE}')!
DolphinSureCertificationAuthorityCreator comment: ''!
!DolphinSureCertificationAuthorityCreator categoriesForClass!Unclassified! !
!DolphinSureCertificationAuthorityCreator methodsFor!

basicCreateCaClassMethodBasicCreateSerial
	SmalltalkSystem current 
		compile: 'basicCreateSerial: anInteger 
	^''' , self serialPrefix 
				, '%1!!06u!!'' formatWith: anInteger'
		in: self caClass class
		categories: #('helpers' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreateCaClassMethodGenerate
	SmalltalkSystem current 
		compile: 'generate
	"Private - Generates an instance of the receiver

        self generate
        "

	| cert info |
	info := DolphinSureCertificateInfo 
				serial: (self basicCreateSerial: 0)
				owner: ''' 
				, self owner 
					, ''' 
				publicKey: self publicKey.
	^(cert := self basicNew)
		publisherCertificate: cert;
		info: info privateKey: self privateKey'
		in: self caClass class
		categories: #('instance creation' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreateCaClassMethodNew
	SmalltalkSystem current 
		compile: 'new
	"Answers an instance of the receiver.

        Show an instance of the receiver using:
                self new show.

        The instance is regenerated from a binary store string. If the data content of the certificate changes then the binary store
        array contained in this method should be recreated (and pasted below) by displaying the following:

                self generate binaryStoreBytes.
        "

	^Object 
		fromBinaryStoreBytes: ' 
				, self caClass generate binaryStoreBytes displayString
		in: self caClass class
		categories: #('instance creation' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreateCaClassMethodPrivateKey
	SmalltalkSystem current 
		compile: 'privateKey
	"Private - Answer the ' , self caClassName 
				, ' private key.
        Obviously this should not be revealed to anyone

        Generate a private/public key pair by displaying the following:
                DigitalSignatureAlgorithm generateKeySetForGroup: self keyGroup

        Paste the first number (the private key) below and the second number (the public key) in
        the #publicKey method.
"

	^' 
					, self caKeyPair first displayString
		in: self caClass class
		categories: #('constants' 'private')
		package: self privatePackage
		extraFlags: 0!

basicCreateCaClassMethodPublicKey
	SmalltalkSystem current 
		compile: 'publicKey
	"Private - Answer the public key for ' , self caClassName , '"

	^' 
				, self caKeyPair second displayString
		in: self caClass class
		categories: #('constants' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreateCaClassMethodValidYears
	SmalltalkSystem current 
		compile: 'validYears
	"Private - Answer the number of years that certificates of this class should be valid
        for by default. Nil indicates valid indefinitely."

	^nil'
		in: self caClass class
		categories: #('constants' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreateCaMethodPublicKey
	SmalltalkSystem current 
		compile: 'publicKey
	"Answer the public key of the receiver"

	^self class publicKey'
		in: self caClass
		categories: #('accessing' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreateCaMethodPublisherPublicKey
	SmalltalkSystem current 
		compile: 'publisherPublicKey
	"Private - Answer the publisher''s public key"

	^self publicKey'
		in: self caClass
		categories: #('accessing' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertClassMethodInfo
	SmalltalkSystem current 
		compile: 'info: certificateInfo 
	"Answers an instance of the receiver"

	| cert |
	cert := self rootCertificateClass new.
	^self 
		info: certificateInfo
		authority: cert
		authorityPrivateKey: cert class privateKey'
		in: self certClass class
		categories: #('instance creation' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertClassMethodInfoAuthorityAuthorityPrivateKey
	SmalltalkSystem current 
		compile: 'info: certificateInfo authority: aCertificationAuthorityCertificate authorityPrivateKey: privateKey 
	"Answers an instance of the receiver"

	^(super publisherCertificate: aCertificationAuthorityCertificate) info: certificateInfo
		privateKey: privateKey'
		in: self certClass class
		categories: #('instance creation' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertClassMethodRootCertificateClass
	SmalltalkSystem current 
		compile: 'rootCertificateClass
	"This method must answer the class of the certification authority certificate"

	^' , self caClassName
		in: self certClass class
		categories: #('instance creation' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertClassMethodSerialOwnerDetails
	SmalltalkSystem current 
		compile: 'serial: serialNumber owner: owner details: details 
	"Answers a certificate definition given the above detail. This definition is presented in a Smalltalk
        workspace the contents of which contain the private/public keys for the certificate and the binary
        store bytes that can be used to regenerate the certificate itself.

        self serial: 2 owner: ''My Own Company Inc'' details: ''secur...@company.com''
        "

	| keys serial info cert validYears |
	keys := self signatureAlgorithm generateKeySetForGroup: self keyGroup.
	serial := self rootCertificateClass basicCreateSerial: serialNumber.
	info := DolphinSureCertificateInfo 
				serial: serial
				owner: owner
				publicKey: keys second.
	info details: details.
	(validYears := self validYears) notNil ifTrue: [info expiry: (info issued addYears: validYears)].

	"The next line actually encodes the certificate so may take some time"
	cert := self info: info.
	^Array with: cert with: keys first'
		in: self certClass class
		categories: #('operations' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertClassMethodValidYears
	SmalltalkSystem current 
		compile: 'validYears
	"Private - Answer the number of years that certificates of this class should be valid
        for by default. Nil indicates valid indefinitely."

	^nil'
		in: self certClass class
		categories: #('constants' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertMethodBackgroundImageFile
	SmalltalkSystem current 
		compile: 'backgroundImageFile
	"Private - Answers the resource name of the background image file
	for the receiver"

	^''Resources/DolphinSureCert.jpg'''
		in: self certClass
		categories: #('constants' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertMethodImageAttributes
	SmalltalkSystem current 
		compile: 'imageAttributes
	"Private - Answers an OrderedCollection detailing the attributes that should
	appear on a image displaying the certificate details"

	^##((OrderedCollection new)
		add: ''Serial no:'' -> #serial;
		add: ''Publisher:'' -> #owner;
		add: ''Details:'' -> #details;
		add: '''' -> #gap;
		add: ''Purpose:'' -> #purpose;
		add: '''' -> #gap;
		add: ''Validity:'' -> #validityString;
		add: ''Issued by:'' -> #issuingAuthority;
		yourself)'
		in: self certClass
		categories: #('drawing' 'helpers' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertMethodPurpose
	SmalltalkSystem current 
		compile: 'purpose
	"Answers the purpose of this certificate"

	^''This certificate ensures that the content is from a known publisher and guarantees that it has not been modified since it was originally created.'''
		in: self certClass
		categories: #('accessing' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreateCertMethodWorkspaceText
	SmalltalkSystem current 
		compile: 'workspaceText: privateKey 
	| stream bytes |
	stream := WriteStream on: String new.
	"Now write code to rebuild the certificate from it''s encoded bytes"
	stream 
		display: (''"' 
				, self owner 
					, ' DolphinSure certificate %1 for %2"

"Evaluate the following expressions in a workspace"

"Recreate the certificate from it''''s bytes"
certificate := Object fromBinaryStoreBytes: '' 
				formatWith: self serial
				with: self owner).
	bytes := self binaryStoreBytes.
	bytes printPrefixOn: stream.
	bytes do: 
			[:each | 
			stream
				display: each;
				display: '' ''].
	bytes printSuffixOn: stream.
	stream 
		display: (''.

"Show the certificate"
certificate show.

"Add it to your personal store"
DolphinSureCertificateStore myCertificates addCertificate: certificate.

"Retrieve it from your personal store"
(DolphinSureCertificateStore myCertificates atSerial: ''''%1'''' ifAbsent:
[]) show.

"If it is your default (first) certificate you can retrieve it like
this..."
DolphinSureCertificateStore myCertificates default show.

'' 
				formatWith: self serial).
	stream 
		display: (''"THIS IS YOUR PRIVATE KEY=%1 (DO NOT DISCLOSE)"

"Sign data with your certificate"
trustedData := certificate sign: ''''This is a secure message''''
asByteArray privateKey: %1.
trustedData validData asString "Display it".
trustedData data at: 1 put: 0 "Change the data".
trustedData validData asString "Now try to Display it"."'' 
				formatWith: privateKey displayString).
	^stream contents'
		in: self certClass
		categories: #('helpers' 'public')
		package: self publicPackage
		extraFlags: 0!

basicCreatePersonalCertMethodBackgroundImageFile
	SmalltalkSystem current 
		compile: 'backgroundImageFile
	"Private - Answers the resource name of the background image file
	for the receiver"

	^''Resources/DolphinSurePersonalCert.jpg'''
		in: self certPersonalClass
		categories: #('constants' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreatePersonalCertMethodImageAttributes
	SmalltalkSystem current 
		compile: 'imageAttributes
	"Private - Answers an OrderedCollection detailing the attributes that should
	appear on a image displaying the certificate details"

	^##((OrderedCollection new)
		add: ''Serial no:'' -> #serial;
		add: ''Issued to:'' -> #owner;
		add: ''Details:'' -> #details;
		add: '''' -> #gap;
		add: ''Purpose:'' -> #purpose;
		add: '''' -> #gap;
		add: ''Validity:'' -> #validityString;
		add: ''Issued by:'' -> #issuingAuthority;
		yourself)'
		in: self certPersonalClass
		categories: #('drawing' 'helpers' 'private')
		package: self publicPackage
		extraFlags: 0!

basicCreatePersonalCertMethodPurpose
	SmalltalkSystem current 
		compile: 'purpose
	"Answers the purpose of this certificate"

	^''This certificate ensures that the content is from a known individual and guarantees that it has not been modified since it was originally created.'''
		in: self certPersonalClass
		categories: #('accessing' 'public')
		package: self publicPackage
		extraFlags: 0!

caClass
	caClass 
		ifNil: 
			[
				caClass := Smalltalk at: self caClassName ifAbsent: [SmalltalkSystem current 
						createSubclass: self caClassName
						of: DolphinSureCertificationAuthorityCertificate
						package: self publicPackage
						subclasses: #()]
				 ].
	^caClass!

caClassName
	caClassName 
		ifNil: 
			[caClassName := (self classNamePrefix , 'CertificationAuthorityCertificate') asSymbol.
			(ClassBuilder isValidClassName: caClassName) 
				ifFalse: [self error: caClassName , ' is not a valid Class name. Please set manually!!']].
	^caClassName!

caClassName: aSymbol 
	caClassName := aSymbol!

caKeyPair
	caKeyPair 
		ifNil: [caKeyPair := DigitalSignatureAlgorithm generateKeySetForGroup: self caClass keyGroup].
	^caKeyPair!

certClass
	certClass 
		ifNil: 
			[certClass := 
			Smalltalk at: self certClassName ifAbsent: [SmalltalkSystem current 
						createSubclass: self certClassName
						of: DolphinSureCertificate
						package: self publicPackage
						subclasses: #()]
			].
	^certClass!

certClassName
	certClassName 
		ifNil: 
			[certClassName := (self classNamePrefix , 'Certificate') asSymbol.
			(ClassBuilder isValidClassName: certClassName) 
				ifFalse: [self error: certClassName , ' is not a valid Class name. Please set manually!!']].
	^certClassName!

certClassName: aSymbol 

	certClassName := aSymbol!

certPersonalClass
	certPersonalClass 
		ifNil: 
			[certPersonalClass := Smalltalk at: self certPersonalClassName
						ifAbsent: 
							[SmalltalkSystem current 
								createSubclass: self certPersonalClassName
								of: self certClass
								package: self publicPackage
								subclasses: #()]].
	^certPersonalClass!

certPersonalClassName
	certPersonalClassName 
		ifNil: 
			[certPersonalClassName := (self classNamePrefix , 'PersonalCertificate') asSymbol.
			(ClassBuilder isValidClassName: certPersonalClassName) 
				ifFalse: [self error: certPersonalClassName , ' is not a valid Class name. Please set manually!!']].
	^certPersonalClassName!

certPersonalClassName: aSymbol 
	certPersonalClassName := aSymbol!

classNamePrefix
	classNamePrefix 
		ifNil: 
			[classNamePrefix := (self owner subStrings inject: String new
						into: [:string :subString | string , subString capitalized]) select: [:each | each isAlphaNumeric]].
	^classNamePrefix!

createCaClassMethods
	self
		basicCreateCaClassMethodPrivateKey;
		basicCreateCaClassMethodPublicKey;
		basicCreateCaClassMethodValidYears;
		basicCreateCaClassMethodBasicCreateSerial;
		basicCreateCaClassMethodGenerate;
		basicCreateCaClassMethodNew!

createCaMethods
	self
		basicCreateCaMethodPublicKey;
		basicCreateCaMethodPublisherPublicKey!

createCertClassMethods
	self
		basicCreateCertClassMethodInfo;
		basicCreateCertClassMethodInfoAuthorityAuthorityPrivateKey;
		basicCreateCertClassMethodRootCertificateClass;
		basicCreateCertClassMethodSerialOwnerDetails;
		basicCreateCertClassMethodValidYears!

createCertMethods
	self
		basicCreateCertMethodBackgroundImageFile;
		basicCreateCertMethodImageAttributes;
		basicCreateCertMethodPurpose;
		basicCreateCertMethodWorkspaceText.
	self
		basicCreatePersonalCertMethodBackgroundImageFile;
		basicCreatePersonalCertMethodImageAttributes;
		basicCreatePersonalCertMethodPurpose!

generate
	self
		createCaMethods;
		createCaClassMethods.
	self
		createCertClassMethods;
		createCertMethods.
	SmalltalkSystem current browsePackages: (Array with: self publicPackage with: self privatePackage)!

owner
	^owner!

owner: aString 
	owner := aString!

privatePackage
	privatePackage 
		ifNil: 
			[| packageName |
			packageName := self owner , ' Certification Authority PRIVATE'.
			privatePackage := Package manager  packageNamed: packageName
						ifNone: [Package manager  newPackage: packageName]].
	^privatePackage!

publicPackage
	publicPackage 
		ifNil: 
			[| packageName |
			packageName := self owner , ' Certification Authority'.
			publicPackage := Package manager packageNamed: packageName
						ifNone: [Package manager  newPackage: packageName]].
	^publicPackage!

serialPrefix
	serialPrefix 
		ifNil: 
			[serialPrefix := self owner subStrings inject: String new
						into: [:string :subString | string , (subString leftString: 1) asUppercase]].
	^serialPrefix!

serialPrefix: aString 
	serialPrefix := aString!

stbSaveOn: anSTBOutFiler 
	"
	3.	to output a proxy in place of the receiver using
			STBOutFiler>>#saveObject: self as: anSTBProxy.
		The proxy will be sent a #fixup:at: message at load time (see STB classes) and should then
		answer the object it represents.

	By default, objects are happy to be saved and loaded just as they are (option 1)."

	anSTBOutFiler
		save: publicPackage as: nil;
		save: privatePackage as: nil;
		save: caClass as: nil; save: certClass as: nil; save: certPersonalClass as: nil;
		saveObject: self
		



! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCaClassMethodBasicCreateSerial!CA Class creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCaClassMethodGenerate!CA Class creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCaClassMethodNew!CA Class creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCaClassMethodPrivateKey!CA Class creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCaClassMethodPublicKey!CA Class creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCaClassMethodValidYears!CA Class creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCaMethodPublicKey!CA Class creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCaMethodPublisherPublicKey!CA Class creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertClassMethodInfo!Cert Class Creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertClassMethodInfoAuthorityAuthorityPrivateKey!Cert Class Creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertClassMethodRootCertificateClass!Cert Class Creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertClassMethodSerialOwnerDetails!Cert Class Creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertClassMethodValidYears!Cert Class Creation-class methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertMethodBackgroundImageFile!Cert Class Creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertMethodImageAttributes!Cert Class Creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertMethodPurpose!Cert Class Creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreateCertMethodWorkspaceText!Cert Class Creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreatePersonalCertMethodBackgroundImageFile!Cert Class Creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreatePersonalCertMethodImageAttributes!Cert Class Creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #basicCreatePersonalCertMethodPurpose!Cert Class Creation-instance methods!helpers!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #caClass!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #caClassName!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #caClassName:!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #caKeyPair!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #certClass!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #certClassName!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #certClassName:!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #certPersonalClass!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #certPersonalClassName!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #certPersonalClassName:!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #classNamePrefix!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #createCaClassMethods!CA Class creation-class methods!helpers!operations!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #createCaMethods!CA Class creation-instance methods!helpers!operations!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #createCertClassMethods!Cert Class Creation-class methods!helpers!operations!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #createCertMethods!Cert Class Creation-instance methods!helpers!operations!private! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #generate!operations!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #owner!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #owner:!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #privatePackage!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #publicPackage!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #serialPrefix!helpers!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #serialPrefix:!accessing!public! !
!DolphinSureCertificationAuthorityCreator categoriesFor: #stbSaveOn:!public! !

!DolphinSureCertificationAuthorityCreator class methodsFor!

icon
	^DolphinSureTrustedData icon!

odbTransientInstanceVariables
	"This method tells OmniBase which instance variables should not be stored into the database."

	^(super odbTransientInstanceVariables)
		add: 'publicPackage';
		add: 'privatePackage';
		add: 'caClass';
		add: 'certClass';
		add: 'certPersonalClass';
		yourself!

owner: aString 
	^(self new)
		owner: aString;
		yourself! !
!DolphinSureCertificationAuthorityCreator class categoriesFor: #icon!constants!development!public! !
!DolphinSureCertificationAuthorityCreator class categoriesFor: #odbTransientInstanceVariables!OmniBase!public! !
!DolphinSureCertificationAuthorityCreator class categoriesFor: #owner:!instance creation!public! !

"Binary Globals"!

