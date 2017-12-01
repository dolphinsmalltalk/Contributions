"Filed out from Dolphin Smalltalk 7"!

Magnitude subclass: #UnicodeCharacter
	instanceVariableNames: 'codePoint'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UnicodeCharacter guid: (GUID fromString: '{af4ea814-0517-4e7d-9261-2c6a60375a8e}')!
UnicodeCharacter comment: ''!
!UnicodeCharacter categoriesForClass!Unclassified! !
!UnicodeCharacter methodsFor!

= comperand 
	^comperand equalsCharacter: self!

asCharacter
	self codePoint > 255 ifTrue: [self error: 'Can''t convert to Character'].
	^Character value: self codePoint!

asInteger
^self codePoint!

asLowercase
	"Answer a <Character> which is the lowercase equivalent of the receiver.
	If the receiver is already lowercase, then answer the receiver unchanged.
	Implementation Note: Apart from the ANSI asLowercase character mappings
	(basically all the uppercase letters are mapped to lowercase letters), this
	implementation will map other characters, depending on the configured
	locale of the host OS."

	^Character value: (UserLibrary default charLowerW: self asParameter )!

asParameter
	"Answer the receiver in a form suitable for passing to an external function
	primitive method (see ExternalLibrary and subclasses). The default is self."

	^self asInteger!

asString
^self asUnicodeString asString!

asUnicode
	^self!

asUnicodeCharacter
	^self!

asUnicodeString
^UnicodeString with: self!

asUppercase
	"Answer a <Character> which is the uppercase equivalent of the receiver.
	If the receiver is already uppercase, then answer the receiver unchanged.
	Implementation Note: Apart from the ANSI asUppercase character mappings
	(basically all the lowercase letters are mapped to uppercase letters), this
	implementation will map other characters, depending on the configured
	locale of the host OS."

	^Character  value: (UserLibrary default charUpperW: self asParameter)!

codePoint
	^codePoint!

equalsCharacter: comparand 
	
	^self asInteger = comparand asInteger!

hash
	"Answer the SmallInteger hash value for the receiver."

	^self asInteger!

isAlphaNumeric
	"Answer whether the receiver is a letter or a digit."

	^UserLibrary default isCharAlphaNumericW: self asParameter!

isLetter
	"Answer whether the receiver is an alphabetic character."

	^UserLibrary default isCharAlphaW: self asParameter!

isLowercase
	"Answer whether the receiver is a lowercase letter."

	^UserLibrary default isCharLowerW: self asParameter!

isUnicodeCharacter
	^true!

isUppercase
	"Answer whether the receiver is an uppercase letter."

	^UserLibrary default isCharUpperW: self asParameter!

printOn: target 
	self codePoint < 256 ifTrue: [^(Character value: self codePoint )printOn: target].
	target nextPutAll: 'U+'.
	self codePoint 
		printOn: target
		base: 16
		showRadix: false!

setCodePoint: anInteger
codePoint := anInteger! !
!UnicodeCharacter categoriesFor: #=!comparing!public! !
!UnicodeCharacter categoriesFor: #asCharacter!converting!public! !
!UnicodeCharacter categoriesFor: #asInteger!converting!public! !
!UnicodeCharacter categoriesFor: #asLowercase!converting!public! !
!UnicodeCharacter categoriesFor: #asParameter!converting!public! !
!UnicodeCharacter categoriesFor: #asString!converting!public! !
!UnicodeCharacter categoriesFor: #asUnicode!converting!public! !
!UnicodeCharacter categoriesFor: #asUnicodeCharacter!converting!public! !
!UnicodeCharacter categoriesFor: #asUnicodeString!converting!public! !
!UnicodeCharacter categoriesFor: #asUppercase!converting!public! !
!UnicodeCharacter categoriesFor: #codePoint!accessing!public! !
!UnicodeCharacter categoriesFor: #equalsCharacter:!comparing!public! !
!UnicodeCharacter categoriesFor: #hash!comparing!public! !
!UnicodeCharacter categoriesFor: #isAlphaNumeric!public!testing! !
!UnicodeCharacter categoriesFor: #isLetter!public!testing! !
!UnicodeCharacter categoriesFor: #isLowercase!public!testing! !
!UnicodeCharacter categoriesFor: #isUnicodeCharacter!public!testing! !
!UnicodeCharacter categoriesFor: #isUppercase!public!testing! !
!UnicodeCharacter categoriesFor: #printOn:!printing!public! !
!UnicodeCharacter categoriesFor: #setCodePoint:!private! !

!UnicodeCharacter class methodsFor!

icon
	^##(Icon fromFile: 'unicode.ico'
		usingLocator: (FolderRelativeFileLocator 
				basePath: (FileLocator imageRelative localFileSpecFor: 'Udo Schneider\Goodies\Resources\')))!

value: anInteger 
	^self new setCodePoint: anInteger; yourself! !
!UnicodeCharacter class categoriesFor: #icon!public! !
!UnicodeCharacter class categoriesFor: #value:!public! !

