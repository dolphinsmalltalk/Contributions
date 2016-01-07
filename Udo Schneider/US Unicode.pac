| package |
package := Package name: 'US Unicode'.
package paxVersion: 1;
	basicComment: '$id: US Unicode 0.023$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 20.09.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.023'.

package basicScriptAt: #preinstall put: 'Win32Constants at: ''CP_UTF8'' put: 65001'.

package classNames
	add: #UnicodeCharacter;
	yourself.

package methodNames
	add: #BSTR -> #at:put:;
	add: #Character -> #=;
	add: #Character -> #asCharacter;
	add: #Character -> #asUnicode;
	add: #Character -> #asUnicodeCharacter;
	add: #Character -> #asUnicodeString;
	add: #Character -> #equalsCharacter:;
	add: #Character -> #isUnicodeCharacter;
	add: #Object -> #equalsCharacter:;
	add: #Object -> #equalsString:;
	add: #Object -> #equalsUnicodeString:;
	add: #String -> #=;
	add: #String -> #asUnicode;
	add: #String -> #asUTF8Content;
	add: #String -> #asUTF8String;
	add: #String -> #at:put:;
	add: #String -> #equalsString:;
	add: #String -> #equalsUnicodeString:;
	add: #UnicodeString -> #=;
	add: #UnicodeString -> #asByteArray;
	add: #UnicodeString -> #asSymbol;
	add: #UnicodeString -> #asUnicode;
	add: #UnicodeString -> #asUTF8Content;
	add: #UnicodeString -> #asUTF8String;
	add: #UnicodeString -> #at:put:;
	add: #UnicodeString -> #displayString;
	add: #UnicodeString -> #equalsString:;
	add: #UnicodeString -> #equalsUnicodeString:;
	add: #UnicodeString -> #hash;
	add: #UnicodeString -> #printOn:;
	add: #UnicodeString -> #replaceBytesOf:from:to:startingAt:;
	add: #UnicodeString -> #shallowCopy;
	add: #UnicodeString -> #wordAtOffset:put:;
	add: #UserLibrary -> #charLowerW:;
	add: #UserLibrary -> #charUpperW:;
	add: #UserLibrary -> #isCharAlphaNumericW:;
	add: #UserLibrary -> #isCharAlphaW:;
	add: #UserLibrary -> #isCharLowerW:;
	add: #UserLibrary -> #isCharUpperW:;
	add: 'Character class' -> #value:;
	add: 'String class' -> #fromUTF8Content:;
	add: 'String class' -> #fromUTF8String:;
	add: 'UnicodeString class' -> #fromUTF8Content:;
	add: 'UnicodeString class' -> #fromUTF8String:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	yourself).

package!

"Class Definitions"!

Magnitude subclass: #UnicodeCharacter
	instanceVariableNames: 'codePoint'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!BSTR methodsFor!

at: anInteger put: aCharacter 
	"Replace the character at index, anInteger, in the receiver with aCharacter."

	| index |
	index := anInteger + anInteger.
	aCharacter asInteger > 65535 ifTrue: [Error signal: 'BSTR can only hold Characters with codePoint < 65536'].
	super
		at: index put: aCharacter asInteger // 256;
		at: index - 1 put: aCharacter asInteger \\ 256.
	^aCharacter! !
!BSTR categoriesFor: #at:put:!accessing!public! !

!Character methodsFor!

= comperand 
	^comperand equalsCharacter: self!

asCharacter
^self!

asUnicode
	^self asUnicodeCharacter!

asUnicodeCharacter
	^UnicodeCharacter value: self codePoint!

asUnicodeString
^UnicodeString with: self!

equalsCharacter: comparand 
	
	^self asInteger = comparand asInteger!

isUnicodeCharacter
^false! !
!Character categoriesFor: #=!comparing!public! !
!Character categoriesFor: #asCharacter!public! !
!Character categoriesFor: #asUnicode!public! !
!Character categoriesFor: #asUnicodeCharacter!public! !
!Character categoriesFor: #asUnicodeString!public! !
!Character categoriesFor: #equalsCharacter:!comparing!public! !
!Character categoriesFor: #isUnicodeCharacter!public!testing! !

!Character class methodsFor!

value: anInteger 
	"Answer the character with ascii value, anInteger. If anInteger is not in the range 0..255, 
	then #at: primitive will fail"
^anInteger <= 255 ifTrue: [CharacterSet at: anInteger + 1] ifFalse: [UnicodeCharacter value: anInteger]! !
!Character class categoriesFor: #value:!instance creation!public! !

!Object methodsFor!

equalsCharacter: comparand 
	^false!

equalsString: comparand 
	^false!

equalsUnicodeString: comparand 
	^false! !
!Object categoriesFor: #equalsCharacter:!comparing!public! !
!Object categoriesFor: #equalsString:!comparing!public! !
!Object categoriesFor: #equalsUnicodeString:!comparing!public! !

!String methodsFor!

= comparand 
	^comparand equalsString: self!

asUnicode
^self asUnicodeString!

asUTF8Content
	^(UnicodeString fromString: self) asUTF8Content!

asUTF8String
#deprecated.
^self asUTF8Content!

at: anInteger put: aCharacter 
	"Replace the character at index, anInteger, in the receiver with aCharacter.

	Primitive failure reasons:
		0 -	anInteger is not a SmallInteger
		1 -	anInteger is out of bounds (not in the range 1..receiver's size).
		2 -	aCharacter is not a Character."

	<primitive: 64>
	^(aCharacter isKindOf: Character) 
		ifTrue: [self errorAt: anInteger put: aCharacter]
		ifFalse: 
			[(aCharacter isKindOf: UnicodeCharacter) 
				ifTrue: 
					[self
						become: (self asUnicodeString );
						at: anInteger put: aCharacter]
				ifFalse: [self error: 'can''t hold ' , aCharacter class name , '''s']]!

equalsString: comparand 
	"Answer whether the receiver and the <Object> argument, comparand, 
	are both Strings containing identical characters (i.e. case sensitive).

	Primitive failure results:
		0 -	aString is not a byte object of the same class as the receiver."

	<primitive: 55>
	^false!

equalsUnicodeString: comparand 
	^self asUnicodeString = comparand! !
!String categoriesFor: #=!comparing!public! !
!String categoriesFor: #asUnicode!converting!public! !
!String categoriesFor: #asUTF8Content!converting!public! !
!String categoriesFor: #asUTF8String!converting!public! !
!String categoriesFor: #at:put:!accessing!public! !
!String categoriesFor: #equalsString:!comparing!public! !
!String categoriesFor: #equalsUnicodeString:!comparing!public! !

!String class methodsFor!

fromUTF8Content: aByteArrayOrString 
	^(UnicodeString fromUTF8Content: aByteArrayOrString) !

fromUTF8String: aString 
	#deprecated.	^self fromUTF8Content: aString! !
!String class categoriesFor: #fromUTF8Content:!instance creation!must not strip!public! !
!String class categoriesFor: #fromUTF8String:!instance creation!must not strip!public! !

!UnicodeString methodsFor!

= comparand 
	^comparand equalsUnicodeString: self!

asByteArray
	"Answer a <ByteArray> containing the Unicode representation of the characters of the
	receiver."

	^ByteArray fromAddress: self yourAddress length: self basicSize!

asSymbol
^self asString asSymbol!

asUnicode
^self!

asUTF8Content
	"Answer an UTF8 representation of the receiver."

	| buf size bytes |
	size := self size.
	buf := String new: size * 8.
	size == 0 ifTrue: [^buf].	"Avoid 'The Parameter is Incorrect' error"
	bytes := KernelLibrary default 
				wideCharToMultiByte: CP_UTF8
				dwFlags: 0
				lpWideCharStr: self
				cchWideChar: size
				lpMultiByteStr: buf
				cchMultiByte: buf size
				lpDefaultChar: nil
				lpUsedDefaultChar: nil.
	bytes == 0 ifTrue: [^KernelLibrary default systemError].
	^buf resize: bytes!

asUTF8String
	#deprecated.
	^self asUTF8Content!

at: anInteger put: aCharacter 
	"Replace the character at index, anInteger, in the receiver with aCharacter."

	| index |
	index := anInteger + anInteger.
	self
		basicAt: index put: aCharacter asInteger  // 256;
		basicAt: index - 1 put: aCharacter asInteger \\ 256.
	^aCharacter!

displayString
	"Answer a String representation of the receiver in a form suitable for
	presentation to an end user.
	Implementation Note: This is implemented purely for performance reasons to
	avoid the Stream overhead when displaying strings because it is such a
	common operation."

	^self asString!

equalsString: comparand 
	^self = comparand asUnicodeString!

equalsUnicodeString: comparand 
	"Answer whether the receiver and the <Object> argument, comparand, 
	are both Strings containing identical characters (i.e. case sensitive).

	Primitive failure results:
		0 -	aString is not a byte object of the same class as the receiver."

	<primitive: 55>
	^false!

hash
"Please note that on might loos 'precision' here. In theory it would be better
	to implement String>>#hash as '^self asUnicodeString' to keep the precision.
	
	However performance wise this is the better way"
	^self asString hash!

printOn: aStream 
	"Append the receiver as a quoted string to aStream. Internal quotes are doubled to produce a
	literal String. Null characters are printed as another character to avoid them being treated
	as null terminators by C api's"

	aStream nextPut: $@;nextPut: $'.
	1 to: self size
		do: 
			[:i | 
			| ch |
			(ch := self at: i) == ##(Character null) 
				ifTrue: [aStream nextPut: NullPrintCharacter]
				ifFalse: 
					[ch codePoint < 256 
						ifTrue: [(aStream nextPut: ch) == $' ifTrue: [aStream nextPut: $']]
						ifFalse: [aStream nextPutAll: ch displayString]]].
	aStream nextPut: $'!

replaceBytesOf: aByteObject from: start to: stop startingAt: fromStart 
	"Private - Standard method for transfering bytes from one variable
	byte object to another, normally double dispatched from #replaceFrom:to:with:startingAt:

	Primitive Failure Reasons:
		0 	- fromStart is not a SmallInteger.
		1	- stop is not a SmallInteger.
		2	- start is not a SmallInteger.
		3	- aByteObject is not a byte object
		4	- 'from' or 'to' interval is out-of-bounds
	"

	| fromOffset |
	fromOffset := fromStart - start.
	stop to: start
		by: -1
		do: [:i | aByteObject at: i put: (self at: i + fromOffset)].
	^aByteObject!

shallowCopy
	
	^UnicodeString fromAddress: self yourAddress length: self size!

wordAtOffset: anInteger put: anObject 
	"Store an unsigned 16-bit value at byte offset anInteger within the receiver.
	If anObject is not representable as a 16-bit unsigned (Small)Integer, then 
	raise a 'cannot hold' error.

	Primitive failure reasons:
		0 -	anInteger is not a SmallInteger.
		1 -	anInteger is out of bounds.
		2 -	anObject is not a SmallInteger
		3 -	anObject is not in the range -32768..32767 (i.e. out of signed 16-bit range)."

	<primitive: 125>
	self primitiveFailed! !
!UnicodeString categoriesFor: #=!comparing!public! !
!UnicodeString categoriesFor: #asByteArray!converting!public! !
!UnicodeString categoriesFor: #asSymbol!public! !
!UnicodeString categoriesFor: #asUnicode!converting!public! !
!UnicodeString categoriesFor: #asUTF8Content!converting!public! !
!UnicodeString categoriesFor: #asUTF8String!converting!public! !
!UnicodeString categoriesFor: #at:put:!accessing!public! !
!UnicodeString categoriesFor: #displayString!printing!public! !
!UnicodeString categoriesFor: #equalsString:!comparing!public! !
!UnicodeString categoriesFor: #equalsUnicodeString:!comparing!public! !
!UnicodeString categoriesFor: #hash!comparing!public! !
!UnicodeString categoriesFor: #printOn:!printing!public! !
!UnicodeString categoriesFor: #replaceBytesOf:from:to:startingAt:!double dispatch!private! !
!UnicodeString categoriesFor: #shallowCopy!copying!public! !
!UnicodeString categoriesFor: #wordAtOffset:put:!accessing!primitives!public! !

!UnicodeString class methodsFor!

fromUTF8Content: aByteArrayOrString 
	"Answer a new instance of the receiver containing the same characters as the <aByteArrayOrString>
	argument.
	Implementation Note: CP_ACP is the only code page supported by Win95."

	| answer answerSize |
	aByteArrayOrString isEmpty ifTrue: [^UnicodeString new].
	answer := self new: aByteArrayOrString size.
	(answerSize := KernelLibrary default 
				multiByteToWideChar: CP_UTF8
				dwFlags: 0
				lpMultiByteStr: aByteArrayOrString
				cchMultiByte: aByteArrayOrString size
				lpWideCharStr: answer
				cchWideChar: answer basicSize) == 0 
		ifTrue: [^KernelLibrary default systemError].
	^answer resize: answerSize!

fromUTF8String: aString 
	#deprecated.
	^self fromUTF8Content: aString! !
!UnicodeString class categoriesFor: #fromUTF8Content:!instance creation!must not strip!public! !
!UnicodeString class categoriesFor: #fromUTF8String:!instance creation!must not strip!public! !

!UserLibrary methodsFor!

charLowerW: aCharacter 
	"Answer the lowercase equivalent of aCharacter. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharLower(LPTSTR  lpsz); 	// single character or pointer to string"

	<stdcall: dword CharLowerW dword>
	^self invalidCall!

charUpperW: aCharacter 
	"Answer the uppercase equivalent of aCharacter. This will be dependent on the semantics 
	of the language selected by the user during setup or by using Control Panel.

		LPTSTR CharUpper(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: dword CharUpperW dword>
	^self invalidCall!

isCharAlphaNumericW: aCharacter 
	"Answer whether a character is an alphabetic character or a digit. This will dependent on the 
	semantics of the language selected by the user during setup or by using Control Panel.
		BOOL IsCharAlphaNumeric(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharAlphaNumericW dword>
	^self invalidCall!

isCharAlphaW: aCharacter 
	"Answer whether a character is an alphabetic character. This will dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
		BOOL IsCharAlpha(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharAlphaW dword>
	^self invalidCall!

isCharLowerW: aCharacter 
	"Answer whether a character is a lowercase letter. This will dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
		BOOL IsCharLower(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharLowerA dword>
	^self invalidCall!

isCharUpperW: aCharacter 
	"Answer whether a character is an uppercase letter. This will dependent on the semantics of 
	the language selected by the user during setup or by using Control Panel.
		BOOL IsCharUpper(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharUpperA dword>
	^self invalidCall! !
!UserLibrary categoriesFor: #charLowerW:!public!win32 functions-string manipulation! !
!UserLibrary categoriesFor: #charUpperW:!public!win32 functions-string manipulation! !
!UserLibrary categoriesFor: #isCharAlphaNumericW:!public!win32 functions-string manipulation! !
!UserLibrary categoriesFor: #isCharAlphaW:!public!win32 functions-string manipulation! !
!UserLibrary categoriesFor: #isCharLowerW:!public!win32 functions-string manipulation! !
!UserLibrary categoriesFor: #isCharUpperW:!public!win32 functions-string manipulation! !

"End of package definition"!

"Source Globals"!

"Classes"!

UnicodeCharacter guid: (GUID fromString: '{AF4EA814-0517-4E7D-9261-2C6A60375A8E}')!
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

"Binary Globals"!

