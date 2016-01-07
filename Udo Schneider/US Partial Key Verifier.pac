| package |
package := Package name: 'US Partial Key Verifier'.
package paxVersion: 1;
	basicComment: '$id: US Partial Key Verifier 0.008$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 26.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Based on http://www.brandonstaggs.com/2007/07/26/implementing-a-partial-serial-number-verification-system-in-delphi/

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.008'.


package classNames
	add: #ExampleVerifier;
	add: #PartialKeyVerifier;
	yourself.

package methodNames
	add: #ByteArray -> #eightToFive;
	add: #ByteArray -> #fiveToEight;
	add: #ByteArray -> #murmurHash2:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #PartialKeyVerifier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'blacklist'!
PartialKeyVerifier subclass: #ExampleVerifier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ByteArray methodsFor!

eightToFive
	| bitCount value source result |
	bitCount := 0.
	value := 0.
	source := self readStream.
	result := ReadWriteStream on: ByteArray new.
	[source atEnd] whileFalse: 
			[value := value + (source next bitShift: bitCount).
			bitCount := bitCount + 8.
			[bitCount >= 5] whileTrue: 
					[result nextPut: (value bitAnd: 2r11111).
					bitCount := bitCount - 5.
					value := value bitShift: -5]].
	bitCount > 0 ifTrue: [result nextPut: value].
	^result contents!

fiveToEight
	| bitCount value source result |
	bitCount := 0.
	value := 0.
	source := self readStream.
	result := ReadWriteStream on: ByteArray new.
	[source atEnd] whileFalse: 
			[[bitCount < 8] whileTrue: 
					[value := value + (source next bitShift: bitCount).
					bitCount := bitCount + 5].
			result nextPut: (value bitAnd: 16rFF).
			bitCount := bitCount - 8.
			value := value bitShift: -8].
	^result contents!

murmurHash2: seed 
	| m r len h offset k |
	m := 16r5BD1E995.
	r := 24.
	len := self size.
	h := seed raisedToInteger: len.
	offset := 0.
	[len >= 4] whileTrue: 
			[k := self dwordAtOffset: offset.
			k := k * m.
			k := k bitXor: (k bitShift: r negated).
			k := k * m.
			h := h * m.
			h := h bitXor: k.
			offset := offset + 4.
			len := len - 4].
	len > 0 
		ifTrue: 
			[len >= 3 ifTrue: [h := h bitXor: ((self at: offset + 3) bitShift: 16)].
			len >= 2 ifTrue: [h := h bitXor: ((self at: offset + 2) bitShift: 8)].
			len >= 1 ifTrue: [h := h bitXor: (self at: offset + 1)].
			h := h * m].
	h := h bitXor: (h bitShift: -13).
	h := h * m.
	h := h bitXor: (h bitShift: -15).
	^h bitAnd: 16rFFFFFFFF! !
!ByteArray categoriesFor: #eightToFive!public! !
!ByteArray categoriesFor: #fiveToEight!public! !
!ByteArray categoriesFor: #murmurHash2:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

PartialKeyVerifier guid: (GUID fromString: '{8A14BAEB-C8B9-4D78-BFBD-88058C05D3F8}')!
PartialKeyVerifier comment: ''!
!PartialKeyVerifier categoriesForClass!Kernel-Objects! !
!PartialKeyVerifier methodsFor!

allKeyBytes: seed 
	^self allKeyValues collect: 
			[:each | 
	
			self 
				calculateKeyByte: seed
				a: each first
				b: each second
				c: each third]!

allKeyValues
	self subclassResponsibility!

calculateChecksum: aByteArray 
	| left right |
	left := 16r56.
	right := 16rAF.
	aByteArray do: 
			[:eachByte | 
			right := right + eachByte .
			right > 16rFF ifTrue: [right := right - 16rFF].
			left := left + right.
			left > 16rFF ifTrue: [left := left - 16rFF]].
	^(left bitShift: 8) + right!

calculateKeyByte: seed a: a b: b c: c 
	| am bm |
	am := a \\ 25.
	bm := b \\ 3.

	^(a even 
		ifTrue: [((seed bitShift: am negated) bitAnd: 16rFF) bitXor: ((seed bitShift: bm negated) bitOr: c)]
		ifFalse: [((seed bitShift: am negated) bitAnd: 16rFF) bitXor: ((seed bitShift: bm negated) bitAnd: c)]) 
			bitAnd: 16rFF!

checkBlacklist: seed 
	^self class blacklist includes: seed!

checkKey: aByteArray 
	| seedBytes seed keyBytes checksumBytes checksum |
	
	[(self checkKeyLength: aByteArray) ifFalse: [^#invalid -> false].
	seedBytes := aByteArray copyFrom: 1 to: 4.
	seed := seedBytes dwordAtOffset: 0.
	keyBytes := aByteArray copyFrom: 5 to: aByteArray size - 2.
	checksumBytes := aByteArray copyFrom: aByteArray size - 1 to: aByteArray size.
	checksum := checksumBytes wordAtOffset: 0.
	(self 
		checkKeyChecksum: checksum
		seed: seedBytes
		key: keyBytes) ifFalse: [^#invalid -> false].
	(self checkBlacklist: seed) ifTrue: [#blacklisted -> false].
	(self checkKeys: seed bytes: keyBytes) ifFalse: [#phony -> false].
	^#valid -> true] 
			on: Error
			do: [:ex | ^#error -> false]!

checkKeyChecksum: checksum seed: seedBytes key: keyBytes 
	^checksum = (self calculateChecksum: seedBytes , keyBytes)!

checkKeyLength: aByteArray 
	^aByteArray size = (4 + self keyValues size + 2)!

checkKeys: seed bytes: keyBytes 
	| keyStream |
	keyStream := keyBytes readStream.
	^self keyValues allSatisfy: 
			[:each | 
			each isNil or: [
			(self 
				calculateKeyByte: seed
				a: each first
				b: each second
				c: each third) = keyStream next]]!

checkKeyString: aString 
	| key  |
	
	[key := ((aString asUppercase asArray select: [:each | self eightToFiveDic includes: each]) 
				collect: [:each | (self eightToFiveDic indexOf: each) - 1]) asByteArray 
				fiveToEight.
	^self checkKey: key] 
			on: Error
			do: [:ex | ^#error -> false]!

eightToFiveDic
	^'abcde2fg3hjk4mn5pq6rs7tuv8wx9yz0' asUppercase!

keyValues
self subclassResponsibility!

makeKey: serial 
	| seed result |

	seed := self seedXor bitXor: serial.
	result := ReadWriteStream on: ByteArray new.
	result nextDWORDPut: seed.
	(self allKeyBytes: seed) do: [:each | result nextPut: each].
	result nextWORDPut: (self calculateChecksum: result contents).
	
	^result contents!

makeKeyString: serial 
	| keyStream keyStringStream |
	keyStream := (self makeKey: serial) eightToFive readStream.
	keyStringStream := ReadWriteStream on: String new.
	[keyStream atEnd] whileFalse: 
			[(keyStream nextAvailable: 4) 
				do: [:each | keyStringStream nextPut: (self eightToFiveDic at: each + 1)].
			keyStream atEnd ifFalse: [keyStringStream nextPut: $-]].
	^keyStringStream contents!

seedXor
^0! !
!PartialKeyVerifier categoriesFor: #allKeyBytes:!helpers!must strip!private! !
!PartialKeyVerifier categoriesFor: #allKeyValues!constants!must strip!private! !
!PartialKeyVerifier categoriesFor: #calculateChecksum:!helpers!private! !
!PartialKeyVerifier categoriesFor: #calculateKeyByte:a:b:c:!helpers!private! !
!PartialKeyVerifier categoriesFor: #checkBlacklist:!helpers!private! !
!PartialKeyVerifier categoriesFor: #checkKey:!public! !
!PartialKeyVerifier categoriesFor: #checkKeyChecksum:seed:key:!helpers!private! !
!PartialKeyVerifier categoriesFor: #checkKeyLength:!helpers!private! !
!PartialKeyVerifier categoriesFor: #checkKeys:bytes:!helpers!private! !
!PartialKeyVerifier categoriesFor: #checkKeyString:!public! !
!PartialKeyVerifier categoriesFor: #eightToFiveDic!constants!private! !
!PartialKeyVerifier categoriesFor: #keyValues!constants!private! !
!PartialKeyVerifier categoriesFor: #makeKey:!public! !
!PartialKeyVerifier categoriesFor: #makeKeyString:!public! !
!PartialKeyVerifier categoriesFor: #seedXor!constants!private! !

!PartialKeyVerifier class methodsFor!

blacklist
blacklist isNil ifTrue: [blacklist  := Set new].
^blacklist!

icon
^Icon fromId: 428!

makeKeyStrings: anInterval
	| verifier keys |
	keys := LookupTable new: anInterval size.
	verifier := self new.
	anInterval do: [:serial |
		keys at: serial put: (verifier makeKeyString: serial)
	].
^keys! !
!PartialKeyVerifier class categoriesFor: #blacklist!public! !
!PartialKeyVerifier class categoriesFor: #icon!public! !
!PartialKeyVerifier class categoriesFor: #makeKeyStrings:!public! !

ExampleVerifier guid: (GUID fromString: '{505779D3-334F-4001-A54A-F7BD48FDD9CA}')!
ExampleVerifier comment: ''!
!ExampleVerifier categoriesForClass!Kernel-Objects! !
!ExampleVerifier methodsFor!

allKeyValues
	^#(#(24 3 200) #(10 0 56) #(1 2 91) #(7 1 100))!

keyValues
^#(#(24 3 200) #(10 0 56) #(1 2 91) #(7 1 100))! !
!ExampleVerifier categoriesFor: #allKeyValues!constants!must strip!private! !
!ExampleVerifier categoriesFor: #keyValues!constants!private! !

"Binary Globals"!

