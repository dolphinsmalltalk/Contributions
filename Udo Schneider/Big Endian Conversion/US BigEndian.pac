| package |
package := Package name: 'US BigEndian'.
package paxVersion: 1;
	basicComment: '$id: US BigEndian 1.004$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Convert from little endian byte order (Intel style) to Big Endian/Network Byte Order

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.004'.


package classNames
	add: #BigEndianDWORD;
	add: #BigEndianDWORDField;
	add: #BigEndianQWORD;
	add: #BigEndianQWORDField;
	add: #BigEndianWORD;
	add: #BigEndianWORDField;
	yourself.

package methodNames
	add: #ByteArray -> #bigEndianDoubleAtOffset:;
	add: #ByteArray -> #bigEndianDoubleAtOffset:put:;
	add: #ByteArray -> #bigEndianDwordAtOffset:;
	add: #ByteArray -> #bigEndianDwordAtOffset:put:;
	add: #ByteArray -> #bigEndianFloatAtOffset:;
	add: #ByteArray -> #bigEndianFloatAtOffset:put:;
	add: #ByteArray -> #bigEndianQwordAtOffset:;
	add: #ByteArray -> #bigEndianQwordAtOffset:put:;
	add: #ByteArray -> #bigEndianWordAtOffset:;
	add: #ByteArray -> #bigEndianWordAtOffset:put:;
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

DWORDField subclass: #BigEndianDWORDField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
QWORDField subclass: #BigEndianQWORDField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WORDField subclass: #BigEndianWORDField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DWORD subclass: #BigEndianDWORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
QWORD subclass: #BigEndianQWORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WORD subclass: #BigEndianWORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ByteArray methodsFor!

bigEndianDoubleAtOffset: anInteger 
	^(self copyFrom: anInteger + 1 to: anInteger + 8) reverse doubleAtOffset: 0!

bigEndianDoubleAtOffset: anInteger put: anObject 
	| bytearray |
	bytearray := (ByteArray new: 8)
				doubleAtOffset: 0 put: anObject;
				reverse.
	self 
		replaceFrom: anInteger + 1
		to: anInteger + 8
		with: bytearray!

bigEndianDwordAtOffset: anInteger 
	^(self bigEndianWordAtOffset: anInteger) * 16r10000 
		+ (self bigEndianWordAtOffset: anInteger + 2)!

bigEndianDwordAtOffset: anInteger put: anObject 

	self bigEndianWordAtOffset: anInteger  put: anObject >> 16.
	self bigEndianWordAtOffset: anInteger +2 put: (anObject bitAnd: 16rFFFF)!

bigEndianFloatAtOffset: anInteger 
^((self copyFrom: anInteger +1 to: anInteger +4) reverse ) floatAtOffset: 0!

bigEndianFloatAtOffset: anInteger put: anObject 
	| bytearray |
	bytearray := (ByteArray new: 4)
				floatAtOffset: 0 put: anObject;
				reverse.
	self 
		replaceFrom: anInteger + 1
		to: anInteger + 4
		with: bytearray!

bigEndianQwordAtOffset: anInteger 
	^(self bigEndianDwordAtOffset: anInteger) * 16r100000000 
		+ (self bigEndianDwordAtOffset: anInteger + 4)!

bigEndianQwordAtOffset: anInteger put: anObject 
	self bigEndianDwordAtOffset: anInteger put: anObject >> 32.
	self bigEndianDwordAtOffset: anInteger + 4 put: (anObject bitAnd: 16rFFFFFFFF)!

bigEndianWordAtOffset: anInteger 
	^(self byteAtOffset: anInteger) *16r100 + (self byteAtOffset: anInteger +1)!

bigEndianWordAtOffset: anInteger put: anObject 
	self byteAtOffset: anInteger  put: anObject >> 8.
	self byteAtOffset: anInteger + 1 put: (anObject bitAnd: 16rFF)! !
!ByteArray categoriesFor: #bigEndianDoubleAtOffset:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianDoubleAtOffset:put:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianDwordAtOffset:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianDwordAtOffset:put:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianFloatAtOffset:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianFloatAtOffset:put:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianQwordAtOffset:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianQwordAtOffset:put:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianWordAtOffset:!accessing!primitives!public! !
!ByteArray categoriesFor: #bigEndianWordAtOffset:put:!accessing!primitives!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

BigEndianDWORDField guid: (GUID fromString: '{8AC8C978-6639-409C-8557-CB3FDB5C9ECE}')!
BigEndianDWORDField comment: ''!
!BigEndianDWORDField categoriesForClass!Unclassified! !
!BigEndianDWORDField methodsFor!

accessorStem
	"Private - Answer the receiver's 'accessorStem'."

	^'bigEndianDword'!

fieldClass
	^BigEndianDWORD!

readFrom: anExternal 
	"Private - Instantiate an object of the the type the receiver represents
	at the receiver's offset in anExternal"

	^anExternal bigEndianDwordAtOffset: offset!

writeInto: anExternal value: anObject 
	"Private - Write anObject into anExternal at the receiver's offset, and in a form
	appropriate for the receiver's External type"

	^anExternal bigEndianDwordAtOffset: offset put: anObject! !
!BigEndianDWORDField categoriesFor: #accessorStem!automatic generation!private! !
!BigEndianDWORDField categoriesFor: #fieldClass!constants!private! !
!BigEndianDWORDField categoriesFor: #readFrom:!indirect accessing!private! !
!BigEndianDWORDField categoriesFor: #writeInto:value:!indirect accessing!private! !

BigEndianQWORDField guid: (GUID fromString: '{6999153F-EDCF-409F-8A89-09897811614C}')!
BigEndianQWORDField comment: ''!
!BigEndianQWORDField categoriesForClass!Unclassified! !
!BigEndianQWORDField methodsFor!

accessorStem
	"Private - Answer the receiver's 'accessorStem'."

	^'bigEndianQword'!

fieldClass
	^BigEndianQWORD!

readFrom: anExternal 
	"Private - Instantiate an object of the the type the receiver represents
	at the receiver's offset in anExternal"

	^anExternal bigEndianQwordAtOffset: offset!

writeInto: anExternal value: anObject 
	"Private - Write anObject into anExternal at the receiver's offset, and in a form
	appropriate for the receiver's External type"

	^anExternal bigEndianQwordAtOffset: offset put: anObject! !
!BigEndianQWORDField categoriesFor: #accessorStem!automatic generation!private! !
!BigEndianQWORDField categoriesFor: #fieldClass!constants!private! !
!BigEndianQWORDField categoriesFor: #readFrom:!indirect accessing!private! !
!BigEndianQWORDField categoriesFor: #writeInto:value:!indirect accessing!private! !

BigEndianWORDField guid: (GUID fromString: '{50FE92B2-5199-40D4-ADF3-E51FF951C5FF}')!
BigEndianWORDField comment: ''!
!BigEndianWORDField categoriesForClass!Unclassified! !
!BigEndianWORDField methodsFor!

accessorStem
	"Private - Answer the receiver's 'accessorStem'."

	^'bigEndianWord'!

fieldClass
	^BigEndianWORD!

readFrom: anExternal 
	"Private - Instantiate an object of the the type the receiver represents
	at the receiver's offset in anExternal"

	^anExternal bigEndianWordAtOffset: offset!

writeInto: anExternal value: anObject 
	"Private - Write anObject into anExternal at the receiver's offset, and in a form
	appropriate for the receiver's External type"

	^anExternal bigEndianWordAtOffset: offset put: anObject! !
!BigEndianWORDField categoriesFor: #accessorStem!automatic generation!private! !
!BigEndianWORDField categoriesFor: #fieldClass!constants!private! !
!BigEndianWORDField categoriesFor: #readFrom:!indirect accessing!private! !
!BigEndianWORDField categoriesFor: #writeInto:value:!indirect accessing!private! !

BigEndianDWORD guid: (GUID fromString: '{8DC2D999-F973-49FE-95AD-BC9F265BE609}')!
BigEndianDWORD comment: ''!
!BigEndianDWORD categoriesForClass!Unclassified! !
!BigEndianDWORD methodsFor!

asSignedInteger
	"Answer the signed 16-bit Integer value of the receiver."

	^bytes bigEndianSdwordAtOffset: 0!

value
	"Answer the receiver's value field as a Smalltalk object.
	Automatically generated get method - do not modify"

	^bytes bigEndianDwordAtOffset: 0!

value: anObject 
	"Set the receiver's value field to the value of anObject.
	Automatically generated set method - do not modify"

	bytes bigEndianDwordAtOffset: 0 put: anObject! !
!BigEndianDWORD categoriesFor: #asSignedInteger!converting!public! !
!BigEndianDWORD categoriesFor: #value!**compiled accessors**!public! !
!BigEndianDWORD categoriesFor: #value:!**compiled accessors**!public! !

!BigEndianDWORD class methodsFor!

arrayType
	"Private - Answer the class of object (typically an <ExternalArray> subclass) to be used to 
	represent arrays of the receiver's field type in other structures."

	"^WORDArray"
^self error: 'Implement if needed'!

defineFields
	"Define the fields of the WORD 'structure'. ExternalInteger subclasses
	have a single value.
		BigEndianDWORD compileDefinition
	"

	self defineField: #value type: BigEndianDWORDField new!

fieldType
	"Private - Answer the <ExternalField> type to be used to represent the receiver's field type in 
	other structures."

	^BigEndianDWORDField! !
!BigEndianDWORD class categoriesFor: #arrayType!constants!development!private! !
!BigEndianDWORD class categoriesFor: #defineFields!initializing!public! !
!BigEndianDWORD class categoriesFor: #fieldType!constants!development!private! !

BigEndianQWORD guid: (GUID fromString: '{B93FFF0E-FB3D-4201-A323-ED6AF12F9C0B}')!
BigEndianQWORD comment: ''!
!BigEndianQWORD categoriesForClass!Unclassified! !
!BigEndianQWORD methodsFor!

asSignedInteger
	"Answer the signed 16-bit Integer value of the receiver."

	^bytes bigEndianSqwordAtOffset: 0!

value
	"Answer the receiver's value field as a Smalltalk object.
	Automatically generated get method - do not modify"

	^bytes bigEndianQwordAtOffset: 0!

value: anObject 
	"Set the receiver's value field to the value of anObject.
	Automatically generated set method - do not modify"

	bytes bigEndianQwordAtOffset: 0 put: anObject! !
!BigEndianQWORD categoriesFor: #asSignedInteger!converting!public! !
!BigEndianQWORD categoriesFor: #value!**compiled accessors**!public! !
!BigEndianQWORD categoriesFor: #value:!**compiled accessors**!public! !

!BigEndianQWORD class methodsFor!

arrayType
	"Private - Answer the class of object (typically an <ExternalArray> subclass) to be used to 
	represent arrays of the receiver's field type in other structures."

	"^QWORDArray"

	^self error: 'Implement if needed'!

defineFields
	"Define the fields of the WORD 'structure'. ExternalInteger subclasses
	have a single value.
		BigEndianDWORD compileDefinition
	"

	self defineField: #value type: BigEndianQWORDField new!

fieldType
	"Private - Answer the <ExternalField> type to be used to represent the receiver's field type in 
	other structures."

	^BigEndianQWORDField! !
!BigEndianQWORD class categoriesFor: #arrayType!constants!development!private! !
!BigEndianQWORD class categoriesFor: #defineFields!initializing!public! !
!BigEndianQWORD class categoriesFor: #fieldType!constants!development!private! !

BigEndianWORD guid: (GUID fromString: '{58D59B89-860F-4708-A5A2-EB1ABC35AF9F}')!
BigEndianWORD comment: ''!
!BigEndianWORD categoriesForClass!Unclassified! !
!BigEndianWORD methodsFor!

asSignedInteger
	"Answer the signed 16-bit Integer value of the receiver."

	^bytes bigEndianSwordAtOffset: 0!

value
	"Answer the receiver's value field as a Smalltalk object.
	Automatically generated get method - do not modify"

	^bytes bigEndianWordAtOffset: 0!

value: anObject 
	"Set the receiver's value field to the value of anObject.
	Automatically generated set method - do not modify"

	bytes bigEndianWordAtOffset: 0 put: anObject! !
!BigEndianWORD categoriesFor: #asSignedInteger!converting!public! !
!BigEndianWORD categoriesFor: #value!**compiled accessors**!public! !
!BigEndianWORD categoriesFor: #value:!**compiled accessors**!public! !

!BigEndianWORD class methodsFor!

arrayType
	"Private - Answer the class of object (typically an <ExternalArray> subclass) to be used to 
	represent arrays of the receiver's field type in other structures."

	"^WORDArray"
^self error: 'Implement if needed'!

defineFields
	"Define the fields of the WORD 'structure'. ExternalInteger subclasses
	have a single value.
		BigEndianWORD compileDefinition
	"

	self defineField: #value type: BigEndianWORDField new!

fieldType
	"Private - Answer the <ExternalField> type to be used to represent the receiver's field type in 
	other structures."

	^BigEndianWORDField! !
!BigEndianWORD class categoriesFor: #arrayType!constants!development!private! !
!BigEndianWORD class categoriesFor: #defineFields!initializing!public! !
!BigEndianWORD class categoriesFor: #fieldType!constants!development!private! !

"Binary Globals"!

