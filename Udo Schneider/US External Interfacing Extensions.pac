| package |
package := Package name: 'US External Interfacing Extensions'.
package paxVersion: 1;
	basicComment: '$id: US External Interfacing Extensions 0.004$, $date: 24.07.2009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) 2007, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.004'.


package classNames
	add: #VariableArrayField;
	yourself.

package methodNames
	add: #PositionableStream -> #nextStructure:;
	add: 'StructureArray class' -> #fromBytes:length:elementClass:;
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

ArrayField subclass: #VariableArrayField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!PositionableStream methodsFor!

nextStructure: anExternalStructureClass
^(anExternalStructureClass fromBytes: (self next: anExternalStructureClass byteSize))! !
!PositionableStream categoriesFor: #nextStructure:!public! !

!StructureArray class methodsFor!

fromBytes: aByteArray length: anInteger elementClass: elementClass 
	
	^self fromAddress: aByteArray yourAddress length: anInteger elementClass: elementClass! !
!StructureArray class categoriesFor: #fromBytes:length:elementClass:!instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

VariableArrayField guid: (GUID fromString: '{601D61FB-1CEB-4058-BA79-4D291CF0C4AF}')!
VariableArrayField comment: ''!
!VariableArrayField categoriesForClass!Unclassified! !
!VariableArrayField methodsFor!

byteSize
	"Private - Answer the byte size of the embedded array the receiver represents"

	^self length * self elementSize!

elementClass
	"Private - Answer the class of <ExternalStructure> used to represent elements of the receiver."

	^fieldClass!

length
	"Answer the length of the array at the field described
	by the receiver. We don't know until run time what the length
	is, so claim there is one element."

	^1!

lengthString
	"Private - Answer the length string to be inserted in the accessor."

	^'self ', length!

readAccessorMethodText: contentsAccessString 
	"Private - Answer suitable method text for compiling a read-accessor method
	to substitute for the receiver (does not include the selector)."

	^(super readAccessorMethodText: contentsAccessString) !

readFrom: struct
	"Private -  Answer a <StructureArray> which references the array
	of structures embedded in the <ExternalStructure>, struct, in the
	field described by the receiver.  The result may be sent #at: and #at:put:
	messages to get/set the actual elements in-place."

	^self arrayClass
		fromAddress: struct yourAddress + offset
		length: (struct perform: length)
		elementClass: self elementClass! !
!VariableArrayField categoriesFor: #byteSize!accessing!private! !
!VariableArrayField categoriesFor: #elementClass!constants!private! !
!VariableArrayField categoriesFor: #length!accessing!public! !
!VariableArrayField categoriesFor: #lengthString!constants!private! !
!VariableArrayField categoriesFor: #readAccessorMethodText:!automatic generation!private! !
!VariableArrayField categoriesFor: #readFrom:!accessing!private! !

"Binary Globals"!

