| package |
package := Package name: 'US OmniBase Extensions'.
package paxVersion: 1;
	basicComment: '$id: US OmniBase Extensions 0.006$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.006'.


package classNames
	add: #ODBPersistentTransactionObject;
	yourself.

package methodNames
	add: #ODBBTreeIndexDictionary -> #getAll;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\ITC Gorisek\OmniBase';
	yourself).

package!

"Class Definitions"!

Object subclass: #ODBPersistentTransactionObject
	instanceVariableNames: 'transaction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ODBBTreeIndexDictionary methodsFor!

getAll
| keys key |
	keys  := ReadWriteStream on: Array new.
	key := self getFirst.
	[ key notNil] whileTrue: [
		keys  nextPut: key.
		key := self getNext ].
	^keys contents! !
!ODBBTreeIndexDictionary categoriesFor: #getAll!enumerating!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ODBPersistentTransactionObject guid: (GUID fromString: '{FB6BDB60-A15C-45E0-9344-BFB2F2CF662D}')!
ODBPersistentTransactionObject comment: ''!
!ODBPersistentTransactionObject categoriesForClass!Unclassified! !
!ODBPersistentTransactionObject methodsFor!

lock
		"Sets a write-lock on the object. Answer <true> if successful or if object is transient."

	^transaction isNil or: [transaction lock: self]!

markDirty
		"Informs transaction that the object has changed and 
		has to be written into the database when transaction commits."

	transaction notNil
		ifTrue: [ transaction markDirty: self ]!

objectId

	^self transaction getObjectID: self!

odbLoadedIn: anOmniBaseTransaction
		"This method is sent when the object is loaded from the database."

	transaction := anOmniBaseTransaction!

odbMadePersistentIn: anOmniBaseTransaction
		"This method is sent when the object is made persistent."

	transaction := anOmniBaseTransaction!

transaction
		"Answer an OmniBase transaction in which the object was loaded
		or <nil> if object is not persistent."

	^transaction!

unlock
		"Removes a write-lock on the object."

	transaction notNil
		ifTrue: [transaction unlock: self]! !
!ODBPersistentTransactionObject categoriesFor: #lock!public! !
!ODBPersistentTransactionObject categoriesFor: #markDirty!public! !
!ODBPersistentTransactionObject categoriesFor: #objectId!public! !
!ODBPersistentTransactionObject categoriesFor: #odbLoadedIn:!private! !
!ODBPersistentTransactionObject categoriesFor: #odbMadePersistentIn:!private! !
!ODBPersistentTransactionObject categoriesFor: #transaction!public! !
!ODBPersistentTransactionObject categoriesFor: #unlock!public! !

!ODBPersistentTransactionObject class methodsFor!

odbTransientInstanceVariables
		"This method tells OmniBase which instance variables should not be stored into the database."

	^OrderedCollection with: 'transaction'! !
!ODBPersistentTransactionObject class categoriesFor: #odbTransientInstanceVariables!public! !

"Binary Globals"!

