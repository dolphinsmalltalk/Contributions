| package |
package := Package name: 'Profiles'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #InitializationFile;
	yourself.

package methodNames
	add: #KernelLibrary -> #getPrivateProfileSection:returnedString:size:filename:;
	add: #KernelLibrary -> #getPrivateProfileSectionNames:size:filename:;
	add: #KernelLibrary -> #getPrivateProfileString:keyName:default:returnedString:returnedStringSize:profileName:;
	add: #KernelLibrary -> #writePrivateProfileSection:sectionData:filename:;
	add: #KernelLibrary -> #writePrivateProfileString:keyName:string:profileName:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #InitializationFile
	instanceVariableNames: 'filename'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!KernelLibrary methodsFor!

getPrivateProfileSection: sectionNameString returnedString: buf size: size filename: aString
	<stdcall: dword GetPrivateProfileSectionA lpstr lpstr dword lpstr>
	^self invalidCall!

getPrivateProfileSectionNames: buf size: size filename: aString
	<stdcall: dword GetPrivateProfileSectionNamesA lpstr dword lpstr>
	^self invalidCall!

getPrivateProfileString: sectionNameString keyName: keyNameString default: defaultString returnedString: returnedString returnedStringSize: aDword profileName: fileNameString
	<stdcall: dword GetPrivateProfileStringA lpstr lpstr lpstr lpvoid dword lpstr>
	^self invalidCall!

writePrivateProfileSection: sectionNameString sectionData: aString filename: profileNameString
	<stdcall: bool WritePrivateProfileSectionA lpstr lpstr lpstr>
	^self invalidCall!

writePrivateProfileString: sectionNameString keyName: keyNameString string: aString profileName: profileNameString
	<stdcall: bool WritePrivateProfileStringA lpstr lpstr lpstr lpstr>
	^self invalidCall! !
!KernelLibrary categoriesFor: #getPrivateProfileSection:returnedString:size:filename:!public!win32 functions-profiles! !
!KernelLibrary categoriesFor: #getPrivateProfileSectionNames:size:filename:!public!win32 functions-profiles! !
!KernelLibrary categoriesFor: #getPrivateProfileString:keyName:default:returnedString:returnedStringSize:profileName:!public!win32 functions-profiles! !
!KernelLibrary categoriesFor: #writePrivateProfileSection:sectionData:filename:!public!win32 functions-profiles! !
!KernelLibrary categoriesFor: #writePrivateProfileString:keyName:string:profileName:!public!win32 functions-profiles! !

"End of package definition"!

"Source Globals"!

"Classes"!

InitializationFile guid: (GUID fromString: '{705E90C3-5BE7-11D3-826C-00001D19F5C2}')!
InitializationFile comment: ''!
!InitializationFile categoriesForClass!No category! !
!InitializationFile methodsFor!

deleteKey: aStringKeyName section: aStringSectionName
	^self section: aStringSectionName
		key: aStringKeyName
		value: nil!

deleteSection: aStringSectionName
	^self section: aStringSectionName
		key: nil
		value: nil!

extractNulDelimitedStringsFrom: aString count: count
	"Private - Extract the zero-delimited strings."

	| col str c |

	col := OrderedCollection new.
	str := String new.

	1 to: count do: [ :i |
		c := aString at: i.
		c codePoint = 0
			ifFalse: [
				str := str, (String with: c) ]
			ifTrue: [
				str size > 0 ifTrue: [
					col add: str.
					str := String new ] ] ].
	str size > 0 ifTrue: [ col add: str ].
	^col!

filename
	"Answer the value of the receiver's instance variable filename.
	This method was automatically generated, but may be modified."

	^filename!

filename: anObject
	"Set the value of the receiver's instance variable filename to anObject.
	This method was automatically generated, but may be modified."

	filename := anObject!

keyNames: aStringSectionName
	"Answer a collection of the names of all keys in the given section."

	| str |

	str := self section: aStringSectionName key: nil default: nil.

	"Extract the nul-delimited strings."

	^self extractNulDelimitedStringsFrom: str count: str size!

section: aStringSectionName
	"Answer the key-value pairs contained in the section named 'aStringSectionName' as a Dictionary."

	| buf count col dict subStrings key value |

	buf := String new: 512.
	count := buf size - 2.

	[ count = (buf size - 2) ] whileTrue: [
		buf := String new: buf size * 2.
		count := buf size - 2.
		count := KernelLibrary default
				getPrivateProfileSection: aStringSectionName
				returnedString: buf
				size: buf size
				filename: self filename ].

	col := self extractNulDelimitedStringsFrom: buf count: count.

	"Build the dictionary."

	dict := Dictionary new: col size.
	col do: [ :aString |
		subStrings := aString subStrings: $=.
		key := subStrings at: 1.
		subStrings size > 1
			ifTrue: [ value := subStrings at: 2 ]
			ifFalse: [ value := nil ].
		dict add: key -> value ].
	dict size > 0
		ifTrue: [ ^dict ]
		ifFalse: [ ^nil ]!

section: aSectionNameString key: aKeyNameString default: aDefaultString
	"Answer the value of the given section and key as a string."

	| str count failedSize |

	str := String new: 512.
	aSectionNameString isNil | aKeyNameString isNil
		ifTrue: [ failedSize := str size - 2 ]
		ifFalse: [ failedSize := str size - 1 ].
	count := failedSize.

	[ count = failedSize ] whileTrue:
		[ str := String new: str size * 2.
		aSectionNameString isNil | aKeyNameString isNil
			ifTrue: [ failedSize := str size - 2 ]
			ifFalse: [ failedSize := str size - 1 ].
		count := KernelLibrary default
					getPrivateProfileString: aSectionNameString
					keyName: aKeyNameString
					default: aDefaultString
					returnedString: str
					returnedStringSize: str size
					profileName: self filename ].
	aKeyNameString isNil
		ifTrue: [ ^str ]
		ifFalse: [ ^str trimNulls ]!

section: aSectionNameString key: aKeyNameString value: aValueString
	"Replace the current value of the given section and key with the new value."

	^KernelLibrary default writePrivateProfileString: aSectionNameString
						keyName: aKeyNameString
						string: aValueString
						profileName: self filename!

section: aStringSectionName put: aDictionary
	"Write the key-value pairs contained in aDictionary to the section named 'aStringSectionName'"

	| str |

	str := String new.
	aDictionary associationsDo: [ :anAssociation |
		str := str,
				anAssociation key displayString,
				'=',
				anAssociation value displayString,
				(String with: (Character value: 0)) ].

	^KernelLibrary default
			writePrivateProfileSection: aStringSectionName
			sectionData: str
			filename: self filename!

sectionNames
	"Answer a collection of the names of all sections in this initialization files."

	| buf str count |

	buf := String new: 512.
	count := buf size - 2.

	"Loop, expanding the buffer as needed, until we manage to get *all* the section names."

	[ count = (buf size - 2) ] whileTrue: [
		buf := String new: buf size * 2.
		count := KernelLibrary default
				getPrivateProfileSectionNames: buf
					size: buf size
					filename: self filename ].

	"Extract the zero-delimited strings."

	^self extractNulDelimitedStringsFrom: buf count: count! !
!InitializationFile categoriesFor: #deleteKey:section:!operations!public! !
!InitializationFile categoriesFor: #deleteSection:!operations!public! !
!InitializationFile categoriesFor: #extractNulDelimitedStringsFrom:count:!private!private helpers! !
!InitializationFile categoriesFor: #filename!accessing!public! !
!InitializationFile categoriesFor: #filename:!accessing!public! !
!InitializationFile categoriesFor: #keyNames:!accessing!public! !
!InitializationFile categoriesFor: #section:!accessing!public! !
!InitializationFile categoriesFor: #section:key:default:!accessing!public! !
!InitializationFile categoriesFor: #section:key:value:!operations!public! !
!InitializationFile categoriesFor: #section:put:!operations!public! !
!InitializationFile categoriesFor: #sectionNames!accessing!public! !

!InitializationFile class methodsFor!

Changes
	"29-Aug-2002  Bob Jarvis  Removed class #test method, which was redundant given the existence of unit tests."!

on: aString
	^self new filename: aString! !
!InitializationFile class categoriesFor: #Changes!public! !
!InitializationFile class categoriesFor: #on:!public! !

"Binary Globals"!

"Resources"!

