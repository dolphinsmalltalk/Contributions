| package |
package := Package name: 'LogFileStream'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #LogFileStream;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\CRTLibraryExtensions\CRTLibraryExtensions';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

FileStream subclass: #LogFileStream
	instanceVariableNames: 'needTimestamp'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LogFileStream guid: (GUID fromString: '{7816D38F-F637-455A-AD42-FAA3D2166937}')!
LogFileStream comment: ''!
!LogFileStream categoriesForClass!Collections-Streams! !
!LogFileStream methodsFor!

cr
	super cr.
	self flush.
	self needTimestamp: true!

initialize
	super initialize.
	self needTimestamp: true!

needTimestamp
	^needTimestamp!

needTimestamp: aBoolean
	needTimestamp := aBoolean!

nextPut: anIntegerOrCharacter
	self needTimestamp ifTrue:
		[ self putTimestamp.
		self needTimestamp: false ].
	super nextPut: anIntegerOrCharacter!

nextPutAll: aStringOrByteArray
	self needTimestamp ifTrue:
		[ self putTimestamp.
		self needTimestamp: false ].
	super nextPutAll: aStringOrByteArray!

putTimestamp
	super
		nextPutAll: (self timeStampString: TimeStamp current);
		nextPutAll: '  '!

timeStampString: aTimeStamp
	| d t |

	d := aTimeStamp date.
	t := aTimeStamp time.
	^'%02d/%02d/%4d %02d:%02d:%02d' sprintfWith: d dayOfMonth with: d monthIndex with:d year
									with: t hours with: t minutes with: t seconds! !
!LogFileStream categoriesFor: #cr!public! !
!LogFileStream categoriesFor: #initialize!public! !
!LogFileStream categoriesFor: #needTimestamp!accessing!public! !
!LogFileStream categoriesFor: #needTimestamp:!accessing!public! !
!LogFileStream categoriesFor: #nextPut:!public! !
!LogFileStream categoriesFor: #nextPutAll:!public! !
!LogFileStream categoriesFor: #putTimestamp!public! !
!LogFileStream categoriesFor: #timeStampString:!public! !

!LogFileStream class methodsFor!

open: aFilenameString
	^super write: aFilenameString mode: #append check: false text: true!

referencesToOtherPackages
	CRTLibraryExtension		"Global defined in the CRTLibraryExtension package"! !
!LogFileStream class categoriesFor: #open:!public! !
!LogFileStream class categoriesFor: #referencesToOtherPackages!private! !

"Binary Globals"!

"Resources"!

