| package |
package := Package name: 'VersionLibraryHelpers'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package methodNames
	add: #VersionLibrary -> #stringValue:from:;
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


"Global Aliases"!


"Loose Methods"!

!VersionLibrary methodsFor!

stringValue: aString from: aFilename
	"aString should be one of the predefined version information strings which VerQueryValue
	 recognizes (CompanyName, FileDescription, FileVersion, InternalName, LegalCopyright,
	 OriginalFilename, ProductName, or ProductVersion).  aFilename should be the fully-
	 qualified name of a file (e.g. c:\winnt\system32\notepad.exe)."

	| versionBuf verQueryBuffer dwLen langCharSet hiWordString loWordString langCharSetString
	  stringQueryBuffer stringLen resultString |

	versionBuf := VersionLibrary default getFileVersionInfo: aFilename.
	versionBuf notNil ifTrue: [
		verQueryBuffer := ExternalPointer newPointer.
		dwLen := DWORD new.
		(VersionLibrary default verQueryValue: versionBuf
						       lpSubBlock: '\VarFileInfo\Translation'
						       lplpBuffer: verQueryBuffer
						       puLen: dwLen) ifTrue: [
			langCharSet := verQueryBuffer bytes dwordAtOffset: 0.
			hiWordString := langCharSet highWord printStringRadix: 16 showRadix: false.
			[ hiWordString size < 4 ] whileTrue: [ hiWordString := '0', hiWordString ].
			loWordString := langCharSet lowWord printStringRadix: 16 showRadix: false.
			[ loWordString size < 4 ] whileTrue: [ loWordString := '0', loWordString ].
			langCharSetString := loWordString, hiWordString.
			stringQueryBuffer := ExternalPointer newPointer.
			stringLen := DWORD new.
			(VersionLibrary default verQueryValue: versionBuf
								lpSubBlock: '\StringFileInfo\', langCharSetString, '\', aString
								lplpBuffer: stringQueryBuffer
								puLen: stringLen) ifTrue: [
				resultString := stringQueryBuffer bytes copyStringFrom: 1 to: stringLen asInteger - 1 ] ] ].

	^resultString! !
!VersionLibrary categoriesFor: #stringValue:from:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

