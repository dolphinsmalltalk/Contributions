| package |
package := Package name: 'US File Extensions'.
package paxVersion: 1;
	basicComment: '$id: US File Extensions 0.012$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.012'.


package methodNames
	add: #Character -> #isInvalidInFilename;
	add: #Character -> #isValidInFilename;
	add: #FileLocator -> #isRelativePath:;
	add: #KernelLibrary -> #createHardLink:lpExistingFileName:;
	add: #KernelLibrary -> #createHardLink:lpExistingFileName:lpSecurityAttributes:;
	add: #KernelLibrary -> #getLongPathName:lpszLongPath:cchBuffer:;
	add: 'Character class' -> #invalidPathChars;
	add: 'File class' -> #hardLink:to:;
	add: 'File class' -> #longPathOf:;
	add: 'File class' -> #sizeOf:;
	add: 'File class' -> #splitDriveFrom:;
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


"Global Aliases"!


"Loose Methods"!

!Character methodsFor!

isInvalidInFilename
	^(self class invalidPathChars includes: self) !

isValidInFilename
	^self isInvalidInFilename not! !
!Character categoriesFor: #isInvalidInFilename!public!testing! !
!Character categoriesFor: #isValidInFilename!public!testing! !

!Character class methodsFor!

invalidPathChars
^##((((0 to: 31) collect: [:each | Character value: each]) , '\/:*?"<>|') asSortedCollection)! !
!Character class categoriesFor: #invalidPathChars!constants!public! !

!File class methodsFor!

hardLink: destination to: source
^KernelLibrary default createHardLink: destination lpExistingFileName: source!

longPathOf: aPathnameString 
	"Answers the long path version of aPathnameString"

	| longpath |
	longpath := String new: self maxPath.
	(KernelLibrary default 
		getLongPathName: aPathnameString
		lpszLongPath: longpath
		cchBuffer: longpath size) == 0 
		ifTrue: [KernelLibrary default systemError].
	^longpath trimNulls!

sizeOf: aFilename 
	| file size |
	[file := self open: aFilename.
	size := file size.
	] ensure: [file close].
	^size!

splitDriveFrom: aString 
	"Splits aPathname string and answers the drive portion.
	Answers the empty string if there is no drive."

	^(self splitPath: aString) at: 1! !
!File class categoriesFor: #hardLink:to:!public! !
!File class categoriesFor: #longPathOf:!filename manipulation!public! !
!File class categoriesFor: #sizeOf:!public! !
!File class categoriesFor: #splitDriveFrom:!public! !

!FileLocator methodsFor!

isRelativePath: aPath 
	^File isRelativePath: (self relativePathTo: aPath)! !
!FileLocator categoriesFor: #isRelativePath:!public! !

!KernelLibrary methodsFor!

createHardLink: lpFileName lpExistingFileName: lpExistingFileName 
	| return |
	return := self 
				createHardLink: lpFileName
				lpExistingFileName: lpExistingFileName
				lpSecurityAttributes: nil.
	return = false ifTrue: [self systemError: self getLastError].
	^return!

createHardLink: lpFileName lpExistingFileName: lpExistingFileName lpSecurityAttributes: lpSecurityAttributes 
	"Establishes a hard link between an existing file and a new file. This function is only supported on the NTFS file system, and only for files, not directories
	BOOL WINAPI CreateHardLink(
		__in        LPCTSTR lpFileName,
		__in        LPCTSTR lpExistingFileName,
		__reserved  LPSECURITY_ATTRIBUTES lpSecurityAttributes
	);"

	<stdcall: bool CreateHardLinkA char* char* lpvoid>
	^self invalidCall!

getLongPathName: lpszShortPath lpszLongPath: lpszLongPath cchBuffer: cchBuffer 
	"Answers the long pathname form of lpszShortPath in lpszLongPath"

	<stdcall: dword GetLongPathNameA lpstr lpstr dword>
	^self invalidCall! !
!KernelLibrary categoriesFor: #createHardLink:lpExistingFileName:!public!win32 functions-file! !
!KernelLibrary categoriesFor: #createHardLink:lpExistingFileName:lpSecurityAttributes:!public!win32 functions-file! !
!KernelLibrary categoriesFor: #getLongPathName:lpszLongPath:cchBuffer:!public!win32 functions-file! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

