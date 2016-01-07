| package |
package := Package name: 'ShellLibraryExtensions'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #SHELLEXECUTEINFO;
	yourself.

package methodNames
	add: #ShellLibrary -> #shellExecuteEx:;
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

Win32Structure subclass: #SHELLEXECUTEINFO
	instanceVariableNames: 'verb file parameters directory class'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ShellLibrary methodsFor!

shellExecuteEx: execInfo
	"Opens or prints the specified file, which can be an executable or document file.
		BOOL ShellExecute(
			LPSHELLEXECUTEINFO lpExecInfo   
		);"

	<stdcall: bool ShellExecuteExA SHELLEXECUTEINFO*>
	^self invalidCall! !
!ShellLibrary categoriesFor: #shellExecuteEx:!public!win32 functions-shell library! !

"End of package definition"!

"Source Globals"!

"Classes"!

SHELLEXECUTEINFO guid: (GUID fromString: '{93C82691-CF74-11D4-BDF7-00010240D5E2}')!
SHELLEXECUTEINFO comment: ''!
!SHELLEXECUTEINFO categoriesForClass!Unclassified! !
!SHELLEXECUTEINFO methodsFor!

class: aString
	"Set the class for the receiver to aString.  We store the class in an instance variable to prevent it
	 from being GC'd"

	class:= aString.
	self lpClass: aString!

directory: aString
	"Set the directory for the receiver to aString.  We store the directory in an instance variable to prevent it
	 from being GC'd"

	directory:= aString.
	self lpDirectory: aString!

dwSize: anObject
	"Set the receiver's dwSize field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

file: aString
	"Set the file for the receiver to aString.  We store the file in an instance variable to prevent it
	 from being GC'd"

	file := aString.
	self lpFile: aString!

hMonitor
	^self hIcon!

hMonitor: aHandle
	^self hIcon: aHandle!

parameters: aString
	"Set the parameters for the receiver to aString.  We store the parameters in an instance variable to prevent it
	 from being GC'd"

	parameters := aString.
	self lpParameters: aString!

verb: aString
	"Set the verb for the receiver to aString.  We store the verb in an instance variable to prevent it
	 from being GC'd"

	verb := aString.
	self lpVerb: aString! !
!SHELLEXECUTEINFO categoriesFor: #class:!public! !
!SHELLEXECUTEINFO categoriesFor: #directory:!public! !
!SHELLEXECUTEINFO categoriesFor: #dwSize:!public! !
!SHELLEXECUTEINFO categoriesFor: #file:!public! !
!SHELLEXECUTEINFO categoriesFor: #hMonitor!public! !
!SHELLEXECUTEINFO categoriesFor: #hMonitor:!public! !
!SHELLEXECUTEINFO categoriesFor: #parameters:!public! !
!SHELLEXECUTEINFO categoriesFor: #verb:!public! !

!SHELLEXECUTEINFO class methodsFor!

defineFields
	"Define the layout of the Win32 SHELLEXECUTEINFO structure.

		SHELLEXECUTEINFO compileDefinition

	typedef struct _SHELLEXECUTEINFO{
		DWORD cbSize;
		ULONG fMask;
		HWND hwnd;
		LPCTSTR lpVerb;
		LPCTSTR lpFile;
		LPCTSTR lpParameters;
		LPCTSTR lpDirectory;
		int nShow;
		HINSTANCE hInstApp;

		// Optional members

		LPVOID lpIDList;
		LPCSTR lpClass;
		HKEY hkeyClass;
		DWORD dwHotKey;
		union {
			HANDLE hIcon;
			HANDLE hMonitor;
			};
		HANDLE hProcess; } SHELLEXECUTEINFO, *LPSHELLEXECUTEINFO; "

 	self 
		defineField: #dwSize type: DWORDField writeOnly beOverride;
		beUncompiled;
		defineField: #fMask type: DWORDField new;
		defineField: #hwnd type: HANDLEField new;
		defineField: #lpVerb type: (PointerField type: String);
		defineField: #lpFile type: (PointerField type: String);
		defineField: #lpParameters type: (PointerField type: String);
		defineField: #lpDirectory type: (PointerField type: String);
		defineField: #nShow type: SDWORDField new;
		defineField: #hInstApp type: HANDLEField new;
		defineField: #lpIDList type: DWORDField new;	"(PointerField type: ITEMIDLIST)"
		defineField: #lpClass type: (PointerField type: String);
		defineField: #hkeyClass type: HANDLEField new;
		defineField: #dwHotKey type: DWORDField new;
		defineField: #hIcon type: HANDLEField new;
		defineField: #hProcess type: HANDLEField new! !
!SHELLEXECUTEINFO class categoriesFor: #defineFields!public! !

"Binary Globals"!

"Resources"!

