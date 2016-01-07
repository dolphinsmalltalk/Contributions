| package |
package := Package name: 'US Metafile'.
package paxVersion: 1;
	basicComment: '$id: US Metafile 1.004$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Provides a Dolphin canvase which writes out contents to a Windows Metafile

Usage
	Create a metafile canvase using MetafileCanvas>>onFile:
	Use regular canvas methods on the newly created canvas
	canvas := MetafileCanvas onFile: ''test.wmf''.
	canvas lineFrom: 10@10 to: 100@100.
	canvas := nil.
	

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.004'.


package classNames
	add: #MetafileCanvas;
	yourself.

package methodNames
	add: #GDILibrary -> #closeMetaFile:;
	add: #GDILibrary -> #createMetaFile:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Canvas subclass: #MetafileCanvas
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!GDILibrary methodsFor!

closeMetaFile: hdc 
	"Closes a metafile device context and returns a handle that identifies a Windows-format metafile
		HMETAFILE CloseMetaFile(
			HDC hdc   // handle to Windows-metafile DC
		);"

	<stdcall: handle CloseMetaFile handle>
	^self invalidCall!

createMetaFile: lpszFile
	"Creates a device context for a Windows-format metafile.
		HDC CreateMetaFile(
			LPCTSTR lpszFile   // file name
		);"
<stdcall: handle CreateMetaFileA lpstr>
^self invalidCall


! !
!GDILibrary categoriesFor: #closeMetaFile:!public!win32 functions-device context! !
!GDILibrary categoriesFor: #createMetaFile:!public!win32 functions-device context! !

"End of package definition"!

"Source Globals"!

"Classes"!

MetafileCanvas guid: (GUID fromString: '{1FBA093D-24AA-4B20-997D-1AF6506F151E}')!
MetafileCanvas comment: ''!
!MetafileCanvas categoriesForClass!Unclassified! !
!MetafileCanvas methodsFor!

basicFree
	"Private - Free the external resources associated with the receiver."

	super basicFree
.

	GDILibrary default closeMetaFile: self asParameter.! !
!MetafileCanvas categoriesFor: #basicFree!private!realizing/unrealizing! !

!MetafileCanvas class methodsFor!

onFile: aFilename

|hDC|
	^(hDC := GDILibrary default createMetaFile: aFilename) isNil ifFalse: [self withOwnedDC: hDC]
! !
!MetafileCanvas class categoriesFor: #onFile:!*-unreferenced selectors!public! !

"Binary Globals"!

