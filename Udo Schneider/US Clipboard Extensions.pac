| package |
package := Package name: 'US Clipboard Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Clipboard Extensions 0.010$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Get filenames from Clipboard:
	filenames := Clipboard current getFilenamesIfNone: [#()]
	filenames := Clipboard current getFilenames
	
Get Unicode Text from Clipboard:
	text := Clipboard current getUnicodeTextIfNone: ['''']
	text := Clipboard current getUnicodeText
	
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.010'.

package basicScriptAt: #postuninstall put: 'Win32Constants removeKey: ''CF_HDROP'' '.
package basicScriptAt: #preinstall put: 'Win32Constants at: ''CF_HDROP'' put: 16rF'.

package methodNames
	add: #Clipboard -> #getFilenames;
	add: #Clipboard -> #getFilenamesIfNone:;
	add: #Clipboard -> #getUnicodeText;
	add: #Clipboard -> #getUnicodeTextFormat:ifNone:;
	add: #Clipboard -> #getUnicodeTextIfNone:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Clipboard methodsFor!

getFilenames
	^self getFilenamesIfNone: [#()]!

getFilenamesIfNone: exceptionHandler 
	"Answer a list of filenames copied from the clipboard, or if none the result
	of evaluating the <niladicValuable> exceptionHandler."

^


self apply: 
			[| hDrop |
			hDrop := UserLibrary default getClipboardData: CF_HDROP .
			hDrop isNull ifTrue: [^exceptionHandler value].
			ShellLibrary default dragQueryFile: hDrop ]!

getUnicodeText
	"Answer a <readableString> containing the the CF_UNICODETEXT contents 
	of the clipboard. If no text is currently available, raise an exception."

	^self getUnicodeTextIfNone: [self errorFormatNotAvailable: #UnicodeString ]!

getUnicodeTextFormat: formatId ifNone: exceptionHandler 
	"Private - Answer a <readableString> containing the text contents
	of the clipboard of the specified format. If the format is not currently 
	available, the answers the result of evaluating the <niladicValuable> 
	exceptionHandler.
	N.B. It is not checked that formatId is actually a text format."

	^self apply: 
			[| hText pText text |
			hText := UserLibrary default getClipboardData: formatId.
			hText isNull ifTrue: [^exceptionHandler value].
			pText := KernelLibrary default globalLock: hText.
			text := UnicodeString  fromAddress: pText.
			KernelLibrary default globalUnlock: hText.
			text]!

getUnicodeTextIfNone: exceptionHandler 
	"Answer a <readableString> containing the the CF_UNICODETEXT contents 
	of the clipboard. If no text is currently available, the answers the
	result of evaluating the <niladicValuable> exceptionHandler."

	^self getUnicodeTextFormat: CF_UNICODETEXT ifNone: exceptionHandler! !
!Clipboard categoriesFor: #getFilenames!accessing!public! !
!Clipboard categoriesFor: #getFilenamesIfNone:!accessing!public! !
!Clipboard categoriesFor: #getUnicodeText!accessing!public! !
!Clipboard categoriesFor: #getUnicodeTextFormat:ifNone:!accessing!private! !
!Clipboard categoriesFor: #getUnicodeTextIfNone:!accessing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

