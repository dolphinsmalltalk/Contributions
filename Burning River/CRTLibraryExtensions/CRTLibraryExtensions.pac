| package |
package := Package name: 'CRTLibraryExtensions'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package methodNames
	add: #CRTLibrary -> #_snprintf:count:format:with:with:with:with:with:with:;
	add: #String -> #sprintfWith:with:with:with:with:with:;
	yourself.

package globalNames
	add: #CRTLibraryExtension;
	yourself.

package binaryGlobalNames: (Set new
	add: #CRTLibraryExtension;
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

!CRTLibrary methodsFor!

_snprintf: buffer count: maxbuf format: format with: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	"Private - Write data formatted by the format string into the buffer.
	see _snprintf:count:format:with:with:with: for further information."

	<cdecl: sdword _snprintf lpvoid sdword lpstr lpvoid lpvoid lpvoid lpvoid lpvoid lpvoid>
	^self invalidCall! !
!CRTLibrary categoriesFor: #_snprintf:count:format:with:with:with:with:with:with:!CRT functions-stream I/O!private! !

!String methodsFor!

sprintfWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	"Answer a String which is a message formatted from the receiver (assumed to be a C-printf
	format String) with substituations from the remaining argument(s).
	Note: This is much faster than formatWith:with:."

	| n crt buf size |
	crt := CRTLibrary default.
	size := self size + 128.
	buf := String new: size.
	[
		n := crt _snprintf: buf count: size format: self with: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6.
		n < 0] whileTrue: [size := size * 2].
	^buf copyFrom: 1 to: n! !
!String categoriesFor: #sprintfWith:with:with:with:with:with:!printing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

CRTLibraryExtension := Object fromBinaryStoreBytes: 
(ByteArray fromHexString: '21535442203120BA0000000000000052000000130000004352544C696272617279457874656E73696F6E')!

"Resources"!

