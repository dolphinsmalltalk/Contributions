| package |
package := Package name: 'US VM Compression'.
package paxVersion: 1;
	basicComment: '$id: US VM Compression 0.001$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 26.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.001'.


package methodNames
	add: #VMLibrary -> #compress2:;
	add: #VMLibrary -> #compress2:destLen:source:sourceLen:level:;
	add: #VMLibrary -> #compress2:level:;
	add: #VMLibrary -> #uncompress:;
	add: #VMLibrary -> #uncompress:destLen:;
	add: #VMLibrary -> #uncompress:destLen:source:sourceLen:;
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

!VMLibrary methodsFor!

compress2: aByteArray ^self compress2: aByteArray level: -1!

compress2: dest destLen: destLen source: source sourceLen: sourceLen level: level 
	"int compress2 (Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen, int level);
	
	Compresses the source buffer into the destination buffer. The level parameter has the same meaning as
	in deflateInit. sourceLen is the byte length of the source buffer. Upon entry, destLen is the total size of the
	destination buffer, which must be at least 0.1% larger than sourceLen plus 12 bytes. Upon exit, destLen is
	the actual size of the compressed buffer.
	
	compress2 returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_BUF_ERROR if there
	was not enough room in the output buffer, Z_STREAM_ERROR if the level parameter is invalid. "

	<cdecl: sdword compress2 lpvoid dword* lpvoid dword dword>
	^self invalidCall!

compress2: aByteArray level: aLevel 


	| destLen dest |
	destLen := DWORD fromInteger: (aByteArray size * 1.001 + 12) rounded.
	dest := ByteArray new: destLen value.
	(self 
		compress2: dest
		destLen: destLen
		source: aByteArray
		sourceLen: aByteArray size
		level: aLevel) = 0 
		ifFalse: [^nil].
	^dest copyFrom: 1 to: destLen value!

uncompress: aByteArray 
	| destLen uncompressed |
	destLen := aByteArray size * 2.
	[(uncompressed := self uncompress: aByteArray destLen: destLen) isNil ] whileTrue: [destLen  := destLen  * 2].
	^uncompressed!

uncompress: aByteArray destLen: anInteger 


	| destLen dest |
	destLen := DWORD fromInteger: anInteger.
	dest := ByteArray new: destLen value.
	(self 
		uncompress: dest
		destLen: destLen
		source: aByteArray
		sourceLen: aByteArray size) = 0 
		ifFalse: [^nil].
	^dest copyFrom: 1 to: destLen value!

uncompress: lpvoid destLen: destLen source: source sourceLen: sourceLen 
	"int uncompress (Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen);
	
	Decompresses the source buffer into the destination buffer. sourceLen is the byte length of the source buffer.
	Upon entry, destLen is the total size of the destination buffer, which must be large enough to hold the entire
	uncompressed data. (The size of the uncompressed data must have been saved previously by the compressor
	and transmitted to the decompressor by some mechanism outside the scope of this compression library.) Upon
	exit, destLen is the actual size of the uncompressed buffer.
	
	This function can be used to decompress a whole file at once if the input file is mmap'ed.
	
	uncompress returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_BUF_ERROR if there
	was not enough room in the output buffer, or Z_DATA_ERROR if the input data was corrupted. "

	<cdecl: sdword uncompress lpvoid dword* lpvoid dword>
	^self invalidCall! !
!VMLibrary categoriesFor: #compress2:!public! !
!VMLibrary categoriesFor: #compress2:destLen:source:sourceLen:level:!public! !
!VMLibrary categoriesFor: #compress2:level:!public! !
!VMLibrary categoriesFor: #uncompress:!public! !
!VMLibrary categoriesFor: #uncompress:destLen:!public! !
!VMLibrary categoriesFor: #uncompress:destLen:source:sourceLen:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

