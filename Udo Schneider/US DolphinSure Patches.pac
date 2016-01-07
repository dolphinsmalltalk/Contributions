| package |
package := Package name: 'US DolphinSure Patches'.
package paxVersion: 1;
	basicComment: '$id: US DolphinSure Patches 0.009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

See
http://groups.google.com/group/comp.lang.smalltalk.dolphin/browse_thread/thread/cd3265048e8b3e18/7d0e9b2ad64b6b63
for more information.


Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.009'.


package methodNames
	add: #DolphinSureCertificateInfo -> #validityString;
	add: 'NotSignedCertificate class' -> #new;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\DolphinSure\DolphinSure';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!DolphinSureCertificateInfo methodsFor!

validityString
	"Answers a <readableString> indicating the validity period for the receiver"

	| stream |
	stream := WriteStream on: String new.
	stream display: 'From '.
	self issued printOn: stream longPicture: false.
	stream display: ' ';
	 display: 'until '.
	self expiry notNil 
		ifTrue: 
			[self expiry printOn: stream longPicture: false.
			self hasExpired ifTrue: [stream nextPutAll: ' (EXPIRED)']]
		ifFalse: [stream display: 'indefinitely'].
	^stream contents! !
!DolphinSureCertificateInfo categoriesFor: #validityString!accessing!public! !

!NotSignedCertificate class methodsFor!

new
	"Answers an instance of the receiver. The byte array should be regenerated using the following
        expression if the data content of an instance changes:

                self generate binaryStoreBytes.

                self new show.
        "

	^Object 
		fromBinaryStoreBytes: #[33 83 84 66 32 51 32 6 4 20 0 78 111 116 83 105 103 110 101 100 67 101 114 116 105 102 105 99 97 116 101 114 0 0 0 201 0 0 0 33 83 84 66 32 51 32 6 6 26 0 68 111 108 112 104 105 110 83 117 114 101 67 101 114 116 105 102 105 99 97 116 101 73 110 102 111 82 0 0 0 20 0 0 0 78 111 116 83 105 103 110 101 100 67 101 114 116 105 102 105 99 97 116 101 82 0 0 0 0 0 0 0 192 1 0 0 6 1 4 0 68 97 116 101 97 46 1 0 0 0 0 0 130 0 0 0 100 0 0 0 8 219 150 216 90 113 81 215 68 14 17 21 135 231 2 178 111 238 239 30 199 88 92 179 189 9 16 162 60 243 121 252 122 152 133 113 28 33 145 205 222 103 117 189 24 239 32 245 69 121 0 91 196 27 120 23 128 154 115 129 62 122 219 165 228 79 63 219 36 63 133 5 131 27 30 17 255 236 32 65 20 6 206 231 63 49 58 99 5 227 64 68 206 22 113 185 0 0 0 0 82 0 0 0 20 0 0 0 78 111 116 83 105 103 110 101 100 67 101 114 116 105 102 105 99 97 116 101 82 0 0 0 105 0 0 0 91 68 83 65 32 100 105 103 105 116 97 108 32 115 105 103 110 97 116 117 114 101 32 52 54 67 70 67 65 68 52 68 53 53 66 66 68 50 53 57 53 50 53 55 49 70 48 51 55 53 65 50 55 70 51 68 68 52 53 68 56 69 54 32 53 54 70 48 57 54 68 53 50 50 70 50 56 48 50 52 66 56 50 69 68 70 51 54 65 52 53 53 65 68 69 69 57 49 56 53 66 50 67 51 93 0 0 0 0]! !
!NotSignedCertificate class categoriesFor: #new!instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

