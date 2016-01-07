| package |
package := Package name: 'UserLibraryExtensions'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package methodNames
	add: #UserLibrary -> #getTabbedTextExtent:lpString:nCount:nTabPositions:lpnTabStopPositions:;
	add: #UserLibrary -> #tabbedTextOut:X:Y:lpString:nCount:nTabPositions:lpnTabPositions:nTabOrigin:;
	yourself.

package globalNames
	add: #UserLibraryExtension;
	yourself.

package binaryGlobalNames: (Set new
	add: #UserLibraryExtension;
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

!UserLibrary methodsFor!

getTabbedTextExtent: hDC lpString: lpString nCount: nCount nTabPositions: nTabPositions lpnTabStopPositions: tabPositions
	"Compute the width and height of a character string using the currently selected font.  If the string contains one
	or more tab characters, the width of the string is based upon the specified tab stops.

		DWORD GetTabbedTextExtent(
		  HDC hDC,                        // handle to DC
		  LPCTSTR lpString,               // character string
		  int nCount,                     // number of characters
		  int nTabPositions,              // number of tab positions
		  CONST LPINT lpnTabStopPositions // array of tab positions
		);"

	<stdcall: sdword GetTabbedTextExtentA handle lpstr sdword sdword sdword*>
	^self invalidCall
!

tabbedTextOut: hdc X: x Y: y lpString: str nCount: nCount nTabPositions: nTabPositions lpnTabPositions: lpnTabPositions nTabOrigin: nTabOrigin
	"Writes a character string at the specified location, using the currently 
	selected font. 
		LONG TabbedTextOut(
  			HDC hdc,						// handle of device context 
			int X,						// x-coordinate of starting position  
			int Y,							// y-coordinate of starting position  
			LPCTSTR lpString,				// address of string 
			int nCount, 					// number of characters in string 
			int nTabPositions,				// number of tabs in array
			CONST LPINT lpnTabStopPositions, 	// array of tab positions
			int nTabOrigin					// start of tab expansion
		);"

	<stdcall: sdword TabbedTextOutA handle sdword sdword lpstr sdword sdword sdword* sdword>
	^self invalidCall! !
!UserLibrary categoriesFor: #getTabbedTextExtent:lpString:nCount:nTabPositions:lpnTabStopPositions:!public! !
!UserLibrary categoriesFor: #tabbedTextOut:X:Y:lpString:nCount:nTabPositions:lpnTabPositions:nTabOrigin:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

UserLibraryExtension := Object fromBinaryStoreBytes: 
(ByteArray fromHexString: '21535442203120BA000000000000005200000014000000557365724C696272617279457874656E73696F6E')!

"Resources"!

