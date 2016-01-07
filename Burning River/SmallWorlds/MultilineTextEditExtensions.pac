| package |
package := Package name: 'MultilineTextEditExtensions'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicScriptAt: #postuninstall put: 'Win32Constants
	removeKey: ''EM_LINESCROLL'' ifAbsent: [];
	removeKey: ''EM_SCROLL'' ifAbsent: []'.
package basicScriptAt: #preinstall put: 'Win32Constants
	at: ''EM_LINESCROLL'' put: 16rB6;
	at: ''EM_SCROLL'' put: 16rB5'.

package methodNames
	add: #MultilineTextEdit -> #scroll:;
	add: #MultilineTextEdit -> #scrollHorizontal:;
	add: #MultilineTextEdit -> #scrollLineDown;
	add: #MultilineTextEdit -> #scrollLineUp;
	add: #MultilineTextEdit -> #scrollPageDown;
	add: #MultilineTextEdit -> #scrollPageUp;
	add: #MultilineTextEdit -> #scrollVertical:;
	yourself.

package globalNames
	add: #MultilineTextEditExtensions;
	yourself.

package binaryGlobalNames: (Set new
	add: #MultilineTextEditExtensions;
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!MultilineTextEdit methodsFor!

scroll: scrollCode
	"Scroll the text control vertically.  scrollCode should be one of SB_LINEDOWN,
	 SB_LINEUP, SB_PAGEDOWN, or SB_PAGEUP."

	^self sendMessage: EM_SCROLL wParam: scrollCode lParam: 0!

scrollHorizontal: characterCount
	^self sendMessage: EM_LINESCROLL wParam: characterCount lParam: 0!

scrollLineDown
	self scroll: SB_LINEDOWN!

scrollLineUp
	self scroll: SB_LINEUP!

scrollPageDown
	self scroll: SB_PAGEDOWN!

scrollPageUp
	self scroll: SB_PAGEUP!

scrollVertical: lineCount
	^self sendMessage: EM_LINESCROLL wParam: 0 lParam: lineCount! !
!MultilineTextEdit categoriesFor: #scroll:!operations!public! !
!MultilineTextEdit categoriesFor: #scrollHorizontal:!operations!public! !
!MultilineTextEdit categoriesFor: #scrollLineDown!operations!public! !
!MultilineTextEdit categoriesFor: #scrollLineUp!operations!public! !
!MultilineTextEdit categoriesFor: #scrollPageDown!operations!public! !
!MultilineTextEdit categoriesFor: #scrollPageUp!operations!public! !
!MultilineTextEdit categoriesFor: #scrollVertical:!operations!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

MultilineTextEditExtensions := Object fromBinaryStoreBytes: 
(ByteArray fromHexString: '21535442203120BA00000000000000520000001B0000004D756C74696C696E655465787445646974457874656E73696F6E73')!

"Resources"!

