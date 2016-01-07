| package |
package := Package name: 'MVP Base Extensions'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package methodNames
	add: #Canvas -> #tabbedTextExtent:tabPositions:;
	add: #Canvas -> #text:at:tabPositions:;
	yourself.

package globalNames
	add: #MVPBaseExtension;
	yourself.

package binaryGlobalNames: (Set new
	add: #MVPBaseExtension;
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Canvas methodsFor!

tabbedTextExtent: string tabPositions: anArrayOfIntegers
	"Answer the extent of string using the receivers currently selected font."

	| nTabs tabPositions extent |

	anArrayOfIntegers notNil
		ifTrue:
			[ nTabs := anArrayOfIntegers size.
			tabPositions := SDWORDArray new: nTabs.
			1 to: nTabs do: [ :n | tabPositions at: n put: (anArrayOfIntegers at: n) ] ]
		ifFalse: [ nTabs := 0 ].

	extent := UserLibrary default 
			getTabbedTextExtent: self asParameter
			lpString: string
			nCount: string size
			nTabPositions: nTabs
			lpnTabStopPositions: tabPositions.
	extent = 0 ifTrue: [^GDILibrary default systemError].

	^Point x: extent lowWord y: extent highWord!

text: aString at: aPoint tabPositions: anArrayOfIntegers
	"Draw aString on the receiver starting at aPoint.  anArrayOfIntegers is assumed to
	contain the desired tab positions, in logical units.  "

	| nTabs tabPositions |

	anArrayOfIntegers notNil
		ifTrue:
			[ nTabs := anArrayOfIntegers size.
			tabPositions := SDWORDArray new: nTabs.
			1 to: nTabs do: [ :n | tabPositions at: n put: (anArrayOfIntegers at: n) ] ]
		ifFalse: [ nTabs := 0 ].

	^UserLibrary default
		tabbedTextOut: self asParameter
		X: aPoint x
		Y: aPoint y
		lpString: aString
		nCount: aString size
		nTabPositions: nTabs
		lpnTabPositions: tabPositions
		nTabOrigin: 0! !
!Canvas categoriesFor: #tabbedTextExtent:tabPositions:!enquiries!public! !
!Canvas categoriesFor: #text:at:tabPositions:!drawing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

MVPBaseExtension := Object fromBinaryStoreBytes: 
(ByteArray fromHexString: '21535442203120BA0000000000000052000000100000004D565042617365457874656E73696F6E')!

"Resources"!

