| package |
package := Package name: 'US Runtime Patches'.
package paxVersion: 1;
	basicComment: '$id: US Runtime Patches 0.026$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Patches several glitches (mainly UI) in Dolphin Applications

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '0.026'.


package methodNames
	add: #CheckBox -> #onColorRequired:;
	add: #Icon -> #copyExtent:;
	add: #Icon -> #copyExtentLarge;
	add: #Icon -> #copyExtentSmall;
	add: #Icon -> #createHandleWithExtent:;
	add: #Icon -> #drawOn:at:extent:frame:background:flags:;
	add: #Icon -> #loadFromInstance:extent:;
	add: #ShellView -> #updateIcons;
	add: #TextEdit -> #positionForKeyboardContextMenu;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: 'Graphics\US GDI Extensions';
	yourself).

package setManualPrerequisites: #(
	'US GDI Extensions').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!CheckBox methodsFor!

onColorRequired: aColorEvent 
	"Private - Workaround XP bug whereby themed checkboxes are drawn against a
	solid black background when using a null brush."

	| canvas back brush |
	
	#USAdded.
	back := self basicActualBackcolor.
	canvas := aColorEvent canvas.
	self forecolor ifNotNil: [:fore | canvas forecolor: fore].
	back isDefault ifTrue: [^nil].
	brush := back brush.
	back isNone 
		ifTrue: 
			[self isThemed 
				ifTrue: 
					[ThemeLibrary default 
						drawThemeParentBackground: self asParameter
						hdc: canvas asParameter
						prc: nil].
			canvas backgroundMode: TRANSPARENT]
		ifFalse: [canvas backcolor: back].
	canvas brush: brush.
	^brush asParameter! !
!CheckBox categoriesFor: #onColorRequired:!*-not in class package!private! !

!Icon methodsFor!

copyExtent: aPoint 
	"Returns a copy of the icon with an preferred extent"

	| newIcon |
	#USAdded.
	self extent = aPoint ifTrue: [^self].
	newIcon := (self class fromOwnedHandle: (self createHandleWithExtent: aPoint))
				fileLocator: self fileLocator;
				identifier: self identifier;
				instanceHandle: self instanceHandle;
				yourself.
				newIcon extent = aPoint ifFalse: [^self]
				.
				^newIcon!

copyExtentLarge
	"Returns a copy of the icon with an large Icon  extent"

	#USAdded.
	^self copyExtent: Icon largeExtent!

copyExtentSmall
	"Returns a copy of the icon with an small Icon extent"

	#USAdded.
	^self copyExtent: Icon smallExtent!

createHandleWithExtent: extent 
	"Returns a new handle of the icon with an preferred extent"

	| spec |
	#USAdded.
	instanceHandle notNil 
		ifTrue: [(self loadFromInstance: instanceHandle extent: extent) ifNotNil: [:hImage | ^hImage]].
	spec := self fileSpec.
	spec isInteger ifFalse: [(self loadFromFile: spec extent: extent) ifNotNil: [:hImage | ^hImage]].
	^(self loadFromInstance: SessionManager current defaultResourceLibrary extent: extent) 
		ifNil: [self class question handle]!

drawOn: aCanvas at: aPoint extent: sizePoint frame: anInteger background: aBrush flags: flagsInteger 
	"Copies the receiver to aCanvas at position aPoint with size sizePoint
	with frame anInteger and background aBrush. Answer whether the frame
	could be drawn. Note that the frame number is ignored if the receiver is not
	an animated icon/cursor."

	^UserLibrary default 
		drawIconEx: aCanvas asParameter
		xLeft: aPoint x
		yTop: aPoint y
		hIcon: ((self copyExtent: sizePoint) ) asParameter
		cxWidth: sizePoint x
		cyHeight: sizePoint y
		istepIfAniCur: anInteger
		hbrFlickerFreeDraw: aBrush asParameter
		diFlags: flagsInteger!

loadFromInstance: hModule extent: aPoint 
	^UserLibrary default 
		loadImage: hModule asParameter
		lpszName: identifier asParameter
		uType: self imageType
		cxDesired: aPoint x
		cyDesired: aPoint y
		fuLoad: LR_DEFAULTCOLOR! !
!Icon categoriesFor: #copyExtent:!*-not in class package!public! !
!Icon categoriesFor: #copyExtentLarge!*-not in class package!public! !
!Icon categoriesFor: #copyExtentSmall!*-not in class package!public! !
!Icon categoriesFor: #createHandleWithExtent:!*-not in class package!public! !
!Icon categoriesFor: #drawOn:at:extent:frame:background:flags:!drawing-bitmaps!public! !
!Icon categoriesFor: #loadFromInstance:extent:!*-not in class package!public! !

!ShellView methodsFor!

updateIcons
	"Private - Update the large and small icons of the receiver"

	"Patched to force the system to use small and large extent Icons"

	| actualLargeIcon actualSmallIcon |

	#USChanged.
	actualLargeIcon := (largeIcon notNil ifTrue: [largeIcon] ifFalse: [self icon]) copyExtentLarge.
	self propertyAt: #largeIcon put: actualLargeIcon.
	self 
		sendMessage: WM_SETICON
		wParam: 1
		lParam: actualLargeIcon asParameter.
	smallIcon notNil 
		ifTrue: 
			[actualSmallIcon := smallIcon copyExtentSmall.
			self propertyAt: #smallIcon put: actualSmallIcon.
			self 
				sendMessage: WM_SETICON
				wParam: 0
				lParam: actualSmallIcon asParameter]! !
!ShellView categoriesFor: #updateIcons!*-not in class package!accessing!public! !

!TextEdit methodsFor!

positionForKeyboardContextMenu
	"Answer the desired position for a context menu requested from the keyboard.
	This should be based on the 'current selection', whatever that means in the context of the
	receiver. Should be overridden by subclasses as appropriate."

	"^(self mapPoint: (self positionOfChar: self caretPosition) to: self class desktop) + (0 @ 20)"

	^
	[(self mapPoint: (self positionOfChar: (self caretPosition min: self value size))
		to: self class desktop) + (0 @ 20)] 
			on: Error
			do: [:ex | self mapPoint: self clientRectangle center to: self class desktop ]! !
!TextEdit categoriesFor: #positionForKeyboardContextMenu!enquiries!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

