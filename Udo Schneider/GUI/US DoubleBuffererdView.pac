| package |
package := Package name: 'US DoubleBuffererdView'.
package paxVersion: 1;
	basicComment: '$id: US DoubleBuffererdView 0.008$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.008'.


package classNames
	add: #USDoubleBufferedView;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	yourself).

package!

"Class Definitions"!

View subclass: #USDoubleBufferedView
	instanceVariableNames: 'backSurface requiresRender'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

USDoubleBufferedView guid: (GUID fromString: '{54B0B57F-7902-4EA2-A3EA-5CC81CA16B52}')!
USDoubleBufferedView comment: 'This is a slightly changed DoubleBuffered View which does not draw directly on the Views DC but instead always uses the WM_PAINT provided DC.
'!
!USDoubleBufferedView categoriesForClass!Unclassified! !
!USDoubleBufferedView methodsFor!

canvas
	"Answer a <Canvas> onto the back surface"

	^(backSurface canvas)
		!

flipTo: aCanvas 
	"Private - Flip the current back surface to the front and paint it"

	backSurface 
		drawOn: aCanvas
		at: Point zero
		extent: backSurface extent!

graphics
	"Answer a <GdiplusGraphic> onto the back surface"

	^GdiplusGraphics fromCanvas: self canvas!

initialize
	"Private - Initialise the receiver."

	super initialize.
	backcolor := Color white.
	requiresRender := false.
!

initializeSurfacesFor: aPointExtent
	"Private - Initialize the front and back surfaces for a view size of aPointExtent"

	| canvas |

	backSurface notNil ifTrue: [ backSurface free ].
	canvas := super canvas.
	backSurface := Bitmap compatible: canvas extent: aPointExtent.
	self invalidate.
!

invalidate
	"Flag the current rendition as being invalid. A repaint will cause a
	render to occur"

	requiresRender := true.
	super invalidate!

onCreated: anEvent
	"Private - Handler for view created "

	super onCreated: anEvent.
	self initializeSurfacesFor: self extent.
!

onEraseRequired: aColorEvent
	"Private - Handler for erase background"

	^true!

onPaintRequired: aPaintEvent 
	"Private - Handler for paint event"

	requiresRender ifTrue: [self render].
	self flipTo:aPaintEvent canvas!

onPositionChanged: aPositionEvent
	"Private - Handle a window position change event (move or resize)."

	aPositionEvent isResize ifTrue: [
		self initializeSurfacesFor: aPositionEvent extent.
		self repaint ].
	^super onPositionChanged: aPositionEvent!

refreshContents
	"The model held by the receiver has been changed so repaint" 

	self repaint
!

render
	"Private - Render the background image"

	requiresRender := false
!

repaint
	"Repaints the receiver"

	self
		render; flipTo: super canvas! !
!USDoubleBufferedView categoriesFor: #canvas!accessing!public! !
!USDoubleBufferedView categoriesFor: #flipTo:!operations!private! !
!USDoubleBufferedView categoriesFor: #graphics!accessing!public! !
!USDoubleBufferedView categoriesFor: #initialize!initializing!private! !
!USDoubleBufferedView categoriesFor: #initializeSurfacesFor:!initializing!private! !
!USDoubleBufferedView categoriesFor: #invalidate!operations!public! !
!USDoubleBufferedView categoriesFor: #onCreated:!event handling!private! !
!USDoubleBufferedView categoriesFor: #onEraseRequired:!event handling!private! !
!USDoubleBufferedView categoriesFor: #onPaintRequired:!event handling!private! !
!USDoubleBufferedView categoriesFor: #onPositionChanged:!event handling!private! !
!USDoubleBufferedView categoriesFor: #refreshContents!public!updating! !
!USDoubleBufferedView categoriesFor: #render!operations!private! !
!USDoubleBufferedView categoriesFor: #repaint!operations!public! !

"Binary Globals"!

