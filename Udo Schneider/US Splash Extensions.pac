| package |
package := Package name: 'US Splash Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Splash Extensions 0.005$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.005'.


package classNames
	add: #FadingSplash;
	yourself.

package methodNames
	add: #Splash -> #onEraseRequired:;
	add: #Splash -> #splashDelay;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Splash subclass: #FadingSplash
	instanceVariableNames: 'backSurface finalBitmap splashProcess percent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Splash methodsFor!

onEraseRequired: aColorEvent
	"Private - Handler for erase background"

	^true!

splashDelay
^splashDelay! !
!Splash categoriesFor: #onEraseRequired:!event handling!private! !
!Splash categoriesFor: #splashDelay!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

FadingSplash guid: (GUID fromString: '{DE87C249-26C6-46CD-A649-49152131356A}')!
FadingSplash comment: ''!
!FadingSplash categoriesForClass!Unclassified! !
!FadingSplash methodsFor!

animationDuration
	"Private - Answer the duration (in ms) of the splash animation"

	^1000!

bitmap: aBitmap finalBitmap: aFinalBitmap 
	"Private - Set the bitmap to display in the receiver"

	super bitmap: aBitmap.
	finalBitmap := aFinalBitmap.
	backSurface := Bitmap compatible: self canvas extent: aBitmap extent.
	!

createSplashProcess
	| sleepPeriod |
	sleepPeriod := self animationDuration // 50.
	^
	[self renderFade.
	self preAnimationDelay wait.
	1 to: 100
		by: 2
		do: 
			[:value | 
			Processor sleep: sleepPeriod.
			percent := value.
			self renderFade]] 
			fork!

defaultSplashDelay
	"Answer the default delay that a splash should be displayed for"

	^Delay forMilliseconds: 6000!

defaultWindowStyle
	"Private - Answer the default basic window creation style"

	^##(WS_POPUP)!

forceClose
	"Private - Handler for image save. Insist that the splash is forcefully closed before any image save."

	splashProcess notNil ifTrue: [ splashProcess terminate ].
	super forceClose!

initialize
percent := 0.
^super initialize!

onCreated: anEvent
	"Start the splash animation process"

	splashProcess := self createSplashProcess!

onDestroyed
	splashProcess terminate.
	^super onDestroyed!

onPaintRequired: aPaintEvent
	"Handler for paint event. Show the bitmap and paint the copyright
	and version info over the top."

	| canvas |
	canvas := aPaintEvent canvas.
	backSurface drawOn: canvas at: Point zero.
	canvas free.
!

preAnimationDelay
	"Private - Answer the delay before animation starts"

	^Delay forMilliseconds: 500!

renderFade
	| canvas blend |
	canvas := backSurface canvas.
	bitmap drawOn: canvas at: Point zero.
	blend := BLENDFUNCTION new.
	blend usePerPixelAlpha: false.
	blend blendPercentage: percent.
	finalBitmap 
		alphaBlendOn: canvas
		at: 0 @ 0
		extent: bitmap extent
		from: 0 @ 0
		extent: bitmap extent
		blend: blend.
	self overlayWith ifNotNil: [:overlay | overlay value: canvas].
	canvas free.
	backSurface drawOn: self canvas at: Point zero! !
!FadingSplash categoriesFor: #animationDuration!constants!private! !
!FadingSplash categoriesFor: #bitmap:finalBitmap:!accessing!initializing!private! !
!FadingSplash categoriesFor: #createSplashProcess!event handling!private! !
!FadingSplash categoriesFor: #defaultSplashDelay!constants!public! !
!FadingSplash categoriesFor: #defaultWindowStyle!constants!private! !
!FadingSplash categoriesFor: #forceClose!event handling!private! !
!FadingSplash categoriesFor: #initialize!public! !
!FadingSplash categoriesFor: #onCreated:!event handling!public! !
!FadingSplash categoriesFor: #onDestroyed!event handling!private! !
!FadingSplash categoriesFor: #onPaintRequired:!event handling!public! !
!FadingSplash categoriesFor: #preAnimationDelay!constants!private! !
!FadingSplash categoriesFor: #renderFade!displaying!private! !

!FadingSplash class methodsFor!

bitmap: bitmap finalBitmap: finalBitmap 
	^super new bitmap: bitmap finalBitmap: finalBitmap; yourself!

bitmap: bitmap finalBitmap: finalBitmap overlayWith: aBlock 
	^super new bitmap: bitmap finalBitmap: finalBitmap ; overlayWith: aBlock ; yourself!

new
	"Shows an instance of the receiver which is the dolphin logo"

	| olePictureClass |
	olePictureClass := Smalltalk at: #OLEPicture.
	^super new 
		bitmap: (olePictureClass fromFile: 'Resources/Dolphin0.jpg' usingLocator: FileLocator installRelative) 
				asDIBSection
		finalBitmap: (olePictureClass fromFile: 'Resources/Dolphin1.jpg'
				usingLocator: FileLocator installRelative) asDIBSection! !
!FadingSplash class categoriesFor: #bitmap:finalBitmap:!instance creation!public! !
!FadingSplash class categoriesFor: #bitmap:finalBitmap:overlayWith:!instance creation!public! !
!FadingSplash class categoriesFor: #new!instance creation!public! !

"Binary Globals"!

