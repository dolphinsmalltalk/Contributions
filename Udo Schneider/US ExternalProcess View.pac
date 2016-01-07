| package |
package := Package name: 'US ExternalProcess View'.
package paxVersion: 1;
	basicComment: '$id: US ExternalProcess View 0.012$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Based on http://www.codeproject.com/KB/dialog/exeHosting.aspx

See ExternalProcessPresenter class>>example1

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.012'.


package classNames
	add: #ExternalProcessPresenter;
	add: #ExternalProcessView;
	yourself.

package methodNames
	add: #ExternalProcess -> #topLevelWindows;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Burning River\ExternalProcess\ExternalProcess';
	add: 'US ExternalProcess Extensions';
	add: 'GUI\US View Extensions';
	yourself).

package setManualPrerequisites: #(
	'US ExternalProcess Extensions'
	'US View Extensions').

package!

"Class Definitions"!

ValuePresenter subclass: #ExternalProcessPresenter
	instanceVariableNames: 'applicationIsAlive'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #ExternalProcessView
	instanceVariableNames: 'process processView'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ExternalProcess methodsFor!

topLevelWindows
	^View  topLevelWindows 
		select: [:each | each threadId = pi dwThreadId and: [each isWindowVisible]]! !
!ExternalProcess categoriesFor: #topLevelWindows!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ExternalProcessPresenter guid: (GUID fromString: '{86B93CED-F329-4F61-85A5-E1C061D4327B}')!
ExternalProcessPresenter comment: ''!
!ExternalProcessPresenter categoriesForClass!Unclassified! !
!ExternalProcessPresenter methodsFor!

createSchematicWiring
self when: #timerTick:
			send: #onTimerTick:
			to: self.
	super createSchematicWiring!

killUpdateTimer
	self view killTimer: 1!

onTimerTick: anInteger 
	| newStatus |
	self value ifNil: [newStatus  := nil. ^self].
	newStatus := self value isAlive.
	(newStatus ~= applicationIsAlive and: [newStatus = false]) 
		ifTrue: [self trigger: #applicationKilled].
	applicationIsAlive := newStatus!

onViewClosed
	self killUpdateTimer.
	^super onViewClosed!

onViewOpened

	self startUpdateTimer.
	^super onViewOpened!

startUpdateTimer
	self view setTimer: 1 interval: 1000! !
!ExternalProcessPresenter categoriesFor: #createSchematicWiring!public! !
!ExternalProcessPresenter categoriesFor: #killUpdateTimer!helpers!private! !
!ExternalProcessPresenter categoriesFor: #onTimerTick:!event handling!private! !
!ExternalProcessPresenter categoriesFor: #onViewClosed!public! !
!ExternalProcessPresenter categoriesFor: #onViewOpened!public! !
!ExternalProcessPresenter categoriesFor: #startUpdateTimer!helpers!private! !

!ExternalProcessPresenter class methodsFor!

example1
	"
	ExternalProcessPresenter example1
	"

	| externalProcess |
	externalProcess := (ExternalProcess new)
				commandLine: ('C:\windows\system32\sol.exe');
				yourself.
	ExternalProcessPresenter showOn: externalProcess!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ExternalProcessView)  98 14 0 0 98 2 8 1174405120 1 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 416 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 674 2561 1601 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 0 5 0 0 32 3 0 0] 98 0 674 193 193 0 27 )! !
!ExternalProcessPresenter class categoriesFor: #example1!examples!must strip!public! !
!ExternalProcessPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

ExternalProcessView guid: (GUID fromString: '{03378A5A-B7A5-4D74-B43D-8D90567DB4EF}')!
ExternalProcessView comment: ''!
!ExternalProcessView categoriesForClass!Unclassified! !
!ExternalProcessView methodsFor!

attachView
	processView ifNil: [^self].
	self addSubView: processView.
	processView
		setParent: self;
		setWindowStyle: WS_VISIBLE ;
		position: 0 @ 0.
	self updateProcessViewSize.
	processView invalidate.
	self invalidate!

basicGetProcessView
	| windows |
	process ifNil: [^nil].
	10 timesRepeat: 
			[windows := process topLevelWindows.
			windows notEmpty ifTrue: [^windows first]
			. Processor  sleep: 2000].
	windows isEmpty ifTrue: [self error: 'Top Level Window not found']!

connectModel
	"Connect the receiver to its model, wiring events, etc.
	ValueConvertingControlViews expect to be connected to a model that obeys the
	ValueModel protocol; i.e. it must generate a #value event when the value it is wrapping 
	is replaced."

	self model 
		ifNotNil: 
			[:m | 
			m 
				when: #valueChanged
				send: #onModelChanged
				to: self]!

defaultExtent
	"Answer the default size of the receiver"

	^View desktop extent // 1.5!

defaultWindowStyle
	"Private - Answer a default base style to use for the receiver"

	^super defaultWindowStyle bitOr: WS_CLIPCHILDREN!

detachView: aView 
	aView ifNil: [^self].
	aView
	
		destroy!

killProcess: aProcess 
	aProcess ifNil: [^self].
	
			aProcess isAlive ifTrue: [aProcess kill]!

onEraseRequired: aColorEvent
	"Private - Handler for erase background"

	^true!

onModelChanged
	self detachView: processView.
	processView := nil.
	self killProcess: process.
	
			process := self value.
			self
				startProcess;
				waitForProcessInputIdle.
			processView := self basicGetProcessView.
			self
				attachView;
				updateProcessViewSize!

onPaintRequired: aPaintEvent 
	processView ifNotNil: [:value | value invalidate]

	!

onPositionChanged: aPositionEvent 
	"Private - Handle a window position change event (move or resize)."

	aPositionEvent isResize ifTrue: [self updateProcessViewSize].
	
	^super onPositionChanged: aPositionEvent!

onViewCreated
self model ifNotNil: [:value | 
	self onModelChanged].
	^super onViewCreated!

onViewDestroyed
	self detachView: processView; killProcess: process.
	
	^super onViewDestroyed!

process
	^process!

processCaptionOrNil
	^processView ifNotNil: [:value | value displayString]!

processLargeIconOrNil
	^processView ifNotNil: [:value | processView basicLargeIcon]!

processSmallIconOrNil
	^processView ifNotNil: [:value | value basicSmallIcon]!

processView
	^processView!

startProcess
	process ifNil: [^self].
	process
		showWindow: SW_MINIMIZE;
		startupFlags: STARTF_USESHOWWINDOW;
		executeAsync!

updateProcessViewSize
	processView ifNil: [^self].
	processView extent: self clientRectangle extent!

value
	"Answer the receiver's model value"

	^self model value!

value: anObject 
"Set the receiver's model value to anObject"

	self model value: anObject!

waitForProcessInputIdle
	| ret |

	process ifNil: [^self].
	[(ret := process waitForInputIdle: 500) = WAIT_TIMEOUT] 
		whileTrue: [SessionManager current inputState pumpMessages].
	ret = WAIT_FAILED ifTrue: [self error: 'WaitForInputIdle failed']! !
!ExternalProcessView categoriesFor: #attachView!helpers!public! !
!ExternalProcessView categoriesFor: #basicGetProcessView!helpers!private! !
!ExternalProcessView categoriesFor: #connectModel!public! !
!ExternalProcessView categoriesFor: #defaultExtent!constants!public! !
!ExternalProcessView categoriesFor: #defaultWindowStyle!public! !
!ExternalProcessView categoriesFor: #detachView:!helpers!private! !
!ExternalProcessView categoriesFor: #killProcess:!helpers!private! !
!ExternalProcessView categoriesFor: #onEraseRequired:!event handling!public! !
!ExternalProcessView categoriesFor: #onModelChanged!public!updating! !
!ExternalProcessView categoriesFor: #onPaintRequired:!event handling!public! !
!ExternalProcessView categoriesFor: #onPositionChanged:!event handling!public! !
!ExternalProcessView categoriesFor: #onViewCreated!event handling!public! !
!ExternalProcessView categoriesFor: #onViewDestroyed!event handling!public! !
!ExternalProcessView categoriesFor: #process!accessing!private! !
!ExternalProcessView categoriesFor: #processCaptionOrNil!public! !
!ExternalProcessView categoriesFor: #processLargeIconOrNil!public! !
!ExternalProcessView categoriesFor: #processSmallIconOrNil!public! !
!ExternalProcessView categoriesFor: #processView!accessing!private! !
!ExternalProcessView categoriesFor: #startProcess!helpers!public! !
!ExternalProcessView categoriesFor: #updateProcessViewSize!helpers!private! !
!ExternalProcessView categoriesFor: #value!accessing!public! !
!ExternalProcessView categoriesFor: #value:!accessing!public! !
!ExternalProcessView categoriesFor: #waitForProcessInputIdle!helpers!private! !

"Binary Globals"!

