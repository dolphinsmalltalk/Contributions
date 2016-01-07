| package |
package := Package name: 'US EstimatingProgressDialog'.
package paxVersion: 1;
	basicComment: '$id: US EstimatingProgressDialog 0.005$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This goodie adds a elapsed/remaining time display to EstimatingProgressDialog.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.005'.


package classNames
	add: #EstimatingProgressDialog;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Dialogs\Progress\Dolphin Progress Dialog';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	yourself).

package!

"Class Definitions"!

ProgressDialog subclass: #EstimatingProgressDialog
	instanceVariableNames: 'remainingPresenter startTime lastUpdate updateProcess getMessageBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

EstimatingProgressDialog guid: (GUID fromString: '{386F13E6-7ECA-42A8-8C08-64881CAA5356}')!
EstimatingProgressDialog comment: ''!
!EstimatingProgressDialog categoriesForClass!MVP-Presenters! !
!EstimatingProgressDialog methodsFor!

createComponents
super createComponents.
remainingPresenter := self add: TextPresenter new name: 'remaining'!

getMessageBlock
	^getMessageBlock!

getMessageBlock: anObject
	getMessageBlock := anObject!

initialize
	startTime := Time now.
	lastUpdate := Time now -> 0.
	getMessageBlock := [ :elapsed :remaining | 
		 elapsed displayString , ' elapsed, ' , remaining displayString , ' remaining'].
	updateProcess := [
			[self updateRemaining.
			(Delay forSeconds: 1) wait] repeat] newProcess.
	^super initialize!

onViewClosed
self terminateUpdateProcess.
^super onViewClosed!

onViewOpened
self startUpdateProccess.
^super onViewOpened!

startUpdateProccess
	updateProcess resume!

terminateUpdateProcess
	updateProcess terminate!

text: progressText 
	self updateRemaining.
	^super text: progressText!

updateRemaining
	| elapsedTime remainingTime |
	elapsedTime := Time now subtractTime: startTime.
	lastUpdate value > 0 
		ifTrue: 
			[remainingTime := Time 
						fromMilliseconds: elapsedTime asMilliseconds / lastUpdate value * (100 - lastUpdate value)]
		ifFalse: [remainingTime := Time fromMilliseconds: 0].
	remainingPresenter 
		value: (getMessageBlock value: elapsedTime  value: remainingTime )!

value: progressValue 
lastUpdate := Time now -> progressValue.
	self updateRemaining.
	^super value: progressValue! !
!EstimatingProgressDialog categoriesFor: #createComponents!public! !
!EstimatingProgressDialog categoriesFor: #getMessageBlock!accessing!public! !
!EstimatingProgressDialog categoriesFor: #getMessageBlock:!accessing!public! !
!EstimatingProgressDialog categoriesFor: #initialize!private! !
!EstimatingProgressDialog categoriesFor: #onViewClosed!public! !
!EstimatingProgressDialog categoriesFor: #onViewOpened!public! !
!EstimatingProgressDialog categoriesFor: #startUpdateProccess!public! !
!EstimatingProgressDialog categoriesFor: #terminateUpdateProcess!public! !
!EstimatingProgressDialog categoriesFor: #text:!accessing!public! !
!EstimatingProgressDialog categoriesFor: #updateRemaining!public! !
!EstimatingProgressDialog categoriesFor: #value:!accessing!public! !

!EstimatingProgressDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 25690113 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 8 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 592 0 0 0 7 0 0 0 592 0 8 4294905359 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  31 151 850 671 41 592 786 8 #text: 98 1 8 'Remaing time' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 75 0 0 0 94 1 0 0 95 0 0 0] 98 0 850 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 31 1082 1104 8 #fixedParentRight -27 1082 1104 8 #fixedParentTop 151 1082 1104 8 #fixedViewTop 41 410 608 98 16 0 416 98 2 8 1140850944 1 1232 0 0 0 7 0 0 0 1232 0 8 4294905359 690 0 0 0 722 202 208 98 2 786 816 98 2 850 35 27 850 665 61 1232 786 912 98 1 8 'Description of operation goes here
Up to two lines of text permitted' 1232 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 17 0 0 0 13 0 0 0 93 1 0 0 43 0 0 0] 98 0 1024 0 27 1042 1088 35 1136 -29 1168 27 1200 61 410 8 ##(Smalltalk.ProgressBar)  98 15 0 416 98 2 8 1140850688 1 1552 721990 2 ##(Smalltalk.ValueHolder)  0 0 1082 8 ##(Smalltalk.SearchPolicy)  8 #never 1 482 512 0 7 0 0 0 1552 0 8 4294906247 690 0 0 722 202 208 98 2 786 816 98 2 850 31 101 850 671 41 1552 786 8 #range: 98 1 525062 ##(Smalltalk.Interval)  1 201 3 1552 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 50 0 0 0 94 1 0 0 70 0 0 0] 98 0 1024 0 27 1042 1088 31 1136 -27 1168 101 1200 41 410 8 ##(Smalltalk.PushButton)  98 17 0 416 98 2 8 1140924416 1 2016 0 482 512 0 7 0 0 0 2016 0 8 4294903935 1180998 4 ##(Smalltalk.CommandDescription)  8 #cancel 8 '&Cancel' 1 1 0 0 32 722 202 208 98 2 786 816 98 2 850 535 207 850 161 51 2016 786 912 98 1 8 '&Cancel' 2016 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 11 1 0 0 103 0 0 0 91 1 0 0 128 0 0 0] 98 0 1024 0 27 1042 1082 1104 8 #fixedViewRight -159 1136 -33 1082 1104 8 #fixedViewBottom -49 1082 1104 8 #fixedParentBottom -21 234 256 98 6 592 8 'remaining' 1232 8 'text' 1552 8 'progress' 0 0 0 0 0 3 0 0 0 850 511 291 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1996367669 722 202 208 98 3 786 816 98 2 850 3359 21 850 761 351 416 786 912 98 1 8 'Please wait...' 416 786 8 #updateMenuBar 98 0 416 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 11 8 0 0 185 0 0 0] 98 4 1232 1552 592 2016 1024 0 27 )! !
!EstimatingProgressDialog class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

