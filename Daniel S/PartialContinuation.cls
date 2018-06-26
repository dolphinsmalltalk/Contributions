"Filed out from Dolphin Smalltalk 7"!

Object subclass: #PartialContinuation
	instanceVariableNames: 'partialProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PartialContinuation guid: (GUID fromString: '{d35c7398-052d-48be-b020-4a69c24b23eb}')!
PartialContinuation comment: ''!
!PartialContinuation categoriesForClass!Unclassified! !
!PartialContinuation methodsFor!

initializeFrom: topFrame to: bottomFrame 

	| actualBottomFrame |

	"Note bottomFrame is *not* copied - this matches behavior of Pharo equivalent method captureFrom:to:
	This means we have to find the frame whose sender is bottomFrame - this is the actual last frame we copy"
	actualBottomFrame := topFrame.
	[actualBottomFrame sender = bottomFrame] whileFalse: [actualBottomFrame := actualBottomFrame sender].

	self initialize.
	partialProcess := topFrame process copyFrom: topFrame to: actualBottomFrame!

numArgs
	^1.!

value
	^self value: nil.!

value: anObject
	| marker |
	marker := self class markerRequest signal ifNil: [Processor activeProcess topFrame sender].
	
	[| answerProcess |
	answerProcess := marker process.
	answerProcess suspend.
	ProcessPatcher patch: partialProcess onto: marker.
	answerProcess resize: answerProcess size " + 1 removed which broke WAFlowPlatrformTest>>testSuspendCallbackDo (incorrect receiver on resumption). All partial continuation-related tests run green without".
	answerProcess at: answerProcess size put: anObject.
	answerProcess topFrame sp: answerProcess size.
	"Useful for working out what's going on:
		answerProcess topFrame stackTrace: 999
		answerProcess debug"
	answerProcess resume]
			forkAt: Processor highIOPriority.!

valueWithArguments: anArray 
	^anArray size = 1 
		ifTrue: [self value: anArray first]
		ifFalse: [self error: 'Continuations can only be resumed with one argument.']! !
!PartialContinuation categoriesFor: #initializeFrom:to:!pharo!public! !
!PartialContinuation categoriesFor: #numArgs!pharo!public! !
!PartialContinuation categoriesFor: #value!pharo!public! !
!PartialContinuation categoriesFor: #value:!pharo!public! !
!PartialContinuation categoriesFor: #valueWithArguments:!pharo!public! !

!PartialContinuation class methodsFor!

currentDo: aBlock
	| marker |
	marker := self markerRequest signal ifNil: [self markerNotFound].
	^aBlock value: (self from: Processor activeProcess topFrame sender to: marker).!

from: aSourceContext to: aTargetContext 
	^self basicNew initializeFrom: aSourceContext to: aTargetContext.!

markerNotFound

	self error: 'Delimiter not found when capturing partial continuation.'!

markerRequest

	^ContinuationDelimiterRequest! !
!PartialContinuation class categoriesFor: #currentDo:!instance creation!public! !
!PartialContinuation class categoriesFor: #from:to:!instance creation!private! !
!PartialContinuation class categoriesFor: #markerNotFound!public! !
!PartialContinuation class categoriesFor: #markerRequest!public! !

