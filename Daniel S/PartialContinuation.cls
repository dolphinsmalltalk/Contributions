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
	self initialize.
	partialProcess := topFrame process copyFrom: topFrame to: bottomFrame!

numArgs
	^1.!

value
	^self value: nil.!

value: anObject
	| marker |
	marker := ContinuationDelimiterRequest signal ifNil: [Processor activeProcess topFrame sender].
	
	[| answerProcess |
	answerProcess := marker process.
	answerProcess suspend.
	ProcessPatcher patch: partialProcess onto: marker.
	answerProcess resize: answerProcess size + 1.
	answerProcess at: answerProcess size put: anObject.
	answerProcess topFrame sp: answerProcess size.
	answerProcess resume]
			forkAt: Processor highIOPriority.!

valueWithArguments: anArray 
	^anArray size = 1 
		ifTrue: [self value: anArray first]
		ifFalse: [self error: 'Continuations can only be resumed with one argument.']!

valueWithPossibleArguments: anArray 
	^self value: anArray first.! !
!PartialContinuation categoriesFor: #initializeFrom:to:!pharo!public! !
!PartialContinuation categoriesFor: #numArgs!pharo!public! !
!PartialContinuation categoriesFor: #value!pharo!public! !
!PartialContinuation categoriesFor: #value:!pharo!public! !
!PartialContinuation categoriesFor: #valueWithArguments:!pharo!public! !
!PartialContinuation categoriesFor: #valueWithPossibleArguments:!pharo!public! !

!PartialContinuation class methodsFor!

currentDo: aBlock
	| marker |
	marker := ContinuationDelimiterRequest signal
				ifNil: [self error: 'Delimiter not found when capturing partial continuation.'].
	^aBlock value: (self from: Processor activeProcess topFrame sender to: marker).!

from: aSourceContext to: aTargetContext 
	^self basicNew initializeFrom: aSourceContext to: aTargetContext.! !
!PartialContinuation class categoriesFor: #currentDo:!public! !
!PartialContinuation class categoriesFor: #from:to:!public! !

