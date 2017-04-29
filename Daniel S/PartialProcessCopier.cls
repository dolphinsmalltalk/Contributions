"Filed out from Dolphin Smalltalk 7"!

AbstractPartialProcessCopier subclass: #PartialProcessCopier
	instanceVariableNames: 'topFrame'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PartialProcessCopier guid: (GUID fromString: '{7c851762-ac1f-4cd5-be37-4e777f9ec4a6}')!
PartialProcessCopier comment: ''!
!PartialProcessCopier categoriesForClass!Unclassified! !
!PartialProcessCopier methodsFor!

copyStackFrom: inputTopFrame to: inputBaseFrame 
	topFrame := inputTopFrame.
	baseFrame := inputBaseFrame.
	source := topFrame process.
	clone := source shallowCopy postCopy.
	seamIndex := baseFrame bp - 2.
	delta := (clone spOfIndex: 1) - (baseFrame basicBP - 2).	"i.e. (source spOfIndex: baseFrame bp - 1) -- but this is faster"
	self
		createBasicCopyOfStack;
		initializeCopiedMap;
		initializeIndexMap;
		cutOffExceptionHandlers;
		patchFrames;
		patchStackSlots;
		patchExceptionHandlers.
	"The clone is not yet valid and should *not* be finalizable (it will crash the VM on unwind). Just return it"
	^clone.!

createBasicCopyOfStack
	1 to: topFrame sp - seamIndex do: [:i | clone at: i put: (clone at: seamIndex + i)].
	(clone frameAtIndex: baseFrame index - seamIndex) basicSender: 0.
	clone
		resize: topFrame sp - seamIndex;
		suspendedFrame: topFrame index - seamIndex;
		name: 'Partial copy of ' , source id printString , ':' , source name displayString!

cutOffExceptionHandlers
	| exceptionHandler |
	exceptionHandler := source exceptionEnvironment.
	exceptionHandler ifNil: [^self].
	exceptionHandler outer ifNil: [copied at: exceptionHandler put: ExceptionHandlerChainCookie].
	
	[| correspondingSourceFrame |
	correspondingSourceFrame := source
				frameAtAddress: (exceptionHandler instVarNamed: 'returnContinuation') home outer.
	correspondingSourceFrame sender homeFrame index >= baseFrame index]
			whileTrue: [exceptionHandler := exceptionHandler outer].
	copied at: exceptionHandler put: ExceptionHandlerChainCookie.!

initializeIndexMap
	indexMap := IdentityDictionary new.
	1 to: seamIndex
		do: [:i | (source at: i) ifNotNil: [:a | a isInteger ifFalse: [indexMap at: a put: i]]]!

patchFrames
	| copyFrame |
	copyFrame := clone suspendedFrame.
	[copyFrame isNil] whileFalse: 
			[copyFrame rebase: self.
			copyFrame := copyFrame sender]!

patchStackSlots
	1 to: clone size
		do: 
			[:i | 
			| each |
			each := source at: i + seamIndex.
			each isInteger ifFalse: [clone at: i put: (self visit: each)]]! !
!PartialProcessCopier categoriesFor: #copyStackFrom:to:!public! !
!PartialProcessCopier categoriesFor: #createBasicCopyOfStack!copying!private! !
!PartialProcessCopier categoriesFor: #cutOffExceptionHandlers!public! !
!PartialProcessCopier categoriesFor: #initializeIndexMap!public! !
!PartialProcessCopier categoriesFor: #patchFrames!copying!private! !
!PartialProcessCopier categoriesFor: #patchStackSlots!copying!private! !

!PartialProcessCopier class methodsFor!

copyStackFrom: topFrame to: bottomFrame 
	^self new copyStackFrom: topFrame to: bottomFrame! !
!PartialProcessCopier class categoriesFor: #copyStackFrom:to:!public! !

