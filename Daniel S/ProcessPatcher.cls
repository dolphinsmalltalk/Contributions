"Filed out from Dolphin Smalltalk 7"!

AbstractPartialProcessCopier subclass: #ProcessPatcher
	instanceVariableNames: 'patchFrames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProcessPatcher guid: (GUID fromString: '{dcafe68c-9699-4132-aeea-3a66c4e53de9}')!
ProcessPatcher comment: ''!
!ProcessPatcher categoriesForClass!Unclassified! !
!ProcessPatcher methodsFor!

findMethodContextForBlock: aBlock beforeFrame: aStackFrame 
	#deprecated.
	"| currentFrame |
	currentFrame := aStackFrame.
	[currentFrame notNil] whileTrue: 
			[(currentFrame method = aBlock method 
				and: [currentFrame environment class = Context and: [currentFrame environment block isNil]]) 
					ifTrue: [^currentFrame environment].
			currentFrame := currentFrame sender].
	self error: 'Couldn''t find plausible Context for ' , aBlock displayString."!

fixOuterForBlock: aBlock beforeFrame: aStackFrame 
	#deprecated.
	"aBlock outer = nil | (aBlock outer = 0) | (fixedBlocks includes: aBlock) ifTrue: [^self].
	aBlock outer block 
		ifNotNil: [:outerBlock | ^self fixOuterForBlock: outerBlock beforeFrame: aStackFrame].
	aBlock outer: (self findMethodContextForBlock: aBlock beforeFrame: aStackFrame).
	fixedBlocks add: aBlock."!

initializeIndexMap
	indexMap := IdentityDictionary new.
	1 to: seamIndex
		do: [:i | (clone at: i) ifNotNil: [:a | a isInteger ifFalse: [indexMap at: a put: i]]]!

pasteBasicCopyOfStack
	| firstPastedFrame |
	clone resize: seamIndex + source size.
	1 to: source size
		do: 
			[:i | 
			| srcVal |
			srcVal := source at: i.
			srcVal isInteger ifFalse: [srcVal := self visit: srcVal].
			clone at: seamIndex + i put: srcVal].
	firstPastedFrame := clone frameAtIndex: seamIndex + patchFrames first index.
	firstPastedFrame sender: baseFrame.
	firstPastedFrame basicSender: firstPastedFrame basicSender - delta.	"Have to intentionally break this frame so that when patchFrames fixes it the result is what we want"
	clone suspendedFrame: (clone frameAtIndex: source topFrame index + seamIndex)!

patch: aPartialProcess onto: aStackFrame 

	baseFrame := aStackFrame. 
	"Previously baseFrame was 'aStackFrame sender' with the comment '??? Need to avoid ending up with duplicate stack frames?'
	Stack capture has now been changed to be exclusive of 'to' frame (see PartialContinuation>>initializeFrom:to:) which has removed the need for sender"
	
	source := aPartialProcess.
	clone := baseFrame process.
	seamIndex := baseFrame sp.
	patchFrames := (source stackFramesFrom: source topFrame depth: SmallInteger maximum) reverse.
	delta := baseFrame basicSP + 2 - (source spOfIndex: 1).
	""
	self
		unwindExistingStack;
		initializeCopiedMap;
		initializeIndexMap;
		patchExceptionHandlers;
		pasteBasicCopyOfStack;
		patchFrames.	";
		patchContexts."
	(clone name displayString findString: 'Patched') == 0 
		ifTrue: [clone name: 'Patched ' , clone name displayString].!

patchContexts
	#deprecated.
	"Seems to be obsoleted by reusing things that first came into existence outside the continuation...but you never know..."

	"fixedBlocks := IdentitySet new.
	patchFrames do: 
			[:sourceFrame | 
			| targetFrame |
			targetFrame := clone frameAtIndex: sourceFrame index + seamIndex.
			targetFrame bp - 1 to: targetFrame sp
				do: 
					[:i | 
					| block |
					block := clone at: i.
					block class = BlockClosure ifTrue: [self fixOuterForBlock: block beforeFrame: targetFrame]]]."!

patchExceptionHandlers
	copied at: ExceptionHandlerChainCookie put: clone exceptionEnvironment.
	super patchExceptionHandlers.!

patchFrames
	| copyFrame |
	copyFrame := clone suspendedFrame.
	[copyFrame notNil and: [copyFrame index > seamIndex]] whileTrue: 
			[copyFrame rebase: self.
			copyFrame := copyFrame sender]!

unwindExistingStack
	| frame |
	frame := clone suspendedFrame.
	[frame index > baseFrame index] whileTrue: 
			["Magic index--this is the special marker block used by #valueOnUnwind:."
			frame receiver == (VMLibrary default registry at: 115)
				ifTrue: 
					[| protectedBlock handlerBlock valueOnUnwindBlock newFrame semaphore offsetFrame |
					self assert: 
							[frame method = (BlockClosure >> #ifCurtailed:)
								and: [frame environment isNil or: [frame environment isZero]]].
					protectedBlock := clone at: frame sp + 1.
					handlerBlock := frame argAt: 1.
					valueOnUnwindBlock := clone at: frame sp + 2.
					semaphore := Semaphore new.
					""
					clone resize: frame sp + 12.	"Trust me, it works (I think :))"
					""
					offsetFrame := clone frameAtIndex: frame index + 1.
					offsetFrame bp: frame bp.
					offsetFrame sp: frame sp + 1.
					offsetFrame sender: frame sender.
					offsetFrame environment: 0.
					offsetFrame method: BlockClosure >> #ifCurtailed:thenSignal:.
					offsetFrame at: 2 put: semaphore.
					"offsetFrame at: 1 put: ifCurtailedBlock."	"Already there"
					offsetFrame receiver: protectedBlock.
					"Last instruction of #ifCurtailed:thenSignal:, which is bigger than regular #ifCurtailed:"
					offsetFrame ip: 25.
					""
					newFrame := clone frameAtIndex: offsetFrame sp + 6.
					newFrame method: BlockClosure >> #ifCurtailed:thenSignal:.
					"One higher than the equivalent in #ifCurtailed: because of the extra arg"
					valueOnUnwindBlock initialIP: 10.
					valueOnUnwindBlock method: BlockClosure >> #ifCurtailed:thenSignal:.
					newFrame environment: valueOnUnwindBlock.
					newFrame sender: offsetFrame.
					newFrame ip: valueOnUnwindBlock initialIP.
					newFrame sp: clone size.
					newFrame bp: offsetFrame sp + 2.
					"Doesn't matter, no references to it, but I think this is correct"
					newFrame receiver: protectedBlock receiver.
					newFrame at: 1 put: nil.
					"Again, doesn't matter, but technically the baseFrame is where we're trying to 'return' to"
					newFrame at: 2 put: baseFrame index.
					newFrame at: 3 put: semaphore.
					newFrame at: 4 put: handlerBlock.
					clone suspendedFrame: newFrame.
					""
					clone resume.
					semaphore wait.
					clone suspend.
					frame := clone topFrame.
					"If the unwind overshot the frame we're trying to patch onto,
					just abandon the patch attempt (this is what Pharo seems to do...)"
					(frame sender method = (BlockClosure >> #ifCurtailed:) and: [(frame sender at: 2) < baseFrame index])
						ifTrue: 
							[clone resume.
							Processor terminateActive]]
				ifFalse: 
					["Expire method contexts we're unwinding past. I think this is correct..."
					(frame environment class = Context and: [frame environment outer isInteger])
						ifTrue: [frame environment outer: 0].
					clone suspendedFrame: (frame := frame sender)]].
	self assert: [clone suspendedFrame = baseFrame].! !
!ProcessPatcher categoriesFor: #findMethodContextForBlock:beforeFrame:!obsolete!public! !
!ProcessPatcher categoriesFor: #fixOuterForBlock:beforeFrame:!obsolete!private! !
!ProcessPatcher categoriesFor: #initializeIndexMap!public! !
!ProcessPatcher categoriesFor: #pasteBasicCopyOfStack!public! !
!ProcessPatcher categoriesFor: #patch:onto:!public! !
!ProcessPatcher categoriesFor: #patchContexts!obsolete!private! !
!ProcessPatcher categoriesFor: #patchExceptionHandlers!public! !
!ProcessPatcher categoriesFor: #patchFrames!copying!public! !
!ProcessPatcher categoriesFor: #unwindExistingStack!public! !

!ProcessPatcher class methodsFor!

patch: aPartialProcess onto: aStackFrame 
	^self new patch: aPartialProcess onto: aStackFrame! !
!ProcessPatcher class categoriesFor: #patch:onto:!public! !

