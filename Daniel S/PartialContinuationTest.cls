"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #PartialContinuationTest
	instanceVariableNames: 'crossKK counter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PartialContinuationTest guid: (GUID fromString: '{3e537595-6fc5-4387-9c87-d310adde82f3}')!
PartialContinuationTest comment: ''!
!PartialContinuationTest categoriesForClass!Unclassified! !
!PartialContinuationTest methodsFor!

crossProcessExpiredInner
	self 
		assert: (self mark: [2 * (self callcc: 
									[:cc | 
									crossKK := cc.
									2])]) 
				= 4.!

ensureWithReturnInner: kk 
	^self mark: [[kk value: 3] ensure: [^0]].!

ensureWithReturnNotVeryFarInner: kk 
	[kk value: 3] ensure: [^0].!

markDiscardingResultOf: aBlock
	| home |

	home := Processor activeProcess topFrame.

	aBlock on: PartialContinuation markerRequest do: [ :request | request resume: home ]!

markWithCounter: aBlock
	| answer |
	answer := aBlock on: PartialContinuation markerRequest
				do: 
					[:ex |
					ex resume: Processor activeProcess topFrame homeFrame.
					^self].
	counter ifNil: [counter := 1] ifNotNil: [counter := counter + 1].
	^answer.!

testCrossProcess
	| k1 k2 sem |
	sem := Semaphore new.
	
	[self 
		assert: (self mark: 
					[2 
						* (self callcc: 
									[:c1 | 
									k1 := c1.
									3 * (self callcc: 
													[:c2 | 
													k2 := c2.
													4])])]) 
				= 24.
	sem signal] 
			fork.
	sem wait.
	
	[self assert: (self mark: [k1 value: 5]) = 10.
	sem signal] fork.
	sem wait.
	
	[self assert: (self mark: [k2 value: 5]) = 30.
	sem signal] fork.
	sem wait.!

testCrossProcessExpired
	| sem |
	sem := Semaphore new.
	
	[self crossProcessExpiredInner.
	sem signal] fork.
	sem wait.
	self assert: (self mark: [crossKK value: 3]) = 6.
	self assert: (self mark: [crossKK value: 4]) = 8.
	self assert: (self mark: [crossKK value: 5]) = 10.!

testEnsureInsideContinuation
	" ensure blocks within the continuation should be unwound as well "

	| kk count |
	count := 0.
	Transcript
		display: Processor activeProcess topFrame index;
		tab;
		display: Processor activeProcess topFrame sp;
		cr.
	self 
		mark: [[2 * (self callcc: 
							[:cc | 
							kk := cc.
							0])] ensure: [count := count + 1]].
	self assert: count = 1.
	self assert: (kk value: 2) = 4.
	self assert: count = 2.
	self assert: (kk value: 3) = 6.
	self assert: count = 3!

testEnsureOutsideContinuation
	" before evaluation, ensure blocks should be unwound "

	| kk count |
	count := 0.
	self mark: [2 * (self callcc: 
							[:cc | 
							kk := cc.
							0])].
	self assert: (self mark: [[kk value: 3] ensure: [count := count + 1]]) = 6.
	self assert: count = 1!

testEnsureWithReturn
	" ensure blocks should be unwound--the question is, how far? Should an ensure block that attempts to return past the mark context be allowed to do so? In Pharo the answer is 'yes' so going with that "

	| kk |
	self mark: [2 * (self callcc: 
							[:cc |
							kk := cc.
							0])].
	self assert: (self ensureWithReturnInner: kk) = 0.
	self
		assert: ((Process primAllInstances select: [:each | each name = (PartialContinuation >> #value:)])
				allSatisfy: [:each | each state = #dead]).
	MemoryManager current compact.
	self
		assert: (Process primAllInstances select: [:each | each name = (PartialContinuation >> #value:)])
				isEmpty.!

testEnsureWithReturnNotVeryFar
	"If an ensure block does a non-local return that doesn't overshoot, we should still run the PartialContinuation"

	| kk |
	self mark: [2 * (self callcc: 
							[:cc | 
							kk := cc.
							0])].
	"Here the continuation will patch itself on top of the ensure block, so the ensure will ultimately govern what is returned."
	self assert: (self ensureWithReturnNotVeryFarInner: kk) = 0.
	"Here the ensure will be executed for any side effects it might have,
	but the non-local return it contains won't really affect control flow since it doesn't return past the mark:"
	self assert: (self mark: [self ensureWithReturnNotVeryFarInner: kk]) = 6.!

testExceptionHandling
	| kk |
	self 
		assert: (
			[self mark: 
					[| value |
					value := [self callcc: 
									[:cc | 
									kk := cc.
									Warning signal + 4]] 
								on: Warning
								do: [:ex | ex resume: 9].
					Warning signal + value]] 
					on: Warning
					do: [:ex | ex resume: 7]) = 20.
	self assert: ([self mark: [kk value: 1]] on: Warning do: [:ex | ex resume: 2]) = 3!

testIfCurtailedThenSignal
	"ProcessPatcher>>unwindExistingStack is very sensitive to this method being defined Just So..."

	self assert: (BlockClosure >> #ifCurtailed:thenSignal:) byteCodes 
				= (Compiler 
						compile: 'ifCurtailed: handlerBlock thenSignal: aSemaphore
^self valueOnUnwind: [:retValue :retFrame | handlerBlock ensure: [aSemaphore signal]]'
						in: BlockClosure) byteCodes.!

testMarkerOnValue
	| kk captured seen |
	captured := false.
	seen := false.
	self shouldnt: 
			[self mark: 
					[| value |
					value := self callcc: 
									[:cc | 
									kk := cc.
									captured := true].
					seen := true.
					value]]
		raise: Error.
	self assert: captured.
	self assert: seen.
	captured := false.
	seen := false.
	"Make sure the marker method is not included in the continuation and the continuation returns directly
	into the marker method."
	self assert: (self mark: 
					[kk value: 123.
					456]) = 123.
	self deny: captured.
	self assert: seen!

testMarkerOnValueDiscardingResult

	"Previously-failing test from WAPartialContinuationTest>>testMarkerOnValue"

	| kk captured seen |
	captured := false.
	seen := false.
	self	shouldnt: [
			self markDiscardingResultOf: [ | value |
				value := self callcc: [ :cc | kk := cc. captured := true ].
				seen := true.
				value ] ]
		raise: Error.
		
	self assert: captured.
	self assert: seen.
	captured := false.
	seen := false.
	"Make sure the marker method is not included in the continuation and the continuation returns directly
	into the marker method."
	self assert: (self mark: [ kk value: 123. 456 ]) = 123.
	self deny: captured.
	self assert: seen!

testMarkFrameIsNotDuplicated
	| kk |
	self markWithCounter: [2 * (self callcc: 
							[:cc | 
							kk := cc.
							0])].
	self assert: counter = 1.
	self assert: (self markWithCounter: [kk value: 3]) = 6.
	self assert: counter = 2.!

testNestedEvaluation
	| k1 k2 |
	self 
		assert: (self mark: 
					[2 
						* (self callcc: 
									[:c1 | 
									k1 := c1.
									3 * (self callcc: 
													[:c2 | 
													k2 := c2.
													4])])]) 
				= 24.
	self assert: (self mark: [k1 value: 5]) = 10.
	self assert: (self mark: [k2 value: 5]) = 30.!

testNoMarkerOnCall
	| captured seen |
	captured := false.
	seen := false.
	self should: 
			[self callcc: [:cc | captured := true].
			seen := true]
		raise: Error.
	self deny: captured.
	self deny: seen.!

testNoMarkerOnValue
	| kk captured seen |
	captured := false.
	seen := false.
	self mark: 
			[| value |
			value := self callcc: 
							[:cc | 
							kk := cc.
							captured := true].
			seen := true.
			value].
	self assert: captured.
	self assert: seen.
	captured := false.
	seen := false.
	"Make sure the marker method was not included in the continuation"
	self assert: (kk value: 123) = 123.
	self deny: captured.
	self assert: seen!

testNoMarkerOnValueDiscardingResult

	"Copied from WAPartialContinuationTest>>testNoMarkerOnValue"

	| kk captured seen |
	captured := false.
	seen := false.
	self	shouldnt: [
			self markDiscardingResultOf: [
				| value |
				value := self callcc: [ :cc | kk := cc. captured := true ].
				seen := true.
				value ] ]
		raise: Error.
		
	self assert: captured.
	self assert: seen.
	captured := false.
	seen := false.
	"Make sure the marker method was not included in the continuation"
	self assert: (kk value: 123) = 123.
	self deny: captured.
	self assert: seen!

testReentrant
	| kk |
	self 
		assert: (self mark: [2 * (self callcc: 
									[:cc | 
									kk := cc.
									2])]) = 4.
	self assert: (self mark: [kk value: 3]) = 6.
	self assert: (self mark: [kk value: 4]) = 8.
	self assert: (self mark: [kk value: 5]) = 10!

testSimple
	self assert: (self mark: [self callcc: [:cc | cc value: true]])! !
!PartialContinuationTest categoriesFor: #crossProcessExpiredInner!public! !
!PartialContinuationTest categoriesFor: #ensureWithReturnInner:!public! !
!PartialContinuationTest categoriesFor: #ensureWithReturnNotVeryFarInner:!public! !
!PartialContinuationTest categoriesFor: #markDiscardingResultOf:!private! !
!PartialContinuationTest categoriesFor: #markWithCounter:!public! !
!PartialContinuationTest categoriesFor: #testCrossProcess!public! !
!PartialContinuationTest categoriesFor: #testCrossProcessExpired!public! !
!PartialContinuationTest categoriesFor: #testEnsureInsideContinuation!public! !
!PartialContinuationTest categoriesFor: #testEnsureOutsideContinuation!public! !
!PartialContinuationTest categoriesFor: #testEnsureWithReturn!public! !
!PartialContinuationTest categoriesFor: #testEnsureWithReturnNotVeryFar!public! !
!PartialContinuationTest categoriesFor: #testExceptionHandling!public! !
!PartialContinuationTest categoriesFor: #testIfCurtailedThenSignal!public! !
!PartialContinuationTest categoriesFor: #testMarkerOnValue!public! !
!PartialContinuationTest categoriesFor: #testMarkerOnValueDiscardingResult!public! !
!PartialContinuationTest categoriesFor: #testMarkFrameIsNotDuplicated!public! !
!PartialContinuationTest categoriesFor: #testNestedEvaluation!public! !
!PartialContinuationTest categoriesFor: #testNoMarkerOnCall!public! !
!PartialContinuationTest categoriesFor: #testNoMarkerOnValue!public! !
!PartialContinuationTest categoriesFor: #testNoMarkerOnValueDiscardingResult!public! !
!PartialContinuationTest categoriesFor: #testReentrant!public! !
!PartialContinuationTest categoriesFor: #testSimple!public! !

