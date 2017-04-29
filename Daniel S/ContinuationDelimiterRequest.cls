"Filed out from Dolphin Smalltalk 7"!

Notification subclass: #ContinuationDelimiterRequest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContinuationDelimiterRequest guid: (GUID fromString: '{6d45b258-c8f5-4b5d-b205-590154a8ea2d}')!
ContinuationDelimiterRequest comment: ''!
!ContinuationDelimiterRequest categoriesForClass!Unclassified! !
!ContinuationDelimiterRequest methodsFor!

defaultAction
	^nil.!

tryBlockFrame
	| proc markAndTryFrame |
	proc := Processor activeProcess.
	markAndTryFrame := proc 
				frameAtAddress: (_activeHandler instVarNamed: 'returnContinuation') home outer.
	2 to: 10
		do: 
			[:i | 
			(((proc at: markAndTryFrame sp + i) isKindOf: CompiledCode) 
				and: [(Array with: Context with: BlockClosure) includes: (proc at: markAndTryFrame sp + i + 1) class]) 
					ifTrue: [^proc frameAtIndex: markAndTryFrame sp + i]]! !
!ContinuationDelimiterRequest categoriesFor: #defaultAction!public! !
!ContinuationDelimiterRequest categoriesFor: #tryBlockFrame!public! !

