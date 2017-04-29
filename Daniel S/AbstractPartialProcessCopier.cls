"Filed out from Dolphin Smalltalk 7"!

ProcessCopier subclass: #AbstractPartialProcessCopier
	instanceVariableNames: 'baseFrame seamIndex indexMap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractPartialProcessCopier guid: (GUID fromString: '{d0363cda-a901-43a0-bb19-d5d6308b3385}')!
AbstractPartialProcessCopier comment: ''!
!AbstractPartialProcessCopier categoriesForClass!Unclassified! !
!AbstractPartialProcessCopier methodsFor!

copyObject: anObject
	^copied at: anObject
		ifAbsent: 
			[((self indexOf: anObject) between: 1 and: seamIndex)
				ifTrue: [copied at: anObject put: anObject]
				ifFalse: 
					[| copy |
					copy := anObject copyForProcessCopier.
					copied at: anObject put: copy.
					copy deepenProcessCopy: self.
					copy]].!

indexOf: anObject 
	^indexMap at: anObject ifAbsent: [0]
	"1 to: source size do: [:i | (source at: i) == anObject ifTrue: [^i]].
	^0"!

initializeIndexMap
	self subclassResponsibility! !
!AbstractPartialProcessCopier categoriesFor: #copyObject:!copying!public! !
!AbstractPartialProcessCopier categoriesFor: #indexOf:!public! !
!AbstractPartialProcessCopier categoriesFor: #initializeIndexMap!public! !

