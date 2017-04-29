"Filed out from Dolphin Smalltalk 7"!

Object subclass: #ExceptionHandlerChainCookie
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExceptionHandlerChainCookie guid: (GUID fromString: '{2f0f24b3-1138-4c10-a890-2dc21e57ad4e}')!
ExceptionHandlerChainCookie comment: 'Not really a class, just a placeholder object that needs some behavior. Used by PartialProcessCopier/ProcessPatcher to indicate ''whatever ExceptionHandler is below this point in the stack'''!
!ExceptionHandlerChainCookie categoriesForClass!Kernel-Objects! !
!ExceptionHandlerChainCookie class methodsFor!

acceptProcessCopier: aProcessCopier 
	^aProcessCopier copyObject: self.!

deepenProcessCopy: aProcessCopier 
	self error: 'Should never hit this method'.!

new
	self shouldNotImplement.! !
!ExceptionHandlerChainCookie class categoriesFor: #acceptProcessCopier:!public! !
!ExceptionHandlerChainCookie class categoriesFor: #deepenProcessCopy:!public! !
!ExceptionHandlerChainCookie class categoriesFor: #new!public! !

