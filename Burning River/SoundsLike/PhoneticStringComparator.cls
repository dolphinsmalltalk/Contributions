"Filed out from Dolphin Smalltalk X6"!

Object subclass: #PhoneticStringComparator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PhoneticStringComparator guid: (GUID fromString: '{3311E23F-5806-404E-8DC3-E7B4EB75F85B}')!
PhoneticStringComparator comment: ''!
!PhoneticStringComparator categoriesForClass!Kernel-Objects! !
!PhoneticStringComparator methodsFor!

does: aString soundLike: anotherString
	| translations1 translations2 result |

	translations1 := self phoneticStringsFor: aString.
	translations2 := self phoneticStringsFor: anotherString.

	result := false.

	translations1 do: [ :t1 |
		translations2 do: [ :t2 |
			t1 = t2 ifTrue: [ result := true ] ] ].

	^result!

phoneticStringsFor: aString
	"Should answer an array of alternate phonetic strings for the given input string."
	self subclassResponsibility! !
!PhoneticStringComparator categoriesFor: #does:soundLike:!public! !
!PhoneticStringComparator categoriesFor: #phoneticStringsFor:!private! !

!PhoneticStringComparator class methodsFor!

defaultClass
	^SoundexStringComparator! !
!PhoneticStringComparator class categoriesFor: #defaultClass!public! !

