"Filed out from Dolphin Smalltalk X6"!

PhoneticStringComparator subclass: #NYSIISStringComparator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NYSIISStringComparator guid: (GUID fromString: '{9664BC9F-6F3A-46F7-B98E-5B093F016018}')!
NYSIISStringComparator comment: 'NYSIIS Algorithm:

1. remove all ''S'' and ''Z'' chars from the end of the surname 
2. transcode initial strings
	MAC => MC 
	PF => F

3. Transcode trailing strings as follows,
	IX => IC 
	EX => EC 
	YE,EE,IE => Y 
	NT,ND => D 

4. transcode ''EV'' to ''EF'' if not at start of name 
5. use first character of name as first character of key 
6. remove any ''W'' that follows a vowel 
7. replace all vowels with ''A'' 
8. transcode ''GHT'' to ''GT'' 
9. transcode ''DG'' to ''G'' 
10. transcode ''PH'' to ''F'' 
11. if not first character, eliminate all ''H'' preceded or followed by a vowel 
12. change ''KN'' to ''N'', else ''K'' to ''C'' 
13. if not first character, change ''M'' to ''N'' 
14. if not first character, change ''Q'' to ''G'' 
15. transcode ''SH'' to ''S'' 
16. transcode ''SCH'' to ''S'' 
17. transcode ''YW'' to ''Y'' 
18. if not first or last character, change ''Y'' to ''A'' 
19. transcode ''WR'' to ''R'' 
20. if not first character, change ''Z'' to ''S'' 
21. transcode terminal ''AY'' to ''Y'' 
22. remove traling vowels 
23. collapse all strings of repeated characters 
24. if first char of original surname was a vowel, append it to the code

'!
!NYSIISStringComparator categoriesForClass!Kernel-Objects! !
!NYSIISStringComparator methodsFor!

phoneticStringsFor: aString
	| k |

	k := self rule1: (aString asUppercase).
	k := self rule2: k.
	k := self rule3: k.
	k := self rule4: k.
	k := self rule5: k.
	k := self rule6: k.
	k := self rule7: k.
	k := self rule8: k.
	k := self rule9: k.
	k := self rule10: k.
	k := self rule11: k.
	k := self rule12: k.
	k := self rule13: k.
	k := self rule14: k.
	k := self rule15: k.
	k := self rule16: k.
	k := self rule17: k.
	k := self rule18: k.
	k := self rule19: k.
	k := self rule20: k.
	k := self rule21: k.
	k := self rule22: k.
	k := self rule23: k.
	k := self rule24: k originalKey: aString.

	^Array with: k!

rule1: key
	| k |

	k := key copy.

	"1. Remove all 'S' and 'Z' chars from the end of the name"

	[ #('S' 'Z') includes: k last ] whileTrue: [ k := k leftString: (k size - 1) ].

	^k!

rule10: key
	"10. transcode 'PH' to 'F' "

	^self transcodeAll: 'PH' of: key to: 'F' startingAt: 1!

rule11: key
	| k c |

	"11. if not first character, eliminate all 'H' preceded or followed by a vowel "

	k := key copy.
	c := SortedCollection sortBlock: [ :a :b | b < a ].

	2 to: key size do:
		[ :i |
		(key at: i) = $H
			ifTrue:
				[ ((key at: i-1) isVowel or: [ (i < key size) and: [ (key at: i + 1) isVowel ] ])
					ifTrue:
						[ c add: i ] ] ].

	c do:
		[ :n |
		k := (k leftString: n - 1), (k rightString: k size - n) ].

	^k!

rule12: key
	| k |

	"12. change 'KN' to 'N', else 'K' to 'C' "

	k := self transcodeAll: 'KN' of: key to: 'K' startingAt: 1.
	k := self transcodeAll: 'K' of: k to: 'C' startingAt: 1.

	^k!

rule13: key
	"13. if not first character, change 'M' to 'N' "

	^self transcodeAll: 'M' of: key to: 'N' startingAt: 2!

rule14: key
	"14. if not first character, change 'Q' to 'G' "

	^self transcodeAll: 'Q' of: key to: 'G' startingAt: 2!

rule15: key
	"15. transcode 'SH' to 'S' "

	^self transcodeAll: 'SH' of: key to: 'S' startingAt: 1!

rule16: key
	"16. transcode 'SCH' to 'S' "

	^self transcodeAll: 'SCH' of: key to: 'S' startingAt: 1!

rule17: key
	"17. transcode 'YW' to 'Y' "

	^self transcodeAll: 'YW' of: key to: 'Y' startingAt: 1!

rule18: key
	| k |

	"18. if not first or last character, change 'Y' to 'A' "

	k := self transcodeAll: 'Y' of: key to: 'A' startingAt: 2.
	key last = $Y ifTrue: [ k at: k size put: $Y ].

	^k
!

rule19: key
	"19. transcode 'WR' to 'R' "

	^self transcodeAll: 'WR' of: key to: 'R' startingAt: 1!

rule2: key
	| k |

	k := key copy.

	"2. Transcode initial strings:  MAC => MC   PF => F"

	(k leftString: 3) = 'MAC' ifTrue: [ k := 'MC', k rightString: (k size - 3) ].
	(k leftString: 2) = 'PF' ifTrue: [ k := 'F', k rightString: (k size - 2) ].

	^k!

rule20: key
	"20. if not first character, change 'Z' to 'S' "

	^self transcodeAll: 'Z' of: key to: 'S' startingAt: 2!

rule21: key
	"21. transcode terminal 'AY' to 'Y' "

	^self transcodeAll: 'AY' of: key to: 'Y' startingAt: key size - 1!

rule22: key
	| k |

	"22. remove trailing vowels "

	k := key copy.

	[ k last isVowel ] whileTrue: [ k := k leftString: k size - 1 ].

	^k
!

rule23: key
	| k c |

	"23. collapse all strings of repeated characters "

	k := key copy.
	c := SortedCollection sortBlock: [ :a :b | b < a ].

	k size to: 2 do:
		[ :i |
		(k at: i) = (k at: i-1)
			ifTrue: [ c add: i ] ].

	c do:
		[ :n |
		k := (k leftString: n-1), (k rightString: (k size - n)) ].

	^k
!

rule24: key originalKey: originalKey
	| k |

	"24. if first char of original surname was a vowel, append it to the code"

	k := key copy.

	originalKey first isVowel ifTrue: [ k := k, originalKey first asString asUppercase ].

	^k
!

rule3: key
	| k |

	"3. Transcode trailing strings as follows:
		IX => IC
		EX => EC
		YE, EE, IE => Y
		NT, ND => D"

	k := key copy.

	k := self transcodeTrailing: #('IX') of: k to: 'IC'.
	k := self transcodeTrailing: #('EX') of: k to: 'EC'.
	k := self transcodeTrailing: #('YE' 'EE' 'IE') of: k to: 'Y'.
	k := self transcodeTrailing: #('NT' 'ND') of: k to: 'D'.

	^k!

rule4: key
	"4. Transcode 'EV' to 'EF' if not at start of name"

	^self transcodeAll: 'EV' of: key to: 'EF' startingAt: 2!

rule5: key
	"5. Use first character of name as first character of key.  Ignored because we're doing an in-place conversion"

	^key
!

rule6: key
	| k i |

	"6. Remove any 'W' that follows a vowel"

	k := key copy.
	i := 2.
	[ (i := k nextIndexOf: 'W' from: i to: k size) > 0 ] whileTrue:
		[ (k at: i-1) isVowel ifTrue:
			[ k := (k leftString: i-1), (k rightString: k size - i).
			i := i - 1 ] ].

	^k
!

rule7: key
	| k |

	"7. replace all vowels with 'A' "

	k := key copy.

	1 to: key size do:
		[ :i |
		(key at: i) isVowel ifTrue: [ k at: i put: $A ] ].

	^k
!

rule8: key
	"8. transcode 'GHT' to 'GT' "

	^self transcodeAll: 'GHT' of: key to: 'GT' startingAt: 1!

rule9: key
	"9. transcode 'DG' to 'G' "

	^self transcodeAll: 'DG' of: key to: 'G' startingAt: 1!

transcodeAll: aString of: key to: replacementString startingAt: start
	| k i |

	k := key copy.

	[ (i := k indexOfSubCollection: aString startingAt: start) > 0 ] whileTrue:
		[ k := (k leftString: i-1), replacementString, (k rightString: (k size - i) - (aString size - 1)) ].

	^k
!

transcodeTrailing: anArrayOfStrings of: key to: replacementString
	| answer |

	answer := key copy.

	anArrayOfStrings do:
		[ :aString |
		answer := self transcodeAll: aString of: answer to: replacementString startingAt: (answer size - aString size) + 1 ].

	^answer
	! !
!NYSIISStringComparator categoriesFor: #phoneticStringsFor:!public! !
!NYSIISStringComparator categoriesFor: #rule1:!private! !
!NYSIISStringComparator categoriesFor: #rule10:!private! !
!NYSIISStringComparator categoriesFor: #rule11:!private! !
!NYSIISStringComparator categoriesFor: #rule12:!private! !
!NYSIISStringComparator categoriesFor: #rule13:!private! !
!NYSIISStringComparator categoriesFor: #rule14:!private! !
!NYSIISStringComparator categoriesFor: #rule15:!private! !
!NYSIISStringComparator categoriesFor: #rule16:!private! !
!NYSIISStringComparator categoriesFor: #rule17:!private! !
!NYSIISStringComparator categoriesFor: #rule18:!private! !
!NYSIISStringComparator categoriesFor: #rule19:!private! !
!NYSIISStringComparator categoriesFor: #rule2:!private! !
!NYSIISStringComparator categoriesFor: #rule20:!private! !
!NYSIISStringComparator categoriesFor: #rule21:!private! !
!NYSIISStringComparator categoriesFor: #rule22:!private! !
!NYSIISStringComparator categoriesFor: #rule23:!private! !
!NYSIISStringComparator categoriesFor: #rule24:originalKey:!private! !
!NYSIISStringComparator categoriesFor: #rule3:!private! !
!NYSIISStringComparator categoriesFor: #rule4:!private! !
!NYSIISStringComparator categoriesFor: #rule5:!private! !
!NYSIISStringComparator categoriesFor: #rule6:!private! !
!NYSIISStringComparator categoriesFor: #rule7:!private! !
!NYSIISStringComparator categoriesFor: #rule8:!private! !
!NYSIISStringComparator categoriesFor: #rule9:!private! !
!NYSIISStringComparator categoriesFor: #transcodeAll:of:to:startingAt:!private! !
!NYSIISStringComparator categoriesFor: #transcodeTrailing:of:to:!private! !

