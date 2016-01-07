"Filed out from Dolphin Smalltalk X6"!

PhoneticStringComparator subclass: #SoundexStringComparator
	instanceVariableNames: ''
	classVariableNames: 'CharacterTranslationDict'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SoundexStringComparator guid: (GUID fromString: '{4B227D9F-995E-45A6-BE8B-78DE5D97ACFF}')!
SoundexStringComparator comment: '(The following was copied from http://www.civilsolutions.com.au/publications/dedup.htm)

SOUNDEX is a phonetic coding algorithm that ignores many of the unreliable
components of names, but by doing so reports more matches. The rules for
coding a name are (from Newcombe):

1.  The first letter of the name is used in its un-coded form to serve as the prefix
    character of the code. (The rest of the code is numerical). 

2.  Thereafter, W and H are ignored entirely. 

3.  A, E, I, 0, U, Y are not assigned a code number, but do serve as "separators"
    (see Step 5). 

4.  Other letters of the name are converted to a numerical equivalent:
		B, P, F, V			1 
		C, G, J, K, Q, S, X, Z		2 
		D, T			3 
		L			4 
		M, N			5 
		R			6 

5.  There are two exceptions: 
	1.  Letters that follow prefix letters which would, if coded, have the same
	    numerical code, are ignored in all cases unless a ''separator'' (see Step
	    3) precedes them. 
	2.  The second letter of any pair of consonants having the same code number
	    is likewise ignored, i.e. unless there is a ''separator'' between them in
	    the name.

6.  The final SOUNDEX code consists of the prefix letter plus three numerical
    characters.  Longer codes are truncated to this length, and shorter codes are
    extended to it by adding zeros.
'!
!SoundexStringComparator categoriesForClass!Kernel-Objects! !
!SoundexStringComparator methodsFor!

phoneticStringsFor: aString
	| u p t prevCode |

	u := aString asUppercase.
	p := u first asString.
	prevCode := self translate: u first.

	(u rightString: u size - 1) do:
		[ :c |
		t := self translate: c.
		t ~~ false & (t ~= '0' & (t ~= prevCode)) ifTrue: [ p := p, t ].
		prevCode := t ].

	[ p size < 4 ] whileTrue: [ p := p, '0' ].

	^Array with: (p leftString: 4)!

translate: aCharacter
	^(self class translationDictionary at: aCharacter ifAbsent: [ false ])! !
!SoundexStringComparator categoriesFor: #phoneticStringsFor:!public! !
!SoundexStringComparator categoriesFor: #translate:!private! !

!SoundexStringComparator class methodsFor!

clearTranslationDictionary
	CharacterTranslationDict := nil!

initializeTranslationDictionary 
	CharacterTranslationDict := Dictionary new
		add: $A -> '0';	"vowels serve as separators"
		add: $E -> '0';
		add: $I -> '0';
		add: $O -> '0';
		add: $U -> '0';
		add: $Y -> '0';

		add: $B -> '1';
		add: $P -> '1';
		add: $F -> '1';
		add: $V -> '1';

		add: $C -> '2';
		add: $S -> '2';
		add: $K -> '2';
		add: $G -> '2';
		add: $J -> '2';
		add: $Q -> '2';
		add: $X -> '2';
		add: $Z -> '2';

		add: $D -> '3';
		add: $T -> '3';

		add: $L -> '4';

		add: $M -> '5';
		add: $N -> '5';

		add: $R -> '6';
		yourself!

translationDictionary
	CharacterTranslationDict isNil ifTrue: [ self initializeTranslationDictionary ].
	^CharacterTranslationDict! !
!SoundexStringComparator class categoriesFor: #clearTranslationDictionary!public! !
!SoundexStringComparator class categoriesFor: #initializeTranslationDictionary!public! !
!SoundexStringComparator class categoriesFor: #translationDictionary!public! !

