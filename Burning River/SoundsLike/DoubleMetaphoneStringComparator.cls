"Filed out from Dolphin Smalltalk X6"!

PhoneticStringComparator subclass: #DoubleMetaphoneStringComparator
	instanceVariableNames: 'inputKey primaryTranslation secondaryTranslation startIndex currentIndex skipCount'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DoubleMetaphoneStringComparator guid: (GUID fromString: '{13C67115-0DC3-479E-8B8C-2854B243EC69}')!
DoubleMetaphoneStringComparator comment: 'Description of the Double Metaphone algorithm:

1.  Convert the key to UPPERCASE.
2.  Skip the first character in the key if it starts with ''GN'', ''KN'', ''PN'', ''WR'', or ''PS''.
3.  If the initial character of the key is ''X'' add ''S'' to the primary and alternate translations and skip past the leading ''X''.
4.  If the initial letter of the input key is a vowel add ''A'' to the primary and alternate translations and skip the leading vowel.

For each remaining character in the key:
    A.  If the length of both the primary and alternate translations is >= 4, exit this loop.
    B.  Skip all non-initial vowels in the key.
    C.  Convert ''B'' or ''BB'' to ''P''.
    D.  Convert ''Ç'' to ''S''.
    E.  The following rules apply when ''C'' is found:
        i.  if the current letter is at least the third letter in the key and
	the letter located two before the current letter is not a vowel and
	the current letter is the middle letter in the substring ''ACH'' and
	(the letter located two after the current letter is not ''I'' and
	(the letter located two after the current letter is not ''E'' or
	the current letter is not the third letter in either ''BACHER'' or ''MACHER''))
	    Add ''K'' to the primary and alternate translations.
	    Skip the current and next characters.
        ii. if the first six characters of the key are ''CAESAR''
	Add ''S'' to the primary and alternate translations.
	Skip the current and next characters.
        iii. if the current letter is the first letter in ''CHIA''
	Add ''K'' to the primary and alternate translations.
	Skip the current and next characters.
        iv. if the current letter is the first letter in ''CH''
	a. if this is not the first letter of the string and the current letter is the first letter in ''CHAE''
	    Add ''K'' to the primary translation
	    Add ''X'' to the secondary translation
	    Skip the current and next characters.
	b. if this is the first letter of the string, and the next five characters are ''HARAC'' or ''HARIS'' or the next
		three characters are ''HOR'', ''HYM'', ''HIA'', or ''HEM'', and the first five characters of the
		string are not ''CHORE''
	    Add ''K'' to the primary and secondary translations.
	    Skip the current and next characters.
	c. if (the string begins with ''VAN '', ''VON '', or ''SCH'', or the ''C'' is the third letter in ''ORCHES'', ''ARCHIT'', or ''ORCHID'', or
		the letter following the ''CH'' is ''T'' or ''S'', or ((the letter preceding the ''CH'' is ''A'', ''O'', ''U'', or ''E'', or the C is the first
		character in the string) AND (the letter following the ''CH'' is ''L'', ''R'', ''N'', ''M'', ''B'', ''H'', ''F'', ''V'', ''W'', or ''<blank>''))
	        Add ''K'' to the primary and secondary translations
	    else
	        if the ''C'' is not the first letter in the string then
	            if the string begins with ''MC''
		Add ''K'' to the primary and secondary translations.
	            else
		Add "X'' to the primary translation.
		Add ''K'' to the secondary translation.
	        else
	            Add ''X'' to the primary and secondary translation.
	    Skip the current and next characters.
        v. if the current letter is the first letter in ''CZ''
	Add ''S'' to the primary translation.
	Add ''X'' to the secondary translation.
	Skip the current and next characters.
        vi. if the three characters following the current character are ''CIA''
	Add ''X'' to the primary translation.
	Add ''X'' to the secondary translation.
	Skip the current and next two characters.
        vii. if the current letter is the first character in ''CC'' and not (the current letter is the second letter in the key and
		the first character of the key is ''M'')
	a. if the letter following the second ''C'' is ''I'', ''E'', or ''H'' and the two characters following the second ''C'' are not ''HU''
	    1. if (the current letter is the second letter in the key and the first letter in the key is ''A'') or
			(the five characters starting with the character preceding the first ''C'' are ''UCCEE'' or ''UCCES'') then
	            Add ''KS'' to the primary and secondary translations.
	            Skip the current and following two characters.
	        else
	            Add ''X'' the primary and secondary translations.
	            Skip the current and following two characters.
	b. otherwise
	        Add ''K'' to the primary and secondary translations.
	        Skip the current and next characters.
        viii. if the current letter is the first character in ''CK'', ''CG'', or ''CQ''
	Add ''K'' to the primary and secondary translations.
	Skip the current and following characters.
        ix. if the current letter is the first character in ''CI'', ''CE'', ''CY''
	a. if the current letter is the first letter in ''CIO'', ''CIE'', ''CIA''
	    Add ''S'' to the primary translation.
	    Add ''X'' to the secondary translation.
	    Skip the current and following characters.
	b.else
	    Add ''S'' to the primary translation.
	    Add ''S'' to the secondary translation.
	    Skip the current and following characters.
        x. if none of the above
	Add ''K'' to the primary and secondary translation.
        xi. if the next two characters following the current character are '' C'', '' Q'', or '' G''
	Skip the current and next two characters
            else
	if the character following the current character is ''C'', ''K'', or ''Q'' and the two characters following the current
		character are not ''CE'' or ''CI'' then
	    Skip the current and next characters.
	else
	    Skip the current character.

    F.  The following rules apply when ''D'' is found:
        i. if the current letter is the first character in ''DG''
            a. if the letter following the ''G'' is ''I'', ''E'', or ''Y''
	Add ''J'' to the primary translation.
	Add ''J'' to the secondary translation.
	Skip the current and next two characters.
            b. otherwise
	Add ''TK'' to the primary translation.
	Add ''TK'' to the secondary translation.
	Skip the current and next character.
        ii. if the current letter is the first character in ''DT'' or ''DD''
            Add ''T'' to the primary translation.
            Add ''T'' to the secondary translation.
            Skip the current and next character.
        iii. otherwise
            Add ''T'' to the primary translation.
            Add ''T'' to the secondary translation.
            Skip the current character.

    G. The following rules apply when ''F'' is found
        Add ''F'' to the primary translation.
        Add ''F'' to the secondary translation.
        i. if the current character is the first character in ''FF''
            Skip the current and next characters.
        ii. otherwise
            Skip the current character.

    H. The following rules apply when ''G'' is found
        i. if the next character is ''H''
            a. if the current letter is not the first letter in the key and the preceding letter is not a vowel
	Add ''K'' to the primary translation.
	Add ''K'' to the secondary translation.
	Skip the current and next characters.
            b. if the current letter is the first, second, or third letter in the key
	1. if the current letter is the first letter in the key
	    i. if the letter following the ''H'' is an ''I''
	        Add ''J'' to the primary translation.
	        Add ''J'' to the secondary translation.
	        Skip the current and next characters.
	    ii. otherwise
	        Add ''K'' to the primary translation.
	        Add ''K'' to the secondary translation.
	        Skip the current and next characters.
            c. if (the current letter is the third or later character in the key, and the character two before the current is ''B'', ''H'', or ''D'')
		or (the current letter is the fourth or later character in the key,
				and the character three before the current is ''B'', ''H'', or ''D'')
		or (the current character is the fifth or later character in the key,
				and the character four before the current is ''B'' or ''H'') then
                Skip the current and next characters.	(Silent ''GH'')
            d. if the current letter is the fourth or later character in the key,
		and the character before the ''G'' is ''U'',
		and the character three before the ''G'' is ''C'', ''G'', ''L'', ''R'', or ''T'' then
                Add ''F'' to the primary translation
	Add ''F'' to the secondary translation
	Skip the current and next characters
            e. if the current letter is not the first character in the key
		and the previous character is not ''I''
                Add ''K'' to the primary translation
                Add ''K'' to the secondary translation
                Skip the current and next characters
        ii. if the next character is ''N''
            a. if the current character is the second character in the key 
		and the first character is the key is a vowel
		and the key is not Slavo-Germanic
                Add ''KN'' to the primary translation
	Add ''N'' to the secondary translation
	Skip the current and next characters
            b. if the two characters starting two characters after the ''G'' are not ''EY''
		and the character following the ''G'' is not ''Y''
		and the key is not Slavo-Germanic
                Add ''N'' to the primary translation
                Add ''KN'' to the secondary translation
                Skip the current and next characters
            c. Otherwise
                Add ''KN'' to the primary translation
                Add ''KN'' to the secondary translation
                Skip the current and next characters
        iii. if the two characters following the ''G'' are ''LI
		and the key is not Slavo-Germanic
            Add ''KL'' to the primary translation
            Add ''L'' to the secondary translation
            Skip the current and next characters
        iv. if this is the first character is the key
		and (the next character in the key is ''Y''
			or the next two characters in the key are one of (''ES'', ''EP'', ''EB'', ''EL'', ''EY'', ''IB'', ''IL''
							        ''IN'', ''IE'', ''EI'', or ''ER''))
            Add ''K'' to the primary translation
            Add ''J'' to the secondary translation
            Skip the current and next characters
        v. if (the two characters following the ''G'' are ''ER''
			or the character following the key is ''Y'')
		and the first six characters of the string are not ''DANGER'', ''RANGER'', or ''MANGER''
		and the character preceding the ''G'' is not ''E'' or ''I''
		and the ''G'' is not the middle character of ''RGY'' or ''OGY'' then
            Add ''K'' to the primary translation
            Add ''J'' to the secondary translation
            Skip the current and next characters
        vi. if the character following the ''G'' is ''E'', ''I'', or ''Y''
		or the ''G'' is not the second character in ''AGGI'' or ''OGGI'' then
            a. if the first four characters of the key are ''VAN '' or ''VON ''
		or the two characters following the ''G'' are ''ET'' then
                Add ''K'' to the primary translation
                Add ''K'' to the secondary translation
                Skip the current and next characters
            b. if the three characters following the ''G'' are ''IER'' then
                Add ''J'' to the primary translation
                Add ''J'' to the secondary translation
                Skip the current and next characters
            c. otherwise
                Add ''J'' to the primary translation
                Add ''K'' to the secondary translation
                Skip the current and next characters
        vii. Otherwise
                Add ''K'' to the primary translation
                Add ''K'' to the secondary translation
                if the character following the ''G'' is another ''G''
                    Skip the current and next characters
                otherwise
                    Skip the current character

    I. The following rules apply when ''H'' is found
        i. if (this is the first character in the key
		or the previous character is a vowel)
		and the next character is a vowel
                Add ''H'' to the primary translation
	Add ''H'' to the secondary translation
	Skip the current and next characters
            otherwise
                Skip the current character

    J. The following rules apply when ''J'' is found
        i. if the next four characters in the key are ''JOSE''
		or the key starts with ''SAN ''
	a. if (this is the first character in the key
			and the character four after the current character is '' '')
			or the key starts with ''SAN ''
		Add ''H'' to the primary translation
		Add ''H'' to the secondary translation
		Skip the current character
	b. otherwise
		Add ''J'' to the primary translation
		Add ''H'' to the secondary translation
		Skip the current character
        ii. if this is the first character of the key
		and the first four characters of the key are NOT ''JOSE''
	Add ''J'' to the primary translation
	Add ''A'' to the secondary translation
        iii. otherwise
	a. if the character before the ''J'' is a vowel
			and the key is NOT SlavoGermanic
			and the character after the ''J'' is either ''A'' or ''O''
		Add ''J'' to the primary translation
		Add ''H'' to the secondary translation
	b. otherwise if ''J'' is the last character in the key
		Add ''J'' to the primary translation
		Add '' '' to the secondary translation
	c. otherwise if the character following the ''J'' is NOT in (''L'', ''T'', ''K'', ''S'', ''N'', ''M'', ''B'', ''Z'')
			and the character before the ''J'' is NOT in (''S'', ''K'', ''L'')
		Add ''J'' to the primary translation
		Add ''J'' to the secondary translation
        iv. if the character following the ''J'' is another ''J''
	Skip the current and next characters
        v. otherwise
	Skip the current character.

    K. The following rules apply when ''K'' is found
        Add ''K'' to the primary translation
        Add ''K'' to the secondary translation
        if the next character is also a ''K''
	Skip the current and next characters
        otherwise
	Skip the current character

    L. The following rules apply when ''L'' is found
        i.  If the current character is the first ''L'' in ''LL''
	a. If (the current character is two before the last character in the key
			and the current character is the first ''L'' in one of (''ILLO'', ''ILLA'', ''ILLE"))
			or ( ((the key end with ''AS'' or ''OS'')
					or (the key ends with ''A'' or ''O''))
				and the current character is the first ''L'' in ''ALLE'')
		Add ''L'' to the primary translation
		Add '' '' to the secondary translation
		Skip the current and next characters
	    otherwise
		Add ''L'' to the primary translation
		Add ''L'' to the secondary translation
		Skip the current and next characters
        ii. otherwise
	Add ''L'' to the primary translation
	Add ''L'' to the secondard translation
	Skip the current character

    M. The following rules apply when ''M'' is found
        Add ''M'' to the primary translation
        Add ''M'' to the secondary translation
        i. if (the current character is the middle character in ''UMB''
		and (the current character is the one before the last character in the key
			or the two characters two after the current one are ''ER''))
		or the character following the current one is ''M''
	Skip the current and next character
        ii. otherwise
	Skip the current character

    N.  The following rules apply when ''N'' is found
        Add ''N'' to the primary translation
        Add ''N'' to the secondary translation
        If the character following the current character is ''N''
	Skip the current and next characters
        otherwise
	Skip the current character

    O. The following rules apply when ''Ñ'' is found
        Add ''N'' to the primary translation
        Add ''N'' to the secondary translation
        Skip the current character

    P. The following rules apply when ''P'' is found
        i. If the next character is ''H''
	Add ''F'' to the primary translation
	Add ''F'' to the secondary translation
	Skip the current and next characters
        ii. if the next character is ''P'' or ''B''
	Add ''P'' to the primary translation
	Add ''P'' to the secondary translation
	Skip the current and next characters
        iii. otherwise
	Add ''P'' to the primary translation
	Add ''P'' to the secondary translation
	Skip the current character

    Q. The following rules apply when ''Q'' is found
        Add ''K'' to the current translation
        Add ''K'' to the secondary translation
        if the next character is also ''Q''
	Skip the current and next character
        otherwise
	Skip the current character

    R. The following rules apply when ''R'' is found
        i. if the current character is the last one in the key
		and the key is not SlavoGermanic
		and the two characters preceding the ''R'' are ''IE''
		and the two characters preceding the ''IE'' are NOT ''ME'' or ''MA''
	Add ''R'' to the secondary translation (only)
        ii. otherwise
	Add ''R'' to the primary translation
	Add ''R'' to the secondary translation
        iii. If the next character is the key is also ''R''
	Skip the current and next characters
        iv. otherwise
	Skip the current character

    S. The following rules apply when ''S'' is found
'!
!DoubleMetaphoneStringComparator categoriesForClass!Kernel-Objects! !
!DoubleMetaphoneStringComparator methodsFor!

addPrimaryTranslation: aString
	self primaryTranslation: self primaryTranslation, aString!

addSecondaryTranslation: aString
	self secondaryTranslation: self secondaryTranslation, aString!

currentIndex
	^currentIndex!

currentIndex: anInteger
	currentIndex := anInteger!

decrementSkipCount
	self skipCount: self skipCount - 1!

incrementSkipCount
	self incrementSkipCount: 1!

incrementSkipCount: anInteger
	self skipCount: self skipCount + anInteger!

incrementStartIndex
	self startIndex: self startIndex + 1!

initialize
	super initialize.

	self
		startIndex: 1;
		primaryTranslation: '';
		secondaryTranslation: '';
		skipCount: 0;
		currentIndex: 1!

inputKey
	^inputKey!

inputKey: aString
	inputKey := aString asUppercase!

isSlavoGermanic: aString
	^((aString includesAnyOf: 'WK') or:
		[ (aString indexOfSubstring: 'CZ' startingAt: 1) >= 1 ]) or:
		[ (aString indexOfSubstring: 'WITZ' startingAt: 1) >= 1 ]
!

keyAt: anInteger
	(anInteger >=1 and: [ anInteger <= self inputKey size ])
		ifTrue: [ ^self inputKey at: anInteger ]
		ifFalse: [ ^$  ]!

keyLeftString: lengthInteger
	^self keyMidString: lengthInteger from: 1!

keyMidString: lengthInteger from: fromInteger
	| result from len additionalSpaces |

	result := ''.
	from := fromInteger.
	len := lengthInteger.

	"Prepend spaces if caller is requesting characters from before the start of the string"

	[ from < 1 ] whileTrue:
		[ result := result, ' '.
		from := from + 1.
		len := len - 1 ].

	from + len - 1 > self inputKey size
		ifTrue:
			[ additionalSpaces := from + len - 1 - self inputKey size.
			len := self inputKey size - from + 1 ]
		ifFalse: [ additionalSpaces := 0 ].

	result := result, (self inputKey midString: len from: from).

	[ additionalSpaces > 0 ] whileTrue:
		[ result := result, ' '.
		additionalSpaces := additionalSpaces - 1 ].

	^result!

keyRightString: lengthInteger
	^self keyMidString: lengthInteger from: self inputKey size - lengthInteger + 1!

performInitialProcessing
	(#('GN' 'KN' 'PN' 'WR' 'PS') includes: (self inputKey leftString: 2))
		ifTrue: [ self incrementStartIndex ].

	(self keyAt: 1) = $X
		ifTrue:
			[ self
				addPrimaryTranslation: 'S';
				addSecondaryTranslation: 'S'.
			self incrementStartIndex ].

	(self keyAt: 1) isVowel
		ifTrue:
			[ self
				addPrimaryTranslation: 'A';
				addSecondaryTranslation: 'A'.
			self incrementStartIndex ]!

phoneticStringsFor: aString
	"Private - Answers an array of alternate phonetic strings for the given input string."

	self inputKey: aString.
	self performInitialProcessing.

	self processRemainingCharacters.

	^Array with: self primaryTranslation with: self alternateTranslation!

primaryTranslation
	^primaryTranslation!

primaryTranslation: anObject
	primaryTranslation := anObject!

processB
	self
		addPrimaryTranslation: 'P';
		addSecondaryTranslation: 'P'.
	(self keyAt: (self currentIndex + 1)) = $B
		ifTrue: [ self incrementSkipCount ].!

processC
	"i"
	((((currentIndex >= 3
		and: [ (self keyAt: self currentIndex-2) isVowel not ])
		and: [ (self keyMidString: 3 from: self currentIndex-1) = 'ACH' ])
		and: [ (self keyAt: self currentIndex+2) ~= $I ])
		and: [ ((self keyAt: self currentIndex+2) ~= $E)
				or: [ (self keyMidString: 6 from: self currentIndex-2) ~= 'BACHER'
						and: [ (self keyMidString: 6 from: self currentIndex-2) ~= 'MACHER' ] ] ])
			ifTrue:
				[ self addPrimaryTranslation: 'K'.
				self addSecondaryTranslation: 'K'.
				self incrementSkipCount: 2.
				^self ].

	"ii"
	(self inputKey beginsWith: 'CAESAR')
		ifTrue:
			[ self addPrimaryTranslation: 'S'.
			self addSecondaryTranslation: 'S'.
			self incrementSkipCount: 1.
			^self ].

	"iii"
	(self keyMidString: 4 from: self currentIndex) = 'CHIA'
		ifTrue:
			[ self addPrimaryTranslation: 'K'.
			self addSecondaryTranslation: 'K'.
			self incrementSkipCount: 1.
			^self ].

	"iv"
	(self keyMidString: 2 from: self currentIndex) = 'CH'
		ifTrue:
			[ (self currentIndex > 1		"a"
					and: [ (self keyMidString: 4 from: self currentIndex) = 'CHAE' ])
				ifTrue: [ self
						addPrimaryTranslation: 'K';
						addSecondaryTranslation: 'X';
						incrementSkipCount: 1.
						^self ].

			(self currentIndex = 1		"b"
					and: [ ((self inputKey leftString: 6) = 'CHARAC'
							or: [ (self inputKey leftString: 6) = 'CHARIS' ] )
						or: [ ((((self inputKey leftString: 4) = 'CHOR'
							or: [ (self inputKey leftString: 4) = 'CHYM' ])
							or: [ (self inputKey leftString: 4) = 'CHIA' ])
							or: [ (self inputKey leftString: 4) = 'CHEM' ])
							and: [ (self inputKey leftString: 4) ~= 'CHORE' ] ] ])
				ifTrue: [ self
						addPrimaryTranslation: 'K';
						addSecondaryTranslation: 'K';
						incrementSkipCount: 1.
						^self ].

			(((((#('VAN ' 'VON ') includes: (self inputKey leftString: 4))		"c"
					or: [ (self inputKey leftString: 3) = 'SCH' ])
					or: [ #('ORCHES' 'ARCHIT' 'ORCHID')
							includes: (self keyMidString: 6 from: self currentIndex-2) ])
					or: [ #($T $S) includes: (self keyAt: self currentIndex+2) ])
					or: [ ((self currentIndex = 1)
							or: [ #($A $O $U $E) includes: (self keyAt: self currentIndex-1) ])
						and: [ #($L $R $N $M $B $H $F $V $W $ ) includes: (self keyAt: self currentIndex+2) ] ] )
				ifTrue:
					[ self
						addPrimaryTranslation: 'K';
						addSecondaryTranslation: 'K';
						incrementSkipCount: 1.
						^self ]
				ifFalse:
					[ self currentIndex > 1
						ifTrue:
							[ (self inputKey leftString: 2) = 'MC'
								ifTrue:
										[ self
												addPrimaryTranslation: 'K';
												addSecondaryTranslation: 'K' ]
								ifFalse:
										[ self
												addPrimaryTranslation: 'X';
												addSecondaryTranslation: 'K' ] ]
						ifFalse:
							[ self
								addPrimaryTranslation: 'X';
								addSecondaryTranslation: 'X' ].
					self incrementSkipCount: 1.
					^self ] ].

	"v"
	(self keyAt: self currentIndex+1) = $Z
		ifTrue:
			[ self
				addPrimaryTranslation: 'S';
				addSecondaryTranslation: 'X';
				incrementSkipCount: 1.
				^self ].

	"vi"
	(self keyMidString: 3 from: self currentIndex+1) = 'CIA'
		ifTrue:
			[ self
				addPrimaryTranslation: 'X';
				addSecondaryTranslation: 'X';
				incrementSkipCount: 2.
				^self ].

	"vii"
	((self keyAt: self currentIndex+1) = $C
			and: [ ((currentIndex = 2)
				and: [ (self keyAt: 1) = $M ]) not ])
		ifTrue:
			[ ((#($I $E $H) includes: (self keyAt: self currentIndex+2))
					and: [ (self keyMidString: 2 from: self currentIndex+2) ~= 'HU' ])
				ifTrue:
					[ ((self currentIndex = 2 and: [ (self keyAt: 1) = $A ])
							or: [ #('UCCEE' 'UCCES') includes: (self keyMidString: 5 from: self currentIndex-1)])
						ifTrue:
							[self
								addPrimaryTranslation: 'KS';
								addSecondaryTranslation: 'KS';
								incrementSkipCount: 2.
								^self ]
						ifFalse:
							[self
								addPrimaryTranslation: 'X';
								addSecondaryTranslation: 'X';
								incrementSkipCount: 2.
								^self ] ]
				ifFalse:
					[ self
						addPrimaryTranslation: 'K';
						addSecondaryTranslation: 'K';
						incrementSkipCount: 2.
						^self ] ].

	"viii"
	(#($K $G $Q) includes: (self keyAt: self currentIndex+1))
		ifTrue:
			[ self
				addPrimaryTranslation: 'K';
				addSecondaryTranslation: 'K';
				incrementSkipCount: 1.
				^self ].

	"ix"
	(#($I $E $Y) includes: (self keyAt: self currentIndex+1))
		ifTrue:
			[ (#('CIO' 'CIE' 'CIA') includes: (self keyMidString: 3 from: self currentIndex))
				ifTrue:
					[self
						addPrimaryTranslation: 'S';
						addSecondaryTranslation: 'X' ]
				ifFalse:
					[self
						addPrimaryTranslation: 'S';
						addSecondaryTranslation: 'S'].
			self incrementSkipCount: 1.
			^self ].

	"x"
	self
		addPrimaryTranslation: 'K';
		addSecondaryTranslation: 'K'.

	"xi"
	(#(' C' ' Q' ' G') includes: (self keyMidString: 2 from: self currentIndex+1))
		ifTrue:
			[ self incrementSkipCount: 2 ]
		ifFalse:
			[ ((#($C $K $Q) includes: (self keyAt: self currentIndex+1))
					and: [ (#('CE' 'CI') includes: (self keyMidString: 2 from: self currentIndex+1)) not ])
				ifTrue: [ self incrementSkipCount: 1] ]!

processCedille 
	self
		addPrimaryTranslation: 'S';
		addSecondaryTranslation: 'S'!

processD
	"i"
	(self keyAt: self currentIndex+1) = $G
		ifTrue:
			[ (#($I $E $Y) includes: (self keyAt: self currentIndex+2))
				ifTrue:
					[ self
						addPrimaryTranslation: 'J';
						addSecondaryTranslation: 'J';
						incrementSkipCount: 2.
					^self ]
				ifFalse:
					[ self
						addPrimaryTranslation: 'TK';
						addSecondaryTranslation: 'TK';
						incrementSkipCount: 1.
					^self ] ].

	"ii"
	(#($T $D) includes: (self keyAt: self currentIndex+1))
		ifTrue:
			[ self
				addPrimaryTranslation: 'T';
				addSecondaryTranslation: 'T';
				incrementSkipCount: 1.
			^self ].

	"iii"
	self
		addPrimaryTranslation: 'T';
		addSecondaryTranslation: 'T'
!

processF
	self
		addPrimaryTranslation: 'F';
		addSecondaryTranslation: 'F'.
	(self keyAt: self currentIndex+1) = $F
		ifTrue: [ self incrementSkipCount: 1 ]!

processRemainingCharacters
	self startIndex to: self inputKey size do:
		[ :i | | c methodSelector |
		self skipCount = 0
			ifTrue:
				[ ((self primaryTranslation size > 4) and: [ self secondaryTranslation size > 4 ])
					ifTrue: [ ^self ].

				self currentIndex: i.
				c := self keyAt: i.

				c isVowel not
					ifTrue:
						[ c = $Ç
							ifTrue: [ methodSelector := #processCedille ]
							ifFalse: [ methodSelector := ('process', c asString) asSymbol ].
						self perform: methodSelector ] ]
			ifFalse: [ self decrementSkipCount ] ]!

secondaryTranslation
	^secondaryTranslation!

secondaryTranslation: anObject
	secondaryTranslation := anObject!

skipCount
	^skipCount!

skipCount: anInteger
	skipCount := anInteger!

startIndex
	^startIndex!

startIndex: anObject
	startIndex := anObject! !
!DoubleMetaphoneStringComparator categoriesFor: #addPrimaryTranslation:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #addSecondaryTranslation:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #currentIndex!private! !
!DoubleMetaphoneStringComparator categoriesFor: #currentIndex:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #decrementSkipCount!private! !
!DoubleMetaphoneStringComparator categoriesFor: #incrementSkipCount!private! !
!DoubleMetaphoneStringComparator categoriesFor: #incrementSkipCount:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #incrementStartIndex!private! !
!DoubleMetaphoneStringComparator categoriesFor: #initialize!public! !
!DoubleMetaphoneStringComparator categoriesFor: #inputKey!private! !
!DoubleMetaphoneStringComparator categoriesFor: #inputKey:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #isSlavoGermanic:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #keyAt:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #keyLeftString:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #keyMidString:from:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #keyRightString:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #performInitialProcessing!private! !
!DoubleMetaphoneStringComparator categoriesFor: #phoneticStringsFor:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #primaryTranslation!private! !
!DoubleMetaphoneStringComparator categoriesFor: #primaryTranslation:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #processB!private! !
!DoubleMetaphoneStringComparator categoriesFor: #processC!private! !
!DoubleMetaphoneStringComparator categoriesFor: #processCedille!private! !
!DoubleMetaphoneStringComparator categoriesFor: #processD!private! !
!DoubleMetaphoneStringComparator categoriesFor: #processF!private! !
!DoubleMetaphoneStringComparator categoriesFor: #processRemainingCharacters!private! !
!DoubleMetaphoneStringComparator categoriesFor: #secondaryTranslation!private! !
!DoubleMetaphoneStringComparator categoriesFor: #secondaryTranslation:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #skipCount!private! !
!DoubleMetaphoneStringComparator categoriesFor: #skipCount:!private! !
!DoubleMetaphoneStringComparator categoriesFor: #startIndex!private! !
!DoubleMetaphoneStringComparator categoriesFor: #startIndex:!private! !

!DoubleMetaphoneStringComparator class methodsFor!

new
	^super new initialize! !
!DoubleMetaphoneStringComparator class categoriesFor: #new!public! !

