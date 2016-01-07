"Filed out from Dolphin Smalltalk X6.1"!

TestCase subclass: #DoubleMetaphoneStringComparatorTest
	instanceVariableNames: 'dmsc'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DoubleMetaphoneStringComparatorTest guid: (GUID fromString: '{0E78B676-F462-42A7-9BC6-0E6DF126ADC6}')!
DoubleMetaphoneStringComparatorTest comment: 'Tests for the DoubleMetaphoneStringComparator class.'!
!DoubleMetaphoneStringComparatorTest categoriesForClass!Unclassified! !
!DoubleMetaphoneStringComparatorTest methodsFor!

setUp
	dmsc := DoubleMetaphoneStringComparator new!

testKeyMidStringFrom
	dmsc inputKey: 'A string of characters'.

	self should: [ (dmsc keyMidString: 3 from: 0) = ' A ' ].
	self should: [ (dmsc keyMidString: 5 from: 7) = 'NG OF' ].
	self should: [ (dmsc keyMidString: 4 from: 21) = 'RS  ' ].!

testPerformInitialProcessing1
	dmsc inputKey: 'gneiss'.
	self should: [ dmsc inputKey = 'GNEISS' ].

	self should: [ dmsc startIndex = 1 ].
	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ (dmsc inputKey at: dmsc startIndex) = $G ].

	dmsc performInitialProcessing.

	self should: [ dmsc startIndex = 2 ].
	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ (dmsc inputKey at: dmsc startIndex) = $N ].!

testPerformInitialProcessing2
	dmsc inputKey: 'zneiss'.
	self should: [ dmsc inputKey = 'ZNEISS' ].

	self should: [ dmsc startIndex = 1 ].
	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ (dmsc inputKey at: dmsc startIndex) = $Z ].

	dmsc performInitialProcessing.

	self should: [ dmsc startIndex = 1 ].
	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ (dmsc inputKey at: dmsc startIndex) = $Z ].!

testPerformInitialProcessing3
	dmsc inputKey: 'xAvIeR'.
	self should: [ dmsc inputKey = 'XAVIER' ].

	self should: [ dmsc startIndex = 1 ].
	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ (dmsc inputKey at: dmsc startIndex) = $X ].

	dmsc performInitialProcessing.

	self should: [ dmsc startIndex = 2 ].
	self should: [ dmsc primaryTranslation = 'S' ].
	self should: [ dmsc secondaryTranslation = 'S' ].
	self should: [ (dmsc inputKey at: dmsc startIndex) = $A ].!

testPerformInitialProcessing4
	dmsc inputKey: 'etaoinshrdlu'.
	self should: [ dmsc inputKey = 'ETAOINSHRDLU' ].

	self should: [ dmsc startIndex = 1 ].
	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ (dmsc inputKey at: dmsc startIndex) = $E ].

	dmsc performInitialProcessing.

	self should: [ dmsc startIndex = 2 ].
	self should: [ dmsc primaryTranslation = 'A' ].
	self should: [ dmsc secondaryTranslation = 'A' ].
	self should: [ (dmsc inputKey at: dmsc startIndex) = $T ].!

testProcessB_1
	dmsc inputKey: 'b'.
	self should: [ dmsc inputKey = 'B' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].

	dmsc processB.

	self should: [ dmsc primaryTranslation = 'P' ].
	self should: [ dmsc secondaryTranslation = 'P' ].
	self should: [ dmsc skipCount = 0 ]!

testProcessB_2
	dmsc inputKey: 'bb'.
	self should: [ dmsc inputKey = 'BB' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].

	dmsc processB.

	self should: [ dmsc primaryTranslation = 'P' ].
	self should: [ dmsc secondaryTranslation = 'P' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCedille
	dmsc inputKey: 'ç'.
	self should: [ dmsc inputKey = 'Ç' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].

	dmsc processCedille.

	self should: [ dmsc primaryTranslation = 'S' ].
	self should: [ dmsc secondaryTranslation = 'S' ].
!

testProcessCi_1
	dmsc inputKey: 'zzachxx'.
	self should: [ dmsc inputKey = 'ZZACHXX' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 4.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 2 ]!

testProcessCii_1
	dmsc inputKey: 'caesarian'.
	self should: [ dmsc inputKey = 'CAESARIAN' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	self should: [ dmsc currentIndex = 1 ].

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'S' ].
	self should: [ dmsc secondaryTranslation = 'S' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiii_1
	dmsc inputKey: 'chianti'.
	self should: [ dmsc inputKey = 'CHIANTI' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	self should: [ dmsc currentIndex = 1 ].

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiv_a_1
	dmsc inputKey: 'michael'.
	self should: [ dmsc inputKey = 'MICHAEL' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 3.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'X' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiv_b_1
	dmsc inputKey: 'character'.
	self should: [ dmsc inputKey = 'CHARACTER' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	self should: [ dmsc currentIndex = 1 ].

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiv_b_2
	dmsc inputKey: 'chymerae'.
	self should: [ dmsc inputKey = 'CHYMERAE' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	self should: [ dmsc currentIndex = 1 ].

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiv_c_1
	dmsc inputKey: 'chymerae'.
	self should: [ dmsc inputKey = 'CHYMERAE' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	self should: [ dmsc currentIndex = 1 ].

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiv_c_2
	dmsc inputKey: 'orchestral'.
	self should: [ dmsc inputKey = 'ORCHESTRAL' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 3.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiv_c_3
	dmsc inputKey: 'mchugh'.
	self should: [ dmsc inputKey = 'MCHUGH' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 2.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiv_c_4
	dmsc inputKey: 'xchugh'.
	self should: [ dmsc inputKey = 'XCHUGH' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 2.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'X' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCiv_c_5
	dmsc inputKey: 'chugh'.
	self should: [ dmsc inputKey = 'CHUGH' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 1.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'X' ].
	self should: [ dmsc secondaryTranslation = 'X' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCix_1
	dmsc inputKey: 'macgregor'.
	self should: [ dmsc inputKey = 'MACGREGOR' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 3.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCix_2
	dmsc inputKey: 'zzzzcizzz'.
	self should: [ dmsc inputKey = 'ZZZZCIZZZ' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 5.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'S' ].
	self should: [ dmsc secondaryTranslation = 'S' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCv_1
	dmsc inputKey: 'czarina'.
	self should: [ dmsc inputKey = 'CZARINA' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 1.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'S' ].
	self should: [ dmsc secondaryTranslation = 'X' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCvi_1
	dmsc inputKey: 'focaccia'.
	self should: [ dmsc inputKey = 'FOCACCIA' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 5.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'X' ].
	self should: [ dmsc secondaryTranslation = 'X' ].
	self should: [ dmsc skipCount = 2 ]!

testProcessCvii_1_1
	dmsc inputKey: 'accept'.
	self should: [ dmsc inputKey = 'ACCEPT' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 2.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'KS' ].
	self should: [ dmsc secondaryTranslation = 'KS' ].
	self should: [ dmsc skipCount = 2 ]!

testProcessCvii_1_2
	dmsc inputKey: 'succeed'.
	self should: [ dmsc inputKey = 'SUCCEED' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 3.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'KS' ].
	self should: [ dmsc secondaryTranslation = 'KS' ].
	self should: [ dmsc skipCount = 2 ]!

testProcessCvii_2
	dmsc inputKey: 'accord'.
	self should: [ dmsc inputKey = 'ACCORD' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 2.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 2 ]!

testProcessCviii_1
	dmsc inputKey: 'trucking'.
	self should: [ dmsc inputKey = 'TRUCKING' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 4.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCviii_2
	dmsc inputKey: 'acquire'.
	self should: [ dmsc inputKey = 'ACQUIRE' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 2.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCviii_3
	dmsc inputKey: 'macgregor'.
	self should: [ dmsc inputKey = 'MACGREGOR' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 3.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 1 ]!

testProcessCx_1
	dmsc inputKey: 'abcdefg'.
	self should: [ dmsc inputKey = 'ABCDEFG' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 3.

	dmsc processC.

	self should: [ dmsc primaryTranslation = 'K' ].
	self should: [ dmsc secondaryTranslation = 'K' ].
	self should: [ dmsc skipCount = 0 ]!

testProcessD_1
	dmsc inputKey: 'edge'.
	self should: [ dmsc inputKey = 'EDGE' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 2.

	dmsc processD.

	self should: [ dmsc primaryTranslation = 'J' ].
	self should: [ dmsc secondaryTranslation = 'J' ].
	self should: [ dmsc skipCount = 2 ].
!

testProcessD_2
	dmsc inputKey: 'edgar'.
	self should: [ dmsc inputKey = 'EDGAR' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 2.

	dmsc processD.

	self should: [ dmsc primaryTranslation = 'TK' ].
	self should: [ dmsc secondaryTranslation = 'TK' ].
	self should: [ dmsc skipCount = 1 ].
!

testProcessD_3
	dmsc inputKey: 'ludt'.
	self should: [ dmsc inputKey = 'LUDT' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 3.

	dmsc processD.

	self should: [ dmsc primaryTranslation = 'T' ].
	self should: [ dmsc secondaryTranslation = 'T' ].
	self should: [ dmsc skipCount = 1 ].
!

testProcessD_4
	dmsc inputKey: 'lud'.
	self should: [ dmsc inputKey = 'LUD' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 3.

	dmsc processD.

	self should: [ dmsc primaryTranslation = 'T' ].
	self should: [ dmsc secondaryTranslation = 'T' ].
	self should: [ dmsc skipCount = 0 ].
!

testProcessF_1
	dmsc inputKey: 'felt'.
	self should: [ dmsc inputKey = 'FELT' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 1.

	dmsc processF.

	self should: [ dmsc primaryTranslation = 'F' ].
	self should: [ dmsc secondaryTranslation = 'F' ].
	self should: [ dmsc skipCount = 0 ].
!

testProcessF_2
	dmsc inputKey: 'affenpinscher'.
	self should: [ dmsc inputKey = 'AFFENPINSCHER' ].

	self should: [ dmsc primaryTranslation = '' ].
	self should: [ dmsc secondaryTranslation = '' ].
	self should: [ dmsc skipCount = 0 ].
	dmsc currentIndex: 2.

	dmsc processF.

	self should: [ dmsc primaryTranslation = 'F' ].
	self should: [ dmsc secondaryTranslation = 'F' ].
	self should: [ dmsc skipCount = 1 ].
! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #setUp!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testKeyMidStringFrom!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testPerformInitialProcessing1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testPerformInitialProcessing2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testPerformInitialProcessing3!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testPerformInitialProcessing4!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessB_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessB_2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCedille!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCi_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCii_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiii_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiv_a_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiv_b_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiv_b_2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiv_c_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiv_c_2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiv_c_3!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiv_c_4!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCiv_c_5!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCix_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCix_2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCv_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCvi_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCvii_1_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCvii_1_2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCvii_2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCviii_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCviii_2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCviii_3!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessCx_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessD_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessD_2!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessD_3!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessD_4!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessF_1!public! !
!DoubleMetaphoneStringComparatorTest categoriesFor: #testProcessF_2!public! !

