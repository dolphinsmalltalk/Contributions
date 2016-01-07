| package |
package := Package name: 'US TypeConverter Extensions'.
package paxVersion: 1;
	basicComment: '$id: US TypeConverter Extensions 0.006$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '0.006'.


package classNames
	add: #CollectionToText;
	add: #DurationToTextConverter;
	add: #FloatToText;
	add: #GenericToText;
	add: #PercentToText;
	add: #WrappedConverter;
	yourself.

package methodNames
	add: #PluggableTypeConverter -> #leftToRightBlock;
	add: #PluggableTypeConverter -> #leftToRightBlock:;
	add: #PluggableTypeConverter -> #rightToLeftBlock;
	add: #PluggableTypeConverter -> #rightToLeftBlock:;
	add: 'PluggableTypeConverter class' -> #publishedAspectsOfInstances;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	yourself).

package!

"Class Definitions"!

TypeConverter subclass: #WrappedConverter
	instanceVariableNames: 'wrapper wrappee'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractToTextConverter subclass: #CollectionToText
	instanceVariableNames: 'elementTypeconverter printSeparator parseSeparator'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractToTextConverter subclass: #DurationToTextConverter
	instanceVariableNames: 'terms'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractToTextConverter subclass: #GenericToText
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NumberToText subclass: #FloatToText
	instanceVariableNames: 'decimalPlaces'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NumberToText subclass: #PercentToText
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!PluggableTypeConverter methodsFor!

leftToRightBlock
	^leftToRightBlock!

leftToRightBlock: anObject
	leftToRightBlock := anObject!

rightToLeftBlock
	^rightToLeftBlock!

rightToLeftBlock: anObject
	rightToLeftBlock := anObject! !
!PluggableTypeConverter categoriesFor: #leftToRightBlock!accessing!development!private! !
!PluggableTypeConverter categoriesFor: #leftToRightBlock:!accessing!development!private! !
!PluggableTypeConverter categoriesFor: #rightToLeftBlock!accessing!development!private! !
!PluggableTypeConverter categoriesFor: #rightToLeftBlock:!accessing!development!private! !

!PluggableTypeConverter class methodsFor!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."


	^(super publishedAspectsOfInstances)
		add: (Aspect name: #leftToRightBlock); add: (Aspect name: #rightToLeftBlock);
		yourself! !
!PluggableTypeConverter class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

WrappedConverter guid: (GUID fromString: '{53A2ED45-1E0A-4F98-8429-075F3A907C00}')!
WrappedConverter comment: ''!
!WrappedConverter categoriesForClass!TypeConverter-Base! !
!WrappedConverter methodsFor!

leftToRight: anObject 
	^wrappee convertFromLeftToRight: (wrapper convertFromLeftToRight: anObject)!

rightToLeft: anObject 
	^wrapper rightToLeft: (wrappee rightToLeft: anObject)!

setWrapper: aWrapper wrappee: aWrappee 	wrapper := aWrapper.	wrappee := aWrappee! !
!WrappedConverter categoriesFor: #leftToRight:!private! !
!WrappedConverter categoriesFor: #rightToLeft:!private! !
!WrappedConverter categoriesFor: #setWrapper:wrappee:!private! !

!WrappedConverter class methodsFor!

wrapper: aWrapper wrappee: aWrappee 	^ self new		setWrapper: aWrapper			wrappee: aWrappee;		yourself! !
!WrappedConverter class categoriesFor: #wrapper:wrappee:!public! !

CollectionToText guid: (GUID fromString: '{AAD7A8B3-0BA3-4785-B902-6F4FD580C110}')!
CollectionToText comment: ''!
!CollectionToText categoriesForClass!MVP-Type Converters-Abstract!MVP-Type Converters-General!MVP-Type Converters-Text! !
!CollectionToText methodsFor!

elementTypeconverter
	^elementTypeconverter!

elementTypeconverter: anObject 
	elementTypeconverter := anObject!

isLeftNullValue: anObject	^anObject isNilOrEmpty!

leftToRight: anObject 
	| stream |
	stream := ReadWriteStream on: String new.
	anObject do: [:each | stream nextPutAll: (elementTypeconverter convertFromLeftToRight: each)]
		separatedBy: [stream nextPutAll: printSeparator].
	^stream contents!

rightToLeft: aString 
	| elements lastFoundPosition nextFoundPosition |
	aString isNilOrEmpty ifTrue: [^self leftNullValue ].
	elements := OrderedCollection new.
	lastFoundPosition := 0.
	[(nextFoundPosition := aString findString: parseSeparator startingAt: lastFoundPosition + 1) > 0] 
		whileTrue: 
			[elements add: (aString copyFrom: lastFoundPosition + 1 to: nextFoundPosition - 1).
			lastFoundPosition := nextFoundPosition + parseSeparator size - 1].
	lastFoundPosition < aString size 
		ifTrue: [elements add: (aString copyFrom: lastFoundPosition + 1 to: aString size)].
	^elements collect: [:each | elementTypeconverter rightToLeft: each trimBlanks]!

setPintSeparator: print parseSeparator: parse elementConverter: aTypeConverter 
	printSeparator := print.
	parseSeparator := parse.
	elementTypeconverter := aTypeConverter! !
!CollectionToText categoriesFor: #elementTypeconverter!accessing!public! !
!CollectionToText categoriesFor: #elementTypeconverter:!accessing!public! !
!CollectionToText categoriesFor: #isLeftNullValue:!public! !
!CollectionToText categoriesFor: #leftToRight:!private! !
!CollectionToText categoriesFor: #rightToLeft:!private! !
!CollectionToText categoriesFor: #setPintSeparator:parseSeparator:elementConverter:!private! !

!CollectionToText class methodsFor!

applicableTypeConverterCategories
	"Answers a class category containing <typeConverter>s that can 
    	be used with the receiver"

	^Set with: (ClassCategory name: 'MVP-Type Converters-General') with: (ClassCategory name: 'MVP-Type Converters-Text')
		!

elementTypeconverter: aTypeConverter 
	^self 
		printSeparator: ', '
		parseSeparator: ','
		elementConverter: aTypeConverter!

new	^ self 		printSeparator: ', '		parseSeparator: ','		elementConverter: NullConverter new!

printSeparator: print parseSeparator: parse elementConverter: aTypeConverter 	^ super new		setPintSeparator: print			parseSeparator: parse			elementConverter: aTypeConverter;		yourself!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| applicableTypeConverters |
	applicableTypeConverters := Set new.
	self applicableTypeConverterCategories do: [:each | applicableTypeConverters addAll: each contents].
	^(super publishedAspectsOfInstances)
		add: (Aspect name: #elementTypeconverter
					chooseFrom: (applicableTypeConverters asSortedCollection asArray 
							collect: [:each | each name , ' new'])) 
						beImmutable;
		yourself!

separator: separator elementConverter: aTypeConverter 	^ self		printSeparator: separator		parseSeparator: separator		elementConverter: aTypeConverter! !
!CollectionToText class categoriesFor: #applicableTypeConverterCategories!constants!public! !
!CollectionToText class categoriesFor: #elementTypeconverter:!public! !
!CollectionToText class categoriesFor: #new!public! !
!CollectionToText class categoriesFor: #printSeparator:parseSeparator:elementConverter:!public! !
!CollectionToText class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!CollectionToText class categoriesFor: #separator:elementConverter:!public! !

DurationToTextConverter guid: (GUID fromString: '{D294D306-423D-4642-A9A1-1AA5626FA842}')!
DurationToTextConverter comment: ''!
!DurationToTextConverter categoriesForClass!TypeConverter-Text! !
!DurationToTextConverter methodsFor!

initialize
	super initialize.
	self terms: #('Year' 'Years' 'Day' 'Days' 'Minute' 'Minutes' 'Second' 'Seconds')!

leftToRight: aDuration 
	| stream days years |
	stream := ReadWriteStream on: String new.
	days := aDuration days.
	years := days // 365.
	days := days - (years * 365).
	years > 0 
		ifTrue: 
			[stream
				nextPutAll: years displayString;
				space;
				nextPutAll: (years > 1 ifTrue: [terms at: 2] ifFalse: [terms at: 1])].
	(years > 0 and: [days > 0]) ifTrue: [stream space].
	days > 0 
		ifTrue: 
			[stream
				nextPutAll: days displayString;
				space;
				nextPutAll: (days > 1 ifTrue: [terms at: 4] ifFalse: [terms at: 3])].
				
	(days > 0 and: [aDuration minutes > 0]) ifTrue: [stream space].
	aDuration minutes > 0 
		ifTrue: 
			[stream
				nextPutAll: aDuration minutes displayString;
				space;
				nextPutAll: (aDuration minutes > 1 ifTrue: [terms at: 6] ifFalse: [terms at: 5])].
	(aDuration minutes > 0 and: [aDuration seconds > 0]) ifTrue: [stream space].
	aDuration seconds > 0 
		ifTrue: 
			[stream
				nextPutAll: aDuration seconds displayString;
				space;
				nextPutAll: (aDuration seconds > 1 ifTrue: [terms at: 8] ifFalse: [terms at: 7])].
	^stream contents!

terms: anArray	terms := anArray! !
!DurationToTextConverter categoriesFor: #initialize!public! !
!DurationToTextConverter categoriesFor: #leftToRight:!private! !
!DurationToTextConverter categoriesFor: #terms:!public! !

!DurationToTextConverter class methodsFor!

german
	^self terms: #('Jahr' 'Jahre' 'Tag' 'Tage' 'Minute' 'Minuten' 'Sekunde' 'Sekunden')!

terms: anArray 	^ self new terms: anArray;		 yourself! !
!DurationToTextConverter class categoriesFor: #german!public! !
!DurationToTextConverter class categoriesFor: #terms:!public! !

GenericToText guid: (GUID fromString: '{C0349179-4C0E-4065-94F3-EDD66636B362}')!
GenericToText comment: ''!
!GenericToText categoriesForClass!TypeConverter-Text! !
!GenericToText methodsFor!

leftToRight: anObject 
	^anObject displayString!

rightToLeft: aString 
	^Object fromString: aString! !
!GenericToText categoriesFor: #leftToRight:!private! !
!GenericToText categoriesFor: #rightToLeft:!private! !

FloatToText guid: (GUID fromString: '{9DFF7317-C813-4845-B150-F7CE87FD3AAD}')!
FloatToText comment: ''!
!FloatToText categoriesForClass!TypeConverter-Text! !
!FloatToText methodsFor!

decimalPlaces: anInteger	decimalPlaces := anInteger!

initialize	super initialize.	decimalPlaces := 6!

leftToRight: aFloat 
	^super leftToRight: (self toDecimalPlaces: aFloat)!

rightToLeft: aString 
	^self toDecimalPlaces: (super rightToLeft: aString)!

toDecimalPlaces: aNumber 	^ ((aNumber asFloat		* (10 raisedToInteger: decimalPlaces)) rounded		/ (10 raisedToInteger: decimalPlaces)) asFloat! !
!FloatToText categoriesFor: #decimalPlaces:!public! !
!FloatToText categoriesFor: #initialize!private! !
!FloatToText categoriesFor: #leftToRight:!private! !
!FloatToText categoriesFor: #rightToLeft:!private! !
!FloatToText categoriesFor: #toDecimalPlaces:!public! !

!FloatToText class methodsFor!

decimalPlaces: anInteger 	^ self new decimalPlaces: anInteger;		 yourself! !
!FloatToText class categoriesFor: #decimalPlaces:!public! !

PercentToText guid: (GUID fromString: '{AD11B882-626D-4B05-879C-5B951E799FCF}')!
PercentToText comment: ''!
!PercentToText categoriesForClass!TypeConverter-Text! !
!PercentToText methodsFor!

leftToRight: aNumber 
	^(super leftToRight: aNumber * 100) , '%'!

rightToLeft: aString 
	^(aString includes: $%) 
		ifTrue: [(super rightToLeft: (aString reject: [:each | each = $%])) / 100]
		ifFalse: [super rightToLeft: aString]! !
!PercentToText categoriesFor: #leftToRight:!private! !
!PercentToText categoriesFor: #rightToLeft:!private! !

"Binary Globals"!

