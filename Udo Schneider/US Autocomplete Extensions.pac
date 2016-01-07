| package |
package := Package name: 'US Autocomplete Extensions'.
package paxVersion: 1;
	basicComment: '$id: US Autocomplete Extensions 0.034$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This package adds bitmaps in front of the entries in autocomplete lists in Smalltalk Workspaces and Searchboxes.

If the images are somehow screwed up rebuild the image cache using:
	AutoCompleteImageManager current reset.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.034'.


package classNames
	add: #AutoCompleteImageManager;
	yourself.

package methodNames
	add: #ClassDescription -> #autocompleteString;
	add: #ClassDescription -> #autocompleteString:;
	add: #CompiledCode -> #autocompleteString;
	add: #Image -> #autoCompleteImage;
	add: #Object -> #autoCompleteImage;
	add: #Object -> #autoCompleteImageKey;
	add: #Object -> #autoCompleteImageXpmString;
	add: #Package -> #autocompleteString;
	add: #SmalltalkSearchPresenter -> #completionListSortBlock;
	add: #SmalltalkSearchPresenter -> #methodsContaining:;
	add: #SmalltalkSearchPresenter -> #searchItemsStartingWith:maxItems:;
	add: #SmalltalkSearchPresenter -> #searchObject;
	add: #SmalltalkSearchPresenter -> #showCompletionList:prefixLength:;
	add: #SmalltalkWorkspace -> #autoCompleteSelectorsFor:;
	add: #SmalltalkWorkspace -> #completionListSortBlock;
	add: #SmalltalkWorkspace -> #identifiersStartingWith:maxItems:;
	add: #SmalltalkWorkspace -> #messagesForToken:startingWith:maxItems:;
	add: #SmalltalkWorkspace -> #showCompletionList:prefixLength:;
	add: #SmalltalkWorkspace -> #showSelectorCompletionListAt:maxItems:;
	add: #SmalltalkWorkspace -> #showSymbolCompletionListAt:maxItems:;
	add: #Symbol -> #autoCompleteImageKey;
	add: #Symbol -> #autocompleteString;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Dialogs\Progress\Dolphin Progress Dialog';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: 'Graphics\US Graphics Extensions';
	add: 'Graphics\US Octree Quantizer';
	add: 'US Runtime Patches';
	add: 'Graphics\US XPM Converter';
	yourself).

package setManualPrerequisites: #(
	'US Graphics Extensions'
	'US Octree Quantizer'
	'US Runtime Patches'
	'US XPM Converter').

package!

"Class Definitions"!

Object subclass: #AutoCompleteImageManager
	instanceVariableNames: 'objects images'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ClassDescription methodsFor!

autocompleteString
	^AutoCompleteImageManager current stringForClass: self!

autocompleteString: selector 
	^AutoCompleteImageManager current stringForClass: self selector: selector! !
!ClassDescription categoriesFor: #autocompleteString!autocompletion!helpers!private! !
!ClassDescription categoriesFor: #autocompleteString:!autocompletion!helpers!private! !

!CompiledCode methodsFor!

autocompleteString
^self methodClass autocompleteString: self selector! !
!CompiledCode categoriesFor: #autocompleteString!autocompletion!helpers!private! !

!Image methodsFor!

autoCompleteImage
	"Return an represenation of self on a 24bit DIB with Small Icon extent"

	| dib canvas |
	dib := DIBSection 
				width: AutoCompleteImageManager imageExtent x
				height: AutoCompleteImageManager imageExtent y + 1
				depth: 24.	"We add onw pixel row here to avoid icons sticking together"
	canvas := dib canvas.
	canvas
		backcolor: AutoCompleteImageManager imageBackroundColor;
		erase.
	self 
		drawOn: canvas
		at: Point zero
		extent: Icon smallExtent.
	^dib! !
!Image categoriesFor: #autoCompleteImage!public! !

!Object methodsFor!

autoCompleteImage
"Return the instance/class icon as AutoComplete Image (16x16x24)"
	^self icon autoCompleteImage!

autoCompleteImageKey
"Return the AutoComplete Image key. As different classes can share the same icon the key is the icon itself"
	^self icon!

autoCompleteImageXpmString
	"Return the XPM String of the AutoComplete Image"

	| image  xpmString |
	image := self autoCompleteImage.
	xpmString := image xpmString.
	"Release AutoCompleteImage to avoid resorce problems"
	image free.
	^xpmString! !
!Object categoriesFor: #autoCompleteImage!public! !
!Object categoriesFor: #autoCompleteImageKey!public! !
!Object categoriesFor: #autoCompleteImageXpmString!public! !

!Package methodsFor!

autocompleteString
	^AutoCompleteImageManager current stringForPackage: self! !
!Package categoriesFor: #autocompleteString!autocompletion!helpers!private! !

!SmalltalkSearchPresenter methodsFor!

completionListSortBlock
	| crt |
	crt := CRTLibrary default.
	^[:a :b | (crt strcmp: (a upTo: $?) string2: (b upTo: $?)) <= 0]!

methodsContaining: aString 
	| filter |
	aString size < 3 ifTrue: [^#()].
	filter := (MethodSourceSearch newPattern: aString) methodReferenceFilter.
	^Cursor wait showWhile: 
			[(PluggableEnvironment onEnvironment: SmalltalkSystem current systemEnvironment select: filter) 
				allMethods]!

searchItemsStartingWith: aString maxItems: anInteger 
	|  result classes |
	aString first isLowerCase 
		ifTrue: 
			[| matchingSymbols |
			matchingSymbols := self selectorsStartingWith: aString maxItems: anInteger.
			^matchingSymbols collect: [:each | each autocompleteString]].
	result := OrderedCollection new.
	classes := environment classes select: [:each | each name first = aString first].
	classes do: 
			[:eachClass | 
			result add: eachClass autocompleteString.
			(environment selectorsForClass: eachClass) 
				do: [:sel | result add: (eachClass autocompleteString: sel)]].
	Package manager packages do: [:each | result add: each autocompleteString].
	(aString beginsWith: '''') 
		ifTrue: 
			[| matchingMethods |
			matchingMethods := self methodsContaining: (aString copyFrom: 2).
			^matchingMethods collect: [:each | aString , ''' in ' , each autocompleteString]].
	^result select: [:each | each beginsWith: aString]!

searchObject
	^
	[| searchText evaluateText |
	searchText := self searchText.
	searchText first isLowerCase ifTrue: [^Symbol findInterned: searchText].
	evaluateText := searchText copyReplaceAll: '>>' with: '>>#'.
	(evaluateText beginsWith: '''') ifTrue: [evaluateText := evaluateText copyFrom: (evaluateText indexOfSubCollection: '''' startingAt: 2) +4 ].
	Compiler evaluate: evaluateText] 
			on: Exception
			do: [:x | Package manager packages detect: [:each | each name = self searchText] ifNone: []]!

showCompletionList: aCollectionOfStrings prefixLength: anInteger 
	(aCollectionOfStrings size = 1 and: [aCollectionOfStrings anyOne size = anInteger]) 
		ifTrue: 
			["Don't display a list of one item that is the same as the prefix"
			^self].
	AutoCompleteImageManager current updateImagesFor: self view.
	view 
		showAutoCompletionList: (aCollectionOfStrings asSortedCollection: self completionListSortBlock)
		prefixLength: anInteger! !
!SmalltalkSearchPresenter categoriesFor: #completionListSortBlock!autocompletion!constants!private! !
!SmalltalkSearchPresenter categoriesFor: #methodsContaining:!private! !
!SmalltalkSearchPresenter categoriesFor: #searchItemsStartingWith:maxItems:!autocompletion!helpers!private! !
!SmalltalkSearchPresenter categoriesFor: #searchObject!commands!public! !
!SmalltalkSearchPresenter categoriesFor: #showCompletionList:prefixLength:!autocompletion!helpers!private! !

!SmalltalkWorkspace methodsFor!

autoCompleteSelectorsFor: aClass 
	| acim selectors |

	selectors := PluggableSet new: self size
				searchPolicy: (PluggableSearchPolicy newCompareBlock: [:x :y | (x upTo: $?) = (y upTo: $?)]
						hashBlock: [:x | (x upTo: $?) hash]).
	acim := AutoCompleteImageManager current.
	selectors
		addAll: ((MethodCategory deprecatedMethods methodsInBehavior: aClass) 
					collect: [:each | acim stringForDeprecatedMessage: each selector]);
		addAll: (((MethodCategory name: 'development') methodsInBehavior: aClass) 
					collect: [:each | acim stringForDevelopementMessage: each selector]);
		addAll: ((MethodCategory private methodsInBehavior: aClass) 
					collect: [:each | acim stringForPrivateMessage: each selector]);
		addAll: ((MethodCategory public methodsInBehavior: aClass) 
					collect: [:each | acim stringForPublicMessage: each selector]).
	^selectors!

completionListSortBlock
	| crt |
	crt := CRTLibrary default.
	^self isAutoCompletionCaseInsensitive 
		ifTrue: [[:a :b | (crt _stricmp: (a upTo: $?) string2: (b upTo: $?)) <= 0]]
		ifFalse: [[:a :b | (crt strcmp: (a upTo: $?) string2: (b upTo: $?)) <= 0]]!

identifiersStartingWith: aString maxItems: anInteger 
	"Private - Build and answer a colleciton of identifiers that are potential completions for
	the specified prefix. If in case-sensitive mode assume that the convention of starting all
	class/pool/global variables with an uppercase letter, and all temps and inst. vars with a
	lowercase one, is followed. If there are more matching identifiers than the specified
	maximum, then answer an empty collection."

	| acim variables ignoreCase filter |
	acim := AutoCompleteImageManager current.
	variables := Set new.
	ignoreCase := self isAutoCompletionCaseInsensitive.
	filter := aString isEmpty 
				ifTrue: 
					[
					[:eachName | 
					variables add: eachName.
					variables size > anInteger ifTrue: [^#()]]]
				ifFalse: 
					[
					[:eachName | 
					(eachName beginsWith: aString ignoreCase: ignoreCase) 
						ifTrue: 
							[variables add: eachName.
							variables size > anInteger ifTrue: [^#()]]]].
	(ignoreCase or: [aString isEmpty or: [aString first isLowercase]]) 
		ifTrue: 
			[Compiler reservedWords 
				do: [:eachReservedWord | filter value: (acim stringForResevedWord: eachReservedWord)].
			self 
				allDefinedVariablesDo: [:eachDefinedVariable | filter value: (acim stringForMethodVariable: eachDefinedVariable)].
			self selfClass allInstVarNames 
				do: [:eachInstanceVariable | filter value: (acim stringForInstanceVariable: eachInstanceVariable)]].
	(ignoreCase or: [aString isEmpty or: [aString first isUppercase]]) 
		ifTrue: 
			[| class |
			class := self selfClass instanceClass.
			class withAllSuperclassesDo: 
					[:eachClass | 
					eachClass classPool 
						keysDo: [:eachClassPool | filter value: (acim stringForClassVariable: eachClassPool)].
					eachClass sharedPools 
						do: [:eachPool | eachPool keysDo: [:eachSharedPool | filter value: (acim stringForSharedPool: eachSharedPool)]]].
			evaluationPools do: 
					[:each | 
					each 
						keysDo: [:eachEvaluationPool | filter value: (acim stringForEvaluationPool: eachEvaluationPool)]].
			class environment keysDo: [:eachGlobalName | filter value: (acim stringForGlobal: eachGlobalName)]].
	^variables!

messagesForToken: anAssociation startingWith: aString maxItems: anInteger 
	"Private - Answer the set of selectors that could potentially be sent as messages following
	the specified token."

	| selectors ignoreCase |
	
	ignoreCase := self isAutoCompletionCaseInsensitive.
	(self classForToken: anAssociation) 
		ifNil: 
			[| allSelectors |
			allSelectors := Smalltalk developmentSystem allSelectors.
			aString isEmpty 
				ifTrue: [selectors := allSelectors size > anInteger ifTrue: [#()] ifFalse: [allSelectors]]
				ifFalse: 
					[selectors := Set new.
					allSelectors do: 
							[:eachName | 
							(eachName beginsWith: aString ignoreCase: ignoreCase) 
								ifTrue: 
									[selectors add: eachName.
									selectors size > anInteger ifTrue: [^#()]]]].
			selectors := selectors 
						collect: [:eachSelector | AutoCompleteImageManager current stringForMessage: eachSelector]]
		ifNotNil: 
			[:class | 
			selectors := PluggableSet new: self size
						searchPolicy: (PluggableSearchPolicy newCompareBlock: [:x :y | (x upTo: $?) = (y upTo: $?)]
								hashBlock: [:x | (x upTo: $?) hash]).
			class withAllSuperclassesDo: (aString isEmpty 
						ifTrue: 
							[
							[:eachClass | 
							selectors addAll: (self autoCompleteSelectorsFor: eachClass).
							selectors size > anInteger ifTrue: [^#()]]]
						ifFalse: 
							[
							[:eachClass | 
							(self autoCompleteSelectorsFor: eachClass) do: 
									[:eachSelector | 
									(eachSelector beginsWith: aString ignoreCase: ignoreCase) 
										ifTrue: 
											[selectors add: eachSelector.
											selectors size > anInteger ifTrue: [^#()]]]]])].
	^selectors!

showCompletionList: aCollectionOfStrings prefixLength: anInteger 

	(aCollectionOfStrings size = 1 and: [aCollectionOfStrings anyOne size = anInteger]) 
		ifTrue: 
			["Don't display a list of one item that is the same as the prefix"
			^self].
	AutoCompleteImageManager current updateImagesFor: self view.
	view 
		showAutoCompletionList: (aCollectionOfStrings asSortedCollection: self completionListSortBlock)
		prefixLength: anInteger!

showSelectorCompletionListAt: posInteger maxItems: maxInteger 
	| prefix start choices |
	start := self tokenStartAt: posInteger.
	prefix := view plainTextFrom: start to: posInteger.
	choices := self selectorsStartingWith: prefix maxItems: maxInteger.
	choices := choices collect: [:each | AutoCompleteImageManager current stringForSelector: each].
	self showCompletionList: choices prefixLength: prefix size!

showSymbolCompletionListAt: posInteger maxItems: maxInteger 
	| prefix start symbols hashed prefixLength |
	start := self tokenStartAt: posInteger.
	prefix := view plainTextRange: (start to: posInteger).
	prefixLength := prefix size.
	(hashed := prefix first == $#) ifTrue: [prefix := prefix copyFrom: 2].
	symbols := self symbolsStartingWith: prefix maxItems: maxInteger.
	symbols := symbols collect: [:each | AutoCompleteImageManager current stringForSymbol: each].
	self 
		showCompletionList: (hashed ifTrue: [symbols collect: [:each | '#' , each]] ifFalse: [symbols])
		prefixLength: prefixLength! !
!SmalltalkWorkspace categoriesFor: #autoCompleteSelectorsFor:!autocompletion!helpers!private! !
!SmalltalkWorkspace categoriesFor: #completionListSortBlock!autocompletion!constants!private! !
!SmalltalkWorkspace categoriesFor: #identifiersStartingWith:maxItems:!autocompletion!helpers!private! !
!SmalltalkWorkspace categoriesFor: #messagesForToken:startingWith:maxItems:!autocompletion!helpers!private! !
!SmalltalkWorkspace categoriesFor: #showCompletionList:prefixLength:!autocompletion!helpers!private! !
!SmalltalkWorkspace categoriesFor: #showSelectorCompletionListAt:maxItems:!autocompletion!helpers!private! !
!SmalltalkWorkspace categoriesFor: #showSymbolCompletionListAt:maxItems:!autocompletion!helpers!private! !

!Symbol methodsFor!

autoCompleteImageKey
"For some special objects there is no icon we can use as a key. So we enable symbols to be their own key"
	^self!

autocompleteString
	^AutoCompleteImageManager current stringForSymbol: self displayString! !
!Symbol categoriesFor: #autoCompleteImageKey!public! !
!Symbol categoriesFor: #autocompleteString!autocompletion!helpers!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

AutoCompleteImageManager guid: (GUID fromString: '{29425450-EC78-49A9-AE9A-6F5178313CD5}')!
AutoCompleteImageManager comment: ''!
!AutoCompleteImageManager categoriesForClass!Unclassified! !
!AutoCompleteImageManager methodsFor!

add: anObject 
	(objects includes: anObject autoCompleteImageKey) 
		ifFalse: [self at: anObject put: anObject autoCompleteImageXpmString]!

at: anObject put: anXpmString 
	| key |
	key := anObject autoCompleteImageKey.
	(objects includes: key) 
		ifFalse: 
			[objects add: key.
			images add: anXpmString].
	^objects indexOf: key!

clearRegisteredImagesInViews
	ScintillaView allInstances collect: [:each | each propertyAt: #lastRegisteredAutoCompleteImageIndex put: 0]!

image: backgroundImage overlay: overlayImage 
	| image canvas |
	image := DIBSection 
				width: self class imageExtent x
				height: self class imageExtent y
				depth: 24.
	canvas := image canvas.
	canvas
		backcolor: AutoCompleteImageManager imageBackroundColor;
		erase.
	backgroundImage 
		drawOn: canvas
		at: Point zero
		extent: image extent.
	overlayImage 
		drawOn: canvas
		at: (image extent * (1 / 4)) truncated
		extent: (image extent * (3 / 4)) truncated.
	^image!

imagesMemory
	^images inject: 0 into: [:sum :each | sum+ each size]!

indexAt: anObject 
	| key |
	key := anObject autoCompleteImageKey.
	(objects includes: key) ifFalse: [self add: anObject].
	^objects indexOf: key!

initialize
	
	self reset!

registerClassImages
	| allClasses |
	MemoryManager current collectGarbage.	"Collect Garbage to ensure free space as we are going to create a lot of (short-lived) GDI objects here"
	allClasses := Class allClasses.
	(ProgressDialog operation: 
			[:progress | 
			(1 to: allClasses size) do: 
					[:index | 
					| eachClass |
					eachClass := allClasses at: index.
					progress
						text: eachClass displayString;
						value: index / allClasses size * 100.
					[self add: eachClass] on: Error
						do: 
							[:ex | 
							Notification signal: 'Error during AutoComplete Image creation for class ' , eachClass name 
										, '. This can be ignored. The image will be created on the fly later']].
			'completed'])
		caption: 'Generating AutoComplete Class Images ...';
		showModal.
	MemoryManager current collectGarbage	"Collect Garbage to ensure free space as we createda lot of (short-lived) GDI objects here"!

registerImages
	self clearRegisteredImagesInViews;
		registerSpecialImages;
		registerClassImages!

registerSpecialImages
	self
		at: #methodVariable put: self class imageMethodVariable autoCompleteImageXpmString;
		at: #instanceVartiable put: self class imageInstanceVariable autoCompleteImageXpmString;
		at: #classVariable put: self class imageClassVariable autoCompleteImageXpmString;
		at: #self put: self class imageSelf autoCompleteImageXpmString;
		at: #super put: self class imageSuper autoCompleteImageXpmString;
		at: #thisContext put: self class imageThisContext autoCompleteImageXpmString;
		at: #method put: self class imageMethod autoCompleteImageXpmString;
		at: #privateMethod put: self class imagePrivateMethod autoCompleteImageXpmString;
		at: #publicMethod put: self class imagePublicMethod autoCompleteImageXpmString;
		at: #deprecatedMethod put: self class imageDeprecatedMethod autoCompleteImageXpmString;
		at: #developmentMethod put: self class imageDevelopmentSystemMethod autoCompleteImageXpmString!

reset
	objects := OrderedCollection new.
	images := OrderedCollection new.
	self registerImages!

stringForClass: aClass 
	"Return a string consisting of the class name and appended AutoComplete Image Index.
	Check if aClass is a Metaclass - in this case use it's instance class icon"

	^self stringForClass: aClass selector: nil!

stringForClass: aClass selector: selector 
	"Return a string consisting of the class name, selector and appended AutoComplete Image Index.
	Check if aClass is a Metaclass - in this case use it's instance class icon"

	| iconClass |
	iconClass := aClass isMeta ifFalse: [aClass] ifTrue: [aClass instanceClass].
	^aClass name , (selector isNil  ifTrue: [''] ifFalse: ['>>' , selector] ) , (self suffixFor: iconClass)!

stringForClassVariable: classVariableName 
	"Return a string consisting of the class varible name and appended AutoComplete Image Index."

	^classVariableName , (self suffixFor: #classVariable)!

stringForDeprecatedMessage: selector 
	"Return a string consisting of the message selector and appended AutoComplete Image Index."

	^selector , (self suffixFor: #deprecatedMethod )!

stringForDevelopementMessage: selector 
	"Return a string consisting of the message selector and appended AutoComplete Image Index."

	^selector , (self suffixFor: #developmentMethod )!

stringForEvaluationPool: evaluationPoolName 
	"Return a string consisting of the evaluation pool name and appended AutoComplete Image Index."

	^evaluationPoolName , (self suffixFor: PoolDictionary)!

stringForGlobal: eachGlobalName 
	"Return a string consisting of the global name and appended AutoComplete Image Index"

	^eachGlobalName , (self suffixFor: (Smalltalk at: eachGlobalName))!

stringForInstanceVariable: instanceVariableName 
	"Return a string consisting of the instance variable and appended AutoComplete Image Index."

	^instanceVariableName , (self suffixFor: #instanceVartiable)!

stringForMessage: selector 
	"Return a string consisting of the message selector and appended AutoComplete Image Index."

	^selector , (self suffixFor: #method)!

stringForMethodVariable: methodVariableName 
	"Return a string consisting of the method varible name and appended AutoComplete Image Index."

	^methodVariableName , (self suffixFor: #methodVariable)!

stringForPackage: aPackage 
	"Return a string consisting of the package name and appended AutoComplete Image Index."

	^aPackage name , (self suffixFor: Package)!

stringForPrivateMessage: selector 
	"Return a string consisting of the message selector and appended AutoComplete Image Index."

	^selector , (self suffixFor: #privateMethod)!

stringForPublicMessage: selector 
	"Return a string consisting of the message selector and appended AutoComplete Image Index."

	^selector , (self suffixFor: #publicMethod )!

stringForResevedWord: reservedWord 
	"Return a string consisting of the reserved word and appended AutoComplete Image Index."

	^reservedWord , (self 
				suffixFor: (##((Dictionary new)
						at: 'nil' put: nil;
						at: 'true' put: true;
						at: 'false' put: false;
						at: 'thisContext' put: #thisContext;
						at: 'self' put: #self;
						at: 'super' put: #super;
						yourself) at: reservedWord ))!

stringForSelector: selector 
	"Return a string consisting of the selector and appended AutoComplete Image Index."

	^selector , (self suffixFor: CompiledMethod)!

stringForSharedPool: sharedPoolName 
	"Return a string consisting of the shared pool and appended AutoComplete Image Index."

	^sharedPoolName , (self suffixFor: PoolDictionary)!

stringForSymbol: aSymbol 
	"Return a string consisting of the symbol and appended AutoComplete Image Index."

	^aSymbol , (self suffixFor: Symbol)!

suffixFor: anObject 
	"Append the integer ID representing anObject to the string"

	^'?' , (self indexAt: anObject) displayString!

updateImagesFor: aScintillaView 
	| lastRegisteredIndex |
	lastRegisteredIndex := aScintillaView propertyAt: #lastRegisteredAutoCompleteImageIndex
				ifAbsent: [aScintillaView propertyAt: #lastRegisteredAutoCompleteImageIndex put: 0].
	lastRegisteredIndex < objects size 
		ifTrue: 
			[lastRegisteredIndex + 1 to: objects size
				do: [:imageIndex | aScintillaView sciRegisterImage: imageIndex xpmData: (images at: imageIndex)].
			aScintillaView propertyAt: #lastRegisteredAutoCompleteImageIndex put: objects size]!

xpmAt: anObject 
	| key |
	key := anObject autoCompleteImageKey.
	(objects includes: key) ifFalse: [self add: anObject].
	^images at: (objects indexOf: key)! !
!AutoCompleteImageManager categoriesFor: #add:!accessing!public! !
!AutoCompleteImageManager categoriesFor: #at:put:!accessing!public! !
!AutoCompleteImageManager categoriesFor: #clearRegisteredImagesInViews!helpers!private! !
!AutoCompleteImageManager categoriesFor: #image:overlay:!helpers!private! !
!AutoCompleteImageManager categoriesFor: #imagesMemory!public! !
!AutoCompleteImageManager categoriesFor: #indexAt:!accessing!public! !
!AutoCompleteImageManager categoriesFor: #initialize!initialization!private! !
!AutoCompleteImageManager categoriesFor: #registerClassImages!helpers!initialization!private! !
!AutoCompleteImageManager categoriesFor: #registerImages!helpers!initialization!private! !
!AutoCompleteImageManager categoriesFor: #registerSpecialImages!helpers!initialization!private! !
!AutoCompleteImageManager categoriesFor: #reset!helpers!private! !
!AutoCompleteImageManager categoriesFor: #stringForClass:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForClass:selector:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForClassVariable:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForDeprecatedMessage:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForDevelopementMessage:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForEvaluationPool:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForGlobal:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForInstanceVariable:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForMessage:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForMethodVariable:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForPackage:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForPrivateMessage:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForPublicMessage:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForResevedWord:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForSelector:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForSharedPool:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #stringForSymbol:!autocompletion!helpers!public! !
!AutoCompleteImageManager categoriesFor: #suffixFor:!autocompletion!helpers!private! !
!AutoCompleteImageManager categoriesFor: #updateImagesFor:!helpers!private! !
!AutoCompleteImageManager categoriesFor: #xpmAt:!accessing!public! !

!AutoCompleteImageManager class methodsFor!

current
Current ifNil: [Current  := self new].
 ^Current!

icon
^WinImageList icon!

imageBackroundColor
^Color window!

imageClassVariable
	^Icon fromFile: self imagePath , 'ClassVariable.ico'!

imageDeprecatedMethod
	"^self image: MessageSend icon overlay: (Icon fromId: 'COMPILEDMETHOD_DEPRECATED.ICO')"

	^Icon fromFile: self imagePath , 'DeprecatedMessageSend.ico'!

imageDevelopmentSystemMethod
	"^self image: MessageSend icon overlay: (Icon fromId: '!!APPLICATION')"

	^Icon fromFile: self imagePath , 'DevelopmentMessageSend.ico'!

imageExtent
	^Icon smallExtent!

imageInstanceVariable
	^Icon fromFile: self imagePath , 'InstanceVariable.ico'!

imageMethod
	^Icon fromFile: self imagePath , 'MessageSend.ico'
	"^MessageSend icon"!

imageMethodVariable
	^Icon fromFile: self imagePath , 'MethodVariable.ico'!

imagePath

	^FileLocator imageRelative localFileSpecFor: 'Udo Schneider\Goodies\Resources\'!

imagePrivateMethod
	"^self image: MessageSend icon overlay: (Icon fromId: 'COMPILEDMETHOD_PRIVATE.ICO')"

	^Icon fromFile: self imagePath , 'PrivateMessageSend.ico'!

imagePublicMethod
	"^self image: MessageSend icon overlay: (Icon fromId: 'COMPILEDMETHOD_PUBLIC.ICO')"

	^Icon fromFile: self imagePath , 'PublicMessageSend.ico'!

imageSelf
	^Icon fromFile: self imagePath , 'self.ico'!

imageSuper
	^Icon fromFile: self imagePath , 'super.ico'!

imageThisContext
	^Icon fromFile: self imagePath , 'Context.ico'!

initialize
	"Ensure that the current instance is reseted and re-created"

	self
		reset;
		current.
	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self.
	SmalltalkSystemShell default 
		when: #sysColorChanged
		send: #onSysColorChange
		to: self.
	^super initialize!

new
^super new initialize!

onStartup
	self current clearRegisteredImagesInViews!

onSysColorChange
	self current reset!

reset
	Current ifNotNil: [:value | value clearRegisteredImagesInViews].
	Current := nil! !
!AutoCompleteImageManager class categoriesFor: #current!instance creation!public! !
!AutoCompleteImageManager class categoriesFor: #icon!public! !
!AutoCompleteImageManager class categoriesFor: #imageBackroundColor!Constants!public! !
!AutoCompleteImageManager class categoriesFor: #imageClassVariable!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imageDeprecatedMethod!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imageDevelopmentSystemMethod!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imageExtent!Constants!public! !
!AutoCompleteImageManager class categoriesFor: #imageInstanceVariable!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imageMethod!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imageMethodVariable!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imagePath!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imagePrivateMethod!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imagePublicMethod!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imageSelf!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imageSuper!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #imageThisContext!Constants!helpers!Images!private! !
!AutoCompleteImageManager class categoriesFor: #initialize!initializing!private! !
!AutoCompleteImageManager class categoriesFor: #new!instance creation!public! !
!AutoCompleteImageManager class categoriesFor: #onStartup!event handling!private! !
!AutoCompleteImageManager class categoriesFor: #onSysColorChange!public! !
!AutoCompleteImageManager class categoriesFor: #reset!private! !

"Binary Globals"!

