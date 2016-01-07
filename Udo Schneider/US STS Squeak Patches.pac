| package |
package := Package name: 'US STS Squeak Patches'.
package paxVersion: 1;
	basicComment: '$id: US STS Squeak Patches 0.010$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.010'.


package methodNames
	add: #StsSqueakMonticelloPackageImporter -> #setVersionAndVersionCommentFor:from:;
	add: #StsSqueakPackageProxy -> #importClassDefinition:;
	add: #StsSqueakPackageProxy -> #isClassBegin:;
	add: #StsSqueakPackageProxy -> #loadFrom:;
	add: #StsSqueakPackageProxy -> #parseMethod:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\ITC Gorisek\Source Tracking System';
	add: 'US Locale Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Locale Extensions').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!StsSqueakMonticelloPackageImporter methodsFor!

setVersionAndVersionCommentFor: package from: fileName 
	| fs array string versionString date time index |
	fs := FileStream read: fileName.
	[string := fs contents] ensure: [fs close].
	(array := Compiler evaluate: '#' , string) isNil 
		ifFalse: 
			[date := ''.
			time := ''.
			versionString := ''.
			(index := array indexOf: #message) > 0 ifTrue: [package comment: (array at: index + 1)].
			(index := array indexOf: #author) > 0 ifTrue: [package developer: (array at: index + 1)].
			(index := array indexOf: #date) > 0 
				ifTrue: 
					[date := array at: index + 1.
					date := Date 
								newDay: (Integer fromString: date subStrings first)
								monthIndex: (Locale englishUS monthNames at: date subStrings second asLowercase)
								year: (Integer fromString: (date subStrings at: 3))].
			(index := array indexOf: #time) > 0 
				ifTrue: 
					[Locale evaluate: [time := Time fromString: (array at: index + 1)] usingLocale: Locale englishUS].
			(index := array indexOf: #name) > 0 ifTrue: [versionString := array at: index + 1].
			package packageVersion: versionString;
			 timestamp: (TimeStamp date: date time: time)]! !
!StsSqueakMonticelloPackageImporter categoriesFor: #setVersionAndVersionCommentFor:from:!public! !

!StsSqueakPackageProxy methodsFor!

importClassDefinition: firstLine 
	| classProxy tokens str className instVars classVars poolDicts |
	className := firstLine.
	(className isNil or: 
			[self isClassBegin: className ]) 
		ifFalse: [^false].
	tokens := className subStrings.
	(className endsWith: 'class') 
		ifTrue: 
			[classProxy := self findOrCreateClassNamed: (tokens at: 1) asSymbol.
			str := self nextLineFromStream trimBlanks.
			self 
				createCompleteClassDefinition: classProxy
				name: nil
				instVars: nil
				classVars: nil
				poolDicts: nil
				classInstVars: (self removeEndingSpace: 'classI' , (str copyFrom: 2 to: str size - 1))]
		ifFalse: 
			[str := tokens at: 3.
			classProxy := self findOrCreateClassNamed: (str copyFrom: 2 to: str size) asSymbol.
			instVars := self nextLineFromStream trimBlanks.
			classVars := self nextLineFromStream trimBlanks.
			str := self nextLineFromStream trimBlanks.
			poolDicts := str copyFrom: 1 to: str size.
			self 
				createCompleteClassDefinition: classProxy
				name: className
				instVars: (self removeEndingSpace: instVars)
				classVars: (self removeEndingSpace: classVars)
				poolDicts: (self removeEndingSpace: poolDicts)
				classInstVars: nil].
	(str := self nextLineFromStream) isEmpty 
		ifFalse: 
			["category := str subStrings at: 2"
			].
	^true!

isClassBegin: aString 

	^(#('* subclass: *' '* variableSubclass: *' '* variableByteSubclass: *') 
		anySatisfy: [:each | each match: aString]) or: [aString endsWith: 'class']!

loadFrom: pathname 
	| str |
	classes := OrderedCollection new.
	looseMethods := OrderedCollection new.
	version := ''.
	comment := ''.
	resources := OrderedCollection new.
	name := File removeExtension: (File splitFilenameFrom: pathname).
	stream := FileStream read: pathname text: true.
	
	
	[
	[str := self nextLineFromStream.
	str isNil or: [(self isClassBegin: str) or: [self isMethodBegin: str]]] 
			whileFalse: [].
	str isNil ifTrue: [^nil].
	[stream atEnd] whileFalse: 
			[[stream atEnd or: [(self isClassBegin: str) or: [self isMethodBegin: str]]] 
				whileFalse: [str := self nextLineFromStream].
			stream atEnd 
				ifFalse: 
					[(self isMethodBegin: str) ifTrue: [self parseMethod: str] ifFalse: [self importClassDefinition: str]].
			str := self nextLineFromStream]] 
			ensure: [stream close]!

parseMethod: firstLine 
	| str token className classProxy isLoose |
	str := firstLine.
	token := str subStrings first.
	className := token copyFrom: 2 to: token size.
	classProxy := self classNamed: className asSymbol.
	(isLoose := classProxy isNil) ifTrue: [classProxy := StsClassProxy new name: className].
(	#('class' 'classSide' ) includes: (str subStrings at: 2)  )
		ifTrue: 
			[classProxy := classProxy metaClass.
			token := str subStrings at: 3]
		ifFalse: [token := str subStrings at: 2].
	(token = 'methodsFor:' or: [token = 'commentStamp:']) 
		ifTrue: 
			[token = 'methodsFor:' 
				ifTrue: 
					[self importMethodsFor: classProxy info: str.
					isLoose ifTrue: [looseMethods addAll: classProxy methods]].
			token = 'commentStamp:' ifTrue: [self importCommentFor: classProxy info: str]]
		ifFalse: [stream skipToAll: '!! !!']! !
!StsSqueakPackageProxy categoriesFor: #importClassDefinition:!private! !
!StsSqueakPackageProxy categoriesFor: #isClassBegin:!private! !
!StsSqueakPackageProxy categoriesFor: #loadFrom:!public! !
!StsSqueakPackageProxy categoriesFor: #parseMethod:!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

