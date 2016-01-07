| package |
package := Package name: 'SmallWorlds'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #Action;
	add: #Actor;
	add: #AdventureShell;
	add: #CommandParser;
	add: #Connection;
	add: #Container;
	add: #DarkLocation;
	add: #Describable;
	add: #Door;
	add: #ForwardingLocation;
	add: #ImmovableItem;
	add: #Item;
	add: #LightSource;
	add: #Location;
	add: #ReflectingLocation;
	add: #Response;
	add: #SimpleActor;
	add: #SimpleCommandParser;
	add: #TerminalLocation;
	add: #TestWorld;
	add: #Treasure;
	add: #World;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #AdventureShell -> 'AdventureShell';
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: 'MultilineTextEditExtensions';
	yourself).

package!

"Class Definitions"!

Object subclass: #Action
	instanceVariableNames: 'verb arguments'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #CommandParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Connection
	instanceVariableNames: 'destination synonyms locked key closeable closed entryProbability linkedConnection openDescription closedDescription lockedDescription soundFilenames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Describable
	instanceVariableNames: 'longDescription shortDescription shortNounPhrase longNounPhrase article longDescriptionSeen plural'
	classVariableNames: 'RandomGenerator'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Response
	instanceVariableNames: 'text'
	classVariableNames: 'RandomGenerator'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CommandParser subclass: #SimpleCommandParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Connection subclass: #Door
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Describable subclass: #Container
	instanceVariableNames: 'contents inventoryable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Container subclass: #Actor
	instanceVariableNames: 'parser location world alive sequence points'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Container subclass: #Item
	instanceVariableNames: 'lightSource lighted movable points treasure'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Container subclass: #Location
	instanceVariableNames: 'connections lighted world hasWater hasOil soundFilenames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Container subclass: #World
	instanceVariableNames: 'actor responses randomGenerator oil water soundDirectory'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Actor subclass: #SimpleActor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Item subclass: #ImmovableItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Item subclass: #LightSource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Item subclass: #Treasure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Location subclass: #DarkLocation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Location subclass: #ForwardingLocation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Location subclass: #ReflectingLocation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Location subclass: #TerminalLocation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
World subclass: #TestWorld
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #AdventureShell
	instanceVariableNames: 'inputTextPresenter displayTextPresenter'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Action guid: (GUID fromString: '{B1FE94A0-54A3-11D3-8268-00001D19F5C2}')!
Action comment: ''!
!Action categoriesForClass!No category! !
!Action methodsFor!

arguments
	arguments isNil ifTrue: [ arguments := OrderedCollection new ].
	^arguments!

arguments: aCollection
	arguments := aCollection asOrderedCollection!

verb
	"Answer the value of the receiver's instance variable verb.
	This method was automatically generated, but may be modified."

	^verb!

verb: aString		
	verb := aString! !
!Action categoriesFor: #arguments!accessing!public! !
!Action categoriesFor: #arguments:!accessing!public! !
!Action categoriesFor: #verb!accessing!public! !
!Action categoriesFor: #verb:!accessing!public! !

!Action class methodsFor!

verb: aString arguments: aString2
	^self new verb: aString; arguments: aString2! !
!Action class categoriesFor: #verb:arguments:!instance creation!public! !

CommandParser guid: (GUID fromString: '{B1FE94A1-54A3-11D3-8268-00001D19F5C2}')!
CommandParser comment: ''!
!CommandParser categoriesForClass!Adventure! !
!CommandParser methodsFor!

parse: aString withDirections: aCollection
	self subclassResponsibility! !
!CommandParser categoriesFor: #parse:withDirections:!operations!public! !

Connection guid: (GUID fromString: '{B1FE94A2-54A3-11D3-8268-00001D19F5C2}')!
Connection comment: 'Represents a connection from one location to another.  Connections are one way only - to have a bidirectional connection between locations each location must have a connection leading to the other.  Connections can optionally have a key object specified, in which case they can be locked and unlocked.  When locked the currentOpenProbability should be set to zero; when unlocked the currentOpenProbability should be reset to the baseOpenProbability value.

Sounds can be added to a Connection which will be played under the appropriate circumstances.  To add a sound to a connection the #addSound:filename: method is used to associate a sound filename with a symbol.  The standard sound symbols for the Connection class are:

	#transitNotAllowed	Played when an Actor attempts to transit a connection which
			the Actor is not allowed to transit.
	#transit		Played when an Actor successfully transits a Connection.
	#transitClosed	Played when an Actor attempts to move through a closed Connection.
	#transitLocked	Played when an Actor attempts to move through a locked Connection.
	#open		Played when an Actor opens a closed Connection.
	#close		Played when an Actor closes an open Connection.
	#lock		Played when an Actor locks a Connection.
	#unlock		Played when an Actor unlocks a Connection.'!
!Connection categoriesForClass!Adventure! !
!Connection methodsFor!

addSound: aSymbol filename: aString
	self soundFilenames add: aSymbol -> aString!

addSynonym: aString
	self removeSynonym: aString.
	self synonyms add: aString asLowercase!

addSynonyms: aCollection
	aCollection do: [ :each | self addSynonym: each ]!

basicClosed: aBoolean
	| prev |
	prev := self closed.
	closed := aBoolean.
	aBoolean ifTrue: [ self closeable: true ].
	(self closed) & (prev = false) ifTrue: [ self playSound: #close ].
	(self open) & (prev = true) ifTrue: [ self playSound: #open ]!

basicLocked: aBoolean
	| prev |
	prev := self locked.
	locked := aBoolean.
	(self locked) & (prev = false) ifTrue: [ self playSound: #lock ].
	(self unlocked) & (prev = true) ifTrue: [ self playSound: #unlock ]!

basicOpen: aBoolean
	self basicClosed: aBoolean not!

closeable
	"Answer the value of the receiver's instance variable closeable.
	This method was automatically generated, but may be modified."

	^closeable!

closeable: aBoolean
	"Set the value of the receiver's instance variable closeable to anObject.
	This method was automatically generated, but may be modified."

	closeable := aBoolean!

closed
	"Answer the value of the receiver's instance variable closed.
	This method was automatically generated, but may be modified."

	^closed!

closed: aBoolean
	"Set the value of the receiver's instance variable closed to anObject.
	This method was automatically generated, but may be modified."

	self basicClosed: aBoolean.
	self linkedConnection notNil ifTrue: [ self linkedConnection basicClosed: aBoolean ]!

closedDescription
	"Answer the value of the receiver's instance variable closedDescription.
	This method was automatically generated, but may be modified."

	^closedDescription!

closedDescription: anObject
	"Set the value of the receiver's instance variable closedDescription to anObject.
	This method was automatically generated, but may be modified."

	closedDescription := anObject!

description
	self locked ifTrue: [ ^self lockedDescription ].
	self closed ifTrue: [ ^self closedDescription ].
	^self openDescription!

destination
	"Answer the value of the receiver's instance variable destination.
	This method was automatically generated, but may be modified."

	^destination!

destination: aLocation
	"Set the value of the receiver's instance variable destination to anObject.
	This method was automatically generated, but may be modified."

	destination := aLocation!

entryProbability
	"Answer the value of the receiver's instance variable entryProbability.
	This method was automatically generated, but may be modified."

	^entryProbability!

entryProbability: aFloat
	"Set the value of the receiver's instance variable entryProbability to anObject.
	This method was automatically generated, but may be modified."

	entryProbability := aFloat!

hasSynonym: aString
	^self synonyms includes: aString asLowercase
!

initialize
	self
		locked: false;
		closeable: false;
		closed: false;
		entryProbability: 1.0;
		closedDescription: '';
		lockedDescription: '';
		openDescription: ''
		!

key
	"Answer the value of the receiver's instance variable key.
	This method was automatically generated, but may be modified."

	^key!

key: anObject
	"Set the value of the receiver's instance variable key to anObject.
	This method was automatically generated, but may be modified."

	key := anObject!

linkedConnection
	"Answer the value of the receiver's instance variable linkedConnection.
	This method was automatically generated, but may be modified."

	^linkedConnection!

linkedConnection: aConnection
	"Set the value of the receiver's instance variable linkedConnection to anObject.
	This method was automatically generated, but may be modified."

	linkedConnection := aConnection!

locked
	"Answer the value of the receiver's instance variable locked.
	This method was automatically generated, but may be modified."

	^locked!

locked: aBoolean
	self basicLocked: aBoolean.
	self linkedConnection notNil ifTrue: [ self linkedConnection basicLocked: aBoolean ]!

lockedDescription
	"Answer the value of the receiver's instance variable lockedDescription.
	This method was automatically generated, but may be modified."

	lockedDescription = ''
		ifTrue: [ ^'It''s locked' ]
		ifFalse: [ ^lockedDescription ]!

lockedDescription: anObject
	"Set the value of the receiver's instance variable lockedDescription to anObject.
	This method was automatically generated, but may be modified."

	lockedDescription := anObject!

open
	^self closed not!

open: aBoolean
	self basicOpen: aBoolean.
	self linkedConnection notNil ifTrue: [ self linkedConnection basicOpen: aBoolean ]!

openDescription
	"Answer the value of the receiver's instance variable openDescription.
	This method was automatically generated, but may be modified."

	^openDescription!

openDescription: anObject
	"Set the value of the receiver's instance variable openDescription to anObject.
	This method was automatically generated, but may be modified."

	openDescription := anObject!

playSound: aSymbol
	| soundFile |
	(soundFile := self soundNamed: aSymbol) notNil ifTrue: [ (Sound fromFile: soundFile) woof ]!

removeAllSynonyms
	synonyms := nil!

removeSynonym: aString
	self synonyms remove: aString asLowercase ifAbsent: [ ]!

soundFilenames
	soundFilenames isNil ifTrue: [ soundFilenames := Dictionary new ].
	^soundFilenames!

soundNamed: aSymbol
	| filename |
	filename := self soundFilenames at: aSymbol ifAbsent: [ nil ].
	filename notNil ifTrue: [ filename := self destination world soundDirectory, '\', filename ].
	^filename!

synonyms
	synonyms isNil ifTrue: [ synonyms := OrderedCollection new ].
	^synonyms!

synonyms: anArray
	anArray do: [ :each | self addSynonym: each ]!

transport: anActor
	self locked ifTrue: [ self playSound: #transitLocked. ^self lockedDescription ].
	self closed ifTrue: [ self playSound: #transitClosed. ^self closedDescription ].
	(anActor location allowTravelBy: anActor through: self) ifFalse: [
		self playSound: #transitNotAllowed.
		^anActor location travelFailureReasonFor: anActor through: self ].

	"Success!!"

	self playSound: #transit.
	^self destination receiveActor: anActor
!

unlocked
	^self locked not! !
!Connection categoriesFor: #addSound:filename:!public!sounds! !
!Connection categoriesFor: #addSynonym:!adding!public! !
!Connection categoriesFor: #addSynonyms:!adding!public! !
!Connection categoriesFor: #basicClosed:!operations!public! !
!Connection categoriesFor: #basicLocked:!operations!public! !
!Connection categoriesFor: #basicOpen:!operations!public! !
!Connection categoriesFor: #closeable!accessing!public! !
!Connection categoriesFor: #closeable:!accessing!public! !
!Connection categoriesFor: #closed!accessing!public! !
!Connection categoriesFor: #closed:!accessing!public! !
!Connection categoriesFor: #closedDescription!accessing!public! !
!Connection categoriesFor: #closedDescription:!accessing!public! !
!Connection categoriesFor: #description!accessing!public! !
!Connection categoriesFor: #destination!accessing!public! !
!Connection categoriesFor: #destination:!accessing!public! !
!Connection categoriesFor: #entryProbability!accessing!public! !
!Connection categoriesFor: #entryProbability:!accessing!public! !
!Connection categoriesFor: #hasSynonym:!accessing!public! !
!Connection categoriesFor: #initialize!initialization!public! !
!Connection categoriesFor: #key!accessing!public! !
!Connection categoriesFor: #key:!accessing!public! !
!Connection categoriesFor: #linkedConnection!accessing!public! !
!Connection categoriesFor: #linkedConnection:!accessing!public! !
!Connection categoriesFor: #locked!accessing!public! !
!Connection categoriesFor: #locked:!accessing!public! !
!Connection categoriesFor: #lockedDescription!accessing!public! !
!Connection categoriesFor: #lockedDescription:!accessing!public! !
!Connection categoriesFor: #open!accessing!public! !
!Connection categoriesFor: #open:!accessing!public! !
!Connection categoriesFor: #openDescription!accessing!public! !
!Connection categoriesFor: #openDescription:!accessing!public! !
!Connection categoriesFor: #playSound:!public!sounds! !
!Connection categoriesFor: #removeAllSynonyms!operations!public! !
!Connection categoriesFor: #removeSynonym:!operations!public! !
!Connection categoriesFor: #soundFilenames!accessing!public!sounds! !
!Connection categoriesFor: #soundNamed:!public!sounds! !
!Connection categoriesFor: #synonyms!accessing!public! !
!Connection categoriesFor: #synonyms:!accessing!public! !
!Connection categoriesFor: #transport:!operations!public! !
!Connection categoriesFor: #unlocked!accessing!public! !

!Connection class methodsFor!

new
	^super new initialize! !
!Connection class categoriesFor: #new!instance creation!public! !

Describable guid: (GUID fromString: '{B1FE94A3-54A3-11D3-8268-00001D19F5C2}')!
Describable comment: 'Something which has a description.'!
!Describable categoriesForClass!Adventure! !
!Describable methodsFor!

article
	article isNil
		ifTrue: [ ^self generateArticle ]
		ifFalse: [ ^article ]
!

article: anObject
	"Set the value of the receiver's instance variable article to anObject.
	This method was automatically generated, but may be modified."

	article := anObject
!

basicDescription
	"Answers the long description if it has not been seen before; otherwise answers
	the short description"

	self longDescriptionSeen
		ifTrue: [ ^self shortDescription ]
		ifFalse: [ self longDescriptionSeen: true.
			     ^self longDescription ]!

basicLongDescription
	^longDescription!

basicLongNounPhrase
	^longNounPhrase!

basicShortDescription
	^shortDescription!

basicShortNounPhrase
	^shortNounPhrase!

description
	^self basicDescription
!

generateArticle
	"Private - Private"
	self longDescriptionSeen
		ifFalse: [ (longNounPhrase at: 1) isVowel ifTrue: [ ^'an' ] ifFalse: [ ^'a' ] ]
		ifTrue: [ (shortNounPhrase at: 1) isVowel ifTrue: [ ^'an' ] ifFalse: [ ^'a' ] ]!

initialize
	self
		longDescriptionSeen: false;
		plural: false!

inventoryDescription2ndPerson
	^'have ', self article, ' ', self nounPhrase!

inventoryDescription3dPerson
	self plural
		ifTrue: [ ^'are ', self article, ' ', self nounPhrase ]
		ifFalse: [ ^'is ', self article, ' ', self nounPhrase ]!

longDescription
	self longNounPhrase notNil
		ifTrue: [ ^(self article size > 0 ifTrue: [ self article, ' '] ifFalse: [ '' ]), self longNounPhrase ]
		ifFalse: [ ^longDescription ]!

longDescription: anObject
	"Set the value of the receiver's instance variable longDescription to anObject.
	This method was automatically generated, but may be modified."

	longDescription := anObject.
	self basicShortDescription isNil ifTrue: [ self shortDescription: anObject ]!

longDescriptionSeen
	"Answer the value of the receiver's instance variable longDescriptionSeen.
	This method was automatically generated, but may be modified."

	^longDescriptionSeen!

longDescriptionSeen: aBoolean
	"Set the value of the receiver's instance variable longDescriptionSeen to anObject.
	This method was automatically generated, but may be modified."

	longDescriptionSeen := aBoolean!

longNounPhrase
	"Answer the value of the receiver's instance variable longNounPhrase.
	This method was automatically generated, but may be modified."

	^longNounPhrase!

longNounPhrase: anObject
	"Set the value of the receiver's instance variable longNounPhrase to anObject.
	This method was automatically generated, but may be modified."

	longNounPhrase := anObject.
	self basicShortNounPhrase isNil ifTrue: [ self shortNounPhrase: anObject ]!

nounPhrase
	self longDescriptionSeen
		ifTrue: [ ^self shortNounPhrase ]
		ifFalse: [ self longDescriptionSeen: true. ^self longNounPhrase ]!

plural
	"Answer the value of the receiver's instance variable plural.
	This method was automatically generated, but may be modified."

	^plural!

plural: anObject
	"Set the value of the receiver's instance variable plural to anObject.
	This method was automatically generated, but may be modified."

	plural := anObject!

points
	^0!

randomGenerator
	^self class randomGenerator!

shortDescription
	self shortNounPhrase notNil
		ifTrue: [ ^self article, ' ', self shortNounPhrase ]
		ifFalse: [ ^shortDescription ]!

shortDescription: anObject
	"Set the value of the receiver's instance variable shortDescription to anObject.
	This method was automatically generated, but may be modified."

	shortDescription := anObject.
	self basicLongDescription isNil ifTrue: [ self longDescription: anObject ]!

shortNounPhrase
	"Answer the value of the receiver's instance variable shortNounPhrase.
	This method was automatically generated, but may be modified."

	^shortNounPhrase!

shortNounPhrase: anObject
	"Set the value of the receiver's instance variable shortNounPhrase to anObject.
	This method was automatically generated, but may be modified."

	shortNounPhrase := anObject.
	self basicLongNounPhrase isNil ifTrue: [ self longNounPhrase: anObject ]! !
!Describable categoriesFor: #article!accessing!public! !
!Describable categoriesFor: #article:!accessing!public! !
!Describable categoriesFor: #basicDescription!accessing!public! !
!Describable categoriesFor: #basicLongDescription!accessing!public! !
!Describable categoriesFor: #basicLongNounPhrase!accessing!public! !
!Describable categoriesFor: #basicShortDescription!accessing!public! !
!Describable categoriesFor: #basicShortNounPhrase!accessing!public! !
!Describable categoriesFor: #description!accessing!public! !
!Describable categoriesFor: #generateArticle!helpers!private! !
!Describable categoriesFor: #initialize!initialization!public! !
!Describable categoriesFor: #inventoryDescription2ndPerson!accessing!public! !
!Describable categoriesFor: #inventoryDescription3dPerson!accessing!public! !
!Describable categoriesFor: #longDescription!accessing!public! !
!Describable categoriesFor: #longDescription:!accessing!public! !
!Describable categoriesFor: #longDescriptionSeen!accessing!public! !
!Describable categoriesFor: #longDescriptionSeen:!accessing!public! !
!Describable categoriesFor: #longNounPhrase!accessing!public! !
!Describable categoriesFor: #longNounPhrase:!accessing!public! !
!Describable categoriesFor: #nounPhrase!accessing!public! !
!Describable categoriesFor: #plural!accessing!public! !
!Describable categoriesFor: #plural:!accessing!public! !
!Describable categoriesFor: #points!accessing!public! !
!Describable categoriesFor: #randomGenerator!accessing!public! !
!Describable categoriesFor: #shortDescription!accessing!public! !
!Describable categoriesFor: #shortDescription:!accessing!public! !
!Describable categoriesFor: #shortNounPhrase!accessing!public! !
!Describable categoriesFor: #shortNounPhrase:!accessing!public! !

!Describable class methodsFor!

chooseFrom: anArrayedCollection
	"Choose a response at random from a collection of possible text strings"

	| randomValue randomNumber |

	randomValue := self randomGenerator next.
	randomNumber := (randomValue * anArrayedCollection size) truncated + 1.
	^anArrayedCollection at: randomNumber!

new
	^super new initialize!

randomGenerator
	RandomGenerator isNil ifTrue: [ RandomGenerator := Random new ].
	^RandomGenerator!

referencesToOtherPackages
	MultilineTextEditExtensions! !
!Describable class categoriesFor: #chooseFrom:!accessing!public! !
!Describable class categoriesFor: #new!instance creation!public! !
!Describable class categoriesFor: #randomGenerator!accessing!public! !
!Describable class categoriesFor: #referencesToOtherPackages!Dependencies!public! !

Response guid: (GUID fromString: '{B1FE94A4-54A3-11D3-8268-00001D19F5C2}')!
Response comment: ''!
!Response categoriesForClass!Adventure! !
!Response methodsFor!

text
	"Answer the value of the receiver's instance variable text.
	This method was automatically generated, but may be modified."

	^text!

text: anObject
	"Set the value of the receiver's instance variable text to anObject.
	This method was automatically generated, but may be modified."

	text := anObject! !
!Response categoriesFor: #text!accessing!public! !
!Response categoriesFor: #text:!accessing!public! !

!Response class methodsFor!

chooseFrom: anArrayedCollection
	"Choose a response at random from a collection of possible text strings"

	| randomIndex |

	randomIndex := (self randomGenerator next * anArrayedCollection size) truncated + 1.
	^Response text: (anArrayedCollection at: randomIndex)!

randomGenerator
	RandomGenerator isNil ifTrue: [ RandomGenerator := Random new ].
	^RandomGenerator!

standardChoicesPlus: anArrayedCollection
	^self chooseFrom: anArrayedCollection, #('Could you be a bit more specific?' 'Huh?'
									'What we have here is a failure to communicate.')!

text: aString
	^self new text: aString! !
!Response class categoriesFor: #chooseFrom:!helpers!public! !
!Response class categoriesFor: #randomGenerator!accessing!public! !
!Response class categoriesFor: #standardChoicesPlus:!accessing!public! !
!Response class categoriesFor: #text:!instance creation!public! !

SimpleCommandParser guid: (GUID fromString: '{B1FE94A5-54A3-11D3-8268-00001D19F5C2}')!
SimpleCommandParser comment: ''!
!SimpleCommandParser categoriesForClass!Adventure! !
!SimpleCommandParser methodsFor!

normalizeVerb: aString
	"Normalize a verb into its basic form.  If no translation exists
	answer aString"

	^self verbNormalizationDictionary at: aString ifAbsent: [ aString ]!

parse: aString withDirections: aCollection
	| subStrings validDirections validVerbs actions translatedVerb |

	validDirections := aCollection.
	validVerbs := self class verbs.
	actions := OrderedCollection new.

	subStrings := aString asLowercase subStrings.

	subStrings size > 0 ifTrue: [ translatedVerb := self normalizeVerb: (subStrings at: 1) ].

	"Parse single-word phrases"

	subStrings size = 1 ifTrue: [
		(validDirections includes: translatedVerb)
			ifTrue: [ ^self parse: 'go ', translatedVerb withDirections: validDirections ]
			ifFalse: [ 
				(validVerbs includes: translatedVerb)
					ifTrue: [ actions add: (Action
										verb: translatedVerb
										arguments: (OrderedCollection new)) ]
					ifFalse: [ actions add: (Action
								verb: 'respond'
								arguments: (OrderedCollection with: 'There''s no way to go that way.')) ] ] ].

	"Parse verb-noun phrases."

	subStrings size = 2 ifTrue: [
		(validVerbs includes: translatedVerb)
			ifTrue: [ actions add: (Action
								verb: translatedVerb
								arguments: (OrderedCollection with: (subStrings at: 2))) ]
			ifFalse: [ actions add: (Action
								verb: 'respond'
								arguments: (OrderedCollection with: 'I don''t know how to ' with: aString)) ] ].

	"Complain if we get a more complex phrase."

	subStrings size > 2 ifTrue: [ actions add: (Action
									verb: 'respond'
									arguments: (OrderedCollection with: 'I don''t understand ' with: aString)) ].

	^actions!

verbNormalizationDictionary
	"Private - create a Dictionary used to translate common verb
	 synonyms to verbs this parser understands.  At some point it
	 might prove useful to cache this in a variable."

	^Dictionary new
		add: 'onward' -> 'forward';
		add: 'back' -> 'backward';
		add: 'retreat' -> 'backward';
		add: 'return' -> 'backward';
		add: 'leave' -> 'exit';
		add: 'out' -> 'exit';
		add: 'outside' -> 'exit';
		add: 'inside' -> 'in';
		add: 'inward' -> 'in';
		add: 'above' -> 'up';
		add: 'ascend' -> 'up';
		add: 'u' -> 'up';
		add: 'upward' -> 'up';
		add: 'd' -> 'down';
		add: 'descend' -> 'down';
		add: 'downward' -> 'down';
		add: 'describe' -> 'look';
		add: 'examine' -> 'look';
		add: 'touch' -> 'look';
		add: 'i' -> 'inventory';
		add: 'e' -> 'east';
		add: 'w' -> 'west';
		add: 'n' -> 'north';
		add: 's' -> 'south';
		add: 'ne' -> 'northeast';
		add: 'se' -> 'southeast';
		add: 'sw' -> 'southwest';
		add: 'nw' -> 'northwest';
		add: 'take' -> 'get';
		add: 'leave' -> 'drop';
		add: 'dump' -> 'drop';
		add: 'lose' -> 'drop';
		add: 'journey' -> 'go';
		add: 'speak' -> 'say';
		add: 'talk' -> 'say';
		yourself! !
!SimpleCommandParser categoriesFor: #normalizeVerb:!accessing!public! !
!SimpleCommandParser categoriesFor: #parse:withDirections:!operations!public! !
!SimpleCommandParser categoriesFor: #verbNormalizationDictionary!accessing!public! !

!SimpleCommandParser class methodsFor!

verbs
	^#('go' 'look' 'get' 'inventory' 'drop' 'say' 'unlock' 'open' 'light' 'wave' 'fill' 'empty' 'help' 'close' 'score')! !
!SimpleCommandParser class categoriesFor: #verbs!constants!public! !

Door guid: (GUID fromString: '{F59B3B9C-5C16-408A-838F-CC9323086852}')!
Door comment: ''!
!Door categoriesForClass!Unclassified! !
!Door methodsFor!

initialize
	super initialize.
	self addSound: #transit filename: 'creaky_door_opening.wav'! !
!Door categoriesFor: #initialize!initialization!public! !

Container guid: (GUID fromString: '{B1FE94A6-54A3-11D3-8268-00001D19F5C2}')!
Container comment: 'Something which can contain other things.'!
!Container categoriesForClass!Adventure! !
!Container methodsFor!

add: anObject
	"Add a new object to the collection of objects in this container"

	self remove: anObject.	"Make sure it's not already in this container"
	self contents add: anObject.
	^Response text: 'OK'!

basicLighted
	^false!

contains: anObject
	^self contents includes: anObject!

containsLightSource
	^self basicLighted | (self contents inject: false into: [ :tot :each | tot | each containsLightSource ])!

contents
	"Answer the value of the receiver's instance variable contents.
	This method was automatically generated, but may be modified."

	^contents!

contentsDescription
	^''!

contentsLike: aString
	| pattern |

	pattern := '*', aString, '*'.
	^self contents select: [ :each |
		(pattern match: each shortDescription) | (pattern match: each longDescription) ]!

initialize
	super initialize.
	contents := OrderedCollection new.
	self inventoryable: true!

inventory
	"Answer a string describing the contents of this container"

	^self subclassResponsibility!

inventory2ndPersonWithPrefix: aPrefixString suffix: aSuffixString
	"Private - answer this Container's inventory using the given prefix and suffix"
	self contents size > 0
		ifTrue: [
			^self contents inject: '' into: [ :string :each |
				each inventoryable
					ifTrue: [
						(each basicLongDescription isNil
							ifTrue: [
								string, (string size > 0 ifTrue: [ String lineDelimiter ] ifFalse: ['']),
								aPrefixString, ' ', each inventoryDescription2ndPerson,
								(aSuffixString size > 0 ifTrue: [ ' ', aSuffixString ] ifFalse: [ '' ]) ]
							ifFalse: [ string, each basicDescription ]),
						(each contents size > 0
							ifTrue: [ '.  The ', each shortNounPhrase, ' contains:',
													String lineDelimiter,
													(each inventoryWithPrefix: '   ' suffix: '') ]
							ifFalse: [ '' ]) ]
					ifFalse: [ string ] ] ]
		ifFalse: [ ^self class chooseFrom: (Array with: (aPrefixString, ' don''t seem to be carrying anything.')
									       with: (aPrefixString, ' are empty-handed.')
									       with: (aPrefixString, ' seem to be a bit light in the loot department.')
									       with: (aPrefixString, ' rummage around but find nothing.')) ]!

inventory3dPersonWithPrefix: aPrefixString suffix: aSuffixString
	"Private - answer this Container's inventory using the given prefix and suffix"

	^self contents inject: '' into: [ :string :each |
		each inventoryable
			ifTrue: [
				(each basicLongDescription isNil
					ifTrue: [
						string, (string size > 0 ifTrue: [ String lineDelimiter ] ifFalse: ['']),
						aPrefixString, ' ', each inventoryDescription3dPerson,
						(aSuffixString size > 0 ifTrue: [ ' ', aSuffixString ] ifFalse: [ '' ]),
						(each treasure ifTrue: [ '!!' ] ifFalse: [ '' ]) ]
					ifFalse: [ string, each basicDescription ]),
				(each contents size > 0
					ifTrue: [ '.  The ', each shortNounPhrase, ' contains:',
											String lineDelimiter,
											(each inventoryWithPrefix: '   ' suffix: '') ]
					ifFalse: [ '' ]) ]
			ifFalse: [ string ] ]!

inventoryable
	"Answer the value of the receiver's instance variable inventoryable.
	This method was automatically generated, but may be modified."

	^inventoryable!

inventoryable: anObject
	"Set the value of the receiver's instance variable inventoryable to anObject.
	This method was automatically generated, but may be modified."

	inventoryable := anObject!

inventoryStringWithPrefix: aPrefixString suffix: aSuffixString
	| description contentsDescription |

	self inventoryable
		ifTrue: [
			self basicLongDescription size > 0
				ifTrue: [ description := self basicDescription ]
				ifFalse: [ description := aPrefixString, ' ', self description,
					      ((aSuffixString size > 0) ifTrue: [ ' ' ] ifFalse: [ '' ]), aSuffixString, '.' ].

			self contents size > 0
				ifTrue: [ contentsDescription := 'The ', self shortNounPhrase, ' contains:',
								String lineDelimiter,
								(self inventoryWithPrefix: '   ' suffix: '') ]
				ifFalse: [ contentsDescription := '' ] ]
		ifFalse: [ description := ''.  contentsDescription := ''. ].

	^description, contentsDescription!

inventoryWithPrefix: aPrefixString suffix: aSuffixString
	"Private - answer this Container's inventory using the given prefix and suffix"
	^self contents inject: '' into: [ :string :each |
		string, (string size > 0 ifTrue: [ String lineDelimiter ] ifFalse: ['']),
			(each inventoryStringWithPrefix: aPrefixString suffix: aSuffixString) ]!

remove: anObject
	^self contents remove: anObject ifAbsent: [ ]! !
!Container categoriesFor: #add:!adding!public! !
!Container categoriesFor: #basicLighted!accessing!public! !
!Container categoriesFor: #contains:!public!testing! !
!Container categoriesFor: #containsLightSource!public!testing! !
!Container categoriesFor: #contents!accessing!public! !
!Container categoriesFor: #contentsDescription!accessing!public! !
!Container categoriesFor: #contentsLike:!accessing!public! !
!Container categoriesFor: #initialize!initialization!public! !
!Container categoriesFor: #inventory!public!verb processing! !
!Container categoriesFor: #inventory2ndPersonWithPrefix:suffix:!public!verb processing! !
!Container categoriesFor: #inventory3dPersonWithPrefix:suffix:!public!verb processing! !
!Container categoriesFor: #inventoryable!accessing!public! !
!Container categoriesFor: #inventoryable:!accessing!public! !
!Container categoriesFor: #inventoryStringWithPrefix:suffix:!operations!public! !
!Container categoriesFor: #inventoryWithPrefix:suffix:!helpers!private! !
!Container categoriesFor: #remove:!public!removing! !

Actor guid: (GUID fromString: '{B1FE94A7-54A3-11D3-8268-00001D19F5C2}')!
Actor comment: 'An object which is capable of processing commands.'!
!Actor categoriesForClass!Adventure! !
!Actor methodsFor!

accumulatePoints: anItem
	self points: self points + anItem points.
	anItem points: 0!

alive
	"Answer the value of the receiver's instance variable alive.
	This method was automatically generated, but may be modified."

	^alive!

alive: anObject
	"Set the value of the receiver's instance variable alive to anObject.
	This method was automatically generated, but may be modified."

	alive := anObject!

close: aCollection
	| connections |

	connections := self location connectionsTo: (aCollection at: 1).
	connections size = 0 ifTrue: [ ^Response standardChoicesPlus: #('What are you trying to open?')].

	connections size = 1 ifTrue: [
		(connections at: 1) closeable ifFalse: [ ^Response standardChoicesPlus: #('There''s no way to close that.')].
		(connections at: 1) closed: true.
		^Response text: 'OK' ].

	"connections size > 1"

	^Response standardChoicesPlus: #('I''m lost.  Narrow it down a bit and try again.')!

drop: anOrderedCollection
	| matchingObjects item |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Drop *WHAT*?!!?')].

	matchingObjects := self contentsLike: (anOrderedCollection at: 1).

	matchingObjects size = 0
		ifTrue: [ 
			self contents do: [ :each | 
				matchingObjects := each contentsLike: (anOrderedCollection at: 1).
				matchingObjects size = 1 ifTrue: [
					item := matchingObjects at: 1.
					each remove: item.
					self add: item.
					^self drop: anOrderedCollection ] ].
			^Response text: 'You don''t have a ', (anOrderedCollection at: 1) ].

	matchingObjects size = 1
		ifTrue: [
			self remove: (matchingObjects at: 1).
			^self location add: (matchingObjects at: 1) ].

	^Response text: 'Could you be a bit more specific?'!

empty: anOrderedCollection
	| matchingObjects response |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Empty *WHAT*?!!?')].

	matchingObjects := self contentsLike: (anOrderedCollection at: 1).
	matchingObjects size = 0
		ifTrue: [ ^Response text: 'I don''t see a ', (anOrderedCollection at: 1), ' here.' ].
	matchingObjects size > 1
		ifTrue: [ ^Response text: 'Could you be a bit more specific?' ].

	response := Response text: 'OK'.

	(matchingObjects at: 1) contents do: [ :each |
		each ~= self world water & (each ~= self world oil)
			ifTrue: [ self location add: each ]
			ifFalse: [ response := Response text: 'The ', each shortNounPhrase, ' splashes on the floor and evaporates.']].

	(matchingObjects at: 1) contents removeAll: (matchingObjects at: 1) contents.
	^response!

execute: anAction
	"Execute the given action, answering an appropriate Response"

	^self perform: (anAction verb, ':') asSymbol with: anAction arguments!

fill: anOrderedCollection
	| matchingObjects |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Fill *WHAT*?!!?')].

	matchingObjects := self contentsLike: (anOrderedCollection at: 1).
	matchingObjects size = 0 ifTrue: [ ^Response text: 'I don''t see a ', (anOrderedCollection at: 1), ' here.' ].
	matchingObjects size > 1 ifTrue: [ ^Response text: 'Could you be a bit more specific?' ].

	self location hasWater
		ifTrue: [ (matchingObjects at: 1) add: self world water.
				^Response text: 'OK' ].

	self location hasOil
		ifTrue: [ (matchingObjects at: 1) add: self world oil.
				^Response text: 'OK' ].

	^Response text: 'I don''t see anything to fill the ', (anOrderedCollection at: 1), ' with.'!

get: anOrderedCollection
	| matchingObjects |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Get *WHAT*?!!?')].

	(anOrderedCollection at: 1) = 'all'
		ifTrue: [
			matchingObjects := self location contents copy.
			matchingObjects remove: self ifAbsent: [ ] ]
		ifFalse: [
			matchingObjects := self location contentsLike: (anOrderedCollection at: 1).
			matchingObjects size = 0
				ifTrue: [ ^Response text: 'I don''t see a ', (anOrderedCollection at: 1), ' here.' ].
			matchingObjects size > 1
				ifTrue: [ ^Response text: 'Could you be a bit more specific?' ] ].

	matchingObjects do: [ :each |
		each  movable
			ifTrue: [
				self location remove: each.
				self add: each ]
			ifFalse: [ ^Response text: 'You can''t seem to pick up the ', (matchingObjects at: 1) shortNounPhrase ] ].

	^Response text: 'OK'!

go: anOrderedCollection
	| connections responseText |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Go *WHERE* (or should that be ''whither'')?!!?')].

	connections := self location connectionsTo: (anOrderedCollection at: 1).
	connections size > 1 ifTrue: [ ^Response text: 'Could you be a bit more specific?' ].
	connections size = 1 ifTrue: [ ^Response text: ((connections at: 1) transport: self) ].

	"Couldn't find a way to go in the desired direction, so we need to determine an
	  appropriate response."

	^Response chooseFrom: #('There''s no exit in that direction'
						'You can''t go that way'
						'You might be able to do that if you had a pickaxe'
						'Huh?')!

help: anOrderedCollection
	^Response text: self world helpString!

initialize
	super initialize.
	self
		alive: true;
		inventoryable: false;
		sequence: 0;
		points: 0!

inventory
	^self inventory2ndPersonWithPrefix: 'You' suffix: ''!

inventory: aCollection
	^Response text: self inventory!

inventoryStringWithPrefix: aPrefixString suffix: aSuffixString
	^''!

light: aCollection
	| items wasLit |

	aCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Light *WHAT*?!!?')].

	wasLit := self location lighted.
	items := self contentsLike: (aCollection at: 1).
	items size > 0
		ifTrue: [ (items at: 1) lightSource
				ifTrue: [ (items at: 1) lighted: true.
						wasLit
							ifTrue: [ ^Response text: 'OK' ]
							ifFalse: [ ^Response text: 'Ahh, much better...', String lineDelimiter, self location description ] ]
				ifFalse: [ ^Response chooseFrom: (Array with:'Nothing happens'
										   with: 'You can''t find the switch'
										   with: 'Huh?'
										   with: 'I''m game.  How?'
										   with: 'Let me guess - half the calories of our regular ',
												(items at: 1) nounPhrase, ' and all the great taste?') ] ]
		ifFalse: [ ^Response text: 'You don''t seem to have a ', (aCollection at: 1) ]!

location
	"Answer the value of the receiver's instance variable location.
	This method was automatically generated, but may be modified."

	^location!

location: aLocation
	location := aLocation!

look: anOrderedCollection
	"Answer a description of the current location"

	self location longDescriptionSeen: false.
	^Response text: self location description, String lineDelimiter, self location inventory!

open: aCollection
	| connections |

	aCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Open *WHAT*?!!?')].

	connections := self location connectionsTo: (aCollection at: 1).
	connections size = 0 ifTrue: [ ^Response text: 'What are you trying to open?' ].

	connections size = 1 ifTrue: [
		(connections at: 1) locked ifTrue: [ ^Response text: 'It seems to be locked' ].

		(connections at: 1) open ifTrue: [ ^Response text: 'It''s already open' ].

		(connections at: 1) open: true.
		^Response text: 'OK' ].

	"connections size > 1"

	^Response text: 'Could you be a bit more specific?'!

parser
	"Answer the value of the receiver's instance variable parser.
	This method was automatically generated, but may be modified."

	parser isNil ifTrue: [ parser := self class defaultParserClass new ].
	^parser!

points
	"Answer the value of the receiver's instance variable points.
	This method was automatically generated, but may be modified."

	^points!

points: anObject
	"Set the value of the receiver's instance variable points to anObject.
	This method was automatically generated, but may be modified."

	points := anObject!

processCommand: aString
	"Process the given command, answering a collection of Responses"

	| actions |

	self sequence: self sequence + 1.
	actions := self parser parse: aString withDirections: self location allDirections.
	^actions collect: [ :anAction | self execute: anAction ]!

respond: anOrderedCollection
	^Response text: (anOrderedCollection inject: '' into: [ :string :each | string := string, each ])!

say: anOrderedCollection
	^Response text: '''', (anOrderedCollection inject: '' into: [ :tot :each | tot, each ]), ''''!

score: anOrderedCollection
	^Response text: 'Current score: ', self points printString!

sequence
	"Answer the value of the receiver's instance variable sequence.
	This method was automatically generated, but may be modified."

	^sequence!

sequence: anObject
	"Set the value of the receiver's instance variable sequence to anObject.
	This method was automatically generated, but may be modified."

	sequence := anObject!

unlock: aCollection
	| connections |

	aCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Unlock *WHAT*?!!?')].

	connections := self location connectionsTo: (aCollection at: 1).
	connections size = 0 ifTrue: [ ^Response text: 'What are you trying to unlock?' ].

	connections size = 1 ifTrue: [
		(connections at: 1) locked
			ifTrue: [
				(self contains: (connections at: 1) key)
					ifTrue: [
						(connections at: 1) locked: false.
						^Response text: (connections at: 1) description, String lineDelimiter, 'OK' ]
					ifFalse: [ ^Response text: 'You don''t seem to have the key' ] ]
			ifFalse: [ ^Response text: 'I don''t think it''s locked' ] ].

	"connections size > 1"

	^Response text: 'Could you be a bit more specific?'!

wave: aCollection
	| matchingObjects |

	aCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Hello.' 'Wave *WHAT*?!!?')].

	matchingObjects := self contentsLike: (aCollection at: 1).
	matchingObjects size = 0 ifTrue: [ ^Response text: 'Huh?' ].
	matchingObjects size > 1 ifTrue: [ ^Response text: 'Could you be a bit more specific?' ].

	^Response chooseFrom: #('Swoosh!!'
						'Hey, that was neat!!  Do it again!!'
						'Nothing happens'
						'Is that nose on the floor yours?')!

world
	"Answer the value of the receiver's instance variable world.
	This method was automatically generated, but may be modified."

	^world!

world: anObject
	"Set the value of the receiver's instance variable world to anObject.
	This method was automatically generated, but may be modified."

	world := anObject! !
!Actor categoriesFor: #accumulatePoints:!operations!public! !
!Actor categoriesFor: #alive!accessing!public! !
!Actor categoriesFor: #alive:!accessing!public! !
!Actor categoriesFor: #close:!public!verb processing! !
!Actor categoriesFor: #drop:!public!verb processing! !
!Actor categoriesFor: #empty:!public!verb processing! !
!Actor categoriesFor: #execute:!public!verb processing! !
!Actor categoriesFor: #fill:!public!verb processing! !
!Actor categoriesFor: #get:!public!verb processing! !
!Actor categoriesFor: #go:!public!verb processing! !
!Actor categoriesFor: #help:!public!verb processing! !
!Actor categoriesFor: #initialize!initializing!public! !
!Actor categoriesFor: #inventory!public!verb processing! !
!Actor categoriesFor: #inventory:!public!verb processing! !
!Actor categoriesFor: #inventoryStringWithPrefix:suffix:!operations!public! !
!Actor categoriesFor: #light:!public!verb processing! !
!Actor categoriesFor: #location!accessing!public! !
!Actor categoriesFor: #location:!accessing!public! !
!Actor categoriesFor: #look:!public!verb processing! !
!Actor categoriesFor: #open:!public!verb processing! !
!Actor categoriesFor: #parser!accessing!public! !
!Actor categoriesFor: #points!accessing!public! !
!Actor categoriesFor: #points:!accessing!public! !
!Actor categoriesFor: #processCommand:!command processing!public! !
!Actor categoriesFor: #respond:!operations!public! !
!Actor categoriesFor: #say:!public!verb processing! !
!Actor categoriesFor: #score:!public!verb processing! !
!Actor categoriesFor: #sequence!accessing!public! !
!Actor categoriesFor: #sequence:!accessing!public! !
!Actor categoriesFor: #unlock:!public!verb processing! !
!Actor categoriesFor: #wave:!public!verb processing! !
!Actor categoriesFor: #world!accessing!public! !
!Actor categoriesFor: #world:!accessing!public! !

!Actor class methodsFor!

defaultParserClass
	self subclassResponsibility! !
!Actor class categoriesFor: #defaultParserClass!accessing!public! !

Item guid: (GUID fromString: '{B1FE94A8-54A3-11D3-8268-00001D19F5C2}')!
Item comment: 'A ''thing'' in the database which is not an actor (e.g. is passive, not active), e.g. lamp, key, rod, etc.

Items should be capable of responding to simple commands, e.g.
    wave rod
when parsed and processed by an Actor might result in
    self perform: (Message selector: #wave: argument: ''rod'')

The Actor handler for #wave might be something like

    wave: aString
        | items |
        items := self findItemsLike: aString.
        items size = 0 ifTrue: [ AdvNoItemFoundException signalWith: aString ].
        items size > 1 ifTrue: [ AdvMultipleItemsFoundException signalWith: aString ]

        ^(items at: 1) performAction: #wave

Thus, items need to have a Collection of things they can do, which may consist of a Dictionary of BlockClosures since we''ll need to change this on an instance-by-instance basis.'!
!Item categoriesForClass!Adventure! !
!Item methodsFor!

basicLighted
	"Answer the value of the receiver's instance variable lighted.
	This method was automatically generated, but may be modified."

	^lighted!

initialize
	super initialize.
	self
		lightSource: false;
		lighted: false;
		movable: true;
		points: 0;
		treasure: false!

lighted
	"Answer the value of the receiver's instance variable lighted.
	This method was automatically generated, but may be modified."

	^self basicLighted!

lighted: aBoolean
	"Set the value of the receiver's instance variable lighted to anObject.
	This method was automatically generated, but may be modified."

	lighted := aBoolean!

lightSource
	"Answer the value of the receiver's instance variable lightSource.
	This method was automatically generated, but may be modified."

	^lightSource!

lightSource: aBoolean
	"Set the value of the receiver's instance variable lightSource to anObject.
	This method was automatically generated, but may be modified."

	lightSource := aBoolean!

movable
	"Answer the value of the receiver's instance variable movable.
	This method was automatically generated, but may be modified."

	^movable!

movable: anObject
	"Set the value of the receiver's instance variable movable to anObject.
	This method was automatically generated, but may be modified."

	movable := anObject!

points
	"Answer the value of the receiver's instance variable points.
	This method was automatically generated, but may be modified."

	^points!

points: anObject
	"Set the value of the receiver's instance variable points to anObject.
	This method was automatically generated, but may be modified."

	points := anObject!

treasure
	"Answer the value of the receiver's instance variable treasure.
	This method was automatically generated, but may be modified."

	^treasure!

treasure: anObject
	"Set the value of the receiver's instance variable treasure to anObject.
	This method was automatically generated, but may be modified."

	treasure := anObject! !
!Item categoriesFor: #basicLighted!accessing!public! !
!Item categoriesFor: #initialize!initialization!public! !
!Item categoriesFor: #lighted!accessing!public! !
!Item categoriesFor: #lighted:!accessing!public! !
!Item categoriesFor: #lightSource!accessing!public! !
!Item categoriesFor: #lightSource:!accessing!public! !
!Item categoriesFor: #movable!accessing!public! !
!Item categoriesFor: #movable:!accessing!public! !
!Item categoriesFor: #points!accessing!public! !
!Item categoriesFor: #points:!accessing!public! !
!Item categoriesFor: #treasure!accessing!public! !
!Item categoriesFor: #treasure:!accessing!public! !

Location guid: (GUID fromString: '{B1FE94A9-54A3-11D3-8268-00001D19F5C2}')!
Location comment: ''!
!Location categoriesForClass!Adventure! !
!Location methodsFor!

addConnection: aConnection
	self connections add: aConnection!

allDirections
	"Answers a collection of all directions which are valid in this location"

	| allDirections |

	allDirections := OrderedCollection new.

	self connections do: [ :each |
		each synonyms do: [ :each2 |
			(allDirections includes: each2) ifFalse: [ allDirections add: each2 ] ] ].

	^allDirections!

allowTravelBy: anActor through: aConnection
	^true!

basicLighted
	^lighted!

connections
	connections isNil ifTrue: [ connections := OrderedCollection new ].
	^connections!

connectionsTo: aString
	^self connections select: [ :each | (each hasSynonym: aString) & (each entryProbability >= self randomGenerator next) ]!

connectionTo: aString
	"Use this when you're sure there's only one way to go in the specified direction."
	^(self connectionsTo: aString) at: 1!

description
	| connectionDescription |

	self lighted
		ifTrue: [
			connectionDescription := self connections inject: '' into: [ :sum :each | sum, each description ].
			^super description,
				(connectionDescription size > 0 ifTrue: [ String lineDelimiter ] ifFalse: ['']),
				connectionDescription ]
		ifFalse: [ ^self class chooseFrom: #( 'Gee, it''s awfully dark in here...'
									'You have wandered into a dark place.  You may be eaten by a grue...'
									'<gulp!!>  Have I m-m-mentioned I''m afraid of the d-d-d-d-dark?') ]!

hasOil
	"Answer the value of the receiver's instance variable hasOil.
	This method was automatically generated, but may be modified."

	^hasOil!

hasOil: anObject
	"Set the value of the receiver's instance variable hasOil to anObject.
	This method was automatically generated, but may be modified."

	hasOil := anObject!

hasWater
	"Answer the value of the receiver's instance variable hasWater.
	This method was automatically generated, but may be modified."

	^hasWater!

hasWater: anObject
	"Set the value of the receiver's instance variable hasWater to anObject.
	This method was automatically generated, but may be modified."

	hasWater := anObject!

initialize
	super initialize.
	self
		lighted: true;
		hasWater: false;
		hasOil: false!

inventory
	"Answer a string describing the contents of this container"

	self lighted
		ifTrue: [ ^self inventory3dPersonWithPrefix: 'There' suffix: 'here' ]
		ifFalse: [ ^'' ]!

inventoryWithPrefix: aPrefixString suffix: aSuffixString
	self lighted
		ifTrue: [ ^super inventoryWithPrefix: aPrefixString suffix: aSuffixString ]
		ifFalse: [ ^'' ]!

lighted
	^self basicLighted | self containsLightSource!

lighted: aBoolean
	"Set the value of the receiver's instance variable lighted to anObject.
	This method was automatically generated, but may be modified."

	lighted := aBoolean!

receiveActor: anActor
	anActor location notNil ifTrue: [ anActor location remove: anActor ].
	anActor location: self.
	self add: anActor.
	^anActor location description, String lineDelimiter, anActor location inventory
!

removeConnection: aConnection
	self connections remove: aConnection ifAbsent: [ ]!

soundFilenames
	soundFilenames isNil ifTrue: [ soundFilenames := Dictionary new ].
	^soundFilenames!

travelFailureReasonFor: anActor through: aConnection
	^''!

world
	"Answer the value of the receiver's instance variable world.
	This method was automatically generated, but may be modified."

	^world!

world: anObject
	"Set the value of the receiver's instance variable world to anObject.
	This method was automatically generated, but may be modified."

	world := anObject! !
!Location categoriesFor: #addConnection:!adding!public! !
!Location categoriesFor: #allDirections!accessing!public! !
!Location categoriesFor: #allowTravelBy:through:!operations!public! !
!Location categoriesFor: #basicLighted!accessing!public! !
!Location categoriesFor: #connections!accessing!public! !
!Location categoriesFor: #connectionsTo:!accessing!public! !
!Location categoriesFor: #connectionTo:!accessing!public! !
!Location categoriesFor: #description!accessing!public! !
!Location categoriesFor: #hasOil!accessing!public! !
!Location categoriesFor: #hasOil:!accessing!public! !
!Location categoriesFor: #hasWater!accessing!public! !
!Location categoriesFor: #hasWater:!accessing!public! !
!Location categoriesFor: #initialize!initialization!public! !
!Location categoriesFor: #inventory!accessing!public! !
!Location categoriesFor: #inventoryWithPrefix:suffix:!accessing!public! !
!Location categoriesFor: #lighted!accessing!public! !
!Location categoriesFor: #lighted:!accessing!public! !
!Location categoriesFor: #receiveActor:!operations!public! !
!Location categoriesFor: #removeConnection:!public!removing! !
!Location categoriesFor: #soundFilenames!accessing!public! !
!Location categoriesFor: #travelFailureReasonFor:through:!accessing!public! !
!Location categoriesFor: #world!accessing!public! !
!Location categoriesFor: #world:!accessing!public! !

World guid: (GUID fromString: '{B1FE94AA-54A3-11D3-8268-00001D19F5C2}')!
World comment: ''!
!World categoriesForClass!Adventure! !
!World methodsFor!

actor
	"Answer the value of the receiver's instance variable actor.
	This method was automatically generated, but may be modified."

	^actor!

actor: anActor
	"Set the value of the receiver's instance variable actor to anObject.
	This method was automatically generated, but may be modified."

	actor := anActor.
	self actor world: self!

addLocation: aLocation
	aLocation world: self.
	self add: aLocation!

defaultFileName
	^'World.stb'!

helpString
	^'No help is available'!

initialize
	super initialize.

	self water: (Item new
				longNounPhrase: 'water';
				shortNounPhrase: 'water';
				article: '').

	self oil: (Item new
				longNounPhrase: 'oil';
				shortNounPhrase: 'oil';
				article: '').!

oil
	"Answer the value of the receiver's instance variable oil.
	This method was automatically generated, but may be modified."

	^oil!

oil: anObject
	"Set the value of the receiver's instance variable oil to anObject.
	This method was automatically generated, but may be modified."

	oil := anObject!

processCommand: aString
	self responses add: (Response text: aString).
	self responses addAll: (self actor processCommand: aString).
	self trigger: #responsesChanged!

randomGenerator
	randomGenerator isNil ifTrue: [ self randomGenerator: Random new ].
	^randomGenerator!

randomGenerator: aRandom
	randomGenerator := aRandom!

responses
	"Answer the value of the receiver's instance variable responses.
	This method was automatically generated, but may be modified."

	responses isNil ifTrue: [ responses := OrderedCollection new ].
	^responses!

restore
	| aFileStream aWorld |

	aFileStream := FileStream read: self defaultFileName text: false.
	aWorld := (STBInFiler on: aFileStream) next.
	self become: aWorld.
	self trigger: #responsesChanged!

save
	| aFileStream |

	aFileStream := FileStream write: self defaultFileName text: false.
	(STBOutFiler on: aFileStream)
		nextPut: self.
	aFileStream close.!

soundDirectory
	^soundDirectory!

soundDirectory: aString
	soundDirectory := aString!

water
	"Answer the value of the receiver's instance variable water.
	This method was automatically generated, but may be modified."

	^water!

water: anObject
	"Set the value of the receiver's instance variable water to anObject.
	This method was automatically generated, but may be modified."

	water := anObject! !
!World categoriesFor: #actor!accessing!public! !
!World categoriesFor: #actor:!accessing!public! !
!World categoriesFor: #addLocation:!adding!public! !
!World categoriesFor: #defaultFileName!accessing!public! !
!World categoriesFor: #helpString!accessing!public! !
!World categoriesFor: #initialize!initializing!public! !
!World categoriesFor: #oil!accessing!public! !
!World categoriesFor: #oil:!accessing!public! !
!World categoriesFor: #processCommand:!accessing!public! !
!World categoriesFor: #randomGenerator!accessing!public! !
!World categoriesFor: #randomGenerator:!accessing!public! !
!World categoriesFor: #responses!accessing!public! !
!World categoriesFor: #restore!binary filing!public! !
!World categoriesFor: #save!binary filing!public! !
!World categoriesFor: #soundDirectory!accessing!public! !
!World categoriesFor: #soundDirectory:!accessing!public! !
!World categoriesFor: #water!accessing!public! !
!World categoriesFor: #water:!accessing!public! !

SimpleActor guid: (GUID fromString: '{B1FE94AB-54A3-11D3-8268-00001D19F5C2}')!
SimpleActor comment: ''!
!SimpleActor categoriesForClass!No category! !
!SimpleActor class methodsFor!

defaultParserClass
	^SimpleCommandParser! !
!SimpleActor class categoriesFor: #defaultParserClass!accessing!public! !

ImmovableItem guid: (GUID fromString: '{B1FE94AC-54A3-11D3-8268-00001D19F5C2}')!
ImmovableItem comment: ''!
!ImmovableItem categoriesForClass!No category! !
!ImmovableItem methodsFor!

initialize
	super initialize.
	self movable: false! !
!ImmovableItem categoriesFor: #initialize!initialization!public! !

LightSource guid: (GUID fromString: '{B1FE94AD-54A3-11D3-8268-00001D19F5C2}')!
LightSource comment: ''!
!LightSource categoriesForClass!No category! !
!LightSource methodsFor!

initialize
	super initialize.
	self lightSource: true! !
!LightSource categoriesFor: #initialize!initialization!public! !

Treasure guid: (GUID fromString: '{B1FE94AE-54A3-11D3-8268-00001D19F5C2}')!
Treasure comment: ''!
!Treasure categoriesForClass!No category! !
!Treasure methodsFor!

initialize
	super initialize.
	self treasure: true! !
!Treasure categoriesFor: #initialize!initialization!public! !

DarkLocation guid: (GUID fromString: '{B1FE94AF-54A3-11D3-8268-00001D19F5C2}')!
DarkLocation comment: ''!
!DarkLocation categoriesForClass!No category! !
!DarkLocation methodsFor!

initialize
	super initialize.
	self lighted: false! !
!DarkLocation categoriesFor: #initialize!initialization!public! !

ForwardingLocation guid: (GUID fromString: '{B1FE94B0-54A3-11D3-8268-00001D19F5C2}')!
ForwardingLocation comment: ''!
!ForwardingLocation categoriesForClass!No category! !
!ForwardingLocation methodsFor!

receiveActor: anActor
	^self description, String lineDelimiter, ((self connections at: 1) destination receiveActor: anActor)! !
!ForwardingLocation categoriesFor: #receiveActor:!operations!public! !

ReflectingLocation guid: (GUID fromString: '{B1FE94B1-54A3-11D3-8268-00001D19F5C2}')!
ReflectingLocation comment: 'A ReflectingLocation is one which returns the Actor to its original location after displaying the location''s description.  This is basically a cheap, no-special-code-required way to spit out a message when an Actor goes in a given direction without moving the Actor from its current location.'!
!ReflectingLocation categoriesForClass!No category! !
!ReflectingLocation methodsFor!

receiveActor: anActor
	^self description! !
!ReflectingLocation categoriesFor: #receiveActor:!operations!public! !

TerminalLocation guid: (GUID fromString: '{B1FE94B2-54A3-11D3-8268-00001D19F5C2}')!
TerminalLocation comment: ''!
!TerminalLocation categoriesForClass!No category! !
!TerminalLocation methodsFor!

receiveActor: anActor
	anActor alive: false.
	^self description! !
!TerminalLocation categoriesFor: #receiveActor:!operations!public! !

TestWorld guid: (GUID fromString: '{B1FE94B3-54A3-11D3-8268-00001D19F5C2}')!
TestWorld comment: ''!
!TestWorld categoriesForClass!No category! !
!TestWorld methodsFor!

defaultFileName
		^'TestWorld.stb'!

initialize
	"Create a world with a few locations and items."

	| loc1 loc2 loc3 loc4 |

	super initialize.

	self
		longDescription: 'You''ve entered a world full of new possibilities';
		shortDescription: 'You''re in the test world'.

	loc1 := Location new initialize
			longDescription: 'You have entered a room with bright red walls.  An arched portico leads north.';
			shortDescription: 'You''re in the red room'.						

	loc2 := Location new initialize
			longDescription: 'You are in a room with bright blue walls.  There is a plain wooden door to the east ',
						'while a damp and dingy tunnel runs to the southeast.';
			shortDescription: 'You''re in the blue room'.

	loc3 := Location new initialize
			longDescription: 'Before you lies a room with bright green walls.  A dusty passageway leads to the ',
						'west, while a narrow hallway leads south.';
			shortDescription: 'You''re a resident of the green room'.

	loc4 := Location new initialize
			longDescription: 'Your eyes are jolted by the electric purple walls of this room.  A ',
						'portal festooned with gilt and pearls leads to the northwest.';
			shortDescription: 'You''re in the purple room'.

	self
		addLocation: loc1;
		addLocation: loc2;
		addLocation: loc3;
		addLocation: loc4.

	loc1 addConnection: (Connection new initialize
					synonyms: #('north' 'portico');
					destination: loc2);
		add: (Item new initialize
				longNounPhrase: 'old-fashioned oil lamp';
				shortNounPhrase: 'lamp').

	loc2
		addConnection: (Connection new initialize
					synonyms: #('east' 'door');
					destination: loc3);
		addConnection: (Connection new initialize
					synonyms: #('southeast' 'tunnel');
					destination: loc4);
		add: (Item new initialize
				longNounPhrase: 'large grandfather clock';
				shortNounPhrase: 'grandfather clock').

	loc3
		addConnection: (Connection new initialize
					synonyms: #('west' 'passage' 'passageway');
					destination: loc2);
		addConnection: (Connection new initialize
					synonyms: #('south' 'hall' 'hallway');
					destination: loc4);
		add: (Item new initialize
				longNounPhrase: 'iron-bound chest';
				shortNounPhrase: 'chest').

	loc4 addConnection: (Connection new initialize
					synonyms: #('northwest' 'portal');
					destination: loc2);
		add: (Item new initialize
				longNounPhrase: 'intricate key';
				shortNounPhrase: 'intricate key').

	self actor: (SimpleActor new initialize
					location: loc1;
					longDescription: 'a nervous-looking individual with thick glasses';
					shortDescription: 'an adventurer').! !
!TestWorld categoriesFor: #defaultFileName!accessing!public! !
!TestWorld categoriesFor: #initialize!initializing!public! !

AdventureShell guid: (GUID fromString: '{B1FE94B4-54A3-11D3-8268-00001D19F5C2}')!
AdventureShell comment: ''!
!AdventureShell categoriesForClass!No category! !
!AdventureShell methodsFor!

createComponents
	super createComponents.

	inputTextPresenter := self add: TextPresenter new name: 'inputText'.
	displayTextPresenter := self add: TextPresenter new name: 'displayText'.!

createSchematicWiring
	super createSchematicWiring.

	self inputTextPresenter
		when: #keyPressed: send: #onKeyPressed: to: self.

	self model
		when: #responsesChanged send: #onResponsesChanged to: self.!

displayTextPresenter
	"Answer the value of the receiver's instance variable displayTextPresenter.
	This method was automatically generated, but may be modified."

	^displayTextPresenter!

inputTextPresenter
	"Answer the value of the receiver's instance variable inputTextPresenter.
	This method was automatically generated, but may be modified."

	^inputTextPresenter!

onExit
	self exit!

onHelp
	MessageBox new
		caption: 'Adventure Help';
		notify: self model helpString!

onKeyPressed: aKeyEvent
	aKeyEvent wParam = VK_RETURN		"Enter key pressed"
		ifTrue: [ self processCommand ]!

onResponsesChanged
	"Display the text of all the responses in the model"

	| text |

	self inputTextPresenter value: ''.
	text := String new.
	self model responses do: [ :each | text := text, String lineDelimiter, each text ].
	self displayTextPresenter value: text.

	self displayTextPresenter view scrollVertical: self displayTextPresenter view lines!

onRestore
	self displayTextPresenter value: ''.
	self model restore!

onSave
	self model save!

onViewOpened
	super onViewOpened.
	self model processCommand: 'look'!

processCommand
	self model processCommand: self inputTextPresenter value.
	self model actor alive
		ifFalse: [ MessageBox warning: 'You seem to have died'.
				self inputTextPresenter view disable ]
! !
!AdventureShell categoriesFor: #createComponents!initializing!public! !
!AdventureShell categoriesFor: #createSchematicWiring!initializing!public! !
!AdventureShell categoriesFor: #displayTextPresenter!accessing!private! !
!AdventureShell categoriesFor: #inputTextPresenter!accessing!private! !
!AdventureShell categoriesFor: #onExit!event handling!public! !
!AdventureShell categoriesFor: #onHelp!event handling!public! !
!AdventureShell categoriesFor: #onKeyPressed:!event handling!public! !
!AdventureShell categoriesFor: #onResponsesChanged!event handling!public! !
!AdventureShell categoriesFor: #onRestore!event handling!public! !
!AdventureShell categoriesFor: #onSave!event handling!public! !
!AdventureShell categoriesFor: #onViewOpened!event handling!public! !
!AdventureShell categoriesFor: #processCommand!operations!private! !

!AdventureShell class methodsFor!

defaultModel
	^TestWorld new!

defaultView
	^'AdventureShell'!

referencesToOtherPackages
	"Private - Force references to other packages."
	MultilineTextEditExtensions! !
!AdventureShell class categoriesFor: #defaultModel!models!public! !
!AdventureShell class categoriesFor: #defaultView!public!views! !
!AdventureShell class categoriesFor: #referencesToOtherPackages!Dependencies!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: AdventureShell name: 'AdventureShell') assign: (Object fromBinaryStoreBytes:
(ByteArray fromHexString: '2153544220312046020C0001000000566965775265736F75726365000000000E0124005354425265736F757263655354424279746541727261794163636573736F7250726F787900000000720000005C090000215354422030204E070C00090000005354425669657750726F7879000000004E020D0001000000535442436C61737350726F78790000000036000600537472696E6707000000446F6C7068696E92000000090000005368656C6C56696577260005004172726179180000000000000000000000C20000000200000001009E010100020060000000000000000000000000000000070000000000000000000000000000000000000006010D004672616D696E674C61796F7574000000000E021A005354424964656E7469747944696374696F6E61727950726F7879000000007A00000000000000A0000000920000000B0000004C6F6F6B75705461626C65C2000000040000005A000000000000007A00000000000000A000000092000000110000004D756C74696C696E655465787445646974C2000000100000000000000060000000C20000000200000036000C004C61726765496E746567657204000000041001440104000060010000000000000000000000000000070000000000000000000000000000000000000000000000B2010000040000007135E77706020D004E756C6C436F6E7665727465720000000000000000000000000900000006010F004D65737361676553657175656E6365000000000E021200535442436F6C6C656374696F6E50726F7879000000007A00000000000000A000000092000000110000004F726465726564436F6C6C656374696F6EC20000000300000006030B004D65737361676553656E64000000000E010E0053544253796D626F6C50726F787900000000920000001000000063726561746541743A657874656E743AC20000000200000006020500506F696E74000000000300000003000000D202000000000000B1040000350000006001000072020000000000009A02000000000000920000000F00000073656C656374696F6E52616E67653AC20000000100000006030800496E74657276616C000000000300000001000000030000006001000072020000000000009A02000000000000920000000B00000069734D6F6469666965643AC200000001000000200000006001000006010F0057494E444F57504C4143454D454E5400000000360009004279746541727261792C0000002C0000000000000001000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0100000001000000590200001B0000002A0200000000000040020000C200000000000000D202000000000000C1000000C10000000000000046081200010000004672616D696E67436F6E73747261696E7473000000009A02000000000000920000000F0000006669786564506172656E744C656674030000009A0200000000000092000000100000006669786564506172656E745269676874010000009A02000000000000920000000E0000006669786564506172656E74546F700300000070040000370000005A0000000000000070010000C2000000100000000000000060000000C200000002000000B201000004000000441121440104000090040000000000000000000000000000070000000000000000000000000000000000000000000000B2010000040000007135E777E20100000000000000000000000000000B00000002020000000000002A0200000000000040020000C2000000030000007202000000000000A0020000C200000002000000D2020000000000000300000037000000D202000000000000B1040000C501000090040000720200000000000010030000C200000001000000420300000000000003000000010000000300000090040000720200000000000070030000C2000000010000002000000090040000A203000000000000C20300002C0000002C0000000000000001000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF010000001B00000059020000FD0000002A0200000000000040020000F0030000000400000000000012040000000000003004000003000000500400000100000070040000370000009A0200000000000092000000110000006669786564506172656E74426F74746F6D010000001A010000000000007A00000000000000A000000092000000120000004964656E7469747944696374696F6E617279C200000004000000600100009200000009000000696E7075745465787490040000920000000B000000646973706C6179546578740000000046050700020000004D656E75426172000000000000000010000000C20000000200000046050400020000004D656E75000000000000000010000000C20000000300000006010F00436F6D6D616E644D656E754974656D000000004604120002000000436F6D6D616E644465736372697074696F6E000000009A0200000000000092000000060000006F6E53617665920000000500000026536176650100000000000000D206000000000000F2060000000000009A0200000000000092000000090000006F6E526573746F7265920000000800000026526573746F72650100000000000000D206000000000000F2060000000000009A0200000000000092000000060000006F6E4578697492000000050000004526786974B12000000000000092000000050000002646696C6500000000A2060000000000000000000010000000C200000001000000D206000000000000F2060000000000009A0200000000000092000000060000006F6E48656C7092000000050000002648656C70912000000000000092000000050000002648656C70000000009200000000000000000000000000000006031000416363656C657261746F725461626C65000000000000000010000000C20000000200000006020B004173736F63696174696F6E00000000B1200000A0070000B208000000000000912000002008000000000000010000000000000000000000000000000000000002020000000000002A0200000000000040020000C2000000030000007202000000000000A0020000C200000002000000D2020000000000000B0000000B000000D202000000000000C3040000310200006000000072020000000000009A020000000000009200000005000000746578743AC2000000010000009200000009000000416476656E747572656000000072020000000000009A0200000000000092000000080000006D656E754261723AC2000000010000008006000060000000A203000000000000C20300002C0000002C0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0500000005000000660200001D0100002A0200000000000040020000C20000000200000060010000900400000004000000000000460504000300000049636F6E0000000000000000100000000E02110053544253696E676C65746F6E50726F7879000000009A000000000000005200000007000000446F6C7068696E5200000018000000496D61676552656C617469766546696C654C6F6361746F72BA00000000000000520000000700000063757272656E74520000000D0000005368656C6C566965772E69636F0E021F0053544245787465726E616C5265736F757263654C69627261727950726F7879000000005200000010000000646F6C7068696E64723030352E646C6C00000000'))!

