| package |
package := Package name: 'ColossalCaveWorld'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.


package classNames
	add: #Bridge;
	add: #Clam;
	add: #ColossalCaveActor;
	add: #ColossalCaveCommandParser;
	add: #ColossalCaveShell;
	add: #ColossalCaveWorld;
	add: #GoldenChain;
	add: #HallOfMists;
	add: #HallOfMountainKing;
	add: #Pearl;
	add: #Plant;
	add: #SecretCanyon;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'SmallWorlds';
	yourself).

package!

"Class Definitions"!

SimpleCommandParser subclass: #ColossalCaveCommandParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Connection subclass: #Bridge
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SimpleActor subclass: #ColossalCaveActor
	instanceVariableNames: 'feeSequence fieSequence foeSequence'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Item subclass: #Clam
	instanceVariableNames: 'world'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ImmovableItem subclass: #Plant
	instanceVariableNames: 'state'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Treasure subclass: #GoldenChain
	instanceVariableNames: 'locked'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Treasure subclass: #Pearl
	instanceVariableNames: 'location'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DarkLocation subclass: #HallOfMists
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DarkLocation subclass: #HallOfMountainKing
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DarkLocation subclass: #SecretCanyon
	instanceVariableNames: 'enteredFromLocation'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
World subclass: #ColossalCaveWorld
	instanceVariableNames: 'nugget steps cage rod bird snake bridge eastBankOfFissure westBankOfFissure coins pillow tablet clam magazine plant dragon troll westPit vase eggs trident emerald pyramid rug spices silverBars jewelry giantPassage door y2 giantRoom pearl culDeSac orientalRoom alcove secretCanyon mouse swChasm neChasm ferociousBear docileBear food chain keys building wittsEnd'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AdventureShell subclass: #ColossalCaveShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ColossalCaveCommandParser guid: (GUID fromString: '{B1FE94B5-54A3-11D3-8268-00001D19F5C2}')!
ColossalCaveCommandParser comment: ''!
!ColossalCaveCommandParser categoriesForClass!No category! !
!ColossalCaveCommandParser class methodsFor!

verbs
	^SimpleCommandParser verbs, #('fee' 'fie' 'foe' 'foo')! !
!ColossalCaveCommandParser class categoriesFor: #verbs!accessing!public! !

Bridge guid: (GUID fromString: '{B1FE94B6-54A3-11D3-8268-00001D19F5C2}')!
Bridge comment: ''!
!Bridge categoriesForClass!No category! !
!Bridge methodsFor!

initialize
	super initialize.
	self addSound: #collapsing filename: 'rubble.wav'!

transport: anActor
	"If trying to cross the bridge with the bear, something awful happens"

	(anActor contains: anActor world docileBear) ifTrue: [
		self playSound: #collapsing.
		anActor alive: false.
		^'Just as you reach the other side, the bridge buckles beneath ',
		 'the weight of the bear, which was still following you around.  ',
		'You scrabble desperately for support, but as the bridge ',
		'collapses you stumble back and fall into the chasm.' ].

	^super transport: anActor! !
!Bridge categoriesFor: #initialize!initialize/release!public! !
!Bridge categoriesFor: #transport:!operations!public! !

ColossalCaveActor guid: (GUID fromString: '{B1FE94B7-54A3-11D3-8268-00001D19F5C2}')!
ColossalCaveActor comment: ''!
!ColossalCaveActor categoriesForClass!No category! !
!ColossalCaveActor methodsFor!

drop: aCollection
	| matchingObjects |

	aCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('''-kick me, Jesus, through the goal-posts of life?')].

	matchingObjects := self contentsLike: (aCollection at: 1).

	"Only drop the vase on the pillow"

	matchingObjects size = 1 ifTrue: [
		(matchingObjects at: 1) == self world vase ifTrue: [
			(self location contains: self world pillow)
				ifTrue: [
					self remove: self world vase.
					self world pillow add: self world vase.
					self location == self world building ifTrue: [
						self accumulatePoints: self world vase ].
					^Response text: 'OK' ] 
				ifFalse: [
					self remove: self world vase.
					self location
						longDescription: (self location basicLongDescription,
										'  The floor is littered with worthless shards of pottery.');
						shortDescription: (self location basicShortDescription,
										'  There are some pieces of broken pottery on the floor.').
					^Response text: 'The ming vase drops with a delicate crash.' ] ].

		"If dropping a treasure for the troll, the troll takes it and goes elsewhere"

		(self location contains: self world troll) & ((matchingObjects at: 1) treasure)
			ifTrue: [
				self remove: (matchingObjects at: 1).
				self location remove: self world troll.
				self world troll longDescriptionSeen: false.
				self location == self world swChasm ifTrue: [ self world neChasm add: self world troll ].
				^Response text: 'The troll catches your treasure and scurries away out of sight.' ].

		"If dropping the bear, change its description"

		((matchingObjects at: 1) == self world docileBear)
			ifTrue: [ self world docileBear
						longDescription: 'There is a very large, tame bear here.';
						shortDescription: 'There is a very large, tame bear here.' ].

		"If dropping the bear in front of the troll, lose the troll"

		((matchingObjects at: 1) == self world docileBear) & (self location contains: self world troll)
			ifTrue: [
				self remove: (matchingObjects at: 1).
				self location add: (matchingObjects at: 1).
				self location remove: self world troll.
				^Response text: 'The bear lumbers toward the troll, who lets out a ',
							'startled shriek and scurries away.  The bear soon gives ',
							'up pursuit and wanders back.' ].

		"If dropping food in front of the ferocious bear replace him with the docile bear"

		((matchingObjects at: 1) == self world food) & (self location contains: self world ferociousBear)
			ifTrue: [
				self location remove: self world ferociousBear.
				self location add: self world docileBear.
				self remove: self world food.
				^Response text: 'The bear eagerly wolfs down your food, after which he seems ',
							'to calm down considerably, and even becomes rather friendly.' ].

		"Count up points earned"

		(self location == self world building) & ((matchingObjects at: 1) ~~ self world magazine) ifTrue: [
			self accumulatePoints: (matchingObjects at: 1) ].

		"Player gets one point for dropping magazines at Witt's End"

		(((matchingObjects at: 1) == self world magazine) & (self location == self world wittsEnd))
			ifTrue: [ self accumulatePoints: self world magazine ] ].

	^super drop: aCollection!

empty: anOrderedCollection
	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Empty *WHAT*?!!?')].

	self location = self world westPit
		ifTrue: [ ^self waterPlant: anOrderedCollection ].

	(self location == self world giantPassage)
		ifTrue: [ ^self oilDoor: anOrderedCollection ].

	^super empty: anOrderedCollection!

fee: anOrderedCollection
	anOrderedCollection size = 0
		ifTrue: [ self feeSequence: self sequence.  ^Response text: 'OK' ]
		ifFalse: [ ^Response text: 'Huh?' ]!

feeSequence
	"Answer the value of the receiver's instance variable feeSequence.
	This method was automatically generated, but may be modified."

	^feeSequence!

feeSequence: anObject
	"Set the value of the receiver's instance variable feeSequence to anObject.
	This method was automatically generated, but may be modified."

	feeSequence := anObject!

fie: anOrderedCollection
	anOrderedCollection size = 0
		ifTrue: [
			self feeSequence = (self sequence -1)
				ifTrue: [ self fieSequence: self sequence ]
				ifFalse: [ self fieSequence: 0 ].
			^Response text: 'OK' ]
		ifFalse: [ ^Response text: 'Huh?' ]!

fieSequence
	"Answer the value of the receiver's instance variable fieSequence.
	This method was automatically generated, but may be modified."

	^fieSequence!

fieSequence: anObject
	"Set the value of the receiver's instance variable fieSequence to anObject.
	This method was automatically generated, but may be modified."

	fieSequence := anObject!

fill: anOrderedCollection
	| matchingObjects |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Fill *WHAT*?!!?')].

	matchingObjects := self contentsLike: (anOrderedCollection at: 1).
	((matchingObjects size = 1) and: [ (matchingObjects at: 1) == self world vase ]) ifTrue: [
		self location hasWater
			ifTrue: [ "Don't *ever* put water into the vase"
				self remove: self world vase.
				self location
					longDescription: (self location basicLongDescription,
									'  The floor is littered with worthless shards of pottery.');
					shortDescription: (self location basicShortDescription,
									'  There are some pieces of broken pottery on the floor.').
				^Response text: 'The sudden change in temperature has delicately shattered the vase.' ].

		self location hasOil
			ifTrue: [ (matchingObjects at: 1) add: self world oil.
					^Response text: 'OK' ] ].

	^super fill: anOrderedCollection!

foe: anOrderedCollection
	anOrderedCollection size = 0
		ifTrue: [
			self fieSequence = (self sequence -1)
				ifTrue: [ self foeSequence: self sequence ]
				ifFalse: [ self foeSequence: 0 ].
			^Response text: 'OK' ]
		ifFalse: [ ^Response text: 'Huh?' ]!

foeSequence
	"Answer the value of the receiver's instance variable foeSequence.
	This method was automatically generated, but may be modified."

	^foeSequence!

foeSequence: anObject
	"Set the value of the receiver's instance variable foeSequence to anObject.
	This method was automatically generated, but may be modified."

	foeSequence := anObject!

foo: anOrderedCollection
	anOrderedCollection size = 0
		ifTrue: [
			self foeSequence = (self sequence -1)
				ifTrue: [ (self contains: self world eggs)
						ifTrue: [ self remove: self world eggs.
							     self world giantRoom add: self world eggs.
							     ^Response text: 'The golden eggs have vanished!!' ]
						ifFalse: [ ^Response text: 'Nothing happens' ] ].
			^Response text: 'OK' ]
		ifFalse: [ ^Response text: 'Huh?' ]!

get: anOrderedCollection
	| matchingObjects |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Get *WHAT*?!!?')].

	matchingObjects := self location contentsLike: (anOrderedCollection at: 1).

	matchingObjects size > 0 ifTrue: [
		"Special processing if trying to get the bird"

		(matchingObjects at: 1) = self world bird
			ifTrue: [ ^self getBird: matchingObjects ].

		"Can't get the rug if the dragon is present"

		((matchingObjects at: 1) = self world rug) & (self location contains: self world dragon)
			ifTrue: [ ^Response text: 'The dragon seems very comfortable on the rug and doesn''t want to get up.' ].

		"If getting the bear, change its description"

		((matchingObjects at: 1) == self world docileBear)
			ifTrue: [ self world docileBear
						longDescription: 'You are being followed by a very large, tame bear.';
						shortDescription: 'You are being followed by a very large, tame bear.' ].

		"Can't get the chain if the bear hasn't been fed"

		((matchingObjects at: 1) == self world chain) ifTrue: [
			(self location contains: self world ferociousBear)
				ifTrue: [ ^Response text: 'There is no way to get past the bear to unlock the chain, which ',
									'is probably just as well.' ].

			"Can't get the chain if it's locked"

			self world chain locked ifTrue: [ ^Response text: 'The chain is still locked firmly to the wall.' ] ] ].

	^super get: anOrderedCollection!

getBird: matchingObjects
	"Private - can't get the bird unless you don't have the rod, and you do have the cage."

	(self contains: self world rod)
		ifTrue: [ ^Response text: 'The bird was unafraid when you entered, but as ',
						    'you approach it becomes disturbed and you ',
						    'cannot catch it' ]
		ifFalse: [ (self contains: self world cage)
					ifTrue: [ self location remove: (matchingObjects at: 1).
							self world cage add: (matchingObjects at: 1).
							^Response text: 'OK' ]
					ifFalse: [ ^Response text: 'You could catch the bird, but you cannot carry it.' ] ]!

go: anOrderedCollection
	| connections |

	"When in the west pit in the TwoPit room you can only 'climb' if the plant has grown enough"

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Go *WHERE*?!!?')].

	(self location == self world westPit) & ((anOrderedCollection at: 1) = 'climb') & (self world plant state ~= 3)
		ifTrue: [ ^Response chooseFrom: #('There''s no exit in that direction'
						'You can''t go that way'
						'You might be able to do that if you had a pickaxe'
						'Huh?') ].

	"At Y2 you can't get out by saying 'plover' if you're carrying the emerald"

	(self location == self world y2) & (self contains: self world emerald) & ((anOrderedCollection at: 1) = 'plover')
		ifTrue: [ ^Response text: 'You can''t go that way' ].

	"At the alcove you can't go into the plover room if you're carrying anything except the emerald"

	(self location == self world alcove) ifTrue: [
			((self contents size > 1) | ((self contents size = 1) & ((self contains: self world emerald) isFalse))) &
			(#('east' 'passage') anySatisfy: [ :each | each = (anOrderedCollection at: 1) ])
		ifTrue: [ ^Response text: 'Something you''re carrying won''t fit through the tunnel with ',
						    'you.  You''d best take inventory and drop something.' ] ].

	"In the secret canyon you can only go back the way you came if the dragon is present"

	(self location == self world secretCanyon) ifTrue: [
		connections := self location connectionsTo: (anOrderedCollection at: 1).
		(connections at: 1) destination ~= self enteredFromLocation ifTrue: [
			^Response text: 'The dragon looks rather nasty.  You''d best not try to get by.' ] ].

	"If at the chasm, the troll won't let you cross the bridge"

	((self location == self world swChasm) | (self location == self world neChasm)) &
			(self location contains: self world troll) ifTrue: [
		^Response text: 'The troll refuses to let you cross.' ].

	^super go: anOrderedCollection!

initialize
	super initialize.

	self
		feeSequence: 0;
		fieSequence: 0;
		foeSequence: 0!

oilDoor: anOrderedCollection
	"Private"
	| matchingObjects response |

	matchingObjects := self contentsLike: (anOrderedCollection at: 1).
	matchingObjects size = 0
		ifTrue: [ ^Response text: 'I don''t see a ', (anOrderedCollection at: 1), ' here.' ].
	matchingObjects size > 1
		ifTrue: [ ^Response text: 'Could you be a bit more specific?' ].

	response := Response text: 'OK'.

	(matchingObjects at: 1) contents do: [ :each |
		each ~= self world water & (each ~= self world oil)
			ifTrue: [ self location add: each ]
			ifFalse: [ 
				each = self world oil
					ifTrue: [ self world door locked: false ] ] ].

	(matchingObjects at: 1) contents removeAll: (matchingObjects at: 1) contents.
	^response!

open: anOrderedCollection
	"'open' usually means 'open a door'.  However, the player MAY want to try opening the
	clam/oyster, in which case they'd better have the trident."

	| matchingObjects response |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Open *WHAT*?!!?')].

	matchingObjects := (self contentsLike: (anOrderedCollection at: 1)),
					(self location contentsLike: (anOrderedCollection at: 1)).
	matchingObjects size ~= 1 ifTrue: [ ^super open: anOrderedCollection ].

	(matchingObjects at: 1) == self world clam ifFalse: [ ^super open: anOrderedCollection ].

	"OK, we're trying to open the clam.  So *does* the actor have the trident?"

	(self contains: self world trident) ifFalse: [ ^Response text: 'Oh?  How?' ].

	"If the pearl has never been seen before it'll be contained by the current world.  If so move it to the
	 cul-de-sac and issue an appropriate message."

	(self world contains: self world pearl)
		ifTrue: [
			self world remove: self world pearl.
			self world culDeSac add: self world pearl.
			^Response text: 'A glistening pearl falls out of the oyster and rolls away' ]
		ifFalse: [ ^Response text: 'OK' ]!

unlock: anOrderedCollection
	"'unlock' usually means 'unlock a door'.  However, the player MAY want to unlock the
	chain, in which case they'd better have the keys."

	| matchingObjects response |

	anOrderedCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Unlock *WHAT*?!!?')].

	matchingObjects := (self contentsLike: (anOrderedCollection at: 1)),
					(self location contentsLike: (anOrderedCollection at: 1)).
	matchingObjects size ~= 1 ifTrue: [ ^super unlock: anOrderedCollection ].

	(matchingObjects at: 1) == self world chain ifFalse: [ ^super unlock: anOrderedCollection ].

	"OK, we're trying to unlock the chain.  So *does* the actor have the keys?"

	(self location contains: self world ferociousBear)
		ifTrue: [ ^Response text: 'There is no way to get past the bear to unlock the chain, which ',
						    'is probably just as well.' ].

	(self contains: self world keys) ifFalse: [ ^Response text: 'You have nothing you can use to unlock the chain.' ].

	"OK, unlock it"

	self world chain locked: false.
	^Response text: 'The chain is now unlocked.'!

waterPlant: anOrderedCollection
	"Private"
	| matchingObjects response |

	matchingObjects := self contentsLike: (anOrderedCollection at: 1).
	matchingObjects size = 0
		ifTrue: [ ^Response text: 'I don''t see a ', (anOrderedCollection at: 1), ' here.' ].
	matchingObjects size > 1
		ifTrue: [ ^Response text: 'Could you be a bit more specific?' ].

	response := Response text: 'OK'.

	(matchingObjects at: 1) contents do: [ :each |
		each ~= self world water & (each ~= self world oil)
			ifTrue: [ self location add: each ]
			ifFalse: [ 
				each = self world water
					ifTrue: [ response := Response text: (self world plant incrementState,
										self world plant longDescription) ]
					ifFalse: [ response := Response text: 'The plant indignantly shakes ',
										     'the oil off its leaves and ',
										     'asks: "Water?".' ] ] ].
	(matchingObjects at: 1) contents removeAll: (matchingObjects at: 1) contents.
	^response!

wave: aCollection
	| matchingObjects |

	aCollection size = 0 ifTrue: [ ^Response standardChoicesPlus: #('Wave *WHAT*?!!?')].

	matchingObjects := self contentsLike: (aCollection at: 1).
	matchingObjects size = 0 ifTrue: [ ^Response text: 'Huh?' ].
	matchingObjects size > 1 ifTrue: [ ^Response text: 'Could you be a bit more specific?' ].

	(matchingObjects at: 1) = self world rod & 
			((self location = self world eastBankOfFissure) | (self location = self world westBankOfFissure))
		ifTrue: [
			"Flop the state of the bridge"

			self world bridge open
				ifTrue: [ self world bridge open: false. ^Response text: 'The bridge has vanished!!' ]
				ifFalse: [ self world bridge open: true. ^Response text: self world bridge openDescription ] ].

	((matchingObjects at: 1) = self world trident) &
			((self location contains: self world clam) | (self contains: self world clam))
		ifTrue: [ self world clam flipFlop. ^Response text: 'OK' ].

	^super wave: aCollection! !
!ColossalCaveActor categoriesFor: #drop:!public!verb processing! !
!ColossalCaveActor categoriesFor: #empty:!public!verb processing! !
!ColossalCaveActor categoriesFor: #fee:!public!verb processing! !
!ColossalCaveActor categoriesFor: #feeSequence!accessing!public! !
!ColossalCaveActor categoriesFor: #feeSequence:!accessing!public! !
!ColossalCaveActor categoriesFor: #fie:!public!verb processing! !
!ColossalCaveActor categoriesFor: #fieSequence!accessing!public! !
!ColossalCaveActor categoriesFor: #fieSequence:!accessing!public! !
!ColossalCaveActor categoriesFor: #fill:!public!verb processing! !
!ColossalCaveActor categoriesFor: #foe:!public!verb processing! !
!ColossalCaveActor categoriesFor: #foeSequence!accessing!public! !
!ColossalCaveActor categoriesFor: #foeSequence:!accessing!public! !
!ColossalCaveActor categoriesFor: #foo:!public!verb processing! !
!ColossalCaveActor categoriesFor: #get:!public!verb processing! !
!ColossalCaveActor categoriesFor: #getBird:!public!verb processing! !
!ColossalCaveActor categoriesFor: #go:!public!verb processing! !
!ColossalCaveActor categoriesFor: #initialize!initialize/release!public! !
!ColossalCaveActor categoriesFor: #oilDoor:!public!verb processing! !
!ColossalCaveActor categoriesFor: #open:!public!verb processing! !
!ColossalCaveActor categoriesFor: #unlock:!public!verb processing! !
!ColossalCaveActor categoriesFor: #waterPlant:!public!verb processing! !
!ColossalCaveActor categoriesFor: #wave:!public! !

!ColossalCaveActor class methodsFor!

defaultParserClass
	^ColossalCaveCommandParser! !
!ColossalCaveActor class categoriesFor: #defaultParserClass!public! !

Clam guid: (GUID fromString: '{B1FE94B8-54A3-11D3-8268-00001D19F5C2}')!
Clam comment: ''!
!Clam categoriesForClass!No category! !
!Clam methodsFor!

beClam
	self
		longNounPhrase: 'enormous giant clam';
		shortNounPhrase: 'enormous giant clam'!

beOyster
	self
		longNounPhrase: 'enormous giant oyster';
		shortNounPhrase: 'enormous giant oyster'!

flipFlop
	self isClam
		ifTrue: [ self beOyster ]
		ifFalse: [ self beClam ]
!

isClam
	^'*clam*' match: self description!

isOyster
	^'*oyster*' match: self description!

world
	"Answer the value of the receiver's instance variable world.
	This method was automatically generated, but may be modified."

	^world!

world: anObject
	"Set the value of the receiver's instance variable world to anObject.
	This method was automatically generated, but may be modified."

	world := anObject! !
!Clam categoriesFor: #beClam!operations!public! !
!Clam categoriesFor: #beOyster!operations!public! !
!Clam categoriesFor: #flipFlop!operations!public! !
!Clam categoriesFor: #isClam!public!testing! !
!Clam categoriesFor: #isOyster!public!testing! !
!Clam categoriesFor: #world!accessing!public! !
!Clam categoriesFor: #world:!accessing!public! !

Plant guid: (GUID fromString: '{B1FE94B9-54A3-11D3-8268-00001D19F5C2}')!
Plant comment: ''!
!Plant categoriesForClass!No category! !
!Plant methodsFor!

basicDescription
	^self longDescription!

basicLongDescription
	^self longDescription!

incrementState
	self state = 1 ifTrue: [
		self state: 2.
		^'The plant spurts into furious growth for a few seconds.' ].
	self state = 2 ifTrue: [
		self state: 3.
		^'The plant grows explosively, almost filling the bottom of the pit.' ].
	self state = 3 ifTrue: [
		self state: 1.
		^'You''ve over-watered the plant!!  It''s shriveling up!! It''s, It''s...' ]!

initialize
	super initialize.
	self state: 1!

longDescription
	self state = 1 ifTrue: [ ^'There is a tiny little plant in the pit, murmuring "Water, Water, ..."' ].
	self state = 2 ifTrue: [ ^'There is a 12-foot-tall beanstalk stretching up out of the pit, bellowing "Water!!!! Water!!!!"' ].
	self state = 3 ifTrue: [ ^'There is a gigantic beanstalk stretching all the way up to the hole.' ]!

state
	"Private"
	^state!

state: anObject
	"Private"

	state := anObject! !
!Plant categoriesFor: #basicDescription!accessing!public! !
!Plant categoriesFor: #basicLongDescription!accessing!public! !
!Plant categoriesFor: #incrementState!operations!public! !
!Plant categoriesFor: #initialize!initialize/release!public! !
!Plant categoriesFor: #longDescription!accessing!public! !
!Plant categoriesFor: #state!accessing!public! !
!Plant categoriesFor: #state:!accessing!public! !

GoldenChain guid: (GUID fromString: '{B1FE94BA-54A3-11D3-8268-00001D19F5C2}')!
GoldenChain comment: ''!
!GoldenChain categoriesForClass!No category! !
!GoldenChain methodsFor!

initialize
	super initialize.
	self
		locked: true;
		longNounPhrase: 'golden chain';
		points: 50
!

locked
	"Answer the value of the receiver's instance variable locked.
	This method was automatically generated, but may be modified."

	^locked!

locked: anObject
	"Set the value of the receiver's instance variable locked to anObject.
	This method was automatically generated, but may be modified."

	locked := anObject! !
!GoldenChain categoriesFor: #initialize!initialize/release!public! !
!GoldenChain categoriesFor: #locked!accessing!public! !
!GoldenChain categoriesFor: #locked:!accessing!public! !

Pearl guid: (GUID fromString: '{B1FE94BB-54A3-11D3-8268-00001D19F5C2}')!
Pearl comment: ''!
!Pearl categoriesForClass!No category! !
!Pearl methodsFor!

location
	"Answer the value of the receiver's instance variable location.
	This method was automatically generated, but may be modified."

	^location!

location: anObject
	"Set the value of the receiver's instance variable location to anObject.
	This method was automatically generated, but may be modified."

	location := anObject! !
!Pearl categoriesFor: #location!accessing!public! !
!Pearl categoriesFor: #location:!accessing!public! !

HallOfMists guid: (GUID fromString: '{B1FE94BC-54A3-11D3-8268-00001D19F5C2}')!
HallOfMists comment: 'The staircase ''up'' from the Hall of Mists is hidden (closed) if the Actor has the nugget when he enters.'!
!HallOfMists categoriesForClass!No category! !
!HallOfMists methodsFor!

receiveActor: anActor
	(anActor contains: anActor world nugget)
		ifTrue: [ (self connectionTo: 'up') closed: true ]
		ifFalse: [ (self connectionTo: 'up') closed: false ].

	^super receiveActor: anActor.

! !
!HallOfMists categoriesFor: #receiveActor:!operations!public! !

HallOfMountainKing guid: (GUID fromString: '{B1FE94BD-54A3-11D3-8268-00001D19F5C2}')!
HallOfMountainKing comment: ''!
!HallOfMountainKing categoriesForClass!No category! !
!HallOfMountainKing methodsFor!

add: anObject
	"If the bird is dropped and the snake is present we remove the snake and
	provide a nice little message"

	| response |

	response := super add: anObject.

	self world notNil ifTrue: [
		(anObject = self world bird) & (self contains: self world snake) ifTrue: [
			self remove: self world snake.
			response := Response text: 'The little bird attacks the green snake, and in an ',
								'astounding flurry drives the snake away.' ] ].
	^response!

allowTravelBy: anActor through: aConnection
	"If the snake is present the adventurer can't go in certain directions"

	(self contains: anActor world snake) ifTrue: [
		#('north' 'left' 'south' 'right' 'west' 'forward') do: [ :each |
			(aConnection hasSynonym: each) ifTrue: [ ^false ] ] ].
	^true!

travelFailureReasonFor: anActor through: aConnection
	(self contains: anActor world snake)
		ifTrue: [ #('north' 'left' 'south' 'right' 'west' 'forward') do: [ :each |
				(aConnection hasSynonym: each) ifTrue: [ ^'You can''t get past the snake' ] ] ]
		ifFalse: [ ^'' ]! !
!HallOfMountainKing categoriesFor: #add:!adding!public! !
!HallOfMountainKing categoriesFor: #allowTravelBy:through:!accessing!public! !
!HallOfMountainKing categoriesFor: #travelFailureReasonFor:through:!accessing!public! !

SecretCanyon guid: (GUID fromString: '{B1FE94BE-54A3-11D3-8268-00001D19F5C2}')!
SecretCanyon comment: ''!
!SecretCanyon categoriesForClass!No category! !
!SecretCanyon methodsFor!

add: anObject
	| response |

	response := super add: anObject.

	"If the bird is dropped and the dragon is present we remove the bird and
	provide a nice little message"

	self world notNil ifTrue: [
		(anObject = self world bird) & (self contains: self world dragon)
			ifTrue: [
				self remove: anObject.
				^Response text: 'The little bird attacks the green dragon, and in an astounding ',
							'flurry gets burnt to a cinder.  The ashes blow away.' ] ].

	"If the mouse is dropped and the dragon is present we remove the dragon and
	provide a nice little message."

	self world notNil ifTrue: [
		(anObject = self world mouse) & (self contains: self world dragon)
			ifTrue: [
				self remove: self world dragon.
				^Response text: 'The dragon leaps up, screams in terror, and lumbers off down ',
							'the passageway.  (How''d you know dragons are scared of mice?)' ] ].
	^response!

enteredFromLocation
	"Answer the value of the receiver's instance variable enteredFromLocation.
	This method was automatically generated, but may be modified."

	^enteredFromLocation!

enteredFromLocation: anObject
	"Set the value of the receiver's instance variable enteredFromLocation to anObject.
	This method was automatically generated, but may be modified."

	enteredFromLocation := anObject!

receiveActor: anActor
	self enteredFromLocation: anActor location.
	^super receiveActor: anActor! !
!SecretCanyon categoriesFor: #add:!operations!public! !
!SecretCanyon categoriesFor: #enteredFromLocation!accessing!public! !
!SecretCanyon categoriesFor: #enteredFromLocation:!accessing!public! !
!SecretCanyon categoriesFor: #receiveActor:!operations!public! !

ColossalCaveWorld guid: (GUID fromString: '{B1FE94BF-54A3-11D3-8268-00001D19F5C2}')!
ColossalCaveWorld comment: ''!
!ColossalCaveWorld categoriesForClass!No category! !
!ColossalCaveWorld methodsFor!

alcove
	"Answer the value of the receiver's instance variable alcove.
	This method was automatically generated, but may be modified."

	^alcove!

alcove: anObject
	"Set the value of the receiver's instance variable alcove to anObject.
	This method was automatically generated, but may be modified."

	alcove := anObject!

bird
	"Answer the value of the receiver's instance variable bird.
	This method was automatically generated, but may be modified."

	^bird!

bird: anObject
	"Set the value of the receiver's instance variable bird to anObject.
	This method was automatically generated, but may be modified."

	bird := anObject!

bridge
	"Answer the value of the receiver's instance variable bridge.
	This method was automatically generated, but may be modified."

	^bridge!

bridge: anObject
	"Set the value of the receiver's instance variable bridge to anObject.
	This method was automatically generated, but may be modified."

	bridge := anObject!

building
	"Answer the value of the receiver's instance variable building.
	This method was automatically generated, but may be modified."

	^building!

building: anObject
	"Set the value of the receiver's instance variable building to anObject.
	This method was automatically generated, but may be modified."

	building := anObject!

cage
	"Answer the value of the receiver's instance variable cage.
	This method was automatically generated, but may be modified."

	^cage!

cage: anObject
	"Set the value of the receiver's instance variable cage to anObject.
	This method was automatically generated, but may be modified."

	cage := anObject!

chain
	"Answer the value of the receiver's instance variable chain.
	This method was automatically generated, but may be modified."

	^chain!

chain: anObject
	"Set the value of the receiver's instance variable chain to anObject.
	This method was automatically generated, but may be modified."

	chain := anObject!

clam
	"Answer the value of the receiver's instance variable clam.
	This method was automatically generated, but may be modified."

	^clam!

clam: anObject
	"Set the value of the receiver's instance variable clam to anObject.
	This method was automatically generated, but may be modified."

	clam := anObject!

coins
	"Answer the value of the receiver's instance variable coins.
	This method was automatically generated, but may be modified."

	^coins!

coins: anObject
	"Set the value of the receiver's instance variable coins to anObject.
	This method was automatically generated, but may be modified."

	coins := anObject!

createConnections: aDictionary
	"Private - create connections between locations in this world."

	self
		createConnections1to10: aDictionary;
		createConnections11to20: aDictionary;
		createConnections21to30: aDictionary;
		createConnections31to40: aDictionary;
		createConnections41to50: aDictionary;
		createConnections51to60: aDictionary;
		createConnections61to70: aDictionary;
		createConnections71to80: aDictionary;
		createConnections81to90: aDictionary;
		createConnections91to100: aDictionary;
		createConnections101to110: aDictionary;
		createConnections111to120: aDictionary;
		createConnections121to130: aDictionary;
		createConnections131to140: aDictionary;
		createConnections141to150: aDictionary
!

createConnections101to110: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 101)
		addConnection: (Connection new
						destination: (aDictionary at: 100);
						synonyms: #('south' 'plover' 'out')).

	(aDictionary at: 102)
		addConnection: (Connection new
						destination: (aDictionary at: 103);
						synonyms: #('down' 'shell' 'out')).

	(aDictionary at: 103)
		addConnection: (Connection new
						destination: (aDictionary at: 102);
						synonyms: #('up' 'hall'));
		addConnection: (Connection new
						destination: (aDictionary at: 104);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 64);
						synonyms: #('south')).

	(aDictionary at: 104)
		addConnection: (Connection new
						destination: (aDictionary at: 103);
						synonyms: #('up' 'shell'));
		addConnection: (Connection new
						destination: (aDictionary at: 105);
						synonyms: #('down')).

	(aDictionary at: 105)
		addConnection: (Connection new
						destination: (aDictionary at: 104);
						synonyms: #('up' 'out'));
		addConnection: (Connection new
						destination: (aDictionary at: 103);
						synonyms: #('shell')).

	(aDictionary at: 106)
		addConnection: (Connection new
						destination: (aDictionary at: 64);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 108);
						synonyms: #('east')).

	(aDictionary at: 107)
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 61);
						synonyms: #('east')).

	(aDictionary at: 108)
		addConnection: (Connection new
						destination: (aDictionary at: 141);
						entryProbability: 0.95;
						synonyms: #('east' 'north' 'south' 'northeast' 'southeast' 'southwest' 'northwest'
								   'up' 'down'));
		addConnection: (Connection new
						destination: (aDictionary at: 142);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 106);
						synonyms: #('east')).

	(aDictionary at: 109)
		addConnection: (Connection new
						destination: (aDictionary at: 69);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 113);
						synonyms: #('north' 'reservoir')).

	(aDictionary at: 110)
		addConnection: (Connection new
						destination: (aDictionary at: 71);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 20);
						synonyms: #('jump')).!

createConnections111to120: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 111)
		addConnection: (Connection new
						destination: (aDictionary at: 70);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 50);
						entryProbability: 0.40;
						synonyms: #('down' 'jump' 'climb'));
		addConnection: (Connection new
						destination: (aDictionary at: 53);
						entryProbability: 0.50;
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 45);
						synonyms: #('down')).

	(aDictionary at: 112)
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 140);
						synonyms: #('south')).

	(aDictionary at: 113)
		addConnection: (Connection new
						destination: (aDictionary at: 109);
						synonyms: #('south' 'out')).

	(aDictionary at: 114)
		addConnection: (Connection new
						destination: (aDictionary at: 84);
						synonyms: #('southeast')).

	(aDictionary at: 115)
		addConnection: (Connection new
						destination: (aDictionary at: 116);
						synonyms: #('southwest')).

	(aDictionary at: 116)
		addConnection: (Connection new
						destination: (aDictionary at: 115);
						synonyms: #('northeast')).

	(aDictionary at: 117)
		addConnection: (Connection new
						destination: (aDictionary at: 118);
						synonyms: #('southwest'));
		addConnection: (Bridge new
						destination: (aDictionary at: 122);
						synonyms: #('over' 'across' 'northeast' 'cross'));
		addConnection: (Connection new
						destination: (aDictionary at: 144);
						synonyms: #('jump')).

	(aDictionary at: 118)
		addConnection: (Connection new
						destination: (aDictionary at: 72);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 117);
						synonyms: #('up')).

	"(aDictionary at: 119)
		addConnection: (Connection new
						destination: (aDictionary at: 69);
						synonyms: #('north' 'out'))."

	(aDictionary at: 120)
		addConnection: (Connection new
						destination: (aDictionary at: 69);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 74);
						synonyms: #('east')).!

createConnections11to20: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 11)
		addConnection: (Connection new
						destination: (aDictionary at: 9);
						synonyms: #('entrance'));
		addConnection: (Connection new
						destination: (aDictionary at: 10);
						synonyms: #('crawl' 'cobblestone' 'passage' 'low' 'east'));
		addConnection: (Connection new
						destination: (aDictionary at: 12);
						synonyms: #('canyon' 'in' 'up' 'west'));
		addConnection: (Connection new
						destination: (aDictionary at: 3);
						synonyms: #('xyzzy'));
		addConnection: (Connection new
						destination: (aDictionary at: 14);
						synonyms: #('pit')).

	(aDictionary at: 12)
		addConnection: (Connection new
						destination: (aDictionary at: 9);
						synonyms: #('entrance'));
		addConnection: (Connection new
						destination: (aDictionary at: 11);
						synonyms: #('down' 'east' 'debris'));
		addConnection: (Connection new
						destination: (aDictionary at: 13);
						synonyms: #('in' 'up' 'west'));
		addConnection: (Connection new
						destination: (aDictionary at: 14);
						synonyms: #('pit')).

	(aDictionary at: 13)
		addConnection: (Connection new
						destination: (aDictionary at: 9);
						synonyms: #('entrance'));
		addConnection: (Connection new
						destination: (aDictionary at: 11);
						synonyms: #('debris'));
		addConnection: (Connection new
						destination: (aDictionary at: 12);
						synonyms: #('canyon' 'east'));
		addConnection: (Connection new
						destination: (aDictionary at: 14);
						synonyms: #('passage' 'pit' 'west')).

	(aDictionary at: 14)
		addConnection: (Connection new
						destination: (aDictionary at: 9);
						synonyms: #('entrance'));
		addConnection: (Connection new
						destination: (aDictionary at: 11);
						synonyms: #('debris'));
		addConnection: (Connection new
						destination: (aDictionary at: 13);
						synonyms: #('passage' 'east'));
		addConnection: (Connection new
						destination: (aDictionary at: 15);
						openDescription: 'Rough stone steps lead down the pit';
						synonyms: #('down' 'pit'));
		addConnection: (Connection new
						destination: (aDictionary at: 16);
						synonyms: #('crack' 'west')).

	(aDictionary at: 15)
		addConnection: (Connection new
						destination: (aDictionary at: 18);
						synonyms: #('left' 'south'));
		addConnection: (Connection new
						destination: (aDictionary at: 17);
						synonyms: #('forward' 'hall' 'west'));
		addConnection: (Connection new
						destination: (aDictionary at: 19);
						synonyms: #('stairs' 'down' 'north'));
		addConnection: (Connection new
						destination: (aDictionary at: 14);
						openDescription: 'Rough stone steps lead up the dome';
						linkedConnection: ((aDictionary at: 14) connectionTo: 'down');
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 34);
						synonyms: #('y2')).

	((aDictionary at: 14) connectionTo: 'down')
		linkedConnection: ((aDictionary at: 15) connectionTo: 'up').

	steps := ((aDictionary at: 14) connectionTo: 'down').

	(aDictionary at: 16)
		addConnection: (Connection new
						destination: (aDictionary at: 14);
						synonyms: #('auto')).

	(aDictionary at: 17)
		addConnection: (Connection new
						destination: (aDictionary at: 15);
						synonyms: #('hall' 'east'));
		addConnection: (Connection new
						destination: (aDictionary at: 27);
						synonyms: #('forward' 'over' 'across' 'west' 'cross');
						open: false;
						openDescription: 'A crystal bridge now spans the fissure.').

	(aDictionary at: 18)
		addConnection: (Connection new
						destination: (aDictionary at: 15);
						synonyms: #('hall' 'out' 'north')).

	(aDictionary at: 19)
		addConnection: (Connection new
						destination: (aDictionary at: 15);
						synonyms: #('stairs' 'up' 'east'));
		addConnection: (Connection new
						destination: (aDictionary at: 28);
						synonyms: #('north' 'left'));
		addConnection: (Connection new
						destination: (aDictionary at: 29);
						synonyms: #('south' 'right'));
		addConnection: (Connection new
						destination: (aDictionary at: 30);
						synonyms: #('west' 'forward'));
		addConnection: (Connection new
						destination: (aDictionary at: 74);
						synonyms: #('southwest');
						entryProbability: 0.35);
		addConnection: (Connection new
						destination: (aDictionary at: 74);
						synonyms: #('secret'))!

createConnections121to130: aDictionary
	"Private - create connections between locations in this world."

	"(aDictionary at: 121)
		addConnection: (Connection new
						destination: (aDictionary at: 74);
						synonyms: #('east' 'out'))."

	(aDictionary at: 122)
		addConnection: (Connection new
						destination: (aDictionary at: 123);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 124);
						synonyms: #('fork'));
		addConnection: (Connection new
						destination: (aDictionary at: 126);
						synonyms: #('view'));
		addConnection: (Connection new
						destination: (aDictionary at: 129);
						synonyms: #('barren'));
		addConnection: (Connection new
						destination: (aDictionary at: 117);
						synonyms: #('southwest')).

	(aDictionary at: 123)
		addConnection: (Connection new
						destination: (aDictionary at: 143);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 124);
						synonyms: #('east' 'fork'));
		addConnection: (Connection new
						destination: (aDictionary at: 126);
						synonyms: #('view'));
		addConnection: (Connection new
						destination: (aDictionary at: 129);
						synonyms: #('barren')).

	(aDictionary at: 124)
		addConnection: (Connection new
						destination: (aDictionary at: 123);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 125);
						synonyms: #('northeast' 'left'));
		addConnection: (Connection new
						destination: (aDictionary at: 128);
						synonyms: #('southeast' 'right' 'down'));
		addConnection: (Connection new
						destination: (aDictionary at: 126);
						synonyms: #('view'));
		addConnection: (Connection new
						destination: (aDictionary at: 129);
						synonyms: #('barren')).

	(aDictionary at: 125)
		addConnection: (Connection new
						destination: (aDictionary at: 124);
						synonyms: #('south' 'fork'));
		addConnection: (Connection new
						destination: (aDictionary at: 126);
						synonyms: #('north' 'view'));
		addConnection: (Connection new
						destination: (aDictionary at: 127);
						synonyms: #('east' 'crawl')).

	(aDictionary at: 126)
		addConnection: (Connection new
						destination: (aDictionary at: 125);
						synonyms: #('south' 'passage' 'out'));
		addConnection: (Connection new
						destination: (aDictionary at: 124);
						synonyms: #('fork')).

	(aDictionary at: 127)
		addConnection: (Connection new
						destination: (aDictionary at: 125);
						synonyms: #('west' 'out' 'crawl'));
		addConnection: (Connection new
						destination: (aDictionary at: 124);
						synonyms: #('fork'));
		addConnection: (Connection new
						destination: (aDictionary at: 126);
						synonyms: #('view')).

	(aDictionary at: 128)
		addConnection: (Connection new
						destination: (aDictionary at: 124);
						synonyms: #('north' 'up' 'fork'));
		addConnection: (Connection new
						destination: (aDictionary at: 129);
						synonyms: #('south' 'down' 'barren'));
		addConnection: (Connection new
						destination: (aDictionary at: 126);
						synonyms: #('view')).

	(aDictionary at: 129)
		addConnection: (Connection new
						destination: (aDictionary at: 128);
						synonyms: #('west' 'up'));
		addConnection: (Connection new
						destination: (aDictionary at: 124);
						synonyms: #('fork'));
		addConnection: (Connection new
						destination: (aDictionary at: 130);
						synonyms: #('east' 'in' 'barren' 'enter'));
		addConnection: (Connection new
						destination: (aDictionary at: 126);
						synonyms: #('view')).

	(aDictionary at: 130)
		addConnection: (Connection new
						destination: (aDictionary at: 129);
						synonyms: #('west' 'exit'));
		addConnection: (Connection new
						destination: (aDictionary at: 124);
						synonyms: #('fork'));
		addConnection: (Connection new
						destination: (aDictionary at: 126);
						synonyms: #('view')).!

createConnections131to140: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 131)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('east')).

	(aDictionary at: 132)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('southeast')).

	(aDictionary at: 133)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('south')).

	(aDictionary at: 134)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('southwest')).

	(aDictionary at: 135)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('up')).

	(aDictionary at: 136)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('north')).

	(aDictionary at: 137)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('west')).

	(aDictionary at: 138)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 139);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('northwest')).

	(aDictionary at: 139)
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 131);
						synonyms: #('northwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 132);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 133);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 134);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 135);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 136);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 137);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 138);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('northeast')).

	(aDictionary at: 140)
		addConnection: (Connection new
						destination: (aDictionary at: 112);
						synonyms: #('north' 'out')).!

createConnections141to150: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 143)
		addConnection: (Bridge new
						destination: (aDictionary at: 145);
						synonyms: #('southwest' 'over' 'across' 'cross'));
		addConnection: (Connection new
						destination: (aDictionary at: 144);
						synonyms: #('jump')).

	(aDictionary at: 145)
		addConnection: (Connection new
						destination: (aDictionary at: 118);
						synonyms: #('southwest')).!

createConnections1to10: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 1)
		addConnection: (Connection new
						destination: (aDictionary at: 2);
						synonyms: #('west' 'up' 'hill'));
		addConnection: (Connection new
						destination: (aDictionary at: 3);
						synonyms: #('east' 'in' 'enter' 'building'));
		addConnection: (Connection new
						destination: (aDictionary at: 4);
						synonyms: #('south' 'down' 'gully' 'down'));
		addConnection: (Connection new
						destination: (aDictionary at: 5);
						synonyms: #('north' 'forest'));
		addConnection: (Connection new
						destination: (aDictionary at: 8);
						synonyms: #('depression')).

	(aDictionary at: 2)
		addConnection: (Connection new
						destination: (aDictionary at: 1);
						synonyms: #('forward' 'east' 'north' 'down' 'hill' 'building'));
		addConnection: (Connection new
						destination: (aDictionary at: 5);
						synonyms: #('south' 'forest')).

	(aDictionary at: 3)
		addConnection: (Door new
						destination: (aDictionary at: 1);
						synonyms: #('exit' 'out' 'outdoors' 'west'));
		addConnection: (Connection new
						destination: (aDictionary at: 11);
						synonyms: #('xyzzy'));
		addConnection: (Connection new
						destination: (aDictionary at: 33);
						synonyms: #('plugh'));
		addConnection: (Connection new
						destination: (aDictionary at: 79);
						synonyms: #('stream')).

	(aDictionary at: 4)
		addConnection: (Connection new
						destination: (aDictionary at: 1);
						synonyms: #('downstream' 'building' 'north'));
		addConnection: (Connection new
						destination: (aDictionary at: 5);
						synonyms: #('forest' 'east' 'west' 'up'));
		addConnection: (Connection new
						destination: (aDictionary at: 7);
						synonyms: #('south' 'down' 'depression')).

	(aDictionary at: 5)
		addConnection: (Connection new
						destination: (aDictionary at: 4);
						synonyms: #('valley' 'east' 'down'));
		addConnection: (Connection new
						destination: (aDictionary at: 5);
						entryProbability: 0.50;
						synonyms: #('forest' 'forward' 'north'));
		addConnection: (Connection new
						destination: (aDictionary at: 6);
						synonyms: #('forest'));
		addConnection: (Connection new
						destination: (aDictionary at: 5);
						synonyms: #('west' 'south')).

	(aDictionary at: 6)
		addConnection: (Connection new
						destination: (aDictionary at: 1);
						synonyms: #('hill' 'north'));
		addConnection: (Connection new
						destination: (aDictionary at: 4);
						synonyms: #('valley' 'east' 'west' 'down'));
		addConnection: (Connection new
						destination: (aDictionary at: 5);
						synonyms: #('forest' 'south')).

	(aDictionary at: 7)
		addConnection: (Connection new
						destination: (aDictionary at: 1);
						synonyms: #('building'));
		addConnection: (Connection new
						destination: (aDictionary at: 4);
						synonyms: #('downstream' 'north'));
		addConnection: (Connection new
						destination: (aDictionary at: 5);
						synonyms: #('forest' 'east' 'west'));
		addConnection: (Connection new
						destination: (aDictionary at: 8);
						synonyms: #('rock' 'bed' 'south')).

	(aDictionary at: 8)
		addConnection: (Connection new
						destination: (aDictionary at: 5);
						synonyms: #('forest' 'east' 'south' 'west'));
		addConnection: (Connection new
						destination: (aDictionary at: 1);
						synonyms: #('building'));
		addConnection: (Connection new
						destination: (aDictionary at: 7);
						synonyms: #('downstream' 'gully' 'north'));
		addConnection: (Connection new
						destination: (aDictionary at: 9);
						closeable: true;
						open: false;
						locked: true;
						lockedDescription: 'The grate is locked';
						closedDescription: 'The grate is closed';
						openDescription: 'The grate is open';
						synonyms: #('grate' 'enter' 'in' 'down')).

	(aDictionary at: 9)
		addConnection: (Connection new
						destination: (aDictionary at: 10);
						synonyms: #('crawl' 'cobblestone' 'in' 'west'));
		addConnection: (Connection new
						destination: (aDictionary at: 14);
						synonyms: #('pit'));
		addConnection: (Connection new
						destination: (aDictionary at: 8);
						closeable: true;
						open: false;
						locked: true;
						lockedDescription: 'The grate is locked';
						closedDescription: 'The grate is closed';
						openDescription: 'The grate is open';
						synonyms: #('up' 'out' 'grate');
						linkedConnection: ((aDictionary at: 8) connectionTo: 'grate'));
		addConnection: (Connection new
						destination: (aDictionary at: 11);
						synonyms: #('debris')).

	((aDictionary at: 8) connectionTo: 'grate')
		linkedConnection: ((aDictionary at: 9) connectionTo: 'grate').

	(aDictionary at: 10)
		addConnection: (Connection new
						destination: (aDictionary at: 9);
						synonyms: #('out' 'surface' 'nowhere' 'east'));
		addConnection: (Connection new
						destination: (aDictionary at: 11);
						synonyms: #('in' 'dark' 'west' 'debris'));
		addConnection: (Connection new
						destination: (aDictionary at: 14);
						synonyms: #('pit')).!

createConnections21to30: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 22)
		addConnection: (Connection new
						destination: (aDictionary at: 15);
						synonyms: #('auto')).

	(aDictionary at: 23)
		addConnection: (Connection new
						destination: (aDictionary at: 67);
						synonyms: #('east' 'across'));
		addConnection: (Connection new
						destination: (aDictionary at: 68);
						synonyms: #('west' 'slab'));
		addConnection: (Connection new
						destination: (aDictionary at: 25);
						synonyms: #('down' 'pit')).

	(aDictionary at: 24)
		addConnection: (Connection new
						destination: (aDictionary at: 67);
						synonyms: #('up' 'out')).

	(aDictionary at: 25)
		addConnection: (Connection new
						destination: (aDictionary at: 23);
						synonyms: #('up' 'out'));
		addConnection: (Connection new
						destination: (aDictionary at: 26);
						synonyms: #('climb')).

	(aDictionary at: 26)
		addConnection: (Connection new
						destination: (aDictionary at: 88);
						synonyms: #('auto')).

	(aDictionary at: 27)
		addConnection: (Connection new
						destination: (aDictionary at: 17);
						synonyms: #('over' 'across' 'east' 'cross');
						open: false;
						openDescription: 'A crystal bridge now spans the fissure.';
						linkedConnection: (((aDictionary at: 17) connectionsTo: 'west') at: 1));
		addConnection: (Connection new
						destination: (aDictionary at: 40);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 41);
						synonyms: #('west')).

	((self eastBankOfFissure connectionsTo: 'west') at: 1)
		linkedConnection: ((self westBankOfFissure connectionsTo: 'east') at: 1).

	(aDictionary at: 28)
		addConnection: (Connection new
						destination: (aDictionary at: 19);
						synonyms: #('hall' 'out' 'south'));
		addConnection: (Connection new
						destination: (aDictionary at: 33);
						synonyms: #('north' 'y2'));
		addConnection: (Connection new
						destination: (aDictionary at: 36);
						synonyms: #('down' 'hole')).

	(aDictionary at: 29)
		addConnection: (Connection new
						destination: (aDictionary at: 19);
						synonyms: #('hall' 'out' 'north')).

	(aDictionary at: 30)
		addConnection: (Connection new
						destination: (aDictionary at: 19);
						synonyms: #('hall' 'out' 'east'));
		addConnection: (Connection new
						destination: (aDictionary at: 62);
						synonyms: #('west' 'up')).!

createConnections31to40: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 31)
		addConnection: (Connection new
						destination: (aDictionary at: 90);
						synonyms: #('auto')).

	(aDictionary at: 32)
		addConnection: (Connection new
						destination: (aDictionary at: 19);
						synonyms: #('auto')).

	(aDictionary at: 33)
		addConnection: (Connection new
						destination: (aDictionary at: 3);
						synonyms: #('plugh'));
		addConnection: (Connection new
						destination: (aDictionary at: 28);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 34);
						synonyms: #('east' 'wall' 'broken'));
		addConnection: (Connection new
						destination: (aDictionary at: 35);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 100);
						synonyms: #('plover')).

	(aDictionary at: 34)
		addConnection: (Connection new
						destination: (aDictionary at: 33);
						synonyms: #('down' 'y2'));
		addConnection: (Connection new
						destination: (aDictionary at: 15);
						synonyms: #('up')).

	(aDictionary at: 35)
		addConnection: (Connection new
						destination: (aDictionary at: 33);
						synonyms: #('east' 'y2'));
		addConnection: (Connection new
						destination: (aDictionary at: 20);
						synonyms: #('jump')).

	(aDictionary at: 36)
		addConnection: (Connection new
						destination: (aDictionary at: 37);
						synonyms: #('east' 'crawl'));
		addConnection: (Connection new
						destination: (aDictionary at: 28);
						synonyms: #('up' 'hole'));
		addConnection: (Connection new
						destination: (aDictionary at: 39);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('bedquilt')).

	(aDictionary at: 37)
		addConnection: (Connection new
						destination: (aDictionary at: 36);
						synonyms: #('west' 'crawl'));
		addConnection: (Connection new
						destination: (aDictionary at: 38);
						synonyms: #('down' 'pit' 'climb')).

	(aDictionary at: 38)
		addConnection: (Connection new
						destination: (aDictionary at: 37);
						synonyms: #('climb' 'up' 'out')).

	(aDictionary at: 39)
		addConnection: (Connection new
						destination: (aDictionary at: 36);
						synonyms: #('east' 'passage'));
		addConnection: (Connection new
						destination: (aDictionary at: 64);
						synonyms: #('down' 'hole' 'floor'));
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('bedquilt')).

	(aDictionary at: 40)
		addConnection: (Connection new
						destination: (aDictionary at: 41);
						synonyms: #('auto')).!

createConnections41to50: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 41)
		addConnection: (Connection new
						destination: (aDictionary at: 42);
						synonyms: #('south' 'up' 'passage' 'climb'));
		addConnection: (Connection new
						destination: (aDictionary at: 27);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 59);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 60);
						synonyms: #('west' 'crawl')).

	(aDictionary at: 42)
		addConnection: (Connection new
						destination: (aDictionary at: 41);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 42);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 43);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 45);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 80);
						synonyms: #('west')).

	(aDictionary at: 43)
		addConnection: (Connection new
						destination: (aDictionary at: 42);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 44);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 45);
						synonyms: #('east')).

	(aDictionary at: 44)
		addConnection: (Connection new
						destination: (aDictionary at: 43);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 48);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 50);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 82);
						synonyms: #('north')).

	(aDictionary at: 45)
		addConnection: (Connection new
						destination: (aDictionary at: 42);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 43);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 46);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 47);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 87);
						synonyms: #('up' 'down')).

	(aDictionary at: 46)
		addConnection: (Connection new
						destination: (aDictionary at: 45);
						synonyms: #('up' 'out')).

	(aDictionary at: 47)
		addConnection: (Connection new
						destination: (aDictionary at: 45);
						synonyms: #('east' 'out')).

	(aDictionary at: 48)
		addConnection: (Connection new
						destination: (aDictionary at: 44);
						synonyms: #('up' 'out')).

	(aDictionary at: 49)
		addConnection: (Connection new
						destination: (aDictionary at: 50);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 51);
						synonyms: #('west')).

	(aDictionary at: 50)
		addConnection: (Connection new
						destination: (aDictionary at: 44);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 49);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 51);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 52);
						synonyms: #('south')).
!

createConnections51to60: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 51)
		addConnection: (Connection new
						destination: (aDictionary at: 49);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 50);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 52);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 53);
						synonyms: #('south')).

	(aDictionary at: 52)
		addConnection: (Connection new
						destination: (aDictionary at: 50);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 51);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 52);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 53);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 55);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 86);
						synonyms: #('down')).

	(aDictionary at: 53)
		addConnection: (Connection new
						destination: (aDictionary at: 51);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 52);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 54);
						synonyms: #('south')).

	(aDictionary at: 54)
		addConnection: (Connection new
						destination: (aDictionary at: 53);
						synonyms: #('west' 'out')).

	(aDictionary at: 55)
		addConnection: (Connection new
						destination: (aDictionary at: 52);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 55);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 56);
						synonyms: #('down'));
		addConnection: (Connection new
						destination: (aDictionary at: 57);
						synonyms: #('east')).

	(aDictionary at: 56)
		addConnection: (Connection new
						destination: (aDictionary at: 55);
						synonyms: #('up' 'out')).

	(aDictionary at: 57)
		addConnection: (Connection new
						destination: (aDictionary at: 13);
						synonyms: #('down' 'climb'));
		addConnection: (Connection new
						destination: (aDictionary at: 55);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 58);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 83);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 84);
						synonyms: #('east')).

	(aDictionary at: 58)
		addConnection: (Connection new
						destination: (aDictionary at: 57);
						synonyms: #('east' 'out')).

	(aDictionary at: 59)
		addConnection: (Connection new
						destination: (aDictionary at: 27);
						synonyms: #('auto')).

	(aDictionary at: 60)
		addConnection: (Connection new
						destination: (aDictionary at: 41);
						synonyms: #('east' 'up' 'crawl'));
		addConnection: (Connection new
						destination: (aDictionary at: 61);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 62);
						synonyms: #('north' 'down' 'hole')).!

createConnections61to70: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 61)
		addConnection: (Connection new
						destination: (aDictionary at: 60);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 62);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 107);
						synonyms: #('south')).

	(aDictionary at: 62)
		addConnection: (Connection new
						destination: (aDictionary at: 60);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 63);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 30);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 61);
						synonyms: #('south')).

	(aDictionary at: 63)
		addConnection: (Connection new
						destination: (aDictionary at: 62);
						synonyms: #('south' 'out')).

	(aDictionary at: 64)
		addConnection: (Connection new
						destination: (aDictionary at: 39);
						synonyms: #('up' 'climb' 'room'));
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('west' 'bedquilt'));
		addConnection: (Connection new
						destination: (aDictionary at: 103);
						synonyms: #('north' 'shell'));
		addConnection: (Connection new
						destination: (aDictionary at: 106);
						synonyms: #('east')).

	(aDictionary at: 65)
		addConnection: (Connection new
						destination: (aDictionary at: 64);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 66);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 68);
						synonyms: #('slab'));
		addConnection: (Connection new
						destination: (aDictionary at: 70);
						entryProbability: 0.50;
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 39);
						synonyms: #('up'));
		addConnection: (Connection new
						destination: (aDictionary at: 72);
						entryProbability: 0.75;
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 71);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 106);
						synonyms: #('down')).

	(aDictionary at: 66)
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('northeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 67);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 77);
						synonyms: #('canyon'));
		addConnection: (Connection new
						destination: (aDictionary at: 96);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 97);
						synonyms: #('oriental')).

	(aDictionary at: 67)
		addConnection: (Connection new
						destination: (aDictionary at: 66);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 23);
						synonyms: #('west' 'across'));
		addConnection: (Connection new
						destination: (aDictionary at: 24);
						synonyms: #('down' 'pit')).

	(aDictionary at: 68)
		addConnection: (Connection new
						destination: (aDictionary at: 23);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 69);
						synonyms: #('up' 'climb'));
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('north')).

	(aDictionary at: 69)
		addConnection: (Connection new
						destination: (aDictionary at: 68);
						synonyms: #('down' 'slab'));
		addConnection: (Connection new
						destination: (aDictionary at: 120);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 109);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 113);
						synonyms: #('reservoir')).

	(aDictionary at: 70)
		addConnection: (Connection new
						destination: (aDictionary at: 71);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('down' 'passage'));
		addConnection: (Connection new
						destination: (aDictionary at: 111);
						synonyms: #('south')).!

createConnections71to80: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 71)
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 70);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 110);
						synonyms: #('north')).

	(aDictionary at: 72)
		addConnection: (Connection new
						destination: (aDictionary at: 65);
						synonyms: #('bedquilt'));
		addConnection: (Connection new
						destination: (aDictionary at: 118);
						synonyms: #('southwest'));
		addConnection: (Connection new
						destination: (aDictionary at: 73);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 97);
						synonyms: #('southeast' 'oriental')).

	(aDictionary at: 73)
		addConnection: (Connection new
						destination: (aDictionary at: 72);
						synonyms: #('south' 'crawl' 'out')).

	(aDictionary at: 74)
		addConnection: (Connection new
						destination: (aDictionary at: 19);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 120);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 75);
						synonyms: #('down')).

	(aDictionary at: 75)
		addConnection: (Connection new
						destination: (aDictionary at: 76);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 77);
						synonyms: #('north')).

	(aDictionary at: 76)
		addConnection: (Connection new
						destination: (aDictionary at: 75);
						synonyms: #('north')).

	(aDictionary at: 77)
		addConnection: (Connection new
						destination: (aDictionary at: 75);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 78);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 66);
						synonyms: #('north' 'crawl')).

	(aDictionary at: 78)
		addConnection: (Connection new
						destination: (aDictionary at: 77);
						synonyms: #('south')).

	(aDictionary at: 79)
		addConnection: (Connection new
						destination: (aDictionary at: 3);
						synonyms: #('auto')).

	(aDictionary at: 80)
		addConnection: (Connection new
						destination: (aDictionary at: 42);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 80);
						synonyms: #('west' 'south'));
		addConnection: (Connection new
						destination: (aDictionary at: 81);
						synonyms: #('east')).!

createConnections81to90: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 81)
		addConnection: (Connection new
						destination: (aDictionary at: 80);
						synonyms: #('west' 'out')).

	(aDictionary at: 82)
		addConnection: (Connection new
						destination: (aDictionary at: 44);
						synonyms: #('south' 'out')).

	(aDictionary at: 83)
		addConnection: (Connection new
						destination: (aDictionary at: 57);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 84);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 85);
						synonyms: #('west')).

	(aDictionary at: 84)
		addConnection: (Connection new
						destination: (aDictionary at: 57);
						synonyms: #('north'));
		addConnection: (Connection new
						destination: (aDictionary at: 83);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 114);
						synonyms: #('northwest')).

	(aDictionary at: 85)
		addConnection: (Connection new
						destination: (aDictionary at: 83);
						synonyms: #('east' 'out')).

	(aDictionary at: 86)
		addConnection: (Connection new
						destination: (aDictionary at: 52);
						synonyms: #('up' 'out')).

	(aDictionary at: 87)
		addConnection: (Connection new
						destination: (aDictionary at: 45);
						synonyms: #('up' 'down')).

	(aDictionary at: 88)
		addConnection: (Connection new
						destination: (aDictionary at: 25);
						synonyms: #('down' 'climb' 'east'));
		addConnection: (Connection new
						destination: (aDictionary at: 20);
						synonyms: #('jump'));
		addConnection: (Connection new
						destination: (aDictionary at: 92);
						synonyms: #('west' 'giant')).

	(aDictionary at: 89)
		addConnection: (Connection new
						destination: (aDictionary at: 25);
						synonyms: #('auto')).

	(aDictionary at: 90)
		addConnection: (Connection new
						destination: (aDictionary at: 23);
						synonyms: #('auto')).!

createConnections91to100: aDictionary
	"Private - create connections between locations in this world."

	(aDictionary at: 91)
		addConnection: (Connection new
						destination: (aDictionary at: 95);
						synonyms: #('north' 'cavern' 'passage'));
		addConnection: (Connection new
						destination: (aDictionary at: 72);
						synonyms: #('down' 'climb')).

	(aDictionary at: 92)
		addConnection: (Connection new
						destination: (aDictionary at: 88);
						synonyms: #('south'));
		addConnection: (Connection new
						destination: (aDictionary at: 93);
						synonyms: #('east'));
		addConnection: (Connection new
						destination: (aDictionary at: 94);
						synonyms: #('north')).

	(aDictionary at: 93)
		addConnection: (Connection new
						destination: (aDictionary at: 92);
						synonyms: #('south' 'giant' 'out')).

	(aDictionary at: 94)
		addConnection: (Connection new
						destination: (aDictionary at: 92);
						synonyms: #('south' 'giant' 'passage'));
		addConnection: (Connection new
						destination: (aDictionary at: 95);
						synonyms: #('north' 'enter' 'cavern' 'door');
						locked: true;
						closed: true;
						lockedDescription: 'The way north is barred by a massive, rusty, iron door.';
						closedDescription: 'The way north leads through a massive, rusty, iron door.';
						openDescription: 'The massive, rusty, iron door is open').

	self door: (((aDictionary at: 94) connectionsTo: 'north') at: 1).

	(aDictionary at: 95)
		addConnection: (Connection new
						destination: (aDictionary at: 94);
						synonyms: #('south' 'out'));
		addConnection: (Connection new
						destination: (aDictionary at: 92);
						synonyms: #('giant'));
		addConnection: (Connection new
						destination: (aDictionary at: 91);
						synonyms: #('west')).

	(aDictionary at: 96)
		addConnection: (Connection new
						destination: (aDictionary at: 66);
						synonyms: #('west' 'out')).

	(aDictionary at: 97)
		addConnection: (Connection new
						destination: (aDictionary at: 66);
						synonyms: #('southeast'));
		addConnection: (Connection new
						destination: (aDictionary at: 72);
						synonyms: #('west' 'crawl'));
		addConnection: (Connection new
						destination: (aDictionary at: 98);
						synonyms: #('up' 'north' 'cavern')).

	(aDictionary at: 98)
		addConnection: (Connection new
						destination: (aDictionary at: 97);
						synonyms: #('south' 'oriental'));
		addConnection: (Connection new
						destination: (aDictionary at: 99);
						synonyms: #('west')).

	(aDictionary at: 99)
		addConnection: (Connection new
						destination: (aDictionary at: 98);
						synonyms: #('northwest' 'cavern'));
		addConnection: (Connection new
						destination: (aDictionary at: 100);
						synonyms: #('east' 'passage')).

	(aDictionary at: 100)
		addConnection: (Connection new
						destination: (aDictionary at: 99);
						synonyms: #('west'));
		addConnection: (Connection new
						destination: (aDictionary at: 33);
						synonyms: #('plover'));
		addConnection: (Connection new
						destination: (aDictionary at: 101);
						synonyms: #('northeast' 'dark')).!

createItems: aCollection
	"Private - create the items in the world"

	self food: (Item new
				article: 'some';
				longNounPhrase: 'tasty food';
				shortNounPhrase: 'food').

	self keys: (Item new
				longNounPhrase: 'set of keys';
				shortNounPhrase: 'set of keys').

	(aCollection at: 3)
		add: (LightSource new
				longNounPhrase: 'shiny brass lamp';
				shortNounPhrase: 'lamp');
		add: self keys;
		add: self food;
		add: (Item new
				longNounPhrase: 'small bottle';
				shortNounPhrase: 'bottle').

	(((aCollection at: 8) connectionsTo: 'grate') at: 1)
		key: (((aCollection at: 3) contentsLike: 'keys') at: 1).

	self cage: (Item new
				longNounPhrase: 'small wicker cage';
				shortNounPhrase: 'wicker cage').
	(aCollection at: 10) add: self cage.

	self rod: (Item new
				longNounPhrase: 'three-foot rod with a rusty star';
				shortNounPhrase: 'rod').
	(aCollection at: 11) add: self rod.

	self bird: (Item new
				longNounPhrase: 'cheerful little bird';
				shortNounPhrase: 'little bird').
	(aCollection at: 13) add: self bird.

	self nugget: (Treasure new
				longNounPhrase: 'large sparkling nugget of gold';
				shortNounPhrase: 'golden nugget';
				points: 50).
	(aCollection at: 18) add: self nugget.

	self snake: (ImmovableItem new
				longDescription: 'A huge green fierce snake bars the way!!').
	(aCollection at: 19) add: self snake.

	self bridge: ((self eastBankOfFissure connectionsTo: 'west') at: 1).

	self plant: Plant new.
	(aCollection at: 25) add: self plant.

	self silverBars: (Treasure new
				longNounPhrase: 'bars of silver';
				points: 50).
	(aCollection at: 28) add: self silverBars.

	self jewelry: (Treasure new
				longNounPhrase: 'precious jewelry';
				points: 50).
	(aCollection at: 29) add: self jewelry.

	self coins: (Treasure new
				longNounPhrase: 'collection of rare coins';
				shortNounPhrase: 'coin collection';
				points: 50).
	(aCollection at: 30) add: self coins.

	self mouse: (Item new
				longNounPhrase: 'cute little mouse').
	(aCollection at: 66) add: self mouse.

	self eggs: (Treasure new
				longNounPhrase: 'large nest full of golden eggs';
				shortNounPhrase: 'nest of golden eggs';
				points: 50).
	(aCollection at: 92) add: self eggs.

	self trident: (Treasure new
				longNounPhrase: 'jewel-encrusted trident';
				points: 50).
	(aCollection at: 95) add: self trident.

	self pillow: (Item new
				longNounPhrase: 'small velvet pillow').
	(aCollection at: 96) add: self pillow.

	self vase: (Treasure new
				longNounPhrase: 'delicate, precious, ming vase';
				shortNounPhrase: 'ming vase';
				points: 50).
	(aCollection at: 97) add: self vase.

	self emerald: (Treasure new
				longNounPhrase: 'emerald the size of a plover''s egg';
				shortNounPhrase: 'large emerald';
				points: 50).
	(aCollection at: 100) add: self emerald.

	self pyramid: (Treasure new
				longNounPhrase: '8 inch platinum pyramid';
				shortNounPhrase: 'platinum pyramid';
				points: 50).
	(aCollection at: 101) add: self pyramid.

	self tablet: (ImmovableItem new
				longDescription: 'A massive stone tablet embedded in the wall reads:',
							String lineDelimiter,
							'"Congratulations on bringing light into the dark-room!!"').
	(aCollection at: 101) add: self tablet.

	self clam: (Clam new
				world: self;
				beClam).
	(aCollection at: 103) add: self clam.

	self magazine: (Item new
					longNounPhrase: 'recent issues of "Spelunker Today" magazine';
					shortNounPhrase: 'magazines';
					article: 'some';
					plural: true;
					points: 1).
	(aCollection at: 106) add: self magazine.

	self troll: (ImmovableItem new
					longDescription: 'A burly troll stands by the bridge and insists you throw ',
								'him a treasure before you may cross.';
					shortDescription: 'The troll still wants a treasure').
	(aCollection at: 117) add: self troll.

	self rug: (Treasure new
					longNounPhrase: 'persian rug';
					points: 50).
	(aCollection at: 120) add: self rug.

	self dragon: (ImmovableItem new
					longDescription: 'A huge green fierce dragon bars the way!!').
	(aCollection at: 120) add: self dragon.

	self spices: (Treasure new
				longNounPhrase: 'rare spices';
				points: 50).
	(aCollection at: 127) add: self spices.

	self ferociousBear: (ImmovableItem new
					longDescription: 'There is a ferocious cave bear eyeing you from the far ',
								'end of the room!!').
	(aCollection at: 130) add: self ferociousBear.

	self docileBear: (Item new
					longDescription: 'There is a very large, tame bear here.').

	self chain: GoldenChain new.
	(aCollection at: 130) add: self chain.

	self pearl: (Pearl new
				longNounPhrase: 'glistening pearl';
				points: 50).
	self add: self pearl.!

createLocations
	"Private - create locations for this world."

	| locations |

	locations := Dictionary new.

	self
		createLocations1to10: locations;
		createLocations11to20: locations;
		createLocations21to30: locations;
		createLocations31to40: locations;
		createLocations41to50: locations;
		createLocations51to60: locations;
		createLocations61to70: locations;
		createLocations71to80: locations;
		createLocations81to90: locations;
		createLocations91to100: locations;
		createLocations101to110: locations;
		createLocations111to120: locations;
		createLocations121to130: locations;
		createLocations131to140: locations;
		createLocations141to150: locations.

	^locations!

createLocations101to110: aDictionary
	"Private - create locations 101 to 110"

	aDictionary
		at: 101 put: (DarkLocation new
				longDescription: 'You''re in the dark-room.  A corridor leading south is the only exit.';
				shortDescription: 'You''re in the dark-room.');
		at: 102 put: (DarkLocation new
				longDescription: 'You are in an arched hall.  A coral passage once continued up and east ',
							'from here, but is now blocked by debris.  The air smells of sea water.';
				shortDescription: 'You''re in arched hall.');
		at: 103 put: (DarkLocation new
				longDescription: 'You''re in a large room carved out of sedimentary rock.  ',
							'The floor and walls are littered with bits of shells ',
							'embedded in the stone.  A shallow passage proceeds ',
							'downward, and a somewhat steeper one leads up.  A low ',
							'hands and knees passage enters from the south.';
				shortDescription: 'You''re in the shell room.');
		at: 104 put: (DarkLocation new
				longDescription: 'You are in a long sloping corridor with ragged sharp walls.';
				shortDescription: 'You are in a long sloping corridor with ragged sharp walls.');
		at: 105 put: (DarkLocation new
				longDescription: 'You are in a cul-de-sac about eight feet across.';
				shortDescription: 'You are in a cul-de-sac about eight feet across.');
		at: 106 put: (DarkLocation new
				longDescription: 'You are in an anteroom leading to a large passage to the ',
							'east.  Small passages go west and up.  The remnants of ',
							'recent digging are evident.  A sign in midair here says: ',
							'"Cave under construction beyond this point." ',
							'"Proceed at your own risk." ',
							'"Witt construction company"';
				shortDescription: 'You''re in the anteroom.');
		at: 107 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all different.';
				shortDescription: 'You are in a maze of twisty little passages, all different.');
		at: 108 put: (DarkLocation new
				longDescription: 'You are at Witt''s end.  Passages lead off in ALL directions.';
				shortDescription: 'You''re at Witt''s end.');
		at: 109 put: (Location new
				longDescription: 'You are in a north/south canyon about 25 feet across.  The ',
							'floor is covered by white mist seeping in from the north.  ',
							'The walls extend upward for well over 100 feet.  Suspended ',
							'from some unseen point far above you, an enormous two-',
							'sided mirror is hanging parallel to and midway between ',
							'the canyon walls.  (The mirror is obviously provided ',
							'for the use of the dwarves who, as you know, are ',
							'extremely vain.)  A small window can be seen in either ',
							'wall, some fifty feet up.';
				shortDescription: 'You''re in mirror canyon.');
		at: 110 put: (Location new
				longDescription: 'You''re at a low window overlooking a huge pit, which ',
							'extends up out of sight.  A floor is indistinctly visible ',
							'over 50 feet below.  Traces of white mist cover the floor ',
							'of the pit, becoming thicker to the left.  Marks in the ',
							'dust around the window would seem to indicate that ',
							'someone has been here recently.  Directly across the pit ',
							'from you and 25 feet away there is a similar window ',
							'looking into a lighted room.  A shadowy figure can be seen ',
							'there peering back at you.';
				shortDescription: 'You''re at a window on the pit.').

	self culDeSac: (aDictionary at: 105).
	self wittsEnd: (aDictionary at: 108).!

createLocations111to120: aDictionary
	"Private - create locations 111 to 120"

	aDictionary
		at: 111 put: (DarkLocation new
				longDescription: 'A large stalactite extends from the roof and almost reaches ',
							'the floor below.  You could climb down it, and jump from ',
							'it to the floor, but having done so you would be unable to ',
							'reach it to climb back up.';
				shortDescription: 'You''re at the top of the stalactite.');
		at: 112 put: (DarkLocation new
				longDescription: 'You are in a little maze of twisting passages, all different.';
				shortDescription: 'You are in a little maze of twisting passages, all different.');
		at: 113 put: (DarkLocation new
				longDescription: 'You are at the edge of a large underground reservoir.  An ',
							'opaque cloud of white mist fills the room and rises ',
							'rapidly upward.  The lake is fed by a stream which tumbles ',
							'out of a hole in the wall about 10 feet overhead and ',
							'splashes noisily into the water somewhere within the mist.  ',
							'The only passage goes back toward the south.';
				shortDescription: 'You''re at the reservoir.';
				hasWater: true);
		at: 114 put: (DarkLocation new
				longDescription: 'Dead end.';
				shortDescription: 'Dead end.');
		at: 115 put: (Location new
				longDescription: 'You are at the northeast end of an immense room, even ',
							'larger than the giant room.  It appears to be a repository ',
							'for the "adventure" program.  Massive torches far overhead ',
							'bathe the room with smoky yellow light.  Scattered about ',
							'you can be seen a pile of bottles (all of them empty), a ',
							'nursery of young beanstalks murmuring quietly, a bed of ',
							'oysters, a bundle of black rods with rusty stars on their ',
							'ends, and a collection of brass lanterns.  Off to one side ',
							'a great many Dwarves are sleeping on the floor, snoring ',
							'loudly.  A sign nearby reads:  ',
							'"Do NOT disturb the Dwarves!!"  ',
							'An immense mirror is hanging against one wall, and ',
							'stretches to the other end of the room, where various ',
							'other sundry objects can be glimpsed dimly in the distance.';
				shortDescription: 'You''re at the northeast end of the repository.');
		at: 116 put: (Location new
				longDescription: 'You are at the southwest end of the repository.  To one ',
							'side is a pit full of fierce green snakes.  On the other ',
							'side is a row of small wicker cages, each of which contains ',
							'a little sulking bird.  In one corner is a bundle of ',
							'black rods with rusty marks on their ends.  A large ',
							'number of velvet pillows are scattered about on the floor.  ',
							'A vast mirror stretches off to the northeast.  At your ',
							'feet is a large steel grate, next to which is a sign ',
							'which reads:  ',
							'"Treasure vault.  Keys in main office."';
				shortDescription: 'You''re at the sourthwest end of the repository.');
		at: 117 put: (DarkLocation new
				longDescription: 'You are on one side of a large deep chasm.  A heavy white ',
							'mist rising up from below obscures all view of the far ',
							'side.  A sw path leads away from the chasm into a winding ',
							'corridor.';
				shortDescription: 'You''re on the southwest side of the chasm.');
		at: 118 put: (DarkLocation new
				longDescription: 'You are in a long winding corridor sloping out of sight ',
							'in both directions.';
				shortDescription: 'You''re in sloping corridor.');
		"at: 119 put: (DarkLocation new
				longDescription: 'You are in a secret canyon which exits to the north and east.';
				shortDescription: 'You are in a secret canyon which exits to the north and east.');"
		at: 120 put: (SecretCanyon new
				longDescription: 'You are in a secret canyon which exits to the north and east.';
				shortDescription: 'You are in a secret canyon which exits to the north and east.').

	self swChasm: (aDictionary at: 117).
	self secretCanyon: (aDictionary at: 120)!

createLocations11to20: aDictionary
	"Private - create location 11 to 20"

	aDictionary
		at: 11 put: (DarkLocation new
				longDescription: 'You are in a debris room filled with stuff ',
							'washed in from the surface.  A low wide ',
							'passage with cobbles becomes plugged ',
							'with mud and debris here, but an awkward ',
							'canyon leads upward and west.  A note on the ',
							'wall says:', String lineDelimiter, String tab,
							'Magic Word "XYZZY"';
				shortDescription: 'You''re in debris room.');
		at: 12 put: (DarkLocation new
				longDescription: 'You are in an awkward sloping east/west canyon.';
				shortDescription: 'You are in an awkward sloping east/west canyon.');
		at: 13 put: (DarkLocation new
				longDescription: 'You are in a splendid chamber thirty feet high.  ',
							'The walls are frozen rivers of orange stone.  ',
							'An awkward canyon and a good passage exit ',
							'from east and west sides of the chamber.';
				shortDescription: 'You''re in the bird chamber.');
		at: 14 put: (DarkLocation new
				longDescription: 'At your feet is a small pit breathing traces ',
							'of white mist.  An east passage ends here ',
							'except for a small crack leading on.';
				shortDescription: 'You''re at the top of the small pit.');
		at: 15 put: (HallOfMists new
				longDescription: 'You are at one end of a vast hall stretching forward out of ',
							'sight to the west.  There are openings to either side.  ',
							'Nearby, a wide stone staircase leads downward.  The hall ',
							'is filled with wisps of white mist swaying to and fro ',
							'almost as if alive.  A cold wind blows up the staircase.  ',
							'There is a passage at the top of a dome behind you.';
				shortDescription: 'You''re in the hall of mists.');
		at: 16 put: (ForwardingLocation new
				longDescription: 'The crack is far too small for you to follow.';
				shortDescription: 'The crack is far too small for you to follow.');
		at: 17 put: (DarkLocation new
				longDescription: 'You are on the east bank of a fissure slicing clear across ',
							'the hall.  The mist is quite thick here, and the fissure ',
							'is too wide to jump.';
				shortDescription: 'You''re on the east bank of the fissure.');
		at: 18 put: (DarkLocation new
				longDescription: 'This is a low room with a crude note on the wall.  The ',
							'note says:  ',
							'You won''t get it up the steps.';
				shortDescription: 'You''re in the nugget of gold room.');
		at: 19 put: (HallOfMountainKing new
				longDescription: 'You are in the hall of the mountain king, with passages ',
							'off in all directions.';
				shortDescription: 'You''re in the hall of the mountain king.');
		at: 20 put: (TerminalLocation new
				longDescription: 'You are at the bottom of the pit with a broken neck.';
				shortDescription: 'You are at the bottom of the pit with a broken neck.').

	self eastBankOfFissure: (aDictionary at: 17)!

createLocations121to130: aDictionary
	"Private - create locations 121 to 130"

	aDictionary
		"at: 121 put: (DarkLocation new
				longDescription: 'You are in a secret canyon which exits to the north and east.';
				shortDescription: 'You are in a secret canyon which exits to the north and east.');"
		at: 122 put: (DarkLocation new
				longDescription: 'You are on the far side of the chasm.  A northeast path leads away ',
							'from the chasm on this side.';
				shortDescription: 'You''re on the northeast side of the chasm.');
		at: 123 put: (DarkLocation new
				longDescription: 'You''re in a long east/west corridor.  A faint rumbling noise ',
							'can be heard in the distance.';
				shortDescription: 'You''re in the corridor.');
		at: 124 put: (DarkLocation new
				longDescription: 'The path forks here.  The left fork leads northeast.  A dull ',
							'rumbling seems to get louder in that direction.  The right ',
							'fork leads southeast down a gentle slope.  The main ',
							'corridor enters from the west.';
				shortDescription: 'You''re at the fork in the path.');
		at: 125 put: (DarkLocation new
				longDescription: 'The walls are quite warm here.  From the north can be heard ',
							'a steady roar, so loud that the entire cave seems to be ',
							'trembling.  Another passage leads south, and a low crawl ',
							'goes east.';
				shortDescription: 'You''re at the junction with warm walls.');
		at: 126 put: (Location new
				longDescription: 'You are on the edge of a breath-taking view.  Far below you is an active ',
							'volcano, from which great gouts of molten lava come surging ',
							'out, cascading ',
							'back down into the depths. The glowing rock fills the farthest ',
							'reaches of ',
							'the cavern with a blood-red glare, giving everything an eerie, macabre ',
							'appearance.  The air is filled with flickering sparks of ash and ',
							'a heavy smell ',
							'of brimstone.  The walls are hot to the touch, and the thundering of the ',
							'volcano drowns out all other sounds.  Embedded in the jagged roof ',
							'far overhead ',
							'are myriad formations composed of pure white alabaster, which ',
							'scatter their ',
							'murky light into sinister apparitions upon the walls.  To one side is ',
							'a deep ',
							'gorge, filled with a bizarre chaos of tortured rock which seems ',
							'to have been ',
							'crafted by the Devil Himself.  An immense river of fire crashes ',
							'out from the ',
							'depths of the volcano, burns its way through the gorge, and ',
							'plummets into a ',
							'bottomless pit far off to your left.  To the right, an immense geyser of ',
							'blistering steam erupts continuously from a barren island in the ',
							'center of a ',
							'sulfurous lake, which bubbles ominously. The far right wall is ',
							'aflame with an ',
							'incandescence of its own, which lends an additional infernal ',
							'splendor to the ',
							'already hellish scene.  Spray-painted on a nearby wall is the message:  ',
							'"Bal was here.  That''s Al, with a B."  ',
							'A dark, foreboding passage exits to the south.';
				shortDescription: 'You''re at the breath-taking view.');
		at: 127 put: (DarkLocation new
				longDescription: 'You are in a small chamber filled with large boulders.  ',
							'The walls are very warm, causing the air in the room ',
							'to be almost stifling from the heat.  The only exit is a ',
							'crawl heading west, through which is coming a low rumbling.  ',
							'Dave''s voice says, "Watch out for the wall - and don''t ask ''What wall?''"';
				shortDescription: 'You''re in the chamber of boulders.');
		at: 128 put: (DarkLocation new
				longDescription: 'You are walking along a gently sloping north/south passage ',
							'lined with oddly shaped limestone formations.';
				shortDescription: 'You''re in the limestone passage.');
		at: 129 put: (DarkLocation new
				longDescription: 'You are standing at the entrance to a large, barren ',
							'room.  A sign posted above the entrance reads:  ',
							'"Caution!!  Bear in room!!"';
				shortDescription: 'You''re in front of the barren room.');
		at: 130 put: (DarkLocation new
				longDescription: 'You are inside a barren room.  The center of the room ',
							'is completely empty except for some dust.  Marks in ',
							'the dust lead away toward the far end of the room.  ',
							'The only exit is the way you came in.';
				shortDescription: 'You''re in the barren room.').!

createLocations131to140: aDictionary
	"Private - create locations 131 to 140"

	aDictionary
		at: 131 put: (DarkLocation new
				longDescription: 'You are in a maze of twisting little passages, all different.';
				shortDescription: 'You are in a maze of twisting little passages, all different.');
		at: 132 put: (DarkLocation new
				longDescription: 'You are in a little maze of twisty passages, all different.';
				shortDescription: 'You are in a little maze of twisty passages, all different.');
		at: 133 put: (DarkLocation new
				longDescription: 'You are in a twisting maze of little passages, all different.';
				shortDescription: 'You are in a twisting maze of little passages, all different.');
		at: 134 put: (DarkLocation new
				longDescription: 'You are in a twisting little maze of passages, all different.';
				shortDescription: 'You are in a twisting little maze of passages, all different.');
		at: 135 put: (DarkLocation new
				longDescription: 'You are in a twisty little maze of passages, all different.';
				shortDescription: 'You are in a twisty little maze of passages, all different.');
		at: 136 put: (DarkLocation new
				longDescription: 'You are in a twisty maze of little passages, all different.';
				shortDescription: 'You are in a twisty maze of little passages, all different.');
		at: 137 put: (DarkLocation new
				longDescription: 'You are in a little twisty maze of passages, all different.';
				shortDescription: 'You are in a little twisty maze of passages, all different.');
		at: 138 put: (DarkLocation new
				longDescription: 'You are in a maze of little twisting passages, all different.';
				shortDescription: 'You are in a maze of little twisting passages, all different.');
		at: 139 put: (DarkLocation new
				longDescription: 'You are in a maze of little twisty passages, all different.';
				shortDescription: 'You are in a maze of little twisty passages, all different.');
		at: 140 put: (DarkLocation new
				longDescription: 'Dead end.';
				shortDescription: 'Dead end.').!

createLocations141to150: aDictionary
	"Private - create locations 141 to 150"

	aDictionary
		at: 141 put: (ReflectingLocation new
				longDescription: 'You have crawled around in some little holes and wound up ',
							'back in the main passage.');
		at: 142 put: (ReflectingLocation new
				longDescription: 'You have crawled around in some little holes and found your ',
							'way blocked by a recent cave-in.  You are now back in the ',
							'main passage.');
		at: 143 put: (DarkLocation new
				longDescription: 'You are on the far side of the chasm.  A northeast path leads away ',
							'from the chasm on this side.';
				shortDescription: 'You''re on the northeast side of the chasm.');
		at: 144 put: (ReflectingLocation new
				longDescription: 'I respectfully suggest you go across the bridge instead of jumping.');
		at: 145 put: (DarkLocation new
				longDescription: 'You are on one side of a large deep chasm.  A heavy white ',
							'mist rising up from below obscures all view of the far ',
							'side.  A sw path leads away from the chasm into a winding ',
							'corridor.';
				shortDescription: 'You''re on the southwest side of the chasm.').

	self neChasm: (aDictionary at: 143).!

createLocations1to10: aDictionary
	"Private - create location 1 to 10"

	aDictionary
		at: 1 put: (Location new
				longDescription: 'You''re at the end of a road before a small brick building.  ',
							'Around you is a forest.  A small stream flows out ',
							'of the building and down a gully.';
				shortDescription: 'You''re at the end of the road again.';
				hasWater: true);
		at: 2 put: (Location new
				longDescription: 'You have walked up a hill, still in the forest.  The road ',
							'slopes back down the other side of the hill.  There is a ',
							'building in the distance.';
				shortDescription: 'You''re at the hill in the road.');
		at: 3 put: (Location new
				longDescription: 'You are inside a building, a well house for a large spring.';
				shortDescription: 'You''re inside the well building.';
				hasWater: true);
		at: 4 put: (Location new
				longDescription: 'You are in a valley in the forest beside a stream ',
							'tumbling along a rocky bed.';
				shortDescription: 'You''re in the valley.';
				hasWater: true);
		at: 5 put: (Location new
				longDescription: 'You are in open forest, with a deep valley to one side.';
				shortDescription: 'You''re in the forest.');
		at: 6 put: (Location new
				longDescription: 'You are in open forest near both a valley and a road.';
				shortDescription: 'You''re in the forest.');
		at: 7 put: (Location new
				longDescription: 'At your feet all the water of the stream ',
							'splashes into a 2-inch slit in the rock.  ',
							'Downstream the streambed is bare rock.';
				shortDescription: 'You''re at the slit in the streambed.';
				hasWater: true);
		at: 8 put: (Location new
				longDescription: 'You are in a 20-foot depression floored ',
							'with bare dirt.  Set into the dirt is a strong ',
							'steel grate mounted in concrete.  A dry ',
							'streambed leads into the depression.';
				shortDescription: 'You''re outside the grate.');
		at: 9 put: (Location new
				longDescription: 'You are in a small chamber beneath ',
							'a 3x3 steel grate to the surface.  A low ',
							'crawl over cobbles leads inward to the west.';
				shortDescription: 'You''re below the grate.');
		at: 10 put: (Location new
				longDescription: 'You are crawling over cobbles in a ',
							'low passage.  There is a dim light at the east ',
							'end of the passage.';
				shortDescription: 'You''re in the cobble crawl.').

	self building: (aDictionary at: 3).!

createLocations21to30: aDictionary
	"Private - create location 21 to 30"

	aDictionary
		at: 21 put: (TerminalLocation new
				longDescription: 'You didn''t make it.';
				shortDescription: 'You didn''t make it.');
		at: 22 put: (ForwardingLocation new
				longDescription: 'The dome is unclimbable.';
				shortDescription: 'The dome is unclimbable.');
		at: 23 put: (DarkLocation new
				longDescription: 'You are at the west end of the twopit room.  There is a large ',
							'hole in the wall above the pit at this end of the room.';
				shortDescription: 'You''re at west end of twopit room.');
		at: 24 put: (DarkLocation new
				longDescription: 'You are that the bottom of the eastern pit in the twopit room.  ',
							'There is a small pool of oil in one corner of the pit.';
				shortDescription: 'You''re in the east pit.';
				hasOil: true);
		at: 25 put: (DarkLocation new
				longDescription: 'You are at the bottom of the western pit in the twopit room.  ',
							'There is a large hole in the wall about 25 feet above you.';
				shortDescription: 'You''re in the west pit.');
		at: 26 put: (ForwardingLocation new
				longDescription: 'You clamber up the plant and scurry through the hole at the top.';
				shortDescription: 'You clamber up the plant and scurry through the hole at the top.');
		at: 27 put: (DarkLocation new
				longDescription: 'You are on the west side of the fissure in the hall of mists.';
				shortDescription: 'You are on the west side of the fissure in the hall of mists.');
		at: 28 put: (DarkLocation new
				longDescription: 'You are in a low N/S passage at a hole in the floor.  The ',
							'hole goes down to an E/W passage.';
				shortDescription: 'You are in a low N/S passage at a hole in the floor.  The ',
							'hole goes down to an E/W passage.');
		at: 29 put: (DarkLocation new
				longDescription: 'You are in the south side chamber.';
				shortDescription: 'You are in the south side chamber.');
		at: 30 put: (DarkLocation new
				longDescription: 'You are in the west side chamber of the hall of the ',
							'mountain king.  A passage continues west and up here.';
				shortDescription: 'You are in the west side chamber of the hall of the ',
							'mountain king.  A passage continues west and up here.').

	self westPit: (aDictionary at: 25).
	self westBankOfFissure: (aDictionary at: 27).!

createLocations31to40: aDictionary
	"Private - create location 31 to 40"

	aDictionary
		at: 31 put: (DarkLocation new
				longDescription: '>$<';
				shortDescription: '>$<');
		at: 32 put: (ForwardingLocation new
				longDescription: 'You can''t get by the snake.';
				shortDescription: 'You can''t get by the snake.');
		at: 33 put: (DarkLocation new
				longDescription: 'You are in a large room, with a passage to the south, ',
							'a passage to the west, and a wall of broken rock to the ',
							'east.  There is a large "Y2" on a rock in the room''s center.';
				shortDescription: 'You''re at "Y2".');
		at: 34 put: (DarkLocation new
				longDescription: 'You are in a jumble of rock, with cracks everywhere.';
				shortDescription: 'You are in a jumble of rock, with cracks everywhere.');
		at: 35 put: (Location new
				longDescription: 'You''re at a low window overlooking a huge pit, which ',
							'extends up out of sight.  A floor is indistinctly visible ',
							'over 50 feet below.  Traces of white mist cover the floor ',
							'of the pit, becoming thicker to the right.  Marks in the ',
							'dust around the window would seem to indicate that ',
							'someone has been here recently.  Directly across the pit ',
							'from you and 25 feet away there is a similar window ',
							'looking into a lighted room.  A shadowy figure can ',
							'be seen there peering back at you.';
				shortDescription: 'You''re at the window on the pit.');
		at: 36 put: (DarkLocation new
				longDescription: 'You are in a dirty broken passage.  To the east is a crawl.  ',
							'To the west is a large passage.  Above you is another passage.';
				shortDescription: 'You''re in dirty passage.');
		at: 37 put: (DarkLocation new
				longDescription: 'You are on the brink of a small clean climbable pit.  A ',
							'crawl leads west.';
				shortDescription: 'You are on the brink of a small clean climbable pit.  A ',
							'crawl leads west.');
		at: 38 put: (DarkLocation new
				longDescription: 'You are in the bottom of a small pit with a little stream, ',
							'which enters and exits through tiny slits';
				shortDescription: 'You are in the bottom of a small pit with a little stream, ',
							'which enters and exits through tiny slits';
				hasWater: true);
		at: 39 put: (DarkLocation new
				longDescription: 'You are in a large room full of dusty rocks.  There is a ',
							'big hole in the floor.  There are cracks everywhere, and ',
							'a passage leading east.';
				shortDescription: 'You''re in dusty rock room.');
		at: 40 put: (ForwardingLocation new
				longDescription: 'You have crawled through a very low wide passage parallel ',
							'to and north of the hall of mists.';
				shortDescription: 'You have crawled through a very low wide passage parallel ',
							'to and north of the hall of mists.').

	self y2: (aDictionary at: 33).!

createLocations41to50: aDictionary
	"Private - create location 41 to 50"

	aDictionary
		at: 41 put: (DarkLocation new
				longDescription: 'You are at the west end of hall of mists.  A low wide crawl ',
							'continues west and another goes north.  To the south is a ',
							'little passage 6 feet off the floor.';
				shortDescription: 'You''re at the west end of the hall of mists.');
		at: 42 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 43 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 44 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 45 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 46 put: (DarkLocation new
				longDescription: 'Dead end';
				shortDescription: 'Dead end');
		at: 47 put: (DarkLocation new
				longDescription: 'Dead end';
				shortDescription: 'Dead end');
		at: 48 put: (DarkLocation new
				longDescription: 'Dead end';
				shortDescription: 'Dead end');
		at: 49 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 50 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.').!

createLocations51to60: aDictionary
	"Private - create locations 51 to 60"

	aDictionary
		at: 51 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 52 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 53 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 54 put: (DarkLocation new
				longDescription: 'Dead end';
				shortDescription: 'Dead end');
		at: 55 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 56 put: (DarkLocation new
				longDescription: 'Dead end';
				shortDescription: 'Dead end');
		at: 57 put: (DarkLocation new
				longDescription: 'You are on the brink of a thirty foot pit with a massive ',
							'orange column down one wall.  You could climb down here but ',
							'you could not get back up.  The maze continues at this level.';
				shortDescription: 'You''re at brink of pit.');
		at: 58 put: (DarkLocation new
				longDescription: 'Dead end';
				shortDescription: 'Dead end');
		at: 59 put: (ForwardingLocation new
				longDescription: 'You have crawled through a very low wide passage parallel ',
							'to and north of the hall of mists.';
				shortDescription: 'You have crawled through a very low wide passage parallel ',
							'to and north of the hall of mists.');
		at: 60 put: (DarkLocation new
				longDescription: 'You are at the east end of a very long hall apparently ',
							'without side chambers.  To the east a low wide crawl ',
							'slants up.  To the north a round two foot hole slants ',
							'down.';
				shortDescription: 'You''re at the east end of the long hall.').!

createLocations61to70: aDictionary
	"Private - create locations 61 to 70"

	aDictionary
		at: 61 put: (DarkLocation new
				longDescription: 'You are at the west end of a very long featureless hall.  ',
							'The hall joins up with a narrow north/south passage.';
				shortDescription: 'You''re at the west end of the long hall.');
		at: 62 put: (DarkLocation new
				longDescription: 'You are at a crossover of a high N/S passage and a low ',
							'E/W one.';
				shortDescription: 'You are at a crossover of a high N/S passage and a low ',
							'E/W one.');
		at: 63 put: (DarkLocation new
				longDescription: 'Dead end';
				shortDescription: 'Dead end');
		at: 64 put: (DarkLocation new
				longDescription: 'You are at a complex junction.  A low hands and knees passage ',
							'from the north joins a higher crawl from the east to make ',
							'a walking passage going west.  There is also a large room ',
							'above.  The air is damp here.';
				shortDescription: 'You''re at a complex junction.');
		at: 65 put: (DarkLocation new
				longDescription: 'You are in bedquilt, a long east/west passage with holes everywhere.  ',
							'To explore at random select north, south, up or down.';
				shortDescription: 'You are in bedquilt, a long east/west passage with holes everywhere.  ',
							'To explore at random select north, south, up or down.');
		at: 66 put: (DarkLocation new
				longDescription: 'You are in a room whose walls resemble swiss cheese.  ',
							'Obvious passages go west, east, ne, and nw.  Part of the ',
							'room is occupied by a large bedrock block.';
				shortDescription: 'You''re in the swiss cheese room.');
		at: 67 put: (DarkLocation new
				longDescription: 'You are at the east end of the twopit room.  The floor here is ',
							'littered with thin rock slabs, which make it easy to descend the pits.  ',
							'There is a path here bypassing the pits to connect passages from ',
							'east and west.  There are holes all over, but the only big one is ',
							'on the wall directly over the west pit where you can''t get at it.';
				shortDescription: 'You''re at the east end of the twopit room.');
		at: 68 put: (DarkLocation new
				longDescription: 'You are in a large low circular chamber whose floor is an immense ',
							'slab fallen from the ceiling.  East and west there once ',
							'were large passages, but they are now filled with boulders.  ',
							'Low small passages go north and south, and the south one ',
							'quickly bends west around the boulders.';
				shortDescription: 'You''re in the slab room.');
		at: 69 put: (DarkLocation new
				longDescription: 'You are in a secret N/S canyon above a large room.';
				shortDescription: 'You are in a secret N/S canyon above a large room.');
		at: 70 put: (DarkLocation new
				longDescription: 'You are in a secret N/S canyon above a sizable passage.';
				shortDescription: 'You are in a secret N/S canyon above a sizable passage.').!

createLocations71to80: aDictionary
	"Private - create locations 71 to 80"

	aDictionary
		at: 71 put: (DarkLocation new
				longDescription: 'You are in a secret canyon at a junction of three canyons, ',
							'bearing north, south and se.  The north one is as tall as ',
							'the other two combined.';
				shortDescription: 'You''re at junction of three secret canyons.');
		at: 72 put: (DarkLocation new
				longDescription: 'You are in a large low room.  Crawls lead north, se, and sw.';
				shortDescription: 'You are in a large low room.  Crawls lead north, se, and sw.');
		at: 73 put: (DarkLocation new
				longDescription: 'Dead end crawl.';
				shortDescription: 'Dead end crawl.');
		at: 74 put: (DarkLocation new
				longDescription: 'You are in a secret canyon which here runs E/W.  It crosses ',
							'over a very tight canyon 15 feet below.  If you go down you ',
							'may not be able to get back up.';
				shortDescription: 'You''re at secret E/W canyon above tight canyon.');
		at: 75 put: (DarkLocation new
				longDescription: 'You are at a wide place in a very tight N/S canyon.';
				shortDescription: 'You are at a wide place in a very tight N/S canyon.');
		at: 76 put: (DarkLocation new
				longDescription: 'The canyon here becomes too tight to go further south.';
				shortDescription: 'The canyon here becomes too tight to go further south.');
		at: 77 put: (DarkLocation new
				longDescription: 'You are in a tall E/W canyon.  A low tight crawl goes 3 ',
							'feet north and seems to open up.';
				shortDescription: 'You are in a tall E/W canyon.  A low tight crawl goes 3 ',
							'feet north and seems to open up.');
		at: 78 put: (DarkLocation new
				longDescription: 'The canyon runs into a mass of boulders -- dead end.';
				shortDescription: 'The canyon runs into a mass of boulders -- dead end.');
		at: 79 put: (ForwardingLocation new
				longDescription: 'The stream flows out through a pair of 1 foot diameter ',
							'sewer pipes.  It would be advisable to use the exit.';
				shortDescription: '';
				hasWater: true);
		at: 80 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.').!

createLocations81to90: aDictionary
	"Private - create locations 81 to 90"

	aDictionary
		at: 81 put: (DarkLocation new
				longDescription: 'Dead end.';
				shortDescription: 'Dead end.');
		at: 82 put: (DarkLocation new
				longDescription: 'Dead end.';
				shortDescription: 'Dead end.');
		at: 83 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 84 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 85 put: (DarkLocation new
				longDescription: 'Dead end.';
				shortDescription: 'Dead end.');
		at: 86 put: (DarkLocation new
				longDescription: 'Dead end.';
				shortDescription: 'Dead end.');
		at: 87 put: (DarkLocation new
				longDescription: 'You are in a maze of twisty little passages, all alike.';
				shortDescription: 'You are in a maze of twisty little passages, all alike.');
		at: 88 put: (DarkLocation new
				longDescription: 'You are in a long, narrow corridor stretching out of sight ',
							'to the west.  At the eastern end is a hole through which ',
							'you can see a profusion of leaves.';
				shortDescription: 'You''re in a narrow corridor.');
		at: 89 put: (ForwardingLocation new
				longDescription: 'There is nothing here to climb.  Use "up" or "out" to leave ',
							'the pit.';
				shortDescription: 'There is nothing here to climb.  Use "up" or "out" to leave ',
							'the pit.');
		at: 90 put: (ForwardingLocation new
				longDescription: 'You have climbed up the plant and out of the pit.';
				shortDescription: 'You have climbed up the plant and out of the pit.').!

createLocations91to100: aDictionary
	"Private - create locations 91 to 100"

	aDictionary
		at: 91 put: (DarkLocation new
				longDescription: 'You are at the top of a steep incline above a large room.  ',
							'You could climb down here, but you would not be able to ',
							'climb up.  There is a passage leading back to the north.';
				shortDescription: 'You''re at a steep incline above a large room.');
		at: 92 put: (DarkLocation new
				longDescription: 'You are in the giant room.  The ceiling is too high up ',
							'for your lamp to show it.  Cavernous passages lead east, ',
							'north, and south.  On the west wall is scrawled the ',
							'inscription:',
							'  "Fee Fie Foe Foo"       {sic}';
				shortDescription: 'You''re in the giant room.');
		at: 93 put: (DarkLocation new
				longDescription: 'The passage here is blocked by a recent cave-in.';
				shortDescription: 'The passage here is blocked by a recent cave-in.');
		at: 94 put: (DarkLocation new
				longDescription: 'You are at one end of an immense north/south passage.';
				shortDescription: 'You are at one end of an immense north/south passage.');
		at: 95 put: (DarkLocation new
				longDescription: 'You are in a magnificent cavern with a rushing stream, ',
							'which cascades over a sparkling waterfall into a ',
							'roaring whirlpool which disappears through a hole in ',
							'the floor.  Passages exit to the south and west.';
				shortDescription: 'You''re in cavern with waterfall.';
				hasWater: true);
		at: 96 put: (DarkLocation new
				longDescription: 'You are in the soft room.  The walls are covered with ',
							'heavy curtains, the floor with a thick pile carpet.  ',
							'Moss covers the ceiling.';
				shortDescription: 'You''re in the soft room.');
		at: 97 put: (DarkLocation new
				longDescription: 'This is the oriental room.  Ancient oriental cave drawings ',
							'cover the walls.  A gently sloping passage leads upward ',
							'to the north, another passage leads se, and a hands and ',
							'knees crawl leads west.';
				shortDescription: 'You''re in the oriental room.');
		at: 98 put: (DarkLocation new
				longDescription: 'You are following a wide path around the outer edge of a large cavern.  ',
							'Far below, through a heavy white mist, strange splashing noises can be heard.  ',
							'The mist rises up through a fissure in the ceiling.  ',
							'The path exits to the south and west.';
				shortDescription: 'You''re in the misty cavern.');
		at: 99 put: (DarkLocation new
				longDescription: 'You are in an alcove.  A small nw path seems to widen after a short distance.  ',
							'An extremely tight tunnel leads east.  It looks like a very tight squeeze.  ',
							'An eerie light can be seen at the other end.';
				shortDescription: 'You''re in the alcove.');
		at: 100 put: (DarkLocation new
				longDescription: 'You''re in a small chamber lit by an eerie green light.  An extremely ',
							'narrow tunnel exits to the west.  A dark corridor leads ne.';
				shortDescription: 'You''re in the plover room.';
				lighted: true).

	self giantRoom: (aDictionary at: 92).
	self giantPassage: (aDictionary at: 94).
	self orientalRoom: (aDictionary at: 97).
	self alcove: (aDictionary at: 99).!

culDeSac
	"Answer the value of the receiver's instance variable culDeSac.
	This method was automatically generated, but may be modified."

	^culDeSac!

culDeSac: anObject
	"Set the value of the receiver's instance variable culDeSac to anObject.
	This method was automatically generated, but may be modified."

	culDeSac := anObject!

defaultFileName
	^'ColossalCaveWorld.stb'!

docileBear
	"Answer the value of the receiver's instance variable docileBear.
	This method was automatically generated, but may be modified."

	^docileBear!

docileBear: anObject
	"Set the value of the receiver's instance variable docileBear to anObject.
	This method was automatically generated, but may be modified."

	docileBear := anObject!

door
	"Answer the value of the receiver's instance variable door.
	This method was automatically generated, but may be modified."

	^door!

door: anObject
	"Set the value of the receiver's instance variable door to anObject.
	This method was automatically generated, but may be modified."

	door := anObject!

dragon
	"Answer the value of the receiver's instance variable dragon.
	This method was automatically generated, but may be modified."

	^dragon!

dragon: anObject
	"Set the value of the receiver's instance variable dragon to anObject.
	This method was automatically generated, but may be modified."

	dragon := anObject!

eastBankOfFissure
	"Answer the value of the receiver's instance variable eastBankOfFissure.
	This method was automatically generated, but may be modified."

	^eastBankOfFissure!

eastBankOfFissure: anObject
	"Set the value of the receiver's instance variable eastBankOfFissure to anObject.
	This method was automatically generated, but may be modified."

	eastBankOfFissure := anObject!

eggs
	"Answer the value of the receiver's instance variable eggs.
	This method was automatically generated, but may be modified."

	^eggs!

eggs: anObject
	"Set the value of the receiver's instance variable eggs to anObject.
	This method was automatically generated, but may be modified."

	eggs := anObject!

emerald
	"Answer the value of the receiver's instance variable emerald.
	This method was automatically generated, but may be modified."

	^emerald!

emerald: anObject
	"Set the value of the receiver's instance variable emerald to anObject.
	This method was automatically generated, but may be modified."

	emerald := anObject!

ferociousBear
	"Answer the value of the receiver's instance variable ferociousBear.
	This method was automatically generated, but may be modified."

	^ferociousBear!

ferociousBear: anObject
	"Set the value of the receiver's instance variable ferociousBear to anObject.
	This method was automatically generated, but may be modified."

	ferociousBear := anObject!

food
	"Answer the value of the receiver's instance variable food.
	This method was automatically generated, but may be modified."

	^food!

food: anObject
	"Set the value of the receiver's instance variable food to anObject.
	This method was automatically generated, but may be modified."

	food := anObject!

giantPassage
	"Answer the value of the receiver's instance variable giantPassage.
	This method was automatically generated, but may be modified."

	^giantPassage!

giantPassage: anObject
	"Set the value of the receiver's instance variable giantPassage to anObject.
	This method was automatically generated, but may be modified."

	giantPassage := anObject!

giantRoom
	"Answer the value of the receiver's instance variable giantRoom.
	This method was automatically generated, but may be modified."

	^giantRoom!

giantRoom: anObject
	"Set the value of the receiver's instance variable giantRoom to anObject.
	This method was automatically generated, but may be modified."

	giantRoom := anObject!

helpString
^'Somewhere nearby is Colossal Cave, where others have ',
'found fortunes in treasure and gold, though it is rumored ',
'that some who enter are never seen again.  Magic is said ',
'to work in the cave.  I will be your eyes and hands.  Direct ',
'me with commands of 1 or 2 words.'!

initialize
	| locations |

	super initialize.

	self soundDirectory: 'c:\winnt\profiles\jarvisb\personal\dolphin smalltalk 4.0\packages' , 					'\smallworlds\sounds'.
	locations := self createLocations.
	self createConnections: locations.
	self createItems: locations.

	locations do: [ :each | self addLocation: each ].

	self actor: (ColossalCaveActor new
					location: (locations at: 3);
					longNounPhrase: 'nervous-looking individual with thick glasses';
					shortNounPhrase: 'adventurer').!

jewelry
	"Answer the value of the receiver's instance variable jewelry.
	This method was automatically generated, but may be modified."

	^jewelry!

jewelry: anObject
	"Set the value of the receiver's instance variable jewelry to anObject.
	This method was automatically generated, but may be modified."

	jewelry := anObject!

keys
	"Answer the value of the receiver's instance variable keys.
	This method was automatically generated, but may be modified."

	^keys!

keys: anObject
	"Set the value of the receiver's instance variable keys to anObject.
	This method was automatically generated, but may be modified."

	keys := anObject!

magazine
	"Answer the value of the receiver's instance variable magazine.
	This method was automatically generated, but may be modified."

	^magazine!

magazine: anObject
	"Set the value of the receiver's instance variable magazine to anObject.
	This method was automatically generated, but may be modified."

	magazine := anObject!

mouse
	"Answer the value of the receiver's instance variable mouse.
	This method was automatically generated, but may be modified."

	^mouse!

mouse: anObject
	"Set the value of the receiver's instance variable mouse to anObject.
	This method was automatically generated, but may be modified."

	mouse := anObject!

neChasm
	"Answer the value of the receiver's instance variable neChasm.
	This method was automatically generated, but may be modified."

	^neChasm!

neChasm: anObject
	"Set the value of the receiver's instance variable neChasm to anObject.
	This method was automatically generated, but may be modified."

	neChasm := anObject!

nugget
	"Answer the value of the receiver's instance variable nugget.
	This method was automatically generated, but may be modified."

	^nugget!

nugget: anObject
	"Set the value of the receiver's instance variable nugget to anObject.
	This method was automatically generated, but may be modified."

	nugget := anObject!

orientalRoom
	"Answer the value of the receiver's instance variable orientalRoom.
	This method was automatically generated, but may be modified."

	^orientalRoom!

orientalRoom: anObject
	"Set the value of the receiver's instance variable orientalRoom to anObject.
	This method was automatically generated, but may be modified."

	orientalRoom := anObject!

pearl
	"Answer the value of the receiver's instance variable pearl.
	This method was automatically generated, but may be modified."

	^pearl!

pearl: anObject
	"Set the value of the receiver's instance variable pearl to anObject.
	This method was automatically generated, but may be modified."

	pearl := anObject!

pillow
	"Answer the value of the receiver's instance variable pillow.
	This method was automatically generated, but may be modified."

	^pillow!

pillow: anObject
	"Set the value of the receiver's instance variable pillow to anObject.
	This method was automatically generated, but may be modified."

	pillow := anObject!

plant
	"Answer the value of the receiver's instance variable plant.
	This method was automatically generated, but may be modified."

	^plant!

plant: anObject
	"Set the value of the receiver's instance variable plant to anObject.
	This method was automatically generated, but may be modified."

	plant := anObject!

pyramid
	"Answer the value of the receiver's instance variable pyramid.
	This method was automatically generated, but may be modified."

	^pyramid!

pyramid: anObject
	"Set the value of the receiver's instance variable pyramid to anObject.
	This method was automatically generated, but may be modified."

	pyramid := anObject!

rod
	"Answer the value of the receiver's instance variable rod.
	This method was automatically generated, but may be modified."

	^rod!

rod: anObject
	"Set the value of the receiver's instance variable rod to anObject.
	This method was automatically generated, but may be modified."

	rod := anObject!

rug
	"Answer the value of the receiver's instance variable rug.
	This method was automatically generated, but may be modified."

	^rug!

rug: anObject
	"Set the value of the receiver's instance variable rug to anObject.
	This method was automatically generated, but may be modified."

	rug := anObject!

secretCanyon
	"Answer the value of the receiver's instance variable secretCanyon.
	This method was automatically generated, but may be modified."

	^secretCanyon!

secretCanyon: anObject
	"Set the value of the receiver's instance variable secretCanyon to anObject.
	This method was automatically generated, but may be modified."

	secretCanyon := anObject!

silverBars
	"Answer the value of the receiver's instance variable silverBars.
	This method was automatically generated, but may be modified."

	^silverBars!

silverBars: anObject
	"Set the value of the receiver's instance variable silverBars to anObject.
	This method was automatically generated, but may be modified."

	silverBars := anObject!

snake
	"Answer the value of the receiver's instance variable snake.
	This method was automatically generated, but may be modified."

	^snake!

snake: anObject
	"Set the value of the receiver's instance variable snake to anObject.
	This method was automatically generated, but may be modified."

	snake := anObject!

spices
	"Answer the value of the receiver's instance variable spices.
	This method was automatically generated, but may be modified."

	^spices!

spices: anObject
	"Set the value of the receiver's instance variable spices to anObject.
	This method was automatically generated, but may be modified."

	spices := anObject!

steps
	"Answer the value of the receiver's instance variable steps.
	This method was automatically generated, but may be modified."

	^steps!

steps: anObject
	"Set the value of the receiver's instance variable steps to anObject.
	This method was automatically generated, but may be modified."

	steps := anObject!

swChasm
	"Answer the value of the receiver's instance variable swChasm.
	This method was automatically generated, but may be modified."

	^swChasm!

swChasm: anObject
	"Set the value of the receiver's instance variable swChasm to anObject.
	This method was automatically generated, but may be modified."

	swChasm := anObject!

tablet
	"Answer the value of the receiver's instance variable tablet.
	This method was automatically generated, but may be modified."

	^tablet!

tablet: anObject
	"Set the value of the receiver's instance variable tablet to anObject.
	This method was automatically generated, but may be modified."

	tablet := anObject!

trident
	"Answer the value of the receiver's instance variable trident.
	This method was automatically generated, but may be modified."

	^trident!

trident: anObject
	"Set the value of the receiver's instance variable trident to anObject.
	This method was automatically generated, but may be modified."

	trident := anObject!

troll
	"Answer the value of the receiver's instance variable troll.
	This method was automatically generated, but may be modified."

	^troll!

troll: anObject
	"Set the value of the receiver's instance variable troll to anObject.
	This method was automatically generated, but may be modified."

	troll := anObject!

vase
	"Answer the value of the receiver's instance variable vase.
	This method was automatically generated, but may be modified."

	^vase!

vase: anObject
	"Set the value of the receiver's instance variable vase to anObject.
	This method was automatically generated, but may be modified."

	vase := anObject!

westBankOfFissure
	"Answer the value of the receiver's instance variable westBankOfFissure.
	This method was automatically generated, but may be modified."

	^westBankOfFissure!

westBankOfFissure: anObject
	"Set the value of the receiver's instance variable westBankOfFissure to anObject.
	This method was automatically generated, but may be modified."

	westBankOfFissure := anObject!

westPit
	"Answer the value of the receiver's instance variable westPit.
	This method was automatically generated, but may be modified."

	^westPit!

westPit: anObject
	"Set the value of the receiver's instance variable westPit to anObject.
	This method was automatically generated, but may be modified."

	westPit := anObject!

wittsEnd
	"Answer the value of the receiver's instance variable wittsEnd.
	This method was automatically generated, but may be modified."

	^wittsEnd!

wittsEnd: anObject
	"Set the value of the receiver's instance variable wittsEnd to anObject.
	This method was automatically generated, but may be modified."

	wittsEnd := anObject!

y2
	"Answer the value of the receiver's instance variable y2.
	This method was automatically generated, but may be modified."

	^y2!

y2: anObject
	"Set the value of the receiver's instance variable y2 to anObject.
	This method was automatically generated, but may be modified."

	y2 := anObject! !
!ColossalCaveWorld categoriesFor: #alcove!accessing!public! !
!ColossalCaveWorld categoriesFor: #alcove:!accessing!public! !
!ColossalCaveWorld categoriesFor: #bird!accessing!public! !
!ColossalCaveWorld categoriesFor: #bird:!accessing!public! !
!ColossalCaveWorld categoriesFor: #bridge!accessing!public! !
!ColossalCaveWorld categoriesFor: #bridge:!accessing!public! !
!ColossalCaveWorld categoriesFor: #building!accessing!public! !
!ColossalCaveWorld categoriesFor: #building:!accessing!public! !
!ColossalCaveWorld categoriesFor: #cage!accessing!public! !
!ColossalCaveWorld categoriesFor: #cage:!accessing!public! !
!ColossalCaveWorld categoriesFor: #chain!accessing!public! !
!ColossalCaveWorld categoriesFor: #chain:!accessing!public! !
!ColossalCaveWorld categoriesFor: #clam!accessing!public! !
!ColossalCaveWorld categoriesFor: #clam:!accessing!public! !
!ColossalCaveWorld categoriesFor: #coins!accessing!public! !
!ColossalCaveWorld categoriesFor: #coins:!accessing!public! !
!ColossalCaveWorld categoriesFor: #createConnections:!initialization!private! !
!ColossalCaveWorld categoriesFor: #createConnections101to110:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections111to120:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections11to20:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections121to130:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections131to140:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections141to150:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections1to10:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections21to30:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections31to40:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections41to50:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections51to60:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections61to70:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections71to80:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections81to90:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createConnections91to100:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createItems:!initializing!private! !
!ColossalCaveWorld categoriesFor: #createLocations!initializing!private! !
!ColossalCaveWorld categoriesFor: #createLocations101to110:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations111to120:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations11to20:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations121to130:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations131to140:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations141to150:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations1to10:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations21to30:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations31to40:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations41to50:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations51to60:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations61to70:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations71to80:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations81to90:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #createLocations91to100:!private!private helpers! !
!ColossalCaveWorld categoriesFor: #culDeSac!accessing!public! !
!ColossalCaveWorld categoriesFor: #culDeSac:!accessing!public! !
!ColossalCaveWorld categoriesFor: #defaultFileName!accessing!public! !
!ColossalCaveWorld categoriesFor: #docileBear!accessing!public! !
!ColossalCaveWorld categoriesFor: #docileBear:!accessing!public! !
!ColossalCaveWorld categoriesFor: #door!accessing!public! !
!ColossalCaveWorld categoriesFor: #door:!accessing!public! !
!ColossalCaveWorld categoriesFor: #dragon!accessing!public! !
!ColossalCaveWorld categoriesFor: #dragon:!accessing!public! !
!ColossalCaveWorld categoriesFor: #eastBankOfFissure!accessing!public! !
!ColossalCaveWorld categoriesFor: #eastBankOfFissure:!accessing!public! !
!ColossalCaveWorld categoriesFor: #eggs!accessing!public! !
!ColossalCaveWorld categoriesFor: #eggs:!accessing!public! !
!ColossalCaveWorld categoriesFor: #emerald!accessing!public! !
!ColossalCaveWorld categoriesFor: #emerald:!accessing!public! !
!ColossalCaveWorld categoriesFor: #ferociousBear!accessing!public! !
!ColossalCaveWorld categoriesFor: #ferociousBear:!accessing!public! !
!ColossalCaveWorld categoriesFor: #food!accessing!public! !
!ColossalCaveWorld categoriesFor: #food:!accessing!public! !
!ColossalCaveWorld categoriesFor: #giantPassage!accessing!public! !
!ColossalCaveWorld categoriesFor: #giantPassage:!accessing!public! !
!ColossalCaveWorld categoriesFor: #giantRoom!accessing!public! !
!ColossalCaveWorld categoriesFor: #giantRoom:!accessing!public! !
!ColossalCaveWorld categoriesFor: #helpString!accessing!public! !
!ColossalCaveWorld categoriesFor: #initialize!initializing!public! !
!ColossalCaveWorld categoriesFor: #jewelry!accessing!public! !
!ColossalCaveWorld categoriesFor: #jewelry:!accessing!public! !
!ColossalCaveWorld categoriesFor: #keys!accessing!public! !
!ColossalCaveWorld categoriesFor: #keys:!accessing!public! !
!ColossalCaveWorld categoriesFor: #magazine!accessing!public! !
!ColossalCaveWorld categoriesFor: #magazine:!accessing!public! !
!ColossalCaveWorld categoriesFor: #mouse!accessing!public! !
!ColossalCaveWorld categoriesFor: #mouse:!accessing!public! !
!ColossalCaveWorld categoriesFor: #neChasm!accessing!public! !
!ColossalCaveWorld categoriesFor: #neChasm:!accessing!public! !
!ColossalCaveWorld categoriesFor: #nugget!accessing!public! !
!ColossalCaveWorld categoriesFor: #nugget:!accessing!public! !
!ColossalCaveWorld categoriesFor: #orientalRoom!accessing!public! !
!ColossalCaveWorld categoriesFor: #orientalRoom:!accessing!public! !
!ColossalCaveWorld categoriesFor: #pearl!accessing!public! !
!ColossalCaveWorld categoriesFor: #pearl:!accessing!public! !
!ColossalCaveWorld categoriesFor: #pillow!accessing!public! !
!ColossalCaveWorld categoriesFor: #pillow:!accessing!public! !
!ColossalCaveWorld categoriesFor: #plant!accessing!public! !
!ColossalCaveWorld categoriesFor: #plant:!accessing!public! !
!ColossalCaveWorld categoriesFor: #pyramid!accessing!public! !
!ColossalCaveWorld categoriesFor: #pyramid:!accessing!public! !
!ColossalCaveWorld categoriesFor: #rod!accessing!public! !
!ColossalCaveWorld categoriesFor: #rod:!accessing!public! !
!ColossalCaveWorld categoriesFor: #rug!accessing!public! !
!ColossalCaveWorld categoriesFor: #rug:!accessing!public! !
!ColossalCaveWorld categoriesFor: #secretCanyon!accessing!public! !
!ColossalCaveWorld categoriesFor: #secretCanyon:!accessing!public! !
!ColossalCaveWorld categoriesFor: #silverBars!accessing!public! !
!ColossalCaveWorld categoriesFor: #silverBars:!accessing!public! !
!ColossalCaveWorld categoriesFor: #snake!accessing!public! !
!ColossalCaveWorld categoriesFor: #snake:!accessing!public! !
!ColossalCaveWorld categoriesFor: #spices!accessing!public! !
!ColossalCaveWorld categoriesFor: #spices:!accessing!public! !
!ColossalCaveWorld categoriesFor: #steps!accessing!public! !
!ColossalCaveWorld categoriesFor: #steps:!accessing!public! !
!ColossalCaveWorld categoriesFor: #swChasm!accessing!public! !
!ColossalCaveWorld categoriesFor: #swChasm:!accessing!public! !
!ColossalCaveWorld categoriesFor: #tablet!accessing!public! !
!ColossalCaveWorld categoriesFor: #tablet:!accessing!public! !
!ColossalCaveWorld categoriesFor: #trident!accessing!public! !
!ColossalCaveWorld categoriesFor: #trident:!accessing!public! !
!ColossalCaveWorld categoriesFor: #troll!accessing!public! !
!ColossalCaveWorld categoriesFor: #troll:!accessing!public! !
!ColossalCaveWorld categoriesFor: #vase!accessing!public! !
!ColossalCaveWorld categoriesFor: #vase:!accessing!public! !
!ColossalCaveWorld categoriesFor: #westBankOfFissure!accessing!public! !
!ColossalCaveWorld categoriesFor: #westBankOfFissure:!accessing!public! !
!ColossalCaveWorld categoriesFor: #westPit!accessing!public! !
!ColossalCaveWorld categoriesFor: #westPit:!accessing!public! !
!ColossalCaveWorld categoriesFor: #wittsEnd!accessing!public! !
!ColossalCaveWorld categoriesFor: #wittsEnd:!accessing!public! !
!ColossalCaveWorld categoriesFor: #y2!accessing!public! !
!ColossalCaveWorld categoriesFor: #y2:!accessing!public! !

ColossalCaveShell guid: (GUID fromString: '{B1FE94C0-54A3-11D3-8268-00001D19F5C2}')!
ColossalCaveShell comment: ''!
!ColossalCaveShell categoriesForClass!No category! !
!ColossalCaveShell class methodsFor!

defaultModel
	^ColossalCaveWorld new! !
!ColossalCaveShell class categoriesFor: #defaultModel!models!public! !

"Binary Globals"!

"Resources"!

