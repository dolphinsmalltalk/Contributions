| package |
package := Package name: 'Scheduling'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #Activity;
	add: #ActualProduct;
	add: #Aluminum;
	add: #BiasedPCPSchedulingStrategy;
	add: #Capacity;
	add: #CompositeBiasedPCPSchedulingStrategy;
	add: #ConstantRuntimeCalculator;
	add: #Constraint;
	add: #ConstraintSchedulingStrategy;
	add: #DeliveryDateGeneratingFactory;
	add: #Factory;
	add: #FactoryBuilder;
	add: #FixedConstraint;
	add: #HexagonalSolid;
	add: #Job;
	add: #LengthRuntimeCalculator;
	add: #Material;
	add: #OctagonalSolid;
	add: #Operation;
	add: #OperationPair;
	add: #PCPSchedulingStrategy;
	add: #PlannedProduct;
	add: #Product;
	add: #ProductCharacteristics;
	add: #ProductShape;
	add: #RectangularSolid;
	add: #RoundSolid;
	add: #RoundTube;
	add: #RuntimeCalculator;
	add: #ScaledRuntimeCalculator;
	add: #SchedulingStrategy;
	add: #SPPCPSchedulingStrategy;
	add: #Steel;
	add: #TestFactoryBuilder;
	add: #Timespan;
	add: #Titanium;
	add: #WeightRuntimeCalculator;
	add: #Workcenter;
	yourself.

package methodNames
	add: #TimeStamp -> #isContainedBy:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Other Packages\Units\Units';
	yourself).

package!

"Class Definitions"!

Object subclass: #Activity
	instanceVariableNames: 'name description'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Capacity
	instanceVariableNames: 'value timeSpan'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Constraint
	instanceVariableNames: 'before after feasible'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Factory
	instanceVariableNames: 'workcenters jobs strategy activities name jobDictionary'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #FactoryBuilder
	instanceVariableNames: 'factory'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Job
	instanceVariableNames: 'id operations startDate requiredDate plannedProduct'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Material
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'current'!
Object subclass: #Operation
	instanceVariableNames: 'id activity workcenter outputProduct constraints earliestStartTime latestFinishTime job complete'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'yieldFactor'!
Object subclass: #OperationPair
	instanceVariableNames: 'i j slackIj slackJi schedulingStrategy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Product
	instanceVariableNames: 'characteristics quantity'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ProductCharacteristics
	instanceVariableNames: 's1 s2 shape material'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ProductShape
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #RuntimeCalculator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SchedulingStrategy
	instanceVariableNames: 'factory'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Timespan
	instanceVariableNames: 'startTimeStamp endTimeStamp'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Workcenter
	instanceVariableNames: 'activities name operations runtimeCalculator capacities'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Constraint subclass: #FixedConstraint
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Factory subclass: #DeliveryDateGeneratingFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FactoryBuilder subclass: #TestFactoryBuilder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Material subclass: #Aluminum
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Material subclass: #Steel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Material subclass: #Titanium
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Product subclass: #ActualProduct
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Product subclass: #PlannedProduct
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProductShape subclass: #HexagonalSolid
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProductShape subclass: #OctagonalSolid
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProductShape subclass: #RectangularSolid
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProductShape subclass: #RoundSolid
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProductShape subclass: #RoundTube
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeCalculator subclass: #ConstantRuntimeCalculator
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeCalculator subclass: #ScaledRuntimeCalculator
	instanceVariableNames: 'scalingFactor'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ScaledRuntimeCalculator subclass: #LengthRuntimeCalculator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ScaledRuntimeCalculator subclass: #WeightRuntimeCalculator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SchedulingStrategy subclass: #ConstraintSchedulingStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ConstraintSchedulingStrategy subclass: #PCPSchedulingStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ConstraintSchedulingStrategy subclass: #SPPCPSchedulingStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PCPSchedulingStrategy subclass: #BiasedPCPSchedulingStrategy
	instanceVariableNames: 'root'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BiasedPCPSchedulingStrategy subclass: #CompositeBiasedPCPSchedulingStrategy
	instanceVariableNames: 'root2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!TimeStamp methodsFor!

isContainedBy: aTimespan
	^(aTimespan startTimeStamp <= self) and: [ self <= aTimespan endTimeStamp ]! !
!TimeStamp categoriesFor: #isContainedBy:!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

Activity guid: (GUID fromString: '{87C8F9A5-8308-11D3-8283-00001D19F5C2}')!
Activity comment: 'Represents a transformation in state or status (e.g. Melt, Roll, Ship) which can be performed on a Product.'!
!Activity categoriesForClass!Scheduling! !
!Activity methodsFor!

description
	"Answer the value of the receiver's ''description'' instance variable."

	^description!

description: anObject
	"Set the value of the receiver's ''description'' instance variable to the argument, anObject."

	description := anObject!

displayOn: aStream
	aStream display: self name, '-', self description!

name
	"Answer the value of the receiver's ''name'' instance variable."

	^name!

name: anObject
	"Set the value of the receiver's ''name'' instance variable to the argument, anObject."

	name := anObject!

printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		display: self;
		nextPut: $)! !
!Activity categoriesFor: #description!accessing!public! !
!Activity categoriesFor: #description:!accessing!public! !
!Activity categoriesFor: #displayOn:!displaying!public! !
!Activity categoriesFor: #name!accessing!public! !
!Activity categoriesFor: #name:!accessing!public! !
!Activity categoriesFor: #printOn:!printing!public! !

Capacity guid: (GUID fromString: '{87468CA1-9C3D-11D3-8287-00001D19F5C2}')!
Capacity comment: 'This class represents the capacity of a Workcenter over a Timespan.'!
!Capacity categoriesForClass!Unclassified! !
!Capacity methodsFor!

printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		display: self value;
		display: ' from ';
		display: self timeSpan;
		nextPut: $)!

timeSpan
	"Answer the value of the receiver's ''timeSpan'' instance variable."

	^timeSpan!

timeSpan: aTimespan
	"Set the value of the receiver's ''timeSpan'' instance variable to the argument, aTimespan."

	timeSpan := aTimespan!

value
	"Private - Answer the value of the receiver's ''value'' instance variable."

	^value!

value: anObject
	"Private - Set the value of the receiver's ''value'' instance variable to the argument, anObject."

	value := anObject! !
!Capacity categoriesFor: #printOn:!printing!public! !
!Capacity categoriesFor: #timeSpan!accessing!public! !
!Capacity categoriesFor: #timeSpan:!accessing!public! !
!Capacity categoriesFor: #value!accessing!public! !
!Capacity categoriesFor: #value:!accessing!public! !

Constraint guid: (GUID fromString: '{F03E7F70-7110-11D3-8279-00001D19F5C2}')!
Constraint comment: 'Specifies the required ordering of a pair of Operations, i.e. which one must come before the other.'!
!Constraint categoriesForClass!Scheduling! !
!Constraint methodsFor!

= aConstraint
	"#= and #hash are implemented for Constraint because instances of this class and its subclasses are
	 intended to be stored in hashed collections (e.g. Set)"

	(aConstraint isKindOf: Constraint)
		ifTrue: [ ^self before == aConstraint before and: [ self after == aConstraint after ] ]
		ifFalse: [ ^false ]
!

after
	"Answer the value of the receiver's 'after' instance variable."

	^after!

after: anOperationOrNil
	"Set the value of the receiver's 'after' instance variable to the argument, anObject."

	self after notNil ifTrue: [ self after removeConstraint: self ].
	after := anOperationOrNil.
	self after notNil ifTrue: [ self after addConstraint: self ]!

before
	"Answer the value of the receiver's 'before' instance variable."

	^before!

before: anOperationOrNil
	"Set the value of the receiver's 'before' instance variable to the argument, anObject."

	self before notNil ifTrue: [ self before removeConstraint: self ].
	before := anOperationOrNil.
	self before notNil ifTrue: [ self before addConstraint: self ]!

feasible
	"Answer the value of the receiver's 'feasible' instance variable."

	^feasible!

feasible: aBoolean
	"Private - Set the value of the receiver's 'feasible' instance variable to the argument, anObject."

	feasible := aBoolean!

hash
	"#= and #hash are reimplemented for Constraint because instances of this class and its
	 subclasses are intended to be stored in hashed collections (e.g. Set)"

	^before hash bitXor: after hash!

initialize
	self feasible: true!

isFixedConstraint
	^false!

otherFrom: anOperation
	"Return the 'other' object referred to by this contraint, from the perspective of the given object 
	(which must be one of the objects referenced by this object"

	anOperation == self before
		ifTrue: [ ^self after ]
		ifFalse: [
			anOperation == self after
				ifTrue: [ ^self before ]
				ifFalse: [ Error signal: 'Invalid operation reference' ] ]!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	before displayOn: aStream.
	aStream display: '  before '.
	after displayOn: aStream.
	aStream nextPut: $).!

referencesOperation: anOper
	^(self before == anOper) | (self after == anOper)! !
!Constraint categoriesFor: #=!comparing!public! !
!Constraint categoriesFor: #after!accessing!public! !
!Constraint categoriesFor: #after:!accessing!public! !
!Constraint categoriesFor: #before!accessing!public! !
!Constraint categoriesFor: #before:!accessing!public! !
!Constraint categoriesFor: #feasible!accessing!public! !
!Constraint categoriesFor: #feasible:!accessing!private! !
!Constraint categoriesFor: #hash!comparing!public! !
!Constraint categoriesFor: #initialize!initialize/release!public! !
!Constraint categoriesFor: #isFixedConstraint!public!testing! !
!Constraint categoriesFor: #otherFrom:!operations!public! !
!Constraint categoriesFor: #printOn:!printing!public! !
!Constraint categoriesFor: #referencesOperation:!public!testing! !

!Constraint class methodsFor!

new
	^super new initialize! !
!Constraint class categoriesFor: #new!instance creation!public! !

Factory guid: (GUID fromString: '{B72A8D49-6C42-11D3-8276-00001D19F5C2}')!
Factory comment: 'A class representing a factory where orders are processed through a group of workcenters.'!
!Factory categoriesForClass!Scheduling! !
!Factory methodsFor!

activities
	activities isNil ifTrue: [ self activities: Dictionary new ].
	^activities!

activities: aDictionary
	activities := aDictionary!

addActivity: anActivity
	self activities add: (anActivity name -> anActivity)!

addJob: aJob
	self jobs add: aJob.
	self jobDictionary add: (aJob id -> aJob).
	^aJob!

addWorkcenter: aWorkcenter
	self workcenters add: (aWorkcenter name -> aWorkcenter)!

buildWith: aFactoryBuilder scheduleWith: aSchedulingStrategy
	aFactoryBuilder build: self.
	self
		strategy: aSchedulingStrategy;
		clearDates;
		propagateDates!

clearDates
	self jobs do: [ :aJob | aJob clearDates ]!

displayOn: aStream
	aStream display: self name; cr; display: 'Workcenters'; cr.
	self workcenters do: [ :aWorkcenter | aWorkcenter displayOn: aStream.  aStream cr ].
	aStream display: 'Jobs:'; cr.
	self jobs do: [ :aJob | aJob displayOn: aStream.  aStream cr ].!

dropAllActivities
	self activities: nil!

jobAt: aString
	^self jobDictionary at: aString!

jobAt: aString ifAbsent: aBlock
	^self jobDictionary at: aString ifAbsent: aBlock!

jobDictionary
	jobDictionary isNil ifTrue: [ jobDictionary := Dictionary new ].
	^jobDictionary!

jobs
	"Answer the value of the receiver's 'jobs' instance variable."

	jobs isNil ifTrue: [ self jobs: OrderedCollection new ].
	^jobs!

jobs: anObject
	"Private - Set the value of the receiver's 'jobs' instance variable to the argument, anObject."

	jobs := anObject!

name
	^name!

name: aString
	name := aString!

propagateDates
	self jobs do: [ :aJob | aJob propagateDates ]!

removeSchedulingConstraints
	self jobs do: [ :aJob | aJob removeSchedulingConstraints ]!

schedule
	self removeSchedulingConstraints.
	self strategy schedule.!

strategy
	^strategy!

strategy: aSchedulingStrategy
	strategy := aSchedulingStrategy.
	self strategy factory: self!

workcenters
	"Answer the value of the receiver's 'workcenters' instance variable."

	workcenters isNil ifTrue: [ self workcenters: Dictionary new ].
	^workcenters!

workcenters: aDictionary
	workcenters := aDictionary! !
!Factory categoriesFor: #activities!accessing!public! !
!Factory categoriesFor: #activities:!accessing!private! !
!Factory categoriesFor: #addActivity:!adding!public! !
!Factory categoriesFor: #addJob:!adding!public! !
!Factory categoriesFor: #addWorkcenter:!adding!public! !
!Factory categoriesFor: #buildWith:scheduleWith:!operations!public! !
!Factory categoriesFor: #clearDates!operations!public! !
!Factory categoriesFor: #displayOn:!displaying!public! !
!Factory categoriesFor: #dropAllActivities!operations!public! !
!Factory categoriesFor: #jobAt:!accessing!public! !
!Factory categoriesFor: #jobAt:ifAbsent:!accessing!public! !
!Factory categoriesFor: #jobDictionary!accessing!public! !
!Factory categoriesFor: #jobs!accessing!public! !
!Factory categoriesFor: #jobs:!accessing!private! !
!Factory categoriesFor: #name!accessing!public! !
!Factory categoriesFor: #name:!accessing!public! !
!Factory categoriesFor: #propagateDates!operations!public! !
!Factory categoriesFor: #removeSchedulingConstraints!public!removing! !
!Factory categoriesFor: #schedule!operations!public! !
!Factory categoriesFor: #strategy!accessing!public! !
!Factory categoriesFor: #strategy:!accessing!public! !
!Factory categoriesFor: #workcenters!accessing!public! !
!Factory categoriesFor: #workcenters:!accessing!private! !

FactoryBuilder guid: (GUID fromString: '{8EA741A0-6F99-11D3-8279-00001D19F5C2}')!
FactoryBuilder comment: 'Implement strategies used to build factories, i.e. add workcenters and jobs to a factory.  Subclasses can be used to define particular construction strategies, e.g. which workcenters and jobs to add.  This is an example of the Builder pattern in the GOF book (page 97).'!
!FactoryBuilder categoriesForClass!Scheduling! !
!FactoryBuilder methodsFor!

build: aFactory
	self
		factory: aFactory;
		createActivities;
		createWorkcenters;
		createJobs.
	^aFactory!

createActivities
	^self subclassResponsibility!

createJobs
	^self subclassResponsibility!

createWorkcenters
	^self subclassResponsibility!

factory
	"Answer the value of the receiver's 'factory' instance variable."

	^factory!

factory: anObject
	"Set the value of the receiver's 'factory' instance variable to the argument, anObject."

	factory := anObject! !
!FactoryBuilder categoriesFor: #build:!operations!public! !
!FactoryBuilder categoriesFor: #createActivities!operations!private! !
!FactoryBuilder categoriesFor: #createJobs!operations!private! !
!FactoryBuilder categoriesFor: #createWorkcenters!operations!private! !
!FactoryBuilder categoriesFor: #factory!accessing!public! !
!FactoryBuilder categoriesFor: #factory:!accessing!public! !

Job guid: (GUID fromString: '{B72A8D32-6C42-11D3-8276-00001D19F5C2}')!
Job comment: 'Represents an order to produce certain product(s) for a customer.'!
!Job categoriesForClass!Scheduling! !
!Job methodsFor!

addOperation:  anOperation
	"Private - Adds a new operation.  Answers the operation which was added."
	anOperation job: self.
	^self operations add: anOperation!

addOperation:  anOperation after: anExistingOperation
	"Adds anOperation to the job, constraining it to follow precedingOperation.  Note that this method
	 does not cause operations which previously followed precedingOperation to now follow
	anOperation - to do this the #insertOperation:after: method should be used.  Answers the operation 	added."

	anExistingOperation notNil ifTrue: [
		FixedConstraint new
			before: anExistingOperation;
			after: anOperation ].
	^self addOperation: anOperation
!

addOperation:  anOperation before: anExistingOperation
	"Adds anOperation to the Job, constraining it to precede followingOperation.  Note that this method
	 does not cause operations which previously preceded followingOperation to now precede
	 anOperation - to do this the #insertOperation:before: method should be used.  Answers the
	operation added."

	anExistingOperation notNil ifTrue: [
		FixedConstraint new
			before: anOperation;
			after: anExistingOperation ].
	^self addOperation: anOperation!

backwardPropagateDates
	self finalOperations do: [ :anOper |
		anOper backwardPropagateLftFrom: self requiredDate ]!

backwardPropagateLatestFinishTimes
	self backwardPropagateLftFrom: self maximumFinalLft!

backwardPropagateLftFrom: aTimestamp
	self finalOperations do: [ :anOper |
		anOper backwardPropagateLftFrom: aTimestamp ]!

clearDates
	"Clear the propagated dates in this job.  Note that we don't clear the requiredDate and
	 startDate fields, as these are inputs to the job."

	self operations do: [ :anOper | anOper clearDates ]!

displayOn: aStream
	aStream
		display: self id;
		tab; show: 'startDate='; display: self startDate;
		tab; show: 'requiredDate='; display: self requiredDate;
		cr.
	self operations do: [ :anOperation | aStream display: anOperation; cr ]!

finalOperations
	"Answers a collection of operations which have no successor operations."

	^self operations select: [ :anOper | anOper isFinalOperation ]!

finalProducts
	"Answer the value of the receiver's 'finalProduct' instance variable."

	^self finalOperations collect: [ :each | each outputProduct ]!

forwardPropagateEarliestStartTimes
	self initialOperations do: [ :anOper | anOper forwardPropagateEstFrom: self startDate ]!

forwardPropagateLatestFinishTimes
	self forwardPropagateLftFrom: self maximumInitialLft!

forwardPropagateLftFrom: aTimestamp
	self initialOperations do: [ :anOper | anOper forwardPropagateLftFrom: aTimestamp ]!

id
	^id!

id: aString
	id := aString!

initialOperations
	"Answers a collection of operations which have no predecessor operations."

	^self operations select: [ :anOper | anOper isInitialOperation ]!

inputProducts
	"Answers a collection of the product which is input to the initial operations of this job."

	^(self initialOperations collect: [ :each | each inputProduct ]) reject: [ :each | each isNil ]!

insertOperation: anOperation after: precedingOperation
	"Inserts the new operation into the job after the specified preceding operation, changing
	 all operations which previously followed precedingOperation to now follow the new
	 operation.  Answers the operation which was inserted."

	precedingOperation notNil ifTrue: [
		precedingOperation successorConstraints do: [ :each | each before: anOperation ] ].
	FixedConstraint new
		before: precedingOperation;
		after: anOperation.
	^self addOperation: anOperation!

insertOperation: anOperation before: successorOperation
	"Inserts the new operation into the job before  the specified preceding operation, changing
	 all operations which previously preceeded successorOperation to now precede the new
	 operation.  Answers the operation which was inserted."

	successorOperation notNil ifTrue: [
		successorOperation predecessorConstraints do: [ :each | each after: anOperation ] ].
	FixedConstraint new
		before: anOperation;
		after: successorOperation.
	^self addOperation: anOperation!

maximumFinalLft
	"Answer the maximum latestFinishTime value from all final operations in this job."

	^self finalOperations inject: nil into: [ :lft :anOper |
		(lft isNil or: [ anOper latestFinishTime > lft ])
			ifTrue: [ anOper latestFinishTime ]
			ifFalse: [ lft ] ]!

maximumInitialLft
	"Answer the maximum latestFinishTime value from all initial operations in this job."

	^self initialOperations inject: nil into: [ :lft :anOper |
		(lft isNil or: [ anOper latestFinishTime > lft ])
			ifTrue: [ anOper latestFinishTime ]
			ifFalse: [ lft ] ]!

operations
	"Answer the value of the receiver's 'operations' instance variable."

	operations isNil ifTrue: [ self operations: OrderedCollection new ].
	^operations!

operations: anObject
	"Private - Set the value of the receiver's 'operations' instance variable to the argument, anObject."

	operations := anObject!

printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		display: self id displayString;
		nextPut: $)!

propagateDates
	self
		forwardPropagateEarliestStartTimes;
		backwardPropagateDates!

removeOperation: anOperation
	(self operations remove: anOperation ifAbsent: [ nil ]) notNil
		ifTrue: [ anOperation removeAllConstraints ].
	^anOperation!

removeSchedulingConstraints
	self operations do: [ :anOper | anOper removeSchedulingConstraints ]!

requiredDate
	requiredDate isNil ifTrue: [
		self requiredDate: (self finalOperations inject: TimeStamp current into: [ :aTimeStamp :aJob |
							aJob latestFinishTime > aTimeStamp
								ifTrue: [ aJob latestFinishTime ]
								ifFalse: [ aTimeStamp ]])].
	^requiredDate!

requiredDate: anObject
	"Set the value of the receiver's 'requiredDate' instance variable to the argument, anObject,
	 which should be a TimeStamp"

	requiredDate := anObject!

startDate
	"Answer the value of the receiver's 'startDate' instance variable."

	^startDate!

startDate: anObject
	"Set the value of the receiver's 'startDate' instance variable to the argument, anObject,
	 which should be a TimeStamp"

	startDate := anObject! !
!Job categoriesFor: #addOperation:!helpers!private! !
!Job categoriesFor: #addOperation:after:!adding!public! !
!Job categoriesFor: #addOperation:before:!adding!public! !
!Job categoriesFor: #backwardPropagateDates!operations!public! !
!Job categoriesFor: #backwardPropagateLatestFinishTimes!operations!public! !
!Job categoriesFor: #backwardPropagateLftFrom:!helpers!private! !
!Job categoriesFor: #clearDates!operations!public! !
!Job categoriesFor: #displayOn:!displaying!public! !
!Job categoriesFor: #finalOperations!accessing!public! !
!Job categoriesFor: #finalProducts!accessing!public! !
!Job categoriesFor: #forwardPropagateEarliestStartTimes!operations!public! !
!Job categoriesFor: #forwardPropagateLatestFinishTimes!operations!public! !
!Job categoriesFor: #forwardPropagateLftFrom:!helpers!private! !
!Job categoriesFor: #id!accessing!public! !
!Job categoriesFor: #id:!accessing!public! !
!Job categoriesFor: #initialOperations!accessing!public! !
!Job categoriesFor: #inputProducts!accessing!public! !
!Job categoriesFor: #insertOperation:after:!operations!public! !
!Job categoriesFor: #insertOperation:before:!operations!public! !
!Job categoriesFor: #maximumFinalLft!accessing!public! !
!Job categoriesFor: #maximumInitialLft!accessing!public! !
!Job categoriesFor: #operations!accessing!public! !
!Job categoriesFor: #operations:!accessing!private! !
!Job categoriesFor: #printOn:!printing!public! !
!Job categoriesFor: #propagateDates!operations!public! !
!Job categoriesFor: #removeOperation:!public!removing! !
!Job categoriesFor: #removeSchedulingConstraints!public!removing! !
!Job categoriesFor: #requiredDate!accessing!public! !
!Job categoriesFor: #requiredDate:!accessing!public! !
!Job categoriesFor: #startDate!accessing!public! !
!Job categoriesFor: #startDate:!accessing!public! !

Material guid: (GUID fromString: '{0C2D17F1-9878-11D3-8287-00001D19F5C2}')!
Material comment: 'Represents a type of material from which products are to be made.'!
!Material categoriesForClass!Scheduling! !
!Material methodsFor!

displayOn: aStream
	aStream display: self class name asLowercase
!

poundsPerCubicInch
	"Answer a UnitValue containing the weight of a cubic inch of this material, in pounds."
	self subclassResponsibility! !
!Material categoriesFor: #displayOn:!displaying!public! !
!Material categoriesFor: #poundsPerCubicInch!accessing!public! !

!Material class methodsFor!

current
	current isNil ifTrue: [ current := self basicNew ].
	^current!

current: anObject
	current := anObject!

new
	"Use #current to get the singleton instance of Material subclasses"
	self shouldNotImplement! !
!Material class categoriesFor: #current!accessing!public! !
!Material class categoriesFor: #current:!accessing!public! !
!Material class categoriesFor: #new!instance creation!public! !

Operation guid: (GUID fromString: '{B72A8D33-6C42-11D3-8276-00001D19F5C2}')!
Operation comment: 'An Operation represents the execution of an Activity by a Workcenter.'!
!Operation categoriesForClass!Scheduling! !
!Operation methodsFor!

activity
	"Private - Answer the value of the receiver's ''activity'' instance variable."

	^activity!

activity: anObject
	"Private - Set the value of the receiver's ''activity'' instance variable to the argument, anObject."

	activity := anObject!

addConstraint: aConstraint
	self constraints add: aConstraint!

backwardPropagateLftFrom: aTimestamp
	aTimestamp notNil ifTrue: [
		(self latestFinishTime isNil or: [ aTimestamp < self latestFinishTime ])
			ifTrue: [ self latestFinishTime: aTimestamp ].
		self predecessors do: [ :anOper |
			anOper backwardPropagateLftFrom: (TimeStamp fromSeconds: (self latestFinishTime asSeconds -
													     self runTime seconds value + 1)) ] ]!

basicRemoveConstraint: aConstraint
	"Private"
	self constraints remove: aConstraint ifAbsent: [ ]!

clearDates
	self
		earliestStartTime: nil;
		latestFinishTime: nil!

complete
	^complete!

complete: aBoolean
	complete := aBoolean.
	complete ifTrue: [ self removeSchedulingConstraints ]!

constraints
	"Private - Answer the value of the receiver's 'constraints' instance variable."

	constraints isNil ifTrue: [ self constraints: OrderedCollection new ].
	^constraints!

constraints: anObject
	"Private - Set the value of the receiver's 'constraints' instance variable to the argument, anObject."

	constraints := anObject!

displayOn: aStream
	aStream
		display: self displayString;
		tab;
		display: self activity name;
		nextPut: $@;
		display: self workcenter name;
		display: '  runtime=';
		display: self runTime hours;
		display: ' (EST: ';
		display: self earliestStartTime;
		display: '  LFT: ';
		display: self latestFinishTime;
		nextPut: $)!

displayString
	^self job id displayString, '|', self id displayString!

earliestStartTime
	"Answer the value of the receiver's 'earliestStartTime' instance variable."

	^earliestStartTime!

earliestStartTime: anObject
	"Set the value of the receiver's 'earliestStartTime' instance variable to the argument, anObject."

	earliestStartTime := anObject!

forwardPropagateEstFrom: aTimestamp
	(self earliestStartTime isNil or: [ aTimestamp > self earliestStartTime ])
		ifTrue: [ self earliestStartTime: aTimestamp ].
	self successors do: [ :anOper |
		anOper forwardPropagateEstFrom: (TimeStamp fromSeconds: (self earliestStartTime asSeconds +
												self runTime seconds value)) ]!

forwardPropagateLftFrom: aTimestamp
	(self latestFinishTime isNil or: [ aTimestamp > self latestFinishTime ])
		ifTrue: [ self latestFinishTime: aTimestamp ].
	self successors do: [ :anOper |
		anOper forwardPropagateLftFrom: (TimeStamp fromSeconds: (self latestFinishTime asSeconds +
												anOper runTime seconds value)) ]!

id
	^id!

id: aString
	id := aString!

initialize
	super initialize.
	self complete: false!

inputFeet
	^self predecessorSiblings inject: 0 feet into: [ :tot :each | tot + each outputFeet ]!

inputPounds
	^self predecessorSiblings inject: 0 pounds into: [ :tot :each | tot + each outputPounds ]!

isFinalOperation
	^self successorSiblings size = 0!

isInitialOperation
	^self predecessorSiblings size = 0!

isIsolatedSibling
	"Answer true if this operation has no predecessor or successor siblings, otherwise answer false."

	^(self isInitialOperation) and: [ self isFinalOperation ]!

job
	"Private - Answer the value of the receiver's ''job'' instance variable."

	^job!

job: anObject
	"Private - Set the value of the receiver's ''job'' instance variable to the argument, anObject."

	job := anObject!

latestFinishTime
	"Answer the value of the receiver's 'latestFinishTime' instance variable."

	^latestFinishTime!

latestFinishTime: aTimeStamp
	"self displayString = 'Job 1|Oper 1' ifTrue: [ self halt ]."
	latestFinishTime := aTimeStamp!

notScheduledWith: anOper
	^(self scheduledWith: anOper) not!

outputFeet
	^self outputProduct feet!

outputPounds
	^self outputProduct pounds!

outputProduct
	^outputProduct!

outputProduct: aProduct
	outputProduct := aProduct!

predecessorConstraints
	^self constraints select: [ :aConstraint | aConstraint after == self ]!

predecessors
	^(self predecessorConstraints collect: [ :aConstraint | aConstraint before ]) asSet!

predecessorsAt: aWorkcenter
	^(self predecessors reject: [ :anOper | anOper workcenter ~~ aWorkcenter ]) asSet!

predecessorSiblings
	"Answer a collection of all operations which immediately precede this operation within the same job."

	^self predecessors select: [ :anOper | anOper job == self job ]!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $)!

removeAllConstraints
	self constraints copy do: [ :aConstraint | self removeConstraint: aConstraint ].
	self constraints: nil!

removeConstraint: aConstraint
	self basicRemoveConstraint: aConstraint.
	(aConstraint otherFrom: self) basicRemoveConstraint: aConstraint.
	^aConstraint!

removeSchedulingConstraints
	self constraints copy do: [ :aConstraint |
		aConstraint isFixedConstraint ifFalse: [self removeConstraint: aConstraint ] ]!

runTime
	^self workcenter runTime: self!

scheduledWith: anOper
	"Determine if this operation has had a scheduling constraint added to it with anOper as
	 the 'other' operation."

	self constraints do: [ :aConstraint |
		(aConstraint otherFrom: self) == anOper ifTrue: [ ^true ] ].

	^false!

successorConstraints
	"Answer a collection of constraints on successor operations."
	^self constraints select: [ :aConstraint | aConstraint before == self ]!

successors
	^(self successorConstraints collect: [ :aConstraint | aConstraint after ]) asSet!

successorsAt: aWorkcenter
	^(self successors reject: [ :anOper | anOper workcenter ~~ aWorkcenter ]) asSet!

successorSiblings
	"Answer a collection of all operations which immediately succeed this operation within the same job."

	^self successors select: [ :anOper | anOper job == self job ]!

workcenter
	"Answer the value of the receiver's 'workcenter' instance variable."

	^workcenter!

workcenter: aWorkcenter
	"Set the value of the receiver's 'workcenter' instance variable to the argument, anObject."

	self workcenter notNil ifTrue: [ self workcenter removeOperation: self ].
	workcenter := aWorkcenter.
	self workcenter notNil ifTrue: [ self workcenter addOperation: self ]! !
!Operation categoriesFor: #activity!accessing!public! !
!Operation categoriesFor: #activity:!accessing!public! !
!Operation categoriesFor: #addConstraint:!adding!public! !
!Operation categoriesFor: #backwardPropagateLftFrom:!operations!public! !
!Operation categoriesFor: #basicRemoveConstraint:!helpers!private! !
!Operation categoriesFor: #clearDates!operations!public! !
!Operation categoriesFor: #complete!accessing!public! !
!Operation categoriesFor: #complete:!accessing!public! !
!Operation categoriesFor: #constraints!accessing!public! !
!Operation categoriesFor: #constraints:!accessing!private! !
!Operation categoriesFor: #displayOn:!displaying!public! !
!Operation categoriesFor: #displayString!converting!public! !
!Operation categoriesFor: #earliestStartTime!accessing!public! !
!Operation categoriesFor: #earliestStartTime:!accessing!public! !
!Operation categoriesFor: #forwardPropagateEstFrom:!operations!public! !
!Operation categoriesFor: #forwardPropagateLftFrom:!operations!public! !
!Operation categoriesFor: #id!accessing!public! !
!Operation categoriesFor: #id:!accessing!public! !
!Operation categoriesFor: #initialize!initialization!public! !
!Operation categoriesFor: #inputFeet!accessing!public! !
!Operation categoriesFor: #inputPounds!accessing!public! !
!Operation categoriesFor: #isFinalOperation!public!testing! !
!Operation categoriesFor: #isInitialOperation!public!testing! !
!Operation categoriesFor: #isIsolatedSibling!public!testing! !
!Operation categoriesFor: #job!accessing!public! !
!Operation categoriesFor: #job:!accessing!public! !
!Operation categoriesFor: #latestFinishTime!accessing!public! !
!Operation categoriesFor: #latestFinishTime:!accessing!public! !
!Operation categoriesFor: #notScheduledWith:!accessing!public! !
!Operation categoriesFor: #outputFeet!accessing!public! !
!Operation categoriesFor: #outputPounds!accessing!public! !
!Operation categoriesFor: #outputProduct!accessing!public! !
!Operation categoriesFor: #outputProduct:!accessing!public! !
!Operation categoriesFor: #predecessorConstraints!accessing!public! !
!Operation categoriesFor: #predecessors!accessing!public! !
!Operation categoriesFor: #predecessorsAt:!accessing!public! !
!Operation categoriesFor: #predecessorSiblings!accessing!public! !
!Operation categoriesFor: #printOn:!printing!public! !
!Operation categoriesFor: #removeAllConstraints!public!removing! !
!Operation categoriesFor: #removeConstraint:!public!removing! !
!Operation categoriesFor: #removeSchedulingConstraints!public!removing! !
!Operation categoriesFor: #runTime!accessing!public! !
!Operation categoriesFor: #scheduledWith:!accessing!public! !
!Operation categoriesFor: #successorConstraints!accessing!public! !
!Operation categoriesFor: #successors!accessing!public! !
!Operation categoriesFor: #successorsAt:!accessing!public! !
!Operation categoriesFor: #successorSiblings!accessing!public! !
!Operation categoriesFor: #workcenter!accessing!public! !
!Operation categoriesFor: #workcenter:!accessing!public! !

!Operation class methodsFor!

new
	^super new initialize!

yieldFactor
	"Private - Answer the value of the receiver's 'yieldFactor' instance variable."

	^yieldFactor!

yieldFactor: anObject
	"Private - Set the value of the receiver's 'yieldFactor' instance variable to the argument, anObject."

	yieldFactor := anObject! !
!Operation class categoriesFor: #new!instance creation!public! !
!Operation class categoriesFor: #yieldFactor!accessing!public! !
!Operation class categoriesFor: #yieldFactor:!accessing!public! !

OperationPair guid: (GUID fromString: '{EF9C2912-CAAD-11D3-8294-00001D19F5C2}')!
OperationPair comment: ''!
!OperationPair categoriesForClass!Unclassified! !
!OperationPair methodsFor!

i
	^i!

i: anObject
	i := anObject!

i: anOper j: anOtherOper
	self i: anOper; j: anOtherOper!

j
	^j!

j: anObject
	j := anObject!

maxSlack
	^self slackIj max: self slackJi!

minSlack
	^self slackIj min: self slackJi!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	i displayOn: aStream.
	aStream display: ' & '.
	j displayOn: aStream.
	aStream nextPut: $).!

schedulingStrategy
	^schedulingStrategy!

schedulingStrategy: anObject
	schedulingStrategy := anObject!

slackIj
	slackIj isNil ifTrue: [ slackIj := self schedulingStrategy slackBetween: i and: j ].
	^slackIj!

slackJi
	slackJi isNil ifTrue: [ slackJi := self schedulingStrategy slackBetween: j and: i ].
	^slackJi! !
!OperationPair categoriesFor: #i!accessing!public! !
!OperationPair categoriesFor: #i:!accessing!public! !
!OperationPair categoriesFor: #i:j:!accessing!public! !
!OperationPair categoriesFor: #j!accessing!public! !
!OperationPair categoriesFor: #j:!accessing!public! !
!OperationPair categoriesFor: #maxSlack!accessing!public! !
!OperationPair categoriesFor: #minSlack!accessing!public! !
!OperationPair categoriesFor: #printOn:!accessing!public! !
!OperationPair categoriesFor: #schedulingStrategy!accessing!public! !
!OperationPair categoriesFor: #schedulingStrategy:!accessing!public! !
!OperationPair categoriesFor: #slackIj!accessing!public! !
!OperationPair categoriesFor: #slackJi!accessing!public! !

Product guid: (GUID fromString: '{B72A8D48-6C42-11D3-8276-00001D19F5C2}')!
Product comment: 'Represents a quantity of material with certain physical characteristics.'!
!Product categoriesForClass!Scheduling! !
!Product methodsFor!

characteristics
	characteristics isNil ifTrue: [ self characteristics: ProductCharacteristics new ].
	^characteristics!

characteristics: aProductCharacteristics
	characteristics := aProductCharacteristics!

displayOn: aStream
	aStream
		display: self quantity;
		nextPutAll: ' of ';
		display: self characteristics!

feet
	(self quantity unit baseUnits consistentWith: Unit feet)
		ifTrue: [ ^self quantity feet ]
		ifFalse: [ ^(self quantity pounds / self characteristics tfw) feet ]!

isActualProduct
	^false!

isPlannedProduct
	^false!

pounds
	(self quantity unit baseUnits consistentWith: Unit pounds)
		ifTrue: [ ^self quantity pounds ]
		ifFalse: [ ^(self quantity feet * self characteristics tfw) pounds ]
!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

quantity
	^quantity!

quantity: aUnitValue
	quantity := aUnitValue! !
!Product categoriesFor: #characteristics!accessing!public! !
!Product categoriesFor: #characteristics:!accessing!public! !
!Product categoriesFor: #displayOn:!displaying!public! !
!Product categoriesFor: #feet!converting!public! !
!Product categoriesFor: #isActualProduct!public!testing! !
!Product categoriesFor: #isPlannedProduct!public!testing! !
!Product categoriesFor: #pounds!converting!public! !
!Product categoriesFor: #printOn:!printing!public! !
!Product categoriesFor: #quantity!accessing!public! !
!Product categoriesFor: #quantity:!accessing!public! !

ProductCharacteristics guid: (GUID fromString: '{B72A8D42-6C42-11D3-8276-00001D19F5C2}')!
ProductCharacteristics comment: 'The physical characteristics (dimensions, shapes, etc) of a given Product.'!
!ProductCharacteristics categoriesForClass!Scheduling! !
!ProductCharacteristics methodsFor!

area
	"Answer the cross-sectional area of the product"

	^self shape area: self!

displayOn: aStream
	aStream
		display: self s1;
		nextPutAll: ' by ';
		display: self s2;
		nextPut: $ ;
		display: self material;
		nextPut: $ ;
		display: self shape!

material
	"Answer the value of the receiver's ''material'' instance variable."

	^material!

material: aMaterial
	"Set the value of the receiver's ''material'' instance variable to the argument, aMaterial."

	material := aMaterial!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

s1
	"Answer the value of the receiver's 's1' instance variable."

	^s1!

s1: anObject
	"Set the value of the receiver's 's1' instance variable to the argument, anObject."

	s1 := anObject inches!

s2
	"Answer the value of the receiver's 's2' instance variable."

	^s2!

s2: anObject
	"Set the value of the receiver's 's2' instance variable to the argument, anObject."

	s2 := anObject inches!

shape
	"Answer the value of the receiver's 'shape' instance variable."

	^shape!

shape: anObject
	"Set the value of the receiver's 'shape' instance variable to the argument, anObject."

	shape := anObject!

tfw
	^self shape tfw: self! !
!ProductCharacteristics categoriesFor: #area!public! !
!ProductCharacteristics categoriesFor: #displayOn:!public! !
!ProductCharacteristics categoriesFor: #material!accessing!public! !
!ProductCharacteristics categoriesFor: #material:!accessing!public! !
!ProductCharacteristics categoriesFor: #printOn:!public! !
!ProductCharacteristics categoriesFor: #s1!accessing!public! !
!ProductCharacteristics categoriesFor: #s1:!accessing!public! !
!ProductCharacteristics categoriesFor: #s2!accessing!public! !
!ProductCharacteristics categoriesFor: #s2:!accessing!public! !
!ProductCharacteristics categoriesFor: #shape!accessing!public! !
!ProductCharacteristics categoriesFor: #shape:!accessing!public! !
!ProductCharacteristics categoriesFor: #tfw!public! !

!ProductCharacteristics class methodsFor!

shapeFromSize1: s1 size2: s2
	s1 > s2
		ifTrue: [ s2 > 0 inches
				ifTrue: [ ^'RDTUBE' ]
				ifFalse: [ ^'RDBAR' ] ]
		ifFalse: [ ^'RCSBAR' ]! !
!ProductCharacteristics class categoriesFor: #shapeFromSize1:size2:!public! !

ProductShape guid: (GUID fromString: '{B72A8D43-6C42-11D3-8276-00001D19F5C2}')!
ProductShape comment: ''!
!ProductShape categoriesForClass!Scheduling! !
!ProductShape methodsFor!

area: aProductCharacteristics
	"Answers the area of the given product in square inches"
	^self subclassResponsibility!

displayOn: aStream
	aStream nextPutAll: self shortDescription!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

shortDescription
	self subclassResponsibility!

tfw: aProductCharacteristics
	"Compute the weight for product of the given characteristics in pounds per foot"

	^(self area: aProductCharacteristics) * (12 / 1 inches squared) *
		aProductCharacteristics material poundsPerCubicInch! !
!ProductShape categoriesFor: #area:!operations!public! !
!ProductShape categoriesFor: #displayOn:!public! !
!ProductShape categoriesFor: #printOn:!public! !
!ProductShape categoriesFor: #shortDescription!private! !
!ProductShape categoriesFor: #tfw:!operations!public! !

ProductShape methodProtocol: #ProductShape attributes: #() selectors: #(#area: #tfw:)!

RuntimeCalculator guid: (GUID fromString: '{87C8F9A4-8308-11D3-8283-00001D19F5C2}')!
RuntimeCalculator comment: 'This class and its subclasses encapsulate interchangeable algorithms used to measure runtime for a given operation performing a given operation at a given workcenter.'!
!RuntimeCalculator categoriesForClass!Scheduling! !
!RuntimeCalculator methodsFor!

runTime: anOperation
	"Answer a UnitValue representing the amount of time required to perform anOperation"

	^self subclassResponsibility! !
!RuntimeCalculator categoriesFor: #runTime:!public! !

SchedulingStrategy guid: (GUID fromString: '{87C8F9A0-8308-11D3-8283-00001D19F5C2}')!
SchedulingStrategy comment: 'An algorithm for scheduling jobs at a Factory.  This is an example of the Strategy pattern from the GOF pattern book (pg. 315).'!
!SchedulingStrategy categoriesForClass!Scheduling! !
!SchedulingStrategy methodsFor!

factory
	"Private - Answer the value of the receiver's 'factory' instance variable."

	^factory!

factory: aFactory
	"Set the value of the receiver's 'factory' instance variable to the argument, aFactory."

	factory := aFactory!

schedule
	self subclassResponsibility! !
!SchedulingStrategy categoriesFor: #factory!accessing!public! !
!SchedulingStrategy categoriesFor: #factory:!accessing!public! !
!SchedulingStrategy categoriesFor: #schedule!public! !

Timespan guid: (GUID fromString: '{87468CA2-9C3D-11D3-8287-00001D19F5C2}')!
Timespan comment: ''!
!Timespan categoriesForClass!Unclassified! !
!Timespan methodsFor!

contains: aTimeStampOrTimespan
	^aTimeStampOrTimespan isContainedBy: self!

endTimeStamp
	^endTimeStamp!

endTimeStamp: aTimeStamp
	endTimeStamp := aTimeStamp!

isContainedBy: aTimespan
	^(aTimespan contains: self startTimeStamp) and: [ aTimespan contains: self endTimeStamp ]!

overlaps: aTimespan
	^((aTimespan contains: self startTimeStamp) or: [ aTimespan contains: self endTimeStamp ]) or:
		[ (self contains: aTimespan startTimeStamp) or: [ self contains: aTimespan endTimeStamp ] ]!

printOn: aStream
	self startTimeStamp date printOn: aStream longPicture: false.
	aStream
		nextPut: $ ;
		print: self startTimeStamp time;
		display: ' to '.
	self endTimeStamp date printOn: aStream longPicture: false.
	aStream
		nextPut: $ ;
		print: self endTimeStamp time!

startTimeStamp
	^startTimeStamp!

startTimeStamp: aTimeStamp
	startTimeStamp := aTimeStamp! !
!Timespan categoriesFor: #contains:!public! !
!Timespan categoriesFor: #endTimeStamp!accessing!public! !
!Timespan categoriesFor: #endTimeStamp:!accessing!public! !
!Timespan categoriesFor: #isContainedBy:!public! !
!Timespan categoriesFor: #overlaps:!public! !
!Timespan categoriesFor: #printOn:!ANSI protocols-Object!public! !
!Timespan categoriesFor: #startTimeStamp!accessing!public! !
!Timespan categoriesFor: #startTimeStamp:!accessing!public! !

Workcenter guid: (GUID fromString: '{B72A8D34-6C42-11D3-8276-00001D19F5C2}')!
Workcenter comment: 'A work unit capable of performing one or more Activities.'!
!Workcenter categoriesForClass!Scheduling! !
!Workcenter methodsFor!

activities
	"Answer the value of the receiver's 'activities' instance variable."

	activities isNil ifTrue: [ activities := OrderedCollection new ].
	^activities!

activities: anObject
	"Private - Set the value of the receiver's 'activities' instance variable to the argument, anObject."

	activities := anObject!

addActivity: anActivity
	self activities add: anActivity!

addCapacity: aCapacity
	self capacities add: aCapacity!

addOperation: anOperation
	self operations add: anOperation!

allLocalPredecessorsOf: anOperation
	"Answers a collection of all operations which precede the specified
	operation, either directly or indirectly, at this workcenter."

	| oc oldPred pred |

	oc := OrderedCollection new.
	pred := anOperation predecessorsAt: self.
	[ oc addAll: pred.
	   oldPred := pred.
	   pred := OrderedCollection new.
	   oldPred do: [ :anOper | pred addAll: (anOper predecessorsAt: self) ].
	   pred size > 0 ] whileTrue.
	^oc asSet!

capacities
	"Answer the value of the receiver's 'capacities' instance variable."

	capacities isNil ifTrue: [ self capacities: OrderedCollection new ].
	^capacities!

capacities: anObject
	"Private - Set the value of the receiver's 'capacities' instance variable to the argument, anObject."

	capacities := anObject!

displayOn: aStream
	aStream display: self name; cr.
	self scheduledOperations do: [ :aCollection |
		aCollection do: [ :anOper | aStream display: anOper; cr ] ]!

latestOperation
	"Scan the existing operations assigned to this workcenter looking for the one with the
	 latest LFT.  Answer this operation."

	| latestOperation |

	self operations do: [ :anOper |
		((latestOperation isNil) or: [ (anOper latestFinishTime ~~ nil) and:
								[ latestOperation latestFinishTime > anOper latestFinishTime ] ])
			ifTrue: [latestOperation := anOper ] ].
	^latestOperation!

name
	"Answer the value of the receiver's 'name' instance variable."

	^name!

name: anObject
	"Set the value of the receiver's 'name' instance variable to the argument, anObject."

	name := anObject!

nextOperation
	"Answer the operation which should be performed next."

	self operations do: [ :anOper |
		anOper predecessors size = 0 ifTrue: [ ^anOper ] ].
	^nil!

operations
	"Answer the value of the receiver's 'operations' instance variable."

	operations isNil ifTrue: [ self operations: OrderedCollection new ].
	^operations!

operations: anObject
	"Private - Set the value of the receiver's 'operations' instance variable to the argument, anObject."

	operations := anObject!

printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		display: self name;
		nextPut: $)!

removeOperation: anOperation
	self operations remove: anOperation ifAbsent: [ ]!

runTime: anOperation
	"Answer the amount of time required to run the given operation"

	^self runtimeCalculator runTime: anOperation!

runtimeCalculator
	"Answer the value of the receiver's 'runtimeCalculator' instance variable."

	^runtimeCalculator!

runtimeCalculator: anObject
	"Set the value of the receiver's 'runtimeCalculator' instance variable to the argument, anObject."

	runtimeCalculator := anObject!

scheduledOperations
	"Answers a collection of collections of operations in scheduled order.  Each collection within the
	 answered collection is a group of operations, the top-most of which can be immediately dispatched."

	| oc oc2 |

	oc := OrderedCollection new.
	0 to: self operations size-1 do: [ :i |
		oc2 := OrderedCollection new.
		self operations do: [ :anOper | (anOper predecessorsAt: self) size = i ifTrue: [ oc2 add: anOper ] ].
		oc2 size > 0 ifTrue: [ oc add: oc2 ] ].
	^oc!

unconstrainedOperationPairs: aSchedulingStrategy
	"Answer a collection of pairs of operations which have not been mutually constrained.
	 TODO: improve this.  Current implementation is O((N^2 - N) / 2)."

	| pairs op1 op2 |

	pairs := OrderedCollection new.

	1 to: self operations size - 1 do: [ :i |
		op1 := self operations at: i.
		i+1 to: self operations size do: [ :j |
			op2 := self operations at: j.
			(((op1 complete == false) & (op2 complete == false))
					and: [ op1 notScheduledWith: op2 ]) ifTrue: [
				pairs add: (OperationPair new
							i: op1 j: op2;
							schedulingStrategy: aSchedulingStrategy ) ] ] ].
	^pairs! !
!Workcenter categoriesFor: #activities!accessing!public! !
!Workcenter categoriesFor: #activities:!accessing!private! !
!Workcenter categoriesFor: #addActivity:!public! !
!Workcenter categoriesFor: #addCapacity:!public! !
!Workcenter categoriesFor: #addOperation:!public! !
!Workcenter categoriesFor: #allLocalPredecessorsOf:!public! !
!Workcenter categoriesFor: #capacities!accessing!public! !
!Workcenter categoriesFor: #capacities:!accessing!private! !
!Workcenter categoriesFor: #displayOn:!public! !
!Workcenter categoriesFor: #latestOperation!public! !
!Workcenter categoriesFor: #name!accessing!public! !
!Workcenter categoriesFor: #name:!accessing!public! !
!Workcenter categoriesFor: #nextOperation!public! !
!Workcenter categoriesFor: #operations!accessing!public! !
!Workcenter categoriesFor: #operations:!accessing!private! !
!Workcenter categoriesFor: #printOn:!public! !
!Workcenter categoriesFor: #removeOperation:!public! !
!Workcenter categoriesFor: #runTime:!operations!public! !
!Workcenter categoriesFor: #runtimeCalculator!accessing!public! !
!Workcenter categoriesFor: #runtimeCalculator:!accessing!public! !
!Workcenter categoriesFor: #scheduledOperations!public! !
!Workcenter categoriesFor: #unconstrainedOperationPairs:!public! !

!Workcenter class methodsFor!

new
	^super new initialize! !
!Workcenter class categoriesFor: #new!public! !

FixedConstraint guid: (GUID fromString: '{EF9C2910-CAAD-11D3-8294-00001D19F5C2}')!
FixedConstraint comment: 'Represents constraints which should never be removed.  Used to maintain sequencing relationships within a job.'!
!FixedConstraint categoriesForClass!Unclassified! !
!FixedConstraint methodsFor!

isFixedConstraint
	^true! !
!FixedConstraint categoriesFor: #isFixedConstraint!public!testing! !

DeliveryDateGeneratingFactory guid: (GUID fromString: '{AC2790D1-BA6A-11D4-BDF6-00010240D5E2}')!
DeliveryDateGeneratingFactory comment: 'A factory capable of generating delivery dates for orders, rather than having to have them supplied by an outside agency.'!
!DeliveryDateGeneratingFactory categoriesForClass!Unclassified! !
!DeliveryDateGeneratingFactory methodsFor!

addJob: aJob
	super addJob: aJob.
	self generateDatesFor: aJob.
	^aJob!

buildWith: aFactoryBuilder scheduleWith: aSchedulingStrategy
	aFactoryBuilder build: self.
	self strategy: aSchedulingStrategy!

generateDatesFor: aJob
	| latestOperation currentOper lft |

	"Forward propagate normally to get start times."

	aJob forwardPropagateEarliestStartTimes.

	"For each workcenter which is to perform an operation for this job, find the maximum existing LFT
	and generate an LFT for the new operation by adding in the runtime for the new operation."

	aJob operations do:
		[ :anOper |
		latestOperation := anOper workcenter latestOperation.
		latestOperation latestFinishTime notNil
			ifTrue: [ lft := TimeStamp fromSeconds:
							(latestOperation latestFinishTime asSeconds + anOper runTime seconds value)]
			ifFalse: [ lft := TimeStamp fromSeconds:
							(TimeStamp current asSeconds + anOper runTime seconds value)].
		anOper latestFinishTime: lft ].

	"Now propagate the finish times forward, looking for inconsistencies (i.e. propagated time < time
	generated above).  When an inconsistency is found take the generated time and continue propagating
	with that.  When complete, backward propagate the LFT of the final operations to generate feasible
	LFTs for all operations in the job."

	aJob forwardPropagateLatestFinishTimes.
	aJob backwardPropagateLatestFinishTimes!

propagateDates
	self jobs do: [ :aJob | self generateDatesFor: aJob ]! !
!DeliveryDateGeneratingFactory categoriesFor: #addJob:!adding!public! !
!DeliveryDateGeneratingFactory categoriesFor: #buildWith:scheduleWith:!operations!public! !
!DeliveryDateGeneratingFactory categoriesFor: #generateDatesFor:!helpers!private! !
!DeliveryDateGeneratingFactory categoriesFor: #propagateDates!operations!public! !

TestFactoryBuilder guid: (GUID fromString: '{48F27D31-9624-11D3-8287-00001D19F5C2}')!
TestFactoryBuilder comment: ''!
!TestFactoryBuilder categoriesForClass!Scheduling! !
!TestFactoryBuilder methodsFor!

createActivities
	self factory
		name: 'Test Factory';
		dropAllActivities.
	self factory
		addActivity: (Activity new name: 'Melt'; description: 'Melt new steel');
		addActivity: (Activity new name: 'Roll'; description: 'Roll product to shape');
		addActivity: (Activity new name: 'Pierce'; description: 'Make a hole in a bar');
		addActivity: (Activity new name: 'Pickle'; description: 'Clean the outside by dipping in acid');
		addActivity: (Activity new name: 'Prep'; description: 'Prepare the product for shipping');
		addActivity: (Activity new name: 'Ship'; description: 'Put it on a truck and get it out of here')!

createJob1
	| job oper prevOper |

	job := Job new
			id: 'Job 1';
			startDate: TimeStamp current;
			requiredDate: (TimeStamp fromSeconds: TimeStamp current asSeconds + (15 days seconds value)).

	oper := Operation new
			job: job;
			id: 'Oper 1';
			activity: (self factory activities at: 'Melt');
			workcenter: (self factory workcenters at: 'Melt 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 11 inches;
											s2: 14 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 2';
			activity: (self factory activities at: 'Roll');
			workcenter: (self factory workcenters at: 'Roll 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 8 inches;
											s2: 8 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 3';
			activity: (self factory activities at: 'Roll');
			workcenter: (self factory workcenters at: 'Roll 2');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 6 inches;
											s2: 0 inches;
											shape: RoundSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 4';
			activity: (self factory activities at: 'Pierce');
			workcenter: (self factory workcenters at: 'Pierce 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 5.75 inches;
											s2: 0.6 inches;
											shape: RoundTube new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 5';
			activity: (self factory activities at: 'Prep');
			workcenter: (self factory workcenters at: 'Prep 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 5.75 inches;
											s2: 0.6 inches;
											shape: RoundTube new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 6';
			activity: (self factory activities at: 'Ship');
			workcenter: (self factory workcenters at: 'Ship 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 5.75 inches;
											s2: 0.6;
											shape: RoundTube new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	^job!

createJob2
	| job oper prevOper |

	job := Job new
			id: 'Job 2';
			startDate: TimeStamp current;
			requiredDate: (TimeStamp fromSeconds: TimeStamp current asSeconds + (15 days seconds value)).

	oper := Operation new
			job: job;
			id: 'Oper 1';
			activity: (self factory activities at: 'Melt');
			workcenter: (self factory workcenters at: 'Melt 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 11 inches;
											s2: 14 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 2';
			activity: (self factory activities at: 'Roll');
			workcenter: (self factory workcenters at: 'Roll 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 7.25 inches;
											s2: 7.25 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 3';
			activity: (self factory activities at: 'Prep');
			workcenter: (self factory workcenters at: 'Prep 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 7.25 inches;
											s2: 7.25 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 4';
			activity: (self factory activities at: 'Ship');
			workcenter: (self factory workcenters at: 'Ship 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 7.25 inches;
											s2: 7.25 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	^job!

createJob3
	| job oper prevOper |

	job := Job new
			id: 'Job 3';
			startDate: TimeStamp current;
			requiredDate: (TimeStamp fromSeconds: TimeStamp current asSeconds + (15 days seconds value)).

	oper := Operation new
			job: job;
			id: 'Oper 1';
			activity: (self factory activities at: 'Melt');
			workcenter: (self factory workcenters at: 'Melt 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 12 inches;
											s2: 12 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 2';
			activity: (self factory activities at: 'Roll');
			workcenter: (self factory workcenters at: 'Roll 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 9 inches;
											s2: 9 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 3';
			activity: (self factory activities at: 'Roll');
			workcenter: (self factory workcenters at: 'Roll 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 6.25 inches;
											s2: 0 inches;
											shape: RoundSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 4';
			activity: (self factory activities at: 'Pierce');
			workcenter: (self factory workcenters at: 'Pierce 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 4.5 inches;
											s2: 0.5 inches;
											shape: RoundTube new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 5';
			activity: (self factory activities at: 'Prep');
			workcenter: (self factory workcenters at: 'Prep 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 4.5 inches;
											s2: 0.5 inches;
											shape: RoundTube new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 6';
			activity: (self factory activities at: 'Ship');
			workcenter: (self factory workcenters at: 'Ship 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 4.5 inches;
											s2: 0.5 inches;
											shape: RoundTube new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	^job!

createJob4
	| job oper prevOper |

	job := Job new
			startDate: TimeStamp current;
			requiredDate: (TimeStamp fromSeconds: TimeStamp current asSeconds + (15 days seconds value)).

	oper := Operation new
			job: job;
			id: 'Oper 1';
			activity: (self factory activities at: 'Melt');
			workcenter: (self factory workcenters at: 'Melt 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 11 inches;
											s2: 14 inches;
											shape: RectangularSolid new)
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 6 inches;
											s2: 6 inches;
											shape: RectangularSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 3';
			activity: (self factory activities at: 'Roll');
			workcenter: (self factory workcenters at: 'Roll 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 4.75 inches;
											s2: 0 inches;
											shape: RoundSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 4';
			activity: (self factory activities at: 'Pickle');
			workcenter: (self factory workcenters at: 'Pickle 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 4.75 inches;
											s2: 0 inches;
											shape: RoundSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 5';
			activity: (self factory activities at: 'Prep');
			workcenter: (self factory workcenters at: 'Prep 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 4.75 inches;
											s2: 0 inches;
											shape: RoundSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	oper := Operation new
			job: job;
			id: 'Oper 6';
			activity: (self factory activities at: 'Ship');
			workcenter: (self factory workcenters at: 'Ship 1');
			outputProduct: (PlannedProduct new
							characteristics: (ProductCharacteristics new
											material: Steel current;
											s1: 4.75 inches;
											s2: 0 inches;
											shape: RoundSolid new);
							quantity: 30000 pounds).
	job addOperation: oper after: prevOper.
	prevOper := oper.

	^job!

createJobs
	self factory
		addJob: self createJob1;
		addJob: self createJob2;
		addJob: self createJob3;
		addJob: self createJob4.!

createWorkcenters
	self factory
		addWorkcenter: 
			(Workcenter new
				name: 'Melt 1';
				runtimeCalculator: (ConstantRuntimeCalculator new value: 4 hours);
				addActivity: (self factory activities at: 'Melt');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
									startTimeStamp: TimeStamp current;
									endTimeStamp: (TimeStamp fromSeconds: 										(TimeStamp current asSeconds + (1 years seconds value))))));
		addWorkcenter:
			(Workcenter new
				name: 'Roll 1';
				runtimeCalculator:
					(LengthRuntimeCalculator new
						scalingFactor: 20 seconds / Unit feet);
				addActivity: (self factory activities at: 'Roll');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
							startTimeStamp: TimeStamp current;
							endTimeStamp: (TimeStamp fromSeconds:
										(TimeStamp current asSeconds + (1 years seconds value))))));
		addWorkcenter:
			(Workcenter new
				name: 'Roll 2';
				runtimeCalculator: (LengthRuntimeCalculator new
						scalingFactor: 15 seconds / Unit feet);
				addActivity: (self factory activities at: 'Roll');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
							startTimeStamp: TimeStamp current;
							endTimeStamp: (TimeStamp fromSeconds:
										(TimeStamp current asSeconds + (1 years seconds value))))));
		addWorkcenter:
			(Workcenter new
				name: 'Roll 3';
				runtimeCalculator: (LengthRuntimeCalculator new
								scalingFactor: 30 seconds / Unit feet);
				addActivity: (self factory activities at: 'Roll');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
							startTimeStamp: TimeStamp current;
							endTimeStamp: (TimeStamp fromSeconds:
										(TimeStamp current asSeconds + (1 years seconds value))))));
		addWorkcenter:
			(Workcenter new
				name: 'Pierce 1';
				runtimeCalculator: (LengthRuntimeCalculator new
								scalingFactor: 1 minutes / Unit feet);
				addActivity: (self factory activities at: 'Pierce');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
							startTimeStamp: TimeStamp current;
							endTimeStamp: (TimeStamp fromSeconds:
										(TimeStamp current asSeconds + (1 years seconds value))))));
		addWorkcenter:
			(Workcenter new
				name: 'Pierce 2';
				runtimeCalculator: (LengthRuntimeCalculator new
								scalingFactor: 3 minutes / Unit feet);
				addActivity: (self factory activities at: 'Pierce');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
							startTimeStamp: TimeStamp current;
							endTimeStamp: (TimeStamp fromSeconds:
										(TimeStamp current asSeconds + (1 years seconds value))))));
		addWorkcenter:
			(Workcenter new
				name: 'Pickle 1';
				runtimeCalculator: (WeightRuntimeCalculator new
									scalingFactor: 1 seconds / 10 pounds);
				addActivity: (self factory activities at: 'Pickle');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
							startTimeStamp: TimeStamp current;
							endTimeStamp: (TimeStamp fromSeconds:
										(TimeStamp current asSeconds + (1 years seconds value))))));
		addWorkcenter:
			(Workcenter new
				name: 'Prep 1';
				runtimeCalculator: (LengthRuntimeCalculator new
									scalingFactor: 5 seconds / Unit feet);
				addActivity: (self factory activities at: 'Roll');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
							startTimeStamp: TimeStamp current;
							endTimeStamp: (TimeStamp fromSeconds:
										(TimeStamp current asSeconds + (1 years seconds value))))));
		addWorkcenter:
			(Workcenter new
				name: 'Ship 1';
				runtimeCalculator: (ConstantRuntimeCalculator new value: 1 hours);
				addActivity: (self factory activities at: 'Ship');
				addCapacity:
					(Capacity new
						value: 100 tons / Unit hours;
						timeSpan: (Timespan new
							startTimeStamp: TimeStamp current;
							endTimeStamp: (TimeStamp fromSeconds:
										(TimeStamp current asSeconds + (1 years seconds value))))))! !
!TestFactoryBuilder categoriesFor: #createActivities!operations!private! !
!TestFactoryBuilder categoriesFor: #createJob1!private!private helpers! !
!TestFactoryBuilder categoriesFor: #createJob2!private!private helpers! !
!TestFactoryBuilder categoriesFor: #createJob3!private!private helpers! !
!TestFactoryBuilder categoriesFor: #createJob4!private!private helpers! !
!TestFactoryBuilder categoriesFor: #createJobs!operations!private! !
!TestFactoryBuilder categoriesFor: #createWorkcenters!operations!private! !

Aluminum guid: (GUID fromString: '{AC8B3682-9E87-11D3-8287-00001D19F5C2}')!
Aluminum comment: ''!
!Aluminum categoriesForClass!Unclassified! !
!Aluminum methodsFor!

poundsPerCubicInch
	^0.097543768 pounds! !
!Aluminum categoriesFor: #poundsPerCubicInch!accessing!public! !

Steel guid: (GUID fromString: '{0C2D17F2-9878-11D3-8287-00001D19F5C2}')!
Steel comment: ''!
!Steel categoriesForClass!Scheduling! !
!Steel methodsFor!

poundsPerCubicInch
	^0.284466529 pounds! !
!Steel categoriesFor: #poundsPerCubicInch!accessing!public! !

Titanium guid: (GUID fromString: '{AC8B3683-9E87-11D3-8287-00001D19F5C2}')!
Titanium comment: ''!
!Titanium categoriesForClass!Unclassified! !
!Titanium methodsFor!

poundsPerCubicInch
	^0.162825838 pounds! !
!Titanium categoriesFor: #poundsPerCubicInch!accessing!public! !

ActualProduct guid: (GUID fromString: '{87C8F9A3-8308-11D3-8283-00001D19F5C2}')!
ActualProduct comment: 'Product which has actually been produced.'!
!ActualProduct categoriesForClass!Scheduling! !
!ActualProduct methodsFor!

isActualProduct
	^true! !
!ActualProduct categoriesFor: #isActualProduct!public! !

PlannedProduct guid: (GUID fromString: '{87C8F9A2-8308-11D3-8283-00001D19F5C2}')!
PlannedProduct comment: 'Product which is planned to be produced.'!
!PlannedProduct categoriesForClass!Scheduling! !
!PlannedProduct methodsFor!

isPlannedProduct
	^true! !
!PlannedProduct categoriesFor: #isPlannedProduct!public! !

HexagonalSolid guid: (GUID fromString: '{C92FC9A1-990D-11D3-8287-00001D19F5C2}')!
HexagonalSolid comment: ''!
!HexagonalSolid categoriesForClass!Scheduling! !
!HexagonalSolid methodsFor!

area: aProductCharacteristics
	^(aProductCharacteristics s1 / 2) squared * (3 sqrt * 2)!

shortDescription
	^'hexagonal bar'! !
!HexagonalSolid categoriesFor: #area:!public! !
!HexagonalSolid categoriesFor: #shortDescription!private! !

OctagonalSolid guid: (GUID fromString: '{C92FC9A2-990D-11D3-8287-00001D19F5C2}')!
OctagonalSolid comment: ''!
!OctagonalSolid categoriesForClass!Scheduling! !
!OctagonalSolid methodsFor!

area: aProductCharacteristics
	^(aProductCharacteristics s1 / 2) squared * 8 * (2 sqrt - 1)!

shortDescription
	^'octagonal bar'! !
!OctagonalSolid categoriesFor: #area:!public! !
!OctagonalSolid categoriesFor: #shortDescription!private! !

RectangularSolid guid: (GUID fromString: '{B72A8D47-6C42-11D3-8276-00001D19F5C2}')!
RectangularSolid comment: ''!
!RectangularSolid categoriesForClass!Scheduling! !
!RectangularSolid methodsFor!

area: aProductCharacteristics
	^aProductCharacteristics s1 inches * aProductCharacteristics s2 inches!

shortDescription
	^'rectangular bar'! !
!RectangularSolid categoriesFor: #area:!public! !
!RectangularSolid categoriesFor: #shortDescription!private! !

RoundSolid guid: (GUID fromString: '{B72A8D44-6C42-11D3-8276-00001D19F5C2}')!
RoundSolid comment: ''!
!RoundSolid categoriesForClass!Scheduling! !
!RoundSolid methodsFor!

area: aProductCharacteristics
	^self areaForDiameter: aProductCharacteristics s1!

areaForDiameter: aUnitValue
	"Answers the area for a round of the given diameter, in square inches"
	^Float pi * (aUnitValue inches / 2) squared!

shortDescription
	^'round bar'! !
!RoundSolid categoriesFor: #area:!public! !
!RoundSolid categoriesFor: #areaForDiameter:!private! !
!RoundSolid categoriesFor: #shortDescription!private! !

RoundTube guid: (GUID fromString: '{B72A8D46-6C42-11D3-8276-00001D19F5C2}')!
RoundTube comment: ''!
!RoundTube categoriesForClass!Scheduling! !
!RoundTube methodsFor!

area: aProductCharacteristics
	^(RoundSolid new areaForDiameter: aProductCharacteristics s1) - 
		(RoundSolid new areaForDiameter: aProductCharacteristics s1 - (2 * aProductCharacteristics s2))
!

shortDescription
	^'round tube'! !
!RoundTube categoriesFor: #area:!public! !
!RoundTube categoriesFor: #shortDescription!private! !

ConstantRuntimeCalculator guid: (GUID fromString: '{81458750-8D41-11D3-8285-00001D19F5C2}')!
ConstantRuntimeCalculator comment: 'A RuntimeCalcuator which answers a constant value, i.e. any workcenter which makes use of this type of runtime calculator will take the same amount of time for all operations.'!
!ConstantRuntimeCalculator categoriesForClass!Scheduling! !
!ConstantRuntimeCalculator methodsFor!

runTime: anOperation
	^self value!

value
	"Answer the value of the receiver's ''value'' instance variable."

	^value!

value: aUnitValue
	"Set the value of the receiver's ''value'' instance variable to the argument, aUnitValue."

	value := aUnitValue! !
!ConstantRuntimeCalculator categoriesFor: #runTime:!public! !
!ConstantRuntimeCalculator categoriesFor: #value!accessing!public! !
!ConstantRuntimeCalculator categoriesFor: #value:!accessing!public! !

ScaledRuntimeCalculator guid: (GUID fromString: '{DA9923B2-97AE-11D3-8287-00001D19F5C2}')!
ScaledRuntimeCalculator comment: ''!
!ScaledRuntimeCalculator categoriesForClass!Scheduling! !
!ScaledRuntimeCalculator methodsFor!

defaultScalingFactor
	self subclassResponsibility!

initialize
	super initialize.
	self scalingFactor: self defaultScalingFactor!

scalingFactor
	"Answer the value of the receiver's ''scalingFactor'' instance variable."

	^scalingFactor!

scalingFactor: aUnitValue
	"Set the value of the receiver's ''scalingFactor'' instance variable to the argument, aUnitValue.  	 	 This UnitValue should represent the amount of time required to process one unit of product."

	scalingFactor := aUnitValue! !
!ScaledRuntimeCalculator categoriesFor: #defaultScalingFactor!public! !
!ScaledRuntimeCalculator categoriesFor: #initialize!public! !
!ScaledRuntimeCalculator categoriesFor: #scalingFactor!accessing!private! !
!ScaledRuntimeCalculator categoriesFor: #scalingFactor:!accessing!public! !

!ScaledRuntimeCalculator class methodsFor!

new
	^super new initialize! !
!ScaledRuntimeCalculator class categoriesFor: #new!public! !

LengthRuntimeCalculator guid: (GUID fromString: '{DA9923B0-97AE-11D3-8287-00001D19F5C2}')!
LengthRuntimeCalculator comment: 'Calculates runtime based on the length of the input product and a linear scaling factor.'!
!LengthRuntimeCalculator categoriesForClass!Scheduling! !
!LengthRuntimeCalculator methodsFor!

defaultScalingFactor
	^1 feet / Unit seconds!

runTime: anOperation
	^anOperation inputFeet  * self scalingFactor! !
!LengthRuntimeCalculator categoriesFor: #defaultScalingFactor!public! !
!LengthRuntimeCalculator categoriesFor: #runTime:!public! !

WeightRuntimeCalculator guid: (GUID fromString: '{DA9923B3-97AE-11D3-8287-00001D19F5C2}')!
WeightRuntimeCalculator comment: ''!
!WeightRuntimeCalculator categoriesForClass!Scheduling! !
!WeightRuntimeCalculator methodsFor!

defaultScalingFactor
	^1 second / 100 pounds!

runTime: anOperation
	^anOperation inputPounds * self scalingFactor! !
!WeightRuntimeCalculator categoriesFor: #defaultScalingFactor!public! !
!WeightRuntimeCalculator categoriesFor: #runTime:!public! !

ConstraintSchedulingStrategy guid: (GUID fromString: '{D4CEB0CA-8306-404E-8452-194017147494}')!
ConstraintSchedulingStrategy comment: ''!
!ConstraintSchedulingStrategy categoriesForClass!Unclassified! !
!ConstraintSchedulingStrategy methodsFor!

postSchedulingConstraints
	self subclassResponsibility!

schedule
	self postSchedulingConstraints! !
!ConstraintSchedulingStrategy categoriesFor: #postSchedulingConstraints!private! !
!ConstraintSchedulingStrategy categoriesFor: #schedule!public! !

PCPSchedulingStrategy guid: (GUID fromString: '{6CF7A871-A0FA-11D3-8288-00001D19F5C2}')!
PCPSchedulingStrategy comment: 'This class implements the Precedence Constraint Posting (PCP) algorithm developed by Dr. Stephen Smith and Dr. Cheng-Chung Cheng at Carnegie Mellon University.  See "Slack-Based Heuristics For Constraint Satisfaction Scheduling", Smith, Stephen F. and Cheng-Chung Cheng, in Proceedings of AAAI-93, ppg. 139-144.  Washington, D.C.  This paper can be obtained by contacting Dr. Smith at sfs@cs.cmu.edu.

Note that this class implements a simple workaround to a basic problem encountered when applying constraint based analysis (CBA).  As originally specified, when the runtime for a pair of operations exceeds the total time available to perform them the entire schedule is taken to be invalid.  This is done because one of the assumptions behind CBA is that the start and finish times of operations is inviolable and may not be altered.  In the case of this implementation the assumption made is different.  Here it is assumed that the finish times of operations can be altered, when necessary, to ensure that a feasible schedule can be generated; in other words it''s better to generate a feasible schedule, even if it requires making some orders late, than it is to generate no schedule.'!
!PCPSchedulingStrategy categoriesForClass!Unclassified! !
!PCPSchedulingStrategy methodsFor!

analyze: aWorkcenter
	self performConstraintAnalysisFor: aWorkcenter!

applyOrderingHeuristicsFor: aWorkcenter
	| unconstrainedPairs constraint minSlackPair |

	"Apply min-slack rule to determine which unordered pair should be ordered next."

	unconstrainedPairs := aWorkcenter unconstrainedOperationPairs: self.
	unconstrainedPairs size > 0 ifTrue: [
		minSlackPair := unconstrainedPairs first.
		unconstrainedPairs do: [ :aPair |
			aPair minSlack < minSlackPair minSlack ifTrue: [ minSlackPair := aPair ] ].

		"Apply max-slack rule to determine which operation should be done first."

		minSlackPair slackIj > minSlackPair slackJi
			ifTrue: [ constraint := Constraint new before: minSlackPair i; after: minSlackPair j ]
			ifFalse: [ constraint := Constraint new before: minSlackPair j; after: minSlackPair i ].
		self propagateConstraint: constraint ]!

basicSlackBetween: operA and: operB
	^operB latestFinishTime asSeconds - operA earliestStartTime asSeconds -
		(operA runTime seconds value + operB runTime seconds value)!

performConstraintAnalysisFor: aWorkcenter
	| constraint pairs |

	(pairs := aWorkcenter unconstrainedOperationPairs: self) do: [ :aPair |
		(constraint := self schedule: aPair) notNil ifTrue: [ self propagateConstraint: constraint ] ]!

postSchedulingConstraints
	"Private - Examine the jobs to be run at the given factory, scheduling operations appropriately."

	self factory workcenters do: [ :aWorkcenter |
		self analyze: aWorkcenter ]!

propagateConstraint: aConstraint
	| before after |

	before := aConstraint before.
	after := aConstraint after.

	after earliestStartTime:
		(TimeStamp fromSeconds: (after earliestStartTime asSeconds max:
					(before earliestStartTime asSeconds + before runTime seconds value))).

	before latestFinishTime:
		(TimeStamp fromSeconds: (before latestFinishTime asSeconds min:
					(after latestFinishTime asSeconds - after runTime seconds value))).

	after forwardPropagateEstFrom: after earliestStartTime.
	before backwardPropagateLftFrom: before latestFinishTime!

schedule: anOperationPair
	^self schedule: anOperationPair i with: anOperationPair j!

schedule: operA with: operB
	"Generate a scheduling constraint for operA vs. operB.  Both should be assigned to the same workcenter."

	| lftA lftB estA estB runtimeA runtimeB slackAB slackBA totalRuntime
	  lftAminusEstB lftBminusEstA requiredAdditionalSlack |

	lftA := operA latestFinishTime asSeconds.
	lftB := operB latestFinishTime asSeconds.
	estA := operA earliestStartTime asSeconds.
	estB := operB earliestStartTime asSeconds.
	runtimeA := operA runTime seconds value.
	runtimeB := operB runTime seconds value.
	totalRuntime := runtimeA + runtimeB.
	lftAminusEstB := lftA - estB.
	lftBminusEstA := lftB - estA.

	"CBA"

	"Case 1 : operA must be scheduled before operB"

	(lftAminusEstB < totalRuntime) & (totalRuntime <= lftBminusEstA)
		ifTrue: [ ^Constraint new before: operA; after: operB ].

	"Case 2 : operB must be scheduled before operA"

	(lftBminusEstA < totalRuntime) & (totalRuntime <= lftAminusEstB)
		ifTrue: [ ^Constraint new before: operB; after: operA ].

	"Case 3 : No feasible solution exists with the current times and constraints, so bump out the LFT of
	 the operation which already has the latest LFT and try again."

	(totalRuntime > lftBminusEstA) & (totalRuntime > lftAminusEstB)
		ifTrue: [ Transcript
				show: operA displayString;
				display:  ' infeasible with ';
				show: operB displayString;
				display: ' at ', operA workcenter name; cr.
				requiredAdditionalSlack := (totalRuntime - lftBminusEstA) max: (totalRuntime - lftAminusEstB).
				lftA > lftB
					ifTrue: [ operA forwardPropagateLftFrom:
							(TimeStamp fromSeconds: (operA latestFinishTime asSeconds + requiredAdditionalSlack))]
					ifFalse: [ operB forwardPropagateLftFrom:
							(TimeStamp fromSeconds: (operB latestFinishTime asSeconds + requiredAdditionalSlack))].
				^self schedule: operA with: operB ].

	"Case 4"	"Either decision is still feasible, so apply the slack rules"

	self applyOrderingHeuristicsFor: operA workcenter.
	^nil!

slackBetween: operA and: operB
	^self basicSlackBetween: operA and: operB! !
!PCPSchedulingStrategy categoriesFor: #analyze:!private! !
!PCPSchedulingStrategy categoriesFor: #applyOrderingHeuristicsFor:!private! !
!PCPSchedulingStrategy categoriesFor: #basicSlackBetween:and:!private! !
!PCPSchedulingStrategy categoriesFor: #performConstraintAnalysisFor:!private! !
!PCPSchedulingStrategy categoriesFor: #postSchedulingConstraints!private! !
!PCPSchedulingStrategy categoriesFor: #propagateConstraint:!private! !
!PCPSchedulingStrategy categoriesFor: #schedule:!private! !
!PCPSchedulingStrategy categoriesFor: #schedule:with:!private! !
!PCPSchedulingStrategy categoriesFor: #slackBetween:and:!public! !

SPPCPSchedulingStrategy guid: (GUID fromString: '{F306735B-CB4A-476D-B830-6D07050EA22A}')!
SPPCPSchedulingStrategy comment: 'This class implements the Shortest Path-based Precedence Constraint Posting (SP-PCP) algorithm developed by Dr. Stephen Smith and Dr. Cheng-Chung Cheng at Carnegie Mellon University.  See "Generating Feasible Schedules under Complex Metric Constraints", Smith, Stephen F. and Cheng-Chung Cheng, in Proceedings of AAAI-94.  This paper can be obtained by contacting Dr. Smith at sfs@cs.cmu.edu.'!
!SPPCPSchedulingStrategy categoriesForClass!Unclassified! !
!SPPCPSchedulingStrategy methodsFor!

postSchedulingConstraints
	self notYetImplemented! !
!SPPCPSchedulingStrategy categoriesFor: #postSchedulingConstraints!public! !

BiasedPCPSchedulingStrategy guid: (GUID fromString: '{142BB231-B0FD-11D4-BDF6-00010240D5E2}')!
BiasedPCPSchedulingStrategy comment: 'This class incorporates the simple biased slack improvements to the PCP algorithm detailed in the "Incorporating Additional Search Bias" portion of Smith & Cheng''s paper, "Slack-Based Heuristics For Constraint Satisfaction Scheduling".'!
!BiasedPCPSchedulingStrategy categoriesForClass!Unclassified! !
!BiasedPCPSchedulingStrategy methodsFor!

biasedSlackBetween: operA and: operB root: n
	| similarity |
	similarity := self similarityBetween: operA and: operB.
	^(self basicSlackBetween: operA and: operB) / ((similarity abs raisedTo: 1 / n) * similarity sign)!

initialize
	super initialize.
	self root: 2!

root
	^root!

root: anInteger
	root := anInteger!

similarityBetween: operA and: operB
	| slackAB slackBA |
	slackAB := self basicSlackBetween: operA and: operB.
	slackBA := self basicSlackBetween: operB and: operA.
	^(slackAB min: slackBA) / (slackAB max: slackBA)!

slackBetween: operA and: operB
	^self biasedSlackBetween: operA and: operB root: self root! !
!BiasedPCPSchedulingStrategy categoriesFor: #biasedSlackBetween:and:root:!private! !
!BiasedPCPSchedulingStrategy categoriesFor: #initialize!public! !
!BiasedPCPSchedulingStrategy categoriesFor: #root!accessing!public! !
!BiasedPCPSchedulingStrategy categoriesFor: #root:!accessing!public! !
!BiasedPCPSchedulingStrategy categoriesFor: #similarityBetween:and:!private! !
!BiasedPCPSchedulingStrategy categoriesFor: #slackBetween:and:!public! !

!BiasedPCPSchedulingStrategy class methodsFor!

new
	^super new initialize! !
!BiasedPCPSchedulingStrategy class categoriesFor: #new!public! !

CompositeBiasedPCPSchedulingStrategy guid: (GUID fromString: '{AAA583B1-B18B-11D4-BDF6-00010240D5E2}')!
CompositeBiasedPCPSchedulingStrategy comment: 'This class incorporates the composite biased slack improvements to the PCP algorithm detailed in the "Incorporating Additional Search Bias" portion of Smith & Cheng''s paper, "Slack-Based Heuristics For Constraint Satisfaction Scheduling".'!
!CompositeBiasedPCPSchedulingStrategy categoriesForClass!Unclassified! !
!CompositeBiasedPCPSchedulingStrategy methodsFor!

compositeBiasedSlackBetween: operA and: operB
	^(self biasedSlackBetween: operA and: operB root: self root) + (self biasedSlackBetween: operA and: operB root: self root2)!

initialize
	super initialize.
	self root2: 3!

root2
	^root2!

root2: anInteger
	root2 := anInteger!

slackBetween: operA and: operB
	^self compositeBiasedSlackBetween: operA and: operB! !
!CompositeBiasedPCPSchedulingStrategy categoriesFor: #compositeBiasedSlackBetween:and:!private! !
!CompositeBiasedPCPSchedulingStrategy categoriesFor: #initialize!public! !
!CompositeBiasedPCPSchedulingStrategy categoriesFor: #root2!accessing!public! !
!CompositeBiasedPCPSchedulingStrategy categoriesFor: #root2:!accessing!public! !
!CompositeBiasedPCPSchedulingStrategy categoriesFor: #slackBetween:and:!public! !

"Binary Globals"!

"Resources"!

