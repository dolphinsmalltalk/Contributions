| package |
package := Package name: 'US Treemap - Layout Ordered'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Layout Ordered 0.021$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.021'.


package classNames
	add: #AbstractOrderedTreemapStrategy;
	add: #HotrizontalOrderedTreemapLayouter;
	add: #OrderedTreemapLayouter;
	add: #PivotByBiggestStrategy;
	add: #PivotByMiddleStrategy;
	add: #PivotBySplitSizeStrategy;
	add: #VerticalOrderedTreemapLayouter;
	yourself.

package methodNames
	add: #SequenceableCollection -> #sumFrom:to:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'US Treemap - Layout Abstract';
	yourself).

package!

"Class Definitions"!

Object subclass: #OrderedTreemapLayouter
	instanceVariableNames: 'nodes layoutRectangle layoutStrategy nodesSize list1 list2 list3 list2size rectangle1 rectangle2 rectangle3 pivotIndex pivotSize pivot remainingRectangle bestWidth bestHeight bestIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractTreemapLayoutStrategy subclass: #AbstractOrderedTreemapStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractOrderedTreemapStrategy subclass: #PivotByBiggestStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractOrderedTreemapStrategy subclass: #PivotByMiddleStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractOrderedTreemapStrategy subclass: #PivotBySplitSizeStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OrderedTreemapLayouter subclass: #HotrizontalOrderedTreemapLayouter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OrderedTreemapLayouter subclass: #VerticalOrderedTreemapLayouter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!SequenceableCollection methodsFor!

sumFrom: start to: end 
	"Sumarizes all elements' size between start and end"


	^(start to: end) inject: 0 into:  [:sum :index | sum + (self at: index) size].
	! !
!SequenceableCollection categoriesFor: #sumFrom:to:!*-not in class package!operations!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

OrderedTreemapLayouter guid: (GUID fromString: '{4BDDE7B1-5E10-404A-A4D5-76743FCAED70}')!
OrderedTreemapLayouter comment: ''!
!OrderedTreemapLayouter categoriesForClass!Unclassified! !
!OrderedTreemapLayouter methodsFor!

calculateBestBorder
	| width remainigHeight height remainingWidth remainingSize list3size pivotAspectRatio bestAspectRatio |
	remainingWidth := remainingRectangle width.
	remainigHeight := remainingRectangle height.
	remainingSize := nodes sumFrom: pivotIndex to: nodes size.
	list2size := 0.
	list3size := nodes
				sumFrom: pivotIndex + 1
				to: nodes size.
	pivotIndex + 1 to: nodes size
		do: 
			[:i | 
			list2size := list2size + (nodes at: i) size.
			list3size := list3size - (nodes at: i) size.
			width := self calculateBorderWidth: remainingWidth size: remainingSize.
			height := self calculateBorderHeight: remainigHeight size: remainingSize.
			pivotAspectRatio := width / height.
			bestIndex isNil 
				ifTrue: 
					[bestAspectRatio := pivotAspectRatio.
					bestWidth := width.
					bestHeight := height.
					bestIndex := i]
				ifFalse: 
					[(pivotAspectRatio - 1) abs < (bestAspectRatio - 1) abs 
						ifTrue: 
							[bestAspectRatio := pivotAspectRatio.
							bestWidth := width.
							bestHeight := height.
							bestIndex := i]]]!

calculateBorderHeight: remainigHeight size: remainingSize 
self subclassResponsibility!

calculateBorderWidth: remainingWidth size: remainingSize 
	self subclassResponsibility!

compute
	nodes size = 1 
		ifTrue: 
			[self layoutSingleRectangle.
			^self].
	self computeNodesSize.
	nodes size = 2 
		ifTrue: 
			[self layoutTwoRectangles.
			^self].
	self
		computePivot;
		computeList1;
		layoutRectangle1;
		computeRemainingRectangle;
		computeRectangle1AndRectangle2;
		layoutList1;
		layoutList2;
		layoutList3.
	#USToDo	"self tryAlternativeLayouts: nodes in: aRectangle."!

computeLessThanThreeRemainingRectangles
	self subclassResponsibility!

computeList1
	list1 := nodes copyFrom: 1 to: pivotIndex - 1.
	!

computeList2: border 
	list2 := nodes copyFrom: pivotIndex + 1 to: border.
	list2size := list2 sum!

computeList3
	list3 := nodes copyFrom: bestIndex + 1 to: nodes size!

computeMoreThanTwoRemainingRectangles
	self subclassResponsibility!

computeNodesSize
	nodesSize := nodes sum!

computeNoRemainingRectangles
	pivot setLayoutRectangle: remainingRectangle!

computePivot
	pivotIndex := layoutStrategy computePivotIndex: nodes.
	pivot := nodes at: pivotIndex.
	pivotSize := pivot size!

computeRectangle1AndRectangle2
|numberOfRemainingRectangles | 
	numberOfRemainingRectangles  := self numberOfRemainingRectangles.
numberOfRemainingRectangles  >=3
		ifTrue: [self computeMoreThanTwoRemainingRectangles]
		ifFalse: 
			[numberOfRemainingRectangles > 0 
				ifTrue: [self computeLessThanThreeRemainingRectangles]
				ifFalse: [self computeNoRemainingRectangles]]!

computeRemainingRectangle
	self subclassResponsibility!

layoutList: aList inRectangle: aRectangle 
	(aList notNil and: [aList notEmpty]) 
		ifTrue: 
			[aList size > 1 
				ifTrue: [layoutStrategy layout: aList in: aRectangle]
				ifFalse: [(aList first) setLayoutRectangle: aRectangle]]!

layoutList1
self layoutList: list1 inRectangle: rectangle1.
!

layoutList2
	self layoutList: list2 inRectangle: rectangle2!

layoutList3
	self layoutList: list3 inRectangle: rectangle3!

layoutPivotRectangle
	pivot setLayoutRectangle: (remainingRectangle topLeft extent: bestWidth @ bestHeight)!

layoutRectangle1
	self subclassResponsibility!

layoutRectangle3
	self subclassResponsibility!

layoutSingleRectangle
	(nodes first) setLayoutRectangle: layoutRectangle!

layoutTwoRectangles
	self subclassResponsibility!

numberOfRemainingRectangles
	^ nodes size - pivotIndex!

setNodes: aCollection rectangle: aRectangle layoutStrategy: aLayoutStrategy 
	nodes := aCollection.
	layoutRectangle := aRectangle.
	layoutStrategy := aLayoutStrategy! !
!OrderedTreemapLayouter categoriesFor: #calculateBestBorder!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #calculateBorderHeight:size:!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #calculateBorderWidth:size:!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #compute!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeLessThanThreeRemainingRectangles!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeList1!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeList2:!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeList3!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeMoreThanTwoRemainingRectangles!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeNodesSize!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeNoRemainingRectangles!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computePivot!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeRectangle1AndRectangle2!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #computeRemainingRectangle!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutList:inRectangle:!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutList1!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutList2!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutList3!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutPivotRectangle!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutRectangle1!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutRectangle3!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutSingleRectangle!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #layoutTwoRectangles!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #numberOfRemainingRectangles!*-in class package!private! !
!OrderedTreemapLayouter categoriesFor: #setNodes:rectangle:layoutStrategy:!*-in class package!initializing!private! !

!OrderedTreemapLayouter class methodsFor!

nodes: aCollection rectangle: aRectangle layoutStrategy: aLayoutStrategy
	^self new setNodes: aCollection rectangle: aRectangle layoutStrategy: aLayoutStrategy
! !
!OrderedTreemapLayouter class categoriesFor: #nodes:rectangle:layoutStrategy:!*-in class package!public! !

AbstractOrderedTreemapStrategy guid: (GUID fromString: '{F1C4FE3B-2F75-4C67-A17E-804A0CEC4B6F}')!
AbstractOrderedTreemapStrategy comment: ''!
!AbstractOrderedTreemapStrategy categoriesForClass!Unclassified! !
!AbstractOrderedTreemapStrategy methodsFor!

computePivotIndex: aCollection 
	^self subclassResponsibility!

layout: children in: aRectangle 
	aRectangle width / aRectangle height >= 1 
		ifTrue: 
			[(HotrizontalOrderedTreemapLayouter 
				nodes: children
				rectangle: aRectangle
				layoutStrategy: self) compute]
		ifFalse: 
			[(VerticalOrderedTreemapLayouter 
				nodes: children
				rectangle: aRectangle
				layoutStrategy: self) compute]!

layout: node using: renderSettings 
	| newRenderSettings children |

	children := node childrenWithExtent.
	children notEmpty ifTrue: [self layout: children in: node layoutRectangle].
	children do: 
			[:each | 
			newRenderSettings := renderSettings update: each in: each layoutRectangle.
			renderStrategy renderNode: each using: newRenderSettings.
			each isLeafNode ifFalse: [self layout: each using: newRenderSettings]]! !
!AbstractOrderedTreemapStrategy categoriesFor: #computePivotIndex:!*-in class package!private! !
!AbstractOrderedTreemapStrategy categoriesFor: #layout:in:!*-in class package!private! !
!AbstractOrderedTreemapStrategy categoriesFor: #layout:using:!*-in class package!private! !

PivotByBiggestStrategy guid: (GUID fromString: '{A886B505-0882-40A1-90D2-93A376330C8E}')!
PivotByBiggestStrategy comment: ''!
!PivotByBiggestStrategy categoriesForClass!Unclassified! !
!PivotByBiggestStrategy methodsFor!

computePivotIndex: aCollection 
	| bestSize bestIndex |
	bestSize :=0.
	1 to: (aCollection size) do: [ :index |
		(aCollection at: index) size > bestSize ifTrue: [
			bestIndex := index.
			bestSize := (aCollection at: index) size]
].
^bestIndex! !
!PivotByBiggestStrategy categoriesFor: #computePivotIndex:!*-in class package!public! !

!PivotByBiggestStrategy class methodsFor!

description
^'Ordered - Pivot by biggest'! !
!PivotByBiggestStrategy class categoriesFor: #description!*-in class package!public! !

PivotByMiddleStrategy guid: (GUID fromString: '{EDD3AAE0-13C6-428C-A619-161B35A078B9}')!
PivotByMiddleStrategy comment: ''!
!PivotByMiddleStrategy categoriesForClass!Unclassified! !
!PivotByMiddleStrategy methodsFor!

computePivotIndex: aCollection 
	^aCollection size // 2 + 1! !
!PivotByMiddleStrategy categoriesFor: #computePivotIndex:!*-in class package!public! !

!PivotByMiddleStrategy class methodsFor!

description
		^'Ordered - Pivot by middle'! !
!PivotByMiddleStrategy class categoriesFor: #description!*-in class package!public! !

PivotBySplitSizeStrategy guid: (GUID fromString: '{6909E4AA-F0B9-4930-8965-A7D021011A32}')!
PivotBySplitSizeStrategy comment: ''!
!PivotBySplitSizeStrategy categoriesForClass!Unclassified! !
!PivotBySplitSizeStrategy methodsFor!

computePivotIndex: aCollection 
	| leftSize rightSize ratio bestRatio bestIndex |
	leftSize := 0.
	rightSize := aCollection sum.
	1 to: aCollection size - 1
		do: 
			[:index | 
			| each |
			each := aCollection at: index.
			leftSize := leftSize + each size.
			rightSize := rightSize - each size.
			ratio := leftSize / rightSize max: rightSize / leftSize.
			(bestIndex isNil or: [ratio < bestRatio]) 
				ifTrue: 
					[bestRatio := ratio.
					bestIndex := index]].
	^bestIndex! !
!PivotBySplitSizeStrategy categoriesFor: #computePivotIndex:!*-in class package!public! !

!PivotBySplitSizeStrategy class methodsFor!

description
	^'Ordered - Pivot by split size'! !
!PivotBySplitSizeStrategy class categoriesFor: #description!*-in class package!public! !

HotrizontalOrderedTreemapLayouter guid: (GUID fromString: '{EAE2F5ED-D0D3-4A98-986C-5C059D7B2475}')!
HotrizontalOrderedTreemapLayouter comment: ''!
!HotrizontalOrderedTreemapLayouter categoriesForClass!Unclassified! !
!HotrizontalOrderedTreemapLayouter methodsFor!

calculateBorderHeight: remainigHeight size: remainingSize |ratio|
	ratio := pivotSize / (pivotSize + list2size).
	^ratio * remainigHeight!

calculateBorderWidth: remainingWidth size: remainingSize |ratio |
	ratio := (pivotSize + list2size) / remainingSize.
	^ratio * remainingWidth!

computeLessThanThreeRemainingRectangles
	| height ratio |
	self computeList2: nodes size.
	ratio := pivotSize / (pivotSize + list2size).
	height := ratio * remainingRectangle height.
	pivot 
		setLayoutRectangle: (remainingRectangle topLeft extent: remainingRectangle width @ height).
	rectangle2 := remainingRectangle left @ (layoutRectangle top + height) 
				extent: remainingRectangle width @ (remainingRectangle height - height)!

computeMoreThanTwoRemainingRectangles
	self calculateBestBorder.
	self computeList2: bestIndex.
	nodes size - bestIndex > 0 ifTrue: [self computeList3].
	self layoutPivotRectangle.
	rectangle2 := remainingRectangle left @ (remainingRectangle top + bestHeight) 
				extent: bestWidth @ (remainingRectangle height - bestHeight).
	list3 notNil ifTrue: [self layoutRectangle3]!

computeRemainingRectangle
	remainingRectangle := (rectangle1 left + rectangle1 width) @ layoutRectangle top 
				extent: (layoutRectangle width - rectangle1 width) @ layoutRectangle height	"Then compute R2 and R3"!

layoutRectangle1
	rectangle1 := layoutRectangle topLeft 
				extent: (( list1 sum) / nodesSize * layoutRectangle width) 
						@ layoutRectangle height!

layoutRectangle3
	rectangle3 := (remainingRectangle left + bestWidth) @ remainingRectangle top 
				extent: (remainingRectangle width - bestWidth) @ remainingRectangle height!

layoutTwoRectangles
	| width ratio |
	ratio := (nodes first) size / nodesSize.
	width := ratio * layoutRectangle width.
	nodes first 
		setLayoutRectangle: (layoutRectangle topLeft extent: width @ layoutRectangle height).
	nodes second setLayoutRectangle: ((layoutRectangle left + width) @ layoutRectangle top 
				extent: (layoutRectangle width - width) @ layoutRectangle height)! !
!HotrizontalOrderedTreemapLayouter categoriesFor: #calculateBorderHeight:size:!*-in class package!private! !
!HotrizontalOrderedTreemapLayouter categoriesFor: #calculateBorderWidth:size:!*-in class package!private! !
!HotrizontalOrderedTreemapLayouter categoriesFor: #computeLessThanThreeRemainingRectangles!*-in class package!private! !
!HotrizontalOrderedTreemapLayouter categoriesFor: #computeMoreThanTwoRemainingRectangles!*-in class package!private! !
!HotrizontalOrderedTreemapLayouter categoriesFor: #computeRemainingRectangle!*-in class package!private! !
!HotrizontalOrderedTreemapLayouter categoriesFor: #layoutRectangle1!*-in class package!private! !
!HotrizontalOrderedTreemapLayouter categoriesFor: #layoutRectangle3!*-in class package!private! !
!HotrizontalOrderedTreemapLayouter categoriesFor: #layoutTwoRectangles!*-in class package!private! !

VerticalOrderedTreemapLayouter guid: (GUID fromString: '{928EE451-36CA-4B91-B75F-2EA74F669BC0}')!
VerticalOrderedTreemapLayouter comment: ''!
!VerticalOrderedTreemapLayouter categoriesForClass!Unclassified! !
!VerticalOrderedTreemapLayouter methodsFor!

calculateBorderHeight: remainigHeight size: remainingSize |ratio|
	ratio := (pivotSize + list2size) / remainingSize.
	^ratio * remainigHeight!

calculateBorderWidth: remainingWidth size: remainingSize |ratio|
	ratio := pivotSize / (pivotSize + list2size).
	^ratio * remainingWidth!

computeLessThanThreeRemainingRectangles
	| width ratio |
	self computeList2: nodes size.
	ratio := pivotSize / (pivotSize + list2size).
	width := ratio * remainingRectangle width.
	pivot 
		setLayoutRectangle: (remainingRectangle topLeft extent: width @ remainingRectangle height).
	rectangle2 := (remainingRectangle left + width) @ remainingRectangle top 
				extent: (remainingRectangle width - width) @ remainingRectangle height!

computeMoreThanTwoRemainingRectangles
	self calculateBestBorder.
	self computeList2: bestIndex.
	nodes size - bestIndex > 0 ifTrue: [self computeList3].
	self layoutPivotRectangle.
	rectangle2 := (remainingRectangle left + bestWidth) @ remainingRectangle top 
				extent: (remainingRectangle width - bestWidth) @ bestHeight.
	list3 notNil ifTrue: [self layoutRectangle3]!

computeRemainingRectangle
	remainingRectangle := layoutRectangle left @ (rectangle1 top + rectangle1 height) 
				extent: layoutRectangle width @ (layoutRectangle height - rectangle1 height)	"Then compute R2 and R3"!

layoutRectangle1
	rectangle1 := layoutRectangle topLeft extent: layoutRectangle width 
						@ (( list1 sum) / nodesSize * layoutRectangle height)!

layoutRectangle3
	rectangle3 := remainingRectangle left @ (remainingRectangle top + bestHeight) 
				extent: remainingRectangle width @ (remainingRectangle height - bestHeight)!

layoutTwoRectangles
	| height ratio |
	ratio := (nodes first) size / nodesSize.
	height := ratio * layoutRectangle height.
	(nodes first) 
		setLayoutRectangle: (layoutRectangle topLeft extent: layoutRectangle width @ height).
	(nodes second) setLayoutRectangle: (layoutRectangle left @ (layoutRectangle top + height) 
				extent: layoutRectangle width @ (layoutRectangle height - height))! !
!VerticalOrderedTreemapLayouter categoriesFor: #calculateBorderHeight:size:!*-in class package!private! !
!VerticalOrderedTreemapLayouter categoriesFor: #calculateBorderWidth:size:!*-in class package!private! !
!VerticalOrderedTreemapLayouter categoriesFor: #computeLessThanThreeRemainingRectangles!*-in class package!private! !
!VerticalOrderedTreemapLayouter categoriesFor: #computeMoreThanTwoRemainingRectangles!*-in class package!private! !
!VerticalOrderedTreemapLayouter categoriesFor: #computeRemainingRectangle!*-in class package!private! !
!VerticalOrderedTreemapLayouter categoriesFor: #layoutRectangle1!*-in class package!private! !
!VerticalOrderedTreemapLayouter categoriesFor: #layoutRectangle3!*-in class package!private! !
!VerticalOrderedTreemapLayouter categoriesFor: #layoutTwoRectangles!*-in class package!private! !

"Binary Globals"!

