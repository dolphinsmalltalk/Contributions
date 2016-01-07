| package |
package := Package name: 'US Treemap - Base'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Base 0.026$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

path := ''S:\lobtest''.
complexTree := VirtualTreeModel withRoots: (Array with: (BonsaiFolder fromString: path)).
complexTree hasChildrenBlock: [:each | each children notEmpty].
complexTree getChildrenBlock: [:each | each children].
dib := DIBSection 
			width: 640
			height: 480
			depth: 32.
renderer := TreeMapRenderer new.
renderer
	renderStrategy: CircleRenderStrategy new;
	setBitmap: dib;
	setLayoutRectangle: (0 @ 0 extent: dib extent);
	setTreeModel: complexTree;
	render.
ImagePresenter showOn: dib


Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.026'.


package classNames
	add: #TreeMapModel;
	add: #TreeMapNode;
	add: #TreeMapRenderer;
	yourself.

package methodNames
	add: #ListModel -> #asTreeModel;
	add: #TreeModelAbstract -> #asTreeModel;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: 'US Treemap - Layout Slice and Dice';
	add: 'US Treemap - Render Flat';
	yourself).

package!

"Class Definitions"!

Object subclass: #TreeMapModel
	instanceVariableNames: 'treeModel rootNode renderer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #TreeMapNode
	instanceVariableNames: 'object parent model children size layoutRectangle'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #TreeMapRenderer
	instanceVariableNames: 'treeMapModel layoutRectangle nodeSizeBlock nodeColorBlock layoutStrategy renderStrategy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ListModel methodsFor!

asTreeModel
	^TreeModel withRoots: self list! !
!ListModel categoriesFor: #asTreeModel!*-not in class package!public! !

!TreeModelAbstract methodsFor!

asTreeModel
	^self! !
!TreeModelAbstract categoriesFor: #asTreeModel!*-not in class package!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

TreeMapModel guid: (GUID fromString: '{85BE892B-E1E9-42D3-9F34-CD2BFE1AC8AA}')!
TreeMapModel comment: ''!
!TreeMapModel categoriesForClass!Unclassified! !
!TreeMapModel methodsFor!

childNodeAt: aPoint 
	^rootNode childNodeAt: aPoint!

initialize
	rootNode := TreeMapNode 
				object: nil
				parent: nil
				model: self.
	!

leafNodeAt: aPoint 
	^rootNode leafNodeAt: aPoint!

nodeForObject: anObject 
	| parents node |
	parents := (treeModel allParentsOf: anObject) copyWith: anObject.
	node := self rootNode.
	parents 
		do: [:eachParent | node := node children detect: [:eachNode | eachNode object = eachParent]].
	^node!

populateNode: node 
	node children do: 
			[:eachNode | 
			eachNode 
				children: ((treeModel childrenOf: eachNode object) collect: 
							[:eachObject | 
							TreeMapNode 
								object: eachObject
								parent: node
								model: self]).
			self populateNode: eachNode]!

populateRootNode
	rootNode := TreeMapNode 
				object: nil
				parent: nil
				model: self.
	rootNode 
		children: (treeModel roots collect: 
					[:eachRoot | 
					TreeMapNode 
						object: eachRoot
						parent: rootNode
						model: self]).
	self populateNode: rootNode!

rootNode
	^rootNode!

setModel: aTreeOrListModel 
	treeModel := aTreeOrListModel asTreeModel .
	self populateRootNode!

setRenderer: aTreeMapRenderer
renderer := aTreeMapRenderer!

sizeOf: anObject 
	^renderer nodeSize:  anObject!

treeModel
	^treeModel! !
!TreeMapModel categoriesFor: #childNodeAt:!*-in class package!public! !
!TreeMapModel categoriesFor: #initialize!*-in class package!private! !
!TreeMapModel categoriesFor: #leafNodeAt:!*-in class package!public! !
!TreeMapModel categoriesFor: #nodeForObject:!*-in class package!public! !
!TreeMapModel categoriesFor: #populateNode:!*-in class package!private! !
!TreeMapModel categoriesFor: #populateRootNode!*-in class package!private! !
!TreeMapModel categoriesFor: #rootNode!*-in class package!accessing!public! !
!TreeMapModel categoriesFor: #setModel:!*-in class package!private! !
!TreeMapModel categoriesFor: #setRenderer:!*-in class package!private! !
!TreeMapModel categoriesFor: #sizeOf:!*-in class package!public! !
!TreeMapModel categoriesFor: #treeModel!*-in class package!public! !

!TreeMapModel class methodsFor!

new
		^self shouldNotImplement!

on: aTreeModel renderer: aTreeMapRenderer 
	^(self basicNew)
		initialize;
		setRenderer: aTreeMapRenderer;
		setModel: aTreeModel;
		yourself! !
!TreeMapModel class categoriesFor: #new!*-in class package!public! !
!TreeMapModel class categoriesFor: #on:renderer:!*-in class package!public! !

TreeMapNode guid: (GUID fromString: '{69FA6A9B-602D-480B-A153-9056740802CA}')!
TreeMapNode comment: ''!
!TreeMapNode categoriesForClass!Unclassified! !
!TreeMapNode methodsFor!

childNodeAt: aPoint 
	^self children detect: [ :each | (each layoutRectangle containsPoint: aPoint)] ifNone: [nil].
"
	self children do: [:each | (each layoutRectangle containsPoint: aPoint) ifTrue: [^each]].
	^nil"!

children
	^children !

children: anArray 
	children := anArray!

childrenWithExtent
	^children select: [:each | each hasExtent]!

hasExtent
	^self size strictlyPositive!

hasParent
	^parent notNil!

isLeafNode
	^children isEmpty!

layoutRectangle
	^layoutRectangle!

leafNodeAt: aPoint 
	"Can use detect or #childNodeAt: here?"

	#USToDo.
	self children do: 
			[:each | 
			(each layoutRectangle containsPoint: aPoint) 
				ifTrue: [each isLeafNode ifFalse: [^each leafNodeAt: aPoint] ifTrue: [^each]]].
	^nil!

object
	"Private - Answer the <Object> which is the element stored in this node."

	^object!

parent
	"Private - Answer the <MoenTreeNode> which is the receiver's parent node."

	^parent!

parent: aMoenTreeNode
	parent := aMoenTreeNode!

printOn: aStream 
	super printOn: aStream.
	aStream
		nextPutAll: ' (';
		print:		object;
		nextPutAll: ')'!

setLayoutRectangle: aRectangle 
	
	layoutRectangle := aRectangle !

setObject: anObject parent: aTreeMapNode model:  aTreeMapModel 
	object := anObject.
	parent := aTreeMapNode.
model :=  aTreeMapModel .
	layoutRectangle := Rectangle new.
	^self!

size
	"Use #inject:into: here?"

	#USToDo.
	size isNil 
		ifTrue: 
			[size := self isLeafNode 
						ifTrue: [model sizeOf: object]
						ifFalse: [self children inject: 0 into: [:sum :each | sum + each size]]].
	^size! !
!TreeMapNode categoriesFor: #childNodeAt:!*-in class package!public! !
!TreeMapNode categoriesFor: #children!*-in class package!enumerating!public! !
!TreeMapNode categoriesFor: #children:!*-in class package!accessing!private! !
!TreeMapNode categoriesFor: #childrenWithExtent!*-in class package!enumerating!public! !
!TreeMapNode categoriesFor: #hasExtent!*-in class package!public! !
!TreeMapNode categoriesFor: #hasParent!*-in class package!public! !
!TreeMapNode categoriesFor: #isLeafNode!*-in class package!public! !
!TreeMapNode categoriesFor: #layoutRectangle!*-in class package!accessing!public! !
!TreeMapNode categoriesFor: #leafNodeAt:!*-in class package!public! !
!TreeMapNode categoriesFor: #object!*-in class package!accessing!public! !
!TreeMapNode categoriesFor: #parent!*-in class package!accessing!public! !
!TreeMapNode categoriesFor: #parent:!*-in class package!accessing!public! !
!TreeMapNode categoriesFor: #printOn:!*-in class package!public! !
!TreeMapNode categoriesFor: #setLayoutRectangle:!*-in class package!accessing!private! !
!TreeMapNode categoriesFor: #setObject:parent:model:!*-in class package!initializing!private! !
!TreeMapNode categoriesFor: #size!*-in class package!public! !

!TreeMapNode class methodsFor!

new
	"Use #object:parent: to create nodes"

	^self shouldNotImplement!

object: anObject parent: aMoenTreeNode model: aTreeMapModel 
	^(self basicNew)
		initialize;
		setObject: anObject
			parent: aMoenTreeNode
			model: aTreeMapModel;yourself! !
!TreeMapNode class categoriesFor: #new!*-in class package!public! !
!TreeMapNode class categoriesFor: #object:parent:model:!*-in class package!instance creation!public! !

TreeMapRenderer guid: (GUID fromString: '{CF4B34C3-5F8B-4152-9413-4A9E5F1844DE}')!
TreeMapRenderer comment: ''!
!TreeMapRenderer categoriesForClass!Unclassified! !
!TreeMapRenderer methodsFor!

defaultNodeColorBlock
	^##([ :object |  Color  white])!

defaultNodeSizeBlock
	^##([ :object | object size])!

initialize

	nodeColorBlock := self defaultNodeColorBlock.
	nodeSizeBlock := self defaultNodeSizeBlock.
	layoutStrategy := SliceAndDiceLayoutStrategy new.
	renderStrategy := FlatRenderStrategy new!

layoutStrategy
	^layoutStrategy!

layoutStrategy: anObject 
	layoutStrategy := anObject. self trigger: #renderSettingsChanged!

nodeColor: anObject 
	^nodeColorBlock value: anObject!

nodeColorBlock
	^nodeColorBlock!

nodeColorBlock: anObject 
	nodeColorBlock := anObject.
	self trigger: #renderSettingsChanged!

nodeSize: anObject 
	^nodeSizeBlock value: anObject.
	!

nodeSizeBlock
	^nodeSizeBlock!

nodeSizeBlock: anObject 
	nodeSizeBlock := anObject .self trigger: #renderSettingsChanged!

render
	| renderState |
	renderStrategy setRenderer: self.
	renderState := (renderStrategy initialRenderState)
				setRootRectangle: layoutRectangle;
				yourself.
	layoutStrategy setRenderStrategy: renderStrategy.
	layoutStrategy 
		layout: treeMapModel
		in: layoutRectangle
		using: renderState!

renderStrategy
	^renderStrategy!

renderStrategy: anObject 
	renderStrategy := anObject. self trigger: #renderSettingsChanged!

setBitmap: aBitmap 
renderStrategy
		setBitmap: aBitmap !

setCanvas: aCanvas 
	renderStrategy setCanvas: aCanvas !

setLayoutRectangle: aRectangle 
	layoutRectangle := aRectangle!

setModel: aTreeOrListModel 
	treeMapModel := TreeMapModel on: aTreeOrListModel  renderer: self!

treeMapModel
	^treeMapModel! !
!TreeMapRenderer categoriesFor: #defaultNodeColorBlock!*-in class package!public! !
!TreeMapRenderer categoriesFor: #defaultNodeSizeBlock!*-in class package!public! !
!TreeMapRenderer categoriesFor: #initialize!*-in class package!public! !
!TreeMapRenderer categoriesFor: #layoutStrategy!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #layoutStrategy:!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #nodeColor:!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #nodeColorBlock!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #nodeColorBlock:!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #nodeSize:!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #nodeSizeBlock!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #nodeSizeBlock:!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #render!*-in class package!public! !
!TreeMapRenderer categoriesFor: #renderStrategy!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #renderStrategy:!*-in class package!accessing!public! !
!TreeMapRenderer categoriesFor: #setBitmap:!*-in class package!public! !
!TreeMapRenderer categoriesFor: #setCanvas:!*-in class package!public! !
!TreeMapRenderer categoriesFor: #setLayoutRectangle:!*-in class package!public! !
!TreeMapRenderer categoriesFor: #setModel:!*-in class package!public! !
!TreeMapRenderer categoriesFor: #treeMapModel!*-in class package!public! !

!TreeMapRenderer class methodsFor!

availableLayoutStrategies
	^(ClassCategory name: 'Treemap-Layout') contents!

availableRenderStrategies
	^(ClassCategory name: 'Treemap-Render') contents !

new
^super new initialize! !
!TreeMapRenderer class categoriesFor: #availableLayoutStrategies!*-in class package!public! !
!TreeMapRenderer class categoriesFor: #availableRenderStrategies!*-in class package!public! !
!TreeMapRenderer class categoriesFor: #new!*-in class package!public! !

"Binary Globals"!

