| package |
package := Package name: 'US Treemap - Development'.
package paxVersion: 1;
	basicComment: '$id: US Treemap - Development 0.012$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.012'.


package classNames
	add: #NonRenderStrategy;
	yourself.

package methodNames
	add: 'TreeMapModel class' -> #publishedAspectsOfInstances;
	add: 'TreeMapNode class' -> #publishedAspectsOfInstances;
	add: 'TreeMapRenderer class' -> #publishedAspectsOfInstances;
	add: 'TreeMapView class' -> #publishedAspectsOfInstances;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: 'US Treemap - Base';
	add: 'US Treemap - Render Abstract';
	add: 'US Treemap - View';
	yourself).

package!

"Class Definitions"!

AbstractTreeMapRenderStrategy subclass: #NonRenderStrategy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!TreeMapModel class methodsFor!

publishedAspectsOfInstances
	| aspects |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect block: #sizeBlock);
		add: (Aspect block: #getNodeColorBlock).
	^aspects! !
!TreeMapModel class categoriesFor: #publishedAspectsOfInstances!*-not in class package!must strip!public! !

!TreeMapNode class methodsFor!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| aspects |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect number: #size);
		add: (Aspect name: #layoutRectangle);
		add: (Aspect collection: #children).
	^aspects! !
!TreeMapNode class categoriesFor: #publishedAspectsOfInstances!*-not in class package!must strip!public! !

!TreeMapRenderer class methodsFor!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
	add: (Aspect name: #layoutStrategy chooseFrom: (self availableLayoutStrategies collect: [:each | each name , ' new']) ) beImmutable;
		add: (Aspect name: #renderStrategy chooseFrom: (self availableRenderStrategies collect: [:each | each name , ' new']) ) beImmutable;
		add: (Aspect block: #nodeColorBlock);
		add: (Aspect block: #nodeSizeBlock);
		yourself
	! !
!TreeMapRenderer class categoriesFor: #publishedAspectsOfInstances!*-not in class package!public! !

!TreeMapView class methodsFor!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| aspects |
	aspects := super publishedAspectsOfInstances.
	aspects
		add: (Aspect name: #renderer);
		add: (Aspect block: #getImageBlock);
		add: (Aspect block: #getTextBlock).
	^aspects! !
!TreeMapView class categoriesFor: #publishedAspectsOfInstances!*-not in class package!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

NonRenderStrategy guid: (GUID fromString: '{80446A49-5BC8-4924-90E7-2132770C0B8D}')!
NonRenderStrategy comment: ''!
!NonRenderStrategy categoriesForClass!Unclassified! !
!NonRenderStrategy methodsFor!

render: node using: renderState 
	"Simply do nothing ;-)"! !
!NonRenderStrategy categoriesFor: #render:using:!*-in class package!public! !

"Binary Globals"!

