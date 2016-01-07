| package |
package := Package name: 'EightQueens'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #EightQueensModel;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Model subclass: #EightQueensModel
	instanceVariableNames: 'board vectors queens savedPositions positionsEvaluated'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

EightQueensModel guid: (GUID fromString: '{D63B046B-CE36-4252-BBDA-5C76640A92A0}')!
EightQueensModel comment: ''!
!EightQueensModel categoriesForClass!MVP-Models! !
!EightQueensModel methodsFor!

at: row at: col
	^board at: row -1 * 8 + col!

at: row at: col add: anInteger
	(self at: row at: col) add: anInteger!

board
	^board!

board: anObject
	board := anObject!

evaluatePosition
	"Private - Answer true if no conflicts, otherwise false"

	| cell |

	self incrementPositionsEvaluated.
	self queens do:
		[ :anArray | | row col |
		row := anArray at: 1.
		col := anArray at: 2.
		cell := self at: row at: col.
		cell do:
			[ :n |
			(self vectors at: n)  ifTrue: [ ^false ].
			self toggleVector: n ] ].
	^true!

incrementPositionsEvaluated
	self positionsEvaluated: (self positionsEvaluated + 1)!

incrementVector: n
	self vectors at: n put: (self vectors at: n) + 1!

initialize
	self board: (Array new: 64).
	1 to: self board size do: [ :n | self board at: n put: OrderedCollection new ].
	self vectors: (Array new: 34).
	self queens: (Array new: 8).
	self savedPositions: (OrderedCollection new).
	self positionsEvaluated: 0.
	self
		initializeVectors;
		populateBoard;
		initializeQueens!

initializeQueens
	1 to: 8 do:
		[ :i |
		self queens at: i put: (Array with: 1 with: i) ]!

initializeVectors
	self vectors atAllPut: false!

moveQueen: n
	"Private - Move the specified queen to its next position.  Answer true if the queen was
	moved back from row 8 to row 1 (indicating that the next queen needs to be
	moved), otherwise answer false."

	| queen row |

	queen := self queens at: n.
	row := queen at: 1.
	row < 8
		ifTrue: [ queen at: 1 put: row + 1.  ^false ]
		ifFalse: [ queen at: 1 put: 1.  ^true ]!

moveQueens
	"Private - Move queens to their next position.  Answer true if the simulation is complete
	(all positions have been tried), otherwise answer false."

	| r |

	r := self moveQueen: 1.
	r ifTrue: [ r := self moveQueen: 2 ].
	r ifTrue: [ r := self moveQueen: 3 ].
	r ifTrue: [ r := self moveQueen: 4 ].
	r ifTrue: [ r := self moveQueen: 5 ].
	r ifTrue: [ r := self moveQueen: 6 ].
	r ifTrue: [ r := self moveQueen: 7 ].
	r ifTrue: [ r := self moveQueen: 8 ].
	^r
!

populateBoard
	1 to: self board size do:
		[ :r |
		1 to: (self board at: r) size do:
			[ :c |
			(self board at: r) at: c put: OrderedCollection new ] ].

	1 to: 8 do:
		[ :r |
		1 to: 8 do:
			[ :c |
			"Up-right diagonals"

			(r > 1 | (c > 1)) & (r < 8 | (c < 8))
				ifTrue: [ self at: r at: c add: (r + c) - 2 ].

			"Rows"

			self at: r at: c add: r + 13.

			"Down-right diagonals"

			(r < 8 | (c > 1)) & (r > 1 | (c < 8))
				ifTrue: [ self at: r at: c add: 35 - (((9 - r) + c) - 2) ] ] ]!

positionsEvaluated
	^positionsEvaluated!

positionsEvaluated: anObject
	positionsEvaluated := anObject!

printOn: aStream
	1 to: 8 do:
		[ :r |
		1 to: 8 do:
			[ :c |
			aStream nextPut: $(.
			(self at: r at: c) do:
				[ :n |
				n displayOn: aStream.
				aStream space ].
			aStream nextPutAll: ') | ' ].
		aStream cr. ]!

queens
	^queens!

queens: anObject
	queens := anObject!

run
	| done |

	done := false.
	[ done ] whileFalse:
		[ self evaluatePosition ifTrue: [ self savePosition ].
		done := self moveQueens.
		self initializeVectors ].!

savedPositions
	^savedPositions!

savedPositions: aCollection
	savedPositions := aCollection!

savePosition
	self savedPositions add: (self queens deepCopy)!

toggleVector: n
	self vectors at: n put: true!

vectors
	^vectors!

vectors: anObject
	vectors := anObject! !
!EightQueensModel categoriesFor: #at:at:!public! !
!EightQueensModel categoriesFor: #at:at:add:!public! !
!EightQueensModel categoriesFor: #board!accessing!public! !
!EightQueensModel categoriesFor: #board:!accessing!private! !
!EightQueensModel categoriesFor: #evaluatePosition!private! !
!EightQueensModel categoriesFor: #incrementPositionsEvaluated!private! !
!EightQueensModel categoriesFor: #incrementVector:!private! !
!EightQueensModel categoriesFor: #initialize!public! !
!EightQueensModel categoriesFor: #initializeQueens!public! !
!EightQueensModel categoriesFor: #initializeVectors!public! !
!EightQueensModel categoriesFor: #moveQueen:!private! !
!EightQueensModel categoriesFor: #moveQueens!private! !
!EightQueensModel categoriesFor: #populateBoard!public! !
!EightQueensModel categoriesFor: #positionsEvaluated!accessing!private! !
!EightQueensModel categoriesFor: #positionsEvaluated:!accessing!private! !
!EightQueensModel categoriesFor: #printOn:!public! !
!EightQueensModel categoriesFor: #queens!accessing!public! !
!EightQueensModel categoriesFor: #queens:!accessing!private! !
!EightQueensModel categoriesFor: #run!public! !
!EightQueensModel categoriesFor: #savedPositions!accessing!public! !
!EightQueensModel categoriesFor: #savedPositions:!accessing!private! !
!EightQueensModel categoriesFor: #savePosition!public! !
!EightQueensModel categoriesFor: #toggleVector:!private! !
!EightQueensModel categoriesFor: #vectors!accessing!public! !
!EightQueensModel categoriesFor: #vectors:!accessing!private! !

!EightQueensModel class methodsFor!

new
	^super new initialize! !
!EightQueensModel class categoriesFor: #new!public! !

"Binary Globals"!

"Resources"!

