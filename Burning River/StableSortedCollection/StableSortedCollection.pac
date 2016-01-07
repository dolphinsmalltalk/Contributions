| package |
package := Package name: 'StableSortedCollection'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #StableSortedCollection;
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

SortedCollection variableSubclass: #StableSortedCollection
	instanceVariableNames: 'tempArray'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StableSortedCollection guid: (GUID fromString: '{B4214B5C-2168-41E7-BCF5-5C27F37F9EE9}')!
StableSortedCollection comment: 'A version of SortedCollection which uses a stable sort (merge sort).

Instance variables:
	tempArray - a temporary array used during when sorting.  This was originally allocated each time the 
		#mergeFrom:mid:to: method was entered, but I found that the sort ran measureably faster
		if the temp array was allocated once at the beginning of the sort and released at the end.'!
!StableSortedCollection categoriesForClass!Unclassified! !
!StableSortedCollection methodsFor!

mergeFrom: start mid: mid to: stop
	| next index1 index2 |

	next := start.
	index1 := start.
	index2 := mid + 1.

	[ (index1 <= mid) and: [ index2 <= stop ] ] whileTrue:
		[ (self sortBlock value: (self basicAt: index1) value: (self basicAt: index2))
			ifTrue: [ tempArray basicAt: next put: (self basicAt: index1).
				     index1 := index1 + 1 ]
			ifFalse: [ tempArray basicAt: next put: (self basicAt: index2).
				      index2 := index2 + 1 ].
		next := next + 1 ].

	[ index1 <= mid ] whileTrue:
		[ tempArray basicAt: next put: (self basicAt: index1).
		index1 := index1 + 1.
		next := next + 1 ].

	start to: next - 1 do: [ :i | self basicAt: i put: (tempArray basicAt: i) ]!

mergeSortFrom: start to: stop
	stop - start > 10
		ifTrue: [ self
				mergeSortFrom: start to: (start + stop) // 2;
				mergeSortFrom: ((start + stop) // 2) + 1 to: stop;
				mergeFrom: start mid: (start + stop) // 2 to: stop ]
		ifFalse: [ self insertsortFrom: start to: stop ]!

sortFrom: start to: stop 
	"Private - Sort elements start through stop of self to be nondescending according to sortBlock."

	"wod - Fix bug where tempArray needs to be initialized to basicSize. Otherwise in may not be
	large enough when the internal 'firstIndex' is something other than 1." 

	tempArray := Array new: self basicSize.
	self mergeSortFrom: start to: stop.
	tempArray := nil! !
!StableSortedCollection categoriesFor: #mergeFrom:mid:to:!helpers!private! !
!StableSortedCollection categoriesFor: #mergeSortFrom:to:!algorithms!private! !
!StableSortedCollection categoriesFor: #sortFrom:to:!private!sorting! !

"Binary Globals"!

"Resources"!

