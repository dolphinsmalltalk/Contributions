| package |
package := Package name: 'StableListPresenter'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #StableListPresenter;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'StableSortedCollection';
	yourself).

package!

"Class Definitions"!

ListPresenter subclass: #StableListPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StableListPresenter guid: (GUID fromString: '{9EDB48F6-B688-4543-A2CC-EF5679FA5BEE}')!
StableListPresenter comment: ''!
!StableListPresenter categoriesForClass!Unclassified! !
!StableListPresenter methodsFor!

list: aSequenceableCollection
	"Set the contents of the receiver to be aSequenceableCollection"

	| list |
	list := self isSorted
		ifTrue: [ (StableSortedCollection new sortBlock: sortBlock) addAll: aSequenceableCollection; yourself ]
		ifFalse: [ aSequenceableCollection ].
	^self model list: list! !
!StableListPresenter categoriesFor: #list:!accessing!public! !

"Binary Globals"!

"Resources"!

