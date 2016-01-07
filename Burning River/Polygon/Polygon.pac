| package |
package := Package name: 'Polygon'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #Polygon;
	add: #PolygonLineSegment;
	add: #PolygonLineSegmentsTest;
	add: #PolygonTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

Object subclass: #Polygon
	instanceVariableNames: 'vertices segments'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #PolygonLineSegment
	instanceVariableNames: 'endPoint1 endPoint2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #PolygonLineSegmentsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #PolygonTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Polygon guid: (GUID fromString: '{D85AD786-792D-48CF-AF2D-1F2855424E9C}')!
Polygon comment: ''!
!Polygon categoriesForClass!Unclassified! !
!Polygon methodsFor!

addVertex: aPoint
	self vertices add: aPoint.
	self clearSegments.!

addVertices: aCollectionOfPoints
	aCollectionOfPoints do:
		[ :aPoint |
		self addVertex: aPoint ]!

area
	^self signedArea abs!

at: anInteger
	^self vertices at: anInteger!

clearSegments
	segments := nil!

contains: aPoint
	"From the comp.graphics.algorithms FAQ"

	| inside j pi pj |

	inside := false.
	j := self size.

	1 to: self size do:
		[ :i |
		pi := self at: i.
		pj := self at: j.

		((((pi y <= aPoint y) and: [ aPoint y < pj y ]) or:
		 [ (pj y <= aPoint y) and: [ aPoint y < pi y ] ]) and:
		[ aPoint x < ((((pj x - pi x) * (aPoint y - pi y)) / (pj y - pi y)) + pi x) ])
			ifTrue: [ inside := inside not ].

		j := i ].

	^inside!

generateLineSegments
	segments := Array new: self vertices size.

	1 to: self vertices size do:
		[ :i |
		i < self vertices size
			ifTrue:
				[ segments at: i put: (PolygonLineSegment new
								endPoint1: self vertices at: i;
								endPoint2: self vertices at: i + 1;
								yourself) ]
			ifFalse:
				[ segments at: i put: (PolygonLineSegment new
								endPoint1: self vertices at: i;
								endPoint2: self vertices at: 1;
								yourself) ] ]!

intersects: aPolygon
	"Answer true if the two polygons intersect."
	"Current implementation is dead-dumb naive (and N^2 to boot) - doubtless there's a better way..."

	self segments do:
		[ :seg1 |
		aPolygon segments do:
			[ :seg2 |
			(seg1 intersects: seg2) ifTrue: [ ^true ] ] ].

	^false!

isClockwise
	^self signedArea < 0!

isCounterClockwise
	^self isClockwise not!

segments
	"Answer a collection of PolygonLineSegments that define the boundaries of the polygon"

	segments isNil ifTrue: [ self generateLineSegments ].
	^segments!

signedArea
	| a p1 p2 |

	a := 0.

	1 to: self vertices size do:
		[ :i |
		p1 := self at: i.
		i < self vertices size
			ifTrue: [ p2 := self at: i+1 ]
			ifFalse: [ p2 := self at: 1 ].
		a := a + ((p1 x * p2 y) - (p1 y * p2 x)) ].

	^a / 2!

size
	^self vertices size!

vertices
	^vertices!

vertices: anOrderedCollection
	vertices := anOrderedCollection.
	self clearSegments! !
!Polygon categoriesFor: #addVertex:!public! !
!Polygon categoriesFor: #addVertices:!public! !
!Polygon categoriesFor: #area!public! !
!Polygon categoriesFor: #at:!public! !
!Polygon categoriesFor: #clearSegments!public! !
!Polygon categoriesFor: #contains:!public! !
!Polygon categoriesFor: #generateLineSegments!public! !
!Polygon categoriesFor: #intersects:!public! !
!Polygon categoriesFor: #isClockwise!public! !
!Polygon categoriesFor: #isCounterClockwise!public! !
!Polygon categoriesFor: #segments!public! !
!Polygon categoriesFor: #signedArea!public! !
!Polygon categoriesFor: #size!public! !
!Polygon categoriesFor: #vertices!public! !
!Polygon categoriesFor: #vertices:!public! !

!Polygon class methodsFor!

new
	^super new
		vertices: OrderedCollection new;
		yourself! !
!Polygon class categoriesFor: #new!public! !

PolygonLineSegment guid: (GUID fromString: '{2DF9B1A6-F60D-42D1-8DB0-E9A58045230F}')!
PolygonLineSegment comment: ''!
!PolygonLineSegment categoriesForClass!Unclassified! !
!PolygonLineSegment methodsFor!

endPoint1
	^endPoint1!

endPoint1: aPoint
	endPoint1 := aPoint!

endPoint2
	^endPoint2!

endPoint2: aPoint
	endPoint2 := aPoint!

intersectionValues: aPolygonLineSegment
	| a b c d num1 num2 denom r s answer |

	"Answers an Array containing the following data:
		Index		Value
		   1		Do lines intersect?  (Boolean)
		   2		Are lines parallel?  (Boolean)
		   3		Are lines coincident?  (Boolean)
		   4		Intersection point (if intersection exists) (Point or nil)"

	answer := Array new: 4.

	a := self endPoint1.
	b := self endPoint2.
	c := aPolygonLineSegment endPoint1.
	d := aPolygonLineSegment endPoint2.

	denom := (((b x - a x) * (d y - c y)) - ((b y - a y) * (d x - c x))).
	num1 := (((a y - c y) * (d x - c x)) - ((a x - c x) * (d y - c y))).
		
	denom = 0
		ifTrue:
			[ answer at: 1 put: false.
			answer at: 2 put: true.
			num1 = 0
				ifTrue: [ answer at: 3 put: true ]
				ifFalse: [ answer at: 3 put: false ] ]
		ifFalse:
			[ num2 := (((a y - c y) * (b x - a x)) - ((a x - c x) * (b y - a y))).

			r := num1 / denom.
			s := num2 / denom.

			answer at: 1 put:( (0 <= r) and: [ (r <= 1) and: [ (0 <= s) and: [ s <= 1 ] ] ]).
			answer at: 2 put: false.
			answer at: 3 put: false.
			answer at: 4 put: (a x + (r * (b x - a x))) @ (a y + (r * (b y - a y))) ].

	^answer!

intersectionWith: aPolygonLineSegment
	^(self intersectionValues: aPolygonLineSegment) at: 4!

intersects: aPolygonLineSegment
	^(self intersectionValues: aPolygonLineSegment) at: 1!

isCoincidentTo: aPolygonLineSegment
	^(self intersectionValues: aPolygonLineSegment) at: 3!

isParallelTo: aPolygonLineSegment
	^(self intersectionValues: aPolygonLineSegment) at: 2! !
!PolygonLineSegment categoriesFor: #endPoint1!public! !
!PolygonLineSegment categoriesFor: #endPoint1:!public! !
!PolygonLineSegment categoriesFor: #endPoint2!public! !
!PolygonLineSegment categoriesFor: #endPoint2:!public! !
!PolygonLineSegment categoriesFor: #intersectionValues:!public! !
!PolygonLineSegment categoriesFor: #intersectionWith:!public! !
!PolygonLineSegment categoriesFor: #intersects:!public! !
!PolygonLineSegment categoriesFor: #isCoincidentTo:!public! !
!PolygonLineSegment categoriesFor: #isParallelTo:!public! !

PolygonLineSegmentsTest guid: (GUID fromString: '{9F3BBA42-560F-4FA5-95A6-A726CC8352C9}')!
PolygonLineSegmentsTest comment: ''!
!PolygonLineSegmentsTest categoriesForClass!Unclassified! !
!PolygonLineSegmentsTest methodsFor!

testIntersects1
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 5@5;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@5;
		endPoint2: 5@0;
		yourself.

	self should: [ l1 intersects: l2 ]!

testIntersects2
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 5@0;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@5;
		endPoint2: 0@0;
		yourself.

	self should: [ l1 intersects: l2 ]!

testIntersects3
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 5@0;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@5;
		endPoint2: 5@5;
		yourself.

	self shouldnt: [ l1 intersects: l2 ]!

testIntersects4
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 5@0;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 0@5;
		yourself.

	"Intersects at endPoint1"

	self should: [ l1 intersects: l2 ]!

testIntersects5
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 5@5;
		endPoint2: 0@5;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 0@5;
		yourself.

	"Intersects at endPoint2"

	self should: [ l1 intersects: l2 ]!

testIntersects6
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 5@5;
		endPoint2: 0@4;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 0@5;
		yourself.

	self should: [ l1 intersects: l2 ]!

testIntersects7
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 0@4;
		endPoint2: 100000@100000;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 0@5;
		yourself.

	self should: [ l1 intersects: l2 ]!

testIntersects8
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 0@4;
		endPoint2: 5@5;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@0;
		endPoint2: 0@5;
		yourself.

	self should: [ l1 intersects: l2 ]!

testIntersects9
	| l1 l2 |

	l1 := PolygonLineSegment new
		endPoint1: 0@4;
		endPoint2: 5@5;
		yourself.
	l2 := PolygonLineSegment new
		endPoint1: 0@5;
		endPoint2: 0@0;
		yourself.

	self should: [ l1 intersects: l2 ]! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects1!public! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects2!public! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects3!public! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects4!public! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects5!public! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects6!public! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects7!public! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects8!public! !
!PolygonLineSegmentsTest categoriesFor: #testIntersects9!public! !

PolygonTest guid: (GUID fromString: '{A04C852D-3E2B-41AD-BE1C-322D6BEC56B3}')!
PolygonTest comment: ''!
!PolygonTest categoriesForClass!Unclassified! !
!PolygonTest methodsFor!

testArea
	| p1 p2 p3 p4 p5 p6 p7 p8 |

	p1 := Polygon new addVertex: 0@0; addVertex: 0@2; addVertex: 2@2; addVertex: 2@0; yourself.
	self should: [ p1 signedArea = -4 ].
	self should: [ p1 area = 4 ].
	self should: [ p1 isClockwise ].
	self shouldnt: [ p1 isCounterClockwise ].

	p2 := Polygon new addVertex: 0@0; addVertex: 2@0; addVertex: 2@2; addVertex: 0@2; yourself.
	self should: [ p2 signedArea = 4 ].
	self should: [ p2 area = 4 ].
	self shouldnt: [ p2 isClockwise ].
	self should: [ p2 isCounterClockwise ].

	p3 := Polygon new addVertex: 0@0; addVertex: (-2)@0; addVertex: (-2)@(-2); addVertex: 0@(-2); yourself.
	self should: [ p3 signedArea = 4 ].
	self should: [ p3 area = 4 ].
	self shouldnt: [ p3 isClockwise ].
	self should: [ p3 isCounterClockwise ].

	p4 := Polygon new addVertex: 0@(-2); addVertex: 0@2; addVertex: 2@2; addVertex: 2@(-2); yourself.
	self should: [ p4 signedArea = -8 ].
	self should: [ p4 area = 8 ].
	self should: [ p4 isClockwise ].
	self shouldnt: [ p4 isCounterClockwise ].

	p5 := Polygon new addVertex: 2@2; addVertex: 0@2; addVertex: 0@0; addVertex: 2@0; yourself.
	self should: [ p5 signedArea = 4 ].
	self should: [ p5 area = 4 ].
	self shouldnt: [ p5 isClockwise ].
	self should: [ p5 isCounterClockwise ].

	p6 := Polygon new addVertex: 0@0; addVertex: 0@2; addVertex: 2@2; addVertex: 2@0; yourself.
	self should: [ p6 signedArea = -4 ].
	self should: [ p6 area = 4 ].
	self should: [ p6 isClockwise ].
	self shouldnt: [ p6 isCounterClockwise ].

	p7 := Polygon new addVertex: 2@2; addVertex: 2@4; addVertex: 4@4; addVertex: 4@2; yourself.
	self should: [ p7 signedArea = -4 ].
	self should: [ p7 area = 4 ].
	self should: [ p7 isClockwise ].
	self shouldnt: [ p7 isCounterClockwise ].

	p8 := Polygon new addVertex: 2@2; addVertex: 4@2; addVertex: 4@4; addVertex: 2@4; yourself.
	self should: [ p8 signedArea = 4 ].
	self should: [ p8 area = 4 ].
	self shouldnt: [ p8 isClockwise ].
	self should: [ p8 isCounterClockwise ].
!

testContains1
	| p |

	p := Polygon new
		addVertex: 0@0;
		addVertex: 2@0;
		addVertex: 2@2;
		addVertex: 0@2;
		yourself.

	self should: [ p contains: 1@1 ].
	self shouldnt: [ p contains: 3@2 ].! !
!PolygonTest categoriesFor: #testArea!public! !
!PolygonTest categoriesFor: #testContains1!public! !

"Binary Globals"!

"Resources"!

