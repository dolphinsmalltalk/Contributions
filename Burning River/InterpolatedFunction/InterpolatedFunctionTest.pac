| package |
package := Package name: 'InterpolatedFunctionTest'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #InterpolatedFunctionTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'InterpolatedFunction';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #InterpolatedFunctionTest
	instanceVariableNames: 'func'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

InterpolatedFunctionTest guid: (GUID fromString: '{68B52CF6-C9F4-4A10-A4DF-9D16418F3D43}')!
InterpolatedFunctionTest comment: ''!
!InterpolatedFunctionTest categoriesForClass!Unclassified! !
!InterpolatedFunctionTest methodsFor!

setUp
	func := InterpolatedFunction for: #( ##(0@0) ##(1@1) ##(2@1.75) ##(3@2.25) ##(4@2.5))
							withEndpointDerivatives: #(100000 0)!

testEvaluation
	self should: [ (func value: 0) = 0 ].
	self should: [ (func value: 1) = 1 ].
	self should: [ (func value: 2) = 1.75 ].
	self should: [ (func value: 3) = 2.25 ].
	self should: [ (func value: 4) = 2.5 ].! !
!InterpolatedFunctionTest categoriesFor: #setUp!public!Running! !
!InterpolatedFunctionTest categoriesFor: #testEvaluation!public!testing! !

"Binary Globals"!

"Resources"!

