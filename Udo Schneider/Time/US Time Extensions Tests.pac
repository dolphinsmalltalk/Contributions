| package |
package := Package name: 'US Time Extensions Tests'.
package paxVersion: 1;
	basicComment: '$id: US Time Extensions Tests 0.006$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.006'.


package classNames
	add: #TimeExtensionsTestCase;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #TimeExtensionsTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

TimeExtensionsTestCase guid: (GUID fromString: '{8D6C42B2-9AB8-4DE5-9CA2-31E160FA9573}')!
TimeExtensionsTestCase comment: ''!
!TimeExtensionsTestCase categoriesForClass!Unclassified! !
!TimeExtensionsTestCase methodsFor!

testMillisecondsFraction
	self 
		assert: ((Time 
				fromHours: 0
				minutes: 0
				seconds: 0
				milliseconds: 500) addTime: (Time 
							fromHours: 0
							minutes: 0
							seconds: 0
							milliseconds: 500)) 
				= (Time 
						fromHours: 0
						minutes: 0
						seconds: 1
						milliseconds: 0).
	self 
		assert: ((Time 
				fromHours: 0
				minutes: 0
				seconds: 0
				millisecondsFraction: 5) addTime: (Time 
							fromHours: 0
							minutes: 0
							seconds: 0
							millisecondsFraction: 5)) 
				= (Time 
						fromHours: 0
						minutes: 0
						seconds: 1
						milliseconds: 0)! !
!TimeExtensionsTestCase categoriesFor: #testMillisecondsFraction!*-unreferenced selectors!public! !

"Binary Globals"!

