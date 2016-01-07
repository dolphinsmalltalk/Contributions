| package |
package := Package name: 'US RFC3339 Tests'.
package paxVersion: 1;
	basicComment: '$id: US RFC3339 Tests 1.305$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Test Cases for converting from and to RFC 3339 Times and Dates

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '1.305'.


package classNames
	add: #RFC3339DateTestCase;
	add: #RFC3339TimeStampTestCase;
	add: #RFC3339TimeTestCase;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Camp Smalltalk\SUnit\SUnit';
	add: 'US RFC3339';
	yourself).

package setManualPrerequisites: #(
	'US RFC3339').

package!

"Class Definitions"!

TestCase subclass: #RFC3339DateTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #RFC3339TimeStampTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #RFC3339TimeTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

RFC3339DateTestCase guid: (GUID fromString: '{32D302D9-2EB8-4067-B0BC-5E9BFAD1A8B7}')!
RFC3339DateTestCase comment: ''!
!RFC3339DateTestCase categoriesForClass!Unclassified! !
!RFC3339DateTestCase methodsFor!

assertRfc3339String: aString equalsYear: year month: month day: day 
	self 
		assert: (Date fromRfc3339String: aString) = (Date 
						newDay: day
						monthIndex: month
						year: year)!

assertYear: year month: month day: day equalsRfc3339String: aString 
	self 
		assert: (Date 
				newDay: day
				monthIndex: month
				year: year) rfc3339String 
				= aString!

testFromRfc3339Date
	self
		assertRfc3339String: '1985-04-12'
			equalsYear: 1985
			month: 4
			day: 12;
		assertRfc3339String: '1996-12-19'
			equalsYear: 1996
			month: 12
			day: 19;
		assertRfc3339String: '1990-12-31'
			equalsYear: 1990
			month: 12
			day: 31;
		assertRfc3339String: '1937-01-01'
			equalsYear: 1937
			month: 1
			day: 1!

testToRfc3339Date
	self
		assertYear: 1985
			month: 4
			day: 12
			equalsRfc3339String: '1985-04-12';
		assertYear: 1996
			month: 12
			day: 19
			equalsRfc3339String: '1996-12-19';
		assertYear: 1990
			month: 12
			day: 31
			equalsRfc3339String: '1990-12-31';
		assertYear: 1937
			month: 1
			day: 1
			equalsRfc3339String: '1937-01-01'! !
!RFC3339DateTestCase categoriesFor: #assertRfc3339String:equalsYear:month:day:!helpers!private! !
!RFC3339DateTestCase categoriesFor: #assertYear:month:day:equalsRfc3339String:!helpers!private! !
!RFC3339DateTestCase categoriesFor: #testFromRfc3339Date!*-unreferenced selectors!public!test! !
!RFC3339DateTestCase categoriesFor: #testToRfc3339Date!*-unreferenced selectors!public!test! !

RFC3339TimeStampTestCase guid: (GUID fromString: '{3340FF13-C3B5-40A6-A66D-836625C4CADA}')!
RFC3339TimeStampTestCase comment: ''!
!RFC3339TimeStampTestCase categoriesForClass!Unclassified! !
!RFC3339TimeStampTestCase methodsFor!

assertRfc3339String: aString equalsYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction 	self assert: (TimeStamp fromRfc3339String: aString) 
				= (TimeStamp date: (Date 
								newDay: day
								monthIndex: month
								year: year)
						time: ((Time 
								fromUtcHours: hour
								minutes: minute
								seconds: second
								millisecondsFraction: millisecondFraction) ))!

assertRfc3339String: aString equalsYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetAdd: offset 
	self assert: (TimeStamp fromRfc3339String: aString) 
				= (TimeStamp date: (Date 
								newDay: day
								monthIndex: month
								year: year)
						time: ((Time 
								fromUtcHours: hour
								minutes: minute
								seconds: second
								millisecondsFraction: millisecondFraction) subtractTime: (Time fromString: offset)))!

assertRfc3339String: aString equalsYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetSubtract: offset 
	self assert: (TimeStamp fromRfc3339String: aString) 
				= (TimeStamp date: (Date 
								newDay: day
								monthIndex: month
								year: year)
						time: ((Time 
								fromUtcHours: hour
								minutes: minute
								seconds: second
								millisecondsFraction: millisecondFraction) addTime: (Time fromString: offset)))!

assertYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction  equalsRfc3339String: string 
	self 
		assert: (TimeStamp date: (Date 
						newDay: day
						monthIndex: month
						year: year)
				time: ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) )) 
					rfc3339String = (TimeStamp fromRfc3339String: string) rfc3339String!

assertYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetAdd: offset equalsRfc3339String: string 
	self 
		assert: (TimeStamp date: (Date 
						newDay: day
						monthIndex: month
						year: year)
				time: ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) subtractTime: (Time fromString: offset))) 
					rfc3339String = (TimeStamp fromRfc3339String: string) rfc3339String!

assertYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetSubtract: offset equalsRfc3339String: string 
	self 
		assert: (TimeStamp date: (Date 
						newDay: day
						monthIndex: month
						year: year)
				time: ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) addTime: (Time fromString: offset))) 
					rfc3339String = (TimeStamp fromRfc3339String: string) rfc3339String!

testFromRfc3339TimeStamp
	self
		assertRfc3339String: '1985-04-12T23:20:50.52Z'
			equalsYear: 1985
			month: 4
			day: 12
			hour: 23
			minute: 20
			second: 50
			millisecondFraction: 52;
		assertRfc3339String: '1996-12-19T16:39:57-08:00'
			equalsYear: 1996
			month: 12
			day: 19
			hour: 16
			minute: 39
			second: 57
			millisecondFraction: 0
			offsetSubtract: '08:00';
		assertRfc3339String: '1937-01-01T12:00:27.87+00:20'
			equalsYear: 1937
			month: 1
			day: 1
			hour: 12
			minute: 0
			second: 27
			millisecondFraction: 87
			offsetAdd: '00:20'!

testToRfc3339TimeStamp
	self
		assertYear: 1985
			month: 4
			day: 12
			hour: 23
			minute: 20
			second: 50
			millisecondFraction: 52
			equalsRfc3339String: '1985-04-12T23:20:50.52Z';
		assertYear: 1996
			month: 12
			day: 19
			hour: 16
			minute: 39
			second: 57
			millisecondFraction: 0
			offsetSubtract: '08:00'
			equalsRfc3339String: '1996-12-19T16:39:57-08:00';
		assertYear: 1937
			month: 1
			day: 1
			hour: 12
			minute: 0
			second: 27
			millisecondFraction: 87
			offsetAdd: '00:20'
			equalsRfc3339String: '1937-01-01T12:00:27.87+00:20'! !
!RFC3339TimeStampTestCase categoriesFor: #assertRfc3339String:equalsYear:month:day:hour:minute:second:millisecondFraction:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertRfc3339String:equalsYear:month:day:hour:minute:second:millisecondFraction:offsetAdd:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertRfc3339String:equalsYear:month:day:hour:minute:second:millisecondFraction:offsetSubtract:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertYear:month:day:hour:minute:second:millisecondFraction:equalsRfc3339String:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertYear:month:day:hour:minute:second:millisecondFraction:offsetAdd:equalsRfc3339String:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertYear:month:day:hour:minute:second:millisecondFraction:offsetSubtract:equalsRfc3339String:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #testFromRfc3339TimeStamp!*-unreferenced selectors!public!test! !
!RFC3339TimeStampTestCase categoriesFor: #testToRfc3339TimeStamp!*-unreferenced selectors!public!test! !

RFC3339TimeTestCase guid: (GUID fromString: '{4C25013E-2270-49DF-8887-9D5E46E510C9}')!
RFC3339TimeTestCase comment: ''!
!RFC3339TimeTestCase categoriesForClass!Unclassified! !
!RFC3339TimeTestCase methodsFor!

assertHour: hour minute: minute second: second millisecondFraction: millisecondFraction equalsRfc3339String: aString 
	self 
		assert: (Time 
				fromUtcHours: hour
				minutes: minute
				seconds: second
				millisecondsFraction: millisecondFraction) rfc3339String 
				= (Time fromRfc3339String: aString) rfc3339String!

assertHour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetAdd: anOffset equalsRfc3339String: aString 
	self 
		assert: ((Time 
				fromUtcHours: hour
				minutes: minute
				seconds: second
				millisecondsFraction: millisecondFraction) subtractTime: (Time fromString: anOffset)) 
				rfc3339String = (Time fromRfc3339String: aString) rfc3339String!

assertHour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetSubtract: anOffset equalsRfc3339String: aString 
	self 
		assert: ((Time 
				fromUtcHours: hour
				minutes: minute
				seconds: second
				millisecondsFraction: millisecondFraction) addTime: (Time fromString: anOffset)) 
				rfc3339String = (Time fromRfc3339String: aString) rfc3339String!

assertRfc3339String: aString equalsHour: hour minute: minute second: second millisecondFraction: millisecondFraction 
	self 
		assert: (Time fromRfc3339String: aString) = (Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction)!

assertRfc3339String: aString equalsHour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetAdd: offset 
	self assert: (Time fromRfc3339String: aString) 
				= ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) subtractTime: (Time fromString: offset))!

assertRfc3339String: aString equalsHour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetSubtract: offset 
	self assert: (Time fromRfc3339String: aString) 
				= ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) addTime: (Time fromString: offset))!

testFromRfc3339Time
	self
		assertRfc3339String: '23:20:50.52Z'
			equalsHour: 23
			minute: 20
			second: 50
			millisecondFraction: 52;
		assertRfc3339String: '16:39:57-08:00'
			equalsHour: 16
			minute: 39
			second: 57
			millisecondFraction: 0
			offsetSubtract: '08:00';
		assertRfc3339String: '12:00:27.87+00:20'
			equalsHour: 12
			minute: 0
			second: 27
			millisecondFraction: 87
			offsetAdd: '00:20'!

testToRfc3339Time
	self
		assertHour: 23
			minute: 20
			second: 50
			millisecondFraction: 52
			equalsRfc3339String: '23:20:50.52Z';
		assertHour: 16
			minute: 39
			second: 57
			millisecondFraction: 0
			offsetSubtract: '08:00'
			equalsRfc3339String: '16:39:57-08:00';
		assertHour: 12
			minute: 0
			second: 27
			millisecondFraction: 87
			offsetAdd: '00:20'
			equalsRfc3339String: '12:00:27.87+00:20'! !
!RFC3339TimeTestCase categoriesFor: #assertHour:minute:second:millisecondFraction:equalsRfc3339String:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertHour:minute:second:millisecondFraction:offsetAdd:equalsRfc3339String:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertHour:minute:second:millisecondFraction:offsetSubtract:equalsRfc3339String:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertRfc3339String:equalsHour:minute:second:millisecondFraction:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertRfc3339String:equalsHour:minute:second:millisecondFraction:offsetAdd:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertRfc3339String:equalsHour:minute:second:millisecondFraction:offsetSubtract:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #testFromRfc3339Time!*-unreferenced selectors!public!test! !
!RFC3339TimeTestCase categoriesFor: #testToRfc3339Time!*-unreferenced selectors!public!test! !

"Binary Globals"!

