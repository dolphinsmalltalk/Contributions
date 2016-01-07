| package |
package := Package name: 'US PropertyList Tests'.
package paxVersion: 1;
	basicComment: '$id: US PropertyList Tests 1.005$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '1.005'.


package classNames
	add: #BinaryPropertyListTest;
	add: #PropertyListTest;
	add: #XMLPropertyListTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Camp Smalltalk\SUnit\SUnit';
	add: 'US PropertyLists';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #PropertyListTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'PropertyListConstants'
	classInstanceVariableNames: ''!
PropertyListTest subclass: #BinaryPropertyListTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PropertyListTest subclass: #XMLPropertyListTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

PropertyListTest guid: (GUID fromString: '{0E394B5D-289D-487F-8043-BB5A591AA7D4}')!
PropertyListTest comment: 'SUnitBrowser openOnTestCase: PropertyListTest'!
!PropertyListTest categoriesForClass!Unclassified! !
!PropertyListTest methodsFor!

assertPlist: aByteArray equals: anObject 
	self 
		assert: ((NSPropertyListSerialization propertyListFromData: aByteArray
				format: self class propertyListTypeConstant)  =anObject)!

testArray
	self assertPlist: self class plistArray equals: self class testArray!

testBoolean
	self
		assertPlist: self class plistFalse equals: false;
		assertPlist: self class plistTrue equals: true!

testData
	self assertPlist: self class plistData equals: self class testData!

testDate
	self assertPlist: self class plistDate equals: self class testTimeStamp!

testDictionary
	self assertPlist: self class plistDictionary equals: self class testDictionary!

testInteger
	self
		assertPlist: self class plistInteger equals: self class testInteger!

testNestedArray
	self assertPlist: self class plistNestedArray equals: self class testNestedArray!

testNestedDictionary
	self assertPlist: self class plistNestedDictionary equals: self class testNestedDictionary!

testNotAPlist
	self should: 
			[NSPropertyListSerialization propertyListFromData: self class plistNotAPlist
				format: self class propertyListTypeConstant]
		raise: Exception!

testReal
	self
	
		assertPlist: self class plistReal equals: self class testReal!

testString
	self
		assertPlist: self class plistString equals: self class testString;
		assertPlist: self class plistStringWithSingleQuotes
			equals: self class testStringWithSingleQuotes;
 assertPlist: self class plistStringWithDoubleQuotes
			equals: self class testStringWithDoubleQuotes!

testWrongPlistVersion

	self should: 
			[NSPropertyListSerialization propertyListFromData: self class plistWrongVersion
				format: self class propertyListTypeConstant]
		raise: Exception! !
!PropertyListTest categoriesFor: #assertPlist:equals:!public! !
!PropertyListTest categoriesFor: #testArray!public!test! !
!PropertyListTest categoriesFor: #testBoolean!*-unreferenced selectors!public!test! !
!PropertyListTest categoriesFor: #testData!public!test! !
!PropertyListTest categoriesFor: #testDate!*-unreferenced selectors!public!test! !
!PropertyListTest categoriesFor: #testDictionary!public!test! !
!PropertyListTest categoriesFor: #testInteger!public!test! !
!PropertyListTest categoriesFor: #testNestedArray!public!test! !
!PropertyListTest categoriesFor: #testNestedDictionary!public!test! !
!PropertyListTest categoriesFor: #testNotAPlist!*-unreferenced selectors!public! !
!PropertyListTest categoriesFor: #testReal!public!test! !
!PropertyListTest categoriesFor: #testString!public!test! !
!PropertyListTest categoriesFor: #testWrongPlistVersion!*-unreferenced selectors!public! !

!PropertyListTest class methodsFor!

createAllPlistMethods
	"
		BinaryPropertyListTest createAllPlistMethods.
		XMLPropertyListTest createAllPlistMethods.
"

	self filesAndMethods 
		keysAndValuesDo: [:key :value | self createPlistMethod: value fromFile: key]!

createPlistMethod: methodName fromFile: aFilename 
	| fileStream stream |
	fileStream := FileStream 
				read: 'C:\Dokumente und Einstellungen\Udo Schneider\Desktop\plists\plists\' , aFilename
				text: false.
	stream := ReadWriteStream on: String new.
	stream
		nextPutAll: methodName;
		cr;
		tab;
		nextPut: $";
		cr;
		tab;
		tab;
		nextPutAll: 'self createPlistMethod: ';
		nextPut: $';
		nextPutAll: methodName;
		nextPut: $';
		nextPutAll: ' fromFile: ';
		nextPut: $';
		nextPutAll: aFilename;
		nextPut: $';
		cr;
		tab;
		nextPut: $";
		cr;
		tab;
		nextPut: $^;
		nextPutAll: '##('.
	fileStream contents base64StoreOn: stream.
	fileStream close.
	stream nextPutAll: ').'.
	^self class compile: stream contents!

isAbstract
	^self sunitName = #TestCase or: [self sunitName = #PropertyListTest]!

propertyListTypeConstant
self subclassResponsibility!

testArray
	| stream |
	stream := ReadWriteStream on: Array new.
	stream
		nextPut: self testString;
		nextPut: self testInteger;
		nextPut: self testReal;
		nextPut: true;
		nextPut: false;
		nextPut: self testTimeStamp;
		nextPut: self testData.
	^stream contents!

testData
	^#[16r01 16r23 16r45 16r67 16r89 16rAB 16rCD 16rEF]!

testDictionary
	^(Dictionary new)
		at: 'data' put: self testData;
		at: 'date' put: self testTimeStamp;
		at: 'false' put: false;
		at: 'true' put: true;
		at: 'number' put: self testInteger;
		at: 'real' put: self testReal;
		at: 'string' put: self testString;
		yourself!

testInteger
	^815!

testNestedArray
	| dic stream |
	dic := self testDictionary.
	dic at: 'array' put: self testArray.
	^self testArray copyWith: dic!

testNestedDictionary
	^(self testDictionary)
		at: 'array' put: (self testArray copyWith: self testDictionary);
		yourself!

testReal
	^3.141592!

testString
	^'The quick brown fox jumps over the lazy dog''s back'!

testStringWithDoubleQuotes
	^'The "quick brown" fox jumps over the lazy dog''s back'!

testStringWithSingleQuotes
	^'The ''quick brown'' fox jumps over the lazy dog''s back'!

testTimeStamp
	^TimeStamp date: (Date 
				newDay: 25
				monthIndex: 7
				year: 2005)
		time: (Time 
				fromUtcHours: 8
				minutes: 29
				seconds: 9
				milliseconds: 0)! !
!PropertyListTest class categoriesFor: #createAllPlistMethods!*-unreferenced selectors!public! !
!PropertyListTest class categoriesFor: #createPlistMethod:fromFile:!public! !
!PropertyListTest class categoriesFor: #isAbstract!public! !
!PropertyListTest class categoriesFor: #propertyListTypeConstant!public! !
!PropertyListTest class categoriesFor: #testArray!public! !
!PropertyListTest class categoriesFor: #testData!public! !
!PropertyListTest class categoriesFor: #testDictionary!public! !
!PropertyListTest class categoriesFor: #testInteger!public! !
!PropertyListTest class categoriesFor: #testNestedArray!public! !
!PropertyListTest class categoriesFor: #testNestedDictionary!public! !
!PropertyListTest class categoriesFor: #testReal!public! !
!PropertyListTest class categoriesFor: #testString!public! !
!PropertyListTest class categoriesFor: #testStringWithDoubleQuotes!public! !
!PropertyListTest class categoriesFor: #testStringWithSingleQuotes!public! !
!PropertyListTest class categoriesFor: #testTimeStamp!public! !

BinaryPropertyListTest guid: (GUID fromString: '{DA5AB207-198D-4E5D-BE38-310AD3111C9D}')!
BinaryPropertyListTest comment: 'SUnitBrowser openOnTestCase: BinaryPropertyListTest'!
!BinaryPropertyListTest categoriesForClass!Unclassified! !
!BinaryPropertyListTest class methodsFor!

filesAndMethods
	^##((Dictionary new)
		at: 'array.bplist' put: 'plistArray';
		at: 'array_nested.bplist' put: 'plistNestedArray';
		at: 'data.bplist' put: 'plistData';
		at: 'date.bplist' put: 'plistDate';
		at: 'dictionary.bplist' put: 'plistDictionary';
		at: 'dictionary_nested.bplist' put: 'plistNestedDictionary';
		at: 'false.bplist' put: 'plistFalse';
		at: 'integer.bplist' put: 'plistInteger';
		at: 'nplist.bplist' put: 'plistNotAPlist';
		at: 'plist_v2.bplist' put: 'plistWrongVersion';
		at: 'real.bplist' put: 'plistReal';
		at: 'string.bplist' put: 'plistString';
		at: 'string_dquotes.bplist' put: 'plistStringWithDoubleQuotes';
		at: 'string_squotes.bplist' put: 'plistStringWithSingleQuotes';
		at: 'true.bplist' put: 'plistTrue';
		yourself)!

plistArray
	"
		self createPlistMethod: 'plistArray' fromFile: 'array.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDCnAQIDBAUGB18QMlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3ZlciB0aGUgbGF6
eSBkb2cncyBiYWNrEQMvI0AJIfr8iwB6CQgzQaEptKoAAABIASNFZ4mrze8IEEVIUVJTXAAAAAAA
AAEBAAAAAAAAAAgAAAAAAAAAAAAAAAAAAABl')).!

plistData
	"
		self createPlistMethod: 'plistData' fromFile: 'data.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDBIASNFZ4mrze8IAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAABE=')).!

plistDate
	"
		self createPlistMethod: 'plistDate' fromFile: 'date.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDAzQaEptKoAAAAIAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAABE=')).!

plistDictionary
	"
		self createPlistMethod: 'plistDictionary' fromFile: 'dictionary.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDDXAQIDBAUGBwgJCgsMDQ5WbnVtYmVyVWZhbHNlVHJlYWxWc3RyaW5nVHRydWVUZGF0
ZVRkYXRhEQMvCCNACSH6/IsAel8QMlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3ZlciB0aGUg
bGF6eSBkb2cncyBiYWNrCTNBoSm0qgAAAEgBI0VniavN7wgXHiQpMDU6P0JDTIGCiwAAAAAAAAEB
AAAAAAAAAA8AAAAAAAAAAAAAAAAAAACU')).!

plistFalse
	"
		self createPlistMethod: 'plistFalse' fromFile: 'false.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDAICAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAJ')).!

plistInteger
	"
		self createPlistMethod: 'plistInteger' fromFile: 'integer.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDARAy8IAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAs=')).!

plistNestedArray
	"
		self createPlistMethod: 'plistNestedArray' fromFile: 'array_nested.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDCoAQIDBAUGBwhfEDJUaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhlIGxh
enkgZG9nJ3MgYmFjaxEDLyNACSH6/IsAegkIM0GhKbSqAAAASAEjRWeJq83v2AkKCwwNDg8QAgUD
AQQTBgdWbnVtYmVyVWZhbHNlVHJlYWxWc3RyaW5nVHRydWVVYXJyYXlUZGF0ZVRkYXRhCAmnAQID
BAUGBwkICBFGSVJTVF1md36EiZCVm6ClpqevsAAAAAAAAAEBAAAAAAAAABYAAAAAAAAAAAAAAAAA
AACx')).!

plistNestedDictionary
	"
		self createPlistMethod: 'plistNestedDictionary' fromFile: 'dictionary_nested.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDDYAQIDBAUGBwgJCgsMDQ4RElZudW1iZXJVZmFsc2VUcmVhbFZzdHJpbmdUdHJ1ZVVh
cnJheVRkYXRlVGRhdGERAy8II0AJIfr8iwB6XxAyVGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBv
dmVyIHRoZSBsYXp5IGRvZydzIGJhY2sJqAwJCw0KERITCQgzQaEptKoAAABIASNFZ4mrze/XAQID
BAUHCAkKCwwNERIICQgZICYrMjc9QkdKS1SJipOUlZ6ntrcAAAAAAAABAQAAAAAAAAAWAAAAAAAA
AAAAAAAAAAAAuA==')).!

plistNotAPlist
	"
		self createPlistMethod: 'plistNotAPlist' fromFile: 'nplist.bplist'
	"
	^##((ByteArray fromBase64String: 'cHBsaXN0MDAjQAkh+vyLAHoIAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAABE=')).!

plistReal
	"
		self createPlistMethod: 'plistReal' fromFile: 'real.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDAjQAkh+vyLAHoIAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAABE=')).!

plistString
	"
		self createPlistMethod: 'plistString' fromFile: 'string.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDBfEDJUaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhlIGxhenkgZG9nJ3Mg
YmFjawgAAAAAAAABAQAAAAAAAAABAAAAAAAAAAAAAAAAAAAAPQ==')).!

plistStringWithDoubleQuotes
	"
		self createPlistMethod: 'plistStringWithDoubleQuotes' fromFile: 'string_dquotes.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDBfEDRUaGUgInF1aWNrIGJyb3duIiBmb3gganVtcHMgb3ZlciB0aGUgbGF6eSBkb2cn
cyBiYWNrCAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAA/')).!

plistStringWithSingleQuotes
	"
		self createPlistMethod: 'plistStringWithSingleQuotes' fromFile: 'string_squotes.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDBfEDRUaGUgJ3F1aWNrIGJyb3duJyBmb3gganVtcHMgb3ZlciB0aGUgbGF6eSBkb2cn
cyBiYWNrCAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAA/')).!

plistTrue
	"
		self createPlistMethod: 'plistTrue' fromFile: 'true.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDAJCAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAJ')).!

plistWrongVersion
	"
		self createPlistMethod: 'plistWrongVersion' fromFile: 'plist_v2.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDEJCA0KDQoNCg0KDQoNCgEBDQoNCg0KDQoNCg0KDQoBDQoNCg0KDQoNCg0KDQoNCg0K
DQoNCg0KDQoNCg0KCQ0K')).!

propertyListTypeConstant
	^NSPropertyListBinaryFormat_v1_0! !
!BinaryPropertyListTest class categoriesFor: #filesAndMethods!public! !
!BinaryPropertyListTest class categoriesFor: #plistArray!public! !
!BinaryPropertyListTest class categoriesFor: #plistData!public! !
!BinaryPropertyListTest class categoriesFor: #plistDate!public! !
!BinaryPropertyListTest class categoriesFor: #plistDictionary!public! !
!BinaryPropertyListTest class categoriesFor: #plistFalse!public! !
!BinaryPropertyListTest class categoriesFor: #plistInteger!public! !
!BinaryPropertyListTest class categoriesFor: #plistNestedArray!public! !
!BinaryPropertyListTest class categoriesFor: #plistNestedDictionary!public! !
!BinaryPropertyListTest class categoriesFor: #plistNotAPlist!public! !
!BinaryPropertyListTest class categoriesFor: #plistReal!public! !
!BinaryPropertyListTest class categoriesFor: #plistString!public! !
!BinaryPropertyListTest class categoriesFor: #plistStringWithDoubleQuotes!public! !
!BinaryPropertyListTest class categoriesFor: #plistStringWithSingleQuotes!public! !
!BinaryPropertyListTest class categoriesFor: #plistTrue!public! !
!BinaryPropertyListTest class categoriesFor: #plistWrongVersion!public! !
!BinaryPropertyListTest class categoriesFor: #propertyListTypeConstant!public! !

XMLPropertyListTest guid: (GUID fromString: '{F495451F-1F75-4FB2-B6C1-A2F9586DEADC}')!
XMLPropertyListTest comment: 'SUnitBrowser openOnTestCase: XMLPropertyListTest'!
!XMLPropertyListTest categoriesForClass!Unclassified! !
!XMLPropertyListTest class methodsFor!

filesAndMethods
	^##((Dictionary new)
		at: 'array.xml' put: 'plistArray';
		at: 'array_nested.xml' put: 'plistNestedArray';
		at: 'data.xml' put: 'plistData';
		at: 'date.xml' put: 'plistDate';
		at: 'dictionary.xml' put: 'plistDictionary';
		at: 'dictionary_nested.xml' put: 'plistNestedDictionary';
		at: 'false.xml' put: 'plistFalse';
		at: 'integer.xml' put: 'plistInteger';
		at: 'nplist.xml' put: 'plistNotAPlist';
		at: 'plist_v2.xml' put: 'plistWrongVersion';
		at: 'real.xml' put: 'plistReal';
		at: 'string.xml' put: 'plistString';
		at: 'string_dquotes.xml' put: 'plistStringWithDoubleQuotes';
		at: 'string_squotes.xml' put: 'plistStringWithSingleQuotes';
		at: 'true.xml' put: 'plistTrue';
		yourself)!

plistArray
	"
		self createPlistMethod: 'plistArray' fromFile: 'array.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8YXJyYXk+DQoJPHN0cmluZz5UaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhl
IGxhenkgZG9nJ3MgYmFjazwvc3RyaW5nPg0KCTxpbnRlZ2VyPjgxNTwvaW50ZWdlcj4NCgk8cmVh
bD4zLjE0MTU5MjwvcmVhbD4NCgk8dHJ1ZS8+DQoJPGZhbHNlLz4NCgk8ZGF0ZT4yMDA1LTA3LTI1
VDA4OjI5OjA5WjwvZGF0ZT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8L2RhdGE+DQo8L2Fy
cmF5Pg0KPC9wbGlzdD4NCg==')).!

plistData
	"
		self createPlistMethod: 'plistData' fromFile: 'data.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZGF0YT4NCkFTTkZaNG1yemU4PQ0KPC9kYXRhPg0KPC9wbGlzdD4NCg==')).!

plistDate
	"
		self createPlistMethod: 'plistDate' fromFile: 'date.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZGF0ZT4yMDA1LTA3LTI1VDA4OjI5OjA5WjwvZGF0ZT4NCjwvcGxpc3Q+DQo=')).!

plistDictionary
	"
		self createPlistMethod: 'plistDictionary' fromFile: 'dictionary.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZGljdD4NCgk8a2V5PmRhdGE8L2tleT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8
L2RhdGE+DQoJPGtleT5kYXRlPC9rZXk+DQoJPGRhdGU+MjAwNS0wNy0yNVQwODoyOTowOVo8L2Rh
dGU+DQoJPGtleT5mYWxzZTwva2V5Pg0KCTxmYWxzZS8+DQoJPGtleT5udW1iZXI8L2tleT4NCgk8
aW50ZWdlcj44MTU8L2ludGVnZXI+DQoJPGtleT5yZWFsPC9rZXk+DQoJPHJlYWw+My4xNDE1OTI8
L3JlYWw+DQoJPGtleT5zdHJpbmc8L2tleT4NCgk8c3RyaW5nPlRoZSBxdWljayBicm93biBmb3gg
anVtcHMgb3ZlciB0aGUgbGF6eSBkb2cncyBiYWNrPC9zdHJpbmc+DQoJPGtleT50cnVlPC9rZXk+
DQoJPHRydWUvPg0KPC9kaWN0Pg0KPC9wbGlzdD4NCg==')).!

plistFalse
	"
		self createPlistMethod: 'plistFalse' fromFile: 'false.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZmFsc2UvPg0KPC9wbGlzdD4NCg==')).!

plistInteger
	"
		self createPlistMethod: 'plistInteger' fromFile: 'integer.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8aW50ZWdlcj44MTU8L2ludGVnZXI+DQo8L3BsaXN0Pg0K')).!

plistNestedArray
	"
		self createPlistMethod: 'plistNestedArray' fromFile: 'array_nested.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8YXJyYXk+DQoJPHN0cmluZz5UaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhl
IGxhenkgZG9nJ3MgYmFjazwvc3RyaW5nPg0KCTxpbnRlZ2VyPjgxNTwvaW50ZWdlcj4NCgk8cmVh
bD4zLjE0MTU5MjwvcmVhbD4NCgk8dHJ1ZS8+DQoJPGZhbHNlLz4NCgk8ZGF0ZT4yMDA1LTA3LTI1
VDA4OjI5OjA5WjwvZGF0ZT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8L2RhdGE+DQo8ZGlj
dD4NCgk8a2V5PmRhdGE8L2tleT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8L2RhdGE+DQoJ
PGtleT5kYXRlPC9rZXk+DQoJPGRhdGU+MjAwNS0wNy0yNVQwODoyOTowOVo8L2RhdGU+DQoJPGtl
eT5mYWxzZTwva2V5Pg0KCTxmYWxzZS8+DQoJPGtleT5udW1iZXI8L2tleT4NCgk8aW50ZWdlcj44
MTU8L2ludGVnZXI+DQoJPGtleT5yZWFsPC9rZXk+DQoJPHJlYWw+My4xNDE1OTI8L3JlYWw+DQoJ
PGtleT5zdHJpbmc8L2tleT4NCgk8c3RyaW5nPlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3Zl
ciB0aGUgbGF6eSBkb2cncyBiYWNrPC9zdHJpbmc+DQoJPGtleT50cnVlPC9rZXk+DQoJPHRydWUv
Pg0KCTxrZXk+YXJyYXk8L2tleT4NCgk8YXJyYXk+DQoJPHN0cmluZz5UaGUgcXVpY2sgYnJvd24g
Zm94IGp1bXBzIG92ZXIgdGhlIGxhenkgZG9nJ3MgYmFjazwvc3RyaW5nPg0KCTxpbnRlZ2VyPjgx
NTwvaW50ZWdlcj4NCgk8cmVhbD4zLjE0MTU5MjwvcmVhbD4NCgk8dHJ1ZS8+DQoJPGZhbHNlLz4N
Cgk8ZGF0ZT4yMDA1LTA3LTI1VDA4OjI5OjA5WjwvZGF0ZT4NCgk8ZGF0YT4NCglBU05GWjRtcnpl
OD0NCgk8L2RhdGE+DQoJPC9hcnJheT4NCjwvZGljdD4NCjwvYXJyYXk+DQo8L3BsaXN0Pg0K')).!

plistNestedDictionary
	"
		self createPlistMethod: 'plistNestedDictionary' fromFile: 'dictionary_nested.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8ZGljdD4NCgk8a2V5PmRhdGE8L2tleT4NCgk8ZGF0YT4NCglBU05GWjRtcnplOD0NCgk8
L2RhdGE+DQoJPGtleT5kYXRlPC9rZXk+DQoJPGRhdGU+MjAwNS0wNy0yNVQwODoyOTowOVo8L2Rh
dGU+DQoJPGtleT5mYWxzZTwva2V5Pg0KCTxmYWxzZS8+DQoJPGtleT5udW1iZXI8L2tleT4NCgk8
aW50ZWdlcj44MTU8L2ludGVnZXI+DQoJPGtleT5yZWFsPC9rZXk+DQoJPHJlYWw+My4xNDE1OTI8
L3JlYWw+DQoJPGtleT5zdHJpbmc8L2tleT4NCgk8c3RyaW5nPlRoZSBxdWljayBicm93biBmb3gg
anVtcHMgb3ZlciB0aGUgbGF6eSBkb2cncyBiYWNrPC9zdHJpbmc+DQoJPGtleT50cnVlPC9rZXk+
DQoJPHRydWUvPg0KCQ0KCQk8a2V5PmFycmF5PC9rZXk+DQoJPGFycmF5Pg0KCTxzdHJpbmc+VGhl
IHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZydzIGJhY2s8L3N0cmluZz4N
Cgk8aW50ZWdlcj44MTU8L2ludGVnZXI+DQoJPHJlYWw+My4xNDE1OTI8L3JlYWw+DQoJPHRydWUv
Pg0KCTxmYWxzZS8+DQoJPGRhdGU+MjAwNS0wNy0yNVQwODoyOTowOVo8L2RhdGU+DQoJPGRhdGE+
DQoJQVNORlo0bXJ6ZTg9DQoJPC9kYXRhPg0KCQ0KPGRpY3Q+DQoJPGtleT5kYXRhPC9rZXk+DQoJ
PGRhdGE+DQoJQVNORlo0bXJ6ZTg9DQoJPC9kYXRhPg0KCTxrZXk+ZGF0ZTwva2V5Pg0KCTxkYXRl
PjIwMDUtMDctMjVUMDg6Mjk6MDlaPC9kYXRlPg0KCTxrZXk+ZmFsc2U8L2tleT4NCgk8ZmFsc2Uv
Pg0KCTxrZXk+bnVtYmVyPC9rZXk+DQoJPGludGVnZXI+ODE1PC9pbnRlZ2VyPg0KCTxrZXk+cmVh
bDwva2V5Pg0KCTxyZWFsPjMuMTQxNTkyPC9yZWFsPg0KCTxrZXk+c3RyaW5nPC9rZXk+DQoJPHN0
cmluZz5UaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhlIGxhenkgZG9nJ3MgYmFjazwv
c3RyaW5nPg0KCTxrZXk+dHJ1ZTwva2V5Pg0KCTx0cnVlLz4NCgk8L2RpY3Q+DQoJPC9hcnJheT4N
CgkNCjwvZGljdD4NCjwvcGxpc3Q+DQo=')).!

plistNotAPlist
	"
		self createPlistMethod: 'plistNotAPlist' fromFile: 'nplist.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjxucGxpc3QgdmVyc2lvbj0i
MS4wIj4NCjx0cnVlLz4NCjwvbnBsaXN0Pg0K')).!

plistReal
	"
		self createPlistMethod: 'plistReal' fromFile: 'real.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8cmVhbD4zLjE0MTU5MjwvcmVhbD4NCjwvcGxpc3Q+DQo=')).!

plistString
	"
		self createPlistMethod: 'plistString' fromFile: 'string.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8c3RyaW5nPlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3ZlciB0aGUgbGF6eSBkb2cn
cyBiYWNrPC9zdHJpbmc+DQo8L3BsaXN0Pg0K')).!

plistStringWithDoubleQuotes
	"
		self createPlistMethod: 'plistStringWithDoubleQuotes' fromFile: 'string_dquotes.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8c3RyaW5nPlRoZSAicXVpY2sgYnJvd24iIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRv
ZydzIGJhY2s8L3N0cmluZz4NCjwvcGxpc3Q+DQo=')).!

plistStringWithSingleQuotes
	"
		self createPlistMethod: 'plistStringWithSingleQuotes' fromFile: 'string_squotes.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8c3RyaW5nPlRoZSAncXVpY2sgYnJvd24nIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRv
ZydzIGJhY2s8L3N0cmluZz4NCjwvcGxpc3Q+DQo=')).!

plistTrue
	"
		self createPlistMethod: 'plistTrue' fromFile: 'true.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjEu
MCI+DQo8dHJ1ZS8+DQo8L3BsaXN0Pg0K')).!

plistWrongVersion
	"
		self createPlistMethod: 'plistWrongVersion' fromFile: 'plist_v2.xml'
	"
	^##((ByteArray fromBase64String: 'PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4NCjwhRE9DVFlQRSBwbGlzdCBQ
VUJMSUMgIi0vL0FwcGxlIENvbXB1dGVyLy9EVEQgUExJU1QgMS4wLy9FTiIgImh0dHA6Ly93d3cu
YXBwbGUuY29tL0RURHMvUHJvcGVydHlMaXN0LTEuMC5kdGQiPg0KPHBsaXN0IHZlcnNpb249IjIu
MCI+DQo8dHJ1ZS8+DQo8L3BsaXN0Pg0K')).!

propertyListTypeConstant
	^NSPropertyListXMLFormat_v1_0! !
!XMLPropertyListTest class categoriesFor: #filesAndMethods!public! !
!XMLPropertyListTest class categoriesFor: #plistArray!public! !
!XMLPropertyListTest class categoriesFor: #plistData!public! !
!XMLPropertyListTest class categoriesFor: #plistDate!public! !
!XMLPropertyListTest class categoriesFor: #plistDictionary!public! !
!XMLPropertyListTest class categoriesFor: #plistFalse!public! !
!XMLPropertyListTest class categoriesFor: #plistInteger!public! !
!XMLPropertyListTest class categoriesFor: #plistNestedArray!public! !
!XMLPropertyListTest class categoriesFor: #plistNestedDictionary!public! !
!XMLPropertyListTest class categoriesFor: #plistNotAPlist!public! !
!XMLPropertyListTest class categoriesFor: #plistReal!public! !
!XMLPropertyListTest class categoriesFor: #plistString!public! !
!XMLPropertyListTest class categoriesFor: #plistStringWithDoubleQuotes!public! !
!XMLPropertyListTest class categoriesFor: #plistStringWithSingleQuotes!public! !
!XMLPropertyListTest class categoriesFor: #plistTrue!public! !
!XMLPropertyListTest class categoriesFor: #plistWrongVersion!public! !
!XMLPropertyListTest class categoriesFor: #propertyListTypeConstant!public! !

"Binary Globals"!

