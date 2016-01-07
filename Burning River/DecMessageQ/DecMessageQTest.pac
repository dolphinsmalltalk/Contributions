| package |
package := Package name: 'DecMessageQTest'.
package paxVersion: 0;
	basicComment: 'SUnit tests for the DecMessageQ package.  To test make sure the SUnit package and associated packages are installed, then evaluate

	TestRunner show

Select some of the DMQ tests from the dropdown and Run them.

Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #DMQQueueTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'DecMessageQ';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #DMQQueueTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

DMQQueueTest guid: (GUID fromString: '{7831520B-125E-11D5-BE08-00010240D5E2}')!
DMQQueueTest comment: 'Unit test for classes in the DecMessageQ package.

Due to server limitations (VMS servers running in multithreaded mode will not allow Windows clients to attach to permanent queues) #testAttachByName and #testGetFromPermanentQueue are commented out.  If you are working with a non-VMS DMQ server, or a VMS server running in single client mode, you can re-enable these tests.'!
!DMQQueueTest categoriesForClass!Unclassified! !
!DMQQueueTest methodsFor!

tearDown
	[ DMQQueue disconnect ] on: Error do: [ :exception | "Ignore any errors during teardown" ]!

testAttach
	| tq |

	tq := DMQTemporaryQueue new.
	self shouldnt: [ tq attach ] raise: Error.
	self should: [ tq status code = DMQConstants current PAMS__SUCCESS ].
	self shouldnt: [ tq detach ] raise: Error.
	self should: [ (tq status code = DMQConstants current PAMS__SUCCESS) |
			   (tq status code = DMQConstants current PAMS__DETACHED) ]
!

testAttachByName
	| q |

	"Per the Windows Client Notes, attaching by name or number to a permanent queue does not work
	 when the Client Library Server (CLS) is on a VMS system and is being run in multithreaded mode.
	 This being the case there's no use running this test."

	"q := DMQPermanentQueue new.
	q attachByName: 'TEST_QUEUE'.
	self should: [ q status code = DMQConstants current PAMS__SUCCESS ].
	q detach.
	self should: [ (q status code = DMQConstants current PAMS__SUCCESS) |
			   (q status code = DMQConstants current PAMS__DETACHED) ]"
!

testAttachTemporary
	| tq |

	tq := DMQTemporaryQueue new.
	self shouldnt: [ tq attach ] raise: Error.
	self should: [ tq status code = DMQConstants current PAMS__SUCCESS ].
	self shouldnt: [ tq detach ] raise: Error.
	self should: [ (tq status code = DMQConstants current PAMS__SUCCESS) |
			   (tq status code = DMQConstants current PAMS__DETACHED) ]
!

testGetFromPermanentQueue
	| q msg |

	"Per the Windows Client Notes, attaching by name or number to a permanent queue does not work
	 when the Client Library Server (CLS) is on a VMS system, and is being run in multithreaded mode.
	 This being the case we just skip these cases."

	"q := DMQPermanentQueue new.

	q attachByName: 'TEST_QUEUE'.
	self should: [ q status code = DMQConstants current PAMS__SUCCESS ].

	msg := q getNextMessage.
	self should: [ q status code = DMQConstants current PAMS__SUCCESS ].

	q detach.
	self should: [ q status code = DMQConstants current PAMS__SUCCESS ]"!

testLocate
	| tq testQAddress |

	"Attach to the message bus so other functions will work."

	tq := DMQTemporaryQueue new.
	self shouldnt: [ tq attach ] raise: Error.
	self should: [ tq status code = DMQConstants current PAMS__SUCCESS ].

	self shouldnt: [ testQAddress := DMQQueue addressOfQueueNamed: 'TEST_QUEUE' ] raise: Error.
	self should: [ testQAddress group = 56 ].
	self should: [ testQAddress queue = 49 ].

	self shouldnt: [ tq detach ] raise: Error.
	self should: [ tq status code = DMQConstants current PAMS__DETACHED ]!

testSend
	| msg tq noteClass pcType |

	noteClass := 500.
	pcType := 10.

	msg := DMQMessage new.
	msg
		bytes: 'TO=JARVISB;SUBJECT=Test Message;BODY=This is a test message';
		messageClass: noteClass;
		type: pcType.

	tq := DMQTemporaryQueue new.
	self shouldnt: [ tq attach ] raise: Error.
	self should: [ tq status code = DMQConstants current PAMS__SUCCESS ].

	self shouldnt: [ tq send: msg to: 'CMS_NOTES' ] raise: Error.
	self should: [ tq status code = DMQConstants current PAMS__SUCCESS ].

	self shouldnt: [ tq send: msg to: 'TEST_QUEUE' ] raise: Error.
	self should: [ tq status code = DMQConstants current PAMS__SUCCESS ].

	self shouldnt: [ tq detach ] raise: Error! !
!DMQQueueTest categoriesFor: #tearDown!public!running! !
!DMQQueueTest categoriesFor: #testAttach!public!testing! !
!DMQQueueTest categoriesFor: #testAttachByName!public!testing! !
!DMQQueueTest categoriesFor: #testAttachTemporary!public!testing! !
!DMQQueueTest categoriesFor: #testGetFromPermanentQueue!public!testing! !
!DMQQueueTest categoriesFor: #testLocate!public!testing! !
!DMQQueueTest categoriesFor: #testSend!public!testing! !

"Binary Globals"!

"Resources"!

