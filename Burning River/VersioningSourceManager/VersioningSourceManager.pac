| package |
package := Package name: 'VersioningSourceManager'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #VersioningSourceManager;
	yourself.

package methodNames
	add: #PackageSelector -> #checkIn;
	add: #PackageSelector -> #checkOut;
	add: #PackageSelector -> #checkOutChanges;
	add: #SourceBrowser -> #checkIn;
	add: #SourceBrowser -> #checkOut;
	add: #SourceManager -> #isVersioning;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\IDE\Standard Edition\Standard Edition Tools';
	yourself).

package!

"Class Definitions"!

SourceManager subclass: #VersioningSourceManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!PackageSelector methodsFor!

checkIn
	"Check in all files in all selected packages."

	| errorString |

	errorString := SourceManager default 
				checkInAll: self selectedSourceObjects
				withComment: 'Revised'.
	self
		refresh;
		setFocus.
	errorString size > 0 
		ifTrue: 
			[ errorString := 'The following errors were noted during check-in:' , String lineDelimiter,
						String lineDelimiter , errorString.
			Transcript show: errorString; cr.
			MessageBox 
				notify: errorString
				caption: 'Errors found during check-in']!

checkOut
	"Check out all files in all selected packages."

	| errorString |
	errorString := SourceManager default checkOutAll: self selectedSourceObjects.
	self
		refresh;
		setFocus.
	errorString size > 0 
		ifTrue: 
			[ errorString := 'The following errors were noted during check-out:' , String lineDelimiter,
						String lineDelimiter , errorString.
			Transcript show: errorString; cr.
			MessageBox 
				notify: errorString
				caption: 'Errors found during check-out']!

checkOutChanges
	"Check out all changed files in all selected packages."

	| errorString |
	errorString := SourceManager default 
				checkOutAll: (self selectedSourceObjects select: [:each | each isChanged]).
	self
		refresh;
		setFocus.
	errorString size > 0 
		ifTrue: 
			[ errorString := 'The following errors were noted during check-out:' , String lineDelimiter,
						String lineDelimiter , errorString.
			Transcript show: errorString; cr.
			MessageBox 
				notify: errorString
				caption: 'Errors found during check-out']! !
!PackageSelector categoriesFor: #checkIn!public! !
!PackageSelector categoriesFor: #checkOut!public! !
!PackageSelector categoriesFor: #checkOutChanges!public! !

!SourceBrowser methodsFor!

checkIn
	| errorString |

	errorString:=  SourceManager default checkInAll: self selectedSourceObjects withComment: 'Revised'. 
	self refresh; setFocus.

	errorString size > 0 ifTrue:
		[ errorString := 'The following errors were noted during check-in:',
					 String lineDelimiter, String lineDelimiter, errorString.
		Transcript show: errorString; cr.
		MessageBox
			notify: errorString
			caption: 'Errors found during check-in' ]!

checkOut
	| errorString |

	errorString:=  SourceManager default checkOutAll: self selectedSourceObjects.
	self refresh; setFocus.

	errorString size > 0 ifTrue:
		[ errorString := 'The following errors were noted during check-out:',
				 String lineDelimiter, String lineDelimiter, errorString.
		Transcript show: errorString; cr.
		MessageBox
			notify: errorString
			caption: 'Errors found during check-out' ]
! !
!SourceBrowser categoriesFor: #checkIn!operations!public! !
!SourceBrowser categoriesFor: #checkOut!operations!public! !

!SourceManager methodsFor!

isVersioning
	"Answer true if the underlying source code manager supports versioning, false otherwise."
	^false! !
!SourceManager categoriesFor: #isVersioning!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

VersioningSourceManager guid: (GUID fromString: '{A54ADA78-1ED9-11D5-BE0B-00010240D5E2}')!
VersioningSourceManager comment: 'This is the base class for access to file-based version control systems.  It doesn''t implement access to any particular version control system; instead, subclasses which implement the VersioningSourceManager protocol are used to provide the actual implementation.

Changes:
08-Dec-2003  RPJ  Changed to support the new #classCommented: event.'!
!VersioningSourceManager categoriesForClass!Unclassified! !
!VersioningSourceManager methodsFor!

checkIn: aSourceObject withComment: aStringComment
	"Private - Check in aStringFilepath.  Signal an error if unsuccessful."

	self subclassResponsibility!

checkInAll: selectedSourceObjects withComment: aStringComment
	| result errorString |

	errorString := ''.
	selectedSourceObjects do:
		[ : each |
		result := self checkIn: each withComment: aStringComment.
		(self resultsContainErrors: result)
			ifTrue: [ errorString := errorString, String lineDelimiter, String lineDelimiter, result ]
			ifFalse: [ each isChanged: false ] ].

	^errorString!

checkOut: aStringFilepath
	self subclassResponsibility!

checkOutAll: selectedSourceObjects
	| result errorString |

	errorString := ''.
	selectedSourceObjects do:
		[ : each |
		result := self checkOut: each fileOutName.
		(self resultsContainErrors: result) ifTrue:
			[ errorString := errorString, String lineDelimiter, String lineDelimiter, result ] ].

	^errorString!

checkOutClass: aClass
	| package |

	(package := Package manager packageOfClass: aClass) notNil
		ifTrue: [ self checkOut: package fileOutName.
			   self checkOut: aClass fileOutName ]!

checkOutMethod: aCompiledMethod
	"If this is a loose method we need to check out the package file.  Otherwise we need to check out
	both the package file and the class file."

	| package |
	package := Package manager packageOfMethod: aCompiledMethod.
	package isNil 
		ifTrue: 
			["Normal case - method is packaged with its class"
			(Package manager packageOfClass: aCompiledMethod methodClass) notNil 
				ifTrue: 
					[package := Package manager packageOfMethodOrItsClass: aCompiledMethod.
					package notNil ifTrue: [self checkOut: package fileOutName].
					aCompiledMethod isClassMethod 
						ifTrue: [self checkOut: aCompiledMethod methodClass instanceClass fileOutName]
						ifFalse: [self checkOut: aCompiledMethod methodClass fileOutName]]]
		ifFalse: 
			["Exceptional case - this is a loose method in a package"
			self checkOut: package fileOutName]!

finalize
	"Private - Cancel any outstanding event registrations"

	super finalize.
	self removeEvents.!

get: aStringFilepath
	"Get the latest version of aStringFilepath from the receiver.  Answer the"
	"results of the SCCS 'get' command."

	self subclassResponsibility!

initialize
	super initialize.
	self beFinalizable.
	self registerEvents!

isFileCheckedOut: aFilename
	self subclassResponsibility!

isVersioning
	"Answer true if the underlying source code manager supports versioning, false otherwise."
	^true!

onClassAdded: aClass
	"Transcript show: 'VersioningSourceManager>>onClassAdded: ', aClass name; cr."

	self checkOutClass: aClass!

onClassCategorized: aClass
	"Transcript show: 'VersioningSourceManager>>onClassCategorized: ', aClass name; cr."

	self checkOutClass: aClass!

onClassCommented: aClass
	"Transcript show: 'VersioningSourceManager>>onClassCommented: ', aClass name; cr."

	aClass isMeta ifFalse: [ self checkOutClass: aClass ]!

onClassRemoved: aClass
	"Transcript show: 'VersioningSourceManager>>onClassRemoved: ', aClass name; cr."

	self checkIn: aClass withComment: 'Removed'.
	aClass isChanged: false!

onClassRepackaged: aClass from: sourcePackage to: destinationPackage
	"Transcript show: 'VersioningSourceManager>>onClassRepackaged: ', aClass name, ' from: ',
				sourcePackage name, ' to: ', destinationPackage name; cr."

	sourcePackage notNil ifTrue: [ self checkOut: sourcePackage fileOutName ].
	destinationPackage notNil ifTrue: [ self checkOut: destinationPackage fileOutName ].
	(Package manager packageOfClass: aClass) notNil
		ifTrue: [ self checkOut: aClass fileOutName ]!

onClassUpdated: aClass
	"Transcript show: 'VersioningSourceManager>>onClassUpdated: ', aClass name; cr."

	aClass isMeta ifFalse: [ self checkOutClass: aClass ]!

onGlobalRemoved: anAssociation
	"Transcript show: 'VersioningSourceManager>>onGlobalRemoved: ', anAssociation displayString; cr."

	(Package manager packageOfGlobalNamed: anAssociation key) notNil
		ifTrue: [ self checkOut: (Package manager packageOfGlobalNamed: anAssociation key) fileOutName ]!

onGlobalRenamed: anAssociation from: oldSymbol
	"Transcript show: 'VersioningSourceManager>>onGlobalRenamed: ', anAssociation displayString,
				' from: ', oldSymbol printString; cr."

	(Package manager packageOfGlobalNamed: anAssociation key) notNil
		ifTrue: [ self checkOut: (Package manager packageOfGlobalNamed: anAssociation key) fileOutName ]!

onGlobalRepackaged: globalName from: sourcePackage to: destinationPackage
	"Transcript show: 'VersioningSourceManager>>onGlobalRepackaged: ', globalName printString,
				' from: ', sourcePackage printString, ' to: ', destinationPackage printString; cr."

	sourcePackage notNil ifTrue: [ self checkOut: sourcePackage fileOutName ].
	destinationPackage notNil ifTrue: [ self checkOut: destinationPackage fileOutName ].
	(Package manager packageOfGlobalNamed: globalName) notNil
		ifTrue: [ self checkOut: (Package manager packageOfGlobalNamed: globalName) fileOutName ]!

onMethodAdded: aCompilationResult
	"Transcript show: 'VersioningSourceManager>>onMethodAdded: ',
				aCompilationResult method name displayString; cr."

	self checkOutMethod: aCompilationResult method!

onMethodCategorized: aCompiledMethod
	"Transcript show: 'VersioningSourceManager>>onMethodCategorized: ',
				aCompiledMethod name displayString; cr."

	self checkOutMethod: aCompiledMethod
!

onMethodRemoved: aCompiledMethod
	"Transcript show: 'VersioningSourceManager>>onMethodRemoved: ',
				aCompiledMethod name displayString; cr."

	self checkOutMethod: aCompiledMethod
!

onMethodRepackaged: aCompiledMethod from: sourcePackage to: destinationPackage
	"Transcript show: 'VersioningSourceManager>>onMethodRepackaged: ', aCompiledMethod name displayString,
				 ' from: ', sourcePackage name, ' to: ', destinationPackage name; cr."

	sourcePackage notNil ifTrue: [ self checkOut: sourcePackage fileOutName ].
	destinationPackage notNil ifTrue: [ self checkOut: destinationPackage fileOutName ].
	self checkOutMethod: aCompiledMethod
!

onMethodUpdated: aCompilationResult
	"Transcript show: 'VersioningSourceManager>>onMethodUpdated: ',
				aCompilationResult method name displayString; cr."

	self checkOutMethod: aCompilationResult method!

onOwnedChanged: aPackage
	"Transcript show: 'VersioningSourceManager>>onOwnedChanged: ', aPackage printString; cr."

	self checkOut: aPackage fileOutName!

onPackageChanged: aPackage
	"Transcript show: 'VersioningSourceManager>>onPackageChanged: ', aPackage printString; cr."

	aPackage isChanged ifTrue: [ self checkOut: aPackage fileOutName ]!

onResourceAdded: aResourceIdentifier
	"Transcript show: 'VersioningSourceManager>>onResourceAdded: ', aResourceIdentifier name; cr."

	(Package manager packageOfResourceIdentifierOrItsClass: aResourceIdentifier) notNil
		ifTrue: [ self checkOut: aResourceIdentifier fileOutName ]!

onResourceRemoved: aResourceIdentifier
	"Transcript show: 'VersioningSourceManager>>onResourceRemoved: ', aResourceIdentifier name; cr."

	(Package manager packageOfResourceIdentifierOrItsClass: aResourceIdentifier) notNil
		ifTrue: [ self checkOut: aResourceIdentifier fileOutName ]!

onResourceRepackaged: aResourceIdentifier from: sourcePackage to: destinationPackage
	"Transcript show: 'VersioningSourceManager>>onResourceRepackaged: ', aResourceIdentifier name, ' from: ',
				sourcePackage name, ' to: ', destinationPackage name; cr."

	sourcePackage notNil ifTrue: [ self checkOut: sourcePackage fileOutName ].
	destinationPackage notNil ifTrue: [ self checkOut: destinationPackage fileOutName ].
	(Package manager packageOfResourceIdentifierOrItsClass: aResourceIdentifier) notNil
		ifTrue: [ self checkOut: aResourceIdentifier fileOutName ]!

onResourceUpdated: aResourceIdentifier
	"Transcript show: 'VersioningSourceManager>>onResourceUpdated: ', aResourceIdentifier name; cr."

	(Package manager packageOfResourceIdentifierOrItsClass: aResourceIdentifier) notNil
		ifTrue: [ self checkOut: aResourceIdentifier fileOutName ]!

registerEvents
	Smalltalk
		when: #globalRenamed:from: send: #onGlobalRenamed:from: to: self;
		when: #methodAdded: send: #onMethodAdded: to: self;
		when: #methodUpdated: send: #onMethodUpdated: to: self;
		when: #methodRemoved: send: #onMethodRemoved: to: self;
		when: #methodCategorized: send: #onMethodCategorized: to: self;
		when: #classAdded: send: #onClassAdded: to: self;
		when: #classUpdated: send: #onClassUpdated: to: self;
		when: #classRemoved: send: #onClassRemoved: to: self;
		when: #classCategorized: send: #onClassCategorized: to: self;
		when: #classCommented: send: #onClassCommented: to: self;
		when: #globalRemoved: send: #onGlobalRemoved: to: self.

	Package manager
		when: #globalRepackaged:from:to: send: #onGlobalRepackaged:from:to: to: self;
		when: #classRepackaged:from:to: send: #onClassRepackaged:from:to: to: self;
		when: #resourceRepackaged:from:to: send: #onResourceRepackaged:from:to: to: self;
		when: #methodRepackaged:from:to: send: #onMethodRepackaged:from:to: to: self;
		when: #packageChanged: send: #onPackageChanged: to: self;
		when: #ownedChanged: send: #onOwnedChanged: to: self.

	SessionManager current resourceManager
		when: #resourceAdded: send: #onResourceAdded: to: self;
		when: #resourceUpdated: send: #onResourceUpdated: to: self;
		when: #resourceRemoved: send: #onResourceRemoved: to: self!

removeEvents
	Smalltalk removeEventsTriggeredFor: self.
	Package manager removeEventsTriggeredFor: self.
	SessionManager current resourceManager removeEventsTriggeredFor: self!

resultsContainErrors: aString
	"Answer 'true' if aString contains an error message returned from the underlying source
	code management system; otherwise answer 'false'."

	self subclassResponsibility! !
!VersioningSourceManager categoriesFor: #checkIn:withComment:!private!source code control! !
!VersioningSourceManager categoriesFor: #checkInAll:withComment:!public! !
!VersioningSourceManager categoriesFor: #checkOut:!private!source code control! !
!VersioningSourceManager categoriesFor: #checkOutAll:!public! !
!VersioningSourceManager categoriesFor: #checkOutClass:!event handling!private! !
!VersioningSourceManager categoriesFor: #checkOutMethod:!helpers!private! !
!VersioningSourceManager categoriesFor: #finalize!finalizing!private! !
!VersioningSourceManager categoriesFor: #get:!public!source code control! !
!VersioningSourceManager categoriesFor: #initialize!initialization!public! !
!VersioningSourceManager categoriesFor: #isFileCheckedOut:!helpers!public! !
!VersioningSourceManager categoriesFor: #isVersioning!public!testing! !
!VersioningSourceManager categoriesFor: #onClassAdded:!event handling!private! !
!VersioningSourceManager categoriesFor: #onClassCategorized:!event handling!private! !
!VersioningSourceManager categoriesFor: #onClassCommented:!event handling!private! !
!VersioningSourceManager categoriesFor: #onClassRemoved:!event handling!private! !
!VersioningSourceManager categoriesFor: #onClassRepackaged:from:to:!event handling!private! !
!VersioningSourceManager categoriesFor: #onClassUpdated:!event handling!private! !
!VersioningSourceManager categoriesFor: #onGlobalRemoved:!event handling!private! !
!VersioningSourceManager categoriesFor: #onGlobalRenamed:from:!event handling!private! !
!VersioningSourceManager categoriesFor: #onGlobalRepackaged:from:to:!event handling!private! !
!VersioningSourceManager categoriesFor: #onMethodAdded:!event handling!private! !
!VersioningSourceManager categoriesFor: #onMethodCategorized:!event handling!private! !
!VersioningSourceManager categoriesFor: #onMethodRemoved:!event handling!private! !
!VersioningSourceManager categoriesFor: #onMethodRepackaged:from:to:!event handling!private! !
!VersioningSourceManager categoriesFor: #onMethodUpdated:!event handling!private! !
!VersioningSourceManager categoriesFor: #onOwnedChanged:!event handling!private! !
!VersioningSourceManager categoriesFor: #onPackageChanged:!event handling!private! !
!VersioningSourceManager categoriesFor: #onResourceAdded:!event handling!private! !
!VersioningSourceManager categoriesFor: #onResourceRemoved:!event handling!private! !
!VersioningSourceManager categoriesFor: #onResourceRepackaged:from:to:!event handling!private! !
!VersioningSourceManager categoriesFor: #onResourceUpdated:!event handling!private! !
!VersioningSourceManager categoriesFor: #registerEvents!event handling!private! !
!VersioningSourceManager categoriesFor: #removeEvents!event handling!private! !
!VersioningSourceManager categoriesFor: #resultsContainErrors:!public! !

VersioningSourceManager methodProtocol: #VersioningSourceManager attributes: #() selectors: #(#checkIn:withComment: #checkOut: #get: #isFileCheckedOut: #resultsContainErrors:)!

!VersioningSourceManager class methodsFor!

new
	^super new initialize! !
!VersioningSourceManager class categoriesFor: #new!instance creation!public! !

"Binary Globals"!

"Resources"!

