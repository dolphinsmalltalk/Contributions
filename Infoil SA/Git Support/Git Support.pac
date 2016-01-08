| package |
package := Package name: 'Git Support'.
package paxVersion: 1;
	basicComment: 'The MIT License (MIT)
Copyright (c) 2015 Infoil S.A. http://www.infoil.com.ar

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicScriptAt: #postuninstall put: 'CodePatcher unpatch: SourceManager selector: #compressChanges'.

package classNames
	add: #GitBranch;
	add: #GitChange;
	add: #GitClient;
	add: #GitCommit;
	add: #GitObject;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Burning River\ExternalProcess\ExternalProcess';
	add: '..\..\..\US\US ExternalProcess Extensions';
	yourself).

package!

"Class Definitions"!

Object subclass: #GitObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitObject subclass: #GitBranch
	instanceVariableNames: 'name current'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitObject subclass: #GitChange
	instanceVariableNames: 'x y name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitObject subclass: #GitClient
	instanceVariableNames: 'directory'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitObject subclass: #GitCommit
	instanceVariableNames: 'sha1 author authorDate message'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

GitObject owningPackageNamed: 'Git Support' !
GitObject guid: (GUID fromString: '{7424EE9E-87FE-4F64-B136-FD1A2F60CEB8}')!
GitObject comment: ''!
!GitObject categoriesForClass!Kernel-Objects! !
!GitObject class methodsFor!

new

	^super new initialize! !
!GitObject class categoriesFor: #new!public! !

GitBranch owningPackageNamed: 'Git Support' !
GitBranch guid: (GUID fromString: '{CC953DF8-556E-496A-B850-70843EAE8983}')!
GitBranch comment: ''!
!GitBranch categoriesForClass!Unclassified! !
!GitBranch methodsFor!

current
	^current!

current: anObject
	current := anObject!

initializeOn: aString

	self current: aString first = $*.
	self name: (self isCurrent ifTrue: [aString copyFrom: 3 to: aString size] ifFalse: [aString])!

isCurrent
	^self current!

name
	^name!

name: anObject
	name := anObject! !
!GitBranch categoriesFor: #current!accessing!public! !
!GitBranch categoriesFor: #current:!accessing!private! !
!GitBranch categoriesFor: #initializeOn:!private! !
!GitBranch categoriesFor: #isCurrent!accessing!public! !
!GitBranch categoriesFor: #name!accessing!public! !
!GitBranch categoriesFor: #name:!accessing!private! !

!GitBranch class methodsFor!

fromString: aString

	^self new initializeOn: aString! !
!GitBranch class categoriesFor: #fromString:!public! !

GitChange owningPackageNamed: 'Git Support' !
GitChange guid: (GUID fromString: '{27B6B4B9-1184-49D6-88FE-8BA07A37E386}')!
GitChange comment: ''!
!GitChange categoriesForClass!Kernel-Objects! !
!GitChange methodsFor!

name
	^name!

name: anObject
	name := anObject!

x
	^x!

x: anObject
	x := anObject!

y
	^y!

y: anObject
	y := anObject! !
!GitChange categoriesFor: #name!accessing!public! !
!GitChange categoriesFor: #name:!accessing!private! !
!GitChange categoriesFor: #x!accessing!public! !
!GitChange categoriesFor: #x:!accessing!private! !
!GitChange categoriesFor: #y!accessing!public! !
!GitChange categoriesFor: #y:!accessing!private! !

!GitChange class methodsFor!

fromString: aString

	| stream |
	stream := aString readStream.
	^self new
		x: stream next;
		y: stream next;
		name: stream upToEnd trimBlanks;
		yourself! !
!GitChange class categoriesFor: #fromString:!public! !

GitClient owningPackageNamed: 'Git Support' !
GitClient guid: (GUID fromString: '{34E7367B-CFF7-4072-9E8B-49A9A5212C8C}')!
GitClient comment: ''!
!GitClient categoriesForClass!Kernel-Objects! !
!GitClient methodsFor!

branches

	^self git: 'branch --list' linesCollect: [:line | GitBranch fromString: line]!

changes

	^self git: 'status --porcelain' linesCollect: [:line | GitChange fromString: line]!

commitWithSha: sha1 
	^(self git: ('log --pretty=format:%H;%an;%ai;%s -n 1 <1d>' expandMacrosWith: sha1) linesCollect: [:each | GitCommit fromString: each]) firstOrNil!

git: command 
	"Answer a readStream over the results of the given command."

	| cmd outputPipe process |
	cmd := (File composePath: self gitPath subPath: 'git.exe') , ' ' , command.
	outputPipe := ExternalPipe new.
	process := ExternalProcess new.
	process commandLine: cmd.
	process directory: directory.
	process stdoutPipe: outputPipe.
	process executeSync.
	process processExitCode ~= 0 
		ifTrue: 
			[Transcript
				show: ('Git Exit code <1d> for command "<2d>". Is there a git repository in: <3d>?' 
							expandMacrosWith: process processExitCode
							with: cmd
							with: directory);
				cr].
	^outputPipe readStream!

git: command linesCollect: aBlock

	| coll |
	coll := OrderedCollection new.
	self git: command linesDo: [:line | coll add: (aBlock value: line)].
	^coll!

git: command linesDo: aBlock 
	self git: command
		resultDo: 
			[:stream | 
			[stream atEnd] whileFalse: 
					[| line |
					line := stream nextLine trimBlanks.
					line notEmpty ifTrue: [aBlock value: line]]]!

git: command resultDo: aBlock

	aBlock value: (self git: command)!

gitPath

	^self class gitPath!

initializeOn: path

	directory := path!

lastCommit

	^(self lastCommitN: 1) lastOrNil!

lastCommitN: lastN

	^self git: ('log --pretty=format:%H;%an;%ai;%s -n <1d>' expandMacrosWith: lastN) linesCollect: [:each | GitCommit fromString: each]! !
!GitClient categoriesFor: #branches!public! !
!GitClient categoriesFor: #changes!public! !
!GitClient categoriesFor: #commitWithSha:!public! !
!GitClient categoriesFor: #git:!public! !
!GitClient categoriesFor: #git:linesCollect:!public! !
!GitClient categoriesFor: #git:linesDo:!public! !
!GitClient categoriesFor: #git:resultDo:!public! !
!GitClient categoriesFor: #gitPath!public! !
!GitClient categoriesFor: #initializeOn:!private! !
!GitClient categoriesFor: #lastCommit!public! !
!GitClient categoriesFor: #lastCommitN:!public! !

!GitClient class methodsFor!

gitPath

	| dirs |
	dirs := Array with: 'c:\Program Files (x86)\Git\bin' with: 'C:\Program Files\Git\bin'.
	^dirs detect: [:each | File exists: each] ifNone: [self error: 'Is Git installed? If it''s installed, please add the installation dir to the know dirs list.']!

new

	^self on: FileLocator imageRelative basePath!

on: directory

	^super new initializeOn: directory! !
!GitClient class categoriesFor: #gitPath!public! !
!GitClient class categoriesFor: #new!public! !
!GitClient class categoriesFor: #on:!public! !

GitCommit owningPackageNamed: 'Git Support' !
GitCommit guid: (GUID fromString: '{DDA3E4BD-FAF5-481F-9948-C63D7F74E4AB}')!
GitCommit comment: ''!
!GitCommit categoriesForClass!Kernel-Objects! !
!GitCommit methodsFor!

= comparand 
	^self class = comparand class and: [self sha1 = comparand sha1]!

author
	^author!

author: aString 
	author := aString!

authorDate
	^authorDate!

authorDate: aTimeStamp 
	authorDate := aTimeStamp!

displayOn: aStream

	aStream
		nextPutAll: self sha1; cr;
		nextPutAll: self author; cr;
		nextPutAll: self authorDate displayString; cr;
		nextPutAll: self message!

hash

	^self sha1 hash!

initialize
	super initialize.
	sha1 := author := message := ''.
	authorDate := TimeStamp current!

message
	^message!

message: aString 
	message := aString!

sha1
	^sha1!

sha1: aString 
	sha1 := aString! !
!GitCommit categoriesFor: #=!public! !
!GitCommit categoriesFor: #author!public! !
!GitCommit categoriesFor: #author:!public! !
!GitCommit categoriesFor: #authorDate!public! !
!GitCommit categoriesFor: #authorDate:!public! !
!GitCommit categoriesFor: #displayOn:!public! !
!GitCommit categoriesFor: #hash!public! !
!GitCommit categoriesFor: #initialize!private! !
!GitCommit categoriesFor: #message!public! !
!GitCommit categoriesFor: #message:!public! !
!GitCommit categoriesFor: #sha1!public! !
!GitCommit categoriesFor: #sha1:!public! !

!GitCommit class methodsFor!

convertDate: aString 
	| parts date time |
	parts := aString subStrings.
	date := Date fromString: parts first format: 'YYYY-MM-DD'.
	time := Time fromString: parts second.
	^TimeStamp date: date time: time!

fromString: aString 
	| stream |
	stream := aString readStream.
	^self new
		sha1: (stream upTo: $;) trimBlanks;
		author: (stream upTo: $;) trimBlanks;
		authorDate: (self convertDate: (stream upTo: $;) trimBlanks);
		message: stream upToEnd trimBlanks;
		yourself! !
!GitCommit class categoriesFor: #convertDate:!public! !
!GitCommit class categoriesFor: #fromString:!public! !

"Binary Globals"!

