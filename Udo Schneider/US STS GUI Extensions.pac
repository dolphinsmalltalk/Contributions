| package |
package := Package name: 'US STS GUI Extensions'.
package paxVersion: 1;
	basicComment: '$id: US STS GUI Extensions 0.014$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 18.09.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.014'.


package classNames
	add: #StsRepositoryChooser;
	yourself.

package methodNames
	add: #Package -> #basicGetStsStatus;
	add: #Package -> #isCurrentInSTS;
	add: #Package -> #resetStsStatus;
	add: #Package -> #stsStatus;
	add: #Package -> #stsStatusColor;
	add: #Package -> #stsStatusIcon;
	add: #Package -> #stsStatusIconImageIndex;
	add: #Package -> #stsStatusSortOrder;
	add: #Package -> #updateCommentFromSts;
	add: #Package -> #updateStsStatus;
	add: #PackageBrowserShell -> #versionPackage;
	add: #SmalltalkSystem -> #browsePackageEditions;
	add: #SmalltalkSystem -> #browseProjectEditions;
	add: #StsImportInformation -> #compareEditions;
	add: #StsImportInformation -> #createComponents;
	add: #StsImportInformation -> #onViewOpened;
	add: #StsImportInformation -> #package;
	add: #StsImportInformation -> #queryCommand:;
	add: #StsImportInformation -> #updateCommentComparing:;
	add: #StsPackageEdition -> #comment:;
	add: #StsPackageEdition -> #printDateTagOn:;
	add: #StsPackageEdition -> #printDeveloperTagOn:;
	add: #StsPackageEdition -> #printDolphinVersionOn:;
	add: #StsPackageEdition -> #printIdTagOn:;
	add: #StsPackageEdition -> #printNameTagOn:;
	add: #StsPackageEdition -> #printTimestampTagOn:;
	add: #StsPackageEdition -> #printTimeTagOn:;
	add: #StsPackageEdition -> #printVersionTagOn:;
	add: #StsPackageEdition -> #updateComment:;
	add: #StsPackageEditionPrompter -> #createSchematicWiring;
	add: #StsPackageEditionsBrowserShell -> #shortCaption;
	add: #StsPackageManager -> #onPackageChanged:;
	add: #StsPackageProxy -> #stsStatus;
	add: #StsPackageProxy -> #updateStsStatus;
	add: #StsProjectBrowserShell -> #shortCaption;
	add: 'StsImportInformation class' -> #resource_Version_package;
	add: 'StsPackageManager class' -> #resetPackageStsStatus;
	add: 'StsPackageManager class' -> #updatePackageStsStatus;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Rich Text Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\ITC Gorisek\Source Tracking System';
	add: 'US STS Extensions';
	yourself).

package setManualPrerequisites: #(
	'US STS Extensions').

package!

"Class Definitions"!

SmalltalkToolShell subclass: #StsRepositoryChooser
	instanceVariableNames: 'repositoriesPresenter'
	classVariableNames: 'KnownRepositories'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Package methodsFor!

basicGetStsStatus
	self isInSTS ifFalse: [^#notInSTS].
	self isObjectArtsPackage 
		ifTrue: 
			[self isChanged ifTrue: [^#newerThanSTS].
			^#recentInSTS].
	self isCurrentInSTS ifFalse: [^#olderThanSTS].
	self isChanged ifTrue: [^#newerThanSTS].
	self isStsPathChanged ifFalse: [^#newerThanSTS].
	^#recentInSTS!

isCurrentInSTS
	^self packageEditions first versionDescriptor = packageVersion!

resetStsStatus
self removePropertyAt: #stsStatus!

stsStatus
	^self propertyAt: #stsStatus ifAbsent: [self updateStsStatus]!

stsStatusColor
	^##((Dictionary new)
		at: #notInSTS put: Color red;
		at: #newerThanSTS put: Color blue;
		at: #olderThanSTS put: (Color red: 214 green: 214 blue: 0) ;
		at: #recentInSTS put: Color darkGreen;
		yourself) at: self stsStatus!

stsStatusIcon
	| iconName |
	iconName := ((Dictionary new)
				at: #notInSTS put: 'stsStatusUnversioned.ico';
				at: #newerThanSTS put: 'stsStatusNewer.ico';
				at: #olderThanSTS put: 'stsStatusOlder.ico';
				at: #recentInSTS put: 'stsStatusRecent.ico';
				yourself) at: self stsStatus.
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\' , iconName.
	"^Icon fromFile: iconName
		usingLocator: (FolderRelativeFileLocator 
				basePath: (FileLocator imageRelative localFileSpecFor: 'Udo Schneider\Goodies\Resources\'))"!

stsStatusIconImageIndex
	^self stsStatusIcon imageIndex!

stsStatusSortOrder
	^##(Array 
		with: #notInSTS
		with: #olderThanSTS
		
		with: #newerThanSTS with: #recentInSTS) keyAtValue: self stsStatus!

updateCommentFromSts
	|  wasChanged |
	wasChanged := self isChanged.
	((StsManager current getPackageEditionsFor: self name) 
		detect: [:each | each packageVersion = packageVersion]
		ifNone: [nil]) ifNotNil: [:packageEdition | self comment: packageEdition comment].
	self isChanged: wasChanged!

updateStsStatus
	
	^self propertyAt: #stsStatus put: self basicGetStsStatus! !
!Package categoriesFor: #basicGetStsStatus!private! !
!Package categoriesFor: #isCurrentInSTS!public!testing! !
!Package categoriesFor: #resetStsStatus!public! !
!Package categoriesFor: #stsStatus!public! !
!Package categoriesFor: #stsStatusColor!public! !
!Package categoriesFor: #stsStatusIcon!public! !
!Package categoriesFor: #stsStatusIconImageIndex!public! !
!Package categoriesFor: #stsStatusSortOrder!public! !
!Package categoriesFor: #updateCommentFromSts!public! !
!Package categoriesFor: #updateStsStatus!public! !

!PackageBrowserShell methodsFor!

versionPackage
	| packages firstPackage |
	packages := self packages.
	packages isEmpty ifTrue: [^self].
	firstPackage := packages first.
	self sourceControl versionPackage: firstPackage.
	firstPackage updateCommentFromSts.
	self updateComment.
	firstPackage save! !
!PackageBrowserShell categoriesFor: #versionPackage!event handling!must strip!public! !

!SmalltalkSystem methodsFor!

browsePackageEditions
	"Open a new package editions browser."

	StsPackageEditionsBrowserShell show!

browseProjectEditions
	"Open a new project editions browser."

	StsProjectBrowserShell show! !
!SmalltalkSystem categoriesFor: #browsePackageEditions!browsing!commands!must strip!public! !
!SmalltalkSystem categoriesFor: #browseProjectEditions!browsing!commands!must strip!public! !

!StsImportInformation methodsFor!

compareEditions
	previousVersionsPresenter selectionOrNil 
		ifNotNil: [:previousEdition | 
		self updateCommentComparing: previousEdition.
		StsManager current comparePackage: self package with: previousEdition]!

createComponents
	"Create the presenters contained by the receiver"

	super createComponents.
	versionPresenter := self add: TextPresenter new name: 'version'.
	developerPresenter := self add: TextPresenter new name: 'developer'.
	previousVersionsPresenter := self add: ListPresenter new name: 'previousVersions'.
	commentPresenter := self add: RichTextPresenter new name: 'comment'!

onViewOpened
	| selectionIndex |
	(selectionIndex := self model previousEdition isNil 
				ifFalse: 
					[previousVersionsPresenter list findFirst: 
							[:packageEdition | 
							packageEdition notNil 
								and: [packageEdition versionDescriptor = self model previousEdition versionDescriptor]]]
				ifTrue: [1]) > 1 
		ifTrue: [previousVersionsPresenter view isEnabled: false].
	previousVersionsPresenter selectionByIndex: selectionIndex.
	
	#USAdded.
	previousVersionsPresenter selectionOrNil 
		ifNotNil: 
			[:previousEdition | 
			self updateCommentComparing: previousEdition.
		]!

package
	^Package manager packageNamed: self model value previousEdition name!

queryCommand: aCommandQuery 
	"Private - Enter details about a potential command for the receiver into the <CommandQuery>
	argument."

	| cmd |
	cmd := aCommandQuery commandSymbol.
	#compareEditions == cmd 
		ifTrue: 
			[aCommandQuery isEnabled: previousVersionsPresenter selectionOrNil  notNil .
			^true].
	^super queryCommand: aCommandQuery!

updateCommentComparing: previousEdition 
	Cursor wait showWhile: 
			[| changes commentStream |
			changes := OrderedCollection new.
			previousEdition compareWithLoadedPackage: self package on: changes.
			commentStream := ReadWriteStream on: String new.
			commentStream nextPutAll: commentPresenter value.
			changes do: 
					[:each | 
					commentStream
						nextPutAll: ' * ' , each description displayString , ': ' , each elementDescription;
						cr].
			commentPresenter value: commentStream contents]! !
!StsImportInformation categoriesFor: #compareEditions!operations!public! !
!StsImportInformation categoriesFor: #createComponents!initializing!public! !
!StsImportInformation categoriesFor: #onViewOpened!public! !
!StsImportInformation categoriesFor: #package!operations!public! !
!StsImportInformation categoriesFor: #queryCommand:!commands!private! !
!StsImportInformation categoriesFor: #updateCommentComparing:!helpers!private! !

!StsImportInformation class methodsFor!

resource_Version_package
	"Answer the literal data from which the 'Version package' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Version_package)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 0 234 256 98 8 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 560 0 482 512 0 7 0 0 0 560 0 8 4294902505 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  251 21 834 571 51 560 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 560 770 8 #isTextModified: 98 1 32 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 125 0 0 0 10 0 0 0 154 1 0 0 35 0 0 0] 98 0 834 193 193 0 27 8 'version' 410 576 98 16 0 416 98 2 8 1140850816 1025 1104 0 482 512 0 7 0 0 0 1104 0 8 4294902505 674 0 0 3 706 202 208 98 3 770 800 98 2 834 251 91 834 571 51 1104 770 896 98 1 930 3 1 3 1104 770 976 98 1 32 1104 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 125 0 0 0 45 0 0 0 154 1 0 0 70 0 0 0] 98 0 1072 0 27 8 'developer' 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1144063491 1025 1472 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 7 0 0 0 1472 0 8 4294902445 8 ##(Smalltalk.BasicListAbstract)  1600 401 706 202 208 98 1 770 800 98 2 834 255 163 834 411 43 1472 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 0 0 0 81 0 0 0 76 1 0 0 102 0 0 0] 98 0 1072 0 27 8 'previousVersions' 410 8 ##(Smalltalk.RichTextEdit)  98 18 0 416 98 2 8 1144066500 1025 1904 0 482 512 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 10 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 2050 1 2082 8 #bePlain 8 '&Plain' 1 1 0 0 0 2050 1 2082 8 #toggleBold 8 '&Bold' 1 1 0 0 0 2050 1 2082 8 #toggleItalic 8 '&Italic' 1 1 0 0 0 2050 1 2082 8 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 2146 4097 2002 0 16 98 3 2050 1025 2082 8 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 2050 1025 2082 8 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 2050 1025 2082 8 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 2146 4097 2050 1 2082 8 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 78 101 119 32 82 111 109 97 110 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 834 193 193 0 1904 0 8 1960643778 674 0 0 9 0 655622 ##(Smalltalk.EDITSTREAM)  8 #[0 0 0 0 0 0 0 0 64 0 4 1] 706 202 208 98 6 770 800 98 2 834 21 281 834 801 231 1904 770 8 #contextMenu: 98 1 2016 1904 770 8 #text: 98 1 524550 ##(Smalltalk.RichText)  8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fswiss\fprq2\fcharset0 Arial;}}
\viewkind4\uc1\pard\f0\fs18 
\par }
' 1904 770 896 98 1 930 3 1 3 1904 770 976 98 1 32 1904 770 8 #resetCharFormat 1600 1904 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 140 0 0 0 154 1 0 0 255 0 0 0] 98 0 1072 0 27 8 'comment' 0 0 0 0 0 22719 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2117657919 706 202 208 98 3 770 800 98 2 834 2799 21 834 851 651 416 770 3136 98 1 8 'Versioning package' 416 770 8 #updateMenuBar 1600 416 1010 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 32 7 0 0 79 1 0 0] 98 11 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 3680 0 0 0 7 0 0 0 3680 0 8 4294902359 674 0 0 0 706 202 208 98 2 770 800 98 2 834 11 21 834 201 51 3680 770 3136 98 1 8 'Package version:' 3680 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 105 0 0 0 35 0 0 0] 98 0 1072 0 27 560 410 3696 98 16 0 416 98 2 8 1140850944 1 4000 0 0 0 7 0 0 0 4000 0 8 4294902359 674 0 0 0 706 202 208 98 2 770 800 98 2 834 11 91 834 241 51 4000 770 3136 98 1 8 'Package developer:' 4000 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 45 0 0 0 125 0 0 0 70 0 0 0] 98 0 1072 0 27 1104 410 3696 98 16 0 416 98 2 8 1140850944 1 4304 0 0 0 7 0 0 0 4304 0 8 4294902359 674 0 0 0 706 202 208 98 2 770 800 98 2 834 11 161 834 211 51 4304 770 3136 98 1 8 'Previous version:' 4304 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 80 0 0 0 110 0 0 0 105 0 0 0] 98 0 1072 0 27 1472 410 8 ##(Smalltalk.PushButton)  98 17 0 416 98 2 8 1140924416 1 4608 0 0 0 7 0 0 0 4608 0 8 4294902401 2082 8 #compareEditions 8 'Compare...' 1 1 0 0 32 706 202 208 98 3 770 800 98 2 834 675 163 834 151 41 4608 770 8 #isEnabled: 98 1 32 4608 770 3136 98 1 8 'Compare...' 4608 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 81 1 0 0 81 0 0 0 156 1 0 0 101 0 0 0] 98 0 1072 0 27 1904 410 3696 98 16 0 416 98 2 8 1140850944 1 5008 0 0 0 7 0 0 0 5008 0 8 4294902359 674 0 0 0 706 202 208 98 2 770 800 98 2 834 11 231 834 241 61 5008 770 3136 98 1 8 'Version comment:' 5008 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 115 0 0 0 125 0 0 0 145 0 0 0] 98 0 1072 0 27 410 4624 98 17 0 416 98 2 8 1140924416 1 5312 0 482 512 0 7 0 0 0 5312 0 8 4294902401 2082 8 #ok 8 '&OK' 1 1 0 0 16 706 202 208 98 3 770 800 98 2 834 451 531 834 171 55 5312 770 4880 98 1 32 5312 770 3136 98 1 8 '&OK' 5312 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 9 1 0 0 54 1 0 0 36 1 0 0] 98 0 1072 0 27 410 4624 98 17 0 416 98 2 8 1140924416 1 5696 0 482 512 0 7 0 0 0 5696 0 8 4294902401 2082 8 #cancel 8 '&Cancel' 1 1 0 0 32 706 202 208 98 2 770 800 98 2 834 651 531 834 171 55 5696 770 3136 98 1 8 '&Cancel' 5696 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 69 1 0 0 9 1 0 0 154 1 0 0 36 1 0 0] 98 0 1072 0 27 1072 0 27 )! !
!StsImportInformation class categoriesFor: #resource_Version_package!public!resources-views! !

!StsPackageEdition methodsFor!

comment: aString 

	comment := self updateComment: aString!

printDateTagOn: aStream 
	aStream nextPutAll: '$date: '.
	self timestamp date printOn: aStream format: 'dd.MM.yyyy'.
	aStream nextPut: $$!

printDeveloperTagOn: aStream 
	aStream
	
		nextPutAll: '$developer: ';
		nextPutAll: self developer;
		nextPut: $$!

printDolphinVersionOn: aStream 
	| versionInfo |
	versionInfo := VMLibrary default versionInfo.
	aStream
		nextPutAll: '$for:';
		space;
		nextPutAll: versionInfo productName;
		space;
		nextPutAll: versionInfo specialBuild;
	
		nextPut: $$!

printIdTagOn: aStream 

	aStream
		nextPutAll: '$id: ';
		nextPutAll: self name;
		space;
		nextPutAll: self versionDescriptor;
		nextPut: $$!

printNameTagOn: aStream 
	aStream

		nextPutAll: '$name: ';

		nextPutAll: self name;
		nextPut: $$!

printTimestampTagOn: aStream 
	aStream nextPutAll: '$timestamp: '.
		self timestamp time printOn: aStream format: 'HH:mm:ss'.
		aStream nextPutAll: ', '.
	self timestamp date printOn: aStream format: 'dd.MM.yyyy'.
	aStream nextPut: $$!

printTimeTagOn: aStream 
	aStream nextPutAll: '$time: '.
	self timestamp time printOn: aStream format: 'HH:mm:ss'.
	aStream nextPut: $$!

printVersionTagOn: aStream 
	
	aStream
		nextPutAll: '$version: ';
		nextPutAll: self versionDescriptor ;
		nextPut: $$!

updateComment: aString 
	| newStream content commentStream char tag tags |
	tags := ##((Dictionary new)
				at: 'date' put: #printDateTagOn:;
				at: 'developer' put: #printDeveloperTagOn:;
				at: 'id' put: #printIdTagOn:;
				at: 'name' put: #printNameTagOn:;
				at: 'timestamp' put: #printTimeTagOn:;
				at: 'time' put: #printTimeTagOn:;
				at: 'version' put: #printVersionTagOn:;
					at: 'for' put: #printDolphinVersionOn:;
				yourself).
	commentStream := aString readStream.
	newStream := WriteStream on: String new.
	[commentStream atEnd] whileFalse: 
			[char := commentStream next.
			char = $$ 
				ifFalse: [newStream nextPut: char]
				ifTrue: 
					[content := commentStream upTo: $$.
					tag := (content subStrings: $:) first asLowercase.
					(tags keys includes: tag) 
						ifTrue: [self perform: (tags at: tag) with: newStream]
						ifFalse: [newStream nextPutAll: content]]].
	^newStream contents! !
!StsPackageEdition categoriesFor: #comment:!public! !
!StsPackageEdition categoriesFor: #printDateTagOn:!public! !
!StsPackageEdition categoriesFor: #printDeveloperTagOn:!public! !
!StsPackageEdition categoriesFor: #printDolphinVersionOn:!public! !
!StsPackageEdition categoriesFor: #printIdTagOn:!public! !
!StsPackageEdition categoriesFor: #printNameTagOn:!public! !
!StsPackageEdition categoriesFor: #printTimestampTagOn:!public! !
!StsPackageEdition categoriesFor: #printTimeTagOn:!public! !
!StsPackageEdition categoriesFor: #printVersionTagOn:!public! !
!StsPackageEdition categoriesFor: #updateComment:!public! !

!StsPackageEditionPrompter methodsFor!

createSchematicWiring
	super createSchematicWiring.
	packages 
		when: #selectionChanged
		send: #onPackageSelected
		to: self.
	#USAdded.
	editions 
		when: #actionPerformed
		send: #ok
		to: self! !
!StsPackageEditionPrompter categoriesFor: #createSchematicWiring!public! !

!StsPackageEditionsBrowserShell methodsFor!

shortCaption
^'Package Editions Browser'! !
!StsPackageEditionsBrowserShell categoriesFor: #shortCaption!public! !

!StsPackageManager methodsFor!

onPackageChanged: aPackage 
	[aPackage updateStsStatus] on: Error do: [:ex | ].
	^super onPackageChanged: aPackage! !
!StsPackageManager categoriesFor: #onPackageChanged:!event handling!private! !

!StsPackageManager class methodsFor!

resetPackageStsStatus
	self current packages do: [:eachPackage | eachPackage resetStsStatus]!

updatePackageStsStatus
	self current  packages do: [:eachPackage | eachPackage updateStsStatus]! !
!StsPackageManager class categoriesFor: #resetPackageStsStatus!public! !
!StsPackageManager class categoriesFor: #updatePackageStsStatus!public! !

!StsPackageProxy methodsFor!

stsStatus
	^#recentInSTS!

updateStsStatus

	^#recentInSTS! !
!StsPackageProxy categoriesFor: #stsStatus!public! !
!StsPackageProxy categoriesFor: #updateStsStatus!public! !

!StsProjectBrowserShell methodsFor!

shortCaption
	^'Project Editions Browser'! !
!StsProjectBrowserShell categoriesFor: #shortCaption!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

StsRepositoryChooser guid: (GUID fromString: '{89569F18-249A-4B43-95ED-FE72255AA120}')!
StsRepositoryChooser comment: ''!
!StsRepositoryChooser categoriesForClass!Development! !
!StsRepositoryChooser methodsFor!

activateRepository
	| repository |
	repository := repositoriesPresenter selectionOrNil.
	repository notNil 
		ifTrue: 
			[| path |
			path := self withoutRepositorySuffix: repository.
			StsManager current startUpOn: path.
			repositoriesPresenter view invalidate.
			PackageSelector allInstances do: [:each | each view invalidate] ]!

addRepository: aString
KnownRepositories add: aString!

createComponents
	super createComponents.
	repositoriesPresenter := self add: ListPresenter new name: 'repositories'!

createSchematicWiring
super createSchematicWiring.
repositoriesPresenter when: #actionPerformed send: #activateRepository to:self!

defaultRepositoryPath
	^StsManager current getRepositoryPath!

deleteRepository
	| repository |
	repository := repositoriesPresenter selectionOrNil.
	repository notNil ifTrue: [self removeRepository: repository ]!

knownRepositories
	KnownRepositories ifNil: [KnownRepositories := ListModel new].
	KnownRepositories isEmpty ifTrue: [KnownRepositories add: (self withoutRepositorySuffix: self defaultRepositoryPath) ].
	^KnownRepositories!

model: aModel
	super model: aModel.
	repositoriesPresenter model: self knownRepositories!

newRepository
	| newLocation |
	newLocation := StsManager current chooseRepository.
	newLocation notNil 
		ifTrue: 
			[StsManager current createRepositoryOn: newLocation.
			self addRepository: newLocation ]!

openRepository
	| newLocation |
	newLocation := StsManager current chooseRepository.
	newLocation notNil 
		ifTrue: 
			[
			self addRepository: newLocation]!

queryCommand: aCommandQuery 
	"Private - Enter details about a potential command for the receiver into the 
	<CommandQuery>."

	| selector |
	selector := aCommandQuery commandSymbol.
	(#(#deleteRepository #activateRepository) includes: selector) 
		ifTrue: 
			[aCommandQuery isEnabled: repositoriesPresenter selectionOrNil ~= self defaultRepositoryPath.
			^true].
	^super queryCommand: aCommandQuery!

removeRepository: aString 
	KnownRepositories remove: aString ifAbsent: []!

showStsPackageEditionBrowser
StsPackageEditionsBrowserShell show.!

showStsProjectEditionBrowser
StsProjectBrowserShell show.!

withoutRepositorySuffix: path 
	^(path endsWith: '\Repository') 
		ifTrue: [path copyFrom: 1 to: path size - '\Repository' size]
		ifFalse: [path]!

withRepositorySuffix: path 
	^(path endsWith: '\Repository') ifTrue: [path] ifFalse: [path , '\Repository']! !
!StsRepositoryChooser categoriesFor: #activateRepository!actions!public! !
!StsRepositoryChooser categoriesFor: #addRepository:!helpers!private! !
!StsRepositoryChooser categoriesFor: #createComponents!public! !
!StsRepositoryChooser categoriesFor: #createSchematicWiring!public! !
!StsRepositoryChooser categoriesFor: #defaultRepositoryPath!constants!private! !
!StsRepositoryChooser categoriesFor: #deleteRepository!actions!public! !
!StsRepositoryChooser categoriesFor: #knownRepositories!accessing!public! !
!StsRepositoryChooser categoriesFor: #model:!public! !
!StsRepositoryChooser categoriesFor: #newRepository!actions!public! !
!StsRepositoryChooser categoriesFor: #openRepository!actions!public! !
!StsRepositoryChooser categoriesFor: #queryCommand:!private! !
!StsRepositoryChooser categoriesFor: #removeRepository:!helpers!private! !
!StsRepositoryChooser categoriesFor: #showStsPackageEditionBrowser!actions!public! !
!StsRepositoryChooser categoriesFor: #showStsProjectEditionBrowser!actions!public! !
!StsRepositoryChooser categoriesFor: #withoutRepositorySuffix:!helpers!private! !
!StsRepositoryChooser categoriesFor: #withRepositorySuffix:!helpers!private! !

!StsRepositoryChooser class methodsFor!

displayOn: aStream 
	aStream nextPutAll: 'Repository Chooser'!

icon
	^Icon fromId: 'SOURCETRACKINGSYSTEM.ICO'!

initialize
	"Private - Initialize the receiver's class variables.
		self initializeAfterLoad.
		self initialize
	"

	(Smalltalk developmentSystem)
		addSystemFolderIcon: self toolsFolderIcon to: Smalltalk developmentSystem sourceTrackingToolsFolder;
		registerTool: self.
	self reuseIfOpen: true!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 328198 ##(Smalltalk.Point)  771 471 551 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409288972 131137 576 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 0 576 658 688 8 4294903753 234 256 98 0 234 256 98 12 60809 1115910 ##(Smalltalk.ToolbarIconButton)  60809 0 576 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #openRepository 8 'Open' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'FileOpen.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 3 514 33 33 1 60811 898 60811 0 576 1 930 8 #deleteRepository 8 'Delete' 1 1 994 0 16 1040 8 'False.ico' 1088 1122 0 16 0 0 0 0 3 514 33 33 1 60813 898 60813 0 576 1 930 8 #activateRepository 8 'Activate' 1 1 994 0 16 1040 8 'SourceTrackingSystem.ico' 1088 1122 0 16 0 0 0 0 3 514 33 33 1 60829 898 60829 0 576 1 930 8 #showStsProjectEditionBrowser 8 'Open STS Project Editions Browser' 1 1 994 0 16 1040 8 'StsProjectEdition.ico' 1088 1122 0 16 0 0 0 0 3 514 33 33 1 60827 898 60827 0 576 1 930 8 #showStsPackageEditionBrowser 8 'Open STS Package Edition Browser' 1 1 994 0 16 1040 8 'StsPackageEdition.ico' 1088 1122 0 16 0 0 0 0 3 514 33 33 1 60807 898 60807 0 576 1 930 8 #newRepository 8 'New' 1 1 994 0 16 1040 8 'DocumentShell.ico' 0 1122 0 16 0 0 0 0 3 514 33 33 1 98 9 1680 912 1168 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 576 3 0 1 1296 1826 0 0 576 3 0 1 1826 0 0 576 3 0 1 1552 1424 234 240 98 12 1264 5 1776 1 1136 3 1392 7 1648 9 1520 11 0 1 0 514 33 33 514 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 514 1 1 514 739 51 576 2050 8 #updateSize 848 576 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 113 1 0 0 25 0 0 0] 98 0 514 193 193 0 27 0 0 0 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1 2256 590662 2 ##(Smalltalk.ListModel)  202 208 848 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  658 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 5 984134 2 ##(Smalltalk.CommandMenuItem)  1 930 1712 8 'New' 9373 1 994 0 16 1040 8 'DocumentShell.ico' 0 0 0 2498 1 930 960 8 'Open' 9375 1 994 0 16 1040 8 'FileOpen.ico' 1088 0 0 2498 1 930 1200 8 'Delete' 1629 1 994 0 16 1040 8 'False.ico' 1088 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 2498 1 930 1328 8 'Activate' 1 1 994 0 16 1040 8 'SourceTrackingSystem.ico' 1088 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 2256 0 8 4294904167 459270 ##(Smalltalk.Message)  8 #displayString 98 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  10 1 2960 8 'doIt' 8 '[ :each | (each , ''\Repository'')= StsManager current getRepositoryPath ifTrue: [(Icon fromId: ''SourceTrackingSystem.ico'') imageIndex ] ifFalse: [nil]]
' 8 #[38 105 17 29 177 31 161 162 132 122 34 35 183 166 106 60 106] 8 '\Repository' 8 #, 8 ##(Smalltalk.StsManager)  8 #current 8 #getRepositoryPath 992 8 'SourceTrackingSystem.ico' 8 #fromId: 8 #imageIndex 2976 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Path' 739 8 #left 2898 2928 2944 8 ##(Smalltalk.SortedCollection)  0 0 2256 0 3 0 2962 0 0 2994 10 1 2960 8 'doIt' 8 '[ :cd | (cd item , ''\Repository'') = StsManager current getRepositoryPath ifTrue: [cd font: cd font beBold ] ]' 8 #[38 105 226 0 30 178 32 162 163 132 123 17 226 6 165 184 106 60 106] 8 #item 8 '\Repository' 3088 3104 3120 3136 8 #font 8 #beBold 8 #font: 3360 7 257 0 8 #report 848 0 131171 0 0 1986 202 208 98 3 2050 2080 98 2 514 1 51 514 739 349 2256 2050 8 #contextMenu: 98 1 2464 2256 2050 8 #text: 98 1 8 'Path' 2256 2178 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 113 1 0 0 199 0 0 0] 98 0 2240 0 27 234 256 98 2 2256 8 'repositories' 0 0 0 0 0 1 0 0 0 0 1 0 0 1986 202 208 98 3 2050 2080 98 2 514 3839 21 514 771 471 416 2050 3712 98 1 8 'STS Repository Chooser' 416 2050 8 #updateMenuBar 848 416 2178 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 0 9 0 0 245 0 0 0] 98 2 576 2256 2240 0 27 )!

show
	StsManager current isConnected ifFalse: [StsManager install].
	StsManager current isConnected ifFalse: [^self].
	super show!

toolsFolderHelpId
	^10902!

uninitialize
	"Private - Un-register the system tools folder icon for the receiver to allow clean
	removal of this class from the system.
		self uninitialize
	Note: As in all Development classes, this method must be stripped to prevent
	it generating an error when this class is removed."

	Smalltalk developmentSystem
		removeSystemFolderIcon: self toolsFolderIcon;
		unregisterTool: self.
	SessionManager current removeEventsTriggeredFor: self ! !
!StsRepositoryChooser class categoriesFor: #displayOn:!displaying!public! !
!StsRepositoryChooser class categoriesFor: #icon!public! !
!StsRepositoryChooser class categoriesFor: #initialize!initializing!private! !
!StsRepositoryChooser class categoriesFor: #resource_Default_view!public!resources-views! !
!StsRepositoryChooser class categoriesFor: #show!public! !
!StsRepositoryChooser class categoriesFor: #toolsFolderHelpId!public! !
!StsRepositoryChooser class categoriesFor: #uninitialize!initializing!must strip!private! !

"Binary Globals"!

