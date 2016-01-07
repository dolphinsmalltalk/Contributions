| package |
package := Package name: 'US IDE Extensions'.
package paxVersion: 1;
	basicComment: '$id: US IDE Extensions 0.035$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 31.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This goodie adds the following functions to the IDE

 * Semi-automatic indenting for sourecode (see http://groups.google.com/group/comp.lang.smalltalk.dolphin/browse_thread/thread/27478673863a28fa/650b8711a3b8d010?lnk=gst&q=onCharAdded%3A#650b8711a3b8d010).

 * PackageBrowser
    - Create ZIP Archives from PackageBrowser. Simply select a package folder and choose "Create ZIP Archive"
    - Add additional columns for STS Status and Package version
       * green: Package in image is the same as the newest in STS
       * blue: Package in image is newer/changes than the newest in STS
       * yellow: Package in image is older than the newset in STS
       * red: Package in image is not in STS
    - Highlight OA packages with OA icon

 * MethodSelector
   - Format method source code as HTML: Select a method in a method selector and choose "HTML Source"
   - Format method source code as LaTeX Select a method in a method selector and choose "LaTeX Source"
   - Highlight loose methods and non-editable methods

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.035'.


package classNames
	add: #USIDEExtensions;
	yourself.

package methodNames
	add: #KernelLibrary -> #debugBreak;
	add: #MethodBrowser -> #showHtmlSource;
	add: #MethodBrowser -> #showLaTeXSource;
	add: #PackageSelector -> #cleanSelectedFolder;
	add: #PackageSelector -> #createProjectEditionFromSelectedFolder;
	add: #PackageSelector -> #createProjectEditionFromSelectedPackages;
	add: #PackageSelector -> #createZipArchive;
	add: #SmalltalkWorkspace -> #autoIndentText;
	add: #SmalltalkWorkspace -> #onCharAdded:;
	add: 'Aspect class' -> #gdiplusImage:;
	add: 'GdiplusImage class' -> #newInstanceAspect:class:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Metagnostic\CU ZipFiles';
	add: '..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Image\Dolphin Image Presenter';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Dialogs\Progress\Dolphin Progress Dialog';
	add: '..\..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	add: '..\..\ITC Gorisek\Source Tracking System';
	add: 'US EstimatingProgressDialog';
	add: 'US STS Extensions';
	yourself).

package setManualPrerequisites: #(
	'US STS Extensions').

package!

"Class Definitions"!

Object subclass: #USIDEExtensions
	instanceVariableNames: 'flags'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Aspect class methodsFor!

gdiplusImage: aGdiplusImageAspectSymbol 
	"Answer an instance of the receiver for aGdiplusImageAspectSymbol.
	The aspect is viewable using an ImagePresenter with a 'Gdiplus view' view
	and editable using a FileDialog"

	^self 
		name: aGdiplusImageAspectSymbol
		presenterBlock: 
			[:p :m | 
			ImagePresenter 
				create: 'Gdiplus view'
				in: p
				on: m]
		editBlock: 
			[:p | 
			| filename |
			filename := (FileOpenDialog new)
						fileTypes: (Array with: FileDialog allFilesType);
						showModal.
			filename notNil ifTrue: [p value: (GdiplusImage fromFile: filename)]]! !
!Aspect class categoriesFor: #gdiplusImage:!public! !

!GdiplusImage class methodsFor!

newInstanceAspect: aSymbol class: aspectClass 
	"Private - Answer a new <Aspect> of the class, aspectClass, and with name, aSymbol, 
    	which is appropriate for representing aspects of the receiver's type."

	^aspectClass gdiplusImage:  aSymbol! !
!GdiplusImage class categoriesFor: #newInstanceAspect:class:!public! !

!KernelLibrary methodsFor!

debugBreak
	"Causes a breakpoint exception to occur in the current process. This allows the calling thread to signal the debugger to handle the exception.

	void WINAPI DebugBreak(void);"
	<stdcall: void DebugBreak>
	^self invalidCall

	! !
!KernelLibrary categoriesFor: #debugBreak!public!win32 functions-file! !

!MethodBrowser methodsFor!

showHtmlSource
	| contents |
	contents := ReadWriteStream on: String new.
	self selections 
		do: 
			[:each | 
			contents
				nextPutAll: each htmlSource;
				cr;
				cr] .SmalltalkWorkspaceDocument 
				show
		setDocumentData: contents contents!

showLaTeXSource
	| contents |
	contents := ReadWriteStream on: String new.
	self selections do: 
			[:each | 
			contents
				nextPutAll: each latexSource ;
				cr;
				cr].
	SmalltalkWorkspaceDocument show setDocumentData: contents contents! !
!MethodBrowser categoriesFor: #showHtmlSource!public! !
!MethodBrowser categoriesFor: #showLaTeXSource!public! !

!PackageSelector methodsFor!

cleanSelectedFolder
	USIDEExtensions current cleanSelectedFolder: self selectedFolder!

createProjectEditionFromSelectedFolder
	USIDEExtensions current createProjectEditionFromFolder: self selectedFolder!

createProjectEditionFromSelectedPackages
	USIDEExtensions current createProjectEditionFromPackages: self selections!

createZipArchive
	USIDEExtensions current createZipArchive:  self selectedFolder! !
!PackageSelector categoriesFor: #cleanSelectedFolder!public! !
!PackageSelector categoriesFor: #createProjectEditionFromSelectedFolder!public! !
!PackageSelector categoriesFor: #createProjectEditionFromSelectedPackages!public! !
!PackageSelector categoriesFor: #createZipArchive!public! !

!SmalltalkWorkspace methodsFor!

autoIndentText
	| text insertionPosition char position crSize numTabs replaceRange |
	"See http://groups.google.com/group/comp.lang.smalltalk.dolphin/browse_thread/thread/27478673863a28fa/650b8711a3b8d010?lnk=gst&q=onCharAdded%3A#650b8711a3b8d010"
	(text := self view plainText) isEmpty ifTrue: [^self].
	crSize := 2.
	insertionPosition := view caretPosition.
	position := insertionPosition - 3 max: 1.
	numTabs := 0.
	char := text at: position.

	"Tab in if we're opening a Block"
	(char == $[ or: [char == $(]) ifTrue: [numTabs := 1].

	"Tab out if we're closing a Block"
	char == $] ifTrue: [numTabs := -1].
	(char == $. and: [(text at: (position - 1 max: 1)) == $]]) ifTrue: [numTabs := -1].

	"Roll back and find out how many tabs the previous line has"
	[position > 0 and: [(char := text at: position) ~~ Character cr]] whileTrue: 
			[char == $[ ifTrue: [numTabs := numTabs max: 0].
			position := position - 1].
	position := position + crSize min: text size.
	[(text at: position ifAbsent: []) == Character tab] whileTrue: 
			[numTabs := numTabs + 1.
			position := position + 1].
	replaceRange := insertionPosition to: insertionPosition - 1.
	view
		selectionRange: replaceRange;
		replaceSelection: ((String new: (numTabs max: 0)) atAllPut: Character tab)!

onCharAdded: aCharacter 
	"See http://groups.google.com/group/comp.lang.smalltalk.dolphin/browse_thread/thread/27478673863a28fa/650b8711a3b8d010?lnk=gst&q=onCharAdded%3A#650b8711a3b8d010"

	"add this line"

	
	aCharacter == Character cr ifTrue: [self autoIndentText].
	(self isAutoCompletionEnabled and: [self isSyntaxColoringEnabled]) ifFalse: [^self].
	self startAutocompleteTimer! !
!SmalltalkWorkspace categoriesFor: #autoIndentText!public! !
!SmalltalkWorkspace categoriesFor: #onCharAdded:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

USIDEExtensions guid: (GUID fromString: '{FAB8A3DC-D421-4995-AD0B-9F0ED9544D37}')!
USIDEExtensions comment: ''!
!USIDEExtensions categoriesForClass!Unclassified! !
!USIDEExtensions methodsFor!

browserClasses
	^(PackageBrowserShell allSubclasses copyWith: PackageBrowserShell) 
		, ClassBrowserAbstract allSubclasses!

cleanSelectedFolder: folder 
	| folderFileLocator packages foundPackagePaths loadedPackagePaths |
	folderFileLocator := FolderRelativeFileLocator basePath: folder.
	foundPackagePaths := Set new.
	File 
		forAll: '*.pac'
		in: folderFileLocator basePath
		do: [:finddata | foundPackagePaths add: finddata path].
	packages := Package manager packages 
				select: [:each | folderFileLocator isRelativePath: each packageFileName].
	loadedPackagePaths := packages asSet collect: [:each | each packageFileName].
	Transcript show: 'Cleaning packages';cr.
	foundPackagePaths - loadedPackagePaths do: [:each | Transcript  show: each; cr.
	File delete: each]!

createProjectEditionFromFolder: folder 
	| folderFileLocator packages |
	folderFileLocator := FolderRelativeFileLocator basePath: folder.
	packages := Package manager packages 
				select: [:each | folderFileLocator isRelativePath: each packageFileName].
	self createProjectEditionFromPackages: packages!

createProjectEditionFromPackages: packages 
	| orderedPackages packagesInLoadOrder stsManager projectName projectEditon stsPackages |
	orderedPackages := packages asOrderedCollection.
	packagesInLoadOrder := OrderedCollection new.
	(EstimatingProgressDialog operation: 
			[:progress | 
			orderedPackages do: 
					[:eachPackage | 
					progress
						value: 90 / orderedPackages size * (orderedPackages indexOf: eachPackage);
						text: eachPackage name.
					packagesInLoadOrder addAllLast: eachPackage prerequisitesInLoadOrderWithoutCleanImagePackages].
			progress
				value: 90;
				text: 'Removing duplicates and clean Image packages'.
			packagesInLoadOrder := packagesInLoadOrder copyWithoutDuplicates 
						reject: [:each | each isCleanImagePackage].
			progress
				value: 100;
				text: 'Getting latest STS editions'.
			stsPackages := packagesInLoadOrder collect: [:each | each latestEdition].
			'completed'])
		caption: 'Gathering Package Prerequisites';
		showModal.
	stsManager := StsManager current.
	projectName := stsManager getAllProjectNames detect: 
					[:eachProjectName | 
					(stsManager getProjectEditionsFor: eachProjectName ) first packageEditions 
						anySatisfy: [:eachPackageEdition | eachPackageEdition name = stsPackages last name]]
				ifNone: [nil].
	projectName := (ChoicePrompter 
				create: 'Extensible choice prompter'
				on: projectName asValue
				choices: stsManager getAllProjectNames
				caption: 'Choose Project') showModal.
	projectName ifNil: [^self].
	(stsManager getAllProjectNames includes: projectName) 
		ifFalse: 
			["Project name unknown - Creating new Project"
			stsManager createNewProjectNamed: projectName createInitialOpenEdition: true]
		ifTrue: 
			["Project name known - create new edition"
			stsManager createNewProjectEditionFrom: (stsManager getProjectEditionsFor: projectName) first
				in: projectName].
	projectEditon := (stsManager getProjectEditionsFor: projectName) first.
	"Remove existing packages from project edition"
	projectEditon packageEditions 
		do: [:each | stsManager removePackageNamed: each name fromProjectEdition: projectEditon].
	"Add packages in load order to project edition"
	stsPackages 
		do: [:eachPackageEdition | stsManager addPackageEdition: eachPackageEdition toProjectEdition: projectEditon].
	stsManager versionProjectEdition: projectEditon!

createZipArchive: selectedFolder 
	| base searchPath zipPath zipfile files zipFilename |
	base := FileLocator imageRelative.
	zipFilename := (((base relativePathTo: selectedFolder) 
				select: [:each | each isAlphaNumeric or: [each = $\]]) 
					collect: [:each | each = $\ ifFalse: [each] ifTrue: [$_]]) 
					, 
						[| stream |
						stream := ReadWriteStream on: String new.
						Date today printOn: stream format: 'yyyMMdd'.
						Time now printOn: stream format: 'HHmm'.
						stream contents] 
								value.
	zipPath := (FileSaveDialog new)
				caption: 'Save Archive';
				fileTypes: (Array with: #('ZIP File (*.zip)' '*.zip') with: FileDialog allFilesType);
				value: (File change: zipFilename extension: 'zip');
				showModal.
	zipPath ifNil: [^self].
	searchPath := selectedFolder.
	files := OrderedCollection new.
	File 
		forAll: '*'
		in: searchPath
		do: [:finddata | finddata isDirectory ifFalse: [files add: finddata copy]].

	zipfile := ZipFile writingFile: zipPath.
	
	[(ProgressDialog operation: 
			[:progress | 
			files keysAndValuesDo: 
					[:fileIndex :finddata | 
					progress
						value: fileIndex / files size * 100;
						text: (base relativePathTo: finddata path).
					zipfile withNewEntry: (base relativePathTo: finddata path)
						do: 
							[:it | 
							| fs |
							fs := FileStream read: finddata path text: false.
							
							[| timestamp |
							timestamp := finddata ftLastWriteTime asLocalTime asSYSTEMTIME asTimeStamp.
							it
								lastModifiedDate: timestamp date;
								lastModifiedTime: timestamp time;
								writeBinary: fs contents] 
									ensure: [fs close]]].
			'completed'])
		caption: 'Creating archive ...';
		showModal] 
			ensure: [zipfile close]!

displayOn: aStream 
	aStream nextPutAll: 'Udo Schneider IDE Extensions'!

extendMethodBrowser: aBrowser 
	| methodBrowser methodListView |
	methodBrowser := (self methodViewOrNilFor: aBrowser) ifNil: [^self].
	methodListView := (methodBrowser isKindOf: ReferenceView) 
				ifFalse: [methodBrowser]
				ifTrue: [methodBrowser referee].
	(methodListView contextMenu)
		addSeparator;
		addCommandDescription: (ClosedCommandDescription 
					command: #showHtmlSource
					description: 'HTML Source'
					queryBlock: 
						[:query | 
						query isEnabled: true.
						true]
					receiver: methodListView presenter parentPresenter); 	addCommandDescription: (ClosedCommandDescription 
					command: #showLaTeXSource
					description: 'LaTeX Source'
					queryBlock: 
						[:query | 
						query isEnabled: true.
						true]
					receiver: methodListView presenter parentPresenter).
	(aBrowser isKindOf: PackageBrowserShell) 
		ifFalse: 
			[methodListView columns first customDrawBlock: 
					[:ctx | 
					ctx item isLoose 
						ifTrue: 
							[ctx
								forecolor: ClassBrowserAbstract looseMethodColor;
								font: ctx font copy beBold].
					(ctx view presenter parentPresenter isEditableMethod: ctx item) 
						ifFalse: [ctx forecolor: (ctx forecolor fadedBy: ClassBrowserAbstract grayedMethodFadeFactor)]]]!

extendPackageFolderMenu: aBrowser 
	| filterMenu filterView |
	filterView := (self filterViewOrNilFor: aBrowser) ifNil: [^self].
	filterMenu := filterView contextMenu.
	filterMenu
		addSeparator;
		addCommandDescription: (ClosedCommandDescription 
					command: #createZipArchive
					description: 'Create ZIP Archive'
					queryBlock: 
						[:query | 
						query isEnabled: true.
						true]
					receiver: filterView presenter parentPresenter);
		addCommandDescription: (ClosedCommandDescription 
					command: #createProjectEditionFromSelectedFolder
					description: 'Create Project Edition'
					queryBlock: 
						[:query | 
						query isEnabled: true.
						true]
					receiver: filterView presenter parentPresenter);
		addCommandDescription: (ClosedCommandDescription 
					command: #cleanSelectedFolder
					description: 'Clean'
					queryBlock: 
						[:query | 
						query isEnabled: true.
						true]
					receiver: filterView presenter parentPresenter)!

extendPackageListMenu: aBrowser 
	| filterView packageListView packageMenu |
	filterView := (self filterViewOrNilFor: aBrowser) ifNil: [^self].
	packageListView := (filterView presenter parentPresenter presenterNamed: 'packages') view.

	
	packageMenu := packageListView contextMenu.
	packageMenu
		addSeparator;
		addCommandDescription: (ClosedCommandDescription 
					command: #createProjectEditionFromSelectedPackages
					description: 'Create Project Edition'
					queryBlock: 
						[:query | 
						query isEnabled: true.
						true]
					receiver: filterView presenter parentPresenter)!

extendPackageListView: aBrowser 
	| filterView packageListView packageMenu |
	filterView := (self filterViewOrNilFor: aBrowser) ifNil: [^self].
	packageListView := (filterView presenter parentPresenter presenterNamed: 'packages') view.
	(packageListView addColumn: (ListViewColumn text: '' width: 16))
		getImageBlock: (Message selector: #stsStatusIconImageIndex);
		getTextBlock: nil;
		sortBlock: [:x :y | x stsStatusSortOrder <= y stsStatusSortOrder];
		yourself.
	(aBrowser isKindOf: PackageBrowserShell) 
		ifTrue: 
			[(packageListView addColumn: (ListViewColumn text: 'Version' width: 65))
				alignment: #right;
				customDrawBlock: [:ctx | ctx forecolor: ctx item stsStatusColor];
				getContentsBlock: (Message selector: #packageVersion);
				sortBlock: (Message selector: #<=);
				yourself.
			"packageListView columnOrder: #(2 3 1 4)"]
		ifFalse: ["packageListView columnOrder: #(2 3 1)"].
	packageListView refreshContents!

filterViewOrNilFor: aBrowser 
^aBrowser view allSubViews detect: [:each | each name = 'filter'] ifNone: [^nil].!

initialize
	"Private - 
		self initialize
	"

	self observeToolEvents.
	Smalltalk developmentSystem registerTool: self!

methodViewOrNilFor: aBrowser 
	^aBrowser view allSubViews detect: [:each | each name = 'methods'] ifNone: [^nil]!

observeBrowserClass: aBrowserClass 
	aBrowserClass 
		when: #viewOpened:
		send: #onBrowserOpened:
		to: self!

observeSystemEvents
	Smalltalk developmentSystem 
		when: #classAdded:
		send: #onClassAdded:
		to: self!

observeToolEvents
	self observeSystemEvents.
	self browserClasses do: [:each | self observeBrowserClass: each]!

onBrowserOpened: aBrowser 
	(self browserClasses includes: aBrowser class) 
		ifTrue: 
			[
			[self
				extendPackageFolderMenu: aBrowser;
				extendPackageListView: aBrowser;
				extendPackageListMenu: aBrowser ] postToInputQueue].
	[self extendMethodBrowser: aBrowser] postToInputQueue!

onClassAdded: aClass 
	(self browserClasses includes: aClass) ifTrue: [self observeBrowserClass: aClass]!

release
	"Private - Remove references to objects that may refer back to the receiver."

	super release.
	Smalltalk developmentSystem unregisterTool: self.
	self browserClasses do: [:each | each removeEventsTriggeredFor: self]! !
!USIDEExtensions categoriesFor: #browserClasses!constants!private! !
!USIDEExtensions categoriesFor: #cleanSelectedFolder:!public! !
!USIDEExtensions categoriesFor: #createProjectEditionFromFolder:!public! !
!USIDEExtensions categoriesFor: #createProjectEditionFromPackages:!public! !
!USIDEExtensions categoriesFor: #createZipArchive:!public! !
!USIDEExtensions categoriesFor: #displayOn:!initializing!public! !
!USIDEExtensions categoriesFor: #extendMethodBrowser:!event handling!private! !
!USIDEExtensions categoriesFor: #extendPackageFolderMenu:!event handling!private! !
!USIDEExtensions categoriesFor: #extendPackageListMenu:!event handling!private! !
!USIDEExtensions categoriesFor: #extendPackageListView:!event handling!private! !
!USIDEExtensions categoriesFor: #filterViewOrNilFor:!event handling!helpers!private! !
!USIDEExtensions categoriesFor: #initialize!initializing!private! !
!USIDEExtensions categoriesFor: #methodViewOrNilFor:!event handling!helpers!private! !
!USIDEExtensions categoriesFor: #observeBrowserClass:!initializing!private! !
!USIDEExtensions categoriesFor: #observeSystemEvents!initializing!private! !
!USIDEExtensions categoriesFor: #observeToolEvents!initializing!private! !
!USIDEExtensions categoriesFor: #onBrowserOpened:!event handling!private! !
!USIDEExtensions categoriesFor: #onClassAdded:!event handling!private! !
!USIDEExtensions categoriesFor: #release!dependency!private! !

!USIDEExtensions class methodsFor!

current
	^Current!

initialize
	"
		self initialize
	"

	
	self uninitialize.
	Current := super new initialize!

new
	"Use #current"

	^self shouldNotImplement!

onPreStripImage
	self uninitialize!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect boolean: #enableKeyBindingsHelp);
		add: (Aspect boolean: #enableClassCommentTemplate);
		yourself!

uninitialize
	"
		self uninitialize
	"

	Current isNil ifTrue: [^self].
	Current release.
	Current := nil! !
!USIDEExtensions class categoriesFor: #current!accessing!public! !
!USIDEExtensions class categoriesFor: #initialize!initializing!public! !
!USIDEExtensions class categoriesFor: #new!instance creation!public! !
!USIDEExtensions class categoriesFor: #onPreStripImage!class hierarchy-removing!private! !
!USIDEExtensions class categoriesFor: #publishedAspectsOfInstances!constants!development!public! !
!USIDEExtensions class categoriesFor: #uninitialize!class hierarchy-removing!public! !

"Binary Globals"!

