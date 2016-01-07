| package |
package := Package name: 'US ResourceChanging ImageStripper'.
package paxVersion: 1;
	basicComment: '$id: US ResourceChanging ImageStripper 0.030$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 01.08.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

What''s in it?

#applicationIcon
	Set the application Icon

#resourceIcons
	Adds all icons in the dictionary to the deployed application. The key will be the resource name. The value the icon.
	
#resourceCursors
	Adds all icons in the dictionary to the deployed application. The key will be the resource name. The value the icon.
	
#resoureImages
	Adds all images in the dictionary to the deployed application. Images of (sub-)class Bitmap will be stored as RT_BITMAP standard type resource. Images of (sub-)cass OLEPicture will be stored as user defined OLEPICTURE type resource. The key will be the resource name. The value the Image.

#resourceObjects
	Adds all objects in the dictionay to the deployed application. Objects are stored in STB streams as user defined STB type resource. The key will be the resource name. The value the STBed Object.
	You can implement your own serialization/unserialization scheme by providing the following methods:
	
	class>>#defaultResourceType (or #resoureType for instance specific resource types)
	#fromResourceBytes:id:locale:in:usingLocator:type:
	 class>>#defaultResourceClass (If you want to provide your own resource format. In this case you might need to change #asWindowsResource as well. Default uses RT_DATA like scheme)
	#resourceBytes
	
#resourceStrings
	Adds all strings in the Dictionary to the string tables of the deployed application. The key is the string index (not the stringtable resource ID!!). The value the string.
	
What''s missing
	Explorer Compatible icon order
	Reading/Writing Animated Cursors
	Reading/Writing Sounds

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '0.030'.


package classNames
	add: #ResourceChangingImageStripper;
	yourself.

package methodNames
	add: #ImageStripper -> #applicationIcon;
	add: #ImageStripper -> #defaultApplicationIcon;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	add: '..\..\..\Object Arts\Dolphin\ActiveX\Components\Picture\OLE Picture';
	add: '..\US Application Information';
	add: 'US Resource Update';
	yourself).

package setManualPrerequisites: #(
	'US Application Information').

package!

"Class Definitions"!

ImageStripper subclass: #ResourceChangingImageStripper
	instanceVariableNames: 'applicationIcon resourceIcons resourceCursors resourceImages resourceObjects resourceStrings resourceIconsPath resourceCursorsPath resourceImagesPath'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ImageStripper methodsFor!

applicationIcon
	^self defaultApplicationIcon!

defaultApplicationIcon
	^Icon fromId: SessionManager applicationIconId! !
!ImageStripper categoriesFor: #applicationIcon!operations!public! !
!ImageStripper categoriesFor: #defaultApplicationIcon!*-in class package!accessing!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

ResourceChangingImageStripper guid: (GUID fromString: '{246E53EC-354C-4447-9718-723B29341F25}')!
ResourceChangingImageStripper comment: ''!
!ResourceChangingImageStripper categoriesForClass!Unclassified! !
!ResourceChangingImageStripper methodsFor!

allResourceCursors
	| allCursors |
	allCursors := self resourceCursors copy.
	[self resourceCursorsPath notNil 
		ifTrue: 
			[File 
				forAll: '*.cur'
				in: self resourceCursorsPath
				do: 
					[:finddata | 
					allCursors at: (File splitFilenameFrom: finddata path) asUppercase
						put: (Cursor fromFile: finddata path)]]] on: Error do: [:ex | ].
	^allCursors!

allResourceIcons
	| allIcons |
	allIcons := self resourceIcons copy.
[	self resourceIconsPath notNil 
		ifTrue: 
			[File 
				forAll: '*.ico'
				in: self resourceIconsPath
				do: 
					[:finddata | 
					allIcons at: (File splitFilenameFrom: finddata path) asUppercase put: (Icon fromFile: finddata path)]]] on: Error do: [:ex | ].
	^allIcons!

allResourceImages
	| allImages |
	allImages := self resourceImages copy.
	[self resourceImagesPath notNil 
		ifTrue: 
			[File 
				forAll: '*.bmp'
				in: self resourceImagesPath
				do: 
					[:finddata | 
					allImages at: (File splitFilenameFrom: finddata path) asUppercase
						put: (DIBSection fromFile: finddata path)].
			File 
				forAll: '*.jpg'
				in: self resourceImagesPath
				do: 
					[:finddata | 
					allImages at: (File splitFilenameFrom: finddata path) asUppercase
						put: (OLEPicture fromFile: finddata path)]].] on: Error do: [:ex | ].
	^allImages!

applicationIcon
	applicationIcon ifNil: [applicationIcon := super applicationIcon ].
	^applicationIcon!

applicationIcon: anIcon 
	applicationIcon := anIcon!

copyAndUpdateStub: stubFile to: exeFile 
	"Private - Copy over the stub and update it if required. Answer the name of the
	new stub file."

	Notification signal: 'Creating executable stub: ' , exeFile.
	File createDirectoryPath: (File splitPathFrom: exeFile).
	File copy: stubFile to: exeFile.
	File isWriteable: exeFile set: true.
	self isResourceUpdatingRequired 
		ifTrue: 
			[#USToDo.
			self removeResources: exeFile.	"Can be removed if the stubfile uses no icons"
			KernelLibrary default updateResourcesOf: exeFile do: [:hUpdate | self updateStubResources: hUpdate]].
	^exeFile!

defaultCursorResources
	| answer |
	answer := self defaultResourcesFor: Cursor .
	answer removeKey: SessionManager applicationIconId ifAbsent: [].
	^answer!

defaultIconResources
	|  answer |
	answer := self defaultResourcesFor: Icon.
	answer removeKey: SessionManager applicationIconId ifAbsent: [].
	^answer!

defaultImageResources
^self defaultResourcesFor: Bitmap!

defaultResourcesFor: aClass 
	| lib imageNames answer |
	lib := ExternalResourceLibrary open: self checkedStubFilePath.
	imageNames := lib resourceNamesOfType: aClass defaultResourceType.
	answer := LookupTable new.
	imageNames do: [:each | answer at: each put: (aClass fromId: each in: lib)].
	^answer!

defaultStringResources
	| lib stringTables answer |
	lib := ExternalResourceLibrary open: self checkedStubFilePath.
	stringTables := (lib resourceNamesOfType: RT_STRING) 
				collect: [:eachId | StringTableResource fromId: eachId in: lib].
	answer := LookupTable new.
	stringTables do: 
			[:each | 
			0 to: 15
				do: 
					[:index | 
					| bytes |
					bytes := each at: index.
					bytes notEmpty ifTrue: [answer at: (each identifier - 1) * 16 + index put: bytes]]].
	^answer

	!

removeResources: exeFile 
	(WindowsResourceUpdater onFile: exeFile) removeResources!

resourceCursors
	resourceCursors ifNil: [resourceCursors := self defaultCursorResources ].
	^resourceCursors!

resourceCursors: aLookupTable 
	resourceCursors := aLookupTable!

resourceCursorsPath
	resourceCursorsPath isNil ifTrue: [resourceCursorsPath := 'Resources'].
	^(File isRelativePath: resourceCursorsPath) 
		ifTrue: [(PackageRelativeFileLocator package: self runtimeSessionManagerClass  owningPackage) localFileSpecFor: resourceCursorsPath ]
		ifFalse: [resourceCursorsPath]!

resourceCursorsPath: anObject
	resourceCursorsPath := anObject!

resourceIcons
	resourceIcons ifNil: [resourceIcons := self defaultIconResources].
	^resourceIcons!

resourceIcons: aLookupTable 
	resourceIcons := aLookupTable!

resourceIconsPath
	resourceIconsPath isNil ifTrue: [resourceIconsPath := 'Resources'].
	^(File isRelativePath: resourceIconsPath) 
		ifTrue: 
			[(PackageRelativeFileLocator package: self runtimeSessionManagerClass owningPackage) 
				localFileSpecFor: resourceIconsPath]
		ifFalse: [resourceIconsPath]!

resourceIconsPath: anObject
	resourceIconsPath := anObject!

resourceImages
	resourceImages ifNil: [resourceImages := self defaultImageResources].
	^resourceImages!

resourceImages: aLookupTable 
	resourceImages := aLookupTable!

resourceImagesPath
	resourceImagesPath isNil ifTrue: [resourceImagesPath := 'Resources'].
	^(File isRelativePath: resourceImagesPath) 
		ifTrue: 
			[(PackageRelativeFileLocator package: self runtimeSessionManagerClass owningPackage) 
				localFileSpecFor: resourceImagesPath]
		ifFalse: [resourceImagesPath]!

resourceImagesPath: anObject
	resourceImagesPath := anObject!

resourceObjects
	resourceObjects ifNil: [resourceObjects := LookupTable  new].
	^resourceObjects!

resourceObjects: anObject
	resourceObjects := anObject!

resourceStrings
	resourceStrings ifNil: [resourceStrings := self defaultStringResources].
	^resourceStrings!

resourceStrings: aLookupTable 
	resourceStrings := aLookupTable!

updateCursorResource: anExternalHandle 
	| groupResources |
	groupResources := OrderedCollection new.
	self allResourceCursors keysAndValuesDo: 
			[:name :cursor | 
			groupResources add: ((cursor asWindowsResource)
						identifier: name;
						yourself)].
	self updateGroupResources: groupResources in: anExternalHandle!

updateGroupResources: resources in: anExternalHandle 
	| offset |
	offset := 1.
	resources do: 
			[:groupIconResource | 
			groupIconResource
				offset: offset;
				updateResource: anExternalHandle.
			offset := groupIconResource offset]!

updateIconResource: anExternalHandle 
	| groupResources |
	groupResources := OrderedCollection new.
	groupResources add: ((self applicationIcon asWindowsResource)
				identifier: SessionManager applicationIconId;
				yourself).
	self allResourceIcons keysAndValuesDo: 
			[:name :icon | 
			groupResources add: ((icon asWindowsResource)
						identifier: name;
						yourself)].
	self updateGroupResources: groupResources in: anExternalHandle!

updateImageResource: anExternalHandle 
	self allResourceImages keysAndValuesDo: 
			[:name :image | 
			(image asWindowsResource)
				identifier: name;
				updateResource: anExternalHandle]!

updateObjectResource: anExternalHandle 

	self resourceObjects keysAndValuesDo: 
			[:name :object | 
			(object asWindowsResource)
				identifier: name;
				updateResource: anExternalHandle]!

updateStringResource: anExternalHandle 
	| tables |
	tables := LookupTable new.
	self resourceStrings keys do: 
			[:index | 
			| identifier table |
			identifier := index // 16 + 1.
			table := tables at: identifier
						ifAbsentPut: 
							[(StringTableResource empty)
								identifier: identifier;
								yourself].
			table at: index \\ 16 put: (self resourceStrings at: index)].
	tables do: [:each | each updateResource: anExternalHandle]!

updateStubResources: anExternalHandle 
	"Private - Update the resources of the exe/dll stub through the supplied resource update
	handle."

	self updateCursorResource: anExternalHandle;
		updateImageResource: anExternalHandle;
		updateObjectResource: anExternalHandle;
		updateStringResource: anExternalHandle.
	^super updateStubResources: anExternalHandle! !
!ResourceChangingImageStripper categoriesFor: #allResourceCursors!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #allResourceIcons!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #allResourceImages!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #applicationIcon!operations!public! !
!ResourceChangingImageStripper categoriesFor: #applicationIcon:!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #copyAndUpdateStub:to:!*-in class package!operations!private! !
!ResourceChangingImageStripper categoriesFor: #defaultCursorResources!*-in class package!accessing!private! !
!ResourceChangingImageStripper categoriesFor: #defaultIconResources!*-in class package!accessing!private! !
!ResourceChangingImageStripper categoriesFor: #defaultImageResources!accessing!private! !
!ResourceChangingImageStripper categoriesFor: #defaultResourcesFor:!accessing!private! !
!ResourceChangingImageStripper categoriesFor: #defaultStringResources!*-in class package!accessing!private! !
!ResourceChangingImageStripper categoriesFor: #removeResources:!*-in class package!operations!private! !
!ResourceChangingImageStripper categoriesFor: #resourceCursors!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceCursors:!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceCursorsPath!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceCursorsPath:!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceIcons!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceIcons:!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceIconsPath!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceIconsPath:!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceImages!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceImages:!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceImagesPath!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceImagesPath:!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceObjects!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceObjects:!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceStrings!*-in class package!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #resourceStrings:!*-in class package!accessing!public! !
!ResourceChangingImageStripper categoriesFor: #updateCursorResource:!*-in class package!operations!public! !
!ResourceChangingImageStripper categoriesFor: #updateGroupResources:in:!*-in class package!operations!private! !
!ResourceChangingImageStripper categoriesFor: #updateIconResource:!*-in class package!operations!public! !
!ResourceChangingImageStripper categoriesFor: #updateImageResource:!operations!public! !
!ResourceChangingImageStripper categoriesFor: #updateObjectResource:!operations!public! !
!ResourceChangingImageStripper categoriesFor: #updateStringResource:!*-in class package!operations!public! !
!ResourceChangingImageStripper categoriesFor: #updateStubResources:!*-in class package!operations!private! !

!ResourceChangingImageStripper class methodsFor!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect icon: #applicationIcon);
		add: (Aspect dictionary: #resourceIcons);
		add: (Aspect dictionary: #resourceCursors);
		add: (Aspect dictionary: #resourceImages);
		add: (Aspect dictionary: #resourceObjects);
		add: (Aspect dictionary: #resourceStrings);
		add: (Aspect folder: #resourceIconsPath);
		add: (Aspect folder: #resourceCursorsPath);
		add: (Aspect folder: #resourceImagesPath);
			add: (Aspect dictionary: #allResourceIcons);
		add: (Aspect dictionary: #allResourceCursors);
		add: (Aspect dictionary: #allResourceImages);
		yourself!

stbConvertFrom: anSTBClassFormat 
	"Convert from previous version. 
		Version 1: Added a retainPachydermInfo instance var (removed in v3).
		Version 2: Added a rootClasses instance var and the events instance var in model.
		Version 3: Consolidates all flags into one integer flags variable.
		Version 4:  isToGo flag added (the default)"

	^
	[:data | 
	| newInstance ver |

	newInstance := self basicNew.
	1 to: data size do: [:i | newInstance instVarAt: i put: (data at: i)].
	ver := anSTBClassFormat version.
	ver < 5 
		ifTrue: 
			["Change layout to adapt to added resourceCursors inst var"
			| newInstVarPos |
			newInstVarPos := self indexOfInstVar: 'resourceCursors'.
			data keysAndValuesDo: 
					[:eachKey :eachValue | 
					newInstance instVarAt: eachKey + (eachKey >= newInstVarPos ifTrue: [1] ifFalse: [0]) put: eachValue]].
	newInstance]!

stbVersion
	^6! !
!ResourceChangingImageStripper class categoriesFor: #publishedAspectsOfInstances!*-in class package!must strip!public! !
!ResourceChangingImageStripper class categoriesFor: #stbConvertFrom:!binary filing!development!public! !
!ResourceChangingImageStripper class categoriesFor: #stbVersion!public! !

"Binary Globals"!

