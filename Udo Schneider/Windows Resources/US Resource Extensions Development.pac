| package |
package := Package name: 'US Resource Extensions Development'.
package paxVersion: 1;
	basicComment: '$id: US Resource Extensions Development 0.012$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.012'.


package methodNames
	add: #ExternalResourceLibrary -> #chooseGroupIcon;
	add: 'ChoicePrompter class' -> #resource_Icon_choice_prompter;
	add: 'Icon class' -> #choose;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\..\..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US Resource Extensions';
	yourself).

package setManualPrerequisites: #(
	'US Resource Extensions').

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!ChoicePrompter class methodsFor!

resource_Icon_choice_prompter
	"Answer the literal data from which the 'Icon choice prompter' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Icon_choice_prompter)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26738689 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 328198 ##(Smalltalk.Point)  501 701 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 576 0 482 31 0 7 0 0 0 576 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.PushButton)  98 17 0 576 98 2 8 1140924416 1 736 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 736 0 8 4294902717 1180998 4 ##(Smalltalk.CommandDescription)  8 #ok 8 'OK' 1 1 0 0 16 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 514 185 21 514 141 51 736 1010 8 #isEnabled: 98 1 32 736 1010 8 #text: 98 1 8 'OK' 736 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 0 0 0 10 0 0 0 162 0 0 0 35 0 0 0] 98 0 514 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedViewRight -139 1338 1360 8 #fixedParentRight -149 1338 1360 8 #fixedViewBottom -49 1338 1360 8 #fixedParentBottom 1 410 752 98 17 0 576 98 2 8 1140924416 1 1488 0 818 848 0 7 0 0 0 1488 0 8 4294902717 882 8 #cancel 8 'Cancel' 1 1 0 0 32 946 202 208 98 2 1010 1040 98 2 514 335 21 514 141 51 1488 1010 1168 98 1 8 'Cancel' 1488 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 167 0 0 0 10 0 0 0 237 0 0 0 35 0 0 0] 98 0 1280 0 27 1298 1344 -139 1392 1 1424 -49 1456 1 234 256 98 0 590342 ##(Smalltalk.Rectangle)  514 1 1 514 1 1 946 202 208 98 1 1010 1040 98 2 514 15 559 514 475 71 576 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 23 1 0 0 244 0 0 0 58 1 0 0] 98 2 736 1488 1280 0 27 0 0 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140965708 1025 2112 590662 2 ##(Smalltalk.ListModel)  202 208 1872 0 1338 8 ##(Smalltalk.SearchPolicy)  8 #identity 818 848 0 7 0 0 0 2112 0 8 4294902425 459270 ##(Smalltalk.Message)  8 #identifier 98 0 2322 8 #imageIndex 2368 1338 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 514 65 65 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 '' 467 8 #left 0 8 ##(Smalltalk.SortedCollection)  0 0 2112 0 3 0 0 8 #largeIcons 1872 0 133121 0 0 946 202 208 98 1 1010 1040 98 2 514 15 15 514 475 545 2112 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 7 0 0 0 244 0 0 0 23 1 0 0] 98 0 1280 0 27 234 256 98 2 2112 8 'choices' 1890 514 15 15 514 17 15 0 0 0 0 11429 0 0 0 514 501 311 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2117657919 946 202 208 98 3 1010 1040 98 2 514 2799 21 514 521 711 416 1010 1168 98 1 8 'Choose one of:' 416 1010 8 #updateMenuBar 1872 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 123 6 0 0 109 1 0 0] 98 2 2112 576 1280 0 27 )! !
!ChoicePrompter class categoriesFor: #resource_Icon_choice_prompter!public!resources-views! !

!ExternalResourceLibrary methodsFor!

chooseGroupIcon
	^(ChoicePrompter 
		create: 'Icon choice prompter'
		on: nil asValue
		choices: self allGroupIcons
		caption: 'Choose icon') showModal! !
!ExternalResourceLibrary categoriesFor: #chooseGroupIcon!development!public! !

!Icon class methodsFor!

choose
	| filename |
	filename := (FileOpenDialog new)
				fileTypes: (Array 
							with: self filesType
							with: #('Executable files (*.exe)' '*.exe')
							with: #('Dynamic Link Libraries (*.dll)' '*.dll')
							with: FileDialog allFilesType);
				showModal.
	^filename notNil 
		ifTrue: 
			[(File splitExtensionFrom: filename) asLowercase = 'ico' 
				ifTrue: [self fromFile: filename]
				ifFalse: [(ExternalResourceLibrary open: filename) chooseGroupIcon]]
		ifFalse: [nil]! !
!Icon class categoriesFor: #choose!*-not in class package!development!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

