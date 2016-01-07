| package |
package := Package name: 'US RichTextEdit Extensions'.
package paxVersion: 1;
	basicComment: '$id: US RichTextEdit Extensions 0.010$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Example:

presenter := RichTextPresenter 
			showOn: ''This is my E-Mail Address: mailto:Udo.Schneider@homeaddress.de . 
Just drop me a mail or visit me here: http://readthesourceluke.blogspot.com/'' 
					asRichText.
(presenter view)
	autoUrlDetect: true;
	link: true;
	enableMailtoLinks.
presenter 
	when: #linkClicked:
	send: #value:
	to: [:link | MessageBox notify: link caption: ''Link clicked'']
	
	
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.010'.


package methodNames
	add: #Character -> #isValidInEmail;
	add: #CHARFORMAT -> #beLink;
	add: #CHARFORMAT -> #beNotLink;
	add: #CHARFORMAT -> #isLink;
	add: #CHARFORMAT -> #isLink:;
	add: #RichTextEdit -> #autoUrlDetect:;
	add: #RichTextEdit -> #beLink;
	add: #RichTextEdit -> #enableMailtoLinks;
	add: #RichTextEdit -> #getEventMask;
	add: #RichTextEdit -> #link;
	add: #RichTextEdit -> #link:;
	add: #RichTextEdit -> #setEventMask:;
	add: #TextPresenter -> #onLinkClicked:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Rich Text Presenter';
	add: '..\..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Character methodsFor!

isValidInEmail
	^self isAlphaNumeric 
		or: [##(Array 
				with: $@
				with: $.
				with: $-
				with: $_) includes: self]! !
!Character categoriesFor: #isValidInEmail!*-not in class package!public!testing! !

!CHARFORMAT methodsFor!

beLink


	self isLink: true!

beNotLink
	self isLink: false!

isLink
	

	^(self maskAny: CFM_LINK ) and: [self effectsAny: CFE_LINK ]!

isLink: aBoolean 


	self 
		setEffect: CFE_LINK
		mask: CFM_LINK
		set: aBoolean! !
!CHARFORMAT categoriesFor: #beLink!*-not in class package!public! !
!CHARFORMAT categoriesFor: #beNotLink!*-not in class package!public! !
!CHARFORMAT categoriesFor: #isLink!*-in class package!accessing!public! !
!CHARFORMAT categoriesFor: #isLink:!*-in class package!accessing!public! !

!RichTextEdit methodsFor!

autoUrlDetect: aBoolean 
self sendMessage: EM_AUTOURLDETECT wParam: aBoolean asParameter!

beLink
	

	self selectionCharFormat: ((CHARFORMAT new)
				beLink ;
				yourself)!

enableMailtoLinks
	"Fixes all Mail addresses to be clickable Links"

	| start stop rtf address insert combinations |
	rtf := self value rtf.
	combinations := OrderedCollection new.
	start := 1.
	"Find all tokens starting with matching 'mailto:*' and hide the 'mailto:' part. This allows us to get the complete mailto: link when the link is clicked but not have it displayed"
	[(start := rtf indexOfSubCollection: 'mailto:' startingAt: start) > 0] whileTrue: 
			[stop := start + 'mailto:' size.
			[(rtf at: (stop := stop + 1)) isValidInEmail] whileTrue.
			address := rtf copyFrom: start + 7 to: stop - 1.
			combinations add: 'mailto:' , address.
			insert := '{\v mailto:\v0' , address , '}'.
			rtf := rtf 
						copyReplaceFrom: start
						to: stop - 1
						with: insert.
			start := start + insert size].
	self value: (RichText fromRtf: rtf).
	"Enumerate all mailto: links found above and make them linkable"
	combinations inject: 1
		into: 
			[:linkStart :each | 
			| offset |
			offset := self plainText indexOfSubCollection: each startingAt: linkStart.
			self
				selectionRange: (offset to: offset + each size - 1);
				beLink.
			offset + each size].
	self selectionRange: (0 to: 0)!

getEventMask

	^self 
		sendMessage: EM_GETEVENTMASK
		wParam: nil
		lpParam: nil
!

link
	^self getEventMask allMask: ENM_LINK!

link: aBoolean 
	self setEventMask: (self getEventMask mask: ENM_LINK set: aBoolean)!

setEventMask: maskInteger 
	

	^(self 
		sendMessage: EM_SETEVENTMASK
		wParam: nil
		lpParam: maskInteger) ~= 0! !
!RichTextEdit categoriesFor: #autoUrlDetect:!*-not in class package!accessing!public! !
!RichTextEdit categoriesFor: #beLink!*-not in class package!public! !
!RichTextEdit categoriesFor: #enableMailtoLinks!*-not in class package!public! !
!RichTextEdit categoriesFor: #getEventMask!*-in class package!helpers!private! !
!RichTextEdit categoriesFor: #link!*-not in class package!accessing!public! !
!RichTextEdit categoriesFor: #link:!*-not in class package!accessing!public! !
!RichTextEdit categoriesFor: #setEventMask:!*-not in class package!helpers!private! !

!TextPresenter methodsFor!

onLinkClicked: aLinkEvent 
	
	^self view onLinkClicked: aLinkEvent 
	
	! !
!TextPresenter categoriesFor: #onLinkClicked:!*-not in class package!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

