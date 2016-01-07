| package |
package := Package name: 'US OpenFlashChart-Dolphin'.
package paxVersion: 1;
	basicComment: '$id: US OpenFlashChart-Dolphin 0.011$, $date: 24.07.2009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '0.011'.


package classNames
	add: #OFCBridge;
	add: #OFCChartModel;
	yourself.

package methodNames
	add: #OFCChart -> #onSelected:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: 'US OpenFlashChart-Core';
	add: 'US ShockwaveFlash Wrapper';
	yourself).

package!

"Class Definitions"!

Object subclass: #OFCChartModel
	instanceVariableNames: 'bridge model'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ActionScriptBridge subclass: #OFCBridge
	instanceVariableNames: 'value isReady'
	classVariableNames: ''
	poolDictionaries: 'CRTConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!OFCChart methodsFor!

onSelected: aSymbol 
	"A click on any of this charts datapoint aSymbol with the point index"

	self ofcOnClick: aSymbol! !
!OFCChart categoriesFor: #onSelected:!accessing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

OFCChartModel guid: (GUID fromString: '{980CC29D-0D83-4FF6-8D2B-D4CB395EF658}')!
OFCChartModel comment: ''!
!OFCChartModel categoriesForClass!Kernel-Objects! !
!OFCChartModel methodsFor!

asBitmap
	^(bridge view)
		ensureVisible;
		printWindow!

model
	^model!

model: anObject 
	model := anObject.
	self refreshContents!

refreshContents
self subclassResponsibility!

setBridge: anActionScriptBridge
bridge := anActionScriptBridge! !
!OFCChartModel categoriesFor: #asBitmap!public! !
!OFCChartModel categoriesFor: #model!public! !
!OFCChartModel categoriesFor: #model:!public! !
!OFCChartModel categoriesFor: #refreshContents!public! !
!OFCChartModel categoriesFor: #setBridge:!private! !

!OFCChartModel class methodsFor!

bridge: anActionScriptBridge
^self new setBridge: anActionScriptBridge; yourself! !
!OFCChartModel class categoriesFor: #bridge:!public! !

OFCBridge guid: (GUID fromString: '{13B1B7DA-61C5-45F7-9CBE-5500A52451B8}')!
OFCBridge comment: ''!
!OFCBridge categoriesForClass!MVP-Views! !
!OFCBridge methodsFor!

defaultValue
	^OFCCanvas render: [:ofcCanvas | ofcCanvas pieChart values: (1 to: 10)]!

initialize
	super initialize.
	isReady := false!

load: aString 
[
self invokeFlash: 'load' with: aString] on: Error do: [:ex | ]!

ofc_ready

	isReady := true!

ofc_ready: id 
	isReady := true.!

open_flash_chart_data
	
	^self value!

open_flash_chart_data:id
	^self value!

value
	value isNil ifTrue: [^self defaultValue].
	^value!

value: aString 
	value := aString.
	isReady ifTrue: [  self load: aString]! !
!OFCBridge categoriesFor: #defaultValue!public! !
!OFCBridge categoriesFor: #initialize!initialize/release!private! !
!OFCBridge categoriesFor: #load:!accessing!public! !
!OFCBridge categoriesFor: #ofc_ready!initialize/release!must not strip!public! !
!OFCBridge categoriesFor: #ofc_ready:!initialize/release!must not strip!public! !
!OFCBridge categoriesFor: #open_flash_chart_data!must not strip!public! !
!OFCBridge categoriesFor: #open_flash_chart_data:!must not strip!public! !
!OFCBridge categoriesFor: #value!accessing!public! !
!OFCBridge categoriesFor: #value:!accessing!public! !

"Binary Globals"!

