| package |
package := Package name: 'US OpenFlashChart-Core'.
package paxVersion: 1;
	basicComment: '$id: US OpenFlashChart-Core 42_us0.011$, $date: 24.07.2009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '42_us0.011'.


package classNames
	add: #OFC3dBarChart;
	add: #OFCAreaChart;
	add: #OFCAxis;
	add: #OFCAxisLabels;
	add: #OFCBackground;
	add: #OFCBarChart;
	add: #OFCBarValue;
	add: #OFCBrush;
	add: #OFCCanvas;
	add: #OFCChart;
	add: #OFCChartDecoration;
	add: #OFCChartElement;
	add: #OFCChartValue;
	add: #OFCCylinderChart;
	add: #OFCDottedLineChart;
	add: #OFCEuclidianAxis;
	add: #OFCGlassBarChart;
	add: #OFCHollowAreaChart;
	add: #OFCHollowLineChart;
	add: #OFCHorizontalBarChart;
	add: #OFCLegendLabel;
	add: #OFCLineAreaChart;
	add: #OFCLineChart;
	add: #OFCLineScatterChart;
	add: #OFCLineValue;
	add: #OFCNamedChart;
	add: #OFCObject;
	add: #OFCOutlinedBarChart;
	add: #OFCPieChart;
	add: #OFCPieValue;
	add: #OFCRadarAxis;
	add: #OFCRadarValue;
	add: #OFCRoundGlassBarChart;
	add: #OFCScatterChart;
	add: #OFCShape;
	add: #OFCSketchBarChart;
	add: #OFCSpokeLabels;
	add: #OFCStackedBarChart;
	add: #OFCStackedBarDataset;
	add: #OFCStackedBarValue;
	add: #OFCStyledText;
	add: #OFCTitle;
	add: #OFCTooltip;
	add: #OFCTooltipStyle;
	add: #OFCXAxis;
	add: #OFCXLegend;
	add: #OFCYAxis;
	add: #OFCYAxisRight;
	add: #OFCYLegend;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Tooltips\Dolphin Tooltips';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'US Collection Extensions';
	add: 'US JSON';
	yourself).

package setManualPrerequisites: #(
	'US Collection Extensions'
	'US JSON').

package!

"Class Definitions"!

Object subclass: #OFCCanvas
	instanceVariableNames: 'data'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #OFCObject
	instanceVariableNames: 'properties'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCObject subclass: #OFCBrush
	instanceVariableNames: 'ofcCanvas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCObject subclass: #OFCChartValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCObject subclass: #OFCLegendLabel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCObject subclass: #OFCStackedBarDataset
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCObject subclass: #OFCTooltip
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBrush subclass: #OFCAxisLabels
	instanceVariableNames: 'axis'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBrush subclass: #OFCChartDecoration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBrush subclass: #OFCChartElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCAxisLabels subclass: #OFCSpokeLabels
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartDecoration subclass: #OFCAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartDecoration subclass: #OFCBackground
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartDecoration subclass: #OFCStyledText
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartDecoration subclass: #OFCTooltipStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCAxis subclass: #OFCEuclidianAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCAxis subclass: #OFCRadarAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCEuclidianAxis subclass: #OFCXAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCEuclidianAxis subclass: #OFCYAxis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCYAxis subclass: #OFCYAxisRight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCStyledText subclass: #OFCTitle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCStyledText subclass: #OFCXLegend
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCStyledText subclass: #OFCYLegend
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartElement subclass: #OFCChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartElement subclass: #OFCShape
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChart subclass: #OFCNamedChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChart subclass: #OFCPieChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCNamedChart subclass: #OFCAreaChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCNamedChart subclass: #OFCBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCNamedChart subclass: #OFCLineChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCNamedChart subclass: #OFCScatterChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCNamedChart subclass: #OFCStackedBarChart
	instanceVariableNames: 'datasets'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCAreaChart subclass: #OFCHollowAreaChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCAreaChart subclass: #OFCLineAreaChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBarChart subclass: #OFC3dBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBarChart subclass: #OFCCylinderChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBarChart subclass: #OFCGlassBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBarChart subclass: #OFCHorizontalBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBarChart subclass: #OFCOutlinedBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCGlassBarChart subclass: #OFCRoundGlassBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCOutlinedBarChart subclass: #OFCSketchBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCLineChart subclass: #OFCDottedLineChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCDottedLineChart subclass: #OFCHollowLineChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCScatterChart subclass: #OFCLineScatterChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartValue subclass: #OFCBarValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartValue subclass: #OFCLineValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartValue subclass: #OFCPieValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartValue subclass: #OFCRadarValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartValue subclass: #OFCStackedBarValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

OFCCanvas guid: (GUID fromString: '{CE229B69-E9AD-4FC1-BC89-C7009A942D37}')!
OFCCanvas comment: ''!
!OFCCanvas categoriesForClass!Unclassified! !
!OFCCanvas methodsFor!

arrayAt: aByteSymbol 
	^data at: aByteSymbol ifAbsentPut: [OrderedCollection new]!

at: key put: value 
	^ data 
		at: key
		put: value!

background
	^ self newBrush: OFCBackground!

background: aBlock 
	^ self background
		with: aBlock;
		yourself!

backgroundColor: aColor 
	"shortcut for
		ofcCanvas background color: aColor
		
	This was done to have access to a seperate background object in case
	more than just a simple color will be supported in the future."
	self background color: aColor!

barChart
	^ self newBrush: OFCBarChart!

barChart: aBlock 
	^ self barChart
		with: aBlock;
		yourself!

cylinderChart
	^ self newBrush: OFCCylinderChart!

cylinderChart: aBlock 
	^ self cylinderChart
		with: aBlock;
		yourself!

data
	^ data!

dictionaryAt: aByteSymbol 
	^data at: aByteSymbol ifAbsentPut: [Dictionary new]!

dottedLineChart
	^ self newBrush: OFCDottedLineChart!

dottedLineChart: aBlock 
	^ self dottedLineChart
		with: aBlock;
		yourself!

glassBarChart
	^ self newBrush: OFCGlassBarChart!

glassBarChart: aBlock 
	^ self glassBarChart
		with: aBlock;
		yourself!

glassRoundBarChart
	^ self newBrush: OFCRoundGlassBarChart!

glassRoundBarChart: aBlock 
	^ self glassRoundBarChart
		with: aBlock;
		yourself!

hollowAreaChart
	^ self newBrush: OFCHollowAreaChart!

hollowAreaChart: aBlock 
	^ self hollowAreaChart
		with: aBlock;
		yourself!

hollowLineChart
	^ self newBrush: OFCHollowLineChart!

hollowLineChart: aBlock 
	^ self hollowLineChart
		with: aBlock;
		yourself!

horizontalBarChart
	^ self newBrush: OFCHorizontalBarChart!

horizontalBarChart: aBlock 
	^ self horizontalBarChart
		with: aBlock;
		yourself!

initialize
	super initialize.
	data := Dictionary new!

jsonSaveOn: aJSONFiler 
	^data jsonSaveOn: aJSONFiler!

lineAreaChart
	^ self newBrush: OFCLineAreaChart!

lineAreaChart: aBlock 
	^ self lineAreaChart
		with: aBlock;
		yourself!

lineChart
	^ self newBrush: OFCLineChart!

lineChart: aBlock 
	^ self lineChart
		with: aBlock;
		yourself!

lineScatterChart
	^ self newBrush: OFCLineScatterChart!

lineScatterChart: aBlock 
	^ self lineScatterChart
		with: aBlock;
		yourself!

newBrush: anOFCObjectClass 
	^ anOFCObjectClass new
		setOFCCanvas: self;
		yourself!

outlinedBarChart
	^ self newBrush: OFCOutlinedBarChart!

outlinedBarChart: aBlock 
	^ self outlinedBarChart
		with: aBlock;
		yourself!

pieChart
	^ self newBrush: OFCPieChart!

pieChart: aBlock 
	^ self pieChart
		with: aBlock;
		yourself!

radarAxis
	^ self newBrush: OFCRadarAxis!

radarAxis: aBlock 
	^ self radarAxis
		with: aBlock;
		yourself!

render: aBlock 
	"Render the chart definition aBlock on self"

	aBlock value: self.
	^self jsonString asString!

scatterChart
	^ self newBrush: OFCScatterChart!

scatterChart: aBlock 
	^ self scatterChart
		with: aBlock;
		yourself!

shape
	^ self newBrush: OFCShape !

shape: aBlock 
	^ self shape
		with: aBlock;
		yourself!

sketchBarChart
	^ self newBrush: OFCSketchBarChart!

sketchBarChart: aBlock 
	^ self sketchBarChart
		with: aBlock;
		yourself!

stackedBarChart
	^ self newBrush: OFCStackedBarChart!

stackedBarChart: aBlock 
	^ self stackedBarChart
		with: aBlock;
		yourself!

threeDBarChart
	^ self newBrush: OFC3dBarChart!

threeDBarChart: aBlock 
	^ self threeDBarChart
		with: aBlock;
		yourself!

title
	^ self newBrush: OFCTitle!

title: aBlock 
	^ self title
		with: aBlock;
		yourself!

tooltipStyle
	^ self newBrush: OFCTooltipStyle!

tooltipStyle: aBlock 
	^ self tooltipStyle
		with: aBlock;
		yourself!

xAxis
	^ self newBrush: OFCXAxis!

xAxis: aBlock 
	^ self xAxis
		with: aBlock;
		yourself!

xLegend
	^ self newBrush: OFCXLegend!

xLegend: aBlock 
	^ self xLegend
		with: aBlock;
		yourself!

yAxis
	^ self newBrush: OFCYAxis!

yAxis: aBlock 
	^ self yAxis
		with: aBlock;
		yourself!

yAxisRight
	^ self newBrush: OFCYAxisRight!

yAxisRight: aBlock 
	^ self yAxisRight
		with: aBlock;
		yourself!

yLegend
	^ self newBrush: OFCYLegend!

yLegend: aBlock 
	^ self yLegend
		with: aBlock;
		yourself! !
!OFCCanvas categoriesFor: #arrayAt:!accessing-helpers!public! !
!OFCCanvas categoriesFor: #at:put:!accessing!public! !
!OFCCanvas categoriesFor: #background!elements!public! !
!OFCCanvas categoriesFor: #background:!elements!public! !
!OFCCanvas categoriesFor: #backgroundColor:!elements!public! !
!OFCCanvas categoriesFor: #barChart!charts-bar!public! !
!OFCCanvas categoriesFor: #barChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #cylinderChart!charts-bar!public! !
!OFCCanvas categoriesFor: #cylinderChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #data!accessing!public! !
!OFCCanvas categoriesFor: #dictionaryAt:!accessing-helpers!public! !
!OFCCanvas categoriesFor: #dottedLineChart!charts-line!public! !
!OFCCanvas categoriesFor: #dottedLineChart:!charts-line!public! !
!OFCCanvas categoriesFor: #glassBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #glassBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #glassRoundBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #glassRoundBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #hollowAreaChart!charts-area!public! !
!OFCCanvas categoriesFor: #hollowAreaChart:!charts-area!public! !
!OFCCanvas categoriesFor: #hollowLineChart!charts-line!public! !
!OFCCanvas categoriesFor: #hollowLineChart:!charts-line!public! !
!OFCCanvas categoriesFor: #horizontalBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #horizontalBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #initialize!initialization!public! !
!OFCCanvas categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCCanvas categoriesFor: #lineAreaChart!charts-area!public! !
!OFCCanvas categoriesFor: #lineAreaChart:!charts-area!public! !
!OFCCanvas categoriesFor: #lineChart!charts-line!public! !
!OFCCanvas categoriesFor: #lineChart:!charts-line!public! !
!OFCCanvas categoriesFor: #lineScatterChart!charts-scatter!public! !
!OFCCanvas categoriesFor: #lineScatterChart:!charts-scatter!public! !
!OFCCanvas categoriesFor: #newBrush:!helpers!public! !
!OFCCanvas categoriesFor: #outlinedBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #outlinedBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #pieChart!charts-pie!public! !
!OFCCanvas categoriesFor: #pieChart:!charts-pie!public! !
!OFCCanvas categoriesFor: #radarAxis!elements!public! !
!OFCCanvas categoriesFor: #radarAxis:!elements!public! !
!OFCCanvas categoriesFor: #render:!public! !
!OFCCanvas categoriesFor: #scatterChart!charts-scatter!public! !
!OFCCanvas categoriesFor: #scatterChart:!charts-scatter!public! !
!OFCCanvas categoriesFor: #shape!charts-shape!public! !
!OFCCanvas categoriesFor: #shape:!charts-shape!public! !
!OFCCanvas categoriesFor: #sketchBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #sketchBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #stackedBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #stackedBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #threeDBarChart!charts-bar!public! !
!OFCCanvas categoriesFor: #threeDBarChart:!charts-bar!public! !
!OFCCanvas categoriesFor: #title!elements!public! !
!OFCCanvas categoriesFor: #title:!elements!public! !
!OFCCanvas categoriesFor: #tooltipStyle!elements!public! !
!OFCCanvas categoriesFor: #tooltipStyle:!elements!public! !
!OFCCanvas categoriesFor: #xAxis!elements!public! !
!OFCCanvas categoriesFor: #xAxis:!elements!public! !
!OFCCanvas categoriesFor: #xLegend!elements!public! !
!OFCCanvas categoriesFor: #xLegend:!elements!public! !
!OFCCanvas categoriesFor: #yAxis!elements!public! !
!OFCCanvas categoriesFor: #yAxis:!elements!public! !
!OFCCanvas categoriesFor: #yAxisRight!elements!public! !
!OFCCanvas categoriesFor: #yAxisRight:!elements!public! !
!OFCCanvas categoriesFor: #yLegend!elements!public! !
!OFCCanvas categoriesFor: #yLegend:!elements!public! !

!OFCCanvas class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCCanvas.ico'!

new
^super new initialize!

render: aBlock 
	^ self new render: aBlock! !
!OFCCanvas class categoriesFor: #icon!public! !
!OFCCanvas class categoriesFor: #new!public! !
!OFCCanvas class categoriesFor: #render:!instance creation!public! !

OFCObject guid: (GUID fromString: '{A65EB67C-18BB-4105-ACC7-015D372E9046}')!
OFCObject comment: ''!
!OFCObject categoriesForClass!Unclassified! !
!OFCObject methodsFor!

properties
	self subclassResponsibility!

propertiesAt: key put: value 
	^ self properties 
		at: key
		put: value!

propertiesAtOrNil: key 
	^ self properties 
		at: key
		ifAbsent: [ nil ]!

with: aBlock 
	aBlock value: self! !
!OFCObject categoriesFor: #properties!accessing!public! !
!OFCObject categoriesFor: #propertiesAt:put:!accessing!public! !
!OFCObject categoriesFor: #propertiesAtOrNil:!accessing!public! !
!OFCObject categoriesFor: #with:!public! !

!OFCObject class methodsFor!

new
^super new initialize! !
!OFCObject class categoriesFor: #new!public! !

OFCBrush guid: (GUID fromString: '{3BD16CF5-0551-4D09-937A-2180EEDDD1EE}')!
OFCBrush comment: ''!
!OFCBrush categoriesForClass!Unclassified! !
!OFCBrush methodsFor!

setOFCCanvas: anOFCCanvas 
	ofcCanvas := anOFCCanvas! !
!OFCBrush categoriesFor: #setOFCCanvas:!initialize-release!public! !

!OFCBrush class methodsFor!

icon
^Icon fromId: 'BRUSH.ICO'! !
!OFCBrush class categoriesFor: #icon!public! !

OFCChartValue guid: (GUID fromString: '{2D2DB168-D81C-4721-BA2A-29488FF03DC0}')!
OFCChartValue comment: ''!
!OFCChartValue categoriesForClass!Unclassified! !
!OFCChartValue methodsFor!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

fillColor: aColorOrHTMLHexString 
	self color: aColorOrHTMLHexString!

jsonSaveOn: aStream 
	self properties jsonSaveOn: aStream!

properties
	properties isNil ifTrue: [properties := Dictionary new].
	^properties!

tooltip
	^ self 
		propertiesAt: #tip
		put: OFCTooltip new!

tooltipTitle: title body: body 
	self tooltip 
		title: title
		body: body!

value: anInteger
	self subclassResponsibility! !
!OFCChartValue categoriesFor: #color:!accessing!public! !
!OFCChartValue categoriesFor: #fillColor:!accessing!public! !
!OFCChartValue categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCChartValue categoriesFor: #properties!accessing!public! !
!OFCChartValue categoriesFor: #tooltip!accessing!public! !
!OFCChartValue categoriesFor: #tooltipTitle:body:!accessing!public! !
!OFCChartValue categoriesFor: #value:!accessing!public! !

!OFCChartValue class methodsFor!

icon ^ValueModel icon! !
!OFCChartValue class categoriesFor: #icon!public! !

OFCLegendLabel guid: (GUID fromString: '{F077972F-7BFF-430B-AFE6-78EA4D91AD70}')!
OFCLegendLabel comment: ''!
!OFCLegendLabel categoriesForClass!Unclassified! !
!OFCLegendLabel methodsFor!

beInvisible
	^ self visible: false!

beVisible
	^ self visible: true!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

isVisible: aBoolean 
	self 
		propertiesAt: #visible
		put: aBoolean!

jsonSaveOn: aStream 
	self properties jsonSaveOn: aStream!

properties
	properties isNil ifTrue: [properties := Dictionary new].
	^properties!

rotate: aNumber 
	self 
		propertiesAt: #rotate
		put: aNumber!

size: aNumber 
	self 
		propertiesAt: #size
		put: aNumber!

text: aString 
	self 
		propertiesAt: #text
		put: aString! !
!OFCLegendLabel categoriesFor: #beInvisible!accessing!public! !
!OFCLegendLabel categoriesFor: #beVisible!accessing!public! !
!OFCLegendLabel categoriesFor: #color:!accessing!public! !
!OFCLegendLabel categoriesFor: #isVisible:!accessing!public! !
!OFCLegendLabel categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCLegendLabel categoriesFor: #properties!accessing!public! !
!OFCLegendLabel categoriesFor: #rotate:!accessing!public! !
!OFCLegendLabel categoriesFor: #size:!accessing!public! !
!OFCLegendLabel categoriesFor: #text:!accessing!public! !

!OFCLegendLabel class methodsFor!

icon
^String icon! !
!OFCLegendLabel class categoriesFor: #icon!public! !

OFCStackedBarDataset guid: (GUID fromString: '{B5DD638B-33E6-4892-A0FE-267CB97BD613}')!
OFCStackedBarDataset comment: ''!
!OFCStackedBarDataset categoriesForClass!Unclassified! !
!OFCStackedBarDataset methodsFor!

color
	^ self propertiesAtOrNil: #colour!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

key

	^ Dictionary new
		at: #colour
			put: self color;
		at: #'font-size'
			put: self size;
		at: #text
			put: self text;
		yourself!

properties
	properties isNil ifTrue: [properties := Dictionary new].
	^properties!

size
	^ self propertiesAtOrNil: #'font-size'!

size: aNumber 
	self 
		propertiesAt: #'font-size'
		put: aNumber!

text
	^ self propertiesAtOrNil: #text!

text: aString
	self propertiesAt: #text put: aString!

values
	^ self propertiesAtOrNil: #values!

values: anArray 
	self 
		propertiesAt: #values
		put: anArray! !
!OFCStackedBarDataset categoriesFor: #color!accessing!public! !
!OFCStackedBarDataset categoriesFor: #color:!accessing!public! !
!OFCStackedBarDataset categoriesFor: #key!accessing!public! !
!OFCStackedBarDataset categoriesFor: #properties!accessing!public! !
!OFCStackedBarDataset categoriesFor: #size!accessing!public! !
!OFCStackedBarDataset categoriesFor: #size:!accessing!public! !
!OFCStackedBarDataset categoriesFor: #text!accessing!public! !
!OFCStackedBarDataset categoriesFor: #text:!accessing!public! !
!OFCStackedBarDataset categoriesFor: #values!accessing!public! !
!OFCStackedBarDataset categoriesFor: #values:!accessing!public! !

!OFCStackedBarDataset class methodsFor!

icon ^ValueModel icon! !
!OFCStackedBarDataset class categoriesFor: #icon!public! !

OFCTooltip guid: (GUID fromString: '{CC8E7D70-9776-4184-AA16-9F8D6FABD5E6}')!
OFCTooltip comment: ''!
!OFCTooltip categoriesForClass!Unclassified! !
!OFCTooltip methodsFor!

body
	^ self propertiesAtOrNil: #body!

body: aString
	self propertiesAt: #body put: aString!

bodyLines
	| input |
	input := self body readStream.
	^ Array
		streamContents: [:output | [input atEnd]
				whileFalse: [output
						nextPut: (input upTo: Character cr).
					input peek = Character lf
						ifTrue: [input next]]]!

jsonSaveOn: aStream 
	| tooltipStream |
	tooltipStream := ReadWriteStream on: String new.
	self title notNil 
		ifTrue: [(tooltipStream
				nextPutAll: self title).
				tooltipStream nextPut: Character lf]
		ifFalse: [tooltipStream nextPut: Character lf].
	self body notNil 
		ifTrue: 
			[self bodyLines do: [:each | tooltipStream nextPutAll: each]
				separatedBy: [tooltipStream nextPut: Character lf]]
		ifFalse: [tooltipStream nextPutAll: ''].
	tooltipStream contents jsonSaveOn: aStream!

properties
	properties isNil ifTrue: [properties := Dictionary new].
	^properties!

text: aString 
	^ self body: aString!

title
	^ self propertiesAtOrNil: #title!

title: aString
	self propertiesAt: #title put: aString!

title: title body: body 
	self title: title.
	self body: body! !
!OFCTooltip categoriesFor: #body!accessing!public! !
!OFCTooltip categoriesFor: #body:!accessing!public! !
!OFCTooltip categoriesFor: #bodyLines!JSON-writing!public! !
!OFCTooltip categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCTooltip categoriesFor: #properties!accessing!public! !
!OFCTooltip categoriesFor: #text:!accessing!public! !
!OFCTooltip categoriesFor: #title!accessing!public! !
!OFCTooltip categoriesFor: #title:!accessing!public! !
!OFCTooltip categoriesFor: #title:body:!accessing!public! !

!OFCTooltip class methodsFor!

icon
^Tooltip icon! !
!OFCTooltip class categoriesFor: #icon!public! !

OFCAxisLabels guid: (GUID fromString: '{34895370-1296-4677-8863-CB33B9C8B5DA}')!
OFCAxisLabels comment: ''!
!OFCAxisLabels categoriesForClass!Unclassified! !
!OFCAxisLabels methodsFor!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

properties
	properties isNil
		ifTrue: [properties := axis properties
						at: #labels
						ifAbsentPut: [Dictionary new]].
	^ properties!

rotate: aNumber 
	self propertiesAt: #rotate put: aNumber!

setOFCCanvas: anOFCCanvas axis: anOFCAxis 
	self setOFCCanvas: anOFCCanvas.
	axis := anOFCAxis!

size: anInteger
	self propertiesAt: #size put: anInteger!

steps: anInteger
	self propertiesAt: #steps put: anInteger!

values: anArray 
	self propertiesAt: #labels put: anArray! !
!OFCAxisLabels categoriesFor: #color:!accessing!public! !
!OFCAxisLabels categoriesFor: #properties!accessing!public! !
!OFCAxisLabels categoriesFor: #rotate:!accessing!public! !
!OFCAxisLabels categoriesFor: #setOFCCanvas:axis:!initialize-release!public! !
!OFCAxisLabels categoriesFor: #size:!accessing!public! !
!OFCAxisLabels categoriesFor: #steps:!accessing!public! !
!OFCAxisLabels categoriesFor: #values:!accessing!public! !

!OFCAxisLabels class methodsFor!

icon
^String icon! !
!OFCAxisLabels class categoriesFor: #icon!public! !

OFCChartDecoration guid: (GUID fromString: '{C281DFFD-2A55-4405-865F-67F66E43860B}')!
OFCChartDecoration comment: ''!
!OFCChartDecoration categoriesForClass!Unclassified! !
!OFCChartDecoration methodsFor!

elementType
	self subclassResponsibility!

properties
	properties isNil ifTrue: [ properties := ofcCanvas dictionaryAt: self elementType ].
	^ properties! !
!OFCChartDecoration categoriesFor: #elementType!constants!public! !
!OFCChartDecoration categoriesFor: #properties!accessing!public! !

OFCChartElement guid: (GUID fromString: '{A3E6DE0E-6FB5-43C6-A3C7-67DF26D4C39D}')!
OFCChartElement comment: ''!
!OFCChartElement categoriesForClass!Unclassified! !
!OFCChartElement methodsFor!

elementType
	self subclassResponsibility!

properties
	properties isNil ifTrue: 
		[ | elements |
		(ofcCanvas arrayAt: #elements) add: (properties := Dictionary new).
		properties 
			at: #type
			put: self elementType ].
	^ properties!

values: anArray
	self propertiesAt: #values put: anArray! !
!OFCChartElement categoriesFor: #elementType!constants!public! !
!OFCChartElement categoriesFor: #properties!accessing!public! !
!OFCChartElement categoriesFor: #values:!accessing!public! !

OFCSpokeLabels guid: (GUID fromString: '{AC20D609-283A-413A-89FA-7CA8E9FE71A9}')!
OFCSpokeLabels comment: ''!
!OFCSpokeLabels categoriesForClass!Unclassified! !
!OFCSpokeLabels methodsFor!

properties
	properties isNil
		ifTrue: [properties := axis properties
						at: #'spoke-labels'
						ifAbsentPut: [Dictionary new]].
	^ properties! !
!OFCSpokeLabels categoriesFor: #properties!accessing!public! !

OFCAxis guid: (GUID fromString: '{20A51DF8-7903-44A6-ABEF-E68BD79B5CBA}')!
OFCAxis comment: ''!
!OFCAxis categoriesForClass!Unclassified! !
!OFCAxis methodsFor!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

gridColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #'grid-colour'
		put: aColorOrHTMLHexString!

labels
	^ OFCAxisLabels new
		setOFCCanvas: ofcCanvas
			axis: self;
		yourself!

labels: aBlock 
	^ self labels
		with: aBlock;
		yourself!

max: aNumber 
	self 
		propertiesAt: #max
		put: aNumber!

min: aNumber 
	self 
		propertiesAt: #min
		put: aNumber!

steps: aNumber 
	self 
		propertiesAt: #steps
		put: aNumber! !
!OFCAxis categoriesFor: #color:!accessing!public! !
!OFCAxis categoriesFor: #gridColor:!accessing!public! !
!OFCAxis categoriesFor: #labels!accessing!public! !
!OFCAxis categoriesFor: #labels:!accessing!public! !
!OFCAxis categoriesFor: #max:!accessing!public! !
!OFCAxis categoriesFor: #min:!accessing!public! !
!OFCAxis categoriesFor: #steps:!accessing!public! !

!OFCAxis class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCAxis.ico'! !
!OFCAxis class categoriesFor: #icon!public! !

OFCBackground guid: (GUID fromString: '{B91AEA67-934F-4C98-B52B-78084C90C1E3}')!
OFCBackground comment: ''!
!OFCBackground categoriesForClass!Unclassified! !
!OFCBackground methodsFor!

color: aColorOrHTMLHexString 
	ofcCanvas 
		at: #'bg_colour'
		put: aColorOrHTMLHexString! !
!OFCBackground categoriesFor: #color:!accessing!public! !

OFCStyledText guid: (GUID fromString: '{18590008-9393-4C91-B8AE-785175839105}')!
OFCStyledText comment: ''!
!OFCStyledText categoriesForClass!Unclassified! !
!OFCStyledText methodsFor!

style: aCssString 
	self 
		propertiesAt: #style
		put: aCssString!

text: aString 
	self 
		propertiesAt: #text
		put: aString! !
!OFCStyledText categoriesFor: #style:!accessing!public! !
!OFCStyledText categoriesFor: #text:!accessing!public! !

!OFCStyledText class methodsFor!

icon
^String icon! !
!OFCStyledText class categoriesFor: #icon!public! !

OFCTooltipStyle guid: (GUID fromString: '{D54CDCF1-DE19-4674-9A37-A149EABA1DE4}')!
OFCTooltipStyle comment: ''!
!OFCTooltipStyle categoriesForClass!Unclassified! !
!OFCTooltipStyle methodsFor!

backgroundColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #background
		put: aColorOrHTMLHexString!

bodyStyle: aCssString 
	self 
		propertiesAt: #body
		put: aCssString!

disabled
	self propertiesAt: #mouse put: 4!

elementType
	^ #tooltip!

hoover
	self propertiesAt: #mouse put: 2!

lineColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

lineWidth: anInteger 
	self 
		propertiesAt: #stroke
		put: anInteger!

proximity
	self propertiesAt: #mouse put: 1!

shadow: aBoolean
	self propertiesAt: #shadow put: aBoolean !

titleStyle: aByteString 
	self propertiesAt: #title put: aByteString! !
!OFCTooltipStyle categoriesFor: #backgroundColor:!accessing!public! !
!OFCTooltipStyle categoriesFor: #bodyStyle:!accessing!public! !
!OFCTooltipStyle categoriesFor: #disabled!accessing!public! !
!OFCTooltipStyle categoriesFor: #elementType!constants!public! !
!OFCTooltipStyle categoriesFor: #hoover!accessing!public! !
!OFCTooltipStyle categoriesFor: #lineColor:!accessing!public! !
!OFCTooltipStyle categoriesFor: #lineWidth:!accessing!public! !
!OFCTooltipStyle categoriesFor: #proximity!accessing!public! !
!OFCTooltipStyle categoriesFor: #shadow:!accessing!public! !
!OFCTooltipStyle categoriesFor: #titleStyle:!accessing!public! !

!OFCTooltipStyle class methodsFor!

icon
^Tooltip icon! !
!OFCTooltipStyle class categoriesFor: #icon!public! !

OFCEuclidianAxis guid: (GUID fromString: '{86A36ACB-128B-4415-8D89-7B7643568FEC}')!
OFCEuclidianAxis comment: ''!
!OFCEuclidianAxis categoriesForClass!Unclassified! !
!OFCEuclidianAxis methodsFor!

lineWidth: aNumber 
	self 
		propertiesAt: #stroke
		put: aNumber!

offset: aBoolean  
	self propertiesAt: #offset put: (aBoolean ifTrue: [1] ifFalse: [0]) !

tickSize: anInteger 
	self subclassResponsibility! !
!OFCEuclidianAxis categoriesFor: #lineWidth:!accessing!public! !
!OFCEuclidianAxis categoriesFor: #offset:!accessing!public! !
!OFCEuclidianAxis categoriesFor: #tickSize:!accessing!public! !

OFCRadarAxis guid: (GUID fromString: '{0372D842-2039-4D09-B4D0-F38FD52351BA}')!
OFCRadarAxis comment: ''!
!OFCRadarAxis categoriesForClass!Unclassified! !
!OFCRadarAxis methodsFor!

elementType
	^ #'radar_axis'!

spokeLabels
	^ OFCSpokeLabels new
		setOFCCanvas: ofcCanvas
			axis: self;
		yourself!

spokeLabels: aBlock 
	^ self spokeLabels
		with: aBlock;
		yourself! !
!OFCRadarAxis categoriesFor: #elementType!constants!public! !
!OFCRadarAxis categoriesFor: #spokeLabels!accessing!public! !
!OFCRadarAxis categoriesFor: #spokeLabels:!accessing!public! !

OFCXAxis guid: (GUID fromString: '{B2356117-638D-4112-A602-0CB63E7BAD2E}')!
OFCXAxis comment: ''!
!OFCXAxis categoriesForClass!Unclassified! !
!OFCXAxis methodsFor!

elementType
	^ #'x_axis'!

threeD: anInteger 
	self propertiesAt: #'3d' put: anInteger!

tickSize: anInteger 
	self propertiesAt: #'tick-height' put: anInteger! !
!OFCXAxis categoriesFor: #elementType!constants!public! !
!OFCXAxis categoriesFor: #threeD:!accessing!public! !
!OFCXAxis categoriesFor: #tickSize:!accessing!public! !

OFCYAxis guid: (GUID fromString: '{0ED40AD5-F481-4A96-BBA9-192A8AD1C842}')!
OFCYAxis comment: ''!
!OFCYAxis categoriesForClass!Unclassified! !
!OFCYAxis methodsFor!

elementType
	^ #'y_axis'!

labels
	self shouldNotImplement!

labels: aBlock 
	^ self labels
		with: aBlock;
		yourself!

labelValues: aCollectionOfStrings 
	self 
		propertiesAt: #labels
		put: aCollectionOfStrings!

tickSize: anInteger 
	self propertiesAt: #'tick-length' put: anInteger! !
!OFCYAxis categoriesFor: #elementType!constants!public! !
!OFCYAxis categoriesFor: #labels!accessing!public! !
!OFCYAxis categoriesFor: #labels:!accessing!public! !
!OFCYAxis categoriesFor: #labelValues:!accessing!public! !
!OFCYAxis categoriesFor: #tickSize:!accessing!public! !

OFCYAxisRight guid: (GUID fromString: '{6267B6E5-0355-427A-AA84-6A2C338D8560}')!
OFCYAxisRight comment: ''!
!OFCYAxisRight categoriesForClass!Unclassified! !
!OFCYAxisRight methodsFor!

elementType
	^ #'y_axis_right'! !
!OFCYAxisRight categoriesFor: #elementType!constants!public! !

OFCTitle guid: (GUID fromString: '{6B3FEBD1-910A-48A5-938F-9673C07542CC}')!
OFCTitle comment: ''!
!OFCTitle categoriesForClass!Unclassified! !
!OFCTitle methodsFor!

elementType
	^ #title! !
!OFCTitle categoriesFor: #elementType!constants!public! !

OFCXLegend guid: (GUID fromString: '{3ECF863B-AA83-436D-AE59-CE4A044B1954}')!
OFCXLegend comment: ''!
!OFCXLegend categoriesForClass!Unclassified! !
!OFCXLegend methodsFor!

elementType
	^ #'x_legend'! !
!OFCXLegend categoriesFor: #elementType!constants!public! !

OFCYLegend guid: (GUID fromString: '{4204B71C-8AC9-4FFC-87AB-E4E3F492CC95}')!
OFCYLegend comment: ''!
!OFCYLegend categoriesForClass!Unclassified! !
!OFCYLegend methodsFor!

elementType
	^ #'y_legend'! !
!OFCYLegend categoriesFor: #elementType!constants!public! !

OFCChart guid: (GUID fromString: '{B8D94817-1911-43D4-B72D-19B349C2FBFC}')!
OFCChart comment: ''!
!OFCChart categoriesForClass!Unclassified! !
!OFCChart methodsFor!

ofcOnClick: anURLorJsFunction 
	self 
		propertiesAt: #'on-click'
		put: anURLorJsFunction!

tooltip
	^ self 
		propertiesAt: #tip
		put: OFCTooltip new!

tooltip: aBlock 
	^ self tooltip
		with: aBlock;
		yourself!

url: anURLString 
	"A click on any of this charts datapoint calls this URL in a NEW WINDOW!!"
	self ofcOnClick: anURLString! !
!OFCChart categoriesFor: #ofcOnClick:!private! !
!OFCChart categoriesFor: #tooltip!accessing!public! !
!OFCChart categoriesFor: #tooltip:!accessing!public! !
!OFCChart categoriesFor: #url:!accessing!public! !

!OFCChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCCanvas.ico'! !
!OFCChart class categoriesFor: #icon!public! !

OFCShape guid: (GUID fromString: '{5421B2B7-440A-4561-BFCC-EE1A8313C351}')!
OFCShape comment: ''!
!OFCShape categoriesForClass!Unclassified! !
!OFCShape methodsFor!

elementType
	^#shape!

fillColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

values: aCollectionOfPoints 
	self 
		propertiesAt: #values
		put: aCollectionOfPoints! !
!OFCShape categoriesFor: #elementType!constants!public! !
!OFCShape categoriesFor: #fillColor:!accessing!public! !
!OFCShape categoriesFor: #values:!accessing!public! !

OFCNamedChart guid: (GUID fromString: '{D1AE6709-FB81-4220-90A9-36EA42275625}')!
OFCNamedChart comment: ''!
!OFCNamedChart categoriesForClass!Unclassified! !
!OFCNamedChart methodsFor!

name: aString
	self propertiesAt: #text put: aString!

nameSize: aNumber 
	self 
		propertiesAt: #'font-size'
		put: aNumber! !
!OFCNamedChart categoriesFor: #name:!accessing!public! !
!OFCNamedChart categoriesFor: #nameSize:!accessing!public! !

OFCPieChart guid: (GUID fromString: '{A2DE5566-C2C4-44EC-A693-89CD52863B47}')!
OFCPieChart comment: ''!
!OFCPieChart categoriesForClass!Unclassified! !
!OFCPieChart methodsFor!

animate: aBoolean 
	self 
		propertiesAt: #animate
		put: aBoolean!

elementType
	^ #pie!

fillColors: anArrayOfColorsOrStrings 
	self propertiesAt: #'colours' put: anArrayOfColorsOrStrings!

gradientFill: aBoolean 
	self 
		propertiesAt: #'gradient-fill'
		put: aBoolean!

hideLabels
	self showLabels: false!

showLabels
	self showLabels: true!

showLabels: aBoolean 
	self 
		propertiesAt: #'no-labels'
		put: aBoolean not!

startAngle: aNumber 
	self 
		propertiesAt: #'start-angle'
		put: aNumber! !
!OFCPieChart categoriesFor: #animate:!accessing!public! !
!OFCPieChart categoriesFor: #elementType!constants!public! !
!OFCPieChart categoriesFor: #fillColors:!accessing!public! !
!OFCPieChart categoriesFor: #gradientFill:!accessing!public! !
!OFCPieChart categoriesFor: #hideLabels!accessing!public! !
!OFCPieChart categoriesFor: #showLabels!accessing!public! !
!OFCPieChart categoriesFor: #showLabels:!accessing!public! !
!OFCPieChart categoriesFor: #startAngle:!accessing!public! !

!OFCPieChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCPieChart.ico'! !
!OFCPieChart class categoriesFor: #icon!public! !

OFCAreaChart guid: (GUID fromString: '{F3B2258A-8C22-4FD8-8193-F63119BFD941}')!
OFCAreaChart comment: ''!
!OFCAreaChart categoriesForClass!Unclassified! !
!OFCAreaChart methodsFor!

closeLoop: aBoolean
	"Should only be used for Radar Charts (Radar Axis)"
	self propertiesAt: #'loop' put: aBoolean!

dotHaloSize: aNumber 
	self 
		propertiesAt: #'halo-size'
		put: aNumber!

dotSize: aNumber 
	self 
		propertiesAt: #'dot-size'
		put: aNumber!

fillAlpha: aNumber 
	self 
		propertiesAt: #'fill-alpha'
		put: aNumber!

fillColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #fill
		put: aColorOrHTMLHexString!

lineColor: aColor
	self propertiesAt: #'colour' put:  (aColor)!

lineWidth: anInteger 
	self propertiesAt: #width put: anInteger! !
!OFCAreaChart categoriesFor: #closeLoop:!accessing!public! !
!OFCAreaChart categoriesFor: #dotHaloSize:!accessing!public! !
!OFCAreaChart categoriesFor: #dotSize:!accessing!public! !
!OFCAreaChart categoriesFor: #fillAlpha:!accessing!public! !
!OFCAreaChart categoriesFor: #fillColor:!accessing!public! !
!OFCAreaChart categoriesFor: #lineColor:!accessing!public! !
!OFCAreaChart categoriesFor: #lineWidth:!accessing!public! !

!OFCAreaChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCAreaChart.ico'! !
!OFCAreaChart class categoriesFor: #icon!public! !

OFCBarChart guid: (GUID fromString: '{7F0FF844-FADA-43EF-A42E-BEAF73FBB778}')!
OFCBarChart comment: ''!
!OFCBarChart categoriesForClass!Unclassified! !
!OFCBarChart methodsFor!

elementType
	^ #bar!

fillAlpha: aNumber 
	self 
		propertiesAt: #alpha
		put: aNumber!

fillColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString! !
!OFCBarChart categoriesFor: #elementType!constants!public! !
!OFCBarChart categoriesFor: #fillAlpha:!accessing!public! !
!OFCBarChart categoriesFor: #fillColor:!accessing!public! !

!OFCBarChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCBarChart.ico'! !
!OFCBarChart class categoriesFor: #icon!public! !

OFCLineChart guid: (GUID fromString: '{8FB6BFEC-E526-4C8B-AFE3-7FFBF7275C97}')!
OFCLineChart comment: ''!
!OFCLineChart categoriesForClass!Unclassified! !
!OFCLineChart methodsFor!

closeLoop: aBoolean 
	"Should only be used for Radar Charts (Radar Axis)"
	self 
		propertiesAt: #loop
		put: aBoolean!

elementType
	^ #line!

lineColor: aColor
	self propertiesAt: #'colour' put:  (aColor)!

lineWidth: anInteger 
	self propertiesAt: #width put: anInteger! !
!OFCLineChart categoriesFor: #closeLoop:!accessing!public! !
!OFCLineChart categoriesFor: #elementType!constants!public! !
!OFCLineChart categoriesFor: #lineColor:!accessing!public! !
!OFCLineChart categoriesFor: #lineWidth:!accessing!public! !

!OFCLineChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCLineChart.ico'! !
!OFCLineChart class categoriesFor: #icon!public! !

OFCScatterChart guid: (GUID fromString: '{3A972261-32B1-4456-91B7-D75F51339F96}')!
OFCScatterChart comment: ''!
!OFCScatterChart categoriesForClass!Unclassified! !
!OFCScatterChart methodsFor!

dotHaloSize: aNumber 
	self 
		propertiesAt: #'halo-size'
		put: aNumber!

dotSize: aNumber 
	self 
		propertiesAt: #'dot-size'
		put: aNumber!

elementType
	^ #'scatter'!

lineColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

lineWidth: aNumber 
	self 
		propertiesAt: #width
		put: aNumber!

values: anArrayOfPoints 
	self 
		propertiesAt: #values
		put: anArrayOfPoints! !
!OFCScatterChart categoriesFor: #dotHaloSize:!accessing!public! !
!OFCScatterChart categoriesFor: #dotSize:!accessing!public! !
!OFCScatterChart categoriesFor: #elementType!constants!public! !
!OFCScatterChart categoriesFor: #lineColor:!accessing!public! !
!OFCScatterChart categoriesFor: #lineWidth:!accessing!public! !
!OFCScatterChart categoriesFor: #values:!accessing!public! !

!OFCScatterChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCScatterChart.ico'! !
!OFCScatterChart class categoriesFor: #icon!public! !

OFCStackedBarChart guid: (GUID fromString: '{AE8BCB28-FDA4-4EAA-AFB8-8B567880426F}')!
OFCStackedBarChart comment: ''!
!OFCStackedBarChart categoriesForClass!Unclassified! !
!OFCStackedBarChart methodsFor!

addDataset
	self properties.
	^ self datasets add: OFCStackedBarDataset new!

datasets
	datasets isNil ifTrue: [ datasets := OrderedCollection new ].
	^ datasets!

elementType
	^#'bar_stack'!

jsonSaveOn: aStream 
	| values colors keys |
	values := OrderedCollection new.
	colors := OrderedCollection new.
	keys := OrderedCollection new.
	datasets do: 
			[:eachSet | 
			colors add: eachSet color.
			keys add: eachSet key].
	1 to: datasets first values size
		do: [:valueIndex | values add: (datasets collect: [:eachSet | eachSet values at: valueIndex])].
	(properties
		copy;
		at: #values put: values;
		at: #colours put: colors;
		at: #keys put: keys;
		yourself) jsonSaveOn: aStream!

properties
	properties isNil ifTrue: 
		[ | elements |
		(ofcCanvas arrayAt: #elements) add: self.
		properties := Dictionary new.
		properties 
			at: #type
			put: self elementType ].
	^ properties!

values: anArray 
	"Use #addDataset instead!!"
	self shouldNotImplement! !
!OFCStackedBarChart categoriesFor: #addDataset!accessing!public! !
!OFCStackedBarChart categoriesFor: #datasets!accessing!public! !
!OFCStackedBarChart categoriesFor: #elementType!constants!public! !
!OFCStackedBarChart categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCStackedBarChart categoriesFor: #properties!accessing!public! !
!OFCStackedBarChart categoriesFor: #values:!accessing!public! !

!OFCStackedBarChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCBarChart.ico'! !
!OFCStackedBarChart class categoriesFor: #icon!public! !

OFCHollowAreaChart guid: (GUID fromString: '{C85F1B32-07A9-40AF-9352-17D1FF22CCC9}')!
OFCHollowAreaChart comment: ''!
!OFCHollowAreaChart categoriesForClass!Unclassified! !
!OFCHollowAreaChart methodsFor!

elementType
	^ #'area_hollow'! !
!OFCHollowAreaChart categoriesFor: #elementType!constants!public! !

OFCLineAreaChart guid: (GUID fromString: '{94210487-C4DD-453A-9C4D-3E7D954DFEDA}')!
OFCLineAreaChart comment: ''!
!OFCLineAreaChart categoriesForClass!Unclassified! !
!OFCLineAreaChart methodsFor!

elementType
	^ #'area_line'! !
!OFCLineAreaChart categoriesFor: #elementType!constants!public! !

OFC3dBarChart guid: (GUID fromString: '{0E717843-8EE0-4A1C-90B5-C8ACE2E74767}')!
OFC3dBarChart comment: ''!
!OFC3dBarChart categoriesForClass!Unclassified! !
!OFC3dBarChart methodsFor!

elementType
	^ #'bar_3d'! !
!OFC3dBarChart categoriesFor: #elementType!constants!public! !

OFCCylinderChart guid: (GUID fromString: '{6075A1B6-3677-4AA5-8E5E-7BA12B5EEF08}')!
OFCCylinderChart comment: ''!
!OFCCylinderChart categoriesForClass!Unclassified! !
!OFCCylinderChart methodsFor!

elementType
	^ #'bar_cylinder'! !
!OFCCylinderChart categoriesFor: #elementType!constants!public! !

OFCGlassBarChart guid: (GUID fromString: '{2B51D2B7-C0F6-4FF9-A7F0-3310401126F8}')!
OFCGlassBarChart comment: ''!
!OFCGlassBarChart categoriesForClass!Unclassified! !
!OFCGlassBarChart methodsFor!

elementType
	^ #'bar_glass'! !
!OFCGlassBarChart categoriesFor: #elementType!constants!public! !

OFCHorizontalBarChart guid: (GUID fromString: '{6369F6D6-BD85-435F-809F-7151A96A2BF4}')!
OFCHorizontalBarChart comment: ''!
!OFCHorizontalBarChart categoriesForClass!Unclassified! !
!OFCHorizontalBarChart methodsFor!

elementType
	^ #hbar! !
!OFCHorizontalBarChart categoriesFor: #elementType!constants!public! !

OFCOutlinedBarChart guid: (GUID fromString: '{9A320B8E-5A83-4F7A-AC68-E79E8D5CD60C}')!
OFCOutlinedBarChart comment: ''!
!OFCOutlinedBarChart categoriesForClass!Unclassified! !
!OFCOutlinedBarChart methodsFor!

elementType
	^ #'bar_filled'!

lineColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #'outline-colour'
		put: aColorOrHTMLHexString! !
!OFCOutlinedBarChart categoriesFor: #elementType!constants!public! !
!OFCOutlinedBarChart categoriesFor: #lineColor:!accessing!public! !

OFCRoundGlassBarChart guid: (GUID fromString: '{63C7F979-A5ED-491D-BBBC-5039F5806C03}')!
OFCRoundGlassBarChart comment: ''!
!OFCRoundGlassBarChart categoriesForClass!Unclassified! !
!OFCRoundGlassBarChart methodsFor!

elementType
	^ #'bar_round_glass'! !
!OFCRoundGlassBarChart categoriesFor: #elementType!constants!public! !

OFCSketchBarChart guid: (GUID fromString: '{BB588311-603E-4BA8-BC4D-1EAFE150D0A7}')!
OFCSketchBarChart comment: ''!
!OFCSketchBarChart categoriesForClass!Unclassified! !
!OFCSketchBarChart methodsFor!

elementType
	^ #'bar_sketch'!

funFactor: anInteger 
	self 
		propertiesAt: #offset
		put: anInteger! !
!OFCSketchBarChart categoriesFor: #elementType!constants!public! !
!OFCSketchBarChart categoriesFor: #funFactor:!accessing!public! !

OFCDottedLineChart guid: (GUID fromString: '{42E5F745-D91A-4B40-B476-832A8981DF5B}')!
OFCDottedLineChart comment: ''!
!OFCDottedLineChart categoriesForClass!Unclassified! !
!OFCDottedLineChart methodsFor!

dotHaloSize: aNumber 
	self 
		propertiesAt: #'halo-size'
		put: aNumber!

dotSize: aNumber 
	self 
		propertiesAt: #'dot-size'
		put: aNumber!

elementType
	^ #'line_dot'! !
!OFCDottedLineChart categoriesFor: #dotHaloSize:!accessing!public! !
!OFCDottedLineChart categoriesFor: #dotSize:!accessing!public! !
!OFCDottedLineChart categoriesFor: #elementType!constants!public! !

!OFCDottedLineChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCDottedLineChart.ico'! !
!OFCDottedLineChart class categoriesFor: #icon!public! !

OFCHollowLineChart guid: (GUID fromString: '{135CFDD9-0E7D-419C-8A81-EB98007FDF42}')!
OFCHollowLineChart comment: ''!
!OFCHollowLineChart categoriesForClass!Unclassified! !
!OFCHollowLineChart methodsFor!

elementType
	^ #'line_hollow'! !
!OFCHollowLineChart categoriesFor: #elementType!constants!public! !

OFCLineScatterChart guid: (GUID fromString: '{EF60ED57-EB6B-4293-90D0-14B0FEA2469B}')!
OFCLineScatterChart comment: ''!
!OFCLineScatterChart categoriesForClass!Unclassified! !
!OFCLineScatterChart methodsFor!

elementType
	^ #'scatter_line'! !
!OFCLineScatterChart categoriesFor: #elementType!constants!public! !

!OFCLineScatterChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCDottedLineChart.ico'! !
!OFCLineScatterChart class categoriesFor: #icon!public! !

OFCBarValue guid: (GUID fromString: '{5F586141-4B3A-461E-921E-A5C821EDE1C3}')!
OFCBarValue comment: ''!
!OFCBarValue categoriesForClass!Unclassified! !
!OFCBarValue methodsFor!

bottom: aNumber
	self propertiesAt: #bottom put: aNumber !

left: aNumber
	self propertiesAt: #left put: aNumber !

lineColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #'outline-colour'
		put: aColorOrHTMLHexString!

right: aNumber 
	self 
		propertiesAt: #right
		put: aNumber!

top: aNumber 
	self 
		propertiesAt: #top
		put: aNumber!

value: aNumber 
	self
		bottom: 0;
		top: aNumber;
		left: 0;
		right: aNumber! !
!OFCBarValue categoriesFor: #bottom:!accessing!public! !
!OFCBarValue categoriesFor: #left:!accessing!public! !
!OFCBarValue categoriesFor: #lineColor:!accessing!public! !
!OFCBarValue categoriesFor: #right:!accessing!public! !
!OFCBarValue categoriesFor: #top:!accessing!public! !
!OFCBarValue categoriesFor: #value:!accessing!public! !

OFCLineValue guid: (GUID fromString: '{2C1095F0-2809-420A-B329-E1FA872C7F53}')!
OFCLineValue comment: ''!
!OFCLineValue categoriesForClass!Unclassified! !
!OFCLineValue methodsFor!

value: anInteger
	self propertiesAt: #value put: anInteger ! !
!OFCLineValue categoriesFor: #value:!accessing!public! !

OFCPieValue guid: (GUID fromString: '{E7621D58-56EC-49B2-9FD9-C26AFA6D72B0}')!
OFCPieValue comment: ''!
!OFCPieValue categoriesForClass!Unclassified! !
!OFCPieValue methodsFor!

label: aString 
	self 
		propertiesAt: #label
		put: aString!

labelColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #'label-colour'
		put: aColorOrHTMLHexString!

labelSize: aNumber 
	self 
		propertiesAt: #'font-size'
		put: aNumber!

value: anInteger
	self propertiesAt: #value put: anInteger ! !
!OFCPieValue categoriesFor: #label:!accessing!public! !
!OFCPieValue categoriesFor: #labelColor:!accessing!public! !
!OFCPieValue categoriesFor: #labelSize:!accessing!public! !
!OFCPieValue categoriesFor: #value:!accessing!public! !

OFCRadarValue guid: (GUID fromString: '{9A266B55-30BF-4BE3-9AF4-82A77FBA091A}')!
OFCRadarValue comment: ''!
!OFCRadarValue categoriesForClass!Unclassified! !
!OFCRadarValue methodsFor!

value: aNumber 
	self 
		propertiesAt: #value
		put: aNumber! !
!OFCRadarValue categoriesFor: #value:!accessing!public! !

OFCStackedBarValue guid: (GUID fromString: '{7F8829D7-7AA4-4BF9-B5A8-81DCAD45408A}')!
OFCStackedBarValue comment: ''!
!OFCStackedBarValue categoriesForClass!Unclassified! !
!OFCStackedBarValue methodsFor!

tooltip
	self shouldNotImplement!

value: anInteger
	self propertiesAt: #val put: anInteger ! !
!OFCStackedBarValue categoriesFor: #tooltip!accessing!public! !
!OFCStackedBarValue categoriesFor: #value:!accessing!public! !

"Binary Globals"!

