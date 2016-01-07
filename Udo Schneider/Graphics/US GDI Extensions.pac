| package |
package := Package name: 'US GDI Extensions'.
package paxVersion: 1;
	basicComment: '$id: US GDI Extensions 0.027$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.027'.

package basicScriptAt: #preinstall put: 'Win32Constants at: #RGN_COPY put: 5.	"Region constants"
"Canvas constants"
Win32Constants
	at: #BLACKONWHITE put: 1;
	at: #WHITEONBLACK put: 2;
	at: #COLORONCOLOR put: 3;
	at: #HALFTONE put: 4;
	at: #STRETCH_ANDSCANS put: 1;
	at: #STRETCH_ORSCANS put: 2;
	at: #STRETCH_DELETESCANS put: 3;
	at: #STRETCH_HALFTONE put: 4.'.

package methodNames
	add: #Bitmap -> #asBitmap;
	add: #Canvas -> #abortPath;
	add: #Canvas -> #angleArc:radius:startAngle:sweepAngle:;
	add: #Canvas -> #arc:start:end:;
	add: #Canvas -> #arcTo:start:end:;
	add: #Canvas -> #beginPath;
	add: #Canvas -> #closeFigure;
	add: #Canvas -> #endPath;
	add: #Canvas -> #fillPath;
	add: #Canvas -> #fillRectangle:round:startColor:endColor:verticalGradient:;
	add: #Canvas -> #flattenPath;
	add: #Canvas -> #getClipRegion;
	add: #Canvas -> #miterLimit;
	add: #Canvas -> #miterLimit:;
	add: #Canvas -> #pathToRegion;
	add: #Canvas -> #polyBezier:;
	add: #Canvas -> #selectClipPath:;
	add: #Canvas -> #stretchBlitMode:;
	add: #Canvas -> #strokeAndFillPath;
	add: #Canvas -> #strokePath;
	add: #Canvas -> #widenPath;
	add: #DIBSection -> #getColors;
	add: #DIBSection -> #setColors:;
	add: #GDILibrary -> #abortPath:;
	add: #GDILibrary -> #angleArc:x:y:radius:startAngle:sweepAngle:;
	add: #GDILibrary -> #arc:nLeftRect:nTopRect:nRightRect:nBottomRect:nXStartArc:nYStartArc:nXEndArc:nYEndArc:;
	add: #GDILibrary -> #arcTo:nLeftRect:nTopRect:nRightRect:nBottomRect:nXStartArc:nYStartArc:nXEndArc:nYEndArc:;
	add: #GDILibrary -> #createDIBitmap:lpbmih:fdwInit:lpbInit:lpbmi:fuUsage:;
	add: #GDILibrary -> #equalRgn:hSrcRgn2:;
	add: #GDILibrary -> #fillPath:;
	add: #GDILibrary -> #flattenPath:;
	add: #GDILibrary -> #getClipRgn:hrgn:;
	add: #GDILibrary -> #getMiterLimit:limit:;
	add: #GDILibrary -> #pathToRegion:;
	add: #GDILibrary -> #polyBezier:lpPoints:nCount:;
	add: #GDILibrary -> #polyBezierTo:lpPoints:nCount:;
	add: #GDILibrary -> #setMiterLimit:newLimit:oldLimit:;
	add: #GDILibrary -> #setStretchBltMode:iStretchMode:;
	add: #GDILibrary -> #strokeAndFillPath:;
	add: #GDILibrary -> #strokePath:;
	add: #GDILibrary -> #widenPath:;
	add: #Image -> #asBitmap;
	add: #Integer -> #wrapArround:;
	add: #Region -> #=;
	add: #Region -> #copy;
	add: #Region -> #xor:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Bitmap methodsFor!

asBitmap
	^self! !
!Bitmap categoriesFor: #asBitmap!public! !

!Canvas methodsFor!

abortPath
^GDILibrary default abortPath: self asParameter!

angleArc: aPoint radius: radius startAngle: startAngle sweepAngle: sweepAngle 
	^GDILibrary default 
		angleArc: self asParameter
		x: aPoint x
		y: aPoint y
		radius: radius
		startAngle: startAngle
		sweepAngle: sweepAngle!

arc: aRectangle start: startPoint end: endPoint 
	^GDILibrary default 
		arc: self asParameter
		nLeftRect: aRectangle left
		nTopRect: aRectangle top
		nRightRect: aRectangle right 
		nBottomRect: aRectangle bottom 
		nXStartArc: startPoint x
		nYStartArc: startPoint y
		nXEndArc: endPoint x
		nYEndArc: endPoint y!

arcTo: aRectangle start: startPoint end: endPoint 
	^GDILibrary default 
		arcTo: self asParameter
		nLeftRect: aRectangle left
		nTopRect: aRectangle top
		nRightRect: aRectangle right
		nBottomRect: aRectangle bottom
		nXStartArc: startPoint x
		nYStartArc: startPoint y
		nXEndArc: endPoint x
		nYEndArc: endPoint y!

beginPath
	^GDILibrary default beginPath: self asParameter!

closeFigure
^GDILibrary default closeFigure: self asParameter!

endPath
	^GDILibrary default endPath: self asParameter!

fillPath
^GDILibrary default fillPath: self asParameter!

fillRectangle: aRectangle round: aPoint startColor: startColor endColor: endColor verticalGradient: aBoolean 
	| oldClippingRegion |
	oldClippingRegion := self getClipRegion.
	self
		beginPath;
		rectangle: aRectangle round: aPoint;
		endPath;
		selectClipPath: RGN_COPY;
		fillRectangle: aRectangle
			startColor: startColor
			endColor: endColor
			verticalGradient: aBoolean;
		selectClipRegion: oldClippingRegion!

flattenPath
^GDILibrary default flattenPath: self asParameter!

getClipRegion
	| region result |
	region := Region empty.
	result := GDILibrary default getClipRgn: self asParameter hrgn: region asParameter.
	result = -1 ifTrue: [GDILibrary default invalidCall].
	result = 0 ifTrue: [
		^nil].
	^region!

miterLimit
	| answer |
	answer := FLOAT new.
	(GDILibrary default getMiterLimit: self asParameter limit: answer) 
		ifFalse: [GDILibrary default systemError].
	^answer value!

miterLimit: aFloat 
	| answer |
	answer := FLOAT new.
	(GDILibrary default 
		setMiterLimit: self asParameter
		newLimit: aFloat
		oldLimit: answer) ifFalse: [GDILibrary default systemError].
	^answer value!

pathToRegion
	^Region fromOwnedHandle: (GDILibrary default pathToRegion: self asParameter)!

polyBezier: collectionOfPoints 
	"Draw a filled Bezier from the collection of points."

	| points count |
	count := collectionOfPoints size.
	points := StructureArray length: count elementClass: POINTL.
	points with: collectionOfPoints
		do: 
			[:pointl :point | 
			pointl
				x: point x;
				y: point y].
	^GDILibrary default polyBezier: 
 self asParameter
		lpPoints: points
		nCount: count!

selectClipPath: iMode 
	"iMode can be one of:
	RGN_AND
		The new clipping region includes the intersection (overlapping areas) of the current clipping region and the current path.
	RGN_COPY 
		The new clipping region is the current path.
	RGN_DIFF
		The new clipping region includes the areas of the current clipping region with those of the current path excluded.
	RGN_OR
		The new clipping region includes the union (combined areas) of the current clipping region and the current path.
	RGN_XOR
		The new clipping region includes the union of the current clipping region and the current path but without the overlapping areas."

	^GDILibrary default selectClipPath: self asParameter iMode: iMode!

stretchBlitMode: anInteger 
	| answer |
	answer := GDILibrary default setStretchBltMode: self asParameter iStretchMode: anInteger.
	answer = 0 ifTrue: [GDILibrary default systemError].
	^answer!

strokeAndFillPath
	^GDILibrary default strokeAndFillPath: self asParameter!

strokePath
	^GDILibrary default strokePath: self asParameter!

widenPath
	^GDILibrary default widenPath: self asParameter! !
!Canvas categoriesFor: #abortPath!public! !
!Canvas categoriesFor: #angleArc:radius:startAngle:sweepAngle:!public! !
!Canvas categoriesFor: #arc:start:end:!public! !
!Canvas categoriesFor: #arcTo:start:end:!public! !
!Canvas categoriesFor: #beginPath!public! !
!Canvas categoriesFor: #closeFigure!public! !
!Canvas categoriesFor: #endPath!public! !
!Canvas categoriesFor: #fillPath!public! !
!Canvas categoriesFor: #fillRectangle:round:startColor:endColor:verticalGradient:!drawing!public! !
!Canvas categoriesFor: #flattenPath!public! !
!Canvas categoriesFor: #getClipRegion!public! !
!Canvas categoriesFor: #miterLimit!public! !
!Canvas categoriesFor: #miterLimit:!public! !
!Canvas categoriesFor: #pathToRegion!public! !
!Canvas categoriesFor: #polyBezier:!drawing!public! !
!Canvas categoriesFor: #selectClipPath:!public! !
!Canvas categoriesFor: #stretchBlitMode:!public! !
!Canvas categoriesFor: #strokeAndFillPath!public! !
!Canvas categoriesFor: #strokePath!public! !
!Canvas categoriesFor: #widenPath!public! !

!DIBSection methodsFor!

getColors
	^self getColorTable collect: [:each | each asColor]!

setColors: aColorArray
^self setColorTable: (StructureArray withAll: (aColorArray collect: [:each | RGBQUAD fromColor: each])
							elementClass: RGBQUAD)! !
!DIBSection categoriesFor: #getColors!accessing!public! !
!DIBSection categoriesFor: #setColors:!accessing!public! !

!GDILibrary methodsFor!

abortPath: hdc 
	"The AbortPath function closes and discards any paths in the specified device context.

	BOOL AbortPath(
		HDC hdc   // handle to DC
	);"

	<stdcall: bool AbortPath handle>
	^self invalidCall!

angleArc: hdc x: x y: y radius: dwRadius startAngle: eStartAngle sweepAngle: eSweepAngle 
	"BOOL AngleArc(
  HDC hdc,            // handle to device context
  int X,              // x-coordinate of circle's center
  int Y,              // y-coordinate of circle's center
  DWORD dwRadius,     // circle's radius
  FLOAT eStartAngle,  // arc's start angle
  FLOAT eSweepAngle   // arc's sweep angle
);"
<stdcall: bool AngleArc handle sdword sdword dword float float>
^self invalidCall
	!

arc: hdc nLeftRect: l nTopRect: t nRightRect: r nBottomRect: b nXStartArc: xs nYStartArc: ys nXEndArc: xe nYEndArc: ye
	"The Arc function draws an elliptical arc.

BOOL Arc(
  HDC hdc,         // handle to device context
  int nLeftRect,   // x-coord of rectangle's upper-left corner
  int nTopRect,    // y-coord of rectangle's upper-left corner
  int nRightRect,  // x-coord of rectangle's lower-right corner
  int nBottomRect, // y-coord of rectangle's lower-right corner
  int nXStartArc,  // x-coord of first radial ending point
  int nYStartArc,  // y-coord of first radial ending point
  int nXEndArc,    // x-coord of second radial ending point
  int nYEndArc     // y-coord of second radial ending point
);"

	<stdcall: bool Arc handle sdword sdword sdword sdword sdword sdword sdword sdword>
	^self invalidCall
!

arcTo: hdc nLeftRect: l nTopRect: t nRightRect: r nBottomRect: b nXStartArc: xs nYStartArc: ys nXEndArc: xe nYEndArc: ye 
	"The ArcTo function draws an elliptical arc.

BOOL ArcTo(
  HDC hdc,          // handle to device context
  int nLeftRect,    // x-coord of rectangle's upper-left corner
  int nTopRect,     // y-coord of rectangle's upper-left corner
  int nRightRect,   // x-coord of rectangle's lower-right corner
  int nBottomRect,  // y-coord of rectangle's lower-right corner
  int nXRadial1,    // x-coord of first radial ending point
  int nYRadial1,    // y-coord of first radial ending point
  int nXRadial2,    // x-coord of second radial ending point
  int nYRadial2     // y-coord of second radial ending point
);"

	<stdcall: bool ArcTo handle sdword sdword sdword sdword sdword sdword sdword sdword>
	^self invalidCall!

createDIBitmap: hdc lpbmih: lpbmih fdwInit: fdwInit lpbInit: lpbInit lpbmi: lpbmi fuUsage: fuUsage 
	"The CreateDIBitmap function creates a compatible bitmap (DDB) from a DIB and, optionally, sets the bitmap bits.

	HBITMAP CreateDIBitmap(
		HDC hdc,                        // handle to DC
		CONST BITMAPINFOHEADER *lpbmih, // bitmap data
		DWORD fdwInit,                  // initialization option
		CONST VOID *lpbInit,            // initialization data
		CONST BITMAPINFO *lpbmi,        // color-format data
		UINT fuUsage                    // color-data usage
	);"

	<stdcall: handle CreateDIBitmap handle lpvoid dword lpvoid lpvoid dword>
	^self invalidCall!

equalRgn: hSrcRgn1 hSrcRgn2: hSrcRgn2 
	"The EqualRgn function checks the two specified regions to determine whether they are identical. The function considers two regions identical if they are equal in size and shape.

	BOOL EqualRgn(
		HRGN hSrcRgn1,  // handle to first region
		HRGN hSrcRgn2   // handle to second region
	);"

	<stdcall: bool EqualRgn handle handle>
	^self invalidCall!

fillPath: hdc 
	"The FillPath function closes any open figures in the current path and fills the path's interior by using the current brush and polygon-filling mode.

	BOOL FillPath(
		HDC hdc   // handle to DC
	);"

	<stdcall: bool FillPath handle>
	^self invalidCall!

flattenPath: hdc 
	"The FlattenPath function transforms any curves in the path that is selected into the current device context (DC), turning each curve into a sequence of lines.

	BOOL FlattenPath(
		HDC hdc   // handle to DC
	);"

	<stdcall: bool FlattenPath handle>
	^self invalidCall!

getClipRgn: hdc hrgn: hrgn 
	"The GetClipRgn function retrieves a handle identifying the current application-defined clipping region for the specified device context.

	int GetClipRgn(
		__in  HDC hdc,
		__in  HRGN hrgn
	);"

	<stdcall: sdword GetClipRgn handle handle>
	^self invalidCall!

getMiterLimit: hdc limit: peLimit 
	"The GetMiterLimit function retrieves the miter limit for the specified device context.

	BOOL GetMiterLimit(
		HDC hdc,         // handle to DC
		PFLOAT peLimit   // miter limit
);"

	<stdcall: bool GetMiterLimit handle float*>
	^self invalidCall!

pathToRegion: hdc 
	"The PathToRegion function creates a region from the path that is selected into the specified device context. The resulting region uses device coordinates.

	HRGN PathToRegion(
		HDC hdc   // handle to DC
	);"

	<stdcall: handle PathToRegion handle>
	^self invalidCall!

polyBezier: hdc lpPoints: points nCount: count 
	"The PolyBezier function draws one or more Bzier curves.

	BOOL PolyBezier(
		HDC hdc,            // handle to device context
		CONST POINT* lppt,  // endpoints and control points
		DWORD cPoints       // count of endpoints and control points
);"

	<stdcall: bool PolyBezier handle lpvoid sdword>
	^self invalidCall!

polyBezierTo: hdc lpPoints: points nCount: count 
	"BOOL PolyBezierTo(
		HDC hdc,            // handle to device context
		CONST POINT *lppt,  // endpoints and control points
		DWORD cCount        // count of endpoints and control points
);"

	<stdcall: bool PolyBezierTo handle lpvoid sdword>
	^self invalidCall!

setMiterLimit: hdc newLimit: eNewLimit oldLimit: peOldLimit 
	"The SetMiterLimit function sets the limit for the length of miter joins for the specified device context.

	BOOL SetMiterLimit(
		HDC hdc,            // handle to DC
		FLOAT eNewLimit,    // new miter limit
		PFLOAT peOldLimit   // previous miter limit
	);"

	<stdcall: bool SetMiterLimit handle float float*>
	^self invalidCall!

setStretchBltMode: hdc iStretchMode: iStretchMode 
	"The SetStretchBltMode function sets the bitmap stretching mode in the specified device context.

	int SetStretchBltMode(
		HDC hdc,           // handle to DC
		int iStretchMode   // bitmap stretching mode
	);"

	<stdcall: sdword SetStretchBltMode handle sdword>
	^self invalidCall!

strokeAndFillPath: hdc 
	"The StrokeAndFillPath function closes any open figures in a path, strokes the outline of the path by using the current pen, and fills its interior by using the current brush.

	BOOL StrokeAndFillPath(
		HDC hdc   // handle to DC
	);"

	<stdcall: bool StrokeAndFillPath handle>
	^self invalidCall!

strokePath: hdc 
	"The StrokePath function renders the specified path by using the current pen.

	BOOL StrokePath(
		HDC hdc   // handle to DC
	);"

	<stdcall: bool StrokePath handle>
	^self invalidCall!

widenPath: hdc 
	"The WidenPath function redefines the current path as the area that would be painted if the path were stroked using the pen currently selected into the given device context.

	BOOL WidenPath(
		HDC hdc   // handle to DC
	);"

	<stdcall: bool WidenPath handle>
	^self invalidCall! !
!GDILibrary categoriesFor: #abortPath:!public! !
!GDILibrary categoriesFor: #angleArc:x:y:radius:startAngle:sweepAngle:!public! !
!GDILibrary categoriesFor: #arc:nLeftRect:nTopRect:nRightRect:nBottomRect:nXStartArc:nYStartArc:nXEndArc:nYEndArc:!public! !
!GDILibrary categoriesFor: #arcTo:nLeftRect:nTopRect:nRightRect:nBottomRect:nXStartArc:nYStartArc:nXEndArc:nYEndArc:!public! !
!GDILibrary categoriesFor: #createDIBitmap:lpbmih:fdwInit:lpbInit:lpbmi:fuUsage:!public! !
!GDILibrary categoriesFor: #equalRgn:hSrcRgn2:!public! !
!GDILibrary categoriesFor: #fillPath:!public! !
!GDILibrary categoriesFor: #flattenPath:!public! !
!GDILibrary categoriesFor: #getClipRgn:hrgn:!public! !
!GDILibrary categoriesFor: #getMiterLimit:limit:!public! !
!GDILibrary categoriesFor: #pathToRegion:!public! !
!GDILibrary categoriesFor: #polyBezier:lpPoints:nCount:!public!win32 functions-filled shape! !
!GDILibrary categoriesFor: #polyBezierTo:lpPoints:nCount:!public!win32 functions-filled shape! !
!GDILibrary categoriesFor: #setMiterLimit:newLimit:oldLimit:!public! !
!GDILibrary categoriesFor: #setStretchBltMode:iStretchMode:!public! !
!GDILibrary categoriesFor: #strokeAndFillPath:!public! !
!GDILibrary categoriesFor: #strokePath:!public! !
!GDILibrary categoriesFor: #widenPath:!public! !

!Image methodsFor!

asBitmap
	| bitmap |
	bitmap := Bitmap displayCompatibleWithExtent: self extent.
	self drawOn: bitmap canvas.
	^bitmap! !
!Image categoriesFor: #asBitmap!public! !

!Integer methodsFor!

wrapArround: arraySize 
	^(self - 1 + arraySize) \\ arraySize + 1! !
!Integer categoriesFor: #wrapArround:!public! !

!Region methodsFor!

= aRegion
^GDILibrary default equalRgn: self asParameter hSrcRgn2: aRegion asParameter!

copy
	"Answers a new region which is a copy of aRegion"

	^self combine: self mode: RGN_COPY!

xor: aRegion 
	"Answers a new region which is the union of two combined regions except for any overlapping areas."

	#USToDo.
	^self combine: aRegion mode: RGN_XOR! !
!Region categoriesFor: #=!public! !
!Region categoriesFor: #copy!public! !
!Region categoriesFor: #xor:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

