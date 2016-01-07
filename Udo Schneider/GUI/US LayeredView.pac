| package |
package := Package name: 'US LayeredView'.
package paxVersion: 1;
	basicComment: '$id: US LayeredView 0.008$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

This package contains additions to the standard Dolphin Smalltalk Shell class which allows you to use alpha blending and color keying functions available in Windows 2000.

Unfortunately it is not possible to define alpha blending and color keying properties in the VC yet. If I''ll find a way to "emulate" GetLayeredWindowAttributes with Windows 2000 (it''s just available in Windows XP) without having to a rewrite of several STB methods in most of the View classes it''s just a matter of adding the Aspects.

You have to do the following to use alpha blending and/or color keying on a view:

presenter := ClassBrowserShell show.
view := presenter view.
view isLayered: true.		"This allows us to apply alpha blending and/or color keying"
view alphaBlend: 128. 	"Values between 0 (invisible) and 255 (fully visible) are allowed here"
view colorKey: Color face3d.	"You can use any possible Color here"

"You can even combine both using #alphaBlend:colorKey: or #colorKey:alphaBlend:"
view alphaBlend: 128 colorKey: Color face3d.



Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.008'.

package basicScriptAt: #postuninstall put: 'Win32Constants removeKey: ''WS_EX_LAYERED'';
		removeKey: ''LWA_COLORKEY'';
		removeKey: ''LWA_ALPHA''.'.
package basicScriptAt: #preinstall put: 'Win32Constants at: ''WS_EX_LAYERED'' put: 16r00080000;
		at: ''LWA_COLORKEY'' put: 16r00000001;
		at: ''LWA_ALPHA'' put: 16r00000002.'.

package methodNames
	add: #UserLibrary -> #getLayeredWindowAttributes:pcrKey:pbAlpha:pdwFlags:;
	add: #UserLibrary -> #setLayeredWindowAttributes:crKey:bAlpha:dwFlags:;
	add: #View -> #alphaBlend;
	add: #View -> #alphaBlend:;
	add: #View -> #alphaBlend:colorKey:;
	add: #View -> #colorKey;
	add: #View -> #colorKey:;
	add: #View -> #colorKey:alphaBlend:;
	add: #View -> #isLayered;
	add: #View -> #isLayered:;
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

!UserLibrary methodsFor!

getLayeredWindowAttributes: hwnd pcrKey: pcrKey pbAlpha: pbAlpha pdwFlags: pdwFlags 
	"BOOL GetLayeredWindowAttributes(
		HWND hwnd,           // handle to the layered window
		COLORREF *pcrKey,    // color key
		BYTE *pbAlpha,       // Alpha value
		DWORD *pdwFlags      // layering flags
	);"

	<stdcall: bool GetLayeredWindowAttributes handle* dword* byte* dword*>
	^self invalidCall!

setLayeredWindowAttributes: hwnd crKey: crKey bAlpha: bAlpha dwFlags: dwFlags
	"BOOL SetLayeredWindowAttributes(
  		HWND hwnd,           // handle to the layered window
  		COLORREF crKey,      // specifies the color key
  		BYTE bAlpha,         // value for the blend function
  		DWORD dwFlags        // action
		);"

	<stdcall: bool SetLayeredWindowAttributes handle dword  byte dword>
	^self invalidCall
! !
!UserLibrary categoriesFor: #getLayeredWindowAttributes:pcrKey:pbAlpha:pdwFlags:!public! !
!UserLibrary categoriesFor: #setLayeredWindowAttributes:crKey:bAlpha:dwFlags:!public! !

!View methodsFor!

alphaBlend
	| alpha flags |
	alpha := BYTE new.
	flags := DWORD new.
	(UserLibrary default 
		getLayeredWindowAttributes: self asParameter
		pcrKey: alpha
		pbAlpha: 0
		pdwFlags: flags) ifFalse: [UserLibrary default systemError].
	^alpha value!

alphaBlend: anInteger
	^UserLibrary default setLayeredWindowAttributes: self asParameter crKey: 0 bAlpha: anInteger dwFlags: LWA_ALPHA!

alphaBlend: anInteger colorKey: aColor
	^self colorKey: aColor alphaBlend: anInteger
!

colorKey
	| color flags |
	color := DWORD new.
	flags := DWORD new.
	(UserLibrary default 
		getLayeredWindowAttributes: self asParameter
		pcrKey: color
		pbAlpha: 0
		pdwFlags: flags) ifFalse: [UserLibrary default systemError].
	^Color fromInteger: color value!

colorKey: aColor
	^UserLibrary default setLayeredWindowAttributes: self asParameter crKey: aColor asParameter bAlpha: 0 dwFlags: LWA_COLORKEY
!

colorKey: aColor alphaBlend: anInteger
	^UserLibrary default setLayeredWindowAttributes: self asParameter crKey: aColor asParameter bAlpha: anInteger dwFlags: LWA_ALPHA | LWA_COLORKEY
!

isLayered
	^self exStyleAllMask: WS_EX_LAYERED.
!

isLayered: aBoolean
	self exStyleMask: WS_EX_LAYERED set: aBoolean recreateIfChanged: false.! !
!View categoriesFor: #alphaBlend!public! !
!View categoriesFor: #alphaBlend:!public! !
!View categoriesFor: #alphaBlend:colorKey:!public! !
!View categoriesFor: #colorKey!public! !
!View categoriesFor: #colorKey:!public! !
!View categoriesFor: #colorKey:alphaBlend:!public! !
!View categoriesFor: #isLayered!public! !
!View categoriesFor: #isLayered:!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

