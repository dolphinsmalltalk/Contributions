| package |
package := Package name: 'US Wallpaper'.
package paxVersion: 1;
	basicComment: '$id: US Wallpaper 1.106$, $date: 24.07.2009$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Get and set the current Windows wallpaper

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicPackageVersion: '1.106'.

package basicScriptAt: #preinstall put: 'Win32Constants
	at: #SPI_GETDESKWALLPAPER put: 16r73;
	at: #SPI_SETDESKWALLPAPER put: 16r0014!!'.

package methodNames
	add: #DesktopView -> #wallpaper;
	add: #DesktopView -> #wallpaper:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!DesktopView methodsFor!

wallpaper
	| path |
	path := String new: 1024.
	(UserLibrary default 
		systemParametersInfo: SPI_GETDESKWALLPAPER
		uiParam: 1024
		pvParam: path
		fWinIni: 0) ifFalse: [UserLibrary default systemError].
	^path trimNulls!

wallpaper: aPath 
	(UserLibrary default 
		systemParametersInfo: SPI_SETDESKWALLPAPER
		uiParam: 0
		pvParam: aPath
		fWinIni: 0) ifFalse: [UserLibrary default systemError]! !
!DesktopView categoriesFor: #wallpaper!*-unreferenced selectors!public! !
!DesktopView categoriesFor: #wallpaper:!*-unreferenced selectors!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

