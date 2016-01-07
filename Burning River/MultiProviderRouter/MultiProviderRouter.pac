| package |
package := Package name: 'MultiProviderRouter'.
package paxVersion: 0;
	basicComment: 'Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.


package classNames
	add: #MprConstants;
	add: #MprLibrary;
	add: #NETRESOURCE;
	add: #WinNetResource;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\ConstantsClassGenerator\ConstantsClassGenerator';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #MprConstants
	instanceVariableNames: ''
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WinNetResource
	instanceVariableNames: 'scope type displayType usage localName remoteName comment provider'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #MprLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Win32Structure subclass: #NETRESOURCE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

MprConstants guid: (GUID fromString: '{2B837871-5592-11D3-8269-00001D19F5C2}')!
MprConstants comment: ''!
!MprConstants categoriesForClass!No category! !
!MprConstants methodsFor!

conndlgConnPoint
	^16r00000002!

conndlgHideBox
	^16r00000008!

conndlgNotPersist
	^16r00000020!

conndlgPersist
	^16r00000010!

conndlgRoPath
	^16r00000001!

conndlgUseMru
	^16r00000004!

connectCurrentMedia
	^16r00000200!

connectDeferred
	^16r00000400!

connectInteractive
	^16r00000008!

connectLocaldrive
	^16r00000100!

connectNeedDrive
	^16r00000020!

connectPrompt
	^16r00000010!

connectRedirect
	^16r00000080!

connectRefcount
	^16r00000040!

connectReserved
	^16rFF000000!

connectTemporary
	^16r00000004!

connectUpdateProfile
	^16r00000001!

connectUpdateRecent
	^16r00000002!

discNoForce
	^16r00000040!

discUpdateProfile
	^16r00000001!

netinfoDiskred
	^16r00000004!

netinfoDll16
	^16r00000001!

netinfoPrinterred
	^16r00000008!

netpropertyPersistent
	^1!

ppDisplayerrors
	^16r01!

remoteNameInfoLevel
	^16r00000002!

resourceConnected
	^16r00000001!

resourceContext
	^16r00000005!

resourcedisplaytypeDirectory
	^16r00000009!

resourcedisplaytypeDomain
	^16r00000001!

resourcedisplaytypeFile
	^16r00000004!

resourcedisplaytypeGeneric
	^16r00000000!

resourcedisplaytypeGroup
	^16r00000005!

resourcedisplaytypeNdscontainer
	^16r0000000B!

resourcedisplaytypeNetwork
	^16r00000006!

resourcedisplaytypeRoot
	^16r00000007!

resourcedisplaytypeServer
	^16r00000002!

resourcedisplaytypeShare
	^16r00000003!

resourcedisplaytypeShareadmin
	^16r00000008!

resourcedisplaytypeTree
	^16r0000000A!

resourceGlobalnet
	^16r00000002!

resourceRecent
	^16r00000004!

resourceRemembered
	^16r00000003!

resourcetypeAny
	^16r00000000!

resourcetypeDisk
	^16r00000001!

resourcetypePrint
	^16r00000002!

resourcetypeReserved
	^16r00000008!

resourcetypeUnknown
	^16rFFFFFFFF!

resourceusageAll
	^self resourceusageConnectable +
	 self resourceusageContainer +
	 self resourceusageAttached!

resourceusageAttached
	^16r00000010!

resourceusageConnectable
	^16r00000001!

resourceusageContainer
	^16r00000002!

resourceusageNolocaldevice
	^16r00000004!

resourceusageReserved
	^16r80000000!

resourceusageSibling
	^16r00000008!

rpInifile
	^16r02!

rpLogon
	^16r01!

universalNameInfoLevel
	^16r00000001!

wnconDynamic
	^16r00000008!

wnconFornetcard
	^16r00000001!

wnconNotrouted
	^16r00000002!

wnconSlowlink
	^16r00000004!

wnfmtAbbreviated
	^16r02!

wnfmtConnection
	^16r20!

wnfmtInenum
	^16r10!

wnfmtMultiline
	^16r01!

wnncNet10net
	^16r00050000!

wnncNet9tiles
	^16r00090000!

wnncNetAppletalk
	^16r00130000!

wnncNetAs400
	^16r000B0000!

wnncNetBwnfs
	^16r00100000!

wnncNetClearcase
	^16r00160000!

wnncNetCogent
	^16r00110000!

wnncNetFarallon
	^16r00120000!

wnncNetFtpNfs
	^16r000C0000!

wnncNetIntergraph
	^16r00140000!

wnncNetLanman
	^16r00020000!

wnncNetLanstep
	^16r00080000!

wnncNetLantastic
	^16r000A0000!

wnncNetLifenet
	^16r000E0000!

wnncNetLocus
	^16r00060000!

wnncNetMsnet
	^16r00010000!

wnncNetNetware
	^16r00030000!

wnncNetPathworks
	^16r000D0000!

wnncNetPowerlan
	^16r000F0000!

wnncNetSunPcNfs
	^16r00070000!

wnncNetSymfonet
	^16r00150000!

wnncNetVines
	^16r00040000! !
!MprConstants categoriesFor: #conndlgConnPoint!public! !
!MprConstants categoriesFor: #conndlgHideBox!public! !
!MprConstants categoriesFor: #conndlgNotPersist!public! !
!MprConstants categoriesFor: #conndlgPersist!public! !
!MprConstants categoriesFor: #conndlgRoPath!public! !
!MprConstants categoriesFor: #conndlgUseMru!public! !
!MprConstants categoriesFor: #connectCurrentMedia!public! !
!MprConstants categoriesFor: #connectDeferred!public! !
!MprConstants categoriesFor: #connectInteractive!public! !
!MprConstants categoriesFor: #connectLocaldrive!public! !
!MprConstants categoriesFor: #connectNeedDrive!public! !
!MprConstants categoriesFor: #connectPrompt!public! !
!MprConstants categoriesFor: #connectRedirect!public! !
!MprConstants categoriesFor: #connectRefcount!public! !
!MprConstants categoriesFor: #connectReserved!public! !
!MprConstants categoriesFor: #connectTemporary!public! !
!MprConstants categoriesFor: #connectUpdateProfile!public! !
!MprConstants categoriesFor: #connectUpdateRecent!public! !
!MprConstants categoriesFor: #discNoForce!public! !
!MprConstants categoriesFor: #discUpdateProfile!public! !
!MprConstants categoriesFor: #netinfoDiskred!public! !
!MprConstants categoriesFor: #netinfoDll16!public! !
!MprConstants categoriesFor: #netinfoPrinterred!public! !
!MprConstants categoriesFor: #netpropertyPersistent!public! !
!MprConstants categoriesFor: #ppDisplayerrors!public! !
!MprConstants categoriesFor: #remoteNameInfoLevel!public! !
!MprConstants categoriesFor: #resourceConnected!public! !
!MprConstants categoriesFor: #resourceContext!public! !
!MprConstants categoriesFor: #resourcedisplaytypeDirectory!public! !
!MprConstants categoriesFor: #resourcedisplaytypeDomain!public! !
!MprConstants categoriesFor: #resourcedisplaytypeFile!public! !
!MprConstants categoriesFor: #resourcedisplaytypeGeneric!public! !
!MprConstants categoriesFor: #resourcedisplaytypeGroup!public! !
!MprConstants categoriesFor: #resourcedisplaytypeNdscontainer!public! !
!MprConstants categoriesFor: #resourcedisplaytypeNetwork!public! !
!MprConstants categoriesFor: #resourcedisplaytypeRoot!public! !
!MprConstants categoriesFor: #resourcedisplaytypeServer!public! !
!MprConstants categoriesFor: #resourcedisplaytypeShare!public! !
!MprConstants categoriesFor: #resourcedisplaytypeShareadmin!public! !
!MprConstants categoriesFor: #resourcedisplaytypeTree!public! !
!MprConstants categoriesFor: #resourceGlobalnet!public! !
!MprConstants categoriesFor: #resourceRecent!public! !
!MprConstants categoriesFor: #resourceRemembered!public! !
!MprConstants categoriesFor: #resourcetypeAny!public! !
!MprConstants categoriesFor: #resourcetypeDisk!public! !
!MprConstants categoriesFor: #resourcetypePrint!public! !
!MprConstants categoriesFor: #resourcetypeReserved!public! !
!MprConstants categoriesFor: #resourcetypeUnknown!public! !
!MprConstants categoriesFor: #resourceusageAll!public! !
!MprConstants categoriesFor: #resourceusageAttached!public! !
!MprConstants categoriesFor: #resourceusageConnectable!public! !
!MprConstants categoriesFor: #resourceusageContainer!public! !
!MprConstants categoriesFor: #resourceusageNolocaldevice!public! !
!MprConstants categoriesFor: #resourceusageReserved!public! !
!MprConstants categoriesFor: #resourceusageSibling!public! !
!MprConstants categoriesFor: #rpInifile!public! !
!MprConstants categoriesFor: #rpLogon!public! !
!MprConstants categoriesFor: #universalNameInfoLevel!public! !
!MprConstants categoriesFor: #wnconDynamic!public! !
!MprConstants categoriesFor: #wnconFornetcard!public! !
!MprConstants categoriesFor: #wnconNotrouted!public! !
!MprConstants categoriesFor: #wnconSlowlink!public! !
!MprConstants categoriesFor: #wnfmtAbbreviated!public! !
!MprConstants categoriesFor: #wnfmtConnection!public! !
!MprConstants categoriesFor: #wnfmtInenum!public! !
!MprConstants categoriesFor: #wnfmtMultiline!public! !
!MprConstants categoriesFor: #wnncNet10net!public! !
!MprConstants categoriesFor: #wnncNet9tiles!public! !
!MprConstants categoriesFor: #wnncNetAppletalk!public! !
!MprConstants categoriesFor: #wnncNetAs400!public! !
!MprConstants categoriesFor: #wnncNetBwnfs!public! !
!MprConstants categoriesFor: #wnncNetClearcase!public! !
!MprConstants categoriesFor: #wnncNetCogent!public! !
!MprConstants categoriesFor: #wnncNetFarallon!public! !
!MprConstants categoriesFor: #wnncNetFtpNfs!public! !
!MprConstants categoriesFor: #wnncNetIntergraph!public! !
!MprConstants categoriesFor: #wnncNetLanman!public! !
!MprConstants categoriesFor: #wnncNetLanstep!public! !
!MprConstants categoriesFor: #wnncNetLantastic!public! !
!MprConstants categoriesFor: #wnncNetLifenet!public! !
!MprConstants categoriesFor: #wnncNetLocus!public! !
!MprConstants categoriesFor: #wnncNetMsnet!public! !
!MprConstants categoriesFor: #wnncNetNetware!public! !
!MprConstants categoriesFor: #wnncNetPathworks!public! !
!MprConstants categoriesFor: #wnncNetPowerlan!public! !
!MprConstants categoriesFor: #wnncNetSunPcNfs!public! !
!MprConstants categoriesFor: #wnncNetSymfonet!public! !
!MprConstants categoriesFor: #wnncNetVines!public! !

!MprConstants class methodsFor!

current
	Current isNil ifTrue: [ Current := self new ].
	^Current! !
!MprConstants class categoriesFor: #current!public! !

WinNetResource guid: (GUID fromString: '{2B837872-5592-11D3-8269-00001D19F5C2}')!
WinNetResource comment: 'This is a Smalltalk-ized version of the Windows NETRESOURCE structure.  The NETRESOURCE has pointers in it which point to strings in dynamically allocated storage which are overwritten when another NETRESOURCE is fetched, thus making the data difficult to work with, at best.  This class is a way to make the data more persistent and easier to deal with.'!
!WinNetResource categoriesForClass!No category! !
!WinNetResource methodsFor!

asNETRESOURCE
	"Create a NETRESOURCE object which duplicates the data in this structure"

	^NETRESOURCE new
		dwScope: self scope;
		dwType: self type;
		dwDisplayType: self displayType;
		dwUsage: self usage;
		lpLocalName: self localName;
		lpRemoteName: self remoteName;
		lpComment: self comment;
		lpProvider: self provider;
		yourself!

comment
	"Answer the value of the receiver's instance variable comment.
	This method was automatically generated, but may be modified."

	^comment!

comment: anObject
	"Set the value of the receiver's instance variable comment to anObject.
	This method was automatically generated, but may be modified."

	comment := anObject!

displayType
	"Answer the value of the receiver's instance variable displayType.
	This method was automatically generated, but may be modified."

	^displayType!

displayType: anObject
	"Set the value of the receiver's instance variable displayType to anObject.
	This method was automatically generated, but may be modified."

	displayType := anObject!

localName
	"Answer the value of the receiver's instance variable localName.
	This method was automatically generated, but may be modified."

	^localName!

localName: anObject
	"Set the value of the receiver's instance variable localName to anObject.
	This method was automatically generated, but may be modified."

	localName := anObject!

provider
	"Answer the value of the receiver's instance variable provider.
	This method was automatically generated, but may be modified."

	^provider!

provider: anObject
	"Set the value of the receiver's instance variable provider to anObject.
	This method was automatically generated, but may be modified."

	provider := anObject!

remoteName
	"Answer the value of the receiver's instance variable remoteName.
	This method was automatically generated, but may be modified."

	^remoteName!

remoteName: anObject
	"Set the value of the receiver's instance variable remoteName to anObject.
	This method was automatically generated, but may be modified."

	remoteName := anObject!

scope
	"Answer the value of the receiver's instance variable scope.
	This method was automatically generated, but may be modified."

	^scope!

scope: anObject
	"Set the value of the receiver's instance variable scope to anObject.
	This method was automatically generated, but may be modified."

	scope := anObject!

type
	"Answer the value of the receiver's instance variable type.
	This method was automatically generated, but may be modified."

	^type!

type: anObject
	"Set the value of the receiver's instance variable type to anObject.
	This method was automatically generated, but may be modified."

	type := anObject!

usage
	"Answer the value of the receiver's instance variable usage.
	This method was automatically generated, but may be modified."

	^usage!

usage: anObject
	"Set the value of the receiver's instance variable usage to anObject.
	This method was automatically generated, but may be modified."

	usage := anObject! !
!WinNetResource categoriesFor: #asNETRESOURCE!public! !
!WinNetResource categoriesFor: #comment!accessing!public! !
!WinNetResource categoriesFor: #comment:!accessing!public! !
!WinNetResource categoriesFor: #displayType!accessing!public! !
!WinNetResource categoriesFor: #displayType:!accessing!public! !
!WinNetResource categoriesFor: #localName!accessing!public! !
!WinNetResource categoriesFor: #localName:!accessing!public! !
!WinNetResource categoriesFor: #provider!accessing!public! !
!WinNetResource categoriesFor: #provider:!accessing!public! !
!WinNetResource categoriesFor: #remoteName!accessing!public! !
!WinNetResource categoriesFor: #remoteName:!accessing!public! !
!WinNetResource categoriesFor: #scope!accessing!public! !
!WinNetResource categoriesFor: #scope:!accessing!public! !
!WinNetResource categoriesFor: #type!accessing!public! !
!WinNetResource categoriesFor: #type:!accessing!public! !
!WinNetResource categoriesFor: #usage!accessing!public! !
!WinNetResource categoriesFor: #usage:!accessing!public! !

MprLibrary guid: (GUID fromString: '{2B837873-5592-11D3-8269-00001D19F5C2}')!
MprLibrary comment: ''!
!MprLibrary categoriesForClass!No category! !
!MprLibrary methodsFor!

allDomainsFor: aNetworkNameString
	"Answer a collection of domain names which are part of the network name specified by aString"

	^self allForProvider: aNetworkNameString!

allForProvider: aProviderNameString
	"Answer a collection of all names on the provider name specified."

	| netResource |

	netResource := NETRESOURCE new lpProvider: aProviderNameString.
	^self enumerate: netResource.!

allForRemoteName: aString
	"Answer a collection of all names on the remote name specified in aString."

	| netResource |

	netResource := NETRESOURCE new lpRemoteName: aString.
	^self enumerate: netResource!

allNetworks
	"Answer a collection of available network names"

	^self allForProvider: ''!

allServersFor: aDomainNameString
	"Answer a collection of all servers on the domain name specified"

	^self allForRemoteName: aDomainNameString!

enumerate: aNetresource
	"Perform an enumeration for the information provided in aNetresource, answering a collection of
	 WinNetResource objects."

	| result enumHandle buffer bufSize bufCount
	  resultResource errorCode errorBuf nameBuf errResult resultCollection |

	resultCollection := OrderedCollection new.
	enumHandle := ExternalHandle new.
	resultResource := NETRESOURCE new.

	result := MprLibrary default wNetOpenEnum: MprConstants current resourceGlobalnet
							dwType: MprConstants current resourcetypeAny
							dwUsage: MprConstants current resourceusageAll
							netResource: aNetresource
							handle: enumHandle.
	result = 0 ifTrue:
		[ [ result = 0 ] whileTrue:
			[ buffer := ExternalMemory newFixed: 16384.
			bufSize := DWORD new value: buffer size.
			bufCount := DWORD new value: 1.
			result := MprLibrary default wNetEnumResource: enumHandle
									lpcCount: bufCount
									lpBuffer: buffer
									lpBufferSize: bufSize.
			result = 0 ifTrue:
				[ resultResource replaceFrom: 1 to: resultResource size with: buffer startingAt: 1.
				resultCollection add: resultResource asWinNetResource ] ].

		MprLibrary default wNetCloseEnum: enumHandle ].

	^resultCollection!

wNetCloseEnum: hEnum
	"DWORD WNetCloseEnum( 
			HANDLE hEnum 	// handle to enumeration
			);"

	<stdcall: dword WNetCloseEnum dword>
	^self invalidCall!

wNetEnumResource: hEnum lpcCount: lpcCount lpBuffer: lpBuffer lpBufferSize: lpBufferSize
	"DWORD WNetEnumResource( 
					HANDLE hEnum, 	// handle to enumeration 	
					LPDWORD lpcCount, 	// pointer to entries to list 	
					LPVOID lpBuffer, 	// pointer to buffer for results 	
					LPDWORD lpBufferSize 	// pointer to buffer size variable 	
					);"

	<stdcall: dword WNetEnumResourceA dword lpvoid lpvoid lpvoid>
	^self invalidCall!

wNetGetLastError: error errorBuf: lpErrorBuf errorBufSize: lpErrorBufSize nameBuf: lpNameBuf nameBufSize: lpNameBufSize
	"DWORD APIENTRY WNetGetLastErrorA(
					LPDWORD lpError,
					LPSTR lpErrorBuf,
					DWORD nErrorBufSize,
					LPSTR	lpNameBuf,
					DWORD nNameBufSize);"
	<stdcall: dword WNetGetLastErrorA lpvoid lpvoid DWORD lpvoid DWORD>
	^self invalidCall!

wNetOpenEnum: dwScope dwType: dwType dwUsage: dwUsage netResource: aNETRESOURCE handle: anExternalHandle
	"DWORD WNetOpenEnum( 
				DWORD dwScope, 	// scope of enumeration 	
				DWORD dwType, 	// resource types to list 	
				DWORD dwUsage, 	// resource usage to list 	
				LPNETRESOURCE lpNetResource, 	// pointer to resource structure 	
				LPHANDLE lphEnum 	// pointer to enumeration handle buffer 	
				);"

	<stdcall: dword WNetOpenEnumA dword dword dword lpvoid lpvoid>
	^self invalidCall! !
!MprLibrary categoriesFor: #allDomainsFor:!public! !
!MprLibrary categoriesFor: #allForProvider:!public! !
!MprLibrary categoriesFor: #allForRemoteName:!public! !
!MprLibrary categoriesFor: #allNetworks!public! !
!MprLibrary categoriesFor: #allServersFor:!public! !
!MprLibrary categoriesFor: #enumerate:!public! !
!MprLibrary categoriesFor: #wNetCloseEnum:!public! !
!MprLibrary categoriesFor: #wNetEnumResource:lpcCount:lpBuffer:lpBufferSize:!public! !
!MprLibrary categoriesFor: #wNetGetLastError:errorBuf:errorBufSize:nameBuf:nameBufSize:!public! !
!MprLibrary categoriesFor: #wNetOpenEnum:dwType:dwUsage:netResource:handle:!public! !

!MprLibrary class methodsFor!

fileName
	^'MPR'!

generateConstants
	"Generate a Smalltalk class containing methods for constants defined in WINNETWK.H"

	ConstantsClassGenerator new
		generateClass: 'MprConstants'
		from: (Array with: 'D:\Program Files\DevStudio\VC\include\Winnetwk.h')!

test: aString
	"Test this library.  aString should be the name of a machine on the network preceded
	by two backslashes, e.g. \\MYMACHINE"

	| netResource enumHandle result resultResource buffer bufSize bufCount
	errorCode errorBuf nameBuf errResult |

	netResource := NETRESOURCE new.
	enumHandle := ExternalHandle new.
	resultResource := NETRESOURCE new.

	netResource lpRemoteName: aString.

	result := MprLibrary default wNetOpenEnum: MprConstants current resourceGlobalnet
							dwType: MprConstants current resourcetypeAny
							dwUsage: 0	"All resources"
							netResource: netResource
							handle: enumHandle.

	result = 0 ifTrue: [
		[ (result = 0) ] whileTrue: [
			buffer := ExternalMemory newFixed: 16384.
			bufSize := DWORD new value: buffer size.
			bufCount := DWORD new value: 1.
			result := MprLibrary default wNetEnumResource: enumHandle
									lpcCount: bufCount
									lpBuffer: buffer
									lpBufferSize: bufSize.
			result = 0
				ifTrue: [ resultResource replaceFrom: 1 to: resultResource size with: buffer startingAt: 1 ]
				ifFalse:
					[ errorCode := DWORD new.
					errorBuf := ExternalMemory newFixed: 1024.
					nameBuf := ExternalMemory newFixed: 1024.
					errResult := MprLibrary default wNetGetLastError: errorCode
										errorBuf: errorBuf
										errorBufSize: (DWORD fromInteger: errorBuf size)
										nameBuf: nameBuf
										nameBufSize: (DWORD fromInteger: nameBuf size) ] ].
		MprLibrary default wNetCloseEnum: enumHandle ]! !
!MprLibrary class categoriesFor: #fileName!public! !
!MprLibrary class categoriesFor: #generateConstants!public! !
!MprLibrary class categoriesFor: #test:!public! !

NETRESOURCE guid: (GUID fromString: '{2B837874-5592-11D3-8269-00001D19F5C2}')!
NETRESOURCE comment: ''!
!NETRESOURCE categoriesForClass!No category! !
!NETRESOURCE methodsFor!

asWinNetResource
	"Create a WinNetResource object which duplicates the data in this structure"

	^WinNetResource new
		scope: self dwScope;
		type: self dwType;
		displayType: self dwDisplayType;
		usage: self dwUsage;
		localName: self lpLocalName;
		remoteName: self lpRemoteName;
		comment: self lpComment;
		provider: self lpProvider;
		yourself!

dwDisplayType
	"Answer the receiver's dwDisplayType field as a Smalltalk object.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	^(bytes dwordAtOffset: 8)!

dwDisplayType: anObject
	"Set the receiver's dwDisplayType field to the value of anObject.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	bytes dwordAtOffset: 8 put: anObject!

dwScope
	"Answer the receiver's dwScope field as a Smalltalk object.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	^(bytes dwordAtOffset: 0)!

dwScope: anObject
	"Set the receiver's dwScope field to the value of anObject.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	bytes dwordAtOffset: 0 put: anObject!

dwType
	"Answer the receiver's dwType field as a Smalltalk object.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	^(bytes dwordAtOffset: 4)!

dwType: anObject
	"Set the receiver's dwType field to the value of anObject.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	bytes dwordAtOffset: 4 put: anObject!

dwUsage
	"Answer the receiver's dwUsage field as a Smalltalk object.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	^(bytes dwordAtOffset: 12)!

dwUsage: anObject
	"Set the receiver's dwUsage field to the value of anObject.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	bytes dwordAtOffset: 12 put: anObject!

lpComment
	"Answer the receiver's lpComment field as a Smalltalk object.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	^String fromAddress: (bytes sdwordAtOffset: 24)!

lpComment: anObject
	"Set the receiver's lpComment field to the value of anObject.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	bytes dwordAtOffset: 24 put: anObject yourAddress!

lpLocalName
	"Answer the receiver's lpLocalName field as a Smalltalk object.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	^String fromAddress: (bytes sdwordAtOffset: 16)!

lpLocalName: anObject
	"Set the receiver's lpLocalName field to the value of anObject.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	bytes dwordAtOffset: 16 put: anObject yourAddress!

lpProvider
	"Answer the receiver's lpProvider field as a Smalltalk object.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	^String fromAddress: (bytes sdwordAtOffset: 28)!

lpProvider: anObject
	"Set the receiver's lpProvider field to the value of anObject.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	bytes dwordAtOffset: 28 put: anObject yourAddress!

lpRemoteName
	"Answer the receiver's lpRemoteName field as a Smalltalk object.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	^String fromAddress: (bytes sdwordAtOffset: 20)!

lpRemoteName: anObject
	"Set the receiver's lpRemoteName field to the value of anObject.
	This method has been automatically generated from the class' structure template.
	Any modifications you make will be lost the next time it is so generated."

	bytes dwordAtOffset: 20 put: anObject yourAddress! !
!NETRESOURCE categoriesFor: #asWinNetResource!public! !
!NETRESOURCE categoriesFor: #dwDisplayType!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #dwDisplayType:!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #dwScope!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #dwScope:!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #dwType!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #dwType:!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #dwUsage!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #dwUsage:!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #lpComment!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #lpComment:!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #lpLocalName!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #lpLocalName:!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #lpProvider!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #lpProvider:!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #lpRemoteName!**compiled accessors**!public! !
!NETRESOURCE categoriesFor: #lpRemoteName:!**compiled accessors**!public! !

!NETRESOURCE class methodsFor!

defineFields
	"Define the fields of the Win32 NETRESOURCE structure.

		NETRESOURCE compileDefinition

	typedef struct _NETRESOURCE
		{ // nr
		DWORD dwScope;
		DWORD dwType;
		DWORD dwDisplayType;
		DWORD dwUsage;
		LPTSTR lpLocalName;
		LPTSTR lpRemoteName;
		LPTSTR lpComment;
		LPTSTR lpProvider; } NETRESOURCE; "

	self
		defineField: #dwScope type: DWORDField new;
		defineField: #dwType type: DWORDField new;
		defineField: #dwDisplayType type: DWORDField new;
		defineField: #dwUsage type: DWORDField new;
		defineField: #lpLocalName type: (PointerField type: String);
		defineField: #lpRemoteName type: (PointerField type: String);
		defineField: #lpComment type: (PointerField type: String);
		defineField: #lpProvider type: (PointerField type: String)! !
!NETRESOURCE class categoriesFor: #defineFields!public! !

"Binary Globals"!

"Resources"!

