| package |
package := Package name: 'US YouTubeVideoView'.
package paxVersion: 1;
	basicComment: '$id: US YouTubeVideoView 0.004$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.004'.


package classNames
	add: #YouTubeVideo;
	yourself.

package globalNames
	add: #YouTubeConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'US ShockwaveFlash Wrapper';
	yourself).

package!

"Class Definitions"!

ActionScriptBridge subclass: #YouTubeVideo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'YouTubeConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

Smalltalk at: #YouTubeConstants put: (PoolConstantsDictionary named: #YouTubeConstants)!
YouTubeConstants at: 'YTBUFFERING' put: 16r3!
YouTubeConstants at: 'YTCUED' put: 16r5!
YouTubeConstants at: 'YTENDED' put: 16r0!
YouTubeConstants at: 'YTPAUSED' put: 16r2!
YouTubeConstants at: 'YTPLAYING' put: 16r1!
YouTubeConstants at: 'YTUNSTARTED' put: -16r1!
YouTubeConstants shrink!

"Classes"!

YouTubeVideo guid: (GUID fromString: '{52872DBA-2006-44A6-98A7-D26B2AE400E3}')!
YouTubeVideo comment: 'http://code.google.com/apis/youtube/js_api_reference.html'!
!YouTubeVideo categoriesForClass!MVP-Views! !
!YouTubeVideo methodsFor!

addEventListener: event listener: listener
"addEventListener(event:String, listener):Void

	Adds a listener function for the specified event. The listener is a string of a function
	name if you are using the JavaScript API and is a reference to a function if you are using the ActionScript API."
	^self invokeFlash: 'addEventListener' with: event with: listener!

clearVideo
	"clearVideo():Void

	Clears the video display. This function is useful if you want to clear the video remnant after calling stopVideo()."

	self invokeFlash: 'clearVideo'!

cueVideoById: videoId startSeconds: startSeconds
"cueVideoById(videoId:String, startSeconds:Number):Void

	Loads the specified video's thumbnail and prepares the player to play the video.
	The player does not request the FLV until playVideo() or seekTo() is called.
	startSeconds accepts a float/integer and specifies the time from which the video
	should start playing when playVideo() is called. If you specify startSeconds and
	then call seekTo(), the startSeconds is value is ignored and the player plays from
	the time specified in the seekTo() call. When the video is cued and ready to play,
	the player will broadcast a video cued event (5)."
	self invokeFlash: 'cueVideoById' with: videoId with: startSeconds!

defaultMovie
	^'http://www.youtube.com/apiplayer?enablejsapi=1'!

getCurrentTime "getCurrentTime():Number

	Returns the elapsed time in seconds since the video started playing."
	^self invokeFlash: 'getCurrentTime'!

getDuration
"getDuration():Number

	Returns the duration in seconds of the currently playing video. Note that getDuration() will return
	0 until the video's metadata is loaded, which normally happens just after the video starts playing."
	^self invokeFlash: 'getDuration'!

getPlayerState
"getPlayerState():Number

	Returns the state of the player. Possible values are unstarted (-1), ended (0), playing (1), paused (2), buffering (3), video cued (5). "
	^self invokeFlash: 'getPlayerState'!

getVideoBytesLoaded
	"getVideoBytesLoaded():Number

	Returns the number of bytes loaded for the current video."

	^self invokeFlash: 'getVideoBytesLoaded'!

getVideoBytesTotal
"getVideoBytesTotal():Number
	
	Returns the size in bytes of the currently loaded/playing video."
	^self invokeFlash: 'getVideoBytesTotal'!

getVideoEmbedCode
"getVideoEmbedCode():String

	Returns the embed code for the currently loaded/playing video."
	^self invokeFlash: 'getVideoEmbedCode'!

getVideoStartBytes"player.getVideoStartBytes():Number

	Returns the number of bytes the video file started loading from. Example scenario:
	the user seeks ahead to a point that hasn't loaded yet, and the player makes a new
	request to play a segment of the video that hasn't loaded yet."
	^self invokeFlash: 'getVideoStartBytes'!

getVideoUrl
"getVideoUrl():String

	Returns the YouTube.com URL for the currently loaded/playing video."
	^self invokeFlash: 'getVideoUrl'!

getVolume
	"getVolume():Number

	Returns the player's current volume, an integer between 0 and 100. Note that getVolume() will return the volume even if the player is muted."

	^self invokeFlash: 'getVolume'!

isBuffering
	^self getPlayerState = YTBUFFERING!

isCued
	^self getPlayerState = YTCUED!

isEnded
	^self getPlayerState = YTENDED!

isMuted
"isMuted():Boolean

	Returns true if the player is muted, false if not."
	^self invokeFlash: 'isMuted'!

isMuted: aBoolean
aBoolean ifTrue: [self mute ] ifFalse: [self unMute ]!

isPaused
	^self getPlayerState = YTPAUSED!

isPlaying
	^self getPlayerState = YTPLAYING!

isUnstarted
	^self getPlayerState = YTUNSTARTED!

loadVideoById: videoId startSeconds: startSeconds 
	"loadVideoById(videoId:String, startSeconds:Number):Void

	Load and plays the specified video. If startSeconds (number can be a float) is specified, the video will start from the closest keyframe to the specified time."

self invokeFlash: 'loadVideoById' with: videoId with: startSeconds!

mute
"mute():Void

	Mutes the player."
	self invokeFlash: 'mute'!

onStateChange: newState 
	| selector |
	selector := ##((Dictionary new)
				at: YTUNSTARTED put: #onUnstarted;
				at: YTENDED put: #onEnded;
				at: YTPLAYING put: #onPlaying;
				at: YTPAUSED put: #onPaused;
				at: YTBUFFERING put: #onBuffering;
				at: YTCUED put: #onCued;
				yourself) at: newState ifAbsent: [^self].
	self  trigger: selector!

onYouTubePlayerReady: playerId 
	
	self addEventListener: 'onStateChange' listener: 'onStateChange:'!

pauseVideo
	"pauseVideo():Void
	
	Pauses the currently playing video."

	self invokeFlash: 'pauseVideo'!

playVideo
	"playVideo():Void
	
	Plays the currently cued/loaded video."

	self invokeFlash: 'playVideo'!

seekTo: seconds allowSeekAhead: allowSeekAhead 
	"seekTo(seconds, allowSeekAhead):Void

	Seeks to the specified time of the video in seconds. The allowSeekAhead determines
	whether or not the player will make a new request to the server if seconds is beyond the
	currently loaded video data. Note that seekTo() will look for the closest keyframe before
	the seconds specified. This means that sometimes the play head may seek to just before
	the requested time, usually no more than ~2 seconds."

	self 
		invokeFlash: 'seekTo'
		with: seconds
		with: allowSeekAhead.
!

setSize: width height: height
"setSize(width:Number, height:Number):Void

	Sets the size in pixels of the player. You should not have to use this method in
	JavaScript as the player will automatically resize when the containing elements
	in the embed code have their height and width properties modified."
	self invokeFlash: 'setSize' with: width with: height!

setVolume: volume "setVolume(volume):Void

	Sets the volume. Accepts an integer between 0 and 100."
	self invokeFlash: 'setVolume' with: volume!

stopVideo
	"stopVideo():Void
	
	Stops the current video. This function also closes the NetStream object
	and cancels the loading of the video. Once stopVideo() is called, a video
	cannot be resumed without reloading the player or loading a new video 
	chromeless player only). When calling stopVideo(), the player broadcasts an end event (0)."

	self invokeFlash: 'stopVideo'!

unMute
"unMute():Void

	Unmutes the player."
	self invokeFlash: 'unMute'!

volume
^self getVolume!

volume: volume 
self setVolume: volume! !
!YouTubeVideo categoriesFor: #addEventListener:listener:!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #clearVideo!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #cueVideoById:startSeconds:!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #defaultMovie!constants!public! !
!YouTubeVideo categoriesFor: #getCurrentTime!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #getDuration!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #getPlayerState!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #getVideoBytesLoaded!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #getVideoBytesTotal!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #getVideoEmbedCode!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #getVideoStartBytes!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #getVideoUrl!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #getVolume!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #isBuffering!ExternalInterface-Wrappers!public! !
!YouTubeVideo categoriesFor: #isCued!ExternalInterface-Wrappers!public! !
!YouTubeVideo categoriesFor: #isEnded!ExternalInterface-Wrappers!public! !
!YouTubeVideo categoriesFor: #isMuted!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #isMuted:!ExternalInterface-Wrappers!public! !
!YouTubeVideo categoriesFor: #isPaused!ExternalInterface-Wrappers!public! !
!YouTubeVideo categoriesFor: #isPlaying!ExternalInterface-Wrappers!public! !
!YouTubeVideo categoriesFor: #isUnstarted!ExternalInterface-Wrappers!public! !
!YouTubeVideo categoriesFor: #loadVideoById:startSeconds:!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #mute!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #onStateChange:!event handlers!ExternalInterface-Callbacks!public! !
!YouTubeVideo categoriesFor: #onYouTubePlayerReady:!event handlers!ExternalInterface-Callbacks!public! !
!YouTubeVideo categoriesFor: #pauseVideo!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #playVideo!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #seekTo:allowSeekAhead:!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #setSize:height:!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #setVolume:!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #stopVideo!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #unMute!ExternalInterface-Functions!public! !
!YouTubeVideo categoriesFor: #volume!ExternalInterface-Wrappers!public! !
!YouTubeVideo categoriesFor: #volume:!ExternalInterface-Wrappers!public! !

!YouTubeVideo class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\youtube.ico'! !
!YouTubeVideo class categoriesFor: #icon!public! !

"Binary Globals"!

