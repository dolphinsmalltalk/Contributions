| package |
package := Package name: 'US COM Extensions'.
package paxVersion: 1;
	basicComment: '$id: US COM Extensions 0.007$
$for: Dolphin Smalltalk X6.1 Beta 2$

(c) $date: 24.07.2009$, $developer: udos@udos-laptop$ <Udo.Schneider@homeaddress.de>
Public Domain, Freeware

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'.

package basicPackageVersion: '0.007'.


package methodNames
	add: #IDispatch -> #asArray;
	add: #IDispatch -> #asDictionary;
	add: #IDispatch -> #items;
	add: #IDispatch -> #names;
	add: #IDispatch -> #values;
	add: 'COMInterface class' -> #compileFunctionsIntoImplementor:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\ActiveX\Automation\ActiveX Automation';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!COMInterface class methodsFor!

compileFunctionsIntoImplementor: aClass 
	"Helper to compile a set of method templates for the receiver's interface
	into the implementing class, aClass. These methods are of the form:

		<selector_arglist>
			"

	"Implement the <interface_name>::<function_name> function."

	"

			^E_NOTIMPL

		IEnumXXXX compileFunctionsIntoImplementor: EnumRECT
	"

	| aStream cat |
	cat := self generatedFunctionCategories first.
	((aClass includesCategory: cat) and: 
			[(MessageBox 
				confirm: ('<1p> already includes the method category ''<2s>''
The generated template methods may overwrite existing methods in the class.
Are you sure you want to proceed?' 
						expandMacrosWith: aClass
						with: cat asString)) 
					not]) 
		ifTrue: [^self].
	aStream := (String new: 256) writeStream.
	functions 
		from: superclass functions size + 1
		to: functions size
		do: 
			[:fn | 
			aStream
				nextPutAll: fn messageSelectorAndArgumentNames;
				cr;
				tab;
				nextPutAll: '"Implement the ';
				nextPutAll: self name;
				nextPutAll: '::';
				nextPutAll: (fn selector upTo: $:);
				nextPutAll: '() interface function."';
				cr;
				cr;
				tab;
				nextPutAll: 'Trace nextPutAll: ''';
				nextPutAll: self name;
				nextPutAll: '::';
				nextPutAll: (fn selector upTo: $:);
				nextPutAll: '''; flush.';
				cr;
				nextPutAll: '^E_NOTIMPL.';
				cr;
				tab;
				nextPutAll: '#todo "Implement me"';
				cr.
			aClass compile: aStream contents categories: self generatedFunctionCategories.
			aStream reset]! !
!COMInterface class categoriesFor: #compileFunctionsIntoImplementor:!automatic generation!development!public! !

!IDispatch methodsFor!

asArray
^self items!

asDictionary
|dic |
dic := Dictionary new.
(1 to: self count) do: [ :index | dic at: (self name: index) put: (self value: index)].
^dic!

items
	
	^(1 to: self count) collect: [:index | self item: index]!

names
	self assert: [self isVBCollection].
	^(1 to: self count) collect: [:index | self name: index]!

values
	self assert: [self isVBCollection].
	^(1 to: self count) collect: [:index | self value: index]! !
!IDispatch categoriesFor: #asArray!public! !
!IDispatch categoriesFor: #asDictionary!public! !
!IDispatch categoriesFor: #items!public! !
!IDispatch categoriesFor: #names!public! !
!IDispatch categoriesFor: #values!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

