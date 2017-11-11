"Filed out from Dolphin Smalltalk 7"!

WNDCLASS subclass: #WNDCLASSW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WNDCLASSW guid: (GUID fromString: '{a3f259b2-8c4d-4965-91b1-7ee32e6c5e72}')!
WNDCLASSW comment: ''!
!WNDCLASSW categoriesForClass!External-Data-Structured-Win32! !
!WNDCLASSW class methodsFor!

defineFields
	"Define the layout of the Win32 WNDCLASS structure. Currently to avoid
	wasting space, the structure is set up for registering classes only
	(i.e. the fields are fillers or write only). Furthermore it is not
	compiled at present.

	WNDCLASS compileDefinition

		typedef struct _WNDCLASS {
			UINT		style; 
			WNDPROC	lpfnWndProc; 
			int		cbClsExtra; 
			int		cbWndExtra; 
			HANDLE	hInstance; 
			HICON	hIcon; 
			HCURSOR	hCursor; 
			HBRUSH	hbrBackground; 
			LPCTSTR	lpszMenuName; 
			LPCTSTR	lpszClassName; 
		} WNDCLASS; "

	self 
		defineField: #style type: DWORDField writeOnly;
		defineField: #lpfnWndProc type: DWORDField writeOnly;
		defineField: #cbClsExtra type: SDWORDField filler;
		defineField: #cbWndExtra type: SDWORDField filler;
		defineField: #hInstance type: DWORDField writeOnly;
		defineField: #hIcon type: DWORDField writeOnly;
		defineField: #hCursor type: DWORDField writeOnly;
		defineField: #hbrBackground type: DWORDField writeOnly;
		defineField: #lpszMenuName type: (PointerField type: UnicodeString ) beWriteOnly;
		defineField: #lpszClassName type: (PointerField type: UnicodeString) beWriteOnly! !
!WNDCLASSW class categoriesFor: #defineFields!initializing!public! !

