"Filed out from Dolphin Smalltalk 7"!

UserLibrary subclass: #UserLibraryW
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UserLibraryW guid: (GUID fromString: '{eefe281e-06e5-4441-b216-28acf93afa3e}')!
UserLibraryW comment: ''!
!UserLibraryW categoriesForClass!External-Libraries-Win32! !
!UserLibraryW methodsFor!

callWindowProc: lpPrevWndFunc hWnd: hWnd msg: msg wParam: wParam lParam: lParam

^self callWindowProcW: lpPrevWndFunc hWnd: hWnd msg: msg wParam: wParam lParam: lParam!

callWindowProcW: lpPrevWndFunc hWnd: hWnd msg: msg wParam: wParam lParam: lParam
	"Pass a message to the specified window procedure. 
		LRESULT CallWindowProc(
  			WNDPROC lpPrevWndFunc,	// pointer to previous procedure
			HWND hWnd,			// handle to window
			UINT Msg,				// message
			WPARAM wParam,			// first message parameter
			LPARAM lParam 			// second message parameter
		);
	N.B. As we don't want to pass the address of a Smalltalk object containing
	the WNDPROC's address to the function, we specify the first parameter
	as being a DWORD as this allows us to pass a wider range of types."

	<stdcall: uintptr CallWindowProcW dword handle dword uintptr uintptr>
	^self invalidCall!

charLower: aCharacter

^self charLowerW: aCharacter!

charLowerW: aCharacter
	"Answer the lowercase equivalent of aCharacter. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharLower(LPTSTR  lpsz); 	// single character or pointer to string"

	<stdcall: char CharLowerW char>
	^self invalidCall!

charUpper: aCharacter

^self charUpperW: aCharacter!

charUpperW: aCharacter
	"Answer the uppercase equivalent of aCharacter. This will be dependent on the semantics 
	of the language selected by the user during setup or by using Control Panel.

		LPTSTR CharUpper(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: char CharUpperW char>
	^self invalidCall!

createAcceleratorTable: pTable cEntries: tableSize

^self createAcceleratorTableW: pTable cEntries: tableSize!

createAcceleratorTableW: pTable cEntries: tableSize
	"Create an accelerator table. 
		HACCEL CreateAcceleratorTable(
  			LPACCEL lpaccl,	// pointer to structure array with accelerator data
			int cEntries 		// number of structures in the array
		);"

	<stdcall: handle CreateAcceleratorTableW lpvoid sdword>
	^self invalidCall!

createDialog: hInstance lpTemplate: template hWndParent: hParent lpDialogFunc: dlgProc dwInitParam: lParam

^self createDialogW: hInstance lpTemplate: template hWndParent: hParent lpDialogFunc: dlgProc dwInitParam: lParam!

createDialogW: hInstance lpTemplate: template hWndParent: hParent lpDialogFunc: dlgProc dwInitParam: lParam
	"Create a modeless dialog box from a dialog box template resource. 
		HWND CreateDialog(
  			HINSTANCE hInstance,		// handle of module containing template
			LPCTSTR lpTemplate,		// resource identifier
			HWND hWndParent,			// handle of owner window
			DLGPROC lpDialogFunc, 		// address of dialog proc
			LPARAM dwInitParam 		// value passed to wmInitDialog:
		);"

	<stdcall: sdword CreateDialogParamW handle lpvoid handle lpvoid sdword>
	^self invalidCall!

createWindowEx: exstyle lpClassName: classname lpWindowName: windowname dwStyle: style x: x y: y nWidth: width nHeight: height hWndParent: parent hMenu: menu hInstance: instance lpParam: lpParam

^self createWindowExW: exstyle lpClassName: classname lpWindowName: windowname dwStyle: style x: x y: y nWidth: width nHeight: height hWndParent: parent hMenu: menu hInstance: instance lpParam: lpParam!

createWindowExW: exstyle lpClassName: classname lpWindowName: windowname dwStyle: style 
		x: x y: y nWidth: width nHeight: height 
		hWndParent: parent hMenu: menu hInstance: instance lpParam: lpParam
	"Creates an overlapped, pop-up, or child window with the specified the parent or owner (if any), class, 
	title, menu, style (and extended style), position and extent. Answer the handle of the
	new window, or nil if the create fails.
	N.B. In order to correctly subclass controls so that Dolphin receive's creation messages
	for those controls, View>>hookWindowCreate should be called before this function.
		HWND CreateWindowEx(
			DWORD dwExStyle,		// extended window style
			LPCTSTR lpClassName,	// pointer to registered class name
			LPCTSTR lpWindowName,	// pointer to window name
			DWORD dwStyle,			// window style
			int x,				// horizontal position of window
			int y,				// vertical position of window
			int nWidth,			// window width
			int nHeight,			// window height
			HWND hWndParent,		// handle to parent or owner window
			HMENU hMenu,			// handle to menu, or child-window identifier
			HINSTANCE hInstance,	// handle to application instance
			LPVOID lpParam 		// pointer to window-creation data
		);"

	<stdcall: handle CreateWindowExW dword lpvoid lpvoid dword sdword sdword sdword sdword handle handle handle lpvoid>
	^self invalidCall
!

defDlgProc: hWnd msg: msg wParam: wParam lParam: lParam

^self defDlgProcW: hWnd msg: msg wParam: wParam lParam: lParam!

defDlgProcW: hWnd msg: msg wParam: wParam lParam: lParam
	"Call the default dialog procedure to provide default processing for 
	any window messages that an application does not process. 

		LRESULT DefDlgProc(
			HWND hDlg,	// handle to dialog box
			UINT Msg,	// message
			WPARAM wParam,	// first message parameter
			LPARAM lParam 	// second message parameter
		);"


	<stdcall: uintptr DefDlgProcW handle dword uintptr uintptr>
	^self invalidCall!

defWindowProc: hWnd msg: msg wParam: wParam lParam: lParam

^self defWindowProcW: hWnd msg: msg wParam: wParam lParam: lParam!

defWindowProcW: hWnd msg: msg wParam: wParam lParam: lParam
	"Call the default window procedure to provide default processing for 
	any window messages that an application does not process. 
	
		LRESULT DefWindowProc(
			HWND hWnd,	// handle to window
			UINT Msg,	// message identifier
			WPARAM wParam,	// first message parameter
			LPARAM lParam 	// second message parameter
		);"

	<stdcall: uintptr DefWindowProcW handle dword uintptr uintptr>
	^self invalidCall!

dispatchMessage: aMSG

^self dispatchMessageW: aMSG!

dispatchMessageW: aMSG
	"Dispatch a message to a window procedure.
		LONG DispatchMessage(
  			CONST MSG *lpmsg 	// pointer to structure with message
		);"

	<stdcall: sdword DispatchMessageW MSG* >
	^self invalidCall!

drawState: hdc hbr: hbr lpOutputFunc: lpOutputFunc lData: lData wData: wData x: x y: y cx: cx cy: cy fuFlags: fuFlags

^self drawStateW: hdc hbr: hbr lpOutputFunc: lpOutputFunc lData: lData wData: wData x: x y: y cx: cx cy: cy fuFlags: fuFlags!

drawStateW: hdc hbr: hbr lpOutputFunc: lpOutputFunc lData: lData wData: wData x: x y: y cx: cx cy: cy fuFlags: fuFlags
	"Invoke the DrawState() function of the module wrapped by the receiver.
	Helpstring: Displays an image and applies a visual effect to indicate a state, such as a disabled or default state

		BOOL __stdcall DrawState(
			[in]HDC HDC,
			[in]HBRUSH hbr,
			[in]DRAWSTATEPROC lpOutputFunc,
			[in]LPARAM lData,
			[in]WPARAM wData,
			[in]int x,
			[in]int y,
			[in]int cx,
			[in]int cy,
			[in]unsigned int fuFlags);"

	<stdcall: bool DrawStateW handle handle void* intptr uintptr sdword sdword sdword sdword dword>
	^self invalidCall!

drawTextEx: hdc lpchText: lpchText cchText: cchText lprc: lprc dwDTFormat: dwDTFormat lpDTParams: lpdtParams

^self drawTextExW: hdc lpchText: lpchText cchText: cchText lprc: lprc dwDTFormat: dwDTFormat lpDTParams: lpdtParams!

drawTextExW: hdc lpchText: lpchText cchText: cchText lprc: lprc dwDTFormat: dwDTFormat lpDTParams: lpdtParams
	"Draw text in the specified rectange with the specified options (see Win32 docs)

		int DrawTextEx(
			HDC hdc,			 						// handle to device context
			LPTSTR lpchText,							// pointer to string to draw 
			int cchText,								// length of string to draw 
			LPRECT lprc,								// pointer to rectangle coordinates 
			UINT dwDTFormat,						// formatting options 
			LPDRAWTEXTPARAMS lpDTParams	// pointer to struct with options 
		);
	" 
	<stdcall: sdword DrawTextExW handle lpstr sdword RECT* dword lpvoid>
	^self invalidCall!

findWindow: lpClassName lpWindowName: lpWindowName

^self findWindowW: lpClassName lpWindowName: lpWindowName!

findWindowW: lpClassName lpWindowName: lpWindowName
	"Find and answer the handle of the top-level window with matching class name and caption."

	<stdcall: handle FindWindowW lpstr lpstr>
	^self invalidCall!

getClassName: aWindowHandle lpClassName: aByteBuffer nMaxCount: anInteger

^self getClassNameW: aWindowHandle lpClassName: aByteBuffer nMaxCount: anInteger!

getClassNameW: aWindowHandle lpClassName: aByteBuffer nMaxCount: anInteger
	"Retrieves the name of the Windows class to which the specified window belongs.
		int GetClassName(
			HWND hWnd,			// handle of window
			LPTSTR lpClassName,	// address of buffer for class name
			int nMaxCount 			// size of buffer, in characters
		);"

	<stdcall: sdword GetClassNameW handle lpvoid sdword>
	^self invalidCall!

getClipboardFormatName: format lpszFormatName: lpszFormatName cchMaxCount: cchMaxCount

^self getClipboardFormatNameW: format lpszFormatName: lpszFormatName cchMaxCount: cchMaxCount!

getClipboardFormatNameW: format lpszFormatName: lpszFormatName cchMaxCount: cchMaxCount
	"Copy the name of the specified registered clipboard format into the
	supplied buffer.

		int GetClipboardFormatName( 
			UINT format,				// clipboard format to retrieve 
			LPTSTR lpszFormatName, 	// address of buffer for name 
			int cchMaxCount  			// length of name string in characters 
		);"

	<stdcall: sdword GetClipboardFormatNameW dword lpstr sdword>
	^self invalidCall!

getKeyNameText: lParam lpString: aString nSize: anInteger

^self getKeyNameTextW: lParam lpString: aString nSize: anInteger!

getKeyNameTextW: lParam lpString: aString nSize: anInteger
	"Populate a string with the name of a virtual key code
		int GetKeyNameText( 
			LONG lParam,	// second parameter of keyboard message	
			LPTSTR lpString,	// address of buffer for key name	
			int nSize 	// maximum length of key-name string length	
		);"

	<stdcall: sdword GetKeyNameTextW dword lpstr sdword>
	^self invalidCall
!

getMessage: aMSG hWnd: aWindowHandle wMsgFilterMin: anIntFilterMin wMsgFilterMax: anIntFilterMax

^self getMessageW: aMSG hWnd: aWindowHandle wMsgFilterMin: anIntFilterMin wMsgFilterMax: anIntFilterMax!

getMessageW: aMSG hWnd: aWindowHandle wMsgFilterMin: anIntFilterMin wMsgFilterMax: anIntFilterMax
	"Retrieves the next message from the Win32 input queue in the range integerFilterMin
	integerFilterMax, for the window with handle aWindowHandle (if Null then for all windows)
	into the Win32 MSG structure, aMSG.

	BOOL GetMessage(
		LPMSG  lpMsg,	// address of structure with message
		HWND  hWnd,	// handle of window
		UINT  wMsgFilterMin,	// first message
		UINT  wMsgFilterMax 	// last message
	);	

	Answers 1 if a message was retrieved, 0 if WM_QUIT was retrieved, or -1 if some
	error occurred. N.B. Though Win32 declares GetMessage() as returning a BOOL,
	it doesn't really (because of the error return value) so to avoid errors, as
	suggested in the help), we have it return a signed integer)"

	<stdcall: sdword GetMessageW MSG* handle dword dword>
	^self invalidCall!

getProp: aWindowHandle lpString: name

^self getPropW: aWindowHandle lpString: name!

getPropW: aWindowHandle lpString: name
	"Answer a data handle which is the previously added (with #setProp:etc) named property 
	of the given window.
		HANDLE GetProp(
			HWND hWnd,	// handle of window
			LPCTSTR lpString 	// atom or address of string
		);.

	N.B. The return specification HANDLE here indicates that the value is an opaque 32-bit
	(DWORD) value, so we use #dword, as we reserve #handle for real handles."

	<stdcall: dword GetPropW handle lpvoid>
	^self invalidCall

!

getWindowLong: aWindowHandle nIndex: zeroBasedIntegerOffset

^self getWindowLongW: aWindowHandle nIndex: zeroBasedIntegerOffset!

getWindowLongPtr: aWindowHandle nIndex: zeroBasedIntegerOffset

^self getWindowLongPtrW: aWindowHandle nIndex: zeroBasedIntegerOffset!

getWindowLongPtrW: aWindowHandle nIndex: zeroBasedIntegerOffset 
	"Answer various signed intptr values retrieved from the window with handle, aWindowHandle.
	If an unsigned value is required, use getWindowULongPtr:nIndex:. Note that this function
	is only supported on 64-bit Windows.

		LONG_PTR GetWindowLongPtr(
			HWND hWnd, // handle of window 
			int nIndex // offset of value to retrieve 
		);

	Valid offsets are in the range 0 to the number of bytes of extra window memory, minus the
	size of an intptr, or one of the following values: 

		GWL_EXSTYLE Extended window styles (WS_EX_XXX) 
		GWL_STYLE Window styles (WS_XXX) 
		GWLP_WNDPROC Address of the Window Procedure
		GWLP_HINSTANCE Handle of the application instance which owns the window. 
		GWLP_HWNDPARENT Handle of the parent window, if any. 
		GWLP_ID Identifier of the window. GWLP_USERDATA An inptr value associated with the window by the owning application.

	The following values are also available for dialog windows: 

		DWLP_DLGPROC Address of the dialog box procedure. 
		DWLP_MSGRESULT The return value of a message processed in the dialog	box procedure. 
		DWLP_USER Extra information private to the application.

	Answers 0 if the function fails."

	<stdcall: intptr GetWindowLongPtrW handle sdword>
	^self invalidCall!

getWindowLongW: aWindowHandle nIndex: zeroBasedIntegerOffset 
	"Answer various signed 32-bit integer values retrieved from the window with handle, aWindowHandle.
	If an unsigned value is required, use getWindowULong:nIndex:

		LONG GetWindowLong(
			HWND hWnd, // handle of window 
			int nIndex // offset of value to retrieve 
		);

	Valid offsets are in the range 0 to the number of bytes of extra window memory, minus the
	size of an int (4), or one of the following values: 

		GWL_EXSTYLE Extended window styles (WS_EX_XXX) 
		GWL_STYLE Window styles (WS_XXX) 

	Answers 0 if the function fails."

	<stdcall: sdword GetWindowLongW handle sdword>
	^self invalidCall!

getWindowText: aWindow lpString: aBuffer nMaxCount: len

^self getWindowTextW: aWindow lpString: aBuffer nMaxCount: len!

getWindowTextLength: hWnd

^self getWindowTextLengthW: hWnd!

getWindowTextLengthW: hWnd
	"Answer the length, in characters, of the specified window's title 'text'
		int GetWindowTextLength(
			HWND hWnd 	// handle of window or control with text
		);"

	<stdcall: sdword GetWindowTextLengthW handle>
	^self invalidCall
!

getWindowTextW: aWindow lpString: aBuffer nMaxCount: len
	"Copy the text of the specified window (its title bar, or other appropriate text)
	into the supplied buffer, up to the specified maximum length
		int GetWindowText(
			HWND hWnd,		// handle of window or control with text
			LPTSTR lpString,	// address of buffer for text
			int nMaxCount 		// maximum number of characters to copy
		);"

	<stdcall: sdword GetWindowTextW handle lpvoid sdword>
	^self invalidCall
!

getWindowULong: aWindowHandle nIndex: zeroBasedIntegerOffset

^self getWindowULongW: aWindowHandle nIndex: zeroBasedIntegerOffset!

getWindowULongPtr: aWindowHandle nIndex: zeroBasedIntegerOffset

^self getWindowULongPtrW: aWindowHandle nIndex: zeroBasedIntegerOffset!

getWindowULongPtrW: aWindowHandle nIndex: zeroBasedIntegerOffset 
	"Answer various unsigned values retrieved from the window with handle, aWindowHandle. See
	#getWindowLongPtr:offset: for more details"

	<stdcall: uintptr GetWindowLongPtrW handle sdword>
	^self invalidCall!

getWindowULongW: aWindowHandle nIndex: zeroBasedIntegerOffset 
	"Answer various unsigned 32-bit integer values retrieved from the window with handle,
	aWindowHandle. See #getWindowLongPtr:offset: for more details"

	<stdcall: dword GetWindowLongW handle sdword>
	^self invalidCall!

insertMenuItem: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMENUITEMINFOA

^self insertMenuItemW: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMENUITEMINFOA!

insertMenuItemW: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMENUITEMINFOA
	"Insert a new item into a menu.

		BOOL InsertMenuItem(
			HMENU hMenu,
			UINT uItem,
			BOOL fByPosition,
			LPCMENUITEMINFO lpmii
		);"

	<stdcall: bool InsertMenuItemW handle dword bool MENUITEMINFOA*>
	^self invalidCall!

isCharAlpha: aCharacter

^self isCharAlphaW: aCharacter!

isCharAlphaNumeric: aCharacter

^self isCharAlphaNumericW: aCharacter!

isCharAlphaNumericW: aCharacter
	"Answer whether a character is an alphabetic character or a digit. This will dependent on the 
	semantics of the language selected by the user during setup or by using Control Panel.
		BOOL IsCharAlphaNumeric(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharAlphaNumericW char>
	^self invalidCall!

isCharAlphaW: aCharacter
	"Answer whether a character is an alphabetic character. This will dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
		BOOL IsCharAlpha(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharAlphaW char>
	^self invalidCall!

isCharLower: aCharacter

^self isCharLowerW: aCharacter!

isCharLowerW: aCharacter
	"Answer whether a character is a lowercase letter. This will dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
		BOOL IsCharLower(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharLowerW char>
	^self invalidCall!

isCharLowerWW: aCharacter 
	"Answer whether a character is a lowercase letter. This will dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
		BOOL IsCharLower(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharLowerW dword>
	^self invalidCall!

isCharUpper: aCharacter

^self isCharUpperW: aCharacter!

isCharUpperW: aCharacter
	"Answer whether a character is an uppercase letter. This will dependent on the semantics of 
	the language selected by the user during setup or by using Control Panel.
		BOOL IsCharUpper(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharUpperW char>
	^self invalidCall!

isCharUpperWW: aCharacter 
	"Answer whether a character is an uppercase letter. This will dependent on the semantics of 
	the language selected by the user during setup or by using Control Panel.
		BOOL IsCharUpper(
			TCHAR  ch 	// character to test  
		);"

	<stdcall: bool IsCharUpperW dword>
	^self invalidCall!

isDialogMessage: aHandle lpMsg: aMSG

^self isDialogMessageW: aHandle lpMsg: aMSG!

isDialogMessageW: aHandle lpMsg: aMSG
	"Determine whether a message is intended for the specified dialog 
	box and, if it is, process the message. 
		BOOL IsDialogMessage(
  			HWND hDlg,	// handle of dialog box
			LPMSG lpMsg	// address of structure with message
		);"

	<stdcall: bool IsDialogMessageW handle MSG* >
	^self invalidCall!

loadAccelerators: anExternalHandle lpTableName: anIntegerOrString

^self loadAcceleratorsW: anExternalHandle lpTableName: anIntegerOrString!

loadAcceleratorsW: anExternalHandle lpTableName: anIntegerOrString
	"Load the specified accelerator table from the resources of the application identified
	by the instance handle, aHandle. The argument anIntegerOrString may be a 16-bit integer
	resource id, or a String name. Answer the handle of the resource, or nil if the function
	fails. Loaded accelerator tables will be freed automatically when Dolphin terminates.

		HACCEL LoadAccelerators(
				HINSTANCE  hInstance,	// handle of application instance
				LPCTSTR  lpTableName 	// address of table-name string
			);"

	<stdcall: handle LoadAcceleratorsW handle lpvoid>
	^self invalidCall!

loadCursor: anInstanceHandle lpCursorName: anIntegerID

^self loadCursorW: anInstanceHandle lpCursorName: anIntegerID!

loadCursorFromFile: aFilename

^self loadCursorFromFileW: aFilename!

loadCursorFromFileW: aFilename
	"Creates a cursor based on data contained in the specified file, and answer the handle, 
	or NULL if it fails. Can load standard (.CUR) or animated (.ANI) cursors.

		HCURSOR LoadCursorFromFile (
  			LPCTSTR  lpFileName	// pointer to name of cursor file, or system cursor identifier
	   );"

	<stdcall: handle LoadCursorFromFileW lpvoid>
	^self invalidCall!

loadCursorW: anInstanceHandle lpCursorName: anIntegerID
	"Loads the specified cursor resource from the module with the specified handle
	Under Win32 it is not necessary to destroy cursors loaded in this manner.

		HCURSOR LoadCursor(
  			HINSTANCE  hInstance,	// handle of application instance
			LPCTSTR  lpCursorName 	// name string or cursor resource identifier  
		);"

	<stdcall: handle LoadCursorW handle lpvoid>
	^self invalidCall!

loadIcon: anInstanceHandle lpIconName: anIntegerID

^self loadIconW: anInstanceHandle lpIconName: anIntegerID!

loadIconW: anInstanceHandle lpIconName: anIntegerID
	"Loads the specified icon resource from the specified module.
		HICON LoadIcon(
			HINSTANCE hInstance,	// handle of application instance
			LPCTSTR lpIconName 	// icon-name string or icon resource identifier
		);"

	<stdcall: handle LoadIconW handle lpvoid>
	^self invalidCall
!

loadImage: hInst lpszName: filename uType: type cxDesired: w cyDesired: h fuLoad: flags

^self loadImageW: hInst lpszName: filename uType: type cxDesired: w cyDesired: h fuLoad: flags!

loadImageW: hInst lpszName: filename uType: type cxDesired: w cyDesired: h fuLoad: flags
	"Load an icon, cursor, or bitmap.
		HANDLE LoadImage(
			HINSTANCE hinst, 	// handle of the instance that contains the image
			LPCTSTR lpszName,	// name or identifier of image
			UINT uType,		// type of image
			int cxDesired,		// desired width
			int cyDesired,		// desired height
			UINT fuLoad		// load flags
		);"

	<stdcall: handle LoadImageW handle lpvoid dword sdword sdword dword>
	^self invalidCall!

loadMenu: anExternalHandle lpMenuName: anIntegerOrString

^self loadMenuW: anExternalHandle lpMenuName: anIntegerOrString!

loadMenuW: anExternalHandle lpMenuName: anIntegerOrString
	"Load the specified menu from the resources of the application identified
	by the instance handle, aHandle. The argument anIntegerOrString may be a 16-bit integer
	resource id, or a String name. Answer the handle of the resource, or nil if the function
	fails.

		HMENU LoadMenu(
			HINSTANCE  hInstance,	// handle of application instance
			LPCTSTR  lpMenuName	// menu name string or menu-resource identifier  
			);"

	<stdcall: handle LoadMenuW handle lpvoid>
	^self invalidCall!

loadString: anExternalHandle uID: anIntegerID lpBuffer: aString nBufferMax: anIntegerLength

^self loadStringW: anExternalHandle uID: anIntegerID lpBuffer: aString nBufferMax: anIntegerLength!

loadStringW: anExternalHandle uID: anIntegerID lpBuffer: aString nBufferMax: anIntegerLength
	"Load a string resource from the executable file associated with 
	the specified module, into the specified buffer appending
	a terminating null character. Answer the length of the string, or 0 
	if it does not exist.
		int LoadString(
			HINSTANCE hInstance,	// handle of module containing string resource 
			UINT uID,				// resource identifier 
			LPTSTR lpBuffer,		// address of buffer for resource 
			int nBufferMax 		// size of buffer
		);"

	<stdcall: sdword LoadStringW handle dword lpstr sdword>
	^self invalidCall!

mapVirtualKey: keyCode uMapType: mapType

^self mapVirtualKeyW: keyCode uMapType: mapType!

mapVirtualKeyW: keyCode uMapType: mapType
	"Map a virtual-key code into a scan code or character value, or translate a scan code into a virtual-key code. 

	The Meaning of keycode and the result of the translation depends on the maptype:

		keyCode					Answer
	0	virtual key code		->	scan code
	1	scan code				->	virtual key code
	2	virtual key code		->	character value
	3	scan code				->	virtual key code	

		UINT	MapVirtualKey(
			UINT uCode,
			UINT uMapType);"

	<stdcall: dword MapVirtualKeyW dword dword>
	^self invalidCall!

messageBox: hWnd lpText: lpText lpCaption: lpCaption uType: uType

^self messageBoxW: hWnd lpText: lpText lpCaption: lpCaption uType: uType!

messageBoxIndirect: aMSGBOXPARAMS

^self messageBoxIndirectW: aMSGBOXPARAMS!

messageBoxIndirectW: aMSGBOXPARAMS
	"Open a message box with the details specified in the structure.

		int MessageBox(
			LPMSGBOXPARRAMS lpMsgBoxParams;
		);"

	<stdcall: sdword MessageBoxIndirectW MSGBOXPARAMS*>
	^self invalidCall!

messageBoxW: hWnd lpText: lpText lpCaption: lpCaption uType: uType
	<stdcall: sdword MessageBoxW handle lpstr lpstr dword>
	^self invalidCall!

modifyMenu: hMenuDrop uPosition: position uFlags: styleFlags uIDNewItem: menuId lpNewItem: menuText

^self modifyMenuW: hMenuDrop uPosition: position uFlags: styleFlags uIDNewItem: menuId lpNewItem: menuText!

modifyMenuW: hMenuDrop uPosition: position uFlags: styleFlags uIDNewItem: menuId lpNewItem: menuText
	"Changes an existing menu item the specified menu with the specified
	style, identifier and text.

		BOOL ModifyMenu(
			HMENU hMenu,	// handle to menu
			UINT uPosition,	// menu item to modify
			UINT uFlags,		// menu item flags
			UINT uIDNewItem,	// menu item identifier or pop-up menu handle 
			LPCTSTR lpNewItem	// menu item content
		);"

	<stdcall: bool ModifyMenuW handle dword dword dword lpstr>
	^self invalidCall!

open: aString

	handle  := self class superclass default handle.!

overlappedMsgBoxIndirect: aMSGBOXPARAMS

^self overlappedMsgBoxIndirectW: aMSGBOXPARAMS!

overlappedMsgBoxIndirectW: aMSGBOXPARAMS
	"Private - Open a message box with the details specified in the structure
	as an overlapped call (i.e. on a separate thread).
	N.B. Don't use for MB_TASKMODAL message boxes.

		int MessageBox(
			LPMSGBOXPARRAMS lpMsgBoxParams;
		);"

	<overlap stdcall: sdword MessageBoxIndirectW MSGBOXPARAMS* >
	^self invalidCall!

peekMessage: aMSG hWnd: aWindowHandle uMsgFilterMin: anIntFilterMin uMsgFilterMax: uMsgFilterMax wRemoveMsg: anIntFlags

^self peekMessageW: aMSG hWnd: aWindowHandle uMsgFilterMin: anIntFilterMin uMsgFilterMax: uMsgFilterMax wRemoveMsg: anIntFlags!

peekMessageW: aMSG hWnd: aWindowHandle uMsgFilterMin: anIntFilterMin uMsgFilterMax: uMsgFilterMax wRemoveMsg: anIntFlags
	"Check the threads message queue for messages for the Window with
	handle aWindowHandle (all windows if nil/0, app messages if -1) in the range
	anIntFilterMin..anIntFilterMax (all messages if 0..0), retrieving 
	the first (if any) of such messages into aMSG, answering whether 
	a message was found. The message is optionally removed from the 
	queue depending on the value of the argument, anIntFlags (PM_REMOVE/
	PM_NOREMOVE). The flag value PmNoYield is obsolete, and has no effect
	in Win32.

		BOOL PeekMessage(
			LPMSG  lpMsg,			// address of structure for message
			HWND  hWnd,			// handle of window
			UINT  uMsgFilterMin,	// first message
			UINT  uMsgFilterMax,	// last message
			UINT  wRemoveMsg 		// removal flags
		);"

	<stdcall: bool PeekMessageW MSG* handle dword dword dword>
	^self invalidCall!

postMessage: aWindowHandle msg: msg wParam: wParam lParam: lParam

^self postMessageW: aWindowHandle msg: msg wParam: wParam lParam: lParam!

postMessageW: aWindowHandle msg: msg wParam: wParam lParam: lParam
	"Post a message to aWindowHandle.

	LRESULT PostMessage(
		HWND hWnd, 		// handle of destination window
		UINT Msg, 		// message to send
		WPARAM wParam, 	// first message parameter
		LPARAM lParam 		// second message parameter
	   );"

	<stdcall: intptr PostMessageW handle dword uintptr intptr>
	^self invalidCall !

postThreadMessage: anIntegerId msg: msg wParam: wParam lParam: lParam

^self postThreadMessageW: anIntegerId msg: msg wParam: wParam lParam: lParam!

postThreadMessageW: anIntegerId msg: msg wParam: wParam lParam: lParam
	"Post a message to the message queue of the specified thread for asynchronous processing.
		BOOL PostThreadMessage(
  			DWORD  idThread,	// thread identifier
			UINT  Msg,		// message to post
			WPARAM  wParam,	// first message parameter
			LPARAM  lParam 	// second message parameter);"

	<stdcall: bool PostThreadMessageW handle dword uintptr uintptr>
	^self invalidCall !

registerClass: aWNDCLASS

^self registerClassW: aWNDCLASS!

registerClassW: aWNDCLASS
	"Registers a window class for subsequent use in calls to the CreateWindow or 
	CreateWindowEx functions. Answers the class atom, or zero if fails.
		ATOM RegisterClass(
			CONST WNDCLASS  *lpwc 	// address of structure with class data
		);"

	<stdcall: word RegisterClassW WNDCLASS* >
	^self invalidCall!

registerClipboardFormat: formatName

^self registerClipboardFormatW: formatName!

registerClipboardFormatW: formatName
	"Register a new clipboard format name, answering its system wide identifier.

		UINT RegisterClipboardFormat( 
			LPCTSTR lpszFormat  // address of name string 
		);"

	<stdcall: dword RegisterClipboardFormatW lpstr>
	^self invalidCall!

registerWindowMessage: aString

^self registerWindowMessageW: aString!

registerWindowMessageW: aString
	"Defines a new window message that is guaranteed to be unique system wide.

		UINT RegisterWindowMessage(
			LPCTSTR  lpString 	// address of message string
		);"

	<stdcall: dword RegisterWindowMessageW lpstr>
	^self invalidCall!

removeProp: aWindowHandle lpString: name

^self removePropW: aWindowHandle lpString: name!

removePropW: aWindowHandle lpString: name
	"Remove an the entry identified by lpString from the property list of 
	the specified window.
		HANDLE RemoveProp(
			HWND hWnd,	// handle to window
			LPCTSTR lpString 	// atom or address of string
		);"

	<stdcall: handle RemovePropW handle lpvoid>
	^self invalidCall

!

sendDlgItemMessage: aWindowHandle nIDDlgItem: anIntegerId msg: aString wParam: wParam lParam: lParam

^self sendDlgItemMessageW: aWindowHandle nIDDlgItem: anIntegerId msg: aString wParam: wParam lParam: lParam!

sendDlgItemMessageW: aWindowHandle nIDDlgItem: anIntegerId msg: aString wParam: wParam lParam: lParam
	"Send a message to the specified control in a dialog box. 
		LRESULT SendDlgItemMessage(
			HWND  hDlg,		// handle of dialog box
			int  nIDDlgItem,	// identifier of control
			UINT  Msg,		// message to send
			WPARAM  wParam,	// first message parameter
			LPARAM  lParam 	// second message parameter
		   );"

	<stdcall: uintptr SendDlgItemMessageW handle sdword dword uintptr intptr>
	^self invalidCall!

sendMessage: aWindowHandle msg: msg wParam: wParam lParam: lParam

^self sendMessageW: aWindowHandle msg: msg wParam: wParam lParam: lParam!

sendMessage: aWindowHandle msg: msg wParam: wParam lpParam: lParam


^self sendMessageW: aWindowHandle msg: msg wParam: wParam lpParam: (lParam class == String ifTrue: [lParam asUnicodeString ] ifFalse: [lParam ])!

sendMessage: aWindowHandle msg: msg wpParam: wParam lpParam: lParam

^self sendMessageW: aWindowHandle msg: msg wpParam: wParam lpParam: lParam!

sendMessageW: aWindowHandle msg: msg wParam: wParam lParam: lParam
	"The SendMessage function sends the specified message to a window or windows.
	The function calls the window procedure for the specified window and does not
	return until the window procedure has processed the message. The PostMessage
	function, in contrast, posts a message to a thread's message queue and returns
	immediately. 

	Implementation Note: Although LPARAM is defined as a signed parameter, more often
	than not it is used to pass an unsigned 32-bit value. As we have a strict definition
	of what can be passed to a signed integer parameter (i.e. only valid two's complement 
	values that fit within the required size), we used the slightly more relaxed unsigned 
	specification. This allows LargeIntegers > the maximum positive signed value for
	the machine word size to be passed.

	LRESULT SendMessage(
		HWND hWnd, 		// handle of destination window
		UINT Msg, 		// message to send
		WPARAM wParam, 	// first message parameter
		LPARAM lParam 		// second message parameter
	   );"

	<stdcall: intptr SendMessageW handle dword uintptr uintptr>
	^self invalidCall !

sendMessageW: aWindowHandle msg: msg wParam: wParam lpParam: lParam
	"As sendMessageW:msg:wParam:lParam, but implicit conversion of lParam
	to pointer."

	<stdcall: intptr SendMessageW handle dword uintptr lpvoid>
	^self invalidCall !

sendMessageW: aWindowHandle msg: msg wpParam: wParam lpParam: lParam
	"As sendMessageW:msg:wParam:lParam, but implicit conversion of lParam
	and wParam to pointers."

	<stdcall: intptr SendMessageW handle dword lpvoid lpvoid>
	^self invalidCall !

setClassLong: aWindowHandle nIndex: offset dwNewLong: value

^self setClassLongW: aWindowHandle nIndex: offset dwNewLong: value!

setClassLongW: aWindowHandle nIndex: offset dwNewLong: value
	"Change an attribute of the specified window class, setting 
	a signed 32-bit (long) value at the specified offset into 
	the extra window memory of a window.
	Note: This is declared as returning a DWORD.

		DWORD SetClassLong(
			HWND hWnd,		// handle of window
			int nIndex,		// offset of value to set
			LONG dwNewLong 	// new value
		);"

	<stdcall: dword SetClassLongW handle sdword sdword>
	^self invalidCall!

setDlgItemText: aWindowHandle nIDDlgItem: anIntegerId lpString: aString

^self setDlgItemTextW: aWindowHandle nIDDlgItem: anIntegerId lpString: aString!

setDlgItemTextW: aWindowHandle nIDDlgItem: anIntegerId lpString: aString
	"Sets the title or text of a control in a dialog box. 
		BOOL SetDlgItemText(
  			HWND  hDlg,		// handle of dialog box
			int  nIDDlgItem,	// identifier of control
			LPCTSTR  lpString 	// text to set
			);"

	<stdcall: bool SetDlgItemTextW handle sdword lpvoid>
	^self invalidCall!

setMenuItemInfo: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMenuItemInfo

^self setMenuItemInfoW: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMenuItemInfo!

setMenuItemInfoW: hMenu uItem: anInteger fByPosition: aBoolean lpmii: aMenuItemInfo
	"Set various information about a menu item.
		BOOL SetMenuItemInfo(
			HMENU hMenu,
			UINT uItem,
			BOOL fByPosition,
			LPMENUITEMINFO lpmii
		);"

	<stdcall: bool SetMenuItemInfoW handle dword bool MENUITEMINFOA*>
	^self invalidCall

!

setProp: aWindowHandle lpString: name hData: anObject

^self setPropW: aWindowHandle lpString: name hData: anObject!

setPropW: aWindowHandle lpString: name hData: anObject
	"Answer true if name and anObject are successfully added to the property list.
	The SetProp function adds a new entry or changes an existing entry in the property list of the specified window.
	The function adds a new entry to the list if the specified character string does not exist already in the list.
	The new entry contains the string and the handle. Otherwise, the function replaces the string's current handle with the specified handle. 
		
		BOOL SetProp(
			HWND  	hWnd,		// handle of window
			LPCTSTR  lpString,		// atom or address of string
			HANDLE  	hData 		// handle of data
		);

	Before destroying a window (that is, before processing the WM_DESTROY message), an application must remove all entries it has added
	to the property list. The application must use the RemoveProp function to remove the entries."

	<stdcall: bool SetPropW handle lpvoid handle>
	^self invalidCall

!

setWindowDWORD: aWindowHandle nIndex: offset dwNewDWORD: value

^self setWindowDWORDW: aWindowHandle nIndex: offset dwNewDWORD: value!

setWindowDWORDW: aWindowHandle nIndex: offset dwNewDWORD: value 
	"See #setWindowLong:nIndex:dwNewLong: value, but note this expects an unsigned 32-bit
	integer as its last argument, and always returns a positive integer constructed by treating
	the return value as unsigned."

	<stdcall: dword SetWindowLongW handle sdword dword>
	^self invalidCall!

setWindowLong: aWindowHandle nIndex: offset dwNewLong: value

^self setWindowLongW: aWindowHandle nIndex: offset dwNewLong: value!

setWindowLongW: aWindowHandle nIndex: offset dwNewLong: value
	"Change an attribute of the specified window, setting 
	a signed 32-bit (long) value at the specified offset into 
	the extra window memory of a window.
		LONG SetWindowLong(
			HWND hWnd,		// handle of window
			int nIndex,		// offset of value to set
			LONG dwNewLong 	// new value
		);"

	<stdcall: sdword SetWindowLongW handle sdword sdword>
	^self invalidCall!

setWindowText: aWindowHandle lpString: aString

^self setWindowTextW: aWindowHandle lpString: aString asUnicodeString!

setWindowTextW: aWindowHandle lpString: aString
	"Set the 'text' of the specified window.
		BOOL SetWindowText(
			HWND hWnd,	// handle of window or control
			LPCTSTR lpString 	// address of string
		);"

	<stdcall: bool SetWindowTextW handle lpvoid>
	^self invalidCall!

stringLower: aString

^self stringLowerW: aString!

stringLowerA: aString

^self stringLowerAW: aString!

stringLowerAW: aString 
	"Convert aString to lower case IN PLACE. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharLower(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: void CharLowerW lpvoid>
	^self invalidCall!

stringLowerW: aString
	"Convert aString to lower case IN PLACE. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharLower(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: void CharLowerW lpvoid>
	^self invalidCall!

stringUpper: aString

^self stringUpperW: aString!

stringUpperA: aString

^self stringUpperAW: aString!

stringUpperAW: aString 
	"Convert aString to uppercase IN PLACE. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharUpper(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: void CharUpperW lpvoid>
	^self invalidCall!

stringUpperW: aString
	"Convert aString to uppercase IN PLACE. This will be dependent on the semantics of the 
	language selected by the user during setup or by using Control Panel.
	N.B. We ignore the return value as it will be a pointer to the argument.

		LPTSTR CharUpper(LPTSTR  lpsz); 	// single character or pointer to string "

	<stdcall: void CharUpperW lpvoid>
	^self invalidCall!

systemParametersInfo: uiAction uiParam: param1 pvParam: param2 fWinIni: update

^self systemParametersInfoW: uiAction uiParam: param1 pvParam: param2 fWinIni: update!

systemParametersInfoW: uiAction uiParam: param1 pvParam: param2 fWinIni: update
	"
	BOOL SystemParametersInfo(
		UINT uiAction,	// system parameter to query or set
		UINT uiParam,	// depends on action to be taken
		PVOID pvParam,	// depends on action to be taken
		UINT fWinIni 	// user profile update flag
	);"

	<stdcall: bool SystemParametersInfoW dword dword lpvoid dword>
	^self invalidCall!

unregisterClass: classAtomOrStringName hInstance: instanceHandle

^self unregisterClassW: classAtomOrStringName hInstance: instanceHandle!

unregisterClassW: classAtomOrStringName hInstance: instanceHandle
	"Remove a window class.
		BOOL UnregisterClass(
			LPCTSTR lpClassName,	// address of class name string
			HINSTANCE hInstance 	// handle of application instance
		);"

	<stdcall: bool UnregisterClassW lpvoid handle>
	^self invalidCall!

vkKeyScan: aChar

^self vkKeyScanW: aChar!

vkKeyScanW: aChar
	"Translate a character to the corresponding virtual-key code 
	and shift state for the current keyboard.
		SHORT VkKeyScan(
			TCHAR ch 	// character to translate
	   );"

	<stdcall: sword VkKeyScanW char>
	^self invalidCall

!

winHelp: hwndMain lpszHelp: lpszHelp uCommand: uCommand dwData: dwData

^self winHelpW: hwndMain lpszHelp: lpszHelp uCommand: uCommand dwData: dwData!

winHelpW: hwndMain lpszHelp: lpszHelp uCommand: uCommand dwData: dwData
	"Invoke the WinHelp() function of the module wrapped by the receiver.
	Helpstring: Starts windows help and passes a request for help

		BOOL __stdcall WinHelp(
			HWND hwndMain,
			LPCSTR lpszHelp,
			unsigned int uCommand,
			ULONG_PTR dwData);"

	<stdcall: bool WinHelpW handle lpstr dword uintptr>
	^self invalidCall! !
!UserLibraryW categoriesFor: #callWindowProc:hWnd:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #callWindowProcW:hWnd:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #charLower:!public! !
!UserLibraryW categoriesFor: #charLowerW:!public! !
!UserLibraryW categoriesFor: #charUpper:!public! !
!UserLibraryW categoriesFor: #charUpperW:!public! !
!UserLibraryW categoriesFor: #createAcceleratorTable:cEntries:!public! !
!UserLibraryW categoriesFor: #createAcceleratorTableW:cEntries:!public! !
!UserLibraryW categoriesFor: #createDialog:lpTemplate:hWndParent:lpDialogFunc:dwInitParam:!public! !
!UserLibraryW categoriesFor: #createDialogW:lpTemplate:hWndParent:lpDialogFunc:dwInitParam:!public! !
!UserLibraryW categoriesFor: #createWindowEx:lpClassName:lpWindowName:dwStyle:x:y:nWidth:nHeight:hWndParent:hMenu:hInstance:lpParam:!public! !
!UserLibraryW categoriesFor: #createWindowExW:lpClassName:lpWindowName:dwStyle:x:y:nWidth:nHeight:hWndParent:hMenu:hInstance:lpParam:!public! !
!UserLibraryW categoriesFor: #defDlgProc:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #defDlgProcW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #defWindowProc:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #defWindowProcW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #dispatchMessage:!public! !
!UserLibraryW categoriesFor: #dispatchMessageW:!public! !
!UserLibraryW categoriesFor: #drawState:hbr:lpOutputFunc:lData:wData:x:y:cx:cy:fuFlags:!public! !
!UserLibraryW categoriesFor: #drawStateW:hbr:lpOutputFunc:lData:wData:x:y:cx:cy:fuFlags:!public! !
!UserLibraryW categoriesFor: #drawTextEx:lpchText:cchText:lprc:dwDTFormat:lpDTParams:!public! !
!UserLibraryW categoriesFor: #drawTextExW:lpchText:cchText:lprc:dwDTFormat:lpDTParams:!public! !
!UserLibraryW categoriesFor: #findWindow:lpWindowName:!public! !
!UserLibraryW categoriesFor: #findWindowW:lpWindowName:!public! !
!UserLibraryW categoriesFor: #getClassName:lpClassName:nMaxCount:!public! !
!UserLibraryW categoriesFor: #getClassNameW:lpClassName:nMaxCount:!public! !
!UserLibraryW categoriesFor: #getClipboardFormatName:lpszFormatName:cchMaxCount:!public! !
!UserLibraryW categoriesFor: #getClipboardFormatNameW:lpszFormatName:cchMaxCount:!public! !
!UserLibraryW categoriesFor: #getKeyNameText:lpString:nSize:!public! !
!UserLibraryW categoriesFor: #getKeyNameTextW:lpString:nSize:!public! !
!UserLibraryW categoriesFor: #getMessage:hWnd:wMsgFilterMin:wMsgFilterMax:!public! !
!UserLibraryW categoriesFor: #getMessageW:hWnd:wMsgFilterMin:wMsgFilterMax:!public! !
!UserLibraryW categoriesFor: #getProp:lpString:!public! !
!UserLibraryW categoriesFor: #getPropW:lpString:!public! !
!UserLibraryW categoriesFor: #getWindowLong:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowLongPtr:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowLongPtrW:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowLongW:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowText:lpString:nMaxCount:!public! !
!UserLibraryW categoriesFor: #getWindowTextLength:!public! !
!UserLibraryW categoriesFor: #getWindowTextLengthW:!public! !
!UserLibraryW categoriesFor: #getWindowTextW:lpString:nMaxCount:!public! !
!UserLibraryW categoriesFor: #getWindowULong:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowULongPtr:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowULongPtrW:nIndex:!public! !
!UserLibraryW categoriesFor: #getWindowULongW:nIndex:!public! !
!UserLibraryW categoriesFor: #insertMenuItem:uItem:fByPosition:lpmii:!public! !
!UserLibraryW categoriesFor: #insertMenuItemW:uItem:fByPosition:lpmii:!public! !
!UserLibraryW categoriesFor: #isCharAlpha:!public! !
!UserLibraryW categoriesFor: #isCharAlphaNumeric:!public! !
!UserLibraryW categoriesFor: #isCharAlphaNumericW:!public! !
!UserLibraryW categoriesFor: #isCharAlphaW:!public! !
!UserLibraryW categoriesFor: #isCharLower:!public! !
!UserLibraryW categoriesFor: #isCharLowerW:!public! !
!UserLibraryW categoriesFor: #isCharLowerWW:!public! !
!UserLibraryW categoriesFor: #isCharUpper:!public! !
!UserLibraryW categoriesFor: #isCharUpperW:!public! !
!UserLibraryW categoriesFor: #isCharUpperWW:!public! !
!UserLibraryW categoriesFor: #isDialogMessage:lpMsg:!public! !
!UserLibraryW categoriesFor: #isDialogMessageW:lpMsg:!public! !
!UserLibraryW categoriesFor: #loadAccelerators:lpTableName:!public! !
!UserLibraryW categoriesFor: #loadAcceleratorsW:lpTableName:!public! !
!UserLibraryW categoriesFor: #loadCursor:lpCursorName:!public! !
!UserLibraryW categoriesFor: #loadCursorFromFile:!public! !
!UserLibraryW categoriesFor: #loadCursorFromFileW:!public! !
!UserLibraryW categoriesFor: #loadCursorW:lpCursorName:!public! !
!UserLibraryW categoriesFor: #loadIcon:lpIconName:!public! !
!UserLibraryW categoriesFor: #loadIconW:lpIconName:!public! !
!UserLibraryW categoriesFor: #loadImage:lpszName:uType:cxDesired:cyDesired:fuLoad:!public! !
!UserLibraryW categoriesFor: #loadImageW:lpszName:uType:cxDesired:cyDesired:fuLoad:!public! !
!UserLibraryW categoriesFor: #loadMenu:lpMenuName:!public! !
!UserLibraryW categoriesFor: #loadMenuW:lpMenuName:!public! !
!UserLibraryW categoriesFor: #loadString:uID:lpBuffer:nBufferMax:!public! !
!UserLibraryW categoriesFor: #loadStringW:uID:lpBuffer:nBufferMax:!public! !
!UserLibraryW categoriesFor: #mapVirtualKey:uMapType:!public! !
!UserLibraryW categoriesFor: #mapVirtualKeyW:uMapType:!public! !
!UserLibraryW categoriesFor: #messageBox:lpText:lpCaption:uType:!public! !
!UserLibraryW categoriesFor: #messageBoxIndirect:!public! !
!UserLibraryW categoriesFor: #messageBoxIndirectW:!public! !
!UserLibraryW categoriesFor: #messageBoxW:lpText:lpCaption:uType:!public! !
!UserLibraryW categoriesFor: #modifyMenu:uPosition:uFlags:uIDNewItem:lpNewItem:!public! !
!UserLibraryW categoriesFor: #modifyMenuW:uPosition:uFlags:uIDNewItem:lpNewItem:!public! !
!UserLibraryW categoriesFor: #open:!public! !
!UserLibraryW categoriesFor: #overlappedMsgBoxIndirect:!public! !
!UserLibraryW categoriesFor: #overlappedMsgBoxIndirectW:!public! !
!UserLibraryW categoriesFor: #peekMessage:hWnd:uMsgFilterMin:uMsgFilterMax:wRemoveMsg:!public! !
!UserLibraryW categoriesFor: #peekMessageW:hWnd:uMsgFilterMin:uMsgFilterMax:wRemoveMsg:!public! !
!UserLibraryW categoriesFor: #postMessage:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #postMessageW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #postThreadMessage:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #postThreadMessageW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #registerClass:!public! !
!UserLibraryW categoriesFor: #registerClassW:!public! !
!UserLibraryW categoriesFor: #registerClipboardFormat:!public! !
!UserLibraryW categoriesFor: #registerClipboardFormatW:!public! !
!UserLibraryW categoriesFor: #registerWindowMessage:!public! !
!UserLibraryW categoriesFor: #registerWindowMessageW:!public! !
!UserLibraryW categoriesFor: #removeProp:lpString:!public! !
!UserLibraryW categoriesFor: #removePropW:lpString:!public! !
!UserLibraryW categoriesFor: #sendDlgItemMessage:nIDDlgItem:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #sendDlgItemMessageW:nIDDlgItem:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #sendMessage:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #sendMessage:msg:wParam:lpParam:!public! !
!UserLibraryW categoriesFor: #sendMessage:msg:wpParam:lpParam:!public! !
!UserLibraryW categoriesFor: #sendMessageW:msg:wParam:lParam:!public! !
!UserLibraryW categoriesFor: #sendMessageW:msg:wParam:lpParam:!public! !
!UserLibraryW categoriesFor: #sendMessageW:msg:wpParam:lpParam:!public! !
!UserLibraryW categoriesFor: #setClassLong:nIndex:dwNewLong:!public! !
!UserLibraryW categoriesFor: #setClassLongW:nIndex:dwNewLong:!public! !
!UserLibraryW categoriesFor: #setDlgItemText:nIDDlgItem:lpString:!public! !
!UserLibraryW categoriesFor: #setDlgItemTextW:nIDDlgItem:lpString:!public! !
!UserLibraryW categoriesFor: #setMenuItemInfo:uItem:fByPosition:lpmii:!public! !
!UserLibraryW categoriesFor: #setMenuItemInfoW:uItem:fByPosition:lpmii:!public! !
!UserLibraryW categoriesFor: #setProp:lpString:hData:!public! !
!UserLibraryW categoriesFor: #setPropW:lpString:hData:!public! !
!UserLibraryW categoriesFor: #setWindowDWORD:nIndex:dwNewDWORD:!public! !
!UserLibraryW categoriesFor: #setWindowDWORDW:nIndex:dwNewDWORD:!public! !
!UserLibraryW categoriesFor: #setWindowLong:nIndex:dwNewLong:!public! !
!UserLibraryW categoriesFor: #setWindowLongW:nIndex:dwNewLong:!public! !
!UserLibraryW categoriesFor: #setWindowText:lpString:!public! !
!UserLibraryW categoriesFor: #setWindowTextW:lpString:!public! !
!UserLibraryW categoriesFor: #stringLower:!public! !
!UserLibraryW categoriesFor: #stringLowerA:!public! !
!UserLibraryW categoriesFor: #stringLowerAW:!public! !
!UserLibraryW categoriesFor: #stringLowerW:!public! !
!UserLibraryW categoriesFor: #stringUpper:!public! !
!UserLibraryW categoriesFor: #stringUpperA:!public! !
!UserLibraryW categoriesFor: #stringUpperAW:!public! !
!UserLibraryW categoriesFor: #stringUpperW:!public! !
!UserLibraryW categoriesFor: #systemParametersInfo:uiParam:pvParam:fWinIni:!public! !
!UserLibraryW categoriesFor: #systemParametersInfoW:uiParam:pvParam:fWinIni:!public! !
!UserLibraryW categoriesFor: #unregisterClass:hInstance:!public! !
!UserLibraryW categoriesFor: #unregisterClassW:hInstance:!public! !
!UserLibraryW categoriesFor: #vkKeyScan:!public! !
!UserLibraryW categoriesFor: #vkKeyScanW:!public! !
!UserLibraryW categoriesFor: #winHelp:lpszHelp:uCommand:dwData:!public! !
!UserLibraryW categoriesFor: #winHelpW:lpszHelp:uCommand:dwData:!public! !

