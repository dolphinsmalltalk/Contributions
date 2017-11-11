"This is how the  "...W" wide API function calls and redirectors were initially created via automated transformation.
This code has already been run and applied to the classes in the Unicode UI pacakge.  It does not need to be re-run
unless regeneration of Unicode function calls is needed. This is included for informational purposes."

uct := MitSciUnicodeTransformer new.
uct scanForAnsiAPIFunctions.

uct addUnicodeMethodsFrom: UserLibrary to: UserLibraryW.
uct addUnicodeMethodsFrom: GDILibrary to: GDILibraryW.
uct addUnicodeMethodsFrom: ODBCLibrary to: ODBCLibraryW forFunctionNames: uct odbcUnicodeFunctionNames.
