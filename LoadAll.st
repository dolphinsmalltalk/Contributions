"This fileIn can be used to load all of the public Contributions packages. It is a requirement that all the
packages listed here must coexist and must pass all image tests."

| packages |

"Add packages to the list below. Filenames are relative to the directory containing this file and
will default to PAX extension if necessary."
 packages := #(
		'Burning River\Complex\Complex'
		'Burning River\Complex\ComplexTest'
		).

"Install all the above packages assuming paths are relative to the current directory."
packages do: [:each | Package manager installHere: each].

"Should you need to, the following code can be evaluated to remove all the loaded packages (ignoring any errors).
packages do: [:each | | package |
	package := Package manager packageNamed: (File splitStemFrom: each) ifNone: [].
	package ifNotNil: [
		Package manager uninstall: package]]
"
