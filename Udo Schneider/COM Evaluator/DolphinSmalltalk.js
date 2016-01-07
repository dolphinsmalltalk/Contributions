module_name = "Dolphin Smalltalk Helper";
module_ver = "0.001a";

function dolphin(){
	return CreateObject("Dolphin.Evaluator");
}

function activeEditor() {
	var editor = newEditor();
	editor.assignActiveEditor();
	return editor;
}

function selectedExpression(){
	editor = activeEditor();
	var selectedExpression = editor.selText();
	if(selectedExpression.length == 0)
		var selectedExpression = editor.lineText();
	return selectedExpression;
}

function Init(){
	var menuName = "Dolphin Smalltalk";
	addMenuItem("&Display It", menuName, "Display", "CTRL+D");
	addMenuItem("E&valuate It", menuName, "Evaluate", "CTRL+E");
	addMenuItem("&Inspect It", menuName, "Inspect", "CTRL+I");
	addMenuItem("Deb&ug It", menuName, "Debug", "F11");
	addMenuItem("Brow&se It", menuName, "Browse", "CTRL+B");
	addMenuItem("Re&format It", menuName, "Reformat", "");
	addMenuItem("-", menuName, "");
	addMenuItem("Definitions of ...", menuName, "Definitions");
	addMenuItem("References to ...", menuName, "References");
	addMenuItem("-", menuName, "");
	addMenuItem("Search", menuName, "Search");
	addMenuItem("Search containing text", menuName, "SearchMethodContaining");
}

function Browse() {
	dolphin().browse(selectedExpression());
}

function Display() {
	var editor = activeEditor();
	var result = dolphin().evaluate(selectedExpression());
	editor.command("ecLineEnd");
	var posX = editor.caretX();
	editor.selText(result);
	editor.setCaretPos(posX,-1);
	editor.command("ecSelLineEnd");
}

function Evaluate() {
	dolphin().evaluate(selectedExpression());
}

function Inspect() {
	dolphin().evaluate("(" + selectedExpression() + ") inspect");
}

function Debug() {
	dolphin().debug(selectedExpression());
}

function Search() {
	dolphin().search(selectedExpression());
}

function SearchMethodContaining() {
	dolphin().searchMethodContaining(selectedExpression());
}

function Definitions() {
	dolphin().definitions(selectedExpression());
}

function References() {
	dolphin().references(selectedExpression());
}

function Reformat() {
	var result = dolphin().reformat(activeEditor().selText());
	activeEditor().selText(result);
}

