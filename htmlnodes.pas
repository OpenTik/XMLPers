{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 - J. Aldo G. de Freitas Junior

Unit
	HTMLNodes;

Interface

Uses
	Classes,
	SysUtils,
	Tree,
	NameValue,
	StrUtils,
	XMLConsole,
	ExpressionParser;

Type
	EHTMLNode = Class(Exception);
	THTMLNode = Class;
	THTMLNodeClass = Class Of THTMLNode;

	THTMLNode = Class(TTreeNodeWithProperties)
	Private
		fRow,
		fCol : Integer;
		fConsole : TXMLCustomConsole;
		fVariables : TNameValue;
	Public
		Constructor Create(Const aOwner : TTreeNode); Override;
		Destructor Destroy; Override;
		Procedure PropagateConsole(Const aConsole : TXMLCustomConsole);
		Function ChildsAsString: String; Virtual;
		Function AsString: String; Virtual;
		Function ReplaceVars(Const aString : String): String;
		Property Row : Integer Read fRow Write fRow;
		Property Col : Integer Read fCol Write fCol;
		Property Console : TXMLCustomConsole Read fConsole;
		Property Variables : TNameValue Read fVariables;
	End;

	THTMLSpecialTag1Node = Class(THTMLNode)
	Public
		Function AsString: String; Override;
	End;

	THTMLSpecialTag2Node = Class(THTMLNode)
	Public
		Function AsString: String; Override;
	End;
	
	THTMLTextNode = Class(THTMLNode)
	Private
		fContent : String;
	Public
		Function AsString: String; Override;
		Property Content : String Read fContent Write fContent;
	End;

	THTMLIfNode = Class(THTMLNode)
	Public
		Function AsString: String; Override;
	End;

	THTMLForEachNode = Class(THTMLNode)
	Public
		Function AsString: String; Override;
	End;

	THTMLMakeVarNode = Class(THTMLNode)
	Public
		Function AsString: String; Override;
	End;

	THTMLRootNode = Class(THTMLNode)
	Public
		Function AsString: String; Override;
	End;

 	THTMLClassFactoryExtraData = Class(TObject)
	Private
		fHasChilds : Boolean;
	Public
		Constructor Create(Const aHasChilds : Boolean);
		Property HasChilds : Boolean Read fHasChilds Write fHasChilds;
	End;

	THTMLClassFactory = Class(TTreeClassFactory)
	Public
		Constructor Create;
		Procedure RegisterClassWithChilds(Const aClassName : String; Const aClassInfo : TTreeNodeClass);
		Procedure RegisterClassWithoutChilds(Const aClassName : String; Const aClassInfo : TTreeNodeClass);
		Function Build(Const aClassName : String; Const aOwner : THTMLNode): THTMLNode; Overload;
		Function TagHasChilds(Const aTagName : String): Boolean;
	End;

Var
	HTMLClassFactory : THTMLClassFactory;

Implementation

{ THTMLNode }

Constructor THTMLNode.Create(Const aOwner : TTreeNode);
Begin
	Inherited Create(aOwner);
	fVariables := TNameValue.Create;
End;

Destructor THTMLNode.Destroy;
Begin
	fVariables.Free;
	Inherited Destroy;
End;

Procedure THTMLNode.PropagateConsole(Const aConsole : TXMLCustomConsole);
Begin
	fConsole := aConsole;
	First;
	While Not IsAfterLast Do
	Begin
		(GetCurrent As THTMLNode).PropagateConsole(aConsole);
		Next;
	End;
End;

Function THTMLNode.ChildsAsString: String;
Begin
	Result := '';
	First;
	While Not IsAfterLast Do
	Begin
		Result := Result + (GetCurrent As THTMLNode).AsString;
		Next;
	End;
End;

Function THTMLNode.AsString: String;
Begin
	If Length(Properties.Pairs) > 0 Then
		Result := '<' + Name + ' ' + Properties.Formatted + '>'
	Else
		Result := '<' + Name + '>';
	If Length(Childs) > 0 Then
		Result := Result + ChildsAsString;
	If HTMLClassFactory.TagHasChilds(Name) Then
		Result := Result + '</' + Name + '>';
	Result := ReplaceVars(Result);
End;

Function THTMLNode.ReplaceVars(Const aString : String): String;
Var
	lCtrl : Integer;
Begin
	Result := aString;
	For lCtrl := Low(fVariables.Pairs) To High(fVariables.Pairs) Do
		Result := StringReplace(Result, '{$' + fVariables.Pairs[lCtrl].Name + '$}', fVariables.Pairs[lCtrl].Value, [ rfReplaceAll ]);
End;

{ THTMLSpecialTag1Node }

Function THTMLSpecialTag1Node.AsString: String;
Begin
	If Length(Properties.Pairs) > 0 Then
		Result := '<?' + Name + ' ' + Properties.Formatted + '?>'
	Else
		Result := '<?' + Name + '?>'
End;

{ THTMLSpecialTag2Node }

Function THTMLSpecialTag2Node.AsString: String;
Begin
	If Length(Properties.Pairs) > 0 Then
		Result := '<!' + Name + ' ' + Properties.Formatted + '!>'
	Else
		Result := '<!' + Name + '!>'
End;

{ THTMLTextNode }

Function THTMLTextNode.AsString: String;
Begin
	Result := fContent;
End;

{ THTMLIfNode }

Function THTMLIfNode.AsString: String;
Var
	lExpressionParser : TExpressionParser;
Begin
	Try
		lExpressionParser := TExpressionParser.Create(Properties.GetValue('expression'), Console.Stack);
		lExpressionParser.Evaluate;
		If Console.Stack.Top.Pop = True Then
			If HasChildNamed('then') Then
				Result := (Find('then') As THTMLNode).AsString
			Else
		Else
			If HasChildNamed('else') Then
				Result := (Locate('else') As THTMLNode).AsString;
	Finally
		lExpressionParser.Free;
	End;
	Result := ReplaceVars(Result);
End;

{ THTMLForEachNode }

Function THTMLForEachNode.AsString: String;
Begin
	Result := '';
	Console.Focused.Locate(Properties.GetValue('node')).First;
	While Not Console.Focused.Locate(Properties.GetValue('node')).IsAfterLast Do
	Begin
		Result := Result + ChildsAsString;
		Console.Focused.Locate(Properties.GetValue('node')).Next;
		Result := ReplaceVars(Result);
	End;
End;

{ THTMLMakeVarNode }

Function THTMLMakeVarNode.AsString: String;
Var
	lExpressionParser : TExpressionParser;
Begin
	Result := '';
	Try
		lExpressionParser := TExpressionParser.Create(Properties.GetValue('expression'), Console.Stack);
		lExpressionParser.Evaluate;
		If Not lExpressionParser.Error Then
			(Owner As THTMLNode).Variables.SetValue(Properties.GetValue('var'), Console.Stack.Top.Pop)
		Else
			Console.OutLn(mkError, 'At line ' + IntToStr(Row) + ', got ' + lExpressionParser.ErrorString);
	Finally
		lExpressionParser.Free;
	End;
End;

{ THTMLRootNode }

Function THTMLRootNode.AsString: String;
Begin
	Result := ChildsAsString;
	Result := ReplaceVars(Result);
End;

{ THTMLClassFactoryExtraData }

Constructor THTMLClassFactoryExtraData.Create(Const aHasChilds : Boolean);
Begin
	Inherited Create;
	fHasChilds := aHasChilds;
End;

{ THTMLClassFactory }

Constructor THTMLClassFactory.Create;
Begin
	Inherited Create;
	DefaultClass := THTMLNode;
End;

Procedure THTMLClassFactory.RegisterClassWithChilds(Const aClassName : String; Const aClassInfo : TTreeNodeClass);
Begin
	Register(aClassName, aClassInfo, THTMLClassFactoryExtraData.Create(True));
End;

Procedure THTMLClassFactory.RegisterClassWithoutChilds(Const aClassName : String; Const aClassInfo : TTreeNodeClass);
Begin
	Register(aClassName, aClassInfo, THTMLClassFactoryExtraData.Create(False));
End;

Function THTMLClassFactory.Build(Const aClassName : String; Const aOwner : THTMLNode): THTMLNode;
Begin
	Result := (Build(aClassName, aOwner As TTreeNode) As THTMLNode);
End;

Function THTMLClassFactory.TagHasChilds(Const aTagName : String): Boolean;
Var
	lCtrl : Integer;
Begin
	Result := False;
	For lCtrl := Low(RegisteredClasses) To High(RegisteredClasses) Do
		If RegisteredClasses[lCtrl].FactoryName = aTagName Then
		Begin
			Result := (RegisteredClasses[lCtrl].FactoryExtra As THTMLClassFactoryExtraData).HasChilds;
			Break;
		End;
End;

Initialization

	HTMLClassFactory := THTMLClassFactory.Create;
	{ HTML nodes with childs }
	HTMLClassFactory.RegisterClassWithChilds('a', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('b', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('big', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('body', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('center', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('dd', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('dl', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('dt', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('em', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('noembed', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('font', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('form', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('h1', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('h2', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('h3', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('h4', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('h5', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('h6', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('head', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('html', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('i', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('li', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('menu', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('ol', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('option', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('p', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('small', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('strike', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('strong', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('table', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('td', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('th', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('title', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('tr', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('tt', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('u', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('ul', THTMLNode);
	{ HTML nodes without childs }
	HTMLClassFactory.RegisterClassWithoutChilds('br', THTMLNode);
	HTMLClassFactory.RegisterClassWithoutChilds('embed', THTMLNode);
	HTMLClassFactory.RegisterClassWithoutChilds('hr', THTMLNode);
	HTMLClassFactory.RegisterClassWithoutChilds('img', THTMLNode);
	HTMLClassFactory.RegisterClassWithoutChilds('input', THTMLNode);
	HTMLClassFactory.RegisterClassWithoutChilds('link', THTMLNode);
	HTMLClassFactory.RegisterClassWithoutChilds('meta', THTMLNode);
	{ HTML Templating }
	HTMLClassFactory.RegisterClassWithChilds('if', THTMLIfNode);
	HTMlClassFactory.RegisterClassWithChilds('then', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('else', THTMLNode);
	HTMLClassFactory.RegisterClassWithChilds('foreach', THTMLForEachNode);
	HTMLClassFactory.RegisterClassWithoutChilds('makevar', THTMLMakeVarNode);

Finalization

	FreeAndNil(HTMLClassFactory);

End.