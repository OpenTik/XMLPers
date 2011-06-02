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
	XMLNodes;

Interface

Uses
	Classes,
	SysUtils,
	Tree,
	StrUtils;

Type
	EXMLNode = Class(Exception);

	TXMLTextNode = Class;
	TXMLNode = Class;
	TXMLNodeClass = Class Of TXMLNode;

	TXMLNode = Class(TTreeNodeWithProperties)
	Public
		Function IndexedName: String; Override;
		Function GetTextChild: String;
		Procedure SetTextChild(Const aContent : String);
		Function AsXML: String; Virtual;
		Function AsClosingXML: String; Virtual;
		Property Text: String Read GetTextChild Write SetTextChild;
	End;

	TXMLSpecialTag1Node = Class(TXMLNode)
	Public
		Function AsXML: String; Override;
		Function AsClosingXML: String; Override;
	End;

	TXMLSpecialTag2Node = Class(TXMLNode)
	Public
		Function AsXML: String; Override;
		Function AsClosingXML: String; Override;
	End;

	TXMLTextNode = Class(TXMLNode)
	Private
		fContent : String;
	Public
		Function AsXML: String; Override;
		Function AsClosingXML: String; Override;
		Property Content : String Read fContent Write fContent;
	End;

	TXMLRootNode = Class(TXMLNode)
	Public
		Function AsXML: String; Override;
		Function AsClosingXML: String; Override;
	End;

	TXMLAsTextIterator = Class(TTreeNodeIterator)
	Private
		fDepth : Integer;
		fOutput : String;
	Public
		Constructor Create;
		Procedure Process(Const aTarget : TTreeNode); Override;
		Procedure OnNoChild(Const aTarget : TTreeNode); Override;
		Procedure OnBeforeSingleChild(Const aTarget : TTreeNode); Override;
		Procedure OnAfterSingleChild(Const aTarget : TTreeNode); Override;
		Procedure OnBeforeAllChilds(Const aTarget : TTreeNode); Override;
		Procedure OnAfterAllChilds(Const aTarget : TTreeNode); Override;
		Procedure OnBeforeChild(Const aTarget : TTreeNode); Override;
		Procedure OnAfterChild(Const aTarget : TTreeNode); Override;
		Property Output : String Read fOutput Write fOutput;
	End;

	TXMLClassFactory = Class(TTreeClassFactory)
	Public
		Constructor Create;
		Function Build(Const aClassName : String; Const aOwner : TXMLNode): TXMLNode; Overload;
	End;

Var
	XMLClassFactory : TXMLClassFactory;

Implementation

{ TXMLNode }

Function TXMLNode.IndexedName: String;
Begin
	If Self Is TXMLTextNode Then
		Result := Inherited IndexedName + 'TEXT'
	Else
		Result := Inherited IndexedName;
End;

Function TXMLNode.GetTextChild: String;
Begin
	If Assigned(GetFirst) And (GetFirst Is TXMLTextNode) Then
		Result := (GetFirst As TXMLTextNode).Content
	Else
		Raise EXMLNode.Create('No text attached to node');
End;

Procedure TXMLNode.SetTextChild(Const aContent : String);
Begin
	If Assigned(GetFirst) And (GetFirst Is TXMLTextNode) Then
		(GetFirst As TXMLTextNode).Content := aContent
	Else
		(AddChild(TXMLTextNode.Create(Self)) As TXMLTextNode).Content := aContent;
End;

Function TXMLNode.AsXML: String;
Begin
	If Length(Properties.Pairs) > 0 Then
		If Length(Childs) > 0 Then
			Result := '<' + Name + Properties.Formatted + '>'
		Else
			Result := '<' + Name + Properties.Formatted + '/>'
	Else
		If Length(Childs) > 0 Then
			Result := '<' + Name + '>'
		Else
			Result := '<' + Name + '/>';
End;

Function TXMLNode.AsClosingXML: String;
Begin
	Result := '</' + Name + '>';
End;

{ TXMLSpecialTag1Node }

Function TXMLSpecialTag1Node.AsXML: String;
Begin
	If Length(Properties.Pairs) > 0 Then
		Result := '<?' + Name + ' ' + Properties.Formatted + '?>'
	Else
		Result := '<?' + Name + '?>';
End;

Function TXMLSpecialTag1Node.AsClosingXML: String;
Begin
	Result := '';
End;

{ TXMLSpecialTag2Node }

Function TXMLSpecialTag2Node.AsXML: String;
Begin
	If Length(Properties.Pairs) > 0 Then
		Result := '<!' + Name + ' ' + Properties.Formatted + '!>'
	Else
		Result := '<!' + Name + '!>';
End;

Function TXMLSpecialTag2Node.AsClosingXML: String;
Begin
	Result := '';
End;

{ TXMLTextNode }

Function TXMLTextNode.AsXML: String;
Begin
	Result := fContent;
End;

Function TXMLTextNode.AsClosingXML: String;
Begin
	Result := '';
End;

{ TXMLRootNode }

Function TXMLRootNode.AsXML: String;
Begin
	Result := '';
End;

Function TXMLRootNode.AsClosingXML: String;
Begin
	Result := '';
End;

{ TXMLAsTextIterator }

Constructor TXMLAsTextIterator.Create;
Begin
	Inherited Create;
	fDepth := 0;
	fOutput := '';
End;

Procedure TXMLAsTextIterator.Process(Const aTarget : TTreeNode);
Begin
End;

Procedure TXMLAsTextIterator.OnNoChild(Const aTarget : TTreeNode);
Begin
	If aTarget Is TXMLTextNode Then
		fOutput := fOutput + (aTarget As TXMLNode).AsXML
	Else
		If Assigned(aTarget.Owner) And (Length(aTarget.Owner.Childs) = 1) Then
			fOutput := fOutput + (aTarget As TXMLNode).AsXML
		Else
			fOutput := fOutput + DupeString(' ', fDepth) + (aTarget As TXMLNode).AsXML;
End;

Procedure TXMLAsTextIterator.OnBeforeSingleChild(Const aTarget : TTreeNode);
Begin
	If Assigned(aTarget.Owner) And (Length(aTarget.Owner.Childs) = 1) Then
		fOutput := fOutput + (aTarget As TXMLNode).AsXML
	Else
		If aTarget.GetFirst Is TXMLTextNode Then
			fOutput := fOutput + DupeString(' ', fDepth) + (aTarget As TXMLNode).AsXML
		Else
			fOutput := fOutput + DupeString(' ', fDepth) + (aTarget As TXMLNode).AsXML + #13#10;
	Inc(fDepth);
End;

Procedure TXMLAsTextIterator.OnAfterSingleChild(Const aTarget : TTreeNode);
Begin
	Dec(fDepth);
	If aTarget.GetFirst Is TXMLTextNode Then
		fOutput := fOutput + (aTarget As TXMLNode).AsClosingXML
	Else
		fOutput := fOutput + #13#10 + DupeString(' ', fDepth) + (aTarget As TXMLNode).AsClosingXML;
End;

Procedure TXMLAsTextIterator.OnBeforeAllChilds(Const aTarget : TTreeNode);
Begin
	fOutput := fOutput + DupeString(' ', fDepth) + (aTarget As TXMLNode).AsXML + #13#10;
	Inc(fDepth);
End;

Procedure TXMLAsTextIterator.OnAfterAllChilds(Const aTarget : TTreeNode);
Begin
	Dec(fDepth);
	fOutput := fOutput + DupeString(' ', fDepth) + (aTarget As TXMLNode).AsClosingXML;
End;

Procedure TXMLAsTextIterator.OnBeforeChild(Const aTarget : TTreeNode);
Begin
End;

Procedure TXMLAsTextIterator.OnAfterChild(Const aTarget : TTreeNode);
Begin
	fOutput := fOutput + #13#10;
End;

{ TXMLClassFactory }

Constructor TXMLClassFactory.Create;
Begin
	Inherited Create;
	DefaultClass := TXMLNode;
End;

Function TXMLClassFactory.Build(Const aClassName : String; Const aOwner : TXMLNode): TXMLNode; Overload;
Begin
	Result := (Build(aClassName, aOwner As TTreeNode) As TXMLNode);
End;

Initialization
	
	XMLClassFactory := TXMLClassFactory.Create;
	XMLClassFactory.DefaultClass := TXMLNode;

Finalization

	XMLClassFactory.Free;

End.