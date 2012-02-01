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
	Tree;

Interface

Uses
	Classes,
	SysUtils,
	StrUtils,
	NameValue;

Type
	TTreeNode = Class;
	TTreeNodeList = Array Of TTreeNode;
	TTreeNodeClass = Class Of TTreeNode;
	ETreeNode = Class(Exception);

	TTreeNode = Class(TObject)
	Private
		fOwner: TTreeNode;
		fChilds: TTreeNodeList;
		fCurrent: Integer;
	Public
		{ Constructor And Destructor }
		Constructor Create(Const aOwner : TTreeNode); Virtual;
		Destructor Destroy; Override;
		{ Child manipulation }
		Function AddChild(Const aNode : TTreeNode): TTreeNode;
		Procedure Delete(Const aNode : TTreeNode);
		Procedure Purge;
		{ Child Navigation }
		Procedure First;
		Procedure Next;
		Procedure Previous;
		Procedure Last;
		Function IsBeforeFirst: Boolean;
		Function IsAtFirst: Boolean;
		Function IsAtMiddle: Boolean;
		Function IsAtLast: Boolean;
		Function IsAfterLast: Boolean;
		Function GetFirst: TTreeNode;
		Function GetCurrent: TTreeNode;
		Function GetNext: TTreeNode;
		Function GetPrevious: TTreeNode;
		Function GetLast: TTreeNode;
		Function GetCurrentAndIncrement: TTreeNode;
		Function GetCurrentAndDecrement: TTreeNode;
		Function GetChildByNumber(Const aIndex : Integer): TTreeNode;
		Function GetCurrentIndex: Integer;
		{ Tree utility }
		Function FindRoot: TTreeNode;
		Function FindUppermost(Const aClass : TTreeNodeClass): TTreeNode;
		Function FindOfClass(Const aClass : TTreeNodeClass): TTreeNode;
		Function FindUpwards(Const aClass : TTreeNodeClass): TTreeNode;
		Function GroupBy(Const aClass : TTreeNodeClass): TTreeNodeList;
		{ Properties }
		Property Owner: TTreeNode Read fOwner;
		Property Childs: TTreeNodeList Read fChilds Write fChilds;
	End;

	TTreeNodeWithName = Class(TTreeNode)
	Private
		fName : String;
	Public
		{ Item location and info }
		Function IndexedName: String; Virtual;
		Function Find(Const aName : String): TTreeNodeWithName;
		Function Locate(aName : String): TTreeNodeWithName;
		Function HasChildNamed(Const aName : String): Boolean;
		Property Name : String Read fName Write fName;
	End;

	TTreeNodeWithProperties = Class(TTreeNodeWithName)
	Private
		fProperties : TNameValue;
	Public
		Constructor Create(Const aOwner : TTreeNode); Override;
		Destructor Destroy; Override;
		Property Properties : TNameValue Read fProperties;
	End;

	TTreeNodeIterator = Class
	Public
		Procedure Visit(Const aTarget : TTreeNode); Virtual;
		Procedure Process(Const aTarget : TTreeNode); Virtual; Abstract;
		Procedure OnNoChild(Const aTarget : TTreeNode); Virtual;
		Procedure OnBeforeSingleChild(Const aTarget : TTreeNode); Virtual;
		Procedure OnAfterSingleChild(Const aTarget : TTreeNode); Virtual;
		Procedure OnBeforeAllChilds(Const aTarget : TTreeNode); Virtual;
		Procedure OnAfterAllChilds(Const aTarget : TTreeNode); Virtual;
		Procedure OnBeforeChild(Const aTarget : TTreeNode); Virtual;
		Procedure OnAfterChild(Const aTarget : TTreeNode); Virtual;
	End;

	TTreeClassFactoryEntry = Record
		FactoryName  : String;
		FactoryClass : TTreeNodeClass;
		FactoryExtra : TObject;
	End;
	TTreeClassFactoryEntryList = Array Of TTreeClassFactoryEntry;

	TTreeClassFactory = Class(TObject)
	Private
		fClasses : TTreeClassFactoryEntryList;
		fDefaultClass : TTreeNodeClass;
	Public
		Procedure Register(Const aClassName : String; Const aClassInfo : TTreeNodeClass; Const aExtraData : TObject = Nil);
		Function Build(Const aClassName : String; Const aOwner : TTreeNode): TTreeNode;
		Property RegisteredClasses : TTreeClassFactoryEntryList Read fClasses Write fClasses;
		Property DefaultClass : TTreeNodeClass Read fDefaultClass Write fDefaultClass;
	End;

Implementation

Function ReportRangeError(Const aCurrent : Integer; Const aNode : TTreeNode): String;
Begin
	If Length(aNode.Childs) > 0 Then
		Result := 'Access out of bounds (Max = ' + 
			IntToStr(High(aNode.Childs)) + ' Min = ' + 
			IntToStr(Low(aNode.Childs)) + ' Value = ' + 
			IntToStr(aCurrent) + ').'
	Else
		Result := 'Access out of bounds (Node have no childs).';
End;

{ TTreeNode }

Constructor TTreeNode.Create(Const aOwner : TTreeNode);
Begin
	Inherited Create;
	fOwner := aOwner;
	If Assigned(fOwner) Then
		fOwner.AddChild(Self);
	SetLength(fChilds, 0);
	fCurrent := Low(fChilds);
End;

Destructor TTreeNode.Destroy;
Begin
	Purge;
	Inherited Destroy;
End;

Function TTreeNode.AddChild(Const aNode : TTreeNode): TTreeNode;
Begin
	SetLength(fChilds, Length(fChilds) + 1);
	fChilds[High(fChilds)] := aNode;
	Result := aNode;
End;

Procedure TTreeNode.Delete(Const aNode : TTreeNode);
Var
	lCtrl1,
	lCtrl2 : Integer;
	lFound : Boolean;
Begin
	If Length(fChilds) > 0 Then
	Begin
		lFound := False;
		For lCtrl1 := Low(fChilds) To High(fChilds) Do
			If fChilds[lCtrl1] = aNode Then
			Begin
				lFound := True;
				FreeAndNil(fChilds[lCtrl1]);
				For lCtrl2 := lCtrl1 To High(fChilds) - 1 Do
					fChilds[lCtrl2] := fChilds[lCtrl2 + 1];
				SetLength(fChilds, Length(fChilds) - 1);
				Break;
			End;
		If Not(lFound) Then
			Raise ETreeNode.Create('Node found to delete.');
	End
	Else
		Raise ETreeNode.Create('Cannot delete from an empty set.');
End;

Procedure TTreeNode.Purge;
Var
	lCtrl : Integer;
Begin
	If Length(fChilds) > 0 Then
		For lCtrl := Low(fChilds) To High(fChilds) Do
			FreeAndNil(fChilds[lCtrl]);
	SetLength(fChilds, 0);
End;

Procedure TTreeNode.First;
Begin
	fCurrent := Low(fChilds);
End;

Procedure TTreeNode.Next;
Begin
	Inc(fCurrent);
End;

Procedure TTreeNode.Previous;
Begin
	Dec(fCurrent);
End;

Procedure TTreeNode.Last;
Begin
	fCurrent := High(fChilds);
End;

Function TTreeNode.IsBeforeFirst: Boolean;
Begin
	Result := fCurrent < Low(fChilds);
End;

Function TTreeNode.IsAtFirst: Boolean;
Begin
	Result := fCurrent = Low(fChilds);
End;

Function TTreeNode.IsAtMiddle: Boolean;
Begin
	Result := (fCurrent > Low(fChilds)) And (fCurrent < High(fChilds));
End;

Function TTreeNode.IsAtLast: Boolean;
Begin
	Result := fCurrent = High(fChilds);
End;

Function TTreeNode.IsAfterLast: Boolean;
Begin
	Result := fCurrent > High(fChilds);
End;

Function TTreeNode.GetFirst: TTreeNode;
Begin
	If Length(fChilds) > 0 Then
		Result := fChilds[Low(fChilds)]
	Else
	Begin
		Result := Nil;
		Raise ETreeNode.Create(ReportRangeError(0, Self));
	End;
End;

Function TTreeNode.GetCurrent: TTreeNode;
Begin
	If (fCurrent >= Low(fChilds)) And (fCurrent <= High(fChilds)) Then
		Result := fChilds[fCurrent]
	Else
	Begin
		Result := Nil;
		If Length(fChilds) > 0 Then
			Raise ETreeNode.Create(ReportRangeError(fCurrent, Self));
	End;
End;

Function TTreeNode.GetNext: TTreeNode;
Begin
	If ((fCurrent + 1) >= Low(fChilds)) And ((fCurrent + 1) <= High(fChilds)) Then
		Result := fChilds[fCurrent + 1]
	Else
	Begin
		Result := Nil;
		Raise ETreeNode.Create(ReportRangeError(fCurrent + 1, Self));
	End;
End;

Function TTreeNode.GetPrevious: TTreeNode;
Begin
	If ((fCurrent - 1) >= Low(fChilds)) And ((fCurrent - 1) <= High(fChilds)) Then
		Result := fChilds[fCurrent - 1]
	Else
	Begin
		Result := Nil;
		Raise ETreeNode.Create(ReportRangeError(fCurrent - 1, Self));
	End;
End;

Function TTreeNode.GetLast: TTreeNode;
Begin
	If Length(fChilds) > 0 Then
		Result := fChilds[High(fChilds)]
	Else
	Begin
		Result := Nil;
		Raise ETreeNode.Create(ReportRangeError(0, Self));
	End;
End;

Function TTreeNode.GetCurrentAndIncrement: TTreeNode;
Begin
	Result := GetCurrent;
	Next;
End;

Function TTreeNode.GetCurrentAndDecrement: TTreeNode;
Begin
	Result := GetCurrent;
	Previous;
End;

Function TTreeNode.GetChildByNumber(Const aIndex : Integer): TTreeNode;
Begin
	If (aIndex >= Low(fChilds)) And (aIndex <= High(fChilds)) Then
		Result := fChilds[aIndex]
	Else
		Raise ETreeNode.Create(ReportRangeError(aIndex, Self));
End;

Function TTreeNode.GetCurrentIndex : Integer;
Begin
	Result := fCurrent;
End;

Function TTreeNode.FindRoot: TTreeNode;
Begin
	If Assigned(fOwner) Then
		Result := fOwner.FindRoot
	Else
		Result := Self;
End;

Function TTreeNode.FindUppermost(Const aClass : TTreeNodeClass): TTreeNode;
Begin
	If Assigned(fOwner) And (fOwner Is aClass) Then
		Result := fOwner.FindUppermost(aClass)
	Else
		Result := Self;
End;

Function TTreeNode.FindOfClass(Const aClass : TTreeNodeClass): TTreeNode;
Var
	lCtrl : Integer;
Begin
	Result := Nil;
	For lCtrl := Low(fChilds) To High(fChilds) Do
		If fChilds[lCtrl] Is aClass Then
		Begin
			Result := fChilds[lCtrl];
			Break;
		End;
End;

Function TTreeNode.FindUpwards(Const aClass : TTreeNodeClass): TTreeNode;
Begin
	If Self Is aClass Then
		Result := Self
	Else
		If Assigned(fOwner) Then
			Result := fOwner.FindUpwards(aClass)
		Else
			Result := Nil;
End;

Function TTreeNode.GroupBy(Const aClass : TTreeNodeClass): TTreeNodeList;
Var
	lCtrl : Integer;
Begin
	SetLength(Result, 0);
	For lCtrl := Low(fChilds) To High(fChilds) Do
		If fChilds[lCtrl] Is aClass Then
		Begin
			SetLength(Result, Length(Result) + 1);
			Result[High(Result)] := fChilds[lCtrl];
		End;
End;

{ TTreeNodeWithName }

Function TTreeNodeWithName.IndexedName : String;
Begin
	If Assigned(Owner) And (Owner Is TTreeNodeWithName) Then
		Result := (Owner As TTreeNodeWithName).IndexedName + '.' + fName
	Else
		Result := fName;
End;

Function TTreeNodeWithName.Find(Const aName : String): TTreeNodeWithName;
Var
	lCtrl : Integer;
Begin
	Result := Nil;
	For lCtrl := Low(Childs) To High(Childs) Do
		If (Childs[lCtrl] As TTreeNodeWithName).Name = aName Then
		Begin
			Result := Childs[lCtrl] As TTreeNodeWithName;
			Break;
		End;
	If Result = Nil Then
		Raise ETreeNode.Create('Theres no child named ' + aName + ' in the node ' + IndexedName);
End;

Function TTreeNodeWithName.Locate(aName : String): TTreeNodeWithName;
Var
	lItem : String;

	Function ExtractItemNumber: Integer;
	Begin
		RemovePadChars(lItem, [ '[', ']' ]);
		Result := StrToInt(lItem);
	End;

Begin
	Result := Nil;
	If aName = '' Then
		Result := Self
	Else
	Begin
		lItem := Copy2SymbDel(aName, '.');
		If UpperCase(lItem) = '$ROOT$' Then
			Result := (FindUppermost(TTreeNodeWithName) As TTreeNodeWithName).Locate(aName)
		Else If UpperCase(lItem) = '$FIRST$' Then
			Result := (GetFirst As TTreeNodeWithName).Locate(aName)
		Else If UpperCase(lItem) = '$CURRENT$' Then
			Result := (GetCurrent As TTreeNodeWithName).Locate(aName)
		Else If UpperCase(lItem) = '$NEXT$' Then
			Result := (GetNext As TTreeNodeWithName).Locate(aName)
		Else If UpperCase(lItem) = '$PREVIOUS$' Then
			Result := (GetPrevious As TTreeNodeWithName).Locate(aName)
		Else If UpperCase(lItem) = '$LAST$' Then
			Result := (GetLast As TTreeNodeWithName).Locate(aName)
		Else If UpperCase(lItem) = '$CURINC$' Then
			Result := (GetCurrentAndIncrement As TTreeNodeWithName).Locate(aName)
		Else If UpperCase(lItem) = '$CURDEC$' Then
			Result := (GetCurrentAndDecrement As TTreeNodeWithName).Locate(aName)
		Else If UpperCase(lItem) = '$OWNER$' Then
			Result := (Owner As TTreeNodeWithName).Locate(aName)
		Else If Copy(lItem, 1, 1) = '[' Then
			Result := (GetChildByNumber(ExtractItemNumber) As TTreeNodeWithName).Locate(aName)
		Else
			Result := Find(lItem).Locate(aName);
	End;
End;

Function TTreeNodeWithName.HasChildNamed(Const aName : String): Boolean;
Var
	lCtrl : Integer;
Begin
	Result := False;
	For lCtrl := Low(Childs) To High(Childs) Do
		If (Childs[lCtrl] As TTreeNodeWithName).Name = aName Then
		Begin
			Result := True;
			Break;
		End;
End;

{ TTreeNodeWithProperties }

Constructor TTreeNodeWithProperties.Create(Const aOwner : TTreeNode);
Begin
	Inherited Create(aOwner);
	fProperties := TNameValue.Create;
End;

Destructor TTreeNodeWithProperties.Destroy;
Begin
	FreeAndNil(fProperties);
	Inherited Destroy;
End;

{ TTreeNodeIterator }

Procedure TTreeNodeIterator.Visit(Const aTarget : TTreeNode);
Var
	lCtrl : Integer;
Begin
	Process(aTarget);
	If Length(aTarget.Childs) > 1 Then
	Begin
		OnBeforeAllChilds(aTarget);
		For lCtrl := Low(aTarget.Childs) To High(aTarget.Childs) Do
		Begin
			OnBeforeChild(aTarget);
			Visit(aTarget.Childs[lCtrl]);
			OnAfterChild(aTarget);
		End;
		OnAfterAllChilds(aTarget);
	End
	Else If Length(aTarget.Childs) = 1 Then
	Begin
		OnBeforeSingleChild(aTarget);
		Visit(aTarget.GetFirst);
		OnAfterSingleChild(aTarget);
	End
	Else If Length(aTarget.Childs) < 1 Then
		OnNoChild(aTarget);
End;

Procedure TTreeNodeIterator.OnNoChild(Const aTarget : TTreeNode);
Begin
End;

Procedure TTreeNodeIterator.OnBeforeSingleChild(Const aTarget : TTreeNode);
Begin
End;

Procedure TTreeNodeIterator.OnAfterSingleChild(Const aTarget : TTreeNode);
Begin
End;

Procedure TTreeNodeIterator.OnBeforeAllChilds(Const aTarget : TTreeNode);
Begin
End;

Procedure TTreeNodeIterator.OnAfterAllChilds(Const aTarget : TTreeNode);
Begin
End;

Procedure TTreeNodeIterator.OnBeforeChild(Const aTarget : TTreeNode);
Begin
End;

Procedure TTreeNodeIterator.OnAfterChild(Const aTarget : TTreeNode);
Begin
End;

{ TTreeClassFactory }

Procedure TTreeClassFactory.Register(Const aClassName : String; Const aClassInfo : TTreeNodeClass; Const aExtraData : TObject = Nil);
Begin
	SetLength(fClasses, Length(fClasses) + 1);
	fClasses[High(fClasses)].FactoryName := aClassName;
	fClasses[High(fClasses)].FactoryClass := aClassInfo;
	fClasses[High(fClasses)].FactoryExtra := aExtraData;
End;

Function TTreeClassFactory.Build(Const aClassName : String; Const aOwner : TTreeNode): TTreeNode;
Var
	lCtrl : Integer;
	lClass : TTreeNodeClass;
Begin
	lClass := fDefaultClass;
	For lCtrl := Low(fClasses) To High(fClasses) Do
		If fClasses[lCtrl].FactoryName = aClassName Then
		Begin
			lClass := fClasses[lCtrl].FactoryClass;
			Break;
		End;
	Result := lClass.Create(aOwner);
End;

End.