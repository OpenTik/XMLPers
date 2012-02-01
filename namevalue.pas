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
	NameValue;

Interface

Uses
	Classes,
	SysUtils,
	Variants;

Type
	ENameValue = Class(Exception);

	TNameValuePair = Class(TObject)
	Private
		fName  : String;
		fValue : Variant;
	Public
		Constructor Create(Const aName : String; Const aValue : Variant);
		Function Formatted : String;
		Property Name : String Read fName Write fName;
		Property Value : Variant Read fValue Write fValue;
	End;
	TNameValueList = Array Of TNameValuePair;

	TNameValue = Class(TObject)
	Private
		fPairs : TNameValueList;
	Public
		Constructor Create;
		Destructor Destroy; Override;
		Function Add(Const aPair : TNameValuePair): TNameValuePair;
		Procedure Delete(Const aPair : TNameValuePair);
		Procedure Purge;
		Function Find(Const aName : String): TNameValuePair;
		Function Exists(Const aName : String): Boolean;
		Procedure SetValue(Const aName : String; Const aValue : Variant);
		Function GetValue(Const aName : String): Variant;
		Function Formatted: String;
		Property Pairs : TNameValueList Read fPairs Write fPairs;
	End;

Implementation

{ TNameValuePair }

Constructor TNameValuePair.Create(Const aName : String; Const aValue : Variant);
Begin
	Inherited Create;
	fName := aName;
	fValue := aValue;
End;

Function TNameValuePair.Formatted : String;
Begin
	Result := fName + '="' + fValue + '"';
End;

{ TNameValue }

Constructor TNameValue.Create;
Begin
	Inherited Create;
	SetLength(fPairs, 0);
End;

Destructor TNameValue.Destroy;
Begin
	Purge;
	Inherited Destroy;
End;

Function TNameValue.Add(Const aPair : TNameValuePair): TNameValuePair;
Begin
	SetLength(fPairs, Length(fPairs) + 1);
	fPairs[High(fPairs)] := aPair;
	Result := aPair;
End;

Procedure TNameValue.Delete(Const aPair : TNameValuePair);
Var
	lCtrl1,
	lCtrl2 : Integer;
Begin
	For lCtrl1 := Low(fPairs) To High(fPairs) Do
		If fPairs[lCtrl1] = aPair Then
		Begin
			FreeAndNil(fPairs[lCtrl1]);
			For lCtrl2 := lCtrl1 To High(fPairs) - 1 Do
				fPairs[lCtrl2] := fPairs[lCtrl2 + 1];
			SetLength(fPairs, Length(fPairs) - 1);
		End;
End;

Procedure TNameValue.Purge;
Var
	lCtrl : Integer;
Begin
	For lCtrl := Low(fPairs) To High(fPairs) Do
		FreeAndNil(fPairs[lCtrl]);
	SetLength(fPairs, 0);
End;

Function TNameValue.Find(Const aName : String): TNameValuePair;
Var
	lCtrl : Integer;
Begin
	Result := Nil;
	For lCtrl := Low(fPairs) To High(fPairs) Do
		If fPairs[lCtrl].Name = aName Then
		Begin
			Result := fPairs[lCtrl];
			Break;
		End;
End;

Function TNameValue.Exists(Const aName : String): Boolean;
Begin
	Result := Find(aName) <> Nil;
End;

Procedure TNameValue.SetValue(Const aName : String; Const aValue : Variant);
Begin
	If Exists(aName) Then
		Find(aName).Value := aValue
	Else
		Add(TNameValuePair.Create(aName, aValue));
End;

Function TNameValue.GetValue(Const aName : String): Variant;
Begin
	If Exists(aName) Then
		Result := Find(aName).Value
	Else
		Raise ENameValue.Create('Theres no such name.');
End;

Function TNameValue.Formatted: String;
Var
	lCtrl : Integer;
Begin
	Result := '';
	For lCtrl := Low(fPairs) To High(fPairs) Do
		If lCtrl < High(fPairs) Then
			Result := Result + fPairs[lCtrl].Formatted + ' '
		Else
			Result := Result + fPairs[lCtrl].Formatted;
End;

End.