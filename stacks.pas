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
	Stacks;

Interface

Uses
	Classes,
	SysUtils,
	Variants;

Type
	EStackUnderflow = Class(Exception);

	Generic TStack<_T> = Class(TObject)
	Private
		fBuffer : Array Of _T;
	Public
		Procedure Push(Const aValue : _T);
		Function Pop: _T;
		Function Top: _T;
		Function First: _T;
		Function Previous: _T;
		Function Peek(Const aIndex : Integer): _T;
		Procedure Poke(Const aIndex : Integer; Const aValue : _T);
		Function Count : Integer;
		Function AtLeast(Const aCount : Integer): Boolean;
	End;

	TIntegerStack = Specialize TStack<Integer>;

Implementation

Procedure TStack.Push(Const aValue : _T);
Begin
	SetLength(fBuffer, Length(fBuffer) + 1);
	fBuffer[High(fBuffer)] := aValue;
End;

Function TStack.Pop: _T;
Begin
	If AtLeast(1) Then
	Begin
		Result := fBuffer[High(fBuffer)];
		SetLength(fBuffer, Length(fBuffer) - 1);
	End
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.Top: _T;
Begin
	If AtLeast(1) Then
		Result := fBuffer[High(fBuffer)]
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.First: _T;
Begin
	If AtLeast(1) Then
	Begin
		Result := fBuffer[Low(fBuffer)];
	End
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.Previous: _T;
Begin
	If AtLeast(2) Then
	Begin
		Result := fBuffer[High(fBuffer) - 1];
	End
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.Peek(Const aIndex : Integer): _T;
Begin
	If (aIndex >= Low(fBuffer)) And (aIndex <= High(fBuffer)) Then
		Result := fBuffer[aIndex]
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Procedure TStack.Poke(Const aIndex : Integer; Const aValue : _T);
Begin
	If (aIndex >= Low(fBuffer)) And (aIndex <= High(fBuffer)) Then
		fBuffer[aIndex] := aValue
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.Count : Integer;
Begin
	Result := Length(fBuffer);
End;

Function TStack.AtLeast(Const aCount : Integer): Boolean;
Begin
	Result := Count >= aCount;
End;

End.