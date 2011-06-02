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
	ExpressionParser;

Interface

Uses
	Classes,
	SysUtils,
	ExprScanner,
	ExprParser,
	ExprNodes,
	Variants,
	StrUtils;

Type
	TExpressionParser = Class(TObject)
	Private
		fStack : TVirtualMachineStack;
		fExpression : String;
		fError : Boolean;
		fErrorString : String;
		fErrorRow : Integer;
		fErrorCol : Integer;
	Public
		Constructor Create(Const aExpression : String; Const aStack : TVirtualMachineStack);
		Procedure Evaluate(Const aSingleExpression : Boolean = True);
		Property Error : Boolean Read fError;
		Property ErrorString : String Read fErrorString;
		Property ErrorRow : Integer Read fErrorRow;
		Property ErrorCol : Integer Read fErrorCol;
	End;

Implementation

{ TExpressionParser }

Constructor TExpressionParser.Create(Const aExpression : String; Const aStack : TVirtualMachineStack);
Begin
	Inherited Create;
	fExpression := aExpression;
	fStack := aStack;
	fError := False;
	fErrorString := '';
End;

Procedure TExpressionParser.Evaluate(Const aSingleExpression : Boolean = True);
Var
	lStringStream : TStringStream;
	lSource : TExprSource;
	lIntermediary : TMemoryStream;
	lScanner : TExprScanner;
	lTokenIterator : TExprTokenIterator;
	lParser : TExprParser;
	lDestination : TSourceNode;
	lTmp : String;
Begin
	Try
		lStringStream := Nil;
		lSource := Nil;
		lIntermediary := Nil;
		lScanner := Nil;
		lTokenIterator := Nil;
		lParser := Nil;
		lDestination := Nil;
		Try
			lStringStream := TStringStream.Create(fExpression);
			lSource := TExprSource.Create(lStringStream, False);
			lIntermediary := TMemoryStream.Create;
			lScanner := TExprScanner.Create(lSource, lIntermediary, False);
			lScanner.Scan;
			lTokenIterator := TExprTokenIterator.Create(lIntermediary, False);
			lParser := TExprParser.Create(lTokenIterator, False);
			If aSingleExpression Then
				lDestination := lParser.ParseSource As TSourceNode
			Else
				lDestination := lParser.ParseParameters As TSourceNode;
			lDestination.PropagateStack(fStack);
			lDestination.Evaluate;
		Finally
			FreeAndNil(lStringStream);
			FreeAndNil(lSource);
			FreeAndNil(lIntermediary);
			FreeAndNil(lScanner);
			FreeAndNil(lTokenIterator);
			FreeAndNil(lParser);
			FreeAndNil(lDestination);
		End;
	Except
		On E: Exception Do
		Begin
			fError := True;
			lTmp := E.Message;
			fErrorRow := StrToInt(Copy2SymbDel(lTmp, ','));
			fErrorCol := StrToInt(Copy2SymbDel(lTmp, ','));
			fErrorString := lTmp;
		End;
	End;
End;

End.