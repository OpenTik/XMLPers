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
	ExprNodes;

Interface

Uses
	Classes,
	SysUtils,
	Tree,
	Stacks;

Type
	TVariantStack = Specialize TStack<Variant>;
	TVariantStackStack = Specialize TStack<TVariantStack>;
	TWindownedStack = Class(TVariantStackStack)
	Public
		Constructor Create;
		Destructor Destroy; Override;
		Procedure Enter(Const aCount : Integer);
		Procedure Leave(Const aCount : Integer);
		Procedure Purge;
	End;

	TArithmeticalStack = Class(TWindownedStack)
	Public
		Procedure DoDup;
		Procedure DoDrop;
		Procedure DoRot;
		Procedure DoSwap;
		Procedure DoPushTrue;
		Procedure DoPushFalse;
		Procedure DoAdd;
		Procedure DoSub;
		Procedure DoMul;
		Procedure DoDiv;
		Procedure DoIDiv;
		Procedure DoMod;
		Procedure DoPow;
		Procedure DoNot;
		Procedure DoAnd;
		Procedure DoOr;
		Procedure DoXOr;
		Procedure DoCmpEq;
		Procedure DoCmpSm;
		Procedure DoCmpGt;
		Procedure DoCmpDif;
		Procedure DoCmpEqSm;
		Procedure DoCmpGtSm;
	End;

	EVirtualMachineStack = Class(Exception);
	TVirtualMachineStack = Class(TArithmeticalStack)
	Public
		Procedure DoCall(Const aFunctionName : String);
		Procedure DefaultHandlerStr(Var aMessage); Override;
		Procedure FunctionTrue(Var aMessage); Message 'true';
		Procedure FunctionFalse(Var aMessage); Message 'false';
	End;

	TExprNode = Class;
	TExprNodeClass = Class Of TExprNode;
	EExprNode = Class(Exception);
	TExprNodeList = Array Of TExprNode;

	TExprNode = Class(TTreeNode)
	Private
		fRow, fCol : Integer;
		fStack : TVirtualMachineStack;
	Public
		Procedure RaiseError(Const aMsg : String);
		Procedure EvaluateChilds; Virtual;
		Procedure Evaluate; Virtual; Abstract;
		Procedure PropagateStack(Const aStack : TVirtualMachineStack); Virtual;
		Property Row : Integer Read fRow Write fRow;
		Property Col : Integer Read fCol Write fCol;
		Property Stack : TVirtualMachineStack Read fStack;
	End;

	TUnaryMinusNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
	End;

	TUnaryNotNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
	End;

	TExpressionListNode = Class;

	TFunctionCallNode = Class(TExprNode)
	Private
		fName : String;
		fParameters : TExpressionListNode;
	Public
		Procedure Evaluate; Override;
		Property Name : String Read fName Write fName;
		Property Parameters : TExpressionListNode Read fParameters Write fParameters;
	End;

	TStringLiteralNode = Class(TExprNode)
	Private
		fValue : String;
	Public
		Procedure Evaluate; Override;
		Property Value : String Read fValue Write fValue;
	End;

	TNumberLiteralNode = Class(TExprNode)
	Private
		fValue : Real;
	Public
		Procedure Evaluate; Override;
		Property Value : Real Read fValue Write fValue;
	End;

	TMulExpressionNode = Class(TExprNode)
	Private
		fOperation : String;
	Public
		Procedure Evaluate; Override;
		Property Operation : String Read fOperation Write fOperation;
	End;

	TAddExpressionNode = Class(TExprNode)
	Private
		fOperation : String;
	Public
		Procedure Evaluate; Override;
		Property Operation : String Read fOperation Write fOperation;
	End;

	TExpressionNode = Class(TExprNode)
	Private
		fOperation : String;
	Public
		Procedure Evaluate; Override;
		Property Operation : String Read fOperation Write fOperation;
	End;

	TExpressionListNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
	End;

	TSourceNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
	End;

Implementation

Type
	TVirtualMachineMessage = Record
		MsgStr : String[255];
		Data : Pointer;
	End;

{ TWindownedStack }

Constructor TWindownedStack.Create;
Begin
	Inherited Create;
	Push(TVariantStack.Create);
End;

Destructor TWindownedStack.Destroy;
Begin
	While AtLeast(1) Do
		Pop.Free;
	Inherited Destroy;
End;

Procedure TWindownedStack.Enter(Const aCount : Integer);
Var
	lCtrl : Integer;
Begin
	Push(TVariantStack.Create);
	For lCtrl := 1 To aCount Do
		Top.Push(Previous.Pop);
End;

Procedure TWindownedStack.Leave(Const aCount : Integer);
Var
	lCtrl : Integer;
Begin
	For lCtrl := 1 To aCount Do
		Previous.Push(Top.Pop);
	Top.Free;
	Pop;
End;

Procedure TWindownedStack.Purge;
Begin
	While AtLeast(1) Do
		Pop.Free;
	Push(TVariantStack.Create);
End;

{ TArithmeticalStack }

Procedure TArithmeticalStack.DoDup;
Var
	lTmp : Variant;
Begin
	lTmp := Top.Pop;
	Top.Push(lTmp);
	Top.Push(lTmp);
End;

Procedure TArithmeticalStack.DoDrop;
Begin
	Top.Pop;
End;

Procedure TArithmeticalStack.DoRot;
Var
	lTmp1,
	lTmp2,
	lTmp3 : Variant;
Begin
	lTmp1 := Top.Pop;
	lTmp2 := Top.Pop;
	lTmp3 := Top.Pop;
	Top.Push(lTmp1);
	Top.Push(lTmp2);
	Top.Push(lTmp3);
End;

Procedure TArithmeticalStack.DoSwap;
Var
	lTmp1,
	lTmp2 : Variant;
Begin
	lTmp1 := Top.Pop;
	lTmp2 := Top.Pop;
	Top.Push(lTmp1);
	Top.Push(lTmp2);
End;

Procedure TArithmeticalStack.DoPushTrue;
Begin
	Top.Push(True);
End;

Procedure TArithmeticalStack.DoPushFalse;
Begin
	Top.Push(False);
End;

Procedure TArithmeticalStack.DoAdd;
Begin
	Top.Push(Top.Pop + Top.Pop);
End;

Procedure TArithmeticalStack.DoSub;
Begin
	Top.Push(Top.Pop - Top.Pop);
End;

Procedure TArithmeticalStack.DoMul;
Begin
	Top.Push(Top.Pop * Top.Pop);
End;

Procedure TArithmeticalStack.DoDiv;
Begin
	Top.Push(Top.Pop / Top.Pop);
End;

Procedure TArithmeticalStack.DoIDiv;
Begin
	Top.Push(Top.Pop Div Top.Pop);
End;

Procedure TArithmeticalStack.DoMod;
Begin
	Top.Push(Top.Pop Mod Top.Pop);
End;

Procedure TArithmeticalStack.DoPow;
Begin
	Top.Push(Top.Pop ** Top.Pop);
End;

Procedure TArithmeticalStack.DoNot;
Begin
	Top.Push(Not(Top.Pop));
End;

Procedure TArithmeticalStack.DoAnd;
Begin
	Top.Push(Top.Pop And Top.Pop);
End;

Procedure TArithmeticalStack.DoOr;
Begin
	Top.Push(Top.Pop Or Top.Pop);
End;

Procedure TArithmeticalStack.DoXOr;
Begin
	Top.Push(Top.Pop XOr Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpEq;
Begin
	Top.Push(Top.Pop = Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpSm;
Begin
	Top.Push(Top.Pop < Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpGt;
Begin
	Top.Push(Top.Pop > Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpDif;
Begin
	Top.Push(Top.Pop <> Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpEqSm;
Begin
	Top.Push(Top.Pop <= Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpGtSm;
Begin
	Top.Push(Top.Pop >= Top.Pop);
End;

{ TVirtualMachineStack }

Procedure TVirtualMachineStack.DoCall(Const aFunctionName : String);
Var
	lMsg : TVirtualMachineMessage;
Begin
	lMsg.MsgStr := LowerCase(aFunctionName);
	lMsg.Data := Nil;
	DispatchStr(lMsg);
End;

Procedure TVirtualMachineStack.DefaultHandlerStr(Var aMessage);
Begin
	Raise EVirtualMachineStack.Create('Unknown identifier "' + TVirtualMachineMessage(aMessage).MsgStr + '".');
End;

Procedure TVirtualMachineStack.FunctionTrue(Var aMessage);
Begin
	DoPushTrue;
End;

Procedure TVirtualMachineStack.FunctionFalse(Var aMessage);
Begin
	DoPushFalse;
End;

{ TExprNode }

Procedure TExprNode.RaiseError(Const aMsg : String);
Begin
	Raise EExprNode.Create(IntToStr(fRow) + ',' + IntToStr(fCol) + ',' + aMsg);
End;

Procedure TExprNode.EvaluateChilds;
Begin
	First;
	While Not(IsAfterLast) Do
	Begin
		(GetCurrent As TExprNode).Evaluate;
		Next;
	End;
End;

Procedure TExprNode.PropagateStack(Const aStack : TVirtualMachineStack);
Begin
	fStack := aStack;
	First;
	While Not(IsAfterLast) Do
	Begin
		(GetCurrent As TExprNode).PropagateStack(aStack);
		Next;
	End;
End;

{ TUnaryMinusNode }

Procedure TUnaryMinusNode.Evaluate;
Begin
	EvaluateChilds;
	Stack.Top.Push(-1);
	Try
		Stack.DoMul;
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

{ TUnaryNotNode }

Procedure TUnaryNotNode.Evaluate;
Begin
	Stack.DoNot;
End;

{ TFunctionCallNode }

Procedure TFunctionCallNode.Evaluate;
Begin
	EvaluateChilds;
	Try
		If Assigned(fParameters) Then
			Stack.Enter(Length(fParameters.Childs))
		Else
			Stack.Enter(0);
		Stack.DoCall(fName);
		Stack.Leave(1);
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

{ TStringLiteralNode }

Procedure TStringLiteralNode.Evaluate;
Begin
	Stack.Top.Push(fValue);
End;

{ TNumberLiteralNode }

Procedure TNumberLiteralNode.Evaluate;
Begin
	Stack.Top.Push(fValue);
End;

{ TMulExpressionNode }

Procedure TMulExpressionNode.Evaluate;
Begin
	EvaluateChilds;
	If fOperation = '*' Then
		Try
			Stack.DoMul;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '/' Then
		Try
			Stack.DoDiv;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'div' Then
		Try
			Stack.DoIDiv;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'mod' Then
		Try
			Stack.DoMod;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'and' Then
		Try
			Stack.DoAnd;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '^' Then
		Try
			Stack.DoPow;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End;
End;

{ TAddExpressionNode }

Procedure TAddExpressionNode.Evaluate;
Begin
	EvaluateChilds;
	If fOperation = '+' Then
		Try
			Stack.DoAdd;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '-' Then
		Try
			Stack.DoSub;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'or' Then
		Try
			Stack.DoOr;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'xor' Then
		Try
			Stack.DoXOr;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End;
End;

{ TExpressionNode }

Procedure TExpressionNode.Evaluate;
Begin
	EvaluateChilds;
	If fOperation = '=' Then
		Try
			Stack.DoCmpEq;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '<' Then
		Try
			Stack.DoCmpSm;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '>' Then
		Try
			Stack.DoCmpGt;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '<>' Then
		Try
			Stack.DoCmpDif;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '<=' Then
		Try
			Stack.DoCmpEqSm;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '>=' Then
		Try
			Stack.DoCmpGtSm;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End;
End;

{ TExpressionListNode }

Procedure TExpressionListNode.Evaluate;
Begin
	EvaluateChilds;
End;

{ TSourceNode }

Procedure TSourceNode.Evaluate;
Begin
	EvaluateChilds;
End;

End.