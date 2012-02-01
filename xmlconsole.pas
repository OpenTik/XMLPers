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
	XMLConsole;

Interface

Uses
	Classes,
	SysUtils,
	StrUtils,
	Stacks,
	XMLScanner,
	XMLParser,
	XMLNodes,
	ExprNodes,
	ExpressionParser;

Type
	EUnknownCommand = Class(Exception);
	ENoSuchChild = Class(Exception);

	TXMLCustomConsole = Class;

	TTokenList = Array Of String;

	TXMLObjectMessage = Record
		MsgStr : String[255];
		Data : Pointer;
	End;

	TXMLAccessStack = Class;

	TXMLConsoleMessageKind = (mkDebug, mkError, mkResponse, mkInfo);
	TXMLVerbosity = Array[mkDebug..mkInfo] Of Boolean;

	TXMLCustomConsole = Class
	Private
		fCommandLine : String;
		fFocused : TXMLNode;
		fRunning : Boolean;
		fStack : TXMLAccessStack;
		fVerbosity : TXMLVerbosity;
		fPrompt : String;
	Public
		Constructor Create;
		Destructor Destroy; Override;
		{ Virtual Console Handling }
		Function ReadLine: String; Virtual; Abstract;
		Procedure Out(Const aKind : TXMLConsoleMessageKind; Const aLine : String); Virtual; Abstract;
		Procedure OutLn(Const aKind : TXMLConsoleMessageKind; Const aLine : String); Virtual; Abstract;
		Procedure Process; Virtual;
		Function AtLeast(Const aCount : Integer): Boolean;
		{ Command handlers }
		Procedure Submit; Virtual;
		Procedure DefaultHandlerStr(Var Message); Override;
		Procedure Save(Var Message); Message 'save';
		Procedure Load(Var Message); Message 'load';
		Procedure Focus(Var Message); Message 'focus';
		Procedure Up(Var Message); Message 'up';
		Procedure ListChilds(Var Message); Message 'list';
		Procedure ListCommands(Var Message); Message 'help';
		Procedure AddChild(Var Message); Message 'add';
		Procedure DeleteChild(Var Message); Message 'delete';
		Procedure SetProperty(Var Message); Message 'setproperty';
		Procedure SetText(Var Message); Message 'settext';
		Procedure DelProp(Var Message); Message 'deleteproperty';
		Procedure PurgeProp(Var Message); Message 'purgeproperties';
		Procedure Quit(Var Message); Message 'quit';
		Procedure Print(Var Message); Message 'print';
		Procedure SetVerbosity(Var Message); Message 'setverbosity';
		Procedure GetVerbosity(Var Message); Message 'verbosity';
		Procedure SetPrompt(Var Message); Message 'setprompt';
		{ Event Handlers }
		Procedure DoLoadFromFile(Const aFileName : String);
		Procedure DoSaveToFile(Const aFileName : String);
		Procedure DoFocus(Const aTarget : TXMLNode); Virtual;
		{ Properties }
		Property CommandLine : String Read fCommandLine;
		Property Stack: TXMLAccessStack Read fStack;
		Property Focused : TXMLNode Read fFocused;
		Property Running : Boolean Read fRunning;
		Property Verbosity : TXMLVerbosity Read fVerbosity Write fVerbosity;
		Property Prompt : String Read fPrompt Write fPrompt;
	End;	
	
	TXMLAccessStack = Class(TVirtualMachineStack)
	Private
		fConsole : TXMLCustomConsole;
	Public
		{ Function handlers }
		Procedure GetProperty(Var aMessage); Message 'getproperty';
		Procedure GetText(Var aMessage); Message 'gettext';
		Procedure Accept(Var aMessage); Message 'accept';
		Property Console : TXMLCustomConsole Read fConsole Write fConsole;
	End;

Function ConcatenateStrings(Const aStrings : TTokenList): String;

Implementation

Function ConcatenateStrings(Const aStrings : TTokenList): String;
Var
	lCtrl : Integer;
Begin
	Result := '';
	For lCtrl := Low(aStrings) To High(aStrings) Do
		If lCtrl <> High(aStrings) Then
			Result := Result + aStrings[lCtrl]  + ', '
		Else
			Result := Result + aStrings[lCtrl];
End;

{ TXMLCustomConsole }

Constructor TXMLCustomConsole.Create;
Begin
	Inherited Create;
	fRunning := True;
	fStack := TXMLAccessStack.Create;
	fStack.Console := Self;
	fVerbosity[mkDebug] := True;
	fVerbosity[mkError] := True;
	fVerbosity[mkResponse] := True;
	fVerbosity[mkInfo] := True;
	fPrompt := '$F>';
End;

Destructor TXMLCustomConsole.Destroy;
Begin
	fStack.Free;
	Inherited Destroy;
End;

Procedure TXMLCustomConsole.Process;
Var
	lTmp : String;
	lParser : TExpressionParser;
Begin
	Try
		Try
			lTmp := ReadLine;
			If Length(lTmp) > 0 Then
			Begin
				fCommandLine := Copy2SymbDel(lTmp, ' ');
				lParser := TExpressionParser.Create(lTmp, fStack);
				If Length(lTmp) > 0 Then
					lParser.Evaluate(False);
				If lParser.Error Then
					OutLn(mkError, 'At Expression "' + lTmp + '" column ' + IntToStr(lParser.ErrorCol) + ' got : ' + lParser.ErrorString)
				Else
					Submit;
			End
			Else
				OutLn(mkError, 'Empty command line.');
		Finally
			lParser.Free;
		End;
	Except
		On E : Exception Do
			OutLn(mkError, E.Message);
	End;
End;

Function TXMLCustomConsole.AtLeast(Const aCount : Integer): Boolean;
Begin
	Result := fStack.Top.AtLeast(aCount);
End;

Procedure TXMLCustomConsole.Submit;
Var
	lMsg : TXMLObjectMessage;
Begin
	lMsg.MsgStr := fCommandLine;
	lMsg.Data := Nil;
	DispatchStr(lMsg);
End;

Procedure TXMLCustomConsole.DefaultHandlerStr(Var Message);
Begin
	OutLn(mkError, 'No handler for the command : ' + fCommandLine);
	fStack.Purge;
End;

Procedure TXMLCustomConsole.Save(Var Message);
Begin
	If AtLeast(1) Then
	Begin
		fStack.Enter(1);
		OutLn(mkInfo, 'Saving to file ' + fStack.Top.Peek(0));
		DoSaveToFile(fStack.Top.Top);
		OutLn(mkInfo, 'Done.');
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Not enough parameters.');
		OutLn(mkInfo, 'Format : ');
		OutLn(mkInfo, ' SAVE <filename>');
	End;
	fStack.Purge;
End;

Procedure TXMLCustomConsole.Load(Var Message);
Begin
	If AtLeast(1) Then
	Begin
		fStack.Enter(1);
		If FileExists(fStack.Top.Peek(0)) Then
		Begin
			OutLn(mkInfo, 'Loading from file ' + fStack.Top.Peek(0));
			DoLoadFromFile(fStack.Top.Top);
			OutLn(mkInfo, 'Done.');
		End
		Else
			OutLn(mkError, 'Cannot find file : "' + fStack.Top.Peek(0) + '".');
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Not enough parameters.');
		OutLn(mkInfo, 'Format : ');
		OutLn(mkInfo, ' LOAD <filename>');
	End;
End;

Procedure TXMLCustomConsole.Focus(Var Message);
Var
	lTarget : TXMLNode;
Begin
	If AtLeast(1) Then
	Begin
		fStack.Enter(1);
		lTarget := fFocused.Locate(fStack.Top.Peek(0)) As TXMLNode;
		If Assigned(lTarget) Then
			fFocused := lTarget
		Else
			OutLn(mkError, 'Cannot locate node : "' + fStack.Top.Peek(0) + '".');
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Please specify target.');
		OutLn(mkInfo, 'Format :');
		OutLn(mkInfo, ' FOCUS <dom>');
	End;
End;

Procedure TXMLCustomConsole.Up(Var Message);
Begin
	If Assigned(fFocused.Owner) Then
		fFocused := fFocused.Owner As TXMLNode
	Else
		OutLn(mkError, 'Cannot go upper than root node.');
End;

Procedure TXMLCustomConsole.ListChilds(Var Message);
Begin
	OutLn(mkInfo, 'Listing childs :');
	fFocused.First;
	While Not(fFocused.IsAfterLast) Do
	Begin
		OutLn(mkResponse, '"' + (fFocused.GetCurrent As TXMLNode).IndexedName + '", "' + IntToStr(fFocused.GetCurrentIndex) + '"');
		fFocused.Next;
	End;
	OutLn(mkInfo, 'Done.');
End;

Procedure TXMLCustomConsole.ListCommands(Var Message);
Var
	lCtrl : Integer;
	lCommands : TTokenList;
	lClass : TClass;
Begin
	SetLength(lCommands, 0);
	lClass := Self.ClassType;
	While Assigned(lClass) Do
	Begin
		If lClass.StringMessageTable <> Nil Then
			If lClass.StringMessageTable^.Count > 0 Then
				For lCtrl := 0 To lClass.StringMessageTable^.Count - 1 Do
				Begin
						SetLength(lCommands, Length(lCommands) + 1);
						lCommands[High(lCommands)] := UpCase((lClass.StringMessageTable^.MsgStrTable[lCtrl].Name^));
				End;
		lClass := lClass.ClassParent;
	End;
	OutLn(mkInfo, 'Available Commands :');
	OutLn(mkResponse, '"' + ConcatenateStrings(lCommands) + '"');
End;

Procedure TXMLCustomConsole.AddChild(Var Message);
Begin
	If AtLeast(1) Then
	Begin
		fStack.Enter(1);
		OutLn(mkInfo, 'Creating a child with tag ' + fStack.Top.Peek(0));
		(XMLClassFactory.Build(fStack.Top.Peek(0), fFocused) As TXMLNode).Name := fStack.Top.Peek(0);
		OutLn(mkInfo, 'Done.');
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Must specify child class');
		OutLn(mkInfo, 'Format :');
		OutLn(mkInfo, ' ADD <tag>');
	End;
End;

Procedure TXMLCustomConsole.DeleteChild(Var Message);
Var
	lTargetOwner,
	lTarget : TXMLNode;
Begin
	If AtLeast(1) Then
	Begin
		fStack.Enter(1);
		lTarget := fFocused.Locate(fStack.Top.Peek(0)) As TXMLNode;
		If Assigned(lTarget) Then
			If lTarget <> (fFocused.FindRoot As TXMLNode) Then
			Begin
				OutLn(mkInfo, 'Deleting node "' + fStack.Top.Peek(0) + '".');
				If fFocused = lTarget Then
					fFocused := lTarget.Owner As TXMLNode;
				lTargetOwner := lTarget.Owner As TXMLNode;
				lTargetOwner.Delete(lTarget);
				OutLn(mkInfo, 'Done.');
			End
			Else
				OutLn(mkError, 'Cannot delete the root node.')
		Else
			OutLn(mkError, 'Cannot locate node.');
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Must specify child name');
		OutLn(mkInfo, 'Format : ');
		OutLn(mkInfo, ' DELETE <dom>');
	End;
End;

Procedure TXMLCustomConsole.SetProperty(Var Message);
Begin
	If AtLeast(2) Then
	Begin
		fStack.Enter(2);
		fStack.Enter(2);
		(fFocused As TXMLNode).Properties.SetValue(fStack.Top.Peek(0), fStack.Top.Peek(1));
		fStack.Enter(2);
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Not enough parameters.');
		OutLn(mkInfo, 'Format : ');
		OutLn(mkInfo, ' SETPROPERTY <propertyname>, <propertyvalue>');
	End;
End;

Procedure TXMLCustomConsole.SetText(Var Message);
Begin
	If AtLeast(1) Then
	Begin
		fStack.Enter(1);
		fFocused.SetTextChild(fStack.Top.Peek(0));
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Not enough parameters.');
		OutLn(mkInfo, 'Format : ');
		OutLn(mkInfo, ' SETTEXT <propertyname>, <propertyvalue>');
	End;
End;

Procedure TXMLCustomConsole.DelProp(Var Message);
Begin
	If AtLeast(1) Then
	Begin
		fStack.Enter(1);
		fFocused.Properties.Delete(fFocused.Properties.Find(fStack.Top.Peek(0)));
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Not enough parameters.');
		OutLn(mkInfo, 'Format :');
		OutLn(mkInfo, ' DELPROP <propertyname>');
	End;
End;

Procedure TXMLCustomConsole.PurgeProp(Var Message);
Begin
	OutLn(mkDebug, 'Deleting all properties of current node.');
	fFocused.Properties.Purge;
End;

Procedure TXMLCustomConsole.Quit(Var Message);
Begin
	OutLn(mkInfo, 'Quitting');
	fRunning := False;
End;

Procedure TXMLCustomConsole.SetVerbosity(Var Message);
Begin
	If AtLeast(2) Then
	Begin
		fStack.Enter(2);
		fStack.Enter(2);
		If LowerCase(fStack.Top.Peek(0)) = 'debug' Then
			fVerbosity[mkDebug] := fStack.Top.Peek(1)
		Else If LowerCase(fStack.Top.Peek(0)) = 'error' Then
			fVerbosity[mkError] := fStack.Top.Peek(1)
		Else If LowerCase(fStack.Top.Peek(0)) = 'response' Then
			fVerbosity[mkResponse] := fStack.Top.Peek(1)
		Else If LowerCase(fStack.Top.Peek(0)) = 'info' Then
			fVerbosity[mkInfo] := fStack.Top.Peek(1);
		fStack.Leave(0);
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Not enough parameters.');
		OutLn(mkInfo, 'Format : ');
		OutLn(mkInfo, ' SETVERBOSE { ''debug'' | ''error'' | ''response'' | ''info'' }, { ''true'' | ''false'' }');
	End;
End;

Procedure TXMLCustomConsole.GetVerbosity(Var Message);
Begin
	OutLn(mkInfo, 'Verbosity options :');
	If fVerbosity[mkDebug] Then
		OutLn(mkResponse, 'DEBUG = TRUE')
	Else
		OutLn(mkResponse, 'DEBUG = FALSE');
	If fVerbosity[mkError] Then
		OutLn(mkResponse, 'ERROR = TRUE')
	Else
		OutLn(mkResponse, 'ERROR = FALSE');
	If fVerbosity[mkResponse] Then
		OutLn(mkResponse, 'RESPONSE = TRUE')
	Else
		OutLn(mkResponse, 'RESPONSE = FALSE');
	If fVerbosity[mkInfo] Then
		OutLn(mkResponse, 'INFO = TRUE')
	Else
		OutLn(mkResponse, 'INFO = FALSE');
End;

Procedure TXMLCustomConsole.SetPrompt(Var Message);
Begin
	If AtLeast(1) Then
	Begin
		fStack.Enter(1);
		fPrompt := fStack.Top.Peek(0);
		fStack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Must specify child class');
		OutLn(mkInfo, 'Format :');
		OutLn(mkInfo, ' SETPROMPT <prompt>');
	End;
End;

Procedure TXMLCustomConsole.Print(Var Message);
Begin
	While fStack.Top.AtLeast(1) Do
		OutLn(mkResponse, fStack.Top.Pop);
End;

Procedure TXMLCustomConsole.DoLoadFromFile(Const aFileName : String);
Var
	lFile : TFileStream;
	lSource : TXMLSource;
	lTokens : TMemoryStream;
	lScanner : TXMLScanner;
	lTokenIterator : TXMLTokenIterator;
	lParser : TXMLParser;
Begin
	fFocused.Purge;
	lFile := Nil;
	lSource := Nil;
	lTokens := Nil;
	lScanner := Nil;
	lTokenIterator := Nil;
	lParser := Nil;
	Try
		lFile := TFileStream.Create(aFileName, fmOpenRead);
		lSource := TXMLSource.Create(lFile, False);
		lTokens := TMemoryStream.Create;
		lScanner := TXMLScanner.Create(lSource, lTokens, False);
		lScanner.Scan;
		lTokenIterator := TXMLTokenIterator.Create(lTokens, False);
		lParser := TXMLParser.Create(lTokenIterator, fFocused, False);
		lParser.Parse;
	Finally
		FreeAndNil(lFile);
		FreeAndNil(lSource);
		FreeAndNil(lTokens);
		FreeAndNil(lScanner);
		FreeAndNil(lTokenIterator);
		FreeAndNil(lParser);
	End;
End;

Procedure TXMLCustomConsole.DoSaveToFile(Const aFileName : String);
Var
	lFile : TStringList;
	lAsXMLIterator : TXMLAsTextIterator;
Begin
	lFile := Nil;
	lAsXMLIterator := Nil;
	Try
		lFile := TStringList.Create;
		lAsXMLIterator := TXMLAsTextIterator.Create;
		lAsXMLIterator.Visit(fFocused);
		lFile.Text := lAsXMLIterator.Output;
		lFile.SaveToFile(aFileName);
	Finally
		FreeAndNil(lAsXMLIterator);
		FreeAndNil(lFile);
	End;
End;

Procedure TXMLCustomConsole.DoFocus(Const aTarget : TXMLNode);
Begin
	fFocused := aTarget;
End;

{ TXMLAccessStack }

Procedure TXMLAccessStack.GetProperty(Var aMessage);
Begin
	Top.Push((fConsole.Focused.Locate(Top.Pop) As TXMLNode).Properties.GetValue(Top.Pop));
End;

Procedure TXMLAccessStack.GetText(Var aMessage);
Begin
	Top.Push((fConsole.Focused.Locate(Top.Pop) As TXMLNode).GetTextChild);
End;

Procedure TXMLAccessStack.Accept(Var aMessage);
Var
	lTmp : String;
Begin
	ReadLn(lTmp);
	Top.Push(lTmp);
End;

End.