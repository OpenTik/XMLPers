Unit
	XMLConsoleWithTemplates;

Interface

Uses
	Classes,
	SysUtils,
	XMLConsole,
	XMLScanner,
	HTMLParser,
	HTMLNodes;


Type
	TXMLConsoleWithTemplates = Class(TXMLCustomConsole)
	Public
		Procedure PrintTemplate(Var Message); Message 'printtemplate';
		Procedure ProcessTemplate(Var Message); Message 'processtemplate';
		Procedure DoPrintTemplate(Const aInputFileName : String);
		Procedure DoProcessTemplate(Const aInputFileName, aOutputFileName : String);
	End;

	TXMLScreenConsoleWithTemplates = Class(TXMLConsoleWithTemplates)
	Public
		Function ReadLine: String; Override;
		Procedure Out(Const aKind : TXMLConsoleMessageKind; Const aLine : String); Override;
		Procedure OutLn(Const aKind : TXMLConsoleMessageKind; Const aLine : String); Override;
	End;

Implementation

{ TXMLConsoleWithTemplates }

Procedure TXMLConsoleWithTemplates.PrintTemplate(Var Message);
Begin
	If AtLeast(1) Then
	Begin
		Stack.Enter(1);
		DoPrintTemplate(Stack.Top.Peek(0));
		Stack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Not enough parameters.');
		OutLn(mkInfo, 'Format :');
		OutLn(mkInfo, ' PRINTTEMPLATE <templatefile>');
	End;
End;

Procedure TXMLConsoleWithTemplates.ProcessTemplate(Var Message);
Begin
	If AtLeast(2) Then
	Begin
		Stack.Enter(2);
		Stack.Enter(2);
		DoProcessTemplate(Stack.Top.Peek(0), Stack.Top.Peek(1));
		Stack.Leave(0);
		Stack.Leave(0);
	End
	Else
	Begin
		OutLn(mkError, 'Not enough parameters.');
		OutLn(mkInfo, 'Format :');
		OutLn(mkInfo, ' PRINTTEMPLATE <templatefile>, <outputfile>');
	End;
End;

Procedure TXMLConsoleWithTemplates.DoPrintTemplate(Const aInputFileName : String);
Var
	lTemplate : THTMLRootNode;
	lFile : TFileStream;
	lSource : TXMLSource;
	lTokens : TMemoryStream;
	lScanner : TXMLScanner;
	lTokenIterator : TXMLTokenIterator;
	lParser : THTMLParser;
Begin
	lFile := Nil;
	lSource := Nil;
	lTokens := Nil;
	lScanner := Nil;
	lTokenIterator := Nil;
	lParser := Nil;
	Try
		lTemplate := THTMLRootNode.Create(Nil);
		lFile := TFileStream.Create(aInputFileName, fmOpenRead);
		lSource := TXMLSource.Create(lFile, False);
		lTokens := TMemoryStream.Create;
		lScanner := TXMLScanner.Create(lSource, lTokens, False);
		lScanner.Scan;
		lTokenIterator := TXMLTokenIterator.Create(lTokens, False);
		lParser := THTMLParser.Create(lTokenIterator, lTemplate, False);
		lParser.Parse;
		lTemplate.PropagateConsole(Self);
		OutLn(mkResponse, '$' + lTemplate.AsString + #13#10);
	Finally
		FreeAndNil(lFile);
		FreeAndNil(lSource);
		FreeAndNil(lTokens);
		FreeAndNil(lScanner);
		FreeAndNil(lTokenIterator);
		FreeAndNil(lParser);
		FreeAndNil(lTemplate);
	End;
End;

Procedure TXMLConsoleWithTemplates.DoProcessTemplate(Const aInputFileName, aOutputFileName : String);
Var
	lTemplate : THTMLRootNode;
	lFile : TFileStream;
	lSource : TXMLSource;
	lTokens : TMemoryStream;
	lScanner : TXMLScanner;
	lTokenIterator : TXMLTokenIterator;
	lParser : THTMLParser;
	lFileOutput : TStringList;
Begin
	lFile := Nil;
	lSource := Nil;
	lTokens := Nil;
	lScanner := Nil;
	lTokenIterator := Nil;
	lParser := Nil;
	lFileOutput := Nil;
	Try
		lTemplate := THTMLRootNode.Create(Nil);
		lFile := TFileStream.Create(aInputFileName, fmOpenRead);
		lSource := TXMLSource.Create(lFile, False);
		lTokens := TMemoryStream.Create;
		lScanner := TXMLScanner.Create(lSource, lTokens, False);
		lScanner.Scan;
		lTokenIterator := TXMLTokenIterator.Create(lTokens, False);
		lParser := THTMLParser.Create(lTokenIterator, lTemplate, False);
		lParser.Parse;
		lTemplate.PropagateConsole(Self);
		lFileOutput := TStringList.Create;
		lFileOutput.Duplicates := dupAccept;
		lFileOutput.Sorted := False;
		lFileOutput.Text := lTemplate.AsString;
		lFileOutput.SaveToFile(aOutputFileName);
	Finally
		FreeAndNil(lFile);
		FreeAndNil(lSource);
		FreeAndNil(lTokens);
		FreeAndNil(lScanner);
		FreeAndNil(lTokenIterator);
		FreeAndNil(lParser);
		FreeAndNil(lTemplate);
	End;
End;

{ TXMLScreenConsoleWithTemplates }

Function TXMLScreenConsoleWithTemplates.ReadLine: String;
Var
	lTmp : String;
Begin
	lTmp := Prompt;
	lTmp := StringReplace(lTmp, '$F', Focused.IndexedName, [ rfReplaceAll ]);
	lTmp := StringReplace(lTmp, '$T', TimeToStr(Now), [ rfReplaceAll ]);
	lTmp := StringReplace(lTmp, '$D', DateToStr(Now), [ rfReplaceAll ]);
	Write(lTmp);
	ReadLn(lTmp);
	Result := lTmp;
End;

Procedure TXMLScreenConsoleWithTemplates.Out(Const aKind : TXMLConsoleMessageKind; Const aLine : String);
Begin
	Case aKind Of
		mkDebug :
			If Verbosity[mkDebug] Then
			Begin
				Write('[D] ');
				Write(aLine);
			End;
		mkError :
			If Verbosity[mkError] Then
			Begin
				Write('[E] ');
				Write(aLine);
			End;
		mkResponse :
			If Verbosity[mkResponse] Then
			Begin
				Write('[R] ');
				Write(aLine);
			End;
		mkInfo:
			If Verbosity[mkInfo] Then
			Begin
				Write('[I] ');
				Write(aLine);
			End;
	End;
End;

Procedure TXMLScreenConsoleWithTemplates.OutLn(Const aKind : TXMLConsoleMessageKind; Const aLine : String);
Begin
	Case aKind Of
		mkDebug :
			If Verbosity[mkDebug] Then
			Begin
				Write('[D] ');
				WriteLn(aLine);
			End;
		mkError :
			If Verbosity[mkError] Then
			Begin
				Write('[E] ');
				WriteLn(aLine);
			End;
		mkResponse :
			If Verbosity[mkResponse] Then
			Begin
				Write('[R] ');
				WriteLn(aLine);
			End;
		mkInfo:
			If Verbosity[mkInfo] Then
			Begin
				Write('[I] ');
				WriteLn(aLine);
			End;
	End;
End;

End.