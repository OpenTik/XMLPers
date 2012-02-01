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
	HTMLParser;
	
Interface

Uses
	Classes,
	SysUtils,
	XMLScanner,
	HTMLNodes;

Type
	THTMLParser = Class
	Private
		fOwnsSource : Boolean;
		fSource : TXMLTokenIterator;
		fDestination : THTMLNode;
		Procedure Mark(aNode : THTMLNode);
		Procedure ParseTagList(Const aPrevious : THTMLNode);
		Procedure ParseTag(Const aPrevious : THTMLNode);
		Procedure ParsePropertyList(Const aPrevious : THTMLNode);
		Procedure ParseProperty(Const aPrevious : THTMLNode);
		Procedure ParseText(Const aPrevious : THTMLNode);
		Procedure ParseSpecialTag1(Const aPrevious : THTMLNode);
		Procedure ParseSpecialTag2(Const aPrevious : THTMLNode);
	Public
		Constructor Create(Const aSource : TXMLTokenIterator; aDestination : THTMLNode; Const aOwnsSource : Boolean = True); Virtual;
		Destructor Destroy; Override;
		Procedure Parse;
	End;

	THTMLRootNode = Class(THTMLNode)
	Public
		Procedure DoLoadFromFile(Const aFileName : String);
	End;

Implementation

Procedure THTMLParser.Mark(aNode : THTMLNode);
Begin
	aNode.Row := fSource.Row;
	aNode.Col := fSource.Col;
End;

Procedure THTMLParser.ParseTagList(Const aPrevious : THTMLNode);
Begin
	// Debug WriteLn('Parsing tag list.');
	While Not(fSource.IsEOS Or (fSource.Kind = tkXMLEOF)) Do
		If fSource.Match([tkXMLText]) Then
			ParseText(aPrevious)
		Else If fSource.Match([tkXMLOpenTag, tkXMLIdentifier]) Then
			ParseTag(aPrevious)
		Else If fSource.Match([tkXMLOpenTag, tkXMLQuestion]) Then
			ParseSpecialTag1(aPrevious)
		Else If fSource.Match([tkXMLOpenTag, tkXMLExclamation]) Then
			ParseSpecialTag2(aPrevious)
		Else If fSource.Match([tkXMLOpenTag, tkXMLSlash]) Then
			Exit
		Else
			fSource.RaiseError('Expected tag or text.');
	// Debug WriteLn('Done.');
End;

Procedure THTMLParser.ParseTag(Const aPrevious : THTMLNode);
Var
	lTag : THTMLNode;
Begin
	// Debug WriteLn('Parsing tag.');
	fSource.Consume(tkXMLOpenTag);
	lTag := HTMLClassFactory.Build(fSource.Literal, aPrevious);
	Mark(lTag);
	lTag.Name := fSource.Extract(tkXMLIdentifier);	
	ParsePropertyList(lTag);
	If HTMLClassFactory.TagHasChilds(lTag.Name) Then
	Begin
		fSource.Consume(tkXMLCloseTag);
		ParseTagList(lTag);
		fSource.Consume(tkXMLOpenTag);
		fSource.Consume(tkXMLSlash);
		fSource.Consume(lTag.Name);
		fSource.Consume(tkXMLCloseTag);
	End
	Else
		fSource.Consume(tkXMLCloseTag);
	// Debug WriteLn('Done.');
End;

Procedure THTMLParser.ParsePropertyList(Const aPrevious : THTMLNode);
Begin
	// Debug WriteLn('Parsing property list.');
	While Not(fSource.Expected([tkXMLExclamation, tkXMLQuestion, tkXMLSlash, tkXMLCloseTag])) Do
	Begin
		fSource.Consume(tkXMLWhite, False);
		ParseProperty(aPrevious);
		If fSource.Expected(tkXMLEOF) Or fSource.IsEOS Then
			fSource.RaiseError('Unexpected end of file.');
	End;
	// Debug WriteLn('Done.');
End;

Procedure THTMLParser.ParseProperty(Const aPrevious : THTMLNode);
Var
	lName,
	lValue : String;
Begin
	// Debug WriteLn('Parsing property.');
	lName := fSource.Extract(tkXMLIdentifier);
	fSource.Consume(tkXMLEqual);
	lValue := fSource.Extract(tkXMLString);
	aPrevious.Properties.SetValue(lName, lValue);
	// Debug WriteLn('Done.');
End;

Procedure THTMLParser.ParseText(Const aPrevious : THTMLNode);
Var
	lTag : THTMLTextNode;
Begin
	// Debug WriteLn('Parsing text.');
	lTag := THTMLTextNode.Create(aPrevious);
	lTag.Name := '';
	lTag.Content := fSource.Extract(tkXMLText);
	// Debug WriteLn('Done.');
End;

Procedure THTMLParser.ParseSpecialTag1(Const aPrevious : THTMLNode);
Var
	lTag : THTMLSpecialTag1Node;
Begin
	// Debug WriteLn('Parsing special tag (<?something?>).');
	fSource.Consume(tkXMLOpenTag);
	fSource.Consume(tkXMLQuestion);
	lTag := THTMLSpecialTag1Node.Create(aPrevious);
	Mark(lTag);
	lTag.Name := fSource.Extract(tkXMLIdentifier);
	ParsePropertyList(lTag);
	fSource.Consume(tkXMLQuestion);
	fSource.Consume(tkXMLCloseTag);
	// Debug WriteLn('Done.');
End;

Procedure THTMLParser.ParseSpecialTag2(Const aPrevious : THTMLNode);
Var
	lTag : THTMLSpecialTag2Node;
Begin
	// Debug WriteLn('Parsing special tag (<!something!>).');
	fSource.Consume(tkXMLOpenTag);
	fSource.Consume(tkXMLExclamation);
	lTag := THTMLSpecialTag2Node.Create(aPrevious);
	Mark(lTag);
	lTag.Name := fSource.Extract(tkXMLIdentifier);
	ParsePropertyList(lTag);
	fSource.Consume(tkXMLExclamation);
	fSource.Consume(tkXMLCloseTag);
	// Debug WriteLn('Done.');
End;

Constructor THTMLParser.Create(Const aSource : TXMLTokenIterator; aDestination : THTMLNode; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fOwnsSource := aOwnsSource;
	fSource := aSource;
	fDestination := aDestination;
End;

Destructor THTMLParser.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	Inherited Destroy;
End;

Procedure THTMLParser.Parse;
Begin
	fSource.Start;
	// Debug WriteLn('Parsing file.');
	ParseTagList(fDestination);
	// Debug WriteLn('Done.');
	fSource.Consume(tkXMLEOF);
End;

Procedure THTMLRootNode.DoLoadFromFile(Const aFileName : String);
Var
	lTokens : TMemoryStream;
Begin
	lTokens := TMemoryStream.Create;
	With TXMLScanner.Create(TXMLSource.Create(TFileStream.Create(aFileName, fmOpenRead)), lTokens) Do
	Begin
		Try
			Scan;
		Finally
			Free;
		End;
	End;
	With THTMLParser.Create(TXMLTokenIterator.Create(lTokens), Self) Do
	Begin
		Try
			Parse;
		Finally
			Free;
		End;
	End;
End;

End.