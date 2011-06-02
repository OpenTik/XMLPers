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

Uses
	Classes,
	SysUtils,
	XMLScanner,
	XMLParser,
	XMLNodes,
	XMLConsole,
	XMLConsoleWithTemplates;

Var
	gConfigXML : TXMLRootNode;
	gConsoleXML : TXMLScreenConsoleWithTemplates;

Begin
	gConfigXML := TXMLRootNode.Create(Nil);
	gConsoleXML := TXMLScreenConsoleWithTemplates.Create;
	gConsoleXML.DoFocus(gConfigXML);
	gConsoleXML.DoLoadFromFile(ParamStr(1));
	While gConsoleXML.Running Do
		gConsoleXML.Process;
	gConsoleXML.Free;
	gConfigXML.Free;
End.