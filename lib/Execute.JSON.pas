unit Execute.JSON;

(*
  TypInfo based JSON parser/builder for Delphi XE8 (c)2015 by Execute SARL

  Paul TOTH <contact@execute.fr>
  http://www.execute.fr

  ------------------------------------------------------------------------
  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
  ------------------------------------------------------------------------

  2015-08-09 : version 1: toJSON(@v, TypeOf(t))
  2015-08-09 : version 2: JSON<T>.toJSON(v)
  2015-08-10 : version 3: JSON.toJSON<T>(v) or JSON.toJSON(v) with type inference
  2015-09-07 : version 4: .fromURL()

  uses
    Execute.JSON;

  type
    TMyObject = class
      Name: string;
      Date: TDateTime;
      Nums: array of Integer;
      Plus: string;
    end;


    TMyRecord = record
      Name: string;
      Date: TDateTime;
      Nums: array of Integer;
      Minus: string; // not the same field as TMyObject !
    end;

  var
    t: TMyObject;
    r: TMyRecord;
    s: string;
  begin
    t := TMyObject.Create;
    t.Name := 'Paul';
    t.Date := Now();
    t.Nums := [1, 2, 3];
    t.Plus := 'plus';
    s := t.toJSON();
    t.Free;

  // s = '{"Name":"Paul","Date":"2015-08-07T17:32:27","Nums":[1,2,3],"Plus":"plus"}';

    t := TMyObject.Create;
    t.FromJSON(s);
    t.Free;

    s := JSON.fromJSON(r, s);  // "Plus" is ignored, "Minus" still empty

    s := JSON.toJSON(r);

  // s = '{"Name":"Paul","Date":"2015-08-07T17:32:27","Nums":[1,2,3],"Minus":null}';
  end;

  you can define how class fields are handled with predefined methods

type
   TMyObject = class
      Name: string;
      Date: TDateTime;
      Nums: array of Integer;
      List: TStringList;
      constructor Create;
      destructor Destroy; override;
      function JSONBuildField(const Name: string; Builder: TJSONBuilder): Boolean;
      function JSONParseField(const Name: string; Parser: TJSONParser): Boolean;
    end;

constructor TMyObject.create;
begin
  inherited;
  List := TStringList.Create;
end;

destructor TMyObject.Destroy;
begin
  List.Free;
  inherited;
end;

function TMyObject.JSONBuildField(const Name: string; Builder: TJSONBuilder): Boolean;
begin
  if Name = 'List' then
  begin
    Builder.AppendStrings(List);
    Exit(True);
  end;

  Result := False;
end;

function TMyObject.JSONParseField(const Name: string; Parser: TJSONParser): Boolean;
begin
  if Name = 'List' then
  begin
    Parser.GetStrings(List);
    Exit(True);
  end;

  Result := False;
end;

NB: you can use Builder.AppendObject() and Parser.ParseObject() to handle sub objects

This library DO NOT CREATE sub objects !

WARNING : inline fixed sized array do not have RTTI information

  do not use

    r = record
      a: array[0..5] of Integer;
    end;

  use

    ta = array[0..5] of Integer;

    r = record
      a: ta;
    end;

*)

interface
{$WARN WIDECHAR_REDUCED OFF}
uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.TypInfo,
  Execute.RTTI;

const
  JSON_TRUE  = 'true';
  JSON_FALSE = 'false';
  JSON_NULL  = 'null';
  JSON_NIL   = JSON_NULL;

  JSON_BOOL  : array[False..True] of string = (JSON_FALSE, JSON_TRUE);

type
  EJSONError = class(Exception)
  end;

  // Build part let build a JSON string from a Delphi variable

  PBuildChain = ^TBuildChain;

  // add some functions to TStringBuilder for JSON
  // keep the "Build Chain"
  TJSONBuilder = class(TStringBuilder)
  private
    FBuild: PBuildChain;
    function KnowInstance(Instance: Pointer): Boolean;
    function BuildField(Name: string): Boolean;
    function GetPath: string;

    procedure BuildJSON(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONClass(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONRecord(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONDynArray(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONArray(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONSet(TypeInfo: PTypeInfo; Instance: Pointer);

  public
    procedure AppendString(const Str: string);
    procedure AppendStrings(List: TStrings);
    procedure AppendObject(Obj: TObject); // to build JSON part of a subObject
    function BeginArray(Count: Integer): Boolean;
    procedure EndArray();
  end;

  // Each time an object is converted to JSON, it's JSONBuildField method is saved with its instance and path
  // a child object can save it's own properties with a relative path name "childField"
  // a parent objet can save sub-objects properties with a global path name "parentField.childField"
  TBuildChain = record
    Build : function(Self: Pointer; const Name: string; Builder: TJSONBuilder): Boolean;
    Self  : Pointer;
    Sender: TObject;
    Path  : string;
    Next  : PBuildChain;
  end;

  // Parser part let read back the JSON string

  PParseChain = ^TParseChain;

  TJSONParser = class
  private
    FText   : string;
    FIndex  : Integer;
    FEof    : Boolean;
    FParse  : PParseChain;
    function ParseField(Name: string): Boolean;

    procedure ParseJSON(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONClass(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONRecord(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONDynArray(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONArray(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONSet(TypeInfo: PTypeInfo; Instance: Pointer);
//    procedure ParseJSONShortString(TypeInfo: PTypeInfo; Instance: Pointer);

  public
    constructor Create(const AText: string);
    procedure ParseObject(Obj: TObject);  // to parse a subObject
    procedure ParseType(Instance, TypeInfo: Pointer); inline;
    function NextChar: Char;
    function ReadChar: Char;
    function Skip(Ch: Char): Boolean;
    procedure Drop(Ch: Char);
    procedure Blanks;
    procedure DropKey(const Str: string);
    function SkipStr(const Str: string): Boolean;
    function GetKey: string;
    function GetValue: string;
    function GetString: string;
    function GetChar: Char;
    procedure GetStrings(List: TStrings);
    function GetNumber: string;
    function GetInteger: Integer;
    function GetInt64: Int64;
    function GetBoolean: Boolean;
    function GetFloat: Double;
    function GetCurrency: Currency;
    function GetDateTime: TDateTime;
    function BeginObject: Boolean;
    function EndObject: Boolean;
    function BeginArray: Boolean;
    function EndArray: Boolean;
    function OpenArray(var Count: Integer): Boolean;
    function CloseArray(Count: Integer): Boolean;
  end;

  // the ParseChain works like the BuildChain

  TParseChain = record
    Parse : function(Instance: Pointer; const Name: string; Parser: TJSONParser): Boolean;
    Self  : Pointer;
    Path  : string;
    Next  : PParseChain;
  end;

{ todo: check methods signatures

  TMethodParamInfo = record
    Flags: TParamFlags;
    Typed: PTypeInfo;
  end;

  TMethodSignature = record
    Params: TArray<TMethodParamInfo>;
    Return: PTypeInfo;
    Conv  : TCallConv;
  end;

  // JSONBuildField( [pfConst] TypeInfo(string), [] TypeInfo(TJSONBuilder)): [TypeInfo(Boolean)]; ccReg
}

// finally, this Object Helper simplify everything

  TObjectHelper = class helper for TObject
    function toJSON: string;
    procedure fromJSON(const Str: string);
  end;

  // thank's to Stefan Glienke to point me out the type inference !
  JSON = class
    class var CountList: Boolean;
    class var TimeSeparator: Char;
    class function buildJSON(TypeInfo: PTypeInfo; Instance: Pointer): string;
    class function toJSON<T>(const instance :T): string; inline;
    class procedure fromJSON<T>(var instance: T; const Str: string); inline;
    class procedure toFile<T>(const instance :T; const FileName: string); inline;
    class procedure fromFile<T>(var instance: T; const FileName: string); inline;
    class function LoadFromFile(const AFileName: string): string;
    class procedure SaveToFile(const AFileName, Str: string);
  end;

var
  JSONFormat: TFormatSettings;

function JSONEscape(const Str: string): string;
function JSONDecodeDateTime(const Str: string): TDateTime;

implementation

function GetNum(const Str: string; Index, Count: Integer): Integer;
var
  Num: Integer;
begin
  Result := 0;
  while Count > 0 do
  begin
    Num := Ord(Str[Index]) - Ord('0');
    if (Num < 0) or (Num > 9) then
      raise EJSONError.Create('Invalid date ' + Str);
    Result := 10 * Result + Num;
    Inc(Index);
    Dec(Count);
  end;
end;

function JSONDecodeDateTime(const Str: string): TDateTime;
// "2012-04-23T18:25:43"
var
  y, m, d, h, s: Integer;
begin
//  Result := 0;
  if Str = '' then
    Exit(0);

  // Time only
  if (Length(Str) = 8) and (Str[3] = ':') and (Str[6] = ':') then
  begin
    h := GetNum(Str, 1, 2);
    m := GetNum(Str, 4, 2);
    s := GetNum(Str, 7, 2);
    Result := EncodeTime(h, m, s, 0);
    Exit;
  end;

  // Date required
  if (Length(Str) < 10) or (Str[5] <> '-') or (Str[8] <> '-') then
    raise EJSonError.Create('Invalid date ' + Str);

  y := GetNum(Str, 1, 4);
  m := GetNum(Str, 6, 2);
  d := GetNum(Str, 9, 2);
  if (y = 0) and (m = 0) and (d = 0) then
    Result := 0
  else
    Result := EncodeDate(y, m, d);

  // DateTime ?
  if (Length(Str) > 10) then
  begin
    if (Length(Str) < 19) or ((Str[11] <> 'T') and (Str[11] <> ' ')) or (Str[14] <> ':') or (Str[17] <> ':') then
      raise EJSONError.Create('Invalid date/time ' + Str);
    h := GetNum(Str, 12, 2);
    m := GetNum(Str, 15, 2);
    s := GetNum(Str, 18, 2);
    Result := Result + EncodeTime(h, m, s, 0);
  end;
end;

class function JSON.LoadFromFile(const AFileName: string): string;
var
  Stream: TFileStream;
  Buffer: TBytes;
begin
  Result := '';
  if FileExists(AFileName) then
  begin
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Buffer, Stream.Size);
      Stream.Read(Buffer[0], Length(Buffer));
    finally
      Stream.Free;
    end;
    Result := TEncoding.UTF8.GetString(Buffer);
  end;
end;

class procedure JSON.SaveToFile(const AFileName, Str: string);
var
  Stream: TFileStream;
  Buffer: TBytes;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Buffer := TEncoding.UTF8.GetBytes(Str);
    Stream.Write(Buffer[0], Length(Buffer));
  finally
    Stream.Free;
  end;
end;

class function JSON.buildJSON(TypeInfo: PTypeInfo; Instance: Pointer): string;
var
  Builder: TJSONBuilder;
begin
  Builder := TJSONBuilder.Create;
  try
    Builder.BuildJSON(TypeInfo, Instance);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

class function JSON.toJSON<T>(const instance :T): string;
begin
  Result := buildJSON(TypeInfo(T), @Instance);
end;

class procedure JSON.fromJSON<T>(var instance: T; const Str: string);
var
  Parser: TJSONParser;
begin
  Finalize(instance);
  FillChar(instance, SizeOf(T), 0);
  if Str = '' then
    Exit;
  Parser := TJSONParser.Create(Str);
  try
    Parser.ParseJSON(TypeInfo(T), @Instance);
  finally
    Parser.Free;
  end;
end;

class procedure JSON.toFile<T>(const instance: T; const FileName: string);
begin
  JSON.SaveToFile(FileName, JSON.toJSON(instance));
end;

class procedure JSON.fromFile<T>(var instance: T; const FileName: string);
begin
  JSON.fromJSON<T>(instance, JSON.LoadFromFile(FileName));
end;

function JSONEscape(const Str: string): string;
var
  Index : Integer;
  Escape: Integer;
  Len   : Integer;
  Ch    : Char;
begin
  Escape := 0;
  Result := '"' + Str + '"';
  Len := Length(Result);
  for Index := 2 to Len - 1 do
    if (Result[Index] in ['\', '"', #13, #10]) then
    begin
      Result[Index] := '\';
      Inc(Escape);
    end;
  if Escape = 0 then
    Exit;
  Inc(Len, Escape);
  SetLength(Result, Len);
  Result[Len] := '"';
  Dec(Len);
  for Index := Length(Str) + 1 downto 2 do
  begin
    if Result[Index] = '\' then
    begin
      Ch := Str[Index - 1];
      if Ch = #10 then
        Ch := 'n'
      else
      if Ch = #13 then
        Ch := 'r';
      Result[Len] := Ch;
      Dec(Len);
      Dec(Escape);
      if Escape = 0 then
        Exit;
    end;
    Result[Len] := Result[Index];
    Dec(Len);
  end;
end;

function JSONEncodeDate(const Value: TDate): string;
begin
  if Value < 1 then
    Result := JSON_NULL
  else
    Result := FormatDateTime('''"''yyyy-mm-dd''"''', Value);
end;

function JSONEncodeTime(const Value: TTime): string;
begin
  Result := FormatDateTime('''"''hh:nn:ss''"''', Value);
end;

function JSONEncodeDateTime(const Value: TDate): string;
begin
  if Value < 1 then
    Result := JSON_NIL
  else
    Result := FormatDateTime('''"''yyyy-mm-dd"' + JSON.TimeSeparator + '"hh:nn:ss''"''', Value);
end;

function JSONEncodeFloat(const Value: Extended): string;
begin
  Result := FloatToStr(Value, JSONFormat);
end;

{ TJSONParser }

constructor TJSONParser.Create(const AText: string);
begin
  FText := AText;
  FIndex:= 1;
  FEof := FIndex > Length(FText);
end;

procedure TJSONParser.ParseObject(Obj: TObject);
begin
  ParseJSONClass(Obj.ClassType.ClassInfo, Obj);
end;

procedure TJSONParser.ParseType(Instance, TypeInfo: Pointer);
begin
  ParseJSON(TypeInfo, Instance);
end;

function TJSONParser.ParseField(Name: string): Boolean;
var
  Chain: PParseChain;
begin
  if FParse = nil then
    Exit(False);
  Chain := FParse;
  repeat
    if FParse = Chain then
      FParse.Path := Name + '.'
    else
      Name := Chain.Path + Name;
    Result := (@Chain.Parse <> nil) and Chain.Parse(Chain.Self, Name, Self);
    Chain := Chain.Next;
  until Result or (Chain = nil);
end;

function TJSONParser.NextChar: Char;
begin
  if FEof then
    raise EJSONError.Create('End of JSON stream');
  Result := FText[FIndex];
end;

function TJSONParser.ReadChar: Char;
begin
  Result := NextChar;
  Inc(FIndex);
  FEof := FIndex > Length(FText);
end;

function TJSONParser.Skip(Ch: Char): Boolean;
begin
  Result := (FEof = False) and (FText[FIndex] = Ch);
  if Result then
    ReadChar;
end;

procedure TJSONParser.Drop(Ch: Char);
begin
  if not Skip(Ch) then
    raise EJSONError.Create('Expected char "' + ch +'" not found in "' + Copy(FText, 1, FIndex) + '"');
end;

procedure TJSONParser.DropKey(const Str: string);
begin
  if GetKey <> Str then
    raise EJSONError.Create('expected key "' + Str + '" not found');
end;

function TJSONParser.SkipStr(const Str: string): Boolean;
var
  Start: Integer;
  Index: Integer;
begin
  Result := False;
  if FEof then
    Exit;
  Start := FIndex;
  if Start + Length(Str) > Length(FText) then
    Exit;
  for Index := 1 to Length(Str) do
  begin
    if FText[Start] <> Str[Index] then
      Exit;
    Inc(Start);
  end;
  Result := True;
  FIndex := Start;
  FEof := FIndex > Length(FText);
end;

function TJSONParser.GetKey: string;
begin
  Result := GetString;
  Drop(':');
end;

function TJSONParser.GetValue: string;
var
  count: Integer;
begin
  Blanks;
  if NextChar = '"' then
    Result := GetString
  else
  if SkipStr(JSON_NIL) then
    Result := ''
  else
  if SkipStr(JSON_TRUE) or SkipStr('"true"') then
    Result := '1'
  else
  if SkipStr(JSON_FALSE) or SkipStr('"false"') then
    Result := '0'
  else
  if OpenArray(count) then
  begin
    Result := '[';
    repeat
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetValue;
    until CloseArray(count);
    Result := Result + ']';
  end else
  if BeginObject then
  begin
    Result := '';
    repeat
      if Result <> '' then
        Result := Result + ',';
      Result := GetKey + '=' + GetValue;
    until EndObject;
    Result := Result + '}';
  end else
  begin
    Result := GetNumber;
  end;
end;

function TJSONParser.GetString: string;
var
  Start : Integer;
  Escape: Integer;
  Len   : Integer;
  Index : Integer;
  Ch    : Char;
begin
  Blanks;
  if SkipStr(JSON_NULL) then
    Exit('');
  Drop('"');
  Start := FIndex;
  Escape := 0;
  while not Skip('"') do
  begin
    if ReadChar = '\' then
    begin
      Inc(Escape);
      ReadChar;
    end;
  end;
  Len := FIndex - Start - 1;
  Result := Copy(FText, Start, Len);
  if Escape > 0 then
  begin
    Start := 0;
    Index := 1;
    while Index <= Len do
    begin
      Ch := Result[Index];
      if Ch = '\' then
      begin
        Inc(Index);
        Ch := Result[Index];
        case Ch of
          'n': Ch := #10;
          'r': Ch := #13;
          'u': // \uHHHH
          begin
            Ch := Char(StrToInt('$' + Copy(Result, Index + 1, 4)));
            Inc(Index, 4);
          end;
        end;
      end;
      Inc(Start);
      if Index > Start then
        Result[Start] := Ch;
      Inc(Index);
    end;
    SetLength(Result, Start);
  end;
end;

function TJSONParser.GetChar: Char;
var
  Str: string;
begin
  Str := GetString;
  if Str = '' then
    Result := #0
  else
    Result := Str[1];
end;

procedure TJSONParser.GetStrings(List: TStrings);
var
  Count: Integer;
begin
  if OpenArray(count) then
  begin
    if Count > 0 then
      List.Capacity := Count;
    repeat
      if BeginObject then
      begin
        List.Add(GetKey);
        GetValue;
        Drop('}');
      end else begin
        List.Add(GetString);
      end;
    until CloseArray(Count);
  end;
end;

function TJSONParser.GetNumber: string;
// -0.5e+1.25
var
  Start: Integer;
  Loop : Integer;
begin
  Blanks;
  if NextChar = '"' then
  begin
    Result := GetString; // "0"
    if Result = '' then
      Result := '0';
  end else begin
    Result := '';
    Start := FIndex;
    Loop := 0;
    repeat
      Inc(Loop);
      if not Skip('-') then
        Skip('+');
      while (FEof = False) and (NextChar in ['0'..'9']) do
        ReadChar;
      if Skip('.') then
      begin
        while (FEof = False) and (NextChar in ['0'..'9']) do
          ReadChar;
      end;
    until (FEof) or (Loop = 2) or (Skip('e') = False);
    Result := Copy(FText, Start, FIndex - Start);
  end;
end;

function TJSONParser.GetInteger: Integer;
begin
  Result := StrToInt(GetNumber);
end;

function TJSONParser.GetInt64: Int64;
begin
  Result := StrToInt64(GetNumber);
end;

function TJSONParser.GetBoolean: Boolean;
begin
  Blanks;
  if SkipStr(JSON_TRUE) or SkipStr('"true"') then
    Exit(True);
  if SkipStr(JSON_FALSE) or SkipStr(JSON_NIL) or SkipStr('"false"') then
    Exit(False);
  Result := GetInteger <> 0;
end;

function TJSONParser.GetFloat: Double;
begin
  Result := StrToFloat(GetNumber, JSONFormat);
end;

function TJSONParser.GetCurrency: Currency;
begin
  Result := StrToCurr(GetNumber, JSONFormat);
end;

function TJSONParser.GetDateTime: TDateTime;
var
  Str: string;
  Int: Int64;
begin
  if SkipStr(JSON_NIL) or SkipStr('""') then
    Result := 0
  else begin
    if NextChar = '"' then
    begin
      Str := GetString;
      if TryStrToInt64(Str, Int) then  // "time"
        Result := UnixToDateTime(Int, False)
      else
        Result := JSONDecodeDateTime(Str) // "yyy-mm-ddThh:nn"
    end else begin
      Result := UnixToDateTime(GetInt64, False);
    end;
  end;
end;

function TJSONParser.BeginObject: Boolean;
begin
  Blanks;
  Result := Skip('{') and not Skip('}');
end;

procedure TJSONParser.Blanks;
begin
  while NextChar in [#9, #10, #13, ' '] do
    ReadChar;
end;

function TJSONParser.EndObject: Boolean;
begin
  Blanks;
  if Skip(',') then
    Exit(False);
  Drop('}');
  Result := True;
end;

function TJSONParser.BeginArray: Boolean;
begin
  Result := Skip('[') and not Skip(']');
end;

function TJSONParser.EndArray: Boolean;
begin
  Blanks;
  if Skip(',') then
    Exit(False);
  Drop(']');
  Result := True;
end;

function TJSONParser.OpenArray(var Count: Integer): Boolean;
begin
  Count := -1;
  Blanks;
  if Skip('[') then
  begin
    Result := not Skip(']')
  end else begin
    Result := SkipStr('{"count":');
    if Result then
    begin
      Count := GetInteger;
      Drop(',');
      DropKey('items');
      Drop('[');
      Result := not Skip(']');
      if Result = False then
        Drop('}');
    end;
  end;
end;

function TJSONParser.CloseArray(Count: Integer): Boolean;
begin
  Result := EndArray();
  if Result and (Count >= 0) then
    Drop('}');
end;

{ TTypeInfoHelper }

//function TTypeInfoHelper.GetOrd(Instance: Pointer): Integer;
//begin
//  case TypeData.OrdType of
//    otSByte: Result := PShortInt(Instance)^;
//    otUByte: Result := PByte(Instance)^;
//    otSWord: Result := PSmallInt(Instance)^;
//    otUWord: Result := PWord(Instance)^;
//    otSLong: Result := PInteger(Instance)^;
//    otULong: Result := PCardinal(Instance)^;
//  else
//    raise EJSONError.Create('Unknown ordinal type ' + string(Name));
//  end;
//end;
//
//function TTypeInfoHelper.GetRecordMethod(const Name: string): Pointer;
//var
//  Ptr   : PByte absolute Result;
//  Field : PRecordTypeField absolute Result;
//  Attr  : PAttrData absolute Result;
//  Word  : PWord absolute Result;
//  Method: PRecordTypeMethod absolute Result;
//  Count : Integer;
//begin
//  if Kind <> tkRecord then
//    Exit(nil);
//  Ptr := RecordFieldsPtr(Count);
//  while Count > 0 do
//  begin
//    Inc(Ptr, Field.InstanceSize);
//    Dec(Count);
//  end;
//  Inc(Ptr, Attr.Len);
//  Count := Word^;
//  Inc(Word);
//  while Count > 0 do
//  begin
//    if string(Method.Name) = Name then
//    begin
//      Exit(Method.Code);
//    end;
//    Inc(Ptr, Method.InstanceSize);
//    Dec(Count);
//  end;
//  Result := nil;
//end;
//
//procedure TTypeInfoHelper.SetOrd(Instance: Pointer; Value: Integer);
//begin
//  case TypeData.OrdType of
//    otSByte: PShortInt(Instance)^ := Value;
//    otUByte: PByte(Instance)^ := Value;
//    otSWord: PSmallInt(Instance)^ := Value;
//    otUWord: PWord(Instance)^ := Value;
//    otSLong: PInteger(Instance)^ := Value;
//    otULong: PCardinal(Instance)^ := Value;
//  end;
//end;
//
//function TTypeInfoHelper.RecordFieldCount: Integer;
//begin
//  RecordFieldsPtr(Result);
//end;
//
//function TTypeInfoHelper.RecordFieldsPtr(var Count: Integer): PByte;
//var
//  NumOps: Byte;
//begin
//  if Kind <> tkRecord then
//  begin
//    Count := 0;
//    Exit(nil);
//  end;
//  Result := PByte(TypeData);
//  Inc(Result, 2 * SizeOf(Integer) + TypeData.ManagedFldCount * SizeOf(TManagedField));
//  NumOps := Result^;
//  Inc(Result, 1 + NumOps * SizeOf(Pointer));
//  Count := PInteger(Result)^;
//  Inc(Result, SizeOf(Integer));
//end;
//
//function TTypeInfoHelper.RecordFieldType(Index: Integer): PRecordTypeField;
//var
//  Ptr  : PByte absolute Result;
//  Count: Integer;
//begin
//  if Index < 0 then
//    Exit(nil);
//  Ptr := RecordFieldsPtr(Count);
//  if Count <= Index then
//    Exit(nil);
//  while Index > 0 do
//  begin
//    Inc(Ptr, Result.InstanceSize);
//    Dec(Index);
//  end;
//end;
//
//function TTypeInfoHelper.RecordFieldByName(const Name: string): PRecordTypeField;
//var
//  Ptr   : PByte absolute Result;
//  Count : Integer;
//begin
//  Ptr := RecordFieldsPtr(Count);
//  while Count > 0 do
//  begin
//    if string(Result.Name) = Name then
//      Exit;
//    Inc(Ptr, Result.InstanceSize);
//    Dec(Count);
//  end;
//  Result := nil;
//end;
//
//function TTypeInfoHelper.DynArrayLength(Instance: Pointer): Integer;
//begin
//  Result := DynArraySize(PPointer(Instance)^);
//end;
//
//function TTypeInfoHelper.DynArrayElType: PTypeInfo;
//begin
//  Result := TypeData.elType2^;
//end;
//
//function TTypeInfoHelper.DynArrayElSize: Integer;
//begin
//  Result := TypeData.elSize;
//end;
//
//function TTypeInfoHelper.GetClassPropInfos: TClassPropInfos;
//begin
//  SetLength(Result, TypeData.PropCount);
//  GetPropInfos(@Self, PPropList(Result));
//end;
//
//function TTypeInfoHelper.GetClassFieldInfos: TClassFieldInfos;
//var
//  TypeInfo: PTypeInfo;
//begin
//  Result := nil;
//  MergeClassFieldInfos(Result);
//  TypeInfo := @Self;
//  while TypeInfo.TypeData.ParentInfo <> nil do
//  begin
//    TypeInfo := TypeInfo.TypeData.ParentInfo^;
//    TypeInfo.MergeClassFieldInfos(Result);
//  end;
//end;
//
//procedure TTypeInfoHelper.MergeClassFieldInfos(var Infos: TClassFieldInfos);
//var
//  Table: PVmtFieldTable;
//  Prev : Integer;
//  Count: Integer;
//  CTab : PVmtFieldClassTab;
//  Field: PVmtFieldEntry absolute Table;
//  Attr : PAttrData absolute Table;
//  Ptr  : PByte absolute Table;
//  ExFld: PFieldExEntry absolute Table;
//  ExCnt: Integer;
//  Index: Integer;
//  Dup  : Integer;
//begin
//  Table := PPointer(PByte(TypeData.ClassType) + vmtFieldTable)^;
//  if Table = nil then
//    Exit;
//
//  Count := Table.Count;
//  CTab   := Table.ClassTab;
//  Inc(Table);
//
//  // classic fields (?)
//  Prev := Length(Infos);
//  SetLength(Infos, Prev + Count);
//  for Index := 0 to Count - 1 do
//  begin
//    Infos[Prev + Index].Name := string(Field.Name);
//    Infos[Prev + Index].Offset := Field.FieldOffset;
//    Infos[Prev + Index].TypeInfo := CTab.ClassRef[Field.TypeIndex].ClassInfo;
//    Attr := PAttrData(Field.NameFld.Tail);
//    Inc(Ptr, Attr.Len);
//  end;
//
//  // extended fields (?!)
//  ExCnt := PWord(Ptr)^;
//  Inc(Ptr, 2);
//
//  for Index := 0 to ExCnt - 1 do
//  begin
//    Dup := Count - 1;
//    while Dup >= 0 do
//    begin
//      if Infos[Prev + Dup].Offset = ExFld.Offset then
//        Break;
//      Dec(Dup);
//    end;
//    if Dup < 0 then
//    begin
//      Dup := Length(Infos);
//      SetLength(Infos, dup + 1);
//      Infos[Dup].Name := string(ExFld.Name);
//      Infos[Dup].Offset := ExFld.Offset;
//      if ExFld.TypeRef = nil then
//      begin
//        infos[Dup].TypeInfo := nil
//      end else begin
//        Infos[Dup].TypeInfo := ExFld.TypeRef^;
//      end;
//    end;
//    Attr := ExFld.AttrData;
//    Inc(Ptr, Attr.Len);
//  end;
//end;
//
//function TTypeInfoHelper.GetMethodAddress(const Name: string): Pointer;
//var
//  TypeInfo: PTypeInfo;
//begin
//  Result := GetClassMethodAddress(Name);
//  TypeInfo := @Self;
//  while (Result = nil) and (TypeInfo.TypeData.ParentInfo <> nil) do
//  begin
//    TypeInfo := TypeInfo.TypeData.ParentInfo^;
//    Result := TypeInfo.GetClassMethodAddress(Name);
//  end;
//end;
//
//function TTypeInfoHelper.GetClassMethodAddress(const Name: string): Pointer;
//var
//  Table: PVmtMethodTable;
//  Entry: PVmtMethodEntry absolute Table;
//  ExCnt: PWord absolute Table;
//  ExEnt: PVmtMethodExEntry absolute Table;
//  Count: Integer;
//  Index: Integer;
//begin
//  Result := nil;
//
//  Table := PPointer(PByte(TypeData.ClassType) + vmtMethodTable)^;
//  if Table = nil then
//    Exit;
//
//  Count := Table.Count;
//  Inc(Table);
//
//  for Index := 1 to Count do
//  begin
//    if string(Entry.Name) = Name then
//    begin
//      Result := Entry.CodeAddress;
//      Exit;
//    end;
//    Inc(PByte(Entry), Entry.Len);
//  end;
//
//  Count := ExCnt^;
//  Inc(ExCnt);
//
//  for Index := 1 to Count do
//  begin
//    if string(ExEnt.Entry.Name) = Name then
//    begin
//      Result := ExEnt.Entry.CodeAddress;
//      Exit;
//    end;
//    Inc(ExEnt);
//  end;
//end;

procedure TJSONBuilder.BuildJSON(TypeInfo: PTypeInfo; Instance: Pointer);
begin
  case TypeInfo.Kind of
    tkRecord  : BuildJSONRecord(TypeInfo, Instance);
    tkDynArray: BuildJSONDynArray(TypeInfo, PPointer(Instance)^);
    tkArray   : BuildJSONArray(TypeInfo, Instance);
    tkSet     : BuildJSONSet(TypeInfo, Instance);
    tkInteger : Append(IntToStr(TypeInfo.GetOrd(Instance)));
    tkChar    : AppendString(string(AnsiChar(Instance^)));
    tkWChar   : AppendString(Char(Instance^));
    tkString  : AppendString(string(ShortString(Instance^)));
    tkClass   :
    begin
//      BuildJSONClass(PPointer(Instance)^, Builder);
      if PPointer(Instance)^= nil then
        Append(JSON_NIL)
      else
        BuildJSONClass(PTypeInfo(TObject(Instance^).ClassInfo), PPointer(Instance^));
    end;
    tkInt64   :
    begin
      if TypeInfo.TypeData.MinInt64Value >= 0 then
        Append(UIntToStr(UInt64(Instance^)))
      else
        Append(IntToStr(Int64(Instance^)));
    end;
    tkLString :
    begin
      if PPointer(Instance^) = nil then
        Append(JSON_NIL)
      else
        AppendString(string(PAnsiString(Instance)^));
    end;
    tkUString :
    begin
      if PPointer(Instance^) = nil then
        Append(JSON_NIL)
      else
        AppendString(PString(Instance)^);
    end;
    tkEnumeration:
    begin
      if TypeInfo = System.TypeInfo(Boolean) then
        Append(JSON_BOOL[TypeInfo.GetOrd(Instance) <> 0])
      else
        Append(JSONEscape(GetEnumName(TypeInfo, TypeInfo.GetOrd(Instance))));
    end;
    tkFloat:
    begin
      if TypeInfo = System.TypeInfo(TDate) then
        Append(JSONEncodeDate(TDate(Instance^)))
      else
      if TypeInfo = System.TypeInfo(TTime) then
        Append(JSONEncodeTime(TTime(Instance^)))
      else
      if TypeInfo = System.TypeInfo(TDateTime) then
        Append(JSONEncodeDateTime(TDateTime(Instance^)))
      else
        case TypeInfo.TypeData.FloatType of
          ftSingle   : Append(JSONEncodeFloat(Single(Instance^)));
          ftDouble   : Append(JSONEncodeFloat(Double(Instance^)));
          ftExtended : Append(JSONEncodeFloat(Extended(Instance^)));
          ftComp     : Append(JSONEncodeFloat(Comp(Instance^)));
          ftCurr     : Append(JSONEncodeFloat(Currency(Instance^)));
        end;
    end;
    tkPointer:
    begin
      if Instance = nil then
        Append(JSON_NIL)
      else begin
        case TypeInfo.TypeData.RefType^.Kind of
          tkChar: AppendString(string(PAnsiChar(Instance^)));
          tkWChar: AppendString(PChar(Instance^));
        else
          BuildJSON(TypeInfo.TypeData.RefType^, PPointer(Instance)^);
        end;
      end;
    end;
//    tkUnknown: ;
//    tkMethod: ;
//    tkWString: ;
//    tkVariant: ;
//    tkInterface: ;
//    tkClassRef: ;
//    tkPointer: ;
//    tkProcedure: ;
  else
    Append('"' + TypeInfo.Name + '"');
  end;
end;

procedure TJSONBuilder.BuildJSONClass(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Fields: TClassFieldInfos;
  Count : Integer;
  Index : Integer;
  Build : procedure(Sender: TObject; Builder: TJSONBuilder);
  Chain : TBuildChain;
begin
  if Instance = nil then
  begin
    Append(JSON_NIL);
    Exit;
  end;

  Build := TypeInfo.GetMethodAddress('JSONBuild');
  if @Build <> nil then
  begin
    Build(Instance, Self);
    Exit;
  end;

  if TypeInfo = TStringList.ClassInfo then
  begin
    AppendStrings(TStringList(Instance));
    Exit;
  end;

  if KnowInstance(Instance) then
    raise EJSONError.Create('Circular reference in ' + GetPath);

  Chain.Next := FBuild;
  Chain.Path := '';
  Chain.Build := TypeInfo.GetMethodAddress('JSONBuildField');
  Chain.Self := Instance;
  Chain.Sender := Instance;
  FBuild := @Chain;

  Append('{');

  Fields := TypeInfo.GetClassFieldInfos;
  Count := System.Length(Fields);

  for Index := 0 to Count - 1 do
  begin
    if Index > 0 then
      Append(',');
    Append('"');
    Append(Fields[Index].Name);
    Append('":');
    if BuildField(Fields[Index].Name) = False then
    begin
      if Fields[Index].TypeInfo = nil then
        raise EJSONError.Create('No RTTI informations for ' + GetPath);
      BuildJSON(Fields[index].TypeInfo, PByte(Instance) + Fields[Index].Offset);
    end;
  end;

  Append('}');

  FBuild := Chain.Next;
end;

procedure TJSONBuilder.BuildJSONRecord(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Count: Integer;
  Index: Integer;
  Field: PRecordTypeField;
  Chain: TBuildChain;
begin
  Chain.Build := TypeInfo.GetRecordMethod('JSONBuildField');
  Chain.Self := Instance;
  Chain.Sender := nil;
  Chain.Path := '';
  Chain.Next := FBuild;
  FBuild := @Chain;

  Append('{');
  Count := TypeInfo.RecordFieldCount;
  for Index := 0 to Count - 1 do
  begin
    if Index > 0 then
      Append(',');
    Field := TypeInfo.RecordFieldType(Index);
    Append('"');
    Append(Field.Name);
    Append('":');
    if BuildField(string(Field.Name)) = False then
    begin
      BuildJSON(Field.Field.TypeRef^, Field.GetInstance(Instance));
    end;
  end;
  Append('}');

  FBuild := Chain.Next;
end;

procedure TJSONBuilder.BuildJSONDynArray(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Len  : Integer;
  Index: Integer;
begin
  Len := DynArraySize(Instance);
  if Len = 0 then
  begin
    Append('[]');
    Exit;
  end;
  if JSON.CountList then
  begin
    Append('{"count":');
    Append(IntToStr(Len));
    Append(',"items":');
  end;
  Append('[');
  for Index := 0 to Len - 1 do
  begin
    if Index > 0 then
      Append(',');
    BuildJSON(TypeInfo.DynArrayElType, Instance);
    Inc(PByte(Instance), TypeInfo.DynArrayElSize);
  end;
  Append(']');
  if JSON.CountList then
    Append('}');
end;

procedure TJSONBuilder.BuildJSONArray(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Len  : Integer;
  Index: Integer;
  Step : Integer;
begin
  Append('[');
  Len := TypeInfo.TypeData.ArrayData.ElCount;
  Step := TypeInfo.TypeData.ArrayData.Size div Len;
  // DimCount can be > 1 with Dims[] all null (no RTTI info)
  for Index := 0 to Len - 1 do
  begin
    if Index > 0 then
      Append(',');
    BuildJSON(TypeInfo.TypeData.ArrayData.ElType^, Instance);
    Inc(PByte(Instance), Step);
  end;
  Append(']');
end;

procedure TJSONBuilder.BuildJSONSet(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Value: Integer;
  Index: Integer;
  Count: Integer;
begin
  Value := TypeInfo.GetOrd(Instance);
  Index := 0;
  Count := 0;
  Append('[');
  while Value > 0 do
  begin
    if Value and 1 > 0 then
    begin
      if Count > 0 then
        Append(',');
       AppendString(GetEnumName(TypeInfo.TypeData.CompType^, Index));
       Inc(Count);
    end;
    Value := Value shr 1;
    Inc(Index);
  end;
  Append(']');
end;

procedure TJSONParser.ParseJSON(TypeInfo: PTypeInfo; Instance: Pointer);
var
  E: Integer;
begin
  case TypeInfo.Kind of
    tkClass   : ParseJSONClass(TypeInfo, PPointer(Instance)^);
    tkRecord  : ParseJSONRecord(TypeInfo, Instance);
    tkDynArray: ParseJSONDynArray(TypeInfo, Instance);
    tkArray   : ParseJSONArray(TypeInfo, Instance);
    tkSet     : ParseJSONSet(TypeInfo, Instance);
    tkString  : TypeInfo.SetShortString(Instance, GetValue);// ParseJSONShortString(TypeInfo, Instance);
    tkInteger : TypeInfo.SetOrd(Instance, GetInteger);
    tkChar    : AnsiChar(Instance^) := AnsiChar(GetChar);
    tkWChar   : Char(Instance^) := GetChar;
    tkLString : RawByteString(Instance^) := RawByteString(GetValue);
    tkUString : string(Instance^)  := GetValue;
    tkInt64   :
    begin
      if TypeInfo.TypeData.MinInt64Value = 0 then
        Val(GetNumber, UInt64(Instance^), E)
      else
        Val(GetNumber, Int64(Instance^), E);
    end;
    tkEnumeration:
    begin
      if TypeInfo = System.TypeInfo(Boolean) then
      begin
        TypeInfo.SetOrd(Instance, Ord(GetBoolean));
      end else begin
        TypeInfo.SetOrd(Instance, GetEnumValue(TypeInfo, GetString));
      end;
    end;
    tkFloat:
    begin
      if TypeInfo = System.TypeInfo(TDate) then
        TDate(Instance^) := DateOf(GetDateTime)
      else
      if TypeInfo = System.TypeInfo(TTime) then
        TTime(Instance^) := TimeOf(GetDateTime)
      else
      if TypeInfo = System.TypeInfo(TDateTime) then
        TDateTime(Instance^) := GetDateTime
      else
        case TypeInfo.TypeData.FloatType of
          ftSingle   : Single(Instance^) := GetFloat;
          ftDouble   : Double(Instance^) := GetFloat;
          ftExtended : Extended(Instance^) := GetFloat;
          ftComp     : Comp(Instance^) := GetFloat;
          ftCurr     : Currency(Instance^) := GetCurrency;
        end;

    end;
//    tkUnknown: ;
//    tkMethod: ;
//    tkWString: ;
//    tkVariant: ;
//    tkInterface: ;
//    tkClassRef: ;
//    tkPointer: ;
//    tkProcedure: ;
  end;
end;

procedure TJSONParser.ParseJSONClass(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Fields: TClassFieldInfos;
  Key   : string;
  Index : Integer;
  Chain : TParseChain;
begin
  if Instance = nil then
    GetValue
  else begin

    if TypeInfo = TStringList.ClassInfo then
    begin
      GetStrings(TStringList(Instance));
      Exit;
    end;

    if BeginObject then
    begin
      Chain.Next := FParse;
      Chain.Path := '';
      Chain.Parse := TypeInfo.GetMethodAddress('JSONParseField');
      Chain.Self := Instance;
      FParse := @Chain;
      Fields := TypeInfo.GetClassFieldInfos;
      repeat
        Key := GetKey;
        if ParseField(Key) = False then
        begin
          Index := Fields.IndexOf(Key);
          if (Index < 0) or (Instance = nil) then
            GetValue
          else begin
            ParseJSON(Fields[Index].TypeInfo, PByte(instance) + Fields[Index].Offset);
          end;
        end;
      until EndObject;
      FParse := Chain.Next;
    end;
  end;
end;

procedure TJSONParser.ParseJSONRecord(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Key  : string;
  Field: PRecordTypeField;
  Chain: TParseChain;
begin
  if BeginObject then
  begin
    Chain.Parse := TypeInfo.GetRecordMethod('JSONParseField');
    Chain.Self := Instance;
    Chain.Path := '';
    Chain.Next := FParse;
    FParse := @Chain;
    repeat
      Key := GetKey;
      if ParseField(Key) = False then
      begin
        Field := TypeInfo.RecordFieldByName(Key);
        if Field = nil then
          GetValue
        else begin
          ParseJSON(Field.Field.TypeRef^, Field.GetInstance(Instance));
        end;
      end;
    until EndObject;
    FParse := Chain.Next;
  end;
end;

procedure TJSONParser.ParseJSONDynArray(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Item : PByte;
  Len  : Integer;
  Count: Integer;
begin
  if OpenArray(Count) then
  begin
    if Count > 0 then
      DynArraySetLength(PPointer(Instance)^, TypeInfo, 1, @Count);
    Len := 0;
    repeat
      Inc(Len);
      if Len > Count then
        DynArraySetLength(PPointer(Instance)^, TypeInfo, 1, @Len);
      Item := PPointer(Instance)^;
      Inc(Item, (Len - 1) * TypeInfo.DynArrayElSize);
      ParseJSON(TypeInfo.DynArrayElType, Item);
    until CloseArray(Count);
  end else begin
    DynArrayClear(PPointer(Instance)^, TypeInfo);
  end;
end;

procedure TJSONParser.ParseJSONArray(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Len  : Integer;
  Step : Integer;
begin
  Len := TypeInfo.TypeData.ArrayData.ElCount;
  Step := TypeInfo.TypeData.ArrayData.Size div Len;
  if BeginArray then
  begin
    repeat
      if Len <= 0 then
        GetValue
      else begin
        ParseJSON(TypeInfo.TypeData.ArrayData.ElType^, Instance);
        Inc(PByte(Instance), Step);
        Dec(Len);
      end;
    until EndArray;
  end;
end;

procedure TJSONParser.ParseJSONSet(TypeInfo: PTypeInfo; Instance: Pointer);
var
  Value: Integer;
  Index: Integer;
begin
  Value := 0;
  if BeginArray then
  begin
    repeat
      Index := GetEnumValue(TypeInfo.TypeData.CompType^, GetString);
      Value := Value or (1 shl Index);
    until EndArray;
  end;
  TypeInfo.SetOrd(Instance, Value);
end;

//procedure TJSONParser.ParseJSONShortString(TypeInfo: PTypeInfo; Instance: Pointer);
//var
//  Str: AnsiString;
//  Len: Integer;
//begin
//  Str := AnsiString(GetString);
//  Len := Length(Str);
//  if Len > TypeInfo.TypeData.MaxLength then
//    Len := TypeInfo.TypeData.MaxLength;
//  ShortString(Instance^)[0] := AnsiChar(Len);
//  if Len > 0 then
//    Move(Str[1], ShortString(Instance^)[1], Len);
//end;



{ TObjectHelper }

function TObjectHelper.toJSON: string;
{
var
  Builder: TJSONBuilder;
begin
  Builder := TJSONBuilder.Create;
  try
    Builder.BuildJSON(PTypeInfo(ClassType.ClassInfo), TypeInfo);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;
}
begin
  Result := JSON.toJSON(Self);
end;

procedure TObjectHelper.fromJSON(const Str: string);
var
  Parser: TJSONParser;
begin
//  JSON.fromJSON(Self, JSON);
  Parser := TJSONParser.Create(Str);
  try
    Parser.ParseJSON(PTypeInfo(ClassType.ClassInfo), @Self);
  finally
    Parser.Free;
  end;
end;

{ TJSONBuilder }

procedure TJSONBuilder.AppendString(const Str: string);
begin
  Append(JSONEscape(Str));
end;

procedure TJSONBuilder.AppendStrings(List: TStrings);
var
  Index: Integer;
begin
  if (List = nil) or (List.Count = 0) then
  begin
    Append('[]');
    Exit;
  end;
  if JSON.CountList then
  begin
    Append('{"count":');
    Append(IntToStr(List.Count));
    Append(',"items":');
  end;
  Append('[');
  for Index := 0 to List.Count - 1 do
  begin
    if Index > 0 then
      Append(',');
    if List.Objects[Index] = nil then
      AppendString(List[Index])
    else begin
      Append('{');
      AppendString(List[Index]);
      Append(':');
      AppendObject(List.Objects[Index]);
      Append('}');
    end;
  end;
  Append(']');
  if JSON.CountList then
    Append('}');
end;

procedure TJSONBuilder.AppendObject(Obj: TObject);
begin
  if Obj = nil then
    Append(JSON_NIL)
  else
    BuildJSONClass(PTypeInfo(Obj.ClassType.classInfo), Obj);
end;

function TJSONBuilder.BeginArray(Count: Integer): Boolean;
begin
  if Count <= 0 then
  begin
    Append('[]');
    Result := False;
  end else begin
    Append('{"count":');
    Append(Count);
    Append(',"items":[');
    Result := True;
  end;
end;

procedure TJSONBuilder.EndArray;
begin
  Append(']}');
end;

function TJSONBuilder.KnowInstance(Instance: Pointer): Boolean;
var
  Chain: PBuildChain;
begin
  Result := False;
  if FBuild = nil then
    Exit;
  Chain := FBuild;
  repeat
    if Chain.Sender = Instance then
      Result := True
    else begin
      Chain := Chain.Next;
    end;
  until Result or (Chain = nil);
end;

function TJSONBuilder.BuildField(Name: string): Boolean;
var
  Chain: PBuildChain;
begin
  if FBuild = nil then
    Exit(False);
  Chain := FBuild;
  repeat
    if Chain = FBuild then
      FBuild.Path := Name + '.'
    else
      Name := Chain.Path + Name;
    Result := (@Chain.Build <> nil) and Chain.Build(Chain.Self, Name, Self);
    Chain := Chain.Next;
  until Result or (Chain = nil);
end;

function TJSONBuilder.GetPath: string;
var
  Chain: PBuildChain;
begin
  Result := '';
  Chain := FBuild;
  while Chain <> nil do
  begin
    Result := Chain.Path + Result;
    if Chain.Sender <> nil then
      Result := Chain.Sender.ClassName + '.' + Result;
    Chain := Chain.Next;
  end;
end;

initialization
//  JSONFormat := TFormatSettings.Invariant;
  JSONFormat.CurrencyString := #$00A4;
  JSONFormat.CurrencyFormat := 0;
  JSONFormat.CurrencyDecimals := 2;
  JSONFormat.DateSeparator := '/';
  JSONFormat.TimeSeparator := ':';
  JSONFormat.ListSeparator := ',';
  JSONFormat.ShortDateFormat := 'MM/dd/yyyy';
  JSONFormat.LongDateFormat := 'dddd, dd MMMMM yyyy HH:mm:ss';
  JSONFormat.TimeAMString := 'AM';
  JSONFormat.TimePMString := 'PM';
  JSONFormat.ShortTimeFormat := 'HH:mm';
  JSONFormat.LongTimeFormat := 'HH:mm:ss';
  {
  for I := Low(DefShortMonthNames) to High(DefShortMonthNames) do
  begin
    JSONFormat.ShortMonthNames[I] := LoadResString(DefShortMonthNames[I]);
    JSONFormat.LongMonthNames[I] := LoadResString(DefLongMonthNames[I]);
  end;
  for I := Low(DefShortDayNames) to High(DefShortDayNames) do
  begin
    JSONFormat.ShortDayNames[I] := LoadResString(DefShortDayNames[I]);
    JSONFormat.LongDayNames[I] := LoadResString(DefLongDayNames[I]);
  end;
  }
  JSONFormat.ThousandSeparator := ',';
  JSONFormat.DecimalSeparator := '.';
  JSONFormat.TwoDigitYearCenturyWindow := 50;
  JSONFormat.NegCurrFormat := 0;
  JSON.TimeSeparator := 'T';
end.



