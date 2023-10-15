unit Execute.JSON.UTF8;

(*
  TypInfo based JSON parser/builder for Delphi (c)2015-2023 by Execute SARL

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
{$ZEROBASEDSTRINGS OFF}
{$LEGACYIFEND ON}
uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.TypInfo,
  Execute.RTTI,
  Execute.UTF8.Utils;

type
  TChar = AnsiChar;
  TString = UTF8String;
  
const
  JSON_TRUE  = 'true';
  JSON_FALSE = 'false';
  JSON_NULL  = 'null';
  JSON_NIL   = JSON_NULL;

  JSON_BOOL  : array[False..True] of TString = (JSON_FALSE, JSON_TRUE);

type
  EJSONError = class(Exception)
  end;

  TJSONRawValue = type TString;

  TNullBoolean = record
  private
    Value: Byte; // 0 = Null, 1 = False, 2 = True
    function GetBoolean: Boolean;
    procedure SetBoolean(Value: Boolean);
  public
    function IsSet: Boolean;
    procedure Clear;
    property AsBoolean: Boolean read GetBoolean write SetBoolean;
    class operator implicit(Value: Boolean): TNullBoolean;
    class operator implicit(const Value: TNullBoolean): Boolean;
  end;

  JSONDontStore = class(TCustomAttribute)
  end;

  // Build part let build a JSON string from a Delphi variable

  PBuildChain = ^TBuildChain;

  TJSONType = (
    jsUnknown,
    jsBoolean,
    jsEnum,
    jsChar,
    jsWChar,
    jsBlank,
    jsShortString,
    jsUTF8String,
    jsAnsiString,
    jsWideString,
    jsRawJSON,
    jsInteger,
    jsInt64,
    jsUInt64,
    jsSingle,
    jsDouble,
    jsExtended,
    jsComp,
    jsCurr,
    jsDate,
    jsTime,
    jsDateTime,
    jsNil,
    jsClass,
    jsRecord,
    jsNullBoolean,
    jsSet,
    jsArray,
    jsDynArray,
    jsBytes,
    jsPointer,
    jsPAnsiChar,
    jsPChar
  );

  // add some functions to TStringBuilder for JSON
  // keep the "Build Chain"
  TJSONBuilder = class
  private
    FBuild: PBuildChain;
    FBuffer: UTF8String;
    FLength: Integer;
    procedure Grow;
    function KnowInstance(Instance: Pointer): Boolean;
    function BuildField(Name: string): Boolean;
    function GetPath: string;

    function JSONStored(TypeInfo: PTypeInfo; Instance: Pointer): Boolean;

    procedure BuildJSON(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONClass(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONRecord(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONDynArray(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONBase64(Bytes: TBytes);
    procedure BuildJSONArray(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONSet(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure BuildJSONNullBoolean(const Value: TNullBoolean);

  public
    procedure Append(const Str: TString); overload;
    procedure Append(Value: Integer); overload;
    procedure AppendString(const Str: UTF8String; Nullable: Boolean = True); overload;
    procedure AppendString(const Str: string; Nullable: Boolean = True); overload;
    procedure AppendStrings(List: TStrings);
    procedure AppendObject(Obj: TObject); // to build JSON part of a subObject
    function BeginArray(Count: Integer): Boolean;
    procedure EndArray();
    function ToString: UTF8String; reintroduce;
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
    FText   : TString;
    FIndex  : Integer;
    FEof    : Boolean;
    FParse  : PParseChain;
    function ParseField(Name: string): Boolean;

    procedure ParseJSON(TypeInfo: PTypeInfo; Instance: Pointer);
    function ParseJSONField(TypeInfo: PTypeInfo; Instance: Pointer; const FieldName: string): Boolean;
    procedure ParseJSONClass(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONRecord(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONDynArray(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONBytes(var Bytes: TBytes);
    procedure ParseJSONArray(TypeInfo: PTypeInfo; Instance: Pointer);
    procedure ParseJSONSet(TypeInfo: PTypeInfo; Instance: Pointer);
//    procedure ParseJSONShortString(TypeInfo: PTypeInfo; Instance: Pointer);

  public
    constructor Create(const AText: TString);
    procedure ParseObject(Obj: TObject);  // to parse a subObject
    procedure ParseType(Instance, TypeInfo: Pointer); inline;
    procedure Parse<T>(var V: T); inline;
    function NextChar: TChar;
    function ReadChar: TChar;
    function Skip(Ch: TChar): Boolean;
    procedure Drop(Ch: TChar);
    procedure Blanks;
    procedure DropKey(const Str: TString);
    function SkipStr(const Str: TString): Boolean;
    function GetKey: TString;
    function ReadValue: TString;
    function GetValue: TString;
    function GetString: TString;
    function GetChar: Char;
    procedure GetStrings(List: TStrings);
    function GetNumber: TString;
    function GetInteger: Integer;
    function GetInt64: Int64;
    function GetBoolean: Boolean;
    function GetFloat: Double;
    function GetCurrency: Currency;
    procedure GetNullBoolean(var Value: TNullBoolean);
    function GetDateTime: TDateTime;
    function BeginObject: Boolean;
    function EndObject: Boolean;
    function BeginArray: Boolean;
    function EndArray: Boolean;
    function OpenArray(var Count: NativeInt): Boolean;
    function CloseArray(Count: NativeInt): Boolean;
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
    function toJSON: TString;
    procedure fromJSON(const Str: TString);
  end;

  // thank's to Stefan Glienke to point me out the type inference !
  JSON = class
    class var CountList: Boolean;
    class var TimeSeparator: Char;
    class var NullString: TString;
    class var NullArray: Boolean;
    class function buildJSON(TypeInfo: PTypeInfo; Instance: Pointer): TString;
    class function toJSON<T>(const instance :T): TString; inline;
    class function fromJSON<T>(var instance: T; const Str: TString): Boolean; inline;
    class function TryFromJSON<T>(var Instance: T; const Str: TString): Boolean;
    class function GetField<T>(var Instance: T; const Str: TString; const FieldName: string): Boolean;
    class procedure toFile<T>(const instance :T; const FileName: string); inline;
    class function fromFile<T>(var instance: T; const FileName: string): Boolean; inline;
    class function LoadFromFile(const AFileName: string): TString;
    class procedure SaveToFile(const AFileName: string; const Str: TString);
  end;

var
  JSONFormat: TFormatSettings;

function JSONEscape(const Str: TString): TString;
function JSONDecodeDateTime(const Str: TString): TDateTime;

implementation

function IntToStr(Value: Integer): UTF8String;
begin
  Str(Value, AnsiString(Result));
end;

function JSONType(TypeInfo: PTypeInfo): TJSONType;
begin
  Result := jsUnknown;
  case TypeInfo.ExtendedType of
    etRecord  :
      if TypeInfo = System.TypeInfo(TNullBoolean) then
        Result := jsNullBoolean
      else
        Result := jsRecord;
    etDynArray: Result := jsDynArray;
    etBytes   : Result := jsBytes;
    etArray   : Result := jsArray;
    etSet8,
    etSet16,
    etSet32   : Result := jsSet;
    etBoolean : Result := jsBoolean;
    etInteger,
    etShortInt,
    etByte,
    etSmallInt,
    etWord,
    etCardinal: Result := jsInteger;
    etAnsiChar: Result := jsChar;
    etWideChar: Result := jsWChar;
    etShortString,
    etSizedString : Result := jsShortString;
    etClass   : Result := jsClass;
    etInt64   : Result := jsInt64;
    etUInt64  : Result := jsUInt64;
    etUTF8String:
      if TypeInfo = System.TypeInfo(TJSONRawValue) then
        Result := jsRawJSON
      else
        Result := jsUTF8String;
    etAnsiString: Result := jsAnsiString;
    etString  : Result := jsWideString;
    etEnum8,
    etEnum16,
    etEnum32  : Result := jsEnum;
    etDateTime: Result := jsDateTime;
    etDate    : Result := jsDate;
    etTime    : Result := jsTime;
    etSingle  : Result := jsSingle;
    etDouble  : Result := jsDouble;
    etExtended: Result := jsExtended;
    etComp    : Result := jsComp;
    etCurrency: Result := jsCurr;
    etPointer  : Result := jsPointer;
    etPAnsiChar: Result := jsPAnsiChar;
    etPChar    : Result := jsPChar;
  end;
end;

function JSONInstanceType(TypeInfo: PTypeInfo; Instance: Pointer): TJSONType;
begin
  Result := JSONType(TypeInfo);
  case Result of
    jsShortString:
      if PByte(Instance)^ = 0 then
        Result := jsBlank;
    jsClass:
      if PPointer(Instance)^= nil then
        Result := jsNil;
    jsAnsiString,
    jsUTF8String,
    jsWideString,
    jsRawJSON:
      if PPointer(Instance^) = nil then
        Result := jsBlank;
  end;
end;

function GetNum(const Str: TString; Index, Count: Integer): Integer;
var
  Num: Integer;
begin
  Result := 0;
  while Count > 0 do
  begin
    Num := Ord(Str[Index]) - Ord('0');
    if (Num < 0) or (Num > 9) then
      raise EJSONError.Create('Invalid date ' + string(Str));
    Result := 10 * Result + Num;
    Inc(Index);
    Dec(Count);
  end;
end;

function JSONDecodeDateTime(const Str: TString): TDateTime;
// "2012-04-23T18:25:43"
var
  y, m, d, h, s: Integer;
begin
//  Result := 0;
  if (Str = '') or (Str = 'null') then
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
    raise EJSonError.Create('Invalid date ' + string(Str));

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
      raise EJSONError.Create('Invalid date/time ' + string(Str));
    h := GetNum(Str, 12, 2);
    m := GetNum(Str, 15, 2);
    s := GetNum(Str, 18, 2);
    Result := Result + EncodeTime(h, m, s, 0);
  end;
end;

class function JSON.LoadFromFile(const AFileName: string): TString;
var
  Stream: TFileStream;
begin
  Result := '';
  if FileExists(AFileName) then
  begin
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result, Stream.Size);
      Stream.Read(Result[1], Length(Result));
    finally
      Stream.Free;
    end;
  end;
end;

class procedure JSON.SaveToFile(const AFileName: string;
  const Str: TString);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Stream.Write(Str[1], Length(Str));
  finally
    Stream.Free;
  end;
end;

class function JSON.buildJSON(TypeInfo: PTypeInfo; Instance: Pointer): TString;
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

class function JSON.toJSON<T>(const instance :T): TString;
begin
  Result := buildJSON(TypeInfo(T), @Instance);
end;

class function JSON.TryFromJSON<T>(var Instance: T;
  const Str: TString): Boolean;
begin
  try
    Result := fromJSON<T>(Instance, STr);
  except
    Result := False;
  end;
end;

class function JSON.GetField<T>(var Instance: T; const Str: TString; const FieldName: string): Boolean;
begin
  Result := False;
  Finalize(Instance);
  FillChar(Instance, SizeOf(T), 0);
  if Str = '' then
    Exit;
  var Parser := TJSONParser.Create(Str);
  try
    Parser.ParseJSONField(TypeInfo(T), @Instance, FieldName);
  finally
    Parser.Free;
  end;
end;

class function JSON.fromJSON<T>(var instance: T; const Str: TString): Boolean;
var
  Parser: TJSONParser;
begin
  Finalize(instance);
  FillChar(instance, SizeOf(T), 0);
  if Str = '' then
    Exit(False);
  Parser := TJSONParser.Create(Str);
  try
    Parser.ParseJSON(TypeInfo(T), @Instance);
    Result := True;
  finally
    Parser.Free;
  end;
end;

class procedure JSON.toFile<T>(const instance: T; const FileName: string);
begin
  JSON.SaveToFile(FileName, JSON.toJSON(instance));
end;

class function JSON.fromFile<T>(var instance: T; const FileName: string): Boolean;
begin
  Result := JSON.fromJSON<T>(instance, JSON.LoadFromFile(FileName));
end;

function JSONEscape(const Str: TString): TString;
var
  Index : Integer;
  Escape: Integer;
  Len   : Integer;
  Ch    : TChar;
begin
  Escape := 0;
  Result := '"' + Str + '"';
  Len := Length(Result);
  for Index := 2 to Len - 1 do
    if (Result[Index] in ['\', '"', #13, #10, #9]) then
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
      if Ch = #9 then
        Ch := 't'
      else
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

function JSONEncodeDate(const Value: TDate): TString;
begin
  if Value < 1 then
    Result := JSON_NULL
  else
    Result := TString(FormatDateTime('''"''yyyy-mm-dd''"''', Value));
end;

function JSONEncodeTime(const Value: TTime): TString;
begin
  Result := TString(FormatDateTime('''"''hh:nn:ss''"''', Value));
end;

function JSONEncodeDateTime(const Value: TDate): TString;
begin
  if Value < 1 then
    Result := JSON_NIL
  else
    Result := TString(FormatDateTime('''"''yyyy-mm-dd"' + JSON.TimeSeparator + '"hh:nn:ss''"''', Value));
end;

function JSONEncodeFloat(const Value: Extended): TString;
begin
  Result := TString(FloatToStr(Value, JSONFormat));
end;

{ TJSONParser }

constructor TJSONParser.Create(const AText: TString);
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

procedure TJSONParser.Parse<T>(var V: T);
begin
  ParseJSON(TypeInfo(T), @V);
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

function TJSONParser.NextChar: TChar;
begin
  if FEof then
    raise EJSONError.Create('End of JSON stream');
  Result := FText[FIndex];
end;

function TJSONParser.ReadChar: TChar;
begin
  Result := NextChar;
  Inc(FIndex);
  FEof := FIndex > Length(FText);
end;

function TJSONParser.Skip(Ch: TChar): Boolean;
begin
  Result := (FEof = False) and (FText[FIndex] = Ch);
  if Result then
    ReadChar;
end;

procedure TJSONParser.Drop(Ch: TChar);
begin
  if not Skip(Ch) then
    raise EJSONError.Create('Expected char "' + ch +'" not found in "' + string(Copy(FText, 1, FIndex)) + '"');
end;

procedure TJSONParser.DropKey(const Str: TString);
begin
  if GetKey <> Str then
    raise EJSONError.Create('expected key "' + string(Str) + '" not found');
end;

function TJSONParser.SkipStr(const Str: TString): Boolean;
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

function TJSONParser.GetKey: TString;
begin
  Result := GetString;
  Drop(':');
end;

function TJSONParser.ReadValue: TString;
var
  start: Integer;
begin
  Start := FIndex;
  GetValue;
  Result := Copy(FText, Start, FIndex - Start);
end;

function TJSONParser.GetValue: TString;
var
  count: NativeInt;
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

function TJSONParser.GetString: TString;
var
  Start : Integer;
  Escape: Integer;
  Len   : Integer;
  Index : Integer;
  Ch    : Char;
  Str   : string;
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
  Str := string(Copy(FText, Start, Len));
  if Escape > 0 then
  begin
    Start := 0;
    Index := 1;
    Len := Length(Str); // ATTENTION conversion UTF8->String !!! la taille change
    while Index <= Len do
    begin
      Ch := Str[Index];
      if Ch = '\' then
      begin
        Inc(Index);
        Ch := Str[Index];
        case Ch of
          'n': Ch := #10;
          'r': Ch := #13;
          'u': // \uHHHH
          begin
            Ch := Char(StrToInt('$' + Copy(string(Str), Index + 1, 4)));
            Inc(Index, 4);
          end;
        end;
      end;
      Inc(Start);
      if Index > Start then
        Str[Start] := Ch;
      Inc(Index);
    end;
    SetLength(Str, Start);
  end;
  Result := UTF8String(Str);
end;

function TJSONParser.GetChar: Char;
var
  Str: string;
begin
  Str := string(GetString);
  if Str = '' then
    Result := #0
  else
    Result := Str[1];
end;

procedure TJSONParser.GetStrings(List: TStrings);
var
  Count: NativeInt;
begin
  if OpenArray(count) then
  begin
    if Count > 0 then
      List.Capacity := Count;
    repeat
      if BeginObject then
      begin
        List.Add(string(GetKey));
        GetValue;
        Drop('}');
      end else begin
        List.Add(string(GetString));
      end;
    until CloseArray(Count);
  end;
end;

procedure TJSONParser.GetNullBoolean(var Value: TNullBoolean);
begin
  if SkipStr(JSON_NULL) then
    Value.Clear
  else
    Value.AsBoolean := GetBoolean;
end;

function TJSONParser.GetNumber: TString;
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
  if SkipStr(JSON_NIL) then
    Result := 0
  else
    Result := StrToInt(string(GetNumber));
end;

function TJSONParser.GetInt64: Int64;
begin
  Result := StrToInt64(string(GetNumber));
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
  Result := StrToFloat(string(GetNumber), JSONFormat);
end;

function TJSONParser.GetCurrency: Currency;
begin
  Result := StrToCurr(string(GetNumber), JSONFormat);
end;

function TJSONParser.GetDateTime: TDateTime;
var
  Str: TString;
  Int: Int64;
begin
  Blanks;
  if SkipStr(JSON_NIL) or SkipStr('""') then
    Result := 0
  else begin
    if NextChar = '"' then
    begin
      Str := GetString;
      if TryStrToInt64(string(Str), Int) then  // "time"
      begin
        if Int <= 0 then
          Result := 0
        else
          Result := UnixToDateTime(Int, False)
      end else begin
        Result := JSONDecodeDateTime(Str) // "yyy-mm-ddThh:nn"
      end;
    end else begin
      Result := UnixToDateTime(GetInt64, False);
    end;
  end;
end;

function TJSONParser.BeginObject: Boolean;
begin
  Blanks;
  if SkipStr(JSON_NULL) then
    Exit(False);
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
  Blanks;
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

function TJSONParser.OpenArray(var Count: NativeInt): Boolean;
begin
  Count := -1;
  Blanks;
  if SkipStr(JSON_NULL) or SkipStr(JSON_FALSE) then
  begin
    Count := 0;
    Exit(False);
  end;
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

function TJSONParser.CloseArray(Count: NativeInt): Boolean;
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
  if Instance = nil then
  begin
    Append('null');
    Exit;
  end;
  case JSONInstanceType(TypeInfo, Instance) of
    jsRecord  : BuildJSONRecord(TypeInfo, Instance);
    jsNullBoolean: BuildJSONNullBoolean(TNullBoolean(Instance^));
    jsDynArray: BuildJSONDynArray(TypeInfo, PPointer(Instance)^);
    jsBytes   : BuildJSONBase64(TBytes(Instance^));
    jsArray   : BuildJSONArray(TypeInfo, Instance);
    jsSet     : BuildJSONSet(TypeInfo, Instance);
    jsInteger : Append(IntToStr(TypeInfo.GetOrd(Instance)));
    jsChar    : AppendString(string(AnsiChar(Instance^)));
    jsWChar   : if Char(Instance^) = #0 then Append('null') else  AppendString(Char(Instance^));
    jsBlank   : AppendString('');
    jsShortString  : AppendString(string(ShortString(Instance^)));
    jsRawJSON : Append(TString(TJSONRawValue(Instance^)));
    jsNil     : Append(JSON.NullString);
    jsClass   : BuildJSONClass(PTypeInfo(TObject(Instance^).ClassInfo), PPointer(Instance^));
    jsInt64   : Append(IntToStr(Int64(Instance^)));
    jsUInt64  : Append(TString(UIntToStr(UInt64(Instance^))));
    jsUTF8String : AppendString(TString(UTF8String(Instance^)));
    jsAnsiString : AppendString(TString(AnsiString(Instance^)));
    jsWideString : AppendString(string(Instance^));
    jsBoolean : Append(JSON_BOOL[TypeInfo.GetOrd(Instance) <> 0]);
    jsEnum    : Append(JSONEscape(TString(GetEnumName(TypeInfo, TypeInfo.GetOrd(Instance)))));
    jsDate    : Append(JSONEncodeDate(TDate(Instance^)));
    jsTime    : Append(JSONEncodeTime(TTime(Instance^)));
    jsDateTime: Append(JSONEncodeDateTime(TDateTime(Instance^)));
    jsSingle  : Append(JSONEncodeFloat(Single(Instance^)));
    jsDouble  : Append(JSONEncodeFloat(Double(Instance^)));
    jsExtended: Append(JSONEncodeFloat(Extended(Instance^)));
    jsComp    : Append(JSONEncodeFloat(Comp(Instance^)));
    jsCurr    : Append(JSONEncodeFloat(Currency(Instance^)));
    jsPointer  : BuildJSON(TypeInfo.TypeData.RefType^, Pointer(Instance^));
    jsPAnsiChar: AppendString(TString(PAnsiChar(Instance^)));
    jsPChar    : AppendString(PChar(Instance^));
  else
    Append('"' + TString(TypeInfo.Name) + '"');
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
    var Name := Fields[Index].Name;
    Append('"');
    Append(Name);
    Append('":');
    if BuildField(Name) = False then
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
  First: Boolean;
begin
  Chain.Build := TypeInfo.GetRecordMethodPointer('JSONBuildField');
  Chain.Self := Instance;
  Chain.Sender := nil;
  Chain.Path := '';
  Chain.Next := FBuild;
  FBuild := @Chain;

  Append('{');
  Count := TypeInfo.RecordFieldCount;
  First := True;
  for Index := 0 to Count - 1 do
  begin
    Field := TypeInfo.RecordFieldType(Index);
    if (Field.Field.TypeRef <> nil) and (JSONStored(Field.Field.TypeRef^, Field.GetInstance(Instance)) = False) then
      Continue;
    if First then
      First := False
    else
      Append(',');
    var Name := Field.GetName;
    Append('"');
    Append(Name);
    Append('":');
    if BuildField(Name) = False then
    begin
      if Field.Field.TypeRef = nil then
        raise EJSONError.Create('Unknow type for ' + GetPath);
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

procedure TJSONBuilder.BuildJSONBase64(Bytes: TBytes);
begin
  if Bytes = nil then
    Append('null')
  else begin
    Append('"');
    Append(Base64Encode(Bytes));
    Append('"');
  end;
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

procedure TJSONBuilder.BuildJSONNullBoolean(const Value: TNullBoolean);
begin
  if Value.IsSet then
    Append(JSON_BOOL[Value.AsBoolean])
  else
    Append(JSON_NULL);
end;

procedure TJSONParser.ParseJSON(TypeInfo: PTypeInfo; Instance: Pointer);
begin
  case JSONType(TypeInfo) of
    jsClass   : ParseJSONClass(TypeInfo, PPointer(Instance)^);
    jsRecord  : ParseJSONRecord(TypeInfo, Instance);
    jsDynArray: ParseJSONDynArray(TypeInfo, Instance);
    jsBytes   : ParseJSONBytes(TBytes(Instance^));
    jsArray   : ParseJSONArray(TypeInfo, Instance);
    jsSet     : ParseJSONSet(TypeInfo, Instance);
    jsShortString  : TypeInfo.SetShortString(Instance, string(GetValue));
    jsInteger : TypeInfo.SetOrd(Instance, GetInteger);
    jsChar    : AnsiChar(Instance^) := AnsiChar(GetChar);
    jsWChar   : Char(Instance^) := GetChar;
    jsAnsiString : RawByteString(Instance^) := RawByteString(GetValue);
    jsUTF8String : UTF8String(Instance^) := UTF8String(GetValue);
    jsWideString : string(Instance^)  := string(GetValue);
    jsRawJSON  : TJSONRawValue(Instance^) := TJSONRawValue(ReadValue);
    jsInt64   : Int64(Instance^) := StrToInt64(string(GetNumber));
    jsUInt64  : UInt64(Instance^) := StrToUInt64(string(GetNumber));
    jsEnum    : TypeInfo.SetOrd(Instance, GetEnumValue(TypeInfo, string(GetString)));
    jsBoolean : TypeInfo.SetOrd(Instance, Ord(GetBoolean));
    jsDate    : TDate(Instance^) := DateOf(GetDateTime);
    jsTime    : TTime(Instance^) := TimeOf(GetDateTime);
    jsDateTime: TDateTime(Instance^) := GetDateTime;
    jsSingle   : Single(Instance^) := GetFloat;
    jsDouble   : Double(Instance^) := GetFloat;
    jsExtended : Extended(Instance^) := GetFloat;
    jsComp     : Comp(Instance^) := GetFloat;
    jsCurr     : Currency(Instance^) := GetCurrency;
    jsNullBoolean: GetNullBoolean(TNullBoolean(Instance^));
  else
    raise Exception.Create('Unsupported type');
  end;
end;

function TJSONParser.ParseJSONField(TypeInfo: PTypeInfo; Instance: Pointer; const FieldName: string): Boolean;
begin
  Result := False;
  var Field := UTF8String(FieldName);
  if BeginObject then
  begin
    repeat
      if GetKey = Field then
      begin
        try
          ParseJSON(TypeInfo, Instance);
          Exit(True);
        except
          Exit(False);
        end;
      end;
      GetValue;
    until EndObject;
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
        Key := string(GetKey);
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
    Chain.Parse := TypeInfo.GetRecordMethodPointer('JSONParseField');
    Chain.Self := Instance;
    Chain.Path := '';
    Chain.Next := FParse;
    FParse := @Chain;
    repeat
      Key := string(GetKey);
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
  Len  : NativeInt;
  Count: NativeInt;
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

procedure TJSONParser.ParseJSONBytes(var Bytes: TBytes);
var
  Start: Integer;
  Len: Integer;
begin
  Blanks;
  if SkipStr(JSON_NIL) then
  begin
    Bytes := nil;
    Exit;
  end;
  Drop('"');
  Start := FIndex;
  while not Skip('"') do
  begin
    if ReadChar = '\' then
    begin
      if ReadChar = 'n' then
      begin
        FText[FIndex - 2] := #13;
        FText[FIndex - 1] := #10;
      end else begin
        raise Exception.Create('Unexpected \ in Base64 string');
      end;
    end;
  end;
  Len := Base64Len(PByte(@FText[Start]), FIndex - Start - 1);
  SetLength(Bytes, Len);
  Base64Decode(PByte(@FText[Start]), PByte(Bytes), Len);
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
      Index := GetEnumValue(TypeInfo.TypeData.CompType^, string(GetString));
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

function TObjectHelper.toJSON: TString;
begin
  Result := JSON.toJSON(Self);
end;

procedure TObjectHelper.fromJSON(const Str: TString);
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(Str);
  try
    Parser.ParseJSON(PTypeInfo(ClassType.ClassInfo), @Self);
  finally
    Parser.Free;
  end;
end;

{ TJSONBuilder }

procedure TJSONBuilder.Append(const Str: UTF8String);
var
  Ofs: Integer;
  Len: Integer;
begin
  Len := Length(Str);
  if Len > 0 then
  begin
    Ofs := FLength + 1;
    Inc(FLength, Len);
    if FLength > Length(FBuffer) then
      Grow;
    Move(Str[1], FBuffer[Ofs], Len);
  end;
end;

procedure TJSONBuilder.Append(Value: Integer);
var
  SValue: UTF8String;
begin
  Str(Value, AnsiString(SValue));
  Append(SValue);
end;

procedure TJSONBuilder.AppendString(const Str: UTF8String; Nullable: Boolean = True);
begin
  if Nullable and (Str = '') then
    Append(JSON.NullString)
  else
    Append(JSONEscape(Str));
end;

procedure TJSONBuilder.AppendString(const Str: string; Nullable: Boolean = True);
begin
  if Nullable and (Str = '') then
    Append(JSON.NullString)
  else
    AppendString(UTF8String(Str));
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

function TJSONBuilder.ToString: UTF8String;
begin
  Result := Copy(FBuffer, 1, FLength);
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

procedure TJSONBuilder.Grow;
var
  Len: Integer;
begin
  Len := (Length(FBuffer) * 3) div 2;
  if FLength > Len then
    Len := 2 * FLength;
  if Len < FLength then // in case of overflow
    Len := FLength;
  SetLength(FBuffer, Len);
end;

function TJSONBuilder.JSONStored(TypeInfo: PTYpeInfo;
  Instance: Pointer): Boolean;
var
  Store: function(Instance: Pointer): Boolean;
begin
  Result := True;
  if @Self = nil then // l'erreur de type indisponible est gérée plus loin dans le code
    Exit;
{$IFNDEF LINUX} // BUG !!
  if TypeInfo.GetAttribute(JSONDontStore) <> nil then
    Exit(False);
{$ENDIF}
  case JSONInstanceType(TypeInfo, Instance) of
    jsNil   : Result := False;
    jsBlank : Result := JSON.NullString <> '';
    jsClass :
    begin
       Store := TypeInfo.GetMethodAddress('JSONStored');
       Result := (@Store = nil) or Store(Pointer(Instance^));
    end;
    jsRecord:
    begin
      Store := TypeInfo.GetRecordMethodPointer('JSONStored');
      Result := (@Store = nil) or Store(Instance);
    end;
    jsDynArray:
    begin
      Result := JSON.NullArray or (DynArraySize(PPointer(Instance)^) > 0);
    end;
    jsDateTime:
    begin
      Result := TDateTime(Instance^) >= 1;
    end;
    jsEnum,
    jsSet :
    begin
      Result := TypeInfo.GetOrd(Instance) <> 0;
    end;
  end;
end;

{ TNullBoolean }

procedure TNullBoolean.Clear;
begin
  Value := 0;
end;

function TNullBoolean.GetBoolean: Boolean;
begin
  Result := Value = 2;
end;

class operator TNullBoolean.implicit(const Value: TNullBoolean): Boolean;
begin
  Result := Value.AsBoolean;
end;

class operator TNullBoolean.implicit(Value: Boolean): TNullBoolean;
begin
  Result.AsBoolean := Value;
end;

function TNullBoolean.IsSet: Boolean;
begin
  Result := Value <> 0;
end;

procedure TNullBoolean.SetBoolean(Value: Boolean);
begin
  Self.Value := Ord(Value) + 1;
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
  JSON.NullString := 'null';
  JSON.NullArray := True;
end.


