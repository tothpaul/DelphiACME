unit Execute.RTTI;
{
   Delphi RTTI unit

   (c)2015-2023 Execute SARL  <contact@execute.fr>

   http://www.execute.fr

}

{ WARNING:
  the Class serializer is not more under active maintenance, I always use Records or Dynamic Arrays
  it should work to serialize an object to an JSON string, but the deserialization requires class instanciation that could fails.
}

interface
{$LEGACYIFEND OFF}
uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.DateUtils;

type
  ETypeInfo = class(Exception);

  TClassFieldInfo = record
    Name    : string;
    Offset  : Cardinal;
    TypeInfo: PTypeInfo;
  end;

  TClassFieldInfos = array of TClassFieldInfo;

  TClassFieldInfosHelper = record helper for TClassFieldInfos
    function IndexOf(Name: string): Integer;
  end;

  TClassPropInfos = array of PPropInfo;

  Alias = class(TCustomAttribute)
    constructor Create(const AName: string);
    class function IsAlias(Field: PRecordTypeField; const AName: string): Boolean;
  end;

  TAttributeClass = class of TCustomAttribute;

  TVmtFieldEntryHelper = record helper for TVmtFieldEntry
    function GetName: string;
  end;

  TRecordTypeFieldHelper = record helper for TRecordTypeField
    function InstanceSize: Integer;
    function GetInstance(RecordInstance: Pointer): Pointer; inline;
    function Visibility: TMemberVisibility; inline;
    function GetAttribute(Attribute: TAttributeClass): PAttrEntry; inline;
    function HasAttribute(Attribute: TAttributeClass): Boolean; inline;
    function Next: PRecordTypeField; inline;
    function GetName: string;
  end;

  TRecordTypeMethodHelper = record helper for TRecordTypeMethod
    function InstanceSize: Integer;
    function Visibility: TMemberVisibility; inline;
  end;

  TProcedureParamHelper = record helper for TProcedureParam
    function InstanceSize: Integer; inline;
  end;

  TAttrDataHelper = record helper for TAttrData
    function GetAttribute(Attribute: TAttributeClass): PAttrEntry;
    function HasAttribute(Attribute: TAttributeClass): Boolean; inline;
  end;

  TAttrEntryHelper = record helper for TAttrEntry
    function IntegerAt(Offset: Integer): Integer;
    function UTF8StringAt(Offset: Integer): UTF8String;
    function StringAt(Offset: Integer): string;
  end;

  TExtendedType = (
    etUnknown,       // tkUnknown,
    etInteger,       // tkInteger
    etAnsiChar,      // tkChar
    etEnum8,         // tkEnumaration -> etEnum16, etEnum32, etBoolean
    etSingle,        // tkFloat -> etDateTime, etDate, etTime, etSingle, etDouble, etExtended, etCurrency
    etShortString,   // tkString -> etSizedString
    etSet8,          // tkSet -> etSet16, etSet32
    etClass,         // tkClasse
    etMethod,        // tkMethod
    etWideChar,      // tkWChar
    etAnsiString,    // tkLString  -> etAnsiString, etUTF8String
    etWideString,    // tkWString
    etVariant,       // tkVariant
    etArray,         // tkArray
    etRecord,        // tkRecord
    etInterface,     // tkInterface
    etInt64,         // tkInt64 -> etUInt64
    etDynArray,      // tkDynArray -> etBytes
    etString,        // tkUString
    etClassRef,      // tkClassRef
    etPointer,       // tkPointer -> etPAnsiChar, etPChar
    etProcedure,     // tkProcedure
    etManagedRecord, // tkMRecord
    etNone,          // ---> first Extended types
  // tkInteger
    etShortInt,
    etByte,
    etSmallInt,
    etWord,
    etCardinal,
  // tkEnumeration
    etEnum16,
    etEnum32,
  // tkFloat
    etDateTime,
    etDate,
    etTime,
    etDouble,
    etExtended,
    etComp,
    etCurrency,
  // tkString
    etSizedString,
  // tkSet
    etBoolean,
    etSet16,
    etSet32,
  // tkLString
    etUTF8String,
  // tkInt64
    etUInt64,
  // tkDynArray
    etBytes,
  // tkPointer
    etPAnsiChar,
    etPChar
  );

  TTypeInfoHelper = record helper for TTypeInfo

    function DataPtr(Offset: Integer): Pointer; inline;
    function ExtendedType: TExtendedType;

  // Ordinals
    function GetOrd(Instance: Pointer): Integer;
    procedure SetOrd(Instance: Pointer; Value: Integer);

  // records
    function RecordFieldCount: Integer; inline;
    function RecordFieldsPtr(var Count: Integer): PRecordTypeField;
    function RecordFieldType(Index: Integer): PRecordTypeField;
    function RecordFieldByName(const Name: string): PRecordTypeField;
    function GetRecordMethod(const Name: string): PRecordTypeMethod;
    function GetRecordMethodPointer(const Name: string): Pointer;
    function RecordAttrData: PAttrData;

  // dynarray
    function DynArrayLength(Instance: Pointer): Integer; inline;
    function DynArrayElType: PTypeInfo; inline;
    function DynArrayElSize: Integer; inline;
    function DynArrayItem(Instance: Pointer; Index: Integer): Pointer;
    function DynArrayAddItem(Instance: Pointer; Count: Integer = 1): Pointer;
    function GrowArray(Instance: Pointer; Count: Integer = 1): Pointer; inline; deprecated 'use DynArrayAddItem';

  // class
    procedure MergeClassFieldInfos(var Infos: TClassfieldInfos);
    function GetClassMethodAddress(const Name: string): Pointer;
    function GetClassPropInfos: TClassPropInfos;
    function GetClassFieldInfos: TClassFieldInfos;
    function GetMethodAddress(const Name: string): Pointer;

  // Attributes
    function EnumBaseType: PTypeInfo;
    function IsBoolean: Boolean;
    function GetAttrData: PAttrData;
    function GetAttribute(Attribute: TAttributeClass): PAttrEntry;
    function HasAttribute(Attribute: TAttributeClass): Boolean; inline;

  // Copy
    function InstanceSize: Integer;
    procedure Copy(Source, Target: Pointer);
    procedure CopyRecord(Source, Target: Pointer);
    procedure CopyArray(Source, Target: Pointer);
    procedure CopyDynArray(Source, Target: Pointer);

  // Compare
    function Equals(Source, Target: Pointer): Boolean;
    function EqualsRecord(Source, Target: Pointer): Boolean;
    function EqualsArray(Source, Target: Pointer): Boolean;
    function EqualsDynArray(Source, Target: Pointer): Boolean;

    procedure SetShortString(Instance: Pointer; const Value: string);

    procedure SetValue(Instance: Pointer; const Value: string);
    function GetValue(Instance: Pointer): string;
    procedure Clear(Instance: Pointer);
    function IsDate: Integer;
    function IsUTF8String: Boolean;
    procedure SetDateTime(Instance: Pointer; Value: TDateTime);
  end;

  TDumper = record
    Lines: TStrings;
    Start: Pointer;
    procedure Dump(TypeInfo: PTypeInfo; Instance: Pointer; const Iter, Name: string);
    procedure DumpRecord(TypeInfo: PTypeInfo; Instance: Pointer; const Iter: string);
    procedure DumpArray(TypeInfo: PTypeInfo; Instance: Pointer; const Iter, Name: string);
  end;

  RTTI = class
    class procedure CopyType(TypeInfo: PTypeInfo; V1, V2: Pointer);
    class procedure Copy<T>(const V1, V2: T);
    class function EqualsType(TypeInfo: PTypeInfo; V1, V2: Pointer): Boolean;
    class function Equals<T>(const V1, V2: T): Boolean; reintroduce;
    class procedure Dump<T>(const V: T; Lines: TStrings);
    class procedure DumpType(TypeInfo: PTypeInfo; V: Pointer; Lines: TStrings);
  end;

implementation

class function RTTI.EqualsType(TypeInfo: PTypeInfo; V1, V2: Pointer): Boolean;
begin
  Result := TypeInfo.Equals(V1, V2);
end;

class procedure RTTI.Copy<T>(const V1, V2: T);
begin
  CopyType(TypeInfo(T), @V1, @V2);
end;

class procedure RTTI.CopyType(TypeInfo: PTypeInfo; V1, V2: Pointer);
begin
  TypeInfo.Copy(V1, V2);
end;

class procedure RTTI.Dump<T>(const V: T; Lines: TStrings);
begin
  DumpType(TypeInfo(T), @V, Lines);
end;

class procedure RTTI.DumpType(TypeInfo: PTypeInfo; V: Pointer; Lines: TStrings);
var
  Dumper: TDumper;
begin
  Dumper.Lines := Lines;
  Dumper.Start := V;
  Lines.BeginUpdate;
  Dumper.Dump(TypeInfo, V, '', '');
  Lines.EndUpdate;
end;

class function RTTI.Equals<T>(const V1, V2: T): Boolean;
begin
  Result := EqualsType(TypeInfo(T), @V1, @V2);
end;

function EndOfString(P: Pointer): Pointer; inline;
begin
  Result := @PByte(P)[PByte(P)^ + 1];
end;

function StrToFloat2(const Str: string): Extended;
var
  i: Integer;
begin
  if FormatSettings.DecimalSeparator = '.' then
    i := Pos(',', Str)
  else
    i := Pos('.', Str);
  if i > 0 then
  begin
    var LStr := Str;
    LStr[i] := FormatSettings.DecimalSeparator;
    Result := StrToFloat(LStr);
  end else begin
    Result := StrToFloat(Str);
  end;

end;

{ TClassFieldInfos }

function TClassFieldInfosHelper.IndexOf(Name: string): Integer;
begin
  Result := Length(Self) - 1;
  while Result >= 0 do
  begin
    if AnsiCompareText(Self[Result].Name, Name) = 0 then
      Exit;
    Dec(Result);
  end;
end;

{ Alias }

constructor Alias.Create(const AName: string);
begin
// Empty
end;

class function Alias.IsAlias(Field: PRecordTypeField; const AName: string): Boolean;
var
  Attr: PAttrEntry;
begin
  Attr := Field.AttrData.GetAttribute(Alias);
  Result := (Attr <> nil) and (AnsiCompareText(AName, Attr.StringAt(0)) = 0);
end;

{ TVmtFieldEntryHelper }

function TVmtFieldEntryHelper.GetName: string;
begin
  var LAlias := AttrData.GetAttribute(Alias);
  if LAlias = nil then
    Result := Name
  else
    Result := LAlias.StringAt(0);
end;

{ TRecordTypeFieldHelper }

function TRecordTypeFieldHelper.GetInstance(RecordInstance: Pointer): Pointer;
begin
  Result := RecordInstance;
  Inc(PByte(Result), Field.FldOffset);
end;

function TRecordTypeFieldHelper.GetName: string;
begin
  var LAlias := GetAttribute(Alias);
  if LAlias = nil then
    Result := string(Name)
  else
    Result := LAlias.StringAt(0);
end;

function TRecordTypeFieldHelper.HasAttribute(
  Attribute: TAttributeClass): Boolean;
begin
  Result := GetAttribute(Attribute) <> nil;
end;

function TRecordTypeFieldHelper.InstanceSize: Integer;
begin
  Result := SizeOf(TRecordTypeField) - 255 + Length(Name) + AttrData.Len;
end;

function TRecordTypeFieldHelper.Next: PRecordTypeField;
begin
  Result := @PByte(@Self)[InstanceSize];
end;

function TRecordTypeFieldHelper.Visibility: TMemberVisibility;
begin
  Result := TMemberVisibility(Flags and 3);
end;

function TRecordTypeFieldHelper.GetAttribute(Attribute: TAttributeClass): PAttrEntry;
begin
  Result := AttrData.GetAttribute(Attribute);
end;

{ TRecordTypeMethodHelper }

function TRecordTypeMethodHelper.InstanceSize: Integer;
var
  Ptr : PByte;
  Sig : PProcedureSignature absolute Ptr;
  Prm : PProcedureParam absolute Ptr;
  Attr: PAttrData absolute Ptr;
  Cnt : Integer;
  Size: Integer;
begin
  Result := SizeOf(TRecordTypeMethod) - 255 + Length(Name);
  Ptr := NameFld.Tail;
  if Sig.Flags = 255 then
    Exit(Result + 1);
  Cnt := Sig.ParamCount;
  Inc(Result, SizeOf(TProcedureSignature));
  Inc(Sig);
  while Cnt > 0 do
  begin
    Size := Prm.InstanceSize;
    Inc(Result, Size);
    Inc(Ptr, Size);
    Dec(Cnt);
  end;
  Inc(Result, Attr.Len);
end;

function TRecordTypeMethodHelper.Visibility: TMemberVisibility;
begin
  Result := TMemberVisibility((Flags shr 2) and 3);
end;

{ TProcedureParamHelper }

function TProcedureParamHelper.InstanceSize: Integer;
begin
   Result := SizeOf(TProcedureParam) - 255 + Length(Name) + AttrData.Len;
end;

{ TTypeInfoHelper }

function TTypeInfoHelper.DataPtr(Offset: Integer): Pointer;
begin
  Result := @PByte(TypeData)[Offset];
end;

function TTypeInfoHelper.GetOrd(Instance: Pointer): Integer;
begin
  case TypeData.OrdType of
    otSByte: Result := PShortInt(Instance)^;
    otUByte: Result := PByte(Instance)^;
    otSWord: Result := PSmallInt(Instance)^;
    otUWord: Result := PWord(Instance)^;
    otSLong: Result := PInteger(Instance)^;
    otULong: Result := PCardinal(Instance)^;
  else
    raise ETypeInfo.Create('Unknown ordinal type ' + string(Name));
  end;
end;

function TTypeInfoHelper.GetRecordMethod(const Name: string): PRecordTypeMethod;
var
  Ptr   : PByte absolute Result;
  Field : PRecordTypeField absolute Result;
  Attr  : PAttrData absolute Result;
  Word  : PWord absolute Result;
  Method: PRecordTypeMethod absolute Result;
  Count : Integer;
begin
  if Kind <> tkRecord then
    Exit(nil);
  Field := RecordFieldsPtr(Count);
  while Count > 0 do
  begin
    Inc(Ptr, Field.InstanceSize);
    Dec(Count);
  end;
  Inc(Ptr, Attr.Len);
  Count := Word^;
  Inc(Word);
  while Count > 0 do
  begin
    if string(Method.Name) = Name then
      Exit;
    Inc(Ptr, Method.InstanceSize);
    Dec(Count);
  end;
  Result := nil;
end;

function TTypeInfoHelper.GetRecordMethodPointer(const Name: string): Pointer;
var
  Method: PRecordTypeMethod;
begin
  Method := GetRecordMethod(Name);
  if Method = nil then
    Result := nil
  else
    Result := Method.Code;
end;

function TTypeInfoHelper.RecordAttrData: PAttrData;
var
  Ptr  : PByte absolute Result;
  Field: PRecordTypeField absolute Result;
  Count: Integer;
begin
  Field := RecordFieldsPtr(Count);
  while Count > 0 do
  begin
    Inc(Ptr, Field.InstanceSize);
    Dec(Count);
  end;
end;

procedure TTypeInfoHelper.SetDateTime(Instance: Pointer; Value: TDateTime);
begin
  if @Self = TypeInfo(TDate) then
    TDate(Instance^) := DateOf(Value)
  else
  if @Self = TypeInfo(TTime) then
    TTime(Instance^) := TimeOf(Value)
  else
  if @Self = TypeInfo(TDateTime) then
    TDateTime(Instance^) := Value
  else
    raise Exception.Create('Not a date time type');
end;

procedure TTypeInfoHelper.SetOrd(Instance: Pointer; Value: Integer);
begin
  case TypeData.OrdType of
    otSByte: PShortInt(Instance)^ := Value;
    otUByte: PByte(Instance)^ := Value;
    otSWord: PSmallInt(Instance)^ := Value;
    otUWord: PWord(Instance)^ := Value;
    otSLong: PInteger(Instance)^ := Value;
    otULong: PCardinal(Instance)^ := Value;
  end;
end;

function TTypeInfoHelper.RecordFieldCount: Integer;
begin
  RecordFieldsPtr(Result);
end;

function TTypeInfoHelper.RecordFieldsPtr(var Count: Integer): PRecordTypeField;
var
  NumOps: Byte;
  Ptr   : PByte absolute Result;
begin
  if Kind <> tkRecord then
  begin
    Count := 0;
    Exit(nil);
  end;
  Ptr := DataPtr(2 * SizeOf(Integer) + TypeData.ManagedFldCount * SizeOf(TManagedField));
  NumOps := Ptr^;
  Inc(Ptr, 1 + NumOps * SizeOf(Pointer));
  Count := PInteger(Ptr)^;
  Inc(Ptr, SizeOf(Integer));
end;

function TTypeInfoHelper.RecordFieldType(Index: Integer): PRecordTypeField;
var
  Ptr  : PByte absolute Result;
  Count: Integer;
begin
  if Index < 0 then
    Exit(nil);
  Result := RecordFieldsPtr(Count);
  if Count <= Index then
    Exit(nil);
  while Index > 0 do
  begin
    Inc(Ptr, Result.InstanceSize);
    Dec(Index);
  end;
end;

function TTypeInfoHelper.RecordFieldByName(const Name: string): PRecordTypeField;
var
  Ptr   : PByte absolute Result;
  Count : Integer;
begin
  Result := RecordFieldsPtr(Count);
  while Count > 0 do
  begin
    if (AnsiCompareText(string(Result.Name), Name) = 0)
    or (Alias.IsAlias(Result, Name)) then
      Exit;
    Inc(Ptr, Result.InstanceSize);
    Dec(Count);
  end;
  Result := nil;
end;

function TTypeInfoHelper.DynArrayLength(Instance: Pointer): Integer;
begin
  Result := DynArraySize(PPointer(Instance)^);
end;

function TTypeInfoHelper.ExtendedType: TExtendedType;
begin
  Result := etNone;
  if @Self = nil then
    Exit;
  case Kind of
    tkUnknown : Result := etUnknown;
    tkInteger :
      case TypeData.OrdType of
        otSByte: Result := etShortInt;
        otUByte: Result := etByte;
        otSWord: Result := etSmallInt;
        otUWord: Result := etWord;
        otSLong: Result := etInteger;
        otULong: Result := etCardinal;
      end;
    tkChar: Result := etAnsiChar;
    tkEnumeration :
      begin
        if @Self = System.TypeInfo(Boolean) then
          Result := etBoolean
        else
          case TypeData.OrdType of
            otSByte,
            otUByte: Result := etEnum8;
            otSWord,
            otUWord: Result := etEnum16;
            otSLong,
            otULong: Result := etEnum32;
          end;
      end;
    tkFloat:
      begin
        if @Self = System.TypeInfo(TDateTime) then
          Result := etDateTime
        else
        if @Self = System.TypeInfo(TDate) then
          Result := etDate
        else
        if @Self = System.TypeInfo(TTime) then
          Result := etTime
        else
        case TypeData.FloatType of
          ftSingle   : Result := etSingle;
          ftDouble   : Result := etDouble;
          ftExtended : Result := etExtended;
          ftComp     : Result := etComp;
          ftCurr     : Result := etCurrency;
        end;
      end;
    tkString:
      if TypeData.MaxLength = 255 then
        Result := etShortString
      else
        Result := etSizedString;
    tkSet:
      case TypeData.OrdType of
        otSByte,
        otUByte: Result := etSet8;
        otSWord,
        otUWord: Result := etSet16;
        otSLong,
        otULong: Result := etSet32;
      end;
    tkClass: Result := etClass;
    tkMethod: Result := etMethod;
    tkWChar: Result := etWideChar;
    tkLString:
      if TypeData.CodePage = 65001 then
        Result := etUTF8String
      else
        Result := etAnsiString;
    tkWString: Result := etWideString;
    tkVariant: Result := etVariant;
    tkArray: Result := etArray;
    tkRecord: Result := etRecord;
    tkInterface: Result := etInterface;
    tkInt64:
      if TypeData.MinInt64Value = 0 then
        Result := etUInt64
      else
        Result := etInt64;
    tkDynArray:
      if @Self = System.TypeInfo(TBytes) then
        Result := etBytes
      else
        Result := etDynArray;
    tkUString: Result := etString;
    tkClassRef : Result := etClassRef;
    tkPointer  :
    begin
      if TypeData.RefType = nil then
        Result := etUnknown
      else begin
        case TypeData.RefType^.Kind of
          tkChar: Result := etPAnsiChar;
          tkWChar: Result := etPChar;
        else
          Result := etPointer;
        end;
      end;
    end;
    tkProcedure: Result := etProcedure;
  {$IF CompilerVersion >= 33.0} // Rio
    tkMRecord: Result := etManagedRecord;
  {$ENDIF}
  end;
end;

function TTypeInfoHelper.DynArrayElType: PTypeInfo;
begin
  Result := TypeData.elType2^;
end;

function TTypeInfoHelper.GrowArray(Instance: Pointer; Count: Integer = 1): Pointer;
begin
  Result := DynArrayAddItem(Instance, Count);
end;

function TTypeInfoHelper.DynArrayItem(Instance: Pointer; Index: Integer): Pointer;
begin
  Result := PPointer(Instance)^;
  Inc(PByte(Result), Index * DynArrayElSize);
end;

function TTypeInfoHelper.DynArrayAddItem(Instance: Pointer; Count: Integer = 1): Pointer;
var
  Len: Integer;
begin
  Len := DynArrayLength(Instance) + Count;
  DynArraySetLength(PPointer(Instance)^, @Self, 1, @Len);
  Result := PPointer(Instance)^;
  Inc(PByte(Result), (Len - Count) * DynArrayElSize);
end;

function TTypeInfoHelper.DynArrayElSize: Integer;
begin
  Result := TypeData.elSize;
end;

function TTypeInfoHelper.GetClassPropInfos: TClassPropInfos;
begin
  SetLength(Result, TypeData.PropCount);
  GetPropInfos(@Self, PPropList(Result));
end;

function TTypeInfoHelper.GetClassFieldInfos: TClassFieldInfos;
var
  TypeInfo: PTypeInfo;
begin
  Result := nil;
  MergeClassFieldInfos(Result);
  TypeInfo := @Self;
  while TypeInfo.TypeData.ParentInfo <> nil do
  begin
    TypeInfo := TypeInfo.TypeData.ParentInfo^;
    TypeInfo.MergeClassFieldInfos(Result);
  end;
end;

procedure TTypeInfoHelper.MergeClassFieldInfos(var Infos: TClassFieldInfos);
var
  Table: PVmtFieldTable;
  Prev : Integer;
  Count: Integer;
  CTab : PVmtFieldClassTab;
  Field: PVmtFieldEntry absolute Table;
  Attr : PAttrData absolute Table;
  Ptr  : PByte absolute Table;
  ExFld: PFieldExEntry absolute Table;
  ExCnt: Integer;
  Index: Integer;
  Dup  : Integer;
begin
  Table := PPointer(PByte(TypeData.ClassType) + vmtFieldTable)^;
  if Table = nil then
    Exit;

  Count := Table.Count;
  CTab   := Table.ClassTab;
  Inc(Table);

  // classic fields (?)
  Prev := Length(Infos);
  SetLength(Infos, Prev + Count);
  for Index := 0 to Count - 1 do
  begin
    Infos[Prev + Index].Name := Field.GetName;
    Infos[Prev + Index].Offset := Field.FieldOffset;
    Infos[Prev + Index].TypeInfo := CTab.ClassRef[Field.TypeIndex].ClassInfo;
    Attr := PAttrData(Field.NameFld.Tail);
    Inc(Ptr, Attr.Len);
  end;

  // extended fields (?!)
  ExCnt := PWord(Ptr)^;
  Inc(Ptr, 2);

  for Index := 0 to ExCnt - 1 do
  begin
    Dup := Count - 1;
    while Dup >= 0 do
    begin
      if Infos[Prev + Dup].Offset = ExFld.Offset then
        Break;
      Dec(Dup);
    end;
    if Dup < 0 then
    begin
      Dup := Length(Infos);
      SetLength(Infos, dup + 1);
      Infos[Dup].Name := string(ExFld.Name);
      Infos[Dup].Offset := ExFld.Offset;
      if ExFld.TypeRef = nil then
      begin
        infos[Dup].TypeInfo := nil
      end else begin
        Infos[Dup].TypeInfo := ExFld.TypeRef^;
      end;
    end;
    Attr := ExFld.AttrData;
    Inc(Ptr, Attr.Len);
  end;
end;

function TTypeInfoHelper.GetClassMethodAddress(const Name: string): Pointer;
var
  Table: PVmtMethodTable;
  Entry: PVmtMethodEntry absolute Table;
  ExCnt: PWord absolute Table;
  ExEnt: PVmtMethodExEntry absolute Table;
  Count: Integer;
  Index: Integer;
begin
  Result := nil;

  Table := PPointer(PByte(TypeData.ClassType) + vmtMethodTable)^;
  if Table = nil then
    Exit;

  Count := Table.Count;
  Inc(Table);

  for Index := 1 to Count do
  begin
    if string(Entry.Name) = Name then
    begin
      Result := Entry.CodeAddress;
      Exit;
    end;
    Inc(PByte(Entry), Entry.Len);
  end;

  Count := ExCnt^;
  Inc(ExCnt);

  for Index := 1 to Count do
  begin
    if string(ExEnt.Entry.Name) = Name then
    begin
      Result := ExEnt.Entry.CodeAddress;
      Exit;
    end;
    Inc(ExEnt);
  end;
end;

function TTypeInfoHelper.GetMethodAddress(const Name: string): Pointer;
var
  TypeInfo: PTypeInfo;
begin
  Result := GetClassMethodAddress(Name);
  TypeInfo := @Self;
  while (Result = nil) and (TypeInfo.TypeData.ParentInfo <> nil) do
  begin
    TypeInfo := TypeInfo.TypeData.ParentInfo^;
    Result := TypeInfo.GetClassMethodAddress(Name);
  end;
end;

function TTypeInfoHelper.EnumBaseType: PTypeInfo;
var
  pType: PPTypeInfo;
begin
  if Kind <> tkEnumeration then
    Exit(nil);
  Result := @Self;
  repeat
    pType := Result.TypeData.BaseType;
    if (pType = nil) or (pType^ = nil) or (pType^ = Result) then
      Exit;
    Result := pType^;
  until False;
end;

function TTypeInfoHelper.Equals(Source, Target: Pointer): Boolean;
begin
  case Kind of
    tkInteger,
    tkChar,
    tkWChar,
    tkEnumeration,
    tkSet,
    tkMethod,
    tkInt64,
    tkPointer,
    tkProcedure,
    tkFloat      : Result := CompareMem(Source, Target, InstanceSize);
    tkString     : Result := ShortString(Target^) = ShortString(Source^);
    tkLString    : Result := RawByteString(Target^) = RawByteString(Source^);
    tkUString    : Result := string(Target^) = string(Source^);
    tkWString    : Result := WideString(Target^) = WideString(Source^);
    tkArray      : Result := EqualsArray(Source, Target);
    tkRecord     : Result := EqualsRecord(Source, Target);
//    tkInterface  : IInterface(Target^) := IInterface(Source^);
    tkDynArray   : Result := EqualsDynArray(Source, Target);
//    tkVariant: ;
//    tkClass: ;
//    tkClassRef: ;
//    tkMRecord: ;
  else
    raise ETypeInfo.Create('Can''t compare type ' + string(Name));
  end;
end;

function TTypeInfoHelper.EqualsArray(Source, Target: Pointer): Boolean;
var
  Len  : Integer;
  Index: Integer;
  Step : Integer;
  Typ  : PTypeInfo;
begin
  Len := TypeData.ArrayData.ElCount;
  Step := TypeData.ArrayData.Size div Len;
  Typ := TypeData.ArrayData.ElType^;
  for Index := 0 to Len - 1 do
  begin
    if not Typ.Equals(Source, Target) then
      Exit(False);
    Inc(PByte(Source), Step);
    Inc(PByte(Target), Step);
  end;
  Result := True;
end;

function TTypeInfoHelper.EqualsDynArray(Source, Target: Pointer): Boolean;
var
  Len  : NativeInt;
  Index: Integer;
  ElType: PTypeInfo;
  ElSize: Integer;
begin
  Len := DynArrayLength(Source);
  if Len <> DynArrayLength(Target) then
    Exit(False);
  Source := PPointer(Source)^;
  Target := PPointer(Target)^;
  ElType := DynArrayElType;
  ElSize := DynArrayElSize;
  for Index := 0 to Len - 1 do
  begin
    if not ElType.Equals(Source, Target) then
      Exit(False);
    Inc(PByte(Source), ElSize);
    Inc(PByte(Target), ElSize);
  end;
  Result := True;
end;

function TTypeInfoHelper.EqualsRecord(Source, Target: Pointer): Boolean;
var
  Count: Integer;
  Field: PRecordTypeField;
begin
  Field := RecordFieldsPtr(Count);
  while Count > 0 do
  begin
    if not Field.Field.TypeRef^.Equals(Field.GetInstance(Source), Field.GetInstance(Target)) then
      Exit(False);
    Field := Field.Next;
    Dec(Count);
  end;
  Result := True;
end;

function TTypeInfoHelper.IsBoolean: Boolean;
var
  eType: PTypeInfo;
begin
  eType := EnumBaseType;
  Result := (eType = System.TypeInfo(Boolean)) or
            (eType = System.TypeInfo(ByteBool)) or
            (eType = System.TypeInfo(WordBool)) or
            (eType = System.TypeInfo(LongBool));
end;

function TTypeInfoHelper.GetAttrData: PAttrData;
var
  data : PTypeData;
  Index: Integer;
begin
  case Kind of
    tkInteger, tkChar, tkWChar:
      Result := DataPtr(SizeOf(TOrdType) + 2 * SizeOf(Integer));
    tkEnumeration:
    begin
      Result := DataPtr(SizeOf(TOrdType) + 2 * SizeOf(LongInt) + SizeOf(Pointer));
      data := TypeData;
      if (data.BaseType = nil) or (data.BaseType^ = @Self) or IsBoolean then
        for Index := data.MinValue to data.MaxValue do
          Result := EndOfString(Result);
      Result := EndOfString(Result); // UnitName
    end;
    tkFloat:
      Result := DataPtr(SizeOf(TFloatType));
    tkString:
      Result := DataPtr(1);
    tkSet:
      Result := DataPtr(SizeOf(UInt8) + SizeOf(PPTypeInfo));
//    tkClass:
//    tkMethod: ;
    tkLString:
      Result := DataPtr(SizeOf(Word));
    tkWString,
    tkUString,
    tkVariant:
      Result := @TypeData.AttrData;
    tkArray:
    begin
//      Result := DataPtr(TypeData.ArrayData.Size);
      Result := @TypeData.ArrayData.Dims[0];
      Inc(PByte(Result), TypeData.ArrayData.DimCount * SizeOf(Pointer));
    end;
    tkRecord:
      Result := RecordAttrData;
//    tkInterface: ;
    tkInt64:
      Result := @TypeData.Int64AttrData;
//    tkDynArray: ;
//    tkClassRef: ;
//    tkPointer: ;
//    tkProcedure: ;
//    tkMRecord: ;
  else
    Result := nil;
  end;
end;

function TTypeInfoHelper.GetAttribute(Attribute: TAttributeClass): PAttrEntry;
var
  T: PTypeInfo;
  D: PTypeData;
begin
  T := @Self;
  while T <> nil do
  begin
    Result := GetAttrData.GetAttribute(Attribute);
    if Result <> nil then
      Exit;
    if not (T.Kind = tkClass) then
      Break;
    D := TypeData;
    if (D.ParentInfo = nil) or (D.ParentInfo^ = T) then
      T := nil
    else
      T := D.ParentInfo^;
  end;
  Result := nil;
end;

function TTypeInfoHelper.HasAttribute(Attribute: TAttributeClass): Boolean;
begin
  Result := GetAttribute(Attribute) <> nil;
end;

function TTypeInfoHelper.InstanceSize: Integer;
begin
  Result := 0;
  case Kind of
    tkInteger,
    tkEnumeration,
    tkSet      :
      case TypeData.OrdType of
        otSByte, otUByte: Result := 1;
        otSWord, otUWord: Result := 2;
        otSLong, otULong: Result := 4;
      end;
    tkChar     : Result := 1;
    tkWChar    : Result := 2;
    tkFloat    :
      case TypeData.FloatType of
        ftSingle   : Result := SizeOf(Single);
        ftDouble   : Result := SizeOf(Double);
        ftExtended : Result := SizeOf(Extended);
        ftComp     : Result := SizeOf(Comp);
        ftCurr     : Result := SizeOf(Currency);
      end;
    tkPointer,
    tkProcedure,
    tkClass    : Result := SizeOf(Pointer);
    tkMethod   : Result := 2 * SizeOf(Pointer);
    tkInterface: Result := SizeOf(Pointer);
    tkInt64    : Result := SizeOf(Int64);
    tkString   : Result := TypeData.MaxLength + 1;
//    tkLString  : ;
//    tkUString  : ;
//    tkWString  : ;
//    tkVariant  : ;
//    tkArray    : ;
//    tkRecord   : ;
//    tkDynArray : ;
//    tkClassRef : ;
//    tkMRecord  : ;
  else
    raise ETypeInfo.Create('Unknow InstanceSize for ' + string(Name));
  end;
end;

procedure TTypeInfoHelper.Clear(Instance: Pointer);
begin
  case Kind of
    tkString  : ShortString(Instance^)[0] := #0;
    tkInteger : SetOrd(Instance, 0);
    tkChar    : AnsiChar(Instance^) := #0;
    tkWChar   : Char(Instance^) := #0;
    tkLString : RawByteString(Instance^) := '';
    tkUString : string(Instance^) := '';
    tkInt64   : UInt64(Instance^) := 0;
    tkEnumeration: SetOrd(Instance, 0);
    tkFloat:
        case TypeData.FloatType of
          ftSingle   : Single(Instance^) := 0;
          ftDouble   : Double(Instance^) := 0;
          ftExtended : Extended(Instance^) := 0;
          ftComp     : Comp(Instance^) := 0;
          ftCurr     : Currency(Instance^) := 0;
        end;
//    tkDynArray: ;
//    tkArray   : ;
//    tkSet     : ;
//    tkRecord: ;
//    tkClass: ;
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

procedure TTypeInfoHelper.Copy(Source: Pointer; Target: Pointer);
begin
  case Kind of
    tkInteger,
    tkChar,
    tkWChar,
    tkEnumeration,
    tkSet,
    tkMethod,
    tkInt64,
    tkPointer,
    tkProcedure,
    tkFloat,
    tkString     : Move(Source^, Target^, InstanceSize);
    tkLString    : RawByteString(Target^) := RawByteString(Source^);
    tkUString    : string(Target^) := string(Source^);
    tkWString    : WideString(Target^) := WideString(Source^);
    tkArray      : CopyArray(Source, Target);
    tkRecord     : CopyRecord(Source, Target);
    tkInterface  : IInterface(Target^) := IInterface(Source^);
    tkDynArray   : CopyDynArray(Source, Target);
//    tkVariant: ;
//    tkClass: ;
//    tkClassRef: ;
//    tkMRecord: ;
  else
    raise ETypeInfo.Create('Can''t copy type ' + string(Name));
  end;
end;

procedure TTypeInfoHelper.CopyRecord(Source: Pointer; Target: Pointer);
var
  Count: Integer;
  Field: PRecordTypeField;
begin
  Field := RecordFieldsPtr(Count);
  while Count > 0 do
  begin
    Field.Field.TypeRef^.Copy(Field.GetInstance(Source), Field.GetInstance(Target));
    Field := Field.Next;
    Dec(Count);
  end;
end;

procedure TTypeInfoHelper.CopyArray(Source: Pointer; Target: Pointer);
var
  Len  : Integer;
  Index: Integer;
  Step : Integer;
  Typ  : PTypeInfo;
begin
  Len := TypeData.ArrayData.ElCount;
  Step := TypeData.ArrayData.Size div Len;
  Typ := TypeData.ArrayData.ElType^;
  for Index := 0 to Len - 1 do
  begin
    Typ.Copy(Source, Target);
    Inc(PByte(Source), Step);
    Inc(PByte(Target), Step);
  end;
end;

procedure TTypeInfoHelper.CopyDynArray(Source: Pointer; Target: Pointer);
var
  Len  : NativeInt;
  Index: Integer;
  ElType: PTypeInfo;
  ElSize: Integer;
begin
  Len := DynArrayLength(Source);
  Source := PPointer(Source)^;
  DynArraySetLength(PPointer(Target)^, @Self, 1, @Len);
  Target := PPointer(Target)^;
  ElType := DynArrayElType;
  ElSize := DynArrayElSize;
  for Index := 0 to Len - 1 do
  begin
    ElType.Copy(Source, Target);
    Inc(PByte(Source), ElSize);
    Inc(PByte(Target), ElSize);
  end;
end;

procedure TTypeInfoHelper.SetShortString(Instance: Pointer;
  const Value: string);
var
  Str: AnsiString;
  Len: Integer;
begin
  Str := AnsiString(Value);
  Len := Length(Str);
  if Len > TypeData.MaxLength then
    Len := TypeData.MaxLength;
  ShortString(Instance^)[0] := AnsiChar(Len);
  if Len > 0 then
    Move(Str[1], ShortString(Instance^)[1], Len);
end;

procedure TTypeInfoHelper.SetValue(Instance: Pointer; const Value: string);
var
  E: Integer;
begin
  case Kind of
    tkString  : SetShortString(Instance, Value);
    tkInteger : SetOrd(Instance, StrToIntDef(Value, 0));
    tkChar    : if Length(Value) > 0 then AnsiChar(Instance^) := AnsiChar(Value[1]);
    tkWChar   : if Length(Value) > 0 then Char(Instance^) := Value[1];
    tkLString : RawByteString(Instance^) := RawByteString(Value);
    tkUString : string(Instance^)  := Value;
    tkInt64   :
    begin
      if TypeData.MinInt64Value = 0 then
        Val(Value, UInt64(Instance^), E)
      else
        Val(Value, Int64(Instance^), E);
    end;
    tkEnumeration:
    begin
      SetOrd(Instance, GetEnumValue(@Self, Value));
    end;
    tkFloat:
    begin
      if @Self = TypeInfo(TDate) then
        TDate(Instance^) := StrToDate(Value)
      else
      if @Self = TypeInfo(TTime) then
        TTime(Instance^) := StrToTime(Value)
      else
      if @Self = TypeInfo(TDateTime) then
      begin
        if Value = '' then
          TDateTime(Instance^) := 0
        else
          TDateTime(Instance^) := StrToDateTime(Value)
      end else
        case TypeData.FloatType of
          ftSingle   : Single(Instance^) := StrToFloat2(Value);
          ftDouble   : Double(Instance^) := StrToFloat2(Value);
          ftExtended : Extended(Instance^) := StrToFloat2(Value);
          ftComp     : Comp(Instance^) := StrToFloat2(Value);
          ftCurr     : Currency(Instance^) := StrToCurr(Value);
        end;
    end;
//    tkDynArray: ;
//    tkArray   : ;
//    tkSet     : ;
//    tkRecord: ;
//    tkClass: ;
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

function TTypeInfoHelper.GetValue(Instance: Pointer): string;
begin
  if Instance = nil then
    Exit('');
  case ExtendedType of
    etInteger        : Result := IntToStr(PInteger(Instance)^);
    etAnsiChar       : Result := string(AnsiChar(Instance^));
    etEnum8,
    etEnum16,
    etEnum32         : Result := GetEnumName(@Self, GetOrd(Instance));
    etSingle         : Result := FloatToStr(Single(Instance^));
    etSizedString,
    etShortString    : Result := string(ShortString(Instance^));
//    etSet8,          // tkSet -> etSet16, etSet32
//    etClass,         // tkClasse
//    etMethod,        // tkMethod
    etWideChar       : Result := Char(Instance^);
    etAnsiString     : if PPointer(Instance^) = nil then Result := '' else Result := string(AnsiString(Instance^));
    etWideString     : if PPointer(Instance^) = nil then Result := '' else Result := WideString(Instance^);
//    etVariant,       // tkVariant
//    etArray,         // tkArray
//    etRecord,        // tkRecord
//    etInterface,     // tkInterface
    etInt64          : Result := IntToStr(Int64(Instance^));
//    etDynArray,      // tkDynArray
    etString         : if PPointer(Instance^) = nil then Result := '' else Result := string(Instance^);
//    etClassRef,      // tkClassRef
//    etPointer,       // tkPointer
//    etProcedure,     // tkProcedure
//    etManagedRecord, // tkMRecord
//    etNone,          //
  // tkInteger
    etShortInt       : Result := IntToStr(PShortInt(Instance)^);
    etByte           : Result := IntToStr(PByte(Instance)^);
    etSmallInt       : Result := IntToStr(PSmallInt(Instance)^);
    etWord           : Result := IntToStr(PWord(Instance)^);
    etCardinal       : Result := UIntToStr(PCardinal(Instance)^);
  // tkEnumeration
  // tkFloat
    etDateTime       : Result := DateTimeToStr(TDateTime(Instance^));
    etDate           : Result := DateToStr(TDate(Instance^));
    etTime           : Result := TimeToStr(TTime(Instance^));
    etDouble         : Result := FloatToStr(Double(Instance^));
    etExtended       : Result := FloatToStr(Extended(Instance^));
    etComp           : Result := FloatToStr(Comp(Instance^));
    etCurrency       : Result := CurrToStr(Currency(Instance^));
    etBoolean        : Result := BooleanIdents[PByte(Instance)^ <> 0];
  // tkSet
//    etSet16,
//    etSet32,
  // tkInt64
    etUInt64         : Result := UIntToStr(PUInt64(Instance)^);
  end;
end;

function TTypeInfoHelper.IsDate: Integer;
begin
  Result := 0;
  if Kind = tkFloat then
  begin
    if @Self = TypeInfo(TDate) then
      Result := 1
    else
    if @Self = TypeInfo(TTime) then
      Result := 2
    else
    if @Self = TypeInfo(TDateTime) then
      Result := 3;
  end;
end;

function TTypeInfoHelper.IsUTF8STring: Boolean;
begin
  Result := (Kind = tkLString) and (TypeData.CodePage = 65001);
end;


{ TAttrEntryHelper }

function TAttrEntryHelper.IntegerAt(Offset: Integer): Integer;
begin
  if (Offset < 0) or (Offset + SizeOf(Integer) > ArgLen) then
    raise ETypeInfo.Create('Argument out of range');
  Result := PInteger(@PByte(@ArgData)[Offset])^;
end;

function TAttrEntryHelper.UTF8StringAt(Offset: Integer): UTF8String;
var
  Len: Word;
begin
  if (Offset < 0) or (Offset + SizeOf(Word) > ArgLen) then
    raise ETypeInfo.Create('Argument out of range');
  Len := PWord(@PByte(@ArgData)[Offset])^;
  SetLength(Result, Len);
  Move(PByte(@ArgData)[Offset + 2], Result[1], Len);
end;

function TAttrEntryHelper.StringAt(Offset: Integer): string;
begin
  Result := string(UTF8StringAt(Offset));
end;

{ TAttrDataHelper }

function TAttrDataHelper.GetAttribute(Attribute: TAttributeClass): PAttrEntry;
var
  I: PAttrEntry;
  E: PByte;
begin
  Result := nil;
  if @Self <> nil then
  begin
    I := @PByte(@Self)[2];
    E := @PByte(@Self)[Len];
    while PByte(I) < E do
    begin
      if I.AttrType^ = Attribute.ClassInfo then
      begin
        Exit(I);
      end;
      Inc(PByte(I), 2 * SizeOf(Pointer) + 2 + I.ArgLen);
    end;
  end;
end;

function TAttrDataHelper.HasAttribute(Attribute: TAttributeClass): Boolean;
begin
  Result := GetAttribute(Attribute) <> nil;
end;

procedure test();
type
  TTestType = record
    b: Byte;
    i: Integer;
    c: Char;
    a: array[0..1,0..1] of string;
    d: TArray<AnsiString>;
    p: PChar;
  end;
var
  T1: TTestType;
  T2: TTestType;
  T3: TTestType;
  T4: TTestType;
  T : PTypeInfo;
  i, j: Integer;
begin
  Assert(PTypeInfo(TypeInfo(Boolean)).ExtendedType = etBoolean);
  T1.b := 1;
  T1.i := 2;
  T1.c := '3';
  for i := 0 to 1 do
    for j := 0 to 1 do
      T1.a[i, j] := IntToStr(i) + ',' + IntToStr(j);
  SetLength(T1.d, 4);
  for i := 0 to 3 do
    T1.d[i] := AnsiString(IntToStr(i));
  T1.p := 'Hello World';

  FillChar(T2, SizeOf(T2), 0);
  FillChar(T3, SizeOf(T3), 0);

  T := TypeInfo(TTestType);
  RTTI.Copy(T1, T2); // T.Copy(@T1, @T2);


{$IF CompilerVersion >= 33.0}
  CopyRecord(@T3, @T1, T);
{$ELSE}
  T3 := T1;
{$ENDIF}

  T4 := T1;

  Assert(T2.b = 1);
  Assert(T2.i = 2);
  Assert(T2.c = '3');
  for i := 0 to 1 do
    for j := 0 to 1 do
    begin
      Assert(Pointer(T2.a[i, j]) = Pointer(T1.a[i, j]));
      var C := StringRefCount(T2.a[i, j]);
      Assert(C = 4);
    end;
  Assert(Length(T2.d) = 4);
  for i := 0 to 3 do
  begin
    Assert(Pointer(T2.d[i]) = Pointer(T1.d[i]));
    Assert(StringRefCount(T2.d[i]) = 2);
  end;
  Assert(T2.p = T1.p);

  Assert(T3.b = 1);
  Assert(T3.i = 2);
  Assert(T3.c = '3');
  for i := 0 to 1 do
    for j := 0 to 1 do
    begin
      Assert(Pointer(T3.a[i, j]) = Pointer(T1.a[i, j]));
      Assert(StringRefCount(T2.a[i, j]) = 4);
    end;
  Assert(Length(T3.d) = 4);
  for i := 0 to 3 do
  begin
    Assert(Pointer(T3.d[i]) = Pointer(T1.d[i]));
    Assert(StringRefCount(T3.d[i]) = 2);
  end;
  Assert(T3.p = T1.p);

  Assert(T4.b = 1);
  Assert(T4.i = 2);
  Assert(T4.c = '3');
  for i := 0 to 1 do
    for j := 0 to 1 do
    begin
      Assert(Pointer(T4.a[i, j]) = Pointer(T1.a[i, j]));
      Assert(StringRefCount(T4.a[i, j]) = 4);
    end;
  Assert(Length(T4.d) = 4);
  for i := 0 to 3 do
  begin
    Assert(Pointer(T4.d[i]) = Pointer(T1.d[i]));
    Assert(StringRefCount(T4.d[i]) = 2);
  end;
  Assert(T4.p = T1.p);


  Assert(RTTI.Equals(T1, T2));
  Assert(RTTI.Equals(T1, T3));
  Assert(RTTI.Equals(T1, T4));

  T1.d[1] := 'X';
  Assert(T2.d[1] <> T1.d[1]);
  Assert(T3.d[1] = T1.d[1]);
  Assert(T4.d[1] = T1.d[1]);

  Assert(RTTI.Equals(T1, T2) = False);
  Assert(RTTI.Equals(T1, T3));
  Assert(RTTI.Equals(T1, T4));
end;

{ TDumper }

procedure TDumper.Dump(TypeInfo: PTypeInfo; Instance: Pointer; const Iter, Name: string);
begin
  Lines.Add('// $' + IntToHex(Cardinal(Instance) - Cardinal(Start)) + ' : ' + IntToStr(Cardinal(Instance) - Cardinal(Start)));
  case TypeInfo.ExtendedType of
    etRecord:
    begin
      if Name = '' then
        Lines.Add(Iter + string(TypeInfo.Name) + ' {')
      else
        Lines.Add(Iter + Name + ' : ' + string(TypeInfo.Name) + ' {');
      DumpRecord(TypeInfo, Instance, Iter + ' ');
      Lines.Add(Iter + '}');
    end;
    etAnsiChar: Lines.Add(Iter + Name + ' : ' + string(TypeInfo.Name) + ' = ' + string(PAnsiChar(Instance)^));
    etWord    : Lines.Add(Iter + Name + ' : ' + string(TypeInfo.Name) + ' = ' + PWord(Instance)^.ToString);
    etCardinal: Lines.Add(Iter + Name + ' : ' + string(TypeInfo.Name) + ' = ' + PCardinal(Instance)^.ToString);
    etArray   : DumpArray(TypeInfo, Instance, Iter, Name);
  else
    Lines.Add(Iter + Name + ':' + string(TypeInfo.Name) + ' ??? ');
  end;
end;

procedure TDumper.DumpArray(TypeInfo: PTypeInfo; Instance: Pointer;
  const Iter, Name: string);
begin
  var Len := TypeInfo.TypeData.ArrayData.ElCount;
  var Step := TypeInfo.TypeData.ArrayData.Size div Len;
  case TypeInfo.TypeData.ArrayData.ElType^.ExtendedType of
    etAnsiChar:
    begin
      var S: AnsiString;
      SetString(S, PAnsiChar(Instance), Len);
      for var I := Len downto 1 do
      begin
        if S[I] < #32 then
        begin
          Insert('#0', S, I);
          S[I + 1] := AnsiChar(IntToHex(Ord(S[I + 2]) div 16)[1]);
          S[I + 2] := AnsiChar(IntToHex(Ord(S[I + 2]) and $F)[1]);
        end;
      end;
      Lines.Add(Iter +  Name + ' : ' + string(TypeInfo.Name) + ' [' + Len.ToString + '] = ' + string(S));
    end
  else
    Lines.Add(Iter +  Name + ' : ' + string(TypeInfo.Name) + ' [' + Len.ToString + '] = {');
    for var Index := 0 to Len - 1 do
    begin
      Inc(PByte(Instance), Step);
      Dump(TypeInfo.TypeData.ArrayData.ElType^, Instance, Iter + ' ', '');
    end;
    Lines.Add(Iter + '}');
  end;
end;

procedure TDumper.DumpRecord(TypeInfo: PTypeInfo; Instance: Pointer; const Iter: string);
begin
  var Count := TypeInfo.RecordFieldCount;
  for var Index := 0 to Count - 1 do
  begin
    var Field := TypeInfo.RecordFieldType(Index);
     if Field.Field.TypeRef = nil then
       Lines.Add(Iter + string(Field.Name) + ' no RTTI info')
     else
      Dump(Field.Field.TypeRef^, Field.GetInstance(Instance), Iter + ' ', string(Field.Name));
  end;
end;

initialization
{$IFDEF DEBUG}
  test();
{$ENDIF}
end.