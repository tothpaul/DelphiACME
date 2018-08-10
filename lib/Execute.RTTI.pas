unit Execute.RTTI;

interface

uses
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

  TRecordTypeFieldHelper = record helper for TRecordTypeField
    function InstanceSize: Integer;
    function GetInstance(RecordInstance: Pointer): Pointer; inline;
  end;

  TRecordTypeMethodHelper = record helper for TRecordTypeMethod
    function InstanceSize: Integer;
  end;

  TProcedureParamHelper = record helper for TProcedureParam
    function InstanceSize: Integer; inline;
  end;

  TTypeInfoHelper = record helper for TTypeInfo
  // Ordinals
    function GetOrd(Instance: Pointer): Integer;
    procedure SetOrd(Instance: Pointer; Value: Integer);

  // records
    function RecordFieldCount: Integer; inline;
    function RecordFieldsPtr(var Count: Integer): PByte;
    function RecordFieldType(Index: Integer): PRecordTypeField;
    function RecordFieldByName(const Name: string): PRecordTypeField;
    function GetRecordMethod(const Name: string): Pointer;

  // dynarray
    function DynArrayLength(Instance: Pointer): Integer; inline;
    function DynArrayElType: PTypeInfo; inline;
    function DynArrayElSize: Integer; inline;
    function GrowArray(Instance: Pointer): Pointer;

  // class
    procedure MergeClassFieldInfos(var Infos: TClassfieldInfos);
    function GetClassMethodAddress(const Name: string): Pointer;
    function GetClassPropInfos: TClassPropInfos;
    function GetClassFieldInfos: TClassFieldInfos;
    function GetMethodAddress(const Name: string): Pointer;

    procedure SetShortString(Instance: Pointer; const Value: string);

    procedure SetValue(Instance: Pointer; const Value: string);
    function IsDate: Integer;
    function IsUTF8STring: Boolean;
    procedure SetDateTime(Instance: Pointer; Value: TDateTime);
  end;

implementation

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

{ TRecordTypeFieldHelper }

function TRecordTypeFieldHelper.GetInstance(RecordInstance: Pointer): Pointer;
begin
  Result := RecordInstance;
  Inc(PByte(Result), Field.FldOffset);
end;

function TRecordTypeFieldHelper.InstanceSize: Integer;
begin
  Result := SizeOf(TRecordTypeField) - 255 + Length(Name) + AttrData.Len;
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

{ TProcedureParamHelper }

function TProcedureParamHelper.InstanceSize: Integer;
begin
   Result := SizeOf(TProcedureParam) - 255 + Length(Name) + AttrData.Len;
end;

{ TTypeInfoHelper }

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

function TTypeInfoHelper.GetRecordMethod(const Name: string): Pointer;
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
  Ptr := RecordFieldsPtr(Count);
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
    begin
      Exit(Method.Code);
    end;
    Inc(Ptr, Method.InstanceSize);
    Dec(Count);
  end;
  Result := nil;
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

function TTypeInfoHelper.RecordFieldsPtr(var Count: Integer): PByte;
var
  NumOps: Byte;
begin
  if Kind <> tkRecord then
  begin
    Count := 0;
    Exit(nil);
  end;
  Result := PByte(TypeData);
  Inc(Result, 2 * SizeOf(Integer) + TypeData.ManagedFldCount * SizeOf(TManagedField));
  NumOps := Result^;
  Inc(Result, 1 + NumOps * SizeOf(Pointer));
  Count := PInteger(Result)^;
  Inc(Result, SizeOf(Integer));
end;

function TTypeInfoHelper.RecordFieldType(Index: Integer): PRecordTypeField;
var
  Ptr  : PByte absolute Result;
  Count: Integer;
begin
  if Index < 0 then
    Exit(nil);
  Ptr := RecordFieldsPtr(Count);
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
  Ptr := RecordFieldsPtr(Count);
  while Count > 0 do
  begin
    if AnsiCompareText(string(Result.Name), Name) = 0 then
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

function TTypeInfoHelper.DynArrayElType: PTypeInfo;
begin
  Result := TypeData.elType2^;
end;

function TTypeInfoHelper.GrowArray(Instance: Pointer): Pointer;
var
  Len: Integer;
begin
  Len := DynArrayLength(Instance) + 1;
  DynArraySetLength(PPointer(Instance)^, @Self, 1, @Len);
  Result := PPointer(Instance)^;
  Inc(PByte(Result), (Len - 1) * DynArrayElSize);
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
    Infos[Prev + Index].Name := string(Field.Name);
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
          ftSingle   : Single(Instance^) := StrToFloat(Value);
          ftDouble   : Double(Instance^) := StrToFloat(Value);
          ftExtended : Extended(Instance^) := StrToFloat(Value);
          ftComp     : Comp(Instance^) := StrToFloat(Value);
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

end.

