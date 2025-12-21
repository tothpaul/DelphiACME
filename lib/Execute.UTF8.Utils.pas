unit Execute.UTF8.Utils;

interface
{
   Delphi UTF8 unit

   (c)2015-2023 Execute SARL  <contact@execute.fr>

   http://www.execute.fr

}
uses
  System.SysUtils,
  System.Classes,
  System.Hash,
  System.AnsiStrings,
  System.WideStrUtils;

const
  LHX: array[0..$F] of AnsiChar = '0123456789abcdef';
  HHX: array[0..$F] of AnsiChar = '0123456789ABCDEF';

function Base64Len(Src: PByte; Len: Integer): Integer;
function Base64LenW(Src: PChar; Len: Integer): Integer;
procedure Base64Decode(Src, Dst: PByte; Len: Integer); overload;
procedure Base64DecodeW(Src: PChar; Dst: PByte; Len: Integer);
function Base64Decode(const Str: string): TBytes; overload;
function Base64Decode(const Str: UTF8String): TBytes; overload;
function Base64DecodeUTF8(const Str: string): UTF8String; overload;
function Base64DecodeUTF8(const Str: RawByteString): UTF8String; overload;

function Base64Encode(const Str: UTF8String): UTF8String; overload;
function Base64Encode(Bytes: TBytes): UTF8String; overload; inline;
function Base64Encode(Data: PByte; Len: Integer): UTF8String; overload;
function Base64FromFile(const AFileName: string): UTF8String;
function Base64EncodeLen(Data: PByte; Len: Integer; LineLength: Integer = 76): UTF8String;
procedure CopyBase64Stream(ASource, ATarget: TStream);

function DetectUF8(Ptr: PByte; Size: Integer): Boolean;

function DecodeIfUTF8(const Str: string): string;
function EnsureUTF8(const Str: RawByteString): UTF8String;

function IntToUTF8(Int: Integer): UTF8String;
function Int64ToUTF8(Int: Int64): UTF8String;
function SizeToStr(Size: Integer): UTF8String;
function UTF8ToIntDef(const Str: UTF8String; Default: Integer): Integer;
function UTF8ToFloatDef(const Str: UTF8String; Default: Extended): Extended;

function UTF8ToBytes(const Str: UTF8String): TBytes;
function BytesToUTF8(const Bytes: TBytes): UTF8String;

//function Pos(const SubStr, Str: UTF8String; Start: Integer = 1): Integer; deprecated;
function UTF8Pos(const SubStr, Str: UTF8String; Start: Integer = 1): Integer;

function UTF8Pad(const Str: UTF8String; Len: Integer): UTF8String;
function UTF8Trunc(const Str: UTF8String; EndChar: AnsiChar): UTF8String; overload;
function UTF8Extract(const Str: UTF8String; FromChar, ToChar: AnsiChar): UTF8String;
function UTF8Trunc(const Str: UTF8String; Len: Integer): UTF8String; overload;
function UTF8StartsWith(const Str, SubStr: UTF8String): Boolean;
function UTF8EndsWith(const Str, SubStr: UTF8String): Boolean;
function UTF8Trim(const Str: UTF8String): UTF8String;
function UTF8LowerCase(const Str: UTF8String): UTF8String;

function UTF8Match(const Str, Sequence: UTF8String; Index :Integer): Boolean;

function UTF8Explode(const Sep, Str: UTF8String; Max: Integer = 0; DropBlanks: Boolean = False): TArray<UTF8String>;

function JSONDecodeDateTime(const Str: UTF8String): TDateTime;

function LowUID(const UID: UTF8String): UTF8String;

type
  TEndOfLine = (
    eolWindows, // \r\n  #13#10
    eolUnix,    // \n    #10
    eolOSX      // \r    #13
  );
function EndOfLine(const Str: UTF8STring; EOL: TEndOfLine = eolWindows): UTF8String;
function EscapeLineFeed(const Str: UTF8String): UTF8String;

function UTF8Replace(const Source, OldPattern, NewPattern: UTF8String): UTF8String; overload;
function UTF8Replace(const Source, OldPattern, NewPattern: UTF8String; Flags: TReplaceFlags): UTF8String; overload;

function MD5(const Str: UTF8String; Hexa: Boolean = True): UTF8String;
function SHA1(Data: Pointer; Len: Integer): UTF8String;
function SHA1Str(const Str: UTF8String): UTF8String; inline;
function SHA1Hex(const Str: UTF8String): UTF8String;
type
  UTF8StringHelper = record helper for UTF8String
    function IndexOf(const Str: UTF8String; Start: Integer = 1): Integer; inline;
    function StartsWith(const Str: UTF8String): Boolean; inline;
    function EndsWith(const Str: UTF8String): Boolean; inline;
    function Explode(const Sep: UTF8String; Max: Integer = 0; DropBlanks: Boolean = False): TArray<UTF8String>; inline;
  end;

implementation

function LowUID(const UID: UTF8String): UTF8String;
var
  Index: Integer;
begin
  // {A6972FB8-478B-4335-950A-9DD2927CBECE}
  if (Length(UID) = 38) and (UID[1] = '{') and (UID[38] = '}') then
    Result := Copy(UID, 2, 36)
  else
    Result := UID;
  for Index := 1 to Length(Result) do
    case Result[Index] of
      'A'..'F': Inc(Result[Index], Ord('a') - Ord('A'));
    end;
end;

const
  B64: array[0..63] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  V64: array of Byte;

function NextB64(var P: PByte): Byte;
begin
  while P^ in [9, 10, 13, 32] do
    Inc(P);
  Result := V64[P^];
  Inc(P);
end;

procedure Base64Decode(Src, Dst: PByte; Len: Integer);
var
  i, a, b: Integer;
begin
  if V64 = nil then
  begin
    SetLength(V64, 256);
    FillChar(V64[0], 256, 0);
    for i := 0 to 63 do
      V64[Ord(B64[i])] := i;
  end;

  while Len >= 3 do
  begin
    a := NextB64(Src);
    b := NextB64(Src);
    Dst^ := Byte((a shl 2) or (b shr 4)); // ..765431 | ..10xxxx
    Inc(Dst);
    a := NextB64(Src);
    Dst^ := Byte((b shl 4) or (a shr 2)); // ..xx7654 | ..3210xx
    Inc(Dst);
    b := NextB64(Src);
    Dst^ := Byte((a shl 6) or b);         // ..xxxx76 | ..543210
    Inc(Dst);
    Dec(Len, 3);
  end;

  if Len > 0 then
  begin
    a := NextB64(Src);
    b := NextB64(Src);
    Dst^ := Byte((a shl 2) or (b shr 4)); // ..765431 | ..10xxxx
    if Len > 1 then
    begin
      a := NextB64(Src);
      Inc(Dst);
      Dst^ := Byte((b shl 4) or (a shr 2));  // ..xx7654 | ..3210xx
    end;
  end;
end;

function NextB64W(var P: PChar): Byte;
begin
  while Ord(P^) in [9, 10, 13, 32] do
    Inc(P);
  Result := V64[Ord(P^)];
  Inc(P);
end;


procedure Base64DecodeW(Src: PChar; Dst: PByte; Len: Integer);
var
  i, a, b: Integer;
begin
  if V64 = nil then
  begin
    SetLength(V64, 256);
    FillChar(V64[0], 256, 0);
    for i := 0 to 63 do
      V64[Ord(B64[i])] := i;
  end;

  while Len >= 3 do
  begin
    a := NextB64W(Src);
    b := NextB64W(Src);
    Dst^ := (a shl 2) or (b shr 4); // ..765431 | ..10xxxx
    Inc(Dst);
    a := NextB64W(Src);
    Dst^ := (b shl 4) or (a shr 2); // ..xx7654 | ..3210xx
    Inc(Dst);
    b := NextB64W(Src);
    Dst^ := (a shl 6) or b;         // ..xxxx76 | ..543210
    Inc(Dst);
    Dec(Len, 3);
  end;

  if Len > 0 then
  begin
    a := NextB64W(Src);
    b := NextB64W(Src);
    Dst^ := (a shl 2) or (b shr 4); // ..765431 | ..10xxxx
    if Len > 1 then
    begin
      a := NextB64W(Src);
      Inc(Dst);
      Dst^ := (b shl 4) or (a shr 2);  // ..xx7654 | ..3210xx
    end;
  end;
end;

function Base64Decode(const Str: string): TBytes; overload;
var
  Src: TBytes;
  Len: Integer;
begin
  if Str = '' then
    Exit(nil);
  Src := TEncoding.ANSI.GetBytes(Str);
  Len := Base64Len(PByte(Src), Length(Src));
  SetLength(Result, Len);
  Base64Decode(PByte(Src), PByte(Result), Len);
end;

function Base64Decode(const Str: UTF8String): TBytes; overload;
var
  Len: Integer;
begin
  Len := Base64Len(PByte(Str), Length(Str));
  SetLength(Result, Len);
  Base64Decode(PByte(Str), PByte(Result), Len);
end;

function Base64DecodeUTF8(const Str: string): UTF8String; overload;
var
  Src: TBytes;
  Len: Integer;
begin
  Src := TEncoding.ANSI.GetBytes(Str);
  Len := Base64Len(PByte(Src), Length(Src));
  SetLength(Result, Len);
  Base64Decode(PByte(Src), PByte(Result), Len);
end;

function Base64DecodeUTF8(const Str: RawByteString): UTF8String; overload;
var
  Len: Integer;
begin
  Len := Base64Len(PByte(Str), Length(Str));
  SetLength(Result, Len);
  Base64Decode(PByte(Str), PByte(Result), Len);
end;

function Base64Encode(const Str: UTF8String): UTF8String;
begin
  Result := Base64Encode(Pointer(Str), Length(Str));
end;

function Base64Encode(Bytes: TBytes): UTF8String;
begin
  Result := Base64Encode(PByte(Bytes), Length(Bytes));
end;

function Base64Encode(Data: PByte; Len: Integer): UTF8String;
var
  Index : Integer;
  Value : PByte;
  Chars : PAnsiChar;
  C1, C2: Integer;
begin
  SetLength(Result, 4 * ((Len + 2) div 3));
  Index := 1;
  Value := Data;
  Chars := @Result[1];
  while Index < Len - 2 do
  begin
    C1 := Value^;
    Inc(Value);
    C2 := Value^;
    Inc(Value);
    Chars^ := B64[C1 shr 2];
    Inc(Chars);
    Chars^ := B64[((C1 and 3) shl 4) or (C2 shr 4)];
    Inc(Chars);
    C1 := Value^;
    Inc(Value);
    Chars^ := B64[((C2 and $0F) shl 2) or (C1 shr 6)];
    Inc(Chars);
    Chars^ := B64[C1 and $3F];
    Inc(Chars);
    Inc(Index, 3);
  end;
  if Index <= Len then
  begin
    C1 := Value^;
    Inc(Index);
    if Index > Len then
      C2 := 0
    else begin
      Inc(Value);
      C2 := Value^;
    end;
    Chars^ := B64[C1 shr 2];
    Inc(Chars);
    Chars^ := B64[((C1 and 3) shl 4) or (C2 shr 4)];
    Inc(Chars);
    if Index > Len then
      Chars^ := '='
    else begin
      Inc(Index);
      if Index > Len then
        C1 := 0
      else begin
        Inc(Value);
        C1 := Value^;
      end;
      Chars^ := B64[((C2 and $0F) shl 2) or (C1 shr 6)];
    end;
    Inc(Chars);
    if Index > Len then
      Chars^ := '='
    else begin
      Chars^ := B64[C1 and $3F];
    end;
  end;
end;

function Base64EncodeLen(Data: PByte; Len: Integer; LineLength: Integer = 76): UTF8String;
var
  LLen  : Integer;
  Index : Integer;
  Value : PByte;
  Chars : PAnsiChar;
  C1, C2: Integer;

  procedure NewLine;
  begin
//    Chars[LLen] := #13;
//    Inc(LLen);
    Chars[LLen] := #10;
    Inc(LLen);
    Inc(Chars, LLen);
    LLen := 0;
  end;

  procedure AddChar(Ch: AnsiChar);
  begin
    if LLen = LineLength then
      NewLine;
    Chars[LLen] := Ch;
    Inc(LLen);
  end;

begin
  Index := 4 * ((Len + 2) div 3);
  Inc(Index, ((Index + LineLength - 1) div LineLength));
  SetLength(Result, Index);
  Index := 1;
  Value := Data;
  Chars := @Result[1];
  LLen := 0;
  while Index < Len - 2 do
  begin
    C1 := Value^;
    Inc(Value);
    C2 := Value^;
    Inc(Value);
    AddChar(B64[C1 shr 2]);
    AddChar(B64[((C1 and 3) shl 4) or (C2 shr 4)]);
    C1 := Value^;
    Inc(Value);
    AddChar(B64[((C2 and $0F) shl 2) or (C1 shr 6)]);
    AddChar(B64[C1 and $3F]);
    Inc(Index, 3);
  end;
  if Index <= Len then
  begin
    C1 := Value^;
    Inc(Index);
    if Index > Len then
      C2 := 0
    else begin
      Inc(Value);
      C2 := Value^;
    end;
    AddChar(B64[C1 shr 2]);
    AddChar(B64[((C1 and 3) shl 4) or (C2 shr 4)]);
    if Index > Len then
      AddChar('=')
    else begin
      Inc(Index);
      if Index > Len then
        C1 := 0
      else begin
        Inc(Value);
        C1 := Value^;
      end;
      AddChar(B64[((C2 and $0F) shl 2) or (C1 shr 6)]);
    end;
    if Index > Len then
      AddChar('=')
    else begin
      AddChar(B64[C1 and $3F]);
    end;
  end;
  if LLen > 0 then
    NewLine;
  Assert(Chars = @Result[Length(Result) + 1]);
end;

procedure CopyBase64Stream(ASource, ATarget: TStream);
var
  Data  : array[0..17, 0..56] of Byte;
  Base  : array[0..17, 0..77] of AnsiChar;
  Size  : Integer;
  Count : Integer;
  Line  : Integer;
  Index : Integer;
  Offset: Integer;
  Total : Integer;
begin
  Size := ASource.Size;
  if Size = 0 then
    Exit;
  ASource.Position := 0;
  while Size > 0 do
  begin
    Count := ASource.Read(Data, SizeOf(Data));
    Dec(Size, Count);
    Total := 0;
    Line := 0;
    while Count > 0 do
    begin
      if Count < SizeOf(Data[Line]) then
        FillChar(Data[Line,Count], 0, SizeOf(Data[Line]) - Count);
      Index := 0;
      Offset := 0;
      while (Index < Count) and (Index < SizeOf(Data[Line])) do
      begin
        Base[Line, Offset] := B64[Data[Line, Index] shr 2];
        Inc(Offset);
        Base[Line, Offset] := B64[((Data[Line, Index] and 3) shl 4) or (Data[Line, Index + 1] shr 4)];
        Inc(Offset);
        Inc(Index);
        if Index >= Count then
          Base[Line, Offset] := '='
        else begin
          Base[Line, Offset] := B64[((Data[Line, Index] and $0F) shl 2) or (Data[Line, Index + 1] shr 6)];
          Inc(Index);
        end;
        Inc(Offset);
        if Index >= Count then
          Base[Line, Offset] := '='
        else begin
          Base[Line, Offset] := B64[Data[Line, Index] and $3F];
          Inc(Index);
        end;
        Inc(Offset);
      end;
      Dec(Count, Index);
      Base[Line, Offset] := #13;
      Inc(Offset);
      Base[Line, Offset] := #10;
      Inc(Offset);
      Inc(Total, Offset);
      Inc(Line);
    end;
    ATarget.WriteBuffer(Base, Total);
  end;
end;

function Base64FromFile(const AFileName: string): UTF8String;
var
  f     : file;
  Total : Integer;
  Len   : Integer;
  Index : Integer;
  Value : PByte;
  Chars : PAnsiChar;
  C1, C2: Integer;
  Buffer: array[0..767] of Byte; // (768 / 3) * 4 = 1024
begin
  Assignfile(f, AFileName);
  Reset(f, 1);
  try
    Total := FileSize(f);
    SetLength(Result, 4 * ((Total + 2) div 3));
    Chars := @Result[1];
    while Total > 0 do
    begin
      BlockRead(f, Buffer, SizeOf(Buffer), Len);
      Dec(Total, Len);
      Value := @Buffer;
      Index := 1;
      while Index < Len - 2 do
      begin
        C1 := Value^;
        Inc(Value);
        C2 := Value^;
        Inc(Value);
        Chars^ := B64[C1 shr 2];
        Inc(Chars);
        Chars^ := B64[((C1 and 3) shl 4) or (C2 shr 4)];
        Inc(Chars);
        C1 := Value^;
        Inc(Value);
        Chars^ := B64[((C2 and $0F) shl 2) or (C1 shr 6)];
        Inc(Chars);
        Chars^ := B64[C1 and $3F];
        Inc(Chars);
        Inc(Index, 3);
      end;
      if Index <= Len then
      begin
        C1 := Value^;
        Inc(Index);
        if Index > Len then
          C2 := 0
        else begin
          Inc(Value);
          C2 := Value^;
        end;
        Chars^ := B64[C1 shr 2];
        Inc(Chars);
        Chars^ := B64[((C1 and 3) shl 4) or (C2 shr 4)];
        Inc(Chars);
        if Index > Len then
          Chars^ := '='
        else begin
          Inc(Index);
          if Index > Len then
            C1 := 0
          else begin
            Inc(Value);
            C1 := Value^;
          end;
          Chars^ := B64[((C2 and $0F) shl 2) or (C1 shr 6)];
        end;
        Inc(Chars);
        if Index > Len then
          Chars^ := '='
        else begin
          Chars^ := B64[C1 and $3F];
        end;
      end;
    end;
  finally
    CloseFile(f);
  end;
end;


procedure TrimR(Src: PByte; var Len: Integer);
begin
  while Src[Len - 1] in [9, 10, 13, 32] do
    Dec(Len);
end;

function Base64Len(Src: PByte; Len: Integer): Integer;
var
  Index: Integer;
begin
// Trim right
  TrimR(Src, Len);
// ignore any spaces
  Result := Len;
  for Index := 0 to Len - 1 do
  begin
    if Src[Index] in [9, 10, 13, 32] then
      Dec(Result);
  end;
// compute decoded size
  Result := (3 * Result) div 4;
// check last two chars (of the right trimed text)
  if Src[Len - 1] = Ord('=') then
  begin
    Dec(Result);
    Dec(Len);
    TrimR(Src, Len);
    if Src[Len - 1] = Ord('=') then
      Dec(Result);
  end;
end;

procedure TrimRW(Src: PChar; var Len: Integer);
begin
  while Ord(Src[Len - 1]) in [9, 10, 13, 32] do
    Dec(Len);
end;

function Base64LenW(Src: PChar; Len: Integer): Integer;
var
  Index: Integer;
begin
// Trim right
  TrimRW(Src, Len);
// ignore any spaces
  Result := Len;
  for Index := 0 to Len - 1 do
  begin
    if Ord(Src[Index]) in [9, 10, 13, 32] then
      Dec(Result);
  end;
// compute decoded size
  Result := (3 * Result) div 4;
// check last two chars (of the right trimed text)
  if Src[Len - 1] = '=' then
  begin
    Dec(Result);
    Dec(Len);
    TrimRW(Src, Len);
    if Src[Len - 1] = '=' then
      Dec(Result);
  end;
end;

function DetectUF8(Ptr: PByte; Size: Integer): Boolean;
var
  Index: Integer;
  C1   : Byte;
  C2   : Byte;
  C3   : Byte;
begin
  Result := False;
  C2 := 0; // UTF8Sequence not found
  Index := 0;
  while Index < Size do
  begin
  {
      $00..$7F

      $C2..$DF + $80..$BF

      $E0      + $A0..$BF + $80..$BF
      $E1..$EC + $80..$BF + $80..$BF
      $ED      + $80..$9F + $80..$BF
      $EE..$EF + $80..$BF + $80..$BF

      $F0      + $90..$BF + $80..$BF + $80..$BF
      $F1..$F3 + $80..$BF + $80..$BF + $80..$BF
      $F4      + $80..$8F + $80..$BF + $80..$BF
  }
    C1 := Ptr[Index];
    Inc(Index);
    case C1 of
      $00..$7F: { ASCII } ;
      $C2..$F4: // UTF8 compatible
      begin
        if Index = Size then
          Exit; // need a second byte
        C2 := Ptr[Index];
        if (C2 < $80) or (C2 > $BF) then
          Exit; // invalid UTF8 Sequence
        Inc(Index);
        if C1 >= $E0 then // more then 2 chars
        begin
          if Index = Size then // 3 chars sequence
            Exit;
          if (C1 = $E0) and (C2 < $A0) then
            Exit;
          if (C1 = $ED) and (C2 > $9F) then
            Exit;
          if (C1 = $F0) and (C2 < $90) then
            Exit;
          C3 := Ptr[Index];
          if (C3 < $80) or (C3 > $BF) then
            Exit; // invalid UTF8 Sequence
          Inc(Index);
          if C2 >= $F0 then
          begin
            if Index = Size then
              Exit; // 4 chars sequence
            C3 := Ptr[Index];
            if (C3 < $80) or (C3 > $BF) then
              Exit; // invalid UTF8 Sequence
            Inc(Index);
          end;
        end;
      end;
    else
      Exit(False); // UTF8 incompatible
    end;
  end;
  Result := C2 <> 0; // UTF8 compatible
end;

function GetUTF8CharLen(const Str: string; Index: Integer; var Seq: Cardinal): Integer;
begin
  Result := 0;
  var L := Length(Str);
  if Index <= L then
  begin
    var C := Ord(Str[Index]);
    case C of
    %11000000..%11011111: Result := 1;
    %11100000..%11101111: Result := 2;
    %11110000..%11110111: Result := 3;
    else
      Exit(1);
    end;
    if Index + Result > L then
      Exit(1);
    var P: PAnsiChar := @Seq;
    P^ := AnsiChar(C);
    Inc(P);
    for var I := Index + 1 to Index + Result do
    begin
      C := Ord(Str[I]);
      if (C and %11000000) <> %10000000 then
        Exit(1);
      P^ := AnsiChar(C);
      Inc(P);
    end;
    Inc(Result);
  end;
end;

function DecodeIfUTF8(const Str: string): string;
const
  MB_ERR_INVALID_CHARS = 8;
var
  UTF8Seq: Cardinal;
begin
  Result := Str;
  var I := 1;
  var L := Length(Result);
  while I <= L do
  begin
    var UTF8Len := GetUTF8CharLen(Result, I, UTF8Seq);
    if UTF8Len > 1 then
    begin
      if UnicodeFromLocaleChars(CP_UTF8, MB_ERR_INVALID_CHARS, @UTF8Seq, UTF8Len, @Result[I], 1) = 1 then
      begin
        Move(Result[I + 2], Result[I + 1], (L - I - 1) * SizeOf(Char));
        Dec(L, UTF8Len - 1);
      end;
    end;
    Inc(I);
  end;
  SetLength(Result, L);
end;

function GetUTF8Chars(const Str: RawByteString; var Index: Integer; var Seq: Cardinal): Integer;
begin
  var L := Length(Str);
  if Index > L then
    Exit(0);
  Seq := Ord(Str[Index]);
  case Seq of
    %00000000..%01111111:
    begin
      Inc(Index);
      Exit(1);
    end;
    %11000000..%11011111: Result := 1;
    %11100000..%11101111: Result := 2;
    %11110000..%11110111: Result := 3;
  else
    Result := -1;
  end;
  if (Result > 0) and (Index + Result <= L) then
  begin
    for var I := Index + 1 to Index + Result do
    begin
      if (Ord(Str[I]) and %11000000) <> %10000000 then
      begin
        Result := -1;
        Break;
      end;
    end;
    if Result > 0 then
    begin
      Inc(Result);
      Move(Str[Index], Seq, Result);
      Inc(Index, Result);
      Exit;
    end;
  end;
  var Ch := Char(Seq);
  Result := LocaleCharsFromUnicode(CP_UTF8, 0, @Ch, 1, @Seq, 4, nil, nil);
  Inc(Index);
end;

function EnsureUTF8(const Str: RawByteString): UTF8String;
var
  UTF8Seq: Cardinal;
begin
  var I := 1;
  var L := 0;
  while I <= Length(Str) do
  begin
    Inc(L, GetUTF8Chars(Str, I, UTF8Seq));
  end;
  if L = Length(Str) then
  begin
    Result := Str;
    SetCodePage(RawByteString(Result), CP_UTF8, False);
  end else begin
    SetLength(Result, L);
    I := 1;
    var X := 1;
    while I <= Length(Str) do
    begin
      L := GetUTF8Chars(Str, I, UTF8Seq);
      Move(UTF8Seq, Result[X], L);
      Inc(X, L);
    end;
  end;
end;

function IntToUTF8(Int: Integer): UTF8String;
var
  S: AnsiString;
begin
  Str(Int, S);
  Result := UTF8String(S);
end;

function UTF8ToIntDef(const Str: UTF8String; Default: Integer): Integer;
var
  Index: Integer;
  Value: Integer;
  Neg  : Boolean;
begin
  if Str = '' then
    Exit(Default);
  Result := 0;
  Neg := False;
  for Index := 1 to Length(Str) do
  begin
    if (Index = 1) and (Str[1] = '-') then
      Neg := True
    else begin
      Value := Ord(Str[Index]) - Ord('0');
      if Value in [0..9] then
        Result := 10 * Result + Value
      else
        Exit(Default);
    end;
  end;
  if Neg then
    Result := -Result;
end;


function Int64ToUTF8(Int: Int64): UTF8String;
var
  S: AnsiString;
begin
  Str(Int, S);
  Result := UTF8String(S);
end;

function SizeToStr(Size: Integer): UTF8String;
var
  Kind: UTF8String;
begin
  if Size = 0 then
    Exit('vide');
  if Size < 1024 then
  begin
    Kind := ' octets';
  end else begin
    Size := (Size + 512) div 1024;
    if Size < 1024 then
      Kind := ' Ko'
    else begin
      Size := (Size + 512) div 1024;
      if Size < 1024 then
        Kind := ' Mo'
      else begin
        Size := (Size + 512) div 1024;
        Kind := ' Go';
      end;
    end;
  end;
  Str(Size, AnsiString(Result));
  Result := Result + Kind;
end;

function UTF8ToFloatDef(const Str: UTF8String; Default: Extended): Extended;
var
  LStr: string;
begin
  LStr := string(Str).Replace('.', FormatSettings.DecimalSeparator);
  Result := System.SysUtils.StrToFloatDef(LStr, Default);
end;

function UTF8ToBytes(const Str: UTF8String): TBytes; inline;
begin
  Result := BytesOf(Str);
//  var Len := Length(Str);
//  SetLength(Result, Len);
//  Move(Pointer(Str)^, Pointer(Result)^, Len);
end;

function BytesToUTF8(const Bytes: TBytes): UTF8String; inline;
begin
  SetString(Result, PAnsiChar(Bytes), Length(Bytes));
//  var Len := Length(Bytes);
//  SetLength(Result, Len);
//  Move(Pointer(Bytes)^, Pointer(Result)^, Len);
end;


function UTF8Pos(const SubStr, Str: UTF8String; Start: Integer = 1): Integer;
var
  L1: Integer;
  L2: Integer;
  B1: PAnsiChar;
  B2: PAnsiChar;
  I: Integer;
  J: Integer;
begin
  L2 := Length(SubStr);
  if L2 = 0 then
    Exit(0);
  L1 := Length(Str) - L2 + 1;
  if Start > 1 then
    Dec(L1, Start - 1)
  else
    Start := 1;
  if L1 <= 0 then
    Exit(0);
  B1 := @Str[Start];
  B2 := Pointer(SubStr);
  for I := 1 to L1 do
  begin
    J := 0;
    while B1[J] = B2[J] do
    begin
      Inc(J);
      if J = L2 then
        Exit(I + Start - 1);
    end;
    Inc(B1);
  end;
  Result := 0;
end;

function UTF8Pad(const Str: UTF8String; Len: Integer): UTF8String;
begin
  var L := Length(Str);
  Result := Str;
  SetLength(Result, Len);
  if L < Len then
  begin
    FillChar(Result[L + 1], Len - L, ' ');
  end;
end;

function UTF8Trunc(const Str: UTF8String; EndChar: AnsiChar): UTF8String;
var
  Index: Integer;
begin
  for Index := 1 to Length(Str) do
  begin
    if Str[Index] = EndChar then
    begin
      Result := Copy(Str, 1, Index - 1);
      Exit;
    end;
  end;
  Result := '';
end;

function UTF8Extract(const Str: UTF8String; FromChar, ToChar: AnsiChar): UTF8String;
var
  Start, Stop: Integer;
begin
  for Start := 1 to Length(Str) do
  begin
    if Str[Start] = FromChar then
    begin
      for Stop := Start + 1 to Length(Str) do
      begin
        if Str[Stop] = ToChar then
          Exit(Copy(Str, Start + 1, Stop - Start - 1));
      end;
    end;
  end;
  Result := '';
end;

function UTF8Trunc(const Str: UTF8String; Len: Integer): UTF8String;
begin
  Result := Str;
  if Length(Result) > Len then
  begin
    var L := 0;
    var I := 1;
    var N: Integer;
    while I <= Len do
    begin
      case Result[I] of
        #$C2..#$DF: N := 2; // 110x xxxx C0 - DF
        #$E0..#$EF: N := 3; // 1110 xxxx E0 - EF
        #$F0..#$F7: N := 4; // 1111 0xxx F0 - F7 // outside traditional UNICODE
        #$F8..#$FB: N := 5; // 1111 10xx F8 - FB // outside UTF-16
        #$FC..#$FD: N := 6; // 1111 110x FC - FD // outside UTF-16
      else
        N := 1;
      end;
      if I + N - 1 > Len then
        Break;
      Inc(I, N);
      L := I - 1;
    end;
    SetLength(Result, L);
  end;
end;

function UTF8StartsWith(const Str, SubStr: UTF8String): Boolean;
begin
  if Str = SubStr then
    Exit(True);
  var L1 := Length(Str);
  var L2 := Length(SubStr);
  if (L2 = 0) or (L2 > L1) then
    Exit(False);
  for var I := 1 to L2 do
  begin
    if Str[I] <> SubStr[I] then
      Exit(False);
  end;
  Result := True;
end;


function UTF8EndsWith(const Str, SubStr: UTF8String): Boolean;
begin
  if Str = SubStr then
    Exit(True);
  var L1 := Length(Str);
  var L2 := Length(SubStr);
  if (L2 = 0) or (L2 > L1) then
    Exit(False);
  for var I := 0 to L2 - 1 do
  begin
    if Str[L1 - I] <> SubStr[L2 - I] then
      Exit(False);
  end;
  Result := True;
end;

function UTF8Trim(const Str: UTF8String): UTF8String;
begin
  var i := 1;
  var j := Length(Str);
  while (j > 0) and (Str[j] = ' ') do
    Dec(j);
  while (i <= j) and (Str[i] = ' ') do
    Inc(i);
  if i = j then
    Result := Str
  else
    Result := Copy(Str, i, j - i + 1);
end;

function UTF8LowerCase(const Str: UTF8String): UTF8String;
begin
  Result := Str;
  for var I := 1 to High(Result) do
  begin
    case Result[I] of
      'A'..'Z': Inc(Result[I], Ord('a') - Ord('A'));
    end;
  end;
end;

function UTF8Match(const Str, Sequence: UTF8String; Index :Integer): Boolean;
begin
  var L := Length(Str);
  for var I := 1 to Length(Sequence) do
  begin
    if (Index > L) or (Str[Index] <> Sequence[I]) then
      Exit(False);
    Inc(Index);
  end;
  Result := True;
end;

function UTF8Explode(const Sep, Str: UTF8String; Max: Integer = 0; DropBlanks: Boolean = False): TArray<UTF8String>;
var
  Index: Integer;
  Count: Integer;
  Start: Integer;
begin
  if DropBlanks and (Str = '') then
    Exit(nil);
  Count := 1;
  Start  := 1;
  Index := UTF8Pos(Sep, Str);
  while (Index > 0) and ((Max = 0) or (Count < Max)) do
  begin
    if (DropBlanks = False) or (Index > Start) then
    begin
      Inc(Count);
    end;
    Start := Index + Length(Sep);
    Index := UTF8Pos(Sep, Str, Start);
  end;
  if DropBlanks and (Start > Length(Str)) then
    Dec(Count);
  SetLength(Result, Count);
  Count := 0;
  Start := 1;
  Index := UTF8Pos(Sep, Str);
  while (Index > 0) and ((Max = 0) or (Count < Max - 1)) do
  begin
    if (DropBlanks = False) or (Index > Start) then
    begin
      Result[Count] := Copy(Str, Start, Index - Start);
      Inc(Count);
    end;
    Start := Index + Length(Sep);
    Index := UTF8Pos(Sep, Str, Start);
  end;
  if Count < Length(Result) then
    Result[Count] := Copy(Str, Start, MaxInt);
end;

function GetNum(const Str: UTF8String; Index, Count: Integer): Integer;
var
  Num: Integer;
begin
  Result := 0;
  while Count > 0 do
  begin
    Num := Ord(Str[Index]) - Ord('0');
    if (Num < 0) or (Num > 9) then
      raise Exception.Create('Invalid date ' + string(Str));
    Result := 10 * Result + Num;
    Inc(Index);
    Dec(Count);
  end;
end;

function JSONDecodeDateTime(const Str: UTF8String): TDateTime;
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
    raise Exception.Create('Invalid date ' + string(Str));

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
    if (Length(Str) < 16) or ((Str[11] <> 'T') and (Str[11] <> ' ')) or (Str[14] <> ':') then
      raise Exception.Create('Invalid date/time ' + string(Str));
    h := GetNum(Str, 12, 2);
    m := GetNum(Str, 15, 2);
    if (Length(Str) < 19) or (Str[17] <> ':') then
      s := 0
    else
      s := GetNum(Str, 18, 2);
    Result := Result + EncodeTime(h, m, s, 0);
  end;
end;

function EndOfLine(const Str: UTF8STring; EOL: TEndOfLine = eolWindows): UTF8String;
begin
  Result := Str;
  if EOL = eolWindows then
  begin
    for var I := Length(Result) downto 1 do
    begin
      case Result[I] of
        #13:
          if (I = Length(Result)) or (Result[I + 1] <> #10) then
            Insert(#10, Result, I + 1);
        #10:
          if (I = 1) or (Result[I - 1] <> #13) then
            Insert(#13, Result, I);
      end;
    end;
  end else begin
    var Remove, Replace: AnsiChar;
    if EOL = eolUnix then
    begin
      Remove := #13;
      Replace := #10;
    end else begin
      Remove := #10;
      Replace := #13;
    end;
    var I := 1;
    while I <= Length(Result) do
    begin
      // Remove CR/LF
      if (I < Length(Result)) and (Result[I] = #13) and (Result[I + 1] = #10) then
        Delete(Result, I + 1, 1);
      // Fix End of Line
      if Result[I] = Remove then
        Result[I] := Replace;
      Inc(I);
    end;
  end;
end;

function EscapeLineFeed(const Str: UTF8String): UTF8String;
var
  Count: Integer;
begin
  Result := Str;
  Count := 0;
  for var I := Length(Str) downto 1 do
  begin
    case Str[I] of
       #9: Result[I] := 't';
      #10: Result[I] := 'n';
      #13: Result[I] := 'r';
    else
      Continue;
    end;
    Inc(Count);
  end;
  if Count > 0 then
  begin
    var Len := Length(Str) + Count;
    SetLength(Result, Len);
    for var I := Length(Str) downto 1 do
    begin
      Result[Len] := Result[I];
      Dec(Len);
      if CharInSet(Str[I], [#9, #10, #13]) then
      begin
        Result[Len] := '\';
        Dec(Len);
        Dec(Count);
        if Count = 0 then
          Break;
      end;
    end;
  end;
end;

function UTF8Replace(const Source, OldPattern, NewPattern: UTF8String): UTF8String;
var
  lStr: Integer;
  lOld: Integer;
  lNew: Integer;
  Delta: Integer;
  nPos: Integer;
  aPos: array[0..9] of Integer;
  iPos: Integer;
begin
  Result := Source;
  lStr := Length(Source);
  lOld := Length(OldPattern);
  if (lOld = 0) or (lStr = 0) then
    Exit;
  lNew := Length(NewPattern);
  Delta := lNew - lOld;
  iPos := 1;
  repeat
    nPos := 0;
    repeat
      iPos := UTF8Pos(OldPattern, Result, iPos);
      if iPos = 0 then
        Break;
      aPos[nPos] := iPos - 1; // 0 based !
      Inc(nPos);
      Inc(iPos, lOld);
    until nPos = 10;
    if nPos > 0 then
    begin
      if Delta > 0 then
      begin
        Inc(lStr, Delta * nPos);
        SetLength(Result, lStr);
      end;
      var p := PAnsiChar(Result);
      for var i := nPos - 1 downto 0 do
      begin
        var n := aPos[i];
        if Delta <> 0 then
          Move(p[n + lOld], p[n + lNew], lStr - (n + lNew));
        Move(Pointer(NewPattern)^, p[n], lNew);
      end;
      if Delta < 0 then
      begin
        Inc(lStr, Delta * nPos);
        SetLength(Result, lStr);
      end;
      Inc(iPos, Delta * nPos);
    end;
  until nPos < 10;
  Assert(PAnsiChar(Result)[lStr] = #0);
end;

function UTF8Replace(const Source, OldPattern, NewPattern: UTF8String;
  Flags: TReplaceFlags): UTF8String;
begin
  if Flags = [rfReplaceAll] then
    Result := UTF8Replace(Source, OldPattern, NewPattern)
  else
    Result := UTF8String(StringReplace(string(Source), string(OldPattern), string(NewPattern), Flags));
end;

procedure BinToHex(Buffer: PByte; Text: PAnsiChar; BufSize: Integer);
var
  I: Integer;
begin
  for I := 0 to BufSize - 1 do
  begin
    Text[0] := LHX[Buffer[I] shr 4];
    Text[1] := LHX[Buffer[I] and $F];
    Inc(Text, 2);
  end;
end;

function MD5(const Str: UTF8String; Hexa: Boolean = True): UTF8String;
begin
  var Hash := THashMD5.Create;
  var L := Length(Str);
  if L > 0 then
    Hash.Update(Str[1], L);
  var D := Hash.HashAsBytes;
  L := Length(D);
  if Hexa then
  begin
    SetLength(Result, 2 * L);
    BinToHex(PByte(D), PAnsiChar(Result), L);
  end else begin
    Move(D[0], Result[1], L);
  end;
end;

function SHA1(Data: Pointer; Len: Integer): UTF8String;
var
  Digest: THashSHA1;
  Value : TBytes;
begin
  Digest := THashSHA1.Create;
  Digest.Update(Data, Len);
  Value := Digest.HashAsBytes;
  Result := Base64Encode(PByte(Value), Length(Value));
end;

function SHA1Str(const Str: UTF8String): UTF8String;
begin
  Result := SHA1(PByte(Str), Length(Str));
end;

function SHA1Hex(const Str: UTF8String): UTF8String;
var
  Digest: THashSHA1;
  Len: Integer;
  Value: TBytes;
begin
  Digest := THashSHA1.Create;
  Len := Length(Str);
  if Len > 0 then
    Digest.Update(Str[1], Len);
  Value := Digest.HashAsBytes;
  Len := Length(Value);
  SetLength(Result, 2 * Len);
  BinToHex(PByte(Value), PAnsiChar(Result), Len);
end;

procedure test;
var
  s: string;
  u: UTF8String;
begin
  Assert(Base64Encode('test') = 'dGVzdA==');
  Assert(TEncoding.ANSI.GetString(Base64Decode('dGVzdA==')) = 'test');
  s := 'Hello There';
  u := 'Hello There';
  assert(UTF8Pos('Hello', u) = 1);
  assert(UTF8Pos('Hello', u) = System.Pos('Hello', s));
  assert(UTF8Pos('There', u) = 7);
  assert(UTF8Pos('There', u) = System.Pos('There', s));
  assert(UTF8Pos('no', u)    = System.Pos('no', s));
  assert(UTF8Pos('e', u)     = System.Pos('e', s));
  assert(UTF8Pos('e', u, 4)  = System.Pos('e', s, 4));
  assert(UTF8Pos('H', u, 4)  = System.Pos('H', s, 4));
  u := '1234';
  assert(UTF8Pad(u, 2) = '12');
  assert(UTF8Pad(u, 6) = '1234  ');
end;

{ UTF8StringHelper }

function UTF8StringHelper.Explode(const Sep: UTF8String; Max: Integer;
  DropBlanks: Boolean): TArray<UTF8String>;
begin
  Result := UTF8Explode(Sep, Self, Max, DropBlanks);
end;

function UTF8StringHelper.IndexOf(const Str: UTF8String; Start: Integer = 1): Integer;
begin
  Result := UTF8Pos(Str, Self, Start);
end;

function UTF8StringHelper.StartsWith(const Str: UTF8String): Boolean;
begin
  Result := UTF8StartsWith(Self, Str);
end;

function UTF8StringHelper.EndsWith(const Str: UTF8String): Boolean;
begin
  Result := UTF8EndsWith(Self, Str);
end;

initialization
{$ifdef debug}
//test();
  Assert(EscapeLineFeed('toto') = 'toto');
  Assert(EscapeLineFeed(#10'toto') = '\ntoto');
  Assert(EscapeLineFeed(#13'toto') = '\rtoto');
  Assert(EscapeLineFeed(#13'to'#9'to'#10) = '\rto\tto\n');
  Assert(UTF8Trunc('Hello', 5) = 'Hello');
  Assert(UTF8Trunc('Hello', 4) = 'Hell');
  Assert(UTF8Trunc('Héllo', 4) = 'Hél');
  Assert(UTF8Trunc('Hé', 3) = 'Hé');
  Assert(UTF8Trunc('Hé', 2) = 'H');
  Assert(UTF8Trunc('Hé', 1) = 'H');
  Assert(UTF8Trunc('Méningocoque type A,C,Y,W-135, tétravalent', 35) = 'Méningocoque type A,C,Y,W-135, té');
  Assert(EndOfLine('Hello', eolWindows) = 'Hello');
  Assert(EndOfLine('Hello', eolUnix) = 'Hello');
  Assert(EndOfLine('Hello', eolOSX) = 'Hello');
  Assert(EndOfLine('Hello'#13'World', eolWindows) = 'Hello'#13#10'World');
  Assert(EndOfLine('Hello'#13'World', eolUnix) = 'Hello'#10'World');
  Assert(EndOfLine('Hello'#13'World', eolOSX) = 'Hello'#13'World');
  Assert(EndOfLine('Hello'#10'World', eolWindows) = 'Hello'#13#10'World');
  Assert(EndOfLine('Hello'#10'World', eolUnix) = 'Hello'#10'World');
  Assert(EndOfLine('Hello'#10'World', eolOSX) = 'Hello'#13'World');
  Assert(EndOfLine('Hello'#13#10'World', eolWindows) = 'Hello'#13#10'World');
  Assert(EndOfLine('Hello'#13#10'World', eolUnix) = 'Hello'#10'World');
  Assert(EndOfLine('Hello'#13#10'World', eolOSX) = 'Hello'#13'World');
  Assert(EndOfLine(#13'Hello'#13, eolWindows) = #13#10'Hello'#13#10);
  Assert(EndOfLine(#13'Hello'#13, eolUnix) = #10'Hello'#10);
  Assert(EndOfLine(#13'Hello'#13, eolOSX) = #13'Hello'#13);
  Assert(EndOfLine(#10'Hello'#10, eolWindows) = #13#10'Hello'#13#10);
  Assert(EndOfLine(#10'Hello'#10, eolUnix) = #10'Hello'#10);
  Assert(EndOfLine(#10'Hello'#10, eolOSX) = #13'Hello'#13);
  Assert(EndOfLine(#13#10'Hello'#13#10, eolWindows) = #13#10'Hello'#13#10);
  Assert(EndOfLine(#13#10'Hello'#13#10, eolUnix) = #10'Hello'#10);
  Assert(EndOfLine(#13#10'Hello'#13#10, eolOSX) = #13'Hello'#13);

  Assert(UTF8Replace('Hello', 'll', 'mm') = 'Hemmo');
  Assert(UTF8Replace('Hello', 'l', 'm') = 'Hemmo');
  Assert(UTF8Replace('été', 't', '1') = 'é1é');
  Assert(UTF8Replace('été', 't', '12') = 'é12é');
  Assert(UTF8Replace('é12é', '12', 't') = 'été');
  Assert(UTF8Replace('été été', 't', '1') = 'é1é é1é');
  Assert(UTF8Replace('été été', 't', '12') = 'é12é é12é');
  Assert(UTF8Replace('é12é é12é', '12', 't') = 'été été');
  var e1 := UTF8Encode(string('é'));
  var e2 := UTF8Encode(string('è'));
  Assert(UTF8Replace('éè', e1, e2) = 'èè');
  Assert(UTF8Replace('éè', e2, e1) = 'éé');
  Assert(UTF8Replace('ééè', 'éé', e2) = 'èè');
  Assert(UTF8Replace('éèè', 'èè', e1) = 'éé');
  Assert(UTF8Replace('éè', e1, 'èè') = 'èèè');
  Assert(UTF8Replace('éè', e2, 'éé') = 'ééé');
  Assert(UTF8Replace('|aaaaaaaaaa|aaaaaaaaaa|', 'a', 'b') = '|bbbbbbbbbb|bbbbbbbbbb|');
  Assert(UTF8Replace('|aaaaaaaaaa|aaaaaaaaaa|', 'a', 'bb') = '|bbbbbbbbbbbbbbbbbbbb|bbbbbbbbbbbbbbbbbbbb|');
  Assert(UTF8Replace('|aaaaaaaaaa|aaaaaaaaaa|', 'aa', 'b') = '|bbbbb|bbbbb|');
  Assert(UTF8Replace('aaa', 'a', 'ab') = 'ababab');
  Assert(UTF8Replace('aaa', 'aa', 'ab') = 'aba');

  Assert(MD5('test') = '098f6bcd4621d373cade4e832627b4f6');

  Assert(DecodeIfUTF8('été') = 'été');
  Assert(DecodeIfUTF8('Ã©tÃ©') = 'été');
  Assert(DecodeIfUTF8('_Ã©tÃ©_') = '_été_');
  Assert(DecodeIfUTF8('_étÃ©_') = '_été_');
  var s: AnsiString := 'été';
  Assert(EnsureUTF8(s) = 'été');
  s := '_étÃ©_';
  Assert(EnsureUTF8(s) = '_été_');
  var S1: string := '{"NumLigne":0,"Nom":"DOLIPRANE 1000 mg Cpr Plq/8","CIP":"3400935955838","CIS":"60234100","ProductId":19649,"ProductName":"DOLIPRANE 1000 mg cp","PrintName":"PARACETAMOL 1 g cp (DOLIPRANE 1000 mg Cpr Plq/8)","PackageId":41928,"CommonNameID":3170,"CommonName":"PARACETAMOL 1 g cp","CountMin":0,"CountMax":0,"CountUnit":{"id":129,"Name":"comprimÃ©","PluralName":"comprimÃ©s","Feminine":false},"CountTimes":0,"PauseDays":0,"Duration":0,"Phases":[{"Doses":[{"Count":0.25,"Moment":"mmMorning"},{"Count":0.5,"Moment":"mmNoon"}]}],"StartDate":null,"StopDate":null,"NbRenouv":0,"NbBoites":1,"NbConditionnement":8,"TauxR":"65%","TSO":false}';
  var S2: string := '{"NumLigne":0,"Nom":"DOLIPRANE 1000 mg Cpr Plq/8","CIP":"3400935955838","CIS":"60234100","ProductId":19649,"ProductName":"DOLIPRANE 1000 mg cp","PrintName":"PARACETAMOL 1 g cp (DOLIPRANE 1000 mg Cpr Plq/8)","PackageId":41928,"CommonNameID":3170,"CommonName":"PARACETAMOL 1 g cp","CountMin":0,"CountMax":0,"CountUnit":{"id":129,"Name":"comprimé","PluralName":"comprimés","Feminine":false},"CountTimes":0,"PauseDays":0,"Duration":0,"Phases":[{"Doses":[{"Count":0.25,"Moment":"mmMorning"},{"Count":0.5,"Moment":"mmNoon"}]}],"StartDate":null,"StopDate":null,"NbRenouv":0,"NbBoites":1,"NbConditionnement":8,"TauxR":"65%","TSO":false}';
  var S3: string := DecodeIfUTF8(S1);
  Assert(S3 = S2);
  Assert(UTF8EndsWith('Test', '') = False);
  Assert(UTF8EndsWith('Test', 'A') = False);
  Assert(UTF8EndsWith('A', 'Test') = False);
  Assert(UTF8EndsWith('Test', 'Test'));
  Assert(UTF8EndsWith('Test', 'est'));
  Assert(UTF8EndsWith('Test', 'st'));
  Assert(UTF8EndsWith('Test', 't'));
  Assert(UTF8Trim('hello') = 'hello');
  Assert(UTF8Trim(' hello') = 'hello');
  Assert(UTF8Trim('hello ') = 'hello');
  Assert(UTF8Trim(' hello ') = 'hello');
  Assert(UTF8Trim('A') = 'A');
  Assert(UTF8Trim(' ') = '');

  Assert(SHA1Hex('Wikipédia, l''encyclopédie libre et gratuite') = '6153a6fa0e4880d9b8d0be4720f78e895265d0a9');
  Assert(SHA1Hex(UTF8String(StringOfChar('A', 66))) = 'eddee92010936db2c45d2c9f5fdd2726fcd28789');
  Assert(SHA1Hex('Wikipédia, l''encyclopédie libre et gratuitE') = '11f453355b28e1158d4e516a2d3edf96b3450406');
  Assert(SHA1Hex('') = 'da39a3ee5e6b4b0d3255bfef95601890afd80709');
{$endif}
end.
