unit Execute.UTF8.Utils;

interface

uses
  System.SysUtils;

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

function DetectUF8(Ptr: PByte; Size: Integer): Boolean;

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

function UTF8Match(const Str, Sequence: UTF8String; Index :Integer): Boolean;

function LowUID(const UID: UTF8String): UTF8String;

type
  TEndOfLine = (
    eolWindows, // \r\n  #13#10
    eolUnix,    // \n    #10
    eolOSX      // \r    #13
  );
function EndOfLine(const Str: UTF8STring; EOL: TEndOfLine = eolWindows): UTF8String;
function EscapeLineFeed(const Str: UTF8String): UTF8String;

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
  for Index := 1 to Length(UID) do
    case Result[Index] of
      'A'..'F': Dec(PByte(@Result[Index])^, Ord('A') - Ord('a'));
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
    while I < Len do
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
      if I + N > Len then
        Break;
      Inc(I, N);
      L := I;
    end;
    SetLength(Result, L);
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
{$endif}
end.
