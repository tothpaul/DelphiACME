unit Execute.OpenSSL;

{

  ACME Delphi client for Let's Encrypt (c)2018-2025 Execute SARL <contact@execute.fr>

  This component is NOT FREE !

  BUT you can use it any OpenSource project under the GPL

  AND you have to register a licence to use it in any commercial product

  you are NOT allowed to use this component to register a commercial website
  without a registered licence, even with a GPL product around this component

}

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils,
  Execute.ACME,
{$IFDEF LINUX64}
  IdGlobal,
{$ENDIF}
  IdSSLOpenSSL,
  IdSSLOpenSSLHeaders;

type
  TRSACertificate = class(TInterfacedObject, IRSACertificate)
  private
    RSA: PRSA;
    procedure FreeRSA;
    procedure GenerateRSA(KeyLength: Integer);
    function RSAKeyToPEM(const Password: UTF8String = ''): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GenerateRSAKey(KeyLength: Integer; const Password: string): string;
    function LoadFromPEM(const PEM, Password: string): Boolean;
    function GetCertificate(const AStr: string; var Domain: string): TBytes;
    function GetPublicKey(var n, e: TBytes): Boolean;
    function SigningRequest(const DomainName: UTF8String; const SubjectAltName: TArray<UTF8String>): TBytes;
    function Sign(const Data: AnsiString): TBytes;
  end;

  TExecuteACMEOpenSSLCertificate = class(TCustomRSACertificate)
  public
    function GetRSACertificate: IRSACertificate; override;
  end;

implementation

// missing functions in idSSLOpenSSLHeaders
// https://github.com/IndySockets/Indy/issues/226
type
  SK_POP_FREE_PROC = procedure(_par1: PSTACK_OF_X509_EXTENSION); cdecl;

{$IFDEF MSWINDOWS}
const
  libeay32 = 'libeay32.dll';
  {$DEFINE STATIC}
{$ENDIF}

{$IFDEF STATIC}

{$IFDEF MSWINDOWS}{$DEFINE DELAYED}{$ENDIF}
{$IFDEF DELAYED}{$WARN SYMBOL_PLATFORM OFF}{$ENDIF}
function BN_num_bits(a: pBIGNUM): Integer; cdecl; external libeay32{$IFDEF DELAYED} delayed{$ENDIF};
function BN_bn2bin(a: pBIGNUM; p: Pointer): Integer; cdecl; external libeay32{$IFDEF DELAYED} delayed{$ENDIF};
function X509_REQ_set_subject_name(x: PX509_REQ; name: pX509_NAME): Integer; cdecl; external libeay32{$IFDEF DELAYED} delayed{$ENDIF};
function X509_NAME_get_text_by_NID(name: PX509_NAME; nid: Integer; buf: PAnsiChar; len: Integer): Integer; cdecl; external libeay32{$IFDEF DELAYED} delayed{$ENDIF};
function sk_X509_EXTENSION_new_null:PSTACK_OF_X509_EXTENSION; cdecl; external libeay32 name 'sk_new_null'{$IFDEF DELAYED} delayed{$ENDIF};
function sk_X509_EXTENSION_push(stack: PSTACK_OF_X509_EXTENSION; data: Pointer): Integer; cdecl; external libeay32 name 'sk_push'{$IFDEF DELAYED} delayed{$ENDIF};
function sk_X509_EXTENSION_pop_free(stack: PSTACK_OF_X509_EXTENSION; func: SK_POP_FREE_PROC): Integer; cdecl; external libeay32 name 'sk_pop_free'{$IFDEF DELAYED} delayed{$ENDIF};
procedure X509_EXTENSION_free(stack: PSTACK_OF_X509_EXTENSION); cdecl; external libeay32 name 'sk_free'{$IFDEF DELAYED} delayed{$ENDIF};
{$IFDEF DELAYED}{$WARN SYMBOL_PLATFORM ON}{$ENDIF}

{$ELSE}

var
  BN_num_bits: function(a: pBIGNUM): Integer; cdecl;
  BN_bn2bin: function(a: pBIGNUM; p: Pointer): Integer; cdecl;
  X509_REQ_set_subject_name: function(x: PX509_REQ; name: pX509_NAME): Integer; cdecl;
  X509_NAME_get_text_by_NID: function(name: PX509_NAME; nid: Integer; buf: PAnsiChar; len: Integer): Integer; cdecl;
  sk_X509_EXTENSION_new_null: function: PSTACK_OF_X509_EXTENSION; cdecl;
  sk_X509_EXTENSION_push: function(stack: PSTACK_OF_X509_EXTENSION; data: Pointer): Integer; cdecl;
  sk_X509_EXTENSION_pop_free: function (stack: PSTACK_OF_X509_EXTENSION; func: SK_POP_FREE_PROC): Integer; cdecl;
  X509_EXTENSION_free: procedure(stack: PSTACK_OF_X509_EXTENSION); cdecl;

procedure LoadFunctions;
const
  SSLDLLVers : array [0..8] of string = ('','.10','.1.0.2','.1.0.1','.1.0.0','0.9.9','.0.9.8','.0.9.7','0.9.6');
var
  lib: THandle;
begin
  if @BN_num_bits = nil then
  begin
    lib := HackLoad('libcrypto', SSLDLLVers);//GetCryptLibHandle;
  {$IFDEF LOG}WriteLn('CryptLibHandle = ', lib);{$ENDIF}
    @BN_num_bits := GetProcAddress(lib, 'BN_num_bits');
  {$IFDEF LOG}WriteLn('BN_num_bits = ', Cardinal(@BN_num_bits));{$ENDIF}
    @BN_bn2bin := GetProcAddress(lib, 'BN_bn2bin');
  {$IFDEF LOG}WriteLn('BN_bn2bin = ', Cardinal(@BN_bn2bin));{$ENDIF}
    @X509_REQ_set_subject_name := GetProcAddress(lib, 'X509_REQ_set_subject_name');
  {$IFDEF LOG}WriteLn('X509_REQ_set_subject_name = ', Cardinal(@X509_REQ_set_subject_name));{$ENDIF}
    @X509_NAME_get_text_by_NID := GetProcAddress(lib, 'X509_NAME_get_text_by_NID');
  {$IFDEF LOG}WriteLn('X509_NAME_get_text_by_NID = ', Cardinal(@X509_NAME_get_text_by_NID));{$ENDIF}
    @sk_X509_EXTENSION_new_null := GetProcAddress(lib, 'sk_new_null');
    @sk_X509_EXTENSION_push := GetProcAddress(lib, 'sk_push');
    @sk_X509_EXTENSION_pop_free := GetProcAddress(lib, 'sk_pop_free');
    @X509_EXTENSION_free := GetProcAddress(lib, 'sk_free');
  end;
end;
{$ENDIF}

{ TRSACertificate }

constructor TRSACertificate.Create;
begin
  inherited;
  if not idSSLOpenSSLHeaders.Load then
    raise Exception.Create('Can not initialize OpenSSL !');
{$IFNDEF STATIC}
  LoadFunctions;
{$ENDIF}
end;

destructor TRSACertificate.Destroy;
begin
  FreeRSA;
  inherited;
end;

procedure TRSACertificate.FreeRSA;
begin
  if RSA <> nil then
  begin
    RSA_free(RSA);
    RSA := nil;
  end;
end;

procedure TRSACertificate.GenerateRSA(KeyLength: Integer);
var
  bne: PBIGNUM;
begin
  RSA := RSA_new();
  if RSA = nil then
    raise Exception.Create('Can not initialize RSA');
  try
    bne := BN_new();
    if bne = nil then
      raise Exception.Create('Can not initialize BIGNUM');
    try
      BN_set_word(bne, RSA_F4);
      if RSA_generate_key_ex(RSA, KeyLength, bne, nil) <> 1 then
        raise Exception.Create('Can not generate RSA Key');
    finally
      BN_free(bne);
    end;
  except
    FreeRSA;
    raise;
  end;
end;

function TRSACertificate.GenerateRSAKey(KeyLength: Integer;
  const Password: string): string;
begin
  FreeRSA;
  GenerateRSA(KeyLength);
  Result := RSAKeyToPEM(UTF8String(Password));
end;

function RequestPassword(buf: PAnsiChar; size: Integer; rwflag: Integer; u: Pointer): Integer; cdecl;
var
  p: UTF8String absolute u;
begin
  Result := Length(p);
  if Result > Size then
    Result := Size;
  if Result > 0 then
    Move(p[1], buf^, Result);
end;

function TRSACertificate.LoadFromPEM(const PEM, Password: string): Boolean;
var
  key: AnsiString;
  pwd: UTF8String;
  bio: pBIO;
begin
  FreeRSA;
  key := AnsiString(PEM);
  pwd := UTF8String(Password);
  bio := BIO_new_mem_buf(Pointer(key), Length(key));
  if bio = nil then
    Exit(False);
  try
    OpenSSL_add_all_ciphers();
    RSA := PEM_read_bio_RSAPrivateKey(bio, nil, RequestPassword, Pointer(pwd));
    Result := RSA <> nil;
  finally
    BIO_free(bio);
  end;
end;

function TRSACertificate.RSAKeyToPEM(const Password: UTF8String): string;
var
  bio: PBIO;
  enc: Pointer;
  pwd: Pointer;
  len: Integer;
  ptr: PAnsiChar;
begin
  bio := BIO_new(BIO_s_mem);
  if bio = nil then
    raise Exception.Create('Can not initialize BIO');
  try
    if Password = '' then
    begin
      enc := nil;
      pwd := nil;
    end else begin
      enc := EVP_des_ede3_cbc;
      pwd := Pointer(Password);
    end;
    PEM_write_bio_RSAPrivateKey(bio, rsa, enc, nil, 0, nil, pwd);
    len := BIO_ctrl(bio, BIO_CTRL_INFO, 0, @ptr);
    SetString(Result, ptr, len);
  finally
    BIO_free(bio);
  end;
end;

function TRSACertificate.Sign(const Data: AnsiString): TBytes;
var
  ctx: EVP_MD_CTX;
  KEY: PEVP_PKEY;
  len: NativeInt;
begin
  Result := nil;
  KEY := EVP_PKEY_new();
  if KEY = nil then
    Exit;
  try
    if EVP_PKEY_set1_RSA(KEY, RSA) <> 1 then
      Exit;
    EVP_MD_CTX_init(@ctx);
    try
      if EVP_DigestSignInit(@ctx, nil, EVP_sha256, nil, KEY) <> 1 then
        Exit;
      if EVP_DigestUpdate(@ctx, Pointer(Data), Length(Data)) <> 1 then
        Exit;
      if EVP_DigestSignFinal(@ctx, nil, @len) <> 1 then
        Exit;
      SetLength(Result, len);
      EVP_DigestSignFinal(@ctx, PAnsiChar(Result), @len);
    finally
      EVP_MD_CTX_cleanup(@ctx);
    end;
  finally
    EVP_PKEY_free(KEY);
  end;
end;

function TRSACertificate.SigningRequest(const DomainName: UTF8String;
  const SubjectAltName: TArray<UTF8String>): TBytes;
var
  len : Integer;
  csr : PX509_REQ;
  nme : PX509_NAME;
  key : PEVP_PKEY;
  ptr : Pointer;
  alt : UTF8String;
  exts: PSTACK_OF_X509_EXTENSION;
  ex  : PX509_EXTENSION;
  i   : Integer;
begin
  csr := X509_REQ_new();
  try
    // Set the subject as CN=DomainName
    nme := X509_NAME_new();
    try
      if X509_NAME_add_entry_by_txt(nme, 'CN', MBSTRING_ASC, PAnsiChar(DomainName), Length(DomainName), -1, 0) <> 1 then
        raise Exception.Create('Can''t set domain name entry "CN=' + string(DomainName) + '"');
      if X509_REQ_set_subject_name(csr, nme) <> 1 then
        raise Exception.Create('Can''t set domain name subject "CN=' + string(DomainName) + '"');
      if SubjectAltName <> nil then
      begin
      // https://stackoverflow.com/questions/15964681/how-do-you-add-a-subjectnamealt-extension-to-x509-req
        alt := 'DNS:' + SubjectAltName[0];
        for i := 1 to High(SubjectAltName) do
          alt := alt + ', DNS:' + SubjectAltName[i];
        exts := sk_X509_EXTENSION_new_null();
        ex := X509V3_EXT_conf_nid(nil, nil, NID_subject_alt_name, PAnsiChar(alt));
        sk_X509_EXTENSION_push(exts, ex);
        X509_REQ_add_extensions(csr, exts);
      end;
    finally
      X509_NAME_free(nme);
    end;

    key := EVP_PKEY_new();
    if key = nil then
      raise Exception.Create('Can''t initialize EVP');
    try
    // Assign RSA to key
      if EVP_PKEY_set1_RSA(key, rsa) <> 1 then
        raise Exception.Create('Can''t assign RSA key for CSR');
    // set the Public Key info
      if X509_REQ_set_pubkey(csr, key) <> 1 then
        raise Exception.Create('Can''t set CSR public key');
    // Sign the request
      if X509_REQ_sign(csr, key, EVP_sha256) <> 512 then // returns the key length
        raise Exception.Create('Can''t sign the Certificat Signing Request');
    finally
      EVP_PKEY_free(key);
    end;

  // return the CSR in DER format (not PEM)
    len := i2d_X509_REQ(csr, nil);
    SetLength(Result, len);
    // Warning, ptr will move to the end of Result !
    ptr := Pointer(Result);
    i2d_X509_REQ(csr, @ptr);

  finally
    X509_REQ_free(csr);
  end;
end;

function TRSACertificate.GetCertificate(const AStr: string;
  var Domain: string): TBytes;
var
  str: AnsiString;
  bio: pBIO;
  crt: pX509;
  len: Integer;
  ptr: Pointer;
  nme: pX509_NAME;
begin
  str := AnsiString(AStr);
  bio := BIO_new_mem_buf(Pointer(str), Length(str));
  if bio = nil then
    raise Exception.Create('Unable to load certificate');
  try
    crt := PEM_read_bio_X509(bio, nil, nil, nil);
    if crt = nil then
      raise Exception.Create('Invalid certificate');
    try
      nme := X509_get_subject_name(crt);
      len := X509_NAME_get_text_by_NID(nme, NID_CommonName, nil, 0);
      SetLength(str, len);
      X509_NAME_get_text_by_NID(nme, NID_CommonName, PAnsiChar(str), len + 1); // there's an extra #0 in a string !
      Domain := string(str);

      len := i2d_X509(crt, nil);
      SetLength(Result, len);
      ptr := Pointer(Result);
      i2d_X509(crt, @ptr);

    finally
      X509_free(crt);
    end;
  finally
    BIO_free(bio);
  end;
end;

function TRSACertificate.GetPublicKey(var n, e: TBytes): Boolean;
var
  len: Integer;
begin
  Result := False;

  if RSA = nil then
    Exit;

  len := (BN_num_bits(RSA.n) + 7) div 8;
  SetLength(n, len);
  BN_bn2bin(RSA.n, n);

  len := (BN_num_bits(RSA.e) + 7) div 8;
  SetLength(e, len);
  BN_bn2bin(RSA.e, e);

  Result := True;
end;

{ TExecuteACMEOpenSSLCertificate }

function TExecuteACMEOpenSSLCertificate.GetRSACertificate: IRSACertificate;
begin
  Result := TRSACertificate.Create;
end;

end.
