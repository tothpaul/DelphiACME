program ACMEConsole;

{

  this console mode demo use TExecuteACME with synchronous calls

  it was first designed for Linux target, but is also Win64 compatible

}

{$APPTYPE CONSOLE}
{$ZEROBASEDSTRINGS OFF}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  Execute.ACME,
  idHTTPServer,
  idContext,
  idCustomHTTPServer;

const
  ACME_PASSWORD = 'dummy_password';

type
  TACMEDemo = class
  private
    FToken: string;
    FThumbprint: string;
    FStatus: (stNone, stPending, stDone, stError);
  public
    ACME: TExecuteACME;
    HTTP: TIdHTTPServer;
    function OnPassword(Sender: TObject; KeyType: TKeyType; var Password: string): Boolean;
    procedure OnHttpChallenge(Sender: TObject; const Domain, Token, Thumbprint: string; var Processed: Boolean);
    procedure OnCertificate(Sender: TObject; Certificate: TStrings);
    procedure OnError(Sender: TObject; const Error: string);
    procedure OnDone(Sender: TObject);
    procedure OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

function LoadFile(const AFileName: string): string;
var
  Str: AnsiString;
  f: file;
begin
  Assignfile(f, AFileNAme);
  Reset(f, 1);
  SetLength(Str, FileSize(f));
  BlockRead(f, Str[1], Length(Str));
  CloseFile(f);
  Result := string(str);
end;

procedure LoadCertificat(Key: TStrings; const FileName: string);
begin
  if FileExists(FileName) then
    Key.LoadFromFile(FileName)
  else begin
    TExecuteACME.GeneraRSAKey(Key, ACME_PASSWORD);
    Key.WriteBOM := False; // required under Linux !
    Key.SaveToFile(FileName);
  end;
end;

constructor TACMEDemo.Create;
begin
  inherited;
  ACME := TExecuteACME.Create(nil);
  ACME.OnPassword := OnPassword;
  ACME.OnHttpChallenge := OnHttpChallenge;
  ACME.OnCertificate := OnCertificate;
  ACME.OnError := OnError;
  ACME.OnDone := OnDone;

  HTTP := TidHTTPServer.Create(nil);
  HTTP.OnCommandGet := OnCommandGet;
end;

destructor TACMEDemo.Destroy;
begin
  HTTP.Free;
  ACME.Free;
  inherited;
end;

function TACMEDemo.OnPassword(Sender: TObject; KeyType: TKeyType; var Password: string): Boolean;
begin
  Password := ACME_PASSWORD;
  Result := True;
end;

procedure TACMEDemo.OnHttpChallenge(Sender: TObject; const Domain: string; const Token: string; const Thumbprint: string; var Processed: Boolean);
begin
  FToken := Token;
  FThumbprint := Thumbprint;
  Processed := True;
end;

procedure TACMEDemo.OnCertificate(Sender: TObject; Certificate: TStrings);
begin
  Certificate.WriteBOM := False; // required for Linux !
  Certificate.SaveToFile('domain.crt');
  WriteLn('Certificate saved !');
  FStatus := stDone;
end;

procedure TACMEDemo.OnError(Sender: TObject; const Error: string);
begin
  WriteLn(Error);
  FStatus := stError;
end;

procedure TACMEDemo.OnDone(Sender: TObject);
begin
  FStatus := stPending;
end;

procedure TACMEDemo.OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  URI: string;
  Token: string;
begin
  URI := ARequestInfo.URI;
  if Pos(TExecuteACME.WELL_KNOWN_URL, URI) = 1 then
  begin
    Token := Copy(URI, 1 + Length(TExecuteACME.WELL_KNOWN_URL));
    if Token = FToken then
    begin
      AResponseInfo.ContentType := 'application/octet-stream';
      AResponseInfo.ContentText := Token + '.' + FThumbprint;
    end;
  end;
end;

procedure TACMEDemo.Execute;
var
  Str: string;
begin
  Str := ParamStr(1);
  if Str = '/?' then
  begin
    WriteLn(ParamStr(0), ' [domain name] [contact email | unregister]');
    Exit;
  end;
  Write('Domain name: ');
  if Str = '' then
    ReadLn(Str)
  else
    WriteLn(Str);
  ACME.DomainName := Str;


  ACME.Environment := TEnvironment.StagingV2;
  LoadCertificat(ACME.AccountKey, 'account.key');
  LoadCertificat(ACME.DomainKey, 'domain.key');

  Str := ParamStr(2);
  if Str = 'unregister' then
  begin
    WriteLn('Unregistering domain');
    ACME.UnregisterDomainNow(LoadFile('domain.crt'), TACMERevokeReason.unspecified);
  end else begin
    Write('Contact email: ');
    if Str = '' then
      ReadLn(Str)
    else
      WriteLn(Str);
    ACME.ContactEmail := Str;

    HTTP.Active := True;

    WriteLn('Registering domain');
    ACME.RegisterDomainNow;

    while FStatus <= stPending do
    begin
      WriteLn('pending...');
      Sleep(1000);
      if FStatus > stPending then
        Break;
      ACME.FinalizeDomainNow;
    end;
  end;
end;

var
  Demo: TACMEDemo;
begin
  WriteLn('Interactive ACME Demo for Win32/64 and Linux64 (c)2020 Execute SARL');
  Demo := TACMEDemo.Create;
  try
    Demo.Execute;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Demo.Free;
end.
