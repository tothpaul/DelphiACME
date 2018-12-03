unit ACMEDemo.Main;

{
   Sample demonstration of Execute's TExecuteACME component for Delphi
   (c)2018 Execute SARL

   <contact@execute.fr>

}

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.UITypes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  IdContext, IdCustomHTTPServer, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdHTTPServer,

// init this to be sure that OpenSSL is available (but it's not required)
  idSSLOpenSSLHeaders,

// ACME component (dependencies: Indy, Execute.JSON, Execute.RTTI)
  Execute.ACME;

type
  TForm1 = class(TForm)
    ExecuteACME1: TExecuteACME;
    IdHTTPServer1: TIdHTTPServer;
    Memo1: TMemo;
    btRegister: TButton;
    Label1: TLabel;
    edDomain: TEdit;
    Label2: TLabel;
    edContact: TEdit;
    btSave: TButton;
    cbMode: TComboBox;
    btUnregister: TButton;
    btLoad: TButton;
    function ExecuteACME1Password(Sender: TObject; KeyType: TKeyType;
      var Password: string): Boolean;
    procedure ExecuteACME1HttpChallenge(Sender: TObject; const Domain, Token,
      Thumbprint: string);
    procedure ExecuteACME1Certificate(Sender: TObject; Certificate: TStrings);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure FormCreate(Sender: TObject);
    procedure ExecuteACME1Error(Sender: TObject; const Error: string);
    procedure btRegisterClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure ExecuteACME1Done(Sender: TObject);
  private
    { Déclarations privées }
    FDomain    : string;
    FToken     : string;
    FThumbprint: string;
    procedure LoadOrCreateKey(Key: TStrings; const Name: string);
    procedure WMUser(var Msg: TMessage); message WM_USER;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Load or Create private keys
//   one for the Let's Ecrypt account
//   one for the registered domain
procedure TForm1.FormCreate(Sender: TObject);
begin
  if not idSSLOpenSSLHeaders.Load then
    raise Exception.Create('Can not initialize OpenSSL !');

  LoadOrCreateKey(ExecuteACME1.AccountKey, 'account.key');
  LoadOrCreateKey(ExecuteACME1.DomainKey, 'domain.key');
end;

// utility function
procedure TForm1.LoadOrCreateKey(Key: TStrings; const Name: string);
var
  Str: string;
begin
  if FileExists(Name) then
    Key.LoadFromFile(Name)
  else begin
    if MessageDlg(
      'Do you want to create the private key ''' + Name + ''' ?',
      mtConfirmation,
      [mbYes, mbNo],
      0
    ) <> mrYes then
      Exit;
    Str := '';
    if not InputQuery('Protect your key with a password', 'Password for ' + Name,  Str) then
      Exit;
    TExecuteACME.GeneraRSAKey(Key, Str);
    Key.SaveToFile(Name);
  end;
end;

// start registration
procedure TForm1.btRegisterClick(Sender: TObject);
begin
  if cbMode.ItemIndex = 0 then
    ExecuteACME1.Environment := TEnvironment.StagingV2
  else
    ExecuteACME1.Environment := TEnvironment.ProductionV2;

  ExecuteACME1.DomainName := edDomain.Text;
  ExecuteACME1.ContactEmail := edContact.Text;

  if Sender = btRegister then
  begin
  // need to handle HTTP Challenge
    idHTTPServer1.Active := True;
  // start registration process
    ExecuteACME1.RegisterDomain;
  end else begin
    ExecuteACME1.UnregisterDomain(Memo1.Lines.Text, TACMERevokeReason.unspecified);
  end;
end;

// is you have defined a password, ExecuteACME ask you for the password of the Keyfile
function TForm1.ExecuteACME1Password(Sender: TObject; KeyType: TKeyType;
  var Password: string): Boolean;
var
  Name: string;
begin
  if KeyType = ktAccount then
    Name := 'Account'
  else
    Name := 'Domain';
  Result := InputQuery(Name + ' Key', Name + ' password', Password);
end;

// upon registration, you have to create an HTTP response
// in this sample demo, the HTTP service is self hosted with TidHTTP
// you can also put the requested file on a Web server
// this URL
//   http://<DomainName>/.well-known/acme-challenge/<Token>
// need to returns
//   <Token>.<Thumbprint>
procedure TForm1.ExecuteACME1HttpChallenge(Sender: TObject; const Domain, Token,
  Thumbprint: string);
begin
  FDomain := Domain;
  FToken := Token;
  FThumbprint := Thumbprint;
end;

// idHTTP server response to the well known ACME challenge
procedure TForm1.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if ARequestInfo.URI = TExecuteACME.WELL_KNOWN_URL + FToken then
  begin
    AResponseInfo.ContentText := FToken + '.' + FThumbprint;
    PostMessage(Handle, WM_USER, 0, 0);
  end;
end;

// just a feedback of the HTTP Challenge request
procedure TForm1.WMUser(var Msg: TMessage);
begin
  ShowMessage('HTTP Challenge done !');
end;

// the certificate is validated, you can save it
procedure TForm1.ExecuteACME1Certificate(Sender: TObject;
  Certificate: TStrings);
begin
  Memo1.Lines.Assign(Certificate);
  btSave.Enabled := True;
  btUnregister.Enabled := True;
  ShowMessage('You can save and use this certificate !');
end;

procedure TForm1.btLoadClick(Sender: TObject);
var
  Str: string;
begin
  Str := 'Domaine.crt';
  if PromptForFileName(
   Str,
   'Certificat (*.crt)|*.crt|All files(*.*)|*.*',
   'CRT',
   'Load a Domain certificate',
   ''
  )
  then
  begin
    Memo1.Lines.LoadFromFile(Str);
    btUnregister.Enabled := True;
  end;
end;

procedure TForm1.btSaveClick(Sender: TObject);
var
  Str: string;
begin
  Str := 'Domaine.crt';
  if PromptForFileName(
   Str,
   'Certificat (*.crt)|*.crt|All files(*.*)|*.*',
   'CRT',
   'Save the Domain certificate',
   '',
   True
  )
  then
   Memo1.Lines.SaveToFile(Str);
end;

procedure TForm1.ExecuteACME1Error(Sender: TObject; const Error: string);
begin
  ShowMessage(Error);
end;

procedure TForm1.ExecuteACME1Done(Sender: TObject);
begin
// the first request will probably not deliver a certificat
// you have to request it once again after the HttpChallenge is done
  ShowMessage('Request done');
end;


end.
