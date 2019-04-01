unit ACMEDemo.Main;

{
   Sample demonstration of Execute's TExecuteACME component for Delphi
   (c)2018-2019 Execute SARL

   <contact@execute.fr>

}

{

  New in version 1.1

    - better error handling

    - new parameter on OnHTTPChallenge event to specify whether the Challenge has been processed.

    - the FinalizeDomain method is now used to get the Certificat instead of a new RegisterDomain call

    - the public property OrderURL is required by FinalizeDomain
      you have to save and restore it if you want to restart the application before finalizing the registration.

    - multiple sub domains can be defined with SubjectAltNames.

}

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.UITypes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  IdContext, IdCustomHTTPServer, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdHTTPServer,

// to call idSSLOpenSSLHeaders.Load
  idSSLOpenSSLHeaders,

// ACME component (dependencies: Indy, Execute.JSON, Execute.RTTI)
  Execute.ACME;

type
  TChallenge = record
    Domain    : string;
    Token     : string;
  end;

  TForm1 = class(TForm)
    ExecuteACME1: TExecuteACME;
    IdHTTPServer1: TIdHTTPServer;
    Memo1: TMemo;
    btRegister: TButton;
    Label2: TLabel;
    edContact: TEdit;
    btSave: TButton;
    cbMode: TComboBox;
    btUnregister: TButton;
    btLoad: TButton;
    Label3: TLabel;
    mmSubjectAltNames: TMemo;
    mmHTTP: TMemo;
    btFinalize: TButton;
    Label1: TLabel;
    edDomainName: TEdit;
    cbHTTPChallenges: TCheckBox;
    function ExecuteACME1Password(Sender: TObject; KeyType: TKeyType;
      var Password: string): Boolean;
    procedure ExecuteACME1HttpChallenge(Sender: TObject; const Domain, Token,
      Thumbprint: string; var Processed: Boolean);
    procedure ExecuteACME1Certificate(Sender: TObject; Certificate: TStrings);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure FormCreate(Sender: TObject);
    procedure ExecuteACME1Error(Sender: TObject; const Error: string);
    procedure btRegisterClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure ExecuteACME1Done(Sender: TObject);
    procedure cbHTTPChallengesClick(Sender: TObject);
  private
    { Déclarations privées }
    FChallenges: TArray<TChallenge>;
    FThumbprint: string;
    procedure LoadOrCreateKey(Key: TStrings; const Name: string);
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
    if not InputQuery('Protect your key with a password', #1'Password for ' + Name,  Str) then
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

  ExecuteACME1.DomainName := edDomainName.Text;
  ExecuteACME1.ContactEmail := edContact.Text;
  // added in version 1.1
  ExecuteACME1.SubjectAltNames.Assign(mmSubjectAltNames.Lines);

  if Sender = btRegister then
  begin
  // need to handle HTTP Challenge
    idHTTPServer1.Active := cbHTTPChallenges.Checked;
  // start registration process
    ExecuteACME1.RegisterDomain;
  end else
  if Sender = btUnregister then
  begin
    ExecuteACME1.UnregisterDomain(Memo1.Lines.Text, TACMERevokeReason.unspecified);
  end else
  if Sender = btFinalize then
  begin
    ExecuteACME1.FinalizeDomain;
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
  Result := InputQuery(Name + ' Key', #1 + Name + ' password', Password);
end;

// upon registration, you have to create an HTTP response
// in this sample demo, the HTTP service is self hosted with TidHTTP
// you can also put the requested file on a Web server
// this URL
//   http://<DomainName>/.well-known/acme-challenge/<Token>
// need to returns
//   <Token>.<Thumbprint>
procedure TForm1.ExecuteACME1HttpChallenge(Sender: TObject; const Domain, Token,
  Thumbprint: string; var Processed: Boolean);
var
  Count: Integer;
  Index: Integer;
  Found: Integer;
begin
// this array if required only for multiple subdomains (one Token per domain)
  Count := Length(FChallenges);
  Found := -1;
  for Index := 0 to Count - 1 do
  begin
    if FChallenges[Index].Domain = Domain then
    begin
      Found := Index;
      Break;
    end;
  end;
  if Found = -1 then
  begin
    Found := Count;
    SetLength(FChallenges, Count + 1);
    FChallenges[Found].Domain := Domain;
  end;
  FChallenges[Found].Token := Token;
  FThumbprint := Thumbprint;

// is the Challenge ready ?
// False by default, don't set it True until the HTTP server can reply to the Challenge !
// This event is fired at each call to FinalizeDomain until you set Processed to True
// if you set Processed to True while the HTTP server is not ready the Order will go to the Invalid state.
  Processed := idHTTPServer1.Active;

  btFinalize.Enabled := True;
end;

// idHTTP server response to the well known ACME challenge
// all the subdomains are supposed to be handled by this application !!!
procedure TForm1.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Index: Integer;
begin
  mmHTTP.Lines.Add(ARequestInfo.URI);
  for Index := 0 to Length(FChallenges) - 1 do
begin
    if ARequestInfo.URI = TExecuteACME.WELL_KNOWN_URL + FChallenges[Index].Token then
  begin
      AResponseInfo.ContentType := 'application/octet-stream';
      AResponseInfo.ContentText := FChallenges[Index].Token + '.' + FThumbprint;
      mmHTTP.Lines.Add('Challenge for ' + FChallenges[Index].Domain + ' : ' + FChallenges[Index].Token + '.' + FThumbprint);
  end;
end;

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

procedure TForm1.cbHTTPChallengesClick(Sender: TObject);
begin
  idHTTPServer1.Active := cbHTTPChallenges.Checked;
end;

procedure TForm1.ExecuteACME1Error(Sender: TObject; const Error: string);
begin
  mmHTTP.Lines.Add(Error);
  MessageDlg(Error, mtWarning, [mbOK], 0);
end;

procedure TForm1.ExecuteACME1Done(Sender: TObject);
begin
  case ExecuteACME1.OrderStatus of
    osNone   : ShowMessage('Request done');
    osPending: ShowMessage('Call FinalizeDomain when the Challenges are processed');
    osReady  : ShowMessage('Call FinalizeDomain to finalize the Order');
    osValid  : ShowMessage('Call FinalizeDomain to get the Certificat');
    osInvalid: ShowMessage('The challenge failed, call FinalizeDomain to get the error or RegisterDomain to start a new Challenge');
    osExpired: ShowMessage('The certificat has expired, you should call RegisterDomain to renew it');
    osRevoked: ShowMessage('The certificat is revoked');
    osUnknown: ShowMessage('Unknow error, check OrderURL (' + ExecuteACME1.OrderURL + ')');
  end;
end;


end.
