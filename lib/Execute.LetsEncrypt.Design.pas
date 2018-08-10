unit Execute.LetsEncrypt.Design;
{
 (c)2018 Execute SARL <contact@execute.fr>
}
interface

uses
  DesignEditors,
  DesignIntf,
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  Execute.LetsEncrypt;

type
  TLetsEncryptEditor = class(TComponentEditor)
  private
    procedure GenerateKey(Key: TStrings; const Name: string);
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Execute', [TLetsEncrypt]);

  RegisterComponentEditor(TLetsEncrypt, TLetsEncryptEditor);
end;

{ TLetsEncryptEditor }

procedure TLetsEncryptEditor.GenerateKey(Key: TStrings; const Name: string);
var
  Str: string;
begin
  Str := '';
  if not InputQuery('Password for the ' + Name, 'Define the Private Key Password', Str) then
    Exit;
  try
    TLetsEncrypt.GeneraRSAKey(Key, Str);
    Str := Name + '.key';
    if PromptForFileName(
      Str,
      'Key files (*.key)|*.key|All files (*.*)|*.*',
      'KEY',
      'Save the Private Key in a safe place !',
      '',
      True
    )
    then
      Key.SaveToFile(Str);
  except
    on e: Exception do
      ShowMessage(e.Message);
  end;
end;

procedure TLetsEncryptEditor.ExecuteVerb(Index: Integer);
var
  LE: TLetsEncrypt;
begin
  LE := GetComponent as TLetsEncrypt;
  case Index of
    0: GenerateKey(LE.AccountKey, 'Account');
    1: GenerateKey(LE.DomainKey, 'Domain');
  end;
end;

function TLetsEncryptEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Generate an Account Private Key';
    1: Result := 'Generate a Domain Private Key';
  end;
end;

function TLetsEncryptEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
