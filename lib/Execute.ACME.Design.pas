unit Execute.ACME.Design;
{
 (c)2018-2020 Execute SARL <contact@execute.fr>
}
interface

uses
  DesignEditors,
  DesignIntf,
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  Execute.ACME;

type
  TExecuteACMEEditor = class(TComponentEditor)
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
  RegisterComponents('Execute', [TExecuteACME]);

  RegisterComponentEditor(TExecuteACME, TExecuteACMEEditor);
end;

{ TExecuteACMEEditor }

procedure TExecuteACMEEditor.GenerateKey(Key: TStrings; const Name: string);
var
  Str: string;
begin
  Str := '';
  if not InputQuery('Password for the ' + Name, 'Define the Private Key Password', Str) then
    Exit;
  try
    TExecuteACME.GeneraRSAKey(Key, Str);
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

procedure TExecuteACMEEditor.ExecuteVerb(Index: Integer);
var
  LE: TExecuteACME;
begin
  LE := GetComponent as TExecuteACME;
  case Index of
    0: GenerateKey(LE.AccountKey, 'Account');
    1: GenerateKey(LE.DomainKey, 'Domain');
  end;
end;

function TExecuteACMEEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Generate an Account Private Key';
    1: Result := 'Generate a Domain Private Key';
  end;
end;

function TExecuteACMEEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
