unit ACMEDemo.DNSChallenge;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TDNSChallenge = class(TForm)
    Label1: TLabel;
    edEntry: TEdit;
    Label2: TLabel;
    edValue: TEdit;
    Label3: TLabel;
    edDNS: TEdit;
    Label4: TLabel;
    Button1: TButton;
    Button2: TButton;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    class function Execute(Sender: TComponent; const Domain, Digest: string): Boolean;
  end;

var
  DNSChallenge: TDNSChallenge;

implementation

uses
  Execute.ACME;

{$R *.dfm}

{ TDNSChallenge }

class function TDNSChallenge.Execute(Sender: TComponent; const Domain,
  Digest: string): Boolean;
begin
  DNSChallenge := TDNSChallenge.Create(Sender);
  try
    var zoneName := Domain;
    var subDomain := '';
    var dots := 0;
    for var I := Length(zoneName) downto 1 do
    begin
      if zoneName[I] = '.' then
      begin
        Inc(dots);
        if dots = 2 then
        begin
          subDomain := Copy(zoneName, 1, I - 1);
          Delete(zoneName, 1, I);
          Break;
        end;
      end;
    end;
    if subDomain = '' then
      subDomain := TExecuteACME.DNS_PREFIX
    else begin
      if subDomain.StartsWith('*.') then
        Delete(subDomain, 1, 2);
      subDomain := TExecuteACME.DNS_PREFIX + '.' + subDomain;
    end;
    DNSChallenge.edEntry.Text := subDomain;
    DNSChallenge.edValue.Text := Digest;
    DNSChallenge.edDNS.Text := subDomain + ' IN TXT "' + Digest + '"';
    Result := DNSChallenge.ShowModal = mrOK;
  finally
    DNSChallenge.Free;
  end;
end;

end.
