unit Execute.LetsEncrypt;
{

  ACME Delphi client for Let's Encrypt (c)2018 Execute SARL <contact@execute.fr>

  This component is NOT FREE !

  BUT you can use it any OpenSource project under the GPL

  AND you have to register a licence to use it in any commercial product

  you are NOT allowed to use this component to register a commercial website
  without a registered licence, even with a GPL product around this component

}

{
  Purpose of use: add a Let's Encrypt certificat to a standalone Delphi WebBroker application


  Put a TLetsEncryt component on a form

  Set or generate the AccountKey and DomainKey RSA Private Keys

  Define DomaineName
  Define ContactEmail (optional)
  Define OnHttpChallenge
  Define OnCertificate

  call TLetsEncrypt.RegisterDomain();

  -> OnHttpChallenge (this is NOT an HTTPS request)
    on this request :
      http://<DomainName>/.well-known/acme-client/<Token>
    you have to reply :
      <Token>.<Thumbprint>
    this can be done by creating the file on an external webserver or by an idHTTP component

  -> OnCertificate
    you have a register certificat in the provided TStrings parameter
}

interface
{$IFDEF DEBUG}
{--$DEFINE LOG}
{$ENDIF}
uses
{$IFDEF LOG}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Hash,
  idHTTP,
  IdSSLOpenSSL,
  IdSSLOpenSSLHeaders,
  Execute.RTTI,
  Execute.JSON;

type
  TEnvironment = (
//    Staging,     - deprecated - non supported
//    Production,  - deprecated - non supported
    StagingV2,     // for testing
    ProductionV2,  // for production
    Custom
  );

  TKeyType = (
    ktAccount,
    ktDomain
  );

  TACMERevokeReason = (
    unspecified = 0,
    keyCompromise = 1,
    cACompromise = 2,
    affiliationChanged = 3,
    superseded = 4,
    cessationOfOperation = 5,
    certificateHold = 6,
//    _not_used_ = 7,
    removeFromCRL = 8,
    privilegeWithdrawn = 9,
    aACompromise = 10
  );

  TPasswordEvent = function(Sender: TObject; KeyType: TKeyType; var Password: string): Boolean of object;
  THttpChallengeEvent = procedure(Sender: TObject; const Domain, Token, Thumbprint: string) of object;
  TCertificateEvent = procedure(Sender: TObject; Certificate: TStrings) of object;
  TErrorEvent = procedure(Sender: TObject; const Error: string) of object;

  TDomainRegistrationThread = class(TThread)
  end;

  TLetsEncrypt = class(TComponent)
  private
    FEnvironment    : TEnvironment;
    FDirectory      : string;
    FDomainName     : string;
    FContactEMail   : string;
    FAccountKey     : TStrings;
    FDomainKey      : TStrings;
    FOnPassword     : TPasswordEvent;
    FOnHttpChallenge: THttpChallengeEvent;
    FOnCertificate  : TCertificateEvent;
    FOnError        : TErrorEvent;
    FOnDone         : TNotifyEvent;
    procedure SetEnvironment(Value: TEnvironment);
    procedure SetDirectory(const Value: string);
    function IsCustomEnvironment: Boolean;
    function GetKey(Index: TKeyType): TStrings;
    procedure SetKey(Index: TKeyType; Value: TStrings);
    procedure Error(const msg: string);
    function LoadRSA(AKeyType: TKeyType): pRSA;
    procedure GetPublicKey(rsa: pRSA; var n, e: TBytes);
    function GetSigning(KeyType: TKeyType; var n, e: TBytes): pEVP_PKEY;
    function GetRequest(var n, e: TBytes): TBytes;
    procedure CheckProperties;
    function StrToCertificate(const AStr: string; var Domain: string): TBytes;
  public const
    WELL_KNOWN_URL = '/.well-known/acme-challenge/';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RegisterDomain: TDomainRegistrationThread;
    function UnregisterDomain(const CRT: string; Reason: TACMERevokeReason): TDomainRegistrationThread;
    class procedure GeneraRSAKey(Strings: TStrings; const Password: string = ''; KeyLength: Integer = 4096);
  published
    property Environment: TEnvironment read FEnvironment write SetEnvironment default StagingV2;
    property Directory: string read FDirectory write SetDirectory stored IsCustomEnvironment;
    property DomainName: string read FDomainName write FDomainName;
    property ContactEmail: string read FContactEMail write FContactEmail;
    property AccountKey: TStrings index ktAccount read GetKey write SetKey;
    property DomainKey: TStrings index ktDomain read GetKey write SetKey;
    property OnPassword: TPasswordEvent read FOnPassword write FOnPassword;
    property OnHttpChallenge: THttpChallengeEvent read FOnHttpChallenge write FOnHttpChallenge;
    property OnCertificate: TCertificateEvent read FOnCertificate write FOnCertificate;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
  end;

implementation

{$WARNING ! SOURCE CODE NOT AVAILABLE ! }

initialization
{$IFDEF LOG}AllocConsole;{$ENDIF}
end.
