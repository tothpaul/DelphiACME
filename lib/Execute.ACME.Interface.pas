unit Execute.ACME;

{

  ACME Delphi client for Let's Encrypt (c)2018-2021 Execute SARL <contact@execute.fr>

  This component is NOT FREE !

  BUT you can use it any OpenSource project under the GPL

  AND you have to register a licence to use it in any commercial product

  you are NOT allowed to use this component to register a commercial website
  without a registered licence, even with a GPL product around this component

}

{
  Purpose of use: add a Let's Encrypt certificat to a standalone Delphi WebBroker application


  Put a TExecuteACME component on a form

  Set or generate the AccountKey and DomainKey RSA Private Keys

  Define DomaineName
  Define ContactEmail (optional)
  Define OnHttpChallenge
  Define OnCertificate

  call TExecuteACME.RegisterDomain();

  -> OnHttpChallenge (this is NOT an HTTPS request)
    on this request :
      http://<DomainName>/.well-known/acme-client/<Token>
    you have to reply :
      <Token>.<Thumbprint>
    this can be done by creating the file on an external webserver or by an idHTTP component

  -> OnCertificate
    you have a register certificat in the provided TStrings parameter
}

{
 New in version 1.1  (2019-02-18)

   - Add a FinalizeDomain method to check the request status
     if you need to restart the application, save the OrderURL properties of the component

   - Add SubjectAltNames(SAN) property to register multiples domains
     DomainName = 'www.mydomain.com'
     SubjectAlternativeNames = ['ftp.mydomain.com']
}

{
  version 1.2 (2019-12-10)

    - POST-as-GET support
}

{
  version 1.3 (2020-04-08)

   - extract local types from Record because of bad support by the IDE
   - Linux64 support
   - add *Now method for direct call of methods (no thread)
}

{
  version 1.4 (2021-08-23)

    - switch from Indy TidHTTP to System.Net.HTTPClient for TLS 1.3 support
    - version tested only with Delphi 10.4.2
}

interface
{$ZEROBASEDSTRINGS OFF}

{.$DEFINE INDY} // INDY still don't support TLS 1.3 !

{$IFDEF DEBUG}
{.$DEFINE LOG}
{$ENDIF}
uses
{$IFDEF LOG}
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Hash,
  idHTTP,
  IdSSLOpenSSL,
  IdSSLOpenSSLHeaders,
{$IFDEF LINUX64}
  IdGlobal,
{$ENDIF}
{$IFNDEF INDY}
  System.Net.HttpClient,
{$ENDIF}
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
  THttpChallengeEvent = procedure(Sender: TObject; const Domain, Token, Thumbprint: string; var Processed: Boolean) of object;
  TCertificateEvent = procedure(Sender: TObject; Certificate: TStrings) of object;
  TErrorEvent = procedure(Sender: TObject; const Error: string) of object;

  TDomainRegistrationThread = class
  end;
  
  TSubmitThread = class(TThread)
  end;

  TACMEOrderStatus = (
    osNone,
    osPending,
    osReady,
    osValid,
    osInvalid,
    osExpired,
    osRevoked,
    osUnknown
  );

  TExecuteACME = class(TComponent)
  public const
    /// <summary>
    /// Path of a HTTP Challenge
    /// </summary>
    WELL_KNOWN_URL = '/.well-known/acme-challenge/';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///  same as RegisterDomain or RegisterDomainNow
    /// </summary>
    function DoRegisterDomain(Submit: Boolean): TDomainRegistrationThread;
    /// <summary>
    ///  same as FinalizeDomain or FinalizeDomainNow
    /// </summary>
    function DoFinalizeDomain(Submit: Boolean): TDomainRegistrationThread;
    /// <summary>
    ///  same as UnregisterDomain or UnregisterDomainNow
    /// </summary>
    function DoUnregisterDomain(const CRT: string; Reason: TACMERevokeReason; Submit: Boolean): TDomainRegistrationThread;
    /// <summary>
    /// Request a new registration - in a thread
    /// </summary>
    function RegisterDomain: TDomainRegistrationThread; inline;
    /// <summary>
    /// Request a new registration - direct call
    /// </summary>
    procedure RegisterDomainNow; inline;
    /// <summary>
    /// Check last registration status - in a thread
    /// </summary>
    function FinalizeDomain: TDomainRegistrationThread; inline;
    /// <summary>
    /// Check last registration status - direct call
    /// </summary>
    procedure FinalizeDomainNow; inline;
    /// <summary>
    /// Unregister a Certificat - in a thread
    /// </summary>
    function UnregisterDomain(const CRT: string; Reason: TACMERevokeReason): TDomainRegistrationThread; inline;
    /// <summary>
    /// Unregister a Certificat - direct call
    /// </summary>
    procedure UnregisterDomainNow(const CRT: string; Reason: TACMERevokeReason); inline;
    /// <summary>
    /// Generate a RSA Key (for Domain or Account for instance)
    /// </summary>
    class procedure GeneraRSAKey(Strings: TStrings; const Password: string = ''; KeyLength: Integer = 4096);
    /// <summary>
    /// The URL of the Order request
    /// </summary>
    property OrderURL: string read FOrderURL write FOrderURL;
    /// <summary>
    /// The Status of the Order request
    /// </summary>
    property OrderStatus: TACMEOrderStatus read FOrderStatus;
  published
    /// <summary>
    /// StagingV2 for testing, ProductionV2 for production
    /// </summary>
    property Environment: TEnvironment read FEnvironment write SetEnvironment default StagingV2;
    /// <summary>
    /// automaticaly set by Environment
    /// </summary>
    property Directory: string read FDirectory write SetDirectory stored IsCustomEnvironment;
    /// <summary>
    /// the DomainName to register
    /// </summary>
    property DomainName: string read FDomainName write FDomainName;
    /// <summary>
    /// alternate domains
    /// </summary>
    property SubjectAltNames: TStrings index SUBJECTALTNAMES_INDEX read GetStrings write SetStrings;
    /// <summary>
    /// optional contact email
    /// </summary>
    property ContactEmail: string read FContactEMail write FContactEmail;
    /// <summary>
    /// a RSA Key for the Let's Encrypt account, can be generated with GeneraRSAKey()
    /// </summary>
    property AccountKey: TStrings index ACCOUNTKEY_INDEX read GetStrings write SetStrings;
    /// <summary>
    /// a RSA Key for the domain, can be generated with GeneraRSAKey()
    /// </summary>
    property DomainKey: TStrings index DOMAINKEY_INDEX read GetStrings write SetStrings;
    /// <summary>
    /// fired by the component when a Password is required
    /// </summary>
    property OnPassword: TPasswordEvent read FOnPassword write FOnPassword;
    /// <summary>
    /// fired by the component when a pending (Processed = False) HttpChallenge is found
    /// </summary>
    property OnHttpChallenge: THttpChallengeEvent read FOnHttpChallenge write FOnHttpChallenge;
    /// <summary>
    /// fired when the requested Certificat is available
    /// </summary>
    property OnCertificate: TCertificateEvent read FOnCertificate write FOnCertificate;
    /// <summary>
    /// fired when a error occurs
    /// </summary>
    property OnError: TErrorEvent read FOnError write FOnError;
    /// <summary>
    /// fired when the request is done without any other event, you can check OrderStatus
    /// </summary>
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
  end;

implementation

{$WARNING ! Source code available on https://store.execute.fr ! }

{$IFDEF MSWINDOWS}
initialization
{$IFDEF LOG}AllocConsole;{$ENDIF}
{$ENDIF}
end.
