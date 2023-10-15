object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TExecuteACME Demo (c)2018-2023 Execute SARL'
  ClientHeight = 672
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    588
    672)
  TextHeight = 13
  object Label2: TLabel
    Left = 10
    Top = 38
    Width = 38
    Height = 13
    Caption = 'Contact'
  end
  object Label3: TLabel
    Left = 10
    Top = 69
    Width = 47
    Height = 13
    Caption = 'Alt names'
  end
  object Label1: TLabel
    Left = 10
    Top = 11
    Width = 70
    Height = 13
    Caption = 'Domaine name'
  end
  object Memo1: TMemo
    Left = 8
    Top = 127
    Width = 568
    Height = 415
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'This application let you create a Certificat for an HTTPS server'
      ''
      '1) PrivateKeys'
      '  you need an Account (account.key) and Domain key (domain.key)'
      '  you can generate them with "openssl genrsa 4096 > filename"'
      '  or let the application create them with OpenSSL API'
      ''
      '2) Domain Names'
      
        '  the purpose of Let'#39's Encrypt is to create a certificate for a ' +
        'domain name'
      '  this application have to be reachabled at this URL:'
      '  http://<DomainName>/'
      '  this is required for the "HTTP Challenge" of Let'#39's Encrypt'
      '  You can use multiple sub-domains in one request like:'
      '   www.mydomain.com'
      '   ftp.mydomain.com'
      '   smtp.mydomain.com'
      ''
      '3) Contact'
      '  the contact email is optional'
      ''
      '4) Staging vs Production'
      '  for testing, use Staging, for production use Production'
      ''
      'to create a certificate, click on the Register button'
      
        'once the HTTP Challenge is peformed, you can Finalize the reques' +
        't'
      'to get the Certificat.'
      ''
      'you can revoke (Unregister) a certificate loaded in this memo.'
      ''
      
        'feedbacks are welcome (in english or french) at contact@execute.' +
        'fr')
    ScrollBars = ssBoth
    TabOrder = 9
  end
  object btRegister: TButton
    Left = 501
    Top = 8
    Width = 75
    Height = 25
    Hint = 'Request a certificate for the specified domain'
    Anchors = [akTop, akRight]
    Caption = 'Register'
    TabOrder = 4
    OnClick = btRegisterClick
  end
  object edContact: TEdit
    Left = 87
    Top = 36
    Width = 245
    Height = 21
    Hint = 'An optional contact email'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    TextHint = 'contact@example.com'
  end
  object btSave: TButton
    Left = 420
    Top = 37
    Width = 75
    Height = 25
    Hint = 'Save the certificate (the content of the memo)'
    Anchors = [akTop, akRight]
    Caption = 'Save...'
    Enabled = False
    TabOrder = 8
    OnClick = btSaveClick
  end
  object cbMode: TComboBox
    Left = 341
    Top = 9
    Width = 154
    Height = 22
    Hint = 'Use staging for your test !'
    Style = csOwnerDrawFixed
    Anchors = [akTop, akRight]
    ItemIndex = 0
    TabOrder = 3
    Text = 'Staging'
    Items.Strings = (
      'Staging'
      'Production')
  end
  object btUnregister: TButton
    Left = 501
    Top = 65
    Width = 75
    Height = 25
    Hint = 'Revoke the certificate displayed in the memo'
    Anchors = [akTop, akRight]
    Caption = 'Unregister'
    TabOrder = 6
    OnClick = btRegisterClick
  end
  object btLoad: TButton
    Left = 340
    Top = 37
    Width = 75
    Height = 25
    Hint = 'Load an existing certificate, so you can revoke it'
    Anchors = [akTop, akRight]
    Caption = 'Load...'
    TabOrder = 7
    OnClick = btLoadClick
  end
  object mmSubjectAltNames: TMemo
    Left = 87
    Top = 68
    Width = 251
    Height = 53
    Hint = 'Subject Alternative names (SAN) like ftp.exemple.com'
    TabOrder = 2
  end
  object mmHTTP: TMemo
    Left = 8
    Top = 548
    Width = 568
    Height = 116
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'mmHTTP')
    ScrollBars = ssBoth
    TabOrder = 10
  end
  object btFinalize: TButton
    Left = 501
    Top = 37
    Width = 75
    Height = 25
    Hint = 'Request a certificate for the specified domain'
    Anchors = [akTop, akRight]
    Caption = 'Finalize'
    Enabled = False
    TabOrder = 5
    OnClick = btRegisterClick
  end
  object edDomainName: TEdit
    Left = 87
    Top = 9
    Width = 245
    Height = 21
    Hint = 'Main domain name'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'www.exemple.com'
  end
  object cbHTTPChallenges: TCheckBox
    Left = 344
    Top = 80
    Width = 144
    Height = 17
    Caption = 'Process HTTP Challenges'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = cbHTTPChallengesClick
  end
  object cbDNSChallenges: TCheckBox
    Left = 344
    Top = 103
    Width = 144
    Height = 17
    Caption = 'Process DNS Challenges'
    Checked = True
    State = cbChecked
    TabOrder = 12
    OnClick = cbDNSChallengesClick
  end
  object ExecuteACME1: TExecuteACME
    OnPassword = ExecuteACME1Password
    OnHttpChallenge = ExecuteACME1HttpChallenge
    OnDnsChallenge = ExecuteACME1DnsChallenge
    OnCertificate = ExecuteACME1Certificate
    OnError = ExecuteACME1Error
    OnDone = ExecuteACME1Done
    Left = 400
    Top = 168
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 400
    Top = 232
  end
end
