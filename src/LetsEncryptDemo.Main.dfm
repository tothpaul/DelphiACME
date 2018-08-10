object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Let'#39's Encrypt (c)2018 Execute SARL'
  ClientHeight = 482
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    556
    482)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 22
    Width = 64
    Height = 13
    Caption = 'Domain name'
  end
  object Label2: TLabel
    Left = 8
    Top = 49
    Width = 38
    Height = 13
    Caption = 'Contact'
  end
  object Memo1: TMemo
    Left = 8
    Top = 80
    Width = 540
    Height = 394
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'This application let you create a Certificat for an HTTPS server'
      ''
      '1) PrivateKeys'
      '  you need an Account (account.key) and Domain key (domain.key)'
      '  you can generate them with "openssl genrsa 4096 > filename"'
      '  or let the application create them with OpenSSL API '
      ''
      '2) Domain Name'
      
        ' the purpose of Let'#39's Encrypt is to create a certificate for a d' +
        'omain name'
      '  this application have to be reachabled at this URL:'
      '  http://<DomainName>/'
      '  this is required for the "HTTP Challenge" of Let'#39's Encrypt'
      ''
      '3) Contact'
      '  the contact email is optional'
      ''
      '4) Staging vs Production'
      '  for testing, use Staging, for production use Production'
      ''
      'to create a certificate, click on the Register button'
      'the first request will probably not give you the certificate'
      'the HTTP Challenge need to be performed first'
      'the second request will give you the certificate'
      ''
      'you can revoke (Unregister) a certificate loaded in this memo.'
      ''
      
        'feedbacks are welcome (in english of french) at contact@execute.' +
        'fr')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btRegister: TButton
    Left = 473
    Top = 18
    Width = 75
    Height = 25
    Hint = 'Request a certificate for the specified domain'
    Anchors = [akTop, akRight]
    Caption = 'Register'
    TabOrder = 1
    OnClick = btRegisterClick
  end
  object edDomain: TEdit
    Left = 88
    Top = 19
    Width = 217
    Height = 21
    Hint = 
      'The domain where this application is reachable and for wich you ' +
      'want et Let'#39's Encrypt certificate'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    TextHint = 'www.example.com'
  end
  object edContact: TEdit
    Left = 88
    Top = 46
    Width = 217
    Height = 21
    Hint = 'An optional contact email'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    TextHint = 'contact@example.com'
  end
  object btSave: TButton
    Left = 392
    Top = 49
    Width = 75
    Height = 25
    Hint = 'Save the certificate (the content of the memo)'
    Anchors = [akTop, akRight]
    Caption = 'Save...'
    Enabled = False
    TabOrder = 4
    OnClick = btSaveClick
  end
  object cbMode: TComboBox
    Left = 313
    Top = 19
    Width = 154
    Height = 22
    Hint = 'Use staging for your test !'
    Style = csOwnerDrawFixed
    Anchors = [akTop, akRight]
    ItemIndex = 0
    TabOrder = 5
    Text = 'Staging'
    Items.Strings = (
      'Staging'
      'Production')
  end
  object btUnregister: TButton
    Left = 473
    Top = 49
    Width = 75
    Height = 25
    Hint = 'Revoke the certificate displayed in the memo'
    Anchors = [akTop, akRight]
    Caption = 'Unregister'
    TabOrder = 6
    OnClick = btRegisterClick
  end
  object btLoad: TButton
    Left = 311
    Top = 49
    Width = 75
    Height = 25
    Hint = 'Load an existing certificate, so you can revoke it'
    Anchors = [akTop, akRight]
    Caption = 'Load...'
    TabOrder = 7
    OnClick = btLoadClick
  end
  object LetsEncrypt1: TLetsEncrypt
    OnPassword = LetsEncrypt1Password
    OnHttpChallenge = LetsEncrypt1HttpChallenge
    OnCertificate = LetsEncrypt1Certificate
    OnError = LetsEncrypt1Error
    OnDone = LetsEncrypt1Done
    Left = 424
    Top = 96
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 424
    Top = 152
  end
end
