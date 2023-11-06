object DNSChallenge: TDNSChallenge
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DNS Challenge'
  ClientHeight = 248
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  DesignSize = (
    529
    248)
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 240
    Height = 15
    Caption = 'Sorry, you have to set the DNS entry yourself !'
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 53
    Height = 15
    Caption = 'DSN Entry'
  end
  object Label3: TLabel
    Left = 16
    Top = 96
    Width = 28
    Height = 15
    Caption = 'Value'
  end
  object Label4: TLabel
    Left = 16
    Top = 133
    Width = 189
    Height = 15
    Caption = 'The DNS entry should look like this :'
  end
  object edEntry: TEdit
    Left = 112
    Top = 53
    Width = 394
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object edValue: TEdit
    Left = 112
    Top = 93
    Width = 394
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edDNS: TEdit
    Left = 16
    Top = 154
    Width = 490
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object Button1: TButton
    Left = 153
    Top = 192
    Width = 97
    Height = 39
    Caption = 'Done'
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 289
    Top = 192
    Width = 97
    Height = 39
    Caption = 'Not yet'
    ModalResult = 2
    TabOrder = 4
  end
end
