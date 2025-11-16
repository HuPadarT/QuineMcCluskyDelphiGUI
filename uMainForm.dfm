object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Quine-McCluskey'
  ClientHeight = 380
  ClientWidth = 485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object lblResult: TLabel
    Left = 8
    Top = 328
    Width = 52
    Height = 13
    Caption = 'Eredm'#233'ny:'
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 485
    Height = 96
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object lblMinterms: TLabel
      Left = 8
      Top = 8
      Width = 53
      Height = 13
      Caption = 'Mintermek:'
    end
    object lblDontCares: TLabel
      Left = 8
      Top = 32
      Width = 91
      Height = 13
      Caption = 'K'#246'z'#246'mb'#246's komb'#243'k:'
    end
    object lblVars: TLabel
      Left = 8
      Top = 56
      Width = 47
      Height = 13
      Caption = 'Variables:'
    end
    object edtMinterms: TEdit
      Left = 105
      Top = 4
      Width = 375
      Height = 21
      TabOrder = 0
      OnChange = edtMintermsChange
    end
    object edtDontCares: TEdit
      Left = 105
      Top = 28
      Width = 375
      Height = 21
      TabOrder = 1
      OnChange = edtDontCaresChange
    end
    object edtVars: TEdit
      Left = 105
      Top = 52
      Width = 55
      Height = 21
      TabOrder = 2
      Text = '4'
      OnChange = edtVarsChange
    end
    object btnRun: TButton
      Left = 400
      Top = 55
      Width = 80
      Height = 25
      Caption = 'Start'
      TabOrder = 3
      OnClick = btnRunClick
    end
  end
  object memSteps: TMemo
    Left = 8
    Top = 104
    Width = 472
    Height = 216
    Lines.Strings = (
      'Itt lesz l'#225'that'#243' a folyamat')
    TabOrder = 1
  end
  object edtResult: TEdit
    Left = 96
    Top = 324
    Width = 384
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
end
