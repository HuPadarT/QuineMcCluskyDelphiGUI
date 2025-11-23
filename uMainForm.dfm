object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Quine-McCluskey'
  ClientHeight = 390
  ClientWidth = 485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    485
    390)
  TextHeight = 13
  object lblResult: TLabel
    Left = 8
    Top = 347
    Width = 52
    Height = 13
    Anchors = [akLeft, akBottom]
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
      Top = 61
      Width = 91
      Height = 13
      Caption = 'K'#246'z'#246'mb'#246's komb'#243'k:'
      Visible = False
    end
    object lblVars: TLabel
      Left = 8
      Top = 33
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
      Top = 57
      Width = 280
      Height = 21
      TabOrder = 1
      Visible = False
      OnChange = edtDontCaresChange
    end
    object edtVars: TEdit
      Left = 105
      Top = 30
      Width = 55
      Height = 21
      TabOrder = 2
      Text = '4'
      OnChange = edtVarsChange
    end
    object btnRun: TButton
      Left = 400
      Top = 30
      Width = 80
      Height = 25
      Caption = 'Start'
      TabOrder = 3
      OnClick = btnRunClick
    end
  end
  object memSteps: TMemo
    Left = 8
    Top = 84
    Width = 472
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Itt lesz l'#225'that'#243' a folyamat')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object edtResult: TEdit
    Left = 66
    Top = 344
    Width = 411
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    TabOrder = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 371
    Width = 485
    Height = 19
    Panels = <
      item
        Text = '  K'#233'sz'#237'tette P'#225'd'#225'r Tibor 2025'
        Width = 50
      end>
  end
end
