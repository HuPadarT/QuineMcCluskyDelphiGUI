object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Quine-McCluskey'
  ClientHeight = 390
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    529
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
    Width = 529
    Height = 96
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 485
    object lblMinterms: TLabel
      Left = 8
      Top = 8
      Width = 53
      Height = 13
      Caption = 'Mintermek:'
      FocusControl = edtMinterms
    end
    object lblDontCares: TLabel
      Left = 8
      Top = 61
      Width = 91
      Height = 13
      Caption = 'K'#246'z'#246'mb'#246's komb'#243'k:'
      FocusControl = edtDontCares
      Visible = False
    end
    object lblVars: TLabel
      Left = 8
      Top = 33
      Width = 77
      Height = 13
      Caption = 'V'#225'ltoz'#243'k sz'#225'ma:'
      FocusControl = edtVars
    end
    object lblAndSign: TLabel
      Left = 176
      Top = 34
      Width = 30
      Height = 13
      Caption = #201'S jel:'
      FocusControl = edtAndSign
    end
    object lblOrSign: TLabel
      Left = 258
      Top = 33
      Width = 44
      Height = 13
      Caption = 'VAGY jel:'
      FocusControl = edtOrSign
    end
    object edtMinterms: TEdit
      Left = 105
      Top = 3
      Width = 416
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
      Left = 441
      Top = 30
      Width = 80
      Height = 25
      Caption = 'Start'
      TabOrder = 3
      OnClick = btnRunClick
    end
    object edtAndSign: TEdit
      Left = 212
      Top = 30
      Width = 21
      Height = 21
      TabOrder = 4
      Text = '*'
    end
    object edtOrSign: TEdit
      Left = 308
      Top = 30
      Width = 21
      Height = 21
      TabOrder = 5
      Text = '+'
    end
  end
  object memSteps: TMemo
    Left = 8
    Top = 84
    Width = 516
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Itt lesz l'#225'that'#243' a folyamat')
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 472
    ExplicitHeight = 252
  end
  object edtResult: TEdit
    Left = 66
    Top = 344
    Width = 455
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    TabOrder = 2
    ExplicitTop = 342
    ExplicitWidth = 411
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 371
    Width = 529
    Height = 19
    Panels = <
      item
        Text = '  K'#233'sz'#237'tette P'#225'd'#225'r Tibor 2025'
        Width = 50
      end>
    ExplicitTop = 369
    ExplicitWidth = 485
  end
end
