object Form1: TForm1
  Left = 192
  Top = 107
  Width = 377
  Height = 338
  Caption = 'Functionspotter'
  Color = clNavy
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 16
    Top = 16
    Width = 106
    Height = 137
    Alignment = taCenter
    AutoSize = False
    Caption = 'Ausgabefenster Gr'#246#223'e'
    Color = clSkyBlue
    ParentColor = False
  end
  object Label6: TLabel
    Left = 136
    Top = 16
    Width = 137
    Height = 137
    Alignment = taCenter
    AutoSize = False
    Caption = 'X und Y Max und Min'
    Color = clSkyBlue
    ParentColor = False
  end
  object Label1: TLabel
    Left = 136
    Top = 72
    Width = 41
    Height = 17
    AutoSize = False
    Caption = 'Xmin'
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 184
    Top = 112
    Width = 41
    Height = 17
    AutoSize = False
    Caption = 'Ymin'
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label3: TLabel
    Left = 232
    Top = 72
    Width = 41
    Height = 17
    AutoSize = False
    Caption = 'Xmax'
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label4: TLabel
    Left = 184
    Top = 40
    Width = 41
    Height = 17
    AutoSize = False
    Caption = 'Ymax'
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 16
    Top = 112
    Width = 65
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Schrittweite'
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label8: TLabel
    Left = 16
    Top = 48
    Width = 33
    Height = 13
    AutoSize = False
    Caption = 'X Form'
    Color = clSkyBlue
    ParentColor = False
  end
  object Label9: TLabel
    Left = 16
    Top = 80
    Width = 33
    Height = 17
    AutoSize = False
    Caption = 'Y Form'
    Color = clSkyBlue
    ParentColor = False
  end
  object Label10: TLabel
    Left = 16
    Top = 168
    Width = 41
    Height = 25
    AutoSize = False
    Caption = ' Y1='
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label11: TLabel
    Left = 16
    Top = 200
    Width = 41
    Height = 25
    AutoSize = False
    Caption = ' Y2='
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label12: TLabel
    Left = 16
    Top = 232
    Width = 41
    Height = 25
    AutoSize = False
    Caption = ' Y3='
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label13: TLabel
    Left = 288
    Top = 16
    Width = 65
    Height = 57
    AutoSize = False
    Caption = 'Hintergrund'
    Color = clSkyBlue
    ParentColor = False
  end
  object Label14: TLabel
    Left = 288
    Top = 96
    Width = 65
    Height = 57
    AutoSize = False
    Caption = 'Vordergrund'
    Color = clSkyBlue
    ParentColor = False
  end
  object Edit1: TEdit
    Left = 136
    Top = 88
    Width = 41
    Height = 21
    AutoSize = False
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '-10'
  end
  object Edit2: TEdit
    Left = 184
    Top = 128
    Width = 41
    Height = 21
    AutoSize = False
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '-10'
  end
  object Edit3: TEdit
    Left = 232
    Top = 88
    Width = 41
    Height = 21
    AutoSize = False
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '10'
  end
  object Edit4: TEdit
    Left = 184
    Top = 56
    Width = 41
    Height = 21
    AutoSize = False
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '10'
  end
  object Edit5: TEdit
    Left = 56
    Top = 128
    Width = 65
    Height = 21
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    Text = '0,01'
  end
  object Button1: TButton
    Left = 16
    Top = 272
    Width = 105
    Height = 25
    Caption = 'Starten'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Edit6: TEdit
    Left = 56
    Top = 48
    Width = 65
    Height = 21
    Color = clSkyBlue
    TabOrder = 6
    Text = '400'
  end
  object Edit7: TEdit
    Left = 56
    Top = 80
    Width = 65
    Height = 21
    Color = clSkyBlue
    TabOrder = 7
    Text = '400'
  end
  object Edit8: TEdit
    Left = 64
    Top = 168
    Width = 225
    Height = 25
    Color = clSkyBlue
    TabOrder = 8
    Text = 'x^2'
    OnChange = Edit8Change
  end
  object Edit9: TEdit
    Left = 64
    Top = 200
    Width = 225
    Height = 25
    Color = clSkyBlue
    TabOrder = 9
    Text = 'x^3'
    OnChange = Edit9Change
  end
  object Panel1: TPanel
    Left = 296
    Top = 168
    Width = 57
    Height = 25
    Caption = 'Color'
    Color = clRed
    TabOrder = 10
    OnClick = Panel1Click
  end
  object Panel2: TPanel
    Left = 296
    Top = 200
    Width = 57
    Height = 25
    Caption = 'Color'
    Color = clLime
    TabOrder = 11
    OnClick = Panel2Click
  end
  object Button2: TButton
    Left = 248
    Top = 272
    Width = 105
    Height = 25
    Caption = 'Beenden'
    TabOrder = 12
    OnClick = Button2Click
  end
  object Edit10: TEdit
    Left = 64
    Top = 232
    Width = 225
    Height = 25
    Color = clSkyBlue
    TabOrder = 13
    Text = 'x^4'
    OnChange = Edit10Change
  end
  object Panel3: TPanel
    Left = 296
    Top = 232
    Width = 57
    Height = 25
    Caption = 'Color'
    Color = clYellow
    TabOrder = 14
    OnClick = Panel3Click
  end
  object Panel4: TPanel
    Left = 296
    Top = 40
    Width = 49
    Height = 25
    Caption = 'Color'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 15
    OnClick = Panel4Click
  end
  object Panel5: TPanel
    Left = 296
    Top = 120
    Width = 49
    Height = 25
    Caption = 'Color'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 16
    OnClick = Panel5Click
  end
  object Timer1: TTimer
    Interval = 50
    Left = 320
    Top = 128
  end
  object ColorDialog1: TColorDialog
    Left = 288
    Top = 128
  end
end
