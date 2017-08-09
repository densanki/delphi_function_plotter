object Form2: TForm2
  Left = 575
  Top = 106
  Width = 329
  Height = 199
  Caption = 'Ausgabe  Klick = Refresh  Doppelklick = Schlie'#223'en'
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClick = FormClick
  OnCloseQuery = FormCloseQuery
  OnDblClick = FormDblClick
  OnMouseMove = FormMouseMove
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 16
    Top = 16
    object Bearbeiten1: TMenuItem
      Caption = 'Bearbeiten'
      object Zwischenablagekopieren1: TMenuItem
        Caption = 'Zwischenablage kopieren'
        OnClick = Zwischenablagekopieren1Click
      end
      object ZeichnungDrucken1: TMenuItem
        Caption = 'Zeichnung Drucken'
        OnClick = ZeichnungDrucken1Click
      end
    end
  end
end
