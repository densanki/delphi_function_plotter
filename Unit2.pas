unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,Unit3, Menus, Clipbrd, StdCtrls,Printers, SpeedParser;

type
  TForm2 = class(TForm)
    MainMenu1: TMainMenu;
    Bearbeiten1: TMenuItem;
    Zwischenablagekopieren1: TMenuItem;
    ZeichnungDrucken1: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDblClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Zwischenablagekopieren1Click(Sender: TObject);
    procedure ZeichnungDrucken1Click(Sender: TObject);
  private

    { Private declarations }
  public
    procedure Plotter;
    procedure DruckPlotter;
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm2.Plotter;
var X,Y : double;
    Xb,Yb : double;
    Z : double;
    I : Integer;
    XMIN,YMIN,XMAX,YMAX : Single;
    XBMAX,YBMAX : Integer;
    Schritt : double;

    m : Single;
    S : Single;
    Y1 : Single;
    Y2 : Single;
    X1 : Single;
    X2 : Single;
begin

  XMIN := STRtoFLOAT(Form1.Edit1.text);
  YMIN := STRtoFLOAT(Form1.Edit2.text);
  XMAX := STRtoFLOAT(Form1.Edit3.text);
  YMAX := STRtoFLOAT(Form1.Edit4.text);

  XBMAX := Form2.ClientWidth;
  YBMAX := Form2.ClientHeight;

  Schritt := STRtoFLOAT(Form1.Edit5.Text);

  //Schnittlinien erzeugen

  Xb := 0 + ((XBMAX-0)/(XMAX - XMIN))*(0-XMIN) ;
  Yb := - (YBMAX/(YMAX-YMIN))*(0-YMIN)+YBMAX   ;

  FOR I := 1 TO XBMAX DO Form2.Canvas.Pixels[I,ROUND(YB)] := Form1.Panel5.Color;
  FOR I := 1 TO YBMAX DO Form2.Canvas.Pixels[ROUND(XB),I] := Form1.Panel5.Color;

  //Function 1 ###########
  Z := -(XBMAX / 2);
  Repeat
    X := Z;
    Y := Form1.SpeedParser1.Parse(X);
    Xb := 0 + ((XBMAX-0)/(XMAX - XMIN))*(X-XMIN);
    Yb := - (YBMAX/(YMAX-YMIN))*(y-YMIN)+YBMAX;
    Form2.Canvas.Pixels[Round(XB),ROUND(YB)] := Form1.Panel1.Color;
    Z := Z + Schritt;
  Until Z >= XBMAX;

  //Function 2 ###########
  Z := -(XBMAX / 2);
  Repeat
    X := Z;
    Y := Form1.SpeedParser2.Parse(X);
    Xb := 0 + ((XBMAX-0)/(XMAX - XMIN))*(X-XMIN);
    Yb := - (YBMAX/(YMAX-YMIN))*(y-YMIN)+YBMAX;
    Form2.Canvas.Pixels[Round(XB),ROUND(YB)] := Form1.Panel2.Color;
    Z := Z + Schritt;
  Until Z >= XBMAX;

  //Function 3 ###########
  Z := -(XBMAX / 2);
  Repeat
    X := Z;
    Y := Form1.SpeedParser3.Parse(X);
    Xb := 0 + ((XBMAX-0)/(XMAX - XMIN))*(X-XMIN);
    Yb := - (YBMAX/(YMAX-YMIN))*(y-YMIN)+YBMAX;
    Form2.Canvas.Pixels[Round(XB),ROUND(YB)] := Form1.Panel3.Color;
    Z := Z + Schritt;
  Until Z >= XBMAX;

  {S := (XMAX-XMIN)/5000;

  X1 := XMIN;
  X2 := X1 + S;

  Repeat
    Y1 := Sin(X1)/X1;
    Y2 := Sin(X2)/X2;
    m := (Y2 -Y1) / (X2-X1);
    XB := (XBMAX/(XMAX-XMIN))*(X1-XMIN);
    YB := -(YBMAX/(YMAX-YMIN))*(m-YMIN)+YBMAX;
    Form2.Canvas.Pixels[Round(XB),ROUND(YB)] := clyellow;
    X1 := X1 + s;
    X2 := X1 + s;
  until X2 >= XMAX;}

end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Application.Terminate;
end;

procedure TForm2.FormDblClick(Sender: TObject);
begin

  Form2.Visible := FALSE;

end;

procedure TForm2.FormClick(Sender: TObject);
begin

  Form2.Refresh;
  Form2.Plotter;

end;

procedure TForm2.FormResize(Sender: TObject);
begin

  Form2.Refresh;
  Form2.Plotter;

end;

procedure TForm2.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var XB, YB : Single;
    XMIN,YMIN,XMAX,YMAX  : Single;
    XBMAX,YBMAX : Integer;
begin

  XMIN := STRtoFLOAT(Form1.Edit1.text);
  YMIN := STRtoFLOAT(Form1.Edit2.text);
  XMAX := STRtoFLOAT(Form1.Edit3.text);
  YMAX := STRtoFLOAT(Form1.Edit4.text);

  XBMAX := Form2.ClientWidth;
  YBMAX := Form2.ClientHeight;

  XB := (X*((XMAX-XMIN)/XBMAX))+XMIN       ;
  YB := (Y-YBMAX)*((YMAX-YMIN)/YBMAX)-YMIN ;

  Form2.Canvas.Font.Color := Form1.Panel5.Color;
  Form2.Canvas.TextOut(15,15, 'X : ' + FormatFloat('#0.000#',XB) + '      ');
  Form2.Canvas.TextOut(15,30, 'Y : ' + FormatFloat('#0.000#',YB) + '      ');

end;

procedure TForm2.Zwischenablagekopieren1Click(Sender: TObject);
var Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := ClientWidth;
  Bitmap.Height := ClientHeight;
  try
    With Bitmap.Canvas do
      CopyRect(ClientRect,Canvas,ClientRect);
    Clipboard.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;

end;

procedure TForm2.ZeichnungDrucken1Click(Sender: TObject);
var r:TRect;
begin

  Printer.BeginDoc;

  try
     Form2.DruckPlotter;
  finally
  end;

  Printer.EndDoc;

end;

procedure TForm2.DruckPlotter;
var X,Y : double;
    Xb,Yb : double;
    Z : double;
    I : Integer;
    XMIN,YMIN,XMAX,YMAX : Single;
    XBMAX,YBMAX : Integer;
    Schritt : double;

    m : Single;
    S : Single;
    Y1 : Single;
    Y2 : Single;
    X1 : Single;
    X2 : Single;
begin

  XMIN := STRtoFLOAT(Form1.Edit1.text);
  YMIN := STRtoFLOAT(Form1.Edit2.text);
  XMAX := STRtoFLOAT(Form1.Edit3.text);
  YMAX := STRtoFLOAT(Form1.Edit4.text);

  XBMAX := Printer.PageWidth;
  YBMAX := Printer.PageHeight;

  Schritt := STRtoFLOAT(Form1.Edit5.Text);

  //Schnittlinien erzeugen

  Xb := 0 + ((XBMAX-0)/(XMAX - XMIN))*(0-XMIN) ;
  Yb := - (YBMAX/(YMAX-YMIN))*(0-YMIN)+YBMAX   ;

  FOR I := 1 TO XBMAX DO Printer.Canvas.Pixels[I,ROUND(YB)]:=clblack;
  FOR I := 1 TO YBMAX DO Printer.Canvas.Pixels[ROUND(XB),I]:=clblack;

  //Function

  Z := -(XBMAX / 2);
  Repeat

    X := Z;

    Y := Sin(X)/X;

    Xb := 0 + ((XBMAX-0)/(XMAX - XMIN))*(X-XMIN);
    Yb := - (YBMAX/(YMAX-YMIN))*(y-YMIN)+YBMAX;

    Printer.Canvas.Pixels[Round(XB),ROUND(YB)] := clBlack;

    Z := Z + Schritt;

  Until Z >= XBMAX;

  S := (XMAX-XMIN)/5000;

  X1 := XMIN;
  X2 := X1 + S;

  Repeat
    Y1 := Sin(X1)/X1;
    Y2 := Sin(X2)/X2;
    m := (Y2 -Y1) / (X2-X1);
    XB := (XBMAX/(XMAX-XMIN))*(X1-XMIN);
    YB := -(YBMAX/(YMAX-YMIN))*(m-YMIN)+YBMAX;
    Printer.Canvas.Pixels[Round(XB),ROUND(YB)] := clblack;
    X1 := X1 + s;
    X2 := X1 + s;
  until X2 >= XMAX;

end;



end.
