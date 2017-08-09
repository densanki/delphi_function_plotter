unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,Math,Unit2,SpeedParser;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit5: TEdit;
    Label5: TLabel;
    Timer1: TTimer;
    Button1: TButton;
    Label6: TLabel;
    Label7: TLabel;
    Edit6: TEdit;
    Edit7: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Edit8: TEdit;
    Label10: TLabel;
    Edit9: TEdit;
    Label11: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ColorDialog1: TColorDialog;
    Button2: TButton;
    Edit10: TEdit;
    Label12: TLabel;
    Panel3: TPanel;
    Label13: TLabel;
    Panel4: TPanel;
    Label14: TLabel;
    Panel5: TPanel;

    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure Edit10Change(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
  private
    { Private declarations }
  public
    SpeedParser1: TSpeedParser;
    SpeedParser2: TSpeedParser;
    SpeedParser3: TSpeedParser;

    { Public declarations }
  end;

var
  Form1: TForm1;


implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin

  Form2.Width  := STRtoINT(Form1.Edit6.text);
  Form2.Height := STRtoINT(Form1.Edit7.text);

  Form2.Visible := TRUE;
  Form2.Plotter;

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Application.Terminate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin

  Close;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SpeedParser1 := TSpeedParser.Create(Self);
  SpeedParser2 := TSpeedParser.Create(Self);
  SpeedParser3 := TSpeedParser.Create(Self);
  Form1.SpeedParser1.ParseString := Form1.Edit8.Text;
  Form1.SpeedParser2.ParseString := Form1.Edit9.Text;
  Form1.SpeedParser3.ParseString := Form1.Edit10.Text;
end;

procedure TForm1.Edit8Change(Sender: TObject);
begin

  try
    Form1.SpeedParser1.ParseString := Form1.Edit8.Text;
    Form2.Refresh;
    Form2.Plotter;
  except
  end;

end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  ColorDialog1.Execute;
  Panel1.Color := ColorDialog1.Color;
  Form2.Refresh;
  Form2.Plotter;
end;

procedure TForm1.Panel2Click(Sender: TObject);
begin
  ColorDialog1.Execute;
  Panel2.Color := ColorDialog1.Color;
  Form2.Refresh;
  Form2.Plotter;
end;

procedure TForm1.Panel3Click(Sender: TObject);
begin
  ColorDialog1.Execute;
  Panel3.Color := ColorDialog1.Color;
  Form2.Refresh;
  Form2.Plotter;  
end;

procedure TForm1.Edit9Change(Sender: TObject);
begin
  try
    Form1.SpeedParser2.ParseString := Form1.Edit9.Text;
    Form2.Refresh;
    Form2.Plotter;
  except
  end;
end;

procedure TForm1.Edit10Change(Sender: TObject);
begin
  try
    Form1.SpeedParser3.ParseString := Form1.Edit10.Text;
    Form2.Refresh;
    Form2.Plotter;
  except
  end;
end;

procedure TForm1.Panel4Click(Sender: TObject);
begin
  ColorDialog1.Execute;
  Form2.Color := ColorDialog1.Color;
  Panel4.Color := ColorDialog1.Color;
  Form2.Refresh;
  Form2.Plotter;
end;

procedure TForm1.Panel5Click(Sender: TObject);
begin
  ColorDialog1.Execute;
  Panel5.Color := ColorDialog1.Color;
  Form2.Refresh;
  Form2.Plotter;
end;

end.
