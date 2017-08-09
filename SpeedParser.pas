unit SpeedParser;

{**************************************}
{  SpeedParser VCL Component, v 0.3    }
{  Copyright © 2001 Mattias Andersson  }
{  mattias@centaurix.com               }
{**************************************}

interface

uses
  SysUtils, Classes, Math;

  { Defines what type to use in your computation -
    valid types are real, single and extended }
  {$DEFINE REAL}

type
  PVarEntry = ^TVarEntry;
  TVarEntry = record
    Name: string[7];
    Value: {$IFDEF REAL}Real{$ENDIF}
           {$IFDEF SINGLE}Single{$ENDIF}
           {$IFDEF EXTENDED}Extended{$ENDIF};
  end;
  TMathMode = (mmMult, mmDiv, mmAdd, mmSub, mmPower, mmFaculty, mmAbs, mmFrac,
               mmSin, mmCos, mmTan, mmCot, mmASin, mmACos, mmATan, mmSinh,
               mmCosh, mmTanh, mmLog, mmLn, mmExp, mmSave);

  TSpeedParser = class(TComponent)
  private
    FParseString: String;
    FoundVar: Boolean;
    ValList: TList; // Constant values from string expression
    MemList: TList; // Value of expression inside a paranthesis

    { Array of pointers from either VarList, ValList or MemList }
    PtrArray: array of ^{$IFDEF REAL}Real{$ENDIF}
                        {$IFDEF SINGLE}Single{$ENDIF}
                        {$IFDEF EXTENDED}Extended{$ENDIF};
    { This array contains the math instructions. The parse function
     will iterate through this array. }
    ModeArray: array of TMathMode;
    Term: array of {$IFDEF REAL}Real{$ENDIF}
                   {$IFDEF SINGLE}Single{$ENDIF}
                   {$IFDEF EXTENDED}Extended{$ENDIF};
    MemIndex, ModeIndex, PtrIndex, MaxTermIndex: Word;
    procedure SetParseString(Value: string);
  public
    VarList: TList; // Variable names and values
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddVar(AName: string; AValue: {$IFDEF REAL}Real{$ENDIF}
                                            {$IFDEF SINGLE}Single{$ENDIF}
                                            {$IFDEF EXTENDED}Extended{$ENDIF} = 0);
    procedure SetVar(AName: string; AValue: {$IFDEF REAL}Real{$ENDIF}
                                            {$IFDEF SINGLE}Single{$ENDIF}
                                            {$IFDEF EXTENDED}Extended{$ENDIF} = 0);
    function GetIndex(const AName: string): Word;
    function FindVar(const AName: string): {$IFDEF REAL}Real{$ENDIF}
                                           {$IFDEF SINGLE}Single{$ENDIF}
                                           {$IFDEF EXTENDED}Extended{$ENDIF};
    function Parse(X: {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}{$IFDEF EXTENDED}Extended{$ENDIF} = 0;
                   Y: {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}{$IFDEF EXTENDED}Extended{$ENDIF} = 0;
                   Z: {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}{$IFDEF EXTENDED}Extended{$ENDIF} = 0):
                   {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}{$IFDEF EXTENDED}Extended{$ENDIF};
    function Faculty(X: {$IFDEF REAL}Real{$ENDIF}
                        {$IFDEF SINGLE}Single{$ENDIF}
                        {$IFDEF EXTENDED}Extended{$ENDIF}):
                        {$IFDEF REAL}Real{$ENDIF}
                        {$IFDEF SINGLE}Single{$ENDIF}
                        {$IFDEF EXTENDED}Extended{$ENDIF};
  published
    property ParseString: string read FParseString write SetParseString;
  end;

procedure Register;

implementation

type
  charSet = set of char;

const
  Digits: charSet = ['0'..'9'];
  Commas: charSet = ['.', ','];
  VarSet: charSet = ['a'..'z', 'A'..'Z'];
  Separators: charSet = ['+', '-', '*', '/', '^', '!'];

procedure Register;
begin
  RegisterComponents('System', [TSpeedParser]);
end;

constructor TSpeedParser.Create(AOwner: TComponent);
var
  i: Byte;
begin
  inherited create(AOwner);
  VarList := TList.Create;
  ValList := TList.Create;
  for i := ord('a') to ord('z') do AddVar(chr(i));
  AddVar('pi');
  SetVar('pi', Pi);
end;

destructor TSpeedParser.Destroy;
begin
  VarList.Free;
  ValList.Free;
  MemList.Free;
  inherited;
end;

{ This routine recompiles the string expression into an array of pointers and
  an array of instructions, which are evaluated in the parse function. This will
  greatly optimize performance in comparison with a routine exclusively working
  with strings }
procedure TSpeedParser.SetParseString(Value: string);

  procedure AddMode(Mode: TMathMode);
  begin
    Inc(ModeIndex);
    SetLength(ModeArray, ModeIndex);
    ModeArray[ModeIndex - 1] := Mode;
  end;

  procedure AddPointer(Value: Pointer);
  begin
    Inc(ptrIndex);
    SetLength(PtrArray, PtrIndex);
    PtrArray[PtrIndex - 1] := Value;
  end;

  function AddReal(var List: TList; const Value: {$IFDEF REAL}Real{$ENDIF}
                                          {$IFDEF SINGLE}Single{$ENDIF}
                                          {$IFDEF EXTENDED}Extended{$ENDIF} = 0): pointer;
  var
    PReal: ^{$IFDEF REAL}Real{$ENDIF}
            {$IFDEF SINGLE}Single{$ENDIF}
            {$IFDEF EXTENDED}Extended{$ENDIF};
  begin
    New(PReal);
    if PReal = nil then
      raise Exception.Create('Could not allocate memory for new value!');
    PReal^ := Value;
    with List do Result := Items[Add(PReal)];
  end;

  procedure Recurse(SubString: string);
  var
    TermCount, I: Word;
    Mode: TMathMode;
    NewMode: Boolean;
    StrValue: string;
  begin
    TermCount := 0;
    Mode := mmMult;
    I := 1;
    while I <= length(SubString) do
    begin
      NewMode := False;
      StrValue := '';
      if (SubString[I] in Digits + Commas) then
      begin
        repeat
          if SubString[I] = '.' then SubString[i] := ',';
          StrValue := StrValue + SubString[i];
          Inc(I);
        until (SubString[I] in Digits + Commas) = false;
        AddPointer(AddReal(ValList, strtofloat(StrValue)));
        NewMode := True;
      end
      else if (SubString[I] in VarSet) then
      begin
        repeat
          StrValue := StrValue + SubString[i];
          Inc(i);
        until (SubString[I] in VarSet) = false;
        FindVar(StrValue);
        if FoundVar then
        begin
          AddPointer(@TVarEntry(VarList.Items[GetIndex(StrValue)]^).Value);
          NewMode := True;
        end;
      end
      else if SubString[I] = '(' then
      begin
        AddPointer(MemList.Items[MemIndex]);
        Inc(MemIndex);
        Inc(I, 2);
        NewMode := True
      end
      else
      if (SubString[I] in separators) then
      begin
        StrValue := SubString[I];
        Inc(I);
      end
      else Inc(I);
      if NewMode then begin AddMode(Mode); end
      else
      begin
        StrValue := LowerCase(StrValue);
        case length(StrValue) of
         1: case StrValue[1] of
              '*': Mode := mmMult;
              '/': Mode := mmDiv;
              '+': begin AddMode(mmAdd); inc(TermCount); end;
              '-': begin AddMode(mmSub); inc(TermCount); end;
              '^': begin Mode := mmPower; Dec(ModeIndex); end;
              '!': begin Dec(ModeIndex); AddMode(mmFaculty); end;
            end;
         2: if StrValue = 'ln' then Mode := mmLn;
         3: begin
              if StrValue = 'abs' then Mode := mmAbs
              else if StrValue = 'log' then Mode := mmLog
              else if StrValue = 'exp' then Mode := mmExp
              else if StrValue = 'sin' then Mode := mmSin
              else if StrValue = 'cos' then Mode := mmCos
              else if StrValue = 'tan' then Mode := mmTan
              else if StrValue = 'cot' then Mode := mmCot
            end;
         4: begin
              if StrValue = 'frac' then Mode := mmFrac
              else if StrValue = 'asin' then Mode := mmASin
              else if StrValue = 'acos' then Mode := mmACos
              else if StrValue = 'atan' then Mode := mmATan
              else if StrValue = 'sinh' then Mode := mmSinh
              else if StrValue = 'cosh' then Mode := mmCosh
              else if StrValue = 'tanh' then Mode := mmTanh
            end;
          end;
        end;
      end;
    if TermCount > MaxTermIndex then MaxTermIndex := TermCount;
    AddMode(mmSave);
  end;

var
  Level, I, Current, MaxLevel, Count, Index: Word;
begin
  FParseString := Value;
  Current := 0;
  Index := 0;
  MaxLevel := 0;
  MemIndex := 0;
  PtrIndex := 0;
  ModeIndex := 0;
  MaxTermIndex := 0;
  MemList.Free;
  MemList := TList.Create;
  if Length(FParseString) = 0 then FParseString := '0';
  // raise Exception.Create('Input string contains no data!');
  for I := 0 to Length(FParseString) do
  begin
    case FParseString[i] of
      '(': Inc(Current);
      ')': Dec(Current);
    end;
    if Current > MaxLevel then MaxLevel := current;
  end;
  Current := 0;
  for Level := MaxLevel downto 1 do
  begin
    I := 0;
    while I <= length(FParseString) do
    begin
      case FParseString[I] of
      ')': begin
             if Current = Level then
             begin
               Count := I - Index;
               Recurse(copy(FParseString, Index, Count));
               AddReal(MemList);
               Delete(FParseString, Index, Count);
               I := Index;
             end;
             Dec(Current);
           end;
      '(': begin
             Inc(Current);
             if Current = Level then Index := I + 1;
           end;
      end;
      Inc(i);
    end;
  end;
  AddReal(MemList);
  Recurse(FParseString);
  SetLength(Term, MaxTermIndex + 1);
end;


{ This function is called from your application to evaluate the ParseString
  expression. The X, Y and Z variable are set when calling this function }
function TSpeedParser.Parse(X: {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}
                               {$IFDEF EXTENDED}Extended{$ENDIF} = 0;
                            Y: {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}
                               {$IFDEF EXTENDED}Extended{$ENDIF} = 0;
                            Z: {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}
                               {$IFDEF EXTENDED}Extended{$ENDIF} = 0):
                            {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}
                            {$IFDEF EXTENDED}Extended{$ENDIF};

  function GetNextVal: Real;
  begin
    Result := PtrArray[PtrIndex]^;
    inc(PtrIndex);
  end;

var
  I, ModeCount: Word;
  TempSum: {$IFDEF REAL}Real{$ENDIF}
           {$IFDEF SINGLE}Single{$ENDIF}
           {$IFDEF EXTENDED}Extended{$ENDIF};
  Index: Word;
  PReal: ^{$IFDEF REAL}Real{$ENDIF}
          {$IFDEF SINGLE}Single{$ENDIF}
          {$IFDEF EXTENDED}Extended{$ENDIF};
begin
  with VarList do
  begin
    TVarEntry(Items[23]^).Value := X;
    TVarEntry(Items[24]^).Value := Y;
    TVarEntry(Items[25]^).Value := Z;
  end;

  PtrIndex := 0;
  MemIndex := 0;
  Index := 0;

  for I := 0 to MaxTermIndex do Term[I] := 1;
  if ModeArray[0] = mmSub then Term[0] := 0;

  for ModeCount := 0 to High(ModeArray) do
  begin
    case ModeArray[ModeCount] of
      mmSave: begin
                TempSum := 0;
                for I := 0 to Index do begin
                  TempSum := TempSum + Term[I];
                  Term[I] := 1;
                end;
                {$IFDEF REAL}Real{$ENDIF}
                {$IFDEF SINGLE}Single{$ENDIF}
                {$IFDEF EXTENDED}Extended{$ENDIF}
                (MemList.Items[MemIndex]^) := TempSum;
                Inc(MemIndex);
                Index := 0;
              end;
      mmMult: Term[Index] := Term[Index] * GetNextVal;
      mmDiv: Term[Index] := Term[Index] / GetNextVal;
      mmAdd: Inc(Index);
      mmSub: begin Inc(Index); Term[Index] := -1; end;
      mmPower: Term[Index] := Term[Index] * Power(GetNextVal, GetNextVal);
      mmFaculty: Term[Index] := Term[Index] * Faculty(GetNextVal);
      mmAbs: Term[Index] := Term[Index] * Abs(GetNextVal);
      mmFrac: Term[Index] := Term[Index] * Frac(GetNextVal);
      mmSin: Term[Index] := Term[Index] * Sin(GetNextVal);
      mmCos: Term[Index] := Term[Index] * Cos(GetNextVal);
      mmTan: Term[Index] := Term[Index] * Tan(GetNextVal);
      mmCot: Term[Index] := Term[Index] * CoTan(GetNextVal);
      mmASin: Term[Index] := Term[Index] * ArcSin(GetNextVal);
      mmACos: Term[Index] := Term[Index] * ArcCos(GetNextVal);
      mmATan: Term[Index] := Term[Index] * ArcTan(GetNextVal);
      mmSinh: Term[Index] := Term[Index] * Sinh(GetNextVal);
      mmCosh: Term[Index] := Term[Index] * Cosh(GetNextVal);
      mmTanh: Term[Index] := Term[Index] * Tanh(GetNextVal);
      mmLog: Term[Index] := Term[Index] * Log10(GetNextVal);
      mmLn: Term[Index] := Term[Index] * Ln(GetNextVal);
      mmExp: Term[Index] := Term[Index] * Exp(GetNextVal);
    end;
  end;
  Result := {$IFDEF REAL}Real{$ENDIF}
            {$IFDEF SINGLE}Single{$ENDIF}
            {$IFDEF EXTENDED}Extended{$ENDIF}(MemList.Items[MemIndex-1]^);
end;

{ This procedure allows you to define your own variables }
procedure TSpeedParser.AddVar(AName: string; AValue: {$IFDEF REAL}Real{$ENDIF}
                                                     {$IFDEF SINGLE}Single{$ENDIF}
                                                     {$IFDEF EXTENDED}Extended{$ENDIF} = 0);
var
  NewEntry: PVarEntry;
begin
  New(NewEntry);
  if NewEntry = nil then
    raise Exception.Create('Could not allocate memory for new variable!');
  with NewEntry^ do
  begin
    Name := AName;
    Value := AValue;
  end;
  Varlist.Add(NewEntry);
end;

{ Use this procedure to set an existing variable }
procedure TSpeedParser.SetVar(AName: string; AValue: {$IFDEF REAL}Real{$ENDIF}
                                                     {$IFDEF SINGLE}Single{$ENDIF}
                                                     {$IFDEF EXTENDED}Extended{$ENDIF} = 0);
var
  I: Integer;
begin
  with VarList do
  begin
    for I := 0 to Count - 1 do
      with TVarEntry(Items[I]^) do
        if Name = AName then Value := AValue;
  end;
end;


function TSpeedParser.FindVar(const AName: string): {$IFDEF REAL}Real{$ENDIF}
                                                    {$IFDEF SINGLE}Single{$ENDIF}
                                                    {$IFDEF EXTENDED}Extended{$ENDIF};
var
  I: Word;
begin
  with VarList do
  begin
    FoundVar := False;
    I := 0;
    while (TVarEntry(Items[I]^).Name <> AName) and (I < Count - 1) do Inc(I);
    with TVarEntry(Items[I]^) do
    begin
      FoundVar := Name = AName;
      Result := Value;
    end;
  end;
end;

function TSpeedParser.GetIndex(const AName: string): Word;
var
  I: Word;
begin
  I := 0;
  while (TVarEntry(Varlist.Items[I]^).Name <> AName) and (I < VarList.Count - 1) do Inc(I);
  Result := I;
end;

function TSpeedParser.Faculty(X: {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}
                                 {$IFDEF EXTENDED}Extended{$ENDIF}):
                                 {$IFDEF REAL}Real{$ENDIF}{$IFDEF SINGLE}Single{$ENDIF}
                                 {$IFDEF EXTENDED}Extended{$ENDIF};
var
  I: Integer;
begin
  Result := 1;
  if frac(x) = 0 then for I := 2 to round(X) do Result := Result * I;
end;

end.
