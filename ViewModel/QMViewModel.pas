unit QMViewModel;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Threading, QMModel, QM.Service;

type
  TNotifyBoolEvent = reference to procedure(Sender: TObject; const Value: Boolean);

  TTermObj = class
  public
    Pattern: string;
    Mins: TList<Integer>;
    constructor Create(const APattern: string; const AMins: TArray<Integer>);
    destructor Destroy; override;
  end;

  TQMViewModel = class
  private
    FService: IQMInterface;
    FMintermsText: string;
    FDontCaresText: string;
    FVarCount: Integer;
    FSteps: TStringList;
    FResult: string;
    FAndSign: string;
    FOrSign: string;
    FIsBusy: Boolean;
    FOnStepsChanged: TNotifyEvent;
    FOnResultChanged: TNotifyEvent;
    FOnBusyChanged: TNotifyBoolEvent;
    procedure SetMintermsText(const Value: string);
    procedure SetDontCaresText(const Value: string);
    procedure SetVarCount(const Value: Integer);
    procedure SetBusy(const Value: Boolean);
    procedure ParseInts(const Text: string; out Arr: TArray<Integer>);
  public
    constructor Create(Service: IQMInterface);
    destructor Destroy; override;

    property MintermsText: string read FMintermsText write SetMintermsText;
    property DontCaresText: string read FDontCaresText write SetDontCaresText;
    property VarCount: Integer read FVarCount write SetVarCount;
    property Steps: TStringList read FSteps;
    property ResultExpr: string read FResult;

    property OnStepsChanged: TNotifyEvent read FOnStepsChanged write FOnStepsChanged;
    property OnResultChanged: TNotifyEvent read FOnResultChanged write FOnResultChanged;
    property OnBusyChanged: TNotifyBoolEvent read FOnBusyChanged write FOnBusyChanged;
    property AndSign: string read FAndSign write FAndSign;
    property OrSign: string read FOrSign write FOrSign;

    procedure RunAsync;
  end;

implementation

{ TTermObj }

constructor TTermObj.Create(const APattern: string; const AMins: TArray<Integer>);
var
  v: Integer;
begin
  Pattern := APattern;
  Mins := TList<Integer>.Create;
  for v in AMins do
    Mins.Add(v);
end;

destructor TTermObj.Destroy;
begin
  Mins.Free;
  inherited;
end;

{ TQMViewModel }

constructor TQMViewModel.Create(Service: IQMInterface);
begin
  inherited Create;
  FService := Service;
  FSteps := TStringList.Create;
  FVarCount := 4;
  FIsBusy := False;
end;

destructor TQMViewModel.Destroy;
begin
  FSteps.Free;
  inherited;
end;

procedure TQMViewModel.ParseInts(const Text: string; out Arr: TArray<Integer>);
var
  parts: TArray<string>;
  p: string;
  tmp: TList<Integer>;
  tmpValue: Integer;
begin
  if trim(Text) <> '' then
  begin
    parts := Text.Split([',',';',' '], TStringSplitOptions.ExcludeEmpty);
    SetLength(Arr, Length(parts));
    tmp := TList<Integer>.Create;
    try
      for var I := 0 to High(parts) do
      begin
        p := Trim(parts[I]);
        if p = '' then Continue;
        if TryStrToInt(p, tmpValue) then
          tmp.Add(tmpValue)
        else
          raise Exception.CreateFmt('Nem érvényes egész szám: %s', [p]);
      end;
      Arr := tmp.ToArray;
    finally
      tmp.Free;
    end;
    end
  else
    SetLength(Arr, 0);
end;

procedure TQMViewModel.SetBusy(const Value: Boolean);
begin
  if FIsBusy = Value then
    Exit;
  FIsBusy := Value;
  if Assigned(FOnBusyChanged) then
    TThread.Queue(nil,
      procedure
      begin
        FOnBusyChanged(Self, FIsBusy);
      end);
end;

procedure TQMViewModel.SetDontCaresText(const Value: string);
begin
  if FDontCaresText = Value then
    Exit;
  FDontCaresText := Value;
end;

procedure TQMViewModel.SetMintermsText(const Value: string);
begin
  if FMintermsText = Value then
    Exit;
  FMintermsText := Value;
end;

procedure TQMViewModel.SetVarCount(const Value: Integer);
begin
  if FVarCount = Value then
    Exit;
  FVarCount := Value;
end;

procedure TQMViewModel.RunAsync;
var
  mins, dcs: TArray<Integer>;
begin
  if FIsBusy then
    Exit;
  ParseInts(FMintermsText, mins);
  ParseInts(FDontCaresText, dcs);

  if Length(FAndSign) <> 1 then
    FAndSign := '&';
  if Length(FOrSign) <> 1 then
    FOrSign := '|';

  FService.Init(FAndSign, FOrSign);

  SetBusy(True);

  TTask.Run(
    procedure
    var
      stepsLocal: TStringList;
      resultExprLocal: string;
    begin
      stepsLocal := TStringList.Create;
      resultExprLocal := '';

      try
        try
          FService.Compute(mins, dcs, FVarCount, stepsLocal, resultExprLocal);
        except
          on E: Exception do
          begin
            stepsLocal.Add('Hiba: ' + E.Message);
            resultExprLocal := '';
          end;
        end;

        TThread.Queue(nil,
          procedure
          begin
            try
              FSteps.Assign(stepsLocal);
              FResult := resultExprLocal;

              if Assigned(FOnStepsChanged) then
                FOnStepsChanged(Self);
              if Assigned(FOnResultChanged) then
                FOnResultChanged(Self);
            finally
              stepsLocal.Free;
              SetBusy(False);
            end;
          end);
      finally
        // Csak ha más erőforrás van, ami feltétlenül igényli a felszabadítást, maradjon itt.
        // stepsLocal.Free már itt NEM kell!
      end;
    end);
end;

end.
