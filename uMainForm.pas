unit uMainForm;

interface

uses
  Vcl.Forms, Vcl.Dialogs, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes, System.SysUtils, QMModel, QMViewModel, QM.Service, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    PanelTop: TPanel;
    lblMinterms: TLabel;
    edtMinterms: TEdit;
    lblDontCares: TLabel;
    edtDontCares: TEdit;
    lblVars: TLabel;
    edtVars: TEdit;
    btnRun: TButton;
    memSteps: TMemo;
    lblResult: TLabel;
    edtResult: TEdit;
    StatusBar1: TStatusBar;
    lblAndSign: TLabel;
    edtAndSign: TEdit;
    lblOrSign: TLabel;
    edtOrSign: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure edtMintermsChange(Sender: TObject);
    procedure edtDontCaresChange(Sender: TObject);
    procedure edtVarsChange(Sender: TObject);
  private
    FViewModel: TQMViewModel;
    FService: IQMInterface;
    procedure VM_StepsChanged(Sender: TObject);
    procedure VM_ResultChanged(Sender: TObject);
    procedure VM_BusyChanged(Sender: TObject; const Value: Boolean);
    procedure BindViewToVM;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.BindViewToVM;
begin
  // A view frissítése a viewmodell-ből
  edtMinterms.Text := FViewModel.MintermsText;
  edtDontCares.Text := FViewModel.DontCaresText;
  edtVars.Text := IntToStr(FViewModel.VarCount);
  memSteps.Lines.Assign(FViewModel.Steps);
  edtResult.Text := FViewModel.ResultExpr;
end;

procedure TMainForm.btnRunClick(Sender: TObject);
begin
  try
    if FViewModel.VarCount < 3 then
    begin
      ShowMessage('A változók száma túl kicsi!');
      Exit;
    end;

    if FViewModel.VarCount > 5 then
    begin
      ShowMessage('A változók száma túl nagy!');
      Exit;
    end;

    if Trim(edtAndSign.Text) <> '' then
      FViewModel.AndSign := edtAndSign.Text;

    if Trim(edtOrSign.Text) <> '' then
      FViewModel.OrSign := edtOrSign.Text;

    FViewModel.RunAsync;

  except
    on E: Exception do
      ShowMessage('Hiba: ' + E.Message);
  end;
end;

procedure TMainForm.edtDontCaresChange(Sender: TObject);
begin
  FViewModel.DontCaresText := edtDontCares.Text;
end;

procedure TMainForm.edtMintermsChange(Sender: TObject);
begin
  FViewModel.MintermsText := edtMinterms.Text;
end;

procedure TMainForm.edtVarsChange(Sender: TObject);
begin
  FViewModel.VarCount := StrToIntDef(edtVars.Text, FViewModel.VarCount);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FService := TQMService.Create;
  FViewModel := TQMViewModel.Create(FService);

  // alapértelmezett értékek, hogy ne legyen üres
  FViewModel.MintermsText := '1,3,12,13,14,15,17,19,28,29,30,31';
  FViewModel.DontCaresText := '2,9,14'; // lehet üres
  FViewModel.VarCount := 5;

  // feliratkozás a viewmodell eseményeire
  FViewModel.OnStepsChanged := VM_StepsChanged;
  FViewModel.OnResultChanged := VM_ResultChanged;
  FViewModel.OnBusyChanged := VM_BusyChanged;

  BindViewToVM;
end;

procedure TMainForm.VM_BusyChanged(Sender: TObject; const Value: Boolean);
begin
  // enable/disable UI while busy
  btnRun.Enabled := not Value;
  edtMinterms.Enabled := not Value;
  edtDontCares.Enabled := not Value;
  edtVars.Enabled := not Value;
  if Value then
    memSteps.Lines.Add('Egyszerűsítés indítása')
  else
    memSteps.Lines.Add('Az egyszerűsítés lefutott.');
end;

procedure TMainForm.VM_ResultChanged(Sender: TObject);
begin
  edtResult.Text := FViewModel.ResultExpr;
end;

procedure TMainForm.VM_StepsChanged(Sender: TObject);
begin
  memSteps.Lines.Assign(FViewModel.Steps);
end;

end.
