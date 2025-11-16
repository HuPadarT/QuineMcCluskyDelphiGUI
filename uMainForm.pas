unit uMainForm;

interface

uses
  Vcl.Forms, Vcl.Dialogs, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes, System.SysUtils, QMModel, QMViewModel, QM.Service;

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
    procedure FormCreate(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure edtMintermsChange(Sender: TObject);
    procedure edtDontCaresChange(Sender: TObject);
    procedure edtVarsChange(Sender: TObject);
  private
    FViewModel: TQMViewModel;
    FService: IQMService;
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
    FViewModel.RunAsync;
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
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
  if FViewModel.VarCount > 5 then
  begin
    FViewModel.VarCount := 5;
    edtVars.Text := '5';
    ShowMessage('A változók száma túl nagy, csökkentve lett 5-re!');
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FService := TQMService.Create;
  FViewModel := TQMViewModel.Create(FService);

  // alapértelmezett értékek, hogy ne legyen üres
  FViewModel.MintermsText := '0,1,2,5,6,7,8,9,10,14';
  FViewModel.DontCaresText := '2,9,14'; // lehet üres
  FViewModel.VarCount := 4;

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
