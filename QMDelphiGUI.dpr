program QMDelphiGUI;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  QMModel in 'Model\QMModel.pas',
  QMViewModel in 'ViewModel\QMViewModel.pas',
  QM.Helpers in 'Helpers\QM.Helpers.pas',
  QM.Service in 'Interface\QM.Service.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
