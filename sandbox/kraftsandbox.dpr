program kraftsandbox;

uses
  Forms,
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
  kraft in '..\src\kraft.pas',
  UnitFormGL in 'UnitFormGL.pas' {FormGL},
  UnitDemoScene in 'UnitDemoScene.pas',
  UnitDemoSceneBoxOnPlane in 'UnitDemoSceneBoxOnPlane.pas',
  UnitDemoSceneSandBox in 'UnitDemoSceneSandBox.pas',
  UnitDemoSceneBoxStacking in 'UnitDemoSceneBoxStacking.pas',
  UnitDemoSceneBoxPyramidStacking in 'UnitDemoSceneBoxPyramidStacking.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
