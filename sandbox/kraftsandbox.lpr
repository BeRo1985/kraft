program kraftsandbox;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
  kraft in '..\src\kraft.pas',
  UnitDemoScene in 'UnitDemoScene.pas',
  UnitDemoSceneBoxOnPlane in 'UnitDemoSceneBoxOnPlane.pas',
  UnitDemoSceneSandBox in 'UnitDemoSceneSandBox.pas',
  UnitDemoSceneBoxStacking in 'UnitDemoSceneBoxStacking.pas',
  UnitDemoSceneBoxPyramidStacking in 'UnitDemoSceneBoxPyramidStacking.pas',
  UnitDemoSceneBridge in 'UnitDemoSceneBridge.pas',
  UnitDemoSceneCombinedShapes in 'UnitDemoSceneCombinedShapes.pas',
  UnitDemoSceneChain in 'UnitDemoSceneChain.pas',
  UnitDemoSceneStrainedChain in 'UnitDemoSceneStrainedChain.pas',
  UnitDemoSceneBrickWall in 'UnitDemoSceneBrickWall.pas',
  UnitDemoSceneDomino in 'UnitDemoSceneDomino.pas',
  UnitDemoSceneChairAndTable in 'UnitDemoSceneChairAndTable.pas',
  UnitDemoSceneConvexHull in 'UnitDemoSceneConvexHull.pas',
  UnitDemoSceneCar in 'UnitDemoSceneCar.pas',
  PasMP in '..\..\..\PASMP.github\trunk\src\PasMP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
