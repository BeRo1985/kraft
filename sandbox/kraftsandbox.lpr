program kraftsandbox;

{$MODE Delphi}

uses
{$ifdef unix}
  cthreads,
{$endif}
{$ifdef Windows}
  Windows,
  MMSystem,
{$endif}
  SysUtils,
  Forms, Interfaces,
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
  kraft in '..\src\kraft.pas',
{$ifdef KraftPasMP}
  PasMP in '..\..\..\PASMP.github\trunk\src\PasMP.pas',
{$endif}
  UnitDemoScene in 'UnitDemoScene.pas',
  UnitDemoSceneCatapult in 'UnitDemoSceneCatapult.pas',
  UnitDemoSceneRoundabout in 'UnitDemoSceneRoundabout.pas',
  UnitDemoSceneCarousel in 'UnitDemoSceneCarousel.pas',
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
  UnitDemoSceneRaycastVehicle in 'UnitDemoSceneRaycastVehicle.pas';

{$R *.res}

begin
{$ifdef Windows}
  timeBeginPeriod(1);
{$endif}
  FormatSettings.DecimalSeparator:='.';
  FormatSettings.ThousandSeparator:=',';
  //Application.UpdateFormatSettings:=false;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
{$ifdef Windows}
  timeEndPeriod(1);
{$endif}
end.
