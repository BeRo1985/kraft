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
{$ifdef LCLQt6}
  qt6,
{$endif}
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
{$ifdef KraftPasMP}
  PasMP in '..\..\pasmp\src\PasMP.pas',
{$endif}
{$ifdef KraftPasJSON}
  PUCU in '..\..\pucu\src\PUCU.pas',
  PasDblStrUtils in '..\..\pasdblstrutils\src\PasDblStrUtils.pas',
  PasJSON in '..\..\pasjson\src\PasJSON.pas',
{$endif}
  kraft in '..\src\kraft.pas',
  KraftArcadeCarPhysics in '..\src\KraftArcadeCarPhysics.pas',
  KraftRayCastVehicle,
  UnitDemoScene in 'UnitDemoScene.pas',
  UnitDemoSceneCatapult in 'UnitDemoSceneCatapult.pas',
  UnitDemoSceneRoundabout in 'UnitDemoSceneRoundabout.pas',
  UnitDemoSceneCarousel in 'UnitDemoSceneCarousel.pas',
  UnitDemoSceneBoxOnPlane in 'UnitDemoSceneBoxOnPlane.pas',
  UnitDemoSceneSandBox in 'UnitDemoSceneSandBox.pas',
  UnitDemoSceneSphereOnSDFTerrain in 'UnitDemoSceneSphereOnSDFTerrain.pas',
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
  UnitDemoSceneSignedDistanceField in 'UnitDemoSceneSignedDistanceField.pas',
  UnitDemoSceneJoints in 'UnitDemoSceneJoints.pas',
  UnitDemoSceneRagdoll in 'UnitDemoSceneRagdoll.pas',
  UnitDemoSceneRagdoll6DOF in 'UnitDemoSceneRagdoll6DOF.pas',
  UnitDemoSceneConstraintVehicle in 'UnitDemoSceneConstraintVehicle.pas',
  UnitDemoSceneRaycastArcadeVehicle in 'UnitDemoSceneRaycastArcadeVehicle.pas',
  UnitDemoSceneRaycastVehicle;

{$R *.res}

var Index:longint;
    Value:string;

begin
  // --list-scenes prints the names which --scene takes and quits
  if GetCommandLineOption('list-scenes',Value) then begin
   DemoScenes.Sort;
   for Index:=0 to DemoScenes.Count-1 do begin
    WriteLn(DemoScenes.Strings[Index]);
   end;
   exit;
  end;
{$ifdef Windows}
  timeBeginPeriod(1);
{$endif}
  FormatSettings.DecimalSeparator:='.';
  FormatSettings.ThousandSeparator:=',';
  //Application.UpdateFormatSettings:=false;
{$ifdef LCLQt6}
  // The native OpenGL view would make its sibling widgets native windows too, the menu bar among them, whose
  // window Qt 6.12 only gets around to destroying in the exit handlers, after its thread storage is gone, so
  // the sandbox crashed on quitting
  QCoreApplication_setAttribute(QtAA_DontCreateNativeWidgetSiblings,true);
{$endif}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
{$ifdef Windows}
  timeEndPeriod(1);
{$endif}
end.
