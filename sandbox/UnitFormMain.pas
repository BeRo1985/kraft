unit UnitFormMain;
{$j+}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvExControls, JvInspector, JvComponentBase, Menus, kraft,
  sSkinProvider, sSkinManager, acTitleBar, ExtCtrls, sSplitter, sPanel,
  StdCtrls, sGroupBox, ComCtrls, sPageControl, sTreeView,
  OpenGL, sListBox, sMemo, UnitDemoScene, JvSimScope, sLabel, sCheckBox,
  PasMP;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    JvInspectorMain: TJvInspector;
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
    sTitleBar1: TsTitleBar;
    N1: TMenuItem;
    N2: TMenuItem;
    sPanelLeft: TsPanel;
    sSplitter1: TsSplitter;
    sPanelLeftTop: TsPanel;
    sSplitter2: TsSplitter;
    sPanelLeftBottom: TsPanel;
    sGroupBoxTree: TsGroupBox;
    sGroupBoxPropertyEditor: TsGroupBox;
    sPanelRight: TsPanel;
    sSplitter3: TsSplitter;
    sPanelMiddle: TsPanel;
    sPanelMiddleBottom: TsPanel;
    sSplitter4: TsSplitter;
    sPageControl1: TsPageControl;
    sTabSheetInfo: TsTabSheet;
    sPageControl2: TsPageControl;
    sTabSheetWorld: TsTabSheet;
    sTreeViewMain: TsTreeView;
    sPanelOpenGL: TsPanel;
    sGroupBoxDemos: TsGroupBox;
    sMemoInfo: TsMemo;
    N3: TMenuItem;
    Skinned1: TMenuItem;
    sTreeViewDemos: TsTreeView;
    sTabSheetPerformance: TsTabSheet;
    sLabelBroadPhaseTime: TsLabel;
    TimerPerformance: TTimer;
    sLabelMidPhaseTime: TsLabel;
    sLabelNarrowPhaseTime: TsLabel;
    sLabelSolverTime: TsLabel;
    sLabelContinuousTime: TsLabel;
    sLabelTotalTime: TsLabel;
    sTabSheetSettings: TsTabSheet;
    sCheckBoxDrawDynamicAABBTree: TsCheckBox;
    sCheckBoxDrawContacts: TsCheckBox;
    sCheckBoxDrawWireFrame: TsCheckBox;
    sCheckBoxDrawSolid: TsCheckBox;
    sCheckBoxDrawConstraints: TsCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure sTreeViewMainChange(Sender: TObject; Node: TTreeNode);
    procedure JvInspectorMainItemDoubleClicked(Sender: TObject;
      Item: TJvCustomInspectorItem);
    procedure JvInspectorMainAfterItemCreate(Sender: TObject;
      Item: TJvCustomInspectorItem);
    procedure Skinned1Click(Sender: TObject);
    procedure sListBoxDemosClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sTreeViewDemosDblClick(Sender: TObject);
    procedure JvSimScope1Update(Sender: TObject);
    procedure TimerPerformanceTimer(Sender: TObject);
    procedure sTreeViewDemosKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    PasMPInstance:TPasMP;
    KraftPhysics:TKraft;
    TreeNodeKraftPhysics:TTreeNode;
    TreeNodeDemos:TTreeNode;
    TreeNodeDemoDefault:TTreeNode;
    DemoScene:TDemoScene;
    procedure AddRigidBody(RigidBody:TKraftRigidBody);
    procedure AddConstraint(Constraint:TKraftConstraint);
    procedure LoadScene(DemoSceneClass:TDemoSceneClass);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses UnitFormGL;

procedure TFormMain.AddRigidBody(RigidBody:TKraftRigidBody);
var TreeNodeRigidBody,TreeNodeRigidBodyShape:TTreeNode;
    Shape:TKraftShape;
begin
 TreeNodeRigidBody:=sTreeViewMain.Items.AddChildObject(TreeNodeKraftPhysics,RigidBody.ClassName,RigidBody);
 Shape:=RigidBody.ShapeFirst;
 while assigned(Shape) do begin
  TreeNodeRigidBodyShape:=sTreeViewMain.Items.AddChildObject(TreeNodeRigidBody,Shape.ClassName,Shape);
  if assigned(TreeNodeRigidBodyShape) then begin
  end;
  Shape:=Shape.ShapeNext;
 end;
end;

procedure TFormMain.AddConstraint(Constraint:TKraftConstraint);
var Index:longint;
    TreeNodeConstraint,TreeNodeRigidBody,TreeNodeRigidBodyShape:TTreeNode;
    RigidBody:TKraftRigidBody;
    Shape:TKraftShape;
begin
 TreeNodeConstraint:=sTreeViewMain.Items.AddChildObject(TreeNodeKraftPhysics,Constraint.ClassName,Constraint);
 for Index:=0 to length(Constraint.RigidBodies)-1 do begin
  RigidBody:=Constraint.RigidBodies[Index];
  if assigned(RigidBody) then begin
   TreeNodeRigidBody:=sTreeViewMain.Items.AddChildObject(TreeNodeConstraint,RigidBody.ClassName,RigidBody);
   Shape:=RigidBody.ShapeFirst;
   while assigned(Shape) do begin
    TreeNodeRigidBodyShape:=sTreeViewMain.Items.AddChildObject(TreeNodeRigidBody,Shape.ClassName,Shape);
    if assigned(TreeNodeRigidBodyShape) then begin
    end;
    Shape:=Shape.ShapeNext;
   end;
  end;
 end;
end;

procedure TFormMain.LoadScene(DemoSceneClass:TDemoSceneClass);
var RigidBody:TKraftRigidBody;
    Constraint:TKraftConstraint;
begin

 sTreeViewMain.Items.BeginUpdate;
 try

  TreeNodeKraftPhysics:=nil;

  JvInspectorMain.InspectObject:=nil;
  sTreeViewMain.Items.Clear;

  FreeAndNil(DemoScene);

  DemoScene:=DemoSceneClass.Create(KraftPhysics);

  TreeNodeKraftPhysics:=sTreeViewMain.Items.AddObjectFirst(nil,'TKraft',KraftPhysics);

  RigidBody:=KraftPhysics.RigidBodyFirst;
  while assigned(RigidBody) do begin
   AddRigidBody(RigidBody);
   RigidBody:=RigidBody.RigidBodyNext;
  end;

  Constraint:=KraftPhysics.ConstraintFirst;
  while assigned(Constraint) do begin
   AddConstraint(Constraint);
   Constraint:=Constraint.Next;
  end;

  sTreeViewMain.Selected:=TreeNodeKraftPhysics;
  TreeNodeKraftPhysics.Expand(true);
  JvInspectorMain.InspectObject:=KraftPhysics;

  FormGL.CurrentCamera.Reset;
  FormGL.LastCamera:=FormGL.CurrentCamera;

 finally
  sTreeViewMain.Items.EndUpdate;
 end;

 KraftPhysics.StoreWorldTransforms;
 KraftPhysics.InterpolateWorldTransforms(0.0);

 FormGL.LastTime:=FormGL.HighResolutionTimer.GetTime;

end;

procedure TFormMain.FormCreate(Sender: TObject);
var Index:longint;
begin

 DemoScene:=nil;

 PasMPInstance:=TPasMP.Create(-1,0,false);

{$ifdef KraftPasMP}
 KraftPhysics:=TKraft.Create(PasMPInstance);
{$else}
 KraftPhysics:=TKraft.Create(-1);
{$endif}

 KraftPhysics.SetFrequency(120.0);

 KraftPhysics.VelocityIterations:=8;

 KraftPhysics.PositionIterations:=3;

 KraftPhysics.SpeculativeIterations:=8;

 KraftPhysics.TimeOfImpactIterations:=20;

 KraftPhysics.Gravity.y:=-9.81;
 
 FormGL:=TFormGL.Create(sPanelOpenGL);
 FormGL.BorderStyle:=bsNone;
 FormGL.Align:=alClient;
 FormGL.Parent:=sPanelOpenGL;
 FormGL.Visible:=true;

 sTreeViewDemos.Items.BeginUpdate;
 try
  DemoScenes.Sort;
  TreeNodeDemos:=sTreeViewDemos.Items.AddChildFirst(nil,'Demos');
  for Index:=0 to DemoScenes.Count-1 do begin
   sTreeViewDemos.Items.AddChildObject(TreeNodeDemos,DemoScenes.Strings[Index],DemoScenes.Objects[Index]);
  end;
  TreeNodeDemoDefault:=TreeNodeDemos.GetFirstChild;
 finally
  sTreeViewDemos.Items.EndUpdate;
 end;
 TreeNodeDemos.Expand(true);

// LoadScene(TDemoSceneBoxOnPlane);

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
 FreeAndNil(DemoScene);
 FreeAndNil(KraftPhysics);
 FreeAndNil(PasMPInstance);
 FormGL.Free;
end;

procedure TFormMain.N2Click(Sender: TObject);
begin
 close;
end;

procedure TFormMain.sTreeViewMainChange(Sender: TObject; Node: TTreeNode);
var TreeNode:TTreeNode;
begin
 TreeNode:=sTreeViewMain.Selected;
 if assigned(TreeNode) then begin
  JvInspectorMain.InspectObject:=TObject(TreeNode.Data);
 end else begin
  JvInspectorMain.InspectObject:=nil;
 end;
end;

procedure TFormMain.JvInspectorMainItemDoubleClicked(Sender: TObject;
  Item: TJvCustomInspectorItem);
begin
{if assigned(Item) and assigned(Item.Data) then begin
  if Item.Data.TypeInfo=TypeInfo(TKraftVector3Property) then begin
  TJvInspectorClassItem(Item).
   JvInspectorMain.InspectObject:=TJvInspectorClassItem(Item).;
  end;
 end;}
end;

procedure TFormMain.JvInspectorMainAfterItemCreate(Sender: TObject;
  Item: TJvCustomInspectorItem);
begin
 if assigned(Item) and (Item is TJvInspectorClassItem) and assigned(Item.Data) then begin
  if Item.Data.TypeInfo=TypeInfo(TKraftVector3Property) then begin
   TJvInspectorClassItem(Item).RenderAsCategory:=true;
   TJvInspectorClassItem(Item).Expanded:=true;
  end;
 end;
end;

procedure TFormMain.Skinned1Click(Sender: TObject);
begin
 Skinned1.Checked:=not Skinned1.Checked;
 sSkinManager1.Active:=Skinned1.Checked;
end;

procedure TFormMain.sListBoxDemosClick(Sender: TObject);
begin
//LoadScene(TDemoSceneClass(sListBoxDemos.Items.Objects[sListBoxDemos.ItemIndex]));
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
 sTreeViewDemos.Selected:=TreeNodeDemoDefault;
 sTreeViewDemosDblClick(nil);
end;

procedure TFormMain.sTreeViewDemosDblClick(Sender: TObject);
var TreeNode:TTreeNode;
begin
 TreeNode:=sTreeViewDemos.Selected;
 if assigned(TreeNode) and assigned(TreeNode.Data) then begin
  LoadScene(TDemoSceneClass(TreeNode.Data));
 end;
end;

procedure TFormMain.JvSimScope1Update(Sender: TObject);
begin
 if assigned(KraftPhysics) then begin
 end;
end;

procedure TFormMain.TimerPerformanceTimer(Sender: TObject);
var s:string;
begin
 if assigned(KraftPhysics) then begin

  Str(KraftPhysics.HighResolutionTimer.ToNanoseconds(KraftPhysics.BroadPhaseTime)/1000000.0:1:5,s);
  sLabelBroadPhaseTime.Caption:='Broad phase: '+s+' ms';

  Str(KraftPhysics.HighResolutionTimer.ToNanoseconds(KraftPhysics.MidPhaseTime)/1000000.0:1:5,s);
  sLabelMidPhaseTime.Caption:='Mid phase: '+s+' ms';

  Str(KraftPhysics.HighResolutionTimer.ToNanoseconds(KraftPhysics.NarrowPhaseTime)/1000000.0:1:5,s);
  sLabelNarrowPhaseTime.Caption:='Narrow phase: '+s+' ms';

  Str(KraftPhysics.HighResolutionTimer.ToNanoseconds(KraftPhysics.SolverTime)/1000000.0:1:5,s);
  sLabelSolverTime.Caption:='Discrete solver: '+s+' ms';

  Str(KraftPhysics.HighResolutionTimer.ToNanoseconds(KraftPhysics.ContinuousTime)/1000000.0:1:5,s);
  sLabelContinuousTime.Caption:='Continuous collision detection and response: '+s+' ms';

  Str(KraftPhysics.HighResolutionTimer.ToNanoseconds(KraftPhysics.TotalTime)/1000000.0:1:5,s);
  sLabelTotalTime.Caption:='Total: '+s+' ms';

 end;

end;

procedure TFormMain.sTreeViewDemosKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=#13 then begin
  sTreeViewDemosDblClick(Sender);
 end;
end;

end.
