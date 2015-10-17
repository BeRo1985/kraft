unit UnitFormGL;
{$j+}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OpenGL, sSkinProvider, kraft, ExtCtrls, Math, mmsystem;

type TCamera=object
      public
       LeftRight:TKraftScalar;
       UpDown:TKraftScalar;
       Position:TKraftVector3;
       Orientation:TKraftQuaternion;
       Matrix:TKraftMatrix4x4;
       FOV:TKraftScalar;
       procedure Reset;
       procedure MoveForwards(Speed:TKraftScalar);
       procedure MoveSidewards(Speed:TKraftScalar);
       procedure MoveUpwards(Speed:TKraftScalar);
       procedure RotateCamera(const x,y:TKraftScalar);
       procedure TestCamera;
       procedure Interpolate(const a,b:TCamera;const t:TKraftScalar);
     end;

  TFormGL = class(TForm)
    sSkinProvider1: TsSkinProvider;
    TimerDraw: TTimer;
    TimerFPS: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerDrawTimer(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TimerFPSTimer(Sender: TObject);
  private
    { Private declarations }
  protected
   procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
   procedure WMGetDlgCode(var msg: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    { Public declarations }
    PixelFormat,Zoom,MaxOrder:integer;
    hDCGL:longword;
    hGL:THandle;
    ReadyGL,FollowPlayer:boolean;
    CurrentCamera,LastCamera,InterpolatedCamera:TCamera;
    LastTime,NowTime,DeltaTime,FST2,FET2,Frames:int64;
    FPS:double;
    FloatDeltaTime:double;
    TimeAccumulator:double;
    LastMouseX,LastMouseY:longint;
    Grabbing,Rotating:boolean;
    KeyLeft,KeyRight,KeyBackwards,KeyForwards,KeyUp,KeyDown:boolean;
    HighResolutionTimer:TKraftHighResolutionTimer;
    procedure InitGL;
    procedure DoneGL;
    procedure Draw;
  //  function CheckCamera(const OldPosition,NewPosition:TKraftVector3;var Delta:TKraftVector3):boolean;
//   procedure TestCamera;
  end;

var
  FormGL: TFormGL;

implementation

{$R *.dfm}

uses UnitFormMain;

var GrabRigidBody:TKraftRigidBody;
    GrabShape:TKraftShape;
    GrabDelta:TKraftVector3;
    GrabDistance:single;
    GrabRigidBodyTransform:TKraftMatrix4x4;
    GrabCameraTransform:TKraftMatrix4x4;
    GrabConstraint:TKraftConstraintJointGrab;

procedure StartGrab;
var Point,Normal:TKraftVector3;
    s:TKraftShape;
    t:single;
begin
 GrabRigidBody:=nil;
 GrabShape:=nil;
 if assigned(FormMain.KraftPhysics) then begin
  if FormMain.KraftPhysics.RayCast(FormGL.CurrentCamera.Position,PKraftVector3(pointer(@FormGL.CurrentCamera.Matrix[2,0]))^,1024.0,s,t,Point,Normal) then begin
   if assigned(s) and assigned(s.RigidBody) and (s.RigidBody.RigidBodyType=krbtDYNAMIC) then begin
    GrabRigidBody:=s.RigidBody;
    GrabShape:=s;
    GrabDelta:=Vector3Sub(s.RigidBody.Sweep.c,FormGL.CurrentCamera.Position);
    GrabDistance:=Vector3Dist(s.RigidBody.Sweep.c,FormGL.CurrentCamera.Position);
    GrabConstraint:=TKraftConstraintJointGrab.Create(FormMain.KraftPhysics,GrabRigidBody,Point,5.0,0.7,GrabRigidBody.Mass*1000.0);
    GrabRigidBody.SetToAwake;
   end;
  end;
 end;
end;

procedure StopGrab;
begin
 if assigned(GrabRigidBody) then begin
  GrabRigidBody.SetToAwake;
  FreeAndNil(GrabConstraint);
 end;
 GrabRigidBody:=nil;
 GrabShape:=nil;
 GrabConstraint:=nil;
end;

procedure ProcessGrab;
begin
 if assigned(GrabRigidBody) and assigned(GrabConstraint) then begin
  GrabConstraint.SetWorldPoint(Vector3Add(FormGL.CurrentCamera.Position,Vector3ScalarMul(PKraftVector3(pointer(@FormGL.CurrentCamera.Matrix[2,0]))^,GrabDistance)));
  GrabRigidBody.SetToAwake;
 end;
end;

procedure TCamera.Reset;
begin
 LeftRight:=1.0;
 UpDown:=0.0;
 Position:=Vector3(0.0,2.0,4.0);
 Orientation:=QuaternionFromAngles(LeftRight*pi,0.0,UpDown*pi);
 Matrix:=QuaternionToMatrix4x4(Orientation);
 FOV:=90.0;
end;

procedure TCamera.MoveForwards(Speed:TKraftScalar);
begin
 Position:=Vector3Add(Position,Vector3ScalarMul(PKraftVector3(pointer(@Matrix[2,0]))^,Speed));
end;

procedure TCamera.MoveSidewards(Speed:TKraftScalar);
begin
 Position:=Vector3Add(Position,Vector3ScalarMul(PKraftVector3(pointer(@Matrix[0,0]))^,Speed));
end;

procedure TCamera.MoveUpwards(Speed:TKraftScalar);
begin
 Position:=Vector3Add(Position,Vector3ScalarMul(PKraftVector3(pointer(@Matrix[1,0]))^,Speed));
end;

procedure TCamera.RotateCamera(const x,y:TKraftScalar);
begin
 LeftRight:=LeftRight+x;
 UpDown:=Min(Max(UpDown-y,-0.5),0.5);
 Orientation:=QuaternionFromAngles(LeftRight*pi,0.0,UpDown*pi);
{Orientation:=QuaternionTermNormalize(QuaternionMul(QuaternionMul(QuaternionFromAxisAngle(Vector3XAxis,y),
                                                                  Orientation),
                                                    QuaternionFromAxisAngle(Vector3YAxis,x)));{}
 Matrix:=QuaternionToMatrix4x4(Orientation);
end;

procedure TCamera.Interpolate(const a,b:TCamera;const t:TKraftScalar);
begin
 Position:=Vector3Lerp(a.Position,b.Position,t);
 Orientation:=QuaternionSlerp(a.Orientation,b.Orientation,t);
 Matrix:=QuaternionToMatrix4x4(Orientation);
 FOV:=(a.FOV*(1.0-t))+(b.FOV*t);
end;

procedure TCamera.TestCamera;
begin
 if assigned(FormMain.KraftPhysics) then begin
  FormMain.KraftPhysics.PushSphere(Position,0.5);
 end;
end;

procedure DrawObjectTreeNode(Tree:TKraftDynamicAABBTree;Node:PKraftDynamicAABBTreeNode);
var
 I: integer;
begin
//glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
 glBegin(GL_LINE_STRIP);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
 glVertex3f(Node^.AABB.Max.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
 glVertex3f(Node^.AABB.Max.X,Node^.AABB.Max.Y,Node^.AABB.Min.Z);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Max.Y,Node^.AABB.Min.Z);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
 glVertex3f(Node^.AABB.Max.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
 glVertex3f(Node^.AABB.Max.X,Node^.AABB.Max.Y,Node^.AABB.Max.Z);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Max.Y,Node^.AABB.Max.Z);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Max.Y,Node^.AABB.Min.Z);
 glVertex3f(Node^.AABB.Min.X,Node^.AABB.Max.Y,Node^.AABB.Max.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(Node^.AABB.Max.X,Node^.AABB.Max.Y,Node^.AABB.Min.Z);
 glVertex3f(Node^.AABB.Max.X,Node^.AABB.Max.Y,Node^.AABB.Max.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(Node^.AABB.Max.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
 glVertex3f(Node^.AABB.Max.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
 glEnd;
//glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
 for I:=0 to 1 do begin
  if Node^.Children[I]>=0 then begin
   DrawObjectTreeNode(Tree,@Tree.Nodes[Node^.Children[I]]);
  end;
 end;
end;

{procedure DrawMeshTree(Shape:TKraftShapeMesh;const CameraMatrix:TKraftMatrix4x4);
var i:integer;
    Node:PKraftMeshSkipListNode;
    ModelViewMatrix:TKraftMatrix4x4;
begin
 glMatrixMode(GL_MODELVIEW);
 ModelViewMatrix:=Matrix4x4TermMul(Shape.WorldTransform,CameraMatrix);
 glLoadMatrixf(pointer(@ModelViewMatrix));
 if DrawMeshTreeDisplayList=0 then begin
  DrawMeshTreeDisplayList:=glGenLists(1);
  glNewList(DrawMeshTreeDisplayList,GL_COMPILE);

  for i:=0 to Shape.Mesh.CountSkipListNodes-1 do begin
   Node:=@Shape.Mesh.SkipListNodes[i];
   glBegin(GL_LINE_STRIP);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
   glVertex3f(Node^.AABB.Max.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
   glVertex3f(Node^.AABB.Max.X,Node^.AABB.Max.Y,Node^.AABB.Min.Z);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Max.Y,Node^.AABB.Min.Z);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
   glEnd;
   glBegin(GL_LINE_STRIP);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
   glVertex3f(Node^.AABB.Max.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
   glVertex3f(Node^.AABB.Max.X,Node^.AABB.Max.Y,Node^.AABB.Max.Z);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Max.Y,Node^.AABB.Max.Z);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
   glEnd;
   glBegin(GL_LINE_STRIP);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
   glEnd;
   glBegin(GL_LINE_STRIP);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Max.Y,Node^.AABB.Min.Z);
   glVertex3f(Node^.AABB.Min.X,Node^.AABB.Max.Y,Node^.AABB.Max.Z);
   glEnd;
   glBegin(GL_LINE_STRIP);
   glVertex3f(Node^.AABB.Max.X,Node^.AABB.Max.Y,Node^.AABB.Min.Z);
   glVertex3f(Node^.AABB.Max.X,Node^.AABB.Max.Y,Node^.AABB.Max.Z);
   glEnd;
   glBegin(GL_LINE_STRIP);
   glVertex3f(Node^.AABB.Max.X,Node^.AABB.Min.Y,Node^.AABB.Min.Z);
   glVertex3f(Node^.AABB.Max.X,Node^.AABB.Min.Y,Node^.AABB.Max.Z);
   glEnd;
  end;

  glEndList;
 end;

 if DrawMeshTreeDisplayList<>0 then begin
  glCallList(DrawMeshTreeDisplayList);
 end;
end;}

procedure DrawObjectAABB(const AABB:TKraftAABB);
begin
//glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
 glBegin(GL_LINE_STRIP);
 glVertex3f(AABB.Min.X,AABB.Min.Y,AABB.Min.Z);
 glVertex3f(AABB.Max.X,AABB.Min.Y,AABB.Min.Z);
 glVertex3f(AABB.Max.X,AABB.Max.Y,AABB.Min.Z);
 glVertex3f(AABB.Min.X,AABB.Max.Y,AABB.Min.Z);
 glVertex3f(AABB.Min.X,AABB.Min.Y,AABB.Min.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(AABB.Min.X,AABB.Min.Y,AABB.Max.Z);
 glVertex3f(AABB.Max.X,AABB.Min.Y,AABB.Max.Z);
 glVertex3f(AABB.Max.X,AABB.Max.Y,AABB.Max.Z);
 glVertex3f(AABB.Min.X,AABB.Max.Y,AABB.Max.Z);
 glVertex3f(AABB.Min.X,AABB.Min.Y,AABB.Max.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(AABB.Min.X,AABB.Min.Y,AABB.Min.Z);
 glVertex3f(AABB.Min.X,AABB.Min.Y,AABB.Max.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(AABB.Min.X,AABB.Max.Y,AABB.Min.Z);
 glVertex3f(AABB.Min.X,AABB.Max.Y,AABB.Max.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(AABB.Max.X,AABB.Max.Y,AABB.Min.Z);
 glVertex3f(AABB.Max.X,AABB.Max.Y,AABB.Max.Z);
 glEnd;
 glBegin(GL_LINE_STRIP);
 glVertex3f(AABB.Max.X,AABB.Min.Y,AABB.Min.Z);
 glVertex3f(AABB.Max.X,AABB.Min.Y,AABB.Max.Z);
 glEnd;
end;


const PFD:TPixelFormatDescriptor=(nSize:sizeof(TPixelFormatDescriptor);nVersion:1;
                                  dwFlags:PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
                                  iPixelType:PFD_TYPE_RGBA;
                                  cColorBits:24;cDepthBits:24;cStencilBits:8;iLayerType:PFD_MAIN_PLANE;);

procedure TFormGL.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
 msg.result := 1;
end;

procedure TFormGL.WMGetDlgCode(var msg: TWMGetDlgCode);
begin
 msg.result := msg.result or DLGC_WANTCHARS or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

procedure TFormGL.InitGL;
begin
 try
  if not ReadyGL then begin
   hDCGL:=GetDC(Handle);
   PixelFormat:=ChoosePixelFormat(hDCGL,@PFD);
   SetPixelFormat(hDCGL,PixelFormat,@PFD);
   DescribePixelFormat(hDCGL,PixelFormat,SizeOf(TPixelFormatDescriptor),PFD);
   hGL:=wglCreateContext(hDCGL);
   wglMakeCurrent(hDCGL,hGL);
   ReadyGL:=true;
  end;
 except
  ReadyGL:=false;
 end;
end;

procedure TFormGL.DoneGL;
begin
 try
  if ReadyGL then begin
   wglMakeCurrent(hDCGL,0);
   wglDeleteContext(hGL);
   ReleaseDC(Handle,hDCGL);
   ReadyGL:=false;
  end;
 except
  ReadyGL:=false;
 end;
end;

procedure TFormGL.Draw;
const
 GlobalAmbient: array[0..3] of GLFLOAT = (0.025,0.025,0.025,1);
 Licht0Ambient: array[0..3] of GLFLOAT = (0.2,0.2,0.2,1);
 Licht0Diffuse: array[0..3] of GLFLOAT = (0.8,0.8,0.8,1);
 Licht0Specular: array[0..3] of GLFLOAT = (0.025,0.025,0.025,1);
 LModellAmbient: array[0..3] of GLFLOAT = (0.025,0.025,0.025,1);
 Licht0Pos: array[0..3] of GLFLOAT = (0.0,160.0,160.0,1.0);
 MaterialDiffuse: array[0..3] of GLFLOAT = (0.8,0.8,0.8,1);
 MaterialSpecular: array[0..3] of GLFLOAT = (0.1,0.1,0.1,1);
 MaterialAmbient: array[0..3] of GLFLOAT = (0.2,0.2,0.2,1);
 MaterialShininess: array[0..3] of GLFLOAT = (0.1,0.1,0.1,1);
var i:longint;
    m:TKraftMatrix4x4;
    v:TKraftVector4;
    vv:TKraftVector3;
    RigidBody:TKraftRigidBody;
    Shape:TKraftShape;
    PhysicsTimeStep:double;
    Constraint:TKraftConstraint;
begin

 if assigned(FormMain.KraftPhysics) then begin

  NowTime:=HighResolutionTimer.GetTime;
  DeltaTime:=NowTime-LastTime;
  LastTime:=NowTime;

  FloatDeltaTime:=Min(Max(HighResolutionTimer.ToFloatSeconds(DeltaTime),0.0),1.0);

  PhysicsTimeStep:=1.0/FormMain.KraftPhysics.WorldFrequency;

  TimeAccumulator:=TimeAccumulator+FloatDeltaTime;
  while TimeAccumulator>=PhysicsTimeStep do begin
   TimeAccumulator:=TimeAccumulator-PhysicsTimeStep;
   LastCamera:=CurrentCamera;
   if Grabbing then begin
    ProcessGrab;
   end;
   FormMain.KraftPhysics.StoreWorldTransforms;
   FormMain.KraftPhysics.Step(PhysicsTimeStep);
   CurrentCamera.TestCamera;
   if KeyLeft then begin
    CurrentCamera.MoveSidewards(PhysicsTimeStep*10.0);
   end;
   if KeyRight then begin
    CurrentCamera.MoveSidewards(-(PhysicsTimeStep*10.0));
   end;
   if KeyForwards then begin
    CurrentCamera.MoveForwards(PhysicsTimeStep*10.0);
   end;
   if KeyBackwards then begin
    CurrentCamera.MoveForwards(-(PhysicsTimeStep*10.0));
   end;
   if KeyUp then begin
    CurrentCamera.MoveUpwards(PhysicsTimeStep*10.0);
   end;
   if KeyDown then begin
    CurrentCamera.MoveUpwards(-(PhysicsTimeStep*10.0));
   end;
   CurrentCamera.TestCamera;
  end;
  FormMain.KraftPhysics.InterpolateWorldTransforms(TimeAccumulator/PhysicsTimeStep);
  InterpolatedCamera.Interpolate(LastCamera,CurrentCamera,TimeAccumulator/PhysicsTimeStep);

  inc(Frames);
  if abs(FST2-NowTime)>=HighResolutionTimer.Frequency then begin
   FET2:=FST2;
   FST2:=NowTime;
   if (FST2-FET2)<>0 then begin
    FPS:=(Frames*HighResolutionTimer.Frequency)/(FST2-FET2);
   end;
   Frames:=0;
  end;

  wglMakeCurrent(hDCGL,hGL);
  glViewPort(0,0,ClientWidth,ClientHeight);
  glClearDepth(1.0);
  glClearColor(0.0,0.0,0.0,0.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

 (**)
  glColor4f(0.0,0.0,0.0,0.0);//0.1725,0.3275,0.6275,1.0);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glDepthMask(GL_FALSE);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
 {}glBegin(GL_QUADS);
  glVertex3f(1.0,-1.0,0.0);
  glVertex3f(1.0,1.0,0.0);
  glVertex3f(-1.0,1.0,0.0);
  glVertex3f(-1.0,-1.0,0.0);
  glEnd;{}
  glDepthMask(GL_TRUE);
  glMatrixMode(GL_PROJECTION);
  m:=Matrix4x4Perspective(InterpolatedCamera.FOV,ClientWidth/ClientHeight,0.1,1024.0);
  glLoadMatrixf(pointer(@m));
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  glCullFace(GL_BACK);
  glDisable(GL_BLEND);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT,@LModellAmbient);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT,@GlobalAmbient);
  m:=Matrix4x4LookAt(InterpolatedCamera.Position,Vector3Add(InterpolatedCamera.Position,PKraftVector3(pointer(@InterpolatedCamera.Matrix[2,0]))^),PKraftVector3(pointer(@InterpolatedCamera.Matrix[1,0]))^);
  v:=PKraftVector4(pointer(@Licht0Pos))^;
  Vector4MatrixMul(v,m);
  glLightfv(GL_LIGHT0,GL_POSITION,@v);
  glLightfv(GL_LIGHT0,GL_AMBIENT,@Licht0Ambient);
  glLightfv(GL_LIGHT0,GL_DIFFUSE,@Licht0Diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,@Licht0Specular);

  glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,@MaterialDiffuse);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,@MaterialSpecular);
  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,@MaterialAmbient);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SHININESS,@MaterialShininess);

  glShadeModel(GL_SMOOTH);

  glDepthFunc(GL_LEQUAL);
  glCullFace(GL_BACK);

  glEnable(GL_LIGHTING);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glLoadMatrixf(@m);

  i:=0;
  glEnable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  if FormMain.sCheckBoxDrawSolid.Checked then begin
   RigidBody:=FormMain.KraftPhysics.RigidBodyFirst;
   while assigned(RigidBody) do begin
    if RigidBody.IsStatic then begin
     glColor4f(0.75,0.5,0.125,1);
    end else if krbfAwake in RigidBody.Flags then begin
     glColor4f(1.0,0.0,1.0,1.0);
    end else begin
     glColor4f(1.0,0.0,0.0,1.0);
    end;
    Shape:=RigidBody.ShapeFirst;
    while assigned(Shape) do begin
     Shape.Draw(m);
     Shape:=Shape.ShapeNext;
    end;
    RigidBody:=RigidBody.RigidBodyNext;
   end;
  end;

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glDisable(GL_LIGHTING);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glLoadMatrixf(@m);
  glLineWidth(2);
  glPolygonOffset(-1,1);
  if FormMain.sCheckBoxDrawDynamicAABBTree.Checked then begin
   if FormMain.KraftPhysics.StaticAABBTree.Root>=0 then begin
    glColor4f(0.5,0.5,1.0,0.75);
    DrawObjectTreeNode(FormMain.KraftPhysics.StaticAABBTree,@FormMain.KraftPhysics.StaticAABBTree.Nodes[FormMain.KraftPhysics.StaticAABBTree.Root]);
   end;
   glPolygonOffset(-2,2);
   if FormMain.KraftPhysics.SleepingAABBTree.Root>=0 then begin
    glColor4f(1.0,0.5,0.5,0.75);
    DrawObjectTreeNode(FormMain.KraftPhysics.SleepingAABBTree,@FormMain.KraftPhysics.SleepingAABBTree.Nodes[FormMain.KraftPhysics.SleepingAABBTree.Root]);
   end;
   glPolygonOffset(-3,3);
   if FormMain.KraftPhysics.DynamicAABBTree.Root>=0 then begin
    glColor4f(0.5,1.0,0.5,0.75);
    DrawObjectTreeNode(FormMain.KraftPhysics.DynamicAABBTree,@FormMain.KraftPhysics.DynamicAABBTree.Nodes[FormMain.KraftPhysics.DynamicAABBTree.Root]);
   end;
   glPolygonOffset(-4,4);
   if FormMain.KraftPhysics.KinematicAABBTree.Root>=0 then begin
    glColor4f(1.0,0.5,1.0,0.75);
    DrawObjectTreeNode(FormMain.KraftPhysics.KinematicAABBTree,@FormMain.KraftPhysics.KinematicAABBTree.Nodes[FormMain.KraftPhysics.KinematicAABBTree.Root]);
   end;
   glPolygonOffset(-5,5);
 //  DrawObjectAABB(ShapeMesh.WorldAABB);
   glLineWidth(1);
   RigidBody:=FormMain.KraftPhysics.RigidBodyFirst;
   while assigned(RigidBody) do begin
    Shape:=RigidBody.ShapeFirst;
    while assigned(Shape) do begin
     if krbfAwake in Shape.RigidBody.Flags then begin
      glColor4f(1.0,1.0,1.0,0.75);
      DrawObjectAABB(Shape.WorldAABB);
 {   end else begin
      glColor4f(1.0,0.75,0.75,0.75);{}
     end;
     Shape:=Shape.ShapeNext;
    end;
    RigidBody:=RigidBody.RigidBodyNext;
   end;
  end;
  glPolygonOffset(-1,1);
  glLineWidth(1);

  glEnable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
  glEnable(GL_POLYGON_OFFSET_LINE);
  glEnable(GL_POLYGON_OFFSET_POINT);
  glPolygonOffset(-8,8);
  glPointSize(1);
  glLineWidth(1);
  if FormMain.sCheckBoxDrawWireFrame.Checked then begin
   RigidBody:=FormMain.KraftPhysics.RigidBodyFirst;
   while assigned(RigidBody) do begin
    Shape:=RigidBody.ShapeFirst;
    while assigned(Shape) do begin
     glColor4f(1.0,1.0,1.0,1.0);
     Shape.Draw(m);
     Shape:=Shape.ShapeNext;
    end;
    RigidBody:=RigidBody.RigidBodyNext;
   end;
  end;
  glDisable(GL_POLYGON_OFFSET_LINE);
  glDisable(GL_POLYGON_OFFSET_POINT);

  glDisable(GL_LIGHTING);
  glEnable(GL_POLYGON_OFFSET_LINE);
  glEnable(GL_POLYGON_OFFSET_POINT);
  glPolygonOffset(-8,8);
  glPointSize(8);
  glLineWidth(4);
  glDisable(GL_DEPTH_TEST);
  if FormMain.sCheckBoxDrawContacts.Checked then begin
   FormMain.KraftPhysics.ContactManager.DebugDraw(m);
  end;
  glDisable(GL_POLYGON_OFFSET_LINE);
  glDisable(GL_POLYGON_OFFSET_POINT);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glEnable(GL_DEPTH_TEST);
  if FormMain.sCheckBoxDrawConstraints.Checked then begin
   Constraint:=FormMain.KraftPhysics.ConstraintFirst;
   while assigned(Constraint) do begin
    if assigned(Constraint.RigidBodies[0]) and
       assigned(Constraint.RigidBodies[1]) then begin
     glLineWidth(5);
     glColor4f(1.0,1.0,0.125,1.0);
     glBegin(GL_LINE_STRIP);
     vv:=Vector3TermMatrixMul(PKraftVector3(pointer(@Constraint.RigidBodies[0].ShapeFirst.InterpolatedWorldTransform[3,0]))^,m);
     glVertex3fv(@vv);
     vv:=Vector3TermMatrixMul(PKraftVector3(pointer(@Constraint.RigidBodies[1].ShapeFirst.InterpolatedWorldTransform[3,0]))^,m);
     glVertex3fv(@vv);
     glEnd;
    end;
    Constraint:=Constraint.Next;
   end;
  end;
  glDisable(GL_DEPTH_TEST);

  if FormMain.sCheckBoxDrawConstraints.Checked then begin
   if assigned(GrabRigidBody) then begin
    glLineWidth(10);
    glColor4f(1.0,1.0,0.125,1.0);
    glBegin(GL_LINE_STRIP);
    vv:=Vector3TermMatrixMul(GrabConstraint.GetWorldPoint,m);
    glVertex3fv(@vv);
    vv:=Vector3TermMatrixMul(GrabConstraint.GetAnchor,m);
    glVertex3fv(@vv);
    glEnd;
   end;
  end;

  glMatrixMode(GL_PROJECTION);
  glClear(GL_STENCIL_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glFrustum(-0.01,0.01,-0.0075,0.0075,0.01,1000.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glColor4f(1,1,1,1);
  glPointSize(2);
  glLineWidth(2);
  glTranslatef(0,0,-4);
  glBegin(GL_LINES);
  glVertex3f(-0.25,0,0);
  glVertex3f(0.25,0,0);
  glVertex3f(0,-0.25,0);
  glVertex3f(0,0.25,0);
  glEnd;
  glBegin(GL_LINE_STRIP);
  for i:=0 to 16 do begin
   glVertex3f(cos(i*pi/8)*0.125,sin(i*pi/8)*0.125,0);
  end;
  glEnd;
  glBegin(GL_LINE_STRIP);
  for i:=0 to 16 do begin
   glVertex3f(cos(i*pi/8)*0.06125*0.5,sin(i*pi/8)*0.06125*0.5,0);
  end;
  glEnd;
  glPointSize(1);
  glLineWidth(1);

  if Focused then begin
   glClear(GL_DEPTH_BUFFER_BIT);
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   glDisable(GL_BLEND);
   glColor4f(0.5,0.5,1.0,1.0);
   glLineWidth(3);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_LIGHTING);
   glDisable(GL_CULL_FACE);
   glDepthMask(GL_FALSE);
   glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
   glBegin(GL_QUADS);
   glVertex3f(1.0,-1.0,0.0);
   glVertex3f(1.0,1.0,0.0);
   glVertex3f(-1.0,1.0,0.0);
   glVertex3f(-1.0,-1.0,0.0);
   glEnd;
   glDisable(GL_BLEND);
  end else begin
   glClear(GL_DEPTH_BUFFER_BIT);
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
   glColor4f(0.0,0.0,0.0,0.25);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_LIGHTING);
   glDisable(GL_CULL_FACE);
   glDepthMask(GL_FALSE);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glBegin(GL_QUADS);
   glVertex3f(1.0,-1.0,0.0);
   glVertex3f(1.0,1.0,0.0);
   glVertex3f(-1.0,1.0,0.0);
   glVertex3f(-1.0,-1.0,0.0);
   glEnd;
   glDisable(GL_BLEND);
  end;

  SwapBuffers(hDCGL);

 end else begin

  wglMakeCurrent(hDCGL,hGL);
  glViewPort(0,0,ClientWidth,ClientHeight);
  glClearDepth(1.0);
  glClearColor(0.0,0.0,0.0,0.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  SwapBuffers(hDCGL);

 end;

end;

procedure TFormGL.FormCreate(Sender: TObject);
begin
 KeyLeft:=false;
 KeyRight:=false;
 KeyBackwards:=false;
 KeyForwards:=false;
 KeyUp:=false;
 KeyDown:=false;

 ReadyGL:=false;

 Grabbing:=false;

 Rotating:=false;

 CurrentCamera.Reset;
 LastCamera:=CurrentCamera;

 HighResolutionTimer:=TKraftHighResolutionTimer.Create;

 LastTime:=HighResolutionTimer.GetTime;

 FPS:=0.0;
 
 FST2:=LastTime;
 FET2:=LastTime;
 Frames:=0;

 TimeAccumulator:=0.0;

end;

procedure TFormGL.FormDestroy(Sender: TObject);
begin
 DoneGL;
 HighResolutionTimer.Free;
end;

procedure TFormGL.FormPaint(Sender: TObject);
begin
 Draw;
end;

procedure TFormGL.FormResize(Sender: TObject);
begin
 Draw;
end;

procedure TFormGL.FormShow(Sender: TObject);
begin
 InitGL;
end;

procedure TFormGL.TimerDrawTimer(Sender: TObject);
begin
 Draw;
end;

procedure TFormGL.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var xrel,yrel:longint;
begin
 if Rotating then begin
  xrel:=LastMouseX-x;
  yrel:=LastMouseY-y;
  if (xrel<>0) or (yrel<>0) then begin
   CurrentCamera.RotateCamera(xrel*0.002,yrel*0.002);
  end;
  if (x<100) or (y<100) or (x>=(ClientWidth-100)) or (y>=(ClientHeight-100)) then begin
   LastMouseX:=ClientWidth div 2;
   LastMouseY:=ClientHeight div 2;
   Mouse.CursorPos:=ClientToScreen(Point(ClientWidth div 2,ClientHeight div 2));
  end else begin
   LastMouseX:=x;
   LastMouseY:=y;
  end;
 end;
end;

procedure TFormGL.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 LastMouseX:=x;
 LastMouseY:=y;
 Rotating:=false;
 case Button of
  mbLeft:begin
   Rotating:=true;
// Cursor:=crNone;
   SetFocus;
  end;
  mbRight:begin
   Grabbing:=true;
   Rotating:=true;
// Cursor:=crNone;
   SetFocus;
   StartGrab;
  end;
 end;
end;

procedure TFormGL.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Grabbing then begin
  Grabbing:=false;
  StopGrab;
 end;
 if Rotating then begin
 //Cursor:=crDefault;
  Rotating:=false;
  KeyLeft:=false;
  KeyRight:=false;
  KeyBackwards:=false;
  KeyForwards:=false;
  KeyUp:=false;
  KeyDown:=false;
 end;
end;

procedure TFormGL.FormClick(Sender: TObject);
begin
 SetFocus;
end;

procedure TFormGL.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{if Rotating then}begin
  case Key of
   VK_LEFT,ord('A'):begin
    KeyLeft:=true;
   end;
   VK_RIGHT,ord('D'):begin
    KeyRight:=true;
   end;
   VK_UP,ord('W'):begin
    KeyForwards:=true;
   end;
   VK_DOWN,ord('S'):begin
    KeyBackwards:=true;
   end;
   VK_PRIOR,ord('R'):begin
    KeyUp:=true;
   end;
   VK_NEXT,ord('F'):begin
    KeyDown:=true;
   end;
  end;
 end;
end;

procedure TFormGL.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{if Rotating then{}begin
  case Key of
   VK_LEFT,ord('A'):begin
    KeyLeft:=false;
   end;
   VK_RIGHT,ord('D'):begin
    KeyRight:=false;
   end;
   VK_UP,ord('W'):begin
    KeyForwards:=false;
   end;
   VK_DOWN,ord('S'):begin
    KeyBackwards:=false;
   end;
   VK_PRIOR,ord('R'):begin
    KeyUp:=false;
   end;
   VK_NEXT,ord('F'):begin
    KeyDown:=false;
   end;
  end;
 end;
end;

procedure TFormGL.FormActivate(Sender: TObject);
begin
 Grabbing:=false;
 Rotating:=false;
 KeyLeft:=false;
 KeyRight:=false;
 KeyBackwards:=false;
 KeyForwards:=false;
 KeyUp:=false;
 KeyDown:=false;
 StopGrab;
end;

procedure TFormGL.FormDeactivate(Sender: TObject);
begin
 Grabbing:=false;
 Rotating:=false;
 KeyLeft:=false;
 KeyRight:=false;
 KeyBackwards:=false;
 KeyForwards:=false;
 KeyUp:=false;
 KeyDown:=false;
 StopGrab;
end;

procedure TFormGL.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 CurrentCamera.FOV:=Min(Max(CurrentCamera.FOV+(WheelDelta*0.01),15.0),160.0);
 Handled:=true;
end;

procedure TFormGL.TimerFPSTimer(Sender: TObject);
begin
 FormMain.sTabSheetWorld.Caption:='World ('+IntToStr(round(FPS))+' FPS)';
end;

initialization
 timeBeginPeriod(1);
finalization
 timeEndPeriod(1);
end.
