unit UnitDemoSceneRaycastVehicle;

{$MODE Delphi}

interface

uses LCLIntf,LCLType,LMessages,SysUtils,Classes,Math,Kraft,KraftArcadeCarPhysics,UnitDemoScene,gl,glext;

type { TDemoSceneRaycastVehicle }

     TDemoSceneRaycastVehicle=class(TDemoScene)
      public
       RigidBodyFloor:TKraftRigidBody;
       ShapeFloorPlane:TKraftShapePlane;
       Vehicle:TVehicle;
       CarSteering:double;
       CarSpeed:double;
       Time:double;
       InputKeyLeft,InputKeyRight,InputKeyUp,InputKeyDown,InputKeyBrake,InputKeyHandBrake:boolean;
       constructor Create(const aKraftPhysics:TKraft); override;
       destructor Destroy; override;
       procedure Step(const DeltaTime:double); override;
       procedure DebugDraw; override;
       function HasOwnKeyboardControls:boolean; override;
       procedure KeyDown(const aKey:Int32); override;
       procedure KeyUp(const aKey:Int32); override;
       function UpdateCamera(var aCameraPosition:TKraftVector3;var aCameraOrientation:TKraftQuaternion):boolean; override;
       procedure StoreWorldTransforms; override;
       procedure InterpolateWorldTransforms(const aAlpha:TKraftScalar); override;
     end;

implementation

uses UnitFormMain;

const CarWidth=1.8;
      CarLength=4.40;
      CarHeight=1.55;

      CarHalfWidth=CarWidth*0.5;

      WheelRadius=0.75;

      WheelY=-WheelRadius;

      ProtectionHeightOffset=4;

{ TDemoSceneRaycastVehicle }

constructor TDemoSceneRaycastVehicle.Create(const aKraftPhysics: TKraft);
var Shape:TKraftShape;
    DummyRigidBody:TKraftRigidBody;
begin
 inherited Create(AKraftPhysics);

 CarSteering:=0.0;
 Time:=0.0;

 RigidBodyFloor:=TKraftRigidBody.Create(KraftPhysics);
 RigidBodyFloor.SetRigidBodyType(krbtSTATIC);
 ShapeFloorPlane:=TKraftShapePlane.Create(KraftPhysics,RigidBodyFloor,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
 ShapeFloorPlane.Restitution:=0.3;
 RigidBodyFloor.Finish;
 RigidBodyFloor.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 RigidBodyFloor.CollisionGroups:=[0];

 Vehicle:=TVehicle.Create(KraftPhysics);

 Vehicle.DownForce:=10.0;

 Vehicle.FlightStabilizationForce:=6.0;
 Vehicle.FlightStabilizationDamping:=0.7;

 Vehicle.AxleFront.Width:=1.55;
 Vehicle.AxleFront.Offset:=Vector2(1.51,-0.5);
//Vehicle.AxleFront.Radius:=0.3;
 Vehicle.AxleFront.WheelVisualScale:=1.0;//2.9;
{Vehicle.AxleFront.Radius:=0.3;
 Vehicle.AxleFront.LaterialFriction:=0.6;
 Vehicle.AxleFront.RollingFriction:=0.03;
 Vehicle.AxleFront.SuspensionStiffness:=15000.0;
 Vehicle.AxleFront.SuspensionDamping:=3000.0;
 Vehicle.AxleFront.SuspensionRestitution:=1.0;
 Vehicle.AxleFront.RelaxedSuspensionLength:=0.45;
 Vehicle.AxleFront.StabilizerBarAntiRollForce:=10000.0;
 Vehicle.AxleFront.WheelVisualScale:=1.0;//2.9;
 Vehicle.AxleFront.AfterFlightSlipperyK:=0.02;
 Vehicle.AxleFront.BrakeSlipperyK:=0.5;
 Vehicle.AxleFront.HandBrakeSlipperyK:=0.3;}
 Vehicle.AxleFront.IsPowered:=false;

 Vehicle.AxleRear.Width:=1.55;
 Vehicle.AxleRear.Offset:=Vector2(-1.29,-0.5);
 Vehicle.AxleRear.WheelVisualScale:=1.0;//2.9;
{Vehicle.AxleRear.Radius:=0.3;
 Vehicle.AxleRear.LaterialFriction:=0.6;
 Vehicle.AxleRear.RollingFriction:=0.03;
 Vehicle.AxleRear.SuspensionStiffness:=9500.0;
 Vehicle.AxleRear.SuspensionDamping:=3000.0;
 Vehicle.AxleRear.SuspensionRestitution:=1.0;
 Vehicle.AxleRear.RelaxedSuspensionLength:=0.45;
 Vehicle.AxleRear.StabilizerBarAntiRollForce:=10000.0;
 Vehicle.AxleRear.WheelVisualScale:=1.0;//2.9;
 Vehicle.AxleRear.AfterFlightSlipperyK:=0.02;
 Vehicle.AxleRear.BrakeSlipperyK:=0.5;
 Vehicle.AxleRear.HandBrakeSlipperyK:=0.2; }
 Vehicle.AxleRear.IsPowered:=true;

 Vehicle.RigidBody:=TKraftRigidBody.Create(aKraftPhysics);
 Vehicle.RigidBody.SetRigidBodyType(krbtDYNAMIC);
 Vehicle.RigidBody.ForcedMass:=1500;
 Shape:=TKraftShapeBox.Create(aKraftPhysics,Vehicle.RigidBody,Vector3(CarHalfWidth,CarHeight*0.5,CarLength*0.5));
 Shape.Restitution:=0.3;
 Shape.Density:=200.0;
 Shape.LocalTransform:=Matrix4x4Translate(0.0,0.0,0.0);
// Shape.LocalCenterOfMass:=Vector3(0.0,-0.38,0.38);
{Shape:=TKraftShapeBox.Create(aKraftPhysics,Vehicle.RigidBody,Vector3(1.0,1.0,2.0));
 Shape.Restitution:=0.3;
 Shape.Density:=10.0;
 Shape.LocalTransform:=Matrix4x4Translate(0.0,1.75,1.0);}
 Vehicle.RigidBody.Finish;
 Vehicle.RigidBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateY(PI),Matrix4x4Translate(0.0,WheelRadius+CarHeight+2,0.0)));
 Vehicle.RigidBody.CollisionGroups:=[1];
 Vehicle.RigidBody.CollideWithCollisionGroups:=[0,1];
 Vehicle.RigidBody.AngularVelocityDamp:=10.0;//10.0;
 Vehicle.RigidBody.LinearVelocityDamp:=0.05;
{Vehicle.RigidBody.Flags:=Vehicle.RigidBody.Flags+[TKraftRigidBodyFlag.krbfHasOwnGravity];
 Vehicle.RigidBody.Gravity.x:=0.0;
 Vehicle.RigidBody.Gravity.y:=0.0;
 Vehicle.RigidBody.Gravity.z:=0.0;}

 Vehicle.Reset;

 InputKeyLeft:=false;
 InputKeyRight:=false;
 InputKeyUp:=false;
 InputKeyDown:=false;
 InputKeyBrake:=false;
 InputKeyHandBrake:=false;

 begin
  DummyRigidBody:=TKraftRigidBody.Create(KraftPhysics);
  DummyRigidBody.SetRigidBodyType(krbtSTATIC);
  Shape:=TKraftShapeBox.Create(KraftPhysics,DummyRigidBody,Vector3(4.0,25.0,2.0));
  Shape.Restitution:=0.3;
  DummyRigidBody.Finish;
  DummyRigidBody.CollisionGroups:=[0];
  DummyRigidBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateX(-0.35*pi),Matrix4x4Translate(0.0,0.0,-10.0)));

 end;

end;

destructor TDemoSceneRaycastVehicle.Destroy;
begin
 FreeAndNil(Vehicle);
 inherited Destroy;
end;

procedure TDemoSceneRaycastVehicle.Step(const DeltaTime:double);
begin
 Time:=Time+DeltaTime;
 Vehicle.InputVertical:=(ord(InputKeyUp) and 1)-(ord(InputKeyDown) and 1);
 Vehicle.InputHorizontal:=(ord(InputKeyLeft) and 1)-(ord(InputKeyRight) and 1);
 Vehicle.InputBrake:=InputKeyBrake;
 Vehicle.InputHandBrake:=InputKeyHandBrake;
 Vehicle.Update;
end;

procedure TDemoSceneRaycastVehicle.DebugDraw;
begin
 inherited DebugDraw;
 glDisable(GL_LIGHTING);
 glEnable(GL_POLYGON_OFFSET_LINE);
 glEnable(GL_POLYGON_OFFSET_POINT);
 glPolygonOffset(-8,8);
 glPointSize(8);
 glLineWidth(4);
 glColor4f(1,1,1,1);
 Vehicle.DebugDraw;
 glDisable(GL_DEPTH_TEST);
 glDisable(GL_POLYGON_OFFSET_LINE);
 glDisable(GL_POLYGON_OFFSET_POINT);
end;

function TDemoSceneRaycastVehicle.HasOwnKeyboardControls:boolean;
begin
 result:=true;
end;

procedure TDemoSceneRaycastVehicle.KeyDown(const aKey:Int32);
begin
 case aKey of
  VK_LEFT:begin
   InputKeyLeft:=true;
  end;
  VK_RIGHT:begin
   InputKeyRight:=true;
  end;
  VK_UP:begin
   InputKeyUp:=true;
  end;
  VK_DOWN:begin
   InputKeyDown:=true;
  end;
  VK_SPACE:begin
   InputKeyBrake:=true;
  end;
  VK_RETURN:begin
   InputKeyHandBrake:=true;
  end;
 end;
end;

procedure TDemoSceneRaycastVehicle.KeyUp(const aKey:Int32);
begin
 case aKey of
  VK_LEFT:begin
   InputKeyLeft:=false;
  end;
  VK_RIGHT:begin
   InputKeyRight:=false;
  end;
  VK_UP:begin
   InputKeyUp:=false;
  end;
  VK_DOWN:begin
   InputKeyDown:=false;
  end;
  VK_SPACE:begin
   InputKeyBrake:=false;
  end;
  VK_RETURN:begin
   InputKeyHandBrake:=false;
  end;
 end;
end;

function TDemoSceneRaycastVehicle.UpdateCamera(var aCameraPosition:TKraftVector3;var aCameraOrientation:TKraftQuaternion):boolean;
var Position:TKraftVector3;
    TargetMatrix:TKraftMatrix3x3;
    LerpFactor:TKraftScalar;
begin
 LerpFactor:=1.0-exp(-(1.0/20.0));
 Position:=Vector3Add(Vector3Add(Vehicle.WorldPosition,Vector3ScalarMul(Vehicle.WorldForward,-5.0)),Vector3ScalarMul(Vehicle.WorldUp,1.0));;
 PKraftVector3(@TargetMatrix[2,0])^:=Vector3Norm(Vector3Sub(Vehicle.WorldPosition,Position));
 PKraftVector3(@TargetMatrix[1,0])^:=Vehicle.WorldUp;
 PKraftVector3(@TargetMatrix[0,0])^:=Vector3Cross(PKraftVector3(@TargetMatrix[1,0])^,PKraftVector3(@TargetMatrix[2,0])^);
 PKraftVector3(@TargetMatrix[1,0])^:=Vector3Cross(PKraftVector3(@TargetMatrix[2,0])^,PKraftVector3(@TargetMatrix[0,0])^);
 aCameraPosition:=Vector3Lerp(aCameraPosition,Position,LerpFactor);
 aCameraOrientation:=QuaternionSlerp(aCameraOrientation,QuaternionFromMatrix3x3(TargetMatrix),LerpFactor);
 result:=true;
end;

procedure TDemoSceneRaycastVehicle.StoreWorldTransforms;
begin
 inherited StoreWorldTransforms;
 Vehicle.StoreWorldTransforms;
end;

procedure TDemoSceneRaycastVehicle.InterpolateWorldTransforms(const aAlpha:TKraftScalar);
begin
 inherited InterpolateWorldTransforms(aAlpha);
 Vehicle.InterpolateWorldTransforms(aAlpha);
end;

initialization
 RegisterDemoScene('Raycast vehicle',TDemoSceneRaycastVehicle);
end.
