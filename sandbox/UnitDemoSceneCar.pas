unit UnitDemoSceneCar;

interface

uses Math,Kraft,UnitDemoScene;

type TDemoSceneCar=class(TDemoScene)
      public
       RigidBodyFloor:TKraftRigidBody;
       ShapeFloorPlane:TKraftShapePlane;
       ChassisRigidBody:TKraftRigidBody;
       WheelRigidBodies:array[0..1,0..1] of TKraftRigidBody;
       WheelHingeJointConstraints:array[0..1,0..1] of TKraftConstraintJointHinge;
       CarSteering:double;
       CarSpeed:double;
       Time:double;
       constructor Create(const AKraftPhysics:TKraft); override;
       destructor Destroy; override;
       procedure Step(const DeltaTime:double); override;
     end;

implementation

uses UnitFormMain,UnitFormGL;

const CarWidth=2.0;
      CarLength=4.0;

      CarHalfWidth=CarWidth*0.5;

constructor TDemoSceneCar.Create(const AKraftPhysics:TKraft);
const WheelPositions:array[0..1,0..1] of TKraftVector3=(((x:CarHalfWidth;y:0.75;z:-CarLength),
                                                         (x:-CarHalfWidth;y:0.75;z:-CarLength)),
                                                        ((x:CarHalfWidth;y:0.75;z:0.0),
                                                         (x:-CarHalfWidth;y:0.75;z:0.0)));
var Index,x,y:longint;
    Shape:TKraftShape;
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

 begin
  ChassisRigidBody:=TKraftRigidBody.Create(KraftPhysics);
  ChassisRigidBody.SetRigidBodyType(krbtDYNAMIC);
  Shape:=TKraftShapeBox.Create(KraftPhysics,ChassisRigidBody,Vector3(1.0,0.5,3.0));
  Shape.Restitution:=0.3;
  Shape.Density:=10.0;
  Shape.LocalTransform:=Matrix4x4Translate(0.0,0.25,0.0);
  Shape:=TKraftShapeBox.Create(KraftPhysics,ChassisRigidBody,Vector3(1.0,1.0,2.0));
  Shape.Restitution:=0.3;
  Shape.Density:=10.0;
  Shape.LocalTransform:=Matrix4x4Translate(0.0,1.75,1.0);
  ChassisRigidBody.Finish;
  ChassisRigidBody.SetWorldTransformation(Matrix4x4Translate(0.0,1.25,-2.0));
  ChassisRigidBody.CollisionGroups:=[0];
  ChassisRigidBody.AngularVelocityDamp:=10.0;
  ChassisRigidBody.LinearVelocityDamp:=0.05;
 end;

 for y:=0 to 1 do begin

  for x:=0 to 1 do begin

   WheelRigidBodies[y,x]:=TKraftRigidBody.Create(KraftPhysics);
   WheelRigidBodies[y,x].SetRigidBodyType(krbtDYNAMIC);
   Shape:=TKraftShapeSphere.Create(KraftPhysics,WheelRigidBodies[y,x],0.75);
   Shape.Restitution:=0.3;
   Shape.Density:=1.0;
   WheelRigidBodies[y,x].Finish;
   WheelRigidBodies[y,x].SetWorldTransformation(Matrix4x4Translate(WheelPositions[y,x]));
   WheelRigidBodies[y,x].CollisionGroups:=[0];
   WheelRigidBodies[y,x].AngularVelocityDamp:=10.0;
   WheelRigidBodies[y,x].LinearVelocityDamp:=0.05;

   WheelHingeJointConstraints[y,x]:=TKraftConstraintJointHinge.Create(KraftPhysics,
                                                                      ChassisRigidBody,
                                                                      WheelRigidBodies[y,x],
                                                                      WheelPositions[y,x],
                                                                      Vector3Norm(Vector3(1.0,0.0,0.0)),
                                                                      false,
                                                                      false,
                                                                      -1.0,
                                                                      1.0,
                                                                      1.0,
                                                                      0.0,
                                                                      false);

  end;
 end;

end;

destructor TDemoSceneCar.Destroy;
begin
 inherited Destroy;
end;

procedure TDemoSceneCar.Step(const DeltaTime:double);
const Signs:array[0..1] of longint=(1,-1);
var x,y:longint;
    CarAngle,Radius:double;
    SideRadius:array[0..1] of single;
    AxisVectors:array[0..1,0..1] of TKraftVector3;
begin
 Time:=Time+DeltaTime;
 CarSteering:=sin((Time*0.015625)*(2.0*pi))*10.0;
 CarSpeed:=4.0;
 CarAngle:=Min(Max(CarSteering,-1.0),1.0)*30.0;
 if abs(CarAngle)>EPSILON then begin
  Radius:=tan(CarAngle*DEG2RAD);
 end else begin
  Radius:=tan(EPSILON*DEG2RAD);
 end;
 Radius:=CarLength/Radius;
 SideRadius[0]:=arctan(CarLength/(Radius-CarHalfWidth));
 SideRadius[1]:=arctan(CarLength/(Radius+CarHalfWidth));
 for y:=0 to 1 do begin
  for x:=0 to 1 do begin
   AxisVectors[y,x]:=Vector3TermMatrixMulBasis(Vector3Norm(Vector3(cos(SideRadius[x]),0.0,sin(SideRadius[x])*Signs[y])),ChassisRigidBody.WorldTransform);
   WheelHingeJointConstraints[y,x].SetWorldRotationAxis(AxisVectors[y,x]);
   WheelRigidBodies[y,x].SetWorldAngularVelocity(Vector3TermMatrixMul(Vector3ScalarMul(AxisVectors[y,x],-CarSpeed),WheelRigidBodies[y,x].WorldInverseInertiaTensor),kfmVelocity);
   WheelRigidBodies[y,x].SetToAwake;
  end;
 end;
 ChassisRigidBody.SetToAwake;
end;

initialization
 RegisterDemoScene('Car',TDemoSceneCar);
end.
