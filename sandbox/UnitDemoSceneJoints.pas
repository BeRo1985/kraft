unit UnitDemoSceneJoints;

{$MODE Delphi}

interface

uses Kraft,UnitDemoScene;

type { TDemoSceneJoints }

     TDemoSceneJoints=class(TDemoScene)
      public
       RigidBodyFloor:TKraftRigidBody;
       ShapeFloorPlane:TKraftShapePlane;
       PistonSliderJoint:TKraftConstraintJointSlider;
       TestRigWheelJoint:TKraftConstraintJointWheel;
       SimulationTime:double;
       OldSolverMode:TKraftSolverMode;
       OldTGSJointMode:TKraftTGSJointMode;
       constructor Create(const AKraftPhysics:TKraft); override;
       destructor Destroy; override;
       procedure Step(const DeltaTime:double); override;
     end;

implementation

uses UnitFormMain;

constructor TDemoSceneJoints.Create(const AKraftPhysics:TKraft);
 function CreateBoxBody(const ARigidBodyType:TKraftRigidBodyType;const AExtents,APosition:TKraftVector3):TKraftRigidBody;
 var Shape:TKraftShapeBox;
 begin
  result:=TKraftRigidBody.Create(KraftPhysics);
  result.SetRigidBodyType(ARigidBodyType);
  Shape:=TKraftShapeBox.Create(KraftPhysics,result,AExtents);
  Shape.Restitution:=0.3;
  Shape.Density:=1.0;
  result.Finish;
  result.SetWorldTransformation(Matrix4x4Translate(APosition.x,APosition.y,APosition.z));
  result.CollisionGroups:=[0];
 end;
 function CreateSphereBody(const ARigidBodyType:TKraftRigidBodyType;const ARadius:TKraftScalar;const APosition:TKraftVector3):TKraftRigidBody;
 var Shape:TKraftShapeSphere;
 begin
  result:=TKraftRigidBody.Create(KraftPhysics);
  result.SetRigidBodyType(ARigidBodyType);
  Shape:=TKraftShapeSphere.Create(KraftPhysics,result,ARadius);
  Shape.Restitution:=0.3;
  Shape.Density:=1.0;
  result.Finish;
  result.SetWorldTransformation(Matrix4x4Translate(APosition.x,APosition.y,APosition.z));
  result.CollisionGroups:=[0];
 end;
var StationX:TKraftScalar;
    AnchorBody:TKraftRigidBody;
    PendulumBody:TKraftRigidBody;
    SpringWeightBody:TKraftRigidBody;
    RopeSphereBody:TKraftRigidBody;
    PulleyWeightBodyA:TKraftRigidBody;
    PulleyWeightBodyB:TKraftRigidBody;
    WeldedBarBodyA:TKraftRigidBody;
    WeldedBarBodyB:TKraftRigidBody;
    DoorBody:TKraftRigidBody;
    RotorBody:TKraftRigidBody;
    PistonBody:TKraftRigidBody;
    HoverBoxBody:TKraftRigidBody;
    UprightBoxBody:TKraftRigidBody;
    PlatformBody:TKraftRigidBody;
    WheelBody:TKraftRigidBody;
    EllipsePendulumBody:TKraftRigidBody;
    PosedArmBody:TKraftRigidBody;
    ShapeBox:TKraftShapeBox;
    ShapeCapsule:TKraftShapeCapsule;
    MotorJoint:TKraftConstraintJointMotor;
    SixDOFJoint:TKraftConstraintJoint6DOF;
begin
 inherited Create(AKraftPhysics);

 OldSolverMode:=KraftPhysics.SolverMode;
 OldTGSJointMode:=KraftPhysics.TGSJointMode;

 KraftPhysics.SolverMode:=ksmTGSSoft;
 KraftPhysics.TGSJointMode:=ktjmNativeSoft;

 RigidBodyFloor:=TKraftRigidBody.Create(KraftPhysics);
 RigidBodyFloor.SetRigidBodyType(krbtSTATIC);
 ShapeFloorPlane:=TKraftShapePlane.Create(KraftPhysics,RigidBodyFloor,Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0));
 ShapeFloorPlane.Restitution:=0.3;
 RigidBodyFloor.Finish;
 RigidBodyFloor.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 RigidBodyFloor.CollisionGroups:=[0];

 // Station 1: Ball socket joint, a box pendulum which swings freely in all directions around a static anchor block
 StationX:=-33.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.25,0.25,0.25),Vector3(StationX,6.0,0.0));
 PendulumBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.4,0.4,0.4),Vector3(StationX+2.5,6.0,0.0));
 TKraftConstraintJointBallSocket.Create(KraftPhysics,AnchorBody,PendulumBody,Vector3(StationX,6.0,0.0),false);

 // Station 2: Distance joint as soft spring, a weight bobs up and down below a static anchor block
 StationX:=-27.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.25,0.25,0.25),Vector3(StationX,6.0,0.0));
 SpringWeightBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.4,0.4,0.4),Vector3(StationX,3.5,0.0));
 TKraftConstraintJointDistance.Create(KraftPhysics,AnchorBody,SpringWeightBody,Vector3(0.0,0.0,0.0),Vector3(0.0,0.0,0.0),1.0,0.05,false);

 // Station 3: Rope joint, the sphere falls freely until the slack rope becomes taut and then it swings
 StationX:=-21.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.25,0.25,0.25),Vector3(StationX,7.0,0.0));
 RopeSphereBody:=CreateSphereBody(krbtDYNAMIC,0.5,Vector3(StationX+1.5,7.0,0.0));
 TKraftConstraintJointRope.Create(KraftPhysics,AnchorBody,RopeSphereBody,Vector3(0.0,0.0,0.0),Vector3(0.0,0.0,0.0),3.0,false);

 // Station 4: Pulley joint with a ratio of 2.0, two hanging weights coupled over two static ground anchor blocks
 StationX:=-15.0;
 CreateBoxBody(krbtSTATIC,Vector3(0.25,0.25,0.25),Vector3(StationX-1.5,7.0,0.0));
 CreateBoxBody(krbtSTATIC,Vector3(0.25,0.25,0.25),Vector3(StationX+1.5,7.0,0.0));
 PulleyWeightBodyA:=CreateBoxBody(krbtDYNAMIC,Vector3(0.4,0.4,0.4),Vector3(StationX-1.5,4.0,0.0));
 PulleyWeightBodyB:=CreateBoxBody(krbtDYNAMIC,Vector3(0.4,0.4,0.4),Vector3(StationX+1.5,4.0,0.0));
 TKraftConstraintJointPulley.Create(KraftPhysics,PulleyWeightBodyA,PulleyWeightBodyB,Vector3(StationX-1.5,7.0,0.0),Vector3(StationX+1.5,7.0,0.0),Vector3(StationX-1.5,4.4,0.0),Vector3(StationX+1.5,4.4,0.0),2.0,false);

 // Station 5: Fixed joint, two boxes welded together as a L shape which drops onto a small bump and topples as one rigid unit
 StationX:=-9.0;
 CreateBoxBody(krbtSTATIC,Vector3(0.3,0.3,0.3),Vector3(StationX-0.8,0.3,0.0));
 WeldedBarBodyA:=CreateBoxBody(krbtDYNAMIC,Vector3(1.0,0.2,0.2),Vector3(StationX,3.0,0.0));
 WeldedBarBodyB:=CreateBoxBody(krbtDYNAMIC,Vector3(0.2,0.8,0.2),Vector3(StationX+0.8,4.0,0.0));
 TKraftConstraintJointFixed.Create(KraftPhysics,WeldedBarBodyA,WeldedBarBodyB,Vector3(StationX+0.8,3.2,0.0),false);

 // Station 6: Hinge joint with angle limits, a door which swings back and forth between its limit stops
 StationX:=-3.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.1,1.5,0.1),Vector3(StationX,1.5,0.0));
 DoorBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.7,1.4,0.05),Vector3(StationX+0.9,1.5,0.0));
 TKraftConstraintJointHinge.Create(KraftPhysics,AnchorBody,DoorBody,Vector3(StationX+0.15,1.5,0.0),Vector3(0.0,1.0,0.0),true,false,-0.8,0.8,0.0,0.0,false);
 DoorBody.AngularVelocity:=Vector3(0.0,2.0,0.0);

 // Station 7: Hinge joint with motor, a windmill rotor spinning at constant speed in front of a static post
 StationX:=3.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.15,2.2,0.15),Vector3(StationX,2.2,0.0));
 RotorBody:=TKraftRigidBody.Create(KraftPhysics);
 RotorBody.SetRigidBodyType(krbtDYNAMIC);
 ShapeBox:=TKraftShapeBox.Create(KraftPhysics,RotorBody,Vector3(1.4,0.12,0.06));
 ShapeBox.Restitution:=0.3;
 ShapeBox.Density:=1.0;
 ShapeBox.Finish;
 ShapeBox:=TKraftShapeBox.Create(KraftPhysics,RotorBody,Vector3(0.12,1.4,0.06));
 ShapeBox.Restitution:=0.3;
 ShapeBox.Density:=1.0;
 ShapeBox.Finish;
 RotorBody.Finish;
 RotorBody.SetWorldTransformation(Matrix4x4Translate(StationX,4.4,0.4));
 RotorBody.CollisionGroups:=[0];
 TKraftConstraintJointHinge.Create(KraftPhysics,AnchorBody,RotorBody,Vector3(StationX,4.4,0.25),Vector3(0.0,0.0,1.0),false,true,-1.0,1.0,2.0,500.0,false);

 // Station 8: Slider joint with limits and motor, a piston which is driven back and forth along its axis (see Step)
 StationX:=9.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(2.5,0.08,0.3),Vector3(StationX,0.6,0.0));
 PistonBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.5,0.35,0.35),Vector3(StationX,1.2,0.0));
 PistonSliderJoint:=TKraftConstraintJointSlider.Create(KraftPhysics,AnchorBody,PistonBody,Vector3(StationX,1.2,0.0),Vector3(1.0,0.0,0.0),true,true,-2.0,2.0,1.5,400.0,false);

 // Station 9: World plane distance joint, a box hovers spring-like at a minimum distance above the floor plane
 StationX:=15.0;
 HoverBoxBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.5,0.5,0.5),Vector3(StationX,3.5,0.0));
 TKraftConstraintJointWorldPlaneDistance.Create(KraftPhysics,HoverBoxBody,Vector3(0.0,-0.5,0.0),Plane(Vector3Norm(Vector3(0.0,1.0,0.0)),0.0),false,2.0,kclbLimitMinimumDistance,1.0,0.5);

 // Station 10: Parallel joint, the tilted box is pulled upright again by the angular spring while the twist around the vertical axis stays free
 StationX:=21.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.2,0.2,0.2),Vector3(StationX,4.0,0.0));
 UprightBoxBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.3,1.1,0.3),Vector3(StationX,1.3,0.0));
 // The joint captures the current relative orientation as its target, so the box is tilted and given a free spin afterwards
 TKraftConstraintJointParallel.Create(KraftPhysics,AnchorBody,UprightBoxBody,Vector3(0.0,1.0,0.0),4.0,0.7,200.0,false);
 UprightBoxBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateZ(0.7),Matrix4x4Translate(StationX,1.3,0.0)));
 UprightBoxBody.AngularVelocity:=Vector3(0.0,3.0,0.0);

 // Station 11: Motor joint as 6-DOF soft servo, the linear spring holds the hovering platform in place while the angular velocity motor spins it
 StationX:=27.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.2,0.2,0.2),Vector3(StationX,4.5,0.0));
 PlatformBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.7,0.1,0.7),Vector3(StationX,3.0,0.0));
 MotorJoint:=TKraftConstraintJointMotor.Create(KraftPhysics,AnchorBody,PlatformBody,Vector3(StationX,3.0,0.0),false);
 MotorJoint.LinearFrequencyHz:=2.0;
 MotorJoint.LinearDampingRatio:=0.7;
 MotorJoint.MaximalSpringForce:=1000.0;
 MotorJoint.AngularVelocityTarget:=Vector3(0.0,1.5,0.0);
 MotorJoint.MaximalVelocityTorque:=500.0;

 // Station 12: Wheel joint as single wheel test rig under a static chassis block, with suspension spring, spin motor and sweeping steering (see Step)
 StationX:=33.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.5,0.2,0.2),Vector3(StationX,1.55,0.0));
 WheelBody:=TKraftRigidBody.Create(KraftPhysics);
 WheelBody.SetRigidBodyType(krbtDYNAMIC);
 ShapeCapsule:=TKraftShapeCapsule.Create(KraftPhysics,WheelBody,0.5,0.3);
 ShapeCapsule.Restitution:=0.1;
 ShapeCapsule.Density:=1.0;
 WheelBody.Finish;
 // The capsule axis points along its local y axis, so rotate it so that it points along the world z spin axis
 WheelBody.SetWorldTransformation(Matrix4x4TermMul(Matrix4x4RotateX(0.5*pi),Matrix4x4Translate(StationX,0.55,0.0)));
 WheelBody.CollisionGroups:=[0];
 TestRigWheelJoint:=TKraftConstraintJointWheel.Create(KraftPhysics,AnchorBody,WheelBody,Vector3(StationX,0.55,0.0),Vector3(0.0,1.0,0.0),Vector3(0.0,0.0,1.0),false);
 TestRigWheelJoint.EnableSuspensionSpring:=true;
 TestRigWheelJoint.SuspensionFrequencyHz:=2.5;
 TestRigWheelJoint.SuspensionDampingRatio:=0.5;
 TestRigWheelJoint.EnableSuspensionLimit:=true;
 TestRigWheelJoint.LowerSuspensionLimit:=-0.4;
 TestRigWheelJoint.UpperSuspensionLimit:=0.2;
 TestRigWheelJoint.EnableSpinMotor:=true;
 TestRigWheelJoint.MaximalSpinTorque:=50.0;
 TestRigWheelJoint.SpinSpeed:=6.0;
 TestRigWheelJoint.EnableSteering:=true;
 TestRigWheelJoint.SteeringFrequencyHz:=4.0;
 TestRigWheelJoint.SteeringDampingRatio:=1.0;
 TestRigWheelJoint.MaximalSteeringTorque:=200.0;
 TestRigWheelJoint.EnableSteeringLimit:=true;
 TestRigWheelJoint.LowerSteeringLimit:=-0.8;
 TestRigWheelJoint.UpperSteeringLimit:=0.8;

 // Station 13: 6-DOF joint. Left, a hanging bar circling inside an elliptical swing cone: the swing reference
 // axis aims the frame x axis at world x, so the bar can lean far along world z (rotation about frame x,
 // half angle 1.1) but only a little along world x (rotation about frame y, half angle 0.25), which traces a
 // visibly elliptical path. Right, a bar held in a tilted pose against gravity purely by the slerp drive
 // spring, the powered-ragdoll building block.
 StationX:=39.0;
 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.2,0.2,0.2),Vector3(StationX-1.2,5.0,0.0));
 EllipsePendulumBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.15,1.0,0.15),Vector3(StationX-1.2,3.7,0.0));
 SixDOFJoint:=TKraftConstraintJoint6DOF.Create(KraftPhysics,AnchorBody,EllipsePendulumBody,Vector3(StationX-1.2,5.0,0.0),Vector3(0.0,1.0,0.0),Vector3(1.0,0.0,0.0));
 SixDOFJoint.SwingMode:=k6damLimited;
 SixDOFJoint.ConeHalfAngleX:=1.1;
 SixDOFJoint.ConeHalfAngleY:=0.25;
 SixDOFJoint.TwistMode:=k6damLocked;
 // Start with a diagonal push so the bar keeps circling along the elliptical cone boundary for a while
 EllipsePendulumBody.AngularVelocity:=Vector3(3.0,0.0,1.5);

 AnchorBody:=CreateBoxBody(krbtSTATIC,Vector3(0.2,0.2,0.2),Vector3(StationX+1.2,5.0,0.0));
 PosedArmBody:=CreateBoxBody(krbtDYNAMIC,Vector3(0.15,1.0,0.15),Vector3(StationX+1.2,3.7,0.0));
 SixDOFJoint:=TKraftConstraintJoint6DOF.Create(KraftPhysics,AnchorBody,PosedArmBody,Vector3(StationX+1.2,5.0,0.0),Vector3(0.0,1.0,0.0),Vector3(1.0,0.0,0.0));
 SixDOFJoint.SwingMode:=k6damFree;
 SixDOFJoint.TwistMode:=k6damFree;
 SixDOFJoint.EnableSlerpDrive:=true;
 SixDOFJoint.SlerpDriveFrequencyHz:=4.0;
 SixDOFJoint.SlerpDriveDampingRatio:=0.7;
 SixDOFJoint.MaximalSlerpDriveTorque:=200.0;
 // Target pose: 45 degrees swung about the frame x axis (world x), held there against gravity
 SixDOFJoint.SlerpDriveTarget:=QuaternionFromAxisAngle(Vector3(1.0,0.0,0.0),0.25*pi);

end;

destructor TDemoSceneJoints.Destroy;
begin
 KraftPhysics.SolverMode:=OldSolverMode;
 KraftPhysics.TGSJointMode:=OldTGSJointMode;
 inherited Destroy;
end;

procedure TDemoSceneJoints.Step(const DeltaTime:double);
begin

 SimulationTime:=SimulationTime+DeltaTime;

 // Reverse the piston motor direction shortly before the slider translation limits are reached
 if assigned(PistonSliderJoint) then begin
  if PistonSliderJoint.GetTranslation>1.8 then begin
   PistonSliderJoint.SetMotorSpeed(-1.5);
  end else begin
   if PistonSliderJoint.GetTranslation<(-1.8) then begin
    PistonSliderJoint.SetMotorSpeed(1.5);
   end;
  end;
 end;

 // Sweep the steering of the wheel test rig back and forth
 if assigned(TestRigWheelJoint) then begin
  TestRigWheelJoint.TargetSteeringAngle:=0.6*sin(SimulationTime*1.5);
 end;

end;

initialization
 RegisterDemoScene('Joints Gallery',TDemoSceneJoints);
end.
