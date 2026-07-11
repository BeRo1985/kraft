unit UnitDemoSceneSphereOnSDFTerrain;

{$MODE Delphi}

interface

uses Math,
     Kraft,
     UnitDemoScene;

type TDemoSceneSphereOnSDFTerrain=class(TDemoScene)
      public
       RigidBodyFloor:TKraftRigidBody;
       TerrainSignedDistanceField:TKraftSignedDistanceFieldTerrain;
       ShapeTerrain:TKraftShapeSignedDistanceField;
       RigidBodySphere:TKraftRigidBody;
       ShapeSphere:TKraftShapeSphere;
       constructor Create(const AKraftPhysics:TKraft); override;
       destructor Destroy; override;
       procedure Step(const DeltaTime:double); override;
     end;

implementation

uses UnitFormMain;

constructor TDemoSceneSphereOnSDFTerrain.Create(const AKraftPhysics:TKraft);
const Resolution=128;
      Size=512.0;
      Height=16.0;
var x,z:Int32;
    k1,k2:TKraftScalar;
begin
 inherited Create(AKraftPhysics);

 RigidBodyFloor:=TKraftRigidBody.Create(KraftPhysics);
 RigidBodyFloor.SetRigidBodyType(krbtSTATIC);
 TerrainSignedDistanceField:=TKraftSignedDistanceFieldTerrain.Create(KraftPhysics,Resolution,Resolution,Size,Size);
 SignedDistanceFieldGarbageCollector.Add(TerrainSignedDistanceField);
 for z:=0 to Resolution-1 do begin
  for x:=0 to Resolution-1 do begin
   k1:=sin(x*pi*4/Resolution)*2;
   k2:=cos(z*pi*4/Resolution)*2;
   TerrainSignedDistanceField.Heights[x,z]:=(Min(Max(((((cos(x*pi*k1/Resolution)*sin(z*pi*k2/Resolution)))+(k1*0.5)-(k2*0.5)))*64,-64),32)/64.0)*Height;
  end;
 end;
 TerrainSignedDistanceField.UpdateData;
 TerrainSignedDistanceField.Finish;
 ShapeTerrain:=TKraftShapeSignedDistanceField.Create(KraftPhysics,RigidBodyFloor,TerrainSignedDistanceField);
 ShapeTerrain.Restitution:=0.3;
 ShapeTerrain.Density:=100.0;
 RigidBodyFloor.Finish;
 RigidBodyFloor.SetWorldTransformation(Matrix4x4Translate(0.0,0.0,0.0));
 RigidBodyFloor.CollisionGroups:=[0];

 RigidBodySphere:=TKraftRigidBody.Create(KraftPhysics);
 RigidBodySphere.SetRigidBodyType(krbtDYNAMIC);
 ShapeSphere:=TKraftShapeSphere.Create(KraftPhysics,RigidBodySphere,1.0);
 ShapeSphere.Restitution:=0.3;
 ShapeSphere.Density:=1.0;
 RigidBodySphere.Finish;
 RigidBodySphere.SetWorldTransformation(Matrix4x4Translate(0.0,24.0,0.0));
 RigidBodySphere.CollisionGroups:=[0];

end;

destructor TDemoSceneSphereOnSDFTerrain.Destroy;
begin
 inherited Destroy;
end;

procedure TDemoSceneSphereOnSDFTerrain.Step(const DeltaTime:double);
begin
end;

initialization
 RegisterDemoScene('Sphere on SDF terrain',TDemoSceneSphereOnSDFTerrain);
end.
