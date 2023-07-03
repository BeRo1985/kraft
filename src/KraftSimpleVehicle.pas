(******************************************************************************
 *                      VEHICLE PHYSICS FOR KRAFT PHYSICS ENGINE              *
 ******************************************************************************
 *                        Version 2023-07-02-10-26-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (c) 2023-2023, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/kraft                                        *
 * 4. Write code, which is compatible with lastest Delphi and lastest         *
 *    FreePascal versions                                                     *
 * 5. Don't use Delphi VCL, FreePascal FCL or Lazarus LCL libraries/units.    *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able                                                       *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging                  *
 * 9. Use TKraftScalar instead of float/double so that Kraft can be compiled  *
 *    as double/single precision.                                             *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms in single   *
 *     and double precision.                                                  *
 *                                                                            *
 ******************************************************************************)
unit KraftSimpleVehicle;
{$ifdef fpc}
 {$mode delphi}
 {$warnings off}
 {$hints off}
 {$define caninline}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpuamd64}
  {$define cpux86_64}
  {$define cpux64}
 {$else}
  {$ifdef cpux86_64}
   {$define cpuamd64}
   {$define cpux64}
  {$endif}
 {$endif}
 {$ifdef cpu386}
  {$define cpu386}
  {$asmmode intel}
  {$define canx86simd}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {$packset fixed}
{$else}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$safedivide off}
 {$optimization on}
 {$undef caninline}
 {$undef canx86simd}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
 {$ifdef ver180}
  {$define caninline}
  {$ifdef cpu386}
   {$define canx86simd}
  {$endif}
  {$finitefloat off}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$extendedsyntax on}
{$writeableconst on}
{$varstringchecks on}
{$typedaddress off}
{$overflowchecks off}
{$rangechecks off}
{$ifndef fpc}
{$realcompatibility off}
{$endif}
{$openstrings on}
{$longstrings on}
{$booleval off}
{$typeinfo on}

{-$define UseMoreCollisionGroups}

{$define UseTriangleMeshFullPerturbation}

{-$define DebugDraw}

{-$define memdebug}

{$ifdef UseDouble}
 {$define NonSIMD}
{$endif}

{-$define NonSIMD}

{$ifdef NonSIMD}
 {$undef CPU386ASMForSinglePrecision}
 {$undef SIMD}
{$else}
 {$ifdef cpu386}
  {$if not (defined(Darwin) or defined(CompileForWithPIC))}
   {$define CPU386ASMForSinglePrecision}
  {$ifend}
 {$endif}
 {$undef SIMD}
 {$ifdef CPU386ASMForSinglePrecision}
  {$define SIMD}
 {$endif}
{$endif}

interface

uses {$ifdef windows}
      Windows,
      MMSystem,
     {$else}
      {$ifdef unix}
       BaseUnix,
       Unix,
       UnixType,
       {$if defined(linux) or defined(android)}
        linux,
       {$ifend}
      {$else}
       SDL,
      {$endif}
     {$endif}
     {$ifdef DebugDraw}
      {$ifndef NoOpenGL}
       {$ifdef fpc}
        GL,
        GLext,
       {$else}
        OpenGL,
       {$endif}
      {$endif}
     {$endif}
     SysUtils,
     Classes,
     SyncObjs,
{$ifdef KraftPasMP}
     PasMP,
{$endif}
{$ifdef KraftPasJSON}
     PasJSON,
{$endif}
     Math,
     Kraft;

type { TKraftSimpleVehicle }
     TKraftSimpleVehicle=class
      public
       const CountWheels=4; // Count of wheels
       type TDebugDrawLine=procedure(const aP0,aP1:TKraftVector3;const aColor:TKraftVector4) of object;
            { TSpringMath }
            TSpringMath=class
             public
              class function CalculateForce(const aCurrentLength,aRestLength,aStrength:TKraftScalar):TKraftScalar; static;
              class function CalculateForceDamped(const aCurrentLength,aLengthVelocity,aRestLength,aStrength,aDamper:TKraftScalar):TKraftScalar; static;
            end; 
            { TSpringData }
            TSpringData=record
             private
              fCurrentLength:TKraftScalar;
              fCurrentVelocity:TKraftScalar;
            end; 
            PSpringData=^TSpringData;
            { TWheel }
            TWheel=
             (
              FrontLeft=0,
              FrontRight=1,
              BackLeft=2,
              BackRight=3   
             );
            PWheel=^TWheel;
            { TSpringDatas }
            TSpringDatas=array[TWheel] of TSpringData;
            PSpringDatas=^TSpringDatas;
            { TWheelData }
            TWheelData=record
             private
              fYawRad:TKraftScalar;
              fRotationRad:TKraftScalar;
              fWorldTransform:TKraftMatrix4x4;
              fLastWorldTransform:TKraftMatrix4x4;
              fVisualWorldTransform:TKraftMatrix4x4;
            end;
            PWheelData=^TWheelData;
            { TWheelDatas }
            TWheelDatas=array[TWheel] of TWheelData;
            PWheelDatas=^TWheelDatas;
            { TVehicleSettings }
            TVehicleSettings=class
             private
              fWidth:TKraftScalar;
              fHeight:TKraftScalar;
              fLength:TKraftScalar;
              fWheelsRadius:TKraftScalar;
              fWheelsHeight:TKraftScalar;
              fWheelsPaddingX:TKraftScalar;
              fWheelsPaddingZ:TKraftScalar;
              fChassisMass:TKraftScalar;
              fTireMass:TKraftScalar;
              fSpringRestLength:TKraftScalar;
              fSpringStrength:TKraftScalar;
              fSpringDamper:TKraftScalar;
              fAccelerationPower:TKraftScalar;
              fBrakePower:TKraftScalar;
              fMaximumSpeed:TKraftScalar;
              fMaximumReverseSpeed:TKraftScalar;
              fSteeringAngle:TKraftScalar;
              fFrontWheelsGripFactor:TKraftScalar;
              fBackWheelsGripFactor:TKraftScalar;
              fAirResistance:TKraftScalar;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
{$ifdef KraftPasJSON}
              procedure LoadFromJSON(const aJSONItem:TPasJSONItem);
              function SaveToJSON:TPasJSONItem;
{$endif}
             public
              property Width:TKraftScalar read fWidth write fWidth;
              property Height:TKraftScalar read fHeight write fHeight;
              property Length:TKraftScalar read fLength write fLength;
              property WheelsRadius:TKraftScalar read fWheelsRadius write fWheelsRadius;
              property WheelsHeight:TKraftScalar read fWheelsHeight write fWheelsHeight;
              property WheelsPaddingX:TKraftScalar read fWheelsPaddingX write fWheelsPaddingX;
              property WheelsPaddingZ:TKraftScalar read fWheelsPaddingZ write fWheelsPaddingZ;
              property ChassisMass:TKraftScalar read fChassisMass write fChassisMass;
              property TireMass:TKraftScalar read fTireMass write fTireMass;
              property SpringRestLength:TKraftScalar read fSpringRestLength write fSpringRestLength;
              property SpringStrength:TKraftScalar read fSpringStrength write fSpringStrength;
              property SpringDamper:TKraftScalar read fSpringDamper write fSpringDamper;
              property AccelerationPower:TKraftScalar read fAccelerationPower write fAccelerationPower;
              property BrakePower:TKraftScalar read fBrakePower write fBrakePower;
              property MaximumSpeed:TKraftScalar read fMaximumSpeed write fMaximumSpeed;
              property MaximumReverseSpeed:TKraftScalar read fMaximumReverseSpeed write fMaximumReverseSpeed;
              property SteeringAngle:TKraftScalar read fSteeringAngle write fSteeringAngle;
              property FrontWheelsGripFactor:TKraftScalar read fFrontWheelsGripFactor write fFrontWheelsGripFactor;
              property BackWheelsGripFactor:TKraftScalar read fBackWheelsGripFactor write fBackWheelsGripFactor;
              property AirResistance:TKraftScalar read fAirResistance write fAirResistance;
            end;
      private
       fPhysics:TKraft;
       fRigidBody:TKraftRigidBody;
       fShape:TKraftShape;
       fSpringDatas:TSpringDatas;
       fWheelDatas:TWheelDatas;
       fSteeringInput:TKraftScalar;
       fAccelerationInput:TKraftScalar;
       fSettings:TVehicleSettings;
       fForward:TKraftVector3;
       fVelocity:TKraftVector3;       
       fDeltaTime:TKraftScalar;
       fInverseDeltaTime:TKraftScalar;
       fDebugDrawLine:TDebugDrawLine;
       fWorldTransform:TKraftMatrix4x4;
       fWorldLeft:TKraftVector3;
       fWorldRight:TKraftVector3;
       fWorldDown:TKraftVector3;
       fWorldUp:TKraftVector3;
       fWorldBackward:TKraftVector3;
       fWorldForward:TKraftVector3;
       fWorldPosition:TKraftVector3;
       fLastWorldTransform:TKraftMatrix4x4;
       fLastWorldLeft:TKraftVector3;
       fLastWorldRight:TKraftVector3;
       fLastWorldDown:TKraftVector3;
       fLastWorldUp:TKraftVector3;
       fLastWorldBackward:TKraftVector3;
       fLastWorldForward:TKraftVector3;
       fLastWorldPosition:TKraftVector3;
       fVisualWorldTransform:TKraftMatrix4x4;
       fVisualWorldLeft:TKraftVector3;
       fVisualWorldRight:TKraftVector3;
       fVisualWorldDown:TKraftVector3;
       fVisualWorldUp:TKraftVector3;
       fVisualWorldBackward:TKraftVector3;
       fVisualWorldForward:TKraftVector3;
       fVisualWorldPosition:TKraftVector3;
       fInputVertical:TKraftScalar;
       fInputHorizontal:TKraftScalar;
       fInputReset:Boolean;
       fInputBrake:Boolean;
       fInputHandBrake:Boolean;
       fSpeed:TKraftScalar;
       fSpeedKMH:TKraftScalar;
       procedure CalculateAckermannSteering;
       procedure SetSteeringInput(const aSteeringInput:TKraftScalar);
       procedure SetAccelerationInput(const aAccelerationInput:TKraftScalar);
       function GetSpringRelativePosition(const aWheel:TWheel):TKraftVector3;
       function GetSpringPosition(const aWheel:TWheel):TKraftVector3;
       function GetSpringHitPosition(const aWheel:TWheel):TKraftVector3;
       function GetWheelRollDirection(const aWheel:TWheel):TKraftVector3;
       function GetWheelSlideDirection(const aWheel:TWheel):TKraftVector3;
       function GetWheelTorqueRelativePosition(const aWheel:TWheel):TKraftVector3;
       function GetWheelTorquePosition(const aWheel:TWheel):TKraftVector3;
       function GetWheelGripFactor(const aWheel:TWheel):TKraftScalar;
       function GetWheelTransform(const aWheel:TWheel):TKraftMatrix4x4;
       function IsGrounded(const aWheel:TWheel):boolean;
       procedure CastSpring(const aWheel:TWheel);
       procedure UpdateWorldTransformVectors;
       procedure UpdateSuspension;
       procedure UpdateSteering;
       procedure UpdateAcceleration;
       procedure UpdateBraking;
       procedure UpdateAirResistance;
       procedure UpdateVisuals;
      public
       constructor Create(const aPhysics:TKraft); reintroduce;
       destructor Destroy; override;
       procedure Finish;
       procedure Update(const aDeltaTime:TKraftScalar);
       procedure StoreWorldTransforms;
       procedure InterpolateWorldTransforms(const aAlpha:TKraftScalar);
{$ifdef DebugDraw}
       procedure DebugDraw;
{$endif}
      public
       property SpringDatas:TSpringDatas read fSpringDatas;
       property Settings:TVehicleSettings read fSettings write fSettings;       
      published
       property Physics:TKraft read fPhysics;
       property RigidBody:TKraftRigidBody read fRigidBody write fRigidBody;
       property Shape:TKraftShape read fShape write fShape;
       property SteeringInput:TKraftScalar read fSteeringInput write SetSteeringInput;
       property AccelerationInput:TKraftScalar read fAccelerationInput write SetAccelerationInput;
      public
       property WorldTransform:TKraftMatrix4x4 read fWorldTransform write fWorldTransform;
       property WorldLeft:TKraftVector3 read fWorldLeft write fWorldLeft;
       property WorldRight:TKraftVector3 read fWorldRight write fWorldRight;
       property WorldDown:TKraftVector3 read fWorldDown write fWorldDown;
       property WorldUp:TKraftVector3 read fWorldUp write fWorldUp;
       property WorldBackward:TKraftVector3 read fWorldBackward write fWorldBackward;
       property WorldForward:TKraftVector3 read fWorldForward write fWorldForward;
       property WorldPosition:TKraftVector3 read fWorldPosition write fWorldPosition;
       property LastWorldTransform:TKraftMatrix4x4 read fLastWorldTransform write fLastWorldTransform;
       property LastWorldLeft:TKraftVector3 read fLastWorldLeft write fLastWorldLeft;
       property LastWorldRight:TKraftVector3 read fLastWorldRight write fLastWorldRight;
       property LastWorldDown:TKraftVector3 read fLastWorldDown write fLastWorldDown;
       property LastWorldUp:TKraftVector3 read fLastWorldUp write fLastWorldUp;
       property LastWorldBackward:TKraftVector3 read fLastWorldBackward write fLastWorldBackward;
       property LastWorldForward:TKraftVector3 read fLastWorldForward write fLastWorldForward;
       property LastWorldPosition:TKraftVector3 read fLastWorldPosition write fLastWorldPosition;
       property VisualWorldTransform:TKraftMatrix4x4 read fVisualWorldTransform write fVisualWorldTransform;
       property VisualWorldLeft:TKraftVector3 read fVisualWorldLeft write fVisualWorldLeft;
       property VisualWorldRight:TKraftVector3 read fVisualWorldRight write fVisualWorldRight;
       property VisualWorldDown:TKraftVector3 read fVisualWorldDown write fVisualWorldDown;
       property VisualWorldUp:TKraftVector3 read fVisualWorldUp write fVisualWorldUp;
       property VisualWorldBackward:TKraftVector3 read fVisualWorldBackward write fVisualWorldBackward;
       property VisualWorldForward:TKraftVector3 read fVisualWorldForward write fVisualWorldForward;
       property VisualWorldPosition:TKraftVector3 read fVisualWorldPosition write fVisualWorldPosition;
      published
       property InputVertical:TKraftScalar read fInputVertical write fInputVertical;
       property InputHorizontal:TKraftScalar read fInputHorizontal write fInputHorizontal;
       property InputReset:Boolean read fInputReset write fInputReset;
       property InputBrake:Boolean read fInputBrake write fInputBrake;
       property InputHandBrake:Boolean read fInputHandBrake write fInputHandBrake;
       property Speed:TKraftScalar read fSpeed write fSpeed;
       property SpeedKMH:TKraftScalar read fSpeedKMH write fSpeedKMH;
       property DebugDrawLine:TDebugDrawLine read fDebugDrawLine write fDebugDrawLine;
     end;

implementation

{ TKraftSimpleVehicle.TSpringMath }

// Calculates the force which wants to restore the spring to its rest length.
class function TKraftSimpleVehicle.TSpringMath.CalculateForce(const aCurrentLength,aRestLength,aStrength:TKraftScalar):TKraftScalar;
begin
 result:=(aRestLength-aCurrentLength)*aStrength;
end;

// Combines the force which wants to restore the spring to its rest length with the force which wants to damp the spring's motion.
class function TKraftSimpleVehicle.TSpringMath.CalculateForceDamped(const aCurrentLength,aLengthVelocity,aRestLength,aStrength,aDamper:TKraftScalar):TKraftScalar;
begin
 result:=((aRestLength-aCurrentLength)*aStrength)-(aLengthVelocity*aDamper);
end;

{ TKraftSimpleVehicle.TVehicleSettings }

constructor TKraftSimpleVehicle.TVehicleSettings.Create;
begin
 inherited Create;
 fWidth:=1.9;
 fHeight:=0.75;
 fLength:=3.4;
 fWheelsRadius:=0.25;
 fWheelsHeight:=-0.25;
 fWheelsPaddingX:=0.06;
 fWheelsPaddingZ:=0.12;
 fChassisMass:=60;
 fTireMass:=1;
 fSpringRestLength:=0.8;
 fSpringStrength:=1200;
 fSpringDamper:=75;
 fAccelerationPower:=300;
 fBrakePower:=1.5;
 fMaximumSpeed:=10;
 fMaximumReverseSpeed:=2.5;
 fSteeringAngle:=20.0;
 fFrontWheelsGripFactor:=0.8;
 fBackWheelsGripFactor:=0.9;
 fAirResistance:=5.0;
end;

destructor TKraftSimpleVehicle.TVehicleSettings.Destroy;
begin
 inherited Destroy;
end;

{$ifdef KraftPasJSON}
procedure TKraftSimpleVehicle.TVehicleSettings.LoadFromJSON(const aJSONItem:TPasJSONItem);
begin
 if assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject) then begin
  fWidth:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['width'],fWidth);
  fHeight:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['height'],fHeight);
  fLength:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['length'],fLength);
  fWheelsRadius:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['wheelsradius'],fWheelsRadius);
  fWheelsHeight:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['wheelsheight'],fWheelsHeight);
  fWheelsPaddingX:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['wheelspaddingx'],fWheelsPaddingX);
  fWheelsPaddingZ:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['wheelspaddingz'],fWheelsPaddingZ);
  fChassisMass:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['chassismass'],fChassisMass);
  fTireMass:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['tiremass'],fTireMass);
  fSpringRestLength:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['springrestlength'],fSpringRestLength);
  fSpringStrength:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['springstrength'],fSpringStrength);
  fSpringDamper:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['springdamper'],fSpringDamper);
  fAccelerationPower:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['accelerationpower'],fAccelerationPower);
  fBrakePower:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['brakepower'],fBrakePower);
  fMaximumSpeed:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['maximumspeed'],fMaximumSpeed);
  fMaximumReverseSpeed:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['maximumreversespeed'],fMaximumReverseSpeed);
  fSteeringAngle:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['steeringangle'],fSteeringAngle);
  fFrontWheelsGripFactor:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['frontwheelsgripfactor'],fFrontWheelsGripFactor);
  fBackWheelsGripFactor:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['backwheelsgripfactor'],fBackWheelsGripFactor);
  fAirResistance:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['airresistance'],fAirResistance);
 end;
end;

function TKraftSimpleVehicle.TVehicleSettings.SaveToJSON:TPasJSONItem;
begin
 result:=TPasJSONItemObject.Create;
 TPasJSONItemObject(result).Add('width',TPasJSONItemNumber.Create(fWidth));
 TPasJSONItemObject(result).Add('height',TPasJSONItemNumber.Create(fHeight));
 TPasJSONItemObject(result).Add('length',TPasJSONItemNumber.Create(fLength));
 TPasJSONItemObject(result).Add('wheelsradius',TPasJSONItemNumber.Create(fWheelsRadius));
 TPasJSONItemObject(result).Add('wheelsheight',TPasJSONItemNumber.Create(fWheelsHeight));
 TPasJSONItemObject(result).Add('wheelspaddingx',TPasJSONItemNumber.Create(fWheelsPaddingX));
 TPasJSONItemObject(result).Add('wheelspaddingz',TPasJSONItemNumber.Create(fWheelsPaddingZ));
 TPasJSONItemObject(result).Add('chassismass',TPasJSONItemNumber.Create(fChassisMass));
 TPasJSONItemObject(result).Add('tiremass',TPasJSONItemNumber.Create(fTireMass));
 TPasJSONItemObject(result).Add('springrestlength',TPasJSONItemNumber.Create(fSpringRestLength));
 TPasJSONItemObject(result).Add('springstrength',TPasJSONItemNumber.Create(fSpringStrength));
 TPasJSONItemObject(result).Add('springdamper',TPasJSONItemNumber.Create(fSpringDamper));
 TPasJSONItemObject(result).Add('accelerationpower',TPasJSONItemNumber.Create(fAccelerationPower));
 TPasJSONItemObject(result).Add('brakepower',TPasJSONItemNumber.Create(fBrakePower));
 TPasJSONItemObject(result).Add('maximumspeed',TPasJSONItemNumber.Create(fMaximumSpeed));
 TPasJSONItemObject(result).Add('maximumreversespeed',TPasJSONItemNumber.Create(fMaximumReverseSpeed));
 TPasJSONItemObject(result).Add('steeringangle',TPasJSONItemNumber.Create(fSteeringAngle));
 TPasJSONItemObject(result).Add('frontwheelsgripfactor',TPasJSONItemNumber.Create(fFrontWheelsGripFactor));
 TPasJSONItemObject(result).Add('backwheelsgripfactor',TPasJSONItemNumber.Create(fBackWheelsGripFactor));
 TPasJSONItemObject(result).Add('airresistance',TPasJSONItemNumber.Create(fAirResistance));
end;
{$endif}

{ TKraftSimpleVehicle }

constructor TKraftSimpleVehicle.Create(const aPhysics:TKraft);
begin
 inherited Create;
 fPhysics:=aPhysics;
 fRigidBody:=nil;
 fShape:=nil;
 fSteeringInput:=0;
 fAccelerationInput:=0;
 fForward:=Vector3(0.0,0.0,-1.0);
 fVelocity:=Vector3(0.0,0.0,0.0);
 fSettings:=TKraftSimpleVehicle.TVehicleSettings.Create;
end;

destructor TKraftSimpleVehicle.Destroy;
begin
 FreeAndNil(fSettings);
 inherited Destroy;
end;

procedure TKraftSimpleVehicle.Finish;
begin
 
 if not (assigned(fRigidBody) and assigned(fShape)) then begin
  
  fRigidBody:=TKraftRigidBody.Create(fPhysics);
  fRigidBody.SetRigidBodyType(krbtDYNAMIC);
  fRigidBody.ForcedMass:=fSettings.fChassisMass+(fSettings.fTireMass*CountWheels);
  fRigidBody.CollisionGroups:=[1];
  fRigidBody.CollideWithCollisionGroups:=[0,1];
  fRigidBody.AngularVelocityDamp:=10.0;//10.0;
  fRigidBody.LinearVelocityDamp:=0.3275;

  fShape:=TKraftShapeBox.Create(fPhysics,fRigidBody,Vector3(fSettings.fWidth*0.5,fSettings.fHeight*0.5,fSettings.fLength*0.5));
  fShape.Flags:=fShape.Flags+[ksfHasForcedCenterOfMass];
  fShape.ForcedCenterOfMass.x:=0.0;
  fShape.ForcedCenterOfMass.y:=fSettings.fWheelsHeight;
  fShape.ForcedCenterOfMass.z:=0.0;

  fRigidBody.Finish;

 end;

end;

procedure TKraftSimpleVehicle.SetSteeringInput(const aSteeringInput:TKraftScalar);
begin
 fSteeringInput:=Min(Max(aSteeringInput,-1.0),1.0);
end;

procedure TKraftSimpleVehicle.SetAccelerationInput(const aAccelerationInput:TKraftScalar);
begin
 fAccelerationInput:=Min(Max(aAccelerationInput,-1.0),1.0);
end;

function TKraftSimpleVehicle.GetSpringRelativePosition(const aWheel:TWheel):TKraftVector3;
var BoxSize:TKraftVector3;
begin
 BoxSize:=Vector3(fSettings.fWidth,fSettings.fHeight,fSettings.fLength);
 case aWheel of
  TWheel.FrontLeft:begin
   result:=Vector3(BoxSize.x*(fSettings.fWheelsPaddingX-0.5),fSettings.fWheelsHeight,BoxSize.z*(0.5-fSettings.fWheelsPaddingZ));
  end;
  TWheel.FrontRight:begin
   result:=Vector3(BoxSize.x*(0.5-fSettings.fWheelsPaddingX),fSettings.fWheelsHeight,BoxSize.z*(0.5-fSettings.fWheelsPaddingZ));
  end;
  TWheel.BackLeft:begin
   result:=Vector3(BoxSize.x*(fSettings.fWheelsPaddingX-0.5),fSettings.fWheelsHeight,BoxSize.z*(fSettings.fWheelsPaddingZ-0.5));
  end;
  TWheel.BackRight:begin
   result:=Vector3(BoxSize.x*(0.5-fSettings.fWheelsPaddingX),fSettings.fWheelsHeight,BoxSize.z*(fSettings.fWheelsPaddingZ-0.5));
  end;
  else begin
   result:=Vector3(0.0,0.0,0.0);
  end;
 end; 
end;

function TKraftSimpleVehicle.GetSpringPosition(const aWheel:TWheel):TKraftVector3;
begin
 result:=Vector3TermMatrixMul(GetSpringRelativePosition(aWheel),fWorldTransform);
end;

function TKraftSimpleVehicle.GetSpringHitPosition(const aWheel:TWheel):TKraftVector3;
begin
 result:=Vector3Add(GetSpringPosition(aWheel),Vector3ScalarMul(fWorldDown,fSpringDatas[aWheel].fCurrentLength));
end;

function TKraftSimpleVehicle.GetWheelRollDirection(const aWheel:TWheel):TKraftVector3;
begin
 if aWheel in [TWheel.FrontLeft,TWheel.FrontRight] then begin
  result:=Vector3TermQuaternionRotate(fWorldForward,QuaternionFromAxisAngle(Vector3(0.0,1.0,0.0),fWheelDatas[aWheel].fYawRad));
 end else begin
  result:=fWorldForward;
 end;
end;

function TKraftSimpleVehicle.GetWheelSlideDirection(const aWheel:TWheel):TKraftVector3;
begin
 result:=Vector3Cross(fWorldUp,GetWheelRollDirection(aWheel));
end;

function TKraftSimpleVehicle.GetWheelTorqueRelativePosition(const aWheel:TWheel):TKraftVector3;
var BoxSize:TKraftVector3;
begin
 BoxSize:=Vector3(fSettings.fWidth,fSettings.fHeight,fSettings.fLength);
 case aWheel of
  TWheel.FrontLeft:begin
   result:=Vector3(BoxSize.x*(fSettings.fWheelsPaddingX-0.5),0.0,BoxSize.z*(0.5-fSettings.fWheelsPaddingZ));
  end;
  TWheel.FrontRight:begin
   result:=Vector3(BoxSize.x*(0.5-fSettings.fWheelsPaddingX),0.0,BoxSize.z*(0.5-fSettings.fWheelsPaddingZ));
  end;
  TWheel.BackLeft:begin
   result:=Vector3(BoxSize.x*(fSettings.fWheelsPaddingX-0.5),0.0,BoxSize.z*(fSettings.fWheelsPaddingZ-0.5));
  end;
  TWheel.BackRight:begin
   result:=Vector3(BoxSize.x*(0.5-fSettings.fWheelsPaddingX),0.0,BoxSize.z*(fSettings.fWheelsPaddingZ-0.5));
  end;
  else begin
   result:=Vector3(0.0,0.0,0.0);
  end;
 end;
end;

function TKraftSimpleVehicle.GetWheelTorquePosition(const aWheel:TWheel):TKraftVector3;
begin
 result:=Vector3TermMatrixMul(GetWheelTorqueRelativePosition(aWheel),fWorldTransform);
end;

function TKraftSimpleVehicle.GetWheelGripFactor(const aWheel:TWheel):TKraftScalar;
begin
 if aWheel in [TWheel.FrontLeft,TWheel.FrontRight] then begin
  result:=fSettings.fFrontWheelsGripFactor;
 end else begin
  result:=fSettings.fBackWheelsGripFactor;
 end;
end;

function TKraftSimpleVehicle.GetWheelTransform(const aWheel:TWheel):TKraftMatrix4x4;
var {LocalWheelPosition,}WorldWheelPosition:TKraftVector3;
    LocalWheelRotation,WorldWheelRotation:TKraftQuaternion;
begin
 LocalWheelRotation:=QuaternionFromAngles(fWheelDatas[aWheel].fYawRad,0.0,fWheelDatas[aWheel].fRotationRad);
 WorldWheelPosition:=Vector3Add(GetSpringPosition(aWheel),Vector3ScalarMul(fWorldDown,fSpringDatas[aWheel].fCurrentLength-fSettings.fWheelsRadius));
 WorldWheelRotation:=QuaternionMul(fRigidBody.Sweep.q,LocalWheelRotation);
 result:=QuaternionToMatrix4x4(WorldWheelRotation);
 PKraftVector3(@result[3,0])^.xyz:=WorldWheelPosition.xyz;
end;

function TKraftSimpleVehicle.IsGrounded(const aWheel:TWheel):boolean;
begin
 result:=fSpringDatas[aWheel].fCurrentLength<fSettings.fSpringRestLength;
end;

procedure TKraftSimpleVehicle.CastSpring(const aWheel:TWheel);
var RayOrigin,RayDirection,HitPoint,HitNormal:TKraftVector3;
    RayLength,PreviousLength,CurrentLength,HitTime:TKraftScalar;
    HitShape:TKraftShape;
begin
 RayOrigin:=GetSpringPosition(aWheel);
 PreviousLength:=fSpringDatas[aWheel].fCurrentLength;
 RayDirection:=fWorldDown;
 RayLength:=fSettings.fSpringRestLength;
 if fPhysics.RayCast(RayOrigin,RayDirection,RayLength,HitShape,HitTime,HitPoint,HitNormal,[0],nil) then begin
  CurrentLength:=HitTime;
 end else begin
  CurrentLength:=fSettings.fSpringRestLength;
 end;
 fSpringDatas[aWheel].fCurrentVelocity:=(CurrentLength-PreviousLength)*fInverseDeltaTime;
 fSpringDatas[aWheel].fCurrentLength:=CurrentLength;
end;

procedure TKraftSimpleVehicle.UpdateWorldTransformVectors;
begin
 fWorldTransform:=fRigidBody.WorldTransform;
 fWorldRight:=Vector3(PKraftRawVector3(pointer(@fWorldTransform[0,0]))^);
 fWorldLeft:=Vector3Neg(fWorldRight);
 fWorldUp:=Vector3(PKraftRawVector3(pointer(@fWorldTransform[1,0]))^);
 fWorldDown:=Vector3Neg(fWorldUp);
 fWorldForward:=Vector3(PKraftRawVector3(pointer(@fWorldTransform[2,0]))^);
 fWorldBackward:=Vector3Neg(fWorldForward);
 fWorldPosition:=Vector3(PKraftRawVector3(pointer(@fWorldTransform[3,0]))^);
end;

procedure TKraftSimpleVehicle.UpdateSuspension;
var Wheel:TWheel;
    CurrentLength,CurrentVelocity,Force:TKraftScalar;
begin
 for Wheel:=Low(TWheel) to High(TWheel) do begin
  CastSpring(Wheel);
  CurrentLength:=fSpringDatas[Wheel].fCurrentLength;
  CurrentVelocity:=fSpringDatas[Wheel].fCurrentVelocity;
  Force:=TKraftSimpleVehicle.TSpringMath.CalculateForceDamped(CurrentLength,
                                                              CurrentVelocity,
                                                              fSettings.fSpringRestLength,
                                                              fSettings.fSpringStrength,
                                                              fSettings.fSpringDamper);
  if abs(Force)>EPSILON then begin
   fRigidBody.AddForceAtPosition(Vector3ScalarMul(fWorldUp,Force),GetSpringPosition(Wheel),kfmForce,true);
  end; 
 end;
end;

procedure TKraftSimpleVehicle.CalculateAckermannSteering;
var SteerAngleRad,AxleSeparation,WheelSeparation,TurningCircleRadius:TKraftScalar;
    AxleDiff,WheelDiff:TKraftVector3;
begin

 SteerAngleRad:=fSteeringInput*fSettings.fSteeringAngle*DEG2RAD;

 AxleDiff:=Vector3Sub(Vector3Avg(GetSpringPosition(TWheel.FrontLeft),GetSpringPosition(TWheel.FrontRight)),
                      Vector3Avg(GetSpringPosition(TWheel.BackLeft),GetSpringPosition(TWheel.BackRight)));
 AxleSeparation:=Vector3Length(AxleDiff);

 WheelDiff:=Vector3Sub(GetSpringPosition(TWheel.FrontLeft),GetSpringPosition(TWheel.FrontRight));
 WheelSeparation:=Vector3Length(WheelDiff);

 TurningCircleRadius:=AxleSeparation/Tan(SteerAngleRad);
 if IsNaN(TurningCircleRadius) then begin
  TurningCircleRadius:=0.0;
 end;

 fWheelDatas[TKraftSimpleVehicle.TWheel.FrontLeft].fYawRad:=ArcTan(AxleSeparation/(TurningCircleRadius+(WheelSeparation*0.5)));
 fWheelDatas[TKraftSimpleVehicle.TWheel.FrontRight].fYawRad:=ArcTan(AxleSeparation/(TurningCircleRadius-(WheelSeparation*0.5)));

 fWheelDatas[TKraftSimpleVehicle.TWheel.BackLeft].fYawRad:=0.0;
 fWheelDatas[TKraftSimpleVehicle.TWheel.BackRight].fYawRad:=0.0;

end;

procedure TKraftSimpleVehicle.UpdateSteering;
var Wheel:TWheel;
    SpringPosition,SlideDirection,Force:TKraftVector3;
    SlideVelocity,DesiredVelocityChange,DesiredAcceleration:TKraftScalar;
begin
 CalculateAckermannSteering;
 for Wheel:=Low(TWheel) to High(TWheel) do begin
  if IsGrounded(Wheel) then begin
   SpringPosition:=GetSpringPosition(Wheel);
   SlideDirection:=GetWheelSlideDirection(Wheel);
   SlideVelocity:=Vector3Dot(SlideDirection,fRigidBody.GetWorldLinearVelocityFromPoint(SpringPosition));
   DesiredVelocityChange:=-SlideVelocity*GetWheelGripFactor(Wheel);
   DesiredAcceleration:=DesiredVelocityChange*fInverseDeltaTime;
   Force:=Vector3ScalarMul(SlideDirection,DesiredAcceleration*fSettings.fTireMass);
   if Vector3Length(Force)>EPSILON then begin
    fRigidBody.AddForceAtPosition(Force,GetWheelTorquePosition(Wheel),kfmForce,true);
   end; 
  end; 
 end;
end; 

procedure TKraftSimpleVehicle.UpdateAcceleration;
var Wheel:TWheel;
    ForwardSpeed,Speed:TKraftScalar;
    MovingForward:boolean;
    WheelForward,Force:TKraftVector3;
begin
 if not IsZero(fAccelerationInput) then begin
  ForwardSpeed:=Vector3Dot(fWorldForward,fRigidBody.LinearVelocity);
  MovingForward:=ForwardSpeed>0.0;
  Speed:=abs(ForwardSpeed);
  for Wheel:=Low(TWheel) to High(TWheel) do begin
   if IsGrounded(Wheel) and
      ((MovingForward and (Speed<fSettings.fMaximumSpeed)) or 
       ((not MovingForward) and (Speed<fSettings.fMaximumReverseSpeed))) then begin

    WheelForward:=GetWheelRollDirection(Wheel);

    Force:=Vector3ScalarMul(WheelForward,fAccelerationInput*fSettings.fAccelerationPower);

    if Vector3Length(Force)>EPSILON then begin
     fRigidBody.AddForceAtPosition(Force,GetWheelTorquePosition(Wheel),kfmForce,true);
    end;

   end; 
  end;
 end; 
end; 

procedure TKraftSimpleVehicle.UpdateBraking;
const AlmostStoppedSpeed=0.1;
var Wheel:TWheel;
    ForwardSpeed,Speed,BrakeRatio,RollVelocity,DesiredVelocityChange,DesiredAcceleration:TKraftScalar;
    AlmostStopping,AccelerationContrary:boolean;
    SpringPosition,RollDirection,Force:TKraftVector3;
begin

 ForwardSpeed:=Vector3Dot(fWorldForward,fRigidBody.LinearVelocity);
 Speed:=abs(ForwardSpeed);
 AlmostStopping:=Speed<AlmostStoppedSpeed;
 if AlmostStopping then begin
  BrakeRatio:=1.0;
 end else begin
  AccelerationContrary:=IsZero(fAccelerationInput) and (Vector3Dot(Vector3ScalarMul(fWorldForward,fAccelerationInput),fRigidBody.LinearVelocity)<0.0);
  if AccelerationContrary then begin
   BrakeRatio:=1.0;
  end else if IsZero(fAccelerationInput) then begin
   BrakeRatio:=0.1;
  end else begin
   exit;
  end;
 end;
 
 for Wheel:=Low(TWheel) to High(TWheel) do begin
  if IsGrounded(Wheel) then begin
   SpringPosition:=GetSpringPosition(Wheel);
   RollDirection:=GetWheelRollDirection(Wheel);
   RollVelocity:=Vector3Dot(RollDirection,fRigidBody.GetWorldLinearVelocityFromPoint(SpringPosition));
   DesiredVelocityChange:=-RollVelocity*BrakeRatio*fSettings.fBrakePower;
   DesiredAcceleration:=DesiredVelocityChange*fInverseDeltaTime;
   Force:=Vector3ScalarMul(RollDirection,DesiredAcceleration*fSettings.fTireMass);
   if Vector3Length(Force)>EPSILON then begin
    fRigidBody.AddForceAtPosition(Force,GetWheelTorquePosition(Wheel),kfmForce,true);
   end;
  end; 
 end;

end;

procedure TKraftSimpleVehicle.UpdateAirResistance;
var Force:TKraftVector3;
begin
 Force:=Vector3ScalarMul(fVelocity,-fSettings.fAirResistance*Vector3Length(Vector3(fSettings.fWidth,fSettings.fHeight,fSettings.fLength)));
 if Vector3Length(Force)>EPSILON then begin
  fRigidBody.AddWorldForce(Force,kfmForce,true);
 end;
end;

procedure TKraftSimpleVehicle.UpdateVisuals;
var Wheel:TWheel;
begin
 for Wheel:=Low(TWheel) to High(TWheel) do begin
  fWheelDatas[Wheel].fWorldTransform:=GetWheelTransform(Wheel);
 end;
end;

procedure TKraftSimpleVehicle.Update(const aDeltaTime:TKraftScalar);
begin
 fDeltaTime:=aDeltaTime;
 fInverseDeltaTime:=1.0/fDeltaTime;
 fSteeringInput:=Min(Max(fInputHorizontal,-1.0),1.0);
 fAccelerationInput:=Min(Max(fInputVertical,-1.0),1.0);
 UpdateWorldTransformVectors;
 UpdateSuspension;
 UpdateSteering;
 UpdateAcceleration;
 UpdateBraking;
 UpdateAirResistance;
 UpdateVisuals;
end;

procedure TKraftSimpleVehicle.StoreWorldTransforms;
var Wheel:TWheel;
begin
 UpdateWorldTransformVectors;
 for Wheel:=Low(TWheel) to High(TWheel) do begin
  fWheelDatas[Wheel].fLastWorldTransform:=fWheelDatas[Wheel].fWorldTransform;
 end;
 fLastWorldTransform:=fWorldTransform;
 fLastWorldRight:=Vector3(PKraftRawVector3(pointer(@fLastWorldTransform[0,0]))^);
 fLastWorldLeft:=Vector3Neg(fLastWorldRight);
 fLastWorldUp:=Vector3(PKraftRawVector3(pointer(@fLastWorldTransform[1,0]))^);
 fLastWorldDown:=Vector3Neg(fLastWorldUp);
 fLastWorldForward:=Vector3(PKraftRawVector3(pointer(@fLastWorldTransform[2,0]))^);
 fLastWorldBackward:=Vector3Neg(fLastWorldForward);
 fLastWorldPosition:=Vector3(PKraftRawVector3(pointer(@fLastWorldTransform[3,0]))^);
end;

procedure TKraftSimpleVehicle.InterpolateWorldTransforms(const aAlpha:TKraftScalar);
var Wheel:TWheel;
begin
 UpdateWorldTransformVectors;
 for Wheel:=Low(TWheel) to High(TWheel) do begin
  fWheelDatas[Wheel].fVisualWorldTransform:=Matrix4x4Slerp(fWheelDatas[Wheel].fLastWorldTransform,fWheelDatas[Wheel].fWorldTransform,aAlpha);
 end;
 fVisualWorldTransform:=Matrix4x4Slerp(fLastWorldTransform,fWorldTransform,aAlpha);
 fVisualWorldRight:=Vector3(PKraftRawVector3(pointer(@fVisualWorldTransform[0,0]))^);
 fVisualWorldLeft:=Vector3Neg(fVisualWorldRight);
 fVisualWorldUp:=Vector3(PKraftRawVector3(pointer(@fVisualWorldTransform[1,0]))^);
 fVisualWorldDown:=Vector3Neg(fVisualWorldUp);
 fVisualWorldForward:=Vector3(PKraftRawVector3(pointer(@fVisualWorldTransform[2,0]))^);
 fVisualWorldBackward:=Vector3Neg(fVisualWorldForward);
 fVisualWorldPosition:=Vector3(PKraftRawVector3(pointer(@fVisualWorldTransform[3,0]))^);
end;

{$ifdef DebugDraw}
procedure TKraftSimpleVehicle.DebugDraw;
var Wheel:TWheel;
    Index:TKraftInt32;
    v0,v1,v2,v3:TKraftVector3;
    Color:TKraftVector4;
begin
{$ifndef NoOpenGL}
 glDisable(GL_DEPTH_TEST);
{$endif}
 for Wheel:=Low(TWheel) to High(TWheel) do begin
  if IsGrounded(Wheel) then begin
   Color:=Vector4(0.0,0.0,1.0,1.0);
  end else begin
   Color:=Vector4(1.0,0.0,1.0,1.0);
  end;

  v2:=Vector3TermMatrixMul(GetSpringRelativePosition(Wheel),fVisualWorldTransform);
  v0:=Vector3Add(v2,Vector3ScalarMul(fVisualWorldLeft,0.1));
  v1:=Vector3Add(v2,Vector3ScalarMul(fVisualWorldRight,0.1));
{$ifdef NoOpenGL}
  if assigned(fDebugDrawLine) then begin
   fDebugDrawLine(v0,v1,Color);
  end;
{$else}
  glColor4fv(@Color);
  glBegin(GL_LINE_STRIP);
  glVertex3fv(@v0);
  glVertex3fv(@v1);
  glEnd;
{$endif}

  v0:=Vector3TermMatrixMul(GetSpringRelativePosition(Wheel),fVisualWorldTransform);
  v1:=Vector3Add(v0,Vector3ScalarMul(fVisualWorldDown,fSpringDatas[Wheel].fCurrentLength));
{$ifdef NoOpenGL}
  if assigned(fDebugDrawLine) then begin
   fDebugDrawLine(v0,v1,Color);
  end;
{$else}
  glColor4fv(@Color);
  glBegin(GL_LINE_STRIP);
  glVertex3fv(@v0);
  glVertex3fv(@v1);
  glEnd;
{$endif}

{$ifdef NoOpenGL}
  v:=Vector3TermMatrixMul(Vector3Origin,fWheelDatas[Wheel].fVisualWorldTransform);
  v0:=v;
  for Index:=0 to 16 do begin
   if assigned(fVehicle.fDebugDrawLine) then begin
    v1:=v0;
    v0:=Vector3TermMatrixMul(Vector3Add(Vector3Add(Vector3Origin,Vector3ScalarMul(Vector3YAxis,Sin((Index/16)*PI*2))),Vector3ScalarMul(Vector3ZAxis,Cos((Index/16)*PI*2))),fWheelDatas[Wheel].fVisualWorldTransform);
    if Index>0 then begin
     fVehicle.fDebugDrawLine(v,v0,Vector4(1.0,1.0,1.0,1.0));
     fVehicle.fDebugDrawLine(v0,v1,Vector4(1.0,1.0,1.0,1.0));
    end;
   end;
  end;
{$else}
  glColor4f(1.0,1.0,1.0,1.0);
  glDisable(GL_CULL_FACE);
  glBegin(GL_TRIANGLE_FAN);
  v0:=Vector3TermMatrixMul(Vector3Origin,fWheelDatas[Wheel].fVisualWorldTransform);
  glVertex3fv(@v0);
  for Index:=0 to 16 do begin
   v0:=Vector3TermMatrixMul(Vector3Add(Vector3Add(Vector3Origin,Vector3ScalarMul(Vector3YAxis,Sin((Index/16)*PI*2))),Vector3ScalarMul(Vector3ZAxis,Cos((Index/16)*PI*2))),fWheelDatas[Wheel].fVisualWorldTransform);
   glVertex3fv(@v0);
  end;
  glEnd;
  glEnable(GL_CULL_FACE);
{$endif}

 end;
{$ifndef NoOpenGL}
 glEnable(GL_DEPTH_TEST);
{$endif}
end;
{$endif}

end.

