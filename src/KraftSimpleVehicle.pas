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
            { TEnvelope }
            TEnvelope=class
             public
              type TMode=
                    (
                     Custom,
                     Linear,
                     EaseInOut
                    );
              type TPoint=record
                    private
                     fTime:TKraftScalar;
                     fValue:TKraftScalar;
                    public
                     property Time:TKraftScalar read fTime write fTime;
                     property Value:TKraftScalar read fValue write fValue;
                   end;
                   PPoint=^TPoint;
                   TPoints=array of TPoint;
             private
              fMode:TMode;
              fPoints:TPoints;
              fCount:TKraftInt32;
             public
              constructor Create; reintroduce;
              constructor CreateLinear(const aTimeStart,aValueStart,aTimeEnd,aValueEnd:TKraftScalar);
              constructor CreateEaseInOut(const aTimeStart,aValueStart,aTimeEnd,aValueEnd:TKraftScalar;const aSteps:TKraftInt32=16);
              destructor Destroy; override;
              procedure Clear;
              procedure Assign(const aFrom:TEnvelope);
              procedure Insert(const aTime,aValue:TKraftScalar);
              procedure FillLinear(const aTimeStart,aValueStart,aTimeEnd,aValueEnd:TKraftScalar);
              procedure FillEaseInOut(const aTimeStart,aValueStart,aTimeEnd,aValueEnd:TKraftScalar;const aSteps:TKraftInt32=16);
{$ifdef KraftPasJSON}
              procedure LoadFromJSON(const aJSONItem:TPasJSONItem);
              function SaveToJSON:TPasJSONItem;
{$endif}
              function GetTimeAtIndex(const aIndex:TKraftInt32):TKraftScalar;
              function GetValueAtIndex(const aIndex:TKraftInt32):TKraftScalar;
              function GetIndexFromTime(const aTime:TKraftScalar):TKraftInt32;
              function GetValueAtTime(const aTime:TKraftScalar):TKraftScalar;
             published
              property Count:TKraftInt32 read fCount;
            end;
            { TSpringMath }
            TSpringMath=class
             public
              class function CalculateForce(const aCurrentLength,aRestLength,aStrength:TKraftScalar):TKraftScalar; static;
              class function CalculateForceDamped(const aCurrentLength,aLengthVelocity,aRestLength,aStrength,aDamper:TKraftScalar):TKraftScalar; static;
            end; 
            { TSpring }
            TSpring=record
             private
              fCurrentLength:TKraftScalar;
              fCurrentVelocity:TKraftScalar;
              fCompression:TKraftScalar;
            end; 
            PSpring=^TSpring;
            { TWheelID }
            TWheelID=
             (
              FrontLeft=0,
              FrontRight=1,
              BackLeft=2,
              BackRight=3   
             );
            PWheelID=^TWheelID;
            { TWheel }
            TWheel=class
             private
              fVehicle:TKraftSimpleVehicle;
              fWheelID:TKraftSimpleVehicle.TWheelID;
              fSpring:TSpring;
              fYawRad:TKraftScalar;
              fRotationRad:TKraftScalar;
              fWorldTransform:TKraftMatrix4x4;
              fLastWorldTransform:TKraftMatrix4x4;
              fVisualWorldTransform:TKraftMatrix4x4;
{$ifdef DebugDraw}
              fDebugAntiRollForce:TKraftVector3;
              fLastDebugAntiRollForce:TKraftVector3;
              fVisualDebugAntiRollForce:TKraftVector3;
              fDebugAccelerationForce:TKraftVector3;
              fLastDebugAccelerationForce:TKraftVector3;
              fVisualDebugAccelerationForce:TKraftVector3;
              fDebugDeaccelerationForce:TKraftVector3;
              fLastDebugDeaccelerationForce:TKraftVector3;
              fVisualDebugDeaccelerationForce:TKraftVector3;
              fDebugSlideForce:TKraftVector3;
              fLastDebugSlideForce:TKraftVector3;
              fVisualDebugSlideForce:TKraftVector3;
{$endif}
              function GetSpringHitPosition:TKraftVector3;
              function GetSpringPosition:TKraftVector3;
              function GetSpringRelativePosition:TKraftVector3;
              function GetWheelGripFactor:TKraftScalar;
              function GetWheelRollDirection:TKraftVector3;
              function GetWheelSlideDirection:TKraftVector3;
              function GetWheelTorquePosition:TKraftVector3;
              function GetWheelTorqueRelativePosition:TKraftVector3;
              function GetWheelTransform:TKraftMatrix4x4;
              function IsGrounded:boolean;
              procedure CastSpring;
              procedure UpdateSuspension;
              procedure UpdateSteering;
              procedure UpdateAcceleration;
              procedure UpdateBraking;
              procedure UpdateWheelRotation;
              procedure UpdateVisuals;
              procedure StoreWorldTransforms;
              procedure InterpolateWorldTransforms(const aAlpha:TKraftScalar);
             public
              constructor Create(const aVehicle:TKraftSimpleVehicle;const aWheelID:TKraftSimpleVehicle.TWheelID); reintroduce;
              destructor Destroy; override;
            end;
            { TWheels }
            TWheels=array[TWheelID] of TWheel;
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
              fStabilizerBarAntiRollForce:TKraftScalar;
              fAccelerationForce:TKraftScalar;
              fBrakeForce:TKraftScalar;
              fRollFriction:TKraftScalar;
              fMaximumSpeed:TKraftScalar;
              fMaximumReverseSpeed:TKraftScalar;
              fFrontWheelsGripFactor:TKraftScalar;
              fBackWheelsGripFactor:TKraftScalar;
              fAirResistance:TKraftScalar;
              fHandBrakeSlipperyTime:TKraftScalar;
              fUseAccelerationCurveEnvelopes:Boolean;
              fAccelerationCurveEnvelope:TEnvelope;
              fReverseAccelerationCurveEnvelope:TEnvelope;
              fReverseEvaluationAccuracy:TKraftInt32;
              fSteerAngleLimitEnvelope:TEnvelope;
              fSteeringResetSpeedEnvelope:TEnvelope;
              fSteeringSpeedEnvelope:TEnvelope;
              fDownForceCurveEnvelope:TEnvelope;
              fDownForce:TKraftScalar;
              fFlightStabilizationDamping:TKraftScalar;
              fFlightStabilizationForce:TKraftScalar;
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
              property StabilizerBarAntiRollForce:TKraftScalar read fStabilizerBarAntiRollForce write fStabilizerBarAntiRollForce;
              property AccelerationForce:TKraftScalar read fAccelerationForce write fAccelerationForce;
              property BrakeForce:TKraftScalar read fBrakeForce write fBrakeForce;
              property RollFriction:TKraftScalar read fRollFriction write fRollFriction;
              property MaximumSpeed:TKraftScalar read fMaximumSpeed write fMaximumSpeed;
              property MaximumReverseSpeed:TKraftScalar read fMaximumReverseSpeed write fMaximumReverseSpeed;
              property FrontWheelsGripFactor:TKraftScalar read fFrontWheelsGripFactor write fFrontWheelsGripFactor;
              property BackWheelsGripFactor:TKraftScalar read fBackWheelsGripFactor write fBackWheelsGripFactor;
              property AirResistance:TKraftScalar read fAirResistance write fAirResistance;
              property HandBrakeSlipperyTime:TKraftScalar read fHandBrakeSlipperyTime write fHandBrakeSlipperyTime;
              property UseAccelerationCurveEnvelopes:Boolean read fUseAccelerationCurveEnvelopes write fUseAccelerationCurveEnvelopes;
              property AccelerationCurveEnvelope:TEnvelope read fAccelerationCurveEnvelope;
              property ReverseAccelerationCurveEnvelope:TEnvelope read fReverseAccelerationCurveEnvelope;
              property ReverseEvaluationAccuracy:TKraftInt32 read fReverseEvaluationAccuracy write fReverseEvaluationAccuracy;
              property SteerAngleLimitEnvelope:TEnvelope read fSteerAngleLimitEnvelope;
              property SteeringResetSpeedEnvelope:TEnvelope read fSteeringResetSpeedEnvelope;
              property SteeringSpeedEnvelope:TEnvelope read fSteeringSpeedEnvelope;
              property DownForceCurveEnvelope:TEnvelope read fDownForceCurveEnvelope;              
              property DownForce:TKraftScalar read fDownForce write fDownForce;
              property FlightStabilizationDamping:TKraftScalar read fFlightStabilizationDamping write fFlightStabilizationDamping;
              property FlightStabilizationForce:TKraftScalar read fFlightStabilizationForce write fFlightStabilizationForce;
            end;
      private
       fPhysics:TKraft;
       fRigidBody:TKraftRigidBody;
       fShape:TKraftShape;
       fWheels:TWheels;
       fControllable:boolean;
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
       fIsAcceleration:Boolean;
       fIsReverseAcceleration:Boolean;
       fIsBrake:Boolean;
       fIsHandBrake:Boolean;
       fAfterFlightSlipperyTiresTime:TKraftScalar;
       fBrakeSlipperyTiresTime:TKraftScalar;
       fHandBrakeSlipperyTiresTime:TKraftScalar;
       fSteeringAngle:TKraftScalar;
       fAccelerationForceMagnitude:TKraftScalar;
       fRelativeSpeed:TKraftScalar;
       fMovingForward:boolean;
       fAbsoluteSpeed:TKraftScalar;
       fSpeed:TKraftScalar;
       fSpeedKMH:TKraftScalar;
{$ifdef DebugDraw}
       fDebugAirResistanceForce:TKraftVector3;
       fLastDebugAirResistanceForce:TKraftVector3;
       fVisualDebugAirResistanceForce:TKraftVector3;
       fDebugDownForce:TKraftVector3;
       fLastDebugDownForce:TKraftVector3;
       fVisualDebugDownForce:TKraftVector3;
{$endif}
       procedure CalculateAckermannSteering;
       function GetHandBrakeK:TKraftScalar;
       function GetSteeringHandBrakeK:TKraftScalar;
       function GetSteerAngleLimitInDeg(const aSpeedMetersPerSec:TKraftScalar):TKraftScalar;
       function GetSpeed:TKraftScalar;
       function GetAccelerationForceMagnitude(const aEnvelope:TEnvelope;const aSpeedMetersPerSec,aDeltaTime:TKraftScalar):TKraftScalar;
       function CalcAccelerationForceMagnitude:TKraftScalar;
       procedure UpdateGlobals;
       procedure UpdateInput;
       procedure UpdateWorldTransformVectors;
       procedure UpdateSuspension;
       procedure UpdateSteering;
       procedure UpdateAcceleration;
       procedure UpdateBraking;
       procedure UpdateAntiRollBar;
       procedure UpdateAirResistance;
       procedure UpdateDownForce;
       procedure UpdateFlightStabilization;
       procedure UpdateWheelRotations;
       procedure UpdateVisuals;
      public
       constructor Create(const aPhysics:TKraft); reintroduce;
       destructor Destroy; override;
       procedure Reset;
       procedure Finish;
       procedure Update(const aDeltaTime:TKraftScalar);
       procedure StoreWorldTransforms;
       procedure InterpolateWorldTransforms(const aAlpha:TKraftScalar);
{$ifdef DebugDraw}
       procedure DebugDraw;
{$endif}
      public
       property Settings:TVehicleSettings read fSettings write fSettings;
      published
       property Physics:TKraft read fPhysics;
       property RigidBody:TKraftRigidBody read fRigidBody write fRigidBody;
       property Shape:TKraftShape read fShape write fShape;
       property Controllable:boolean read fControllable write fControllable;
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

function Clamp(const aValue,aMin,aMax:TKraftScalar):TKraftScalar; inline;
begin
 if aValue<=aMin then begin
  result:=aMin;
 end else if aValue>=aMax then begin
  result:=aMax;
 end else begin
  result:=aValue;
 end;
end;

function Clamp01(const aValue:TKraftScalar):TKraftScalar; inline;
begin
 if aValue<=0.0 then begin
  result:=0.0;
 end else if aValue>=1.0 then begin
  result:=1.0;
 end else begin
  result:=aValue;
 end;
end;

function Lerp(a,b,x:TKraftScalar):TKraftScalar; inline;
begin
 if x<=0.0 then begin
  result:=a;
 end else if x>=1.0 then begin
  result:=b;
 end else begin
  result:=(a*(1.0-x))+(b*x);
 end;
end;

{ TEnvelope }

constructor TKraftSimpleVehicle.TEnvelope.Create;
begin
 inherited Create;
 fMode:=TKraftSimpleVehicle.TEnvelope.TMode.Custom;
 fPoints:=nil;
 fCount:=0;
end;

constructor TKraftSimpleVehicle.TEnvelope.CreateLinear(const aTimeStart,aValueStart,aTimeEnd,aValueEnd:TKraftScalar);
begin
 Create;
 FillLinear(aTimeStart,aValueStart,aTimeEnd,aValueEnd);
end;

constructor TKraftSimpleVehicle.TEnvelope.CreateEaseInOut(const aTimeStart,aValueStart,aTimeEnd,aValueEnd:TKraftScalar;const aSteps:TKraftInt32=16);
begin
 Create;
 FillEaseInOut(aTimeStart,aValueStart,aTimeEnd,aValueEnd,aSteps);
end;

destructor TKraftSimpleVehicle.TEnvelope.Destroy;
begin
 fPoints:=nil;
 inherited Destroy;
end;

procedure TKraftSimpleVehicle.TEnvelope.Clear;
begin
 fMode:=TKraftSimpleVehicle.TEnvelope.TMode.Custom;
 fPoints:=nil;
 fCount:=0;
end;

procedure TKraftSimpleVehicle.TEnvelope.Assign(const aFrom:TEnvelope);
begin
 fMode:=aFrom.fMode;
 fPoints:=copy(aFrom.fPoints);
 fCount:=aFrom.fCount;
end;

procedure TKraftSimpleVehicle.TEnvelope.Insert(const aTime,aValue:TKraftScalar);
var Index,LowIndex,HighIndex,MidIndex:TKraftInt32;
    Point:PPoint;
begin
 if fCount>0 then begin
  if aTime<fPoints[0].fTime then begin
   LowIndex:=0;
  end else if fPoints[fCount-1].fTime<aTime then begin
   LowIndex:=fCount;
  end else begin
   LowIndex:=0;
   HighIndex:=fCount-1;
   while LowIndex<=HighIndex do begin
    MidIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
    Point:=@fPoints[MidIndex];
    case Sign(Point^.fTime-aTime) of
     -1:begin
      LowIndex:=MidIndex+1;
     end;
     1:begin
      HighIndex:=MidIndex-1;
     end;
     else begin
      Point^.fValue:=aValue;
      exit;
     end;
    end;
   end;
  end;
  inc(fCount);
  if length(fPoints)<fCount then begin
   SetLength(fPoints,(fCount*3) shr 1);
  end;
  for Index:=fCount-1 downto LowIndex+1 do begin
   fPoints[Index]:=fPoints[Index-1];
  end;
  Index:=LowIndex;
 end else begin
  fCount:=1;
  SetLength(fPoints,1);
  Index:=0;
 end;
 fMode:=TKraftSimpleVehicle.TEnvelope.TMode.Custom;
 Point:=@fPoints[Index];
 Point^.fTime:=aTime;
 Point^.fValue:=aValue;
end;

procedure TKraftSimpleVehicle.TEnvelope.FillLinear(const aTimeStart,aValueStart,aTimeEnd,aValueEnd:TKraftScalar);
begin
 Clear;
 Insert(aTimeStart,aValueStart);
 Insert(aTimeEnd,aValueEnd);
 fMode:=TKraftSimpleVehicle.TEnvelope.TMode.Linear;
end;

procedure TKraftSimpleVehicle.TEnvelope.FillEaseInOut(const aTimeStart,aValueStart,aTimeEnd,aValueEnd:TKraftScalar;const aSteps:TKraftInt32=16);
var Index,Last:TKraftInt32;
    x,Time,Value:TKraftScalar;
begin
 Clear;
 Last:=aSteps-1;
 for Index:=0 to Last do begin
  x:=Index/Last;
  Time:=(aTimeStart*(1.0-x))+(aTimeEnd*x);
  if x<0.5 then begin
   x:=(1-sqrt(1-sqr(2*x)))*0.5;
  end else begin
   x:=(sqrt(1-sqr((-2*x)+2))+1)*0.5;
  end;
  Value:=(aValueStart*(1.0-x))+(aValueEnd*x);
  Insert(Time,Value);
 end;
 fMode:=TKraftSimpleVehicle.TEnvelope.TMode.EaseInOut;
end;

{$ifdef KraftPasJSON}
procedure TKraftSimpleVehicle.TEnvelope.LoadFromJSON(const aJSONItem:TPasJSONItem);
var RootJSONItemObject:TPasJSONItemObject;
    Mode:TPasJSONUTF8String;
    JSONItem:TPasJSONItem;
    JSONItemArray:TPasJSONItemArray;
    JSONItemObject:TPasJSONItemObject;
begin
 if assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject) then begin
  RootJSONItemObject:=TPasJSONItemObject(aJSONItem);
  Mode:=TPasJSON.GetString(RootJSONItemObject.Properties['mode'],'');
  if Mode='linear' then begin
   FillLinear(TPasJSON.GetNumber(RootJSONItemObject.Properties['timestart'],0.0),
              TPasJSON.GetNumber(RootJSONItemObject.Properties['valuestart'],0.0),
              TPasJSON.GetNumber(RootJSONItemObject.Properties['timeend'],0.0),
              TPasJSON.GetNumber(RootJSONItemObject.Properties['valueend'],0.0));
  end else if Mode='easeinout' then begin
   FillEaseInOut(TPasJSON.GetNumber(RootJSONItemObject.Properties['timestart'],0.0),
                 TPasJSON.GetNumber(RootJSONItemObject.Properties['valuestart'],0.0),
                 TPasJSON.GetNumber(RootJSONItemObject.Properties['timeend'],0.0),
                 TPasJSON.GetNumber(RootJSONItemObject.Properties['valueend'],0.0),
                 TPasJSON.GetInt64(RootJSONItemObject.Properties['steps'],16));
  end else if Mode='custom' then begin
   Clear;
   JSONItem:=RootJSONItemObject.Properties['points'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) then begin
    JSONItemArray:=TPasJSONItemArray(JSONItem);
    for JSONItem in JSONItemArray do begin
     if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
      JSONItemObject:=TPasJSONItemObject(JSONItem);
      Insert(TPasJSON.GetNumber(JSONItemObject.Properties['time'],0.0),TPasJSON.GetNumber(JSONItemObject.Properties['value'],0.0));
     end;
    end;
   end;
  end;
 end;
end;

function TKraftSimpleVehicle.TEnvelope.SaveToJSON:TPasJSONItem;
var Index:TKraftSizeInt;
    JSONItemArray:TPasJSONItemArray;
    JSONItemObject:TPasJSONItemObject;
begin
 result:=TPasJSONItemObject.Create;
 case fMode of
  TKraftSimpleVehicle.TEnvelope.TMode.Linear:begin
   TPasJSONItemObject(result).Add('mode',TPasJSONItemString.Create('linear'));
   TPasJSONItemObject(result).Add('timestart',TPasJSONItemNumber.Create(fPoints[0].fTime));
   TPasJSONItemObject(result).Add('valuestart',TPasJSONItemNumber.Create(fPoints[0].fValue));
   TPasJSONItemObject(result).Add('timeend',TPasJSONItemNumber.Create(fPoints[1].fTime));
   TPasJSONItemObject(result).Add('valueend',TPasJSONItemNumber.Create(fPoints[1].fValue));
  end;
  TKraftSimpleVehicle.TEnvelope.TMode.EaseInOut:begin
   TPasJSONItemObject(result).Add('mode',TPasJSONItemString.Create('easeinout'));
   TPasJSONItemObject(result).Add('timestart',TPasJSONItemNumber.Create(fPoints[0].fTime));
   TPasJSONItemObject(result).Add('valuestart',TPasJSONItemNumber.Create(fPoints[0].fValue));
   TPasJSONItemObject(result).Add('timeend',TPasJSONItemNumber.Create(fPoints[fCount-1].fTime));
   TPasJSONItemObject(result).Add('valueend',TPasJSONItemNumber.Create(fPoints[fCount-1].fValue));
   TPasJSONItemObject(result).Add('steps',TPasJSONItemNumber.Create(fCount));
  end;
  else {TKraftSimpleVehicle.TEnvelope.TMode.Custom:}begin
   TPasJSONItemObject(result).Add('mode',TPasJSONItemString.Create('custom'));
   JSONItemArray:=TPasJSONItemArray.Create;
   try
    for Index:=0 to length(fPoints)-1 do begin
     JSONItemObject:=TPasJSONItemObject.Create;
     try
      JSONItemObject.Add('time',TPasJSONItemNumber.Create(fPoints[Index].fTime));
      JSONItemObject.Add('value',TPasJSONItemNumber.Create(fPoints[Index].fValue));
     finally
      JSONItemArray.Add(JSONItemObject);
     end;
    end;
   finally
    TPasJSONItemObject(result).Add('points',JSONItemArray);
   end;
  end;
 end;
end;
{$endif}

function TKraftSimpleVehicle.TEnvelope.GetTimeAtIndex(const aIndex:TKraftInt32):TKraftScalar;
begin
 if (aIndex>=0) and (aIndex<fCount) then begin
  result:=fPoints[aIndex].fTime;
 end else begin
  result:=0.0;
 end;
end;

function TKraftSimpleVehicle.TEnvelope.GetValueAtIndex(const aIndex:TKraftInt32):TKraftScalar;
begin
 if (aIndex>=0) and (aIndex<fCount) then begin
  result:=fPoints[aIndex].fValue;
 end else begin
  result:=0.0;
 end;
end;

function TKraftSimpleVehicle.TEnvelope.GetIndexFromTime(const aTime:TKraftScalar):TKraftInt32;
var LowIndex,HighIndex,MidIndex:TKraftInt32;
    Point:PPoint;
begin
 if fCount>0 then begin
  if aTime<fPoints[0].fTime then begin
   LowIndex:=0;
  end else if fPoints[fCount-1].fTime<aTime then begin
   LowIndex:=fCount;
  end else begin
   LowIndex:=0;
   HighIndex:=fCount-1;
   while LowIndex<=HighIndex do begin
    MidIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
    Point:=@fPoints[MidIndex];
    case Sign(Point^.fTime-aTime) of
     -1:begin
      LowIndex:=MidIndex+1;
     end;
     1:begin
      HighIndex:=MidIndex-1;
     end;
     else begin
      result:=MidIndex;
      exit;
     end;
    end;
   end;
  end;
  HighIndex:=LowIndex;
  dec(LowIndex);
  if LowIndex<0 then begin
   LowIndex:=0;
  end else if LowIndex>=fCount then begin
   LowIndex:=fCount-1;
  end;
  if HighIndex<0 then begin
   HighIndex:=0;
  end else if HighIndex>=fCount then begin
   HighIndex:=fCount-1;
  end;
  if (LowIndex=HighIndex) or SameValue(fPoints[LowIndex].fTime,fPoints[HighIndex].fTime) then begin
   result:=HighIndex;
  end else begin
   result:=LowIndex;
  end;
 end else begin
  result:=-1;
 end;
end;

function TKraftSimpleVehicle.TEnvelope.GetValueAtTime(const aTime:TKraftScalar):TKraftScalar;
var LowIndex,HighIndex,MidIndex:TKraftInt32;
    Point:PPoint;
begin
 if fCount>0 then begin
  if aTime<fPoints[0].fTime then begin
   LowIndex:=0;
  end else if fPoints[fCount-1].fTime<aTime then begin
   LowIndex:=fCount;
  end else begin
   LowIndex:=0;
   HighIndex:=fCount-1;
   while LowIndex<=HighIndex do begin
    MidIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
    Point:=@fPoints[MidIndex];
    case Sign(Point^.fTime-aTime) of
     -1:begin
      LowIndex:=MidIndex+1;
     end;
     1:begin
      HighIndex:=MidIndex-1;
     end;
     else begin
      result:=Point^.fValue;
      exit;
     end;
    end;
   end;
  end;
  HighIndex:=LowIndex;
  dec(LowIndex);
  if LowIndex<0 then begin
   LowIndex:=0;
  end else if LowIndex>=fCount then begin
   LowIndex:=fCount-1;
  end;
  if HighIndex<0 then begin
   HighIndex:=0;
  end else if HighIndex>=fCount then begin
   HighIndex:=fCount-1;
  end;
  if (LowIndex=HighIndex) or SameValue(fPoints[LowIndex].fTime,fPoints[HighIndex].fTime) then begin
   result:=fPoints[HighIndex].fValue;
  end else begin
   result:=Clamp01((aTime-fPoints[LowIndex].fTime)/(fPoints[HighIndex].fTime-fPoints[LowIndex].fTime));
   result:=(fPoints[LowIndex].fValue*(1.0-result))+(fPoints[HighIndex].fValue*result);
  end;
 end else begin
  result:=0;
 end;
end;

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

{ TKraftSimpleVehicle.TWheel }

constructor TKraftSimpleVehicle.TWheel.Create(const aVehicle:TKraftSimpleVehicle;const aWheelID:TKraftSimpleVehicle.TWheelID);
begin
 inherited Create;
 fVehicle:=aVehicle;
 fWheelID:=aWheelID;
 fSpring.fCurrentLength:=0.0;
 fSpring.fCurrentVelocity:=0.0;
 fYawRad:=0.0;
 fRotationRad:=0.0;
 fWorldTransform:=Matrix4x4Identity;
 fLastWorldTransform:=Matrix4x4Identity;
 fVisualWorldTransform:=Matrix4x4Identity;
end;

destructor TKraftSimpleVehicle.TWheel.Destroy;
begin
 inherited Destroy;
end;

function TKraftSimpleVehicle.TWheel.GetSpringRelativePosition:TKraftVector3;
var BoxSize:TKraftVector3;
begin
 BoxSize:=Vector3(fVehicle.fSettings.fWidth,fVehicle.fSettings.fHeight,fVehicle.fSettings.fLength);
 case fWheelID of
  TWheelID.FrontLeft:begin
   result:=Vector3(BoxSize.x*(fVehicle.fSettings.fWheelsPaddingX-0.5),fVehicle.fSettings.fWheelsHeight,BoxSize.z*(0.5-fVehicle.fSettings.fWheelsPaddingZ));
  end;
  TWheelID.FrontRight:begin
   result:=Vector3(BoxSize.x*(0.5-fVehicle.fSettings.fWheelsPaddingX),fVehicle.fSettings.fWheelsHeight,BoxSize.z*(0.5-fVehicle.fSettings.fWheelsPaddingZ));
  end;
  TWheelID.BackLeft:begin
   result:=Vector3(BoxSize.x*(fVehicle.fSettings.fWheelsPaddingX-0.5),fVehicle.fSettings.fWheelsHeight,BoxSize.z*(fVehicle.fSettings.fWheelsPaddingZ-0.5));
  end;
  TWheelID.BackRight:begin
   result:=Vector3(BoxSize.x*(0.5-fVehicle.fSettings.fWheelsPaddingX),fVehicle.fSettings.fWheelsHeight,BoxSize.z*(fVehicle.fSettings.fWheelsPaddingZ-0.5));
  end;
  else begin
   result:=Vector3(0.0,0.0,0.0);
  end;
 end;
end;

function TKraftSimpleVehicle.TWheel.GetSpringPosition:TKraftVector3;
begin
 result:=Vector3TermMatrixMul(GetSpringRelativePosition,fVehicle.fWorldTransform);
end;

function TKraftSimpleVehicle.TWheel.GetSpringHitPosition:TKraftVector3;
begin
 result:=Vector3Add(GetSpringPosition,Vector3ScalarMul(fVehicle.fWorldDown,fSpring.fCurrentLength));
end;

function TKraftSimpleVehicle.TWheel.GetWheelRollDirection:TKraftVector3;
begin
 if fWheelID in [TWheelID.FrontLeft,TWheelID.FrontRight] then begin
  result:=Vector3TermQuaternionRotate(fVehicle.fWorldForward,QuaternionFromAxisAngle(Vector3(0.0,1.0,0.0),fYawRad));
 end else begin
  result:=fVehicle.fWorldForward;
 end;
end;

function TKraftSimpleVehicle.TWheel.GetWheelSlideDirection:TKraftVector3;
begin
 result:=Vector3Cross(fVehicle.fWorldUp,GetWheelRollDirection);
end;

function TKraftSimpleVehicle.TWheel.GetWheelTorqueRelativePosition:TKraftVector3;
var BoxSize:TKraftVector3;
begin
 BoxSize:=Vector3(fVehicle.fSettings.fWidth,fVehicle.fSettings.fHeight,fVehicle.fSettings.fLength);
 case fWheelID of
  TWheelID.FrontLeft:begin
   result:=Vector3(BoxSize.x*(fVehicle.fSettings.fWheelsPaddingX-0.5),0.0,BoxSize.z*(0.5-fVehicle.fSettings.fWheelsPaddingZ));
  end;
  TWheelID.FrontRight:begin
   result:=Vector3(BoxSize.x*(0.5-fVehicle.fSettings.fWheelsPaddingX),0.0,BoxSize.z*(0.5-fVehicle.fSettings.fWheelsPaddingZ));
  end;
  TWheelID.BackLeft:begin
   result:=Vector3(BoxSize.x*(fVehicle.fSettings.fWheelsPaddingX-0.5),0.0,BoxSize.z*(fVehicle.fSettings.fWheelsPaddingZ-0.5));
  end;
  TWheelID.BackRight:begin
   result:=Vector3(BoxSize.x*(0.5-fVehicle.fSettings.fWheelsPaddingX),0.0,BoxSize.z*(fVehicle.fSettings.fWheelsPaddingZ-0.5));
  end;
  else begin
   result:=Vector3(0.0,0.0,0.0);
  end;
 end;
end;

function TKraftSimpleVehicle.TWheel.GetWheelTorquePosition:TKraftVector3;
begin
 result:=Vector3TermMatrixMul(GetWheelTorqueRelativePosition,fVehicle.fWorldTransform);
end;

function TKraftSimpleVehicle.TWheel.GetWheelGripFactor:TKraftScalar;
begin
 if fWheelID in [TWheelID.FrontLeft,TWheelID.FrontRight] then begin
  result:=fVehicle.fSettings.fFrontWheelsGripFactor;
 end else begin
  result:=fVehicle.fSettings.fBackWheelsGripFactor;
 end;
end;

function TKraftSimpleVehicle.TWheel.GetWheelTransform:TKraftMatrix4x4;
var {LocalWheelPosition,}WorldWheelPosition:TKraftVector3;
    LocalWheelRotation,WorldWheelRotation:TKraftQuaternion;
begin
 LocalWheelRotation:=QuaternionFromAngles(fYawRad,0.0,fRotationRad);
 WorldWheelPosition:=Vector3Add(GetSpringPosition,Vector3ScalarMul(fVehicle.fWorldDown,fSpring.fCurrentLength-fVehicle.fSettings.fWheelsRadius));
 WorldWheelRotation:=QuaternionMul(fVehicle.fRigidBody.Sweep.q,LocalWheelRotation);
 result:=QuaternionToMatrix4x4(WorldWheelRotation);
 PKraftVector3(@result[3,0])^.xyz:=WorldWheelPosition.xyz;
end;

function TKraftSimpleVehicle.TWheel.IsGrounded:boolean;
begin
 result:=fSpring.fCurrentLength<fVehicle.fSettings.fSpringRestLength;
end;

procedure TKraftSimpleVehicle.TWheel.CastSpring;
var RayOrigin,RayDirection,HitPoint,HitNormal:TKraftVector3;
    RayLength,PreviousLength,CurrentLength,HitTime:TKraftScalar;
    HitShape:TKraftShape;
begin
 RayOrigin:=GetSpringPosition;
 PreviousLength:=fSpring.fCurrentLength;
 RayDirection:=fVehicle.fWorldDown;
 RayLength:=fVehicle.fSettings.fSpringRestLength;
 if fVehicle.fPhysics.RayCast(RayOrigin,RayDirection,RayLength,HitShape,HitTime,HitPoint,HitNormal,[0],nil) then begin
  CurrentLength:=HitTime;
 end else begin
  CurrentLength:=fVehicle.fSettings.fSpringRestLength;
 end;
 fSpring.fCurrentVelocity:=(CurrentLength-PreviousLength)*fVehicle.fInverseDeltaTime;
 fSpring.fCurrentLength:=CurrentLength;
 fSpring.fCompression:=1.0-Clamp01(CurrentLength/fVehicle.fSettings.fSpringRestLength);
end;

procedure TKraftSimpleVehicle.TWheel.UpdateSuspension;
var CurrentLength,CurrentVelocity,Force:TKraftScalar;
begin
 CurrentLength:=fSpring.fCurrentLength;
 CurrentVelocity:=fSpring.fCurrentVelocity;
 Force:=TKraftSimpleVehicle.TSpringMath.CalculateForceDamped(CurrentLength,
                                                             CurrentVelocity,
                                                             fVehicle.fSettings.fSpringRestLength,
                                                             fVehicle.fSettings.fSpringStrength,
                                                             fVehicle.fSettings.fSpringDamper);
 if abs(Force)>EPSILON then begin
  fVehicle.fRigidBody.AddForceAtPosition(Vector3ScalarMul(fVehicle.fWorldUp,Force),GetSpringPosition,kfmForce,true);
 end;
end;

procedure TKraftSimpleVehicle.TWheel.UpdateSteering;
var SpringPosition,SlideDirection,Force:TKraftVector3;
    SlideVelocity,DesiredVelocityChange,DesiredAcceleration:TKraftScalar;
begin
{$ifdef DebugDraw}
 fDebugSlideForce:=Vector3Origin;
{$endif}
 if IsGrounded then begin
  SpringPosition:=GetSpringPosition;
  SlideDirection:=GetWheelSlideDirection;
  SlideVelocity:=Vector3Dot(SlideDirection,fVehicle.fRigidBody.GetWorldLinearVelocityFromPoint(SpringPosition));
  DesiredVelocityChange:=-SlideVelocity*GetWheelGripFactor;
  DesiredAcceleration:=DesiredVelocityChange*fVehicle.fInverseDeltaTime;
  Force:=Vector3ScalarMul(SlideDirection,DesiredAcceleration*fVehicle.fSettings.fTireMass);
{$ifdef DebugDraw}
  Vector3DirectAdd(fDebugSlideForce,Force);
{$endif}
  if Vector3Length(Force)>EPSILON then begin
   fVehicle.fRigidBody.AddForceAtPosition(Force,GetWheelTorquePosition,kfmForce,true);
  end;
 end;
end;

procedure TKraftSimpleVehicle.TWheel.UpdateAcceleration;
var WheelForward,Force:TKraftVector3;
begin

{$ifdef DebugDraw}
 fDebugAccelerationForce:=Vector3Origin;
{$endif}

 if (fVehicle.fSettings.fUseAccelerationCurveEnvelopes and not IsZero(fVehicle.fAccelerationForceMagnitude)) or
    ((not fVehicle.fSettings.fUseAccelerationCurveEnvelopes) and not IsZero(fVehicle.fAccelerationInput)) then begin

  if IsGrounded and
     ((fVehicle.fMovingForward and (fVehicle.fAbsoluteSpeed<fVehicle.fSettings.fMaximumSpeed)) or
      ((not fVehicle.fMovingForward) and (fVehicle.fAbsoluteSpeed<fVehicle.fSettings.fMaximumReverseSpeed))) then begin

   WheelForward:=GetWheelRollDirection;

   if fVehicle.fSettings.fUseAccelerationCurveEnvelopes then begin
    Force:=Vector3ScalarMul(WheelForward,(fVehicle.fAccelerationForceMagnitude/TKraftSimpleVehicle.CountWheels)*fVehicle.fInverseDeltaTime);
   end else begin
    Force:=Vector3ScalarMul(WheelForward,fVehicle.fAccelerationInput*fVehicle.fSettings.fAccelerationForce);
   end;

{$ifdef DebugDraw}
   Vector3DirectAdd(fDebugAccelerationForce,Force);
{$endif}

   if Vector3Length(Force)>EPSILON then begin
    fVehicle.fRigidBody.AddForceAtPosition(Force,GetWheelTorquePosition,kfmForce,true);
   end;

  end;

 end;
end;

procedure TKraftSimpleVehicle.TWheel.UpdateBraking;
const AlmostStoppedSpeed=0.1;
var BrakeRatio,RollFrictionRatio,RollVelocity,DesiredVelocityChange,DesiredAcceleration:TKraftScalar;
    AlmostStopping,AccelerationContrary:boolean;
    SpringPosition,RollDirection,Force:TKraftVector3;
begin

{$ifdef DebugDraw}
 fDebugDeaccelerationForce:=Vector3Origin;
{$endif}

 if fVehicle.fSettings.fUseAccelerationCurveEnvelopes then begin

  if fVehicle.fIsBrake or fVehicle.fIsHandBrake then begin
   if fVehicle.fIsHandBrake and not fVehicle.fIsBrake then begin
    BrakeRatio:=0.8;
   end else begin
    BrakeRatio:=1.0;
   end;
   RollFrictionRatio:=0.0;
  end else if not (fVehicle.fIsAcceleration or fVehicle.fIsReverseAcceleration) then begin
   BrakeRatio:=0.0;
   RollFrictionRatio:=1.0;
  end else begin
   exit;
  end;

 end else begin

  AlmostStopping:=fVehicle.fAbsoluteSpeed<AlmostStoppedSpeed;
  if AlmostStopping or fVehicle.fIsBrake or fVehicle.fIsHandBrake then begin
   if fVehicle.fIsHandBrake and not fVehicle.fIsBrake then begin
    BrakeRatio:=0.8;
   end else begin
    BrakeRatio:=1.0;
   end;
   RollFrictionRatio:=0.0;
  end else begin
   AccelerationContrary:=(fVehicle.fIsAcceleration or fVehicle.fIsReverseAcceleration) and
                         (Vector3Dot(Vector3ScalarMul(fVehicle.fWorldForward,fVehicle.fAccelerationInput),fVehicle.fRigidBody.LinearVelocity)<0.0);
   if AccelerationContrary then begin
    BrakeRatio:=1.0;
    RollFrictionRatio:=0.0;
   end else if not (fVehicle.fIsAcceleration or fVehicle.fIsReverseAcceleration) then begin
    BrakeRatio:=0.0;
    RollFrictionRatio:=1.0;
   end else begin
    exit;
   end;
  end;

 end;

 if IsGrounded then begin
  SpringPosition:=GetSpringPosition;
  RollDirection:=GetWheelRollDirection;
  RollVelocity:=Vector3Dot(RollDirection,fVehicle.fRigidBody.GetWorldLinearVelocityFromPoint(SpringPosition));
  DesiredVelocityChange:=-RollVelocity*((BrakeRatio*fVehicle.fSettings.fBrakeForce)+
                                        (RollFrictionRatio*fVehicle.fSettings.fRollFriction));
  DesiredAcceleration:=DesiredVelocityChange*fVehicle.fInverseDeltaTime;
  Force:=Vector3ScalarMul(RollDirection,DesiredAcceleration*fVehicle.fSettings.fTireMass);
{$ifdef DebugDraw}
  Vector3DirectAdd(fDebugDeaccelerationForce,Force);
{$endif}
  if Vector3Length(Force)>EPSILON then begin
   fVehicle.fRigidBody.AddForceAtPosition(Force,GetWheelTorquePosition,kfmForce,true);
  end;
 end;

end;

procedure TKraftSimpleVehicle.TWheel.UpdateWheelRotation;
const TwoPI=2.0*PI;
var WheelID:TWheelID;
    WorldWheelPosition,WorldWheelForward,VelocityQueryPos,WheelVelocity:TKraftVector3;
    LocalWheelRotation,WorldWheelRotation:TKraftQuaternion;
    TireLongSpeed,WheelLengthMeters,RevolutionsPerSecond,DeltaRotation:TKraftScalar;
begin

 LocalWheelRotation:=QuaternionFromAngles(fYawRad,0.0,0);

 WorldWheelPosition:=Vector3Add(GetSpringPosition,Vector3ScalarMul(fVehicle.WorldDown,fSpring.fCurrentLength));
 WorldWheelRotation:=QuaternionMul(fVehicle.fRigidBody.Sweep.q,LocalWheelRotation);

 WorldWheelForward:=Vector3TermQuaternionRotate(Vector3(0.0,0.0,1.0),WorldWheelRotation);

 VelocityQueryPos:=WorldWheelPosition;
 WheelVelocity:=fVehicle.fRigidBody.GetWorldLinearVelocityFromPoint(VelocityQueryPos);

 // Longitudinal speed (meters/sec)
 TireLongSpeed:=Vector3Dot(WheelVelocity,WorldWheelForward);

 // Circle length = 2 * PI * R
 WheelLengthMeters:=TwoPI*fVehicle.fSettings.fWheelsRadius;

 // Wheel "Revolutions per second";
 RevolutionsPerSecond:=TireLongSpeed/WheelLengthMeters;

 DeltaRotation:=TwoPI*RevolutionsPerSecond*fVehicle.fDeltaTime;

 fRotationRad:=fRotationRad+DeltaRotation;

end;

procedure TKraftSimpleVehicle.TWheel.UpdateVisuals;
begin
 fWorldTransform:=GetWheelTransform;
end;

procedure TKraftSimpleVehicle.TWheel.StoreWorldTransforms;
begin
 fLastWorldTransform:=fWorldTransform;
{$ifdef DebugDraw}
 fLastDebugAntiRollForce:=fDebugAntiRollForce;
 fLastDebugAccelerationForce:=fDebugAccelerationForce;
 fLastDebugDeaccelerationForce:=fDebugDeaccelerationForce;
 fLastDebugSlideForce:=fDebugSlideForce;
{$endif}
end;

procedure TKraftSimpleVehicle.TWheel.InterpolateWorldTransforms(const aAlpha:TKraftScalar);
begin
 fVisualWorldTransform:=Matrix4x4Slerp(fLastWorldTransform,fWorldTransform,aAlpha);
{$ifdef DebugDraw}
 fVisualDebugAntiRollForce:=Vector3Lerp(fLastDebugAntiRollForce,fDebugAntiRollForce,aAlpha);
 fVisualDebugAccelerationForce:=Vector3Lerp(fLastDebugAccelerationForce,fDebugAccelerationForce,aAlpha);
 fVisualDebugDeaccelerationForce:=Vector3Lerp(fLastDebugDeaccelerationForce,fDebugDeaccelerationForce,aAlpha);
 fVisualDebugSlideForce:=Vector3Lerp(fLastDebugSlideForce,fDebugSlideForce,aAlpha);
{$endif}
end;

{ TKraftSimpleVehicle.TVehicleSettings }

constructor TKraftSimpleVehicle.TVehicleSettings.Create;
begin
 inherited Create;
 fWidth:=1.9;
 fHeight:=0.75;
 fLength:=3.4;
 fWheelsRadius:=0.5;
 fWheelsHeight:=-0.25;
 fWheelsPaddingX:=0.06;
 fWheelsPaddingZ:=0.12;
 fChassisMass:=60;
 fTireMass:=1;
 fSpringRestLength:=0.8;
 fSpringStrength:=1200;
 fSpringDamper:=75.0;
 fStabilizerBarAntiRollForce:=100.0;
 fAccelerationForce:=300.0;
 fBrakeForce:=1.5;
 fRollFriction:=0.15;
 fMaximumSpeed:=10;
 fMaximumReverseSpeed:=2.5;
 fFrontWheelsGripFactor:=0.8;
 fBackWheelsGripFactor:=0.9;
 fAirResistance:=5.0;
 fHandBrakeSlipperyTime:=2.2;
 fUseAccelerationCurveEnvelopes:=true;
 fAccelerationCurveEnvelope:=TEnvelope.CreateLinear(0.0,0.0,5.0,300.0);
 fReverseAccelerationCurveEnvelope:=TEnvelope.CreateLinear(0.0,0.0,5.0,20.0);
 fReverseEvaluationAccuracy:=25;
 fSteerAngleLimitEnvelope:=TEnvelope.CreateLinear(0.0,35.0,100.0,5.0);
 fSteeringResetSpeedEnvelope:=TEnvelope.CreateEaseInOut(0.0,30.0,100.0,10.0,64);
 fSteeringSpeedEnvelope:=TEnvelope.CreateLinear(0.0,2.0,100.0,0.5);
 fDownForceCurveEnvelope:=TEnvelope.CreateLinear(0.0,0.0,200.0,100.0);
 fDownForce:=1.0;
 fFlightStabilizationForce:=1.0;
 fFlightStabilizationDamping:=0.1;
end;

destructor TKraftSimpleVehicle.TVehicleSettings.Destroy;
begin
 FreeAndNil(fAccelerationCurveEnvelope);
 FreeAndNil(fReverseAccelerationCurveEnvelope);
 FreeAndNil(fSteerAngleLimitEnvelope);
 FreeAndNil(fSteeringResetSpeedEnvelope);
 FreeAndNil(fSteeringSpeedEnvelope);
 FreeAndNil(fDownForceCurveEnvelope);
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
  fStabilizerBarAntiRollForce:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['stabilizerbarantirollforce'],fStabilizerBarAntiRollForce);
  fAccelerationForce:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['accelerationforce'],fAccelerationForce);
  fBrakeForce:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['brakeforce'],fBrakeForce);
  fRollFriction:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['rollfriction'],fRollFriction);
  fMaximumSpeed:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['maximumspeed'],fMaximumSpeed);
  fMaximumReverseSpeed:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['maximumreversespeed'],fMaximumReverseSpeed);
  fFrontWheelsGripFactor:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['frontwheelsgripfactor'],fFrontWheelsGripFactor);
  fBackWheelsGripFactor:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['backwheelsgripfactor'],fBackWheelsGripFactor);
  fAirResistance:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['airresistance'],fAirResistance);
  fHandBrakeSlipperyTime:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['handbrakeslipperytime'],fHandBrakeSlipperyTime);
  fUseAccelerationCurveEnvelopes:=TPasJSON.GetBoolean(TPasJSONItemObject(aJSONItem).Properties['useaccelerationcurveenvelopes'],fUseAccelerationCurveEnvelopes);
  fAccelerationCurveEnvelope.LoadFromJSON(TPasJSONItemObject(aJSONItem).Properties['accelerationcurveenvelope']);
  fReverseAccelerationCurveEnvelope.LoadFromJSON(TPasJSONItemObject(aJSONItem).Properties['reverseaccelerationcurveenvelope']);
  fReverseEvaluationAccuracy:=TPasJSON.GetInt64(TPasJSONItemObject(aJSONItem).Properties['reverseevaluationaccuracy'],fReverseEvaluationAccuracy);
  fSteerAngleLimitEnvelope.LoadFromJSON(TPasJSONItemObject(aJSONItem).Properties['steeranglelimitenvelope']);
  fSteeringResetSpeedEnvelope.LoadFromJSON(TPasJSONItemObject(aJSONItem).Properties['steeringresetspeedenvelope']);
  fSteeringSpeedEnvelope.LoadFromJSON(TPasJSONItemObject(aJSONItem).Properties['steeringspeedenvelope']);
  fDownForceCurveEnvelope.LoadFromJSON(TPasJSONItemObject(aJSONItem).Properties['downforcecurveenvelope']);
  fDownForce:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['downforce'],fDownForce);
  fFlightStabilizationDamping:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['flightstabilizationdamping'],fFlightStabilizationDamping);
  fFlightStabilizationForce:=TPasJSON.GetNumber(TPasJSONItemObject(aJSONItem).Properties['flightstabilizationforce'],fFlightStabilizationForce);
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
 TPasJSONItemObject(result).Add('stabilizerbarantirollforce',TPasJSONItemNumber.Create(fStabilizerBarAntiRollForce));
 TPasJSONItemObject(result).Add('accelerationforce',TPasJSONItemNumber.Create(fAccelerationForce));
 TPasJSONItemObject(result).Add('brakeforce',TPasJSONItemNumber.Create(fBrakeForce));
 TPasJSONItemObject(result).Add('rollfriction',TPasJSONItemNumber.Create(fRollFriction));
 TPasJSONItemObject(result).Add('maximumspeed',TPasJSONItemNumber.Create(fMaximumSpeed));
 TPasJSONItemObject(result).Add('maximumreversespeed',TPasJSONItemNumber.Create(fMaximumReverseSpeed));
 TPasJSONItemObject(result).Add('frontwheelsgripfactor',TPasJSONItemNumber.Create(fFrontWheelsGripFactor));
 TPasJSONItemObject(result).Add('backwheelsgripfactor',TPasJSONItemNumber.Create(fBackWheelsGripFactor));
 TPasJSONItemObject(result).Add('airresistance',TPasJSONItemNumber.Create(fAirResistance));
 TPasJSONItemObject(result).Add('handbrakeslipperytime',TPasJSONItemNumber.Create(fHandBrakeSlipperyTime));
 TPasJSONItemObject(result).Add('useaccelerationcurveenvelopes',TPasJSONItemBoolean.Create(fUseAccelerationCurveEnvelopes));
 TPasJSONItemObject(result).Add('accelerationcurveenvelope',fAccelerationCurveEnvelope.SaveToJSON);
 TPasJSONItemObject(result).Add('reverseaccelerationcurveenvelope',fReverseAccelerationCurveEnvelope.SaveToJSON);
 TPasJSONItemObject(result).Add('reverseevaluationaccuracy',TPasJSONItemNumber.Create(fReverseEvaluationAccuracy));
 TPasJSONItemObject(result).Add('steeranglelimitenvelope',fSteerAngleLimitEnvelope.SaveToJSON);
 TPasJSONItemObject(result).Add('steeringresetspeedenvelope',fSteeringResetSpeedEnvelope.SaveToJSON);
 TPasJSONItemObject(result).Add('steeringspeedenvelope',fSteeringSpeedEnvelope.SaveToJSON);
 TPasJSONItemObject(result).Add('downforcecurveenvelope',fDownForceCurveEnvelope.SaveToJSON);
 TPasJSONItemObject(result).Add('downforce',TPasJSONItemNumber.Create(fDownForce));
 TPasJSONItemObject(result).Add('flightstabilizationdamping',TPasJSONItemNumber.Create(fFlightStabilizationDamping));
 TPasJSONItemObject(result).Add('flightstabilizationforce',TPasJSONItemNumber.Create(fFlightStabilizationForce));
end;
{$endif}

{ TKraftSimpleVehicle }

constructor TKraftSimpleVehicle.Create(const aPhysics:TKraft);
var WheelID:TWheelID;
begin
 inherited Create;
 fPhysics:=aPhysics;
 fRigidBody:=nil;
 fShape:=nil;
 fAccelerationInput:=0.0;
 fControllable:=true;
 fForward:=Vector3(0.0,0.0,-1.0);
 fVelocity:=Vector3(0.0,0.0,0.0);
 fSettings:=TKraftSimpleVehicle.TVehicleSettings.Create;
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  fWheels[WheelID]:=TKraftSimpleVehicle.TWheel.Create(self,WheelID);
 end;
 Reset;
end;

destructor TKraftSimpleVehicle.Destroy;
var WheelID:TWheelID;
begin
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  FreeAndNil(fWheels[WheelID]);
 end;
 FreeAndNil(fSettings);
 inherited Destroy;
end;

procedure TKraftSimpleVehicle.Reset;
begin
 fAfterFlightSlipperyTiresTime:=0.0;
 fBrakeSlipperyTiresTime:=0.0;
 fHandBrakeSlipperyTiresTime:=0.0;
 fSteeringAngle:=0.0;
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

function TKraftSimpleVehicle.GetHandBrakeK:TKraftScalar;
begin
 result:=fHandBrakeSlipperyTiresTime/Max(0.1,fSettings.fHandBrakeSlipperyTime);
 result:=result*result*result*(result*((result*6.0)-15.0)+10.0);
end;

function TKraftSimpleVehicle.GetSteeringHandBrakeK:TKraftScalar;
begin
 result:=0.4+(1.0-GetHandBrakeK)*0.6;
end;

function TKraftSimpleVehicle.GetSteerAngleLimitInDeg(const aSpeedMetersPerSec:TKraftScalar):TKraftScalar;
begin
 result:=fSettings.fSteerAngleLimitEnvelope.GetValueAtTime(aSpeedMetersPerSec*3.6*GetSteeringHandBrakeK);
end;

function TKraftSimpleVehicle.GetSpeed:TKraftScalar;
var LinearVelocity,WorldSpaceForward,ProjectedVector:TKraftVector3;
    Factor:TKraftScalar;
begin
 LinearVelocity:=fRigidBody.LinearVelocity;
 WorldSpaceForward:=Vector3(PKraftRawVector3(@fRigidBody.WorldTransform[2,0])^);
 Factor:=Vector3Dot(WorldSpaceForward,LinearVelocity);
 ProjectedVector:=Vector3ScalarMul(WorldSpaceForward,Factor);
 result:=Vector3Length(ProjectedVector)*Sign(Factor);
end;

function TKraftSimpleVehicle.GetAccelerationForceMagnitude(const aEnvelope:TEnvelope;const aSpeedMetersPerSec,aDeltaTime:TKraftScalar):TKraftScalar;
const Inv3d6=1.0/3.6;
var Index,Count:TKraftInt32;
    SpeedKMH,Mass,MinTime,MaxTime,TimeNow,CurrentSpeed,CurrentSpeedDifference,
    Step,StepTime,StepSpeed,StepSpeedDifference:TKraftScalar;
begin

 SpeedKMH:=aSpeedMetersPerSec*3.6;

 Mass:=fRigidBody.Mass;

 Count:=aEnvelope.Count;

 case Count of
  0:begin
   result:=0.0;
  end;
  1:begin
   result:=Max(0.0,((aEnvelope.fPoints[0].fValue-SpeedKMH)*Inv3d6)*Mass);
  end;
  else begin

   MinTime:=aEnvelope.fPoints[0].fTime;
   MaxTime:=aEnvelope.fPoints[Count-1].fTime;

   Step:=MaxTime-MinTime;

   TimeNow:=MinTime;

   if SpeedKMH<aEnvelope.fPoints[Count-1].fValue then begin

    for Index:=0 to fSettings.fReverseEvaluationAccuracy-1 do begin

     CurrentSpeed:=aEnvelope.GetValueAtTime(TimeNow);
     CurrentSpeedDifference:=abs(SpeedKMH-CurrentSpeed);

     StepTime:=TimeNow+Step;
     StepSpeed:=aEnvelope.GetValueAtTime(StepTime);
     StepSpeedDifference:=abs(SpeedKMH-StepSpeed);

     if StepSpeedDifference<CurrentSpeedDifference then begin
      TimeNow:=StepTime;
      CurrentSpeed:=StepSpeed;
     end;

     Step:=abs(Step*0.5)*Sign(SpeedKMH-CurrentSpeed);

    end;

    result:=aEnvelope.GetValueAtTime(TimeNow+aDeltaTime);

   end else begin

    result:=aEnvelope.fPoints[Count-1].fValue;

   end;

   result:=Max(0.0,(result-SpeedKMH)*Inv3d6*Mass);

  end;

 end;

end;

function TKraftSimpleVehicle.CalcAccelerationForceMagnitude:TKraftScalar;
begin
 if fIsAcceleration or fIsReverseAcceleration then begin
  if fIsAcceleration then begin
   result:=GetAccelerationForceMagnitude(fSettings.fAccelerationCurveEnvelope,fSpeed,fDeltaTime);
  end else begin
   result:=-GetAccelerationForceMagnitude(fSettings.fReverseAccelerationCurveEnvelope,-fSpeed,fDeltaTime);
  end;
 end else begin
  result:=0.0;
 end;
end;

procedure TKraftSimpleVehicle.UpdateGlobals;
begin

 fSpeed:=GetSpeed;
 fSpeedKMH:=abs(fSpeed)*3.6;

 fRelativeSpeed:=Vector3Dot(fWorldForward,fRigidBody.LinearVelocity);
 fMovingForward:=fRelativeSpeed>0.0;
 fAbsoluteSpeed:=abs(fRelativeSpeed);

end;

procedure TKraftSimpleVehicle.UpdateInput;
var Vertical,Horizontal,NewSteerAngle,AngleReturnSpeedDegressPerSecond:TKraftScalar;
    IsBrakeNow,IsHandBrakeNow:boolean;
begin

 if fControllable then begin
  Vertical:=fInputVertical;
  Horizontal:=fInputHorizontal;
  if fInputReset then begin
//  Reset;
  end;
 end else begin
  Vertical:=0.0;
  Horizontal:=0.0;
 end;

 fAccelerationInput:=Min(Max(Vertical,-1.0),1.0);

 IsBrakeNow:=false;
 IsHandBrakeNow:=fControllable and fInputHandBrake;

 fIsAcceleration:=false;
 fIsReverseAcceleration:=false;

 if fInputBrake and fControllable then begin
  IsBrakeNow:=true;
 end else if Vertical>0.4 then begin
  if fSpeed<-0.5 then begin
   IsBrakeNow:=true;
  end else begin
   fIsAcceleration:=true;
  end;
 end else if Vertical<-0.4 then begin
  if fSpeed>0.5 then begin
   IsBrakeNow:=true;
  end else begin
   fIsReverseAcceleration:=true;
  end;
 end;

 if IsBrakeNow and not fIsBrake then begin
  fBrakeSlipperyTiresTime:=1.0;
 end;

 if IsHandBrakeNow then begin
  fHandBrakeSlipperyTiresTime:=Max(0.1,fSettings.fHandBrakeSlipperyTime);
 end;

 fIsBrake:=IsBrakeNow;

 fIsHandBrake:=IsHandBrakeNow and not (fIsAcceleration or fIsReverseAcceleration);

 if abs(Horizontal)>0.001 then begin
  NewSteerAngle:=fSteeringAngle+(Horizontal*fSettings.fSteeringSpeedEnvelope.GetValueAtTime(fSpeedKMH*GetSteeringHandBrakeK));
  fSteeringAngle:=Min(abs(NewSteerAngle),GetSteerAngleLimitInDeg(Speed))*Sign(NewSteerAngle);
 end else begin
  AngleReturnSpeedDegressPerSecond:=fSettings.fSteeringResetSpeedEnvelope.GetValueAtTime(fSpeedKMH)*Clamp01(fSpeedKMH*0.5);
  fSteeringAngle:=Max(abs(fSteeringAngle)-(AngleReturnSpeedDegressPerSecond*fDeltaTime),0.0)*Sign(fSteeringAngle);
 end;

 fAccelerationForceMagnitude:=CalcAccelerationForceMagnitude*Clamp01(0.8+((1.0-GetHandBrakeK)*0.2));

end;

procedure TKraftSimpleVehicle.UpdateSuspension;
var WheelID:TWheelID;
    Wheel:TWheel;
begin
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  Wheel:=fWheels[WheelID];
  Wheel.CastSpring;
  Wheel.UpdateSuspension;
 end;
end;

procedure TKraftSimpleVehicle.CalculateAckermannSteering;
var SteerAngleRad,AxleSeparation,WheelSeparation,TurningCircleRadius:TKraftScalar;
    AxleDiff,WheelDiff:TKraftVector3;
begin

 SteerAngleRad:=fSteeringAngle*DEG2RAD;

 AxleDiff:=Vector3Sub(Vector3Avg(fWheels[TWheelID.FrontLeft].GetSpringPosition,fWheels[TWheelID.FrontRight].GetSpringPosition),
                      Vector3Avg(fWheels[TWheelID.BackLeft].GetSpringPosition,fWheels[TWheelID.BackRight].GetSpringPosition));
 AxleSeparation:=Vector3Length(AxleDiff);

 WheelDiff:=Vector3Sub(fWheels[TWheelID.FrontLeft].GetSpringPosition,fWheels[TWheelID.FrontRight].GetSpringPosition);
 WheelSeparation:=Vector3Length(WheelDiff);

 TurningCircleRadius:=AxleSeparation/Tan(SteerAngleRad);
 if IsNaN(TurningCircleRadius) then begin
  TurningCircleRadius:=0.0;
 end;

 fWheels[TKraftSimpleVehicle.TWheelID.FrontLeft].fYawRad:=ArcTan(AxleSeparation/(TurningCircleRadius+(WheelSeparation*0.5)));
 fWheels[TKraftSimpleVehicle.TWheelID.FrontRight].fYawRad:=ArcTan(AxleSeparation/(TurningCircleRadius-(WheelSeparation*0.5)));

 fWheels[TKraftSimpleVehicle.TWheelID.BackLeft].fYawRad:=0.0;
 fWheels[TKraftSimpleVehicle.TWheelID.BackRight].fYawRad:=0.0;

end;

procedure TKraftSimpleVehicle.UpdateSteering;
var WheelID:TWheelID;
begin
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  fWheels[WheelID].UpdateSteering;
 end;
end;

procedure TKraftSimpleVehicle.UpdateAcceleration;
var WheelID:TWheelID;
begin
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  fWheels[WheelID].UpdateAcceleration;
 end;
end; 

procedure TKraftSimpleVehicle.UpdateBraking;
var WheelID:TWheelID;
begin
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  fWheels[WheelID].UpdateBraking;
 end;
end;

procedure TKraftSimpleVehicle.UpdateAntiRollBar;
 procedure ProcessAxle(const aWheelLeft,aWheelRight:TKraftSimpleVehicle.TWheel);
 var TravelL,TravelR,AntiRollForce:TKraftScalar;
 begin
  TravelL:=1.0-Clamp01(aWheelLeft.fSpring.fCompression);
  TravelR:=1.0-Clamp01(aWheelRight.fSpring.fCompression);
  AntiRollForce:=(TravelL-TravelR)*fSettings.fStabilizerBarAntiRollForce;
  if aWheelLeft.IsGrounded then begin
   fRigidBody.AddForceAtPosition(Vector3ScalarMul(fWorldDown,AntiRollForce),aWheelLeft.GetSpringHitPosition,kfmForce,false);
{$ifdef DebugDraw}
   aWheelLeft.fDebugAntiRollForce:=Vector3ScalarMul(fWorldDown,AntiRollForce);
{$endif}
  end else begin
{$ifdef DebugDraw}
   aWheelLeft.fDebugAntiRollForce:=Vector3Origin;
{$endif}
  end;
  if aWheelRight.IsGrounded then begin
   fRigidBody.AddForceAtPosition(Vector3ScalarMul(fWorldDown,-AntiRollForce),aWheelRight.GetSpringHitPosition,kfmForce,false);
{$ifdef DebugDraw}
   aWheelRight.fDebugAntiRollForce:=Vector3ScalarMul(fWorldDown,AntiRollForce);
{$endif}
  end else begin
{$ifdef DebugDraw}
   aWheelRight.fDebugAntiRollForce:=Vector3Origin;
{$endif}
  end;
 end;
begin
 if not IsZero(fSettings.fStabilizerBarAntiRollForce) then begin
  ProcessAxle(fWheels[TWheelID.FrontLeft],fWheels[TWheelID.FrontRight]);
  ProcessAxle(fWheels[TWheelID.BackLeft],fWheels[TWheelID.BackRight]);
 end; 
end;

procedure TKraftSimpleVehicle.UpdateAirResistance;
var Force:TKraftVector3;
begin
 Force:=Vector3ScalarMul(fRigidBody.LinearVelocity,-fSettings.fAirResistance*Vector3Length(Vector3(fSettings.fWidth,fSettings.fHeight,fSettings.fLength)));
{$ifdef DebugDraw}
 fDebugAirResistanceForce:=Force;
{$endif}
 if Vector3Length(Force)>EPSILON then begin
  fRigidBody.AddWorldForce(Force,kfmForce,true);
 end;
end;

procedure TKraftSimpleVehicle.UpdateDownForce;
var WheelID:TWheelID;
    DownForceAmount:TKraftScalar;
    Force:TKraftVector3;
    AllWheelsGrounded:boolean;
begin
 AllWheelsGrounded:=true;
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  if not fWheels[WheelID].IsGrounded then begin
   AllWheelsGrounded:=false;
   break;
  end;
 end;
 if AllWheelsGrounded and not IsZero(fSettings.fDownForce) then begin
  DownForceAmount:=fSettings.fDownForceCurveEnvelope.GetValueAtTime(fSpeedKMH)*0.01;
  Force:=Vector3ScalarMul(fWorldDown,fRigidBody.Mass*DownForceAmount*fSettings.fDownForce);
{$ifdef DebugDraw}
  fDebugDownForce:=Force;
{$endif}
  if Vector3Length(Force)>EPSILON then begin
   fRigidBody.AddWorldForce(Force,kfmForce,true);
  end;
{$ifdef DebugDraw}
 end else begin
  fDebugDownForce:=Vector3Origin;
{$endif}
 end;
end;

procedure TKraftSimpleVehicle.UpdateFlightStabilization;
var WheelID:TWheelID;
    VehicleUp,AntiGravityUp,Axis,Force:TKraftVector3;
    AllWheelsGrounded:boolean;
begin
 AllWheelsGrounded:=true;
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  if not fWheels[WheelID].IsGrounded then begin
   AllWheelsGrounded:=false;
   break;
  end;
 end;
 if not AllWheelsGrounded then begin

  fAfterFlightSlipperyTiresTime:=1.0;

  // Length of axis depends on the angle - i.e. the further awat
  // the vehicle is from being upright, the larger the applied impulse
  // will be, resulting in fast changes when the vehicle is on its
  // side, but not overcompensating (and therefore shaking) when
  // the vehicle is not much away from being upright.
  VehicleUp:=fWorldUp;
  AntiGravityUp:=Vector3Neg(fPhysics.Gravity.Vector);
  Axis:=Vector3Norm(Vector3Cross(VehicleUp,AntiGravityUp));

  // To avoid the vehicle going backwards/forwards (or rolling sideways),
  // set the pitch/roll to 0 before applying the 'straightening' impulse.
  if fSettings.fFlightStabilizationDamping>0.0 then begin
   fRigidBody.AngularVelocity:=Vector3Lerp(fRigidBody.AngularVelocity,
                                           Vector3(0.0,fRigidBody.AngularVelocity.y,0.0),
                                           Clamp01(fSettings.fFlightStabilizationDamping*fDeltaTime));
  end;

  // Give a nicely balanced feeling for rebalancing the vehicle
  if fSettings.fFlightStabilizationForce>0.0 then begin
   fRigidBody.AddWorldTorque(Vector3ScalarMul(Axis,fSettings.fFlightStabilizationForce*fRigidBody.Mass));
  end;

 end;
end;

procedure TKraftSimpleVehicle.UpdateWheelRotations;
var WheelID:TWheelID;
begin
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  fWheels[WheelID].UpdateWheelRotation;
 end;
end;

procedure TKraftSimpleVehicle.UpdateVisuals;
var WheelID:TWheelID;
begin
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  fWheels[WheelID].UpdateVisuals;
 end;
end;

procedure TKraftSimpleVehicle.Update(const aDeltaTime:TKraftScalar);
begin
 fDeltaTime:=aDeltaTime;
 fInverseDeltaTime:=1.0/fDeltaTime;
 UpdateWorldTransformVectors;
 UpdateGlobals;
 UpdateInput;
 UpdateSuspension;
 CalculateAckermannSteering;
 UpdateSteering;
 UpdateAcceleration;
 UpdateBraking;
 UpdateAntiRollBar;
 UpdateAirResistance;
 UpdateDownForce;
 UpdateFlightStabilization;
 UpdateWheelRotations;
 UpdateVisuals;

 fAfterFlightSlipperyTiresTime:=Max(0.0,fAfterFlightSlipperyTiresTime-fDeltaTime);

 fBrakeSlipperyTiresTime:=Max(0.0,fBrakeSlipperyTiresTime-fDeltaTime);

 fHandBrakeSlipperyTiresTime:=Max(0.0,fHandBrakeSlipperyTiresTime-fDeltaTime);

end;

procedure TKraftSimpleVehicle.StoreWorldTransforms;
var WheelID:TWheelID;
begin
 UpdateWorldTransformVectors;
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  fWheels[WheelID].StoreWorldTransforms;
 end;
 fLastWorldTransform:=fWorldTransform;
 fLastWorldRight:=Vector3(PKraftRawVector3(pointer(@fLastWorldTransform[0,0]))^);
 fLastWorldLeft:=Vector3Neg(fLastWorldRight);
 fLastWorldUp:=Vector3(PKraftRawVector3(pointer(@fLastWorldTransform[1,0]))^);
 fLastWorldDown:=Vector3Neg(fLastWorldUp);
 fLastWorldForward:=Vector3(PKraftRawVector3(pointer(@fLastWorldTransform[2,0]))^);
 fLastWorldBackward:=Vector3Neg(fLastWorldForward);
 fLastWorldPosition:=Vector3(PKraftRawVector3(pointer(@fLastWorldTransform[3,0]))^);
{$ifdef DebugDraw}
 fLastDebugAirResistanceForce:=fDebugAirResistanceForce;
 fLastDebugDownForce:=fDebugDownForce;
{$endif}
end;

procedure TKraftSimpleVehicle.InterpolateWorldTransforms(const aAlpha:TKraftScalar);
var WheelID:TWheelID;
begin
 UpdateWorldTransformVectors;
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin
  fWheels[WheelID].InterpolateWorldTransforms(aAlpha);
 end;
 fVisualWorldTransform:=Matrix4x4Slerp(fLastWorldTransform,fWorldTransform,aAlpha);
 fVisualWorldRight:=Vector3(PKraftRawVector3(pointer(@fVisualWorldTransform[0,0]))^);
 fVisualWorldLeft:=Vector3Neg(fVisualWorldRight);
 fVisualWorldUp:=Vector3(PKraftRawVector3(pointer(@fVisualWorldTransform[1,0]))^);
 fVisualWorldDown:=Vector3Neg(fVisualWorldUp);
 fVisualWorldForward:=Vector3(PKraftRawVector3(pointer(@fVisualWorldTransform[2,0]))^);
 fVisualWorldBackward:=Vector3Neg(fVisualWorldForward);
 fVisualWorldPosition:=Vector3(PKraftRawVector3(pointer(@fVisualWorldTransform[3,0]))^);
{$ifdef DebugDraw}
 fVisualDebugAirResistanceForce:=Vector3Lerp(fLastDebugAirResistanceForce,fDebugAirResistanceForce,aAlpha);
 fVisualDebugDownForce:=Vector3Lerp(fLastDebugDownForce,fDebugDownForce,aAlpha);
{$endif}
end;

{$ifdef DebugDraw}
procedure TKraftSimpleVehicle.DebugDraw;
var WheelID:TWheelID;
    Wheel:TWheel;
    Index:TKraftInt32;
    v0,v1,v2,v3,v:TKraftVector3;
    Color:TKraftVector4;
begin
{$ifndef NoOpenGL}
 glDisable(GL_DEPTH_TEST);
{$endif}
 begin
  Color:=Vector4(0.0,0.0,1.0,1.0);
{ v0:=Vector3TermMatrixMul(Vector3Origin,fWheels[TWheelID.FrontLeft].fVisualWorldTransform);
  v1:=Vector3TermMatrixMul(Vector3Origin,fWheels[TWheelID.FrontRight].fVisualWorldTransform);
  v2:=Vector3TermMatrixMul(Vector3Origin,fWheels[TWheelID.BackLeft].fVisualWorldTransform);
  v3:=Vector3TermMatrixMul(Vector3Origin,fWheels[TWheelID.BackRight].fVisualWorldTransform);}
  v0:=Vector3TermMatrixMul(fWheels[TWheelID.FrontLeft].GetSpringRelativePosition,fVisualWorldTransform);
  v1:=Vector3TermMatrixMul(fWheels[TWheelID.FrontRight].GetSpringRelativePosition,fVisualWorldTransform);
  v2:=Vector3TermMatrixMul(fWheels[TWheelID.BackLeft].GetSpringRelativePosition,fVisualWorldTransform);
  v3:=Vector3TermMatrixMul(fWheels[TWheelID.BackRight].GetSpringRelativePosition,fVisualWorldTransform);
{$ifdef NoOpenGL}
  if assigned(fDebugDrawLine) then begin
   fDebugDrawLine(v0,v1,Color);
   fDebugDrawLine(v2,v3,Color);
  end;
{$else}
  glColor4fv(@Color);
  glBegin(GL_LINE_STRIP);
  glVertex3fv(@v0);
  glVertex3fv(@v1);
  glEnd;
  glBegin(GL_LINE_STRIP);
  glVertex3fv(@v2);
  glVertex3fv(@v3);
  glEnd;
{$endif}
  v0:=Vector3Avg(v0,v1);
  v3:=Vector3Avg(v2,v3);
  v:=Vector3TermMatrixMul(Vector3Origin,fVisualWorldTransform);
  v1:=Vector3Lerp(v0,v,0.9);
  v2:=Vector3Lerp(v3,v,0.9);
{$ifdef NoOpenGL}
  if assigned(fDebugDrawLine) then begin
   fDebugDrawLine(v0,v1,Color);
   fDebugDrawLine(v2,v3,Color);
  end;
{$else}
  glColor4fv(@Color);
  glBegin(GL_LINE_STRIP);
  glVertex3fv(@v0);
  glVertex3fv(@v1);
  glEnd;
  glBegin(GL_LINE_STRIP);
  glVertex3fv(@v2);
  glVertex3fv(@v3);
  glEnd;
{$endif}
  Color:=Vector4(0.0,1.0,1.0,1.0);
{$ifdef NoOpenGL}
  if assigned(fDebugDrawLine) then begin
   fDebugDrawLine(v1,v2,Color);
  end;
{$else}
  glColor4fv(@Color);
  glBegin(GL_LINE_STRIP);
  glVertex3fv(@v1);
  glVertex3fv(@v2);
  glEnd;
{$endif}

  v0:=v;
  v1:=Vector3Add(v0,Vector3ScalarMul(fVisualDebugAirResistanceForce,1.0));
  Color:=Vector4(0.0,1.0,0.0,1.0);
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

  v0:=v;
  v1:=Vector3Add(v0,Vector3ScalarMul(fVisualDebugDownForce,1.0));
  Color:=Vector4(0.5,0.25,0.75,1.0);
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

 end;
 for WheelID:=Low(TWheelID) to High(TWheelID) do begin

  Wheel:=fWheels[WheelID];

  if Wheel.IsGrounded then begin
   Color:=Vector4(0.0,0.0,1.0,1.0);
  end else begin
   Color:=Vector4(1.0,0.0,1.0,1.0);
  end;

  v0:=Vector3TermMatrixMul(Wheel.GetSpringRelativePosition,fVisualWorldTransform);
  v1:=Vector3Add(v0,Vector3ScalarMul(fVisualWorldDown,Wheel.fSpring.fCurrentLength));
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

  Color:=Vector4(1.0,0.0,1.0,1.0);
  v0:=Vector3TermMatrixMul(Wheel.GetSpringRelativePosition,fVisualWorldTransform);
  v1:=Vector3Add(v0,Wheel.fVisualDebugAntiRollForce);
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

  Color:=Vector4(1.0,0.0,0.0,1.0);
  v0:=Vector3TermMatrixMul(Wheel.GetSpringRelativePosition,fVisualWorldTransform);
  v1:=Vector3Add(v0,Wheel.fVisualDebugAccelerationForce);
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

  Color:=Vector4(1.0,0.5,0.5,1.0);
  v0:=Vector3TermMatrixMul(Wheel.GetSpringRelativePosition,fVisualWorldTransform);
  v1:=Vector3Add(v0,Wheel.fVisualDebugDeaccelerationForce);
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

  Color:=Vector4(1.0,0.75,0.25,1.0);
  v0:=Vector3TermMatrixMul(Wheel.GetSpringRelativePosition,fVisualWorldTransform);
  v1:=Vector3Add(v0,Wheel.fVisualDebugSlideForce);
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
  v:=Vector3TermMatrixMul(Vector3Origin,Wheel.fVisualWorldTransform);
  v0:=v;
  for Index:=0 to 16 do begin
   if assigned(fVehicle.fDebugDrawLine) then begin
    v1:=v0;
    v0:=Vector3TermMatrixMul(Vector3Add(Vector3Add(Vector3Origin,Vector3ScalarMul(Vector3YAxis,Sin((Index/16)*PI*2)*fSettings.fWheelsRadius)),Vector3ScalarMul(Vector3ZAxis,Cos((Index/16)*PI*2)*fSettings.fWheelsRadius)),Wheel.fVisualWorldTransform);
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
  v0:=Vector3TermMatrixMul(Vector3Origin,Wheel.fVisualWorldTransform);
  glVertex3fv(@v0);
  for Index:=0 to 16 do begin
   v0:=Vector3TermMatrixMul(Vector3Add(Vector3Add(Vector3Origin,Vector3ScalarMul(Vector3YAxis,Sin((Index/16)*PI*2)*fSettings.fWheelsRadius)),Vector3ScalarMul(Vector3ZAxis,Cos((Index/16)*PI*2)*fSettings.fWheelsRadius)),Wheel.fVisualWorldTransform);
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

