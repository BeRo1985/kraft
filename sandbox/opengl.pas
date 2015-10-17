unit opengl;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {-$pic off}
 {$define caninline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define delphi}
 {$undef HasSAR}
 {$define UseDIV}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef cpu386}
 {$define cpux86}
{$endif}
{$ifdef cpuamd64}
 {$define cpux86}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$ifdef windows}
 {$define win}
{$endif}
{$ifdef sdl20}
 {$define sdl}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$ifdef fpc}
 {$define caninline}
{$else}
 {$undef caninline}
 {$ifdef ver180}
  {$define caninline}
 {$else}
  {$ifdef conditionalexpressions}
   {$if compilerversion>=18}
    {$define caninline}
   {$ifend}
  {$endif}
 {$endif}
{$endif}
{$ifdef gles20}
{** OpenGL ES 2.0 headers
 **
 ** Ported/Translated for FreePascal and Delphi by Benjamin 'BeRo' Rosseaux
 ** benjamin@rosseaux.com - http://www.rosseaux.com
 ** 
 ** EGL part:
 **
 ** Copyright (c) 2007-2009 The Khronos Group Inc.
 **
 ** Permission is hereby granted, free of charge, to any person obtaining a
 ** copy of this software and/or associated documentation files (the
 ** "Materials"), to deal in the Materials without restriction, including
 ** without limitation the rights to use, copy, modify, merge, publish,
 ** distribute, sublicense, and/or sell copies of the Materials, and to
 ** permit persons to whom the Materials are furnished to do so, subject to
 ** the following conditions:
 **
 ** The above copyright notice and this permission notice shall be included
 ** in all copies or substantial portions of the Materials.
 **
 ** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 ** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 ** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 ** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 ** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 ** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
 ** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 **
 ** GLESv2 part:
 **
 ** This document is licensed under the SGI Free Software B License Version
 ** 2.0. For details, see http://oss.sgi.com/projects/FreeB/ 
 **}
{$ifdef android}
 {$define NoDynamicLoad}
{$endif}
{$ifdef linux}
{$ifndef android}
 {$define EGL}
{$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef windows}
 {$define EGL}
{$endif}

interface

uses SysUtils{$ifdef fpc},dynlibs{$endif}{$ifdef linux},ctypes{$endif}{$ifdef windows},Windows{$endif};

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
{$ifdef EGL}
  EGLLibName={$ifdef windows}'libEGL.dll'{$else}'libEGL.so'{$endif};
{$endif}
  GLES20LibName={$ifdef darwin}'/System/Library/Frameworks/OpenGLES.framework/OpenGLES'{$else}{$ifdef windows}'libGLESv2.dll'{$else}'libGLESv2.so'{$endif}{$endif};

{Type
  Pansichar  = ^ansichar;}

{$ifdef fpc}
type GLptruint=ptruint;
     GLptrint=ptrint;
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
type GLptruint=NativeUInt;
     GLptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
{$ifdef cpu64}
type GLptruint=qword;
     GLptrint=int64;
{$else}
type GLptruint=longword;
     GLptrint=longint;
{$endif}
{$endif}

{$ifdef EGL}
type
  PEGLConfig  = ^EGLConfig;
  PEGLint  = ^EGLint;
     EGLint = {$ifdef win64}int64{$else}longint{$endif}; // Why int64 only on win64 and not even on 64-bit linux???

     EGLConfig = pointer;

  { EGL Types  }
  { EGLint is defined in eglplatform.h  }

  type

{$ifdef linux}
     EGLNativeDisplayType = pointer; // PDisplay;

     EGLNativeWindowType = culong; //TWindow;

     EGLNativePixmapType = culong; //TPixmap;
{$else linux}
{$ifdef windows}
     EGLNativeDisplayType = HDC;

     EGLNativeWindowType = HWND;

     EGLNativePixmapType = HBITMAP;
{$else windows}
     EGLNativeDisplayType = GLptrint;

     EGLNativeWindowType = pointer;

     EGLNativePixmapType = pointer;
{$endif windows}
{$endif linux}

     EGLBoolean = dword;

     EGLenum = dword;


     EGLContext = pointer;

     EGLDisplay = pointer;

     EGLSurface = pointer;

     EGLClientBuffer = pointer;
  { EGL Versioning  }

  const
     EGL_VERSION_1_0 = 1;     
     EGL_VERSION_1_1 = 1;     
     EGL_VERSION_1_2 = 1;     
     EGL_VERSION_1_3 = 1;     
     EGL_VERSION_1_4 = 1;     
  { EGL Enumerants. Bitmasks and other exceptional cases aside, most
   * enums are assigned unique values starting at 0x3000.
    }
  { EGL aliases  }
     EGL_FALSE = 0;     
     EGL_TRUE = 1;     
  { Out-of-band handle values  }
  { was #define dname def_expr }
  function EGL_DEFAULT_DISPLAY : EGLNativeDisplayType;    

  { was #define dname def_expr }
  function EGL_NO_CONTEXT : EGLContext;    

  { was #define dname def_expr }
  function EGL_NO_DISPLAY : EGLDisplay;    

  { was #define dname def_expr }
  function EGL_NO_SURFACE : EGLSurface;    

  { Out-of-band attribute value  }
  { was #define dname def_expr }
  function EGL_DONT_CARE : EGLint;    

  { Errors / GetError return values  }

  const
     EGL_SUCCESS = $3000;     
     EGL_NOT_INITIALIZED = $3001;     
     EGL_BAD_ACCESS = $3002;     
     EGL_BAD_ALLOC = $3003;     
     EGL_BAD_ATTRIBUTE = $3004;     
     EGL_BAD_CONFIG = $3005;     
     EGL_BAD_CONTEXT = $3006;     
     EGL_BAD_CURRENT_SURFACE = $3007;     
     EGL_BAD_DISPLAY = $3008;     
     EGL_BAD_MATCH = $3009;     
     EGL_BAD_NATIVE_PIXMAP = $300A;     
     EGL_BAD_NATIVE_WINDOW = $300B;     
     EGL_BAD_PARAMETER = $300C;     
     EGL_BAD_SURFACE = $300D;     
  { EGL 1.1 - IMG_power_management  }
     EGL_CONTEXT_LOST = $300E;     
  { Reserved 0x300F-0x301F for additional errors  }
  { Config attributes  }
     EGL_BUFFER_SIZE = $3020;     
     EGL_ALPHA_SIZE = $3021;     
     EGL_BLUE_SIZE = $3022;     
     EGL_GREEN_SIZE = $3023;
     EGL_RED_SIZE = $3024;     
     EGL_DEPTH_SIZE = $3025;     
     EGL_STENCIL_SIZE = $3026;
     EGL_CONFIG_CAVEAT = $3027;     
     EGL_CONFIG_ID = $3028;     
     EGL_LEVEL = $3029;     
     EGL_MAX_PBUFFER_HEIGHT = $302A;     
     EGL_MAX_PBUFFER_PIXELS = $302B;     
     EGL_MAX_PBUFFER_WIDTH = $302C;     
     EGL_NATIVE_RENDERABLE = $302D;     
     EGL_NATIVE_VISUAL_ID = $302E;
     EGL_NATIVE_VISUAL_TYPE = $302F;     
     EGL_PRESERVED_RESOURCES = $3030;     
     EGL_SAMPLES = $3031;     
     EGL_SAMPLE_BUFFERS = $3032;     
     EGL_SURFACE_TYPE = $3033;     
     EGL_TRANSPARENT_TYPE = $3034;     
     EGL_TRANSPARENT_BLUE_VALUE = $3035;     
     EGL_TRANSPARENT_GREEN_VALUE = $3036;     
     EGL_TRANSPARENT_RED_VALUE = $3037;     
  { Attrib list terminator  }
     EGL_NONE = $3038;     
     EGL_BIND_TO_TEXTURE_RGB = $3039;     
     EGL_BIND_TO_TEXTURE_RGBA = $303A;     
     EGL_MIN_SWAP_INTERVAL = $303B;     
     EGL_MAX_SWAP_INTERVAL = $303C;     
     EGL_LUMINANCE_SIZE = $303D;     
     EGL_ALPHA_MASK_SIZE = $303E;     
     EGL_COLOR_BUFFER_TYPE = $303F;     
     EGL_RENDERABLE_TYPE = $3040;     
  { Pseudo-attribute (not queryable)  }
     EGL_MATCH_NATIVE_PIXMAP = $3041;     
     EGL_CONFORMANT = $3042;     
  { Reserved 0x3041-0x304F for additional config attributes  }
  { Config attribute values  }
  { EGL_CONFIG_CAVEAT value  }
     EGL_SLOW_CONFIG = $3050;     
  { EGL_CONFIG_CAVEAT value  }
     EGL_NON_CONFORMANT_CONFIG = $3051;     
  { EGL_TRANSPARENT_TYPE value  }
     EGL_TRANSPARENT_RGB = $3052;     
  { EGL_COLOR_BUFFER_TYPE value  }
     EGL_RGB_BUFFER = $308E;     
  { EGL_COLOR_BUFFER_TYPE value  }
     EGL_LUMINANCE_BUFFER = $308F;     
  { More config attribute values, for EGL_TEXTURE_FORMAT  }
     EGL_NO_TEXTURE = $305C;     
     EGL_TEXTURE_RGB = $305D;     
     EGL_TEXTURE_RGBA = $305E;     
     EGL_TEXTURE_2D = $305F;     
  { Config attribute mask bits  }
  { EGL_SURFACE_TYPE mask bits  }
     EGL_PBUFFER_BIT = $0001;     
  { EGL_SURFACE_TYPE mask bits  }
     EGL_PIXMAP_BIT = $0002;     
  { EGL_SURFACE_TYPE mask bits  }
     EGL_WINDOW_BIT = $0004;     
  { EGL_SURFACE_TYPE mask bits  }
     EGL_VG_COLORSPACE_LINEAR_BIT = $0020;     
  { EGL_SURFACE_TYPE mask bits  }
     EGL_VG_ALPHA_FORMAT_PRE_BIT = $0040;     
  { EGL_SURFACE_TYPE mask bits  }
     EGL_MULTISAMPLE_RESOLVE_BOX_BIT = $0200;     
  { EGL_SURFACE_TYPE mask bits  }
     EGL_SWAP_BEHAVIOR_PRESERVED_BIT = $0400;     
  { EGL_RENDERABLE_TYPE mask bits  }
     EGL_OPENGL_ES_BIT = $0001;     
  { EGL_RENDERABLE_TYPE mask bits  }
     EGL_OPENVG_BIT = $0002;     
  { EGL_RENDERABLE_TYPE mask bits  }
     EGL_OPENGL_ES2_BIT = $0004;     
  { EGL_RENDERABLE_TYPE mask bits  }
     EGL_OPENGL_BIT = $0008;
  { QueryString targets  }
     EGL_VENDOR = $3053;     
     EGL_VERSION = $3054;     
     EGL_EXTENSIONS = $3055;     
     EGL_CLIENT_APIS = $308D;     
  { QuerySurface / SurfaceAttrib / CreatePbufferSurface targets  }
     EGL_HEIGHT = $3056;     
     EGL_WIDTH = $3057;
     EGL_LARGEST_PBUFFER = $3058;     
     EGL_TEXTURE_FORMAT = $3080;     
     EGL_TEXTURE_TARGET = $3081;     
     EGL_MIPMAP_TEXTURE = $3082;     
     EGL_MIPMAP_LEVEL = $3083;     
     EGL_RENDER_BUFFER = $3086;     
     EGL_VG_COLORSPACE = $3087;
     EGL_VG_ALPHA_FORMAT = $3088;     
     EGL_HORIZONTAL_RESOLUTION = $3090;     
     EGL_VERTICAL_RESOLUTION = $3091;     
     EGL_PIXEL_ASPECT_RATIO = $3092;     
     EGL_SWAP_BEHAVIOR = $3093;     
     EGL_MULTISAMPLE_RESOLVE = $3099;     
  { EGL_RENDER_BUFFER values / BindTexImage / ReleaseTexImage buffer targets  }
     EGL_BACK_BUFFER = $3084;     
     EGL_SINGLE_BUFFER = $3085;     
  { OpenVG color spaces  }
  { EGL_VG_COLORSPACE value  }
     EGL_VG_COLORSPACE_sRGB = $3089;     
  { EGL_VG_COLORSPACE value  }
     EGL_VG_COLORSPACE_LINEAR = $308A;     
  { OpenVG alpha formats  }
  { EGL_ALPHA_FORMAT value  }
     EGL_VG_ALPHA_FORMAT_NONPRE = $308B;
  { EGL_ALPHA_FORMAT value  }
     EGL_VG_ALPHA_FORMAT_PRE = $308C;     
  { Constant scale factor by which fractional display resolutions &
   * aspect ratio are scaled when queried as integer values.
    }
     EGL_DISPLAY_SCALING = 10000;     
  { Unknown display resolution/aspect ratio  }
  { was #define dname def_expr }
  function EGL_UNKNOWN : EGLint;    

  { Back buffer swap behaviors  }
  { EGL_SWAP_BEHAVIOR value  }

  const
     EGL_BUFFER_PRESERVED = $3094;     
  { EGL_SWAP_BEHAVIOR value  }
     EGL_BUFFER_DESTROYED = $3095;     
  { CreatePbufferFromClientBuffer buffer types  }
     EGL_OPENVG_IMAGE = $3096;     
  { QueryContext targets  }
     EGL_CONTEXT_CLIENT_TYPE = $3097;     
  { CreateContext attributes  }
     EGL_CONTEXT_CLIENT_VERSION = $3098;     
  { Multisample resolution behaviors  }
  { EGL_MULTISAMPLE_RESOLVE value  }
     EGL_MULTISAMPLE_RESOLVE_DEFAULT = $309A;     
  { EGL_MULTISAMPLE_RESOLVE value  }
     EGL_MULTISAMPLE_RESOLVE_BOX = $309B;     
  { BindAPI/QueryAPI targets  }
     EGL_OPENGL_ES_API = $30A0;     
     EGL_OPENVG_API = $30A1;     
     EGL_OPENGL_API = $30A2;     
  { GetCurrentSurface targets  }
     EGL_DRAW = $3059;     
     EGL_READ = $305A;
  { WaitNative engines  }
     EGL_CORE_NATIVE_ENGINE = $305B;     
  { EGL 1.2 tokens renamed for consistency in EGL 1.3  }
     EGL_COLORSPACE = EGL_VG_COLORSPACE;     
     EGL_ALPHA_FORMAT = EGL_VG_ALPHA_FORMAT;     
     EGL_COLORSPACE_sRGB = EGL_VG_COLORSPACE_sRGB;     
     EGL_COLORSPACE_LINEAR = EGL_VG_COLORSPACE_LINEAR;     
     EGL_ALPHA_FORMAT_NONPRE = EGL_VG_ALPHA_FORMAT_NONPRE;     
     EGL_ALPHA_FORMAT_PRE = EGL_VG_ALPHA_FORMAT_PRE;     
  { EGL extensions must request enum blocks from the Khronos
   * API Registrar, who maintains the enumerant registry. Submit
   * a bug in Khronos Bugzilla against task "Registry".
    }
  { EGL Functions  }

{$ifdef NoDynamicLoad}
function eglGetError:EGLint;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglGetError';
function eglGetDisplay(display_id:EGLNativeDisplayType):EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglGetDisplay';
function eglInitialize(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglInitialize';
function eglTerminate(dpy:EGLDisplay):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglTerminate';
(* Const before type ignored *)
function eglQueryString(dpy:EGLDisplay; name:EGLint):pchar;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglQueryString';
function eglGetConfigs(dpy:EGLDisplay; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglGetConfigs';
(* Const before type ignored *)
function eglChooseConfig(dpy:EGLDisplay; attrib_list:pEGLint; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglChooseConfig';
function eglGetConfigAttrib(dpy:EGLDisplay; config:EGLConfig; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglGetConfigAttrib';
(* Const before type ignored *)
function eglCreateWindowSurface(dpy:EGLDisplay; config:EGLConfig; win:EGLNativeWindowType; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglCreateWindowSurface';
(* Const before type ignored *)
function eglCreatePbufferSurface(dpy:EGLDisplay; config:EGLConfig; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglCreatePbufferSurface';
(* Const before type ignored *)
function eglCreatePixmapSurface(dpy:EGLDisplay; config:EGLConfig; pixmap:EGLNativePixmapType; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglCreatePixmapSurface';
function eglDestroySurface(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglDestroySurface';
function eglQuerySurface(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglQuerySurface';
function eglBindAPI(api:EGLenum):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglBindAPI';
function eglQueryAPI:EGLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglQueryAPI';
function eglWaitClient:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglWaitClient';
function eglReleaseThread:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglReleaseThread';
(* Const before type ignored *)
function eglCreatePbufferFromClientBuffer(dpy:EGLDisplay; buftype:EGLenum; buffer:EGLClientBuffer; config:EGLConfig; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglCreatePbufferFromClientBuffer';
function eglSurfaceAttrib(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglSurfaceAttrib';
function eglBindTexImage(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglBindTexImage';
function eglReleaseTexImage(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglReleaseTexImage';
function eglSwapInterval(dpy:EGLDisplay; interval:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglSwapInterval';
(* Const before type ignored *)
function eglCreateContext(dpy:EGLDisplay; config:EGLConfig; share_context:EGLContext; attrib_list:pEGLint):EGLContext;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglCreateContext';
function eglDestroyContext(dpy:EGLDisplay; ctx:EGLContext):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglDestroyContext';
function eglMakeCurrent(dpy:EGLDisplay; draw:EGLSurface; read:EGLSurface; ctx:EGLContext):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglMakeCurrent';
function eglGetCurrentContext:EGLContext;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglGetCurrentContext';
function eglGetCurrentSurface(readdraw:EGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglGetCurrentSurface';
function eglGetCurrentDisplay:EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglGetCurrentDisplay';
function eglQueryContext(dpy:EGLDisplay; ctx:EGLContext; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglQueryContext';
function eglWaitGL:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglWaitGL';
function eglWaitNative(engine:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglWaitNative';
function eglSwapBuffers(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglSwapBuffers';
function eglCopyBuffers(dpy:EGLDisplay; surface:EGLSurface; target:EGLNativePixmapType):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglCopyBuffers';
{$else}
  var
    eglGetError : function:EGLint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetDisplay : function(display_id:EGLNativeDisplayType):EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglInitialize : function(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglTerminate : function(dpy:EGLDisplay):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    eglQueryString : function(dpy:EGLDisplay; name:EGLint):Pansichar;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetConfigs : function(dpy:EGLDisplay; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    eglChooseConfig : function(dpy:EGLDisplay; attrib_list:pEGLint; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetConfigAttrib : function(dpy:EGLDisplay; config:EGLConfig; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    eglCreateWindowSurface : function(dpy:EGLDisplay; config:EGLConfig; win:EGLNativeWindowType; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    eglCreatePbufferSurface : function(dpy:EGLDisplay; config:EGLConfig; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    eglCreatePixmapSurface : function(dpy:EGLDisplay; config:EGLConfig; pixmap:EGLNativePixmapType; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglDestroySurface : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglQuerySurface : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglBindAPI : function(api:EGLenum):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglQueryAPI : function:EGLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglWaitClient : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglReleaseThread : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    eglCreatePbufferFromClientBuffer : function(dpy:EGLDisplay; buftype:EGLenum; buffer:EGLClientBuffer; config:EGLConfig; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglSurfaceAttrib : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglBindTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglReleaseTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglSwapInterval : function(dpy:EGLDisplay; interval:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    eglCreateContext : function(dpy:EGLDisplay; config:EGLConfig; share_context:EGLContext; attrib_list:pEGLint):EGLContext;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglDestroyContext : function(dpy:EGLDisplay; ctx:EGLContext):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglMakeCurrent : function(dpy:EGLDisplay; draw:EGLSurface; read:EGLSurface; ctx:EGLContext):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetCurrentContext : function:EGLContext;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetCurrentSurface : function(readdraw:EGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetCurrentDisplay : function:EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglQueryContext : function(dpy:EGLDisplay; ctx:EGLContext; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglWaitGL : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglWaitNative : function(engine:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglSwapBuffers : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglCopyBuffers : function(dpy:EGLDisplay; surface:EGLSurface; target:EGLNativePixmapType):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}
  { This is a generic function pointer type, whose name indicates it must
   * be cast to the proper type *and calling convention* before use.
    }

  type

     __eglMustCastToProperFunctionPointerType = procedure (_para1:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  { Now, define eglGetProcAddress using the generic function ptr. type  }
(* Const before type ignored *)

{$ifdef NoDynamicLoad}
function eglGetProcAddress(procname:pchar):__eglMustCastToProperFunctionPointerType;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external EGLLibName name 'eglGetProcAddress';
{$else}
  var
    eglGetProcAddress : function(procname:Pansichar):__eglMustCastToProperFunctionPointerType;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}
  { Header file version number  }
  { Current version at http://www.khronos.org/registry/egl/  }

  const
     EGL_EGLEXT_VERSION = 3;
     EGL_KHR_config_attribs = 1;
  { EGLConfig attribute  }
     EGL_CONFORMANT_KHR = $3042;
  { EGL_SURFACE_TYPE bitfield  }
     EGL_VG_COLORSPACE_LINEAR_BIT_KHR = $0020;
  { EGL_SURFACE_TYPE bitfield  }
     EGL_VG_ALPHA_FORMAT_PRE_BIT_KHR = $0040;     
     EGL_KHR_lock_surface = 1;     
  { EGL_LOCK_USAGE_HINT_KHR bitfield  }
     EGL_READ_SURFACE_BIT_KHR = $0001;     
  { EGL_LOCK_USAGE_HINT_KHR bitfield  }
     EGL_WRITE_SURFACE_BIT_KHR = $0002;     
  { EGL_SURFACE_TYPE bitfield  }
     EGL_LOCK_SURFACE_BIT_KHR = $0080;     
  { EGL_SURFACE_TYPE bitfield  }
     EGL_OPTIMAL_FORMAT_BIT_KHR = $0100;     
  { EGLConfig attribute  }
     EGL_MATCH_FORMAT_KHR = $3043;     
  { EGL_MATCH_FORMAT_KHR value  }
     EGL_FORMAT_RGB_565_EXACT_KHR = $30C0;     
  { EGL_MATCH_FORMAT_KHR value  }
     EGL_FORMAT_RGB_565_KHR = $30C1;     
  { EGL_MATCH_FORMAT_KHR value  }
     EGL_FORMAT_RGBA_8888_EXACT_KHR = $30C2;     
  { EGL_MATCH_FORMAT_KHR value  }
     EGL_FORMAT_RGBA_8888_KHR = $30C3;     
  { eglLockSurfaceKHR attribute  }
     EGL_MAP_PRESERVE_PIXELS_KHR = $30C4;
  { eglLockSurfaceKHR attribute  }
     EGL_LOCK_USAGE_HINT_KHR = $30C5;     
  { eglQuerySurface attribute  }
     EGL_BITMAP_POINTER_KHR = $30C6;
  { eglQuerySurface attribute  }
     EGL_BITMAP_PITCH_KHR = $30C7;     
  { eglQuerySurface attribute  }
     EGL_BITMAP_ORIGIN_KHR = $30C8;     
  { eglQuerySurface attribute  }
     EGL_BITMAP_PIXEL_RED_OFFSET_KHR = $30C9;     
  { eglQuerySurface attribute  }
     EGL_BITMAP_PIXEL_GREEN_OFFSET_KHR = $30CA;     
  { eglQuerySurface attribute  }
     EGL_BITMAP_PIXEL_BLUE_OFFSET_KHR = $30CB;     
  { eglQuerySurface attribute  }
     EGL_BITMAP_PIXEL_ALPHA_OFFSET_KHR = $30CC;     
  { eglQuerySurface attribute  }
     EGL_BITMAP_PIXEL_LUMINANCE_OFFSET_KHR = $30CD;     
  { EGL_BITMAP_ORIGIN_KHR value  }
     EGL_LOWER_LEFT_KHR = $30CE;
  { EGL_BITMAP_ORIGIN_KHR value  }
     EGL_UPPER_LEFT_KHR = $30CF;     
(* Const before type ignored *)

  const
     EGL_KHR_image = 1;     
  { eglCreateImageKHR target  }
     EGL_NATIVE_PIXMAP_KHR = $30B0;     

type
     EGLImageKHR = pointer;
  { was #define dname def_expr }
  function EGL_NO_IMAGE_KHR : EGLImageKHR;

(* Const before type ignored *)

  const
     EGL_KHR_vg_parent_image = 1;
  { eglCreateImageKHR target  }
     EGL_VG_PARENT_IMAGE_KHR = $30BA;     
     EGL_KHR_gl_texture_2D_image = 1;     
  { eglCreateImageKHR target  }
     EGL_GL_TEXTURE_2D_KHR = $30B1;     
  { eglCreateImageKHR attribute  }
     EGL_GL_TEXTURE_LEVEL_KHR = $30BC;     
     EGL_KHR_gl_texture_cubemap_image = 1;     
  { eglCreateImageKHR target  }
     EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X_KHR = $30B3;     
  { eglCreateImageKHR target  }
     EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X_KHR = $30B4;     
  { eglCreateImageKHR target  }
     EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y_KHR = $30B5;     
  { eglCreateImageKHR target  }
     EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_KHR = $30B6;     
  { eglCreateImageKHR target  }
     EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z_KHR = $30B7;     
  { eglCreateImageKHR target  }
     EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_KHR = $30B8;     
     EGL_KHR_gl_texture_3D_image = 1;     
  { eglCreateImageKHR target  }
     EGL_GL_TEXTURE_3D_KHR = $30B2;     
  { eglCreateImageKHR attribute  }
     EGL_GL_TEXTURE_ZOFFSET_KHR = $30BD;     
     EGL_KHR_gl_renderbuffer_image = 1;
  { eglCreateImageKHR target  }
     EGL_GL_RENDERBUFFER_KHR = $30B9;     
     EGL_KHR_image_base = 1;     
  { Most interfaces defined by EGL_KHR_image_pixmap above  }
  { eglCreateImageKHR attribute  }
     EGL_IMAGE_PRESERVED_KHR = $30D2;     
     EGL_KHR_image_pixmap = 1;     
  { Interfaces defined by EGL_KHR_image above  }

{$endif EGL}

  
type
  PGLubyte = ^GLubyte;
  PGLboolean  = ^GLboolean;
  PGLenum  = ^GLenum;
  PGLfloat  = ^GLfloat;
  PGLint  = ^GLint;
  PGLsizei  = ^GLsizei;
  PGLuint  = ^GLuint;

  {-------------------------------------------------------------------------
   * Data type definitions
   *----------------------------------------------------------------------- }

     GLvoid = pointer;
     TGLvoid = GLvoid;

     GLenum = dword;
     TGLenum = GLenum;

     GLboolean = byte;
     TGLboolean = GLboolean;

     GLbitfield = dword;
     TGLbitfield = GLbitfield;

     GLbyte = shortint;
     TGLbyte = GLbyte;

     GLshort = smallint;
     TGLshort = GLshort;

     GLint = longint;
     TGLint = GLint;

     GLsizei = longint;
     TGLsizei = GLsizei;

     GLubyte = byte;
     TGLubyte = GLubyte;

     GLushort = word;
     TGLushort = GLushort;

     GLuint = longword;
     TGLuint = GLuint;

     GLfloat = single;
     TGLfloat = GLfloat;

     GLclampf = single;
     TGLclampf = GLclampf;

     GLfixed = longint;
     TGLfixed = GLfixed;
  { GL types for handling large vertex buffer objects  }

     GLintptr = GLptrint;

     GLsizeiptr = GLptrint;
  { OpenGL ES core versions  }

  const
     GL_ES_VERSION_2_0 = 1;     
  { ClearBufferMask  }
     GL_DEPTH_BUFFER_BIT = $00000100;     
     GL_STENCIL_BUFFER_BIT = $00000400;     
     GL_COLOR_BUFFER_BIT = $00004000;
  { Boolean  }
     GL_FALSE = 0;     
     GL_TRUE = 1;     
  { BeginMode  }
     GL_POINTS = $0000;     
     GL_LINES = $0001;     
     GL_LINE_LOOP = $0002;     
     GL_LINE_STRIP = $0003;     
     GL_TRIANGLES = $0004;     
     GL_TRIANGLE_STRIP = $0005;     
     GL_TRIANGLE_FAN = $0006;     
  { AlphaFunction (not supported in ES20)  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { BlendingFactorDest  }
     GL_ZERO = 0;     
     GL_ONE = 1;     
     GL_SRC_COLOR = $0300;     
     GL_ONE_MINUS_SRC_COLOR = $0301;     
     GL_SRC_ALPHA = $0302;
     GL_ONE_MINUS_SRC_ALPHA = $0303;
     GL_DST_ALPHA = $0304;     
     GL_ONE_MINUS_DST_ALPHA = $0305;     
  { BlendingFactorSrc  }
  {      GL_ZERO  }
  {      GL_ONE  }
     GL_DST_COLOR = $0306;     
     GL_ONE_MINUS_DST_COLOR = $0307;     
     GL_SRC_ALPHA_SATURATE = $0308;
  {      GL_SRC_ALPHA  }
  {      GL_ONE_MINUS_SRC_ALPHA  }
  {      GL_DST_ALPHA  }
  {      GL_ONE_MINUS_DST_ALPHA  }
  { BlendEquationSeparate  }
     GL_FUNC_ADD = $8006;     
     GL_BLEND_EQUATION = $8009;     
  { same as BLEND_EQUATION  }
     GL_BLEND_EQUATION_RGB = $8009;     
     GL_BLEND_EQUATION_ALPHA = $883D;     
  { BlendSubtract  }
     GL_FUNC_SUBTRACT = $800A;     
     GL_FUNC_REVERSE_SUBTRACT = $800B;     
  { Separate Blend Functions  }
     GL_BLEND_DST_RGB = $80C8;     
     GL_BLEND_SRC_RGB = $80C9;     
     GL_BLEND_DST_ALPHA = $80CA;     
     GL_BLEND_SRC_ALPHA = $80CB;     
     GL_CONSTANT_COLOR = $8001;     
     GL_ONE_MINUS_CONSTANT_COLOR = $8002;     
     GL_CONSTANT_ALPHA = $8003;     
     GL_ONE_MINUS_CONSTANT_ALPHA = $8004;     
     GL_BLEND_COLOR = $8005;     
  { Buffer Objects  }
     GL_ARRAY_BUFFER = $8892;     
     GL_ELEMENT_ARRAY_BUFFER = $8893;
     GL_ARRAY_BUFFER_BINDING = $8894;
     GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;     
     GL_STREAM_DRAW = $88E0;     
     GL_STATIC_DRAW = $88E4;     
     GL_DYNAMIC_DRAW = $88E8;     
     GL_BUFFER_SIZE = $8764;     
     GL_BUFFER_USAGE = $8765;     
     GL_CURRENT_VERTEX_ATTRIB = $8626;     
  { CullFaceMode  }
     GL_FRONT = $0404;
     GL_BACK = $0405;     
     GL_FRONT_AND_BACK = $0408;     
  { DepthFunction  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { EnableCap  }
     GL_TEXTURE_2D = $0DE1;     
     GL_CULL_FACE = $0B44;     
     GL_BLEND = $0BE2;     
     GL_DITHER = $0BD0;     
     GL_STENCIL_TEST = $0B90;     
     GL_DEPTH_TEST = $0B71;     
     GL_SCISSOR_TEST = $0C11;     
     GL_POLYGON_OFFSET_FILL = $8037;     
     GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;     
     GL_SAMPLE_COVERAGE = $80A0;     
  { ErrorCode  }
     GL_NO_ERROR = 0;     
     GL_INVALID_ENUM = $0500;
     GL_INVALID_VALUE = $0501;
     GL_INVALID_OPERATION = $0502;     
     GL_OUT_OF_MEMORY = $0505;     
  { FrontFaceDirection  }
     GL_CW = $0900;     
     GL_CCW = $0901;     
  { GetPName  }
     GL_LINE_WIDTH = $0B21;     
     GL_ALIASED_POINT_SIZE_RANGE = $846D;
     GL_ALIASED_LINE_WIDTH_RANGE = $846E;
     GL_CULL_FACE_MODE = $0B45;     
     GL_FRONT_FACE = $0B46;     
     GL_DEPTH_RANGE = $0B70;     
     GL_DEPTH_WRITEMASK = $0B72;     
     GL_DEPTH_CLEAR_VALUE = $0B73;     
     GL_DEPTH_FUNC = $0B74;     
     GL_STENCIL_CLEAR_VALUE = $0B91;     
     GL_STENCIL_FUNC = $0B92;     
     GL_STENCIL_FAIL = $0B94;     
     GL_STENCIL_PASS_DEPTH_FAIL = $0B95;     
     GL_STENCIL_PASS_DEPTH_PASS = $0B96;     
     GL_STENCIL_REF = $0B97;     
     GL_STENCIL_VALUE_MASK = $0B93;     
     GL_STENCIL_WRITEMASK = $0B98;     
     GL_STENCIL_BACK_FUNC = $8800;     
     GL_STENCIL_BACK_FAIL = $8801;     
     GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;     
     GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;     
     GL_STENCIL_BACK_REF = $8CA3;     
     GL_STENCIL_BACK_VALUE_MASK = $8CA4;     
     GL_STENCIL_BACK_WRITEMASK = $8CA5;     
     GL_VIEWPORT = $0BA2;     
     GL_SCISSOR_BOX = $0C10;     
  {      GL_SCISSOR_TEST  }
     GL_COLOR_CLEAR_VALUE = $0C22;
     GL_COLOR_WRITEMASK = $0C23;
     GL_UNPACK_ALIGNMENT = $0CF5;     
     GL_PACK_ALIGNMENT = $0D05;     
     GL_MAX_TEXTURE_SIZE = $0D33;     
     GL_MAX_VIEWPORT_DIMS = $0D3A;     
     GL_SUBPIXEL_BITS = $0D50;     
     GL_RED_BITS = $0D52;     
     GL_GREEN_BITS = $0D53;     
     GL_BLUE_BITS = $0D54;
     GL_ALPHA_BITS = $0D55;
     GL_DEPTH_BITS = $0D56;     
     GL_STENCIL_BITS = $0D57;     
     GL_POLYGON_OFFSET_UNITS = $2A00;     
  {      GL_POLYGON_OFFSET_FILL  }
     GL_POLYGON_OFFSET_FACTOR = $8038;     
     GL_TEXTURE_BINDING_2D = $8069;     
     GL_SAMPLE_BUFFERS = $80A8;     
     GL_SAMPLES = $80A9;     
     GL_SAMPLE_COVERAGE_VALUE = $80AA;     
     GL_SAMPLE_COVERAGE_INVERT = $80AB;     
  { GetTextureParameter  }
  {      GL_TEXTURE_MAG_FILTER  }
  {      GL_TEXTURE_MIN_FILTER  }
  {      GL_TEXTURE_WRAP_S  }
  {      GL_TEXTURE_WRAP_T  }
     GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;     
     GL_COMPRESSED_TEXTURE_FORMATS = $86A3;     
  { HintMode  }
     GL_DONT_CARE = $1100;     
     GL_FASTEST = $1101;     
     GL_NICEST = $1102;     
  { HintTarget  }
     GL_GENERATE_MIPMAP_HINT = $8192;     
  { DataType  }
     GL_BYTE = $1400;
     GL_UNSIGNED_BYTE = $1401;
     GL_SHORT = $1402;     
     GL_UNSIGNED_SHORT = $1403;     
     GL_INT = $1404;     
     GL_UNSIGNED_INT = $1405;     
     GL_FLOAT = $1406;     
     GL_FIXED = $140C;     
  { PixelFormat  }
     GL_DEPTH_COMPONENT = $1902;
     GL_ALPHA = $1906;
     GL_RGB = $1907;     
     GL_RGBA = $1908;     
     GL_LUMINANCE = $1909;     
     GL_LUMINANCE_ALPHA = $190A;     
  { PixelType  }
  {      GL_UNSIGNED_BYTE  }
     GL_UNSIGNED_SHORT_4_4_4_4 = $8033;     
     GL_UNSIGNED_SHORT_5_5_5_1 = $8034;     
     GL_UNSIGNED_SHORT_5_6_5 = $8363;     
  { Shaders  }
     GL_FRAGMENT_SHADER = $8B30;     
     GL_VERTEX_SHADER = $8B31;     
     GL_MAX_VERTEX_ATTRIBS = $8869;     
     GL_MAX_VERTEX_UNIFORM_VECTORS = $8DFB;     
     GL_MAX_VARYING_VECTORS = $8DFC;     
     GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;     
     GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;     
     GL_MAX_TEXTURE_IMAGE_UNITS = $8872;     
     GL_MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;     
     GL_SHADER_TYPE = $8B4F;     
     GL_DELETE_STATUS = $8B80;     
     GL_LINK_STATUS = $8B82;     
     GL_VALIDATE_STATUS = $8B83;     
     GL_ATTACHED_SHADERS = $8B85;     
     GL_ACTIVE_UNIFORMS = $8B86;
     GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
     GL_ACTIVE_ATTRIBUTES = $8B89;     
     GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;     
     GL_SHADING_LANGUAGE_VERSION = $8B8C;     
     GL_CURRENT_PROGRAM = $8B8D;     
  { StencilFunction  }
     GL_NEVER = $0200;     
     GL_LESS = $0201;     
     GL_EQUAL = $0202;
     GL_LEQUAL = $0203;
     GL_GREATER = $0204;     
     GL_NOTEQUAL = $0205;     
     GL_GEQUAL = $0206;     
     GL_ALWAYS = $0207;     
  { StencilOp  }
  {      GL_ZERO  }
     GL_KEEP = $1E00;     
     GL_REPLACE = $1E01;     
     GL_INCR = $1E02;     
     GL_DECR = $1E03;     
     GL_INVERT = $150A;     
     GL_INCR_WRAP = $8507;     
     GL_DECR_WRAP = $8508;     
  { StringName  }
     GL_VENDOR = $1F00;     
     GL_RENDERER = $1F01;     
     GL_VERSION = $1F02;     
     GL_EXTENSIONS = $1F03;     
  { TextureMagFilter  }
     GL_NEAREST = $2600;     
     GL_LINEAR = $2601;     
  { TextureMinFilter  }
  {      GL_NEAREST  }
  {      GL_LINEAR  }
     GL_NEAREST_MIPMAP_NEAREST = $2700;
     GL_LINEAR_MIPMAP_NEAREST = $2701;
     GL_NEAREST_MIPMAP_LINEAR = $2702;     
     GL_LINEAR_MIPMAP_LINEAR = $2703;     
  { TextureParameterName  }
     GL_TEXTURE_MAG_FILTER = $2800;     
     GL_TEXTURE_MIN_FILTER = $2801;     
     GL_TEXTURE_WRAP_S = $2802;     
     GL_TEXTURE_WRAP_T = $2803;     
  { TextureTarget  }
  {      GL_TEXTURE_2D  }
     GL_TEXTURE = $1702;     
     GL_TEXTURE_CUBE_MAP = $8513;
     GL_TEXTURE_BINDING_CUBE_MAP = $8514;
     GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;     
     GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;     
     GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;     
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;     
     GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;     
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;     
     GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;     
  { TextureUnit  }
     GL_TEXTURE0 = $84C0;     
     GL_TEXTURE1 = $84C1;     
     GL_TEXTURE2 = $84C2;     
     GL_TEXTURE3 = $84C3;     
     GL_TEXTURE4 = $84C4;     
     GL_TEXTURE5 = $84C5;     
     GL_TEXTURE6 = $84C6;     
     GL_TEXTURE7 = $84C7;     
     GL_TEXTURE8 = $84C8;     
     GL_TEXTURE9 = $84C9;     
     GL_TEXTURE10 = $84CA;     
     GL_TEXTURE11 = $84CB;     
     GL_TEXTURE12 = $84CC;     
     GL_TEXTURE13 = $84CD;
     GL_TEXTURE14 = $84CE;
     GL_TEXTURE15 = $84CF;     
     GL_TEXTURE16 = $84D0;     
     GL_TEXTURE17 = $84D1;     
     GL_TEXTURE18 = $84D2;     
     GL_TEXTURE19 = $84D3;     
     GL_TEXTURE20 = $84D4;     
     GL_TEXTURE21 = $84D5;     
     GL_TEXTURE22 = $84D6;
     GL_TEXTURE23 = $84D7;
     GL_TEXTURE24 = $84D8;     
     GL_TEXTURE25 = $84D9;     
     GL_TEXTURE26 = $84DA;     
     GL_TEXTURE27 = $84DB;     
     GL_TEXTURE28 = $84DC;     
     GL_TEXTURE29 = $84DD;     
     GL_TEXTURE30 = $84DE;     
     GL_TEXTURE31 = $84DF;     
     GL_ACTIVE_TEXTURE = $84E0;     
  { TextureWrapMode  }
     GL_REPEAT = $2901;     
     GL_CLAMP_TO_EDGE = $812F;     
     GL_MIRRORED_REPEAT = $8370;     
  { Uniform Types  }
     GL_FLOAT_VEC2 = $8B50;     
     GL_FLOAT_VEC3 = $8B51;     
     GL_FLOAT_VEC4 = $8B52;     
     GL_INT_VEC2 = $8B53;     
     GL_INT_VEC3 = $8B54;     
     GL_INT_VEC4 = $8B55;     
     GL_BOOL = $8B56;     
     GL_BOOL_VEC2 = $8B57;     
     GL_BOOL_VEC3 = $8B58;     
     GL_BOOL_VEC4 = $8B59;     
     GL_FLOAT_MAT2 = $8B5A;
     GL_FLOAT_MAT3 = $8B5B;
     GL_FLOAT_MAT4 = $8B5C;     
     GL_SAMPLER_2D = $8B5E;     
     GL_SAMPLER_CUBE = $8B60;     
  { Vertex Arrays  }
     GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;     
     GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;     
     GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;     
     GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
     GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
     GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;     
     GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;     
  { Read Format  }
     GL_IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;     
     GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;     
  { Shader Source  }
     GL_COMPILE_STATUS = $8B81;     
     GL_INFO_LOG_LENGTH = $8B84;     
     GL_SHADER_SOURCE_LENGTH = $8B88;     
     GL_SHADER_COMPILER = $8DFA;     
  { Shader Binary  }
     GL_SHADER_BINARY_FORMATS = $8DF8;     
     GL_NUM_SHADER_BINARY_FORMATS = $8DF9;     
  { Shader Precision-Specified Types  }
     GL_LOW_FLOAT = $8DF0;     
     GL_MEDIUM_FLOAT = $8DF1;     
     GL_HIGH_FLOAT = $8DF2;     
     GL_LOW_INT = $8DF3;     
     GL_MEDIUM_INT = $8DF4;     
     GL_HIGH_INT = $8DF5;     
  { Framebuffer Object.  }
     GL_FRAMEBUFFER = $8D40;     
     GL_RENDERBUFFER = $8D41;     
     GL_RGBA4 = $8056;     
     GL_RGB5_A1 = $8057;
     GL_RGB565 = $8D62;
     GL_DEPTH_COMPONENT16 = $81A5;     
     GL_STENCIL_INDEX = $1901;     
     GL_STENCIL_INDEX8 = $8D48;     
     GL_RENDERBUFFER_WIDTH = $8D42;     
     GL_RENDERBUFFER_HEIGHT = $8D43;     
     GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;     
     GL_RENDERBUFFER_RED_SIZE = $8D50;     
     GL_RENDERBUFFER_GREEN_SIZE = $8D51;
     GL_RENDERBUFFER_BLUE_SIZE = $8D52;
     GL_RENDERBUFFER_ALPHA_SIZE = $8D53;     
     GL_RENDERBUFFER_DEPTH_SIZE = $8D54;     
     GL_RENDERBUFFER_STENCIL_SIZE = $8D55;     
     GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;     
     GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;     
     GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;     
     GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
     GL_COLOR_ATTACHMENT0 = $8CE0;     
     GL_DEPTH_ATTACHMENT = $8D00;     
     GL_STENCIL_ATTACHMENT = $8D20;     
     GL_NONE = 0;     
     GL_FRAMEBUFFER_COMPLETE = $8CD5;     
     GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;     
     GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;     
     GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;     
     GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;     
     GL_FRAMEBUFFER_BINDING = $8CA6;     
     GL_RENDERBUFFER_BINDING = $8CA7;     
     GL_MAX_RENDERBUFFER_SIZE = $84E8;     
     GL_INVALID_FRAMEBUFFER_OPERATION = $0506;     
  {-------------------------------------------------------------------------
   * GL core functions.
   *----------------------------------------------------------------------- }

{$ifdef NoDynamicLoad}
procedure glActiveTexture(texture:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glActiveTexture';
procedure glAttachShader(_program:GLuint; shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glAttachShader';
(* Const before type ignored *)
procedure glBindAttribLocation(_program:GLuint; index:GLuint; name:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBindAttribLocation';
procedure glBindBuffer(target:GLenum; buffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBindBuffer';
procedure glBindFramebuffer(target:GLenum; framebuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBindFramebuffer';
procedure glBindRenderbuffer(target:GLenum; renderbuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBindRenderbuffer';
procedure glBindTexture(target:GLenum; texture:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBindTexture';
procedure glBlendColor(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBlendColor';
procedure glBlendEquation(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBlendEquation';
procedure glBlendEquationSeparate(modeRGB:GLenum; modeAlpha:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBlendEquationSeparate';
procedure glBlendFunc(sfactor:GLenum; dfactor:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBlendFunc';
procedure glBlendFuncSeparate(srcRGB:GLenum; dstRGB:GLenum; srcAlpha:GLenum; dstAlpha:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBlendFuncSeparate';
(* Const before type ignored *)
procedure glBufferData(target:GLenum; size:GLsizeiptr; data:pointer; usage:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBufferData';
(* Const before type ignored *)
procedure glBufferSubData(target:GLenum; offset:GLintptr; size:GLsizeiptr; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBufferSubData';
function glCheckFramebufferStatus(target:GLenum):GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCheckFramebufferStatus';
procedure glClear(mask:GLbitfield);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glClear';
procedure glClearColor(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glClearColor';
procedure glClearDepthf(depth:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glClearDepthf';
procedure glClearStencil(s:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glClearStencil';
procedure glColorMask(red:GLboolean; green:GLboolean; blue:GLboolean; alpha:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glColorMask';
procedure glCompileShader(shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCompileShader';
(* Const before type ignored *)
procedure glCompressedTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;         border:GLint; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCompressedTexImage2D';
(* Const before type ignored *)
procedure glCompressedTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei;         height:GLsizei; format:GLenum; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCompressedTexSubImage2D';
procedure glCopyTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; x:GLint; y:GLint;      width:GLsizei; height:GLsizei; border:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCopyTexImage2D';
procedure glCopyTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; x:GLint;      y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCopyTexSubImage2D';
function glCreateProgram:GLuint;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCreateProgram';
function glCreateShader(_type:GLenum):GLuint;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCreateShader';
procedure glCullFace(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCullFace';
(* Const before type ignored *)
procedure glDeleteBuffers(n:GLsizei; buffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDeleteBuffers';
(* Const before type ignored *)
procedure glDeleteFramebuffers(n:GLsizei; framebuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDeleteFramebuffers';
procedure glDeleteProgram(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDeleteProgram';
(* Const before type ignored *)
procedure glDeleteRenderbuffers(n:GLsizei; renderbuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDeleteRenderbuffers';
procedure glDeleteShader(shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDeleteShader';
(* Const before type ignored *)
procedure glDeleteTextures(n:GLsizei; textures:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDeleteTextures';
procedure glDepthFunc(func:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDepthFunc';
procedure glDepthMask(flag:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDepthMask';
procedure glDepthRangef(zNear:GLclampf; zFar:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDepthRangef';
procedure glDetachShader(_program:GLuint; shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDetachShader';
procedure glDisable(cap:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDisable';
procedure glDisableVertexAttribArray(index:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDisableVertexAttribArray';
procedure glDrawArrays(mode:GLenum; first:GLint; count:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDrawArrays';
(* Const before type ignored *)
procedure glDrawElements(mode:GLenum; count:GLsizei; _type:GLenum; indices:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDrawElements';
procedure glEnable(cap:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glEnable';
procedure glEnableVertexAttribArray(index:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glEnableVertexAttribArray';
procedure glFinish;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glFinish';
procedure glFlush;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glFlush';
procedure glFramebufferRenderbuffer(target:GLenum; attachment:GLenum; renderbuffertarget:GLenum; renderbuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glFramebufferRenderbuffer';
procedure glFramebufferTexture2D(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glFramebufferTexture2D';
procedure glFrontFace(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glFrontFace';
procedure glGenBuffers(n:GLsizei; buffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGenBuffers';
procedure glGenerateMipmap(target:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGenerateMipmap';
procedure glGenFramebuffers(n:GLsizei; framebuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGenFramebuffers';
procedure glGenRenderbuffers(n:GLsizei; renderbuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGenRenderbuffers';
procedure glGenTextures(n:GLsizei; textures:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGenTextures';
procedure glGetActiveAttrib(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint;      _type:pGLenum; name:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetActiveAttrib';
procedure glGetActiveUniform(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint;       _type:pGLenum; name:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetActiveUniform';
procedure glGetAttachedShaders(_program:GLuint; maxcount:GLsizei; count:pGLsizei; shaders:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetAttachedShaders';
(* Const before type ignored *)
function glGetAttribLocation(_program:GLuint; name:pchar):longint;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetAttribLocation';
procedure glGetBooleanv(pname:GLenum; params:pGLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetBooleanv';
procedure glGetBufferParameteriv(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetBufferParameteriv';
function glGetError:GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetError';
procedure glGetFloatv(pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetFloatv';
procedure glGetFramebufferAttachmentParameteriv(target:GLenum; attachment:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetFramebufferAttachmentParameteriv';
procedure glGetIntegerv(pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetIntegerv';
procedure glGetProgramiv(_program:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetProgramiv';
procedure glGetProgramInfoLog(_program:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetProgramInfoLog';
procedure glGetRenderbufferParameteriv(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetRenderbufferParameteriv';
procedure glGetShaderiv(shader:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetShaderiv';
procedure glGetShaderInfoLog(shader:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetShaderInfoLog';
procedure glGetShaderPrecisionFormat(shadertype:GLenum; precisiontype:GLenum; range:pGLint; precision:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetShaderPrecisionFormat';
procedure glGetShaderSource(shader:GLuint; bufsize:GLsizei; length:pGLsizei; source:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetShaderSource';
(* Const before type ignored *)
function glGetString(name:GLenum):PAnsiChar;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetString';
procedure glGetTexParameterfv(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetTexParameterfv';
procedure glGetTexParameteriv(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetTexParameteriv';
procedure glGetUniformfv(_program:GLuint; location:GLint; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetUniformfv';
procedure glGetUniformiv(_program:GLuint; location:GLint; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetUniformiv';
(* Const before type ignored *)
function glGetUniformLocation(_program:GLuint; name:pchar):longint;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetUniformLocation';
procedure glGetVertexAttribfv(index:GLuint; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetVertexAttribfv';
procedure glGetVertexAttribiv(index:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetVertexAttribiv';
procedure glGetVertexAttribPointerv(index:GLuint; pname:GLenum; pointer:Ppointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetVertexAttribPointerv';
procedure glHint(target:GLenum; mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glHint';
function glIsBuffer(buffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glIsBuffer';
function glIsEnabled(cap:GLenum):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glIsEnabled';
function glIsFramebuffer(framebuffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glIsFramebuffer';
function glIsProgram(_program:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glIsProgram';
function glIsRenderbuffer(renderbuffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glIsRenderbuffer';
function glIsShader(shader:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glIsShader';
function glIsTexture(texture:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glIsTexture';
procedure glLineWidth(width:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glLineWidth';
procedure glLinkProgram(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glLinkProgram';
procedure glPixelStorei(pname:GLenum; param:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glPixelStorei';
procedure glPolygonOffset(factor:GLfloat; units:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glPolygonOffset';
procedure glReadPixels(x:GLint; y:GLint; width:GLsizei; height:GLsizei; format:GLenum;      _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glReadPixels';
procedure glReleaseShaderCompiler;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glReleaseShaderCompiler';
procedure glRenderbufferStorage(target:GLenum; internalformat:GLenum; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glRenderbufferStorage';
procedure glSampleCoverage(value:GLclampf; invert:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glSampleCoverage';
procedure glScissor(x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glScissor';
(* Const before type ignored *)
(* Const before type ignored *)
procedure glShaderBinary(n:GLsizei; shaders:pGLuint; binaryformat:GLenum; binary:pointer; length:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glShaderBinary';
(* Const before type ignored *)
(* Const before type ignored *)
procedure glShaderSource(shader:GLuint; count:GLsizei; _string:Ppchar; length:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glShaderSource';
procedure glStencilFunc(func:GLenum; ref:GLint; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glStencilFunc';
procedure glStencilFuncSeparate(face:GLenum; func:GLenum; ref:GLint; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glStencilFuncSeparate';
procedure glStencilMask(mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glStencilMask';
procedure glStencilMaskSeparate(face:GLenum; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glStencilMaskSeparate';
procedure glStencilOp(fail:GLenum; zfail:GLenum; zpass:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glStencilOp';
procedure glStencilOpSeparate(face:GLenum; fail:GLenum; zfail:GLenum; zpass:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glStencilOpSeparate';
(* Const before type ignored *)
procedure glTexImage2D(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;      border:GLint; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glTexImage2D';
procedure glTexParameterf(target:GLenum; pname:GLenum; param:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glTexParameterf';
(* Const before type ignored *)
procedure glTexParameterfv(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glTexParameterfv';
procedure glTexParameteri(target:GLenum; pname:GLenum; param:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glTexParameteri';
(* Const before type ignored *)
procedure glTexParameteriv(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glTexParameteriv';
(* Const before type ignored *)
procedure glTexSubImage2D(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei;      height:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glTexSubImage2D';
procedure glUniform1f(location:GLint; x:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform1f';
(* Const before type ignored *)
procedure glUniform1fv(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform1fv';
procedure glUniform1i(location:GLint; x:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform1i';
(* Const before type ignored *)
procedure glUniform1iv(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform1iv';
procedure glUniform2f(location:GLint; x:GLfloat; y:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform2f';
(* Const before type ignored *)
procedure glUniform2fv(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform2fv';
procedure glUniform2i(location:GLint; x:GLint; y:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform2i';
(* Const before type ignored *)
procedure glUniform2iv(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform2iv';
procedure glUniform3f(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform3f';
(* Const before type ignored *)
procedure glUniform3fv(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform3fv';
procedure glUniform3i(location:GLint; x:GLint; y:GLint; z:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform3i';
(* Const before type ignored *)
procedure glUniform3iv(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform3iv';
procedure glUniform4f(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform4f';
(* Const before type ignored *)
procedure glUniform4fv(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform4fv';
procedure glUniform4i(location:GLint; x:GLint; y:GLint; z:GLint; w:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform4i';
(* Const before type ignored *)
procedure glUniform4iv(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniform4iv';
(* Const before type ignored *)
procedure glUniformMatrix2fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniformMatrix2fv';
(* Const before type ignored *)
procedure glUniformMatrix3fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniformMatrix3fv';
(* Const before type ignored *)
procedure glUniformMatrix4fv(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUniformMatrix4fv';
procedure glUseProgram(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUseProgram';
procedure glValidateProgram(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glValidateProgram';
procedure glVertexAttrib1f(indx:GLuint; x:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttrib1f';
(* Const before type ignored *)
procedure glVertexAttrib1fv(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttrib1fv';
procedure glVertexAttrib2f(indx:GLuint; x:GLfloat; y:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttrib2f';
(* Const before type ignored *)
procedure glVertexAttrib2fv(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttrib2fv';
procedure glVertexAttrib3f(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttrib3f';
(* Const before type ignored *)
procedure glVertexAttrib3fv(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttrib3fv';
procedure glVertexAttrib4f(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttrib4f';
(* Const before type ignored *)
procedure glVertexAttrib4fv(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttrib4fv';
(* Const before type ignored *)
procedure glVertexAttribPointer(indx:GLuint; size:GLint; _type:GLenum; normalized:GLboolean; stride:GLsizei;      ptr:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glVertexAttribPointer';
procedure glViewport(x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glViewport';
{$else}
  var
    glActiveTexture : procedure(texture:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glAttachShader : procedure(_program:GLuint; shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glBindAttribLocation : procedure(_program:GLuint; index:GLuint; name:Pansichar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindBuffer : procedure(target:GLenum; buffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindFramebuffer : procedure(target:GLenum; framebuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindRenderbuffer : procedure(target:GLenum; renderbuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindTexture : procedure(target:GLenum; texture:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendColor : procedure(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendEquation : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendEquationSeparate : procedure(modeRGB:GLenum; modeAlpha:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendFunc : procedure(sfactor:GLenum; dfactor:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendFuncSeparate : procedure(srcRGB:GLenum; dstRGB:GLenum; srcAlpha:GLenum; dstAlpha:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glBufferData : procedure(target:GLenum; size:GLsizeiptr; data:pointer; usage:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glBufferSubData : procedure(target:GLenum; offset:GLintptr; size:GLsizeiptr; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCheckFramebufferStatus : function(target:GLenum):GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClear : procedure(mask:GLbitfield);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearColor : procedure(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearDepthf : procedure(depth:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearStencil : procedure(s:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glColorMask : procedure(red:GLboolean; green:GLboolean; blue:GLboolean; alpha:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCompileShader : procedure(shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glCompressedTexImage2D : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; 
      border:GLint; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glCompressedTexSubImage2D : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei; 
      height:GLsizei; format:GLenum; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCopyTexImage2D : procedure(target:GLenum; level:GLint; internalformat:GLenum; x:GLint; y:GLint; 
      width:GLsizei; height:GLsizei; border:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCopyTexSubImage2D : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; x:GLint; 
      y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCreateProgram : function:GLuint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCreateShader : function(_type:GLenum):GLuint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCullFace : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDeleteBuffers : procedure(n:GLsizei; buffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDeleteFramebuffers : procedure(n:GLsizei; framebuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteProgram : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDeleteRenderbuffers : procedure(n:GLsizei; renderbuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteShader : procedure(shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDeleteTextures : procedure(n:GLsizei; textures:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDepthFunc : procedure(func:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDepthMask : procedure(flag:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDepthRangef : procedure(zNear:GLclampf; zFar:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDetachShader : procedure(_program:GLuint; shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDisable : procedure(cap:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDisableVertexAttribArray : procedure(index:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDrawArrays : procedure(mode:GLenum; first:GLint; count:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDrawElements : procedure(mode:GLenum; count:GLsizei; _type:GLenum; indices:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEnable : procedure(cap:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEnableVertexAttribArray : procedure(index:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFinish : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFlush : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFramebufferRenderbuffer : procedure(target:GLenum; attachment:GLenum; renderbuffertarget:GLenum; renderbuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFramebufferTexture2D : procedure(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFrontFace : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenBuffers : procedure(n:GLsizei; buffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenerateMipmap : procedure(target:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenFramebuffers : procedure(n:GLsizei; framebuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenRenderbuffers : procedure(n:GLsizei; renderbuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenTextures : procedure(n:GLsizei; textures:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetActiveAttrib : procedure(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint; 
      _type:pGLenum; name:Pansichar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetActiveUniform : procedure(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint; 
      _type:pGLenum; name:Pansichar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetAttachedShaders : procedure(_program:GLuint; maxcount:GLsizei; count:pGLsizei; shaders:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetAttribLocation : function(_program:GLuint; name:Pansichar):longint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBooleanv : procedure(pname:GLenum; params:pGLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBufferParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetError : function:GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetFloatv : procedure(pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetFramebufferAttachmentParameteriv : procedure(target:GLenum; attachment:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetIntegerv : procedure(pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetProgramiv : procedure(_program:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetProgramInfoLog : procedure(_program:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:Pansichar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetRenderbufferParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderiv : procedure(shader:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderInfoLog : procedure(shader:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:Pansichar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderPrecisionFormat : procedure(shadertype:GLenum; precisiontype:GLenum; range:pGLint; precision:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderSource : procedure(shader:GLuint; bufsize:GLsizei; length:pGLsizei; source:Pansichar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetString : function(name:GLenum):Pansichar;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetTexParameterfv : procedure(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetTexParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformfv : procedure(_program:GLuint; location:GLint; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformiv : procedure(_program:GLuint; location:GLint; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetUniformLocation : function(_program:GLuint; name:Pansichar):longint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetVertexAttribfv : procedure(index:GLuint; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetVertexAttribiv : procedure(index:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetVertexAttribPointerv : procedure(index:GLuint; pname:GLenum; pointer:Ppointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glHint : procedure(target:GLenum; mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsBuffer : function(buffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsEnabled : function(cap:GLenum):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsFramebuffer : function(framebuffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsProgram : function(_program:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsRenderbuffer : function(renderbuffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsShader : function(shader:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsTexture : function(texture:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glLineWidth : procedure(width:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glLinkProgram : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glPixelStorei : procedure(pname:GLenum; param:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glPolygonOffset : procedure(factor:GLfloat; units:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glReadPixels : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei; format:GLenum; 
      _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glReleaseShaderCompiler : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glRenderbufferStorage : procedure(target:GLenum; internalformat:GLenum; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSampleCoverage : procedure(value:GLclampf; invert:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glScissor : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
(* Const before type ignored *)
    glShaderBinary : procedure(n:GLsizei; shaders:pGLuint; binaryformat:GLenum; binary:pointer; length:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
(* Const before type ignored *)
    glShaderSource : procedure(shader:GLuint; count:GLsizei; _string:Ppchar; length:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilFunc : procedure(func:GLenum; ref:GLint; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilFuncSeparate : procedure(face:GLenum; func:GLenum; ref:GLint; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilMask : procedure(mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilMaskSeparate : procedure(face:GLenum; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilOp : procedure(fail:GLenum; zfail:GLenum; zpass:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilOpSeparate : procedure(face:GLenum; fail:GLenum; zfail:GLenum; zpass:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexImage2D : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei; 
      border:GLint; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTexParameterf : procedure(target:GLenum; pname:GLenum; param:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexParameterfv : procedure(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTexParameteri : procedure(target:GLenum; pname:GLenum; param:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexSubImage2D : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei;
      height:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform1f : procedure(location:GLint; x:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform1fv : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform1i : procedure(location:GLint; x:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform1iv : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform2f : procedure(location:GLint; x:GLfloat; y:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform2fv : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform2i : procedure(location:GLint; x:GLint; y:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform2iv : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform3f : procedure(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform3fv : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform3i : procedure(location:GLint; x:GLint; y:GLint; z:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform3iv : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform4f : procedure(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform4fv : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform4i : procedure(location:GLint; x:GLint; y:GLint; z:GLint; w:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform4iv : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniformMatrix2fv : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniformMatrix3fv : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniformMatrix4fv : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUseProgram : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glValidateProgram : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttrib1f : procedure(indx:GLuint; x:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttrib1fv : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttrib2f : procedure(indx:GLuint; x:GLfloat; y:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttrib2fv : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttrib3f : procedure(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttrib3fv : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttrib4f : procedure(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttrib4fv : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttribPointer : procedure(indx:GLuint; size:GLint; _type:GLenum; normalized:GLboolean; stride:GLsizei;
      ptr:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glViewport : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}
  {------------------------------------------------------------------------*
   * IMG extension tokens
   *------------------------------------------------------------------------ }
  { GL_IMG_binary_shader  }

  const
     GL_SGX_BINARY_IMG = $8C0A;     
  { GL_IMG_texture_compression_pvrtc  }
     GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = $8C00;     
     GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = $8C01;     
     GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = $8C02;     
     GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = $8C03;     
     GL_BGRA = $80E1;     
  {------------------------------------------------------------------------*
   * IMG extension functions
   *------------------------------------------------------------------------ }
  { GL_IMG_binary_shader  }
     GL_IMG_binary_shader = 1;     
  { GL_IMG_texture_compression_pvrtc  }
     GL_IMG_texture_compression_pvrtc = 1;     
  {
   * This document is licensed under the SGI Free Software B License Version
   * 2.0. For details, see http://oss.sgi.com/projects/FreeB/ .
    }
  {------------------------------------------------------------------------*
   * OES extension tokens
   *------------------------------------------------------------------------ }
  { GL_OES_compressed_ETC1_RGB8_texture  }
     GL_ETC1_RGB8_OES = $8D64;     
  { GL_OES_compressed_paletted_texture  }
     GL_PALETTE4_RGB8_OES = $8B90;
     GL_PALETTE4_RGBA8_OES = $8B91;     
     GL_PALETTE4_R5_G6_B5_OES = $8B92;     
     GL_PALETTE4_RGBA4_OES = $8B93;     
     GL_PALETTE4_RGB5_A1_OES = $8B94;     
     GL_PALETTE8_RGB8_OES = $8B95;     
     GL_PALETTE8_RGBA8_OES = $8B96;     
     GL_PALETTE8_R5_G6_B5_OES = $8B97;     
     GL_PALETTE8_RGBA4_OES = $8B98;     
     GL_PALETTE8_RGB5_A1_OES = $8B99;     
  { GL_OES_depth24  }
     GL_DEPTH_COMPONENT24_OES = $81A6;     
  { GL_OES_depth32  }
     GL_DEPTH_COMPONENT32_OES = $81A7;     
  { GL_OES_depth_texture  }
  { No new tokens introduced by this extension.  }
  { GL_OES_EGL_image  }

  type

     GLeglImageOES = pointer;
  { GL_OES_get_program_binary  }

  const
     GL_PROGRAM_BINARY_LENGTH_OES = $8741;     
     GL_NUM_PROGRAM_BINARY_FORMATS_OES = $87FE;
     GL_PROGRAM_BINARY_FORMATS_OES = $87FF;     
  { GL_OES_mapbuffer  }
     GL_WRITE_ONLY_OES = $88B9;     
     GL_BUFFER_ACCESS_OES = $88BB;     
     GL_BUFFER_MAPPED_OES = $88BC;     
     GL_BUFFER_MAP_POINTER_OES = $88BD;
  { GL_OES_packed_depth_stencil  }
     GL_DEPTH_STENCIL_OES = $84F9;     
     GL_UNSIGNED_INT_24_8_OES = $84FA;     
     GL_DEPTH24_STENCIL8_OES = $88F0;
  { GL_OES_rgb8_rgba8  }
     GL_RGB8_OES = $8051;     
     GL_RGBA8_OES = $8058;     
  { GL_OES_standard_derivatives  }
     GL_FRAGMENT_SHADER_DERIVATIVE_HINT_OES = $8B8B;     
  { GL_OES_stencil1  }
     GL_STENCIL_INDEX1_OES = $8D46;     
  { GL_OES_stencil4  }
     GL_STENCIL_INDEX4_OES = $8D47;     
  { GL_OES_texture3D  }
     GL_TEXTURE_WRAP_R_OES = $8072;     
     GL_TEXTURE_3D_OES = $806F;     
     GL_TEXTURE_BINDING_3D_OES = $806A;     
     GL_MAX_3D_TEXTURE_SIZE_OES = $8073;     
     GL_SAMPLER_3D_OES = $8B5F;     
     GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_OES = $8CD4;     
  { GL_OES_texture_half_float  }
     GL_HALF_FLOAT_OES = $8D61;     
  { GL_OES_vertex_half_float  }
  { GL_HALF_FLOAT_OES defined in GL_OES_texture_half_float already.  }
  { GL_OES_vertex_type_10_10_10_2  }
     GL_UNSIGNED_INT_10_10_10_2_OES = $8DF6;     
     GL_INT_10_10_10_2_OES = $8DF7;     
  {------------------------------------------------------------------------*
   * AMD extension tokens
   *------------------------------------------------------------------------ }
  { GL_AMD_compressed_3DC_texture  }
     GL_3DC_X_AMD = $87F9;     
     GL_3DC_XY_AMD = $87FA;     
  { GL_AMD_compressed_ATC_texture  }
     GL_ATC_RGB_AMD = $8C92;
     GL_ATC_RGBA_EXPLICIT_ALPHA_AMD = $8C93;     
     GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD = $87EE;     
  { GL_AMD_program_binary_Z400  }
     GL_Z400_BINARY_AMD = $8740;
  { GL_AMD_performance_monitor  }
{$define GL_AMD_performance_monitor}  
     GL_COUNTER_TYPE_AMD = $8BC0;     
     GL_COUNTER_RANGE_AMD = $8BC1;     
     GL_UNSIGNED_INT64_AMD = $8BC2;     
     GL_PERCENTAGE_AMD = $8BC3;     
     GL_PERFMON_RESULT_AVAILABLE_AMD = $8BC4;     
     GL_PERFMON_RESULT_SIZE_AMD = $8BC5;     
     GL_PERFMON_RESULT_AMD = $8BC6;     
  {------------------------------------------------------------------------*
   * EXT extension tokens
   *------------------------------------------------------------------------ }
  { GL_EXT_texture_filter_anisotropic  }
     GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;     
     GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;     
  { GL_EXT_texture_type_2_10_10_10_REV  }
     GL_UNSIGNED_INT_2_10_10_10_REV_EXT = $8368;     
  {------------------------------------------------------------------------*
   * OES extension functions
   *------------------------------------------------------------------------ }
  { GL_OES_compressed_ETC1_RGB8_texture  }
     GL_OES_compressed_ETC1_RGB8_texture = 1;     
  { GL_OES_compressed_paletted_texture  }
     GL_OES_compressed_paletted_texture = 1;     
  { GL_OES_EGL_image  }

{$ifdef NoDynamicLoad}
procedure glEGLImageTargetTexture2DOES(target:GLenum; image:GLeglImageOES);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glEGLImageTargetTexture2DOES';
procedure glEGLImageTargetRenderbufferStorageOES(target:GLenum; image:GLeglImageOES);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glEGLImageTargetRenderbufferStorageOES';
{$else}
  var
    glEGLImageTargetTexture2DOES : procedure(target:GLenum; image:GLeglImageOES);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEGLImageTargetRenderbufferStorageOES : procedure(target:GLenum; image:GLeglImageOES);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}

  { GL_OES_depth24  }

  const
     GL_OES_depth24 = 1;
  { GL_OES_depth32  }
     GL_OES_depth32 = 1;
  { GL_OES_depth_texture  }
     GL_OES_depth_texture = 1;
  { GL_OES_element_index_uint  }
     GL_OES_element_index_uint = 1;
  { GL_OES_fbo_render_mipmap  }
     GL_OES_fbo_render_mipmap = 1;
  { GL_OES_fragment_precision_high  }
     GL_OES_fragment_precision_high = 1;
  { GL_OES_get_program_binary  }

{$ifdef NoDynamicLoad}
procedure glGetProgramBinaryOES(_program:GLuint; bufSize:GLsizei; length:pGLsizei; binaryFormat:pGLenum; binary:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetProgramBinaryOES';
procedure glProgramBinaryOES(_program:GLuint; binaryFormat:GLenum; binary:pointer; length:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glProgramBinaryOES';
{$else}
  var
    glGetProgramBinaryOES : procedure(_program:GLuint; bufSize:GLsizei; length:pGLsizei; binaryFormat:pGLenum; binary:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glProgramBinaryOES : procedure(_program:GLuint; binaryFormat:GLenum; binary:pointer; length:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}

(* Const before type ignored *)
  { GL_OES_mapbuffer  }

  const
     GL_OES_mapbuffer = 1;     

{$ifdef NoDynamicLoad}
function glMapBufferOES(target:GLenum; access:GLenum):pointer;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glMapBufferOES';
function glUnmapBufferOES(target:GLenum):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glUnmapBufferOES';
procedure glGetBufferPointervOES(target:GLenum; pname:GLenum; params:Ppointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetBufferPointervOES';
{$else}
  var
    glMapBufferOES : function(target:GLenum; access:GLenum):pointer;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUnmapBufferOES : function(target:GLenum):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBufferPointervOES : procedure(target:GLenum; pname:GLenum; params:Ppointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}

  type

     PFNGLMAPBUFFEROESPROC = pointer;
  { GL_OES_packed_depth_stencil  }

  const
     GL_OES_packed_depth_stencil = 1;     
  { GL_OES_rgb8_rgba8  }
     GL_OES_rgb8_rgba8 = 1;     
  { GL_OES_standard_derivatives  }
     GL_OES_standard_derivatives = 1;     
  { GL_OES_stencil1  }
     GL_OES_stencil1 = 1;     
  { GL_OES_stencil4  }
     GL_OES_stencil4 = 1;     
  { GL_OES_texture_3D  }
     GL_OES_texture_3D = 1;     
(* Const before type ignored *)

{$ifdef NoDynamicLoad}
procedure glTexImage3DOES(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;   depth:GLsizei; border:GLint; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glTexImage3DOES';
(* Const before type ignored *)
procedure glTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;   width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; _type:GLenum;      pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glTexSubImage3DOES';
procedure glCopyTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;      x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCopyTexSubImage3DOES';
(* Const before type ignored *)
procedure glCompressedTexImage3DOES(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;      depth:GLsizei; border:GLint; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCompressedTexImage3DOES';
(* Const before type ignored *)
procedure glCompressedTexSubImage3DOES(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;      width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; imageSize:GLsizei;      data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glCompressedTexSubImage3DOES';
procedure glFramebufferTexture3DOES(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint;      zoffset:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glFramebufferTexture3DOES';
(* Const before type ignored *)
{$else}
  var
    glTexImage3DOES : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
      depth:GLsizei; border:GLint; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexSubImage3DOES : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
      width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; _type:GLenum;
      pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCopyTexSubImage3DOES : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
      x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glCompressedTexImage3DOES : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
      depth:GLsizei; border:GLint; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glCompressedTexSubImage3DOES : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
      width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; imageSize:GLsizei;
      data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFramebufferTexture3DOES : procedure(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint;
      zoffset:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}
(* Const before type ignored *)

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  { GL_OES_texture_float_linear  }

  const
     GL_OES_texture_float_linear = 1;
  { GL_OES_texture_half_float_linear  }
     GL_OES_texture_half_float_linear = 1;
  { GL_OES_texture_float  }
     GL_OES_texture_float = 1;
  { GL_OES_texture_half_float  }
     GL_OES_texture_half_float = 1;
  { GL_OES_texture_npot  }
     GL_OES_texture_npot = 1;
  { GL_OES_vertex_half_float  }
     GL_OES_vertex_half_float = 1;
  { GL_OES_vertex_type_10_10_10_2  }
     GL_OES_vertex_type_10_10_10_2 = 1;
  {------------------------------------------------------------------------*
   * AMD extension functions
   *------------------------------------------------------------------------ }
  { GL_AMD_compressed_3DC_texture  }
     GL_AMD_compressed_3DC_texture = 1;
  { GL_AMD_compressed_ATC_texture  }
     GL_AMD_compressed_ATC_texture = 1;
  { GL_AMD_program_binary_Z400  }
     GL_AMD_program_binary_Z400 = 1;
  { AMD_performance_monitor  }
     GL_AMD_performance_monitor = 1;

{$ifdef NoDynamicLoad}
procedure glGetPerfMonitorGroupsAMD(numGroups:pGLint; groupsSize:GLsizei; groups:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetPerfMonitorGroupsAMD';
procedure glGetPerfMonitorCountersAMD(group:GLuint; numCounters:pGLint; maxActiveCounters:pGLint; counterSize:GLsizei; counters:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetPerfMonitorCountersAMD';
procedure glGetPerfMonitorGroupStringAMD(group:GLuint; bufSize:GLsizei; length:pGLsizei; groupString:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetPerfMonitorGroupStringAMD';
procedure glGetPerfMonitorCounterStringAMD(group:GLuint; counter:GLuint; bufSize:GLsizei; length:pGLsizei; counterString:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetPerfMonitorCounterStringAMD';
procedure glGetPerfMonitorCounterInfoAMD(group:GLuint; counter:GLuint; pname:GLenum; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetPerfMonitorCounterInfoAMD';
procedure glGenPerfMonitorsAMD(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGenPerfMonitorsAMD';
procedure glDeletePerfMonitorsAMD(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDeletePerfMonitorsAMD';
procedure glSelectPerfMonitorCountersAMD(monitor:GLuint; enable:GLboolean; group:GLuint; numCounters:GLint; countersList:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glSelectPerfMonitorCountersAMD';
procedure glBeginPerfMonitorAMD(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glBeginPerfMonitorAMD';
procedure glEndPerfMonitorAMD(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glEndPerfMonitorAMD';
procedure glGetPerfMonitorCounterDataAMD(monitor:GLuint; pname:GLenum; dataSize:GLsizei; data:pGLuint; bytesWritten:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glGetPerfMonitorCounterDataAMD';
{$else}
  var
    glGetPerfMonitorGroupsAMD : procedure(numGroups:pGLint; groupsSize:GLsizei; groups:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCountersAMD : procedure(group:GLuint; numCounters:pGLint; maxActiveCounters:pGLint; counterSize:GLsizei; counters:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorGroupStringAMD : procedure(group:GLuint; bufSize:GLsizei; length:pGLsizei; groupString:Pansichar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterStringAMD : procedure(group:GLuint; counter:GLuint; bufSize:GLsizei; length:pGLsizei; counterString:Pansichar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterInfoAMD : procedure(group:GLuint; counter:GLuint; pname:GLenum; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenPerfMonitorsAMD : procedure(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeletePerfMonitorsAMD : procedure(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSelectPerfMonitorCountersAMD : procedure(monitor:GLuint; enable:GLboolean; group:GLuint; numCounters:GLint; countersList:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBeginPerfMonitorAMD : procedure(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEndPerfMonitorAMD : procedure(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterDataAMD : procedure(monitor:GLuint; pname:GLenum; dataSize:GLsizei; data:pGLuint; bytesWritten:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}

  {------------------------------------------------------------------------*
   * EXT extension functions
   *------------------------------------------------------------------------ }
  { GL_EXT_texture_filter_anisotropic  }

  const
     GL_EXT_texture_filter_anisotropic = 1;
  { GL_EXT_texture_type_2_10_10_10_REV  }
     GL_EXT_texture_type_2_10_10_10_REV = 1;

  { GL_EXT_discard_framebuffer }
  const
     GL_COLOR_EXT = $1800;
     GL_DEPTH_EXT = $1801;
     GL_STENCIL_EXT = $1802;

{$ifdef NoDynamicLoad}
procedure glDiscardFramebufferEXT(target:GLenum; numAttachments:GLsizei; attachments:pGLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif} external GLES20LibName name 'glDiscardFramebufferEXT';
{$else}
  var
    glDiscardFramebufferEXT : procedure(target:GLenum; numAttachments:GLsizei; attachments:pGLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}

function glGetProcAddress(ahlib:{$ifdef fpc}tlibhandle{$else}THandle{$endif};ProcName:Pansichar):pointer;

implementation

function glGetProcAddress(ahlib:{$ifdef fpc}tlibhandle{$else}THandle{$endif};ProcName:Pansichar):pointer;
begin
{$ifdef fpc}
 result:=dynlibs.GetProcAddress(ahlib,ProcName);
{$else}
{$ifdef windows}
 result:=GetProcAddress(ahlib,ProcName);
{$else}
 result:=nil;
{$endif}
{$endif}
{$ifdef EGL}
 if assigned(eglGetProcAddress) and not assigned(result) then begin
  result:=pointer(eglGetProcAddress(ProcName));
 end;
{$endif}
end;

{$ifdef EGL}
{ was #define dname def_expr }
function EGL_DEFAULT_DISPLAY : EGLNativeDisplayType;
begin
 EGL_DEFAULT_DISPLAY:=EGLNativeDisplayType(0);
end;

{ was #define dname def_expr }
function EGL_NO_CONTEXT : EGLContext;
begin
 EGL_NO_CONTEXT:=EGLContext(0);
end;

{ was #define dname def_expr }
function EGL_NO_DISPLAY : EGLDisplay;
begin
 EGL_NO_DISPLAY:=EGLDisplay(0);
end;

{ was #define dname def_expr }
function EGL_NO_SURFACE : EGLSurface;
begin
 EGL_NO_SURFACE:=EGLSurface(0);
end;

{ was #define dname def_expr }
function EGL_DONT_CARE : EGLint;
begin
 EGL_DONT_CARE:=EGLint(-(1));
end;

{ was #define dname def_expr }
function EGL_UNKNOWN : EGLint;
begin
 EGL_UNKNOWN:=EGLint(-(1));
end;

{ was #define dname def_expr }
function EGL_NO_IMAGE_KHR : EGLImageKHR;
begin
 EGL_NO_IMAGE_KHR:=EGLImageKHR(0);
end;

{$ifndef NoDynamicLoad}
var EGLLib : {$ifdef fpc}tlibhandle{$else}THandle{$endif};

procedure FreeEGL;
begin
 if EGLLib<>0 then begin
  FreeLibrary(EGLLib);
 end;
 eglGetError:=nil;
 eglGetDisplay:=nil;
 eglInitialize:=nil;
 eglTerminate:=nil;
 eglQueryString:=nil;
 eglGetConfigs:=nil;
 eglChooseConfig:=nil;
 eglGetConfigAttrib:=nil;
 eglCreateWindowSurface:=nil;
 eglCreatePbufferSurface:=nil;
 eglCreatePixmapSurface:=nil;
 eglDestroySurface:=nil;
 eglQuerySurface:=nil;
 eglBindAPI:=nil;
 eglQueryAPI:=nil;
 eglWaitClient:=nil;
 eglReleaseThread:=nil;
 eglCreatePbufferFromClientBuffer:=nil;
 eglSurfaceAttrib:=nil;
 eglBindTexImage:=nil;
 eglReleaseTexImage:=nil;
 eglSwapInterval:=nil;
 eglCreateContext:=nil;
 eglDestroyContext:=nil;
 eglMakeCurrent:=nil;
 eglGetCurrentContext:=nil;
 eglGetCurrentSurface:=nil;
 eglGetCurrentDisplay:=nil;
 eglQueryContext:=nil;
 eglWaitGL:=nil;
 eglWaitNative:=nil;
 eglSwapBuffers:=nil;
 eglCopyBuffers:=nil;
 eglGetProcAddress:=nil;
end;

procedure LoadEGL(lib : Pansichar);
begin
 FreeEGL;
{$ifdef fpc}
 EGLLib:=dynlibs.LoadLibrary(lib);
{$else}
{$ifdef windows}
 EGLLib:=LoadLibrary(lib);
{$else}
 EGLLib:=0;
{$endif}
{$endif}
 if EGLLib=0 then begin
  raise Exception.Create(format('Could not load library: %s',[lib]));
 end;

 eglGetProcAddress:=GetProcAddress(EGLLib,'glGetProcAddress');

 eglGetError:=glGetProcAddress(EGLLib,'eglGetError');
 eglGetDisplay:=glGetProcAddress(EGLLib,'eglGetDisplay');
 eglInitialize:=glGetProcAddress(EGLLib,'eglInitialize');
 eglTerminate:=glGetProcAddress(EGLLib,'eglTerminate');
 eglQueryString:=glGetProcAddress(EGLLib,'eglQueryString');
 eglGetConfigs:=glGetProcAddress(EGLLib,'eglGetConfigs');
 eglChooseConfig:=glGetProcAddress(EGLLib,'eglChooseConfig');
 eglGetConfigAttrib:=glGetProcAddress(EGLLib,'eglGetConfigAttrib');
 eglCreateWindowSurface:=glGetProcAddress(EGLLib,'eglCreateWindowSurface');
 eglCreatePbufferSurface:=glGetProcAddress(EGLLib,'eglCreatePbufferSurface');
 eglCreatePixmapSurface:=glGetProcAddress(EGLLib,'eglCreatePixmapSurface');
 eglDestroySurface:=glGetProcAddress(EGLLib,'eglDestroySurface');
 eglQuerySurface:=glGetProcAddress(EGLLib,'eglQuerySurface');
 eglBindAPI:=glGetProcAddress(EGLLib,'eglBindAPI');
 eglQueryAPI:=glGetProcAddress(EGLLib,'eglQueryAPI');
 eglWaitClient:=glGetProcAddress(EGLLib,'eglWaitClient');
 eglReleaseThread:=glGetProcAddress(EGLLib,'eglReleaseThread');
 eglCreatePbufferFromClientBuffer:=glGetProcAddress(EGLLib,'eglCreatePbufferFromClientBuffer');
 eglSurfaceAttrib:=glGetProcAddress(EGLLib,'eglSurfaceAttrib');
 eglBindTexImage:=glGetProcAddress(EGLLib,'eglBindTexImage');
 eglReleaseTexImage:=glGetProcAddress(EGLLib,'eglReleaseTexImage');
 eglSwapInterval:=glGetProcAddress(EGLLib,'eglSwapInterval');
 eglCreateContext:=glGetProcAddress(EGLLib,'eglCreateContext');
 eglDestroyContext:=glGetProcAddress(EGLLib,'eglDestroyContext');
 eglMakeCurrent:=glGetProcAddress(EGLLib,'eglMakeCurrent');
 eglGetCurrentContext:=glGetProcAddress(EGLLib,'eglGetCurrentContext');
 eglGetCurrentSurface:=glGetProcAddress(EGLLib,'eglGetCurrentSurface');
 eglGetCurrentDisplay:=glGetProcAddress(EGLLib,'eglGetCurrentDisplay');
 eglQueryContext:=glGetProcAddress(EGLLib,'eglQueryContext');
 eglWaitGL:=glGetProcAddress(EGLLib,'eglWaitGL');
 eglWaitNative:=glGetProcAddress(EGLLib,'eglWaitNative');
 eglSwapBuffers:=glGetProcAddress(EGLLib,'eglSwapBuffers');
 eglCopyBuffers:=glGetProcAddress(EGLLib,'eglCopyBuffers');
end;
{$endif NoDynamicLoad}
{$endif EGL}

{$ifndef NoDynamicLoad}
var GLES20Lib : {$ifdef fpc}tlibhandle{$else}THandle{$endif};

procedure FreeGLES20;
begin
 if GLES20Lib<>0 then begin
  FreeLibrary(GLES20Lib);
 end;

 glActiveTexture:=nil;
 glAttachShader:=nil;
 glBindAttribLocation:=nil;
 glBindBuffer:=nil;
 glBindFramebuffer:=nil;
 glBindRenderbuffer:=nil;
 glBindTexture:=nil;
 glBlendColor:=nil;
 glBlendEquation:=nil;
 glBlendEquationSeparate:=nil;
 glBlendFunc:=nil;
 glBlendFuncSeparate:=nil;
 glBufferData:=nil;
 glBufferSubData:=nil;
 glCheckFramebufferStatus:=nil;
 glClear:=nil;
 glClearColor:=nil;
 glClearDepthf:=nil;
 glClearStencil:=nil;
 glColorMask:=nil;
 glCompileShader:=nil;
 glCompressedTexImage2D:=nil;
 glCompressedTexSubImage2D:=nil;
 glCopyTexImage2D:=nil;
 glCopyTexSubImage2D:=nil;
 glCreateProgram:=nil;
 glCreateShader:=nil;
 glCullFace:=nil;
 glDeleteBuffers:=nil;
 glDeleteFramebuffers:=nil;
 glDeleteProgram:=nil;
 glDeleteRenderbuffers:=nil;
 glDeleteShader:=nil;
 glDeleteTextures:=nil;
 glDepthFunc:=nil;
 glDepthMask:=nil;
 glDepthRangef:=nil;
 glDetachShader:=nil;
 glDisable:=nil;
 glDisableVertexAttribArray:=nil;
 glDrawArrays:=nil;
 glDrawElements:=nil;
 glEnable:=nil;
 glEnableVertexAttribArray:=nil;
 glFinish:=nil;
 glFlush:=nil;
 glFramebufferRenderbuffer:=nil;
 glFramebufferTexture2D:=nil;
 glFrontFace:=nil;
 glGenBuffers:=nil;
 glGenerateMipmap:=nil;
 glGenFramebuffers:=nil;
 glGenRenderbuffers:=nil;
 glGenTextures:=nil;
 glGetActiveAttrib:=nil;
 glGetActiveUniform:=nil;
 glGetAttachedShaders:=nil;
 glGetAttribLocation:=nil;
 glGetBooleanv:=nil;
 glGetBufferParameteriv:=nil;
 glGetError:=nil;
 glGetFloatv:=nil;
 glGetFramebufferAttachmentParameteriv:=nil;
 glGetIntegerv:=nil;
 glGetProgramiv:=nil;
 glGetProgramInfoLog:=nil;
 glGetRenderbufferParameteriv:=nil;
 glGetShaderiv:=nil;
 glGetShaderInfoLog:=nil;
 glGetShaderPrecisionFormat:=nil;
 glGetShaderSource:=nil;
 glGetString:=nil;
 glGetTexParameterfv:=nil;
 glGetTexParameteriv:=nil;
 glGetUniformfv:=nil;
 glGetUniformiv:=nil;
 glGetUniformLocation:=nil;
 glGetVertexAttribfv:=nil;
 glGetVertexAttribiv:=nil;
 glGetVertexAttribPointerv:=nil;
 glHint:=nil;
 glIsBuffer:=nil;
 glIsEnabled:=nil;
 glIsFramebuffer:=nil;
 glIsProgram:=nil;
 glIsRenderbuffer:=nil;
 glIsShader:=nil;
 glIsTexture:=nil;
 glLineWidth:=nil;
 glLinkProgram:=nil;
 glPixelStorei:=nil;
 glPolygonOffset:=nil;
 glReadPixels:=nil;
 glReleaseShaderCompiler:=nil;
 glRenderbufferStorage:=nil;
 glSampleCoverage:=nil;
 glScissor:=nil;
 glShaderBinary:=nil;
 glShaderSource:=nil;
 glStencilFunc:=nil;
 glStencilFuncSeparate:=nil;
 glStencilMask:=nil;
 glStencilMaskSeparate:=nil;
 glStencilOp:=nil;
 glStencilOpSeparate:=nil;
 glTexImage2D:=nil;
 glTexParameterf:=nil;
 glTexParameterfv:=nil;
 glTexParameteri:=nil;
 glTexParameteriv:=nil;
 glTexSubImage2D:=nil;
 glUniform1f:=nil;
 glUniform1fv:=nil;
 glUniform1i:=nil;
 glUniform1iv:=nil;
 glUniform2f:=nil;
 glUniform2fv:=nil;
 glUniform2i:=nil;
 glUniform2iv:=nil;
 glUniform3f:=nil;
 glUniform3fv:=nil;
 glUniform3i:=nil;
 glUniform3iv:=nil;
 glUniform4f:=nil;
 glUniform4fv:=nil;
 glUniform4i:=nil;
 glUniform4iv:=nil;
 glUniformMatrix2fv:=nil;
 glUniformMatrix3fv:=nil;
 glUniformMatrix4fv:=nil;
 glUseProgram:=nil;
 glValidateProgram:=nil;
 glVertexAttrib1f:=nil;
 glVertexAttrib1fv:=nil;
 glVertexAttrib2f:=nil;
 glVertexAttrib2fv:=nil;
 glVertexAttrib3f:=nil;
 glVertexAttrib3fv:=nil;
 glVertexAttrib4f:=nil;
 glVertexAttrib4fv:=nil;
 glVertexAttribPointer:=nil;
 glViewport:=nil;
 glEGLImageTargetTexture2DOES:=nil;
 glEGLImageTargetRenderbufferStorageOES:=nil;
 glGetProgramBinaryOES:=nil;
 glProgramBinaryOES:=nil;
 glMapBufferOES:=nil;
 glUnmapBufferOES:=nil;
 glGetBufferPointervOES:=nil;
 glTexImage3DOES:=nil;
 glTexSubImage3DOES:=nil;
 glCopyTexSubImage3DOES:=nil;
 glCompressedTexImage3DOES:=nil;
 glCompressedTexSubImage3DOES:=nil;
 glFramebufferTexture3DOES:=nil;
 glGetPerfMonitorGroupsAMD:=nil;
 glGetPerfMonitorCountersAMD:=nil;
 glGetPerfMonitorGroupStringAMD:=nil;
 glGetPerfMonitorCounterStringAMD:=nil;
 glGetPerfMonitorCounterInfoAMD:=nil;
 glGenPerfMonitorsAMD:=nil;
 glDeletePerfMonitorsAMD:=nil;
 glSelectPerfMonitorCountersAMD:=nil;
 glBeginPerfMonitorAMD:=nil;
 glEndPerfMonitorAMD:=nil;
 glGetPerfMonitorCounterDataAMD:=nil;
end;


procedure LoadGLES20(lib : Pansichar);
begin
 FreeGLES20;
{$ifdef fpc}
 GLES20Lib:=dynlibs.LoadLibrary(lib);
{$else}
{$ifdef windows}
 GLES20Lib:=LoadLibrary(lib);
{$else}
 GLES20Lib:=0;
{$endif}
{$endif}
 if GLES20Lib=0 then begin
  raise Exception.Create(format('Could not load library: %s',[lib]));
 end;

 glActiveTexture:=glGetProcAddress(GLES20Lib,'glActiveTexture');
 glAttachShader:=glGetProcAddress(GLES20Lib,'glAttachShader');
 glBindAttribLocation:=glGetProcAddress(GLES20Lib,'glBindAttribLocation');
 glBindBuffer:=glGetProcAddress(GLES20Lib,'glBindBuffer');
 glBindFramebuffer:=glGetProcAddress(GLES20Lib,'glBindFramebuffer');
 glBindRenderbuffer:=glGetProcAddress(GLES20Lib,'glBindRenderbuffer');
 glBindTexture:=glGetProcAddress(GLES20Lib,'glBindTexture');
 glBlendColor:=glGetProcAddress(GLES20Lib,'glBlendColor');
 glBlendEquation:=glGetProcAddress(GLES20Lib,'glBlendEquation');
 glBlendEquationSeparate:=glGetProcAddress(GLES20Lib,'glBlendEquationSeparate');
 glBlendFunc:=glGetProcAddress(GLES20Lib,'glBlendFunc');
 glBlendFuncSeparate:=glGetProcAddress(GLES20Lib,'glBlendFuncSeparate');
 glBufferData:=glGetProcAddress(GLES20Lib,'glBufferData');
 glBufferSubData:=glGetProcAddress(GLES20Lib,'glBufferSubData');
 glCheckFramebufferStatus:=glGetProcAddress(GLES20Lib,'glCheckFramebufferStatus');
 glClear:=glGetProcAddress(GLES20Lib,'glClear');
 glClearColor:=glGetProcAddress(GLES20Lib,'glClearColor');
 glClearDepthf:=glGetProcAddress(GLES20Lib,'glClearDepthf');
 glClearStencil:=glGetProcAddress(GLES20Lib,'glClearStencil');
 glColorMask:=glGetProcAddress(GLES20Lib,'glColorMask');
 glCompileShader:=glGetProcAddress(GLES20Lib,'glCompileShader');
 glCompressedTexImage2D:=glGetProcAddress(GLES20Lib,'glCompressedTexImage2D');
 glCompressedTexSubImage2D:=glGetProcAddress(GLES20Lib,'glCompressedTexSubImage2D');
 glCopyTexImage2D:=glGetProcAddress(GLES20Lib,'glCopyTexImage2D');
 glCopyTexSubImage2D:=glGetProcAddress(GLES20Lib,'glCopyTexSubImage2D');
 glCreateProgram:=glGetProcAddress(GLES20Lib,'glCreateProgram');
 glCreateShader:=glGetProcAddress(GLES20Lib,'glCreateShader');
 glCullFace:=glGetProcAddress(GLES20Lib,'glCullFace');
 glDeleteBuffers:=glGetProcAddress(GLES20Lib,'glDeleteBuffers');
 glDeleteFramebuffers:=glGetProcAddress(GLES20Lib,'glDeleteFramebuffers');
 glDeleteProgram:=glGetProcAddress(GLES20Lib,'glDeleteProgram');
 glDeleteRenderbuffers:=glGetProcAddress(GLES20Lib,'glDeleteRenderbuffers');
 glDeleteShader:=glGetProcAddress(GLES20Lib,'glDeleteShader');
 glDeleteTextures:=glGetProcAddress(GLES20Lib,'glDeleteTextures');
 glDepthFunc:=glGetProcAddress(GLES20Lib,'glDepthFunc');
 glDepthMask:=glGetProcAddress(GLES20Lib,'glDepthMask');
 glDepthRangef:=glGetProcAddress(GLES20Lib,'glDepthRangef');
 glDetachShader:=glGetProcAddress(GLES20Lib,'glDetachShader');
 glDisable:=glGetProcAddress(GLES20Lib,'glDisable');
 glDisableVertexAttribArray:=glGetProcAddress(GLES20Lib,'glDisableVertexAttribArray');
 glDrawArrays:=glGetProcAddress(GLES20Lib,'glDrawArrays');
 glDrawElements:=glGetProcAddress(GLES20Lib,'glDrawElements');
 glEnable:=glGetProcAddress(GLES20Lib,'glEnable');
 glEnableVertexAttribArray:=glGetProcAddress(GLES20Lib,'glEnableVertexAttribArray');
 glFinish:=glGetProcAddress(GLES20Lib,'glFinish');
 glFlush:=glGetProcAddress(GLES20Lib,'glFlush');
 glFramebufferRenderbuffer:=glGetProcAddress(GLES20Lib,'glFramebufferRenderbuffer');
 glFramebufferTexture2D:=glGetProcAddress(GLES20Lib,'glFramebufferTexture2D');
 glFrontFace:=glGetProcAddress(GLES20Lib,'glFrontFace');
 glGenBuffers:=glGetProcAddress(GLES20Lib,'glGenBuffers');
 glGenerateMipmap:=glGetProcAddress(GLES20Lib,'glGenerateMipmap');
 glGenFramebuffers:=glGetProcAddress(GLES20Lib,'glGenFramebuffers');
 glGenRenderbuffers:=glGetProcAddress(GLES20Lib,'glGenRenderbuffers');
 glGenTextures:=glGetProcAddress(GLES20Lib,'glGenTextures');
 glGetActiveAttrib:=glGetProcAddress(GLES20Lib,'glGetActiveAttrib');
 glGetActiveUniform:=glGetProcAddress(GLES20Lib,'glGetActiveUniform');
 glGetAttachedShaders:=glGetProcAddress(GLES20Lib,'glGetAttachedShaders');
 glGetAttribLocation:=glGetProcAddress(GLES20Lib,'glGetAttribLocation');
 glGetBooleanv:=glGetProcAddress(GLES20Lib,'glGetBooleanv');
 glGetBufferParameteriv:=glGetProcAddress(GLES20Lib,'glGetBufferParameteriv');
 glGetError:=glGetProcAddress(GLES20Lib,'glGetError');
 glGetFloatv:=glGetProcAddress(GLES20Lib,'glGetFloatv');
 glGetFramebufferAttachmentParameteriv:=glGetProcAddress(GLES20Lib,'glGetFramebufferAttachmentParameteriv');
 glGetIntegerv:=glGetProcAddress(GLES20Lib,'glGetIntegerv');
 glGetProgramiv:=glGetProcAddress(GLES20Lib,'glGetProgramiv');
 glGetProgramInfoLog:=glGetProcAddress(GLES20Lib,'glGetProgramInfoLog');
 glGetRenderbufferParameteriv:=glGetProcAddress(GLES20Lib,'glGetRenderbufferParameteriv');
 glGetShaderiv:=glGetProcAddress(GLES20Lib,'glGetShaderiv');
 glGetShaderInfoLog:=glGetProcAddress(GLES20Lib,'glGetShaderInfoLog');
 glGetShaderPrecisionFormat:=glGetProcAddress(GLES20Lib,'glGetShaderPrecisionFormat');
 glGetShaderSource:=glGetProcAddress(GLES20Lib,'glGetShaderSource');
 glGetString:=glGetProcAddress(GLES20Lib,'glGetString');
 glGetTexParameterfv:=glGetProcAddress(GLES20Lib,'glGetTexParameterfv');
 glGetTexParameteriv:=glGetProcAddress(GLES20Lib,'glGetTexParameteriv');
 glGetUniformfv:=glGetProcAddress(GLES20Lib,'glGetUniformfv');
 glGetUniformiv:=glGetProcAddress(GLES20Lib,'glGetUniformiv');
 glGetUniformLocation:=glGetProcAddress(GLES20Lib,'glGetUniformLocation');
 glGetVertexAttribfv:=glGetProcAddress(GLES20Lib,'glGetVertexAttribfv');
 glGetVertexAttribiv:=glGetProcAddress(GLES20Lib,'glGetVertexAttribiv');
 glGetVertexAttribPointerv:=glGetProcAddress(GLES20Lib,'glGetVertexAttribPointerv');
 glHint:=glGetProcAddress(GLES20Lib,'glHint');
 glIsBuffer:=glGetProcAddress(GLES20Lib,'glIsBuffer');
 glIsEnabled:=glGetProcAddress(GLES20Lib,'glIsEnabled');
 glIsFramebuffer:=glGetProcAddress(GLES20Lib,'glIsFramebuffer');
 glIsProgram:=glGetProcAddress(GLES20Lib,'glIsProgram');
 glIsRenderbuffer:=glGetProcAddress(GLES20Lib,'glIsRenderbuffer');
 glIsShader:=glGetProcAddress(GLES20Lib,'glIsShader');
 glIsTexture:=glGetProcAddress(GLES20Lib,'glIsTexture');
 glLineWidth:=glGetProcAddress(GLES20Lib,'glLineWidth');
 glLinkProgram:=glGetProcAddress(GLES20Lib,'glLinkProgram');
 glPixelStorei:=glGetProcAddress(GLES20Lib,'glPixelStorei');
 glPolygonOffset:=glGetProcAddress(GLES20Lib,'glPolygonOffset');
 glReadPixels:=glGetProcAddress(GLES20Lib,'glReadPixels');
 glReleaseShaderCompiler:=glGetProcAddress(GLES20Lib,'glReleaseShaderCompiler');
 glRenderbufferStorage:=glGetProcAddress(GLES20Lib,'glRenderbufferStorage');
 glSampleCoverage:=glGetProcAddress(GLES20Lib,'glSampleCoverage');
 glScissor:=glGetProcAddress(GLES20Lib,'glScissor');
 glShaderBinary:=glGetProcAddress(GLES20Lib,'glShaderBinary');
 glShaderSource:=glGetProcAddress(GLES20Lib,'glShaderSource');
 glStencilFunc:=glGetProcAddress(GLES20Lib,'glStencilFunc');
 glStencilFuncSeparate:=glGetProcAddress(GLES20Lib,'glStencilFuncSeparate');
 glStencilMask:=glGetProcAddress(GLES20Lib,'glStencilMask');
 glStencilMaskSeparate:=glGetProcAddress(GLES20Lib,'glStencilMaskSeparate');
 glStencilOp:=glGetProcAddress(GLES20Lib,'glStencilOp');
 glStencilOpSeparate:=glGetProcAddress(GLES20Lib,'glStencilOpSeparate');
 glTexImage2D:=glGetProcAddress(GLES20Lib,'glTexImage2D');
 glTexParameterf:=glGetProcAddress(GLES20Lib,'glTexParameterf');
 glTexParameterfv:=glGetProcAddress(GLES20Lib,'glTexParameterfv');
 glTexParameteri:=glGetProcAddress(GLES20Lib,'glTexParameteri');
 glTexParameteriv:=glGetProcAddress(GLES20Lib,'glTexParameteriv');
 glTexSubImage2D:=glGetProcAddress(GLES20Lib,'glTexSubImage2D');
 glUniform1f:=glGetProcAddress(GLES20Lib,'glUniform1f');
 glUniform1fv:=glGetProcAddress(GLES20Lib,'glUniform1fv');
 glUniform1i:=glGetProcAddress(GLES20Lib,'glUniform1i');
 glUniform1iv:=glGetProcAddress(GLES20Lib,'glUniform1iv');
 glUniform2f:=glGetProcAddress(GLES20Lib,'glUniform2f');
 glUniform2fv:=glGetProcAddress(GLES20Lib,'glUniform2fv');
 glUniform2i:=glGetProcAddress(GLES20Lib,'glUniform2i');
 glUniform2iv:=glGetProcAddress(GLES20Lib,'glUniform2iv');
 glUniform3f:=glGetProcAddress(GLES20Lib,'glUniform3f');
 glUniform3fv:=glGetProcAddress(GLES20Lib,'glUniform3fv');
 glUniform3i:=glGetProcAddress(GLES20Lib,'glUniform3i');
 glUniform3iv:=glGetProcAddress(GLES20Lib,'glUniform3iv');
 glUniform4f:=glGetProcAddress(GLES20Lib,'glUniform4f');
 glUniform4fv:=glGetProcAddress(GLES20Lib,'glUniform4fv');
 glUniform4i:=glGetProcAddress(GLES20Lib,'glUniform4i');
 glUniform4iv:=glGetProcAddress(GLES20Lib,'glUniform4iv');
 glUniformMatrix2fv:=glGetProcAddress(GLES20Lib,'glUniformMatrix2fv');
 glUniformMatrix3fv:=glGetProcAddress(GLES20Lib,'glUniformMatrix3fv');
 glUniformMatrix4fv:=glGetProcAddress(GLES20Lib,'glUniformMatrix4fv');
 glUseProgram:=glGetProcAddress(GLES20Lib,'glUseProgram');
 glValidateProgram:=glGetProcAddress(GLES20Lib,'glValidateProgram');
 glVertexAttrib1f:=glGetProcAddress(GLES20Lib,'glVertexAttrib1f');
 glVertexAttrib1fv:=glGetProcAddress(GLES20Lib,'glVertexAttrib1fv');
 glVertexAttrib2f:=glGetProcAddress(GLES20Lib,'glVertexAttrib2f');
 glVertexAttrib2fv:=glGetProcAddress(GLES20Lib,'glVertexAttrib2fv');
 glVertexAttrib3f:=glGetProcAddress(GLES20Lib,'glVertexAttrib3f');
 glVertexAttrib3fv:=glGetProcAddress(GLES20Lib,'glVertexAttrib3fv');
 glVertexAttrib4f:=glGetProcAddress(GLES20Lib,'glVertexAttrib4f');
 glVertexAttrib4fv:=glGetProcAddress(GLES20Lib,'glVertexAttrib4fv');
 glVertexAttribPointer:=glGetProcAddress(GLES20Lib,'glVertexAttribPointer');
 glViewport:=glGetProcAddress(GLES20Lib,'glViewport');
 glEGLImageTargetTexture2DOES:=glGetProcAddress(GLES20Lib,'glEGLImageTargetTexture2DOES');
 glEGLImageTargetRenderbufferStorageOES:=glGetProcAddress(GLES20Lib,'glEGLImageTargetRenderbufferStorageOES');
 glGetProgramBinaryOES:=glGetProcAddress(GLES20Lib,'glGetProgramBinaryOES');
 glProgramBinaryOES:=glGetProcAddress(GLES20Lib,'glProgramBinaryOES');
 glMapBufferOES:=glGetProcAddress(GLES20Lib,'glMapBufferOES');
 glUnmapBufferOES:=glGetProcAddress(GLES20Lib,'glUnmapBufferOES');
 glGetBufferPointervOES:=glGetProcAddress(GLES20Lib,'glGetBufferPointervOES');
 glTexImage3DOES:=glGetProcAddress(GLES20Lib,'glTexImage3DOES');
 glTexSubImage3DOES:=glGetProcAddress(GLES20Lib,'glTexSubImage3DOES');
 glCopyTexSubImage3DOES:=glGetProcAddress(GLES20Lib,'glCopyTexSubImage3DOES');
 glCompressedTexImage3DOES:=glGetProcAddress(GLES20Lib,'glCompressedTexImage3DOES');
 glCompressedTexSubImage3DOES:=glGetProcAddress(GLES20Lib,'glCompressedTexSubImage3DOES');
 glFramebufferTexture3DOES:=glGetProcAddress(GLES20Lib,'glFramebufferTexture3DOES');
 glGetPerfMonitorGroupsAMD:=glGetProcAddress(GLES20Lib,'glGetPerfMonitorGroupsAMD');
 glGetPerfMonitorCountersAMD:=glGetProcAddress(GLES20Lib,'glGetPerfMonitorCountersAMD');
 glGetPerfMonitorGroupStringAMD:=glGetProcAddress(GLES20Lib,'glGetPerfMonitorGroupStringAMD');
 glGetPerfMonitorCounterStringAMD:=glGetProcAddress(GLES20Lib,'glGetPerfMonitorCounterStringAMD');
 glGetPerfMonitorCounterInfoAMD:=glGetProcAddress(GLES20Lib,'glGetPerfMonitorCounterInfoAMD');
 glGenPerfMonitorsAMD:=glGetProcAddress(GLES20Lib,'glGenPerfMonitorsAMD');
 glDeletePerfMonitorsAMD:=glGetProcAddress(GLES20Lib,'glDeletePerfMonitorsAMD');
 glSelectPerfMonitorCountersAMD:=glGetProcAddress(GLES20Lib,'glSelectPerfMonitorCountersAMD');
 glBeginPerfMonitorAMD:=glGetProcAddress(GLES20Lib,'glBeginPerfMonitorAMD');
 glEndPerfMonitorAMD:=glGetProcAddress(GLES20Lib,'glEndPerfMonitorAMD');
 glGetPerfMonitorCounterDataAMD:=glGetProcAddress(GLES20Lib,'glGetPerfMonitorCounterDataAMD');
 glDiscardFramebufferEXT:=glGetProcAddress(GLES20Lib,'glDiscardFramebufferEXT');
end;
{$endif NoDynamicLoad}

initialization
{$ifndef NoDynamicLoad}
{$ifdef EGL}
 EGLLib:=0;
 LoadEGL(EGLLibName);
{$endif}
 GLES20Lib:=0;
 LoadGLES20(GLES20LibName);
{$endif}
finalization
{$ifndef NoDynamicLoad}
 FreeGLES20;
{$ifdef EGL}
 FreeEGL;
{$endif}
{$endif}
{$else}
(*
** License Applicability. Except to the extent portions of this file are
** made subject to an alternative license as permitted in the SGI Free
** Software License B, Version 1.1 (the "License"), the contents of this
** file are subject only to the provisions of the License. You may not use
** this file except in compliance with the License. You may obtain a copy
** of the License at Silicon Graphics, Inc., attn: Legal Services, 1600
** Amphitheatre Parkway, Mountain View, CA 94043-1351, or at:
** 
** http://oss.sgi.com/projects/FreeB
** 
** Note that, as provided in the License, the Software is distributed on an
** "AS IS" basis, with ALL EXPRESS AND IMPLIED WARRANTIES AND CONDITIONS
** DISCLAIMED, INCLUDING, WITHOUT LIMITATION, ANY IMPLIED WARRANTIES AND
** CONDITIONS OF MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A
** PARTICULAR PURPOSE, AND NON-INFRINGEMENT.
**
** Original Code. The Original Code is: OpenGL Sample Implementation,
** Version 1.2.1, released January 26, 2000, developed by Silicon Graphics,
** Inc. The Original Code is Copyright (c) 1991-2000 Silicon Graphics, Inc.
** Copyright in any portions created by third parties is as indicated
** elsewhere herein. All Rights Reserved.
** 
** Additional Notice Provisions: This software was created using the
** OpenGL(R) version 1.2.1 Sample Implementation published by SGI, but has
** not been independently verified as being compliant with the OpenGL(R)
** version 1.2.1 Specification.
**
** (this unit actually only contains the 1.1 parts of the specification,
**  the parts from the subsequence versions are in glext.pp)
*)

{******************************************************************************}
{ Converted to Delphi by Tom Nuydens (tom@delphi3d.net)                        }
{******************************************************************************}
{$ifdef fpc}
{$MACRO ON}
{$IFDEF Windows}
  {$DEFINE extdecl := stdcall}
{$ELSE}
  {$DEFINE extdecl := cdecl}
  {$IFDEF MorphOS}
    {$INLINE ON}
    {$DEFINE GL_UNIT}
  {$ELSE}
   {$IFNDEF OS2}
    {$LINKLIB c}
   {$ENDIF OS2}
  {$ENDIF}
{$ENDIF}
{$ELSE}
{$DEFINE windows}
{$ENDIF}
{$j+}

interface

uses
  SysUtils,
  {$IFDEF Windows}
  Windows{$ifdef fpc}, dynlibs{$endif}
  {$ELSE Windows}
  {$IFDEF MorphOS}
  TinyGL
  {$ELSE MorphOS}
  dynlibs
  {$ENDIF MorphOS}
  {$ENDIF Windows};

{$ifdef fpc}
type GLptruint=ptruint;
     GLptrint=ptrint;
{$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
type GLptruint=NativeUInt;
     GLptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
{$ifdef cpu64}
type GLptruint=qword;
     GLptrint=int64;
{$else}
type GLptruint=longword;
     GLptrint=longint;
{$endif}
{$endif}

{$IFNDEF MORPHOS}
var
  LibGL: {$ifdef fpc}TLibHandle{$else}GLPtrUInt{$endif};
{$ENDIF MORPHOS}

type
  GLenum     = Cardinal;      PGLenum     = ^GLenum;
  GLboolean  = Byte;          PGLboolean  = ^GLboolean;
  GLbitfield = Cardinal;      PGLbitfield = ^GLbitfield;
  GLbyte     = ShortInt;      PGLbyte     = ^GLbyte;
  GLshort    = SmallInt;      PGLshort    = ^GLshort;
  GLint      = Integer;       PGLint      = ^GLint;
  GLsizei    = Integer;       PGLsizei    = ^GLsizei;
  GLubyte    = Byte;          PGLubyte    = ^GLubyte;
  GLushort   = Word;          PGLushort   = ^GLushort;
  GLuint     = Cardinal;      PGLuint     = ^GLuint;
  GLfloat    = Single;        PGLfloat    = ^GLfloat;
  GLclampf   = Single;        PGLclampf   = ^GLclampf;
  GLdouble   = Double;        PGLdouble   = ^GLdouble;
  GLclampd   = Double;        PGLclampd   = ^GLclampd;
{ GLvoid     = void; }        PGLvoid     = Pointer;
                              PPGLvoid    = ^PGLvoid;

  TGLenum     = GLenum;
  TGLboolean  = GLboolean;
  TGLbitfield = GLbitfield;
  TGLbyte     = GLbyte;
  TGLshort    = GLshort;
  TGLint      = GLint;
  TGLsizei    = GLsizei;
  TGLubyte    = GLubyte;
  TGLushort   = GLushort;
  TGLuint     = GLuint;
  TGLfloat    = GLfloat;
  TGLclampf   = GLclampf;
  TGLdouble   = GLdouble;
  TGLclampd   = GLclampd;

{******************************************************************************}

const
  // Version
  GL_VERSION_1_1                    = 1;

  // AccumOp
  GL_ACCUM                          = $0100;
  GL_LOAD                           = $0101;
  GL_RETURN                         = $0102;
  GL_MULT                           = $0103;
  GL_ADD                            = $0104;

  // AlphaFunction
  GL_NEVER                          = $0200;
  GL_LESS                           = $0201;
  GL_EQUAL                          = $0202;
  GL_LEQUAL                         = $0203;
  GL_GREATER                        = $0204;
  GL_NOTEQUAL                       = $0205;
  GL_GEQUAL                         = $0206;
  GL_ALWAYS                         = $0207;

  // AttribMask
  GL_CURRENT_BIT                    = $00000001;
  GL_POINT_BIT                      = $00000002;
  GL_LINE_BIT                       = $00000004;
  GL_POLYGON_BIT                    = $00000008;
  GL_POLYGON_STIPPLE_BIT            = $00000010;
  GL_PIXEL_MODE_BIT                 = $00000020;
  GL_LIGHTING_BIT                   = $00000040;
  GL_FOG_BIT                        = $00000080;
  GL_DEPTH_BUFFER_BIT               = $00000100;
  GL_ACCUM_BUFFER_BIT               = $00000200;
  GL_STENCIL_BUFFER_BIT             = $00000400;
  GL_VIEWPORT_BIT                   = $00000800;
  GL_TRANSFORM_BIT                  = $00001000;
  GL_ENABLE_BIT                     = $00002000;
  GL_COLOR_BUFFER_BIT               = $00004000;
  GL_HINT_BIT                       = $00008000;
  GL_EVAL_BIT                       = $00010000;
  GL_LIST_BIT                       = $00020000;
  GL_TEXTURE_BIT                    = $00040000;
  GL_SCISSOR_BIT                    = $00080000;
  GL_ALL_ATTRIB_BITS                = $000FFFFF;

  // BeginMode
  GL_POINTS                         = $0000;
  GL_LINES                          = $0001;
  GL_LINE_LOOP                      = $0002;
  GL_LINE_STRIP                     = $0003;
  GL_TRIANGLES                      = $0004;
  GL_TRIANGLE_STRIP                 = $0005;
  GL_TRIANGLE_FAN                   = $0006;
  GL_QUADS                          = $0007;
  GL_QUAD_STRIP                     = $0008;
  GL_POLYGON                        = $0009;

  // BlendingFactorDest
  GL_ZERO                           = $0000;
  GL_ONE                            = $0001;
  GL_SRC_COLOR                      = $0300;
  GL_ONE_MINUS_SRC_COLOR            = $0301;
  GL_SRC_ALPHA                      = $0302;
  GL_ONE_MINUS_SRC_ALPHA            = $0303;
  GL_DST_ALPHA                      = $0304;
  GL_ONE_MINUS_DST_ALPHA            = $0305;

  // BlendingFactorSrc
  //      GL_ZERO
  //      GL_ONE
  GL_DST_COLOR                      = $0306;
  GL_ONE_MINUS_DST_COLOR            = $0307;
  GL_SRC_ALPHA_SATURATE             = $0308;
  //      GL_SRC_ALPHA
  //      GL_ONE_MINUS_SRC_ALPHA
  //      GL_DST_ALPHA
  //      GL_ONE_MINUS_DST_ALPHA

  // Boolean
  GL_TRUE                           = 1;
  GL_FALSE                          = 0;

  // ClearBufferMask
  //      GL_COLOR_BUFFER_BIT
  //      GL_ACCUM_BUFFER_BIT
  //      GL_STENCIL_BUFFER_BIT
  //      GL_DEPTH_BUFFER_BIT

  // ClientArrayType
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY

  // ClipPlaneName
  GL_CLIP_PLANE0                    = $3000;
  GL_CLIP_PLANE1                    = $3001;
  GL_CLIP_PLANE2                    = $3002;
  GL_CLIP_PLANE3                    = $3003;
  GL_CLIP_PLANE4                    = $3004;
  GL_CLIP_PLANE5                    = $3005;

  // ColorMaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // ColorMaterialParameter
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR
  //      GL_EMISSION
  //      GL_AMBIENT_AND_DIFFUSE

  // ColorPointerType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // CullFaceMode
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // DataType
  GL_BYTE                           = $1400;
  GL_UNSIGNED_BYTE                  = $1401;
  GL_SHORT                          = $1402;
  GL_UNSIGNED_SHORT                 = $1403;
  GL_INT                            = $1404;
  GL_UNSIGNED_INT                   = $1405;
  GL_FLOAT                          = $1406;
  GL_2_BYTES                        = $1407;
  GL_3_BYTES                        = $1408;
  GL_4_BYTES                        = $1409;
  GL_DOUBLE                         = $140A;

  // DepthFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // DrawBufferMode
  GL_NONE                           = 0;
  GL_FRONT_LEFT                     = $0400;
  GL_FRONT_RIGHT                    = $0401;
  GL_BACK_LEFT                      = $0402;
  GL_BACK_RIGHT                     = $0403;
  GL_FRONT                          = $0404;
  GL_BACK                           = $0405;
  GL_LEFT                           = $0406;
  GL_RIGHT                          = $0407;
  GL_FRONT_AND_BACK                 = $0408;
  GL_AUX0                           = $0409;
  GL_AUX1                           = $040A;
  GL_AUX2                           = $040B;
  GL_AUX3                           = $040C;

  // Enable
  //      GL_FOG
  //      GL_LIGHTING
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_LINE_STIPPLE
  //      GL_POLYGON_STIPPLE
  //      GL_CULL_FACE
  //      GL_ALPHA_TEST
  //      GL_BLEND
  //      GL_INDEX_LOGIC_OP
  //      GL_COLOR_LOGIC_OP
  //      GL_DITHER
  //      GL_STENCIL_TEST
  //      GL_DEPTH_TEST
  //      GL_CLIP_PLANE0
  //      GL_CLIP_PLANE1
  //      GL_CLIP_PLANE2
  //      GL_CLIP_PLANE3
  //      GL_CLIP_PLANE4
  //      GL_CLIP_PLANE5
  //      GL_LIGHT0
  //      GL_LIGHT1
  //      GL_LIGHT2
  //      GL_LIGHT3
  //      GL_LIGHT4
  //      GL_LIGHT5
  //      GL_LIGHT6
  //      GL_LIGHT7
  //      GL_TEXTURE_GEN_S
  //      GL_TEXTURE_GEN_T
  //      GL_TEXTURE_GEN_R
  //      GL_TEXTURE_GEN_Q
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_POINT_SMOOTH
  //      GL_LINE_SMOOTH
  //      GL_POLYGON_SMOOTH
  //      GL_SCISSOR_TEST
  //      GL_COLOR_MATERIAL
  //      GL_NORMALIZE
  //      GL_AUTO_NORMAL
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_POLYGON_OFFSET_POINT
  //      GL_POLYGON_OFFSET_LINE
  //      GL_POLYGON_OFFSET_FILL

  // ErrorCode
  GL_NO_ERROR                       = 0;
  GL_INVALID_ENUM                   = $0500;
  GL_INVALID_VALUE                  = $0501;
  GL_INVALID_OPERATION              = $0502;
  GL_STACK_OVERFLOW                 = $0503;
  GL_STACK_UNDERFLOW                = $0504;
  GL_OUT_OF_MEMORY                  = $0505;

  // FeedBackMode
  GL_2D                             = $0600;
  GL_3D                             = $0601;
  GL_3D_COLOR                       = $0602;
  GL_3D_COLOR_TEXTURE               = $0603;
  GL_4D_COLOR_TEXTURE               = $0604;

  // FeedBackToken
  GL_PASS_THROUGH_TOKEN             = $0700;
  GL_POINT_TOKEN                    = $0701;
  GL_LINE_TOKEN                     = $0702;
  GL_POLYGON_TOKEN                  = $0703;
  GL_BITMAP_TOKEN                   = $0704;
  GL_DRAW_PIXEL_TOKEN               = $0705;
  GL_COPY_PIXEL_TOKEN               = $0706;
  GL_LINE_RESET_TOKEN               = $0707;

  // FogMode
  //      GL_LINEAR
  GL_EXP                            = $0800;
  GL_EXP2                           = $0801;

  // FogParameter
  //      GL_FOG_COLOR
  //      GL_FOG_DENSITY
  //      GL_FOG_END
  //      GL_FOG_INDEX
  //      GL_FOG_MODE
  //      GL_FOG_START

  // FrontFaceDirection
  GL_CW                             = $0900;
  GL_CCW                            = $0901;

  // GetMapTarget
  GL_COEFF                          = $0A00;
  GL_ORDER                          = $0A01;
  GL_DOMAIN                         = $0A02;

  // GetPixelMap
  //      GL_PIXEL_MAP_I_TO_I
  //      GL_PIXEL_MAP_S_TO_S
  //      GL_PIXEL_MAP_I_TO_R
  //      GL_PIXEL_MAP_I_TO_G
  //      GL_PIXEL_MAP_I_TO_B
  //      GL_PIXEL_MAP_I_TO_A
  //      GL_PIXEL_MAP_R_TO_R
  //      GL_PIXEL_MAP_G_TO_G
  //      GL_PIXEL_MAP_B_TO_B
  //      GL_PIXEL_MAP_A_TO_A

  // GetPointerTarget
  //      GL_VERTEX_ARRAY_POINTER
  //      GL_NORMAL_ARRAY_POINTER
  //      GL_COLOR_ARRAY_POINTER
  //      GL_INDEX_ARRAY_POINTER
  //      GL_TEXTURE_COORD_ARRAY_POINTER
  //      GL_EDGE_FLAG_ARRAY_POINTER

  // GetTarget
  GL_CURRENT_COLOR                  = $0B00;
  GL_CURRENT_INDEX                  = $0B01;
  GL_CURRENT_NORMAL                 = $0B02;
  GL_CURRENT_TEXTURE_COORDS         = $0B03;
  GL_CURRENT_RASTER_COLOR           = $0B04;
  GL_CURRENT_RASTER_INDEX           = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS  = $0B06;
  GL_CURRENT_RASTER_POSITION        = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID  = $0B08;
  GL_CURRENT_RASTER_DISTANCE        = $0B09;
  GL_POINT_SMOOTH                   = $0B10;
  GL_POINT_SIZE                     = $0B11;
  GL_POINT_SIZE_RANGE               = $0B12;
  GL_POINT_SIZE_GRANULARITY         = $0B13;
  GL_LINE_SMOOTH                    = $0B20;
  GL_LINE_WIDTH                     = $0B21;
  GL_LINE_WIDTH_RANGE               = $0B22;
  GL_LINE_WIDTH_GRANULARITY         = $0B23;
  GL_LINE_STIPPLE                   = $0B24;
  GL_LINE_STIPPLE_PATTERN           = $0B25;
  GL_LINE_STIPPLE_REPEAT            = $0B26;
  GL_LIST_MODE                      = $0B30;
  GL_MAX_LIST_NESTING               = $0B31;
  GL_LIST_BASE                      = $0B32;
  GL_LIST_INDEX                     = $0B33;
  GL_POLYGON_MODE                   = $0B40;
  GL_POLYGON_SMOOTH                 = $0B41;
  GL_POLYGON_STIPPLE                = $0B42;
  GL_EDGE_FLAG                      = $0B43;
  GL_CULL_FACE                      = $0B44;
  GL_CULL_FACE_MODE                 = $0B45;
  GL_FRONT_FACE                     = $0B46;
  GL_LIGHTING                       = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER       = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE           = $0B52;
  GL_LIGHT_MODEL_AMBIENT            = $0B53;
  GL_SHADE_MODEL                    = $0B54;
  GL_COLOR_MATERIAL_FACE            = $0B55;
  GL_COLOR_MATERIAL_PARAMETER       = $0B56;
  GL_COLOR_MATERIAL                 = $0B57;
  GL_FOG                            = $0B60;
  GL_FOG_INDEX                      = $0B61;
  GL_FOG_DENSITY                    = $0B62;
  GL_FOG_START                      = $0B63;
  GL_FOG_END                        = $0B64;
  GL_FOG_MODE                       = $0B65;
  GL_FOG_COLOR                      = $0B66;
  GL_DEPTH_RANGE                    = $0B70;
  GL_DEPTH_TEST                     = $0B71;
  GL_DEPTH_WRITEMASK                = $0B72;
  GL_DEPTH_CLEAR_VALUE              = $0B73;
  GL_DEPTH_FUNC                     = $0B74;
  GL_ACCUM_CLEAR_VALUE              = $0B80;
  GL_STENCIL_TEST                   = $0B90;
  GL_STENCIL_CLEAR_VALUE            = $0B91;
  GL_STENCIL_FUNC                   = $0B92;
  GL_STENCIL_VALUE_MASK             = $0B93;
  GL_STENCIL_FAIL                   = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL        = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS        = $0B96;
  GL_STENCIL_REF                    = $0B97;
  GL_STENCIL_WRITEMASK              = $0B98;
  GL_MATRIX_MODE                    = $0BA0;
  GL_NORMALIZE                      = $0BA1;
  GL_VIEWPORT                       = $0BA2;
  GL_MODELVIEW_STACK_DEPTH          = $0BA3;
  GL_PROJECTION_STACK_DEPTH         = $0BA4;
  GL_TEXTURE_STACK_DEPTH            = $0BA5;
  GL_MODELVIEW_MATRIX               = $0BA6;
  GL_PROJECTION_MATRIX              = $0BA7;
  GL_TEXTURE_MATRIX                 = $0BA8;
  GL_ATTRIB_STACK_DEPTH             = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH      = $0BB1;
  GL_ALPHA_TEST                     = $0BC0;
  GL_ALPHA_TEST_FUNC                = $0BC1;
  GL_ALPHA_TEST_REF                 = $0BC2;
  GL_DITHER                         = $0BD0;
  GL_BLEND_DST                      = $0BE0;
  GL_BLEND_SRC                      = $0BE1;
  GL_BLEND                          = $0BE2;
  GL_LOGIC_OP_MODE                  = $0BF0;
  GL_INDEX_LOGIC_OP                 = $0BF1;
  GL_COLOR_LOGIC_OP                 = $0BF2;
  GL_AUX_BUFFERS                    = $0C00;
  GL_DRAW_BUFFER                    = $0C01;
  GL_READ_BUFFER                    = $0C02;
  GL_SCISSOR_BOX                    = $0C10;
  GL_SCISSOR_TEST                   = $0C11;
  GL_INDEX_CLEAR_VALUE              = $0C20;
  GL_INDEX_WRITEMASK                = $0C21;
  GL_COLOR_CLEAR_VALUE              = $0C22;
  GL_COLOR_WRITEMASK                = $0C23;
  GL_INDEX_MODE                     = $0C30;
  GL_RGBA_MODE                      = $0C31;
  GL_DOUBLEBUFFER                   = $0C32;
  GL_STEREO                         = $0C33;
  GL_RENDER_MODE                    = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT    = $0C50;
  GL_POINT_SMOOTH_HINT              = $0C51;
  GL_LINE_SMOOTH_HINT               = $0C52;
  GL_POLYGON_SMOOTH_HINT            = $0C53;
  GL_FOG_HINT                       = $0C54;
  GL_TEXTURE_GEN_S                  = $0C60;
  GL_TEXTURE_GEN_T                  = $0C61;
  GL_TEXTURE_GEN_R                  = $0C62;
  GL_TEXTURE_GEN_Q                  = $0C63;
  GL_PIXEL_MAP_I_TO_I               = $0C70;
  GL_PIXEL_MAP_S_TO_S               = $0C71;
  GL_PIXEL_MAP_I_TO_R               = $0C72;
  GL_PIXEL_MAP_I_TO_G               = $0C73;
  GL_PIXEL_MAP_I_TO_B               = $0C74;
  GL_PIXEL_MAP_I_TO_A               = $0C75;
  GL_PIXEL_MAP_R_TO_R               = $0C76;
  GL_PIXEL_MAP_G_TO_G               = $0C77;
  GL_PIXEL_MAP_B_TO_B               = $0C78;
  GL_PIXEL_MAP_A_TO_A               = $0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE          = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE          = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE          = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE          = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE          = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE          = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE          = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE          = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE          = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE          = $0CB9;
  GL_UNPACK_SWAP_BYTES              = $0CF0;
  GL_UNPACK_LSB_FIRST               = $0CF1;
  GL_UNPACK_ROW_LENGTH              = $0CF2;
  GL_UNPACK_SKIP_ROWS               = $0CF3;
  GL_UNPACK_SKIP_PIXELS             = $0CF4;
  GL_UNPACK_ALIGNMENT               = $0CF5;
  GL_PACK_SWAP_BYTES                = $0D00;
  GL_PACK_LSB_FIRST                 = $0D01;
  GL_PACK_ROW_LENGTH                = $0D02;
  GL_PACK_SKIP_ROWS                 = $0D03;
  GL_PACK_SKIP_PIXELS               = $0D04;
  GL_PACK_ALIGNMENT                 = $0D05;
  GL_MAP_COLOR                      = $0D10;
  GL_MAP_STENCIL                    = $0D11;
  GL_INDEX_SHIFT                    = $0D12;
  GL_INDEX_OFFSET                   = $0D13;
  GL_RED_SCALE                      = $0D14;
  GL_RED_BIAS                       = $0D15;
  GL_ZOOM_X                         = $0D16;
  GL_ZOOM_Y                         = $0D17;
  GL_GREEN_SCALE                    = $0D18;
  GL_GREEN_BIAS                     = $0D19;
  GL_BLUE_SCALE                     = $0D1A;
  GL_BLUE_BIAS                      = $0D1B;
  GL_ALPHA_SCALE                    = $0D1C;
  GL_ALPHA_BIAS                     = $0D1D;
  GL_DEPTH_SCALE                    = $0D1E;
  GL_DEPTH_BIAS                     = $0D1F;
  GL_MAX_EVAL_ORDER                 = $0D30;
  GL_MAX_LIGHTS                     = $0D31;
  GL_MAX_CLIP_PLANES                = $0D32;
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_PIXEL_MAP_TABLE            = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH         = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH      = $0D36;
  GL_MAX_NAME_STACK_DEPTH           = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH     = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH        = $0D39;
  GL_MAX_VIEWPORT_DIMS              = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  = $0D3B;
  GL_SUBPIXEL_BITS                  = $0D50;
  GL_INDEX_BITS                     = $0D51;
  GL_RED_BITS                       = $0D52;
  GL_GREEN_BITS                     = $0D53;
  GL_BLUE_BITS                      = $0D54;
  GL_ALPHA_BITS                     = $0D55;
  GL_DEPTH_BITS                     = $0D56;
  GL_STENCIL_BITS                   = $0D57;
  GL_ACCUM_RED_BITS                 = $0D58;
  GL_ACCUM_GREEN_BITS               = $0D59;
  GL_ACCUM_BLUE_BITS                = $0D5A;
  GL_ACCUM_ALPHA_BITS               = $0D5B;
  GL_NAME_STACK_DEPTH               = $0D70;
  GL_AUTO_NORMAL                    = $0D80;
  GL_MAP1_COLOR_4                   = $0D90;
  GL_MAP1_INDEX                     = $0D91;
  GL_MAP1_NORMAL                    = $0D92;
  GL_MAP1_TEXTURE_COORD_1           = $0D93;
  GL_MAP1_TEXTURE_COORD_2           = $0D94;
  GL_MAP1_TEXTURE_COORD_3           = $0D95;
  GL_MAP1_TEXTURE_COORD_4           = $0D96;
  GL_MAP1_VERTEX_3                  = $0D97;
  GL_MAP1_VERTEX_4                  = $0D98;
  GL_MAP2_COLOR_4                   = $0DB0;
  GL_MAP2_INDEX                     = $0DB1;
  GL_MAP2_NORMAL                    = $0DB2;
  GL_MAP2_TEXTURE_COORD_1           = $0DB3;
  GL_MAP2_TEXTURE_COORD_2           = $0DB4;
  GL_MAP2_TEXTURE_COORD_3           = $0DB5;
  GL_MAP2_TEXTURE_COORD_4           = $0DB6;
  GL_MAP2_VERTEX_3                  = $0DB7;
  GL_MAP2_VERTEX_4                  = $0DB8;
  GL_MAP1_GRID_DOMAIN               = $0DD0;
  GL_MAP1_GRID_SEGMENTS             = $0DD1;
  GL_MAP2_GRID_DOMAIN               = $0DD2;
  GL_MAP2_GRID_SEGMENTS             = $0DD3;
  GL_TEXTURE_1D                     = $0DE0;
  GL_TEXTURE_2D                     = $0DE1;
  GL_FEEDBACK_BUFFER_POINTER        = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE           = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE           = $0DF2;
  GL_SELECTION_BUFFER_POINTER       = $0DF3;
  GL_SELECTION_BUFFER_SIZE          = $0DF4;
  //      GL_TEXTURE_BINDING_1D
  //      GL_TEXTURE_BINDING_2D
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_VERTEX_ARRAY_SIZE
  //      GL_VERTEX_ARRAY_TYPE
  //      GL_VERTEX_ARRAY_STRIDE
  //      GL_NORMAL_ARRAY_TYPE
  //      GL_NORMAL_ARRAY_STRIDE
  //      GL_COLOR_ARRAY_SIZE
  //      GL_COLOR_ARRAY_TYPE
  //      GL_COLOR_ARRAY_STRIDE
  //      GL_INDEX_ARRAY_TYPE
  //      GL_INDEX_ARRAY_STRIDE
  //      GL_TEXTURE_COORD_ARRAY_SIZE
  //      GL_TEXTURE_COORD_ARRAY_TYPE
  //      GL_TEXTURE_COORD_ARRAY_STRIDE
  //      GL_EDGE_FLAG_ARRAY_STRIDE
  //      GL_POLYGON_OFFSET_FACTOR
  //      GL_POLYGON_OFFSET_UNITS

  // GetTextureParameter
  //      GL_TEXTURE_MAG_FILTER
  //      GL_TEXTURE_MIN_FILTER
  //      GL_TEXTURE_WRAP_S
  //      GL_TEXTURE_WRAP_T
  GL_TEXTURE_WIDTH                  = $1000;
  GL_TEXTURE_HEIGHT                 = $1001;
  GL_TEXTURE_INTERNAL_FORMAT        = $1003;
  GL_TEXTURE_BORDER_COLOR           = $1004;
  GL_TEXTURE_BORDER                 = $1005;
  //      GL_TEXTURE_RED_SIZE
  //      GL_TEXTURE_GREEN_SIZE
  //      GL_TEXTURE_BLUE_SIZE
  //      GL_TEXTURE_ALPHA_SIZE
  //      GL_TEXTURE_LUMINANCE_SIZE
  //      GL_TEXTURE_INTENSITY_SIZE
  //      GL_TEXTURE_PRIORITY
  //      GL_TEXTURE_RESIDENT

  // HintMode
  GL_DONT_CARE                      = $1100;
  GL_FASTEST                        = $1101;
  GL_NICEST                         = $1102;

  // HintTarget
  //      GL_PERSPECTIVE_CORRECTION_HINT
  //      GL_POINT_SMOOTH_HINT
  //      GL_LINE_SMOOTH_HINT
  //      GL_POLYGON_SMOOTH_HINT
  //      GL_FOG_HINT

  // IndexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // LightModelParameter
  //      GL_LIGHT_MODEL_AMBIENT
  //      GL_LIGHT_MODEL_LOCAL_VIEWER
  //      GL_LIGHT_MODEL_TWO_SIDE

  // LightName
  GL_LIGHT0                         = $4000;
  GL_LIGHT1                         = $4001;
  GL_LIGHT2                         = $4002;
  GL_LIGHT3                         = $4003;
  GL_LIGHT4                         = $4004;
  GL_LIGHT5                         = $4005;
  GL_LIGHT6                         = $4006;
  GL_LIGHT7                         = $4007;

  // LightParameter
  GL_AMBIENT                        = $1200;
  GL_DIFFUSE                        = $1201;
  GL_SPECULAR                       = $1202;
  GL_POSITION                       = $1203;
  GL_SPOT_DIRECTION                 = $1204;
  GL_SPOT_EXPONENT                  = $1205;
  GL_SPOT_CUTOFF                    = $1206;
  GL_CONSTANT_ATTENUATION           = $1207;
  GL_LINEAR_ATTENUATION             = $1208;
  GL_QUADRATIC_ATTENUATION          = $1209;

  // InterleavedArrays
  //      GL_V2F
  //      GL_V3F
  //      GL_C4UB_V2F
  //      GL_C4UB_V3F
  //      GL_C3F_V3F
  //      GL_N3F_V3F
  //      GL_C4F_N3F_V3F
  //      GL_T2F_V3F
  //      GL_T4F_V4F
  //      GL_T2F_C4UB_V3F
  //      GL_T2F_C3F_V3F
  //      GL_T2F_N3F_V3F
  //      GL_T2F_C4F_N3F_V3F
  //      GL_T4F_C4F_N3F_V4F

  // ListMode
  GL_COMPILE                        = $1300;
  GL_COMPILE_AND_EXECUTE            = $1301;

  // ListNameType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_2_BYTES
  //      GL_3_BYTES
  //      GL_4_BYTES

  // LogicOp
  GL_CLEAR                          = $1500;
  GL_AND                            = $1501;
  GL_AND_REVERSE                    = $1502;
  GL_COPY                           = $1503;
  GL_AND_INVERTED                   = $1504;
  GL_NOOP                           = $1505;
  GL_XOR                            = $1506;
  GL_OR                             = $1507;
  GL_NOR                            = $1508;
  GL_EQUIV                          = $1509;
  GL_INVERT                         = $150A;
  GL_OR_REVERSE                     = $150B;
  GL_COPY_INVERTED                  = $150C;
  GL_OR_INVERTED                    = $150D;
  GL_NAND                           = $150E;
  GL_SET                            = $150F;

  // MapTarget
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4

  // MaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // MaterialParameter
  GL_EMISSION                       = $1600;
  GL_SHININESS                      = $1601;
  GL_AMBIENT_AND_DIFFUSE            = $1602;
  GL_COLOR_INDEXES                  = $1603;
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR

  // MatrixMode
  GL_MODELVIEW                      = $1700;
  GL_PROJECTION                     = $1701;
  GL_TEXTURE                        = $1702;

  // MeshMode1
  //      GL_POINT
  //      GL_LINE

  // MeshMode2
  //      GL_POINT
  //      GL_LINE
  //      GL_FILL

  // NormalPointerType
  //      GL_BYTE
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // PixelCopyType
  GL_COLOR                          = $1800;
  GL_DEPTH                          = $1801;
  GL_STENCIL                        = $1802;

  // PixelFormat
  GL_COLOR_INDEX                    = $1900;
  GL_STENCIL_INDEX                  = $1901;
  GL_DEPTH_COMPONENT                = $1902;
  GL_RED                            = $1903;
  GL_GREEN                          = $1904;
  GL_BLUE                           = $1905;
  GL_ALPHA                          = $1906;
  GL_RGB                            = $1907;
  GL_RGBA                           = $1908;
  GL_LUMINANCE                      = $1909;
  GL_LUMINANCE_ALPHA                = $190A;

      GL_RG=$8227;
      GL_RG_INTEGER=$8228;
      GL_R8=$8229;
      GL_R16=$822A;
      GL_RG8=$822B;
      GL_RG16=$822C;
      GL_R16F=$822D;
      GL_R32F=$822E;
      GL_RG16F=$822F;
      GL_RG32F=$8230;
      GL_R8I=$8231;
      GL_R8UI=$8232;
      GL_R16I=$8233;
      GL_R16UI=$8234;
      GL_R32I=$8235;
      GL_R32UI=$8236;
      GL_RG8I=$8237;
      GL_RG8UI=$8238;
      GL_RG16I=$8239;
      GL_RG16UI=$823A;
      GL_RG32I=$823B;
      GL_RG32UI=$823C;
  
  // PixelMap
  //      GL_PIXEL_MAP_I_TO_I
  //      GL_PIXEL_MAP_S_TO_S
  //      GL_PIXEL_MAP_I_TO_R
  //      GL_PIXEL_MAP_I_TO_G
  //      GL_PIXEL_MAP_I_TO_B
  //      GL_PIXEL_MAP_I_TO_A
  //      GL_PIXEL_MAP_R_TO_R
  //      GL_PIXEL_MAP_G_TO_G
  //      GL_PIXEL_MAP_B_TO_B
  //      GL_PIXEL_MAP_A_TO_A

  // PixelStore
  //      GL_UNPACK_SWAP_BYTES
  //      GL_UNPACK_LSB_FIRST
  //      GL_UNPACK_ROW_LENGTH
  //      GL_UNPACK_SKIP_ROWS
  //      GL_UNPACK_SKIP_PIXELS
  //      GL_UNPACK_ALIGNMENT
  //      GL_PACK_SWAP_BYTES
  //      GL_PACK_LSB_FIRST
  //      GL_PACK_ROW_LENGTH
  //      GL_PACK_SKIP_ROWS
  //      GL_PACK_SKIP_PIXELS
  //      GL_PACK_ALIGNMENT

  // PixelTransfer
  //      GL_MAP_COLOR
  //      GL_MAP_STENCIL
  //      GL_INDEX_SHIFT
  //      GL_INDEX_OFFSET
  //      GL_RED_SCALE
  //      GL_RED_BIAS
  //      GL_GREEN_SCALE
  //      GL_GREEN_BIAS
  //      GL_BLUE_SCALE
  //      GL_BLUE_BIAS
  //      GL_ALPHA_SCALE
  //      GL_ALPHA_BIAS
  //      GL_DEPTH_SCALE
  //      GL_DEPTH_BIAS

  // PixelType
  GL_BITMAP                         = $1A00;
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT

  // PolygonMode
  GL_POINT                          = $1B00;
  GL_LINE                           = $1B01;
  GL_FILL                           = $1B02;

  // ReadBufferMode
  //      GL_FRONT_LEFT
  //      GL_FRONT_RIGHT
  //      GL_BACK_LEFT
  //      GL_BACK_RIGHT
  //      GL_FRONT
  //      GL_BACK
  //      GL_LEFT
  //      GL_RIGHT
  //      GL_AUX0
  //      GL_AUX1
  //      GL_AUX2
  //      GL_AUX3

  // RenderingMode
  GL_RENDER                         = $1C00;
  GL_FEEDBACK                       = $1C01;
  GL_SELECT                         = $1C02;

  // ShadingModel
  GL_FLAT                           = $1D00;
  GL_SMOOTH                         = $1D01;

  // StencilFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // StencilOp
  //      GL_ZERO
  GL_KEEP                           = $1E00;
  GL_REPLACE                        = $1E01;
  GL_INCR                           = $1E02;
  GL_DECR                           = $1E03;
  //      GL_INVERT

  // StringName
  GL_VENDOR                         = $1F00;
  GL_RENDERER                       = $1F01;
  GL_VERSION                        = $1F02;
  GL_EXTENSIONS                     = $1F03;

  // TextureCoordName
  GL_S                              = $2000;
  GL_T                              = $2001;
  GL_R                              = $2002;
  GL_Q                              = $2003;

  // TexCoordPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // TextureEnvMode
  GL_MODULATE                       = $2100;
  GL_DECAL                          = $2101;
  //      GL_BLEND
  //      GL_REPLACE

  // TextureEnvParameter
  GL_TEXTURE_ENV_MODE               = $2200;
  GL_TEXTURE_ENV_COLOR              = $2201;

  // TextureEnvTarget
  GL_TEXTURE_ENV                    = $2300;

  // TextureGenMode
  GL_EYE_LINEAR                     = $2400;
  GL_OBJECT_LINEAR                  = $2401;
  GL_SPHERE_MAP                     = $2402;

  // TextureGenParameter
  GL_TEXTURE_GEN_MODE               = $2500;
  GL_OBJECT_PLANE                   = $2501;
  GL_EYE_PLANE                      = $2502;

  // TextureMagFilter
  GL_NEAREST                        = $2600;
  GL_LINEAR                         = $2601;

  // TextureMinFilter
  //      GL_NEAREST
  //      GL_LINEAR
  GL_NEAREST_MIPMAP_NEAREST         = $2700;
  GL_LINEAR_MIPMAP_NEAREST          = $2701;
  GL_NEAREST_MIPMAP_LINEAR          = $2702;
  GL_LINEAR_MIPMAP_LINEAR           = $2703;

  // TextureParameterName
  GL_TEXTURE_MAG_FILTER             = $2800;
  GL_TEXTURE_MIN_FILTER             = $2801;
  GL_TEXTURE_WRAP_S                 = $2802;
  GL_TEXTURE_WRAP_T                 = $2803;
  //      GL_TEXTURE_BORDER_COLOR
  //      GL_TEXTURE_PRIORITY

  // TextureTarget
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_PROXY_TEXTURE_1D
  //      GL_PROXY_TEXTURE_2D

  // TextureWrapMode
  GL_CLAMP                          = $2900;
  GL_REPEAT                         = $2901;

  // VertexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // ClientAttribMask
  GL_CLIENT_PIXEL_STORE_BIT         = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT        = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS         = $FFFFFFFF;

  // polygon_offset
  GL_POLYGON_OFFSET_FACTOR          = $8038;
  GL_POLYGON_OFFSET_UNITS           = $2A00;
  GL_POLYGON_OFFSET_POINT           = $2A01;
  GL_POLYGON_OFFSET_LINE            = $2A02;
  GL_POLYGON_OFFSET_FILL            = $8037;

  // texture
  GL_ALPHA4                         = $803B;
  GL_ALPHA8                         = $803C;
  GL_ALPHA12                        = $803D;
  GL_ALPHA16                        = $803E;
  GL_LUMINANCE4                     = $803F;
  GL_LUMINANCE8                     = $8040;
  GL_LUMINANCE12                    = $8041;
  GL_LUMINANCE16                    = $8042;
  GL_LUMINANCE4_ALPHA4              = $8043;
  GL_LUMINANCE6_ALPHA2              = $8044;
  GL_LUMINANCE8_ALPHA8              = $8045;
  GL_LUMINANCE12_ALPHA4             = $8046;
  GL_LUMINANCE12_ALPHA12            = $8047;
  GL_LUMINANCE16_ALPHA16            = $8048;
  GL_INTENSITY                      = $8049;
  GL_INTENSITY4                     = $804A;
  GL_INTENSITY8                     = $804B;
  GL_INTENSITY12                    = $804C;
  GL_INTENSITY16                    = $804D;
  GL_R3_G3_B2                       = $2A10;
  GL_RGB4                           = $804F;
  GL_RGB5                           = $8050;
  GL_RGB8                           = $8051;
  GL_RGB10                          = $8052;
  GL_RGB12                          = $8053;
  GL_RGB16                          = $8054;
  GL_RGBA2                          = $8055;
  GL_RGBA4                          = $8056;
  GL_RGB5_A1                        = $8057;
  GL_RGBA8                          = $8058;
  GL_RGB10_A2                       = $8059;
  GL_RGBA12                         = $805A;
  GL_RGBA16                         = $805B;
  GL_TEXTURE_RED_SIZE               = $805C;
  GL_TEXTURE_GREEN_SIZE             = $805D;
  GL_TEXTURE_BLUE_SIZE              = $805E;
  GL_TEXTURE_ALPHA_SIZE             = $805F;
  GL_TEXTURE_LUMINANCE_SIZE         = $8060;
  GL_TEXTURE_INTENSITY_SIZE         = $8061;
  GL_PROXY_TEXTURE_1D               = $8063;
  GL_PROXY_TEXTURE_2D               = $8064;

  // texture_object
  GL_TEXTURE_PRIORITY               = $8066;
  GL_TEXTURE_RESIDENT               = $8067;
  GL_TEXTURE_BINDING_1D             = $8068;
  GL_TEXTURE_BINDING_2D             = $8069;

  // vertex_array
  GL_VERTEX_ARRAY                   = $8074;
  GL_NORMAL_ARRAY                   = $8075;
  GL_COLOR_ARRAY                    = $8076;
  GL_INDEX_ARRAY                    = $8077;
  GL_TEXTURE_COORD_ARRAY            = $8078;
  GL_EDGE_FLAG_ARRAY                = $8079;
  GL_VERTEX_ARRAY_SIZE              = $807A;
  GL_VERTEX_ARRAY_TYPE              = $807B;
  GL_VERTEX_ARRAY_STRIDE            = $807C;
  GL_NORMAL_ARRAY_TYPE              = $807E;
  GL_NORMAL_ARRAY_STRIDE            = $807F;
  GL_COLOR_ARRAY_SIZE               = $8081;
  GL_COLOR_ARRAY_TYPE               = $8082;
  GL_COLOR_ARRAY_STRIDE             = $8083;
  GL_INDEX_ARRAY_TYPE               = $8085;
  GL_INDEX_ARRAY_STRIDE             = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE       = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE       = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE     = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE         = $808C;
  GL_VERTEX_ARRAY_POINTER           = $808E;
  GL_NORMAL_ARRAY_POINTER           = $808F;
  GL_COLOR_ARRAY_POINTER            = $8090;
  GL_INDEX_ARRAY_POINTER            = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER    = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER        = $8093;
  GL_V2F                            = $2A20;
  GL_V3F                            = $2A21;
  GL_C4UB_V2F                       = $2A22;
  GL_C4UB_V3F                       = $2A23;
  GL_C3F_V3F                        = $2A24;
  GL_N3F_V3F                        = $2A25;
  GL_C4F_N3F_V3F                    = $2A26;
  GL_T2F_V3F                        = $2A27;
  GL_T4F_V4F                        = $2A28;
  GL_T2F_C4UB_V3F                   = $2A29;
  GL_T2F_C3F_V3F                    = $2A2A;
  GL_T2F_N3F_V3F                    = $2A2B;
  GL_T2F_C4F_N3F_V3F                = $2A2C;
  GL_T4F_C4F_N3F_V4F                = $2A2D;

  // For compatibility with OpenGL v1.0
  GL_LOGIC_OP                       = GL_INDEX_LOGIC_OP;
  GL_TEXTURE_COMPONENTS             = GL_TEXTURE_INTERNAL_FORMAT;

{******************************************************************************}

{$IFDEF MORPHOS}

{ MorphOS GL works differently due to different dynamic-library handling on Amiga-like }
{ systems, so its headers are included here. }
{$INCLUDE tinyglh.inc}

{$ELSE MORPHOS}
var
  glAccum: procedure(op: GLenum; value: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAlphaFunc: procedure(func: GLenum; ref: GLclampf); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAreTexturesResident: function (n: GLsizei; const textures: PGLuint; residences: PGLboolean): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glArrayElement: procedure(i: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBegin: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindTexture: procedure(target: GLenum; texture: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBitmap: procedure (width, height: GLsizei; xorig, yorig: GLfloat; xmove, ymove: GLfloat; const bitmap: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBlendFunc: procedure(sfactor, dfactor: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCallList: procedure(list: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCallLists: procedure(n: GLsizei; atype: GLenum; const lists: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClear: procedure(mask: GLbitfield); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearAccum: procedure(red, green, blue, alpha: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearColor: procedure(red, green, blue, alpha: GLclampf); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearDepth: procedure(depth: GLclampd); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearIndex: procedure(c: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearStencil: procedure(s: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClipPlane: procedure(plane: GLenum; const equation: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3b: procedure(red, green, blue: GLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3bv: procedure(const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3d: procedure(red, green, blue: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3f: procedure(red, green, blue: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3i: procedure(red, green, blue: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3s: procedure(red, green, blue: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3ub: procedure(red, green, blue: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3ubv: procedure(const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3ui: procedure(red, green, blue: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3uiv: procedure(const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3us: procedure(red, green, blue: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3usv: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4b: procedure(red, green, blue, alpha: GLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4bv: procedure(const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4d: procedure(red, green, blue, alpha: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4f: procedure(red, green, blue, alpha: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4i: procedure(red, green, blue, alpha: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4s: procedure(red, green, blue, alpha: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4ub: procedure(red, green, blue, alpha: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4ubv: procedure(const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4ui: procedure(red, green, blue, alpha: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4uiv: procedure(const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4us: procedure(red, green, blue, alpha: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4usv: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorMask: procedure(red, green, blue, alpha: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorMaterial: procedure(face, mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyPixels: procedure(x, y: GLint; width, height: GLsizei; atype: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyTexImage1D: procedure (target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width: GLsizei; border: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y: GLint; width: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCullFace: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteLists: procedure(list: GLuint; range: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteTextures: procedure(n: GLsizei; const textures: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDepthFunc: procedure(func: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDepthMask: procedure(flag: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDepthRange: procedure(zNear, zFar: GLclampd); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDisable: procedure(cap: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDisableClientState: procedure(aarray: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawBuffer: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawElements: procedure(mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawPixels: procedure(width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEdgeFlag: procedure(flag: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEdgeFlagPointer: procedure(stride: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEdgeFlagv: procedure(const flag: PGLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEnable: procedure(cap: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEnableClientState: procedure(aarray: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEnd: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndList: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalCoord1d: procedure(u: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalCoord1dv: procedure(const u: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalCoord1f: procedure(u: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalCoord1fv: procedure(const u: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalCoord2d: procedure(u, v: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalCoord2dv: procedure(const u: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalCoord2f: procedure(u, v: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalCoord2fv: procedure(const u: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalMesh1: procedure(mode: GLenum; i1, i2: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalPoint1: procedure(i: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalPoint2: procedure(i, j: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFeedbackBuffer: procedure(size: GLsizei; atype: GLenum; buffer: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFinish: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFlush: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogf: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogfv: procedure(pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogi: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogiv: procedure(pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFrontFace: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFrustum: procedure(left, right, bottom, top, zNear, zFar: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenLists: function(range: GLsizei): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenTextures: procedure(n: GLsizei; textures: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetBooleanv: procedure(pname: GLenum; params: PGLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetClipPlane: procedure(plane: GLenum; equation: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetDoublev: procedure(pname: GLenum; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetError: function: GLenum; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetFloatv: procedure(pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetIntegerv: procedure(pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetLightfv: procedure(light, pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetLightiv: procedure(light, pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMapdv: procedure(target, query: GLenum; v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMapfv: procedure(target, query: GLenum; v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMapiv: procedure(target, query: GLenum; v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMaterialfv: procedure(face, pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMaterialiv: procedure(face, pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetPixelMapfv: procedure(map: GLenum; values: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetPixelMapuiv: procedure(map: GLenum; values: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetPixelMapusv: procedure(map: GLenum; values: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetPointerv: procedure(pname: GLenum; params: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetPolygonStipple: procedure(mask: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetString: function(name: GLenum): pansichar; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexEnvfv: procedure(target, pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexEnviv: procedure(target, pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexGendv: procedure(coord, pname: GLenum; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexGenfv: procedure(coord, pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexGeniv: procedure(coord, pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexImage: procedure(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexLevelParameterfv: procedure(target: GLenum; level: GLint; pname: GLenum; params: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexLevelParameteriv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexParameterfv: procedure(target, pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexParameteriv: procedure(target, pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glHint: procedure(target, mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexMask: procedure(mask: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexPointer: procedure(atype: GLenum; stride: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexd: procedure(c: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexdv: procedure(const c: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexf: procedure(c: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexfv: procedure(const c: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexi: procedure(c: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexiv: procedure(const c: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexs: procedure(c: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexsv: procedure(const c: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexub: procedure(c: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexubv: procedure(const c: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glInitNames: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glInterleavedArrays: procedure(format: GLenum; stride: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsEnabled: function(cap: GLenum): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsList: function(list: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsTexture: function(texture: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLightModelf: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLightModelfv: procedure(pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLightModeli: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLightModeliv: procedure(pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLightf: procedure(light, pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLightfv: procedure(light, pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLighti: procedure(light, pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLightiv: procedure(light, pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLineStipple: procedure(factor: GLint; pattern: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLineWidth: procedure(width: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glListBase: procedure(base: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLoadIdentity: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLoadMatrixd: procedure(const m: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLoadMatrixf: procedure(const m: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLoadName: procedure(name: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLogicOp: procedure(opcode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMap1d: procedure(target: GLenum; u1, u2: GLdouble; stride, order: GLint; const points: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMap1f: procedure(target: GLenum; u1, u2: GLfloat; stride, order: GLint; const points: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMap2d: procedure(target: GLenum; u1, u2: GLdouble; ustride, uorder: GLint; v1, v2: GLdouble; vstride, vorder: GLint; const points: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMap2f: procedure(target: GLenum; u1, u2: GLfloat; ustride, uorder: GLint; v1, v2: GLfloat; vstride, vorder: GLint; const points: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMapGrid1d: procedure(un: GLint; u1, u2: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMapGrid1f: procedure(un: GLint; u1, u2: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMapGrid2d: procedure(un: GLint; u1, u2: GLdouble; vn: GLint; v1, v2: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMapGrid2f: procedure(un: GLint; u1, u2: GLfloat; vn: GLint; v1, v2: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMaterialf: procedure(face, pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMaterialfv: procedure(face, pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMateriali: procedure(face, pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMaterialiv: procedure(face, pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMatrixMode: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultMatrixd: procedure(const m: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultMatrixf: procedure(const m: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNewList: procedure(list: GLuint; mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3b: procedure(nx, ny, nz: GLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3bv: procedure(const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3d: procedure(nx, ny, nz: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3f: procedure(nx, ny, nz: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3i: procedure(nx, ny, nz: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3s: procedure(nx, ny, nz: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalPointer: procedure(atype: GLenum; stride: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glOrtho: procedure(left, right, bottom, top, zNear, zFar: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPassThrough: procedure(token: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelMapfv: procedure(map: GLenum; mapsize: GLint; const values: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelMapuiv: procedure(map: GLenum; mapsize: GLint; const values: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelMapusv: procedure(map: GLenum; mapsize: GLint; const values: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelStoref: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelStorei: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelTransferf: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelTransferi: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelZoom: procedure(xfactor, yfactor: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPointSize: procedure(size: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPolygonMode: procedure(face, mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPolygonOffset: procedure(factor, units: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPolygonStipple: procedure(const mask: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPopAttrib: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPopClientAttrib: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPopMatrix: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPopName: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPrioritizeTextures: procedure(n: GLsizei; const textures: PGLuint; const priorities: PGLclampf); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPushAttrib: procedure(mask: GLbitfield); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPushClientAttrib: procedure(mask: GLbitfield); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPushMatrix: procedure; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPushName: procedure(name: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos2d: procedure(x, y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos2dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos2f: procedure(x, y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos2fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos2i: procedure(x, y: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos2iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos2s: procedure(x, y: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos2sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos3d: procedure(x, y, z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos3dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos3f: procedure(x, y, z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos3fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos3i: procedure(x, y, z: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos3iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos3s: procedure(x, y, z: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos3sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos4d: procedure(x, y, z, w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos4dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos4f: procedure(x, y, z, w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos4fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos4i: procedure(x, y, z, w: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos4iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos4s: procedure(x, y, z, w: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRasterPos4sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReadBuffer: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReadPixels: procedure(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRectd: procedure(x1, y1, x2, y2: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRectdv: procedure(const v1: PGLdouble; const v2: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRectf: procedure(x1, y1, x2, y2: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRectfv: procedure(const v1: PGLfloat; const v2: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRecti: procedure(x1, y1, x2, y2: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRectiv: procedure(const v1: PGLint; const v2: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRects: procedure(x1, y1, x2, y2: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRectsv: procedure(const v1: PGLshort; const v2: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRenderMode: function(mode: GLint): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRotated: procedure(angle, x, y, z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRotatef: procedure(angle, x, y, z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glScaled: procedure(x, y, z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glScalef: procedure(x, y, z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glScissor: procedure(x, y: GLint; width, height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSelectBuffer: procedure(size: GLsizei; buffer: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glShadeModel: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glStencilMask: procedure(mask: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glStencilOp: procedure(fail, zfail, zpass: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1d: procedure(s: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1f: procedure(s: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1i: procedure(s: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1s: procedure(s: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2d: procedure(s, t: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2f: procedure(s, t: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2i: procedure(s, t: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2s: procedure(s, t: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3d: procedure(s, t, r: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3f: procedure(s, t, r: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3i: procedure(s, t, r: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3s: procedure(s, t, r: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4d: procedure(s, t, r, q: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4f: procedure(s, t, r, q: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4i: procedure(s, t, r, q: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4s: procedure(s, t, r, q: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexEnvf: procedure(target: GLenum; pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexEnvfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexEnvi: procedure(target: GLenum; pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexEnviv: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexGend: procedure(coord: GLenum; pname: GLenum; param: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexGendv: procedure(coord: GLenum; pname: GLenum; const params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexGenf: procedure(coord: GLenum; pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexGenfv: procedure(coord: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexGeni: procedure(coord: GLenum; pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexGeniv: procedure(coord: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexImage1D: procedure(target: GLenum; level: GLInt; internalformat: GLEnum; width: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexImage2D: procedure(target: GLenum; level: GLInt; internalformat: GLEnum; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexParameterf: procedure(target: GLenum; pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexSubImage1D: procedure(target: GLenum; level, xoffset: GLint; width: GLsizei; format, atype: GLenum; const pixels: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTranslated: procedure(x, y, z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTranslatef: procedure(x, y, z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2d: procedure(x, y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2f: procedure(x, y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2i: procedure(x, y: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2s: procedure(x, y: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3d: procedure(x, y, z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3f: procedure(x, y, z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3i: procedure(x, y, z: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3s: procedure(x, y, z: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4d: procedure(x, y, z, w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4f: procedure(x, y, z, w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4i: procedure(x, y, z, w: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4s: procedure(x, y, z, w: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glViewport: procedure(x, y: GLint; width, height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  {$IFDEF Windows}
  ChoosePixelFormat: function(DC: HDC; p2: PPixelFormatDescriptor): Integer; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  {$ENDIF}
{$ENDIF MORPHOS}

type
  // EXT_vertex_array
  PFNGLARRAYELEMENTEXTPROC = procedure(i: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLDRAWARRAYSEXTPROC = procedure(mode: GLenum; first: GLint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLVERTEXPOINTEREXTPROC = procedure(size: GLint; atype: GLenum;
                                        stride, count: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLNORMALPOINTEREXTPROC = procedure(atype: GLenum; stride, count: GLsizei;
                                        const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLCOLORPOINTEREXTPROC = procedure(size: GLint; atype: GLenum; stride, count: GLsizei;
                                       const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLINDEXPOINTEREXTPROC = procedure(atype: GLenum; stride, count: GLsizei;
                                       const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLTEXCOORDPOINTEREXTPROC = procedure(size: GLint; atype: GLenum;
                                          stride, count: GLsizei; const pointer: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLEDGEFLAGPOINTEREXTPROC = procedure(stride, count: GLsizei;
                                          const pointer: PGLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLGETPOINTERVEXTPROC = procedure(pname: GLenum; params: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLARRAYELEMENTARRAYEXTPROC = procedure(mode: GLenum; count: GLsizei;
                                            const pi: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

  // WIN_swap_hint
  PFNGLADDSWAPHINTRECTWINPROC = procedure(x, y: GLint; width, height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

  // EXT_paletted_texture
  PFNGLCOLORTABLEEXTPROC = procedure(target, internalFormat: GLenum; width: GLsizei;
                                     format, atype: GLenum; const data: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLCOLORSUBTABLEEXTPROC = procedure(target: GLenum; start, count: GLsizei;
                                        format, atype: GLenum; const data: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLGETCOLORTABLEEXTPROC = procedure(target, format, atype: GLenum; data: Pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLGETCOLORTABLEPARAMETERIVEXTPROC = procedure(target, pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  PFNGLGETCOLORTABLEPARAMETERFVEXTPROC = procedure(target, pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

procedure LoadOpenGL(const dll: String);
procedure FreeOpenGL;

// Extensions

{$IFDEF Windows}
{ Declared in Windows unit as well in FPC; but declared here as well, to be
  fully compatible to upstream version  - sg }
function wglGetProcAddress(proc: pansichar): Pointer; {$ifdef fpc}extdecl;{$else}stdcall;{$endif} external 'OpenGL32.dll';
{$ELSE}
function wglGetProcAddress(proc: pansichar): Pointer;
{$ENDIF}

// Test if the given extension name is present in the given extension ansistring.
function glext_ExtensionSupported(const extension: ansistring; const searchIn: ansistring): Boolean;

// Load the extension with the given name.
function glext_LoadExtension(ext: ansistring): Boolean;

type

  GLcharARB = ansichar;
  TGLcharARB = GLcharARB;
  PGLcharARB = ^GLcharARB;
  PPGLchar = ^PGLchar;

  GLhandleARB = Cardinal;
  TGLhandleARB = GLhandleARB;
  PGLhandleARB = ^GLhandleARB;

  GLintptr = GLPtrInt;
  TGLintptr = GLintptr;
  PGLintptr = ^GLintptr;

  GLsizeiptr = GLPtrInt;
  TGLsizeiptr = GLsizeiptr;
  PGLsizeiptr = ^GLsizeiptr;

  GLchar = ansichar;
  TGLchar = GLchar;
  PGLchar = pansichar;

  GLint64 = Int64;
  TGLint64 = GLint64;
  PGLint64 = ^GLint64;

  GLuint64 = {$ifdef fpc}QWord{$else}UInt64{$endif};
  TGLuint64 = GLuint64;
  PGLuint64 = ^GLuint64;


//***** GL_version_1_2 *****//
const
  GL_UNSIGNED_BYTE_3_3_2 = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_INT_8_8_8_8 = $8035;
  GL_UNSIGNED_INT_10_10_10_2 = $8036;
  GL_RESCALE_NORMAL = $803A;
  GL_UNSIGNED_BYTE_2_3_3_REV = $8362;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
  GL_BGR = $80E0;
  GL_BGRA = $80E1;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_CLAMP_TO_EDGE = $812F;
  GL_TEXTURE_MIN_LOD = $813A;
  GL_TEXTURE_MAX_LOD = $813B;
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;
  GL_LIGHT_MODEL_COLOR_CONTROL = $81F8;
  GL_SINGLE_COLOR = $81F9;
  GL_SEPARATE_SPECULAR_COLOR = $81FA;
  GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY = $0B23;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
  GL_PACK_SKIP_IMAGES = $806B;
  GL_PACK_IMAGE_HEIGHT = $806C;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_PROXY_TEXTURE_3D = $8070;
  GL_TEXTURE_DEPTH = $8071;
  GL_TEXTURE_WRAP_R = $8072;
  GL_MAX_3D_TEXTURE_SIZE = $8073;
var
  glBlendColor: procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBlendEquation: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawRangeElements: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorTable: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const table: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorTableParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorTableParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyColorTable: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetColorTable: procedure(target: GLenum; format: GLenum; _type: GLenum; table: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetColorTableParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetColorTableParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorSubTable: procedure(target: GLenum; start: GLsizei; count: GLsizei; format: GLenum; _type: GLenum; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyColorSubTable: procedure(target: GLenum; start: GLsizei; x: GLint; y: GLint; width: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionFilter1D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const image: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionFilter2D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const image: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionParameterf: procedure(target: GLenum; pname: GLenum; params: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionParameteri: procedure(target: GLenum; pname: GLenum; params: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyConvolutionFilter1D: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyConvolutionFilter2D: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetConvolutionFilter: procedure(target: GLenum; format: GLenum; _type: GLenum; image: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetConvolutionParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetConvolutionParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetSeparableFilter: procedure(target: GLenum; format: GLenum; _type: GLenum; row: PGLvoid; column: PGLvoid; span: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSeparableFilter2D: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const row: PGLvoid; const column: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetHistogram: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetHistogramParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetHistogramParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMinmax: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMinmaxParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMinmaxParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glHistogram: procedure(target: GLenum; width: GLsizei; internalformat: GLenum; sink: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMinmax: procedure(target: GLenum; internalformat: GLenum; sink: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glResetHistogram: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glResetMinmax: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_version_1_2: Boolean;

//***** GL_ARB_imaging *****//
const
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_BLEND_COLOR = $8005;
  GL_FUNC_ADD = $8006;
  GL_MIN = $8007;
  GL_MAX = $8008;
  GL_BLEND_EQUATION = $8009;
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;
  GL_CONVOLUTION_1D = $8010;
  GL_CONVOLUTION_2D = $8011;
  GL_SEPARABLE_2D = $8012;
  GL_CONVOLUTION_BORDER_MODE = $8013;
  GL_CONVOLUTION_FILTER_SCALE = $8014;
  GL_CONVOLUTION_FILTER_BIAS = $8015;
  GL_REDUCE = $8016;
  GL_CONVOLUTION_FORMAT = $8017;
  GL_CONVOLUTION_WIDTH = $8018;
  GL_CONVOLUTION_HEIGHT = $8019;
  GL_MAX_CONVOLUTION_WIDTH = $801A;
  GL_MAX_CONVOLUTION_HEIGHT = $801B;
  GL_POST_CONVOLUTION_RED_SCALE = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE = $801F;
  GL_POST_CONVOLUTION_RED_BIAS = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS = $8023;
  GL_HISTOGRAM = $8024;
  GL_PROXY_HISTOGRAM = $8025;
  GL_HISTOGRAM_WIDTH = $8026;
  GL_HISTOGRAM_FORMAT = $8027;
  GL_HISTOGRAM_RED_SIZE = $8028;
  GL_HISTOGRAM_GREEN_SIZE = $8029;
  GL_HISTOGRAM_BLUE_SIZE = $802A;
  GL_HISTOGRAM_ALPHA_SIZE = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE = $802C;
  GL_HISTOGRAM_SINK = $802D;
  GL_MINMAX = $802E;
  GL_MINMAX_FORMAT = $802F;
  GL_MINMAX_SINK = $8030;
  GL_TABLE_TOO_LARGE = $8031;
  GL_COLOR_MATRIX = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS = $80BA;
  GL_POST_COLOR_MATIX_ALPHA_BIAS = $80BB;
  GL_COLOR_TABLE = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE = $80D2;
  GL_PROXY_COLOR_TABLE = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = $80D5;
  GL_COLOR_TABLE_SCALE = $80D6;
  GL_COLOR_TABLE_BIAS = $80D7;
  GL_COLOR_TABLE_FORMAT = $80D8;
  GL_COLOR_TABLE_WIDTH = $80D9;
  GL_COLOR_TABLE_RED_SIZE = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE = $80DF;
  GL_IGNORE_BORDER = $8150;
  GL_CONSTANT_BORDER = $8151;
  GL_WRAP_BORDER = $8152;
  GL_REPLICATE_BORDER = $8153;
  GL_CONVOLUTION_BORDER_COLOR = $8154;

function Load_GL_ARB_imaging: Boolean;

//***** GL_version_1_3 *****//
const
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE = $84E1;
  GL_MAX_TEXTURE_UNITS = $84E2;
  GL_TRANSPOSE_MODELVIEW_MATRIX = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX = $84E6;
  GL_MULTISAMPLE = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_ALPHA_TO_ONE = $809F;
  GL_SAMPLE_COVERAGE = $80A0;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  GL_MULTISAMPLE_BIT = $20000000;
  GL_NORMAL_MAP = $8511;
  GL_REFLECTION_MAP = $8512;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_CUBE_MAP_SEAMLESS = $884F;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  GL_COMPRESSED_ALPHA = $84E9;
  GL_COMPRESSED_LUMINANCE = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA = $84EB;
  GL_COMPRESSED_INTENSITY = $84EC;
  GL_COMPRESSED_RGB = $84ED;
  GL_COMPRESSED_RGBA = $84EE;
  GL_TEXTURE_COMPRESSION_HINT = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
  GL_TEXTURE_COMPRESSED = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  GL_CLAMP_TO_BORDER = $812D;
  GL_CLAMP_TO_BORDER_SGIS = $812D;
  GL_COMBINE = $8570;
  GL_COMBINE_RGB = $8571;
  GL_COMBINE_ALPHA = $8572;
  GL_SOURCE0_RGB = $8580;
  GL_SOURCE1_RGB = $8581;
  GL_SOURCE2_RGB = $8582;
  GL_SOURCE0_ALPHA = $8588;
  GL_SOURCE1_ALPHA = $8589;
  GL_SOURCE2_ALPHA = $858A;
  GL_OPERAND0_RGB = $8590;
  GL_OPERAND1_RGB = $8591;
  GL_OPERAND2_RGB = $8592;
  GL_OPERAND0_ALPHA = $8598;
  GL_OPERAND1_ALPHA = $8599;
  GL_OPERAND2_ALPHA = $859A;
  GL_RGB_SCALE = $8573;
  GL_ADD_SIGNED = $8574;
  GL_INTERPOLATE = $8575;
  GL_SUBTRACT = $84E7;
  GL_CONSTANT = $8576;
  GL_PRIMARY_COLOR = $8577;
  GL_PREVIOUS = $8578;
  GL_DOT3_RGB = $86AE;
  GL_DOT3_RGBA = $86AF;
var
  glActiveTexture: procedure(texture: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClientActiveTexture: procedure(texture: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1d: procedure(target: GLenum; s: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1dv: procedure(target: GLenum; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1f: procedure(target: GLenum; s: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1fv: procedure(target: GLenum; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1i: procedure(target: GLenum; s: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1iv: procedure(target: GLenum; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1s: procedure(target: GLenum; s: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1sv: procedure(target: GLenum; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2d: procedure(target: GLenum; s: GLdouble; t: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2dv: procedure(target: GLenum; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2f: procedure(target: GLenum; s: GLfloat; t: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2fv: procedure(target: GLenum; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2i: procedure(target: GLenum; s: GLint; t: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2iv: procedure(target: GLenum; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2s: procedure(target: GLenum; s: GLshort; t: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2sv: procedure(target: GLenum; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3d: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3dv: procedure(target: GLenum; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3f: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3fv: procedure(target: GLenum; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3i: procedure(target: GLenum; s: GLint; t: GLint; r: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3iv: procedure(target: GLenum; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3s: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3sv: procedure(target: GLenum; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4d: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble; q: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4dv: procedure(target: GLenum; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4f: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat; q: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4fv: procedure(target: GLenum; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4i: procedure(target: GLenum; s: GLint; t: GLint; r: GLint; q: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4iv: procedure(target: GLenum; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4s: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort; q: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4sv: procedure(target: GLenum; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLoadTransposeMatrixf: procedure(const m: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLoadTransposeMatrixd: procedure(const m: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultTransposeMatrixf: procedure(const m: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultTransposeMatrixd: procedure(const m: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSampleCoverage: procedure(value: GLclampf; invert: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexImage1D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexSubImage1D: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetCompressedTexImage: procedure(target: GLenum; level: GLint; img: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_version_1_3: Boolean;

//***** GL_ARB_multitexture *****//
const
  GL_TEXTURE0_ARB = $84C0;
  GL_TEXTURE1_ARB = $84C1;
  GL_TEXTURE2_ARB = $84C2;
  GL_TEXTURE3_ARB = $84C3;
  GL_TEXTURE4_ARB = $84C4;
  GL_TEXTURE5_ARB = $84C5;
  GL_TEXTURE6_ARB = $84C6;
  GL_TEXTURE7_ARB = $84C7;
  GL_TEXTURE8_ARB = $84C8;
  GL_TEXTURE9_ARB = $84C9;
  GL_TEXTURE10_ARB = $84CA;
  GL_TEXTURE11_ARB = $84CB;
  GL_TEXTURE12_ARB = $84CC;
  GL_TEXTURE13_ARB = $84CD;
  GL_TEXTURE14_ARB = $84CE;
  GL_TEXTURE15_ARB = $84CF;
  GL_TEXTURE16_ARB = $84D0;
  GL_TEXTURE17_ARB = $84D1;
  GL_TEXTURE18_ARB = $84D2;
  GL_TEXTURE19_ARB = $84D3;
  GL_TEXTURE20_ARB = $84D4;
  GL_TEXTURE21_ARB = $84D5;
  GL_TEXTURE22_ARB = $84D6;
  GL_TEXTURE23_ARB = $84D7;
  GL_TEXTURE24_ARB = $84D8;
  GL_TEXTURE25_ARB = $84D9;
  GL_TEXTURE26_ARB = $84DA;
  GL_TEXTURE27_ARB = $84DB;
  GL_TEXTURE28_ARB = $84DC;
  GL_TEXTURE29_ARB = $84DD;
  GL_TEXTURE30_ARB = $84DE;
  GL_TEXTURE31_ARB = $84DF;
  GL_ACTIVE_TEXTURE_ARB = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE_ARB = $84E1;
  GL_MAX_TEXTURE_UNITS_ARB = $84E2;
var
  glActiveTextureARB: procedure(texture: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClientActiveTextureARB: procedure(texture: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1dARB: procedure(target: GLenum; s: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1dvARB: procedure(target: GLenum; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1fARB: procedure(target: GLenum; s: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1fvARB: procedure(target: GLenum; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1iARB: procedure(target: GLenum; s: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1ivARB: procedure(target: GLenum; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1sARB: procedure(target: GLenum; s: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1svARB: procedure(target: GLenum; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2dARB: procedure(target: GLenum; s: GLdouble; t: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2dvARB: procedure(target: GLenum; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2fARB: procedure(target: GLenum; s: GLfloat; t: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2fvARB: procedure(target: GLenum; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2iARB: procedure(target: GLenum; s: GLint; t: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2ivARB: procedure(target: GLenum; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2sARB: procedure(target: GLenum; s: GLshort; t: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2svARB: procedure(target: GLenum; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3dARB: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3dvARB: procedure(target: GLenum; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3fARB: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3fvARB: procedure(target: GLenum; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3iARB: procedure(target: GLenum; s: GLint; t: GLint; r: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3ivARB: procedure(target: GLenum; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3sARB: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3svARB: procedure(target: GLenum; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4dARB: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble; q: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4dvARB: procedure(target: GLenum; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4fARB: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat; q: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4fvARB: procedure(target: GLenum; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4iARB: procedure(target: GLenum; s: GLint; t: GLint; r: GLint; q: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4ivARB: procedure(target: GLenum; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4sARB: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort; q: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4svARB: procedure(target: GLenum; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_multitexture: Boolean;

//***** GL_ARB_transpose_matrix *****//
const
  GL_TRANSPOSE_MODELVIEW_MATRIX_ARB = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX_ARB = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX_ARB = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX_ARB = $84E6;
var
  glLoadTransposeMatrixfARB: procedure(m: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLoadTransposeMatrixdARB: procedure(m: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultTransposeMatrixfARB: procedure(m: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultTransposeMatrixdARB: procedure(m: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_transpose_matrix: Boolean;

//***** GL_ARB_multisample *****//
const
  WGL_SAMPLE_BUFFERS_ARB = $2041;
  WGL_SAMPLES_ARB = $2042;
  GL_MULTISAMPLE_ARB = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB = $809F;
  GL_SAMPLE_COVERAGE_ARB = $80A0;
  GL_MULTISAMPLE_BIT_ARB = $20000000;
  GL_SAMPLE_BUFFERS_ARB = $80A8;
  GL_SAMPLES_ARB = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB = $80AB;
var
  glSampleCoverageARB: procedure(value: GLclampf; invert: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_multisample: Boolean;

//***** GL_ARB_texture_env_add *****//

function Load_GL_ARB_texture_env_add: Boolean;

{$IFDEF Windows}
//***** WGL_ARB_extensions_string *****//
var
  wglGetExtensionsStringARB: function(hdc: HDC): pansichar; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_ARB_extensions_string: Boolean;

//***** WGL_ARB_buffer_region *****//
const
  WGL_FRONT_COLOR_BUFFER_BIT_ARB = $0001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB = $0002;
  WGL_DEPTH_BUFFER_BIT_ARB = $0004;
  WGL_STENCIL_BUFFER_BIT_ARB = $0008;
var
  wglCreateBufferRegionARB: function(hDC: HDC; iLayerPlane: GLint; uType: GLuint): THandle; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglDeleteBufferRegionARB: procedure(hRegion: THandle); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglSaveBufferRegionARB: function(hRegion: THandle; x: GLint; y: GLint; width: GLint; height: GLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglRestoreBufferRegionARB: function(hRegion: THandle; x: GLint; y: GLint; width: GLint; height: GLint; xSrc: GLint; ySrc: GLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_ARB_buffer_region: Boolean;
{$ENDIF}

//***** GL_ARB_texture_cube_map *****//
const
  GL_NORMAL_MAP_ARB = $8511;
  GL_REFLECTION_MAP_ARB = $8512;
  GL_TEXTURE_CUBE_MAP_ARB = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_ARB = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARB = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = $851C;

function Load_GL_ARB_texture_cube_map: Boolean;

//***** GL_ARB_depth_texture *****//
const
  GL_DEPTH_COMPONENT16_ARB = $81A5;
  GL_DEPTH_COMPONENT24_ARB = $81A6;
  GL_DEPTH_COMPONENT32_ARB = $81A7;
  GL_TEXTURE_DEPTH_SIZE_ARB = $884A;
  GL_DEPTH_TEXTURE_MODE_ARB = $884B;

function Load_GL_ARB_depth_texture: Boolean;

//***** GL_ARB_point_parameters *****//
const
  GL_POINT_SIZE_MIN_ARB = $8126;
  GL_POINT_SIZE_MAX_ARB = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_ARB = $8128;
  GL_POINT_DISTANCE_ATTENUATION_ARB = $8129;
var
  glPointParameterfARB: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPointParameterfvARB: procedure(pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_point_parameters: Boolean;

//***** GL_ARB_shadow *****//
const
  GL_TEXTURE_COMPARE_MODE_ARB = $884C;
  GL_TEXTURE_COMPARE_FUNC_ARB = $884D;
  GL_COMPARE_R_TO_TEXTURE_ARB = $884E;

function Load_GL_ARB_shadow: Boolean;

//***** GL_ARB_shadow_ambient *****//
const
  GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = $80BF;

function Load_GL_ARB_shadow_ambient: Boolean;

//***** GL_ARB_texture_border_clamp *****//
const
  GL_CLAMP_TO_BORDER_ARB = $812D;

function Load_GL_ARB_texture_border_clamp: Boolean;

//***** GL_ARB_texture_compression *****//
const
  GL_COMPRESSED_ALPHA_ARB = $84E9;
  GL_COMPRESSED_LUMINANCE_ARB = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB = $84EB;
  GL_COMPRESSED_INTENSITY_ARB = $84EC;
  GL_COMPRESSED_RGB_ARB = $84ED;
  GL_COMPRESSED_RGBA_ARB = $84EE;
  GL_TEXTURE_COMPRESSION_HINT_ARB = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB = $86A0;
  GL_TEXTURE_COMPRESSED_ARB = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS_ARB = $86A3;
var
  glCompressedTexImage3DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexImage2DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexImage1DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexSubImage3DARB: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexSubImage2DARB: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompressedTexSubImage1DARB: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetCompressedTexImageARB: procedure(target: GLenum; lod: GLint; img: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_texture_compression: Boolean;

//***** GL_ARB_texture_env_combine *****//
const
  GL_COMBINE_ARB = $8570;
  GL_COMBINE_RGB_ARB = $8571;
  GL_COMBINE_ALPHA_ARB = $8572;
  GL_SOURCE0_RGB_ARB = $8580;
  GL_SOURCE1_RGB_ARB = $8581;
  GL_SOURCE2_RGB_ARB = $8582;
  GL_SOURCE0_ALPHA_ARB = $8588;
  GL_SOURCE1_ALPHA_ARB = $8589;
  GL_SOURCE2_ALPHA_ARB = $858A;
  GL_OPERAND0_RGB_ARB = $8590;
  GL_OPERAND1_RGB_ARB = $8591;
  GL_OPERAND2_RGB_ARB = $8592;
  GL_OPERAND0_ALPHA_ARB = $8598;
  GL_OPERAND1_ALPHA_ARB = $8599;
  GL_OPERAND2_ALPHA_ARB = $859A;
  GL_RGB_SCALE_ARB = $8573;
  GL_ADD_SIGNED_ARB = $8574;
  GL_INTERPOLATE_ARB = $8575;
  GL_SUBTRACT_ARB = $84E7;
  GL_CONSTANT_ARB = $8576;
  GL_PRIMARY_COLOR_ARB = $8577;
  GL_PREVIOUS_ARB = $8578;

function Load_GL_ARB_texture_env_combine: Boolean;

//***** GL_ARB_texture_env_crossbar *****//

function Load_GL_ARB_texture_env_crossbar: Boolean;

//***** GL_ARB_texture_env_dot3 *****//
const
  GL_DOT3_RGB_ARB = $86AE;
  GL_DOT3_RGBA_ARB = $86AF;

function Load_GL_ARB_texture_env_dot3: Boolean;

//***** GL_ARB_texture_mirrored_repeat *****//
const
  GL_MIRRORED_REPEAT_ARB = $8370;

function Load_GL_ARB_texture_mirrored_repeat: Boolean;

//***** GL_ARB_vertex_blend *****//
const
  GL_MAX_VERTEX_UNITS_ARB = $86A4;
  GL_ACTIVE_VERTEX_UNITS_ARB = $86A5;
  GL_WEIGHT_SUM_UNITY_ARB = $86A6;
  GL_VERTEX_BLEND_ARB = $86A7;
  GL_MODELVIEW0_ARB = $1700;
  GL_MODELVIEW1_ARB = $850A;
  GL_MODELVIEW2_ARB = $8722;
  GL_MODELVIEW3_ARB = $8723;
  GL_MODELVIEW4_ARB = $8724;
  GL_MODELVIEW5_ARB = $8725;
  GL_MODELVIEW6_ARB = $8726;
  GL_MODELVIEW7_ARB = $8727;
  GL_MODELVIEW8_ARB = $8728;
  GL_MODELVIEW9_ARB = $8729;
  GL_MODELVIEW10_ARB = $872A;
  GL_MODELVIEW11_ARB = $872B;
  GL_MODELVIEW12_ARB = $872C;
  GL_MODELVIEW13_ARB = $872D;
  GL_MODELVIEW14_ARB = $872E;
  GL_MODELVIEW15_ARB = $872F;
  GL_MODELVIEW16_ARB = $8730;
  GL_MODELVIEW17_ARB = $8731;
  GL_MODELVIEW18_ARB = $8732;
  GL_MODELVIEW19_ARB = $8733;
  GL_MODELVIEW20_ARB = $8734;
  GL_MODELVIEW21_ARB = $8735;
  GL_MODELVIEW22_ARB = $8736;
  GL_MODELVIEW23_ARB = $8737;
  GL_MODELVIEW24_ARB = $8738;
  GL_MODELVIEW25_ARB = $8739;
  GL_MODELVIEW26_ARB = $873A;
  GL_MODELVIEW27_ARB = $873B;
  GL_MODELVIEW28_ARB = $873C;
  GL_MODELVIEW29_ARB = $873D;
  GL_MODELVIEW30_ARB = $873E;
  GL_MODELVIEW31_ARB = $873F;
  GL_CURRENT_WEIGHT_ARB = $86A8;
  GL_WEIGHT_ARRAY_TYPE_ARB = $86A9;
  GL_WEIGHT_ARRAY_STRIDE_ARB = $86AA;
  GL_WEIGHT_ARRAY_SIZE_ARB = $86AB;
  GL_WEIGHT_ARRAY_POINTER_ARB = $86AC;
  GL_WEIGHT_ARRAY_ARB = $86AD;
var
  glWeightbvARB: procedure(size: GLint; weights: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightsvARB: procedure(size: GLint; weights: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightivARB: procedure(size: GLint; weights: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightfvARB: procedure(size: GLint; weights: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightdvARB: procedure(size: GLint; weights: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightvARB: procedure(size: GLint; weights: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightubvARB: procedure(size: GLint; weights: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightusvARB: procedure(size: GLint; weights: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightuivARB: procedure(size: GLint; weights: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWeightPointerARB: procedure(size: GLint; _type: GLenum; stride: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexBlendARB: procedure(count: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_vertex_blend: Boolean;

//***** GL_ARB_vertex_program *****//
const
  GL_VERTEX_PROGRAM_ARB = $8620;
  GL_VERTEX_PROGRAM_POINT_SIZE_ARB = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_ARB = $8643;
  GL_COLOR_SUM_ARB = $8458;
  GL_PROGRAM_FORMAT_ASCII_ARB = $8875;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB = $886A;
  GL_CURRENT_VERTEX_ATTRIB_ARB = $8626;
  GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB = $8645;
  GL_PROGRAM_LENGTH_ARB = $8627;
  GL_PROGRAM_FORMAT_ARB = $8876;
  GL_PROGRAM_BINDING_ARB = $8677;
  GL_PROGRAM_INSTRUCTIONS_ARB = $88A0;
  GL_MAX_PROGRAM_INSTRUCTIONS_ARB = $88A1;
  GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A2;
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A3;
  GL_PROGRAM_TEMPORARIES_ARB = $88A4;
  GL_MAX_PROGRAM_TEMPORARIES_ARB = $88A5;
  GL_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A6;
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A7;
  GL_PROGRAM_PARAMETERS_ARB = $88A8;
  GL_MAX_PROGRAM_PARAMETERS_ARB = $88A9;
  GL_PROGRAM_NATIVE_PARAMETERS_ARB = $88AA;
  GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB = $88AB;
  GL_PROGRAM_ATTRIBS_ARB = $88AC;
  GL_MAX_PROGRAM_ATTRIBS_ARB = $88AD;
  GL_PROGRAM_NATIVE_ATTRIBS_ARB = $88AE;
  GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB = $88AF;
  GL_PROGRAM_ADDRESS_REGISTERS_ARB = $88B0;
  GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB = $88B1;
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B2;
  GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B3;
  GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB = $88B4;
  GL_MAX_PROGRAM_ENV_PARAMETERS_ARB = $88B5;
  GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB = $88B6;
  GL_PROGRAM_STRING_ARB = $8628;
  GL_PROGRAM_ERROR_POSITION_ARB = $864B;
  GL_CURRENT_MATRIX_ARB = $8641;
  GL_TRANSPOSE_CURRENT_MATRIX_ARB = $88B7;
  GL_CURRENT_MATRIX_STACK_DEPTH_ARB = $8640;
  GL_MAX_VERTEX_ATTRIBS_ARB = $8869;
  GL_MAX_PROGRAM_MATRICES_ARB = $862F;
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB = $862E;
  GL_PROGRAM_ERROR_STRING_ARB = $8874;
  GL_MATRIX0_ARB = $88C0;
  GL_MATRIX1_ARB = $88C1;
  GL_MATRIX2_ARB = $88C2;
  GL_MATRIX3_ARB = $88C3;
  GL_MATRIX4_ARB = $88C4;
  GL_MATRIX5_ARB = $88C5;
  GL_MATRIX6_ARB = $88C6;
  GL_MATRIX7_ARB = $88C7;
  GL_MATRIX8_ARB = $88C8;
  GL_MATRIX9_ARB = $88C9;
  GL_MATRIX10_ARB = $88CA;
  GL_MATRIX11_ARB = $88CB;
  GL_MATRIX12_ARB = $88CC;
  GL_MATRIX13_ARB = $88CD;
  GL_MATRIX14_ARB = $88CE;
  GL_MATRIX15_ARB = $88CF;
  GL_MATRIX16_ARB = $88D0;
  GL_MATRIX17_ARB = $88D1;
  GL_MATRIX18_ARB = $88D2;
  GL_MATRIX19_ARB = $88D3;
  GL_MATRIX20_ARB = $88D4;
  GL_MATRIX21_ARB = $88D5;
  GL_MATRIX22_ARB = $88D6;
  GL_MATRIX23_ARB = $88D7;
  GL_MATRIX24_ARB = $88D8;
  GL_MATRIX25_ARB = $88D9;
  GL_MATRIX26_ARB = $88DA;
  GL_MATRIX27_ARB = $88DB;
  GL_MATRIX28_ARB = $88DC;
  GL_MATRIX29_ARB = $88DD;
  GL_MATRIX30_ARB = $88DE;
  GL_MATRIX31_ARB = $88DF;
var
  glVertexAttrib1sARB: procedure(index: GLuint; x: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1fARB: procedure(index: GLuint; x: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1dARB: procedure(index: GLuint; x: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2sARB: procedure(index: GLuint; x: GLshort; y: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3sARB: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4sARB: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4NubARB: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1svARB: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1fvARB: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1dvARB: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2svARB: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2fvARB: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2dvARB: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3svARB: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3fvARB: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3dvARB: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4bvARB: procedure(index: GLuint; const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4svARB: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4ivARB: procedure(index: GLuint; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4ubvARB: procedure(index: GLuint; const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4usvARB: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4uivARB: procedure(index: GLuint; const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4fvARB: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4dvARB: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4NbvARB: procedure(index: GLuint; const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4NsvARB: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4NivARB: procedure(index: GLuint; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4NubvARB: procedure(index: GLuint; const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4NusvARB: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4NuivARB: procedure(index: GLuint; const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribPointerARB: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEnableVertexAttribArrayARB: procedure(index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDisableVertexAttribArrayARB: procedure(index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramStringARB: procedure(target: GLenum; format: GLenum; len: GLsizei; const _string: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindProgramARB: procedure(target: GLenum; _program: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteProgramsARB: procedure(n: GLsizei; const programs: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenProgramsARB: procedure(n: GLsizei; programs: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramEnvParameter4dARB: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramEnvParameter4dvARB: procedure(target: GLenum; index: GLuint; const params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramEnvParameter4fARB: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramEnvParameter4fvARB: procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramLocalParameter4dARB: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramLocalParameter4dvARB: procedure(target: GLenum; index: GLuint; const params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramLocalParameter4fARB: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramLocalParameter4fvARB: procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramEnvParameterdvARB: procedure(target: GLenum; index: GLuint; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramEnvParameterfvARB: procedure(target: GLenum; index: GLuint; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramLocalParameterdvARB: procedure(target: GLenum; index: GLuint; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramLocalParameterfvARB: procedure(target: GLenum; index: GLuint; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramStringARB: procedure(target: GLenum; pname: GLenum; _string: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribdvARB: procedure(index: GLuint; pname: GLenum; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribfvARB: procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribivARB: procedure(index: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribPointervARB: procedure(index: GLuint; pname: GLenum; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsProgramARB: function(_program: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_vertex_program: Boolean;

//***** GL_ARB_window_pos *****//
var
  glWindowPos2dARB: procedure(x: GLdouble; y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2fARB: procedure(x: GLfloat; y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2iARB: procedure(x: GLint; y: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2sARB: procedure(x: GLshort; y: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2dvARB: procedure(const p: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2fvARB: procedure(const p: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2ivARB: procedure(const p: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2svARB: procedure(const p: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3dARB: procedure(x: GLdouble; y: GLdouble; z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3fARB: procedure(x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3iARB: procedure(x: GLint; y: GLint; z: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3sARB: procedure(x: GLshort; y: GLshort; z: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3dvARB: procedure(const p: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3fvARB: procedure(const p: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3ivARB: procedure(const p: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3svARB: procedure(const p: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_window_pos: Boolean;

//***** GL_EXT_422_pixels *****//
const
  GL_422_EXT = $80CC;
  GL_422_REV_EXT = $80CD;
  GL_422_AVERAGE_EXT = $80CE;
  GL_422_REV_AVERAGE_EXT = $80CF;

function Load_GL_EXT_422_pixels: Boolean;

//***** GL_EXT_abgr *****//
const
  GL_ABGR_EXT = $8000;

function Load_GL_EXT_abgr: Boolean;

//***** GL_EXT_bgra *****//
const
  GL_BGR_EXT = $80E0;
  GL_BGRA_EXT = $80E1;

function Load_GL_EXT_bgra: Boolean;

//***** GL_EXT_blend_color *****//
const
  GL_CONSTANT_COLOR_EXT = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT = $8002;
  GL_CONSTANT_ALPHA_EXT = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT = $8004;
  GL_BLEND_COLOR_EXT = $8005;
var
  glBlendColorEXT: procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_blend_color: Boolean;

//***** GL_EXT_blend_func_separate *****//
const
  GL_BLEND_DST_RGB_EXT = $80C8;
  GL_BLEND_SRC_RGB_EXT = $80C9;
  GL_BLEND_DST_ALPHA_EXT = $80CA;
  GL_BLEND_SRC_ALPHA_EXT = $80CB;
var
  glBlendFuncSeparateEXT: procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_blend_func_separate: Boolean;

//***** GL_EXT_blend_logic_op *****//

function Load_GL_EXT_blend_logic_op: Boolean;

//***** GL_EXT_blend_minmax *****//
const
  GL_FUNC_ADD_EXT = $8006;
  GL_MIN_EXT = $8007;
  GL_MAX_EXT = $8008;
  GL_BLEND_EQUATION_EXT = $8009;
var
  glBlendEquationEXT: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_blend_minmax: Boolean;

//***** GL_EXT_blend_subtract *****//
const
  GL_FUNC_SUBTRACT_EXT = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT = $800B;

function Load_GL_EXT_blend_subtract: Boolean;

//***** GL_EXT_clip_volume_hint *****//
const
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT = $80F0;

function Load_GL_EXT_clip_volume_hint: Boolean;

//***** GL_EXT_color_subtable *****//
var
  glColorSubTableEXT: procedure(target: GLenum; start: GLsizei; count: GLsizei; format: GLenum; _type: GLenum; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyColorSubTableEXT: procedure(target: GLenum; start: GLsizei; x: GLint; y: GLint; width: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_color_subtable: Boolean;

//***** GL_EXT_compiled_vertex_array *****//
const
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT = $81A8;
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT = $81A9;
var
  glLockArraysEXT: procedure(first: GLint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUnlockArraysEXT: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_compiled_vertex_array: Boolean;

//***** GL_EXT_convolution *****//
const
  GL_CONVOLUTION_1D_EXT = $8010;
  GL_CONVOLUTION_2D_EXT = $8011;
  GL_SEPARABLE_2D_EXT = $8012;
  GL_CONVOLUTION_BORDER_MODE_EXT = $8013;
  GL_CONVOLUTION_FILTER_SCALE_EXT = $8014;
  GL_CONVOLUTION_FILTER_BIAS_EXT = $8015;
  GL_REDUCE_EXT = $8016;
  GL_CONVOLUTION_FORMAT_EXT = $8017;
  GL_CONVOLUTION_WIDTH_EXT = $8018;
  GL_CONVOLUTION_HEIGHT_EXT = $8019;
  GL_MAX_CONVOLUTION_WIDTH_EXT = $801A;
  GL_MAX_CONVOLUTION_HEIGHT_EXT = $801B;
  GL_POST_CONVOLUTION_RED_SCALE_EXT = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE_EXT = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE_EXT = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE_EXT = $801F;
  GL_POST_CONVOLUTION_RED_BIAS_EXT = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS_EXT = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS_EXT = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS_EXT = $8023;
var
  glConvolutionFilter1DEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const image: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionFilter2DEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const image: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyConvolutionFilter1DEXT: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyConvolutionFilter2DEXT: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetConvolutionFilterEXT: procedure(target: GLenum; format: GLenum; _type: GLenum; image: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSeparableFilter2DEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const row: PGLvoid; const column: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetSeparableFilterEXT: procedure(target: GLenum; format: GLenum; _type: GLenum; row: PGLvoid; column: PGLvoid; span: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionParameteriEXT: procedure(target: GLenum; pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionParameterivEXT: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionParameterfEXT: procedure(target: GLenum; pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glConvolutionParameterfvEXT: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetConvolutionParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetConvolutionParameterfvEXT: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_convolution: Boolean;

//***** GL_EXT_fog_coord *****//
const
  GL_FOG_COORDINATE_SOURCE_EXT = $8450;
  GL_FOG_COORDINATE_EXT = $8451;
  GL_FRAGMENT_DEPTH_EXT = $8452;
  GL_CURRENT_FOG_COORDINATE_EXT = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT = $8456;
  GL_FOG_COORDINATE_ARRAY_EXT = $8457;
var
  glFogCoordfEXT: procedure(coord: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoorddEXT: procedure(coord: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordfvEXT: procedure(coord: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoorddvEXT: procedure(coord: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordPointerEXT: procedure(_type: GLenum; stride: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_fog_coord: Boolean;

//***** GL_EXT_histogram *****//
const
  GL_HISTOGRAM_EXT = $8024;
  GL_PROXY_HISTOGRAM_EXT = $8025;
  GL_HISTOGRAM_WIDTH_EXT = $8026;
  GL_HISTOGRAM_FORMAT_EXT = $8027;
  GL_HISTOGRAM_RED_SIZE_EXT = $8028;
  GL_HISTOGRAM_GREEN_SIZE_EXT = $8029;
  GL_HISTOGRAM_BLUE_SIZE_EXT = $802A;
  GL_HISTOGRAM_ALPHA_SIZE_EXT = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE_EXT = $802C;
  GL_HISTOGRAM_SINK_EXT = $802D;
  GL_MINMAX_EXT = $802E;
  GL_MINMAX_FORMAT_EXT = $802F;
  GL_MINMAX_SINK_EXT = $8030;
var
  glHistogramEXT: procedure(target: GLenum; width: GLsizei; internalformat: GLenum; sink: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glResetHistogramEXT: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetHistogramEXT: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetHistogramParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetHistogramParameterfvEXT: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMinmaxEXT: procedure(target: GLenum; internalformat: GLenum; sink: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glResetMinmaxEXT: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMinmaxEXT: procedure(target: GLenum; reset: GLboolean; format: GLenum; _type: GLenum; values: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMinmaxParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMinmaxParameterfvEXT: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_histogram: Boolean;

//***** GL_EXT_multi_draw_arrays *****//
var
  glMultiDrawArraysEXT: procedure(mode: GLenum; first: PGLint; count: PGLsizei; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiDrawElementsEXT: procedure(mode: GLenum; count: PGLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_multi_draw_arrays: Boolean;

//***** GL_EXT_packed_depth_stencil *****//
const
  GL_DEPTH_STENCIL_EXT = $84F9;
  GL_UNSIGNED_INT_24_8_EXT = $84FA;
  GL_DEPTH24_STENCIL8_EXT = $88F0;
  GL_TEXTURE_STENCIL_SIZE_EXT = $88F1;

function Load_GL_EXT_packed_depth_stencil: Boolean;

//***** GL_EXT_packed_pixels *****//
const
  GL_UNSIGNED_BYTE_3_3_2_EXT = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4_EXT = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1_EXT = $8034;
  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
  GL_UNSIGNED_INT_10_10_10_2_EXT = $8036;

function Load_GL_EXT_packed_pixels: Boolean;

//***** GL_EXT_paletted_texture *****//
const
  GL_COLOR_INDEX1_EXT = $80E2;
  GL_COLOR_INDEX2_EXT = $80E3;
  GL_COLOR_INDEX4_EXT = $80E4;
  GL_COLOR_INDEX8_EXT = $80E5;
  GL_COLOR_INDEX12_EXT = $80E6;
  GL_COLOR_INDEX16_EXT = $80E7;
  GL_COLOR_TABLE_FORMAT_EXT = $80D8;
  GL_COLOR_TABLE_WIDTH_EXT = $80D9;
  GL_COLOR_TABLE_RED_SIZE_EXT = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_EXT = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_EXT = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_EXT = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT = $80DF;
  GL_TEXTURE_INDEX_SIZE_EXT = $80ED;
// GL_TEXTURE_1D = $0DE0;
//  GL_TEXTURE_2D = $0DE1;
  GL_TEXTURE_3D_EXT = $806F;
  // GL_TEXTURE_CUBE_MAP_ARB  { already defined }
// GL_PROXY_TEXTURE_1D = $8063;
// GL_PROXY_TEXTURE_2D = $8064;
  GL_PROXY_TEXTURE_3D_EXT = $8070;
  // GL_PROXY_TEXTURE_CUBE_MAP_ARB  { already defined }
  // GL_TEXTURE_1D  { already defined }
  // GL_TEXTURE_2D  { already defined }
  // GL_TEXTURE_3D_EXT  { already defined }
  // GL_TEXTURE_CUBE_MAP_ARB  { already defined }
var
  glColorTableEXT: procedure(target: GLenum; internalFormat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  // glColorSubTableEXT  { already defined }
  glGetColorTableEXT: procedure(target: GLenum; format: GLenum; _type: GLenum; data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetColorTableParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetColorTableParameterfvEXT: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_paletted_texture: Boolean;

//***** GL_EXT_point_parameters *****//
const
  GL_POINT_SIZE_MIN_EXT = $8126;
  GL_POINT_SIZE_MAX_EXT = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT = $8128;
  GL_DISTANCE_ATTENUATION_EXT = $8129;
var
  glPointParameterfEXT: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPointParameterfvEXT: procedure(pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_point_parameters: Boolean;

//***** GL_EXT_polygon_offset *****//
const
  GL_POLYGON_OFFSET_EXT = $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT = $8038;
  GL_POLYGON_OFFSET_BIAS_EXT = $8039;
var
  glPolygonOffsetEXT: procedure(factor: GLfloat; bias: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_polygon_offset: Boolean;

//***** GL_EXT_secondary_color *****//
const
  GL_COLOR_SUM_EXT = $8458;
  GL_CURRENT_SECONDARY_COLOR_EXT = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT = $845D;
  GL_SECONDARY_COLOR_ARRAY_EXT = $845E;
var
  glSecondaryColor3bEXT: procedure(components: GLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3sEXT: procedure(components: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3iEXT: procedure(components: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3fEXT: procedure(components: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3dEXT: procedure(components: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3ubEXT: procedure(components: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3usEXT: procedure(components: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3uiEXT: procedure(components: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3bvEXT: procedure(components: GLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3svEXT: procedure(components: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3ivEXT: procedure(components: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3fvEXT: procedure(components: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3dvEXT: procedure(components: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3ubvEXT: procedure(components: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3usvEXT: procedure(components: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3uivEXT: procedure(components: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColorPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_secondary_color: Boolean;

//***** GL_EXT_separate_specular_color *****//
const
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT = $81F8;
  GL_SINGLE_COLOR_EXT = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT = $81FA;

function Load_GL_EXT_separate_specular_color: Boolean;

//***** GL_EXT_shadow_funcs *****//

function Load_GL_EXT_shadow_funcs: Boolean;

//***** GL_EXT_shared_texture_palette *****//
const
  GL_SHARED_TEXTURE_PALETTE_EXT = $81FB;

function Load_GL_EXT_shared_texture_palette: Boolean;

//***** GL_EXT_stencil_two_side *****//
const
  GL_STENCIL_TEST_TWO_SIDE_EXT = $8910;
  GL_ACTIVE_STENCIL_FACE_EXT = $8911;
var
  glActiveStencilFaceEXT: procedure(face: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_stencil_two_side: Boolean;

//***** GL_EXT_stencil_wrap *****//
const
  GL_INCR_WRAP_EXT = $8507;
  GL_DECR_WRAP_EXT = $8508;

function Load_GL_EXT_stencil_wrap: Boolean;

//***** GL_EXT_subtexture *****//
var
  glTexSubImage1DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexSubImage2DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexSubImage3DEXT: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_subtexture: Boolean;

//***** GL_EXT_texture3D *****//
const
  GL_PACK_SKIP_IMAGES_EXT = $806B;
  GL_PACK_IMAGE_HEIGHT_EXT = $806C;
  GL_UNPACK_SKIP_IMAGES_EXT = $806D;
  GL_UNPACK_IMAGE_HEIGHT_EXT = $806E;
  // GL_TEXTURE_3D_EXT  { already defined }
  // GL_PROXY_TEXTURE_3D_EXT  { already defined }
  GL_TEXTURE_DEPTH_EXT = $8071;
  GL_TEXTURE_WRAP_R_EXT = $8072;
  GL_MAX_3D_TEXTURE_SIZE_EXT = $8073;
var
  glTexImage3DEXT: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_texture3D: Boolean;

//***** GL_EXT_texture_compression_s3tc *****//
const
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;

function Load_GL_EXT_texture_compression_s3tc: Boolean;

//***** GL_EXT_texture_env_add *****//

function Load_GL_EXT_texture_env_add: Boolean;

//***** GL_EXT_texture_env_combine *****//
const
  GL_COMBINE_EXT = $8570;
  GL_COMBINE_RGB_EXT = $8571;
  GL_COMBINE_ALPHA_EXT = $8572;
  GL_SOURCE0_RGB_EXT = $8580;
  GL_SOURCE1_RGB_EXT = $8581;
  GL_SOURCE2_RGB_EXT = $8582;
  GL_SOURCE0_ALPHA_EXT = $8588;
  GL_SOURCE1_ALPHA_EXT = $8589;
  GL_SOURCE2_ALPHA_EXT = $858A;
  GL_OPERAND0_RGB_EXT = $8590;
  GL_OPERAND1_RGB_EXT = $8591;
  GL_OPERAND2_RGB_EXT = $8592;
  GL_OPERAND0_ALPHA_EXT = $8598;
  GL_OPERAND1_ALPHA_EXT = $8599;
  GL_OPERAND2_ALPHA_EXT = $859A;
  GL_RGB_SCALE_EXT = $8573;
  GL_ADD_SIGNED_EXT = $8574;
  GL_INTERPOLATE_EXT = $8575;
  GL_CONSTANT_EXT = $8576;
  GL_PRIMARY_COLOR_EXT = $8577;
  GL_PREVIOUS_EXT = $8578;

function Load_GL_EXT_texture_env_combine: Boolean;

//***** GL_EXT_texture_env_dot3 *****//
const
  GL_DOT3_RGB_EXT = $8740;
  GL_DOT3_RGBA_EXT = $8741;

function Load_GL_EXT_texture_env_dot3: Boolean;

//***** GL_EXT_texture_filter_anisotropic *****//
const
  GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;

function Load_GL_EXT_texture_filter_anisotropic: Boolean;

//***** GL_EXT_texture_lod_bias *****//
const
  GL_TEXTURE_FILTER_CONTROL_EXT = $8500;
  GL_TEXTURE_LOD_BIAS_EXT = $8501;
  GL_MAX_TEXTURE_LOD_BIAS_EXT = $84FD;

function Load_GL_EXT_texture_lod_bias: Boolean;

//***** GL_EXT_texture_object *****//
const
  GL_TEXTURE_PRIORITY_EXT = $8066;
  GL_TEXTURE_RESIDENT_EXT = $8067;
  GL_TEXTURE_1D_BINDING_EXT = $8068;
  GL_TEXTURE_2D_BINDING_EXT = $8069;
  GL_TEXTURE_3D_BINDING_EXT = $806A;
var
  glGenTexturesEXT: procedure(n: GLsizei; textures: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteTexturesEXT: procedure(n: GLsizei; const textures: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindTextureEXT: procedure(target: GLenum; texture: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPrioritizeTexturesEXT: procedure(n: GLsizei; const textures: PGLuint; const priorities: PGLclampf); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAreTexturesResidentEXT: function(n: GLsizei; const textures: PGLuint; residences: PGLboolean): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsTextureEXT: function(texture: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_texture_object: Boolean;

//***** GL_EXT_vertex_array *****//
const
  GL_VERTEX_ARRAY_EXT = $8074;
  GL_NORMAL_ARRAY_EXT = $8075;
  GL_COLOR_ARRAY_EXT = $8076;
  GL_INDEX_ARRAY_EXT = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT = $8078;
  GL_EDGE_FLAG_ARRAY_EXT = $8079;
  GL_DOUBLE_EXT = $140A;
  GL_VERTEX_ARRAY_SIZE_EXT = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT = $8080;
  GL_COLOR_ARRAY_SIZE_EXT = $8081;
  GL_COLOR_ARRAY_TYPE_EXT = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT = $8083;
  GL_COLOR_ARRAY_COUNT_EXT = $8084;
  GL_INDEX_ARRAY_TYPE_EXT = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT = $8086;
  GL_INDEX_ARRAY_COUNT_EXT = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT = $808F;
  GL_COLOR_ARRAY_POINTER_EXT = $8090;
  GL_INDEX_ARRAY_POINTER_EXT = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT = $8093;
var
  glArrayElementEXT: procedure(i: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawArraysEXT: procedure(mode: GLenum; first: GLint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; count: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalPointerEXT: procedure(_type: GLenum; stride: GLsizei; count: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; count: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIndexPointerEXT: procedure(_type: GLenum; stride: GLsizei; count: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; count: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEdgeFlagPointerEXT: procedure(stride: GLsizei; count: GLsizei; const pointer: PGLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetPointervEXT: procedure(pname: GLenum; params: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_vertex_array: Boolean;

//***** GL_EXT_vertex_shader *****//
const
  GL_VERTEX_SHADER_EXT = $8780;
  GL_VARIANT_VALUE_EXT = $87E4;
  GL_VARIANT_DATATYPE_EXT = $87E5;
  GL_VARIANT_ARRAY_STRIDE_EXT = $87E6;
  GL_VARIANT_ARRAY_TYPE_EXT = $87E7;
  GL_VARIANT_ARRAY_EXT = $87E8;
  GL_VARIANT_ARRAY_POINTER_EXT = $87E9;
  GL_INVARIANT_VALUE_EXT = $87EA;
  GL_INVARIANT_DATATYPE_EXT = $87EB;
  GL_LOCAL_CONSTANT_VALUE_EXT = $87EC;
  GL_LOCAL_CONSTANT_DATATYPE_EXT = $87ED;
  GL_OP_INDEX_EXT = $8782;
  GL_OP_NEGATE_EXT = $8783;
  GL_OP_DOT3_EXT = $8784;
  GL_OP_DOT4_EXT = $8785;
  GL_OP_MUL_EXT = $8786;
  GL_OP_ADD_EXT = $8787;
  GL_OP_MADD_EXT = $8788;
  GL_OP_FRAC_EXT = $8789;
  GL_OP_MAX_EXT = $878A;
  GL_OP_MIN_EXT = $878B;
  GL_OP_SET_GE_EXT = $878C;
  GL_OP_SET_LT_EXT = $878D;
  GL_OP_CLAMP_EXT = $878E;
  GL_OP_FLOOR_EXT = $878F;
  GL_OP_ROUND_EXT = $8790;
  GL_OP_EXP_BASE_2_EXT = $8791;
  GL_OP_LOG_BASE_2_EXT = $8792;
  GL_OP_POWER_EXT = $8793;
  GL_OP_RECIP_EXT = $8794;
  GL_OP_RECIP_SQRT_EXT = $8795;
  GL_OP_SUB_EXT = $8796;
  GL_OP_CROSS_PRODUCT_EXT = $8797;
  GL_OP_MULTIPLY_MATRIX_EXT = $8798;
  GL_OP_MOV_EXT = $8799;
  GL_OUTPUT_VERTEX_EXT = $879A;
  GL_OUTPUT_COLOR0_EXT = $879B;
  GL_OUTPUT_COLOR1_EXT = $879C;
  GL_OUTPUT_TEXTURE_COORD0_EXT = $879D;
  GL_OUTPUT_TEXTURE_COORD1_EXT = $879E;
  GL_OUTPUT_TEXTURE_COORD2_EXT = $879F;
  GL_OUTPUT_TEXTURE_COORD3_EXT = $87A0;
  GL_OUTPUT_TEXTURE_COORD4_EXT = $87A1;
  GL_OUTPUT_TEXTURE_COORD5_EXT = $87A2;
  GL_OUTPUT_TEXTURE_COORD6_EXT = $87A3;
  GL_OUTPUT_TEXTURE_COORD7_EXT = $87A4;
  GL_OUTPUT_TEXTURE_COORD8_EXT = $87A5;
  GL_OUTPUT_TEXTURE_COORD9_EXT = $87A6;
  GL_OUTPUT_TEXTURE_COORD10_EXT = $87A7;
  GL_OUTPUT_TEXTURE_COORD11_EXT = $87A8;
  GL_OUTPUT_TEXTURE_COORD12_EXT = $87A9;
  GL_OUTPUT_TEXTURE_COORD13_EXT = $87AA;
  GL_OUTPUT_TEXTURE_COORD14_EXT = $87AB;
  GL_OUTPUT_TEXTURE_COORD15_EXT = $87AC;
  GL_OUTPUT_TEXTURE_COORD16_EXT = $87AD;
  GL_OUTPUT_TEXTURE_COORD17_EXT = $87AE;
  GL_OUTPUT_TEXTURE_COORD18_EXT = $87AF;
  GL_OUTPUT_TEXTURE_COORD19_EXT = $87B0;
  GL_OUTPUT_TEXTURE_COORD20_EXT = $87B1;
  GL_OUTPUT_TEXTURE_COORD21_EXT = $87B2;
  GL_OUTPUT_TEXTURE_COORD22_EXT = $87B3;
  GL_OUTPUT_TEXTURE_COORD23_EXT = $87B4;
  GL_OUTPUT_TEXTURE_COORD24_EXT = $87B5;
  GL_OUTPUT_TEXTURE_COORD25_EXT = $87B6;
  GL_OUTPUT_TEXTURE_COORD26_EXT = $87B7;
  GL_OUTPUT_TEXTURE_COORD27_EXT = $87B8;
  GL_OUTPUT_TEXTURE_COORD28_EXT = $87B9;
  GL_OUTPUT_TEXTURE_COORD29_EXT = $87BA;
  GL_OUTPUT_TEXTURE_COORD30_EXT = $87BB;
  GL_OUTPUT_TEXTURE_COORD31_EXT = $87BC;
  GL_OUTPUT_FOG_EXT = $87BD;
  GL_SCALAR_EXT = $87BE;
  GL_VECTOR_EXT = $87BF;
  GL_MATRIX_EXT = $87C0;
  GL_VARIANT_EXT = $87C1;
  GL_INVARIANT_EXT = $87C2;
  GL_LOCAL_CONSTANT_EXT = $87C3;
  GL_LOCAL_EXT = $87C4;
  GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT = $87C5;
  GL_MAX_VERTEX_SHADER_VARIANTS_EXT = $87C6;
  GL_MAX_VERTEX_SHADER_INVARIANTS_EXT = $87C7;
  GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87C8;
  GL_MAX_VERTEX_SHADER_LOCALS_EXT = $87C9;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT = $87CA;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT = $87CB;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87CC;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT = $87CD;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT = $87CE;
  GL_VERTEX_SHADER_INSTRUCTIONS_EXT = $87CF;
  GL_VERTEX_SHADER_VARIANTS_EXT = $87D0;
  GL_VERTEX_SHADER_INVARIANTS_EXT = $87D1;
  GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87D2;
  GL_VERTEX_SHADER_LOCALS_EXT = $87D3;
  GL_VERTEX_SHADER_BINDING_EXT = $8781;
  GL_VERTEX_SHADER_OPTIMIZED_EXT = $87D4;
  GL_X_EXT = $87D5;
  GL_Y_EXT = $87D6;
  GL_Z_EXT = $87D7;
  GL_W_EXT = $87D8;
  GL_NEGATIVE_X_EXT = $87D9;
  GL_NEGATIVE_Y_EXT = $87DA;
  GL_NEGATIVE_Z_EXT = $87DB;
  GL_NEGATIVE_W_EXT = $87DC;
  GL_ZERO_EXT = $87DD;
  GL_ONE_EXT = $87DE;
  GL_NEGATIVE_ONE_EXT = $87DF;
  GL_NORMALIZED_RANGE_EXT = $87E0;
  GL_FULL_RANGE_EXT = $87E1;
  GL_CURRENT_VERTEX_EXT = $87E2;
  GL_MVP_MATRIX_EXT = $87E3;
var
  glBeginVertexShaderEXT: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndVertexShaderEXT: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindVertexShaderEXT: procedure(id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenVertexShadersEXT: function(range: GLuint): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteVertexShaderEXT: procedure(id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glShaderOp1EXT: procedure(op: GLenum; res: GLuint; arg1: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glShaderOp2EXT: procedure(op: GLenum; res: GLuint; arg1: GLuint; arg2: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glShaderOp3EXT: procedure(op: GLenum; res: GLuint; arg1: GLuint; arg2: GLuint; arg3: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSwizzleEXT: procedure(res: GLuint; _in: GLuint; outX: GLenum; outY: GLenum; outZ: GLenum; outW: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWriteMaskEXT: procedure(res: GLuint; _in: GLuint; outX: GLenum; outY: GLenum; outZ: GLenum; outW: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glInsertComponentEXT: procedure(res: GLuint; src: GLuint; num: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glExtractComponentEXT: procedure(res: GLuint; src: GLuint; num: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenSymbolsEXT: function(datatype: GLenum; storagetype: GLenum; range: GLenum; components: GLuint): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSetInvariantEXT: procedure(id: GLuint; _type: GLenum; addr: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSetLocalConstantEXT: procedure(id: GLuint; _type: GLenum; addr: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantbvEXT: procedure(id: GLuint; addr: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantsvEXT: procedure(id: GLuint; addr: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantivEXT: procedure(id: GLuint; addr: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantfvEXT: procedure(id: GLuint; addr: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantdvEXT: procedure(id: GLuint; addr: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantubvEXT: procedure(id: GLuint; addr: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantusvEXT: procedure(id: GLuint; addr: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantuivEXT: procedure(id: GLuint; addr: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantPointerEXT: procedure(id: GLuint; _type: GLenum; stride: GLuint; addr: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEnableVariantClientStateEXT: procedure(id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDisableVariantClientStateEXT: procedure(id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindLightParameterEXT: function(light: GLenum; value: GLenum): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindMaterialParameterEXT: function(face: GLenum; value: GLenum): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindTexGenParameterEXT: function(_unit: GLenum; coord: GLenum; value: GLenum): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindTextureUnitParameterEXT: function(_unit: GLenum; value: GLenum): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindParameterEXT: function(value: GLenum): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsVariantEnabledEXT: function(id: GLuint; cap: GLenum): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVariantBooleanvEXT: procedure(id: GLuint; value: GLenum; data: PGLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVariantIntegervEXT: procedure(id: GLuint; value: GLenum; data: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVariantFloatvEXT: procedure(id: GLuint; value: GLenum; data: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVariantPointervEXT: procedure(id: GLuint; value: GLenum; data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetInvariantBooleanvEXT: procedure(id: GLuint; value: GLenum; data: PGLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetInvariantIntegervEXT: procedure(id: GLuint; value: GLenum; data: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetInvariantFloatvEXT: procedure(id: GLuint; value: GLenum; data: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetLocalConstantBooleanvEXT: procedure(id: GLuint; value: GLenum; data: PGLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetLocalConstantIntegervEXT: procedure(id: GLuint; value: GLenum; data: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetLocalConstantFloatvEXT: procedure(id: GLuint; value: GLenum; data: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_vertex_shader: Boolean;

//***** GL_EXT_vertex_weighting *****//
const
  GL_VERTEX_WEIGHTING_EXT = $8509;
  GL_MODELVIEW0_EXT = $1700;
  GL_MODELVIEW1_EXT = $850A;
  GL_MODELVIEW0_MATRIX_EXT = $0BA6;
  GL_MODELVIEW1_MATRIX_EXT = $8506;
  GL_CURRENT_VERTEX_WEIGHT_EXT = $850B;
  GL_VERTEX_WEIGHT_ARRAY_EXT = $850C;
  GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT = $850D;
  GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT = $850E;
  GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT = $850F;
  GL_MODELVIEW0_STACK_DEPTH_EXT = $0BA3;
  GL_MODELVIEW1_STACK_DEPTH_EXT = $8502;
  GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT = $8510;
var
  glVertexWeightfEXT: procedure(weight: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexWeightfvEXT: procedure(weight: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexWeightPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_vertex_weighting: Boolean;

//***** GL_HP_occlusion_test *****//
const
  GL_OCCLUSION_TEST_HP = $8165;
  GL_OCCLUSION_TEST_RESULT_HP = $8166;

function Load_GL_HP_occlusion_test: Boolean;

//***** GL_NV_blend_square *****//

function Load_GL_NV_blend_square: Boolean;

//***** GL_NV_copy_depth_to_color *****//
const
  GL_DEPTH_STENCIL_TO_RGBA_NV = $886E;
  GL_DEPTH_STENCIL_TO_BGRA_NV = $886F;

function Load_GL_NV_copy_depth_to_color: Boolean;

//***** GL_NV_depth_clamp *****//
const
  GL_DEPTH_CLAMP_NV = $864F;

function Load_GL_NV_depth_clamp: Boolean;

//***** GL_NV_evaluators *****//
const
  GL_EVAL_2D_NV = $86C0;
  GL_EVAL_TRIANGULAR_2D_NV = $86C1;
  GL_MAP_TESSELLATION_NV = $86C2;
  GL_MAP_ATTRIB_U_ORDER_NV = $86C3;
  GL_MAP_ATTRIB_V_ORDER_NV = $86C4;
  GL_EVAL_FRACTIONAL_TESSELLATION_NV = $86C5;
  GL_EVAL_VERTEX_ATTRIB0_NV = $86C6;
  GL_EVAL_VERTEX_ATTRIB1_NV = $86C7;
  GL_EVAL_VERTEX_ATTRIB2_NV = $86C8;
  GL_EVAL_VERTEX_ATTRIB3_NV = $86C9;
  GL_EVAL_VERTEX_ATTRIB4_NV = $86CA;
  GL_EVAL_VERTEX_ATTRIB5_NV = $86CB;
  GL_EVAL_VERTEX_ATTRIB6_NV = $86CC;
  GL_EVAL_VERTEX_ATTRIB7_NV = $86CD;
  GL_EVAL_VERTEX_ATTRIB8_NV = $86CE;
  GL_EVAL_VERTEX_ATTRIB9_NV = $86CF;
  GL_EVAL_VERTEX_ATTRIB10_NV = $86D0;
  GL_EVAL_VERTEX_ATTRIB11_NV = $86D1;
  GL_EVAL_VERTEX_ATTRIB12_NV = $86D2;
  GL_EVAL_VERTEX_ATTRIB13_NV = $86D3;
  GL_EVAL_VERTEX_ATTRIB14_NV = $86D4;
  GL_EVAL_VERTEX_ATTRIB15_NV = $86D5;
  GL_MAX_MAP_TESSELLATION_NV = $86D6;
  GL_MAX_RATIONAL_EVAL_ORDER_NV = $86D7;
var
  glMapControlPointsNV: procedure(target: GLenum; index: GLuint; _type: GLenum; ustride: GLsizei; vstride: GLsizei; uorder: GLint; vorder: GLint; _packed: GLboolean; const points: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMapParameterivNV: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMapParameterfvNV: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMapControlPointsNV: procedure(target: GLenum; index: GLuint; _type: GLenum; ustride: GLsizei; vstride: GLsizei; _packed: GLboolean; points: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMapParameterivNV: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMapParameterfvNV: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMapAttribParameterivNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMapAttribParameterfvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEvalMapsNV: procedure(target: GLenum; mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_evaluators: Boolean;

//***** GL_NV_fence *****//
const
  GL_ALL_COMPLETED_NV = $84F2;
  GL_FENCE_STATUS_NV = $84F3;
  GL_FENCE_CONDITION_NV = $84F4;
var
  glGenFencesNV: procedure(n: GLsizei; fences: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteFencesNV: procedure(n: GLsizei; const fences: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSetFenceNV: procedure(fence: GLuint; condition: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTestFenceNV: function(fence: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFinishFenceNV: procedure(fence: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsFenceNV: function(fence: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetFenceivNV: procedure(fence: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_fence: Boolean;

//***** GL_NV_fog_distance *****//
const
  GL_FOG_DISTANCE_MODE_NV = $855A;
  GL_EYE_RADIAL_NV = $855B;
  GL_EYE_PLANE_ABSOLUTE_NV = $855C;

function Load_GL_NV_fog_distance: Boolean;

//***** GL_NV_light_max_exponent *****//
const
  GL_MAX_SHININESS_NV = $8504;
  GL_MAX_SPOT_EXPONENT_NV = $8505;

function Load_GL_NV_light_max_exponent: Boolean;

//***** GL_NV_multisample_filter_hint *****//
const
  GL_MULTISAMPLE_FILTER_HINT_NV = $8534;

function Load_GL_NV_multisample_filter_hint: Boolean;

//***** GL_NV_occlusion_query *****//
  // GL_OCCLUSION_TEST_HP  { already defined }
  // GL_OCCLUSION_TEST_RESULT_HP  { already defined }
const
  GL_PIXEL_COUNTER_BITS_NV = $8864;
  GL_CURRENT_OCCLUSION_QUERY_ID_NV = $8865;
  GL_PIXEL_COUNT_NV = $8866;
  GL_PIXEL_COUNT_AVAILABLE_NV = $8867;
var
  glGenOcclusionQueriesNV: procedure(n: GLsizei; ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteOcclusionQueriesNV: procedure(n: GLsizei; const ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsOcclusionQueryNV: function(id: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBeginOcclusionQueryNV: procedure(id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndOcclusionQueryNV: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetOcclusionQueryivNV: procedure(id: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetOcclusionQueryuivNV: procedure(id: GLuint; pname: GLenum; params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_occlusion_query: Boolean;

//***** GL_NV_packed_depth_stencil *****//
const
  GL_DEPTH_STENCIL_NV = $84F9;
  GL_UNSIGNED_INT_24_8_NV = $84FA;

function Load_GL_NV_packed_depth_stencil: Boolean;

//***** GL_NV_point_sprite *****//
const
  GL_POINT_SPRITE_NV = $8861;
  GL_COORD_REPLACE_NV = $8862;
  GL_POINT_SPRITE_R_MODE_NV = $8863;
var
  glPointParameteriNV: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPointParameterivNV: procedure(pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_point_sprite: Boolean;

//***** GL_NV_register_combiners *****//
const
  GL_REGISTER_COMBINERS_NV = $8522;
  GL_COMBINER0_NV = $8550;
  GL_COMBINER1_NV = $8551;
  GL_COMBINER2_NV = $8552;
  GL_COMBINER3_NV = $8553;
  GL_COMBINER4_NV = $8554;
  GL_COMBINER5_NV = $8555;
  GL_COMBINER6_NV = $8556;
  GL_COMBINER7_NV = $8557;
  GL_VARIABLE_A_NV = $8523;
  GL_VARIABLE_B_NV = $8524;
  GL_VARIABLE_C_NV = $8525;
  GL_VARIABLE_D_NV = $8526;
  GL_VARIABLE_E_NV = $8527;
  GL_VARIABLE_F_NV = $8528;
  GL_VARIABLE_G_NV = $8529;
  GL_CONSTANT_COLOR0_NV = $852A;
  GL_CONSTANT_COLOR1_NV = $852B;
  GL_PRIMARY_COLOR_NV = $852C;
  GL_SECONDARY_COLOR_NV = $852D;
  GL_SPARE0_NV = $852E;
  GL_SPARE1_NV = $852F;
  GL_UNSIGNED_IDENTITY_NV = $8536;
  GL_UNSIGNED_INVERT_NV = $8537;
  GL_EXPAND_NORMAL_NV = $8538;
  GL_EXPAND_NEGATE_NV = $8539;
  GL_HALF_BIAS_NORMAL_NV = $853A;
  GL_HALF_BIAS_NEGATE_NV = $853B;
  GL_SIGNED_IDENTITY_NV = $853C;
  GL_SIGNED_NEGATE_NV = $853D;
  GL_E_TIMES_F_NV = $8531;
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV = $8532;
  GL_SCALE_BY_TWO_NV = $853E;
  GL_SCALE_BY_FOUR_NV = $853F;
  GL_SCALE_BY_ONE_HALF_NV = $8540;
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV = $8541;
  GL_DISCARD_NV = $8530;
  GL_COMBINER_INPUT_NV = $8542;
  GL_COMBINER_MAPPING_NV = $8543;
  GL_COMBINER_COMPONENT_USAGE_NV = $8544;
  GL_COMBINER_AB_DOT_PRODUCT_NV = $8545;
  GL_COMBINER_CD_DOT_PRODUCT_NV = $8546;
  GL_COMBINER_MUX_SUM_NV = $8547;
  GL_COMBINER_SCALE_NV = $8548;
  GL_COMBINER_BIAS_NV = $8549;
  GL_COMBINER_AB_OUTPUT_NV = $854A;
  GL_COMBINER_CD_OUTPUT_NV = $854B;
  GL_COMBINER_SUM_OUTPUT_NV = $854C;
  GL_NUM_GENERAL_COMBINERS_NV = $854E;
  GL_COLOR_SUM_CLAMP_NV = $854F;
  GL_MAX_GENERAL_COMBINERS_NV = $854D;
var
  glCombinerParameterfvNV: procedure(pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCombinerParameterivNV: procedure(pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCombinerParameterfNV: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCombinerParameteriNV: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCombinerInputNV: procedure(stage: GLenum; portion: GLenum; variable: GLenum; input: GLenum; mapping: GLenum; componentUsage: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCombinerOutputNV: procedure(stage: GLenum; portion: GLenum; abOutput: GLenum; cdOutput: GLenum; sumOutput: GLenum; scale: GLenum; bias: GLenum; abDotProduct: GLboolean; cdDotProduct: GLboolean; muxSum: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFinalCombinerInputNV: procedure(variable: GLenum; input: GLenum; mapping: GLenum; componentUsage: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetCombinerInputParameterfvNV: procedure(stage: GLenum; portion: GLenum; variable: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetCombinerInputParameterivNV: procedure(stage: GLenum; portion: GLenum; variable: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetCombinerOutputParameterfvNV: procedure(stage: GLenum; portion: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetCombinerOutputParameterivNV: procedure(stage: GLenum; portion: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetFinalCombinerInputParameterfvNV: procedure(variable: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetFinalCombinerInputParameterivNV: procedure(variable: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_register_combiners: Boolean;

//***** GL_NV_register_combiners2 *****//
const
  GL_PER_STAGE_CONSTANTS_NV = $8535;
var
  glCombinerStageParameterfvNV: procedure(stage: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetCombinerStageParameterfvNV: procedure(stage: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_register_combiners2: Boolean;

//***** GL_NV_texgen_emboss *****//
const
  GL_EMBOSS_MAP_NV = $855F;
  GL_EMBOSS_LIGHT_NV = $855D;
  GL_EMBOSS_CONSTANT_NV = $855E;

function Load_GL_NV_texgen_emboss: Boolean;

//***** GL_NV_texgen_reflection *****//
const
  GL_NORMAL_MAP_NV = $8511;
  GL_REFLECTION_MAP_NV = $8512;

function Load_GL_NV_texgen_reflection: Boolean;

//***** GL_NV_texture_compression_vtc *****//
  // GL_COMPRESSED_RGB_S3TC_DXT1_EXT  { already defined }
  // GL_COMPRESSED_RGBA_S3TC_DXT1_EXT  { already defined }
  // GL_COMPRESSED_RGBA_S3TC_DXT3_EXT  { already defined }
  // GL_COMPRESSED_RGBA_S3TC_DXT5_EXT  { already defined }

function Load_GL_NV_texture_compression_vtc: Boolean;

//***** GL_NV_texture_env_combine4 *****//
const
  GL_COMBINE4_NV = $8503;
  GL_SOURCE3_RGB_NV = $8583;
  GL_SOURCE3_ALPHA_NV = $858B;
  GL_OPERAND3_RGB_NV = $8593;
  GL_OPERAND3_ALPHA_NV = $859B;

function Load_GL_NV_texture_env_combine4: Boolean;

//***** GL_NV_texture_rectangle *****//
const
  GL_TEXTURE_RECTANGLE_NV = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_NV = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_NV = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_NV = $84F8;

function Load_GL_NV_texture_rectangle: Boolean;

//***** GL_NV_texture_shader *****//
const
  GL_TEXTURE_SHADER_NV = $86DE;
  GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV = $86D9;
  GL_SHADER_OPERATION_NV = $86DF;
  GL_CULL_MODES_NV = $86E0;
  GL_OFFSET_TEXTURE_MATRIX_NV = $86E1;
  GL_OFFSET_TEXTURE_SCALE_NV = $86E2;
  GL_OFFSET_TEXTURE_BIAS_NV = $86E3;
  GL_PREVIOUS_TEXTURE_INPUT_NV = $86E4;
  GL_CONST_EYE_NV = $86E5;
  GL_SHADER_CONSISTENT_NV = $86DD;
  GL_PASS_THROUGH_NV = $86E6;
  GL_CULL_FRAGMENT_NV = $86E7;
  GL_OFFSET_TEXTURE_2D_NV = $86E8;
  GL_OFFSET_TEXTURE_RECTANGLE_NV = $864C;
  GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV = $864D;
  GL_DEPENDENT_AR_TEXTURE_2D_NV = $86E9;
  GL_DEPENDENT_GB_TEXTURE_2D_NV = $86EA;
  GL_DOT_PRODUCT_NV = $86EC;
  GL_DOT_PRODUCT_DEPTH_REPLACE_NV = $86ED;
  GL_DOT_PRODUCT_TEXTURE_2D_NV = $86EE;
  GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV = $864E;
  GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV = $86F0;
  GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV = $86F1;
  GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV = $86F2;
  GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV = $86F3;
  GL_HILO_NV = $86F4;
  GL_DSDT_NV = $86F5;
  GL_DSDT_MAG_NV = $86F6;
  GL_DSDT_MAG_VIB_NV = $86F7;
  GL_UNSIGNED_INT_S8_S8_8_8_NV = $86DA;
  GL_UNSIGNED_INT_8_8_S8_S8_REV_NV = $86DB;
  GL_SIGNED_RGBA_NV = $86FB;
  GL_SIGNED_RGBA8_NV = $86FC;
  GL_SIGNED_RGB_NV = $86FE;
  GL_SIGNED_RGB8_NV = $86FF;
  GL_SIGNED_LUMINANCE_NV = $8701;
  GL_SIGNED_LUMINANCE8_NV = $8702;
  GL_SIGNED_LUMINANCE_ALPHA_NV = $8703;
  GL_SIGNED_LUMINANCE8_ALPHA8_NV = $8704;
  GL_SIGNED_ALPHA_NV = $8705;
  GL_SIGNED_ALPHA8_NV = $8706;
  GL_SIGNED_INTENSITY_NV = $8707;
  GL_SIGNED_INTENSITY8_NV = $8708;
  GL_SIGNED_RGB_UNSIGNED_ALPHA_NV = $870C;
  GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV = $870D;
  GL_HILO16_NV = $86F8;
  GL_SIGNED_HILO_NV = $86F9;
  GL_SIGNED_HILO16_NV = $86FA;
  GL_DSDT8_NV = $8709;
  GL_DSDT8_MAG8_NV = $870A;
  GL_DSDT_MAG_INTENSITY_NV = $86DC;
  GL_DSDT8_MAG8_INTENSITY8_NV = $870B;
  GL_HI_SCALE_NV = $870E;
  GL_LO_SCALE_NV = $870F;
  GL_DS_SCALE_NV = $8710;
  GL_DT_SCALE_NV = $8711;
  GL_MAGNITUDE_SCALE_NV = $8712;
  GL_VIBRANCE_SCALE_NV = $8713;
  GL_HI_BIAS_NV = $8714;
  GL_LO_BIAS_NV = $8715;
  GL_DS_BIAS_NV = $8716;
  GL_DT_BIAS_NV = $8717;
  GL_MAGNITUDE_BIAS_NV = $8718;
  GL_VIBRANCE_BIAS_NV = $8719;
  GL_TEXTURE_BORDER_VALUES_NV = $871A;
  GL_TEXTURE_HI_SIZE_NV = $871B;
  GL_TEXTURE_LO_SIZE_NV = $871C;
  GL_TEXTURE_DS_SIZE_NV = $871D;
  GL_TEXTURE_DT_SIZE_NV = $871E;
  GL_TEXTURE_MAG_SIZE_NV = $871F;

function Load_GL_NV_texture_shader: Boolean;

//***** GL_NV_texture_shader2 *****//
const
  GL_DOT_PRODUCT_TEXTURE_3D_NV = $86EF;
  // GL_HILO_NV  { already defined }
  // GL_DSDT_NV  { already defined }
  // GL_DSDT_MAG_NV  { already defined }
  // GL_DSDT_MAG_VIB_NV  { already defined }
  // GL_UNSIGNED_INT_S8_S8_8_8_NV  { already defined }
  // GL_UNSIGNED_INT_8_8_S8_S8_REV_NV  { already defined }
  // GL_SIGNED_RGBA_NV  { already defined }
  // GL_SIGNED_RGBA8_NV  { already defined }
  // GL_SIGNED_RGB_NV  { already defined }
  // GL_SIGNED_RGB8_NV  { already defined }
  // GL_SIGNED_LUMINANCE_NV  { already defined }
  // GL_SIGNED_LUMINANCE8_NV  { already defined }
  // GL_SIGNED_LUMINANCE_ALPHA_NV  { already defined }
  // GL_SIGNED_LUMINANCE8_ALPHA8_NV  { already defined }
  // GL_SIGNED_ALPHA_NV  { already defined }
  // GL_SIGNED_ALPHA8_NV  { already defined }
  // GL_SIGNED_INTENSITY_NV  { already defined }
  // GL_SIGNED_INTENSITY8_NV  { already defined }
  // GL_SIGNED_RGB_UNSIGNED_ALPHA_NV  { already defined }
  // GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV  { already defined }
  // GL_HILO16_NV  { already defined }
  // GL_SIGNED_HILO_NV  { already defined }
  // GL_SIGNED_HILO16_NV  { already defined }
  // GL_DSDT8_NV  { already defined }
  // GL_DSDT8_MAG8_NV  { already defined }
  // GL_DSDT_MAG_INTENSITY_NV  { already defined }
  // GL_DSDT8_MAG8_INTENSITY8_NV  { already defined }

function Load_GL_NV_texture_shader2: Boolean;

//***** GL_NV_texture_shader3 *****//
const
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV = $8850;
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV = $8851;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8852;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV = $8853;
  GL_OFFSET_HILO_TEXTURE_2D_NV = $8854;
  GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV = $8855;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV = $8856;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8857;
  GL_DEPENDENT_HILO_TEXTURE_2D_NV = $8858;
  GL_DEPENDENT_RGB_TEXTURE_3D_NV = $8859;
  GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV = $885A;
  GL_DOT_PRODUCT_PASS_THROUGH_NV = $885B;
  GL_DOT_PRODUCT_TEXTURE_1D_NV = $885C;
  GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV = $885D;
  GL_HILO8_NV = $885E;
  GL_SIGNED_HILO8_NV = $885F;
  GL_FORCE_BLUE_TO_ONE_NV = $8860;

function Load_GL_NV_texture_shader3: Boolean;

//***** GL_NV_vertex_array_range *****//
const
  GL_VERTEX_ARRAY_RANGE_NV = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV = $851E;
  GL_VERTEX_ARRAY_RANGE_VALID_NV = $851F;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_NV = $8521;
var
  glVertexArrayRangeNV: procedure(length: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFlushVertexArrayRangeNV: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
{$IFDEF Windows}
  wglAllocateMemoryNV: function(size: GLsizei; readFrequency: GLfloat; writeFrequency: GLfloat; priority: GLfloat): PGLvoid; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglFreeMemoryNV: procedure(pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
{$ENDIF}

function Load_GL_NV_vertex_array_range: Boolean;

//***** GL_NV_vertex_array_range2 *****//
const
  GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV = $8533;

function Load_GL_NV_vertex_array_range2: Boolean;

//***** GL_NV_vertex_program *****//
const
  GL_VERTEX_PROGRAM_NV = $8620;
  GL_VERTEX_PROGRAM_POINT_SIZE_NV = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_NV = $8643;
  GL_VERTEX_STATE_PROGRAM_NV = $8621;
  GL_ATTRIB_ARRAY_SIZE_NV = $8623;
  GL_ATTRIB_ARRAY_STRIDE_NV = $8624;
  GL_ATTRIB_ARRAY_TYPE_NV = $8625;
  GL_CURRENT_ATTRIB_NV = $8626;
  GL_PROGRAM_PARAMETER_NV = $8644;
  GL_ATTRIB_ARRAY_POINTER_NV = $8645;
  GL_PROGRAM_TARGET_NV = $8646;
  GL_PROGRAM_LENGTH_NV = $8627;
  GL_PROGRAM_RESIDENT_NV = $8647;
  GL_PROGRAM_STRING_NV = $8628;
  GL_TRACK_MATRIX_NV = $8648;
  GL_TRACK_MATRIX_TRANSFORM_NV = $8649;
  GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV = $862E;
  GL_MAX_TRACK_MATRICES_NV = $862F;
  GL_CURRENT_MATRIX_STACK_DEPTH_NV = $8640;
  GL_CURRENT_MATRIX_NV = $8641;
  GL_VERTEX_PROGRAM_BINDING_NV = $864A;
  GL_PROGRAM_ERROR_POSITION_NV = $864B;
  GL_MODELVIEW_PROJECTION_NV = $8629;
  GL_MATRIX0_NV = $8630;
  GL_MATRIX1_NV = $8631;
  GL_MATRIX2_NV = $8632;
  GL_MATRIX3_NV = $8633;
  GL_MATRIX4_NV = $8634;
  GL_MATRIX5_NV = $8635;
  GL_MATRIX6_NV = $8636;
  GL_MATRIX7_NV = $8637;
  GL_IDENTITY_NV = $862A;
  GL_INVERSE_NV = $862B;
  GL_TRANSPOSE_NV = $862C;
  GL_INVERSE_TRANSPOSE_NV = $862D;
  GL_VERTEX_ATTRIB_ARRAY0_NV = $8650;
  GL_VERTEX_ATTRIB_ARRAY1_NV = $8651;
  GL_VERTEX_ATTRIB_ARRAY2_NV = $8652;
  GL_VERTEX_ATTRIB_ARRAY3_NV = $8653;
  GL_VERTEX_ATTRIB_ARRAY4_NV = $8654;
  GL_VERTEX_ATTRIB_ARRAY5_NV = $8655;
  GL_VERTEX_ATTRIB_ARRAY6_NV = $8656;
  GL_VERTEX_ATTRIB_ARRAY7_NV = $8657;
  GL_VERTEX_ATTRIB_ARRAY8_NV = $8658;
  GL_VERTEX_ATTRIB_ARRAY9_NV = $8659;
  GL_VERTEX_ATTRIB_ARRAY10_NV = $865A;
  GL_VERTEX_ATTRIB_ARRAY11_NV = $865B;
  GL_VERTEX_ATTRIB_ARRAY12_NV = $865C;
  GL_VERTEX_ATTRIB_ARRAY13_NV = $865D;
  GL_VERTEX_ATTRIB_ARRAY14_NV = $865E;
  GL_VERTEX_ATTRIB_ARRAY15_NV = $865F;
  GL_MAP1_VERTEX_ATTRIB0_4_NV = $8660;
  GL_MAP1_VERTEX_ATTRIB1_4_NV = $8661;
  GL_MAP1_VERTEX_ATTRIB2_4_NV = $8662;
  GL_MAP1_VERTEX_ATTRIB3_4_NV = $8663;
  GL_MAP1_VERTEX_ATTRIB4_4_NV = $8664;
  GL_MAP1_VERTEX_ATTRIB5_4_NV = $8665;
  GL_MAP1_VERTEX_ATTRIB6_4_NV = $8666;
  GL_MAP1_VERTEX_ATTRIB7_4_NV = $8667;
  GL_MAP1_VERTEX_ATTRIB8_4_NV = $8668;
  GL_MAP1_VERTEX_ATTRIB9_4_NV = $8669;
  GL_MAP1_VERTEX_ATTRIB10_4_NV = $866A;
  GL_MAP1_VERTEX_ATTRIB11_4_NV = $866B;
  GL_MAP1_VERTEX_ATTRIB12_4_NV = $866C;
  GL_MAP1_VERTEX_ATTRIB13_4_NV = $866D;
  GL_MAP1_VERTEX_ATTRIB14_4_NV = $866E;
  GL_MAP1_VERTEX_ATTRIB15_4_NV = $866F;
  GL_MAP2_VERTEX_ATTRIB0_4_NV = $8670;
  GL_MAP2_VERTEX_ATTRIB1_4_NV = $8671;
  GL_MAP2_VERTEX_ATTRIB2_4_NV = $8672;
  GL_MAP2_VERTEX_ATTRIB3_4_NV = $8673;
  GL_MAP2_VERTEX_ATTRIB4_4_NV = $8674;
  GL_MAP2_VERTEX_ATTRIB5_4_NV = $8675;
  GL_MAP2_VERTEX_ATTRIB6_4_NV = $8676;
  GL_MAP2_VERTEX_ATTRIB7_4_NV = $8677;
  GL_MAP2_VERTEX_ATTRIB8_4_NV = $8678;
  GL_MAP2_VERTEX_ATTRIB9_4_NV = $8679;
  GL_MAP2_VERTEX_ATTRIB10_4_NV = $867A;
  GL_MAP2_VERTEX_ATTRIB11_4_NV = $867B;
  GL_MAP2_VERTEX_ATTRIB12_4_NV = $867C;
  GL_MAP2_VERTEX_ATTRIB13_4_NV = $867D;
  GL_MAP2_VERTEX_ATTRIB14_4_NV = $867E;
  GL_MAP2_VERTEX_ATTRIB15_4_NV = $867F;
var
  glBindProgramNV: procedure(target: GLenum; id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteProgramsNV: procedure(n: GLsizei; const ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glExecuteProgramNV: procedure(target: GLenum; id: GLuint; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenProgramsNV: procedure(n: GLsizei; ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAreProgramsResidentNV: function(n: GLsizei; const ids: PGLuint; residences: PGLboolean): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRequestResidentProgramsNV: procedure(n: GLsizei; ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramParameterfvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramParameterdvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramivNV: procedure(id: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramStringNV: procedure(id: GLuint; pname: GLenum; _program: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTrackMatrixivNV: procedure(target: GLenum; address: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribdvNV: procedure(index: GLuint; pname: GLenum; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribfvNV: procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribivNV: procedure(index: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribPointervNV: procedure(index: GLuint; pname: GLenum; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsProgramNV: function(id: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLoadProgramNV: procedure(target: GLenum; id: GLuint; len: GLsizei; const _program: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramParameter4fNV: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramParameter4fvNV: procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramParameters4dvNV: procedure(target: GLenum; index: GLuint; num: GLuint; const params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramParameters4fvNV: procedure(target: GLenum; index: GLuint; num: GLuint; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTrackMatrixNV: procedure(target: GLenum; address: GLuint; matrix: GLenum; transform: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribPointerNV: procedure(index: GLuint; size: GLint; _type: GLenum; stride: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1sNV: procedure(index: GLuint; x: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1fNV: procedure(index: GLuint; x: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1dNV: procedure(index: GLuint; x: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2sNV: procedure(index: GLuint; x: GLshort; y: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2fNV: procedure(index: GLuint; x: GLfloat; y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2dNV: procedure(index: GLuint; x: GLdouble; y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3sNV: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3fNV: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3dNV: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4sNV: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4fNV: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4dNV: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4ubNV: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1svNV: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1fvNV: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1dvNV: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2svNV: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2fvNV: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2dvNV: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3svNV: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3fvNV: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3dvNV: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4svNV: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4fvNV: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4dvNV: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4ubvNV: procedure(index: GLuint; const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs1svNV: procedure(index: GLuint; n: GLsizei; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs1fvNV: procedure(index: GLuint; n: GLsizei; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs1dvNV: procedure(index: GLuint; n: GLsizei; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs2svNV: procedure(index: GLuint; n: GLsizei; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs2fvNV: procedure(index: GLuint; n: GLsizei; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs2dvNV: procedure(index: GLuint; n: GLsizei; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs3svNV: procedure(index: GLuint; n: GLsizei; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs3fvNV: procedure(index: GLuint; n: GLsizei; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs3dvNV: procedure(index: GLuint; n: GLsizei; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs4svNV: procedure(index: GLuint; n: GLsizei; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs4fvNV: procedure(index: GLuint; n: GLsizei; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs4dvNV: procedure(index: GLuint; n: GLsizei; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs4ubvNV: procedure(index: GLuint; n: GLsizei; const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_vertex_program: Boolean;

//***** GL_NV_vertex_program1_1 *****//

function Load_GL_NV_vertex_program1_1: Boolean;

//***** GL_ATI_element_array *****//
const
  GL_ELEMENT_ARRAY_ATI = $8768;
  GL_ELEMENT_ARRAY_TYPE_ATI = $8769;
  GL_ELEMENT_ARRAY_POINTER_ATI = $876A;
var
  glElementPointerATI: procedure(_type: GLenum; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawElementArrayATI: procedure(mode: GLenum; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawRangeElementArrayATI: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_element_array: Boolean;

//***** GL_ATI_envmap_bumpmap *****//
const
  GL_BUMP_ROT_MATRIX_ATI = $8775;
  GL_BUMP_ROT_MATRIX_SIZE_ATI = $8776;
  GL_BUMP_NUM_TEX_UNITS_ATI = $8777;
  GL_BUMP_TEX_UNITS_ATI = $8778;
  GL_DUDV_ATI = $8779;
  GL_DU8DV8_ATI = $877A;
  GL_BUMP_ENVMAP_ATI = $877B;
  GL_BUMP_TARGET_ATI = $877C;
var
  glTexBumpParameterivATI: procedure(pname: GLenum; param: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexBumpParameterfvATI: procedure(pname: GLenum; param: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexBumpParameterivATI: procedure(pname: GLenum; param: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexBumpParameterfvATI: procedure(pname: GLenum; param: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_envmap_bumpmap: Boolean;

//***** GL_ATI_fragment_shader *****//
const
  GL_FRAGMENT_SHADER_ATI = $8920;
  GL_REG_0_ATI = $8921;
  GL_REG_1_ATI = $8922;
  GL_REG_2_ATI = $8923;
  GL_REG_3_ATI = $8924;
  GL_REG_4_ATI = $8925;
  GL_REG_5_ATI = $8926;
  GL_CON_0_ATI = $8941;
  GL_CON_1_ATI = $8942;
  GL_CON_2_ATI = $8943;
  GL_CON_3_ATI = $8944;
  GL_CON_4_ATI = $8945;
  GL_CON_5_ATI = $8946;
  GL_CON_6_ATI = $8947;
  GL_CON_7_ATI = $8948;
  GL_MOV_ATI = $8961;
  GL_ADD_ATI = $8963;
  GL_MUL_ATI = $8964;
  GL_SUB_ATI = $8965;
  GL_DOT3_ATI = $8966;
  GL_DOT4_ATI = $8967;
  GL_MAD_ATI = $8968;
  GL_LERP_ATI = $8969;
  GL_CND_ATI = $896A;
  GL_CND0_ATI = $896B;
  GL_DOT2_ADD_ATI = $896C;
  GL_SECONDARY_INTERPOLATOR_ATI = $896D;
  GL_SWIZZLE_STR_ATI = $8976;
  GL_SWIZZLE_STQ_ATI = $8977;
  GL_SWIZZLE_STR_DR_ATI = $8978;
  GL_SWIZZLE_STQ_DQ_ATI = $8979;
  GL_RED_BIT_ATI = $0001;
  GL_GREEN_BIT_ATI = $0002;
  GL_BLUE_BIT_ATI = $0004;
  GL_2X_BIT_ATI = $0001;
  GL_4X_BIT_ATI = $0002;
  GL_8X_BIT_ATI = $0004;
  GL_HALF_BIT_ATI = $0008;
  GL_QUARTER_BIT_ATI = $0010;
  GL_EIGHTH_BIT_ATI = $0020;
  GL_SATURATE_BIT_ATI = $0040;
  // GL_2X_BIT_ATI  { already defined }
  GL_COMP_BIT_ATI = $0002;
  GL_NEGATE_BIT_ATI = $0004;
  GL_BIAS_BIT_ATI = $0008;
var
  glGenFragmentShadersATI: function(range: GLuint): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindFragmentShaderATI: procedure(id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteFragmentShaderATI: procedure(id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBeginFragmentShaderATI: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndFragmentShaderATI: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPassTexCoordATI: procedure(dst: GLuint; coord: GLuint; swizzle: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSampleMapATI: procedure(dst: GLuint; interp: GLuint; swizzle: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorFragmentOp1ATI: procedure(op: GLenum; dst: GLuint; dstMask: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorFragmentOp2ATI: procedure(op: GLenum; dst: GLuint; dstMask: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint; arg2: GLuint; arg2Rep: GLuint; arg2Mod: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorFragmentOp3ATI: procedure(op: GLenum; dst: GLuint; dstMask: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint; arg2: GLuint; arg2Rep: GLuint; arg2Mod: GLuint; arg3: GLuint; arg3Rep: GLuint; arg3Mod: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAlphaFragmentOp1ATI: procedure(op: GLenum; dst: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAlphaFragmentOp2ATI: procedure(op: GLenum; dst: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint; arg2: GLuint; arg2Rep: GLuint; arg2Mod: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAlphaFragmentOp3ATI: procedure(op: GLenum; dst: GLuint; dstMod: GLuint; arg1: GLuint; arg1Rep: GLuint; arg1Mod: GLuint; arg2: GLuint; arg2Rep: GLuint; arg2Mod: GLuint; arg3: GLuint; arg3Rep: GLuint; arg3Mod: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSetFragmentShaderConstantATI: procedure(dst: GLuint; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_fragment_shader: Boolean;

//***** GL_ATI_pn_triangles *****//
const
  GL_PN_TRIANGLES_ATI = $87F0;
  GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI = $87F1;
  GL_PN_TRIANGLES_POINT_MODE_ATI = $87F2;
  GL_PN_TRIANGLES_NORMAL_MODE_ATI = $87F3;
  GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI = $87F4;
  GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI = $87F5;
  GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI = $87F6;
  GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI = $87F7;
  GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI = $87F8;
var
  glPNTrianglesiATI: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPNTrianglesfATI: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_pn_triangles: Boolean;

//***** GL_ATI_texture_mirror_once *****//
const
  GL_MIRROR_CLAMP_ATI = $8742;
  GL_MIRROR_CLAMP_TO_EDGE_ATI = $8743;

function Load_GL_ATI_texture_mirror_once: Boolean;

//***** GL_ATI_vertex_array_object *****//
const
  GL_STATIC_ATI = $8760;
  GL_DYNAMIC_ATI = $8761;
  GL_PRESERVE_ATI = $8762;
  GL_DISCARD_ATI = $8763;
  GL_OBJECT_BUFFER_SIZE_ATI = $8764;
  GL_OBJECT_BUFFER_USAGE_ATI = $8765;
  GL_ARRAY_OBJECT_BUFFER_ATI = $8766;
  GL_ARRAY_OBJECT_OFFSET_ATI = $8767;
var
  glNewObjectBufferATI: function(size: GLsizei; const pointer: PGLvoid; usage: GLenum): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsObjectBufferATI: function(buffer: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUpdateObjectBufferATI: procedure(buffer: GLuint; offset: GLuint; size: GLsizei; const pointer: PGLvoid; preserve: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetObjectBufferfvATI: procedure(buffer: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetObjectBufferivATI: procedure(buffer: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteObjectBufferATI: procedure(buffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glArrayObjectATI: procedure(_array: GLenum; size: GLint; _type: GLenum; stride: GLsizei; buffer: GLuint; offset: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetArrayObjectfvATI: procedure(_array: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetArrayObjectivATI: procedure(_array: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVariantArrayObjectATI: procedure(id: GLuint; _type: GLenum; stride: GLsizei; buffer: GLuint; offset: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVariantArrayObjectfvATI: procedure(id: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVariantArrayObjectivATI: procedure(id: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_vertex_array_object: Boolean;

//***** GL_ATI_vertex_streams *****//
const
  GL_MAX_VERTEX_STREAMS_ATI = $876B;
  GL_VERTEX_STREAM0_ATI = $876C;
  GL_VERTEX_STREAM1_ATI = $876D;
  GL_VERTEX_STREAM2_ATI = $876E;
  GL_VERTEX_STREAM3_ATI = $876F;
  GL_VERTEX_STREAM4_ATI = $8770;
  GL_VERTEX_STREAM5_ATI = $8771;
  GL_VERTEX_STREAM6_ATI = $8772;
  GL_VERTEX_STREAM7_ATI = $8773;
  GL_VERTEX_SOURCE_ATI = $8774;
var
  glVertexStream1s: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream1i: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream1f: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream1d: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream1sv: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream1iv: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream1fv: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream1dv: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream2s: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream2i: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream2f: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream2d: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream2sv: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream2iv: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream2fv: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream2dv: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream3s: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream3i: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream3f: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream3d: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream3sv: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream3iv: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream3fv: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream3dv: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream4s: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream4i: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream4f: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream4d: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream4sv: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream4iv: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream4fv: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexStream4dv: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3b: procedure(stream: GLenum; coords: GLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3s: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3i: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3f: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3d: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3bv: procedure(stream: GLenum; coords: GLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3sv: procedure(stream: GLenum; coords: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3iv: procedure(stream: GLenum; coords: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3fv: procedure(stream: GLenum; coords: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalStream3dv: procedure(stream: GLenum; coords: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClientActiveVertexStream: procedure(stream: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexBlendEnvi: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexBlendEnvf: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_vertex_streams: Boolean;

{$IFDEF Windows}
//***** WGL_I3D_image_buffer *****//
const
  WGL_IMAGE_BUFFER_MIN_ACCESS_I3D = $0001;
  WGL_IMAGE_BUFFER_LOCK_I3D = $0002;
var
  wglCreateImageBufferI3D: function(hDC: HDC; dwSize: DWORD; uFlags: UINT): PGLvoid; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglDestroyImageBufferI3D: function(hDC: HDC; pAddress: PGLvoid): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglAssociateImageBufferEventsI3D: function(hdc: HDC; pEvent: PHandle; pAddress: PGLvoid; pSize: PDWORD; count: UINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglReleaseImageBufferEventsI3D: function(hdc: HDC; pAddress: PGLvoid; count: UINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_I3D_image_buffer: Boolean;

//***** WGL_I3D_swap_frame_lock *****//
var
  wglEnableFrameLockI3D: function(): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglDisableFrameLockI3D: function(): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglIsEnabledFrameLockI3D: function(pFlag: PBOOL): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglQueryFrameLockMasterI3D: function(pFlag: PBOOL): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_I3D_swap_frame_lock: Boolean;

//***** WGL_I3D_swap_frame_usage *****//
var
  wglGetFrameUsageI3D: function(pUsage: PGLfloat): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglBeginFrameTrackingI3D: function(): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglEndFrameTrackingI3D: function(): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglQueryFrameTrackingI3D: function(pFrameCount: PDWORD; pMissedFrames: PDWORD; pLastMissedUsage: PGLfloat): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_I3D_swap_frame_usage: Boolean;
{$ENDIF}

//***** GL_3DFX_texture_compression_FXT1 *****//
const
  GL_COMPRESSED_RGB_FXT1_3DFX = $86B0;
  GL_COMPRESSED_RGBA_FXT1_3DFX = $86B1;

function Load_GL_3DFX_texture_compression_FXT1: Boolean;

//***** GL_IBM_cull_vertex *****//
const
  GL_CULL_VERTEX_IBM = $1928A;

function Load_GL_IBM_cull_vertex: Boolean;

//***** GL_IBM_multimode_draw_arrays *****//
var
  glMultiModeDrawArraysIBM: procedure(mode: PGLenum; first: PGLint; count: PGLsizei; primcount: GLsizei; modestride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiModeDrawElementsIBM: procedure(mode: PGLenum; count: PGLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei; modestride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_IBM_multimode_draw_arrays: Boolean;

//***** GL_IBM_raster_pos_clip *****//
const
  GL_RASTER_POSITION_UNCLIPPED_IBM = $19262;

function Load_GL_IBM_raster_pos_clip: Boolean;

//***** GL_IBM_texture_mirrored_repeat *****//
const
  GL_MIRRORED_REPEAT_IBM = $8370;

function Load_GL_IBM_texture_mirrored_repeat: Boolean;

//***** GL_IBM_vertex_array_lists *****//
const
  GL_VERTEX_ARRAY_LIST_IBM = $1929E;
  GL_NORMAL_ARRAY_LIST_IBM = $1929F;
  GL_COLOR_ARRAY_LIST_IBM = $192A0;
  GL_INDEX_ARRAY_LIST_IBM = $192A1;
  GL_TEXTURE_COORD_ARRAY_LIST_IBM = $192A2;
  GL_EDGE_FLAG_ARRAY_LIST_IBM = $192A3;
  GL_FOG_COORDINATE_ARRAY_LIST_IBM = $192A4;
  GL_SECONDARY_COLOR_ARRAY_LIST_IBM = $192A5;
  GL_VERTEX_ARRAY_LIST_STRIDE_IBM = $192A8;
  GL_NORMAL_ARRAY_LIST_STRIDE_IBM = $192A9;
  GL_COLOR_ARRAY_LIST_STRIDE_IBM = $192AA;
  GL_INDEX_ARRAY_LIST_STRIDE_IBM = $192AB;
  GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM = $192AC;
  GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM = $192AD;
  GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM = $192AE;
  GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM = $192AF;
var
  glColorPointerListIBM: procedure(size: GLint; _type: GLenum; stride: GLint; const pointer: PGLvoid; ptrstride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColorPointerListIBM: procedure(size: GLint; _type: GLenum; stride: GLint; const pointer: PGLvoid; ptrstride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEdgeFlagPointerListIBM: procedure(stride: GLint; const pointer: PGLboolean; ptrstride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordPointerListIBM: procedure(_type: GLenum; stride: GLint; const pointer: PGLvoid; ptrstride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalPointerListIBM: procedure(_type: GLenum; stride: GLint; const pointer: PGLvoid; ptrstride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordPointerListIBM: procedure(size: GLint; _type: GLenum; stride: GLint; const pointer: PGLvoid; ptrstride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexPointerListIBM: procedure(size: GLint; _type: GLenum; stride: GLint; const pointer: PGLvoid; ptrstride: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_IBM_vertex_array_lists: Boolean;

//***** GL_MESA_resize_buffers *****//
var
  glResizeBuffersMESA: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_MESA_resize_buffers: Boolean;

//***** GL_MESA_window_pos *****//
var
  glWindowPos2dMESA: procedure(x: GLdouble; y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2fMESA: procedure(x: GLfloat; y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2iMESA: procedure(x: GLint; y: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2sMESA: procedure(x: GLshort; y: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2ivMESA: procedure(const p: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2svMESA: procedure(const p: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2fvMESA: procedure(const p: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2dvMESA: procedure(const p: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3iMESA: procedure(x: GLint; y: GLint; z: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3sMESA: procedure(x: GLshort; y: GLshort; z: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3fMESA: procedure(x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3dMESA: procedure(x: GLdouble; y: GLdouble; z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3ivMESA: procedure(const p: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3svMESA: procedure(const p: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3fvMESA: procedure(const p: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3dvMESA: procedure(const p: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos4iMESA: procedure(x: GLint; y: GLint; z: GLint; w: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos4sMESA: procedure(x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos4fMESA: procedure(x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos4dMESA: procedure(x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos4ivMESA: procedure(const p: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos4svMESA: procedure(const p: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos4fvMESA: procedure(const p: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos4dvMESA: procedure(const p: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_MESA_window_pos: Boolean;

//***** GL_OML_interlace *****//
const
  GL_INTERLACE_OML = $8980;
  GL_INTERLACE_READ_OML = $8981;

function Load_GL_OML_interlace: Boolean;

//***** GL_OML_resample *****//
const
  GL_PACK_RESAMPLE_OML = $8984;
  GL_UNPACK_RESAMPLE_OML = $8985;
  GL_RESAMPLE_REPLICATE_OML = $8986;
  GL_RESAMPLE_ZERO_FILL_OML = $8987;
  GL_RESAMPLE_AVERAGE_OML = $8988;
  GL_RESAMPLE_DECIMATE_OML = $8989;
  // GL_RESAMPLE_AVERAGE_OML  { already defined }

function Load_GL_OML_resample: Boolean;

//***** GL_OML_subsample *****//
const
  GL_FORMAT_SUBSAMPLE_24_24_OML = $8982;
  GL_FORMAT_SUBSAMPLE_244_244_OML = $8983;

function Load_GL_OML_subsample: Boolean;

//***** GL_SGIS_generate_mipmap *****//
const
  GL_GENERATE_MIPMAP_SGIS = $8191;
  GL_GENERATE_MIPMAP_HINT_SGIS = $8192;

function Load_GL_SGIS_generate_mipmap: Boolean;

//***** GL_SGIS_multisample *****//
const
  GLX_SAMPLE_BUFFERS_SGIS = $186A0;
  GLX_SAMPLES_SGIS = $186A1;
  GL_MULTISAMPLE_SGIS = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_SGIS = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_SGIS = $809F;
  GL_SAMPLE_MASK_SGIS = $80A0;
  GL_MULTISAMPLE_BIT_EXT = $20000000;
  GL_1PASS_SGIS = $80A1;
  GL_2PASS_0_SGIS = $80A2;
  GL_2PASS_1_SGIS = $80A3;
  GL_4PASS_0_SGIS = $80A4;
  GL_4PASS_1_SGIS = $80A5;
  GL_4PASS_2_SGIS = $80A6;
  GL_4PASS_3_SGIS = $80A7;
  GL_SAMPLE_BUFFERS_SGIS = $80A8;
  GL_SAMPLES_SGIS = $80A9;
  GL_SAMPLE_MASK_VALUE_SGIS = $80AA;
  GL_SAMPLE_MASK_INVERT_SGIS = $80AB;
  GL_SAMPLE_PATTERN_SGIS = $80AC;
var
  glSampleMaskSGIS: procedure(value: GLclampf; invert: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSamplePatternSGIS: procedure(pattern: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_SGIS_multisample: Boolean;

//***** GL_SGIS_pixel_texture *****//
const
  GL_PIXEL_TEXTURE_SGIS = $8353;
  GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS = $8354;
  GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS = $8355;
  GL_PIXEL_GROUP_COLOR_SGIS = $8356;
var
  glPixelTexGenParameteriSGIS: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPixelTexGenParameterfSGIS: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetPixelTexGenParameterivSGIS: procedure(pname: GLenum; params: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetPixelTexGenParameterfvSGIS: procedure(pname: GLenum; params: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_SGIS_pixel_texture: Boolean;

//***** GL_SGIS_texture_border_clamp *****//
  // GL_CLAMP_TO_BORDER_SGIS  { already defined }

function Load_GL_SGIS_texture_border_clamp: Boolean;

//***** GL_SGIS_texture_color_mask *****//
const
  GL_TEXTURE_COLOR_WRITEMASK_SGIS = $81EF;
var
  glTextureColorMaskSGIS: procedure(r: GLboolean; g: GLboolean; b: GLboolean; a: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_SGIS_texture_color_mask: Boolean;

//***** GL_SGIS_texture_edge_clamp *****//
const
  GL_CLAMP_TO_EDGE_SGIS = $812F;

function Load_GL_SGIS_texture_edge_clamp: Boolean;

//***** GL_SGIS_texture_lod *****//
const
  GL_TEXTURE_MIN_LOD_SGIS = $813A;
  GL_TEXTURE_MAX_LOD_SGIS = $813B;
  GL_TEXTURE_BASE_LEVEL_SGIS = $813C;
  GL_TEXTURE_MAX_LEVEL_SGIS = $813D;

function Load_GL_SGIS_texture_lod: Boolean;

//***** GL_SGIS_depth_texture *****//
const
  GL_DEPTH_COMPONENT16_SGIX = $81A5;
  GL_DEPTH_COMPONENT24_SGIX = $81A6;
  GL_DEPTH_COMPONENT32_SGIX = $81A7;

function Load_GL_SGIS_depth_texture: Boolean;

//***** GL_SGIX_fog_offset *****//
const
  GL_FOG_OFFSET_SGIX = $8198;
  GL_FOG_OFFSET_VALUE_SGIX = $8199;

function Load_GL_SGIX_fog_offset: Boolean;

//***** GL_SGIX_interlace *****//
const
  GL_INTERLACE_SGIX = $8094;

function Load_GL_SGIX_interlace: Boolean;

//***** GL_SGIX_shadow_ambient *****//
const
  GL_SHADOW_AMBIENT_SGIX = $80BF;

function Load_GL_SGIX_shadow_ambient: Boolean;

//***** GL_SGI_color_matrix *****//
const
  GL_COLOR_MATRIX_SGI = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH_SGI = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE_SGI = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS_SGI = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI = $80BB;

function Load_GL_SGI_color_matrix: Boolean;

//***** GL_SGI_color_table *****//
const
  GL_COLOR_TABLE_SGI = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE_SGI = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D2;
  GL_PROXY_COLOR_TABLE_SGI = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D5;
  GL_COLOR_TABLE_SCALE_SGI = $80D6;
  GL_COLOR_TABLE_BIAS_SGI = $80D7;
  GL_COLOR_TABLE_FORMAT_SGI = $80D8;
  GL_COLOR_TABLE_WIDTH_SGI = $80D9;
  GL_COLOR_TABLE_RED_SIZE_SGI = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_SGI = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_SGI = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_SGI = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_SGI = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_SGI = $80DF;
var
  glColorTableSGI: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; format: GLenum; _type: GLenum; const table: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCopyColorTableSGI: procedure(target: GLenum; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorTableParameterivSGI: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorTableParameterfvSGI: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetColorTableSGI: procedure(target: GLenum; format: GLenum; _type: GLenum; table: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetColorTableParameterivSGI: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetColorTableParameterfvSGI: procedure(target: GLenum; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_SGI_color_table: Boolean;

//***** GL_SGI_texture_color_table *****//
const
  GL_TEXTURE_COLOR_TABLE_SGI = $80BC;
  GL_PROXY_TEXTURE_COLOR_TABLE_SGI = $80BD;

function Load_GL_SGI_texture_color_table: Boolean;

//***** GL_SUN_vertex *****//
var
  glColor4ubVertex2fSUN: procedure(r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4ubVertex2fvSUN: procedure(const c: PGLubyte; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4ubVertex3fSUN: procedure(r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4ubVertex3fvSUN: procedure(const c: PGLubyte; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3fVertex3fSUN: procedure(r: GLfloat; g: GLfloat; b: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3fVertex3fvSUN: procedure(const c: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3fVertex3fSUN: procedure(nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3fVertex3fvSUN: procedure(const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4fNormal3fVertex3fSUN: procedure(r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4fNormal3fVertex3fvSUN: procedure(const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fVertex3fvSUN: procedure(const tc: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4fVertex4fSUN: procedure(s: GLfloat; t: GLfloat; p: GLfloat; q: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4fVertex4fvSUN: procedure(const tc: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fColor4ubVertex3fSUN: procedure(s: GLfloat; t: GLfloat; r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fColor4ubVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLubyte; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fColor3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fColor3fVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fNormal3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fNormal3fVertex3fvSUN: procedure(const tc: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fColor4fNormal3fVertex3fSUN: procedure(s: GLfloat; t: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4fColor4fNormal3fVertex4fSUN: procedure(s: GLfloat; t: GLfloat; p: GLfloat; q: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4fColor4fNormal3fVertex4fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiVertex3fSUN: procedure(rc: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiVertex3fvSUN: procedure(const rc: PGLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiColor4ubVertex3fSUN: procedure(rc: GLuint; r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiColor4ubVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLubyte; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiColor3fVertex3fSUN: procedure(rc: GLuint; r: GLfloat; g: GLfloat; b: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiColor3fVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiNormal3fVertex3fSUN: procedure(rc: GLuint; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiColor4fNormal3fVertex3fSUN: procedure(rc: GLuint; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiTexCoord2fVertex3fSUN: procedure(rc: GLuint; s: GLfloat; t: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiTexCoord2fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN: procedure(rc: GLuint; s: GLfloat; t: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN: procedure(rc: GLuint; s: GLfloat; t: GLfloat; r: GLfloat; g: GLfloat; b: GLfloat; a: GLfloat; nx: GLfloat; ny: GLfloat; nz: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_SUN_vertex: Boolean;

//***** GL_ARB_fragment_program *****//
const
  GL_FRAGMENT_PROGRAM_ARB = $8804;
  // GL_PROGRAM_FORMAT_ASCII_ARB  { already defined }
  // GL_PROGRAM_LENGTH_ARB  { already defined }
  // GL_PROGRAM_FORMAT_ARB  { already defined }
  // GL_PROGRAM_BINDING_ARB  { already defined }
  // GL_PROGRAM_INSTRUCTIONS_ARB  { already defined }
  // GL_MAX_PROGRAM_INSTRUCTIONS_ARB  { already defined }
  // GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB  { already defined }
  // GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB  { already defined }
  // GL_PROGRAM_TEMPORARIES_ARB  { already defined }
  // GL_MAX_PROGRAM_TEMPORARIES_ARB  { already defined }
  // GL_PROGRAM_NATIVE_TEMPORARIES_ARB  { already defined }
  // GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB  { already defined }
  // GL_PROGRAM_PARAMETERS_ARB  { already defined }
  // GL_MAX_PROGRAM_PARAMETERS_ARB  { already defined }
  // GL_PROGRAM_NATIVE_PARAMETERS_ARB  { already defined }
  // GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB  { already defined }
  // GL_PROGRAM_ATTRIBS_ARB  { already defined }
  // GL_MAX_PROGRAM_ATTRIBS_ARB  { already defined }
  // GL_PROGRAM_NATIVE_ATTRIBS_ARB  { already defined }
  // GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB  { already defined }
  // GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB  { already defined }
  // GL_MAX_PROGRAM_ENV_PARAMETERS_ARB  { already defined }
  // GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB  { already defined }
  GL_PROGRAM_ALU_INSTRUCTIONS_ARB = $8805;
  GL_PROGRAM_TEX_INSTRUCTIONS_ARB = $8806;
  GL_PROGRAM_TEX_INDIRECTIONS_ARB = $8807;
  GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $8808;
  GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $8809;
  GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $880A;
  GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB = $880B;
  GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB = $880C;
  GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB = $880D;
  GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $880E;
  GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $880F;
  GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $8810;
  // GL_PROGRAM_STRING_ARB  { already defined }
  // GL_PROGRAM_ERROR_POSITION_ARB  { already defined }
  // GL_CURRENT_MATRIX_ARB  { already defined }
  // GL_TRANSPOSE_CURRENT_MATRIX_ARB  { already defined }
  // GL_CURRENT_MATRIX_STACK_DEPTH_ARB  { already defined }
  // GL_MAX_PROGRAM_MATRICES_ARB  { already defined }
  // GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB  { already defined }
  GL_MAX_TEXTURE_COORDS_ARB = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_ARB = $8872;
  // GL_PROGRAM_ERROR_STRING_ARB  { already defined }
  // GL_MATRIX0_ARB  { already defined }
  // GL_MATRIX1_ARB  { already defined }
  // GL_MATRIX2_ARB  { already defined }
  // GL_MATRIX3_ARB  { already defined }
  // GL_MATRIX4_ARB  { already defined }
  // GL_MATRIX5_ARB  { already defined }
  // GL_MATRIX6_ARB  { already defined }
  // GL_MATRIX7_ARB  { already defined }
  // GL_MATRIX8_ARB  { already defined }
  // GL_MATRIX9_ARB  { already defined }
  // GL_MATRIX10_ARB  { already defined }
  // GL_MATRIX11_ARB  { already defined }
  // GL_MATRIX12_ARB  { already defined }
  // GL_MATRIX13_ARB  { already defined }
  // GL_MATRIX14_ARB  { already defined }
  // GL_MATRIX15_ARB  { already defined }
  // GL_MATRIX16_ARB  { already defined }
  // GL_MATRIX17_ARB  { already defined }
  // GL_MATRIX18_ARB  { already defined }
  // GL_MATRIX19_ARB  { already defined }
  // GL_MATRIX20_ARB  { already defined }
  // GL_MATRIX21_ARB  { already defined }
  // GL_MATRIX22_ARB  { already defined }
  // GL_MATRIX23_ARB  { already defined }
  // GL_MATRIX24_ARB  { already defined }
  // GL_MATRIX25_ARB  { already defined }
  // GL_MATRIX26_ARB  { already defined }
  // GL_MATRIX27_ARB  { already defined }
  // GL_MATRIX28_ARB  { already defined }
  // GL_MATRIX29_ARB  { already defined }
  // GL_MATRIX30_ARB  { already defined }
  // GL_MATRIX31_ARB  { already defined }
  // glProgramStringARB  { already defined }
  // glBindProgramARB  { already defined }
  // glDeleteProgramsARB  { already defined }
  // glGenProgramsARB  { already defined }
  // glProgramEnvParameter4dARB  { already defined }
  // glProgramEnvParameter4dvARB  { already defined }
  // glProgramEnvParameter4fARB  { already defined }
  // glProgramEnvParameter4fvARB  { already defined }
  // glProgramLocalParameter4dARB  { already defined }
  // glProgramLocalParameter4dvARB  { already defined }
  // glProgramLocalParameter4fARB  { already defined }
  // glProgramLocalParameter4fvARB  { already defined }
  // glGetProgramEnvParameterdvARB  { already defined }
  // glGetProgramEnvParameterfvARB  { already defined }
  // glGetProgramLocalParameterdvARB  { already defined }
  // glGetProgramLocalParameterfvARB  { already defined }
  // glGetProgramivARB  { already defined }
  // glGetProgramStringARB  { already defined }
  // glIsProgramARB  { already defined }

function Load_GL_ARB_fragment_program: Boolean;

{***** GL_ATI_text_fragment_shader *****}
const
     GL_TEXT_FRAGMENT_SHADER_ATI = $8200;

function Load_GL_ATI_text_fragment_shader: Boolean;

{***** GL_ARB_vertex_buffer_object *****}
const
     GL_BUFFER_SIZE_ARB = $8764;
     GL_BUFFER_USAGE_ARB = $8765;
     GL_ARRAY_BUFFER_ARB = $8892;
     GL_ELEMENT_ARRAY_BUFFER_ARB = $8893;
     GL_ARRAY_BUFFER_BINDING_ARB = $8894;
     GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB = $8895;
     GL_VERTEX_ARRAY_BUFFER_BINDING_ARB = $8896;
     GL_NORMAL_ARRAY_BUFFER_BINDING_ARB = $8897;
     GL_COLOR_ARRAY_BUFFER_BINDING_ARB = $8898;
     GL_INDEX_ARRAY_BUFFER_BINDING_ARB = $8899;
     GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB = $889A;
     GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB = $889B;
     GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB = $889C;
     GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB = $889D;
     GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB = $889E;
     GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB = $889F;
     GL_READ_ONLY_ARB = $88B8;
     GL_WRITE_ONLY_ARB = $88B9;
     GL_READ_WRITE_ARB = $88BA;
     GL_BUFFER_ACCESS_ARB = $88BB;
     GL_BUFFER_MAPPED_ARB = $88BC;
     GL_BUFFER_MAP_POINTER_ARB = $88BD;
     GL_STREAM_DRAW_ARB = $88E0;
     GL_STREAM_READ_ARB = $88E1;
     GL_STREAM_COPY_ARB = $88E2;
     GL_STATIC_DRAW_ARB = $88E4;
     GL_STATIC_READ_ARB = $88E5;
     GL_STATIC_COPY_ARB = $88E6;
     GL_DYNAMIC_DRAW_ARB = $88E8;
     GL_DYNAMIC_READ_ARB = $88E9;
     GL_DYNAMIC_COPY_ARB = $88EA;

var
     glBindBufferARB : procedure(target : GLenum; buffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glDeleteBuffersARB : procedure(n : GLsizei; buffers : PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glGenBuffersARB : procedure(n : GLsizei; buffers : PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glIsBufferARB : function (buffer : GLuint) :GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glBufferDataARB : procedure(target : GLenum; size:GLsizei; data:PGLvoid;usage: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glBufferSubDataARB : procedure(target : GLenum; offset :GLint; size : GLsizei; data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glGetBufferSubDataARB : procedure(target : GLenum; offset :GLint; size : GLsizei; data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glMapBufferARB : function (target :GLenum; access: GLenum) : PGLvoid; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glUnmapBufferARB : function (target :GLenum) :GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glGetBufferParameterivARB:procedure(target:GLenum; pname:GLenum; params:PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
     glGetBufferPointervARB : procedure(target: GLenum; pname:GLenum; params: PPGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
function Load_GL_ARB_vertex_buffer_object : boolean;

//***** GL_APPLE_client_storage *****//
const
  GL_UNPACK_CLIENT_STORAGE_APPLE = $85B2;

function Load_GL_APPLE_client_storage: Boolean;

//***** GL_APPLE_element_array *****//
const
  GL_ELEMENT_ARRAY_APPLE = $8768;
  GL_ELEMENT_ARRAY_TYPE_APPLE = $8769;
  GL_ELEMENT_ARRAY_POINTER_APPLE = $876A;
var
  glElementPointerAPPLE: procedure(_type: GLenum; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawElementArrayAPPLE: procedure(mode: GLenum; first: GLint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawRangeElementArrayAPPLE: procedure(mode: GLenum; start: GLuint; _end: GLuint; first: GLint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiDrawElementArrayAPPLE: procedure(mode: GLenum; const first: PGLint; const count: PGLsizei; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiDrawRangeElementArrayAPPLE: procedure(mode: GLenum; start: GLuint; _end: GLuint; const first: PGLint; const count: PGLsizei; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_APPLE_element_array: Boolean;

//***** GL_APPLE_fence *****//
const
  GL_DRAW_PIXELS_APPLE = $8A0A;
  GL_FENCE_APPLE = $8A0B;
var
  glGenFencesAPPLE: procedure(n: GLsizei; fences: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteFencesAPPLE: procedure(n: GLsizei; const fences: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSetFenceAPPLE: procedure(fence: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsFenceAPPLE: function(fence: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTestFenceAPPLE: function(fence: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFinishFenceAPPLE: procedure(fence: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTestObjectAPPLE: function(_object: GLenum; name: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFinishObjectAPPLE: procedure(_object: GLenum; name: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_APPLE_fence: Boolean;

//***** GL_APPLE_vertex_array_object *****//
const
  GL_VERTEX_ARRAY_BINDING_APPLE = $85B5;
var
  glBindVertexArrayAPPLE: procedure(_array: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteVertexArraysAPPLE: procedure(n: GLsizei; const arrays: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenVertexArraysAPPLE: procedure(n: GLsizei; const arrays: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsVertexArrayAPPLE: function(_array: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_APPLE_vertex_array_object: Boolean;

//***** GL_APPLE_vertex_array_range *****//
const
  GL_VERTEX_ARRAY_RANGE_APPLE = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE = $851E;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_APPLE = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_APPLE = $8521;
  GL_VERTEX_ARRAY_STORAGE_HINT_APPLE = $851F;
  GL_STORAGE_CACHED_APPLE = $85BE;
  GL_STORAGE_SHARED_APPLE = $85BF;
var
  glVertexArrayRangeAPPLE: procedure(length: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFlushVertexArrayRangeAPPLE: procedure(length: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexArrayParameteriAPPLE: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_APPLE_vertex_array_range: Boolean;

{$IFDEF Windows}
//***** WGL_ARB_pixel_format *****//
const
  WGL_NUMBER_PIXEL_FORMATS_ARB = $2000;
  WGL_DRAW_TO_WINDOW_ARB = $2001;
  WGL_DRAW_TO_BITMAP_ARB = $2002;
  WGL_ACCELERATION_ARB = $2003;
  WGL_NEED_PALETTE_ARB = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB = $2006;
  WGL_SWAP_METHOD_ARB = $2007;
  WGL_NUMBER_OVERLAYS_ARB = $2008;
  WGL_NUMBER_UNDERLAYS_ARB = $2009;
  WGL_TRANSPARENT_ARB = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB = $203B;
  WGL_SHARE_DEPTH_ARB = $200C;
  WGL_SHARE_STENCIL_ARB = $200D;
  WGL_SHARE_ACCUM_ARB = $200E;
  WGL_SUPPORT_GDI_ARB = $200F;
  WGL_SUPPORT_OPENGL_ARB = $2010;
  WGL_DOUBLE_BUFFER_ARB = $2011;
  WGL_STEREO_ARB = $2012;
  WGL_PIXEL_TYPE_ARB = $2013;
  WGL_COLOR_BITS_ARB = $2014;
  WGL_RED_BITS_ARB = $2015;
  WGL_RED_SHIFT_ARB = $2016;
  WGL_GREEN_BITS_ARB = $2017;
  WGL_GREEN_SHIFT_ARB = $2018;
  WGL_BLUE_BITS_ARB = $2019;
  WGL_BLUE_SHIFT_ARB = $201A;
  WGL_ALPHA_BITS_ARB = $201B;
  WGL_ALPHA_SHIFT_ARB = $201C;
  WGL_ACCUM_BITS_ARB = $201D;
  WGL_ACCUM_RED_BITS_ARB = $201E;
  WGL_ACCUM_GREEN_BITS_ARB = $201F;
  WGL_ACCUM_BLUE_BITS_ARB = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB = $2021;
  WGL_DEPTH_BITS_ARB = $2022;
  WGL_STENCIL_BITS_ARB = $2023;
  WGL_AUX_BUFFERS_ARB = $2024;
  WGL_NO_ACCELERATION_ARB = $2025;
  WGL_GENERIC_ACCELERATION_ARB = $2026;
  WGL_FULL_ACCELERATION_ARB = $2027;
  WGL_SWAP_EXCHANGE_ARB = $2028;
  WGL_SWAP_COPY_ARB = $2029;
  WGL_SWAP_UNDEFINED_ARB = $202A;
  WGL_TYPE_RGBA_ARB = $202B;
  WGL_TYPE_COLORINDEX_ARB = $202C;
var
  wglGetPixelFormatAttribivARB: function(hdc: HDC; iPixelFormat: GLint; iLayerPlane: GLint; nAttributes: GLuint; const piAttributes: PGLint; piValues: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetPixelFormatAttribfvARB: function(hdc: HDC; iPixelFormat: GLint; iLayerPlane: GLint; nAttributes: GLuint; const piAttributes: PGLint; pfValues: PGLfloat): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglChoosePixelFormatARB: function(hdc: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLfloat; nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLuint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_ARB_pixel_format: Boolean;

//***** WGL_ARB_make_current_read *****//
const
  WGL_ERROR_INVALID_PIXEL_TYPE_ARB = $2043;
  WGL_ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB = $2054;
var
  wglMakeContextCurrentARB: function(hDrawDC: HDC; hReadDC: HDC; hglrc: HGLRC): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetCurrentReadDCARB: function(): HDC; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_ARB_make_current_read: Boolean;

//***** WGL_ARB_pbuffer *****//
const
  WGL_DRAW_TO_PBUFFER_ARB = $202D;
  // WGL_DRAW_TO_PBUFFER_ARB  { already defined }
  WGL_MAX_PBUFFER_PIXELS_ARB = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB = $2030;
  WGL_PBUFFER_LARGEST_ARB = $2033;
  WGL_PBUFFER_WIDTH_ARB = $2034;
  WGL_PBUFFER_HEIGHT_ARB = $2035;
  WGL_PBUFFER_LOST_ARB = $2036;
var
  wglCreatePbufferARB: function(hDC: HDC; iPixelFormat: GLint; iWidth: GLint; iHeight: GLint; const piAttribList: PGLint): THandle; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetPbufferDCARB: function(hPbuffer: THandle): HDC; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglReleasePbufferDCARB: function(hPbuffer: THandle; hDC: HDC): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglDestroyPbufferARB: function(hPbuffer: THandle): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglQueryPbufferARB: function(hPbuffer: THandle; iAttribute: GLint; piValue: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_ARB_pbuffer: Boolean;

//***** WGL_EXT_swap_control *****//
var
  wglSwapIntervalEXT: function(interval: GLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetSwapIntervalEXT: function(): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_EXT_swap_control: Boolean;

//***** WGL_ARB_render_texture *****//
const
  WGL_BIND_TO_TEXTURE_RGB_ARB = $2070;
  WGL_BIND_TO_TEXTURE_RGBA_ARB = $2071;
  WGL_TEXTURE_FORMAT_ARB = $2072;
  WGL_TEXTURE_TARGET_ARB = $2073;
  WGL_MIPMAP_TEXTURE_ARB = $2074;
  WGL_TEXTURE_RGB_ARB = $2075;
  WGL_TEXTURE_RGBA_ARB = $2076;
  WGL_NO_TEXTURE_ARB = $2077;
  WGL_TEXTURE_CUBE_MAP_ARB = $2078;
  WGL_TEXTURE_1D_ARB = $2079;
  WGL_TEXTURE_2D_ARB = $207A;
  // WGL_NO_TEXTURE_ARB  { already defined }
  WGL_MIPMAP_LEVEL_ARB = $207B;
  WGL_CUBE_MAP_FACE_ARB = $207C;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $207D;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $207E;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $207F;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $2080;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $2081;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $2082;
  WGL_FRONT_LEFT_ARB = $2083;
  WGL_FRONT_RIGHT_ARB = $2084;
  WGL_BACK_LEFT_ARB = $2085;
  WGL_BACK_RIGHT_ARB = $2086;
  WGL_AUX0_ARB = $2087;
  WGL_AUX1_ARB = $2088;
  WGL_AUX2_ARB = $2089;
  WGL_AUX3_ARB = $208A;
  WGL_AUX4_ARB = $208B;
  WGL_AUX5_ARB = $208C;
  WGL_AUX6_ARB = $208D;
  WGL_AUX7_ARB = $208E;
  WGL_AUX8_ARB = $208F;
  WGL_AUX9_ARB = $2090;
var
  wglBindTexImageARB: function(hPbuffer: THandle; iBuffer: GLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglReleaseTexImageARB: function(hPbuffer: THandle; iBuffer: GLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglSetPbufferAttribARB: function(hPbuffer: THandle; const piAttribList: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_ARB_render_texture: Boolean;

//***** WGL_EXT_extensions_string *****//
var
  wglGetExtensionsStringEXT: function(): pansichar; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_EXT_extensions_string: Boolean;

//***** WGL_EXT_make_current_read *****//
var
  wglMakeContextCurrentEXT: function(hDrawDC: HDC; hReadDC: HDC; hglrc: HGLRC): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetCurrentReadDCEXT: function(): HDC; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_EXT_make_current_read: Boolean;

//***** WGL_EXT_pbuffer *****//
const
  WGL_DRAW_TO_PBUFFER_EXT = $202D;
  WGL_MAX_PBUFFER_PIXELS_EXT = $202E;
  WGL_MAX_PBUFFER_WIDTH_EXT = $202F;
  WGL_MAX_PBUFFER_HEIGHT_EXT = $2030;
  WGL_OPTIMAL_PBUFFER_WIDTH_EXT = $2031;
  WGL_OPTIMAL_PBUFFER_HEIGHT_EXT = $2032;
  WGL_PBUFFER_LARGEST_EXT = $2033;
  WGL_PBUFFER_WIDTH_EXT = $2034;
  WGL_PBUFFER_HEIGHT_EXT = $2035;
var
  wglCreatePbufferEXT: function(hDC: HDC; iPixelFormat: GLint; iWidth: GLint; iHeight: GLint; const piAttribList: PGLint): THandle; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetPbufferDCEXT: function(hPbuffer: THandle): HDC; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglReleasePbufferDCEXT: function(hPbuffer: THandle; hDC: HDC): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglDestroyPbufferEXT: function(hPbuffer: THandle): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglQueryPbufferEXT: function(hPbuffer: THandle; iAttribute: GLint; piValue: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_EXT_pbuffer: Boolean;

//***** WGL_EXT_pixel_format *****//
const
  WGL_NUMBER_PIXEL_FORMATS_EXT = $2000;
  WGL_DRAW_TO_WINDOW_EXT = $2001;
  WGL_DRAW_TO_BITMAP_EXT = $2002;
  WGL_ACCELERATION_EXT = $2003;
  WGL_NEED_PALETTE_EXT = $2004;
  WGL_NEED_SYSTEM_PALETTE_EXT = $2005;
  WGL_SWAP_LAYER_BUFFERS_EXT = $2006;
  WGL_SWAP_METHOD_EXT = $2007;
  WGL_NUMBER_OVERLAYS_EXT = $2008;
  WGL_NUMBER_UNDERLAYS_EXT = $2009;
  WGL_TRANSPARENT_EXT = $200A;
  WGL_TRANSPARENT_VALUE_EXT = $200B;
  WGL_SHARE_DEPTH_EXT = $200C;
  WGL_SHARE_STENCIL_EXT = $200D;
  WGL_SHARE_ACCUM_EXT = $200E;
  WGL_SUPPORT_GDI_EXT = $200F;
  WGL_SUPPORT_OPENGL_EXT = $2010;
  WGL_DOUBLE_BUFFER_EXT = $2011;
  WGL_STEREO_EXT = $2012;
  WGL_PIXEL_TYPE_EXT = $2013;
  WGL_COLOR_BITS_EXT = $2014;
  WGL_RED_BITS_EXT = $2015;
  WGL_RED_SHIFT_EXT = $2016;
  WGL_GREEN_BITS_EXT = $2017;
  WGL_GREEN_SHIFT_EXT = $2018;
  WGL_BLUE_BITS_EXT = $2019;
  WGL_BLUE_SHIFT_EXT = $201A;
  WGL_ALPHA_BITS_EXT = $201B;
  WGL_ALPHA_SHIFT_EXT = $201C;
  WGL_ACCUM_BITS_EXT = $201D;
  WGL_ACCUM_RED_BITS_EXT = $201E;
  WGL_ACCUM_GREEN_BITS_EXT = $201F;
  WGL_ACCUM_BLUE_BITS_EXT = $2020;
  WGL_ACCUM_ALPHA_BITS_EXT = $2021;
  WGL_DEPTH_BITS_EXT = $2022;
  WGL_STENCIL_BITS_EXT = $2023;
  WGL_AUX_BUFFERS_EXT = $2024;
  WGL_NO_ACCELERATION_EXT = $2025;
  WGL_GENERIC_ACCELERATION_EXT = $2026;
  WGL_FULL_ACCELERATION_EXT = $2027;
  WGL_SWAP_EXCHANGE_EXT = $2028;
  WGL_SWAP_COPY_EXT = $2029;
  WGL_SWAP_UNDEFINED_EXT = $202A;
  WGL_TYPE_RGBA_EXT = $202B;
  WGL_TYPE_COLORINDEX_EXT = $202C;
var
  wglGetPixelFormatAttribivEXT: function(hdc: HDC; iPixelFormat: GLint; iLayerPlane: GLint; nAttributes: GLuint; piAttributes: PGLint; piValues: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetPixelFormatAttribfvEXT: function(hdc: HDC; iPixelFormat: GLint; iLayerPlane: GLint; nAttributes: GLuint; piAttributes: PGLint; pfValues: PGLfloat): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglChoosePixelFormatEXT: function(hdc: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLfloat; nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLuint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_EXT_pixel_format: Boolean;

//***** WGL_I3D_digital_video_control *****//
const
  WGL_DIGITAL_VIDEO_CURSOR_ALPHA_FRAMEBUFFER_I3D = $2050;
  WGL_DIGITAL_VIDEO_CURSOR_ALPHA_VALUE_I3D = $2051;
  WGL_DIGITAL_VIDEO_CURSOR_INCLUDED_I3D = $2052;
  WGL_DIGITAL_VIDEO_GAMMA_CORRECTED_I3D = $2053;
var
  wglGetDigitalVideoParametersI3D: function(hDC: HDC; iAttribute: GLint; piValue: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglSetDigitalVideoParametersI3D: function(hDC: HDC; iAttribute: GLint; const piValue: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_I3D_digital_video_control: Boolean;

//***** WGL_I3D_gamma *****//
const
  WGL_GAMMA_TABLE_SIZE_I3D = $204E;
  WGL_GAMMA_EXCLUDE_DESKTOP_I3D = $204F;
  // WGL_GAMMA_EXCLUDE_DESKTOP_I3D  { already defined }
var
  wglGetGammaTableParametersI3D: function(hDC: HDC; iAttribute: GLint; piValue: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglSetGammaTableParametersI3D: function(hDC: HDC; iAttribute: GLint; const piValue: PGLint): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetGammaTableI3D: function(hDC: HDC; iEntries: GLint; puRed: PGLUSHORT; puGreen: PGLUSHORT; puBlue: PGLUSHORT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglSetGammaTableI3D: function(hDC: HDC; iEntries: GLint; const puRed: PGLUSHORT; const puGreen: PGLUSHORT; const puBlue: PGLUSHORT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_I3D_gamma: Boolean;

//***** WGL_I3D_genlock *****//
const
  WGL_GENLOCK_SOURCE_MULTIVIEW_I3D = $2044;
  WGL_GENLOCK_SOURCE_EXTERNAL_SYNC_I3D = $2045;
  WGL_GENLOCK_SOURCE_EXTERNAL_FIELD_I3D = $2046;
  WGL_GENLOCK_SOURCE_EXTERNAL_TTL_I3D = $2047;
  WGL_GENLOCK_SOURCE_DIGITAL_SYNC_I3D = $2048;
  WGL_GENLOCK_SOURCE_DIGITAL_FIELD_I3D = $2049;
  WGL_GENLOCK_SOURCE_EDGE_FALLING_I3D = $204A;
  WGL_GENLOCK_SOURCE_EDGE_RISING_I3D = $204B;
  WGL_GENLOCK_SOURCE_EDGE_BOTH_I3D = $204C;
var
  wglEnableGenlockI3D: function(hDC: HDC): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglDisableGenlockI3D: function(hDC: HDC): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglIsEnabledGenlockI3D: function(hDC: HDC; pFlag: PBOOL): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGenlockSourceI3D: function(hDC: HDC; uSource: GLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetGenlockSourceI3D: function(hDC: HDC; uSource: PGLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGenlockSourceEdgeI3D: function(hDC: HDC; uEdge: GLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetGenlockSourceEdgeI3D: function(hDC: HDC; uEdge: PGLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGenlockSampleRateI3D: function(hDC: HDC; uRate: GLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetGenlockSampleRateI3D: function(hDC: HDC; uRate: PGLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGenlockSourceDelayI3D: function(hDC: HDC; uDelay: GLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglGetGenlockSourceDelayI3D: function(hDC: HDC; uDelay: PGLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  wglQueryGenlockMaxSourceDelayI3D: function(hDC: HDC; uMaxLineDelay: PGLUINT; uMaxPixelDelay: PGLUINT): BOOL; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_WGL_I3D_genlock: Boolean;
{$ENDIF}

//***** GL_ARB_matrix_palette *****//
const
  GL_MATRIX_PALETTE_ARB = $8840;
  GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB = $8841;
  GL_MAX_PALETTE_MATRICES_ARB = $8842;
  GL_CURRENT_PALETTE_MATRIX_ARB = $8843;
  GL_MATRIX_INDEX_ARRAY_ARB = $8844;
  GL_CURRENT_MATRIX_INDEX_ARB = $8845;
  GL_MATRIX_INDEX_ARRAY_SIZE_ARB = $8846;
  GL_MATRIX_INDEX_ARRAY_TYPE_ARB = $8847;
  GL_MATRIX_INDEX_ARRAY_STRIDE_ARB = $8848;
  GL_MATRIX_INDEX_ARRAY_POINTER_ARB = $8849;
var
  glCurrentPaletteMatrixARB: procedure(index: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMatrixIndexubvARB: procedure(size: GLint; indices: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMatrixIndexusvARB: procedure(size: GLint; indices: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMatrixIndexuivARB: procedure(size: GLint; indices: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMatrixIndexPointerARB: procedure(size: GLint; _type: GLenum; stride: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_matrix_palette: Boolean;

//***** GL_NV_element_array *****//
const
  GL_ELEMENT_ARRAY_TYPE_NV = $8769;
  GL_ELEMENT_ARRAY_POINTER_NV = $876A;
var
  glElementPointerNV: procedure(_type: GLenum; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawElementArrayNV: procedure(mode: GLenum; first: GLint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawRangeElementArrayNV: procedure(mode: GLenum; start: GLuint; _end: GLuint; first: GLint; count: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiDrawElementArrayNV: procedure(mode: GLenum; const first: PGLint; const count: PGLsizei; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiDrawRangeElementArrayNV: procedure(mode: GLenum; start: GLuint; _end: GLuint; const first: PGLint; const count: PGLsizei; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_element_array: Boolean;

//***** GL_NV_float_buffer *****//
const
  GL_FLOAT_R_NV = $8880;
  GL_FLOAT_RG_NV = $8881;
  GL_FLOAT_RGB_NV = $8882;
  GL_FLOAT_RGBA_NV = $8883;
  GL_FLOAT_R16_NV = $8884;
  GL_FLOAT_R32_NV = $8885;
  GL_FLOAT_RG16_NV = $8886;
  GL_FLOAT_RG32_NV = $8887;
  GL_FLOAT_RGB16_NV = $8888;
  GL_FLOAT_RGB32_NV = $8889;
  GL_FLOAT_RGBA16_NV = $888A;
  GL_FLOAT_RGBA32_NV = $888B;
  GL_TEXTURE_FLOAT_COMPONENTS_NV = $888C;
  GL_FLOAT_CLEAR_COLOR_VALUE_NV = $888D;
  GL_FLOAT_RGBA_MODE_NV = $888E;
{$IFDEF Windows}
  WGL_FLOAT_COMPONENTS_NV = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV = $20B4;
  WGL_TEXTURE_FLOAT_R_NV = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV = $20B8;
{$ENDIF}

function Load_GL_NV_float_buffer: Boolean;

//***** GL_NV_fragment_program *****//
const
  GL_FRAGMENT_PROGRAM_NV = $8870;
  GL_MAX_TEXTURE_COORDS_NV = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_NV = $8872;
  GL_FRAGMENT_PROGRAM_BINDING_NV = $8873;
  GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV = $8868;
  GL_PROGRAM_ERROR_STRING_NV = $8874;
var
  glProgramNamedParameter4fNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramNamedParameter4dNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramNamedParameterfvNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramNamedParameterdvNV: procedure(id: GLuint; len: GLsizei; const name: PGLubyte; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  // glProgramLocalParameter4dARB  { already defined }
  // glProgramLocalParameter4dvARB  { already defined }
  // glProgramLocalParameter4fARB  { already defined }
  // glProgramLocalParameter4fvARB  { already defined }
  // glGetProgramLocalParameterdvARB  { already defined }
  // glGetProgramLocalParameterfvARB  { already defined }

function Load_GL_NV_fragment_program: Boolean;

//***** GL_NV_primitive_restart *****//
const
  GL_PRIMITIVE_RESTART_NV = $8558;
  GL_PRIMITIVE_RESTART_INDEX_NV = $8559;
var
  glPrimitiveRestartNV: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPrimitiveRestartIndexNV: procedure(index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_primitive_restart: Boolean;

//***** GL_NV_vertex_program2 *****//

function Load_GL_NV_vertex_program2: Boolean;

{$IFDEF Windows}
//***** WGL_NV_render_texture_rectangle *****//
const
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGB_NV = $20A0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGBA_NV = $20A1;
  WGL_TEXTURE_RECTANGLE_NV = $20A2;

function Load_WGL_NV_render_texture_rectangle: Boolean;
{$ENDIF}

//***** GL_NV_pixel_data_range *****//
const
  GL_WRITE_PIXEL_DATA_RANGE_NV = $8878;
  GL_READ_PIXEL_DATA_RANGE_NV = $8879;
  GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV = $887A;
  GL_READ_PIXEL_DATA_RANGE_LENGTH_NV = $887B;
  GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV = $887C;
  GL_READ_PIXEL_DATA_RANGE_POINTER_NV = $887D;
var
  glPixelDataRangeNV: procedure(target: GLenum; length: GLsizei; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFlushPixelDataRangeNV: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  // wglAllocateMemoryNV  { already defined }
  // wglFreeMemoryNV  { already defined }

function Load_GL_NV_pixel_data_range: Boolean;

//***** GL_EXT_texture_rectangle *****//
const
  GL_TEXTURE_RECTANGLE_EXT = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_EXT = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_EXT = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_EXT = $84F8;

function Load_GL_EXT_texture_rectangle: Boolean;

//***** GL_S3_s3tc *****//
const
  GL_RGB_S3TC = $83A0;
  GL_RGB4_S3TC = $83A1;
  GL_RGBA_S3TC = $83A2;
  GL_RGBA4_S3TC = $83A3;

function Load_GL_S3_s3tc: Boolean;

//***** GL_ATI_draw_buffers *****//
const
  GL_MAX_DRAW_BUFFERS_ATI = $8824;
  GL_DRAW_BUFFER0_ATI = $8825;
  GL_DRAW_BUFFER1_ATI = $8826;
  GL_DRAW_BUFFER2_ATI = $8827;
  GL_DRAW_BUFFER3_ATI = $8828;
  GL_DRAW_BUFFER4_ATI = $8829;
  GL_DRAW_BUFFER5_ATI = $882A;
  GL_DRAW_BUFFER6_ATI = $882B;
  GL_DRAW_BUFFER7_ATI = $882C;
  GL_DRAW_BUFFER8_ATI = $882D;
  GL_DRAW_BUFFER9_ATI = $882E;
  GL_DRAW_BUFFER10_ATI = $882F;
  GL_DRAW_BUFFER11_ATI = $8830;
  GL_DRAW_BUFFER12_ATI = $8831;
  GL_DRAW_BUFFER13_ATI = $8832;
  GL_DRAW_BUFFER14_ATI = $8833;
  GL_DRAW_BUFFER15_ATI = $8834;
var
  glDrawBuffersATI: procedure(n: GLsizei; const bufs: PGLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_draw_buffers: Boolean;

{$IFDEF Windows}
//***** WGL_ATI_pixel_format_float *****//
const
  WGL_RGBA_FLOAT_MODE_ATI = $8820;
  WGL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI = $8835;
  WGL_TYPE_RGBA_FLOAT_ATI = $21A0;

function Load_WGL_ATI_pixel_format_float: Boolean;
{$ENDIF}

//***** GL_ATI_texture_env_combine3 *****//
const
  GL_MODULATE_ADD_ATI = $8744;
  GL_MODULATE_SIGNED_ADD_ATI = $8745;
  GL_MODULATE_SUBTRACT_ATI = $8746;

function Load_GL_ATI_texture_env_combine3: Boolean;

//***** GL_ATI_texture_float *****//
const
  GL_RGBA_FLOAT32_ATI = $8814;
  GL_RGB_FLOAT32_ATI = $8815;
  GL_ALPHA_FLOAT32_ATI = $8816;
  GL_INTENSITY_FLOAT32_ATI = $8817;
  GL_LUMINANCE_FLOAT32_ATI = $8818;
  GL_LUMINANCE_ALPHA_FLOAT32_ATI = $8819;
  GL_RGBA_FLOAT16_ATI = $881A;
  GL_RGB_FLOAT16_ATI = $881B;
  GL_ALPHA_FLOAT16_ATI = $881C;
  GL_INTENSITY_FLOAT16_ATI = $881D;
  GL_LUMINANCE_FLOAT16_ATI = $881E;
  GL_LUMINANCE_ALPHA_FLOAT16_ATI = $881F;

function Load_GL_ATI_texture_float: Boolean;

//***** GL_NV_texture_expand_normal *****//
const
  GL_TEXTURE_UNSIGNED_REMAP_MODE_NV = $888F;

function Load_GL_NV_texture_expand_normal: Boolean;

//***** GL_NV_half_float *****//
const
  GL_HALF_FLOAT_NV = $140B;
var
  glVertex2hNV: procedure(x: GLushort; y: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex2hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3hNV: procedure(x: GLushort; y: GLushort; z: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex3hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4hNV: procedure(x: GLushort; y: GLushort; z: GLushort; w: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertex4hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3hNV: procedure(nx: GLushort; ny: GLushort; nz: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormal3hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3hNV: procedure(red: GLushort; green: GLushort; blue: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor3hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4hNV: procedure(red: GLushort; green: GLushort; blue: GLushort; alpha: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColor4hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1hNV: procedure(s: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord1hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2hNV: procedure(s: GLushort; t: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord2hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3hNV: procedure(s: GLushort; t: GLushort; r: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord3hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4hNV: procedure(s: GLushort; t: GLushort; r: GLushort; q: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoord4hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1hNV: procedure(target: GLenum; s: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord1hvNV: procedure(target: GLenum; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2hNV: procedure(target: GLenum; s: GLushort; t: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord2hvNV: procedure(target: GLenum; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3hNV: procedure(target: GLenum; s: GLushort; t: GLushort; r: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord3hvNV: procedure(target: GLenum; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4hNV: procedure(target: GLenum; s: GLushort; t: GLushort; r: GLushort; q: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoord4hvNV: procedure(target: GLenum; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordhNV: procedure(fog: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordhvNV: procedure(const fog: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3hNV: procedure(red: GLushort; green: GLushort; blue: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3hvNV: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexWeighthNV: procedure(weight: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexWeighthvNV: procedure(const weight: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1hNV: procedure(index: GLuint; x: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1hvNV: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2hNV: procedure(index: GLuint; x: GLushort; y: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2hvNV: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3hNV: procedure(index: GLuint; x: GLushort; y: GLushort; z: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3hvNV: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4hNV: procedure(index: GLuint; x: GLushort; y: GLushort; z: GLushort; w: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4hvNV: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs1hvNV: procedure(index: GLuint; n: GLsizei; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs2hvNV: procedure(index: GLuint; n: GLsizei; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs3hvNV: procedure(index: GLuint; n: GLsizei; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribs4hvNV: procedure(index: GLuint; n: GLsizei; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_NV_half_float: Boolean;

//***** GL_ATI_map_object_buffer *****//
var
  glMapObjectBufferATI: function(buffer: GLuint): PGLvoid; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUnmapObjectBufferATI: procedure(buffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_map_object_buffer: Boolean;

//***** GL_ATI_separate_stencil *****//
const
///GL_KEEP = $1E00;
//  GL_ZERO = $0000;
// GL_REPLACE = $1E01;
//  GL_INCR = $1E02;
//  GL_DECR = $1E03;
// GL_INVERT = $150A;
//  GL_NEVER = $0200;
//  GL_LESS = $0201;
//  GL_LEQUAL = $0203;
//  GL_GREATER = $0204;
//  GL_GEQUAL = $0206;
//  GL_EQUAL = $0202;
//  GL_NOTEQUAL = $0205;
//  GL_ALWAYS = $0207;
//  GL_FRONT = $0404;
//  GL_BACK = $0405;
//  GL_FRONT_AND_BACK = $0408;
  GL_STENCIL_BACK_FUNC_ATI = $8800;
  GL_STENCIL_BACK_FAIL_ATI = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI = $8803;
var
  glStencilOpSeparateATI: procedure(face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glStencilFuncSeparateATI: procedure(frontfunc: GLenum; backfunc: GLenum; ref: GLint; mask: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_separate_stencil: Boolean;

//***** GL_ATI_vertex_attrib_array_object *****//
var
  glVertexAttribArrayObjectATI: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; buffer: GLuint; offset: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribArrayObjectfvATI: procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribArrayObjectivATI: procedure(index: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ATI_vertex_attrib_array_object: Boolean;

//***** GL_ARB_occlusion_query *****//
const
  GL_SAMPLES_PASSED_ARB = $8914;
  GL_QUERY_COUNTER_BITS_ARB = $8864;
  GL_CURRENT_QUERY_ARB = $8865;
  GL_QUERY_RESULT_ARB = $8866;
  GL_QUERY_RESULT_AVAILABLE_ARB = $8867;
var
  glGenQueriesARB: procedure(n: GLsizei; ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteQueriesARB: procedure(n: GLsizei; const ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsQueryARB: function(id: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBeginQueryARB: procedure(target: GLenum; id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndQueryARB: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryObjectivARB: procedure(id: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryObjectuivARB: procedure(id: GLuint; pname: GLenum; params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_occlusion_query: Boolean;

//***** GL_ARB_shader_objects *****//
const
  GL_PROGRAM_OBJECT_ARB = $8B40;
  GL_OBJECT_TYPE_ARB = $8B4E;
  GL_OBJECT_SUBTYPE_ARB = $8B4F;
  GL_OBJECT_DELETE_STATUS_ARB = $8B80;
  GL_OBJECT_COMPILE_STATUS_ARB = $8B81;
  GL_OBJECT_LINK_STATUS_ARB = $8B82;
  GL_OBJECT_VALIDATE_STATUS_ARB = $8B83;
  GL_OBJECT_INFO_LOG_LENGTH_ARB = $8B84;
  GL_OBJECT_ATTACHED_OBJECTS_ARB = $8B85;
  GL_OBJECT_ACTIVE_UNIFORMS_ARB = $8B86;
  GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB = $8B87;
  GL_OBJECT_SHADER_SOURCE_LENGTH_ARB = $8B88;
  GL_SHADER_OBJECT_ARB = $8B48;
//GL_FLOAT = $1406;
  GL_FLOAT_VEC2_ARB = $8B50;
  GL_FLOAT_VEC3_ARB = $8B51;
  GL_FLOAT_VEC4_ARB = $8B52;
// GL_INT = $1404;
  GL_INT_VEC2_ARB = $8B53;
  GL_INT_VEC3_ARB = $8B54;
  GL_INT_VEC4_ARB = $8B55;
  GL_BOOL_ARB = $8B56;
  GL_BOOL_VEC2_ARB = $8B57;
  GL_BOOL_VEC3_ARB = $8B58;
  GL_BOOL_VEC4_ARB = $8B59;
  GL_FLOAT_MAT2_ARB = $8B5A;
  GL_FLOAT_MAT3_ARB = $8B5B;
  GL_FLOAT_MAT4_ARB = $8B5C;
var
  glDeleteObjectARB: procedure(obj: GLhandleARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetHandleARB: function(pname: GLenum): GLhandleARB; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDetachObjectARB: procedure(containerObj: GLhandleARB; attachedObj: GLhandleARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCreateShaderObjectARB: function(shaderType: GLenum): GLhandleARB; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glShaderSourceARB: procedure(shaderObj: GLhandleARB; count: GLsizei; const _string: PGLvoid; const length: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompileShaderARB: procedure(shaderObj: GLhandleARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCreateProgramObjectARB: function(): GLhandleARB; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAttachObjectARB: procedure(containerObj: GLhandleARB; obj: GLhandleARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLinkProgramARB: procedure(programObj: GLhandleARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUseProgramObjectARB: procedure(programObj: GLhandleARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glValidateProgramARB: procedure(programObj: GLhandleARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1fARB: procedure(location: GLint; v0: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1iARB: procedure(location: GLint; v0: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2iARB: procedure(location: GLint; v0: GLint; v1: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3iARB: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4iARB: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix2fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix3fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix4fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetObjectParameterfvARB: procedure(obj: GLhandleARB; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetObjectParameterivARB: procedure(obj: GLhandleARB; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetInfoLogARB: procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; infoLog: PGLcharARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetAttachedObjectsARB: procedure(containerObj: GLhandleARB; maxCount: GLsizei; count: PGLsizei; obj: PGLhandleARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformLocationARB: function(programObj: GLhandleARB; const name: PGLcharARB): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveUniformARB: procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLcharARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformfvARB: procedure(programObj: GLhandleARB; location: GLint; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformivARB: procedure(programObj: GLhandleARB; location: GLint; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetShaderSourceARB: procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; source: PGLcharARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_shader_objects: Boolean;

//***** GL_ARB_vertex_shader *****//
const
  GL_VERTEX_SHADER_ARB = $8B31;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB = $8B4A;
  GL_MAX_VARYING_FLOATS_ARB = $8B4B;
  // GL_MAX_VERTEX_ATTRIBS_ARB  { already defined }
  // GL_MAX_TEXTURE_IMAGE_UNITS_ARB  { already defined }
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB = $8B4D;
  // GL_MAX_TEXTURE_COORDS_ARB  { already defined }
  // GL_VERTEX_PROGRAM_POINT_SIZE_ARB  { already defined }
  // GL_VERTEX_PROGRAM_TWO_SIDE_ARB  { already defined }
  // GL_OBJECT_TYPE_ARB  { already defined }
  // GL_OBJECT_SUBTYPE_ARB  { already defined }
  GL_OBJECT_ACTIVE_ATTRIBUTES_ARB = $8B89;
  GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB = $8B8A;
  // GL_SHADER_OBJECT_ARB  { already defined }
  // GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB  { already defined }
  // GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB  { already defined }
  // GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB  { already defined }
  // GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB  { already defined }
  // GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB  { already defined }
  // GL_CURRENT_VERTEX_ATTRIB_ARB  { already defined }
  // GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB  { already defined }
  // GL_FLOAT  { already defined }
  // GL_FLOAT_VEC2_ARB  { already defined }
  // GL_FLOAT_VEC3_ARB  { already defined }
  // GL_FLOAT_VEC4_ARB  { already defined }
  // GL_FLOAT_MAT2_ARB  { already defined }
  // GL_FLOAT_MAT3_ARB  { already defined }
  // GL_FLOAT_MAT4_ARB  { already defined }
  // glVertexAttrib1fARB  { already defined }
  // glVertexAttrib1sARB  { already defined }
  // glVertexAttrib1dARB  { already defined }
  // glVertexAttrib2fARB  { already defined }
  // glVertexAttrib2sARB  { already defined }
  // glVertexAttrib2dARB  { already defined }
  // glVertexAttrib3fARB  { already defined }
  // glVertexAttrib3sARB  { already defined }
  // glVertexAttrib3dARB  { already defined }
  // glVertexAttrib4fARB  { already defined }
  // glVertexAttrib4sARB  { already defined }
  // glVertexAttrib4dARB  { already defined }
  // glVertexAttrib4NubARB  { already defined }
  // glVertexAttrib1fvARB  { already defined }
  // glVertexAttrib1svARB  { already defined }
  // glVertexAttrib1dvARB  { already defined }
  // glVertexAttrib2fvARB  { already defined }
  // glVertexAttrib2svARB  { already defined }
  // glVertexAttrib2dvARB  { already defined }
  // glVertexAttrib3fvARB  { already defined }
  // glVertexAttrib3svARB  { already defined }
  // glVertexAttrib3dvARB  { already defined }
  // glVertexAttrib4fvARB  { already defined }
  // glVertexAttrib4svARB  { already defined }
  // glVertexAttrib4dvARB  { already defined }
  // glVertexAttrib4ivARB  { already defined }
  // glVertexAttrib4bvARB  { already defined }
  // glVertexAttrib4ubvARB  { already defined }
  // glVertexAttrib4usvARB  { already defined }
  // glVertexAttrib4uivARB  { already defined }
  // glVertexAttrib4NbvARB  { already defined }
  // glVertexAttrib4NsvARB  { already defined }
  // glVertexAttrib4NivARB  { already defined }
  // glVertexAttrib4NubvARB  { already defined }
  // glVertexAttrib4NusvARB  { already defined }
  // glVertexAttrib4NuivARB  { already defined }
  // glVertexAttribPointerARB  { already defined }
  // glEnableVertexAttribArrayARB  { already defined }
  // glDisableVertexAttribArrayARB  { already defined }
var
  glBindAttribLocationARB: procedure(programObj: GLhandleARB; index: GLuint; const name: PGLcharARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveAttribARB: procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLcharARB); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetAttribLocationARB: function(programObj: GLhandleARB; const name: PGLcharARB): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  // glGetVertexAttribdvARB  { already defined }
  // glGetVertexAttribfvARB  { already defined }
  // glGetVertexAttribivARB  { already defined }
  // glGetVertexAttribPointervARB  { already defined }

function Load_GL_ARB_vertex_shader: Boolean;

//***** GL_ARB_fragment_shader *****//
const
  GL_FRAGMENT_SHADER_ARB = $8B30;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB = $8B49;
  // GL_MAX_TEXTURE_COORDS_ARB  { already defined }
  // GL_MAX_TEXTURE_IMAGE_UNITS_ARB  { already defined }
  // GL_OBJECT_TYPE_ARB  { already defined }
  // GL_OBJECT_SUBTYPE_ARB  { already defined }
  // GL_SHADER_OBJECT_ARB  { already defined }

function Load_GL_ARB_fragment_shader: Boolean;

//***** GL_ARB_shading_language_100 *****//

function Load_GL_ARB_shading_language_100: Boolean;

//***** GL_ARB_texture_non_power_of_two *****//

function Load_GL_ARB_texture_non_power_of_two: Boolean;

//***** GL_ARB_point_sprite *****//
const
  GL_POINT_SPRITE_ARB = $8861;
  GL_COORD_REPLACE_ARB = $8862;

function Load_GL_ARB_point_sprite: Boolean;

//***** GL_EXT_depth_bounds_test *****//
const
  GL_DEPTH_BOUNDS_TEST_EXT = $8890;
  GL_DEPTH_BOUNDS_EXT = $8891;
var
  glDepthBoundsEXT: procedure(zmin: GLclampd; zmax: GLclampd); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_depth_bounds_test: Boolean;

//***** GL_EXT_texture_mirror_clamp *****//
const
  GL_MIRROR_CLAMP_EXT = $8742;
  GL_MIRROR_CLAMP_TO_EDGE_EXT = $8743;
  GL_MIRROR_CLAMP_TO_BORDER_EXT = $8912;

function Load_GL_EXT_texture_mirror_clamp: Boolean;

//***** GL_EXT_blend_equation_separate *****//
const
  GL_BLEND_EQUATION_RGB_EXT = $8009;
  GL_BLEND_EQUATION_ALPHA_EXT = $883D;
var
  glBlendEquationSeparateEXT: procedure(modeRGB: GLenum; modeAlpha: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_blend_equation_separate: Boolean;

//***** GL_MESA_pack_invert *****//
const
  GL_PACK_INVERT_MESA = $8758;

function Load_GL_MESA_pack_invert: Boolean;

//***** GL_MESA_ycbcr_texture *****//
const
  GL_YCBCR_MESA = $8757;
  GL_UNSIGNED_SHORT_8_8_MESA = $85BA;
  GL_UNSIGNED_SHORT_8_8_REV_MESA = $85BB;

function Load_GL_MESA_ycbcr_texture: Boolean;

//***** GL_ARB_fragment_program_shadow *****//

function Load_GL_ARB_fragment_program_shadow: Boolean;

//***** GL_NV_fragment_program_option *****//

function Load_GL_NV_fragment_program_option: Boolean;

//***** GL_EXT_pixel_buffer_object *****//
const
  GL_PIXEL_PACK_BUFFER_EXT = $88EB;
  GL_PIXEL_UNPACK_BUFFER_EXT = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING_EXT = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING_EXT = $88EF;

function Load_GL_EXT_pixel_buffer_object: Boolean;

//***** GL_NV_fragment_program2 *****//
const
  GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV = $88F4;
  GL_MAX_PROGRAM_CALL_DEPTH_NV = $88F5;
  GL_MAX_PROGRAM_IF_DEPTH_NV = $88F6;
  GL_MAX_PROGRAM_LOOP_DEPTH_NV = $88F7;
  GL_MAX_PROGRAM_LOOP_COUNT_NV = $88F8;

function Load_GL_NV_fragment_program2: Boolean;

//***** GL_NV_vertex_program2_option *****//
  // GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV  { already defined }
  // GL_MAX_PROGRAM_CALL_DEPTH_NV  { already defined }

function Load_GL_NV_vertex_program2_option: Boolean;

//***** GL_NV_vertex_program3 *****//
  // GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB  { already defined }

function Load_GL_NV_vertex_program3: Boolean;

//***** GL_ARB_draw_buffers *****//
const
  GL_MAX_DRAW_BUFFERS_ARB = $8824;
  GL_DRAW_BUFFER0_ARB = $8825;
  GL_DRAW_BUFFER1_ARB = $8826;
  GL_DRAW_BUFFER2_ARB = $8827;
  GL_DRAW_BUFFER3_ARB = $8828;
  GL_DRAW_BUFFER4_ARB = $8829;
  GL_DRAW_BUFFER5_ARB = $882A;
  GL_DRAW_BUFFER6_ARB = $882B;
  GL_DRAW_BUFFER7_ARB = $882C;
  GL_DRAW_BUFFER8_ARB = $882D;
  GL_DRAW_BUFFER9_ARB = $882E;
  GL_DRAW_BUFFER10_ARB = $882F;
  GL_DRAW_BUFFER11_ARB = $8830;
  GL_DRAW_BUFFER12_ARB = $8831;
  GL_DRAW_BUFFER13_ARB = $8832;
  GL_DRAW_BUFFER14_ARB = $8833;
  GL_DRAW_BUFFER15_ARB = $8834;
var
  glDrawBuffersARB: procedure(n: GLsizei; const bufs: PGLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_draw_buffers: Boolean;

//***** GL_ARB_texture_rectangle *****//
const
  GL_TEXTURE_RECTANGLE_ARB = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_ARB = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_ARB = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB = $84F8;

function Load_GL_ARB_texture_rectangle: Boolean;

//***** GL_ARB_color_buffer_float *****//
const
  GL_RGBA_FLOAT_MODE_ARB = $8820;
  GL_CLAMP_VERTEX_COLOR_ARB = $891A;
  GL_CLAMP_FRAGMENT_COLOR_ARB = $891B;
  GL_CLAMP_READ_COLOR_ARB = $891C;
  GL_FIXED_ONLY_ARB = $891D;
  WGL_TYPE_RGBA_FLOAT_ARB = $21A0;
var
  glClampColorARB: procedure(target: GLenum; clamp: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_color_buffer_float: Boolean;

//***** GL_ARB_half_float_pixel *****//
const
  GL_HALF_FLOAT_ARB = $140B;

function Load_GL_ARB_half_float_pixel: Boolean;

//***** GL_ARB_texture_float *****//
const
  GL_TEXTURE_RED_TYPE_ARB = $8C10;
  GL_TEXTURE_GREEN_TYPE_ARB = $8C11;
  GL_TEXTURE_BLUE_TYPE_ARB = $8C12;
  GL_TEXTURE_ALPHA_TYPE_ARB = $8C13;
  GL_TEXTURE_LUMINANCE_TYPE_ARB = $8C14;
  GL_TEXTURE_INTENSITY_TYPE_ARB = $8C15;
  GL_TEXTURE_DEPTH_TYPE_ARB = $8C16;
  GL_UNSIGNED_NORMALIZED_ARB = $8C17;
  GL_RGBA32F_ARB = $8814;
  GL_RGB32F_ARB = $8815;
  GL_ALPHA32F_ARB = $8816;
  GL_INTENSITY32F_ARB = $8817;
  GL_LUMINANCE32F_ARB = $8818;
  GL_LUMINANCE_ALPHA32F_ARB = $8819;
  GL_RGBA16F_ARB = $881A;
  GL_RGB16F_ARB = $881B;
  GL_ALPHA16F_ARB = $881C;
  GL_INTENSITY16F_ARB = $881D;
  GL_LUMINANCE16F_ARB = $881E;
  GL_LUMINANCE_ALPHA16F_ARB = $881F;

function Load_GL_ARB_texture_float: Boolean;

//***** GL_EXT_texture_compression_dxt1 *****//
  // GL_COMPRESSED_RGB_S3TC_DXT1_EXT  { already defined }
  // GL_COMPRESSED_RGBA_S3TC_DXT1_EXT  { already defined }

function Load_GL_EXT_texture_compression_dxt1: Boolean;

//***** GL_ARB_pixel_buffer_object *****//
const
  GL_PIXEL_PACK_BUFFER_ARB = $88EB;
  GL_PIXEL_UNPACK_BUFFER_ARB = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING_ARB = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING_ARB = $88EF;

function Load_GL_ARB_pixel_buffer_object: Boolean;

//***** GL_EXT_framebuffer_object *****//
const
  GL_FRAMEBUFFER_EXT = $8D40;
  GL_RENDERBUFFER_EXT = $8D41;
  GL_STENCIL_INDEX_EXT = $8D45;
  GL_STENCIL_INDEX1_EXT = $8D46;
  GL_STENCIL_INDEX4_EXT = $8D47;
  GL_STENCIL_INDEX8_EXT = $8D48;
  GL_STENCIL_INDEX16_EXT = $8D49;
  GL_RENDERBUFFER_WIDTH_EXT = $8D42;
  GL_RENDERBUFFER_HEIGHT_EXT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT_EXT = $8D44;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT = $8CD4;
  GL_COLOR_ATTACHMENT0_EXT = $8CE0;
  GL_COLOR_ATTACHMENT1_EXT = $8CE1;
  GL_COLOR_ATTACHMENT2_EXT = $8CE2;
  GL_COLOR_ATTACHMENT3_EXT = $8CE3;
  GL_COLOR_ATTACHMENT4_EXT = $8CE4;
  GL_COLOR_ATTACHMENT5_EXT = $8CE5;
  GL_COLOR_ATTACHMENT6_EXT = $8CE6;
  GL_COLOR_ATTACHMENT7_EXT = $8CE7;
  GL_COLOR_ATTACHMENT8_EXT = $8CE8;
  GL_COLOR_ATTACHMENT9_EXT = $8CE9;
  GL_COLOR_ATTACHMENT10_EXT = $8CEA;
  GL_COLOR_ATTACHMENT11_EXT = $8CEB;
  GL_COLOR_ATTACHMENT12_EXT = $8CEC;
  GL_COLOR_ATTACHMENT13_EXT = $8CED;
  GL_COLOR_ATTACHMENT14_EXT = $8CEE;
  GL_COLOR_ATTACHMENT15_EXT = $8CEF;
  GL_DEPTH_ATTACHMENT_EXT = $8D00;
  GL_STENCIL_ATTACHMENT_EXT = $8D20;
  GL_FRAMEBUFFER_COMPLETE_EXT = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = $8CD8;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT = $8CD9;
  GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT = $8CDA;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED_EXT = $8CDD;
  GL_FRAMEBUFFER_STATUS_ERROR_EXT = $8CDE;
  GL_FRAMEBUFFER_BINDING_EXT = $8CA6;
  GL_RENDERBUFFER_BINDING_EXT = $8CA7;
  GL_MAX_COLOR_ATTACHMENTS_EXT = $8CDF;
  GL_MAX_RENDERBUFFER_SIZE_EXT = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION_EXT = $0506;
var
  glIsRenderbufferEXT: function(renderbuffer: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindRenderbufferEXT: procedure(target: GLenum; renderbuffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteRenderbuffersEXT: procedure(n: GLsizei; const renderbuffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenRenderbuffersEXT: procedure(n: GLsizei; renderbuffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRenderbufferStorageEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetRenderbufferParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsFramebufferEXT: function(framebuffer: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindFramebufferEXT: procedure(target: GLenum; framebuffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteFramebuffersEXT: procedure(n: GLsizei; const framebuffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenFramebuffersEXT: procedure(n: GLsizei; framebuffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCheckFramebufferStatusEXT: function(target: GLenum): GLenum; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferTexture1DEXT: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferTexture2DEXT: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferTexture3DEXT: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferRenderbufferEXT: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetFramebufferAttachmentParameterivEXT: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenerateMipmapEXT: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_EXT_framebuffer_object: Boolean;

//**** GL_ARB_framebuffer_object *****//
const
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;
  GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
  GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
  GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
  GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
  GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
  GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
  GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
  GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
  GL_FRAMEBUFFER_DEFAULT = $8218;
  GL_FRAMEBUFFER_UNDEFINED = $8219;
  GL_DEPTH_STENCIL_ATTACHMENT = $821A;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_TEXTURE_STENCIL_SIZE = $88F1;
  GL_TEXTURE_RED_TYPE = $8C10;
  GL_TEXTURE_GREEN_TYPE = $8C11;
  GL_TEXTURE_BLUE_TYPE = $8C12;
  GL_TEXTURE_ALPHA_TYPE = $8C13;
  GL_TEXTURE_DEPTH_TYPE = $8C16;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_DRAW_FRAMEBUFFER_BINDING = GL_FRAMEBUFFER_BINDING;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_STENCIL_INDEX1 = $8D46;
  GL_STENCIL_INDEX4 = $8D47;
  GL_STENCIL_INDEX8 = $8D48;
  GL_STENCIL_INDEX16 = $8D49;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;
var
  glIsRenderbuffer: function(renderbuffer: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindRenderbuffer: procedure(target: GLenum; renderbuffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteRenderbuffers: procedure(n: GLsizei; const renderbuffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRenderbufferStorage: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetRenderbufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsFramebuffer: function(framebuffer: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindFramebuffer: procedure(target: GLenum; framebuffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteFramebuffers: procedure(n: GLsizei; const framebuffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCheckFramebufferStatus: function(target: GLenum): GLenum; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferTexture1D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferTexture2D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferTexture3D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferRenderbuffer: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetFramebufferAttachmentParameteriv: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenerateMipmap: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBlitFramebuffer: procedure(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glRenderbufferStorageMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferTextureLayer: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

{ Using LoadAsCore = true means that we will *not* check
  if the extension is advertised in glGetString(GL_EXTENSIONS) ansistring.
  This allows to successfully Load_GL_version_3_0 in an OpenGL 3.0
  forward-compatible context, where the "core extensions" do not have
  to be mentioned inside glGetString(GL_EXTENSIONS). }

function Load_GL_ARB_framebuffer_object(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_framebuffer_object DEPRECATED *****//
const
  GL_INDEX = $8222;
  GL_TEXTURE_LUMINANCE_TYPE = $8C14;
  GL_TEXTURE_INTENSITY_TYPE = $8C15;


//**** GL_ARB_map_buffer_range *****//
const
  GL_MAP_READ_BIT = $0001;
  GL_MAP_WRITE_BIT = $0002;
  GL_MAP_INVALIDATE_RANGE_BIT = $0004;
  GL_MAP_INVALIDATE_BUFFER_BIT = $0008;
  GL_MAP_FLUSH_EXPLICIT_BIT = $0010;
  GL_MAP_UNSYNCHRONIZED_BIT = $0020;
var
  glMapBufferRange: function(target: GLenum; offset: GLintptr; length: GLsizeiptr; access: GLbitfield): PGLvoid; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFlushMappedBufferRange: procedure(target: GLenum; offset: GLintptr; length: GLsizeiptr); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_map_buffer_range(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_vertex_array_object *****//
const
  GL_VERTEX_ARRAY_BINDING = $85B5;
var
  glBindVertexArray: procedure(_array: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteVertexArrays: procedure(n: GLsizei; const arrays: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenVertexArrays: procedure(n: GLsizei; arrays: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsVertexArray: function(_array: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_vertex_array_object(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_copy_buffer *****//
const
  GL_COPY_READ_BUFFER = $8F36;
  GL_COPY_WRITE_BUFFER = $8F37;
var
  glCopyBufferSubData: procedure(readTarget: GLenum; writeTarget: GLenum; readOffset: GLintptr; writeOffset: GLintptr; size: GLsizeiptr); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_copy_buffer(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_uniform_buffer_object *****//
const
  GL_UNIFORM_BUFFER = $8A11;
  GL_UNIFORM_BUFFER_BINDING = $8A28;
  GL_UNIFORM_BUFFER_START = $8A29;
  GL_UNIFORM_BUFFER_SIZE = $8A2A;
  GL_MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
  GL_MAX_GEOMETRY_UNIFORM_BLOCKS = $8A2C;
  GL_MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
  GL_MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
  GL_MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
  GL_MAX_UNIFORM_BLOCK_SIZE = $8A30;
  GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
  GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS = $8A32;
  GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
  GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
  GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH = $8A35;
  GL_ACTIVE_UNIFORM_BLOCKS = $8A36;
  GL_UNIFORM_TYPE = $8A37;
  GL_UNIFORM_SIZE = $8A38;
  GL_UNIFORM_NAME_LENGTH = $8A39;
  GL_UNIFORM_BLOCK_INDEX = $8A3A;
  GL_UNIFORM_OFFSET = $8A3B;
  GL_UNIFORM_ARRAY_STRIDE = $8A3C;
  GL_UNIFORM_MATRIX_STRIDE = $8A3D;
  GL_UNIFORM_IS_ROW_MAJOR = $8A3E;
  GL_UNIFORM_BLOCK_BINDING = $8A3F;
  GL_UNIFORM_BLOCK_DATA_SIZE = $8A40;
  GL_UNIFORM_BLOCK_NAME_LENGTH = $8A41;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
  GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
  GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER = $8A45;
  GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
  GL_INVALID_INDEX = DWord($FFFFFFFF);
var
  glGetUniformIndices: procedure(_program: GLuint; uniformCount: GLsizei; const uniformNames: PPGLchar; uniformIndices: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveUniformsiv: procedure(_program: GLuint; uniformCount: GLsizei; const uniformIndices: PGLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveUniformName: procedure(_program: GLuint; uniformIndex: GLuint; bufSize: GLsizei; length: PGLsizei; uniformName: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformBlockIndex: function(_program: GLuint; const uniformBlockName: PGLchar): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveUniformBlockiv: procedure(_program: GLuint; uniformBlockIndex: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveUniformBlockName: procedure(_program: GLuint; uniformBlockIndex: GLuint; bufSize: GLsizei; length: PGLsizei; uniformBlockName: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformBlockBinding: procedure(_program: GLuint; uniformBlockIndex: GLuint; uniformBlockBinding: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_uniform_buffer_object(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_draw_elements_base_vertex *****//
var
  glDrawElementsBaseVertex: procedure(mode: GLenum; count: GLsizei; _type: GLenum; const indices: PGLvoid; basevertex: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawRangeElementsBaseVertex: procedure(mode: GLenum; start: GLuint; _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid; basevertex: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawElementsInstancedBaseVertex: procedure(mode: GLenum; count: GLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei; basevertex: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiDrawElementsBaseVertex: procedure(mode: GLenum; const count: PGLsizei; _type: GLenum; const indices: PPGLvoid; primcount: GLsizei; const basevertex: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_draw_elements_base_vertex(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_provoking_vertex *****//
const
  GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION = $8E4C;
  GL_FIRST_VERTEX_CONVENTION = $8E4D;
  GL_LAST_VERTEX_CONVENTION = $8E4E;
  GL_PROVOKING_VERTEX = $8E4F;
var
  glProvokingVertex: procedure(mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_provoking_vertex(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_sync *****//
type
  GLsync = Pointer;
  TGLsync = GLSync;
  PGLsync = ^GLSync;

const
  GL_MAX_SERVER_WAIT_TIMEOUT = $9111;
  GL_OBJECT_TYPE = $9112;
  GL_SYNC_CONDITION = $9113;
  GL_SYNC_STATUS = $9114;
  GL_SYNC_FLAGS = $9115;
  GL_SYNC_FENCE = $9116;
  GL_SYNC_GPU_COMMANDS_COMPLETE = $9117;
  GL_UNSIGNALED = $9118;
  GL_SIGNALED = $9119;
  GL_ALREADY_SIGNALED = $911A;
  GL_TIMEOUT_EXPIRED = $911B;
  GL_CONDITION_SATISFIED = $911C;
  GL_WAIT_FAILED = $911D;
  GL_SYNC_FLUSH_COMMANDS_BIT = $00000001;
  GL_TIMEOUT_IGNORED = GLUInt64($FFFFFFFFFFFFFFFF);
var
  glFenceSync: function(condition: GLenum; flags: GLbitfield): GLsync; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsSync: function(sync: GLsync): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteSync: procedure(sync: GLsync); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClientWaitSync: function(sync: GLsync; flags: GLbitfield; timeout: GLuint64): GLenum; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWaitSync: procedure(sync: GLsync; flags: GLbitfield; timeout: GLuint64); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetInteger64v: procedure(pname: GLenum; params: PGLint64); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetSynciv: procedure(sync: GLsync; pname: GLenum; bufSize: GLsizei; length: PGLsizei; values: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_sync(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_texture_multisample *****//
const
  GL_SAMPLE_POSITION = $8E50;
  GL_SAMPLE_MASK = $8E51;
  GL_SAMPLE_MASK_VALUE = $8E52;
  GL_MAX_SAMPLE_MASK_WORDS = $8E59;
  GL_TEXTURE_2D_MULTISAMPLE = $9100;
  GL_PROXY_TEXTURE_2D_MULTISAMPLE = $9101;
  GL_TEXTURE_2D_MULTISAMPLE_ARRAY = $9102;
  GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY = $9103;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE = $9104;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY = $9105;
  GL_TEXTURE_SAMPLES = $9106;
  GL_TEXTURE_FIXED_SAMPLE_LOCATIONS = $9107;
  GL_SAMPLER_2D_MULTISAMPLE = $9108;
  GL_INT_SAMPLER_2D_MULTISAMPLE = $9109;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE = $910A;
  GL_SAMPLER_2D_MULTISAMPLE_ARRAY = $910B;
  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910C;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910D;
  GL_MAX_COLOR_TEXTURE_SAMPLES = $910E;
  GL_MAX_DEPTH_TEXTURE_SAMPLES = $910F;
  GL_MAX_INTEGER_SAMPLES = $9110;
var
  glTexImage2DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLint; width: GLsizei; height: GLsizei; fixedsamplelocations: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexImage3DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; fixedsamplelocations: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetMultisamplefv: procedure(pname: GLenum; index: GLuint; val: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSampleMaski: procedure(index: GLuint; mask: GLbitfield); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_texture_multisample(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_blend_func_extended *****//
const
  GL_SRC1_COLOR = $88F9;
// reuse GL_SRC1_ALPHA
const
  GL_ONE_MINUS_SRC1_COLOR = $88FA;
  GL_ONE_MINUS_SRC1_ALPHA = $88FB;
  GL_MAX_DUAL_SOURCE_DRAW_BUFFERS = $88FC;
var
  glBindFragDataLocationIndexed: procedure(_program: GLuint; colorNumber: GLuint; index: GLuint; const name: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetFragDataIndex: function(_program: GLuint; const name: PGLchar): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_blend_func_extended(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_sampler_objects *****//
const
  GL_SAMPLER_BINDING = $8919;
var
  glGenSamplers: procedure(count: GLsizei; samplers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteSamplers: procedure(count: GLsizei; const samplers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsSampler: function(sampler: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindSampler: procedure(_unit: GLenum; sampler: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSamplerParameteri: procedure(sampler: GLuint; pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSamplerParameteriv: procedure(sampler: GLuint; pname: GLenum; const param: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSamplerParameterf: procedure(sampler: GLuint; pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSamplerParameterfv: procedure(sampler: GLuint; pname: GLenum; const param: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSamplerParameterIiv: procedure(sampler: GLuint; pname: GLenum; const param: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSamplerParameterIuiv: procedure(sampler: GLuint; pname: GLenum; const param: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetSamplerParameteriv: procedure(sampler: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetSamplerParameterIiv: procedure(sampler: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetSamplerParameterfv: procedure(sampler: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetSamplerParameterIuiv: procedure(sampler: GLuint; pname: GLenum; params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_sampler_objects(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_timer_query *****//
const
  GL_TIME_ELAPSED = $88BF;
  GL_TIMESTAMP = $8E28;
var
  glQueryCounter: procedure(id: GLuint; target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryObjecti64v: procedure(id: GLuint; pname: GLenum; params: PGLint64); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryObjectui64v: procedure(id: GLuint; pname: GLenum; params: PGLuint64); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_timer_query(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_vertex_type_2_10_10_10_rev *****//

const
  GL_INT_2_10_10_10_REV = $8D9F;
var
  glVertexP2ui: procedure(_type: GLenum; value: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexP2uiv: procedure(_type: GLenum; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexP3ui: procedure(_type: GLenum; value: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexP3uiv: procedure(_type: GLenum; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexP4ui: procedure(_type: GLenum; value: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexP4uiv: procedure(_type: GLenum; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordP1ui: procedure(_type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordP1uiv: procedure(_type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordP2ui: procedure(_type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordP2uiv: procedure(_type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordP3ui: procedure(_type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordP3uiv: procedure(_type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordP4ui: procedure(_type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexCoordP4uiv: procedure(_type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoordP1ui: procedure(texture: GLenum; _type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoordP1uiv: procedure(texture: GLenum; _type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoordP2ui: procedure(texture: GLenum; _type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoordP2uiv: procedure(texture: GLenum; _type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoordP3ui: procedure(texture: GLenum; _type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoordP3uiv: procedure(texture: GLenum; _type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoordP4ui: procedure(texture: GLenum; _type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiTexCoordP4uiv: procedure(texture: GLenum; _type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalP3ui: procedure(_type: GLenum; coords: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glNormalP3uiv: procedure(_type: GLenum; const coords: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorP3ui: procedure(_type: GLenum; color: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorP3uiv: procedure(_type: GLenum; const color: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorP4ui: procedure(_type: GLenum; color: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glColorP4uiv: procedure(_type: GLenum; const color: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColorP3ui: procedure(_type: GLenum; color: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColorP3uiv: procedure(_type: GLenum; const color: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribP1ui: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; value: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribP1uiv: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribP2ui: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; value: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribP2uiv: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribP3ui: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; value: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribP3uiv: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribP4ui: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; value: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribP4uiv: procedure(index: GLuint; _type: GLenum; normalized: GLboolean; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_vertex_type_2_10_10_10_rev(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_gpu_shader_fp64 *****//
// reuse GL_DOUBLE
const
  GL_DOUBLE_VEC2 = $8FFC;
  GL_DOUBLE_VEC3 = $8FFD;
  GL_DOUBLE_VEC4 = $8FFE;
  GL_DOUBLE_MAT2 = $8F46;
  GL_DOUBLE_MAT3 = $8F47;
  GL_DOUBLE_MAT4 = $8F48;
  GL_DOUBLE_MAT2x3 = $8F49;
  GL_DOUBLE_MAT2x4 = $8F4A;
  GL_DOUBLE_MAT3x2 = $8F4B;
  GL_DOUBLE_MAT3x4 = $8F4C;
  GL_DOUBLE_MAT4x2 = $8F4D;
  GL_DOUBLE_MAT4x3 = $8F4E;
var
  glUniform1d: procedure(location: GLint; x: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2d: procedure(location: GLint; x: GLdouble; y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3d: procedure(location: GLint; x: GLdouble; y: GLdouble; z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4d: procedure(location: GLint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1dv: procedure(location: GLint; count: GLsizei; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2dv: procedure(location: GLint; count: GLsizei; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3dv: procedure(location: GLint; count: GLsizei; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4dv: procedure(location: GLint; count: GLsizei; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix2x3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix2x4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix3x2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix3x4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix4x2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix4x3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformdv: procedure(_program: GLuint; location: GLint; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  
  { All of the following ProgramUniform* functions are supported if and only
    if EXT_direct_state_access is supported.
    (See http://www.opengl.org/registry/specs/ARB/gpu_shader_fp64.txt)

    Load_GL_ARB_gpu_shader_fp64 will try to load them, but their presence/absence
    will have no effect on the result of Load_GL_ARB_gpu_shader_fp64 and
    Load_GL_VERSION_4_0 functions. (Because they are not mandatory parts of
    the extension or OpenGL 4.0 core spec.) }   
  
  glProgramUniform1dEXT: procedure(_program: GLuint; location: GLint; x: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniform2dEXT: procedure(_program: GLuint; location: GLint; x: GLdouble; y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniform3dEXT: procedure(_program: GLuint; location: GLint; x: GLdouble; y: GLdouble; z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniform4dEXT: procedure(_program: GLuint; location: GLint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniform1dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniform2dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniform3dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniform4dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix2dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix3dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix4dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix2x3dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix2x4dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix3x2dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix3x4dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix4x2dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramUniformMatrix4x3dvEXT: procedure(_program: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_gpu_shader_fp64(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_shader_subroutine *****//
const
  GL_ACTIVE_SUBROUTINES = $8DE5;
  GL_ACTIVE_SUBROUTINE_UNIFORMS = $8DE6;
  GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS = $8E47;
  GL_ACTIVE_SUBROUTINE_MAX_LENGTH = $8E48;
  GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH = $8E49;
  GL_MAX_SUBROUTINES = $8DE7;
  GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS = $8DE8;
  GL_NUM_COMPATIBLE_SUBROUTINES = $8E4A;
  GL_COMPATIBLE_SUBROUTINES = $8E4B;

var
  glGetSubroutineUniformLocation: function(_program: GLuint; shadertype: GLenum; const name: PGLchar): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetSubroutineIndex: function(_program: GLuint; shadertype: GLenum; const name: PGLchar): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveSubroutineUniformiv: procedure(_program: GLuint; shadertype: GLenum; index: GLuint; pname: GLenum; values: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveSubroutineUniformName: procedure(_program: GLuint; shadertype: GLenum; index: GLuint; bufsize: GLsizei; length: PGLsizei; name: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveSubroutineName: procedure(_program: GLuint; shadertype: GLenum; index: GLuint; bufsize: GLsizei; length: PGLsizei; name: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformSubroutinesuiv: procedure(shadertype: GLenum; count: GLsizei; const indices: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformSubroutineuiv: procedure(shadertype: GLenum; location: GLint; params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramStageiv: procedure(_program: GLuint; shadertype: GLenum; pname: GLenum; values: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_shader_subroutine(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_tessellation_shader *****//
const
  GL_PATCHES = $000E;
  GL_PATCH_VERTICES = $8E72;
  GL_PATCH_DEFAULT_INNER_LEVEL = $8E73;
  GL_PATCH_DEFAULT_OUTER_LEVEL = $8E74;
  GL_TESS_CONTROL_OUTPUT_VERTICES = $8E75;
  GL_TESS_GEN_MODE = $8E76;
  GL_TESS_GEN_SPACING = $8E77;
  GL_TESS_GEN_VERTEX_ORDER = $8E78;
  GL_TESS_GEN_POINT_MODE = $8E79;
// reuse GL_TRIANGLES
// reuse GL_QUADS
const
  GL_ISOLINES = $8E7A;
// reuse GL_EQUAL
const
  GL_FRACTIONAL_ODD = $8E7B;
  GL_FRACTIONAL_EVEN = $8E7C;
// reuse GL_CCW
// reuse GL_CW
const
  GL_MAX_PATCH_VERTICES = $8E7D;
  GL_MAX_TESS_GEN_LEVEL = $8E7E;
  GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS = $8E7F;
  GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E80;
  GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS = $8E81;
  GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS = $8E82;
  GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS = $8E83;
  GL_MAX_TESS_PATCH_COMPONENTS = $8E84;
  GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS = $8E85;
  GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS = $8E86;
  GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS = $8E89;
  GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS = $8E8A;
  GL_MAX_TESS_CONTROL_INPUT_COMPONENTS = $886C;
  GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS = $886D;
  GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS = $8E1E;
  GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E1F;
  GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER = $84F0;
  GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER = $84F1;
  GL_TESS_EVALUATION_SHADER = $8E87;
  GL_TESS_CONTROL_SHADER = $8E88;
var
  glPatchParameteri: procedure(pname: GLenum; value: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPatchParameterfv: procedure(pname: GLenum; const values: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_tessellation_shader(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_transform_feedback2 *****//
const
  GL_TRANSFORM_FEEDBACK = $8E22;
  GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED = $8E23;
  GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE = $8E24;
  GL_TRANSFORM_FEEDBACK_BINDING = $8E25;
var
  glBindTransformFeedback: procedure(target: GLenum; id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteTransformFeedbacks: procedure(n: GLsizei; const ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenTransformFeedbacks: procedure(n: GLsizei; ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsTransformFeedback: function(id: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPauseTransformFeedback: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glResumeTransformFeedback: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawTransformFeedback: procedure(mode: GLenum; id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_transform_feedback2(LoadAsCore: boolean = false): Boolean;

//**** GL_ARB_transform_feedback3 *****//
const
  GL_MAX_TRANSFORM_FEEDBACK_BUFFERS = $8E70;
var
  glDrawTransformFeedbackStream: procedure(mode: GLenum; id: GLuint; stream: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBeginQueryIndexed: procedure(target: GLenum; index: GLuint; id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndQueryIndexed: procedure(target: GLenum; index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryIndexediv: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_transform_feedback3(LoadAsCore: boolean = false): Boolean;

//***  GL_ARB_get_program_binary **+/

const
  GL_PROGRAM_BINARY_RETRIEVABLE_HINT = $8257;
  GL_PROGRAM_BINARY_LENGTH = $8741;
  GL_NUM_PROGRAM_BINARY_FORMATS = $87fe;
  GL_PROGRAM_BINARY_FORMATS = $87ff;

var
  glGetProgramBinary : procedure(_program:GLuint; bufSize:GLsizei; length:pGLsizei; binaryFormat:pGLenum; binary:pointer); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramBinary : procedure(_program:GLuint; binaryFormat:GLenum; binary:pointer; length:GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_ARB_get_program_binary(LoadAsCore: boolean = false): Boolean;

//***** GL_version_1_4 *****//
const
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_POINT_SIZE_MIN = $8126;
  GL_POINT_SIZE_MAX = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE = $8128;
  GL_POINT_DISTANCE_ATTENUATION = $8129;
  GL_GENERATE_MIPMAP = $8191;
  GL_GENERATE_MIPMAP_HINT = $8192;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_DEPTH_COMPONENT32 = $81A7;
  GL_MIRRORED_REPEAT = $8370;
  GL_FOG_COORDINATE_SOURCE = $8450;
  GL_FOG_COORDINATE = $8451;
  GL_FRAGMENT_DEPTH = $8452;
  GL_CURRENT_FOG_COORDINATE = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER = $8456;
  GL_FOG_COORDINATE_ARRAY = $8457;
  GL_COLOR_SUM = $8458;
  GL_CURRENT_SECONDARY_COLOR = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER = $845D;
  GL_SECONDARY_COLOR_ARRAY = $845E;
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
  GL_TEXTURE_FILTER_CONTROL = $8500;
  GL_TEXTURE_LOD_BIAS = $8501;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;
  GL_TEXTURE_DEPTH_SIZE = $884A;
  GL_DEPTH_TEXTURE_MODE = $884B;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_COMPARE_R_TO_TEXTURE = $884E;
var
  glBlendFuncSeparate: procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordf: procedure(coord: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordfv: procedure(const coord: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordd: procedure(coord: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoorddv: procedure(const coord: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFogCoordPointer: procedure(_type: GLenum; stride: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiDrawArrays: procedure(mode: GLenum; first: PGLint; count: PGLsizei; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMultiDrawElements: procedure(mode: GLenum; const count: PGLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPointParameterf: procedure(pname: GLenum; param: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPointParameterfv: procedure(pname: GLenum; const params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPointParameteri: procedure(pname: GLenum; param: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPointParameteriv: procedure(pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3b: procedure(red: GLbyte; green: GLbyte; blue: GLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3bv: procedure(const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3d: procedure(red: GLdouble; green: GLdouble; blue: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3f: procedure(red: GLfloat; green: GLfloat; blue: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3i: procedure(red: GLint; green: GLint; blue: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3s: procedure(red: GLshort; green: GLshort; blue: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3ub: procedure(red: GLubyte; green: GLubyte; blue: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3ubv: procedure(const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3ui: procedure(red: GLuint; green: GLuint; blue: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3uiv: procedure(const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3us: procedure(red: GLushort; green: GLushort; blue: GLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColor3usv: procedure(const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glSecondaryColorPointer: procedure(size: GLint; _type: GLenum; stride: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2d: procedure(x: GLdouble; y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2f: procedure(x: GLfloat; y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2i: procedure(x: GLint; y: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2s: procedure(x: GLshort; y: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos2sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3d: procedure(x: GLdouble; y: GLdouble; z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3dv: procedure(const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3f: procedure(x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3fv: procedure(const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3i: procedure(x: GLint; y: GLint; z: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3iv: procedure(const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3s: procedure(x: GLshort; y: GLshort; z: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glWindowPos3sv: procedure(const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_version_1_4: Boolean;

//***** GL_version_1_5 *****//
const
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_QUERY_COUNTER_BITS = $8864;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY = $88B8;
  GL_WRITE_ONLY = $88B9;
  GL_READ_WRITE = $88BA;
  GL_BUFFER_ACCESS = $88BB;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_DRAW = $88E0;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_DRAW = $88E4;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_DRAW = $88E8;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;
  GL_SAMPLES_PASSED = $8914;
  GL_FOG_COORD_SRC = $8450;
  GL_FOG_COORD = $8451;
  GL_CURRENT_FOG_COORD = $8453;
  GL_FOG_COORD_ARRAY_TYPE = $8454;
  GL_FOG_COORD_ARRAY_STRIDE = $8455;
  GL_FOG_COORD_ARRAY_POINTER = $8456;
  GL_FOG_COORD_ARRAY = $8457;
  GL_FOG_COORD_ARRAY_BUFFER_BINDING = $889D;
  GL_SRC0_RGB = $8580;
  GL_SRC1_RGB = $8581;
  GL_SRC2_RGB = $8582;
  GL_SRC0_ALPHA = $8588;
  GL_SRC1_ALPHA = $8589;
  GL_SRC2_ALPHA = $858A;
var
  glGenQueries: procedure(n: GLsizei; ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteQueries: procedure(n: GLsizei; const ids: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsQuery: function(id: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBeginQuery: procedure(target: GLenum; id: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndQuery: procedure(target: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryiv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryObjectiv: procedure(id: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetQueryObjectuiv: procedure(id: GLuint; pname: GLenum; params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindBuffer: procedure(target: GLenum; buffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteBuffers: procedure(n: GLsizei; const buffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGenBuffers: procedure(n: GLsizei; buffers: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsBuffer: function(buffer: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBufferData: procedure(target: GLenum; size: GLsizeiptr; const data: PGLvoid; usage: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBufferSubData: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; const data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetBufferSubData: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glMapBuffer: function(target: GLenum; access: GLenum): PGLvoid; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUnmapBuffer: function(target: GLenum): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetBufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetBufferPointerv: procedure(target: GLenum; pname: GLenum; params: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_version_1_5: Boolean;

//***** GL_version_2_0 *****//
const
  GL_BLEND_EQUATION_RGB = $8009;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  GL_VERTEX_PROGRAM_POINT_SIZE = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;
  GL_BLEND_EQUATION_ALPHA = $883D;
  GL_POINT_SPRITE = $8861;
  GL_COORD_REPLACE = $8862;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_MAX_TEXTURE_COORDS = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_MAX_VARYING_FLOATS = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_SHADER_TYPE = $8B4F;
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_1D = $8B5D;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_3D = $8B5F;
  GL_SAMPLER_CUBE = $8B60;
  GL_SAMPLER_1D_SHADOW = $8B61;
  GL_SAMPLER_2D_SHADOW = $8B62;
  GL_DELETE_STATUS = $8B80;
  GL_COMPILE_STATUS = $8B81;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  GL_CURRENT_PROGRAM = $8B8D;
  GL_POINT_SPRITE_COORD_ORIGIN = $8CA0;
  GL_LOWER_LEFT = $8CA1;
  GL_UPPER_LEFT = $8CA2;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;
var
  glBlendEquationSeparate: procedure(modeRGB: GLenum; modeAlpha: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawBuffers: procedure(n: GLsizei; const bufs: PGLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glStencilOpSeparate: procedure(face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glStencilFuncSeparate: procedure(frontfunc: GLenum; backfunc: GLenum; ref: GLint; mask: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glStencilMaskSeparate: procedure(face: GLenum; mask: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glAttachShader: procedure(_program: GLuint; shader: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindAttribLocation: procedure(_program: GLuint; index: GLuint; const name: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCompileShader: procedure(shader: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCreateProgram: function(): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glCreateShader: function(_type: GLenum): GLuint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteProgram: procedure(_program: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDeleteShader: procedure(shader: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDetachShader: procedure(_program: GLuint; shader: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDisableVertexAttribArray: procedure(index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEnableVertexAttribArray: procedure(index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveAttrib: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetActiveUniform: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetAttachedShaders: procedure(_program: GLuint; maxCount: GLsizei; count: PGLsizei; obj: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetAttribLocation: function(_program: GLuint; const name: PGLchar): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramiv: procedure(_program: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetProgramInfoLog: procedure(_program: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetShaderiv: procedure(shader: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetShaderInfoLog: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetShaderSource: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; source: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformLocation: function(_program: GLuint; const name: PGLchar): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformfv: procedure(_program: GLuint; location: GLint; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformiv: procedure(_program: GLuint; location: GLint; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribdv: procedure(index: GLuint; pname: GLenum; params: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribfv: procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribiv: procedure(index: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribPointerv: procedure(index: GLuint; pname: GLenum; pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsProgram: function(_program: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsShader: function(shader: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glLinkProgram: procedure(_program: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glShaderSource: procedure(shader: GLuint; count: GLsizei; const _string: PGLchar; const length: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUseProgram: procedure(_program: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1f: procedure(location: GLint; v0: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1i: procedure(location: GLint; v0: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1iv: procedure(location: GLint; count: GLsizei; const value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2iv: procedure(location: GLint; count: GLsizei; const value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3iv: procedure(location: GLint; count: GLsizei; const value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4iv: procedure(location: GLint; count: GLsizei; const value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glValidateProgram: procedure(_program: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1d: procedure(index: GLuint; x: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1dv: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1f: procedure(index: GLuint; x: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1fv: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1s: procedure(index: GLuint; x: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib1sv: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2d: procedure(index: GLuint; x: GLdouble; y: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2dv: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2f: procedure(index: GLuint; x: GLfloat; y: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2fv: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2s: procedure(index: GLuint; x: GLshort; y: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib2sv: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3dv: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3fv: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib3sv: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4Nbv: procedure(index: GLuint; const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4Niv: procedure(index: GLuint; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4Nsv: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4Nub: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4Nubv: procedure(index: GLuint; const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4Nuiv: procedure(index: GLuint; const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4Nusv: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4bv: procedure(index: GLuint; const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4dv: procedure(index: GLuint; const v: PGLdouble); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4fv: procedure(index: GLuint; const v: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4iv: procedure(index: GLuint; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4sv: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4ubv: procedure(index: GLuint; const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4uiv: procedure(index: GLuint; const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttrib4usv: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribPointer: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_version_2_0: Boolean;

//**** GL_VERSION_2_1 *****//
const
  GL_PIXEL_PACK_BUFFER = $88EB;
  GL_PIXEL_UNPACK_BUFFER = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;
  GL_FLOAT_MAT2x3 = $8B65;
  GL_FLOAT_MAT2x4 = $8B66;
  GL_FLOAT_MAT3x2 = $8B67;
  GL_FLOAT_MAT3x4 = $8B68;
  GL_FLOAT_MAT4x2 = $8B69;
  GL_FLOAT_MAT4x3 = $8B6A;
  GL_SRGB = $8C40;
  GL_SRGB8 = $8C41;
  GL_SRGB_ALPHA = $8C42;
  GL_SRGB8_ALPHA8 = $8C43;
  GL_COMPRESSED_SRGB = $8C48;
  GL_COMPRESSED_SRGB_ALPHA = $8C49;
var
  glUniformMatrix2x3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix3x2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix2x4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix4x2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix3x4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniformMatrix4x3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_VERSION_2_1(): Boolean;

//**** GL_VERSION_2_1 DEPRECATED *****//
const
  GL_CURRENT_RASTER_SECONDARY_COLOR = $845F;
  GL_SLUMINANCE_ALPHA = $8C44;
  GL_SLUMINANCE8_ALPHA8 = $8C45;
  GL_SLUMINANCE = $8C46;
  GL_SLUMINANCE8 = $8C47;
  GL_COMPRESSED_SLUMINANCE = $8C4A;
  GL_COMPRESSED_SLUMINANCE_ALPHA = $8C4B;

//**** GL_VERSION_3_0 *****//

const
  GL_COMPARE_REF_TO_TEXTURE = $884E;
  GL_CLIP_DISTANCE0 = $3000;
  GL_CLIP_DISTANCE1 = $3001;
  GL_CLIP_DISTANCE2 = $3002;
  GL_CLIP_DISTANCE3 = $3003;
  GL_CLIP_DISTANCE4 = $3004;
  GL_CLIP_DISTANCE5 = $3005;
  GL_CLIP_DISTANCE6 = $3006;
  GL_CLIP_DISTANCE7 = $3007;
  GL_MAX_CLIP_DISTANCES = $0D32;
  GL_MAJOR_VERSION = $821B;
  GL_MINOR_VERSION = $821C;
  GL_NUM_EXTENSIONS = $821D;
  GL_CONTEXT_FLAGS = $821E;
  GL_DEPTH_BUFFER = $8223;
  GL_STENCIL_BUFFER = $8224;
  GL_COMPRESSED_RED = $8225;
  GL_COMPRESSED_RG = $8226;
  GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT = $0001;
  GL_RGBA32F = $8814;
  GL_RGB32F = $8815;
  GL_RGBA16F = $881A;
  GL_RGB16F = $881B;
  GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
  GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
  GL_MIN_PROGRAM_TEXEL_OFFSET = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET = $8905;
  GL_CLAMP_READ_COLOR = $891C;
  GL_FIXED_ONLY = $891D;
  GL_MAX_VARYING_COMPONENTS = $8B4B;
  GL_TEXTURE_1D_ARRAY = $8C18;
  GL_PROXY_TEXTURE_1D_ARRAY = $8C19;
  GL_TEXTURE_2D_ARRAY = $8C1A;
  GL_PROXY_TEXTURE_2D_ARRAY = $8C1B;
  GL_TEXTURE_BINDING_1D_ARRAY = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
  GL_R11F_G11F_B10F = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
  GL_RGB9_E5 = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
  GL_TEXTURE_SHARED_SIZE = $8C3F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
  GL_PRIMITIVES_GENERATED = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
  GL_RASTERIZER_DISCARD = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
  GL_INTERLEAVED_ATTRIBS = $8C8C;
  GL_SEPARATE_ATTRIBS = $8C8D;
  GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
  GL_RGBA32UI = $8D70;
  GL_RGB32UI = $8D71;
  GL_RGBA16UI = $8D76;
  GL_RGB16UI = $8D77;
  GL_RGBA8UI = $8D7C;
  GL_RGB8UI = $8D7D;
  GL_RGBA32I = $8D82;
  GL_RGB32I = $8D83;
  GL_RGBA16I = $8D88;
  GL_RGB16I = $8D89;
  GL_RGBA8I = $8D8E;
  GL_RGB8I = $8D8F;
  GL_RED_INTEGER = $8D94;
  GL_GREEN_INTEGER = $8D95;
  GL_BLUE_INTEGER = $8D96;
  GL_RGB_INTEGER = $8D98;
  GL_RGBA_INTEGER = $8D99;
  GL_BGR_INTEGER = $8D9A;
  GL_BGRA_INTEGER = $8D9B;
  GL_SAMPLER_1D_ARRAY = $8DC0;
  GL_SAMPLER_2D_ARRAY = $8DC1;
  GL_SAMPLER_1D_ARRAY_SHADOW = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
  GL_SAMPLER_CUBE_SHADOW = $8DC5;
  GL_UNSIGNED_INT_VEC2 = $8DC6;
  GL_UNSIGNED_INT_VEC3 = $8DC7;
  GL_UNSIGNED_INT_VEC4 = $8DC8;
  GL_INT_SAMPLER_1D = $8DC9;
  GL_INT_SAMPLER_2D = $8DCA;
  GL_INT_SAMPLER_3D = $8DCB;
  GL_INT_SAMPLER_CUBE = $8DCC;
  GL_INT_SAMPLER_1D_ARRAY = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY = $8DCF;
  GL_UNSIGNED_INT_SAMPLER_1D = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
  GL_QUERY_WAIT = $8E13;
  GL_QUERY_NO_WAIT = $8E14;
  GL_QUERY_BY_REGION_WAIT = $8E15;
  GL_QUERY_BY_REGION_NO_WAIT = $8E16;
  GL_BUFFER_ACCESS_FLAGS = $911F;
  GL_BUFFER_MAP_LENGTH = $9120;
  GL_BUFFER_MAP_OFFSET = $9121;

var
  glColorMaski: procedure(index: GLuint; r: GLboolean; g: GLboolean; b: GLboolean; a: GLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetBooleani_v: procedure(target: GLenum; index: GLuint; data: PGLboolean); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetIntegeri_v: procedure(target: GLenum; index: GLuint; data: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif} (* Also used in GL_ARB_uniform_buffer_object *)
  glEnablei: procedure(target: GLenum; index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDisablei: procedure(target: GLenum; index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glIsEnabledi: function(target: GLenum; index: GLuint): GLboolean; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBeginTransformFeedback: procedure(primitiveMode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndTransformFeedback: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindBufferRange: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); {$ifdef fpc}extdecl;{$else}stdcall;{$endif} (* Also used in GL_ARB_uniform_buffer_object *)
  glBindBufferBase: procedure(target: GLenum; index: GLuint; buffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif} (* Also used in GL_ARB_uniform_buffer_object *)
  glTransformFeedbackVaryings: procedure(_program: GLuint; count: GLsizei; const varyings: PPGLchar; bufferMode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTransformFeedbackVarying: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLsizei; _type: PGLenum; name: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClampColor: procedure(target: GLenum; clamp: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBeginConditionalRender: procedure(id: GLuint; mode: GLenum); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glEndConditionalRender: procedure(); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribIPointer: procedure(index: GLuint; size: GLint; _type: GLenum; stride: GLsizei; const _pointer: PGLvoid); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribIiv: procedure(index: GLuint; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetVertexAttribIuiv: procedure(index: GLuint; pname: GLenum; params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI1i: procedure(index: GLuint; x: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI2i: procedure(index: GLuint; x: GLint; y: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI3i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI4i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI1ui: procedure(index: GLuint; x: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI2ui: procedure(index: GLuint; x: GLuint; y: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI3ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI4ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI1iv: procedure(index: GLuint; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI2iv: procedure(index: GLuint; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI3iv: procedure(index: GLuint; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI4iv: procedure(index: GLuint; const v: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI1uiv: procedure(index: GLuint; const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI2uiv: procedure(index: GLuint; const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI3uiv: procedure(index: GLuint; const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI4uiv: procedure(index: GLuint; const v: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI4bv: procedure(index: GLuint; const v: PGLbyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI4sv: procedure(index: GLuint; const v: PGLshort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI4ubv: procedure(index: GLuint; const v: PGLubyte); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glVertexAttribI4usv: procedure(index: GLuint; const v: PGLushort); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetUniformuiv: procedure(_program: GLuint; location: GLint; params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glBindFragDataLocation: procedure(_program: GLuint; color: GLuint; const name: PGLchar); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetFragDataLocation: function(_program: GLuint; const name: PGLchar): GLint; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1ui: procedure(location: GLint; v0: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2ui: procedure(location: GLint; v0: GLuint; v1: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform1uiv: procedure(location: GLint; count: GLsizei; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform2uiv: procedure(location: GLint; count: GLsizei; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform3uiv: procedure(location: GLint; count: GLsizei; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glUniform4uiv: procedure(location: GLint; count: GLsizei; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexParameterIiv: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexParameterIuiv: procedure(target: GLenum; pname: GLenum; const params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexParameterIiv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetTexParameterIuiv: procedure(target: GLenum; pname: GLenum; params: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearBufferiv: procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearBufferuiv: procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearBufferfv: procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLfloat); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glClearBufferfi: procedure(buffer: GLenum; drawbuffer: GLint; depth: GLfloat; stencil: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetStringi: function(name: GLenum; index: GLuint): PGLubyte; {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_VERSION_3_0(): Boolean;

//**** GL_VERSION_3_0 DEPRECATED *****//
const
  GL_CLAMP_VERTEX_COLOR = $891A;
  GL_CLAMP_FRAGMENT_COLOR = $891B;
  GL_ALPHA_INTEGER = $8D97;

//**** GL_VERSION_3_1 *****//
const
  GL_SAMPLER_2D_RECT = $8B63;
  GL_SAMPLER_2D_RECT_SHADOW = $8B64;
  GL_SAMPLER_BUFFER = $8DC2;
  GL_INT_SAMPLER_2D_RECT = $8DCD;
  GL_INT_SAMPLER_BUFFER = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_2D_RECT = $8DD5;
  GL_UNSIGNED_INT_SAMPLER_BUFFER = $8DD8;
  GL_TEXTURE_BUFFER = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE = $8C2B;
  GL_TEXTURE_BINDING_BUFFER = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT = $8C2E;
  GL_TEXTURE_RECTANGLE = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE = $84F8;
  GL_RED_SNORM = $8F90;
  GL_RG_SNORM = $8F91;
  GL_RGB_SNORM = $8F92;
  GL_RGBA_SNORM = $8F93;
  GL_R8_SNORM = $8F94;
  GL_RG8_SNORM = $8F95;
  GL_RGB8_SNORM = $8F96;
  GL_RGBA8_SNORM = $8F97;
  GL_R16_SNORM = $8F98;
  GL_RG16_SNORM = $8F99;
  GL_RGB16_SNORM = $8F9A;
  GL_RGBA16_SNORM = $8F9B;
  GL_SIGNED_NORMALIZED = $8F9C;
  GL_PRIMITIVE_RESTART = $8F9D;
  GL_PRIMITIVE_RESTART_INDEX = $8F9E;

var
  glDrawArraysInstanced: procedure(mode: GLenum; first: GLint; count: GLsizei; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glDrawElementsInstanced: procedure(mode: GLenum; count: GLsizei; _type: GLenum; const indices: PGLvoid; primcount: GLsizei); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glTexBuffer: procedure(target: GLenum; internalformat: GLenum; buffer: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glPrimitiveRestartIndex: procedure(index: GLuint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_VERSION_3_1(): Boolean;

//**** GL_VERSION_3_2 *****//
const
  GL_CONTEXT_CORE_PROFILE_BIT = $00000001;
  GL_CONTEXT_COMPATIBILITY_PROFILE_BIT = $00000002;
  GL_LINES_ADJACENCY = $000A;
  GL_LINE_STRIP_ADJACENCY = $000B;
  GL_TRIANGLES_ADJACENCY = $000C;
  GL_TRIANGLE_STRIP_ADJACENCY = $000D;
  GL_PROGRAM_POINT_SIZE = $8642;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS = $8C29;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED = $8DA7;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS = $8DA8;
  GL_GEOMETRY_SHADER = $8DD9;
  GL_GEOMETRY_VERTICES_OUT = $8916;
  GL_GEOMETRY_INPUT_TYPE = $8917;
  GL_GEOMETRY_OUTPUT_TYPE = $8918;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS = $8DE1;
  GL_MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
  GL_MAX_GEOMETRY_INPUT_COMPONENTS = $9123;
  GL_MAX_GEOMETRY_OUTPUT_COMPONENTS = $9124;
  GL_MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
  GL_CONTEXT_PROFILE_MASK = $9126;

var
  glGetInteger64i_v: procedure(target: GLenum; index: GLuint; data: PGLint64); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glGetBufferParameteri64v: procedure(target: GLenum; pname: GLenum; params: PGLint64); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glProgramParameteri: procedure(_program: GLuint; pname: GLenum; value: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}
  glFramebufferTexture: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint); {$ifdef fpc}extdecl;{$else}stdcall;{$endif}

function Load_GL_VERSION_3_2(): Boolean;

//**** GL_VERSION_3_3 *****//

function Load_GL_VERSION_3_3(): Boolean;

//**** GL_VERSION_4_0 *****//

function Load_GL_VERSION_4_0(): Boolean;

{$endif}

implementation

{$ifndef gles20}

{$ifdef fpc}
{$if defined(cpui386) or defined(cpux86_64)}
uses
  math;
{$ifend}
{$else}
uses
  math;
{$endif}

{$ifdef windows}
function WinChoosePixelFormat(DC: HDC; p2: PPixelFormatDescriptor): Integer; {$ifdef fpc}extdecl;{$else}stdcall;{$endif} external 'gdi32' name 'ChoosePixelFormat';
{$endif}

{$IFDEF MORPHOS}

{ MorphOS GL works differently due to different dynamic-library handling on Amiga-like }
{ systems, so its functions are included here. }
{$INCLUDE tinygl.inc}

{$ENDIF MORPHOS}

procedure FreeOpenGL;
begin
{$IFDEF MORPHOS}

  // MorphOS's GL will closed down by TinyGL unit, nothing is needed here.

{$ELSE MORPHOS}
  @glAccum := nil;
  @glAlphaFunc := nil;
  @glAreTexturesResident := nil;
  @glArrayElement := nil;
  @glBegin := nil;
  @glBindTexture := nil;
  @glBitmap := nil;
  @glBlendFunc := nil;
  @glCallList := nil;
  @glCallLists := nil;
  @glClear := nil;
  @glClearAccum := nil;
  @glClearColor := nil;
  @glClearDepth := nil;
  @glClearIndex := nil;
  @glClearStencil := nil;
  @glClipPlane := nil;
  @glColor3b := nil;
  @glColor3bv := nil;
  @glColor3d := nil;
  @glColor3dv := nil;
  @glColor3f := nil;
  @glColor3fv := nil;
  @glColor3i := nil;
  @glColor3iv := nil;
  @glColor3s := nil;
  @glColor3sv := nil;
  @glColor3ub := nil;
  @glColor3ubv := nil;
  @glColor3ui := nil;
  @glColor3uiv := nil;
  @glColor3us := nil;
  @glColor3usv := nil;
  @glColor4b := nil;
  @glColor4bv := nil;
  @glColor4d := nil;
  @glColor4dv := nil;
  @glColor4f := nil;
  @glColor4fv := nil;
  @glColor4i := nil;
  @glColor4iv := nil;
  @glColor4s := nil;
  @glColor4sv := nil;
  @glColor4ub := nil;
  @glColor4ubv := nil;
  @glColor4ui := nil;
  @glColor4uiv := nil;
  @glColor4us := nil;
  @glColor4usv := nil;
  @glColorMask := nil;
  @glColorMaterial := nil;
  @glColorPointer := nil;
  @glCopyPixels := nil;
  @glCopyTexImage1D := nil;
  @glCopyTexImage2D := nil;
  @glCopyTexSubImage1D := nil;
  @glCopyTexSubImage2D := nil;
  @glCullFace := nil;
  @glDeleteLists := nil;
  @glDeleteTextures := nil;
  @glDepthFunc := nil;
  @glDepthMask := nil;
  @glDepthRange := nil;
  @glDisable := nil;
  @glDisableClientState := nil;
  @glDrawArrays := nil;
  @glDrawBuffer := nil;
  @glDrawElements := nil;
  @glDrawPixels := nil;
  @glEdgeFlag := nil;
  @glEdgeFlagPointer := nil;
  @glEdgeFlagv := nil;
  @glEnable := nil;
  @glEnableClientState := nil;
  @glEnd := nil;
  @glEndList := nil;
  @glEvalCoord1d := nil;
  @glEvalCoord1dv := nil;
  @glEvalCoord1f := nil;
  @glEvalCoord1fv := nil;
  @glEvalCoord2d := nil;
  @glEvalCoord2dv := nil;
  @glEvalCoord2f := nil;
  @glEvalCoord2fv := nil;
  @glEvalMesh1 := nil;
  @glEvalMesh2 := nil;
  @glEvalPoint1 := nil;
  @glEvalPoint2 := nil;
  @glFeedbackBuffer := nil;
  @glFinish := nil;
  @glFlush := nil;
  @glFogf := nil;
  @glFogfv := nil;
  @glFogi := nil;
  @glFogiv := nil;
  @glFrontFace := nil;
  @glFrustum := nil;
  @glGenLists := nil;
  @glGenTextures := nil;
  @glGetBooleanv := nil;
  @glGetClipPlane := nil;
  @glGetDoublev := nil;
  @glGetError := nil;
  @glGetFloatv := nil;
  @glGetIntegerv := nil;
  @glGetLightfv := nil;
  @glGetLightiv := nil;
  @glGetMapdv := nil;
  @glGetMapfv := nil;
  @glGetMapiv := nil;
  @glGetMaterialfv := nil;
  @glGetMaterialiv := nil;
  @glGetPixelMapfv := nil;
  @glGetPixelMapuiv := nil;
  @glGetPixelMapusv := nil;
  @glGetPointerv := nil;
  @glGetPolygonStipple := nil;
  @glGetString := nil;
  @glGetTexEnvfv := nil;
  @glGetTexEnviv := nil;
  @glGetTexGendv := nil;
  @glGetTexGenfv := nil;
  @glGetTexGeniv := nil;
  @glGetTexImage := nil;
  @glGetTexLevelParameterfv := nil;
  @glGetTexLevelParameteriv := nil;
  @glGetTexParameterfv := nil;
  @glGetTexParameteriv := nil;
  @glHint := nil;
  @glIndexMask := nil;
  @glIndexPointer := nil;
  @glIndexd := nil;
  @glIndexdv := nil;
  @glIndexf := nil;
  @glIndexfv := nil;
  @glIndexi := nil;
  @glIndexiv := nil;
  @glIndexs := nil;
  @glIndexsv := nil;
  @glIndexub := nil;
  @glIndexubv := nil;
  @glInitNames := nil;
  @glInterleavedArrays := nil;
  @glIsEnabled := nil;
  @glIsList := nil;
  @glIsTexture := nil;
  @glLightModelf := nil;
  @glLightModelfv := nil;
  @glLightModeli := nil;
  @glLightModeliv := nil;
  @glLightf := nil;
  @glLightfv := nil;
  @glLighti := nil;
  @glLightiv := nil;
  @glLineStipple := nil;
  @glLineWidth := nil;
  @glListBase := nil;
  @glLoadIdentity := nil;
  @glLoadMatrixd := nil;
  @glLoadMatrixf := nil;
  @glLoadName := nil;
  @glLogicOp := nil;
  @glMap1d := nil;
  @glMap1f := nil;
  @glMap2d := nil;
  @glMap2f := nil;
  @glMapGrid1d := nil;
  @glMapGrid1f := nil;
  @glMapGrid2d := nil;
  @glMapGrid2f := nil;
  @glMaterialf := nil;
  @glMaterialfv := nil;
  @glMateriali := nil;
  @glMaterialiv := nil;
  @glMatrixMode := nil;
  @glMultMatrixd := nil;
  @glMultMatrixf := nil;
  @glNewList := nil;
  @glNormal3b := nil;
  @glNormal3bv := nil;
  @glNormal3d := nil;
  @glNormal3dv := nil;
  @glNormal3f := nil;
  @glNormal3fv := nil;
  @glNormal3i := nil;
  @glNormal3iv := nil;
  @glNormal3s := nil;
  @glNormal3sv := nil;
  @glNormalPointer := nil;
  @glOrtho := nil;
  @glPassThrough := nil;
  @glPixelMapfv := nil;
  @glPixelMapuiv := nil;
  @glPixelMapusv := nil;
  @glPixelStoref := nil;
  @glPixelStorei := nil;
  @glPixelTransferf := nil;
  @glPixelTransferi := nil;
  @glPixelZoom := nil;
  @glPointSize := nil;
  @glPolygonMode := nil;
  @glPolygonOffset := nil;
  @glPolygonStipple := nil;
  @glPopAttrib := nil;
  @glPopClientAttrib := nil;
  @glPopMatrix := nil;
  @glPopName := nil;
  @glPrioritizeTextures := nil;
  @glPushAttrib := nil;
  @glPushClientAttrib := nil;
  @glPushMatrix := nil;
  @glPushName := nil;
  @glRasterPos2d := nil;
  @glRasterPos2dv := nil;
  @glRasterPos2f := nil;
  @glRasterPos2fv := nil;
  @glRasterPos2i := nil;
  @glRasterPos2iv := nil;
  @glRasterPos2s := nil;
  @glRasterPos2sv := nil;
  @glRasterPos3d := nil;
  @glRasterPos3dv := nil;
  @glRasterPos3f := nil;
  @glRasterPos3fv := nil;
  @glRasterPos3i := nil;
  @glRasterPos3iv := nil;
  @glRasterPos3s := nil;
  @glRasterPos3sv := nil;
  @glRasterPos4d := nil;
  @glRasterPos4dv := nil;
  @glRasterPos4f := nil;
  @glRasterPos4fv := nil;
  @glRasterPos4i := nil;
  @glRasterPos4iv := nil;
  @glRasterPos4s := nil;
  @glRasterPos4sv := nil;
  @glReadBuffer := nil;
  @glReadPixels := nil;
  @glRectd := nil;
  @glRectdv := nil;
  @glRectf := nil;
  @glRectfv := nil;
  @glRecti := nil;
  @glRectiv := nil;
  @glRects := nil;
  @glRectsv := nil;
  @glRenderMode := nil;
  @glRotated := nil;
  @glRotatef := nil;
  @glScaled := nil;
  @glScalef := nil;
  @glScissor := nil;
  @glSelectBuffer := nil;
  @glShadeModel := nil;
  @glStencilFunc := nil;
  @glStencilMask := nil;
  @glStencilOp := nil;
  @glTexCoord1d := nil;
  @glTexCoord1dv := nil;
  @glTexCoord1f := nil;
  @glTexCoord1fv := nil;
  @glTexCoord1i := nil;
  @glTexCoord1iv := nil;
  @glTexCoord1s := nil;
  @glTexCoord1sv := nil;
  @glTexCoord2d := nil;
  @glTexCoord2dv := nil;
  @glTexCoord2f := nil;
  @glTexCoord2fv := nil;
  @glTexCoord2i := nil;
  @glTexCoord2iv := nil;
  @glTexCoord2s := nil;
  @glTexCoord2sv := nil;
  @glTexCoord3d := nil;
  @glTexCoord3dv := nil;
  @glTexCoord3f := nil;
  @glTexCoord3fv := nil;
  @glTexCoord3i := nil;
  @glTexCoord3iv := nil;
  @glTexCoord3s := nil;
  @glTexCoord3sv := nil;
  @glTexCoord4d := nil;
  @glTexCoord4dv := nil;
  @glTexCoord4f := nil;
  @glTexCoord4fv := nil;
  @glTexCoord4i := nil;
  @glTexCoord4iv := nil;
  @glTexCoord4s := nil;
  @glTexCoord4sv := nil;
  @glTexCoordPointer := nil;
  @glTexEnvf := nil;
  @glTexEnvfv := nil;
  @glTexEnvi := nil;
  @glTexEnviv := nil;
  @glTexGend := nil;
  @glTexGendv := nil;
  @glTexGenf := nil;
  @glTexGenfv := nil;
  @glTexGeni := nil;
  @glTexGeniv := nil;
  @glTexImage1D := nil;
  @glTexImage2D := nil;
  @glTexParameterf := nil;
  @glTexParameterfv := nil;
  @glTexParameteri := nil;
  @glTexParameteriv := nil;
  @glTexSubImage1D := nil;
  @glTexSubImage2D := nil;
  @glTranslated := nil;
  @glTranslatef := nil;
  @glVertex2d := nil;
  @glVertex2dv := nil;
  @glVertex2f := nil;
  @glVertex2fv := nil;
  @glVertex2i := nil;
  @glVertex2iv := nil;
  @glVertex2s := nil;
  @glVertex2sv := nil;
  @glVertex3d := nil;
  @glVertex3dv := nil;
  @glVertex3f := nil;
  @glVertex3fv := nil;
  @glVertex3i := nil;
  @glVertex3iv := nil;
  @glVertex3s := nil;
  @glVertex3sv := nil;
  @glVertex4d := nil;
  @glVertex4dv := nil;
  @glVertex4f := nil;
  @glVertex4fv := nil;
  @glVertex4i := nil;
  @glVertex4iv := nil;
  @glVertex4s := nil;
  @glVertex4sv := nil;
  @glVertexPointer := nil;
  @glViewport := nil;
  @glGetProgramBinary := nil;
  @glProgramBinary := nil;
  @glProgramParameteri := nil;
  {$IFDEF Windows}
  @ChoosePixelFormat := nil;
  {$ENDIF}

  if (LibGL <> 0) then
    FreeLibrary(LibGL);
{$ENDIF MORPHOS}
end;

procedure LoadOpenGL(const dll: String);
{$IFDEF MORPHOS}
begin
  // MorphOS's GL has own initialization in TinyGL unit, nothing is needed here.
end;
{$ELSE MORPHOS}
var
  MethodName: string;

  function GetGLProcAddress(Lib: GLPtrInt; ProcName: pchar): Pointer;
  begin
    MethodName:=ProcName;
    Result:=GetProcAddress(Lib, ProcName);
  end;

begin
  MethodName:='';

  @glGetProgramBinary := nil;
  @glProgramBinary := nil;
  @glProgramParameteri := nil;

  FreeOpenGL;

  LibGL := LoadLibrary(pchar(string(dll)));
  if LibGL = 0 then raise Exception.Create('Could not load OpenGL from ' + dll);
  try
    @glAccum := GetGLProcAddress(LibGL, 'glAccum');
    @glAlphaFunc := GetGLProcAddress(LibGL, 'glAlphaFunc');
    @glAreTexturesResident := GetGLProcAddress(LibGL, 'glAreTexturesResident');
    @glArrayElement := GetGLProcAddress(LibGL, 'glArrayElement');
    @glBegin := GetGLProcAddress(LibGL, 'glBegin');
    @glBindTexture := GetGLProcAddress(LibGL, 'glBindTexture');
    @glBitmap := GetGLProcAddress(LibGL, 'glBitmap');
    @glBlendFunc := GetGLProcAddress(LibGL, 'glBlendFunc');
    @glCallList := GetGLProcAddress(LibGL, 'glCallList');
    @glCallLists := GetGLProcAddress(LibGL, 'glCallLists');
    @glClear := GetGLProcAddress(LibGL, 'glClear');
    @glClearAccum := GetGLProcAddress(LibGL, 'glClearAccum');
    @glClearColor := GetGLProcAddress(LibGL, 'glClearColor');
    @glClearDepth := GetGLProcAddress(LibGL, 'glClearDepth');
    @glClearIndex := GetGLProcAddress(LibGL, 'glClearIndex');
    @glClearStencil := GetGLProcAddress(LibGL, 'glClearStencil');
    @glClipPlane := GetGLProcAddress(LibGL, 'glClipPlane');
    @glColor3b := GetGLProcAddress(LibGL, 'glColor3b');
    @glColor3bv := GetGLProcAddress(LibGL, 'glColor3bv');
    @glColor3d := GetGLProcAddress(LibGL, 'glColor3d');
    @glColor3dv := GetGLProcAddress(LibGL, 'glColor3dv');
    @glColor3f := GetGLProcAddress(LibGL, 'glColor3f');
    @glColor3fv := GetGLProcAddress(LibGL, 'glColor3fv');
    @glColor3i := GetGLProcAddress(LibGL, 'glColor3i');
    @glColor3iv := GetGLProcAddress(LibGL, 'glColor3iv');
    @glColor3s := GetGLProcAddress(LibGL, 'glColor3s');
    @glColor3sv := GetGLProcAddress(LibGL, 'glColor3sv');
    @glColor3ub := GetGLProcAddress(LibGL, 'glColor3ub');
    @glColor3ubv := GetGLProcAddress(LibGL, 'glColor3ubv');
    @glColor3ui := GetGLProcAddress(LibGL, 'glColor3ui');
    @glColor3uiv := GetGLProcAddress(LibGL, 'glColor3uiv');
    @glColor3us := GetGLProcAddress(LibGL, 'glColor3us');
    @glColor3usv := GetGLProcAddress(LibGL, 'glColor3usv');
    @glColor4b := GetGLProcAddress(LibGL, 'glColor4b');
    @glColor4bv := GetGLProcAddress(LibGL, 'glColor4bv');
    @glColor4d := GetGLProcAddress(LibGL, 'glColor4d');
    @glColor4dv := GetGLProcAddress(LibGL, 'glColor4dv');
    @glColor4f := GetGLProcAddress(LibGL, 'glColor4f');
    @glColor4fv := GetGLProcAddress(LibGL, 'glColor4fv');
    @glColor4i := GetGLProcAddress(LibGL, 'glColor4i');
    @glColor4iv := GetGLProcAddress(LibGL, 'glColor4iv');
    @glColor4s := GetGLProcAddress(LibGL, 'glColor4s');
    @glColor4sv := GetGLProcAddress(LibGL, 'glColor4sv');
    @glColor4ub := GetGLProcAddress(LibGL, 'glColor4ub');
    @glColor4ubv := GetGLProcAddress(LibGL, 'glColor4ubv');
    @glColor4ui := GetGLProcAddress(LibGL, 'glColor4ui');
    @glColor4uiv := GetGLProcAddress(LibGL, 'glColor4uiv');
    @glColor4us := GetGLProcAddress(LibGL, 'glColor4us');
    @glColor4usv := GetGLProcAddress(LibGL, 'glColor4usv');
    @glColorMask := GetGLProcAddress(LibGL, 'glColorMask');
    @glColorMaterial := GetGLProcAddress(LibGL, 'glColorMaterial');
    @glColorPointer := GetGLProcAddress(LibGL, 'glColorPointer');
    @glCopyPixels := GetGLProcAddress(LibGL, 'glCopyPixels');
    @glCopyTexImage1D := GetGLProcAddress(LibGL, 'glCopyTexImage1D');
    @glCopyTexImage2D := GetGLProcAddress(LibGL, 'glCopyTexImage2D');
    @glCopyTexSubImage1D := GetGLProcAddress(LibGL, 'glCopyTexSubImage1D');
    @glCopyTexSubImage2D := GetGLProcAddress(LibGL, 'glCopyTexSubImage2D');
    @glCullFace := GetGLProcAddress(LibGL, 'glCullFace');
    @glDeleteLists := GetGLProcAddress(LibGL, 'glDeleteLists');
    @glDeleteTextures := GetGLProcAddress(LibGL, 'glDeleteTextures');
    @glDepthFunc := GetGLProcAddress(LibGL, 'glDepthFunc');
    @glDepthMask := GetGLProcAddress(LibGL, 'glDepthMask');
    @glDepthRange := GetGLProcAddress(LibGL, 'glDepthRange');
    @glDisable := GetGLProcAddress(LibGL, 'glDisable');
    @glDisableClientState := GetGLProcAddress(LibGL, 'glDisableClientState');
    @glDrawArrays := GetGLProcAddress(LibGL, 'glDrawArrays');
    @glDrawBuffer := GetGLProcAddress(LibGL, 'glDrawBuffer');
    @glDrawElements := GetGLProcAddress(LibGL, 'glDrawElements');
    @glDrawPixels := GetGLProcAddress(LibGL, 'glDrawPixels');
    @glEdgeFlag := GetGLProcAddress(LibGL, 'glEdgeFlag');
    @glEdgeFlagPointer := GetGLProcAddress(LibGL, 'glEdgeFlagPointer');
    @glEdgeFlagv := GetGLProcAddress(LibGL, 'glEdgeFlagv');
    @glEnable := GetGLProcAddress(LibGL, 'glEnable');
    @glEnableClientState := GetGLProcAddress(LibGL, 'glEnableClientState');
    @glEnd := GetGLProcAddress(LibGL, 'glEnd');
    @glEndList := GetGLProcAddress(LibGL, 'glEndList');
    @glEvalCoord1d := GetGLProcAddress(LibGL, 'glEvalCoord1d');
    @glEvalCoord1dv := GetGLProcAddress(LibGL, 'glEvalCoord1dv');
    @glEvalCoord1f := GetGLProcAddress(LibGL, 'glEvalCoord1f');
    @glEvalCoord1fv := GetGLProcAddress(LibGL, 'glEvalCoord1fv');
    @glEvalCoord2d := GetGLProcAddress(LibGL, 'glEvalCoord2d');
    @glEvalCoord2dv := GetGLProcAddress(LibGL, 'glEvalCoord2dv');
    @glEvalCoord2f := GetGLProcAddress(LibGL, 'glEvalCoord2f');
    @glEvalCoord2fv := GetGLProcAddress(LibGL, 'glEvalCoord2fv');
    @glEvalMesh1 := GetGLProcAddress(LibGL, 'glEvalMesh1');
    @glEvalMesh2 := GetGLProcAddress(LibGL, 'glEvalMesh2');
    @glEvalPoint1 := GetGLProcAddress(LibGL, 'glEvalPoint1');
    @glEvalPoint2 := GetGLProcAddress(LibGL, 'glEvalPoint2');
    @glFeedbackBuffer := GetGLProcAddress(LibGL, 'glFeedbackBuffer');
    @glFinish := GetGLProcAddress(LibGL, 'glFinish');
    @glFlush := GetGLProcAddress(LibGL, 'glFlush');
    @glFogf := GetGLProcAddress(LibGL, 'glFogf');
    @glFogfv := GetGLProcAddress(LibGL, 'glFogfv');
    @glFogi := GetGLProcAddress(LibGL, 'glFogi');
    @glFogiv := GetGLProcAddress(LibGL, 'glFogiv');
    @glFrontFace := GetGLProcAddress(LibGL, 'glFrontFace');
    @glFrustum := GetGLProcAddress(LibGL, 'glFrustum');
    @glGenLists := GetGLProcAddress(LibGL, 'glGenLists');
    @glGenTextures := GetGLProcAddress(LibGL, 'glGenTextures');
    @glGetBooleanv := GetGLProcAddress(LibGL, 'glGetBooleanv');
    @glGetClipPlane := GetGLProcAddress(LibGL, 'glGetClipPlane');
    @glGetDoublev := GetGLProcAddress(LibGL, 'glGetDoublev');
    @glGetError := GetGLProcAddress(LibGL, 'glGetError');
    @glGetFloatv := GetGLProcAddress(LibGL, 'glGetFloatv');
    @glGetIntegerv := GetGLProcAddress(LibGL, 'glGetIntegerv');
    @glGetLightfv := GetGLProcAddress(LibGL, 'glGetLightfv');
    @glGetLightiv := GetGLProcAddress(LibGL, 'glGetLightiv');
    @glGetMapdv := GetGLProcAddress(LibGL, 'glGetMapdv');
    @glGetMapfv := GetGLProcAddress(LibGL, 'glGetMapfv');
    @glGetMapiv := GetGLProcAddress(LibGL, 'glGetMapiv');
    @glGetMaterialfv := GetGLProcAddress(LibGL, 'glGetMaterialfv');
    @glGetMaterialiv := GetGLProcAddress(LibGL, 'glGetMaterialiv');
    @glGetPixelMapfv := GetGLProcAddress(LibGL, 'glGetPixelMapfv');
    @glGetPixelMapuiv := GetGLProcAddress(LibGL, 'glGetPixelMapuiv');
    @glGetPixelMapusv := GetGLProcAddress(LibGL, 'glGetPixelMapusv');
    @glGetPointerv := GetGLProcAddress(LibGL, 'glGetPointerv');
    @glGetPolygonStipple := GetGLProcAddress(LibGL, 'glGetPolygonStipple');
    @glGetString := GetGLProcAddress(LibGL, 'glGetString');
    @glGetTexEnvfv := GetGLProcAddress(LibGL, 'glGetTexEnvfv');
    @glGetTexEnviv := GetGLProcAddress(LibGL, 'glGetTexEnviv');
    @glGetTexGendv := GetGLProcAddress(LibGL, 'glGetTexGendv');
    @glGetTexGenfv := GetGLProcAddress(LibGL, 'glGetTexGenfv');
    @glGetTexGeniv := GetGLProcAddress(LibGL, 'glGetTexGeniv');
    @glGetTexImage := GetGLProcAddress(LibGL, 'glGetTexImage');
    @glGetTexLevelParameterfv := GetGLProcAddress(LibGL, 'glGetTexLevelParameterfv');
    @glGetTexLevelParameteriv := GetGLProcAddress(LibGL, 'glGetTexLevelParameteriv');
    @glGetTexParameterfv := GetGLProcAddress(LibGL, 'glGetTexParameterfv');
    @glGetTexParameteriv := GetGLProcAddress(LibGL, 'glGetTexParameteriv');
    @glHint := GetGLProcAddress(LibGL, 'glHint');
    @glIndexMask := GetGLProcAddress(LibGL, 'glIndexMask');
    @glIndexPointer := GetGLProcAddress(LibGL, 'glIndexPointer');
    @glIndexd := GetGLProcAddress(LibGL, 'glIndexd');
    @glIndexdv := GetGLProcAddress(LibGL, 'glIndexdv');
    @glIndexf := GetGLProcAddress(LibGL, 'glIndexf');
    @glIndexfv := GetGLProcAddress(LibGL, 'glIndexfv');
    @glIndexi := GetGLProcAddress(LibGL, 'glIndexi');
    @glIndexiv := GetGLProcAddress(LibGL, 'glIndexiv');
    @glIndexs := GetGLProcAddress(LibGL, 'glIndexs');
    @glIndexsv := GetGLProcAddress(LibGL, 'glIndexsv');
    @glIndexub := GetGLProcAddress(LibGL, 'glIndexub');
    @glIndexubv := GetGLProcAddress(LibGL, 'glIndexubv');
    @glInitNames := GetGLProcAddress(LibGL, 'glInitNames');
    @glInterleavedArrays := GetGLProcAddress(LibGL, 'glInterleavedArrays');
    @glIsEnabled := GetGLProcAddress(LibGL, 'glIsEnabled');
    @glIsList := GetGLProcAddress(LibGL, 'glIsList');
    @glIsTexture := GetGLProcAddress(LibGL, 'glIsTexture');
    @glLightModelf := GetGLProcAddress(LibGL, 'glLightModelf');
    @glLightModelfv := GetGLProcAddress(LibGL, 'glLightModelfv');
    @glLightModeli := GetGLProcAddress(LibGL, 'glLightModeli');
    @glLightModeliv := GetGLProcAddress(LibGL, 'glLightModeliv');
    @glLightf := GetGLProcAddress(LibGL, 'glLightf');
    @glLightfv := GetGLProcAddress(LibGL, 'glLightfv');
    @glLighti := GetGLProcAddress(LibGL, 'glLighti');
    @glLightiv := GetGLProcAddress(LibGL, 'glLightiv');
    @glLineStipple := GetGLProcAddress(LibGL, 'glLineStipple');
    @glLineWidth := GetGLProcAddress(LibGL, 'glLineWidth');
    @glListBase := GetGLProcAddress(LibGL, 'glListBase');
    @glLoadIdentity := GetGLProcAddress(LibGL, 'glLoadIdentity');
    @glLoadMatrixd := GetGLProcAddress(LibGL, 'glLoadMatrixd');
    @glLoadMatrixf := GetGLProcAddress(LibGL, 'glLoadMatrixf');
    @glLoadName := GetGLProcAddress(LibGL, 'glLoadName');
    @glLogicOp := GetGLProcAddress(LibGL, 'glLogicOp');
    @glMap1d := GetGLProcAddress(LibGL, 'glMap1d');
    @glMap1f := GetGLProcAddress(LibGL, 'glMap1f');
    @glMap2d := GetGLProcAddress(LibGL, 'glMap2d');
    @glMap2f := GetGLProcAddress(LibGL, 'glMap2f');
    @glMapGrid1d := GetGLProcAddress(LibGL, 'glMapGrid1d');
    @glMapGrid1f := GetGLProcAddress(LibGL, 'glMapGrid1f');
    @glMapGrid2d := GetGLProcAddress(LibGL, 'glMapGrid2d');
    @glMapGrid2f := GetGLProcAddress(LibGL, 'glMapGrid2f');
    @glMaterialf := GetGLProcAddress(LibGL, 'glMaterialf');
    @glMaterialfv := GetGLProcAddress(LibGL, 'glMaterialfv');
    @glMateriali := GetGLProcAddress(LibGL, 'glMateriali');
    @glMaterialiv := GetGLProcAddress(LibGL, 'glMaterialiv');
    @glMatrixMode := GetGLProcAddress(LibGL, 'glMatrixMode');
    @glMultMatrixd := GetGLProcAddress(LibGL, 'glMultMatrixd');
    @glMultMatrixf := GetGLProcAddress(LibGL, 'glMultMatrixf');
    @glNewList := GetGLProcAddress(LibGL, 'glNewList');
    @glNormal3b := GetGLProcAddress(LibGL, 'glNormal3b');
    @glNormal3bv := GetGLProcAddress(LibGL, 'glNormal3bv');
    @glNormal3d := GetGLProcAddress(LibGL, 'glNormal3d');
    @glNormal3dv := GetGLProcAddress(LibGL, 'glNormal3dv');
    @glNormal3f := GetGLProcAddress(LibGL, 'glNormal3f');
    @glNormal3fv := GetGLProcAddress(LibGL, 'glNormal3fv');
    @glNormal3i := GetGLProcAddress(LibGL, 'glNormal3i');
    @glNormal3iv := GetGLProcAddress(LibGL, 'glNormal3iv');
    @glNormal3s := GetGLProcAddress(LibGL, 'glNormal3s');
    @glNormal3sv := GetGLProcAddress(LibGL, 'glNormal3sv');
    @glNormalPointer := GetGLProcAddress(LibGL, 'glNormalPointer');
    @glOrtho := GetGLProcAddress(LibGL, 'glOrtho');
    @glPassThrough := GetGLProcAddress(LibGL, 'glPassThrough');
    @glPixelMapfv := GetGLProcAddress(LibGL, 'glPixelMapfv');
    @glPixelMapuiv := GetGLProcAddress(LibGL, 'glPixelMapuiv');
    @glPixelMapusv := GetGLProcAddress(LibGL, 'glPixelMapusv');
    @glPixelStoref := GetGLProcAddress(LibGL, 'glPixelStoref');
    @glPixelStorei := GetGLProcAddress(LibGL, 'glPixelStorei');
    @glPixelTransferf := GetGLProcAddress(LibGL, 'glPixelTransferf');
    @glPixelTransferi := GetGLProcAddress(LibGL, 'glPixelTransferi');
    @glPixelZoom := GetGLProcAddress(LibGL, 'glPixelZoom');
    @glPointSize := GetGLProcAddress(LibGL, 'glPointSize');
    @glPolygonMode := GetGLProcAddress(LibGL, 'glPolygonMode');
    @glPolygonOffset := GetGLProcAddress(LibGL, 'glPolygonOffset');
    @glPolygonStipple := GetGLProcAddress(LibGL, 'glPolygonStipple');
    @glPopAttrib := GetGLProcAddress(LibGL, 'glPopAttrib');
    @glPopClientAttrib := GetGLProcAddress(LibGL, 'glPopClientAttrib');
    @glPopMatrix := GetGLProcAddress(LibGL, 'glPopMatrix');
    @glPopName := GetGLProcAddress(LibGL, 'glPopName');
    @glPrioritizeTextures := GetGLProcAddress(LibGL, 'glPrioritizeTextures');
    @glPushAttrib := GetGLProcAddress(LibGL, 'glPushAttrib');
    @glPushClientAttrib := GetGLProcAddress(LibGL, 'glPushClientAttrib');
    @glPushMatrix := GetGLProcAddress(LibGL, 'glPushMatrix');
    @glPushName := GetGLProcAddress(LibGL, 'glPushName');
    @glRasterPos2d := GetGLProcAddress(LibGL, 'glRasterPos2d');
    @glRasterPos2dv := GetGLProcAddress(LibGL, 'glRasterPos2dv');
    @glRasterPos2f := GetGLProcAddress(LibGL, 'glRasterPos2f');
    @glRasterPos2fv := GetGLProcAddress(LibGL, 'glRasterPos2fv');
    @glRasterPos2i := GetGLProcAddress(LibGL, 'glRasterPos2i');
    @glRasterPos2iv := GetGLProcAddress(LibGL, 'glRasterPos2iv');
    @glRasterPos2s := GetGLProcAddress(LibGL, 'glRasterPos2s');
    @glRasterPos2sv := GetGLProcAddress(LibGL, 'glRasterPos2sv');
    @glRasterPos3d := GetGLProcAddress(LibGL, 'glRasterPos3d');
    @glRasterPos3dv := GetGLProcAddress(LibGL, 'glRasterPos3dv');
    @glRasterPos3f := GetGLProcAddress(LibGL, 'glRasterPos3f');
    @glRasterPos3fv := GetGLProcAddress(LibGL, 'glRasterPos3fv');
    @glRasterPos3i := GetGLProcAddress(LibGL, 'glRasterPos3i');
    @glRasterPos3iv := GetGLProcAddress(LibGL, 'glRasterPos3iv');
    @glRasterPos3s := GetGLProcAddress(LibGL, 'glRasterPos3s');
    @glRasterPos3sv := GetGLProcAddress(LibGL, 'glRasterPos3sv');
    @glRasterPos4d := GetGLProcAddress(LibGL, 'glRasterPos4d');
    @glRasterPos4dv := GetGLProcAddress(LibGL, 'glRasterPos4dv');
    @glRasterPos4f := GetGLProcAddress(LibGL, 'glRasterPos4f');
    @glRasterPos4fv := GetGLProcAddress(LibGL, 'glRasterPos4fv');
    @glRasterPos4i := GetGLProcAddress(LibGL, 'glRasterPos4i');
    @glRasterPos4iv := GetGLProcAddress(LibGL, 'glRasterPos4iv');
    @glRasterPos4s := GetGLProcAddress(LibGL, 'glRasterPos4s');
    @glRasterPos4sv := GetGLProcAddress(LibGL, 'glRasterPos4sv');
    @glReadBuffer := GetGLProcAddress(LibGL, 'glReadBuffer');
    @glReadPixels := GetGLProcAddress(LibGL, 'glReadPixels');
    @glRectd := GetGLProcAddress(LibGL, 'glRectd');
    @glRectdv := GetGLProcAddress(LibGL, 'glRectdv');
    @glRectf := GetGLProcAddress(LibGL, 'glRectf');
    @glRectfv := GetGLProcAddress(LibGL, 'glRectfv');
    @glRecti := GetGLProcAddress(LibGL, 'glRecti');
    @glRectiv := GetGLProcAddress(LibGL, 'glRectiv');
    @glRects := GetGLProcAddress(LibGL, 'glRects');
    @glRectsv := GetGLProcAddress(LibGL, 'glRectsv');
    @glRenderMode := GetGLProcAddress(LibGL, 'glRenderMode');
    @glRotated := GetGLProcAddress(LibGL, 'glRotated');
    @glRotatef := GetGLProcAddress(LibGL, 'glRotatef');
    @glScaled := GetGLProcAddress(LibGL, 'glScaled');
    @glScalef := GetGLProcAddress(LibGL, 'glScalef');
    @glScissor := GetGLProcAddress(LibGL, 'glScissor');
    @glSelectBuffer := GetGLProcAddress(LibGL, 'glSelectBuffer');
    @glShadeModel := GetGLProcAddress(LibGL, 'glShadeModel');
    @glStencilFunc := GetGLProcAddress(LibGL, 'glStencilFunc');
    @glStencilMask := GetGLProcAddress(LibGL, 'glStencilMask');
    @glStencilOp := GetGLProcAddress(LibGL, 'glStencilOp');
    @glTexCoord1d := GetGLProcAddress(LibGL, 'glTexCoord1d');
    @glTexCoord1dv := GetGLProcAddress(LibGL, 'glTexCoord1dv');
    @glTexCoord1f := GetGLProcAddress(LibGL, 'glTexCoord1f');
    @glTexCoord1fv := GetGLProcAddress(LibGL, 'glTexCoord1fv');
    @glTexCoord1i := GetGLProcAddress(LibGL, 'glTexCoord1i');
    @glTexCoord1iv := GetGLProcAddress(LibGL, 'glTexCoord1iv');
    @glTexCoord1s := GetGLProcAddress(LibGL, 'glTexCoord1s');
    @glTexCoord1sv := GetGLProcAddress(LibGL, 'glTexCoord1sv');
    @glTexCoord2d := GetGLProcAddress(LibGL, 'glTexCoord2d');
    @glTexCoord2dv := GetGLProcAddress(LibGL, 'glTexCoord2dv');
    @glTexCoord2f := GetGLProcAddress(LibGL, 'glTexCoord2f');
    @glTexCoord2fv := GetGLProcAddress(LibGL, 'glTexCoord2fv');
    @glTexCoord2i := GetGLProcAddress(LibGL, 'glTexCoord2i');
    @glTexCoord2iv := GetGLProcAddress(LibGL, 'glTexCoord2iv');
    @glTexCoord2s := GetGLProcAddress(LibGL, 'glTexCoord2s');
    @glTexCoord2sv := GetGLProcAddress(LibGL, 'glTexCoord2sv');
    @glTexCoord3d := GetGLProcAddress(LibGL, 'glTexCoord3d');
    @glTexCoord3dv := GetGLProcAddress(LibGL, 'glTexCoord3dv');
    @glTexCoord3f := GetGLProcAddress(LibGL, 'glTexCoord3f');
    @glTexCoord3fv := GetGLProcAddress(LibGL, 'glTexCoord3fv');
    @glTexCoord3i := GetGLProcAddress(LibGL, 'glTexCoord3i');
    @glTexCoord3iv := GetGLProcAddress(LibGL, 'glTexCoord3iv');
    @glTexCoord3s := GetGLProcAddress(LibGL, 'glTexCoord3s');
    @glTexCoord3sv := GetGLProcAddress(LibGL, 'glTexCoord3sv');
    @glTexCoord4d := GetGLProcAddress(LibGL, 'glTexCoord4d');
    @glTexCoord4dv := GetGLProcAddress(LibGL, 'glTexCoord4dv');
    @glTexCoord4f := GetGLProcAddress(LibGL, 'glTexCoord4f');
    @glTexCoord4fv := GetGLProcAddress(LibGL, 'glTexCoord4fv');
    @glTexCoord4i := GetGLProcAddress(LibGL, 'glTexCoord4i');
    @glTexCoord4iv := GetGLProcAddress(LibGL, 'glTexCoord4iv');
    @glTexCoord4s := GetGLProcAddress(LibGL, 'glTexCoord4s');
    @glTexCoord4sv := GetGLProcAddress(LibGL, 'glTexCoord4sv');
    @glTexCoordPointer := GetGLProcAddress(LibGL, 'glTexCoordPointer');
    @glTexEnvf := GetGLProcAddress(LibGL, 'glTexEnvf');
    @glTexEnvfv := GetGLProcAddress(LibGL, 'glTexEnvfv');
    @glTexEnvi := GetGLProcAddress(LibGL, 'glTexEnvi');
    @glTexEnviv := GetGLProcAddress(LibGL, 'glTexEnviv');
    @glTexGend := GetGLProcAddress(LibGL, 'glTexGend');
    @glTexGendv := GetGLProcAddress(LibGL, 'glTexGendv');
    @glTexGenf := GetGLProcAddress(LibGL, 'glTexGenf');
    @glTexGenfv := GetGLProcAddress(LibGL, 'glTexGenfv');
    @glTexGeni := GetGLProcAddress(LibGL, 'glTexGeni');
    @glTexGeniv := GetGLProcAddress(LibGL, 'glTexGeniv');
    @glTexImage1D := GetGLProcAddress(LibGL, 'glTexImage1D');
    @glTexImage2D := GetGLProcAddress(LibGL, 'glTexImage2D');
    @glTexParameterf := GetGLProcAddress(LibGL, 'glTexParameterf');
    @glTexParameterfv := GetGLProcAddress(LibGL, 'glTexParameterfv');
    @glTexParameteri := GetGLProcAddress(LibGL, 'glTexParameteri');
    @glTexParameteriv := GetGLProcAddress(LibGL, 'glTexParameteriv');
    @glTexSubImage1D := GetGLProcAddress(LibGL, 'glTexSubImage1D');
    @glTexSubImage2D := GetGLProcAddress(LibGL, 'glTexSubImage2D');
    @glTranslated := GetGLProcAddress(LibGL, 'glTranslated');
    @glTranslatef := GetGLProcAddress(LibGL, 'glTranslatef');
    @glVertex2d := GetGLProcAddress(LibGL, 'glVertex2d');
    @glVertex2dv := GetGLProcAddress(LibGL, 'glVertex2dv');
    @glVertex2f := GetGLProcAddress(LibGL, 'glVertex2f');
    @glVertex2fv := GetGLProcAddress(LibGL, 'glVertex2fv');
    @glVertex2i := GetGLProcAddress(LibGL, 'glVertex2i');
    @glVertex2iv := GetGLProcAddress(LibGL, 'glVertex2iv');
    @glVertex2s := GetGLProcAddress(LibGL, 'glVertex2s');
    @glVertex2sv := GetGLProcAddress(LibGL, 'glVertex2sv');
    @glVertex3d := GetGLProcAddress(LibGL, 'glVertex3d');
    @glVertex3dv := GetGLProcAddress(LibGL, 'glVertex3dv');
    @glVertex3f := GetGLProcAddress(LibGL, 'glVertex3f');
    @glVertex3fv := GetGLProcAddress(LibGL, 'glVertex3fv');
    @glVertex3i := GetGLProcAddress(LibGL, 'glVertex3i');
    @glVertex3iv := GetGLProcAddress(LibGL, 'glVertex3iv');
    @glVertex3s := GetGLProcAddress(LibGL, 'glVertex3s');
    @glVertex3sv := GetGLProcAddress(LibGL, 'glVertex3sv');
    @glVertex4d := GetGLProcAddress(LibGL, 'glVertex4d');
    @glVertex4dv := GetGLProcAddress(LibGL, 'glVertex4dv');
    @glVertex4f := GetGLProcAddress(LibGL, 'glVertex4f');
    @glVertex4fv := GetGLProcAddress(LibGL, 'glVertex4fv');
    @glVertex4i := GetGLProcAddress(LibGL, 'glVertex4i');
    @glVertex4iv := GetGLProcAddress(LibGL, 'glVertex4iv');
    @glVertex4s := GetGLProcAddress(LibGL, 'glVertex4s');
    @glVertex4sv := GetGLProcAddress(LibGL, 'glVertex4sv');
    @glVertexPointer := GetGLProcAddress(LibGL, 'glVertexPointer');
    @glViewport := GetGLProcAddress(LibGL, 'glViewport');
  except
    raise Exception.Create('Failed loading ' + MethodName +' from ' + dll);
  end;

  {$IFDEF Windows}
  try
    @ChoosePixelFormat := GetGLProcAddress(LibGL, 'ChoosePixelFormat');
    if not Assigned(ChoosePixelFormat) then
      @ChoosePixelFormat := @WinChoosePixelFormat;
  except
    raise Exception.Create('Unable to select pixel format');
  end;
  {$ENDIF}
end;
{$ENDIF MORPHOS}

// Extensions

{$IFNDEF Windows}
function wglGetProcAddress(proc: pansichar): Pointer;
begin
  Result := GetProcAddress(LibGL, proc);
end;
{$ENDIF}

function glext_ExtensionSupported(const extension: ansistring; const searchIn: ansistring): Boolean;
var
  extensions: pansichar;
  start: pansichar;
  where, terminator: pansichar;
begin

  if (Pos(' ', String(extension)) <> 0) or (extension = '') then
  begin
    Result := FALSE;
    Exit;
  end;

  if searchIn = '' then extensions := pansichar(glGetString(GL_EXTENSIONS))
  else extensions := pansichar(searchIn);
  start := extensions;
  while TRUE do
  begin
    where := StrPos(start, pansichar(extension));
    if where = nil then Break;
    terminator := Pointer(GLPtrUInt(GLPtrUInt(where) + GLPtrUint(Length(extension))));
    if (where = start) or (pansichar(GLPtrUInt(where) - 1)^ = ' ') then
    begin
      if (terminator^ = ' ') or (terminator^ = #0) then
      begin
        Result := TRUE;
        Exit;
      end;
    end;
    start := terminator;
  end;
  Result := FALSE;

end;

function Load_GL_version_1_2: Boolean;
begin

  Result := FALSE;

    glBlendColor := wglGetProcAddress('glBlendColor');
    if not Assigned(glBlendColor) then Exit;
    glBlendEquation := wglGetProcAddress('glBlendEquation');
    if not Assigned(glBlendEquation) then Exit;
    glDrawRangeElements := wglGetProcAddress('glDrawRangeElements');
    if not Assigned(glDrawRangeElements) then Exit;
    glTexImage3D := wglGetProcAddress('glTexImage3D');
    if not Assigned(glTexImage3D) then Exit;
    glTexSubImage3D := wglGetProcAddress('glTexSubImage3D');
    if not Assigned(glTexSubImage3D) then Exit;
    glCopyTexSubImage3D := wglGetProcAddress('glCopyTexSubImage3D');
    if not Assigned(glCopyTexSubImage3D) then Exit;
    Result := TRUE;

end;

function Load_GL_ARB_imaging: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_imaging', extstring) then
  begin
    glColorTable := wglGetProcAddress('glColorTable');
    if not Assigned(glColorTable) then Exit;
    glColorTableParameterfv := wglGetProcAddress('glColorTableParameterfv');
    if not Assigned(glColorTableParameterfv) then Exit;
    glColorTableParameteriv := wglGetProcAddress('glColorTableParameteriv');
    if not Assigned(glColorTableParameteriv) then Exit;
    glCopyColorTable := wglGetProcAddress('glCopyColorTable');
    if not Assigned(glCopyColorTable) then Exit;
    glGetColorTable := wglGetProcAddress('glGetColorTable');
    if not Assigned(glGetColorTable) then Exit;
    glGetColorTableParameterfv := wglGetProcAddress('glGetColorTableParameterfv');
    if not Assigned(glGetColorTableParameterfv) then Exit;
    glGetColorTableParameteriv := wglGetProcAddress('glGetColorTableParameteriv');
    if not Assigned(glGetColorTableParameteriv) then Exit;
    glColorSubTable := wglGetProcAddress('glColorSubTable');
    if not Assigned(glColorSubTable) then Exit;
    glCopyColorSubTable := wglGetProcAddress('glCopyColorSubTable');
    if not Assigned(glCopyColorSubTable) then Exit;
    glConvolutionFilter1D := wglGetProcAddress('glConvolutionFilter1D');
    if not Assigned(glConvolutionFilter1D) then Exit;
    glConvolutionFilter2D := wglGetProcAddress('glConvolutionFilter2D');
    if not Assigned(glConvolutionFilter2D) then Exit;
    glConvolutionParameterf := wglGetProcAddress('glConvolutionParameterf');
    if not Assigned(glConvolutionParameterf) then Exit;
    glConvolutionParameterfv := wglGetProcAddress('glConvolutionParameterfv');
    if not Assigned(glConvolutionParameterfv) then Exit;
    glConvolutionParameteri := wglGetProcAddress('glConvolutionParameteri');
    if not Assigned(glConvolutionParameteri) then Exit;
    glConvolutionParameteriv := wglGetProcAddress('glConvolutionParameteriv');
    if not Assigned(glConvolutionParameteriv) then Exit;
    glCopyConvolutionFilter1D := wglGetProcAddress('glCopyConvolutionFilter1D');
    if not Assigned(glCopyConvolutionFilter1D) then Exit;
    glCopyConvolutionFilter2D := wglGetProcAddress('glCopyConvolutionFilter2D');
    if not Assigned(glCopyConvolutionFilter2D) then Exit;
    glGetConvolutionFilter := wglGetProcAddress('glGetConvolutionFilter');
    if not Assigned(glGetConvolutionFilter) then Exit;
    glGetConvolutionParameterfv := wglGetProcAddress('glGetConvolutionParameterfv');
    if not Assigned(glGetConvolutionParameterfv) then Exit;
    glGetConvolutionParameteriv := wglGetProcAddress('glGetConvolutionParameteriv');
    if not Assigned(glGetConvolutionParameteriv) then Exit;
    glGetSeparableFilter := wglGetProcAddress('glGetSeparableFilter');
    if not Assigned(glGetSeparableFilter) then Exit;
    glSeparableFilter2D := wglGetProcAddress('glSeparableFilter2D');
    if not Assigned(glSeparableFilter2D) then Exit;
    glGetHistogram := wglGetProcAddress('glGetHistogram');
    if not Assigned(glGetHistogram) then Exit;
    glGetHistogramParameterfv := wglGetProcAddress('glGetHistogramParameterfv');
    if not Assigned(glGetHistogramParameterfv) then Exit;
    glGetHistogramParameteriv := wglGetProcAddress('glGetHistogramParameteriv');
    if not Assigned(glGetHistogramParameteriv) then Exit;
    glGetMinmax := wglGetProcAddress('glGetMinmax');
    if not Assigned(glGetMinmax) then Exit;
    glGetMinmaxParameterfv := wglGetProcAddress('glGetMinmaxParameterfv');
    if not Assigned(glGetMinmaxParameterfv) then Exit;
    glGetMinmaxParameteriv := wglGetProcAddress('glGetMinmaxParameteriv');
    if not Assigned(glGetMinmaxParameteriv) then Exit;
    glHistogram := wglGetProcAddress('glHistogram');
    if not Assigned(glHistogram) then Exit;
    glMinmax := wglGetProcAddress('glMinmax');
    if not Assigned(glMinmax) then Exit;
    glResetHistogram := wglGetProcAddress('glResetHistogram');
    if not Assigned(glResetHistogram) then Exit;
    glResetMinmax := wglGetProcAddress('glResetMinmax');
    if not Assigned(glResetMinmax) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_version_1_3: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

    glActiveTexture := wglGetProcAddress('glActiveTexture');
    if not Assigned(glActiveTexture) then Exit;
    glClientActiveTexture := wglGetProcAddress('glClientActiveTexture');
    if not Assigned(glClientActiveTexture) then Exit;
    glMultiTexCoord1d := wglGetProcAddress('glMultiTexCoord1d');
    if not Assigned(glMultiTexCoord1d) then Exit;
    glMultiTexCoord1dv := wglGetProcAddress('glMultiTexCoord1dv');
    if not Assigned(glMultiTexCoord1dv) then Exit;
    glMultiTexCoord1f := wglGetProcAddress('glMultiTexCoord1f');
    if not Assigned(glMultiTexCoord1f) then Exit;
    glMultiTexCoord1fv := wglGetProcAddress('glMultiTexCoord1fv');
    if not Assigned(glMultiTexCoord1fv) then Exit;
    glMultiTexCoord1i := wglGetProcAddress('glMultiTexCoord1i');
    if not Assigned(glMultiTexCoord1i) then Exit;
    glMultiTexCoord1iv := wglGetProcAddress('glMultiTexCoord1iv');
    if not Assigned(glMultiTexCoord1iv) then Exit;
    glMultiTexCoord1s := wglGetProcAddress('glMultiTexCoord1s');
    if not Assigned(glMultiTexCoord1s) then Exit;
    glMultiTexCoord1sv := wglGetProcAddress('glMultiTexCoord1sv');
    if not Assigned(glMultiTexCoord1sv) then Exit;
    glMultiTexCoord2d := wglGetProcAddress('glMultiTexCoord2d');
    if not Assigned(glMultiTexCoord2d) then Exit;
    glMultiTexCoord2dv := wglGetProcAddress('glMultiTexCoord2dv');
    if not Assigned(glMultiTexCoord2dv) then Exit;
    glMultiTexCoord2f := wglGetProcAddress('glMultiTexCoord2f');
    if not Assigned(glMultiTexCoord2f) then Exit;
    glMultiTexCoord2fv := wglGetProcAddress('glMultiTexCoord2fv');
    if not Assigned(glMultiTexCoord2fv) then Exit;
    glMultiTexCoord2i := wglGetProcAddress('glMultiTexCoord2i');
    if not Assigned(glMultiTexCoord2i) then Exit;
    glMultiTexCoord2iv := wglGetProcAddress('glMultiTexCoord2iv');
    if not Assigned(glMultiTexCoord2iv) then Exit;
    glMultiTexCoord2s := wglGetProcAddress('glMultiTexCoord2s');
    if not Assigned(glMultiTexCoord2s) then Exit;
    glMultiTexCoord2sv := wglGetProcAddress('glMultiTexCoord2sv');
    if not Assigned(glMultiTexCoord2sv) then Exit;
    glMultiTexCoord3d := wglGetProcAddress('glMultiTexCoord3d');
    if not Assigned(glMultiTexCoord3d) then Exit;
    glMultiTexCoord3dv := wglGetProcAddress('glMultiTexCoord3dv');
    if not Assigned(glMultiTexCoord3dv) then Exit;
    glMultiTexCoord3f := wglGetProcAddress('glMultiTexCoord3f');
    if not Assigned(glMultiTexCoord3f) then Exit;
    glMultiTexCoord3fv := wglGetProcAddress('glMultiTexCoord3fv');
    if not Assigned(glMultiTexCoord3fv) then Exit;
    glMultiTexCoord3i := wglGetProcAddress('glMultiTexCoord3i');
    if not Assigned(glMultiTexCoord3i) then Exit;
    glMultiTexCoord3iv := wglGetProcAddress('glMultiTexCoord3iv');
    if not Assigned(glMultiTexCoord3iv) then Exit;
    glMultiTexCoord3s := wglGetProcAddress('glMultiTexCoord3s');
    if not Assigned(glMultiTexCoord3s) then Exit;
    glMultiTexCoord3sv := wglGetProcAddress('glMultiTexCoord3sv');
    if not Assigned(glMultiTexCoord3sv) then Exit;
    glMultiTexCoord4d := wglGetProcAddress('glMultiTexCoord4d');
    if not Assigned(glMultiTexCoord4d) then Exit;
    glMultiTexCoord4dv := wglGetProcAddress('glMultiTexCoord4dv');
    if not Assigned(glMultiTexCoord4dv) then Exit;
    glMultiTexCoord4f := wglGetProcAddress('glMultiTexCoord4f');
    if not Assigned(glMultiTexCoord4f) then Exit;
    glMultiTexCoord4fv := wglGetProcAddress('glMultiTexCoord4fv');
    if not Assigned(glMultiTexCoord4fv) then Exit;
    glMultiTexCoord4i := wglGetProcAddress('glMultiTexCoord4i');
    if not Assigned(glMultiTexCoord4i) then Exit;
    glMultiTexCoord4iv := wglGetProcAddress('glMultiTexCoord4iv');
    if not Assigned(glMultiTexCoord4iv) then Exit;
    glMultiTexCoord4s := wglGetProcAddress('glMultiTexCoord4s');
    if not Assigned(glMultiTexCoord4s) then Exit;
    glMultiTexCoord4sv := wglGetProcAddress('glMultiTexCoord4sv');
    if not Assigned(glMultiTexCoord4sv) then Exit;
    glLoadTransposeMatrixf := wglGetProcAddress('glLoadTransposeMatrixf');
    if not Assigned(glLoadTransposeMatrixf) then Exit;
    glLoadTransposeMatrixd := wglGetProcAddress('glLoadTransposeMatrixd');
    if not Assigned(glLoadTransposeMatrixd) then Exit;
    glMultTransposeMatrixf := wglGetProcAddress('glMultTransposeMatrixf');
    if not Assigned(glMultTransposeMatrixf) then Exit;
    glMultTransposeMatrixd := wglGetProcAddress('glMultTransposeMatrixd');
    if not Assigned(glMultTransposeMatrixd) then Exit;
    glSampleCoverage := wglGetProcAddress('glSampleCoverage');
    if not Assigned(glSampleCoverage) then Exit;
    glCompressedTexImage3D := wglGetProcAddress('glCompressedTexImage3D');
    if not Assigned(glCompressedTexImage3D) then Exit;
    glCompressedTexImage2D := wglGetProcAddress('glCompressedTexImage2D');
    if not Assigned(glCompressedTexImage2D) then Exit;
    glCompressedTexImage1D := wglGetProcAddress('glCompressedTexImage1D');
    if not Assigned(glCompressedTexImage1D) then Exit;
    glCompressedTexSubImage3D := wglGetProcAddress('glCompressedTexSubImage3D');
    if not Assigned(glCompressedTexSubImage3D) then Exit;
    glCompressedTexSubImage2D := wglGetProcAddress('glCompressedTexSubImage2D');
    if not Assigned(glCompressedTexSubImage2D) then Exit;
    glCompressedTexSubImage1D := wglGetProcAddress('glCompressedTexSubImage1D');
    if not Assigned(glCompressedTexSubImage1D) then Exit;
    glGetCompressedTexImage := wglGetProcAddress('glGetCompressedTexImage');
    if not Assigned(glGetCompressedTexImage) then Exit;
    Result := Load_GL_version_1_2;

end;

function Load_GL_ARB_multitexture: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_multitexture', extstring) then
  begin
    glActiveTextureARB := wglGetProcAddress('glActiveTextureARB');
    if not Assigned(glActiveTextureARB) then Exit;
    glClientActiveTextureARB := wglGetProcAddress('glClientActiveTextureARB');
    if not Assigned(glClientActiveTextureARB) then Exit;
    glMultiTexCoord1dARB := wglGetProcAddress('glMultiTexCoord1dARB');
    if not Assigned(glMultiTexCoord1dARB) then Exit;
    glMultiTexCoord1dvARB := wglGetProcAddress('glMultiTexCoord1dvARB');
    if not Assigned(glMultiTexCoord1dvARB) then Exit;
    glMultiTexCoord1fARB := wglGetProcAddress('glMultiTexCoord1fARB');
    if not Assigned(glMultiTexCoord1fARB) then Exit;
    glMultiTexCoord1fvARB := wglGetProcAddress('glMultiTexCoord1fvARB');
    if not Assigned(glMultiTexCoord1fvARB) then Exit;
    glMultiTexCoord1iARB := wglGetProcAddress('glMultiTexCoord1iARB');
    if not Assigned(glMultiTexCoord1iARB) then Exit;
    glMultiTexCoord1ivARB := wglGetProcAddress('glMultiTexCoord1ivARB');
    if not Assigned(glMultiTexCoord1ivARB) then Exit;
    glMultiTexCoord1sARB := wglGetProcAddress('glMultiTexCoord1sARB');
    if not Assigned(glMultiTexCoord1sARB) then Exit;
    glMultiTexCoord1svARB := wglGetProcAddress('glMultiTexCoord1svARB');
    if not Assigned(glMultiTexCoord1svARB) then Exit;
    glMultiTexCoord2dARB := wglGetProcAddress('glMultiTexCoord2dARB');
    if not Assigned(glMultiTexCoord2dARB) then Exit;
    glMultiTexCoord2dvARB := wglGetProcAddress('glMultiTexCoord2dvARB');
    if not Assigned(glMultiTexCoord2dvARB) then Exit;
    glMultiTexCoord2fARB := wglGetProcAddress('glMultiTexCoord2fARB');
    if not Assigned(glMultiTexCoord2fARB) then Exit;
    glMultiTexCoord2fvARB := wglGetProcAddress('glMultiTexCoord2fvARB');
    if not Assigned(glMultiTexCoord2fvARB) then Exit;
    glMultiTexCoord2iARB := wglGetProcAddress('glMultiTexCoord2iARB');
    if not Assigned(glMultiTexCoord2iARB) then Exit;
    glMultiTexCoord2ivARB := wglGetProcAddress('glMultiTexCoord2ivARB');
    if not Assigned(glMultiTexCoord2ivARB) then Exit;
    glMultiTexCoord2sARB := wglGetProcAddress('glMultiTexCoord2sARB');
    if not Assigned(glMultiTexCoord2sARB) then Exit;
    glMultiTexCoord2svARB := wglGetProcAddress('glMultiTexCoord2svARB');
    if not Assigned(glMultiTexCoord2svARB) then Exit;
    glMultiTexCoord3dARB := wglGetProcAddress('glMultiTexCoord3dARB');
    if not Assigned(glMultiTexCoord3dARB) then Exit;
    glMultiTexCoord3dvARB := wglGetProcAddress('glMultiTexCoord3dvARB');
    if not Assigned(glMultiTexCoord3dvARB) then Exit;
    glMultiTexCoord3fARB := wglGetProcAddress('glMultiTexCoord3fARB');
    if not Assigned(glMultiTexCoord3fARB) then Exit;
    glMultiTexCoord3fvARB := wglGetProcAddress('glMultiTexCoord3fvARB');
    if not Assigned(glMultiTexCoord3fvARB) then Exit;
    glMultiTexCoord3iARB := wglGetProcAddress('glMultiTexCoord3iARB');
    if not Assigned(glMultiTexCoord3iARB) then Exit;
    glMultiTexCoord3ivARB := wglGetProcAddress('glMultiTexCoord3ivARB');
    if not Assigned(glMultiTexCoord3ivARB) then Exit;
    glMultiTexCoord3sARB := wglGetProcAddress('glMultiTexCoord3sARB');
    if not Assigned(glMultiTexCoord3sARB) then Exit;
    glMultiTexCoord3svARB := wglGetProcAddress('glMultiTexCoord3svARB');
    if not Assigned(glMultiTexCoord3svARB) then Exit;
    glMultiTexCoord4dARB := wglGetProcAddress('glMultiTexCoord4dARB');
    if not Assigned(glMultiTexCoord4dARB) then Exit;
    glMultiTexCoord4dvARB := wglGetProcAddress('glMultiTexCoord4dvARB');
    if not Assigned(glMultiTexCoord4dvARB) then Exit;
    glMultiTexCoord4fARB := wglGetProcAddress('glMultiTexCoord4fARB');
    if not Assigned(glMultiTexCoord4fARB) then Exit;
    glMultiTexCoord4fvARB := wglGetProcAddress('glMultiTexCoord4fvARB');
    if not Assigned(glMultiTexCoord4fvARB) then Exit;
    glMultiTexCoord4iARB := wglGetProcAddress('glMultiTexCoord4iARB');
    if not Assigned(glMultiTexCoord4iARB) then Exit;
    glMultiTexCoord4ivARB := wglGetProcAddress('glMultiTexCoord4ivARB');
    if not Assigned(glMultiTexCoord4ivARB) then Exit;
    glMultiTexCoord4sARB := wglGetProcAddress('glMultiTexCoord4sARB');
    if not Assigned(glMultiTexCoord4sARB) then Exit;
    glMultiTexCoord4svARB := wglGetProcAddress('glMultiTexCoord4svARB');
    if not Assigned(glMultiTexCoord4svARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_transpose_matrix: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_transpose_matrix', extstring) then
  begin
    glLoadTransposeMatrixfARB := wglGetProcAddress('glLoadTransposeMatrixfARB');
    if not Assigned(glLoadTransposeMatrixfARB) then Exit;
    glLoadTransposeMatrixdARB := wglGetProcAddress('glLoadTransposeMatrixdARB');
    if not Assigned(glLoadTransposeMatrixdARB) then Exit;
    glMultTransposeMatrixfARB := wglGetProcAddress('glMultTransposeMatrixfARB');
    if not Assigned(glMultTransposeMatrixfARB) then Exit;
    glMultTransposeMatrixdARB := wglGetProcAddress('glMultTransposeMatrixdARB');
    if not Assigned(glMultTransposeMatrixdARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_multisample: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_multisample', extstring) then
  begin
    glSampleCoverageARB := wglGetProcAddress('glSampleCoverageARB');
    if not Assigned(glSampleCoverageARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_env_add: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_env_add', extstring) then
  begin
    Result := TRUE;
  end;

end;

{$IFDEF Windows}
function Load_WGL_ARB_extensions_string: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_ARB_extensions_string', extstring) then
  begin
    wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
    if not Assigned(wglGetExtensionsStringARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_ARB_buffer_region: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_ARB_buffer_region', extstring) then
  begin
    wglCreateBufferRegionARB := wglGetProcAddress('wglCreateBufferRegionARB');
    if not Assigned(wglCreateBufferRegionARB) then Exit;
    wglDeleteBufferRegionARB := wglGetProcAddress('wglDeleteBufferRegionARB');
    if not Assigned(wglDeleteBufferRegionARB) then Exit;
    wglSaveBufferRegionARB := wglGetProcAddress('wglSaveBufferRegionARB');
    if not Assigned(wglSaveBufferRegionARB) then Exit;
    wglRestoreBufferRegionARB := wglGetProcAddress('wglRestoreBufferRegionARB');
    if not Assigned(wglRestoreBufferRegionARB) then Exit;
    Result := TRUE;
  end;

end;
{$ENDIF}

function Load_GL_ARB_texture_cube_map: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_cube_map', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_depth_texture: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_depth_texture', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_point_parameters: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_point_parameters', extstring) then
  begin
    glPointParameterfARB := wglGetProcAddress('glPointParameterfARB');
    if not Assigned(glPointParameterfARB) then Exit;
    glPointParameterfvARB := wglGetProcAddress('glPointParameterfvARB');
    if not Assigned(glPointParameterfvARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_shadow: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shadow', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_shadow_ambient: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shadow_ambient', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_border_clamp: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_border_clamp', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_compression: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_compression', extstring) then
  begin
    glCompressedTexImage3DARB := wglGetProcAddress('glCompressedTexImage3DARB');
    if not Assigned(glCompressedTexImage3DARB) then Exit;
    glCompressedTexImage2DARB := wglGetProcAddress('glCompressedTexImage2DARB');
    if not Assigned(glCompressedTexImage2DARB) then Exit;
    glCompressedTexImage1DARB := wglGetProcAddress('glCompressedTexImage1DARB');
    if not Assigned(glCompressedTexImage1DARB) then Exit;
    glCompressedTexSubImage3DARB := wglGetProcAddress('glCompressedTexSubImage3DARB');
    if not Assigned(glCompressedTexSubImage3DARB) then Exit;
    glCompressedTexSubImage2DARB := wglGetProcAddress('glCompressedTexSubImage2DARB');
    if not Assigned(glCompressedTexSubImage2DARB) then Exit;
    glCompressedTexSubImage1DARB := wglGetProcAddress('glCompressedTexSubImage1DARB');
    if not Assigned(glCompressedTexSubImage1DARB) then Exit;
    glGetCompressedTexImageARB := wglGetProcAddress('glGetCompressedTexImageARB');
    if not Assigned(glGetCompressedTexImageARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_env_combine: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_env_combine', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_env_crossbar: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_env_crossbar', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_env_dot3: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_env_dot3', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_mirrored_repeat: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_mirrored_repeat', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_vertex_blend: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_blend', extstring) then
  begin
    glWeightbvARB := wglGetProcAddress('glWeightbvARB');
    if not Assigned(glWeightbvARB) then Exit;
    glWeightsvARB := wglGetProcAddress('glWeightsvARB');
    if not Assigned(glWeightsvARB) then Exit;
    glWeightivARB := wglGetProcAddress('glWeightivARB');
    if not Assigned(glWeightivARB) then Exit;
    glWeightfvARB := wglGetProcAddress('glWeightfvARB');
    if not Assigned(glWeightfvARB) then Exit;
    glWeightdvARB := wglGetProcAddress('glWeightdvARB');
    if not Assigned(glWeightdvARB) then Exit;
    glWeightvARB := wglGetProcAddress('glWeightvARB');
    if not Assigned(glWeightvARB) then Exit;
    glWeightubvARB := wglGetProcAddress('glWeightubvARB');
    if not Assigned(glWeightubvARB) then Exit;
    glWeightusvARB := wglGetProcAddress('glWeightusvARB');
    if not Assigned(glWeightusvARB) then Exit;
    glWeightuivARB := wglGetProcAddress('glWeightuivARB');
    if not Assigned(glWeightuivARB) then Exit;
    glWeightPointerARB := wglGetProcAddress('glWeightPointerARB');
    if not Assigned(glWeightPointerARB) then Exit;
    glVertexBlendARB := wglGetProcAddress('glVertexBlendARB');
    if not Assigned(glVertexBlendARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_vertex_program: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_program', extstring) then
  begin
    glVertexAttrib1sARB := wglGetProcAddress('glVertexAttrib1sARB');
    if not Assigned(glVertexAttrib1sARB) then Exit;
    glVertexAttrib1fARB := wglGetProcAddress('glVertexAttrib1fARB');
    if not Assigned(glVertexAttrib1fARB) then Exit;
    glVertexAttrib1dARB := wglGetProcAddress('glVertexAttrib1dARB');
    if not Assigned(glVertexAttrib1dARB) then Exit;
    glVertexAttrib2sARB := wglGetProcAddress('glVertexAttrib2sARB');
    if not Assigned(glVertexAttrib2sARB) then Exit;
    glVertexAttrib2fARB := wglGetProcAddress('glVertexAttrib2fARB');
    if not Assigned(glVertexAttrib2fARB) then Exit;
    glVertexAttrib2dARB := wglGetProcAddress('glVertexAttrib2dARB');
    if not Assigned(glVertexAttrib2dARB) then Exit;
    glVertexAttrib3sARB := wglGetProcAddress('glVertexAttrib3sARB');
    if not Assigned(glVertexAttrib3sARB) then Exit;
    glVertexAttrib3fARB := wglGetProcAddress('glVertexAttrib3fARB');
    if not Assigned(glVertexAttrib3fARB) then Exit;
    glVertexAttrib3dARB := wglGetProcAddress('glVertexAttrib3dARB');
    if not Assigned(glVertexAttrib3dARB) then Exit;
    glVertexAttrib4sARB := wglGetProcAddress('glVertexAttrib4sARB');
    if not Assigned(glVertexAttrib4sARB) then Exit;
    glVertexAttrib4fARB := wglGetProcAddress('glVertexAttrib4fARB');
    if not Assigned(glVertexAttrib4fARB) then Exit;
    glVertexAttrib4dARB := wglGetProcAddress('glVertexAttrib4dARB');
    if not Assigned(glVertexAttrib4dARB) then Exit;
    glVertexAttrib4NubARB := wglGetProcAddress('glVertexAttrib4NubARB');
    if not Assigned(glVertexAttrib4NubARB) then Exit;
    glVertexAttrib1svARB := wglGetProcAddress('glVertexAttrib1svARB');
    if not Assigned(glVertexAttrib1svARB) then Exit;
    glVertexAttrib1fvARB := wglGetProcAddress('glVertexAttrib1fvARB');
    if not Assigned(glVertexAttrib1fvARB) then Exit;
    glVertexAttrib1dvARB := wglGetProcAddress('glVertexAttrib1dvARB');
    if not Assigned(glVertexAttrib1dvARB) then Exit;
    glVertexAttrib2svARB := wglGetProcAddress('glVertexAttrib2svARB');
    if not Assigned(glVertexAttrib2svARB) then Exit;
    glVertexAttrib2fvARB := wglGetProcAddress('glVertexAttrib2fvARB');
    if not Assigned(glVertexAttrib2fvARB) then Exit;
    glVertexAttrib2dvARB := wglGetProcAddress('glVertexAttrib2dvARB');
    if not Assigned(glVertexAttrib2dvARB) then Exit;
    glVertexAttrib3svARB := wglGetProcAddress('glVertexAttrib3svARB');
    if not Assigned(glVertexAttrib3svARB) then Exit;
    glVertexAttrib3fvARB := wglGetProcAddress('glVertexAttrib3fvARB');
    if not Assigned(glVertexAttrib3fvARB) then Exit;
    glVertexAttrib3dvARB := wglGetProcAddress('glVertexAttrib3dvARB');
    if not Assigned(glVertexAttrib3dvARB) then Exit;
    glVertexAttrib4bvARB := wglGetProcAddress('glVertexAttrib4bvARB');
    if not Assigned(glVertexAttrib4bvARB) then Exit;
    glVertexAttrib4svARB := wglGetProcAddress('glVertexAttrib4svARB');
    if not Assigned(glVertexAttrib4svARB) then Exit;
    glVertexAttrib4ivARB := wglGetProcAddress('glVertexAttrib4ivARB');
    if not Assigned(glVertexAttrib4ivARB) then Exit;
    glVertexAttrib4ubvARB := wglGetProcAddress('glVertexAttrib4ubvARB');
    if not Assigned(glVertexAttrib4ubvARB) then Exit;
    glVertexAttrib4usvARB := wglGetProcAddress('glVertexAttrib4usvARB');
    if not Assigned(glVertexAttrib4usvARB) then Exit;
    glVertexAttrib4uivARB := wglGetProcAddress('glVertexAttrib4uivARB');
    if not Assigned(glVertexAttrib4uivARB) then Exit;
    glVertexAttrib4fvARB := wglGetProcAddress('glVertexAttrib4fvARB');
    if not Assigned(glVertexAttrib4fvARB) then Exit;
    glVertexAttrib4dvARB := wglGetProcAddress('glVertexAttrib4dvARB');
    if not Assigned(glVertexAttrib4dvARB) then Exit;
    glVertexAttrib4NbvARB := wglGetProcAddress('glVertexAttrib4NbvARB');
    if not Assigned(glVertexAttrib4NbvARB) then Exit;
    glVertexAttrib4NsvARB := wglGetProcAddress('glVertexAttrib4NsvARB');
    if not Assigned(glVertexAttrib4NsvARB) then Exit;
    glVertexAttrib4NivARB := wglGetProcAddress('glVertexAttrib4NivARB');
    if not Assigned(glVertexAttrib4NivARB) then Exit;
    glVertexAttrib4NubvARB := wglGetProcAddress('glVertexAttrib4NubvARB');
    if not Assigned(glVertexAttrib4NubvARB) then Exit;
    glVertexAttrib4NusvARB := wglGetProcAddress('glVertexAttrib4NusvARB');
    if not Assigned(glVertexAttrib4NusvARB) then Exit;
    glVertexAttrib4NuivARB := wglGetProcAddress('glVertexAttrib4NuivARB');
    if not Assigned(glVertexAttrib4NuivARB) then Exit;
    glVertexAttribPointerARB := wglGetProcAddress('glVertexAttribPointerARB');
    if not Assigned(glVertexAttribPointerARB) then Exit;
    glEnableVertexAttribArrayARB := wglGetProcAddress('glEnableVertexAttribArrayARB');
    if not Assigned(glEnableVertexAttribArrayARB) then Exit;
    glDisableVertexAttribArrayARB := wglGetProcAddress('glDisableVertexAttribArrayARB');
    if not Assigned(glDisableVertexAttribArrayARB) then Exit;
    glProgramStringARB := wglGetProcAddress('glProgramStringARB');
    if not Assigned(glProgramStringARB) then Exit;
    glBindProgramARB := wglGetProcAddress('glBindProgramARB');
    if not Assigned(glBindProgramARB) then Exit;
    glDeleteProgramsARB := wglGetProcAddress('glDeleteProgramsARB');
    if not Assigned(glDeleteProgramsARB) then Exit;
    glGenProgramsARB := wglGetProcAddress('glGenProgramsARB');
    if not Assigned(glGenProgramsARB) then Exit;
    glProgramEnvParameter4dARB := wglGetProcAddress('glProgramEnvParameter4dARB');
    if not Assigned(glProgramEnvParameter4dARB) then Exit;
    glProgramEnvParameter4dvARB := wglGetProcAddress('glProgramEnvParameter4dvARB');
    if not Assigned(glProgramEnvParameter4dvARB) then Exit;
    glProgramEnvParameter4fARB := wglGetProcAddress('glProgramEnvParameter4fARB');
    if not Assigned(glProgramEnvParameter4fARB) then Exit;
    glProgramEnvParameter4fvARB := wglGetProcAddress('glProgramEnvParameter4fvARB');
    if not Assigned(glProgramEnvParameter4fvARB) then Exit;
    glProgramLocalParameter4dARB := wglGetProcAddress('glProgramLocalParameter4dARB');
    if not Assigned(glProgramLocalParameter4dARB) then Exit;
    glProgramLocalParameter4dvARB := wglGetProcAddress('glProgramLocalParameter4dvARB');
    if not Assigned(glProgramLocalParameter4dvARB) then Exit;
    glProgramLocalParameter4fARB := wglGetProcAddress('glProgramLocalParameter4fARB');
    if not Assigned(glProgramLocalParameter4fARB) then Exit;
    glProgramLocalParameter4fvARB := wglGetProcAddress('glProgramLocalParameter4fvARB');
    if not Assigned(glProgramLocalParameter4fvARB) then Exit;
    glGetProgramEnvParameterdvARB := wglGetProcAddress('glGetProgramEnvParameterdvARB');
    if not Assigned(glGetProgramEnvParameterdvARB) then Exit;
    glGetProgramEnvParameterfvARB := wglGetProcAddress('glGetProgramEnvParameterfvARB');
    if not Assigned(glGetProgramEnvParameterfvARB) then Exit;
    glGetProgramLocalParameterdvARB := wglGetProcAddress('glGetProgramLocalParameterdvARB');
    if not Assigned(glGetProgramLocalParameterdvARB) then Exit;
    glGetProgramLocalParameterfvARB := wglGetProcAddress('glGetProgramLocalParameterfvARB');
    if not Assigned(glGetProgramLocalParameterfvARB) then Exit;
    glGetProgramivARB := wglGetProcAddress('glGetProgramivARB');
    if not Assigned(glGetProgramivARB) then Exit;
    glGetProgramStringARB := wglGetProcAddress('glGetProgramStringARB');
    if not Assigned(glGetProgramStringARB) then Exit;
    glGetVertexAttribdvARB := wglGetProcAddress('glGetVertexAttribdvARB');
    if not Assigned(glGetVertexAttribdvARB) then Exit;
    glGetVertexAttribfvARB := wglGetProcAddress('glGetVertexAttribfvARB');
    if not Assigned(glGetVertexAttribfvARB) then Exit;
    glGetVertexAttribivARB := wglGetProcAddress('glGetVertexAttribivARB');
    if not Assigned(glGetVertexAttribivARB) then Exit;
    glGetVertexAttribPointervARB := wglGetProcAddress('glGetVertexAttribPointervARB');
    if not Assigned(glGetVertexAttribPointervARB) then Exit;
    glIsProgramARB := wglGetProcAddress('glIsProgramARB');
    if not Assigned(glIsProgramARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_window_pos: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_window_pos', extstring) then
  begin
    glWindowPos2dARB := wglGetProcAddress('glWindowPos2dARB');
    if not Assigned(glWindowPos2dARB) then Exit;
    glWindowPos2fARB := wglGetProcAddress('glWindowPos2fARB');
    if not Assigned(glWindowPos2fARB) then Exit;
    glWindowPos2iARB := wglGetProcAddress('glWindowPos2iARB');
    if not Assigned(glWindowPos2iARB) then Exit;
    glWindowPos2sARB := wglGetProcAddress('glWindowPos2sARB');
    if not Assigned(glWindowPos2sARB) then Exit;
    glWindowPos2dvARB := wglGetProcAddress('glWindowPos2dvARB');
    if not Assigned(glWindowPos2dvARB) then Exit;
    glWindowPos2fvARB := wglGetProcAddress('glWindowPos2fvARB');
    if not Assigned(glWindowPos2fvARB) then Exit;
    glWindowPos2ivARB := wglGetProcAddress('glWindowPos2ivARB');
    if not Assigned(glWindowPos2ivARB) then Exit;
    glWindowPos2svARB := wglGetProcAddress('glWindowPos2svARB');
    if not Assigned(glWindowPos2svARB) then Exit;
    glWindowPos3dARB := wglGetProcAddress('glWindowPos3dARB');
    if not Assigned(glWindowPos3dARB) then Exit;
    glWindowPos3fARB := wglGetProcAddress('glWindowPos3fARB');
    if not Assigned(glWindowPos3fARB) then Exit;
    glWindowPos3iARB := wglGetProcAddress('glWindowPos3iARB');
    if not Assigned(glWindowPos3iARB) then Exit;
    glWindowPos3sARB := wglGetProcAddress('glWindowPos3sARB');
    if not Assigned(glWindowPos3sARB) then Exit;
    glWindowPos3dvARB := wglGetProcAddress('glWindowPos3dvARB');
    if not Assigned(glWindowPos3dvARB) then Exit;
    glWindowPos3fvARB := wglGetProcAddress('glWindowPos3fvARB');
    if not Assigned(glWindowPos3fvARB) then Exit;
    glWindowPos3ivARB := wglGetProcAddress('glWindowPos3ivARB');
    if not Assigned(glWindowPos3ivARB) then Exit;
    glWindowPos3svARB := wglGetProcAddress('glWindowPos3svARB');
    if not Assigned(glWindowPos3svARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_422_pixels: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_422_pixels', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_abgr: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_abgr', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_bgra: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_bgra', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_blend_color: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_color', extstring) then
  begin
    glBlendColorEXT := wglGetProcAddress('glBlendColorEXT');
    if not Assigned(glBlendColorEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_blend_func_separate: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_func_separate', extstring) then
  begin
    glBlendFuncSeparateEXT := wglGetProcAddress('glBlendFuncSeparateEXT');
    if not Assigned(glBlendFuncSeparateEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_blend_logic_op: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_logic_op', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_blend_minmax: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_minmax', extstring) then
  begin
    glBlendEquationEXT := wglGetProcAddress('glBlendEquationEXT');
    if not Assigned(glBlendEquationEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_blend_subtract: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_subtract', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_clip_volume_hint: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_clip_volume_hint', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_color_subtable: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_color_subtable', extstring) then
  begin
    glColorSubTableEXT := wglGetProcAddress('glColorSubTableEXT');
    if not Assigned(glColorSubTableEXT) then Exit;
    glCopyColorSubTableEXT := wglGetProcAddress('glCopyColorSubTableEXT');
    if not Assigned(glCopyColorSubTableEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_compiled_vertex_array: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_compiled_vertex_array', extstring) then
  begin
    glLockArraysEXT := wglGetProcAddress('glLockArraysEXT');
    if not Assigned(glLockArraysEXT) then Exit;
    glUnlockArraysEXT := wglGetProcAddress('glUnlockArraysEXT');
    if not Assigned(glUnlockArraysEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_convolution: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_convolution', extstring) then
  begin
    glConvolutionFilter1DEXT := wglGetProcAddress('glConvolutionFilter1DEXT');
    if not Assigned(glConvolutionFilter1DEXT) then Exit;
    glConvolutionFilter2DEXT := wglGetProcAddress('glConvolutionFilter2DEXT');
    if not Assigned(glConvolutionFilter2DEXT) then Exit;
    glCopyConvolutionFilter1DEXT := wglGetProcAddress('glCopyConvolutionFilter1DEXT');
    if not Assigned(glCopyConvolutionFilter1DEXT) then Exit;
    glCopyConvolutionFilter2DEXT := wglGetProcAddress('glCopyConvolutionFilter2DEXT');
    if not Assigned(glCopyConvolutionFilter2DEXT) then Exit;
    glGetConvolutionFilterEXT := wglGetProcAddress('glGetConvolutionFilterEXT');
    if not Assigned(glGetConvolutionFilterEXT) then Exit;
    glSeparableFilter2DEXT := wglGetProcAddress('glSeparableFilter2DEXT');
    if not Assigned(glSeparableFilter2DEXT) then Exit;
    glGetSeparableFilterEXT := wglGetProcAddress('glGetSeparableFilterEXT');
    if not Assigned(glGetSeparableFilterEXT) then Exit;
    glConvolutionParameteriEXT := wglGetProcAddress('glConvolutionParameteriEXT');
    if not Assigned(glConvolutionParameteriEXT) then Exit;
    glConvolutionParameterivEXT := wglGetProcAddress('glConvolutionParameterivEXT');
    if not Assigned(glConvolutionParameterivEXT) then Exit;
    glConvolutionParameterfEXT := wglGetProcAddress('glConvolutionParameterfEXT');
    if not Assigned(glConvolutionParameterfEXT) then Exit;
    glConvolutionParameterfvEXT := wglGetProcAddress('glConvolutionParameterfvEXT');
    if not Assigned(glConvolutionParameterfvEXT) then Exit;
    glGetConvolutionParameterivEXT := wglGetProcAddress('glGetConvolutionParameterivEXT');
    if not Assigned(glGetConvolutionParameterivEXT) then Exit;
    glGetConvolutionParameterfvEXT := wglGetProcAddress('glGetConvolutionParameterfvEXT');
    if not Assigned(glGetConvolutionParameterfvEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_fog_coord: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_fog_coord', extstring) then
  begin
    glFogCoordfEXT := wglGetProcAddress('glFogCoordfEXT');
    if not Assigned(glFogCoordfEXT) then Exit;
    glFogCoorddEXT := wglGetProcAddress('glFogCoorddEXT');
    if not Assigned(glFogCoorddEXT) then Exit;
    glFogCoordfvEXT := wglGetProcAddress('glFogCoordfvEXT');
    if not Assigned(glFogCoordfvEXT) then Exit;
    glFogCoorddvEXT := wglGetProcAddress('glFogCoorddvEXT');
    if not Assigned(glFogCoorddvEXT) then Exit;
    glFogCoordPointerEXT := wglGetProcAddress('glFogCoordPointerEXT');
    if not Assigned(glFogCoordPointerEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_histogram: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_histogram', extstring) then
  begin
    glHistogramEXT := wglGetProcAddress('glHistogramEXT');
    if not Assigned(glHistogramEXT) then Exit;
    glResetHistogramEXT := wglGetProcAddress('glResetHistogramEXT');
    if not Assigned(glResetHistogramEXT) then Exit;
    glGetHistogramEXT := wglGetProcAddress('glGetHistogramEXT');
    if not Assigned(glGetHistogramEXT) then Exit;
    glGetHistogramParameterivEXT := wglGetProcAddress('glGetHistogramParameterivEXT');
    if not Assigned(glGetHistogramParameterivEXT) then Exit;
    glGetHistogramParameterfvEXT := wglGetProcAddress('glGetHistogramParameterfvEXT');
    if not Assigned(glGetHistogramParameterfvEXT) then Exit;
    glMinmaxEXT := wglGetProcAddress('glMinmaxEXT');
    if not Assigned(glMinmaxEXT) then Exit;
    glResetMinmaxEXT := wglGetProcAddress('glResetMinmaxEXT');
    if not Assigned(glResetMinmaxEXT) then Exit;
    glGetMinmaxEXT := wglGetProcAddress('glGetMinmaxEXT');
    if not Assigned(glGetMinmaxEXT) then Exit;
    glGetMinmaxParameterivEXT := wglGetProcAddress('glGetMinmaxParameterivEXT');
    if not Assigned(glGetMinmaxParameterivEXT) then Exit;
    glGetMinmaxParameterfvEXT := wglGetProcAddress('glGetMinmaxParameterfvEXT');
    if not Assigned(glGetMinmaxParameterfvEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_multi_draw_arrays: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_multi_draw_arrays', extstring) then
  begin
    glMultiDrawArraysEXT := wglGetProcAddress('glMultiDrawArraysEXT');
    if not Assigned(glMultiDrawArraysEXT) then Exit;
    glMultiDrawElementsEXT := wglGetProcAddress('glMultiDrawElementsEXT');
    if not Assigned(glMultiDrawElementsEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_packed_depth_stencil: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_packed_depth_stencil', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_packed_pixels: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_packed_pixels', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_paletted_texture: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_paletted_texture', extstring) then
  begin
    glColorTableEXT := wglGetProcAddress('glColorTableEXT');
    if not Assigned(glColorTableEXT) then Exit;
    glColorSubTableEXT := wglGetProcAddress('glColorSubTableEXT');
    if not Assigned(glColorSubTableEXT) then Exit;
    glGetColorTableEXT := wglGetProcAddress('glGetColorTableEXT');
    if not Assigned(glGetColorTableEXT) then Exit;
    glGetColorTableParameterivEXT := wglGetProcAddress('glGetColorTableParameterivEXT');
    if not Assigned(glGetColorTableParameterivEXT) then Exit;
    glGetColorTableParameterfvEXT := wglGetProcAddress('glGetColorTableParameterfvEXT');
    if not Assigned(glGetColorTableParameterfvEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_point_parameters: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_point_parameters', extstring) then
  begin
    glPointParameterfEXT := wglGetProcAddress('glPointParameterfEXT');
    if not Assigned(glPointParameterfEXT) then Exit;
    glPointParameterfvEXT := wglGetProcAddress('glPointParameterfvEXT');
    if not Assigned(glPointParameterfvEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_polygon_offset: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_polygon_offset', extstring) then
  begin
    glPolygonOffsetEXT := wglGetProcAddress('glPolygonOffsetEXT');
    if not Assigned(glPolygonOffsetEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_secondary_color: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_secondary_color', extstring) then
  begin
    glSecondaryColor3bEXT := wglGetProcAddress('glSecondaryColor3bEXT');
    if not Assigned(glSecondaryColor3bEXT) then Exit;
    glSecondaryColor3sEXT := wglGetProcAddress('glSecondaryColor3sEXT');
    if not Assigned(glSecondaryColor3sEXT) then Exit;
    glSecondaryColor3iEXT := wglGetProcAddress('glSecondaryColor3iEXT');
    if not Assigned(glSecondaryColor3iEXT) then Exit;
    glSecondaryColor3fEXT := wglGetProcAddress('glSecondaryColor3fEXT');
    if not Assigned(glSecondaryColor3fEXT) then Exit;
    glSecondaryColor3dEXT := wglGetProcAddress('glSecondaryColor3dEXT');
    if not Assigned(glSecondaryColor3dEXT) then Exit;
    glSecondaryColor3ubEXT := wglGetProcAddress('glSecondaryColor3ubEXT');
    if not Assigned(glSecondaryColor3ubEXT) then Exit;
    glSecondaryColor3usEXT := wglGetProcAddress('glSecondaryColor3usEXT');
    if not Assigned(glSecondaryColor3usEXT) then Exit;
    glSecondaryColor3uiEXT := wglGetProcAddress('glSecondaryColor3uiEXT');
    if not Assigned(glSecondaryColor3uiEXT) then Exit;
    glSecondaryColor3bvEXT := wglGetProcAddress('glSecondaryColor3bvEXT');
    if not Assigned(glSecondaryColor3bvEXT) then Exit;
    glSecondaryColor3svEXT := wglGetProcAddress('glSecondaryColor3svEXT');
    if not Assigned(glSecondaryColor3svEXT) then Exit;
    glSecondaryColor3ivEXT := wglGetProcAddress('glSecondaryColor3ivEXT');
    if not Assigned(glSecondaryColor3ivEXT) then Exit;
    glSecondaryColor3fvEXT := wglGetProcAddress('glSecondaryColor3fvEXT');
    if not Assigned(glSecondaryColor3fvEXT) then Exit;
    glSecondaryColor3dvEXT := wglGetProcAddress('glSecondaryColor3dvEXT');
    if not Assigned(glSecondaryColor3dvEXT) then Exit;
    glSecondaryColor3ubvEXT := wglGetProcAddress('glSecondaryColor3ubvEXT');
    if not Assigned(glSecondaryColor3ubvEXT) then Exit;
    glSecondaryColor3usvEXT := wglGetProcAddress('glSecondaryColor3usvEXT');
    if not Assigned(glSecondaryColor3usvEXT) then Exit;
    glSecondaryColor3uivEXT := wglGetProcAddress('glSecondaryColor3uivEXT');
    if not Assigned(glSecondaryColor3uivEXT) then Exit;
    glSecondaryColorPointerEXT := wglGetProcAddress('glSecondaryColorPointerEXT');
    if not Assigned(glSecondaryColorPointerEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_separate_specular_color: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_separate_specular_color', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_shadow_funcs: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_shadow_funcs', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_shared_texture_palette: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_shared_texture_palette', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_stencil_two_side: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_stencil_two_side', extstring) then
  begin
    glActiveStencilFaceEXT := wglGetProcAddress('glActiveStencilFaceEXT');
    if not Assigned(glActiveStencilFaceEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_stencil_wrap: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_stencil_wrap', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_subtexture: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_subtexture', extstring) then
  begin
    glTexSubImage1DEXT := wglGetProcAddress('glTexSubImage1DEXT');
    if not Assigned(glTexSubImage1DEXT) then Exit;
    glTexSubImage2DEXT := wglGetProcAddress('glTexSubImage2DEXT');
    if not Assigned(glTexSubImage2DEXT) then Exit;
    glTexSubImage3DEXT := wglGetProcAddress('glTexSubImage3DEXT');
    if not Assigned(glTexSubImage3DEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture3D: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture3D', extstring) then
  begin
    glTexImage3DEXT := wglGetProcAddress('glTexImage3DEXT');
    if not Assigned(glTexImage3DEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_compression_s3tc: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_compression_s3tc', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_env_add: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_env_add', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_env_combine: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_env_combine', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_env_dot3: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_env_dot3', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_filter_anisotropic: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_filter_anisotropic', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_lod_bias: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_lod_bias', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_object: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_object', extstring) then
  begin
    glGenTexturesEXT := wglGetProcAddress('glGenTexturesEXT');
    if not Assigned(glGenTexturesEXT) then Exit;
    glDeleteTexturesEXT := wglGetProcAddress('glDeleteTexturesEXT');
    if not Assigned(glDeleteTexturesEXT) then Exit;
    glBindTextureEXT := wglGetProcAddress('glBindTextureEXT');
    if not Assigned(glBindTextureEXT) then Exit;
    glPrioritizeTexturesEXT := wglGetProcAddress('glPrioritizeTexturesEXT');
    if not Assigned(glPrioritizeTexturesEXT) then Exit;
    glAreTexturesResidentEXT := wglGetProcAddress('glAreTexturesResidentEXT');
    if not Assigned(glAreTexturesResidentEXT) then Exit;
    glIsTextureEXT := wglGetProcAddress('glIsTextureEXT');
    if not Assigned(glIsTextureEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_vertex_array: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_vertex_array', extstring) then
  begin
    glArrayElementEXT := wglGetProcAddress('glArrayElementEXT');
    if not Assigned(glArrayElementEXT) then Exit;
    glDrawArraysEXT := wglGetProcAddress('glDrawArraysEXT');
    if not Assigned(glDrawArraysEXT) then Exit;
    glVertexPointerEXT := wglGetProcAddress('glVertexPointerEXT');
    if not Assigned(glVertexPointerEXT) then Exit;
    glNormalPointerEXT := wglGetProcAddress('glNormalPointerEXT');
    if not Assigned(glNormalPointerEXT) then Exit;
    glColorPointerEXT := wglGetProcAddress('glColorPointerEXT');
    if not Assigned(glColorPointerEXT) then Exit;
    glIndexPointerEXT := wglGetProcAddress('glIndexPointerEXT');
    if not Assigned(glIndexPointerEXT) then Exit;
    glTexCoordPointerEXT := wglGetProcAddress('glTexCoordPointerEXT');
    if not Assigned(glTexCoordPointerEXT) then Exit;
    glEdgeFlagPointerEXT := wglGetProcAddress('glEdgeFlagPointerEXT');
    if not Assigned(glEdgeFlagPointerEXT) then Exit;
    glGetPointervEXT := wglGetProcAddress('glGetPointervEXT');
    if not Assigned(glGetPointervEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_vertex_shader: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_vertex_shader', extstring) then
  begin
    glBeginVertexShaderEXT := wglGetProcAddress('glBeginVertexShaderEXT');
    if not Assigned(glBeginVertexShaderEXT) then Exit;
    glEndVertexShaderEXT := wglGetProcAddress('glEndVertexShaderEXT');
    if not Assigned(glEndVertexShaderEXT) then Exit;
    glBindVertexShaderEXT := wglGetProcAddress('glBindVertexShaderEXT');
    if not Assigned(glBindVertexShaderEXT) then Exit;
    glGenVertexShadersEXT := wglGetProcAddress('glGenVertexShadersEXT');
    if not Assigned(glGenVertexShadersEXT) then Exit;
    glDeleteVertexShaderEXT := wglGetProcAddress('glDeleteVertexShaderEXT');
    if not Assigned(glDeleteVertexShaderEXT) then Exit;
    glShaderOp1EXT := wglGetProcAddress('glShaderOp1EXT');
    if not Assigned(glShaderOp1EXT) then Exit;
    glShaderOp2EXT := wglGetProcAddress('glShaderOp2EXT');
    if not Assigned(glShaderOp2EXT) then Exit;
    glShaderOp3EXT := wglGetProcAddress('glShaderOp3EXT');
    if not Assigned(glShaderOp3EXT) then Exit;
    glSwizzleEXT := wglGetProcAddress('glSwizzleEXT');
    if not Assigned(glSwizzleEXT) then Exit;
    glWriteMaskEXT := wglGetProcAddress('glWriteMaskEXT');
    if not Assigned(glWriteMaskEXT) then Exit;
    glInsertComponentEXT := wglGetProcAddress('glInsertComponentEXT');
    if not Assigned(glInsertComponentEXT) then Exit;
    glExtractComponentEXT := wglGetProcAddress('glExtractComponentEXT');
    if not Assigned(glExtractComponentEXT) then Exit;
    glGenSymbolsEXT := wglGetProcAddress('glGenSymbolsEXT');
    if not Assigned(glGenSymbolsEXT) then Exit;
    glSetInvariantEXT := wglGetProcAddress('glSetInvariantEXT');
    if not Assigned(glSetInvariantEXT) then Exit;
    glSetLocalConstantEXT := wglGetProcAddress('glSetLocalConstantEXT');
    if not Assigned(glSetLocalConstantEXT) then Exit;
    glVariantbvEXT := wglGetProcAddress('glVariantbvEXT');
    if not Assigned(glVariantbvEXT) then Exit;
    glVariantsvEXT := wglGetProcAddress('glVariantsvEXT');
    if not Assigned(glVariantsvEXT) then Exit;
    glVariantivEXT := wglGetProcAddress('glVariantivEXT');
    if not Assigned(glVariantivEXT) then Exit;
    glVariantfvEXT := wglGetProcAddress('glVariantfvEXT');
    if not Assigned(glVariantfvEXT) then Exit;
    glVariantdvEXT := wglGetProcAddress('glVariantdvEXT');
    if not Assigned(glVariantdvEXT) then Exit;
    glVariantubvEXT := wglGetProcAddress('glVariantubvEXT');
    if not Assigned(glVariantubvEXT) then Exit;
    glVariantusvEXT := wglGetProcAddress('glVariantusvEXT');
    if not Assigned(glVariantusvEXT) then Exit;
    glVariantuivEXT := wglGetProcAddress('glVariantuivEXT');
    if not Assigned(glVariantuivEXT) then Exit;
    glVariantPointerEXT := wglGetProcAddress('glVariantPointerEXT');
    if not Assigned(glVariantPointerEXT) then Exit;
    glEnableVariantClientStateEXT := wglGetProcAddress('glEnableVariantClientStateEXT');
    if not Assigned(glEnableVariantClientStateEXT) then Exit;
    glDisableVariantClientStateEXT := wglGetProcAddress('glDisableVariantClientStateEXT');
    if not Assigned(glDisableVariantClientStateEXT) then Exit;
    glBindLightParameterEXT := wglGetProcAddress('glBindLightParameterEXT');
    if not Assigned(glBindLightParameterEXT) then Exit;
    glBindMaterialParameterEXT := wglGetProcAddress('glBindMaterialParameterEXT');
    if not Assigned(glBindMaterialParameterEXT) then Exit;
    glBindTexGenParameterEXT := wglGetProcAddress('glBindTexGenParameterEXT');
    if not Assigned(glBindTexGenParameterEXT) then Exit;
    glBindTextureUnitParameterEXT := wglGetProcAddress('glBindTextureUnitParameterEXT');
    if not Assigned(glBindTextureUnitParameterEXT) then Exit;
    glBindParameterEXT := wglGetProcAddress('glBindParameterEXT');
    if not Assigned(glBindParameterEXT) then Exit;
    glIsVariantEnabledEXT := wglGetProcAddress('glIsVariantEnabledEXT');
    if not Assigned(glIsVariantEnabledEXT) then Exit;
    glGetVariantBooleanvEXT := wglGetProcAddress('glGetVariantBooleanvEXT');
    if not Assigned(glGetVariantBooleanvEXT) then Exit;
    glGetVariantIntegervEXT := wglGetProcAddress('glGetVariantIntegervEXT');
    if not Assigned(glGetVariantIntegervEXT) then Exit;
    glGetVariantFloatvEXT := wglGetProcAddress('glGetVariantFloatvEXT');
    if not Assigned(glGetVariantFloatvEXT) then Exit;
    glGetVariantPointervEXT := wglGetProcAddress('glGetVariantPointervEXT');
    if not Assigned(glGetVariantPointervEXT) then Exit;
    glGetInvariantBooleanvEXT := wglGetProcAddress('glGetInvariantBooleanvEXT');
    if not Assigned(glGetInvariantBooleanvEXT) then Exit;
    glGetInvariantIntegervEXT := wglGetProcAddress('glGetInvariantIntegervEXT');
    if not Assigned(glGetInvariantIntegervEXT) then Exit;
    glGetInvariantFloatvEXT := wglGetProcAddress('glGetInvariantFloatvEXT');
    if not Assigned(glGetInvariantFloatvEXT) then Exit;
    glGetLocalConstantBooleanvEXT := wglGetProcAddress('glGetLocalConstantBooleanvEXT');
    if not Assigned(glGetLocalConstantBooleanvEXT) then Exit;
    glGetLocalConstantIntegervEXT := wglGetProcAddress('glGetLocalConstantIntegervEXT');
    if not Assigned(glGetLocalConstantIntegervEXT) then Exit;
    glGetLocalConstantFloatvEXT := wglGetProcAddress('glGetLocalConstantFloatvEXT');
    if not Assigned(glGetLocalConstantFloatvEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_vertex_weighting: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_vertex_weighting', extstring) then
  begin
    glVertexWeightfEXT := wglGetProcAddress('glVertexWeightfEXT');
    if not Assigned(glVertexWeightfEXT) then Exit;
    glVertexWeightfvEXT := wglGetProcAddress('glVertexWeightfvEXT');
    if not Assigned(glVertexWeightfvEXT) then Exit;
    glVertexWeightPointerEXT := wglGetProcAddress('glVertexWeightPointerEXT');
    if not Assigned(glVertexWeightPointerEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_HP_occlusion_test: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_HP_occlusion_test', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_blend_square: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_blend_square', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_copy_depth_to_color: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_copy_depth_to_color', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_depth_clamp: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_depth_clamp', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_evaluators: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_evaluators', extstring) then
  begin
    glMapControlPointsNV := wglGetProcAddress('glMapControlPointsNV');
    if not Assigned(glMapControlPointsNV) then Exit;
    glMapParameterivNV := wglGetProcAddress('glMapParameterivNV');
    if not Assigned(glMapParameterivNV) then Exit;
    glMapParameterfvNV := wglGetProcAddress('glMapParameterfvNV');
    if not Assigned(glMapParameterfvNV) then Exit;
    glGetMapControlPointsNV := wglGetProcAddress('glGetMapControlPointsNV');
    if not Assigned(glGetMapControlPointsNV) then Exit;
    glGetMapParameterivNV := wglGetProcAddress('glGetMapParameterivNV');
    if not Assigned(glGetMapParameterivNV) then Exit;
    glGetMapParameterfvNV := wglGetProcAddress('glGetMapParameterfvNV');
    if not Assigned(glGetMapParameterfvNV) then Exit;
    glGetMapAttribParameterivNV := wglGetProcAddress('glGetMapAttribParameterivNV');
    if not Assigned(glGetMapAttribParameterivNV) then Exit;
    glGetMapAttribParameterfvNV := wglGetProcAddress('glGetMapAttribParameterfvNV');
    if not Assigned(glGetMapAttribParameterfvNV) then Exit;
    glEvalMapsNV := wglGetProcAddress('glEvalMapsNV');
    if not Assigned(glEvalMapsNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_fence: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fence', extstring) then
  begin
    glGenFencesNV := wglGetProcAddress('glGenFencesNV');
    if not Assigned(glGenFencesNV) then Exit;
    glDeleteFencesNV := wglGetProcAddress('glDeleteFencesNV');
    if not Assigned(glDeleteFencesNV) then Exit;
    glSetFenceNV := wglGetProcAddress('glSetFenceNV');
    if not Assigned(glSetFenceNV) then Exit;
    glTestFenceNV := wglGetProcAddress('glTestFenceNV');
    if not Assigned(glTestFenceNV) then Exit;
    glFinishFenceNV := wglGetProcAddress('glFinishFenceNV');
    if not Assigned(glFinishFenceNV) then Exit;
    glIsFenceNV := wglGetProcAddress('glIsFenceNV');
    if not Assigned(glIsFenceNV) then Exit;
    glGetFenceivNV := wglGetProcAddress('glGetFenceivNV');
    if not Assigned(glGetFenceivNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_fog_distance: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fog_distance', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_light_max_exponent: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_light_max_exponent', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_multisample_filter_hint: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_multisample_filter_hint', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_occlusion_query: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_occlusion_query', extstring) then
  begin
    glGenOcclusionQueriesNV := wglGetProcAddress('glGenOcclusionQueriesNV');
    if not Assigned(glGenOcclusionQueriesNV) then Exit;
    glDeleteOcclusionQueriesNV := wglGetProcAddress('glDeleteOcclusionQueriesNV');
    if not Assigned(glDeleteOcclusionQueriesNV) then Exit;
    glIsOcclusionQueryNV := wglGetProcAddress('glIsOcclusionQueryNV');
    if not Assigned(glIsOcclusionQueryNV) then Exit;
    glBeginOcclusionQueryNV := wglGetProcAddress('glBeginOcclusionQueryNV');
    if not Assigned(glBeginOcclusionQueryNV) then Exit;
    glEndOcclusionQueryNV := wglGetProcAddress('glEndOcclusionQueryNV');
    if not Assigned(glEndOcclusionQueryNV) then Exit;
    glGetOcclusionQueryivNV := wglGetProcAddress('glGetOcclusionQueryivNV');
    if not Assigned(glGetOcclusionQueryivNV) then Exit;
    glGetOcclusionQueryuivNV := wglGetProcAddress('glGetOcclusionQueryuivNV');
    if not Assigned(glGetOcclusionQueryuivNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_packed_depth_stencil: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_packed_depth_stencil', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_point_sprite: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_point_sprite', extstring) then
  begin
    glPointParameteriNV := wglGetProcAddress('glPointParameteriNV');
    if not Assigned(glPointParameteriNV) then Exit;
    glPointParameterivNV := wglGetProcAddress('glPointParameterivNV');
    if not Assigned(glPointParameterivNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_register_combiners: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_register_combiners', extstring) then
  begin
    glCombinerParameterfvNV := wglGetProcAddress('glCombinerParameterfvNV');
    if not Assigned(glCombinerParameterfvNV) then Exit;
    glCombinerParameterivNV := wglGetProcAddress('glCombinerParameterivNV');
    if not Assigned(glCombinerParameterivNV) then Exit;
    glCombinerParameterfNV := wglGetProcAddress('glCombinerParameterfNV');
    if not Assigned(glCombinerParameterfNV) then Exit;
    glCombinerParameteriNV := wglGetProcAddress('glCombinerParameteriNV');
    if not Assigned(glCombinerParameteriNV) then Exit;
    glCombinerInputNV := wglGetProcAddress('glCombinerInputNV');
    if not Assigned(glCombinerInputNV) then Exit;
    glCombinerOutputNV := wglGetProcAddress('glCombinerOutputNV');
    if not Assigned(glCombinerOutputNV) then Exit;
    glFinalCombinerInputNV := wglGetProcAddress('glFinalCombinerInputNV');
    if not Assigned(glFinalCombinerInputNV) then Exit;
    glGetCombinerInputParameterfvNV := wglGetProcAddress('glGetCombinerInputParameterfvNV');
    if not Assigned(glGetCombinerInputParameterfvNV) then Exit;
    glGetCombinerInputParameterivNV := wglGetProcAddress('glGetCombinerInputParameterivNV');
    if not Assigned(glGetCombinerInputParameterivNV) then Exit;
    glGetCombinerOutputParameterfvNV := wglGetProcAddress('glGetCombinerOutputParameterfvNV');
    if not Assigned(glGetCombinerOutputParameterfvNV) then Exit;
    glGetCombinerOutputParameterivNV := wglGetProcAddress('glGetCombinerOutputParameterivNV');
    if not Assigned(glGetCombinerOutputParameterivNV) then Exit;
    glGetFinalCombinerInputParameterfvNV := wglGetProcAddress('glGetFinalCombinerInputParameterfvNV');
    if not Assigned(glGetFinalCombinerInputParameterfvNV) then Exit;
    glGetFinalCombinerInputParameterivNV := wglGetProcAddress('glGetFinalCombinerInputParameterivNV');
    if not Assigned(glGetFinalCombinerInputParameterivNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_register_combiners2: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_register_combiners2', extstring) then
  begin
    glCombinerStageParameterfvNV := wglGetProcAddress('glCombinerStageParameterfvNV');
    if not Assigned(glCombinerStageParameterfvNV) then Exit;
    glGetCombinerStageParameterfvNV := wglGetProcAddress('glGetCombinerStageParameterfvNV');
    if not Assigned(glGetCombinerStageParameterfvNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_texgen_emboss: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texgen_emboss', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_texgen_reflection: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texgen_reflection', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_texture_compression_vtc: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_compression_vtc', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_texture_env_combine4: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_env_combine4', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_texture_rectangle: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_rectangle', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_texture_shader: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_shader', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_texture_shader2: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_shader2', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_texture_shader3: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_shader3', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_vertex_array_range: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_array_range', extstring) then
  begin
    glVertexArrayRangeNV := wglGetProcAddress('glVertexArrayRangeNV');
    if not Assigned(glVertexArrayRangeNV) then Exit;
    glFlushVertexArrayRangeNV := wglGetProcAddress('glFlushVertexArrayRangeNV');
    if not Assigned(glFlushVertexArrayRangeNV) then Exit;
{$IFDEF Windows}
    wglAllocateMemoryNV := wglGetProcAddress('wglAllocateMemoryNV');
    if not Assigned(wglAllocateMemoryNV) then Exit;
    wglFreeMemoryNV := wglGetProcAddress('wglFreeMemoryNV');
    if not Assigned(wglFreeMemoryNV) then Exit;
{$ENDIF}
    Result := TRUE;
  end;

end;

function Load_GL_NV_vertex_array_range2: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_array_range2', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_vertex_program: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program', extstring) then
  begin
    glBindProgramNV := wglGetProcAddress('glBindProgramNV');
    if not Assigned(glBindProgramNV) then Exit;
    glDeleteProgramsNV := wglGetProcAddress('glDeleteProgramsNV');
    if not Assigned(glDeleteProgramsNV) then Exit;
    glExecuteProgramNV := wglGetProcAddress('glExecuteProgramNV');
    if not Assigned(glExecuteProgramNV) then Exit;
    glGenProgramsNV := wglGetProcAddress('glGenProgramsNV');
    if not Assigned(glGenProgramsNV) then Exit;
    glAreProgramsResidentNV := wglGetProcAddress('glAreProgramsResidentNV');
    if not Assigned(glAreProgramsResidentNV) then Exit;
    glRequestResidentProgramsNV := wglGetProcAddress('glRequestResidentProgramsNV');
    if not Assigned(glRequestResidentProgramsNV) then Exit;
    glGetProgramParameterfvNV := wglGetProcAddress('glGetProgramParameterfvNV');
    if not Assigned(glGetProgramParameterfvNV) then Exit;
    glGetProgramParameterdvNV := wglGetProcAddress('glGetProgramParameterdvNV');
    if not Assigned(glGetProgramParameterdvNV) then Exit;
    glGetProgramivNV := wglGetProcAddress('glGetProgramivNV');
    if not Assigned(glGetProgramivNV) then Exit;
    glGetProgramStringNV := wglGetProcAddress('glGetProgramStringNV');
    if not Assigned(glGetProgramStringNV) then Exit;
    glGetTrackMatrixivNV := wglGetProcAddress('glGetTrackMatrixivNV');
    if not Assigned(glGetTrackMatrixivNV) then Exit;
    glGetVertexAttribdvNV := wglGetProcAddress('glGetVertexAttribdvNV');
    if not Assigned(glGetVertexAttribdvNV) then Exit;
    glGetVertexAttribfvNV := wglGetProcAddress('glGetVertexAttribfvNV');
    if not Assigned(glGetVertexAttribfvNV) then Exit;
    glGetVertexAttribivNV := wglGetProcAddress('glGetVertexAttribivNV');
    if not Assigned(glGetVertexAttribivNV) then Exit;
    glGetVertexAttribPointervNV := wglGetProcAddress('glGetVertexAttribPointervNV');
    if not Assigned(glGetVertexAttribPointervNV) then Exit;
    glIsProgramNV := wglGetProcAddress('glIsProgramNV');
    if not Assigned(glIsProgramNV) then Exit;
    glLoadProgramNV := wglGetProcAddress('glLoadProgramNV');
    if not Assigned(glLoadProgramNV) then Exit;
    glProgramParameter4fNV := wglGetProcAddress('glProgramParameter4fNV');
    if not Assigned(glProgramParameter4fNV) then Exit;
    glProgramParameter4fvNV := wglGetProcAddress('glProgramParameter4fvNV');
    if not Assigned(glProgramParameter4fvNV) then Exit;
    glProgramParameters4dvNV := wglGetProcAddress('glProgramParameters4dvNV');
    if not Assigned(glProgramParameters4dvNV) then Exit;
    glProgramParameters4fvNV := wglGetProcAddress('glProgramParameters4fvNV');
    if not Assigned(glProgramParameters4fvNV) then Exit;
    glTrackMatrixNV := wglGetProcAddress('glTrackMatrixNV');
    if not Assigned(glTrackMatrixNV) then Exit;
    glVertexAttribPointerNV := wglGetProcAddress('glVertexAttribPointerNV');
    if not Assigned(glVertexAttribPointerNV) then Exit;
    glVertexAttrib1sNV := wglGetProcAddress('glVertexAttrib1sNV');
    if not Assigned(glVertexAttrib1sNV) then Exit;
    glVertexAttrib1fNV := wglGetProcAddress('glVertexAttrib1fNV');
    if not Assigned(glVertexAttrib1fNV) then Exit;
    glVertexAttrib1dNV := wglGetProcAddress('glVertexAttrib1dNV');
    if not Assigned(glVertexAttrib1dNV) then Exit;
    glVertexAttrib2sNV := wglGetProcAddress('glVertexAttrib2sNV');
    if not Assigned(glVertexAttrib2sNV) then Exit;
    glVertexAttrib2fNV := wglGetProcAddress('glVertexAttrib2fNV');
    if not Assigned(glVertexAttrib2fNV) then Exit;
    glVertexAttrib2dNV := wglGetProcAddress('glVertexAttrib2dNV');
    if not Assigned(glVertexAttrib2dNV) then Exit;
    glVertexAttrib3sNV := wglGetProcAddress('glVertexAttrib3sNV');
    if not Assigned(glVertexAttrib3sNV) then Exit;
    glVertexAttrib3fNV := wglGetProcAddress('glVertexAttrib3fNV');
    if not Assigned(glVertexAttrib3fNV) then Exit;
    glVertexAttrib3dNV := wglGetProcAddress('glVertexAttrib3dNV');
    if not Assigned(glVertexAttrib3dNV) then Exit;
    glVertexAttrib4sNV := wglGetProcAddress('glVertexAttrib4sNV');
    if not Assigned(glVertexAttrib4sNV) then Exit;
    glVertexAttrib4fNV := wglGetProcAddress('glVertexAttrib4fNV');
    if not Assigned(glVertexAttrib4fNV) then Exit;
    glVertexAttrib4dNV := wglGetProcAddress('glVertexAttrib4dNV');
    if not Assigned(glVertexAttrib4dNV) then Exit;
    glVertexAttrib4ubNV := wglGetProcAddress('glVertexAttrib4ubNV');
    if not Assigned(glVertexAttrib4ubNV) then Exit;
    glVertexAttrib1svNV := wglGetProcAddress('glVertexAttrib1svNV');
    if not Assigned(glVertexAttrib1svNV) then Exit;
    glVertexAttrib1fvNV := wglGetProcAddress('glVertexAttrib1fvNV');
    if not Assigned(glVertexAttrib1fvNV) then Exit;
    glVertexAttrib1dvNV := wglGetProcAddress('glVertexAttrib1dvNV');
    if not Assigned(glVertexAttrib1dvNV) then Exit;
    glVertexAttrib2svNV := wglGetProcAddress('glVertexAttrib2svNV');
    if not Assigned(glVertexAttrib2svNV) then Exit;
    glVertexAttrib2fvNV := wglGetProcAddress('glVertexAttrib2fvNV');
    if not Assigned(glVertexAttrib2fvNV) then Exit;
    glVertexAttrib2dvNV := wglGetProcAddress('glVertexAttrib2dvNV');
    if not Assigned(glVertexAttrib2dvNV) then Exit;
    glVertexAttrib3svNV := wglGetProcAddress('glVertexAttrib3svNV');
    if not Assigned(glVertexAttrib3svNV) then Exit;
    glVertexAttrib3fvNV := wglGetProcAddress('glVertexAttrib3fvNV');
    if not Assigned(glVertexAttrib3fvNV) then Exit;
    glVertexAttrib3dvNV := wglGetProcAddress('glVertexAttrib3dvNV');
    if not Assigned(glVertexAttrib3dvNV) then Exit;
    glVertexAttrib4svNV := wglGetProcAddress('glVertexAttrib4svNV');
    if not Assigned(glVertexAttrib4svNV) then Exit;
    glVertexAttrib4fvNV := wglGetProcAddress('glVertexAttrib4fvNV');
    if not Assigned(glVertexAttrib4fvNV) then Exit;
    glVertexAttrib4dvNV := wglGetProcAddress('glVertexAttrib4dvNV');
    if not Assigned(glVertexAttrib4dvNV) then Exit;
    glVertexAttrib4ubvNV := wglGetProcAddress('glVertexAttrib4ubvNV');
    if not Assigned(glVertexAttrib4ubvNV) then Exit;
    glVertexAttribs1svNV := wglGetProcAddress('glVertexAttribs1svNV');
    if not Assigned(glVertexAttribs1svNV) then Exit;
    glVertexAttribs1fvNV := wglGetProcAddress('glVertexAttribs1fvNV');
    if not Assigned(glVertexAttribs1fvNV) then Exit;
    glVertexAttribs1dvNV := wglGetProcAddress('glVertexAttribs1dvNV');
    if not Assigned(glVertexAttribs1dvNV) then Exit;
    glVertexAttribs2svNV := wglGetProcAddress('glVertexAttribs2svNV');
    if not Assigned(glVertexAttribs2svNV) then Exit;
    glVertexAttribs2fvNV := wglGetProcAddress('glVertexAttribs2fvNV');
    if not Assigned(glVertexAttribs2fvNV) then Exit;
    glVertexAttribs2dvNV := wglGetProcAddress('glVertexAttribs2dvNV');
    if not Assigned(glVertexAttribs2dvNV) then Exit;
    glVertexAttribs3svNV := wglGetProcAddress('glVertexAttribs3svNV');
    if not Assigned(glVertexAttribs3svNV) then Exit;
    glVertexAttribs3fvNV := wglGetProcAddress('glVertexAttribs3fvNV');
    if not Assigned(glVertexAttribs3fvNV) then Exit;
    glVertexAttribs3dvNV := wglGetProcAddress('glVertexAttribs3dvNV');
    if not Assigned(glVertexAttribs3dvNV) then Exit;
    glVertexAttribs4svNV := wglGetProcAddress('glVertexAttribs4svNV');
    if not Assigned(glVertexAttribs4svNV) then Exit;
    glVertexAttribs4fvNV := wglGetProcAddress('glVertexAttribs4fvNV');
    if not Assigned(glVertexAttribs4fvNV) then Exit;
    glVertexAttribs4dvNV := wglGetProcAddress('glVertexAttribs4dvNV');
    if not Assigned(glVertexAttribs4dvNV) then Exit;
    glVertexAttribs4ubvNV := wglGetProcAddress('glVertexAttribs4ubvNV');
    if not Assigned(glVertexAttribs4ubvNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_vertex_program1_1: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program1_1', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ATI_element_array: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_element_array', extstring) then
  begin
    glElementPointerATI := wglGetProcAddress('glElementPointerATI');
    if not Assigned(glElementPointerATI) then Exit;
    glDrawElementArrayATI := wglGetProcAddress('glDrawElementArrayATI');
    if not Assigned(glDrawElementArrayATI) then Exit;
    glDrawRangeElementArrayATI := wglGetProcAddress('glDrawRangeElementArrayATI');
    if not Assigned(glDrawRangeElementArrayATI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_envmap_bumpmap: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_envmap_bumpmap', extstring) then
  begin
    glTexBumpParameterivATI := wglGetProcAddress('glTexBumpParameterivATI');
    if not Assigned(glTexBumpParameterivATI) then Exit;
    glTexBumpParameterfvATI := wglGetProcAddress('glTexBumpParameterfvATI');
    if not Assigned(glTexBumpParameterfvATI) then Exit;
    glGetTexBumpParameterivATI := wglGetProcAddress('glGetTexBumpParameterivATI');
    if not Assigned(glGetTexBumpParameterivATI) then Exit;
    glGetTexBumpParameterfvATI := wglGetProcAddress('glGetTexBumpParameterfvATI');
    if not Assigned(glGetTexBumpParameterfvATI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_fragment_shader: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_fragment_shader', extstring) then
  begin
    glGenFragmentShadersATI := wglGetProcAddress('glGenFragmentShadersATI');
    if not Assigned(glGenFragmentShadersATI) then Exit;
    glBindFragmentShaderATI := wglGetProcAddress('glBindFragmentShaderATI');
    if not Assigned(glBindFragmentShaderATI) then Exit;
    glDeleteFragmentShaderATI := wglGetProcAddress('glDeleteFragmentShaderATI');
    if not Assigned(glDeleteFragmentShaderATI) then Exit;
    glBeginFragmentShaderATI := wglGetProcAddress('glBeginFragmentShaderATI');
    if not Assigned(glBeginFragmentShaderATI) then Exit;
    glEndFragmentShaderATI := wglGetProcAddress('glEndFragmentShaderATI');
    if not Assigned(glEndFragmentShaderATI) then Exit;
    glPassTexCoordATI := wglGetProcAddress('glPassTexCoordATI');
    if not Assigned(glPassTexCoordATI) then Exit;
    glSampleMapATI := wglGetProcAddress('glSampleMapATI');
    if not Assigned(glSampleMapATI) then Exit;
    glColorFragmentOp1ATI := wglGetProcAddress('glColorFragmentOp1ATI');
    if not Assigned(glColorFragmentOp1ATI) then Exit;
    glColorFragmentOp2ATI := wglGetProcAddress('glColorFragmentOp2ATI');
    if not Assigned(glColorFragmentOp2ATI) then Exit;
    glColorFragmentOp3ATI := wglGetProcAddress('glColorFragmentOp3ATI');
    if not Assigned(glColorFragmentOp3ATI) then Exit;
    glAlphaFragmentOp1ATI := wglGetProcAddress('glAlphaFragmentOp1ATI');
    if not Assigned(glAlphaFragmentOp1ATI) then Exit;
    glAlphaFragmentOp2ATI := wglGetProcAddress('glAlphaFragmentOp2ATI');
    if not Assigned(glAlphaFragmentOp2ATI) then Exit;
    glAlphaFragmentOp3ATI := wglGetProcAddress('glAlphaFragmentOp3ATI');
    if not Assigned(glAlphaFragmentOp3ATI) then Exit;
    glSetFragmentShaderConstantATI := wglGetProcAddress('glSetFragmentShaderConstantATI');
    if not Assigned(glSetFragmentShaderConstantATI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_pn_triangles: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_pn_triangles', extstring) then
  begin
    glPNTrianglesiATI := wglGetProcAddress('glPNTrianglesiATI');
    if not Assigned(glPNTrianglesiATI) then Exit;
    glPNTrianglesfATI := wglGetProcAddress('glPNTrianglesfATI');
    if not Assigned(glPNTrianglesfATI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_texture_mirror_once: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_texture_mirror_once', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ATI_vertex_array_object: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_vertex_array_object', extstring) then
  begin
    glNewObjectBufferATI := wglGetProcAddress('glNewObjectBufferATI');
    if not Assigned(glNewObjectBufferATI) then Exit;
    glIsObjectBufferATI := wglGetProcAddress('glIsObjectBufferATI');
    if not Assigned(glIsObjectBufferATI) then Exit;
    glUpdateObjectBufferATI := wglGetProcAddress('glUpdateObjectBufferATI');
    if not Assigned(glUpdateObjectBufferATI) then Exit;
    glGetObjectBufferfvATI := wglGetProcAddress('glGetObjectBufferfvATI');
    if not Assigned(glGetObjectBufferfvATI) then Exit;
    glGetObjectBufferivATI := wglGetProcAddress('glGetObjectBufferivATI');
    if not Assigned(glGetObjectBufferivATI) then Exit;
    glDeleteObjectBufferATI := wglGetProcAddress('glDeleteObjectBufferATI');
    if not Assigned(glDeleteObjectBufferATI) then Exit;
    glArrayObjectATI := wglGetProcAddress('glArrayObjectATI');
    if not Assigned(glArrayObjectATI) then Exit;
    glGetArrayObjectfvATI := wglGetProcAddress('glGetArrayObjectfvATI');
    if not Assigned(glGetArrayObjectfvATI) then Exit;
    glGetArrayObjectivATI := wglGetProcAddress('glGetArrayObjectivATI');
    if not Assigned(glGetArrayObjectivATI) then Exit;
    glVariantArrayObjectATI := wglGetProcAddress('glVariantArrayObjectATI');
    if not Assigned(glVariantArrayObjectATI) then Exit;
    glGetVariantArrayObjectfvATI := wglGetProcAddress('glGetVariantArrayObjectfvATI');
    if not Assigned(glGetVariantArrayObjectfvATI) then Exit;
    glGetVariantArrayObjectivATI := wglGetProcAddress('glGetVariantArrayObjectivATI');
    if not Assigned(glGetVariantArrayObjectivATI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_vertex_streams: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_vertex_streams', extstring) then
  begin
    glVertexStream1s := wglGetProcAddress('glVertexStream1s');
    if not Assigned(glVertexStream1s) then Exit;
    glVertexStream1i := wglGetProcAddress('glVertexStream1i');
    if not Assigned(glVertexStream1i) then Exit;
    glVertexStream1f := wglGetProcAddress('glVertexStream1f');
    if not Assigned(glVertexStream1f) then Exit;
    glVertexStream1d := wglGetProcAddress('glVertexStream1d');
    if not Assigned(glVertexStream1d) then Exit;
    glVertexStream1sv := wglGetProcAddress('glVertexStream1sv');
    if not Assigned(glVertexStream1sv) then Exit;
    glVertexStream1iv := wglGetProcAddress('glVertexStream1iv');
    if not Assigned(glVertexStream1iv) then Exit;
    glVertexStream1fv := wglGetProcAddress('glVertexStream1fv');
    if not Assigned(glVertexStream1fv) then Exit;
    glVertexStream1dv := wglGetProcAddress('glVertexStream1dv');
    if not Assigned(glVertexStream1dv) then Exit;
    glVertexStream2s := wglGetProcAddress('glVertexStream2s');
    if not Assigned(glVertexStream2s) then Exit;
    glVertexStream2i := wglGetProcAddress('glVertexStream2i');
    if not Assigned(glVertexStream2i) then Exit;
    glVertexStream2f := wglGetProcAddress('glVertexStream2f');
    if not Assigned(glVertexStream2f) then Exit;
    glVertexStream2d := wglGetProcAddress('glVertexStream2d');
    if not Assigned(glVertexStream2d) then Exit;
    glVertexStream2sv := wglGetProcAddress('glVertexStream2sv');
    if not Assigned(glVertexStream2sv) then Exit;
    glVertexStream2iv := wglGetProcAddress('glVertexStream2iv');
    if not Assigned(glVertexStream2iv) then Exit;
    glVertexStream2fv := wglGetProcAddress('glVertexStream2fv');
    if not Assigned(glVertexStream2fv) then Exit;
    glVertexStream2dv := wglGetProcAddress('glVertexStream2dv');
    if not Assigned(glVertexStream2dv) then Exit;
    glVertexStream3s := wglGetProcAddress('glVertexStream3s');
    if not Assigned(glVertexStream3s) then Exit;
    glVertexStream3i := wglGetProcAddress('glVertexStream3i');
    if not Assigned(glVertexStream3i) then Exit;
    glVertexStream3f := wglGetProcAddress('glVertexStream3f');
    if not Assigned(glVertexStream3f) then Exit;
    glVertexStream3d := wglGetProcAddress('glVertexStream3d');
    if not Assigned(glVertexStream3d) then Exit;
    glVertexStream3sv := wglGetProcAddress('glVertexStream3sv');
    if not Assigned(glVertexStream3sv) then Exit;
    glVertexStream3iv := wglGetProcAddress('glVertexStream3iv');
    if not Assigned(glVertexStream3iv) then Exit;
    glVertexStream3fv := wglGetProcAddress('glVertexStream3fv');
    if not Assigned(glVertexStream3fv) then Exit;
    glVertexStream3dv := wglGetProcAddress('glVertexStream3dv');
    if not Assigned(glVertexStream3dv) then Exit;
    glVertexStream4s := wglGetProcAddress('glVertexStream4s');
    if not Assigned(glVertexStream4s) then Exit;
    glVertexStream4i := wglGetProcAddress('glVertexStream4i');
    if not Assigned(glVertexStream4i) then Exit;
    glVertexStream4f := wglGetProcAddress('glVertexStream4f');
    if not Assigned(glVertexStream4f) then Exit;
    glVertexStream4d := wglGetProcAddress('glVertexStream4d');
    if not Assigned(glVertexStream4d) then Exit;
    glVertexStream4sv := wglGetProcAddress('glVertexStream4sv');
    if not Assigned(glVertexStream4sv) then Exit;
    glVertexStream4iv := wglGetProcAddress('glVertexStream4iv');
    if not Assigned(glVertexStream4iv) then Exit;
    glVertexStream4fv := wglGetProcAddress('glVertexStream4fv');
    if not Assigned(glVertexStream4fv) then Exit;
    glVertexStream4dv := wglGetProcAddress('glVertexStream4dv');
    if not Assigned(glVertexStream4dv) then Exit;
    glNormalStream3b := wglGetProcAddress('glNormalStream3b');
    if not Assigned(glNormalStream3b) then Exit;
    glNormalStream3s := wglGetProcAddress('glNormalStream3s');
    if not Assigned(glNormalStream3s) then Exit;
    glNormalStream3i := wglGetProcAddress('glNormalStream3i');
    if not Assigned(glNormalStream3i) then Exit;
    glNormalStream3f := wglGetProcAddress('glNormalStream3f');
    if not Assigned(glNormalStream3f) then Exit;
    glNormalStream3d := wglGetProcAddress('glNormalStream3d');
    if not Assigned(glNormalStream3d) then Exit;
    glNormalStream3bv := wglGetProcAddress('glNormalStream3bv');
    if not Assigned(glNormalStream3bv) then Exit;
    glNormalStream3sv := wglGetProcAddress('glNormalStream3sv');
    if not Assigned(glNormalStream3sv) then Exit;
    glNormalStream3iv := wglGetProcAddress('glNormalStream3iv');
    if not Assigned(glNormalStream3iv) then Exit;
    glNormalStream3fv := wglGetProcAddress('glNormalStream3fv');
    if not Assigned(glNormalStream3fv) then Exit;
    glNormalStream3dv := wglGetProcAddress('glNormalStream3dv');
    if not Assigned(glNormalStream3dv) then Exit;
    glClientActiveVertexStream := wglGetProcAddress('glClientActiveVertexStream');
    if not Assigned(glClientActiveVertexStream) then Exit;
    glVertexBlendEnvi := wglGetProcAddress('glVertexBlendEnvi');
    if not Assigned(glVertexBlendEnvi) then Exit;
    glVertexBlendEnvf := wglGetProcAddress('glVertexBlendEnvf');
    if not Assigned(glVertexBlendEnvf) then Exit;
    Result := TRUE;
  end;

end;

{$IFDEF Windows}
function Load_WGL_I3D_image_buffer: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_I3D_image_buffer', extstring) then
  begin
    wglCreateImageBufferI3D := wglGetProcAddress('wglCreateImageBufferI3D');
    if not Assigned(wglCreateImageBufferI3D) then Exit;
    wglDestroyImageBufferI3D := wglGetProcAddress('wglDestroyImageBufferI3D');
    if not Assigned(wglDestroyImageBufferI3D) then Exit;
    wglAssociateImageBufferEventsI3D := wglGetProcAddress('wglAssociateImageBufferEventsI3D');
    if not Assigned(wglAssociateImageBufferEventsI3D) then Exit;
    wglReleaseImageBufferEventsI3D := wglGetProcAddress('wglReleaseImageBufferEventsI3D');
    if not Assigned(wglReleaseImageBufferEventsI3D) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_I3D_swap_frame_lock: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_I3D_swap_frame_lock', extstring) then
  begin
    wglEnableFrameLockI3D := wglGetProcAddress('wglEnableFrameLockI3D');
    if not Assigned(wglEnableFrameLockI3D) then Exit;
    wglDisableFrameLockI3D := wglGetProcAddress('wglDisableFrameLockI3D');
    if not Assigned(wglDisableFrameLockI3D) then Exit;
    wglIsEnabledFrameLockI3D := wglGetProcAddress('wglIsEnabledFrameLockI3D');
    if not Assigned(wglIsEnabledFrameLockI3D) then Exit;
    wglQueryFrameLockMasterI3D := wglGetProcAddress('wglQueryFrameLockMasterI3D');
    if not Assigned(wglQueryFrameLockMasterI3D) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_I3D_swap_frame_usage: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_I3D_swap_frame_usage', extstring) then
  begin
    wglGetFrameUsageI3D := wglGetProcAddress('wglGetFrameUsageI3D');
    if not Assigned(wglGetFrameUsageI3D) then Exit;
    wglBeginFrameTrackingI3D := wglGetProcAddress('wglBeginFrameTrackingI3D');
    if not Assigned(wglBeginFrameTrackingI3D) then Exit;
    wglEndFrameTrackingI3D := wglGetProcAddress('wglEndFrameTrackingI3D');
    if not Assigned(wglEndFrameTrackingI3D) then Exit;
    wglQueryFrameTrackingI3D := wglGetProcAddress('wglQueryFrameTrackingI3D');
    if not Assigned(wglQueryFrameTrackingI3D) then Exit;
    Result := TRUE;
  end;

end;
{$ENDIF}

function Load_GL_3DFX_texture_compression_FXT1: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_3DFX_texture_compression_FXT1', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_IBM_cull_vertex: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_cull_vertex', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_IBM_multimode_draw_arrays: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_multimode_draw_arrays', extstring) then
  begin
    glMultiModeDrawArraysIBM := wglGetProcAddress('glMultiModeDrawArraysIBM');
    if not Assigned(glMultiModeDrawArraysIBM) then Exit;
    glMultiModeDrawElementsIBM := wglGetProcAddress('glMultiModeDrawElementsIBM');
    if not Assigned(glMultiModeDrawElementsIBM) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_IBM_raster_pos_clip: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_raster_pos_clip', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_IBM_texture_mirrored_repeat: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_texture_mirrored_repeat', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_IBM_vertex_array_lists: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_IBM_vertex_array_lists', extstring) then
  begin
    glColorPointerListIBM := wglGetProcAddress('glColorPointerListIBM');
    if not Assigned(glColorPointerListIBM) then Exit;
    glSecondaryColorPointerListIBM := wglGetProcAddress('glSecondaryColorPointerListIBM');
    if not Assigned(glSecondaryColorPointerListIBM) then Exit;
    glEdgeFlagPointerListIBM := wglGetProcAddress('glEdgeFlagPointerListIBM');
    if not Assigned(glEdgeFlagPointerListIBM) then Exit;
    glFogCoordPointerListIBM := wglGetProcAddress('glFogCoordPointerListIBM');
    if not Assigned(glFogCoordPointerListIBM) then Exit;
    glNormalPointerListIBM := wglGetProcAddress('glNormalPointerListIBM');
    if not Assigned(glNormalPointerListIBM) then Exit;
    glTexCoordPointerListIBM := wglGetProcAddress('glTexCoordPointerListIBM');
    if not Assigned(glTexCoordPointerListIBM) then Exit;
    glVertexPointerListIBM := wglGetProcAddress('glVertexPointerListIBM');
    if not Assigned(glVertexPointerListIBM) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_MESA_resize_buffers: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESA_resize_buffers', extstring) then
  begin
    glResizeBuffersMESA := wglGetProcAddress('glResizeBuffersMESA');
    if not Assigned(glResizeBuffersMESA) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_MESA_window_pos: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESA_window_pos', extstring) then
  begin
    glWindowPos2dMESA := wglGetProcAddress('glWindowPos2dMESA');
    if not Assigned(glWindowPos2dMESA) then Exit;
    glWindowPos2fMESA := wglGetProcAddress('glWindowPos2fMESA');
    if not Assigned(glWindowPos2fMESA) then Exit;
    glWindowPos2iMESA := wglGetProcAddress('glWindowPos2iMESA');
    if not Assigned(glWindowPos2iMESA) then Exit;
    glWindowPos2sMESA := wglGetProcAddress('glWindowPos2sMESA');
    if not Assigned(glWindowPos2sMESA) then Exit;
    glWindowPos2ivMESA := wglGetProcAddress('glWindowPos2ivMESA');
    if not Assigned(glWindowPos2ivMESA) then Exit;
    glWindowPos2svMESA := wglGetProcAddress('glWindowPos2svMESA');
    if not Assigned(glWindowPos2svMESA) then Exit;
    glWindowPos2fvMESA := wglGetProcAddress('glWindowPos2fvMESA');
    if not Assigned(glWindowPos2fvMESA) then Exit;
    glWindowPos2dvMESA := wglGetProcAddress('glWindowPos2dvMESA');
    if not Assigned(glWindowPos2dvMESA) then Exit;
    glWindowPos3iMESA := wglGetProcAddress('glWindowPos3iMESA');
    if not Assigned(glWindowPos3iMESA) then Exit;
    glWindowPos3sMESA := wglGetProcAddress('glWindowPos3sMESA');
    if not Assigned(glWindowPos3sMESA) then Exit;
    glWindowPos3fMESA := wglGetProcAddress('glWindowPos3fMESA');
    if not Assigned(glWindowPos3fMESA) then Exit;
    glWindowPos3dMESA := wglGetProcAddress('glWindowPos3dMESA');
    if not Assigned(glWindowPos3dMESA) then Exit;
    glWindowPos3ivMESA := wglGetProcAddress('glWindowPos3ivMESA');
    if not Assigned(glWindowPos3ivMESA) then Exit;
    glWindowPos3svMESA := wglGetProcAddress('glWindowPos3svMESA');
    if not Assigned(glWindowPos3svMESA) then Exit;
    glWindowPos3fvMESA := wglGetProcAddress('glWindowPos3fvMESA');
    if not Assigned(glWindowPos3fvMESA) then Exit;
    glWindowPos3dvMESA := wglGetProcAddress('glWindowPos3dvMESA');
    if not Assigned(glWindowPos3dvMESA) then Exit;
    glWindowPos4iMESA := wglGetProcAddress('glWindowPos4iMESA');
    if not Assigned(glWindowPos4iMESA) then Exit;
    glWindowPos4sMESA := wglGetProcAddress('glWindowPos4sMESA');
    if not Assigned(glWindowPos4sMESA) then Exit;
    glWindowPos4fMESA := wglGetProcAddress('glWindowPos4fMESA');
    if not Assigned(glWindowPos4fMESA) then Exit;
    glWindowPos4dMESA := wglGetProcAddress('glWindowPos4dMESA');
    if not Assigned(glWindowPos4dMESA) then Exit;
    glWindowPos4ivMESA := wglGetProcAddress('glWindowPos4ivMESA');
    if not Assigned(glWindowPos4ivMESA) then Exit;
    glWindowPos4svMESA := wglGetProcAddress('glWindowPos4svMESA');
    if not Assigned(glWindowPos4svMESA) then Exit;
    glWindowPos4fvMESA := wglGetProcAddress('glWindowPos4fvMESA');
    if not Assigned(glWindowPos4fvMESA) then Exit;
    glWindowPos4dvMESA := wglGetProcAddress('glWindowPos4dvMESA');
    if not Assigned(glWindowPos4dvMESA) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_OML_interlace: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_OML_interlace', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_OML_resample: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_OML_resample', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_OML_subsample: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_OML_subsample', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGIS_generate_mipmap: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_generate_mipmap', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGIS_multisample: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_multisample', extstring) then
  begin
    glSampleMaskSGIS := wglGetProcAddress('glSampleMaskSGIS');
    if not Assigned(glSampleMaskSGIS) then Exit;
    glSamplePatternSGIS := wglGetProcAddress('glSamplePatternSGIS');
    if not Assigned(glSamplePatternSGIS) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_SGIS_pixel_texture: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_pixel_texture', extstring) then
  begin
    glPixelTexGenParameteriSGIS := wglGetProcAddress('glPixelTexGenParameteriSGIS');
    if not Assigned(glPixelTexGenParameteriSGIS) then Exit;
    glPixelTexGenParameterfSGIS := wglGetProcAddress('glPixelTexGenParameterfSGIS');
    if not Assigned(glPixelTexGenParameterfSGIS) then Exit;
    glGetPixelTexGenParameterivSGIS := wglGetProcAddress('glGetPixelTexGenParameterivSGIS');
    if not Assigned(glGetPixelTexGenParameterivSGIS) then Exit;
    glGetPixelTexGenParameterfvSGIS := wglGetProcAddress('glGetPixelTexGenParameterfvSGIS');
    if not Assigned(glGetPixelTexGenParameterfvSGIS) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_SGIS_texture_border_clamp: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_border_clamp', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGIS_texture_color_mask: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_color_mask', extstring) then
  begin
    glTextureColorMaskSGIS := wglGetProcAddress('glTextureColorMaskSGIS');
    if not Assigned(glTextureColorMaskSGIS) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_SGIS_texture_edge_clamp: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_edge_clamp', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGIS_texture_lod: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_texture_lod', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGIS_depth_texture: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIS_depth_texture', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGIX_fog_offset: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_fog_offset', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGIX_interlace: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_interlace', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGIX_shadow_ambient: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGIX_shadow_ambient', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGI_color_matrix: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGI_color_matrix', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SGI_color_table: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGI_color_table', extstring) then
  begin
    glColorTableSGI := wglGetProcAddress('glColorTableSGI');
    if not Assigned(glColorTableSGI) then Exit;
    glCopyColorTableSGI := wglGetProcAddress('glCopyColorTableSGI');
    if not Assigned(glCopyColorTableSGI) then Exit;
    glColorTableParameterivSGI := wglGetProcAddress('glColorTableParameterivSGI');
    if not Assigned(glColorTableParameterivSGI) then Exit;
    glColorTableParameterfvSGI := wglGetProcAddress('glColorTableParameterfvSGI');
    if not Assigned(glColorTableParameterfvSGI) then Exit;
    glGetColorTableSGI := wglGetProcAddress('glGetColorTableSGI');
    if not Assigned(glGetColorTableSGI) then Exit;
    glGetColorTableParameterivSGI := wglGetProcAddress('glGetColorTableParameterivSGI');
    if not Assigned(glGetColorTableParameterivSGI) then Exit;
    glGetColorTableParameterfvSGI := wglGetProcAddress('glGetColorTableParameterfvSGI');
    if not Assigned(glGetColorTableParameterfvSGI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_SGI_texture_color_table: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SGI_texture_color_table', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_SUN_vertex: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_SUN_vertex', extstring) then
  begin
    glColor4ubVertex2fSUN := wglGetProcAddress('glColor4ubVertex2fSUN');
    if not Assigned(glColor4ubVertex2fSUN) then Exit;
    glColor4ubVertex2fvSUN := wglGetProcAddress('glColor4ubVertex2fvSUN');
    if not Assigned(glColor4ubVertex2fvSUN) then Exit;
    glColor4ubVertex3fSUN := wglGetProcAddress('glColor4ubVertex3fSUN');
    if not Assigned(glColor4ubVertex3fSUN) then Exit;
    glColor4ubVertex3fvSUN := wglGetProcAddress('glColor4ubVertex3fvSUN');
    if not Assigned(glColor4ubVertex3fvSUN) then Exit;
    glColor3fVertex3fSUN := wglGetProcAddress('glColor3fVertex3fSUN');
    if not Assigned(glColor3fVertex3fSUN) then Exit;
    glColor3fVertex3fvSUN := wglGetProcAddress('glColor3fVertex3fvSUN');
    if not Assigned(glColor3fVertex3fvSUN) then Exit;
    glNormal3fVertex3fSUN := wglGetProcAddress('glNormal3fVertex3fSUN');
    if not Assigned(glNormal3fVertex3fSUN) then Exit;
    glNormal3fVertex3fvSUN := wglGetProcAddress('glNormal3fVertex3fvSUN');
    if not Assigned(glNormal3fVertex3fvSUN) then Exit;
    glColor4fNormal3fVertex3fSUN := wglGetProcAddress('glColor4fNormal3fVertex3fSUN');
    if not Assigned(glColor4fNormal3fVertex3fSUN) then Exit;
    glColor4fNormal3fVertex3fvSUN := wglGetProcAddress('glColor4fNormal3fVertex3fvSUN');
    if not Assigned(glColor4fNormal3fVertex3fvSUN) then Exit;
    glTexCoord2fVertex3fSUN := wglGetProcAddress('glTexCoord2fVertex3fSUN');
    if not Assigned(glTexCoord2fVertex3fSUN) then Exit;
    glTexCoord2fVertex3fvSUN := wglGetProcAddress('glTexCoord2fVertex3fvSUN');
    if not Assigned(glTexCoord2fVertex3fvSUN) then Exit;
    glTexCoord4fVertex4fSUN := wglGetProcAddress('glTexCoord4fVertex4fSUN');
    if not Assigned(glTexCoord4fVertex4fSUN) then Exit;
    glTexCoord4fVertex4fvSUN := wglGetProcAddress('glTexCoord4fVertex4fvSUN');
    if not Assigned(glTexCoord4fVertex4fvSUN) then Exit;
    glTexCoord2fColor4ubVertex3fSUN := wglGetProcAddress('glTexCoord2fColor4ubVertex3fSUN');
    if not Assigned(glTexCoord2fColor4ubVertex3fSUN) then Exit;
    glTexCoord2fColor4ubVertex3fvSUN := wglGetProcAddress('glTexCoord2fColor4ubVertex3fvSUN');
    if not Assigned(glTexCoord2fColor4ubVertex3fvSUN) then Exit;
    glTexCoord2fColor3fVertex3fSUN := wglGetProcAddress('glTexCoord2fColor3fVertex3fSUN');
    if not Assigned(glTexCoord2fColor3fVertex3fSUN) then Exit;
    glTexCoord2fColor3fVertex3fvSUN := wglGetProcAddress('glTexCoord2fColor3fVertex3fvSUN');
    if not Assigned(glTexCoord2fColor3fVertex3fvSUN) then Exit;
    glTexCoord2fNormal3fVertex3fSUN := wglGetProcAddress('glTexCoord2fNormal3fVertex3fSUN');
    if not Assigned(glTexCoord2fNormal3fVertex3fSUN) then Exit;
    glTexCoord2fNormal3fVertex3fvSUN := wglGetProcAddress('glTexCoord2fNormal3fVertex3fvSUN');
    if not Assigned(glTexCoord2fNormal3fVertex3fvSUN) then Exit;
    glTexCoord2fColor4fNormal3fVertex3fSUN := wglGetProcAddress('glTexCoord2fColor4fNormal3fVertex3fSUN');
    if not Assigned(glTexCoord2fColor4fNormal3fVertex3fSUN) then Exit;
    glTexCoord2fColor4fNormal3fVertex3fvSUN := wglGetProcAddress('glTexCoord2fColor4fNormal3fVertex3fvSUN');
    if not Assigned(glTexCoord2fColor4fNormal3fVertex3fvSUN) then Exit;
    glTexCoord4fColor4fNormal3fVertex4fSUN := wglGetProcAddress('glTexCoord4fColor4fNormal3fVertex4fSUN');
    if not Assigned(glTexCoord4fColor4fNormal3fVertex4fSUN) then Exit;
    glTexCoord4fColor4fNormal3fVertex4fvSUN := wglGetProcAddress('glTexCoord4fColor4fNormal3fVertex4fvSUN');
    if not Assigned(glTexCoord4fColor4fNormal3fVertex4fvSUN) then Exit;
    glReplacementCodeuiVertex3fSUN := wglGetProcAddress('glReplacementCodeuiVertex3fSUN');
    if not Assigned(glReplacementCodeuiVertex3fSUN) then Exit;
    glReplacementCodeuiVertex3fvSUN := wglGetProcAddress('glReplacementCodeuiVertex3fvSUN');
    if not Assigned(glReplacementCodeuiVertex3fvSUN) then Exit;
    glReplacementCodeuiColor4ubVertex3fSUN := wglGetProcAddress('glReplacementCodeuiColor4ubVertex3fSUN');
    if not Assigned(glReplacementCodeuiColor4ubVertex3fSUN) then Exit;
    glReplacementCodeuiColor4ubVertex3fvSUN := wglGetProcAddress('glReplacementCodeuiColor4ubVertex3fvSUN');
    if not Assigned(glReplacementCodeuiColor4ubVertex3fvSUN) then Exit;
    glReplacementCodeuiColor3fVertex3fSUN := wglGetProcAddress('glReplacementCodeuiColor3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiColor3fVertex3fSUN) then Exit;
    glReplacementCodeuiColor3fVertex3fvSUN := wglGetProcAddress('glReplacementCodeuiColor3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiColor3fVertex3fvSUN) then Exit;
    glReplacementCodeuiNormal3fVertex3fSUN := wglGetProcAddress('glReplacementCodeuiNormal3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiNormal3fVertex3fSUN) then Exit;
    glReplacementCodeuiNormal3fVertex3fvSUN := wglGetProcAddress('glReplacementCodeuiNormal3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiNormal3fVertex3fvSUN) then Exit;
    glReplacementCodeuiColor4fNormal3fVertex3fSUN := wglGetProcAddress('glReplacementCodeuiColor4fNormal3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiColor4fNormal3fVertex3fSUN) then Exit;
    glReplacementCodeuiColor4fNormal3fVertex3fvSUN := wglGetProcAddress('glReplacementCodeuiColor4fNormal3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiColor4fNormal3fVertex3fvSUN) then Exit;
    glReplacementCodeuiTexCoord2fVertex3fSUN := wglGetProcAddress('glReplacementCodeuiTexCoord2fVertex3fSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fVertex3fSUN) then Exit;
    glReplacementCodeuiTexCoord2fVertex3fvSUN := wglGetProcAddress('glReplacementCodeuiTexCoord2fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fVertex3fvSUN) then Exit;
    glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN := wglGetProcAddress('glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN) then Exit;
    glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN := wglGetProcAddress('glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN) then Exit;
    glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN := wglGetProcAddress('glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN) then Exit;
    glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN := wglGetProcAddress('glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN');
    if not Assigned(glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_fragment_program: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_fragment_program', extstring) then
  begin
    glProgramStringARB := wglGetProcAddress('glProgramStringARB');
    if not Assigned(glProgramStringARB) then Exit;
    glBindProgramARB := wglGetProcAddress('glBindProgramARB');
    if not Assigned(glBindProgramARB) then Exit;
    glDeleteProgramsARB := wglGetProcAddress('glDeleteProgramsARB');
    if not Assigned(glDeleteProgramsARB) then Exit;
    glGenProgramsARB := wglGetProcAddress('glGenProgramsARB');
    if not Assigned(glGenProgramsARB) then Exit;
    glProgramEnvParameter4dARB := wglGetProcAddress('glProgramEnvParameter4dARB');
    if not Assigned(glProgramEnvParameter4dARB) then Exit;
    glProgramEnvParameter4dvARB := wglGetProcAddress('glProgramEnvParameter4dvARB');
    if not Assigned(glProgramEnvParameter4dvARB) then Exit;
    glProgramEnvParameter4fARB := wglGetProcAddress('glProgramEnvParameter4fARB');
    if not Assigned(glProgramEnvParameter4fARB) then Exit;
    glProgramEnvParameter4fvARB := wglGetProcAddress('glProgramEnvParameter4fvARB');
    if not Assigned(glProgramEnvParameter4fvARB) then Exit;
    glProgramLocalParameter4dARB := wglGetProcAddress('glProgramLocalParameter4dARB');
    if not Assigned(glProgramLocalParameter4dARB) then Exit;
    glProgramLocalParameter4dvARB := wglGetProcAddress('glProgramLocalParameter4dvARB');
    if not Assigned(glProgramLocalParameter4dvARB) then Exit;
    glProgramLocalParameter4fARB := wglGetProcAddress('glProgramLocalParameter4fARB');
    if not Assigned(glProgramLocalParameter4fARB) then Exit;
    glProgramLocalParameter4fvARB := wglGetProcAddress('glProgramLocalParameter4fvARB');
    if not Assigned(glProgramLocalParameter4fvARB) then Exit;
    glGetProgramEnvParameterdvARB := wglGetProcAddress('glGetProgramEnvParameterdvARB');
    if not Assigned(glGetProgramEnvParameterdvARB) then Exit;
    glGetProgramEnvParameterfvARB := wglGetProcAddress('glGetProgramEnvParameterfvARB');
    if not Assigned(glGetProgramEnvParameterfvARB) then Exit;
    glGetProgramLocalParameterdvARB := wglGetProcAddress('glGetProgramLocalParameterdvARB');
    if not Assigned(glGetProgramLocalParameterdvARB) then Exit;
    glGetProgramLocalParameterfvARB := wglGetProcAddress('glGetProgramLocalParameterfvARB');
    if not Assigned(glGetProgramLocalParameterfvARB) then Exit;
    glGetProgramivARB := wglGetProcAddress('glGetProgramivARB');
    if not Assigned(glGetProgramivARB) then Exit;
    glGetProgramStringARB := wglGetProcAddress('glGetProgramStringARB');
    if not Assigned(glGetProgramStringARB) then Exit;
    glIsProgramARB := wglGetProcAddress('glIsProgramARB');
    if not Assigned(glIsProgramARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_text_fragment_shader: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_text_fragment_shader', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_APPLE_client_storage: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_client_storage', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_APPLE_element_array: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_element_array', extstring) then
  begin
    glElementPointerAPPLE := wglGetProcAddress('glElementPointerAPPLE');
    if not Assigned(glElementPointerAPPLE) then Exit;
    glDrawElementArrayAPPLE := wglGetProcAddress('glDrawElementArrayAPPLE');
    if not Assigned(glDrawElementArrayAPPLE) then Exit;
    glDrawRangeElementArrayAPPLE := wglGetProcAddress('glDrawRangeElementArrayAPPLE');
    if not Assigned(glDrawRangeElementArrayAPPLE) then Exit;
    glMultiDrawElementArrayAPPLE := wglGetProcAddress('glMultiDrawElementArrayAPPLE');
    if not Assigned(glMultiDrawElementArrayAPPLE) then Exit;
    glMultiDrawRangeElementArrayAPPLE := wglGetProcAddress('glMultiDrawRangeElementArrayAPPLE');
    if not Assigned(glMultiDrawRangeElementArrayAPPLE) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_APPLE_fence: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_fence', extstring) then
  begin
    glGenFencesAPPLE := wglGetProcAddress('glGenFencesAPPLE');
    if not Assigned(glGenFencesAPPLE) then Exit;
    glDeleteFencesAPPLE := wglGetProcAddress('glDeleteFencesAPPLE');
    if not Assigned(glDeleteFencesAPPLE) then Exit;
    glSetFenceAPPLE := wglGetProcAddress('glSetFenceAPPLE');
    if not Assigned(glSetFenceAPPLE) then Exit;
    glIsFenceAPPLE := wglGetProcAddress('glIsFenceAPPLE');
    if not Assigned(glIsFenceAPPLE) then Exit;
    glTestFenceAPPLE := wglGetProcAddress('glTestFenceAPPLE');
    if not Assigned(glTestFenceAPPLE) then Exit;
    glFinishFenceAPPLE := wglGetProcAddress('glFinishFenceAPPLE');
    if not Assigned(glFinishFenceAPPLE) then Exit;
    glTestObjectAPPLE := wglGetProcAddress('glTestObjectAPPLE');
    if not Assigned(glTestObjectAPPLE) then Exit;
    glFinishObjectAPPLE := wglGetProcAddress('glFinishObjectAPPLE');
    if not Assigned(glFinishObjectAPPLE) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_APPLE_vertex_array_object: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_vertex_array_object', extstring) then
  begin
    glBindVertexArrayAPPLE := wglGetProcAddress('glBindVertexArrayAPPLE');
    if not Assigned(glBindVertexArrayAPPLE) then Exit;
    glDeleteVertexArraysAPPLE := wglGetProcAddress('glDeleteVertexArraysAPPLE');
    if not Assigned(glDeleteVertexArraysAPPLE) then Exit;
    glGenVertexArraysAPPLE := wglGetProcAddress('glGenVertexArraysAPPLE');
    if not Assigned(glGenVertexArraysAPPLE) then Exit;
    glIsVertexArrayAPPLE := wglGetProcAddress('glIsVertexArrayAPPLE');
    if not Assigned(glIsVertexArrayAPPLE) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_APPLE_vertex_array_range: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_APPLE_vertex_array_range', extstring) then
  begin
    glVertexArrayRangeAPPLE := wglGetProcAddress('glVertexArrayRangeAPPLE');
    if not Assigned(glVertexArrayRangeAPPLE) then Exit;
    glFlushVertexArrayRangeAPPLE := wglGetProcAddress('glFlushVertexArrayRangeAPPLE');
    if not Assigned(glFlushVertexArrayRangeAPPLE) then Exit;
    glVertexArrayParameteriAPPLE := wglGetProcAddress('glVertexArrayParameteriAPPLE');
    if not Assigned(glVertexArrayParameteriAPPLE) then Exit;
    Result := TRUE;
  end;

end;


function load_GL_ARB_vertex_buffer_object : boolean;

var extstring:ansistring;

begin
  Result:=false;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));
  if glext_ExtensionSupported('GL_ARB_vertex_buffer_object',extstring) then
    begin
      glBindBufferARB := wglGetProcAddress('glBindBufferARB');
      if not Assigned(glBindBufferARB) then Exit;
      glDeleteBuffersARB := wglGetProcAddress('glDeleteBuffersARB');
      if not Assigned(glDeleteBuffersARB) then Exit;
      glGenBuffersARB := wglGetProcAddress('glGenBuffersARB');
      if not Assigned(glGenBuffersARB) then Exit;
      glIsBufferARB := wglGetProcAddress('glIsBufferARB');
      if not Assigned(glIsBufferARB) then Exit;
      glBufferDataARB := wglGetProcAddress('glBufferDataARB');
      if not Assigned(glBufferDataARB) then Exit;
      glBufferSubDataARB := wglGetProcAddress('glBufferSubDataARB');
      if not Assigned(glBufferSubDataARB) then Exit;
      glGetBufferSubDataARB := wglGetProcAddress('glGetBufferSubDataARB');
      if not Assigned(glGetBufferSubDataARB) then Exit;
      glMapBufferARB := wglGetProcAddress('glMapBufferARB');
      if not Assigned(glMapBufferARB) then Exit;
      glUnmapBufferARB := wglGetProcAddress('glUnmapBufferARB');
      if not Assigned(glMapBufferARB) then Exit;
      glGetBufferParameterivARB := wglGetProcAddress('glGetBufferParameterivARB');
      if not Assigned(glGetBufferParameterivARB) then Exit;
      glGetBufferPointervARB := wglGetProcAddress('glGetBufferPointervARB');
      if not Assigned(glGetBufferPointervARB) then Exit;
      Result:=true;
    end;
end;

{$IFDEF Windows}
function Load_WGL_ARB_pixel_format: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_ARB_pixel_format', extstring) then
  begin
    wglGetPixelFormatAttribivARB := wglGetProcAddress('wglGetPixelFormatAttribivARB');
    if not Assigned(wglGetPixelFormatAttribivARB) then Exit;
    wglGetPixelFormatAttribfvARB := wglGetProcAddress('wglGetPixelFormatAttribfvARB');
    if not Assigned(wglGetPixelFormatAttribfvARB) then Exit;
    wglChoosePixelFormatARB := wglGetProcAddress('wglChoosePixelFormatARB');
    if not Assigned(wglChoosePixelFormatARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_ARB_make_current_read: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_ARB_make_current_read', extstring) then
  begin
    wglMakeContextCurrentARB := wglGetProcAddress('wglMakeContextCurrentARB');
    if not Assigned(wglMakeContextCurrentARB) then Exit;
    wglGetCurrentReadDCARB := wglGetProcAddress('wglGetCurrentReadDCARB');
    if not Assigned(wglGetCurrentReadDCARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_ARB_pbuffer: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_ARB_pbuffer', extstring) then
  begin
    wglCreatePbufferARB := wglGetProcAddress('wglCreatePbufferARB');
    if not Assigned(wglCreatePbufferARB) then Exit;
    wglGetPbufferDCARB := wglGetProcAddress('wglGetPbufferDCARB');
    if not Assigned(wglGetPbufferDCARB) then Exit;
    wglReleasePbufferDCARB := wglGetProcAddress('wglReleasePbufferDCARB');
    if not Assigned(wglReleasePbufferDCARB) then Exit;
    wglDestroyPbufferARB := wglGetProcAddress('wglDestroyPbufferARB');
    if not Assigned(wglDestroyPbufferARB) then Exit;
    wglQueryPbufferARB := wglGetProcAddress('wglQueryPbufferARB');
    if not Assigned(wglQueryPbufferARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_EXT_swap_control: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_EXT_swap_control', extstring) then
  begin
    wglSwapIntervalEXT := wglGetProcAddress('wglSwapIntervalEXT');
    if not Assigned(wglSwapIntervalEXT) then Exit;
    wglGetSwapIntervalEXT := wglGetProcAddress('wglGetSwapIntervalEXT');
    if not Assigned(wglGetSwapIntervalEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_ARB_render_texture: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_ARB_render_texture', extstring) then
  begin
    wglBindTexImageARB := wglGetProcAddress('wglBindTexImageARB');
    if not Assigned(wglBindTexImageARB) then Exit;
    wglReleaseTexImageARB := wglGetProcAddress('wglReleaseTexImageARB');
    if not Assigned(wglReleaseTexImageARB) then Exit;
    wglSetPbufferAttribARB := wglGetProcAddress('wglSetPbufferAttribARB');
    if not Assigned(wglSetPbufferAttribARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_EXT_extensions_string: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_EXT_extensions_string', extstring) then
  begin
    wglGetExtensionsStringEXT := wglGetProcAddress('wglGetExtensionsStringEXT');
    if not Assigned(wglGetExtensionsStringEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_EXT_make_current_read: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_EXT_make_current_read', extstring) then
  begin
    wglMakeContextCurrentEXT := wglGetProcAddress('wglMakeContextCurrentEXT');
    if not Assigned(wglMakeContextCurrentEXT) then Exit;
    wglGetCurrentReadDCEXT := wglGetProcAddress('wglGetCurrentReadDCEXT');
    if not Assigned(wglGetCurrentReadDCEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_EXT_pbuffer: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_EXT_pbuffer', extstring) then
  begin
    wglCreatePbufferEXT := wglGetProcAddress('wglCreatePbufferEXT');
    if not Assigned(wglCreatePbufferEXT) then Exit;
    wglGetPbufferDCEXT := wglGetProcAddress('wglGetPbufferDCEXT');
    if not Assigned(wglGetPbufferDCEXT) then Exit;
    wglReleasePbufferDCEXT := wglGetProcAddress('wglReleasePbufferDCEXT');
    if not Assigned(wglReleasePbufferDCEXT) then Exit;
    wglDestroyPbufferEXT := wglGetProcAddress('wglDestroyPbufferEXT');
    if not Assigned(wglDestroyPbufferEXT) then Exit;
    wglQueryPbufferEXT := wglGetProcAddress('wglQueryPbufferEXT');
    if not Assigned(wglQueryPbufferEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_EXT_pixel_format: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_EXT_pixel_format', extstring) then
  begin
    wglGetPixelFormatAttribivEXT := wglGetProcAddress('wglGetPixelFormatAttribivEXT');
    if not Assigned(wglGetPixelFormatAttribivEXT) then Exit;
    wglGetPixelFormatAttribfvEXT := wglGetProcAddress('wglGetPixelFormatAttribfvEXT');
    if not Assigned(wglGetPixelFormatAttribfvEXT) then Exit;
    wglChoosePixelFormatEXT := wglGetProcAddress('wglChoosePixelFormatEXT');
    if not Assigned(wglChoosePixelFormatEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_I3D_digital_video_control: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_I3D_digital_video_control', extstring) then
  begin
    wglGetDigitalVideoParametersI3D := wglGetProcAddress('wglGetDigitalVideoParametersI3D');
    if not Assigned(wglGetDigitalVideoParametersI3D) then Exit;
    wglSetDigitalVideoParametersI3D := wglGetProcAddress('wglSetDigitalVideoParametersI3D');
    if not Assigned(wglSetDigitalVideoParametersI3D) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_I3D_gamma: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_I3D_gamma', extstring) then
  begin
    wglGetGammaTableParametersI3D := wglGetProcAddress('wglGetGammaTableParametersI3D');
    if not Assigned(wglGetGammaTableParametersI3D) then Exit;
    wglSetGammaTableParametersI3D := wglGetProcAddress('wglSetGammaTableParametersI3D');
    if not Assigned(wglSetGammaTableParametersI3D) then Exit;
    wglGetGammaTableI3D := wglGetProcAddress('wglGetGammaTableI3D');
    if not Assigned(wglGetGammaTableI3D) then Exit;
    wglSetGammaTableI3D := wglGetProcAddress('wglSetGammaTableI3D');
    if not Assigned(wglSetGammaTableI3D) then Exit;
    Result := TRUE;
  end;

end;

function Load_WGL_I3D_genlock: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := ansistring(pansichar(wglGetExtensionsStringARB(wglGetCurrentDC)));

  if glext_ExtensionSupported('WGL_I3D_genlock', extstring) then
  begin
    wglEnableGenlockI3D := wglGetProcAddress('wglEnableGenlockI3D');
    if not Assigned(wglEnableGenlockI3D) then Exit;
    wglDisableGenlockI3D := wglGetProcAddress('wglDisableGenlockI3D');
    if not Assigned(wglDisableGenlockI3D) then Exit;
    wglIsEnabledGenlockI3D := wglGetProcAddress('wglIsEnabledGenlockI3D');
    if not Assigned(wglIsEnabledGenlockI3D) then Exit;
    wglGenlockSourceI3D := wglGetProcAddress('wglGenlockSourceI3D');
    if not Assigned(wglGenlockSourceI3D) then Exit;
    wglGetGenlockSourceI3D := wglGetProcAddress('wglGetGenlockSourceI3D');
    if not Assigned(wglGetGenlockSourceI3D) then Exit;
    wglGenlockSourceEdgeI3D := wglGetProcAddress('wglGenlockSourceEdgeI3D');
    if not Assigned(wglGenlockSourceEdgeI3D) then Exit;
    wglGetGenlockSourceEdgeI3D := wglGetProcAddress('wglGetGenlockSourceEdgeI3D');
    if not Assigned(wglGetGenlockSourceEdgeI3D) then Exit;
    wglGenlockSampleRateI3D := wglGetProcAddress('wglGenlockSampleRateI3D');
    if not Assigned(wglGenlockSampleRateI3D) then Exit;
    wglGetGenlockSampleRateI3D := wglGetProcAddress('wglGetGenlockSampleRateI3D');
    if not Assigned(wglGetGenlockSampleRateI3D) then Exit;
    wglGenlockSourceDelayI3D := wglGetProcAddress('wglGenlockSourceDelayI3D');
    if not Assigned(wglGenlockSourceDelayI3D) then Exit;
    wglGetGenlockSourceDelayI3D := wglGetProcAddress('wglGetGenlockSourceDelayI3D');
    if not Assigned(wglGetGenlockSourceDelayI3D) then Exit;
    wglQueryGenlockMaxSourceDelayI3D := wglGetProcAddress('wglQueryGenlockMaxSourceDelayI3D');
    if not Assigned(wglQueryGenlockMaxSourceDelayI3D) then Exit;
    Result := TRUE;
  end;

end;
{$ENDIF}

function Load_GL_ARB_matrix_palette: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_matrix_palette', extstring) then
  begin
    glCurrentPaletteMatrixARB := wglGetProcAddress('glCurrentPaletteMatrixARB');
    if not Assigned(glCurrentPaletteMatrixARB) then Exit;
    glMatrixIndexubvARB := wglGetProcAddress('glMatrixIndexubvARB');
    if not Assigned(glMatrixIndexubvARB) then Exit;
    glMatrixIndexusvARB := wglGetProcAddress('glMatrixIndexusvARB');
    if not Assigned(glMatrixIndexusvARB) then Exit;
    glMatrixIndexuivARB := wglGetProcAddress('glMatrixIndexuivARB');
    if not Assigned(glMatrixIndexuivARB) then Exit;
    glMatrixIndexPointerARB := wglGetProcAddress('glMatrixIndexPointerARB');
    if not Assigned(glMatrixIndexPointerARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_element_array: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_element_array', extstring) then
  begin
    glElementPointerNV := wglGetProcAddress('glElementPointerNV');
    if not Assigned(glElementPointerNV) then Exit;
    glDrawElementArrayNV := wglGetProcAddress('glDrawElementArrayNV');
    if not Assigned(glDrawElementArrayNV) then Exit;
    glDrawRangeElementArrayNV := wglGetProcAddress('glDrawRangeElementArrayNV');
    if not Assigned(glDrawRangeElementArrayNV) then Exit;
    glMultiDrawElementArrayNV := wglGetProcAddress('glMultiDrawElementArrayNV');
    if not Assigned(glMultiDrawElementArrayNV) then Exit;
    glMultiDrawRangeElementArrayNV := wglGetProcAddress('glMultiDrawRangeElementArrayNV');
    if not Assigned(glMultiDrawRangeElementArrayNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_float_buffer: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_float_buffer', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_fragment_program: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fragment_program', extstring) then
  begin
    glProgramNamedParameter4fNV := wglGetProcAddress('glProgramNamedParameter4fNV');
    if not Assigned(glProgramNamedParameter4fNV) then Exit;
    glProgramNamedParameter4dNV := wglGetProcAddress('glProgramNamedParameter4dNV');
    if not Assigned(glProgramNamedParameter4dNV) then Exit;
    glGetProgramNamedParameterfvNV := wglGetProcAddress('glGetProgramNamedParameterfvNV');
    if not Assigned(glGetProgramNamedParameterfvNV) then Exit;
    glGetProgramNamedParameterdvNV := wglGetProcAddress('glGetProgramNamedParameterdvNV');
    if not Assigned(glGetProgramNamedParameterdvNV) then Exit;
    glProgramLocalParameter4dARB := wglGetProcAddress('glProgramLocalParameter4dARB');
    if not Assigned(glProgramLocalParameter4dARB) then Exit;
    glProgramLocalParameter4dvARB := wglGetProcAddress('glProgramLocalParameter4dvARB');
    if not Assigned(glProgramLocalParameter4dvARB) then Exit;
    glProgramLocalParameter4fARB := wglGetProcAddress('glProgramLocalParameter4fARB');
    if not Assigned(glProgramLocalParameter4fARB) then Exit;
    glProgramLocalParameter4fvARB := wglGetProcAddress('glProgramLocalParameter4fvARB');
    if not Assigned(glProgramLocalParameter4fvARB) then Exit;
    glGetProgramLocalParameterdvARB := wglGetProcAddress('glGetProgramLocalParameterdvARB');
    if not Assigned(glGetProgramLocalParameterdvARB) then Exit;
    glGetProgramLocalParameterfvARB := wglGetProcAddress('glGetProgramLocalParameterfvARB');
    if not Assigned(glGetProgramLocalParameterfvARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_primitive_restart: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_primitive_restart', extstring) then
  begin
    glPrimitiveRestartNV := wglGetProcAddress('glPrimitiveRestartNV');
    if not Assigned(glPrimitiveRestartNV) then Exit;
    glPrimitiveRestartIndexNV := wglGetProcAddress('glPrimitiveRestartIndexNV');
    if not Assigned(glPrimitiveRestartIndexNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_NV_vertex_program2: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program2', extstring) then
  begin
    Result := TRUE;
  end;

end;

{$IFDEF Windows}
function Load_WGL_NV_render_texture_rectangle: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  @wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := wglGetExtensionsStringARB(wglGetCurrentDC);

  if glext_ExtensionSupported('WGL_NV_render_texture_rectangle', extstring) then
  begin
    Result := TRUE;
  end;

end;
{$ENDIF}

function Load_GL_NV_pixel_data_range: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_pixel_data_range', extstring) then
  begin
    @glPixelDataRangeNV := wglGetProcAddress('glPixelDataRangeNV');
    if not Assigned(glPixelDataRangeNV) then Exit;
    @glFlushPixelDataRangeNV := wglGetProcAddress('glFlushPixelDataRangeNV');
    if not Assigned(glFlushPixelDataRangeNV) then Exit;
    {$IFDEF Windows}
    @wglAllocateMemoryNV := wglGetProcAddress('wglAllocateMemoryNV');
    if not Assigned(wglAllocateMemoryNV) then Exit;
    @wglFreeMemoryNV := wglGetProcAddress('wglFreeMemoryNV');
    if not Assigned(wglFreeMemoryNV) then Exit;
    {$ENDIF}
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_rectangle: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_rectangle', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_S3_s3tc: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_S3_s3tc', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ATI_draw_buffers: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_draw_buffers', extstring) then
  begin
    @glDrawBuffersATI := wglGetProcAddress('glDrawBuffersATI');
    if not Assigned(glDrawBuffersATI) then Exit;
    Result := TRUE;
  end;

end;

{$IFDEF Windows}
function Load_WGL_ATI_pixel_format_float: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  @wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
  if not Assigned(wglGetExtensionsStringARB) then Exit;
  extstring := wglGetExtensionsStringARB(wglGetCurrentDC);

  if glext_ExtensionSupported('WGL_ATI_pixel_format_float', extstring) then
  begin
    Result := TRUE;
  end;

end;
{$ENDIF}

function Load_GL_ATI_texture_env_combine3: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_texture_env_combine3', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ATI_texture_float: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_texture_float', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_texture_expand_normal: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_texture_expand_normal', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_half_float: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_half_float', extstring) then
  begin
    @glVertex2hNV := wglGetProcAddress('glVertex2hNV');
    if not Assigned(glVertex2hNV) then Exit;
    @glVertex2hvNV := wglGetProcAddress('glVertex2hvNV');
    if not Assigned(glVertex2hvNV) then Exit;
    @glVertex3hNV := wglGetProcAddress('glVertex3hNV');
    if not Assigned(glVertex3hNV) then Exit;
    @glVertex3hvNV := wglGetProcAddress('glVertex3hvNV');
    if not Assigned(glVertex3hvNV) then Exit;
    @glVertex4hNV := wglGetProcAddress('glVertex4hNV');
    if not Assigned(glVertex4hNV) then Exit;
    @glVertex4hvNV := wglGetProcAddress('glVertex4hvNV');
    if not Assigned(glVertex4hvNV) then Exit;
    @glNormal3hNV := wglGetProcAddress('glNormal3hNV');
    if not Assigned(glNormal3hNV) then Exit;
    @glNormal3hvNV := wglGetProcAddress('glNormal3hvNV');
    if not Assigned(glNormal3hvNV) then Exit;
    @glColor3hNV := wglGetProcAddress('glColor3hNV');
    if not Assigned(glColor3hNV) then Exit;
    @glColor3hvNV := wglGetProcAddress('glColor3hvNV');
    if not Assigned(glColor3hvNV) then Exit;
    @glColor4hNV := wglGetProcAddress('glColor4hNV');
    if not Assigned(glColor4hNV) then Exit;
    @glColor4hvNV := wglGetProcAddress('glColor4hvNV');
    if not Assigned(glColor4hvNV) then Exit;
    @glTexCoord1hNV := wglGetProcAddress('glTexCoord1hNV');
    if not Assigned(glTexCoord1hNV) then Exit;
    @glTexCoord1hvNV := wglGetProcAddress('glTexCoord1hvNV');
    if not Assigned(glTexCoord1hvNV) then Exit;
    @glTexCoord2hNV := wglGetProcAddress('glTexCoord2hNV');
    if not Assigned(glTexCoord2hNV) then Exit;
    @glTexCoord2hvNV := wglGetProcAddress('glTexCoord2hvNV');
    if not Assigned(glTexCoord2hvNV) then Exit;
    @glTexCoord3hNV := wglGetProcAddress('glTexCoord3hNV');
    if not Assigned(glTexCoord3hNV) then Exit;
    @glTexCoord3hvNV := wglGetProcAddress('glTexCoord3hvNV');
    if not Assigned(glTexCoord3hvNV) then Exit;
    @glTexCoord4hNV := wglGetProcAddress('glTexCoord4hNV');
    if not Assigned(glTexCoord4hNV) then Exit;
    @glTexCoord4hvNV := wglGetProcAddress('glTexCoord4hvNV');
    if not Assigned(glTexCoord4hvNV) then Exit;
    @glMultiTexCoord1hNV := wglGetProcAddress('glMultiTexCoord1hNV');
    if not Assigned(glMultiTexCoord1hNV) then Exit;
    @glMultiTexCoord1hvNV := wglGetProcAddress('glMultiTexCoord1hvNV');
    if not Assigned(glMultiTexCoord1hvNV) then Exit;
    @glMultiTexCoord2hNV := wglGetProcAddress('glMultiTexCoord2hNV');
    if not Assigned(glMultiTexCoord2hNV) then Exit;
    @glMultiTexCoord2hvNV := wglGetProcAddress('glMultiTexCoord2hvNV');
    if not Assigned(glMultiTexCoord2hvNV) then Exit;
    @glMultiTexCoord3hNV := wglGetProcAddress('glMultiTexCoord3hNV');
    if not Assigned(glMultiTexCoord3hNV) then Exit;
    @glMultiTexCoord3hvNV := wglGetProcAddress('glMultiTexCoord3hvNV');
    if not Assigned(glMultiTexCoord3hvNV) then Exit;
    @glMultiTexCoord4hNV := wglGetProcAddress('glMultiTexCoord4hNV');
    if not Assigned(glMultiTexCoord4hNV) then Exit;
    @glMultiTexCoord4hvNV := wglGetProcAddress('glMultiTexCoord4hvNV');
    if not Assigned(glMultiTexCoord4hvNV) then Exit;
    @glFogCoordhNV := wglGetProcAddress('glFogCoordhNV');
    if not Assigned(glFogCoordhNV) then Exit;
    @glFogCoordhvNV := wglGetProcAddress('glFogCoordhvNV');
    if not Assigned(glFogCoordhvNV) then Exit;
    @glSecondaryColor3hNV := wglGetProcAddress('glSecondaryColor3hNV');
    if not Assigned(glSecondaryColor3hNV) then Exit;
    @glSecondaryColor3hvNV := wglGetProcAddress('glSecondaryColor3hvNV');
    if not Assigned(glSecondaryColor3hvNV) then Exit;
    @glVertexWeighthNV := wglGetProcAddress('glVertexWeighthNV');
    if not Assigned(glVertexWeighthNV) then Exit;
    @glVertexWeighthvNV := wglGetProcAddress('glVertexWeighthvNV');
    if not Assigned(glVertexWeighthvNV) then Exit;
    @glVertexAttrib1hNV := wglGetProcAddress('glVertexAttrib1hNV');
    if not Assigned(glVertexAttrib1hNV) then Exit;
    @glVertexAttrib1hvNV := wglGetProcAddress('glVertexAttrib1hvNV');
    if not Assigned(glVertexAttrib1hvNV) then Exit;
    @glVertexAttrib2hNV := wglGetProcAddress('glVertexAttrib2hNV');
    if not Assigned(glVertexAttrib2hNV) then Exit;
    @glVertexAttrib2hvNV := wglGetProcAddress('glVertexAttrib2hvNV');
    if not Assigned(glVertexAttrib2hvNV) then Exit;
    @glVertexAttrib3hNV := wglGetProcAddress('glVertexAttrib3hNV');
    if not Assigned(glVertexAttrib3hNV) then Exit;
    @glVertexAttrib3hvNV := wglGetProcAddress('glVertexAttrib3hvNV');
    if not Assigned(glVertexAttrib3hvNV) then Exit;
    @glVertexAttrib4hNV := wglGetProcAddress('glVertexAttrib4hNV');
    if not Assigned(glVertexAttrib4hNV) then Exit;
    @glVertexAttrib4hvNV := wglGetProcAddress('glVertexAttrib4hvNV');
    if not Assigned(glVertexAttrib4hvNV) then Exit;
    @glVertexAttribs1hvNV := wglGetProcAddress('glVertexAttribs1hvNV');
    if not Assigned(glVertexAttribs1hvNV) then Exit;
    @glVertexAttribs2hvNV := wglGetProcAddress('glVertexAttribs2hvNV');
    if not Assigned(glVertexAttribs2hvNV) then Exit;
    @glVertexAttribs3hvNV := wglGetProcAddress('glVertexAttribs3hvNV');
    if not Assigned(glVertexAttribs3hvNV) then Exit;
    @glVertexAttribs4hvNV := wglGetProcAddress('glVertexAttribs4hvNV');
    if not Assigned(glVertexAttribs4hvNV) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_map_object_buffer: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_map_object_buffer', extstring) then
  begin
    @glMapObjectBufferATI := wglGetProcAddress('glMapObjectBufferATI');
    if not Assigned(glMapObjectBufferATI) then Exit;
    @glUnmapObjectBufferATI := wglGetProcAddress('glUnmapObjectBufferATI');
    if not Assigned(glUnmapObjectBufferATI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_separate_stencil: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_separate_stencil', extstring) then
  begin
    @glStencilOpSeparateATI := wglGetProcAddress('glStencilOpSeparateATI');
    if not Assigned(glStencilOpSeparateATI) then Exit;
    @glStencilFuncSeparateATI := wglGetProcAddress('glStencilFuncSeparateATI');
    if not Assigned(glStencilFuncSeparateATI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ATI_vertex_attrib_array_object: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ATI_vertex_attrib_array_object', extstring) then
  begin
    @glVertexAttribArrayObjectATI := wglGetProcAddress('glVertexAttribArrayObjectATI');
    if not Assigned(glVertexAttribArrayObjectATI) then Exit;
    @glGetVertexAttribArrayObjectfvATI := wglGetProcAddress('glGetVertexAttribArrayObjectfvATI');
    if not Assigned(glGetVertexAttribArrayObjectfvATI) then Exit;
    @glGetVertexAttribArrayObjectivATI := wglGetProcAddress('glGetVertexAttribArrayObjectivATI');
    if not Assigned(glGetVertexAttribArrayObjectivATI) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_occlusion_query: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_occlusion_query', extstring) then
  begin
    @glGenQueriesARB := wglGetProcAddress('glGenQueriesARB');
    if not Assigned(glGenQueriesARB) then Exit;
    @glDeleteQueriesARB := wglGetProcAddress('glDeleteQueriesARB');
    if not Assigned(glDeleteQueriesARB) then Exit;
    @glIsQueryARB := wglGetProcAddress('glIsQueryARB');
    if not Assigned(glIsQueryARB) then Exit;
    @glBeginQueryARB := wglGetProcAddress('glBeginQueryARB');
    if not Assigned(glBeginQueryARB) then Exit;
    @glEndQueryARB := wglGetProcAddress('glEndQueryARB');
    if not Assigned(glEndQueryARB) then Exit;
    @glGetQueryivARB := wglGetProcAddress('glGetQueryivARB');
    if not Assigned(glGetQueryivARB) then Exit;
    @glGetQueryObjectivARB := wglGetProcAddress('glGetQueryObjectivARB');
    if not Assigned(glGetQueryObjectivARB) then Exit;
    @glGetQueryObjectuivARB := wglGetProcAddress('glGetQueryObjectuivARB');
    if not Assigned(glGetQueryObjectuivARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_shader_objects: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shader_objects', extstring) then
  begin
    @glDeleteObjectARB := wglGetProcAddress('glDeleteObjectARB');
    if not Assigned(glDeleteObjectARB) then Exit;
    @glGetHandleARB := wglGetProcAddress('glGetHandleARB');
    if not Assigned(glGetHandleARB) then Exit;
    @glDetachObjectARB := wglGetProcAddress('glDetachObjectARB');
    if not Assigned(glDetachObjectARB) then Exit;
    @glCreateShaderObjectARB := wglGetProcAddress('glCreateShaderObjectARB');
    if not Assigned(glCreateShaderObjectARB) then Exit;
    @glShaderSourceARB := wglGetProcAddress('glShaderSourceARB');
    if not Assigned(glShaderSourceARB) then Exit;
    @glCompileShaderARB := wglGetProcAddress('glCompileShaderARB');
    if not Assigned(glCompileShaderARB) then Exit;
    @glCreateProgramObjectARB := wglGetProcAddress('glCreateProgramObjectARB');
    if not Assigned(glCreateProgramObjectARB) then Exit;
    @glAttachObjectARB := wglGetProcAddress('glAttachObjectARB');
    if not Assigned(glAttachObjectARB) then Exit;
    @glLinkProgramARB := wglGetProcAddress('glLinkProgramARB');
    if not Assigned(glLinkProgramARB) then Exit;
    @glUseProgramObjectARB := wglGetProcAddress('glUseProgramObjectARB');
    if not Assigned(glUseProgramObjectARB) then Exit;
    @glValidateProgramARB := wglGetProcAddress('glValidateProgramARB');
    if not Assigned(glValidateProgramARB) then Exit;
    @glUniform1fARB := wglGetProcAddress('glUniform1fARB');
    if not Assigned(glUniform1fARB) then Exit;
    @glUniform2fARB := wglGetProcAddress('glUniform2fARB');
    if not Assigned(glUniform2fARB) then Exit;
    @glUniform3fARB := wglGetProcAddress('glUniform3fARB');
    if not Assigned(glUniform3fARB) then Exit;
    @glUniform4fARB := wglGetProcAddress('glUniform4fARB');
    if not Assigned(glUniform4fARB) then Exit;
    @glUniform1iARB := wglGetProcAddress('glUniform1iARB');
    if not Assigned(glUniform1iARB) then Exit;
    @glUniform2iARB := wglGetProcAddress('glUniform2iARB');
    if not Assigned(glUniform2iARB) then Exit;
    @glUniform3iARB := wglGetProcAddress('glUniform3iARB');
    if not Assigned(glUniform3iARB) then Exit;
    @glUniform4iARB := wglGetProcAddress('glUniform4iARB');
    if not Assigned(glUniform4iARB) then Exit;
    @glUniform1fvARB := wglGetProcAddress('glUniform1fvARB');
    if not Assigned(glUniform1fvARB) then Exit;
    @glUniform2fvARB := wglGetProcAddress('glUniform2fvARB');
    if not Assigned(glUniform2fvARB) then Exit;
    @glUniform3fvARB := wglGetProcAddress('glUniform3fvARB');
    if not Assigned(glUniform3fvARB) then Exit;
    @glUniform4fvARB := wglGetProcAddress('glUniform4fvARB');
    if not Assigned(glUniform4fvARB) then Exit;
    @glUniform1ivARB := wglGetProcAddress('glUniform1ivARB');
    if not Assigned(glUniform1ivARB) then Exit;
    @glUniform2ivARB := wglGetProcAddress('glUniform2ivARB');
    if not Assigned(glUniform2ivARB) then Exit;
    @glUniform3ivARB := wglGetProcAddress('glUniform3ivARB');
    if not Assigned(glUniform3ivARB) then Exit;
    @glUniform4ivARB := wglGetProcAddress('glUniform4ivARB');
    if not Assigned(glUniform4ivARB) then Exit;
    @glUniformMatrix2fvARB := wglGetProcAddress('glUniformMatrix2fvARB');
    if not Assigned(glUniformMatrix2fvARB) then Exit;
    @glUniformMatrix3fvARB := wglGetProcAddress('glUniformMatrix3fvARB');
    if not Assigned(glUniformMatrix3fvARB) then Exit;
    @glUniformMatrix4fvARB := wglGetProcAddress('glUniformMatrix4fvARB');
    if not Assigned(glUniformMatrix4fvARB) then Exit;
    @glGetObjectParameterfvARB := wglGetProcAddress('glGetObjectParameterfvARB');
    if not Assigned(glGetObjectParameterfvARB) then Exit;
    @glGetObjectParameterivARB := wglGetProcAddress('glGetObjectParameterivARB');
    if not Assigned(glGetObjectParameterivARB) then Exit;
    @glGetInfoLogARB := wglGetProcAddress('glGetInfoLogARB');
    if not Assigned(glGetInfoLogARB) then Exit;
    @glGetAttachedObjectsARB := wglGetProcAddress('glGetAttachedObjectsARB');
    if not Assigned(glGetAttachedObjectsARB) then Exit;
    @glGetUniformLocationARB := wglGetProcAddress('glGetUniformLocationARB');
    if not Assigned(glGetUniformLocationARB) then Exit;
    @glGetActiveUniformARB := wglGetProcAddress('glGetActiveUniformARB');
    if not Assigned(glGetActiveUniformARB) then Exit;
    @glGetUniformfvARB := wglGetProcAddress('glGetUniformfvARB');
    if not Assigned(glGetUniformfvARB) then Exit;
    @glGetUniformivARB := wglGetProcAddress('glGetUniformivARB');
    if not Assigned(glGetUniformivARB) then Exit;
    @glGetShaderSourceARB := wglGetProcAddress('glGetShaderSourceARB');
    if not Assigned(glGetShaderSourceARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_vertex_shader: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_vertex_shader', extstring) then
  begin
    @glVertexAttrib1fARB := wglGetProcAddress('glVertexAttrib1fARB');
    if not Assigned(glVertexAttrib1fARB) then Exit;
    @glVertexAttrib1sARB := wglGetProcAddress('glVertexAttrib1sARB');
    if not Assigned(glVertexAttrib1sARB) then Exit;
    @glVertexAttrib1dARB := wglGetProcAddress('glVertexAttrib1dARB');
    if not Assigned(glVertexAttrib1dARB) then Exit;
    @glVertexAttrib2fARB := wglGetProcAddress('glVertexAttrib2fARB');
    if not Assigned(glVertexAttrib2fARB) then Exit;
    @glVertexAttrib2sARB := wglGetProcAddress('glVertexAttrib2sARB');
    if not Assigned(glVertexAttrib2sARB) then Exit;
    @glVertexAttrib2dARB := wglGetProcAddress('glVertexAttrib2dARB');
    if not Assigned(glVertexAttrib2dARB) then Exit;
    @glVertexAttrib3fARB := wglGetProcAddress('glVertexAttrib3fARB');
    if not Assigned(glVertexAttrib3fARB) then Exit;
    @glVertexAttrib3sARB := wglGetProcAddress('glVertexAttrib3sARB');
    if not Assigned(glVertexAttrib3sARB) then Exit;
    @glVertexAttrib3dARB := wglGetProcAddress('glVertexAttrib3dARB');
    if not Assigned(glVertexAttrib3dARB) then Exit;
    @glVertexAttrib4fARB := wglGetProcAddress('glVertexAttrib4fARB');
    if not Assigned(glVertexAttrib4fARB) then Exit;
    @glVertexAttrib4sARB := wglGetProcAddress('glVertexAttrib4sARB');
    if not Assigned(glVertexAttrib4sARB) then Exit;
    @glVertexAttrib4dARB := wglGetProcAddress('glVertexAttrib4dARB');
    if not Assigned(glVertexAttrib4dARB) then Exit;
    @glVertexAttrib4NubARB := wglGetProcAddress('glVertexAttrib4NubARB');
    if not Assigned(glVertexAttrib4NubARB) then Exit;
    @glVertexAttrib1fvARB := wglGetProcAddress('glVertexAttrib1fvARB');
    if not Assigned(glVertexAttrib1fvARB) then Exit;
    @glVertexAttrib1svARB := wglGetProcAddress('glVertexAttrib1svARB');
    if not Assigned(glVertexAttrib1svARB) then Exit;
    @glVertexAttrib1dvARB := wglGetProcAddress('glVertexAttrib1dvARB');
    if not Assigned(glVertexAttrib1dvARB) then Exit;
    @glVertexAttrib2fvARB := wglGetProcAddress('glVertexAttrib2fvARB');
    if not Assigned(glVertexAttrib2fvARB) then Exit;
    @glVertexAttrib2svARB := wglGetProcAddress('glVertexAttrib2svARB');
    if not Assigned(glVertexAttrib2svARB) then Exit;
    @glVertexAttrib2dvARB := wglGetProcAddress('glVertexAttrib2dvARB');
    if not Assigned(glVertexAttrib2dvARB) then Exit;
    @glVertexAttrib3fvARB := wglGetProcAddress('glVertexAttrib3fvARB');
    if not Assigned(glVertexAttrib3fvARB) then Exit;
    @glVertexAttrib3svARB := wglGetProcAddress('glVertexAttrib3svARB');
    if not Assigned(glVertexAttrib3svARB) then Exit;
    @glVertexAttrib3dvARB := wglGetProcAddress('glVertexAttrib3dvARB');
    if not Assigned(glVertexAttrib3dvARB) then Exit;
    @glVertexAttrib4fvARB := wglGetProcAddress('glVertexAttrib4fvARB');
    if not Assigned(glVertexAttrib4fvARB) then Exit;
    @glVertexAttrib4svARB := wglGetProcAddress('glVertexAttrib4svARB');
    if not Assigned(glVertexAttrib4svARB) then Exit;
    @glVertexAttrib4dvARB := wglGetProcAddress('glVertexAttrib4dvARB');
    if not Assigned(glVertexAttrib4dvARB) then Exit;
    @glVertexAttrib4ivARB := wglGetProcAddress('glVertexAttrib4ivARB');
    if not Assigned(glVertexAttrib4ivARB) then Exit;
    @glVertexAttrib4bvARB := wglGetProcAddress('glVertexAttrib4bvARB');
    if not Assigned(glVertexAttrib4bvARB) then Exit;
    @glVertexAttrib4ubvARB := wglGetProcAddress('glVertexAttrib4ubvARB');
    if not Assigned(glVertexAttrib4ubvARB) then Exit;
    @glVertexAttrib4usvARB := wglGetProcAddress('glVertexAttrib4usvARB');
    if not Assigned(glVertexAttrib4usvARB) then Exit;
    @glVertexAttrib4uivARB := wglGetProcAddress('glVertexAttrib4uivARB');
    if not Assigned(glVertexAttrib4uivARB) then Exit;
    @glVertexAttrib4NbvARB := wglGetProcAddress('glVertexAttrib4NbvARB');
    if not Assigned(glVertexAttrib4NbvARB) then Exit;
    @glVertexAttrib4NsvARB := wglGetProcAddress('glVertexAttrib4NsvARB');
    if not Assigned(glVertexAttrib4NsvARB) then Exit;
    @glVertexAttrib4NivARB := wglGetProcAddress('glVertexAttrib4NivARB');
    if not Assigned(glVertexAttrib4NivARB) then Exit;
    @glVertexAttrib4NubvARB := wglGetProcAddress('glVertexAttrib4NubvARB');
    if not Assigned(glVertexAttrib4NubvARB) then Exit;
    @glVertexAttrib4NusvARB := wglGetProcAddress('glVertexAttrib4NusvARB');
    if not Assigned(glVertexAttrib4NusvARB) then Exit;
    @glVertexAttrib4NuivARB := wglGetProcAddress('glVertexAttrib4NuivARB');
    if not Assigned(glVertexAttrib4NuivARB) then Exit;
    @glVertexAttribPointerARB := wglGetProcAddress('glVertexAttribPointerARB');
    if not Assigned(glVertexAttribPointerARB) then Exit;
    @glEnableVertexAttribArrayARB := wglGetProcAddress('glEnableVertexAttribArrayARB');
    if not Assigned(glEnableVertexAttribArrayARB) then Exit;
    @glDisableVertexAttribArrayARB := wglGetProcAddress('glDisableVertexAttribArrayARB');
    if not Assigned(glDisableVertexAttribArrayARB) then Exit;
    @glBindAttribLocationARB := wglGetProcAddress('glBindAttribLocationARB');
    if not Assigned(glBindAttribLocationARB) then Exit;
    @glGetActiveAttribARB := wglGetProcAddress('glGetActiveAttribARB');
    if not Assigned(glGetActiveAttribARB) then Exit;
    @glGetAttribLocationARB := wglGetProcAddress('glGetAttribLocationARB');
    if not Assigned(glGetAttribLocationARB) then Exit;
    @glGetVertexAttribdvARB := wglGetProcAddress('glGetVertexAttribdvARB');
    if not Assigned(glGetVertexAttribdvARB) then Exit;
    @glGetVertexAttribfvARB := wglGetProcAddress('glGetVertexAttribfvARB');
    if not Assigned(glGetVertexAttribfvARB) then Exit;
    @glGetVertexAttribivARB := wglGetProcAddress('glGetVertexAttribivARB');
    if not Assigned(glGetVertexAttribivARB) then Exit;
    @glGetVertexAttribPointervARB := wglGetProcAddress('glGetVertexAttribPointervARB');
    if not Assigned(glGetVertexAttribPointervARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_fragment_shader: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_fragment_shader', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_shading_language_100: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_shading_language_100', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_non_power_of_two: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_texture_non_power_of_two', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_point_sprite: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_point_sprite', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_depth_bounds_test: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_depth_bounds_test', extstring) then
  begin
    @glDepthBoundsEXT := wglGetProcAddress('glDepthBoundsEXT');
    if not Assigned(glDepthBoundsEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_mirror_clamp: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_texture_mirror_clamp', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_blend_equation_separate: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_blend_equation_separate', extstring) then
  begin
    @glBlendEquationSeparateEXT := wglGetProcAddress('glBlendEquationSeparateEXT');
    if not Assigned(glBlendEquationSeparateEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_MESA_pack_invert: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESA_pack_invert', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_MESA_ycbcr_texture: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_MESA_ycbcr_texture', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_fragment_program_shadow: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_ARB_fragment_program_shadow', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_fragment_program_option: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fragment_program_option', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_pixel_buffer_object: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_EXT_pixel_buffer_object', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_fragment_program2: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_fragment_program2', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_vertex_program2_option: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program2_option', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_NV_vertex_program3: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if glext_ExtensionSupported('GL_NV_vertex_program3', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_draw_buffers: Boolean;
var
  extstring: pansichar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_draw_buffers', extstring) then
  begin
    glDrawBuffersARB := wglGetProcAddress('glDrawBuffersARB');
    if not Assigned(glDrawBuffersARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_rectangle: Boolean;
var
  extstring: pansichar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_texture_rectangle', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_color_buffer_float: Boolean;
var
  extstring: pansichar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_color_buffer_float', extstring) then
  begin
    glClampColorARB := wglGetProcAddress('glClampColorARB');
    if not Assigned(glClampColorARB) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_half_float_pixel: Boolean;
var
  extstring: pansichar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_half_float_pixel', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_texture_float: Boolean;
var
  extstring: pansichar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_texture_float', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_texture_compression_dxt1: Boolean;
var
  extstring: pansichar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_EXT_texture_compression_dxt1', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_ARB_pixel_buffer_object: Boolean;
var
  extstring: pansichar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_ARB_pixel_buffer_object', extstring) then
  begin
    Result := TRUE;
  end;

end;

function Load_GL_EXT_framebuffer_object: Boolean;
var
  extstring: pansichar;
begin

  Result := FALSE;
  extstring := glGetString(GL_EXTENSIONS);

  if glext_ExtensionSupported('GL_EXT_framebuffer_object', extstring) then
  begin
    glIsRenderbufferEXT := wglGetProcAddress('glIsRenderbufferEXT');
    if not Assigned(glIsRenderbufferEXT) then Exit;
    glBindRenderbufferEXT := wglGetProcAddress('glBindRenderbufferEXT');
    if not Assigned(glBindRenderbufferEXT) then Exit;
    glDeleteRenderbuffersEXT := wglGetProcAddress('glDeleteRenderbuffersEXT');
    if not Assigned(glDeleteRenderbuffersEXT) then Exit;
    glGenRenderbuffersEXT := wglGetProcAddress('glGenRenderbuffersEXT');
    if not Assigned(glGenRenderbuffersEXT) then Exit;
    glRenderbufferStorageEXT := wglGetProcAddress('glRenderbufferStorageEXT');
    if not Assigned(glRenderbufferStorageEXT) then Exit;
    glGetRenderbufferParameterivEXT := wglGetProcAddress('glGetRenderbufferParameterivEXT');
    if not Assigned(glGetRenderbufferParameterivEXT) then Exit;
    glIsFramebufferEXT := wglGetProcAddress('glIsFramebufferEXT');
    if not Assigned(glIsFramebufferEXT) then Exit;
    glBindFramebufferEXT := wglGetProcAddress('glBindFramebufferEXT');
    if not Assigned(glBindFramebufferEXT) then Exit;
    glDeleteFramebuffersEXT := wglGetProcAddress('glDeleteFramebuffersEXT');
    if not Assigned(glDeleteFramebuffersEXT) then Exit;
    glGenFramebuffersEXT := wglGetProcAddress('glGenFramebuffersEXT');
    if not Assigned(glGenFramebuffersEXT) then Exit;
    glCheckFramebufferStatusEXT := wglGetProcAddress('glCheckFramebufferStatusEXT');
    if not Assigned(glCheckFramebufferStatusEXT) then Exit;
    glFramebufferTexture1DEXT := wglGetProcAddress('glFramebufferTexture1DEXT');
    if not Assigned(glFramebufferTexture1DEXT) then Exit;
    glFramebufferTexture2DEXT := wglGetProcAddress('glFramebufferTexture2DEXT');
    if not Assigned(glFramebufferTexture2DEXT) then Exit;
    glFramebufferTexture3DEXT := wglGetProcAddress('glFramebufferTexture3DEXT');
    if not Assigned(glFramebufferTexture3DEXT) then Exit;
    glFramebufferRenderbufferEXT := wglGetProcAddress('glFramebufferRenderbufferEXT');
    if not Assigned(glFramebufferRenderbufferEXT) then Exit;
    glGetFramebufferAttachmentParameterivEXT := wglGetProcAddress('glGetFramebufferAttachmentParameterivEXT');
    if not Assigned(glGetFramebufferAttachmentParameterivEXT) then Exit;
    glGenerateMipmapEXT := wglGetProcAddress('glGenerateMipmapEXT');
    if not Assigned(glGenerateMipmapEXT) then Exit;
    Result := TRUE;
  end;

end;

function Load_GL_ARB_framebuffer_object(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
  p: pansichar;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_framebuffer_object', extstring) then
  begin
    glIsRenderbuffer := wglGetProcAddress('glIsRenderbuffer');
    if not Assigned(glIsRenderbuffer) then Exit;
    glBindRenderbuffer := wglGetProcAddress('glBindRenderbuffer');
    if not Assigned(glBindRenderbuffer) then Exit;
    glDeleteRenderbuffers := wglGetProcAddress('glDeleteRenderbuffers');
    if not Assigned(glDeleteRenderbuffers) then Exit;
    glGenRenderbuffers := wglGetProcAddress('glGenRenderbuffers');
    if not Assigned(glGenRenderbuffers) then Exit;
    glRenderbufferStorage := wglGetProcAddress('glRenderbufferStorage');
    if not Assigned(glRenderbufferStorage) then Exit;
    glGetRenderbufferParameteriv := wglGetProcAddress('glGetRenderbufferParameteriv');
    if not Assigned(glGetRenderbufferParameteriv) then Exit;
    glIsFramebuffer := wglGetProcAddress('glIsFramebuffer');
    if not Assigned(glIsFramebuffer) then Exit;
    glBindFramebuffer := wglGetProcAddress('glBindFramebuffer');
    if not Assigned(glBindFramebuffer) then Exit;
    glDeleteFramebuffers := wglGetProcAddress('glDeleteFramebuffers');
    if not Assigned(glDeleteFramebuffers) then Exit;
    glGenFramebuffers := wglGetProcAddress('glGenFramebuffers');
    if not Assigned(glGenFramebuffers) then Exit;
    glCheckFramebufferStatus := wglGetProcAddress('glCheckFramebufferStatus');
    if not Assigned(glCheckFramebufferStatus) then Exit;
    glFramebufferTexture1D := wglGetProcAddress('glFramebufferTexture1D');
    if not Assigned(glFramebufferTexture1D) then Exit;
    glFramebufferTexture2D := wglGetProcAddress('glFramebufferTexture2D');
    if not Assigned(glFramebufferTexture2D) then Exit;
    glFramebufferTexture3D := wglGetProcAddress('glFramebufferTexture3D');
    if not Assigned(glFramebufferTexture3D) then Exit;
    glFramebufferRenderbuffer := wglGetProcAddress('glFramebufferRenderbuffer');
    if not Assigned(glFramebufferRenderbuffer) then Exit;
    glGetFramebufferAttachmentParameteriv := wglGetProcAddress('glGetFramebufferAttachmentParameteriv');
    if not Assigned(glGetFramebufferAttachmentParameteriv) then Exit;
    glGenerateMipmap := wglGetProcAddress('glGenerateMipmap');
    if not Assigned(glGenerateMipmap) then Exit;
    glBlitFramebuffer := wglGetProcAddress('glBlitFramebuffer');
    if not Assigned(glBlitFramebuffer) then Exit;
    glRenderbufferStorageMultisample := wglGetProcAddress('glRenderbufferStorageMultisample');
    if not Assigned(glRenderbufferStorageMultisample) then Exit;
    glFramebufferTextureLayer := wglGetProcAddress('glFramebufferTextureLayer');
    if not Assigned(glFramebufferTextureLayer) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_map_buffer_range(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_map_buffer_range', extstring) then
  begin
    glMapBufferRange := wglGetProcAddress('glMapBufferRange');
    if not Assigned(glMapBufferRange) then Exit;
    glFlushMappedBufferRange := wglGetProcAddress('glFlushMappedBufferRange');
    if not Assigned(glFlushMappedBufferRange) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_array_object(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_vertex_array_object', extstring) then
  begin
    glBindVertexArray := wglGetProcAddress('glBindVertexArray');
    if not Assigned(glBindVertexArray) then Exit;
    glDeleteVertexArrays := wglGetProcAddress('glDeleteVertexArrays');
    if not Assigned(glDeleteVertexArrays) then Exit;
    glGenVertexArrays := wglGetProcAddress('glGenVertexArrays');
    if not Assigned(glGenVertexArrays) then Exit;
    glIsVertexArray := wglGetProcAddress('glIsVertexArray');
    if not Assigned(glIsVertexArray) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_copy_buffer(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_copy_buffer', extstring) then
  begin
    glCopyBufferSubData := wglGetProcAddress('glCopyBufferSubData');
    if not Assigned(glCopyBufferSubData) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_uniform_buffer_object(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_uniform_buffer_object', extstring) then
  begin
    glGetUniformIndices := wglGetProcAddress('glGetUniformIndices');
    if not Assigned(glGetUniformIndices) then Exit;
    glGetActiveUniformsiv := wglGetProcAddress('glGetActiveUniformsiv');
    if not Assigned(glGetActiveUniformsiv) then Exit;
    glGetActiveUniformName := wglGetProcAddress('glGetActiveUniformName');
    if not Assigned(glGetActiveUniformName) then Exit;
    glGetUniformBlockIndex := wglGetProcAddress('glGetUniformBlockIndex');
    if not Assigned(glGetUniformBlockIndex) then Exit;
    glGetActiveUniformBlockiv := wglGetProcAddress('glGetActiveUniformBlockiv');
    if not Assigned(glGetActiveUniformBlockiv) then Exit;
    glGetActiveUniformBlockName := wglGetProcAddress('glGetActiveUniformBlockName');
    if not Assigned(glGetActiveUniformBlockName) then Exit;
    glUniformBlockBinding := wglGetProcAddress('glUniformBlockBinding');
    if not Assigned(glUniformBlockBinding) then Exit;
    (* Shared entry points *)
    glBindBufferRange := wglGetProcAddress('glBindBufferRange');
    if not Assigned(glBindBufferRange) then Exit;
    glBindBufferBase := wglGetProcAddress('glBindBufferBase');
    if not Assigned(glBindBufferBase) then Exit;
    glGetIntegeri_v := wglGetProcAddress('glGetIntegeri_v');
    if not Assigned(glGetIntegeri_v) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_draw_elements_base_vertex(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_draw_elements_base_vertex', extstring) then
  begin
    glDrawElementsBaseVertex := wglGetProcAddress('glDrawElementsBaseVertex');
    if not Assigned(glDrawElementsBaseVertex) then Exit;
    glDrawRangeElementsBaseVertex := wglGetProcAddress('glDrawRangeElementsBaseVertex');
    if not Assigned(glDrawRangeElementsBaseVertex) then Exit;
    glDrawElementsInstancedBaseVertex := wglGetProcAddress('glDrawElementsInstancedBaseVertex');
    if not Assigned(glDrawElementsInstancedBaseVertex) then Exit;
    glMultiDrawElementsBaseVertex := wglGetProcAddress('glMultiDrawElementsBaseVertex');
    if not Assigned(glMultiDrawElementsBaseVertex) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_provoking_vertex(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_provoking_vertex', extstring) then
  begin
    glProvokingVertex := wglGetProcAddress('glProvokingVertex');
    if not Assigned(glProvokingVertex) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_sync(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_sync', extstring) then
  begin
    glFenceSync := wglGetProcAddress('glFenceSync');
    if not Assigned(glFenceSync) then Exit;
    glIsSync := wglGetProcAddress('glIsSync');
    if not Assigned(glIsSync) then Exit;
    glDeleteSync := wglGetProcAddress('glDeleteSync');
    if not Assigned(glDeleteSync) then Exit;
    glClientWaitSync := wglGetProcAddress('glClientWaitSync');
    if not Assigned(glClientWaitSync) then Exit;
    glWaitSync := wglGetProcAddress('glWaitSync');
    if not Assigned(glWaitSync) then Exit;
    glGetInteger64v := wglGetProcAddress('glGetInteger64v');
    if not Assigned(glGetInteger64v) then Exit;
    glGetSynciv := wglGetProcAddress('glGetSynciv');
    if not Assigned(glGetSynciv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_texture_multisample(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_texture_multisample', extstring) then
  begin
    glTexImage2DMultisample := wglGetProcAddress('glTexImage2DMultisample');
    if not Assigned(glTexImage2DMultisample) then Exit;
    glTexImage3DMultisample := wglGetProcAddress('glTexImage3DMultisample');
    if not Assigned(glTexImage3DMultisample) then Exit;
    glGetMultisamplefv := wglGetProcAddress('glGetMultisamplefv');
    if not Assigned(glGetMultisamplefv) then Exit;
    glSampleMaski := wglGetProcAddress('glSampleMaski');
    if not Assigned(glSampleMaski) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_sampler_objects(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_sampler_objects', extstring) then
  begin
    glGenSamplers := wglGetProcAddress('glGenSamplers');
    if not Assigned(glGenSamplers) then Exit;
    glDeleteSamplers := wglGetProcAddress('glDeleteSamplers');
    if not Assigned(glDeleteSamplers) then Exit;
    glIsSampler := wglGetProcAddress('glIsSampler');
    if not Assigned(glIsSampler) then Exit;
    glBindSampler := wglGetProcAddress('glBindSampler');
    if not Assigned(glBindSampler) then Exit;
    glSamplerParameteri := wglGetProcAddress('glSamplerParameteri');
    if not Assigned(glSamplerParameteri) then Exit;
    glSamplerParameteriv := wglGetProcAddress('glSamplerParameteriv');
    if not Assigned(glSamplerParameteriv) then Exit;
    glSamplerParameterf := wglGetProcAddress('glSamplerParameterf');
    if not Assigned(glSamplerParameterf) then Exit;
    glSamplerParameterfv := wglGetProcAddress('glSamplerParameterfv');
    if not Assigned(glSamplerParameterfv) then Exit;
    glSamplerParameterIiv := wglGetProcAddress('glSamplerParameterIiv');
    if not Assigned(glSamplerParameterIiv) then Exit;
    glSamplerParameterIuiv := wglGetProcAddress('glSamplerParameterIuiv');
    if not Assigned(glSamplerParameterIuiv) then Exit;
    glGetSamplerParameteriv := wglGetProcAddress('glGetSamplerParameteriv');
    if not Assigned(glGetSamplerParameteriv) then Exit;
    glGetSamplerParameterIiv := wglGetProcAddress('glGetSamplerParameterIiv');
    if not Assigned(glGetSamplerParameterIiv) then Exit;
    glGetSamplerParameterfv := wglGetProcAddress('glGetSamplerParameterfv');
    if not Assigned(glGetSamplerParameterfv) then Exit;
    glGetSamplerParameterIuiv := wglGetProcAddress('glGetSamplerParameterIuiv');
    if not Assigned(glGetSamplerParameterIuiv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_blend_func_extended(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_blend_func_extended', extstring) then
  begin
    glBindFragDataLocationIndexed := wglGetProcAddress('glBindFragDataLocationIndexed');
    if not Assigned(glBindFragDataLocationIndexed) then Exit;
    glGetFragDataIndex := wglGetProcAddress('glGetFragDataIndex');
    if not Assigned(glGetFragDataIndex) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_timer_query(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_timer_query', extstring) then
  begin
    glQueryCounter := wglGetProcAddress('glQueryCounter');
    if not Assigned(glQueryCounter) then Exit;
    glGetQueryObjecti64v := wglGetProcAddress('glGetQueryObjecti64v');
    if not Assigned(glGetQueryObjecti64v) then Exit;
    glGetQueryObjectui64v := wglGetProcAddress('glGetQueryObjectui64v');
    if not Assigned(glGetQueryObjectui64v) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_vertex_type_2_10_10_10_rev(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_vertex_type_2_10_10_10_rev', extstring) then
  begin
    glVertexP2ui := wglGetProcAddress('glVertexP2ui');
    if not Assigned(glVertexP2ui) then Exit;
    glVertexP2uiv := wglGetProcAddress('glVertexP2uiv');
    if not Assigned(glVertexP2uiv) then Exit;
    glVertexP3ui := wglGetProcAddress('glVertexP3ui');
    if not Assigned(glVertexP3ui) then Exit;
    glVertexP3uiv := wglGetProcAddress('glVertexP3uiv');
    if not Assigned(glVertexP3uiv) then Exit;
    glVertexP4ui := wglGetProcAddress('glVertexP4ui');
    if not Assigned(glVertexP4ui) then Exit;
    glVertexP4uiv := wglGetProcAddress('glVertexP4uiv');
    if not Assigned(glVertexP4uiv) then Exit;
    glTexCoordP1ui := wglGetProcAddress('glTexCoordP1ui');
    if not Assigned(glTexCoordP1ui) then Exit;
    glTexCoordP1uiv := wglGetProcAddress('glTexCoordP1uiv');
    if not Assigned(glTexCoordP1uiv) then Exit;
    glTexCoordP2ui := wglGetProcAddress('glTexCoordP2ui');
    if not Assigned(glTexCoordP2ui) then Exit;
    glTexCoordP2uiv := wglGetProcAddress('glTexCoordP2uiv');
    if not Assigned(glTexCoordP2uiv) then Exit;
    glTexCoordP3ui := wglGetProcAddress('glTexCoordP3ui');
    if not Assigned(glTexCoordP3ui) then Exit;
    glTexCoordP3uiv := wglGetProcAddress('glTexCoordP3uiv');
    if not Assigned(glTexCoordP3uiv) then Exit;
    glTexCoordP4ui := wglGetProcAddress('glTexCoordP4ui');
    if not Assigned(glTexCoordP4ui) then Exit;
    glTexCoordP4uiv := wglGetProcAddress('glTexCoordP4uiv');
    if not Assigned(glTexCoordP4uiv) then Exit;
    glMultiTexCoordP1ui := wglGetProcAddress('glMultiTexCoordP1ui');
    if not Assigned(glMultiTexCoordP1ui) then Exit;
    glMultiTexCoordP1uiv := wglGetProcAddress('glMultiTexCoordP1uiv');
    if not Assigned(glMultiTexCoordP1uiv) then Exit;
    glMultiTexCoordP2ui := wglGetProcAddress('glMultiTexCoordP2ui');
    if not Assigned(glMultiTexCoordP2ui) then Exit;
    glMultiTexCoordP2uiv := wglGetProcAddress('glMultiTexCoordP2uiv');
    if not Assigned(glMultiTexCoordP2uiv) then Exit;
    glMultiTexCoordP3ui := wglGetProcAddress('glMultiTexCoordP3ui');
    if not Assigned(glMultiTexCoordP3ui) then Exit;
    glMultiTexCoordP3uiv := wglGetProcAddress('glMultiTexCoordP3uiv');
    if not Assigned(glMultiTexCoordP3uiv) then Exit;
    glMultiTexCoordP4ui := wglGetProcAddress('glMultiTexCoordP4ui');
    if not Assigned(glMultiTexCoordP4ui) then Exit;
    glMultiTexCoordP4uiv := wglGetProcAddress('glMultiTexCoordP4uiv');
    if not Assigned(glMultiTexCoordP4uiv) then Exit;
    glNormalP3ui := wglGetProcAddress('glNormalP3ui');
    if not Assigned(glNormalP3ui) then Exit;
    glNormalP3uiv := wglGetProcAddress('glNormalP3uiv');
    if not Assigned(glNormalP3uiv) then Exit;
    glColorP3ui := wglGetProcAddress('glColorP3ui');
    if not Assigned(glColorP3ui) then Exit;
    glColorP3uiv := wglGetProcAddress('glColorP3uiv');
    if not Assigned(glColorP3uiv) then Exit;
    glColorP4ui := wglGetProcAddress('glColorP4ui');
    if not Assigned(glColorP4ui) then Exit;
    glColorP4uiv := wglGetProcAddress('glColorP4uiv');
    if not Assigned(glColorP4uiv) then Exit;
    glSecondaryColorP3ui := wglGetProcAddress('glSecondaryColorP3ui');
    if not Assigned(glSecondaryColorP3ui) then Exit;
    glSecondaryColorP3uiv := wglGetProcAddress('glSecondaryColorP3uiv');
    if not Assigned(glSecondaryColorP3uiv) then Exit;
    glVertexAttribP1ui := wglGetProcAddress('glVertexAttribP1ui');
    if not Assigned(glVertexAttribP1ui) then Exit;
    glVertexAttribP1uiv := wglGetProcAddress('glVertexAttribP1uiv');
    if not Assigned(glVertexAttribP1uiv) then Exit;
    glVertexAttribP2ui := wglGetProcAddress('glVertexAttribP2ui');
    if not Assigned(glVertexAttribP2ui) then Exit;
    glVertexAttribP2uiv := wglGetProcAddress('glVertexAttribP2uiv');
    if not Assigned(glVertexAttribP2uiv) then Exit;
    glVertexAttribP3ui := wglGetProcAddress('glVertexAttribP3ui');
    if not Assigned(glVertexAttribP3ui) then Exit;
    glVertexAttribP3uiv := wglGetProcAddress('glVertexAttribP3uiv');
    if not Assigned(glVertexAttribP3uiv) then Exit;
    glVertexAttribP4ui := wglGetProcAddress('glVertexAttribP4ui');
    if not Assigned(glVertexAttribP4ui) then Exit;
    glVertexAttribP4uiv := wglGetProcAddress('glVertexAttribP4uiv');
    if not Assigned(glVertexAttribP4uiv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_gpu_shader_fp64(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_gpu_shader_fp64', extstring) then
  begin
    glUniform1d := wglGetProcAddress('glUniform1d');
    if not Assigned(glUniform1d) then Exit;
    glUniform2d := wglGetProcAddress('glUniform2d');
    if not Assigned(glUniform2d) then Exit;
    glUniform3d := wglGetProcAddress('glUniform3d');
    if not Assigned(glUniform3d) then Exit;
    glUniform4d := wglGetProcAddress('glUniform4d');
    if not Assigned(glUniform4d) then Exit;
    glUniform1dv := wglGetProcAddress('glUniform1dv');
    if not Assigned(glUniform1dv) then Exit;
    glUniform2dv := wglGetProcAddress('glUniform2dv');
    if not Assigned(glUniform2dv) then Exit;
    glUniform3dv := wglGetProcAddress('glUniform3dv');
    if not Assigned(glUniform3dv) then Exit;
    glUniform4dv := wglGetProcAddress('glUniform4dv');
    if not Assigned(glUniform4dv) then Exit;
    glUniformMatrix2dv := wglGetProcAddress('glUniformMatrix2dv');
    if not Assigned(glUniformMatrix2dv) then Exit;
    glUniformMatrix3dv := wglGetProcAddress('glUniformMatrix3dv');
    if not Assigned(glUniformMatrix3dv) then Exit;
    glUniformMatrix4dv := wglGetProcAddress('glUniformMatrix4dv');
    if not Assigned(glUniformMatrix4dv) then Exit;
    glUniformMatrix2x3dv := wglGetProcAddress('glUniformMatrix2x3dv');
    if not Assigned(glUniformMatrix2x3dv) then Exit;
    glUniformMatrix2x4dv := wglGetProcAddress('glUniformMatrix2x4dv');
    if not Assigned(glUniformMatrix2x4dv) then Exit;
    glUniformMatrix3x2dv := wglGetProcAddress('glUniformMatrix3x2dv');
    if not Assigned(glUniformMatrix3x2dv) then Exit;
    glUniformMatrix3x4dv := wglGetProcAddress('glUniformMatrix3x4dv');
    if not Assigned(glUniformMatrix3x4dv) then Exit;
    glUniformMatrix4x2dv := wglGetProcAddress('glUniformMatrix4x2dv');
    if not Assigned(glUniformMatrix4x2dv) then Exit;
    glUniformMatrix4x3dv := wglGetProcAddress('glUniformMatrix4x3dv');
    if not Assigned(glUniformMatrix4x3dv) then Exit;
    glGetUniformdv := wglGetProcAddress('glGetUniformdv');
    if not Assigned(glGetUniformdv) then Exit;
    
    { Ignore presence/absence of functions below.
      See their special definition in 
      http://www.opengl.org/registry/specs/ARB/gpu_shader_fp64.txt:
      "All of the following ProgramUniform* functions are supported if and only
      if EXT_direct_state_access is supported." }
    
    glProgramUniform1dEXT := wglGetProcAddress('glProgramUniform1dEXT');
//    if not Assigned(glProgramUniform1dEXT) then Exit;
    glProgramUniform2dEXT := wglGetProcAddress('glProgramUniform2dEXT');
//    if not Assigned(glProgramUniform2dEXT) then Exit;
    glProgramUniform3dEXT := wglGetProcAddress('glProgramUniform3dEXT');
//    if not Assigned(glProgramUniform3dEXT) then Exit;
    glProgramUniform4dEXT := wglGetProcAddress('glProgramUniform4dEXT');
//    if not Assigned(glProgramUniform4dEXT) then Exit;
    glProgramUniform1dvEXT := wglGetProcAddress('glProgramUniform1dvEXT');
//    if not Assigned(glProgramUniform1dvEXT) then Exit;
    glProgramUniform2dvEXT := wglGetProcAddress('glProgramUniform2dvEXT');
//    if not Assigned(glProgramUniform2dvEXT) then Exit;
    glProgramUniform3dvEXT := wglGetProcAddress('glProgramUniform3dvEXT');
//    if not Assigned(glProgramUniform3dvEXT) then Exit;
    glProgramUniform4dvEXT := wglGetProcAddress('glProgramUniform4dvEXT');
//    if not Assigned(glProgramUniform4dvEXT) then Exit;
    glProgramUniformMatrix2dvEXT := wglGetProcAddress('glProgramUniformMatrix2dvEXT');
//    if not Assigned(glProgramUniformMatrix2dvEXT) then Exit;
    glProgramUniformMatrix3dvEXT := wglGetProcAddress('glProgramUniformMatrix3dvEXT');
//    if not Assigned(glProgramUniformMatrix3dvEXT) then Exit;
    glProgramUniformMatrix4dvEXT := wglGetProcAddress('glProgramUniformMatrix4dvEXT');
//    if not Assigned(glProgramUniformMatrix4dvEXT) then Exit;
    glProgramUniformMatrix2x3dvEXT := wglGetProcAddress('glProgramUniformMatrix2x3dvEXT');
//    if not Assigned(glProgramUniformMatrix2x3dvEXT) then Exit;
    glProgramUniformMatrix2x4dvEXT := wglGetProcAddress('glProgramUniformMatrix2x4dvEXT');
//    if not Assigned(glProgramUniformMatrix2x4dvEXT) then Exit;
    glProgramUniformMatrix3x2dvEXT := wglGetProcAddress('glProgramUniformMatrix3x2dvEXT');
//    if not Assigned(glProgramUniformMatrix3x2dvEXT) then Exit;
    glProgramUniformMatrix3x4dvEXT := wglGetProcAddress('glProgramUniformMatrix3x4dvEXT');
//    if not Assigned(glProgramUniformMatrix3x4dvEXT) then Exit;
    glProgramUniformMatrix4x2dvEXT := wglGetProcAddress('glProgramUniformMatrix4x2dvEXT');
//    if not Assigned(glProgramUniformMatrix4x2dvEXT) then Exit;
    glProgramUniformMatrix4x3dvEXT := wglGetProcAddress('glProgramUniformMatrix4x3dvEXT');
//    if not Assigned(glProgramUniformMatrix4x3dvEXT) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_shader_subroutine(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_shader_subroutine', extstring) then
  begin
    glGetSubroutineUniformLocation := wglGetProcAddress('glGetSubroutineUniformLocation');
    if not Assigned(glGetSubroutineUniformLocation) then Exit;
    glGetSubroutineIndex := wglGetProcAddress('glGetSubroutineIndex');
    if not Assigned(glGetSubroutineIndex) then Exit;
    glGetActiveSubroutineUniformiv := wglGetProcAddress('glGetActiveSubroutineUniformiv');
    if not Assigned(glGetActiveSubroutineUniformiv) then Exit;
    glGetActiveSubroutineUniformName := wglGetProcAddress('glGetActiveSubroutineUniformName');
    if not Assigned(glGetActiveSubroutineUniformName) then Exit;
    glGetActiveSubroutineName := wglGetProcAddress('glGetActiveSubroutineName');
    if not Assigned(glGetActiveSubroutineName) then Exit;
    glUniformSubroutinesuiv := wglGetProcAddress('glUniformSubroutinesuiv');
    if not Assigned(glUniformSubroutinesuiv) then Exit;
    glGetUniformSubroutineuiv := wglGetProcAddress('glGetUniformSubroutineuiv');
    if not Assigned(glGetUniformSubroutineuiv) then Exit;
    glGetProgramStageiv := wglGetProcAddress('glGetProgramStageiv');
    if not Assigned(glGetProgramStageiv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_tessellation_shader(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_tessellation_shader', extstring) then
  begin
    glPatchParameteri := wglGetProcAddress('glPatchParameteri');
    if not Assigned(glPatchParameteri) then Exit;
    glPatchParameterfv := wglGetProcAddress('glPatchParameterfv');
    if not Assigned(glPatchParameterfv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_transform_feedback2(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_transform_feedback2', extstring) then
  begin
    glBindTransformFeedback := wglGetProcAddress('glBindTransformFeedback');
    if not Assigned(glBindTransformFeedback) then Exit;
    glDeleteTransformFeedbacks := wglGetProcAddress('glDeleteTransformFeedbacks');
    if not Assigned(glDeleteTransformFeedbacks) then Exit;
    glGenTransformFeedbacks := wglGetProcAddress('glGenTransformFeedbacks');
    if not Assigned(glGenTransformFeedbacks) then Exit;
    glIsTransformFeedback := wglGetProcAddress('glIsTransformFeedback');
    if not Assigned(glIsTransformFeedback) then Exit;
    glPauseTransformFeedback := wglGetProcAddress('glPauseTransformFeedback');
    if not Assigned(glPauseTransformFeedback) then Exit;
    glResumeTransformFeedback := wglGetProcAddress('glResumeTransformFeedback');
    if not Assigned(glResumeTransformFeedback) then Exit;
    glDrawTransformFeedback := wglGetProcAddress('glDrawTransformFeedback');
    if not Assigned(glDrawTransformFeedback) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_transform_feedback3(LoadAsCore: boolean): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_transform_feedback3', extstring) then
  begin
    glDrawTransformFeedbackStream := wglGetProcAddress('glDrawTransformFeedbackStream');
    if not Assigned(glDrawTransformFeedbackStream) then Exit;
    glBeginQueryIndexed := wglGetProcAddress('glBeginQueryIndexed');
    if not Assigned(glBeginQueryIndexed) then Exit;
    glEndQueryIndexed := wglGetProcAddress('glEndQueryIndexed');
    if not Assigned(glEndQueryIndexed) then Exit;
    glGetQueryIndexediv := wglGetProcAddress('glGetQueryIndexediv');
    if not Assigned(glGetQueryIndexediv) then Exit;
    Result := True;
  end;
end;

function Load_GL_ARB_get_program_binary(LoadAsCore: boolean = false): Boolean;
var
  extstring: ansistring;
begin
  Result := False;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

  if LoadAsCore or glext_ExtensionSupported('GL_ARB_get_program_binary', extstring) then
  begin
    glGetProgramBinary := wglGetProcAddress('glGetProgramBinary');
    if not Assigned(glGetProgramBinary) then Exit;
    glProgramBinary := wglGetProcAddress('glProgramBinary');
    if not Assigned(glProgramBinary) then Exit;
    glProgramParameteri := wglGetProcAddress('glProgramParameteri');
    if not Assigned(glProgramParameteri) then Exit;
    Result := True;
  end;
end;

function Load_GL_version_1_4: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

    glBlendFuncSeparate := wglGetProcAddress('glBlendFuncSeparate');
    if not Assigned(glBlendFuncSeparate) then Exit;
    glFogCoordf := wglGetProcAddress('glFogCoordf');
    if not Assigned(glFogCoordf) then Exit;
    glFogCoordfv := wglGetProcAddress('glFogCoordfv');
    if not Assigned(glFogCoordfv) then Exit;
    glFogCoordd := wglGetProcAddress('glFogCoordd');
    if not Assigned(glFogCoordd) then Exit;
    glFogCoorddv := wglGetProcAddress('glFogCoorddv');
    if not Assigned(glFogCoorddv) then Exit;
    glFogCoordPointer := wglGetProcAddress('glFogCoordPointer');
    if not Assigned(glFogCoordPointer) then Exit;
    glMultiDrawArrays := wglGetProcAddress('glMultiDrawArrays');
    if not Assigned(glMultiDrawArrays) then Exit;
    glMultiDrawElements := wglGetProcAddress('glMultiDrawElements');
    if not Assigned(glMultiDrawElements) then Exit;
    glPointParameterf := wglGetProcAddress('glPointParameterf');
    if not Assigned(glPointParameterf) then Exit;
    glPointParameterfv := wglGetProcAddress('glPointParameterfv');
    if not Assigned(glPointParameterfv) then Exit;
    glPointParameteri := wglGetProcAddress('glPointParameteri');
    if not Assigned(glPointParameteri) then Exit;
    glPointParameteriv := wglGetProcAddress('glPointParameteriv');
    if not Assigned(glPointParameteriv) then Exit;
    glSecondaryColor3b := wglGetProcAddress('glSecondaryColor3b');
    if not Assigned(glSecondaryColor3b) then Exit;
    glSecondaryColor3bv := wglGetProcAddress('glSecondaryColor3bv');
    if not Assigned(glSecondaryColor3bv) then Exit;
    glSecondaryColor3d := wglGetProcAddress('glSecondaryColor3d');
    if not Assigned(glSecondaryColor3d) then Exit;
    glSecondaryColor3dv := wglGetProcAddress('glSecondaryColor3dv');
    if not Assigned(glSecondaryColor3dv) then Exit;
    glSecondaryColor3f := wglGetProcAddress('glSecondaryColor3f');
    if not Assigned(glSecondaryColor3f) then Exit;
    glSecondaryColor3fv := wglGetProcAddress('glSecondaryColor3fv');
    if not Assigned(glSecondaryColor3fv) then Exit;
    glSecondaryColor3i := wglGetProcAddress('glSecondaryColor3i');
    if not Assigned(glSecondaryColor3i) then Exit;
    glSecondaryColor3iv := wglGetProcAddress('glSecondaryColor3iv');
    if not Assigned(glSecondaryColor3iv) then Exit;
    glSecondaryColor3s := wglGetProcAddress('glSecondaryColor3s');
    if not Assigned(glSecondaryColor3s) then Exit;
    glSecondaryColor3sv := wglGetProcAddress('glSecondaryColor3sv');
    if not Assigned(glSecondaryColor3sv) then Exit;
    glSecondaryColor3ub := wglGetProcAddress('glSecondaryColor3ub');
    if not Assigned(glSecondaryColor3ub) then Exit;
    glSecondaryColor3ubv := wglGetProcAddress('glSecondaryColor3ubv');
    if not Assigned(glSecondaryColor3ubv) then Exit;
    glSecondaryColor3ui := wglGetProcAddress('glSecondaryColor3ui');
    if not Assigned(glSecondaryColor3ui) then Exit;
    glSecondaryColor3uiv := wglGetProcAddress('glSecondaryColor3uiv');
    if not Assigned(glSecondaryColor3uiv) then Exit;
    glSecondaryColor3us := wglGetProcAddress('glSecondaryColor3us');
    if not Assigned(glSecondaryColor3us) then Exit;
    glSecondaryColor3usv := wglGetProcAddress('glSecondaryColor3usv');
    if not Assigned(glSecondaryColor3usv) then Exit;
    glSecondaryColorPointer := wglGetProcAddress('glSecondaryColorPointer');
    if not Assigned(glSecondaryColorPointer) then Exit;
    glWindowPos2d := wglGetProcAddress('glWindowPos2d');
    if not Assigned(glWindowPos2d) then Exit;
    glWindowPos2dv := wglGetProcAddress('glWindowPos2dv');
    if not Assigned(glWindowPos2dv) then Exit;
    glWindowPos2f := wglGetProcAddress('glWindowPos2f');
    if not Assigned(glWindowPos2f) then Exit;
    glWindowPos2fv := wglGetProcAddress('glWindowPos2fv');
    if not Assigned(glWindowPos2fv) then Exit;
    glWindowPos2i := wglGetProcAddress('glWindowPos2i');
    if not Assigned(glWindowPos2i) then Exit;
    glWindowPos2iv := wglGetProcAddress('glWindowPos2iv');
    if not Assigned(glWindowPos2iv) then Exit;
    glWindowPos2s := wglGetProcAddress('glWindowPos2s');
    if not Assigned(glWindowPos2s) then Exit;
    glWindowPos2sv := wglGetProcAddress('glWindowPos2sv');
    if not Assigned(glWindowPos2sv) then Exit;
    glWindowPos3d := wglGetProcAddress('glWindowPos3d');
    if not Assigned(glWindowPos3d) then Exit;
    glWindowPos3dv := wglGetProcAddress('glWindowPos3dv');
    if not Assigned(glWindowPos3dv) then Exit;
    glWindowPos3f := wglGetProcAddress('glWindowPos3f');
    if not Assigned(glWindowPos3f) then Exit;
    glWindowPos3fv := wglGetProcAddress('glWindowPos3fv');
    if not Assigned(glWindowPos3fv) then Exit;
    glWindowPos3i := wglGetProcAddress('glWindowPos3i');
    if not Assigned(glWindowPos3i) then Exit;
    glWindowPos3iv := wglGetProcAddress('glWindowPos3iv');
    if not Assigned(glWindowPos3iv) then Exit;
    glWindowPos3s := wglGetProcAddress('glWindowPos3s');
    if not Assigned(glWindowPos3s) then Exit;
    glWindowPos3sv := wglGetProcAddress('glWindowPos3sv');
    if not Assigned(glWindowPos3sv) then Exit;
    Result := Load_GL_version_1_3;

end;

function Load_GL_version_1_5: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

    glGenQueries := wglGetProcAddress('glGenQueries');
    if not Assigned(glGenQueries) then Exit;
    glDeleteQueries := wglGetProcAddress('glDeleteQueries');
    if not Assigned(glDeleteQueries) then Exit;
    glIsQuery := wglGetProcAddress('glIsQuery');
    if not Assigned(glIsQuery) then Exit;
    glBeginQuery := wglGetProcAddress('glBeginQuery');
    if not Assigned(glBeginQuery) then Exit;
    glEndQuery := wglGetProcAddress('glEndQuery');
    if not Assigned(glEndQuery) then Exit;
    glGetQueryiv := wglGetProcAddress('glGetQueryiv');
    if not Assigned(glGetQueryiv) then Exit;
    glGetQueryObjectiv := wglGetProcAddress('glGetQueryObjectiv');
    if not Assigned(glGetQueryObjectiv) then Exit;
    glGetQueryObjectuiv := wglGetProcAddress('glGetQueryObjectuiv');
    if not Assigned(glGetQueryObjectuiv) then Exit;
    glBindBuffer := wglGetProcAddress('glBindBuffer');
    if not Assigned(glBindBuffer) then Exit;
    glDeleteBuffers := wglGetProcAddress('glDeleteBuffers');
    if not Assigned(glDeleteBuffers) then Exit;
    glGenBuffers := wglGetProcAddress('glGenBuffers');
    if not Assigned(glGenBuffers) then Exit;
    glIsBuffer := wglGetProcAddress('glIsBuffer');
    if not Assigned(glIsBuffer) then Exit;
    glBufferData := wglGetProcAddress('glBufferData');
    if not Assigned(glBufferData) then Exit;
    glBufferSubData := wglGetProcAddress('glBufferSubData');
    if not Assigned(glBufferSubData) then Exit;
    glGetBufferSubData := wglGetProcAddress('glGetBufferSubData');
    if not Assigned(glGetBufferSubData) then Exit;
    glMapBuffer := wglGetProcAddress('glMapBuffer');
    if not Assigned(glMapBuffer) then Exit;
    glUnmapBuffer := wglGetProcAddress('glUnmapBuffer');
    if not Assigned(glUnmapBuffer) then Exit;
    glGetBufferParameteriv := wglGetProcAddress('glGetBufferParameteriv');
    if not Assigned(glGetBufferParameteriv) then Exit;
    glGetBufferPointerv := wglGetProcAddress('glGetBufferPointerv');
    if not Assigned(glGetBufferPointerv) then Exit;
    Result := Load_GL_version_1_4;

end;

function Load_GL_version_2_0: Boolean;
var
  extstring: ansistring;
begin

  Result := FALSE;
  extstring := ansistring(pansichar(glGetString(GL_EXTENSIONS)));

    glBlendEquationSeparate := wglGetProcAddress('glBlendEquationSeparate');
    if not Assigned(glBlendEquationSeparate) then Exit;
    glDrawBuffers := wglGetProcAddress('glDrawBuffers');
    if not Assigned(glDrawBuffers) then Exit;
    glStencilOpSeparate := wglGetProcAddress('glStencilOpSeparate');
    if not Assigned(glStencilOpSeparate) then Exit;
    glStencilFuncSeparate := wglGetProcAddress('glStencilFuncSeparate');
    if not Assigned(glStencilFuncSeparate) then Exit;
    glStencilMaskSeparate := wglGetProcAddress('glStencilMaskSeparate');
    if not Assigned(glStencilMaskSeparate) then Exit;
    glAttachShader := wglGetProcAddress('glAttachShader');
    if not Assigned(glAttachShader) then Exit;
    glBindAttribLocation := wglGetProcAddress('glBindAttribLocation');
    if not Assigned(glBindAttribLocation) then Exit;
    glCompileShader := wglGetProcAddress('glCompileShader');
    if not Assigned(glCompileShader) then Exit;
    glCreateProgram := wglGetProcAddress('glCreateProgram');
    if not Assigned(glCreateProgram) then Exit;
    glCreateShader := wglGetProcAddress('glCreateShader');
    if not Assigned(glCreateShader) then Exit;
    glDeleteProgram := wglGetProcAddress('glDeleteProgram');
    if not Assigned(glDeleteProgram) then Exit;
    glDeleteShader := wglGetProcAddress('glDeleteShader');
    if not Assigned(glDeleteShader) then Exit;
    glDetachShader := wglGetProcAddress('glDetachShader');
    if not Assigned(glDetachShader) then Exit;
    glDisableVertexAttribArray := wglGetProcAddress('glDisableVertexAttribArray');
    if not Assigned(glDisableVertexAttribArray) then Exit;
    glEnableVertexAttribArray := wglGetProcAddress('glEnableVertexAttribArray');
    if not Assigned(glEnableVertexAttribArray) then Exit;
    glGetActiveAttrib := wglGetProcAddress('glGetActiveAttrib');
    if not Assigned(glGetActiveAttrib) then Exit;
    glGetActiveUniform := wglGetProcAddress('glGetActiveUniform');
    if not Assigned(glGetActiveUniform) then Exit;
    glGetAttachedShaders := wglGetProcAddress('glGetAttachedShaders');
    if not Assigned(glGetAttachedShaders) then Exit;
    glGetAttribLocation := wglGetProcAddress('glGetAttribLocation');
    if not Assigned(glGetAttribLocation) then Exit;
    glGetProgramiv := wglGetProcAddress('glGetProgramiv');
    if not Assigned(glGetProgramiv) then Exit;
    glGetProgramInfoLog := wglGetProcAddress('glGetProgramInfoLog');
    if not Assigned(glGetProgramInfoLog) then Exit;
    glGetShaderiv := wglGetProcAddress('glGetShaderiv');
    if not Assigned(glGetShaderiv) then Exit;
    glGetShaderInfoLog := wglGetProcAddress('glGetShaderInfoLog');
    if not Assigned(glGetShaderInfoLog) then Exit;
    glGetShaderSource := wglGetProcAddress('glGetShaderSource');
    if not Assigned(glGetShaderSource) then Exit;
    glGetUniformLocation := wglGetProcAddress('glGetUniformLocation');
    if not Assigned(glGetUniformLocation) then Exit;
    glGetUniformfv := wglGetProcAddress('glGetUniformfv');
    if not Assigned(glGetUniformfv) then Exit;
    glGetUniformiv := wglGetProcAddress('glGetUniformiv');
    if not Assigned(glGetUniformiv) then Exit;
    glGetVertexAttribdv := wglGetProcAddress('glGetVertexAttribdv');
    if not Assigned(glGetVertexAttribdv) then Exit;
    glGetVertexAttribfv := wglGetProcAddress('glGetVertexAttribfv');
    if not Assigned(glGetVertexAttribfv) then Exit;
    glGetVertexAttribiv := wglGetProcAddress('glGetVertexAttribiv');
    if not Assigned(glGetVertexAttribiv) then Exit;
    glGetVertexAttribPointerv := wglGetProcAddress('glGetVertexAttribPointerv');
    if not Assigned(glGetVertexAttribPointerv) then Exit;
    glIsProgram := wglGetProcAddress('glIsProgram');
    if not Assigned(glIsProgram) then Exit;
    glIsShader := wglGetProcAddress('glIsShader');
    if not Assigned(glIsShader) then Exit;
    glLinkProgram := wglGetProcAddress('glLinkProgram');
    if not Assigned(glLinkProgram) then Exit;
    glShaderSource := wglGetProcAddress('glShaderSource');
    if not Assigned(glShaderSource) then Exit;
    glUseProgram := wglGetProcAddress('glUseProgram');
    if not Assigned(glUseProgram) then Exit;
    glUniform1f := wglGetProcAddress('glUniform1f');
    if not Assigned(glUniform1f) then Exit;
    glUniform2f := wglGetProcAddress('glUniform2f');
    if not Assigned(glUniform2f) then Exit;
    glUniform3f := wglGetProcAddress('glUniform3f');
    if not Assigned(glUniform3f) then Exit;
    glUniform4f := wglGetProcAddress('glUniform4f');
    if not Assigned(glUniform4f) then Exit;
    glUniform1i := wglGetProcAddress('glUniform1i');
    if not Assigned(glUniform1i) then Exit;
    glUniform2i := wglGetProcAddress('glUniform2i');
    if not Assigned(glUniform2i) then Exit;
    glUniform3i := wglGetProcAddress('glUniform3i');
    if not Assigned(glUniform3i) then Exit;
    glUniform4i := wglGetProcAddress('glUniform4i');
    if not Assigned(glUniform4i) then Exit;
    glUniform1fv := wglGetProcAddress('glUniform1fv');
    if not Assigned(glUniform1fv) then Exit;
    glUniform2fv := wglGetProcAddress('glUniform2fv');
    if not Assigned(glUniform2fv) then Exit;
    glUniform3fv := wglGetProcAddress('glUniform3fv');
    if not Assigned(glUniform3fv) then Exit;
    glUniform4fv := wglGetProcAddress('glUniform4fv');
    if not Assigned(glUniform4fv) then Exit;
    glUniform1iv := wglGetProcAddress('glUniform1iv');
    if not Assigned(glUniform1iv) then Exit;
    glUniform2iv := wglGetProcAddress('glUniform2iv');
    if not Assigned(glUniform2iv) then Exit;
    glUniform3iv := wglGetProcAddress('glUniform3iv');
    if not Assigned(glUniform3iv) then Exit;
    glUniform4iv := wglGetProcAddress('glUniform4iv');
    if not Assigned(glUniform4iv) then Exit;
    glUniformMatrix2fv := wglGetProcAddress('glUniformMatrix2fv');
    if not Assigned(glUniformMatrix2fv) then Exit;
    glUniformMatrix3fv := wglGetProcAddress('glUniformMatrix3fv');
    if not Assigned(glUniformMatrix3fv) then Exit;
    glUniformMatrix4fv := wglGetProcAddress('glUniformMatrix4fv');
    if not Assigned(glUniformMatrix4fv) then Exit;
    glValidateProgram := wglGetProcAddress('glValidateProgram');
    if not Assigned(glValidateProgram) then Exit;
    glVertexAttrib1d := wglGetProcAddress('glVertexAttrib1d');
    if not Assigned(glVertexAttrib1d) then Exit;
    glVertexAttrib1dv := wglGetProcAddress('glVertexAttrib1dv');
    if not Assigned(glVertexAttrib1dv) then Exit;
    glVertexAttrib1f := wglGetProcAddress('glVertexAttrib1f');
    if not Assigned(glVertexAttrib1f) then Exit;
    glVertexAttrib1fv := wglGetProcAddress('glVertexAttrib1fv');
    if not Assigned(glVertexAttrib1fv) then Exit;
    glVertexAttrib1s := wglGetProcAddress('glVertexAttrib1s');
    if not Assigned(glVertexAttrib1s) then Exit;
    glVertexAttrib1sv := wglGetProcAddress('glVertexAttrib1sv');
    if not Assigned(glVertexAttrib1sv) then Exit;
    glVertexAttrib2d := wglGetProcAddress('glVertexAttrib2d');
    if not Assigned(glVertexAttrib2d) then Exit;
    glVertexAttrib2dv := wglGetProcAddress('glVertexAttrib2dv');
    if not Assigned(glVertexAttrib2dv) then Exit;
    glVertexAttrib2f := wglGetProcAddress('glVertexAttrib2f');
    if not Assigned(glVertexAttrib2f) then Exit;
    glVertexAttrib2fv := wglGetProcAddress('glVertexAttrib2fv');
    if not Assigned(glVertexAttrib2fv) then Exit;
    glVertexAttrib2s := wglGetProcAddress('glVertexAttrib2s');
    if not Assigned(glVertexAttrib2s) then Exit;
    glVertexAttrib2sv := wglGetProcAddress('glVertexAttrib2sv');
    if not Assigned(glVertexAttrib2sv) then Exit;
    glVertexAttrib3d := wglGetProcAddress('glVertexAttrib3d');
    if not Assigned(glVertexAttrib3d) then Exit;
    glVertexAttrib3dv := wglGetProcAddress('glVertexAttrib3dv');
    if not Assigned(glVertexAttrib3dv) then Exit;
    glVertexAttrib3f := wglGetProcAddress('glVertexAttrib3f');
    if not Assigned(glVertexAttrib3f) then Exit;
    glVertexAttrib3fv := wglGetProcAddress('glVertexAttrib3fv');
    if not Assigned(glVertexAttrib3fv) then Exit;
    glVertexAttrib3s := wglGetProcAddress('glVertexAttrib3s');
    if not Assigned(glVertexAttrib3s) then Exit;
    glVertexAttrib3sv := wglGetProcAddress('glVertexAttrib3sv');
    if not Assigned(glVertexAttrib3sv) then Exit;
    glVertexAttrib4Nbv := wglGetProcAddress('glVertexAttrib4Nbv');
    if not Assigned(glVertexAttrib4Nbv) then Exit;
    glVertexAttrib4Niv := wglGetProcAddress('glVertexAttrib4Niv');
    if not Assigned(glVertexAttrib4Niv) then Exit;
    glVertexAttrib4Nsv := wglGetProcAddress('glVertexAttrib4Nsv');
    if not Assigned(glVertexAttrib4Nsv) then Exit;
    glVertexAttrib4Nub := wglGetProcAddress('glVertexAttrib4Nub');
    if not Assigned(glVertexAttrib4Nub) then Exit;
    glVertexAttrib4Nubv := wglGetProcAddress('glVertexAttrib4Nubv');
    if not Assigned(glVertexAttrib4Nubv) then Exit;
    glVertexAttrib4Nuiv := wglGetProcAddress('glVertexAttrib4Nuiv');
    if not Assigned(glVertexAttrib4Nuiv) then Exit;
    glVertexAttrib4Nusv := wglGetProcAddress('glVertexAttrib4Nusv');
    if not Assigned(glVertexAttrib4Nusv) then Exit;
    glVertexAttrib4bv := wglGetProcAddress('glVertexAttrib4bv');
    if not Assigned(glVertexAttrib4bv) then Exit;
    glVertexAttrib4d := wglGetProcAddress('glVertexAttrib4d');
    if not Assigned(glVertexAttrib4d) then Exit;
    glVertexAttrib4dv := wglGetProcAddress('glVertexAttrib4dv');
    if not Assigned(glVertexAttrib4dv) then Exit;
    glVertexAttrib4f := wglGetProcAddress('glVertexAttrib4f');
    if not Assigned(glVertexAttrib4f) then Exit;
    glVertexAttrib4fv := wglGetProcAddress('glVertexAttrib4fv');
    if not Assigned(glVertexAttrib4fv) then Exit;
    glVertexAttrib4iv := wglGetProcAddress('glVertexAttrib4iv');
    if not Assigned(glVertexAttrib4iv) then Exit;
    glVertexAttrib4s := wglGetProcAddress('glVertexAttrib4s');
    if not Assigned(glVertexAttrib4s) then Exit;
    glVertexAttrib4sv := wglGetProcAddress('glVertexAttrib4sv');
    if not Assigned(glVertexAttrib4sv) then Exit;
    glVertexAttrib4ubv := wglGetProcAddress('glVertexAttrib4ubv');
    if not Assigned(glVertexAttrib4ubv) then Exit;
    glVertexAttrib4uiv := wglGetProcAddress('glVertexAttrib4uiv');
    if not Assigned(glVertexAttrib4uiv) then Exit;
    glVertexAttrib4usv := wglGetProcAddress('glVertexAttrib4usv');
    if not Assigned(glVertexAttrib4usv) then Exit;
    glVertexAttribPointer := wglGetProcAddress('glVertexAttribPointer');
    if not Assigned(glVertexAttribPointer) then Exit;
    Result := Load_GL_version_1_5;

end;

function glext_LoadExtension(ext: ansistring): Boolean;
begin

  Result := FALSE;

  if ext = 'GL_version_1_2' then Result := Load_GL_version_1_2
  else if ext = 'GL_ARB_imaging' then Result := Load_GL_ARB_imaging
  else if ext = 'GL_version_1_3' then Result := Load_GL_version_1_3
  else if ext = 'GL_ARB_multitexture' then Result := Load_GL_ARB_multitexture
  else if ext = 'GL_ARB_transpose_matrix' then Result := Load_GL_ARB_transpose_matrix
  else if ext = 'GL_ARB_multisample' then Result := Load_GL_ARB_multisample
  else if ext = 'GL_ARB_texture_env_add' then Result := Load_GL_ARB_texture_env_add
{$IFDEF Windows}
  else if ext = 'WGL_ARB_extensions_string' then Result := Load_WGL_ARB_extensions_string
  else if ext = 'WGL_ARB_buffer_region' then Result := Load_WGL_ARB_buffer_region
{$ENDIF}
  else if ext = 'GL_ARB_texture_cube_map' then Result := Load_GL_ARB_texture_cube_map
  else if ext = 'GL_ARB_depth_texture' then Result := Load_GL_ARB_depth_texture
  else if ext = 'GL_ARB_point_parameters' then Result := Load_GL_ARB_point_parameters
  else if ext = 'GL_ARB_shadow' then Result := Load_GL_ARB_shadow
  else if ext = 'GL_ARB_shadow_ambient' then Result := Load_GL_ARB_shadow_ambient
  else if ext = 'GL_ARB_texture_border_clamp' then Result := Load_GL_ARB_texture_border_clamp
  else if ext = 'GL_ARB_texture_compression' then Result := Load_GL_ARB_texture_compression
  else if ext = 'GL_ARB_texture_env_combine' then Result := Load_GL_ARB_texture_env_combine
  else if ext = 'GL_ARB_texture_env_crossbar' then Result := Load_GL_ARB_texture_env_crossbar
  else if ext = 'GL_ARB_texture_env_dot3' then Result := Load_GL_ARB_texture_env_dot3
  else if ext = 'GL_ARB_texture_mirrored_repeat' then Result := Load_GL_ARB_texture_mirrored_repeat
  else if ext = 'GL_ARB_vertex_blend' then Result := Load_GL_ARB_vertex_blend
  else if ext = 'GL_ARB_vertex_program' then Result := Load_GL_ARB_vertex_program
  else if ext = 'GL_ARB_window_pos' then Result := Load_GL_ARB_window_pos
  else if ext = 'GL_EXT_422_pixels' then Result := Load_GL_EXT_422_pixels
  else if ext = 'GL_EXT_abgr' then Result := Load_GL_EXT_abgr
  else if ext = 'GL_EXT_bgra' then Result := Load_GL_EXT_bgra
  else if ext = 'GL_EXT_blend_color' then Result := Load_GL_EXT_blend_color
  else if ext = 'GL_EXT_blend_func_separate' then Result := Load_GL_EXT_blend_func_separate
  else if ext = 'GL_EXT_blend_logic_op' then Result := Load_GL_EXT_blend_logic_op
  else if ext = 'GL_EXT_blend_minmax' then Result := Load_GL_EXT_blend_minmax
  else if ext = 'GL_EXT_blend_subtract' then Result := Load_GL_EXT_blend_subtract
  else if ext = 'GL_EXT_clip_volume_hint' then Result := Load_GL_EXT_clip_volume_hint
  else if ext = 'GL_EXT_color_subtable' then Result := Load_GL_EXT_color_subtable
  else if ext = 'GL_EXT_compiled_vertex_array' then Result := Load_GL_EXT_compiled_vertex_array
  else if ext = 'GL_EXT_convolution' then Result := Load_GL_EXT_convolution
  else if ext = 'GL_EXT_fog_coord' then Result := Load_GL_EXT_fog_coord
  else if ext = 'GL_EXT_histogram' then Result := Load_GL_EXT_histogram
  else if ext = 'GL_EXT_multi_draw_arrays' then Result := Load_GL_EXT_multi_draw_arrays
  else if ext = 'GL_EXT_packed_pixels' then Result := Load_GL_EXT_packed_pixels
  else if ext = 'GL_EXT_paletted_texture' then Result := Load_GL_EXT_paletted_texture
  else if ext = 'GL_EXT_point_parameters' then Result := Load_GL_EXT_point_parameters
  else if ext = 'GL_EXT_polygon_offset' then Result := Load_GL_EXT_polygon_offset
  else if ext = 'GL_EXT_secondary_color' then Result := Load_GL_EXT_secondary_color
  else if ext = 'GL_EXT_separate_specular_color' then Result := Load_GL_EXT_separate_specular_color
  else if ext = 'GL_EXT_shadow_funcs' then Result := Load_GL_EXT_shadow_funcs
  else if ext = 'GL_EXT_shared_texture_palette' then Result := Load_GL_EXT_shared_texture_palette
  else if ext = 'GL_EXT_stencil_two_side' then Result := Load_GL_EXT_stencil_two_side
  else if ext = 'GL_EXT_stencil_wrap' then Result := Load_GL_EXT_stencil_wrap
  else if ext = 'GL_EXT_subtexture' then Result := Load_GL_EXT_subtexture
  else if ext = 'GL_EXT_texture3D' then Result := Load_GL_EXT_texture3D
  else if ext = 'GL_EXT_texture_compression_s3tc' then Result := Load_GL_EXT_texture_compression_s3tc
  else if ext = 'GL_EXT_texture_env_add' then Result := Load_GL_EXT_texture_env_add
  else if ext = 'GL_EXT_texture_env_combine' then Result := Load_GL_EXT_texture_env_combine
  else if ext = 'GL_EXT_texture_env_dot3' then Result := Load_GL_EXT_texture_env_dot3
  else if ext = 'GL_EXT_texture_filter_anisotropic' then Result := Load_GL_EXT_texture_filter_anisotropic
  else if ext = 'GL_EXT_texture_lod_bias' then Result := Load_GL_EXT_texture_lod_bias
  else if ext = 'GL_EXT_texture_object' then Result := Load_GL_EXT_texture_object
  else if ext = 'GL_EXT_vertex_array' then Result := Load_GL_EXT_vertex_array
  else if ext = 'GL_EXT_vertex_shader' then Result := Load_GL_EXT_vertex_shader
  else if ext = 'GL_EXT_vertex_weighting' then Result := Load_GL_EXT_vertex_weighting
  else if ext = 'GL_HP_occlusion_test' then Result := Load_GL_HP_occlusion_test
  else if ext = 'GL_NV_blend_square' then Result := Load_GL_NV_blend_square
  else if ext = 'GL_NV_copy_depth_to_color' then Result := Load_GL_NV_copy_depth_to_color
  else if ext = 'GL_NV_depth_clamp' then Result := Load_GL_NV_depth_clamp
  else if ext = 'GL_NV_evaluators' then Result := Load_GL_NV_evaluators
  else if ext = 'GL_NV_fence' then Result := Load_GL_NV_fence
  else if ext = 'GL_NV_fog_distance' then Result := Load_GL_NV_fog_distance
  else if ext = 'GL_NV_light_max_exponent' then Result := Load_GL_NV_light_max_exponent
  else if ext = 'GL_NV_multisample_filter_hint' then Result := Load_GL_NV_multisample_filter_hint
  else if ext = 'GL_NV_occlusion_query' then Result := Load_GL_NV_occlusion_query
  else if ext = 'GL_NV_packed_depth_stencil' then Result := Load_GL_NV_packed_depth_stencil
  else if ext = 'GL_NV_point_sprite' then Result := Load_GL_NV_point_sprite
  else if ext = 'GL_NV_register_combiners' then Result := Load_GL_NV_register_combiners
  else if ext = 'GL_NV_register_combiners2' then Result := Load_GL_NV_register_combiners2
  else if ext = 'GL_NV_texgen_emboss' then Result := Load_GL_NV_texgen_emboss
  else if ext = 'GL_NV_texgen_reflection' then Result := Load_GL_NV_texgen_reflection
  else if ext = 'GL_NV_texture_compression_vtc' then Result := Load_GL_NV_texture_compression_vtc
  else if ext = 'GL_NV_texture_env_combine4' then Result := Load_GL_NV_texture_env_combine4
  else if ext = 'GL_NV_texture_rectangle' then Result := Load_GL_NV_texture_rectangle
  else if ext = 'GL_NV_texture_shader' then Result := Load_GL_NV_texture_shader
  else if ext = 'GL_NV_texture_shader2' then Result := Load_GL_NV_texture_shader2
  else if ext = 'GL_NV_texture_shader3' then Result := Load_GL_NV_texture_shader3
  else if ext = 'GL_NV_vertex_array_range' then Result := Load_GL_NV_vertex_array_range
  else if ext = 'GL_NV_vertex_array_range2' then Result := Load_GL_NV_vertex_array_range2
  else if ext = 'GL_NV_vertex_program' then Result := Load_GL_NV_vertex_program
  else if ext = 'GL_NV_vertex_program1_1' then Result := Load_GL_NV_vertex_program1_1
  else if ext = 'GL_ATI_element_array' then Result := Load_GL_ATI_element_array
  else if ext = 'GL_ATI_envmap_bumpmap' then Result := Load_GL_ATI_envmap_bumpmap
  else if ext = 'GL_ATI_fragment_shader' then Result := Load_GL_ATI_fragment_shader
  else if ext = 'GL_ATI_pn_triangles' then Result := Load_GL_ATI_pn_triangles
  else if ext = 'GL_ATI_texture_mirror_once' then Result := Load_GL_ATI_texture_mirror_once
  else if ext = 'GL_ATI_vertex_array_object' then Result := Load_GL_ATI_vertex_array_object
  else if ext = 'GL_ATI_vertex_streams' then Result := Load_GL_ATI_vertex_streams
{$IFDEF Windows}
  else if ext = 'WGL_I3D_image_buffer' then Result := Load_WGL_I3D_image_buffer
  else if ext = 'WGL_I3D_swap_frame_lock' then Result := Load_WGL_I3D_swap_frame_lock
  else if ext = 'WGL_I3D_swap_frame_usage' then Result := Load_WGL_I3D_swap_frame_usage
{$ENDIF}
  else if ext = 'GL_3DFX_texture_compression_FXT1' then Result := Load_GL_3DFX_texture_compression_FXT1
  else if ext = 'GL_IBM_cull_vertex' then Result := Load_GL_IBM_cull_vertex
  else if ext = 'GL_IBM_multimode_draw_arrays' then Result := Load_GL_IBM_multimode_draw_arrays
  else if ext = 'GL_IBM_raster_pos_clip' then Result := Load_GL_IBM_raster_pos_clip
  else if ext = 'GL_IBM_texture_mirrored_repeat' then Result := Load_GL_IBM_texture_mirrored_repeat
  else if ext = 'GL_IBM_vertex_array_lists' then Result := Load_GL_IBM_vertex_array_lists
  else if ext = 'GL_MESA_resize_buffers' then Result := Load_GL_MESA_resize_buffers
  else if ext = 'GL_MESA_window_pos' then Result := Load_GL_MESA_window_pos
  else if ext = 'GL_OML_interlace' then Result := Load_GL_OML_interlace
  else if ext = 'GL_OML_resample' then Result := Load_GL_OML_resample
  else if ext = 'GL_OML_subsample' then Result := Load_GL_OML_subsample
  else if ext = 'GL_SGIS_generate_mipmap' then Result := Load_GL_SGIS_generate_mipmap
  else if ext = 'GL_SGIS_multisample' then Result := Load_GL_SGIS_multisample
  else if ext = 'GL_SGIS_pixel_texture' then Result := Load_GL_SGIS_pixel_texture
  else if ext = 'GL_SGIS_texture_border_clamp' then Result := Load_GL_SGIS_texture_border_clamp
  else if ext = 'GL_SGIS_texture_color_mask' then Result := Load_GL_SGIS_texture_color_mask
  else if ext = 'GL_SGIS_texture_edge_clamp' then Result := Load_GL_SGIS_texture_edge_clamp
  else if ext = 'GL_SGIS_texture_lod' then Result := Load_GL_SGIS_texture_lod
  else if ext = 'GL_SGIS_depth_texture' then Result := Load_GL_SGIS_depth_texture
  else if ext = 'GL_SGIX_fog_offset' then Result := Load_GL_SGIX_fog_offset
  else if ext = 'GL_SGIX_interlace' then Result := Load_GL_SGIX_interlace
  else if ext = 'GL_SGIX_shadow_ambient' then Result := Load_GL_SGIX_shadow_ambient
  else if ext = 'GL_SGI_color_matrix' then Result := Load_GL_SGI_color_matrix
  else if ext = 'GL_SGI_color_table' then Result := Load_GL_SGI_color_table
  else if ext = 'GL_SGI_texture_color_table' then Result := Load_GL_SGI_texture_color_table
  else if ext = 'GL_SUN_vertex' then Result := Load_GL_SUN_vertex
  else if ext = 'GL_ARB_fragment_program' then Result := Load_GL_ARB_fragment_program
  else if ext = 'GL_ATI_text_fragment_shader' then Result := Load_GL_ATI_text_fragment_shader
  else if ext = 'GL_APPLE_client_storage' then Result := Load_GL_APPLE_client_storage
  else if ext = 'GL_APPLE_element_array' then Result := Load_GL_APPLE_element_array
  else if ext = 'GL_APPLE_fence' then Result := Load_GL_APPLE_fence
  else if ext = 'GL_APPLE_vertex_array_object' then Result := Load_GL_APPLE_vertex_array_object
  else if ext = 'GL_APPLE_vertex_array_range' then Result := Load_GL_APPLE_vertex_array_range
{$IFDEF Windows}
  else if ext = 'WGL_ARB_pixel_format' then Result := Load_WGL_ARB_pixel_format
  else if ext = 'WGL_ARB_make_current_read' then Result := Load_WGL_ARB_make_current_read
  else if ext = 'WGL_ARB_pbuffer' then Result := Load_WGL_ARB_pbuffer
  else if ext = 'WGL_EXT_swap_control' then Result := Load_WGL_EXT_swap_control
  else if ext = 'WGL_ARB_render_texture' then Result := Load_WGL_ARB_render_texture
  else if ext = 'WGL_EXT_extensions_string' then Result := Load_WGL_EXT_extensions_string
  else if ext = 'WGL_EXT_make_current_read' then Result := Load_WGL_EXT_make_current_read
  else if ext = 'WGL_EXT_pbuffer' then Result := Load_WGL_EXT_pbuffer
  else if ext = 'WGL_EXT_pixel_format' then Result := Load_WGL_EXT_pixel_format
  else if ext = 'WGL_I3D_digital_video_control' then Result := Load_WGL_I3D_digital_video_control
  else if ext = 'WGL_I3D_gamma' then Result := Load_WGL_I3D_gamma
  else if ext = 'WGL_I3D_genlock' then Result := Load_WGL_I3D_genlock
{$ENDIF}
  else if ext = 'GL_ARB_matrix_palette' then Result := Load_GL_ARB_matrix_palette
  else if ext = 'GL_NV_element_array' then Result := Load_GL_NV_element_array
  else if ext = 'GL_NV_float_buffer' then Result := Load_GL_NV_float_buffer
  else if ext = 'GL_NV_fragment_program' then Result := Load_GL_NV_fragment_program
  else if ext = 'GL_NV_primitive_restart' then Result := Load_GL_NV_primitive_restart
  else if ext = 'GL_NV_vertex_program2' then Result := Load_GL_NV_vertex_program2
  {$IFDEF Windows}
  else if ext = 'WGL_NV_render_texture_rectangle' then Result := Load_WGL_NV_render_texture_rectangle
  {$ENDIF}
  else if ext = 'GL_NV_pixel_data_range' then Result := Load_GL_NV_pixel_data_range
  else if ext = 'GL_EXT_texture_rectangle' then Result := Load_GL_EXT_texture_rectangle
  else if ext = 'GL_S3_s3tc' then Result := Load_GL_S3_s3tc
  else if ext = 'GL_ATI_draw_buffers' then Result := Load_GL_ATI_draw_buffers
  {$IFDEF Windows}
  else if ext = 'WGL_ATI_pixel_format_float' then Result := Load_WGL_ATI_pixel_format_float
  {$ENDIF}
  else if ext = 'GL_ATI_texture_env_combine3' then Result := Load_GL_ATI_texture_env_combine3
  else if ext = 'GL_ATI_texture_float' then Result := Load_GL_ATI_texture_float
  else if ext = 'GL_NV_texture_expand_normal' then Result := Load_GL_NV_texture_expand_normal
  else if ext = 'GL_NV_half_float' then Result := Load_GL_NV_half_float
  else if ext = 'GL_ATI_map_object_buffer' then Result := Load_GL_ATI_map_object_buffer
  else if ext = 'GL_ATI_separate_stencil' then Result := Load_GL_ATI_separate_stencil
  else if ext = 'GL_ATI_vertex_attrib_array_object' then Result := Load_GL_ATI_vertex_attrib_array_object
  else if ext = 'GL_ARB_vertex_buffer_object' then Result := Load_GL_ARB_vertex_buffer_object
  else if ext = 'GL_ARB_occlusion_query' then Result := Load_GL_ARB_occlusion_query
  else if ext = 'GL_ARB_shader_objects' then Result := Load_GL_ARB_shader_objects
  else if ext = 'GL_ARB_vertex_shader' then Result := Load_GL_ARB_vertex_shader
  else if ext = 'GL_ARB_fragment_shader' then Result := Load_GL_ARB_fragment_shader
  else if ext = 'GL_ARB_shading_language_100' then Result := Load_GL_ARB_shading_language_100
  else if ext = 'GL_ARB_texture_non_power_of_two' then Result := Load_GL_ARB_texture_non_power_of_two
  else if ext = 'GL_ARB_point_sprite' then Result := Load_GL_ARB_point_sprite
  else if ext = 'GL_EXT_depth_bounds_test' then Result := Load_GL_EXT_depth_bounds_test
  else if ext = 'GL_EXT_texture_mirror_clamp' then Result := Load_GL_EXT_texture_mirror_clamp
  else if ext = 'GL_EXT_blend_equation_separate' then Result := Load_GL_EXT_blend_equation_separate
  else if ext = 'GL_MESA_pack_invert' then Result := Load_GL_MESA_pack_invert
  else if ext = 'GL_MESA_ycbcr_texture' then Result := Load_GL_MESA_ycbcr_texture
  else if ext = 'GL_ARB_fragment_program_shadow' then Result := Load_GL_ARB_fragment_program_shadow
  else if ext = 'GL_NV_fragment_program_option' then Result := Load_GL_NV_fragment_program_option
  else if ext = 'GL_EXT_pixel_buffer_object' then Result := Load_GL_EXT_pixel_buffer_object
  else if ext = 'GL_NV_fragment_program2' then Result := Load_GL_NV_fragment_program2
  else if ext = 'GL_NV_vertex_program2_option' then Result := Load_GL_NV_vertex_program2_option
  else if ext = 'GL_NV_vertex_program3' then Result := Load_GL_NV_vertex_program3
  else if ext = 'GL_ARB_draw_buffers' then Result := Load_GL_ARB_draw_buffers
  else if ext = 'GL_ARB_texture_rectangle' then Result := Load_GL_ARB_texture_rectangle
  else if ext = 'GL_ARB_color_buffer_float' then Result := Load_GL_ARB_color_buffer_float
  else if ext = 'GL_ARB_half_float_pixel' then Result := Load_GL_ARB_half_float_pixel
  else if ext = 'GL_ARB_texture_float' then Result := Load_GL_ARB_texture_float
  else if ext = 'GL_EXT_texture_compression_dxt1' then Result := Load_GL_EXT_texture_compression_dxt1
  else if ext = 'GL_ARB_pixel_buffer_object' then Result := Load_GL_ARB_pixel_buffer_object
  else if ext = 'GL_EXT_framebuffer_object' then Result := Load_GL_EXT_framebuffer_object
  else if ext = 'GL_version_1_4' then Result := Load_GL_version_1_4
  else if ext = 'GL_version_1_5' then Result := Load_GL_version_1_5
  else if ext = 'GL_version_2_0' then Result := Load_GL_version_2_0

end;

function Load_GL_VERSION_2_1(): Boolean;
begin
  Result := False;
  glUniformMatrix2x3fv := wglGetProcAddress('glUniformMatrix2x3fv');
  if not Assigned(glUniformMatrix2x3fv) then Exit;
  glUniformMatrix3x2fv := wglGetProcAddress('glUniformMatrix3x2fv');
  if not Assigned(glUniformMatrix3x2fv) then Exit;
  glUniformMatrix2x4fv := wglGetProcAddress('glUniformMatrix2x4fv');
  if not Assigned(glUniformMatrix2x4fv) then Exit;
  glUniformMatrix4x2fv := wglGetProcAddress('glUniformMatrix4x2fv');
  if not Assigned(glUniformMatrix4x2fv) then Exit;
  glUniformMatrix3x4fv := wglGetProcAddress('glUniformMatrix3x4fv');
  if not Assigned(glUniformMatrix3x4fv) then Exit;
  glUniformMatrix4x3fv := wglGetProcAddress('glUniformMatrix4x3fv');
  if not Assigned(glUniformMatrix4x3fv) then Exit;
  Result := Load_GL_VERSION_2_0();
end;

function Load_GL_VERSION_3_0(): Boolean;
begin
  Result := False;
  glColorMaski := wglGetProcAddress('glColorMaski');
  if not Assigned(glColorMaski) then Exit;
  glGetBooleani_v := wglGetProcAddress('glGetBooleani_v');
  if not Assigned(glGetBooleani_v) then Exit;
  glGetIntegeri_v := wglGetProcAddress('glGetIntegeri_v');
  if not Assigned(glGetIntegeri_v) then Exit;
  glEnablei := wglGetProcAddress('glEnablei');
  if not Assigned(glEnablei) then Exit;
  glDisablei := wglGetProcAddress('glDisablei');
  if not Assigned(glDisablei) then Exit;
  glIsEnabledi := wglGetProcAddress('glIsEnabledi');
  if not Assigned(glIsEnabledi) then Exit;
  glBeginTransformFeedback := wglGetProcAddress('glBeginTransformFeedback');
  if not Assigned(glBeginTransformFeedback) then Exit;
  glEndTransformFeedback := wglGetProcAddress('glEndTransformFeedback');
  if not Assigned(glEndTransformFeedback) then Exit;
  glBindBufferRange := wglGetProcAddress('glBindBufferRange');
  if not Assigned(glBindBufferRange) then Exit;
  glBindBufferBase := wglGetProcAddress('glBindBufferBase');
  if not Assigned(glBindBufferBase) then Exit;
  glTransformFeedbackVaryings := wglGetProcAddress('glTransformFeedbackVaryings');
  if not Assigned(glTransformFeedbackVaryings) then Exit;
  glGetTransformFeedbackVarying := wglGetProcAddress('glGetTransformFeedbackVarying');
  if not Assigned(glGetTransformFeedbackVarying) then Exit;
  glClampColor := wglGetProcAddress('glClampColor');
  if not Assigned(glClampColor) then Exit;
  glBeginConditionalRender := wglGetProcAddress('glBeginConditionalRender');
  if not Assigned(glBeginConditionalRender) then Exit;
  glEndConditionalRender := wglGetProcAddress('glEndConditionalRender');
  if not Assigned(glEndConditionalRender) then Exit;
  glVertexAttribIPointer := wglGetProcAddress('glVertexAttribIPointer');
  if not Assigned(glVertexAttribIPointer) then Exit;
  glGetVertexAttribIiv := wglGetProcAddress('glGetVertexAttribIiv');
  if not Assigned(glGetVertexAttribIiv) then Exit;
  glGetVertexAttribIuiv := wglGetProcAddress('glGetVertexAttribIuiv');
  if not Assigned(glGetVertexAttribIuiv) then Exit;
  glVertexAttribI1i := wglGetProcAddress('glVertexAttribI1i');
  if not Assigned(glVertexAttribI1i) then Exit;
  glVertexAttribI2i := wglGetProcAddress('glVertexAttribI2i');
  if not Assigned(glVertexAttribI2i) then Exit;
  glVertexAttribI3i := wglGetProcAddress('glVertexAttribI3i');
  if not Assigned(glVertexAttribI3i) then Exit;
  glVertexAttribI4i := wglGetProcAddress('glVertexAttribI4i');
  if not Assigned(glVertexAttribI4i) then Exit;
  glVertexAttribI1ui := wglGetProcAddress('glVertexAttribI1ui');
  if not Assigned(glVertexAttribI1ui) then Exit;
  glVertexAttribI2ui := wglGetProcAddress('glVertexAttribI2ui');
  if not Assigned(glVertexAttribI2ui) then Exit;
  glVertexAttribI3ui := wglGetProcAddress('glVertexAttribI3ui');
  if not Assigned(glVertexAttribI3ui) then Exit;
  glVertexAttribI4ui := wglGetProcAddress('glVertexAttribI4ui');
  if not Assigned(glVertexAttribI4ui) then Exit;
  glVertexAttribI1iv := wglGetProcAddress('glVertexAttribI1iv');
  if not Assigned(glVertexAttribI1iv) then Exit;
  glVertexAttribI2iv := wglGetProcAddress('glVertexAttribI2iv');
  if not Assigned(glVertexAttribI2iv) then Exit;
  glVertexAttribI3iv := wglGetProcAddress('glVertexAttribI3iv');
  if not Assigned(glVertexAttribI3iv) then Exit;
  glVertexAttribI4iv := wglGetProcAddress('glVertexAttribI4iv');
  if not Assigned(glVertexAttribI4iv) then Exit;
  glVertexAttribI1uiv := wglGetProcAddress('glVertexAttribI1uiv');
  if not Assigned(glVertexAttribI1uiv) then Exit;
  glVertexAttribI2uiv := wglGetProcAddress('glVertexAttribI2uiv');
  if not Assigned(glVertexAttribI2uiv) then Exit;
  glVertexAttribI3uiv := wglGetProcAddress('glVertexAttribI3uiv');
  if not Assigned(glVertexAttribI3uiv) then Exit;
  glVertexAttribI4uiv := wglGetProcAddress('glVertexAttribI4uiv');
  if not Assigned(glVertexAttribI4uiv) then Exit;
  glVertexAttribI4bv := wglGetProcAddress('glVertexAttribI4bv');
  if not Assigned(glVertexAttribI4bv) then Exit;
  glVertexAttribI4sv := wglGetProcAddress('glVertexAttribI4sv');
  if not Assigned(glVertexAttribI4sv) then Exit;
  glVertexAttribI4ubv := wglGetProcAddress('glVertexAttribI4ubv');
  if not Assigned(glVertexAttribI4ubv) then Exit;
  glVertexAttribI4usv := wglGetProcAddress('glVertexAttribI4usv');
  if not Assigned(glVertexAttribI4usv) then Exit;
  glGetUniformuiv := wglGetProcAddress('glGetUniformuiv');
  if not Assigned(glGetUniformuiv) then Exit;
  glBindFragDataLocation := wglGetProcAddress('glBindFragDataLocation');
  if not Assigned(glBindFragDataLocation) then Exit;
  glGetFragDataLocation := wglGetProcAddress('glGetFragDataLocation');
  if not Assigned(glGetFragDataLocation) then Exit;
  glUniform1ui := wglGetProcAddress('glUniform1ui');
  if not Assigned(glUniform1ui) then Exit;
  glUniform2ui := wglGetProcAddress('glUniform2ui');
  if not Assigned(glUniform2ui) then Exit;
  glUniform3ui := wglGetProcAddress('glUniform3ui');
  if not Assigned(glUniform3ui) then Exit;
  glUniform4ui := wglGetProcAddress('glUniform4ui');
  if not Assigned(glUniform4ui) then Exit;
  glUniform1uiv := wglGetProcAddress('glUniform1uiv');
  if not Assigned(glUniform1uiv) then Exit;
  glUniform2uiv := wglGetProcAddress('glUniform2uiv');
  if not Assigned(glUniform2uiv) then Exit;
  glUniform3uiv := wglGetProcAddress('glUniform3uiv');
  if not Assigned(glUniform3uiv) then Exit;
  glUniform4uiv := wglGetProcAddress('glUniform4uiv');
  if not Assigned(glUniform4uiv) then Exit;
  glTexParameterIiv := wglGetProcAddress('glTexParameterIiv');
  if not Assigned(glTexParameterIiv) then Exit;
  glTexParameterIuiv := wglGetProcAddress('glTexParameterIuiv');
  if not Assigned(glTexParameterIuiv) then Exit;
  glGetTexParameterIiv := wglGetProcAddress('glGetTexParameterIiv');
  if not Assigned(glGetTexParameterIiv) then Exit;
  glGetTexParameterIuiv := wglGetProcAddress('glGetTexParameterIuiv');
  if not Assigned(glGetTexParameterIuiv) then Exit;
  glClearBufferiv := wglGetProcAddress('glClearBufferiv');
  if not Assigned(glClearBufferiv) then Exit;
  glClearBufferuiv := wglGetProcAddress('glClearBufferuiv');
  if not Assigned(glClearBufferuiv) then Exit;
  glClearBufferfv := wglGetProcAddress('glClearBufferfv');
  if not Assigned(glClearBufferfv) then Exit;
  glClearBufferfi := wglGetProcAddress('glClearBufferfi');
  if not Assigned(glClearBufferfi) then Exit;
  glGetStringi := wglGetProcAddress('glGetStringi');
  if not Assigned(glGetStringi) then Exit;
  if not Load_GL_ARB_framebuffer_object(true) then Exit;
  if not Load_GL_ARB_map_buffer_range(true) then Exit;
  if not Load_GL_ARB_vertex_array_object(true) then Exit;
  Result := Load_GL_VERSION_2_1();
end;

function Load_GL_VERSION_3_1(): Boolean;
begin
  Result := False;
  glDrawArraysInstanced := wglGetProcAddress('glDrawArraysInstanced');
  if not Assigned(glDrawArraysInstanced) then Exit;
  glDrawElementsInstanced := wglGetProcAddress('glDrawElementsInstanced');
  if not Assigned(glDrawElementsInstanced) then Exit;
  glTexBuffer := wglGetProcAddress('glTexBuffer');
  if not Assigned(glTexBuffer) then Exit;
  glPrimitiveRestartIndex := wglGetProcAddress('glPrimitiveRestartIndex');
  if not Assigned(glPrimitiveRestartIndex) then Exit;
  if not Load_GL_ARB_copy_buffer(true) then Exit;
  if not Load_GL_ARB_uniform_buffer_object(true) then Exit;
  Result := Load_GL_VERSION_3_0();
end;

function Load_GL_VERSION_3_2(): Boolean;
begin
  Result := False;
  glGetInteger64i_v := wglGetProcAddress('glGetInteger64i_v');
  if not Assigned(glGetInteger64i_v) then Exit;
  glGetBufferParameteri64v := wglGetProcAddress('glGetBufferParameteri64v');
  if not Assigned(glGetBufferParameteri64v) then Exit;
  glProgramParameteri := wglGetProcAddress('glProgramParameteri');
  if not Assigned(glProgramParameteri) then Exit;
  glFramebufferTexture := wglGetProcAddress('glFramebufferTexture');
  if not Assigned(glFramebufferTexture) then Exit;
  if not Load_GL_ARB_draw_elements_base_vertex(true) then Exit;
  if not Load_GL_ARB_provoking_vertex(true) then Exit;
  if not Load_GL_ARB_sync(true) then Exit;
  if not Load_GL_ARB_texture_multisample(true) then Exit;
  Result := Load_GL_VERSION_3_1();
end;

function Load_GL_VERSION_3_3(): Boolean;
begin
  Result := False;
  if not Load_GL_ARB_blend_func_extended(true) then Exit;
  if not Load_GL_ARB_sampler_objects(true) then Exit;
  if not Load_GL_ARB_timer_query(true) then Exit;
  if not Load_GL_ARB_vertex_type_2_10_10_10_rev(true) then Exit;
  Result := Load_GL_VERSION_3_2();
end;

function Load_GL_VERSION_4_0(): Boolean;
begin
  Result := False;
  if not Load_GL_ARB_gpu_shader_fp64(true) then Exit;
  if not Load_GL_ARB_shader_subroutine(true) then Exit;
  if not Load_GL_ARB_tessellation_shader(true) then Exit;
  if not Load_GL_ARB_transform_feedback2(true) then Exit;
  if not Load_GL_ARB_transform_feedback3(true) then Exit;
  Result := Load_GL_VERSION_3_3();
end;

initialization

  { according to bug 7570, this is necessary on all x86 platforms,
    maybe we've to fix the sse control word as well }
  { Yes, at least for darwin/x86_64 (JM) }
{$ifdef fpc}
{$if defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$ifend}
{$else}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}

  {$IFDEF Windows}
  LoadOpenGL('opengl32.dll');
  {$ELSE}
  {$IFDEF OS2}
  LoadOpenGL('opengl.dll');
  {$ELSE OS2}
  {$ifdef darwin}
  LoadOpenGL('/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib');
  {$ELSE}
  {$IFDEF MorphOS}
  InitTinyGLLibrary;
  {$ELSE}
  {$ifdef haiku}
  LoadOpenGL('libGL.so');
  {$else}
  LoadOpenGL('libGL.so.1');
  {$endif}
  {$ENDIF}
  {$endif}
  {$ENDIF OS2}
  {$ENDIF}

finalization

  FreeOpenGL;

{$endif}

end.
