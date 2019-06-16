/* -----------------------------------------------------------------------------
 *
 * Copyright   :  (c) Edward Kmett 2014, (c) Sven Panne 2013
 * License     :  BSD3
 *
 * Maintainer  :  Edward Kmett <ekmett@gmail.com>
 * Stability   :  stable
 * Portability :  portable
 *
 * C support for "Graphics.GL.Raw.Proc"
 * -------------------------------------------------------------------------- */

#if defined(USE_WGLGETPROCADDRESS)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static void*
getProcAddress(const char *name)
{
  static int firstTime = 1;
  static HMODULE handle = NULL;

  if (firstTime) {
    firstTime = 0;
    handle = LoadLibrary(TEXT("opengl32"));
  }
  return handle ? GetProcAddress(handle, name) : NULL;
}


void*
hs_gl_getProcAddress(const char *name)
{
  void *apiEntry = wglGetProcAddress(name);
  /* Sometimes core API entries are not returned via wglGetProcAddress, so
     we fall back to GetProcAddress in these cases. */
  return (apiEntry == NULL) ? getProcAddress(name) : apiEntry;
}

/* -------------------------------------------------------------------------- */
#elif defined(USE_NSADDRESSOFSYMBOL)

#include <mach-o/dyld.h>
#include <stdlib.h>
#include <string.h>

void*
hs_gl_getProcAddress(const char *name)
{
  NSSymbol symbol;

  /* Prepend a '_' for the Unix C symbol mangling convention */
  char* symbolName = (char*)malloc(strlen(name) + 2);
  if (!symbolName) {
    return NULL;
  }
  symbolName[0] = '_';
  strcpy(symbolName + 1, name);

  if (!NSIsSymbolNameDefined(symbolName)) {
    free(symbolName);
    return NULL;
  }

  symbol = NSLookupAndBindSymbol(symbolName);
  free(symbolName);
  if (!symbol) {
    return NULL;
  }

  return NSAddressOfSymbol(symbol);
}

/* -------------------------------------------------------------------------- */
#elif defined(USE_DLSYM)

#include <stdlib.h>
#include <dlfcn.h>

/* Do not depend on <GL/gl.h> */
typedef unsigned char GLubyte;

/* Do not depend on <GL/glx.h> */
typedef void (*genericFunctionPointer)(void);
typedef genericFunctionPointer (*PFNGLXGETPROCADDRESSARB)(const GLubyte *);

static const char* gpaNames[] = {
  "glXGetProcAddress", "glXGetProcAddressARB", "glXGetProcAddressEXT",
  "_glXGetProcAddress", "_glXGetProcAddressARB", "_glXGetProcAddressEXT"
};

void*
hs_gl_getProcAddress(const char *name)
{
  static int firstTime = 1;
  static void *handle = NULL;
  static void *gpa = NULL;

  if (firstTime) {
    firstTime = 0;

    /* Get a handle for our executable. */
    handle = dlopen(NULL, RTLD_LAZY);
    /* If fail this early, there's not much we can do about it. */
    if (!handle) {
      return NULL;
    }

    {
      /* Let's see if our platform supports a glXGetProcAddress() variant. */
      int numNames = (int)(sizeof(gpaNames) / sizeof(gpaNames[0]));
      int i;
      for (i = 0;   (!gpa) && (i < numNames);   ++i) {
        gpa = dlsym(handle, gpaNames[i]);
      }
    }
  }

  if (gpa) {
    /* Fine, we seem to have some kind of glXGetProcAddress(), so use it. */
    return ((PFNGLXGETPROCADDRESSARB)gpa)((const GLubyte *)name);
  } else if (handle) {
    /* Fallback to dlsym() if we have no glXGetProcAddress(), although we then
       ignore the fact that OpenGL entry points could be context dependent. */
    return dlsym(handle, name);
  } else {
    return NULL;
  }
}

/* -------------------------------------------------------------------------- */
#elif defined(USE_GLXGETPROCADDRESS)

/* Do not depend on <GL/gl.h> */
typedef unsigned char GLubyte;

/* Do not depend on <GL/glx.h> */
typedef void (*genericFunctionPointer)(void);
extern genericFunctionPointer glXGetProcAddressARB(const GLubyte *);

void*
hs_gl_getProcAddress(const char *name)
{
  return glXGetProcAddressARB((const GLubyte*)name);
}

/* -------------------------------------------------------------------------- */
#else

#error "Don't know how to retrieve OpenGL extension entries"

#endif
