

EXTENSION_ENTRY(dyn_glGetUniformfv,ptr_glGetUniformfv,"glGetUniformfv",glGetUniformfv,GLuint -> GLint -> Ptr GLfloat -> IO ())

#define EXTENSION_ENTRY(_dyn_entry,_ptr_entry,_str_entry,_entry,_ty) \
foreign import CALLCONV unsafe "dynamic" _dyn_entry :: Graphics.Rendering.OpenGL.Raw.Extensions.Invoker (_ty) ; \
_entry :: (_ty) ; \
_entry = _dyn_entry _ptr_entry ; \
_ptr_entry :: FunPtr a ; \
_ptr_entry = unsafePerformIO (Graphics.Rendering.OpenGL.Raw.Extensions.getExtensionEntry extensionNameString _str_entry) ; \
{-HASH NOINLINE _ptr_entry HASH-}

