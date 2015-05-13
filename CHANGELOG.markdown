0.7.7
-----
* Added suuport for a new extension:
  * `KHR_no_error`

0.7.6
-----
* Correct the type signature of `glListDrawCommandsStatesClientNV`.
* Remove `glSampleCoverageOES` from `OES_fixed_point` extension due to changes made to its specification.
* Added support for two new extensions:
  * `OVR_multiview`
  * `OVR_multiview2`

0.7.5
-----
* Added support for two new extensions:
  * `EXT_buffer_storage`
  * `EXT_sparse_texture`

0.7.4
-----
* Added support for three new extensions:
  * `EXT_float_blend`
  * `NV_command_list`
  * `NV_polygon_mode`

0.7.3
-----
* Added support for three new extensions:
  * `EXT_texture_sRGB_R8`
  * `EXT_texture_sRGB_RG8`
  * `EXT_YUV_target`

0.7.2.4
-------
* `filepath` 1.4 support

0.7.2.3
-------
* Switch to `stdcall` on 32-bit Windows, per the OpenGL ABI.

0.7.2
-----
* Remove `GL_TEXTURE_BINDING` from OpenGL 4.5 and `ARB_direct_state_access` due to changes made to their specifications.

0.7.1
-----
* Fix enumeration constants for the `SGIX_resample` extension to their correct values.
* Added support for a new extension:
  * `OES_EGL_image_external_essl3`

0.7
---
* Fixed the type of `glPathGlyphIndexRangeNV`. (issue #6)
* Added support for fourteen new extensions:
  * `OES_copy_image`
  * `OES_draw_buffers_indexed`
  * `OES_draw_elements_base_vertex`
  * `OES_geometry_point_size`
  * `OES_geometry_shader`
  * `OES_gpu_shader5`
  * `OES_primitive_bounding_box`
  * `OES_shader_io_blocks`
  * `OES_tessellation_point_size`
  * `OES_tessellation_shader`
  * `OES_texture_border_clamp`
  * `OES_texture_buffer`
  * `OES_texture_cube_map_array`
  * `OES_texture_view`

0.6.3
-----
* Added support for a new extension:
  * `NV_viewport_array2`

0.6.2
-----
* Added `GL_BLEND_COLOR` enumeration for OpenGL versions 3.1 and up. See https://khronos.org/bugzilla/show_bug.cgi?id=1249 for more information.

0.6.1
-----
* Added support for eight new extensions:
  * `EXT_base_instance`
  * `EXT_draw_elements_base_vertex`
  * `EXT_multi_draw_indirect`
  * `EXT_render_snorm`
  * `EXT_render_norm16`
  * `NV_image_formats`
  * `NV_shader_noperspective_interpolation`
  * `NV_viewport_array`

0.6
---
* Stopped exporting all extensions from `Graphics.GL`. You'll need to import `Graphics.GL.Ext` as well.
* Added exports for `GLhalf` and `GLhalfARB`

0.5
---
* Haddock links to the OpenGL ES 2 registry
* `Compatibility40` depends on `Compatibility33`
* Shorter modules names to try to eke out a successful windows build.

0.4
---
* Haddocks!
* Fixed a major issue where pointers to pointers in the API were getting the wrong types.

0.3
---
* Switched to non-C style types for the most part. `Word32`, etc. have better understood support within the Haskell ecosystem. `CPtrdiff` remains as it varies across viable target platforms.
* Added a dependency on `Numeric.Fixed` from the `fixed` package for `GLfixed`.

0.2
---
* Support `Half` from the `half` package for `GLhalfNV`, so you can compute with the results.

0.1
---
* Initial release
