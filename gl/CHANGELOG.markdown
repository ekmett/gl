0.8.0
-----
* Added support for cabal setup dependencies. This should ensure we build correctly in more environments going forward.
* Added support for OpenGL ES 3.2
* Added support for OpenGL SC 2.0
* Added missing `_EXT` suffix to pattern synonyms for `EXT_buffer_storage`
* Fixed incorrect export of the `ARB` variant of `glTexPageCommitment` in `EXT_sparse_texture`
* Added missing pattern synonym `GL_FRAMEBUFFER_INCOMPLETE_VIEW_TARGETS_OVR` for `OVR_multiview`
* Added suuport for fourty-four new extensions:
  * `AMD_framebuffer_sample_positions`
  * `AMD_gpu_shader_half_float`
  * `AMD_shader_ballot`
  * `AMD_shader_explicit_vertex_parameter`
  * `ARB_ES3_2_compatibility`
  * `ARB_fragment_shader_interlock`
  * `ARB_gpu_shader_int64`
  * `ARB_parallel_shader_compile`
  * `ARB_post_depth_coverage`
  * `ARB_sample_locations`
  * `ARB_shader_atomic_counter_ops`
  * `ARB_shader_ballot`
  * `ARB_shader_clock`
  * `ARB_shader_viewport_layer_array`
  * `ARB_sparse_texture2`
  * `ARB_sparse_texture_clamp`
  * `ARB_texture_filter_minmax`
  * `EXT_blend_func_extended`
  * `EXT_clear_texture`
  * `EXT_clip_cull_distance`
  * `EXT_color_buffer_float`
  * `EXT_conservative_depth`
  * `EXT_draw_transform_feedback`
  * `EXT_multisample_compatibility`
  * `EXT_protected_textures`
  * `EXT_shader_group_vote`
  * `EXT_shader_non_constant_global_initializers`
  * `EXT_shader_pixel_local_storage2`
  * `EXT_window_rectangles`
  * `IMG_bindless_texture`
  * `IMG_framebuffer_downsample`
  * `IMG_texture_filter_cubic`
  * `INTEL_conservative_rasterization`
  * `INTEL_framebufer_CMAA`
  * `KHR_texture_compression_astc_sliced_3d`
  * `NV_clip_space_w_scaling`
  * `NV_conservative_raster_dilate`
  * `NV_conservative_raster_pre_snap_triangles`
  * `NV_robustness_video_memory_purge`
  * `NV_shader_atomic_float64`
  * `NV_stereo_view_rendering`
  * `NV_viewport_swizzle`
  * `OES_viewport_array`
  * `OVR_multiview_multisampled_render_to_texture`

0.7.8.1
-------
* Cleaned up the remaining unused import warnings

0.7.8
-----
* Build clean on GHC 8

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
