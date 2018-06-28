# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.10

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/Cellar/cmake/3.10.1/bin/cmake

# The command to remove a file.
RM = /usr/local/Cellar/cmake/3.10.1/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/dtapley/dev/pitchskell/opencv-sandbox

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/dtapley/dev/pitchskell/opencv-sandbox

# Include any dependencies generated for this target.
include CMakeFiles/Homography.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Homography.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Homography.dir/flags.make

CMakeFiles/Homography.dir/homography.cpp.o: CMakeFiles/Homography.dir/flags.make
CMakeFiles/Homography.dir/homography.cpp.o: homography.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/dtapley/dev/pitchskell/opencv-sandbox/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/Homography.dir/homography.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Homography.dir/homography.cpp.o -c /Users/dtapley/dev/pitchskell/opencv-sandbox/homography.cpp

CMakeFiles/Homography.dir/homography.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Homography.dir/homography.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/dtapley/dev/pitchskell/opencv-sandbox/homography.cpp > CMakeFiles/Homography.dir/homography.cpp.i

CMakeFiles/Homography.dir/homography.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Homography.dir/homography.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/dtapley/dev/pitchskell/opencv-sandbox/homography.cpp -o CMakeFiles/Homography.dir/homography.cpp.s

CMakeFiles/Homography.dir/homography.cpp.o.requires:

.PHONY : CMakeFiles/Homography.dir/homography.cpp.o.requires

CMakeFiles/Homography.dir/homography.cpp.o.provides: CMakeFiles/Homography.dir/homography.cpp.o.requires
	$(MAKE) -f CMakeFiles/Homography.dir/build.make CMakeFiles/Homography.dir/homography.cpp.o.provides.build
.PHONY : CMakeFiles/Homography.dir/homography.cpp.o.provides

CMakeFiles/Homography.dir/homography.cpp.o.provides.build: CMakeFiles/Homography.dir/homography.cpp.o


# Object files for target Homography
Homography_OBJECTS = \
"CMakeFiles/Homography.dir/homography.cpp.o"

# External object files for target Homography
Homography_EXTERNAL_OBJECTS =

Homography: CMakeFiles/Homography.dir/homography.cpp.o
Homography: CMakeFiles/Homography.dir/build.make
Homography: /usr/local/lib/libopencv_stitching.3.4.0.dylib
Homography: /usr/local/lib/libopencv_superres.3.4.0.dylib
Homography: /usr/local/lib/libopencv_videostab.3.4.0.dylib
Homography: /usr/local/lib/libopencv_aruco.3.4.0.dylib
Homography: /usr/local/lib/libopencv_bgsegm.3.4.0.dylib
Homography: /usr/local/lib/libopencv_bioinspired.3.4.0.dylib
Homography: /usr/local/lib/libopencv_ccalib.3.4.0.dylib
Homography: /usr/local/lib/libopencv_dpm.3.4.0.dylib
Homography: /usr/local/lib/libopencv_face.3.4.0.dylib
Homography: /usr/local/lib/libopencv_fuzzy.3.4.0.dylib
Homography: /usr/local/lib/libopencv_img_hash.3.4.0.dylib
Homography: /usr/local/lib/libopencv_line_descriptor.3.4.0.dylib
Homography: /usr/local/lib/libopencv_optflow.3.4.0.dylib
Homography: /usr/local/lib/libopencv_reg.3.4.0.dylib
Homography: /usr/local/lib/libopencv_rgbd.3.4.0.dylib
Homography: /usr/local/lib/libopencv_saliency.3.4.0.dylib
Homography: /usr/local/lib/libopencv_stereo.3.4.0.dylib
Homography: /usr/local/lib/libopencv_structured_light.3.4.0.dylib
Homography: /usr/local/lib/libopencv_surface_matching.3.4.0.dylib
Homography: /usr/local/lib/libopencv_tracking.3.4.0.dylib
Homography: /usr/local/lib/libopencv_xfeatures2d.3.4.0.dylib
Homography: /usr/local/lib/libopencv_ximgproc.3.4.0.dylib
Homography: /usr/local/lib/libopencv_xobjdetect.3.4.0.dylib
Homography: /usr/local/lib/libopencv_xphoto.3.4.0.dylib
Homography: /usr/local/lib/libopencv_shape.3.4.0.dylib
Homography: /usr/local/lib/libopencv_photo.3.4.0.dylib
Homography: /usr/local/lib/libopencv_datasets.3.4.0.dylib
Homography: /usr/local/lib/libopencv_plot.3.4.0.dylib
Homography: /usr/local/lib/libopencv_text.3.4.0.dylib
Homography: /usr/local/lib/libopencv_dnn.3.4.0.dylib
Homography: /usr/local/lib/libopencv_ml.3.4.0.dylib
Homography: /usr/local/lib/libopencv_video.3.4.0.dylib
Homography: /usr/local/lib/libopencv_calib3d.3.4.0.dylib
Homography: /usr/local/lib/libopencv_features2d.3.4.0.dylib
Homography: /usr/local/lib/libopencv_highgui.3.4.0.dylib
Homography: /usr/local/lib/libopencv_videoio.3.4.0.dylib
Homography: /usr/local/lib/libopencv_phase_unwrapping.3.4.0.dylib
Homography: /usr/local/lib/libopencv_flann.3.4.0.dylib
Homography: /usr/local/lib/libopencv_imgcodecs.3.4.0.dylib
Homography: /usr/local/lib/libopencv_objdetect.3.4.0.dylib
Homography: /usr/local/lib/libopencv_imgproc.3.4.0.dylib
Homography: /usr/local/lib/libopencv_core.3.4.0.dylib
Homography: CMakeFiles/Homography.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/dtapley/dev/pitchskell/opencv-sandbox/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable Homography"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Homography.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Homography.dir/build: Homography

.PHONY : CMakeFiles/Homography.dir/build

CMakeFiles/Homography.dir/requires: CMakeFiles/Homography.dir/homography.cpp.o.requires

.PHONY : CMakeFiles/Homography.dir/requires

CMakeFiles/Homography.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Homography.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Homography.dir/clean

CMakeFiles/Homography.dir/depend:
	cd /Users/dtapley/dev/pitchskell/opencv-sandbox && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/dtapley/dev/pitchskell/opencv-sandbox /Users/dtapley/dev/pitchskell/opencv-sandbox /Users/dtapley/dev/pitchskell/opencv-sandbox /Users/dtapley/dev/pitchskell/opencv-sandbox /Users/dtapley/dev/pitchskell/opencv-sandbox/CMakeFiles/Homography.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Homography.dir/depend
