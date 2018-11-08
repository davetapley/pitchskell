#include <stdio.h>
#include <iostream>
#include "opencv2/opencv.hpp"
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"
#include "opencv2/features2d.hpp"
#include "opencv2/highgui.hpp"
#include "opencv2/calib3d.hpp"
#include "opencv2/xfeatures2d.hpp"
using namespace cv;
using namespace cv::xfeatures2d;

/// Global Variables
const int axes_1_slider_max = 600;
int axes_1_slider;

const int axes_2_slider_max = 800;
int axes_2_slider;

const int angle_max = 360;
int angle_slider;

const int start_max = 360;
int start_slider;

const int end_max = 360;
int end_slider;

Mat canvas(600, 800, CV_8UC3, Scalar(255, 255, 255));

/**
 * @function on_trackbar
 * @brief Callback for trackbar
 */
void on_trackbar( int, void* )
{
  int thickness = -1;
  int lineType = 8;

  int angle = 0;
  int w = 600;

  canvas.setTo(Scalar(255, 255, 255));

  char debug[255];
  sprintf( debug, "Axes 1, %d \
      Axes 2, %d \
      Angle, %d \
      Start, %d \
      End, %d \
      diff, %d \
      ", axes_1_slider,
      axes_2_slider,
      angle_slider,
      start_slider,
      end_slider,
      end_slider - start_slider
      );

  putText(canvas, debug, Point(20,20), FONT_HERSHEY_PLAIN, 1, Scalar(0,0,0));

  line( canvas,
      Point( w/2.0, w/2.0 ),
      Point( (w/2.0) + 400, (w/2.0) + 400),
      Scalar( 0, 0, 255 ),
      2,
      lineType );

  line( canvas,
      Point( w/2.0, w/2.0 ),
      Point( (w/2.0) - 400, (w/2.0) + 400),
      Scalar( 0, 0, 255 ),
      2,
      lineType );

  ellipse( canvas,
      Point( w/2.0, w/2.0 ),
      Size( axes_1_slider, axes_2_slider ),
      angle_slider,
      start_slider,
      end_slider,
      Scalar( 255, 0, 0 ),
      thickness,
      lineType );
  imshow( "Canvas", canvas );
}

int main( int argc, char** argv )
{
  /// Initialize values
  axes_1_slider = 100;
  axes_2_slider = 102;
  angle_slider = 0;
  start_slider = 45;
  end_slider = 135;

  /// Create Windows
  namedWindow("Canvas", 1);

  createTrackbar( "Axes 1", "Canvas", &axes_1_slider, axes_1_slider_max, on_trackbar );
  createTrackbar( "Axes 2", "Canvas", &axes_2_slider, axes_2_slider_max, on_trackbar );
  createTrackbar( "Angle", "Canvas", &angle_slider, angle_max, on_trackbar );
  createTrackbar( "Start", "Canvas", &start_slider, start_max, on_trackbar );
  createTrackbar( "End", "Canvas", &end_slider, end_max, on_trackbar );

  /// Show some stuff
  on_trackbar(axes_1_slider, 0 );

  /// Wait until user press some key
  waitKey(0);
  return 0;
}
