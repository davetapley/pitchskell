#include <stdio.h>
#include <iostream>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"
#include "opencv2/features2d.hpp"
#include "opencv2/highgui.hpp"
#include "opencv2/calib3d.hpp"
#include "opencv2/xfeatures2d.hpp"
using namespace cv;
using namespace cv::xfeatures2d;
void readme();
/* @function main */
int main( int argc, char** argv )
{
  // READ FILES
  if( !(argc == 3 || argc == 4 ))
  { readme(); return -1; }
  Mat img_object = imread( argv[1]);
  Mat img_scene = imread( argv[2]);
  if( !img_object.data || !img_scene.data )
  { std::cout<< " --(!) Error reading images " << std::endl; return -1; }

  // CREATE DETECTOR
  Ptr<SIFT> detector = SIFT::create();
  std::vector<KeyPoint> keypoints_object, keypoints_scene;
  Mat descriptors_object, descriptors_scene;

  // DETECT KEY POINTS AND DESCRIPTORS
  detector->detectAndCompute( img_object, Mat(), keypoints_object, descriptors_object );
  detector->detectAndCompute( img_scene, Mat(), keypoints_scene, descriptors_scene );

  if ( descriptors_object.empty() ) {
     cvError(0,"MatchFinder","object descriptor empty",__FILE__,__LINE__);
  }
  if ( descriptors_scene.empty() ) {
     cvError(0,"MatchFinder","scene descriptor empty",__FILE__,__LINE__);
  }

  // MATCH
  FlannBasedMatcher matcher;
  std::vector< DMatch > matches;
  matcher.match( descriptors_object, descriptors_scene, matches );
  double max_dist = 0; double min_dist = 100;

  for( int i = 0; i < descriptors_object.rows; i++ )
  { double dist = matches[i].distance;
    if( dist < min_dist ) min_dist = dist;
    if( dist > max_dist ) max_dist = dist;
  }
  printf("-- Max dist : %f \n", max_dist );
  printf("-- Min dist : %f \n", min_dist );
  printf("-- Match count : %i \n", descriptors_object.rows );

  // DRAW MATCHES
  Mat img_matches;
  drawMatches( img_object, keypoints_object, img_scene, keypoints_scene,
               matches, img_matches, Scalar::all(-1), Scalar::all(-1),
               std::vector<char>());

  std::vector<Point2f> obj;
  std::vector<Point2f> scene;
  for( size_t i = 0; i < matches.size(); i++ )
  {
    obj.push_back( keypoints_object[ matches[i].queryIdx ].pt );
    scene.push_back( keypoints_scene[ matches[i].trainIdx ].pt );
  }

  Mat H = findHomography( obj, scene, RANSAC );
  if ( H.empty() ) {
     cvError(0,"MatchFinder","no no homography",__FILE__,__LINE__);
  }

  std::vector<Point2f> obj_corners(4);
  obj_corners[0] = cvPoint(0,0);
  obj_corners[1] = cvPoint( img_object.cols, 0 );
  obj_corners[2] = cvPoint( img_object.cols, img_object.rows );
  obj_corners[3] = cvPoint( 0, img_object.rows );
  std::vector<Point2f> scene_corners(4);
  perspectiveTransform( obj_corners, scene_corners, H);

  line( img_matches, scene_corners[0] + Point2f( img_object.cols, 0), scene_corners[1] + Point2f( img_object.cols, 0), Scalar(0, 255, 0), 4 );
  line( img_matches, scene_corners[1] + Point2f( img_object.cols, 0), scene_corners[2] + Point2f( img_object.cols, 0), Scalar( 0, 255, 0), 4 );
  line( img_matches, scene_corners[2] + Point2f( img_object.cols, 0), scene_corners[3] + Point2f( img_object.cols, 0), Scalar( 0, 255, 0), 4 );
  line( img_matches, scene_corners[3] + Point2f( img_object.cols, 0), scene_corners[0] + Point2f( img_object.cols, 0), Scalar( 0, 255, 0), 4 );

  if(argc == 3 ) {
    imshow( "Good Matches & Object detection", img_matches );
    waitKey(0);
  } else {
    imwrite(argv[3], img_matches);
  }
  return 0;
}

void readme()
{ std::cout << " Usage: ./SIFT_descriptor <img1> <img2>" << std::endl; }
