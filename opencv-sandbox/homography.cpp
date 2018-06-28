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
  if( argc != 3 )
  { readme(); return -1; }
  Mat img_object = imread( argv[1], IMREAD_GRAYSCALE );
  Mat img_scene = imread( argv[2], IMREAD_GRAYSCALE );
  if( !img_object.data || !img_scene.data )
  { std::cout<< " --(!) Error reading images " << std::endl; return -1; }
  //-- Step 1: Detect the keypoints and extract descriptors using SIFT
  int minHessian = 300;
  Ptr<SIFT> detector = SIFT::create();
  std::vector<KeyPoint> keypoints_object, keypoints_scene;
  Mat descriptors_object, descriptors_scene;

  //if(descriptors_object.type()!=CV_32F) {
  //  descriptors_object.convertTo(descriptors_object, CV_32F);
  //}
  //if(descriptors_scene.type()!=CV_32F) {
  //    descriptors_scene.convertTo(descriptors_scene, CV_32F);
  //}

  detector->detectAndCompute( img_object, Mat(), keypoints_object, descriptors_object );
  detector->detectAndCompute( img_scene, Mat(), keypoints_scene, descriptors_scene );

  if ( descriptors_object.empty() ) {
     cvError(0,"MatchFinder","object descriptor empty",__FILE__,__LINE__);
  }
  if ( descriptors_scene.empty() ) {
     cvError(0,"MatchFinder","scene descriptor empty",__FILE__,__LINE__);
  }

  //-- Step 2: Matching descriptor vectors using FLANN matcher


  FlannBasedMatcher matcher;
  std::vector< DMatch > matches;
  matcher.match( descriptors_object, descriptors_scene, matches );
  double max_dist = 0; double min_dist = 100;
  //-- Quick calculation of max and min distances between keypoints
  for( int i = 0; i < descriptors_object.rows; i++ )
  { double dist = matches[i].distance;
    if( dist < min_dist ) min_dist = dist;
    if( dist > max_dist ) max_dist = dist;
  }
  printf("-- Max dist : %f \n", max_dist );
  printf("-- Min dist : %f \n", min_dist );
  //-- Draw only "good" matches (i.e. whose distance is less than 3*min_dist )
  std::vector< DMatch > good_matches;
  int good_match_count = 0;
  for( int i = 0; i < descriptors_object.rows; i++ )
  {
    //if( matches[i].distance < 10*min_dist ) {
      good_matches.push_back( matches[i]);
      good_match_count++;
    //}
  }
  printf("-- Good match count : %i \n", good_match_count );
  Mat img_matches;
  drawMatches( img_object, keypoints_object, img_scene, keypoints_scene,
               good_matches, img_matches, Scalar::all(-1), Scalar::all(-1),
               std::vector<char>(), DrawMatchesFlags::NOT_DRAW_SINGLE_POINTS );
  //-- Localize the object
  std::vector<Point2f> obj;
  std::vector<Point2f> scene;
  for( size_t i = 0; i < good_matches.size(); i++ )
  {
    //-- Get the keypoints from the good matches
    obj.push_back( keypoints_object[ good_matches[i].queryIdx ].pt );
    scene.push_back( keypoints_scene[ good_matches[i].trainIdx ].pt );
  }
  // Mat H = findHomography( obj, scene, RANSAC );
  // if ( H.empty() ) {
  //    cvError(0,"MatchFinder","no no homography",__FILE__,__LINE__);
  // }
  // //-- Get the corners from the image_1 ( the object to be "detected" )
  // std::vector<Point2f> obj_corners(4);
  // obj_corners[0] = cvPoint(0,0);
  // obj_corners[1] = cvPoint( img_object.cols, 0 );
  // obj_corners[2] = cvPoint( img_object.cols, img_object.rows );
  // obj_corners[3] = cvPoint( 0, img_object.rows );
  // std::vector<Point2f> scene_corners(4);
  // perspectiveTransform( obj_corners, scene_corners, H);
  // //-- Draw lines between the corners (the mapped object in the scene - image_2 )
  // line( img_matches, scene_corners[0] + Point2f( img_object.cols, 0), scene_corners[1] + Point2f( img_object.cols, 0), Scalar(0, 255, 0), 4 );
  // line( img_matches, scene_corners[1] + Point2f( img_object.cols, 0), scene_corners[2] + Point2f( img_object.cols, 0), Scalar( 0, 255, 0), 4 );
  // line( img_matches, scene_corners[2] + Point2f( img_object.cols, 0), scene_corners[3] + Point2f( img_object.cols, 0), Scalar( 0, 255, 0), 4 );
  // line( img_matches, scene_corners[3] + Point2f( img_object.cols, 0), scene_corners[0] + Point2f( img_object.cols, 0), Scalar( 0, 255, 0), 4 );
  // //-- Show detected matches
  // imshow( "Good Matches & Object detection", img_matches );
  waitKey(0);
  return 0;
  }
  /* @function readme */
  void readme()
  { std::cout << " Usage: ./SIFT_descriptor <img1> <img2>" << std::endl; }