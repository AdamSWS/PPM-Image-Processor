//
// F# image processing functions.
//
// Program to implement 5 of the following processing functions for .ppm images
//   - grayscale, threshold, flip horizontal, edge detect, right-rotate 90
//
// Adam Shaar, University of Illinois at Chicago, 3/30/23
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // convert image pixels into grayscale by multiplying the weights 29.9% for red, 58.7% for green, and 11.4% for blue
    let applyGrayscale (r:int, g:int, b:int) =
      let newShade = int(float r * 0.299 + float g * 0.587 + float b * 0.114)
      (newShade, newShade, newShade)
    // update the rows of the image to apply the effect to each row 
    let updateRows (rows:(int*int*int) list list) =
      List.map (fun row -> List.map applyGrayscale row) rows

    updateRows image

  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // convert the image pixels' rgb values to 0 if its less than the threshold input or to 255 if it is greater than threshold input
    let applyThreshold (r:int, g:int, b:int) =
      let t (x:int) =
        match x > threshold with
        | true -> depth
        | false -> 0
      (t r, t g, t b)
    // update the rows of the image to apply the effects to each row
    let updateRows (rows:(int*int*int) list list) =
       List.map (fun row -> List.map applyThreshold row) rows
  
    updateRows image

  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // uses higher order functions to flip the pixels of the image from left -> right
    List.map List.rev image


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    // distance function to calculate the "color difference" between two pixels
    let colorDistance (r1:int, g1:int, b1:int) (r2:int, g2:int, b2:int) =
      let dist = sqrt (float ((r1 - r2) * (r1 - r2) + (g1 - g2) * (g1 - g2) + (b1 - b2) * (b1 - b2)))
      dist
    // find if a pixel is an image from its color distance with its right neighbor and below neighbor
    let isEdge (x:int) (y:int) =
      let pixel = List.nth (List.nth image y) x
      // grab right pixel for color distancing if it exists.
      let rightPixel = 
        match x < width - 1 with
        | true -> List.nth (List.nth image y) (x + 1)
        | false -> pixel
      // grab below pixel for color distancing if it exists.
      let belowPixel = 
        match y < height - 1 with
        | true -> List.nth (List.nth image (y + 1)) x
        | false -> pixel
      // get the color distances between the pixel and its neightbors
      let dist1 = colorDistance pixel rightPixel
      let dist2 = colorDistance pixel belowPixel
      // logical or statement to determine if the original pixel has a color distance higher than the inputted threshhold
      dist1 > float threshold || dist2 > float threshold
    // sets the edge pixels colors to black and sets the non-edges to white
    let edgeColor x y =
      match isEdge x y with
      | true -> (0, 0, 0)
      | false -> (255, 255, 255)
    // update the image rows
    let createRow y =
      List.init (width - 1) (fun x -> edgeColor x y)

    let updatedImage = 
      List.init (height - 1) createRow

    updatedImage


  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) =
    // saving the width to make the newHeight
    let newHeight = width
    // creating a new row with a reversed image
    let newRow (col:int) =
        let revImage = List.rev image
        List.map (fun row -> List.item col row) revImage
    // creating new image
    let newImage = List.init newHeight newRow
  
    newImage

