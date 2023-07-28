# Sums a list of numbers and returns the total.
proc ladd {list} {
  set total 0
  foreach {el} $list {
    incr total $el
  }
  return $total
}

# Searches an image (eg., a 2D array) for a given term,
# and returns the number of occurrences.
proc searchImage {image term} {
  set counts [
    lmap {pixelRow} $image {
      llength [lsearch -inline -all $pixelRow $term] 
    }
  ]

  return [ladd $counts]
}

# Finds the image with the fewest number of zeroes
# present among all the layers.
proc imageWithFewestZero {layers} {
  set minZeroCount Inf
  set minZeroImage -1

  foreach {image} $layers {
    set zeroCount [searchImage $image 0]
    if {$zeroCount < $minZeroCount} {
      set minZeroCount $zeroCount
      set minZeroImage $image
    }
  }

  return $minZeroImage
}

# Initializes a 2D array with a given width and height
# by filling it in with zeroes.
proc initImage {w h} {
  set image [lrepeat $h [lrepeat $w 0]]
  return $image
}

# Set imageData. Each integer is a pixel.
set reader [open "data.txt" r]
set imageData [lindex [split [read $reader] ","] 0]
close $reader

set pixels [split $imageData ""]
set width 25
set height 6
set image [initImage $width $height]

set layers {}
set x 0
set y 0

# Creates layers of images from a list of pixels.
for {set i 0} {$i < [llength $pixels]} {incr i} {
  # If we have reached the end of a row, reset our X position
  # to zero and increase the row (eg., Y position) by one.
  if {$i % $width == 0 && $i != 0} {
    incr y
    set x 0
  }

  # If we have reached the end of a layer (eg., the Y value is
  # beyond the heigh of the image), reset the Y position to
  # zero, append the image to our layers list, and make a new
  # image (eg., a 2D array).
  if {$y == $height} {
    lappend layers $image
    set image [initImage $width $height]
    set y 0
  }

  set pixel [lindex $pixels $i]
  lset image $y $x $pixel

  # Increase the X position for the next pixel.
  incr x
}

# To make sure the image wasn't corrupted during transmission, the Elves
# would like you to find the layer that contains the fewest 0 digits.
set fewestZeroes [imageWithFewestZero $layers]

# On that layer, what is the number of 1 digits multiplied by the number
# of 2 digits.
set onesCount [searchImage $fewestZeroes 1]
set twosCount [searchImage $fewestZeroes 2]
set product [expr {$onesCount * $twosCount}]
puts "Product: $product"
