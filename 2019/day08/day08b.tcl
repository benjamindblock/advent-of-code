# Initializes a 2D array with a given width and height
# by filling it in with zeroes.
proc initImage {w h {fill 0}} {
  set image [lrepeat $h [lrepeat $w $fill]]
  return $image
}

# Read and set imageData. Each integer is a pixel.
set reader [open "data.txt" r]
set imageData [lindex [split [read $reader] ","] 0]
close $reader

# Basic pixel input and image dimensions.
set pixels [split $imageData ""]
set width 25
set height 6
set image [initImage $width $height]

# Setup colors for the final rendering.
set black 0
set white 1
set transparent 2

# Vars to track the construction of the image layers.
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

# Construct the rendered image. We start with a default image
# where all pixels are transparent and then fill each pixel in
# for the first non-transparent pixel we come across.
set rendered [initImage $width $height 2]
foreach {image} $layers {
  for {set x 0} {$x < $width} {incr x} {
    for {set y 0} {$y < $height} {incr y} {
      set imagePixel [lindex $image $y $x]
      set renderedPixel [lindex $rendered $y $x]

      if {$renderedPixel == $transparent} {
        lset rendered $y $x $imagePixel
      }
    }
  }
}

# Print the final rendered image, using ANSI escape
# sequences to print colors to the terminal.
foreach {row} $rendered {
  foreach {pixel} $row {
    if {$pixel == $black} {
      # 37 is ANSI for black.
      puts -nonewline "\033\[1;30m";
      puts -nonewline "$pixel "
    } else {
      # 37 is ANSI for white.
      puts -nonewline "\033\[1;37m";
      puts -nonewline "$pixel "
    }
  }

  # Print a newline between each row
  puts ""
}

# Reset color at the end.
puts -nonewline "\033\[1;0m";
