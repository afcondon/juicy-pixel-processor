#  Simple batch resizing program using Juicy-Pixels and Juicy-Pixels-extra

This is a standalone example, very vanilla, a bit more than a gist, but it builds and when built (`stack build; stack install`) and run (for example, `juicy-exe --source source-image-dir --destination dest-image-dir`) will process all the images found under the source directory into their resized counterparts in the destination directory. 

  * It scales each image to have longest edge equal to given `--longedge` and `--quality` as well as producing a thumbnail in a separate sub-directory `thumbs`.

  * Resized images are renamed with their dimensions - `foo.jpg`, starting out at, say `2912 × 4368` pixels and being resized to have longest edge be `2000` becomes `foo_1333x2000.jpg`

  * the directory structure underneath the source directory is retained
  
  * thumbnails are always quality 80 and 320px in their longest dimension
  
  * uses optparse-applicative and provides defaults for the size and quality (see command line help below)

```
juicy-exe -- Image processor for Hakyll static web

Usage: juicy-exe [-q|--quality INT] [-l|--longedge INT] (-s|--source SOURCE)
                 (-d|--destination TARGET)
  Scales and compresses images for web, and produces thumbnails

Available options:
  -q,--quality INT         Quality of output JPEG (percentage) (default: 40)
  -l,--longedge INT        Length of longest edge (default: 2000)
  -s,--source SOURCE       Directory where images to be processed are found
  -d,--destination TARGET  Directory to write the resized files and thumbnails
  -h,--help                Show this help text
```
