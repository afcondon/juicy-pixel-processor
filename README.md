# Example of scaling and producing thumbnail images using JuicyPixels

This is a standalone example, very vanilla, almost just a gist, but it builds and when built (`stack build; stack install`) and run (for example, `juicy-exe 40 2000 image.jpg`) will scale the given image to have longest edge equal to given param (2000) and quality (40) as well as producing a thumbnail in a separate sub-directory `thumbs`.
