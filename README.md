# Digit

**Digit** is a [digitizer](https://en.wikipedia.org/wiki/Digitization), a tool to extract the original data from images representing plots. According to [Wikipedia](https://en.wikipedia.org/wiki/Digitizer_(disambiguation)), *a digitizer is a machine that converts an analog object, image or signal into a digital (i.e. computer-readable) format*. Quite often (e.g., in scientific papers), information is represented as plots, which are convenient for humans but useless for computers, which need the raw data. Unfortunately, both forms of representation are rarely available at the same time, so a tool is needed to extract data from plots. **Digit** is such a tool.

Digitization is a four-step process:

1. **Import the plot image**. **Digit** can accept most image formats (e.g., GIF, BMP, PNG, JPEG and TIFF), and the image can be loaded from a file or pasted from the clipboard.
2. **Define the axis system**. **Digit** can handle plots in Cartesian or polar coordinates, axes with linear, logarithmic, and reciprocal scales, and non-orthogonal axes. Additionally, **Digit** can deal with skewed, tilted or distorted plots, and multiple plots in one image.
3. **Digitize the data**. **Digit** has algorithms for automatic data digitization by detecting lines or colors and for manual digitization via markers that can be added with a mouse click.
4. **Export the data values**. In **Digit** the digitized data can be saved in CSV format or copied to the clipboard, to use them in any other application.

## Usage

**Digit**'s philosophy is to present the user with several tools with reasonable defaults, and get out of the way. For basic digitization, the only mandatory steps are to define (i) the scale and (ii) the curve color. In most cases this is sufficient, but in more complex cases some user tweaks may be required.

Among the options available to the user for digitization are *grid removal*, for the rare cases in which the grid interferes with the digitalization and the *plot box* (an area containing the data) definition, to avoid false detections. The most important options for processing the digitized data are *resampling*, to reduce the number of data, and *smoothing*, to remove the noise introduced by digitization. There are also tools to adjust digitized curves, convert them to scatter plots, or remove spurious data.

## Features

**Digit** has numerous features, among which we can mention:

* Can import almost most common image file formats, including GIF, BMP, PNG, JPEG and TIFF.
* Images can be pasted from the clipboard.
* Plots can be zoomed, cropped and rotated.
* Can handle skewed, tilted or distorted plots.
* Can handle multiple plots in one image.
* Can handle plots in Cartesian or polar coordinates.
* Can handle axes with linear, logarithmic, and reciprocal scales, and non-orthogonal axes.
* Tool for grid removal.
* Automatic digitization of line and scatter plots.
* Manual digitization via markers that can be added with a mouse click.
* Markers can be used also to guide the automatic digitization.
* Multiple data sets can be defined.
* Automatic sorting of data values.
* Tools for resampling ans smoothing data values.
* Data can be exported as CSV or via clipboard.
* Digitizations are saved as XML files to facilitate interoperability with similar programs.
* Available for Windows, Mac, Linux, and can be compiled on any OS with support for [Lazarus](https://www.lazarus-ide.org/) and [FPC](https://www.freepascal.org/).
