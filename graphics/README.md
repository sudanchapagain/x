# CIE 1931 XYZ to HEX Converter

This Go implementation converts colors from the CIE 1931 XYZ color space to sRGB
hex representation. The CIE 1931 XYZ color space is device-independent and based
on human vision. This implementation includes an optional chromatic adaptation
transformation from D50 to D65 using the Bradford method, which is a von
Kries-based chromatic adaptation model.