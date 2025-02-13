package main

import (
	"errors"
	"fmt"
	"math"
)

/* supported CIE color spaces */
type CIEType string

const CIE1931XYZ CIEType = "CIE1931XYZ"

// reference white values for D65
const Xr, Yr, Zr = 0.9505, 1.0000, 1.0890

/* normalize XYZ values based on D65 reference white */
func normalizeXYZ(x, y, z float64) (float64, float64, float64) {
	if y == 0 {
		return 0, 0, 0
	}
	return x / Xr, y / Yr, z / Zr
}

/* chromatic adaptation (D50 -> D65) */
func adaptD50toD65(x, y, z float64, applyAdaptation bool) (float64, float64, float64) {
	if !applyAdaptation {
		return x, y, z
	}

	// Bradford Transform : D50 -> D65
	m := [3][3]float64{
		{0.8951, 0.2664, -0.1614},
		{-0.7502, 1.7135, 0.0367},
		{0.0389, -0.0685, 1.0296},
	}
	mInv := [3][3]float64{
		{0.986993, -0.147054, 0.159963},
		{0.432305, 0.518360, 0.049291},
		{-0.008530, 0.040043, 0.968487},
	}

	// Transform to LMS space
	l := m[0][0]*x + m[0][1]*y + m[0][2]*z
	mm := m[1][0]*x + m[1][1]*y + m[1][2]*z
	s := m[2][0]*x + m[2][1]*y + m[2][2]*z

	// Scale for D65 white point
	l *= 0.9642 / 0.9505
	s *= 0.8251 / 1.0890

	// Convert back to XYZ
	xD65 := mInv[0][0]*l + mInv[0][1]*mm + mInv[0][2]*s
	yD65 := mInv[1][0]*l + mInv[1][1]*mm + mInv[1][2]*s
	zD65 := mInv[2][0]*l + mInv[2][1]*mm + mInv[2][2]*s

	return xD65, yD65, zD65
}

/* XYZ to sRGB in hex representation */
func cieToHex(x, y, z float64, cieType CIEType, applyAdaptation bool) (string, error) {
	if cieType != CIE1931XYZ {
		return "", errors.New("unsupported CIE type")
	}

	// normalize input
	x, y, z = normalizeXYZ(x, y, z)

	// apply chromatic adaptation if needed
	x, y, z = adaptD50toD65(x, y, z, applyAdaptation)

	// XYZ to linear sRGB
	r := 3.2406*x - 1.5372*y - 0.4986*z
	g := -0.9689*x + 1.8758*y + 0.0415*z
	b := 0.0557*x - 0.2040*y + 1.0570*z

	// clamp negatives & scale
	r, g, b = math.Max(0, r), math.Max(0, g), math.Max(0, b)
	r, g, b = scaleToGamut(r, g, b)

	// Gamma correction
	r, g, b = gammaCorrect(r), gammaCorrect(g), gammaCorrect(b)

	// Convert to 8-bit integer values
	r8 := clampInt(int(math.Round(r * 255)))
	g8 := clampInt(int(math.Round(g * 255)))
	b8 := clampInt(int(math.Round(b * 255)))

	return fmt.Sprintf("#%02X%02X%02X", r8, g8, b8), nil
}

/* Scale RGB values if out of gamut */
func scaleToGamut(r, g, b float64) (float64, float64, float64) {
	maxVal := math.Max(r, math.Max(g, b))
	if maxVal > 1 {
		r /= maxVal
		g /= maxVal
		b /= maxVal
	}
	return r, g, b
}

/* Apply sRGB gamma correction */
func gammaCorrect(c float64) float64 {
	if c <= 0.0031308 {
		return 12.92 * c
	}
	return 1.055*math.Pow(c, 1.0/2.4) - 0.055
}

/* Ensure values are within the 0-255 range */
func clampInt(v int) int {
	if v < 0 {
		return 0
	}
	if v > 255 {
		return 255
	}
	return v
}

// func main() {
// 	// Example usage:
// 	// The last parameter indicates whether to apply chromatic adaptation.
// 	// Pass 'true' if your input XYZ is based on D50 (and you need adaptation to D65),
// 	// or 'false' if the input is already in D65.
// 	hexColor, err := cieToHex(0.3, 0.4, 0.2, CIE1931XYZ, true)
// 	if err != nil {
// 		fmt.Println("Error:", err)
// 	} else {
// 		fmt.Println("Hex Color:", hexColor)
// 	}
// }