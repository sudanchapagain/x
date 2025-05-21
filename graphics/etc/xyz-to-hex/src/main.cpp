#include <algorithm>
#include <array>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <format>
#include <string>

constexpr const char* CIE1931XYZ = "CIE1931XYZ";

/**
 * @brief Normalizes the XYZ color values to a white reference of D65.
 * @param xyz A reference to an array of three floating point values representing the XYZ color space.
 */
inline void normalizeXYZ(std::array<float, 3>& xyz) {
  constexpr std::array<float, 3> whiteRef{0.9505f, 1.0f, 1.089f};
  for (size_t i = 0; i < 3; ++i) {
    xyz[i] /= whiteRef[i];
  }
}

/**
 * @brief Adapts the XYZ color space from D50 to D65 using a chromatic adaptation matrix.
 *
 * chromatic adaptation transformation using the Bradford transform matrix and
 * its inverse.
 * 
 * @param xyz A reference to an array of three floating point values representing the XYZ color space.
 */
inline void adaptD50toD65(std::array<float, 3>& xyz) {
  constexpr std::array<std::array<float, 3>, 3> m{{
    {0.8951f, 0.2664f, -0.1614f},
    {-0.7502f, 1.7135f, 0.0367f},
    {0.0389f, -0.0685f, 1.0296f}}};

  constexpr std::array<std::array<float, 3>, 3> mInv{{
    {0.986993f, -0.147054f, 0.159963f},
    {0.432305f, 0.518360f, 0.049291f},
    {-0.008530f, 0.040043f, 0.968487f}}};

  float l = m[0][0] * xyz[0] + m[0][1] * xyz[1] + m[0][2] * xyz[2];
  float mVal = m[1][0] * xyz[0] + m[1][1] * xyz[1] + m[1][2] * xyz[2];
  float s = m[2][0] * xyz[0] + m[2][1] * xyz[1] + m[2][2] * xyz[2];

  l *= 0.9642f / 0.9505f;
  s *= 0.8251f / 1.089f;

  xyz[0] = mInv[0][0] * l + mInv[0][1] * mVal + mInv[0][2] * s;
  xyz[1] = mInv[1][0] * l + mInv[1][1] * mVal + mInv[1][2] * s;
  xyz[2] = mInv[2][0] * l + mInv[2][1] * mVal + mInv[2][2] * s;
}

/**
 * @brief Converts XYZ color values to linear RGB values.
 * 
 * XYZ color space values to RGB color space using a standard transformation
 * matrix.
 * 
 * @param xyz A reference to an array of three floating point values representing the XYZ color space.
 */
inline void xyzToLinearRGB(std::array<float, 3>& xyz) {
  float x = xyz[0], y = xyz[1], z = xyz[2];

  xyz[0] = 3.2406f * x - 1.5372f * y - 0.4986f * z;
  xyz[1] = -0.9689f * x + 1.8758f * y + 0.0415f * z;
  xyz[2] = 0.0557f * x - 0.2040f * y + 1.0570f * z;
}

/**
 * @brief Clamps RGB values to the [0, 1] range and scales them to fit within the color gamut.
 * @param rgb A reference to an array of three floating point values representing the RGB color.
 */
inline void clampAndScaleToGamut(std::array<float, 3>& rgb) {
  for (auto& c : rgb) c = std::max(0.0f, c);

  float maxVal = std::max({rgb[0], rgb[1], rgb[2]});

  if (maxVal > 1.0f) {
    for (auto& c : rgb) c /= maxVal;
  }
}

/**
 * @brief Applies gamma correction to RGB values.
 * @param rgb A reference to an array of three floating point values representing the RGB color.
 */
inline void applyGammaCorrection(std::array<float, 3>& rgb) {
  for (auto& c : rgb) {
    c = (c <= 0.0031308f) ? 12.92f * c : 1.055f * std::pow(c, 1.0f / 2.4f) - 0.055f;
  }
}

/**
 * @brief Converts CIE XYZ color values to a hexadecimal RGB string.
 * 
 * @param x The X component of the XYZ color space.
 * @param y The Y component of the XYZ color space.
 * @param z The Z component of the XYZ color space.
 * @param type The color space name (default is "CIE1931XYZ").
 * @param adapt If true, performs the adaptation from D50 to D65 (default is true).
 * 
 * @return A string representing the RGB color in hexadecimal format (e.g., "#RRGGBB").
 * 
 * @note This function assumes the input color is in CIE1931XYZ format by default. If a different 
 *       color space is used, the function will return "#000000" as a fallback.
 */
inline std::string cieToHex(float x, float y, float z, const char* type = CIE1931XYZ, bool adapt = true) {
  if (std::strcmp(type, CIE1931XYZ) != 0) return "#000000";

  std::array<float, 3> color{x, y, z};

  normalizeXYZ(color);

  if (adapt) adaptD50toD65(color);

  xyzToLinearRGB(color);
  clampAndScaleToGamut(color);
  applyGammaCorrection(color);

  auto toInt = [](float c) -> int {
    return std::clamp(static_cast<int>(std::round(c * 255.0f)), 0, 255);
  };

  char buffer[8];
  std::snprintf(buffer, sizeof(buffer), "#%02X%02X%02X", toInt(color[0]), toInt(color[1]), toInt(color[2]));
  return std::string(buffer);
}

int main() {
  std::string hex = cieToHex(0.3f, 0.4f, 0.2f);
  std::puts(hex.c_str());
  return 0;
}
