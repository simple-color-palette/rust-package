#![allow(clippy::tabs_in_doc_comments)]

//! An implementation of the [Simple Color Palette](https://simplecolorpalette.com) format — a minimal JSON-based file format for defining color palettes.
//!
//! A palette contains colors with optional names, and the palette itself can also have a name. Colors are stored in extended sRGB color space (wide gamut). While colors are serialized in their linear form, the API primarily works with non-linear (gamma-corrected) values since these better match human perception and are what most color pickers and design tools use.
//!
//! ```rust
//! use simple_color_palette::{ColorPalette, Color};
//!
//! let palette = ColorPalette::new(
//! 	vec![
//! 		Color::new(1.0, 0.0, 0.0, None, Some("Red".into())),
//! 		Color::new(0.0, 1.0, 0.0, None, Some("Green".into())),
//! 	],
//! 	Some("Traffic Lights".into()),
//! );
//!
//! let path = std::path::Path::new("Traffic Lights.color-palette");
//!
//! // Save palette
//! palette.write_to_file(path);
//!
//! // Load palette
//! let loadedPalette = ColorPalette::read_from_file(path);
//! ```

use serde::{Deserialize, Serialize};
use std::{fs, path::Path};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct ColorPalette {
	/// List of colors in the palette.
	pub colors: Vec<Color>,

	/// Optional name of the palette.
	#[serde(skip_serializing_if = "Option::is_none")]
	pub name: Option<String>,
}

/// Represents a single color in the palette.
///
/// The color components are stored in extended linear sRGB color space, but the API primarily works with non-linear (gamma-corrected) values which better match human perception.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Color {
	/// The color components in extended linear sRGB color space.
	///
	/// For most purposes, you will want to use `components()` instead which returns normal sRGB values,
	/// meaning adjusted for human perception and display on screens.
	#[serde(rename = "components", with = "color_components_as_linear")]
	linear_components: ColorComponents,

	/// Optional name for the color.
	#[serde(skip_serializing_if = "Option::is_none")]
	pub name: Option<String>,
}

/// Color components representing RGB values and opacity.
///
/// Values are stored with full precision internally and rounded to 4 decimal places during serialization and deserialization.
///
/// Opacity is automatically clamped to 0.0...1.0.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ColorComponents {
	pub red: f32,
	pub green: f32,
	pub blue: f32,
	pub opacity: f32,
}

impl ColorPalette {
	/// Creates a new color palette with the specified colors and optional name.
	#[must_use]
	pub const fn new(colors: Vec<Color>, name: Option<String>) -> Self {
		Self { colors, name }
	}

	/// Writes the palette to a file in the Simple Color Palette format.
	///
	/// # Parameters
	/// - `path`: The file path to write the palette to. Use the `.color-palette` file extension.
	///
	/// # Errors
	///
	/// Returns an error if the file cannot be written.
	pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), PaletteError> {
		let json = serde_json::to_string_pretty(self)?;
		fs::write(path, json)?;
		Ok(())
	}

	/// Reads a palette from a file in the Simple Color Palette format.
	///
	/// # Parameters
	/// - `path`: The file path to read the palette from.
	///
	/// # Errors
	///
	/// Returns an error if the file cannot be read or parsed.
	pub fn read_from_file<P: AsRef<Path>>(path: P) -> Result<Self, PaletteError> {
		let data = fs::read_to_string(path)?;
		Ok(serde_json::from_str(&data)?)
	}
}

impl Color {
	/// Creates a color using extended non-linear sRGB components.
	///
	/// - Note: If you don't know the difference between linear and non-linear, this is the initializer you want.
	///
	/// # Parameters
	///	- `red`: Red component.
	///	- `green`: Green component.
	///	- `blue`: Blue component.
	///	- `opacity`: Optional opacity value (0.0...1.0), defaults to 1.0.
	///	- `name`: Optional name for the color.
	#[must_use]
	pub fn new(
		red: f32,
		green: f32,
		blue: f32,
		opacity: Option<f32>,
		name: Option<String>,
	) -> Self {
		Self {
			linear_components: ColorComponents::new(red, green, blue, opacity),
			name,
		}
	}

	/// Creates a color using extended linear sRGB components.
	///
	/// - Note: For most purposes, you will want to use `new()` instead.
	///
	/// # Parameters
	///	- `red`: Linear red component.
	///	- `green`: Linear green component.
	///	- `blue`: Linear blue component.
	///	- `opacity`: Optional opacity value (0.0...1.0), defaults to 1.0.
	///	- `name`: Optional name for the color.
	#[must_use]
	pub fn from_linear(
		red: f32,
		green: f32,
		blue: f32,
		opacity: Option<f32>,
		name: Option<String>,
	) -> Self {
		Self {
			linear_components: ColorComponents::from_linear(red, green, blue, opacity),
			name,
		}
	}

	/// Returns the color components in extended non-linear sRGB color space.
	///
	/// - Note: This is probably what you want. The color components are adjusted for human perception and display on screens.
	#[must_use]
	pub fn components(&self) -> ColorComponents {
		self.linear_components.to_srgb_components()
	}

	/// Returns the color components in extended linear sRGB color space.
	///
	/// - Note: For most purposes, you will want to use `components()` instead which returns normal sRGB values.
	#[must_use]
	pub const fn components_linear(&self) -> &ColorComponents {
		&self.linear_components
	}

	/// Creates a color from a hex string.
	///
	/// Supports the following formats:
	/// - RGB: "#RGB" or "RGB"
	/// - RGBA: "#RGBA" or "RGBA"
	/// - RRGGBB: "#RRGGBB" or "RRGGBB"
	/// - RRGGBBAA: "#RRGGBBAA" or "RRGGBBAA"
	///
	/// The "#" prefix is optional.
	///
	/// # Errors
	/// Returns an error if the string format is invalid.
	pub fn from_hex_str(hex: &str) -> Result<Self, &'static str> {
		if hex.is_empty() {
			return Err("Empty hex string");
		}

		let digits = hex.strip_prefix('#').unwrap_or(hex);

		if !digits.chars().all(|c| c.is_ascii_hexdigit()) {
			return Err("Invalid hex characters");
		}

		fn parse_hex(d: &str) -> Result<(u8, u8, u8, Option<u8>), &'static str> {
			let digit = |i| {
				u8::from_str_radix(&d[i..=i], 16)
					.map(|n| n << 4 | n)
					.map_err(|_| "Invalid hex value")
			};

			let pair = |i| u8::from_str_radix(&d[i..=i + 1], 16).map_err(|_| "Invalid hex value");

			match d.len() {
				3 => Ok((digit(0)?, digit(1)?, digit(2)?, None)),
				4 => Ok((digit(0)?, digit(1)?, digit(2)?, Some(digit(3)?))),
				6 => Ok((pair(0)?, pair(2)?, pair(4)?, None)),
				8 => Ok((pair(0)?, pair(2)?, pair(4)?, Some(pair(6)?))),
				_ => Err("Invalid hex color format"),
			}
		}

		let (r, g, b, a) = parse_hex(digits)?;

		Ok(Self::new(
			f32::from(r) / 255.0,
			f32::from(g) / 255.0,
			f32::from(b) / 255.0,
			a.map(|a| f32::from(a) / 255.0),
			None,
		))
	}

	/// Creates a color from an integer hex value.
	///
	/// Supports the following formats:
	/// - RGB: 0xRGB (12-bit)
	/// - RGBA: 0xRGBA (16-bit)
	/// - RRGGBB: 0xRRGGBB (24-bit)
	/// - RRGGBBAA: 0xRRGGBBAA (32-bit)
	///
	/// # Errors
	/// Returns an error if the hex value is not in a valid format.
	pub fn from_hex_int(hex: u32) -> Result<Self, &'static str> {
		let (r, g, b, a) = match hex {
			0..=0xFFF => {
				// 12-bit RGB
				let r = ((hex >> 8) & 0xF) as u8;
				let g = ((hex >> 4) & 0xF) as u8;
				let b = (hex & 0xF) as u8;
				(r << 4 | r, g << 4 | g, b << 4 | b, None)
			}
			0x1000..=0xFFFF => {
				// 16-bit RGBA
				let r = ((hex >> 12) & 0xF) as u8;
				let g = ((hex >> 8) & 0xF) as u8;
				let b = ((hex >> 4) & 0xF) as u8;
				let a = (hex & 0xF) as u8;
				(r << 4 | r, g << 4 | g, b << 4 | b, Some(a << 4 | a))
			}
			0x10000..=0x00FF_FFFF => {
				// 24-bit RGB
				let r = ((hex >> 16) & 0xFF) as u8;
				let g = ((hex >> 8) & 0xFF) as u8;
				let b = (hex & 0xFF) as u8;
				(r, g, b, None)
			}
			0x0100_0000..=0xFFFF_FFFF => {
				// 32-bit RGBA
				let r = ((hex >> 24) & 0xFF) as u8;
				let g = ((hex >> 16) & 0xFF) as u8;
				let b = ((hex >> 8) & 0xFF) as u8;
				let a = (hex & 0xFF) as u8;
				(r, g, b, Some(a))
			}
		};

		Ok(Self::new(
			f32::from(r) / 255.0,
			f32::from(g) / 255.0,
			f32::from(b) / 255.0,
			a.map(|a| f32::from(a) / 255.0),
			None,
		))
	}
}

impl ColorComponents {
	/// Creates a color from sRGB values (non-linear).
	#[must_use]
	pub fn new(red: f32, green: f32, blue: f32, opacity: Option<f32>) -> Self {
		Self {
			red: srgb_to_linear(red),
			green: srgb_to_linear(green),
			blue: srgb_to_linear(blue),
			opacity: opacity.unwrap_or(1.0).clamp(0.0, 1.0),
		}
	}

	/// Creates a color from sRGB values (linear).
	#[must_use]
	pub fn from_linear(red: f32, green: f32, blue: f32, opacity: Option<f32>) -> Self {
		Self {
			red,
			green,
			blue,
			opacity: opacity.unwrap_or(1.0).clamp(0.0, 1.0),
		}
	}

	/// Converts linear components to non-linear components.
	fn to_srgb_components(self) -> Self {
		Self {
			red: linear_to_srgb(self.red),
			green: linear_to_srgb(self.green),
			blue: linear_to_srgb(self.blue),
			opacity: self.opacity,
		}
	}

	/// Returns the linear components as a rounded vector.
	fn to_serialized_vec(self) -> Vec<f32> {
		let r = round(self.red);
		let g = round(self.green);
		let b = round(self.blue);
		let a = round(self.opacity);
		if a == 1.0 {
			vec![r, g, b]
		} else {
			vec![r, g, b, a]
		}
	}

	/// Creates linear components from a float vector (3 or 4 values).
	fn from_serialized_vec(vec: &[f32]) -> Result<Self, &'static str> {
		match vec {
			[r, g, b] => Ok(Self::from_linear(*r, *g, *b, None)),
			[r, g, b, a] => Ok(Self::from_linear(*r, *g, *b, Some(a.clamp(0.0, 1.0)))),
			_ => Err("Expected 3 or 4 float values"),
		}
	}
}

impl From<(f32, f32, f32, f32)> for ColorComponents {
	fn from((r, g, b, a): (f32, f32, f32, f32)) -> Self {
		Self {
			red: r,
			green: g,
			blue: b,
			opacity: a,
		}
	}
}

impl From<(f32, f32, f32)> for ColorComponents {
	fn from((r, g, b): (f32, f32, f32)) -> Self {
		Self {
			red: r,
			green: g,
			blue: b,
			opacity: 1.0,
		}
	}
}

mod color_components_as_linear {
	use super::ColorComponents;
	use serde::{Deserialize, Deserializer, Serialize, Serializer};

	pub fn serialize<S>(value: &ColorComponents, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		value.to_serialized_vec().serialize(serializer)
	}

	pub fn deserialize<'de, D>(deserializer: D) -> Result<ColorComponents, D::Error>
	where
		D: Deserializer<'de>,
	{
		let vec: Vec<f32> = Deserialize::deserialize(deserializer)?;
		ColorComponents::from_serialized_vec(&vec).map_err(serde::de::Error::custom)
	}
}

#[derive(Debug)]
pub enum PaletteError {
	Io(std::io::Error),
	Parse(serde_json::Error),
}

impl From<std::io::Error> for PaletteError {
	fn from(err: std::io::Error) -> Self {
		Self::Io(err)
	}
}

impl From<serde_json::Error> for PaletteError {
	fn from(err: serde_json::Error) -> Self {
		Self::Parse(err)
	}
}

fn round(value: f32) -> f32 {
	(value * 10000.0).round() / 10000.0
}

fn srgb_to_linear(srgb: f32) -> f32 {
	if srgb <= 0.04045 {
		srgb / 12.92
	} else {
		((srgb + 0.055) / 1.055).powf(2.4)
	}
}

fn linear_to_srgb(linear: f32) -> f32 {
	if linear <= 0.003_130_8 {
		linear * 12.92
	} else {
		linear.powf(1.0 / 2.4).mul_add(1.055, -0.055)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_color_conversion() {
		let color = Color::new(0.5, 0.7, 0.3, Some(0.8), None);
		let components = color.components();
		assert!((components.red - 0.5).abs() < 0.0001);
		assert!((components.green - 0.7).abs() < 0.0001);
		assert!((components.blue - 0.3).abs() < 0.0001);
		assert!((components.opacity - 0.8).abs() < 0.0001);
	}

	#[test]
	fn test_serialization_roundtrip() {
		let palette = ColorPalette::new(
			vec![Color::new(1.0, 0.0, 0.0, Some(0.5), Some("Red".into()))],
			Some("Test".into()),
		);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		assert_eq!(palette, parsed);
	}

	#[test]
	fn test_from_linear_and_linear_components() {
		let color = Color::from_linear(0.1, 0.2, 0.3, Some(0.75), Some("Linear Gray".into()));
		let linear = color.components_linear();
		assert_float_eq(linear.red, 0.1);
		assert_float_eq(linear.green, 0.2);
		assert_float_eq(linear.blue, 0.3);
		assert_float_eq(linear.opacity, 0.75);
	}

	#[test]
	fn test_srgb_conversion_roundtrip() {
		let color = Color::new(0.3, 0.6, 0.9, Some(0.65), None);
		let components = color.components();
		let (r, g, b, a) = (
			components.red,
			components.green,
			components.blue,
			components.opacity,
		);
		let color2 = Color::new(r, g, b, Some(a), None);
		let delta = 0.0001;
		assert!((color.components().red - color2.components().red).abs() < delta);
		assert!((color.components().green - color2.components().green).abs() < delta);
		assert!((color.components().blue - color2.components().blue).abs() < delta);
		assert!((color.components().opacity - color2.components().opacity).abs() < delta);
	}

	#[test]
	fn test_serialization_trims_full_opacity() {
		let color = Color::new(1.0, 0.0, 0.0, Some(1.0), Some("Opaque Red".into()));
		let json = serde_json::to_string(&color).unwrap();
		assert!(json.contains("\"components\":[1.0,0.0,0.0]"));
		assert!(!json.contains("1.0,0.0,0.0,1.0"));
	}

	#[test]
	fn test_serialization_preserves_partial_opacity() {
		let color = Color::from_linear(0.5, 0.5, 0.5, Some(0.25), Some("Translucent Gray".into()));
		let json = serde_json::to_string(&color).unwrap();
		let value: serde_json::Value = serde_json::from_str(&json).unwrap();

		assert_eq!(
			value["components"],
			serde_json::json!([0.5, 0.5, 0.5, 0.25])
		);
	}

	#[test]
	fn test_deserialization_invalid_component_count() {
		let invalid_json = r#"{ \"components\": [0.5], \"name\": \"Invalid\" }"#;
		let result: Result<Color, _> = serde_json::from_str(invalid_json);
		assert!(result.is_err());
	}

	#[test]
	fn test_color_palette_write_and_read_file() {
		let palette = ColorPalette::new(
			vec![Color::new(0.2, 0.4, 0.6, Some(0.9), Some("Test".into()))],
			Some("Palette Name".into()),
		);

		let tmp_dir = tempfile::tempdir().unwrap();
		let file_path = tmp_dir.path().join("palette.json");

		palette.write_to_file(&file_path).unwrap();
		let parsed = ColorPalette::read_from_file(&file_path).unwrap();

		assert_eq!(parsed.name, Some("Palette Name".into()));
		assert_eq!(parsed.colors.len(), 1);
		assert_eq!(parsed.colors[0].name, Some("Test".into()));
	}

	const EPSILON: f32 = 0.002; // Increased epsilon to account for u8->f32 conversion

	fn assert_float_eq(a: f32, b: f32) {
		assert!((a - b).abs() < EPSILON, "Expected {a} to be close to {b}",);
	}

	#[test]
	fn test_from_hex_str() {
		// Test full hex with #
		let color = Color::from_hex_str("#FF0000").unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 1.0);

		// Test full hex without #
		let color = Color::from_hex_str("FF0000").unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 1.0);

		// Test short hex with #
		let color = Color::from_hex_str("#F00").unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 1.0);

		// Test short hex without #
		let color = Color::from_hex_str("F00").unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 1.0);

		// Test RGBA 4-digit shorthand
		let color = Color::from_hex_str("#F008").unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 136.0 / 255.0);

		// Test full hex with alpha
		let color = Color::from_hex_str("#FF000080").unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 128.0 / 255.0);

		// Test invalid cases
		assert!(Color::from_hex_str("#").is_err());
		assert!(Color::from_hex_str("").is_err());
		assert!(Color::from_hex_str("#FF").is_err());
		assert!(Color::from_hex_str("#FFFFF").is_err());
		assert!(Color::from_hex_str("#FFFFFFF").is_err());
		assert!(Color::from_hex_str("#FFFFFFFFF").is_err());
		assert!(Color::from_hex_str("#GG0000").is_err());
		assert!(Color::from_hex_str("#XY0000").is_err());
	}

	#[test]
	fn test_from_hex_int() {
		// Test 24-bit RGB
		let color = Color::from_hex_int(0x00FF_0000).unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 1.0);

		// Test 32-bit RGBA
		let color = Color::from_hex_int(0xFF00_0080).unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 128.0 / 255.0); // Exact value for 0x80

		// Test 12-bit RGB
		let color = Color::from_hex_int(0xF00).unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 1.0);

		// Test 16-bit RGBA
		let color = Color::from_hex_int(0xF008).unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 136.0 / 255.0); // Exact value for 0x88 (expanded from 0x8)

		// Test partial values
		let color = Color::from_hex_int(0x0080_0000).unwrap();
		assert_float_eq(color.components().red, 128.0 / 255.0); // Exact value for 0x80
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 1.0);

		// Test zero
		let color = Color::from_hex_int(0x0000_0000).unwrap();
		assert_float_eq(color.components().red, 0.0);
		assert_float_eq(color.components().green, 0.0);
		assert_float_eq(color.components().blue, 0.0);
		assert_float_eq(color.components().opacity, 1.0);

		// Test white
		let color = Color::from_hex_int(0x00FF_FFFF).unwrap();
		assert_float_eq(color.components().red, 1.0);
		assert_float_eq(color.components().green, 1.0);
		assert_float_eq(color.components().blue, 1.0);
		assert_float_eq(color.components().opacity, 1.0);
	}
}
