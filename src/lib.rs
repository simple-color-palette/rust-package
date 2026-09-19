/*!
An implementation of the [Simple Color Palette](https://simplecolorpalette.com) format, a minimal JSON-based file format for defining color palettes.

A palette contains colors with optional names, and the palette itself can also have a name. A palette written and read back compares equal, including the order of the colors, with one exception: an empty name is written as no name, since the format does not allow a name that is present to be empty.

```rust
use simple_color_palette::{Color, ColorPalette};

let palette = ColorPalette::new(vec![
	Color::new(1.0, 0.0, 0.0).with_name("Red"),
	Color::new(0.0, 1.0, 0.0).with_name("Green"),
])
.with_name("Traffic Lights");

let path = std::env::temp_dir().join("Traffic Lights.color-palette");

// Save palette
palette.write_to_file(&path)?;

// Load palette
let loaded_palette = ColorPalette::read_from_file(&path)?;

assert_eq!(loaded_palette, palette);
# Ok::<(), simple_color_palette::PaletteError>(())
```

# Color space

Components are stored in **extended linear sRGB** (D65), which is what the format defines and what gets written to the file. Extended means values may be below 0 or above 1, which is how the format carries wide gamut and HDR colors. RGB components are never clamped.

Most code does not want linear values. [`Color::new`] takes, and [`Color::components`] returns, ordinary non-linear sRGB, which is what color pickers and design tools work in. Use [`Color::from_linear`] and [`Color::components_linear`] only when you are doing color math.

# Precision

The format defines values at a resolution of 1×10⁻⁵. Components are rounded to that resolution with round-half-to-even both when they are created and again when they are written, so equal colors are always written identically, and a value this crate writes reads back unchanged.

The resolution applies to the components that are stored, which are the linear ones. Two linear components compare equal exactly when they round to the same grid point, and values either side of a grid point round apart however close together they are.

Non-linear values are quantized twice, once as non-linear sRGB and again as linear sRGB, so converting a color back is not exact. `Color::new(0.001, 0.0, 0.0).components().red` is `0.00103`, and the drift reaches 6.5e-5 near black. Every whole 8-bit sRGB value does round trip exactly, which is what the hex initializers produce.

Opacity is additionally clamped to `0.0...1.0`, and is left out of the file entirely when it is `1.0`, since that is what the format means by an absent opacity.

# Format

- File extension: `.color-palette`
- Encoding: UTF-8 without a byte order mark
- Media type: `application/x.sindresorhus.simple-color-palette+json`
- Uniform type identifier: `com.sindresorhus.simple-color-palette`
*/

#![allow(clippy::tabs_in_doc_comments)]
#![warn(missing_docs)]
#![warn(clippy::missing_errors_doc)]
#![warn(clippy::missing_panics_doc)]
#![warn(clippy::doc_markdown)]

use serde::{Deserialize, Serialize};
use std::{error::Error, fmt, fs, path::Path};

/**
Pulls the readme in as a doctest, so its example is compiled and run by `cargo test`. Nothing is rendered; this only exists so the readme cannot drift from the API.

Every code block in the readme must carry a language tag. An indented block is treated as Rust, compiled, and run, and a failure in it is reported at a line of this file rather than of the readme.
*/
#[cfg(doctest)]
#[doc = include_str!("../readme.md")]
#[doc(hidden)]
pub struct ReadmeDoctests;

/**
The number of decimal places the spec defines component values at.

The spec calls this a quantization rule, not a formatting requirement. Writers round to this resolution before writing, and readers accept any precision and quantize to it.
*/
const DECIMAL_PLACES: i32 = 5;

// The used comment style is an intentional choice.

/**
A palette: an ordered list of colors with an optional name.

```rust
use simple_color_palette::{Color, ColorPalette};

let palette = ColorPalette::new(vec![Color::new(1.0, 0.0, 0.0).with_name("Red")])
	.with_name("Favorites");

assert_eq!(palette.colors.len(), 1);
assert_eq!(palette.name.as_deref(), Some("Favorites"));
```
*/
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct ColorPalette {
	/**
	List of colors in the palette.

	The order is significant and is preserved.

	- Note: The schema requires at least one color, but an empty palette is deliberately allowed so that writing and reading one stays lossless. Refusing it would leave a palette built in memory unable to be saved. The Swift and JavaScript implementations do the same.
	*/
	pub colors: Vec<Color>,

	/**
	Optional name of the palette.

	An empty name is written as no name, since the format requires a present name to be non-empty.
	*/
	#[serde(skip_serializing_if = "is_empty_name")]
	pub name: Option<String>,
}

/**
Represents a single color in the palette.

The color components are stored in extended linear sRGB color space, but the API primarily works with non-linear (gamma-corrected) values which better match human perception.
*/
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Color {
	/**
	The color components in extended linear sRGB color space.

	For most purposes, you will want to use `components()` instead which returns normal sRGB values, meaning adjusted for human perception and display on screens.
	*/
	#[serde(rename = "components", with = "color_components_as_linear")]
	linear_components: ColorComponents,

	/**
	Optional name for the color.

	An empty name is written as no name, since the format requires a present name to be non-empty.
	*/
	#[serde(skip_serializing_if = "is_empty_name")]
	pub name: Option<String>,
}

/**
Color components representing RGB values and opacity.

The same type carries both of the color spaces this crate works in, so what the numbers mean depends on where they came from. [`Color::components`] and [`ColorComponents::new`] are extended non-linear sRGB, the form color pickers and design tools use, while [`Color::components_linear`] and [`ColorComponents::from_linear`] are extended linear sRGB, the form the format stores.

The initializers round the RGB components to the 5 decimal place resolution, and non-linear values are rounded on the way in before the linear result is rounded again, so comparison happens at that resolution and the non-linear round trip is not exact. The [crate documentation](crate) has the details. Opacity starts at `1.0`; only [`ColorComponents::with_opacity`] changes it, rounding and then clamping it to `0.0...1.0`. The fields are public, so assigning to them directly skips all of this.

RGB components are never clamped, since the format is extended range.
*/
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ColorComponents {
	/**
	Red component. Extended range, so it may be below 0 or above 1.
	*/
	pub red: f64,

	/**
	Green component. Extended range, so it may be below 0 or above 1.
	*/
	pub green: f64,

	/**
	Blue component. Extended range, so it may be below 0 or above 1.
	*/
	pub blue: f64,

	/**
	Opacity, clamped to `0.0...1.0`.

	It is left out of the file when it is `1.0`, since that is what the format means by an absent opacity.
	*/
	pub opacity: f64,
}

impl ColorPalette {
	/**
	Creates a new color palette with the specified colors.

	- Note: Use `with_name()` to set the palette name.
	*/
	#[must_use]
	pub const fn new(colors: Vec<Color>) -> Self {
		Self { colors, name: None }
	}

	/**
	Returns the palette with the given name.
	*/
	#[must_use]
	pub fn with_name(mut self, name: impl Into<String>) -> Self {
		self.name = Some(name.into());
		self
	}

	/**
	Writes the palette to a file in the Simple Color Palette format.

	The file is written as pretty-printed JSON, with no byte order mark and no trailing newline.

	- Parameter path: The file path to write the palette to. Use the `.color-palette` file extension.

	# Errors

	Returns [`PaletteError::Io`] if the file cannot be written, and [`PaletteError::Json`] if a component is not a finite number, since the format has no way to write one. Nothing is written in that case.
	*/
	pub fn write_to_file(&self, path: impl AsRef<Path>) -> Result<(), PaletteError> {
		let json = serde_json::to_string_pretty(self)?;
		fs::write(path, json)?;
		Ok(())
	}

	/**
	Reads a palette from a file in the Simple Color Palette format.

	The reader is deliberately lenient: unknown fields are ignored so that a file from a newer version of the format still loads, and an empty `colors` array is accepted even though the schema asks for at least one color.

	- Note: The spec says a palette with no name takes the file name without its extension. Do that on top of this if you need it, since the palette does not know which file it came from.

	- Parameter path: The file path to read the palette from.

	# Errors

	Returns [`PaletteError::Io`] if the file cannot be read or is not UTF-8, and [`PaletteError::Json`] if it is not a palette.
	*/
	pub fn read_from_file(path: impl AsRef<Path>) -> Result<Self, PaletteError> {
		let data = fs::read_to_string(path)?;
		Ok(serde_json::from_str(&data)?)
	}
}

impl Color {
	/**
	Creates a color using extended non-linear sRGB components.

	- Note: If you don't know the difference between linear and non-linear sRGB, this is the initializer you want.

	- Parameters:
		- red: Red component, normally in `0.0...1.0` where `1.0` is full intensity.
		- green: Green component, normally in `0.0...1.0`.
		- blue: Blue component, normally in `0.0...1.0`.

	Values outside `0.0...1.0` are wide gamut and are kept as they are, so this is neither a clamp nor an 8-bit value.

	Use `with_opacity()` for opacity and `with_name()` for a name.
	*/
	#[must_use]
	pub fn new(red: f64, green: f64, blue: f64) -> Self {
		Self {
			linear_components: ColorComponents::new(red, green, blue),
			name: None,
		}
	}

	/**
	Creates a color using extended linear sRGB components.

	- Note: For most purposes, you will want to use `new()` instead.

	- Parameters:
		- red: Linear red component.
		- green: Linear green component.
		- blue: Linear blue component.

	Use `with_opacity()` for opacity and `with_name()` for a name.
	*/
	#[must_use]
	pub fn from_linear(red: f64, green: f64, blue: f64) -> Self {
		Self {
			linear_components: ColorComponents::from_linear(red, green, blue),
			name: None,
		}
	}

	/**
	Returns the color with the given opacity (0.0...1.0).

	Out-of-range values are clamped. An infinity clamps like any other out-of-range value, but a NaN is not a number and is kept, and a palette holding one cannot be written.
	*/
	#[must_use]
	pub fn with_opacity(mut self, opacity: f64) -> Self {
		self.linear_components = self.linear_components.with_opacity(opacity);
		self
	}

	/**
	Returns the color with the given name.
	*/
	#[must_use]
	pub fn with_name(mut self, name: impl Into<String>) -> Self {
		self.name = Some(name.into());
		self
	}

	/**
	Returns the color components in extended non-linear sRGB color space.

	This is probably what you want. The color components are adjusted for human perception and display on screens, and are what you format a color for display with.

	The values are rounded to the 5 decimal place resolution and the result is a copy, so changing it does not change the color. Because the stored value is quantized in linear space, going back is not exact: `Color::new(0.001, 0.0, 0.0).components().red` is `0.00103`.

	- Note: There is no hex writer. The components are extended range, so a color can fall outside what `#RRGGBB` can express, and formatting one is a display decision. Scale by `255.0` and clamp first if that is what you want.

	```rust
	use simple_color_palette::Color;

	let color = Color::new(1.0, 0.5, 0.0);
	let components = color.components();

	assert_eq!(components.red, 1.0);
	assert_eq!(components.blue, 0.0);
	assert_eq!(components.opacity, 1.0);
	```
	*/
	#[must_use]
	pub fn components(&self) -> ColorComponents {
		self.linear_components.to_srgb_components()
	}

	/**
	Returns the color components in extended linear sRGB color space, which is what the format stores.

	- Note: For most purposes, you will want to use `components()` instead which returns normal sRGB values.

	```rust
	use simple_color_palette::Color;

	// Ordinary sRGB 0.5 is stored as linear 0.21404.
	let color = Color::new(0.5, 0.5, 0.5);

	assert_eq!(color.components_linear().red, 0.21404);
	```
	*/
	#[must_use]
	pub const fn components_linear(&self) -> ColorComponents {
		self.linear_components
	}

	/**
	Creates a color from a hex string.

	Supports the following formats:
	- RGB: "#RGB" or "RGB"
	- RGBA: "#RGBA" or "RGBA"
	- RRGGBB: "#RRGGBB" or "RRGGBB"
	- RRGGBBAA: "#RRGGBBAA" or "RRGGBBAA"

	The "#" prefix is optional and digits may be upper or lower case. Surrounding spaces, tabs and other Unicode space separators are ignored, but newlines are not, which matches the Swift implementation.

	The input is read as sRGB, which is the standard color space for hex colors on the web and in design tools.

	```rust
	use simple_color_palette::Color;

	let red = Color::from_hex_str("#F00").unwrap();
	assert_eq!(red.components_linear().red, 1.0);

	// The same color, spelled out with an opacity digit.
	let translucent_red = Color::from_hex_str("#F008").unwrap();
	assert_eq!(translucent_red.components_linear().opacity, 0.53333);

	assert!(Color::from_hex_str("#GG0000").is_err());
	```

	# Errors

	Returns [`HexError::InvalidLength`] if the text is not 3, 4, 6 or 8 digits, and [`HexError::InvalidCharacters`] if it contains anything that is not a hex digit. The length is checked first, so text that fails both reports [`HexError::InvalidLength`].

	This is a separate error type from [`PaletteError`] and does not convert into it, so a function returning `PaletteError` cannot `?` this call.
	*/
	pub fn from_hex_str(hex: &str) -> Result<Self, HexError> {
		let digits = hex.trim_matches(is_trimmable);
		let digits = digits.strip_prefix('#').unwrap_or(digits);

		/*
		The format is chosen by the length of the text, not by the numeric value, since a number cannot express leading zeros. Short forms are expanded and a missing opacity is filled in, so what is parsed is always `RRGGBBAA`.
		*/
		let length = digits.chars().count();

		if !matches!(length, 3 | 4 | 6 | 8) {
			return Err(HexError::InvalidLength);
		}

		let expanded: String = if matches!(length, 3 | 4) {
			digits.chars().flat_map(|digit| [digit, digit]).collect()
		} else {
			digits.to_owned()
		};

		let expanded = if matches!(length, 3 | 6) {
			format!("{expanded}FF")
		} else {
			expanded
		};

		if !expanded
			.chars()
			.all(|character| character.is_ascii_hexdigit())
		{
			return Err(HexError::InvalidCharacters);
		}

		// Every character is a hex digit by now, so this cannot fail.
		let value = u32::from_str_radix(&expanded, 16).map_err(|_| HexError::InvalidCharacters)?;

		Ok(Self::from_rgb8(
			(value >> 24) as u8,
			(value >> 16) as u8,
			(value >> 8) as u8,
			value as u8,
		))
	}

	/**
	Creates a color from a hex number in the `0xRRGGBB` format.

	The input is read as sRGB, which is the standard color space for hex colors on the web and in design tools.

	- Note: A number cannot represent leading zeros, so `0xFF` and `0x0000FF` are the same number and the short and opacity formats are only supported by `from_hex_str()`.

	```rust
	use simple_color_palette::Color;

	let red = Color::from_hex_int(0xFF0000).unwrap();
	assert_eq!(red.components_linear().red, 1.0);

	// Blue needs the leading zeros to be written out, since a number cannot carry them.
	let blue = Color::from_hex_int(0x0000FF).unwrap();
	assert_eq!(blue.components_linear().blue, 1.0);

	assert!(Color::from_hex_int(0xFF000080).is_err());
	```

	# Errors

	Returns [`HexError::OutOfRange`] if the number is outside `0x000000...0xFFFFFF`.
	*/
	pub fn from_hex_int(hex: u32) -> Result<Self, HexError> {
		if hex > 0x00FF_FFFF {
			return Err(HexError::OutOfRange);
		}

		Ok(Self::from_rgb8(
			(hex >> 16) as u8,
			(hex >> 8) as u8,
			hex as u8,
			0xFF,
		))
	}

	/**
	Creates a color from 8-bit sRGB components.
	*/
	#[must_use]
	fn from_rgb8(red: u8, green: u8, blue: u8, opacity: u8) -> Self {
		Self::new(
			f64::from(red) / 255.0,
			f64::from(green) / 255.0,
			f64::from(blue) / 255.0,
		)
		.with_opacity(f64::from(opacity) / 255.0)
	}
}

impl ColorComponents {
	/**
	Creates color components from extended non-linear sRGB values.

	- Note: If you don't know the difference between linear and non-linear sRGB, this is the initializer you want.

	Use `with_opacity()` for opacity.
	*/
	#[must_use]
	pub fn new(red: f64, green: f64, blue: f64) -> Self {
		/*
		The non-linear values are rounded to the resolution before they are converted, which is what the Swift implementation does and what makes the two agree on every 8-bit input.
		*/
		Self::from_linear(
			srgb_to_linear(round_to_decimal_places(red)),
			srgb_to_linear(round_to_decimal_places(green)),
			srgb_to_linear(round_to_decimal_places(blue)),
		)
	}

	/**
	Creates color components from extended linear sRGB values.

	- Note: For most purposes, you will want to use `new()` instead.

	Use `with_opacity()` for opacity.
	*/
	#[must_use]
	pub fn from_linear(red: f64, green: f64, blue: f64) -> Self {
		Self {
			red: round_to_decimal_places(red),
			green: round_to_decimal_places(green),
			blue: round_to_decimal_places(blue),
			opacity: 1.0,
		}
	}

	/**
	Returns the components with the given opacity (0.0...1.0).

	Out-of-range values are clamped. An infinity clamps like any other out-of-range value, but a NaN is not a number and is kept, and a palette holding one cannot be written.
	*/
	#[must_use]
	pub fn with_opacity(mut self, opacity: f64) -> Self {
		self.opacity = Self::normalized_opacity(opacity);
		self
	}

	/**
	Rounds then clamps an opacity value, following the order the spec defines for writing values.
	*/
	#[must_use]
	fn normalized_opacity(opacity: f64) -> f64 {
		round_to_decimal_places(opacity).clamp(0.0, 1.0)
	}

	/**
	Converts linear components to non-linear components.
	*/
	#[must_use]
	fn to_srgb_components(self) -> Self {
		Self {
			red: round_to_decimal_places(linear_to_srgb(self.red)),
			green: round_to_decimal_places(linear_to_srgb(self.green)),
			blue: round_to_decimal_places(linear_to_srgb(self.blue)),
			opacity: self.opacity,
		}
	}

	/**
	Whether every component is a real number that can be written to the format.
	*/
	#[must_use]
	fn is_finite(self) -> bool {
		self.red.is_finite()
			&& self.green.is_finite()
			&& self.blue.is_finite()
			&& self.opacity.is_finite()
	}

	/**
	Returns the linear components as a vector, with full opacity omitted.
	*/
	#[must_use]
	fn to_serialized_vec(self) -> Vec<f64> {
		let red = round_to_decimal_places(self.red);
		let green = round_to_decimal_places(self.green);
		let blue = round_to_decimal_places(self.blue);
		let opacity = Self::normalized_opacity(self.opacity);

		if opacity == 1.0 {
			vec![red, green, blue]
		} else {
			vec![red, green, blue, opacity]
		}
	}

	/**
	Creates linear components from a float vector (3 or 4 values).
	*/
	fn from_serialized_vec(values: &[f64]) -> Result<Self, &'static str> {
		match values {
			[red, green, blue] => Ok(Self::from_linear(*red, *green, *blue)),
			[red, green, blue, opacity] => {
				Ok(Self::from_linear(*red, *green, *blue).with_opacity(*opacity))
			}
			_ => Err("Expected 3 or 4 float values"),
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
		// JSON has no way to write a non-finite number, and some serializers would quietly turn it into `null`, which the format cannot read back. Fail instead.
		if !value.is_finite() {
			return Err(serde::ser::Error::custom(
				"Color components must be finite numbers",
			));
		}

		value.to_serialized_vec().serialize(serializer)
	}

	pub fn deserialize<'de, D>(deserializer: D) -> Result<ColorComponents, D::Error>
	where
		D: Deserializer<'de>,
	{
		let values: Vec<f64> = Deserialize::deserialize(deserializer)?;
		ColorComponents::from_serialized_vec(&values).map_err(serde::de::Error::custom)
	}
}

/**
The reason a hex value could not be read as a color.
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HexError {
	/**
	The text is not 3, 4, 6 or 8 digits, ignoring an optional `#` prefix.
	*/
	InvalidLength,
	/**
	The text contains a character that is not an ASCII hex digit.
	*/
	InvalidCharacters,
	/**
	The number is outside the `0x000000...0xFFFFFF` range.
	*/
	OutOfRange,
}

impl fmt::Display for HexError {
	fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::InvalidLength => formatter
				.write_str("A hex color needs 3, 4, 6 or 8 digits, with an optional `#` prefix"),
			Self::InvalidCharacters => {
				formatter.write_str("A hex color may only contain hex digits")
			}
			Self::OutOfRange => formatter.write_str("A hex number must be in the `0xRRGGBB` range"),
		}
	}
}

impl Error for HexError {}

/**
The reason a palette could not be read from or written to a file.
*/
#[derive(Debug)]
pub enum PaletteError {
	/**
	The file could not be read or written, including when it is not UTF-8.
	*/
	Io(std::io::Error),
	/**
	The contents are not a palette, or a component cannot be written because it is not a finite number.
	*/
	Json(serde_json::Error),
}

impl fmt::Display for PaletteError {
	fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Io(error) => write!(
				formatter,
				"Failed to read or write the palette file: {error}"
			),
			Self::Json(error) => write!(
				formatter,
				"Failed to convert the palette to or from JSON: {error}"
			),
		}
	}
}

impl Error for PaletteError {
	fn source(&self) -> Option<&(dyn Error + 'static)> {
		match self {
			Self::Io(error) => Some(error),
			Self::Json(error) => Some(error),
		}
	}
}

impl From<std::io::Error> for PaletteError {
	fn from(error: std::io::Error) -> Self {
		Self::Io(error)
	}
}

impl From<serde_json::Error> for PaletteError {
	fn from(error: serde_json::Error) -> Self {
		Self::Json(error)
	}
}

/**
Whether a character is trimmed from around a hex string.

The Swift implementation trims `CharacterSet.whitespaces`, whose documented description is the Unicode space separator category plus the tab. The set also contains `U+200B ZERO WIDTH SPACE`, which was reclassified out of that category in Unicode 4.0 but is still in the set, so it is covered here too.

This deliberately excludes newlines and the other non-space whitespace, which Swift does not trim and therefore rejects.

The exact membership is pinned by a test, so this stays in step with the reference.
*/
fn is_trimmable(character: char) -> bool {
	if matches!(character, '\u{2000}'..='\u{200B}') {
		return true;
	}

	matches!(
		character,
		'\u{0009}' | '\u{0020}' | '\u{00A0}' | '\u{1680}' | '\u{202F}' | '\u{205F}' | '\u{3000}'
	)
}

/**
Whether a name should be left out of the JSON.

The format does not allow a name that is present to be empty, so an unset or empty name is omitted when writing. Reading keeps whatever the file says.
*/
fn is_empty_name(name: &Option<String>) -> bool {
	name.as_ref().is_none_or(String::is_empty)
}

/**
Rounds a value to the spec's 5 decimal place resolution using banker's rounding.

Ties round to the nearest even digit, so `0.123445` becomes `0.12344` and `0.123455` becomes `0.12346`. The scaling happens in `f64`, so a decimal tie that binary cannot represent exactly lands on the nearer neighbour instead, which is also how the spec's own Swift example rounds.

A value with no fractional part is already a multiple of the resolution, and scaling one is neither always exact nor always finite, so integers are returned unchanged. Note that this tests the value, not the scaled product: a value one ulp off a grid point can still scale to an exact integer, and treating that as "already on the grid" would write it out unrounded.

A negative zero is returned as a positive zero, since no component is meaningfully different for it and it would otherwise show up in the written file.
*/
#[must_use]
fn round_to_decimal_places(value: f64) -> f64 {
	let rounded = if value.fract() == 0.0 {
		value
	} else {
		let multiplier = 10_f64.powi(DECIMAL_PLACES);
		(value * multiplier).round_ties_even() / multiplier
	};

	if rounded == 0.0 { 0.0 } else { rounded }
}

/**
Converts a non-linear sRGB component to linear.

The extended sRGB curve mirrors around zero, so negative (wide-gamut) values stay symmetric. Without this, a negative value would take the linear branch and land far away.

Non-finite values propagate instead of recursing, since `-x` flips the sign of a NaN, and the caller rejects them before they can be written.
*/
fn srgb_to_linear(srgb: f64) -> f64 {
	if srgb < 0.0 {
		return -srgb_to_linear(-srgb);
	}

	if srgb > 0.040_45 {
		((srgb + 0.055) / 1.055).powf(2.4)
	} else {
		srgb / 12.92
	}
}

/**
Converts a linear component to non-linear sRGB.

The extended sRGB curve mirrors around zero, so negative (wide-gamut) values stay symmetric.
*/
fn linear_to_srgb(linear: f64) -> f64 {
	if linear < 0.0 {
		return -linear_to_srgb(-linear);
	}

	if linear > 0.003_130_8 {
		linear.powf(1.0 / 2.4).mul_add(1.055, -0.055)
	} else {
		linear * 12.92
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	/**
	Converting to linear and back is limited by the 5 decimal place resolution, which is worst near black where the sRGB curve is steepest. The measured worst case is 6.43e-5 over the 8-bit sRGB values, and 6.5e-5 over the whole non-linear range.
	*/
	const GAMUT_EPSILON: f64 = 0.000_2;

	fn close_to(actual: f64, expected: f64, epsilon: f64) {
		assert!(
			(actual - expected).abs() < epsilon,
			"Expected {actual} to be close to {expected}"
		);
	}

	#[test]
	fn rounds_ties_to_even() {
		// The spec's own examples for its quantization rule.
		assert_eq!(round_to_decimal_places(0.123_456), 0.123_46);
		assert_eq!(round_to_decimal_places(0.123_455), 0.123_46);
		assert_eq!(round_to_decimal_places(-0.000_015), -0.000_02);
		assert_eq!(round_to_decimal_places(1.000_005), 1.0);
		assert_eq!(round_to_decimal_places(0.999_995), 1.0);

		// These two are the ties that a half away from zero rounding would get wrong, as 0.123_45 and 0.000_01.
		assert_eq!(round_to_decimal_places(0.123_445), 0.123_44);
		assert_eq!(round_to_decimal_places(0.000_005), 0.0);

		// Scaling this one in `f32` lands exactly on the tie and would round down to 0.068_92. Doing it in `f64` stays just above the tie.
		assert_eq!(round_to_decimal_places(0.068_925_001_907_377_74), 0.068_93);
	}

	#[test]
	fn the_non_linear_round_trip_is_lossy_in_a_bounded_way() {
		// Documented: the stored value is quantized in linear space, so going back is not exact.
		assert_eq!(Color::new(0.001, 0.0, 0.0).components().red, 0.001_03);

		// Because the quantization happens in linear space, two non-linear values a whole step apart can still be equal.
		assert_eq!(Color::new(0.0, 0.0, 0.0), Color::new(0.000_01, 0.0, 0.0));

		// The drift stays within the documented bound of 6.5e-5, with slack for the last bit of the comparison.
		let bound = 6.5e-5 + f64::EPSILON;

		for step in 0..=200_000 {
			let value = f64::from(step) / 10_000_000.0;
			let drift = (Color::new(value, 0.0, 0.0).components().red - value).abs();

			assert!(drift <= bound, "{value} drifted by {drift}");
		}
	}

	#[test]
	fn eight_bit_values_round_trip_back_to_their_byte() {
		// The hex initializers go through the lossy non-linear path, so what matters is that 8-bit precision survives it.
		for byte in 0..=255_u8 {
			let value = f64::from(byte) / 255.0;
			let round_tripped = Color::new(value, value, value).components().red;

			assert_eq!((round_tripped * 255.0).round() as u8, byte, "{byte}");
		}
	}

	#[test]
	fn rounding_always_lands_on_the_grid() {
		/*
		Sweep values that are not on the grid and check that rounding never moves a value by more than half a step and is always idempotent, which means the result is on the grid.
		*/
		for step in -100_000..=100_000 {
			let value = f64::from(step) * 0.000_001;
			let rounded = round_to_decimal_places(value);

			assert_eq!(round_to_decimal_places(rounded), rounded, "{value}");
			assert!(
				(rounded - value).abs() <= 0.000_005 + f64::EPSILON,
				"{value}"
			);
		}
	}

	#[test]
	fn values_one_ulp_off_the_grid_still_round_trip() {
		/*
		A value one ulp away from a grid point can still scale to an exact integer, which must not be mistaken for "already on the grid". `-19.99 / 10.0` is one ulp below the grid double for -1.999 and scales to exactly -199900.0.
		*/
		let off_grid = -19.99_f64 / 10.0;
		assert_ne!(off_grid, -1.999);
		assert_eq!(round_to_decimal_places(off_grid), -1.999);

		for step in -3000..=3000 {
			if step == 0 {
				continue;
			}

			let base = f64::from(step) / 1000.0;

			for value in [
				f64::from_bits(base.to_bits() - 1),
				f64::from_bits(base.to_bits() + 1),
				base / 3.0,
			] {
				let color = Color::from_linear(value, 0.5, 0.5);
				let json = serde_json::to_string(&color).unwrap();

				assert_eq!(
					serde_json::from_str::<Color>(&json).unwrap(),
					color,
					"{value}"
				);
			}
		}
	}

	#[test]
	fn components_that_round_to_the_same_grid_point_compare_equal() {
		// The resolution is the point of the rounding, so these are the same color.
		assert_eq!(
			Color::from_linear(0.123_456_1, 0.0, 0.0),
			Color::from_linear(0.123_456_4, 0.0, 0.0)
		);

		// But being closer together than the resolution is not enough. These differ by 1e-7 and still round apart, because they sit either side of a grid boundary.
		assert_ne!(
			Color::from_linear(0.123_445, 0.0, 0.0),
			Color::from_linear(0.123_445_1, 0.0, 0.0)
		);
	}

	#[test]
	fn rounding_leaves_values_that_are_already_on_the_grid_alone() {
		// Every value on the 5 decimal place grid across -2.0...2.0 must come back untouched, which also makes rounding idempotent.
		for step in -200_000..=200_000 {
			let value = f64::from(step) / 100_000.0;

			assert_eq!(round_to_decimal_places(value), value, "{value}");
		}
	}

	#[test]
	fn serialization_writes_five_decimal_places() {
		// Rounding itself is covered by `rounds_ties_to_even`. Components only reach the serializer already rounded, so this pins the wire format instead.
		let color = Color::from_linear(0.123_456, 0.123_454, 0.123_444).with_opacity(0.123_449);
		let json = serde_json::to_string(&color).unwrap();

		assert_eq!(json, r#"{"components":[0.12346,0.12345,0.12344,0.12345]}"#);
	}

	#[test]
	fn creation_rounds_components_to_five_decimals() {
		let color = Color::from_linear(0.123_456_789, 0.5, 0.999_999_9);
		let linear = color.components_linear();

		assert_eq!(linear.red, 0.123_46);
		assert_eq!(linear.green, 0.5);
		assert_eq!(linear.blue, 1.0);
	}

	#[test]
	fn creation_clamps_opacity() {
		let components = ColorComponents::from_linear(0.0, 0.0, 0.0);

		assert_eq!(components.opacity, 1.0);
		assert_eq!(components.with_opacity(1.5).opacity, 1.0);
		assert_eq!(components.with_opacity(-0.5).opacity, 0.0);
		assert_eq!(components.with_opacity(0.75).opacity, 0.75);
	}

	#[test]
	fn extended_srgb_mirrors_the_curve_for_negative_values() {
		// Another implementation writes non-linear sRGB -0.5 as this linear value.
		// Reading it back has to give -0.5, not the -2.77 you get from reusing the linear branch.
		let color = Color::from_linear(-0.214_041_140_482_232_55, 0.0, 0.0);
		close_to(color.components().red, -0.5, GAMUT_EPSILON);

		// And writing non-linear sRGB -0.5 has to produce that same linear value.
		let color = Color::new(-0.5, 0.0, 0.0);
		close_to(
			color.components_linear().red,
			-0.214_041_140_482_232_55,
			GAMUT_EPSILON,
		);
	}

	#[test]
	fn extended_srgb_curve_matches_apples_color_spaces() {
		/*
		The reference values are read back from CoreGraphics' `extendedSRGB` and `extendedLinearSRGB` color spaces. CoreGraphics stores components with float precision, so they are only good to about 1e-5.
		*/
		let apple_precision = 0.000_01;

		for (linear, expected) in [
			(-1.5, -1.194_176_673_889_16),
			(-0.5, -0.735_356_986_522_675),
			(-0.1, -0.349_190_235_137_939),
			(-0.01, -0.099_852_837_622_166),
			(0.0, 0.0),
			(0.5, 0.735_356_986_522_675),
			(1.0, 1.0),
		] {
			close_to(linear_to_srgb(linear), expected, apple_precision);
		}

		for (srgb, expected) in [
			(-1.5, -2.537_154_912_948_608),
			(-0.5, -0.214_041_143_655_777),
			(-0.1, -0.010_022_822_767_496),
			(-0.01, -0.000_773_993_786_424),
			(0.0, 0.0),
			(0.5, 0.214_041_143_655_777),
			(1.0, 1.0),
		] {
			close_to(srgb_to_linear(srgb), expected, apple_precision);
		}
	}

	#[test]
	fn extended_srgb_round_trips_across_the_wide_gamut_range() {
		/*
		A round trip uses the same pair of functions in both directions, so it cancels out a mistake that is consistent on both sides and cannot catch a wrong branch. The direction that matters is pinned against CoreGraphics in the test above.
		*/
		for value in [-1.5, -0.5, -0.1, -0.01, 0.0, 0.01, 0.5, 1.0, 1.5, 2.0] {
			let color = Color::new(value, value, value);

			close_to(color.components().red, value, GAMUT_EPSILON);
		}
	}

	#[test]
	fn linear_conversion_round_trips() {
		let color = Color::new(0.3, 0.6, 0.9).with_opacity(0.65);
		let components = color.components();

		let reconstructed = Color::new(components.red, components.green, components.blue)
			.with_opacity(components.opacity);

		close_to(
			reconstructed.components_linear().red,
			color.components_linear().red,
			GAMUT_EPSILON,
		);
		close_to(
			reconstructed.components_linear().green,
			color.components_linear().green,
			GAMUT_EPSILON,
		);
		close_to(
			reconstructed.components_linear().blue,
			color.components_linear().blue,
			GAMUT_EPSILON,
		);
		assert_eq!(reconstructed.components().opacity, 0.65);
	}

	#[test]
	fn components_are_rounded_when_converted() {
		// Whole values stay whole instead of coming back as 0.9999999999999999.
		for value in [0.0, 0.5, 1.0] {
			assert_eq!(Color::new(value, value, value).components().red, value);
		}

		// Every converted component lands exactly on the 5 decimal place grid.
		for value in [-1.5, -0.1, 0.2, 0.25, 0.3, 0.75, 0.9, 1.5, 2.0] {
			let components = Color::from_linear(value, value, value).components();

			for component in [components.red, components.green, components.blue] {
				assert_eq!(
					round_to_decimal_places(component),
					component,
					"{component} is not on the 5 decimal place grid"
				);
			}
		}
	}

	#[test]
	fn very_large_components_do_not_overflow_when_rounding() {
		// Scaling by 100000 must not turn a finite value into infinity, which would write an unreadable file.
		let palette = ColorPalette::new(vec![Color::from_linear(1e308, -1e308, 1e300)]);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		assert_eq!(parsed, palette);
		assert!(!json.contains("null"));
	}

	#[test]
	fn non_finite_components_cannot_be_written() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Broken.color-palette");

		// A negative NaN would recurse forever in the curve if the sign test there were `is_sign_negative()` rather than `< 0.0`.
		for value in [f64::NAN, -f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
			for color in [
				Color::from_linear(value, 0.0, 0.0),
				Color::from_linear(0.0, value, 0.0),
				Color::from_linear(0.0, 0.0, value),
				// The non-linear path has to reject them too.
				Color::new(value, 0.0, 0.0),
				Color::new(0.0, value, 0.0),
				Color::new(0.0, 0.0, value),
			] {
				let palette = ColorPalette::new(vec![color]);

				// JSON has no way to write these, so both the direct serde path and the helper must fail.
				assert!(serde_json::to_string(&palette).is_err());
				assert!(
					matches!(palette.write_to_file(&path), Err(PaletteError::Json(_))),
					"{value}"
				);

				assert!(!path.exists(), "{value}");
			}
		}

		// Only NaN survives the opacity clamp as a non-finite value.
		let palette = ColorPalette::new(vec![
			Color::from_linear(0.5, 0.5, 0.5).with_opacity(f64::NAN),
		]);

		assert!(serde_json::to_string(&palette).is_err());
		assert!(matches!(
			palette.write_to_file(&path),
			Err(PaletteError::Json(_))
		));
		assert!(!path.exists());
	}

	#[test]
	fn infinite_opacity_is_clamped_rather_than_rejected() {
		// An infinity is merely out of range, so it clamps like any other out-of-range opacity.
		let components = ColorComponents::from_linear(0.5, 0.5, 0.5);

		assert_eq!(components.with_opacity(f64::INFINITY).opacity, 1.0);
		assert_eq!(components.with_opacity(f64::NEG_INFINITY).opacity, 0.0);
	}

	#[test]
	fn components_are_written_as_plain_decimal_numbers() {
		/*
		The JSON serializer is free to change how it formats floats, so this pins the wire format that files actually contain. Ordinary component values get a plain decimal form, with a trailing `.0` for whole numbers, and only extreme values fall back to an exponent.
		*/
		for (linear, expected) in [
			(1.0, "1.0"),
			(0.0, "0.0"),
			(-1.5, "-1.5"),
			(0.5, "0.5"),
			(0.000_01, "0.00001"),
			(0.123_45, "0.12345"),
			(12.25, "12.25"),
			(100_000.0, "100000.0"),
			(1_234_567.0, "1234567.0"),
			(1e300, "1e+300"),
		] {
			let color = Color::from_linear(linear, 0.0, 0.0);

			assert_eq!(
				serde_json::to_string(&color).unwrap(),
				format!(r#"{{"components":[{expected},0.0,0.0]}}"#),
				"{linear}"
			);
		}
	}

	#[test]
	fn small_opacity_values_are_written() {
		// Anything that is not exactly full opacity belongs in the file, however small it is.
		for (opacity, expected) in [
			(0.000_01, "0.00001"),
			(0.000_05, "0.00005"),
			(0.999_99, "0.99999"),
		] {
			let color = Color::from_linear(0.5, 0.5, 0.5).with_opacity(opacity);

			assert_eq!(
				serde_json::to_string(&color).unwrap(),
				format!(r#"{{"components":[0.5,0.5,0.5,{expected}]}}"#),
				"{opacity}"
			);
		}
	}

	#[test]
	fn names_with_json_syntax_round_trip() {
		let names = [
			r#"a "quoted" name"#,
			r"a back\slash",
			"a\nnewline\ttab",
			"a\u{0001}control",
			"emoji 🎨 and accents éü",
		];

		let palette = ColorPalette::new(
			names
				.iter()
				.map(|name| Color::new(1.0, 0.0, 0.0).with_name(*name))
				.collect(),
		)
		.with_name(names[0]);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		assert_eq!(parsed, palette);
		assert_eq!(parsed.name.as_deref(), Some(names[0]));

		let parsed_names: Vec<_> = parsed
			.colors
			.iter()
			.filter_map(|color| color.name.as_deref())
			.collect();
		assert_eq!(parsed_names, names);
	}

	#[test]
	fn serialization_omits_full_opacity() {
		let color = Color::new(1.0, 0.0, 0.0).with_opacity(1.0);
		let json = serde_json::to_string(&color).unwrap();

		assert_eq!(json, r#"{"components":[1.0,0.0,0.0]}"#);
	}

	#[test]
	fn serialization_keeps_wide_gamut_and_opacity() {
		let palette = ColorPalette::new(vec![
			Color::from_linear(-0.5, 2.0, 0.000_01).with_opacity(0.5),
			Color::from_linear(0.0, 0.0, 0.0).with_opacity(0.0),
		]);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		assert_eq!(parsed, palette);
	}

	#[test]
	fn serialization_omits_missing_values() {
		let palette = ColorPalette::new(vec![Color::new(1.0, 0.0, 0.0)]);
		let value: serde_json::Value = serde_json::to_value(&palette).unwrap();

		assert_eq!(
			value,
			serde_json::json!({"colors": [{"components": [1.0, 0.0, 0.0]}]})
		);
	}

	#[test]
	fn serializes_the_shipped_sample_file() {
		let palette = ColorPalette::new(vec![
			Color::new(1.0, 0.0, 0.0).with_name("Red"),
			Color::new(0.0, 1.0, 0.0).with_name("Green"),
		])
		.with_name("Traffic Lights");

		let json = serde_json::to_string_pretty(&palette).unwrap();

		// The exact bytes of the sample file in the repository root.
		assert_eq!(json, include_str!("../Traffic Lights.color-palette"));
	}

	#[test]
	fn deserializes_the_spec_example() {
		let palette: ColorPalette = serde_json::from_str(
			r#"{
				"name": "Favorites",
				"colors": [
					{ "name": "Hot Pink", "components": [1, 0.1274, 0.418] },
					{ "components": [0.592, 0.278, 0.996, 0.9] }
				]
			}"#,
		)
		.unwrap();

		assert_eq!(palette.name.as_deref(), Some("Favorites"));
		assert_eq!(palette.colors[0].name.as_deref(), Some("Hot Pink"));

		let linear = palette.colors[0].components_linear();
		assert_eq!(
			(linear.red, linear.green, linear.blue),
			(1.0, 0.1274, 0.418)
		);

		assert_eq!(palette.colors[1].name, None);
		assert_eq!(palette.colors[1].components_linear().opacity, 0.9);
	}

	#[test]
	fn deserialization_accepts_arbitrary_precision() {
		// Readers must accept more decimals than the resolution, and scientific notation.
		let color: Color =
			serde_json::from_str(r#"{"components":[0.123456789, 1e-5, 2.5E-3]}"#).unwrap();
		let linear = color.components_linear();

		assert_eq!(linear.red, 0.123_46);
		assert_eq!(linear.green, 0.000_01);
		assert_eq!(linear.blue, 0.002_5);
	}

	#[test]
	fn deserialization_rounds_then_clamps_opacity() {
		// The spec's own out-of-range example.
		let color: Color = serde_json::from_str(r#"{"components":[0.5, 0.5, 0.5, 1.2]}"#).unwrap();
		assert_eq!(color.components_linear().opacity, 1.0);

		let color: Color = serde_json::from_str(r#"{"components":[0.5, 0.5, 0.5, -0.2]}"#).unwrap();
		assert_eq!(color.components_linear().opacity, 0.0);
	}

	#[test]
	fn deserialization_rejects_invalid_component_counts() {
		for json in [
			r#"{"components":[0.5]}"#,
			r#"{"components":[0.5, 0.5]}"#,
			r#"{"components":[0.5, 0.5, 0.5, 1.0, 1.0]}"#,
			r#"{"components":[]}"#,
		] {
			let result: Result<Color, _> = serde_json::from_str(json);
			assert!(result.is_err(), "Expected {json} to fail");
		}
	}

	#[test]
	fn deserialization_rejects_non_numeric_components() {
		for json in [
			r#"{"components":["1", 0, 0]}"#,
			r#"{"components":[true, 0, 0]}"#,
			r#"{"components":"1,0,0"}"#,
		] {
			let result: Result<Color, _> = serde_json::from_str(json);
			assert!(result.is_err(), "Expected {json} to fail");
		}
	}

	#[test]
	fn deserialization_treats_explicit_null_name_as_missing() {
		let palette: ColorPalette =
			serde_json::from_str(r#"{"name":null,"colors":[{"name":null,"components":[1,0,0]}]}"#)
				.unwrap();

		assert_eq!(palette.name, None);
		assert_eq!(palette.colors[0].name, None);
	}

	#[test]
	fn empty_names_are_dropped_when_writing() {
		let palette =
			ColorPalette::new(vec![Color::new(1.0, 0.0, 0.0).with_name("")]).with_name("");

		assert_eq!(
			serde_json::to_string(&palette).unwrap(),
			r#"{"colors":[{"components":[1.0,0.0,0.0]}]}"#
		);

		// Reading keeps what the file says, which is a name that is present but empty.
		let parsed: ColorPalette =
			serde_json::from_str(r#"{"name":"","colors":[{"components":[1,0,0],"name":""}]}"#)
				.unwrap();
		assert_eq!(parsed.name.as_deref(), Some(""));
		assert_eq!(parsed.colors[0].name.as_deref(), Some(""));

		// Writing that back drops them, so a file never contains a name the format does not allow.
		let json = serde_json::to_string(&parsed).unwrap();
		assert_eq!(json, r#"{"colors":[{"components":[1.0,0.0,0.0]}]}"#);

		// Reading that gives no names at all.
		let decoded: ColorPalette = serde_json::from_str(&json).unwrap();
		assert_eq!(decoded.name, None);
		assert_eq!(decoded.colors[0].name, None);
	}

	#[test]
	fn from_hex_str_supports_all_formats() {
		// Every spelling of red gives exactly the same color, including the short form.
		let red = Color::from_hex_str("#FF0000").unwrap();
		for hex in ["FF0000", "#F00", "F00"] {
			assert_eq!(Color::from_hex_str(hex).unwrap(), red, "{hex}");
		}

		let components = red.components_linear();
		assert_eq!(
			(
				components.red,
				components.green,
				components.blue,
				components.opacity
			),
			(1.0, 0.0, 0.0, 1.0)
		);

		// The short form expands the opacity digit too, so `#F008` is `#FF000088`.
		let translucent_red = Color::from_hex_str("#F008").unwrap().components_linear();
		assert_eq!(
			(
				translucent_red.red,
				translucent_red.green,
				translucent_red.blue,
				translucent_red.opacity
			),
			(1.0, 0.0, 0.0, 0.533_33)
		);

		assert_eq!(
			Color::from_hex_str("#FF000080")
				.unwrap()
				.components_linear()
				.opacity,
			0.501_96
		);

		let orange = Color::from_hex_str("#FF8000").unwrap().components_linear();
		assert_eq!(
			(orange.red, orange.green, orange.blue, orange.opacity),
			(1.0, 0.215_86, 0.0, 1.0)
		);
	}

	#[test]
	fn from_hex_str_is_case_insensitive_and_trims_whitespace() {
		let expected = Color::from_hex_str("#FF8800").unwrap();

		assert_eq!(Color::from_hex_str("ff8800").unwrap(), expected);
		assert_eq!(Color::from_hex_str("  #ff8800  ").unwrap(), expected);
	}

	#[test]
	fn trimmable_characters_are_exactly_the_reference_set() {
		/*
		This is the complete set that `CharacterSet.whitespaces` contains, verified by enumerating every Unicode scalar against it. U+200B is not a space separator any more, but it is still in the set.
		*/
		let expected = [
			'\u{0009}', '\u{0020}', '\u{00A0}', '\u{1680}', '\u{2000}', '\u{2001}', '\u{2002}',
			'\u{2003}', '\u{2004}', '\u{2005}', '\u{2006}', '\u{2007}', '\u{2008}', '\u{2009}',
			'\u{200A}', '\u{200B}', '\u{202F}', '\u{205F}', '\u{3000}',
		];

		let actual: Vec<char> = (0..=0x10_FFFF_u32)
			.filter_map(char::from_u32)
			.filter(|character| is_trimmable(*character))
			.collect();

		assert_eq!(actual, expected);
	}

	#[test]
	fn from_hex_str_trims_the_same_characters_as_the_reference() {
		let expected = Color::from_hex_str("#FF8800").unwrap();

		// The Unicode space separators and the tab, which is what the reference trims, plus the zero width space.
		for hex in [
			"\t#FF8800\t",
			"\u{00A0}#FF8800\u{00A0}",
			"\u{1680}#FF8800\u{1680}",
			"\u{2000}#FF8800\u{200A}",
			"\u{200B}#FF8800\u{200B}",
			"\u{202F}#FF8800\u{202F}",
			"\u{205F}#FF8800\u{205F}",
			"\u{3000}#FF8800\u{3000}",
		] {
			assert_eq!(Color::from_hex_str(hex).unwrap(), expected, "{hex:?}");
		}

		// Newlines and the other non-space whitespace are not trimmed, which also matches the Swift implementation.
		for hex in [
			"\n#FF8800\n",
			"\r#FF8800\r",
			"\u{000B}#FF8800",
			"\u{000C}#FF8800",
			"\u{0085}#FF8800",
			"\u{2028}#FF8800",
			"\u{2029}#FF8800",
			"#FF8800\n",
		] {
			assert!(Color::from_hex_str(hex).is_err(), "{hex:?}");
		}
	}

	#[test]
	fn from_hex_str_rounds_the_non_linear_value_before_converting() {
		// The reference rounds the non-linear value to the resolution before converting it, which changes the result for some bytes. These two are among them.
		let color = Color::from_hex_str("#FF2F00").unwrap();
		assert_eq!(color.components_linear().green, 0.028_42);

		let color = Color::from_hex_str("#FFC800").unwrap();
		assert_eq!(color.components_linear().green, 0.577_57);
	}

	#[test]
	fn from_hex_str_keeps_leading_zeros() {
		// The format comes from the text length, since a number would lose the leading zeros.
		let blue = Color::from_hex_str("#0000FF").unwrap().components_linear();
		assert_eq!(
			(blue.red, blue.green, blue.blue, blue.opacity),
			(0.0, 0.0, 1.0, 1.0)
		);

		// The number of digits picks the format, so `#00F` is blue while `#000F` is black with a full opacity digit.
		assert_eq!(
			Color::from_hex_str("#00F")
				.unwrap()
				.components_linear()
				.blue,
			1.0
		);

		let black_with_opacity_digit = Color::from_hex_str("#000F").unwrap().components_linear();
		assert_eq!(
			(
				black_with_opacity_digit.red,
				black_with_opacity_digit.green,
				black_with_opacity_digit.blue,
				black_with_opacity_digit.opacity
			),
			(0.0, 0.0, 0.0, 1.0)
		);

		let transparent = Color::from_hex_str("#00000000")
			.unwrap()
			.components_linear();
		assert_eq!((transparent.red, transparent.opacity), (0.0, 0.0));

		let black = Color::from_hex_str("#000000").unwrap().components_linear();
		assert_eq!(
			(black.red, black.green, black.blue, black.opacity),
			(0.0, 0.0, 0.0, 1.0)
		);
	}

	#[test]
	fn from_hex_str_rejects_invalid_input() {
		for hex in [
			"",
			"#",
			"#F",
			"#FF",
			"#FFFFF",
			"#FFFFFFF",
			"#FFFFFFFFF",
			"#GG0000",
			"#XY0000",
			"##FF0000",
			"#FF 0000",
			"FF0000#",
			"0xFF0000",
			"#+FF0000",
			"-FF000",
			"+F00",
			"#FF00-0",
			// Multi-byte input, which the length and digit checks both have to survive.
			"１２３",
			"🎨🎨🎨",
			"éééé",
		] {
			assert!(
				Color::from_hex_str(hex).is_err(),
				"Expected {hex:?} to be rejected"
			);
		}
	}

	#[test]
	fn from_hex_int_reads_the_24_bit_format() {
		let red = Color::from_hex_int(0xFF_00_00).unwrap().components_linear();
		assert_eq!(
			(red.red, red.green, red.blue, red.opacity),
			(1.0, 0.0, 0.0, 1.0)
		);

		// A number cannot express leading zeros, so `0x0000FF` is the only way to write blue.
		let blue = Color::from_hex_int(0x00_00_FF).unwrap().components_linear();
		assert_eq!((blue.red, blue.green, blue.blue), (0.0, 0.0, 1.0));

		let white = Color::from_hex_int(0xFF_FF_FF).unwrap().components_linear();
		assert_eq!((white.red, white.green, white.blue), (1.0, 1.0, 1.0));

		let mixed = Color::from_hex_int(0x12_34_56).unwrap().components_linear();
		assert_eq!(
			(mixed.red, mixed.green, mixed.blue),
			(0.006_05, 0.034_34, 0.093_06)
		);

		let black = Color::from_hex_int(0x00_00_00).unwrap().components_linear();
		assert_eq!(
			(black.red, black.green, black.blue, black.opacity),
			(0.0, 0.0, 0.0, 1.0)
		);
	}

	#[test]
	fn hex_errors_say_what_is_wrong() {
		assert_eq!(
			Color::from_hex_str("#FF").unwrap_err(),
			HexError::InvalidLength
		);
		assert_eq!(
			Color::from_hex_str("#GG0000").unwrap_err(),
			HexError::InvalidCharacters
		);
		assert_eq!(
			Color::from_hex_int(0x01_00_00_00).unwrap_err(),
			HexError::OutOfRange
		);
	}

	#[test]
	fn hex_errors_are_real_errors() {
		fn read(hex: &str) -> Result<Color, Box<dyn Error>> {
			Ok(Color::from_hex_str(hex)?)
		}

		assert!(read("#FF0000").is_ok());
		assert!(read("#FF").is_err());

		for error in [
			HexError::InvalidLength,
			HexError::InvalidCharacters,
			HexError::OutOfRange,
		] {
			assert!(!error.to_string().is_empty());
			assert!(error.source().is_none());
		}
	}

	#[test]
	fn from_hex_int_rejects_values_outside_the_24_bit_format() {
		for hex in [0x01_00_00_00, 0xFF_00_00_80, u32::MAX] {
			assert!(
				Color::from_hex_int(hex).is_err(),
				"Expected {hex:#X} to be rejected"
			);
		}
	}

	#[test]
	fn file_round_trip_preserves_everything() {
		let palette = ColorPalette::new(vec![
			Color::new(0.2, 0.4, 0.6)
				.with_opacity(0.9)
				.with_name("Test"),
			Color::from_linear(-0.5, 2.0, 0.000_01).with_name("Wide"),
		])
		.with_name("Palette Name");

		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Favorites.color-palette");

		palette.write_to_file(&path).unwrap();
		let parsed = ColorPalette::read_from_file(&path).unwrap();

		assert_eq!(parsed, palette);
		assert_eq!(parsed.name.as_deref(), Some("Palette Name"));
		assert_eq!(parsed.colors[0].name.as_deref(), Some("Test"));
	}

	#[test]
	fn reading_a_missing_file_reports_an_io_error() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Missing.color-palette");

		let error = ColorPalette::read_from_file(&path).unwrap_err();

		assert!(matches!(error, PaletteError::Io(_)));
		assert!(error.source().is_some());
		assert!(!error.to_string().is_empty());
	}

	#[test]
	fn writing_to_an_invalid_path_reports_an_io_error() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory
			.path()
			.join("missing-directory")
			.join("palette.color-palette");

		let palette = ColorPalette::new(vec![Color::new(1.0, 0.0, 0.0)]);
		let error = palette.write_to_file(&path).unwrap_err();

		assert!(matches!(error, PaletteError::Io(_)));
	}

	#[test]
	fn parsing_invalid_json_reports_a_json_error() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Broken.color-palette");
		fs::write(&path, "not json").unwrap();

		let error = ColorPalette::read_from_file(&path).unwrap_err();

		assert!(matches!(error, PaletteError::Json(_)));
		assert!(error.source().is_some());
	}

	#[test]
	fn palette_error_works_as_a_boxed_error() {
		fn parse(json: &str) -> Result<ColorPalette, Box<dyn Error>> {
			Ok(serde_json::from_str(json)?)
		}

		assert!(parse(r#"{"colors":[{"components":[1,0,0]}]}"#).is_ok());
		assert!(parse("nope").is_err());
	}

	#[test]
	fn empty_palette_round_trips() {
		// The schema asks for at least one color, but that is deliberately not enforced so an empty palette can still be written and read back.
		let palette = ColorPalette::default();
		let json = serde_json::to_string(&palette).unwrap();

		assert_eq!(json, r#"{"colors":[]}"#);

		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();
		assert_eq!(parsed, palette);
		assert!(parsed.colors.is_empty());
		assert_eq!(parsed.name, None);

		// An empty array is accepted from a file too.
		let parsed: ColorPalette = serde_json::from_str(r#"{"colors": []}"#).unwrap();
		assert!(parsed.colors.is_empty());
	}

	#[test]
	fn colors_keep_their_order() {
		// The spec says the order is significant and must be preserved.
		let palette = ColorPalette::new(vec![
			Color::from_linear(0.1, 0.0, 0.0).with_name("First"),
			Color::from_linear(0.2, 0.0, 0.0).with_name("Second"),
			Color::from_linear(0.3, 0.0, 0.0).with_name("Third"),
		]);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		let names: Vec<_> = parsed
			.colors
			.iter()
			.filter_map(|color| color.name.as_deref())
			.collect();
		assert_eq!(names, ["First", "Second", "Third"]);

		let reds: Vec<_> = parsed
			.colors
			.iter()
			.map(|color| color.components_linear().red)
			.collect();
		assert_eq!(reds, [0.1, 0.2, 0.3]);
	}

	#[test]
	fn opacity_that_rounds_to_full_is_omitted() {
		// Omitting the opacity means full opacity, so writing it would be redundant.
		for opacity in [0.999_995, 0.999_999_9, 1.0] {
			let color = Color::new(1.0, 0.0, 0.0).with_opacity(opacity);

			assert_eq!(
				serde_json::to_string(&color).unwrap(),
				r#"{"components":[1.0,0.0,0.0]}"#,
				"{opacity}"
			);
		}
	}

	#[test]
	fn opacity_that_rounds_to_zero_is_kept() {
		// A transparent color still needs its opacity, since omitting it would mean the opposite.
		let color = Color::new(1.0, 0.0, 0.0).with_opacity(0.000_004);
		let json = serde_json::to_string(&color).unwrap();

		assert_eq!(json, r#"{"components":[1.0,0.0,0.0,0.0]}"#);

		let parsed: Color = serde_json::from_str(&json).unwrap();
		assert_eq!(parsed.components_linear().opacity, 0.0);
	}

	#[test]
	fn an_explicit_full_opacity_is_the_same_as_omitting_it() {
		let written = r#"{"components":[1.0,0.0,0.0]}"#;
		let explicit = r#"{"components":[1.0,0.0,0.0,1.0]}"#;

		let without_opacity: Color = serde_json::from_str(written).unwrap();
		let with_opacity: Color = serde_json::from_str(explicit).unwrap();

		assert_eq!(without_opacity, with_opacity);

		// Both are written back with the opacity omitted.
		assert_eq!(serde_json::to_string(&without_opacity).unwrap(), written);
		assert_eq!(serde_json::to_string(&with_opacity).unwrap(), written);
	}

	#[test]
	fn rgb_is_never_clamped() {
		// The color space is extended range, so only the opacity is clamped.
		let palette = ColorPalette::new(vec![
			Color::from_linear(-5.0, 12.25, 1.000_01).with_opacity(3.0),
		]);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();
		let linear = parsed.colors[0].components_linear();

		assert_eq!(
			(linear.red, linear.green, linear.blue),
			(-5.0, 12.25, 1.000_01)
		);
		assert_eq!(linear.opacity, 1.0);
	}

	#[test]
	fn extended_range_values_survive_a_round_trip() {
		let mut colors = Vec::new();

		for red in [-2.0, -0.5, -0.000_01, 0.0, 0.000_01, 0.5, 1.0, 1.5, 4.0] {
			for opacity in [0.0, 0.5, 1.0] {
				colors.push(Color::from_linear(red, red * 2.0, 0.25).with_opacity(opacity));
			}
		}

		let palette = ColorPalette::new(colors);
		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		assert_eq!(parsed, palette);
		// Writing it again gives the same bytes, so the format is stable.
		assert_eq!(serde_json::to_string(&parsed).unwrap(), json);
	}

	#[test]
	fn values_at_the_resolution_round_trip() {
		let palette = ColorPalette::new(vec![
			Color::from_linear(0.000_01, -0.000_01, 1e300),
			Color::from_linear(-1e300, 1e-5, 0.0),
		]);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		assert_eq!(parsed, palette);
	}

	#[test]
	fn components_below_the_resolution_round_to_zero() {
		// The format cannot express anything finer than 1e-5, so these all collapse to zero.
		let color = Color::from_linear(0.000_004, -0.000_004, 1e-300);
		let linear = color.components_linear();

		assert_eq!((linear.red, linear.green, linear.blue), (0.0, 0.0, 0.0));

		// Negative zero is written as a positive zero.
		let json = serde_json::to_string(&color).unwrap();
		assert_eq!(json, r#"{"components":[0.0,0.0,0.0]}"#);

		let parsed: Color = serde_json::from_str(&json).unwrap();
		assert_eq!(parsed, color);
	}

	#[test]
	fn negative_zero_is_never_written() {
		// `-0.0` is a valid JSON number but not a meaningful component, so every path that can produce it collapses it.
		assert_eq!(round_to_decimal_places(-0.0), 0.0);
		assert_eq!(round_to_decimal_places(-0.000_000_1), 0.0);
		assert_eq!(round_to_decimal_places(-0.000_004), 0.0);

		for color in [
			Color::from_linear(-0.0, -0.0, -0.0),
			Color::from_linear(-0.000_004, -0.000_004, -0.000_004),
			Color::from_linear(0.5, 0.5, 0.5).with_opacity(-0.0),
			Color::from_linear(0.5, 0.5, 0.5).with_opacity(-0.000_004),
		] {
			let json = serde_json::to_string(&color).unwrap();

			assert!(!json.contains("-0.0"), "{json}");
		}
	}

	#[test]
	fn rebuilding_a_color_from_its_components_gives_the_same_color() {
		// The non-linear accessor is rounded and the constructor rounds its input the same way, so going through it has to be stable.
		for step in -200..=200 {
			let value = f64::from(step) / 100.0;

			for others in [(0.0, 0.0), (0.5, 1.0), (1.0, 0.25)] {
				let color = Color::new(value, others.0, others.1).with_opacity(0.5);
				let components = color.components();

				assert_eq!(
					Color::new(components.red, components.green, components.blue)
						.with_opacity(components.opacity),
					color,
					"{value}"
				);
			}
		}
	}

	#[test]
	fn a_wide_slice_of_the_grid_round_trips_through_a_file() {
		let palette = ColorPalette::new(
			(-100_000..=100_000)
				.step_by(7)
				.map(|step| Color::from_linear(f64::from(step) / 100_000.0, 0.5, -1.25))
				.collect(),
		);

		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Grid.color-palette");

		palette.write_to_file(&path).unwrap();

		assert_eq!(ColorPalette::read_from_file(&path).unwrap(), palette);
	}

	#[test]
	fn reading_and_writing_again_gives_the_same_bytes() {
		let palette = ColorPalette::new(vec![
			Color::new(0.1, 0.2, 0.3).with_name("One"),
			Color::from_linear(-1.0, 2.0, 0.5).with_opacity(0.5),
		])
		.with_name("Stable");

		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Stable.color-palette");

		palette.write_to_file(&path).unwrap();
		let first = fs::read_to_string(&path).unwrap();

		ColorPalette::read_from_file(&path)
			.unwrap()
			.write_to_file(&path)
			.unwrap();

		assert_eq!(fs::read_to_string(&path).unwrap(), first);
	}

	#[test]
	fn a_single_color_palette_round_trips() {
		let palette = ColorPalette::new(vec![Color::new(0.25, 0.5, 0.75).with_opacity(0.125)]);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		assert_eq!(parsed, palette);
	}

	#[test]
	fn duplicate_names_are_allowed() {
		// The spec puts no uniqueness constraint on names.
		let palette = ColorPalette::new(vec![
			Color::new(1.0, 0.0, 0.0).with_name("Same"),
			Color::new(0.0, 0.0, 1.0).with_name("Same"),
		]);

		let json = serde_json::to_string(&palette).unwrap();
		let parsed: ColorPalette = serde_json::from_str(&json).unwrap();

		assert_eq!(parsed, palette);
		assert_eq!(parsed.colors[0].name, parsed.colors[1].name);
	}

	#[test]
	fn a_palette_name_does_not_require_color_names() {
		let palette = ColorPalette::new(vec![Color::new(1.0, 0.0, 0.0), Color::new(0.0, 1.0, 0.0)])
			.with_name("Only The Palette");

		let value: serde_json::Value = serde_json::to_value(&palette).unwrap();

		assert_eq!(
			value,
			serde_json::json!({
				"name": "Only The Palette",
				"colors": [
					{"components": [1.0, 0.0, 0.0]},
					{"components": [0.0, 1.0, 0.0]},
				],
			})
		);
	}

	#[test]
	fn names_round_trip_through_a_file() {
		let palette = ColorPalette::new(vec![
			Color::new(1.0, 0.0, 0.0).with_name("Rød"),
			Color::new(0.0, 1.0, 0.0).with_name("Grün 🌱"),
			Color::new(0.0, 0.0, 1.0).with_name("  padded  "),
		])
		.with_name("Unicode – Palette");

		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Unicode.color-palette");

		palette.write_to_file(&path).unwrap();
		let parsed = ColorPalette::read_from_file(&path).unwrap();

		assert_eq!(parsed, palette);
		assert_eq!(parsed.colors[1].name.as_deref(), Some("Grün 🌱"));
		// Only an empty name is dropped, not one made of whitespace.
		assert_eq!(parsed.colors[2].name.as_deref(), Some("  padded  "));
	}

	#[test]
	fn builder_order_does_not_matter() {
		let one = Color::new(0.5, 0.5, 0.5)
			.with_opacity(0.25)
			.with_name("Grey");
		let other = Color::new(0.5, 0.5, 0.5)
			.with_name("Grey")
			.with_opacity(0.25);

		assert_eq!(one, other);
		assert_eq!(one.components_linear().opacity, 0.25);
		assert_eq!(one.name.as_deref(), Some("Grey"));
	}

	#[test]
	fn with_name_accepts_owned_and_borrowed_strings() {
		assert_eq!(
			Color::new(1.0, 0.0, 0.0).with_name("Borrowed"),
			Color::new(1.0, 0.0, 0.0).with_name(String::from("Borrowed"))
		);
		assert_eq!(
			ColorPalette::new(Vec::new())
				.with_name(String::from("Owned"))
				.name
				.as_deref(),
			Some("Owned")
		);
	}

	#[test]
	fn default_palette_is_an_empty_palette() {
		assert_eq!(ColorPalette::default(), ColorPalette::new(Vec::new()));
		assert!(ColorPalette::default().colors.is_empty());
		assert_eq!(ColorPalette::default().name, None);
	}

	#[test]
	fn linear_and_srgb_components_describe_different_things() {
		let srgb = Color::new(0.5, 0.5, 0.5);
		let linear = Color::from_linear(0.5, 0.5, 0.5);

		assert_ne!(srgb, linear);
		assert_eq!(srgb.components_linear().red, 0.214_04);
		assert_eq!(linear.components_linear().red, 0.5);

		// Converting the stored linear value back gives what the sRGB one started as.
		assert_eq!(srgb.components().red, 0.5);
		assert_eq!(linear.components().red, 0.735_36);
	}

	#[test]
	fn non_linear_values_are_rounded_before_they_are_converted() {
		// The reference rounds the non-linear value to the resolution first, so values that only differ past it describe the same color.
		assert_eq!(
			Color::new(0.300_001, 0.0, 0.0).components_linear(),
			Color::new(0.3, 0.0, 0.0).components_linear()
		);
		assert_eq!(
			Color::new(0.5, 0.700_004, 1.000_004),
			Color::new(0.5, 0.7, 1.0)
		);
	}

	#[test]
	fn components_linear_returns_what_was_constructed() {
		let color = Color::from_linear(-1.25, 0.000_01, 8.5).with_opacity(0.125);
		let linear = color.components_linear();

		assert_eq!(
			(linear.red, linear.green, linear.blue, linear.opacity),
			(-1.25, 0.000_01, 8.5, 0.125)
		);
	}

	#[test]
	fn hex_int_and_hex_str_agree() {
		// The same 24-bit value has to give the same color through either entry point.
		for (hex, text) in [
			(0xFF_00_00_u32, "#FF0000"),
			(0x00_80_00, "#008000"),
			(0x12_34_56, "#123456"),
			(0x00_00_00, "#000000"),
			(0xFF_FF_FF, "#FFFFFF"),
		] {
			assert_eq!(
				Color::from_hex_int(hex).unwrap(),
				Color::from_hex_str(text).unwrap(),
				"{text}"
			);
		}
	}

	#[test]
	fn a_file_written_by_another_implementation_is_read() {
		// These linear values are what another implementation writes for non-linear sRGB -0.5, 0.5 and 1.0 using the mirrored extended sRGB curve.
		let palette: ColorPalette = serde_json::from_str(
			r#"{"colors": [{"components": [-0.21404, 0.21404, 1], "name": "Wide"}]}"#,
		)
		.unwrap();

		let color = palette.colors[0].components();

		assert_eq!(color.red, -0.5);
		assert_eq!(color.green, 0.5);
		assert_eq!(color.blue, 1.0);
	}

	#[test]
	fn the_schema_examples_are_accepted() {
		// The component examples from the format's JSON schema.
		for (components, expected) in [
			("[1, 0.1274, 0.418]", (1.0, 0.1274, 0.418)),
			("[0.592, 0.278, 0.996, 0.9]", (0.592, 0.278, 0.996)),
			("[0.5, 0.5, 0.5, 1.2]", (0.5, 0.5, 0.5)),
			// Out of range RGB is kept as-is, which is the point of the extended range.
			("[-0.2, 1.5, 2.0]", (-0.2, 1.5, 2.0)),
		] {
			let json = format!(r#"{{"colors": [{{"components": {components}}}]}}"#);
			let palette: ColorPalette = serde_json::from_str(&json).unwrap();
			let linear = palette.colors[0].components_linear();

			assert_eq!(
				(linear.red, linear.green, linear.blue),
				expected,
				"{components}"
			);
		}
	}

	#[test]
	fn a_palette_without_colors_is_rejected() {
		// `colors` is the only required key.
		for json in ["{}", r#"{"name": "No colors"}"#] {
			assert!(
				serde_json::from_str::<ColorPalette>(json).is_err(),
				"{json}"
			);
		}
	}

	#[test]
	fn nesting_past_the_parser_limit_is_reported_as_a_json_error() {
		// The parser caps nesting depth, so this must come back as an ordinary error from the file reader rather than a panic or a stack overflow.
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Deep.color-palette");

		fs::write(
			&path,
			format!(
				r#"{{"colors": [{{"components": [{0}{1}]}}]}}"#,
				"[".repeat(10_000),
				"]".repeat(10_000)
			),
		)
		.unwrap();

		assert!(matches!(
			ColorPalette::read_from_file(&path),
			Err(PaletteError::Json(_))
		));
	}

	#[test]
	fn wrong_json_types_are_rejected() {
		for json in [
			r#"{"colors": "not an array"}"#,
			r#"{"colors": [1, 2, 3]}"#,
			r#"{"colors": [{"components": [1, 0, 0]}], "name": 12}"#,
			r#"{"colors": [{"components": [1, 0, 0], "name": 12}]}"#,
			r#"{"colors": [{"components": [1, 0, 0, null]}]}"#,
			r#"{"colors": [{"components": [null, 0, 0]}]}"#,
			r#"{"colors": [{"components": {"red": 1}}]}"#,
			r#"{"colors": [{"name": "No components"}]}"#,
			"[]",
			"null",
		] {
			assert!(
				serde_json::from_str::<ColorPalette>(json).is_err(),
				"{json}"
			);
		}
	}

	#[test]
	fn unknown_fields_are_ignored() {
		// The schema forbids extra fields, but the reader stays lenient so a file from a newer version still loads, which is what the Swift and JavaScript implementations do.
		let palette: ColorPalette = serde_json::from_str(
			r#"{"name": "P", "future": true, "colors": [{"components": [1, 0, 0], "future": true}]}"#,
		)
		.unwrap();

		assert_eq!(palette.name.as_deref(), Some("P"));
		assert_eq!(palette.colors[0].components_linear().red, 1.0);

		// Writing it back drops them, since the format does not define them.
		assert!(!serde_json::to_string(&palette).unwrap().contains("future"));
	}

	#[test]
	fn writing_overwrites_an_existing_file() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Overwrite.color-palette");

		fs::write(&path, "stale content that is not json").unwrap();

		let palette = ColorPalette::new(vec![Color::new(1.0, 0.0, 0.0)]).with_name("New");
		palette.write_to_file(&path).unwrap();

		assert_eq!(ColorPalette::read_from_file(&path).unwrap(), palette);
	}

	#[test]
	fn a_failed_write_does_not_disturb_an_existing_file() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Existing.color-palette");

		let good = ColorPalette::new(vec![Color::new(1.0, 0.0, 0.0)]);
		good.write_to_file(&path).unwrap();
		let original = fs::read_to_string(&path).unwrap();

		// Serialization runs before the file is opened, so the old contents have to survive.
		let bad = ColorPalette::new(vec![Color::from_linear(f64::NAN, 0.0, 0.0)]);
		assert!(bad.write_to_file(&path).is_err());

		assert_eq!(fs::read_to_string(&path).unwrap(), original);
	}

	#[test]
	fn a_written_file_is_exactly_the_serialized_form() {
		let palette = ColorPalette::new(vec![Color::new(0.1, 0.2, 0.3).with_opacity(0.5)])
			.with_name("On Disk");

		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Disk.color-palette");

		palette.write_to_file(&path).unwrap();

		// No byte order mark, no trailing newline, no reformatting.
		assert_eq!(
			fs::read_to_string(&path).unwrap(),
			serde_json::to_string_pretty(&palette).unwrap()
		);
	}

	#[test]
	fn a_byte_order_mark_is_rejected() {
		// The format is defined as UTF-8 without a byte order mark.
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Bom.color-palette");

		fs::write(
			&path,
			format!("\u{FEFF}{}", r#"{"colors": [{"components": [1, 0, 0]}]}"#),
		)
		.unwrap();

		assert!(matches!(
			ColorPalette::read_from_file(&path),
			Err(PaletteError::Json(_))
		));
	}

	#[test]
	fn a_non_utf8_file_reports_an_io_error() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Binary.color-palette");

		fs::write(&path, [0xFF, 0xFE, 0x00, 0x01]).unwrap();

		assert!(matches!(
			ColorPalette::read_from_file(&path),
			Err(PaletteError::Io(_))
		));
	}

	#[test]
	fn a_directory_reports_an_io_error() {
		let temporary_directory = tempfile::tempdir().unwrap();

		assert!(matches!(
			ColorPalette::read_from_file(temporary_directory.path()),
			Err(PaletteError::Io(_))
		));
	}

	#[test]
	fn an_empty_file_reports_a_json_error() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Empty.color-palette");

		fs::write(&path, "").unwrap();

		assert!(matches!(
			ColorPalette::read_from_file(&path),
			Err(PaletteError::Json(_))
		));
	}

	#[test]
	fn a_path_is_accepted_as_a_string_or_a_path() {
		let temporary_directory = tempfile::tempdir().unwrap();
		let path = temporary_directory.path().join("Stringy.color-palette");
		let palette = ColorPalette::new(vec![Color::new(1.0, 0.0, 0.0)]);

		palette.write_to_file(&path).unwrap();

		assert_eq!(
			ColorPalette::read_from_file(path.to_str().unwrap()).unwrap(),
			palette
		);
	}
}
