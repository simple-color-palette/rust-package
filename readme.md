# simple-color-palette

> A Rust implementation of the [Simple Color Palette](https://simplecolorpalette.com) format — a minimal JSON-based file format for defining color palettes

*Feedback wanted on the API.*

## Install

```sh
cargo add simple-color-palette
```

## Usage

[API documentation](https://docs.rs/simple-color-palette)

```rust
use simple_color_palette::{Color, ColorPalette};

fn main() -> Result<(), simple_color_palette::PaletteError> {
	let palette = ColorPalette::new(vec![
		Color::new(1.0, 0.0, 0.0).with_name("Red"),
		Color::new(0.0, 1.0, 0.0).with_name("Green"),
	])
	.with_name("Traffic Lights");

	// Anywhere writable. A real program would use a path the user chose.
	// Deliberately not the same path as the crate-level example, since doctests run in parallel.
	let path = std::env::temp_dir().join("traffic-lights.color-palette");

	// Save palette
	palette.write_to_file(&path)?;

	// Load palette
	let loaded_palette = ColorPalette::read_from_file(&path)?;

	assert_eq!(loaded_palette, palette);
	Ok(())
}
```

## Color space

Colors are stored in extended linear sRGB, which is what the format writes. `Color::new()` and `Color::components()` work in ordinary non-linear sRGB, which is what color pickers and design tools use, so most code never touches the linear values. Use `Color::from_linear()` and `Color::components_linear()` only for color math.

Values are rounded to 5 decimal places, opacity is clamped to `0.0...1.0` and left out of the file when it is `1.0`, and RGB is never clamped, which is how the format carries wide gamut and HDR colors.

The rounding applies to the stored linear values, so `Color::new()` does not round trip exactly: `Color::new(0.001, 0.0, 0.0).components().red` is `0.00103`. Whole 8-bit values, which is what the hex initializers use, are exact.

There is no hex writer, since a color can fall outside what `#RRGGBB` can express. Scale the components by `255.0` and clamp if that is what you need.

## Format

Files written by this package follow the [Simple Color Palette specification](https://github.com/simple-color-palette/spec).

- File extension: `.color-palette`
- Encoding: UTF-8 without a byte order mark
- Media type: `application/x.sindresorhus.simple-color-palette+json`
- Uniform type identifier: `com.sindresorhus.simple-color-palette`
