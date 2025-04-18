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
use simple_color_palette::{ColorPalette, Color};

let palette = ColorPalette::new(
	vec![
		Color::new(1.0, 0.0, 0.0, None, Some("Red".into())),
		Color::new(0.0, 1.0, 0.0, None, Some("Green".into())),
	],
	Some("Traffic Lights".into()),
);

let path = std::path::Path::new("Traffic Lights.color-palette");

// Save palette
palette.write_to_file(path);

// Load palette
let loadedPalette = ColorPalette::read_from_file(path);
```
