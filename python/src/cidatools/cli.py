import argparse
import functools
import math
import pathlib
import sys
from importlib.metadata import version

from cidatools.project import project_status

# The block between the 'fmt: off' and 'fmt: on' blocks below is necessary to prevent
# code formatters from modifying the internal spacing the of the banner.
# fmt: off
# Banner generated with https://www.asciiart.eu/text-to-ascii-art, using the 'Sub-zero' font.
CIDATOOLS_BANNER = r"""
 ______     __     _____     ______     ______   ______     ______     __         ______
/\  ___\   /\ \   /\  __-.  /\  __ \   /\__  _\ /\  __ \   /\  __ \   /\ \       /\  ___\
\ \ \____  \ \ \  \ \ \/\ \ \ \  __ \  \/_/\ \/ \ \ \/\ \  \ \ \/\ \  \ \ \____  \ \___  \
 \ \_____\  \ \_\  \ \____-  \ \_\ \_\    \ \_\  \ \_____\  \ \_____\  \ \_____\  \/\_____\
  \/_____/   \/_/   \/____/   \/_/\/_/     \/_/   \/_____/   \/_____/   \/_____/   \/_____/
"""

# Banner footer with version and maintenance info.
CIDATOOLS_BANNER_FOOTER = f"""
CIDA Tools Version {version("cidatools")} (Python/CLI)
Maintained by the CIDA Research Tools Committee.
"""

# Epilog appears at the end of the help screen.
CIDATOOLS_EPILOG = f"""
Questions, Comments or Suggestions: mailto:CIDA-RT@olucdenver.onmicrosoft.com
Found a bug? https://github.com/CIDA-CSPH/CIDAtools/issues
"""
# fmt: on

# This is the 'Vibrant Summer' palette from https://coolors.co/palette/ff595e-ffca3a-8ac926-1982c4-6a4c93
CIDATOOLS_DEFAULT_COLOR_PALETTE = ["FF595E", "FFCA3A", "8AC926", "1982C4", "6A4C93"]


def cli():
    """Entrypoint for CLI commands.
    :return:
    """
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=banner(),
        epilog=CIDATOOLS_EPILOG,
    )
    # Print version information.
    parser.add_argument("-v", "--version", action="version", version=banner())
    parser.add_argument(
        "-C",
        type=pathlib.Path,
        help="Subsequent commands will reference the project in the given directory instead of the working directory.",
    )
    # Each subcommand is a subparser
    subparsers = parser.add_subparsers(dest="command", required=True)

    status = subparsers.add_parser(name="status")
    status.set_defaults(func=lambda a: project_status(project_root=a.C))

    args = parser.parse_args()
    args.func(args)


def _parse_rgb(rgb_hex: str):
    """Convenience function for parsing RGB hex color codes into usable RGB tuples.
    :param rgb_hex: An RGB hex string.
    :return:
    """
    rgb_hex = rgb_hex.lstrip("#")
    return tuple(int(rgb_hex[i : i + 2], 16) for i in range(0, 6, 2))


def _lerp_rgb_1d(i: float, cmap: list[tuple[int, int, int]], ccp: list[float]):
    """Function to determine a color
    :param i: The normalized value [0,1] which we are computing colors for.
    :param cmap: The colormap to use for the lerp.
    :param ccp: The list of indices in the interval [0, 1] where each color begins.
    :return:
    """
    # Clamp i within the range.
    if i <= 0:
        return cmap[0]
    elif i >= 1:
        return cmap[-1]
    # Find the low index (first color in the lerp)
    low_idx = max(j for j in range(len(ccp)) if i - ccp[j] >= 0)
    # High index is the next color
    high_idx = low_idx + 1
    # Get both colors from the map
    c1 = cmap[low_idx]
    c2 = cmap[high_idx]
    # Normalize i from a global interpolation to a local (between two colors) value
    i_norm = (i - ccp[low_idx]) / (ccp[high_idx] - ccp[low_idx])
    # Lerp between the colors.
    return tuple(round(e[0] * (1 - i_norm) + e[1] * i_norm) for e in zip(c1, c2))


@functools.lru_cache(maxsize=1)
def banner():
    # Parse the default color palette into something usable.
    parsed_colors = list(map(_parse_rgb, CIDATOOLS_DEFAULT_COLOR_PALETTE))
    # Split banner string into rows.
    banner_rows = CIDATOOLS_BANNER.split("\n")
    # Get the length of the banner.
    banner_width = max(map(len, banner_rows))
    # Get the points where the colors change on the banner.
    color_change_points = [
        i / (len(CIDATOOLS_DEFAULT_COLOR_PALETTE) - 1) for i in range(len(CIDATOOLS_DEFAULT_COLOR_PALETTE))
    ]
    # Compute the colors for each column of the banner.
    c_c = [_lerp_rgb_1d(i=i / banner_width, cmap=parsed_colors, ccp=color_change_points) for i in range(banner_width)]
    # Compute banner width.
    footer_rows = CIDATOOLS_BANNER_FOOTER.split("\n")
    total_width = max(banner_width, max(map(len, footer_rows)))
    # Apply color codes to each character in the banner string.
    colored_banner_rows = (
        "".join(" " if s_i == " " else f"\033[38;2;{c[0]};{c[1]};{c[2]}m{s_i}\033[0m" for c, s_i in zip(c_c, row))
        for row in banner_rows
    )
    # Center the banner.
    padded_banner_rows = (
        (" " * int(math.ceil(total_width - len(orig_row)) / 2))
        + color_row
        + (" " * int(math.floor(total_width - len(orig_row)) / 2))
        for orig_row, color_row in zip(banner_rows, colored_banner_rows)
    )
    # Return the whole banner.
    return "\n".join(row.center(total_width) for row in padded_banner_rows) + "\n".join(
        row.center(total_width) for row in footer_rows
    )


if __name__ == "__main__":
    cli()
    sys.exit(0)
