import argparse
import functools
import math
import pathlib
import sys
from importlib.metadata import version

from cidatools.defaults import CIDA_PROJECT_DEFAULT_FOLDERS, CIDADefaults
from cidatools.git import create_empty_github_repository, create_github_repository_from_template, setup_github
from cidatools.project import create_github_project, create_local_project, current_project, project_status
from cidatools.utils import print_failure

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
CIDATOOLS_EPILOG = """
For more information on specific commands, use the help flag with a subcommand
(i.e. cidatools create --help)

Questions, Comments or Suggestions: mailto:CIDA-RT@olucdenver.onmicrosoft.com
Found a bug? https://github.com/CIDA-CSPH/CIDAtools/issues
"""
# fmt: on

# This is the 'Vibrant Summer' palette from https://coolors.co/palette/ff595e-ffca3a-8ac926-1982c4-6a4c93
CIDATOOLS_DEFAULT_COLOR_PALETTE = ["FF595E", "FFCA3A", "8AC926", "1982C4", "6A4C93"]


def cli() -> int:
    """Entrypoint for CLI commands.
    :return:
    """
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=cidatools_banner(),
        epilog=CIDATOOLS_EPILOG,
    )
    # Print version information.
    parser.add_argument("-v", "--version", action="version", version=cidatools_banner())
    parser.add_argument(
        "-C",
        type=pathlib.Path,
        help="Subsequent commands will reference the project in the given directory instead of the working directory.",
    )
    # Each subcommand is a sub-parser
    subparsers = parser.add_subparsers(dest="command", required=True)

    # Project Status Sub-parser
    status = subparsers.add_parser(name="status", help="Get status of a CIDA project.")
    status.set_defaults(func=lambda a: project_status(project_root=a.C))

    # Project Creation Sub-parser
    create = subparsers.add_parser(name="create", help="Create a new CIDA project or GitHub repo.")

    # Required arguments for project/repo creation.
    create.add_argument("target", choices=["github-project", "github-repo", "project"])
    create.add_argument(
        "directory",
        type=pathlib.Path,
        default=None,
        help="The directory where the new project will be created. By default, the current working directory will be used.",
    )
    # Optional arguments for project/repo creation.
    create.add_argument(
        "--template", type=str, help="If specified, will to create the new repository using this template name."
    )
    create.add_argument(
        "--description",
        type=str,
        help="If specified, will be used as the description of the new GitHub repository. (No effect for non-GitHub projects)",
    )
    create.add_argument(
        "--repo-name",
        type=str,
        help="If specified, will be used as the name of the new GitHub repository, overriding the default directory name (No effect for non-GitHub projects)",
    )
    create.add_argument(
        "--create-folders",
        type=str,
        nargs="+",
        help="Which folders to create when creating a local project (no effect for GitHub projects, as this is determined by the template used).",
        default=CIDA_PROJECT_DEFAULT_FOLDERS,
    )
    # Repository visibility
    visibility = create.add_mutually_exclusive_group()
    visibility.add_argument("--private", action="store_true", help="If specified, make a private repository.")
    visibility.add_argument(
        "--internal",
        action="store_true",
        default=True,
        help="If specified, make a internal (visible to other members of CIDA) repository.",
    )
    # Project metadata
    create.add_argument(
        "--project-name", type=str, default=None, help="If specified, will set the project name for the new project"
    )
    create.add_argument(
        "--analyst", type=str, default=None, nargs="+", help="If specified, will set the analyst(s) for the new project"
    )
    create.add_argument(
        "--pi",
        type=str,
        default=None,
        nargs="+",
        help="If specified, will set the principal investigator for the new project",
    )
    create.add_argument(
        "--data-location", type=str, default=None, help="If specified, will set the data location for the new project"
    )
    create.add_argument(
        "--git-location",
        type=str,
        default=None,
        help="If specified, will set the git location for the new project (No effect for a GitHub project, as this value is set to the URL of the created repository).",
    )
    create.set_defaults(func=_cli_create)

    # Project Metadata Get
    get = subparsers.add_parser(name="get", help="Get CIDA project metadata or defaults.")
    get.add_argument("field", type=str, help="The metadata field to get")
    get.add_argument("--default", action="store_true", help="If specified, will search the default values only.")
    get.set_defaults(func=_cli_get)

    # Project Metadata Set
    set_ = subparsers.add_parser(name="set", help="Set CIDA project metadata or defaults.")
    set_.add_argument("field", type=str, help="The metadata field to set.")
    set_.add_argument("value", type=str, nargs="+", help="The new metadata value.")
    set_.add_argument("--default", action="store_true", help="If specified, will set the default value.")
    set_.set_defaults(func=_cli_set)

    # Project Metadata Unset
    unset = subparsers.add_parser(name="unset", help="Unset CIDA project metadata or defaults.")
    unset.add_argument("field", type=str, help="The metadata field to unset.")
    unset.add_argument("--default", action="store_true", help="If specified, will unset the default value.")
    unset.set_defaults(func=_cli_unset)

    # Setup Commands
    setup = subparsers.add_parser(name="setup", help="Setup CIDAtools integrations (GitHub, etc).")
    setup.add_argument("target", choices=["github"], help="Which component to set up.")
    setup.set_defaults(func=_cli_setup)
    # Parse the arguments and dispatch to the sub-parser handler
    args = parser.parse_args()
    return args.func(args)


def _cli_get(args: argparse.Namespace) -> int:
    """Entrypoint for the CLI 'get' subcommand.
    :param args: The parsed argument namespace.
    :return: Integer status code
    """
    # Check if default flag is requested.
    if args.default:
        wrapper = CIDADefaults()
    else:
        wrapper = current_project(project_root=args.C)
    # If we can't obtain the wrapper, exit.
    if wrapper is None:
        return 1
    # Check if the requested field is valid
    if args.field in wrapper.__model__.model_fields:
        print(getattr(wrapper, args.field))
        return 0
    return 1


def _cli_set(args: argparse.Namespace) -> int:
    """Entrypoint for the CLI 'set' subcommand.
    :param args: The parsed argument namespace.
    :return: Integer status code
    """
    # Check if default flag is requested.
    if args.default:
        wrapper = CIDADefaults()
    else:
        wrapper = current_project(project_root=args.C)
    # If we can't obtain the wrapper, exit.
    if wrapper is None:
        return 1
    # Check if the requested field is valid
    if args.field in wrapper.__model__.model_fields:
        # Auto-unpack a singleton value so we don't store a list unnecessarily.
        value_ = args.value[0] if isinstance(args.value, list) and len(args.value) == 1 else args.value
        setattr(wrapper, args.field, value_)
        return 0
    return 1


def _cli_unset(args: argparse.Namespace) -> int:
    """Entrypoint for the CLI 'unset' subcommand.
    :param args: The parsed argument namespace.
    :return: Integer status code
    """
    # Check if default flag is requested.
    if args.default:
        wrapper = CIDADefaults()
    else:
        wrapper = current_project(project_root=args.C)
    # If we can't obtain the wrapper, exit.
    if wrapper is None:
        return 1
    # Check if the requested field is valid
    if args.field in wrapper.__model__.model_fields:
        setattr(wrapper, args.field, None)
        return 0
    return 1


def _cli_setup(args: argparse.Namespace) -> int:
    """Entrypoint for the CLI 'setup' subcommand.
    :param args: The parsed argument namespace.
    :return: Integer status code
    """
    if args.target == "github":
        setup_github(force_pat=False)
        return 0
    else:
        print_failure(f"Unknown target '{args.target}'")
        return 1


def _cli_create(args: argparse.Namespace) -> int:
    """Entrypoint for the CLI 'create' subcommand.
    :param args: The parsed argument namespace.
    :return: Integer status code
    """
    project_directory = args.directory.absolute()
    if args.target == "github-project":
        # If not specified, we create a default (empty) repository.
        # template_name = "empty" if args.template is None else args.template
        # Create a new GitHub project.
        create_result = create_github_project(
            project_name=project_directory.name if args.project_name is None else args.project_name,
            project_root=project_directory,
            repository_name=args.repo_name,
            template_name=args.template,
            description=args.description,
            visibility="private" if args.private else "internal" if args.internal else "public",
            analyst=args.analyst,
            principal_investigator=args.pi,
            data_location=args.data_location,
        )

        if create_result is None:
            return 1
        else:
            return 0
    elif args.target == "project":
        create_result = create_local_project(
            project_name=project_directory.name if args.project_name is None else args.project_name,
            project_root=project_directory,
            principal_investigator=args.pi,
            analyst=args.analyst,
            data_location=args.data_location,
            git_location=args.git_location,
        )
        if create_result is None:
            return 1
        else:
            return 0
    elif args.target == "github-repo":
        if args.template is None:
            repo_url = create_empty_github_repository(
                name=project_directory.name if args.repo_name is None else args.repo_name,
                description=args.description,
                visibility="private" if args.private else "internal" if args.internal else "public",
            )
        else:
            repo_url = create_github_repository_from_template(
                name=project_directory.name if args.repo_name is None else args.repo_name,
                description=args.description,
                visibility="private" if args.private else "internal" if args.internal else "public",
                template_name=args.template,
            )
        if repo_url is None:
            return 1
        else:
            return 0
    return 1


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
def banner(banner_str: str, color_palette: list[str], footer: str | None = None):
    # Parse the default color palette into something usable.
    parsed_colors = list(map(_parse_rgb, color_palette))
    # Split banner string into rows.
    banner_rows = banner_str.split("\n")
    # Get the length of the banner.
    banner_width = max(map(len, banner_rows))
    # Get the points where the colors change on the banner.
    color_change_points = [i / (len(color_palette) - 1) for i in range(len(color_palette))]
    # Compute the colors for each column of the banner.
    c_c = [_lerp_rgb_1d(i=i / banner_width, cmap=parsed_colors, ccp=color_change_points) for i in range(banner_width)]
    # Compute banner width.
    footer_rows = footer.split("\n") if footer is not None else []
    total_width = max(banner_width, max(map(len, footer_rows))) if len(footer_rows) > 0 else banner_width
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


def cidatools_banner():
    return banner(
        banner_str=CIDATOOLS_BANNER, color_palette=CIDATOOLS_DEFAULT_COLOR_PALETTE, footer=CIDATOOLS_BANNER_FOOTER
    )


if __name__ == "__main__":
    exit_code = cli()
    sys.exit(exit_code)
