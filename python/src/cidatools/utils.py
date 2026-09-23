from collections.abc import Sequence
from importlib import resources

GREEN_CHECK = "\x1b[1m\x1b[32m\u2713\x1b[0m"
RED_XMARK = "\x1b[1m\x1b[31m\u2717\x1b[0m"
YELLOW_TRIANGLE = "\x1b[1m\x1b[33m\u26a0\x1b[0m"
BLUE_INFO = "\x1b[1m\x1b[34m\u2139\x1b[0m"

RED = "\x1b[31m"
YELLOW = "\x1b[33m"
GREEN = "\x1b[32m"
CLEAR = "\x1b[0m"


def get_user_prompt_by_name(name: str) -> str:
    """Gets a user prompt (stored in the 'resources/templates/user_prompts/' directory) as a string
    :param name: The name of the template to retrieve.
    :return:
    """
    return resources.files("cidatools").joinpath("resources/templates/user_prompts").joinpath(name).read_text()


def print_success(message: str):
    """Prints a success message.
    :param message: Message to print.
    :return:
    """
    print(f"{GREEN_CHECK} {message}")


def print_warning(message: str):
    """Prints a warning message.
    :param message: Message to print.
    """
    print(f"{YELLOW_TRIANGLE} {message}")


def print_failure(message: str):
    """Prints a failure message.
    :param message: Message to print.
    :return:
    """
    print(f"{RED_XMARK} {message}")


def print_info(message: str):
    """
    Prints a refresh message.
    :param message:
    :return:
    """
    print(f"{BLUE_INFO} {message}")


def print_red(message: str):
    """
    Prints a red message.
    :param message:
    :return:
    """
    print(f"{RED}{message}{CLEAR}")


def print_yellow(message: str):
    """
    Prints a yellow message.
    :param message:
    :return:
    """
    print(f"{YELLOW}{message}{CLEAR}")


def print_green(message: str):
    """
    Prints a green message.
    :param message:
    :return:
    """
    print(f"{GREEN}{message}{CLEAR}")


def parse_rgb(rgb_hex: str) -> tuple[int, ...]:
    """Convenience function for parsing RGB hex color codes into usable RGB tuples.
    :param rgb_hex: An RGB hex string.
    :return:
    """
    rgb_hex = rgb_hex.lstrip("#")
    return tuple(int(rgb_hex[i : i + 2], 16) for i in range(0, 6, 2))


def lerp_rgb_1d(i: float, cmap: Sequence[Sequence[int]], ccp: Sequence[float]):
    """Function to linearly interpolate between a sequence of RGB colors.
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
