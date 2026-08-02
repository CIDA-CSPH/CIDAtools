from importlib import resources

GREEN_CHECK = "\x1b[1m\x1b[32m\u2713\x1b[0m"
RED_XMARK = "\x1b[1m\x1b[31m\u2717\x1b[0m"
YELLOW_TRIANGLE = "\x1b[1m\x1b[33m\u26a0\x1b[0m"
BLUE_INFO = "\x1b[1m\x1b[34m\u2139\x1b[0m"


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
    print(f"\x1b[31m{message}\x1b[0m")


def print_yellow(message: str):
    """
    Prints a yellow message.
    :param message:
    :return:
    """
    print(f"\x1b[33m{message}\x1b[0m")


def print_green(message: str):
    """
    Prints a green message.
    :param message:
    :return:
    """
    print(f"\x1b[32m{message}\x1b[0m")
