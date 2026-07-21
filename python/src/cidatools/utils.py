
GREEN_CHECK = "\x1b[32m\u2713\x1b[0m"
RED_XMARK = "\x1b[31m\u2717\x1b[0m"


def print_success(message: str):
    """ Prints a success message.
    :param message: Message to print.
    :return:
    """
    print(f"{GREEN_CHECK} {message}")


def print_failure(message: str):
    """ Prints a failure message.
    :param message: Message to print.
    :return:
    """
    print(f"{RED_XMARK} {message}")

