import pathlib


def _parse_mount_path(path: str | pathlib.Path):
    """ Parses a CIDA network share path into the prefix (mount path) and branch.

    :param path:
    :return:
    """
    # Parse as path if provided as string
    path = pathlib.Path(path) if isinstance(path, str) else path

