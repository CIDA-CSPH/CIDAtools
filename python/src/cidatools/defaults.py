import json
from typing import Literal

from pydantic import PrivateAttr, Field
from pydantic_settings import BaseSettings, SettingsConfigDict

import pathlib

from cidatools.utils import filesize_mtime_cache, print_failure

CIDA_PROJECT_DEFAULTS_PATH = pathlib.Path("~/.cida/project_defaults.json").expanduser()


class CIDAProjectDefaults(BaseSettings, frozen=True):
    _source_file: pathlib.Path = PrivateAttr()
    model_config = SettingsConfigDict(env_prefix="CIDA_")

    # These settings are hardcoded for now, and can not be changed.
    _directory_name: Literal[".cida"] = PrivateAttr(default=".cida")
    _github_organization: Literal["https://github.com/CIDA-CSPH/"] = PrivateAttr(
        default="https://github.com/CIDA-CSPH/"
    )
    _project_config_name: Literal["project.json"] = PrivateAttr(default="project.json")

    # Default analyst is a global variable which can be persisted to settings.
    default_analyst: str | None = Field(default=None)


def read_project_defaults(
    path: pathlib.Path = CIDA_PROJECT_DEFAULTS_PATH, create: bool = True
) -> CIDAProjectDefaults:
    """Loads the cidatools project defaults from file.
    :param path:
    :param create: Create the project defaults file if it doesn't already exist.
    :return: An instance of CIDAProjectDefaults.
    """
    if not path.exists():
        if create:
            path.parent.mkdir(parents=True)
            new_defaults = CIDAProjectDefaults()
            with open(path, "w") as f:
                f.write(new_defaults.model_dump_json(indent=4))
            return new_defaults
        else:
            print_failure(f"Path {path} does not exist, and create=False.")
            return CIDAProjectDefaults()
    else:
        return _read_project_defaults(path)


@filesize_mtime_cache
def _read_project_defaults(path: pathlib.Path) -> CIDAProjectDefaults:
    with open(path, "r") as f:
        return CIDAProjectDefaults.model_validate({"source_file": path} | json.load(f))
