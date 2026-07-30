import json
import pathlib

from pydantic import Field, ValidationError
from pydantic_settings import BaseSettings, SettingsConfigDict

from cidatools.persistence import PersistentField, PersistentWrapper
from cidatools.utils import print_failure, print_success

CIDA_PROJECT_DEFAULTS_PATH = pathlib.Path("~/.cida/project_defaults.json").expanduser()

CIDA_PROJECT_DEFAULT_FOLDERS = (
    "Admin",
    "Background",
    "Code",
    "DataRaw",
    "DataProcessed",
    "Dissemination",
    "Reports",
)


class CIDADefaultsModel(BaseSettings, frozen=True):
    model_config = SettingsConfigDict(env_prefix="CIDATOOLS_")
    # Default analyst for new projects, is only used if analyst not specified when creating project.
    analyst: str | list[str] | None = Field(default=None)

    # Token used for GitHub operations. Should have access to CSPH-CIDA organization and have 'repo' scope.
    github_token: str | None = Field(default=None)


class CIDADefaults(PersistentWrapper):
    # Class variables
    path: pathlib.Path = CIDA_PROJECT_DEFAULTS_PATH
    model_type = CIDADefaultsModel
    parent = None

    # Defaults
    analyst: str | list[str] | None = PersistentField()
    github_token: str | None = PersistentField()


def _write_project_defaults(defaults_path: pathlib.Path, defaults: CIDADefaults = None):
    try:
        defaults_path.parent.mkdir(parents=True)
        new_defaults = defaults if defaults is not None else CIDADefaults()
        tmp_defaults_path = defaults_path.with_suffix(".tmp")
        with open(tmp_defaults_path, "w") as f:
            f.write(new_defaults.model_dump_json(indent=4))
        tmp_defaults_path.replace(defaults_path)
        print_success(f"Wrote updated defaults to {defaults_path}.")
        return new_defaults
    except (FileNotFoundError, ValidationError) as e:
        print_failure(f"Unable to write defaults: {e}")


def _read_project_defaults(
    defaults_path: pathlib.Path = CIDA_PROJECT_DEFAULTS_PATH, create: bool = True
) -> CIDADefaultsModel | None:
    """Loads the cidatools project defaults from file.
    :param defaults_path:
    :param create: Create the project defaults file if it doesn't already exist.
    :return: An instance of CIDAProjectDefaults.
    """
    if not defaults_path.exists():
        if create:
            _write_project_defaults(defaults_path)
        else:
            print_failure(
                f"CIDA defaults path {defaults_path} does not exist, and create=False."
            )
            return None
    else:
        try:
            with open(defaults_path, "r") as f:
                return CIDADefaultsModel.model_validate(json.load(f))
        except (FileNotFoundError, ValidationError) as e:
            print_failure(f"Unable to load defaults: {e}")
        return None
    try:
        with open(defaults_path, "r") as f:
            return CIDADefaultsModel.model_validate(json.load(f))
    except (FileNotFoundError, ValidationError) as e:
        print_failure(f"Unable to read config file: {e}")
    return None


# class CIDADefaults:
#     default_analyst = PersistentField(CIDADefaultsModel)
#
#     def __init__(self, path: pathlib.Path = CIDA_PROJECT_DEFAULTS_PATH):
