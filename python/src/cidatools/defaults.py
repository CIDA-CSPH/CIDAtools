import pathlib

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict

from cidatools.persistence import PersistentField, PersistentWrapper

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
    __model__ = CIDADefaultsModel
    __parent__ = None

    # Defaults
    analyst: str | list[str] | None = PersistentField()
    github_token: str | None = PersistentField()

    def __init__(self):
        super().__init__(path=CIDA_PROJECT_DEFAULTS_PATH)
