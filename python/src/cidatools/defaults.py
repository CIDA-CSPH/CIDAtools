import pathlib

from pydantic import BaseModel, Field
from pydantic_settings import BaseSettings, SettingsConfigDict

from cidatools.persistence import PersistentField, PersistentWrapper
from cidatools.utils import print_failure

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


class GithubCredentials(BaseModel, frozen=True):
    host: str = Field(default="github.com")
    protocol: str = Field(default="https")
    username: str
    password: str


class CIDADefaultsModel(BaseSettings, frozen=True):
    model_config = SettingsConfigDict(env_prefix="CIDATOOLS_", env_nested_delimiter="__")
    # Default analyst for new projects, is only used if analyst not specified when creating project.
    analyst: str | list[str] | None = Field(default=None)

    # Token used for GitHub operations. Should have access to CSPH-CIDA organization and have 'repo' scope.
    github_creds: GithubCredentials | None = Field(default=None)


class CIDADefaults(PersistentWrapper):
    __model__ = CIDADefaultsModel
    __parent__ = None

    # Defaults
    analyst: str | list[str] | None = PersistentField()
    github_creds: GithubCredentials | None = PersistentField()

    def __init__(self):
        # Try to create the defaults path
        try:
            if not CIDA_PROJECT_DEFAULTS_PATH.exists():
                CIDA_PROJECT_DEFAULTS_PATH.parent.mkdir(parents=True, exist_ok=True)
                temp_model = CIDADefaultsModel()
                with open(CIDA_PROJECT_DEFAULTS_PATH, "w+") as f:
                    f.write(temp_model.model_dump_json(indent=4))
        except (PermissionError, FileNotFoundError, FileExistsError, NotADirectoryError) as e:
            print_failure(f"Error when initializing CIDADefaults: {e}")
        super().__init__(path=CIDA_PROJECT_DEFAULTS_PATH)
