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
    username: str | None
    password: str | None


class CIDADefaultsModel(BaseSettings, frozen=True):
    model_config = SettingsConfigDict(env_prefix="CIDATOOLS_")
    # Default analyst for new projects, is only used if analyst not specified when creating project.
    analyst: str | list[str] | None = Field(default=None)

    # Host and protocol generally shouldn't need to be set.
    github_host: str = Field(default="github.com")
    github_protocol: str = Field(default="https")
    # Username will need to be set by the user.
    # TODO: we could remove username entirely from the model if keyring.get_credential worked correctly
    #  when username=None on Mac. Revisit if/when https://github.com/jaraco/keyring/issues/784 is addressed.
    github_username: str | None = Field(default=None)


class CIDADefaults(PersistentWrapper):
    __model__ = CIDADefaultsModel
    __parent__ = None

    # Defaults
    analyst: str | list[str] | None = PersistentField()
    github_username: str | None = PersistentField()

    def __init__(self, silent: bool = False):
        # Try to create the defaults path
        try:
            if not CIDA_PROJECT_DEFAULTS_PATH.exists():
                CIDA_PROJECT_DEFAULTS_PATH.parent.mkdir(parents=True, exist_ok=True)
                temp_model = CIDADefaultsModel()
                with open(CIDA_PROJECT_DEFAULTS_PATH, "w+") as f:
                    f.write(temp_model.model_dump_json(indent=4))
        except (PermissionError, FileNotFoundError, FileExistsError, NotADirectoryError) as e:
            print_failure(f"Error when initializing CIDADefaults: {e}")
            raise
        super().__init__(path=CIDA_PROJECT_DEFAULTS_PATH, silent=silent, parent=None)
