import os
from consts import CIDA_PROJECT_PATH


def _set_project_path(project_path: str):
    os.environ[CIDA_PROJECT_PATH] = project_path

    
def _get_project_path() -> str:
    return os.environ[CIDA_PROJECT_PATH]

