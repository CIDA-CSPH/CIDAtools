import functools
import pathlib
import json
import re
import shutil
from enum import StrEnum
from importlib import resources
from typing import Literal

from jinja2 import Template
from pydantic import BaseModel, model_validator, Field, PrivateAttr
from pydantic_settings import BaseSettings, SettingsConfigDict

from cidatools.consts import CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME
from cidatools.defaults import read_project_defaults, CIDAProjectDefaults
from cidatools.utils import print_success, print_failure, print_warning, filesize_mtime_cache


class CIDAProjectModel(BaseModel, frozen=True):
    project_name: str | None = Field(serialization_alias="project_name")
    principal_investigator: str | None = Field(serialization_alias="principal_investigator")
    data_location: str | None = Field(serialization_alias="data_location")
    git_location: str | None = Field(serialization_alias="git_location")
    analyst: str | list[str] | None = Field(serialization_alias="analyst")


class CIDAProject:

    def __init__(self, source_file: pathlib.Path):
        """ The user-facing interface for a CIDA project.
        :param source_file: The path to the file which this loads.
        """
        self._source_file: pathlib.Path = source_file

    def __write_property(self, **kwargs):
        inst_copy = _read_config(self._source_file).model_copy(update={k: v for k, v in kwargs.items()})
        with open(self._source_file, "w") as cf:
            cf.write(inst_copy.model_dump_json(indent=4))

    def __read_defaults(self) -> CIDAProjectDefaults:
        return read_project_defaults(self._source_file)

    def __read_property(self, property_name: str):
        # Read (if changed) or access the loaded config.
        config_ = _read_config(self._source_file)

        # Check if the property is set in the loaded config.
        property_value = getattr(config_, property_name)

        # If the property is not set in the loaded config, check the defaults.
        defaults_ = _read
        return None

    @property
    def project_name(self):
        return self.project_name_

    @project_name.setter
    def project_name(self, project_name: str) -> None:
        self.project_name_ = project_name
        self.__write_property(project_name=project_name)

    @property
    def principal_investigator(self):
        return self.principal_investigator_

    @property
    def data_location(self):
        return self.data_location_

    @property
    def git_location(self):
        return self.git_location_

    @property
    def analyst(self):
        return self.analyst_


@filesize_mtime_cache
def _read_config(config_path: pathlib.Path) -> CIDAProjectModel:
    """ Deserializes a CIDA project JSON file into a `CIDAProject` object.
    :param config_path:
    :return: A CIDAProject object.
    """
    with open(config_path, "r") as f:
        return CIDAProjectModel.model_validate({"source_file": config_path} | json.load(f))


def _write_config(project_root: pathlib.Path, config: CIDAProject, overwrite: bool = False) -> None:
    """ Serializes a `CIDAProject` object into a CIDA project JSON file.
    :param project_root: The root directory for the project.
    :param config: The CIDAProject object to be serialized.
    :param overwrite: Whether to overwrite existing serialized JSON file.
    :return:
    """
    if not project_root.is_dir():
        print_failure(f"project_root {project_root} is not a directory, ensure directory exists.")
        return

    # Directory containing the config file.
    config_dir_path = project_root.joinpath(CIDA_DIRECTORY_NAME)
    config_dir_path.mkdir(parents=True, exist_ok=True)

    # Path to config within the directory.
    config_path = config_dir_path.joinpath(CIDA_PROJECT_CONFIG_NAME)

    # If path exists, don't overwrite unless overwrite=True.
    if overwrite or not (config_path.exists() and config_path.is_file()):
        with open(config_path, "w") as f:
            f.write(config.model_dump_json(indent=4))
        print_success(f"Created new project JSON at {config_path}.")
    # Otherwise, print a fail message.
    else:
        #log.warning(f"{config_path} already exists, will not overwrite. Pass overwrite=True to force an overwrite.")
        print_failure(f"CIDA project config already exists at {config_path}, will not overwrite.")


def _write_templated_readme(project_root: pathlib.Path, template_name: str, config: CIDAProject, overwrite: bool = False, subdir: str = None) -> None:
    """ Writes a templated READMe to te
    :param project_root: The path where the README will be written.
    :param template_name: The name of the template to use (must be one of the template names in resources/templates/readme/)
    :param config: The CIDAProject object, used to populate the templates with project information.
    :param overwrite: Whether to overwrite the existing README file, if present.
    :param subdir: The subdirectory to use for the README file.
    :return:
    """
    if not project_root.is_dir():
        print_failure(f"project_root {project_root} is not a directory, ensure directory exists.")
        return

    # Construct the path to the README.
    if subdir is not None:
        subdir_path = project_root.joinpath(subdir)
        subdir_path.mkdir(parents=True, exist_ok=True)
        readme_path = subdir_path.joinpath("README.md")
    else:
        readme_path = project_root.joinpath("README.md")

    # Only make the README if it doesn't already exist, or if we specified to overwrite
    if overwrite or not (readme_path.exists() and readme_path.is_file()):
        # Obtain the template file using the provided template name.
        template_str = resources.files("cidatools").joinpath("resources/templates/readme/").joinpath(template_name).read_text()

        # Populate the template with information from the CIDAProject config.
        rendered_template_str = Template(template_str).render(config=config)

        # Write the rendered template to file.
        with open(readme_path, "w") as f:
            f.write(rendered_template_str)

        print_success(f"Created README.md at {readme_path.relative_to(project_root)}.")
    # Otherwise print an error message.
    else:
        #log.warning(f"{readme_path} already exists, will not overwrite. Pass overwrite=True to force an overwrite.")
        print_failure(f"{readme_path.relative_to(project_root)} already exists, will not overwrite.")


def _write_rprofile(project_root: pathlib.Path) -> None:
    """ Writes a .Rprofile file.
    :param project_root: The path where the .Rprofile will be written.
    :return:
    """
    # Read the template Rprofile.
    rprofile_block = resources.files("cidatools").joinpath("resources").joinpath("DefaultCIDARprofile.R").read_text()

    # Get the path to the rprofile in this directory.
    rprofile_path = project_root.joinpath(".Rprofile")

    # If the .Rprofile already exists, parse it for the block we will add
    if rprofile_path.exists() and rprofile_path.is_file():
        with open(rprofile_path, "r") as f:
            rprofile_contents = f.read()
        # Only add the block if we don't see it in the .Rprofile.
        if re.match("(?s)# BEGIN CIDATools Added\n.*?# END CIDATools Added", rprofile_contents) is None:
            # Append the CIDAtools block to the file
            with open(rprofile_path, "a+") as f:
                f.write(rprofile_block)
            print_success(f"Appended CIDAtools block to .Rprofile.")
        else:
            print_failure(".Rprofile already contains a CIDAtools block.")
    else:
        with open(rprofile_path, "w") as f:
            f.write(rprofile_block)
        print_success("Created .Rprofile and added CIDAtools block.")


def _write_rproj(project_root: pathlib.Path, config: CIDAProject):
    """ Creates a .Rproj file in the current directory if None exists.
    :param project_root: The root directory for the project.
    :param config: The CIDAProject object, used to generate a name for the Rproj file if it doesn't exist.
    :return:
    """
    if not project_root.is_dir():
        print_failure(f"project_root {project_root} is not a directory, ensure directory exists.")
        return

    # Generate the name for the project based on the project name, or use a default if not available.
    rproj_path = project_root.joinpath(f"{config.project_name or 'CIDAProject'}.Rproj")

    if rproj_path.exists() and rproj_path.is_file():
        print_failure(f"{rproj_path.relative_to(project_root)} already exists, will not create new .Rproj.")
    else:
        # The template file for the CIDA project.
        with resources.as_file(resources.files("cidatools").joinpath("resources").joinpath("DefaultCIDAProject.Rproj")) as default_rproj:
            shutil.copy(default_rproj, rproj_path)
        # Print success
        print_success(f"Created {rproj_path.relative_to(project_root)}.")


def _write_gitignore(project_root: pathlib.Path):
    """ Creates a .gitignore file at the given path if None exists.
    :param project_root: The root directory for the project.
    """
    if not project_root.is_dir():
        print_failure(f"project_root {project_root} is not a directory, ensure directory exists.")
        return

    # Generate gitignore path
    gitignore_path = project_root.joinpath(".gitignore")

    if gitignore_path.exists() and gitignore_path.is_file():
        print_failure(f"{gitignore_path.relative_to(project_root)} already exists, will not overwrite.")
    else:
        # Template gitignore
        with resources.as_file(resources.files("cidatools").joinpath("resources").joinpath("gitignore")) as default_gitignore:
            shutil.copy(default_gitignore, gitignore_path)
        # Print success
        print_success(f"Created {gitignore_path.relative_to(project_root)}.")


def project_status(project_root: pathlib.Path = None) -> CIDAProject:
    """ Prints the status of the CIDA project at `project_root`.

    If project_root is not specified, use the current working directory.
    :param project_root: The root directory for the project.
    :return:
    """
    if project_root is None:
        project_root = pathlib.Path.cwd().absolute()

    cur_project = current_project(project_root)
    if cur_project is None:
        print_failure("No CIDA project found.")
        return
    else:
        print_success(f"Loaded CIDA project.")

    # Check issues
    print("Project Config: ")
    longest_key = max(len(k) for k in cur_project.__dict__.keys())
    for key, val in cur_project.__dict__.items():
        print(f"{key.ljust(longest_key)} : {val}")

    # Check for Git repository.


def current_project(project_root: pathlib.Path = None) -> CIDAProject | None:
    """ Returns the currently active CIDA project or None

    Searches recursively for a CIDA project config file, starting at the working directory and working upwards
    toward the root directory.

    :param project_root: The root directory for the project.
    :return: A CIDAProject object or None
    """
    # Accept a user-provided project path, or use the working directory if none is provided.
    project_root = pathlib.Path.cwd().absolute() if project_root is None else project_root

    # Quit when we reach the root directory.
    while project_root != project_root.parent:
        # Check the current directory for a CIDA folder
        project_path = project_root.joinpath(CIDA_DIRECTORY_NAME)
        # If the path exists, check if it is a folder
        if project_path.exists():
            # If it is a folder, check if the config file exists inside.
            if project_path.is_dir():
                config_path = project_path.joinpath(CIDA_PROJECT_CONFIG_NAME)
                # If the config file exists, parse JSON and return the object.
                if config_path.is_file():
                    return CIDAProject(config_path)
                else:
                    print_failure(f"Project directory found at {project_path} but there is no {CIDA_PROJECT_CONFIG_NAME} file inside.")
                    return None
            else:
                print_failure(f"'{project_path}' is not a directory.")
                return None
        else:
            project_root = project_root.parent
    # If we find nothing, return None
    return None


def create_local_project(
        project_root: pathlib.Path = None,
        project_name: str = None,
        principal_investigator: str = None,
        analyst: str | list[str] = None,
        data_location: str = None,
        git_location: str = None
) -> CIDAProject:
    """ Creates a CIDA project structure at `project_root`.

    This function operates offline, does not configure an associated GitHub repository, and uses the basic/default project template.
    :param project_root:
    :param project_name:
    :param principal_investigator:
    :param analyst:
    :param data_location:
    :param git_location:
    :return:
    """
    # If the project name does not exist, use a placeholder.
    project_name = "CIDAProject" if project_name is None else project_name

    # If the project path is not specified, use the working directory.
    project_root = pathlib.Path.cwd() if project_root is None else project_root

    # If the project path does not exist, create it.
    if not project_root.exists():
        project_root.mkdir(parents=True)
        print_success(f"Created new project directory at {project_root}.")
    elif current_project() is not None:
        print_warning(f"A CIDA project already exists at {project_root}.")
    else:
        print_success("Project directory already exists.")

    # Build a new project config.
    project_config_dir = project_root.joinpath(CIDA_DIRECTORY_NAME)

    # Create the project config directory
    project_config_dir.mkdir(parents=True, exist_ok=True)

    # Create the new CIDAProject object.
    project_config = CIDAProject(
        project_name=project_name,
        principal_investigator=principal_investigator,
        analyst=analyst,
        data_location=data_location,
        git_location=git_location
    )
    _write_config(project_root=project_root, config=project_config)

    # Create the main README file.
    _write_templated_readme(project_root=project_root, template_name="Project.md", config=project_config)

    # Create the other directories and populate with READMEs
    for project_subdir in ["Admin", "Background", "Code", "DataRaw", "DataProcessed", "Dissemination", "Reports"]:
        # Get the path to this subdirectory.
        project_subdir_path = project_root.joinpath(project_subdir)
        # If the subdirectory does not already exist, get it.
        if not project_subdir_path.exists():
            project_subdir_path.mkdir(parents=True)
            print_success(f"Created {project_subdir}/ subdirectory.")
        # Write the README to this subdirectory.
        _write_templated_readme(project_root=project_root, template_name=f"{project_subdir}.md", subdir=project_subdir, config=project_config)

    # Create the Rprofile hook.
    _write_rprofile(project_root=project_root)

    # Create default RProj.
    _write_rproj(project_root=project_root, config=project_config)

    # Create default .gitignore
    _write_gitignore(project_root=project_root)

    return project_config


def create_github_project(
        project_name: str = None,
        repository_url: str = None,
        project_root: pathlib.Path = None,
        template_url: str = None,
        principal_investigator: str = None,
        analyst: str | list[str] = None,
        data_location: str = None,
) -> CIDAProject:
    """ Creates a new CIDA project with GitHub integration.

    :param project_name: A name for the new project.
    :param repository_url: URL for the GitHub repository to be created for this project.
    :param project_root: The (local) root directory for the project.
    :param template_url: URL for a template repository to be used for this project.
    :param principal_investigator:
    :param analyst:
    :param data_location:
    :return:
    """
    if project_name is None and repository_url is None:
        raise ValueError("Must specify either project_name or repository_url.")

    # If the project name does not exist, use a placeholder.
    project_name = "CIDAProject" if project_name is None else project_name

    # If the project path is not specified, use the working directory.
    project_root = pathlib.Path.cwd() if project_root is None else project_root

    # If the project path does not exist, create it.
    if not project_root.exists():
        project_root.mkdir(parents=True)
        print_success(f"Created new project directory at {project_root}.")
    elif current_project() is not None:
        print_failure(f"A CIDA project already exists at {project_root}.")
        return
    else:
        print_success("Project directory already exists.")


def create_project(project_path: pathlib.Path = None) -> CIDAProject:
    """ Interactive flow for creating a CIDA project.

    This function will guide users through the interactive process for creating a new CIDA project which involves:
    1. Choosing whether to create a local project or a GitHub project.
    2. (If GitHub project)
        a. Choosing the project template (template repository)
        b. Providing a GitHub repository name.

    For non-interactive project creation (for use in a script, etc.) use `create_project_local()` or `create_project_github()`.
    :param project_path: Path to the directory where the project will be created, or None.
    :return:
    """
    # If the project path is not specified, use the working directory.
    if project_path is None:
        project_path = pathlib.Path.cwd()

    #


def project_attribute(f):
    """ Decorator used to create CIDA project attributes.
    :param f: The function to be wrapped.
    :return: A decorated project attribute getter.
    """
    @functools.wraps(f)
    def wrapper():
        cur_proj = current_project()
        if cur_proj is not None:
            return f(cur_proj)
        return None

    return wrapper


@project_attribute
def get_project_name(project: CIDAProject):
    return project.project_name


@project_attribute
def get_project_principal_investigator(project: CIDAProject) -> str | None:
    return project.principal_investigator


@project_attribute
def get_project_analyst(project: CIDAProject) -> None | str | list[str]:
    return project.analyst


@project_attribute
def get_project_data_location(project: CIDAProject) -> str | None:
    return project.data_location


@project_attribute
def get_project_git_location(project: CIDAProject) -> str | None:
    return project.git_location


