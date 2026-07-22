import datetime
import glob
import pathlib
import json
import logging
import re
import shutil
from enum import StrEnum
from importlib import resources

from jinja2 import Template
from pydantic import BaseModel, model_validator, Field

from cidatools.consts import CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME
from cidatools.utils import print_success, print_failure

log = logging.getLogger(__name__)


class CIDAProjectSchema(StrEnum):
    V1 = "v1"


class CIDAProject(BaseModel):
    project_schema: CIDAProjectSchema = Field(default=CIDAProjectSchema.V1, frozen=True)
    project_name: str | None
    principal_investigator: str | None
    analyst: str | list[str] | None
    data_location: str | None
    git_location: str | None


def _read_config(config_path: pathlib.Path) -> CIDAProject:
    """ Deserializes a CIDA project JSON file into a `CIDAProject` object.
    :param config_path:
    :return: A CIDAProject object.
    """
    with open(config_path, "r") as f:
        return CIDAProject.model_validate(json.load(f))


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
        log.warning(f"{config_path} already exists, will not overwrite. Pass overwrite=True to force an overwrite.")
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
        log.warning(f"{readme_path} already exists, will not overwrite. Pass overwrite=True to force an overwrite.")
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
    """ Creates a .gitignore file at the given path if None exists. """
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


def current_project() -> CIDAProject | None:
    """ Returns the currently active CIDA project or None

    Searches recursively for a CIDA project config file, starting at the working directory and working upwards
    toward the root directory.
    :return: A CIDAProject object or None
    """
    # Get the absolute path to our current location
    cur_dir = pathlib.Path.cwd().absolute()

    # Quit when we reach the root directory.
    while cur_dir != cur_dir.parent:
        # Check the current directory for a CIDA folder
        project_path = cur_dir.joinpath(CIDA_DIRECTORY_NAME)
        # If the path exists, check if it is a folder
        if project_path.exists():
            # If it is a folder, check if the config file exists inside.
            if project_path.is_dir():
                config_path = project_path.joinpath(CIDA_PROJECT_CONFIG_NAME)
                # If the config file exists, parse JSON and return the object.
                if config_path.is_file():
                    return _read_config(project_path)
                else:
                    log.error(f"Project directory found at {project_path} but there is no {CIDA_PROJECT_CONFIG_NAME} file inside.")
                    return None
            else:
                log.error(f"'{project_path}' exists but is not a directory.")
                return None
        else:
            cur_dir = cur_dir.parent
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
    # If the project path is not specified, use the working directory.
    if project_root is None:
        project_root = pathlib.Path.cwd()

    # If the project path does not exist, create it.
    if not project_root.exists():
        project_root.mkdir(parents=True)
        print_success(f"Created new project directory at {project_root}.")
    elif current_project() is not None:
        log.warning(f"A CIDA project already exists at {project_root}")
    else:
        print_success("Project directory already exists.")

    # Build a new project config.
    project_config_dir = project_root.joinpath(CIDA_DIRECTORY_NAME)
    project_config_path = project_config_dir.joinpath(CIDA_PROJECT_CONFIG_NAME)

    # Warn if the directory already has a project in it.
    if project_config_dir.exists() or project_config_path.exists():
        log.warning(f"This directory may already contain a CIDA project.")

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


