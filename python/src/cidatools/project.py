import functools
import json
import pathlib
import re
import shutil
from importlib import resources
from typing import Literal

from jinja2 import Template
from pydantic import BaseModel, Field, ValidationError

from cidatools.consts import (
    CIDA_DIRECTORY_NAME,
    CIDA_PROJECT_CONFIG_NAME,
)
from cidatools.defaults import CIDA_PROJECT_DEFAULT_FOLDERS, CIDADefaults
from cidatools.git import (
    clone_github_repository,
    create_empty_github_repository,
    create_github_repository_from_template,
    list_github_templates,
)
from cidatools.persistence import PersistentField, PersistentWrapper
from cidatools.utils import print_failure, print_success, print_warning


class CIDAProjectModel(BaseModel, frozen=True):
    project_name: str | None = Field(default=None)
    principal_investigator: str | None = Field(default=None)
    data_location: str | None = Field(default=None)
    git_location: str | None = Field(default=None)
    analyst: str | list[str] | None = Field(default=None)


class CIDAProject(PersistentWrapper):
    # Class Attributes
    __model__ = CIDAProjectModel
    __parent__ = CIDADefaults()

    # Persistent fields
    project_name = PersistentField()
    principal_investigator = PersistentField()
    data_location = PersistentField()
    git_location = PersistentField()
    analyst = PersistentField()


def _ensure_config_path(project_root: pathlib.Path) -> pathlib.Path:
    """Constructs a config path given a CIDA project root directory.
    :param project_root: A pathlib.Path pointing to either the CIDA project root directory
    or the config file within the project directory.
    :return:
    """
    if project_root.name == CIDA_PROJECT_CONFIG_NAME and project_root.parent.name == CIDA_DIRECTORY_NAME:
        config_path = project_root
    else:
        config_path = project_root.joinpath(CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME)

    return config_path


def _read_config(project_root: pathlib.Path) -> CIDAProjectModel | None:
    """Deserializes a CIDA project JSON file into a `CIDAProjectConfig` object.
    :param project_root:
    :return: A CIDAProject object.
    """
    # Construct a config path from a path to the project root.
    config_path = _ensure_config_path(project_root=project_root)

    try:
        with open(config_path, "r") as f:
            return CIDAProjectModel.model_validate(json.load(f))
    except (FileNotFoundError, ValidationError) as e:
        print_failure(f"Unable to read config file: {e}")
    return None


def _write_config(project_root: pathlib.Path, config: CIDAProjectModel, overwrite: bool = False) -> None:
    """Serializes a `CIDAProject` object into a CIDA project JSON file.
    :param project_root: The root directory for the project.
    :param config: The CIDAProject object to be serialized.
    :param overwrite: Whether to overwrite existing serialized JSON file.
    :return:
    """
    # Construct a config path from a path to the project root.
    config_path = _ensure_config_path(project_root=project_root)

    # If path exists, don't overwrite unless overwrite=True.
    if overwrite or not (config_path.exists() and config_path.is_file()):
        tmp_config_path = config_path.with_suffix(".tmp")
        with open(tmp_config_path, "w") as f:
            f.write(config.model_dump_json(indent=4))
        tmp_config_path.replace(config_path)
        print_success(f"Created new project JSON at {config_path}.")
    # Otherwise, print a fail message.
    else:
        # log.warning(f"{config_path} already exists, will not overwrite. Pass overwrite=True to force an overwrite.")
        print_failure(f"CIDA project config already exists at {config_path}, will not overwrite.")


def _write_templated_readme(
    project_root: pathlib.Path,
    template_name: str,
    config: CIDAProjectModel,
    overwrite: bool = False,
    subdir: str | None = None,
) -> None:
    """Writes a templated READMe to te
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
        template_str = (
            resources.files("cidatools").joinpath("resources/templates/readme/").joinpath(template_name).read_text()
        )

        # Populate the template with information from the CIDAProject config.
        rendered_template_str = Template(template_str).render(config=config)

        # Write the rendered template to file.
        with open(readme_path, "w") as f:
            f.write(rendered_template_str)

        print_success(f"Created README.md at {readme_path.relative_to(project_root)}.")
    # Otherwise print an error message.
    else:
        # log.warning(f"{readme_path} already exists, will not overwrite. Pass overwrite=True to force an overwrite.")
        print_failure(f"{readme_path.relative_to(project_root)} already exists, will not overwrite.")


def _write_rprofile(project_root: pathlib.Path) -> None:
    """Writes a .Rprofile file.
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
        if (
            re.match(
                "(?s)# BEGIN CIDATools Added\n.*?# END CIDATools Added",
                rprofile_contents,
            )
            is None
        ):
            # Append the CIDAtools block to the file
            with open(rprofile_path, "a+") as f:
                f.write(rprofile_block)
            print_success("Appended CIDAtools block to .Rprofile.")
        else:
            print_failure(".Rprofile already contains a CIDAtools block.")
    else:
        with open(rprofile_path, "w") as f:
            f.write(rprofile_block)
        print_success("Created .Rprofile and added CIDAtools block.")


def _write_rproj(project_root: pathlib.Path, config: CIDAProjectModel):
    """Creates a .Rproj file in the current directory if None exists.
    :param project_root: The root directory for the project.
    :param config: The CIDAProjectConfig object, used to generate a name for the Rproj file if it doesn't exist.
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
        with resources.as_file(
            resources.files("cidatools").joinpath("resources").joinpath("DefaultCIDAProject.Rproj")
        ) as default_rproj:
            shutil.copy(default_rproj, rproj_path)
        # Print success
        print_success(f"Created {rproj_path.relative_to(project_root)}.")


def _write_gitignore(project_root: pathlib.Path):
    """Creates a .gitignore file at the given path if None exists.
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
        with resources.as_file(
            resources.files("cidatools").joinpath("resources").joinpath("gitignore")
        ) as default_gitignore:
            shutil.copy(default_gitignore, gitignore_path)
        # Print success
        print_success(f"Created {gitignore_path.relative_to(project_root)}.")


def project_status(project_root: pathlib.Path | None = None):
    """Prints the status of the CIDA project at `project_root`.

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
        print_success("Loaded CIDA project.")

    # Check issues
    print("Project Config: ")
    longest_key = max(len(k) for k in cur_project.__dict__)
    for key, val in cur_project.__dict__.items():
        print(f"{key.ljust(longest_key)} : {val}")

    # Check for Git repository.
    return


def current_project(project_root: pathlib.Path | None = None) -> CIDAProject | None:
    """Returns the currently active CIDA project or None

    Searches recursively for a CIDA project config file, starting at the working directory and working upwards
    toward the root directory.

    :param project_root: The root directory for the project.
    :return: A CIDAProject object or None
    """
    # Accept a user-provided project path, or use the working directory if none is provided.
    project_root = (pathlib.Path.cwd() if project_root is None else project_root).absolute()

    if not project_root.is_dir():
        print_failure(f"Path {project_root} is not a directory.")
        return None

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
                    return CIDAProject(path=project_path)
                else:
                    print_failure(
                        f"Project directory found at {project_path} but there is no {CIDA_PROJECT_CONFIG_NAME} file inside."
                    )
                    return None
            else:
                print_failure(f"'{project_path}' is not a directory.")
                return None
        else:
            project_root = project_root.parent
    # If we find nothing, return None
    return None


def create_local_project(
    project_root: pathlib.Path | None = None,
    project_name: str | None = None,
    principal_investigator: str | None = None,
    analyst: str | list[str] | None = None,
    data_location: str | None = None,
    git_location: str | None = None,
    folders_to_create: list[str] | None = None,
) -> CIDAProject:
    """Creates a CIDA project structure at `project_root`.

    This function operates offline, does not configure an associated GitHub repository, and uses the basic/default project template.
    :param project_root:
    :param project_name:
    :param principal_investigator:
    :param analyst:
    :param data_location:
    :param git_location:
    :param folders_to_create: A list of folders to create as part of the project.
    If not specified, uses CIDA_PROJECT_DEFAULT_FOLDERS
    :return:
    """
    # If the project name does not exist, use a placeholder.
    project_name = "CIDAProject" if project_name is None else project_name

    # If the project path is not specified, use the working directory.
    _project_root = (pathlib.Path.cwd() if project_root is None else project_root).absolute()

    # If the project path does not exist, create it.
    if not _project_root.exists():
        _project_root.mkdir(parents=True)
        print_success(f"Created new project directory at {_project_root}.")
    elif current_project() is not None:
        print_warning(f"A CIDA project already exists at {_project_root}.")
    else:
        print_success("Project directory already exists.")

    # Build a new project config.
    project_config_dir = _project_root.joinpath(CIDA_DIRECTORY_NAME)

    # Create the project config directory
    project_config_dir.mkdir(parents=True, exist_ok=True)

    # Create the new CIDAProjectConfig object.
    project_config = CIDAProjectModel(
        project_name=project_name,
        principal_investigator=principal_investigator,
        analyst=analyst,
        data_location=data_location,
        git_location=git_location,
    )

    # Write the config to file.
    _write_config(project_root=_project_root, config=project_config)

    # Create the main README file.
    _write_templated_readme(project_root=_project_root, template_name="Project.md", config=project_config)

    # Create the other directories and populate with READMEs
    project_subdirs = CIDA_PROJECT_DEFAULT_FOLDERS if folders_to_create is None else folders_to_create
    for project_subdir in project_subdirs:
        # Get the path to this subdirectory.
        project_subdir_path = _project_root.joinpath(project_subdir)
        # If the subdirectory does not already exist, get it.
        if not project_subdir_path.exists():
            project_subdir_path.mkdir(parents=True)
            print_success(f"Created {project_subdir}/ subdirectory.")
        # Write the README to this subdirectory.
        _write_templated_readme(
            project_root=_project_root,
            template_name=f"{project_subdir}.md",
            subdir=project_subdir,
            config=project_config,
        )

    # Create the Rprofile hook.
    _write_rprofile(project_root=_project_root)

    # Create default RProj.
    _write_rproj(project_root=_project_root, config=project_config)

    # Create default .gitignore
    _write_gitignore(project_root=_project_root)

    return CIDAProject(path=_ensure_config_path(_project_root))


def create_github_project(
    project_name: str | None = None,
    repository_name: str | None = None,
    project_root: pathlib.Path | None = None,
    template_name: str | None = None,
    principal_investigator: str | None = None,
    analyst: str | list[str] | None = None,
    data_location: str | None = None,
    description: str | None = None,
    visibility: Literal["public", "private", "internal"] = "internal",
) -> CIDAProject | None:
    """Creates a new CIDA project with GitHub integration.

    :param project_name: A name for the new project.
    :param repository_name: An optional name for the repository.
        If none is specified, the `project_name` will be used.
    :param project_root: The (local) root directory for the project.
    :param template_name: The name of a template project to use.
        To see available templates, call list_github_templates().
    :param principal_investigator:
    :param analyst:
    :param data_location:
    :param description: An optional description for the repository. (default: None)
    :param visibility: The visibility for the repository (default: 'internal')
    :return:
    """
    # The project name is required since we need something to use as the repo name.
    if project_name is None and repository_name is None and project_root is None:
        print_failure(
            "One of 'project_name', 'repository_name' or 'project_root' must be specified to create a GitHub project."
        )

    # If the project path is not specified, use the working directory.
    _project_root = pathlib.Path.cwd() if project_root is None else project_root

    # If the project path does not exist, create it.
    if not _project_root.exists():
        _project_root.mkdir(parents=True)
        print_success(f"Created new project directory at {_project_root}.")
    elif _project_root.exists() and not _project_root.is_dir():
        print_failure(f"{_project_root} exists and is not a directory.")
        return None
    elif any(_project_root.iterdir()):
        print_failure(f"Cannot create a new project in non-empty directory {_project_root}.")
        return None
    # elif current_project() is not None:
    #     print_failure(f"Cannot create a new project at {_project_root}, an existing project already exists here.")
    #     return None
    else:
        print_success("Project directory already exists.")

    # Retrieve the available templates.
    template_list = list_github_templates(include_empty=True, display=template_name is None)

    # Determine the repo name with the following precedence:
    #   1. The repository_name (if specified)
    #   2. The name of the current working directory.
    repo_name = repository_name if repository_name is not None else _project_root.name
    # If a template name is not chosen, prompt interactively.
    if template_name is None:
        # The user's selection defaults to 0 (an empty repository)
        user_i = 0
        while user_i not in range(len(template_list)):
            user_choice = input("Choose a template from the above list (default 0): ")
            if user_choice != "":
                user_i = int(user_choice)
        # Use the user's selection to choose the template.
        if user_i == 0:
            _template_name = "empty"
        else:
            _template_name = template_list[user_i].name
    else:
        _template_name = template_name

    # If an empty project is requested, create it.
    if _template_name == "empty":
        repo_url = create_empty_github_repository(
            name=repo_name,
            description=description,
            visibility=visibility,
        )
    # Otherwise create from the selected template.
    else:
        repo_url = create_github_repository_from_template(
            name=repo_name,
            description=description,
            visibility=visibility,
            template_name=_template_name,
        )

    # If there was an error during repository creation, stop here.
    if repo_url is None:
        print_failure("Unable to create new GitHub project.")
        return None

    # Now, pull the GitHub repo from url.
    clone_success = clone_github_repository(repository_url=repo_url, local_path=_project_root)
    if not clone_success:
        print_failure("Error when cloning GitHub project.")
        return None

    # At this point, we should have the git repository cloned to the local path.
    project_config = CIDAProjectModel(
        project_name=project_name,
        principal_investigator=principal_investigator,
        analyst=analyst,
        data_location=data_location,
        git_location=repo_url,
    )

    # Write the config to file.
    _write_config(project_root=_project_root, config=project_config)

    # TODO: Optionally stage, commit and push the project updates.
    print_success(
        f"Created new CIDA project.\n  Local path: {_project_root}\n  GitHub URL: {repo_url}.\n"
        "Your project configuration has been updated, changes are unstaged in the working directory."
    )

    # Return the new CIDAProject
    return CIDAProject(path=_ensure_config_path(_project_root))


def create_project(project_path: pathlib.Path | None = None) -> CIDAProject | None:
    """Interactive flow for creating a CIDA project.

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

    # TODO: The rest of this


def project_attribute(f):
    """Decorator used for CIDA project getter functions.
    :param f: The function to be wrapped.
    :return: A decorated project attribute getter.
    """

    @functools.wraps(f)
    def wrapper(project: CIDAProject | None):
        # If the project is given, we use it, otherwise, default to the current_project().
        project = current_project() if project is None else project

        # If we can't load a current project, return None.
        if project is None:
            print_failure("No active CIDA project, unable to retrieve or modify attributes.")
            return None

        # Otherwise call the function with the project.
        return f(project)

    return wrapper


@project_attribute
def get_project_name(project: CIDAProject):
    return project.project_name


@project_attribute
def set_project_name(project: CIDAProject, project_name: str | None) -> None:
    project.project_name = project_name


@project_attribute
def get_project_principal_investigator(project: CIDAProject) -> str | None:
    return project.principal_investigator


@project_attribute
def set_project_principal_investigator(project: CIDAProject, principal_investigator: str | None) -> None:
    project.principal_investigator = principal_investigator


@project_attribute
def get_project_analyst(project: CIDAProject) -> None | str | list[str]:
    return project.analyst


@project_attribute
def set_project_analyst(project: CIDAProject, analyst: str | list[str] | None) -> None:
    project.analyst = analyst


@project_attribute
def get_project_data_location(project: CIDAProject) -> str | None:
    return project.data_location


@project_attribute
def set_project_data_location(project: CIDAProject, data_location: str | None) -> None:
    project.data_location = data_location


@project_attribute
def get_project_git_location(project: CIDAProject) -> str | None:
    return project.git_location


@project_attribute
def set_project_git_location(project: CIDAProject, git_location: str | None) -> None:
    project.git_location = git_location
