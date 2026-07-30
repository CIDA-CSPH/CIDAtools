import pathlib
import tempfile

import pytest
from cidatools import project
from cidatools.consts import CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME
from cidatools.project import CIDAProjectModel
from pydantic import ValidationError


@pytest.fixture(
    params=[
        {
            "project_name": "MyNewProject",
            "principal_investigator": "Dr. PI",
            "analyst": "Single Analyst",
            "data_location": None,
            "git_location": None,
        },
        {
            "project_name": "MyNewProject2",
            "principal_investigator": None,
            "analyst": ["Analyst 1", "Analyst 2"],
            "data_location": "/path/on/P/drive",
            "git_location": None,
        },
        {
            "project_name": "MyNewProject3",
            "principal_investigator": None,
            "analyst": ["Analyst 1", "Analyst 2"],
            "data_location": None,
            "git_location": "https://github.com/CIDA-CSPH/someRepo",
        },
        {
            "project_name": None,
            "principal_investigator": None,
            "analyst": None,
            "data_location": None,
            "git_location": None,
        },
    ]
)
def local_project(request):
    # Unpack parameters
    project_name, principal_investigator, analyst, data_location, git_location = (
        request.param.values()
    )

    # Make a temporary directory
    tmpdir = tempfile.TemporaryDirectory()

    # Make a local project.
    project_config = project.create_local_project(
        project_name=project_name,
        project_root=pathlib.Path(tmpdir.name),
        principal_investigator=principal_investigator,
        analyst=analyst,
        data_location=data_location,
        git_location=git_location,
    )

    # Provide the temporary directory and config file
    yield tmpdir, project_config

    # Clean up the entire directory on completion.
    tmpdir.cleanup()


def test_create_local_project(local_project):
    # Unpack the fixture
    tmpdir, local_proj_inst = local_project

    # Get local project path
    proj_root = pathlib.Path(tmpdir.name)

    # Check that config directory exists
    proj_dir = proj_root.joinpath(CIDA_DIRECTORY_NAME)
    assert proj_dir.is_dir()

    # Check that config file exists
    config_path = proj_dir.joinpath(CIDA_PROJECT_CONFIG_NAME)
    assert config_path.is_file()

    # Check that config contains all entries expected
    loaded_config = project._read_config(project_root=config_path)
    for field in CIDAProjectModel.model_fields:
        assert getattr(local_proj_inst, field) == getattr(loaded_config, field)

    # TODO: Check directory, README, and project file consistency.


def test_double_create_local_project():
    with tempfile.TemporaryDirectory() as tmpdir:
        project.create_local_project(
            project_name="MyNewProject", project_root=pathlib.Path(tmpdir)
        )
        project.create_local_project(
            project_name="MyNewProject", project_root=pathlib.Path(tmpdir)
        )
        print("Done!")


def test_project_status(local_project):
    tmpdir, _ = local_project
    project.project_status(project_root=pathlib.Path(tmpdir.name))


def test_set_attribute_valid(local_project):
    # Get a project
    _, local_project_inst = local_project

    # Modify an attribute in a valid way.
    new_name = "Andrew's Project"
    local_project_inst.project_name = new_name

    # Check that the attribute is what we expect
    # (This operation should pull from disk).
    assert local_project_inst.project_name == new_name


def test_set_attribute_invalid(local_project):
    # Get a project
    _, local_project_inst = local_project

    # Modify an attribute to something invalid
    invalid_name = 0xFFFF

    # Setting a property to an invalid value should throw an error.
    with pytest.raises(ValidationError):
        local_project_inst.project_name = invalid_name
        print("Done")
