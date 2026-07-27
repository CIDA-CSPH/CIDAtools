import pathlib

import pytest

from cidatools import project
import tempfile

from cidatools.consts import CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME


@pytest.fixture(params=["MyNewProject", None])
def project_name(request):
    return request.param


@pytest.fixture(params=["Dr. PI", None])
def principal_investigator(request):
    return request.param


@pytest.fixture(params=["Single Analyst", ["Analyst 1", "Analyst 2"], None])
def analyst(request):
    return request.param


@pytest.fixture(params=["/path/on/hard/drive", None])
def data_location(request):
    return request.param


@pytest.fixture(params=["https://github.com/CIDA-CSPH/someRepo", None])
def git_location(request):
    return request.param


@pytest.fixture()
def local_project(project_name, principal_investigator, analyst, data_location, git_location):
    # Make a temporary directory
    tmpdir = tempfile.TemporaryDirectory()

    # Make a local project.
    project_config = project.create_local_project(
        project_name=project_name,
        project_root=pathlib.Path(tmpdir.name),
        principal_investigator=principal_investigator,
        analyst=analyst,
        data_location=data_location,
        git_location=git_location
    )

    # Provide the temporary directory and config file
    yield tmpdir, project_config

    # Clean up the entire directory on completion.
    tmpdir.cleanup()


def test_create_local_project(local_project):
    # Unpack the fixture
    tmpdir, local_config = local_project

    # Get local project path
    proj_root = pathlib.Path(tmpdir.name)

    # Check that config directory exists
    proj_dir = proj_root.joinpath(CIDA_DIRECTORY_NAME)
    assert proj_dir.is_dir()

    # Check that config file exists
    config_path = proj_dir.joinpath(CIDA_PROJECT_CONFIG_NAME)
    assert config_path.is_file()

    # Check that config contains all entries expected
    loaded_config = project._read_config(config_path=config_path)
    assert local_config == loaded_config

    # TODO: Check directory, README, and project file consistency.


def test_double_create_local_project():
    with tempfile.TemporaryDirectory() as tmpdir:
        project.create_local_project(project_name="MyNewProject", project_root=pathlib.Path(tmpdir))
        project.create_local_project(project_name="MyNewProject", project_root=pathlib.Path(tmpdir))
        print("Done!")


def test_project_status(local_project):
    tmpdir, local_config = local_project
    project.project_status(project_root=pathlib.Path(tmpdir.name))
