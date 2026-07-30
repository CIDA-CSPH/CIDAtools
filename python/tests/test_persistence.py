import pathlib
import tempfile

from cidatools.consts import CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME
from cidatools.defaults import CIDADefaults, CIDADefaultsModel
from cidatools.project import CIDAProject, CIDAProjectModel


def test_defaults_persistence(mocker):
    with tempfile.TemporaryDirectory() as tmpdir:
        # Fake defaults path
        fake_defaults_path = pathlib.Path(tmpdir).joinpath("projet_defaults.json")
        # Fake defaults
        fake_defaults = CIDADefaultsModel(analyst="Fake Analyst")
        # Write defaults
        with open(fake_defaults_path, "w") as f:
            f.write(fake_defaults.model_dump_json())
        # Patch the default path
        mocker.patch.object(CIDADefaults, "path", fake_defaults_path)
        # Create defaults wrapper
        defaults_wrapper = CIDADefaults()
        # Check that the value is correct
        assert defaults_wrapper.analyst == "Fake Analyst"


def test_project_persistence(mocker):
    with tempfile.TemporaryDirectory() as tmpdir:
        # Fake path for project file
        fake_path = pathlib.Path(tmpdir).joinpath(
            CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME
        )
        fake_path.parent.mkdir(parents=True, exist_ok=False)
        # Fake project model
        fake_project = CIDAProjectModel(
            analyst="Fake Analyst",
            project_name="Fake Project",
            principal_investigator="Fake Principal Investigator",
            data_location="Fake Data Location",
            git_location="Fake Git Location",
        )
        # Write fake project to disk
        with open(fake_path, "w") as f:
            f.write(fake_project.model_dump_json())
        # Check that the wrapper correctly parses the file.
        project_wrapper = CIDAProject(path=fake_path)
        # Check that the attributes are correct
        assert project_wrapper.analyst == "Fake Analyst"
        assert project_wrapper.project_name == "Fake Project"
        assert project_wrapper.principal_investigator == "Fake Principal Investigator"
        assert project_wrapper.data_location == "Fake Data Location"
        assert project_wrapper.git_location == "Fake Git Location"


def test_project_persistence_with_defaults(mocker):
    with tempfile.TemporaryDirectory() as tmpdir:
        # Fake defaults path
        fake_defaults_path = pathlib.Path(tmpdir).joinpath("projet_defaults.json")
        # Fake defaults
        fake_defaults = CIDADefaultsModel(analyst="Default Fake Analyst")
        # Write defaults
        with open(fake_defaults_path, "w") as f:
            f.write(fake_defaults.model_dump_json())
        # Patch the default path, but don't instance the wrapper.
        mocker.patch.object(CIDADefaults, "path", fake_defaults_path)
        # Fake path for project file
        fake_path = pathlib.Path(tmpdir).joinpath(
            CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME
        )
        fake_path.parent.mkdir(parents=True, exist_ok=False)
        # Fake project model
        fake_project = CIDAProjectModel(
            analyst=None,
            project_name="Fake Project",
            principal_investigator="Fake Principal Investigator",
            data_location="Fake Data Location",
            git_location="Fake Git Location",
        )
        # Write fake project to disk
        with open(fake_path, "w") as f:
            f.write(fake_project.model_dump_json())
        # Check that the wrapper correctly parses the file.
        project_wrapper = CIDAProject(path=fake_path)
        # Check that the attributes are correct.
        assert project_wrapper.analyst == "Default Fake Analyst"
        assert project_wrapper.project_name == "Fake Project"
        assert project_wrapper.principal_investigator == "Fake Principal Investigator"
        assert project_wrapper.data_location == "Fake Data Location"
        assert project_wrapper.git_location == "Fake Git Location"


# TODO: Test that persistence works within a session when the can_persist=False
#  i.e. Override CIDADefaults path with invalid path, then try to manually set
#  the default analyst and verify that it works.
