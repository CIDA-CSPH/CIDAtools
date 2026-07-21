import pathlib
import pytest
from cidatools import project
import tempfile


def test_create_local_project():
    with tempfile.TemporaryDirectory() as tmpdir:
        project.create_local_project(project_name="MyNewProject", project_root=pathlib.Path(tmpdir))
        print("Done!")


def test_double_create_local_project():
    with tempfile.TemporaryDirectory() as tmpdir:
        project.create_local_project(project_name="MyNewProject", project_root=pathlib.Path(tmpdir))
        project.create_local_project(project_name="MyNewProject", project_root=pathlib.Path(tmpdir))
        print("Done!")
