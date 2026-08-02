import json
from posixpath import join as posixjoin
from typing import Literal

import pytest

from cidatools.consts import CIDA_GITHUB_ORGANIZATION
from cidatools.git import (
    _get_github_token,
    create_empty_github_repository,
    create_github_repository_from_template,
    list_github_templates,
)


def test_retrieve_gcm_token(mocker):
    mock_gcm = mocker.patch("subprocess.run")
    mock_gcm.return_value.stdout = b"protocol=https\nhost=github.com\nusername=Andrew0Hill\npassword=gho_FaKeTokeN\n\n"
    gcm_token = _get_github_token()
    assert gcm_token == "gho_FaKeTokeN"


@pytest.mark.skip(reason="Not implemented")
def test_retrieve_default_token(mocker):
    pass


def test_list_github_templates(mocker):
    # Mock the GitHub token retrieval
    # get_token = mocker.patch("cidatools.git._get_github_token")
    # get_token.return_value = "my_fake_token"
    # Mock the get request.
    # mocker_request = mocker.patch("requests.get")
    # mocker_request.return_value.status_code = 200
    # mocker_request.return_value.json.return_value = {
    #     "incomplete_results": False,
    #     "items": [
    #         {
    #             "description": "Default CIDAtools template",
    #             "full_name": "CIDA-CSPH/ct_default_template",
    #             "id": 1300873186,
    #             "is_template": True,
    #             "name": "ct_default_template",
    #             "node_id": "R_kgDOTYm_4g",
    #             "url": "https://api.github.com/repos/CIDA-CSPH/ct_default_template",
    #         }
    #     ],
    #     "total_count": 1,
    # }
    template_list = list_github_templates()
    # Check that we parsed the template list correctly
    assert len(template_list) == 2
    empty_template = template_list[0]
    assert empty_template.name == "(No Template)"
    template_entry = template_list[1]
    # Check that the values match
    assert template_entry.name == "ct_default_template"
    assert template_entry.full_name == "CIDA-CSPH/ct_default_template"
    assert template_entry.description == "Default CIDAtools template"
    assert template_entry.id == 1300873186


def test_create_empty_github_repository_success(pytestconfig, mocker):
    # Mock the GitHub token retrieval
    get_token = mocker.patch("cidatools.git._get_github_token")
    get_token.return_value = "my_fake_token"
    # The get request is mocked for the initial existence check.
    mock_get = mocker.patch("requests.get")
    mock_get.return_value.status_code = 404
    # Load the mock response from file.
    with open(pytestconfig.rootpath.joinpath("tests/resources/test_git/repo_available_response.json"), "r") as f:
        mock_get.return_value.json.return_value = json.load(f)
    # The post request is mocked for the repository creation.
    mock_post = mocker.patch("requests.post")
    mock_post.return_value.status_code = 201

    repo_url = create_empty_github_repository(
        name="test_cidatools_repo",
        description="A test description.",
        visibility="internal",
    )
    assert repo_url == posixjoin(CIDA_GITHUB_ORGANIZATION, "test_cidatools_repo")


@pytest.mark.parametrize("visibility", ["internal", "public", "private"])
def test_create_github_repository_from_template_success(
    pytestconfig, mocker, visibility: Literal["internal", "public", "private"]
):
    # Mock the GitHub token retrieval
    get_token = mocker.patch("cidatools.git._get_github_token")
    get_token.return_value = "my_fake_token"
    # The get request is mocked for the initial existence check.
    mock_get = mocker.patch("requests.get")
    mock_get.return_value.status_code = 404
    # Load the mock response from file.
    with open(pytestconfig.rootpath.joinpath("tests/resources/test_git/repo_available_response.json"), "r") as f:
        mock_get.return_value.json.return_value = json.load(f)
    # The post request is mocked for the repository creation.
    mock_post = mocker.patch("requests.post")
    mock_post.return_value.status_code = 201
    # The patch request is used to update visibility, and should only be called
    # if visibility="internal"
    mock_patch = mocker.patch("requests.patch")
    mock_patch.return_value.status_code = 200
    # Call the repo creation command
    repo_url = create_github_repository_from_template(
        name="test_cidatools_repo",
        description="A test description.",
        visibility=visibility,
        template_name="ct_default_template",
    )

    # If repo is public, everything should fail.
    if visibility == "public":
        assert repo_url is None
        get_token.assert_not_called()
        mock_post.assert_not_called()
        mock_patch.assert_not_called()
    # A private repo should generate successfully, but should not call the patch method
    elif visibility == "private":
        assert repo_url == posixjoin(CIDA_GITHUB_ORGANIZATION, "test_cidatools_repo")
        mock_patch.assert_not_called()
    # An internal repo should generate successfully, and call the patch method successfully.
    elif visibility == "internal":
        assert repo_url == posixjoin(CIDA_GITHUB_ORGANIZATION, "test_cidatools_repo")
        mock_patch.assert_called_once()
