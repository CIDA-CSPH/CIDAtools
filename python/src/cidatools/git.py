import pathlib
import re
import shutil
import subprocess
from enum import Flag, auto
from posixpath import join as posixjoin
from typing import Any, Literal

import requests
from pydantic import BaseModel, ValidationError, field_validator

from cidatools.consts import (
    CIDA_GITHUB_ORGANIZATION,
    GITHUB_REPO_API_URL,
    GITHUB_REPO_CREATE_API_URL,
    GITHUB_REPO_CREATE_FROM_TEMPLATE_API_URL,
    GITHUB_SEARCH_API_URL,
)
from cidatools.defaults import CIDADefaults
from cidatools.utils import (
    get_user_prompt_by_name,
    print_failure,
    print_info,
    print_success,
    print_warning,
)


class GitStatus(Flag):
    GIT_INSTALLED = auto()
    GCM_INSTALLED = auto()
    GCM_CONFIGURED = auto()


class TemplateRepo(BaseModel):
    id: int
    name: str
    full_name: str
    description: str | None
    url: str | None

    @classmethod
    def empty(cls):
        return TemplateRepo(
            id=0,
            name="empty",
            full_name="empty",
            description="An empty repository (no template).",
            url=None,
        )


class TemplateRepoList(BaseModel):
    total_count: int
    incomplete_results: bool
    items: list[TemplateRepo]

    @field_validator("items", mode="before")
    @classmethod
    def filter_non_template_repos(cls, value: Any) -> list:
        """Pre-filter for template repositories
        This function filters out any repositories in the list which are not template repos.
        :param value: A list of JSON repository objects.
        :return: A filtered list of JSON repository objects.
        """
        if not isinstance(value, list):
            raise TypeError("Expected 'items' to be a list.")
        return [elem for elem in value if elem.get("is_template")]

    @property
    def items_with_empty(self) -> list[TemplateRepo]:
        return [TemplateRepo.empty(), *self.items]


def _check_git_integration() -> GitStatus:
    """Checks the GitHub integration status.

    This function checks for the following items:
    1. Git is installed and executable on PATH.
    2. Git Credential Manager is installed.
    3. Git Credential Manager is configured as the credential helper for git.
    :return: A flag containing the integration status.
    """
    # Holds the status
    git_status = GitStatus(0)
    # If git is not installed, we cannot continue.
    git_path = shutil.which("git")
    if git_path is None:
        print_failure(get_user_prompt_by_name("install_git.txt"))
    else:
        print_success(f"Found git ({git_path}).")
        git_status |= GitStatus.GIT_INSTALLED

    # Next, check if git is using Git Credential Manager by running a git command and checking the version
    cred_manager_version = subprocess.run(
        ["git", "credential-manager", "--version"], capture_output=True, check=True
    ).stdout.decode("utf-8")
    # Parse the string
    if git_path is None or "is not a git command" in cred_manager_version.lower():
        print_failure(get_user_prompt_by_name("install_git_credential_manager.txt"))
    else:
        vers_display = cred_manager_version.split("+")[0]
        print_success(f"Found git credential-manager (version {vers_display}).")
        git_status |= GitStatus.GCM_INSTALLED

    # Now, check if credential-manager is configured as the helper for Git credentials.
    cred_helper = subprocess.run(["git", "config", "credential.helper"], capture_output=True, check=True).stdout.decode(
        "utf-8"
    )
    if git_path is None or "manager" not in cred_helper.lower():
        print_failure(
            f"Git Credential Manager is not configured as your credential helper. Expected: 'manager', Found: '{cred_helper}'"
        )
    else:
        print_success("Git Credential Manager is configured as the credential helper.")
        git_status |= GitStatus.GCM_CONFIGURED

    # Return the result
    return git_status


def _write_default_token(github_token: str) -> None:
    """Writes the GitHub token to the CIDA defaults file.
    :return: The GitHub token to write.
    """
    defaults = CIDADefaults()
    try:
        defaults.github_token = github_token
    except (AttributeError, FileNotFoundError, ValidationError):
        print_failure("Unable to set GitHub token.")
    print_success(f"Github token has been written to {defaults.path}")


def _read_default_token() -> str | None:
    """Reads the GitHub token from the CIDA defaults file.
    :return: A string token or None
    """
    defaults = CIDADefaults()
    try:
        return defaults.github_token
    except (AttributeError, FileNotFoundError, ValidationError):
        print_failure("Unable to read GitHub token.")
    return None


def setup_github():
    """Function to guide users through setting up cidatools GitHub integration.
    :return: None
    """
    # TODO: Should this be a common resource so the R version can use the same text?
    prompt_str = """
    To create a new GitHub Token:
    1. Navigate to https://github.com/settings/tokens and select 'Personal access tokens → Tokens (classic)'.
    2. Click 'Generate new token → Generate new token (classic).'
    3. Name your token, set 'Expiration = No expiration' and check the 'repo (full control of private repositories)' scope.
    4. Copy the generated token, and paste into the prompt below.
    5. On the tokens page, find your token and click 'Configure SSO → CIDA-CSPH'. This will allow the token to access repositories in the CIDA organization.

    IMPORTANT: Do not hardcode this token into any code, scripts, or files you commit to GitHub!
    """
    # Check the current GitHub integration status
    git_status = _check_git_integration()

    # If GCM is not set up, prompt user for manual token.
    if git_status.GCM_CONFIGURED:
        print_success("Detected Git Credential Manager is installed and configured.")
        return
    elif not (git_status.GCM_CONFIGURED and git_status.GCM_INSTALLED):
        print_info(
            "Git Credential Manager is not installed, CIDAtools can still work with a manually configured GitHub token."
        )

    resp_l = None
    while resp_l not in ["y", "n"]:
        response = input("Would you like to configure a GitHub token now? [y/N]:")
        if response == "":
            resp_l = "n"
        else:
            resp_l = response.strip().lower()

    if resp_l == "y":
        # Prompt the user.
        print(prompt_str)
        # Retrieve the GitHub token from file.
        github_token = input("Enter GitHub Token:")
        # Write GitHub token to file.
        _write_default_token(github_token)
    elif resp_l == "n":
        return


def _get_gcm_token() -> str | None:
    """Function to retrieve GCM token"""
    # Retrieve a token from GCM
    raw_gcm_out = subprocess.run(
        ["git", "credential-manager", "get"],
        capture_output=True,
        input=b"protocol=https\nhost=github.com\n\n",
        check=True,
    ).stdout.decode()
    if not raw_gcm_out.startswith("protocol=https"):
        print_failure("Unable to retrieve GCM token.")
        return None
    # Parse the GCM response
    gcm_creds = {k: v for k, v in [tup.split("=") for tup in raw_gcm_out.strip().split("\n")]}
    print_info(f"Retrieved GCM token for user '{gcm_creds['username']}'")
    # Return the token
    return gcm_creds["password"]


def _get_github_token() -> str:
    """Retrieves a GitHub token to be used for CIDAtools functionality
    This function will first attempt to retrieve a GitHub token from Git Credential Manager, but will
    also check the CIDADefaults.
    """
    # First, try GCM
    token = _get_gcm_token()
    # Check if we got something
    if token is not None:
        return token
    # If GCM didn't work, check for something in CIDA defaults
    token = _read_default_token()
    # Check if we got something.
    if token is not None:
        return token
    print_failure("Unable to retrieve GitHub token from GCM or CIDA defaults.")
    return None


def list_github_templates(display: bool = True, include_empty: bool = True) -> list[TemplateRepo] | None:
    """Function to list the available CIDAtools GitHub templates.
    :param display: Whether to print the list of templates to the console.
    :param include_empty: If true, include the option to create an empty repository.
    :return: A list of `TemplateRepo` objects.
    """
    # Check if we have a GitHub token set.
    gh_token = _get_github_token()
    if gh_token is None:
        print_failure("Unable to list template repositories.")
        return None
    # Perform the search request
    resp = requests.get(
        GITHUB_SEARCH_API_URL + "?q=org:CIDA-CSPH+topic:cidatools-template",
        headers={
            "User-Agent": "CIDA-CSPH/CIDAtools",
            "Accept": "application/vnd.github+json",
            "Authorization": f"Bearer {gh_token}",
        },
    )
    if resp.status_code != 200:
        print_failure("Unable to list template repositories.")
        return None
    # Construct the model for the response.
    template_list = TemplateRepoList.model_validate(resp.json())

    # Choose the correct item list to display.
    items_list = template_list.items_with_empty if include_empty else template_list.items

    # Only print if requested.
    if display:
        # Length of longest list ID.
        i_len = len(str(len(template_list.items)))
        # Length of longest repo name.
        repo_len = max(len(r.name) for r in template_list.items)
        # Display the choices
        print(
            "\n".join(
                f"[{{i:{i_len}}}] {{name:{repo_len}}} - {{description}}".format(
                    i=i, name=repo_i.name, description=repo_i.description
                )
                for i, repo_i in enumerate(items_list)
            )
        )
    # Return the list of templates
    return items_list


def _pre_create_github_repository(name: str, visibility: str) -> tuple[bool, str | None, str | None]:
    """Internal function which runs before GitHub repository creation.

    This function performs a few tasks:
    1. Validate that `name` is a valid GitHub repository name.
    2. Validate that `name` is not an existing GitHub repository name.
    :param name: The name of the new repository.
    :param visibility: The visibility for the repository (default: 'internal')
    :return:
    """
    # Check if repo name is valid
    if re.match(r"[^a-zA-Z0-9_\-\.]", name):
        print_failure(f"{name} is not a valid repository name.")
        return False, None, None

    # Check if visibility is correct
    if visibility not in ["internal", "private", "public"]:
        print_failure(f"{visibility} is not a valid visibility setting.")
        return False, None, None
    elif visibility == "public":
        print_failure("Creating a public repository is not supported.")
        return False, None, None

    # Check if we have a GitHub token set.
    gh_token = _get_github_token()
    if gh_token is None:
        print_failure("Unable to check existence of GitHub repository.")
        return False, None, None

    # Check if repo exists already
    exist_resp = requests.get(
        posixjoin(GITHUB_REPO_API_URL, name),
        headers={
            "User-Agent": "CIDA-CSPH/CIDAtools",
            "Accept": "application/vnd.github+json",
            "Authorization": f"Bearer {gh_token}",
            "X-GitHub-Api-Version": "2026-03-10",
        },
    )
    exist_json = exist_resp.json()
    # Check the status code on the API response.
    repo_url = posixjoin(CIDA_GITHUB_ORGANIZATION, name)
    if (
        exist_resp.status_code == 404
        and exist_json.get("documentation_url") == "https://docs.github.com/rest/repos/repos#get-a-repository"
    ):
        print_success(f"Repository {repo_url} is available.")
        return True, gh_token, repo_url
    elif exist_resp.status_code in [200, 301]:
        print_failure(f"Unable to create repository, {repo_url} already exists.")
        return False, None, None
    elif exist_resp.status_code == 403:
        print_failure(f"Unable to check for repository existence (Status: {exist_resp.status_code}).")
        return False, None, None
    else:
        print_failure(f"Unable to create GitHub repository (Status: {exist_resp.status_code}).")
        return False, None, None


def create_empty_github_repository(
    name: str,
    description: str | None,
    visibility: Literal["internal", "private", "public"] = "internal",
) -> str | None:
    """Function to create a new empty GitHub repository.
    :param name: The name of the repository to create (e.g. https://github.com/CIDA-CSPH/<name>)
        `name` must be a valid URL component (i.e. no spaces, special characters, etc).
    :param description: An optional description for the repository. (default: None)
    :param visibility: The visibility for the repository (default: 'internal').
        Currently, creating 'public' repositories is not supported to prevent unintentional
        data leakage.
    :return: A URL pointing to the new repository or None
    """
    status, token, repo_url = _pre_create_github_repository(name=name, visibility=visibility)
    if not status:
        return None

    # Perform the repository creation request
    resp = requests.post(
        GITHUB_REPO_CREATE_API_URL,
        headers={
            "User-Agent": "CIDA-CSPH/CIDAtools",
            "Accept": "application/vnd.github+json",
            "Authorization": f"Bearer {token}",
            "X-GitHub-Api-Version": "2026-03-10",
        },
        json={
            "name": name,
            "description": description,
            "visibility": visibility,
        },
    )

    # Handle the API response, which tells us if the repo was created or not.
    if resp.status_code == 201:
        print_success(f"Successfully created a new GitHub repository at: {repo_url}")
        return repo_url
    elif resp.status_code == 403:
        print_failure(f"Forbidden from creating a new GitHub repository: (Status: {resp.status_code}).")
        return None
    else:
        print_failure(f"Unable to create a new GitHub repository: (Status: {resp.status_code}).")
        return None


def create_github_repository_from_template(
    name: str,
    template_name: str,
    description: str | None,
    visibility: Literal["internal", "private", "public"] = "internal",
) -> str | None:
    """Function to create a new GitHub repository from a template.
    :param name: The name of the repository to create (e.g. https://github.com/CIDA-CSPH/<name>)
    :param template_name: The name of the template repository to use (e.g. CIDA-CSPH/<template_name>)
    :param description: An optional description for the repository. (default: None)
    :param visibility: The visibility for the repository (default: 'internal')
    :return: A URL pointing to the new repository or None
    """
    status, token, repo_url = _pre_create_github_repository(name=name, visibility=visibility)
    if not status:
        return None

    # Make the request to create the new repository.
    resp = requests.post(
        GITHUB_REPO_CREATE_FROM_TEMPLATE_API_URL.format(template_name=template_name),
        headers={
            "User-Agent": "CIDA-CSPH/CIDAtools",
            "Accept": "application/vnd.github+json",
            "Authorization": f"Bearer {token}",
            "X-GitHub-Api-Version": "2026-03-10",
        },
        json={"owner": "CIDA-CSPH", "name": name, "description": description, "private": True},
    )

    # Check status code
    if resp.status_code == 201:
        ret_val = repo_url
    else:
        ret_val = None

    # If the repo is private, we are done.
    if ret_val is not None and visibility == "internal":
        # Due to GitHub API limitation, we cannot create an 'internal' repo in a single step,
        # so we must create the repo as private, then send a follow-up request to modify the
        # repository visibility.
        update_resp = requests.patch(
            posixjoin(GITHUB_REPO_API_URL, name),
            headers={
                "User-Agent": "CIDA-CSPH/CIDAtools",
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {token}",
                "X-GitHub-Api-Version": "2026-03-10",
            },
            json={"visibility": "internal"},
        )

        # Check status code on our request
        if update_resp.status_code == 200:
            print_success("Successfully updated repository visibility (internal).")
        elif update_resp.status_code == 422:
            print_warning(
                "Insufficient permission to set repo visibility to 'internal', visibility will remain 'private'."
            )
        else:
            print_warning(
                f"Unable to set repo visibility to 'internal', visibility will remain 'private'. (Status: {update_resp.status_code})."
            )

    # Print the deferred success or failure message for the initial repository create, so that this
    # message appears last for users.
    if ret_val is None:
        print_failure(f"Unable to create a new GitHub repository: (Status: {resp.status_code}).")
    else:
        print_success(f"Successfully created a new GitHub repository at: {repo_url}")

    # Return the result.
    return ret_val


def create_github_repository(
    name: str,
    description: str | None = None,
    visibility: Literal["internal", "private", "public"] = "internal",
    template_name: str | None = None,
) -> str | None:
    """Function to create a new GitHub repository.
    This function is intended for interactive use, and is a wrapper around the more specific
    `create_empty_github_repository()` and `create_github_repository_from_template()` functions.

    If you do not need the interactive component (i.e. in a script), call one of those functions instead.
    :param name: A name for the repository. (e.g. https://github.com/CIDA-CSPH/<name>)
    :param description: An optional description for the repository. (default: None)
    :param visibility: The visibility of the new GitHub repo.
    :param template_name: The name of the template to use. (default: None)
        If None, will interactively prompt the user to choose a template.
        If 'empty', will create a new empty GitHub repo.
        Otherwise, will attempt to create a GitHub repo with the given template name.
    :return: A string containing the GitHub repository URL, or None if repo creation failed.
    """
    # Retrieve the available templates.
    template_list = list_github_templates(include_empty=True, display=template_name is None)

    # If we cannot list templates, we cannot continue.
    if template_list is None:
        print_failure("Unable to list templates.")
        return None

    # If a template name is not chosen, prompt interactively.
    if template_name is None:
        # The user's selection defaults to 0 (an empty repository)
        user_i = None
        while user_i not in range(len(template_list)):
            user_choice = input("Choose a template from the above list (default 0): ")
            if user_choice == "":
                user_i = 0
            else:
                try:
                    user_i = int(user_choice)
                except ValueError:
                    pass
        # Use the user's selection to choose the template.
        _template_name = template_list[user_i].name
    else:
        # List all template names
        valid_template_names = [template.name for template in template_list]
        # Check that the chosen name is one of the options.
        if not any(template_name == valid_template_name for valid_template_name in valid_template_names):
            print_failure(f"Template '{template_name}' is not a a valid template in: {', '.join(valid_template_names)}")
            return None
        # Assign the template name
        _template_name = template_name

    # If an empty project is requested, create it.
    if _template_name == "empty":
        repo_url = create_empty_github_repository(
            name=name,
            description=description,
            visibility=visibility,
        )
    # Otherwise create from the selected template.
    else:
        repo_url = create_github_repository_from_template(
            name=name,
            description=description,
            visibility=visibility,
            template_name=_template_name,
        )

    return repo_url


def clone_github_repository(repository_url: str, local_path: pathlib.Path) -> bool:
    """Clones a GitHub repository via git.
    :param repository_url: URL of the repository to clone
    :param local_path: The path where the repository should be cloned.
    :return:
    """
    # The local path should either not exist, or exist but be a directory.
    if local_path.exists() and not local_path.is_dir():
        print_failure("Path exists but is not a directory.")

    # Run the clone operation.
    try:
        clone_res = subprocess.run(
            ["git", "clone", repository_url, local_path.absolute()], check=True, capture_output=True
        )
    except subprocess.CalledProcessError:
        return False

    # Success is determined by status code.
    return clone_res.returncode == 0


def get_git_remote_url(project_root: pathlib.Path | None = None, name: str = "origin") -> str | None:
    """Checks the URL for a git remote.
    :param project_root: The root directory of the project to get the remote URL for.
    :param name: The name of the remote to fetch the URL for (default: 'origin').
    :return: str containing URL name or None
    """
    project_root = pathlib.Path.cwd() if project_root is None else project_root
    # Use subprocess to run git.
    try:
        remote_out = subprocess.run(
            ["git", "-C", str(project_root.absolute()), "remote", "get-url", name], check=True, capture_output=True
        )
    except subprocess.CalledProcessError:
        return None
    # If we get an error, return None
    if remote_out.returncode != 0:
        return None
    # Get the url provided by Git.
    remote_url = remote_out.stdout.decode().strip()
    # Return the URL
    return remote_url
