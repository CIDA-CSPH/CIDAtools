---
icon: lucide/braces
---

# Contributing

We welcome suggestions and contributions for CIDAtools!

For organization and tracking purposes, we ask that all contributions are submitted through GitHub. 

## Submitting an Issue
To submit an issue, navigate to the [issues page](https://github.com/CIDA-CSPH/CIDAtools/issues) for this repository. 

When creating an issue, you can assign specific 'tags' which relate to the type of request you are making (i.e. bug report, feature request, etc.)

The GitHub issues page is useful for:

- **Bug/Issue Reports** - Reports for broken links, visual bug, outdated/incorrect information, etc.
- **Feature Requests** - If you would have a request for a feature/function/etc that you would like to see in CIDAtools, feel free to submit an issue!

## Submitting a Pull Request
We are happy to review pull requests for bug fixes and feature additions.

All pull requests will be reviewed by the Research Tools Committee for inclusion into the next release of CIDAtools.

See the [Making Changes](#making-changes) section for detailed instructions on how to set up a local environment for developing CIDAtools.


## Making Changes

The CIDAtools package is a polyglot repo, with the Python (`cidatools`) and R (`CIDAtools`) packages living in the `python/` and `R/` subdirectories respectively. The `cidatools` Python package also houses the command line (CLI) version of CIDAtools.

Shared resources such as data files, templates, user prompts, etc are stored in the top level `resources/` directory, which is then symlinked into the `python/` and `R/` subdirectories.

To begin local development of CIDAtools, we recommend the following:

### 1. Fork the repository
Forking the CIDAtools repository creates a copy of the repository under your own GitHub username, which you can then edit freely. 

You can create a fork using the 'Fork' button on the repository's [GitHub page](https://github.com/CIDA-CSPH/CIDAtools).

### 2. Clone the forked repository
You can clone your fork of the site repository using any method you find convenient (command line, RStudio, etc).

```shell
git clone https://<your_github_username>/CIDAtools.git
```

### 3. Configure local development environment 

#### R Setup
You can build and test the R package using `pak::local_install("R/")`

#### Python/CLI Setup
To set up the Python package, we recommend using a `uv` environment. 

With `uv` installed, run:

```shell
cd python/ && uv sync
```

to set up a `uv` environment in the `python/` subdirectory.

All tests are run using `pytest`.

#### `pre-commit` Git Hooks
You can optionally set up the `pre-commit` git hooks using:

```shell
uv run pre-commit install
```

which will run automated checks and file formatting each time you `git commit`.