---
icon: lucide/rocket
title: Getting Started
---


## (Optional) GitHub Configuration

Once CIDAtools is installed, you can optionally configure GitHub integration, which allows you to use
functions like `create_github_repository()` and `create_github_project()`.

=== "R"
    ```R
    CIDAtools::setup_github()
    ```

=== "Python"
    ```python
    # Import CIDAtools
    import cidatools
    # Run the setup function
    cidatools.setup_github()
    ```

=== "CLI"
    ```shell
    cidatools setup github
    ```

CIDAtools uses a GitHub token in order to authenticate GitHub API requests. 

The GitHub setup process will first determine if Git Credential Manager (GCM) is installed and configured. If so, CIDAtools can use a token provided by GCM.
If GCM is not installed, you can also manually configure a GitHub Token which CIDAtools can use.