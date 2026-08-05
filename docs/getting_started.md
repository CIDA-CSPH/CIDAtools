---
icon: lucide/rocket
title: Getting Started
---

## Installing CIDAtools

CIDAtools is available for R, Python and CLI.

=== "R"

    ```R
    install.packages("pak") # Only required if pak is not installed
    pak::pak('CIDA-CSPH/CIDAtools')
    ```

    !!! info
    
        If you encounter issues with the new version of CIDAtools (v2.0.0+), you can install the older version using: 
        
        <pre><code>pak::pak("CIDA-CSPH/CIDAtools<mark>@v0.1.2</mark>")</pre></code>

=== "Python/CLI"

    ```python
    pip install git+https://github.com/CIDA-CSPH/CIDAtools#subdirectory=python
    ```
    
    !!! info
        
        To use the CLI version of CIDAtools, install Python and the CIDAtools package using the instructions above.
        After installation, the `cidatools` command will be available in your terminal.


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