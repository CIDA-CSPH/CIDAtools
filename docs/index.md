---
icon: lucide/house
title: CIDAtools Overview
---

# CIDATools

## Overview

This package contains numerous templates and tools to make life at CIDA (part of the Colorado School of Public Health) happier and more efficient!

To get started,

## Installing CIDAtools

CIDAtools is available for R and Python, and also has a command line interface (CLI).

Version-specific installation instructions are available below:

=== "R"
    In an R console, run:

    ```R
    install.packages("pak") # Only required if pak is not installed
    pak::pak('CIDA-CSPH/CIDAtools')
    ```

    !!! info
    
        If you encounter issues with the new version of CIDAtools (v2.0.0+), you can install the older version using 
        
        <pre><code>pak::pak("CIDA-CSPH/CIDAtools<mark>@v0.1.2</mark>")</pre></code>

=== "Python/CLI"
    
    If not installed, Python can be downloaded from the [official Python website](https://www.python.org).
    
    Afterwards, open a terminal (Terminal on Mac or Powershell/Terminal on PC) and run
    ```shell
    pip install git+https://github.com/CIDA-CSPH/CIDAtools#subdirectory=python
    ```
    to install the CIDAtools package via `pip`, Python's package manager.
    !!! tip "CIDAtools at the Command Line"
        The command line version of cidatools is included with the `cidatools` Python package.

        After installing the package, the `cidatools` command will be available at the command line.

        You can run:
        <pre><code>cidatools --help</code></pre>
        to see the available commands.

