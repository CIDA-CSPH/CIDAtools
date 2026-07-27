**Project Name**: {{ config.project_name }}

**PI**: {{ config.principal_investigator }}

**Analyst**: {% if config.analyst is string or config.analyst is none %} {{ config.analyst }} {% else %} {{ config.analyst | join(', ') }} {% endif %}

**CIDA Drive Location**: {{ config.data_location }}

**GitHub Location**: {{ config.git_location }}

Details about the folders:

| File             | Description                                                                       |
|------------------|-----------------------------------------------------------------------------------|
| `Admin/`         | Contains the scope of work and other administrative documents                     |
| `Background/`    | Contains the background information for the analysis                              |
| `Code/`          | Contains all scripts and code for this project                                    |
| `DataRaw/`       | Contains all raw data provided by investigators                                   |
| `DataProcessed/` | Contains the processed data used for analysis                                     |
| `Dissemination/` | Contains any materials produced for dissemination, ie. Abstracts, Posters, Papers |
| `Reports/`       | Contains all output, rmarkdown files and report                                   |
