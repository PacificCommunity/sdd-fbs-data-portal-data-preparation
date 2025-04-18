# Fiji Data portal data preparation

A general template for FBS Data Portal data preparation.

# Usage

The project is used basically to create and assemble together R scripts to generate and prepare data in the standard SDMX format ready to be uploaded and hosted by the FBS national data portal.

# Folder structure

There are four main folders in this repository:
- `docs`: Contains the documentation of the project.
- `data`: Contains temporary local copies of the raw data used in the project. This folder won't be uploaded to the repository.
- `output`: Contains the temporary output files generated by the project (png, pdfs, small data units). This folder won't be uploaded to the repository.

# gitignore

The `.gitignore` file is configured to ignore the most common development temporary files for Python, R, and Stata. It also ignore most file formats in the `/temp/` subdirectories.