
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EPICIndia

<hr>

   

## Introduction

<hr>

-   EPICIndia is the official code repository for all PMU work streams.

 

## File Structure

The code is structured as a standard R package. For those unfamiliar
with the R package framework, it is very simple to understand. Here is a
quick summary:

-   Each project has its own R Markdown file (with .Rmd extension, which
    is a reporting format which combines code and the narrative for the
    code).

-   The markdown file can be compiled (or “knitted” in R terminology) in
    part (running code for a specific section) or in full (i.e. knitting
    the entire file) and the result will either be: (a) self contained
    reports (e.g. puccSurveyReport.Rmd gives us the
    daily/weekly/monthly/any time period report for the pucc CDV’s
    survey). (b) output files that are a result of some specific process
    (e.g. cleaning, partitioning, graphing, etc.).

-   For each markdown file (or in other words for each project) there is
    a helper R script (.R extension, with the same name as the project
    and an additional “utils” keyword attached to it) that contains
    helper functions that were written for that project (can be used in
    other projects too). R scripts for all projects can be found in the
    “R” subdirectory.

-   All output files for each project can be found in the **output**
    sub-directory (as of now I have not included this folder because the
    data files are huge). Within the “output” sub-directory, output
    files for each type of project are contained in their own sub-folder
    (which is named the same as the project name but with an added
    “outputFiles” suffix at the end). **Before running the code, please
    take this folder separately from me and add it to the root of the
    EPICIndia repository. Also, please do not edit the names of the
    folders or sub-folders as they all follow a specific naming scheme,
    which should not be altered, unless discussed with me. All paths in
    code are set relative to the root of the repository**

-   Any raw data files for each of these projects are found in the
    “data-raw” sub-directory (as of now I have not included this folder
    because the data files are huge). Within the “data-raw”
    sub-directory, raw data files for each type of project are contained
    in their own sub-folder (which is named the same as the project
    name). **Before running the code, please take this folder separately
    from me and add it to the root of the EPICIndia repository. Also,
    please do not edit the names of the folders or sub-folders as they
    all follow a specific naming scheme, which should not be altered,
    unless discussed with me. All paths in code are set relative to the
    root of the repository**

-   Description file contains information like the name of the author,
    license information, package version, etc.

-   Namespace file: Contains all the functions that are being used in
    all the projects. These will be loaded once anyone downloads the
    package.

-   Readme.Rmd file is the file that renders the Readme.md file that
    shows up right below the code once you open the GitHub repository.

-   EPICIndia.Rproj file is an R project file, which is the file that
    upon double clicking will open the EPICIndia project in RStudio.

-   The **R/** subfolder contains some other files e.g. **app_ui.R**,
    **app_server.R**, etc. All these files are not relevant for now.
    These will be used when we want to make a R Shiny dashboard for this
    project.

## How to run the code?

As of now, the best way to run code is the following (in future, I will
publish this as a package and then it can be downloaded by a single R
command):

-   Download this repository as a zip file.

-   Unzip it.

-   Take the **data-raw** and **output** folders from Aarsh and add it
    to the root of the downloaded repository. **Make no changes to the
    folder or file names within the folders. All names follow a specific
    naming scheme, which should not be altered unless discussed with
    Aarsh.**

-   Open the EPICIndia.Rproj file in R Studio. For those unfamiliar with
    R Studio: R Studio is one of the best IDE’s for R. There are others
    like Jupyter Notebook, etc. But, I recommend R Studio. It is free
    and can be downloaded from the R Studio website. At this point, I
    assume that you have already downloaded the latest version of R
    Studio and R.

-   Make sure that you have all relevant packages installed. If you are
    regular R user, most of these packages are probably already
    installed on your system. If you are new to R, a package (or a
    library) is a set of functions that someone else wrote, and is
    available for use to everyone in the community. It is very simple to
    download these packages. Here are a few examples on how to download
    a package:

    -   General Syntax for downloading a single package:
        `install.packages("name_of_package", dependencies = TRUE)`

    -   Example for downloading a single package:
        `install.packages("tidyverse", dependencies = TRUE)`

    -   General Syntax for downloading multiple packages at once:
        `install.packages(c("package1", "package2"), dependencies = TRUE)`

    -   Example for downloading multiple packages at once:
        `install.packages(c("stringr", "dplyr"), dependencies = TRUE)`.

    -   Note: The `dependencies = TRUE` argument automatically detects
        and downloads any additional packages which are needed by your
        current package (the one you are downloading).

-   Here is the list of packages that should be downloaded on your
    system:

-   Once you have downloaded these packages, you can open any of the
    project Rmd files and depending on the context (see project listing
    section below), execute code section wise or knit all at once
    (e.g. to generate the report of pucc survey by CDV’s).

## Projects Listing

As of now there are 4 projects:

-   **Pollution Under Check Certificate Checking by Civil Defence
    Volunteers**

    -   All code for this is contained in a single project called
        **puccSurveyReport.Rmd** file which can be found at the root of
        the repository. It’s corresponding helper functions can be found
        in the **R/** subdirectory under the name
        **puccSurveyReportRmdScript_utils.R**.

    -   You can knit this file by clicking the knit button in R Studio.
        By default, it will generate the daily report for yesterday
        (SystemDate - 1).

    -   To generate Weekly, Monthly reports, etc, choose the knit with
        parameters dialogue box which will open a GUI that will allow
        you to select the custom date range for report generation.

    -   Note: Before knitting, please take the **data-raw** and
        **output** folders from me and add it to the root of the
        repository.

-   **Pollution under check certificates Data Dump (2015-2021)**

    -   This contains 2 projects: **puccRawDataDumpPartitioning.Rmd**
        and **puccRawDataDumpCleaning.Rmd**.

    -   **puccRawDataDumpPartitioning.Rmd**

        -   Contains the description of raw data files that were
            received from the transport department.

        -   Contains code that partitions the master files for each
            table (there are a total of 5 types of tables) and stores
            these partitions in a single list. Although we did receive
            partitioned files for each table from the transport
            department, each partition were too big. This is why I used
            the master files for each table to recreate the partitioned
            files such that each file contains a maximum of 1 million
            rows. This way each partition can be opened in Excel. In
            creating these partitions, I assigned a specific naming
            schema for each table and columns within those tables. I
            also assigned the default column types for each column of
            each table.

        -   Next, I use this list to export each partition into its own
            excel file.

        -   Finally, it contains code to combine all the partitions of a
            given table into a single master table.

        -   All of the helper functions used can be found in
            **R/puccRawDataDumpPartitioningRmdScript_utils.R**.

    -   **puccRawDataDumpCleaning.Rmd**

        -   In this project, we first explore a sample file from each
            table to determine broad cleaning steps. There are five
            sections for this exploratory data analysis, one for each
            table.

        -   Post, EDA (exploratory data analysis): use the master
            cleaning function (which is present in:
            **R/puccRawDataDumpCleaningRmdScript_utils.R**, along with
            the other helper functions for this project) which reads in
            all the partitioned raw files and output their cleaned
            versions into the relevant sub-folder in the **ouptut**
            sub-directory.

        -   Then there is code for combining and exporting all the
            cleaned partitions of a given table into a single master
            file for that table. At the end of this process, we have
            five master cleaned files for each of the five tables.

        -   Next, to perform data analysis on these files, we have to
            consider their size. Analysis on individual partitions is
            relatively fast within R (by reading them in as tibbles).
            But, for analysis on master files, it is recommended that we
            either use data.table, a remote database or a cluster
            computing framework like spark.

        -   Next, up in this project, I explore various ways to read the
            master tables in so that analysis can be performed. First, I
            read master tables directly into R (as tibbles), this is
            slow as R stores these objects in their entirety in memory.
            The other way that I explore is reading the master tables
            into a remote database (still local) e.g. postgres, sqlite.
            This is relatively faster, as we perform analysis in the
            remote database and only pull in the final results in R
            (which is also read in by reference and that too by lazy
            loading, so as to boost the speed).

        -   But, still the analysis speed can be significantly boosted
            (especially if we want to perform fast analysis on the
            master tables) if we use big data cluster computing
            frameworks like Apache Spark. R has a package called
            `sparklyr` which allows us to leverage the power of spark.
            But, for this we need to get a small cluster. This should be
            the next step.

-   **Small Tasks Compilation**

    -   This contains a single project called **smallTasks.Rmd**.

    -   It contains the code for all small ad-hoc data analysis tasks,
        each task has its own section.

    -   Depending on the task you’d like to run, you can execute the
        code in the relevant sub-sections.

    -   All of the helper functions for this project can be found in
        **R/smallTasksRmdScript_utils.R**.

<!-- badges: start -->
<!-- badges: end -->
