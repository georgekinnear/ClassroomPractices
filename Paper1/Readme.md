# Paper 1 - Data and code

This folder contains R scripts and the anonymised dataset for this paper:

* Kinnear, G., Smith, S., Anderson, R., Gant, T., MacKay, J. R. D., Dr, Docherty, P., … Galloway, R. (2020, April 7). Classroom practices can be reliably classified using lecture recordings. https://doi.org/10.31219/osf.io/7n6qt

## Prepare anonymous data

[Paper1_prepare_anon_data.R](Paper1_prepare_anon_data.R)

This script:
* reads in a spreadsheet of course details,
* traverses a folder structure to read in .csv files of FILL+ coding data,
* appends the codes to the spreadsheet data,
* converts course/lecturer names to pseudonyms,
* performs various sanity checks on the data, e.g. that there are no periods with negative duration (which can be caused by mistyping the time that a change of code takes place)

The data that this script relies on are not provided, to preserve anonymity of the courses/lecturers involved. However, we do provide example data for one course, ILA. **TODO**

The script outputs an RDS file containing all the anonymised data:

* [all_codings_anon.rds](all_codings_anon.rds)

## Tool comparison

[Paper1_tool_comparison.Rmd](Paper1_tool_comparison.Rmd) ([knitted PDF](Paper1_tool_comparison.pdf))

This script reads in the FILL and PORTAAL coding data from the 8 pilot lectures (files named "SS_tool_comparison*.csv"), and produces the plots in Fig 1 and Fig 2 in the paper.



## Analyse FILL+ data

[Paper1_FILLplus_analysis.R](Paper1_FILLplus_analysis.R)

This script reads in the anonymised data, and performs all of the analyses discussed in the paper.

In particular, it produces:
* a spreadsheet of all the anonymised data: [anon_codes_with_durations.csv](anon_codes_with_durations.csv)
* various plots, including Figures 3-6 in the paper, and those in the Appendix.
