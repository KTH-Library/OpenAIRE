library(usethis)
use_data_raw()
use_readme_rmd()
use_mit_license("CC0")

library(desc)

unlink("DESCRIPTION")

my_desc <- description$new("!new")
my_desc$set("Package", "OpenAIRE")
my_desc$set("Authors@R", "person('Markus', 'Skyttner', email = 'markussk@kth.se', role = c('cre', 'aut'))")

my_desc$del("Maintainer")

my_desc$set_version("0.0.0.9000")

my_desc$set(Title = "OpenAIRE - Open Scholarly Communication Infrastructure Supporting European Research")

my_desc$set(Description = "OpenAIRE is an open science initiative, which supports the Open Access policy of the European Commission. This R package OpenAIRE provides access from within R to the OpenAIRE RESTful APIs, which are intended for metadata discovery and exploration only.")
my_desc$set("URL", "https://github.com/KTH-Library/OpenAIRE")
my_desc$set("BugReports", "https://github.com/KTH-Library/OpenAIRE/issues")
my_desc$set("License", "MIT")
my_desc$write(file = "DESCRIPTION")

desc_add_author("Mohamad", "Bazzi", "bazzi@kth.se", "aut")

#use_mit_license(name = "Markus Skyttner")
#use_code_of_conduct()
use_news_md()
use_lifecycle_badge("Experimental")

use_tidy_description()

use_testthat()
#use_vignette("swecris")

use_package("purrr")
use_package("dplyr")
use_package("xml2")
use_package("httr")
use_package("lubridate")

use_roxygen_md()
use_pkgdown_github_pages()

document()
test()
check()
pkgdown::build_site()

use_github(organisation = "KTH-Library")

use_github_actions()
use_github_actions_badge()
use_pipe()

use_package("magrittr")
use_package("glue")
use_package("tibble")
#use_package("lubridate")
use_package("readr")
use_package("progress")
use_package("rlang")

non_ascii_fixer <- function(x) {
  has_non_ascii <- length(tools::showNonASCII(x)) > 0
  if (has_non_ascii) {
    message("Fix to:")
    return(cat(stringi::stri_escape_unicode(x)))
  }
  x
}

#tools::showNonASCIIfile()
non_ascii_fixer("Kungliga tekniska högskolan")
stringi::stri_escape_unicode("➛")

chartr("\u201c\u201d", "\"\"", x)
cat("\u201c\u201d")
"“quoted”" |> iconv(toRaw = TRUE)
"“quoted”" |> stringi::stri_escape_unicode() |> stringi::stri_unescape_unicode()
