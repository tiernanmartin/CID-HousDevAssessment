# Example showing the purpose of the function ----

options(httr_oob_default=TRUE) 
drive_auth(new_user = TRUE)

mkdir_df <- tibble::tribble(
                                        ~name,                                                    ~path,
                      "CID-HousDevAssessment",                               "~/Futurewise/YCC/SCIDpda",
                                     "1-data",         "~/Futurewise/YCC/SCIDpda/CID-HousDevAssessment",
                                      "1-raw",  "~/Futurewise/YCC/SCIDpda/CID-HousDevAssessment/1-data",
                                 "2-external",  "~/Futurewise/YCC/SCIDpda/CID-HousDevAssessment/1-data",
                                  "3-interim",  "~/Futurewise/YCC/SCIDpda/CID-HousDevAssessment/1-data",
                                    "4-ready",  "~/Futurewise/YCC/SCIDpda/CID-HousDevAssessment/1-data"
                     )


mkdir_df %>% 
        transpose %>% 
        map(~ drive_mkdir(name = .x$name, path = .x$path, verbose = FALSE) %>% drive_share(role = "commenter", type = "anyone")
        )

# Upload some files ----

# Prepare the names and paths

files_df <- 
        tibble("from" = list.files('./1-data/2-external',full.names = TRUE)) %>% 
        mutate(name = basename(from))


# double-check the folder exists

dest_folder <- drive_path("~/Futurewise/YCC/SCIDpda/CID-HousDevAssessment/1-data/2-external")
is_folder(dest_folder)


# Upload

files_df %>% 
        transpose %>% 
        map(
                ~ drive_upload(from = .x$from, 
                               name = .x$name,
                               folder = dest_folder)
    )

