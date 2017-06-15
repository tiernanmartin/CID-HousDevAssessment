mkdir_df <- tibble::tribble(
                   ~name,                                      ~path,
 "CID-HousDevAssessment",                       "~/data/futurewise/",
                 "1-raw",  "~/data/futurewise/CID-HousDevAssessment",
            "2-external",  "~/data/futurewise/CID-HousDevAssessment",
             "3-interim",  "~/data/futurewise/CID-HousDevAssessment",
               "4-ready",  "~/data/futurewise/CID-HousDevAssessment"
                                                                        )
mkdir_df %>% 
        transpose %>% 
        map(~ drive_mkdir(name = .x$name, path = .x$path, verbose = FALSE) %>% drive_share(role = "commenter", type = "anyone")
        )


df <- tibble("folders" = list.files(path = "./1-data",all.files = FALSE,full.names = TRUE)) %>% 
        mutate(name = basename(folders))

test_folder <- df[1,1][[1]]

test_name <- df[1,2][[1]]

drive_upload(from = test_folder,
             name = test_name,
             folder = drive_path("~/data/futurewise/CID-HousDevAssessment"),
             type = "application/vnd.google-apps.folder")
