# conosle prompt to fetch updates from GitHub
if (interactive() && file.exists(".git")) {
  options(ask_git_pull = TRUE)
}

setHook("rstudio.sessionInit", function(...) {
  if (getOption("ask_git_pull", FALSE)) {
    message("Checking for remote changes...")
    system("git fetch")
    local_status <- system("git status -uno", intern = TRUE)
    
    if (any(grepl("Your branch is behind", local_status))) {
      response <- readline("CAUTION!!! Your local branch is behind GitHub. Do you want to pull the latest changes? (y/n): ")
      if (tolower(response) == "y") {
        system("git pull")
      } else {
        message("Skipped git pull.")
      }
    } else {
      message("Up to date.")
    }
  }
}, action = "append")


# confirm reading .Rprofile
print("Rprofile is running")
