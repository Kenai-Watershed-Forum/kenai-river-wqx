# fetch updates from GitHub
if (interactive() && file.exists(".git")) {
  message("Checking for remote changes...")
  system("git fetch") # Fetch latest changes
  local_status <- system("git status -uno", intern = TRUE)
  
  if (any(grepl("Your branch is behind", local_status))) {
    response <- readline("Your branch is behind. Do you want to pull the latest changes? (y/n): ")
    if (tolower(response) == "y") {
      system("git pull")
    } else {
      message("Skipped git pull.")
    }
  } else {
    message("Up to date.")
  }
}

print("Rprofile is running")
