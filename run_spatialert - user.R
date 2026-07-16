# ============================================================
# spatiAlert --> Run this file to launch the app
# ============================================================
# HOW TO USE THIS FILE
#
# 1. Set the 'dir' variable below to the folder ON YOUR COMPUTER that
#    directly contains the "spatialert_package" folder.
#
#    Example: if you unzipped things so that you have
#        C:/Users/yourname/Documents/spatialert_package/spatialert/...
#    then 'dir' should be:
#        C:/Users/yourname/Documents
#
#    (Not ".../spatialert_package" itself, and not ".../spatialert_package/spatialert"
#    — one level ABOVE "spatialert_package".)
#
# 2. Click "Source" (or press Ctrl+Shift+S / Cmd+Shift+S).
# 3. The app should open in your browser automatically.
#
# If you get an error, it will tell you specifically what looks wrong with
# the path you set — read the error message, it's meant to help you fix it
# in one try.

dir <- "C:/Users/yourname/path/to/folder/containing/spatialert_package"

# ============================================================
# Do not edit below this line
# ============================================================

if (!nzchar(dir) || dir == "C:/Users/yourname/path/to/folder/containing/spatialert_package") {
  stop(
    "You still need to set the 'dir' variable above to a real path.\n",
    "It should point to the folder that directly contains 'spatialert_package' ",
    "(see the instructions in the comments at the top of this file)."
  )
}

pkg_container <- file.path(dir, "spatialert_package")
app_base       <- file.path(pkg_container, "spatialert")

if (!dir.exists(app_base)) {

  # Try to give a specific, actionable diagnosis rather than a generic error.
  if (file.exists(file.path(dir, "DESCRIPTION"))) {
    stop(
      "It looks like 'dir' is pointing directly at the 'spatialert' package folder itself.\n",
      "Set 'dir' one level higher instead \u2014 to the folder that CONTAINS 'spatialert_package'."
    )
  } else if (basename(dir) == "spatialert_package") {
    stop(
      "It looks like 'dir' is pointing directly at 'spatialert_package'.\n",
      "Set 'dir' one level higher instead \u2014 to the folder that CONTAINS 'spatialert_package', ",
      "e.g. if 'spatialert_package' is at:\n  ", dir, "\nthen 'dir' should be:\n  ", dirname(dir)
    )
  } else if (dir.exists(pkg_container) && !dir.exists(app_base)) {
    stop(
      "Found 'spatialert_package' at:\n  ", pkg_container,
      "\nbut no 'spatialert' folder inside it. The package folder may have been ",
      "renamed, moved, or only partially unzipped. Try re-unzipping the original ",
      "download and pointing 'dir' at its parent folder without renaming anything."
    )
  } else if (!dir.exists(dir)) {
    stop(
      "The folder set in 'dir' does not exist:\n  ", dir,
      "\nDouble-check the path (copy it directly from File Explorer / Finder ",
      "to avoid typos)."
    )
  } else {
    stop(
      "Could not find 'spatialert_package' inside:\n  ", dir,
      "\nMake sure 'dir' points to the folder that directly contains 'spatialert_package', ",
      "and that you haven't renamed that folder."
    )
  }
}

shiny::runApp(file.path(app_base, "inst/app"))
