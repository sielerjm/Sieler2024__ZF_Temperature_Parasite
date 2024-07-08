# dada2_processing.R

### LOCAL


# -------------------------------------------------------------------------



# Check project file location is in correct directory
## Path to project directory (Rproject file should be stored in "~/Code")
if(grepl("/Code", getwd())){
    proj.path <- getwd()
} else(
    # Script will stop if project file is not located in ~/Code directory
    stop("Project file (`.Rproj`) is not located in `~/Code` directory. 
       \t**Exiting script."))

# Helper Scripts

## Load libraries
source(paste0(proj.path,"/HelperScripts/libraries.R"))

## Load functions
source(paste0(proj.path,"/HelperScripts/functions_2.R"))

## Load custom plot settings
source(paste0(proj.path,"/HelperScripts/plot_settings.R"))

# Set important paths

## Path to data files/sub-folders
data.path <- check_proj_struct(proj.path,  # Project path
                               "/../Data")  # Sub-directory

## Path to Robjects (Renv, Rds, Rdata, etc.) saved here
objects.path <- check_proj_struct(data.path,  # Project path
                                  "/Clean/Robjects")  # Sub-directory

## Path to Output (Figures, Tables)
output.path <- check_proj_struct(proj.path,  # Project path
                                 "/../Output")  # Sub-directory

## Path to Output (Figures, Tables)
inDir <- check_proj_struct(data.path,  # Data path
                           "/Clean/Input")  # Sub-directory


# Location of FastTree installation
local.symlinks <- "/Users/michaelsieler/Dropbox/Mac (2)/Documents/Sharpton_Lab/Bioinformatics_files/Symlinks"

# -------------------------------------------------------------------------


# Contains Sample and Barcode columns
file.ids.file <- "sample_file_IDs.xlsx"

# Contains metadata information, sample, treatments, etc.
metadata.file <- "metadata.csv"

# Needs to match sample column in metadata
smpl.col <- "Sample"  # not sure why this needs to be here, since repeated below

# Read in file IDs xlsx ("Sample", "Barcode")
file.ids.dt <- read_excel(file.path(inDir, file.ids.file)) %>%
  as.data.table()
names(file.ids.dt)[1] <- "Sample"
# file.ids.dt[, Sample := ""]  # What does this do?


# Read in metadata file
sample.dt <- read.csv(file.path(inDir, metadata.file)) %>%
  as.data.table() #%>%
  #subset(Dissect_result != "no gut")

# Rename sample name column
# names(sample.dt)[which(names(sample.dt) == "Sample")] <- "Sample" # If col name is not sample, this rename it
# sample.dt[, Sample := ""]  # Not sure what this does

## Combine data tables (Can skip this section if metadata has barcode column)
# Combines barcode file with metadata if not already combined, aka metada doesn't have barcode column
# combined.dt <- sample.dt[file.ids.dt, on = "Sample"]

# Checks for duplicates
dupes <- table(combined.dt$Sample) %>% subset(. > 1) %>% names()  # if no duplicates it will say "error"
matched.dupes <- data.table(Dupe = dupes, Matches = 0) %>% setkey(Dupe)  # Matches duplicates if there are some, skip if you didn't have any in previous line


for (i in 1:nrow(combined.dt)) {
  if (combined.dt$Sample[i] %in% dupes) {
    matched.dupes[combined.dt$Sample[i]]$Matches %+=% 1
    combined.dt$Sample[i] <- paste0(combined.dt$Sample[i], "_", matched.dupes[combined.dt$Sample[i]]$Matches)
  } else {
    combined.dt$Sample[i] <- combined.dt$Sample[i]
  }
}
table(combined.dt$Sample) %>% subset(. > 1) %>% names()

# Save RDS obj with file ids
# saveRDS(combined.dt, file = file.path(inDir, "metadata_withFileIDs.rds"))
saveRDS(sample.dt, file = file.path(inDir, "metadata_withFileIDs.rds"))  # Export RDS obj



# -------------------------------------------------------------------------
### DARWIN


###### Definitely assign/change these variables ######
raw.seq.dirs <- c(
  Run1 = paste0(data.path, "/Raw/FastQs") #"/Users/michaelsieler/Dropbox/Mac (2)/Documents/Sharpton_Lab/Projects_Repository/ZF_Interstrain/Data/Raw/Sequences_Raw"  # Change to your directory
  # Run2 = "/nfs2/hts/miseq/210614_M01498_0803_000000000-JR55H/L1"
)
sample.ids.filename <-  "metadata_withFileIDs.rds" # for properly grabbing sequence files
                           # this *may* be the same file as `metadata.filename` below
file.ids.col <- "Barcode"
metadata.filename <- "metadata_withFileIDs.rds" # sample metadata to be used in endpoint phyloseq object
                         # this *may be the same file as `sample.ids.filename` above
taxa.db.path <- "/Users/michaelsieler/Dropbox/Mac (2)/Documents/Sharpton_Lab/Bioinformatics_files/silva_nr99_v138.1_train_set.fa" # path to database to be used for assigning taxonomy. Should be a gzipped FASTA file.
fasttree.path <- paste0(local.symlinks, "/FastTree") # path to the FastTree executable
cleanup <- TRUE
cores <- 9

###### Only change these variables for a specific reason ######
split <- "--"

# inDir <- "Input" # Repeated from above
smpl.col <- "Sample" # Repeated from above

sample.ids.dt <- dada2.pipeline::read.file(file.path(inDir, sample.ids.filename))
if (!("data.table" %in% sample.ids.dt)) {
  sample.ids.dt <- as.data.table(sample.ids.dt) %>%
    setkeyv(smpl.col)
}


orig.dir <- getwd()

fastq.dir <- file.path(inDir, "FastQs")

setDTthreads(threads = cores)
if (!dir.exists(fastq.dir)) {dir.create(fastq.dir)}

###### Begin processsing ######

# Start script timer
print("Starting script.")
start_time <- Sys.time()


initiate.pipeline()
setwd(fastq.dir)

###### Symlink FASTQs ######
symlink_fastqs_local(
    seq.dir = raw.seq.dirs,
    ids.tbl = sample.ids.dt,
    file.id.col = file.ids.col,
    split.pattern = split
  )

setwd(orig.dir)

###### Run dada2 up to quality plots ######
dada2.upto.qualPlots(
  fastq.path = fastq.dir,
  file.split.pattern = split,
  maxCores = cores,
  random.seed = 33 # seed 42 gave an error from the function `plotQualityProfile`, no idea why
)

### If working on a cluster you may need to move PDFs to a local machine, which may be faster if they're in your home directory
### If so, uncomment below
# system(paste0("cp -v dada2*", Sys.Date(), "_output/*.pdf ~/temp"))

### Look at quality PDFs
### Quality of reads looks great for some, not so great for others. Going to proceed and see just how much we get out of this

### Proceeding with paired reads # (forward only or both)

###### Finish dada2 processing ######
dada2.finish_local(
  fastq.path = fastq.dir,
  truncFwd.len = 225,
  truncRev.len = 200,
  taxa.db = taxa.db.path,
  metadata.file = file.path(inDir, metadata.filename),
  paired = TRUE,
  maxCores = cores,
  build.tree = TRUE,
  fasttree.path = fasttree.path
  # user.output.path = output.path
)

###### Move files to proper directories ######
file.copy(
  from = file.path(run.env$output, "phyloseq.rds"),
  to = file.path(inDir, "phyloseq.rds"),
  overwrite = TRUE
)

###### Compress and archive output, remove unneeded files ######
if (cleanup) {
  system(paste0("tar zvcf ", output, ".tgz ", output))
  system(paste("rm -r", output))
  system(paste("rm -r", fastq.dir))
}


# Save Environment
save_env(objects.path, extra_info = "_DADA2_pipeline_output")

# End Timer ---------------------------------------------------------------
end_time <- Sys.time()
print(paste0("Total runtime of script was: ", end_time - start_time))
