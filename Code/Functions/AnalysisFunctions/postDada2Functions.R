# Post-DADA2 Processing Functions



# CLR Matrix -------------------------------------------------------
#   Description: 
#   Input: OTU matrix
#   Output: CLR matrix

gen.clr.matrix <- function(
    asv.mat,
    min_reads,
    min_prop = 0.001,
    min_occur = 0,
    smpls_by_row = TRUE,
    method = "CZM",
    lab = 0
) {
  require(CoDaSeq)
  require(zCompositions)
  asv.mat.f <- codaSeq.filter(
    asv.mat,
    min.reads = min_reads,
    min.prop = min_prop,
    min.occurrence = min_occur,
    samples.by.row = smpls_by_row
  )
  # replace 0 values with an estimate
  asv.mat.fn0 <- cmultRepl(t(asv.mat.f), method = method, label = lab)
  return(codaSeq.clr(asv.mat.fn0))
}



# Phyloseq Refactor Levels -------------------------------------------------------
#   Description: refactors levels within a phyloseq metadata object
#   Input: phyloseq object
#   Output: phyloseq object


ps_refactor_lvls <- function(physeq = ps0){
  
  levels(sample_data(physeq)$Treatment) <- factor(levels(sample_data(physeq)$Treatment), levels = c("Control", "Dose_1", "Dose_2"))   # Not sure why unique() is needed here
  # levels(sample_data(physeq)$Treatment)
  
  levels(sample_data(physeq)$Timepoint) <- factor(levels(sample_data(physeq)$Timepoint), levels = c("3mpf", "6mpf"))
  # levels(sample_data(physeq)$Timepoint)
  
  levels(sample_data(physeq)$Exposure) <- factor(levels(sample_data(physeq)$Exposure), levels = c("Unexposed", "Exposed"))
  # levels(sample_data(physeq)$Exposure)
  
  levels(sample_data(physeq)$PrePostExp) <- factor(levels(sample_data(physeq)$PrePostExp), levels = c("Pre-exposure", "Unexposed", "Exposed"))
  # levels(sample_data(physeq)$PrePostExp)
  
  levels(sample_data(physeq)$Sex) <- factor(levels(sample_data(physeq)$Sex), levels = c("Female", "Male"))
  # levels(sample_data(physeq)$Sex)
  
  return(physeq)
}



# CLR Matrix --------------------------------------------------------------
#   Description: creates a Center Log Ratio matrix
#   Input: 
#   Output: 

gen.clr.matrix <- function(
    asv.mat,
    min_reads,
    min_prop = 0.001,
    min_occur = 0,
    smpls_by_row = TRUE,
    method = "CZM",
    lab = 0
) {
  require(CoDaSeq)
  require(zCompositions)
  asv.mat.f <- codaSeq.filter(
    asv.mat,
    min.reads = min_reads,
    min.prop = min_prop,
    min.occurrence = min_occur,
    samples.by.row = smpls_by_row
  )
  # replace 0 values with an estimate
  asv.mat.fn0 <- cmultRepl(t(asv.mat.f), method = method, label = lab)
  return(codaSeq.clr(asv.mat.fn0))
}
