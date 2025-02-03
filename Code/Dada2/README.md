# Dada2

This folder contains files related to the DADA2 pipeline for microbiome analysis. The main components include:

## Key Files
- **PostDADA2Cleaning.Rmd**: R Markdown file for post-DADA2 processing and cleaning of the phyloseq object
- **dada2_1.26.0_2022-11-15_output/**: Directory containing DADA2 pipeline output and debugging files
  - **Debug_dada2finish.R**: Debugging script for the DADA2 finishing steps
  - **Debug_symlinkfastqs.Rmd**: R Markdown for debugging fastq file symlinking
  - **asv_NASTaligned_seqs.nwk**: Phylogenetic tree file in Newick format

## Processing Steps
1. **Sequence Processing**:
   - Quality filtering and trimming
   - Error rate learning
   - Sample inference
   - Sequence table construction
   - Chimera removal

2. **Taxonomic Assignment**:
   - Using reference databases
   - ASV sequence generation

3. **Phylogenetic Tree Building**:
   - Sequence alignment
   - Tree construction using FastTree

4. **Post-Processing**:
   - Sample filtering (min. 5000 reads)
   - Taxonomic filtering (min. 3 samples)
   - Removal of unwanted taxa (Mitochondria, Chloroplast, Eukaryota)
   - Metadata cleaning and transformation

## Outputs
- Filtered and cleaned phyloseq object
- ASV sequence table
- Taxonomic assignments
- Phylogenetic tree
- Process tracking statistics

## Dependencies
- DADA2
- phyloseq
- microViz
- tidyverse