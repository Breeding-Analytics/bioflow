# Bioflow Genlight Implementation Changes

This document summarizes the key changes made to the code in the Bioflow Genlight implementation. The modifications affect the organization of data, metadata, and the logging of filtering modifications.

---

## Data Slot

### `result$data$geno`
- **Description:**
  - This slot is now populated with the output of the `get_geno_data()` reactive expression.
  - Instead of an allelic dosage dataframe, it returns a `genlight` object.
- **Rationale:**
  - Using a `genlight` instance (from the **adegenet** package) streamlines downstream analyses by leveraging specialized data structures for genetic data.

### `result$data$geno_imp`
- **Description:**
  - This slot is a **named list**.
  - Each element is indexed by the float conversion of a timestamp (i.e., `AnalysisID`).
  - Each index holds a `genlight` object that has been filtered and imputed.
- **Rationale:**
  - Storing imputed objects separately allows for efficient tracking and re-use of modified datasets.

---

## Metadata Slot

### `results$metadata$geno`
- **Previous Implementation:**
  - Contained physical positions and the REF/ALT alleles for each locus.
- **Updated Implementation:**
  - The `genlight` object already contains the necessary locus information, making duplicate storage redundant.
  - Now, this slot holds a dataframe with two columns: `[parameter, value]`.
- **Mandatory Parameters:**
  - **`input_format`**: Specifies the input file format. Accepted values are `hapmap`, `vcf`, `dartseqsnp`, or `dartag`.
  - **`ploidity`**: An integer indicating the organism’s ploidy level.
- **Additional Parameters (for `dartseq` input):**
  - **`dartseq_marker_id`** (string)
  - **`dartseq_chr`** (string)
  - **`dartseq_pos`** (string)
- **Rationale:**
  - Consolidating metadata into a simple dataframe avoids redundancy.
  - This approach provides a clear, concise summary of the key parameters associated with the dataset.

---

## Modifications Slot

### `result$modifications$geno`
- **Description:**
  - This slot is a dataframe designed to record the modifications made during the filtering process.
  - It logs changes made to both rows (individuals) and columns (loci/markers).
- **Dataframe Columns:**
  - **`reason`**: A concatenation of `metric_name`, `logical_operator`, and `threshold` that triggered the removal.
  - **`row`**: The index of the affected individual. (If `NA`, the change applies to a column.)
  - **`col`**: The index of the affected locus. (If `NA`, the change applies to a row.)
  - **`analysisId`**: A unique identifier for the analysis step.
  - **`analysisName`**: A descriptive name for the analysis step.
  - **`module`**: Indicates the module responsible (e.g., `[qageno]`).

### `results$modifications$geno_imp`
- **Description:**
  - This slot is a named list where each element is identified by the `analysisId` (casted to a float).
  - Within each element, there is a named dictionary containing `comb(2, ploidity)` items. For example, for diploid data (ploidity = 2), there are three possible genotype calls: `0`, `1`, and `2`.
  - Each dictionary entry stores the planar index of a missing value that was imputed with the corresponding genotype call.

---

## Summary

- **Data Slot Enhancements:**  
  Transitioning to a `genlight` object for genotype data and maintaining a separate, dynamically keyed list for imputed data streamlines data handling and improves clarity.

- **Metadata Slot Enhancements:**  
  Removing redundant storage by leveraging the inherent properties of the `genlight` object, while still capturing essential parameters in a clear dataframe, simplifies metadata management.

- **Modifications Slot Enhancements:**  
  Detailed logging of both filtering and imputation steps via well-structured dataframes and named lists enhances traceability, reproducibility, and debugging capabilities.

These changes collectively improve the robustness, clarity, and maintainability of the Bioflow Genlight code implementation.
