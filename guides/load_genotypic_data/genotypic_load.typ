#import "@preview/fontawesome:0.1.0": *

// Document setup
#set document(
  title: "Application Guide",
  author: "Your Name",
  date: auto,
)

// Page setup with margins
#set page(
  paper: "a4",
  margin: (top: 5cm, bottom: 2.5cm, left: 2cm, right: 2cm),
  header: [
    #set align(right)
    #image("./assets/img/bioflowTransBg.png", width: 20%)
    #line(length: 100%, stroke: 0.5pt + gray)
  ],
  numbering: "1"
)

// Text styling
#set text(
  font: "Calibri",
  size: 11pt,
  lang: "en",
)

// Paragraph spacing
#set par(spacing: 1.1em)

// Heading styles
#show heading: it => {
  set text(weight: "bold")
  it
  v(0.3em)
}

#show heading.where(level: 1): it => {
  set text(size: 18pt, fill: rgb("#1f4788"))
  v(0.5em)
  it
  v(0.3em)
}

#show heading.where(level: 2): it => {
  set text(size: 14pt, fill: rgb("#2e5c8a"))
  v(0.3em)
  it
  v(0.2em)
}

#show heading.where(level: 3): it => {
  set text(size: 12pt, fill: rgb("#4a7ba7"))
  it
  v(0.1em)
}

// Link styling
#show link: it => {
  set text(fill: rgb("#0066cc"), weight: "semibold")
  it
}

// List styling
#set list(indent: 1.5em, body-indent: 0.5em)
#set enum(indent: 1.5em, body-indent: 0.5em)

// ============================================
// Title Page
// ============================================

#align(center)[
  #v(4em)
  #text(size: 18pt, weight: "bold", fill: rgb("#1f4788"))[
    Genotypic Load Application Guide
  ]
  #v(2em)
  #text(size: 12pt, fill: gray)[
    A comprehensive guide to using the genotyping load application for managing and analyzing genotypic data.
  ]
  #v(4em)
  #text(size: 12pt)[
    Version 1.0 \
    #datetime.today().display("[month repr:long] [day], [year]")
  ]
]

#pagebreak()

// ============================================
// Table of Contents
// ============================================

#outline(
  title: [Table of Contents],
  depth: 2,
  indent: 1em,
)

#pagebreak()

// ============================================
// Introduction
// ============================================

= Introduction

Bioflow application support the loading of genotypic data of only *biallelic SNP markers*. This information is used for other Bioflow modules for the calculation of genetic parameters, the estimation of genomic relationships, validate hybrid combinations, and other genetic analyses. The genotypic data can be loaded in different formats depending on the ploidy level of the organism.

Independently of the format, is mandatory that each makrer has a *unique identifier* and specify the *reference and alternative alleles*. If physical positions are not provided the application will assign them sequentially.

= DArTSeq SNP files

Bioflow supports the loading of genotypic data from DArTSeq SNP files, which are commonly used in genotyping studies. DartR package is used in the backend to read this format using the function `dartR.base::gl.read.dart(path)`. In the CgiarGenomics repository you can find example DArTSeq SNP file #link("https://raw.githubusercontent.com/Breeding-Analytics/cgiarGenomics/refs/heads/main/tests/DartSeq_fmt/DartSeq/DArTSeq_CB_snp.csv")[`test/dartseq/DartSeq_fmt/DartSeq/DArTSeq_CB_snp.csv`]  that can be used for testing the application.

In the dropdown menu select the option DarTSeq SNP and then specify the path to the DArTSeq SNP file. DarTSeq SNP format only supports diploid data, so make sure that the file contains genotypic data for diploid organisms. The file should include the columns *AlleleID*, *Chromosome*, *Position*. Normally the vendor provides those columns mapped to a reference genome. In @dartseq_params you can see the parameters used to load the example file.

#figure(
  image("assets/img/dart_seq_bioflow_params.png", width: 100%),
  caption:[ Parameters to load DArTSeq SNP files in the genotype loading module.]
)<dartseq_params>

Once the file is loaded, you can review the genotypic data in the table and proceed to filter and use it for downstream analyses in other Bioflow modules (see @dartseq_success).

#figure(
  image("assets/img/dart_seq_bioflow_success.png", width: 100%),
  caption:[ Summary table with marker counts by chromosome after successfully loading a DArTSeq SNP file in the genotype loading module.]
)<dartseq_success>

= DArTag files

Bioflow also supports the loading of genotypic data from DArTag files. Vendor usually provides two files: the allelic dosages and read counts for each marker and sample. In CgiarGenomics repository you can find example DArTag files #link("https://raw.githubusercontent.com/Breeding-Analytics/cgiarGenomics/refs/heads/main/tests/DartSeq_fmt/DartTag/DArTag_CB_snp.csvhttps://raw.githubusercontent.com/Breeding-Analytics/cgiarGenomics/refs/heads/main/tests/DartSeq_fmt/DArT_TAG_sweet_potato/DP23-8340_Allele_Dose_Report.csv")[`tests/DartSeq_fmt/DArT_TAG_sweet_potato/DP23-8340_Allele_Dose_Report.csv`] and #link("https://raw.githubusercontent.com/Breeding-Analytics/cgiarGenomics/refs/heads/main/tests/DartSeq_fmt/DartTag/DArTag_CB_snp.csvhttps://raw.githubusercontent.com/Breeding-Analytics/cgiarGenomics/refs/heads/main/tests/DartSeq_fmt/DArT_TAG_sweet_potato/DP23-8340_Allele_match_counts_collapsed.csv")[`tests/DartSeq_fmt/DArT_TAG_sweet_potato/DP23-8340_Allele_match_counts_collapsed.csv`]. Select the option DarTag SNP and upload the counts and dosage files. In the example data the organism is tetraploid, so make sure that you specify the ploidy level of the organism in the parameters (see @dartag_params). 


#figure(
  image("assets/img/dart_tag_bioflow_params.png", width: 100%),
  caption:[ Parameters to load DArTag SNP files in the genotype loading module.]
)<dartag_params>

Once the file is loaded, you can review the genotypic data in the table and proceed to filter and use it for downstream analyses in other Bioflow modules (see @dartag_success).

#figure(
  image("assets/img/dart_tag_bioflow_success.png", width: 100%),
  caption:[ Summary table with marker counts by chromosome after successfully loading a DArTag file in the genotype loading module.]
)<dartag_success>



== Change Log

*Version 1.0* - Initial release

#pagebreak()

// ============================================
// Contact and Support
// ============================================

= Contact and Support

For additional help and support, please reach out to us in Bioflow issues section #link("https://github.com/Breeding-Analytics/bioflow/issues", [Bioflow GitHub repository])

We appreciate your feedback and suggestions for improving this guide and the application!
