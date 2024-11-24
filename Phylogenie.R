# Installation des packages requis
required_packages <- c("rentrez", "msa", "phangorn", "Biostrings", "ggtree", "ape")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# Charger les packages
library(rentrez)
library(msa)
library(phangorn)
library(Biostrings)
library(ggtree)
library(ape)

# Fonction pour télécharger les séquences H5N1
download_h5n1_sequences <- function(query, max_results = 20, output_file = "sequences_h5n1.fasta") {
  cat("Recherche des séquences pour la requête :", query, "\n")
  search_results <- entrez_search(db = "nuccore", term = query, retmax = max_results)
  
  if (length(search_results$ids) == 0) {
    stop("Aucune séquence trouvée pour la requête donnée.")
  }
  
  cat("Nombre de séquences trouvées :", search_results$count, "\n")
  uids <- search_results$ids
  cat("Téléchargement des séquences...\n")
  
  fasta_sequences <- entrez_fetch(db = "nuccore", id = uids, rettype = "fasta", retmode = "text")
  write(fasta_sequences, file = output_file)
  cat("Séquences sauvegardées dans :", output_file, "\n")
  
  return(output_file)
}

# Téléchargement des séquences H5N1
query <- "H5N1[Organism] AND (complete genome[Title] OR segment[Title])"
fasta_file <- download_h5n1_sequences(query = query, max_results = 20)

# Fonction pour effectuer l'alignement multiple
align_sequences <- function(fasta_file, alignment_file = "aligned_sequences.fasta") {
  cat("Chargement des séquences depuis :", fasta_file, "\n")
  sequences <- readDNAStringSet(fasta_file, format = "fasta")
  
  cat("Réalisation de l'alignement multiple avec MUSCLE...\n")
  alignment <- msa(sequences, method = "Muscle")
  
  cat("Sauvegarde de l'alignement dans :", alignment_file, "\n")
  writeXStringSet(as(alignment, "XStringSet"), filepath = alignment_file, format = "fasta")
  
  return(alignment)
}

# Alignement des séquences
alignment <- align_sequences(fasta_file)

# Fonction pour construire un arbre phylogénétique
build_phylogenetic_tree <- function(alignment) {
  cat("Conversion de l'alignement en format utilisable pour phangorn...\n")
  dna_alignment <- as.matrix(as.DNAbin(alignment))
  
  # Calcul de la matrice de distance
  cat("Calcul de la matrice de distance...\n")
  distance_matrix <- dist.dna(dna_alignment)
  
  # Construction d'un arbre de départ (NJ)
  cat("Construction d'un arbre initial par Neighbour-Joining...\n")
  nj_tree <- nj(distance_matrix)
  
  # Méthode de maximum de vraisemblance
  cat("Optimisation de l'arbre par maximum de vraisemblance...\n")
  fit <- pml(nj_tree, data = phyDat(dna_alignment, type = "DNA"))
  optimized_tree <- optim.pml(fit, model = "HKY", rearrangement = "stochastic")
  
  return(optimized_tree$tree)
}

# Construction de l'arbre phylogénétique
phylo_tree <- build_phylogenetic_tree(alignment)

# Visualisation de l'arbre avec ggtree
visualize_tree <- function(tree, output_image = "phylogenetic_tree.png") {
  cat("Génération de la visualisation de l'arbre...\n")
  tree_plot <- ggtree(tree) + 
    geom_tiplab() +
    theme_tree2() +
    ggtitle("Arbre phylogénétique des variants H5N1")
  
  ggsave(output_image, tree_plot, width = 10, height = 8)
  cat("Arbre phylogénétique sauvegardé dans :", output_image, "\n")
}
# Installer ggplot2 si ce n'est pas encore fait
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

# Charger ggplot2
library(ggplot2)

# Visualiser l'arbre
visualize_tree(phylo_tree)

