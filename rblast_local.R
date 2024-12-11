run_rblast <-function(fasta_file, bdd_rblastlocal){

# Vérifie si le fasta a comparer est chargé
if (!file.exists(fasta_file)) {
    stop("Le fichier FASTA spécifié n'existe pas.")
}

seq <- Biostrings::readDNAStringSet(fasta_file) # Stocke les séquences sans les identifiants

bl <- blast(db = bdd_rblastlocal) # Charge la base de données
print(bl)
cl <- predict(bl, seq, BLAST_args = "-perc_identity 70") # Stocke les résultats

return(cl)
}
