run_rblast <-function(fasta_file, bdd_rblastlocal){

if (!file.exists(fasta_file)) {
    stop("Le fichier FASTA spécifié n'existe pas.")
}

seq <- Biostrings::readDNAStringSet(fasta_file)

bl <- blast(db = bdd_rblastlocal)
print(bl)
cl <- predict(bl, seq, BLAST_args = "-perc_identity 70")

return(cl)
}