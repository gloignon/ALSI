# ALSI Demo: Reading Corpora with Different Encodings
#
# Many French corpora come in non-UTF-8 encodings (Latin-1, Windows-1252).
# This demo shows how build_corpus() handles them gracefully.
#
# Run each section one at a time to see what happens.
# This might be particularly relevant for you if you're having error messages due
# to how your text files are encoded.
#
# Last update: 2026-03-26
#

source('R/fnt_corpus.R', encoding = 'UTF-8')

corpus_dir <- 'demo_corpus/'


# 1) Standard UTF-8 read -----
# Most modern files are UTF-8. No special options needed.

dt_utf8 <- build_corpus(corpus_dir)
cat("UTF-8 corpus:", nrow(dt_utf8), "documents\n")
cat(substr(dt_utf8$text[1], 1, 200), "...\n\n")


# 2) Latin-1 (ISO-8859-1) -----
# Older French corpora (pre-2010, institutional sources) are often in Latin-1.
# To demo this, we take one of our UTF-8 files and re-save it as Latin-1.

src_file <- list.files(corpus_dir, full.names = TRUE)[1]
txt_utf8 <- readr::read_file(src_file)

tmp_latin1 <- file.path(tempdir(), "legacy_latin1.txt")
writeBin(iconv(txt_utf8, from = "UTF-8", to = "latin1", sub = "byte"), tmp_latin1)
message("Source file: ", basename(src_file), " re-saved as Latin-1")

# If you try to read a Latin-1 file without telling build_corpus,
# it will error because Latin-1 bytes are not valid UTF-8.
# The error message will suggest the correct encoding to use.
# (Uncomment the next line to see the error yourself.)

# build_corpus(tmp_latin1)

# To fix it, just tell build_corpus which encoding to use:
dt_latin1 <- build_corpus(tmp_latin1, encoding = "latin1")
cat("Latin-1 read OK:", substr(dt_latin1$text, 1, 120), "...\n")

unlink(tmp_latin1)


# 3) Windows-1252 (CP1252) -----
# Very common for French text exported from Word, older web scrapes, or
# institutional databases. It is a superset of Latin-1 that adds characters
# like: oe ligature (oeuvre), euro sign (€), curly quotes (""), em-dashes (—).
#
# If you use encoding = "latin1" on a CP1252 file, accents will be fine but
# those extra characters (oe, €, etc.) will be silently garbled.

tmp_cp1252 <- file.path(tempdir(), "legacy_cp1252.txt")
writeBin(iconv(txt_utf8, from = "UTF-8", to = "windows-1252", sub = "byte"), tmp_cp1252)

dt_cp1252 <- build_corpus(tmp_cp1252, encoding = "windows-1252")
cat("CP1252 read OK:", substr(dt_cp1252$text, 1, 120), "...\n")

unlink(tmp_cp1252)


# 4) Auto-detection -----
# If you don't know the encoding, use encoding = "auto".
# build_corpus will sniff each file and pick the best match.

tmp_auto <- file.path(tempdir(), "mystery_encoding.txt")
writeBin(iconv(txt_utf8, from = "UTF-8", to = "latin1", sub = "byte"), tmp_auto)

# build_corpus detects the encoding automatically and tells you what it found:
dt_auto <- build_corpus(tmp_auto, encoding = "auto")
cat("Auto-detected read OK:", substr(dt_auto$text, 1, 120), "...\n")

unlink(tmp_auto)

# Auto-detection also works on whole directories.
# Here we convert the entire demo corpus to CP1252:
tmp_dir <- file.path(tempdir(), "legacy_corpus")
dir.create(tmp_dir, showWarnings = FALSE)
for (f in list.files(corpus_dir, full.names = TRUE)) {
  txt <- readr::read_file(f)
  writeBin(iconv(txt, from = "UTF-8", to = "windows-1252", sub = "byte"),
           file.path(tmp_dir, basename(f)))
}

dt_auto_dir <- build_corpus(tmp_dir, encoding = "auto")
cat("Auto-detected directory:", nrow(dt_auto_dir), "documents read OK\n")

unlink(tmp_dir, recursive = TRUE)


# 5) Mixed encodings in one directory -----
# Sometimes a folder contains files saved in different encodings (e.g. some
# in Latin-1, others in UTF-8). With encoding = "auto", build_corpus detects
# each file individually and warns you that encodings are mixed.

tmp_mixed <- file.path(tempdir(), "mixed_corpus")
dir.create(tmp_mixed, showWarnings = FALSE)

# One file in Latin-1, another stays UTF-8
writeBin(iconv(txt_utf8, from = "UTF-8", to = "latin1", sub = "byte"),
         file.path(tmp_mixed, "legacy.txt"))
writeLines(txt_utf8, file.path(tmp_mixed, "modern.txt"))

# You will see a warning about mixed encodings, but all files are read correctly:
dt_mixed <- build_corpus(tmp_mixed, encoding = "auto")
cat("Mixed directory:", nrow(dt_mixed), "documents read OK\n")

unlink(tmp_mixed, recursive = TRUE)
