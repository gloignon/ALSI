# ALSI Demo: Reading Corpora with Different Encodings
#
# Many French corpora come in non-UTF-8 encodings (Latin-1, Windows-1252).
# This demo shows how build_corpus() handles them gracefully.
#
# An "encoding" tells the computer how to interpret the bytes in a text file.
# UTF-8 is the modern standard (it can represent any character in any language).
# Older French corpora were often saved in Latin-1 or Windows-1252, which can
# only represent Western European characters but were the default in Windows
# and older word processors. If you get errors like "invalid multibyte string"
# when reading a file, an encoding mismatch is almost certainly the cause.
#
# Run each section one at a time to see what happens.
#
# Prerequisites:
#   - demo_corpus/   — any folder of .txt files
#
# Last update: 2026-03-26

library(readr)  # for read_file() and guess_encoding()

source("R/fnt_corpus.R", encoding = "UTF-8")

# Adjust this path to point at a folder containing at least one .txt file.
corpus_dir <- "demo_corpora/viki_wiki/"


# 1) Standard UTF-8 read ----
# Most modern files are UTF-8. No special options needed.
# build_corpus() returns a data.table with columns: doc_id (filename without
# extension) and text (the full document content as a single string).

dt_utf8 <- build_corpus(corpus_dir)
message(nrow(dt_utf8), " documents loaded")
print(substr(dt_utf8$text[1], 1, 200))  # preview the first 200 characters


# 2) Latin-1 (ISO-8859-1) ----
# Older French corpora (pre-2010, institutional sources) are often in Latin-1.
# To illustrate the problem, we take one of our UTF-8 files and re-save it
# in Latin-1, then try to read it.

src_file <- list.files(corpus_dir, full.names = TRUE)[1]
txt_utf8 <- readr::read_file(src_file)

# Write a Latin-1 version to a temporary file.
# writeBin + iconv is the standard R way to write non-UTF-8 bytes to disk.
tmp_latin1 <- file.path(tempdir(), "legacy_latin1.txt")
writeBin(iconv(txt_utf8, from = "UTF-8", to = "latin1", sub = "byte"), tmp_latin1)
message("Re-saved as Latin-1: ", tmp_latin1)

# Trying to read a Latin-1 file without telling build_corpus will error,
# because the Latin-1 bytes are not valid UTF-8.
# (Uncomment the line below to see the error message.)
# build_corpus(tmp_latin1)

# The fix is to pass encoding = "latin1":
dt_latin1 <- build_corpus(tmp_latin1, encoding = "latin1")
message("Latin-1 read OK — first 120 chars:")
print(substr(dt_latin1$text, 1, 120))

unlink(tmp_latin1)  # clean up temp file


# 3) Windows-1252 (CP1252) ----
# Very common for French text exported from Word, older web scrapes, or
# institutional databases. CP1252 is a superset of Latin-1 — it adds
# characters like the oe ligature (œ), euro sign (€), and em-dash (—).
#
# If you use encoding = "latin1" on a CP1252 file, standard accented characters
# (é, è, à, …) will be fine, but those extra CP1252-only characters will be
# silently garbled. Use encoding = "windows-1252" to be safe.

tmp_cp1252 <- file.path(tempdir(), "legacy_cp1252.txt")
writeBin(iconv(txt_utf8, from = "UTF-8", to = "windows-1252", sub = "byte"), tmp_cp1252)

dt_cp1252 <- build_corpus(tmp_cp1252, encoding = "windows-1252")
message("CP1252 read OK — first 120 chars:")
print(substr(dt_cp1252$text, 1, 120))

unlink(tmp_cp1252)


# 4) Auto-detection (single file) ----
# If you don't know the encoding, pass encoding = "auto".
# build_corpus() uses readr::guess_encoding() to sniff the file and
# picks the most likely encoding automatically.
# It also prints a message telling you what it detected.

tmp_auto <- file.path(tempdir(), "mystery_encoding.txt")
writeBin(iconv(txt_utf8, from = "UTF-8", to = "latin1", sub = "byte"), tmp_auto)

dt_auto <- build_corpus(tmp_auto, encoding = "auto")
message("Auto-detected read OK — first 120 chars:")
print(substr(dt_auto$text, 1, 120))

unlink(tmp_auto)


# 5) Auto-detection (whole directory) ----
# Auto-detection also works on entire directories. build_corpus() processes
# each file independently, which is useful when different files may have
# been saved with different settings.

tmp_dir <- file.path(tempdir(), "legacy_corpus")
dir.create(tmp_dir, showWarnings = FALSE)

# Convert all files in the demo corpus to CP1252 and save to temp folder.
for (f in list.files(corpus_dir, full.names = TRUE)) {
  txt <- readr::read_file(f)
  writeBin(
    iconv(txt, from = "UTF-8", to = "windows-1252", sub = "byte"),
    file.path(tmp_dir, basename(f))
  )
}

dt_auto_dir <- build_corpus(tmp_dir, encoding = "auto")
message(nrow(dt_auto_dir), " documents read with auto-detection")

unlink(tmp_dir, recursive = TRUE)


# 6) Mixed encodings in one directory ----
# Sometimes a folder contains files saved in different encodings — for example,
# some from an old archive (Latin-1) and newer ones (UTF-8).
# With encoding = "auto", build_corpus() detects each file individually.
# It will emit a warning to let you know the encodings were mixed, but
# all files are read correctly.

tmp_mixed <- file.path(tempdir(), "mixed_corpus")
dir.create(tmp_mixed, showWarnings = FALSE)

writeBin(
  iconv(txt_utf8, from = "UTF-8", to = "latin1", sub = "byte"),
  file.path(tmp_mixed, "legacy.txt")    # Latin-1
)
writeLines(txt_utf8, file.path(tmp_mixed, "modern.txt"))  # UTF-8

# Expect a "mixed encodings" warning — this is informational, not an error.
dt_mixed <- build_corpus(tmp_mixed, encoding = "auto")
message(nrow(dt_mixed), " documents read from mixed-encoding directory")

unlink(tmp_mixed, recursive = TRUE)
