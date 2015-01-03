#!/bin/sh
#
# This is a dummy C code line counter originated from the book, "21st Century 
# C", and just for my practice with shell and GitHub.
#
# If you would like to count ines of C code, the best tool is *CLOC*: 
# http://cloc.sourceforge.net/ 
#

# Count lines with a ;, ), or }, and let that count be named Lines.
Lines=`grep -r '[;)}]' *.c | wc -l`

# Now count how many lines there are in a directory listing; name it Files.
Files=`find ./ -name '*.c' -print | wc -l`

echo files=$Files and lines=$Lines

# Arithmetic expansion is a double-paren.
# In bash, the remainder is truncated; more on this later.
echo line/file = $(($Lines/$Files))

# Or, use those variables in a here script.
# By setting scale=3, answers are printed to 3 decimal places.
bc << ---
scale=3
$Lines/$Files
---