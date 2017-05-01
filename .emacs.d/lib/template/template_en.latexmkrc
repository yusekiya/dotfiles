#!/usr/bin/env perl
$pdflatex = 'pdflatex -file-line-error --synctex=1 -interaction=nonstopmode %O %S';
$pdf_mode = 1;
$max_repeat = 5;
$bibtex = bibtex;
$biber = 'biber --bblencoding=utf8 -u -U --output_safechars';
