#!/usr/bin/env perl
$latex = 'platex -kanji=utf8 -no-guess-input-enc -synctex=1 -interaction=nonstopmode %O %S';
$dvipdf = 'dvipdfmx %O -o %D %S';
$bibtex = 'pbibtex %O %B';
$biber = 'biber --bblencoding=utf8 -u -U --output_safechars';
$pdf_mode = 3; # use dvipdf
$max_repeat = 5;
