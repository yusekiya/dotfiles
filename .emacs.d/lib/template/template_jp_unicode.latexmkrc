#!/usr/bin/env perl
$latex = 'uplatex -kanji=utf8 -no-guess-input-enc -synctex=1 -interaction=nonstopmode %O %S';
$dvipdf = 'dvipdfmx %O -o %D %S';
$bibtex = 'upbibtex %O %B';
$pdf_mode = 3;
$max_repeat = 5;
