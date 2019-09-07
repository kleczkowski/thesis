# LaTeXmk build directory
$out_dir = 'build';

# pdfLaTeX configuration
$pdf_mode = 1;
$dvi_mode = 0;
$postscript_mode = 0;
$pdflatex = 'pdflatex -synctex=1 -shell-escape --interaction=nonstopmode -file-line-error';

# PDF previewing utility
$pdf_previewer = 'mupdf-x11';
$pdf_update_method = 2;
