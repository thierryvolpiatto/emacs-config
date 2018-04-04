;;; Auctex/Latex config
;;
(load "auctex")

(defun tv-insert-skel-latex-doc ()
  "Insert a LaTeX skeleton in an empty file."
  (interactive)
  (insert "\\documentclass[a4paper,11pt]{article}\n"
          "\\usepackage[french]{babel}\n"
          "\\usepackage[utf8]{inputenc}\n"
          "\\usepackage{textcomp}% Allow to use euro sign\n"
          "\n"
          "%\\usepackage[pdftex=true,
           %hyperindex=true,
           %colorlinks=true]{hyperref}"
          "\n"
          "%\\usepackage[hypertex=true,
           %hyperindex=true,
           %colorlinks=false]{hyperref}"
          "\n"
          "%\\usepackage{url}\n"
          "%\\usepackage{natbib}\n"
          "%\\usepackage{setspace}\n"
          "%\\usepackage{qtree}\n"
          "%\\usepackage{booktabs}\n"
          "\n"
          "\n"
          "\\begin{document}\n"
          "%\n"
          "%\\begin{titlepage}\n"
          "\\title{}\n"
          "\\date{\\today}\n"
          "\\author{}\n"
          "\\maketitle\n"
          "%\\tableofcontents\n"
          "%\\end{titlepage}\n"
          "\n"
          "\n"
          "\\end{document}\n")
  (goto-char (point-min))
  (when (re-search-forward "[\\]title")
    (beginning-of-line)
    (forward-char 7)))

;; This template needs the lettre package
;; included in texlive-latex-extra package.
(defun tv-insert-skel-latex-letter ()
  "Insert a latex skeleton letter in an empty file"
  (interactive)
  (insert
   "\\documentclass[12pt]{lettre}\n"
   "\n"
   "\n"
   "\\usepackage[T1]{fontenc}\n"
   "\\usepackage{lmodern}\n"
   "\\usepackage{eurosym} % Use \euro for €\n"
   "\\usepackage[francais]{babel}\n"
   "\\usepackage[utf8]{inputenc}\n"
   "\\begin{document}\n"
   "\n"
   "\\begin{letter}{destinataire\\\\adresse1\\\\adresse2} % nom et addresse destinataire\n"
   "\\name{expéditeur}\n"
   "\\signature{Thierry Volpiatto}\n"
   "\\address{expéditeur\\\\adresse1\\\\adresse2} % nom expéditeur\n"
   "\\lieu{ville}\n"
   "\\telephone{01~02~03~04~05}\n"
   "\\email{thierry@fai.fr}\n"
   "\\nofax\n"
   "\n"
   "\\def\\concname{Objet :~} % ne rien modifier ici\n"
   "\\conc{objet de la lettre} % objet modifier ici\n"
   "\\opening{Madame, Monsieur,}\n"
   "\n"
   "% Contenu de la lettre\n"
   "\n"
   "\\closing{Je vous prie d'agréer, Madame, Monsieur, mes salutations distinguées.}\n"
   "\n"
   "\\encl{Pièces jointes}\n"
   "\n"
   "\\ps{PS :~}{Post scritum ici}\n"
   "\\end{letter}\n"
   "\n"
   "\\end{document}\n")
  (goto-char (point-min))
  (when
      (re-search-forward "[\\]begin\{letter\}")
    (beginning-of-line)
    (forward-char 15)))

-;;; xdvi (Needed in auctex)
-;;
-(use-package xdvi-search)
