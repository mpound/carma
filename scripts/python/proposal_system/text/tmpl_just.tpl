\documentclass[preprint, letterpaper, 12pt]{aastex}
\usepackage[table,rgb]{xcolor}
\usepackage[letterpaper]{geometry}
\usepackage{helvet}
\usepackage{tabularx}
\pagestyle{empty}
\geometry{left=0.75in, right=0.75in, top=0.75in, bottom=0.75in}
\begin{document}
\newlength{\carmaindent}
\setlength{\carmaindent}{\parindent}
\setlength{\parskip}{0in}
\newlength{\sectitlelength}
\newcommand{\sectitlel}[1]{
  \setlength{\sectitlelength}{\parindent}
  \setlength{\parindent}{0in}
  \vskip 0.15in
  \begin{tabularx}{\textwidth}{@{}l@{}}
    \hiderowcolors
    {\sffamily \Large \textbf{#1} \normalfont} \\
    \hline
    \showrowcolors
  \end{tabularx}
  \setlength{\parindent}{\sectitlelength}
  \vskip -0.3cm
}

\sectitlel{Scientific Justification}

%s

\sectitlel{Techical Justification}

%s

\end{document}
