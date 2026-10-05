\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{masters_thesis}[2026/04/17 Custom thesis class]

\LoadClass[12pt,a4paper]{article}

\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage[english]{babel}
\RequirePackage{geometry}
\geometry{left=2cm, right=2cm, top=2cm, bottom=2cm, headheight=15pt}
\RequirePackage{graphicx}
\RequirePackage{setspace}
\RequirePackage{csquotes}
\RequirePackage{hyperref}
\RequirePackage[nameinlink, capitalize]{cleveref}
\RequirePackage{bookmark}
\RequirePackage[dvipsnames]{xcolor}
\RequirePackage{ragged2e}
\RequirePackage[backend=biber, style=chicago-authordate]{biblatex}
\RequirePackage[flushleft]{threeparttable}
\RequirePackage{booktabs}
\RequirePackage{makecell}
\RequirePackage{amssymb}
\RequirePackage{caption}
\RequirePackage{environ}
\RequirePackage{pdflscape}
\RequirePackage{afterpage}
\RequirePackage{float}
\RequirePackage{fancyhdr}
\RequirePackage{pdfpages}

\renewcommand{\sectionmark}[1]{\markboth{\thesection\quad #1}{}}

\renewcommand{\arraystretch}{1.3}

\newcommand\todo[1]{\textcolor{red}{\textbf{TODO:} #1}}

\newenvironment{thesisabstract}{
    \begin{center}
        \begin{minipage}{0.8\textwidth}
            \begin{singlespace}
                \justifying
}{
            \end{singlespace}
        \end{minipage}
    \end{center}
}
\providecommand{\myAbstract}{}

\setcounter{tocdepth}{2}

\renewcommand{\maketitle}{%
    \begin{titlepage}
        \vspace*{\fill}
        \begin{center}
            {\Large Master's Thesis \par}
            \vspace{1cm}
            {\huge\bfseries \myTitle \par}
            \vspace{0.5cm}
            {\Large \myAuthor \par}
            \myInstitute\par
            \myUniversity\par
        \end{center}
        \vspace*{\fill}
        \begin{flushleft}
            \begin{tabular}{ll}
                \textbf{Course of Study:} & \myCourse \\
                \textbf{Examiner:} & \mySupervisor \\
                \textbf{Start date: } & \myStartDate \\
                \textbf{End date: } & \myEndDate \\
            \end{tabular}
        \end{flushleft}
    \end{titlepage}%
    \clearpage
    \pagenumbering{roman}
    \setcounter{page}{2}
    \begin{thesisabstract}
        \begin{center}
            {\textbf{Abstract}}
        \end{center}
        \myAbstract
    \end{thesisabstract}
    \clearpage
    \begingroup
        \singlespacing
        \hypersetup{linkcolor=black}
        \tableofcontents
        \clearpage
        \listoffigures
        \listoftables
        \clearpage
    \endgroup
    \pagenumbering{arabic}
    \setcounter{page}{1}

    \pagestyle{fancy}
    \fancyhf{}
    \fancyhead[L]{\leftmark}
    \fancyhead[R]{\thepage}
    \renewcommand{\headrulewidth}{0.4pt}
}

\onehalfspacing
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt}


\hypersetup{
    colorlinks=true,
    linkcolor=MidnightBlue,
    citecolor=MidnightBlue,
    urlcolor=MidnightBlue
}

\creflabelformat{equation}{#2#1#3}
\crefname{equation}{Equation}{Equations}
\Crefname{equation}{Equation}{Equations}
\crefname{figure}{Figure}{Figures}
\Crefname{figure}{Figure}{Figures}

\graphicspath{{../output/figures}}
\newcommand{\tablerendering}[1]{\input{../output/tables/#1.tex}}



\DeclareCaptionFormat{custom}{#1\par#3} 

\captionsetup[figure]{
    format=custom,
    labelsep=none,
    justification=centering,
    singlelinecheck=false,
    labelfont={sc},
    textfont={normalfont},
    position=top,
    skip=10pt
}

\captionsetup[table]{
    format=custom,
    labelsep=none,
    justification=centering,
    singlelinecheck=false,
    labelfont={sc},
    textfont={normalfont},
    position=top,
    skip=10pt
}

\NewEnviron{customfigure}[1][htb]{%
    \begin{figure}[#1]
        \centering
        \BODY
    \end{figure}
}

\NewEnviron{customtable}[1][htbp]{%
    \begin{table}[#1]
        \centering
        \begin{threeparttable}
            \BODY
        \end{threeparttable}
    \end{table}
}

\NewDocumentEnvironment{customltable}{ +b }{%
    \afterpage{%
        \begin{landscape}
            \thispagestyle{empty}
            \begin{table}
                \centering
                \begin{threeparttable}
                    #1 %
                \end{threeparttable}
            \end{table}
        \end{landscape}
    }%
}{}

\newcommand{\fignotes}[1]{%
    \par\nobreak\vspace{10pt}
    \noindent\begin{minipage}{\linewidth}
        \small\normalfont\textit{Notes:} #1
    \end{minipage}
}

\newcommand{\tabnotes}[1]{%
    \begin{tablenotes}[flushleft]
        \small\normalfont
        \item \textit{Notes:} #1
    \end{tablenotes}
}


\newcommand{\lsection}[2]{%
    \section{#1}
    \label{sec:#2}
}