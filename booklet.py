#!/usr/bin/env python3
"""
make_book_ae.py - turn the "Analisi Economica" GitHub README (Italian) into a
single, FULLY SELF-CONTAINED, book-styled HTML file:
  - cover page + auto table of contents;
  - LaTeX math rendered to native MathML AT BUILD TIME (latex2mathml), so it
    needs no internet, no MathJax, and no "trust scripts" step;
  - R code syntax-highlighted AT BUILD TIME (Pygments) - also offline;
  - GitHub `> [!NOTE]` alerts turned into styled callout boxes;
  - figure captions that were wrapped in <sub> (which collapse to unreadable
    lines) restyled as proper captions.

The only remote content left is the figures themselves (raw.githubusercontent
image URLs); everything else works with no network.

Requirements: pip install markdown latex2mathml pygments
Usage: python make_book_ae.py [README.md] [-o booklet.html]
"""
import argparse
import os
import re
from datetime import date
import markdown
import latex2mathml.converter as l2m
from pygments.formatters import HtmlFormatter

# ---------------------------------------------------------------------------
# Step 1: load & normalise the raw markdown
# ---------------------------------------------------------------------------
def normalise_source(raw: str) -> str:
    raw = raw.replace("\x00", "")
    raw = raw.replace("\r\n", "\n").replace("\r", "\n")
    # Drop the subtitle heading: it lives on the cover (hardcoded) and would
    # otherwise become a stray top-level entry in the auto TOC (dead anchor).
    raw = raw.replace(
        "### Produzione, distribuzione, conflitto e moneta \U0001F4B0\n", "", 1
    )
    # Fence the one 4-space-indented code block (install.packages) as ```r.
    raw = raw.replace(
        '    install.packages(c("ggplot2", "dplyr", "tidyr", "knitr", "kableExtra"))',
        '```r\ninstall.packages(c("ggplot2", "dplyr", "tidyr", "knitr", "kableExtra"))\n```',
    )
    return raw


def strip_accidental_indentation(raw: str) -> str:
    out_lines, in_fence = [], False
    for line in raw.split("\n"):
        s = line.lstrip()
        if s.startswith("```"):
            in_fence = not in_fence
            out_lines.append(s)
            continue
        out_lines.append(line if in_fence else s)
    return "\n".join(out_lines)


def fix_lists_interrupting_paragraphs(raw: str) -> str:
    list_re = re.compile(r"^\s*([-*+]\s|\d+[.)]\s)")
    fixed, in_fence = [], False
    for line in raw.split("\n"):
        if line.lstrip().startswith("```"):
            in_fence = not in_fence
            fixed.append(line)
            continue
        if not in_fence and list_re.match(line):
            prev = fixed[-1] if fixed else ""
            if prev.strip() != "" and not list_re.match(prev):
                fixed.append("")
        fixed.append(line)
    return "\n".join(fixed)


def strip_github_alert_markers(raw: str) -> str:
    out = []
    for line in raw.split("\n"):
        if re.match(r"^\s*>\s*\[!(NOTE|TIP|IMPORTANT|WARNING|CAUTION)\]\s*$", line):
            continue
        out.append(line)
    return "\n".join(out)


def restyle_captions(raw: str) -> str:
    """Figure captions that are wrapped in <sub>...</sub> collapse to
    overlapping lines (because inline <sub> has zero line-height). Convert
    `<sub><em> ... </em></sub>` into `<em class="figcap"> ... </em>` and tag
    the newer `<em><strong>Figura ...` captions with the same class, so all
    captions share one readable style."""
    raw = re.sub(
        r"<sub>\s*<em>(.*?)</em>\s*</sub>",
        r'<em class="figcap">\1</em>',
        raw,
    )
    raw = raw.replace("<em><strong>Figura", '<em class="figcap"><strong>Figura')
    return raw


def replace_programma_with_toc(raw: str) -> str:
    pattern = re.compile(
        r"### Parte I - Le origini dell'approccio classico\n.*?"
        r"- \[Riferimenti bibliografici\]\([^)]*\)\n",
        re.DOTALL,
    )
    m = pattern.search(raw)
    return raw if not m else raw[: m.start()] + "[TOC]\n" + raw[m.end():]


def fix_github_image_urls(raw: str) -> str:
    def fix_img(match):
        user, repo, branch, path = match.groups()
        return f'src="https://raw.githubusercontent.com/{user}/{repo}/{branch}/{path}"'
    return re.sub(
        r'src="https://github\.com/([^/]+)/([^/]+)/blob/([^/]+)/([^"]+)"',
        fix_img,
        raw,
    )


# ---------------------------------------------------------------------------
# Step 2: protect LaTeX math, then restore it as build-time MathML
# ---------------------------------------------------------------------------
class MathProtector:
    def __init__(self):
        self.store = []
        self.failures = []

    def _stash(self, s):
        self.store.append(s)
        return f"@@MATH{len(self.store) - 1}@@"

    def protect(self, raw):
        out, in_fence = [], False
        for line in raw.split("\n"):
            if line.lstrip().startswith("```"):
                in_fence = not in_fence
                out.append(line)
                continue
            if in_fence:
                out.append(line)
                continue
            line = re.sub(r"\$\$[^\n]+?\$\$", lambda m: self._stash(m.group(0)), line)
            line = re.sub(r"\$[^\$\n]+?\$", lambda m: self._stash(m.group(0)), line)
            out.append(line)
        return "\n".join(out)

    def _to_mathml(self, span):
        disp = span.startswith("$$")
        tex = span[2:-2] if disp else span[1:-1]
        try:
            return l2m.convert(tex, display="block" if disp else "inline")
        except Exception as e:  # never lose content: fall back to raw code
            self.failures.append((tex, str(e)[:80]))
            safe = tex.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            return f'<code class="mathfallback">{safe}</code>'

    def restore(self, html):
        html = re.sub(
            r"@@MATH(\d+)@@",
            lambda m: self._to_mathml(self.store[int(m.group(1))]),
            html,
        )
        # Unwrap display math that markdown parked inside its own <p>.
        html = re.sub(
            r"<p>\s*(<math[^>]*display=\"block\".*?</math>)\s*</p>",
            r"\1",
            html,
            flags=re.DOTALL,
        )
        return html


# ---------------------------------------------------------------------------
# Step 3: markdown -> HTML (with server-side Pygments code highlighting)
# ---------------------------------------------------------------------------
def convert_to_html(raw):
    md = markdown.Markdown(
        extensions=["extra", "toc", "sane_lists", "codehilite"],
        extension_configs={
            "toc": {"toc_depth": "2-3", "permalink": False},
            "codehilite": {"guess_lang": False, "css_class": "highlight",
                           "linenums": False},
        },
    )
    return md.convert(raw), md.toc


# ---------------------------------------------------------------------------
# Step 4: assemble the styled, book-like document
# ---------------------------------------------------------------------------
PYGMENTS_CSS = HtmlFormatter(style="friendly").get_style_defs(".highlight")

CSS = """
:root {
  --ink:#232323; --muted:#6b6b6b; --paper:#fdfcf9; --accent:#8a3b2b;
  --accent-soft:#f4ece6; --rule:#e2ddd3; --code-bg:#f6f5f1;
  --box-bg:#fbf3e7; --box-border:#d9a15a;
}
* { box-sizing: border-box; }
html { background:#e9e6de; }
body { margin:0; padding:3rem 0 6rem; background:#e9e6de; color:var(--ink);
  font-family:"Iowan Old Style","Palatino Linotype",Palatino,Georgia,"Times New Roman",serif;
  font-size:18px; line-height:1.7; }
.page { max-width:820px; margin:0 auto; background:var(--paper);
  padding:4rem 5rem 5rem; box-shadow:0 0 0 1px rgba(0,0,0,.04),0 20px 50px rgba(0,0,0,.15); }
.cover { min-height:90vh; display:flex; flex-direction:column; align-items:center;
  justify-content:center; text-align:center; border-bottom:1px solid var(--rule);
  margin-bottom:3rem; padding-bottom:3rem; }
.kicker { text-transform:uppercase; letter-spacing:.13em; font-size:.76rem;
  color:var(--muted); margin-bottom:1.6rem; max-width:34rem; line-height:1.6; }
.book-title { font-size:2.9rem; line-height:1.12; margin:0 0 .9rem; font-weight:600; }
.book-subtitle { font-style:italic; color:var(--muted); font-size:1.2rem; max-width:32rem; margin:0 0 2.2rem; }
.cover-image { max-width:88%; border-radius:6px; box-shadow:0 10px 30px rgba(0,0,0,.18); margin-bottom:2rem; }
.blurb { max-width:34rem; color:#40403d; font-size:.98rem; margin-bottom:1.8rem; }
.author { font-size:.95rem; letter-spacing:.05em; color:var(--muted); }
.updated { margin-top:1.2rem; font-size:.82rem; color:var(--muted); letter-spacing:.03em; }
h1,h2,h3,h4 { font-weight:600; color:#1c1c1c; }
h2 { font-size:1.7rem; margin-top:3.2rem; padding-top:1.4rem; border-top:1px solid var(--rule); }
h2:first-of-type { border-top:none; }
h3 { font-size:1.28rem; margin-top:2.4rem; color:var(--accent); }
h4 { font-size:1.05rem; margin-top:1.8rem; }
p { margin:1.1em 0; }
a { color:var(--accent); text-decoration:none; border-bottom:1px solid rgba(138,59,43,.35); }
a:hover { border-bottom-color:var(--accent); }
hr { border:none; border-top:1px solid var(--rule); margin:2.6rem 0; }
figure { margin:2rem 0; text-align:center; }
img { max-width:100%; height:auto; border-radius:4px; }
figure img { box-shadow:0 6px 20px rgba(0,0,0,.12); }
table { border-collapse:collapse; width:100%; margin:1.6rem 0; font-size:.9rem; }
th,td { border:1px solid var(--rule); padding:.45rem .7rem; text-align:left; vertical-align:top; }
th { background:var(--accent-soft); font-weight:600; }
code { font-family:"SFMono-Regular",Menlo,Consolas,"Courier New",monospace;
  background:var(--code-bg); padding:.1em .35em; border-radius:3px; font-size:.86em; }
pre { background:var(--code-bg); border:1px solid var(--rule); border-radius:6px;
  padding:1rem 1.2rem; overflow-x:auto; font-size:.82rem; line-height:1.5; }
pre code { background:none; padding:0; }
.highlight { background:var(--code-bg); border:1px solid var(--rule); border-radius:6px; margin:1.6rem 0; }
.highlight pre { border:none; margin:0; background:none; }
blockquote { background:var(--box-bg); border-left:4px solid var(--box-border);
  border-radius:4px; margin:2rem 0; padding:.4rem 1.6rem; }
blockquote > *:first-child { margin-top:.6rem; }
blockquote p { font-size:.96rem; }
blockquote table { font-size:.85rem; }
.toc { background:#faf8f2; border:1px solid var(--rule); border-radius:6px;
  padding:1.4rem 2rem; margin:1.6rem 0 2.6rem; }
.toc ul { list-style:none; margin:.2rem 0; padding-left:1.1rem; }
.toc > ul { padding-left:0; }
.toc li { margin:.3rem 0; }
.toc a { border-bottom:none; }
.toc a:hover { text-decoration:underline; }
/* figure captions (formerly wrapped in <sub>, which collapsed the lines) */
.figcap { display:block; font-style:italic; font-size:.85rem; line-height:1.5;
  color:var(--muted); margin-top:.5rem; }
.figcap strong { color:#4a463d; font-style:normal; }
/* native MathML */
math { font-size:1.02em; }
math[display="block"] { display:block; margin:1.3rem 0; font-size:1.12em; overflow-x:auto; }
.mathfallback { color:#8a3b2b; }
sub, sup { line-height:0; }
footer.colophon { max-width:820px; margin:2rem auto 0; text-align:center; color:var(--muted); font-size:.8rem; }
@media print {
  body { background:var(--paper); padding:0; }
  .page { box-shadow:none; padding:0 .4in; max-width:none; }
  .cover { min-height:100vh; page-break-after:always; }
  h2 { page-break-before:always; border-top:none; padding-top:0; }
  h2:first-of-type { page-break-before:auto; }
  .toc { page-break-after:always; }
  a { border-bottom:none; }
  pre, .highlight, blockquote, table, figure { page-break-inside:avoid; }
}
@media (max-width:720px) { .page { padding:2.2rem 1.4rem; } body { font-size:16.5px; } }
"""


def build_cover(header_html):
    m = re.search(r'src="([^"]*analisi_economica_cover\.png)"', header_html)
    img = f'<img class="cover-image" src="{m.group(1)}" alt="Copertina">' if m else ""
    return f"""
<section class="cover">
  <p class="kicker">Universit&agrave; degli Studi dell'Aquila &middot; DIIIE &middot; Corso di Laurea Magistrale in Amministrazione, Economia e Finanza</p>
  <h1 class="book-title">Analisi Economica</h1>
  <p class="book-subtitle">Produzione, distribuzione, conflitto e moneta</p>
  {img}
  <p class="blurb">Note delle lezioni: l'economia capitalistica come sistema monetario di produzione, dall'economia politica classica agli sviluppi modellistici pi&ugrave; recenti (input-output, modelli fondi-flussi, sistemi complessi).</p>
  <p class="author">Marco Veronese Passarella</p>
  <p class="updated">Ultimo aggiornamento: {date.today().strftime('%d/%m/%Y')}</p>
</section>
"""


def assemble_document(body_html):
    idx = body_html.find("<h2")
    header_html, rest_html = body_html[:idx], body_html[idx:]
    return f"""<!DOCTYPE html>
<html lang="it">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Analisi Economica &mdash; Note delle lezioni</title>
<style>{CSS}
{PYGMENTS_CSS}
</style>
</head>
<body>
<div class="page">
{build_cover(header_html)}
{rest_html}
</div>
<footer class="colophon">
  Compilato dal README del corso <em>Analisi Economica</em> (Universit&agrave; degli Studi dell'Aquila).<br>
  Repository: <a href="https://github.com/marcoverpas/analisi_economica">github.com/marcoverpas/analisi_economica</a>
</footer>
</body>
</html>
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", nargs="?", default="README.md",
                        help="path to the README markdown file (default: README.md)")
    parser.add_argument("-o", "--output", default="booklet.html",
                        help="output HTML path (default: booklet.html)")
    args = parser.parse_args()

    if not os.path.exists(args.source):
        raise SystemExit(
            f"File sorgente non trovato: '{args.source}'.\n"
            f"Lancia:  python {os.path.basename(__file__)} <nome_del_README>.md -o booklet.html"
        )

    raw = open(args.source, encoding="utf-8").read()
    raw = normalise_source(raw)
    raw = strip_accidental_indentation(raw)
    raw = fix_lists_interrupting_paragraphs(raw)
    raw = strip_github_alert_markers(raw)
    raw = restyle_captions(raw)
    raw = replace_programma_with_toc(raw)
    raw = fix_github_image_urls(raw)

    protector = MathProtector()
    raw = protector.protect(raw)
    body_html, _ = convert_to_html(raw)
    body_html = protector.restore(body_html)
    final_html = assemble_document(body_html)

    with open(args.output, "w", encoding="utf-8") as f:
        f.write(final_html)
    msg = f"Wrote {args.output} ({len(final_html):,} bytes)"
    if protector.failures:
        msg += f"  [ATTENZIONE: {len(protector.failures)} formule non convertite]"
    print(msg)


if __name__ == "__main__":
    main()
