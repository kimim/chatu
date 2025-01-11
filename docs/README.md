# Introduction

A small tool converts various files to .svg files or downloads image
files, and inserts them to orgmode or markdown buffer on-the-fly.

**插 chā 图 tú**: insert a diagram.

See examples in [chatu.org](./chatu.org) and [chatu.md](./chatu.md).

# Install

Before using `chatu`, you may need to install some external programs and
set them in your PATH:
- draw.io and pdf2svg for drawio file conversion
- plantuml.jar, java for plantuml
- clojure for .clj script
- babashka for .bb script
- Python for .py script
- curl for downloading file from web
- R for statistic plotting
- lilypond for music notes

Add `chatu.el` from melpa with use-package:

```emacs-lisp
(use-package chatu
  :hook ((org-mode markdown-mode) . chatu-mode)
  :commands (chatu-add
             chatu-open)
  :custom ((chatu-input-dir "./draws")
           (chatu-output-dir "./draws_out")))
```

Or git submodule and use-package

```emacs-lisp
(use-package chatu
  :load-path "~/.emacs.d/site-lisp/chatu"
  :hook ((org-mode markdown-mode) . chatu-mode)
  :commands (chatu-add
             chatu-open)
  :custom ((chatu-input-dir "./draws")
           (chatu-output-dir "./draws_out")))
```

## draw.io

You may need to add draw.io to your PATH. If you installed draw.io
with homebrew, `draw.io` executable is located in following path:

```shell
export PATH=/opt/homebrew/Caskroom/drawio/24.2.5/draw.io.app/Contents/MacOS:$PATH
```

# Usage
Add `chatu` line with `chatu-new` command:

```org
#+chatu: :drawio "diagram.drawio" :page 0 :input-dir "./draws" :output-dir "./draws_out" :output "diagram.svg" :crop :nopdf
```
```markdown
<!-- #+chatu: :drawio "diagram.drawio" :page 0 :input-dir "./draws" :output-dir "./draws_out" :output "diagram.svg" :crop :nopdf -->
```

Options/Settings:
- `:drawio` specifies the backend, they can be `:planuml`, `:babashka`, `:curl` or others added in the future by you.
- file name after backend keyword is the input file name.
- `:page` species which page you want to import to buffer.
- `:input-dir` and `:output-dir` are the folders for input or output file.
- `:output` is the output file name.
- `:crop` if you want to crop the empty white in page.
- `:nopdf` if you do not want intermediate pdf file for drawio backend.

Move cursor to `chatu` line,
- `C-c C-c` will invoke `chatu-add` to add image in orgmode.
- `C-c C-c C-c` will invoke `chatu-add` to add image in markdown-mode.  
- `C-c C-o` will invoke `chatu-open` to open original .drawio or .puml file.

`chatu` line means different in orgmode and markdown:
- `#+chatu: :drawio` or `#+chatu: :plantuml` in orgmode
- `<-- #+chatu: :drawio -->` or `<-- #+chatu: :plantuml -->` in markdown

**Remind!**
- the input files should be the first parameter, for example, .drawio
  or .puml file.
- in order to support whitespace in file and dir name, please "quote
  all of them".

# Extension

You can easily extend this package by adding new `chatu-<tool>.el`. For
example, when `<tool> = drawio`, you need to define `chatu-drawio-open` and  `chatu-drawio-script` in `chatu-drawio.el`.
- `chatu-drawio-open` is invoked to open the drawio input file at the line.
- `chatu-drawio-script` is used to generate the shell script for conversion.

Both method use a `keyword-plist` parameter, which contains the
`chatu` settings from `chatu` line.

```org
#+chatu: :drawio "diagram.drawio" :page 0 :input-dir "./draws" :output-dir "./draws_out" :output "diagram.svg" :crop :nopdf :output-ext svg
```

For example, we can get following `keyword-plist` from above `chatu` line:

```emacs-lisp
(:chatu t :type "drawio"
 :input "diagram.drawio" :output "diagram.svg" :page "0"
 :input-dir "./draws" :output-dir "./draws_out"
 :input-path "./draws/diagram.drawio"
 :output-path "./draws_out/diagram.svg"
 :output-ext "svg"
 :crop t
 :nopdf t)
```

# Usage

See example [chatu.org](./chatu.org) in orgmode and  [chatu.md](./chatu.md) in markdown-mode.

## Simple Usage

org-mode:
```org
#+chatu: :drawio "diagram.drawio"
#+chatu: :plantuml "diagram.puml"
#+chatu: :curl "http://example.org/image.svg"
#+chatu: :babashka "babashka.bb"
```

markdown-mode:
```markdown
<!-- #+chatu: :drawio "diagram.drawio" -->
<!-- #+chatu: :plantuml "diagram.puml" -->
<!-- #+chatu: :curl "http://example.org/image.svg" -->
<!-- #+chatu: :babashka "babashka.bb" -->
```

## Omit the extension

org-mode:
```org
#+chatu: :drawio "diagram"
#+chatu: :plantuml "diagram"
```

markdown-mode:
```markdown
<!-- #+chatu: :drawio "diagram" -->
<!-- #+chatu: :plantuml "diagram" -->
```

## Add more properties

org-mode:
```org
#+chatu: :drawio "diagram"
#+name: workflow
#+caption: chatu workflow
```

## Extract specific page

org-mode:
```org
#+chatu: :drawio "diagram.drawio" :page 1
#+chatu: :plantuml "diagram.puml" :page 1
```

markdown-mode:
```markdown
<!-- #+chatu: :drawio "diagram.drawio" :page 1 -->
<!-- #+chatu: :plantuml "diagram.puml" :page 1 -->
```

## Even more specific

Add input-dir, output-dir and output file name

org-mode:
```org
#+chatu: :drawio "diagram.drawio" :page 0 :input-dir "./draws" :output-dir "./draws_out" :output "diagram.svg"
```

markdown-mode:
```markdown
<!-- #+chatu: :drawio "diagram.drawio" :page 0 :input-dir "./draws" :output-dir "./draws_out" :output "diagram.svg" -->
```

# Contributors

<a href = "https://github.com/kimim/chatu/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=kimim/chatu"/>
</a>
