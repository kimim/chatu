# Introduction

A small tool to various files to .svg files or download image files,
and insert it to orgmode or markdown on-the-fly.

**插 chā 图 tú**: insert a diagram.

# Install

Before using `chatu`, you may need to install some external programs and
set them in your PATH:
- draw.io and pdf2svg for drawio file conversion
- plantuml.jar, java for plantuml
- babashka for .bb script
- curl for downloading file from web

From melpa with use-package:

```emacs-lisp
(use-package chatu
  :hook ((org-mode markdown-mode) . chatu-mode)
  :commands (chatu-add
             chatu-open)
  :custom ((chatu-input-dir "./draws")
           (chatu-output-dir "./images")))
```

Or git submodule and use-package

```emacs-lisp
(use-package chatu
  :load-path "~/.emacs.d/site-lisp/chatu"
  :hook ((org-mode markdown-mode) . chatu-mode)
  :commands (chatu-add
             chatu-open)
  :custom ((chatu-input-dir "./draws")
           (chatu-output-dir "./images")))
```

# Usage

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
#+chatu: :drawio "diagram.drawio" :page 0 :input-dir "./draws" :output-dir "./images" :output "diagram.svg"
```

For example, we can get following `keyword-plist` from above `chatu` line:

```emacs-lisp
(:keyword "chatu" :type "drawio"
 :input "diagram.drawio" :output "diagram.svg" :page "0"
 :input-dir "./draws" :output-dir "./images"
 :input-path "./draws/diagram.drawio"
 :output-path "./images/diagram.svg")
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
#+chatu: :drawio "diagram.drawio" :page 0 :input-dir "./draws" :output-dir "./images" :output "diagram.svg"
```

markdown-mode:
```markdown
<!-- #+chatu: :drawio "diagram.drawio" :page 0 :input-dir "./draws" :output-dir "./images" :output "diagram.svg" -->
```

# Contributors

<a href = "https://github.com/kimim/chatu/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=kimim/chatu"/>
</a>
