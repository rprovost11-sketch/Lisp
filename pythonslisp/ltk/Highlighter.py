"""Syntax highlighting and Markdown rendering for terminal display.

Public API:
   highlight(code, lang)          — ANSI-color a code snippet by language
   render_markdown(text, use_color) — render a Markdown string for the terminal
"""
from __future__ import annotations
import re
import os
import sys

# ANSI escape codes
_R   = '\033[0m'      # reset
_DIM = '\033[2;37m'   # dim gray          — comments
_GRN = '\033[32m'     # green             — strings
_CYN = '\033[36m'     # cyan              — Lisp :keywords
_YEL = '\033[33m'     # yellow            — numbers; Python decorators
_BCY = '\033[1;36m'   # bold cyan         — Lisp special operators; Python def/class names
_BMA = '\033[1;35m'   # bold magenta      — Lisp definition forms
_BYE = '\033[1;33m'   # bold yellow       — T  NIL
_BBL = '\033[1;34m'   # bold blue         — Python keywords
_BWH = '\033[1;97m'   # bold bright white — parentheses and brackets
_BLD = '\033[1m'      # bold              — Markdown h2/h3 and **bold**
_UND = '\033[1;4m'    # bold + underline  — Markdown h1
_ICY = '\033[96m'     # bright cyan       — Markdown inline `code`


# ---------------------------------------------------------------------------
# Lisp highlighter
# ---------------------------------------------------------------------------

_LISP_SPECIAL_OPS = frozenset({
    'IF', 'QUOTE', 'SETQ', 'LET', 'LET*', 'PROGN', 'COND', 'CASE',
    'FUNCALL', 'APPLY', 'AND', 'OR', 'NOT', 'WHEN', 'UNLESS',
    'BLOCK', 'RETURN', 'RETURN-FROM', 'CATCH', 'THROW', 'SETF',
})

_LISP_DEF_FORMS = frozenset({
    'DEFUN', 'DEFMACRO', 'DEFSTRUCT', 'LAMBDA',
})

_LISP_BOOLS = frozenset({'T', 'NIL'})

_LISP_RE = re.compile(
    r'(?P<comment>;[^\n]*)'
    r'|(?P<string>"(?:[^"\\]|\\.)*")'
    r'|(?P<keyword>:[A-Za-z][A-Za-z0-9+\-~!$%^&*_=/?<>|]*)'
    r'|(?P<number>[+-]?\d+(?:/\d+|\.\d*(?:[eE][+-]?\d+)?)?)'
    r'|(?P<paren>,@|,|[()\'\x60])'
    r'|(?P<symbol>[A-Za-z!$%&*+\-/<=>?@^_~][A-Za-z0-9!$%&*+\-/<=>?@^_~|:#.]*)'
)


def highlight_lisp(code: str) -> str:
   """Return an ANSI-colored version of a Lisp code string."""
   parts = []
   pos   = 0
   for m in _LISP_RE.finditer(code):
      start, end = m.span()
      if start > pos:
         parts.append(code[pos:start])
      text = m.group()
      kind = m.lastgroup
      if kind == 'comment':
         parts.append(_DIM + text + _R)
      elif kind == 'string':
         parts.append(_GRN + text + _R)
      elif kind == 'keyword':
         parts.append(_CYN + text + _R)
      elif kind == 'number':
         parts.append(_YEL + text + _R)
      elif kind == 'paren':
         parts.append(_BWH + text + _R)
      elif kind == 'symbol':
         upper = text.upper()
         if upper in _LISP_SPECIAL_OPS:
            parts.append(_BCY + text + _R)
         elif upper in _LISP_DEF_FORMS:
            parts.append(_BMA + text + _R)
         elif upper in _LISP_BOOLS:
            parts.append(_BYE + text + _R)
         else:
            parts.append(text)
      else:
         parts.append(text)
      pos = end
   if pos < len(code):
      parts.append(code[pos:])
   return ''.join(parts)


# ---------------------------------------------------------------------------
# Python highlighter
# ---------------------------------------------------------------------------

_PY_KEYWORDS = frozenset({
    'def', 'class', 'if', 'elif', 'else', 'return', 'for', 'while',
    'import', 'from', 'and', 'or', 'not', 'in', 'is', 'None', 'True',
    'False', 'lambda', 'with', 'try', 'except', 'raise', 'continue',
    'break', 'pass', 'yield', 'as', 'global', 'nonlocal', 'del',
    'assert', 'finally', 'async', 'await',
})

_PY_DEF_STARTERS = frozenset({'def', 'class'})

_PY_RE = re.compile(
    r'(?P<comment>#[^\n]*)'
    r'|(?P<triplestr>"""[\s\S]*?"""|\'\'\'[\s\S]*?\'\'\')'
    r'|(?P<string>"(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\')'
    r'|(?P<decorator>@[A-Za-z_][A-Za-z0-9_]*)'
    r'|(?P<number>\b\d+(?:\.\d+)?(?:[eE][+-]?\d+)?\b)'
    r'|(?P<bracket>[(){}\[\]])'
    r'|(?P<word>[A-Za-z_][A-Za-z0-9_]*)'
)


def highlight_python(code: str) -> str:
   """Return an ANSI-colored version of a Python code string."""
   parts           = []
   pos             = 0
   next_is_defname = False
   for m in _PY_RE.finditer(code):
      start, end = m.span()
      if start > pos:
         parts.append(code[pos:start])
      text = m.group()
      kind = m.lastgroup
      if kind == 'comment':
         parts.append(_DIM + text + _R)
         next_is_defname = False
      elif kind in ('triplestr', 'string'):
         parts.append(_GRN + text + _R)
         next_is_defname = False
      elif kind == 'decorator':
         parts.append(_YEL + text + _R)
         next_is_defname = False
      elif kind == 'number':
         parts.append(_YEL + text + _R)
         next_is_defname = False
      elif kind == 'bracket':
         parts.append(_BWH + text + _R)
         next_is_defname = False
      elif kind == 'word':
         if next_is_defname:
            parts.append(_BCY + text + _R)
            next_is_defname = False
         elif text in _PY_KEYWORDS:
            parts.append(_BBL + text + _R)
            next_is_defname = text in _PY_DEF_STARTERS
         else:
            parts.append(text)
            next_is_defname = False
      else:
         parts.append(text)
         next_is_defname = False
      pos = end
   if pos < len(code):
      parts.append(code[pos:])
   return ''.join(parts)


def highlight(code: str, lang: str) -> str:
   """Dispatch to the appropriate highlighter by language name.
   Supported lang values: 'python', 'lisp'.  Unknown languages pass through unchanged."""
   lang = lang.lower()
   if lang == 'python':
      return highlight_python(code)
   if lang in ('lisp', 'commonlisp', 'common-lisp'):
      return highlight_lisp(code)
   return code


# ---------------------------------------------------------------------------
# Markdown renderer
# ---------------------------------------------------------------------------

_INLINE_RE = re.compile(r'\*\*(.+?)\*\*|``(.+?)``|`([^`]+)`')


def _render_inline(line: str) -> str:
   """Apply inline Markdown formatting: **bold**, ``double-backtick code``, and `code`."""
   def _replace(m):
      if m.group(1) is not None:
         return _BLD + m.group(1) + _R
      content = m.group(2) if m.group(2) is not None else m.group(3)
      return _ICY + content + _R
   return _INLINE_RE.sub(_replace, line)


def render_markdown(text: str, use_color: bool = True) -> str:
   """Render a Markdown string for terminal display.

   Handles: # headings, **bold**, `inline code`, fenced code blocks
   (tagged with 'python' or 'lisp').

   When use_color is False, fence markers are stripped and all other
   text is passed through as-is (raw Markdown is readable as plain text)."""
   lines         = text.split('\n')
   output        = []
   in_code_block = False
   code_lines    = []
   code_lang     = ''

   for line in lines:
      if in_code_block:
         if line.strip() == '```':
            code = '\n'.join(code_lines)
            output.append(highlight(code, code_lang) if use_color else code)
            output.append('')
            in_code_block = False
            code_lines    = []
            code_lang     = ''
         else:
            code_lines.append(line)
      elif line.startswith('```'):
         code_lang     = line[3:].strip()
         in_code_block = True
      elif use_color and line.startswith('# '):
         output.append(_UND + line[2:] + _R)
      elif use_color and line.startswith('## '):
         output.append(_BLD + line[3:] + _R)
      elif use_color and line.startswith('### '):
         output.append(_BBL + line[4:] + _R)
      else:
         output.append(_render_inline(line) if use_color else line)

   if in_code_block:
      # Unclosed code block — flush whatever was accumulated
      code = '\n'.join(code_lines)
      output.append(highlight(code, code_lang) if use_color else code)

   return '\n'.join(output)
