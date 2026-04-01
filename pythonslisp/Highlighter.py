"""Syntax highlighting and Markdown rendering for terminal display.

Public API:
   highlight(code, lang)          - ANSI-color a code snippet by language
   render_markdown(text, use_color) - render a Markdown string for the terminal
"""
from __future__ import annotations
import re
import os
import sys

# ANSI escape codes
_R   = '\033[0m'      # reset
_DIM = '\033[2;37m'   # dim gray          - comments
_GRN = '\033[32m'     # green             - strings
_CYN = '\033[36m'     # cyan              - Lisp :keywords
_YEL = '\033[33m'     # yellow            - numbers; Python decorators
_BCY = '\033[1;36m'   # bold cyan         - Lisp special operators; Python def/class names
_BMA = '\033[1;35m'   # bold magenta      - Lisp definition forms
_BYE = '\033[1;33m'   # bold yellow       - T  NIL
_BBL = '\033[1;34m'   # bold blue         - Python keywords
_BWH = '\033[1;97m'   # bold bright white - parentheses and brackets
_BLD = '\033[1m'      # bold              - Markdown h2/h3 and **bold**
_UND = '\033[1;4m'    # bold + underline  - Markdown h1
_ICY = '\033[96m'     # bright cyan       - Markdown inline `code`
_ITA = '\033[3m'      # italic            - Markdown *italic*


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

_INLINE_RE = re.compile(
   r'\*\*(.+?)\*\*'                              # group 1 - **bold**
   r'|``(.+?)``'                                 # group 2 - ``code``
   r'|`([^`]+)`'                                 # group 3 - `code`
   r'|(?<!\*)\*(?!\*)([^*\n]+)(?<!\*)\*(?!\*)'  # group 4 - *italic*
)
_ANSI_RE = re.compile(r'\033\[[^m]*m')
_HR_RE   = re.compile(r'^(-{3,}|\*{3,}|_{3,})\s*$')
_UL_RE   = re.compile(r'^( *)([-*+]) (.+)')
_OL_RE   = re.compile(r'^( *)(\d+\.) (.+)')


def _render_inline(line: str) -> str:
   """Apply inline Markdown formatting: **bold**, *italic*, ``code``, and `code`."""
   def _replace(m):
      if m.group(1) is not None:
         return _BLD + m.group(1) + _R
      if m.group(4) is not None:
         return _ITA + m.group(4) + _R
      content = m.group(2) if m.group(2) is not None else m.group(3)
      return _ICY + content + _R
   return _INLINE_RE.sub(_replace, line)


def _visible_len(s: str) -> int:
   """Return the visible (printable) length of a string, ignoring ANSI escape codes."""
   return len(_ANSI_RE.sub('', s))


def _parse_table_row(line: str) -> list[str]:
   """Split a Markdown table row '| a | b | c |' into a list of cell strings."""
   s = line.strip()
   if s.startswith('|'):
      s = s[1:]
   if s.endswith('|'):
      s = s[:-1]
   return s.split('|')


def _is_separator_row(cells: list[str]) -> bool:
   """Return True if every non-empty cell matches the --- separator pattern."""
   return all(re.match(r'^:?-+:?$', c.strip()) for c in cells if c.strip())


def _render_table(rows: list[list[str]], use_color: bool) -> list[str]:
   """Render a list of parsed table rows as an aligned terminal table."""
   if len(rows) < 2 or not _is_separator_row(rows[1]):
      # Not a well-formed GFM table - pass through raw
      return ['|' + '|'.join(row) + '|' for row in rows]

   header = rows[0]
   data   = rows[2:]

   def render_cell(cell: str) -> str:
      return _render_inline(cell.strip()) if use_color else cell.strip()

   rendered_header = [render_cell(c) for c in header]
   rendered_data   = [[render_cell(c) for c in row] for row in data]

   num_cols = max(
      len(rendered_header),
      max((len(row) for row in rendered_data), default=0),
   )

   widths = [0] * num_cols
   for i, cell in enumerate(rendered_header):
      if i < num_cols:
         widths[i] = max(widths[i], _visible_len(cell))
   for row in rendered_data:
      for i, cell in enumerate(row):
         if i < num_cols:
            widths[i] = max(widths[i], _visible_len(cell))

   def pad_cell(cell: str, width: int) -> str:
      return cell + ' ' * (width - _visible_len(cell))

   def format_row(cells: list[str]) -> str:
      parts = []
      for i in range(num_cols):
         cell = cells[i] if i < len(cells) else ''
         parts.append(' ' + pad_cell(cell, widths[i]) + ' ')
      return '|' + '|'.join(parts) + '|'

   result = [format_row(rendered_header)]
   result.append('|' + '|'.join('-' * (w + 2) for w in widths) + '|')
   for row in rendered_data:
      result.append(format_row(row))
   return result


def render_markdown(text: str, use_color: bool = True) -> str:
   """Render a Markdown string for terminal display.

   Handles: headings (h1–h4), **bold**, *italic*, `inline code`, fenced code
   blocks (tagged with 'python' or 'lisp'), GFM pipe tables, horizontal rules,
   and unordered/ordered lists.

   When use_color is False, fence markers are stripped and all other
   text is passed through as-is (raw Markdown is readable as plain text)."""
   lines         = text.split('\n')
   output        = []
   in_code_block = False
   code_lines    = []
   code_lang     = ''
   table_rows: list[list[str]] = []

   def flush_table() -> None:
      if table_rows:
         output.extend(_render_table(table_rows, use_color))
         table_rows.clear()

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
         flush_table()
         code_lang     = line[3:].strip()
         in_code_block = True
      elif line.startswith('|'):
         table_rows.append(_parse_table_row(line))
      else:
         flush_table()
         if _HR_RE.match(line):
            output.append('-' * 78)
         elif use_color and line.startswith('# '):
            output.append(_UND + line[2:] + _R)
         elif use_color and line.startswith('## '):
            output.append(_BLD + line[3:] + _R)
         elif use_color and line.startswith('### '):
            output.append(_BBL + line[4:] + _R)
         elif use_color and line.startswith('#### '):
            output.append(_BLD + line[5:] + _R)
         elif m := _UL_RE.match(line):
            indent, content = m.group(1), m.group(3)
            rendered = _render_inline(content) if use_color else content
            output.append(indent + '- ' + rendered)
         elif m := _OL_RE.match(line):
            indent, num, content = m.group(1), m.group(2), m.group(3)
            rendered = _render_inline(content) if use_color else content
            output.append(indent + num + ' ' + rendered)
         else:
            output.append(_render_inline(line) if use_color else line)

   flush_table()

   if in_code_block:
      # Unclosed code block - flush whatever was accumulated
      code = '\n'.join(code_lines)
      output.append(highlight(code, code_lang) if use_color else code)

   return '\n'.join(output)
