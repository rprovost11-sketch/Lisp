# readline_win.py
# Windows readline replacement using msvcrt.
# Module-level API mirrors the GNU readline module for drop-in compatibility.
# This module is Windows-only; importing it on other platforms will raise ImportError.

import os
import sys
import msvcrt

# Enable ANSI escape code processing in the Windows console.
os.system('')

# ---------------------------------------------------------------------------
# Key constants
# ---------------------------------------------------------------------------

_KEY_ENTER     = 'ENTER'
_KEY_BACKSPACE = 'BACKSPACE'
_KEY_DELETE    = 'DELETE'
_KEY_LEFT      = 'LEFT'
_KEY_RIGHT     = 'RIGHT'
_KEY_UP        = 'UP'
_KEY_DOWN      = 'DOWN'
_KEY_HOME      = 'HOME'
_KEY_END       = 'END'
_KEY_CTRL_C    = 'CTRL_C'
_KEY_EOF       = 'EOF'

# Second-byte map for \xe0-prefixed extended key sequences.
_EXTENDED_KEYS = {
    '\x48': _KEY_UP,
    '\x50': _KEY_DOWN,
    '\x4b': _KEY_LEFT,
    '\x4d': _KEY_RIGHT,
    '\x47': _KEY_HOME,
    '\x4f': _KEY_END,
    '\x53': _KEY_DELETE,
}

# ---------------------------------------------------------------------------
# Module state
# ---------------------------------------------------------------------------

_history:     list[str] = []
_history_max: int       = 500

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _read_key():
    """Read one logical key from the console.
    Returns a _KEY_* constant or a printable character string.
    Returns None for unrecognised escape sequences.
    """
    ch = msvcrt.getwch()
    if ch in ('\x00', '\xe0'):
        ch2 = msvcrt.getwch()
        return _EXTENDED_KEYS.get(ch2, None)
    if ch == '\r':
        return _KEY_ENTER
    if ch == '\x08':
        return _KEY_BACKSPACE
    if ch == '\x03':
        return _KEY_CTRL_C
    if ch in ('\x04', '\x1a'):
        return _KEY_EOF
    return ch

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def input_line(prompt: str = '', continuation_prompt: str = '... ') -> str:
    """Read one line of input with full line editing and history navigation.

    Multi-line history entries (containing newlines) are displayed across
    multiple lines using continuation_prompt for lines after the first.

    Raises KeyboardInterrupt on Ctrl-C.
    Raises EOFError on Ctrl-D or Ctrl-Z when the buffer is empty.
    """
    buf:          list = []
    cursor:       int  = 0
    hist_idx:     int  = -1   # -1 = editing current input; 0 = newest history entry
    hist_pending: str  = ''   # saves in-progress line while navigating history
    prev_extra:   int  = 0    # extra display lines drawn in the previous redraw

    def redraw() -> None:
        nonlocal prev_extra
        content = ''.join(buf)
        parts   = content.split('\n')
        extra   = len(parts) - 1

        # Move cursor up to the first line if the previous redraw drew multiple lines.
        if prev_extra > 0:
            sys.stdout.write(f'\033[{prev_extra}A')

        # Draw each line with the appropriate prompt.
        for i, part in enumerate(parts):
            pfx = prompt if i == 0 else continuation_prompt
            if i > 0:
                sys.stdout.write('\n')
            sys.stdout.write('\r' + pfx + part + '\033[K')

        # Clear any lines left over from a previous longer draw.
        leftover = prev_extra - extra
        if leftover > 0:
            for _ in range(leftover):
                sys.stdout.write('\n\r\033[K')
            sys.stdout.write(f'\033[{leftover}A')

        # Compute which display line and column the cursor belongs on.
        before       = content[:cursor]
        before_parts = before.split('\n')
        cursor_line  = len(before_parts) - 1
        cursor_col   = len(before_parts[-1])
        cursor_pfx   = prompt if cursor_line == 0 else continuation_prompt

        # Move up from the last drawn line to the cursor's line.
        lines_up = extra - cursor_line
        if lines_up > 0:
            sys.stdout.write(f'\033[{lines_up}A')

        # Position the cursor at the correct column.
        target_col = len(cursor_pfx) + cursor_col
        sys.stdout.write('\r')
        if target_col > 0:
            sys.stdout.write(f'\033[{target_col}C')

        sys.stdout.flush()
        prev_extra = extra

    def move_to_end_and_newline() -> None:
        """Position cursor at end of last display line then emit a newline."""
        content      = ''.join(buf)
        parts        = content.split('\n')
        extra        = len(parts) - 1
        before       = content[:cursor]
        before_parts = before.split('\n')
        cursor_line  = len(before_parts) - 1
        lines_down   = extra - cursor_line
        if lines_down > 0:
            sys.stdout.write(f'\033[{lines_down}B')
        sys.stdout.write('\n')
        sys.stdout.flush()

    redraw()

    while True:
        key = _read_key()

        if key is None:
            pass   # unrecognised escape sequence — ignore

        elif key == _KEY_ENTER:
            move_to_end_and_newline()
            return ''.join(buf)

        elif key == _KEY_CTRL_C:
            move_to_end_and_newline()
            raise KeyboardInterrupt

        elif key == _KEY_EOF:
            if not buf:
                move_to_end_and_newline()
                raise EOFError
            # Ctrl-D with content — ignore (matches GNU readline behaviour)

        elif key == _KEY_BACKSPACE:
            if cursor > 0:
                del buf[cursor - 1]
                cursor -= 1
                redraw()

        elif key == _KEY_DELETE:
            if cursor < len(buf):
                del buf[cursor]
                redraw()

        elif key == _KEY_LEFT:
            if cursor > 0:
                cursor -= 1
                redraw()

        elif key == _KEY_RIGHT:
            if cursor < len(buf):
                cursor += 1
                redraw()

        elif key == _KEY_HOME:
            cursor = 0
            redraw()

        elif key == _KEY_END:
            cursor = len(buf)
            redraw()

        elif key == _KEY_UP:
            if hist_idx == -1:
                hist_pending = ''.join(buf)
            next_idx = hist_idx + 1
            if next_idx < len(_history):
                hist_idx = next_idx
                buf      = list(_history[-(1 + hist_idx)])
                cursor   = len(buf)
                redraw()

        elif key == _KEY_DOWN:
            if hist_idx > 0:
                hist_idx -= 1
                buf      = list(_history[-(1 + hist_idx)])
                cursor   = len(buf)
                redraw()
            elif hist_idx == 0:
                hist_idx = -1
                buf      = list(hist_pending)
                cursor   = len(buf)
                redraw()

        elif isinstance(key, str) and key.isprintable():
            buf.insert(cursor, key)
            cursor += 1
            redraw()


def add_history(entry: str) -> None:
    """Append entry to history. Skips empty entries and exact duplicates of the most recent."""
    if not entry:
        return
    if _history and _history[-1] == entry:
        return
    _history.append(entry)
    if len(_history) > _history_max:
        _history.pop(0)


def set_history_length(n: int) -> None:
    """Set the maximum number of history entries retained."""
    global _history_max
    _history_max = n


def read_history_file(path: str) -> None:
    """Load history from file. Silently ignores a missing file.
    Embedded newlines are stored as the two-character sequence \\n and decoded on load.
    """
    try:
        with open(path, 'r', encoding='utf-8') as f:
            for raw in f:
                entry = raw.rstrip('\n').replace('\\n', '\n')
                if entry:
                    _history.append(entry)
    except FileNotFoundError:
        pass


def write_history_file(path: str) -> None:
    """Save history to file.
    Embedded newlines are encoded as the two-character sequence \\n.
    """
    entries = _history[-_history_max:]
    with open(path, 'w', encoding='utf-8') as f:
        for entry in entries:
            f.write(entry.replace('\n', '\\n') + '\n')

