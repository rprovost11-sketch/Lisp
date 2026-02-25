# readlinetest.py
# Standalone test harness for readline integration.
# Runs on Windows (using readline_win) and Linux/macOS (using GNU readline).
# Usage: python pythonslisp/readlinetest.py

import os
import sys
import atexit

# ---------------------------------------------------------------------------
# Platform-specific readline setup
# ---------------------------------------------------------------------------

if sys.platform == 'win32':
    import readline_win as _rl
    def _prompt(p, continuation_prompt='... '):
        return _rl.input_line(p, continuation_prompt=continuation_prompt)
else:
    import readline as _rl
    _rl.set_auto_history(False)
    def _prompt(p, continuation_prompt='... '):
        return input(p)

add_history        = _rl.add_history
set_history_length = _rl.set_history_length
read_history_file  = _rl.read_history_file
write_history_file = _rl.write_history_file

# ---------------------------------------------------------------------------
# History persistence
# ---------------------------------------------------------------------------

_HIST_FILE = os.path.expanduser('~/.readline_test_history')

try:
    read_history_file(_HIST_FILE)
except Exception:
    pass

set_history_length(200)
atexit.register(write_history_file, _HIST_FILE)

# ---------------------------------------------------------------------------
# Test harness — accumulates lines until blank line, then submits as one entry
# ---------------------------------------------------------------------------

print('readline test harness — Ctrl-D or blank line to quit.')
print('Type expressions and use arrow keys to navigate history.')
print()

lines: list[str] = []

while True:
    prompt = '...  ' if lines else 'test> '
    try:
        line = _prompt(prompt, continuation_prompt='...  ')
    except KeyboardInterrupt:
        print()
        lines = []
        continue
    except EOFError:
        print()
        break

    if line == '' and not lines:
        break

    if line == '' and lines:
        entry = '\n'.join(lines)
        add_history(entry)
        print(f'  got: {repr(entry)}')
        lines = []
    else:
        lines.append(line)

print('Bye.')
