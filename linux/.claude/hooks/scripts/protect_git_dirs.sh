#!/usr/bin/env bash
set -euo pipefail

payload="$(cat)"

decision="allow"
reason="No .git destructive pattern detected"

if PAYLOAD="$payload" python3 - <<'PY'; then
import json, re, os, sys

raw = os.environ.get("PAYLOAD", "")
text = raw
try:
    data = json.loads(raw)
    text = json.dumps(data, ensure_ascii=False)
except Exception:
    pass

patterns = [
    r"\brm\s+-rf\s+[^\n]*\.git(?:\b|/)",
    r"\brm\s+-r\s+[^\n]*\.git(?:\b|/)",
    r"\bfind\b[^\n]*-name\s+['\"]?\\?\.git['\"]?[^\n]*-delete\b",
    r"\bmv\s+[^\n]*\.git(?:\b|/)\s+",
    r"\brmdir\s+[^\n]*\.git(?:\b|/)",
    r"\bgit\s+clean\b[^\n]*-fdx\b",
]

for p in patterns:
    if re.search(p, text, flags=re.IGNORECASE):
        sys.exit(0)

sys.exit(1)
PY
  decision="ask"
  reason="Potential destructive operation involving .git detected. Explicit user approval required."
fi

cat <<JSON
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "${decision}",
    "permissionDecisionReason": "${reason}"
  }
}
JSON
