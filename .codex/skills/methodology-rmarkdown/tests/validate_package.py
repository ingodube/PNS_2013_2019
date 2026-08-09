#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import py_compile
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[1]


def main() -> int:
    skill = (ROOT / "SKILL.md").read_text("utf-8")
    if not skill.startswith("---\nname: methodology-rmarkdown\n"):
        raise SystemExit("invalid SKILL.md frontmatter")
    py_compile.compile(str(ROOT / "scripts" / "validate_report.py"), doraise=True)
    subprocess.run(
        [
            sys.executable,
            str(ROOT / "scripts" / "validate_report.py"),
            str(ROOT / "tests" / "fixture_report.html"),
            "--expected-tables", "1",
            "--expected-figures", "1",
            "--expected-code-blocks", "1",
            "--expected-references", "1",
            "--author-date",
            "--required-text", "Texto técnico.",
        ],
        check=True,
    )
    print("package_validation=PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
