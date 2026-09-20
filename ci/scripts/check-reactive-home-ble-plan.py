#!/usr/bin/env python3
"""Check the application's transitive BLE dependencies, not all local packages."""

import argparse
import json
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("--backend", choices=("bluez", "simpleble"), required=True)
parser.add_argument("--plan", type=Path, default=Path("dist-newstyle/cache/plan.json"))
args = parser.parse_args()
units = {unit["id"]: unit for unit in json.loads(args.plan.read_text())["install-plan"]}
pending = [key for key, unit in units.items() if unit.get("pkg-name") == "reactive-home"]
if not pending:
    raise SystemExit("reactive-home is missing from the Cabal plan")
seen = set()
while pending:
    key = pending.pop()
    if key not in seen:
        seen.add(key)
        pending.extend(units[key].get("depends", []))
names = {units[key]["pkg-name"] for key in seen}
required = {"switchbot-core", "switchbot-" + args.backend}
forbidden = {"switchbot-simpleble", "simpleble-hs"} if args.backend == "bluez" else {"switchbot-bluez"}
if not required <= names or forbidden & names:
    raise SystemExit(f"Unexpected BLE dependencies: missing {sorted(required - names)}, forbidden {sorted(forbidden & names)}")
print(f"reactive-home uses {args.backend}; its dependency closure excludes the other backend")
