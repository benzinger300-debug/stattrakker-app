#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# Stattrakker data backup.
# Copies the live data file (athletes.rds) to a timestamped, retained archive.
# Designed to run on a schedule via cron (see install command in chat).
# Keeps the 200 most recent snapshots, then prunes the oldest.
# ─────────────────────────────────────────────────────────────────────────────
set -u
SRC="/srv/shiny-server/own-it/data/athletes.rds"
DEST="/root/stattrakker-backups"
KEEP=200

mkdir -p "$DEST"
TS=$(date +%Y%m%d-%H%M%S)

if [ -f "$SRC" ]; then
  cp "$SRC" "$DEST/athletes-$TS.rds"
  # prune: keep only the $KEEP most recent snapshots
  ls -1t "$DEST"/athletes-*.rds 2>/dev/null | tail -n +$((KEEP+1)) | xargs -r rm -f
  echo "$(date '+%F %T')  OK   athletes-$TS.rds  ($(du -h "$SRC" | cut -f1))" >> "$DEST/backup.log"
else
  echo "$(date '+%F %T')  WARN source not found: $SRC" >> "$DEST/backup.log"
fi
