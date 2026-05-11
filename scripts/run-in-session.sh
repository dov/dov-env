#!/bin/bash

run_in_tmux_session() {
  local SessionName="$1"
  local WorkingDir="$2"
  local CommandToRun="$3"

  # Create the session if it doesn't exist
  if ! tmux has-session -t "$SessionName" 2>/dev/null; then
    tmux new-session -d -s "$SessionName"
  fi

  # Prepare the pane: clear existing tasks, change directory, and run
  tmux send-keys -t "$SessionName" C-c
  tmux send-keys -t "$SessionName" "cd \"$WorkingDir\"" C-m
  tmux send-keys -t "$SessionName" "$CommandToRun" C-m
}

run_in_tmux_session "$1" "$2" "$3"

