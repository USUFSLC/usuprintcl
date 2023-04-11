#!/bin/bash
# Thanks ChatGPT

# Specify the directory containing your Lisp files
LISP_DIR="."

# Command to start your process
PROCESS_CMD="sbcl --load run.lisp"

# Debounce interval in seconds
DEBOUNCE_INTERVAL=3

# Function to restart the process
restart_process() {
    if [ ! -z "$PROCESS_PID" ]; then
        kill "$PROCESS_PID"
    fi

    $PROCESS_CMD &
    PROCESS_PID=$!
}

# Start the process for the first time
restart_process

# Initialize the debounce timer
DEBOUNCE_TIMER=0

# Watch for changes in .lisp files
inotifywait -m -r -e modify --format '%w%f' -e create --format '%w%f' "${LISP_DIR}" | while read FILE
do
    if [[ "$FILE" =~ .*\.lisp$ ]]; then
        CUR_TIME=$(date +%s)

        # Check if debounce interval has passed since the last restart
        if (( CUR_TIME - DEBOUNCE_TIMER >= DEBOUNCE_INTERVAL )); then
            echo "Detected change in $FILE, restarting process..."
            restart_process
            DEBOUNCE_TIMER=$CUR_TIME
        fi
    fi
done
