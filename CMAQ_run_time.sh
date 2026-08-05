#!/bin/bash

# Define the .out file and task name
OUT_FILE="task_output.out"  # Replace with your actual .out file path
TASK_NAME="TaskX"           # Replace with the task name you're monitoring
MAX_TIME=30                 # Maximum allowed time in seconds
NODES=("node1" "node2" "node3") # List of available nodes
EXCLUDED_NODES=()           # List to keep track of excluded nodes

# Function to run the main job with a subset of nodes
run_job() {
    local nodes=("$@")
    echo "Running job with nodes: ${nodes[*]}"
    
    # Example of passing nodes to your job script
    mpirun -np ${#nodes[@]} -host ${nodes[*]} ./your_main_script.sh
}

# Function to check task time
check_task_time() {
    # Extract the time from the .out file for the given task
    local task_time=$(grep "$TASK_NAME" "$OUT_FILE" | awk '{print $2}') # Adjust grep/awk as needed
    
    # If task_time exceeds MAX_TIME, return 1 (failure), else return 0 (success)
    if [[ "$task_time" -gt "$MAX_TIME" ]]; then
        return 1
    else
        return 0
    fi
}

# Main execution loop
while [[ ${#NODES[@]} -gt 0 ]]; do
    # Run the job
    run_job "${NODES[@]}"
    
    # Wait for job to complete and generate the .out file
    wait
    
    # Check task time
    if check_task_time; then
        echo "Task completed within allowed time."
        break
    else
        echo "Task exceeded allowed time. Excluding one node."
        
        # Remove the last node from the list
        EXCLUDED_NODES+=("${NODES[-1]}")
        unset 'NODES[-1]'
    fi
done

# Final check for available nodes
if [[ ${#NODES[@]} -eq 0 ]]; then
    echo "All nodes excluded. Job cannot proceed."
else
    echo "Job completed successfully with nodes: ${NODES[*]}"
fi
