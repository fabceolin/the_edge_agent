import the_edge_agent as tea

# Define the graph
graph = tea.StateGraph({"value": int})

# Add nodes
graph.add_node("start", run=lambda state: {"value": state.get("value", 0) + 1})
graph.add_node("end", run=lambda state: {"result": f"Final value: {state['value']}"})

# Set entry and finish points
graph.set_entry_point("start")
graph.set_finish_point("end")

# Add edges
graph.add_edge("start", "end")

# Compile the graph with interrupts (requires checkpointer)
# Interrupts STOP execution and require explicit resume
checkpointer = tea.MemoryCheckpointer()
graph.compile(interrupt_before=["start"], interrupt_after=["end"], checkpointer=checkpointer)

# Execute the graph using stream (yields intermediate states and interrupts)
# With stop/resume behavior, we need to handle interrupts explicitly
checkpoint_path = None
input_state = {"value": 1}

while True:
    # Use checkpoint to resume if we have one, otherwise start fresh
    if checkpoint_path:
        stream = graph.stream(None, checkpoint=checkpoint_path)
    else:
        stream = graph.stream(input_state)

    for output in stream:
        if output["type"] == "state":
            print(f"Intermediate state at node {output['node']}: {output['state']}")
        elif output["type"].startswith("interrupt"):
            print(f"Interrupt at node {output['node']}: {output['state']}")
            # Save checkpoint path to resume later
            checkpoint_path = output.get("checkpoint_path")
        elif output["type"] == "final":
            print(f"Final state: {output['state']}")
            checkpoint_path = None  # Done, exit loop

    if checkpoint_path is None:
        break

