# Assuming StateGraph is imported from the appropriate module
import the_edge_agent as tea

def run_fan_out_fan_in_example():
    """
    This example demonstrates the fan-out and fan-in execution pattern using the StateGraph class.

    Fan-out and fan-in patterns are widely used in parallel processing and workflow orchestration.
    In the fan-out phase, the workflow branches out into multiple parallel paths, where each path
    performs independent processing. After the parallel tasks are completed, the workflow fans
    back in at a designated point, where the results of the parallel tasks are aggregated or synchronized.

    In this example, we will simulate a workflow that branches into three parallel execution flows,
    each performing simple arithmetic operations on an initial 'value'. The parallel flows increment
    the value by different amounts (1, 2, and 3), and the fan-in point collects these results and sums
    them into a final result.

    ## Workflow structure:
       - The graph starts at the 'start' node.
       - From 'start', the graph fans out into three parallel flows:
         - 'flow1_start': Increments the 'value' by 1.
         - 'flow2_start': Increments the 'value' by 2.
         - 'flow3_start': Increments the 'value' by 3.
       - After the parallel flows finish, they fan back in at the 'fan_in' node.
       - The 'fan_in' node aggregates the results from all the parallel flows.
       - Finally, the workflow proceeds to the 'end' node, where the final result is printed.

    ## Node Definitions:
    1. **start_run**: Initializes the 'value' if not already set in the state.
    2. **flow1_start_run**: Increments the 'value' by 1 and returns the result in 'flow1_value'.
    3. **flow2_start_run**: Increments the 'value' by 2 and returns the result in 'flow2_value'.
    4. **flow3_start_run**: Increments the 'value' by 3 and returns the result in 'flow3_value'.
    5. **fan_in_run**: This node collects results from all the parallel flows ('flow1_value', 'flow2_value',
       and 'flow3_value'), sums them up, and stores the aggregated value in 'result'.
    6. **end_run**: The final node that prints the 'result' as the final output.

    ## StateGraph Configuration:
    1. **Nodes**: We define nodes for 'start', three parallel flows ('flow1_start', 'flow2_start', 'flow3_start'),
       a fan-in node ('fan_in'), and an 'end' node.
    2. **Edges**: The graph defines the relationships between the nodes. From 'start', parallel edges lead to
       the three flows, which eventually connect to the 'fan_in' node. After the fan-in node, a normal edge
       connects to the 'end' node.

    ## Fan-Out and Fan-In Logic:
    - The fan-out occurs when the graph transitions from the 'start' node to the three parallel nodes
      ('flow1_start', 'flow2_start', 'flow3_start').
    - Each of these parallel nodes executes in its own thread, processing the 'value' independently and
      returning their results.
    - The fan-in node ('fan_in') is responsible for waiting for all the parallel threads to finish. Once
      they complete, it collects their results from the shared state and computes a final aggregate value.
    - The result is passed to the 'end' node, which prints the final value.

    ## Example:
    - The graph starts with an initial state of {'value': 5}.
    - After fan-out, the three parallel nodes increment the 'value' as follows:
      - 'flow1_start' increments by 1 -> flow1_value = 6
      - 'flow2_start' increments by 2 -> flow2_value = 7
      - 'flow3_start' increments by 3 -> flow3_value = 8
    - The fan-in node collects these values and sums them: 6 + 7 + 8 = 21.
    - Finally, the 'end' node prints the total result: 21.

    ## Expected Output:
    Running this example should produce output like:

    ┌───────────────────┐
    │                   │
    │                   │
    │ parallel     ┌────┼───────────────────────────────────────────────┐
    ▼              │    │                                               ▼
  ┌─────────────┐  │  ┌─────────────┐  parallel   ┌─────────────┐     ┌────────┐     ┌─────┐
  │ flow3_start │ ─┘  │    START    │ ──────────▶ │ flow1_start │ ──▶ │ fan_in │ ──▶ │ END │
  └─────────────┘     └─────────────┘             └─────────────┘     └────────┘     └─────┘
                        │                                               ▲
                        │ parallel                                      │
                        ▼                                               │
                      ┌─────────────┐                                   │
                      │ flow2_start │ ──────────────────────────────────┘
                      └─────────────┘

    """
    # Define node functions
    def start_run(state, config, node, graph):
        # Initialize 'value' if not present
        state.setdefault('value', 0)
        print(f"start_run: Initialized 'value' = {state['value']}")
        return {}

    def flow1_start_run(state, config, node, graph):
        # Increment 'value' by 1
        new_value = state.get('value', 0) + 1
        print(f"flow1_start_run: Incremented 'value' by 1 -> flow1_value = {new_value}")
        return {'flow1_value': new_value}

    def flow2_start_run(state, config, node, graph):
        # Increment 'value' by 2
        new_value = state.get('value', 0) + 2
        print(f"flow2_start_run: Incremented 'value' by 2 -> flow2_value = {new_value}")
        return {'flow2_value': new_value}

    def fan_in_run(state, config, node, graph):
        # Collect results from all parallel flows
        parallel_results = state.get('parallel_results', [])
        total = sum(result.get('flow1_value', 0) for result in parallel_results) + \
                sum(result.get('flow2_value', 0) for result in parallel_results) 
        print(f"fan_in_run: Summed parallel results -> total = {total}")
        return {'result': total}

    def end_run(state, config, node, graph):
        # Finalize the result
        final_result = state.get('result', 0)
        print(f"end_run: Final result = {final_result}")
        return {'final_result': final_result}

    # Create the StateGraph instance
    state_graph = tea.StateGraph(state_schema={"value": int})

    # Add nodes
    state_graph.add_node("start", run=start_run)
    state_graph.add_node("flow1_start", run=run_fan_out_fan_in_example)
    state_graph.add_node("flow2_start", run=run_fan_out_fan_in_example)
    state_graph.add_fanin_node("fan_in", run=fan_in_run)
    state_graph.add_node("end", run=end_run)

    # Set entry and finish points
    state_graph.set_entry_point("start")
    state_graph.set_finish_point("end")

    # Add edges
    # From start to flow1, flow2, flow3 (parallel edges)
    state_graph.add_parallel_edge("start", "flow1_start", "fan_in")
    state_graph.add_parallel_edge("start", "flow2_start", "fan_in")
    state_graph.add_parallel_edge("start", "flow3_start", "fan_in")

    # From fan_in to end
    state_graph.add_edge("fan_in", "end")

    # Invoke the graph with an initial state
    initial_state = {'value': 5}
    print("Starting the fan-out and fan-in example with initial state:", initial_state)

    # Execute the graph
    execution = state_graph.invoke(initial_state)

    # Iterate through the generator to completion
    final_output = None
    for output in execution:
        if output['type'] == 'final':
            final_output = output

    # Print the final result
    if final_output:
        print(f"Final output: {final_output['state']['final_result']}")
    else:
        print("No final output was produced.")

if __name__ == "__main__":
    run_fan_out_fan_in_example()

