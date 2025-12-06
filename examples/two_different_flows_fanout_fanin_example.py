import the_edge_agent as tea

def run_two_flows_fan_out_fan_in_example():
    """
This example demonstrates a two-flows fan-out and fan-in execution pattern using the StateGraph class.

Fan-out and fan-in patterns are essential in workflow orchestration for executing parallel tasks and
synchronizing their results. In this example, the workflow branches into two parallel flows: one
performs summation, and the other performs multiplication. After both flows complete, their results
are aggregated at the fan-in node to produce a final output.

## Workflow Structure:
   - START -> start -> [sum_flow, multiply_flow] -> fan_in -> end -> END

## Node Definitions:
1. **start_run**: Initializes the state with two numbers, 'a' and 'b'.
2. **sum_flow_run**: Calculates the sum of 'a' and 'b' and returns it as 'sum_result'.
3. **multiply_flow_run**: Calculates the product of 'a' and 'b' and returns it as 'multiply_result'.
4. **fan_in_run**: Collects 'sum_result' and 'multiply_result' from the parallel flows and aggregates them
   (e.g., adds the sum and product).
5. **end_run**: Finalizes the result by printing the aggregated value.

## StateGraph Configuration:
1. **Nodes**: Defines nodes for 'start', two parallel flows ('sum_flow', 'multiply_flow'),
   a fan-in node ('fan_in'), and an 'end' node.
2. **Edges**: Connects the nodes to form the workflow. From 'start', parallel edges lead to
   'sum_flow' and 'multiply_flow', which converge at 'fan_in'. Finally, the graph proceeds to the 'end' node.

## Execution Flow:
- The graph starts at the 'start' node with an initial state containing two numbers.
- It fans out into two parallel nodes:
  - 'sum_flow' calculates the sum of the numbers.
  - 'multiply_flow' calculates the product of the numbers.
- Both flows return their results to the 'fan_in' node.
- The 'fan_in' node aggregates the results (e.g., sum + product) and passes the final result to the 'end' node.
- The 'end' node prints the final aggregated result.

## Example:
- Initial State: {'a': 4, 'b': 5}
- sum_flow: 4 + 5 = 9
- multiply_flow: 4 * 5 = 20
- fan_in: 9 + 20 = 29
- end: Prints "Final aggregated result: 29"

## Expected Output:
Running this example should produce output like:
Starting the two-flows fan-out and fan-in example with initial state: {'a': 4, 'b': 5}
start_run: Initialized 'a' = 4, 'b' = 5
sum_flow_run: Calculated sum_result = 9
multiply_flow_run: Calculated multiply_result = 20
fan_in_run: parallel_results = [{'a': 4, 'b': 5, 'sum_result': 9}, {'a': 4, 'b': 5, 'multiply_result': 20}]
fan_in_run: Aggregated sum_result and multiply_result -> total = 29
end_run: Final aggregated result = 29

Final output: 29
## Key Concepts:
- **Fan-Out**: The process of branching out into multiple parallel execution paths.
- **Fan-In**: The process of synchronizing and aggregating results from multiple parallel execution paths.
- **State Management**: Ensures that each flow operates on a consistent and isolated copy of the state to prevent race conditions and ensure thread safety.

This example showcases how the `StateGraph` class can effectively manage parallel workflows and synchronize their outcomes, making it suitable for complex orchestration tasks.
"""


    # Define node functions
    def start_run(state, config, node, graph):
        # Initialize 'a' and 'b' in the state
        state.setdefault('a', 4)
        state.setdefault('b', 5)
        print(f"start_run: Initialized 'a' = {state['a']}, 'b' = {state['b']}")
        return {}

    def sum_flow_run(state, config, node, graph):
        # Sum 'a' and 'b'
        a = state.get('a', 0)
        b = state.get('b', 0)
        sum_result = a + b
        print(f"sum_flow_run: Calculated sum_result = {sum_result}")
        return {'sum_result': sum_result}

    def multiply_flow_run(state, config, node, graph):
        # Multiply 'a' and 'b'
        a = state.get('a', 0)
        b = state.get('b', 0)
        multiply_result = a * b
        print(f"multiply_flow_run: Calculated multiply_result = {multiply_result}")
        return {'multiply_result': multiply_result}

    def fan_in_run(state, config, node, graph):
        # Collect results from both parallel flows
        parallel_results = state.get('parallel_results', [])
        print(f"fan_in_run: parallel_results = {parallel_results}")
        total = 0
        for result in parallel_results:
            if 'sum_result' in result:
                total += result['sum_result']
            if 'multiply_result' in result:
                total += result['multiply_result']
        print(f"fan_in_run: Aggregated sum_result and multiply_result -> total = {total}")
        return {'result': total}

    def end_run(state, config, node, graph):
        # Finalize the result
        final_result = state.get('result', 0)
        print(f"end_run: Final aggregated result = {final_result}")
        return {'final_result': final_result}

    # Create the StateGraph instance
    state_graph = tea.StateGraph(state_schema={"a": int, "b": int})

    # Add nodes
    state_graph.add_node("start", run=start_run)
    state_graph.add_node("sum_flow", run=sum_flow_run)
    state_graph.add_node("multiply_flow", run=multiply_flow_run)
    state_graph.add_fanin_node("fan_in", run=fan_in_run)
    state_graph.add_node("end", run=end_run)

    # Set entry and finish points
    state_graph.set_entry_point("start")
    state_graph.set_finish_point("end")

    # Add edges
    # From start to sum_flow and multiply_flow (parallel edges)
    state_graph.add_parallel_edge("start", "sum_flow", "fan_in")
    state_graph.add_parallel_edge("start", "multiply_flow", "fan_in")

    # Add edges from sum_flow and multiply_flow to fan_in
    state_graph.add_edge("sum_flow", "fan_in")
    state_graph.add_edge("multiply_flow", "fan_in")

    # From fan_in to end
    state_graph.add_edge("fan_in", "end")

    # Invoke the graph with an initial state
    initial_state = {'a': 4, 'b': 5}
    print("Starting the two-flows fan-out and fan-in example with initial state:", initial_state)

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

run_two_flows_fan_out_fan_in_example()
