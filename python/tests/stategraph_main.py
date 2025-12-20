from stategraph import StateGraph, START, END
from langchain_community.chat_models import ChatPerplexity
from typing import Dict, Any

# Initialize the LLM model
llm = ChatPerplexity(model="llama-3.1-8b-instruct")

def wrap_node_action(action):
    def wrapped_action(state: Dict[str, Any]) -> Dict[str, Any]:
        # Execute the original action to get the updated state
        new_state = action(state)
        
        # Run the LLM with a "hello world" prompt
        model = ChatPerplexity(model_name="llama-3.1-8b-instruct", temperature=0.1)
        llm_response = model.invoke("Hello world, llama.")
        
        # Merge the new state with the LLM response
        new_state["llm_response"] = llm_response.content
        return new_state

    return wrapped_action

if __name__ == "__main__":
    # Define a simple state schema
    state_schema = {
        "review": int,
        "title_suggested": bool,
        "search_done": bool,
        "topic_sentence": bool,
        "paper_written": bool,
        "reflection_reviewed": bool,
        "instructions_given": bool,
        "abstract_written": bool,
        "references_generated": bool,
        "citations_generated": bool,
        "captions_generated": bool,
        "llm_response": str
    }

    builder = StateGraph(state_schema)

    def review_decision(state: Dict[str, Any]) -> str:
        return "review_more" if state.get("review", 0) < 2 else "next_phase"

    def manual_review(state: Dict[str, Any]) -> str:
        return "manual_review" if state.get("review", 0) < 2 else "finalize"

    # Add nodes with wrapped actions
    builder.add_node("suggest_title", wrap_node_action(lambda state: {"title_suggested": True}))
    builder.add_node("suggest_title_review", wrap_node_action(lambda state: {"review": state.get("review", 0) + 1}))
    builder.add_node("internet_search", wrap_node_action(lambda state: {"search_done": True}))
    builder.add_node("topic_sentence_writer", wrap_node_action(lambda state: {"topic_sentence": True}))
    builder.add_node("topic_sentence_manual_review", wrap_node_action(lambda state: {"review": state.get("review", 0) + 1}))
    builder.add_node("paper_writer", wrap_node_action(lambda state: {"paper_written": True}))
    builder.add_node("writer_manual_reviewer", wrap_node_action(lambda state: {"review": state.get("review", 0) + 1}))
    builder.add_node("reflection_reviewer", wrap_node_action(lambda state: {"reflection_reviewed": True}))
    builder.add_node("additional_reflection_instructions", wrap_node_action(lambda state: {"instructions_given": True}))
    builder.add_node("write_abstract", wrap_node_action(lambda state: {"abstract_written": True}))
    builder.add_node("generate_references", wrap_node_action(lambda state: {"references_generated": True}))
    builder.add_node("generate_citations", wrap_node_action(lambda state: {"citations_generated": True}))
    builder.add_node("generate_figure_captions", wrap_node_action(lambda state: {"captions_generated": True}))

    # Add conditional edges
    builder.add_conditional_edges(
        "suggest_title_review",
        review_decision,
        {"review_more": "suggest_title", "next_phase": "internet_search"}
    )
    builder.add_conditional_edges(
        "topic_sentence_manual_review",
        manual_review,
        {"manual_review": "topic_sentence_manual_review", "finalize": "paper_writer"}
    )
    builder.add_conditional_edges(
        "writer_manual_reviewer",
        manual_review,
        {"manual_review": "writer_manual_reviewer", "finalize": "write_abstract"}
    )

    # Add regular edges
    builder.add_edge("suggest_title", "suggest_title_review")
    builder.add_edge("internet_search", "topic_sentence_writer")
    builder.add_edge("topic_sentence_writer", "topic_sentence_manual_review")
    builder.add_edge("paper_writer", "writer_manual_reviewer")
    builder.add_edge("reflection_reviewer", "additional_reflection_instructions")
    builder.add_edge("additional_reflection_instructions", "paper_writer")
    builder.add_edge("write_abstract", "generate_references")
    builder.add_edge("generate_references", "generate_citations")
    builder.add_edge("generate_citations", "generate_figure_captions")
    builder.add_edge("write_abstract", "generate_figure_captions")

    builder.set_entry_point("suggest_title")
    builder.set_finish_point("generate_figure_captions")

    g = builder.compile(
        interrupt_before=[
            "suggest_title_review",
            "topic_sentence_manual_review",
            "writer_manual_reviewer",
            "additional_reflection_instructions",
            "generate_citations"
        ],
        interrupt_after=[]
    )

    # Initialize the state
    initial_state = {"review": 0}

    for event in g.stream(initial_state):
        if isinstance(event, dict) and event.get("type") == "interrupt":
            print(f"Interrupt before node: {event['node']}")
            print(f"Current state: {event['state']['values']}")
            print(f"LLM Response: {event['state']['values'].get('llm_response', 'No response')}")
            # Here you could add user interaction or other interrupt handling
        elif isinstance(event, dict):
            print(f"Current state: {event}")
            print(f"LLM Response: {event.get('llm_response', 'No response')}")
        else:
            print(f"Final state: {event}")
        print("---")

    g.render_graphviz().render("state_graph", format="png")
