import openai
import the_edge_agent as tea
import logging

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Replace with your actual API key
openai.api_key = "replace me"


def llm_call(prompt):
    logging.debug(f"Making LLM call with prompt: {prompt}")
    try:
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[{"role": "user", "content": prompt}]
        )
        result = response.choices[0].message.content.strip()
        logging.debug(f"LLM response: {result}")
        return result
    except Exception as e:
        logging.error(f"Error in LLM call: {str(e)}")
        raise

def categorize_query(state):
    logging.debug("Entering categorize_query function")
    prompt = f"Categorize the following customer query into one of these categories: 'Technical Issue', 'Billing Question', 'Feature Request', or 'Other': \n\n{state['customer_query']}"
    category = llm_call(prompt)
    logging.debug(f"Query categorized as: {category}")
    return {"category": category}

def generate_response(state):
    logging.debug("Entering generate_response function")
    if state["category"] == "Technical Issue":
        prompt = f"Based your knowledge, generate a detailed troubleshooting guide for the following technical issue: \n\n{state['customer_query']}. Make it clear that is a AI response."
    elif state["category"] == "Billing Question":
        prompt = f"Provide a clear explanation for the following billing question: \n\n{state['customer_query']}. Make it clear that is a AI response."
    elif state["category"] == "Feature Request":
        prompt = f"Craft a response acknowledging the feature request and explaining the product roadmap process: \n\n{state['customer_query']}. Make it cleat that is a AI response."
    else:
        prompt = f"Generate a general response to the following query: \n\n{state['customer_query']}. Make it cleat that is a AI response."

    response = llm_call(prompt)
    logging.debug(f"Generated response: {response}")
    return {"response": response}

def determine_escalation(state):
    logging.debug("Entering determine_escalation function")
    prompt = f"Based on the customer query and our response, determine if this issue needs to be escalated to a human support agent. If it is a Billing support, you must need to Escalate to Human Agent and answer Yes. Respond with 'Yes' or 'No':\n\nCustomer Query: {state['customer_query']}\n\nOur Response: {state['response']}"
    escalation_decision = llm_call(prompt)
    logging.debug(f"Escalation decision: {escalation_decision}")
    return {"escalate": escalation_decision == "Yes"}

def human_escalation(state):
    logging.debug("Entering human_escalation function")
    print("\nHuman Escalation Required!")
    print(f"Customer Query: {state['customer_query']}")
    print(f"AI Response: {state['response']}")
    action = input("What action should be taken? (respond/transfer/close): ").strip().lower()
    if action == "respond":
        new_response = input("Enter new response: ")
        return {"response": new_response, "escalate": False}
    elif action == "transfer":
        return {"response": "Your query is being transferred to a specialist.", "escalate": True}
    else:
        return {"response": "Thank you for contacting us. Your case has been resolved.", "escalate": False}

def create_customer_support_graph():
    """

                                         ┌──────────────────────┐
                                         │      categorize      │
                                         └──────────────────────┘
                                           │
                                           │
                                           ▼
                                         ┌──────────────────────┐
                                         │  generate_response   │
                                         └──────────────────────┘
                                           │
                                           │
                                           ▼
┌──────────────────┐  escalate == True   ┌──────────────────────┐
│ human_escalation │ ◀────────────────── │ determine_escalation │ ◀┐
└──────────────────┘                     └──────────────────────┘  │
  │                                        │                       │
  │                                        │ escalate == False     │
  │                                        ▼                       │
  │                                      ╔══════════════════════╗  │
  │                                      ║         END          ║  │
  │                                      ╚══════════════════════╝  │
  │                                                                │
  └────────────────────────────────────────────────────────────────┘
    """
    logging.debug("Creating customer support graph")
    graph = tea.StateGraph({"customer_query": str, "category": str, "response": str, "escalate": bool})

    graph.add_node("categorize", run=categorize_query)
    graph.add_node("generate_response", run=generate_response)
    graph.add_node("determine_escalation", run=determine_escalation)
    graph.add_node("human_escalation", run=human_escalation)

    graph.set_entry_point("categorize")
    graph.add_edge("categorize", "generate_response")
    graph.add_edge("generate_response", "determine_escalation")
    graph.add_conditional_edges("determine_escalation", lambda state: state["escalate"], {True: "human_escalation", False: tea.END})
    graph.add_edge("human_escalation", "determine_escalation")

    # Interrupts require a checkpointer for stop/resume behavior
    checkpointer = tea.MemoryCheckpointer()
    return graph.compile(interrupt_before=["human_escalation"], checkpointer=checkpointer)

def print_graph_chart(graph):
    try:
        graph.save_graph_image("customer_support_workflow.png")
        print("Graph chart saved as 'customer_support_workflow.png'")
    except Exception as e:
        print(f"Error saving graph chart: {str(e)}")

def customer_support_workflow(customer_query):
    logging.info(f"Starting customer support workflow with query: {customer_query}")
    graph = create_customer_support_graph()
    initial_state = {"customer_query": customer_query}

    final_state = None
    checkpoint_path = None
    try:
        # With stop/resume behavior, we loop until completion
        while True:
            if checkpoint_path:
                stream = graph.stream(None, checkpoint=checkpoint_path)
            else:
                stream = graph.stream(initial_state)

            for event in stream:
                logging.debug(f"Current event: {event}")
                if isinstance(event, dict):
                    if "type" in event and event["type"] == "interrupt":
                        print("\nInterrupt detected. Human intervention required.")
                        # Execution stops here - save checkpoint for resume
                        checkpoint_path = event.get("checkpoint_path")
                        # In a real app, you'd wait for human input here
                        # For demo, we auto-resume
                        break
                    elif "type" in event and event["type"] == "final":
                        final_state = event["state"]
                        checkpoint_path = None  # Done
                        break
                    elif all(key in event for key in ["customer_query", "category", "response", "escalate"]):
                        final_state = event
                        checkpoint_path = None  # Done
                        break

            if checkpoint_path is None:
                break  # Completed or got final state

    except Exception as e:
        logging.error(f"Error during graph execution: {str(e)}")
        raise

    if final_state:
        logging.info("Workflow completed successfully")
        return {
            "category": final_state["category"],
            "response": final_state["response"],
            "escalate": final_state["escalate"]
        }
    else:
        logging.error("Workflow did not complete successfully")
        raise RuntimeError("Workflow did not complete successfully")

# Example usage
if __name__ == "__main__":
    customer_query = "I've been charged twice for my subscription this month. Can you help me understand why?"
    try:
        result = customer_support_workflow(customer_query)
        print(f"\nFinal Result:")
        print(f"Query Category: {result['category']}")
        print(f"Generated Response: {result['response']}")
        print(f"Escalate to Human Agent: {result['escalate']}")
    except Exception as e:
        logging.error(f"An error occurred: {str(e)}")
        print(f"An error occurred: {str(e)}")
