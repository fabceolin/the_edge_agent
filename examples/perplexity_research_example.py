import os
from perplexity import Perplexity
import the_edge_agent as tea
import logging

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Initialize Perplexity client
# API key should be set in PERPLEXITY_API_KEY environment variable
# or in a .env file: PERPLEXITY_API_KEY="your-api-key"
client = Perplexity(api_key=os.getenv("PERPLEXITY_API_KEY"))


def ask_user_query(state):
    """
    First node: Ask the user what they want to research
    """
    logging.info("Asking user for search query")
    print("\n" + "="*60)
    print("PERPLEXITY DEEP RESEARCH TOOL")
    print("="*60)

    query = input("\nWhat would you like to research? ")

    if not query.strip():
        query = "latest developments in artificial intelligence"
        print(f"No query provided. Using default: {query}")

    logging.info(f"User query: {query}")
    return {"query": query}


def perform_search(state):
    """
    Second node: Perform the Perplexity search
    """
    logging.info(f"Performing Perplexity search for: {state['query']}")
    print(f"\nSearching Perplexity for: '{state['query']}'...")

    try:
        # Use Perplexity's chat completion with sonar model for deep research
        response = client.chat.completions.create(
            messages=[
                {
                    "role": "system",
                    "content": "You are a helpful research assistant. Provide comprehensive, well-researched answers with relevant sources and citations."
                },
                {
                    "role": "user",
                    "content": f"Conduct deep research on the following topic and provide a detailed answer: {state['query']}"
                }
            ],
            model="sonar",
        )

        # Extract the response content
        research_result = response.choices[0].message.content

        # Also perform a web search to get specific sources
        search_results = client.search.create(
            query=state['query'],
            max_results=5
        )

        # Collect source URLs
        sources = []
        for result in search_results.results:
            sources.append({
                "title": result.title,
                "url": result.url
            })

        logging.info("Search completed successfully")
        return {
            "research_result": research_result,
            "sources": sources
        }

    except Exception as e:
        logging.error(f"Error during Perplexity search: {str(e)}")
        return {
            "research_result": f"Error occurred during search: {str(e)}",
            "sources": []
        }


def display_results(state):
    """
    Third node: Display the research results
    """
    logging.info("Displaying results")
    print("\n" + "="*60)
    print("RESEARCH RESULTS")
    print("="*60)

    print(f"\nQuery: {state['query']}")
    print("\n" + "-"*60)
    print("DETAILED ANSWER:")
    print("-"*60)
    print(state['research_result'])

    if state['sources']:
        print("\n" + "-"*60)
        print("SOURCES:")
        print("-"*60)
        for idx, source in enumerate(state['sources'], 1):
            print(f"\n{idx}. {source['title']}")
            print(f"   URL: {source['url']}")

    print("\n" + "="*60)
    print("RESEARCH COMPLETE")
    print("="*60 + "\n")

    return {"status": "completed"}


def create_perplexity_research_graph():
    """
    Create the state graph for Perplexity deep research workflow

    Flow:
        ┌─────────────────┐
        │  ask_user_query │
        └─────────────────┘
                │
                │
                ▼
        ┌─────────────────┐
        │ perform_search  │
        └─────────────────┘
                │
                │
                ▼
        ┌─────────────────┐
        │ display_results │
        └─────────────────┘
                │
                │
                ▼
            ╔═══════╗
            ║  END  ║
            ╚═══════╝
    """
    logging.debug("Creating Perplexity research graph")

    # Define state schema
    graph = tea.StateGraph({
        "query": str,
        "research_result": str,
        "sources": list,
        "status": str
    })

    # Add nodes
    graph.add_node("ask_user_query", run=ask_user_query)
    graph.add_node("perform_search", run=perform_search)
    graph.add_node("display_results", run=display_results)

    # Define the flow
    graph.set_entry_point("ask_user_query")
    graph.add_edge("ask_user_query", "perform_search")
    graph.add_edge("perform_search", "display_results")
    graph.set_finish_point("display_results")

    return graph.compile()


def save_graph_visualization(graph):
    """
    Save the graph visualization
    """
    try:
        graph.save_graph_image("examples/perplexity_research_graph.png")
        print("Graph visualization saved as 'perplexity_research_graph.png'")
    except Exception as e:
        print(f"Warning: Could not save graph visualization: {str(e)}")


def main():
    """
    Main function to run the Perplexity research workflow
    """
    logging.info("Starting Perplexity deep research workflow")

    # Create and compile the graph
    graph = create_perplexity_research_graph()

    # Save graph visualization
    save_graph_visualization(graph)

    # Initialize with empty state (user input will be collected in first node)
    initial_state = {
        "query": "",
        "research_result": "",
        "sources": [],
        "status": "pending"
    }

    try:
        # Execute the graph
        results = list(graph.invoke(initial_state))
        logging.info("Workflow completed successfully")

    except KeyboardInterrupt:
        print("\n\nWorkflow interrupted by user.")
        logging.info("Workflow interrupted by user")
    except Exception as e:
        logging.error(f"Error during workflow execution: {str(e)}")
        print(f"\nAn error occurred: {str(e)}")


if __name__ == "__main__":
    main()
