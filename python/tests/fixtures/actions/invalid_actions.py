"""Invalid action module - missing register_actions function."""

# This module intentionally does NOT have a register_actions function
# to test error handling when the contract is violated.


def some_other_function():
    """This is not the required function."""
    return {"error": "This should not be called"}


MY_CONSTANT = "This module has no register_actions"
