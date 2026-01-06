"""
Authentication Actions for YAMLEngine.

Story: TEA-BUILTIN-015.3 (Auth Middleware in YAML)

Provides auth.verify and auth.get_user actions for explicit
authentication verification within agent workflows.

Example YAML:
    nodes:
      - name: verify_auth
        steps:
          - uses: auth.verify
            with:
              token: "{{ state.custom_token }}"
            output: auth_result

          - uses: auth.get_user
            with:
              uid: "{{ state.auth_result.user.uid }}"
            output: user_profile
"""

import asyncio
import logging
from typing import Any, Callable, Dict, Optional

logger = logging.getLogger(__name__)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register authentication actions in the action registry.

    Actions registered:
        - auth.verify: Verify a token and return user info
        - auth.get_user: Get full user profile by UID

    Args:
        registry: Action registry to populate
        engine: YAMLEngine instance for accessing auth middleware
    """

    def auth_verify(
        state: Dict[str, Any],
        token: Optional[str] = None,
        headers: Optional[Dict[str, str]] = None,
        provider: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Verify an authentication token.

        This action performs explicit token verification within a workflow.
        Useful when auth middleware is disabled (inject_user: false) or
        when verifying tokens from different sources.

        Args:
            state: Current workflow state
            token: Token to verify (if None, extracts from headers)
            headers: Request headers for token extraction
            provider: Provider type override (firebase, jwt, api_key)

        Returns:
            Dict with:
                - success: bool
                - user: UserInfo dict if success
                - error: str if failure

        Example YAML:
            - uses: auth.verify
              with:
                token: "{{ state.custom_token }}"
              output: auth_result

            - name: check_auth
              run: |
                if state["auth_result"]["success"]:
                    return {"user_id": state["auth_result"]["user"]["uid"]}
                else:
                    return {"error": state["auth_result"]["error"]}
        """
        # Check if auth middleware is configured
        auth_middleware = getattr(engine, "_auth_middleware", None)

        if auth_middleware is None:
            # Try to get provider from engine's auth settings
            auth_config = getattr(engine, "_auth_config", None)
            if auth_config is None:
                return {
                    "success": False,
                    "user": None,
                    "error": "Auth middleware not configured",
                }

            # Create provider for this call
            try:
                from ..auth import create_auth_provider

                auth_provider = create_auth_provider(auth_config)
            except Exception as e:
                return {
                    "success": False,
                    "user": None,
                    "error": f"Failed to create auth provider: {e}",
                }
        else:
            auth_provider = auth_middleware.provider

        if auth_provider is None:
            return {
                "success": False,
                "user": None,
                "error": "No auth provider configured",
            }

        # Extract token if not provided
        if token is None and headers:
            token_header = "Authorization"
            if auth_middleware:
                token_header = auth_middleware.settings.token_header

            token = auth_provider.extract_token(
                headers=headers or {},
                query_params={},
                token_header=token_header,
            )

        if token is None:
            return {
                "success": False,
                "user": None,
                "error": "No token provided",
            }

        # Verify token (run async)
        try:
            loop = asyncio.get_event_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)

        try:
            if loop.is_running():
                # Already in async context, create task
                import concurrent.futures

                with concurrent.futures.ThreadPoolExecutor() as pool:
                    result = pool.submit(
                        asyncio.run, auth_provider.verify_token(token)
                    ).result()
            else:
                result = loop.run_until_complete(auth_provider.verify_token(token))
        except Exception as e:
            return {
                "success": False,
                "user": None,
                "error": f"Verification failed: {e}",
            }

        if result.success:
            user_dict = result.user.to_state_dict() if result.user else None

            # Apply claims mapping if configured
            if auth_middleware and result.user:
                claims_mapping = auth_middleware.settings.claims_mapping
                if claims_mapping:
                    mapped_user = result.user.apply_claims_mapping(claims_mapping)
                    user_dict = mapped_user.to_state_dict()

            return {
                "success": True,
                "user": user_dict,
                "error": None,
            }
        else:
            return {
                "success": False,
                "user": None,
                "error": result.error,
            }

    def auth_get_user(
        state: Dict[str, Any],
        uid: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Get full user profile by UID.

        This action fetches the complete user profile from the auth provider.
        For Firebase, this retrieves user data from Firebase Auth.
        For JWT providers, this returns minimal info.

        Args:
            state: Current workflow state
            uid: User ID to look up (if None, uses __user__.uid from state)

        Returns:
            Dict with:
                - success: bool
                - user: Full UserInfo dict if success
                - error: str if failure

        Example YAML:
            - uses: auth.get_user
              with:
                uid: "{{ state.__user__.uid }}"
              output: full_profile

            - name: use_profile
              run: |
                profile = state["full_profile"]
                if profile["success"]:
                    return {"name": profile["user"]["name"]}
        """
        # Get UID from parameter or state
        if uid is None:
            user_state = state.get("__user__")
            if user_state and isinstance(user_state, dict):
                uid = user_state.get("uid")

        if uid is None:
            return {
                "success": False,
                "user": None,
                "error": "No user ID provided",
            }

        # Get auth provider
        auth_middleware = getattr(engine, "_auth_middleware", None)
        if auth_middleware and auth_middleware.provider:
            auth_provider = auth_middleware.provider
        else:
            # Try to create from config
            auth_config = getattr(engine, "_auth_config", None)
            if auth_config:
                try:
                    from ..auth import create_auth_provider

                    auth_provider = create_auth_provider(auth_config)
                except Exception as e:
                    return {
                        "success": False,
                        "user": None,
                        "error": f"Failed to create auth provider: {e}",
                    }
            else:
                return {
                    "success": False,
                    "user": None,
                    "error": "No auth provider configured",
                }

        if auth_provider is None:
            return {
                "success": False,
                "user": None,
                "error": "No auth provider available",
            }

        # Get user (run async)
        try:
            loop = asyncio.get_event_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)

        try:
            if loop.is_running():
                import concurrent.futures

                with concurrent.futures.ThreadPoolExecutor() as pool:
                    user = pool.submit(
                        asyncio.run, auth_provider.get_user(uid)
                    ).result()
            else:
                user = loop.run_until_complete(auth_provider.get_user(uid))
        except Exception as e:
            return {
                "success": False,
                "user": None,
                "error": f"Failed to get user: {e}",
            }

        if user:
            return {
                "success": True,
                "user": user.to_state_dict(),
                "error": None,
            }
        else:
            return {
                "success": False,
                "user": None,
                "error": f"User not found: {uid}",
            }

    # Register actions with both naming conventions
    registry["auth.verify"] = auth_verify
    registry["actions.auth_verify"] = auth_verify
    registry["auth.get_user"] = auth_get_user
    registry["actions.auth_get_user"] = auth_get_user
