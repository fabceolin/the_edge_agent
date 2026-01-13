# TEA Cross-Platform Wrapper

This is an Actually Portable Executable (APE) wrapper for The Edge Agent Docker container.

## What is this?

The `tea.com` binary is a single executable that runs on:
- Linux (x86_64, aarch64)
- macOS (x86_64, arm64)
- Windows (x86_64)
- FreeBSD, NetBSD, OpenBSD

It provides a native CLI experience while internally invoking the TEA Docker container.

## Building

```bash
# Download Cosmopolitan toolchain and build
make download
make

# Test the build
make test
```

## Usage

```bash
# Run a TEA agent
./tea.com run agent.yaml --input '{"query": "test"}'

# Validate a workflow
./tea.com validate workflow.yaml

# Show TEA version (from container)
./tea.com --version

# Wrapper-specific commands
./tea.com --wrapper-version    # Show wrapper version
./tea.com --docker-pull        # Pull latest image
./tea.com --docker-image       # Show current image name
./tea.com --wrapper-help       # Show wrapper help
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `TEA_IMAGE` | Docker image name | `ghcr.io/fabceolin/tea` |
| `TEA_VERSION` | Image tag | `latest` |
| `OPENAI_API_KEY` | Passed to container | - |
| `ANTHROPIC_API_KEY` | Passed to container | - |
| (other API keys) | Passed to container | - |

## How it Works

1. User runs `tea.com run agent.yaml`
2. Wrapper detects current directory and config path
3. Wrapper builds Docker command with volume mounts
4. Docker container executes the TEA command
5. Exit code is propagated back to user

## Requirements

- Docker installed and running
- Permission to run Docker commands

## Windows Notes

On Windows, rename `tea.com` to `tea.exe` for easier command-line use:

```cmd
ren tea.com tea.exe
tea --version
```

## Technical Details

Built with [Cosmopolitan Libc](https://github.com/jart/cosmopolitan), which creates
Actually Portable Executables that run on multiple operating systems without recompilation.

The wrapper is ~1.1MB and has no runtime dependencies other than Docker.
