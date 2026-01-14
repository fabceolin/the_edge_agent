/* tea.c - Cross-Platform Docker Wrapper for The Edge Agent
 *
 * TEA-DIST-001: Docker Distribution with Cross-Platform Wrapper
 *
 * This is an Actually Portable Executable (APE) wrapper that invokes
 * the TEA Docker container on any platform (Linux, macOS, Windows, BSD).
 *
 * Build with Cosmopolitan Libc:
 *   ./cosmocc/bin/cosmocc -Os -o tea.com tea.c
 *
 * License: MIT
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* Standard includes - Cosmopolitan provides POSIX-compatible headers */
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifndef _WIN32
#include <sys/wait.h>
#endif

/* Constants */
#define DEFAULT_IMAGE "ghcr.io/fabceolin/tea"
#define DEFAULT_VERSION "latest"
#define WRAPPER_VERSION "0.2.0"
#define MAX_ARGS 256
#define DEFAULT_GPU "auto"
#define MAX_ENV_VARS 64
#define PATH_MAX_SIZE 4096
#define CMD_MAX_SIZE 8192

/* Environment variables to pass through to container */
static const char *ENV_PASSTHROUGH[] = {
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY",
    "GEMINI_API_KEY",
    "GROQ_API_KEY",
    "OLLAMA_HOST",
    "NEO4J_URI",
    "NEO4J_USER",
    "NEO4J_PASSWORD",
    "FIREBASE_PROJECT_ID",
    "GOOGLE_APPLICATION_CREDENTIALS",
    "OPIK_API_KEY",
    "OPIK_PROJECT_NAME",
    NULL
};

/* Helper: Get environment variable with default */
static const char *get_env_or(const char *name, const char *def) {
    const char *val = getenv(name);
    return val ? val : def;
}

/* GPU vendor types */
typedef enum {
    GPU_NONE = 0,
    GPU_NVIDIA,
    GPU_AMD,
    GPU_INTEL
} gpu_vendor_t;

/* Helper: Run command and check exit code */
static int cmd_succeeds(const char *cmd) {
    int result = system(cmd);
#ifdef _WIN32
    return result == 0;
#else
    return WIFEXITED(result) && WEXITSTATUS(result) == 0;
#endif
}

/* Helper: Check if NVIDIA GPU runtime is available */
static int has_nvidia_gpu(void) {
    return cmd_succeeds("nvidia-smi >/dev/null 2>&1");
}

/* Helper: Check if AMD ROCm GPU is available */
static int has_amd_gpu(void) {
#ifdef _WIN32
    return 0;  /* ROCm not supported on Windows */
#else
    /* Check for ROCm device and rocm-smi */
    struct stat st;
    if (stat("/dev/kfd", &st) == 0) {
        return 1;
    }
    return cmd_succeeds("rocm-smi >/dev/null 2>&1");
#endif
}

/* Helper: Check if Intel/Vulkan GPU is available via DRI */
static int has_intel_gpu(void) {
#ifdef _WIN32
    return 0;  /* DRI not available on Windows */
#else
    struct stat st;
    return stat("/dev/dri", &st) == 0;
#endif
}

/* Helper: Detect GPU vendor automatically */
static gpu_vendor_t detect_gpu_vendor(void) {
    /* Priority: NVIDIA > AMD > Intel/Vulkan */
    if (has_nvidia_gpu()) {
        return GPU_NVIDIA;
    }
    if (has_amd_gpu()) {
        return GPU_AMD;
    }
    if (has_intel_gpu()) {
        return GPU_INTEL;
    }
    return GPU_NONE;
}

/* Helper: Get GPU vendor from TEA_GPU environment variable */
static gpu_vendor_t get_gpu_vendor_from_env(const char *gpu_env) {
    if (strcmp(gpu_env, "nvidia") == 0 || strcmp(gpu_env, "all") == 0) {
        return GPU_NVIDIA;
    }
    if (strcmp(gpu_env, "amd") == 0 || strcmp(gpu_env, "rocm") == 0) {
        return GPU_AMD;
    }
    if (strcmp(gpu_env, "intel") == 0 || strcmp(gpu_env, "vulkan") == 0 || strcmp(gpu_env, "dri") == 0) {
        return GPU_INTEL;
    }
    if (strcmp(gpu_env, "none") == 0 || strcmp(gpu_env, "off") == 0) {
        return GPU_NONE;
    }
    /* Numeric value (e.g., "1", "2") implies NVIDIA */
    if (gpu_env[0] >= '0' && gpu_env[0] <= '9') {
        return GPU_NVIDIA;
    }
    return GPU_NONE;
}

/* Helper: Get GPU configuration */
static gpu_vendor_t get_gpu_config(const char **gpu_param) {
    const char *gpu_env = get_env_or("TEA_GPU", DEFAULT_GPU);

    /* "auto" mode: detect GPU vendor automatically */
    if (strcmp(gpu_env, "auto") == 0) {
        gpu_vendor_t vendor = detect_gpu_vendor();
        if (vendor == GPU_NVIDIA) {
            *gpu_param = "all";
        }
        return vendor;
    }

    /* Store specific param for NVIDIA (e.g., "1", "2", "all") */
    if (gpu_env[0] >= '0' && gpu_env[0] <= '9') {
        *gpu_param = gpu_env;
        return GPU_NVIDIA;
    }
    if (strcmp(gpu_env, "all") == 0) {
        *gpu_param = "all";
        return GPU_NVIDIA;
    }

    return get_gpu_vendor_from_env(gpu_env);
}

/* Helper: Check if stdout is a TTY */
static int is_tty(void) {
#ifdef _WIN32
    return _isatty(_fileno(stdout)) && _isatty(_fileno(stdin));
#else
    return isatty(STDOUT_FILENO) && isatty(STDIN_FILENO);
#endif
}

/* Helper: Get the full Docker image name */
static void get_image(char *buf, size_t bufsize) {
    const char *image = get_env_or("TEA_IMAGE", DEFAULT_IMAGE);
    const char *version = get_env_or("TEA_VERSION", DEFAULT_VERSION);
    snprintf(buf, bufsize, "%s:%s", image, version);
}

/* Helper: Get config directory based on platform */
static void get_config_dir(char *buf, size_t bufsize) {
    const char *home = getenv("HOME");
    const char *userprofile = getenv("USERPROFILE");
    const char *xdg_config = getenv("XDG_CONFIG_HOME");

    if (xdg_config && *xdg_config) {
        snprintf(buf, bufsize, "%s/tea", xdg_config);
    } else if (home && *home) {
        snprintf(buf, bufsize, "%s/.tea", home);
    } else if (userprofile && *userprofile) {
        snprintf(buf, bufsize, "%s\\.tea", userprofile);
    } else {
        snprintf(buf, bufsize, ".tea");
    }
}

/* Helper: Ensure directory exists */
static void ensure_dir(const char *path) {
#ifdef _WIN32
    _mkdir(path);
#else
    mkdir(path, 0755);
#endif
}

/* Helper: Get current working directory */
static int get_cwd(char *buf, size_t bufsize) {
    if (getcwd(buf, bufsize) == NULL) {
        return -1;
    }
    return 0;
}

/* Show wrapper version */
static void show_version(void) {
    printf("tea-wrapper %s (cosmopolitan)\n", WRAPPER_VERSION);
}

/* Show current Docker image */
static void show_image(void) {
    char image[256];
    get_image(image, sizeof(image));
    printf("%s\n", image);
}

/* Pull the Docker image */
static int pull_image(void) {
    char image[256];
    char cmd[CMD_MAX_SIZE];

    get_image(image, sizeof(image));
    printf("Pulling %s...\n", image);

    snprintf(cmd, sizeof(cmd), "docker pull %s", image);
    int result = system(cmd);

    if (result == 0) {
        printf("Successfully pulled %s\n", image);
    } else {
        fprintf(stderr, "Failed to pull image (exit code: %d)\n", result);
    }

    return result;
}

/* Run the Docker container with provided arguments */
static int run_docker(int argc, char *argv[]) {
    char image[256];
    char cwd[PATH_MAX_SIZE];
    char config_dir[PATH_MAX_SIZE];
    char cmd[CMD_MAX_SIZE];
    char *p = cmd;
    char *end = cmd + sizeof(cmd);
    int i;
    const char *gpu_param = NULL;
    gpu_vendor_t gpu_vendor;

    /* Get image name */
    get_image(image, sizeof(image));

    /* Get GPU configuration */
    gpu_vendor = get_gpu_config(&gpu_param);

    /* Get current working directory */
    if (get_cwd(cwd, sizeof(cwd)) != 0) {
        fprintf(stderr, "Error: Failed to get current directory: %s\n", strerror(errno));
        return 127;
    }

    /* Get and create config directory */
    get_config_dir(config_dir, sizeof(config_dir));
    ensure_dir(config_dir);

    /* Start building docker command */
    p += snprintf(p, end - p, "docker run --rm");

    /* GPU support - add appropriate flags based on vendor */
    switch (gpu_vendor) {
        case GPU_NVIDIA:
            /* NVIDIA Container Toolkit: --gpus all (or specific count) */
            p += snprintf(p, end - p, " --gpus %s", gpu_param ? gpu_param : "all");
            break;
        case GPU_AMD:
            /* AMD ROCm: expose KFD and DRI devices */
            p += snprintf(p, end - p, " --device=/dev/kfd --device=/dev/dri --group-add video");
            break;
        case GPU_INTEL:
            /* Intel/Vulkan: expose DRI for GPU access */
            p += snprintf(p, end - p, " --device=/dev/dri");
            break;
        case GPU_NONE:
        default:
            /* No GPU support */
            break;
    }

    /* Volume mounts */
    p += snprintf(p, end - p, " -v \"%s:/work\"", cwd);
    p += snprintf(p, end - p, " -v \"%s:/home/tea/.tea\"", config_dir);
    p += snprintf(p, end - p, " -w /work");

    /* TTY handling - only add -it if we're in a terminal */
    if (is_tty()) {
        p += snprintf(p, end - p, " -it");
    }

    /* Pass through known environment variables */
    for (i = 0; ENV_PASSTHROUGH[i] != NULL; i++) {
        if (getenv(ENV_PASSTHROUGH[i])) {
            p += snprintf(p, end - p, " -e %s", ENV_PASSTHROUGH[i]);
        }
    }

    /* Pass through all TEA_* environment variables (except TEA_IMAGE, TEA_VERSION) */
    extern char **environ;
    if (environ) {
        char **env;
        for (env = environ; *env != NULL; env++) {
            if (strncmp(*env, "TEA_", 4) == 0) {
                /* Extract variable name (up to =) */
                const char *eq = strchr(*env, '=');
                if (eq) {
                    size_t name_len = eq - *env;
                    char name[256];
                    if (name_len < sizeof(name)) {
                        strncpy(name, *env, name_len);
                        name[name_len] = '\0';
                        /* Skip TEA_IMAGE and TEA_VERSION */
                        if (strcmp(name, "TEA_IMAGE") != 0 && strcmp(name, "TEA_VERSION") != 0) {
                            p += snprintf(p, end - p, " -e %s", name);
                        }
                    }
                }
            }
        }
    }

    /* Add image name */
    p += snprintf(p, end - p, " %s", image);

    /* Add user arguments (skip argv[0] which is program name) */
    for (i = 1; i < argc; i++) {
        /* Quote arguments that contain spaces */
        if (strchr(argv[i], ' ') || strchr(argv[i], '"') || strchr(argv[i], '\'')) {
            p += snprintf(p, end - p, " \"%s\"", argv[i]);
        } else {
            p += snprintf(p, end - p, " %s", argv[i]);
        }
    }

    /* Check for buffer overflow */
    if (p >= end) {
        fprintf(stderr, "Error: Command too long\n");
        return 127;
    }

    /* Execute */
    int result = system(cmd);

    /* Convert system() return to exit code */
#ifdef _WIN32
    return result;
#else
    if (WIFEXITED(result)) {
        return WEXITSTATUS(result);
    }
    return result ? 127 : 0;
#endif
}

/* Print help message */
static void show_help(void) {
    printf("tea-wrapper - Cross-platform Docker wrapper for The Edge Agent\n\n");
    printf("Usage: tea [WRAPPER_OPTIONS] [COMMAND] [ARGS...]\n\n");
    printf("Wrapper Options (handled by wrapper, not passed to container):\n");
    printf("  --wrapper-version    Show wrapper version\n");
    printf("  --docker-pull        Pull the latest Docker image\n");
    printf("  --docker-image       Show current Docker image name\n");
    printf("  --wrapper-help       Show this help message\n");
    printf("\nAll other arguments are passed to the TEA container.\n\n");
    printf("Environment Variables:\n");
    printf("  TEA_IMAGE     Override Docker image (default: %s)\n", DEFAULT_IMAGE);
    printf("  TEA_VERSION   Override image tag (default: %s)\n", DEFAULT_VERSION);
    printf("  TEA_GPU       GPU configuration (default: %s)\n", DEFAULT_GPU);
    printf("                  auto   - Auto-detect GPU vendor (default)\n");
    printf("                  nvidia - Force NVIDIA GPU (--gpus all)\n");
    printf("                  all    - Same as nvidia (--gpus all)\n");
    printf("                  amd    - AMD ROCm (--device=/dev/kfd,/dev/dri)\n");
    printf("                  rocm   - Same as amd\n");
    printf("                  intel  - Intel/Vulkan (--device=/dev/dri)\n");
    printf("                  vulkan - Same as intel\n");
    printf("                  dri    - Same as intel\n");
    printf("                  none   - Disable GPU support\n");
    printf("                  N      - Specific NVIDIA GPU count (--gpus N)\n");
    printf("\nGPU Support:\n");
    printf("  Auto-detection priority: NVIDIA > AMD > Intel/Vulkan\n");
    printf("  NVIDIA: Requires NVIDIA Container Toolkit\n");
    printf("          https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/\n");
    printf("  AMD:    Requires ROCm and /dev/kfd device\n");
    printf("          https://rocm.docs.amd.com/projects/install-on-linux/\n");
    printf("  Intel:  Requires /dev/dri device (DRI support)\n");
    printf("\nExample:\n");
    printf("  tea run agent.yaml --input '{\"query\": \"test\"}'\n");
    printf("  tea validate workflow.yaml\n");
    printf("  TEA_VERSION=v0.9.42 tea --version\n");
    printf("  TEA_GPU=nvidia tea run llm-agent.yaml       # Force NVIDIA GPU\n");
    printf("  TEA_GPU=amd tea run llm-agent.yaml          # Force AMD ROCm\n");
    printf("  TEA_GPU=vulkan tea run llm-agent.yaml       # Force Intel/Vulkan\n");
    printf("  TEA_GPU=none tea run agent.yaml             # Disable GPU\n");
}

int main(int argc, char *argv[]) {
    /* Handle wrapper-specific commands */
    if (argc > 1) {
        if (strcmp(argv[1], "--wrapper-version") == 0) {
            show_version();
            return 0;
        }
        if (strcmp(argv[1], "--docker-pull") == 0) {
            return pull_image();
        }
        if (strcmp(argv[1], "--docker-image") == 0) {
            show_image();
            return 0;
        }
        if (strcmp(argv[1], "--wrapper-help") == 0) {
            show_help();
            return 0;
        }
    }

    /* All other commands pass through to Docker */
    int exit_code = run_docker(argc, argv);

    /* Handle docker not found gracefully (AC: 14) */
    if (exit_code == 127) {
        fprintf(stderr, "\nError: Docker command failed. Please ensure:\n");
        fprintf(stderr, "  1. Docker is installed (https://docs.docker.com/get-docker/)\n");
        fprintf(stderr, "  2. Docker daemon is running\n");
        fprintf(stderr, "  3. You have permission to run Docker\n\n");
        fprintf(stderr, "On Linux, you may need to add your user to the docker group:\n");
        fprintf(stderr, "  sudo usermod -aG docker $USER\n");
        fprintf(stderr, "  (log out and back in for this to take effect)\n");
    }

    return exit_code;
}
