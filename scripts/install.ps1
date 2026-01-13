# TEA-DIST-001: Install script for Windows
#
# Usage (PowerShell):
#   irm https://raw.githubusercontent.com/fabceolin/the_edge_agent/main/scripts/install.ps1 | iex
#
# Or specify a version:
#   $env:TEA_VERSION = "v0.9.42"; irm https://raw.githubusercontent.com/fabceolin/the_edge_agent/main/scripts/install.ps1 | iex

# Configuration
$Repo = "fabceolin/the_edge_agent"
$BinaryName = "tea.com"
$InstallDir = "$env:LOCALAPPDATA\tea"
$Version = if ($env:TEA_VERSION) { $env:TEA_VERSION } else { "latest" }

function Write-Info {
    param([string]$Message)
    Write-Host "[INFO] " -ForegroundColor Green -NoNewline
    Write-Host $Message
}

function Write-Warn {
    param([string]$Message)
    Write-Host "[WARN] " -ForegroundColor Yellow -NoNewline
    Write-Host $Message
}

function Write-Err {
    param([string]$Message)
    Write-Host "[ERROR] " -ForegroundColor Red -NoNewline
    Write-Host $Message
    exit 1
}

function Test-DockerInstalled {
    try {
        $null = Get-Command docker -ErrorAction Stop
        return $true
    } catch {
        return $false
    }
}

function Get-LatestVersion {
    try {
        $response = Invoke-RestMethod -Uri "https://api.github.com/repos/$Repo/releases/latest"
        return $response.tag_name
    } catch {
        Write-Err "Failed to get latest version: $_"
    }
}

function Install-Tea {
    param([string]$Version)

    # Resolve latest version
    if ($Version -eq "latest") {
        Write-Info "Fetching latest release..."
        $Version = Get-LatestVersion
        if (-not $Version) {
            Write-Err "Failed to get latest version"
        }
    }

    Write-Info "Installing TEA $Version..."

    # Create install directory
    if (-not (Test-Path $InstallDir)) {
        New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
    }

    # Download binary
    $downloadUrl = "https://github.com/$Repo/releases/download/$Version/$BinaryName"
    $outputPath = "$InstallDir\tea.exe"

    Write-Info "Downloading from $downloadUrl..."

    try {
        Invoke-WebRequest -Uri $downloadUrl -OutFile $outputPath -UseBasicParsing
    } catch {
        Write-Err "Failed to download TEA. Check if version $Version exists."
    }

    Write-Info "TEA installed to $outputPath"
}

function Add-ToPath {
    $currentPath = [Environment]::GetEnvironmentVariable("Path", "User")

    if ($currentPath -notlike "*$InstallDir*") {
        Write-Host ""
        Write-Warn "Adding $InstallDir to your PATH..."

        $newPath = "$InstallDir;$currentPath"
        [Environment]::SetEnvironmentVariable("Path", $newPath, "User")

        # Update current session
        $env:Path = "$InstallDir;$env:Path"

        Write-Info "PATH updated. You may need to restart your terminal."
    }
}

function Test-Installation {
    $teaPath = "$InstallDir\tea.exe"

    if (Test-Path $teaPath) {
        Write-Host ""
        Write-Info "Installation successful!"
        Write-Host ""

        & $teaPath --wrapper-version

        Write-Host ""
        Write-Info "Run 'tea --help' to get started."
    } else {
        Write-Err "Installation verification failed"
    }
}

function Pull-DockerImage {
    if (Test-DockerInstalled) {
        Write-Host ""
        Write-Info "Pulling TEA Docker image (this may take a minute)..."

        try {
            docker pull ghcr.io/fabceolin/tea:latest
        } catch {
            Write-Warn "Failed to pull Docker image. You can pull it later with: tea --docker-pull"
        }
    }
}

# Main
function Main {
    Write-Host ""
    Write-Host "================================================"
    Write-Host "  TEA - The Edge Agent Installer (Windows)"
    Write-Host "================================================"
    Write-Host ""

    if (-not (Test-DockerInstalled)) {
        Write-Warn "Docker is not installed. TEA requires Docker to run."
        Write-Warn "Install Docker Desktop from: https://docs.docker.com/docker-for-windows/install/"
    }

    Install-Tea -Version $Version
    Add-ToPath
    Test-Installation
    Pull-DockerImage

    Write-Host ""
    Write-Host "================================================"
    Write-Host "  Installation Complete!"
    Write-Host "================================================"
    Write-Host ""
}

Main
