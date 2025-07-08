# Path config
$EmacsDir = "$HOME\.emacs.d"
$OneDriveTarget = "$HOME\OneDrive\.emacs.d\"

# Make sure target exists
if (-Not (Test-Path $OneDriveTarget)) {
    New-Item -ItemType Directory -Path $OneDriveTarget | Out-Null
}

# Get all git-tracked files relative to .emacs.d
Push-Location $EmacsDir
$TrackedFiles = git ls-files
Pop-Location

# Clear the target (optional – be cautious if you store extra stuff there)
Remove-Item "$OneDriveTarget\*" -Recurse -Force -ErrorAction SilentlyContinue

# Copy only tracked files
foreach ($file in $TrackedFiles) {
    $source = Join-Path $EmacsDir $file
    $dest = Join-Path $OneDriveTarget $file
    $destDir = Split-Path $dest -Parent
    if (-Not (Test-Path $destDir)) {
        New-Item -ItemType Directory -Path $destDir -Force | Out-Null
    }
    Copy-Item $source -Destination $dest -Force
}
