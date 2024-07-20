# SAM (Software Automatic Mouth) - Go Port

## What is SAM?

SAM (Software Automatic Mouth) is a vintage text-to-speech engine originally developed for the Commodore 64 in 1982 by Don't Ask Software (now SoftVoice, Inc.). This project is a Go port of SAM based on the C adaptation by [Stefan Macke](https://github.com/s-macke/SAM).

## Features

* Text-to-speech conversion
* Phonetic input mode
* Adjustable pitch, speed, mouth, and throat settings
* Singing mode
* WAV file output
* Debug mode for detailed processing information

```markdown

## Installation

To install SAM, ensure you have Go installed on your system, then run the following:

```bash
go install github.com/pdxiv/samgo@latest
```

This command will download the source code, compile it, and install the `samgo` binary in your Go bin directory.

### Verifying the Installation

After installation, you may need to ensure the Go bin directory is in your system's PATH. Here's how you can check and set it up if needed:

1. Find your Go bin directory:
   

```bash
   go env GOPATH
   ```

   The Go bin directory is typically `$(go env GOPATH)/bin` .

2. Check if it's in your PATH:
   

```bash
   echo $PATH
   ```

   Look for the Go bin directory in the output.

3. If it's not in your PATH, add it:

   - For Bash (Linux/macOS), add this line to your `~/.bashrc` or `~/.bash_profile` :

     

```bash
     export PATH=$PATH:$(go env GOPATH)/bin
     ```

   - For Zsh (macOS), add the same line to `~/.zshrc` :

     

```bash
     export PATH=$PATH:$(go env GOPATH)/bin
     ```

   - For Windows (PowerShell), add this to your PowerShell profile:

     

```powershell
     $env:Path += ";$(go env GOPATH)\bin"
     ```

4. After modifying your shell configuration, restart your terminal or run `source` on the modified file (e.g.,   `source ~/.bashrc`) to apply the changes.

5. Verify the installation by running:
   

```bash
   samgo
   ```

   If installed correctly, this should display SAM's standard help text.

## Usage

Basic usage:

```
samgo "Hello, world!"
```

Using phonetic input:

```
samgo -phonetic "HEH4LOW WER4LD"
```

Adjusting speech parameters:

```
samgo -pitch 64 -speed 72 -mouth 128 -throat 128 "Hello, world!"
```

Output to WAV file:

```
samgo -wav output.wav "Hello, world!"
```

## Dependencies

This project uses the following external library:

* github.com/ebitengine/oto/v3 for audio output

## Current Status

The Go port of SAM is functional with the following notes:

* All core features from the original SAM have been implemented.
* WAV file output works correctly.
* Direct audio output is functional but may sound slightly garbled compared to the WAV output.

## Acknowledgments

* Original SAM software by Don't Ask Software (SoftVoice, Inc.)
* C port by Stefan Macke (https://github.com/s-macke/SAM)
