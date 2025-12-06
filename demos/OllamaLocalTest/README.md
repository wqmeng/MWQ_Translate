Ollama Local Translate Service Tests
===================================

This folder contains a small console test that validates the JSON request builder
used by the `OllamaLocalTranslateService` implementation.

Files
- `TestOllama.dpr` - console test program. Returns exit code `0` on success, non-zero on failure.

Run

- Open `TestOllama.dpr` in RAD Studio / Delphi, ensure `src` is on the project's search path, then compile and run.
- Or build from command-line with MSBuild if your environment is configured.

What it tests
- Verifies `TJSONRequestBuilder.BuildTranslationRequest` outputs a JSON object with keys `model`, `prompt`, `stream`, and a `format` schema that requires a `translation` property.
