Console Demo for MWQ_Translate
=================================

This is a minimal Delphi console demo that shows how to use `TTranslationManager`
and the built-in `LibreTranslate` service implementation.

Files
- `ConsoleDemo.dpr` - the console application source.

Build & Run

- Open `ConsoleDemo.dpr` in RAD Studio / Delphi (ensure the `src` folder is on the project's search path so the units can be found), or create a new console project and add `ConsoleDemo.dpr` as the main file.
- Alternatively, build from command-line with MSBuild if you have RAD Studio installed and configured.

Notes
- The demo registers the `LibreTranslate` service and calls `AddTranslator` with a public LibreTranslate endpoint (`https://trans.zillyhuhn.com/translate`). You can replace that URL with your own endpoint.
- If you prefer another translation backend (Microsoft, DeepL, Ollama), register the corresponding service via `TTranslationManager.RegisterService` and provide any required API keys.
