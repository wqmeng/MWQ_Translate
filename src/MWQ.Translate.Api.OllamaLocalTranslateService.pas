unit MWQ.Translate.Api.OllamaLocalTranslateService;

interface

uses
  System.Classes, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.Net.URLClient, System.JSON
  , System.Generics.Collections
  , MWQ.Translate.TranslationServiceInterface
  , MWQ.Translate.Api.BaseTranslationService
  , MWQ.Ollama.Types
  , MWQ.Ollama.PromptBuilder;

type
  /// Simple pair type for passing messages: Role -> Content
  TMessagePair = TPair<string,string>;
  TMessageArray = TArray<TMessagePair>;

  TJSONRequestBuilder = class
  public
    class function BuildTranslationRequest(
      const AModel, APrompt: string): TJSONObject;
  end;

  TOllamaService = class(TBaseTranslationService)
  private
    FModelType: TOllamaModelType;
    FEndPoint: TEndpointFlavor;
  protected
    procedure InitializeLanguageMappings; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Translate(const AText, ASourceLang, ADestLang: string; var ATranslated: string; const IsCode: Boolean = false): Boolean; override;
    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function SupportBatchTranslations: Boolean; override;
    function TranslateBatch(const ATexts: TArray<string>; const ASourceLang, ADestLang: string; const IsCode: Boolean = false): TArray<string>; override;
    procedure SetBaseURL(const ABaseUrl: string); override;
    procedure SetModel(const AModel: String); override;
    procedure SetEndpointFlavor(const AEndPoint: TEndpointFlavor);
  end;

implementation

uses
  System.SysUtils, Quick.Logger, MWQ.Ollama.ResponseParser, MWQ.Ollama.Manager, System.StrUtils;

{ TOllamaService }

function TOllamaService.AddTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin
  Result := True;
end;

constructor TOllamaService.Create;
begin
  inherited;
  FBASE_URL := VOllama_Base_Url;
  FModel := 'llama3:latest';
  FEndPoint := TEndpointFlavor.efGenerate;
end;

function TOllamaService.DelTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin
  Result := True;
end;

destructor TOllamaService.Destroy;
begin
  inherited;
end;

procedure TOllamaService.InitializeLanguageMappings;
begin
  inherited;

end;

procedure TOllamaService.SetBaseURL(const ABaseUrl: string);
begin
  inherited;

  if VOllama_Base_Url <> Self.FBASE_URL then
    VOllama_Base_Url := FBASE_URL;
end;

procedure TOllamaService.SetEndpointFlavor(const AEndPoint: TEndpointFlavor);
begin
  FEndPoint := AEndPoint;
end;

procedure TOllamaService.SetModel(const AModel: String);
var
  Models: TArray<string>;
begin
  inherited; // FModel := AModel

  // --- 0. Check if the model exists on the server ---
  Models := TOllamaManager.GetModelsList;
  if not MatchText(FModel, Models) then
  begin
    Log('Model "' + FModel + '" does not exist on the server.', etWarning);
    Exit;
  end;

  // --- 1. Ensure model is running ---
  if not TOllamaManager.IsModelActive(FModel) then
  begin
    if TOllamaManager.StartModel(FModel) then
    begin
      TOllamaManager.AddActiveModel(FModel);
      Log('Model "' + FModel + '" was not running, started successfully.', etInfo);
    end
    else
      Log('Model "' + FModel + '" is not running and could not be started.', etWarning);
  end
  else begin
    TOllamaManager.AddActiveModel(FModel);
    Log('Model "' + FModel + '" is already active.', etInfo);
  end;

  if not TOllamaManager.IsKeepAliveThreadStarted then begin
    TOllamaManager.StartKeepAlive; // idempotent
  end;

  // --- 2. Detect model type ---
  FModelType := TOllamaModelDetector.DetectModelType(FModel);

  // --- 3. Setup language mappings for Riva-style models ---
  if FModelType = TOllamaModelType.mtRiva then
  begin
    FLanguageNamesToCodes.Clear;
    FLanguageCodesToNames.Clear;

    // 1. PRIMARY MAP — code → name
    FLanguageCodesToNames.AddOrSetValue('en', 'English');
    FLanguageCodesToNames.AddOrSetValue('es', 'Spanish');
    FLanguageCodesToNames.AddOrSetValue('fr', 'French');
    FLanguageCodesToNames.AddOrSetValue('de', 'German');

    FLanguageCodesToNames.AddOrSetValue('zh', 'Simplified Chinese');
    FLanguageCodesToNames.AddOrSetValue('zh-cn', 'Simplified Chinese');
    FLanguageCodesToNames.AddOrSetValue('zh-tw', 'Traditional Chinese');
    FLanguageCodesToNames.AddOrSetValue('zh-hk', 'Hong Kong Chinese');

    FLanguageCodesToNames.AddOrSetValue('ja', 'Japanese');
    FLanguageCodesToNames.AddOrSetValue('ru', 'Russian');

    // Model-specific languages
    if FModel.ToLower.Contains('riva-translate') then
    begin
      FLanguageCodesToNames.AddOrSetValue('ar', 'Arabic');
      FLanguageCodesToNames.AddOrSetValue('es-es', 'European Spanish');
      FLanguageCodesToNames.AddOrSetValue('es-us', 'Latin American Spanish');
      FLanguageCodesToNames.AddOrSetValue('ko', 'Korean');
      FLanguageCodesToNames.AddOrSetValue('pt-br', 'Brazilian Portuguese');
    end;

    // 2. Build reverse mapping — name → code
    for var Pair in FLanguageCodesToNames do
      FLanguageNamesToCodes.AddOrSetValue(Pair.Value, Pair.Key);
  end;
end;

function TOllamaService.SupportBatchTranslations: Boolean;
begin
  Result := false;
end;

function TOllamaService.Translate(const AText, ASourceLang, ADestLang: string; var ATranslated: string; const IsCode: Boolean = false): Boolean;
var
  LReqBody: TStringStream;        // Holds the JSON payload for the HTTP POST
  LResponse: IHTTPResponse;       // Holds the HTTP response returned by the Ollama server
  LJsonStr, LContent: string;     // LJsonStr = final JSON payload; LContent = parsed assistant output
  LRetry: Integer;                // Retry counter for failed requests
  LOllamaUrl, LSrc, LDst: string;             // Human-readable source/destination language names for logging
  Msgs: TMessageArray;            // Array of role/content messages (used by chat endpoints)
  SysPrompt: string;              // Optional system prompt for chat-style payloads
  UserPrompt: string;             // Optional user prompt (used for efGenerate endpoint)
begin
  Result := false; // Initialize result

  if ADestLang = '' then
    Exit;

  // -------------------------------
  // Step 1: Prepare human-readable language names for logging
  // -------------------------------
  if IsCode then begin
    LSrc := Self.LanguageCodeToName(ASourceLang);  // e.g., "en" → "English"
    LDst := Self.LanguageCodeToName(ADestLang);    // e.g., "zh" → "Chinese"
  end else begin
    if ASourceLang = '' then
      LSrc := 'English'
    else
      LSrc := ASourceLang;  // e.g., "en" → "English"
    LDst := ADestLang;    // e.g., "zh" → "Chinese"
  end;

  // -------------------------------
  // Step 2: Prepare the payload based on endpoint flavor
  // -------------------------------
  if FEndPoint = efGenerate then
  begin
    // --- efGenerate (instruction-style) ---
    // The /api/generate endpoint does NOT use "messages".
    // The text to translate is passed directly via the 'prompt' field.
    SysPrompt := '';           // Not used for efGenerate

    // Concise translation instruction
    UserPrompt := Format(
      'Translate from %s to %s. Output ONLY the translation, no explanations or notes:%s%s',
      [LSrc, LDst, sLineBreak, AText]
    );
//    UserPrompt := AText;       // Pass the input text as the user prompt
    SetLength(Msgs, 0);        // Clear messages array; not applicable
  end
  else
  begin
    // --- Chat-style endpoints (efChat / efOpenAIChat) ---
    // These endpoints use a structured messages array with roles like "user", "assistant", "system".
    SysPrompt := '';           // Optional system-level instructions (empty here)
    UserPrompt := '';          // Not used; messages array drives the payload
    Msgs := TOllamaPromptBuilder.MakeMessages(['user', Format('Translate from %s to %s. Output ONLY the translation, no explanations or notes:%s%s',
      [LSrc, LDst, sLineBreak, AText])]); // Wrap AText as a user message
  end;

  // -------------------------------
  // Step 3: Build the final JSON payload
  // -------------------------------
  // TOllamaPromptBuilder.BuildPayload automatically constructs the correct JSON format
  // depending on the endpoint flavor:
  // - efGenerate → {"model": "...", "prompt": "...", "stream": false}
  // - efChat / efOpenAIChat → {"model": "...", "messages": [...], "stream": false, ...}
  LJsonStr := TOllamaPromptBuilder.BuildPayload(
    FEndPoint,   // Endpoint flavor (efGenerate, efChat, etc.)
    FModel,      // Model name (e.g., "llama3.1:latest")
    Msgs,        // Messages array (only used for chat-style endpoints)
    SysPrompt,   // Optional system prompt (used only for chat-style)
    UserPrompt,  // Optional user prompt (used only for efGenerate)
    False        // Stream = false (we want the full response in one go)
  );

  // -------------------------------
  // Step 4: At this point, LJsonStr is ready to be sent to the Ollama server
  // Example payloads:
  // efGenerate:
  // {"model":"llama3.1:latest","prompt":"Hello, world","stream":false}
  //
  // efChat:
  // {"model":"llama3.1:latest","messages":[{"role":"user","content":"Hello, world"}],"stream":false}
  // -------------------------------

  // --- Log the payload ---
  Log('TOllamaService.Translate Payload: ' + sLineBreak + LJsonStr, etInfo);

  LReqBody := TStringStream.Create(LJsonStr, TEncoding.UTF8);
  LOllamaUrl := BuildOllamaUrl(FEndPoint);
  try
    for LRetry := 1 to FRetry do
    begin
      try
        LReqBody.Position := 0;
        // --- Log before posting ---
        Log(Format('TOllamaService.Translate: Attempt %d sending request to [%s]%s%s',
          [LRetry, LOllamaUrl, sLineBreak, LJsonStr]), etInfo);

        LResponse := FHttpClient.Post(
          LOllamaUrl,
//          FBASE_URL + 'v1/chat/completions',
          LReqBody,
          nil,
          []
        );

        if (LResponse = nil) then
          raise Exception.Create('HTTP response is nil');

        Log('HTTP Status: ' + LResponse.StatusCode.ToString, etInfo);
        Log('Raw Response: ' + LResponse.ContentAsString, etInfo);

        if LResponse.StatusCode <> 200 then
          raise Exception.Create('HTTP Code ' + LResponse.StatusCode.ToString);

        LContent := LResponse.ContentAsString;

        if TOllamaResponseParser.Parse(LContent, Self.FEndPoint, ATranslated) then
        begin
          ATranslated := Trim(ATranslated);
          Result := True;
          // ---- Log Success ----
          Log('Source [' + LSrc + ']: ' + AText, etInfo);
          Log('Translation [' + LDst + ']: ' + ATranslated, etInfo);

          Exit;
        end
        else
        begin
          // ---- Log Failure ----
          Log('Failed to parse response for Endpoint [' + EndpointFlavorNames[FEndPoint] + ']', etWarning);
          Log('Raw Response: ' + LContent, etInfo);
        end;

      except
        on E: Exception do
        begin
          Log(Format('Translate attempt %d failed: %s', [LRetry, E.Message]), etError);
          if LRetry = FRetry then
          begin
            Log('Final Failure: ', etError);
            Exit;
          end;
          Sleep(150);
        end;
      end;
    end;

  finally
    LReqBody.Free;
  end;
end;

function TOllamaService.TranslateBatch(const ATexts: TArray<string>;
  const ASourceLang, ADestLang: string; const IsCode: Boolean = false): TArray<string>;
begin

end;

{ TJSONRequestBuilder }

class function TJSONRequestBuilder.BuildTranslationRequest(const AModel,
  APrompt: string): TJSONObject;
var
  LFormat, LProps, LTranslationProp: TJSONObject;
  LRequired: TJSONArray;
begin
  Result := TJSONObject.Create;

  // Standard Ollama fields
  Result.AddPair('model', AModel);
  Result.AddPair('prompt', APrompt);
  Result.AddPair('stream', TJSONBool.Create(False));

  // ---------- FORMAT SCHEMA ----------
  LFormat := TJSONObject.Create;
  LProps := TJSONObject.Create;
  LTranslationProp := TJSONObject.Create;
  LRequired := TJSONArray.Create;

  LTranslationProp.AddPair('type', 'string');
  LProps.AddPair('translation', LTranslationProp);

  LFormat.AddPair('type', 'object');
  LFormat.AddPair('properties', LProps);

  LRequired.Add('translation');
  LFormat.AddPair('required', LRequired);

  // schema → final JSON
  Result.AddPair('format', LFormat);
end;

end.
