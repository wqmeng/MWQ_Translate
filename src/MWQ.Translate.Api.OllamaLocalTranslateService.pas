unit MWQ.Translate.Api.OllamaLocalTranslateService;

interface

uses
  System.Classes, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.Net.URLClient, System.JSON, System.Generics.Collections, MWQ.Translate.TranslationServiceInterface, MWQ.Translate.Api.BaseTranslationService;

type
  TOllamaService = class(TBaseTranslationService)
  private
  protected
    procedure InitializeLanguageMappings; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Translate(const AText, ASourceLang, ADestLang: string): string; override;
    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function SupportBatchTranslations: Boolean; override;
    function TranslateBatch(const ATexts: TArray<string>; const ASourceLang, ADestLang: string): TArray<string>; override;
    procedure SetBaseURL; override;
  end;

implementation

uses
  System.SysUtils;

{ TOllamaService }

function TOllamaService.AddTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin
  Result := True;
end;

constructor TOllamaService.Create;
begin
  inherited;
  FModel := 'llama3:latest';
  Self.FBASE_URL := 'http://localhost:11434/api/'
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

procedure TOllamaService.SetBaseURL;
begin
  inherited;

end;

function TOllamaService.SupportBatchTranslations: Boolean;
begin
  Result := false;
end;

function TOllamaService.Translate(const AText, ASourceLang, ADestLang: string): string;
var
  LSourceLang, LPrompt: string;
  LRequestBody: TStringStream;
  LJson, LFormat, LProps, LTranslationProp: TJSONObject;
  LRequired: TJSONArray;

  LResponse: IHTTPResponse;
  LResStr, LInnerJsonStr: string;
  LJsonResp, LInnerJson: TJSONObject;

  LRetry: Integer;
  Success: Boolean;
begin
  Result := '';

  // Determine language label
  if ASourceLang = '' then
    LSourceLang := 'English'
  else
    LSourceLang := ASourceLang;

  LPrompt := Format('Please translate from %s to %s: "%s"', [LSourceLang, ADestLang, AText]);

  // ----------------------------
  // Build request body JSON
  // ----------------------------
  LJson := TJSONObject.Create;
  try
    LJson.AddPair('model', FModel);
    LJson.AddPair('prompt', LPrompt);
    LJson.AddPair('stream', false);

    // --- format schema ---
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

    LJson.AddPair('format', LFormat);

    LRequestBody := TStringStream.Create(LJson.ToString);
  finally
    LJson.Free; // Frees LFormat, LProps, LTranslationProp, LRequired as well
  end;

  // ----------------------------
  // Perform retries
  // ----------------------------
  Success := False;
  try
    for LRetry := 1 to FRetry do
    begin
      try
        LRequestBody.Position := 0;  // Reset before each POST

        LResponse := FHttpClient.Post(
          FBASE_URL + 'generate',
          LRequestBody,
          nil,
          []
        );

        if (LResponse <> nil) and (LResponse.StatusCode = 200) then
        begin
          // Parse outer JSON
          LJsonResp := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
          try
            if Assigned(LJsonResp) and
               LJsonResp.TryGetValue<string>('response', LInnerJsonStr) then
            begin
              // Parse inner JSON
              LInnerJson := TJSONObject.ParseJSONValue(LInnerJsonStr) as TJSONObject;
              if Assigned(LInnerJson) then
              begin
                Result := LInnerJson.GetValue<string>('translation');
                if Result <> '' then
                begin
                  Success := True;
                  Break;
                end;
              end;
            end;
          finally
            if LInnerJson <> nil then
              FreeAndNil(LInnerJson);
            if LJsonResp <> nil then
              FreeAndNil(LJsonResp);
          end;
        end;
      except
        on E: Exception do
        begin
          if LRetry >= FRetry then
            raise;
          Sleep(150);
        end;
      end;
    end;

    if not Success then
      Result := 'Translation failed with all available translators.';

  finally
    LRequestBody.Free;
  end;
end;

function TOllamaService.TranslateBatch(const ATexts: TArray<string>;
  const ASourceLang, ADestLang: string): TArray<string>;
begin

end;

end.
