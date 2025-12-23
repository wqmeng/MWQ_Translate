unit MWQ.Translate.Api.LibreTranslateService;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.Net.URLClient, System.JSON, System.Generics.Collections,
  MWQ.Translate.TranslationServiceInterface, MWQ.Translate.Api.BaseTranslationService;

type
  TLibreTranslateService = class(TBaseTranslationService)
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
    function DelDefaultBaseTranslator: Boolean; override;
  end;

implementation

uses
  System.Math; // For Random

{ TLibreTranslateService }

constructor TLibreTranslateService.Create;
begin
  Inherited create;
  FBASE_URL := 'https://trans.zillyhuhn.com/translate';
end;

destructor TLibreTranslateService.Destroy;
begin
//  for Key in FTranslators.Keys do
//    FTranslators[Key].Free; // Free each list of API keys
////  FTranslators.Free;
////  FHttpClient.Free;
  inherited;
end;

procedure TLibreTranslateService.InitializeLanguageMappings;
begin
  inherited; // Call base implementation

  // Override or add additional language mappings specific to Libre Translate
//  FLanguageNamesToCodes.Add('Italian', 'it');
//  FLanguageNamesToCodes.Add('Portuguese', 'pt');
  // You can override existing mappings if needed
end;

procedure TLibreTranslateService.SetBaseURL(const ABaseUrl: string);
begin
  inherited;
end;

function TLibreTranslateService.SupportBatchTranslations: Boolean;
begin
  Result := false;
end;

function TLibreTranslateService.Translate(const AText, ASourceLang, ADestLang: string; var ATranslated: string; const IsCode: Boolean = false): Boolean;
var
  LResponse: IHTTPResponse;
  LRequestBody: TStringStream;
  LJson, LJsonResp: TJSONObject;
  ApiKey: string;
  TranslatorUrls: TArray<string>;
  ApiKeys: TList<string>;
  SelectedTranslatorUrl: string;
  Success: Boolean;
  I: Integer;
  LSrc, LDst: String;
begin
  Result := false;
  if ADestLang = '' then
    Exit;

  if not IsCode then begin
    LSrc := Self.LanguageNameToCode(ASourceLang);  // e.g., "English" ¡ú "en"
    LDst := Self.LanguageNameToCode(ADestLang);    // e.g., "Chinese" ¡ú "zh"
  end else begin
    if ASourceLang = '' then
      LSrc := 'en'
    else
      LSrc := ASourceLang;
    LDst := ADestLang;
  end;

  Success := False;

  LRequestBody := TStringStream.Create;
  try
    // Prepare the JSON request body
    LJson := TJSONObject.Create;
    try
      LJson.AddPair('q', AText);
      LJson.AddPair('source', LSrc);
      LJson.AddPair('target', LDst);
      LJson.AddPair('format', 'html');
//      LJson.AddPair('alternatives', 3);

      // Get all translator URLs
      TranslatorUrls := FTranslators.Keys.ToArray;

      // Try each translator URL
      for I := 0 to Length(TranslatorUrls) - 1 do
      begin
        // Get a random translator URL
        SelectedTranslatorUrl := TranslatorUrls[RandomRange(0, Length(TranslatorUrls))];

        // Get the list of API keys for the selected translator URL
        ApiKeys := FTranslators[SelectedTranslatorUrl];

        // Select a random API key from the list
        if ApiKeys.Count > 0 then
          ApiKey := ApiKeys[RandomRange(0, ApiKeys.Count)]
        else
          ApiKey := ''; // Handle case where there are no API keys

        LJson.AddPair('api_key', ApiKey);
        LRequestBody.WriteString(LJson.ToString);
        LRequestBody.Position := 0;

        // Make the POST request
        try
          LResponse := FHttpClient.Post(SelectedTranslatorUrl, LRequestBody, nil, [TNetHeader.Create('Content-Type', 'application/json')]);
        except
          on E: Exception do
          begin
          end;
        end;

        // Check response status
        if (LResponse <> nil) and (LResponse.StatusCode = 200) then
        begin
          // Parse the response
          var LResStr := LResponse.ContentAsString;
          LJsonResp := TJSONObject.ParseJSONValue(LResStr) as TJSONObject;
          try
            ATranslated := LJsonResp.GetValue<string>('translatedText');
            Result := True;
            Break; // Exit the loop on success
          finally
            LJsonResp.Free;
          end;
        end;
      end;

      // If all translators failed, you can handle that case here if needed
//      if not Success then
//        Result := TRANSLATION_FAIL_ALL_MSG;

    finally
      LJson.Free;
    end;
  finally
    LRequestBody.Free;
  end;
end;

function TLibreTranslateService.TranslateBatch(const ATexts: TArray<string>; const ASourceLang, ADestLang: string; const IsCode: Boolean = false): TArray<string>;
const
  LIMITCOUNT = 4;
var
  LJsonArrayString: string;
  LTranslatedTexts: string; // Assuming Translate returns a JSON array string
  I, J, LTransCountOnce: Integer;
//  LStartIndex, LEndIndex: Integer;
  LResult: TList<string>; // Use a dynamic list for easier accumulation
  LJsonValue: TJSONArray;
  LSrc, LDst: String;
begin
  raise Exception.Create('Not implement.');

  if ADestLang = '' then
    Exit;

  if not IsCode then begin
    LSrc := Self.LanguageNameToCode(ASourceLang);  // e.g., "English" ¡ú "en"
    LDst := Self.LanguageNameToCode(ADestLang);    // e.g., "Chinese" ¡ú "zh"
  end else begin
    if ASourceLang = '' then
      LSrc := 'en'
    else
      LSrc := ASourceLang;
    LDst := ADestLang;
  end;

  LResult := TList<string>.Create;
  try
    // Process texts in chunks of 8
    for I := 0 to High(ATexts) div 8 do
    begin
      // Build JSON array string for the current chunk
      LJsonArrayString := '[';
      LTransCountOnce := 0;
      for J := 0 to LIMITCOUNT - 1 do
      begin
        if (I * LIMITCOUNT + J) > High(ATexts) then
          Break; // Exit if we exceed the number of texts

        if J > 0 then begin
          LJsonArrayString := LJsonArrayString + ',';
          Inc(LTransCountOnce, 1);
        end;
        LJsonArrayString := LJsonArrayString + '"' + StringReplace(ATexts[I * LIMITCOUNT + J], '"', '\"', [rfReplaceAll]) + '"';
      end;

      LJsonArrayString := LJsonArrayString + ']';

      // Call the Translate function with the JSON array string
      if Translate(LJsonArrayString, LSrc, LDst, LTranslatedTexts) then
        LTranslatedTexts := 'result:' + LTranslatedTexts;

      // Parse the JSON array string manually
//      if (LTranslatedTexts.StartsWith('[')) and (LTranslatedTexts.EndsWith(']')) then
//      begin
        // Parse the JSON array string
        LTranslatedTexts := StringReplace(LTranslatedTexts, '"', '\"', [rfReplaceAll]);
        LJsonValue := TJSONObject.ParseJSONValue(LTranslatedTexts) as TJSONArray;
        if Assigned(LJsonValue) then begin
          try
            for J := 0 to LJsonValue.Count - 1 do
            begin
              LResult.Add(LJsonValue.Items[J].Value.Trim(['"']));
            end;
          finally
            LJsonValue.Free; // Free the JSON array
          end;

        end else begin
          for J := 0 to LTransCountOnce - 1 do
            LResult.add('');
        end;
    end;

    // Convert the list to an array for the result
    Result := LResult.ToArray;
  finally
    LResult.Free; // Free the dynamic list
  end;
end;


function TLibreTranslateService.AddTranslator(const ATransApiUrl, AApiKey: string): Boolean;
var
  ApiKeys: TList<string>;
begin
  inherited;
  Result := False; // Default result
  if not FTranslators.ContainsKey(ATransApiUrl) then
  begin
    ApiKeys := TList<string>.Create;
    ApiKeys.Add(AApiKey);
    FTranslators.Add(ATransApiUrl, ApiKeys);
    Result := True; // Successfully added
  end
  else
  begin
    // If the URL exists, add the API key if it's not already present
    if not FTranslators[ATransApiUrl].Contains(AApiKey) then
    begin
      FTranslators[ATransApiUrl].Add(AApiKey);
      Result := True; // Successfully added
    end;
  end;
end;

function TLibreTranslateService.DelDefaultBaseTranslator: Boolean;
begin
  Result := inherited;
end;

function TLibreTranslateService.DelTranslator(const ATransApiUrl, AApiKey: string): Boolean;
begin
  inherited;
  Result := False; // Default result
  if FTranslators.ContainsKey(ATransApiUrl) then
  begin
    if FTranslators[ATransApiUrl].Remove(AApiKey) >= 0 then
    begin
      // If the list of API keys is now empty, remove the URL from the dictionary
      if FTranslators[ATransApiUrl].Count = 0 then
      begin
        FTranslators.Items[ATransApiUrl].Free;
        FTranslators.Remove(ATransApiUrl);
      end;
      Result := True; // Successfully deleted
    end;
  end;
end;

end.

