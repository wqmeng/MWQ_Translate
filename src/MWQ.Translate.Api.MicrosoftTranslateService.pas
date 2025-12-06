unit MwQ.Translate.Api.MicrosoftTranslateService;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.URLClient, System.Generics.Collections,
  System.JSON, MWQ.Translate.TranslationServiceInterface, MWQ.Translate.Api.BaseTranslationService;

type
  TMicrosoftTranslateService = class(TBaseTranslationService)
  private

  protected
    procedure InitializeLanguageMappings; override;
  public
    constructor Create(const ASubscriptionKey: string);
    destructor Destroy; override;

    procedure SetBaseURL(const ABaseUrl: string); override;
    function Translate(const AText, ASourceLang, ADestLang: string; const IsCode: Boolean = false): string; override;
    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function TranslateBatch(const ATexts: TArray<string>; const ASourceLang, ADestLang: string): TArray<string>; override;
  end;

implementation

{ TMicrosoftTranslateService }

function TMicrosoftTranslateService.AddTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin
  Result := false;
end;

constructor TMicrosoftTranslateService.Create(const ASubscriptionKey: string);
begin
  Inherited Create;
  FAPIKEY := ASubscriptionKey;
  FBASE_URL := 'https://api.cognitive.microsofttranslator.com/translate?api-version=3.0';
end;

function TMicrosoftTranslateService.DelTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin
  Result := true;
end;

destructor TMicrosoftTranslateService.Destroy;
begin
  inherited;
end;

procedure TMicrosoftTranslateService.InitializeLanguageMappings;
begin
  inherited;
//  FLanguageNamesToCodes.Items['Chinese (Literary)'] := 'zh-hs';
//  FLanguageNamesToCodes.Items['Chinese Simplified'] := 'zh-hs';
//  FLanguageNamesToCodes['Chinese Traditional'] := 'zh-ts';
end;

procedure TMicrosoftTranslateService.SetBaseURL(const ABaseUrl: string);
begin
  inherited;
end;

function TMicrosoftTranslateService.Translate(const AText, ASourceLang, ADestLang: string; const IsCode: Boolean = false): string;
var
  LResponse: IHTTPResponse;
  LRequestBody: TStringStream;
  LJson, LResult: TJSONArray;
  LTranslation: TJSONObject;
  LSrc, LDst: String;
begin
  Result := '';
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

  LRequestBody := TStringStream.Create;
  try
    // Prepare the JSON request body
    LJson := TJSONArray.Create;
    try
      LJson.Add(TJSONObject.Create
        .AddPair('Text', AText));

      LRequestBody.WriteString(LJson.ToString);
      LRequestBody.Position := 0;

      // Make the POST request
      LResponse := FHttpClient.Post(
        FBASE_URL + '&from=' + LSrc + '&to=' + LDst,
        LRequestBody,
        nil,
        [
          TNetHeader.Create('Content-Type', 'application/json'),
          TNetHeader.Create('Ocp-Apim-Subscription-Key', FAPIKEY)
        ]
      );

      // Check response status
      if LResponse.StatusCode = 200 then
      begin
        // Parse the response
        LResult := TJSONArray.ParseJSONValue(LResponse.ContentAsString) as TJSONArray;
        try
          if LResult.Count > 0 then begin
            // Access the translations array
            LTranslation := LResult.Items[0].GetValue<TJSONArray>('translations').Items[0] as TJSONObject;
            Result := LTranslation.GetValue<string>('text');
          end;
        finally
          LResult.Free;
        end;
      end
      else
        raise Exception.Create('Translation failed: ' + LResponse.ContentAsString);
    finally
      LJson.Free;
    end;
  finally
    LRequestBody.Free;
  end;
end;

function TMicrosoftTranslateService.TranslateBatch(const ATexts: TArray<string>;
  const ASourceLang, ADestLang: string): TArray<string>;
begin
  raise Exception.Create('Not implement.');

  Result := [];
end;

end.

