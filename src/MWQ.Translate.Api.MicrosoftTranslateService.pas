unit MwQ.Translate.Api.MicrosoftTranslateService;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.URLClient,
  System.JSON, MWQ.Translate.TranslationServiceInterface, MWQ.Translate.Api.BaseTranslationService;

type
  TMicrosoftTranslateService = class(TBaseTranslationService, ITranslationService)
  private
    FSubscriptionKey: string;
    const
      BASE_URL = 'https://api.cognitive.microsofttranslator.com/translate?api-version=3.0';
  protected
    procedure InitializeLanguageMappings; override;
  public
    constructor Create(const ASubscriptionKey: string);
    destructor Destroy; override;

    function Translate(const AText, ASourceLang, ADestLang: string): string;
    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean;
  end;

implementation

{ TMicrosoftTranslateService }

function TMicrosoftTranslateService.AddTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin

end;

constructor TMicrosoftTranslateService.Create(const ASubscriptionKey: string);
begin
  Inherited Create;
  FSubscriptionKey := ASubscriptionKey;
end;

function TMicrosoftTranslateService.DelTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin

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

function TMicrosoftTranslateService.Translate(const AText, ASourceLang, ADestLang: string): string;
var
  LResponse: IHTTPResponse;
  LRequestBody: TStringStream;
  LJson, LResult: TJSONArray;
  LTranslation: TJSONObject;
begin
  Result := '';

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
        BASE_URL + '&from=' + ASourceLang + '&to=' + ADestLang,
        LRequestBody,
        nil,
        [
          TNetHeader.Create('Content-Type', 'application/json'),
          TNetHeader.Create('Ocp-Apim-Subscription-Key', FSubscriptionKey)
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

end.

