unit MWQ.Translate.Api.BaseTranslationService;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MWQ.Translate.TranslationServiceInterface, System.Net.HttpClient;

type
  TBaseTranslationService = class(TInterfacedObject, ITranslationService)

  protected
    FRetry: integer;
    FBASE_URL: string;
    FAPIKEY: string;
    FModel: String;

    FHttpClient: THttpClient;
    FTranslators: TDictionary<string, TList<string>>;
    FLanguageNamesToCodes: TDictionary<string, string>;
    FLanguageCodesToNames: TDictionary<string, string>;
    procedure InitializeLanguageMappings; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetBaseURL(const ABaseUrl: string); virtual;
    function Translate(const AText, ASourceLang, ADestLang: string; const IsCode: Boolean = false): string; virtual; abstract;
    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean; virtual; abstract;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean; virtual; abstract;
    function GetSupportedLanguages: TDictionary<string, string>; // New method
    function LanguageNameToCode(const AName: string): string;
    function LanguageCodeToName(const ACode: string): string;
    function SupportBatchTranslations: Boolean; virtual;
    function TranslateBatch(const ATexts: TArray<string>; const ASourceLang, ADestLang: string): TArray<string>; virtual; abstract;
    function DelDefaultBaseTranslator: Boolean; virtual;
    procedure SetTimeOut(const AMicroSeconds: Integer); virtual;
    procedure SetRetry(const ARetry: Integer); virtual;
    procedure SetModel(const AModel: String); virtual;
    function GetModel: string; virtual;
    property ApiKey: string read FAPIKEY write FAPIKEY;

  end;

implementation

constructor TBaseTranslationService.Create;
var
  DefaultApiKeys: TList<string>;
begin
  FRetry := 3;
  FBASE_URL := '';
  FAPIKEY := '';
  FHttpClient := THttpClient.Create;
  FTranslators := TDictionary<string, TList<string>>.Create;
  FLanguageNamesToCodes := TDictionary<string, string>.Create;
  FLanguageCodesToNames := TDictionary<string, string>.Create;

  InitializeLanguageMappings;

  // Initialize default translator with an empty API key
  DefaultApiKeys := TList<string>.Create;
  DefaultApiKeys.Add(''); // Add empty API key
  FTranslators.Add(FBASE_URL, DefaultApiKeys);
end;

function TBaseTranslationService.DelDefaultBaseTranslator: Boolean;
begin
  Result := DelTranslator(Self.FBASE_URL, FAPIKEY);
end;

destructor TBaseTranslationService.Destroy;
var
  LKeys: TArray<String>;
  I: Integer;
begin
  FLanguageNamesToCodes.Free;
  FLanguageCodesToNames.Free;
  if FTranslators <> nil then begin
    LKeys := FTranslators.Keys.ToArray;
    for I := Low(LKeys) to High(LKeys) do begin
      FTranslators[LKeys[I]].Free;
    end;
    FTranslators.Clear;
    FTranslators.free;
  end;
  FHttpClient.Free;
  inherited;
end;

function TBaseTranslationService.GetModel: string;
begin
  Result := FModel;
end;

function TBaseTranslationService.GetSupportedLanguages: TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create(FLanguageNamesToCodes);
end;

procedure TBaseTranslationService.InitializeLanguageMappings;
begin
  // Clear both
  FLanguageCodesToNames.Clear;
  FLanguageNamesToCodes.Clear;

  // Canonical mapping
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
  // Populate reverse mappings
  for var Pair in FLanguageCodesToNames do
    FLanguageNamesToCodes.AddOrSetValue(Pair.Value, Pair.Key);
end;

function TBaseTranslationService.LanguageNameToCode(const AName: string): string;
var
  Pair: TPair<string, string>;
  LName: string;
begin
  Result := '';
  if AName = '' then Exit;
  LName := AName.ToLower;

  // Iterate over key-value pairs directly
  for Pair in FLanguageNamesToCodes do
  begin
    if SameText(Pair.Key.ToLower, LName) then
    begin
      Result := Pair.Value;
      Exit;
    end;
  end;
end;

procedure TBaseTranslationService.SetBaseURL(const ABaseUrl: string);
begin
  FBASE_URL := ABaseUrl;
end;

procedure TBaseTranslationService.SetModel(const AModel: String);
begin
  FModel := AModel;
end;

procedure TBaseTranslationService.SetRetry(const ARetry: Integer);
begin
  if FRetry <> ARetry then
    FRetry := ARetry;
end;

procedure TBaseTranslationService.SetTimeOut(const AMicroSeconds: Integer);
begin
  if FHttpClient <> nil then
    FHttpClient.ResponseTimeout := AMicroSeconds;
end;

function TBaseTranslationService.SupportBatchTranslations: Boolean;
begin
  Result := false;
end;

function TBaseTranslationService.LanguageCodeToName(const ACode: string): string;
var
  Key: string;
begin
  if ACode = '' then
    Exit('English');
  Key := ACode.ToLower;
  if FLanguageCodesToNames.TryGetValue(Key, Result) then
    Exit;
  Result := 'English'; // Return English if not found
end;

end.

