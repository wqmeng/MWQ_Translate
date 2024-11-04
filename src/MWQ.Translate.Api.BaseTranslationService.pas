unit MWQ.Translate.Api.BaseTranslationService;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MWQ.Translate.TranslationServiceInterface, System.Net.HttpClient;

type
  TBaseTranslationService = class(TInterfacedObject, ITranslationService)

  protected
    FHttpClient: THttpClient;
    FTranslators: TDictionary<string, TList<string>>;
    FLanguageNamesToCodes: TDictionary<string, string>;
    FLanguageCodesToNames: TDictionary<string, string>;
    procedure InitializeLanguageMappings; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Translate(const AText, ASourceLang, ADestLang: string): string; virtual; abstract;
    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean; virtual; abstract;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean; virtual; abstract;
    function GetSupportedLanguages: TDictionary<string, string>; // New method
    function LanguageNameToCode(const AName: string): string;
    function LanguageCodeToName(const ACode: string): string;
    function SupportBatchTranslations: Boolean; virtual;
    function TranslateBatch(const ATexts: TArray<string>; const ASourceLang, ADestLang: string): TArray<string>; virtual; abstract;
  end;

implementation

constructor TBaseTranslationService.Create;
begin
  FHttpClient := THttpClient.Create;
  FTranslators := TDictionary<string, TList<string>>.Create;
  FLanguageNamesToCodes := TDictionary<string, string>.Create;
  FLanguageCodesToNames := TDictionary<string, string>.Create;

  InitializeLanguageMappings;
end;

destructor TBaseTranslationService.Destroy;
var
  LKeys: TArray<String>;
  I: Integer;
begin
  FLanguageNamesToCodes.Free;
  FLanguageCodesToNames.Free;

  LKeys := FTranslators.Keys.ToArray;
  for I := Low(LKeys) to High(LKeys) do begin
    FTranslators[LKeys[I]].Free;
  end;
  FTranslators.Clear;
  FTranslators.free;

  FHttpClient.Free;
  inherited;
end;

function TBaseTranslationService.GetSupportedLanguages: TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create(FLanguageNamesToCodes);
end;

procedure TBaseTranslationService.InitializeLanguageMappings;
begin
  // Base language mappings
  FLanguageNamesToCodes.Add('English', 'en');
  FLanguageNamesToCodes.Add('Spanish', 'es');
  FLanguageNamesToCodes.Add('French', 'fr');
  FLanguageNamesToCodes.Add('German', 'de');
  FLanguageNamesToCodes.Add('Chinese', 'zh');
  FLanguageNamesToCodes.Add('Japanese', 'ja');
  FLanguageNamesToCodes.Add('Russian', 'ru');

  // Populate reverse mappings
  for var Pair in FLanguageNamesToCodes do
    FLanguageCodesToNames.Add(Pair.Value, Pair.Key);
end;

function TBaseTranslationService.LanguageNameToCode(const AName: string): string;
begin
  if FLanguageNamesToCodes.TryGetValue(AName, Result) then
    Exit;
  Result := ''; // Return empty if not found
end;

function TBaseTranslationService.SupportBatchTranslations: Boolean;
begin
  Result := false;
end;

function TBaseTranslationService.LanguageCodeToName(const ACode: string): string;
begin
  if FLanguageCodesToNames.TryGetValue(ACode, Result) then
    Exit;
  Result := ''; // Return empty if not found
end;

end.

