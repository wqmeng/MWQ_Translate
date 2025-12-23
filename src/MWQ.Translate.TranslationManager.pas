unit MWQ.Translate.TranslationManager;

interface

uses
  System.SysUtils, System.Generics.Collections, MWQ.Translate.TranslationServiceInterface,
  System.Classes, MWQ.Translate.Types;

type
  TTranslationManager = class
  private
    FServices: TDictionary<string, ITranslationService>;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterService(const AService: TTranslationService; const AApiKey: string): ITranslationService;
    function TranslateByName(const AServiceName, AText, ASourceLangName, ADestLangName: string): string;
    function TranslateByCode(const AServiceName, AText, ASourceLangCode, ADestLangCode: string): string;
    function GetRegisteredServices: TStringList;
    function GetTranslateService(const AServiceName: string): ITranslationService;
    function IsServiceRegistered(AService: TTranslationService): Boolean;

    function GetSupportedLanguages: TDictionary<string, string>;
    function SupportsLanguage(const Lang: string): Boolean;
    function GetBestServiceForLanguage(
      const Lang: string
    ): ITranslationService;

  end;

implementation

uses
  MWQ.Translate.Api.LibreTranslateService, MWQ.Translate.Api.DeepLXTranslateService,
  MwQ.Translate.Api.MicrosoftTranslateService, MWQ.Translate.Api.OllamaLocalTranslateService;

{ TTranslationManager }

constructor TTranslationManager.Create;
begin
  FServices := TDictionary<string, ITranslationService>.Create;
end;

destructor TTranslationManager.Destroy;
begin
  FServices.Free;
  inherited;
end;

function TTranslationManager.GetBestServiceForLanguage(
  const Lang: string): ITranslationService;
var
  Service: TPair<string, ITranslationService>;
  Normalized: string;
begin
  Result := nil;
  Normalized := LowerCase(Lang);

  for Service in FServices do
    if Service.Value.SupportsLanguage(Normalized) then
      Exit(Service.Value);
end;

function TTranslationManager.GetRegisteredServices: TStringList;
var
  ServiceName: string;
  ServiceList: TStringList;
begin
  ServiceList := TStringList.Create;
  try
    for ServiceName in FServices.Keys do
    begin
      ServiceList.Add(ServiceName);
    end;
    Result := ServiceList; // Return the list of registered services
  except
    ServiceList.Free; // Ensure to free in case of an exception
    raise;
  end;
end;

function TTranslationManager.GetSupportedLanguages: TDictionary<string, string>;
var
  ServicePair: TPair<string, ITranslationService>;
  LangPair: TPair<string, string>;
begin
  Result := TDictionary<string, string>.Create;

  for ServicePair in FServices do
  begin
    for LangPair in ServicePair.Value.GetSupportedLanguages do
    begin
      // Keep first occurrence (or override if you prefer)
      if not Result.ContainsKey(LangPair.Key) then
        Result.Add(LangPair.Key, LangPair.Value);
    end;
  end;
end;

function TTranslationManager.GetTranslateService(
  const AServiceName: string): ITranslationService;
begin
  if not FServices.TryGetValue(AServiceName, Result) then
    Result := nil;
end;

function TTranslationManager.IsServiceRegistered(
  AService: TTranslationService): Boolean;
var
  ServiceName: string;
begin
  ServiceName := TranslationServiceNames[AService];
  Result := FServices.ContainsKey(ServiceName); // Check if the service is registered
end;

function TTranslationManager.RegisterService(const AService: TTranslationService; const AApiKey: string): ITranslationService;
begin
  if not FServices.ContainsKey(TranslationServiceNames[AService]) then begin
    case AService of
      tsMicrosoftTranslate:
        Result := TMicrosoftTranslateService.create(AApiKey);
      tsGoogleTranslate, tsAmazonTranslate:
        raise Exception.Create('Not implement. ' + TranslationServiceNames[AService]);
      tsLibreTranslate:
        Result := TLibreTranslateService.create;
      tsDeepLXTranslate:
        Result := TDeepLXService.create;
      tsOllamaTranslate:
        Result := TOllamaService.create;
    end;
    if Result <> nil then
      FServices.Add(TranslationServiceNames[AService], Result);
  end else
    Result := FServices[TranslationServiceNames[AService]];
end;

function TTranslationManager.SupportsLanguage(const Lang: string): Boolean;
var
  ServicePair: TPair<string, ITranslationService>;
begin
  for ServicePair in FServices do
    if ServicePair.Value.SupportsLanguage(Lang) then
      Exit(True);

  Result := False;
end;

function TTranslationManager.TranslateByCode(const AServiceName, AText,
  ASourceLangCode, ADestLangCode: string): string;
var
  Service: ITranslationService;
begin
  Result := '';
  if FServices.TryGetValue(AServiceName, Service) then
    Service.Translate(AText, ASourceLangCode, ADestLangCode, Result, true)
  else
    raise Exception.Create('Translation service not found: ' + AServiceName);
end;

function TTranslationManager.TranslateByName(const AServiceName, AText, ASourceLangName, ADestLangName: string): string;
var
  Service: ITranslationService;
begin
  Result := '';
  if FServices.TryGetValue(AServiceName, Service) then
    Service.Translate(AText, ASourceLangName, ADestLangName, Result, false)
  else
    raise Exception.Create('Translation service not found: ' + AServiceName);
end;

end.


