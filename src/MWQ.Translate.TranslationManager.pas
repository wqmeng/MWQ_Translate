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

function TTranslationManager.TranslateByCode(const AServiceName, AText,
  ASourceLangCode, ADestLangCode: string): string;
var
  Service: ITranslationService;
begin
  if FServices.TryGetValue(AServiceName, Service) then
    Result := Service.Translate(AText, ASourceLangCode, ADestLangCode)
  else
    raise Exception.Create('Translation service not found: ' + AServiceName);
end;

function TTranslationManager.TranslateByName(const AServiceName, AText, ASourceLangName, ADestLangName: string): string;
var
  Service: ITranslationService;
begin
  if FServices.TryGetValue(AServiceName, Service) then
    Result := Service.Translate(AText, ASourceLangName, ADestLangName)
  else
    raise Exception.Create('Translation service not found: ' + AServiceName);
end;

end.


