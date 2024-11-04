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

    procedure RegisterService(const AServiceName: string; AService: ITranslationService);
    function Translate(const AServiceName, AText, ASourceLang, ADestLang: string): string;
    function GetRegisteredServices: TStringList;
    function GetTranslateService(const AServiceName: string): ITranslationService;
    function IsServiceRegistered(AService: TTranslationService): Boolean;
  end;

implementation

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

procedure TTranslationManager.RegisterService(const AServiceName: string; AService: ITranslationService);
begin
  FServices.Add(AServiceName, AService);
end;

function TTranslationManager.Translate(const AServiceName, AText, ASourceLang, ADestLang: string): string;
var
  Service: ITranslationService;
begin
  if FServices.TryGetValue(AServiceName, Service) then
    Result := Service.Translate(AText, ASourceLang, ADestLang)
  else
    raise Exception.Create('Translation service not found: ' + AServiceName);
end;

end.

