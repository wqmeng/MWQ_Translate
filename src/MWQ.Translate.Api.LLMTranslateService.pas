unit MWQ.Translate.Api.LLMTranslateService;

interface

uses
  MWQ.Translate.Api.BaseTranslationService,
  MWQ.LLM.Provider;

type
  TLLMService = class(TBaseTranslationService)
  private
    FProvider: ILLMProvider;
  public
    constructor Create;
    destructor Destroy;

    procedure SetProvider(const AProvider: string);
    procedure SetBaseURL(const ABaseUrl: string); override;
    procedure SetModel(const AModel: string); override;

    function Translate(
        const AText, ASourceLang, ADestLang: string;
        var ATranslated: string;
        const IsCode: Boolean = false
    ): Boolean; override;
  end;

implementation
uses
  MWQ.LLM.Provider.Ollama,
  MWQ.LLM.Provider.LMStudio,
  System.SysUtils;

{ TLLMService }

constructor TLLMService.Create;
begin
  Inherited;
end;

destructor TLLMService.Destroy;
begin
  FProvider := nil;
end;

procedure TLLMService.SetBaseURL(const ABaseUrl: string);
begin
  inherited;
  if Assigned(FProvider) then
    FProvider.SetBaseURL(ABaseUrl);
end;

procedure TLLMService.SetModel(const AModel: string);
begin
  inherited;
  Self.FModel := AModel;
end;

procedure TLLMService.SetProvider(const AProvider: string);
begin
  if AProvider = 'ollama' then
    FProvider := TLLMProviderOllama.Create
  else if AProvider = 'lmstudio' then
    FProvider := TLMStudioProvider.Create
  else
    raise Exception.Create('Unsupported provider');
end;

function TLLMService.Translate(const AText, ASourceLang, ADestLang: string;
  var ATranslated: string; const IsCode: Boolean): Boolean;
begin
  ATranslated := FProvider.Translate(Self.FModel, AText, ASourceLang, ADestLang, '', '');

  Result := ATranslated <> '';
end;

end.
