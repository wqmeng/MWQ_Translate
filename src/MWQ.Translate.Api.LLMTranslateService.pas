unit MWQ.Translate.Api.LLMTranslateService;

interface

uses
  MWQ.Translate.Api.BaseTranslationService,
  MWQ.LLM.Provider,
  MWQ.LLM.Manager,
  Spring.Collections,
  MWQ.Translate.Types,
  MWQ.LLM.Types;

type
  TLLMService = class(TBaseTranslationService)
  private
    FLLMManager: TLLMManager;
    FProvider: ILLMProvider;
    function ParseTranslateLanguages(const ALangStr: string): TArray<TLangDef>;
  public
    constructor Create(const ALLMManager: TLLMManager);
    destructor Destroy; override;

    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function TranslateBatch(
        const ATexts: TArray<string>;
        const ASourceLang, ADestLang: string;
        const IsCode: Boolean = false
    ): TArray<string>; override;

    procedure SetProvider(const AProvider: string);
    procedure SetBaseURL(const ABaseUrl: string); override;
    procedure SetModel(const AModel: string); override;

    function GetModels: TArray<string>;

    function Translate(
        const AText, ASourceLang, ADestLang: string;
        var ATranslated: string;
        const IsCode: Boolean = false
    ): Boolean; override;
  public
    property LLMManager: TLLMManager read FLLMManager;
  end;

implementation

uses
  MWQ.LLM.Provider.Ollama,
  MWQ.LLM.Provider.LMStudio,
  // Assuming TLMLLaCppProvider exists or needs to be added later. For now, I will add a placeholder comment based on the request.
  System.SysUtils,
  MWQ.LLM.ModelDetector,
  System.Classes,
  Quick.Logger;

{ TLLMService }

function TLLMService.AddTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin
  Result := false;
end;

constructor TLLMService.Create(const ALLMManager: TLLMManager);
begin
  inherited Create;
  FLLMManager := ALLMManager;
end;

function TLLMService.DelTranslator(const ATransApiUrl,
  AApiKey: string): Boolean;
begin
  Result := false;
end;

destructor TLLMService.Destroy;
begin
  FProvider := nil;
end;

function TLLMService.GetModels: TArray<string>;
begin
  if FProvider = nil then
    Exit(nil);
  Result := FProvider.GetModels();
end;

procedure TLLMService.SetBaseURL(const ABaseUrl: string);
begin
  inherited;
  if Assigned(FProvider) then
    FProvider.SetBaseURL(ABaseUrl);
end;

function TLLMService.ParseTranslateLanguages(const ALangStr: string): TArray<TLangDef>;
var
  Lines: TStringList;
  i: Integer;
  Parts: TArray<string>;
  Code, Name: string;
  Dict: IDictionary<string, TLangDef>;
  Lang: TLangDef;
  Key: string;
begin
  if ALangStr = '' then
    Exit;

  Dict := TCollections.CreateDictionary<string, TLangDef>;
  Lines := TStringList.Create;
  try
    Lines.Text := ALangStr;

    for i := 0 to Lines.Count - 1 do begin
      Parts := Lines[i].Split([' '], 2);
      if Length(Parts) < 2 then
        Continue;

      Code := Parts[0];
      Name := Parts[1];

      Key := Name.ToLower;

      if not Dict.TryGetValue(Key, Lang) then begin
        Lang := MakeLang(Name, [Code], Code);
        Dict.Add(Key, Lang);
      end
      else begin
        Lang.Codes := Lang.Codes + [Code];
        Dict[Key] := Lang;
      end;
    end;

    SetLength(Result, Dict.Count);
    i := 0;
    for Lang in Dict.Values do begin
      Result[i] := Lang;
      Inc(i);
    end;

  finally
    Lines.Free;
    Dict := nil;
  end;
end;

procedure TLLMService.SetModel(const AModel: string);
var
  Langs: TArray<TLangDef>;
  Lang: TLangDef;
  Code: string;
  LLLMLang: string;
begin
  if FModel = AModel then
    Exit;

  inherited;

  // --------------------------------------------------
  // 1. Detect model capabilities
  // --------------------------------------------------
  FModelInfo := TLLMModelDetector.DetectModel(FModel);
  //  FProvider.
  //  FModelInfo := ModelInfo;

  //  // choose endpoint (centralized decision)
  //  case ModelInfo.Family of
  //    mtTranslateGemma,
  //    mtGemma:
  //      FEndPoint := efChat;
  //
  //    mtRiva:
  //      FEndPoint := efGenerate;
  //
  //    mtLlama,
  //    mtQwen,
  //    mtMistral:
  //      FEndPoint := efGenerate;
  //  else
  //    FEndPoint := efGenerate;
  //  end;

  LLLMLang := TLLMManager.GetTranslateLanguages(FModelInfo);

  // --------------------------------------------------
  // 2. Reset language maps
  // --------------------------------------------------
  FLanguageCodesToNames.Clear;
  FLanguageNamesToCodes.Clear;

  // --------------------------------------------------
  // 3. Translation-capable models
  // --------------------------------------------------
  if LLLMLang <> '' then begin
    Langs := ParseTranslateLanguages(LLLMLang);

    for Lang in Langs do begin
      for Code in Lang.Codes do begin
        FLanguageCodesToNames.AddOrSetValue(Code, Lang.Name);
        AddNameCode(LowerCase(Lang.Name), Code, FLanguageNamesToCodes);
      end;
    end;

    Exit;
  end;

  // --------------------------------------------------
  // 4. Default fallback language set (chat models)
  // --------------------------------------------------
  FLanguageCodesToNames.AddOrSetValue('en', 'English');
  FLanguageCodesToNames.AddOrSetValue('es', 'Spanish');
  FLanguageCodesToNames.AddOrSetValue('fr', 'French');
  FLanguageCodesToNames.AddOrSetValue('de', 'German');
  FLanguageCodesToNames.AddOrSetValue('zh', 'Chinese');
  FLanguageCodesToNames.AddOrSetValue('ja', 'Japanese');
  FLanguageCodesToNames.AddOrSetValue('ru', 'Russian');

  for var Pair in FLanguageCodesToNames do
    AddNameCode(LowerCase(Pair.Value), Pair.Key, FLanguageNamesToCodes);
end;

procedure TLLMService.SetProvider(const AProvider: string);
begin
  if Assigned(FProvider) and (FProvider.GetName = AProvider) then
    Exit;

  FProvider := FLLMManager.GetProvider(AProvider);
  if FProvider <> nil then
    Exit;
  if AProvider = 'ollama' then begin
    FProvider := TLLMProviderOllama.Create(Self.FLLMManager);
  end
  else if AProvider = 'lmstudio' then
    FProvider := TLLMProviderLMStudio.Create(Self.FLLMManager)
  else if AProvider = 'llama.cpp' then
    // Placeholder for Llama.cpp provider implementation
    // FProvider := TLMLLaCppProvider.Create
    raise Exception.Create('Llama.cpp provider is not yet implemented.')
  else
    raise Exception.Create('Unsupported provider: ' + AProvider);
end;

function TLLMService.Translate(
    const AText, ASourceLang, ADestLang: string;
    var ATranslated: string;
    const IsCode: Boolean
): Boolean;
var
  LSrcCode, LDstCode, LSrcName, LDstName: string;
  LResult: TLLMResult;
begin
  Result := False;
  ATranslated := '';

  if (AText = '') or (ADestLang = '') then
    Exit;

  // -------------------------------
  // Step 1: language mapping
  // -------------------------------
  if IsCode then
  begin
    LSrcCode := ASourceLang;
    LDstCode := ADestLang;
    LSrcName := Self.LanguageCodeToName(ASourceLang);
    LDstName := Self.LanguageCodeToName(ADestLang);
  end
  else
  begin
    if ASourceLang = '' then
      LSrcName := 'English'
    else
      LSrcName := ASourceLang;

    LDstName := ADestLang;

    LSrcCode := Self.LanguageNameToCode(LSrcName);
    LDstCode := Self.LanguageNameToCode(LDstName);
  end;

  // -------------------------------
  // Step 2: call provider
  // -------------------------------
  LResult :=
    FProvider.Translate(
      Self.FModel,
      AText,
      LSrcCode,
      LDstCode,
      LSrcName,
      LDstName
    );

  // -------------------------------
  // Step 3: handle result (IMPORTANT)
  // -------------------------------
  if not LResult.Success then
  begin
    Log('Translate failed: ' + LResult.ErrorMsg, etError);
    Exit(False);
  end;

  ATranslated := Trim(LResult.Content);

  Result := ATranslated <> '';
end;

function TLLMService.TranslateBatch(const ATexts: TArray<string>;
  const ASourceLang, ADestLang: string; const IsCode: Boolean): TArray<string>;
begin
  Result := [];
end;

end.
