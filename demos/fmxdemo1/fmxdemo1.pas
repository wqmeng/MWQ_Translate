unit fmxdemo1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  System.Threading,
  System.SyncObjs,
  MWQ.Translate.TranslationManager,
  FMX.ListBox,
  FMX.Edit,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo;

type
  TForm13 = class(TForm)
    btnOllama: TButton;
    btnLibre: TButton;
    cbbModel: TComboBox;
    cbbSrcLang: TComboBox;
    cbbDstLang: TComboBox;
    edtSrc: TEdit;
    mmoResult: TMemo;
    rbOllama: TRadioButton;
    rbLibre: TRadioButton;
    rbDeepLX: TRadioButton;
    lblSrc: TLabel;
    lblDest: TLabel;
    rbLmstudio: TRadioButton;
    cbbPort: TComboBox;
    lblPort: TLabel;
    rbLlamaCpp: TRadioButton;
    procedure btnOllamaClick(Sender: TObject);
    procedure btnLibreClick(Sender: TObject);
    procedure cbbModelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbLibreChange(Sender: TObject);
    procedure rbLmstudioChange(Sender: TObject);
    procedure rbOllamaChange(Sender: TObject);
  private
    { Private declarations }
    FTranslationManager: TTranslationManager;
  public
  { Public declarations }
  end;

var
  Form13: TForm13;

implementation
uses
  MWQ.Translate.TranslationServiceInterface,
  MWQ.Translate.Types,
  MWQ.Ollama.Manager,
  System.Generics.Collections,
  System.StrUtils,
  MWQ.Translate.Api.LLMTranslateService,
  MWQ.LLM.Manager;
{$R *.fmx}

procedure TForm13.btnOllamaClick(Sender: TObject);
var
  LibreService, LDeepLXService, LLLMService: ITranslationService;
  //  TranslatedText: string;
  //  SubscriptionKey: string;
  LSrc, LDest: string;
begin
  try
    if (rbOllama.IsChecked or rbLmStudio.IsChecked) then begin
      mmoResult.Text := '';
      //      LOllamaService := FTranslationManager.RegisterService(TTranslationService.tsOllamaTranslate, '');

      //      LLLMService := FTranslationManager.RegisterService(TTranslationService.tsLLMTranslate, 'llm');
      LLLMService := FTranslationManager.GetTranslateService(TranslationServiceNames[tsLLMTranslate]);

      if LLLMService <> nil then begin
        if rbOllama.IsChecked then begin
          TLLMService(LLLMService).SetProvider('ollama');
//          TLLMService(LLLMService).SetBaseURL('http://localhost:11434');
        end
        else if rbLmStudio.IsChecked then begin
          TLLMService(LLLMService).SetProvider('lmstudio');
//          TLLMService(LLLMService).LLMManager.SetRateLimit(3);
//          TLLMService(LLLMService).SetBaseURL('http://localhost:1234');
        end;

        if (cbbSrcLang.ItemIndex < 0) or (cbbDstLang.ItemIndex < 0) then begin
          mmoResult.Text := 'Please select source and destination languages';
          Exit;
        end;

        LSrc := cbbSrcLang.ListItems[cbbSrcLang.ItemIndex].TagString;
        LDest := cbbDstLang.ListItems[cbbDstLang.ItemIndex].TagString;
        TTask.Run(
            procedure
            var
              LStr: string;
            begin
              LStr := edtSrc.Text;
              LStr :=
                  FTranslationManager.TranslateByCode(
                      TranslationServiceNames[TTranslationService.tsLLMTranslate],
                      LStr,
                      LSrc,
                      LDest
                  );

              TThread.Queue(nil, procedure begin mmoResult.Text := LStr; end)
            end
        );
      end;

    end
    else if rbLibre.IsChecked then begin
      LibreService := FTranslationManager.RegisterService(TTranslationService.tsLibreTranslate, '');
      LibreService.DelDefaultBaseTranslator;
      LibreService.AddTranslator('http://127.0.0.1:5000/translate', '');
    end
    else if rbDeepLX.IsChecked then begin
      LDeepLXService := FTranslationManager.RegisterService(TTranslationService.tsDeepLXTranslate, '');
    end;
  except
    Log.d('Translate fail.');
  end;
end;

procedure TForm13.btnLibreClick(Sender: TObject);
var
  LibreService: ITranslationService;
begin
  try
    LibreService := FTranslationManager.RegisterService(TTranslationService.tsLibreTranslate, '');
    // Ensure the default/public translator is present; replace URL if you run your own instance
    LibreService.AddTranslator('https://trans.zillyhuhn.com/translate', '');

    if FTranslationManager.IsServiceRegistered(TTranslationService.tsLibreTranslate) then
      mmoResult.Text :=
          FTranslationManager.TranslateByName(
              TranslationServiceNames[TTranslationService.tsLibreTranslate],
              edtSrc.Text,
              'English',
              'Chinese'
          );
  except
    on E: Exception do
      Log.d('Libre translate fail: ' + E.Message);
  end;
end;

// TStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;
function LangCompare(List: TStringList; Index1, Index2: Integer): Integer;
  function GetBaseLang(const S: string): string;
  var
    P: Integer;
  begin
    P := Pos('-', S);
    if P = 0 then
      Result := S
    else
      Result := Copy(S, 1, P - 1);
  end;
var
  L, R, LBase, RBase: string;
  LDash, RDash: Integer;
begin
  L := List.Names[Index1];
  R := List.Names[Index2];

  LBase := GetBaseLang(L);
  RBase := GetBaseLang(R);

  // 1. base language first
  Result := CompareStr(LBase, RBase);
  if Result <> 0 then
    Exit;

  // 2. same base �� shorter code first (Google-like behavior)
  LDash := Length(L);
  RDash := Length(R);

  Result := LDash - RDash;

  // 3. fallback: normal compare
  if Result = 0 then
    Result := CompareStr(L, R);
end;

procedure TForm13.cbbModelChange(Sender: TObject);
var
  LLlmService: ITranslationService;
  LCodesToNames: TDictionary<string, string>;
  Pair: TPair<string, string>;
  LName: string;
  SL: TStringList;
  I: Integer;
  Len, Lzh: Integer;
begin
  if (rbOllama.IsChecked or rbLmstudio.IsChecked) then begin
    mmoResult.Text := '';

    //     LOllamaService := FTranslationManager.RegisterService(TTranslationService.tsOllamaTranslate, '');
    LLlmService := FTranslationManager.GetTranslateService(TranslationServiceNames[tsLLMTranslate]);
    if not (cbbModel.Text.StartsWith('llama', True) or cbbModel.Text.StartsWith('translategemma', True)) then begin
      mmoResult.Text := 'Unsupported model, please install TranslateGemma or Llama';
      btnOllama.Enabled := false;
      Exit;
    end;

    btnOllama.Enabled := true;

    //    if LOllamaService.GetModel <> cbbModel.Text then
    //      LOllamaService.SetModel(cbbModel.Text);

    if LLlmService.GetModel <> cbbModel.Text then
      LLlmService.SetModel(cbbModel.Text);

    // --- reset UI ---
    cbbSrcLang.Clear;
    cbbDstLang.Clear;

    //    LCodesToNames := LOllamaService.GetSupportedLanguages;
    LCodesToNames := LLlmService.GetSupportedLanguages;

    if (LCodesToNames = nil) or (LCodesToNames.Count = 0) then
      Exit;

    SL := TStringList.Create;
    try
      SL.Duplicates := dupIgnore;

      // --- populate comboboxes ---
      for Pair in LCodesToNames do begin
        LName := Format('%s (%s)', [Pair.Value, Pair.Key]);
        SL.AddPair(Pair.Key, LName);
      end;

      SL.CustomSort(LangCompare);

      Len := -1;
      Lzh := -1;

      for I := 0 to SL.Count - 1 do begin
        LName := SL.ValueFromIndex[I];
        cbbSrcLang.Items.Add(LName);
        cbbDstLang.Items.Add(LName);

        LName := SL.Names[I];
        // store code using TagString-like mapping
        cbbSrcLang.ListItems[cbbDstLang.Items.Count - 1].TagString := LName;
        cbbDstLang.ListItems[cbbDstLang.Items.Count - 1].TagString := LName;
        if LName = 'en' then
          Len := I
        else if LName = 'zh-Hans' then
          Lzh := I;
      end;

      if Len >= 0 then
        cbbSrcLang.ItemIndex := Len;
      if Lzh >= 0 then
        cbbDstLang.ItemIndex := Lzh;
    finally
      SL.Free;
    end;
  end;
end;

procedure TForm13.FormCreate(Sender: TObject);
begin
  FTranslationManager := TTranslationManager.Create;
end;

procedure TForm13.rbLibreChange(Sender: TObject);
begin
  btnLibre.Enabled := rbLibre.IsChecked;
  btnOllama.Enabled := not rbLibre.IsChecked;
end;

procedure TForm13.rbLmstudioChange(Sender: TObject);
var
  LLLMService: ITranslationService;
begin
  cbbModel.Clear;

  LLLMService := FTranslationManager.RegisterService(TTranslationService.tsLLMTranslate, 'lmstudio');
  TLLMService(LLLMService).SetProvider('lmstudio');

  LLLMService := FTranslationManager.GetTranslateService(TranslationServiceNames[tsLLMTranslate]);
  cbbModel.Items.AddStrings(TLLMService(LLLMService).GetModels);
  if cbbModel.Count > 0 then
    cbbModel.ItemIndex := 0;

  btnLibre.Enabled := not rbOllama.IsChecked;
end;

procedure TForm13.rbOllamaChange(Sender: TObject);
var
  LLLMService: ITranslationService;
begin
  cbbModel.Clear;
//  cbbModel.Items.AddStrings(TOllamaManager.GetModelsList);

  LLLMService := FTranslationManager.RegisterService(TTranslationService.tsLLMTranslate, 'ollama');
  TLLMService(LLLMService).SetProvider('ollama');

  cbbModel.Items.AddStrings(TLLMService(LLLMService).GetModels);

  if cbbModel.Count > 0 then
    cbbModel.ItemIndex := 0;

  btnLibre.Enabled := not rbOllama.IsChecked;
end;

end.
