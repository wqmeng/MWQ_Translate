unit fmxdemo1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm13 = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    btn1: TButton;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form13: TForm13;

implementation
  uses MWQ.Translate.TranslationManager, MWQ.Translate.TranslationServiceInterface, MWQ.Translate.Types;
{$R *.fmx}

procedure TForm13.btn1Click(Sender: TObject);
var
  LibreService, LDeepLXService, LOllamaService: ITranslationService;
  TranslatedText: string;
  SubscriptionKey: string;
  LTranslationManager: TTranslationManager;
begin
  LTranslationManager := TTranslationManager.Create;
  try
//    LibreService := LTranslationManager.RegisterService(TTranslationService.tsLibreTranslate, '');
//    LibreService.DelDefaultBaseTranslator;
//    LibreService.AddTranslator('http://127.0.0.1:5000/translate', '');
//    MicrosoftService := LTranslationManager.RegisterService(TTranslationService.tsMicrosoftTranslate, 'YOUR_SUBSCRIPTION_KEY');
//    LDeepLXService := LTranslationManager.RegisterService(TTranslationService.tsDeepLXTranslate, '');

    LOllamaService := LTranslationManager.RegisterService(TTranslationService.tsOllamaTranslate, '');
    LOllamaService.SetModel('hf-mirror.com/SpaceTimee/Suri-Qwen-3.1-4B-Uncensored-GGUF:Q4_K_M');

    if LTranslationManager.IsServiceRegistered(TTranslationService.tsOllamaTranslate) then
      lbl2.Text := LTranslationManager.TranslateByName(TranslationServiceNames[TTranslationService.tsOllamaTranslate], lbl1.Text, 'English', 'Chinese');
  except
    Log.d('Translate fail.');
  end;

  LTranslationManager.free;
end;

procedure TForm13.btn2Click(Sender: TObject);
var
  LTranslationManager: TTranslationManager;
  LibreService: ITranslationService;
begin
  LTranslationManager := TTranslationManager.Create;
  try
    LibreService := LTranslationManager.RegisterService(TTranslationService.tsLibreTranslate, '');
    // Ensure the default/public translator is present; replace URL if you run your own instance
    LibreService.AddTranslator('https://trans.zillyhuhn.com/translate', '');

    if LTranslationManager.IsServiceRegistered(TTranslationService.tsLibreTranslate) then
      lbl2.Text := LTranslationManager.TranslateByName(TranslationServiceNames[TTranslationService.tsLibreTranslate], lbl1.Text, 'English', 'Chinese');
  except
    on E: Exception do
      Log.d('Libre translate fail: ' + E.Message);
  end;
  LTranslationManager.Free;
end;

end.
