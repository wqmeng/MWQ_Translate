unit fmxdemo1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Threading, System.SyncObjs, MWQ.Translate.TranslationManager;

type
  TForm13 = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    btn1: TButton;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTranslationManager: TTranslationManager;
  public
    { Public declarations }
  end;

var
  Form13: TForm13;

implementation
  uses MWQ.Translate.TranslationServiceInterface, MWQ.Translate.Types;
{$R *.fmx}

procedure TForm13.btn1Click(Sender: TObject);
var
  LibreService, LDeepLXService, LOllamaService: ITranslationService;
  TranslatedText: string;
  SubscriptionKey: string;
begin
  try
//    LibreService := LTranslationManager.RegisterService(TTranslationService.tsLibreTranslate, '');
//    LibreService.DelDefaultBaseTranslator;
//    LibreService.AddTranslator('http://127.0.0.1:5000/translate', '');
//    MicrosoftService := LTranslationManager.RegisterService(TTranslationService.tsMicrosoftTranslate, 'YOUR_SUBSCRIPTION_KEY');
//    LDeepLXService := LTranslationManager.RegisterService(TTranslationService.tsDeepLXTranslate, '');
    lbl2.Text := '';
    LOllamaService := FTranslationManager.RegisterService(TTranslationService.tsOllamaTranslate, '');
    if LOllamaService.GetModel <> 'llama3.1:latest' then
      LOllamaService.SetModel('llama3.1:latest');

    if FTranslationManager.IsServiceRegistered(TTranslationService.tsOllamaTranslate) then begin
      TTask.Run(
        procedure
        var
          LStr: string;
        begin
          LStr := lbl1.Text;
          LStr := FTranslationManager.TranslateByName(TranslationServiceNames[TTranslationService.tsOllamaTranslate], LStr, 'English', 'Chinese');

          TThread.Queue(nil,
            procedure begin
              lbl2.Text := LStr;
            end
          )
        end
      );
    end;
  except
    Log.d('Translate fail.');
  end;
end;

procedure TForm13.btn2Click(Sender: TObject);
var
  LibreService: ITranslationService;
begin
  try
    LibreService := FTranslationManager.RegisterService(TTranslationService.tsLibreTranslate, '');
    // Ensure the default/public translator is present; replace URL if you run your own instance
    LibreService.AddTranslator('https://trans.zillyhuhn.com/translate', '');

    if FTranslationManager.IsServiceRegistered(TTranslationService.tsLibreTranslate) then
      lbl2.Text := FTranslationManager.TranslateByName(TranslationServiceNames[TTranslationService.tsLibreTranslate], lbl1.Text, 'English', 'Chinese');
  except
    on E: Exception do
      Log.d('Libre translate fail: ' + E.Message);
  end;
end;

procedure TForm13.FormCreate(Sender: TObject);
begin
  FTranslationManager := TTranslationManager.Create;
end;

end.
