program ConsoleDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MWQ.Translate.TranslationManager,
  MWQ.Translate.Types,
  MWQ.Translate.TranslationServiceInterface;

var
  Manager: TTranslationManager;
  Service: ITranslationService;
  Res: string;
begin
  try
    Manager := TTranslationManager.Create;
    try
      // Register LibreTranslate service (no API key required by default)
      Service := Manager.RegisterService(tsLibreTranslate, '');

      // Add a translator backend URL. The project ships with a default public instance,
      // but you can replace it with your own LibreTranslate endpoint.
      Service.AddTranslator('https://trans.zillyhuhn.com/translate', '');

      // Translate by language code
      Res := Manager.TranslateByCode('LibreTranslate', 'Hello world', 'en', 'zh');
      Writeln('en -> zh: ', Res);

      Res := Manager.TranslateByCode('LibreTranslate', '你好，世界', 'zh', 'en');
      Writeln('zh -> en: ', Res);
    finally
      Manager.Free;
    end;
  except
    on E: Exception do
      Writeln('Error: ', E.ClassName, ': ', E.Message);
  end;
end.
