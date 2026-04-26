unit MWQ.Translate.Types;

interface

type
  TTranslationService = (tsMicrosoftTranslate, tsGoogleTranslate, tsAmazonTranslate, tsLibreTranslate, tsDeepLXTranslate, tsOllamaTranslate, tsLLMTranslate);
  TLangDef = record
    Name: string;
    Codes: TArray<string>;
    DefaultCode: string;
  end;

const
  TranslationServiceNames: array[TTranslationService] of string = (
    'MicrosoftTranslate',
    'GoogleTranslate',
    'AmazonTranslate',
    'LibreTranslate',
    'DeepLX',
    'Ollama',
    'LLM'
  );

implementation

end.

