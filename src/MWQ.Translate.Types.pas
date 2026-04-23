unit MWQ.Translate.Types;

interface

type
  TTranslationService = (tsMicrosoftTranslate, tsGoogleTranslate, tsAmazonTranslate, tsLibreTranslate, tsDeepLXTranslate, tsOllamaTranslate, tsLLMTranslate);

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

