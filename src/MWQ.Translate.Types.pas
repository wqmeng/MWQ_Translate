unit MWQ.Translate.Types;

interface

type
  TTranslationService = (tsMicrosoftTranslate, tsGoogleTranslate, tsAmazonTranslate, tsLibreTranslate, tsDeepLXTranslate, tsOllamaTranslate);

const
  TranslationServiceNames: array[TTranslationService] of string = (
    'MicrosoftTranslate',
    'GoogleTranslate',
    'AmazonTranslate',
    'LibreTranslate',
    'DeepLX',
    'Ollama'
  );

implementation

end.

