unit MWQ.Translate.Types;

interface

type
  TTranslationService = (tsMicrosoftTranslate, tsGoogleTranslate, tsAmazonTranslate, tsLibreTranslate);

const
  TranslationServiceNames: array[TTranslationService] of string = (
    'MicrosoftTranslate',
    'GoogleTranslate',
    'AmazonTranslate',
    'LibreTranslate'
  );

implementation

end.

