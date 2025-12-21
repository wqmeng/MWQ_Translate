unit MWQ.Translate.TranslationServiceInterface;

interface

uses
  System.SysUtils, System.Generics.Collections;

const
  TRANSLATION_FAIL_MSG = 'Translation failed.';
  TRANSLATION_FAIL_ALL_MSG = 'Translation failed with all available translators.';

type
  ITranslationService = interface
    ['{D6E2B1E5-5F77-4D7C-8FFD-9E5E8C8B7B75}']
    function Translate(const AText, ASourceLang, ADestLang: string; const IsCode: Boolean = false): string;
    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean;
    function LanguageNameToCode(const AName: string): string;
    function LanguageCodeToName(const ACode: string): string;
    function GetSupportedLanguages: TDictionary<string, string>;
    function SupportsLanguage(const Lang: string): Boolean;
    function SupportBatchTranslations: Boolean;
    function TranslateBatch(const ATexts: TArray<string>; const ASourceLang, ADestLang: string; const IsCode: Boolean = false): TArray<string>;
    function DelDefaultBaseTranslator: Boolean;
    procedure SetTimeOut(const AMicroSeconds: Integer);
    procedure SetRetry(const ARetry: Integer);
    procedure SetModel(const AModel: String);
    function GetModel: string;
  end;

implementation

end.
