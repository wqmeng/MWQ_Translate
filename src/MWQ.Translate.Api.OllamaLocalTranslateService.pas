unit MWQ.Translate.Api.OllamaLocalTranslateService;

interface

uses
  System.Classes,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.Net.URLClient,
  System.JSON,
  System.Generics.Collections,
  MWQ.Translate.TranslationServiceInterface,
  MWQ.Translate.Api.BaseTranslationService,
  MWQ.Ollama.Types,
  MWQ.Ollama.PromptBuilder;

type
  /// Simple pair type for passing messages: Role -> Content
  TMessagePair = TPair<string, string>;
  TMessageArray = TArray<TMessagePair>;

  TJSONRequestBuilder = class
  public
    class function BuildTranslationRequest(const AModel, APrompt: string): TJSONObject;
  end;

  TOllamaService = class(TBaseTranslationService)
  private
    FModelType: TOllamaModelType;
    FEndPoint: TEndpointFlavor;
    function GetTranslateGemmaLanguages: TArray<TLangDef>;
  protected
    procedure InitializeLanguageMappings; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Translate(
        const AText, ASourceLang, ADestLang: string;
        var ATranslated: string;
        const IsCode: Boolean = false
    ): Boolean; override;
    function AddTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function DelTranslator(const ATransApiUrl, AApiKey: string): Boolean; override;
    function SupportBatchTranslations: Boolean; override;
    function TranslateBatch(
        const ATexts: TArray<string>;
        const ASourceLang, ADestLang: string;
        const IsCode: Boolean = false
    ): TArray<string>; override;
    procedure SetBaseURL(const ABaseUrl: string); override;
    procedure SetModel(const AModel: string); override;
    procedure SetEndpointFlavor(const AEndPoint: TEndpointFlavor);
  end;

implementation

uses
  System.SysUtils,
  Quick.Logger,
  MWQ.Ollama.ResponseParser,
  MWQ.Ollama.Manager,
  System.StrUtils;

{ TOllamaService }

function TOllamaService.AddTranslator(const ATransApiUrl, AApiKey: string): Boolean;
begin
  Result := True;
end;

constructor TOllamaService.Create;
begin
  inherited;
  FBASE_URL := VOllama_Base_Url;
  FModel := 'llama3:latest';
  FEndPoint := TEndpointFlavor.efGenerate;
end;

function TOllamaService.DelTranslator(const ATransApiUrl, AApiKey: string): Boolean;
begin
  Result := True;
end;

destructor TOllamaService.Destroy;
begin
  inherited;
end;

procedure TOllamaService.InitializeLanguageMappings;
begin
  inherited;

end;

procedure TOllamaService.SetBaseURL(const ABaseUrl: string);
begin
  inherited;

  if VOllama_Base_Url <> Self.FBASE_URL then
    VOllama_Base_Url := FBASE_URL;
end;

procedure TOllamaService.SetEndpointFlavor(const AEndPoint: TEndpointFlavor);
begin
  FEndPoint := AEndPoint;
end;

function TOllamaService.GetTranslateGemmaLanguages: TArray<TLangDef>;
const
  RAW_LANG_LIST =
      'aa Afar'#10
          + 'aa-DJ Afar'#10
          + 'aa-ER Afar'#10
          + 'ab Abkhazian'#10
          + 'af Afrikaans'#10
          + 'af-NA Afrikaans'#10
          + 'ak Akan'#10
          + 'am Amharic'#10
          + 'an Aragonese'#10
          + 'ar Arabic'#10
          + 'ar-AE Arabic'#10
          + 'ar-BH Arabic'#10
          + 'ar-DJ Arabic'#10
          + 'ar-DZ Arabic'#10
          + 'ar-EG Arabic'#10
          + 'ar-EH Arabic'#10
          + 'ar-ER Arabic'#10
          + 'ar-IL Arabic'#10
          + 'ar-IQ Arabic'#10
          + 'ar-JO Arabic'#10
          + 'ar-KM Arabic'#10
          + 'ar-KW Arabic'#10
          + 'ar-LB Arabic'#10
          + 'ar-LY Arabic'#10
          + 'ar-MA Arabic'#10
          + 'ar-MR Arabic'#10
          + 'ar-OM Arabic'#10
          + 'ar-PS Arabic'#10
          + 'ar-QA Arabic'#10
          + 'ar-SA Arabic'#10
          + 'ar-SD Arabic'#10
          + 'ar-SO Arabic'#10
          + 'ar-SS Arabic'#10
          + 'ar-SY Arabic'#10
          + 'ar-TD Arabic'#10
          + 'ar-TN Arabic'#10
          + 'ar-YE Arabic'#10
          + 'as Assamese'#10
          + 'az Azerbaijani'#10
          + 'az-Arab Azerbaijani'#10
          + 'az-Arab-IQ Azerbaijani'#10
          + 'az-Arab-TR Azerbaijani'#10
          + 'az-Cyrl Azerbaijani'#10
          + 'az-Latn Azerbaijani'#10
          + 'ba Bashkir'#10
          + 'be Belarusian'#10
          + 'be-tarask Belarusian'#10
          + 'bg Bulgarian'#10
          + 'bg-BG Bulgarian'#10
          + 'bm Bambara'#10
          + 'bm-Nkoo Bambara'#10
          + 'bn Bengali'#10
          + 'bn-IN Bengali'#10
          + 'bo Tibetan'#10
          + 'bo-IN Tibetan'#10
          + 'br Breton'#10
          + 'bs Bosnian'#10
          + 'bs-Cyrl Bosnian'#10
          + 'bs-Latn Bosnian'#10
          + 'ca Catalan'#10
          + 'ca-AD Catalan'#10
          + 'ca-ES Catalan'#10
          + 'ca-FR Catalan'#10
          + 'ca-IT Catalan'#10
          + 'ce Chechen'#10
          + 'co Corsican'#10
          + 'cs Czech'#10
          + 'cs-CZ Czech'#10
          + 'cv Chuvash'#10
          + 'cy Welsh'#10
          + 'da Danish'#10
          + 'da-DK Danish'#10
          + 'da-GL Danish'#10
          + 'de German'#10
          + 'de-AT German'#10
          + 'de-BE German'#10
          + 'de-CH German'#10
          + 'de-DE German'#10
          + 'de-IT German'#10
          + 'de-LI German'#10
          + 'de-LU German'#10
          + 'dv Divehi'#10
          + 'dz Dzongkha'#10
          + 'ee Ewe'#10
          + 'ee-TG Ewe'#10
          + 'el Greek'#10
          + 'el-CY Greek'#10
          + 'el-GR Greek'#10
          + 'el-polyton Greek'#10
          + 'en English'#10
          + 'en-AE English'#10
          + 'en-AG English'#10
          + 'en-AI English'#10
          + 'en-AS English'#10
          + 'en-AT English'#10
          + 'en-AU English'#10
          + 'en-BB English'#10
          + 'en-BE English'#10
          + 'en-BI English'#10
          + 'en-BM English'#10
          + 'en-BS English'#10
          + 'en-BW English'#10
          + 'en-BZ English'#10
          + 'en-CA English'#10
          + 'en-CC English'#10
          + 'en-CH English'#10
          + 'en-CK English'#10
          + 'en-CM English'#10
          + 'en-CX English'#10
          + 'en-CY English'#10
          + 'en-CZ English'#10
          + 'en-DE English'#10
          + 'en-DG English'#10
          + 'en-DK English'#10
          + 'en-DM English'#10
          + 'en-ER English'#10
          + 'en-ES English'#10
          + 'en-FI English'#10
          + 'en-FJ English'#10
          + 'en-FK English'#10
          + 'en-FM English'#10
          + 'en-FR English'#10
          + 'en-GB English'#10
          + 'en-GD English'#10
          + 'en-GG English'#10
          + 'en-GH English'#10
          + 'en-GI English'#10
          + 'en-GM English'#10
          + 'en-GS English'#10
          + 'en-GU English'#10
          + 'en-GY English'#10
          + 'en-HK English'#10
          + 'en-HU English'#10
          + 'en-ID English'#10
          + 'en-IE English'#10
          + 'en-IL English'#10
          + 'en-IM English'#10
          + 'en-IN English'#10
          + 'en-IO English'#10
          + 'en-IT English'#10
          + 'en-JE English'#10
          + 'en-JM English'#10
          + 'en-KE English'#10
          + 'en-KI English'#10
          + 'en-KN English'#10
          + 'en-KY English'#10
          + 'en-LC English'#10
          + 'en-LR English'#10
          + 'en-LS English'#10
          + 'en-MG English'#10
          + 'en-MH English'#10
          + 'en-MO English'#10
          + 'en-MP English'#10
          + 'en-MS English'#10
          + 'en-MT English'#10
          + 'en-MU English'#10
          + 'en-MV English'#10
          + 'en-MW English'#10
          + 'en-MY English'#10
          + 'en-NA English'#10
          + 'en-NF English'#10
          + 'en-NG English'#10
          + 'en-NL English'#10
          + 'en-NO English'#10
          + 'en-NR English'#10
          + 'en-NU English'#10
          + 'en-NZ English'#10
          + 'en-PG English'#10
          + 'en-PH English'#10
          + 'en-PK English'#10
          + 'en-PL English'#10
          + 'en-PN English'#10
          + 'en-PR English'#10
          + 'en-PT English'#10
          + 'en-PW English'#10
          + 'en-RO English'#10
          + 'en-RW English'#10
          + 'en-SB English'#10
          + 'en-SC English'#10
          + 'en-SD English'#10
          + 'en-SE English'#10
          + 'en-SG English'#10
          + 'en-SH English'#10
          + 'en-SI English'#10
          + 'en-SK English'#10
          + 'en-SL English'#10
          + 'en-SS English'#10
          + 'en-SX English'#10
          + 'en-SZ English'#10
          + 'en-TC English'#10
          + 'en-TK English'#10
          + 'en-TO English'#10
          + 'en-TT English'#10
          + 'en-TV English'#10
          + 'en-TZ English'#10
          + 'en-UG English'#10
          + 'en-UM English'#10
          + 'en-VC English'#10
          + 'en-VG English'#10
          + 'en-VI English'#10
          + 'en-VU English'#10
          + 'en-WS English'#10
          + 'en-ZA English'#10
          + 'en-ZM English'#10
          + 'en-ZW English'#10
          + 'eo Esperanto'#10
          + 'es Spanish'#10
          + 'es-AR Spanish'#10
          + 'es-BO Spanish'#10
          + 'es-BR Spanish'#10
          + 'es-BZ Spanish'#10
          + 'es-CL Spanish'#10
          + 'es-CO Spanish'#10
          + 'es-CR Spanish'#10
          + 'es-CU Spanish'#10
          + 'es-DO Spanish'#10
          + 'es-EA Spanish'#10
          + 'es-EC Spanish'#10
          + 'es-ES Spanish'#10
          + 'es-GQ Spanish'#10
          + 'es-GT Spanish'#10
          + 'es-HN Spanish'#10
          + 'es-IC Spanish'#10
          + 'es-MX Spanish'#10
          + 'es-NI Spanish'#10
          + 'es-PA Spanish'#10
          + 'es-PE Spanish'#10
          + 'es-PH Spanish'#10
          + 'es-PR Spanish'#10
          + 'es-PY Spanish'#10
          + 'es-SV Spanish'#10
          + 'es-US Spanish'#10
          + 'es-UY Spanish'#10
          + 'es-VE Spanish'#10
          + 'et Estonian'#10
          + 'et-EE Estonian'#10
          + 'eu Basque'#10
          + 'fa Persian'#10
          + 'fa-AF Persian'#10
          + 'fa-IR Persian'#10
          + 'ff Fulah'#10
          + 'ff-Adlm Fulah'#10
          + 'ff-Adlm-BF Fulah'#10
          + 'ff-Adlm-CM Fulah'#10
          + 'ff-Adlm-GH Fulah'#10
          + 'ff-Adlm-GM Fulah'#10
          + 'ff-Adlm-GW Fulah'#10
          + 'ff-Adlm-LR Fulah'#10
          + 'ff-Adlm-MR Fulah'#10
          + 'ff-Adlm-NE Fulah'#10
          + 'ff-Adlm-NG Fulah'#10
          + 'ff-Adlm-SL Fulah'#10
          + 'ff-Adlm-SN Fulah'#10
          + 'ff-Latn Fulah'#10
          + 'ff-Latn-BF Fulah'#10
          + 'ff-Latn-CM Fulah'#10
          + 'ff-Latn-GH Fulah'#10
          + 'ff-Latn-GM Fulah'#10
          + 'ff-Latn-GN Fulah'#10
          + 'ff-Latn-GW Fulah'#10
          + 'ff-Latn-LR Fulah'#10
          + 'ff-Latn-MR Fulah'#10
          + 'ff-Latn-NE Fulah'#10
          + 'ff-Latn-NG Fulah'#10
          + 'ff-Latn-SL Fulah'#10
          + 'fi Finnish'#10
          + 'fi-FI Finnish'#10
          + 'fil-PH Filipino'#10
          + 'fo Faroese'#10
          + 'fo-DK Faroese'#10
          + 'fr French'#10
          + 'fr-BE French'#10
          + 'fr-BF French'#10
          + 'fr-BI French'#10
          + 'fr-BJ French'#10
          + 'fr-BL French'#10
          + 'fr-CA French'#10
          + 'fr-CD French'#10
          + 'fr-CF French'#10
          + 'fr-CG French'#10
          + 'fr-CH French'#10
          + 'fr-CI French'#10
          + 'fr-CM French'#10
          + 'fr-DJ French'#10
          + 'fr-DZ French'#10
          + 'fr-FR French'#10
          + 'fr-GA French'#10
          + 'fr-GF French'#10
          + 'fr-GN French'#10
          + 'fr-GP French'#10
          + 'fr-GQ French'#10
          + 'fr-HT French'#10
          + 'fr-KM French'#10
          + 'fr-LU French'#10
          + 'fr-MA French'#10
          + 'fr-MC French'#10
          + 'fr-MF French'#10
          + 'fr-MG French'#10
          + 'fr-ML French'#10
          + 'fr-MQ French'#10
          + 'fr-MR French'#10
          + 'fr-MU French'#10
          + 'fr-NC French'#10
          + 'fr-NE French'#10
          + 'fr-PF French'#10
          + 'fr-PM French'#10
          + 'fr-RE French'#10
          + 'fr-RW French'#10
          + 'fr-SC French'#10
          + 'fr-SN French'#10
          + 'fr-SY French'#10
          + 'fr-TD French'#10
          + 'fr-TG French'#10
          + 'fr-TN French'#10
          + 'fr-VU French'#10
          + 'fr-WF French'#10
          + 'fr-YT French'#10
          + 'fy Western Frisian'#10
          + 'ga Irish'#10
          + 'ga-GB Irish'#10
          + 'gd Scottish Gaelic'#10
          + 'gl Galician'#10
          + 'gn Guarani'#10
          + 'gu Gujarati'#10
          + 'gu-IN Gujarati'#10
          + 'gv Manx'#10
          + 'ha Hausa'#10
          + 'ha-Arab Hausa'#10
          + 'ha-Arab-SD Hausa'#10
          + 'ha-GH Hausa'#10
          + 'ha-NE Hausa'#10
          + 'he Hebrew'#10
          + 'he-IL Hebrew'#10
          + 'hi Hindi'#10
          + 'hi-IN Hindi'#10
          + 'hi-Latn Hindi'#10
          + 'hr Croatian'#10
          + 'hr-BA Croatian'#10
          + 'hr-HR Croatian'#10
          + 'ht Haitian'#10
          + 'hu Hungarian'#10
          + 'hu-HU Hungarian'#10
          + 'hy Armenian'#10
          + 'ia Interlingua'#10
          + 'id Indonesian'#10
          + 'id-ID Indonesian'#10
          + 'ie Interlingue'#10
          + 'ig Igbo'#10
          + 'ii Sichuan Yi'#10
          + 'ik Inupiaq'#10
          + 'io Ido'#10
          + 'is Icelandic'#10
          + 'it Italian'#10
          + 'it-CH Italian'#10
          + 'it-IT Italian'#10
          + 'it-SM Italian'#10
          + 'it-VA Italian'#10
          + 'iu Inuktitut'#10
          + 'iu-Latn Inuktitut'#10
          + 'ja Japanese'#10
          + 'ja-JP Japanese'#10
          + 'jv Javanese'#10
          + 'ka Georgian'#10
          + 'ki Kikuyu'#10
          + 'kk Kazakh'#10
          + 'kk-Arab Kazakh'#10
          + 'kk-Cyrl Kazakh'#10
          + 'kk-KZ Kazakh'#10
          + 'kl Kalaallisut'#10
          + 'km Central Khmer'#10
          + 'kn Kannada'#10
          + 'kn-IN Kannada'#10
          + 'ko Korean'#10
          + 'ko-CN Korean'#10
          + 'ko-KP Korean'#10
          + 'ko-KR Korean'#10
          + 'ks Kashmiri'#10
          + 'ks-Arab Kashmiri'#10
          + 'ks-Deva Kashmiri'#10
          + 'ku Kurdish'#10
          + 'kw Cornish'#10
          + 'ky Kyrgyz'#10
          + 'la Latin'#10
          + 'lb Luxembourgish'#10
          + 'lg Ganda'#10
          + 'ln Lingala'#10
          + 'ln-AO Lingala'#10
          + 'ln-CF Lingala'#10
          + 'ln-CG Lingala'#10
          + 'lo Lao'#10
          + 'lt Lithuanian'#10
          + 'lt-LT Lithuanian'#10
          + 'lu Luba-Katanga'#10
          + 'lv Latvian'#10
          + 'lv-LV Latvian'#10
          + 'mg Malagasy'#10
          + 'mi Maori'#10
          + 'mk Macedonian'#10
          + 'ml Malayalam'#10
          + 'ml-IN Malayalam'#10
          + 'mn Mongolian'#10
          + 'mn-Mong Mongolian'#10
          + 'mn-Mong-MN Mongolian'#10
          + 'mr Marathi'#10
          + 'mr-IN Marathi'#10
          + 'ms Malay'#10
          + 'ms-Arab Malay'#10
          + 'ms-Arab-BN Malay'#10
          + 'ms-BN Malay'#10
          + 'ms-ID Malay'#10
          + 'ms-SG Malay'#10
          + 'mt Maltese'#10
          + 'my Burmese'#10
          + 'nb Norwegian Bokmål'#10
          + 'nb-SJ Norwegian Bokmål'#10
          + 'nd North Ndebele'#10
          + 'ne Nepali'#10
          + 'ne-IN Nepali'#10
          + 'nl Dutch'#10
          + 'nl-AW Dutch'#10
          + 'nl-BE Dutch'#10
          + 'nl-BQ Dutch'#10
          + 'nl-CW Dutch'#10
          + 'nl-NL Dutch'#10
          + 'nl-SR Dutch'#10
          + 'nl-SX Dutch'#10
          + 'nn Norwegian Nynorsk'#10
          + 'no Norwegian'#10
          + 'no-NO Norwegian'#10
          + 'nr South Ndebele'#10
          + 'nv Navajo'#10
          + 'ny Chichewa'#10
          + 'oc Occitan'#10
          + 'oc-ES Occitan'#10
          + 'om Oromo'#10
          + 'om-KE Oromo'#10
          + 'or Oriya'#10
          + 'os Ossetian'#10
          + 'os-RU Ossetian'#10
          + 'pa Punjabi'#10
          + 'pa-IN Punjabi'#10
          + 'pa-Arab Punjabi'#10
          + 'pa-Guru Punjabi'#10
          + 'pl Polish'#10
          + 'pl-PL Polish'#10
          + 'ps Pashto'#10
          + 'ps-PK Pashto'#10
          + 'pt Portuguese'#10
          + 'pt-AO Portuguese'#10
          + 'pt-BR Portuguese'#10
          + 'pt-CH Portuguese'#10
          + 'pt-CV Portuguese'#10
          + 'pt-GQ Portuguese'#10
          + 'pt-GW Portuguese'#10
          + 'pt-LU Portuguese'#10
          + 'pt-MO Portuguese'#10
          + 'pt-MZ Portuguese'#10
          + 'pt-PT Portuguese'#10
          + 'pt-ST Portuguese'#10
          + 'pt-TL Portuguese'#10
          + 'qu Quechua'#10
          + 'qu-BO Quechua'#10
          + 'qu-EC Quechua'#10
          + 'rm Romansh'#10
          + 'rn Rundi'#10
          + 'ro Romanian'#10
          + 'ro-MD Romanian'#10
          + 'ro-RO Romanian'#10
          + 'ru Russian'#10
          + 'ru-BY Russian'#10
          + 'ru-KG Russian'#10
          + 'ru-KZ Russian'#10
          + 'ru-MD Russian'#10
          + 'ru-RU Russian'#10
          + 'ru-UA Russian'#10
          + 'rw Kinyarwanda'#10
          + 'sa Sanskrit'#10
          + 'sc Sardinian'#10
          + 'sd Sindhi'#10
          + 'sd-Arab Sindhi'#10
          + 'sd-Deva Sindhi'#10
          + 'se Northern Sami'#10
          + 'se-FI Northern Sami'#10
          + 'se-SE Northern Sami'#10
          + 'sg Sango'#10
          + 'si Sinhala'#10
          + 'sk Slovak'#10
          + 'sk-SK Slovak'#10
          + 'sl Slovenian'#10
          + 'sl-SI Slovenian'#10
          + 'sn Shona'#10
          + 'so Somali'#10
          + 'so-DJ Somali'#10
          + 'so-ET Somali'#10
          + 'so-KE Somali'#10
          + 'sq Albanian'#10
          + 'sq-MK Albanian'#10
          + 'sq-XK Albanian'#10
          + 'sr Serbian'#10
          + 'sr-RS Serbian'#10
          + 'sr-Cyrl Serbian'#10
          + 'sr-Cyrl-BA Serbian'#10
          + 'sr-Cyrl-ME Serbian'#10
          + 'sr-Cyrl-XK Serbian'#10
          + 'sr-Latn Serbian'#10
          + 'sr-Latn-BA Serbian'#10
          + 'sr-Latn-ME Serbian'#10
          + 'sr-Latn-XK Serbian'#10
          + 'ss Swati'#10
          + 'ss-SZ Swati'#10
          + 'st Southern Sotho'#10
          + 'st-LS Southern Sotho'#10
          + 'su Sundanese'#10
          + 'su-Latn Sundanese'#10
          + 'sv Swedish'#10
          + 'sv-AX Swedish'#10
          + 'sv-FI Swedish'#10
          + 'sv-SE Swedish'#10
          + 'sw Swahili'#10
          + 'sw-CD Swahili'#10
          + 'sw-KE Swahili'#10
          + 'sw-TZ Swahili'#10
          + 'sw-UG Swahili'#10
          + 'ta Tamil'#10
          + 'ta-IN Tamil'#10
          + 'ta-LK Tamil'#10
          + 'ta-MY Tamil'#10
          + 'ta-SG Tamil'#10
          + 'te Telugu'#10
          + 'te-IN Telugu'#10
          + 'tg Tajik'#10
          + 'th Thai'#10
          + 'th-TH Thai'#10
          + 'ti Tigrinya'#10
          + 'ti-ER Tigrinya'#10
          + 'tk Turkmen'#10
          + 'tl Tagalog'#10
          + 'tn Tswana'#10
          + 'tn-BW Tswana'#10
          + 'to Tonga'#10
          + 'tr Turkish'#10
          + 'tr-CY Turkish'#10
          + 'tr-TR Turkish'#10
          + 'ts Tsonga'#10
          + 'tt Tatar'#10
          + 'ug Uyghur'#10
          + 'uk Ukrainian'#10
          + 'uk-UA Ukrainian'#10
          + 'ur Urdu'#10
          + 'ur-IN Urdu'#10
          + 'ur-PK Urdu'#10
          + 'uz Uzbek'#10
          + 'uz-Arab Uzbek'#10
          + 'uz-Cyrl Uzbek'#10
          + 'uz-Latn Uzbek'#10
          + 've Venda'#10
          + 'vi Vietnamese'#10
          + 'vi-VN Vietnamese'#10
          + 'vo Volapük'#10
          + 'wa Walloon'#10
          + 'wo Wolof'#10
          + 'xh Xhosa'#10
          + 'yi Yiddish'#10
          + 'yo Yoruba'#10
          + 'yo-BJ Yoruba'#10
          + 'za Zhuang'#10
          + 'zh Chinese'#10
          + 'zh-CH Chinese'#10
          + 'zh-TW Chinese'#10
          + 'zh-Hans Chinese'#10
          + 'zh-Hans-HK Chinese'#10
          + 'zh-Hans-MO Chinese'#10
          + 'zh-Hans-MY Chinese'#10
          + 'zh-Hans-SG Chinese'#10
          + 'zh-Hant Chinese'#10
          + 'zh-Hant-HK Chinese'#10
          + 'zh-Hant-MO Chinese'#10
          + 'zh-Hant-MY Chinese'#10
          + 'zh-Latn Chinese'#10
          + 'zu Zulu'#10
          + 'zu-ZA Zulu';
var
  Lines: TStringList;
  i: Integer;
  Parts: TArray<string>;
  Code, Name: string;
  Dict: TDictionary<string, TLangDef>;
  Lang: TLangDef;
  Key: string;
begin
  Dict := TDictionary<string, TLangDef>.Create;
  Lines := TStringList.Create;
  try
    Lines.Text := RAW_LANG_LIST;

    for i := 0 to Lines.Count - 1 do
    begin
      Parts := Lines[i].Split([' '], 2);
      if Length(Parts) < 2 then
        Continue;

      Code := Parts[0];
      Name := Parts[1];

      Key := Name.ToLower;

      if not Dict.TryGetValue(Key, Lang) then
      begin
        Lang := MakeLang(Name, [Code], Code);
        Dict.Add(Key, Lang);
      end
      else
      begin
        Lang.Codes := Lang.Codes + [Code];
        Dict.AddOrSetValue(Key, Lang);
      end;
    end;

    SetLength(Result, Dict.Count);
    i := 0;
    for Lang in Dict.Values do
    begin
      Result[i] := Lang;
      Inc(i);
    end;

  finally
    Lines.Free;
    Dict.Free;
  end;
end;

procedure TOllamaService.SetModel(const AModel: string);
var
  Models: TArray<string>;
begin
  inherited; // FModel := AModel

  // --- 0. Check if the model exists on the server ---
  Models := TOllamaManager.GetModelsList;
  if not MatchText(FModel, Models) then begin
    Log('Model "' + FModel + '" does not exist on the server.', etWarning);
    Exit;
  end;

  // --- 1. Ensure model is running ---
  if not TOllamaManager.IsModelActive(FModel) then begin
    if TOllamaManager.StartModel(FModel) then begin
      TOllamaManager.AddActiveModel(FModel);
      Log('Model "' + FModel + '" was not running, started successfully.', etInfo);
    end
    else
      Log('Model "' + FModel + '" is not running and could not be started.', etWarning);
  end
  else begin
    TOllamaManager.AddActiveModel(FModel);
    Log('Model "' + FModel + '" is already active.', etInfo);
  end;

  if not TOllamaManager.IsKeepAliveThreadStarted then begin
    TOllamaManager.StartKeepAlive; // idempotent
  end;

  // --- 2. Detect model type ---
  FModelType := TOllamaModelDetector.DetectModelType(FModel);

  case FModelType of
    mtTranslateGemma, mtGemma: FEndPoint := efChat; // REQUIRED

    mtRiva: FEndPoint := efGenerate; // Riva-style

    mtLlama, mtQwen, mtMistral: FEndPoint := efGenerate; // or efChat if you prefer

  else
    FEndPoint := efGenerate;
  end;

  // --- 3. Setup language mappings for Riva-style models ---
  if FModelType = TOllamaModelType.mtRiva then begin
    FLanguageNamesToCodes.Clear;
    FLanguageCodesToNames.Clear;

    // 1. PRIMARY MAP — code → name
    FLanguageCodesToNames.AddOrSetValue('en', 'English');
    FLanguageCodesToNames.AddOrSetValue('es', 'Spanish');
    FLanguageCodesToNames.AddOrSetValue('fr', 'French');
    FLanguageCodesToNames.AddOrSetValue('de', 'German');

    FLanguageCodesToNames.AddOrSetValue('zh', 'Chinese');
    FLanguageCodesToNames.AddOrSetValue('zh-cn', 'Simplified Chinese');
    FLanguageCodesToNames.AddOrSetValue('zh-tw', 'Traditional Chinese');
    FLanguageCodesToNames.AddOrSetValue('zh-hk', 'Hong Kong Chinese');

    FLanguageCodesToNames.AddOrSetValue('ja', 'Japanese');
    FLanguageCodesToNames.AddOrSetValue('ru', 'Russian');

    // Model-specific languages
    if FModel.ToLower.Contains('riva-translate') then begin
      FLanguageCodesToNames.AddOrSetValue('ar', 'Arabic');
      FLanguageCodesToNames.AddOrSetValue('es-es', 'European Spanish');
      FLanguageCodesToNames.AddOrSetValue('es-us', 'Latin American Spanish');
      FLanguageCodesToNames.AddOrSetValue('ko', 'Korean');
      FLanguageCodesToNames.AddOrSetValue('pt-br', 'Brazilian Portuguese');
    end;

    // 2. Build reverse mapping — name → code
    for var Pair in FLanguageCodesToNames do
      AddNameCode(LowerCase(Pair.Value), Pair.Key, FLanguageNamesToCodes);
  end;

  if FModelType = TOllamaModelType.mtTranslateGemma then begin
    var Langs: TArray<TLangDef>;
    var Lang: TLangDef;
    var Code: string;

    FLanguageCodesToNames.Clear;
    FLanguageNamesToCodes.Clear;

    Langs := GetTranslateGemmaLanguages;

    for Lang in Langs do begin
      for Code in Lang.Codes do begin
        FLanguageCodesToNames.AddOrSetValue(Code, Lang.Name);
        AddNameCode(LowerCase(Lang.Name), Code, FLanguageNamesToCodes);
      end;
    end;
  end;
end;

function TOllamaService.SupportBatchTranslations: Boolean;
begin
  Result := false;
end;

function TOllamaService.Translate(
    const AText, ASourceLang, ADestLang: string;
    var ATranslated: string;
    const IsCode: Boolean = false
): Boolean;
var
  LReqBody: TStringStream; // Holds the JSON payload for the HTTP POST
  LResponse: IHTTPResponse; // Holds the HTTP response returned by the Ollama server
  LJsonStr, LContent: string; // LJsonStr = final JSON payload; LContent = parsed assistant output
  LRetry: Integer; // Retry counter for failed requests
  LOllamaUrl, LSrcCode, LDstCode, LSrcName, LDstName:
      string; // Human-readable source/destination language names for logging
  Msgs: TMessageArray; // Array of role/content messages (used by chat endpoints)
  SysPrompt: string; // Optional system prompt for chat-style payloads
  UserPrompt: string; // Optional user prompt (used for efGenerate endpoint)
  Profile: TOllamaPromptProfile;
begin
  Result := false; // Initialize result

  if (AText = '') or (ADestLang = '') then
    Exit;

  // -------------------------------
  // Step 1: Prepare human-readable language names for logging
  // -------------------------------
  if IsCode then begin
    LSrcCode := ASourceLang;
    LDstCode := ADestLang;
    LSrcName := Self.LanguageCodeToName(ASourceLang); // e.g., "en" → "English"
    LDstName := Self.LanguageCodeToName(ADestLang); // e.g., "zh" → "Chinese"
  end
  else begin
    if ASourceLang = '' then
      LSrcName := 'English'
    else
      LSrcName := ASourceLang; // e.g., "en" → "English"
    LDstName := ADestLang; // e.g., "zh" → "Chinese"

    LSrcCode := Self.LanguageNameToCode(LSrcName);
    LDstCode := Self.LanguageNameToCode(LDstName);
  end;

  Profile := TOllamaPromptBuilder.DetectPromptProfile(FModel);

  // -------------------------------
  // Build payload
  // -------------------------------
  if Profile = oppTranslateGemma then begin
    // TranslateGemma always uses efChat style
    Msgs :=
        TOllamaPromptBuilder.MakeMessages(
            ['user', TOllamaPromptBuilder.BuildTranslatePrompt(Profile, AText, LSrcCode, LDstCode, LSrcName, LDstName)]
        );
    SysPrompt := '';
    UserPrompt := '';
  end
  else begin
    // -------------------------------
    // Other models: Step 2: Prepare the payload based on endpoint flavor
    // -------------------------------
    if FEndPoint = efGenerate then begin
      // --- efGenerate (instruction-style) ---
      // The /api/generate endpoint does NOT use "messages".
      // The text to translate is passed directly via the 'prompt' field.
      SysPrompt := ''; // Not used for efGenerate

      // Concise translation instruction
      //    UserPrompt :=
      //        Format(
      //            'Translate from %s to %s. Output ONLY the translation, no explanations or notes:%s%s',
      //            [LSrc, LDst, sLineBreak, AText]
      //        );
      UserPrompt := TOllamaPromptBuilder.BuildTranslatePrompt(Profile, AText, LSrcCode, LDstCode, LSrcName, LDstName);

      //    UserPrompt := AText;       // Pass the input text as the user prompt
      SetLength(Msgs, 0); // Clear messages array; not applicable
    end
    else begin
      // --- Chat-style endpoints (efChat / efOpenAIChat) ---
      // These endpoints use a structured messages array with roles like "user", "assistant", "system".
      SysPrompt := ''; // Optional system-level instructions (empty here)
      UserPrompt := ''; // Not used; messages array drives the payload
      //    Msgs :=
      //        TOllamaPromptBuilder.MakeMessages(
      //            [
      //                'user',
      //                Format(
      //                    'Translate from %s to %s. Output ONLY the translation, no explanations or notes:%s%s',
      //                    [LSrc, LDst, sLineBreak, AText]
      //                )
      //            ]
      //        ); // Wrap AText as a user message
      Msgs :=
          TOllamaPromptBuilder.MakeMessages(
              [
                  'user',
                  TOllamaPromptBuilder.BuildTranslatePrompt(Profile, AText, ASourceLang, ADestLang, LSrcName, LDstName)
              ]
          );
    end;
  end;
  // -------------------------------
  // Step 3: Build the final JSON payload
  // -------------------------------
  // TOllamaPromptBuilder.BuildPayload automatically constructs the correct JSON format
  // depending on the endpoint flavor:
  // - efGenerate → {"model": "...", "prompt": "...", "stream": false}
  // - efChat / efOpenAIChat → {"model": "...", "messages": [...], "stream": false, ...}
  LJsonStr :=
      TOllamaPromptBuilder.BuildPayload(
          FEndPoint, // Endpoint flavor (efGenerate, efChat, etc.)
          FModel, // Model name (e.g., "llama3.1:latest")
          Msgs, // Messages array (only used for chat-style endpoints)
          SysPrompt, // Optional system prompt (used only for chat-style)
          UserPrompt, // Optional user prompt (used only for efGenerate)
          False // Stream = false (we want the full response in one go)
      );

  // -------------------------------
  // Step 4: At this point, LJsonStr is ready to be sent to the Ollama server
  // Example payloads:
  // efGenerate:
  // {"model":"llama3.1:latest","prompt":"Hello, world","stream":false}
  //
  // efChat:
  // {"model":"llama3.1:latest","messages":[{"role":"user","content":"Hello, world"}],"stream":false}
  // -------------------------------

  // --- Log the payload ---
  Log('TOllamaService.Translate Payload: ' + sLineBreak + LJsonStr, etInfo);

  LReqBody := TStringStream.Create(LJsonStr, TEncoding.UTF8);
  LOllamaUrl := BuildOllamaUrl(FEndPoint);
  try
    for LRetry := 1 to FRetry do begin
      try
        LReqBody.Position := 0;
        // --- Log before posting ---
        Log(
            Format(
                'TOllamaService.Translate: Attempt %d sending request to [%s]%s%s',
                [LRetry, LOllamaUrl, sLineBreak, LJsonStr]
            ),
            etInfo
        );

        LResponse :=
            FHttpClient.Post(
                LOllamaUrl,
                //          FBASE_URL + 'v1/chat/completions',
                LReqBody,
                nil,
                []
            );

        if (LResponse = nil) then
          raise Exception.Create('HTTP response is nil');

        Log('HTTP Status: ' + LResponse.StatusCode.ToString, etInfo);
        Log('Raw Response: ' + LResponse.ContentAsString, etInfo);

        if LResponse.StatusCode <> 200 then
          raise Exception.Create('HTTP Code ' + LResponse.StatusCode.ToString);

        LContent := LResponse.ContentAsString;

        if TOllamaResponseParser.Parse(FModelType, LContent, Self.FEndPoint, ATranslated) then begin
          ATranslated := Trim(ATranslated);
          Result := True;
          // ---- Log Success ----
          Log('Source [' + LSrcName + ']: ' + AText, etInfo);
          Log('Translation [' + LDstName + ']: ' + ATranslated, etInfo);

          Exit;
        end
        else begin
          // ---- Log Failure ----
          Log('Failed to parse response for Endpoint [' + EndpointFlavorNames[FEndPoint] + ']', etWarning);
          Log('Raw Response: ' + LContent, etInfo);
        end;

      except
        on E: Exception do begin
          Log(Format('Translate attempt %d failed: %s', [LRetry, E.Message]), etError);
          if LRetry = FRetry then begin
            Log('Final Failure: ', etError);
            Exit;
          end;
          Sleep(150);
        end;
      end;
    end;

  finally
    LReqBody.Free;
  end;
end;

function TOllamaService.TranslateBatch(
    const ATexts: TArray<string>;
    const ASourceLang, ADestLang: string;
    const IsCode: Boolean = false
): TArray<string>;
begin

end;

{ TJSONRequestBuilder }

class function TJSONRequestBuilder.BuildTranslationRequest(const AModel, APrompt: string): TJSONObject;
var
  LFormat, LProps, LTranslationProp: TJSONObject;
  LRequired: TJSONArray;
begin
  Result := TJSONObject.Create;

  // Standard Ollama fields
  Result.AddPair('model', AModel);
  Result.AddPair('prompt', APrompt);
  Result.AddPair('stream', TJSONBool.Create(False));

  // ---------- FORMAT SCHEMA ----------
  LFormat := TJSONObject.Create;
  LProps := TJSONObject.Create;
  LTranslationProp := TJSONObject.Create;
  LRequired := TJSONArray.Create;

  LTranslationProp.AddPair('type', 'string');
  LProps.AddPair('translation', LTranslationProp);

  LFormat.AddPair('type', 'object');
  LFormat.AddPair('properties', LProps);

  LRequired.Add('translation');
  LFormat.AddPair('required', LRequired);

  // schema → final JSON
  Result.AddPair('format', LFormat);
end;

end.
