program TestOllama;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.JSON,
  MWQ.Translate.Api.OllamaLocalTranslateService;

function CheckTranslationRequestFormat(const AJson: TJSONObject): Boolean;
var
  LFormat, LProps, LTranslationProp: TJSONValue;
  LRequired: TJSONArray;
begin
  Result := False;
  if AJson = nil then Exit;

  // Top-level keys
  if (AJson.GetValue('model') = nil) or (AJson.GetValue('prompt') = nil) or (AJson.GetValue('stream') = nil) then
    Exit;

  LFormat := AJson.GetValue('format');
  if not (LFormat is TJSONObject) then Exit;

  LProps := (LFormat as TJSONObject).GetValue('properties');
  if not (LProps is TJSONObject) then Exit;

  LTranslationProp := (LProps as TJSONObject).GetValue('translation');
  if not (LTranslationProp is TJSONObject) then Exit;

  LRequired := (LFormat as TJSONObject).GetValue('required') as TJSONArray;
  if (LRequired = nil) then Exit;

  // required should contain 'translation'
  Result := False;
  for var I := 0 to LRequired.Count - 1 do
    if (LRequired.Items[I].Value = 'translation') then
      Result := True;
end;

var
  LJson: TJSONObject;
  Passed: Boolean;
begin
  try
    LJson := TJSONRequestBuilder.BuildTranslationRequest('llama3:latest', 'Translate: Hello');
    try
      Passed := CheckTranslationRequestFormat(LJson);
      if Passed then
      begin
        Writeln('TestOllama: PASS - JSON format valid');
        ExitCode := 0;
      end
      else
      begin
        Writeln('TestOllama: FAIL - JSON format invalid');
        Writeln(LJson.ToString);
        ExitCode := 2;
      end;
    finally
      LJson.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('TestOllama: ERROR - ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
