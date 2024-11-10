program Demo1;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmxdemo1 in 'fmxdemo1.pas' {Form13};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
