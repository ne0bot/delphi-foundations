program BinaryReaderWriterExample;
{
  A simple demo for a simple pair of classes, TBinaryReader and TBinaryWriter.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes;

type
  TPerson = record
    Forename, Surname: string;
    DateOfBirth: TDate; //=Double
    constructor Create(const AForename, ASurname: string; const ADateOfBirth: TDate);
  end;

constructor TPerson.Create(const AForename, ASurname: string; const ADateOfBirth: TDate);
begin
  Forename := AForename;
  Surname := ASurname;
  DateOfBirth := ADateOfBirth;
end;

const
  Signature = 'MyCustomData';

function ReadDataFile(const FileName: string): TArray<TPerson>;
var
  I: Integer;
  Reader: TBinaryReader;
begin
  Reader := TBinaryReader.Create(FileName);
  try
    if Reader.ReadString <> Signature then
      raise EStreamError.Create('Invalid file format!');
    SetLength(Result, Reader.ReadInt32);
    for I := 0 to High(Result) do
    begin
      Result[I].Forename := Reader.ReadString;
      Result[I].Surname := Reader.ReadString;
      Result[I].DateOfBirth := Reader.ReadDouble;
    end;
  finally
    Reader.Free;
  end;
end;

procedure WriteDataFile(const FileName: string; const Data: array of TPerson);
var
  Rec: TPerson;
  Writer: TBinaryWriter;
begin
  Writer := TBinaryWriter.Create(FileName);
  try
    Writer.Write(Signature);
    Writer.Write(Int32(Length(Data)));
    for Rec in Data do
    begin
      Writer.Write(Rec.Forename);
      Writer.Write(Rec.Surname);
      Writer.Write(Rec.DateOfBirth);
    end;
  finally
    Writer.Free;
  end;
end;

procedure WriteTestFile(const FileName: string);
var
  Data: TArray<TPerson>;
begin
  SetLength(Data, 4);
  Data[0] := TPerson.Create('John', 'Smith', EncodeDate(1974, 5, 24));
  Data[1] := TPerson.Create('Joe', 'Bloggs', EncodeDate(1986, 4, 16));
  Data[2] := TPerson.Create('Joanna', 'Smythe', EncodeDate(1962, 9, 7));
  Data[3] := TPerson.Create('Jill', 'Soates', EncodeDate(1979, 8, 30));
  WriteDataFile(FileName, Data);
end;

var
  Data: TArray<TPerson>;
  Rec: TPerson;
  TestFile: string;
begin
  try
    //write out the test data
    TestFile := ChangeFileExt(ParamStr(0), '.dat');
    WriteTestFile(TestFile);
    //read it back in
    Data := ReadDataFile(TestFile);
    WriteLn('Read ', Length(Data), ' records');
    for Rec in Data do
    begin
      WriteLn;
      WriteLn('Forname:        ', Rec.Forename);
      WriteLn('Surname:        ', Rec.Surname);
      WriteLn('Date of birth:  ', DateToStr(Rec.DateOfBirth));
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
