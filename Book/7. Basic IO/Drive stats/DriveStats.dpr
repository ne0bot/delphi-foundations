program DriveStats;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils;

function DiskCapacity(DriveLetter: Char): Int64;
begin
  case DriveLetter of
    'a'..'z': Result := DiskSize(1 + Ord(DriveLetter) - Ord('a'));
    'A'..'Z': Result := DiskSize(1 + Ord(DriveLetter) - Ord('A'));
  else Result := -1;
  end;
end;

function DiskSpaceFree(DriveLetter: Char): Int64;
begin
  case DriveLetter of
    'a'..'z': Result := DiskFree(1 + Ord(DriveLetter) - Ord('a'));
    'A'..'Z': Result := DiskFree(1 + Ord(DriveLetter) - Ord('A'));
  else Result := -1;
  end;
end;

const
  BytesPerGB = 1024 * 1024 * 1024;
  BytesPerMB = 1024 * 1024;
  SDiskStatsGB = '%.0n GB; %.0n GB';
  SDiskStatsMB = '%.0n MB; %.0n MB';
var
  Capacity, FreeSpace: Int64;
  DriveStr, Msg: string;
begin
  for DriveStr in TDirectory.GetLogicalDrives do
  begin
    Capacity := DiskCapacity(DriveStr[1]);
    if Capacity = -1 then
    begin
      Writeln('The size of ' + DriveStr + ' could not be determined');
      Continue;
    end;
    FreeSpace := DiskSpaceFree(DriveStr[1]);
    if FreeSpace < BytesPerGB then
      Msg := Format(SDiskStatsMB, [Capacity / BytesPerMB, FreeSpace / BytesPerMB])
    else
      Msg := Format(SDiskStatsGB, [Capacity / BytesPerGB, FreeSpace / BytesPerGB]);
    WriteLn('Size of ' + DriveStr + ' is ' + Msg + ' is free');
  end;
  ReadLn;
end.
