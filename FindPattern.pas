program TimeLoopFinder;

uses
  SysUtils, Classes;

type
  TStringMap = record
    Keys: array of string;
    Values: array of Integer;
  end;

procedure AddToMap(var map: TStringMap; key: string; value: Integer);
var
  i: Integer;
begin
  for i := 0 to High(map.Keys) do
  begin
    if map.Keys[i] = key then
    begin
      map.Values[i] := map.Values[i] + value;
      Exit;
    end;
  end;
  SetLength(map.Keys, Length(map.Keys) + 1);
  SetLength(map.Values, Length(map.Values) + 1);
  map.Keys[High(map.Keys)] := key;
  map.Values[High(map.Values)] := value;
end;

procedure RemoveIfLessThanTwo(var map: TStringMap);
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(map.Keys) do
  begin
    if map.Values[i] >= 2 then
    begin
      map.Keys[count] := map.Keys[i];
      map.Values[count] := map.Values[i];
      Inc(count);
    end;
  end;
  SetLength(map.Keys, count);
  SetLength(map.Values, count);
end;

function GenerateRandomList(n: Integer): TList;
var
  i: Integer;
  intPtr: ^Integer;
  randomList: TList;
begin
  Randomize;
  randomList := TList.Create;
  for i := 0 to n - 1 do
  begin
    New(intPtr);
    intPtr^ := Random(10);
    randomList.Add(intPtr);
  end;
  GenerateRandomList := randomList;
end;

function FindTimeLoops(nums: TList; len: Integer): TStringMap;
var
  i, j: Integer;
  pattern: string;
  timeLoops: TStringMap;
begin
  SetLength(timeLoops.Keys, 0);
  SetLength(timeLoops.Values, 0);

  for i := 0 to nums.Count - len do
  begin
    pattern := '';
    for j := i to i + len - 1 do
      pattern := pattern + IntToStr(Integer(PInteger(nums[j])^)) + ',';

    AddToMap(timeLoops, pattern, 1);
  end;

  RemoveIfLessThanTwo(timeLoops);
  FindTimeLoops := timeLoops;
end;

procedure LookForOutliers(timeLoops: TStringMap);
var
  avg: Double;
  max, i: Integer;
  maxKey: string;
begin
  avg := 0.0;
  max := 0;
  maxKey := '';

  WriteLn;
  WriteLn('Gefundene Zeitschleifen in timeLoops:');
  if Length(timeLoops.Keys) = 0 then
    WriteLn('Keine Zeitschleifen gefunden.')
  else
  begin
    for i := 0 to High(timeLoops.Keys) do
    begin
      WriteLn('Muster: ', timeLoops.Keys[i], ' | Wiederholungen: ', timeLoops.Values[i]);
      avg := avg + timeLoops.Values[i];
      if timeLoops.Values[i] > max then
      begin
        max := timeLoops.Values[i];
        maxKey := timeLoops.Keys[i];
      end;
    end;
    avg := avg / Length(timeLoops.Keys);
    if max - avg > 3 then
    begin
      WriteLn;
      WriteLn('Vermeintliche Zeitschleife gefunden!');
      WriteLn(maxKey, ' wurde ', max, ' mal wiederholt.');
    end
    else
    begin
      WriteLn;
      WriteLn('Keine vermeintliche Zeitschleife gefunden!');
    end;
  end;
end;

procedure FreeRandomList(nums: TList);
var
  i: Integer;
begin
  for i := 0 to nums.Count - 1 do
    Dispose(PInteger(nums[i]));
  nums.Free;
end;

var
  randomNums: TList;
  len: Integer;
  randomTimeLoops: TStringMap;

begin
  // 1. Zufällige nums generieren
  randomNums := GenerateRandomList(1000);

  // 2. Zeitschleifen erkennen
  len := 5;
  randomTimeLoops := FindTimeLoops(randomNums, len);

  // 3. Ergebnisse anzeigen
  LookForOutliers(randomTimeLoops);

  // 4. Speicher freigeben
  FreeRandomList(randomNums);
end.
