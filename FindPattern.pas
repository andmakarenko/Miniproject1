program RandomFrequencySequence;

uses sysutils;

const
  n = 1000;

var
  numbers: array[1..n] of integer;
  frequency: array[1..9] of integer;
  sequences: array[1..n-2] of string;
  sequenceCounts: array[1..n-2] of integer;
  i, j, count: integer;
  sequence: string;
  found: boolean;

procedure GenerateRandomNumbers;
begin
  Randomize;
  for i := 1 to n do
    numbers[i] := Random(9) + 1;  // Random number between 1 and 9
end;

procedure CalculateFrequency;
begin
  for i := 1 to 9 do
    frequency[i] := 0;

  for i := 1 to n do
    Inc(frequency[numbers[i]]);
end;

procedure FindRepeatedSequences;
begin
  count := 0;
  for i := 1 to n - 2 do
  begin
    sequence := IntToStr(numbers[i]) + ',' + IntToStr(numbers[i + 1]) + ',' + IntToStr(numbers[i + 2]);
    found := False;

    for j := 1 to count do
    begin
      if sequences[j] = sequence then
      begin
        Inc(sequenceCounts[j]);
        found := True;
        Break;
      end;
    end;

    if not found then
    begin
      Inc(count);
      sequences[count] := sequence;
      sequenceCounts[count] := 1;
    end;
  end;
end;

procedure PrintFrequency;
begin
  Writeln('Frequency of each number:');
  for i := 1 to 9 do
    Writeln(i, ': ', frequency[i], ' times');
end;

procedure PrintRepeatedSequences;
begin
  Writeln;  
  Writeln('Repeated sequences of length 3 that come up more than 4 times:');
  for i := 1 to count do
  begin
    if sequenceCounts[i] > 4 then
      Writeln(sequences[i], ' appears ', sequenceCounts[i], ' times');
  end;
end;

begin
  GenerateRandomNumbers;
  CalculateFrequency;
  PrintFrequency;
  FindRepeatedSequences;
  PrintRepeatedSequences;
end.
