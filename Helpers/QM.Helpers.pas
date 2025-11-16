unit QM.Helpers;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  TStaticHelper = class(TObject)
  public
    class function ToBinary(Value, Bits: Integer): string;
    class function CountOnes(const Pattern: string): Integer;
    class function IntArrayToStr(const ASep: string; const AArray: TArray<Integer>): string;
  end;

implementation

{ TStaticHelper }

class function TStaticHelper.IntArrayToStr(const ASep: string; const AArray: TArray<Integer>): string;
begin
  if High(AArray) < 1 then
  begin
    Result := '';
    Exit;
  end;
  Result := AArray[0].ToString;
  if High(AArray) > 1 then
    for var I := 1 to High(AArray) do
      Result := Result + ASep + AArray[I].ToString;
end;

class function TStaticHelper.ToBinary(Value, Bits: Integer): string;
begin
  Result := StringOfChar('0', Bits);
  for var i := Bits downto 1 do
  begin
    Result[i] := Char(Ord('0') + (Value and 1));
    Value := Value shr 1;
  end;
end;

class function TStaticHelper.CountOnes(const Pattern: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Pattern) do
    if Pattern[i] = '1' then
      Inc(Result);
end;

end.
