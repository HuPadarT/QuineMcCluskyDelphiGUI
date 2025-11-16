unit QMModel;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, QM.Service;

type
  TQMService = class(TInterfacedObject, IQMService)
  private
    procedure Log(Steps: TStrings; const Msg: string);
    function DiffPos(const A, B: string): Integer;
    function PatternToLiteral(const Pattern: string): string;
    procedure GetPrimeImplicants(const AllTerms: TList<string>;
      VariableCount: Integer; Steps: TStrings; out PrimeImplicants: TList<string>;
      out PrimeCovers: TList<TArray<Integer>>);
  public
    procedure Compute(const Minterms, DontCares: TArray<Integer>; VariableCount: Integer;
      Steps: TStrings; out ResultExpr: string);
  end;

implementation

uses System.StrUtils, QM.Helpers;

{ TQMService }

procedure TQMService.Log(Steps: TStrings; const Msg: string);
begin
  if Assigned(Steps) then
    Steps.Add(Msg);
end;

function TQMService.DiffPos(const A, B: string): Integer;
var
  i: Integer;
  diffs: Integer;
begin
  Result := -1;
  diffs := 0;
  for i := 1 to Length(A) do
  begin
    if A[i] <> B[i] then
    begin
      Inc(diffs);
      Result := i;
      if diffs > 1 then
      begin
        Result := -1;
        Break;
      end;
    end;
  end;
end;

function TQMService.PatternToLiteral(const Pattern: string): string;
var
  i: Integer;
  parts: TArray<string>;
  pList: TList<string>;
  v: string;
begin
  pList := TList<string>.Create;
  try
    for i := 1 to Length(Pattern) do
    begin
      if Pattern[i] = '-' then
        Continue;
      v := Chr(Ord('A') + i - 1);
      if Pattern[i] = '1' then
        pList.Add(v)
      else
        pList.Add('!' + v);
    end;
    if pList.Count = 0 then
      Exit('1');
    parts := pList.ToArray;
    Result := string.Join(' & ', parts);
  finally
    pList.Free;
  end;
end;

procedure TQMService.GetPrimeImplicants(const AllTerms: TList<string>;
  VariableCount: Integer; Steps: TStrings; out PrimeImplicants: TList<string>;
  out PrimeCovers: TList<TArray<Integer>>);
type
  TTerm = record
    Pattern: string;
    Mins: TArray<Integer>;
  end;
var
  current: TList<TTerm>;
  nextList: TList<TTerm>;
  used: TList<Boolean>;
  i, j, k, dpos: Integer;
  t1, t2: TTerm;
  merged: TTerm;
  primeSet: TList<TTerm>;
  seen: TDictionary<string, Integer>;
  s: string;
begin
  current := TList<TTerm>.Create;
  primeSet := TList<TTerm>.Create;
  seen := TDictionary<string, Integer>.Create;
  try
    // inicializálja az aktuálisat az összes mintermből (minta -> egyetlen minterm, egész számként tárolva a karakterláncban)
    for i := 0 to AllTerms.Count - 1 do
    begin
      s := AllTerms[i];
      t1.Pattern := s;
      SetLength(t1.Mins, 1);
      t1.Mins[0] := StrToInt(s); // caller will have numeric string representing minterm
      current.Add(t1);
    end;

    Log(Steps, 'Prime implicant generation:');
    while True do
    begin
      nextList := TList<TTerm>.Create;
      used := TList<Boolean>.Create;
      used.Count := current.Count;
      for i := 0 to current.Count - 1 do
        used[i] := False;

      for i := 0 to current.Count - 1 do
        for j := i + 1 to current.Count - 1 do
        begin
          dpos := DiffPos(current[i].Pattern, current[j].Pattern);
          if dpos >= 1 then
          begin
            merged.Pattern := current[i].Pattern;
            merged.Pattern[dpos] := '-';
            // mintermek összefűzése
            merged.Mins := current[i].Mins + current[j].Mins;
            // duplázódások megszüntetése
            merged.Mins := merged.Mins;
            // egyediek hozzáadásas
            if not seen.ContainsKey(merged.Pattern) then
            begin
              seen.Add(merged.Pattern, nextList.Count);
              nextList.Add(merged);
              Log(Steps, Format('  Combine %s + %s -> %s', [current[i].Pattern, current[j].Pattern, merged.Pattern]));
            end;
            used[i] := True;
            used[j] := True;
          end;
        end;

      // ami nem lett felhasználva az prím implikáns
      for i := 0 to current.Count - 1 do
        if not used[i] then
        begin
          primeSet.Add(current[i]);
          Log(Steps, '  Prime: ' + current[i].Pattern);
        end;

      if nextList.Count = 0 then  // kilép ha mindet feldolgoztuk
        Break;
      current := nextList;
      seen.Clear;
    end;

    PrimeImplicants := TList<string>.Create;
    PrimeCovers := TList<TArray<Integer>>.Create;
    for i := 0 to primeSet.Count - 1 do
    begin
      PrimeImplicants.Add(primeSet[i].Pattern);
      PrimeCovers.Add(primeSet[i].Mins);
    end;
  finally
    for k := 0 to primeSet.Count - 1 do ;
    primeSet.Free;
    seen.Free;
  end;
end;

procedure TQMService.Compute(const Minterms, DontCares: TArray<Integer>;
  VariableCount: Integer; Steps: TStrings; out ResultExpr: string);
var
  all: TList<string>;
  i, v: Integer;
  termPattern: string;
  primeImplicants: TList<string>;
  primeCovers: TList<TArray<Integer>>;
  minset: TList<Integer>;
  chart: TDictionary<Integer, TList<Integer>>;
  idx: Integer;
  essentialIdx: TList<Integer>;
  covered: TDictionary<Integer, Boolean>;
  remaining: TList<Integer>;
  p: Integer;
  exprParts: TList<string>;
begin
  if Assigned(Steps) then
    Steps.Clear;
  all := TList<string>.Create;
  try
    for i := 0 to Length(Minterms) - 1 do
      all.Add(IntToStr(Minterms[i]));
    for i := 0 to Length(DontCares) - 1 do
      all.Add(IntToStr(DontCares[i]));
    Log(Steps, 'Bemenet: Mintermek = [' + string.Join(',', all.ToArray) + '] Változók száma=' + IntToStr(VariableCount));

    // mindegyik mintermnek elkészítjük a bináris képét
    all.Clear;
    for i := 0 to Length(Minterms) - 1 do
      all.Add(TStaticHelper.ToBinary(Minterms[i], VariableCount));
    for i := 0 to Length(DontCares) - 1 do
      all.Add(TStaticHelper.ToBinary(DontCares[i], VariableCount));
    Log(Steps, 'Bináris formák: ' + string.Join(',', all.ToArray));

    // prím implikánsok generálásának előkészítése logolással
    GetPrimeImplicants(all, VariableCount, Steps, primeImplicants, primeCovers);

    Log(Steps, 'Prím implikánsok: ' + string.Join(', ', primeImplicants.ToArray));

    // Vizuális megjelenítés készítése csak a mintermekhez
    minset := TList<Integer>.Create;
    try
      for i := 0 to Length(Minterms) - 1 do
        minset.Add(Minterms[i]);

      chart := TDictionary<Integer, TList<Integer>>.Create;
      try
        for v in minset do
        begin
          chart.Add(v, TList<Integer>.Create);
          for idx := 0 to primeImplicants.Count - 1 do
          begin
            termPattern := primeImplicants[idx];
            var bin := TStaticHelper.ToBinary(v, VariableCount);
            var matches := True;
            for p := 1 to Length(termPattern) do
              if (termPattern[p] <> '-') and (termPattern[p] <> bin[p]) then
              begin
                matches := False;
                Break;
              end;
            if matches then
              chart[v].Add(idx);
          end;
        end;

        // Nélkülözhetetlen prím implikánsok keresése
        essentialIdx := TList<Integer>.Create;
        try
          for v in chart.Keys do
            if chart[v].Count = 1 then
              essentialIdx.Add(chart[v][0]);

          Log(Steps, 'Nélkülözhetetlen prím implikánsok: ' + IfThen(essentialIdx.Count = 0, 'nincs', IntToStr(essentialIdx[0])));

          covered := TDictionary<Integer, Boolean>.Create;
          try
            for i := 0 to essentialIdx.Count - 1 do
            begin
              idx := essentialIdx[i];
              // A már lefedettek jelölése
              for v in minset do
              begin
                // lefedettség ismételt ellenőrzése
                termPattern := primeImplicants[idx];
                var bin2 := TStaticHelper.ToBinary(v, VariableCount);
                var match := True;
                for p := 1 to Length(termPattern) do
                  if (termPattern[p] <> '-') and (termPattern[p] <> bin2[p]) then
                  begin
                    match := False;
                    Break;
                  end;
                if match then
                  covered.AddOrSetValue(v, True);
              end;
            end;

            // ha mind megvan, akkor az eredmény készítése
            if covered.Count = minset.Count then
            begin
              exprParts := TList<string>.Create;
              try
                for idx in essentialIdx do
                  exprParts.Add(PatternToLiteral(primeImplicants[idx]));
                ResultExpr := string.Join(' | ', exprParts.ToArray);
                Log(Steps, 'All minterms covered by essentials. Result: ' + ResultExpr);
                Exit;
              finally
                exprParts.Free;
              end;
            end;

            // Maradék mintermek és minimális lefedettség egyszerű algoritmussal (az olvashatóság érdekében)
            remaining := TList<Integer>.Create;
            try
              for v in minset do
                if not covered.ContainsKey(v) then
                  remaining.Add(v);
              var s := TStaticHelper.IntArrayToStr(',', remaining.ToArray);
              Log(Steps, 'Remaining minterms: ' + s);

              // Kiválasztjuk az implikánst, amely a legtöbb maradékot lefedi.
              var selected := TList<Integer>.Create;
              try
                // A nélkülözhetetlenekkel kezdjük
                for idx in essentialIdx do
                  if not selected.Contains(idx) then
                    selected.Add(idx);

                while remaining.Count > 0 do
                begin
                  // mindegyiket pontozzuk
                  var bestIdx := -1;
                  var bestScore := -1;
                  for idx := 0 to primeImplicants.Count - 1 do
                  begin
                    if selected.Contains(idx) then Continue;
                    var score := 0;
                    for v in remaining do
                    begin
                      termPattern := primeImplicants[idx];
                      var bin3 := TStaticHelper.ToBinary(v, VariableCount);
                      var mm := True;
                      for p := 1 to Length(termPattern) do
                        if (termPattern[p] <> '-') and (termPattern[p] <> bin3[p]) then
                        begin
                          mm := False;
                          Break;
                        end;
                      if mm then Inc(score);
                    end;
                    if score > bestScore then
                    begin
                      bestScore := score;
                      bestIdx := idx;
                    end;
                  end;
                  if bestIdx < 0 then Break;
                  selected.Add(bestIdx);
                  Log(Steps, Format('A kiválasztott %d (%s) implikáns lefedi a %d mintermeket', [bestIdx, primeImplicants[bestIdx], bestScore]));
                  // a lefedetteket eltávolítjuk
                  var r := TList<Integer>.Create;
                  try
                    for v in remaining do
                    begin
                      termPattern := primeImplicants[bestIdx];
                      var bin4 := TStaticHelper.ToBinary(v, VariableCount);
                      var mm := True;
                      for p := 1 to Length(termPattern) do
                        if (termPattern[p] <> '-') and (termPattern[p] <> bin4[p]) then
                        begin
                          mm := False;
                          Break;
                        end;
                      if not mm then
                        r.Add(v);
                    end;
                    remaining.Free;
                    remaining := r;
                  except
                    r.Free;
                    raise;
                  end;
                end;

                // kifejezés készítése
                exprParts := TList<string>.Create;
                try
                  for idx in selected do
                    exprParts.Add(PatternToLiteral(primeImplicants[idx]));
                  ResultExpr := string.Join(' | ', exprParts.ToArray);
                  Log(Steps, 'Végső kiválasztás: ' + ResultExpr);
                finally
                  exprParts.Free;
                end;
              finally
                selected.Free;
              end;
            finally
              remaining.Free;
            end;
          finally
            covered.Free;
          end;
        finally
          essentialIdx.Free;
        end;
      finally
        chart.Free;
      end;
    finally
      minset.Free;
    end;

  finally
    all.Free;
  end;
end;

end.
