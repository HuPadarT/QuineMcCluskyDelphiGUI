unit QM.Service;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections;

type
  IQMService = interface
    ['{8A6E7F9B-1A2B-4C4F-9B60-6D84E2E2BBF2}']
    procedure Compute(const Minterms, DontCares: TArray<Integer>; VariableCount: Integer;
      Steps: TStrings; out ResultExpr: string);
  end;

implementation

end.
