unit Minsk.Runtime.Types;

interface

type
  TMinskType = (
    mtNull,
    mtInteger,
    mtBoolean);

  TMinskValue = record
    case MinskType: TMinskType of
      mtNull: ();
      mtInteger: (IntegerValue: Integer);
      mtBoolean: (BooleanValue: Boolean);
  end;

function MinskNull: TMinskValue;
function MinskInteger(AValue: Integer): TMinskValue;
function MinskBoolean(AValue: Boolean): TMinskValue;
procedure WriteMinskValue(const AValue: TMinskValue);
function MinskTypeToString(const AType: TMinskType): string;
operator = (const ALeft, ARight: TMinskValue): Boolean;
operator <> (const ALeft, ARight: TMinskValue): Boolean;

implementation

uses
  TypInfo;

function MinskNull: TMinskValue;
begin
  Result.MinskType := mtNull;
end;

function MinskInteger(AValue: Integer): TMinskValue;
begin
  Result.MinskType := mtInteger;
  Result.IntegerValue := AValue;
end;

function MinskBoolean(AValue: Boolean): TMinskValue;
begin
  Result.MinskType := mtBoolean;
  Result.BooleanValue := AValue;
end;

procedure WriteMinskValue(const AValue: TMinskValue);
begin
  case AValue.MinskType of
    mtNull: ;
    mtInteger: Write(AValue.IntegerValue);
    mtBoolean:
      if AValue.BooleanValue then
        Write('true')
      else
        Write('false');
    end;
end;

function MinskTypeToString(const AType: TMinskType): string;
begin
  Result := GetEnumName(TypeInfo(TMinskType), Ord(AType));
  Delete(Result, 1, 2);
end;

operator = (const ALeft, ARight: TMinskValue): Boolean;
begin
  Result := ALeft.MinskType = ARight.MinskType;
  if Result then
    case ALeft.MinskType of
      mtNull: ;
      mtInteger: Result := ALeft.IntegerValue = ARight.IntegerValue;
      mtBoolean: Result := ALeft.BooleanValue = ARight.BooleanValue;
    end;
end;

operator <> (const ALeft, ARight: TMinskValue): Boolean;
begin
  Result := not (ALeft = ARight);
end;

end.
