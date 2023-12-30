﻿unit Minsk.Runtime.Types;

interface

type
  TMinskType = (
    mtNull,
    mtInteger);

  TMinskValue = record
    case MinskType: TMinskType of
      mtNull: ();
      mtInteger: (IntegerValue: Integer);
  end;

  TMinskException = class
  public
    Message: string;
    constructor Create(const AMessage: string);
  end;

function MinskNull: TMinskValue;
function MinskInteger(AValue: Integer): TMinskValue;
procedure WriteMinskValue(const AValue: TMinskValue);
function MinskTypeToString(const AType: TMinskType): string;

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

procedure WriteMinskValue(const AValue: TMinskValue);
begin
  case AValue.MinskType of
    mtNull: ;
    mtInteger: Write(AValue.IntegerValue);
    end;
end;

constructor TMinskException.Create(const AMessage: string);
begin
  Message := AMessage;
end;

function MinskTypeToString(const AType: TMinskType): string;
begin
  Result := GetEnumName(TypeInfo(TMinskType), Ord(AType));
  Delete(Result, 1, 2);
end;

end.
