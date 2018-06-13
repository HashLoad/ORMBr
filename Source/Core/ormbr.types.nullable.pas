{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.types.nullable;

interface

uses
  Generics.Defaults,
  SysUtils,
  Variants,
  Rtti;

type
  Nullable<T> = record
  private
    FValue: T;
    FHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean;
    procedure Clear;
    class function VarIsNullOrEmpty(const Value: Variant): Boolean; static;
  public
    constructor Create(const Value: T); overload;
    constructor Create(const Value: Variant); overload;
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(const defaultValue: T): T; overload;
    function Equals(const other: Nullable<T>): Boolean;
    property HasValue: Boolean read GetHasValue;
    property Value: T read GetValue;

    { Operator Overloads }
    class operator Implicit(const Value: Nullable<T>): T;
    class operator Implicit(const Value: T): Nullable<T>;
    class operator Implicit(const Value: Nullable<T>): Variant;
    class operator Implicit(const Value: Variant): Nullable<T>;
    class operator Implicit(Value: Pointer): Nullable<T>;
    class operator Explicit(const Value: Nullable<T>): T;
    class operator Equal(const a, b: Nullable<T>) : Boolean;
    class operator NotEqual(const a, b: Nullable<T>) : Boolean;
  end;

implementation

const
  CHasValueFlag = '@';

constructor Nullable<T>.Create(const Value: T);
begin
  FValue := Value;
  FHasValue := CHasValueFlag;
end;

constructor Nullable<T>.Create(const Value: Variant);
var
  LValue: TValue;
begin
  if not VarIsNullOrEmpty(Value) then
  begin
    LValue := TValue.FromVariant(Value);
    FValue := LValue.AsType<T>;
    FHasValue := CHasValueFlag;
  end
  else
    Clear;
end;

procedure Nullable<T>.Clear;
begin
  FHasValue := '';
  FValue := Default(T);
end;

class function Nullable<T>.VarIsNullOrEmpty(const Value: Variant): Boolean;
begin
  Result := VarIsNull(Value) or VarIsEmpty(Value);
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := Length(FHasValue) > 0;
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
     raise Exception.Create('Invalid operation, Nullable type has no value.');
  Result := FValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := Value
  else
    Result := Default(T);
end;

function Nullable<T>.GetValueOrDefault(const defaultValue: T): T;
begin
  if HasValue then
    Result := Value
  else
    Result := defaultValue;
end;

function Nullable<T>.Equals(const other: Nullable<T>): Boolean;
begin
  if HasValue and other.HasValue then
    Result := TEqualityComparer<T>.Default.Equals(Value, other.Value)
  else
    Result := HasValue = other.HasValue;
end;

class operator Nullable<T>.Implicit(const Value: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(Value);
end;

class operator Nullable<T>.Implicit(const Value: Nullable<T>): T;
begin
  Result := Value.Value;
end;

class operator Nullable<T>.Implicit(const Value: Nullable<T>): Variant;
var
  vValue: TValue;
begin
  if Value.HasValue then
  begin
    vValue := TValue.From<T>(Value.Value);
    if vValue.IsType<Boolean> then
      Result := vValue.AsBoolean
    else
      Result := vValue.AsVariant;
  end
  else
    Result := Null;
end;

class operator Nullable<T>.Implicit(const Value: Variant): Nullable<T>;
var
  vValue: TValue;
begin
  if not VarIsNullOrEmpty(Value) then
  begin
    vValue := TValue.FromVariant(Value);
    Result := Nullable<T>.Create(vValue.AsType<T>);
  end
  else
    Result.Clear;
end;

class operator Nullable<T>.Implicit(Value: Pointer): Nullable<T>;
begin
  if not Assigned(Value) then
    Result.Clear
  else
    raise Exception.Create('Cannot assigned non-null pointer to nullable type.');
end;

class operator Nullable<T>.Explicit(const Value: Nullable<T>): T;
begin
  Result := Value.Value;
end;

class operator Nullable<T>.Equal(const a, b: Nullable<T>): Boolean;
begin
  Result := a.Equals(b);
end;

class operator Nullable<T>.NotEqual(const a, b: Nullable<T>): Boolean;
begin
  Result := not a.Equals(b);
end;

end.
