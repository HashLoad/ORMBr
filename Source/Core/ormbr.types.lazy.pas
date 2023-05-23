{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
}

unit ormbr.types.lazy;

interface

uses
  Rtti,
  SysUtils,
  TypInfo;

const
  ObjCastGUID: TGUID = '{2B0E75F4-EB17-4995-B4DB-FE6D40F1189F}';

type
  ILazy<T> = interface(TFunc<T>)
    ['{C303E426-F0C3-447B-9FCB-7B911B9E5CF5}']
    function IsValueCreated: Boolean;
    property Value: T read Invoke;
  end;

  TLazy<T> = class(TInterfacedObject, ILazy<T>, IInterface)
  private
    FIsValueCreated: Boolean;
    FValue: T;
    FValueFactory: TFunc<T>;
    procedure Initialize;
    function Invoke: T;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    constructor Create(ValueFactory: TFunc<T>);
    destructor Destroy; override;

    function IsValueCreated: Boolean;
    property Value: T read Invoke;
  end;

  Lazy<T> = record
  strict private
    FLazy: ILazy<T>;
    function GetValue: T;
  public
    class constructor Create;
    property Value: T read GetValue;
    class operator Implicit(const Value: Lazy<T>): ILazy<T>; overload;
    class operator Implicit(const Value: Lazy<T>): T; overload;
    class operator Implicit(const Value: TFunc<T>): Lazy<T>; overload;
  end;

  PObject = ^TObject;

implementation

uses
  dbcbr.rtti.helper;

{ TLazy<T> }

constructor TLazy<T>.Create(ValueFactory: TFunc<T>);
begin
  FValueFactory := ValueFactory;
end;

destructor TLazy<T>.Destroy;
var
  LTypeInfo: PTypeInfo;
begin
  if FIsValueCreated then
  begin
    LTypeInfo := TypeInfo(T);
    if LTypeInfo.Kind = tkClass then
      PObject(@FValue)^.Free();
  end;
  inherited;
end;

procedure TLazy<T>.Initialize;
begin
  if not FIsValueCreated then
  begin
    FValue := FValueFactory();
    FIsValueCreated := True;
  end;
end;

function TLazy<T>.Invoke: T;
begin
  Initialize();
  Result := FValue;
end;

function TLazy<T>.IsValueCreated: Boolean;
begin
  Result := FIsValueCreated;
end;

function TLazy<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, ObjCastGUID) then
  begin
    Initialize;
  end;
  Result := inherited;
end;

{ Lazy<T> }

class constructor Lazy<T>.Create;
begin

end;

function Lazy<T>.GetValue: T;
var
  LTypeInfo: PTypeInfo;
begin
  if not Assigned(FLazy) then
    FLazy := TLazy<T>.Create(function: T
                             var
                               LContext: TRttiContext;
                               LRttiType: TRttiType;
                               LValue: TValue;
                               LObject: TObject;
                               LMethod: TRttiMethod;
                             begin
                               LRttiType := LContext.GetType(TypeInfo(T));
                               if LRttiType = nil then
                                 Exit;
                               LMethod := LRttiType.GetMethod('Create');
                               if Assigned(LMethod) then
                               begin
                                  LObject := LRttiType.AsInstance.MetaclassType.Create;
                                  if LRttiType.IsList then
                                    LValue := LMethod.Invoke(LObject, [True])
                                  else
                                    LValue := LMethod.Invoke(LObject, []);
                               end;
                               Result := LValue.AsType<T>;
                             end);
  Result := FLazy();
end;

class operator Lazy<T>.Implicit(const Value: Lazy<T>): ILazy<T>;
begin
  Result := Value.FLazy;
end;

class operator Lazy<T>.Implicit(const Value: Lazy<T>): T;
begin
  Result := Value.Value;
end;

class operator Lazy<T>.Implicit(const Value: TFunc<T>): Lazy<T>;
begin
  Result.FLazy := TLazy<T>.Create(Value);
end;

end.

