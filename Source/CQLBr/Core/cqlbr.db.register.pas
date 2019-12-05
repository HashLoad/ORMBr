{
         CQL Brasil - Criteria Query Language for Delphi/Lazarus


                   Copyright (c) 2019, Isaque Pinheiro
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

{ @abstract(CQLBr Framework)
  @created(18 Jul 2019)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit cqlbr.db.register;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  SysUtils,
  Generics.Collections,
  cqlbr.interfaces;

type
  TDBRegister = class
  strict private
    class var FCQLSelect: TDictionary<TDBName, ICQLSelect>;
    class var FCQLWhere: TDictionary<TDBName, ICQLWhere>;
    class var FCQLSerialize: TDictionary<TDBName, ICQLSerialize>;
  private
    class constructor Create;
    class destructor Destroy;
  public
    /// <summary>
    ///   Select for database
    /// </summary>
    class procedure RegisterSelect(const ADBName: TDBName;
      const ACQLSelect: ICQLSelect);
    class function Select(const ADBName: TDBName): ICQLSelect;
    /// <summary>
    ///   Select for database
    /// </summary>
    class procedure RegisterWhere(const ADBName: TDBName;
      const ACQLWhere: ICQLWhere);
    class function Where(const ADBName: TDBName): ICQLWhere;
    /// <summary>
    ///   Serialize for database
    /// </summary>
    class procedure RegisterSerialize(const ADBName: TDBName;
      const ACQLSelect: ICQLSerialize);
    class function Serialize(const ADBName: TDBName): ICQLSerialize;
  end;

implementation

const
  TStrDBName: array[dbnMSSQL..dbnNexusDB] of
                  string = ('MSSQL','MySQL','Firebird','SQLite','Interbase','DB2',
                            'Oracle','Informix','PostgreSQL','ADS','ASA',
                            'AbsoluteDB','MongoDB','ElevateDB','NexusDB');


class constructor TDBRegister.Create;
begin
  FCQLSelect := TDictionary<TDBName, ICQLSelect>.Create;
  FCQLWhere := TDictionary<TDBName, ICQLWhere>.Create;
  FCQLSerialize := TDictionary<TDBName, ICQLSerialize>.Create;
end;

class destructor TDBRegister.Destroy;
begin
  FCQLSelect.Clear;
  FCQLSelect.Free;
  FCQLWhere.Clear;
  FCQLWhere.Free;
  FCQLSerialize.Clear;
  FCQLSerialize.Free;
  inherited;
end;

class function TDBRegister.Select(const ADBName: TDBName): ICQLSelect;
begin
  Result := nil;
  if FCQLSelect.ContainsKey(ADBName) then
    Result := FCQLSelect[ADBName];
end;

class procedure TDBRegister.RegisterSelect(const ADBName: TDBName;
  const ACQLSelect: ICQLSelect);
begin
  FCQLSelect.AddOrSetValue(ADBName, ACQLSelect);
end;

class function TDBRegister.Serialize(const ADBName: TDBName): ICQLSerialize;
begin
  if not FCQLSerialize.ContainsKey(ADBName) then
    raise Exception
            .Create('O serialize do banco ' + TStrDBName[ADBName] + ' n�o est� registrado, adicione a unit "cqlbr.serialize.???.pas" onde ??? nome do banco, na cl�usula USES do seu projeto!');

  Result := FCQLSerialize[ADBName];
end;

class function TDBRegister.Where(const ADBName: TDBName): ICQLWhere;
begin
  Result := nil;
  if FCQLWhere.ContainsKey(ADBName) then
    Result := FCQLWhere[ADBName];
end;

class procedure TDBRegister.RegisterSerialize(const ADBName: TDBName;
  const ACQLSelect: ICQLSerialize);
begin
  FCQLSerialize.AddOrSetValue(ADBName, ACQLSelect);
end;

class procedure TDBRegister.RegisterWhere(const ADBName: TDBName; const ACQLWhere: ICQLWhere);
begin
  FCQLWhere.AddOrSetValue(ADBName, ACQLWhere);
end;

end.
