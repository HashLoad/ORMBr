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

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.command.abstract;

interface

uses
  DB,
  Rtti,
  ormbr.criteria,
  ormbr.factory.interfaces,
  ormbr.types.database,
  ormbr.driver.register,
  ormbr.dml.interfaces;

type
  TDMLCommandAbstract = class abstract
  protected
    FConnection: IDBConnection;
    FGeneratorCommand: IDMLGeneratorCommand;
    FParams: TParams;
    FCommand: string;
  public
    constructor Create(AConnection: IDBConnection; ADriverName: TDriverName;
      AObject: TObject); virtual;
    destructor Destroy; override;
    function GetDMLCommand: string;
    function Params: TParams;
  end;

implementation

{ TDMLCommandAbstract }

constructor TDMLCommandAbstract.Create(AConnection: IDBConnection;
  ADriverName: TDriverName; AObject: TObject);
begin
  /// <summary>
  /// Driver de Conex�o
  /// </summary>
  FConnection := AConnection;
  /// <summary>
  /// Driver do banco de dados
  /// </summary>
  FGeneratorCommand := TDriverRegister.GetDriver(ADriverName);
  FGeneratorCommand.SetConnection(AConnection);
  /// <summary>
  /// Lista de par�metros
  /// </summary>
  FParams := TParams.Create;
end;

destructor TDMLCommandAbstract.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TDMLCommandAbstract.GetDMLCommand: string;
begin
  Result := FCommand;
end;

function TDMLCommandAbstract.Params: TParams;
begin
  Result := FParams;
end;

end.
