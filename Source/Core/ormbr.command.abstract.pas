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

unit ormbr.command.abstract;

interface

uses
  DB,
  Rtti,
  dbebr.factory.interfaces,
  ormbr.driver.register,
  ormbr.dml.interfaces;

type
  TDMLCommandAbstract = class abstract
  protected
    FConnection: IDBConnection;
    FGeneratorCommand: IDMLGeneratorCommand;
    FParams: TParams;
    FResultCommand: string;
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
  // Driver de Conex�o
  FConnection := AConnection;
  // Driver do banco de dados
  FGeneratorCommand := TDriverRegister.GetDriver(ADriverName);
  FGeneratorCommand.SetConnection(AConnection);
  // Lista de par�metros
  FParams := TParams.Create;
end;

destructor TDMLCommandAbstract.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TDMLCommandAbstract.GetDMLCommand: string;
begin
  Result := FResultCommand;
end;

function TDMLCommandAbstract.Params: TParams;
begin
  Result := FParams;
end;

end.
