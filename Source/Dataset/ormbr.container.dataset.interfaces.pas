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
}

unit ormbr.container.dataset.interfaces;

interface

uses
  DB,
  RTTi,
  Classes,
  Generics.Collections,
  ormbr.dataset.base.adapter;

type
  IContainerDataSet<M: class, constructor> = interface
    ['{67DC311E-06BF-4B41-93E1-FA66AB0D8537}']
  {$REGION 'Property Getters & Setters'}
    function _GetAutoNextPacket: Boolean;
    procedure _SetAutoNextPacket(const Value: Boolean);
  {$ENDREGION}
    procedure LoadLazy(AOwner: M);
    procedure Open; overload;
    procedure Open(const AID: Integer); overload;
    procedure Open(const AID: String); overload;
    procedure OpenWhere(const AWhere: string; const AOrderBy: string = '');
    procedure Insert;
    procedure Append;
    procedure Post;
    procedure Edit;
    procedure Delete;
    procedure Close;
    procedure Cancel;
    procedure RefreshRecord;
    procedure RefreshRecordWhere(const AWhere: string);
    procedure EmptyDataSet;
    procedure CancelUpdates;
    procedure Save(AObject: M);
    procedure ApplyUpdates(MaxErros: Integer);
    procedure AddLookupField(AFieldName: string;
                             AKeyFields: string;
                             ALookupDataSet: TObject;
                             ALookupKeyFields: string;
                             ALookupResultField: string;
                             ADisplayLabel: string = '');
    procedure NextPacket;
    function DataSet: TDataSet;
    function MasterObject: TDataSetBaseAdapter<M>; deprecated 'Use This';
    function This: TDataSetBaseAdapter<M>;
    function Current: M;
    /// ObjectSet
    function Find: TObjectList<M>; overload;
    function Find(const AID: Integer): M; overload;
    function Find(const AID: String): M; overload;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>;
    /// DataSet
    property AutoNextPacket: Boolean read _GetAutoNextPacket write _SetAutoNextPacket;
  end;

implementation

end.
