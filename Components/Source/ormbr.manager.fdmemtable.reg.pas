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

unit ormbr.manager.fdmemtable.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  ormbr.manager.fdmemtable;

type
  TORMBrManagerFDMemTableEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('ORMBr-DB', [TORMBrManagerFDMemTable]);
  RegisterSelectionEditor(TORMBrManagerFDMemTable, TORMBrManagerFDMemTableEditor);
end;

{ TORMBrManagerFDMemTableEditor }

procedure TORMBrManagerFDMemTableEditor.RequiresUnits(Proc: TGetStrProc);
begin

end;

end.
