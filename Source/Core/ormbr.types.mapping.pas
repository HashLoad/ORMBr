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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.types.mapping;

interface

type
  TRuleAction = (None, Cascade, SetNull, SetDefault);
  TSortingOrder = (NoSort, Ascending, Descending);
  TMultiplicity = (OneToOne, OneToMany, ManyToOne, ManyToMany);
  TGenerated = (Never, Insert, Always);
  TJoin = (InnerJoin, LeftJoin, RightJoin, FullJoin);
  TSequenceType = (NotInc, AutoInc, TableInc, GuidInc);
  TRestriction = (NotNull, NoInsert, NoUpdate, NoValidate, Unique, Hidden);
  TRestrictions = set of TRestriction;
  TCascadeAction = (CascadeNone, CascadeAutoInc, CascadeInsert, CascadeUpdate, CascadeDelete);
  TCascadeActions = set of TCascadeAction;
  TMasterEvent = (AutoPost, AutoEdit, AutoInsert);
  TMasterEvents = set of TMasterEvent;
  TEnumType = (etChar, etString, etInteger, etBoolean);
  TFieldEvent = (onChange, onGetText, onSetText, onValidate);
  TFieldEvents = set of TFieldEvent;

implementation

end.
