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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.core.consts;

interface

uses
  TypInfo;

const
  cENUMERATIONSTYPEERROR = 'Invalid type. Type enumerator supported [ftBoolean, ftInteger, ftFixedChar, ftString]';
  cMESSAGEPKNOTFOUND = 'PrimaryKey not found on your model!';
  cMESSAGECOLUMNNOTFOUND = 'Nenhum atributo [Column()] foi definido nas propriedades da classe [ %s ]';
  cPROPERTYTYPES_1 = [tkUnknown,
                      tkInterface,
                      tkClass,
                      tkClassRef,
                      tkPointer,
                      tkProcedure];

  cPROPERTYTYPES_2 = [tkUnknown,
                      tkInterface,
                      tkClassRef,
                      tkPointer,
                      tkProcedure];


implementation

end.
