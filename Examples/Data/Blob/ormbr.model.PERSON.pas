unit ormbr.model.person;

interface

uses
  DB, 
  Classes, 
  SysUtils, 
  Generics.Collections, 

  /// orm 
  ormbr.types.blob, 
  dbcbr.types.mapping,
  ormbr.types.nullable, 
  dbcbr.mapping.classes,
  dbcbr.mapping.register,
  dbcbr.mapping.attributes;

type
  [Entity]
  [Table('PERSON', '')]
  [PrimaryKey('ID', TAutoIncType.NotInc,
                    TGeneratorType.NoneInc,
                    TSortingOrder.NoSort,
                    False, 'Chave primária')]
  TPERSON = class
  private
    { Private declarations } 
    FID: Integer;
    FFIRSTNAME: String;
    FLASTNAME: Nullable<String>;
    FAGE: Integer;
    FSALARY: Double;
    FPERSON_FLD1: Nullable<Integer>;
    FPERSON_FLD3: Nullable<Currency>;
    FPERSON_FLD4: Nullable<String>;
    FPERSON_FLD5: Nullable<Double>;
    FPERSON_FLD6: Nullable<TDate>;
    FPERSON_FLD7: Nullable<TTime>;
    FPERSON_FLD8: Nullable<TDateTime>;
    FPERSON_FLD9: Nullable<String>;
    FPERSON_FLD10: Nullable<String>;
    FPERSON_FLD11: TBlob;
    FPERSON_FLD12: TBlob;
    FPERSON_FLD13: TBlob;
  public
    { Public declarations } 
    [Restrictions([TRestriction.NotNull])]
    [Column('ID', ftInteger)]
    [Dictionary('ID', 'Mensagem de validação', '', '', '', taCenter)]
    property ID: Integer read FID write FID;

    [Restrictions([TRestriction.NotNull])]
    [Column('FIRSTNAME', ftString, 40)]
    [Dictionary('FIRSTNAME', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property FIRSTNAME: String read FFIRSTNAME write FFIRSTNAME;

    [Column('LASTNAME', ftString, 43)]
    [Dictionary('LASTNAME', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property LASTNAME: Nullable<String> read FLASTNAME write FLASTNAME;

    [Restrictions([TRestriction.NotNull])]
    [Column('AGE', ftInteger)]
    [Dictionary('AGE', 'Mensagem de validação', '', '', '', taCenter)]
    property AGE: Integer read FAGE write FAGE;

    [Restrictions([TRestriction.NotNull])]
    [Column('SALARY', ftBCD, 18, 3)]
    [Dictionary('SALARY', 'Mensagem de validação', '0', '', '', taRightJustify)]
    property SALARY: Double read FSALARY write FSALARY;


    [Column('PERSON_FLD1', ftInteger)]
    [Dictionary('PERSON_FLD1', 'Mensagem de validação', '', '', '', taCenter)]
    property PERSON_FLD1: Nullable<Integer> read FPERSON_FLD1 write FPERSON_FLD1;


    [Column('PERSON_FLD3', ftCurrency)]
    [Dictionary('PERSON_FLD3', 'Mensagem de validação', '0', '', '', taRightJustify)]
    property PERSON_FLD3: Nullable<Currency> read FPERSON_FLD3 write FPERSON_FLD3;

    [Column('PERSON_FLD4', ftMemo)]
    [Dictionary('PERSON_FLD4', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property PERSON_FLD4: Nullable<String> read FPERSON_FLD4 write FPERSON_FLD4;

    [Column('PERSON_FLD5', ftBCD, 18, 2)]
    [Dictionary('PERSON_FLD5', 'Mensagem de validação', '0', '', '', taRightJustify)]
    property PERSON_FLD5: Nullable<Double> read FPERSON_FLD5 write FPERSON_FLD5;

    [Column('PERSON_FLD6', ftDate)]
    [Dictionary('PERSON_FLD6', 'Mensagem de validação', '', '', '', taCenter)]
    property PERSON_FLD6: Nullable<TDate> read FPERSON_FLD6 write FPERSON_FLD6;

    [Column('PERSON_FLD7', ftTime)]
    [Dictionary('PERSON_FLD7', 'Mensagem de validação', '', '', '', taCenter)]
    property PERSON_FLD7: Nullable<TTime> read FPERSON_FLD7 write FPERSON_FLD7;

    [Column('PERSON_FLD8', ftDateTime)]
    [Dictionary('PERSON_FLD8', 'Mensagem de validação', '', '', '', taCenter)]
    property PERSON_FLD8: Nullable<TDateTime> read FPERSON_FLD8 write FPERSON_FLD8;

    [Column('PERSON_FLD9', ftString, 20)]
    [Dictionary('PERSON_FLD9', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property PERSON_FLD9: Nullable<String> read FPERSON_FLD9 write FPERSON_FLD9;

    [Column('PERSON_FLD10', ftString, 3000)]
    [Dictionary('PERSON_FLD10', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property PERSON_FLD10: Nullable<String> read FPERSON_FLD10 write FPERSON_FLD10;

    [Column('PERSON_FLD11', ftBlob)]
    [Dictionary('PERSON_FLD11', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property PERSON_FLD11: TBlob read FPERSON_FLD11 write FPERSON_FLD11;

    [Column('PERSON_FLD12', ftBlob)]
    [Dictionary('PERSON_FLD12', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property PERSON_FLD12: TBlob read FPERSON_FLD12 write FPERSON_FLD12;

    [Column('PERSON_FLD13', ftBlob)]
    [Dictionary('PERSON_FLD13', 'Mensagem de validação', '', '', '', taLeftJustify)]
    property PERSON_FLD13: TBlob read FPERSON_FLD13 write FPERSON_FLD13;
  end;

implementation

initialization

  TRegisterClass.RegisterEntity(TPERSON)

end.
