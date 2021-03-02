{
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

{ @abstract(ORMBr Livebindings)
  @created(29 Nov 2020)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Telegram : @IsaquePinheiro)
}

unit ormbr.controls.helpers;

interface

uses
  {$IFDEF HAS_FMX}
  FMX.Controls,
  {$ELSE HAS_VCL}
  VCL.Controls,
  {$ENDIF}
  System.Generics.Collections,
  System.Classes;

type
  /// <summary>
  ///   Lista registradora de cada componente do form, para que o ORMBr
  ///   encontre de forma simples e rápida, para notificar ao LiveBindings
  /// </summary>
  TListComponents = TObjectDictionary<String, TComponent>;

  /// <summary>
  ///   Lista registradora de cada nomes das propriedade do controle para que o
  ///   ORMBr encontre de forma simples e rápida, para notificar ao LiveBindings
  /// </summary>
  TListFieldNames = TDictionary<String, String>;


  /// <summary>
  ///   Classe usada pelo ORMBr para registrar e notificar o livebindings
  /// </summary>
  TListControls = class
  private
    class var FListComponents: TListComponents;
    class var FListFieldNames: TListFieldNames;
  public
    class constructor Create;
    class destructor Destroy;
    class function ListComponents: TListComponents;
    class function ListFieldNames: TListFieldNames;
  end;

  /// <summary>
  ///   Helper do TControl, dando recurso ao ORMBr de capturar o nome da
  ///   propriedade atribuida ao attributo FieldLiveBindings, para cada
  ///   cotrole a ser gerenciado pelo Livebindings.
  /// </summary>
  TControlHelper = class helper for TControl
  private
    class var FFieldName: String;
    class procedure SetFieldName(const Value: String); static;
  public
    { Public declarations }
    class property FieldName: String read FFieldName write SetFieldName;
  end;

implementation

{ TListControls }

class constructor TListControls.Create;
begin
  FListComponents := TListComponents.Create;
  FListFieldNames := TListFieldNames.Create;
end;

class destructor TListControls.Destroy;
begin
  FListComponents.Clear;
  FListComponents.Free;
  FListFieldNames.Free;
end;

class function TListControls.ListComponents: TListComponents;
begin
  Result := FListComponents;
end;

class function TListControls.ListFieldNames: TListFieldNames;
begin
  Result := FListFieldNames;
end;

{ TControlHelper }

class procedure TControlHelper.SetFieldName(const Value: String);
begin
  FFieldName := Value;
end;

end.

