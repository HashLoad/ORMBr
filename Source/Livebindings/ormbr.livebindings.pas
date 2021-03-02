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

unit ormbr.livebindings;

interface

uses
  RTTI,
  Classes,
  SysUtils,
  Controls,
  TypInfo,
  Bindings.Expression,
  Bindings.Helper,
  Data.Bind.Components,
  Data.Bind.ObjectScope;

type
//  TBindingsListHack = class(TBindingsList);

  LiveBindingsControl = class(TCustomAttribute)
  private
    FLinkControl: String;
    FFieldName: String;
    FExpression: String;
  public
    constructor Create(const ALinkControl, AFieldName, AExpression: String); overload;
    constructor Create(const ALinkControl, AFieldName: String); overload;
    property LinkControl: String read FLinkControl;
    property FieldName: String read FFieldName;
    property Expression: String read FExpression;
  end;

  TORMBrLivebindings = class(TObject)
  private
//    FAdapterBindSource: TAdapterBindSource;
//    FBindingsList: TBindingsList;
//    procedure DoCreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  ormbr.controls.helpers;

constructor TORMBrLiveBindings.Create;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProperty: TRttiProperty;
  LCustomAttribute: TCustomAttribute;
  LLiveBindingsControl: LiveBindingsControl;
  LControl: TControl;
  LExpression: String;
begin
//  // AdapterBindSource
//  FAdapterBindSource := TAdapterBindSource.Create(nil);
//  // BindingsList
//  FBindingsList := TBindingsList.Create(nil);
//  TBindingsListHack(FBindingsList).AddBindComp(nil);

  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(Self.ClassType);
    if LType = nil then
      Exit;

    for LProperty in LType.GetProperties do
    begin
      for LCustomAttribute in LProperty.GetAttributes do
      begin
        if not (LCustomAttribute is LiveBindingsControl) then
          Continue;
        LLiveBindingsControl := LiveBindingsControl(LCustomAttribute);
        // Get Component
        LControl := TListControls.ListComponents.Items[LLiveBindingsControl.LinkControl] as TControl;
        if LControl = nil then
          raise Exception.Create('Component [' + LLiveBindingsControl.LinkControl + '] not found!');
        // Expression do atributo
        LExpression := LLiveBindingsControl.Expression;
        if LExpression = '' then
          LExpression := Self.ClassName + '.' + LProperty.Name;
        // Add Components List
        TListControls.ListFieldNames.AddOrSetValue(LLiveBindingsControl.LinkControl, LLiveBindingsControl.FieldName);
        // Registro no LiveBindings
        TBindings.CreateManagedBinding(
              [
                        TBindings.CreateAssociationScope(
                                  [Associate(Self, Self.ClassName)])
              ],
                                  LExpression,
              [
                        TBindings.CreateAssociationScope(
                                  [Associate(LControl, LLiveBindingsControl.LinkControl)])
              ],
                                  LLiveBindingsControl.LinkControl + '.' + LLiveBindingsControl.FieldName,
                        nil);
        // Component
        TBindings.CreateManagedBinding(
              [
                        TBindings.CreateAssociationScope(
                                  [Associate(LControl, LLiveBindingsControl.LinkControl)])
              ],
                                  LLiveBindingsControl.LinkControl + '.' + LLiveBindingsControl.FieldName,
              [
                        TBindings.CreateAssociationScope(
                                  [Associate(Self, Self.ClassName)])
              ],
                                  Self.ClassName + '.' + LProperty.Name,
                        nil);
      end;
    end;
  finally
    LContext.Free;
  end;
end;

{ LiveBindingControl }

constructor LiveBindingsControl.Create(const ALinkControl, AFieldName, AExpression: String);
begin
  FLinkControl := ALinkControl;
  FFieldName := AFieldName;
  FExpression := AExpression;
end;

constructor LiveBindingsControl.Create(const ALinkControl, AFieldName: String);
begin
  Create(ALinkControl, AFieldName, '');
end;

destructor TORMBrLivebindings.Destroy;
begin
//  FAdapterBindSource.Free;
  inherited;
end;

//procedure TORMBrLivebindings.DoCreateAdapter(Sender: TObject;
//  var ABindSourceAdapter: TBindSourceAdapter);
//begin
////  ABindSourceAdapter := TListBindSourceAdapter<TORMBrLivebindings>.Create(;
//end;

end.
