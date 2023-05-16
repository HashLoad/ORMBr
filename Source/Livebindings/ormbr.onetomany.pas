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

  @credit(Stephen Ball)
  @https://delphiaball.co.uk/2016/08/23/livebindings-vcl-developers-live/)
}

unit ormbr.onetomany;

interface

uses
  Classes,
  Generics.Collections,
  Data.Bind.ObjectScope;

type
  // Methods used to get the latest version of the object.
  TFuncChild<M, D: class> = reference to function(const ACurrentMaster: M): D;
  TFuncChildList<M, D: class> = reference to function(const ACurrentMaster: M): TObjectList<D>;

  TSyncChildBase = class;
  TSyncChildLinkArray = array of TSyncChildBase;

  // Foundation class for all detail objects
  TSyncChildBase = class
  private
    FChildArray: TSyncChildLinkArray;
    function GetChildBindSource: TBindSourceAdapter; virtual; abstract;
    procedure OnAfterScroll(const AAdapter: TBindSourceAdapter); virtual; abstract;
    procedure OnBeforeScroll(const AAdapter: TBindSourceAdapter); virtual; abstract;
    procedure SetChildArray(const AValue: TSyncChildLinkArray);
  public
    destructor Destroy; override;
    property ChildBindSource : TBindSourceAdapter read GetChildBindSource;
    property ChildArray : TSyncChildLinkArray read FChildArray write SetChildArray;
  end;

  // Base Class for all detail classes working with Generics
  TSyncGenericChildBase<M, D: class> = class(TSyncChildBase)
  private
  end;

  // Link for an Object
  TSyncChildObjectLink<M, D: class> = class(TSyncGenericChildBase<M, D>)
  strict private
    FGetChild: TFuncChild<M, D>;
    FChildAdapter: TObjectBindSourceAdapter<D>;
    function GetChildBindSource: TBindSourceAdapter; override;
    procedure OnAfterScroll(const AAdapter: TBindSourceAdapter); override;
    procedure OnBeforeScroll(const AAdapter: TBindSourceAdapter); override;
  public
    constructor Create(const AOwner: M; const AGetChild: TFuncChild<M, D>;
      const AChildArray : TSyncChildLinkArray);
    destructor Destroy; override;
  end;

  // Link for an Object List
  TSyncChildListLink<M, D: class> = class(TSyncGenericChildBase<M, D>)
  strict private
    FGetChild: TFuncChildList<M,D>;
    FChildListAdapter: TListBindSourceAdapter<D>;
    function GetChildBindSource: TBindSourceAdapter; override;
    procedure OnUIAfterScroll(AAdapter: TBindSourceAdapter);
    procedure OnUIBeforeScroll(AAdapter: TBindSourceAdapter);
    procedure OnAfterScroll(const AAdapter: TBindSourceAdapter); override;
    procedure OnBeforeScroll(const AAdapter: TBindSourceAdapter); override;
  public
    constructor Create(const AOwner: M; const AGetChild: TFuncChildList<M, D>;
      const AChildArray: TSyncChildLinkArray);
    destructor Destroy; override;
  end;

  // Owner class that has the top level list.
  TSyncOwnerList<M: class> = class
  strict private
    FOwnerAdapter: TListBindSourceAdapter<M>;
    FChildArray: TSyncChildLinkArray;
    procedure OnAfterScroll(AAdapter: TBindSourceAdapter);
    procedure OnBeforeScroll(AAdapter: TBindSourceAdapter);
    procedure SetDetailsArray(const AValue: TSyncChildLinkArray);
  public
    constructor Create(AOwner: TList<M>; AChildArray: TSyncChildLinkArray);
    destructor Destroy; override;
    property MasterBindSource: TListBindSourceAdapter<M> read FOwnerAdapter;
    property ChildArray: TSyncChildLinkArray read FChildArray write SetDetailsArray;
  end;

implementation

uses Dialogs;

{ TSyncMasterList<M> }

constructor TSyncOwnerList<M>.Create(AOwner: TList<M>;
  AChildArray: TSyncChildLinkArray);
begin
  FChildArray := AChildArray;
  FOwnerAdapter := TListBindSourceAdapter<M>.Create(nil, AOwner, False);
  TListBindSourceAdapter<M>(FOwnerAdapter).AfterScroll := OnAfterScroll;
  TListBindSourceAdapter<M>(FOwnerAdapter).BeforeScroll := OnBeforeScroll;

  //OnAfterScroll(FMasterAdapter);
end;

destructor TSyncOwnerList<M>.Destroy;
var
  LChild: TSyncChildBase;
begin
  for LChild in ChildArray do
  begin
    if LChild <> nil then
      LChild.Free;
  end;
  inherited;
end;

procedure TSyncOwnerList<M>.OnAfterScroll(AAdapter: TBindSourceAdapter);
var
  LChild: TSyncChildBase;
begin
  if (FOwnerAdapter <> nil) then
  begin
    for LChild in ChildArray do
      LChild.OnAfterScroll(AAdapter);
  end;
end;

procedure TSyncOwnerList<M>.OnBeforeScroll(AAdapter: TBindSourceAdapter);
var
  LChild: TSyncChildBase;
begin
  if (FOwnerAdapter <> nil) then
  begin
    for LChild in ChildArray do
      LChild.OnBeforeScroll(AAdapter);
  end;
end;

procedure TSyncOwnerList<M>.SetDetailsArray(
  const AValue: TSyncChildLinkArray);
var
  LChild: TSyncChildBase;
begin
  // Assert no nulls.
  for LChild in AValue do
    Assert(Assigned(LChild),'Detail Array Value cannot be null');

  FChildArray := AValue;
end;

{ TSyncDetailLink<M, D> }

constructor TSyncChildObjectLink<M, D>.Create(const AOwner: M;
  const AGetChild: TFuncChild<M, D>; const AChildArray : TSyncChildLinkArray);
var
  LChildNew: D;
begin
  inherited Create;
  FGetChild := AGetChild;
  ChildArray := AChildArray;

  LChildNew := FGetChild(AOwner);
  FChildAdapter := TObjectBindSourceAdapter<D>.Create(nil, LChildNew, False);
end;

destructor TSyncChildObjectLink<M, D>.Destroy;
begin
  FChildAdapter.Free;
  inherited;
end;

function TSyncChildObjectLink<M, D>.GetChildBindSource: TBindSourceAdapter;
begin
  Result := FChildAdapter;
end;

procedure TSyncChildObjectLink<M, D>.OnAfterScroll(
  const AAdapter: TBindSourceAdapter);
var
  LChildNew: D;
  LChild: TSyncChildBase;
begin
  if (FChildAdapter <> nil) and (AAdapter <> nil) and (AAdapter.Current <> nil) then
  begin
    if AAdapter.Current is M then
    begin
      // List details of current master
      LChildNew := FGetChild(M(AAdapter.Current));
      FChildAdapter.SetDataObject(LChildNew, False)  // False because instances are owned by master
    end
    else
    begin
      // Empty list
      FChildAdapter.SetDataObject(nil, False);
    end;
    // CODE REVIEW.... What is the best way to handle nil?
    if FChildAdapter.Current <> nil then
      FChildAdapter.Active := True;

    for LChild in ChildArray do
      LChild.OnAfterScroll(ChildBindSource);
  end;

end;

procedure TSyncChildObjectLink<M, D>.OnBeforeScroll(
  const AAdapter: TBindSourceAdapter);
var
  LChild: TSyncChildBase;
begin
  if (FChildAdapter <> nil) and (AAdapter <> nil) then
  begin
    if FChildAdapter.State in seEditModes then
      FChildAdapter.Post;

    for LChild in ChildArray do
      LChild.OnBeforeScroll(ChildBindSource);
  end;
end;

{ TSyncDetailListLink<M, D> }

constructor TSyncChildListLink<M, D>.Create(const AOwner: M;
  const AGetChild: TFuncChildList<M, D>;
  const AChildArray: TSyncChildLinkArray);
var
  LChildNew: TList<D>;
begin
  inherited Create;
  FGetChild := AGetChild;
  ChildArray := AChildArray;

  LChildNew := FGetChild(AOwner);
  FChildListAdapter := TListBindSourceAdapter<D>.Create(nil, LChildNew, False);
  FChildListAdapter.AfterScroll := OnUIAfterScroll;
  FChildListAdapter.BeforeScroll := OnUIBeforeScroll;
end;

destructor TSyncChildListLink<M, D>.Destroy;
begin
  FChildListAdapter.Free;
  inherited;
end;

function TSyncChildListLink<M, D>.GetChildBindSource: TBindSourceAdapter;
begin
  Result := FChildListAdapter;
end;

procedure TSyncChildListLink<M, D>.OnAfterScroll(
  const AAdapter: TBindSourceAdapter);
var
  LChildNew : TList<D>;
  LChild: TSyncChildBase;
begin
  if (FChildListAdapter <> nil) and (AAdapter <> nil) then
  begin
    if AAdapter.Current is M then
    begin
      // List details of current master
      LChildNew := FGetChild(AAdapter.Current);
      FChildListAdapter.SetList(LChildNew, False)  // False because instances are owned by master
    end
    else
    begin
      // Empty list
      FChildListAdapter.SetList(nil, False);
    end;
    // CODE REVIEW.... What is the best way to handle nil?
    if FChildListAdapter.Current <> nil then
      FChildListAdapter.Active := True;

    for LChild in ChildArray do
      LChild.OnAfterScroll(FChildListAdapter);
  end;
end;

procedure TSyncChildListLink<M, D>.OnBeforeScroll(
  const AAdapter: TBindSourceAdapter);
var
  LChild: TSyncChildBase;
begin
  if (FChildListAdapter <> nil) and (AAdapter <> nil) then
  begin
    if FChildListAdapter.State in seEditModes then
      FChildListAdapter.Post;

    for LChild in ChildArray do
      LChild.OnBeforeScroll(FChildListAdapter);
  end;
end;

procedure TSyncChildListLink<M, D>.OnUIAfterScroll(
  AAdapter: TBindSourceAdapter);
var
  LChild: TSyncChildBase;
begin
  for LChild in ChildArray do
    LChild.OnAfterScroll(FChildListAdapter);
end;

procedure TSyncChildListLink<M, D>.OnUIBeforeScroll(
  AAdapter: TBindSourceAdapter);
var
  LChild: TSyncChildBase;
begin
  for LChild in ChildArray do
    LChild.OnBeforeScroll(FChildListAdapter);
end;

{ TSyncDetailBase }

destructor TSyncChildBase.Destroy;
var
  LChild: TSyncChildBase;
begin
  for LChild in ChildArray do
  begin
    if LChild <> nil then
      LChild.Free;
  end;
  inherited;
end;

procedure TSyncChildBase.SetChildArray(const AValue: TSyncChildLinkArray);
var
  LChild: TSyncChildBase;
begin
  // Assert no nulls.
  for LChild in AValue do
    Assert(Assigned(LChild),'Child Array Value cannot be null');

  FChildArray := AValue;
end;

end.
