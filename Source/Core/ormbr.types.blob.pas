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

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.types.blob;

interface

uses
  DB,
  Classes,
  SysUtils,
  {$IFDEF FML}
  FMX.Graphics,
  {$ELSE}
  AnsiStrings,
  Graphics,
  GIFImg,
  JPEG,
  PngImage,
  pnglang,
  {$ENDIF FML}
  ZLib,
  ormbr.encddecd;

type
  TBlob = record
  private
    FBase64String: String;
    FBase64Bytes: TBytes;
    FBlobField: TBlobField;
    function StreamToByteArray(AStream: TStream): TBytes;
    {$IFNDEF FML}
    function FindGraphicClass(const ABuffer; const ABufferSize: Int64;
      out AGraphicClass: TGraphicClass): Boolean; overload;
    function FindGraphicClass(AStream: TStream;
      out AGraphicClass: TGraphicClass): Boolean; overload;
    {$ENDIF FML}
    procedure CompressStream(ASource, ATarget: TStream);
    procedure DecompressStream(ASource, ATarget: TStream);
    procedure BuildBlobFieldToStream;
  public
    procedure SetBlobField(const Value: TBlobField);
    procedure SetBytes(const Value: TBytes);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const FileName: string);
    {$IFNDEF FML}
    procedure ToPicture(APicture: TPicture);
    {$ENDIF FML}
    function ToBytes: TBytes;
    function ToBytesString: string; overload;
    function ToBytesString(const AString: string): Boolean; overload;
    function ToString: String;
    function ToSize: Integer;
  end;

implementation

const
  MinGraphicSize = 44;

{ TBlob }

procedure TBlob.SetBlobField(const Value: TBlobField);
begin
  if Value.IsBlob then
  begin
    FBlobField := Value;
    /// <summary>
    /// Gera Stream do BlobField e armazena em var interna
    /// </summary>
    BuildBlobFieldToStream;
  end
  else
    raise Exception.Create(Format('Column [%s] must have blob value', [Value.FieldName]));
end;

procedure TBlob.SetBytes(const Value: TBytes);
begin
  FBase64Bytes := Value;
  /// <summary>
  /// Codifica os Bytes em string
  /// </summary>
  FBase64String := String(EncodeBase64(FBase64Bytes, Length(FBase64Bytes)));
end;

procedure TBlob.BuildBlobFieldToStream;
var
  LSourceStream: TMemoryStream;
//  LTargetStream: TMemoryStream;
begin
  if Assigned(FBlobField) then
  begin
    LSourceStream := TMemoryStream.Create;
//    LTargetStream := TMemoryStream.Create;
    try
      TBlobField(FBlobField).SaveToStream(LSourceStream);
      LSourceStream.Position := 0;
      /// <summary>
      /// Compressão dos dados
      /// </summary>
//      CompressStream(LSourceStream, LTargetStream);
      /// <summary>
      /// Gera cadeia de Bytes
      /// </summary>
      FBase64Bytes := StreamToByteArray(LSourceStream);
      /// <summary>
      /// Codifica os Bytes em string
      /// </summary>
      FBase64String := String(EncodeBase64(FBase64Bytes, Length(FBase64Bytes)));
    finally
      LSourceStream.Free;
//      LTargetStream.Free;
    end;
  end;
end;

function TBlob.StreamToByteArray(AStream: TStream): TBytes;
begin
  if Assigned(AStream) then
  begin
    AStream.Position := 0;
    SetLength(Result, AStream.Size);
    AStream.Read(pointer(Result)^, AStream.Size);
  end
  else
    SetLength(Result, 0);
end;

{$IFNDEF FML}
procedure TBlob.ToPicture(APicture: TPicture);
var
  LGraphic: TGraphic;
  LSourceStream: TMemoryStream;
//  LTargetStream: TMemoryStream;
  LGraphicClass: TGraphicClass;
begin
  LGraphic := nil;
  LSourceStream := TMemoryStream.Create;
//  LTargetStream := TMemoryStream.Create;
  try
    if Length(FBase64Bytes) = 0 then
    begin
      APicture.Assign(nil);
      Exit;
    end;
    LSourceStream.Write(FBase64Bytes, Length(FBase64Bytes));
//    DecompressStream(LSourceStream, LTargetStream);
    {$IFNDEF FML}
    if not FindGraphicClass(LSourceStream.Memory^, LSourceStream.Size, LGraphicClass) then
      raise EInvalidGraphic.Create('Invalid image');
    {$ENDIF FML}
    LGraphic := LGraphicClass.Create;
    LSourceStream.Position := 0;
    LGraphic.LoadFromStream(LSourceStream);
    APicture.Assign(LGraphic);
  finally
    LSourceStream.Free;
//    LTargetStream.Free;
    LGraphic.Free;
  end;
end;
{$ENDIF FML}

function TBlob.ToSize: Integer;
begin
  Result := Length(FBase64Bytes);
end;

function TBlob.ToString: String;
begin
  Result := DecodeString(FBase64String);
end;

function TBlob.ToBytesString(const AString: string): Boolean;
begin
  FBase64String := AString;
  Result := True;
end;

function TBlob.ToBytesString: string;
begin
  Result := FBase64String;
end;

{$IFNDEF FML}
function TBlob.FindGraphicClass(const ABuffer; const ABufferSize: Int64;
  out AGraphicClass: TGraphicClass): Boolean;
var
  LLongWords: Array[Byte] of LongWord absolute ABuffer;
  LWords: Array[Byte] of Word absolute ABuffer;
begin
  AGraphicClass := nil;
  Result := False;
  if ABufferSize < MinGraphicSize then Exit;
  case LWords[0] of
    $4D42: AGraphicClass := TBitmap;
    $D8FF: AGraphicClass := TJPEGImage;
    $4949: if LWords[1] = $002A then AGraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if LWords[1] = $2A00 then AGraphicClass := TWicImage; //i.e., TIFF
  else
    if Int64(ABuffer) = $A1A0A0D474E5089 then
      AGraphicClass := TPNGImage
    else if LLongWords[0] = $9AC6CDD7 then
      AGraphicClass := TMetafile
    else if (LLongWords[0] = 1) and (LLongWords[10] = $464D4520) then
      AGraphicClass := TMetafile
    else if SysUtils.AnsiStrLComp(PAnsiChar(@ABuffer), PAnsiChar('GIF'), 3) = 0 then
      AGraphicClass := TGIFImage
    else if LWords[1] = 1 then
      AGraphicClass := TIcon;
  end;
  Result := (AGraphicClass <> nil);
end;

function TBlob.FindGraphicClass(AStream: TStream; out AGraphicClass: TGraphicClass): Boolean;
var
  LBuffer: PByte;
  LCurPos: Int64;
  LBytesRead: Integer;
begin
  if AStream is TCustomMemoryStream then
  begin
    LBuffer := TCustomMemoryStream(AStream).Memory;
    LCurPos := AStream.Position;
    Inc(LBuffer, LCurPos);
    Result := FindGraphicClass(LBuffer^, AStream.Size - LCurPos, AGraphicClass);
    Exit;
  end;
  GetMem(LBuffer, MinGraphicSize);
  try
    LBytesRead := AStream.Read(LBuffer^, MinGraphicSize);
    AStream.Seek(-LBytesRead, soCurrent);
    Result := FindGraphicClass(LBuffer^, LBytesRead, AGraphicClass);
  finally
    FreeMem(LBuffer);
  end;
end;
{$ENDIF FML}

procedure TBlob.LoadFromFile(const AFileName: string);
var
  LSourceStream: TMemoryStream;
begin
  LSourceStream := TMemoryStream.Create;
  try
    LSourceStream.LoadFromFile(AFileName);
    LSourceStream.Position := 0;
    /// <summary>
    /// Compressão dos dados
    /// </summary>
//      CompressStream(LSourceStream, LTargetStream);
    /// <summary>
    /// Gera cadeia de Bytes
    /// </summary>
    FBase64Bytes := StreamToByteArray(LSourceStream);
    /// <summary>
    /// Codifica os Bytes em string
    /// </summary>
    FBase64String := String(EncodeBase64(FBase64Bytes, Length(FBase64Bytes)));
  finally
    LSourceStream.Free;
  end;
end;

function TBlob.ToBytes: TBytes;
begin
  Result := FBase64Bytes;
end;

procedure TBlob.SaveToFile(const FileName: string);
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create(FBase64Bytes);
  try
    LStream.SaveToFile(FileName);
  finally
    LStream.Free;
  end;
end;

procedure TBlob.CompressStream(ASource, ATarget: TStream);
var
  LStream: TCompressionStream;
begin
  LStream := TCompressionStream.Create(clFastest, ATarget);
  try
    LStream.CopyFrom(ASource, ASource.Size);
    LStream.CompressionRate;
  finally
    LStream.Free;
  end;
end;

procedure TBlob.DecompressStream(ASource, ATarget: TStream) ;
var
  LStream: TDecompressionStream;
  LRead: Integer;
  LBuffer: Array [0..1023] of Char;
begin
  LStream := TDecompressionStream.Create(ASource);
  try
    repeat
      LRead := LStream.Read(LBuffer, SizeOf(LBuffer));
      if LRead <> 0 then
        ATarget.Write(LBuffer, LRead);
    until LRead <= 0;
  finally
    LStream.Free;
  end;
end;

end.
