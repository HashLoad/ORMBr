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
  {$IFDEF HAS_FMX}
    FMX.Graphics,
  {$ELSE}
    // Delphi > 2010 adicionar em:
    // Option->Delphi Compiler->Unit scope names, "Vcl", "Vcl.Imaging"
    Graphics,
    GIFImg,
    JPEG,
    PngImage,
    pnglang,
    {$IF COMPILERVERSION > 23}
      AnsiStrings,
    {$IFEND}
  {$ENDIF}
  // CodeBase64 e DecodeBase64
  {$IFDEF HAS_NET_ENCODING}
    System.NetEncoding,
  {$ELSE IFDEF HAS_SOAP_ENCODING}
    Soap.EncdDecd,
  {$ELSE}
    encddecd,
  {$IFEND}
  ZLib;

type
  TBlob = record
  private
    FBase64String: String;
    FBase64Bytes: TBytes;
    FBlobField: TBlobField;
    function StreamToByteArray(AStream: TStream): TBytes;
    {$IFNDEF HAS_FMX}
    function FindGraphicClass(const ABuffer; const ABufferSize: Int64;
      out AGraphicClass: TGraphicClass): Boolean; overload;
    function FindGraphicClass(AStream: TStream;
      out AGraphicClass: TGraphicClass): Boolean; overload;
    {$ENDIF}
    procedure CompressStream(ASource, ATarget: TStream);
    procedure DecompressStream(ASource, ATarget: TStream);
    procedure BuildBlobFieldToStream(const ACompression: Boolean = False);
    function GetEncodeBase64: String;
    function GetDecodeBase64(const Value: String): Boolean;
  public
    procedure SetBlobField(const Value: TBlobField; const ACompression: Boolean = False);
    procedure SetBytes(const Value: TBytes);
    procedure LoadFromFile(const AFileName: string; const ACompression: Boolean = False);
    procedure SaveToFile(const FileName: string);
    {$IFNDEF HAS_FMX}
    procedure ToPicture(APicture: TPicture; const ACompression: Boolean = False);
    {$ENDIF}
    procedure ToBitmap(ABitmap: TBitmap; const ACompression: Boolean = False);
    function ToBytes: TBytes;
    function ToBytesString: string; overload;
    function ToStringBytes(const AString: string): Boolean; overload;
    function ToString: String;
    function ToSize: Integer;
  end;

implementation

const
  MinGraphicSize = 44;

{ TBlob }

procedure TBlob.SetBlobField(const Value: TBlobField; const ACompression: Boolean);
begin
  if Value.IsBlob then
  begin
    FBlobField := Value;
    // Gera Stream do BlobField e armazena em var interna
    BuildBlobFieldToStream(ACompression);
  end
  else
    raise Exception.Create(Format('Column [%s] must have blob value', [Value.FieldName]));
end;

procedure TBlob.SetBytes(const Value: TBytes);
begin
  FBase64Bytes := Value;
  // Codifica os Bytes em string
  FBase64String := ToBytesString;
end;

function TBlob.ToBytesString: string;
begin
  Result := GetEncodeBase64;
end;

function TBlob.ToString: String;
begin
  Result := FBase64String;
end;

function TBlob.ToStringBytes(const AString: string): Boolean;
begin
  Result := GetDecodeBase64(AString);
end;

procedure TBlob.BuildBlobFieldToStream(const ACompression: Boolean);
var
  LSourceStream: TMemoryStream;
  LTargetStream: TMemoryStream;
begin
  if not Assigned(FBlobField) then
    Exit;
  LSourceStream := TMemoryStream.Create;
  try
    TBlobField(FBlobField).SaveToStream(LSourceStream);
    LSourceStream.Position := 0;
    if ACompression then
    begin
      LTargetStream := TMemoryStream.Create;
      try
        CompressStream(LSourceStream, LTargetStream);
        FBase64Bytes := StreamToByteArray(LTargetStream);
      finally
        LTargetStream.Free;
      end;
    end
    else
      FBase64Bytes := StreamToByteArray(LSourceStream);
    FBase64String := ToBytesString;
  finally
    LSourceStream.Free;
  end;
end;

function TBlob.StreamToByteArray(AStream: TStream): TBytes;
begin
  if Assigned(AStream) then
  begin
    AStream.Position := 0;
    SetLength(Result, AStream.Size);
    AStream.Read(Pointer(Result)^, AStream.Size);
  end
  else
    SetLength(Result, 0);
end;

{$IFNDEF HAS_FMX}
procedure TBlob.ToPicture(APicture: TPicture; const ACompression: Boolean);
var
  LGraphic: TGraphic;
  LSourceStream: TMemoryStream;
  LTargetStream: TMemoryStream;
  LGraphicClass: TGraphicClass;
begin
  LGraphic := nil;
  LSourceStream := TMemoryStream.Create;
  try
    if Length(FBase64Bytes) = 0 then
    begin
      APicture.Assign(nil);
      Exit;
    end;
    LSourceStream.Write(FBase64Bytes, ToSize);
    if ACompression then
    begin
      LTargetStream := TMemoryStream.Create;
      try
        DecompressStream(LSourceStream, LTargetStream);
        if not FindGraphicClass(LTargetStream.Memory^, LTargetStream.Size, LGraphicClass) then
          raise EInvalidGraphic.Create('Invalid image');
        LGraphic := LGraphicClass.Create;
        LTargetStream.Position := 0;
        LGraphic.LoadFromStream(LTargetStream);
      finally
        LTargetStream.Free;
      end;
    end
    else
    begin
      if not FindGraphicClass(LSourceStream.Memory^, LSourceStream.Size, LGraphicClass) then
        raise EInvalidGraphic.Create('Invalid image');
      LGraphic := LGraphicClass.Create;
      LSourceStream.Position := 0;
      LGraphic.LoadFromStream(LSourceStream);
    end;
    APicture.Assign(LGraphic);
  finally
    LSourceStream.Free;
    LGraphic.Free;
  end;
end;
{$ENDIF}

function TBlob.ToSize: Integer;
begin
  Result := Length(FBase64Bytes);
end;

{$IFNDEF HAS_FMX}
function TBlob.FindGraphicClass(const ABuffer; const ABufferSize: Int64;
  out AGraphicClass: TGraphicClass): Boolean;
var
  LLongWords: Array[Byte] of LongWord absolute ABuffer;
  LWords: Array[Byte] of Word absolute ABuffer;
begin
  AGraphicClass := nil;
  Result := False;
  if ABufferSize < MinGraphicSize then
    Exit;
  case LWords[0] of
    $4D42: AGraphicClass := TBitmap;
    $D8FF: AGraphicClass := TJPEGImage;
    $4949: if LWords[1] = $002A then AGraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if LWords[1] = $2A00 then AGraphicClass := TWicImage; //i.e., TIFF
  else
    if Int64(ABuffer) = $A1A0A0D474E5089 then
      AGraphicClass := TPNGImage
    else
    if LLongWords[0] = $9AC6CDD7 then
      AGraphicClass := TMetafile
    else
    if (LLongWords[0] = 1) and (LLongWords[10] = $464D4520) then
      AGraphicClass := TMetafile
    else
{$if CompilerVersion > 23}
    if AnsiStrings.AnsiStrLComp(PAnsiChar(@ABuffer), PAnsiChar('GIF'), 3) = 0 then
{$else}
    if AnsiStrLComp(PAnsiChar(@ABuffer), PAnsiChar('GIF'), 3) = 0 then
{$ifend}
      AGraphicClass := TGIFImage
    else
    if LWords[1] = 1 then
      AGraphicClass := TIcon;
  end;
  Result := (AGraphicClass <> nil);
end;

function TBlob.FindGraphicClass(AStream: TStream;
  out AGraphicClass: TGraphicClass): Boolean;
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
{$ENDIF}

procedure TBlob.LoadFromFile(const AFileName: string;
  const ACompression: Boolean = False);
var
  LSourceStream: TMemoryStream;
  LTargetStream: TMemoryStream;
begin
  LSourceStream := TMemoryStream.Create;
  try
    LSourceStream.LoadFromFile(AFileName);
    LSourceStream.Position := 0;
    if ACompression then
    begin
      LTargetStream := TMemoryStream.Create;
      try
        CompressStream(LSourceStream, LTargetStream);
        FBase64Bytes := StreamToByteArray(LTargetStream);
      finally
        LTargetStream.Free;
      end
    end
    else
      FBase64Bytes := StreamToByteArray(LSourceStream);
    FBase64String := ToBytesString;
  finally
    LSourceStream.Free;
  end
end;

procedure TBlob.ToBitmap(ABitmap: TBitmap; const ACompression: Boolean);
var
  LSourceStream: TMemoryStream;
  LTargetStream: TMemoryStream;
begin
  LSourceStream := TMemoryStream.Create;
  try
    if Length(FBase64Bytes) = 0 then
    begin
      ABitmap.Assign(nil);
      Exit;
    end;
    LSourceStream.Write(FBase64Bytes, ToSize);
    if ACompression then
    begin
      LTargetStream := TMemoryStream.Create;
      try
        DecompressStream(LSourceStream, LTargetStream);
        LTargetStream.Position := 0;
        ABitmap.LoadFromStream(LTargetStream);
      finally
        LTargetStream.Free;
      end;
    end
    else
    begin
      LSourceStream.Position := 0;
      ABitmap.LoadFromStream(LSourceStream);
    end;
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

/// <summary>
///   clNone    - Não especifica nenhuma compressão; os dados são meramente copiados para o fluxo de saída.
///   clFastest - Especifica a compressão mais rápida, resultando em um arquivo maior.
///   clDefault - Compromisso entre velocidade e quantidade de compressão.
///   clMax     - Especifica a compressão máxima, resultando em um tempo maior para realizar a operação.
/// </summary>
procedure TBlob.CompressStream(ASource, ATarget: TStream);
var
  LStream: TCompressionStream;
begin
  LStream := TCompressionStream.Create(clDefault, ATarget);
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
  ASource.Seek(0, soFromBeginning);
  ATarget.Seek(0, soFromBeginning);
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

{$IFDEF HAS_NET_ENCODING}
function TBlob.GetEncodeBase64: String;
var
  LNetEncoding: TBase64Encoding;
begin
  LNetEncoding := TBase64Encoding.Create;
  try
    Result := LNetEncoding.EncodeBytesToString(FBase64Bytes, ToSize);
  finally
    LNetEncoding.Free;
  end;
end;

function TBlob.GetDecodeBase64(const Value: String): Boolean;
var
  LNetEncoding: TBase64Encoding;
begin
  Result := False;
  LNetEncoding := TBase64Encoding.Create;
  try
    FBase64Bytes := LNetEncoding.DecodeStringToBytes(Value);
    FBase64String := Value;
    Result := True;
  finally
    LNetEncoding.Free;
  end;
end;
{$ENDIF}

{$IFDEF HAS_SOAP_ENCODING}
function TBlob.GetEncodeBase64: String;
begin
  Result := EncodeBase64(FBase64Bytes, ToSize);
end;

function TBlob.GetDecodeBase64(const Value: String): Boolean;
begin
  Result := False;
  FBase64Bytes := DecodeBase64(AString);
  FBase64String := Value;
  Result := True;
end;
{$ENDIF}

{$IFDEF HAS_ENCDDECD}
function TBlob.GetEncodeBase64: String;
begin
  Result := EncodeBase64(FBase64Bytes, ToSize);
end;

function TBlob.GetDecodeBase64(const Value: String): Boolean;
begin
  Result := False;
  FBase64Bytes := DecodeBase64(AString);
  FBase64String := Value;
  Result := True;
end;
{$ENDIF}

end.
