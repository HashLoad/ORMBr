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

// Recomendação da Embarcadero por usar o tipo: PByte
{$POINTERMATH ON}

unit ormbr.encddecd;

interface

uses
  Classes,
  SysUtils,
  Character;

procedure EncodeStream(Input, Output: TStream);
procedure DecodeStream(Input, Output: TStream);
function  EncodeString(const Input: string): string;
function  DecodeString(const Input: string): string;
function  DecodeBase64(const Input: string): TBytes;
function  EncodeBase64(const Input: Pointer; Size: Integer): string;

implementation

uses RTLConsts;

const
  EncodeTable: array[0..63] of Char =
    String('ABCDEFGHIJKLMNOPQRSTUVWXYZ') +
    String('abcdefghijklmnopqrstuvwxyz') +
    String('0123456789+/');

  DecodeTable: array[#0..#127] of Integer = (
    Byte('='), 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
           64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
           64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
           52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
           64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
           15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
           64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
           41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64);

type
  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
      3: (c: array[0..3] of Char);
  end;

  TPointerStream = class(TCustomMemoryStream)
  public
    constructor Create(P: Pointer; Size: Integer);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

procedure EncodePacket(const Packet: TPacket; NumChars: Integer; OutBuf: PByte);
begin
  OutBuf[0] := Ord(EnCodeTable[Packet.a[0] shr 2]);
  OutBuf[1] := Ord(EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f]);
  if NumChars < 2 then
    OutBuf[2] := Ord('=')
  else OutBuf[2] := Ord(EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f]);
  if NumChars < 3 then
    OutBuf[3] := Ord('=')
  else OutBuf[3] := Ord(EnCodeTable[Packet.a[2] and $0000003f]);
end;

function DecodePacket(InBuf: PByte; var nChars: Integer): TPacket;
begin
  Result.a[0] := (DecodeTable[Chr(InBuf[0])] shl 2) or
    (DecodeTable[Chr(InBuf[1])] shr 4) ;
  NChars := 1;
  if Chr(InBuf[2]) <> '=' then
  begin
    Inc(NChars);
    Result.a[1] := Byte((DecodeTable[Chr(InBuf[1])] shl 4) or (DecodeTable[Chr(InBuf[2])] shr 2));
  end;
  if Chr(InBuf[3]) <> '=' then
  begin
    Inc(NChars);
    Result.a[2] := Byte((DecodeTable[Chr(InBuf[2])] shl 6) or DecodeTable[Chr(InBuf[3])]);
  end;
end;

procedure EncodeStream(Input, Output: TStream);
type
  PInteger = ^Integer;
var
  InBuf: array[0..509] of Byte;
  OutBuf: array[0..1023] of Char;
  BufPtr: PByte;
  I, J, K, BytesRead: Integer;
  Packet: TPacket;
begin
  K := 0;
  repeat
    BytesRead := Input.Read(InBuf, SizeOf(InBuf));
    I := 0;
    BufPtr := @OutBuf;
    while I < BytesRead do
    begin
      if BytesRead - I < 3 then
        J := BytesRead - I
      else J := 3;
      Packet.i := 0;
      Packet.b0 := InBuf[I];
      if J > 1 then
        Packet.b1 := InBuf[I + 1];
      if J > 2 then
        Packet.b2 := InBuf[I + 2];
      EncodePacket(Packet, J, BufPtr);
      Inc(I, 3);
      Inc(BufPtr, 4);
      Inc(K, 4);
      if K > 75 then
      begin
        BufPtr[0] := Ord(#$0D);
        BufPtr[1] := Ord(#$0A);
        Inc(BufPtr, 2);
        K := 0;
      end;
    end;
    Output.Write(Outbuf, BufPtr - PChar(@OutBuf));
  until BytesRead = 0;
end;

procedure DecodeStream(Input, Output: TStream);
var
  InBuf: array[0..75] of Char;
  OutBuf: array[0..60] of Byte;
  InBufPtr, OutBufPtr: PByte;
  I, J, K, BytesRead: Integer;
  Packet: TPacket;

  procedure SkipWhite;
  var
    C: Char;
    NumRead: Integer;
  begin
    while True do
    begin
      NumRead := Input.Read(C, 1);
      if NumRead = 1 then
      begin
        if CharInSet(C, ['0'..'9','A'..'Z','a'..'z','+','/','=']) then
        begin
          Input.Position := Input.Position - 1;
          Break;
        end;
      end else Break;
    end;
  end;

  function ReadInput: Integer;
  var
    WhiteFound, EndReached : Boolean;
    CntRead, Idx, IdxEnd: Integer;
  begin
    IdxEnd:= 0;
    repeat
      WhiteFound := False;
      CntRead := Input.Read(InBuf[IdxEnd], (SizeOf(InBuf)-IdxEnd));
      EndReached := CntRead < (SizeOf(InBuf)-IdxEnd);
      Idx := IdxEnd;
      IdxEnd := CntRead + IdxEnd;
      while (Idx < IdxEnd) do
      begin
        if not (CharInSet(InBuf[Idx], ['0'..'9','A'..'Z','a'..'z','+','/','='])) then
        begin
          Dec(IdxEnd);
          if Idx < IdxEnd then
            Move(InBuf[Idx+1], InBuf[Idx], IdxEnd-Idx);
          WhiteFound := True;
        end
        else
          Inc(Idx);
      end;
    until (not WhiteFound) or (EndReached);
    Result := IdxEnd;
  end;

begin
  repeat
    SkipWhite;
    BytesRead := ReadInput;
    InBufPtr := @InBuf;
    OutBufPtr := @OutBuf;
    I := 0;
    while I < BytesRead do
    begin
      Packet := DecodePacket(InBufPtr, J);
      K := 0;
      while J > 0 do
      begin
        OutBufPtr^ := (Packet.a[K]);
        Inc(OutBufPtr);
        Dec(J);
        Inc(K);
      end;
      Inc(InBufPtr, 4);
      Inc(I, 4);
    end;
    I := OutBufPtr - PByte(@OutBuf);
    Output.Write(OutBuf, I);
  until BytesRead = 0;
end;

function EncodeString(const Input: string): string;
var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input);
  try
    OutStr := TStringStream.Create('');
    try
      EncodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

function DecodeString(const Input: string): string;
var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input);
  try
    OutStr := TStringStream.Create('');
    try
      DecodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

constructor TPointerStream.Create(P: Pointer; Size: Integer);
begin
  SetPointer(P, Size);
end;

function TPointerStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos, EndPos, Size: Longint;
  Mem: Pointer;
begin
  Pos := Self.Position;

  if (Pos >= 0) and (Count > 0) then
  begin
    EndPos := Pos + Count;
    Size:= Self.Size;
    if EndPos > Size then
      raise EStreamError.CreateRes(@SMemoryStreamError);

    Mem := Self.Memory;
    System.Move(Buffer, Pointer(Longint(Mem) + Pos)^, Count);
    Self.Position := Pos;
    Result := Count;
    Exit;
  end;
  Result := 0;
end;

function DecodeBase64(const Input: String): TBytes;
var
  InStr: TPointerStream;
  OutStr: TBytesStream;
  Len: Integer;
begin
  InStr := TPointerStream.Create(PChar(Input), Length(Input));
  try
    OutStr := TBytesStream.Create;
    try
      DecodeStream(InStr, OutStr);
      Result := OutStr.Bytes;
      Len := OutStr.Size;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
  SetLength(Result, Len);
end;

function EncodeBase64(const Input: Pointer; Size: Integer): String;
var
  InStr: TPointerStream;
  OutStr: TBytesStream;
begin
  InStr := TPointerStream.Create(Input, Size);
  try
    OutStr := TBytesStream.Create;
    try
      EncodeStream(InStr, OutStr);
      if OutStr.Size > 0 then
      begin
        SetLength(Result, OutStr.Size);
        Move(OutStr.Memory^, Result[1], OutStr.Size);
      end;
//      Access violantion por algum motivo ao tentar carregar a logo do sistema
//      SetString(Result, PChar(OutStr.Memory), OutStr.Size);
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

end.
