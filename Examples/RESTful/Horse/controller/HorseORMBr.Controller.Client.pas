unit HorseORMBr.Controller.Client;

interface

uses
  Horse,
  HorseORMBr.DAO.Base,
  System.JSON,
  REST.Json,
  ormbr.json,
  System.Generics.Collections,
  ormbr.model.client,
  DM.Connection;

procedure List(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure Find(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure Insert(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure Update(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure Delete(Req: THorseRequest; Res: THorseResponse; Next: TProc);

implementation

procedure Delete(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  conn: TDMConn;
  dao : THorseORMBrDAOBase<TClient>;
  client : TClient;
  id : String;
begin
  id := Req.Params['id'];
  conn := TDMConn.Create(nil);
  try
    dao := THorseORMBrDAOBase<TClient>.create(conn.FDConnection1);
    try
      client := dao.findWhere('client_id = ' + id);
      try
        dao.delete(client);
        Res.Status(204);
      finally
        client.Free;
      end;
    finally
      dao.Free;
    end;
  finally
    conn.Free;
  end;
end;

procedure Update(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  conn: TDMConn;
  dao : THorseORMBrDAOBase<TClient>;
  client : TClient;
  id : String;
begin
  id := Req.Params['id'];
  conn := TDMConn.Create(nil);
  try
    dao := THorseORMBrDAOBase<TClient>.create(conn.FDConnection1);
    try
      client := dao.findWhere('client_id = ' + id);
      try
        dao.modify(client);
        TORMBrJson.JsonToObject(Req.Body, client);
        dao.update(client);
        Res.Send(TORMBrJson.ObjectToJsonString(client))
           .ContentType('application/json');
      finally
        client.Free;
      end;
    finally
      dao.Free;
    end;
  finally
    conn.Free;
  end;
end;

procedure Insert(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  conn: TDMConn;
  dao : THorseORMBrDAOBase<Tclient>;
  client : Tclient;
begin
  client := TORMBrJson.JsonToObject<Tclient>(Req.Body);
  try
    conn := TDMConn.Create(nil);
    try
      dao := THorseORMBrDAOBase<Tclient>.create(conn.FDConnection1);
      try
        dao.insert(client);
        Res.Send(TORMBrJson.ObjectToJsonString(client))
           .Status(201)
           .ContentType('application/json');
      finally
        dao.Free;
      end;
    finally
      conn.Free;
    end;
  finally
    client.Free;
  end;
end;

procedure Find(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  conn: TDMConn;
  dao : THorseORMBrDAOBase<Tclient>;
  client : Tclient;
  id : String;
begin
  id := Req.Params['id'];
  conn := TDMConn.Create(nil);
  try
    dao := THorseORMBrDAOBase<Tclient>.create(conn.FDConnection1);
    try
      client := dao.findWhere('client_id = ' + id);
      try
        Res.Send(TORMBrJson.ObjectToJsonString(client))
           .ContentType('application/json');
      finally
        client.Free;
      end;
    finally
      dao.Free;
    end;
  finally
    conn.Free;
  end;
end;

procedure List(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  clientList: TObjectList<Tclient>;
  conn: TDMConn;
  dao : THorseORMBrDAOBase<Tclient>;
begin
  conn := TDMConn.Create(nil);
  try
    dao := THorseORMBrDAOBase<Tclient>.create(conn.FDConnection1);
    try
      clientList := dao.listAll;
      try
        Res.Send(TORMBrJson.ObjectListToJsonString<Tclient>(clientList))
           .ContentType('application/json');
      finally
        clientList.Free;
      end;
    finally
      dao.Free;
    end;
  finally
    conn.Free;
  end;
end;

end.

