import {SQLiteDatabaseClient} from "@abaplint/database-sqlite";

export async function setup(abap, schemas, insert) {
  abap.context.databaseConnections["DEFAULT"] = new SQLiteDatabaseClient();
  await abap.context.databaseConnections["DEFAULT"].connect();
  await abap.context.databaseConnections["DEFAULT"].execute(schemas.sqlite);
  await abap.context.databaseConnections["DEFAULT"].execute(insert);
}