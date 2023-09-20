import sqlite3 from "sqlite3";


/** @type {(file: string) => EffectFnAff<sqlite3.Database>} */
export const _database = (file) => (onError, onSuccess) => {
  const db = new sqlite3.Database(file, (err) => {
    if (err) onError(err);
    else onSuccess(db);
  });

  return  (cancelError, onCancelerError, onCancelerSuccess)  => {
    db.close((err) => {
      if (err) onCancelerError(err);
      else onCancelerSuccess();
    });
  };
};

/** @type {(db: sqlite3.Database) => EffectFnAff<void>} */
export const _close = (db) => (onError, onSuccess) => {
  db.close((err) => (err ? onError() : onSuccess()));
  return (_cancelError, _onCancelerError, onCancelerSuccess) =>
    onCancelerSuccess();
};

/** @type { (sql: string) => (params: string[]) => (db: sqlite3.Database) => EffectFnAff<void> } */
export const _run = (sql) => (params) => (db) => (onFail, onSucceed) => {
  db.run(sql, params, (err) => (err ? onFail(err) : onSucceed()));
};

/** @type {(sql: string) => (params: string[]) => (db: sqlite3.Database) => EffectFnAff<any> } */
export const _get = (sql) => (params) => (db) => (onFail, onSucceed) => {
  db.get(sql, params, (err, row) => (err ? onFail(err) : onSucceed(row)));
};

/** @type { (sql: string) => (params: string[]) => (db: sqlite3.Database) => EffectFnAff<any> } */
export const _all = (sql) => (params) => (db) => (onFail, onSucceed) => {
  db.all(sql, params, (err, rows) => (err ? onFail(err) : onSucceed(rows)));
};
