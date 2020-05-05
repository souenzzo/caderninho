CREATE TABLE IF NOT EXISTS app_user  (
  id SERIAL UNIQUE NOT NULL PRIMARY KEY,
  username TEXT UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS app_todo (
  id SERIAL UNIQUE NOT NULL PRIMARY KEY,
  note TEXT NOT NULL ,
  author SERIAL NOT NULL references app_user (id)
);

CREATE TABLE IF NOT EXISTS app_session (
  id SERIAL UNIQUE NOT NULL PRIMARY KEY,
  csrf TEXT,
  authed SERIAL references app_user (id)
);
