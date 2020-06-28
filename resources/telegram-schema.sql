CREATE TABLE IF NOT EXISTS telegram_update  (
  id INTEGER UNIQUE NOT NULL PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS telegram_message (
  id SERIAL UNIQUE NOT NULL PRIMARY KEY,
  note TEXT NOT NULL ,
  author INTEGER NOT NULL references app_user (id)
);

CREATE TABLE IF NOT EXISTS app_session (
  id UUID UNIQUE NOT NULL PRIMARY KEY,
  csrf TEXT UNIQUE NOT NULL,
  authed INTEGER references app_user (id)
);
