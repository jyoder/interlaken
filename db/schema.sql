CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(255) primary key);
CREATE TABLE users (
  id text primary key,
  created_at datetime not null default (datetime('now')),
  email text not null unique,
  hashed_password text not null
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20220628034935');
