-- migrate:up
create table users (
  id text primary key,
  created_at datetime not null default (datetime('now')),
  email text not null unique,
  hashed_password text not null
);

-- migrate:down
drop table users;
