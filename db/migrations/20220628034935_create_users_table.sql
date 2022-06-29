-- migrate:up
create table users (
  id text primary key,
  created_at date not null,
  updated_at date not null,
  email text not null unique,
  hashed_password text not null
);

-- migrate:down
drop table users;
