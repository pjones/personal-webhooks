/* Hooks */
create table hooks (
  id         bigserial primary key,
  code       text not null,
  expires_at timestamp with time zone,
  action     json not null,

  constraint hooks_unique_code unique(code),
  constraint hooks_nonblank_code check(code <> '')
);

create index on hooks (code, expires_at);
