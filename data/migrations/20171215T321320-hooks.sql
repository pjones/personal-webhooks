create sequence hooks_id_seq;

create table hooks (
  id         bigint primary key default nextval('hooks_id_seq'),
  code       text not null,
  expires_at timestamp with time zone,
  action     json not null,

  constraint hooks_unique_code unique(code),
  constraint hooks_nonblank_code check(code <> '')
);
