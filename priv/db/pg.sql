-- CREATE DATABASE wispo_api;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- SELECT * FROM pg_available_extensions;

CREATE TABLE IF NOT EXISTS wispo_api_users (
    id      BIGSERIAL NOT NULL PRIMARY KEY,
    jid     VARCHAR(3071) DEFAULT NULL,
    phone   VARCHAR(22) NOT NULL UNIQUE
);

CREATE UNIQUE INDEX IF NOT EXISTS wispo_api_users_jid_key ON wispo_api_users USING btree (jid);
CREATE UNIQUE INDEX IF NOT EXISTS wispo_api_users_phone_key ON wispo_api_users USING btree (phone);
