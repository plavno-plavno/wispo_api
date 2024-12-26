-- CREATE DATABASE wispo_api;

CREATE TABLE IF NOT EXISTS wispo_api_phones (
    id              BIGSERIAL NOT NULL PRIMARY KEY,
    phone           VARCHAR(...) NOT NULL UNIQUE,
    code            VARCHAR(4) DEFAULT NULL,
    code_exp        INTEGER DEFAULT NULL,
    is_confirmed    BOOL NOT NULL DEFAULE FALSE
);

CREATE TABLE IF NOT EXISTS wispo_api_synced_contacts (
    id              BIGSERIAL NOT NULL PRIMARY KEY,
    phone           VARCHAR(...) NOT NULL UNIQUE,
    contacts        ...
);