CREATE TABLE invoice (
    id integer primary key autoincrement,
    title text not null
);

CREATE TABLE invoice_item (
    id integer primary key autoincrement,
    invoice_id integer not null references invoice(id),
    title text not null,
    work_date date not null,
    rate decimal not null,
    hours decimal not null
);

INSERT INTO SQLITE_SEQUENCE VALUES ('invoice', 100);
