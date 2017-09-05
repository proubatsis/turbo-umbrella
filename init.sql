CREATE TABLE invoice (
    id integer primary key autoincrement,
    title text not null
);

CREATE TABLE invoice_item (
    id integer primary key autoincrement,
    invoice_id integer not null references invoice(id),
    title text not null,
    work_date date not null
);

CREATE TABLE timesheet_entry (
    id integer primary key autoincrement,
    invoice_item_id not null references invoice_item(id),
    entry text,
    hours decimal
);

