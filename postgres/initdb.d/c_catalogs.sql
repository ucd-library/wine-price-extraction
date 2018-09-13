-- Hold all media
SET search_path = catalogs,public,pg_catalog;

CREATE TABLE catalogs (
    catalog_id uuid primary key,
    ark text,
    title text,
    publisher text,
    year bigint,
    editable boolean default true,
    completed boolean default false,
    created_at timestamp without time zone default now(),
    updated_at timestamp without time zone default now()
);

\COPY catalogs (catalog_id,ark,title,publisher,year,editable,completed,created_at,updated_at) from /io/catalogs.csv with csv header;

create or replace function mint(catalogs)
returns text as $$
select format('_status:reserved\nerc.who:%s\nerc.what:%s\nerc.when:%s\nucd.catalog_id:%s',
$1.publisher,$1.title,$1.year,$1.catalog_id);
$$ LANGUAGE SQL IMMUTABLE;

CREATE TABLE jpegs (
file text
);

\COPY jpegs from /io/jpegs.csv with csv header;

CREATE TABLE catalog_xwalk (
cover_image text,
catalog_id uuid,
page_id uuid,
title text,
file_count integer,
pages integer,
year integer,
note text
);

\COPY catalog_xwalk (cover_image,catalog_id,page_id,title,file_count,pages,year,note) from /io/catalog_xwalk.csv with csv header

CREATE TABLE pages (
    page_id uuid primary key,
    catalog_id uuid,
    page integer,
    editable boolean default true,
    completed boolean default false,
    created_at timestamp without time zone default now()
);

\COPY pages (page_id,catalog_id,page,editable,completed,created_at) from /io/pages.csv with csv header;

create or replace view jwalk as
WITH n AS (
 SELECT regexp_replace(xwalk.cover_image, '.(d|p)ng|.png'::text, ''::text) AS file,
 regexp_replace(xwalk.cover_image, 'UCD_Lehmann_(\d+)(.(d|p)ng)?'::text, '\1'::text)::integer AS num,
 xwalk.catalog_id,
 c.ark
 FROM catalog_xwalk xwalk
 JOIN catalogs c using (catalog_id)
),
xx as (
  select xn.catalog_id,
  min(xx.num)-1 as max
  from n xn join n xx on (xn.num < xx.num)
  group by xn.catalog_id
  union
  select catalog_id,num+1 as max
  from n join (select max(num) as num from n) as x using (num)
),
x as (
 select * from n join xx using (catalog_id)
),
j AS (
 SELECT regexp_replace(jpegs.file, '.jpg'::text, ''::text) AS file,
 regexp_replace(jpegs.file, 'UCD_Lehmann_(\d+).jpg'::text, '\1'::text)::integer AS num
 FROM jpegs
),
c AS (
  SELECT j.file,
  x.num AS cover
  FROM x
  JOIN j ON ((j.num >= x.num)
    and ((j.num < x.max) -- do not get last color pic
         OR
         (j.num <=x.max and x.num=x.max)))
)
SELECT c.file,
   x.catalog_id,
   x.ark,
   c.cover,
   j.num - c.cover AS page
  FROM c
    JOIN j USING (file)
    JOIN x ON c.cover = x.num
ORDER BY c.cover, (j.num - c.cover);


CREATE TABLE page_xwalk (
file text,
page_id uuid unique,
catalog_id uuid,
page integer);

\COPY page_xwalk (file,page_id,catalog_id,page) from /io/page_xwalk.csv with csv header

CREATE TABLE marks (
mark_id uuid unique,
user_id text,
page_id uuid,
xy integer[2],
type text, --references mark_type (type),
wine_type text, --references wine_type(wine_type),
color text, --references wine_color(color),
county text, --references countries(country),
name text not null,
section text default null,
anonymous boolean,
vintage integer default null,
bottle_type text references bottle_info(bottle_type),
perprice decimal(6,2),
caseprice decimal(6,2),
created timestamp,
updated timestamp
);

\COPY marks from /io/marks.csv with csv header
