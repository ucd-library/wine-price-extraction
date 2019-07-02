create table hocr (
ark text,
rotation float,
hocr jsonb
);

--COPY hocr (ark,rotation,hocr) from /io/hocr.json;
--COPY hocr (ark,rotation,hocr) from /io/unrotated.json;
--COPY hocr (ark,rotation,hocr) from /io/rotated.json;

--  refresh materialized view carea;
--refresh materialized view par;
--refresh materialized view line;
--refresh materialized view words;

--  refresh materialized view carea; refresh materialized view par; refresh materialized view line; refresh materialized view words;

create materialized view carea as
with a as ( select
 ark,rotation,
 jsonb_array_elements(hocr->0->'children') as json from hocr
)
select ark,rotation,json->>'id' as carea,json from a;

create materialized view par as
with a as (select
 ark,rotation,carea,
 jsonb_array_elements(json->'children') as json
from carea)
 select ark,rotation,carea,json->>'id' as par,json from a;

create materialized view line as
with a as (select
 ark,rotation,carea,par,
 jsonb_array_elements(json->'children') as json
from par),
b as (
select
 ark,rotation,carea,par,json->>'id' as line,json,
 regexp_replace(json->>'title','^.*; x_wconf (\d+).*','\1') as x_wconf,
 regexp_replace(json->>'title','^.*; basejson ((-?[.\d]+) (-?[.\d]+)).*','\1') as baseline,
 regexp_split_to_array(regexp_replace(json->>'title',';.*',''),' ') as b
 from a
)
select
 row_number() over () as line_id,
 ark,rotation,carea,par,line,x_wconf,json,
 st_setsrid(st_makebox2d(st_makepoint(b[2]::integer,-b[3]::integer),st_makepoint(b[4]::integer,-b[5]::integer)),32662) as bbox
 from b;

drop function if exists ocr_title_parm(in l json,in key text,out v text[]);
create function ocr_title_parm(in l json,in key text,out v text[])
LANGUAGE SQL AS $$
with a as (
 select regexp_split_to_array(unnest(t),' ') as v
 from line,regexp_split_to_array($1->>'title','\s*;\s*') as t
)
select v[2:100] as v from a where v[1]=$2
$$;

create materialized view words as
with a as (
select
 ark,rotation,carea,par,line,line_id,
 jsonb_array_elements(json->'children') as json
from line
),
b as (
 select
 ark,rotation,carea,par,line,line_id,
 json->>'id' as word,
 json->>'text' as text,
 regexp_replace(json->>'title','^.*; x_wconf (\d+).*','\1') as x_wconf,
 regexp_split_to_array(regexp_replace(json->>'title',';.*',''),' ') as b
 from a
)
select
 row_number() over () as word_id,
 ark,rotation,carea,par,line,word,line_id,x_wconf,text,
 st_setsrid(st_makebox2d(st_makepoint(b[2]::integer,-b[3]::integer),st_makepoint(b[4]::integer,-b[5]::integer)),32662) as bbox
 from b
 where text is not null;

-- create index words_ark on words(ark);
-- create index words_line_id on words(line_id);
