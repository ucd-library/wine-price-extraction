create type whisker as (
  min numeric,
  min_outliers numeric[],
  qn numeric,
  q1 numeric,
  median numeric,
  q3 numeric,
  qx numeric,
  max_outliers numeric[],
  max numeric );

CREATE OR REPLACE FUNCTION array_whisker(numeric[])
    RETURNS whisker AS
  $$
    with
    a as (select unnest($1) as i),
    s as (
       select count(*),min(i),max(i),array_agg(i order by i) as sorted
       from a
    ),
    m as (
      select min,
      sorted[ceiling(count/4.0)] as q1,
      sorted[ceiling(count/2.0)] as median,
      sorted[ceiling(count*3.0/4.0)] as q3,
      max
      from s),
    n as (
      select array_agg(i order by i) as min_outliers from a,m where i<(median-(q3-q1)*1.5)
    ),
    x as (
      select array_agg(i order by i ) as max_outliers from a,m where i>(median+(q3-q1)*1.5)
    )
    select
    ( min,
      coalesce(min_outliers,ARRAY[]::numeric[]),
      median-(q3-q1)*1.5,
      q1,median,q3,
      median+(q3-q1)*1.5,
      coalesce(max_outliers,ARRAY[]::numeric[]),
      max)::whisker from m,n,x;
$$
LANGUAGE 'sql' IMMUTABLE;


CREATE AGGREGATE whisker(numeric) (
  SFUNC=array_append,
  STYPE=numeric[],
  FINALFUNC=array_whisker
);
