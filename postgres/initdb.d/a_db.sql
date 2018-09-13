CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA public;
COMMENT ON EXTENSION postgis IS 'Postgis Geographic Package';

--DROP SCHEMA catalogs cascade;
CREATE SCHEMA catalogs;
SET search_path = catalogs,public,pg_catalog;
alter database :DBNAME set search_path to catalogs,public,pg_catalog;

CREATE TABLE languages (
    id bigint NOT NULL,
    language text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);

CREATE TABLE wine_type (
 wine_type text primary key
);
insert into wine_type
values ('Still'),('Sparkling'),('Fortified');

CREATE TABLE wine_color (
 color text primary key
);
insert into wine_color
values ('Red'),('White'),('Rosé');

CREATE TABLE bottle_info (
    bottle_type text primary key,
    volume double precision,
    ratio double precision,
    notes text,
    champagne boolean,
    bordeaux boolean,
    burgandy boolean
);

insert into bottle_info (bottle_type,volume,ratio,notes,champagne,bordeaux,burgandy)
select * from
(VALUES
('Piccolo',0.1875,0.25,'"Small" in Italian. Also known as a quarter bottle, pony, snipe or split.',true,false,false),
('Split',0.1875,0.25,null,true,false,false),
('Quarter',0.2,0.2667,'Used for Champagne',true,false,false),
('Chopine',0.25,0.33,'Traditional French unit of volume',false,true,false),
('Demi',0.375,0.5,'"Half" in French. Also known as a half bottle.',true,true,true),
('Half Bottle',0.375,0.5,'"One half standard bottle.',false,false,false),
('Tenth',0.378,0.505,'One tenth of a US gallon*',false,false,false),
('Jennie',0.5,0.67,'Also known as a 50 cl bottle. Used for Tokaj, Sauternes, Jerez, as well as several other types of sweet wines, also common for cheaper wines in Switzerland.',true,false,false),
('Demie',0.5,0.67,'',true,false,false),
('Pinte',0.5,0.67,'',true,false,false),
('Clavelin',0.62,0.83,'Primarily used for vin jaune.',false,false,false),
('Standard',0.75,1,'',true,true,true),
('Fifth',0.757,1.01,'One-fifth of a US gallon* (before 1979)',false,false,false),
('Litre',1,1.33,'Popular size for Austrian wines.',false,false,false),
('Magnum',1.5,2,'',true,true,true),
('Marie Jeanne',2.25,3,'Also known as a Tregnum or Tappit Hen in the port wine trade.',false,true,false),
('Jeroboam',3,4,'Biblical, First king of Northern Kingdom. ''Jeroboam'' has different meanings (that is, indicates different sizes) for different regions in France.',true,true,true),
('Rehoboam',4.5,6,'Biblical, First king of separate Judea',true,false,true),
('McKenzie',5,6.67,'Uncommon, primarily found in France',false,true,false),
('Imperial',6,8,'',false,true,false),
('Methuselah',6,8,'Biblical, Oldest Man',true,false,true),
('Salmanazar',9,12,'Biblical, Assyrian King',true,true,true),
('Balthazar',12,16,'One of three Wise Men (according to legend) to present gifts at Jesus'' nativity; Belshazzar can also denote the co-regent of Babylon during the madness of Nebuchadnezzar, for whom the next-larger bottle size is named.',true,true,true),
('Nebuchadnezzar',15,20,'Biblical, King of Babylon',true,true,true),
('Melchior',18,24,'One of three Wise Men (according to legend) to present gifts at Jesus'' nativity',true,true,true),
('Solomon',18,24,'Biblical, King of Israel, Son of David',true,false,false),
('Sovereign',26.25,35,'Reportedly created by Taittinger in 1988 for the launch of the then world''s largest cruise liner Sovereign of the Seas[10]',true,false,false),
('Primat',27,36,'Biblical, stoned by David',true,true,false),
('Goliath',27,36,null,true,true,false),
('Melchizedek',30,40,'Biblical, King of Salem',true,false,false),
('Midas',30,40,'Biblical, King of Salem',true,false,false)
) as b(bottle_type,volume,ratio,notes,champagne,bordeaux,burgandy);
