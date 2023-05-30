


-------------------------------------------------------------------
-- This file provides SQL and PL/SQL examples of many Oracle Spatial functions
-- and procedures. 
-- Many of these examples are used in the Spatial manual (Oracle 
-- Spatial User's Guide and Reference).
--
-- The data and tasks in these examples are selected primarily for their 
-- tutorial value, and are not intended as "realistic" 
-- in the context of a production application. 
-------------------------------------------------------------------

-- It will create a user called scott and runs all the
-- examples as that user.

spool spatial_examples.log;
set serveroutput on

SET FEEDBACK 1
SET NUMWIDTH 10
SET LINESIZE 80
SET TRIMSPOOL ON
SET TAB OFF
SET PAGESIZE 100
SET ECHO ON
-------------------------------------------------------------------
-- INITIAL SET-UP
-------------------------------------------------------------------
-- Grant privileges --
-- function-based index and member function returning SDO_GEOMETRY.
GRANT query rewrite to scott;

---------------------------------------------------------------------------
-- CREATE AND POPULATE SPATIAL TABLE --
---------------------------------------------------------------------------
CONNECT scott/tiger

-- Create a table for cola (soft drink) markets in a
-- given geography (such as city or state).
-- Each row will be an area of interest for a specific
-- cola (for example, where the cola is most preferred
-- by residents, where the manufacturer believes the
-- cola has growth potential, etc.

CREATE TABLE cola_markets (
  mkt_id NUMBER PRIMARY KEY,
  name VARCHAR2(32),
  shape MDSYS.SDO_GEOMETRY);

-- Note re. areas of interest: cola_a (rectangle) and
-- cola_b (4-sided polygon) are side by side (share 1 border).
-- cola_c is a small 4-sided polygon that overlaps parts of
-- cola_a and cola_b. A rough sketch:
--     ---------+-
--     |    a    |  b  \
--     |     +------+    |   
--     |   /___c _|     |
--     |         |          |
--     ---------+---------|

-- The next INSERT statement creates an area of interest for 
-- Cola A. This area happens to be a rectangle.
-- The area could represent any user-defined criterion: for
-- example, where Cola A is the preferred drink, where
-- Cola A is under competitive pressure, where Cola A
-- has strong growth potential, and so on.
 
INSERT INTO cola_markets VALUES(
  1,
  'cola_a',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3), -- one rectangle (1003 = exterior)
    MDSYS.SDO_ORDINATE_ARRAY(1,1, 5,7) -- only 2 points needed to
          -- define rectangle (lower left and upper right) with
          -- Cartesian-coordinate data
  )
);

-- The next two INSERT statements create areas of interest for 
-- Cola B and Cola C. These areas are simple polygons (but not
-- rectangles).

INSERT INTO cola_markets VALUES(
  2,
  'cola_b',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- one polygon (exterior polygon ring)
    MDSYS.SDO_ORDINATE_ARRAY(5,1, 8,1, 8,6, 5,7, 5,1)
  )
);

INSERT INTO cola_markets VALUES(
  3,
  'cola_c',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- one polygon (exterior polygon ring)
    MDSYS.SDO_ORDINATE_ARRAY(3,3, 6,3, 6,5, 4,5, 3,3)
  )
);

-- Now insert an area of interest for Cola D. This is a
-- circle with a radius of 2. It is completely outside the
-- first three areas of interest.

INSERT INTO cola_markets VALUES(
  4,
  'cola_d',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,4), -- one circle
    MDSYS.SDO_ORDINATE_ARRAY(8,7, 10,9, 8,11)
  )
);

---------------------------------------------------------------------------
-- UPDATE METADATA VIEW --
---------------------------------------------------------------------------
-- Update the USER_SDO_GEOM_METADATA view. This is required
-- before the Spatial index can be created. Do this only once for each
-- layer (i.e., table-column combination; here: cola_markets and shape).
-- The spatial domain for this example is a flat (Cartesian) space 20 by 20
-- units long. This not geodetic data (for example, no longitude/latitude
-- coordinates), and no specific distance unit is used.

INSERT INTO USER_SDO_GEOM_METADATA 
  VALUES (
  'cola_markets',
  'shape',
  MDSYS.SDO_DIM_ARRAY(   -- 20X20 grid
    MDSYS.SDO_DIM_ELEMENT('X', 0, 20, 0.005),
    MDSYS.SDO_DIM_ELEMENT('Y', 0, 20, 0.005)
     ),
  NULL   -- SRID
);

-------------------------------------------------------------------
-- CREATE THE SPATIAL INDEX --
-------------------------------------------------------------------
-- Create an R-tree index.
CREATE INDEX cola_spatial_idx
ON cola_markets(shape)
INDEXTYPE IS MDSYS.SPATIAL_INDEX;

-------------------------------------------------------------------
-- PERFORM SOME SPATIAL QUERIES --
-------------------------------------------------------------------
DECLARE
  cola_a_geom MDSYS.SDO_GEOMETRY;
  cola_b_geom MDSYS.SDO_GEOMETRY;
  cola_c_geom MDSYS.SDO_GEOMETRY;
  cola_d_geom MDSYS.SDO_GEOMETRY;
  cola_diminfo MDSYS.SDO_DIM_ARRAY;
  returned_geom MDSYS.SDO_GEOMETRY;
  returned_number NUMBER;
  returned_integer INTEGER;
  returned_varchar VARCHAR2(32);

BEGIN

-- Populate geometry variables with cola market shapes.
SELECT c.shape into cola_a_geom FROM cola_markets c
  WHERE c.name = 'cola_a';
SELECT c.shape into cola_b_geom FROM cola_markets c
  WHERE c.name = 'cola_b';
SELECT c.shape into cola_c_geom FROM cola_markets c
  WHERE c.name = 'cola_c';
SELECT c.shape into cola_d_geom FROM cola_markets c
  WHERE c.name = 'cola_d';

-- Populate the dimension information variable.
SELECT m.diminfo into cola_diminfo from 
  user_sdo_geom_metadata m
  WHERE m.table_name = 'COLA_MARKETS';

-- SDO_DIFFERENCE/INTERSECTION/UNION/XOR

-- Return the topological difference of two geometries.
returned_geom := SDO_GEOM.SDO_DIFFERENCE(
  cola_a_geom, cola_diminfo, cola_c_geom, cola_diminfo);

-- Return the topological intersection of two geometries.
returned_geom := SDO_GEOM.SDO_INTERSECTION(
  cola_a_geom, cola_diminfo, cola_c_geom, cola_diminfo);

-- Return the topological union of two geometries.
returned_geom := SDO_GEOM.SDO_UNION(
  cola_a_geom, cola_diminfo, cola_c_geom, cola_diminfo);

-- Return the topological symmetric difference of two geometries.
returned_geom := SDO_GEOM.SDO_XOR(
  cola_a_geom, cola_diminfo, cola_c_geom, cola_diminfo);

-- Other functions

-- Return the area of a geometry, then display it.
returned_number := SDO_GEOM.SDO_AREA(
  cola_a_geom, cola_diminfo);
DBMS_OUTPUT.PUT_LINE('Area of cola_a = ' || returned_number);

-- Do two geometries have any spatial relationship?
returned_varchar := SDO_GEOM.RELATE(
  cola_b_geom, cola_diminfo, 'anyinteract', cola_d_geom, cola_diminfo);
DBMS_OUTPUT.PUT_LINE('Any interaction between cola_b and 
  cola_d? = ' || returned_varchar);

-- Return the distance between two geometries, then display it.
returned_number := SDO_GEOM.SDO_DISTANCE(
  cola_b_geom, cola_diminfo, cola_d_geom, cola_diminfo);
DBMS_OUTPUT.PUT_LINE('Shortest distance between cola_b 
  and cola_d = ' || returned_number);

-- Return the perimeter of a geometry, then display it.
returned_number := SDO_GEOM.SDO_LENGTH(
  cola_a_geom, cola_diminfo);
DBMS_OUTPUT.PUT_LINE('Perimeter of cola_a = ' || returned_number);

-- Is a geometry valid?
returned_varchar := SDO_GEOM.VALIDATE_GEOMETRY(
  cola_c_geom, cola_diminfo);
DBMS_OUTPUT.PUT_LINE('Validity of cola_c geometry?  = ' || returned_varchar);

-- Are two geometries within 1 unit of distance apart?
returned_varchar := SDO_GEOM.WITHIN_DISTANCE(
  cola_b_geom, cola_diminfo, 1, cola_d_geom, cola_diminfo);
DBMS_OUTPUT.PUT_LINE('Are cola_b and cola_d within 1 unit 
  apart? = ' || returned_varchar);

END;
/


-- Try several geometry functions.

-- Return the topological difference of two geometries.
SELECT SDO_GEOM.SDO_DIFFERENCE(c_a.shape, m.diminfo, c_c.shape, m.diminfo) 
  FROM cola_markets c_a, cola_markets c_c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c_a.name = 'cola_a' AND c_c.name = 'cola_c';

-- Return the topological intersection of two geometries.
SELECT SDO_GEOM.SDO_INTERSECTION(c_a.shape, c_c.shape, 0.005)
   FROM cola_markets c_a, cola_markets c_c 
   WHERE c_a.name = 'cola_a' AND c_c.name = 'cola_c';

-- Return the topological union of two geometries.
SELECT SDO_GEOM.SDO_UNION(c_a.shape, m.diminfo, c_c.shape, m.diminfo) 
  FROM cola_markets c_a, cola_markets c_c, user_sdo_geom_metadata m
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c_a.name = 'cola_a' AND c_c.name = 'cola_c';

-- Return the topological symmetric difference of two geometries.
SELECT SDO_GEOM.SDO_XOR(c_a.shape, m.diminfo, c_c.shape, m.diminfo) 
  FROM cola_markets c_a, cola_markets c_c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c_a.name = 'cola_a' AND c_c.name = 'cola_c';

-- Do two geometries have any spatial relationship?
SELECT SDO_GEOM.RELATE(c_b.shape, 'anyinteract', c_d.shape, 0.005)
  FROM cola_markets c_b, cola_markets c_d
  WHERE c_b.name = 'cola_b' AND c_d.name = 'cola_d';

-- Generate a buffer of 1 unit around a geometry.
SELECT c.name, SDO_GEOM.SDO_BUFFER(c.shape, m.diminfo, 1) 
 FROM cola_markets c, user_sdo_geom_metadata m WHERE m.table_name = 'COLA_MARKETS' 
 AND m.column_name = 'SHAPE' AND c.name = 'cola_a';

-- Return the areas of all cola markets
SELECT name, SDO_GEOM.SDO_AREA(shape, 0.005) FROM cola_markets;

-- Return the area of just cola_a
SELECT c.name, SDO_GEOM.SDO_AREA(c.shape, 0.005) FROM cola_markets c 
   WHERE c.name = 'cola_a';

-- Return the centroid of a geometry.
SELECT c.name, SDO_GEOM.SDO_CENTROID(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_c';

-- Return the convex hull of a polygon.
SELECT c.name, SDO_GEOM.SDO_CONVEXHULL(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_c';

-- Return the convex hull of a circle. (Will return a square.)
SELECT c.name, SDO_GEOM.SDO_CONVEXHULL(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_d';

-- Return the distance between two geometries
SELECT SDO_GEOM.SDO_DISTANCE(c_b.shape, c_d.shape, 0.005)
   FROM cola_markets c_b, cola_markets c_d
   WHERE c_b.name = 'cola_b' AND c_d.name = 'cola_d';

-- Return the perimeters of all cola markets
SELECT c.name, SDO_GEOM.SDO_LENGTH(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE';

-- Return the perimeter of just cola_a
SELECT c.name, SDO_GEOM.SDO_LENGTH(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_a';

-- Return a point guaranteed to be on the surface of a geometry
SELECT SDO_GEOM.SDO_POINTONSURFACE(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_a';

-- Is a geometry valid?
SELECT c.name, SDO_GEOM.VALIDATE_GEOMETRY(c.shape, 0.005)
   FROM cola_markets c WHERE c.name = 'cola_c';

-- Is a layer valid?
CREATE TABLE val_results (mkt_id number, result varchar2(10));
EXECUTE SDO_GEOM.VALIDATE_LAYER('COLA_MARKETS', 'SHAPE', 'MKT_ID',  'VAL_RESULTS');
SELECT * from val_results;

-- Is a layer valid? Test with commit_interval < number of rows.
DROP TABLE val_results;
CREATE TABLE val_results (mkt_id number, result varchar2(10));
EXECUTE SDO_GEOM.VALIDATE_LAYER('COLA_MARKETS', 'SHAPE', 'MKT_ID',  'VAL_RESULTS', 2);
SELECT * from val_results;

-- Is a layer valid? Test with commit_interval > number of rows.
DROP TABLE val_results;
CREATE TABLE val_results (mkt_id number, result varchar2(10));
EXECUTE SDO_GEOM.VALIDATE_LAYER('COLA_MARKETS', 'SHAPE', 'MKT_ID',  'VAL_RESULTS', 10);
SELECT * from val_results;

-- Are two geometries within 1 unit of distance apart?
SELECT SDO_GEOM.WITHIN_DISTANCE(c_b.shape, m.diminfo, 1,
     c_d.shape, m.diminfo) 
  FROM cola_markets c_b, cola_markets c_d, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c_b.name = 'cola_b' AND c_d.name = 'cola_d';

-- Arc densification of the circle cola_d
SELECT c.name, SDO_GEOM.SDO_ARC_DENSIFY(c.shape, m.diminfo, 'arc_tolerance=0.05') 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_d';

-- Tuning functions and procedures

DECLARE
  table_name      VARCHAR2(32) := 'COLA_MARKETS';
  column_name  VARCHAR2(32) := 'SHAPE';
  width              NUMBER;
  height            NUMBER;
BEGIN
SDO_TUNE.AVERAGE_MBR(
  table_name,
  column_name,
  width,
  height);
DBMS_OUTPUT.PUT_LINE('Width = ' || width);
DBMS_OUTPUT.PUT_LINE('Height = ' || height);
END;
/

DECLARE
  table_name      VARCHAR2(32) := 'COLA_MARKETS';
  column_name  VARCHAR2(32) := 'SHAPE';
  sample_ratio  INTEGER := 2;  -- Should be larger, but only 4 table rows
  tiling_level  INTEGER := 4;
  num_tiles  INTEGER := 10; 
  window_obj MDSYS.SDO_GEOMETRY := 
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- one polygon
    MDSYS.SDO_ORDINATE_ARRAY(3,3, 6,3, 6,5, 4,5, 3,3)
  );
  tiling_time NUMBER; 
  filter_time   NUMBER;
  query_time  NUMBER;
  ret_number NUMBER;
BEGIN
ret_number := SDO_TUNE.ESTIMATE_INDEX_PERFORMANCE(
  table_name,
  column_name,
  sample_ratio,
  tiling_level,
  num_tiles,
  window_obj, 
  tiling_time, 
  filter_time, 
  query_time
);
DBMS_OUTPUT.PUT_LINE('Tiling time = ' || tiling_time);
DBMS_OUTPUT.PUT_LINE('Filter time = ' || filter_time);
DBMS_OUTPUT.PUT_LINE('Query time = ' || query_time);
DBMS_OUTPUT.PUT_LINE('Result = ' || ret_number);
END;
/

DECLARE
  table_name      VARCHAR2(32) := 'COLA_MARKETS';
  column_name  VARCHAR2(32) := 'SHAPE';
  maxtiles          NUMBER := 10000;
  type_of_estimate  VARCHAR2(32) := 'ALL_GID_EXTENT';
  ret_integer INTEGER;
BEGIN
ret_integer := SDO_TUNE.ESTIMATE_TILING_LEVEL(
  table_name,
  column_name,
  maxtiles,
  type_of_estimate
);
DBMS_OUTPUT.PUT_LINE('Result = ' || ret_integer);
END;
/

SELECT SDO_TUNE.ESTIMATE_TILING_LEVEL('COLA_MARKETS',  'SHAPE',
            10000, 'ALL_GID_EXTENT') 
  FROM DUAL;

DECLARE
  table_name      VARCHAR2(32) := 'COLA_MARKETS';
  column_name  VARCHAR2(32) := 'SHAPE';
  sample_ratio  INTEGER := 2;  -- Should be larger, but only 4 table rows
  tiling_level         INTEGER := 6;
  num_tiles         INTEGER := 10;
  ret_number      NUMBER;
BEGIN
ret_number := SDO_TUNE.ESTIMATE_TILING_TIME(
  table_name,
  column_name,
  sample_ratio,
  tiling_level,
  num_tiles
);
DBMS_OUTPUT.PUT_LINE('Result = ' || ret_number);
END;
/

DECLARE
  table_name      VARCHAR2(32) := 'COLA_MARKETS';
  column_name  VARCHAR2(32) := 'SHAPE';
  sample_ratio  INTEGER := 2;  -- Should be larger, but only 4 table rows
  tiling_level  INTEGER := 4;
  num_tiles  INTEGER := 10;
  num_largetiles  INTEGER;
  ret_integer INTEGER;
BEGIN
ret_integer := SDO_TUNE.ESTIMATE_TOTAL_NUMTILES(
  table_name,
  column_name,
  sample_ratio,
  tiling_level,
  num_tiles,
  num_largetiles
);
DBMS_OUTPUT.PUT_LINE('Num_largetiles = ' || num_largetiles);
DBMS_OUTPUT.PUT_LINE('Result = ' || ret_integer);
END;
/

DECLARE
  table_name      VARCHAR2(32) := 'COLA_MARKETS';
  column_name  VARCHAR2(32) := 'SHAPE';
  ret_geometry MDSYS.SDO_GEOMETRY;
BEGIN
ret_geometry := SDO_TUNE.EXTENT_OF(
  table_name,
  column_name
);
END;
/

SELECT SDO_TUNE.EXTENT_OF('COLA_MARKETS',  'SHAPE') 
  FROM DUAL;

EXECUTE SDO_TUNE.MIX_INFO('COLA_MARKETS',  'SHAPE');

-- Test SDO_NN operator.

SELECT  /*+ INDEX(cola_markets cola_spatial_idx) */ c.mkt_id, c.name  
   FROM cola_markets c  
   WHERE SDO_NN(c.shape,  mdsys.sdo_geometry(2001, NULL, 
      mdsys.sdo_point_type(10,7,NULL), NULL,  NULL),  'sdo_num_res=2') = 'TRUE'; 

-- Test SDO_NN_DISTANCE ancillary operator
SELECT   /*+ INDEX(cola_markets cola_spatial_idx) */ 
   c.mkt_id, c.name, mdsys.SDO_NN_DISTANCE(1) dist
   FROM cola_markets c  
   WHERE SDO_NN(c.shape,  mdsys.sdo_geometry(2001, NULL, 
      mdsys.sdo_point_type(10,7,NULL), NULL,  NULL),
      'sdo_num_res=2', 1) = 'TRUE' ORDER BY dist; 

-- Test the sdo_batch_size parameter for SDO_NN operator.
-- Find the two objects (ROWNUM <=2), with a NAME value less then ’cola_d’, from
-- the SHAPE column in the COLA_MARKETS table that are nearest to point (10,7).
SELECT /*+ INDEX(cola_markets cola_spatial_idx) */ c.mkt_id, c.name
   FROM cola_markets c  
   WHERE SDO_NN(c.shape,  mdsys.sdo_geometry(2001, NULL,
      mdsys.sdo_point_type(10,7,NULL), NULL,  NULL),
      'sdo_batch_size=3') = 'TRUE'
   AND c.name < 'cola_d' AND ROWNUM <= 2; 

---------------------------------------------------------------------
-- Some new interfaces for release 9i
---------------------------------------------------------------------

-- MBR (minimum bounding rectangle)
SELECT SDO_GEOM.SDO_MBR(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_d';

-- Max/min MBR ordinate

SELECT SDO_GEOM.SDO_MAX_MBR_ORDINATE(c.shape, m.diminfo, 1) 
  FROM cola_markets c, user_sdo_geom_metadata m
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_d';

SELECT SDO_GEOM.SDO_MIN_MBR_ORDINATE(c.shape, m.diminfo, 1) 
  FROM cola_markets c, user_sdo_geom_metadata m
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_d';

-- SDO_GEOMETRY type methods (member functions)

SELECT c.mkt_id, c.shape.GET_GTYPE() FROM cola_markets c;

SELECT c.mkt_id, c.shape.GET_DIMS() 
  FROM cola_markets c WHERE c.name = 'cola_d';

-- Spatial aggregate functions

-- Minimum bounding rectangle of all cola_markets geometries.
SELECT SDO_AGGR_MBR(shape) FROM cola_markets;

-- Aggregate union of first 3 cola_markets geometries (all except cola_d).
SELECT SDO_AGGR_UNION(
  MDSYS.SDOAGGRTYPE(c.shape, 0.005))
  FROM cola_markets c
  WHERE c.name < 'cola_d';

-- Aggregate centroid of all 4 cola_markets geometries
SELECT SDO_AGGR_CENTROID(MDSYS.SDOAGGRTYPE(shape, 0.005))
  FROM cola_markets;

-- Aggregate convex hull of all 4 cola_markets geometries
SELECT SDO_AGGR_CONVEXHULL(MDSYS.SDOAGGRTYPE(shape, 0.005))
  FROM cola_markets;


-- SDO_GEOMETRY in user-defined type: create type, create table,
-- insert data, create index, perform queries

CREATE OR REPLACE TYPE market_type AS OBJECT 
  (name VARCHAR2(32), shape MDSYS.SDO_GEOMETRY);
/

CREATE TABLE cola_markets_2 (
  mkt_id NUMBER PRIMARY KEY,
  market MARKET_TYPE);

INSERT INTO cola_markets_2 VALUES(
  1,
  MARKET_TYPE('cola_a',
    MDSYS.SDO_GEOMETRY(
      2003,  -- 2-dimensional polygon
      NULL,
      NULL,
      MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3), -- one rectangle (1003 = exterior)
      MDSYS.SDO_ORDINATE_ARRAY(1,1, 5,7) -- only 2 points needed to
            -- define rectangle (lower left and upper right)
      )
  )
);

-- The next two INSERT statements create areas of interest for 
-- Cola B and Cola C. These areas are simple polygons (but not
-- rectangles).

INSERT INTO cola_markets_2 VALUES(
  2,
  MARKET_TYPE( 'cola_b',
    MDSYS.SDO_GEOMETRY(
      2003,  -- 2-dimensional polygon
      NULL,
      NULL,
      MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- one polygon (exterior polygon ring)
      MDSYS.SDO_ORDINATE_ARRAY(5,1, 8,1, 8,6, 5,7, 5,1)
      )
  )
);

INSERT INTO cola_markets_2 VALUES(
  3,
  MARKET_TYPE('cola_c',
    MDSYS.SDO_GEOMETRY(
      2003,  -- 2-dimensional polygon
      NULL,
      NULL,
      MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- one polygon (exterior polygon ring)
      MDSYS.SDO_ORDINATE_ARRAY(3,3, 6,3, 6,5, 4,5, 3,3)
      )
  )
);

-- Now insert an area of interest for Cola D. This is a
-- circle with a radius of 2. It is completely outside the
-- first three areas of interest.

INSERT INTO cola_markets_2 VALUES(
  4,
  MARKET_TYPE('cola_d',
    MDSYS.SDO_GEOMETRY(
      2003,  -- 2-dimensional polygon
      NULL,
      NULL,
      MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,4), -- one circle
      MDSYS.SDO_ORDINATE_ARRAY(8,7, 10,9, 8,11)
      )
  )
);

-- Update metadata view
-- Update the USER_SDO_GEOM_METADATA view. This is required
-- before the Spatial index can be created. Do this only once for each
-- layer (i.e., table-column combination; here: cola_markets_2 and market.shape).

INSERT INTO USER_SDO_GEOM_METADATA 
  VALUES (
  'cola_markets_2',
  'market.shape',
  MDSYS.SDO_DIM_ARRAY(   -- 20X20 grid
    MDSYS.SDO_DIM_ELEMENT('X', 0, 20, 0.005),
    MDSYS.SDO_DIM_ELEMENT('Y', 0, 20, 0.005)
     ),
  NULL   -- SRID
);

-- Create the index.
CREATE INDEX cola_spatial_idx_2
ON cola_markets_2(market.shape)
INDEXTYPE IS MDSYS.SPATIAL_INDEX;

-- Perform a simple query.
SELECT c.mkt_id, c.market.name, c.market.shape 
  FROM cola_markets_2 c
  WHERE c.market.name = 'cola_a';

-- Another query: What geometry has any interaction with a query window?

SELECT c.mkt_id, c.market.name
  FROM cola_markets_2 c
  WHERE SDO_RELATE(c.market.shape,
            MDSYS.SDO_GEOMETRY(2003, NULL, NULL,
              MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3),
              MDSYS.SDO_ORDINATE_ARRAY(4,6, 8,8)),
            'mask=anyinteract querytype=window') = 'TRUE';

-- Test function-based index.
--
-- Next two ALTER statements required to use certain features:
-- function-based index and member function returning SDO_GEOMETRY.
ALTER SESSION SET QUERY_REWRITE_INTEGRITY = TRUSTED;
ALTER SESSION SET QUERY_REWRITE_ENABLED = TRUE;

-- Create a function to return a point geometry (SDO_GTYPE = 2001) with
-- input of 2 numbers: longitude and latitude (SDO_SRID = 8307, for
-- "Longitude / Latitude (WGS 84)",  probably the most widely used 
--  coordinate system, and the one used for GPS devices.
-- Specify DETERMINISTIC for the function.

create or replace function get_long_lat_pt(longitude in number, 
                                           latitude in number)
return MDSYS.SDO_GEOMETRY deterministic is
begin
     return mdsys.sdo_geometry(2001, 8307, 
                mdsys.sdo_point_type(longitude, latitude, NULL),NULL, NULL);
end;
/

create table LONG_LAT_TABLE 
(longitude number, latitude number, name varchar2(32));

insert into LONG_LAT_TABLE values (10,10, 'Place1');
insert into LONG_LAT_TABLE values (20,20, 'Place2');
insert into LONG_LAT_TABLE values (30,30, 'Place3');

-- Set up the metadata entry for this table.
-- note that the column name sets up the function on top
-- of the two columns used in this function
-- along with the owner of the function
insert into user_sdo_geom_metadata values('LONG_LAT_TABLE',
 'scott.get_long_lat_pt(longitude,latitude)',
 mdsys.sdo_dim_array(
   mdsys.sdo_dim_element('Longitude', -180, 180, 0.005),
   mdsys.sdo_dim_element('Latitude', -90, 90, 0.005)), 8307);

-- Create the index using the function
create index LONG_LAT_TABLE_IDX on 
   LONG_LAT_TABLE(get_long_lat_pt(longitude,latitude))
   indextype is mdsys.spatial_index;

-- test insert
insert into LONG_LAT_TABLE values (40,40, 'Place4');

-- test delete
delete from LONG_LAT_TABLE where NAME = 'Place3';

-- The next two queries accomplish the same thing. The first query
-- uses a constructor to specify the point, whereas the second query
-- uses the function that was defined.

-- First query: call sdo_filter with an SDO_GEOMETRY constructor
select name from LONG_LAT_TABLE  a
   where sdo_filter(get_long_lat_pt(a.longitude,a.latitude), 
      mdsys.sdo_geometry(2001, 8307, 
        mdsys.sdo_point_type(10,10,NULL), NULL, NULL), 
      'querytype=WINDOW')='TRUE';

-- Second query: call sdo_filter with the function that returns an sdo_geometry
select name from LONG_LAT_TABLE  a
   where sdo_filter(get_long_lat_pt(a.longitude,a.latitude), 
     get_long_lat_pt(10,10),
     'querytype=WINDOW')='TRUE';

-- User-defined type with member function

create type long_lat as object ( 
   longitude number, 
   latitude number, 
member function GetGeometry(SELF in long_lat) 
RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC) 
/ 

create or replace type body long_lat as 
  member function GetGeometry(self in long_lat) 
  return MDSYS.SDO_GEOMETRY is 
    begin 
       return mdsys.sdo_geometry(2001, 8307, 
           mdsys.sdo_point_type(longitude, latitude, NULL), NULL,NULL); 
    end; 
end; 
/ 
  
create table test_long_lat 
   (location long_lat, name varchar2(32)); 

insert into test_long_lat values (long_lat(10,10), 'Place1'); 
insert into test_long_lat values (long_lat(20,20), 'Place2'); 
insert into test_long_lat values (long_lat(30,30), 'Place3'); 

insert into user_sdo_geom_metadata values('test_long_lat', 
 'scott.long_lat.GetGeometry(location)', 
 mdsys.sdo_dim_array(
   mdsys.sdo_dim_element('Longitude', -180, 180, 0.005),
   mdsys.sdo_dim_element('Latitude', -90, 90, 0.005)), 8307);

create index test_long_lat_idx on test_long_lat(location.GetGeometry()) 
  indextype is mdsys.spatial_index; 

-- Geometries likely to interact with (10,10)?
SELECT a.name FROM test_long_lat a
  WHERE SDO_FILTER(long_lat.GetGeometry(a.location),
            MDSYS.SDO_GEOMETRY(2001, 8307,
                MDSYS.SDO_POINT_TYPE(10,10,NULL), NULL, NULL),
            'querytype=window') = 'TRUE';

-- R-tree index analysis and tuning
-- First, add some geometries, and maybe delete.
INSERT INTO cola_markets VALUES(
  5,
  'cola_e',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3), -- one rectangle (1003 = exterior)
    MDSYS.SDO_ORDINATE_ARRAY(12,1, 14,5) -- only 2 points needed to
          -- define rectangle (lower left and upper right)
  )
);

INSERT INTO cola_markets VALUES(
  6,
  'cola_f',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,4), -- one circle
    MDSYS.SDO_ORDINATE_ARRAY(12,11, 14,13, 12,15)
  )
);

-- Insert polygon withOUT hole first.
INSERT INTO cola_markets VALUES(
  9,
  'polygon_without_hole',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- one polygon (exterior polygon ring)
    MDSYS.SDO_ORDINATE_ARRAY(2,4, 4,3, 10,3, 13,5, 13,9, 11,13, 5,13, 2,11, 2,4)
  )
);

-- Insert polygon with hole; test various functions on it.
INSERT INTO cola_markets VALUES(
  10,
  'polygon_with_hole',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1, 19,2003,1), -- polygon with hole
    MDSYS.SDO_ORDINATE_ARRAY(2,4, 4,3, 10,3, 13,5, 13,9, 11,13, 5,13, 2,11, 2,4,
        7,5, 7,10, 10,10, 10,5, 7,5)
  )
);

-- Generate a buffer of 1 unit around a geometry.
SELECT c.name, SDO_GEOM.SDO_BUFFER(c.shape, m.diminfo, 1) 
 FROM cola_markets c, user_sdo_geom_metadata m WHERE m.table_name = 'COLA_MARKETS' 
 AND m.column_name = 'SHAPE' AND c.name = 'polygon_without_hole';

-- Generate a buffer of 1 unit around a geometry.
SELECT c.name, SDO_GEOM.SDO_BUFFER(c.shape, m.diminfo, 1) 
 FROM cola_markets c, user_sdo_geom_metadata m WHERE m.table_name = 'COLA_MARKETS' 
 AND m.column_name = 'SHAPE' AND c.name = 'polygon_with_hole';

-- Return the area of just polygon_without_hole
SELECT c.name, SDO_GEOM.SDO_AREA(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'polygon_without_hole';

-- Return the area of just polygon_with_hole
SELECT c.name, SDO_GEOM.SDO_AREA(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'polygon_with_hole';

-- Return the centroid of a geometry.
SELECT c.name, SDO_GEOM.SDO_CENTROID(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'polygon_without_hole';

SELECT c.name, SDO_GEOM.SDO_CENTROID(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'polygon_with_hole';

-- Return the convex hull of a polygon.
SELECT c.name, SDO_GEOM.SDO_CONVEXHULL(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'polygon_with_hole';

-- Return the perimeter of just polygon_with_hole and without_hole
SELECT c.name, SDO_GEOM.SDO_LENGTH(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'polygon_without_hole';

SELECT c.name, SDO_GEOM.SDO_LENGTH(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'polygon_with_hole';

-- Return a point guaranteed to be on the surface of a geometry
SELECT SDO_GEOM.SDO_POINTONSURFACE(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'polygon_with_hole';

-- Insert compound line string
INSERT INTO cola_markets VALUES(
  11,
  'compound_line_string',
  MDSYS.SDO_GEOMETRY(
    2002,  -- line or curve (or combination)
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,4,2, 1,2,1, 3,2,2), -- compound line string
    MDSYS.SDO_ORDINATE_ARRAY(10,10, 10,14, 6,10, 14,10)
  )
);

-- Is the geometry valid?
SELECT c.name, SDO_GEOM.VALIDATE_GEOMETRY(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'compound_line_string';

-- Insert compound polygon
INSERT INTO cola_markets VALUES(
  12,
  'compound_polygon',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1005,2, 1,2,1, 5,2,2), -- compound polygon
    MDSYS.SDO_ORDINATE_ARRAY(6,10, 10,1, 14,10, 10,14, 6,10)
  )
);

-- Is the geometry valid?
SELECT c.name, SDO_GEOM.VALIDATE_GEOMETRY(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name = 'compound_polygon';

-- Insert geometry with type zero element
INSERT INTO cola_markets VALUES(
  13,
  'type_zero_element_geom',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,0,57, 11,1003,3), -- 1st is type 0 element
    MDSYS.SDO_ORDINATE_ARRAY(6,6, 12,6, 9,8, 6,10, 12,10, 6,4, 12,12)
  )
);

-- Is the geometry valid?
SELECT c.name, SDO_GEOM.VALIDATE_GEOMETRY(c.shape, m.diminfo) 
  FROM cola_markets c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS' AND m.column_name = 'SHAPE' 
  AND c.name =  'type_zero_element_geom';

SELECT * from cola_markets c WHERE c.name = 'compound_line_string';

SELECT * from cola_markets c WHERE c.name = 'compound_polygon';

SELECT * from cola_markets c WHERE c.name = 'type_zero_element_geom';

DELETE FROM cola_markets c WHERE c.name = 'cola_a';



-------------------------------------------------------------------------------
-- LINEAR REFERENCING SYSTEM (LRS) Examples
-------------------------------------------------------------------------------

---------------------------------------------------------------------------
-- CREATE AND POPULATE TABLE --
---------------------------------------------------------------------------

-- Create a table for routes (highways).
CREATE TABLE lrs_routes (
  route_id  NUMBER PRIMARY KEY,
  route_name  VARCHAR2(32),
  route_geometry  MDSYS.SDO_GEOMETRY);

-- Populate table with just one route for this example.
INSERT INTO lrs_routes VALUES(
  1,
  'Route1',
  MDSYS.SDO_GEOMETRY(
    3302,  -- line string, 3 dimensions (X,Y,M), 3rd is linear referencing dimension
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1), -- one line string, straight segments
    MDSYS.SDO_ORDINATE_ARRAY(
      2,2,0,   -- Starting point - Exit1; 0 is measure from start.
      2,4,2,   -- Exit2; 2 is measure from start. 
      8,4,8,   -- Exit3; 8 is measure from start. 
      12,4,12,  -- Exit4; 12 is measure from start. 
      12,10,NULL,  -- Not an exit; measure will be automatically calculated and filled.
      8,10,22,  -- Exit5; 22 is measure from start.  
      5,14,27)  -- Ending point (Exit6); 27 is measure from start.
  )
);

---------------------------------------------------------------------------
-- UPDATE METADATA VIEW --
---------------------------------------------------------------------------
-- Update the USER_SDO_GEOM_METADATA view. This is required
-- before the Spatial index can be created. Do this only once for each
-- layer (i.e., table-column combination; here: cola_markets and shape).
INSERT INTO USER_SDO_GEOM_METADATA 
  VALUES (
  'lrs_routes',
  'route_geometry',
  MDSYS.SDO_DIM_ARRAY(   -- 20X20 grid
    MDSYS.SDO_DIM_ELEMENT('X', 0, 20, 0.005),
    MDSYS.SDO_DIM_ELEMENT('Y', 0, 20, 0.005),
    MDSYS.SDO_DIM_ELEMENT('M', 0, 20, 0.005) -- Measure dimension
     ),
  NULL   -- SRID (spatial reference system, also called coordinate system)
);

-------------------------------------------------------------------
-- CREATE THE SPATIAL INDEX --
-------------------------------------------------------------------
-- Use SDO_INDX_DIMS parameter, to index only X, Y dimensions.
CREATE INDEX lrs_routes_idx ON lrs_routes(route_geometry)
  INDEXTYPE IS MDSYS.SPATIAL_INDEX
  PARAMETERS('SDO_INDX_DIMS=2');
  
-------------------------------------------------------------------
-- TEST THE LRS PROCEDURES AND FUNCTIONS --
-------------------------------------------------------------------
DECLARE
geom_segment MDSYS.SDO_GEOMETRY;
line_string MDSYS.SDO_GEOMETRY;
dim_array MDSYS.SDO_DIM_ARRAY;
result_geom_1 MDSYS.SDO_GEOMETRY;
result_geom_2 MDSYS.SDO_GEOMETRY;
result_geom_3 MDSYS.SDO_GEOMETRY;

BEGIN

SELECT a.route_geometry into geom_segment FROM lrs_routes a
  WHERE a.route_name = 'Route1';
SELECT m.diminfo into dim_array from 
  user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY';

-- Define the LRS segment for Route1. This will populate any null measures.
-- No need to specify start and end measures, because they're already defined 
-- in the geometry.
SDO_LRS.DEFINE_GEOM_SEGMENT (geom_segment, dim_array);

SELECT a.route_geometry INTO line_string FROM lrs_routes a 
  WHERE a.route_name = 'Route1';

-- Split Route1 into two segments.
SDO_LRS.SPLIT_GEOM_SEGMENT(line_string,dim_array,5,result_geom_1,result_geom_2);

-- Concatenate the segments that were just split.
result_geom_3 := SDO_LRS.CONCATENATE_GEOM_SEGMENTS(result_geom_1, dim_array, result_geom_2, dim_array);

-- Update and insert geometries into table, to display later.
UPDATE lrs_routes a SET a.route_geometry = geom_segment
   WHERE a.route_id = 1;

INSERT INTO lrs_routes VALUES(
  11,
  'result_geom_1',
  result_geom_1
);
INSERT INTO lrs_routes VALUES(
  12,
  'result_geom_2',
  result_geom_2
);
INSERT INTO lrs_routes VALUES(
  13,
  'result_geom_3',
  result_geom_3
);

END;
/

-- First, display the data in the LRS table.
SELECT route_id, route_name, route_geometry from lrs_routes;

-- Are result_geom_1 and result_geom2 connected? 
SELECT  SDO_LRS.CONNECTED_GEOM_SEGMENTS(a.route_geometry,
           b.route_geometry, 0.005)
  FROM lrs_routes a, lrs_routes b
  WHERE a.route_id = 11 AND b.route_id = 12;

-- Is the Route1 segment valid?
SELECT  SDO_LRS.VALID_GEOM_SEGMENT(route_geometry)
  FROM lrs_routes WHERE route_id = 1;

-- Is 50 a valid measure on Route1? (Should return FALSE; highest Route1 measure is 27.)
SELECT  SDO_LRS.VALID_MEASURE(route_geometry, 50)
  FROM lrs_routes WHERE route_id = 1;

-- Is the Route1 segment defined?
SELECT  SDO_LRS.IS_GEOM_SEGMENT_DEFINED(route_geometry)
  FROM lrs_routes WHERE route_id = 1;

-- How long is Route1?
SELECT  SDO_LRS.GEOM_SEGMENT_LENGTH(route_geometry)
  FROM lrs_routes WHERE route_id = 1;

-- What is the start measure of Route1?
SELECT  SDO_LRS.GEOM_SEGMENT_START_MEASURE(route_geometry)
  FROM lrs_routes WHERE route_id = 1;

-- What is the end measure of Route1?
SELECT  SDO_LRS.GEOM_SEGMENT_END_MEASURE(route_geometry)
  FROM lrs_routes WHERE route_id = 1;

-- What is the start point of Route1?
SELECT  SDO_LRS.GEOM_SEGMENT_START_PT(route_geometry)
  FROM lrs_routes WHERE route_id = 1;

-- What is the end point of Route1?
SELECT  SDO_LRS.GEOM_SEGMENT_END_PT(route_geometry)
  FROM lrs_routes WHERE route_id = 1;

-- Shift by 5 (for example, 5-mile segment added before original start)
SELECT  SDO_LRS.SCALE_GEOM_SEGMENT(a.route_geometry, m.diminfo, 0, 27, 5)
  FROM lrs_routes a, user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;

-- "Convert" mile measures to kilometers (27 * 1.609 = 43.443)
SELECT  SDO_LRS.SCALE_GEOM_SEGMENT(route_geometry, 0, 43.443, 0)
  FROM lrs_routes WHERE route_id = 1;

-- Clip a piece of Route1.
SELECT  SDO_LRS.CLIP_GEOM_SEGMENT(route_geometry, 5, 10)
  FROM lrs_routes WHERE route_id = 1;

-- Synonym for clip.
SELECT  SDO_LRS.DYNAMIC_SEGMENT(route_geometry, 5, 10)
  FROM lrs_routes WHERE route_id = 1;

-- Point (9,3,NULL) is off the road; should return (9,4,9).
SELECT  SDO_LRS.PROJECT_PT(route_geometry, 
  MDSYS.SDO_GEOMETRY(3301, NULL, NULL, 
     MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1), 
     MDSYS.SDO_ORDINATE_ARRAY(9, 3, NULL)) )
  FROM lrs_routes WHERE route_id = 1;

-- Return the measure of the projected point.
SELECT  SDO_LRS.GET_MEASURE(
 SDO_LRS.PROJECT_PT(a.route_geometry, m.diminfo,
  MDSYS.SDO_GEOMETRY(3301, NULL, NULL, 
     MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1), 
     MDSYS.SDO_ORDINATE_ARRAY(9, 3, NULL)) ),
 m.diminfo )
 FROM lrs_routes a, user_sdo_geom_metadata m
 WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;

-- Is point (9,3,NULL) a valid LRS point? (Should return TRUE.)
SELECT  SDO_LRS.VALID_LRS_PT(
  MDSYS.SDO_GEOMETRY(3301, NULL, NULL, 
     MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1), 
     MDSYS.SDO_ORDINATE_ARRAY(9, 3, NULL)),
  m.diminfo)
  FROM lrs_routes a, user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;

-- Locate the point on Route1 at measure 9, offset 0.
SELECT  SDO_LRS.LOCATE_PT(route_geometry, 9, 0)
  FROM lrs_routes WHERE route_id = 1;

-- Measure queries

-- Find measure for point on segment closest to 10,7
-- Should return 15 (for point 12,7)
SELECT  SDO_LRS.FIND_MEASURE(a.route_geometry, m.diminfo,
  MDSYS.SDO_GEOMETRY(3301, NULL, NULL, 
     MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1), 
     MDSYS.SDO_ORDINATE_ARRAY(10, 7, NULL)) )
 FROM lrs_routes a, user_sdo_geom_metadata m
 WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;

-- Find position of the measure dimension
-- Should return 3
SELECT SDO_LRS.FIND_LRS_DIM_POS('LRS_ROUTES', 'ROUTE_GEOMETRY') 
   FROM DUAL;

-- Measure range
SELECT SDO_LRS.MEASURE_RANGE(route_geometry)
  FROM lrs_routes WHERE route_id = 1;

-- Measure to percentage, and vice versa
SELECT SDO_LRS.MEASURE_TO_PERCENTAGE(a.route_geometry, m.diminfo, 5)
  FROM lrs_routes a, user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;
SELECT SDO_LRS.PERCENTAGE_TO_MEASURE(a.route_geometry, m.diminfo, 50)
  FROM lrs_routes a, user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;

-- Reverse, translate (Note: Redefine done earlier)
-- First, display the original segment; then, reverse.
SELECT a.route_geometry FROM lrs_routes a WHERE a.route_id = 1;
SELECT SDO_LRS.REVERSE_MEASURE(a.route_geometry, m.diminfo)
  FROM lrs_routes a, user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;
 
-- Translate (+10)
-- First, display the original segment; then, translate.
SELECT a.route_geometry FROM lrs_routes a WHERE a.route_id = 1;
SELECT SDO_LRS.TRANSLATE_MEASURE(a.route_geometry, m.diminfo, 10)
  FROM lrs_routes a, user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;

-- Conversion (to/from standard and LRS). In each case, display data before and after.
-- First, to standard because they're already LRS.
SELECT diminfo FROM user_sdo_geom_metadata WHERE table_name = 'LRS_ROUTES';
SELECT SDO_LRS.CONVERT_TO_STD_DIM_ARRAY(m.diminfo)
   FROM user_sdo_geom_metadata m
   WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY';
SELECT diminfo FROM user_sdo_geom_metadata 
   WHERE table_name = 'LRS_ROUTES' AND column_name = 'ROUTE_GEOMETRY';

SELECT a.route_geometry, m.diminfo FROM lrs_routes a, user_sdo_geom_metadata m
             WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
                AND a.route_id = 1;
SELECT SDO_LRS.CONVERT_TO_STD_GEOM(a.route_geometry, m.diminfo)
  FROM lrs_routes a, user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;
SELECT a.route_geometry, m.diminfo FROM lrs_routes a, user_sdo_geom_metadata m
             WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
                AND a.route_id = 1;

-- Next, back to LRS.
SELECT diminfo FROM user_sdo_geom_metadata WHERE table_name = 'LRS_ROUTES';
SELECT SDO_LRS.CONVERT_TO_LRS_DIM_ARRAY(m.diminfo)
   FROM user_sdo_geom_metadata m
   WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY';
SELECT diminfo FROM user_sdo_geom_metadata 
   WHERE table_name = 'LRS_ROUTES' AND column_name = 'ROUTE_GEOMETRY';

SELECT a.route_geometry, m.diminfo FROM lrs_routes a, user_sdo_geom_metadata m
      WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
          AND a.route_id = 1;
SELECT SDO_LRS.CONVERT_TO_LRS_GEOM(a.route_geometry, m.diminfo)
  FROM lrs_routes a, user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
    AND a.route_id = 1;
SELECT a.route_geometry, m.diminfo FROM lrs_routes a, user_sdo_geom_metadata m
        WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
            AND a.route_id = 1;

-- Do layer conversions at end, because index needs to be dropped.

--------------------------
-- Some LRS features new for 9i
---------------------------

-- Create a segment offset 2 to the left from measures 5 through 10.
-- First, display the original segment; then, offset.
SELECT a.route_geometry FROM lrs_routes a WHERE a.route_id = 1;
SELECT  SDO_LRS.OFFSET_GEOM_SEGMENT(a.route_geometry, m.diminfo, 5, 10, 2)
    FROM lrs_routes a, user_sdo_geom_metadata m
    WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
      AND a.route_id = 1;

-- Reverse direction and measures (for example, to prepare for
-- concatenating with another road)
-- First, display the original segment; then, reverse.
SELECT a.route_geometry FROM lrs_routes a WHERE a.route_id = 1;
SELECT  SDO_LRS.REVERSE_GEOMETRY(a.route_geometry, m.diminfo)
    FROM lrs_routes a, user_sdo_geom_metadata m
    WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
      AND a.route_id = 1;

-- Set the measure value of point 8,10 to 20 (originally 22).
DECLARE
geom_segment MDSYS.SDO_GEOMETRY;
dim_array MDSYS.SDO_DIM_ARRAY;
result VARCHAR2(32);

BEGIN

SELECT a.route_geometry into geom_segment FROM lrs_routes a
  WHERE a.route_name = 'Route1';
SELECT m.diminfo into dim_array from 
  user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY';

-- Set the measure value of point 8,10 to 20 (originally 22).
result := SDO_LRS.SET_PT_MEASURE (geom_segment, 
  MDSYS.SDO_GEOMETRY(3301, NULL, NULL, 
     MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1), 
     MDSYS.SDO_ORDINATE_ARRAY(8, 10, 22)),
  20);

-- Display the result.
DBMS_OUTPUT.PUT_LINE('Returned value = ' || result);

END;
/

-- One of the SDO_GEOMETRY type methods: GET_LRS_DIM
SELECT a.route_id, a.route_geometry.GET_LRS_DIM() 
    FROM lrs_routes a WHERE  a.route_id = 1;

-- Aggregate concatenation 

-- First, add a segment with route_id less than 1 (here, zero).
INSERT INTO lrs_routes VALUES(
  0,
  'Route0',
  MDSYS.SDO_GEOMETRY(
    3302,  -- line string, 3 dimensions (X,Y,M), 3rd is linear referencing dimension
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1), -- one line string, straight segments
    MDSYS.SDO_ORDINATE_ARRAY(
      5,14,5,   -- Starting point - 5 is measure from start.
      10,14,0)  -- Ending point - 0 measure (decreasing measure)
  )
);

-- Concatenate all routes.
SELECT SDO_AGGR_LRS_CONCAT(MDSYS.SDOAGGRTYPE(route_geometry, 0.005))
    FROM lrs_routes;

-- Aggregate concatenation using subquery for ordering.
SELECT 
SDO_AGGR_LRS_CONCAT(MDSYS.SDOAGGRTYPE(route_geometry, 0.005)) 
FROM ( 
             SELECT /*+ NO_MERGE */ route_geometry 
             FROM lrs_routes 
             ORDER BY route_id); 

-- Add a segment for Route2
INSERT INTO lrs_routes VALUES(
  2,
  'Route2',
  MDSYS.SDO_GEOMETRY(
    3302,  -- line string, 3 dimensions (X,Y,M), 3rd is linear referencing dimension
    NULL,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1), -- one line string, straight segments
    MDSYS.SDO_ORDINATE_ARRAY(
      5,14,0,   -- Starting point - 0 is measure from start.
      10,14,5)  -- Ending point - 5 is measure from start.
  )
);

DECLARE
geom_segment_1 MDSYS.SDO_GEOMETRY;
geom_segment_2 MDSYS.SDO_GEOMETRY;
result_geom_21 MDSYS.SDO_GEOMETRY;
result_geom_22 MDSYS.SDO_GEOMETRY;

BEGIN

SELECT a.route_geometry into geom_segment_1 FROM lrs_routes a
  WHERE a.route_name = 'Route1';

SELECT a.route_geometry into geom_segment_2 FROM lrs_routes a
  WHERE a.route_name = 'Route2';

-- Concatenate the segments.
result_geom_21 := SDO_LRS.CONCATENATE_GEOM_SEGMENTS(geom_segment_1, geom_segment_2, 0.005);
result_geom_22 := SDO_LRS.CONCATENATE_GEOM_SEGMENTS(geom_segment_2, geom_segment_1, 0.005);

INSERT INTO lrs_routes VALUES(
  21,
  'result_geom_21',
  result_geom_21
);
INSERT INTO lrs_routes VALUES(
  22,
  'result_geom_22',
  result_geom_22
);

END;
/

SELECT * FROM lrs_routes WHERE route_id = 21;

SELECT * FROM lrs_routes WHERE route_id = 22;

-- First, display the original segment; then, reverse.
SELECT a.route_geometry FROM lrs_routes a WHERE a.route_id = 1;
SELECT SDO_LRS.REVERSE_GEOMETRY(a.route_geometry, m.diminfo)
   FROM lrs_routes a, user_sdo_geom_metadata m
   WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
   AND a.route_id = 1;

SELECT SDO_LRS.IS_MEASURE_DECREASING(a.route_geometry, m.diminfo)
   FROM lrs_routes a, user_sdo_geom_metadata m
   WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
   AND a.route_id = 1;

SELECT SDO_LRS.IS_MEASURE_INCREASING(a.route_geometry, m.diminfo)
   FROM lrs_routes a, user_sdo_geom_metadata m
   WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
   AND a.route_id = 1;

SELECT SDO_LRS.VALIDATE_LRS_GEOMETRY(a.route_geometry, m.diminfo)
   FROM lrs_routes a, user_sdo_geom_metadata m
   WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY'
   AND a.route_id = 1;

-- Layer conversions -- Index must be dropped before conversion.

-- Drop the index.
DROP INDEX lrs_routes_idx;

-- First, display the original diminfo
SELECT diminfo FROM user_sdo_geom_metadata 
   WHERE table_name = 'LRS_ROUTES' AND column_name = 'ROUTE_GEOMETRY';

BEGIN
  IF (SDO_LRS.CONVERT_TO_STD_LAYER('LRS_ROUTES', 'ROUTE_GEOMETRY') =  'TRUE')
     THEN
       DBMS_OUTPUT.PUT_LINE('Conversion from LRS_LAYER to STD_LAYER succeeded');
     ELSE
       DBMS_OUTPUT.PUT_LINE('Conversion from LRS_LAYER to STD_LAYER failed');
  END IF;
END;
.
/
SELECT diminfo FROM user_sdo_geom_metadata WHERE table_name = 'LRS_ROUTES';

BEGIN
  IF (SDO_LRS.CONVERT_TO_LRS_LAYER('LRS_ROUTES', 'ROUTE_GEOMETRY') =  'TRUE')
     THEN
       DBMS_OUTPUT.PUT_LINE('Conversion from STD_LAYER to LRS_LAYER succeeded');
     ELSE
       DBMS_OUTPUT.PUT_LINE('Conversion from STD_LAYER to LRS_LAYER failed');
  END IF;
END;
.
/
SELECT diminfo FROM user_sdo_geom_metadata 
   WHERE table_name = 'LRS_ROUTES' AND column_name = 'ROUTE_GEOMETRY';

-- First, display the original segment; then, redefine.
SELECT a.route_geometry FROM lrs_routes a WHERE a.route_id = 1;

-- Redefine geom segment to "convert" miles to kilometers.
DECLARE
geom_segment MDSYS.SDO_GEOMETRY;
dim_array MDSYS.SDO_DIM_ARRAY;

BEGIN

SELECT a.route_geometry into geom_segment FROM lrs_routes a
  WHERE a.route_name = 'Route1';
SELECT m.diminfo into dim_array from 
  user_sdo_geom_metadata m
  WHERE m.table_name = 'LRS_ROUTES' AND m.column_name = 'ROUTE_GEOMETRY';

-- "Convert" mile measures to kilometers (27 * 1.609 = 43.443).
SDO_LRS.REDEFINE_GEOM_SEGMENT (geom_segment, 
  dim_array,
   0,    -- Zero starting measure: LRS segment starts at start of route.
   43.443);  -- End of LRS segment. 27 miles = 43.443 kilometers.

-- Update and insert geometries into table, to display later.
UPDATE lrs_routes a SET a.route_geometry = geom_segment
   WHERE a.route_id = 1;

END;
/

-- -- Display the redefined segment, with all measures "converted".
SELECT a.route_geometry FROM lrs_routes a WHERE a.route_id = 1;


---------------------------------------------------------------------------------------------------
-- COORDINATE SYSTEMS (Spatial Reference Systems) Examples
---------------------------------------------------------------------------------------------------

-- This section uses mostly the same geometry data (cola markets) as earlier, 
--  except that instead of null SDO_SRID values, the SDO_SRID value 8307 is used. 
-- That is, the geometries are defined as using the coordinate system whose SRID is 8307 
-- and whose well-known name is "Longitude / Latitude (WGS 84)". This is probably the
--  most widely used coordinate system, and it is the one used for global positioning 
-- system (GPS) devices. 

-- Create a table for cola (soft drink) markets in a
-- given geography (such as city or state).
-- Each row will be an area of interest for a specific
-- cola (for example, where the cola is most preferred
-- by residents, where the manufacturer believes the
-- cola has growth potential, etc.

CREATE TABLE cola_markets_cs (
  mkt_id NUMBER PRIMARY KEY,
  name VARCHAR2(32),
  shape MDSYS.SDO_GEOMETRY);

-- Note re. areas of interest: cola_a (rectangle) and
-- cola_b (4-sided polygon) are side by side (share 1 border).
-- cola_c is a small 4-sided polygon that overlaps parts of
-- cola_a and cola_b. A rough sketch:
--     ---------+-
--     |    a    |  b  \
--     |     +------+    |   
--     |   /___c _|     |
--     |         |          |
--     ---------+---------|

-- The next INSERT statement creates an area of interest for 
-- Cola A. This area happens to be a rectangle.
-- The area could represent any user-defined criterion: for
-- example, where Cola A is the preferred drink, where
-- Cola A is under competitive pressure, where Cola A
-- has strong growth potential, and so on.
 
INSERT INTO cola_markets_cs VALUES(
  1,
  'cola_a',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    8307,  -- SRID for 'Longitude / Latitude (WGS 84)' coordinate system
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- polygon
    MDSYS.SDO_ORDINATE_ARRAY(1,1, 5,1, 5,7, 1,7, 1,1) -- All vertices must
              -- be defined for rectangle with geodetic data.
  )
);

-- The next two INSERT statements create areas of interest for 
-- Cola B and Cola C. These areas are simple polygons (but not
-- rectangles).

INSERT INTO cola_markets_cs VALUES(
  2,
  'cola_b',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    8307,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- one polygon (exterior polygon ring)
    MDSYS.SDO_ORDINATE_ARRAY(5,1, 8,1, 8,6, 5,7, 5,1)
  )
);

INSERT INTO cola_markets_cs VALUES(
  3,
  'cola_c',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    8307,
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), --one polygon (exterior polygon ring)
    MDSYS.SDO_ORDINATE_ARRAY(3,3, 6,3, 6,5, 4,5, 3,3)
  )
);

-- Insert a rectangle (here, square) instead of a circle as in the original,
-- because arcs are not supported with geodetic coordinate systems.
INSERT INTO cola_markets_cs VALUES(
  4,
  'cola_d',
  MDSYS.SDO_GEOMETRY(
    2003,  -- 2-dimensional polygon
    8307,  -- SRID for 'Longitude / Latitude (WGS 84)' coordinate system
    NULL,
    MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1), -- polygon
    MDSYS.SDO_ORDINATE_ARRAY(10,9, 11,9, 11,10, 10,10, 10,9) -- All vertices must
              -- be defined for rectangle with geodetic data.
  )
);

---------------------------------------------------------------------------
-- UPDATE METADATA VIEW --
---------------------------------------------------------------------------
-- Update the USER_SDO_GEOM_METADATA view. This is required
-- before the Spatial index can be created. Do this only once for each
-- layer (i.e., table-column combination; here: cola_markets_cs and shape).

INSERT INTO USER_SDO_GEOM_METADATA 
  VALUES (
  'cola_markets_cs',
  'shape',
  MDSYS.SDO_DIM_ARRAY(
    MDSYS.SDO_DIM_ELEMENT('Longitude', -180, 180, 10),  -- 10 meters tolerance
    MDSYS.SDO_DIM_ELEMENT('Latitude', -90, 90, 10)  -- 10 meters tolerance
     ),
  8307   -- SRID for ''Longitude / Latitude (WGS 84)' coordinate system
);

-------------------------------------------------------------------
-- CREATE THE SPATIAL INDEX --
-------------------------------------------------------------------
-- Must be R-tree; quadtree not supported for geodetic data.
CREATE INDEX cola_spatial_idx_cs
ON cola_markets_cs(shape)
INDEXTYPE IS MDSYS.SPATIAL_INDEX;

-------------------------------------------------------------------
-- TEST COORDINATE SYSTEM TRANSFORMATION --
-------------------------------------------------------------------

-- Return the transformation of cola_c using to_srid 8199 
-- ('Longitude / Latitude (Arc 1950)')
SELECT c.name, SDO_CS.TRANSFORM(c.shape, m.diminfo, 8199) 
  FROM cola_markets_cs c, user_sdo_geom_metadata m 
  WHERE m.table_name = 'COLA_MARKETS_CS' AND m.column_name = 'SHAPE' 
  AND c.name = 'cola_c';

-- Same as preceding, but using to_srname parameter.
SELECT c.name, SDO_CS.TRANSFORM(c.shape, m.diminfo, 
      'Longitude / Latitude (Arc 1950)')
  FROM cola_markets_cs c, user_sdo_geom_metadata m
  WHERE m.table_name = 'COLA_MARKETS_CS' AND m.column_name = 'SHAPE'
  AND c.name = 'cola_c';

-- Transform the entire SHAPE layer and put results in the table
-- named cola_markets_cs_8199, which the procedure will create.
EXECUTE SDO_CS.TRANSFORM_LAYER('COLA_MARKETS_CS','SHAPE','COLA_MARKETS_CS_8199', 8199);

-- Select all from the old (existing) table.
SELECT * from cola_markets_cs;

-- Select all from the new (layer transformed) table.
SELECT * from cola_markets_cs_8199;

-- Show metadata for the new (layer transformed) table.
DESCRIBE cola_markets_cs_8199;

-- Viewport_Transform
SELECT c.name FROM cola_markets_cs c WHERE
   SDO_FILTER(c.shape, SDO_CS.VIEWPORT_TRANSFORM(
       MDSYS.SDO_GEOMETRY(
           2003,
           0,    -- SRID = 0 (special case)
           NULL,
           MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,3),
           MDSYS.SDO_ORDINATE_ARRAY(-180,-90,180,90)),
       8307), 'querytype=window') = 'TRUE';

