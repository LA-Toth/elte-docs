-- Nehany egyszeru pelda a pl/sql fuggvenyek
-- es procedurak hasznalatara vonatkozoan

SET SERVEROUTPUT ON


-- Az alabbi blokk alprogramjai nem taroltak, azok csak 
-- a blokk utasitasaiban hivhatok 
DECLARE
  szam number(6);

  FUNCTION fv_plusz_1(szam number) RETURN number IS
    lokalis_valtozo NUMBER(6);
  BEGIN
    lokalis_valtozo := szam + 1;
    return(lokalis_valtozo);
  END;

  PROCEDURE pr_plusz_1(szam number) is
    lokalis_valtozo NUMBER(6);
  BEGIN
    lokalis_valtozo := szam + 1;
    dbms_output.put_line(TO_CHAR(lokalis_valtozo));
  END;


BEGIN
  szam := fv_plusz_1(100);

  pr_plusz_1(szam);
END;
/

-- Az alabbi alprogramok viszont taroltak, azok az adatbazisban
-- tarolodnak es a kesobbiekben barmikor hivhatok.
-- A fv SQL utasitasban is hasznalhato (a procedura csak PL/SQL-ben).

CREATE OR REPLACE FUNCTION fv_plusz_2(szam number) RETURN number IS
  lokalis_valtozo NUMBER(6);
BEGIN
  lokalis_valtozo := szam + 2;
  return(lokalis_valtozo);
END;
/      

SELECT fv_plusz_2(1000) FROM dual;


CREATE OR REPLACE PROCEDURE pr_plusz_2(szam number) is
  lokalis_valtozo NUMBER(6);
BEGIN
  lokalis_valtozo := szam + 2;
  dbms_output.put_line(TO_CHAR(lokalis_valtozo));
END;
/

BEGIN
  pr_plusz_2(2000);
END;
/

-- Vagy a fentivel ekvivalens meghivasi mod SQLPLUS-bol

EXECUTE pr_plusz_2(2000);



-- pelda tomb (pl/sql tabla) hasznalatara

SET SERVEROUTPUT ON

DECLARE 
  v_nev VARCHAR2(20);
  CURSOR emp_cur IS 
  SELECT deptno, ename FROM emp;
  rec emp_cur%ROWTYPE;
  TYPE tab_tip IS TABLE OF emp_cur%ROWTYPE INDEX BY BINARY_INTEGER;
  tabla tab_tip;
  i  NUMBER(4);
BEGIN
  OPEN emp_cur;
  LOOP
    FETCH emp_cur INTO rec;
    EXIT WHEN emp_cur%NOTFOUND;
    i:= emp_cur%ROWCOUNT;
    tabla(i) := rec;
    dbms_output.put_line(to_char(tabla(i).deptno)||' - '||tabla(i).ename);
  END LOOP;
  CLOSE emp_cur;
END;
/


-- pelda hiba es kivetelkezelesre
-- ha apro modositasokat irunk a programba, vagy commentbe
-- teszunk sorokat, masik agra terelhetjuk a hibakezelest

DECLARE 
  v_nev  VARCHAR2(20);
  v_szam NUMBER := 0;
  CURSOR emp_cur IS SELECT ename FROM emp;

  hiba1 EXCEPTION;
  pragma EXCEPTION_INIT(hiba1, -20001);
  hiba2 EXCEPTION;
  pragma EXCEPTION_INIT(hiba2, -20002);

  PROCEDURE hibas_proc(szam NUMBER) IS
  BEGIN
    IF MOD(szam, 2) = 0 THEN
      RAISE_APPLICATION_ERROR('-20001', 'Elso hiba');
    ELSE
      RAISE_APPLICATION_ERROR('-20002', 'Masodik hiba');
    END IF;
  END;

BEGIN
  hibas_proc(1);
  v_szam := 1/v_szam;  -- nullaval osztas
  SELECT ename INTO v_nev FROM emp WHERE empno < 0;  -- no_data/too_many_rows
  OPEN emp_cur;
  LOOP
    FETCH emp_cur INTO v_nev;
    EXIT WHEN emp_cur%notfound;
    dbms_output.put_line(v_nev);
    dbms_output.put_line(to_char(emp_cur%rowcount));
  END LOOP;
  CLOSE emp_cur;
EXCEPTION
  WHEN hiba1 THEN
    dbms_output.put_line('Elso hiba fordult elo');
  WHEN hiba2 THEN
    dbms_output.put_line('Masodik hiba fordult elo');
  WHEN zero_divide THEN
    dbms_output.put_line('Nullaval osztas hiba');
  WHEN no_data_found THEN
    dbms_output.put_line('No Data Found hiba');
  WHEN too_many_rows THEN
    dbms_output.put_line('Too many rows hiba');
  WHEN OTHERS THEN
    dbms_output.put_line('Valami mas jellegu hiba ...');
END;
/


