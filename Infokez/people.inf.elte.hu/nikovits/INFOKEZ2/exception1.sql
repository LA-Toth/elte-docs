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
