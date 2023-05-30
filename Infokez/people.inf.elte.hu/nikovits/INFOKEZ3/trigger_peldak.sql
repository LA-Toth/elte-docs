CREATE OR REPLACE TRIGGER dml_trg
AFTER UPDATE OF o1
ON t1
FOR EACH ROW
WHEN (NEW.o1 >= 1000)
DECLARE
  v_local   VARCHAR2(20);
BEGIN
 v_local := :new.o1;
END;
/

CREATE OR REPLACE TRIGGER dml_trg2
BEFORE INSERT OR DELETE OR UPDATE 
ON t2
BEGIN
 null;
END;
/

CREATE OR REPLACE TRIGGER dml_trg3
INSTEAD OF UPDATE 
ON v1
FOR EACH ROW   -- enelkul is sortrigger lesz az instead of miatt
BEGIN
 null;
END;
/

CREATE OR REPLACE TRIGGER db_trg
AFTER SERVERERROR OR LOGON OR STARTUP OR SUSPEND
ON DATABASE
DECLARE     -- A PL/SQL blokknak lehet deklar�ci�s r�sze is
 i number;
BEGIN
 null;
END ;
/

CREATE OR REPLACE TRIGGER db_trg2
BEFORE LOGOFF OR SHUTDOWN
ON DATABASE
BEGIN
 null;
END ;
/

CREATE OR REPLACE TRIGGER any_ddl_trg
BEFORE DDL
ON DATABASE
BEGIN
 null;
END ;
/

CREATE OR REPLACE TRIGGER spec_ddl_trg
BEFORE ALTER OR ANALYZE OR ASSOCIATE STATISTICS OR AUDIT OR CREATE OR DROP OR GRANT
ON DATABASE
BEGIN
 null;
END ;
/

CREATE OR REPLACE TRIGGER sajat_ddl_trg
BEFORE DDL ON nikovits.SCHEMA
BEGIN
 null;
END ;
/

CREATE OR REPLACE TRIGGER sajat_logon_trg
AFTER LOGON ON SCHEMA
BEGIN
 null;
END ;
/

CREATE OR REPLACE TRIGGER any_logon_trg
AFTER LOGON ON DATABASE
BEGIN
 null;
END ;
/