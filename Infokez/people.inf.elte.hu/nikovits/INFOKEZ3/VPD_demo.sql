-- VPD = Virtual Private Database
-- Alkalm�z�s k�rnyezet (context) haszn�lata jogosults�gkezel�s implement�l�s�ra 
-- A p�lda azt mutatja be, hogy hogyan lehet egy felhaszn�l�t korl�tozni, hogy egy 
-- t�bl�nak csak bizonyos sorait l�thassa. K�t felhaszn�l�t hozunk l�tre, akik
-- egy p�ldat�bl�nak m�s-m�s sorait fogj�k l�tni. 

PROMPT SYSTEM
CONNECT SYSTEM@ablinux

-- L�trehozzuk a k�t usert �s a t�blatulajdonost, akikkel tesztelni fogjuk a m�k�d�st
DROP USER user1;
DROP USER user2;
DROP USER tulaj CASCADE;
CREATE USER user1 identified by user1;
CREATE USER user2 identified by user2;
CREATE USER tulaj identified by tulaj;
GRANT connect, resource TO user1,user2, tulaj;

-- L�trehozzuk az alkalmaz�s k�rnyezetet (context), aminek a v�ltoz�i
-- minden bejelentkezett user eset�n az aktu�lis inform�ci�t fogj�k t�rolni
-- A context v�ltoz�it csak az al�bbi package futtat�s�val lehet majd m�dos�tani.
DROP CONTEXT user_info;
CREATE CONTEXT user_info USING tulaj.set_ctx;


PROMPT tulaj
CONNECT tulaj/tulaj@ablinux

-- DROP TABLE proba_tab;
CREATE TABLE proba_tab(szam NUMBER, szoveg VARCHAR2(30), datum DATE);
INSERT INTO proba_tab VALUES(1, 'elso     ...', SYSDATE);
INSERT INTO proba_tab VALUES(2, 'masodik  ...', SYSDATE+1);
INSERT INTO proba_tab VALUES(3, 'harmadik ...', SYSDATE+2);
INSERT INTO proba_tab VALUES(4, 'negyedik ...', SYSDATE+3);
INSERT INTO proba_tab VALUES(5, 'otodik   ...', SYSDATE+4);
INSERT INTO proba_tab VALUES(6, 'hatodik  ...', SYSDATE+5);
COMMIT;
GRANT SELECT ON proba_tab TO user1, user2;
CREATE TABLE hiba_tabla (uzenet VARCHAR2(1000));
GRANT SELECT, INSERT ON hiba_tabla TO public;


-- Az al�bbi t�bla tartalmazza azokat az inform�ci�kat a felhaszn�l�kr�l
-- amelyek alapj�n majd eld�l, hogy a proba_tab t�bl�nak ki mely sorait l�thatja. 
-- Ezen inf�k alapj�n egy login trigger fogja majd be�ll�tani a context v�ltoz�it.
-- DROP TABLE user_info_tab;
CREATE TABLE user_info_tab(jog_kulcs NUMBER, username VARCHAR2(30));
INSERT INTO user_info_tab VALUES(1, 'USER1');
INSERT INTO user_info_tab VALUES(2, 'USER2');
INSERT INTO user_info_tab VALUES(3, 'tulaj');
INSERT INTO user_info_tab VALUES(4, 'SYS');
INSERT INTO user_info_tab VALUES(5, 'SYSTEM');
COMMIT;
GRANT SELECT ON user_info_tab TO public;


-- Ez a package �ll�tja be a context v�ltoz�inak �rt�k�t.
-- Ezt a package-et fogj�k megh�vni a login triggerek, �gy mindenkinek
-- futtat�si jogot kell adni r�.
CREATE OR REPLACE PACKAGE set_ctx AS
   PROCEDURE set_jog_kulcs;
END;
/
CREATE OR REPLACE PACKAGE BODY set_ctx AS
  PROCEDURE set_jog_kulcs IS
    v_jog_kulcs NUMBER;
  BEGIN
    SELECT jog_kulcs INTO v_jog_kulcs FROM tulaj.user_info_tab 
    WHERE username = SYS_CONTEXT('USERENV', 'session_user');
    DBMS_SESSION.SET_CONTEXT('user_info', 'jog_kulcs', v_jog_kulcs);
  END set_jog_kulcs;
END;
/
GRANT EXECUTE ON set_ctx TO PUBLIC;

-- A biztons�gi ir�nyelvet (policy-t) az al�bbi package implement�lja
-- A f�ggv�ny �ltal visszaadott karakterl�ncot fogja az Oracle minden 
-- lek�rdez�shez plusz felt�telk�nt hozz�f�zni.
CREATE OR REPLACE PACKAGE policy_pack AS 
  FUNCTION Dinam_pred (D1 VARCHAR2, D2 VARCHAR2) RETURN VARCHAR2; 
END;
/
CREATE OR REPLACE PACKAGE BODY policy_pack AS
  FUNCTION Dinam_pred (D1 VARCHAR2, D2 VARCHAR2) RETURN VARCHAR2
  IS
    D_predicate VARCHAR2(2000);
    v_jog_kulcs VARCHAR2(200);
    v_hiba      VARCHAR2(400);
  BEGIN
    v_jog_kulcs := SYS_CONTEXT('user_info', 'jog_kulcs');
    IF v_jog_kulcs IS NULL THEN
      D_predicate := '1=1';
    ELSE
      D_predicate := 'MOD(szam,2) = MOD('||v_jog_kulcs||',2)';
    END IF;
--    D_predicate := 'szam > SYS_CONTEXT(''user_info'', ''jog_kulcs'')';
    RETURN D_predicate; 
  EXCEPTION
   WHEN OTHERS THEN
    RETURN '1=2';   
  END Dinam_pred;
END policy_pack;
/
-- GRANT execute on policy_pack to public;

-- �j security policy l�trehoz�sa. A tulaj.proba_tab t�bl�hoz ad hozz� egy 
-- policy1 nev� policy-t, amit a tulaj user policy_pack.dinam_pred nev� 
-- f�ggv�nye implement�l, �s a select utas�t�sra vonatkozik.

PROMPT SYSTEM
CONNECT system@ablinux

-- A t�bla eldob�sa automatikusan t�rli a hozz� tartoz� policy-t is,
-- Ez�rt nem kell az al�bbi sort kiadni.
-- EXECUTE DBMS_RLS.DROP_POLICY('tulaj','proba_tab','policy1') 
EXECUTE DBMS_RLS.ADD_POLICY('tulaj','proba_tab','policy1','tulaj','policy_pack.dinam_pred','select')

-- Ha oszlopszint� v�delmet szeretn�nk akkor az al�bbi param�terez�ssel kell h�vnunk a proced�r�t.
-- Ennek hat�s�ra a v�delem (policy) csak a megadott oszlopok lek�rdez�se eset�n l�p �letbe, 
-- a t�bbi oszlop lek�rdez�se eset�n nem. (Vagyis ez esetben minden sort l�t.)
-- EXECUTE DBMS_RLS.ADD_POLICY('tulaj','proba_tab','policy1','tulaj','policy_pack.dinam_pred','select',
--                  sec_relevant_cols=>'datum,szoveg')

-- Ha az al�bbi param�terez�ssel h�vjuk a proced�r�t, akkor minden sort l�t a lek�rdez�, de azokban a 
-- sorokban, amit egy�bk�nt nem l�tna, a v�dett oszlopokban NULL jelenik meg sz�m�ra. 
-- EXECUTE DBMS_RLS.ADD_POLICY('tulaj','proba_tab','policy1','tulaj','policy_pack.dinam_pred','select',
--                  sec_relevant_cols=>'datum,szoveg', sec_relevant_cols_opt=>DBMS_RLS.ALL_ROWS)



-- L�trehozzuk a k�t felhaszn�l� login trigger�t is
CREATE OR REPLACE TRIGGER tr1
AFTER LOGON ON user1.SCHEMA
BEGIN
  tulaj.set_ctx.set_jog_kulcs;
END;
/

CREATE OR REPLACE TRIGGER tr2
AFTER LOGON ON user2.SCHEMA
BEGIN
  tulaj.set_ctx.set_jog_kulcs;
END;
/
