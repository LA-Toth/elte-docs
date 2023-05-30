-- VPD = Virtual Private Database
-- Alkalmázás környezet (context) használata jogosultságkezelés implementálására 
-- A példa azt mutatja be, hogy hogyan lehet egy felhasználót korlátozni, hogy egy 
-- táblának csak bizonyos sorait láthassa. Két felhasználót hozunk létre, akik
-- egy példatáblának más-más sorait fogják látni. 

PROMPT SYSTEM
CONNECT SYSTEM@ablinux

-- Létrehozzuk a két usert és a táblatulajdonost, akikkel tesztelni fogjuk a mûködést
DROP USER user1;
DROP USER user2;
DROP USER tulaj CASCADE;
CREATE USER user1 identified by user1;
CREATE USER user2 identified by user2;
CREATE USER tulaj identified by tulaj;
GRANT connect, resource TO user1,user2, tulaj;

-- Létrehozzuk az alkalmazás környezetet (context), aminek a változói
-- minden bejelentkezett user esetén az aktuális információt fogják tárolni
-- A context változóit csak az alábbi package futtatásával lehet majd módosítani.
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


-- Az alábbi tábla tartalmazza azokat az információkat a felhasználókról
-- amelyek alapján majd eldõl, hogy a proba_tab táblának ki mely sorait láthatja. 
-- Ezen infók alapján egy login trigger fogja majd beállítani a context változóit.
-- DROP TABLE user_info_tab;
CREATE TABLE user_info_tab(jog_kulcs NUMBER, username VARCHAR2(30));
INSERT INTO user_info_tab VALUES(1, 'USER1');
INSERT INTO user_info_tab VALUES(2, 'USER2');
INSERT INTO user_info_tab VALUES(3, 'tulaj');
INSERT INTO user_info_tab VALUES(4, 'SYS');
INSERT INTO user_info_tab VALUES(5, 'SYSTEM');
COMMIT;
GRANT SELECT ON user_info_tab TO public;


-- Ez a package állítja be a context változóinak értékét.
-- Ezt a package-et fogják meghívni a login triggerek, így mindenkinek
-- futtatási jogot kell adni rá.
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

-- A biztonsági irányelvet (policy-t) az alábbi package implementálja
-- A függvény által visszaadott karakterláncot fogja az Oracle minden 
-- lekérdezéshez plusz feltételként hozzáfûzni.
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

-- Új security policy létrehozása. A tulaj.proba_tab táblához ad hozzá egy 
-- policy1 nevû policy-t, amit a tulaj user policy_pack.dinam_pred nevû 
-- függvénye implementál, és a select utasításra vonatkozik.

PROMPT SYSTEM
CONNECT system@ablinux

-- A tábla eldobása automatikusan törli a hozzá tartozó policy-t is,
-- Ezért nem kell az alábbi sort kiadni.
-- EXECUTE DBMS_RLS.DROP_POLICY('tulaj','proba_tab','policy1') 
EXECUTE DBMS_RLS.ADD_POLICY('tulaj','proba_tab','policy1','tulaj','policy_pack.dinam_pred','select')

-- Ha oszlopszintû védelmet szeretnénk akkor az alábbi paraméterezéssel kell hívnunk a procedúrát.
-- Ennek hatására a védelem (policy) csak a megadott oszlopok lekérdezése esetén lép életbe, 
-- a többi oszlop lekérdezése esetén nem. (Vagyis ez esetben minden sort lát.)
-- EXECUTE DBMS_RLS.ADD_POLICY('tulaj','proba_tab','policy1','tulaj','policy_pack.dinam_pred','select',
--                  sec_relevant_cols=>'datum,szoveg')

-- Ha az alábbi paraméterezéssel hívjuk a procedúrát, akkor minden sort lát a lekérdezõ, de azokban a 
-- sorokban, amit egyébként nem látna, a védett oszlopokban NULL jelenik meg számára. 
-- EXECUTE DBMS_RLS.ADD_POLICY('tulaj','proba_tab','policy1','tulaj','policy_pack.dinam_pred','select',
--                  sec_relevant_cols=>'datum,szoveg', sec_relevant_cols_opt=>DBMS_RLS.ALL_ROWS)



-- Létrehozzuk a két felhasználó login triggerét is
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
