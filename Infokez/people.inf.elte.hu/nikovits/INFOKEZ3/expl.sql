PROMPT "Adja meg az utasitas nevet: "
ACCEPT ut

SELECT LPAD(' ', 2*level)||operation||' '||options||' '||object_name terv
FROM plan_table
START WITH id = 0 AND statement_id = '&&ut'
CONNECT BY PRIOR id = parent_id AND statement_id = '&&ut';




