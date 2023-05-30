create table proba(k3 char(3), k2 char(2));
insert into proba values('AB ', 'AB');
select k3,k2, length(k3), length(k2) from proba where k3=k2;

A k�t oszlopot egyenl�nek fogja l�tni az oracle. Ha varchar2 
oszlopok lenn�nek, akkor nem lesznek egyenl�k.


-- VARCHAR2 oszlopok hossza b�jtokban illetve karakterekben megadva

create table proba(o1 varchar2(10 byte), o2 varchar2(10 char));
insert into proba values ('abcde�', '������');
insert into proba values ('abcdefgh�', 'abcd������');
select o1, o2, lengthb(o1), lengthb(o2) from proba;