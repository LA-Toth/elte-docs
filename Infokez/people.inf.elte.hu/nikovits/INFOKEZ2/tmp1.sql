create table proba(k3 char(3), k2 char(2));
insert into proba values('AB ', 'AB');
select k3,k2, length(k3), length(k2) from proba where k3=k2;

A két oszlopot egyenlõnek fogja látni az oracle. Ha varchar2 
oszlopok lennének, akkor nem lesznek egyenlõk.


-- VARCHAR2 oszlopok hossza bájtokban illetve karakterekben megadva

create table proba(o1 varchar2(10 byte), o2 varchar2(10 char));
insert into proba values ('abcdeû', 'éõûûûû');
insert into proba values ('abcdefghû', 'abcdéõûûûû');
select o1, o2, lengthb(o1), lengthb(o2) from proba;