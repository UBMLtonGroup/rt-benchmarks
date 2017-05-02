.separator ,

.once cloj-gcb-com1.csv
select s1.type,s1.tid,s1.iter,s1.ts,(s2.ts-s1.ts),s1.heap,s2.heap
  from stats s1 inner join stats s2 on s1.tid=s2.tid and s1.iter=s2.iter and s1.action!=s2.action and s1.type=s2.type
 where s1.type='compute' and s1.action='start' and s1.name = '20170321-gcbench-run6' and s2.name = '20170321-gcbench-run6'
 order by s1.iter desc;

.once cloj-gcb-gc1.csv
select s1.type,s1.tid,s1.iter,s1.ts,(s2.ts-s1.ts),s1.heap,s2.heap
  from stats s1 inner join stats s2 on s1.tid=s2.tid and s1.iter=s2.iter and s1.action!=s2.action and s1.type=s2.type
 where s1.type='gc' and s1.action='start' and s1.name = '20170321-gcbench-run6' and s2.name = '20170321-gcbench-run6'
 order by s1.iter desc;
