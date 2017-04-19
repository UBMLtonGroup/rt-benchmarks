.separator ,

.once hask-gcb-com3.csv
select s1.type,s1.tid,s1.iter,s1.ts,(s2.ts-s1.ts),s1.heap,s2.heap
  from stats s1 inner join stats s2 on s1.tid=s2.tid and s1.iter=s2.iter and s1.action!=s2.action and s1.type=s2.type
 where s1.type='compute' and s1.action='start' and s1.name = 'haskell-norts' and s2.name = 'haskell-norts'
 order by s1.iter desc;

.once hask-gcb-gc3.csv
select s1.type,s1.tid,s1.iter,s1.ts,(s2.ts-s1.ts),s1.heap,s2.heap
  from stats s1 inner join stats s2 on s1.tid=s2.tid and s1.iter=s2.iter and s1.action!=s2.action and s1.type=s2.type
 where s1.type='gc' and s1.action='start' and s1.name = 'haskell-norts' and s2.name = 'haskell-norts'
 order by s1.iter desc;
