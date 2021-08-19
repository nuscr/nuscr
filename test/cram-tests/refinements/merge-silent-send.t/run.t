MergeSend should fail, since C cannot tell how One or Two was used.
(At least in the current theory, maybe later we can try to do some weakening of
conditions)
  $ nuscr --project C@MergeSend Merge.nuscr
  nuscr: User error: Unable to merge: 
         (silent) x(int);
         Stuff() to B;
         end
         
         and
         
         (silent) y(int);
         Stuff() to B;
         end
  [124]
