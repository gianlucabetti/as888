000100161014          // statds
000101161014         dcl-ds trulstatds template qualified;
000102161014           st_mode        uns(10);
000103161014           st_ino         uns(10);
000104161014           st_nlink       uns(5);
000105161014           st_pad         char(2);
000106161014           st_uid         uns(10);
000107161014           st_gid         uns(10);
000108161014           st_size        int(10);
000109161014           st_atime       int(10);
000110161014           st_mtime       int(10);
000111161014           st_ctime       int(10);
000112161014           st_dev         uns(10);
000113161014           st_blksize     uns(10);
000114161014           st_allocsize   uns(10);
000115161102           st_objtype     char(10);
000116161014           st_codepage    uns(5);
000117161014           st_reserved1   char(62);
000118161014           st_ino_gen_id  uns(10);
000119161014         end-ds;
000120161014
000121161014         dcl-pr TRulstat extpgm('TRULSTAT');
000200161014           filename varchar(1024) const;
000400161014           statds likeds(trulstatds);
000800161014           esito int(10);
000901161014         end-pr;
000902161014
