000100110623     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200110623
000701161014     D stringa         S          10000    varying
000800161012     D esito           S             10I 0
001100161013
001101161014     D wstringa        S          10000    varying
001103161013
001104161014     D wStat           S                   like(statds)
001105161014     D wSize           S             10I 0
001106161014
001232161012     D*--------------------------------------------------------------------
001233161012     D* Compone e Verifica Regular Expression
001234161012     D*
001235161012     D*--------------------------------------------------------------------
001236161014
001237161014     D p_statds        S               *
001238161014     D statds          DS                  BASED(p_statds)
001239161014     D  st_mode                      10U 0
001240161014     D  st_ino                       10U 0
001241161014     D  st_nlink                      5U 0
001242161014     D  st_pad                        2A
001243161014     D  st_uid                       10U 0
001244161014     D  st_gid                       10U 0
001245161014     D  st_size                      10I 0
001246161014     D  st_atime                     10I 0
001247161014     D  st_mtime                     10I 0
001248161014     D  st_ctime                     10I 0
001249161014     D  st_dev                       10U 0
001250161014     D  st_blksize                   10U 0
001251161014     D  st_allocsize                 10U 0
001252161014     D  st_objtype                   12A
001253161014     D  st_codepage                   5U 0
001254161014     D  st_reserved1                 62A
001255161014     D  st_ino_gen_id                10U 0
001256161014
001257161014     D stat            PR            10I 0 ExtProc('stat')
001258161014     D   path                          *   value options(*string)
001259161014     D   buf                           *   value
001260161014
001261161014     D MyStat          S                   like(statds)
001262161014     D MySize          S             10I 0
001263161014
001267161012      /COPY QSYSINC/QRPGLESRC,QUSEC
001268161014
001269161014
001270161012
001271161012     C     *ENTRY        PLIST
001272161013     C                   PARM                    Pattern
001273161013     C                   PARM                    Stringa
001274161013     C                   Parm                    Esito
001275161013
001500161013           clear  Esito;
001501161013
001502161014             wStringa = Stringa;
001601161013
001628161013           // Se Regular Expression corrente è soddisfatta ...
001629161013               esito = Dir_RegExp(wPattern:wStringa);
001633161013
001639161014
001686161014      * get stat info into "MyStat":
001687161014     c                   if        stat('/path/to/file.txt':
001688161014     c                                   %addr(mystat)) < 0
001689161014     c                   callp     EscErrno(errno)
001690161014     c                   endif
001691161014
001692161014      * move structure to overlay the "mystat" info:
001693161014     c                   eval      p_statds = %addr(mystat)
001694161014
001695161014      * read the file's size into MySize:
001696161014                      wSize = st_size
001697161014
001698161014                *inrt = *on;
001713161014                Return;
001714161014
001715161014
