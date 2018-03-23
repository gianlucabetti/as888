000100140904     Fwfbolle0f IF   e             disk    usropn
000101140904     fqsysprt   o    f  132        printer
000200140814     F
000300140904     D wbolok          S              7  0 inz
000301140904     D wbolerr         S              5  0 inz
000302140904     D wtempo          S              5  0 inz
000303140904     D wtempom         S              5  0 inz
000304140904     D wtempos         S              2  0 inz
000305140904     D wdataorain      S               z   inz
000306140904     D wdataorafi      S               z   inz
000307140904     D NSpediz         S             12
000400040330     D esito           S              1
000403140904     D tis174dsi     E DS                  INZ
000404140904     D tis174dso     E DS                  INZ
000405140904     d rifero74_30            28     57
002600140818      * - Campi per esecuzione comandi
002700140818     d Qcmd            s            150    inz
002800140818     d Qlen            s             15  5 inz(150)
002900140818      *
003200040330
003500140818      *
003600140904     c****               eval      Qcmd ='CHKOBJ OBJ(GAITRAGRPS) OBJTYPE(*LIB)'
003700140818
003800140904     c***                call      'QCMDEXC'                            90
003900140904     c*                  parm                    Qcmd
004000140904     c*                  parm                    Qlen
004100140818      *
004200140904     C*                  if        not *in90
004300140904     c*                  eval      Qcmd ='ADDLIBLE FILTRAPRD *LAST   '
004400140904     c*                  call      'QCMDEXC'                            90
004500140904     c*                  parm                    Qcmd
004600140904     c*                  parm                    Qlen
004700140818
004800140904     c*                  else
004900140818
005000140904     c*                  eval      Qcmd ='ADDLIBLE FILTRA201  *LAST   '
005100140904     c*                  call      'QCMDEXC'                            90
005200140904     c*                  parm                    Qcmd
005300140904     c*                  parm                    Qlen
005400140818
005500140904     c*                  endif
005600140818
005700140904     c*                  eval      Qcmd ='ADDLIBLE STRATEDEV         '
005800140904     c*                  call      'QCMDEXC'                            90
005900140904     c*                  parm                    Qcmd
006000140904     c*                  parm                    Qlen
006100140818     c
006200140904     c                   open      wfbolle0f
006300140904     c* memorizzo timestamp di inzio elaborazione
006301140904     c                   eval      wdataorain = %timestamp()
006302140904     c* loop lettura file bolle da elaborare
006303140904     c                   read      wfbolle0f
006304140904     c                   dow       not %eof(wfbolle0f)
006305140904     c                   eval      nspedizI74 = %editc(lnp:'X')+
006306140904     c                                         %editc(serie:'X')+%editc(nsp:'X')
006307140904     c                   CALL      'TIS774R'
006308140904     c                   PARM      *BLANK        esito
006309140904     c                   PARM                    tis174dsi
006310140904     c                   PARM                    tis174dso
006311140904     c                   if        rifero74_30 <>*blanks
006312140904     c                   eval      wbolok+=1
006313140904     c                   else
006314140904     c                   eval      wbolerr+=1
006315140904     c                   endif
006316140904     c                   read      wfbolle0f
006317140904     c                   enddo
006318140904     c* fine loop
009801140904     c* memorizzo timestamp di fine  elaborazione
009802140904     c                   eval      wdataorafi = %timestamp()
009804140904     c                   eval      wtempo=%diff(wdataorafi:wdataorain:*seconds)
009805140904     c                   eval      wtempom=%div(wtempo:60)
009806140904     c                   eval      wtempos=%rem(wtempo:60)
009807140904     c* stampo
009808140904     c                   except    riga
010100140814     c
010200140530     C                   EVAL      *INLR = *ON
010300140904     Oqsysprt   e            riga
010301140904     o                                        +   1 '774'
010302140904     o                                        +   1 'Tot ok:'
010400140904     O                       wbolok        z  +   1
010401140904     o                                        +   2 'Tot err:'
010500140904     O                       wbolerr       z  +   1
010501140904     o                                        +   2 'Elab: Inizio '
010600140904     O                       wdataorain       +   1
010601140904     o                                        +   2 'Fine '
010700140904     o                       wdataorafi       +   1
010702140904     o                       wtempom          +   2
010800140904     o                                        +   1 'm'
010900140904     o                       wtempos          +   1
011000140904     o                                        +   1 's'
