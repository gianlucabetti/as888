000100140904     Fwfbolle0f IF   e             disk    usropn
000101140904     fqsysprt   o    f  132        printer
000200140814     F
000300140904     D wbolok          S              7  0 inz
000301140904     D wbolerr         S              7  0 inz
000302140904     D wtempo          S              5  0 inz
000303140904     D wdataorain      S               z   inz
000304140904     D wdataorafi      S               z   inz
000305140904     D NSpediz         S             12
000400040330     D esito           S              1
000500140814     D tis778dsi     E DS                  qualified INZ(*EXTDFT)
000600140814     D tis778dso     E DS                  INZ(*EXTDFT)
000700140530     D tis778dsM     E DS                  QUALIFIED INZ(*EXTDFT)
000800140530     D this            DS                  QUALIFIED INZ
000900140530     D  rpyOpCode...
001000140530     D                               10I 0
001100140530     D  rpyIdMsg...
001200140530     D                               10I 0
001300140530     D  e...
001400140530     D                                3U 0
001500140530     D  m...
001600140530     D                                3U 0
001700140530     D  idCampo...
001800140530     D                               10A
001900140530     D  idMsg...
002000140530     D                                7A
002100140530     D  textMsg...
002200140530     D                              255A
002300140530     D  errWarn...
002400140530     D                                1A
002500140818     d
002600140818      * - Campi per esecuzione comandi
002700140818     d Qcmd            s            150    inz
002800140818     d Qlen            s             15  5 inz(150)
002900140818      *
003000140530      /copy gaitrasrc/srcprotopr,tis778R
003100140530      /copy gaitrasrc/srcconst,tis778R
003200040330
003500140818      *
003600140818     c                   eval      Qcmd ='CHKOBJ OBJ(GAITRAGRPS) OBJTYPE(*LIB)'
003700140818
003800140818     c                   call      'QCMDEXC'                            90
003900140818     c                   parm                    Qcmd
004000140818     c                   parm                    Qlen
004100140818      *
004200140818     C                   if        not *in90
004300140818     c                   eval      Qcmd ='ADDLIBLE FILTRAPRD *LAST   '
004400140818     c                   call      'QCMDEXC'                            90
004500140818     c                   parm                    Qcmd
004600140818     c                   parm                    Qlen
004700140818
004800140818     c                   else
004900140818
005000140818     c                   eval      Qcmd ='ADDLIBLE FILTRA201  *LAST   '
005100140818     c                   call      'QCMDEXC'                            90
005200140818     c                   parm                    Qcmd
005300140818     c                   parm                    Qlen
005400140818
005500140818     c                   endif
005600140818
005700140818     c                   eval      Qcmd ='ADDLIBLE STRATEDEV         '
005800140818     c                   call      'QCMDEXC'                            90
005900140818     c                   parm                    Qcmd
006000140818     c                   parm                    Qlen
006100140818     c
006200140904     c                   open      wfbolle0f
006300140904     c* memorizzo timestamp di inzio elaborazione
006301140904     c                   eval      wdataorain = %timestamp()
006400140530      /free
006500140818
006502140904       read           wfbolle0f;
006503140904       dow not %eof(wfbolle0f);
006600140818
006700140904       tis778dsi.nspedizI74 = %editc(lnp:'X')+%editc(serie:'X')+%editc(nsp:'X');
006800140530        MONITOR;
006900140530         TIS778_GetBolla( TIS778_RQSOPCODE_GETBOLLA
007000140530                        : this.rpyOpCode
007100140530                        : this.rpyIdMsg
007200140603                        : TIS778DSI.formatoi74
007300140530                        : TIS778DSI
007400140530                        : %SIZE(TIS778DSI)
007500140814                        : formatoo74
007600140530                        : TIS778DSO
007700140530                        : %SIZE(TIS778DSO)
007800140530                        : TIS778DSM.formato
007900140530                        : TIS778DSM
008000140530                        : %SIZE(TIS778DSM)
008100140530                        );
008200140530         ON-ERROR;
008300140530           this.rpyOpCode = TIS778_RPYOPCODE_ERROR;
008301140904           wbolerr+=1;
008400140818         ENDMON;
008500140814           if rifero74>0  ;
008501140904             wbolok+=1;
009600140814           endif ;
009601140904       read           wfbolle0f;
009700140904       enddo;
009800140530      /end-free
009801140904     c* memorizzo timestamp di fine  elaborazione
009802140904     c                   eval      wdataorafi = %timestamp()
009803140904      /free
009804140904       wtempo=%diff(wdataorafi:wdataorain:*minutes);
009805140904      /end-free
009806140904     c* stampo
009807140904     c                   except    riga
010100140814     c
010200140530     C                   EVAL      *INLR = *ON
010300140904     Oqsysprt   e            riga
010301140904     o                                        +   1 '778'
010302140904     o                                        +   1 'Tot ok:'
010400140904     O                       wbolok        z  +   1
010401140904     o                                        +   2 'Tot err:'
010500140904     O                       wbolerr       z  +   1
010501140904     o                                        +   2 'Elab: Inizio '
010600140904     O                       wdataorain       +   1
010601140904     o                                        +   2 'Fine '
010700140904     o                       wdataorafi       +   1
010701140904     o                                        +   2 'Minuti '
010702140904     o                       wtempo        z  +   1
