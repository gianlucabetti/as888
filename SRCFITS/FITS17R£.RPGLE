000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040913      * FITS17R  *----------------------------------------------------*
000400040913      *  O.R.M.
000500021010      *---------------------------------------------------------------* c
000600040913     fFnORMNEW  if A e           k disk    rename(FNORM000:ORMNEW)
000700040913     fFnORFNEW  o  A e           k disk    rename(FNORF000:ORFNEW)
000800040913     fFnORNNEW  o  A e           k disk    rename(FNORN000:ORNNEW)
000900040913     fFnORPNEW  o  A e           k disk    rename(FNORP000:ORPNEW)
001000040913     fFnORVNEW  o  A e           k disk    rename(FNORV000:ORVNEW)
001100080131     fFnORgNEW  o  A e           k disk    rename(FNORg000:ORgNEW)
001200090406     fFnORaNEW  o  A e           k disk    rename(FNORa000:ORaNEW)
001300100604     fFnOReNEW  o  A e           k disk    rename(FNORe000:OReNEW)
001400140124     fFnORpenw  o  A e           k disk    rename(FNORpe00:ORpenW)
001500140124     fFnORvenw  o  A e           k disk    rename(FNORve00:ORvenW)
001600040913     fFnORM01l  If   e           K disk
001700040913     fFnORM06l  If   e           K disk
001800040913     F                                     RENAME(FNORM000:FNORM006)
001900040913     fFnorf01l  If a e           k disk
002000040913     fFnorn01l  If a e           k disk
002100040913     fFnorv01l  If a e           k disk
002200040913     fFnorp01l  If a e           k disk
002300080131     fFnorg01l  If a e           k disk
002400090406     fFnora01l  If a e           k disk
002500100604     fFnore01l  If a e           k disk
002600140124     fFnorpe1l  If a e           k disk
002700140124     fFnorve1l  If a e           k disk
002800040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002900021008      *
003000021010     d KPJBA         E DS
003100040805     D* Schiera p.o. da trasferire
003200040805     D  SKPO           s              3    dim(500)
003300040913     d DataMin         s                   Like(OrmDao)
003400040913     d DataMinima      s               d   datfmt(*iso)
003500040913     d KLIN            s                   like(ORMPOE)
003600040805      *
003700021008      *****************************************************************
003800021010     c     *ENTRY        PLIST
003900021010     c                   PARM                    KPJBA
004000040805     C                   parm                    PARAM          1500
004100040805     c
004200040805     c     ' ':'0'       xlate     param         param
004300040805     c                   movea     param         skpo
004400040913     c*
004500040913      * Data Minima
004600040913     c                   Move      *Date         DataMINIMA
004700040915     c                   Subdur    60:*d         DataMinima
004800040913     c                   move      DataMinima    DataMin
004900040805     C*
005000040913     c     KORM          KLIST
005100040913     c                   KFLD                    ORMPOE
005200040913     c                   KFLD                    ORMNSR
005300040913     c                   KFLD                    ORMNOR
005400040913     c                   KFLD                    ORMNRV
005500021010      *---------------------------------------------------------------*
005600021008      ****
005700040910      ** LETTURA
005800021008      ****
005900040913     C* LEGGO le linee passate
006000040913     C*
006100040913     c                   z-add     1             x                 4 0
006200040913    1c                   dow       skpo(x)>*zeros
006300040913     c                   movel     skpo(X)       klin
006400040913     c* per p.o. emittente
006500040913     C     klin          SETLL     FNorm01l
006600040913     C     klin          READe     FNorm01l                               30
006700040913     C*
006800040913    2C     *IN30         DOWEQ     *OFF
006900040913     C*
007000040913     C** ELABORAZIONE solo degli ultimi 60 giorni di orm
007100040913     c                   If        OrmDao >=DataMin
007200040913     C                   EXSR      elabora
007300040913     c                   endif
007400040913     C*
007500040913     C     KLIN          READE     FNorm01l                               30
007600040913    2C                   ENDDO
007700040913     c
007800040913     c* per p.o. ritiro
007900040913     C     klin          SETLL     FNorm06l
008000040913     C     klin          READe     FNorm06l                               30
008100040913     C*
008200040913    2C     *IN30         DOWEQ     *OFF
008300040913     c* escludo orm se p.o. emittente nella schiera
008400040913     c                   movel     ormpoe        w003a             3
008500040913     c     w003a         lookup    skpo                                   31
008600040913     C*
008700040913     C** ELABORAZIONE solo degli ultimi 60 giorni di orm
008800040913     c                   If        not *in31 and OrmDao >=DataMin
008900040913     C                   EXSR      elabora
009000040913     c                   endif
009100040913     C*
009200040913     C     KLIN          READE     FNorm06l                               30
009300040913    2C                   ENDDO
009400040913     c
009500040913     C                   ADD       1             X
009600040913     C                   ENDDO
009700040913     C*
009800040805     c                   EXCEPT    RINUM1
009900021011     c                   seton                                        LR
010000040913      *---------------------------------------------------------------*
010100040913      * ELABORO I FILES COLLEGATI
010200040913      *---------------------------------------------------------------*
010300040913     c     ELABORA       BegSr
010400040913      * Controllo se già scritto in precedenza
010500040913     c     Korm          Chain     FnormNEW
010600040913
010700040913     c                   If        NOT %found(FnormNEW)
010800040913
010900040913     c                   write     ORMNEW                               61
011000040913     c                   add       1             TotORM            7 0
011100040913     C* FASI
011200040913     C     KORM          SETLL     FNORF01L
011300040913     C     KORM          READE     FNORF01L
011400040913     C                   dow       not %eof(fnorf01l)
011500040913     c                   write     orfnew
011600040913     c                   add       1             TotORf            7 0
011700040913     C     KORM          READE     FNORF01L
011800040913     c                   enddo
011900080131     C* giri
012000080131     C     KORM          SETLL     FNORg01L
012100080131     C     KORM          READE     FNORg01L
012200080131     C                   dow       not %eof(fnorg01l)
012300080131     c                   write     orgnew
012400080131     c                   add       1             TotORg            7 0
012500080131     C     KORM          READE     FNORg01L
012600080131     c                   enddo
012700090406     C* appuntamenti
012800090406     C     KORM          SETLL     FNORa01L
012900090406     C     KORM          READE     FNORa01L
013000090406     C                   dow       not %eof(fnora01l)
013100090406     c                   write     oranew
013200090406     c                   add       1             TotORa            7 0
013300090406     C     KORM          READE     FNORa01L
013400090406     c                   enddo
013500100604     C* estensione
013600100604     C     KORM          SETLL     FNORe01L
013700100604     C     KORM          READE     FNORe01L
013800100604     C                   dow       not %eof(fnore01l)
013900100604     c                   write     orenew
014000100604     c                   add       1             TotORe            7 0
014100100604     C     KORM          READE     FNORe01L
014200100604     c                   enddo
014300040913     C* proposte variazioni
014400040913     C     KORM          SETLL     FNORP01L
014500040913     C     KORM          READE     FNORP01L
014600040913     C                   dow       not %eof(fnorP01l)
014700040913     c                   write     orpnew
014800040913     c                   add       1             TotORp            7 0
014900040913     C     KORM          READE     FNORp01L
015000040913     c                   enddo
015100040913     C* note
015200040913     C     KORM          SETLL     FNORn01L
015300040913     C     KORM          READE     FNORn01L
015400040913     C                   dow       not %eof(fnorn01l)
015500040913     c                   write     ornnew
015600040913     c                   add       1             TotORn            7 0
015700040913     C     KORM          READE     FNORn01L
015800040913     c                   enddo
015900040913     C* variazioni
016000040913     C     KORM          SETLL     FNORv01L
016100040913     C     KORM          READE     FNORv01L
016200040913     C                   dow       not %eof(fnorv01l)
016300140314     c                   write     orvnew
016400040913     c                   add       1             TotORv            7 0
016500040913     C     KORM          READE     FNORv01L
016600040913     c                   enddo
016700140124     C* Estensioni proposte
016800140124     C     KORM          SETLL     FNORpe1L
016900140124     C     KORM          READE     FNORpe1L
017000140124     C                   dow       not %eof(fnorpe1l)
017100140124     c                   write     orpenw
017200140124     c                   add       1             TotORpe           7 0
017300140124     C     KORM          READE     FNORpe1L
017400140124     c                   enddo
017500140124     C* Estensioni variazioni
017600140124     C     KORM          SETLL     FNORve1L
017700140124     C     KORM          READE     FNORve1L
017800140124     C                   dow       not %eof(fnorve1l)
017900140124     c                   write     orvenw
018000140124     c                   add       1             TotORve           7 0
018100140124     C     KORM          READE     FNORve1L
018200140124     c                   enddo
018300040913     c
018400040913     C                   ENDIF
018500040913     C                   ENDSR
018600021010      *
018700021010      *---------------------------------------------------------------*
018800040805     oQSYSPRT   E            RINUM1         2 02
018900040913     o                                         +  5 'ORM '
019000040913     o                                         +  5 'Fits17R'
019100021010     o                       UDATE             +  2 '  /  /  '
019200021010     o          E            RINUM1      1
019300040913     o                                         +  3 'FNORM00F:'
019400040913     o                       TOTorm        2   +  2
019500040913     o                                         +  3 'FNORF00F:'
019600040913     o                       TOTorf        2   +  2
019700080131     o                                         +  3 'FNORG00F:'
019800080131     o                       TOTorg        2   +  2
019900080131     o                                         +  3 'FNORN00F:'
020000040913     o                       TOTorn        2   +  2
020100080131     o          E            RINUM1      1
020200040913     o                                         +  3 'FNORP00F:'
020300040913     o                       TOTorp        2   +  2
020400040913     o                                         +  3 'FNORV00F:'
020500040913     o                       TOTorv        2   +  2
020600090406     o                                         +  3 'FNORA00F:'
020700090406     o                       TOTora        2   +  2
020800100604     o                                         +  3 'FNORE00F:'
020900100604     o                       TOTore        2   +  2
021000140124     o          E            RINUM1      1
021100140124     o                                         +  3 'FNORPE0F:'
021200140124     o                       TOTorpe       2   +  2
021300140124     o                                         +  3 'FNORVE0F:'
021400140124     o                       TOTorve       2   +  2
