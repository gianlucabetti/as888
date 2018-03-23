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
001400150702     fFnORtNEW  o  A e           k disk    rename(FNORt000:ORtNEW)
001500140124     fFnORpenw  o  A e           k disk    rename(FNORpe00:ORpenW)
001600140124     fFnORvenw  o  A e           k disk    rename(FNORve00:ORvenW)
001700040913     fFnORM01l  If   e           K disk
001800040913     fFnORM06l  If   e           K disk
001900040913     F                                     RENAME(FNORM000:FNORM006)
002000040913     fFnorf01l  If a e           k disk
002100040913     fFnorn01l  If a e           k disk
002200040913     fFnorv01l  If a e           k disk
002300040913     fFnorp01l  If a e           k disk
002400080131     fFnorg01l  If a e           k disk
002500090406     fFnora01l  If a e           k disk
002600100604     fFnore01l  If a e           k disk
002700150702     fFnort01l  If a e           k disk
002800140124     fFnorpe1l  If a e           k disk
002900140124     fFnorve1l  If a e           k disk
003000040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003100021008      *
003200021010     d KPJBA         E DS
003300040805     D* Schiera p.o. da trasferire
003400040805     D  SKPO           s              3    dim(500)
003500040913     d DataMin         s                   Like(OrmDao)
003600040913     d DataMinima      s               d   datfmt(*iso)
003700040913     d KLIN            s                   like(ORMPOE)
003800040805      *
003900021008      *****************************************************************
004000021010     c     *ENTRY        PLIST
004100021010     c                   PARM                    KPJBA
004200040805     C                   parm                    PARAM          1500
004300040805     c
004400040805     c     ' ':'0'       xlate     param         param
004500040805     c                   movea     param         skpo
004600040913     c*
004700040913      * Data Minima
004800040913     c                   Move      *Date         DataMINIMA
004900040915     c                   Subdur    60:*d         DataMinima
005000040913     c                   move      DataMinima    DataMin
005100040805     C*
005200040913     c     KORM          KLIST
005300040913     c                   KFLD                    ORMPOE
005400040913     c                   KFLD                    ORMNSR
005500040913     c                   KFLD                    ORMNOR
005600040913     c                   KFLD                    ORMNRV
005700021010      *---------------------------------------------------------------*
005800021008      ****
005900040910      ** LETTURA
006000021008      ****
006100040913     C* LEGGO le linee passate
006200040913     C*
006300040913     c                   z-add     1             x                 4 0
006400040913    1c                   dow       skpo(x)>*zeros
006500040913     c                   movel     skpo(X)       klin
006600040913     c* per p.o. emittente
006700040913     C     klin          SETLL     FNorm01l
006800040913     C     klin          READe     FNorm01l                               30
006900040913     C*
007000040913    2C     *IN30         DOWEQ     *OFF
007100040913     C*
007200040913     C** ELABORAZIONE solo degli ultimi 60 giorni di orm
007300040913     c                   If        OrmDao >=DataMin
007400040913     C                   EXSR      elabora
007500040913     c                   endif
007600040913     C*
007700040913     C     KLIN          READE     FNorm01l                               30
007800040913    2C                   ENDDO
007900040913     c
008000040913     c* per p.o. ritiro
008100040913     C     klin          SETLL     FNorm06l
008200040913     C     klin          READe     FNorm06l                               30
008300040913     C*
008400040913    2C     *IN30         DOWEQ     *OFF
008500040913     c* escludo orm se p.o. emittente nella schiera
008600040913     c                   movel     ormpoe        w003a             3
008700040913     c     w003a         lookup    skpo                                   31
008800040913     C*
008900040913     C** ELABORAZIONE solo degli ultimi 60 giorni di orm
009000040913     c                   If        not *in31 and OrmDao >=DataMin
009100040913     C                   EXSR      elabora
009200040913     c                   endif
009300040913     C*
009400040913     C     KLIN          READE     FNorm06l                               30
009500040913    2C                   ENDDO
009600040913     c
009700040913     C                   ADD       1             X
009800040913     C                   ENDDO
009900040913     C*
010000040805     c                   EXCEPT    RINUM1
010100021011     c                   seton                                        LR
010200040913      *---------------------------------------------------------------*
010300040913      * ELABORO I FILES COLLEGATI
010400040913      *---------------------------------------------------------------*
010500040913     c     ELABORA       BegSr
010600040913      * Controllo se già scritto in precedenza
010700040913     c     Korm          Chain     FnormNEW
010800040913
010900040913     c                   If        NOT %found(FnormNEW)
011000040913
011100040913     c                   write     ORMNEW                               61
011200040913     c                   add       1             TotORM            7 0
011300040913     C* FASI
011400040913     C     KORM          SETLL     FNORF01L
011500040913     C     KORM          READE     FNORF01L
011600040913     C                   dow       not %eof(fnorf01l)
011700040913     c                   write     orfnew
011800040913     c                   add       1             TotORf            7 0
011900040913     C     KORM          READE     FNORF01L
012000040913     c                   enddo
012100080131     C* giri
012200080131     C     KORM          SETLL     FNORg01L
012300080131     C     KORM          READE     FNORg01L
012400080131     C                   dow       not %eof(fnorg01l)
012500080131     c                   write     orgnew
012600080131     c                   add       1             TotORg            7 0
012700080131     C     KORM          READE     FNORg01L
012800080131     c                   enddo
012900090406     C* appuntamenti
013000090406     C     KORM          SETLL     FNORa01L
013100090406     C     KORM          READE     FNORa01L
013200090406     C                   dow       not %eof(fnora01l)
013300090406     c                   write     oranew
013400090406     c                   add       1             TotORa            7 0
013500090406     C     KORM          READE     FNORa01L
013600090406     c                   enddo
013700100604     C* estensione
013800100604     C     KORM          SETLL     FNORe01L
013900100604     C     KORM          READE     FNORe01L
014000100604     C                   dow       not %eof(fnore01l)
014100100604     c                   write     orenew
014200100604     c                   add       1             TotORe            7 0
014300100604     C     KORM          READE     FNORe01L
014400100604     c                   enddo
014500040913     C* proposte variazioni
014600040913     C     KORM          SETLL     FNORP01L
014700040913     C     KORM          READE     FNORP01L
014800040913     C                   dow       not %eof(fnorP01l)
014900040913     c                   write     orpnew
015000040913     c                   add       1             TotORp            7 0
015100040913     C     KORM          READE     FNORp01L
015200040913     c                   enddo
015300040913     C* note
015400040913     C     KORM          SETLL     FNORn01L
015500040913     C     KORM          READE     FNORn01L
015600040913     C                   dow       not %eof(fnorn01l)
015700040913     c                   write     ornnew
015800040913     c                   add       1             TotORn            7 0
015900040913     C     KORM          READE     FNORn01L
016000040913     c                   enddo
016100040913     C* variazioni
016200040913     C     KORM          SETLL     FNORv01L
016300040913     C     KORM          READE     FNORv01L
016400040913     C                   dow       not %eof(fnorv01l)
016500140314     c                   write     orvnew
016600040913     c                   add       1             TotORv            7 0
016700040913     C     KORM          READE     FNORv01L
016800040913     c                   enddo
016900150702     C* note aut
017000150702     C     KORM          SETLL     FNORt01L
017100150702     C     KORM          READE     FNORt01L
017200150702     C                   dow       not %eof(fnort01l)
017300150702     c                   write     ortnew
017400150702     c                   add       1             TotORt            7 0
017500150702     C     KORM          READE     FNORt01L
017600150702     c                   enddo
017700140124     C* Estensioni proposte
017800140124     C     KORM          SETLL     FNORpe1L
017900140124     C     KORM          READE     FNORpe1L
018000140124     C                   dow       not %eof(fnorpe1l)
018100140124     c                   write     orpenw
018200140124     c                   add       1             TotORpe           7 0
018300140124     C     KORM          READE     FNORpe1L
018400140124     c                   enddo
018500140124     C* Estensioni variazioni
018600140124     C     KORM          SETLL     FNORve1L
018700140124     C     KORM          READE     FNORve1L
018800140124     C                   dow       not %eof(fnorve1l)
018900140124     c                   write     orvenw
019000140124     c                   add       1             TotORve           7 0
019100140124     C     KORM          READE     FNORve1L
019200140124     c                   enddo
019300040913     c
019400040913     C                   ENDIF
019500040913     C                   ENDSR
019600021010      *
019700021010      *---------------------------------------------------------------*
019800040805     oQSYSPRT   E            RINUM1         2 02
019900040913     o                                         +  5 'ORM '
020000040913     o                                         +  5 'Fits17R'
020100021010     o                       UDATE             +  2 '  /  /  '
020200021010     o          E            RINUM1      1
020300040913     o                                         +  3 'FNORM00F:'
020400040913     o                       TOTorm        2   +  2
020500040913     o                                         +  3 'FNORF00F:'
020600040913     o                       TOTorf        2   +  2
020700080131     o                                         +  3 'FNORG00F:'
020800080131     o                       TOTorg        2   +  2
020900080131     o                                         +  3 'FNORN00F:'
021000040913     o                       TOTorn        2   +  2
021100080131     o          E            RINUM1      1
021200040913     o                                         +  3 'FNORP00F:'
021300040913     o                       TOTorp        2   +  2
021400040913     o                                         +  3 'FNORV00F:'
021500040913     o                       TOTorv        2   +  2
021600090406     o                                         +  3 'FNORA00F:'
021700090406     o                       TOTora        2   +  2
021800100604     o                                         +  3 'FNORE00F:'
021900100604     o                       TOTore        2   +  2
022000140124     o          E            RINUM1      1
022100150702     o                                         +  3 'FNORT00F:'
022200150702     o                       TOTort        2   +  2
022300140124     o                                         +  3 'FNORPE0F:'
022400140124     o                       TOTorpe       2   +  2
022500140124     o                                         +  3 'FNORVE0F:'
022600140124     o                       TOTorve       2   +  2
