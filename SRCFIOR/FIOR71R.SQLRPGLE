000100070727     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070301     h dftactgrp(*no) actgrp(*caller)
000300070727      * FIOR71R *----------------------------------------------------*
000400070727      *         Assegnaz.distinta ORM automatica
000500070727      *         Aggiorno rif. distinta su ORM/ORF partendo da ORG
000600070730      *         richiamando il programma FIOR61R per ogni distinta
000700070727      *--------------------------------------------------------------*
000800070913     FFior63p   O    E             PRINTER
000900070323     Fazorg01L  IF   E           K DISK
001000950524      *--------------------------------------------------------------*
001100950524      *   DS
001200950524      *--------------------------------------------------------------*
001300070228     ddtaiso           s               d
001400070228     ddtaeur           s               d   datfmt(*eur)
001500070730     D fnorgds       e ds                  extname(fnorg00f)
001600070730     D fnormds       e ds                  extname(fnorm00f)
001700050520     D ddatiute      e ds
001800030910     D azuteds       e ds                  extname(AZUTE00F)
001900030910     D tibs34ds      E DS                  inz
002000950524     D KPJBA         E DS
002100070730     D Fior71ds      E DS
002200070730     D Fior61ds      E DS
002300070730     D og147         E DS
002400070301     d distinta        ds
002500070730     d  dfv                                like(orgddc)
002600070730     d  nfv                                like(orgndc)
002700070730     d  fgs                                like(orgfgs)
002800070301     d savdist         s                   like(distinta)
002900070323     D WLBDA8          DS
003000070323     D  G02DAT                 1      8  0
003100070323     D  G02INV                 9     16  0
003200070323     D  G02ERR                17     17
003300070323     D  G02TGI                18     22  0
003400070228     D*--------------------------------------------------------------*
003500070228     D*  SCHIERE
003600070228     D*--------------------------------------------------------------*
003700070727     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
003800950524     C*--------------------------------------------------------------*
003900950524     C*  Ciclo principale
004000950524     C*--------------------------------------------------------------*
004100070730      * leggo gli ORM messi in distinta su ORG e le inserisco su ORM
004200061003     C/EXEC SQL
004300070730     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.* FROM fnorg03l
004400070730     C+ join fnorm01l on ormpoe= orgpoe and ormnsr=orgnsr and ormnor=
004500070730     C+ orgnor and ormnrv= orgnrv WHERE orgddc = :or71dfvi  and
004600071105     C+ orgfgs = :or71fgsi and ormndc= 0 and ormeti <> ' '
004700070730     C+ ORDER BY Fnorg03l.orgfgs, Fnorg03l.orgndc, Fnorg03l.orgddc
004800061003     C/END-EXEC
004900061003
005000061003     C/EXEC SQL
005100070228     C+ OPEN a1
005200061003     C/END-EXEC
005300061003
005400061003     C                   do        *hival
005500061003
005600061003     C/EXEC SQL
005700070730     C+ FETCH NEXT FROM a1 INTO :fnormds, :fnorgds
005800061003     C/END-EXEC
005900061003
006000070730     c                   select
006100070730     c                   when      sqlcod = 100
006200061003     c                   leave
006300070730     c                   WHEN      SqlCod < 0
006400070730      * record allocato
006500070206     c                   if        sqlcod <> -913
006600070206     C                   seton                                        H1
006700070206     c                   end
006800070227     c                   exsr      srstampa
006900070206     c                   exsr      srfine
007000070206     c                   other
007100070730      * a rottura di distinta richiamo il pgm che aggiorna fnorm e fnorf
007200070730     c                   eval      dfv = orgddc
007300070730     c                   eval      nfv = orgndc
007400070730     c                   eval      fgs = orgfgs
007500070730     c                   if        distinta <> savdist and savdist <> *zeros
007600070730     c                   exsr      sraggorm
007700070301     c                   eval      savdist = distinta
007800070301     c                   end
007900070730     c                   endsl
008000070730
008100070730     c                   enddo
008200061003
008300061003     C/EXEC SQL
008400070228     C+ CLOSE a1
008500061003     C/END-EXEC
008600070730
008700070730     c                   eval      OR71pag = pages
008800070730     c                   eval      OR71riga= riga
008900070730     c                   eval      kpjbu = fior71ds
009000070206     c*
009100950524     C                   SETON                                        LR
009200070730      *------------------------------------------------------------------------*
009300070730      * Aggiorno il file dgli orm
009400070730      *------------------------------------------------------------------------*
009500070730     c     sraggorm      BEGSR
009600070730     c                   clear                   fior61ds
009700070730     c                   eval      or61cmti= 'S'
009800070730     C                   eval      or61FGSi = or71FGSi
009900070730     C                   eval      or61Dfvi = or71Dfvi
010000070730     c                   eval      or61nftl = *zeros
010100070730     C                   eval      or61Nfvi = orgndc
010200070730     c                   eval      or61mtdi = 'M'
010300070730     C                   EVAL      or61PAG = PAGES
010400070730     C                   EVAL      or61RIGA= RIGA
010500070801     c                   call      'FIOR61R'
010600070730     c                   parm                    kpjba
010700070730     c                   parm                    fior61ds
010800070730     c                   eval      pages = or61pag
010900070730     c                   eval      riga = or61riga
011000070730     c                   if        or61erro<>' '
011100070730     c                   exsr      srfine
011200070730     c                   end
011300070730     c                   endsr
011400070730      *------------------------------------------------------------------------*
011500070730      * stampa
011600070730      *------------------------------------------------------------------------*
011700070730     c     srstampa      BEGSR
011800070730     c                   move      or71dfvi      dtaiso
011900070227     c                   move      dtaiso        dtaeur
012000070227     c                   move      dtaeur        prtdfv
012100070730     c                   move      orgndc        prtnfv
012200070704     c                   if        *in85
012300070730     c                   eval      prtpoe =      orgpoe
012400070730     c                   eval      prtnsr =      orgnsr
012500070730     c                   eval      prtnor =      orgnor
012600070730     c                   eval      prtnrv =      orgnrv
012700070730     c                   eval      prtmsg = 'L''ORM non è stata aggiorna-
012800070730     c                             to correttamente.VERIFICARLA'
012900070704     c                   end
013000070730      * PAGINA IN STAMPA
013100070730     c                   IF        *IN80 OR RIGA >=60
013200070730     c                   SETON                                        80
013300070730     c                   ADD       1             PAGES
013400070730     c                   Z-ADD     3             RIGA              3 0
013500070730     c                   END
013600070730      *
013700070227     c   80              write     testa
013800070227     c                   setoff                                       80
013900070319     c   82              write     riga2
014000070323     C   82              ADD       1             RIGA
014100070227     c   84              write     riga4
014200070323     C   84              ADD       1             RIGA
014300070704     c   85              write     riga5
014400070704     C   85              ADD       1             RIGA
014500070704     c                   setoff                                       828485
014600070730      *
014700070227     c                   endsr
014800070730      **************************************************************************
014900070730      * esco con codice d'errore
015000070730      **************************************************************************
015100070730     c     srfine        BEGSR
015200070730      *
015300070730     c                   eval      or71erro= '1'
015400070730     c                   eval      kpjbu = fior71ds
015500070206     c                   seton                                        lr
015600070206     c                   return
015700070730      *
015800070206     c                   endsr
015900070730      *--------------------------------------------------------------*
016000070730      *  Operazioni iniziali
016100070730      *--------------------------------------------------------------*
016200070730     c     *INZSR        BEGSR
016300070730      *
016400070730     c     *ENTRY        PLIST
016500070730     c                   PARM                    KPJBA
016600070730     c                   MOVEL(P)  KPJBU         Fior71ds
016700070730     c                   eval      pages = or71pag
016800070730     c                   eval      riga  = or71riga
016900070730     c                   clear                   or71erro
017000070730      * Inverto la data del giorno
017100070730     c                   TIME                    WHHDAT           14 0
017200070730     c                   MOVE      WHHDAT        G02DAT
017300070730     c                   MOVE      *BLANKS       G02ERR
017400070730     c                   CALL      'XSRDA8'
017500070730     c                   PARM                    WLBDA8
017600070730     c                   Z-ADD     G02INV        DATEU             8 0
017700070730     c     or71fgsi      chain     azorg01l
017800070323     c                   if        %found(azorg01l)
017900070730     c                   MOVEL     ORGDE7        og147
018000070730      * se p.o. non partito esco
018100070730     c                   if        §ogddao <>' '
018200070730     c                   movel     §ogddao       comdda            8 0
018300070323     c                   else
018400070323     c                   clear                   comdda
018500070323     c                   end
018600070323     c                   if        comdda <= dateu and comdda <> 0
018700070323     c                   else
018800070323     c                   seton                                        lr
018900070323     c                   return
019000070323     c                   end
019100070323     c                   end
019200070730      *
019300030718     c     *dtaara       define    §azute        azuteds
019400030718     c     *dtaara       define    §datiute      ddatiute
019500070730     c                   in(E)     *dtaara
019600070730     c                   IF        %Error  or  RSUT = *blanks
019700070730     c                   call      'TIBS34R'
019800070730     c                   parm                    Tibs34Ds
019900070730     c                   in        *dtaara
020000030718     c                   ENDIF
020100070730     c                   ENDSR
020200070523** ErrMsg
020300070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
