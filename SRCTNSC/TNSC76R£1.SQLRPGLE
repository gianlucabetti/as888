000100030326     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND QC2LE)
000200031114     H*PARMS ACTGRP(*CALLER)
000300030326     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     F*----------------------------------------------------------------
000500030326     FTNCSB03L  UF   E           K DISK    commit
000600030326     FTABEL00F  UF   E           K DISK    commit
000700030326     FTNCSBM1k  uf   E             DISK    prefix(£) commit
000800030326     FTNCSBMIT  O    E             DISK    commit
000900030326     Ffivabcaf  o    E             DISK    commit
001000030331     Ffivadcaf  o    E             DISK    commit
001001130424     Fwfvam00f  O    E             DISK
001002130426     Fwfvam01l  if   E           k DISK    rename(wfvam000:wfvam1)
001003130429     Fwfvam02l  uf   E           k DISK    prefix(£) rename(wfvam000:wfvam2)
001100081125     Ftivgd00f  if a E             DISK    commit
001200030325     Fazorg01l  IF   E           K DISK
001201130426     Ftntbe01l  if   e           k disk
001300030325     Ftnsc76p   O    e             PRINTER
001400030314     d*
001401130430     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
001402130430     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
001800000000     D*----------------------------------------------------------------
001900030314     D KPJBA         E DS
002000030314     D DS4B          E DS
002100030314     D DS4C          E DS
002200081124     D tnvasds       E DS
002300081124     D trul28ds      E DS
002400030325     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002500030325     D CNACO         E DS                  EXTNAME(CNACO00F)
002600030325     D CNIND         E DS                  EXTNAME(CNIND00F)
002700030325     D CNCLP         E DS                  EXTNAME(CNCLP00F)
002800030325     D FNCLS         E DS                  EXTNAME(FNCLS00F)
002900030324     D dsbld7        E DS
003000030325     D soc001        E DS                  EXTNAME(xsoc001ds)
003100030314     D xsocds          DS          1000
003200031113     D xatbds        e DS
003300031113?     *  Tabella Y4Z
003400031113     D angycs        E DS                  extname(angycsds)
003500021112     ** atoi() -- Convert Character String to Integer
003600021112     D atoi            PR            10I 0
003700021112     D                                     EXTPROC('atoi')
003800021112     D  Char                           *
003900021112     D                                     VALUE
004000021112     D                                     OPTIONS(*STRING)
004100120227     D sass            S             16    DIM(5000)
004200120227     D comass          S             16
004201120301     D savsped         S             16
004300120222     d xx              s              3  0
004301120320     D sasst           S             16    DIM(5000)
004302120320     D comasst         S             16
004303120320     D savspedt        S             16
004304120320     d xxt             s              3  0
004305130424      *
004306130430     d coman           s             99
004307130430     d luncmd          s             15  5
004308130430      *
004309130424     d conta           s              3  0
004310130430     d sped            s              1
004311130429     d exdte           s                   like(vamdte)
004312130429     d exnra           s                   like(vamnra)
004313130429     d exabi           s                   like(vamabi)
004314130429     d excai           s                   like(vamcai)
004315130429     d exksc           s                   like(vamksc)
004316130429     d exndp           s                   like(vamndp)
004317130430     d exaas           s                   like(vamaas)
004318130430     d exnsp           s                   like(vamnsp)
004319130430     d exnrs           s                   like(vamnrs)
004320130430     d exlnp           s                   like(vamlnp)
004321130424     d
004400030314     d* tracciati restituiti da sql                                    *****
004500030314     D tncsbc          DS
004600030325     D  tfac                          3  0
004700030325     D  poac                          3  0
004800030325     D  zscc                          2  0
004900030325     D  cdic                               like(csbcdi)
005000030325     D  rsmc                         35
005100030325     D  inmc                         35
005200030325     D  lomc                         35
005300030325     D  camc                          9
005400030325     D  prmc                          2
005500030325     D  nzmc                          3
005600030325     D  rscc                         35
005700030325     D  indc                         35
005800030325     D  lodc                         35
005900030325     D  cadc                          9
006000030325     D  prdc                          2
006100030325     D  nzdc                          3
006200030327     D  CONc                          7  0
006300030325     D tncsbw          DS
006400030310     D                                     OCCURS(20)
006500030310     D                                     INZ
006600030325     D  aasw                               like(csbaas)
006700030314     D  nrsw                               like(csbnrs)
006800030325     D  nspw                               like(csbnsp)
006900030314     D  mgsw                               like(csbmgs)
007000030314     D  casw                               like(csbcas)
007100030314     D  vcaw                               like(csbvca)
007200030325     D  nraw                               like(csbnra)
007300030314     D  abiw                               like(csbabi)
007400030314     D  cabw                               like(csbcab)
007500030325     D  rmnw                               like(csbrmn)
007600030325     D  rmaw                               like(csbrma)
007700030325     D  rsdw                               like(csbrsd)
007800030325     D  lnpw                               like(csblnp)
007900051118     D  lnaw                               like(csblna)
008000051118     D  prow                               like(csbpro)
008100030325     D  dtew                               like(csbdte)
008200030325     D  tpiw                               like(csbtpi)
008300030325     D  ndpw                               like(csbndp)
008400030325     D  ddpw                               like(csbddp)
008500030325     D  fpcw                               like(csbfpc)
008600030325     D  dtaw                               like(csbdta)
008700030326     D  RRNw                         15  0
008800120222     D  prgw                               like(£csbprg)
008900120222     D  idcw                               like(£csbidc)
009000120222     D  rgfw                               like(£csbrgf)
009100030325     D poas            S                   like(poac)
009200081125     D var             S                   like(vgdksu)
009300120222     D pnas1           S                   like(pnca)
009400120222     D pnas2           S                   like(pnca)
009500030325     D I               S              5I 0
009600030310     D dtaiso          S               d
009700030310     D dtaeur          S               d   datfmt(*eur)
009800030326     Dnoctrscos        s              1
009900030326     Desito            s              1
010000030822     c* ds per sottocampare il barcode della spedizione
010100030822     D ds_pis          DS
010200030822     D  ds_pisaas                     2  0
010300030822     D  ds_pislnp                     3  0
010400030822     D  ds_pisnrs                     2  0
010500030822     D  ds_pisnsp                     7  0
010600030526     DDATACOM          DS
010700030526     D GGCOM                          2  0
010800030526     D MMCOM                          2  0
010900030526     D AACOM                          4  0
010901120301     DspedCOM          DS
010902120301     D asCOM                          4  0
010903120301     D lnCOM                          3  0
010904120301     D nrCOM                          2  0
010905120301     D nsCOM                          7  0
011000060516     d $Intest1        c                   const('Sede Operativa ed Am-
011100060516     d                                     ministrativa:')
011200060531     d $Intest2        c                   const('via E. Mattei, 42 -
011300060531     d                                     - 40138 BOLOGNA')
011400060531     d $Intest4        c                   const('P.IVA 04507990150')
011500021112
011600030314     D* convertitore in moneta di conto
011700030314     D/COPY *LIBL/SRCCPY,PJX002PR
011800030314     c*****************************************************************
011900081127     c*
012000030310     C* MI POSIZIONO CON TPI=M + BNA=0  + CDI
012100030324     c* mi seleziona un cliente alla volta
012200030310     C/EXEC SQL
012300030325     C+ DECLARE TNCSBC CURSOR FOR SELECT DISTINCT CSBtfa, csbpoa, csbzsc,
012400030327     C+ CSBCDI, CSBRSM, CSBINM, CSBLOM, CSBCAM, CSBPRM, CSBNZM, CSBRSC,
012500030331     C+ CSBIND, CSBLOD, CSBCAD, CSBPRD, CSBNZD, COUNT(*) FROM tncsbm1k
012600030331     C+ GROUP BY CSBtfa, csbpoa, csbzsc, CSBCDI, CSBRSM, CSBINM, CSBLOM,
012700030331     C+ CSBCAM, CSBPRM, CSBNZM, CSBRSC, CSBIND, CSBLOD, CSBCAD, CSBPRD,
012800120221     C+ CSBNZD ORDER BY CSBtfa, csbpoa, CSBCDI
012900030310     C/END-EXEC
013000030314
013100030314     C/EXEC SQL
013200030314     C+ OPEN tncsbc
013300030314     C/END-EXEC
013400030314
013500030325     C                   DOU       sqlcod <> 0
013600030314
013700030314     C/EXEC SQL
013800030326     C+ FETCH NEXT FROM TNCSBC INTO : tncsbc
013900030314     C/END-EXEC
014000030314
014100030314     C                   SELECT
014200030314     C                   WHEN      SQLCod = 100
014300030314     C                   WHEN      SQLCod < 0
014400030314     C                   EVAL      *INH1 = *ON
014500030314     C                   OTHER
014600030314     c* stampa testata e apre membro
014700030314     c                   exsr      leggi
014800030314     C                   ENDSL
014900030314
015000030314     C                   ENDDO
015100030314     C/EXEC SQL
015200030314     C+ CLOSE tncsbc
015300030314     C/END-EXEC
015301130424     c                   exsr      riord_vam
015400030310
015500030325     C* chiudo BARCODE
015600030325     C                   clear                   trul28ds
015700030325     c                   eval      i28tla = 'C'
015800030822     c                   call      'TRUL28R1'
015900030325     c                   parm                    trul28ds
016000030314     C*
016100081127     c                   clear                   kpjbu
016101130430     c                   feod      wfvam00f
016102130430     c                   movea     cmd1          coman
016103130430     c                   exsr      comandi
016200030314     C                   SETON                                        LR
016300030314     C*--------------------------------------------------------
016400030314     C* leggi           elabora record letto                  *
016500030314     C*--------------------------------------------------------
016600030314     C     leggi         BEGSR
016700030327     c* calcolo totale pagine
016800030331     c     conc          div       20            paga
016900030327     c                   mvr                     resto             7 0
017000030327     c                   if        resto > 0
017100030331     c                   add       1             paga
017200030327     c                   end
017300030519     c                   z-add     paga          pcol
017400030519     c                   z-add     1             pagda
017500030327     c                   seton                                        60
017600081125     C* CONTROLLA SE CLIENTE CON SCARICO DISCHETTO C/A MITT.
017700081125     C                   SETOFF                                       50
017800090127     c                   movel     *all'0'       var
017900081125     c                   clear                   ds4c
018000081125     C                   MOVEL     '4C'          TBLCOD
018100081125     C                   MOVEL(p)  cdic          TBLKEY
018200081125     C     KTAB          CHAIN     TABEL                              90
018300081125     c                   if        %found(tabel00f)
018400081125     C                   MOVEL     TBLUNI        DS4C
018500081125     C     §4CFLG        COMP      'S'                                    50
018600081125     C* COD. CLIENTE UNIFIC. X APERTURA MEMBRO  DA TABELLA
018700090127     C     §4CCKS        IFEQ      *ZEROS
018800090127     C                   MOVE      cdic          VAR
018900081125     C                   ELSE
019000090127     C                   MOVE      §4CCKS        VAR
019100081125     C                   END
019200081125     c                   end
019300030317     c* per ogni cliente letto raggruppo max 20 record (perchè x ogni 20
019400030401     c* record letti devo fare una lettera).
019500030317     c                   exsr      srsql
019600030310     C                   ENDSR
019700120222     C*--------------------------------------------------------
019800120222     c* conta spedizioni e assegni
019900120222     C*--------------------------------------------------------
020000120222     C     srconta       BEGSR
020100120222     c                   clear                   pnca
020200120222     c                   clear                   pnas
020300120222     c                   clear                   pnas1
020400120222     c                   clear                   pnas2
020500030317     C/EXEC SQL
020600120222     C+ SELECT Count(*) into :pnca
020700120222     C+ from (SELECT csbaas, csblnp, csbnrs, csbnsp
020800120222     C+ FROM tncsbm1k WHERE csbcdi = :cdic and
020900030401     C+ csbtfa = :tfac and csbpoa = :poac and CSBzsc = :zscc and CSBRSM =
021000030401     C+ :rsmc and CSBINM = :inmc and CSBLOM = :lomc and csbcam = :camc
021100030401     C+ and CSBPRM = :prmc and CSBNZM = :nzmc and CSBRSC = :rscc and
021200030401     C+ CSBIND = :indc and CSBLOD = :lodc and CSBCAD = :cadc and CSBPRD =
021300120222     C+ :prdc and CSBNZD = :nzdc GROUP BY csbaas, CSBlnp, CSBNRS, CSBnsp)
021400120222     C+ as pippo
021500030317     C/END-EXEC
021600120222     c* assegni singoli
021700120222     C/EXEC SQL
021800120222     C+ SELECT Count(*) into :pnas1
021900120222     C+ FROM tncsbm1k WHERE csbcdi = :cdic and
022000120222     C+ csbtfa = :tfac and csbpoa = :poac and CSBzsc = :zscc and CSBRSM =
022100120222     C+ :rsmc and CSBINM = :inmc and CSBLOM = :lomc and csbcam = :camc
022200120222     C+ and CSBPRM = :prmc and CSBNZM = :nzmc and CSBRSC = :rscc and
022300120222     C+ CSBIND = :indc and CSBLOD = :lodc and CSBCAD = :cadc and CSBPRD =
022400120222     C+ :prdc and CSBNZD = :nzdc and csbprg = ' '
022500120222     C/END-EXEC
022600120222     c* assegni multipli
022700120222     C/EXEC SQL
022800120222     C+ SELECT Count(*) into :pnas2
022900120222     C+ from (SELECT csbnra, csbabi, csbcab, csbdte
023000120222     C+ FROM tncsbm1k WHERE csbcdi = :cdic and
023100120222     C+ csbtfa = :tfac and csbpoa = :poac and CSBzsc = :zscc and CSBRSM =
023200120222     C+ :rsmc and CSBINM = :inmc and CSBLOM = :lomc and csbcam = :camc
023300120222     C+ and CSBPRM = :prmc and CSBNZM = :nzmc and CSBRSC = :rscc and
023400120222     C+ CSBIND = :indc and CSBLOD = :lodc and CSBCAD = :cadc and CSBPRD =
023500120227     C+ :prdc and CSBNZD = :nzdc and csbprg <> ' '
023600120227     C+ GROUP BY csbnra, csbabi, csbcab, csbdte)
023700120222     C+ as pippo1
023800120222     C/END-EXEC
023900120222     c                   eval      pnas = pnas1 + pnas2
024000120222     c                   endsr
024100120222     C*--------------------------------------------------------
024200120222     C* srsql sql per clienti NO 8888 o 9999                   *
024300120222     C*--------------------------------------------------------
024400120222     C     srsql         BEGSR
024500120222     c                   exsr      srconta
024600120222     c                   clear                   wsborder
024700120222     c                   clear                   sass
024800120222     c                   clear                   xx
024801120320     c                   clear                   sasst
024802120320     c                   clear                   xxt
024900120222     C/EXEC SQL
025000120222     C+ DECLARE TNCSB2 CURSOR FOR SELECT CSBAAS, CSBNRS, CSBNSP, CSBMGS,
025100120222     C+ CSBCAS, CSBVCA, CSBNRA, CSBABI, CSBCAB, CSBRMN, CSBRMA, CSBrsd,
025200120222     C+ CSBLNP, CSBLNA, CSBPRO, CSBDTE, CSBTPI, CSBNDP, CSBDDP, CSBFPC,
025300120222     C+ csbdta, RRN(TNCSBM1K), csbprg, csbidc, csbrgf FROM tncsbm1k WHERE
025400120222     C+ csbcdi = :cdic and csbtfa = :tfac and csbpoa = :poac and CSBzsc =
025500120222     C+ :zscc and CSBRSM = :rsmc and CSBINM = :inmc and CSBLOM = :lomc
025600120222     C+ and csbcam = :camc and CSBPRM = :prmc and CSBNZM = :nzmc and
025700120222     C+ CSBRSC = :rscc and CSBIND = :indc and CSBLOD = :lodc and CSBCAD =
025800120227     C+ :cadc and CSBPRD = :prdc and CSBNZD = :nzdc ORDER BY csbprg,
025900120222     C+ CSBCAS, CSBNRA, CSBABI, CSBCAB FOR READ ONLY OPTIMIZE FOR 20 ROWS
026000120222     C/END-EXEC
026100030317
026200030317     C/EXEC SQL
026300030325     C+ OPEN tncsb2
026400030317     C/END-EXEC
026500030317
026600030325     C                   DOU       sqlcod <> 0
026700030317
026800030317     C/EXEC SQL
026900030325     C+ FETCH NEXT FROM TNCSB2 FOR 20 ROWS INTO :tncsbw
027000030317     C/END-EXEC
027100030317
027200030317     C                   SELECT
027300030317     C                   WHEN      SQLCod = 100
027400030317     C                   WHEN      SQLCod < 0
027500030317     C                   EVAL      *INH1 = *ON
027600030317     C                   OTHER
027700030317     C                   EXSR      stampa
027800030317     C                   ENDSL
027900030317
028000030317     C                   ENDDO
028100030317
028200030317     C/EXEC SQL
028300030325     C+ CLOSE tncsb2
028400030317     C/END-EXEC
028500030331     C* AGGIORNO NUMERATORE LETTERA
028600030331     C                   EXSR      SRAGGTAB
028700030331     C                   COMMIT
028800030317     c                   endsr
028900030314     C*--------------------------------------------------------
029000030314     C* stampa          elabora il dettaglio                  *
029100030314     C*--------------------------------------------------------
029200030314     C     stampa        BEGSR
029300030331     c* calcolo il totale della lettera (sqler3 è il numero dei record
029400030331     c* letti)
029500030331     C                   CLEAR                   I
029600030331     C                   CLEAR                   pimp
029700030331     C                   DOU       I = SQLEr3
029800030331     C                   EVAL      I = I + 1
029900030331     C     I             OCCUR     tncsbw
029901120320     c* controllo di sommare la spedizione una volta sola con il
029902120320     c* primo assegno nel caso di multiassegni
029903120320     c                   setoff                                       89
029904120320     c                   if        prgw <> ' '
029905120628     c                   movel     aasw          comaast           4
029906120320     c                   movel     lnpw          comlnpt           3
029907120320     c                   movel     nrsw          comnrst           2
029908120320     c                   movel     nspw          comnspt           7
029909120320     c                   eval      comasst=comaast+comlnpt+comnrst+comnspt
029910120320     c     comasst       lookup    sasst                                  89
029915120320     c                   if        not *in89
029916120320     c                   add       1             xxt
029917120320     c                   eval      sasst(xxt) = comasst
029918120320     c                   else
029919120320     c                   iter
029920120320     c                   end
029921120320     c                   end
030000030331     c                   if        vcaw <> 'EUR'
030100030331     C                   RESET                   PJX00202DS
030200030331     C                   MOVEL     vcaw          Divisa0202
030300030331     C                   MOVEL     'EUR'         DivMC0202
030400030331     C                   Z-ADD     casw          ImporD0202
030500030331     c                   move      amgudt        dtaiso
030600030331     C                   MOVE      Dtaiso        DtRif0202
030700030331     C                   MOVEL     *blank        Esito
030800030331     C                   Move      *On           NoCtrScos
030900030331     C* chiamata per reperire il cambio
031000030331     C                   CALLP     X0202TestC(PJX00202DS :
031100030331     C                                            %Size(PJX00202DS):
031200030331     C                                            *Off             :
031300030331     C                                            Esito            :
031400030331     C                                            NoCtrScos        :
031500030331     C                                            *Omit            :
031600030331     C                                            %Size(ANDVS00F)  :
031700030331     C                                            %Size(ANDVX00F)  :
031800030331     C                                            ANDVS00F         :
031900030331     C                                            ANDVX00F          )
032000030331     C* se non ci sono stati errori
032100030331     C                   IF        Esito = *off
032200030331     C                   add       ImporM0202    pimp
032300030331     C                   ENDIF
032400030331     c                   else
032500030331     c                   add       casw          pimp
032600030331     c                   end
032700030331     c                   enddo
032800030331     C* STAMPO TESTATA 1° volta
032900030331     c                   if        *in60
033000030314     C                   EXSR      WRITES
033100030327     c                   setoff                                       60
033200030331     c                   else
033300030331     c                   exsr      write1
033400030331     c                   end
033500030314     ** leggo 20 record e stampo le righe ricevute.
033600030326     c                   if        o28err = *blanks
033700030314     C                   CLEAR                   I
033800030314     C                   DOU       I = SQLEr3
033900030314     C                   EVAL      I = I + 1
034000030325     C     I             OCCUR     tncsbw
034100030314     C* STAMPA DETTAGLIO RIGA DI LETTERA CONTRASSEGNI
034200030314     C                   EXSR      WRIRIG
034300030314     C* SCRIVE RECORD SU MEMBRO SCARICO C/A
034400030314     C   50              EXSR      WRTTCC
034500030314     C* SCRIVE RECORD SU MEMBRO PER SCARICO SU FILE PC
034600030314     C                   EXSR      WRTTPC
034700030314     C* DELETO RECORD APPENA SCRITTO PRIMA DI LEGGERE IL SUCCESSIVO
034800030326     C     rrnw          DELETE    TNCSBW00
034900030314     C                   ENDDO
035000030324     c                   end
035100030314     C                   ENDSR
035200030331     C*--------------------------------------------------------
035300030331     C* WRITES    ESEGUE STAMPA INTESTAZIONE LETTERA 1° pagina*
035400030331     C*--------------------------------------------------------
035500030331     C     WRITES        BEGSR
035600030331     c                   z-add     §4buln        numlet
035700030324     C                   ADD       1             NUMLET
035800030411     C* REPERISCE BARCODE della spedizione
035900030822     C                   clear                   trul28ds
036000030822     C                   clear                   ds_pis
036100030822     c                   eval      i28mod = 'BAR'
036200030822     c                   move      *year         ds_pisaas
036300030822     c                   eval      ds_pislnp = 102
036400030822     c                   eval      ds_pisnrs = 02
036500030822     c                   eval      ds_pisnsp = numlet
036600030822     c                   eval      i28cod = ds_pis
036700030822     c                   call      'TRUL28R1'
036800030822     c                   parm                    trul28ds
036900030411     c* se reperito barcode imposto i campi in stampa
037000030822     c                   if        o28err = *blanks
037100030822     c                   eval      pbarSPE = o28cod
037200030417     c                   eval      pfvf = 'DOCUMENTI'
037300030325     c* terminal arrivo
037400030325     c                   eval      ptfp = tfac
037500030325     c* po arrivo
037600030325     c                   eval      plna = poac
037700030325     c* zona
037800030325     c                   eval      pznc = zscc
037900030325     c* Bartolini
038000030325     c                   eval      prsm = xscrgs
038100030325     c                   eval      pfir = xscrgs
038200030325     c* data spedizione (oggi)
038300030325     c                   eval      pdsp = gmaudt
038400030325     c* po partenza fisso 102
038500030325     c                   eval      plnp = 102
038600030519     c                   eval      plnps = 102
038700030325     c* spedizione=numero lettera
038800030404     c                   eval      pnrs = 02
038900030519     c                   eval      pnrss = 02
039000030325     c                   eval      pnsp = numlet
039100030325     c* reperisco l'indirizzo della linea di arrivo
039200030325     c                   if        poac <> poas
039300030325     c     poac          chain     azorg01l
039400030325     c                   eval      pinm = orgind
039500030326     c                   clear                   pcam
039600030326     c                   move      orgcpf        pcam
039700030325     c                   eval      plom = orgloc
039800051118     c*                  eval      ppol = orgloc
039900030325     c                   eval      ppot = orgtel
040000030325     c                   eval      pprm = orgpro
040100030325     c                   eval      poas = poac
040200030325     c                   end
040300030325     c* numero spedizioni
040400120222     c*                  eval      pnca = conc
040500030325     c* dati di spedizione del cliente/destinatario
040600030325     c                   eval      prsd = rsmc
040700030325     c                   eval      pksc = cdic
040800030325     c                   eval      pind = inmc
040900030325     c                   eval      pcad = camc
041000030325     c                   eval      plod = lomc
041100030325     c                   eval      pprd = prmc
041200030325     c* dati abituali del cliente/destinatario
041300030325     c                   eval      prsc = rscc
041400030325     c                   eval      pinc = indc
041500030325     c                   eval      pcac = cadc
041600030325     c                   eval      ploc = lodc
041700030325     c                   eval      pprc = prdc
041800030325     c*
041900030325     c                   move      gmaudt        dtaeur
042000030326     c                   move      dtaeur        com10            10
042100030326     c                   eval      pdta = 'Bologna, lì ' + com10
042200030331     C* Stampa testata lettera
042300030331     C                   EXSR      write1
042400030331     C* SCRIVO IL RECORD SU FIVABCAF
042500030403     c* se reperito barcode imposto i campi in stampa
042600030403     c                   if        o28err = *blanks
042700030331     C                   EXSR      srvab
042800030324     c                   end
042900030822     c                   end
043000030314     c*
043100910619     C                   ENDSR
043200030331     C*--------------------------------------------------------
043300030331     C* WRITE1    ESEGUE STAMPA INTESTAZIONE LETTERA pag. successive
043400030331     C*--------------------------------------------------------
043500030331     C     WRITE1        BEGSR
043600030331     c* segnacollo se prima volta = al numero lettera se no aggiungo 1
043700030331     c                   if        *in60
043800030331     c                   z-add     numlet        pseg
043900030331     c                   else
044000030331     c                   add       1             pseg
044100030331     c                   add       1             pagda
044200030331     c                   end
044300030513     c                   movel(p)  pseg          prif
044400030403     C* REPERISCE BARCODE del segnacollo
044500030403     C                   clear                   trul28ds
044600030403     C                   clear                   dsbld7
044700030403     c                   eval      i28mod = 'BAR'
044800030403     c                   eval      §lclnp = 102
044900030403     c                   eval      §lclna = poac
045000030404     c                   eval      §lcnrs = 02
045100030403     c                   eval      §lcncd = pseg
045200030403     c                   eval      §lczsc = zscc
045300030403     c                   eval      i28cod = dsbld7
045400030822     c                   call      'TRUL28R1'
045500030403     c                   parm                    trul28ds
045600030403     c* se reperito barcode imposto i campi in stampa
045700030403     c                   if        o28err = *blanks
045800030411     c* barcode SEGNACOLLO
045900030411     c                   eval      pbarSGN = o28cod
046000030331     C** TESTATA LETTERA
046100030417     C                   write     prttxt
046200030508     C                   write     prtbar
046300030508     C                   write     prtbox
046400030331     C                   write     prttx1
046500030331     C                   write     prttx9
046600030331     C                   write     prtest
046700030403     c                   end
046800030331     C                   ENDSR
046900000000     C*--------------------------------------------------------
047000030314     C* WRIRIG    ESEGUE STAMPA Dettaglio                     *
047100000000     C*--------------------------------------------------------
047200000000     C     WRIRIG        BEGSR
047300000000     C* DATA DI SPEDIZIONE
047400120301     C                   MOVE      AASw          AsCOM
047401120301     C                   MOVE      lnpw          lnCOM
047402120301     C                   MOVE      nrsw          nrCOM
047403120301     C                   MOVE      nspw          nsCOM
047405120301     C                   MOVE      AASw          AACOM
047500030526     C                   MOVEl     MGSw          MMCOM
047600030526     C                   MOVE      MGSw          GGCOM
047700030526     C                   MOVEL     DATACOM       PDDS
047800030327     C                   eval      pdnr = nrsw
047900030327     C                   eval      pdns = nspw
048000030326     C                   eval      pdlp = lnpw
048100051118     C                   eval      pprv = proW
048200030326     C                   eval      pdde = RSDw
048300030327     C                   eval      pdrn = rmnw
048400030326     C                   eval      pdra = Rmaw
048500030326     C                   z-add     casw          pdim
048600030326     C                   eval      pddi = vcaw
048700030715     C                   z-add     ABIw          pdab
048800030715     C                   z-add     CABw          pdca
048900030326     C                   EVAL      pdna = atoi(nraw)
049000030507     C                   EVAL      pdna = atoi(%SUBST(%TRIML(nraw):1:4))
049100120628     c* evidenzio i multiassegni con un "*" se codice cli.mittente diverso
049200120222     c                   if        prgw = *blanks
049300120222     c                   clear                   ppiu
049400120222     c                   else
049401120628     c* A fronte di un multiassegno verifico se ho codici clienti diversi
049402120313     c                   clear                   ppiu
049403120628     c                   clear                   outnrcli          2 0
049404120628     c                   call      'TNSC76R1'
049405120628     c                   parm                    prgw
049406120628     c                   parm                    outnrcli
049409120628     c                   if        outnrcli > 1
049500120628     c                   eval      ppiu = '*'
049600120222     c                   end
049601120628     c                   end
049602120301     c* stampo i rif. spedizione una volta sola,  l'archivio dovrebbe già
049603120301     c* essere ordinato per spedizione in caso di multiassegni
049604120301     c                   if        spedcom <> savsped
049700030325     C                   write     prtesd
049701120301     c                   eval      savsped = spedcom
049702120301     c                   else
049703120301     C                   write     prtesda
049704120301     c                   end
049800941031     C* AGGIORNA DATI SU TNCSB
049900030314     C     KNSP          CHAIN     TNCSB03L                           41
050000910619     C                   Z-ADD     *ALL'9'       CSBBNA
050100941031     C                   Z-ADD     AMGUDT        CSBDDP
050200030521     C                   Z-ADD     pseg          CSBNDP
050300030512     C                   Z-ADD     AMGUDT        DDPw
050400030521     C                   Z-ADD     pseg          NDPw
050500931025     C                   MOVE      *ZERO         CSBDTR
050600931025     C                   MOVE      ' '           CSBFTR
050700941031     C                   EXCEPT    TNCSB
050800000000     C*
050900000000     C                   ENDSR
051000910923     C*--------------------------------------------------------
051100910923     C* WRITCC    SCRIVO IL TRACCIATO C/A PER I CLIENTI       *
051200910923     C*--------------------------------------------------------
051300910923     C     WRTTCC        BEGSR
051400081124     c                   clear                   tnvasds
051401120321     c                   setoff                                       88
051500120222     c* controllo di scrivere la spedizione una volta sola con il
051600120222     c* primo assegno nel caso di multiassegni
051700120222     c                   if        prgw <> ' '
051800120628     c                   movel     aasw          comaas            4
051900120227     c                   movel     lnpw          comlnp            3
052000120227     c                   movel     nrsw          comnrs            2
052100120227     c                   movel     nspw          comnsp            7
052200120227     c                   eval      comass =  comaas+comlnp+comnrs+comnsp
052300120222     c     comass        lookup    sass                                   88
052400120222     c                   else
052401120321     c                   setoff                                       88
052600120222     c                   end
052700930927     C*
052800120222     c                   if        not *in88
052900120222     c                   if        prgw <> ' '
053000120222     c                   add       1             xx
053100120222     c                   eval      sass(xx) = comass
053200120222     c                   end
053300030326     C                   eval      vaslnp = LNPw
053400030326     C                   MOVEL     AASw          VASAAS
053500030326     C                   MOVEL     NRSw          VASNRS
053600030326     C                   MOVEL     NSPw          VASNSP
053700030326     C                   MOVEL     LNAw          VASLNA
053800030326     C                   MOVEL     RMNw          VASRMN
053900030326     C                   MOVEL     RMAw          VASRMA
054000030326     C                   Z-ADD     CASw          VASCAS
054100030326     C                   MOVEL     VCAw          VASVCA
054200030326     C                   MOVEL     NRAw          VASNRA
054300030326     C                   Z-ADD     DTEw          VASDTE
054400030326     C                   MOVEL     TPIw          VASTPI
054500030326     C                   Z-ADD     ABIw          VASABI
054600030326     C                   Z-ADD     CABw          VASCAI
054700030326     C                   MOVEL     NDPw          VASNDP
054800030326     C                   MOVEL     DDPw          VASDDP
054900030326     C                   MOVEL     FPCw          VASFPC
055000030325     C                   MOVEL     CDIc          VASKSC
055100910923     C* SE PAGAMENTO CON BONIFICO :
055200910923     C* CHAIN SU CNCLP PER PRENDERE C/C BONIFICO CONTRASSEGNO
055300941031     C                   MOVEL     *BLANK        VASCCB
055400030326     C     FPCw          IFEQ      '2'
055500981007     C                   CLEAR                   TIBS69
055600981007     C                   CLEAR                   CNACO
055700981007     C                   CLEAR                   CNIND
055800981007     C                   CLEAR                   CNCLP
055900981007     C                   CLEAR                   FNCLS
056000981007     C                   MOVEL     ' '           I69TLA
056100981007     C                   MOVEL     KNSIF         I69SIF
056200981007     C                   Z-ADD     0             I69KCC
056300030325     C                   Z-ADD     cdic          I69KAC
056400030325     C                   Z-ADD     cdic          I69KCP
056500981007     C                   CALL      'TIBS69R'
056600981007     C                   PARM                    TIBS69
056700981007     C                   PARM                    CNACO
056800981007     C                   PARM                    CNIND
056900981007     C                   PARM                    CNCLP
057000981007     C                   PARM                    FNCLS
057100981008     C     O69ERR        IFEQ      *BLANK
057200981007     C                   MOVEL     CLPCCB        VASCCB
057300981007     C                   MOVEL     CLPABI        VASABP
057400981007     C                   MOVEL     CLPCAB        VASCAP
057500981007     C                   END
057600910923     C                   ELSE
057700030326     C                   Z-ADD     ABIw          VASABP
057800030326     C                   Z-ADD     CABw          VASCAP
057900910923     C                   END
058000081125     c* scrivo file tivgd00f
058100081124     c                   clear                   tivgd000
058200081124     C                   eval      vgdDTA = %subst(TNVASDS:1:%size(TNVASDS))
058300081124     c                   eval      vgdtip = 'VS'
058400081125     c                   eval      vgdksu = var
058500081125     c                   eval      vgdtsc = §4CTSD
058600081124     c                   eval      vgdpgm = 'TNSC76R'
058700081125     c                   eval      vgddat = amgudt
058800081124     C                   WRITE     tivgd000
058900120222     c                   end
059000910923     C                   ENDSR
059100910923     C*
059200010510     C*--------------------------------------------------------
059300010510     C* WRTTPC    SCRIVO IL TRACCIATO PER FILE PC MACCHINA SMISTA ASS.
059400010510     C*--------------------------------------------------------
059500010510     C     WRTTPC        BEGSR
059600010510     C*
059700120222     c                   add       1             wsborder
059800120222     C                   Z-ADD     rgfw          WSBrgf
059900120222     C                   Z-ADD     LNPw          WSBLNP
060000030326     C                   Z-ADD     LNAw          WSBLNA
060100030326     C                   Z-ADD     AASw          WSBAAS
060200030326     C                   Z-ADD     NRSw          WSBNRS
060300030326     C                   Z-ADD     NSPw          WSBNSP
060400010510     C*
060500030326     C                   Z-ADD     CASw          WSBCAS
060600120222     C                   Z-ADD     idcw          WSBidc
060700030326     C                   Z-ADD     ABIw          WSBABI
060800030326     C                   Z-ADD     CABw          WSBCAB
060900030326     C                   Z-ADD     NDPw          WSBNDP
061000030326     C                   Z-ADD     DDPw          WSBDDP
061100120302     C                   Z-ADD     cdic          WSBCDI
061101120302     c                   eval      wsbragcdi = rsmc
061102120302     c                   eval      wsbrsd = rsdw
061200030507     C                   EVAL      WSBNA4 = atoi(%SUBST(%TRIML(nraw):1:4))
061300030326     C                   MOVEL     VCAw          WSBDIV
061400010510     C*
061500010510     C                   WRITE     TNWSBM00
061501130423      *verifico se scrivere file cliente da banca
061503130417     C                   MOVEL     'CCA'         TBecod
061504130417     C                   MOVEL(p)  cdic          TBeke1
061505130426     C     KTbe          CHAIN     Tntbe01l
061506130417     c                   if        %found(tntbe01l)
061507130417     C                   exsr      file_vam
061510130417     c                   endif
061600010510     C                   ENDSR
061601130417     C*--------------------------------------------------------
061602130417     C* File-vam  SCRIVO IL TRACCIATO PER FILE cliente banca
061603130417     C*--------------------------------------------------------
061604130417     C     File_vam      BEGSR
061605130417     C*
061606130417     c                   clear                   wfvam000
061608130417     C                   Z-ADD     WSBLNP        vamLNP
061609130417     C                   Z-ADD     WSBLNA        vamLNA
061610130417     C                   Z-ADD     WSBAAS        vamAAS
061611130417     C                   Z-ADD     WSBNRS        vamNRS
061612130417     C                   Z-ADD     WSBNSP        vamNSP
061613130417     C*
061614130417     C                   Z-ADD     WSBCAS        vamCAS
061615130417     C                   Z-ADD     WSBidc        vamidc
061616130417     C                   Z-ADD     WSBABI        vamABI
061617130417     C                   Z-ADD     WSBCAB        vamCAi
061618130417     C                   Z-ADD     WSBNDP        vamNDP
061619130417     C                   Z-ADD     WSBDDP        vamDDP
061620130417     C                   Z-ADD     WSBCDI        vamksc
061621130430     C                   movel     wsbragcdi     VAMRSM
061624130417     C                   Z-ADD     RMNw          VAMRMN
061625130423     C                   movel     RMAw          VAMRMA
061631130417     C                   Z-ADD     mgsw          VAMMGS
061632130423     C                   movel     vcaw          VAMVCA
061636130417     C                   Z-ADD     dtew          VAMDTE
061637130423     C                   move      tpiw          VAMTPI
061640130423     C                   movel     rsdw          VAMRSD
061641130423     C                   move      prow          VAMPRO
061644130423     C                   Z-ADD     0             VAMNDPAS
061645130423      *
061647130424     C                   movel     prgw          VAMkey
061654130426     C*mm                EVAL      vamnra = atoi(%SUBST(%TRIML(nraw):1:4))
061655130426     c                   movel     nraw          vamnra
061656130430      * imposta ASS in quelli 1 a 1
061657130430     c                   if        vamkey = *blank
061658130430     c                   move      'ASS'         vamtrd
061659130430     c                   endif
061660130417     C*
061661130423     C                   WRITE     wfvam000
061662130424     c                   endsr
061663130424     C*--------------------------------------------------------
061664130424     C* File-vam  SCRIVO IL TRACCIATO PER FILE cliente banca
061665130424     C*--------------------------------------------------------
061666130424     C     riord_vam     BEGSR
061667130424      *
061668130426     c                   movel     '0'           kchiave
061669130426     c     kchiave       setll     wfvam01l
061670130424     c                   do        *hival
061671130424     c                   read      wfvam01l
061672130424     c                   if        %eof(wfvam01l)
061673130429     c                   exsr      agg_vam
061674130424     c                   leave
061675130424     c                   endif
061676130424      *memorizzo la prima chiave per le rotture
061677130424     c                   if        primo = *blank
061678130426     c                   move      'X'           primo             1
061679130426     c                   move      vamkey        kchiave          10
061680130429     c                   z-add     vamdte        exdte
061681130429     c                   move      vamnra        exnra
061682130429     c                   move      vamabi        exabi
061683130429     c                   move      vamcai        excai
061684130429     c                   move      vamksc        exksc
061685130429     c                   move      vamndp        exndp
061686130424     c                   z-add     1             conta
061687130429     c                   iter
061688130424     c                   endif
061689130424      *
061690130426     c                   if        vamkey <> kchiave
061691130424     c                   exsr      agg_vam
061692130426     c                   move      vamkey        kchiave
061693130429     c                   z-add     vamdte        exdte
061694130429     c                   move      vamnra        exnra
061695130429     c                   move      vamabi        exabi
061696130429     c                   move      vamcai        excai
061697130429     c                   move      vamksc        exksc
061698130429     c                   move      vamndp        exndp
061699130424     c                   z-add     1             conta
061700130424     c                   iter
061701130424     c                   endif
061702130429      *memorizzo codice cliente più basso di valore nell'ambito della stessa
061703130429      * chiave
061704130429     c                   if        vamksc < exksc
061705130429     c                   z-add     vamksc        exksc
061706130429     c                   move      vamndp        exndp
061707130429     c                   endif
061708130429      * verifica rottura assegno per attribuzione tipo rekord
061709130429      *
061710130429     c                   if        vamdte <> exdte  or
061711130429     c                             vamnra <> exnra  or
061712130429     c                             vamabi <> exabi  or
061713130429     c                             vamcai <> excai
061714130424     c                   add       1             conta
061715130424     c                   endif
061716130424     c
061717130424     c                   enddo
061718130424     C*
061719130424     c                   endsr
061720130424     C*--------------------------------------------------------
061721130424     C* File-vam  SCRIVO IL TRACCIATO PER FILE cliente banca
061722130424     C*--------------------------------------------------------
061723130426     C     agg_vam       BEGSR
061724130424      *
061725130430     c                   clear                   sped
061726130426     c     kchiave       setll     wfvam02l
061727130424     c                   do        *hival
061728130426     c     kchiave       reade     wfvam02l
061729130424     c                   if        %eof(wfvam02l)
061730130424     c                   leave
061731130424     c                   endif
061732130430     c                   if        £vamndp <> exndp
061733130429     c                   z-add     exndp         £vamndpas
061734130430     c                   endif
061735130424      *memorizzo la prima chiave per le rotture
061736130424     c                   if        conta > 1
061737130430      * memorizzo spedizione per verificare la rottura e capire se oltre
061738130430      * a multiassegno è anche multispedizione
061739130430     c                   if        sped  = *blank
061740130430     c                   move      £vamaas       exaas
061741130430     c                   move      £vamnsp       exnsp
061742130430     c                   move      £vamnrs       exnrs
061743130430     c                   move      £vamlnp       exlnp
061744130430     c                   move      'X'           sped
061745130430     c                   endif
061746130430      *se per multiassegno trova anche più spedizioni per le successive
061747130430      * inserisce tipo rekord di annullamento perchè non vengano poi sommati
061748130430     c                   if        £vamaas = exaas and
061749130430     c                             £vamnsp = exnsp and
061750130430     c                             £vamnrs = exnrs and
061751130430     c                             £vamlnp = exlnp
061752130429     c                   move      'ASS'         £vamtrd
061753130430     c                   else
061754130430     c                   move      'NUL'         £vamtrd
061755130430     c                   endif
061756130430      *monoassegno
061757130430     c                   else
061758130429     c                   move      'CAS'         £vamtrd
061759130424     c                   endif
061760130424     c
061761130424     C                   update    wfvam2
061762130424     c                   enddo
061763130424     C*
061764130424     c                   endsr
061765030317     C*--------------------------------------------------------
061800030331     C* srvab     SCRIVO un record per ogni spediz. sul FIVABCAF e FIVADCAF
061900030317     C*--------------------------------------------------------
062000030317     C     srvab         BEGSR
062100030331     c* FIVABCAF
062200030325     c                   clear                   fivab000
062300030317     C                   Z-ADD     102           vabfgs
062400030404     C                   z-add     1020004       VAbccm
062500030325     C                   z-add     102           VAbLNP
062600030325     C                   MOVEL     amgudt        VAbAAS
062700030325     c                   MOVE      amgudt        VAbmgs
062800030404     C                   EVAL      VAbNRS = 02
062900030326     C                   z-add     numlet        VAbNSP
063000030325     C                   MOVEL     'FW'          VAbcbo
063100030325     C                   z-add     poac          VAblna
063200030326     C                   eval      vabrsd = rsmc
063300030326     C                   eval      vabind = inmc
063400030326     C                   eval      vabcad = camc
063500030326     C                   eval      vablod = lomc
063600030326     C                   eval      vabprd = prmc
063700030326     C                   eval      vabnzd = nzmc
063800030404     C                   eval      vabpkb = 0,1
063900030404     C                   eval      vabvlb = 0,001
064000030331     C                   eval      vabnas = 'DOCUMENTI'
064100030331     C                   eval      vabncl = paga
064200030325     C                   Z-ADD     numlet        VAbrmn
064300030401     C                   MOVE      cdic          SERV89            4 0
064400030331     c                   if        serv89 <> 8888 and serv89 <> 9999
064500030521     C                   MOVEl     cdic          cod7              7
064600030512     C                   eval      vabnot= 'cliente tassazione ' + cod7
064700030331     c                   end
064800030331     c* il segnacollo iniziale è uguale al numero della lettera mentre
064900030331     c* i restanti segnacolli sono calcolati aggiungendo sempre 1 al
065000030331     c* numeratore delle lettere quindi il segnacollo finale è data
065100030331     c* dal numero della lettera + il numero delle pagine (cioè quanti
065200030331     c* segnacolli sono stati stampati) - 1 perchè già conteggiato nella
065300030331     c* prima lettera.
065400030331     c* es. spedizione: 1 segnacollo iniziale: 1 totale pag.: 3 segnacollo
065500030331     c* finale: 3. Spedizione successiva: 4 segnacollo iniziale: 4 totale
065600030331     c* pag.: 1 segnacollo finale 4
065700030325     C                   Z-ADD     numlet        VAbncd
065800030509     C                   if        paga = 1
065900030509     c                   clear                   vabnca
066000030509     c                   else
066100030331     C                   eval      VAbnca = numlet + paga - 1
066200030509     c                   end
066300030325     C                   z-add     zscc          VAbznc
066400030404     c                   eval      vabctm = '2W'
066500030325     C                   WRITE     fivab000
066600030331     c* FIVADCAF
066700030331     c                   clear                   fivad000
066800030404     C                   z-add     1020004       VAdccm
066900030401     C                   z-add     102           VAdLNP
067000030529     C                   z-add     102           VAdfgs
067100030401     C                   MOVEL     amgudt        VAdAAS
067200030404     C                   EVAL      VAdNRS = 02
067300030401     C                   z-add     numlet        VAdNSP
067400030401     C*?                 eval      VAdcdp =
067500030401     C                   eval      vadncl = paga
067600030401     C                   Z-ADD     numlet        VAdncd
067700030401     C                   eval      VAdnca = numlet + paga - 1
067800030331     C                   WRITE     fivad000
067900030317     C                   ENDSR
068000010510     C*
068100030325     C*---------------------------------------------------------------*
068200030326     C     sraggtab      BEGSR
068300030325     C*---------------------------------------------------------------*
068400030326     C* AGGIORNO numeratore lettera
068500030326     C                   MOVEL     '4B'          TBLCOD
068600030326     C                   MOVEL(p)  '1'           TBLKEY
068700030326     C     KTAB          CHAIN     TABEL00F                           20
068800030331     C  N20              z-add     pseg          §4BULN
068900030326     C  N20              MOVEL     DS4B          TBLUNI
069000030326     C  N20              UPDATE    TABEL
069100030326     C                   ENDSR
069200030326     C*---------------------------------------------------------------*
069300030326     C     *inzsr        BEGSR
069400030326     C*---------------------------------------------------------------*
069500030310     C     *ENTRY        PLIST
069600030310     C                   PARM                    KPJBA
069700030314     C*---------- RICERCA DITTA :
069800030314     C                   MOVEL     'SOC001'      TIPXSC
069900030314     C                   MOVEL     'SOC001'      TIPXSC
070000030314     C                   MOVEL     *BLANK        SOCXSC
070100030314     C                   EXSR      REPSOC
070200030314     C     RTNXSC        IFNE      '1'
070300030314     C                   MOVEL     XSOCDS        SOC001
070400030314     C                   MOVEL     xscrgs        RSUT             20
070500030314     c                   end
070600030310     C* ACQU. ULTIMO NUMERO LETTERA
070700030326     C                   Z-ADD     1             CODUT             1 0
070800030310     C                   MOVEL     '4B'          TBLCOD
070900030310     C                   MOVEL(p)  '1'           TBLKEY
071000030310     C     KTAB          CHAIN     TABEL00F                           20
071100030310     C  N20              MOVEL     TBLUNI        DS4B
071200030310     C   20              SETON                                        H1
071300030310     C*---------------------------------------------------------------*
071400030310     C* DEFINIZIONE CHIAVI
071500030310     C*
071600030310     C     KNSP          KLIST
071700030326     C                   KFLD                    AASw
071800030326     C                   KFLD                    LNPw
071900030326     C                   KFLD                    NRSw
072000030326     C                   KFLD                    NSPw
072100030310     C     KTAB          KLIST
072200030310     C                   KFLD                    CODUT
072300030310     C                   KFLD                    TBLCOD
072400030310     C                   KFLD                    TBLKEY
072401130426     C     Ktbe          KLIST
072402130426     C                   KFLD                    TBECOD
072403130426     C                   KFLD                    TBEKE1
072500030325     C*
072600030310     C     *LIKE         DEFINE    CSBNDP        NUMLET
072700030310     C*
072800030310     c                   move      udate         dtaeur
072900030310     c                   move      dtaeur        dtaiso
073000030310     C                   move      dtaiso        AMGUDT            8 0
073100030326     C                   MOVE      dtaeur        GMAUDT            8 0
073200030310      * FORZO LA DATA DEL JOB
073300030326     C     KDTJB         IFne      *blanks
073400030310     c                   move      kdtjb         com06             6 0
073500030326     c     *dmy          move      com06         dtaeur
073600030310     c                   move      dtaeur        dtaiso
073700030310     C                   move      dtaiso        AMGUDT            8 0
073800030326     C                   MOVE      dtaeur        GMAUDT            8 0
073900030310     C                   ENDIF
074000031114     c*  lettura tabella YCS
074100031114     c                   clear                   xatbds
074200031114     c                   move      '1'           xtbric
074300031114     c                   move      *blank        xtbkey
074400031114     c                   move      'YCS'         xtbcod
074500031114     c                   movel     XSCSOC        xtbkey
074600031114     c                   call      'XATB'
074700031114     c                   parm                    xatbds
074800031114     c                   if        xtberr = '0'
074900031114     c                   movel     xtbuni        angycs
075000031114     c                   else
075100031114     c                   clear                   angycs
075200031114     c                   end
075300031114     C* IMPOSTO dati anagrafici società
075400110510     c                   evalr     tbar= %trimr(xscrgs)
075500060516     c                   eval      tbaS1= $Intest1
075600060531     c                   eval      tba1= $Intest2
075700060531     c                   eval      tba2= $Intest4
075800031114     c                   if        amgudt < 20031231
075900031114     c                   eval      tba3= 'Capitale Sociale Euro' +
076000031114     c                             %TRIMr(%EDITC(YCSIMPCASO:'2'))+
076100031114     c                             ' int. vers.'
076200031114     c                   else
076300031114     c                   eval      tba4= 'Capitale Sociale Euro' +
076400031114     c                             %TRIMr(%EDITC(YCSIMPCASO:'2'))+
076500031114     c                             ' int. vers.'
076600031114     c                   seton                                        80
076700031114     c                   end
076800031114     c                   eval      tba= tbar
076900060516     c                   eval      tbas= tbas1
077000031114     c                   eval      tb1=tba1
077100031114     c                   eval      tb2= tba2
077200031114     c                   if        amgudt < 20031231
077300031114     c                   eval      tb3= tba3
077400031114     c                   else
077500031114     c                   eval      tb4= tba4
077600031114     c                   end
077700031114     c                   eval      tba5= 'info on-line'
077800120319     c                   eval      tba6= 'www.brt.it'
077801130430      * elimina file in wrkquery
077802130430     c                   movea     cmd2          coman
077803130430     c                   exsr      comandi
077900030310     C                   ENDSR
078000030314     C*----------------------------------------------------*
078100030314     C* Reperimento dati società
078200030314     C*----------------------------------------------------*
078300030314     C     REPSOC        BEGSR
078400030314     C*
078500030314     C                   CALLB     'XSOC'
078600030314     C                   PARM                    TIPXSC            6
078700030326     C                   PARM      '201'         SOCXSC            3
078800030314     C                   PARM                    CDSXSC            9 0
078900030314     C                   PARM                    MODXSC            3
079000030314     C                   PARM      *blanks       RTNXSC            1
079100030314     C                   PARM                    XSOCDS
079200030314     C                   PARM                    KPJBA
079300030314     c                   movel     xscrgs        prsm
079400030314     c                   movel     xscvia        pinm
079500030314     c                   movel     xsccap        pcam
079600030314     c                   movel     xsccit        plom
079700030314     c                   movel     xscprv        pprm
079800030314     C*
079900030314     C                   ENDSR
079901130430     c*-------------------------------------------------
079902130430     c     comandi       begsr
079903130430     c*-------------------------------------------------
079904130430      *
079905130430     C                   z-add     99            luncmd
079906130430     C                   CALL      'QCMDEXC'                            34
079907130430     C                   parm                    coman
079908130430     C                   parm                    luncmd
079909130430      *
079910130430     c                   endsr
080000000000     C*---------------------------------------------------------------*
080100941031     OTNCSB000  E            TNCSB
080200910618     O                       CSBBNA
080300910618     O                       CSBNDP
080400910618     O                       CSBDDP
080500931025     O                       CSBFTR
080600931025     O                       CSBDTR
080700130430**  CMD1
080800130430CRTDUPOBJ OBJ(WFVAM00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) DATA(*YES)
080900130430**  CMD2
081000130430DLTF FILE(wrkquery/WFVAM00F)
