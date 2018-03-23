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
001100130424     Fwfvam00f  O    E             DISK
001200130426     Fwfvam01l  if   E           k DISK    rename(wfvam000:wfvam1)
001300130429     Fwfvam02l  uf   E           k DISK    prefix(£) rename(wfvam000:wfvam2)
001400081125     Ftivgd00f  if a E             DISK    commit
001500030325     Fazorg01l  IF   E           K DISK
001600130426     Ftntbe01l  if   e           k disk
001700030325     Ftnsc76p   O    e             PRINTER
001800030314     d*
001900130430     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002000130430     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002100000000     D*----------------------------------------------------------------
002200030314     D KPJBA         E DS
002300030314     D DS4B          E DS
002400030314     D DS4C          E DS
002500081124     D tnvasds       E DS
002600081124     D trul28ds      E DS
002700030325     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002800030325     D CNACO         E DS                  EXTNAME(CNACO00F)
002900030325     D CNIND         E DS                  EXTNAME(CNIND00F)
003000030325     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003100030325     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003200030324     D dsbld7        E DS
003300030325     D soc001        E DS                  EXTNAME(xsoc001ds)
003400030314     D xsocds          DS          1000
003500031113     D xatbds        e DS
003600031113?     *  Tabella Y4Z
003700031113     D angycs        E DS                  extname(angycsds)
003800021112     ** atoi() -- Convert Character String to Integer
003900021112     D atoi            PR            10I 0
004000021112     D                                     EXTPROC('atoi')
004100021112     D  Char                           *
004200021112     D                                     VALUE
004300021112     D                                     OPTIONS(*STRING)
004400120227     D sass            S             16    DIM(5000)
004500120227     D comass          S             16
004600120301     D savsped         S             16
004700120222     d xx              s              3  0
004800120320     D sasst           S             16    DIM(5000)
004900120320     D comasst         S             16
005000120320     D savspedt        S             16
005100120320     d xxt             s              3  0
005200130424      *
005300130430     d coman           s             99
005400130430     d luncmd          s             15  5
005500130430      *
005600130424     d conta           s              3  0
005700130430     d sped            s              1
005800160419     d millecinque     s              4  0
005900160413     d wpnsp           s                   like(pnsp)
006000130429     d exdte           s                   like(vamdte)
006100130429     d exnra           s                   like(vamnra)
006200130429     d exabi           s                   like(vamabi)
006300130429     d excai           s                   like(vamcai)
006400130429     d exksc           s                   like(vamksc)
006500130429     d exndp           s                   like(vamndp)
006600130430     d exaas           s                   like(vamaas)
006700130430     d exnsp           s                   like(vamnsp)
006800130430     d exnrs           s                   like(vamnrs)
006900130430     d exlnp           s                   like(vamlnp)
007000130424     d
007100030314     d* tracciati restituiti da sql                                    *****
007200030314     D tncsbc          DS
007300030325     D  tfac                          3  0
007400030325     D  poac                          3  0
007500030325     D  zscc                          2  0
007600030325     D  cdic                               like(csbcdi)
007700030325     D  rsmc                         35
007800030325     D  inmc                         35
007900030325     D  lomc                         35
008000030325     D  camc                          9
008100030325     D  prmc                          2
008200030325     D  nzmc                          3
008300030325     D  rscc                         35
008400030325     D  indc                         35
008500030325     D  lodc                         35
008600030325     D  cadc                          9
008700030325     D  prdc                          2
008800030325     D  nzdc                          3
008900030327     D  CONc                          7  0
009000030325     D tncsbw          DS
009100030310     D                                     OCCURS(20)
009200030310     D                                     INZ
009300030325     D  aasw                               like(csbaas)
009400030314     D  nrsw                               like(csbnrs)
009500030325     D  nspw                               like(csbnsp)
009600030314     D  mgsw                               like(csbmgs)
009700030314     D  casw                               like(csbcas)
009800030314     D  vcaw                               like(csbvca)
009900030325     D  nraw                               like(csbnra)
010000030314     D  abiw                               like(csbabi)
010100030314     D  cabw                               like(csbcab)
010200030325     D  rmnw                               like(csbrmn)
010300030325     D  rmaw                               like(csbrma)
010400030325     D  rsdw                               like(csbrsd)
010500030325     D  lnpw                               like(csblnp)
010600051118     D  lnaw                               like(csblna)
010700051118     D  prow                               like(csbpro)
010800030325     D  dtew                               like(csbdte)
010900030325     D  tpiw                               like(csbtpi)
011000030325     D  ndpw                               like(csbndp)
011100030325     D  ddpw                               like(csbddp)
011200030325     D  fpcw                               like(csbfpc)
011300030325     D  dtaw                               like(csbdta)
011400030326     D  RRNw                         15  0
011500120222     D  prgw                               like(£csbprg)
011600120222     D  idcw                               like(£csbidc)
011700120222     D  rgfw                               like(£csbrgf)
011800030325     D poas            S                   like(poac)
011900081125     D var             S                   like(vgdksu)
012000120222     D pnas1           S                   like(pnca)
012100120222     D pnas2           S                   like(pnca)
012200030325     D I               S              5I 0
012300030310     D dtaiso          S               d
012400030310     D dtaeur          S               d   datfmt(*eur)
012500030326     Dnoctrscos        s              1
012600030326     Desito            s              1
012700030822     c* ds per sottocampare il barcode della spedizione
012800030822     D ds_pis          DS
012900030822     D  ds_pisaas                     2  0
013000030822     D  ds_pislnp                     3  0
013100030822     D  ds_pisnrs                     2  0
013200030822     D  ds_pisnsp                     7  0
013300030526     DDATACOM          DS
013400030526     D GGCOM                          2  0
013500030526     D MMCOM                          2  0
013600030526     D AACOM                          4  0
013700120301     DspedCOM          DS
013800120301     D asCOM                          4  0
013900120301     D lnCOM                          3  0
014000120301     D nrCOM                          2  0
014100120301     D nsCOM                          7  0
014200060516     d $Intest1        c                   const('Sede Operativa ed Am-
014300060516     d                                     ministrativa:')
014400060531     d $Intest2        c                   const('via E. Mattei, 42 -
014500060531     d                                     - 40138 BOLOGNA')
014600060531     d $Intest4        c                   const('P.IVA 04507990150')
014700021112
014800030314     D* convertitore in moneta di conto
014900030314     D/COPY *LIBL/SRCCPY,PJX002PR
015000030314     c*****************************************************************
015100081127     c*
015200030310     C* MI POSIZIONO CON TPI=M + BNA=0  + CDI
015300030324     c* mi seleziona un cliente alla volta
015400030310     C/EXEC SQL
015500030325     C+ DECLARE TNCSBC CURSOR FOR SELECT DISTINCT CSBtfa, csbpoa, csbzsc,
015600030327     C+ CSBCDI, CSBRSM, CSBINM, CSBLOM, CSBCAM, CSBPRM, CSBNZM, CSBRSC,
015700030331     C+ CSBIND, CSBLOD, CSBCAD, CSBPRD, CSBNZD, COUNT(*) FROM tncsbm1k
015800030331     C+ GROUP BY CSBtfa, csbpoa, csbzsc, CSBCDI, CSBRSM, CSBINM, CSBLOM,
015900030331     C+ CSBCAM, CSBPRM, CSBNZM, CSBRSC, CSBIND, CSBLOD, CSBCAD, CSBPRD,
016000120221     C+ CSBNZD ORDER BY CSBtfa, csbpoa, CSBCDI
016100030310     C/END-EXEC
016200030314
016300030314     C/EXEC SQL
016400030314     C+ OPEN tncsbc
016500030314     C/END-EXEC
016600030314
016700030325     C                   DOU       sqlcod <> 0
016800030314
016900030314     C/EXEC SQL
017000030326     C+ FETCH NEXT FROM TNCSBC INTO : tncsbc
017100030314     C/END-EXEC
017200030314
017300030314     C                   SELECT
017400030314     C                   WHEN      SQLCod = 100
017500030314     C                   WHEN      SQLCod < 0
017600030314     C                   EVAL      *INH1 = *ON
017700030314     C                   OTHER
017800030314     c* stampa testata e apre membro
017900030314     c                   exsr      leggi
018000030314     C                   ENDSL
018100030314
018200030314     C                   ENDDO
018300030314     C/EXEC SQL
018400030314     C+ CLOSE tncsbc
018500030314     C/END-EXEC
018600130424     c                   exsr      riord_vam
018700030310
018800030325     C* chiudo BARCODE
018900030325     C                   clear                   trul28ds
019000030325     c                   eval      i28tla = 'C'
019100030822     c                   call      'TRUL28R1'
019200030325     c                   parm                    trul28ds
019300030314     C*
019400081127     c                   clear                   kpjbu
019500130430     c                   feod      wfvam00f
019600130430     c                   movea     cmd1          coman
019700130430     c                   exsr      comandi
019800030314     C                   SETON                                        LR
019900030314     C*--------------------------------------------------------
020000030314     C* leggi           elabora record letto                  *
020100030314     C*--------------------------------------------------------
020200030314     C     leggi         BEGSR
020300030327     c* calcolo totale pagine
020400030331     c     conc          div       20            paga
020500030327     c                   mvr                     resto             7 0
020600030327     c                   if        resto > 0
020700030331     c                   add       1             paga
020800030327     c                   end
020900030519     c                   z-add     paga          pcol
021000030519     c                   z-add     1             pagda
021100030327     c                   seton                                        60
021200081125     C* CONTROLLA SE CLIENTE CON SCARICO DISCHETTO C/A MITT.
021300081125     C                   SETOFF                                       50
021400090127     c                   movel     *all'0'       var
021500081125     c                   clear                   ds4c
021600081125     C                   MOVEL     '4C'          TBLCOD
021700081125     C                   MOVEL(p)  cdic          TBLKEY
021800081125     C     KTAB          CHAIN     TABEL                              90
021900081125     c                   if        %found(tabel00f)
022000081125     C                   MOVEL     TBLUNI        DS4C
022100081125     C     §4CFLG        COMP      'S'                                    50
022200081125     C* COD. CLIENTE UNIFIC. X APERTURA MEMBRO  DA TABELLA
022300090127     C     §4CCKS        IFEQ      *ZEROS
022400090127     C                   MOVE      cdic          VAR
022500081125     C                   ELSE
022600090127     C                   MOVE      §4CCKS        VAR
022700081125     C                   END
022800081125     c                   end
022900030317     c* per ogni cliente letto raggruppo max 20 record (perchè x ogni 20
023000030401     c* record letti devo fare una lettera).
023100030317     c                   exsr      srsql
023200030310     C                   ENDSR
023300120222     C*--------------------------------------------------------
023400120222     c* conta spedizioni e assegni
023500120222     C*--------------------------------------------------------
023600120222     C     srconta       BEGSR
023700120222     c                   clear                   pnca
023800120222     c                   clear                   pnas
023900120222     c                   clear                   pnas1
024000120222     c                   clear                   pnas2
024100030317     C/EXEC SQL
024200120222     C+ SELECT Count(*) into :pnca
024300120222     C+ from (SELECT csbaas, csblnp, csbnrs, csbnsp
024400120222     C+ FROM tncsbm1k WHERE csbcdi = :cdic and
024500030401     C+ csbtfa = :tfac and csbpoa = :poac and CSBzsc = :zscc and CSBRSM =
024600030401     C+ :rsmc and CSBINM = :inmc and CSBLOM = :lomc and csbcam = :camc
024700030401     C+ and CSBPRM = :prmc and CSBNZM = :nzmc and CSBRSC = :rscc and
024800030401     C+ CSBIND = :indc and CSBLOD = :lodc and CSBCAD = :cadc and CSBPRD =
024900120222     C+ :prdc and CSBNZD = :nzdc GROUP BY csbaas, CSBlnp, CSBNRS, CSBnsp)
025000120222     C+ as pippo
025100030317     C/END-EXEC
025200120222     c* assegni singoli
025300120222     C/EXEC SQL
025400120222     C+ SELECT Count(*) into :pnas1
025500120222     C+ FROM tncsbm1k WHERE csbcdi = :cdic and
025600120222     C+ csbtfa = :tfac and csbpoa = :poac and CSBzsc = :zscc and CSBRSM =
025700120222     C+ :rsmc and CSBINM = :inmc and CSBLOM = :lomc and csbcam = :camc
025800120222     C+ and CSBPRM = :prmc and CSBNZM = :nzmc and CSBRSC = :rscc and
025900120222     C+ CSBIND = :indc and CSBLOD = :lodc and CSBCAD = :cadc and CSBPRD =
026000120222     C+ :prdc and CSBNZD = :nzdc and csbprg = ' '
026100120222     C/END-EXEC
026200120222     c* assegni multipli
026300120222     C/EXEC SQL
026400120222     C+ SELECT Count(*) into :pnas2
026500120222     C+ from (SELECT csbnra, csbabi, csbcab, csbdte
026600120222     C+ FROM tncsbm1k WHERE csbcdi = :cdic and
026700120222     C+ csbtfa = :tfac and csbpoa = :poac and CSBzsc = :zscc and CSBRSM =
026800120222     C+ :rsmc and CSBINM = :inmc and CSBLOM = :lomc and csbcam = :camc
026900120222     C+ and CSBPRM = :prmc and CSBNZM = :nzmc and CSBRSC = :rscc and
027000120222     C+ CSBIND = :indc and CSBLOD = :lodc and CSBCAD = :cadc and CSBPRD =
027100120227     C+ :prdc and CSBNZD = :nzdc and csbprg <> ' '
027200120227     C+ GROUP BY csbnra, csbabi, csbcab, csbdte)
027300120222     C+ as pippo1
027400120222     C/END-EXEC
027500120222     c                   eval      pnas = pnas1 + pnas2
027600120222     c                   endsr
027700120222     C*--------------------------------------------------------
027800120222     C* srsql sql per clienti NO 8888 o 9999                   *
027900120222     C*--------------------------------------------------------
028000120222     C     srsql         BEGSR
028100120222     c                   exsr      srconta
028200120222     c                   clear                   wsborder
028300120222     c                   clear                   sass
028400120222     c                   clear                   xx
028500120320     c                   clear                   sasst
028600120320     c                   clear                   xxt
028700120222     C/EXEC SQL
028800120222     C+ DECLARE TNCSB2 CURSOR FOR SELECT CSBAAS, CSBNRS, CSBNSP, CSBMGS,
028900120222     C+ CSBCAS, CSBVCA, CSBNRA, CSBABI, CSBCAB, CSBRMN, CSBRMA, CSBrsd,
029000120222     C+ CSBLNP, CSBLNA, CSBPRO, CSBDTE, CSBTPI, CSBNDP, CSBDDP, CSBFPC,
029100120222     C+ csbdta, RRN(TNCSBM1K), csbprg, csbidc, csbrgf FROM tncsbm1k WHERE
029200120222     C+ csbcdi = :cdic and csbtfa = :tfac and csbpoa = :poac and CSBzsc =
029300120222     C+ :zscc and CSBRSM = :rsmc and CSBINM = :inmc and CSBLOM = :lomc
029400120222     C+ and csbcam = :camc and CSBPRM = :prmc and CSBNZM = :nzmc and
029500120222     C+ CSBRSC = :rscc and CSBIND = :indc and CSBLOD = :lodc and CSBCAD =
029600120227     C+ :cadc and CSBPRD = :prdc and CSBNZD = :nzdc ORDER BY csbprg,
029700120222     C+ CSBCAS, CSBNRA, CSBABI, CSBCAB FOR READ ONLY OPTIMIZE FOR 20 ROWS
029800120222     C/END-EXEC
029900030317
030000030317     C/EXEC SQL
030100030325     C+ OPEN tncsb2
030200030317     C/END-EXEC
030300030317
030400030325     C                   DOU       sqlcod <> 0
030500030317
030600030317     C/EXEC SQL
030700030325     C+ FETCH NEXT FROM TNCSB2 FOR 20 ROWS INTO :tncsbw
030800030317     C/END-EXEC
030900030317
031000030317     C                   SELECT
031100030317     C                   WHEN      SQLCod = 100
031200030317     C                   WHEN      SQLCod < 0
031300030317     C                   EVAL      *INH1 = *ON
031400030317     C                   OTHER
031500030317     C                   EXSR      stampa
031600030317     C                   ENDSL
031700030317
031800030317     C                   ENDDO
031900030317
032000030317     C/EXEC SQL
032100030325     C+ CLOSE tncsb2
032200030317     C/END-EXEC
032300030331     C* AGGIORNO NUMERATORE LETTERA
032400030331     C                   EXSR      SRAGGTAB
032500030331     C                   COMMIT
032600030317     c                   endsr
032700030314     C*--------------------------------------------------------
032800030314     C* stampa          elabora il dettaglio                  *
032900030314     C*--------------------------------------------------------
033000030314     C     stampa        BEGSR
033100030331     c* calcolo il totale della lettera (sqler3 è il numero dei record
033200030331     c* letti)
033300160121     c                   movea     '00000'       *in(01)
033400160120     c                   clear                   outndp1           7 0
033500160120     c                   clear                   outndp2           7 0
033600160120     c                   clear                   outndp3           7 0
033700160121     c                   clear                   outndp4           7 0
033800160121     c                   clear                   outndp5           7 0
033900160120     c                   clear                   outndpecc         4
034000030331     C                   CLEAR                   I
034100030331     C                   CLEAR                   pimp
034200030331     C                   DOU       I = SQLEr3
034300030331     C                   EVAL      I = I + 1
034400030331     C     I             OCCUR     tncsbw
034500120320     c                   setoff                                       89
034600160118     c* per multiassegno
034700160119     c                   clear                   outcdi            7 0
034800160119     c                   clear                   outndp            7 0
034900160119     c                   clear                   outddp            8 0
035000120320     c                   if        prgw <> ' '
035100160118     c* verifico il cliente e lettera nella quale sono gli assegni
035200160118     c                   call      'TNSC76R2'
035300160118     c                   parm                    prgw
035400160118     c                   parm                    outcdi
035500160118     c                   parm                    outndp
035600160118     c                   parm                    outddp
035700160120     c                   if        outddp <> 0
035800160119     c                   move      outddp        dtaiso
035900160119     C                   MOVE      Dtaiso        dtaeur
036000160119     C                   MOVE      Dtaeur        outddpx
036100160119     c                   else
036200160119     c                   clear                   outddpx
036300160119     c                   end
036400160120     c                   if        outndp <> 0
036500160120     c                   select
036600160120     c                   when      outndp1 = 0
036700160120     c                   eval      outndp1 = outndp
036800160121     c                   seton                                        01
036900160120     c                   when      outndp2 = 0 and outndp <> outndp1
037000160120     c                   eval      outndp2 = outndp
037100160121     c                   seton                                        02
037200160120     c                   when      outndp3 = 0 and outndp <> outndp1
037300160120     c                             and outndp <> outndp2
037400160120     c                   eval      outndp3 = outndp
037500160121     c                   seton                                        03
037600160121     c                   when      outndp4 = 0 and outndp <> outndp1
037700160121     c                             and outndp <> outndp2
037800160121     c                             and outndp <> outndp3
037900160121     c                   eval      outndp4 = outndp
038000160121     c                   seton                                        04
038100160121     c                   when      outndp5 = 0 and outndp <> outndp1
038200160121     c                             and outndp <> outndp2
038300160121     c                             and outndp <> outndp3
038400160121     c                             and outndp <> outndp4
038500160121     c                   eval      outndp5 = outndp
038600160121     c                   seton                                        05
038700160121     c                   when      outndp <> outndp1
038800160120     c                             and outndp <> outndp2
038900160121     c                             and outndp <> outndp3
039000160121     c                             and outndp <> outndp4
039100160121     c                             and outndp <> outndp5
039200160120     c                   eval      outndpecc= 'ecc.'
039300160120     c                   endsl
039400160120     c                   end
039500160118     c* controllo di sommare la spedizione una volta sola con il
039600160118     c* primo assegno
039700120628     c                   movel     aasw          comaast           4
039800120320     c                   movel     lnpw          comlnpt           3
039900120320     c                   movel     nrsw          comnrst           2
040000120320     c                   movel     nspw          comnspt           7
040100120320     c                   eval      comasst=comaast+comlnpt+comnrst+comnspt
040200120320     c     comasst       lookup    sasst                                  89
040300120320     c                   if        not *in89
040400120320     c                   add       1             xxt
040500120320     c                   eval      sasst(xxt) = comasst
040600120320     c                   else
040700120320     c                   iter
040800120320     c                   end
040900120320     c                   end
041000030331     c                   if        vcaw <> 'EUR'
041100030331     C                   RESET                   PJX00202DS
041200030331     C                   MOVEL     vcaw          Divisa0202
041300030331     C                   MOVEL     'EUR'         DivMC0202
041400030331     C                   Z-ADD     casw          ImporD0202
041500030331     c                   move      amgudt        dtaiso
041600030331     C                   MOVE      Dtaiso        DtRif0202
041700030331     C                   MOVEL     *blank        Esito
041800030331     C                   Move      *On           NoCtrScos
041900030331     C* chiamata per reperire il cambio
042000030331     C                   CALLP     X0202TestC(PJX00202DS :
042100030331     C                                            %Size(PJX00202DS):
042200030331     C                                            *Off             :
042300030331     C                                            Esito            :
042400030331     C                                            NoCtrScos        :
042500030331     C                                            *Omit            :
042600030331     C                                            %Size(ANDVS00F)  :
042700030331     C                                            %Size(ANDVX00F)  :
042800030331     C                                            ANDVS00F         :
042900030331     C                                            ANDVX00F          )
043000030331     C* se non ci sono stati errori
043100030331     C                   IF        Esito = *off
043200030331     C                   add       ImporM0202    pimp
043300030331     C                   ENDIF
043400030331     c                   else
043500030331     c                   add       casw          pimp
043600030331     c                   end
043700030331     c                   enddo
043800030331     C* STAMPO TESTATA 1° volta
043900030331     c                   if        *in60
044000030314     C                   EXSR      WRITES
044100030327     c                   setoff                                       60
044200030331     c                   else
044300030331     c                   exsr      write1
044400030331     c                   end
044500030314     ** leggo 20 record e stampo le righe ricevute.
044600030326     c                   if        o28err = *blanks
044700030314     C                   CLEAR                   I
044800030314     C                   DOU       I = SQLEr3
044900030314     C                   EVAL      I = I + 1
045000030325     C     I             OCCUR     tncsbw
045100030314     C* STAMPA DETTAGLIO RIGA DI LETTERA CONTRASSEGNI
045200030314     C                   EXSR      WRIRIG
045300030314     C* SCRIVE RECORD SU MEMBRO SCARICO C/A
045400030314     C   50              EXSR      WRTTCC
045500030314     C* SCRIVE RECORD SU MEMBRO PER SCARICO SU FILE PC
045600030314     C                   EXSR      WRTTPC
045700030314     C* DELETO RECORD APPENA SCRITTO PRIMA DI LEGGERE IL SUCCESSIVO
045800030326     C     rrnw          DELETE    TNCSBW00
045900030314     C                   ENDDO
046000030324     c                   end
046100030314     C                   ENDSR
046200030331     C*--------------------------------------------------------
046300030331     C* WRITES    ESEGUE STAMPA INTESTAZIONE LETTERA 1° pagina*
046400030331     C*--------------------------------------------------------
046500030331     C     WRITES        BEGSR
046600030331     c                   z-add     §4buln        numlet
046700030324     C                   ADD       1             NUMLET
046800030411     C* REPERISCE BARCODE della spedizione
046900030822     C                   clear                   trul28ds
047000030822     C                   clear                   ds_pis
047100030822     c                   eval      i28mod = 'BAR'
047200030822     c                   move      *year         ds_pisaas
047300030822     c                   eval      ds_pislnp = 102
047400030822     c                   eval      ds_pisnrs = 02
047500030822     c                   eval      ds_pisnsp = numlet
047600030822     c                   eval      i28cod = ds_pis
047700030822     c                   call      'TRUL28R1'
047800030822     c                   parm                    trul28ds
047900030411     c* se reperito barcode imposto i campi in stampa
048000030822     c                   if        o28err = *blanks
048100030822     c                   eval      pbarSPE = o28cod
048200030417     c                   eval      pfvf = 'DOCUMENTI'
048300030325     c* terminal arrivo
048400030325     c                   eval      ptfp = tfac
048500030325     c* po arrivo
048600030325     c                   eval      plna = poac
048700030325     c* zona
048800030325     c                   eval      pznc = zscc
048900030325     c* Bartolini
049000030325     c                   eval      prsm = xscrgs
049100030325     c                   eval      pfir = xscrgs
049200030325     c* data spedizione (oggi)
049300030325     c                   eval      pdsp = gmaudt
049400030325     c* po partenza fisso 102
049500030325     c                   eval      plnp = 102
049600030519     c                   eval      plnps = 102
049700030325     c* spedizione=numero lettera
049800030404     c                   eval      pnrs = 02
049900030519     c                   eval      pnrss = 02
050000030325     c                   eval      pnsp = numlet
050100030325     c* reperisco l'indirizzo della linea di arrivo
050200030325     c                   if        poac <> poas
050300030325     c     poac          chain     azorg01l
050400030325     c                   eval      pinm = orgind
050500030326     c                   clear                   pcam
050600030326     c                   move      orgcpf        pcam
050700030325     c                   eval      plom = orgloc
050800051118     c*                  eval      ppol = orgloc
050900030325     c                   eval      ppot = orgtel
051000030325     c                   eval      pprm = orgpro
051100030325     c                   eval      poas = poac
051200030325     c                   end
051300030325     c* numero spedizioni
051400120222     c*                  eval      pnca = conc
051500030325     c* dati di spedizione del cliente/destinatario
051600030325     c                   eval      prsd = rsmc
051700030325     c                   eval      pksc = cdic
051800030325     c                   eval      pind = inmc
051900030325     c                   eval      pcad = camc
052000030325     c                   eval      plod = lomc
052100030325     c                   eval      pprd = prmc
052200030325     c* dati abituali del cliente/destinatario
052300030325     c                   eval      prsc = rscc
052400030325     c                   eval      pinc = indc
052500030325     c                   eval      pcac = cadc
052600030325     c                   eval      ploc = lodc
052700030325     c                   eval      pprc = prdc
052800030325     c*
052900030325     c                   move      gmaudt        dtaeur
053000030326     c                   move      dtaeur        com10            10
053100030326     c                   eval      pdta = 'Bologna, lì ' + com10
053200030331     C* Stampa testata lettera
053300030331     C                   EXSR      write1
053400030331     C* SCRIVO IL RECORD SU FIVABCAF
053500030403     c* se reperito barcode imposto i campi in stampa
053600030403     c                   if        o28err = *blanks
053700030331     C                   EXSR      srvab
053800030324     c                   end
053900030822     c                   end
054000030314     c*
054100910619     C                   ENDSR
054200030331     C*--------------------------------------------------------
054300030331     C* WRITE1    ESEGUE STAMPA INTESTAZIONE LETTERA pag. successive
054400030331     C*--------------------------------------------------------
054500030331     C     WRITE1        BEGSR
054600030331     c* segnacollo se prima volta = al numero lettera se no aggiungo 1
054700030331     c                   if        *in60
054800030331     c                   z-add     numlet        pseg
054900030331     c                   else
055000030331     c                   add       1             pseg
055100030331     c                   add       1             pagda
055200030331     c                   end
055300030513     c                   movel(p)  pseg          prif
055400030403     C* REPERISCE BARCODE del segnacollo
055500030403     C                   clear                   trul28ds
055600030403     C                   clear                   dsbld7
055700030403     c                   eval      i28mod = 'BAR'
055800030403     c                   eval      §lclnp = 102
055900030403     c                   eval      §lclna = poac
056000030404     c                   eval      §lcnrs = 02
056100030403     c                   eval      §lcncd = pseg
056200030403     c                   eval      §lczsc = zscc
056300030403     c                   eval      i28cod = dsbld7
056400030822     c                   call      'TRUL28R1'
056500030403     c                   parm                    trul28ds
056600030403     c* se reperito barcode imposto i campi in stampa
056700030403     c                   if        o28err = *blanks
056800030411     c* barcode SEGNACOLLO
056900030411     c                   eval      pbarSGN = o28cod
057000030331     C** TESTATA LETTERA
057100160413     c* forzo chiusura file di spool per limite 500 pagine in modo da
057200160413     c* poter spezzare lo spool
057300160419     c                   if        millecinque > 1500 and
057400160413     c                             pnsp <> wpnsp
057500160413     c                   close     tnsc76p
057600160419     c                   z-add     0             millecinque
057700160413     c                   open      tnsc76p
057800160413     c                   endif
057900160413      * memorizzo la spedizione che sto stampando e incremento il numero
058000160413      * delle lettere stampate
058100160413     c                   z-add     pnsp          wpnsp
058200160419     c                   add       1             millecinque
058300030417     C                   write     prttxt
058400030508     C                   write     prtbar
058500030508     C                   write     prtbox
058600030331     C                   write     prttx1
058700030331     C                   write     prttx9
058800160120     c* ho una solo lettera che contiene tutti i multiassegni
058900160120     c                   if        outndp1 <> 0 and outndp2 = 0
059000160119     C                   write     prttx10
059100160119     c                   end
059200160120     c* ho una solo lettera che contiene tutti i multiassegni
059300160120     c                   if        outndp2 <>0
059400160120     C                   write     prttx11
059500160120     c                   end
059600030331     C                   write     prtest
059700030403     c                   end
059800030331     C                   ENDSR
059900000000     C*--------------------------------------------------------
060000030314     C* WRIRIG    ESEGUE STAMPA Dettaglio                     *
060100000000     C*--------------------------------------------------------
060200000000     C     WRIRIG        BEGSR
060300000000     C* DATA DI SPEDIZIONE
060400120301     C                   MOVE      AASw          AsCOM
060500120301     C                   MOVE      lnpw          lnCOM
060600120301     C                   MOVE      nrsw          nrCOM
060700120301     C                   MOVE      nspw          nsCOM
060800120301     C                   MOVE      AASw          AACOM
060900030526     C                   MOVEl     MGSw          MMCOM
061000030526     C                   MOVE      MGSw          GGCOM
061100030526     C                   MOVEL     DATACOM       PDDS
061200030327     C                   eval      pdnr = nrsw
061300030327     C                   eval      pdns = nspw
061400030326     C                   eval      pdlp = lnpw
061500051118     C                   eval      pprv = proW
061600030326     C                   eval      pdde = RSDw
061700030327     C                   eval      pdrn = rmnw
061800030326     C                   eval      pdra = Rmaw
061900030326     C                   z-add     casw          pdim
062000030326     C                   eval      pddi = vcaw
062100030715     C                   z-add     ABIw          pdab
062200030715     C                   z-add     CABw          pdca
062300030326     C                   EVAL      pdna = atoi(nraw)
062400030507     C                   EVAL      pdna = atoi(%SUBST(%TRIML(nraw):1:4))
062500120628     c* evidenzio i multiassegni con un "*" se codice cli.mittente diverso
062600120222     c                   if        prgw = *blanks
062700120222     c                   clear                   ppiu
062800120222     c                   else
062900120628     c* A fronte di un multiassegno verifico se ho codici clienti diversi
063000120313     c                   clear                   ppiu
063100120628     c                   clear                   outnrcli          2 0
063200120628     c                   call      'TNSC76R1'
063300120628     c                   parm                    prgw
063400120628     c                   parm                    outnrcli
063500120628     c                   if        outnrcli > 1
063600120628     c                   eval      ppiu = '*'
063700120222     c                   end
063800120628     c                   end
063900120301     c* stampo i rif. spedizione una volta sola,  l'archivio dovrebbe già
064000120301     c* essere ordinato per spedizione in caso di multiassegni
064100120301     c                   if        spedcom <> savsped
064200030325     C                   write     prtesd
064300120301     c                   eval      savsped = spedcom
064400120301     c                   else
064500120301     C                   write     prtesda
064600120301     c                   end
064700941031     C* AGGIORNA DATI SU TNCSB
064800030314     C     KNSP          CHAIN     TNCSB03L                           41
064900910619     C                   Z-ADD     *ALL'9'       CSBBNA
065000941031     C                   Z-ADD     AMGUDT        CSBDDP
065100030521     C                   Z-ADD     pseg          CSBNDP
065200030512     C                   Z-ADD     AMGUDT        DDPw
065300030521     C                   Z-ADD     pseg          NDPw
065400931025     C                   MOVE      *ZERO         CSBDTR
065500931025     C                   MOVE      ' '           CSBFTR
065600941031     C                   EXCEPT    TNCSB
065700000000     C*
065800000000     C                   ENDSR
065900910923     C*--------------------------------------------------------
066000910923     C* WRITCC    SCRIVO IL TRACCIATO C/A PER I CLIENTI       *
066100910923     C*--------------------------------------------------------
066200910923     C     WRTTCC        BEGSR
066300081124     c                   clear                   tnvasds
066400120321     c                   setoff                                       88
066500120222     c* controllo di scrivere la spedizione una volta sola con il
066600120222     c* primo assegno nel caso di multiassegni
066700120222     c                   if        prgw <> ' '
066800120628     c                   movel     aasw          comaas            4
066900120227     c                   movel     lnpw          comlnp            3
067000120227     c                   movel     nrsw          comnrs            2
067100120227     c                   movel     nspw          comnsp            7
067200120227     c                   eval      comass =  comaas+comlnp+comnrs+comnsp
067300120222     c     comass        lookup    sass                                   88
067400120222     c                   else
067500120321     c                   setoff                                       88
067600120222     c                   end
067700930927     C*
067800120222     c                   if        not *in88
067900120222     c                   if        prgw <> ' '
068000120222     c                   add       1             xx
068100120222     c                   eval      sass(xx) = comass
068200120222     c                   end
068300030326     C                   eval      vaslnp = LNPw
068400030326     C                   MOVEL     AASw          VASAAS
068500030326     C                   MOVEL     NRSw          VASNRS
068600030326     C                   MOVEL     NSPw          VASNSP
068700030326     C                   MOVEL     LNAw          VASLNA
068800030326     C                   MOVEL     RMNw          VASRMN
068900030326     C                   MOVEL     RMAw          VASRMA
069000030326     C                   Z-ADD     CASw          VASCAS
069100030326     C                   MOVEL     VCAw          VASVCA
069200030326     C                   MOVEL     NRAw          VASNRA
069300030326     C                   Z-ADD     DTEw          VASDTE
069400030326     C                   MOVEL     TPIw          VASTPI
069500030326     C                   Z-ADD     ABIw          VASABI
069600030326     C                   Z-ADD     CABw          VASCAI
069700030326     C                   MOVEL     NDPw          VASNDP
069800030326     C                   MOVEL     DDPw          VASDDP
069900030326     C                   MOVEL     FPCw          VASFPC
070000030325     C                   MOVEL     CDIc          VASKSC
070100910923     C* SE PAGAMENTO CON BONIFICO :
070200910923     C* CHAIN SU CNCLP PER PRENDERE C/C BONIFICO CONTRASSEGNO
070300941031     C                   MOVEL     *BLANK        VASCCB
070400030326     C     FPCw          IFEQ      '2'
070500981007     C                   CLEAR                   TIBS69
070600981007     C                   CLEAR                   CNACO
070700981007     C                   CLEAR                   CNIND
070800981007     C                   CLEAR                   CNCLP
070900981007     C                   CLEAR                   FNCLS
071000981007     C                   MOVEL     ' '           I69TLA
071100981007     C                   MOVEL     KNSIF         I69SIF
071200981007     C                   Z-ADD     0             I69KCC
071300030325     C                   Z-ADD     cdic          I69KAC
071400030325     C                   Z-ADD     cdic          I69KCP
071500981007     C                   CALL      'TIBS69R'
071600981007     C                   PARM                    TIBS69
071700981007     C                   PARM                    CNACO
071800981007     C                   PARM                    CNIND
071900981007     C                   PARM                    CNCLP
072000981007     C                   PARM                    FNCLS
072100981008     C     O69ERR        IFEQ      *BLANK
072200981007     C                   MOVEL     CLPCCB        VASCCB
072300981007     C                   MOVEL     CLPABI        VASABP
072400981007     C                   MOVEL     CLPCAB        VASCAP
072500981007     C                   END
072600910923     C                   ELSE
072700030326     C                   Z-ADD     ABIw          VASABP
072800030326     C                   Z-ADD     CABw          VASCAP
072900910923     C                   END
073000081125     c* scrivo file tivgd00f
073100081124     c                   clear                   tivgd000
073200081124     C                   eval      vgdDTA = %subst(TNVASDS:1:%size(TNVASDS))
073300081124     c                   eval      vgdtip = 'VS'
073400081125     c                   eval      vgdksu = var
073500081125     c                   eval      vgdtsc = §4CTSD
073600081124     c                   eval      vgdpgm = 'TNSC76R'
073700081125     c                   eval      vgddat = amgudt
073800081124     C                   WRITE     tivgd000
073900120222     c                   end
074000910923     C                   ENDSR
074100910923     C*
074200010510     C*--------------------------------------------------------
074300010510     C* WRTTPC    SCRIVO IL TRACCIATO PER FILE PC MACCHINA SMISTA ASS.
074400010510     C*--------------------------------------------------------
074500010510     C     WRTTPC        BEGSR
074600010510     C*
074700120222     c                   add       1             wsborder
074800120222     C                   Z-ADD     rgfw          WSBrgf
074900120222     C                   Z-ADD     LNPw          WSBLNP
075000030326     C                   Z-ADD     LNAw          WSBLNA
075100030326     C                   Z-ADD     AASw          WSBAAS
075200030326     C                   Z-ADD     NRSw          WSBNRS
075300030326     C                   Z-ADD     NSPw          WSBNSP
075400010510     C*
075500030326     C                   Z-ADD     CASw          WSBCAS
075600120222     C                   Z-ADD     idcw          WSBidc
075700030326     C                   Z-ADD     ABIw          WSBABI
075800030326     C                   Z-ADD     CABw          WSBCAB
075900030326     C                   Z-ADD     NDPw          WSBNDP
076000030326     C                   Z-ADD     DDPw          WSBDDP
076100120302     C                   Z-ADD     cdic          WSBCDI
076200120302     c                   eval      wsbragcdi = rsmc
076300120302     c                   eval      wsbrsd = rsdw
076400030507     C                   EVAL      WSBNA4 = atoi(%SUBST(%TRIML(nraw):1:4))
076500030326     C                   MOVEL     VCAw          WSBDIV
076600010510     C*
076700010510     C                   WRITE     TNWSBM00
076800130423      *verifico se scrivere file cliente da banca
076900130417     C                   MOVEL     'CCA'         TBecod
077000130417     C                   MOVEL(p)  cdic          TBeke1
077100130426     C     KTbe          CHAIN     Tntbe01l
077200130417     c                   if        %found(tntbe01l)
077300130417     C                   exsr      file_vam
077400130417     c                   endif
077500010510     C                   ENDSR
077600130417     C*--------------------------------------------------------
077700130417     C* File-vam  SCRIVO IL TRACCIATO PER FILE cliente banca
077800130417     C*--------------------------------------------------------
077900130417     C     File_vam      BEGSR
078000130417     C*
078100130417     c                   clear                   wfvam000
078200130417     C                   Z-ADD     WSBLNP        vamLNP
078300130417     C                   Z-ADD     WSBLNA        vamLNA
078400130417     C                   Z-ADD     WSBAAS        vamAAS
078500130417     C                   Z-ADD     WSBNRS        vamNRS
078600130417     C                   Z-ADD     WSBNSP        vamNSP
078700130417     C*
078800130417     C                   Z-ADD     WSBCAS        vamCAS
078900130417     C                   Z-ADD     WSBidc        vamidc
079000130417     C                   Z-ADD     WSBABI        vamABI
079100130417     C                   Z-ADD     WSBCAB        vamCAi
079200130417     C                   Z-ADD     WSBNDP        vamNDP
079300130417     C                   Z-ADD     WSBDDP        vamDDP
079400130417     C                   Z-ADD     WSBCDI        vamksc
079500130430     C                   movel     wsbragcdi     VAMRSM
079600130417     C                   Z-ADD     RMNw          VAMRMN
079700130423     C                   movel     RMAw          VAMRMA
079800130417     C                   Z-ADD     mgsw          VAMMGS
079900130423     C                   movel     vcaw          VAMVCA
080000130417     C                   Z-ADD     dtew          VAMDTE
080100130423     C                   move      tpiw          VAMTPI
080200130423     C                   movel     rsdw          VAMRSD
080300130423     C                   move      prow          VAMPRO
080400130423     C                   Z-ADD     0             VAMNDPAS
080500130423      *
080600130424     C                   movel     prgw          VAMkey
080700130426     C*mm                EVAL      vamnra = atoi(%SUBST(%TRIML(nraw):1:4))
080800130426     c                   movel     nraw          vamnra
080900130430      * imposta ASS in quelli 1 a 1
081000130430     c                   if        vamkey = *blank
081100130430     c                   move      'ASS'         vamtrd
081200130430     c                   endif
081300130417     C*
081400130423     C                   WRITE     wfvam000
081500130424     c                   endsr
081600130424     C*--------------------------------------------------------
081700130424     C* File-vam  SCRIVO IL TRACCIATO PER FILE cliente banca
081800130424     C*--------------------------------------------------------
081900130424     C     riord_vam     BEGSR
082000130424      *
082100130426     c                   movel     '0'           kchiave
082200130426     c     kchiave       setll     wfvam01l
082300130424     c                   do        *hival
082400130424     c                   read      wfvam01l
082500130424     c                   if        %eof(wfvam01l)
082600130429     c                   exsr      agg_vam
082700130424     c                   leave
082800130424     c                   endif
082900130424      *memorizzo la prima chiave per le rotture
083000130424     c                   if        primo = *blank
083100130426     c                   move      'X'           primo             1
083200130426     c                   move      vamkey        kchiave          10
083300130429     c                   z-add     vamdte        exdte
083400130429     c                   move      vamnra        exnra
083500130429     c                   move      vamabi        exabi
083600130429     c                   move      vamcai        excai
083700130429     c                   move      vamksc        exksc
083800130429     c                   move      vamndp        exndp
083900130424     c                   z-add     1             conta
084000130429     c                   iter
084100130424     c                   endif
084200130424      *
084300130426     c                   if        vamkey <> kchiave
084400130424     c                   exsr      agg_vam
084500130426     c                   move      vamkey        kchiave
084600130429     c                   z-add     vamdte        exdte
084700130429     c                   move      vamnra        exnra
084800130429     c                   move      vamabi        exabi
084900130429     c                   move      vamcai        excai
085000130429     c                   move      vamksc        exksc
085100130429     c                   move      vamndp        exndp
085200130424     c                   z-add     1             conta
085300130424     c                   iter
085400130424     c                   endif
085500130429      *memorizzo codice cliente più basso di valore nell'ambito della stessa
085600130429      * chiave
085700130429     c                   if        vamksc < exksc
085800130429     c                   z-add     vamksc        exksc
085900130429     c                   move      vamndp        exndp
086000130429     c                   endif
086100130429      * verifica rottura assegno per attribuzione tipo rekord
086200130429      *
086300130429     c                   if        vamdte <> exdte  or
086400130429     c                             vamnra <> exnra  or
086500130429     c                             vamabi <> exabi  or
086600130429     c                             vamcai <> excai
086700130424     c                   add       1             conta
086800130424     c                   endif
086900130424     c
087000130424     c                   enddo
087100130424     C*
087200130424     c                   endsr
087300130424     C*--------------------------------------------------------
087400130424     C* File-vam  SCRIVO IL TRACCIATO PER FILE cliente banca
087500130424     C*--------------------------------------------------------
087600130426     C     agg_vam       BEGSR
087700130424      *
087800130430     c                   clear                   sped
087900130426     c     kchiave       setll     wfvam02l
088000130424     c                   do        *hival
088100130426     c     kchiave       reade     wfvam02l
088200130424     c                   if        %eof(wfvam02l)
088300130424     c                   leave
088400130424     c                   endif
088500130430     c                   if        £vamndp <> exndp
088600130429     c                   z-add     exndp         £vamndpas
088700130430     c                   endif
088800130424      *memorizzo la prima chiave per le rotture
088900130424     c                   if        conta > 1
089000130430      * memorizzo spedizione per verificare la rottura e capire se oltre
089100130430      * a multiassegno è anche multispedizione
089200130430     c                   if        sped  = *blank
089300130430     c                   move      £vamaas       exaas
089400130430     c                   move      £vamnsp       exnsp
089500130430     c                   move      £vamnrs       exnrs
089600130430     c                   move      £vamlnp       exlnp
089700130430     c                   move      'X'           sped
089800130430     c                   endif
089900130430      *se per multiassegno trova anche più spedizioni per le successive
090000130430      * inserisce tipo rekord di annullamento perchè non vengano poi sommati
090100130430     c                   if        £vamaas = exaas and
090200130430     c                             £vamnsp = exnsp and
090300130430     c                             £vamnrs = exnrs and
090400130430     c                             £vamlnp = exlnp
090500130429     c                   move      'ASS'         £vamtrd
090600130430     c                   else
090700130430     c                   move      'NUL'         £vamtrd
090800130430     c                   endif
090900130430      *monoassegno
091000130430     c                   else
091100130429     c                   move      'CAS'         £vamtrd
091200130424     c                   endif
091300130424     c
091400130424     C                   update    wfvam2
091500130424     c                   enddo
091600130424     C*
091700130424     c                   endsr
091800030317     C*--------------------------------------------------------
091900030331     C* srvab     SCRIVO un record per ogni spediz. sul FIVABCAF e FIVADCAF
092000030317     C*--------------------------------------------------------
092100030317     C     srvab         BEGSR
092200030331     c* FIVABCAF
092300030325     c                   clear                   fivab000
092400030317     C                   Z-ADD     102           vabfgs
092500030404     C                   z-add     1020004       VAbccm
092600030325     C                   z-add     102           VAbLNP
092700030325     C                   MOVEL     amgudt        VAbAAS
092800030325     c                   MOVE      amgudt        VAbmgs
092900030404     C                   EVAL      VAbNRS = 02
093000030326     C                   z-add     numlet        VAbNSP
093100030325     C                   MOVEL     'FW'          VAbcbo
093200030325     C                   z-add     poac          VAblna
093300030326     C                   eval      vabrsd = rsmc
093400030326     C                   eval      vabind = inmc
093500030326     C                   eval      vabcad = camc
093600030326     C                   eval      vablod = lomc
093700030326     C                   eval      vabprd = prmc
093800030326     C                   eval      vabnzd = nzmc
093900030404     C                   eval      vabpkb = 0,1
094000030404     C                   eval      vabvlb = 0,001
094100030331     C                   eval      vabnas = 'DOCUMENTI'
094200030331     C                   eval      vabncl = paga
094300030325     C                   Z-ADD     numlet        VAbrmn
094400030401     C                   MOVE      cdic          SERV89            4 0
094500030331     c                   if        serv89 <> 8888 and serv89 <> 9999
094600030521     C                   MOVEl     cdic          cod7              7
094700030512     C                   eval      vabnot= 'cliente tassazione ' + cod7
094800030331     c                   end
094900030331     c* il segnacollo iniziale è uguale al numero della lettera mentre
095000030331     c* i restanti segnacolli sono calcolati aggiungendo sempre 1 al
095100030331     c* numeratore delle lettere quindi il segnacollo finale è data
095200030331     c* dal numero della lettera + il numero delle pagine (cioè quanti
095300030331     c* segnacolli sono stati stampati) - 1 perchè già conteggiato nella
095400030331     c* prima lettera.
095500030331     c* es. spedizione: 1 segnacollo iniziale: 1 totale pag.: 3 segnacollo
095600030331     c* finale: 3. Spedizione successiva: 4 segnacollo iniziale: 4 totale
095700030331     c* pag.: 1 segnacollo finale 4
095800030325     C                   Z-ADD     numlet        VAbncd
095900030509     C                   if        paga = 1
096000030509     c                   clear                   vabnca
096100030509     c                   else
096200030331     C                   eval      VAbnca = numlet + paga - 1
096300030509     c                   end
096400030325     C                   z-add     zscc          VAbznc
096500030404     c                   eval      vabctm = '2W'
096600030325     C                   WRITE     fivab000
096700030331     c* FIVADCAF
096800030331     c                   clear                   fivad000
096900030404     C                   z-add     1020004       VAdccm
097000030401     C                   z-add     102           VAdLNP
097100030529     C                   z-add     102           VAdfgs
097200030401     C                   MOVEL     amgudt        VAdAAS
097300030404     C                   EVAL      VAdNRS = 02
097400030401     C                   z-add     numlet        VAdNSP
097500030401     C*?                 eval      VAdcdp =
097600030401     C                   eval      vadncl = paga
097700030401     C                   Z-ADD     numlet        VAdncd
097800030401     C                   eval      VAdnca = numlet + paga - 1
097900030331     C                   WRITE     fivad000
098000030317     C                   ENDSR
098100010510     C*
098200030325     C*---------------------------------------------------------------*
098300030326     C     sraggtab      BEGSR
098400030325     C*---------------------------------------------------------------*
098500030326     C* AGGIORNO numeratore lettera
098600030326     C                   MOVEL     '4B'          TBLCOD
098700030326     C                   MOVEL(p)  '1'           TBLKEY
098800030326     C     KTAB          CHAIN     TABEL00F                           20
098900030331     C  N20              z-add     pseg          §4BULN
099000030326     C  N20              MOVEL     DS4B          TBLUNI
099100030326     C  N20              UPDATE    TABEL
099200030326     C                   ENDSR
099300030326     C*---------------------------------------------------------------*
099400030326     C     *inzsr        BEGSR
099500030326     C*---------------------------------------------------------------*
099600030310     C     *ENTRY        PLIST
099700030310     C                   PARM                    KPJBA
099800030314     C*---------- RICERCA DITTA :
099900030314     C                   MOVEL     'SOC001'      TIPXSC
100000030314     C                   MOVEL     'SOC001'      TIPXSC
100100030314     C                   MOVEL     *BLANK        SOCXSC
100200030314     C                   EXSR      REPSOC
100300030314     C     RTNXSC        IFNE      '1'
100400030314     C                   MOVEL     XSOCDS        SOC001
100500030314     C                   MOVEL     xscrgs        RSUT             20
100600030314     c                   end
100700030310     C* ACQU. ULTIMO NUMERO LETTERA
100800030326     C                   Z-ADD     1             CODUT             1 0
100900030310     C                   MOVEL     '4B'          TBLCOD
101000030310     C                   MOVEL(p)  '1'           TBLKEY
101100030310     C     KTAB          CHAIN     TABEL00F                           20
101200030310     C  N20              MOVEL     TBLUNI        DS4B
101300030310     C   20              SETON                                        H1
101400030310     C*---------------------------------------------------------------*
101500030310     C* DEFINIZIONE CHIAVI
101600030310     C*
101700030310     C     KNSP          KLIST
101800030326     C                   KFLD                    AASw
101900030326     C                   KFLD                    LNPw
102000030326     C                   KFLD                    NRSw
102100030326     C                   KFLD                    NSPw
102200030310     C     KTAB          KLIST
102300030310     C                   KFLD                    CODUT
102400030310     C                   KFLD                    TBLCOD
102500030310     C                   KFLD                    TBLKEY
102600130426     C     Ktbe          KLIST
102700130426     C                   KFLD                    TBECOD
102800130426     C                   KFLD                    TBEKE1
102900030325     C*
103000030310     C     *LIKE         DEFINE    CSBNDP        NUMLET
103100030310     C*
103200030310     c                   move      udate         dtaeur
103300030310     c                   move      dtaeur        dtaiso
103400030310     C                   move      dtaiso        AMGUDT            8 0
103500030326     C                   MOVE      dtaeur        GMAUDT            8 0
103600030310      * FORZO LA DATA DEL JOB
103700030326     C     KDTJB         IFne      *blanks
103800030310     c                   move      kdtjb         com06             6 0
103900030326     c     *dmy          move      com06         dtaeur
104000030310     c                   move      dtaeur        dtaiso
104100030310     C                   move      dtaiso        AMGUDT            8 0
104200030326     C                   MOVE      dtaeur        GMAUDT            8 0
104300030310     C                   ENDIF
104400031114     c*  lettura tabella YCS
104500031114     c                   clear                   xatbds
104600031114     c                   move      '1'           xtbric
104700031114     c                   move      *blank        xtbkey
104800031114     c                   move      'YCS'         xtbcod
104900031114     c                   movel     XSCSOC        xtbkey
105000031114     c                   call      'XATB'
105100031114     c                   parm                    xatbds
105200031114     c                   if        xtberr = '0'
105300031114     c                   movel     xtbuni        angycs
105400031114     c                   else
105500031114     c                   clear                   angycs
105600031114     c                   end
105700031114     C* IMPOSTO dati anagrafici società
105800110510     c                   evalr     tbar= %trimr(xscrgs)
105900060516     c                   eval      tbaS1= $Intest1
106000060531     c                   eval      tba1= $Intest2
106100060531     c                   eval      tba2= $Intest4
106200031114     c                   if        amgudt < 20031231
106300031114     c                   eval      tba3= 'Capitale Sociale Euro' +
106400031114     c                             %TRIMr(%EDITC(YCSIMPCASO:'2'))+
106500031114     c                             ' int. vers.'
106600031114     c                   else
106700031114     c                   eval      tba4= 'Capitale Sociale Euro' +
106800031114     c                             %TRIMr(%EDITC(YCSIMPCASO:'2'))+
106900031114     c                             ' int. vers.'
107000031114     c                   seton                                        80
107100031114     c                   end
107200031114     c                   eval      tba= tbar
107300060516     c                   eval      tbas= tbas1
107400031114     c                   eval      tb1=tba1
107500031114     c                   eval      tb2= tba2
107600031114     c                   if        amgudt < 20031231
107700031114     c                   eval      tb3= tba3
107800031114     c                   else
107900031114     c                   eval      tb4= tba4
108000031114     c                   end
108100031114     c                   eval      tba5= 'info on-line'
108200120319     c                   eval      tba6= 'www.brt.it'
108300130430      * elimina file in wrkquery
108400130430     c                   movea     cmd2          coman
108500130430     c                   exsr      comandi
108600030310     C                   ENDSR
108700030314     C*----------------------------------------------------*
108800030314     C* Reperimento dati società
108900030314     C*----------------------------------------------------*
109000030314     C     REPSOC        BEGSR
109100030314     C*
109200030314     C                   CALLB     'XSOC'
109300030314     C                   PARM                    TIPXSC            6
109400030326     C                   PARM      '201'         SOCXSC            3
109500030314     C                   PARM                    CDSXSC            9 0
109600030314     C                   PARM                    MODXSC            3
109700030314     C                   PARM      *blanks       RTNXSC            1
109800030314     C                   PARM                    XSOCDS
109900030314     C                   PARM                    KPJBA
110000030314     c                   movel     xscrgs        prsm
110100030314     c                   movel     xscvia        pinm
110200030314     c                   movel     xsccap        pcam
110300030314     c                   movel     xsccit        plom
110400030314     c                   movel     xscprv        pprm
110500030314     C*
110600030314     C                   ENDSR
110700130430     c*-------------------------------------------------
110800130430     c     comandi       begsr
110900130430     c*-------------------------------------------------
111000130430      *
111100130430     C                   z-add     99            luncmd
111200130430     C                   CALL      'QCMDEXC'                            34
111300130430     C                   parm                    coman
111400130430     C                   parm                    luncmd
111500130430      *
111600130430     c                   endsr
111700000000     C*---------------------------------------------------------------*
111800941031     OTNCSB000  E            TNCSB
111900910618     O                       CSBBNA
112000910618     O                       CSBNDP
112100910618     O                       CSBDDP
112200931025     O                       CSBFTR
112300931025     O                       CSBDTR
112400130430**  CMD1
112500130430CRTDUPOBJ OBJ(WFVAM00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) DATA(*YES)
112600130430**  CMD2
112700130430DLTF FILE(wrkquery/WFVAM00F)
