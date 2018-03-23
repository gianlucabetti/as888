000100120109      /TITLE Incasso assegni mittente
000200120110     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400020812
000500120220     ffncss01l  if   e           k disk    rename(fncss000:fncss001)
000600120112     f                                     usropn
000700120209     ffncsb01l  if   e           k disk    usropn
000800120223     ftncsv01l  if   e           k disk    usropn
000900120629     ftncsa01l  if   e           k disk    usropn
001000120223     ftncsb03l  if   e           k disk    usropn
001100120112     ftncsb09l  if   e           k disk    rename(tncsb000:tncsb009)
001200120112     f                                     usropn
001300120111     ftncsm02l  if   e           k disk    rename(tncsm000:tncsm002)
001400120220     ffncss03l  if   e           k disk    usropn
001500120220     F                                     rename(fncss000:fncss003)
001600120220     ffncss04l  uf a e           k disk    commit(cmt) usropn
001700120111     ftncsm03l  uf a e           k disk    commit(cmt) usropn
001800120109     fcnabi01l  if   e           k disk
001900120109     ftabel00f  if   e           k disk
002000120210     Ftnsc30d   CF   E             WORKSTN
002100000000     F                                     SFILE(DSF1:NRR1)
002200000000     F                                     SFILE(DSF2:NRR2)
002300120210     D tnsc30ds      E DS
002400120209     D fncsbds       E DS                  extname(fncsb00f)
002500120223     D tncsvds       E DS                  extname(tncsv00f)
002600120111     c* numeratori
002700120111     d trul33ds      e ds
002800120109     D ddatiute      e ds
002900120110     D tncsmds       e ds                  extname(TNCSM00F)
003000120109     D azuteds       e ds                  extname(AZUTE00F)
003100120109     D tibs34ds      E DS                  inz
003200120109     D ds1a          E DS                  inz
003300120109     D dstm          E DS                  inz
003400120629     D TIBS69        E DS                  EXTNAME(TIBS69DS)
003500120629     D CNACO         E DS                  EXTNAME(CNACO00F)
003600120629     D CNIND         E DS                  EXTNAME(CNIND00F)
003700120629     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003800120629     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003900120109     D WLBDA8          DS
004000120109     D  G02DAT                 1      8  0
004100120109     D  G02INV                 9     16  0
004200120109     D  G02ERR                17     17
004300120109     D  G02TGI                18     22  0
004400000000     D KPJBA         E DS
004500120111     D cmt             S              1
004600120109     D xx              S              2  0
004700120109     D D1A             S             78    DIM(200)
004800120109     D C1A             S              2    DIM(200)
004900120629     D comcdi          S                   like(csbcdi)
005000120629     D kaas            S                   like(csbaas)
005100120629     D klnp            S                   like(csblnp)
005200120629     D knrs            S                   like(csbnrs)
005300120629     D knsp            S                   like(csbnsp)
005400120629     D savmit          S                   like(cssmit)
005500120111     D totass          S                   like(v1cidc)
005600120111     D totcass         S                   like(v1cidc)
005700120222     D comdiffe        S                   like(v1cidc)
005800120109     D dtaeur          S               d   datfmt(*eur)
005900120109     D dtaiso          S               d   datfmt(*iso)
006000120111     D sass            S             28    DIM(200)
006100120111     D comass          S             28
006200120220     D comkey          S             10
006300120306     D comopz          S             10
006400120710     D w1cABi          S                   like(v1cABi)
006500120710     D w1cCAb          S                   like(v1cCAb)
006600000111
006700920312     C*-----
006800120110     c                   exsr      srcarsfl
006900120110     C* almeno una spedizione                              *
007000000000     C     PIENO         IFEQ      *BLANK
007100120210     c                   eval      sc30erro = '1'
007200120210     c                   eval      sc30msgo = 'Nessuna spedizione inserita'
007300120109     c                   exsr      endpgm
007400120109     c                   end
007500000000     C*--- EMISSIONE SUBFILE                               *
007600120110?    C                   do        *hival
007700910522     C                   SETOFF                                       6465
007800000000     C     NRR1          IFEQ      0
007900000000     C                   SETOFF                                           61
008000000000     C                   END
008100000000     C     NRR2          IFEQ      0
008200000000     C                   SETOFF                                           62
008300000000     C                   END
008400020812
008500020813     C                   WRITE     DCT1
008600120629     C                   WRITE     CB36D2
008700000000     C                   EXFMT     DCT2
008800120109      *  Guida
008900120109     C     *INKL         IFEQ      '1'
009000120313     c                   eval      sc30keyo = comkey
009100120210     c                   eval      sc30erro = 'L'
009200120109     C                   exsr      endpgm
009300120109     C                   END
009400120109     c* annullamento
009500120209     c   kq              exsr      srannulla
009600120110     C*...Controllo i dati modificati
009700000000?    C                   EXSR      CONTA
009800120110     C*...per ERRORE torno ad emettere il sfl.
009900120110     c                   if        *in50
010000120111     C                   MOVE      NRR2          RECNB2
010100120111     c                   iter
010200120111     c                   end
010300120110     C* aggiornamento                                      *
010400120629     C                   IF        *inkf or *inkg
010500120111?    C                   EXSR      AGGIOR
010600120111     c                   end
010700120110     c                   enddo
010800120110     C*
010900920312     C                   SETON                                        LR
011000120110     c**********************************************************************
011100120110     C*...Inizializzo i sub-file.
011200120110     c**********************************************************************
011300120110     c     srcarsfl      begsr
011400120110     C                   Z-ADD     1             RECNBR
011500120111     C                   Z-ADD     1             RECNB2
011600120110     C                   Z-ADD     0             NRR2              5 0
011700120110     C                   Z-ADD     0             NUMRE2
011800120110     C                   Z-ADD     0             NRR1              5 0
011900120110     C                   Z-ADD     0             NUMREC
012000120110     C                   SETON                                        60
012100120110     C                   SETOFF                                       61
012200120110     C                   SETOFF                                       62
012300120110     C                   WRITE     DCT1
012400120110     C                   WRITE     DCT2
012500120110     C                   SETON                                        61
012600120110     C                   SETON                                        62
012700120111     C                   SETOFF                                       60
012800120110     C*...Carico il sub-file delle spedizioni              *
012900120110     C                   MOVEL     *BLANK        PIENO
013000120110     C                   Z-ADD     0             NRR1
013100120110     C                   Z-ADD     0             NUMREC
013200120111     C                   Z-ADD     0             totcass
013300120222     C                   Z-ADD     0             comdiffe
013400120113     c* filiale
013500120209     c                   select
013600120210     c                   when      sc30ambi = ' '
013700120113     c* inserimento (distinta + opzione)
013800120210     c                   if        sc30tpci = 'I'
013900120220     C     kleg          SETLL     fncss04l
014000120113     c                   else
014100120113     c* no inserimento (legame)
014200120220     C     comkey        SETLL     fncss03l
014300120113     c                   end
014400120110?    C                   do        *hival
014500120210     c                   if        sc30tpci = 'I'
014600120306     C     kleg          READE(n)  fncss04l
014700120220     C                   if        %eof(fncss04l)
014800120220     c                   leave
014900120220     c                   end
015000120312     c* in inserimento solo quelle senza legame
015100120312     C                   if        csskey <> ' '
015200120312     c                   iter
015300120312     c                   end
015400120113     c                   else
015500120306     C     comkey        READE     fncss03l
015600120220     C                   if        %eof(fncss03l)
015700120110     c                   leave
015800120110     c                   end
015900120220     c                   end
016000120110     c                   if        cssatb <> ' '
016100120110     c                   iter
016200120110     c                   end
016300120110     C*
016400120110?    C                   EXSR      CARSB1
016500120110     C                   enddo
016600120209     c* partenza
016700120210     c                   when      sc30ambi ='P'
016800120209     C     ksped         chain     fncsb01l
016900120209     c                   if        %found(fncsb01l)
017000120209?    C                   EXSR      CARSB1P
017100120209     c                   end
017200120518     c* sede
017300120223     c                   when      sc30ambi ='S'
017400120112     C     kcsb          SETLL     tncsb09l
017500120112?    C                   do        *hival
017600120112     C     kcsb          READE     tncsb09l
017700120112     C                   if        %eof(tncsb09l)
017800120112     c                   leave
017900120112     c                   end
018000120112     c* solo il contrassegno con legame
018100120112     c                   if        csbnra <> comkey
018200120112     c                   iter
018300120112     c                   end
018400120112     C*
018500120112?    C                   EXSR      CARSB1S
018600120112     C                   enddo
018700120223     c* storico sede
018800120223     c                   when      sc30ambi ='V'
018900120223     C/EXEC SQL
019000120223     C+ DECLARE TNCSVC CURSOR FOR SELECT * FROM tncsv00f WHERE csvnra =
019100120322     C+ :comkey and CSVDTV = :sc30dtvi
019200120223     C/END-EXEC
019300120223
019400120223     C/EXEC SQL
019500120223     C+ OPEN tncsvc
019600120223     C/END-EXEC
019700120223
019800120223     C                   DOU       sqlcod <> 0
019900120223
020000120223     C/EXEC SQL
020100120223     C+ FETCH NEXT FROM TNCSvC INTO : tncsvds
020200120223     C/END-EXEC
020300120223
020400120223     C                   SELECT
020500120223     C                   WHEN      SQLCod = 100
020600120223     C                   WHEN      SQLCod < 0
020700120223     C                   EVAL      *INH1 = *ON
020800120223     C                   OTHER
020900120223     c* stampa testata e apre membro
021000120223     c                   exsr      carsb1v
021100120223     C                   ENDSL
021200120223
021300120223     C                   ENDDO
021400120223     C/EXEC SQL
021500120223     C+ CLOSE tncsvc
021600120223     C/END-EXEC
021700120209     c                   endsl
021800120110     C*...Carico il sub-file degli assegni                 *
021900120110     C                   MOVEL     *BLANK        PIEN1
022000120110     C                   Z-ADD     0             NRR2
022100120110     C                   Z-ADD     0             NUMRE2
022200120111     C     comkey        SETLL     tncsm03l
022300120110?    C                   do        *hival
022400120111     C     comkey        READE     tncsm03l
022500120110     C                   if        %eof(tncsm03l)
022600120110     c                   leave
022700120110     c                   end
022800120110     c                   if        csmatb <> ' '
022900120110     c                   iter
023000120110     c                   end
023100120110     c*
023200120110?    C                   EXSR      CARSB2
023300120110     C                   ENDdo
023400120110     c* carico 21 righe vuote nel sfl degli assegni
023500120210     c                   if        sc30tpci <> 'V'
023600120110     c                   do        21
023700120110     c                   clear                   tncsmds
023800120110?    C                   EXSR      CARSB2
023900120110     c                   enddo
024000120111     c                   end
024100120110     c                   endsr
024200000000     C*---------------------------------------------------------------*---
024300120110     C*  CARSB1:   CARICA SUBFILE SPEDIZIONI                          *
024400000000     C*---------------------------------------------------------------*
024500000000     C     CARSB1        BEGSR
024600000000     C*
024700120109     C*...Carico riga spedizione
024800120110     c                   eval      v1caas = cssaas
024900120111     c                   eval      v1clnp = csslnp
025000120110     c                   eval      v1cnrs = cssnrs
025100120110     c                   eval      v1cnsp = cssnsp
025200120518     c                   if        cssnso <> cssnsp
025300120110     c                   eval      v1caao = cssaao
025400120110     c                   eval      v1clpo = csslno
025500120110     c                   eval      v1cnro = cssnro
025600120110     c                   eval      v1cnso = CSSNSo
025700120518     c                   else
025800120518     c                   clear                   v1caao
025900120518     c                   clear                   v1clpo
026000120518     c                   clear                   v1cnro
026100120518     c                   clear                   v1cnso
026200120518     c                   end
026300120628     c                   eval      v1cvca = CSSvca
026400120518     c                   eval      v1cmit = CSSmit
026500120628     c* se sono in inserimento verifico se le ragioni sociali dei clienti
026600120628     c* sono diverse chiedo la forzatura
026700120629     c                   if        cssmit <> savmit
026800120628     c                             and sc30tpci = 'I'
026900120628     c                   if        savmit <> ' '
027000120702     c                   eval      comodomsg = 'ATTE.NE ci sono ra-
027100120702     c                             gioni sociali diverse!! RIPRISTINARE per co-
027200120629     c                             ntinuare.'
027300120629     C                   SETON                                        33
027400120628     c                   else
027500120628     c                   eval      savmit = CSSmit
027600120628     c                   end
027700120628     c                   end
027800120628     c*
027900120110     C                   Z-ADD     csscas        v1ccas
028000120111     c                   add       csscas        totcass
028100120110     c* mi servirà nel sfl degli assegni
028200120110     c                   eval      v1ctic = csstic
028300000000     C                   ADD       1             NRR1
028400000000     C                   ADD       1             NUMREC            4 0
028500000000     C                   WRITE     DSF1
028600000000     C                   MOVEL     '1'           PIENO             1
028700000000     C*                                                    *
028800000000     C                   ENDSR
028900120112     C*---------------------------------------------------------------*---
029000120112     C*  CARSB1S   CARICA SUBFILE SPEDIZIONI da sede                  *
029100120112     C*---------------------------------------------------------------*
029200120112     C     CARSB1S       BEGSR
029300120112     C*
029400120518     c                   clear                   v1cmit
029500120518     c                   clear                   v1caao
029600120518     c                   clear                   v1clpo
029700120518     c                   clear                   v1cnro
029800120518     c                   clear                   v1cnso
029900120629     c                   eval      comcdi = csbcdi
030000120629     c                   eval      kaas = csbaas
030100120629     c                   eval      klnp = csblnp
030200120629     c                   eval      knrs = csbnrs
030300120629     c                   eval      knsp = csbnsp
030400120629     c                   exsr      srcli
030500120112     C*...Carico riga spedizione
030600120112     c                   eval      v1caas = csbaas
030700120112     c                   eval      v1clnp = csblnp
030800120112     c                   eval      v1cnrs = csbnrs
030900120112     c                   eval      v1cnsp = csbnsp
031000120112     c                   eval      v1cvca = CSbvca
031100120117     C                   Z-ADD     csbcas        v1ccas
031200120117     c                   add       csbcas        totcass
031300120112     c* mi servirà nel sfl degli assegni
031400120112     c                   movel     csbtpa        v1ctic
031500120112     c                   move      csbtpi        v1ctic
031600120112     C                   ADD       1             NRR1
031700120112     C                   ADD       1             NUMREC            4 0
031800120112     C                   WRITE     DSF1
031900120112     C                   MOVEL     '1'           PIENO             1
032000120112     C*                                                    *
032100120112     C                   ENDSR
032200120223     C*---------------------------------------------------------------*---
032300120223     C*  CARSB1v   CARICA SUBFILE SPEDIZIONI da variazioni di sede    *
032400120223     C*---------------------------------------------------------------*
032500120223     C     CARSB1v       BEGSR
032600120223     C*
032700120518     c                   clear                   v1cmit
032800120518     c                   clear                   v1caao
032900120518     c                   clear                   v1clpo
033000120518     c                   clear                   v1cnro
033100120518     c                   clear                   v1cnso
033200120629     c                   eval      comcdi = csvcdi
033300120629     c                   eval      kaas = csvaas
033400120629     c                   eval      klnp = csvlnp
033500120629     c                   eval      knrs = csvnrs
033600120629     c                   eval      knsp = csvnsp
033700120629     c                   exsr      srcli
033800120223     C*...Carico riga spedizione
033900120223     c                   eval      v1caas = csvaas
034000120223     c                   eval      v1clnp = csvlnp
034100120223     c                   eval      v1cnrs = csvnrs
034200120223     c                   eval      v1cnsp = csvnsp
034300120223     c                   eval      v1cvca = CSvvca
034400120223     C                   Z-ADD     csvcas        v1ccas
034500120223     c                   add       csvcas        totcass
034600120223     c* mi servirà nel sfl degli assegni
034700120223     c                   movel     csvtpa        v1ctic
034800120223     c                   move      csvtpi        v1ctic
034900120223     C                   ADD       1             NRR1
035000120223     C                   ADD       1             NUMREC            4 0
035100120223     C                   WRITE     DSF1
035200120223     C                   MOVEL     '1'           PIENO             1
035300120223     C*                                                    *
035400120223     C                   ENDSR
035500120629     C*---------------------------------------------------------------*---
035600120629     C*  decodifica mittente per la sede
035700120629     C*---------------------------------------------------------------*
035800120629     C     srcli         BEGSR
035900120629     c* decodifica mittente
036000120629     C                   MOVE      comcdi        APP               4
036100120629     C     APP           IFEQ      '8888'
036200120629     C     APP           OREQ      '9999'
036300120629     C                   MOVEL     *BLANKS       CSARSD
036400120629     C     KCSA          CHAIN     TNCSA01L                           31
036500120629     C                   MOVEL     CSARSD        V1cmit
036600120629     C                   ELSE
036700120629      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
036800120629     C                   CLEAR                   TIBS69
036900120629     C                   CLEAR                   CNACO
037000120629     C                   CLEAR                   CNIND
037100120629     C                   CLEAR                   CNCLP
037200120629     C                   CLEAR                   FNCLS
037300120629     C                   MOVEL     ' '           I69TLA
037400120629     C                   MOVEL     KNSIF         I69SIF
037500120629     C                   Z-ADD     comcdi        I69KAC
037600120629     C                   Z-ADD     comcdi        I69KIN
037700120629     C                   CALL      'TIBS69R'
037800120629     C                   PARM                    TIBS69
037900120629     C                   PARM                    CNACO
038000120629     C                   PARM                    CNIND
038100120629     C                   PARM                    CNCLP
038200120629     C                   PARM                    FNCLS
038300120629     C     O69ERR        IFEQ      *BLANK
038400120629     C                   MOVEL     ACORAG        V1cmit
038500120629     C                   END
038600120629     C                   ENDIF
038700120629     c                   endsr
038800120209     C*---------------------------------------------------------------*---
038900120209     C*  CARSB1S   CARICA SUBFILE SPEDIZIONI da filiale partenza      *
039000120209     C*---------------------------------------------------------------*
039100120209     C     CARSB1p       BEGSR
039200120209     C*
039300120209     C/EXEC SQL
039400120209     C+ Declare A1 Cursor for
039500120209     C+ SELECT *
039600120209     C+ from fncsb00f
039700120209     C+ where csbnra = : comkey
039800120209     C/END-EXEC
039900120209      *          apertura cursore
040000120209     C/EXEC SQL
040100120209     C+ OPEN A1
040200120209     C/END-EXEC
040300120209     C                   DO        *hival
040400120209      *          lettura cursore
040500120209     C/EXEC SQL
040600120209     C+ Fetch Next From A1 Into
040700120209     C+ :fncsbds
040800120209     C/END-EXEC
040900120209     C                   SELECT
041000120209     C                   WHEN      SqlCod = 100
041100120209     C                   LEAVE
041200120209     C                   WHEN      SqlCod >= 0
041300120209     C*...Carico riga spedizione
041400120209     c                   exsr      CARSB1S
041500120209     C                   OTHER
041600120209     C                   ENDSL
041700120209      *
041800120209     C                   ENDDO
041900120209     C/EXEC SQL
042000120209     C+ Close A1
042100120209     C/END-EXEC
042200120209     C*                                                    *
042300120209     C                   ENDSR
042400000000     C*---------------------------------------------------------------*---
042500000000     C*  CARSB2:   CARICA SUBFILE DELLA CONTABILITA'                  *
042600000000     C*---------------------------------------------------------------*
042700000000     C     CARSB2        BEGSR
042800000000     C*
042900120110     C                   Z-ADD     csmidc        v1cidc
043000120110     c                   movel     csmdiv        v1cdiv
043100120113     c                   if        csmdiv = ' '
043200120113     c                   eval      v1cdiv = 'EUR'
043300120113     c                   end
043400120111     c                   movel     csmnra        v1cnra
043500120110     c                   z-add     csmabi        v1cabi
043600120110     c                   z-add     csmcab        v1ccab
043700120111     c                   if        csmdte = 0
043800120111     c                   clear                   v1cdte
043900120111     c                   else
044000120111     C                   move      csmdte        dtaiso
044100120111     C                   move      dtaiso        dtaeur
044200120111     C                   move      dtaeur        v1cdte
044300120111     c                   end
044400990830
044500000000     C                   ADD       1             NRR2
044600000000     C                   ADD       1             NUMRE2            4 0
044700000000     C                   WRITE     DSF2
044800000000     C                   MOVEL     '1'           PIEN1             1
044900000000     C*
045000000000     C                   ENDSR
045100120111      *---------------------------------------------------------------*
045200120111      *  Loop di errori  subfile errori                               *
045300120111      *---------------------------------------------------------------*
045400120111     C     Conta         BEGSR
045500120111     c                   clear                   xx
045600120111     c                   clear                   sass
045700120628     c                   clear                   totass
045800120628     c                   clear                   nrass             2 0
045900120222     c                   clear                   comdiffe
046000120111     c                   setoff                                       313940
046100120111     c                   setoff                                       414243
046200120111     c                   setoff                                       448986
046300120710     c                   setoff                                       45
046400120117     c                   setoff                                       858150
046500120628     c                   setoff                                       323334
046600120111     C                   Z-ADD     0             NRR6              4 0
046700120111      *
046800120111     c                   do        *hival
046900120111     C                   ADD       1             NRR6
047000120111     C     NRR6          CHAIN     dsf2                               95
047100120111     c                   if        *in95
047200120111     c                   leave
047300120111     c                   end
047400120111     c*
047500120111     c                   if        v1cidc = 0 and
047600120117     c                             v1cnra = ' ' and
047700120117     c                             v1cabi = 0 and
047800120111     c                             v1ccab = 0 and
047900120111     c                             v1cdte = 0
048000120117     C                   UPDATE    dsf2
048100120111     c                   iter
048200120111     c                   end
048300120111     C*  Controllo se richiesta ricerca tipo incasso
048400120111     C     '?'           SCAN      V1CTIC                                 95
048500120111     C     *IN95         IFEQ      '1'
048600120111     C                   MOVEL     1             §KUT
048700120111     C                   MOVEL     '1A'          §COD
048800120111     C                   MOVE      *BLANKS       V1CTIC
048900120111     C                   CALL      'X§TABER'
049000120111     C                   PARM                    §KUT
049100120111     C                   PARM                    §COD
049200120111     C                   PARM                    §KEY
049300120111     C                   PARM                    §DES             30
049400120111     C                   MOVEL     §KEY          V1CTIC
049500120111     C                   END
049600120111     C*  Controllo se corretto tipo incasso
049700120111     c                   if        v1ctic = ' '
049800120111     C                   SETON                                        4050
049900120111     C                   UPDATE    dsf2
050000120111     C                   leavesr
050100120111     c                   else
050200120111     C                   Z-ADD     1             X
050300120111     C     V1CTIC        LOOKUP    C1A(X)                                 95
050400120111     C     *IN95         IFEQ      '0'
050500120111     C                   SETON                                        4050
050600120111     C                   UPDATE    dsf2
050700120111     C                   leavesr
050800120111     c                   else
050900120111     c                   movel     d1a(x)        ds1a
051000120111     C                   end
051100120111     C                   end
051200120111     C*  Controllo se cliente abilitato
051300120117     c                   if        §1ATTM ='S'
051400120111     C                   CLEAR                   DSTM
051500120111     C                   MOVEL     'TM'          tblcod
051600120117     c* filiale
051700120210     c                   if        sc30ambi = ' '
051800120111     C                   MOVEL     cssccm        tblkey
051900120117     c                   else
052000120117     C                   MOVEL     csbcdi        tblkey
052100120117     c                   end
052200120117     C     KTAB          CHAIN     TABEL00F
052300120111     c                   if        %found(tabel00f) and tblflg = ' '
052400120111     C                   MOVEL     tbluni        DSTM
052500120111     c                   else
052600120117     C                   SETON                                        8150
052700120111     C                   UPDATE    dsf2
052800120111     C                   leavesr
052900120111     c                   end
053000120111     C     V1CTIC        IFEQ      'TM'
053100120111     C     §tmftm        ANDNE     'S'
053200120111     C     V1CTIC        OREQ      'TO'
053300120111     C     §TMFTO        ANDNE     'S'
053400120111     C     V1CTIC        OREQ      'CA'
053500120111     C     §TMFCA        ANDNE     'S'
053600150526     C**** V1CTIC        OREQ      'BP'
053700150526     C**** §TMFbp        ANDNE     'S'
053800150522     C     V1CTIC        OREQ      'TP'
053900150522     C     §TMFtp        ANDNE     'S'
054000120117     C                   SETON                                        8150
054100120111     C                   UPDATE    dsf2
054200120111     C                   leavesr
054300120111     C                   END
054400120117     C                   END
054500120111     c* importo
054600120111     c                   if        v1cidc = 0
054700120111     C                   SETON                                        3950
054800120111     C                   UPDATE    dsf2
054900120111     C                   leavesr
055000120111     c                   else
055100120111     c                   add       v1cidc        totass
055200120628     c                   add       1             nrass
055300120111     c                   end
055400120111     c* numero assegno
055500120111     C     V1cnra        IFEQ      *BLANKS
055600120111     C     V1cnra        OREQ      *ZEROS
055700120111     C                   SETON                                        4150
055800120111     C                   UPDATE    dsf2
055900120111     C                   leavesr
056000120111     C                   END
056100120111     c* se tipo incasso non ammette assegni postali controllo abi  07601
056200120111     c     §1anap        ifeq      'S'
056300120111     c     v1cabi        andeq     07601
056400120111     C                   SETON                                        4250
056500120111     C                   UPDATE    dsf2
056600120111     C                   leavesr
056700120111     c                   endif
056800120111     C*  ABI e CAB
056900120111     C     V1cABi        IFle      *ZEROS
057000120111     C     V1cCAb        ORle      *ZEROS
057100120111     C                   SETON                                        4450
057200120111     C                   UPDATE    dsf2
057300120111     C                   leavesr
057400120111     C                   End
057500120111     C*  Se immesso Abi-CAb verifico congruenza
057600120716     c                   if        vforza = *blank or
057700120716     c                             (w1cABi <> V1cABi or
057800120716     c                             w1cCAb <> V1cCAb)
057900120111     C     KABI          CHAIN     CNABI01L
058000120111     c                   if        not %found(cnabi01l)
058100120710     c                   if        w1cABi = V1cABi and
058200120710     c                             w1cCAb = V1cCAb and
058300120710     c                             *inkb = '1'
058400120710     c                   move      '1'           vforza
058500120716     c                   move      V1cABi        w1cABi
058600120716     c                   move      V1cCAb        w1cCAb
058700120710     C                   UPDATE    dsf2
058800120710     c                   clear                   vforza
058900120710     C                   leavesr
059000120710     c                   else
059100120710     C                   SETON                                        4550
059200120716     c                   move      V1cABi        w1cABi
059300120716     c                   move      V1cCAb        w1cCAb
059400120111     C                   UPDATE    dsf2
059500120111     C                   leavesr
059600120710     C                   endif
059700120111     C                   END
059800120710     C                   endif
059900120111     C* Se non ho indicato la data
060000120111     C* imposto in automatico quella del giorno
060100120111     C     V1cdte        IFEQ      *ZEROS
060200120111     C                   MOVEL     WOGGI         V1cDTe
060300120111     C                   End
060400120111     C*  Giro la data assegno
060500120111     C                   MOVEL     V1cDTe        G02DAT
060600120111     C                   Z-ADD     0             G02INV
060700120111     C                   MOVEL     *BLANKS       G02ERR
060800120111     C                   CALL      'XSRDA8'
060900120111     C                   PARM                    WLBDA8
061000120111     C                   MOVEL     g02dat        v1cdte
061100120111     C     G02ERR        IFEQ      '1'
061200120619     C**                 SETON                                        4328
061300120619     C                   SETON                                        4350
061400120111     C                   UPDATE    dsf2
061500120111     C                   leavesr
061600120111     C                   END
061700120112     C* Controllo data assegno > OGGI se TM o TO e viceversa  ?????
061800120111     C     g02inv        ifle      OGGI
061900120111     c                   if        v1ctic = 'TM' or
062000150522     c                             v1ctic = 'TO' or
062100150522     c                             v1ctic = 'TP'
062200120111     C                   SETON                                        8550
062300120111     C                   UPDATE    dsf2
062400120111     C                   leavesr
062500120111     C                   END
062600120111     c                   else
062700120111     c                   if        v1ctic <>'TM' and
062800150522     c                             v1ctic <>'TO' and
062900150522     c                             v1ctic <>'TP'
063000120111     C                   SETON                                        8650
063100120111     C                   UPDATE    dsf2
063200120111     C                   leavesr
063300120111     C                   END
063400120111     C                   END
063500120111     c* controllo che non ci sia lo stesso assegno
063600120111     c                   movel     g02inv        comdte            8
063700120111     c                   movel     V1cABi        comabi            5
063800120111     c                   movel     V1cCAb        comcab            5
063900120111     c                   eval      comass =  comdte+comabi+comcab+v1cnra
064000120111     c     comass        lookup    sass                                   89
064100120111     c                   if        *in89
064200120111     C                   SETON                                        50
064300120111     c                   leavesr
064400120111     c                   else
064500120111     c                   add       1             xx
064600120111     c                   movel     comass        sass(xx)
064700120111     c                   end
064800120111     c*
064900120111     C                   UPDATE    dsf2
065000120111     c*
065100120111     C                   ENDdo
065200120628     c* se il una spedizione e un assegno errore, non è possibile il
065300120628     c* multiassegno
065400120628     c                   if        nrr1 = 1 and nrass = 1 and
065500120628     c                             totass = totcass and
065600120628     c                             totass <> 0
065700120628     C                   SETON                                        5034
065800120628     c                   z-add     1             nrr2
065900120628     c                   leavesr
066000120628     c                   end
066100120222     c* se la differenza tra assegni e c/assegni è maggiore dei centesimi
066200120222     c* errore
066300120222     c                   eval      comdiffe = totass - totcass
066400120222     c                   if        comdiffe < 0
066500120222     c                   eval      comdiffe = comdiffe * -1
066600120222     c                   end
066700120222     c                   if        comdiffe >=1
066800120222     C                   SETON                                        5032
066900120222     c                   z-add     1             nrr2
067000120628     c                   leavesr
067100120222     c                   else
067200120628     c* se il totale assegni <> da c/ass. chiedo forzatura
067300120111     c                   if        totass <> totcass   and
067400120111     c                             totass <> 0  and not *inkg
067500120111     C                   SETON                                        5031
067600120117     c                   z-add     1             nrr2
067700120628     c                   leavesr
067800120111     c                   else
067900120111     c                   setoff                                       31
068000120111     c                   end
068100120222     c                   end
068200120111     c*
068300120111     C                   ENDSR
068400120111      *---------------------------------------------------------------*
068500120111      *  Loop di ANNULLAMENTO                                         *
068600120111      *---------------------------------------------------------------*
068700120111     C     srannulla     BEGSR
068800120112     C* CANCELLO QUELLO SCRITTO PRIMA
068900120307     c                   if        comkey <> ' '
069000120111     C/EXEC SQL
069100120308     C+ DELETE FROM TNCSM03L WHERE CSmkey = :comkey
069200120307     C+ WITH UR
069300120111     C/END-EXEC
069400120307     c                   end
069500120112     c*
069600120210     c                   if        *inkq or sc30TPCI = 'A'
069700120209     c* per il momento l'annullamento può essere fatto solo in filiale
069800120307     c                   if        sc30ambi = ' '
069900120306     c                   if        comkey <> ' '
070000120112     C/EXEC SQL
070100120307     C+ delete from FNCSS04l
070200120209     C+  WHERE CSSkey = :comkey
070300120307     C+ WITH UR
070400120112     C/END-EXEC
070500120306     c                   end
070600120306     c                   if        comkey =  ' '  and comopz <> ' '
070700120308     c                             and v1cfgs <> 0 and v1cndc <> 0
070800120306     C/EXEC SQL
070900120307     C+ delete from FNCSS04l
071000120306     C+  WHERE CSSopz = :comopz and cssfgs = :v1cfgs and cssndc = :v1cndc
071100120312     C+  AND CSSKEY = ' '
071200120307     C+ WITH UR
071300120306     C/END-EXEC
071400120306     c                   end
071500120209     c                   end
071600120210     c                   eval      sc30erro = 'Q'
071700120111     c                   exsr      endpgm
071800120111     c                   end
071900120111     C*
072000120111     c                   endsr
072100120111      *---------------------------------------------------------------*
072200120111      *  Loop di aggiornamento                                        *
072300120111      *---------------------------------------------------------------*
072400120111     C     aggior        BEGSR
072500120112     c* cancello gli assegni precedentemente inseriti per poi riscriverli
072600120210     c                   if        sc30tpci = 'M'
072700120112     c                   exsr      srannulla
072800120112     c                   end
072900120112     c* prelevo il numeratore per impostare il legame solo in inserimento
073000120113     c                   clear                   o33nrf
073100120210     c                   if        sc30tpci = 'I'
073200120111     c                   do        *hival
073300120111     C                   CLEAR                   TRUL33DS
073400120111     c                   eval      i33tla = 'L'
073500120111     C                   Z-ADD     2             I33CNU
073600120111     C                   Z-ADD     0             I33PO1
073700120111     C                   Z-ADD     1             I33NUM
073800120111     c                   movel(p)  trul33ds      kpjbu
073900120111     c                   call      'TRUL33R'
074000120111     c                   parm                    kpjba
074100120111     c                   movel     kpjbu         trul33ds
074200120111     C                   IF        O33ERR=0
074300120111     c                   leave
074400120111     C                   END
074500120111     C                   ENDdo
074600120112     c* aggiorno il legame sulle spedizioni solo in inserimento
074700120220     c     kleg          setll     fncss04l
074800120112     c                   do        *hival
074900120220     c     kleg          reade     fncss04l
075000120220     c                   if        %eof(fncss04l)
075100120112     c                   leave
075200120112     c                   end
075300120313     c* in inserimento solo quelle senza legame
075400120313     C                   if        csskey <> ' '
075500120313     c                   iter
075600120313     c                   end
075700120220     c                   move      O33Nrf        csskey
075800120220     c                   update    fncss000
075900120112     c                   enddo
076000120112     c                   else
076100120112     c* imposto il numeratore con il legame inserito precedentemente
076200120113     C                   move      comkey        O33Nrf
076300120112     C                   END
076400120112     C* scrivo gli assegni
076500120111     C                   Z-ADD     0             NRR6              4 0
076600120111     c                   do        *hival
076700120111     C                   ADD       1             NRR6
076800120111     C     NRR6          CHAIN     dsf2                               95
076900120111     c                   if        *in95
077000120111     c                   leave
077100120111     c                   end
077200120111     c                   if        v1cidc =0
077300120111     c                   iter
077400120111     c                   end
077500120111     C                   move      O33Nrf        csmkey
077600120111     C                   eval      csmidc =      v1cidc
077700120111     C                   eval      csmdiv =      v1cdiv
077800120111     C                   eval      csmnra =      v1cnra
077900120111     C                   eval      csmabi =      v1cabi
078000120111     C                   eval      csmcab =      v1ccab
078100120111     C                   MOVE      v1cdte        dtaeur
078200120111     C                   MOVE      dtaeur        dtaiso
078300120111     C                   MOVE      dtaiso        csmdte
078400120314     C     V1CTIC        LOOKUP    C1A(X)                                 95
078500120314     C     *IN95         IFEQ      '1'
078600120314     c                   movel     d1a(x)        ds1a
078700120314     C                   end
078800120314     C                   MOVE      §1AFCA        csmTPA
078900120314     c                   move      'M'           csmtpi
079000120111     C                   eval      CSmpru =      knmus
079100120111     C                   eval      csmdatora = WHHDAT
079200120111     c                   clear                   csmatb
079300120111     c                   if        *inkg
079400120111     C                   eval      csmfla =  'S'
079500120111     c                   else
079600120111     c                   clear                   csmfla
079700120111     c                   end
079800120111     c                   write     tncsm000
079900120111     c                   enddo
080000120111     c*
080100120210     c                   if        sc30cmti = 'S'
080200120111     c                   commit
080300120111     c                   end
080400120112     c*
080500120302     c                   eval      sc30keyo = csmkey
080600120111     C                   EXSR      endpgm
080700120111     C                   ENDSR
080800120109     c**********************************************************************
080900990826     C     *inzsr        begsr
081000120109     c**********************************************************************
081100990826     C     *ENTRY        PLIST
081200990826     C                   PARM                    KPJBA
081300120210     c                   movel     kpjbu         tnsc30ds
081400120302     c                   clear                   sc30keyo
081500120307     c                   clear                   sc30msgo
081600120307     c                   clear                   sc30erro
081700120307     c                   eval      comopz = sc30opzi
081800120111     C     *LIKE         DEFINE    TBLKUT        §KUT
081900120111     C     *LIKE         DEFINE    TBLCOD        §COD
082000120111     C     *LIKE         DEFINE    TBLkey        §key
082100120109     C* Giro data del giorno
082200120109     C                   TIME                    WHHDAT           14 0
082300120109     C                   MOVEL     WHHDAT        TIMES             6 0
082400120109     C                   MOVE      WHHDAT        G02DAT
082500120109     C                   MOVEL     *BLANK        G02ERR
082600120109     C                   CALL      'XSRDA8'
082700120109     C                   PARM                    WLBDA8
082800120109     C                   Z-ADD     G02INV        OGGI              8 0
082900120109     C                   Z-ADD     G02DAT        WOGGI             8 0
083000120111     c* verifico i parametri
083100120111     c                   exsr      srparam
083200120109     C* Decodifica Elaboratore
083300120109     c     *dtaara       define    §azute        azuteds
083400120109     c     *dtaara       define    §datiute      ddatiute
083500120109     C                   in(E)     *dtaara
083600120109     C                   IF        %Error  or  RSUT = *blanks
083700120109     C                   call      'TIBS34R'
083800120109     C                   parm                    Tibs34Ds
083900120109     C                   in        *dtaara
084000120109     c                   ENDIF
084100120111     C*-
084200120111     C     kleg          klist
084300120210     C                   kfld                    sc30fgsi
084400120210     C                   kfld                    sc30ndci
084500120307     C                   kfld                    comopz
084600120109     c     ksped         klist
084700120210     c                   kfld                    sc30aani
084800120210     c                   kfld                    sc30lpni
084900120210     c                   kfld                    sc30nrni
085000120210     c                   kfld                    sc30nsni
085100120223     c     kspedv        klist
085200120223     c                   kfld                    sc30aani
085300120223     c                   kfld                    sc30lpni
085400120223     c                   kfld                    sc30nrni
085500120223     c                   kfld                    sc30nsni
085600120223     c                   kfld                    sc30dtvi
085700120223     c                   kfld                    sc30orvi
085800120112     c     kcsb          klist
085900120112     c                   kfld                    csbrgf
086000120112     c                   kfld                    csbndt
086100120112     c                   kfld                    csbddc
086200120109     c     ktab          klist
086300120109     c                   kfld                    TBLKUT
086400120109     c                   kfld                    TBLCOD
086500120109     c                   kfld                    TBLKEY
086600120109     c     ktab1         klist
086700120109     c                   kfld                    TBLKUT
086800120109     c                   kfld                    TBLCOD
086900120109     c     kabi          klist
087000120109     c                   kfld                    V1cABi
087100120109     c                   kfld                    V1cCAb
087200120629     C     KCSA          KLIST
087300120629     C                   KFLD                    KAAS
087400120629     C                   KFLD                    KLNP
087500120629     C                   KFLD                    KNRS
087600120629     C                   KFLD                    KNSP
087700120111     c                   eval      tblkut = 1
087800120109     c*
087900120109     C                   CLEAR                   DS1A
088000120109     C                   Z-ADD     0             X                 3 0
088100120109     C                   MOVEL     '1A'          tblcod
088200120109     C     KTAB1         setll     TABEL00F
088300120109     C                   DO        *hival
088400120109     C     KTAB1         READE     TABEL00F
088500120109     c                   if        %eof(tabel00f)
088600120109     c                   leave
088700120109     C                   END
088800120109     C     X             ifeq      200
088900120109     c                   leave
089000120109     C                   END
089100120109     C     TBLFLG        IFne      ' '
089200120109     c                   iter
089300120109     C                   END
089400120109     C                   MOVEL     TBLUNI        DS1A
089500120109     c* solo mittente
089600120109     C                   IF        §1AFMB <> 'M'
089700120109     c                   iter
089800120109     C                   END
089900120109     c* solo assegno
090000120109     C                   IF        §1ANAS <> ' '
090100120109     c                   iter
090200120109     C                   END
090300120109     c*
090400120109     C                   ADD       1             X
090500120109     C                   MOVEL     TBLKEY        C1A(X)
090600120109     C                   MOVEL     TBLUNI        D1A(X)
090700120109     C                   ENDdo
090800990826     C*
090900990826     C                   endsr
091000020809     C************************************************************
091100120111     C* parametri
091200020809     C************************************************************
091300120111     C     srparam       BEGSR
091400120111     C*
091500120307     c                   if        sc30cmti = 'S' or
091600120307     c                             sc30cmti ='F'
091700120111     c                   eval      cmt = *on
091800120111     c                   else
091900120111     c                   eval      cmt = *off
092000120111     c                   end
092100120307     c* apro i file a seconda dell'ambiente in cui sono
092200120323     c                   seton                                        02
092300120209     c                   select
092400120210     c                   when      sc30ambi = ' '
092500120220     c                   open      fncss01l
092600120220     c                   open      fncss03l
092700120220     c                   open      fncss04l
092800120323     c                   setoff                                       02
092900120210     c                   when      sc30ambi = 'P'
093000120209     c                   open      fncsb01l
093100120223     c                   when      sc30ambi = 'S'
093200120112     c                   open      tncsb03l
093300120112     c                   open      tncsb09l
093400120629     c                   open      tncsa01l
093500120223     c                   when      sc30ambi = 'V'
093600120629     c                   open      tncsa01l
093700120223     c                   open      tncsv01l
093800120307     c                   endsl
093900120209     c*
094000120111     c                   open      tncsm03l
094100120307     c*
094200120210     c                   if        sc30tpci ='V'
094300120113     c                   seton                                        01
094400120113     c                   end
094500120112     C* i parametri obbligatori per poter accedere al pgm sono:
094600120112     c* INSERIMENTO  distinta + opzione
094700120112     c* MODIFICA     spedizione
094800120307     c* VISUALIZZAZ. spedizione
094900120307     c* ANNULLAMENTO OCCULTO spedizione
095000120210     c                   if        sc30TPCi = 'I'
095100120210     c                   if        (sc30fgsi = 0 or sc30ndci = 0 or
095200120210     c                              sc30opzi = ' ' )
095300120210     c                   eval      sc30erro = '1'
095400120210     c                   eval      sc30msgo = 'Errore nei parametri passati'
095500120111     c                   exsr      endpgm
095600120112     c                   end
095700120210     C                   Z-ADD     sc30fgsi      v1Cfgs
095800120210     C                   Z-ADD     sc30ndci      v1Cndc
095900120323     c                   seton                                        03
096000120112     c                   else
096100120307     c* modifica / visualizza / annullamento
096200120210     c                   if         sc30aasi= 0 or
096300120210     c                              sc30lnpi = 0 or
096400120210     c                              sc30nspi = 0
096500120210     c                   eval      sc30erro = '1'
096600120210     c                   eval      sc30msgo = 'Errore nei parametri passati'
096700120111     c                   exsr      endpgm
096800120111     c                   end
096900120307     c*
097000120210     c                   if         sc30aani= 0 or
097100120210     c                              sc30lpni = 0 or
097200120210     c                              sc30nsni = 0
097300120210     c                   eval      sc30aani = sc30aasi
097400120210     c                   eval      sc30lpni = sc30lnpi
097500120210     c                   eval      sc30nrni = sc30nrsi
097600120210     c                   eval      sc30nsni = sc30nspi
097700120112     c                   end
097800120112     c*
097900120209     c                   select
098000120209     c* filiale
098100120210     c                   when      sc30ambi = ' '
098200120307     c* se il legame è blanks vuol dire che ci sono stati
098300120307     c* degli aggiornamenti mozzi ed è presente ancora l'opzione
098400120220     c     ksped         chain     fncss01l
098500120220     c                   if        %found(fncss01l)
098600120307     c* in modifica / visualizzazione errore se non presente legame
098700120307     c* annullamento non importa
098800120307     c                   if        csskey = ' ' and
098900120307     c                             (sc30tpci = 'M' or
099000120307     c                             sc30tpci = 'V')
099100120210     c                   eval      sc30erro = '1'
099200120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
099300120112     c                             presenti dati incompleti'
099400120112     c                   exsr      endpgm
099500120112     c                   end
099600120307     c*
099700120306     c                   eval      comkey  = csskey
099800120112     C                   Z-ADD     cssfgs        v1Cfgs
099900120112     C                   Z-ADD     cssndc        v1Cndc
100000120306     C                   eval      comopz = cssopz
100100120111     c                   else
100200120210     c                   eval      sc30erro = '1'
100300120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
100400120111     c                   exsr      endpgm
100500120111     c                   end
100600120209     c* partenza
100700120210     c                   when      sc30ambi = 'P'
100800120209     c     ksped         chain     fncsb01l
100900120209     c                   if        %found(fncsb01l) and csbnra <> ' ' and
101000120209     c                             csbabi = 0 and csbcab = 0
101100120209     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
101200120307     c* degli aggiornamenti mozzi
101300120209     c     ' '           scan      csbnra                                 99
101400120209     c                   if        *in99
101500120210     c                   eval      sc30erro = '1'
101600120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
101700120209     c                             presenti dati incompleti'
101800120209     c                   exsr      endpgm
101900120209     c                   end
102000120209     c                   eval      comkey = csbnra
102100120209     C                   Z-ADD     0             v1Cfgs
102200120209     C                   Z-ADD     0             v1Cndc
102300120209     c                   else
102400120210     c                   eval      sc30erro = '1'
102500120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
102600120209     c                   exsr      endpgm
102700120209     c                   end
102800120307     c* sede
102900120223     c                   when      sc30ambi = 'S'
103000120112     c     ksped         chain     tncsb03l
103100120112     c                   if        %found(tncsb03l) and csbnra <> ' ' and
103200120112     c                             csbabi = 0 and csbcab = 0
103300120112     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
103400120307     c* degli aggiornamenti mozzi
103500120112     c     ' '           scan      csbnra                                 99
103600120112     c                   if        *in99
103700120210     c                   eval      sc30erro = '1'
103800120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
103900120112     c                             presenti dati incompleti'
104000120112     c                   exsr      endpgm
104100120112     c                   end
104200120112     c                   eval      comkey = csbnra
104300120112     C                   Z-ADD     0             v1Cfgs
104400120112     C                   Z-ADD     0             v1Cndc
104500120112     c                   else
104600120210     c                   eval      sc30erro = '1'
104700120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
104800120112     c                   exsr      endpgm
104900120112     c                   end
105000120307     c* sede variazioni
105100120223     c                   when      sc30ambi = 'V'
105200120223     c     kspedv        chain     tncsv01l
105300120223     c                   if        %found(tncsv01l) and csvnra <> ' ' and
105400120223     c                             csvabi = 0 and csvcab = 0
105500120223     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
105600120307     c* degli aggiornamenti mozzi
105700120223     c     ' '           scan      csvnra                                 99
105800120223     c                   if        *in99
105900120223     c                   eval      sc30erro = '1'
106000120223     c                   eval      sc30msgo = 'Errore nell''annullamento +
106100120223     c                             presenti dati incompleti'
106200120223     c                   exsr      endpgm
106300120223     c                   end
106400120223     c                   eval      comkey = csvnra
106500120223     C                   Z-ADD     0             v1Cfgs
106600120223     C                   Z-ADD     0             v1Cndc
106700120223     c                   else
106800120223     c                   eval      sc30erro = '1'
106900120223     c                   eval      sc30msgo = 'Non trovata la spedizione'
107000120223     c                   exsr      endpgm
107100120223     c                   end
107200120209     c                   endsl
107300120112     c                   end
107400120112     c* in annullamento eseguo in occulto
107500120210     c                   if        sc30tpci = 'A'
107600120112     c                   exsr      srannulla
107700120112     c                   end
107800120112     C*
107900120111     C                   endsr
108000120111     C************************************************************
108100120111     C* FINE PROGRAMMA
108200120111     C************************************************************
108300120111     C     ENDPGM        BEGSR
108400120313     c                   if        sc30ambi = ' '
108500120307     c                   unlock    fncss04l
108600120313     c                   end
108700120307     c                   unlock    tncsm03l
108800120210     c                   movel     tnsc30ds      kpjbu
108900020809     C                   SETON                                            LR
109000020809     C                   RETURN
109100020809     C                   ENDSR
109200020812
