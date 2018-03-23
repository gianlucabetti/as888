000100120109      /TITLE Incasso assegni mittente
000200120110     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400020812
000500120220     ffncss01l  if   e           k disk    rename(fncss000:fncss001)
000600120112     f                                     usropn
000700120209     ffncsb01l  if   e           k disk    usropn
000800120223     ftncsv01l  if   e           k disk    usropn
000900120223     ftncsb03l  if   e           k disk    usropn
001000120112     ftncsb09l  if   e           k disk    rename(tncsb000:tncsb009)
001100120112     f                                     usropn
001200120111     ftncsm02l  if   e           k disk    rename(tncsm000:tncsm002)
001300120220     ffncss03l  if   e           k disk    usropn
001400120220     F                                     rename(fncss000:fncss003)
001500120220     ffncss04l  uf a e           k disk    commit(cmt) usropn
001600120111     ftncsm03l  uf a e           k disk    commit(cmt) usropn
001700120109     fcnabi01l  if   e           k disk
001800120109     ftabel00f  if   e           k disk
001900120210     Ftnsc30d   CF   E             WORKSTN
002000000000     F                                     SFILE(DSF1:NRR1)
002100000000     F                                     SFILE(DSF2:NRR2)
002200120210     D tnsc30ds      E DS
002300120209     D fncsbds       E DS                  extname(fncsb00f)
002400120223     D tncsvds       E DS                  extname(tncsv00f)
002500120111     c* numeratori
002600120111     d trul33ds      e ds
002700120109     D ddatiute      e ds
002800120110     D tncsmds       e ds                  extname(TNCSM00F)
002900120109     D azuteds       e ds                  extname(AZUTE00F)
003000120109     D tibs34ds      E DS                  inz
003100120109     D ds1a          E DS                  inz
003200120109     D dstm          E DS                  inz
003300120109     D WLBDA8          DS
003400120109     D  G02DAT                 1      8  0
003500120109     D  G02INV                 9     16  0
003600120109     D  G02ERR                17     17
003700120109     D  G02TGI                18     22  0
003800000000     D KPJBA         E DS
003900120111     D cmt             S              1
004000120109     D xx              S              2  0
004100120109     D D1A             S             78    DIM(200)
004200120109     D C1A             S              2    DIM(200)
004300120111     D totass          S                   like(v1cidc)
004400120111     D totcass         S                   like(v1cidc)
004500120222     D comdiffe        S                   like(v1cidc)
004600120109     D dtaeur          S               d   datfmt(*eur)
004700120109     D dtaiso          S               d   datfmt(*iso)
004800120111     D sass            S             28    DIM(200)
004900120111     D comass          S             28
005000120220     D comkey          S             10
005101120306     D comopz          S             10
005200000111
005300920312     C*-----
005400120110     c                   exsr      srcarsfl
005500120110     C* almeno una spedizione                              *
005600000000     C     PIENO         IFEQ      *BLANK
005700120210     c                   eval      sc30erro = '1'
005800120210     c                   eval      sc30msgo = 'Nessuna spedizione inserita'
005900120109     c                   exsr      endpgm
006000120109     c                   end
006100000000     C*--- EMISSIONE SUBFILE                               *
006200120110?    C                   do        *hival
006300910522     C                   SETOFF                                       6465
006400000000     C     NRR1          IFEQ      0
006500000000     C                   SETOFF                                           61
006600000000     C                   END
006700000000     C     NRR2          IFEQ      0
006800000000     C                   SETOFF                                           62
006900000000     C                   END
007000020812
007100000000     C                   WRITE     CB36D2
007200020813     C                   WRITE     DCT1
007300000000     C                   EXFMT     DCT2
007400120113     C*                  READ      DCT1                                   63
007500120109      *  Guida
007600120109     C     *INKL         IFEQ      '1'
007601120313     c                   eval      sc30keyo = comkey
007700120210     c                   eval      sc30erro = 'L'
007800120109     C                   exsr      endpgm
007900120109     C                   END
008000120109     c* annullamento
008100120209     c   kq              exsr      srannulla
008200120110     C*...Controllo i dati modificati
008300000000?    C                   EXSR      CONTA
008400120110     C*...per ERRORE torno ad emettere il sfl.
008500120110     c                   if        *in50
008600120111     C                   MOVE      NRR2          RECNB2
008700120111     c                   iter
008800120111     c                   end
008900120110     C* aggiornamento                                      *
009000120111     C                   IF        *inkf or *inkg
009100120111?    C                   EXSR      AGGIOR
009200120111     c                   end
009300120110     c                   enddo
009400120110     C*
009500920312     C                   SETON                                        LR
009600120110     c**********************************************************************
009700120110     C*...Inizializzo i sub-file.
009800120110     c**********************************************************************
009900120110     c     srcarsfl      begsr
010000120110     C                   Z-ADD     1             RECNBR
010100120111     C                   Z-ADD     1             RECNB2
010200120110     C                   Z-ADD     0             NRR2              5 0
010300120110     C                   Z-ADD     0             NUMRE2
010400120110     C                   Z-ADD     0             NRR1              5 0
010500120110     C                   Z-ADD     0             NUMREC
010600120110     C                   SETON                                        60
010700120110     C                   SETOFF                                       61
010800120110     C                   SETOFF                                       62
010900120110     C                   WRITE     DCT1
011000120110     C                   WRITE     DCT2
011100120110     C                   SETON                                        61
011200120110     C                   SETON                                        62
011300120111     C                   SETOFF                                       60
011400120110     C*...Carico il sub-file delle spedizioni              *
011500120110     C                   MOVEL     *BLANK        PIENO
011600120110     C                   Z-ADD     0             NRR1
011700120110     C                   Z-ADD     0             NUMREC
011800120111     C                   Z-ADD     0             totcass
011900120222     C                   Z-ADD     0             comdiffe
012000120113     c* filiale
012100120209     c                   select
012200120210     c                   when      sc30ambi = ' '
012300120113     c* inserimento (distinta + opzione)
012400120210     c                   if        sc30tpci = 'I'
012500120220     C     kleg          SETLL     fncss04l
012600120113     c                   else
012700120113     c* no inserimento (legame)
012800120220     C     comkey        SETLL     fncss03l
012900120113     c                   end
013000120110?    C                   do        *hival
013100120210     c                   if        sc30tpci = 'I'
013200120306     C     kleg          READE(n)  fncss04l
013300120220     C                   if        %eof(fncss04l)
013400120220     c                   leave
013500120220     c                   end
013501120312     c* in inserimento solo quelle senza legame
013502120312     C                   if        csskey <> ' '
013503120312     c                   iter
013504120312     c                   end
013600120113     c                   else
013700120306     C     comkey        READE     fncss03l
013800120220     C                   if        %eof(fncss03l)
013900120110     c                   leave
014000120110     c                   end
014100120220     c                   end
014200120110     c                   if        cssatb <> ' '
014300120110     c                   iter
014400120110     c                   end
014500120110     C*
014600120110?    C                   EXSR      CARSB1
014700120110     C                   enddo
014800120209     c* partenza
014900120210     c                   when      sc30ambi ='P'
015000120209     C     ksped         chain     fncsb01l
015100120209     c                   if        %found(fncsb01l)
015200120209?    C                   EXSR      CARSB1P
015300120209     c                   end
015400120223     c* partenza
015500120223     c                   when      sc30ambi ='S'
015600120112     c* sede
015700120112     C     kcsb          SETLL     tncsb09l
015800120112?    C                   do        *hival
015900120112     C     kcsb          READE     tncsb09l
016000120112     C                   if        %eof(tncsb09l)
016100120112     c                   leave
016200120112     c                   end
016300120112     c* solo il contrassegno con legame
016400120112     c                   if        csbnra <> comkey
016500120112     c                   iter
016600120112     c                   end
016700120112     C*
016800120112?    C                   EXSR      CARSB1S
016900120112     C                   enddo
017000120223     c* storico sede
017100120223     c                   when      sc30ambi ='V'
017200120223     C/EXEC SQL
017300120223     C+ DECLARE TNCSVC CURSOR FOR SELECT * FROM tncsv00f WHERE csvnra =
017400120322     C+ :comkey and CSVDTV = :sc30dtvi
017500120223     C/END-EXEC
017600120223
017700120223     C/EXEC SQL
017800120223     C+ OPEN tncsvc
017900120223     C/END-EXEC
018000120223
018100120223     C                   DOU       sqlcod <> 0
018200120223
018300120223     C/EXEC SQL
018400120223     C+ FETCH NEXT FROM TNCSvC INTO : tncsvds
018500120223     C/END-EXEC
018600120223
018700120223     C                   SELECT
018800120223     C                   WHEN      SQLCod = 100
018900120223     C                   WHEN      SQLCod < 0
019000120223     C                   EVAL      *INH1 = *ON
019100120223     C                   OTHER
019200120223     c* stampa testata e apre membro
019300120223     c                   exsr      carsb1v
019400120223     C                   ENDSL
019500120223
019600120223     C                   ENDDO
019700120223     C/EXEC SQL
019800120223     C+ CLOSE tncsvc
019900120223     C/END-EXEC
020000120209     c                   endsl
020100120110     C*...Carico il sub-file degli assegni                 *
020200120110     C                   MOVEL     *BLANK        PIEN1
020300120110     C                   Z-ADD     0             NRR2
020400120110     C                   Z-ADD     0             NUMRE2
020500120111     C     comkey        SETLL     tncsm03l
020600120110?    C                   do        *hival
020700120111     C     comkey        READE     tncsm03l
020800120110     C                   if        %eof(tncsm03l)
020900120110     c                   leave
021000120110     c                   end
021100120110     c                   if        csmatb <> ' '
021200120110     c                   iter
021300120110     c                   end
021400120110     c*
021500120110?    C                   EXSR      CARSB2
021600120110     C                   ENDdo
021700120110     c* carico 21 righe vuote nel sfl degli assegni
021800120210     c                   if        sc30tpci <> 'V'
021900120110     c                   do        21
022000120110     c                   clear                   tncsmds
022100120110?    C                   EXSR      CARSB2
022200120110     c                   enddo
022300120111     c                   end
022400120110     c                   endsr
022500000000     C*---------------------------------------------------------------*---
022600120110     C*  CARSB1:   CARICA SUBFILE SPEDIZIONI                          *
022700000000     C*---------------------------------------------------------------*
022800000000     C     CARSB1        BEGSR
022900000000     C*
023000120109     C*...Carico riga spedizione
023100120110     c                   eval      v1caas = cssaas
023200120111     c                   eval      v1clnp = csslnp
023300120110     c                   eval      v1cnrs = cssnrs
023400120110     c                   eval      v1cnsp = cssnsp
023500120110     c                   eval      v1caao = cssaao
023600120110     c                   eval      v1clpo = csslno
023700120110     c                   eval      v1cnro = cssnro
023800120110     c                   eval      v1cnso = CSSNSo
023900120110     c                   eval      v1cvca = CSSvca
024000120110     C                   Z-ADD     csscas        v1ccas
024100120111     c                   add       csscas        totcass
024200120110     c* mi servirà nel sfl degli assegni
024300120110     c                   eval      v1ctic = csstic
024400000000     C                   ADD       1             NRR1
024500000000     C                   ADD       1             NUMREC            4 0
024600000000     C                   WRITE     DSF1
024700000000     C                   MOVEL     '1'           PIENO             1
024800000000     C*                                                    *
024900000000     C                   ENDSR
025000120112     C*---------------------------------------------------------------*---
025100120112     C*  CARSB1S   CARICA SUBFILE SPEDIZIONI da sede                  *
025200120112     C*---------------------------------------------------------------*
025300120112     C     CARSB1S       BEGSR
025400120112     C*
025500120112     C*...Carico riga spedizione
025600120112     c                   eval      v1caas = csbaas
025700120112     c                   eval      v1clnp = csblnp
025800120112     c                   eval      v1cnrs = csbnrs
025900120112     c                   eval      v1cnsp = csbnsp
026000120112     c                   eval      v1cvca = CSbvca
026100120117     C                   Z-ADD     csbcas        v1ccas
026200120117     c                   add       csbcas        totcass
026300120112     c* mi servirà nel sfl degli assegni
026400120112     c                   movel     csbtpa        v1ctic
026500120112     c                   move      csbtpi        v1ctic
026600120112     C                   ADD       1             NRR1
026700120112     C                   ADD       1             NUMREC            4 0
026800120112     C                   WRITE     DSF1
026900120112     C                   MOVEL     '1'           PIENO             1
027000120112     C*                                                    *
027100120112     C                   ENDSR
027200120223     C*---------------------------------------------------------------*---
027300120223     C*  CARSB1v   CARICA SUBFILE SPEDIZIONI da variazioni di sede    *
027400120223     C*---------------------------------------------------------------*
027500120223     C     CARSB1v       BEGSR
027600120223     C*
027700120223     C*...Carico riga spedizione
027800120223     c                   eval      v1caas = csvaas
027900120223     c                   eval      v1clnp = csvlnp
028000120223     c                   eval      v1cnrs = csvnrs
028100120223     c                   eval      v1cnsp = csvnsp
028200120223     c                   eval      v1cvca = CSvvca
028300120223     C                   Z-ADD     csvcas        v1ccas
028400120223     c                   add       csvcas        totcass
028500120223     c* mi servirà nel sfl degli assegni
028600120223     c                   movel     csvtpa        v1ctic
028700120223     c                   move      csvtpi        v1ctic
028800120223     C                   ADD       1             NRR1
028900120223     C                   ADD       1             NUMREC            4 0
029000120223     C                   WRITE     DSF1
029100120223     C                   MOVEL     '1'           PIENO             1
029200120223     C*                                                    *
029300120223     C                   ENDSR
029400120209     C*---------------------------------------------------------------*---
029500120209     C*  CARSB1S   CARICA SUBFILE SPEDIZIONI da filiale partenza      *
029600120209     C*---------------------------------------------------------------*
029700120209     C     CARSB1p       BEGSR
029800120209     C*
029900120209     C/EXEC SQL
030000120209     C+ Declare A1 Cursor for
030100120209     C+ SELECT *
030200120209     C+ from fncsb00f
030300120209     C+ where csbnra = : comkey
030400120209     C/END-EXEC
030500120209      *          apertura cursore
030600120209     C/EXEC SQL
030700120209     C+ OPEN A1
030800120209     C/END-EXEC
030900120209     C                   DO        *hival
031000120209      *          lettura cursore
031100120209     C/EXEC SQL
031200120209     C+ Fetch Next From A1 Into
031300120209     C+ :fncsbds
031400120209     C/END-EXEC
031500120209     C                   SELECT
031600120209     C                   WHEN      SqlCod = 100
031700120209     C                   LEAVE
031800120209     C                   WHEN      SqlCod >= 0
031900120209     C*...Carico riga spedizione
032000120209     c                   exsr      CARSB1S
032100120209     C                   OTHER
032200120209     C                   ENDSL
032300120209      *
032400120209     C                   ENDDO
032500120209     C/EXEC SQL
032600120209     C+ Close A1
032700120209     C/END-EXEC
032800120209     C*                                                    *
032900120209     C                   ENDSR
033000000000     C*---------------------------------------------------------------*---
033100000000     C*  CARSB2:   CARICA SUBFILE DELLA CONTABILITA'                  *
033200000000     C*---------------------------------------------------------------*
033300000000     C     CARSB2        BEGSR
033400000000     C*
033500120110     C                   Z-ADD     csmidc        v1cidc
033600120110     c                   movel     csmdiv        v1cdiv
033700120113     c                   if        csmdiv = ' '
033800120113     c                   eval      v1cdiv = 'EUR'
033900120113     c                   end
034000120111     c                   movel     csmnra        v1cnra
034100120110     c                   z-add     csmabi        v1cabi
034200120110     c                   z-add     csmcab        v1ccab
034300120111     c                   if        csmdte = 0
034400120111     c                   clear                   v1cdte
034500120111     c                   else
034600120111     C                   move      csmdte        dtaiso
034700120111     C                   move      dtaiso        dtaeur
034800120111     C                   move      dtaeur        v1cdte
034900120111     c                   end
035000990830
035100000000     C                   ADD       1             NRR2
035200000000     C                   ADD       1             NUMRE2            4 0
035300000000     C                   WRITE     DSF2
035400000000     C                   MOVEL     '1'           PIEN1             1
035500000000     C*
035600000000     C                   ENDSR
035700120111      *---------------------------------------------------------------*
035800120111      *  Loop di errori  subfile errori                               *
035900120111      *---------------------------------------------------------------*
036000120111     C     Conta         BEGSR
036100120111     c                   clear                   xx
036200120111     c                   clear                   sass
036300120111     c                   clear                   totass
036400120222     c                   clear                   comdiffe
036500120111     c                   setoff                                       313940
036600120111     c                   setoff                                       414243
036700120111     c                   setoff                                       448986
036800120117     c                   setoff                                       858150
036900120222     c                   setoff                                       32
037000120111     C                   Z-ADD     0             NRR6              4 0
037100120111      *
037200120111     c                   do        *hival
037300120111     C                   ADD       1             NRR6
037400120111     C     NRR6          CHAIN     dsf2                               95
037500120111     c                   if        *in95
037600120111     c                   leave
037700120111     c                   end
037800120111     c*
037900120111     c                   if        v1cidc = 0 and
038000120117     c                             v1cnra = ' ' and
038100120117     c                             v1cabi = 0 and
038200120111     c                             v1ccab = 0 and
038300120111     c                             v1cdte = 0
038400120117     C                   UPDATE    dsf2
038500120111     c                   iter
038600120111     c                   end
038700120111     C*  Controllo se richiesta ricerca tipo incasso
038800120111     C     '?'           SCAN      V1CTIC                                 95
038900120111     C     *IN95         IFEQ      '1'
039000120111     C                   MOVEL     1             §KUT
039100120111     C                   MOVEL     '1A'          §COD
039200120111     C                   MOVE      *BLANKS       V1CTIC
039300120111     C                   CALL      'X§TABER'
039400120111     C                   PARM                    §KUT
039500120111     C                   PARM                    §COD
039600120111     C                   PARM                    §KEY
039700120111     C                   PARM                    §DES             30
039800120111     C                   MOVEL     §KEY          V1CTIC
039900120111     C                   END
040000120111     C*  Controllo se corretto tipo incasso
040100120111     c                   if        v1ctic = ' '
040200120111     C                   SETON                                        4050
040300120111     C                   UPDATE    dsf2
040400120111     C                   leavesr
040500120111     c                   else
040600120111     C                   Z-ADD     1             X
040700120111     C     V1CTIC        LOOKUP    C1A(X)                                 95
040800120111     C     *IN95         IFEQ      '0'
040900120111     C                   SETON                                        4050
041000120111     C                   UPDATE    dsf2
041100120111     C                   leavesr
041200120111     c                   else
041300120111     c                   movel     d1a(x)        ds1a
041400120111     C                   end
041500120111     C                   end
041600120111     C*  Controllo se cliente abilitato
041700120117     c                   if        §1ATTM ='S'
041800120111     C                   CLEAR                   DSTM
041900120111     C                   MOVEL     'TM'          tblcod
042000120117     c* filiale
042100120210     c                   if        sc30ambi = ' '
042200120111     C                   MOVEL     cssccm        tblkey
042300120117     c                   else
042400120117     C                   MOVEL     csbcdi        tblkey
042500120117     c                   end
042600120117     C     KTAB          CHAIN     TABEL00F
042700120111     c                   if        %found(tabel00f) and tblflg = ' '
042800120111     C                   MOVEL     tbluni        DSTM
042900120111     c                   else
043000120117     C                   SETON                                        8150
043100120111     C                   UPDATE    dsf2
043200120111     C                   leavesr
043300120111     c                   end
043400120111     C     V1CTIC        IFEQ      'TM'
043500120111     C     §tmftm        ANDNE     'S'
043600120111     C     V1CTIC        OREQ      'TO'
043700120111     C     §TMFTO        ANDNE     'S'
043800120111     C     V1CTIC        OREQ      'CA'
043900120111     C     §TMFCA        ANDNE     'S'
044000120111     C     V1CTIC        OREQ      'BP'
044100120111     C     §TMFbp        ANDNE     'S'
044200120117     C                   SETON                                        8150
044300120111     C                   UPDATE    dsf2
044400120111     C                   leavesr
044500120111     C                   END
044600120117     C                   END
044700120111     c* importo
044800120111     c                   if        v1cidc = 0
044900120111     C                   SETON                                        3950
045000120111     C                   UPDATE    dsf2
045100120111     C                   leavesr
045200120111     c                   else
045300120111     c                   add       v1cidc        totass
045400120111     c                   end
045500120111     c* numero assegno
045600120111     C     V1cnra        IFEQ      *BLANKS
045700120111     C     V1cnra        OREQ      *ZEROS
045800120111     C                   SETON                                        4150
045900120111     C                   UPDATE    dsf2
046000120111     C                   leavesr
046100120111     C                   END
046200120111     c* se tipo incasso non ammette assegni postali controllo abi  07601
046300120111     c     §1anap        ifeq      'S'
046400120111     c     v1cabi        andeq     07601
046500120111     C                   SETON                                        4250
046600120111     C                   UPDATE    dsf2
046700120111     C                   leavesr
046800120111     c                   endif
046900120111     C*  ABI e CAB
047000120111     C     V1cABi        IFle      *ZEROS
047100120111     C     V1cCAb        ORle      *ZEROS
047200120111     C                   SETON                                        4450
047300120111     C                   UPDATE    dsf2
047400120111     C                   leavesr
047500120111     C                   End
047600120111     C*  Se immesso Abi-CAb verifico congruenza
047700120111     C     KABI          CHAIN     CNABI01L
047800120111     c                   if        not %found(cnabi01l)
047900120111     C                   SETON                                        4450
048000120111     C                   UPDATE    dsf2
048100120111     C                   leavesr
048200120111     C                   END
048300120111     C* Se non ho indicato la data
048400120111     C* imposto in automatico quella del giorno
048500120111     C     V1cdte        IFEQ      *ZEROS
048600120111     C                   MOVEL     WOGGI         V1cDTe
048700120111     C                   End
048800120111     C*  Giro la data assegno
048900120111     C                   MOVEL     V1cDTe        G02DAT
049000120111     C                   Z-ADD     0             G02INV
049100120111     C                   MOVEL     *BLANKS       G02ERR
049200120111     C                   CALL      'XSRDA8'
049300120111     C                   PARM                    WLBDA8
049400120111     C                   MOVEL     g02dat        v1cdte
049500120111     C     G02ERR        IFEQ      '1'
049600120111     C                   SETON                                        4328
049700120111     C                   UPDATE    dsf2
049800120111     C                   leavesr
049900120111     C                   END
050000120112     C* Controllo data assegno > OGGI se TM o TO e viceversa  ?????
050100120111     C     g02inv        ifle      OGGI
050200120111     c                   if        v1ctic = 'TM' or
050300120111     c                             v1ctic = 'TO'
050400120111     C                   SETON                                        8550
050500120111     C                   UPDATE    dsf2
050600120111     C                   leavesr
050700120111     C                   END
050800120111     c                   else
050900120111     c                   if        v1ctic <>'TM' and
051000120111     c                             v1ctic <>'TO'
051100120111     C                   SETON                                        8650
051200120111     C                   UPDATE    dsf2
051300120111     C                   leavesr
051400120111     C                   END
051500120111     C                   END
051600120111     c* controllo che non ci sia lo stesso assegno
051700120111     c                   movel     g02inv        comdte            8
051800120111     c                   movel     V1cABi        comabi            5
051900120111     c                   movel     V1cCAb        comcab            5
052000120111     c                   eval      comass =  comdte+comabi+comcab+v1cnra
052100120111     c     comass        lookup    sass                                   89
052200120111     c                   if        *in89
052300120111     C                   SETON                                        50
052400120111     c                   leavesr
052500120111     c                   else
052600120111     c                   add       1             xx
052700120111     c                   movel     comass        sass(xx)
052800120111     c                   end
052900120111     c*
053000120111     C                   UPDATE    dsf2
053100120111     c*
053200120111     C                   ENDdo
053300120222     c* se la differenza tra assegni e c/assegni è maggiore dei centesimi
053400120222     c* errore
053500120222     c                   eval      comdiffe = totass - totcass
053600120222     c                   if        comdiffe < 0
053700120222     c                   eval      comdiffe = comdiffe * -1
053800120222     c                   end
053900120222     c                   if        comdiffe >=1
054000120222     C                   SETON                                        5032
054100120222     c                   z-add     1             nrr2
054200120222     c                   else
054300120222     c*  se il totale assegni <> da c/ass. chiedo forzatura
054400120111     c                   if        totass <> totcass   and
054500120111     c                             totass <> 0  and not *inkg
054600120111     C                   SETON                                        5031
054700120117     c                   z-add     1             nrr2
054800120111     c                   else
054900120111     c                   setoff                                       31
055000120111     c                   end
055100120222     c                   end
055200120111     c*
055300120111     C                   ENDSR
055400120111      *---------------------------------------------------------------*
055500120111      *  Loop di ANNULLAMENTO                                         *
055600120111      *---------------------------------------------------------------*
055700120111     C     srannulla     BEGSR
055800120112     C* CANCELLO QUELLO SCRITTO PRIMA
055801120307     c                   if        comkey <> ' '
055900120111     C/EXEC SQL
056000120308     C+ DELETE FROM TNCSM03L WHERE CSmkey = :comkey
056001120307     C+ WITH UR
056200120111     C/END-EXEC
056201120307     c                   end
056300120112     c*
056400120210     c                   if        *inkq or sc30TPCI = 'A'
056500120209     c* per il momento l'annullamento può essere fatto solo in filiale
056501120307     c                   if        sc30ambi = ' '
056601120306     c                   if        comkey <> ' '
056700120112     C/EXEC SQL
056800120307     C+ delete from FNCSS04l
056900120209     C+  WHERE CSSkey = :comkey
056901120307     C+ WITH UR
057000120112     C/END-EXEC
057001120306     c                   end
057002120306     c                   if        comkey =  ' '  and comopz <> ' '
057003120308     c                             and v1cfgs <> 0 and v1cndc <> 0
057004120306     C/EXEC SQL
057005120307     C+ delete from FNCSS04l
057006120306     C+  WHERE CSSopz = :comopz and cssfgs = :v1cfgs and cssndc = :v1cndc
057007120312     C+  AND CSSKEY = ' '
057008120307     C+ WITH UR
057009120306     C/END-EXEC
057010120306     c                   end
057100120209     c                   end
057200120210     c                   eval      sc30erro = 'Q'
057300120111     c                   exsr      endpgm
057400120111     c                   end
057500120111     C*
057600120111     c                   endsr
057700120111      *---------------------------------------------------------------*
057800120111      *  Loop di aggiornamento                                        *
057900120111      *---------------------------------------------------------------*
058000120111     C     aggior        BEGSR
058100120112     c* cancello gli assegni precedentemente inseriti per poi riscriverli
058200120210     c                   if        sc30tpci = 'M'
058300120112     c                   exsr      srannulla
058400120112     c                   end
058500120112     c* prelevo il numeratore per impostare il legame solo in inserimento
058600120113     c                   clear                   o33nrf
058700120210     c                   if        sc30tpci = 'I'
058800120111     c                   do        *hival
058900120111     C                   CLEAR                   TRUL33DS
059000120111     c                   eval      i33tla = 'L'
059100120111     C                   Z-ADD     2             I33CNU
059200120111     C                   Z-ADD     0             I33PO1
059300120111     C                   Z-ADD     1             I33NUM
059400120111     c                   movel(p)  trul33ds      kpjbu
059500120111     c                   call      'TRUL33R'
059600120111     c                   parm                    kpjba
059700120111     c                   movel     kpjbu         trul33ds
059800120111     C                   IF        O33ERR=0
059900120111     c                   leave
060000120111     C                   END
060100120111     C                   ENDdo
060200120112     c* aggiorno il legame sulle spedizioni solo in inserimento
060300120220     c     kleg          setll     fncss04l
060400120112     c                   do        *hival
060500120220     c     kleg          reade     fncss04l
060600120220     c                   if        %eof(fncss04l)
060700120112     c                   leave
060800120112     c                   end
060801120313     c* in inserimento solo quelle senza legame
060802120313     C                   if        csskey <> ' '
060803120313     c                   iter
060804120313     c                   end
060900120220     c                   move      O33Nrf        csskey
061000120220     c                   update    fncss000
061100120112     c                   enddo
061200120112     c                   else
061300120112     c* imposto il numeratore con il legame inserito precedentemente
061400120113     C                   move      comkey        O33Nrf
061500120112     C                   END
061600120112     C* scrivo gli assegni
061700120111     C                   Z-ADD     0             NRR6              4 0
061800120111     c                   do        *hival
061900120111     C                   ADD       1             NRR6
062000120111     C     NRR6          CHAIN     dsf2                               95
062100120111     c                   if        *in95
062200120111     c                   leave
062300120111     c                   end
062400120111     c                   if        v1cidc =0
062500120111     c                   iter
062600120111     c                   end
062700120111     C                   move      O33Nrf        csmkey
062800120111     C                   eval      csmidc =      v1cidc
062900120111     C                   eval      csmdiv =      v1cdiv
063000120111     C                   eval      csmnra =      v1cnra
063100120111     C                   eval      csmabi =      v1cabi
063200120111     C                   eval      csmcab =      v1ccab
063300120111     C                   MOVE      v1cdte        dtaeur
063400120111     C                   MOVE      dtaeur        dtaiso
063500120111     C                   MOVE      dtaiso        csmdte
063701120314     C     V1CTIC        LOOKUP    C1A(X)                                 95
063702120314     C     *IN95         IFEQ      '1'
063707120314     c                   movel     d1a(x)        ds1a
063708120314     C                   end
063709120314     C                   MOVE      §1AFCA        csmTPA
063710120314     c                   move      'M'           csmtpi
063800120111     C                   eval      CSmpru =      knmus
063900120111     C                   eval      csmdatora = WHHDAT
064000120111     c                   clear                   csmatb
064100120111     c                   if        *inkg
064200120111     C                   eval      csmfla =  'S'
064300120111     c                   else
064400120111     c                   clear                   csmfla
064500120111     c                   end
064600120111     c                   write     tncsm000
064700120111     c                   enddo
064800120111     c*
064900120210     c                   if        sc30cmti = 'S'
065000120111     c                   commit
065100120111     c                   end
065200120112     c*
065201120302     c                   eval      sc30keyo = csmkey
065300120111     C                   EXSR      endpgm
065400120111     C                   ENDSR
065500120109     c**********************************************************************
065600990826     C     *inzsr        begsr
065700120109     c**********************************************************************
065800990826     C     *ENTRY        PLIST
065900990826     C                   PARM                    KPJBA
066000120210     c                   movel     kpjbu         tnsc30ds
066100120302     c                   clear                   sc30keyo
066101120307     c                   clear                   sc30msgo
066102120307     c                   clear                   sc30erro
066200120307     c                   eval      comopz = sc30opzi
066300120111     C     *LIKE         DEFINE    TBLKUT        §KUT
066400120111     C     *LIKE         DEFINE    TBLCOD        §COD
066500120111     C     *LIKE         DEFINE    TBLkey        §key
066800120109     C* Giro data del giorno
066900120109     C                   TIME                    WHHDAT           14 0
067000120109     C                   MOVEL     WHHDAT        TIMES             6 0
067100120109     C                   MOVE      WHHDAT        G02DAT
067200120109     C                   MOVEL     *BLANK        G02ERR
067300120109     C                   CALL      'XSRDA8'
067400120109     C                   PARM                    WLBDA8
067500120109     C                   Z-ADD     G02INV        OGGI              8 0
067600120109     C                   Z-ADD     G02DAT        WOGGI             8 0
067700120111     c* verifico i parametri
067800120111     c                   exsr      srparam
067900120109     C* Decodifica Elaboratore
068000120109     c     *dtaara       define    §azute        azuteds
068100120109     c     *dtaara       define    §datiute      ddatiute
068200120109     C                   in(E)     *dtaara
068300120109     C                   IF        %Error  or  RSUT = *blanks
068400120109     C                   call      'TIBS34R'
068500120109     C                   parm                    Tibs34Ds
068600120109     C                   in        *dtaara
068700120109     c                   ENDIF
068800120111     C*-
068900120111     C     kleg          klist
069000120210     C                   kfld                    sc30fgsi
069100120210     C                   kfld                    sc30ndci
069200120307     C                   kfld                    comopz
069300120109     c     ksped         klist
069400120210     c                   kfld                    sc30aani
069500120210     c                   kfld                    sc30lpni
069600120210     c                   kfld                    sc30nrni
069700120210     c                   kfld                    sc30nsni
069800120223     c     kspedv        klist
069900120223     c                   kfld                    sc30aani
070000120223     c                   kfld                    sc30lpni
070100120223     c                   kfld                    sc30nrni
070200120223     c                   kfld                    sc30nsni
070300120223     c                   kfld                    sc30dtvi
070400120223     c                   kfld                    sc30orvi
070500120112     c     kcsb          klist
070600120112     c                   kfld                    csbrgf
070700120112     c                   kfld                    csbndt
070800120112     c                   kfld                    csbddc
070900120109     c     ktab          klist
071000120109     c                   kfld                    TBLKUT
071100120109     c                   kfld                    TBLCOD
071200120109     c                   kfld                    TBLKEY
071300120109     c     ktab1         klist
071400120109     c                   kfld                    TBLKUT
071500120109     c                   kfld                    TBLCOD
071600120109     c     kabi          klist
071700120109     c                   kfld                    V1cABi
071800120109     c                   kfld                    V1cCAb
071900120111     c                   eval      tblkut = 1
072000120109     c*
072100120109     C                   CLEAR                   DS1A
072200120109     C                   Z-ADD     0             X                 3 0
072300120109     C                   MOVEL     '1A'          tblcod
072400120109     C     KTAB1         setll     TABEL00F
072500120109     C                   DO        *hival
072600120109     C     KTAB1         READE     TABEL00F
072700120109     c                   if        %eof(tabel00f)
072800120109     c                   leave
072900120109     C                   END
073000120109     C     X             ifeq      200
073100120109     c                   leave
073200120109     C                   END
073300120109     C     TBLFLG        IFne      ' '
073400120109     c                   iter
073500120109     C                   END
073600120109     C                   MOVEL     TBLUNI        DS1A
073700120109     c* solo mittente
073800120109     C                   IF        §1AFMB <> 'M'
073900120109     c                   iter
074000120109     C                   END
074100120109     c* solo assegno
074200120109     C                   IF        §1ANAS <> ' '
074300120109     c                   iter
074400120109     C                   END
074500120109     c*
074600120109     C                   ADD       1             X
074700120109     C                   MOVEL     TBLKEY        C1A(X)
074800120109     C                   MOVEL     TBLUNI        D1A(X)
074900120109     C                   ENDdo
075000990826     C*
075100990826     C                   endsr
075200020809     C************************************************************
075300120111     C* parametri
075400020809     C************************************************************
075500120111     C     srparam       BEGSR
075600120111     C*
075700120307     c                   if        sc30cmti = 'S' or
075701120307     c                             sc30cmti ='F'
075800120111     c                   eval      cmt = *on
075900120111     c                   else
076000120111     c                   eval      cmt = *off
076100120111     c                   end
076200120307     c* apro i file a seconda dell'ambiente in cui sono
076201120323     c                   seton                                        02
076300120209     c                   select
076400120210     c                   when      sc30ambi = ' '
076500120220     c                   open      fncss01l
076600120220     c                   open      fncss03l
076700120220     c                   open      fncss04l
076701120323     c                   setoff                                       02
076800120210     c                   when      sc30ambi = 'P'
076900120209     c                   open      fncsb01l
077000120223     c                   when      sc30ambi = 'S'
077100120112     c                   open      tncsb03l
077200120112     c                   open      tncsb09l
077400120223     c                   when      sc30ambi = 'V'
077500120223     c                   open      tncsv01l
077700120307     c                   endsl
077800120209     c*
077900120111     c                   open      tncsm03l
077901120307     c*
078000120210     c                   if        sc30tpci ='V'
078100120113     c                   seton                                        01
078200120113     c                   end
078300120112     C* i parametri obbligatori per poter accedere al pgm sono:
078400120112     c* INSERIMENTO  distinta + opzione
078500120112     c* MODIFICA     spedizione
078600120307     c* VISUALIZZAZ. spedizione
078700120307     c* ANNULLAMENTO OCCULTO spedizione
078800120210     c                   if        sc30TPCi = 'I'
078900120210     c                   if        (sc30fgsi = 0 or sc30ndci = 0 or
079000120210     c                              sc30opzi = ' ' )
079100120210     c                   eval      sc30erro = '1'
079200120210     c                   eval      sc30msgo = 'Errore nei parametri passati'
079300120111     c                   exsr      endpgm
079400120112     c                   end
079500120210     C                   Z-ADD     sc30fgsi      v1Cfgs
079600120210     C                   Z-ADD     sc30ndci      v1Cndc
079601120323     c                   seton                                        03
079700120112     c                   else
079800120307     c* modifica / visualizza / annullamento
079900120210     c                   if         sc30aasi= 0 or
080000120210     c                              sc30lnpi = 0 or
080100120210     c                              sc30nspi = 0
080200120210     c                   eval      sc30erro = '1'
080300120210     c                   eval      sc30msgo = 'Errore nei parametri passati'
080400120111     c                   exsr      endpgm
080500120111     c                   end
080501120307     c*
080600120210     c                   if         sc30aani= 0 or
080700120210     c                              sc30lpni = 0 or
080800120210     c                              sc30nsni = 0
080900120210     c                   eval      sc30aani = sc30aasi
081000120210     c                   eval      sc30lpni = sc30lnpi
081100120210     c                   eval      sc30nrni = sc30nrsi
081200120210     c                   eval      sc30nsni = sc30nspi
081300120112     c                   end
081400120112     c*
081500120209     c                   select
081600120209     c* filiale
081700120210     c                   when      sc30ambi = ' '
081701120307     c* se il legame è blanks vuol dire che ci sono stati
081702120307     c* degli aggiornamenti mozzi ed è presente ancora l'opzione
081800120220     c     ksped         chain     fncss01l
081900120220     c                   if        %found(fncss01l)
082401120307     c* in modifica / visualizzazione errore se non presente legame
082402120307     c* annullamento non importa
082403120307     c                   if        csskey = ' ' and
082404120307     c                             (sc30tpci = 'M' or
082405120307     c                             sc30tpci = 'V')
082500120210     c                   eval      sc30erro = '1'
082600120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
082700120112     c                             presenti dati incompleti'
082800120112     c                   exsr      endpgm
082900120112     c                   end
082902120307     c*
083000120306     c                   eval      comkey  = csskey
083100120112     C                   Z-ADD     cssfgs        v1Cfgs
083200120112     C                   Z-ADD     cssndc        v1Cndc
083201120306     C                   eval      comopz = cssopz
083300120111     c                   else
083400120210     c                   eval      sc30erro = '1'
083500120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
083600120111     c                   exsr      endpgm
083700120111     c                   end
083800120209     c* partenza
083900120210     c                   when      sc30ambi = 'P'
084000120209     c     ksped         chain     fncsb01l
084100120209     c                   if        %found(fncsb01l) and csbnra <> ' ' and
084200120209     c                             csbabi = 0 and csbcab = 0
084300120209     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
084400120307     c* degli aggiornamenti mozzi
084600120209     c     ' '           scan      csbnra                                 99
084700120209     c                   if        *in99
084800120210     c                   eval      sc30erro = '1'
084900120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
085000120209     c                             presenti dati incompleti'
085100120209     c                   exsr      endpgm
085200120209     c                   end
085300120209     c                   eval      comkey = csbnra
085400120209     C                   Z-ADD     0             v1Cfgs
085500120209     C                   Z-ADD     0             v1Cndc
085600120209     c                   else
085700120210     c                   eval      sc30erro = '1'
085800120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
085900120209     c                   exsr      endpgm
086000120209     c                   end
086002120307     c* sede
086100120223     c                   when      sc30ambi = 'S'
086300120112     c     ksped         chain     tncsb03l
086400120112     c                   if        %found(tncsb03l) and csbnra <> ' ' and
086500120112     c                             csbabi = 0 and csbcab = 0
086600120112     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
086700120307     c* degli aggiornamenti mozzi
086900120112     c     ' '           scan      csbnra                                 99
087000120112     c                   if        *in99
087100120210     c                   eval      sc30erro = '1'
087200120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
087300120112     c                             presenti dati incompleti'
087400120112     c                   exsr      endpgm
087500120112     c                   end
087600120112     c                   eval      comkey = csbnra
087700120112     C                   Z-ADD     0             v1Cfgs
087800120112     C                   Z-ADD     0             v1Cndc
087900120112     c                   else
088000120210     c                   eval      sc30erro = '1'
088100120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
088200120112     c                   exsr      endpgm
088300120112     c                   end
088301120307     c* sede variazioni
088400120223     c                   when      sc30ambi = 'V'
088600120223     c     kspedv        chain     tncsv01l
088700120223     c                   if        %found(tncsv01l) and csvnra <> ' ' and
088800120223     c                             csvabi = 0 and csvcab = 0
088900120223     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
089000120307     c* degli aggiornamenti mozzi
089200120223     c     ' '           scan      csvnra                                 99
089300120223     c                   if        *in99
089400120223     c                   eval      sc30erro = '1'
089500120223     c                   eval      sc30msgo = 'Errore nell''annullamento +
089600120223     c                             presenti dati incompleti'
089700120223     c                   exsr      endpgm
089800120223     c                   end
089900120223     c                   eval      comkey = csvnra
090000120223     C                   Z-ADD     0             v1Cfgs
090100120223     C                   Z-ADD     0             v1Cndc
090200120223     c                   else
090300120223     c                   eval      sc30erro = '1'
090400120223     c                   eval      sc30msgo = 'Non trovata la spedizione'
090500120223     c                   exsr      endpgm
090600120223     c                   end
090700120209     c                   endsl
090800120112     c                   end
090900120112     c* in annullamento eseguo in occulto
091000120210     c                   if        sc30tpci = 'A'
091100120112     c                   exsr      srannulla
091200120112     c                   end
091300120112     C*
091400120111     C                   endsr
091500120111     C************************************************************
091600120111     C* FINE PROGRAMMA
091700120111     C************************************************************
091800120111     C     ENDPGM        BEGSR
091801120313     c                   if        sc30ambi = ' '
091803120307     c                   unlock    fncss04l
091804120313     c                   end
091805120307     c                   unlock    tncsm03l
091900120210     c                   movel     tnsc30ds      kpjbu
092000020809     C                   SETON                                            LR
092100020809     C                   RETURN
092200020809     C                   ENDSR
092300020812
