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
005100120306     D comopz          S             10
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
007700120313     c                   eval      sc30keyo = comkey
007800120210     c                   eval      sc30erro = 'L'
007900120109     C                   exsr      endpgm
008000120109     C                   END
008100120109     c* annullamento
008200120209     c   kq              exsr      srannulla
008300120110     C*...Controllo i dati modificati
008400000000?    C                   EXSR      CONTA
008500120110     C*...per ERRORE torno ad emettere il sfl.
008600120110     c                   if        *in50
008700120111     C                   MOVE      NRR2          RECNB2
008800120111     c                   iter
008900120111     c                   end
009000120110     C* aggiornamento                                      *
009100120111     C                   IF        *inkf or *inkg
009200120111?    C                   EXSR      AGGIOR
009300120111     c                   end
009400120110     c                   enddo
009500120110     C*
009600920312     C                   SETON                                        LR
009700120110     c**********************************************************************
009800120110     C*...Inizializzo i sub-file.
009900120110     c**********************************************************************
010000120110     c     srcarsfl      begsr
010100120110     C                   Z-ADD     1             RECNBR
010200120111     C                   Z-ADD     1             RECNB2
010300120110     C                   Z-ADD     0             NRR2              5 0
010400120110     C                   Z-ADD     0             NUMRE2
010500120110     C                   Z-ADD     0             NRR1              5 0
010600120110     C                   Z-ADD     0             NUMREC
010700120110     C                   SETON                                        60
010800120110     C                   SETOFF                                       61
010900120110     C                   SETOFF                                       62
011000120110     C                   WRITE     DCT1
011100120110     C                   WRITE     DCT2
011200120110     C                   SETON                                        61
011300120110     C                   SETON                                        62
011400120111     C                   SETOFF                                       60
011500120110     C*...Carico il sub-file delle spedizioni              *
011600120110     C                   MOVEL     *BLANK        PIENO
011700120110     C                   Z-ADD     0             NRR1
011800120110     C                   Z-ADD     0             NUMREC
011900120111     C                   Z-ADD     0             totcass
012000120222     C                   Z-ADD     0             comdiffe
012100120113     c* filiale
012200120209     c                   select
012300120210     c                   when      sc30ambi = ' '
012400120113     c* inserimento (distinta + opzione)
012500120210     c                   if        sc30tpci = 'I'
012600120220     C     kleg          SETLL     fncss04l
012700120113     c                   else
012800120113     c* no inserimento (legame)
012900120220     C     comkey        SETLL     fncss03l
013000120113     c                   end
013100120110?    C                   do        *hival
013200120210     c                   if        sc30tpci = 'I'
013300120306     C     kleg          READE(n)  fncss04l
013400120220     C                   if        %eof(fncss04l)
013500120220     c                   leave
013600120220     c                   end
013700120312     c* in inserimento solo quelle senza legame
013800120312     C                   if        csskey <> ' '
013900120312     c                   iter
014000120312     c                   end
014100120113     c                   else
014200120306     C     comkey        READE     fncss03l
014300120220     C                   if        %eof(fncss03l)
014400120110     c                   leave
014500120110     c                   end
014600120220     c                   end
014700120110     c                   if        cssatb <> ' '
014800120110     c                   iter
014900120110     c                   end
015000120110     C*
015100120110?    C                   EXSR      CARSB1
015200120110     C                   enddo
015300120209     c* partenza
015400120210     c                   when      sc30ambi ='P'
015500120209     C     ksped         chain     fncsb01l
015600120209     c                   if        %found(fncsb01l)
015700120209?    C                   EXSR      CARSB1P
015800120209     c                   end
015900120518     c* sede
016000120223     c                   when      sc30ambi ='S'
016100120112     C     kcsb          SETLL     tncsb09l
016200120112?    C                   do        *hival
016300120112     C     kcsb          READE     tncsb09l
016400120112     C                   if        %eof(tncsb09l)
016500120112     c                   leave
016600120112     c                   end
016700120112     c* solo il contrassegno con legame
016800120112     c                   if        csbnra <> comkey
016900120112     c                   iter
017000120112     c                   end
017100120112     C*
017200120112?    C                   EXSR      CARSB1S
017300120112     C                   enddo
017400120223     c* storico sede
017500120223     c                   when      sc30ambi ='V'
017600120223     C/EXEC SQL
017700120223     C+ DECLARE TNCSVC CURSOR FOR SELECT * FROM tncsv00f WHERE csvnra =
017800120322     C+ :comkey and CSVDTV = :sc30dtvi
017900120223     C/END-EXEC
018000120223
018100120223     C/EXEC SQL
018200120223     C+ OPEN tncsvc
018300120223     C/END-EXEC
018400120223
018500120223     C                   DOU       sqlcod <> 0
018600120223
018700120223     C/EXEC SQL
018800120223     C+ FETCH NEXT FROM TNCSvC INTO : tncsvds
018900120223     C/END-EXEC
019000120223
019100120223     C                   SELECT
019200120223     C                   WHEN      SQLCod = 100
019300120223     C                   WHEN      SQLCod < 0
019400120223     C                   EVAL      *INH1 = *ON
019500120223     C                   OTHER
019600120223     c* stampa testata e apre membro
019700120223     c                   exsr      carsb1v
019800120223     C                   ENDSL
019900120223
020000120223     C                   ENDDO
020100120223     C/EXEC SQL
020200120223     C+ CLOSE tncsvc
020300120223     C/END-EXEC
020400120209     c                   endsl
020500120110     C*...Carico il sub-file degli assegni                 *
020600120110     C                   MOVEL     *BLANK        PIEN1
020700120110     C                   Z-ADD     0             NRR2
020800120110     C                   Z-ADD     0             NUMRE2
020900120111     C     comkey        SETLL     tncsm03l
021000120110?    C                   do        *hival
021100120111     C     comkey        READE     tncsm03l
021200120110     C                   if        %eof(tncsm03l)
021300120110     c                   leave
021400120110     c                   end
021500120110     c                   if        csmatb <> ' '
021600120110     c                   iter
021700120110     c                   end
021800120110     c*
021900120110?    C                   EXSR      CARSB2
022000120110     C                   ENDdo
022100120110     c* carico 21 righe vuote nel sfl degli assegni
022200120210     c                   if        sc30tpci <> 'V'
022300120110     c                   do        21
022400120110     c                   clear                   tncsmds
022500120110?    C                   EXSR      CARSB2
022600120110     c                   enddo
022700120111     c                   end
022800120110     c                   endsr
022900000000     C*---------------------------------------------------------------*---
023000120110     C*  CARSB1:   CARICA SUBFILE SPEDIZIONI                          *
023100000000     C*---------------------------------------------------------------*
023200000000     C     CARSB1        BEGSR
023300000000     C*
023400120109     C*...Carico riga spedizione
023500120110     c                   eval      v1caas = cssaas
023600120111     c                   eval      v1clnp = csslnp
023700120110     c                   eval      v1cnrs = cssnrs
023800120110     c                   eval      v1cnsp = cssnsp
023900120518     c                   if        cssnso <> cssnsp
024000120110     c                   eval      v1caao = cssaao
024100120110     c                   eval      v1clpo = csslno
024200120110     c                   eval      v1cnro = cssnro
024300120110     c                   eval      v1cnso = CSSNSo
024400120518     c                   else
024500120518     c                   clear                   v1caao
024600120518     c                   clear                   v1clpo
024700120518     c                   clear                   v1cnro
024800120518     c                   clear                   v1cnso
024900120518     c                   end
025000120110     c                   eval      v1cvca = CSSvca
025100120518     c                   eval      v1cmit = CSSmit
025200120110     C                   Z-ADD     csscas        v1ccas
025300120111     c                   add       csscas        totcass
025400120110     c* mi servirà nel sfl degli assegni
025500120110     c                   eval      v1ctic = csstic
025600000000     C                   ADD       1             NRR1
025700000000     C                   ADD       1             NUMREC            4 0
025800000000     C                   WRITE     DSF1
025900000000     C                   MOVEL     '1'           PIENO             1
026000000000     C*                                                    *
026100000000     C                   ENDSR
026200120112     C*---------------------------------------------------------------*---
026300120112     C*  CARSB1S   CARICA SUBFILE SPEDIZIONI da sede                  *
026400120112     C*---------------------------------------------------------------*
026500120112     C     CARSB1S       BEGSR
026600120112     C*
026700120518     c                   clear                   v1cmit
026800120518     c                   clear                   v1caao
026900120518     c                   clear                   v1clpo
027000120518     c                   clear                   v1cnro
027100120518     c                   clear                   v1cnso
027200120112     C*...Carico riga spedizione
027300120112     c                   eval      v1caas = csbaas
027400120112     c                   eval      v1clnp = csblnp
027500120112     c                   eval      v1cnrs = csbnrs
027600120112     c                   eval      v1cnsp = csbnsp
027700120112     c                   eval      v1cvca = CSbvca
027800120117     C                   Z-ADD     csbcas        v1ccas
027900120117     c                   add       csbcas        totcass
028000120112     c* mi servirà nel sfl degli assegni
028100120112     c                   movel     csbtpa        v1ctic
028200120112     c                   move      csbtpi        v1ctic
028300120112     C                   ADD       1             NRR1
028400120112     C                   ADD       1             NUMREC            4 0
028500120112     C                   WRITE     DSF1
028600120112     C                   MOVEL     '1'           PIENO             1
028700120112     C*                                                    *
028800120112     C                   ENDSR
028900120223     C*---------------------------------------------------------------*---
029000120223     C*  CARSB1v   CARICA SUBFILE SPEDIZIONI da variazioni di sede    *
029100120223     C*---------------------------------------------------------------*
029200120223     C     CARSB1v       BEGSR
029300120223     C*
029400120518     c                   clear                   v1cmit
029500120518     c                   clear                   v1caao
029600120518     c                   clear                   v1clpo
029700120518     c                   clear                   v1cnro
029800120518     c                   clear                   v1cnso
029900120223     C*...Carico riga spedizione
030000120223     c                   eval      v1caas = csvaas
030100120223     c                   eval      v1clnp = csvlnp
030200120223     c                   eval      v1cnrs = csvnrs
030300120223     c                   eval      v1cnsp = csvnsp
030400120223     c                   eval      v1cvca = CSvvca
030500120223     C                   Z-ADD     csvcas        v1ccas
030600120223     c                   add       csvcas        totcass
030700120223     c* mi servirà nel sfl degli assegni
030800120223     c                   movel     csvtpa        v1ctic
030900120223     c                   move      csvtpi        v1ctic
031000120223     C                   ADD       1             NRR1
031100120223     C                   ADD       1             NUMREC            4 0
031200120223     C                   WRITE     DSF1
031300120223     C                   MOVEL     '1'           PIENO             1
031400120223     C*                                                    *
031500120223     C                   ENDSR
031600120209     C*---------------------------------------------------------------*---
031700120209     C*  CARSB1S   CARICA SUBFILE SPEDIZIONI da filiale partenza      *
031800120209     C*---------------------------------------------------------------*
031900120209     C     CARSB1p       BEGSR
032000120209     C*
032100120209     C/EXEC SQL
032200120209     C+ Declare A1 Cursor for
032300120209     C+ SELECT *
032400120209     C+ from fncsb00f
032500120209     C+ where csbnra = : comkey
032600120209     C/END-EXEC
032700120209      *          apertura cursore
032800120209     C/EXEC SQL
032900120209     C+ OPEN A1
033000120209     C/END-EXEC
033100120209     C                   DO        *hival
033200120209      *          lettura cursore
033300120209     C/EXEC SQL
033400120209     C+ Fetch Next From A1 Into
033500120209     C+ :fncsbds
033600120209     C/END-EXEC
033700120209     C                   SELECT
033800120209     C                   WHEN      SqlCod = 100
033900120209     C                   LEAVE
034000120209     C                   WHEN      SqlCod >= 0
034100120209     C*...Carico riga spedizione
034200120209     c                   exsr      CARSB1S
034300120209     C                   OTHER
034400120209     C                   ENDSL
034500120209      *
034600120209     C                   ENDDO
034700120209     C/EXEC SQL
034800120209     C+ Close A1
034900120209     C/END-EXEC
035000120209     C*                                                    *
035100120209     C                   ENDSR
035200000000     C*---------------------------------------------------------------*---
035300000000     C*  CARSB2:   CARICA SUBFILE DELLA CONTABILITA'                  *
035400000000     C*---------------------------------------------------------------*
035500000000     C     CARSB2        BEGSR
035600000000     C*
035700120110     C                   Z-ADD     csmidc        v1cidc
035800120110     c                   movel     csmdiv        v1cdiv
035900120113     c                   if        csmdiv = ' '
036000120113     c                   eval      v1cdiv = 'EUR'
036100120113     c                   end
036200120111     c                   movel     csmnra        v1cnra
036300120110     c                   z-add     csmabi        v1cabi
036400120110     c                   z-add     csmcab        v1ccab
036500120111     c                   if        csmdte = 0
036600120111     c                   clear                   v1cdte
036700120111     c                   else
036800120111     C                   move      csmdte        dtaiso
036900120111     C                   move      dtaiso        dtaeur
037000120111     C                   move      dtaeur        v1cdte
037100120111     c                   end
037200990830
037300000000     C                   ADD       1             NRR2
037400000000     C                   ADD       1             NUMRE2            4 0
037500000000     C                   WRITE     DSF2
037600000000     C                   MOVEL     '1'           PIEN1             1
037700000000     C*
037800000000     C                   ENDSR
037900120111      *---------------------------------------------------------------*
038000120111      *  Loop di errori  subfile errori                               *
038100120111      *---------------------------------------------------------------*
038200120111     C     Conta         BEGSR
038300120111     c                   clear                   xx
038400120111     c                   clear                   sass
038500120111     c                   clear                   totass
038600120222     c                   clear                   comdiffe
038700120111     c                   setoff                                       313940
038800120111     c                   setoff                                       414243
038900120111     c                   setoff                                       448986
039000120117     c                   setoff                                       858150
039100120222     c                   setoff                                       32
039200120111     C                   Z-ADD     0             NRR6              4 0
039300120111      *
039400120111     c                   do        *hival
039500120111     C                   ADD       1             NRR6
039600120111     C     NRR6          CHAIN     dsf2                               95
039700120111     c                   if        *in95
039800120111     c                   leave
039900120111     c                   end
040000120111     c*
040100120111     c                   if        v1cidc = 0 and
040200120117     c                             v1cnra = ' ' and
040300120117     c                             v1cabi = 0 and
040400120111     c                             v1ccab = 0 and
040500120111     c                             v1cdte = 0
040600120117     C                   UPDATE    dsf2
040700120111     c                   iter
040800120111     c                   end
040900120111     C*  Controllo se richiesta ricerca tipo incasso
041000120111     C     '?'           SCAN      V1CTIC                                 95
041100120111     C     *IN95         IFEQ      '1'
041200120111     C                   MOVEL     1             §KUT
041300120111     C                   MOVEL     '1A'          §COD
041400120111     C                   MOVE      *BLANKS       V1CTIC
041500120111     C                   CALL      'X§TABER'
041600120111     C                   PARM                    §KUT
041700120111     C                   PARM                    §COD
041800120111     C                   PARM                    §KEY
041900120111     C                   PARM                    §DES             30
042000120111     C                   MOVEL     §KEY          V1CTIC
042100120111     C                   END
042200120111     C*  Controllo se corretto tipo incasso
042300120111     c                   if        v1ctic = ' '
042400120111     C                   SETON                                        4050
042500120111     C                   UPDATE    dsf2
042600120111     C                   leavesr
042700120111     c                   else
042800120111     C                   Z-ADD     1             X
042900120111     C     V1CTIC        LOOKUP    C1A(X)                                 95
043000120111     C     *IN95         IFEQ      '0'
043100120111     C                   SETON                                        4050
043200120111     C                   UPDATE    dsf2
043300120111     C                   leavesr
043400120111     c                   else
043500120111     c                   movel     d1a(x)        ds1a
043600120111     C                   end
043700120111     C                   end
043800120111     C*  Controllo se cliente abilitato
043900120117     c                   if        §1ATTM ='S'
044000120111     C                   CLEAR                   DSTM
044100120111     C                   MOVEL     'TM'          tblcod
044200120117     c* filiale
044300120210     c                   if        sc30ambi = ' '
044400120111     C                   MOVEL     cssccm        tblkey
044500120117     c                   else
044600120117     C                   MOVEL     csbcdi        tblkey
044700120117     c                   end
044800120117     C     KTAB          CHAIN     TABEL00F
044900120111     c                   if        %found(tabel00f) and tblflg = ' '
045000120111     C                   MOVEL     tbluni        DSTM
045100120111     c                   else
045200120117     C                   SETON                                        8150
045300120111     C                   UPDATE    dsf2
045400120111     C                   leavesr
045500120111     c                   end
045600120111     C     V1CTIC        IFEQ      'TM'
045700120111     C     §tmftm        ANDNE     'S'
045800120111     C     V1CTIC        OREQ      'TO'
045900120111     C     §TMFTO        ANDNE     'S'
046000120111     C     V1CTIC        OREQ      'CA'
046100120111     C     §TMFCA        ANDNE     'S'
046200120111     C     V1CTIC        OREQ      'BP'
046300120111     C     §TMFbp        ANDNE     'S'
046400120117     C                   SETON                                        8150
046500120111     C                   UPDATE    dsf2
046600120111     C                   leavesr
046700120111     C                   END
046800120117     C                   END
046900120111     c* importo
047000120111     c                   if        v1cidc = 0
047100120111     C                   SETON                                        3950
047200120111     C                   UPDATE    dsf2
047300120111     C                   leavesr
047400120111     c                   else
047500120111     c                   add       v1cidc        totass
047600120111     c                   end
047700120111     c* numero assegno
047800120111     C     V1cnra        IFEQ      *BLANKS
047900120111     C     V1cnra        OREQ      *ZEROS
048000120111     C                   SETON                                        4150
048100120111     C                   UPDATE    dsf2
048200120111     C                   leavesr
048300120111     C                   END
048400120111     c* se tipo incasso non ammette assegni postali controllo abi  07601
048500120111     c     §1anap        ifeq      'S'
048600120111     c     v1cabi        andeq     07601
048700120111     C                   SETON                                        4250
048800120111     C                   UPDATE    dsf2
048900120111     C                   leavesr
049000120111     c                   endif
049100120111     C*  ABI e CAB
049200120111     C     V1cABi        IFle      *ZEROS
049300120111     C     V1cCAb        ORle      *ZEROS
049400120111     C                   SETON                                        4450
049500120111     C                   UPDATE    dsf2
049600120111     C                   leavesr
049700120111     C                   End
049800120111     C*  Se immesso Abi-CAb verifico congruenza
049900120111     C     KABI          CHAIN     CNABI01L
050000120111     c                   if        not %found(cnabi01l)
050100120111     C                   SETON                                        4450
050200120111     C                   UPDATE    dsf2
050300120111     C                   leavesr
050400120111     C                   END
050500120111     C* Se non ho indicato la data
050600120111     C* imposto in automatico quella del giorno
050700120111     C     V1cdte        IFEQ      *ZEROS
050800120111     C                   MOVEL     WOGGI         V1cDTe
050900120111     C                   End
051000120111     C*  Giro la data assegno
051100120111     C                   MOVEL     V1cDTe        G02DAT
051200120111     C                   Z-ADD     0             G02INV
051300120111     C                   MOVEL     *BLANKS       G02ERR
051400120111     C                   CALL      'XSRDA8'
051500120111     C                   PARM                    WLBDA8
051600120111     C                   MOVEL     g02dat        v1cdte
051700120111     C     G02ERR        IFEQ      '1'
051800120619     C**                 SETON                                        4328
051900120619     C                   SETON                                        4350
052000120111     C                   UPDATE    dsf2
052100120111     C                   leavesr
052200120111     C                   END
052300120112     C* Controllo data assegno > OGGI se TM o TO e viceversa  ?????
052400120111     C     g02inv        ifle      OGGI
052500120111     c                   if        v1ctic = 'TM' or
052600120111     c                             v1ctic = 'TO'
052700120111     C                   SETON                                        8550
052800120111     C                   UPDATE    dsf2
052900120111     C                   leavesr
053000120111     C                   END
053100120111     c                   else
053200120111     c                   if        v1ctic <>'TM' and
053300120111     c                             v1ctic <>'TO'
053400120111     C                   SETON                                        8650
053500120111     C                   UPDATE    dsf2
053600120111     C                   leavesr
053700120111     C                   END
053800120111     C                   END
053900120111     c* controllo che non ci sia lo stesso assegno
054000120111     c                   movel     g02inv        comdte            8
054100120111     c                   movel     V1cABi        comabi            5
054200120111     c                   movel     V1cCAb        comcab            5
054300120111     c                   eval      comass =  comdte+comabi+comcab+v1cnra
054400120111     c     comass        lookup    sass                                   89
054500120111     c                   if        *in89
054600120111     C                   SETON                                        50
054700120111     c                   leavesr
054800120111     c                   else
054900120111     c                   add       1             xx
055000120111     c                   movel     comass        sass(xx)
055100120111     c                   end
055200120111     c*
055300120111     C                   UPDATE    dsf2
055400120111     c*
055500120111     C                   ENDdo
055600120222     c* se la differenza tra assegni e c/assegni è maggiore dei centesimi
055700120222     c* errore
055800120222     c                   eval      comdiffe = totass - totcass
055900120222     c                   if        comdiffe < 0
056000120222     c                   eval      comdiffe = comdiffe * -1
056100120222     c                   end
056200120222     c                   if        comdiffe >=1
056300120222     C                   SETON                                        5032
056400120222     c                   z-add     1             nrr2
056500120222     c                   else
056600120222     c*  se il totale assegni <> da c/ass. chiedo forzatura
056700120111     c                   if        totass <> totcass   and
056800120111     c                             totass <> 0  and not *inkg
056900120111     C                   SETON                                        5031
057000120117     c                   z-add     1             nrr2
057100120111     c                   else
057200120111     c                   setoff                                       31
057300120111     c                   end
057400120222     c                   end
057500120111     c*
057600120111     C                   ENDSR
057700120111      *---------------------------------------------------------------*
057800120111      *  Loop di ANNULLAMENTO                                         *
057900120111      *---------------------------------------------------------------*
058000120111     C     srannulla     BEGSR
058100120112     C* CANCELLO QUELLO SCRITTO PRIMA
058200120307     c                   if        comkey <> ' '
058300120111     C/EXEC SQL
058400120308     C+ DELETE FROM TNCSM03L WHERE CSmkey = :comkey
058500120307     C+ WITH UR
058600120111     C/END-EXEC
058700120307     c                   end
058800120112     c*
058900120210     c                   if        *inkq or sc30TPCI = 'A'
059000120209     c* per il momento l'annullamento può essere fatto solo in filiale
059100120307     c                   if        sc30ambi = ' '
059200120306     c                   if        comkey <> ' '
059300120112     C/EXEC SQL
059400120307     C+ delete from FNCSS04l
059500120209     C+  WHERE CSSkey = :comkey
059600120307     C+ WITH UR
059700120112     C/END-EXEC
059800120306     c                   end
059900120306     c                   if        comkey =  ' '  and comopz <> ' '
060000120308     c                             and v1cfgs <> 0 and v1cndc <> 0
060100120306     C/EXEC SQL
060200120307     C+ delete from FNCSS04l
060300120306     C+  WHERE CSSopz = :comopz and cssfgs = :v1cfgs and cssndc = :v1cndc
060400120312     C+  AND CSSKEY = ' '
060500120307     C+ WITH UR
060600120306     C/END-EXEC
060700120306     c                   end
060800120209     c                   end
060900120210     c                   eval      sc30erro = 'Q'
061000120111     c                   exsr      endpgm
061100120111     c                   end
061200120111     C*
061300120111     c                   endsr
061400120111      *---------------------------------------------------------------*
061500120111      *  Loop di aggiornamento                                        *
061600120111      *---------------------------------------------------------------*
061700120111     C     aggior        BEGSR
061800120112     c* cancello gli assegni precedentemente inseriti per poi riscriverli
061900120210     c                   if        sc30tpci = 'M'
062000120112     c                   exsr      srannulla
062100120112     c                   end
062200120112     c* prelevo il numeratore per impostare il legame solo in inserimento
062300120113     c                   clear                   o33nrf
062400120210     c                   if        sc30tpci = 'I'
062500120111     c                   do        *hival
062600120111     C                   CLEAR                   TRUL33DS
062700120111     c                   eval      i33tla = 'L'
062800120111     C                   Z-ADD     2             I33CNU
062900120111     C                   Z-ADD     0             I33PO1
063000120111     C                   Z-ADD     1             I33NUM
063100120111     c                   movel(p)  trul33ds      kpjbu
063200120111     c                   call      'TRUL33R'
063300120111     c                   parm                    kpjba
063400120111     c                   movel     kpjbu         trul33ds
063500120111     C                   IF        O33ERR=0
063600120111     c                   leave
063700120111     C                   END
063800120111     C                   ENDdo
063900120112     c* aggiorno il legame sulle spedizioni solo in inserimento
064000120220     c     kleg          setll     fncss04l
064100120112     c                   do        *hival
064200120220     c     kleg          reade     fncss04l
064300120220     c                   if        %eof(fncss04l)
064400120112     c                   leave
064500120112     c                   end
064600120313     c* in inserimento solo quelle senza legame
064700120313     C                   if        csskey <> ' '
064800120313     c                   iter
064900120313     c                   end
065000120220     c                   move      O33Nrf        csskey
065100120220     c                   update    fncss000
065200120112     c                   enddo
065300120112     c                   else
065400120112     c* imposto il numeratore con il legame inserito precedentemente
065500120113     C                   move      comkey        O33Nrf
065600120112     C                   END
065700120112     C* scrivo gli assegni
065800120111     C                   Z-ADD     0             NRR6              4 0
065900120111     c                   do        *hival
066000120111     C                   ADD       1             NRR6
066100120111     C     NRR6          CHAIN     dsf2                               95
066200120111     c                   if        *in95
066300120111     c                   leave
066400120111     c                   end
066500120111     c                   if        v1cidc =0
066600120111     c                   iter
066700120111     c                   end
066800120111     C                   move      O33Nrf        csmkey
066900120111     C                   eval      csmidc =      v1cidc
067000120111     C                   eval      csmdiv =      v1cdiv
067100120111     C                   eval      csmnra =      v1cnra
067200120111     C                   eval      csmabi =      v1cabi
067300120111     C                   eval      csmcab =      v1ccab
067400120111     C                   MOVE      v1cdte        dtaeur
067500120111     C                   MOVE      dtaeur        dtaiso
067600120111     C                   MOVE      dtaiso        csmdte
067700120314     C     V1CTIC        LOOKUP    C1A(X)                                 95
067800120314     C     *IN95         IFEQ      '1'
067900120314     c                   movel     d1a(x)        ds1a
068000120314     C                   end
068100120314     C                   MOVE      §1AFCA        csmTPA
068200120314     c                   move      'M'           csmtpi
068300120111     C                   eval      CSmpru =      knmus
068400120111     C                   eval      csmdatora = WHHDAT
068500120111     c                   clear                   csmatb
068600120111     c                   if        *inkg
068700120111     C                   eval      csmfla =  'S'
068800120111     c                   else
068900120111     c                   clear                   csmfla
069000120111     c                   end
069100120111     c                   write     tncsm000
069200120111     c                   enddo
069300120111     c*
069400120210     c                   if        sc30cmti = 'S'
069500120111     c                   commit
069600120111     c                   end
069700120112     c*
069800120302     c                   eval      sc30keyo = csmkey
069900120111     C                   EXSR      endpgm
070000120111     C                   ENDSR
070100120109     c**********************************************************************
070200990826     C     *inzsr        begsr
070300120109     c**********************************************************************
070400990826     C     *ENTRY        PLIST
070500990826     C                   PARM                    KPJBA
070600120210     c                   movel     kpjbu         tnsc30ds
070700120302     c                   clear                   sc30keyo
070800120307     c                   clear                   sc30msgo
070900120307     c                   clear                   sc30erro
071000120307     c                   eval      comopz = sc30opzi
071100120111     C     *LIKE         DEFINE    TBLKUT        §KUT
071200120111     C     *LIKE         DEFINE    TBLCOD        §COD
071300120111     C     *LIKE         DEFINE    TBLkey        §key
071400120109     C* Giro data del giorno
071500120109     C                   TIME                    WHHDAT           14 0
071600120109     C                   MOVEL     WHHDAT        TIMES             6 0
071700120109     C                   MOVE      WHHDAT        G02DAT
071800120109     C                   MOVEL     *BLANK        G02ERR
071900120109     C                   CALL      'XSRDA8'
072000120109     C                   PARM                    WLBDA8
072100120109     C                   Z-ADD     G02INV        OGGI              8 0
072200120109     C                   Z-ADD     G02DAT        WOGGI             8 0
072300120111     c* verifico i parametri
072400120111     c                   exsr      srparam
072500120109     C* Decodifica Elaboratore
072600120109     c     *dtaara       define    §azute        azuteds
072700120109     c     *dtaara       define    §datiute      ddatiute
072800120109     C                   in(E)     *dtaara
072900120109     C                   IF        %Error  or  RSUT = *blanks
073000120109     C                   call      'TIBS34R'
073100120109     C                   parm                    Tibs34Ds
073200120109     C                   in        *dtaara
073300120109     c                   ENDIF
073400120111     C*-
073500120111     C     kleg          klist
073600120210     C                   kfld                    sc30fgsi
073700120210     C                   kfld                    sc30ndci
073800120307     C                   kfld                    comopz
073900120109     c     ksped         klist
074000120210     c                   kfld                    sc30aani
074100120210     c                   kfld                    sc30lpni
074200120210     c                   kfld                    sc30nrni
074300120210     c                   kfld                    sc30nsni
074400120223     c     kspedv        klist
074500120223     c                   kfld                    sc30aani
074600120223     c                   kfld                    sc30lpni
074700120223     c                   kfld                    sc30nrni
074800120223     c                   kfld                    sc30nsni
074900120223     c                   kfld                    sc30dtvi
075000120223     c                   kfld                    sc30orvi
075100120112     c     kcsb          klist
075200120112     c                   kfld                    csbrgf
075300120112     c                   kfld                    csbndt
075400120112     c                   kfld                    csbddc
075500120109     c     ktab          klist
075600120109     c                   kfld                    TBLKUT
075700120109     c                   kfld                    TBLCOD
075800120109     c                   kfld                    TBLKEY
075900120109     c     ktab1         klist
076000120109     c                   kfld                    TBLKUT
076100120109     c                   kfld                    TBLCOD
076200120109     c     kabi          klist
076300120109     c                   kfld                    V1cABi
076400120109     c                   kfld                    V1cCAb
076500120111     c                   eval      tblkut = 1
076600120109     c*
076700120109     C                   CLEAR                   DS1A
076800120109     C                   Z-ADD     0             X                 3 0
076900120109     C                   MOVEL     '1A'          tblcod
077000120109     C     KTAB1         setll     TABEL00F
077100120109     C                   DO        *hival
077200120109     C     KTAB1         READE     TABEL00F
077300120109     c                   if        %eof(tabel00f)
077400120109     c                   leave
077500120109     C                   END
077600120109     C     X             ifeq      200
077700120109     c                   leave
077800120109     C                   END
077900120109     C     TBLFLG        IFne      ' '
078000120109     c                   iter
078100120109     C                   END
078200120109     C                   MOVEL     TBLUNI        DS1A
078300120109     c* solo mittente
078400120109     C                   IF        §1AFMB <> 'M'
078500120109     c                   iter
078600120109     C                   END
078700120109     c* solo assegno
078800120109     C                   IF        §1ANAS <> ' '
078900120109     c                   iter
079000120109     C                   END
079100120109     c*
079200120109     C                   ADD       1             X
079300120109     C                   MOVEL     TBLKEY        C1A(X)
079400120109     C                   MOVEL     TBLUNI        D1A(X)
079500120109     C                   ENDdo
079600990826     C*
079700990826     C                   endsr
079800020809     C************************************************************
079900120111     C* parametri
080000020809     C************************************************************
080100120111     C     srparam       BEGSR
080200120111     C*
080300120307     c                   if        sc30cmti = 'S' or
080400120307     c                             sc30cmti ='F'
080500120111     c                   eval      cmt = *on
080600120111     c                   else
080700120111     c                   eval      cmt = *off
080800120111     c                   end
080900120307     c* apro i file a seconda dell'ambiente in cui sono
081000120323     c                   seton                                        02
081100120209     c                   select
081200120210     c                   when      sc30ambi = ' '
081300120220     c                   open      fncss01l
081400120220     c                   open      fncss03l
081500120220     c                   open      fncss04l
081600120323     c                   setoff                                       02
081700120210     c                   when      sc30ambi = 'P'
081800120209     c                   open      fncsb01l
081900120223     c                   when      sc30ambi = 'S'
082000120112     c                   open      tncsb03l
082100120112     c                   open      tncsb09l
082200120223     c                   when      sc30ambi = 'V'
082300120223     c                   open      tncsv01l
082400120307     c                   endsl
082500120209     c*
082600120111     c                   open      tncsm03l
082700120307     c*
082800120210     c                   if        sc30tpci ='V'
082900120113     c                   seton                                        01
083000120113     c                   end
083100120112     C* i parametri obbligatori per poter accedere al pgm sono:
083200120112     c* INSERIMENTO  distinta + opzione
083300120112     c* MODIFICA     spedizione
083400120307     c* VISUALIZZAZ. spedizione
083500120307     c* ANNULLAMENTO OCCULTO spedizione
083600120210     c                   if        sc30TPCi = 'I'
083700120210     c                   if        (sc30fgsi = 0 or sc30ndci = 0 or
083800120210     c                              sc30opzi = ' ' )
083900120210     c                   eval      sc30erro = '1'
084000120210     c                   eval      sc30msgo = 'Errore nei parametri passati'
084100120111     c                   exsr      endpgm
084200120112     c                   end
084300120210     C                   Z-ADD     sc30fgsi      v1Cfgs
084400120210     C                   Z-ADD     sc30ndci      v1Cndc
084500120323     c                   seton                                        03
084600120112     c                   else
084700120307     c* modifica / visualizza / annullamento
084800120210     c                   if         sc30aasi= 0 or
084900120210     c                              sc30lnpi = 0 or
085000120210     c                              sc30nspi = 0
085100120210     c                   eval      sc30erro = '1'
085200120210     c                   eval      sc30msgo = 'Errore nei parametri passati'
085300120111     c                   exsr      endpgm
085400120111     c                   end
085500120307     c*
085600120210     c                   if         sc30aani= 0 or
085700120210     c                              sc30lpni = 0 or
085800120210     c                              sc30nsni = 0
085900120210     c                   eval      sc30aani = sc30aasi
086000120210     c                   eval      sc30lpni = sc30lnpi
086100120210     c                   eval      sc30nrni = sc30nrsi
086200120210     c                   eval      sc30nsni = sc30nspi
086300120112     c                   end
086400120112     c*
086500120209     c                   select
086600120209     c* filiale
086700120210     c                   when      sc30ambi = ' '
086800120307     c* se il legame è blanks vuol dire che ci sono stati
086900120307     c* degli aggiornamenti mozzi ed è presente ancora l'opzione
087000120220     c     ksped         chain     fncss01l
087100120220     c                   if        %found(fncss01l)
087200120307     c* in modifica / visualizzazione errore se non presente legame
087300120307     c* annullamento non importa
087400120307     c                   if        csskey = ' ' and
087500120307     c                             (sc30tpci = 'M' or
087600120307     c                             sc30tpci = 'V')
087700120210     c                   eval      sc30erro = '1'
087800120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
087900120112     c                             presenti dati incompleti'
088000120112     c                   exsr      endpgm
088100120112     c                   end
088200120307     c*
088300120306     c                   eval      comkey  = csskey
088400120112     C                   Z-ADD     cssfgs        v1Cfgs
088500120112     C                   Z-ADD     cssndc        v1Cndc
088600120306     C                   eval      comopz = cssopz
088700120111     c                   else
088800120210     c                   eval      sc30erro = '1'
088900120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
089000120111     c                   exsr      endpgm
089100120111     c                   end
089200120209     c* partenza
089300120210     c                   when      sc30ambi = 'P'
089400120209     c     ksped         chain     fncsb01l
089500120209     c                   if        %found(fncsb01l) and csbnra <> ' ' and
089600120209     c                             csbabi = 0 and csbcab = 0
089700120209     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
089800120307     c* degli aggiornamenti mozzi
089900120209     c     ' '           scan      csbnra                                 99
090000120209     c                   if        *in99
090100120210     c                   eval      sc30erro = '1'
090200120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
090300120209     c                             presenti dati incompleti'
090400120209     c                   exsr      endpgm
090500120209     c                   end
090600120209     c                   eval      comkey = csbnra
090700120209     C                   Z-ADD     0             v1Cfgs
090800120209     C                   Z-ADD     0             v1Cndc
090900120209     c                   else
091000120210     c                   eval      sc30erro = '1'
091100120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
091200120209     c                   exsr      endpgm
091300120209     c                   end
091400120307     c* sede
091500120223     c                   when      sc30ambi = 'S'
091600120112     c     ksped         chain     tncsb03l
091700120112     c                   if        %found(tncsb03l) and csbnra <> ' ' and
091800120112     c                             csbabi = 0 and csbcab = 0
091900120112     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
092000120307     c* degli aggiornamenti mozzi
092100120112     c     ' '           scan      csbnra                                 99
092200120112     c                   if        *in99
092300120210     c                   eval      sc30erro = '1'
092400120210     c                   eval      sc30msgo = 'Errore nell''annullamento +
092500120112     c                             presenti dati incompleti'
092600120112     c                   exsr      endpgm
092700120112     c                   end
092800120112     c                   eval      comkey = csbnra
092900120112     C                   Z-ADD     0             v1Cfgs
093000120112     C                   Z-ADD     0             v1Cndc
093100120112     c                   else
093200120210     c                   eval      sc30erro = '1'
093300120210     c                   eval      sc30msgo = 'Non trovata la spedizione'
093400120112     c                   exsr      endpgm
093500120112     c                   end
093600120307     c* sede variazioni
093700120223     c                   when      sc30ambi = 'V'
093800120223     c     kspedv        chain     tncsv01l
093900120223     c                   if        %found(tncsv01l) and csvnra <> ' ' and
094000120223     c                             csvabi = 0 and csvcab = 0
094100120223     c* se nel legame sono presenti dei blnaks vuol dire che ci sono stati
094200120307     c* degli aggiornamenti mozzi
094300120223     c     ' '           scan      csvnra                                 99
094400120223     c                   if        *in99
094500120223     c                   eval      sc30erro = '1'
094600120223     c                   eval      sc30msgo = 'Errore nell''annullamento +
094700120223     c                             presenti dati incompleti'
094800120223     c                   exsr      endpgm
094900120223     c                   end
095000120223     c                   eval      comkey = csvnra
095100120223     C                   Z-ADD     0             v1Cfgs
095200120223     C                   Z-ADD     0             v1Cndc
095300120223     c                   else
095400120223     c                   eval      sc30erro = '1'
095500120223     c                   eval      sc30msgo = 'Non trovata la spedizione'
095600120223     c                   exsr      endpgm
095700120223     c                   end
095800120209     c                   endsl
095900120112     c                   end
096000120112     c* in annullamento eseguo in occulto
096100120210     c                   if        sc30tpci = 'A'
096200120112     c                   exsr      srannulla
096300120112     c                   end
096400120112     C*
096500120111     C                   endsr
096600120111     C************************************************************
096700120111     C* FINE PROGRAMMA
096800120111     C************************************************************
096900120111     C     ENDPGM        BEGSR
097000120313     c                   if        sc30ambi = ' '
097100120307     c                   unlock    fncss04l
097200120313     c                   end
097300120307     c                   unlock    tncsm03l
097400120210     c                   movel     tnsc30ds      kpjbu
097500020809     C                   SETON                                            LR
097600020809     C                   RETURN
097700020809     C                   ENDSR
097800020812
