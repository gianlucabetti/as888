000100040415     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000     H*------------------------------------------------------------------------*
000300040309     H* STATISTICA STAGIONALITA' GENERA WRK FILE
000400000000     H*------------------------------------------------------------------------*
000500040311      *----
000600040312     FCNCLP00F  IF   E           K DISK    rename(cnclp000: CLrec) prefix(Read_)
000700040311      *----
000800040419     FTITAS31C  IF   E           K DISK
000900040311      *----
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FTABEL00F  IF   E           K DISK
001200040311      *----
001300040309     FWFSTGC0F  O  A E             DISK
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001900000000     D*-------------------
002000000000     D FIL             S              3  0 DIM(500)                             *FILIALE
002100000000     D ARE             S              3  0 DIM(500)                             *AREA
002200000000     D DIV             S              1    DIM(500)                             *DIVISIONE
002300000000     D DIT             S              3    DIM(500)                             *RAMO AZIENDALE
002400040309     D*-------------------
002500040309     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
002600040309     D*-------------------
002700040309     D RBL             S              1    DIM(5)                               RAGGRUPPAMENTO BOLLA
002800040309     D FSP             S              1    DIM(5)                               FLAG SPEDIZIONE
002900040309     D FRI             S              1    DIM(5)                               FLAG RICAVO
003000040309     D*-------------------
003100040309     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003200040309     D*-------------------
003300040309     D TB              S              2    DIM(20)                              TIP0 BOLLA
003400040309     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
003500040309     D RI              S              1    DIM(20)                              FLAG RICAVO
003600040309     D*-------------------
003700040309     D* DS TIPO BOLLA
003800040309     D*-------------------
003900040309     D DSTB          E DS
004000040309     D*-------------------
004100040309     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
004200040309     D*-------------------
004300040309     D DS2W          E DS
004400040312     D*------------------
004500040312     D* DS REPERIMENTO DATI UTENTE
004600040312     D*-------------------
004700040312     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
004800040312     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
004900040312     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005000040312     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005100040312     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005200000000     D*------------------------------------------------------------------------*
005300040311     D* OUTPUT CAMPI da totalizzare
005400000000     D*------------------------------------------------------------------------*
005500040318     D Stg_NSP_DS      DS
005600040318     D  Stgs01                 1      8  0
005700040318     D  Stgs02                 9     16  0
005800040318     D  Stgs03                17     24  0
005900040318     D  Stgs04                25     32  0
006000040318     D  Stgs05                33     40  0
006100040318     D  Stgs06                41     48  0
006200040318     D  Stgs07                49     56  0
006300040318     D  Stgs08                57     64  0
006400040318     D  Stgs09                65     72  0
006500040318     D  Stgs10                73     80  0
006600040318     D  Stgs11                81     88  0
006700040318     D  Stgs12                89     96  0
006800040311     d    NSP                  1     96  0 dim(12)
006900040311      *
007000040318     D Stg_COL_DS      DS
007100040318     D  Stgc01                 1     11  0
007200040318     D  Stgc02                12     22  0
007300040318     D  Stgc03                23     33  0
007400040318     D  Stgc04                34     44  0
007500040318     D  Stgc05                45     55  0
007600040318     D  Stgc06                56     66  0
007700040318     D  Stgc07                67     77  0
007800040318     D  Stgc08                78     88  0
007900040318     D  Stgc09                89     99  0
008000040318     D  Stgc10               100    110  0
008100040318     D  Stgc11               111    121  0
008200040318     D  Stgc12               122    132  0
008300040311     d    COL                  1    132  0 dim(12)
008400040311      *
008500040318     D Stg_PES_DS      DS
008600040318     D  Stgp01                 1     12  1
008700040318     D  Stgp02                13     24  1
008800040318     D  Stgp03                25     36  1
008900040318     D  Stgp04                37     48  1
009000040318     D  Stgp05                49     60  1
009100040318     D  Stgp06                61     72  1
009200040318     D  Stgp07                73     84  1
009300040318     D  Stgp08                85     96  1
009400040318     D  Stgp09                97    108  1
009500040318     D  Stgp10               109    120  1
009600040318     D  Stgp11               121    132  1
009700040318     D  Stgp12               133    144  1
009800040311     d    PES                  1    144  1 dim(12)
009900040311      *
010000040318     D Stg_VOL_DS      DS
010100040318     D  Stgv01                 1     10  3
010200040318     D  Stgv02                11     20  3
010300040318     D  Stgv03                21     30  3
010400040318     D  Stgv04                31     40  3
010500040318     D  Stgv05                41     50  3
010600040318     D  Stgv06                51     60  3
010700040318     D  Stgv07                61     70  3
010800040318     D  Stgv08                71     80  3
010900040318     D  Stgv09                81     90  3
011000040318     D  Stgv10                91    100  3
011100040318     D  Stgv11               101    110  3
011200040318     D  Stgv12               111    120  3
011300040311     d    VOL                  1    120  3 dim(12)
011400000000     D*-------------------
011500000000     D* ARCHITETTURA
011600000000     D*-------------------
011700000000     D KPJBA         E DS
011800000000     D*-------------------
011900000000     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
012000000000     D*-------------------
012100991028     D                 DS                  INZ
012200991028     D  MM                     1      2  0
012300991028     D  MMGG                   1      4  0
012400000000     D                 DS                  INZ
012500000000     D  DIANN                  1      4  0
012600000000     D  DIMES                  5      6  0
012700000000     D  DIMGS                  5      8  0
012800000000     D  DIGIO                  7      8  0
012900040317     D  DIAME                  3      6  0
013000000000     D  DIAAM                  1      6  0
013100991213     D  DIDAT                  1      8  0
013200000000     D                 DS                  INZ
013300000000     D  DFANN                  1      4  0
013400000000     D  DFMES                  5      6  0
013500000000     D  DFMGS                  5      8  0
013600000000     D  DFGIO                  7      8  0
013700000000     D  DFAAM                  1      6  0
013800040317     D  DFAME                  3      6  0
013900040317     D  DFDAT                  1      8  0
014000000000     D*-------------------
014100000000     D* DS SCOMPOSIZIONE CODICE CLIENTE
014200000000     D*-------------------
014300000000     D                 DS
014400000000     D  DSCLI                  1      7  0                                      *CLIEN
014500000000     D  DSFIL                  1      3  0                                      *FIL
014600000000     D  DSCOD                  4      7  0                                      *COD
014700040312     D*-------------------
014800040312     D* DS Pgm richiamati
014900040312     D*-------------------
015000040415     d***** tibs10ds      E DS
015100040312     D*-------------------
015200040312     D TCUDS           DS
015300040312     D  F3                     3      3
015400040312     D  F4                     4      4
015500040312     D  F56                    5      6
015600040312     D*-----
015700040312     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
015800040312     D  TCU                  398    697
015900040312     D                                     DIM(50)
016000040312     D  KCU                  698    847P 0
016100040312     D                                     DIM(50)
016200000000     D*-------------------
016300000000     D* DS PARAMETRI DI LANCIO PGM
016400000000     D*-------------------
016500000000     D PARAM           DS
016600000000     D  PARTLA                 1      1
016700000000     D  PARANN                 2      5  0
016800000000     D  PARMMI                 6      7  0
016900000000     D  PARMMF                 8      9  0
017000040309     D*-------------------
017100040309     D* Definizione Campi
017200040309     D*-------------------
017300000000     C*------------------------------------------------------------------------*
017400000000     C* MAIN LINE
017500000000     C*------------------------------------------------------------------------*
017600040311     c     *loval        setll     cnclp00f
017700040311     c                   read      cnclp00f
017800040311     C*
017900040311     c                   dow       not %eof(cnclp00f)
018000040311      *
018100040311      * Scarta a priori i clienti generici
018200040312     C                   Z-ADD     read_clpksc   N4                4 0          *CLIENTE MITTENTE
018300040317     C                   movel     read_clpksc   N3                3 0          *CLIENTE MITTENTE
018400040317     c                   if        N4 <> 8888 and N4 <> 9999 and
018500040317     c                             N3 <> 180
018600040311      *
018700040312      *  Controlla se l'ultima fattura fatta dal cliente ci permette
018800040312      *  di considerare il cliente altrimenti lo salta
018900040312      *-  Data Inizio/Fine
019000040312     c                   movel     read_clpdps   da_anme_4         4 0          AAMM
019100040317     c                   move      da_anme_4     da_anme_6         6 0          AAAAMM
019200040317     c                   movel     20            da_anme_6                      AAAAMM
019300040317     c                   if        da_anme_4 > 4000
019400040317     c                   movel     19            da_anme_6                      AAAAMM
019500040317     c                   end
019600040312     c                   movel     read_clpdus   a_anme_4          4 0          AAMM
019700040317     c                   move      a_anme_4      a_anme_6          6 0          AAAAMM
019800040317     c                   movel     20            a_anme_6                       AAAAMM
019900040317     c                   if        a_anme_4 > 4000
020000040317     c                   movel     19            a_anme_6                       AAAAMM
020100040317     c                   end
020200040312      *
020300040317     c                   if        DIaam  <= a_anme_6  and
020400040317     c                             DFaam  >= da_anme_6
020500040312      *
020600040415      *  Controlla se il cliente UNificante è di tipo Direzionale/TOP/A
020700040415     c                   clear                   Tipo_UNIfic       1
020800040415     c                   clear                   Cod_Unific        7 0
020900040415     c                   call      'TIST81R1'
021000040415     c                   parm                    read_clpkut
021100040415     c                   parm                    read_clpkcc
021200040415     c                   parm                    read_clpksc
021300040415     c                   parm                    Cod_Unific
021400040415     c                   parm                    Tipo_UNIfic
021500040415      *
021600040311     C*   se è un tipo cliente Direzionale Top o A
021700040415     c                   if        Tipo_UNIFic = 'D' or
021800040415     c                             Tipo_UNIFic = 'T' or
021900040415     c                             Tipo_UNIFic = 'A'
022000040312      *
022100040312     C                   clear                   WFSTG000
022200040311     c                   exsr      Legge_TAS
022300040312      *
022400040311     c                   end
022500040312     c                   end
022600040311     C*
022700040311     c                   end
022800040311     C*
022900040311     c                   read      cnclp00f
023000040311     c                   enddo
023100000000     C*
023200040312     c     fine          tag
023300000000     C                   SETON                                        LR
023400000000     C*------------------------------------------------------------------------*
023500040311     C* Lettura del TITAS
023600000000     C*------------------------------------------------------------------------*
023700040311     C     Legge_TAS     BEGSR
023800040311     C*
023900040311     C*  Pulizia campi di totalizzazione
024000040311     c                   clear                   NSP
024100040311     c                   clear                   COL
024200040311     c                   clear                   PES
024300040311     c                   clear                   VOL
024400040311     C*
024500040415     C*  Posizionamento
024600040311     C                   Z-ADD     DIANN         KTAAAS
024700040311     C                   Z-ADD     DIMGS         KTAMGS
024800040311     C*
024900040311     c                   move      'N'           almeno_uno        1
025000040419     C     read_clpksc   setll     TITAS31C
025100040419     C     read_clpksc   reade     TITAS31C
025200040311     C*
025300040311     C*  Fino al range richiesto
025400040419     c                   dow       not %eof(TITAS31C)
025500040311     C*
025600040311     C*  Controlla se il record è valido x essere preso in considerazione
025700040311     c                   move      'OK'          record_ok         2
025800040311     C                   EXSR      Check_rec
025900040311     C*
026000040311     C*   Solo se il record è valido somma nei campi di salvataggio
026100040311     c                   if        record_ok ='OK'
026200040311     c                   move      'S'           almeno_uno        1
026300040311     C                   EXSR      Somma_rec
026400040311     c                   end
026500040311     C*
026600040419     C     read_clpksc   reade     TITAS31C
026700040311     c                   enddo
026800040311     C*
026900040311     C* CREA IL RECORD
027000040311     c                   if        almeno_uno = 'S'
027100040311     C                   EXSR      Crea_Rec
027200040311     c                   end
027300040311     C*
027400040311     C                   ENDSR
027500000000     C*------------------------------------------------------------------------*
027600000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
027700000000     C*------------------------------------------------------------------------*
027800040311     C     Check_rec     BEGSR
027900040305     C*
028000040419     C*  E' una bolla da prendere in considerazione nel range richiesto ?
028100040419     C* Controllo  l'anno  della spedizione
028200040419     C                   if        TASAAS <> parANN
028300040419     C                   eval      record_ok ='NO'
028400040419     c                   goto      new_read
028500040419     C                   ENDIF
028600040419     C*
028700040419     C* Controllo  il mese della spedizione
028800040419     C                   Z-ADD     TASMGS        MMGG
028900040419     C*
029000040419     C                   if        MM < parMMI or
029100040419     C                             MM > parMMF
029200040419     C                   eval      record_ok ='NO'
029300040419     c                   goto      new_read
029400040419     C                   ENDIF
029500040419     C*
029600000000     C* ESCLUDE LE BOLLE NON VALIDE COME "SPEDIZIONE"
029700000000     C                   SETOFF                                           28
029800040311     C                   Z-ADD     1             J
029900000000     C     TASTBL        LOOKUP    TB(J)                                  28
030000040311     C   28SP(J)         IFEQ      'N'
030100040311     C                   eval      record_ok ='NO'
030200040311     c                   goto      new_read
030300000000     C                   ENDIF
030400000000     C*
030500040312     C* Deve scartare le bolle poste x Franchi con cod CLIENTE <> MITTENTE
030600040312     C                   IF        tasCCM <> tasKSC  and tasLNP = 180 and
030700040312     c                             %subst(tasTBL:1:1) = 'F'
030800040312     C                   eval      record_ok ='NO'
030900040312     c                   goto      new_read
031000040312     c                   end
031100040312     C*
031200040309     C     new_read      tag
031300040309     C*
031400000000     C                   ENDSR
031500000000     C*------------------------------------------------------------------------*
031600040311     C* Somma  - Somma i dati x emissione successiva record
031700000000     C*------------------------------------------------------------------------*
031800040311     C     Somma_rec     BEGSR
031900040311     C*
032000040419     C* Prende solo il mese della spedizione (spostato nella Rout.di Check)
032100040419     C**********         Z-ADD     TASMGS        MMGG
032200040311     C*
032300040311     C*  campi di totali
032400040311     c                   add       1             NSP(MM)
032500040311     c                   add       TASncl        COL(MM)
032600040311     c                   add       TASpkf        PES(MM)
032700040311     c                   add       TASvlf        VOL(MM)
032800040311     C*
032900040311     C                   ENDSR
033000040311     C*------------------------------------------------------------------------*
033100040311     C* CREA   - CREA IL RECORD SUL FILE DI LAVORO
033200040311     C*------------------------------------------------------------------------*
033300040311     C     CREA_Rec      BEGSR
033400040311     C*
033500040415     c                   move      Tipo_UNIfic   StgCIC
033600040318     c                   move      read_clpKSC   StgCCM
033700040312      *
033800040311     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE GIA' CONVERTITO
033900040311     C                   EXSR      DECCLI
034000040312      *
034100040318     c                   move      PARANN        StgANN
034200040318     c                   move      PARMMI        StgDME
034300040318     c                   move      PARMMF        StgAME
034400040312      *
034500040312     C                   WRITE     WFSTG000
034600991025     C*
034700000000     C                   ENDSR
034800040220     C*------------------------------------------------------------------------*
034900040220     C* DECCLI - DECODIFICA CLIENTE
035000040220     C*------------------------------------------------------------------------*
035100040220     C     DECCLI        BEGSR
035200040220     C*
035300040312     C* Ricerca codice unificante
035400040415     C*  già fatta in precedenza con CALLB esterna quindi non rieseguo
035500040415     c**********         clear                   tibs10ds
035600040415     C**********
035700040415     c**********         clear                   Cli_Unif         11 0
035800040415     c**********         movel     'ST'          d10TLE
035900040415     c**********         z-add     read_CLPKSC   d10COD
036000040415     c**********         movel     'P'           d10PAF
036100040415     c**********         z-add     *date         d10DRF
036200040415     c**********         CALL      'TIBS10R'
036300040415     c**********         PARM                    tibs10ds
036400040415     c**********         if        d10ERR = *blank
036500040415     c**********         z-add     d10COP        Cli_Unif
036600040415     c**********         else
036700040415     c**********         z-add     read_CLPKSC   Cli_Unif
036800040415     c**********         end
036900040415     C**********
037000040415     C*  Descrizione cliente unificante
037100040415     c**********         z-add     Cli_Unif      StgKSU
037200040415     C**********         z-add     Cli_Unif      codUNI            7 0
037300040415     C**********
037400040415     c                   z-add     Cod_Unific    StgKSU
037500040415     C                   z-add     Cod_Unific    codUNI            7 0
037600040312      *-----
037700040312     C                   CLEAR                   BS69DS
037800040312     C                   CLEAR                   ACODS
037900040312     C                   MOVEL     KNSIF         I69SIF                         *S.I.
038000040312     C                   Z-ADD     codUNI        I69KAC                         *CLIENTE X CNACO
038100040312     C                   Z-ADD     codUNI        I69KIN                         *CLIENTE X CNIND
038200040312     C                   CALL      'TIBS69R'
038300040312     C                   PARM                    BS69DS
038400040312     C                   PARM                    ACODS
038500040312     C                   PARM                    INDDS
038600040312     C                   PARM                    CLPDS
038700040312     C                   PARM                    CLSDS
038800040312     C     O69ERR        IFNE      '1'                                          *NO ERRORE
038900040318     c                   movel     AcoRAG        StgRSC
039000040318     c                   movel     IndCIT        StgLOC
039100040318     c                   movel     IndPRV        StgPRO
039200040312     C                   ENDIF
039300040312     c*-------
039400040312     C*
039500040312     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
039600040312     C                   Z-ADD     codUNI        DSCLI
039700040312     C                   Z-ADD     DSFIL         wFIL              3 0
039800040312      *               *--------------------*
039900040312     C                   EXSR      DIVARE
040000040312      *               *--------------------*
040100040318     C                   z-add     wFIL          StgPUN
040200040318     C                   MOVEL     wDIV          StgDIT
040300040318     C                   z-add     wARE          StgARE
040400040312     C*
040500040220     C                   ENDSR
040600000000     C*------------------------------------------------------------------------*
040700000000     C* DIVARE - RECUPERA AREA E DIVISIONE
040800000000     C*------------------------------------------------------------------------*
040900000000     C     DIVARE        BEGSR
041000000000     C*
041100000000     C                   Z-ADD     1             I
041200000000     C     WFIL          LOOKUP    FIL(I)                                 99
041300000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
041400000000     C                   MOVEL     '*'           WDIV              1
041500000000     C                   Z-ADD     999           WARE              3 0
041600000000     C                   ELSE                                                   <-- 3 -->
041700000000     C                   MOVEL     DIV(I)        WDIV
041800000000     C                   Z-ADD     ARE(I)        WARE
041900000000     C                   END                                                    <-- 3 ---
042000000000     C*
042100000000     C                   ENDSR
042200040220     C*------------------------------------------------------------------------*
042300000000     C* *INZSR - ROUTINE INIZIALE
042400000000     C*------------------------------------------------------------------------*
042500000000     C     *INZSR        BEGSR
042600000000     C*--------------------
042700000000     C* RICEVIMENTO PARAMETRI
042800000000     C*--------------------
042900000000     C     *ENTRY        PLIST
043000000000     C                   PARM                    KPJBA
043100000000     C                   MOVEL     KPJBU         PARAM
043200000000     C*--------------------
043300000000     C* CAMPI RIFERITI AL DATA BASE
043400000000     C*--------------------
043500000000     C     *LIKE         DEFINE    TASAAS        KTAAAS
043600000000     C     *LIKE         DEFINE    TASMGS        KTAMGS
043700000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
043800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
043900000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
044000040312     C     *LIKE         DEFINE    read_clpKSC   WCLI
044100000000     C*--------------------
044200000000     C* CHIAVI DI LETTURA
044300000000     C*--------------------
044400040419     C* CHIAVE LETTURA TITAS31C - COMPLETA
044500040311     C     keyTAS        KLIST
044600040312     C                   KFLD                    read_CLPKSC
044700000000     C                   KFLD                    KTAAAS
044800000000     C                   KFLD                    KTAMGS
044900040311      *----
045000040311     C     keyAAS        KLIST
045100040312     C                   KFLD                    read_CLPKSC
045200040311     C                   KFLD                    KTAAAS
045300000000     C*
045400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
045500040311     C     keyTAB        KLIST
045600000000     C                   KFLD                    KTBUTE
045700000000     C                   KFLD                    KTBCOD
045800000000     C                   KFLD                    KTBKEY
045900000000     C*
046000040312     C     keyKSC        KLIST
046100040312     C                   KFLD                    Codut
046200040312     C                   KFLD                    KCI
046300040312     C                   KFLD                    codUNI            7 0
046400040312     C*
046500000000     C*--------------------
046600000000     C* CAMPI DI CONTROLLO
046700000000     C*--------------------
046800000000     C                   MOVEL     '0'           FINL00            1
046900000000     C                   Z-ADD     1             CODUT             1 0
047000991025     C                   Z-ADD     *ZEROS        MEMCLI            7 0
047100991025     C                   Z-ADD     *ZEROS        MEMCTR            3 0
047200040311     C*
047300040311     C* POSIZIONAMENTO E PRIMA LETTURA
047400040311     C                   MOVE      PARANN        DIANN
047500040311     C                   MOVE      PARANN        DFANN
047600040311     C                   MOVE      PARMMI        DIMES
047700040311     C                   MOVE      PARMMF        DFMES
047800040311     C                   MOVE      '99'          DFGIO
047900040312     C*--- RICERCA CAPOCONTI------------------------------------------*
048000040312     C                   CALL      'X§PARUT'
048100040312     C                   PARM                    UTEDSE
048200040312     C     CODUT         CABEQ     -1            FINE
048300040312     C*----------
048400040312     C                   DO        50            X                 2 0
048500040312     C                   MOVE      TCU(X)        TCUDS
048600040312     C     F56           IFEQ      'CG'
048700040312     C     F4            ANDEQ     '1'
048800040312     C     F3            ANDEQ     '0'
048900040312     C                   Z-ADD     KCU(X)        KCI               4 0
049000040312     C                   END
049100040312     C                   END
049200040309      *
049300040309      * CARICA ORGANIGRAMMA
049400000000     C                   EXSR      CARORG
049500040309      *
049600040309      * CARICA TIPI BOLLA
049700040309     C                   EXSR      CARTBL
049800040309      *
049900000000     C                   ENDSR
050000040309      *------------------------------------------------------------------------*
050100040309      * CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
050200040309      *------------------------------------------------------------------------*
050300040309     C     CARTBL        BEGSR
050400040309      *--------------------
050500040309      * FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
050600040309      *--------------------
050700040309     C                   Z-ADD     0             I
050800040309     C                   Z-ADD     CODUT         KTBUTE
050900040309     C                   MOVEL     '2W'          KTBCOD
051000040309     C                   MOVEL     *BLANKS       KTBKEY
051100040309     C     KEYTAB        SETLL     TABEL00F
051200040309     C                   READ      TABEL00F                               99
051300040309     C     *IN99         DOWEQ     *OFF
051400040309     C     TBLCOD        ANDEQ     '2W'
051500040309     C     TBLFLG        IFEQ      *BLANKS
051600040309     C                   ADD       1             I
051700040309     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
051800040309     C                   MOVEL     TBLUNI        DS2W
051900040309     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
052000040309     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
052100040309     C                   END
052200040309     C                   READ      TABEL00F                               99
052300040309     C                   END
052400040309      *--------------------
052500040309      * TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
052600040309      *--------------------
052700040309     C                   Z-ADD     0             I
052800040309     C                   Z-ADD     CODUT         KTBUTE
052900040309     C                   MOVEL     'TB'          KTBCOD
053000040309     C                   MOVEL     *BLANKS       KTBKEY
053100040309     C     KEYTAB        SETLL     TABEL00F
053200040309     C                   READ      TABEL00F                               99
053300040309     C     *IN99         DOWEQ     *OFF
053400040309     C     TBLCOD        ANDEQ     'TB'
053500040309     C     TBLFLG        IFEQ      *BLANKS
053600040309     C                   ADD       1             I
053700040309     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
053800040309     C                   MOVEL     TBLUNI        DSTB
053900040311     C                   Z-ADD     1             J                 5 0
054000040309     C                   SETOFF                                       28
054100040309     C     §TBRBL        LOOKUP    RBL(J)                                 28
054200040309     C     *IN28         IFEQ      *ON
054300040309     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
054400040309     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
054500040309     C                   ELSE
054600040309     C                   MOVEL     'N'           SP(I)
054700040309     C                   MOVEL     'N'           RI(I)
054800040309     C                   ENDIF
054900040309     C                   END
055000040309     C                   READ      TABEL00F                               99
055100040309     C                   END
055200040309      *
055300040309     C                   ENDSR
055400040309      *------------------------------------------------------------------------*
055500040309      * CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
055600040309      *------------------------------------------------------------------------*
055700040309     C     CARORG        BEGSR
055800040309      *
055900040312     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
056000040309     C  N99              READ      AZORG01L                               99
056100040309     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
056200040309     C     *IN99         DOWEQ     '0'                                          --- 1 -->
056300040309     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
056400040309     C     ORGFAG        IFNE      'V'
056500040309     C                   ADD       1             I
056600040309     C                   Z-ADD     ORGFIL        FIL(I)
056700040309     C                   Z-ADD     ORGCAR        ARE(I)
056800040309     C                   MOVEL     ORGFL3        DIV(I)
056900040309     C                   MOVEL     ORGDIT        DIT(I)
057000040309     C                   END
057100040309     C                   END                                                    <-- 2 ---
057200040309     C                   READ      AZORG01L                               99
057300040309     C                   END                                                    <-- 1 ---
057400040309      *
057500040309     C                   ENDSR
057600040309      *------------------------------------------------------------------------*
