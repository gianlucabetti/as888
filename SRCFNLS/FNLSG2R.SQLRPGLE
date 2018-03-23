000100140611     /*PRM  dbgview(*source)
000200140611     /*END
000300000000     H*------------------------------------------------------------------------*
000400991029     H* STATISTICA VOLUMI CML - GENERA WRK FILE
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700140611     h option(*nodebugio)
000800140611     F*------------------------------------------------------------------------*
000900080318     Ffnblt01l  IF   E           K DISK
001000140611     fAZCMM01L  if   e           k disk
001100000000     FAZORG01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300090709     FWFVLMF1L  UF A E           K DISK
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000216     D*------------------
001800000216     D* DECODIFICA
001900000216     D*------------------
002000000216     D DEC             S             40    DIM(5) CTDATA PERRCD(1)
002100000000     D*-------------------
002200000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
002300000000     D*-------------------
002400000000     D FIL             S              3  0 DIM(500)
002500000000     D ARE             S              3  0 DIM(500)
002600000000     D DIV             S              1    DIM(500)
002700000000     D DIT             S              3    DIM(500)
002800000000     D*-------------------
002900000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
003000000000     D*-------------------
003100000000     D RBL             S              1    DIM(5)
003200000000     D FSP             S              1    DIM(5)
003300000000     D FRI             S              1    DIM(5)
003400000000     D*-------------------
003500000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003600000000     D*-------------------
003700000000     D TB              S              2    DIM(20)
003800000000     D SP              S              1    DIM(20)
003900000000     D RI              S              1    DIM(20)
004000080318     D cbo             S              2    DIM(99)
004100080318     D tbl             S              2    DIM(99)
004200000000     D*------------------------------------------------------------------------*
004300000000     D* INPUT
004400000000     D*------------------------------------------------------------------------*
004500000000     D*-------------------
004600000000     D* ARCHITETTURA
004700000000     D*-------------------
004800000000     D KPJBA         E DS
004900080318     D*-------------------
005000080318     D fnblpds       E DS                  EXTNAME(fnblp00f)
005100000000     D*-------------------
005200000000     D* DS TIPO BOLLA
005300000000     D*-------------------
005400000000     D DSTB          E DS
005500000000     D*-------------------
005600000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
005700000000     D*-------------------
005800000000     D DS2W          E DS
005900080318     D DS3a          E DS
006000991029     D*------------------
006100991029     D* DS REPERIMENTO DATI UTENTE
006200991029     D*-------------------
006300991029     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
006400991029     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
006500991029     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
006600991029     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
006700991029     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
006800000000     D*-------------------
006900000000     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
007000000000     D*-------------------
007100991028     D                 DS                  INZ
007200991028     D  MM                     1      2  0
007300991028     D  MMGG                   1      4  0
007400000000     D                 DS                  INZ
007500000000     D  DIANN                  1      4  0
007600000000     D  DIMES                  5      6  0
007700000000     D  DIMGS                  5      8  0
007800000000     D  DIGIO                  7      8  0
007900000000     D  DIAAM                  1      6  0
008000000000     D                 DS                  INZ
008100000000     D  DFANN                  1      4  0
008200000000     D  DFMES                  5      6  0
008300000000     D  DFMGS                  5      8  0
008400000000     D  DFGIO                  7      8  0
008500000000     D  DFAAM                  1      6  0
008600000000     D*-------------------
008700000000     D* DS SCOMPOSIZIONE CODICE CLIENTE
008800000000     D*-------------------
008900000000     D                 DS
009000000000     D  DSCLI                  1      7  0
009100000000     D  DSFIL                  1      3  0
009200000000     D  DSCOD                  4      7  0
009300000000     D*-------------------
009400000000     D* DS PARAMETRI DI LANCIO PGM
009500000000     D*-------------------
009600000000     D PARAM           DS
009700991029     D  PARTLA                 1      1
009800080318     D**PARANN                 2      5  0
009900080318     D**PARMMI                 6      7  0
010000080318     D**PARMMF                 8      9  0
010100080318     d  pardal                 2      9  0
010200080318     D**PARSNR                10     10
010300991029     D  PARPID                27     31  2
010400080318     D**PARDIV                32     32
010500080318     D**PARARE                33     35  0
010600080318     D**PARFIL                36     38  0
010700080318     D  PARtfp                36     38  0
010800991029     D  PARDET                39     39
010900080318     D**PARTOT                40     40
011000991029     D  PARRG1                41     44  1
011100991029     D  PARRG2                45     48  1
011200080318     D**PARPIA                49     53  2
011300991029     D  PARPSD                54     58  2
011400080318     D**PARPSA                59     63  2
011500080318     D**PAROK                 65     66
011600991229     D  PARMIN                67     71  3
011700080318     D**PARPOS                72     72
011800080318     D**PARNT1                73     75
011900080318     D**PARNT2                76     78
012000080318     D**PARNT3                79     81
012100080318     D**PARNT4                82     84
012200080318     D**PARSAL                85     85
012300080318     d  paral                 86     93  0
012400090709     d  parord                75     75
012500090709     d  parpag                76     76
012600090709     d  parflp                77     79  0
012700080318
012800080318     d totvuc                         8  6
012900080318     d wvlc            s                   like(blpvlc)
013000080318     d wncr            s                   like(blpncr)
013100090709      *
013200090709     d w_clpage        s                   like(clpage)
013300000000     C*------------------------------------------------------------------------*
013400000000     C* MAIN LINE
013500000000     C*------------------------------------------------------------------------*
013600000000     C*
013700080318     c/exec sql
013800080318     c+ set option dynusrprf = *owner, closqlcsr = *endmod
013900080318     c/end-exec
014000080318
014100090709     c                   if        parflp=0
014200080318     C/EXEC SQL
014300080318     C+ Declare A1 Cursor for
014400080318     c+ SELECT * FROM fnblp00f
014500080318     c+ WHERE blptfp = :partfp AND blpaas*10000+blpmgs
014600080318     c+ between :PARdal and :paral
014700080318     C/END-EXEC
014800080318      *          apertura cursore
014900080318     C/EXEC SQL
015000080318     C+ OPEN A1
015100080318     C/END-EXEC
015200090709     c                   else
015300090709     C/EXEC SQL
015400090709     C+ Declare B1 Cursor for
015500090709     c+ SELECT * FROM fnblp00f
015600090709     c+ WHERE blptfp = :partfp AND blplnp= :parflp AND blpaas*10000+blpmgs
015700090709     c+ between :PARdal and :paral
015800090709     C/END-EXEC
015900090709      *          apertura cursore
016000090709     C/EXEC SQL
016100090709     C+ OPEN B1
016200090709     C/END-EXEC
016300090709     c                   endif
016400080318     C                   DOU       SqlCod <> 0
016500090709     c                   if        parflp=0
016600080318      *          lettura cursore
016700080318     C/EXEC SQL
016800080318     C+ Fetch Next From A1 Into :fnblpds
016900080318     C/END-EXEC
017000090709     c                   else
017100090709      *          lettura cursore
017200090709     C/EXEC SQL
017300090709     C+ Fetch Next From B1 Into :fnblpds
017400090709     C/END-EXEC
017500090709     c                   endif
017600080318     C                   SELECT
017700080318     C                   WHEN      SqlCod = 100
017800080318     c                   leave
017900080318      *
018000080318     C                   WHEN      SqlCod < 0
018100080318     c                   leave
018200080318      *
018300080318     C                   OTHER
018400080318      * legge e scrive
018500080318     c                   exsr      chkrec
018600080318     c                   if        wrecok='S'
018700080318     C                   EXSR      CREARE
018800080318     c                   endif
018900080318      *
019000080318     C                   ENDSL
019100080318      *
019200080318     C                   ENDDO
019300090709     c                   if        parflp=0
019400080318     C/EXEC SQL
019500080318     C+ Close A1
019600080318     C/END-EXEC
019700090709     c                   else
019800090709     C/EXEC SQL
019900090709     C+ Close B1
020000090709     C/END-EXEC
020100090709     c                   endif
020200991029     C*
020300991029     C* OPERAZIONI FINALI
020400991029     C                   EXSR      FINSR
020500000000     C*
020600000000     C                   SETON                                        LR
020700000000     C*------------------------------------------------------------------------*
020800000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
020900000000     C*------------------------------------------------------------------------*
021000000000     C     CHKREC        BEGSR
021100000000     C*
021200000000     C                   MOVEL     'S'           WRECOK            1
021300001011     C*
021400001011     C* NUOVO CONTROLLO ==> SE IL CAMPO TASNCR = 99999 ==> ESCLUDO COMPLETAMENTE
021500080318     C***  WRECOK        IFEQ      'S'
021600080318     C***  blpncr        IFEQ      99999
021700080318     C***                MOVEL     'N'           WRECOK
021800080318     C***                ENDIF
021900080318     C***                ENDIF
022000000000     C*
022100000000     C* ESCLUDE LE BOLLE NON VALIDE COME "SPEDIZIONE"
022200000000     C     WRECOK        IFEQ      'S'
022300000000     C                   Z-ADD     1             J
022400080318     C                   Z-ADD     1             k                 4 0
022500000000     C                   SETOFF                                           28
022600080318     c     blpcbo        lookup    cbo(J)                                 28
022700080318     C     tbl(J)        LOOKUP    TB(K)                                  28
022800080318     C     SP(K)         IFEQ      'N'
022900000000     C                   MOVEL     'N'           WRECOK
023000000000     C                   ENDIF
023100000000     C                   ENDIF
023200000000     C*
023300000000     C* SE E' ASSEGNATO PRENDE CLIENTE MITTENTE, SE FRANCO CLIENTE DESTINO
023400000000     C     WRECOK        IFEQ      'S'
023500000000     C                   Z-ADD     *ZEROS        WCLI
023600000000     C                   Z-ADD     *ZEROS        WCTR
023700080318     C                   MOVEL     Tbl(J)        A1                1
023800000000     C     A1            IFEQ      'A'
023900080318     C     blpccm        IFEQ      *ZEROS
024000000000     C                   MOVEL     'N'           WRECOK
024100000000     C                   ELSE
024200080318     C                   Z-ADD     blpCCM        WCLI
024300000000     C                   Z-ADD     999           WCTR
024400000000     C                   ENDIF
024500000000     C                   ELSE
024600080318     C                   Z-ADD     blpKSC        WCLI
024700080318     C                   MOVE      blpKSC        N4                4 0
024800000000     C     N4            IFEQ      8888
024900000000     C                   Z-ADD     888           WCTR
025000000000     C                   ELSE
025100080318     C                   Z-ADD     blpctr        WCTR
025200000000     C                   ENDIF
025300000000     C                   ENDIF
025400991029     C                   ENDIF
025500991029     C*
025600991029     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE GIA' CONVERTITO
025700991029     C                   EXSR      DECCLI
025800000000     C*
025900000000     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
026000000000     C     WRECOK        IFEQ      'S'
026100000000     C                   Z-ADD     WCLI          DSCLI
026200000000     C                   Z-ADD     DSFIL         WFIL              3 0
026300080318     C***                EXSR      DIVARE
026400080318     C***                MOVEL     WDIV          WDIV              1
026500080318     C***                Z-ADD     WARE          WARE              3 0
026600000000     C                   ENDIF
026700000000     C*
026800080318     C**   WRECOK        IFEQ      'S'
026900000000     C*
027000000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
027100000000     C*
027200080318     C**   PARDIV        IFNE      *BLANKS
027300080318     C**   WDIV          IFEQ      PARDIV
027400080318     C**                 ELSE
027500080318     C**                 MOVEL     'N'           WRECOK
027600080318     C**                 ENDIF
027700080318     C**                 ENDIF
027800000000     C*
027900000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
028000080318     C**   PARARE        IFNE      *ZEROS
028100080318     C**   WARE          IFEQ      PARARE
028200080318     C**                 ELSE
028300080318     C**                 MOVEL     'N'           WRECOK
028400080318     C**                 ENDIF
028500080318     C**                 ENDIF
028600000000     C*
028700000000     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
028800080318     C**   PARFIL        IFNE      *ZEROS
028900080318     C***  TASLNP        IFNE      PARFIL
029000080318     C***                MOVEL     'N'           WRECOK
029100080318     C***                ENDIF
029200080318     C***                ENDIF
029300080318     C***                ENDIF
029400000000     C*
029500000000     C                   ENDSR
029600000000     C*------------------------------------------------------------------------*
029700000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
029800000000     C*------------------------------------------------------------------------*
029900000000     C     CREARE        BEGSR
030000991029     C*
030100991029     C* IMPOSTA IL FLAG TIPO VOLUME (REALE O NON REALE)
030200040211IF  1C*--- TASFVB        IFNE      'R'
030300040211     C*---               MOVEL     *BLANKS       TASFVB
030400040211E   1C*---               ENDIF
030500991025     C*
030600080318     C                   clear                   KWVANN
030700991029     C                   MOVEL     WDIV          KWVDIV
030800991029     C                   Z-ADD     WARE          KWVARE
030900080318     C                   Z-ADD     partfp        KWVFIL
031000991029     C                   MOVEL     WFVRAG        KWVRAG
031100991029     C                   Z-ADD     WCLI          KWVKSC
031200991029     C                   Z-ADD     WCTR          KWVCTR
031300080318     C                   clear                   KWVMES
031400040211     C*---               MOVEL     TASFVB        KWVFVB
031500040211     C                   MOVEL     *BLANKS       KWVFVB
031600000000     C*
031700000000     C* SCRIVO O RISCRIVO FILE DI LAVORO PER STAMPA STATISTICA PESI
031800000000     C*
031900090709     C     KEYWFV        CHAIN     WFVLMF1L                           99
032000000000     C*
032100000000     C* RIEMPIO BUFFER RECORD
032200000000     C*
032300991029     C                   Z-ADD     KWVANN        WFVANN
032400991029     C                   MOVEL     KWVDIV        WFVDIV
032500991029     C                   Z-ADD     KWVARE        WFVARE
032600991029     C                   Z-ADD     KWVFIL        WFVFIL
032700991029     C                   Z-ADD     KWVKSC        WFVKSC
032800991029     C                   Z-ADD     KWVCTR        WFVCTR
032900991029     C                   Z-ADD     KWVMES        WFVMES
033000991029     C                   MOVEL     KWVFVB        WFVFVB
033100991029     C   99              EXSR      AZZWFV
033200991029     C                   EXSR      ADDWFV
033300090709     C   99              WRITE     WFVOLF
033400090709     C  N99              UPDATE    WFVOLF
033500991025     C*
033600000000     C                   ENDSR
033700000000     C*------------------------------------------------------------------------*
033800090709     C* ADDWFV - AGGIUNGE LA SPEDIZIONE AL RECORD WFVLMF0F
033900000000     C*------------------------------------------------------------------------*
034000991029     C     ADDWFV        BEGSR
034100991029     C*
034200991029     C                   MOVEL     'S'           ESEGUI            1
034300991025     C*
034400080318     C                   ADD       blpNCL        WFVTCL
034500080318     c                   exsr      sr_blt
034600991104     C*
034700080318     C**   TASFVF        IFEQ      'T'
034800080318     C**                 Z-ADD     TASVLF        WVLC
034900080318     C**                 ELSE
035000080318     C     blpncl        IFEQ      wncr
035100080318     C     wvlc          IFeq      *ZEROS
035200991104     C                   MOVEL     'N'           ESEGUI
035300991104     C                   ENDIF
035400991104     C                   ELSE
035500080318     C     wvlc          IFle      blpvlb
035600991104     C                   MOVEL     'N'           ESEGUI
035700991104     C                   ENDIF
035800991104     C                   ENDIF
035900080318     C***                ENDIF
036000991104     C*
036100991029     C     ESEGUI        IFEQ      'S'
036200991029     C                   ADD       WVLC          WFVVLC
036300080318     C                   ADD       blpVLB        WFVVLB
036400080318     C                   ADD       blpNCL        WFVNCL
036500080318     C                   ADD       wncr          WFVNCR
036600080318     C                   ADD       blppkb        WFVPKB
036700991029     C                   ADD       1             WFVNSP
036800991026     C                   ENDIF
036900991025     C*
037000000000     C                   ENDSR
037100080318     C*------------------------------------------------------------------------*
037200080318     C* sr_blt - legge e totalizza volume vdl rilevato in partenza
037300080318     C*------------------------------------------------------------------------*
037400080318     C     sr_blt        BEGSR
037500080318     c                   clear                   totvuc
037600080318     c                   clear                   wvlc
037700080318     c                   clear                   wncr
037800080318     c     kblt          setll     fnblt01l
037900080318     c                   do        *hival
038000080318     c     kblt          reade     fnblt01l
038100080318     c                   if         %eof(fnblt01l)
038200080318     c                   leave
038300080318     c                   endif
038400080318     c                   if        bltfvc='P' and bltvuc>0
038500080318     c                   add       bltvuc        totvuc
038600080318     c                   add       1             wncr
038700080318     c                   endif
038800080318     c                   enddo
038900080318     C                   Z-ADD(H)  totvuc        wvlc
039000080318     C                   ENDSR
039100000000     C*------------------------------------------------------------------------*
039200090709     C* AZZWFV - AZZERA CAMPI IN WFVLMF0F
039300000000     C*------------------------------------------------------------------------*
039400991029     C     AZZWFV        BEGSR
039500000000     C*
039600991029     C                   Z-ADD     *ZEROS        WFVTCL
039700991029     C                   Z-ADD     *ZEROS        WFVVLC
039800991029     C                   Z-ADD     *ZEROS        WFVNCL
039900991029     C                   Z-ADD     *ZEROS        WFVNCR
040000991029     C                   Z-ADD     *ZEROS        WFVNSP
040100991029     C                   Z-ADD     *ZEROS        WFVVLB
040200991029     C                   Z-ADD     *ZEROS        WFVPKB
040300000000     C*
040400000000     C                   ENDSR
040500991029     C*------------------------------------------------------------------------*
040600991029     C* DECCLI - DECODIFICA CLIENTE
040700991029     C*------------------------------------------------------------------------*
040800991029     C     DECCLI        BEGSR
040900991029     C*
041000000216     C                   Z-ADD     WCLI          DSCLI
041100991029     C                   CLEAR                   BS69DS
041200991029     C                   CLEAR                   ACODS
041300991029     C                   MOVEL     KNSIF         I69SIF
041400991029     C                   Z-ADD     WCLI          I69KAC
041500090709     C                   Z-ADD     WCLI          I69Kcp
041600991029     C                   CALL      'TIBS69R'
041700991029     C                   PARM                    BS69DS
041800991029     C                   PARM                    ACODS
041900991029     C                   PARM                    INDDS
042000991029     C                   PARM                    CLPDS
042100991029     C                   PARM                    CLSDS
042200991029     C     O69ERR        IFNE      '1'
042300991029     C                   MOVEL     ACORAG        WFVRAG
042400090709     C                   MOVEL     clpage        W_clpage
042500991029     C                   ELSE
042600991029     C                   MOVEL     *ALL'*'       WFVRAG
042700090709     c                   clear                   w_clpage
042800991029     C                   ENDIF
042900090709IF  1C     DSCOD         IFEQ      8888
043000090709     C                   MOVEL     DEC(1)        WFVRAG
043100000216E   1C                   ENDIF
043200090709     c* reperimento codice commerciale unificante
043300140611     C     w_CLPage      chain     AZCMM000
043400140611     c                   if        %found(AZCMM01L)
043500140611     C                   z-add     CMMuni        WFVCUN
043600090709     c                   else
043700090709     c                   clear                   wfVcun
043800090709     c                   endif
043900991029     C*
044000991029     C                   ENDSR
044100000000     C*------------------------------------------------------------------------*
044200000000     C* DIVARE - RECUPERA AREA E DIVISIONE
044300000000     C*------------------------------------------------------------------------*
044400000000     C     DIVARE        BEGSR
044500000000     C*
044600000000     C                   Z-ADD     1             I
044700000000     C     WFIL          LOOKUP    FIL(I)                                 99
044800000000     C     *IN99         IFEQ      '0'
044900000000     C                   MOVEL     '*'           WDIV              1
045000000000     C                   Z-ADD     999           WARE              3 0
045100000000     C                   ELSE
045200000000     C                   MOVEL     DIV(I)        WDIV
045300000000     C                   Z-ADD     ARE(I)        WARE
045400000000     C                   END
045500000000     C*
045600000000     C                   ENDSR
045700000000     C*------------------------------------------------------------------------*
045800000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
045900000000     C*------------------------------------------------------------------------*
046000000000     C     CARORG        BEGSR
046100000000     C*
046200000000     C                   Z-ADD     *ZEROS        KORFIL
046300000000     C     KEYORG        SETLL     AZORG01L                           99
046400000000     C  N99              READ      AZORG01L                               99
046500000000     C                   Z-ADD     0             I                 3 0
046600000000     C     *IN99         DOWEQ     '0'
046700000000     C     ORGFVA        IFEQ      *BLANKS
046800000000     C     ORGFAG        IFNE      'V'
046900000000     C                   ADD       1             I
047000000000     C                   Z-ADD     ORGFIL        FIL(I)
047100000000     C                   Z-ADD     ORGCAR        ARE(I)
047200000000     C                   MOVEL     ORGFL3        DIV(I)
047300000000     C                   MOVEL     ORGDIT        DIT(I)
047400000000     C                   END
047500000000     C                   END
047600000000     C                   READ      AZORG01L                               99
047700000000     C                   END
047800000000     C*
047900000000     C                   ENDSR
048000000000     C*------------------------------------------------------------------------*
048100000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
048200000000     C*------------------------------------------------------------------------*
048300000000     C     CARTBL        BEGSR
048400000000     C*--------------------
048500000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
048600000000     C*--------------------
048700000000     C                   Z-ADD     0             I
048800000000     C                   Z-ADD     CODUT         KTBUTE
048900000000     C                   MOVEL     '2W'          KTBCOD
049000000000     C                   MOVEL     *BLANKS       KTBKEY
049100000000     C     KEYTAB        SETLL     TABEL00F
049200000000     C                   READ      TABEL00F                               99
049300000000     C     *IN99         DOWEQ     *OFF
049400000000     C     TBLCOD        ANDEQ     '2W'
049500000000     C     TBLFLG        IFEQ      *BLANKS
049600000000     C                   ADD       1             I
049700000000     C                   MOVEL     TBLKEY        RBL(I)
049800000000     C                   MOVEL     TBLUNI        DS2W
049900000000     C                   MOVEL     §2WFSP        FSP(I)
050000000000     C                   MOVEL     §2WFRI        FRI(I)
050100000000     C                   END
050200000000     C                   READ      TABEL00F                               99
050300000000     C                   END
050400000000     C*--------------------
050500000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
050600000000     C*--------------------
050700000000     C                   Z-ADD     0             I
050800000000     C                   Z-ADD     CODUT         KTBUTE
050900000000     C                   MOVEL     'TB'          KTBCOD
051000000000     C                   MOVEL     *BLANKS       KTBKEY
051100000000     C     KEYTAB        SETLL     TABEL00F
051200000000     C                   READ      TABEL00F                               99
051300000000     C     *IN99         DOWEQ     *OFF
051400000000     C     TBLCOD        ANDEQ     'TB'
051500000000     C     TBLFLG        IFEQ      *BLANKS
051600000000     C                   ADD       1             I
051700000000     C                   MOVEL     TBLKEY        TB(I)
051800000000     C                   MOVEL     TBLUNI        DSTB
051900000000     C                   Z-ADD     1             J                 4 0
052000000000     C                   SETOFF                                       28
052100000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
052200000000     C     *IN28         IFEQ      *ON
052300000000     C                   MOVEL     FSP(J)        SP(I)
052400000000     C                   MOVEL     FRI(J)        RI(I)
052500000000     C                   ELSE
052600000000     C                   MOVEL     'N'           SP(I)
052700000000     C                   MOVEL     'N'           RI(I)
052800000000     C                   ENDIF
052900000000     C                   END
053000000000     C                   READ      TABEL00F                               99
053100000000     C                   END
053200080318     C*--------------------
053300080318     C* codice bolla e relativo tipo bolla
053400080318     C*--------------------
053500080318     C                   Z-ADD     0             I
053600080318     C                   Z-ADD     CODUT         KTBUTE
053700080318     C                   MOVEL     '3A'          KTBCOD
053800080318     C                   MOVEL     *BLANKS       KTBKEY
053900080318     C     KEYTAB        SETLL     TABEL00F
054000080318     C                   READ      TABEL00F                               99
054100080318     C     *IN99         DOWEQ     *OFF
054200080318     C     TBLCOD        ANDEQ     '3A'
054300080318     C     TBLFLG        IFEQ      *BLANKS
054400080318     C                   ADD       1             I
054500080318     C                   MOVEL     TBLKEY        cbo(I)
054600080318     C                   MOVEL     TBLUNI        DS3A
054700080318     C                   MOVEL     §3atb1        tbl(I)
054800080318     C                   END
054900080318     C                   READ      TABEL00F                               99
055000080318     C                   END
055100000000     C*
055200000000     C                   ENDSR
055300991029     C*------------------------------------------------------------------------*
055400991029     C* FINSR - OPERAZIONI FINALI
055500991029     C*------------------------------------------------------------------------*
055600991029     C     FINSR         BEGSR
055700991029     C*
055800991029     C* CHIUDE I FILE DEI PGM CHIAMATI
055900991029     C                   CLEAR                   BS69DS
056000991029     C                   CLEAR                   ACODS
056100991029     C                   CLEAR                   INDDS
056200991029     C                   CLEAR                   CLPDS
056300991029     C                   CLEAR                   CLSDS
056400991029     C                   MOVEL     'C'           I69TLA
056500991029     C                   MOVEL     KNSIF         I69SIF
056600991029     C                   CALL      'TIBS69R'
056700991029     C                   PARM                    BS69DS
056800991029     C                   PARM                    ACODS
056900991029     C                   PARM                    INDDS
057000991029     C                   PARM                    CLPDS
057100991029     C                   PARM                    CLSDS
057200991029     C*
057300991029     C                   ENDSR
057400000000     C*------------------------------------------------------------------------*
057500000000     C* *INZSR - ROUTINE INIZIALE
057600000000     C*------------------------------------------------------------------------*
057700000000     C     *INZSR        BEGSR
057800000000     C*--------------------
057900000000     C* RICEVIMENTO PARAMETRI
058000000000     C*--------------------
058100000000     C     *ENTRY        PLIST
058200000000     C                   PARM                    KPJBA
058300000000     C                   MOVEL     KPJBU         PARAM
058400000000     C*--------------------
058500000000     C* CAMPI RIFERITI AL DATA BASE
058600000000     C*--------------------
058700080318     C***  *LIKE         DEFINE    TASAAS        KTAAAS
058800080318     C***  *LIKE         DEFINE    TASMGS        KTAMGS
058900080318     C***  *LIKE         DEFINE    TASKSC        KTAKSC
059000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
059100000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
059200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
059300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
059400991029     C     *LIKE         DEFINE    WFVANN        KWVANN
059500991029     C     *LIKE         DEFINE    WFVDIV        KWVDIV
059600991029     C     *LIKE         DEFINE    WFVARE        KWVARE
059700991029     C     *LIKE         DEFINE    WFVFIL        KWVFIL
059800991029     C     *LIKE         DEFINE    WFVRAG        KWVRAG
059900991029     C     *LIKE         DEFINE    WFVKSC        KWVKSC
060000991029     C     *LIKE         DEFINE    WFVCTR        KWVCTR
060100991029     C     *LIKE         DEFINE    WFVMES        KWVMES
060200991029     C     *LIKE         DEFINE    WFVFVB        KWVFVB
060300080318     C**** *LIKE         DEFINE    TASKSC        WCLI
060400080318     C***  *LIKE         DEFINE    TASCTR        WCTR
060500080318     C     *LIKE         DEFINE    blpKSC        WCLI
060600080318     C     *LIKE         DEFINE    blpCTR        WCTR
060700080318     C***  *LIKE         DEFINE    TASVLC        WVLC
060800000000     C*--------------------
060900000000     C* CHIAVI DI LETTURA
061000000000     C*--------------------
061100000000     C* CHIAVE LETTURA TITAS38C - COMPLETA
061200080318     C***  KEYTAS        KLIST
061300080318     C***                KFLD                    KTAAAS
061400080318     C***                KFLD                    KTAMGS
061500080318     C***                KFLD                    KTAKSC
061600000000     C*
061700000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
061800000000     C     KEYORG        KLIST
061900000000     C                   KFLD                    KORFIL
062000000000     C*
062100000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
062200000000     C     KEYTAB        KLIST
062300000000     C                   KFLD                    KTBUTE
062400000000     C                   KFLD                    KTBCOD
062500000000     C                   KFLD                    KTBKEY
062600000000     C*
062700090709     C* CHIAVE LETTURA WFVLMF1L - COMPLETA
062800991029     C     KEYWFV        KLIST
062900991029     C                   KFLD                    KWVANN
063000991029     C                   KFLD                    KWVDIV
063100991029     C                   KFLD                    KWVARE
063200991029     C                   KFLD                    KWVFIL
063300991029     C                   KFLD                    KWVRAG
063400991029     C                   KFLD                    KWVKSC
063500991029     C                   KFLD                    KWVCTR
063600991029     C                   KFLD                    KWVMES
063700991029     C                   KFLD                    KWVFVB
063800080318     C* CHIAVE LETTURA fnblt01l
063900080318     C     kblt          KLIST
064000080318     C                   KFLD                    blpaas
064100080318     C                   KFLD                    blplnp
064200080318     C                   KFLD                    blpnrs
064300080318     C                   KFLD                    blpnsp
064400000000     C*--------------------
064500000000     C* CAMPI DI CONTROLLO
064600000000     C*--------------------
064700000000     C                   MOVEL     '0'           FINL00            1
064800000000     C                   Z-ADD     1             CODUT             1 0
064900991025     C                   Z-ADD     *ZEROS        MEMCLI            7 0
065000991025     C                   Z-ADD     *ZEROS        MEMCTR            3 0
065100000000     C*
065200000000     C***
065300000000     C* LANCIO PER SALDI
065400000000     C***
065500000000     C* CARICA ORGANIGRAMMA
065600000000     C                   EXSR      CARORG
065700000000     C*
065800000000     C* CARICA TIPI BOLLA
065900000000     C                   EXSR      CARTBL
066000080318     c* recupera divisione e area del terminale di partenza per memo in
066100080318     c* file di work
066200080318     C                   Z-ADD     partfp        WFIL              3 0
066300080318     C                   EXSR      DIVARE
066400080318     C                   MOVEL     WDIV          WDIV              1
066500080318     C                   Z-ADD     WARE          WARE              3 0
066600000000     C*
066700000000     C                   ENDSR
066800000216**   DEC - DECODIFICHE
066900000216MITTENTE NON CODIFICATO                 1
0670000002162
0671000002163
0672000002164
0673000002165
