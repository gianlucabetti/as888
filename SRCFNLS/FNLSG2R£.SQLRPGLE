000100000000     H*------------------------------------------------------------------------*
000200991029     H* STATISTICA VOLUMI CML - GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500080318     Ffnblt01l  IF   E           K DISK
000600000000     FAZORG01L  IF   E           K DISK
000700000000     FTABEL00F  IF   E           K DISK
000800090709     FWFVLMF1L  UF A E           K DISK
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200000216     D*------------------
001300000216     D* DECODIFICA
001400000216     D*------------------
001500000216     D DEC             S             40    DIM(5) CTDATA PERRCD(1)
001600000000     D*-------------------
001700000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001800000000     D*-------------------
001900000000     D FIL             S              3  0 DIM(500)
002000000000     D ARE             S              3  0 DIM(500)
002100000000     D DIV             S              1    DIM(500)
002200000000     D DIT             S              3    DIM(500)
002300000000     D*-------------------
002400000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
002500000000     D*-------------------
002600000000     D RBL             S              1    DIM(5)
002700000000     D FSP             S              1    DIM(5)
002800000000     D FRI             S              1    DIM(5)
002900000000     D*-------------------
003000000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003100000000     D*-------------------
003200000000     D TB              S              2    DIM(20)
003300000000     D SP              S              1    DIM(20)
003400000000     D RI              S              1    DIM(20)
003500080318     D cbo             S              2    DIM(99)
003600080318     D tbl             S              2    DIM(99)
003700000000     D*------------------------------------------------------------------------*
003800000000     D* INPUT
003900000000     D*------------------------------------------------------------------------*
004000000000     D*-------------------
004100000000     D* ARCHITETTURA
004200000000     D*-------------------
004300000000     D KPJBA         E DS
004400080318     D*-------------------
004500080318     D fnblpds       E DS                  EXTNAME(fnblp00f)
004600000000     D*-------------------
004700000000     D* DS TIPO BOLLA
004800000000     D*-------------------
004900000000     D DSTB          E DS
004901090709     D*-------------------
004902090709     D* DS agenti commerciali
004903090709     D*-------------------
004904090709     D DS01          E DS
005000000000     D*-------------------
005100000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
005200000000     D*-------------------
005300000000     D DS2W          E DS
005400080318     D DS3a          E DS
005500991029     D*------------------
005600991029     D* DS REPERIMENTO DATI UTENTE
005700991029     D*-------------------
005800991029     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
005900991029     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
006000991029     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
006100991029     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
006200991029     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
006300000000     D*-------------------
006400000000     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
006500000000     D*-------------------
006600991028     D                 DS                  INZ
006700991028     D  MM                     1      2  0
006800991028     D  MMGG                   1      4  0
006900000000     D                 DS                  INZ
007000000000     D  DIANN                  1      4  0
007100000000     D  DIMES                  5      6  0
007200000000     D  DIMGS                  5      8  0
007300000000     D  DIGIO                  7      8  0
007400000000     D  DIAAM                  1      6  0
007500000000     D                 DS                  INZ
007600000000     D  DFANN                  1      4  0
007700000000     D  DFMES                  5      6  0
007800000000     D  DFMGS                  5      8  0
007900000000     D  DFGIO                  7      8  0
008000000000     D  DFAAM                  1      6  0
008100000000     D*-------------------
008200000000     D* DS SCOMPOSIZIONE CODICE CLIENTE
008300000000     D*-------------------
008400000000     D                 DS
008500000000     D  DSCLI                  1      7  0
008600000000     D  DSFIL                  1      3  0
008700000000     D  DSCOD                  4      7  0
008800000000     D*-------------------
008900000000     D* DS PARAMETRI DI LANCIO PGM
009000000000     D*-------------------
009100000000     D PARAM           DS
009200991029     D  PARTLA                 1      1
009300080318     D**PARANN                 2      5  0
009400080318     D**PARMMI                 6      7  0
009500080318     D**PARMMF                 8      9  0
009600080318     d  pardal                 2      9  0
009700080318     D**PARSNR                10     10
009800991029     D  PARPID                27     31  2
009900080318     D**PARDIV                32     32
010000080318     D**PARARE                33     35  0
010100080318     D**PARFIL                36     38  0
010200080318     D  PARtfp                36     38  0
010300991029     D  PARDET                39     39
010400080318     D**PARTOT                40     40
010500991029     D  PARRG1                41     44  1
010600991029     D  PARRG2                45     48  1
010700080318     D**PARPIA                49     53  2
010800991029     D  PARPSD                54     58  2
010900080318     D**PARPSA                59     63  2
011000080318     D**PAROK                 65     66
011100991229     D  PARMIN                67     71  3
011200080318     D**PARPOS                72     72
011300080318     D**PARNT1                73     75
011400080318     D**PARNT2                76     78
011500080318     D**PARNT3                79     81
011600080318     D**PARNT4                82     84
011700080318     D**PARSAL                85     85
011800080318     d  paral                 86     93  0
011801090709     d  parord                75     75
011802090709     d  parpag                76     76
011803090709     d  parflp                77     79  0
011900080318
012000080318     d totvuc                         8  6
012100080318     d wvlc            s                   like(blpvlc)
012200080318     d wncr            s                   like(blpncr)
012201090709      *
012202090709     d w_clpage        s                   like(clpage)
012300000000     C*------------------------------------------------------------------------*
012400000000     C* MAIN LINE
012500000000     C*------------------------------------------------------------------------*
012600000000     C*
012700080318     c/exec sql
012800080318     c+ set option dynusrprf = *owner, closqlcsr = *endmod
012900080318     c/end-exec
013000080318
013001090709     c                   if        parflp=0
013100080318     C/EXEC SQL
013200080318     C+ Declare A1 Cursor for
013300080318     c+ SELECT * FROM fnblp00f
013400080318     c+ WHERE blptfp = :partfp AND blpaas*10000+blpmgs
013500080318     c+ between :PARdal and :paral
013600080318     C/END-EXEC
013700080318      *          apertura cursore
013800080318     C/EXEC SQL
013900080318     C+ OPEN A1
014000080318     C/END-EXEC
014001090709     c                   else
014002090709     C/EXEC SQL
014003090709     C+ Declare B1 Cursor for
014004090709     c+ SELECT * FROM fnblp00f
014005090709     c+ WHERE blptfp = :partfp AND blplnp= :parflp AND blpaas*10000+blpmgs
014006090709     c+ between :PARdal and :paral
014007090709     C/END-EXEC
014008090709      *          apertura cursore
014009090709     C/EXEC SQL
014010090709     C+ OPEN B1
014011090709     C/END-EXEC
014012090709     c                   endif
014100080318     C                   DOU       SqlCod <> 0
014101090709     c                   if        parflp=0
014200080318      *          lettura cursore
014300080318     C/EXEC SQL
014400080318     C+ Fetch Next From A1 Into :fnblpds
014500080318     C/END-EXEC
014501090709     c                   else
014502090709      *          lettura cursore
014503090709     C/EXEC SQL
014504090709     C+ Fetch Next From B1 Into :fnblpds
014505090709     C/END-EXEC
014506090709     c                   endif
014600080318     C                   SELECT
014700080318     C                   WHEN      SqlCod = 100
014800080318     c                   leave
014900080318      *
015000080318     C                   WHEN      SqlCod < 0
015100080318     c                   leave
015200080318      *
015300080318     C                   OTHER
015400080318      * legge e scrive
015500080318     c                   exsr      chkrec
015600080318     c                   if        wrecok='S'
015700080318     C                   EXSR      CREARE
015800080318     c                   endif
015900080318      *
016000080318     C                   ENDSL
016100080318      *
016200080318     C                   ENDDO
016201090709     c                   if        parflp=0
016300080318     C/EXEC SQL
016400080318     C+ Close A1
016500080318     C/END-EXEC
016501090709     c                   else
016502090709     C/EXEC SQL
016503090709     C+ Close B1
016504090709     C/END-EXEC
016505090709     c                   endif
016600991029     C*
016700991029     C* OPERAZIONI FINALI
016800991029     C                   EXSR      FINSR
016900000000     C*
017000000000     C                   SETON                                        LR
017100000000     C*------------------------------------------------------------------------*
017200000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
017300000000     C*------------------------------------------------------------------------*
017400000000     C     CHKREC        BEGSR
017500000000     C*
017600000000     C                   MOVEL     'S'           WRECOK            1
017700001011     C*
017800001011     C* NUOVO CONTROLLO ==> SE IL CAMPO TASNCR = 99999 ==> ESCLUDO COMPLETAMENTE
017900080318     C***  WRECOK        IFEQ      'S'
018000080318     C***  blpncr        IFEQ      99999
018100080318     C***                MOVEL     'N'           WRECOK
018200080318     C***                ENDIF
018300080318     C***                ENDIF
018400000000     C*
018500000000     C* ESCLUDE LE BOLLE NON VALIDE COME "SPEDIZIONE"
018600000000     C     WRECOK        IFEQ      'S'
018700000000     C                   Z-ADD     1             J
018800080318     C                   Z-ADD     1             k                 4 0
018900000000     C                   SETOFF                                           28
019000080318     c     blpcbo        lookup    cbo(J)                                 28
019100080318     C     tbl(J)        LOOKUP    TB(K)                                  28
019200080318     C     SP(K)         IFEQ      'N'
019300000000     C                   MOVEL     'N'           WRECOK
019400000000     C                   ENDIF
019500000000     C                   ENDIF
019600000000     C*
019700000000     C* SE E' ASSEGNATO PRENDE CLIENTE MITTENTE, SE FRANCO CLIENTE DESTINO
019800000000     C     WRECOK        IFEQ      'S'
019900000000     C                   Z-ADD     *ZEROS        WCLI
020000000000     C                   Z-ADD     *ZEROS        WCTR
020100080318     C                   MOVEL     Tbl(J)        A1                1
020200000000     C     A1            IFEQ      'A'
020300080318     C     blpccm        IFEQ      *ZEROS
020400000000     C                   MOVEL     'N'           WRECOK
020500000000     C                   ELSE
020600080318     C                   Z-ADD     blpCCM        WCLI
020700000000     C                   Z-ADD     999           WCTR
020800000000     C                   ENDIF
020900000000     C                   ELSE
021000080318     C                   Z-ADD     blpKSC        WCLI
021100080318     C                   MOVE      blpKSC        N4                4 0
021200000000     C     N4            IFEQ      8888
021300000000     C                   Z-ADD     888           WCTR
021400000000     C                   ELSE
021500080318     C                   Z-ADD     blpctr        WCTR
021600000000     C                   ENDIF
021700000000     C                   ENDIF
021800991029     C                   ENDIF
021900991029     C*
022000991029     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE GIA' CONVERTITO
022100991029     C                   EXSR      DECCLI
022200000000     C*
022300000000     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
022400000000     C     WRECOK        IFEQ      'S'
022500000000     C                   Z-ADD     WCLI          DSCLI
022600000000     C                   Z-ADD     DSFIL         WFIL              3 0
022700080318     C***                EXSR      DIVARE
022800080318     C***                MOVEL     WDIV          WDIV              1
022900080318     C***                Z-ADD     WARE          WARE              3 0
023000000000     C                   ENDIF
023100000000     C*
023200080318     C**   WRECOK        IFEQ      'S'
023300000000     C*
023400000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
023500000000     C*
023600080318     C**   PARDIV        IFNE      *BLANKS
023700080318     C**   WDIV          IFEQ      PARDIV
023800080318     C**                 ELSE
023900080318     C**                 MOVEL     'N'           WRECOK
024000080318     C**                 ENDIF
024100080318     C**                 ENDIF
024200000000     C*
024300000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
024400080318     C**   PARARE        IFNE      *ZEROS
024500080318     C**   WARE          IFEQ      PARARE
024600080318     C**                 ELSE
024700080318     C**                 MOVEL     'N'           WRECOK
024800080318     C**                 ENDIF
024900080318     C**                 ENDIF
025000000000     C*
025100000000     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
025200080318     C**   PARFIL        IFNE      *ZEROS
025300080318     C***  TASLNP        IFNE      PARFIL
025400080318     C***                MOVEL     'N'           WRECOK
025500080318     C***                ENDIF
025600080318     C***                ENDIF
025700080318     C***                ENDIF
025800000000     C*
025900000000     C                   ENDSR
026000000000     C*------------------------------------------------------------------------*
026100000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
026200000000     C*------------------------------------------------------------------------*
026300000000     C     CREARE        BEGSR
026400991029     C*
026500991029     C* IMPOSTA IL FLAG TIPO VOLUME (REALE O NON REALE)
026600040211IF  1C*--- TASFVB        IFNE      'R'
026700040211     C*---               MOVEL     *BLANKS       TASFVB
026800040211E   1C*---               ENDIF
026900991025     C*
027000080318     C                   clear                   KWVANN
027100991029     C                   MOVEL     WDIV          KWVDIV
027200991029     C                   Z-ADD     WARE          KWVARE
027300080318     C                   Z-ADD     partfp        KWVFIL
027400991029     C                   MOVEL     WFVRAG        KWVRAG
027500991029     C                   Z-ADD     WCLI          KWVKSC
027600991029     C                   Z-ADD     WCTR          KWVCTR
027700080318     C                   clear                   KWVMES
027800040211     C*---               MOVEL     TASFVB        KWVFVB
027900040211     C                   MOVEL     *BLANKS       KWVFVB
028000000000     C*
028100000000     C* SCRIVO O RISCRIVO FILE DI LAVORO PER STAMPA STATISTICA PESI
028200000000     C*
028300090709     C     KEYWFV        CHAIN     WFVLMF1L                           99
028400000000     C*
028500000000     C* RIEMPIO BUFFER RECORD
028600000000     C*
028700991029     C                   Z-ADD     KWVANN        WFVANN
028800991029     C                   MOVEL     KWVDIV        WFVDIV
028900991029     C                   Z-ADD     KWVARE        WFVARE
029000991029     C                   Z-ADD     KWVFIL        WFVFIL
029100991029     C                   Z-ADD     KWVKSC        WFVKSC
029200991029     C                   Z-ADD     KWVCTR        WFVCTR
029300991029     C                   Z-ADD     KWVMES        WFVMES
029400991029     C                   MOVEL     KWVFVB        WFVFVB
029500991029     C   99              EXSR      AZZWFV
029600991029     C                   EXSR      ADDWFV
029700090709     C   99              WRITE     WFVOLF
029800090709     C  N99              UPDATE    WFVOLF
029900991025     C*
030000000000     C                   ENDSR
030100000000     C*------------------------------------------------------------------------*
030200090709     C* ADDWFV - AGGIUNGE LA SPEDIZIONE AL RECORD WFVLMF0F
030300000000     C*------------------------------------------------------------------------*
030400991029     C     ADDWFV        BEGSR
030500991029     C*
030600991029     C                   MOVEL     'S'           ESEGUI            1
030700991025     C*
030800080318     C                   ADD       blpNCL        WFVTCL
030900080318     c                   exsr      sr_blt
031000991104     C*
031100080318     C**   TASFVF        IFEQ      'T'
031200080318     C**                 Z-ADD     TASVLF        WVLC
031300080318     C**                 ELSE
031400080318     C     blpncl        IFEQ      wncr
031500080318     C     wvlc          IFeq      *ZEROS
031600991104     C                   MOVEL     'N'           ESEGUI
031700991104     C                   ENDIF
031800991104     C                   ELSE
031900080318     C     wvlc          IFle      blpvlb
032000991104     C                   MOVEL     'N'           ESEGUI
032100991104     C                   ENDIF
032200991104     C                   ENDIF
032300080318     C***                ENDIF
032400991104     C*
032500991029     C     ESEGUI        IFEQ      'S'
032600991029     C                   ADD       WVLC          WFVVLC
032700080318     C                   ADD       blpVLB        WFVVLB
032800080318     C                   ADD       blpNCL        WFVNCL
032900080318     C                   ADD       wncr          WFVNCR
033000080318     C                   ADD       blppkb        WFVPKB
033100991029     C                   ADD       1             WFVNSP
033200991026     C                   ENDIF
033300991025     C*
033400000000     C                   ENDSR
033500080318     C*------------------------------------------------------------------------*
033600080318     C* sr_blt - legge e totalizza volume vdl rilevato in partenza
033700080318     C*------------------------------------------------------------------------*
033800080318     C     sr_blt        BEGSR
033900080318     c                   clear                   totvuc
034000080318     c                   clear                   wvlc
034100080318     c                   clear                   wncr
034200080318     c     kblt          setll     fnblt01l
034300080318     c                   do        *hival
034400080318     c     kblt          reade     fnblt01l
034500080318     c                   if         %eof(fnblt01l)
034600080318     c                   leave
034700080318     c                   endif
034800080318     c                   if        bltfvc='P' and bltvuc>0
034900080318     c                   add       bltvuc        totvuc
035000080318     c                   add       1             wncr
035100080318     c                   endif
035200080318     c                   enddo
035300080318     C                   Z-ADD(H)  totvuc        wvlc
035400080318     C                   ENDSR
035500000000     C*------------------------------------------------------------------------*
035600090709     C* AZZWFV - AZZERA CAMPI IN WFVLMF0F
035700000000     C*------------------------------------------------------------------------*
035800991029     C     AZZWFV        BEGSR
035900000000     C*
036000991029     C                   Z-ADD     *ZEROS        WFVTCL
036100991029     C                   Z-ADD     *ZEROS        WFVVLC
036200991029     C                   Z-ADD     *ZEROS        WFVNCL
036300991029     C                   Z-ADD     *ZEROS        WFVNCR
036400991029     C                   Z-ADD     *ZEROS        WFVNSP
036500991029     C                   Z-ADD     *ZEROS        WFVVLB
036600991029     C                   Z-ADD     *ZEROS        WFVPKB
036700000000     C*
036800000000     C                   ENDSR
036900991029     C*------------------------------------------------------------------------*
037000991029     C* DECCLI - DECODIFICA CLIENTE
037100991029     C*------------------------------------------------------------------------*
037200991029     C     DECCLI        BEGSR
037300991029     C*
037400000216     C                   Z-ADD     WCLI          DSCLI
037800991029     C                   CLEAR                   BS69DS
037900991029     C                   CLEAR                   ACODS
038000991029     C                   MOVEL     KNSIF         I69SIF
038100991029     C                   Z-ADD     WCLI          I69KAC
038101090709     C                   Z-ADD     WCLI          I69Kcp
038200991029     C                   CALL      'TIBS69R'
038300991029     C                   PARM                    BS69DS
038400991029     C                   PARM                    ACODS
038500991029     C                   PARM                    INDDS
038600991029     C                   PARM                    CLPDS
038700991029     C                   PARM                    CLSDS
038800991029     C     O69ERR        IFNE      '1'
038900991029     C                   MOVEL     ACORAG        WFVRAG
038901090709     C                   MOVEL     clpage        W_clpage
039000991029     C                   ELSE
039100991029     C                   MOVEL     *ALL'*'       WFVRAG
039101090709     c                   clear                   w_clpage
039200991029     C                   ENDIF
039201090709IF  1C     DSCOD         IFEQ      8888
039202090709     C                   MOVEL     DEC(1)        WFVRAG
039300000216E   1C                   ENDIF
039301090709     c* reperimento codice commerciale unificante
039302090709     c                   clear                   ds01
039303090709     c                   z-add     codut         ktbute
039304090709     C                   MOVEL     '01'          ktbcod
039305090709     C                   MOVEL(P)  W_clpage      Ktbkey
039306090709     C     keytab        CHAIN     TABEL00F
039307090709     c                   if        %found(tabel00f)
039308090709     C                   MOVEL     TBLUNI        ds01
039309090709     C                   MOVEL     §01rgf        WFVCUN
039310090709     c                   else
039311090709     c                   clear                   wfVcun
039312090709     c                   endif
039400991029     C*
039500991029     C                   ENDSR
039600000000     C*------------------------------------------------------------------------*
039700000000     C* DIVARE - RECUPERA AREA E DIVISIONE
039800000000     C*------------------------------------------------------------------------*
039900000000     C     DIVARE        BEGSR
040000000000     C*
040100000000     C                   Z-ADD     1             I
040200000000     C     WFIL          LOOKUP    FIL(I)                                 99
040300000000     C     *IN99         IFEQ      '0'
040400000000     C                   MOVEL     '*'           WDIV              1
040500000000     C                   Z-ADD     999           WARE              3 0
040600000000     C                   ELSE
040700000000     C                   MOVEL     DIV(I)        WDIV
040800000000     C                   Z-ADD     ARE(I)        WARE
040900000000     C                   END
041000000000     C*
041100000000     C                   ENDSR
041200000000     C*------------------------------------------------------------------------*
041300000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
041400000000     C*------------------------------------------------------------------------*
041500000000     C     CARORG        BEGSR
041600000000     C*
041700000000     C                   Z-ADD     *ZEROS        KORFIL
041800000000     C     KEYORG        SETLL     AZORG01L                           99
041900000000     C  N99              READ      AZORG01L                               99
042000000000     C                   Z-ADD     0             I                 3 0
042100000000     C     *IN99         DOWEQ     '0'
042200000000     C     ORGFVA        IFEQ      *BLANKS
042300000000     C     ORGFAG        IFNE      'V'
042400000000     C                   ADD       1             I
042500000000     C                   Z-ADD     ORGFIL        FIL(I)
042600000000     C                   Z-ADD     ORGCAR        ARE(I)
042700000000     C                   MOVEL     ORGFL3        DIV(I)
042800000000     C                   MOVEL     ORGDIT        DIT(I)
042900000000     C                   END
043000000000     C                   END
043100000000     C                   READ      AZORG01L                               99
043200000000     C                   END
043300000000     C*
043400000000     C                   ENDSR
043500000000     C*------------------------------------------------------------------------*
043600000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
043700000000     C*------------------------------------------------------------------------*
043800000000     C     CARTBL        BEGSR
043900000000     C*--------------------
044000000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
044100000000     C*--------------------
044200000000     C                   Z-ADD     0             I
044300000000     C                   Z-ADD     CODUT         KTBUTE
044400000000     C                   MOVEL     '2W'          KTBCOD
044500000000     C                   MOVEL     *BLANKS       KTBKEY
044600000000     C     KEYTAB        SETLL     TABEL00F
044700000000     C                   READ      TABEL00F                               99
044800000000     C     *IN99         DOWEQ     *OFF
044900000000     C     TBLCOD        ANDEQ     '2W'
045000000000     C     TBLFLG        IFEQ      *BLANKS
045100000000     C                   ADD       1             I
045200000000     C                   MOVEL     TBLKEY        RBL(I)
045300000000     C                   MOVEL     TBLUNI        DS2W
045400000000     C                   MOVEL     §2WFSP        FSP(I)
045500000000     C                   MOVEL     §2WFRI        FRI(I)
045600000000     C                   END
045700000000     C                   READ      TABEL00F                               99
045800000000     C                   END
045900000000     C*--------------------
046000000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
046100000000     C*--------------------
046200000000     C                   Z-ADD     0             I
046300000000     C                   Z-ADD     CODUT         KTBUTE
046400000000     C                   MOVEL     'TB'          KTBCOD
046500000000     C                   MOVEL     *BLANKS       KTBKEY
046600000000     C     KEYTAB        SETLL     TABEL00F
046700000000     C                   READ      TABEL00F                               99
046800000000     C     *IN99         DOWEQ     *OFF
046900000000     C     TBLCOD        ANDEQ     'TB'
047000000000     C     TBLFLG        IFEQ      *BLANKS
047100000000     C                   ADD       1             I
047200000000     C                   MOVEL     TBLKEY        TB(I)
047300000000     C                   MOVEL     TBLUNI        DSTB
047400000000     C                   Z-ADD     1             J                 4 0
047500000000     C                   SETOFF                                       28
047600000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
047700000000     C     *IN28         IFEQ      *ON
047800000000     C                   MOVEL     FSP(J)        SP(I)
047900000000     C                   MOVEL     FRI(J)        RI(I)
048000000000     C                   ELSE
048100000000     C                   MOVEL     'N'           SP(I)
048200000000     C                   MOVEL     'N'           RI(I)
048300000000     C                   ENDIF
048400000000     C                   END
048500000000     C                   READ      TABEL00F                               99
048600000000     C                   END
048700080318     C*--------------------
048800080318     C* codice bolla e relativo tipo bolla
048900080318     C*--------------------
049000080318     C                   Z-ADD     0             I
049100080318     C                   Z-ADD     CODUT         KTBUTE
049200080318     C                   MOVEL     '3A'          KTBCOD
049300080318     C                   MOVEL     *BLANKS       KTBKEY
049400080318     C     KEYTAB        SETLL     TABEL00F
049500080318     C                   READ      TABEL00F                               99
049600080318     C     *IN99         DOWEQ     *OFF
049700080318     C     TBLCOD        ANDEQ     '3A'
049800080318     C     TBLFLG        IFEQ      *BLANKS
049900080318     C                   ADD       1             I
050000080318     C                   MOVEL     TBLKEY        cbo(I)
050100080318     C                   MOVEL     TBLUNI        DS3A
050200080318     C                   MOVEL     §3atb1        tbl(I)
050300080318     C                   END
050400080318     C                   READ      TABEL00F                               99
050500080318     C                   END
050600000000     C*
050700000000     C                   ENDSR
050800991029     C*------------------------------------------------------------------------*
050900991029     C* FINSR - OPERAZIONI FINALI
051000991029     C*------------------------------------------------------------------------*
051100991029     C     FINSR         BEGSR
051200991029     C*
051300991029     C* CHIUDE I FILE DEI PGM CHIAMATI
051400991029     C                   CLEAR                   BS69DS
051500991029     C                   CLEAR                   ACODS
051600991029     C                   CLEAR                   INDDS
051700991029     C                   CLEAR                   CLPDS
051800991029     C                   CLEAR                   CLSDS
051900991029     C                   MOVEL     'C'           I69TLA
052000991029     C                   MOVEL     KNSIF         I69SIF
052100991029     C                   CALL      'TIBS69R'
052200991029     C                   PARM                    BS69DS
052300991029     C                   PARM                    ACODS
052400991029     C                   PARM                    INDDS
052500991029     C                   PARM                    CLPDS
052600991029     C                   PARM                    CLSDS
052700991029     C*
052800991029     C                   ENDSR
052900000000     C*------------------------------------------------------------------------*
053000000000     C* *INZSR - ROUTINE INIZIALE
053100000000     C*------------------------------------------------------------------------*
053200000000     C     *INZSR        BEGSR
053300000000     C*--------------------
053400000000     C* RICEVIMENTO PARAMETRI
053500000000     C*--------------------
053600000000     C     *ENTRY        PLIST
053700000000     C                   PARM                    KPJBA
053800000000     C                   MOVEL     KPJBU         PARAM
053900000000     C*--------------------
054000000000     C* CAMPI RIFERITI AL DATA BASE
054100000000     C*--------------------
054200080318     C***  *LIKE         DEFINE    TASAAS        KTAAAS
054300080318     C***  *LIKE         DEFINE    TASMGS        KTAMGS
054400080318     C***  *LIKE         DEFINE    TASKSC        KTAKSC
054500000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
054600000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
054700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
054800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
054900991029     C     *LIKE         DEFINE    WFVANN        KWVANN
055000991029     C     *LIKE         DEFINE    WFVDIV        KWVDIV
055100991029     C     *LIKE         DEFINE    WFVARE        KWVARE
055200991029     C     *LIKE         DEFINE    WFVFIL        KWVFIL
055300991029     C     *LIKE         DEFINE    WFVRAG        KWVRAG
055400991029     C     *LIKE         DEFINE    WFVKSC        KWVKSC
055500991029     C     *LIKE         DEFINE    WFVCTR        KWVCTR
055600991029     C     *LIKE         DEFINE    WFVMES        KWVMES
055700991029     C     *LIKE         DEFINE    WFVFVB        KWVFVB
055800080318     C**** *LIKE         DEFINE    TASKSC        WCLI
055900080318     C***  *LIKE         DEFINE    TASCTR        WCTR
056000080318     C     *LIKE         DEFINE    blpKSC        WCLI
056100080318     C     *LIKE         DEFINE    blpCTR        WCTR
056200080318     C***  *LIKE         DEFINE    TASVLC        WVLC
056300000000     C*--------------------
056400000000     C* CHIAVI DI LETTURA
056500000000     C*--------------------
056600000000     C* CHIAVE LETTURA TITAS38C - COMPLETA
056700080318     C***  KEYTAS        KLIST
056800080318     C***                KFLD                    KTAAAS
056900080318     C***                KFLD                    KTAMGS
057000080318     C***                KFLD                    KTAKSC
057100000000     C*
057200000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
057300000000     C     KEYORG        KLIST
057400000000     C                   KFLD                    KORFIL
057500000000     C*
057600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
057700000000     C     KEYTAB        KLIST
057800000000     C                   KFLD                    KTBUTE
057900000000     C                   KFLD                    KTBCOD
058000000000     C                   KFLD                    KTBKEY
058100000000     C*
058200090709     C* CHIAVE LETTURA WFVLMF1L - COMPLETA
058300991029     C     KEYWFV        KLIST
058400991029     C                   KFLD                    KWVANN
058500991029     C                   KFLD                    KWVDIV
058600991029     C                   KFLD                    KWVARE
058700991029     C                   KFLD                    KWVFIL
058800991029     C                   KFLD                    KWVRAG
058900991029     C                   KFLD                    KWVKSC
059000991029     C                   KFLD                    KWVCTR
059100991029     C                   KFLD                    KWVMES
059200991029     C                   KFLD                    KWVFVB
059300080318     C* CHIAVE LETTURA fnblt01l
059400080318     C     kblt          KLIST
059500080318     C                   KFLD                    blpaas
059600080318     C                   KFLD                    blplnp
059700080318     C                   KFLD                    blpnrs
059800080318     C                   KFLD                    blpnsp
059900000000     C*--------------------
060000000000     C* CAMPI DI CONTROLLO
060100000000     C*--------------------
060200000000     C                   MOVEL     '0'           FINL00            1
060300000000     C                   Z-ADD     1             CODUT             1 0
060400991025     C                   Z-ADD     *ZEROS        MEMCLI            7 0
060500991025     C                   Z-ADD     *ZEROS        MEMCTR            3 0
060600000000     C*
060700000000     C***
060800000000     C* LANCIO PER SALDI
060900000000     C***
061000000000     C* CARICA ORGANIGRAMMA
061100000000     C                   EXSR      CARORG
061200000000     C*
061300000000     C* CARICA TIPI BOLLA
061400000000     C                   EXSR      CARTBL
061500080318     c* recupera divisione e area del terminale di partenza per memo in
061600080318     c* file di work
061700080318     C                   Z-ADD     partfp        WFIL              3 0
061800080318     C                   EXSR      DIVARE
061900080318     C                   MOVEL     WDIV          WDIV              1
062000080318     C                   Z-ADD     WARE          WARE              3 0
062100000000     C*
062200000000     C                   ENDSR
062300000216**   DEC - DECODIFICHE
062400000216MITTENTE NON CODIFICATO                 1
0625000002162
0626000002163
0627000002164
0628000002165
