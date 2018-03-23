000100140611     /*PRM  dbgview(*source)
000200140611     /*END
000300000000     H*------------------------------------------------------------------------*
000400000000     H* STATISTICA PESO VDL - GENERA WRK FILE
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700140611     h option(*nodebugio)
000800140611     F*------------------------------------------------------------------------*
000900080317     Ffnblt01l  IF   E           K DISK
001000140611     fAZCMM01L  if   e           k disk
001100000000     FAZORG01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300090707     FWFPESF1L  UF A E           K DISK
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001900000000     D*-------------------
002000000000     D FIL             S              3  0 DIM(500)
002100000000     D ARE             S              3  0 DIM(500)
002200000000     D DIV             S              1    DIM(500)
002300000000     D DIT             S              3    DIM(500)
002400000000     D*-------------------
002500000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
002600000000     D*-------------------
002700000000     D RBL             S              1    DIM(5)
002800000000     D FSP             S              1    DIM(5)
002900000000     D FRI             S              1    DIM(5)
003000000000     D*-------------------
003100000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003200000000     D*-------------------
003300000000     D TB              S              2    DIM(20)
003400000000     D SP              S              1    DIM(20)
003500000000     D RI              S              1    DIM(20)
003600080318     D cbo             S              2    DIM(99)
003700080318     D tbl             S              2    DIM(99)
003800000000     D*------------------------------------------------------------------------*
003900000000     D* INPUT
004000000000     D*------------------------------------------------------------------------*
004100000000     D*-------------------
004200000000     D* ARCHITETTURA
004300000000     D*-------------------
004400000000     D KPJBA         E DS
004500080317     D*-------------------
004600080317     D fnblpds       E DS                  EXTNAME(fnblp00f)
004700000000     D*-------------------
004800000000     D* DS TIPO BOLLA
004900000000     D*-------------------
005000000000     D DSTB          E DS
005100000000     D*-------------------
005200000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
005300000000     D*-------------------
005400000000     D DS2W          E DS
005500080318     D DS3a          E DS
005600040220     D*------------------
005700040220     D* DS REPERIMENTO DATI UTENTE
005800040220     D*-------------------
005900040220     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
006000040220     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
006100040220     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
006200040220     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
006300040220     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
006400000000     D*-------------------
006500000000     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
006600000000     D*-------------------
006700991028     D                 DS                  INZ
006800991028     D  MM                     1      2  0
006900991028     D  MMGG                   1      4  0
007000000000     D                 DS                  INZ
007100000000     D  DIANN                  1      4  0
007200000000     D  DIMES                  5      6  0
007300000000     D  DIMGS                  5      8  0
007400000000     D  DIGIO                  7      8  0
007500000000     D  DIAAM                  1      6  0
007600991213     D  DIDAT                  1      8  0
007700000000     D                 DS                  INZ
007800000000     D  DFANN                  1      4  0
007900000000     D  DFMES                  5      6  0
008000000000     D  DFMGS                  5      8  0
008100000000     D  DFGIO                  7      8  0
008200000000     D  DFAAM                  1      6  0
008300000000     D*-------------------
008400000000     D* DS SCOMPOSIZIONE CODICE CLIENTE
008500000000     D*-------------------
008600000000     D                 DS
008700000000     D  DSCLI                  1      7  0
008800000000     D  DSFIL                  1      3  0
008900000000     D  DSCOD                  4      7  0
009000000000     D*-------------------
009100000000     D* DS PARAMETRI DI LANCIO PGM
009200000000     D*-------------------
009300000000     D PARAM           DS
009400000000     D  PARTLA                 1      1
009500080317     D**PARANN                 2      5  0
009600080317     D**PARMMI                 6      7  0
009700080317     D**PARMMF                 8      9  0
009800080317     d  pardal                 2      9  0
009900080317     D**PARSNR                10     10
010000080317     D**PARDTI                11     18  0
010100080317     D**PARDTF                19     26  0
010200991025     D  PARPEI                27     31  2
010300080317     D**PARDIV                32     32
010400080317     D**PARARE                33     35  0
010500080317     D**PARFIL                36     38  0
010600080317     D  PARtfp                36     38  0
010700000000     D  PARDET                39     39
010800080317     D**PARTOT                40     40
010900991026     D  PARRG1                41     44  1
011000991026     D  PARRG2                45     48  1
011100991026     D  PARTAR                49     53  3
011200991026     D  PARPES                54     58  2
011300991229     D  PARMIN                59     63  3
011400080317     D**PARPOS                64     64
011500080320     D**PARNEG                65     65
011600080317     D**PARSAL                66     66
011700080317     d  paral                 67     74  0
011800090707     d  parord                75     75
011900090707     d  parpag                76     76
012000090708     d  parflp                77     79  0
012100080317     d
012200080318     d totpuc                         8  3
012300080318     d wpkc            s                   like(blppkc)
012400080318     d wncp            s                   like(blpncp)
012500090707      *
012600090707     d w_clpage        s                   like(clpage)
012700000000     C*------------------------------------------------------------------------*
012800000000     C* MAIN LINE
012900000000     C*------------------------------------------------------------------------*
013000080317     c/exec sql
013100080317     c+ set option dynusrprf = *owner, closqlcsr = *endmod
013200080317     c/end-exec
013300080317
013400090707     c                   if        parflp=0
013500080317     C/EXEC SQL
013600080317     C+ Declare A1 Cursor for
013700080317     c+ SELECT * FROM fnblp00f
013800080317     c+ WHERE blptfp = :partfp AND blpaas*10000+blpmgs
013900080317     c+ between :PARdal and :paral
014000080317     C/END-EXEC
014100090708      *          apertura cursore
014200090708     C/EXEC SQL
014300090708     C+ OPEN A1
014400090708     C/END-EXEC
014500090707     c                   else
014600090707     C/EXEC SQL
014700090708     C+ Declare B1 Cursor for
014800090707     c+ SELECT * FROM fnblp00f
014900090707     c+ WHERE blptfp = :partfp AND blplnp= :parflp AND blpaas*10000+blpmgs
015000090707     c+ between :PARdal and :paral
015100090707     C/END-EXEC
015200090708      *          apertura cursore
015300090708     C/EXEC SQL
015400090708     C+ OPEN B1
015500090708     C/END-EXEC
015600090707     c                   endif
015700080317     C                   DOU       SqlCod <> 0
015800090708     c*
015900090708     c                   if        parflp=0
016000080317      *          lettura cursore
016100080317     C/EXEC SQL
016200080317     C+ Fetch Next From A1 Into :fnblpds
016300080317     C/END-EXEC
016400090708     c                   else
016500090708      *          lettura cursore
016600090708     C/EXEC SQL
016700090708     C+ Fetch Next From B1 Into :fnblpds
016800090708     C/END-EXEC
016900090708     c                   endif
017000090708
017100080317     C                   SELECT
017200080317     C                   WHEN      SqlCod = 100
017300080317     c                   leave
017400080317      *
017500080317     C                   WHEN      SqlCod < 0
017600080317     c                   leave
017700080317      *
017800080317     C                   OTHER
017900080317      * legge e scrive
018000080317     c                   exsr      chkrec
018100080317     c                   if        wrecok='S'
018200080317     C                   EXSR      CREARE
018300080317     c                   endif
018400080317      *
018500080317     C                   ENDSL
018600080317      *
018700080317     C                   ENDDO
018800090708     c                   if        parflp=0
018900080317     C/EXEC SQL
019000080317     C+ Close A1
019100080317     C/END-EXEC
019200090708     c                   else
019300090708     C/EXEC SQL
019400090708     C+ Close B1
019500090708     C/END-EXEC
019600090708     c                   endif
019700000000     C*
019800000000     C                   SETON                                        LR
019900000000     C*------------------------------------------------------------------------*
020000000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
020100000000     C*------------------------------------------------------------------------*
020200000000     C     CHKREC        BEGSR
020300000000     C*
020400000000     C                   MOVEL     'S'           WRECOK            1
020500000000     C*
020600080320     C***  blpNCP        IFEQ      99999
020700080320     C***                MOVEL     'N'           WRECOK
020800080320     C***                ENDIF
020900040305     C*
021000000000     C* ESCLUDE LE BOLLE NON VALIDE COME "SPEDIZIONE"
021100000000     C     WRECOK        IFEQ      'S'
021200000000     C                   Z-ADD     1             J
021300080318     C                   Z-ADD     1             k                 4 0
021400000000     C                   SETOFF                                           28
021500080318     c     blpcbo        lookup    cbo(J)                                 28
021600080318     C     tbl(J)        LOOKUP    TB(K)                                  28
021700080318     C     SP(k)         IFEQ      'N'
021800000000     C                   MOVEL     'N'           WRECOK
021900000000     C                   ENDIF
022000000000     C                   ENDIF
022100000000     C*
022200000000     C* SE E' ASSEGNATO PRENDE CLIENTE MITTENTE, SE FRANCO CLIENTE DESTINO
022300000000     C     WRECOK        IFEQ      'S'
022400000000     C                   Z-ADD     *ZEROS        WCLI
022500000000     C                   Z-ADD     *ZEROS        WCTR
022600080318     C                   MOVEL     tbl(j)        A1                1
022700000000     C     A1            IFEQ      'A'
022800080318     C     blpccm        IFEQ      *ZEROS
022900000000     C                   MOVEL     'N'           WRECOK
023000000000     C                   ELSE
023100080318     C                   Z-ADD     blpccm        WCLI
023200000000     C                   Z-ADD     999           WCTR
023300000000     C                   ENDIF
023400000000     C                   ELSE
023500080318     C                   Z-ADD     blpksc        WCLI
023600080318     C                   MOVE      blpksc        N4                4 0
023700000000     C     N4            IFEQ      8888
023800000000     C                   Z-ADD     888           WCTR
023900000000     C                   ELSE
024000080318     C                   Z-ADD     blpctr        WCTR
024100000000     C                   ENDIF
024200000000     C                   ENDIF
024300000000     C                   ENDIF
024400040220     C*
024500040220     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE GIA' CONVERTITO
024600040220     C                   EXSR      DECCLI
024700000000     C*
024800000000     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
024900000000     C     WRECOK        IFEQ      'S'
025000000000     C                   Z-ADD     WCLI          DSCLI
025100000000     C                   Z-ADD     DSFIL         WFIL              3 0
025200080318     C**                 EXSR      DIVARE
025300080318     C**                 MOVEL     WDIV          WDIV              1
025400080318     C**                 Z-ADD     WARE          WARE              3 0
025500000000     C                   ENDIF
025600000000     C*
025700080317     C**   WRECOK        IFEQ      'S'
025800000000     C*
025900000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
026000000000     C*
026100080317     C**   PARDIV        IFNE      *BLANKS
026200080317     C**   WDIV          IFEQ      PARDIV
026300080317     C**                 ELSE
026400080317     C**                 MOVEL     'N'           WRECOK
026500080317     C**                 ENDIF
026600080317     C**                 ENDIF
026700000000     C*
026800000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
026900080317     C**   PARARE        IFNE      *ZEROS
027000080317     C**   WARE          IFEQ      PARARE
027100080317     C**                 ELSE
027200080317     C**                 MOVEL     'N'           WRECOK
027300080317     C**                 ENDIF
027400080317     C**                 ENDIF
027500000000     C*
027600000000     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
027700080317     C**   PARFIL        IFNE      *ZEROS
027800080317     C**   TASLNP        IFNE      PARFIL
027900080317     C**                 MOVEL     'N'           WRECOK
028000080317     c**                 ENDIF
028100080317     C**                 ENDIF
028200080317     C**                 ENDIF
028300011112     C*
028400011112     C* SE NEL LANCIO RICHIESTO SOLO SPEDIZIONI "NEGATIVE" (OVVERO CON BESO BOLL
028500011112     C* PESO RILEVATO) => ESCUDO I RECORD CHE NON SODDISFANO LA CONDIZOINE RICHI
028600080320     C***  PARNEG        IFEQ      'S'
028700080320     C***  blpPKB        IFGE      blpPKC
028800080320     C***                MOVEL     'N'           WRECOK
028900080320     C***                ENDIF
029000080320     C***                ENDIF
029100011112     C*
029200000000     C                   ENDSR
029300000000     C*------------------------------------------------------------------------*
029400000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
029500000000     C*------------------------------------------------------------------------*
029600000000     C     CREARE        BEGSR
029700991025     C*
029800991025     C*
029900080317     C                   clear                   KWPANN
030000000000     C                   MOVEL     WDIV          KWPDIV
030100000000     C                   Z-ADD     WARE          KWPARE
030200080318     C                   Z-ADD     partfp        KWPFIL
030300040220     C                   MOVEL     WFPRAG        KWPRAG
030400000000     C                   Z-ADD     WCLI          KWPKSC
030500000000     C                   Z-ADD     WCTR          KWPCTR
030600080317     C                   clear                   KWPMES
030700000000     C*
030800000000     C* SCRIVO O RISCRIVO FILE DI LAVORO PER STAMPA STATISTICA PESI
030900000000     C*
031000090707     C     KEYWFP        CHAIN     WFPESF1L                           99
031100000000     C*
031200000000     C* RIEMPIO BUFFER RECORD
031300000000     C*
031400000000     C                   Z-ADD     KWPANN        WFPANN
031500000000     C                   MOVEL     KWPDIV        WFPDIV
031600000000     C                   Z-ADD     KWPARE        WFPARE
031700000000     C                   Z-ADD     KWPFIL        WFPFIL
031800000000     C                   Z-ADD     KWPKSC        WFPKSC
031900000000     C                   Z-ADD     KWPCTR        WFPCTR
032000000000     C                   Z-ADD     KWPMES        WFPMES
032100991025     C   99              EXSR      AZZWFP
032200000000     C                   EXSR      ADDWFP
032300090708     C   99              WRITE     WFPESf
032400090708     C  N99              UPDATE    WFPESf
032500991025     C*
032600000000     C                   ENDSR
032700000000     C*------------------------------------------------------------------------*
032800090708     C* ADDWFP - AGGIUNGE LA SPEDIZIONE AL RECORD WFPESF0F
032900000000     C*------------------------------------------------------------------------*
033000000000     C     ADDWFP        BEGSR
033100991025     C*
033200991025     C                   ADD       1             WFPTNS
033300080317     C                   ADD       blppkb        WFPTPB
033400080317     C                   ADD       blpncl        WFPTCL
033500080317     c*
033600080317     c                   exsr      sr_blt
033700991025     C*
033800991025     C* CONTROLLO SE NUM. COLLI BOLL. = NUM. COLLI VDL
033900991025     C* SE DIVERSO RIEMPIO CAMPI SE PESO VDL COMUNQUE
034000991025     C* > PESO BOLLETTATO                                              RILEVATI
034100991025     C*
034200991025     C                   Z-ADD     0             VDLTAR            8 3
034300991025     C                   Z-ADD     0             VDLNET            9 1
034400080317     C     wncp          MULT      PARTAR        VDLTAR
034500080318     C     wpkc          SUB       VDLTAR        VDLNET
034600991025     C*
034700080317     C     blpncl        IFNE      wncp
034800080318     C     VDLNET        IFGT      blpPKB
034900000000     C                   ADD       VDLNET        WFPPSC
035000080317     C                   ADD       blpncl        WFPNCL
035100000000     C                   ADD       1             WFPNSP
035200080317     C                   ADD       blpPKB        WFPPSB
035300080317     C                   ADD       wncp          WFPNCV
035400991026     C                   ENDIF
035500991026     C                   ELSE
035600991026     C                   ADD       VDLNET        WFPPSC
035700080317     C                   ADD       blpncl        WFPNCL
035800991026     C                   ADD       1             WFPNSP
035900080317     C                   ADD       blppkb        WFPPSB
036000080317     C                   ADD       wncp          WFPNCV
036100991026     C                   ENDIF
036200991025     C*
036300000000     C                   ENDSR
036400080317     C*------------------------------------------------------------------------*
036500080317     C* sr_blt - legge e totalizza peso vdl rilevato in partenza
036600080317     C*------------------------------------------------------------------------*
036700080317     C     sr_blt        BEGSR
036800080318     c                   clear                   totpuc
036900080318     c                   clear                   wpkc
037000080317     c                   clear                   wncp
037100080317     c     kblt          setll     fnblt01l
037200080317     c                   do        *hival
037300080317     c     kblt          reade     fnblt01l
037400080317     c                   if         %eof(fnblt01l)
037500080317     c                   leave
037600080317     c                   endif
037700080317     c                   if        bltfpc='P' and bltpuc>0
037800080318     c                   add       bltpuc        totpuc
037900080317     c                   add       1             wncp
038000080318     c                   endif
038100080317     c                   enddo
038200080318     C                   Z-ADD(H)  totpuc        wpkc
038300080317     C                   ENDSR
038400000000     C*------------------------------------------------------------------------*
038500090708     C* AZZWFP - AZZERA CAMPI RANGE IN WFPESF0F
038600000000     C*------------------------------------------------------------------------*
038700000000     C     AZZWFP        BEGSR
038800000000     C*
038900991025     C                   Z-ADD     *ZEROS        WFPTCL
039000991025     C                   Z-ADD     *ZEROS        WFPTPB
039100991025     C                   Z-ADD     *ZEROS        WFPTNS
039200000000     C                   Z-ADD     *ZEROS        WFPPSC
039300000000     C                   Z-ADD     *ZEROS        WFPNCL
039400000000     C                   Z-ADD     *ZEROS        WFPNSP
039500000000     C                   Z-ADD     *ZEROS        WFPPSB
039600000000     C                   Z-ADD     *ZEROS        WFPNCV
039700000000     C*
039800000000     C                   ENDSR
039900040220     C*------------------------------------------------------------------------*
040000040220     C* DECCLI - DECODIFICA CLIENTE
040100040220     C*------------------------------------------------------------------------*
040200040220     C     DECCLI        BEGSR
040300040220     C*
040400040220     C                   Z-ADD     WCLI          DSCLI
040500040220     C                   CLEAR                   BS69DS
040600040220     C                   CLEAR                   ACODS
040700040220     C                   MOVEL     KNSIF         I69SIF
040800040220     C                   Z-ADD     WCLI          I69KAC
040900090707     C                   Z-ADD     WCLI          I69Kcp
041000040220     C                   CALL      'TIBS69R'
041100040220     C                   PARM                    BS69DS
041200040220     C                   PARM                    ACODS
041300040220     C                   PARM                    INDDS
041400040220     C                   PARM                    CLPDS
041500040220     C                   PARM                    CLSDS
041600040220     C     O69ERR        IFNE      '1'
041700040220     C                   MOVEL     ACORAG        WFPRAG
041800090707     C                   MOVEL     clpage        W_clpage
041900040220     C                   ELSE
042000040220     C                   MOVEL     *ALL'*'       WFPRAG
042100090707     c                   clear                   w_clpage
042200040220     C                   ENDIF
042300090707IF  1C     DSCOD         IFEQ      8888
042400090707     c                   EVAL      WFPRAG = 'Cliente non codificato'
042500090707     c                   endif
042600090707     c* reperimento codice commerciale unificante
042700140611     C     w_CLPage      chain     AZCMM000
042800140611     c                   if        %found(AZCMM01L)
042900140611     C                   z-add     CMMuni        WFPCUN
043000090708     c                   else
043100090708     c                   clear                   wfpcun
043200090708     c                   endif
043300040220     C*
043400040220     C                   ENDSR
043500000000     C*------------------------------------------------------------------------*
043600000000     C* DIVARE - RECUPERA AREA E DIVISIONE
043700000000     C*------------------------------------------------------------------------*
043800000000     C     DIVARE        BEGSR
043900000000     C*
044000000000     C                   Z-ADD     1             I
044100000000     C     WFIL          LOOKUP    FIL(I)                                 99
044200000000     C     *IN99         IFEQ      '0'
044300000000     C                   MOVEL     '*'           WDIV              1
044400000000     C                   Z-ADD     999           WARE              3 0
044500000000     C                   ELSE
044600000000     C                   MOVEL     DIV(I)        WDIV
044700000000     C                   Z-ADD     ARE(I)        WARE
044800000000     C                   END
044900000000     C*
045000000000     C                   ENDSR
045100000000     C*------------------------------------------------------------------------*
045200000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
045300000000     C*------------------------------------------------------------------------*
045400000000     C     CARORG        BEGSR
045500000000     C*
045600000000     C                   Z-ADD     *ZEROS        KORFIL
045700000000     C     KEYORG        SETLL     AZORG01L                           99
045800000000     C  N99              READ      AZORG01L                               99
045900000000     C                   Z-ADD     0             I                 3 0
046000000000     C     *IN99         DOWEQ     '0'
046100000000     C     ORGFVA        IFEQ      *BLANKS
046200000000     C     ORGFAG        IFNE      'V'
046300000000     C                   ADD       1             I
046400000000     C                   Z-ADD     ORGFIL        FIL(I)
046500000000     C                   Z-ADD     ORGCAR        ARE(I)
046600000000     C                   MOVEL     ORGFL3        DIV(I)
046700000000     C                   MOVEL     ORGDIT        DIT(I)
046800000000     C                   END
046900000000     C                   END
047000000000     C                   READ      AZORG01L                               99
047100000000     C                   END
047200000000     C*
047300000000     C                   ENDSR
047400000000     C*------------------------------------------------------------------------*
047500000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
047600000000     C*------------------------------------------------------------------------*
047700000000     C     CARTBL        BEGSR
047800000000     C*--------------------
047900000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
048000000000     C*--------------------
048100000000     C                   Z-ADD     0             I
048200000000     C                   Z-ADD     CODUT         KTBUTE
048300000000     C                   MOVEL     '2W'          KTBCOD
048400000000     C                   MOVEL     *BLANKS       KTBKEY
048500000000     C     KEYTAB        SETLL     TABEL00F
048600000000     C                   READ      TABEL00F                               99
048700000000     C     *IN99         DOWEQ     *OFF
048800000000     C     TBLCOD        ANDEQ     '2W'
048900000000     C     TBLFLG        IFEQ      *BLANKS
049000000000     C                   ADD       1             I
049100000000     C                   MOVEL     TBLKEY        RBL(I)
049200000000     C                   MOVEL     TBLUNI        DS2W
049300000000     C                   MOVEL     §2WFSP        FSP(I)
049400000000     C                   MOVEL     §2WFRI        FRI(I)
049500000000     C                   END
049600000000     C                   READ      TABEL00F                               99
049700000000     C                   END
049800000000     C*--------------------
049900000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
050000000000     C*--------------------
050100000000     C                   Z-ADD     0             I
050200000000     C                   Z-ADD     CODUT         KTBUTE
050300000000     C                   MOVEL     'TB'          KTBCOD
050400000000     C                   MOVEL     *BLANKS       KTBKEY
050500000000     C     KEYTAB        SETLL     TABEL00F
050600000000     C                   READ      TABEL00F                               99
050700000000     C     *IN99         DOWEQ     *OFF
050800000000     C     TBLCOD        ANDEQ     'TB'
050900000000     C     TBLFLG        IFEQ      *BLANKS
051000000000     C                   ADD       1             I
051100000000     C                   MOVEL     TBLKEY        TB(I)
051200000000     C                   MOVEL     TBLUNI        DSTB
051300000000     C                   Z-ADD     1             J                 4 0
051400000000     C                   SETOFF                                       28
051500000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
051600000000     C     *IN28         IFEQ      *ON
051700000000     C                   MOVEL     FSP(J)        SP(I)
051800000000     C                   MOVEL     FRI(J)        RI(I)
051900000000     C                   ELSE
052000000000     C                   MOVEL     'N'           SP(I)
052100000000     C                   MOVEL     'N'           RI(I)
052200000000     C                   ENDIF
052300000000     C                   END
052400000000     C                   READ      TABEL00F                               99
052500000000     C                   END
052600080318     C*--------------------
052700080318     C* codice bolla e relativo tipo bolla
052800080318     C*--------------------
052900080318     C                   Z-ADD     0             I
053000080318     C                   Z-ADD     CODUT         KTBUTE
053100080318     C                   MOVEL     '3A'          KTBCOD
053200080318     C                   MOVEL     *BLANKS       KTBKEY
053300080318     C     KEYTAB        SETLL     TABEL00F
053400080318     C                   READ      TABEL00F                               99
053500080318     C     *IN99         DOWEQ     *OFF
053600080318     C     TBLCOD        ANDEQ     '3A'
053700080318     C     TBLFLG        IFEQ      *BLANKS
053800080318     C                   ADD       1             I
053900080318     C                   MOVEL     TBLKEY        cbo(I)
054000080318     C                   MOVEL     TBLUNI        DS3A
054100080318     C                   MOVEL     §3atb1        tbl(I)
054200080318     C                   END
054300080318     C                   READ      TABEL00F                               99
054400080318     C                   END
054500000000     C*
054600000000     C                   ENDSR
054700040220     C*------------------------------------------------------------------------*
054800040220     C* FINSR - OPERAZIONI FINALI
054900040220     C*------------------------------------------------------------------------*
055000040220     C     FINSR         BEGSR
055100040220     C*
055200040220     C* CHIUDE I FILE DEI PGM CHIAMATI
055300040220     C                   CLEAR                   BS69DS
055400040220     C                   CLEAR                   ACODS
055500040220     C                   CLEAR                   INDDS
055600040220     C                   CLEAR                   CLPDS
055700040220     C                   CLEAR                   CLSDS
055800040220     C                   MOVEL     'C'           I69TLA
055900040220     C                   MOVEL     KNSIF         I69SIF
056000040220     C                   CALL      'TIBS69R'
056100040220     C                   PARM                    BS69DS
056200040220     C                   PARM                    ACODS
056300040220     C                   PARM                    INDDS
056400040220     C                   PARM                    CLPDS
056500040220     C                   PARM                    CLSDS
056600040220     C*
056700040220     C                   ENDSR
056800000000     C*------------------------------------------------------------------------*
056900000000     C* *INZSR - ROUTINE INIZIALE
057000000000     C*------------------------------------------------------------------------*
057100000000     C     *INZSR        BEGSR
057200000000     C*--------------------
057300000000     C* RICEVIMENTO PARAMETRI
057400000000     C*--------------------
057500000000     C     *ENTRY        PLIST
057600000000     C                   PARM                    KPJBA
057700000000     C                   MOVEL     KPJBU         PARAM
057800000000     C*--------------------
057900000000     C* CAMPI RIFERITI AL DATA BASE
058000000000     C*--------------------
058100080318     C***  *LIKE         DEFINE    TASAAS        KTAAAS
058200080318     C***  *LIKE         DEFINE    TASMGS        KTAMGS
058300080318     C***  *LIKE         DEFINE    TASKSC        KTAKSC
058400000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
058500000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
058600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
058700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
058800000000     C     *LIKE         DEFINE    WFPANN        KWPANN
058900000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
059000000000     C     *LIKE         DEFINE    WFPARE        KWPARE
059100000000     C     *LIKE         DEFINE    WFPFIL        KWPFIL
059200040220     C     *LIKE         DEFINE    WFPRAG        KWPRAG
059300000000     C     *LIKE         DEFINE    WFPKSC        KWPKSC
059400000000     C     *LIKE         DEFINE    WFPCTR        KWPCTR
059500000000     C     *LIKE         DEFINE    WFPMES        KWPMES
059600080318     C***  *LIKE         DEFINE    TASKSC        WCLI
059700080318     C**** *LIKE         DEFINE    TASCTR        WCTR
059800080318     C     *LIKE         DEFINE    blpKSC        WCLI
059900080318     C     *LIKE         DEFINE    blpCTR        WCTR
060000000000     C*--------------------
060100000000     C* CHIAVI DI LETTURA
060200000000     C*--------------------
060300000000     C* CHIAVE LETTURA TITAS38C - COMPLETA
060400080318     C*    KEYTAS        KLIST
060500080318     C*                  KFLD                    KTAAAS
060600080318     C*                  KFLD                    KTAMGS
060700080318     C*                  KFLD                    KTAKSC
060800000000     C*
060900000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
061000000000     C     KEYORG        KLIST
061100000000     C                   KFLD                    KORFIL
061200090707     C*
061300000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
061400000000     C     KEYTAB        KLIST
061500000000     C                   KFLD                    KTBUTE
061600000000     C                   KFLD                    KTBCOD
061700000000     C                   KFLD                    KTBKEY
061800000000     C*
061900090708     C* CHIAVE LETTURA WFPESF1L - COMPLETA
062000000000     C     KEYWFP        KLIST
062100000000     C                   KFLD                    KWPANN
062200000000     C                   KFLD                    KWPDIV
062300000000     C                   KFLD                    KWPARE
062400000000     C                   KFLD                    KWPFIL
062500040220     C                   KFLD                    KWPRAG
062600000000     C                   KFLD                    KWPKSC
062700000000     C                   KFLD                    KWPCTR
062800000000     C                   KFLD                    KWPMES
062900080318
063000080318     C* CHIAVE LETTURA fnblt01l
063100080318     C     kblt          KLIST
063200080318     C                   KFLD                    blpaas
063300080318     C                   KFLD                    blplnp
063400080318     C                   KFLD                    blpnrs
063500080318     C                   KFLD                    blpnsp
063600000000     C*--------------------
063700000000     C* CAMPI DI CONTROLLO
063800000000     C*--------------------
063900000000     C                   MOVEL     '0'           FINL00            1
064000000000     C                   Z-ADD     1             CODUT             1 0
064100991025     C                   Z-ADD     *ZEROS        MEMCLI            7 0
064200991025     C                   Z-ADD     *ZEROS        MEMCTR            3 0
064300000000     C*
064400000000     C***
064500000000     C* LANCIO PER SALDI
064600000000     C***
064700000000     C* CARICA ORGANIGRAMMA
064800000000     C                   EXSR      CARORG
064900000000     C*
065000000000     C* CARICA TIPI BOLLA
065100000000     C                   EXSR      CARTBL
065200090707     c* recupera divisione e area del terminal di partenza per memo in
065300080318     c* file di work
065400080318     C                   Z-ADD     partfp        WFIL              3 0
065500080318     C                   EXSR      DIVARE
065600080318     C                   MOVEL     WDIV          WDIV              1
065700080318     C                   Z-ADD     WARE          WARE              3 0
065800000000     C*
065900000000     C                   ENDSR
