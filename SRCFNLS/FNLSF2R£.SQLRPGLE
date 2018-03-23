000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA PESO VDL - GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500080317     Ffnblt01l  IF   E           K DISK
000600000000     FAZORG01L  IF   E           K DISK
000700000000     FTABEL00F  IF   E           K DISK
000800090707     FWFPESF1L  UF A E           K DISK
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200000000     D*-------------------
001300000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001400000000     D*-------------------
001500000000     D FIL             S              3  0 DIM(500)
001600000000     D ARE             S              3  0 DIM(500)
001700000000     D DIV             S              1    DIM(500)
001800000000     D DIT             S              3    DIM(500)
001900000000     D*-------------------
002000000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
002100000000     D*-------------------
002200000000     D RBL             S              1    DIM(5)
002300000000     D FSP             S              1    DIM(5)
002400000000     D FRI             S              1    DIM(5)
002500000000     D*-------------------
002600000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
002700000000     D*-------------------
002800000000     D TB              S              2    DIM(20)
002900000000     D SP              S              1    DIM(20)
003000000000     D RI              S              1    DIM(20)
003100080318     D cbo             S              2    DIM(99)
003200080318     D tbl             S              2    DIM(99)
003300000000     D*------------------------------------------------------------------------*
003400000000     D* INPUT
003500000000     D*------------------------------------------------------------------------*
003600000000     D*-------------------
003700000000     D* ARCHITETTURA
003800000000     D*-------------------
003900000000     D KPJBA         E DS
004000080317     D*-------------------
004100080317     D fnblpds       E DS                  EXTNAME(fnblp00f)
004200000000     D*-------------------
004300000000     D* DS TIPO BOLLA
004400000000     D*-------------------
004500000000     D DSTB          E DS
004501090707     D*-------------------
004502090707     D* DS agenti commerciali
004503090707     D*-------------------
004504090707     D DS01          E DS
004600000000     D*-------------------
004700000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
004800000000     D*-------------------
004900000000     D DS2W          E DS
005000080318     D DS3a          E DS
005100040220     D*------------------
005200040220     D* DS REPERIMENTO DATI UTENTE
005300040220     D*-------------------
005400040220     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
005500040220     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
005600040220     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005700040220     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005800040220     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005900000000     D*-------------------
006000000000     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
006100000000     D*-------------------
006200991028     D                 DS                  INZ
006300991028     D  MM                     1      2  0
006400991028     D  MMGG                   1      4  0
006500000000     D                 DS                  INZ
006600000000     D  DIANN                  1      4  0
006700000000     D  DIMES                  5      6  0
006800000000     D  DIMGS                  5      8  0
006900000000     D  DIGIO                  7      8  0
007000000000     D  DIAAM                  1      6  0
007100991213     D  DIDAT                  1      8  0
007200000000     D                 DS                  INZ
007300000000     D  DFANN                  1      4  0
007400000000     D  DFMES                  5      6  0
007500000000     D  DFMGS                  5      8  0
007600000000     D  DFGIO                  7      8  0
007700000000     D  DFAAM                  1      6  0
007800000000     D*-------------------
007900000000     D* DS SCOMPOSIZIONE CODICE CLIENTE
008000000000     D*-------------------
008100000000     D                 DS
008200000000     D  DSCLI                  1      7  0
008300000000     D  DSFIL                  1      3  0
008400000000     D  DSCOD                  4      7  0
008500000000     D*-------------------
008600000000     D* DS PARAMETRI DI LANCIO PGM
008700000000     D*-------------------
008800000000     D PARAM           DS
008900000000     D  PARTLA                 1      1
009000080317     D**PARANN                 2      5  0
009100080317     D**PARMMI                 6      7  0
009200080317     D**PARMMF                 8      9  0
009300080317     d  pardal                 2      9  0
009400080317     D**PARSNR                10     10
009500080317     D**PARDTI                11     18  0
009600080317     D**PARDTF                19     26  0
009700991025     D  PARPEI                27     31  2
009800080317     D**PARDIV                32     32
009900080317     D**PARARE                33     35  0
010000080317     D**PARFIL                36     38  0
010100080317     D  PARtfp                36     38  0
010200000000     D  PARDET                39     39
010300080317     D**PARTOT                40     40
010400991026     D  PARRG1                41     44  1
010500991026     D  PARRG2                45     48  1
010600991026     D  PARTAR                49     53  3
010700991026     D  PARPES                54     58  2
010800991229     D  PARMIN                59     63  3
010900080317     D**PARPOS                64     64
011000080320     D**PARNEG                65     65
011100080317     D**PARSAL                66     66
011200080317     d  paral                 67     74  0
011201090707     d  parord                75     75
011202090707     d  parpag                76     76
011203090708     d  parflp                77     79  0
011300080317     d
011400080318     d totpuc                         8  3
011500080318     d wpkc            s                   like(blppkc)
011600080318     d wncp            s                   like(blpncp)
011601090707      *
011602090707     d w_clpage        s                   like(clpage)
011700000000     C*------------------------------------------------------------------------*
011800000000     C* MAIN LINE
011900000000     C*------------------------------------------------------------------------*
012000080317     c/exec sql
012100080317     c+ set option dynusrprf = *owner, closqlcsr = *endmod
012200080317     c/end-exec
012300080317
012301090707     c                   if        parflp=0
012400080317     C/EXEC SQL
012500080317     C+ Declare A1 Cursor for
012600080317     c+ SELECT * FROM fnblp00f
012700080317     c+ WHERE blptfp = :partfp AND blpaas*10000+blpmgs
012800080317     c+ between :PARdal and :paral
012900080317     C/END-EXEC
012901090708      *          apertura cursore
012902090708     C/EXEC SQL
012903090708     C+ OPEN A1
012904090708     C/END-EXEC
012905090707     c                   else
012906090707     C/EXEC SQL
012907090708     C+ Declare B1 Cursor for
012908090707     c+ SELECT * FROM fnblp00f
012909090707     c+ WHERE blptfp = :partfp AND blplnp= :parflp AND blpaas*10000+blpmgs
012910090707     c+ between :PARdal and :paral
012911090707     C/END-EXEC
012912090708      *          apertura cursore
012913090708     C/EXEC SQL
012914090708     C+ OPEN B1
012915090708     C/END-EXEC
012916090707     c                   endif
013400080317     C                   DOU       SqlCod <> 0
013401090708     c*
013402090708     c                   if        parflp=0
013500080317      *          lettura cursore
013600080317     C/EXEC SQL
013700080317     C+ Fetch Next From A1 Into :fnblpds
013800080317     C/END-EXEC
013801090708     c                   else
013802090708      *          lettura cursore
013803090708     C/EXEC SQL
013804090708     C+ Fetch Next From B1 Into :fnblpds
013805090708     C/END-EXEC
013806090708     c                   endif
013807090708
013900080317     C                   SELECT
014000080317     C                   WHEN      SqlCod = 100
014100080317     c                   leave
014200080317      *
014300080317     C                   WHEN      SqlCod < 0
014400080317     c                   leave
014500080317      *
014600080317     C                   OTHER
014700080317      * legge e scrive
014800080317     c                   exsr      chkrec
014900080317     c                   if        wrecok='S'
015000080317     C                   EXSR      CREARE
015100080317     c                   endif
015200080317      *
015300080317     C                   ENDSL
015400080317      *
015500080317     C                   ENDDO
015501090708     c                   if        parflp=0
015600080317     C/EXEC SQL
015700080317     C+ Close A1
015800080317     C/END-EXEC
015801090708     c                   else
015802090708     C/EXEC SQL
015803090708     C+ Close B1
015804090708     C/END-EXEC
015805090708     c                   endif
015900000000     C*
016000000000     C                   SETON                                        LR
016100000000     C*------------------------------------------------------------------------*
016200000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
016300000000     C*------------------------------------------------------------------------*
016400000000     C     CHKREC        BEGSR
016500000000     C*
016600000000     C                   MOVEL     'S'           WRECOK            1
016700000000     C*
016800080320     C***  blpNCP        IFEQ      99999
016900080320     C***                MOVEL     'N'           WRECOK
017000080320     C***                ENDIF
017100040305     C*
017200000000     C* ESCLUDE LE BOLLE NON VALIDE COME "SPEDIZIONE"
017300000000     C     WRECOK        IFEQ      'S'
017400000000     C                   Z-ADD     1             J
017500080318     C                   Z-ADD     1             k                 4 0
017600000000     C                   SETOFF                                           28
017700080318     c     blpcbo        lookup    cbo(J)                                 28
017800080318     C     tbl(J)        LOOKUP    TB(K)                                  28
017900080318     C     SP(k)         IFEQ      'N'
018000000000     C                   MOVEL     'N'           WRECOK
018100000000     C                   ENDIF
018200000000     C                   ENDIF
018300000000     C*
018400000000     C* SE E' ASSEGNATO PRENDE CLIENTE MITTENTE, SE FRANCO CLIENTE DESTINO
018500000000     C     WRECOK        IFEQ      'S'
018600000000     C                   Z-ADD     *ZEROS        WCLI
018700000000     C                   Z-ADD     *ZEROS        WCTR
018800080318     C                   MOVEL     tbl(j)        A1                1
018900000000     C     A1            IFEQ      'A'
019000080318     C     blpccm        IFEQ      *ZEROS
019100000000     C                   MOVEL     'N'           WRECOK
019200000000     C                   ELSE
019300080318     C                   Z-ADD     blpccm        WCLI
019400000000     C                   Z-ADD     999           WCTR
019500000000     C                   ENDIF
019600000000     C                   ELSE
019700080318     C                   Z-ADD     blpksc        WCLI
019800080318     C                   MOVE      blpksc        N4                4 0
019900000000     C     N4            IFEQ      8888
020000000000     C                   Z-ADD     888           WCTR
020100000000     C                   ELSE
020200080318     C                   Z-ADD     blpctr        WCTR
020300000000     C                   ENDIF
020400000000     C                   ENDIF
020500000000     C                   ENDIF
020600040220     C*
020700040220     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE GIA' CONVERTITO
020800040220     C                   EXSR      DECCLI
020900000000     C*
021000000000     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
021100000000     C     WRECOK        IFEQ      'S'
021200000000     C                   Z-ADD     WCLI          DSCLI
021300000000     C                   Z-ADD     DSFIL         WFIL              3 0
021400080318     C**                 EXSR      DIVARE
021500080318     C**                 MOVEL     WDIV          WDIV              1
021600080318     C**                 Z-ADD     WARE          WARE              3 0
021700000000     C                   ENDIF
021800000000     C*
021900080317     C**   WRECOK        IFEQ      'S'
022000000000     C*
022100000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
022200000000     C*
022300080317     C**   PARDIV        IFNE      *BLANKS
022400080317     C**   WDIV          IFEQ      PARDIV
022500080317     C**                 ELSE
022600080317     C**                 MOVEL     'N'           WRECOK
022700080317     C**                 ENDIF
022800080317     C**                 ENDIF
022900000000     C*
023000000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
023100080317     C**   PARARE        IFNE      *ZEROS
023200080317     C**   WARE          IFEQ      PARARE
023300080317     C**                 ELSE
023400080317     C**                 MOVEL     'N'           WRECOK
023500080317     C**                 ENDIF
023600080317     C**                 ENDIF
023700000000     C*
023800000000     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
023900080317     C**   PARFIL        IFNE      *ZEROS
024000080317     C**   TASLNP        IFNE      PARFIL
024100080317     C**                 MOVEL     'N'           WRECOK
024200080317     c**                 ENDIF
024300080317     C**                 ENDIF
024400080317     C**                 ENDIF
024500011112     C*
024600011112     C* SE NEL LANCIO RICHIESTO SOLO SPEDIZIONI "NEGATIVE" (OVVERO CON BESO BOLL
024700011112     C* PESO RILEVATO) => ESCUDO I RECORD CHE NON SODDISFANO LA CONDIZOINE RICHI
024800080320     C***  PARNEG        IFEQ      'S'
024900080320     C***  blpPKB        IFGE      blpPKC
025000080320     C***                MOVEL     'N'           WRECOK
025100080320     C***                ENDIF
025200080320     C***                ENDIF
025300011112     C*
025400000000     C                   ENDSR
025500000000     C*------------------------------------------------------------------------*
025600000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
025700000000     C*------------------------------------------------------------------------*
025800000000     C     CREARE        BEGSR
025900991025     C*
026000991025     C*
026100080317     C                   clear                   KWPANN
026200000000     C                   MOVEL     WDIV          KWPDIV
026300000000     C                   Z-ADD     WARE          KWPARE
026400080318     C                   Z-ADD     partfp        KWPFIL
026500040220     C                   MOVEL     WFPRAG        KWPRAG
026600000000     C                   Z-ADD     WCLI          KWPKSC
026700000000     C                   Z-ADD     WCTR          KWPCTR
026800080317     C                   clear                   KWPMES
026900000000     C*
027000000000     C* SCRIVO O RISCRIVO FILE DI LAVORO PER STAMPA STATISTICA PESI
027100000000     C*
027200090707     C     KEYWFP        CHAIN     WFPESF1L                           99
027300000000     C*
027400000000     C* RIEMPIO BUFFER RECORD
027500000000     C*
027600000000     C                   Z-ADD     KWPANN        WFPANN
027700000000     C                   MOVEL     KWPDIV        WFPDIV
027800000000     C                   Z-ADD     KWPARE        WFPARE
027900000000     C                   Z-ADD     KWPFIL        WFPFIL
028000000000     C                   Z-ADD     KWPKSC        WFPKSC
028100000000     C                   Z-ADD     KWPCTR        WFPCTR
028200000000     C                   Z-ADD     KWPMES        WFPMES
028300991025     C   99              EXSR      AZZWFP
028400000000     C                   EXSR      ADDWFP
028500090708     C   99              WRITE     WFPESf
028600090708     C  N99              UPDATE    WFPESf
028700991025     C*
028800000000     C                   ENDSR
028900000000     C*------------------------------------------------------------------------*
029000090708     C* ADDWFP - AGGIUNGE LA SPEDIZIONE AL RECORD WFPESF0F
029100000000     C*------------------------------------------------------------------------*
029200000000     C     ADDWFP        BEGSR
029300991025     C*
029400991025     C                   ADD       1             WFPTNS
029500080317     C                   ADD       blppkb        WFPTPB
029600080317     C                   ADD       blpncl        WFPTCL
029700080317     c*
029800080317     c                   exsr      sr_blt
029900991025     C*
030000991025     C* CONTROLLO SE NUM. COLLI BOLL. = NUM. COLLI VDL
030100991025     C* SE DIVERSO RIEMPIO CAMPI SE PESO VDL COMUNQUE
030200991025     C* > PESO BOLLETTATO                                              RILEVATI
030300991025     C*
030400991025     C                   Z-ADD     0             VDLTAR            8 3
030500991025     C                   Z-ADD     0             VDLNET            9 1
030600080317     C     wncp          MULT      PARTAR        VDLTAR
030700080318     C     wpkc          SUB       VDLTAR        VDLNET
030800991025     C*
030900080317     C     blpncl        IFNE      wncp
031000080318     C     VDLNET        IFGT      blpPKB
031100000000     C                   ADD       VDLNET        WFPPSC
031200080317     C                   ADD       blpncl        WFPNCL
031300000000     C                   ADD       1             WFPNSP
031400080317     C                   ADD       blpPKB        WFPPSB
031500080317     C                   ADD       wncp          WFPNCV
031600991026     C                   ENDIF
031700991026     C                   ELSE
031800991026     C                   ADD       VDLNET        WFPPSC
031900080317     C                   ADD       blpncl        WFPNCL
032000991026     C                   ADD       1             WFPNSP
032100080317     C                   ADD       blppkb        WFPPSB
032200080317     C                   ADD       wncp          WFPNCV
032300991026     C                   ENDIF
032400991025     C*
032500000000     C                   ENDSR
032600080317     C*------------------------------------------------------------------------*
032700080317     C* sr_blt - legge e totalizza peso vdl rilevato in partenza
032800080317     C*------------------------------------------------------------------------*
032900080317     C     sr_blt        BEGSR
033000080318     c                   clear                   totpuc
033100080318     c                   clear                   wpkc
033200080317     c                   clear                   wncp
033300080317     c     kblt          setll     fnblt01l
033400080317     c                   do        *hival
033500080317     c     kblt          reade     fnblt01l
033600080317     c                   if         %eof(fnblt01l)
033700080317     c                   leave
033800080317     c                   endif
033900080317     c                   if        bltfpc='P' and bltpuc>0
034000080318     c                   add       bltpuc        totpuc
034100080317     c                   add       1             wncp
034200080318     c                   endif
034300080317     c                   enddo
034400080318     C                   Z-ADD(H)  totpuc        wpkc
034500080317     C                   ENDSR
034600000000     C*------------------------------------------------------------------------*
034700090708     C* AZZWFP - AZZERA CAMPI RANGE IN WFPESF0F
034800000000     C*------------------------------------------------------------------------*
034900000000     C     AZZWFP        BEGSR
035000000000     C*
035100991025     C                   Z-ADD     *ZEROS        WFPTCL
035200991025     C                   Z-ADD     *ZEROS        WFPTPB
035300991025     C                   Z-ADD     *ZEROS        WFPTNS
035400000000     C                   Z-ADD     *ZEROS        WFPPSC
035500000000     C                   Z-ADD     *ZEROS        WFPNCL
035600000000     C                   Z-ADD     *ZEROS        WFPNSP
035700000000     C                   Z-ADD     *ZEROS        WFPPSB
035800000000     C                   Z-ADD     *ZEROS        WFPNCV
035900000000     C*
036000000000     C                   ENDSR
036100040220     C*------------------------------------------------------------------------*
036200040220     C* DECCLI - DECODIFICA CLIENTE
036300040220     C*------------------------------------------------------------------------*
036400040220     C     DECCLI        BEGSR
036500040220     C*
036600040220     C                   Z-ADD     WCLI          DSCLI
037000040220     C                   CLEAR                   BS69DS
037100040220     C                   CLEAR                   ACODS
037200040220     C                   MOVEL     KNSIF         I69SIF
037300040220     C                   Z-ADD     WCLI          I69KAC
037301090707     C                   Z-ADD     WCLI          I69Kcp
037400040220     C                   CALL      'TIBS69R'
037500040220     C                   PARM                    BS69DS
037600040220     C                   PARM                    ACODS
037700040220     C                   PARM                    INDDS
037800040220     C                   PARM                    CLPDS
037900040220     C                   PARM                    CLSDS
038000040220     C     O69ERR        IFNE      '1'
038100040220     C                   MOVEL     ACORAG        WFPRAG
038101090707     C                   MOVEL     clpage        W_clpage
038200040220     C                   ELSE
038300040220     C                   MOVEL     *ALL'*'       WFPRAG
038301090707     c                   clear                   w_clpage
038400040220     C                   ENDIF
038401090707IF  1C     DSCOD         IFEQ      8888
038403090707     c                   EVAL      WFPRAG = 'Cliente non codificato'
038404090707     c                   endif
038405090707     c* reperimento codice commerciale unificante
038406090708     c                   clear                   ds01
038407090707     c                   z-add     codut         ktbute
038408090707     C                   MOVEL     '01'          ktbcod
038409090707     C                   MOVEL(P)  W_clpage      Ktbkey
038410090707     C     keytab        CHAIN     TABEL00F
038411090708     c                   if        %found(tabel00f)
038412090707     C                   MOVEL     TBLUNI        ds01
038413090707     C                   MOVEL     §01rgf        WFPCUN
038414090708     c                   else
038415090708     c                   clear                   wfpcun
038416090708     c                   endif
038600040220     C*
038700040220     C                   ENDSR
038800000000     C*------------------------------------------------------------------------*
038900000000     C* DIVARE - RECUPERA AREA E DIVISIONE
039000000000     C*------------------------------------------------------------------------*
039100000000     C     DIVARE        BEGSR
039200000000     C*
039300000000     C                   Z-ADD     1             I
039400000000     C     WFIL          LOOKUP    FIL(I)                                 99
039500000000     C     *IN99         IFEQ      '0'
039600000000     C                   MOVEL     '*'           WDIV              1
039700000000     C                   Z-ADD     999           WARE              3 0
039800000000     C                   ELSE
039900000000     C                   MOVEL     DIV(I)        WDIV
040000000000     C                   Z-ADD     ARE(I)        WARE
040100000000     C                   END
040200000000     C*
040300000000     C                   ENDSR
040400000000     C*------------------------------------------------------------------------*
040500000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
040600000000     C*------------------------------------------------------------------------*
040700000000     C     CARORG        BEGSR
040800000000     C*
040900000000     C                   Z-ADD     *ZEROS        KORFIL
041000000000     C     KEYORG        SETLL     AZORG01L                           99
041100000000     C  N99              READ      AZORG01L                               99
041200000000     C                   Z-ADD     0             I                 3 0
041300000000     C     *IN99         DOWEQ     '0'
041400000000     C     ORGFVA        IFEQ      *BLANKS
041500000000     C     ORGFAG        IFNE      'V'
041600000000     C                   ADD       1             I
041700000000     C                   Z-ADD     ORGFIL        FIL(I)
041800000000     C                   Z-ADD     ORGCAR        ARE(I)
041900000000     C                   MOVEL     ORGFL3        DIV(I)
042000000000     C                   MOVEL     ORGDIT        DIT(I)
042100000000     C                   END
042200000000     C                   END
042300000000     C                   READ      AZORG01L                               99
042400000000     C                   END
042500000000     C*
042600000000     C                   ENDSR
042700000000     C*------------------------------------------------------------------------*
042800000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
042900000000     C*------------------------------------------------------------------------*
043000000000     C     CARTBL        BEGSR
043100000000     C*--------------------
043200000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
043300000000     C*--------------------
043400000000     C                   Z-ADD     0             I
043500000000     C                   Z-ADD     CODUT         KTBUTE
043600000000     C                   MOVEL     '2W'          KTBCOD
043700000000     C                   MOVEL     *BLANKS       KTBKEY
043800000000     C     KEYTAB        SETLL     TABEL00F
043900000000     C                   READ      TABEL00F                               99
044000000000     C     *IN99         DOWEQ     *OFF
044100000000     C     TBLCOD        ANDEQ     '2W'
044200000000     C     TBLFLG        IFEQ      *BLANKS
044300000000     C                   ADD       1             I
044400000000     C                   MOVEL     TBLKEY        RBL(I)
044500000000     C                   MOVEL     TBLUNI        DS2W
044600000000     C                   MOVEL     §2WFSP        FSP(I)
044700000000     C                   MOVEL     §2WFRI        FRI(I)
044800000000     C                   END
044900000000     C                   READ      TABEL00F                               99
045000000000     C                   END
045100000000     C*--------------------
045200000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
045300000000     C*--------------------
045400000000     C                   Z-ADD     0             I
045500000000     C                   Z-ADD     CODUT         KTBUTE
045600000000     C                   MOVEL     'TB'          KTBCOD
045700000000     C                   MOVEL     *BLANKS       KTBKEY
045800000000     C     KEYTAB        SETLL     TABEL00F
045900000000     C                   READ      TABEL00F                               99
046000000000     C     *IN99         DOWEQ     *OFF
046100000000     C     TBLCOD        ANDEQ     'TB'
046200000000     C     TBLFLG        IFEQ      *BLANKS
046300000000     C                   ADD       1             I
046400000000     C                   MOVEL     TBLKEY        TB(I)
046500000000     C                   MOVEL     TBLUNI        DSTB
046600000000     C                   Z-ADD     1             J                 4 0
046700000000     C                   SETOFF                                       28
046800000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
046900000000     C     *IN28         IFEQ      *ON
047000000000     C                   MOVEL     FSP(J)        SP(I)
047100000000     C                   MOVEL     FRI(J)        RI(I)
047200000000     C                   ELSE
047300000000     C                   MOVEL     'N'           SP(I)
047400000000     C                   MOVEL     'N'           RI(I)
047500000000     C                   ENDIF
047600000000     C                   END
047700000000     C                   READ      TABEL00F                               99
047800000000     C                   END
047900080318     C*--------------------
048000080318     C* codice bolla e relativo tipo bolla
048100080318     C*--------------------
048200080318     C                   Z-ADD     0             I
048300080318     C                   Z-ADD     CODUT         KTBUTE
048400080318     C                   MOVEL     '3A'          KTBCOD
048500080318     C                   MOVEL     *BLANKS       KTBKEY
048600080318     C     KEYTAB        SETLL     TABEL00F
048700080318     C                   READ      TABEL00F                               99
048800080318     C     *IN99         DOWEQ     *OFF
048900080318     C     TBLCOD        ANDEQ     '3A'
049000080318     C     TBLFLG        IFEQ      *BLANKS
049100080318     C                   ADD       1             I
049200080318     C                   MOVEL     TBLKEY        cbo(I)
049300080318     C                   MOVEL     TBLUNI        DS3A
049400080318     C                   MOVEL     §3atb1        tbl(I)
049500080318     C                   END
049600080318     C                   READ      TABEL00F                               99
049700080318     C                   END
049800000000     C*
049900000000     C                   ENDSR
050000040220     C*------------------------------------------------------------------------*
050100040220     C* FINSR - OPERAZIONI FINALI
050200040220     C*------------------------------------------------------------------------*
050300040220     C     FINSR         BEGSR
050400040220     C*
050500040220     C* CHIUDE I FILE DEI PGM CHIAMATI
050600040220     C                   CLEAR                   BS69DS
050700040220     C                   CLEAR                   ACODS
050800040220     C                   CLEAR                   INDDS
050900040220     C                   CLEAR                   CLPDS
051000040220     C                   CLEAR                   CLSDS
051100040220     C                   MOVEL     'C'           I69TLA
051200040220     C                   MOVEL     KNSIF         I69SIF
051300040220     C                   CALL      'TIBS69R'
051400040220     C                   PARM                    BS69DS
051500040220     C                   PARM                    ACODS
051600040220     C                   PARM                    INDDS
051700040220     C                   PARM                    CLPDS
051800040220     C                   PARM                    CLSDS
051900040220     C*
052000040220     C                   ENDSR
052100000000     C*------------------------------------------------------------------------*
052200000000     C* *INZSR - ROUTINE INIZIALE
052300000000     C*------------------------------------------------------------------------*
052400000000     C     *INZSR        BEGSR
052500000000     C*--------------------
052600000000     C* RICEVIMENTO PARAMETRI
052700000000     C*--------------------
052800000000     C     *ENTRY        PLIST
052900000000     C                   PARM                    KPJBA
053000000000     C                   MOVEL     KPJBU         PARAM
053100000000     C*--------------------
053200000000     C* CAMPI RIFERITI AL DATA BASE
053300000000     C*--------------------
053400080318     C***  *LIKE         DEFINE    TASAAS        KTAAAS
053500080318     C***  *LIKE         DEFINE    TASMGS        KTAMGS
053600080318     C***  *LIKE         DEFINE    TASKSC        KTAKSC
053700000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
053800000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
053900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
054000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
054100000000     C     *LIKE         DEFINE    WFPANN        KWPANN
054200000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
054300000000     C     *LIKE         DEFINE    WFPARE        KWPARE
054400000000     C     *LIKE         DEFINE    WFPFIL        KWPFIL
054500040220     C     *LIKE         DEFINE    WFPRAG        KWPRAG
054600000000     C     *LIKE         DEFINE    WFPKSC        KWPKSC
054700000000     C     *LIKE         DEFINE    WFPCTR        KWPCTR
054800000000     C     *LIKE         DEFINE    WFPMES        KWPMES
054900080318     C***  *LIKE         DEFINE    TASKSC        WCLI
055000080318     C**** *LIKE         DEFINE    TASCTR        WCTR
055100080318     C     *LIKE         DEFINE    blpKSC        WCLI
055200080318     C     *LIKE         DEFINE    blpCTR        WCTR
055300000000     C*--------------------
055400000000     C* CHIAVI DI LETTURA
055500000000     C*--------------------
055600000000     C* CHIAVE LETTURA TITAS38C - COMPLETA
055700080318     C*    KEYTAS        KLIST
055800080318     C*                  KFLD                    KTAAAS
055900080318     C*                  KFLD                    KTAMGS
056000080318     C*                  KFLD                    KTAKSC
056100000000     C*
056200000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
056300000000     C     KEYORG        KLIST
056400000000     C                   KFLD                    KORFIL
056500090707     C*
056600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
056700000000     C     KEYTAB        KLIST
056800000000     C                   KFLD                    KTBUTE
056900000000     C                   KFLD                    KTBCOD
057000000000     C                   KFLD                    KTBKEY
057100000000     C*
057200090708     C* CHIAVE LETTURA WFPESF1L - COMPLETA
057300000000     C     KEYWFP        KLIST
057400000000     C                   KFLD                    KWPANN
057500000000     C                   KFLD                    KWPDIV
057600000000     C                   KFLD                    KWPARE
057700000000     C                   KFLD                    KWPFIL
057800040220     C                   KFLD                    KWPRAG
057900000000     C                   KFLD                    KWPKSC
058000000000     C                   KFLD                    KWPCTR
058100000000     C                   KFLD                    KWPMES
058200080318
058300080318     C* CHIAVE LETTURA fnblt01l
058400080318     C     kblt          KLIST
058500080318     C                   KFLD                    blpaas
058600080318     C                   KFLD                    blplnp
058700080318     C                   KFLD                    blpnrs
058800080318     C                   KFLD                    blpnsp
058900000000     C*--------------------
059000000000     C* CAMPI DI CONTROLLO
059100000000     C*--------------------
059200000000     C                   MOVEL     '0'           FINL00            1
059300000000     C                   Z-ADD     1             CODUT             1 0
059400991025     C                   Z-ADD     *ZEROS        MEMCLI            7 0
059500991025     C                   Z-ADD     *ZEROS        MEMCTR            3 0
059600000000     C*
059700000000     C***
059800000000     C* LANCIO PER SALDI
059900000000     C***
060000000000     C* CARICA ORGANIGRAMMA
060100000000     C                   EXSR      CARORG
060200000000     C*
060300000000     C* CARICA TIPI BOLLA
060400000000     C                   EXSR      CARTBL
060500090707     c* recupera divisione e area del terminal di partenza per memo in
060600080318     c* file di work
060700080318     C                   Z-ADD     partfp        WFIL              3 0
060800080318     C                   EXSR      DIVARE
060900080318     C                   MOVEL     WDIV          WDIV              1
061000080318     C                   Z-ADD     WARE          WARE              3 0
061100000000     C*
061200000000     C                   ENDSR
