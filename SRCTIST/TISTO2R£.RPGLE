000100000000     H*------------------------------------------------------------------------*
000200051108     H* Statistica spedizioni oltre 20 colli - Crea Work file
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     FTITAS38C  IF   E           K DISK
000600000000     FAZORG01L  IF   E           K DISK
000700000000     FTABEL00F  IF   E           K DISK
000800051108     FWFo2001l  UF A E           K DISK
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200000000     D*-------------------
001300000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001400000000     D*-------------------
001500000000     D FIL             S              3  0 DIM(500)                             *FILIALE
001600000000     D ARE             S              3  0 DIM(500)                             *AREA
001700000000     D DIV             S              1    DIM(500)                             *DIVISIONE
001800051109     D dfil            S             20    DIM(500)                             *FILIALE
001900051109     D dare            S             25    DIM(500)                             *AREA
002000051109     D ddiv            S             25    DIM(500)                             *DIVISIONE
002100000000     D*-------------------
002200000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
002300051109     D CT              S              2    DIM(50)                              TIP0 BOLLA
002400000000     D*------------------------------------------------------------------------*
002500000000     D* INPUT
002600000000     D*------------------------------------------------------------------------*
002700000000     D*-------------------
002800000000     D* ARCHITETTURA
002900000000     D*-------------------
003000000000     D KPJBA         E DS
003100000000     D*-------------------
003200000000     D* DS TIPO BOLLA
003300000000     D*-------------------
003400000000     D DSTB          E DS
003500051109     D*-------------------
003600051109     D DS05          E DS
003700051109     D DS17          E DS
003800051109     D DS1b          E DS
003900040220     D*------------------
004000040220     D* DS REPERIMENTO DATI UTENTE
004100040220     D*-------------------
004200051108     d Tibs69ds      e ds
004300040220     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
004400040220     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
004500040220     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
004600040220     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
004700000000     D*-------------------
004800000000     D* DS PARAMETRI DI LANCIO PGM
004900000000     D*-------------------
005000000000     D PARAM           DS
005100051108     D  PARdda                 1      8  0
005200051108     D  PARdal                 9     16  0
005300051109     D  PARaal                 9     12  0
005400051109     D  PARmga                13     16  0
005500000000     C*------------------------------------------------------------------------*
005600000000     C* MAIN LINE
005700000000     C*------------------------------------------------------------------------*
005800000000     C*
005900000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
006000000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
006100000000     C*
006200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
006300000000     C                   EXSR      SETFIL
006400000000     C*
006500000000     C* CICLO FINO A FINE FILE
006600000000     C     FINL00        DOWEQ     '0'                                          --- 2 -->
006700000000     C*
006800000000     C* CREA IL RECORD SUI FILE DI LAVORO
006900000000     C                   EXSR      CREARE
007000000000     C*
007100000000     C* LEGGE IL RECORD SUCCESSIVO
007200000000     C                   EXSR      LETFIL
007300000000     C                   ENDDO                                                  <-- 2 ---
007400000000     C*
007500000000     C                   ENDIF                                                  <-- 1 ---
007600000000     C*
007700000000     C                   SETON                                        LR
007800051109     C* CHIUDE I FILE DEI PGM CHIAMATI
007900051109     C                   CLEAR                   tibs69ds
008000051109     C                   CLEAR                   ACODS
008100051109     C                   CLEAR                   INDDS
008200051109     C                   CLEAR                   CLPDS
008300051109     C                   CLEAR                   CLSDS
008400051109     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
008500051109     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
008600051109     C                   CALL      'TIBS69R'
008700051109     C                   PARM                    tibs69ds
008800051109     C                   PARM                    ACODS
008900051109     C                   PARM                    INDDS
009000051109     C                   PARM                    CLPDS
009100051109     C                   PARM                    CLSDS
009200000000     C*------------------------------------------------------------------------*
009300000000     C* SETFIL - POSIZIONAMENTO SUL FILE
009400000000     C*------------------------------------------------------------------------*
009500000000     C     SETFIL        BEGSR
009600000000     C*
009700000000     C* POSIZIONAMENTO E PRIMA LETTURA
009800000000     C*
009900051108     C                   movel     pardda        KTAAAS
010000051108     C                   move      pardda        KTAMGS
010100000000     C                   Z-ADD     *ZEROS        KTAKSC
010200991123     C*
010300051109     C     KEYTAS        SETLL     TITAS38C                           99        *FINE FILE
010400000000     C*
010500000000     C     *IN99         IFEQ      '1'
010600000000     C                   MOVEL     '1'           FINL00
010700000000     C                   ELSE
010800000000     C                   EXSR      LETFIL
010900000000     C                   END
011000000000     C*
011100000000     C                   ENDSR
011200000000     C*------------------------------------------------------------------------*
011300000000     C* LETFIL - LETTURA PROSSIMO RECORD
011400000000     C*------------------------------------------------------------------------*
011500000000     C     LETFIL        BEGSR
011600000000     C*
011700000000     C* LEGGO FINO A:
011800000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
011900000000     C*     - TROVATO RECORD VALIDO
012000000000     C                   MOVEL     'N'           WRECOK
012100000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
012200000000     C     WRECOK        ANDEQ     'N'
012300000000     C*
012400000000     C* LETTURE SUCCESSIVE ALLA PRIMA
012500000000     C                   READ      TITAS38C                               99
012600000000     C*
012700000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
012800051109     C     TASAAS        ORGT      PARAal                                       *SUPERATO RANGE PERI
012900051109     C     TASMGS        ORGT      parmga
013000000000     C                   MOVEL     '1'           FINL00
013100000000     C                   ELSE
013200000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
013300000000     C                   END
013400000000     C*
013500000000     C                   END                                                    <-- 1 ---
013600000000     C*
013700000000     C                   ENDSR
013800000000     C*------------------------------------------------------------------------*
013900000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
014000000000     C*------------------------------------------------------------------------*
014100000000     C     CHKREC        BEGSR
014200000000     C*
014300000000     C                   MOVEL     'S'           WRECOK            1
014400040305     C*
014500051109     C* ESCLUDE LE BOLLE di recupero
014600051109     C                   Z-ADD     1             i
014700000000     C                   SETOFF                                           28
014800051109     C     TASTBL        LOOKUP    TB(i)                                  28
014900051109     C                   IF        *in28 = *on
015000000000     C                   MOVEL     'N'           WRECOK
015100000000     C                   ENDIF
015200000000     C*
015300051109     C* Escludo se bolla non oltre 20 colli
015400000000     C     WRECOK        IFEQ      'S'
015500051109     c                   if        tasncl <= 20
015600051109     C                   MOVEL     'N'           WRECOK
015700051109     c                   else
015800051109     c                   if        %subst(tasgva:1:1) =  'B' or
015900051109     c                             %Subst(tasgva:1:1) =  'O'
016000051109     C                   MOVEL     'N'           WRECOK
016100051109     c                   endif
016200051109     c                   endif
016300000000     C                   ENDIF
016400040220     C*
016500051109     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE
016600051109     C     WRECOK        IFEQ      'S'
016700040220     C                   EXSR      DECCLI
016800000000     C*
016900000000     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
017000000000     C                   EXSR      DIVARE
017100051109     c* Recupera soluzione vas se non è diskB vedo se diskA+C altrimenti è
017200051109     c* no disk
017300051109     c                   clear                   wsol              1
017400051109     c                   if        tasnrs > *zeros
017500051109     c                   eval      wsol = 'B'
017600051109     c                   else
017700051109     c     tasctm        lookup    ct                                     99
017800051109     c                   if        *in99 = *on
017900051109     c                   eval      wsol = 'C'
018000051109     c                   endif
018100051109     c                   endif
018200051109     c
018300000000     C                   ENDIF
018400011112     C*
018500000000     C                   ENDSR
018600000000     C*------------------------------------------------------------------------*
018700000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
018800000000     C*------------------------------------------------------------------------*
018900000000     C     CREARE        BEGSR
019000000000     C*
019100051109     C* Scrivo o riscrivo file di work per statistica
019200000000     C*
019300051109     C     KEYWFO        CHAIN     WFO2001L                           99
019400000000     C*
019500000000     C* RIEMPIO BUFFER RECORD
019600000000     C*
019700051109     C                   Z-ADD     pardda        o20dda
019800051109     C                   Z-ADD     pardal        o20dal
019900051109     C                   mOVEL     wdiv          o20div
020000051109     C                   Z-ADD     ware          o20ARE
020100051109     C                   Z-ADD     taslnp        o20lnp
020200051109     C                   Z-ADD     tasccm        o20ccm
020300051109     C   99              EXSR      AZZWFO                                       *RE-INIZIALIZZA BUFF
020400051109     C                   EXSR      ADDWFO                                       *AGGIUNGE SPED.AL RE
020500051109     C   99              WRITE     WFO20000                                     *RECORD NON ESISTE,
020600051109     C  N99              UPDATE    WFO20000                                     *RECORD ESISTE, AGGI
020700991025     C*
020800000000     C                   ENDSR
020900000000     C*------------------------------------------------------------------------*
021000051109     C* ADDWFO - Incrementa totali
021100000000     C*------------------------------------------------------------------------*
021200051109     C     ADDWFO        BEGSR
021300991025     C*
021400051109     C                   ADD       1             o20tsp
021500051109     C                   ADD       TASNCL        o20TCL
021600051122     c                   select
021700051122     c                   when      wsol = 'B'
021800051122     C                   ADD       1             o20tspb
021900051122     C                   ADD       TASNCL        o20TCLb
022000051122     c                   when      wsol = 'C'
022100051122     C                   ADD       1             o20tspc
022200051122     C                   ADD       TASNCL        o20TCLc
022300051122     c                   other
022400051122     C                   ADD       1             o20tspa
022500051122     C                   ADD       TASNCL        o20TCLa
022600051122     c                   endsl
022700991025     C*
022800000000     C                   ENDSR
022900000000     C*------------------------------------------------------------------------*
023000051109     C* AZZWFO - AZZERA CAMPI di totale
023100000000     C*------------------------------------------------------------------------*
023200051109     C     AZZWFO        BEGSR
023300000000     C*
023400051109     C                   Z-ADD     *ZEROS        o20TCL
023500051109     C                   Z-ADD     *ZEROS        o20tsp
023600051122     C                   z-add     *zeros        o20tspb
023700051122     C                   z-add     *zeros        o20TCLb
023800051122     C                   z-add     *zeros        o20tspc
023900051122     C                   z-add     *zeros        o20TCLc
024000051122     C                   z-add     *zeros        o20tspa
024100051122     C                   z-add     *zeros        o20TCLa
024200000000     C*
024300000000     C                   ENDSR
024400040220     C*------------------------------------------------------------------------*
024500040220     C* DECCLI - DECODIFICA CLIENTE
024600040220     C*------------------------------------------------------------------------*
024700040220     C     DECCLI        BEGSR
024800040220     C*
024900051108     C                   CLEAR                   tibs69ds
025000040220     C                   CLEAR                   ACODS
025100040220     C                   MOVEL     KNSIF         I69SIF                         *S.I.
025200051109     C                   Z-ADD     tasccm        I69KAC                         *CLIENTE X CNACO
025300040220     C                   CALL      'TIBS69R'
025400051108     C                   PARM                    tibs69ds
025500040220     C                   PARM                    ACODS
025600040220     C                   PARM                    INDDS
025700040220     C                   PARM                    CLPDS
025800040220     C                   PARM                    CLSDS
025900040220     C     O69ERR        IFNE      '1'                                          *NO ERRORE
026000051109     C                   MOVEL     ACORAG        o20RAG
026100040220     C                   ELSE
026200051109     C                   MOVEL     *ALL'*'       o20rAG
026300040220     C                   ENDIF
026400040220     C*
026500040220     C                   ENDSR
026600000000     C*------------------------------------------------------------------------*
026700000000     C* DIVARE - RECUPERA AREA E DIVISIONE
026800000000     C*------------------------------------------------------------------------*
026900000000     C     DIVARE        BEGSR
027000000000     C*
027100000000     C                   Z-ADD     1             I
027200051109     C     taslnp        LOOKUP    FIL(I)                                 99
027300000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
027400000000     C                   MOVEL     '*'           WDIV              1
027500000000     C                   Z-ADD     999           WARE              3 0
027600000000     C                   ELSE                                                   <-- 3 -->
027700000000     C                   MOVEL     DIV(I)        WDIV
027800000000     C                   Z-ADD     ARE(I)        WARE
027900051109     c                   movel     ddiv(i)       o20ddi
028000051109     c                   movel     dare(i)       o20dar
028100051109     c                   movel     dfil(i)       o20dln
028200000000     C                   END                                                    <-- 3 ---
028300000000     C*
028400000000     C                   ENDSR
028500000000     C*------------------------------------------------------------------------*
028600000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
028700000000     C*------------------------------------------------------------------------*
028800000000     C     CARORG        BEGSR
028900000000     C*
029000000000     C                   Z-ADD     *ZEROS        KORFIL
029100000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
029200000000     C  N99              READ      AZORG01L                               99
029300000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
029400000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
029500000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
029600000000     C     ORGFAG        IFNE      'V'
029700051109     C                   ADD       1             I
029800000000     C                   Z-ADD     ORGFIL        FIL(I)
029900000000     C                   Z-ADD     ORGCAR        ARE(I)
030000000000     C                   MOVEL     ORGFL3        DIV(I)
030100051109     c* riempio schiere decodifiche p.o., distretto e area
030200051109     c* distretto
030300051109     C                   Z-ADD     CODUT         KTBUTE
030400051109     C                   MOVEL     '17'          Ktbcod
030500051109     C                   MOVEL(P)  ORGFL3        KKEY
030600051109     C     KTAB          CHAIN     TABEL00F                           30
030700051109    2C     *IN30         IFEQ      *OFF
030800051109     C     TBLFLG        ANDEQ     *BLANKS
030900051109     C                   MOVEL     TBLUNI        DS17
031000051109     C                   MOVEL     §17DES        ddiv(I)
031100051109     C                   ELSE
031200051109     C                   MOVE      *BLANKS       ddiv(I)
031300051109    2C                   END
031400051109     c* area
031500051109     C                   Z-ADD     CODUT         KTBUTE
031600051109     C                   MOVEL     '05'          Ktbcod
031700051109     C                   MOVEL(P)  ORGCAR        KKEY
031800051109     C     KTAB          CHAIN     TABEL00F                           30
031900051109    2C     *IN30         IFEQ      *OFF
032000051109     C     TBLFLG        ANDEQ     *BLANKS
032100051109     C                   MOVEL     TBLUNI        DS05
032200051109     C                   MOVEL     §05DES        dare(I)
032300051109     C                   ELSE
032400051109     C                   MOVE      *BLANKS       dare(I)
032500051109    2C                   END
032600051109     c* p.o.
032700051109     C                   MOVEl     orgdes        dfil(I)
032800051109     c
032900000000     C                   END
033000000000     C                   END                                                    <-- 2 ---
033100000000     C                   READ      AZORG01L                               99
033200000000     C                   END                                                    <-- 1 ---
033300000000     C*
033400000000     C                   ENDSR
033500000000     C*------------------------------------------------------------------------*
033600051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
033700000000     C*------------------------------------------------------------------------*
033800000000     C     CARTBL        BEGSR
033900000000     C*--------------------
034000051109     C* TIPI BOLLA di recupero
034100000000     C*--------------------
034200000000     C                   Z-ADD     0             I
034300000000     C                   Z-ADD     CODUT         KTBUTE
034400000000     C                   MOVEL     'TB'          KTBCOD
034500000000     C     KEYTAB        SETLL     TABEL00F
034600051109     C                   READe     TABEL00F                               99
034700000000     C     *IN99         DOWEQ     *OFF
034800000000     C     TBLFLG        IFEQ      *BLANKS
034900051109     C                   MOVEL     TBLUNI        DSTB
035000051109     c                   if        §tbrbl = 'R'
035100051109     C                   ADD       1             I
035200051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
035300051109     c                   endif
035400000000     C                   END
035500051109     C     keytab        READe     TABEL00F                               99
035600000000     C                   END
035700000000     C*
035800000000     C                   ENDSR
035900000000     C*------------------------------------------------------------------------*
036000051109     C* CANWFO - CANCELLA IL WFo2000F TUTTI I RECORD DEl periodo richiesto
036100000000     C*------------------------------------------------------------------------*
036200051109     C     CANWFO        BEGSR
036300000000     C*
036400000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
036500051109     C     KEYWFod       SETLL     WFo2001L
036600051109     C     keywfod       READe     WFo2001L                               99
036700000000     C*
036800051109     C* CICLO FINO A FINE FILE
036900000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
037000000000     C*
037100051109     C* ELIMINA RECORD SE ALL'INTERNO DEL periodo richiesto
037200051109     C                   DELETE    WFo20000
037300000000     C*
037400000000     C* LEGGE IL RECORD SUCCESSIVO
037500051109     C     keywfod       READe     WFo20000                               99
037600000000     C*
037700051109     C                   ENDdo                                                  <-- 1 ---
037800000000     C*
037900000000     C                   ENDSR
038000000000     C*------------------------------------------------------------------------*
038100000000     C* *INZSR - ROUTINE INIZIALE
038200000000     C*------------------------------------------------------------------------*
038300000000     C     *INZSR        BEGSR
038400000000     C*--------------------
038500000000     C* RICEVIMENTO PARAMETRI
038600000000     C*--------------------
038700000000     C     *ENTRY        PLIST
038800000000     C                   PARM                    KPJBA
038900000000     C                   MOVEL     KPJBU         PARAM
039000000000     C*--------------------
039100000000     C* CAMPI RIFERITI AL DATA BASE
039200000000     C*--------------------
039300000000     C     *LIKE         DEFINE    TASAAS        KTAAAS
039400000000     C     *LIKE         DEFINE    TASMGS        KTAMGS
039500000000     C     *LIKE         DEFINE    TASKSC        KTAKSC
039600000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
039700000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
039800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
039900051109     C     *LIKE         DEFINE    TBLkey        Kkey
040000000000     C*--------------------
040100000000     C* CHIAVI DI LETTURA
040200000000     C*--------------------
040300000000     C* CHIAVE LETTURA TITAS38C - COMPLETA
040400000000     C     KEYTAS        KLIST
040500000000     C                   KFLD                    KTAAAS
040600000000     C                   KFLD                    KTAMGS
040700000000     C                   KFLD                    KTAKSC
040800000000     C*
040900000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
041000000000     C     KEYORG        KLIST
041100000000     C                   KFLD                    KORFIL
041200000000     C*
041300000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
041400000000     C     KEYTAB        KLIST
041500000000     C                   KFLD                    KTBUTE
041600000000     C                   KFLD                    KTBCOD
041700051109     C     KTAB          KLIST
041800051109     C                   KFLD                    ktbute
041900051109     C                   KFLD                    ktbcod
042000051109     C                   KFLD                    KKEY
042100000000     C*
042200051109     C* CHIAVE LETTURA WFO2001L - COMPLETA
042300051109     C     KEYWFO        KLIST
042400051109     C                   KFLD                    pardda
042500051109     C                   KFLD                    pardal
042600051109     C                   KFLD                    wdiv
042700051109     C                   KFLD                    ware
042800051109     C                   KFLD                    taslnp
042900051109     C                   KFLD                    tasccm
043000051109     C     KEYWFOd       KLIST
043100051109     C                   KFLD                    pardda
043200051109     C                   KFLD                    pardal
043300000000     C*--------------------
043400000000     C* CAMPI DI CONTROLLO
043500000000     C*--------------------
043600000000     C                   MOVEL     '0'           FINL00            1
043700000000     C                   Z-ADD     1             CODUT             1 0
043800000000     C*
043900000000     C***
044000000000     C* CARICA ORGANIGRAMMA
044100000000     C                   EXSR      CARORG
044200000000     C*
044300000000     C* CARICA TIPI BOLLA
044400000000     C                   EXSR      CARTBL
044500000000     C*
044600051117     C* CANCELLA TUTTI I RECORD DA WFO2000F
044700051117     C**                 EXSR      CANWFO
044800051109     c* CARICA CTM DiskC CLIENTE
044900051109     C                   Z-ADD     0             I
045000051109     C                   Z-ADD     CODUT         KTBUTE
045100051109     C                   MOVEL     '1B'          KTBCOD
045200051109     C     KEYTAB        SETLL     TABEL00F
045300051109     C                   READe     TABEL00F                               99
045400051109     C     *IN99         DOWEQ     *OFF
045500051109     C     TBLFLG        IFEQ      *BLANKS
045600051109     C                   MOVEL     TBLUNI        DS1B
045700051109     c                   if        §1bdkc = 'C'
045800051109     C                   ADD       1             I
045900051109     C                   MOVEL     TBLKEY        CT(I)                          *TIPO BOLLA
046000051109     c                   endif
046100051109     C                   END
046200051109     C     keytab        READe     TABEL00F                               99
046300051109     C                   END
046400000000     C*
046500000000     C                   ENDSR
