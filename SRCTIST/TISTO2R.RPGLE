000100000000     H*------------------------------------------------------------------------*
000200051108     H* Statistica spedizioni oltre 20 colli - Crea Work file
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     FTITAS38C  IF   E           K DISK
000600060524     Ffiar531c  IF   E           K DISK
000700000000     FAZORG01L  IF   E           K DISK
000800000000     FTABEL00F  IF   E           K DISK
000900051108     FWFo2001l  UF A E           K DISK
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001500000000     D*-------------------
001600000000     D FIL             S              3  0 DIM(500)                             *FILIALE
001700000000     D ARE             S              3  0 DIM(500)                             *AREA
001800000000     D DIV             S              1    DIM(500)                             *DIVISIONE
001900051109     D dfil            S             20    DIM(500)                             *FILIALE
002000051109     D dare            S             25    DIM(500)                             *AREA
002100051109     D ddiv            S             25    DIM(500)                             *DIVISIONE
002200000000     D*-------------------
002300000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
002400051109     D CT              S              2    DIM(50)                              TIP0 BOLLA
002500000000     D*------------------------------------------------------------------------*
002600000000     D* INPUT
002700000000     D*------------------------------------------------------------------------*
002800000000     D*-------------------
002900000000     D* ARCHITETTURA
003000000000     D*-------------------
003100000000     D KPJBA         E DS
003200000000     D*-------------------
003300000000     D* DS TIPO BOLLA
003400000000     D*-------------------
003500000000     D DSTB          E DS
003600051109     D*-------------------
003700051109     D DS05          E DS
003800051109     D DS17          E DS
003900051109     D DS1b          E DS
004000040220     D*------------------
004100040220     D* DS REPERIMENTO DATI UTENTE
004200040220     D*-------------------
004300051108     d Tibs69ds      e ds
004400040220     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
004500040220     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
004600040220     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
004700040220     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
004800060524     D dar5bnb       E DS
004900000000     D*-------------------
005000000000     D* DS PARAMETRI DI LANCIO PGM
005100000000     D*-------------------
005200000000     D PARAM           DS
005300051108     D  PARdda                 1      8  0
005400051108     D  PARdal                 9     16  0
005500051109     D  PARaal                 9     12  0
005600051109     D  PARmga                13     16  0
005700060524     d
005800060524     d KAr5Trd         s                   Like(Ar5Trd) inz('BNB')
005900000000     C*------------------------------------------------------------------------*
006000000000     C* MAIN LINE
006100000000     C*------------------------------------------------------------------------*
006200000000     C*
006300000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
006400000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
006500000000     C*
006600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
006700000000     C                   EXSR      SETFIL
006800000000     C*
006900000000     C* CICLO FINO A FINE FILE
007000000000     C     FINL00        DOWEQ     '0'                                          --- 2 -->
007100000000     C*
007200000000     C* CREA IL RECORD SUI FILE DI LAVORO
007300000000     C                   EXSR      CREARE
007400000000     C*
007500000000     C* LEGGE IL RECORD SUCCESSIVO
007600000000     C                   EXSR      LETFIL
007700000000     C                   ENDDO                                                  <-- 2 ---
007800000000     C*
007900000000     C                   ENDIF                                                  <-- 1 ---
008000000000     C*
008100000000     C                   SETON                                        LR
008200051109     C* CHIUDE I FILE DEI PGM CHIAMATI
008300051109     C                   CLEAR                   tibs69ds
008400051109     C                   CLEAR                   ACODS
008500051109     C                   CLEAR                   INDDS
008600051109     C                   CLEAR                   CLPDS
008700051109     C                   CLEAR                   CLSDS
008800051109     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
008900051109     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
009000051109     C                   CALL      'TIBS69R'
009100051109     C                   PARM                    tibs69ds
009200051109     C                   PARM                    ACODS
009300051109     C                   PARM                    INDDS
009400051109     C                   PARM                    CLPDS
009500051109     C                   PARM                    CLSDS
009600000000     C*------------------------------------------------------------------------*
009700000000     C* SETFIL - POSIZIONAMENTO SUL FILE
009800000000     C*------------------------------------------------------------------------*
009900000000     C     SETFIL        BEGSR
010000000000     C*
010100000000     C* POSIZIONAMENTO E PRIMA LETTURA
010200000000     C*
010300051108     C                   movel     pardda        KTAAAS
010400051108     C                   move      pardda        KTAMGS
010500000000     C                   Z-ADD     *ZEROS        KTAKSC
010600991123     C*
010700051109     C     KEYTAS        SETLL     TITAS38C                           99        *FINE FILE
010800000000     C*
010900000000     C     *IN99         IFEQ      '1'
011000000000     C                   MOVEL     '1'           FINL00
011100000000     C                   ELSE
011200000000     C                   EXSR      LETFIL
011300000000     C                   END
011400000000     C*
011500000000     C                   ENDSR
011600000000     C*------------------------------------------------------------------------*
011700000000     C* LETFIL - LETTURA PROSSIMO RECORD
011800000000     C*------------------------------------------------------------------------*
011900000000     C     LETFIL        BEGSR
012000000000     C*
012100000000     C* LEGGO FINO A:
012200000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
012300000000     C*     - TROVATO RECORD VALIDO
012400000000     C                   MOVEL     'N'           WRECOK
012500000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
012600000000     C     WRECOK        ANDEQ     'N'
012700000000     C*
012800000000     C* LETTURE SUCCESSIVE ALLA PRIMA
012900000000     C                   READ      TITAS38C                               99
013000000000     C*
013100000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
013200051109     C     TASAAS        ORGT      PARAal                                       *SUPERATO RANGE PERI
013300051109     C     TASMGS        ORGT      parmga
013400000000     C                   MOVEL     '1'           FINL00
013500000000     C                   ELSE
013600000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
013700000000     C                   END
013800000000     C*
013900000000     C                   END                                                    <-- 1 ---
014000000000     C*
014100000000     C                   ENDSR
014200000000     C*------------------------------------------------------------------------*
014300000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
014400000000     C*------------------------------------------------------------------------*
014500000000     C     CHKREC        BEGSR
014600000000     C*
014700000000     C                   MOVEL     'S'           WRECOK            1
014800040305     C*
014900051109     C* ESCLUDE LE BOLLE di recupero
015000051109     C                   Z-ADD     1             i
015100000000     C                   SETOFF                                           28
015200051109     C     TASTBL        LOOKUP    TB(i)                                  28
015300051109     C                   IF        *in28 = *on
015400000000     C                   MOVEL     'N'           WRECOK
015500000000     C                   ENDIF
015600000000     C*
015700060524     C*
015800000000     C     WRECOK        IFEQ      'S'
015900060524     c***                if        tasncl <= 20
016000060524     C***                MOVEL     'N'           WRECOK
016100060524     c***                else
016200060524     c***                if        %subst(tasgva:1:1) =  'B' or
016300060524     c***                          %Subst(tasgva:1:1) =  'O'
016400060524     C***                MOVEL     'N'           WRECOK
016500060524     c***                endif
016600060524     c***                endif
016700060524     c                   select
016800060524     c* se bancalato con "B" escludo sempre
016900060524     c                   when      %Subst(tasgva:1:1) =  'B'
017000060524     C                   MOVEL     'N'           WRECOK
017100060524     c* se bancalato con "O" escludo se serie > 0
017200060524     c                   when      %Subst(tasgva:1:1) =  'O' and tasnrs > 0
017300060524     C                   MOVEL     'N'           WRECOK
017400060524     c* se non bancalato escludo se numero colli <= 20
017500060524     c                   when      %Subst(tasgva:1:1) <> 'O' and tasncl<=20
017600060524     C                   MOVEL     'N'           WRECOK
017700060524     c                   endsl
017800000000     C                   ENDIF
017900040220     C*
018000051109     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE
018100051109     C     WRECOK        IFEQ      'S'
018200040220     C                   EXSR      DECCLI
018300000000     C*
018400000000     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
018500000000     C                   EXSR      DIVARE
018600051109     c* Recupera soluzione vas se non è diskB vedo se diskA+C altrimenti è
018700051109     c* no disk
018800060525     c                   if        %Subst(tasgva:1:1) <> 'O'
018900051109     c                   clear                   wsol              1
019000051109     c                   if        tasnrs > *zeros
019100051109     c                   eval      wsol = 'B'
019200051109     c                   else
019300051109     c     tasctm        lookup    ct                                     99
019400051109     c                   if        *in99 = *on
019500051109     c                   eval      wsol = 'C'
019600051109     c                   endif
019700051109     c                   endif
019800060525     c                   endif
019900051109     c
020000000000     C                   ENDIF
020100011112     C*
020200000000     C                   ENDSR
020300000000     C*------------------------------------------------------------------------*
020400000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
020500000000     C*------------------------------------------------------------------------*
020600000000     C     CREARE        BEGSR
020700000000     C*
020800051109     C* Scrivo o riscrivo file di work per statistica
020900000000     C*
021000051109     C     KEYWFO        CHAIN     WFO2001L                           99
021100000000     C*
021200000000     C* RIEMPIO BUFFER RECORD
021300000000     C*
021400051109     C                   Z-ADD     pardda        o20dda
021500051109     C                   Z-ADD     pardal        o20dal
021600051109     C                   mOVEL     wdiv          o20div
021700051109     C                   Z-ADD     ware          o20ARE
021800051109     C                   Z-ADD     taslnp        o20lnp
021900051109     C                   Z-ADD     tasccm        o20ccm
022000051109     C   99              EXSR      AZZWFO                                       *RE-INIZIALIZZA BUFF
022100051109     C                   EXSR      ADDWFO                                       *AGGIUNGE SPED.AL RE
022200051109     C   99              WRITE     WFO20000                                     *RECORD NON ESISTE,
022300051109     C  N99              UPDATE    WFO20000                                     *RECORD ESISTE, AGGI
022400991025     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700051109     C* ADDWFO - Incrementa totali
022800000000     C*------------------------------------------------------------------------*
022900051109     C     ADDWFO        BEGSR
023000991025     C*
023100060524     c                   if        %Subst(tasgva:1:1) <> 'O'
023200051109     C                   ADD       1             o20tsp
023300051109     C                   ADD       TASNCL        o20TCL
023400051122     c                   select
023500051122     c                   when      wsol = 'B'
023600051122     C                   ADD       1             o20tspb
023700051122     C                   ADD       TASNCL        o20TCLb
023800051122     c                   when      wsol = 'C'
023900051122     C                   ADD       1             o20tspc
024000051122     C                   ADD       TASNCL        o20TCLc
024100051122     c                   other
024200051122     C                   ADD       1             o20tspa
024300051122     C                   ADD       TASNCL        o20TCLa
024400051122     c                   endsl
024500060524     c                   else
024600060524     C                   ADD       1             o20tspo
024700060524     c* colli della spedizione
024800060524     C                   ADD       TASNCL        o20TCso
024900060524     c* colli originali
025000060524     c                   clear                   dar5bnb
025100060524     c     kar5          chain     fiar531c
025200060524     c                   if        %found(fiar531c)
025300060524     c                   movel     ar5uni        dar5bnb
025400060524     c                   endif
025500060524     C                   ADD       §AR5BCOR      o20TCoo
025600060524     c                   endif
025700991025     C*
025800000000     C                   ENDSR
025900000000     C*------------------------------------------------------------------------*
026000051109     C* AZZWFO - AZZERA CAMPI di totale
026100000000     C*------------------------------------------------------------------------*
026200051109     C     AZZWFO        BEGSR
026300000000     C*
026400051109     C                   Z-ADD     *ZEROS        o20TCL
026500051109     C                   Z-ADD     *ZEROS        o20tsp
026600051122     C                   z-add     *zeros        o20tspb
026700051122     C                   z-add     *zeros        o20TCLb
026800051122     C                   z-add     *zeros        o20tspc
026900051122     C                   z-add     *zeros        o20TCLc
027000051122     C                   z-add     *zeros        o20tspa
027100051122     C                   z-add     *zeros        o20TCLa
027200060524     C                   Z-ADD     *ZEROS        o20Tspo
027300060524     C                   Z-ADD     *ZEROS        o20tcso
027400060524     C                   Z-ADD     *ZEROS        o20tcoo
027500000000     C*
027600000000     C                   ENDSR
027700040220     C*------------------------------------------------------------------------*
027800040220     C* DECCLI - DECODIFICA CLIENTE
027900040220     C*------------------------------------------------------------------------*
028000040220     C     DECCLI        BEGSR
028100040220     C*
028200051108     C                   CLEAR                   tibs69ds
028300040220     C                   CLEAR                   ACODS
028400040220     C                   MOVEL     KNSIF         I69SIF                         *S.I.
028500051109     C                   Z-ADD     tasccm        I69KAC                         *CLIENTE X CNACO
028600040220     C                   CALL      'TIBS69R'
028700051108     C                   PARM                    tibs69ds
028800040220     C                   PARM                    ACODS
028900040220     C                   PARM                    INDDS
029000040220     C                   PARM                    CLPDS
029100040220     C                   PARM                    CLSDS
029200040220     C     O69ERR        IFNE      '1'                                          *NO ERRORE
029300051109     C                   MOVEL     ACORAG        o20RAG
029400040220     C                   ELSE
029500051109     C                   MOVEL     *ALL'*'       o20rAG
029600040220     C                   ENDIF
029700040220     C*
029800040220     C                   ENDSR
029900000000     C*------------------------------------------------------------------------*
030000000000     C* DIVARE - RECUPERA AREA E DIVISIONE
030100000000     C*------------------------------------------------------------------------*
030200000000     C     DIVARE        BEGSR
030300000000     C*
030400000000     C                   Z-ADD     1             I
030500051109     C     taslnp        LOOKUP    FIL(I)                                 99
030600000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
030700000000     C                   MOVEL     '*'           WDIV              1
030800000000     C                   Z-ADD     999           WARE              3 0
030900000000     C                   ELSE                                                   <-- 3 -->
031000000000     C                   MOVEL     DIV(I)        WDIV
031100000000     C                   Z-ADD     ARE(I)        WARE
031200051109     c                   movel     ddiv(i)       o20ddi
031300051109     c                   movel     dare(i)       o20dar
031400051109     c                   movel     dfil(i)       o20dln
031500000000     C                   END                                                    <-- 3 ---
031600000000     C*
031700000000     C                   ENDSR
031800000000     C*------------------------------------------------------------------------*
031900000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
032000000000     C*------------------------------------------------------------------------*
032100000000     C     CARORG        BEGSR
032200000000     C*
032300000000     C                   Z-ADD     *ZEROS        KORFIL
032400000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
032500000000     C  N99              READ      AZORG01L                               99
032600000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
032700000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
032800000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
032900000000     C     ORGFAG        IFNE      'V'
033000051109     C                   ADD       1             I
033100000000     C                   Z-ADD     ORGFIL        FIL(I)
033200000000     C                   Z-ADD     ORGCAR        ARE(I)
033300000000     C                   MOVEL     ORGFL3        DIV(I)
033400051109     c* riempio schiere decodifiche p.o., distretto e area
033500051109     c* distretto
033600051109     C                   Z-ADD     CODUT         KTBUTE
033700051109     C                   MOVEL     '17'          Ktbcod
033800051109     C                   MOVEL(P)  ORGFL3        KKEY
033900051109     C     KTAB          CHAIN     TABEL00F                           30
034000051109    2C     *IN30         IFEQ      *OFF
034100051109     C     TBLFLG        ANDEQ     *BLANKS
034200051109     C                   MOVEL     TBLUNI        DS17
034300051109     C                   MOVEL     §17DES        ddiv(I)
034400051109     C                   ELSE
034500051109     C                   MOVE      *BLANKS       ddiv(I)
034600051109    2C                   END
034700051109     c* area
034800051109     C                   Z-ADD     CODUT         KTBUTE
034900051109     C                   MOVEL     '05'          Ktbcod
035000051109     C                   MOVEL(P)  ORGCAR        KKEY
035100051109     C     KTAB          CHAIN     TABEL00F                           30
035200051109    2C     *IN30         IFEQ      *OFF
035300051109     C     TBLFLG        ANDEQ     *BLANKS
035400051109     C                   MOVEL     TBLUNI        DS05
035500051109     C                   MOVEL     §05DES        dare(I)
035600051109     C                   ELSE
035700051109     C                   MOVE      *BLANKS       dare(I)
035800051109    2C                   END
035900051109     c* p.o.
036000051109     C                   MOVEl     orgdes        dfil(I)
036100051109     c
036200000000     C                   END
036300000000     C                   END                                                    <-- 2 ---
036400000000     C                   READ      AZORG01L                               99
036500000000     C                   END                                                    <-- 1 ---
036600000000     C*
036700000000     C                   ENDSR
036800000000     C*------------------------------------------------------------------------*
036900051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
037000000000     C*------------------------------------------------------------------------*
037100000000     C     CARTBL        BEGSR
037200000000     C*--------------------
037300051109     C* TIPI BOLLA di recupero
037400000000     C*--------------------
037500000000     C                   Z-ADD     0             I
037600000000     C                   Z-ADD     CODUT         KTBUTE
037700000000     C                   MOVEL     'TB'          KTBCOD
037800000000     C     KEYTAB        SETLL     TABEL00F
037900051109     C                   READe     TABEL00F                               99
038000000000     C     *IN99         DOWEQ     *OFF
038100000000     C     TBLFLG        IFEQ      *BLANKS
038200051109     C                   MOVEL     TBLUNI        DSTB
038300051109     c                   if        §tbrbl = 'R'
038400051109     C                   ADD       1             I
038500051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
038600051109     c                   endif
038700000000     C                   END
038800051109     C     keytab        READe     TABEL00F                               99
038900000000     C                   END
039000000000     C*
039100000000     C                   ENDSR
039200000000     C*------------------------------------------------------------------------*
039300051109     C* CANWFO - CANCELLA IL WFo2000F TUTTI I RECORD DEl periodo richiesto
039400000000     C*------------------------------------------------------------------------*
039500051109     C     CANWFO        BEGSR
039600000000     C*
039700000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
039800051109     C     KEYWFod       SETLL     WFo2001L
039900051109     C     keywfod       READe     WFo2001L                               99
040000000000     C*
040100051109     C* CICLO FINO A FINE FILE
040200000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
040300000000     C*
040400051109     C* ELIMINA RECORD SE ALL'INTERNO DEL periodo richiesto
040500051109     C                   DELETE    WFo20000
040600000000     C*
040700000000     C* LEGGE IL RECORD SUCCESSIVO
040800051109     C     keywfod       READe     WFo20000                               99
040900000000     C*
041000051109     C                   ENDdo                                                  <-- 1 ---
041100000000     C*
041200000000     C                   ENDSR
041300000000     C*------------------------------------------------------------------------*
041400000000     C* *INZSR - ROUTINE INIZIALE
041500000000     C*------------------------------------------------------------------------*
041600000000     C     *INZSR        BEGSR
041700000000     C*--------------------
041800000000     C* RICEVIMENTO PARAMETRI
041900000000     C*--------------------
042000000000     C     *ENTRY        PLIST
042100000000     C                   PARM                    KPJBA
042200000000     C                   MOVEL     KPJBU         PARAM
042300000000     C*--------------------
042400000000     C* CAMPI RIFERITI AL DATA BASE
042500000000     C*--------------------
042600000000     C     *LIKE         DEFINE    TASAAS        KTAAAS
042700000000     C     *LIKE         DEFINE    TASMGS        KTAMGS
042800000000     C     *LIKE         DEFINE    TASKSC        KTAKSC
042900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
043000000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
043100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
043200051109     C     *LIKE         DEFINE    TBLkey        Kkey
043300000000     C*--------------------
043400000000     C* CHIAVI DI LETTURA
043500000000     C*--------------------
043600000000     C* CHIAVE LETTURA TITAS38C - COMPLETA
043700000000     C     KEYTAS        KLIST
043800000000     C                   KFLD                    KTAAAS
043900000000     C                   KFLD                    KTAMGS
044000000000     C                   KFLD                    KTAKSC
044100000000     C*
044200000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
044300000000     C     KEYORG        KLIST
044400000000     C                   KFLD                    KORFIL
044500000000     C*
044600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
044700000000     C     KEYTAB        KLIST
044800000000     C                   KFLD                    KTBUTE
044900000000     C                   KFLD                    KTBCOD
045000051109     C     KTAB          KLIST
045100051109     C                   KFLD                    ktbute
045200051109     C                   KFLD                    ktbcod
045300051109     C                   KFLD                    KKEY
045400000000     C*
045500051109     C* CHIAVE LETTURA WFO2001L - COMPLETA
045600051109     C     KEYWFO        KLIST
045700051109     C                   KFLD                    pardda
045800051109     C                   KFLD                    pardal
045900051109     C                   KFLD                    wdiv
046000051109     C                   KFLD                    ware
046100051109     C                   KFLD                    taslnp
046200051109     C                   KFLD                    tasccm
046300051109     C     KEYWFOd       KLIST
046400051109     C                   KFLD                    pardda
046500051109     C                   KFLD                    pardal
046600060524     C     Kar5          KLIST
046700060524     C                   KFLD                    tasAAS
046800060524     C                   KFLD                    taslnp
046900060524     C                   KFLD                    tasnrs
047000060524     C                   KFLD                    tasnsp
047100060524     c                   kfld                    kar5trd
047200000000     C*--------------------
047300000000     C* CAMPI DI CONTROLLO
047400000000     C*--------------------
047500000000     C                   MOVEL     '0'           FINL00            1
047600000000     C                   Z-ADD     1             CODUT             1 0
047700000000     C*
047800000000     C***
047900000000     C* CARICA ORGANIGRAMMA
048000000000     C                   EXSR      CARORG
048100000000     C*
048200000000     C* CARICA TIPI BOLLA
048300000000     C                   EXSR      CARTBL
048400000000     C*
048500051117     C* CANCELLA TUTTI I RECORD DA WFO2000F
048600051117     C**                 EXSR      CANWFO
048700051109     c* CARICA CTM DiskC CLIENTE
048800051109     C                   Z-ADD     0             I
048900051109     C                   Z-ADD     CODUT         KTBUTE
049000051109     C                   MOVEL     '1B'          KTBCOD
049100051109     C     KEYTAB        SETLL     TABEL00F
049200051109     C                   READe     TABEL00F                               99
049300051109     C     *IN99         DOWEQ     *OFF
049400051109     C     TBLFLG        IFEQ      *BLANKS
049500051109     C                   MOVEL     TBLUNI        DS1B
049600051109     c                   if        §1bdkc = 'C'
049700051109     C                   ADD       1             I
049800051109     C                   MOVEL     TBLKEY        CT(I)                          *TIPO BOLLA
049900051109     c                   endif
050000051109     C                   END
050100051109     C     keytab        READe     TABEL00F                               99
050200051109     C                   END
050300000000     C*
050400000000     C                   ENDSR
