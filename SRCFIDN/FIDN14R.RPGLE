000100980909
000200980909      *------------------------------------------------------------------------*
000300980909      * GESTIONE ANAGRAFICA DATI FISSI PER DENUNCIE                            *
000400980909      *------------------------------------------------------------------------*
000500980909
000600980909      *--------------------------------------------
000700980909      *  RIEPILOGO INDICATORI
000800980909      *--------------------------------------------
000900980909      * 01 - Elaboro record annullato
001000980909      * 02 - Acceso INSERIMENTO, Spento MANUTENZIONE
001100980930      * 05 - SPROTEGGO P.O. IN GESTIONE
001200980909      * 28 - ERRORE generico, emissione campo messaggio
001300980909      * 30 - Comodo
001400980909      * 40 - ERRORE codice P.O.
001500980909      * 41 - ERRORE Autorità Rag. Soc.
001600980909      * 42 - ERRORE Autorità Indirizzo
001700980909      * 43 - ERRORE Autorità Località
001800980909      * 44 - ERRORE Autorità Provincia
001900980909      * 45 - ERRORE Autorità CAP
002000980909      * 46 - ERRORE Denunciante Rag. Soc.
002100980909      * 47 - ERRORE Denunciante Indirizzo
002200980909      * 48 - ERRORE Denunciante Località
002300980909      * 49 - ERRORE Denunciante Provincia
002400980909      * 50 - ERRORE Denunciante Data di nascita
002500980909      * 51 - ERRORE Denunciante Luogo di nascita
002600980909      * 52 - ERRORE Denunciante Provincia di nascita
002700980909      * 53 - ERRORE Denunciante Mansione
002800021022     *** * 54 - ERRORE Constatavo ......
002900021022     *** * 55 - ERRORE Modulo Denuncia
003000021024      * 56 - libero
003100021022      * 57 - ERRORE Addetto al magazzino
003200021022      * 58 - ERRORE Addetto all'ufficio assistenza clienti
003300980909      *--------------------------------------------
003400980909
003500980506     H DECEDIT('0,') DATEDIT(*DMY.)
003600980909
003700980909     FFIDN14D   CF   E             WORKSTN
003800980909     FFNDAD01L  UF A E           K DISK
003900980909     FAZORG01L  IF   E           K DISK
004000980909
004100980930     D MSG             S             78    DIM(5) CTDATA PERRCD(1)              MSG VIDEO
004200980930     D L1              S              3  0 DIM(30)                              P.O.GESTITI
004300980507      *--------------------------------------------
004400980507     D KPJBA         E DS
004500020502     D* rem                    1      3
004600020502     D* remfil                 4      6
004700980930      *
004800980930     D TRUL06DS      E DS
004900980930     D  LIN                    1     90  0    DIM(30)                           P.O. COMODO
005000980930      *
005100980909     D DDAD          E DS
005200980508     D CNCR80        E DS
005300980507     D UT§DSE0F      E DS
005400980909      *
005500980909     D WLBDAT          DS                  INZ
005600980909     D  G02DAT                 1      8  0
005700980909     D  G02INV                 9     16  0
005800980909     D  G02ERR                17     17
005900980909     D  G02TGI                18     22  0
006000980507      *--------------------------------------------
006100980909     D DATA_eur        S               D   DATFMT(*eur)
006200980507     D W0140           S             14  0
006300980507     D WDTGIO          S              8  0
006400980507     D WORA            S              6  0
006500980909     D XX              S              3  0
006600980930      *--------------------------------------------
006700980909
006800980506      * EMISSIONE PRIMA VIDEATA
006900980506     C     EMD01         TAG
007000980909     C                   EXFMT     FI14D01
007100980930      *
007200980909     C                   clear                   V1CMSG
007300980930     C                   setoff                                       283040
007400980930      *
007500980506      * F3=FINE
007600980909     c   KC              goto      fine
007700980930      *
007800980506      * CONTROLLI VIDEATA
007900980506     C                   EXSR      CTRD01
008000980506      *
008100980909     C   28              goto      emd01
008200980506      *
008300980909      * IMPOSTO CAMPI SECONDA VIDEATA
008400980909     C                   EXSR      RIED02
008500980507      *
008600980507      * -------------------  GESTIONE SECONDA VIDEATA --------------------------
008700980506      *
008800980507      * EMISSIONE
008900980506     C     EMD02         TAG
009000980909     C                   EXFMT     FI14D02
009100980909      *
009200980506     C                   CLEAR                   V2CMSG
009300980507     C                   eval      *in28 = *off
009400980506      *
009500980506      * F3=FINE
009600980909     c   KC              goto      fine
009700980506      *
009800980507      * F12=RITORNO
009900980909     C   KL              GOTO      EMD01
010000980507      *
010100980506      * CONTROLLI VIDEATA
010200980506     C                   EXSR      CTRD02
010300980717      * errori
010400980909     C   28              GOTO      EMD02
010500980909      *
010600980909      * GESTIONE CONFERME: F5(ke)=Ripristino; F6(kf)=Conferma; F16(kq)=Annullamento
010700980909     c                   IF        *inKE = *ON  or  *inKF = *ON  or  *inKQ = *ON
010800980909     c                   exsr      RIEDAD
010900980909     C                   goto      EMD01
011000980909     c                   ENDIF
011100980909      *
011200980909     C                   goto      EMD02
011300980506      *
011400980506     C     FINE          TAG
011500980506     C                   EVAL      *INLR = *ON
011600980506      *****************************************************************
011700980506      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
011800980506      *****************************************************************
011900980506     C     CTRD01        BEGSR
012000980507      *
012100980930     C                   IF        V1Cfgs > *zeros
012200980930     c     V1Cfgs        Lookup    L1                                     30
012300980930     c  N30              seton                                        2840
012400980930     C  N30              movel     MSG(5)        V1CMSG
012500980930     c                   ELSE
012600980909      *
012700980930     c                   seton                                        2840
012800980930     C                   movel     MSG(1)        V1CMSG
012900980930     c                   ENDIF
013000980930      *
013100980506     C                   ENDSR
013200980506      *****************************************************************
013300980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
013400980506      *****************************************************************
013500980506     C     RIED02        BEGSR
013600980909      *
013700980909     c                   clear                   FNDAD000
013800980909     c                   clear                   DDAD
013900980909     c                   eval      *in01 = *off
014000980930      *
014100980930     c     V1Cfgs        CHAIN     AZORG                              30
014200980909      *
014300980930     c     V1Cfgs        CHAIN     FNDAD000                           02
014400980506      *
014500980909     c  N02              movel     DADdat        Ddad
014600980909     c                   eval      *in01 = (DADatb = 'A')
014700980909      *
014800980930     C                   movel     V1Cfgs        V2Ccpo
014900980909     C                   movel     ORGDES        V2Dcpo
015000980909     C                   movel     §DADaurs      V2Caurs
015100980909     C                   movel     §DADauin      V2Cauin
015200980909     C                   movel     §DADauca      V2Cauca
015300980909     C                   movel     §DADaulo      V2Caulo
015400980909     C                   movel     §DADaupr      V2Caupr
015500980910     C                   movel     §DADsors      V2Csors
015600980909     C                   movel     §DADsoin      V2Csoin
015700980909     C                   movel     §DADsolo      V2Csolo
015800980909     C                   movel     §DADsopr      V2Csopr
015900980909     C                   movel     §DADsoln      V2Csoln
016000980909     C                   movel     §DADsopn      V2Csopn
016100980909     C                   movel     §DADsoma      V2Csoma
016200021022     ***C                   movel     §DADcons      V2Ccons
016300021022     ***C                   movel     §DADmode      V2Cmode
016400021022     c                   movel     §DADamag      V2Camag
016500021022     c                   movel     §DADauac      V2Cauac
016600980910     C                   clear                   V2Csodn
016700980909     c                   if        §dadsodn > *zeros
016800980910     c     *iso          move      §DADsodn      DATA_eur
016900980910     c     *eur          move      DATA_eur      V2Csodn
017000980909     c                   endif
017100980506      *
017200980506     C                   ENDSR
017300980506      *****************************************************************
017400980506      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
017500980506      *****************************************************************
017600980506     C     CTRD02        BEGSR
017700980507      *
017800980909     c                   eval      *in41 = (V2Caurs = *blanks)
017900980909     c                   eval      *in42 = (V2Cauin = *blanks)
018000980930     c                   eval      *in43 = (V2Caulo = *blanks)
018100980930     c                   eval      *in44 = (V2Caupr = *blanks)
018200980930     c                   eval      *in45 = (V2Cauca = *blanks)
018300980909     c                   eval      *in46 = (V2Csors = *blanks)
018400980909     c                   eval      *in47 = (V2Csoin = *blanks)
018500980909     c                   eval      *in48 = (V2Csolo = *blanks)
018600980909     c                   eval      *in49 = (V2Csopr = *blanks)
018700980930     c                   eval      *in51 = (V2Csoln = *blanks)
018800980909     c                   eval      *in52 = (V2Csopn = *blanks)
018900980909     c                   eval      *in53 = (V2Csoma = *blanks)
019000021022     ***c                   eval      *in54 = (V2Ccons = *blanks)
019100021022     ***c                   eval      *in55 = (V2Cmode = *blanks)
019200021022     c                   eval      *in57 = (V2Camag = *blanks)
019300021022     c                   eval      *in58 = (V2Cauac = *blanks)
019400980506      *
019500980909     C                   IF        V2Csodn <> 0
019600980909     C                   eval      *IN50 = *off
019700980909     C                   move      V2Csodn       G02DAT
019800980909     C                   eval      g02err = *blanks
019900980909     C                   CALL      'XSRDA8'
020000980909     C                   PARM                    WLBDAT
020100980909     C                   IF        G02ERR = '1'
020200980909     C                   eval      *IN50 = *on
020300980909     C                   movel     MSG(3)        V2Cmsg
020400980909     C                   ELSE
020500980909     C                   movel     G02DAT        V2Csodn
020600980909     C                   ENDIF
020700980909     c                   ELSE
020800980909     C                   eval      *IN50 = *on
020900980909     C                   ENDIF
021000980909      *
021100021022     ***c     41            DO        55            XX
021200021022     c     41            DO        58            XX
021300980909     c                   eval      *IN28 = (*IN(XX) = *ON)
021400980909     c   28              leave
021500980909     c                   ENDDO
021600980909      *
021700980909     c                   IF        *IN28 = *ON  and  V2Cmsg = *blanks
021800980909     c                   movel     MSG(2)        V2Cmsg
021900980909     c                   ENDIF
022000980909      *
022100980909     C                   ENDSR
022200980507      *****************************************************************
022300980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
022400980507      *****************************************************************
022500980909     C     RIEDAD        BEGSR
022600980909      *
022700980909     C                   movel     V2Caurs       §DADaurs
022800980909     C                   movel     V2Cauin       §DADauin
022900980909     C                   movel     V2Cauca       §DADauca
023000980909     C                   movel     V2Caulo       §DADaulo
023100980909     C                   movel     V2Caupr       §DADaupr
023200980910     C                   movel     V2Csors       §DADsors
023300980909     C                   movel     V2Csoin       §DADsoin
023400980909     C                   movel     V2Csolo       §DADsolo
023500980909     C                   movel     V2Csopr       §DADsopr
023600980909     C                   movel     V2Csoln       §DADsoln
023700980909     C                   movel     V2Csopn       §DADsopn
023800980909     C                   movel     V2Csoma       §DADsoma
023900021022     ***C                   movel     V2Ccons       §DADcons
024000021022     ***C                   movel     V2Cmode       §DADmode
024100021022     c                   movel     V2Camag       §DADamag
024200021022     c                   movel     V2Cauac       §DADauac
024300980910     c     *eur          move      v2csodn       DATA_eur
024400980910     c     *iso          move      DATA_eur      §DADsodn
024500980507      *
024600980909     C   KQ              movel     'A'           DADatb
024700980909     C   KE              movel     *blanks       DADatb
024800980909      *
024900980930     C                   z-add     V1Cfgs        DADfil
025000980909     C                   movel     DDAD          DADdat
025100980909     C                   movel     *blanks       DADftr
025200980909     C                   z-add     *zeros        DADdtr
025300980909      *
025400980909     C   02              WRITE     FNDAD000
025500980909     C  N02              UPDATE    FNDAD000
025600980507      *
025700980507     C                   ENDSR
025800980506      *****************************************************************
025900980506      *   ROUTINE INIZIALE
026000980506      *****************************************************************
026100980506     C     *INZSR        BEGSR
026200980506      *
026300980506     C     *ENTRY        PLIST
026400980506     C                   PARM                    KPJBA
026500980506      *
026600980506     C                   Z-ADD     1             CODUT
026700980506     C                   CALL      'X§PARUT'
026800980507     C                   PARM                    UT§DSE0F
026900980506     C                   MOVEL     RAGUT         RSUT
027000980508     C                   MOVEL     REC80         CNCR80
027100980506      *
027200980507     C                   TIME                    W0140
027300980507     C                   MOVE      W0140         WDTGIO
027400980507     C                   MOVEL     W0140         WORA
027500980930      *-----------------
027600980930      * Preimposto P.O.
027700980930      *-----------------
027800020502     c                   z-add     simpou        v1cfgs
027900980930      *     Se sono un secondo livello blocco la scelta
028000020502     c                   if        simtpp = '2' or simtpp = *blanks
028100020502     c                             or simtpp = 'S'
028200020502     c                   z-add     v1cfgs        l1(1)
028300020502     C                   eval      *in05 = *on
028400020502     C*                  IF        rem = 'REM'  AND  remfil > *ZEROS
028500020502     C*                  movel     REMFIL        V1Cfgs
028600020502     C*                  z-add     V1Cfgs        L1(1)
028700020502     C*                  eval      *in05 = *on
028800020502     C*                  ELSE
028900980930      *     Se sede imposto P.O. = 46
029000020502     C*                  IF        SIMFEL = 0
029100020502     C*                  z-add     46            V1CFGS
029200020502     C*                  z-add     V1Cfgs        L1(1)
029300020502     C*                  eval      *in05 = *on
029400980930     C                   ELSE
029500980930      *     Se P.O. primo livello imposto SIMFEL e carico £1
029600020502     C*                  movel     SIMFEL        V1CFGS
029700980930     C                   CLEAR                   TRUL06DS
029800980930     C                   MOVE      '£1'          D06COD
029900980930     C                   MOVEL     SIMFEL        D06KEY
030000980930     C                   MOVEL     TRUL06DS      KPJBU
030100980930     C                   CALL      'TRUL06R'
030200980930     C                   PARM                    KPJBA
030300980930     C                   MOVEL     KPJBU         TRUL06DS
030400980930     C                   MOVEA     LIN           L1
030500980930    2C                   ENDIF
030600020502    2C*                  ENDIF
030700980930      *
030800980506     C                   ENDSR
030900980909      *****************************************************************
031000980909** MSG  (Lungh. 78)                                                          *
031100980909Immettere codice punto operativo valido                                            1
031200980909Campo obbligatorio                                                                 2
031300980909Inserire data valida                                                               3
031400980909Inserire valore corretto                                                           4
031500980930Filiale in gestione errata                                                         5
