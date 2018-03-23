000100900914     H DECEDIT('0,') DATEDIT(*DMY.)
000200900911     F* ------------------------------------------------------------------
000300981228     F* TRASFERISCO BOLLE DA UN P.O. AD UN ALTRO                       ---
000400900911     F* ------------------------------------------------------------------
000500981228     FFNLV70D   CF   E             WORKSTN
000600990107     F                                     SFILE(LV70S01:NRR2)
000700981228     FAZORG01L  IF   E           K DISK
000800141126     FTNTBE01L  IF   E           K DISK
000900020925     FFIVAB01L  UF A E           K DISK
001000020925     FFIVAD01L  UF A E           K DISK
001001171215     FFIVAD12L  UF A E           K DISK
001100020925     FFIVAT01L  UF A E           K DISK
001200040604     FFIVAX01L  UF A E           K DISK
001300081020     FFIRAB02L  UF A E           K DISK
001400021017     D MSG             S             78    DIM(14) CTDATA PERRCD(1)
001500920921     D*
001600981228     D L1              S              3  0 DIM(30)
001700020926     D L1NPPT          S              3  0 DIM(30)
001800990107     D KSM             S              7    DIM(20)
001900981228     D* DS PER TRUL06R - CARICAMENTO £X
002000981228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002100981228     D  LIN                    1     90  0
002200981228     D                                     DIM(30)
002300981228     D DSBS69        E DS                  EXTNAME(TIBS69DS)
002400981228     D DSACO         E DS                  EXTNAME(CNACO00F)
002500981228     D DSIND         E DS                  EXTNAME(CNIND00F)
002600981228     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002700981228     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002800900911     D KPJBA         E DS
002900020925     D OG143         E DS
003000141126     d D3TR          e ds                  inz
003100990107     D                 DS
003200990107     D  V1CKLP                 1      3  0
003300990107     D  V1CNUM                 4      7  0
003400990107     D  V1CKSC                 1      7  0
003500020923      *------------
003600171229     D*FNLSE5DS      e ds
003700020923      *------------
003800020923      * DS DATI UTENTE
003900020923      *------------
004000020923     D tibs34ds      e ds                  inz
004100020923     D ddatiute      e ds
004200020923     D azuteds       e ds                  extname(AZUTE00F)
004300051215     D tntb50ds      e ds
004400141126      *------------
004500141126     d wFilTrasf       s              3s 0
004600141126     d wPOABI          ds
004700141126     d  SkPOABI                       3    dim(10)
004800141126     d wNTW_1          s                   like (§OGNTW)
004900141126     d wNTW_2          s                   like (§OGNTW)
005000981228     C* ------------------------------------------------------------------
005100981228     C     *ENTRY        PLIST
005200981228     C                   PARM                    KPJBA
005300020925     C                   Z-ADD     1             CODUT             1 0
005400020923      *
005500020923     c     *dtaara       define    §azute        azuteds
005600020923     c     *dtaara       define    §datiute      ddatiute
005700020923     c                   in(E)     *dtaara
005800020923     c                   IF        %error  or RSUT = *blanks
005900020923     c                   CLEAR                   tibs34ds
006000020923     c                   CALL      'TIBS34R'
006100020923     c                   PARM                    tibs34ds
006200020923     c                   in        *dtaara
006300020923     c                   endif
006400020923     C***
006500981228     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
006600981228     C***
006700981228     C                   CLEAR                   L1
006800981228     C     SIMFEL        IFGT      0
006900981228     C                   CLEAR                   DSUL06
007000981228     C                   MOVE      '£1'          D06COD
007100981228     C                   MOVEL     SIMFEL        D06KEY
007200981228     C                   MOVEL     'L'           D06TLA
007300981228     C                   MOVEL     DSUL06        KPJBU
007400981228     C*
007500981228     C                   CALL      'TRUL06R'
007600981228     C                   PARM                    KPJBA
007700981228     C                   MOVEL     KPJBU         DSUL06
007800981228     C                   MOVEA     LIN           L1
007900981228     C                   ENDIF
008000020923     C***
008100021002     C* COSTRUISCO SCHIERE DA L1 SENZA I P.O POSTE
008200020923     C***
008300020924     C                   Z-ADD     1             Y                 2 0
008400020923     C     Y             DOWLT     30
008500020923     C     L1(Y)         ANDNE     *zeros
008600020923     C                   MOVE      L1(Y)         WORG              3 0
008700020923     C*
008800020923     C     WORG          CHAIN     AZORG01L                           37
008900020923     C                   IF        *IN37=*OFF AND ORGFVA = ' '
009000020923     C                   MOVEL     ORGDE3        OG143
009100020923     C                   ENDIF
009200020923     C     §OGNTW        IFNE      'PPT'
009300020924     C                   Z-ADD     L1(Y)         L1NPPT(Y)
009400020923     c                   ENDIF
009500020924     C                   ADD       1             Y
009600020923     C                   ENDDO
009700021002      *
009800021002      * Richiamo Pgm per spostare dati da FNVAB a FIVAB
009900171229     C***                CLEAR                   FNLSE5DS
010000171229     C***                Z-ADD     DUTPOU        IE5POU
010100171229     C***                CALL      'FNLSE5R'
010200171229     C***                PARM                    FNLSE5DS
010300020923     C
010400981228     C* ------------------------------------------------------------------
010500981228     C     KVAB          KLIST
010600981228     C                   KFLD                    V1CKSC
010700981228     C                   KFLD                    V1CRMD
010800981228     C     KVAD          KLIST
010900021002     C                   KFLD                    VABFGS
011000020923     C                   KFLD                    V1CKSC
011100981228     C                   KFLD                    VABAAS
011200981228     C                   KFLD                    VABLNP
011300981228     C                   KFLD                    VABNRS
011400981228     C                   KFLD                    VABNSP
011500141126     C* Definizione chiavi - TNTBE01L
011600141126     C     K02tbe01      KLIST
011700141126     C                   KFLD                    tbeCOD
011800141126     C                   KFLD                    tbeKE1
011900981228     C**
012000981228     C     *LIKE         DEFINE    V1CKSC        SAVKSC
012100981228     C     *LIKE         DEFINE    V1CRMA        SAVRMA
012200981228     C     *LIKE         DEFINE    V1CRMD        SAVRMD
012300900911     C* ------------------------------------------------------------------
012400990107     C     INIZIO        TAG
012500981228     C                   SETOFF                                       01
012600981228     C                   CLEAR                   SAVRMD
012700981228     C                   CLEAR                   SAVRMA
012800981228     C                   CLEAR                   SAVKSC
012900990105     C                   CLEAR                   V1CKSC
013000990105     C                   CLEAR                   V1DKSC
013100990105     C                   CLEAR                   V1CRMD
013200990105     C                   CLEAR                   V1CRMA
013300990105     C                   CLEAR                   V1CFIL
013400990107     C                   CLEAR                   V1DFIL
013500021015     C                   move      'S'           V1CANN
013600990105     C                   CLEAR                   V1CNBO
013700990107     C                   MOVEL     SIMFEL        V1CKLP
013800020925     C     DUTLPO        IFEQ      '2'
013900020925     C     DUTLPO        OREQ      *BLANKS
014000020925     C                   MOVEL     DUTPOU        V1CKLP
014100990107     C                   END
014200990107     C*
014300990107     C                   SETON                                        35
014400990107     C                   WRITE     LV70C01
014500990107     C                   SETOFF                                       35
014600021002     C* SCRIVO LA PAGINA DI SFL
014700990107     C                   DO        13            NRR2              4 0
014800990107     C                   WRITE     LV70S01
014900990107     C                   ENDDO
015000990107     C                   Z-ADD     1             NRR2
015100981228     C     FOR01         TAG
015200990107     C                   WRITE     LV70Z01
015300990107     C                   EXFMT     LV70C01
015400021017     C                   SETOFF                                       3828
015500981228     C* F3 - FINE
015600981228     C   KC              GOTO      FINE
015700990105     C*
015800990105     C                   SETOFF                                       289040
015900990105     C                   SETOFF                                       414243
016000981228     C* CONTROLLI
016100981228     C                   EXSR      CTRD01
016200981228     C   90              GOTO      FOR01
016300981228     C**
016400021017     C  NKF
016500021017     CANNKJ              GOTO      FOR01
016600021017      **
016700021017      ** F6 - CONFERMA CON CANCELLAZIONE BOLLA,
016800021017      ** F10- CONFERMA SENZA CANCELLAZIONE BOLLA
016900021017      **
017000021017     C   KJ              DO
017100021017     C                   IF        V1CAnn = 'S'
017200021017     C                   MOVEL     MSG(14)       VIDMSG
017300021017     C                   SETON                                          28
017400021017     C                   SETOFF                                       38
017500021017     C                   GOTO      FOR01
017600021017     C                   ELSE
017700021017     C                   EXSR      ELAB
017800021017     C                   ENDIF
017900021017     C                   ENDDO
018000021017      *
018100021017     C   KF              DO
018200021017     C                   IF        V1CAnn = 'N'
018300021017     C                   MOVEL     MSG(13)       VIDMSG
018400021017     C                   SETON                                        3828
018500021017     C                   GOTO      FOR01
018600021017     C                   ELSE
018700021017     C                   EXSR      ELAB
018800021017     C                   ENDIF
018900021017     C                   ENDDO
019000990107     C   KF
019100990107     CAN 90              GOTO      FOR01
019200990107     C**
019300990107     C                   GOTO      INIZIO
019400981228     C**
019500981228     C     FINE          TAG
019600981228     C                   CLEAR                   DSBS69
019700981228     C                   MOVEL     'C'           I69TLA
019800981228     C                   CALL      'TIBS69R'
019900981228     C                   PARM                    DSBS69
020000981228     C                   PARM                    DSACO
020100981228     C                   PARM                    DSIND
020200981228     C                   PARM                    DSCLP
020300981228     C                   PARM                    DSCLS
020400900911     C*
020500900911     C                   SETON                                        LR
020600990107     C**
020700981228     C* CONTROLLI FORMATO 1 ------------------------------------------*
020800981228     C     CTRD01        BEGSR
020900021002     C* IL PO A CUI TRASFERIRE NON PUO' ESSERE IN L1
021000021002    1C     V1CFIL        IFEQ      0
021100990107     C                   MOVEL     MSG(4)        VIDMSG
021200990107     C                   SETON                                        422890
021300990107     C                   GOTO      ENDCTR
021400021002    1C                   ENDIF
021500990107     c*
021600990107     C     V1CFIL        CHAIN     AZORG01L                           30
021700021002    1C     *IN30         IFEQ      *ON
021800990107     C     ORGFVA        ORNE      ' '
021900990107     C     ORGFAG        ORNE      'A'
022000990107     C     ORGFAG        ANDNE     'F'
022100990107     C                   MOVEL     MSG(5)        VIDMSG
022200990107     C                   SETON                                        422890
022300990107     C                   GOTO      ENDCTR
022400021002    1C                   ENDIF
022500990107     C                   MOVEL     ORGDES        V1DFIL
022600141126     C* reperisco il suo network
022700141126     C                   MOVEL     ORGDE3        OG143
022800141126     C                   EVAL      wNTW_1 = §OGNTW
022900990107     c*
023000990107     C     V1CFIL        LOOKUP    L1                                     30
023100021002    1C     *IN30         IFEQ      *ON
023200990107     C                   MOVEL     MSG(8)        VIDMSG
023300990107     C                   SETON                                        422890
023400990107     C                   GOTO      ENDCTR
023500021002    1C                   ENDIF
023600990107     C**
023700990107     C                   Z-ADD     1             NRR2
023800990107     C                   CLEAR                   WTROV
023900990107     C**
024000990107    1C     NRR2          DOWLE     13
024100990107     C     NRR2          CHAIN     LV70S01                            31
024200990107     C**
024300990107    2C     *IN31         IFEQ      *OFF
024400990107     C                   SETOFF                                       01
024500990107     C                   CLEAR                   V1CNBO
024600990107     C** RICERCA ALFABETICA
024700990107    3C   KGV1DKSC        IFNE      *BLANKS
024800990107     C     V1CNUM        ANDEQ     0
024900990107     C* DESCRIZIONE AZIENDA
025000990107     C                   MOVEL     RSUT          PAXDUT           30
025100990107     C* PAXSTA=9 ESCLUDO ANNULLATI
025200990107     C                   Z-ADD     9             PAXSTA
025300990107     C* CAPOCONTO CLIENTI
025400020925     C                   Z-ADD     DUTKCI        PAXCCC
025500990107     C                   MOVEL     V1DKSC        PAXDMT           48
025600990107     C                   CLEAR                   ACOKSC
025700021002    4C     V1CKLP        IFGT      0
025800990107     C                   MOVEL     V1CKLP        PAXFLR
025900021002    4C                   ENDIF
026000990107     C                   CLEAR                   PAXKSM
026100990107     C                   CLEAR                   PAXKCM
026200990107     C                   CLEAR                   PAXKDM
026300991203     C                   Z-ADD     13            PAXNUM
026400990107     C*
026500990107     C                   CALL      'XALFA3BR'
026600990107     C                   PARM                    PAXDUT
026700990107     C                   PARM                    CODUT
026800990107     C                   PARM                    PAXDMT
026900990107     C                   PARM                    PAXCCC            4 0
027000990107     C                   PARM                    PAXSTA            1 0
027100990107     C                   PARM                    PAXFLR           90
027200990107     C                   PARM                    PAXDIT            3
027300991203     C                   PARM                    PAXNUM            2 0
027400990107     C                   PARM                    PAXKCM           80
027500990107     C                   PARM                    PAXKSM          140
027600990107     C                   PARM                    PAXKDM           60
027700990107     C*
027800990107     C* NON TROVATO O PREMUTO CMD12
027900990107    4C     PAXSTA        IFGT      -1
028000990107     C* CODICI SELEZIONATI: IMPOSTO A VIDEO
028100990107     C                   MOVEA     PAXKSM        KSM
028200990107     C                   Z-ADD     NRR2          SAVNRR
028300990107     C     *LIKE         DEFINE    NRR2          SAVNRR
028400990107     C*
028500990107     C                   Z-ADD     1             BB                2 0
028600990107    5C     KSM(BB)       DOWNE     *BLANKS
028700990107     C     NRR2          ANDLE     13
028800990107    6C     BB            IFGT      1
028900990107     C                   UPDATE    LV70S01
029000990107     C                   ADD       1             NRR2
029100990107     C     NRR2          CHAIN     LV70S01                            32
029200990107    6C                   ENDIF
029300990107     C**
029400990107    6C     V1CNUM        IFEQ      0
029500990107     C                   MOVE      KSM(BB)       V1CKSC
029600990107     C                   ADD       1             BB
029700990107    6C                   ENDIF
029800990107     C**
029900990107    5C                   ENDDO
030000990107     C**
030100990107     C                   SETON                                        90
030200990107    5C     KSM(2)        IFNE      *BLANKS
030300990107     C                   UPDATE    LV70S01
030400990107     C**
030500990107     C                   Z-ADD     SAVNRR        NRR2
030600990107     C     NRR2          CHAIN     LV70S01                            31
030700990107    5C                   ENDIF
030800990107    4C                   ENDIF
030900990107    3C                   ENDIF
031000990107     C**
031100990107     C* ELABORO SOLO SE C'E' QUALCHE COSA
031200990107    3C     V1CNUM        IFGT      0
031300990107     C     V1CRMD        ORGT      0
031400990107     C     V1CRMA        ORGT      0
031500981228     C* CLIENTE
031600981228     C                   CLEAR                   DSBS69
031700020923     C                   MOVEL     DUTKCI        I69KCC
031800981228     C                   MOVEL     V1CKSC        I69KAC
031900981228     C                   MOVEL     KNSIF         I69SIF
032000981228     C                   CALL      'TIBS69R'
032100981228     C                   PARM                    DSBS69
032200981228     C                   PARM                    DSACO
032300981228     C                   PARM                    DSIND
032400981228     C                   PARM                    DSCLP
032500981228     C                   PARM                    DSCLS
032600981228     C* ERRORE
032700990107    4C     O69ERR        IFNE      ' '
032800981228     C                   MOVEL     MSG(1)        VIDMSG
032900981228     C                   SETON                                        402890
033000981228     C                   GOTO      ENDCT1
033100990107    4C                   ENDIF
033200990105     C                   MOVEL     ACORAG        V1DKSC
033300051215     c* il p.o. a cui trasferire deve essere abilitato come da tablla
033400051215     c* 3TR --> effettuo il controllo richiamando il pgm tntb51r
033500060518     c****               clear                   tntb50ds
033600060518     c****               movel     v1cksc        b50ke2
033700060518     c****               movel     v1cksc        b50ke1
033800060518     c****               movel     'C'           b50opz
033900060518     c****               movel     v1cfil        b50poa
034000060518     c****               call      'TNTB51R'
034100060518     c****               parm                    kpjba
034200060518     c****               parm                    tntb50ds
034300060518     c****
034400060518     c****               if        b50err<>' '
034500060518     c****               movel     b50msg        vidmsg
034600060518     c****               seton                                        402890
034700060518     c****               goto      endct1
034800060518     c****               endif
034900060518     C****
035000141126     C* riattivo il test con la tab. 3TR, però in questo modo:
035100141126     C*
035200141126     C*
035300141126     C* reperisco il network del PO di partenza
035400141126     C     V1CKLP        CHAIN     AZORG01L                           28
035500141126    1C     *IN28         IFEQ      *ON
035600141126     C     ORGFVA        ORNE      ' '
035700141126     C     ORGFAG        ORNE      'A'
035800141126     C     ORGFAG        ANDNE     'F'
035900141126     C* non do un msg di errore perché avrò quello di cliente inesistente
036000141126     C                   GOTO      ENDCTR
036100141126    1C                   ENDIF
036200141126     C* reperisco il suo network
036300141126     C                   MOVEL     ORGDE3        OG143
036400141126     C                   EVAL      wNTW_2 = §OGNTW
036500141126     C*
036600141126     C* Reperimento tabella 3TR
036700141126     C                   MOVE(P)   '3TR'         tbeCOD
036800141126     C                   EVAL      tbeKE1 = %editc(V1CKLP:'X')
036900141126     C     K02TBE01      CHAIN     TNTBE01L
037000141126     C* se non trovata filiale su tab.3TR significa che la filiale cliente può a
037100141126     C* confermate da qualsiasi altra filiale
037200141126     C                   IF        %found(TNTBE01L) AND
037300141126     C                             tbeATB <> 'A'
037400141126     C                   MOVEL     tbeUNI        D3TR
037500141126     C                   EVAL      wPOABI = §3TRPOABI
037600141126     C* se la filiale cliente è presente su tab. 3TR controllo:
037700141126     C* - che la filiale di conferma sia presente nella schiera di quelle abilit
037800141126     C                   IF        %lookup(%editc(V1CFIL:'X'):SkPOABI) = 0
037900141126     C                             or SkPOABI(1) = '999'
038000141126     C                   SETON                                        432890
038100141126     C                   MOVEL     MSG(9)        VIDMSG
038200141126     C                   GOTO      ENDCTR
038300141126     C                   ENDIF
038400141126     C                   ENDIF
038500141126     C*
038600141126     C* controllo che se il network della filiale conferma bolle è estero
038700141126     C                   IF        wNTW_1 = 'DPD' or
038800141126     C                             wNTW_1 = 'EEX' or
038900141126     C                             wNTW_1 = 'FED'
039000141126     C* il network della filiale cliente deve essere lo stesso
039100141126     C                   IF        wNTW_2 <> wNTW_1
039200141126     C                   SETON                                        432890
039300141126     C                   MOVEL     MSG(10)       VIDMSG
039400141126     C                   GOTO      ENDCTR
039500141126     C                   ENDIF
039600141126     C                   ENDIF
039700141126     C*
039800020924     c* NON ESISTONO BOLLE PER IL CLIENTE DIGITATO
039900020924     c* LE BOLLE VALIDE DEVONO INOLTRE AVERE IL P.O DI
040000020924     c* GESTIONE IN L1
040100020924     C     V1CKSC        SETLL     FIVAB01L
040200020924     C     V1CKSC        READE     FIVAB01L                               30
040300020924      *
040400021002    4C     *IN30         DOWEQ     *OFF
040500021002     C     VABFGS        LOOKUP    L1NPPT                                 32
040600020924     C   32              LEAVE
040700020924     C     V1CKSC        READE     FIVAB01L                               30
040800021002    4C                   ENDDO
040900020924      *
041000020924    4C     *IN30         IFEQ      *ON
041100981228     C                   MOVEL     MSG(2)        VIDMSG
041200981228     C                   SETON                                        402890
041300981228     C                   GOTO      ENDCT1
041400990107    4C                   ENDIF
041500981228     C* rif mitt al > dal
041600990107    4C     V1CRMA        IFGT      0
041700981228     C     V1CRMA        ANDLT     V1CRMD
041800981228     C                   MOVEL     MSG(3)        VIDMSG
041900981228     C                   SETON                                        412890
042000981228     C                   GOTO      ENDCT1
042100990107    4C                   ENDIF
042200990107     C**
042300981228     C* CONTROLLO SE ESISTE ALMENO UNA BOLLA
042400020924     c* LE BOLLE VALIDE DEVONO INOLTRE AVERE IL P.O DI
042500020924     c* GESTIONE IN L1
042600990107    4C     V1CRMD        IFGT      0
042700981228     C     V1CRMA        ANDEQ     0
042800020924     C     KVAB          SETLL     FIVAB01L
042900021002     C     KVAB          READE     FIVAB01L                               30
043000020924      *
043100021002    5C     *IN30         DOWEQ     *OFF
043200021002     C     VABFGS        LOOKUP    L1NPPT                                 32
043300020924     C   32              LEAVE
043400021002     C     KVAB          READE     FIVAB01L                               30
043500021002    5C                   ENDDO
043600020924      *
043700021002    5C     *IN30         IFEQ      *ON
043800981228     C                   MOVEL     MSG(6)        VIDMSG
043900990105     C                   SETON                                        412890
044000981228     C                   GOTO      ENDCT1
044100021002    5C                   ENDIF
044200990107    4C                   ENDIF
044300981228     c*
044400990107    4C     V1CRMA        IFGT      0
044500020923     C     KVAB          SETLL     FIVAB01L
044600020923     C     V1CKSC        READE     FIVAB01L                               30
044700020924      *
044800021002    5C     *IN30         DOWEQ     *OFF
044900021002     C     vabfgs        LOOKUP    L1NPPT                                 32
045000020924     C   32              LEAVE
045100020924     C     V1CKSC        READE     FIVAB01L                               30
045200021002    5C                   ENDDO
045300020924      *
045400990107    5C     *IN30         IFEQ      *OFF
045500981228     C     VABRMN        ANDGT     V1CRMA
045600021002     C                   eval      *in30 = *on
045700990107    5C                   ENDIF
045800990107    5C     *IN30         IFEQ      *ON
045900981228     C                   MOVEL     MSG(7)        VIDMSG
046000981228     C                   SETON                                        422890
046100981228     C                   GOTO      ENDCT1
046200990107    5C                   ENDIF
046300990107    4C                   ENDIF
046400990107     C*
046500990107     C                   MOVEL     'S'           WTROV             1
046600990107    3C                   ENDIF
046700981228     C**
046800990107     C     ENDCT1        TAG
046900990107     C**
047000990107     C* SE NON  CI SONO ERRORI CONTO LE BOLLE
047100021002    3C     *IN90         IFEQ      *OFF
047200021002     C     V1CNUM        ANDGT     0
047300021002     C                   EXSR      conta
047400021002     C                   eval      *in01 = *on
047500021002    3C                   ENDIF
047600990107     C*
047700990107     C                   UPDATE    LV70S01
047800021002    2C                   ENDIF
047900990107     C* SE C'E' ERRORE ESCO
048000021002    2C     *IN90         IFEQ      *ON
048100990107     C                   Z-ADD     14            NRR2
048200021002   x2C                   ELSE
048300990107     C                   ADD       1             NRR2
048400021002    2C                   ENDIF
048500021002      *
048600021002    1C                   ENDDO
048700990107     C**
048800990107     C* SE NON IMPOSTATO NEMMENO UN CODICE ERRORE
048900990107     C  N90WTROV         IFEQ      ' '
049000990107     C                   MOVEL     MSG(11)       VIDMSG
049100990107     C                   SETON                                        2890
049200990107     C                   ENDIF
049300021002      *
049400990107     C     ENDCTR        ENDSR
049500021002     C**
049600021002      * CONTO RECORD DA TRASMETTERE-----------------------------------*
049700021002     C     CONTA         BEGSR
049800021002      *
049900021002     C     KVAB          SETLL     FIVAB01L
050000021002     C     V1CKSC        READE     FIVAB01L                               30
050100021002      *
050200021002    1C     *IN30         DOWEQ     *OFF
050300021002     C     vabfgs        LOOKUP    L1NPPT                                 32
050400021002     C   32              LEAVE
050500021002     C     V1CKSC        READE     FIVAB01L                               30
050600021002    1C                   ENDDO
050700021002      *
050800021002    1C     *IN30         IFEQ      *OFF
050900021002      *
051000021002    2C     V1CRMA        IFEQ      *zeros
051100021002     C     V1CRMD        ANDGT     *zeros
051200021002     C                   Z-ADD     1             V1CNBO
051300021002   X2C                   ELSE
051400021002      *
051500021002    3C     *IN30         DOWEQ     *OFF
051600021002    4C     V1CRMA        IFGT      *zeros
051700021002     C     VABRMN        ANDGT     V1CRMA
051800021002     C                   eval      *in30 = *on
051900021002   x4C                   ELSE
052000021002      *
052100021002     C                   ADD       1             V1CNBO
052200021002     C     V1CKSC        READE     FIVAB01L                               30
052300021002      *
052400021002    5C     *IN30         DOWEQ     *OFF
052500021002     C     vabfgs        LOOKUP    L1NPPT                                 32
052600021002     C   32              LEAVE
052700021002     C     V1CKSC        READE     FIVAB01L                               30
052800021002    5C                   ENDDO
052900021002      *
053000021002    4C                   ENDIF
053100021002    3C                   ENDDO
053200021002      *
053300021002    2C                   ENDIF
053400021002    1C                   ENDIF
053500021002      **
053600021002     C                   ENDSR
053700021002     C**
053800981228     C* TRASMETTO I DATI RICHIESTI------------------------------------*
053900981228     C     ELAB          BEGSR
054000021002      *
054100021002     C                   SETOFF                                       3031
054200040604     C                   SETOFF                                       32
054300021001     C                   Z-ADD     1             NRR2
054400021002      *
054500021002      *
054600990107    1C     NRR2          DOWLE     13
054700021002      *
054800990107     C     NRR2          CHAIN     LV70S01                            31
054900990107    2C  N31V1CNUM        IFGT      0
055000020925      *
055100021002     C                   eval      *in34 = *off
055200021002      *
055300981228     C**
055400020926     C**  CERCO LE BOLLE DA TRASMETTERE O SCRIVERE/AGGIORNARE IN LOCALE
055500981228     C**
055600020926     C     KVAB          SETLL     FIVAB01L
055700020930     C     V1CKSC        READE     FIVAB01L                               30
055800020926      *
055900020926      * CERCO IL PRIMO RECORD CHE MI COMPETE
056000020926    3C     *IN30         DOWEQ     *OFF
056100021002     C     VABFGS        LOOKUP    L1NPPT                                 32
056200020926     C   32              LEAVE
056300020930     C     V1CKSC        READE     FIVAB01L                               30
056400020926    3C                   ENDDO
056500020926      *
056600020926      * PROSEGUO SE HO TROVATO UN RECORD VALIDO
056700020926    3C                   IF        *IN30=*OFF
056800020926    4C     *IN34         DOWEQ     *OFF
056900020926      *
057000020926    5C     V1CRMA        IFGT      0
057100981228     C     VABRMN        ANDGT     V1CRMA
057200021002     C                   SETON                                        3034
057300021002    5C                   ENDIF
057400021002    5C     V1CRMA        IFEQ      0
057500021002     C     V1CRMD        ANDGT     0
057600021002     C     VABRMN        ANDNE     V1CRMD
057700021002     C                   SETON                                        3034
057800021002    5C                   ENDIF
057900021002      *
058000021002    5C                   IF        *IN30 = *OFF
058100981228     C** TRASFERISCO ANCHE IL SUO DETTAGLIO SE C'E'
058200021002     C     KVAD          setll     FIVAD01L
058300021002     C     KVAD          READE     FIVAD01L                               31
058400981228     C**
058500020926    6C     *IN31         DOWEQ     *OFF
058600020930     C                   Z-ADD     V1CFIL        VADFGS
058700021002     C                   EXSR      GESLOC_VAD
058800981228     C**
058900020923     C     KVAD          READE     FIVAD01L                               31
059000020926    6C                   ENDDO
059001171215     C**
059002171215     C** TRASFERISCO ANCHE l'estensione del dettaglio se c'e'
059003171215     C     KVAD          setll     FIVAD12L
059004171215     C     KVAD          READE     FIVAD12L                               31
059005171215     C**
059006171215    6C     *IN31         DOWEQ     *OFF
059007171215     C                   Z-ADD     V1CFIL        VADFGS
059008171215     C                   EXSR      GESLOC_VAD1
059009171215     C**
059010171215     C     KVAD          READE     FIVAD12L                               31
059011171215    6C                   ENDDO
059100020424     C**
059200020424     C** TRASFERISCO ANCHE LA SUA ESTENSIONE-DETTAGLIO SE C'E'
059300021002     C     KVAD          setll     FIVAT01L
059400021002     C     KVAD          READE     FIVAT01L                               31
059500020424     C**
059600020926    6C     *IN31         DOWEQ     *OFF
059700020930     C                   Z-ADD     V1CFIL        VATFGS
059800021002     C                   EXSR      GESLOC_VAT
059900020424     C**
060000020923     C     KVAD          READE     FIVAT01L                               31
060100020926    6C                   ENDDO
060200040604     c**
060300040604     C** TRASFERISCO ANCHE LA SUA ESTENSIONE-TESTATA SE C'E'
060400040604     C     KVAD          setll     FIVAX01L
060500040604     C     KVAD          READE     FIVAX01L                               31
060600040604     C**
060700040604    6C     *IN31         DOWEQ     *OFF
060800040604     C                   Z-ADD     V1CFIL        VAXFGS
060900040604     C                   EXSR      GESLOC_VAX
061000040604     C**
061100040604     C     KVAD          READE     FIVAX01L                               31
061200040604    6C                   ENDDO
061300081020     C** TRASFERISCO ANCHE FIRAB
061400081020     C     KVAD          setll     FIRAB02L
061500081020     C     KVAD          READE     FIRAB02L                               31
061600081020     C**
061700081020    6C     *IN31         DOWEQ     *OFF
061800081020     C                   Z-ADD     V1CFIL        RABFGS
061900081020     C                   EXSR      GESLOC_RAB
062000081020     C**
062100081020     C     KVAD          READE     FIRAB02L                               31
062200081020    6C                   ENDDO
062300981228     C**
062400020930     C                   Z-ADD     V1CFIL        VABFGS
062500021002     C                   EXSR      GESLOC_VAB
062600981228     C**
062700020926    5C                   ENDIF
062800020924     C**
062900020926    5C     *IN34         DOWEQ     *OFF
063000020930     C     V1CKSC        READE     FIVAB01L                               34
063100021002     C     VABFGS        LOOKUP    L1NPPT                                 32
063200020926     C   32              LEAVE
063300020926    5C                   ENDDO
063400020926    4C                   ENDDO
063500020926    3C                   ENDIF
063600020926      *
063700021001    2C                   ENDIF
063800990107     C                   ADD       1             NRR2
063900021001    1C                   ENDDO
064000981228     C**
064100981228     C     ENDELA        ENDSR
064200020924     C**----------------------------------
064300020925     C** AGGIORNO/SCRIVO FILES VAD IN LOCALE -------------------------*
064400020924     C**----------------------------------
064500020925     C     GESLOC_VAD    BEGSR
064600020925     C                   IF        V1CANN = 'S'
064700020925     C                   UPDATE    FIVAD000
064800020925     C                   ELSE
064900020925     C                   WRITE     FIVAD000
065000020925     C                   ENDIF
065100020924     C                   ENDSR
065101171215     C**----------------------------------
065102171215     C** AGGIORNO/SCRIVO FILES VAD10 IN LOCALE -------------------------*
065103171215     C**----------------------------------
065104171215     C     GESLOC_VAD1   BEGSR
065105171215     C                   IF        V1CANN = 'S'
065106171215     C                   UPDATE    FIVAD100
065107171215     C                   ELSE
065108171215     C                   WRITE     FIVAD100
065109171215     C                   ENDIF
065110171215     C                   ENDSR
065200020925     C**----------------------------------
065300020925     C** AGGIORNO/SCRIVO FILES VAT IN LOCALE -------------------------*
065400020925     C**----------------------------------
065500020925     C     GESLOC_VAT    BEGSR
065600020925     C                   IF        V1CANN = 'S'
065700020925     C                   UPDATE    FIVAT000
065800020925     C                   ELSE
065900020925     C                   WRITE     FIVAT000
066000020925     C                   ENDIF
066100020925     C                   ENDSR
066200040604     C**----------------------------------
066300040604     C** AGGIORNO/SCRIVO FILES VAX IN LOCALE -------------------------*
066400040604     C**----------------------------------
066500040604     C     GESLOC_VAX    BEGSR
066600040604     C                   IF        V1CANN = 'S'
066700040604     C                   UPDATE    FIVAX000
066800040604     C                   ELSE
066900040604     C                   WRITE     FIVAX000
067000040604     C                   ENDIF
067100040604     C                   ENDSR
067200081020     C**----------------------------------
067300081020     C** AGGIORNO/SCRIVO FILES RAB IN LOCALE -------------------------*
067400081020     C**----------------------------------
067500081020     C     GESLOC_RAB    BEGSR
067600081020     C                   IF        V1CANN = 'S'
067700081020     C                   UPDATE    FIRAB000
067800081020     C                   ELSE
067900081020     C                   WRITE     FIRAB000
068000081020     C                   ENDIF
068100081020     C                   ENDSR
068200020925     C**----------------------------------
068300020925     C** AGGIORNO/SCRIVO FILES VAB IN LOCALE -------------------------*
068400020925     C**----------------------------------
068500020925     C     GESLOC_VAB    BEGSR
068600020925     C                   IF        V1CANN = 'S'
068700020926     C                   UPDATE    FIVAB000
068800020925     C                   ELSE
068900020926     C                   WRITE     FIVAB000
069000020925     C                   ENDIF
069100020925     C                   ENDSR
069200990105     C**----------------------------------
069300020424**
069400981228Cliente inesistente                                                           1
069500981228Non esistono bolle da confermare per il cliente digitato                      2
069600981228Il numero riferimento mittente DAL deve essere minore del numero AL           3
069700121022Filiale da trasferire obbligatoria                                            4
069800121022Filiale da trasferire errata                                                  5
069900981228Non esiste la bolla con il riferimento immesso, per il cliente indicato       6
070000981228Non esistono bolle con i riferimenti immessi, per il cliente indicato         7
070100121022Se si trasferisce, indicare filiale di altro terminal di partenza             8
070200141126Filiale di conferma bolle NON abilitata per il cliente, interpellare il POC   9
070300141126Network Filiale di conf. bolle diverso da quello cliente, interpellare il POC
070400990107Indicare almeno un codice mittente da trasferire
070500990107Immettere la chiave di ricerca alfabetica
070600021017Confermare con F10 la scelta di non cancellare le bolle dopo il trasferimento
070700021017Premere F6 per confermare
