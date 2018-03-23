000100900914     H DECEDIT('0,') DATEDIT(*DMY.)
000200900911     F* ------------------------------------------------------------------
000300981228     F* TRASFERISCO BOLLE DA UN P.O. AD UN ALTRO                       ---
000400900911     F* ------------------------------------------------------------------
000500981228     FFNLV70D   CF   E             WORKSTN
000600990107     F                                     SFILE(LV70S01:NRR2)
000700981228     FAZORG01L  IF   E           K DISK
000800020925     FFIVAB01L  UF A E           K DISK
000900020925     FFIVAD01L  UF A E           K DISK
001000020925     FFIVAT01L  UF A E           K DISK
001100040604     FFIVAX01L  UF A E           K DISK
001101081020     FFIRAB02L  UF A E           K DISK
001200021017     D MSG             S             78    DIM(14) CTDATA PERRCD(1)
001300920921     D*
001400981228     D L1              S              3  0 DIM(30)
001500020926     D L1NPPT          S              3  0 DIM(30)
001600990107     D KSM             S              7    DIM(20)
001700981228     D* DS PER TRUL06R - CARICAMENTO £X
001800981228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001900981228     D  LIN                    1     90  0
002000981228     D                                     DIM(30)
002100981228     D DSBS69        E DS                  EXTNAME(TIBS69DS)
002200981228     D DSACO         E DS                  EXTNAME(CNACO00F)
002300981228     D DSIND         E DS                  EXTNAME(CNIND00F)
002400981228     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002500981228     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002600900911     D KPJBA         E DS
002700020925     D OG143         E DS
002800990107     D                 DS
002900990107     D  V1CKLP                 1      3  0
003000990107     D  V1CNUM                 4      7  0
003100990107     D  V1CKSC                 1      7  0
003200020923      *------------
003300020923     D FNLSE5DS      e ds
003400020923      *------------
003500020923      * DS DATI UTENTE
003600020923      *------------
003700020923     D tibs34ds      e ds                  inz
003800020923     D ddatiute      e ds
003900020923     D azuteds       e ds                  extname(AZUTE00F)
004000051215     D tntb50ds      e ds
004100981228     C* ------------------------------------------------------------------
004200981228     C     *ENTRY        PLIST
004300981228     C                   PARM                    KPJBA
004400020925     C                   Z-ADD     1             CODUT             1 0
004500020923      *
004600020923     c     *dtaara       define    §azute        azuteds
004700020923     c     *dtaara       define    §datiute      ddatiute
004800020923     c                   in(E)     *dtaara
004900020923     c                   IF        %error  or RSUT = *blanks
005000020923     c                   CLEAR                   tibs34ds
005100020923     c                   CALL      'TIBS34R'
005200020923     c                   PARM                    tibs34ds
005300020923     c                   in        *dtaara
005400020923     c                   endif
005500020923     C***
005600981228     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
005700981228     C***
005800981228     C                   CLEAR                   L1
005900981228     C     SIMFEL        IFGT      0
006000981228     C                   CLEAR                   DSUL06
006100981228     C                   MOVE      '£1'          D06COD
006200981228     C                   MOVEL     SIMFEL        D06KEY
006300981228     C                   MOVEL     'L'           D06TLA
006400981228     C                   MOVEL     DSUL06        KPJBU
006500981228     C*
006600981228     C                   CALL      'TRUL06R'
006700981228     C                   PARM                    KPJBA
006800981228     C                   MOVEL     KPJBU         DSUL06
006900981228     C                   MOVEA     LIN           L1
007000981228     C                   ENDIF
007100020923     C***
007200021002     C* COSTRUISCO SCHIERE DA L1 SENZA I P.O POSTE
007300020923     C***
007400020924     C                   Z-ADD     1             Y                 2 0
007500020923     C     Y             DOWLT     30
007600020923     C     L1(Y)         ANDNE     *zeros
007700020923     C                   MOVE      L1(Y)         WORG              3 0
007800020923     C*
007900020923     C     WORG          CHAIN     AZORG01L                           37
008000020923     C                   IF        *IN37=*OFF AND ORGFVA = ' '
008100020923     C                   MOVEL     ORGDE3        OG143
008200020923     C                   ENDIF
008300020923     C     §OGNTW        IFNE      'PPT'
008400020924     C                   Z-ADD     L1(Y)         L1NPPT(Y)
008500020923     c                   ENDIF
008600020924     C                   ADD       1             Y
008700020923     C                   ENDDO
008800021002      *
008900021002      * Richiamo Pgm per spostare dati da FNVAB a FIVAB
009000020923     C                   CLEAR                   FNLSE5DS
009100020923     C                   Z-ADD     DUTPOU        IE5POU
009200020923     C                   CALL      'FNLSE5R'
009300020923     C                   PARM                    FNLSE5DS
009400020923     C
009500981228     C* ------------------------------------------------------------------
009600981228     C     KVAB          KLIST
009700981228     C                   KFLD                    V1CKSC
009800981228     C                   KFLD                    V1CRMD
009900981228     C     KVAD          KLIST
010000021002     C                   KFLD                    VABFGS
010100020923     C                   KFLD                    V1CKSC
010200981228     C                   KFLD                    VABAAS
010300981228     C                   KFLD                    VABLNP
010400981228     C                   KFLD                    VABNRS
010500981228     C                   KFLD                    VABNSP
010600981228     C**
010700981228     C     *LIKE         DEFINE    V1CKSC        SAVKSC
010800981228     C     *LIKE         DEFINE    V1CRMA        SAVRMA
010900981228     C     *LIKE         DEFINE    V1CRMD        SAVRMD
011000900911     C* ------------------------------------------------------------------
011100990107     C     INIZIO        TAG
011200981228     C                   SETOFF                                       01
011300981228     C                   CLEAR                   SAVRMD
011400981228     C                   CLEAR                   SAVRMA
011500981228     C                   CLEAR                   SAVKSC
011600990105     C                   CLEAR                   V1CKSC
011700990105     C                   CLEAR                   V1DKSC
011800990105     C                   CLEAR                   V1CRMD
011900990105     C                   CLEAR                   V1CRMA
012000990105     C                   CLEAR                   V1CFIL
012100990107     C                   CLEAR                   V1DFIL
012200021015     C                   move      'S'           V1CANN
012300990105     C                   CLEAR                   V1CNBO
012400990107     C                   MOVEL     SIMFEL        V1CKLP
012500020925     C     DUTLPO        IFEQ      '2'
012600020925     C     DUTLPO        OREQ      *BLANKS
012700020925     C                   MOVEL     DUTPOU        V1CKLP
012800990107     C                   END
012900990107     C*
013000990107     C                   SETON                                        35
013100990107     C                   WRITE     LV70C01
013200990107     C                   SETOFF                                       35
013300021002     C* SCRIVO LA PAGINA DI SFL
013400990107     C                   DO        13            NRR2              4 0
013500990107     C                   WRITE     LV70S01
013600990107     C                   ENDDO
013700990107     C                   Z-ADD     1             NRR2
013800981228     C     FOR01         TAG
013900990107     C                   WRITE     LV70Z01
014000990107     C                   EXFMT     LV70C01
014100021017     C                   SETOFF                                       3828
014200981228     C* F3 - FINE
014300981228     C   KC              GOTO      FINE
014400990105     C*
014500990105     C                   SETOFF                                       289040
014600990105     C                   SETOFF                                       414243
014700981228     C* CONTROLLI
014800981228     C                   EXSR      CTRD01
014900981228     C   90              GOTO      FOR01
015000981228     C**
015100021017     C  NKF
015200021017     CANNKJ              GOTO      FOR01
015300021017      **
015400021017      ** F6 - CONFERMA CON CANCELLAZIONE BOLLA,
015500021017      ** F10- CONFERMA SENZA CANCELLAZIONE BOLLA
015600021017      **
015700021017     C   KJ              DO
015800021017     C                   IF        V1CAnn = 'S'
015900021017     C                   MOVEL     MSG(14)       VIDMSG
016000021017     C                   SETON                                          28
016100021017     C                   SETOFF                                       38
016200021017     C                   GOTO      FOR01
016300021017     C                   ELSE
016400021017     C                   EXSR      ELAB
016500021017     C                   ENDIF
016600021017     C                   ENDDO
016700021017      *
016800021017     C   KF              DO
016900021017     C                   IF        V1CAnn = 'N'
017000021017     C                   MOVEL     MSG(13)       VIDMSG
017100021017     C                   SETON                                        3828
017200021017     C                   GOTO      FOR01
017300021017     C                   ELSE
017400021017     C                   EXSR      ELAB
017500021017     C                   ENDIF
017600021017     C                   ENDDO
017700990107     C   KF
017800990107     CAN 90              GOTO      FOR01
017900990107     C**
018000990107     C                   GOTO      INIZIO
018100981228     C**
018200981228     C     FINE          TAG
018300981228     C                   CLEAR                   DSBS69
018400981228     C                   MOVEL     'C'           I69TLA
018500981228     C                   CALL      'TIBS69R'
018600981228     C                   PARM                    DSBS69
018700981228     C                   PARM                    DSACO
018800981228     C                   PARM                    DSIND
018900981228     C                   PARM                    DSCLP
019000981228     C                   PARM                    DSCLS
019100900911     C*
019200900911     C                   SETON                                        LR
019300990107     C**
019400981228     C* CONTROLLI FORMATO 1 ------------------------------------------*
019500981228     C     CTRD01        BEGSR
019600021002     C* IL PO A CUI TRASFERIRE NON PUO' ESSERE IN L1
019700021002    1C     V1CFIL        IFEQ      0
019800990107     C                   MOVEL     MSG(4)        VIDMSG
019900990107     C                   SETON                                        422890
020000990107     C                   GOTO      ENDCTR
020100021002    1C                   ENDIF
020200990107     c*
020300990107     C     V1CFIL        CHAIN     AZORG01L                           30
020400021002    1C     *IN30         IFEQ      *ON
020500990107     C     ORGFVA        ORNE      ' '
020600990107     C     ORGFAG        ORNE      'A'
020700990107     C     ORGFAG        ANDNE     'F'
020800990107     C                   MOVEL     MSG(5)        VIDMSG
020900990107     C                   SETON                                        422890
021000990107     C                   GOTO      ENDCTR
021100021002    1C                   ENDIF
021200990107     C                   MOVEL     ORGDES        V1DFIL
021300990107     c*
021400990107     C     V1CFIL        LOOKUP    L1                                     30
021500021002    1C     *IN30         IFEQ      *ON
021600990107     C                   MOVEL     MSG(8)        VIDMSG
021700990107     C                   SETON                                        422890
021800990107     C                   GOTO      ENDCTR
021900021002    1C                   ENDIF
022000990107     C**
022100990107     C                   Z-ADD     1             NRR2
022200990107     C                   CLEAR                   WTROV
022300990107     C**
022400990107    1C     NRR2          DOWLE     13
022500990107     C     NRR2          CHAIN     LV70S01                            31
022600990107     C**
022700990107    2C     *IN31         IFEQ      *OFF
022800990107     C                   SETOFF                                       01
022900990107     C                   CLEAR                   V1CNBO
023000990107     C** RICERCA ALFABETICA
023100990107    3C   KGV1DKSC        IFNE      *BLANKS
023200990107     C     V1CNUM        ANDEQ     0
023300990107     C* DESCRIZIONE AZIENDA
023400990107     C                   MOVEL     RSUT          PAXDUT           30
023500990107     C* PAXSTA=9 ESCLUDO ANNULLATI
023600990107     C                   Z-ADD     9             PAXSTA
023700990107     C* CAPOCONTO CLIENTI
023800020925     C                   Z-ADD     DUTKCI        PAXCCC
023900990107     C                   MOVEL     V1DKSC        PAXDMT           48
024000990107     C                   CLEAR                   ACOKSC
024100021002    4C     V1CKLP        IFGT      0
024200990107     C                   MOVEL     V1CKLP        PAXFLR
024300021002    4C                   ENDIF
024400990107     C                   CLEAR                   PAXKSM
024500990107     C                   CLEAR                   PAXKCM
024600990107     C                   CLEAR                   PAXKDM
024700991203     C                   Z-ADD     13            PAXNUM
024800990107     C*
024900990107     C                   CALL      'XALFA3BR'
025000990107     C                   PARM                    PAXDUT
025100990107     C                   PARM                    CODUT
025200990107     C                   PARM                    PAXDMT
025300990107     C                   PARM                    PAXCCC            4 0
025400990107     C                   PARM                    PAXSTA            1 0
025500990107     C                   PARM                    PAXFLR           90
025600990107     C                   PARM                    PAXDIT            3
025700991203     C                   PARM                    PAXNUM            2 0
025800990107     C                   PARM                    PAXKCM           80
025900990107     C                   PARM                    PAXKSM          140
026000990107     C                   PARM                    PAXKDM           60
026100990107     C*
026200990107     C* NON TROVATO O PREMUTO CMD12
026300990107    4C     PAXSTA        IFGT      -1
026400990107     C* CODICI SELEZIONATI: IMPOSTO A VIDEO
026500990107     C                   MOVEA     PAXKSM        KSM
026600990107     C                   Z-ADD     NRR2          SAVNRR
026700990107     C     *LIKE         DEFINE    NRR2          SAVNRR
026800990107     C*
026900990107     C                   Z-ADD     1             BB                2 0
027000990107    5C     KSM(BB)       DOWNE     *BLANKS
027100990107     C     NRR2          ANDLE     13
027200990107    6C     BB            IFGT      1
027300990107     C                   UPDATE    LV70S01
027400990107     C                   ADD       1             NRR2
027500990107     C     NRR2          CHAIN     LV70S01                            32
027600990107    6C                   ENDIF
027700990107     C**
027800990107    6C     V1CNUM        IFEQ      0
027900990107     C                   MOVE      KSM(BB)       V1CKSC
028000990107     C                   ADD       1             BB
028100990107    6C                   ENDIF
028200990107     C**
028300990107    5C                   ENDDO
028400990107     C**
028500990107     C                   SETON                                        90
028600990107    5C     KSM(2)        IFNE      *BLANKS
028700990107     C                   UPDATE    LV70S01
028800990107     C**
028900990107     C                   Z-ADD     SAVNRR        NRR2
029000990107     C     NRR2          CHAIN     LV70S01                            31
029100990107    5C                   ENDIF
029200990107    4C                   ENDIF
029300990107    3C                   ENDIF
029400990107     C**
029500990107     C* ELABORO SOLO SE C'E' QUALCHE COSA
029600990107    3C     V1CNUM        IFGT      0
029700990107     C     V1CRMD        ORGT      0
029800990107     C     V1CRMA        ORGT      0
029900981228     C* CLIENTE
030000981228     C                   CLEAR                   DSBS69
030100020923     C                   MOVEL     DUTKCI        I69KCC
030200981228     C                   MOVEL     V1CKSC        I69KAC
030300981228     C                   MOVEL     KNSIF         I69SIF
030400981228     C                   CALL      'TIBS69R'
030500981228     C                   PARM                    DSBS69
030600981228     C                   PARM                    DSACO
030700981228     C                   PARM                    DSIND
030800981228     C                   PARM                    DSCLP
030900981228     C                   PARM                    DSCLS
031000981228     C* ERRORE
031100990107    4C     O69ERR        IFNE      ' '
031200981228     C                   MOVEL     MSG(1)        VIDMSG
031300981228     C                   SETON                                        402890
031400981228     C                   GOTO      ENDCT1
031500990107    4C                   ENDIF
031600990105     C                   MOVEL     ACORAG        V1DKSC
031700051215     c* il p.o. a cui trasferire deve essere abilitato come da tablla
031800051215     c* 3TR --> effettuo il controllo richiamando il pgm tntb51r
031900060518     c****               clear                   tntb50ds
032000060518     c****               movel     v1cksc        b50ke2
032100060518     c****               movel     v1cksc        b50ke1
032200060518     c****               movel     'C'           b50opz
032300060518     c****               movel     v1cfil        b50poa
032400060518     c****               call      'TNTB51R'
032500060518     c****               parm                    kpjba
032600060518     c****               parm                    tntb50ds
032700060518     c****
032800060518     c****               if        b50err<>' '
032900060518     c****               movel     b50msg        vidmsg
033000060518     c****               seton                                        402890
033100060518     c****               goto      endct1
033200060518     c****               endif
033300060518     C****
033400020924     c* NON ESISTONO BOLLE PER IL CLIENTE DIGITATO
033500020924     c* LE BOLLE VALIDE DEVONO INOLTRE AVERE IL P.O DI
033600020924     c* GESTIONE IN L1
033700020924     C     V1CKSC        SETLL     FIVAB01L
033800020924     C     V1CKSC        READE     FIVAB01L                               30
033900020924      *
034000021002    4C     *IN30         DOWEQ     *OFF
034100021002     C     VABFGS        LOOKUP    L1NPPT                                 32
034200020924     C   32              LEAVE
034300020924     C     V1CKSC        READE     FIVAB01L                               30
034400021002    4C                   ENDDO
034500020924      *
034600020924    4C     *IN30         IFEQ      *ON
034700981228     C                   MOVEL     MSG(2)        VIDMSG
034800981228     C                   SETON                                        402890
034900981228     C                   GOTO      ENDCT1
035000990107    4C                   ENDIF
035100981228     C* rif mitt al > dal
035200990107    4C     V1CRMA        IFGT      0
035300981228     C     V1CRMA        ANDLT     V1CRMD
035400981228     C                   MOVEL     MSG(3)        VIDMSG
035500981228     C                   SETON                                        412890
035600981228     C                   GOTO      ENDCT1
035700990107    4C                   ENDIF
035800990107     C**
035900981228     C* CONTROLLO SE ESISTE ALMENO UNA BOLLA
036000020924     c* LE BOLLE VALIDE DEVONO INOLTRE AVERE IL P.O DI
036100020924     c* GESTIONE IN L1
036200990107    4C     V1CRMD        IFGT      0
036300981228     C     V1CRMA        ANDEQ     0
036400020924     C     KVAB          SETLL     FIVAB01L
036500021002     C     KVAB          READE     FIVAB01L                               30
036600020924      *
036700021002    5C     *IN30         DOWEQ     *OFF
036800021002     C     VABFGS        LOOKUP    L1NPPT                                 32
036900020924     C   32              LEAVE
037000021002     C     KVAB          READE     FIVAB01L                               30
037100021002    5C                   ENDDO
037200020924      *
037300021002    5C     *IN30         IFEQ      *ON
037400981228     C                   MOVEL     MSG(6)        VIDMSG
037500990105     C                   SETON                                        412890
037600981228     C                   GOTO      ENDCT1
037700021002    5C                   ENDIF
037800990107    4C                   ENDIF
037900981228     c*
038000990107    4C     V1CRMA        IFGT      0
038100020923     C     KVAB          SETLL     FIVAB01L
038200020923     C     V1CKSC        READE     FIVAB01L                               30
038300020924      *
038400021002    5C     *IN30         DOWEQ     *OFF
038500021002     C     vabfgs        LOOKUP    L1NPPT                                 32
038600020924     C   32              LEAVE
038700020924     C     V1CKSC        READE     FIVAB01L                               30
038800021002    5C                   ENDDO
038900020924      *
039000990107    5C     *IN30         IFEQ      *OFF
039100981228     C     VABRMN        ANDGT     V1CRMA
039200021002     C                   eval      *in30 = *on
039300990107    5C                   ENDIF
039400990107    5C     *IN30         IFEQ      *ON
039500981228     C                   MOVEL     MSG(7)        VIDMSG
039600981228     C                   SETON                                        422890
039700981228     C                   GOTO      ENDCT1
039800990107    5C                   ENDIF
039900990107    4C                   ENDIF
040000990107     C*
040100990107     C                   MOVEL     'S'           WTROV             1
040200990107    3C                   ENDIF
040300981228     C**
040400990107     C     ENDCT1        TAG
040500990107     C**
040600990107     C* SE NON  CI SONO ERRORI CONTO LE BOLLE
040700021002    3C     *IN90         IFEQ      *OFF
040800021002     C     V1CNUM        ANDGT     0
040900021002     C                   EXSR      conta
041000021002     C                   eval      *in01 = *on
041100021002    3C                   ENDIF
041200990107     C*
041300990107     C                   UPDATE    LV70S01
041400021002    2C                   ENDIF
041500990107     C* SE C'E' ERRORE ESCO
041600021002    2C     *IN90         IFEQ      *ON
041700990107     C                   Z-ADD     14            NRR2
041800021002   x2C                   ELSE
041900990107     C                   ADD       1             NRR2
042000021002    2C                   ENDIF
042100021002      *
042200021002    1C                   ENDDO
042300990107     C**
042400990107     C* SE NON IMPOSTATO NEMMENO UN CODICE ERRORE
042500990107     C  N90WTROV         IFEQ      ' '
042600990107     C                   MOVEL     MSG(11)       VIDMSG
042700990107     C                   SETON                                        2890
042800990107     C                   ENDIF
042900021002      *
043000990107     C     ENDCTR        ENDSR
043100021002     C**
043200021002      * CONTO RECORD DA TRASMETTERE-----------------------------------*
043300021002     C     CONTA         BEGSR
043400021002      *
043500021002     C     KVAB          SETLL     FIVAB01L
043600021002     C     V1CKSC        READE     FIVAB01L                               30
043700021002      *
043800021002    1C     *IN30         DOWEQ     *OFF
043900021002     C     vabfgs        LOOKUP    L1NPPT                                 32
044000021002     C   32              LEAVE
044100021002     C     V1CKSC        READE     FIVAB01L                               30
044200021002    1C                   ENDDO
044300021002      *
044400021002    1C     *IN30         IFEQ      *OFF
044500021002      *
044600021002    2C     V1CRMA        IFEQ      *zeros
044700021002     C     V1CRMD        ANDGT     *zeros
044800021002     C                   Z-ADD     1             V1CNBO
044900021002   X2C                   ELSE
045000021002      *
045100021002    3C     *IN30         DOWEQ     *OFF
045200021002    4C     V1CRMA        IFGT      *zeros
045300021002     C     VABRMN        ANDGT     V1CRMA
045400021002     C                   eval      *in30 = *on
045500021002   x4C                   ELSE
045600021002      *
045700021002     C                   ADD       1             V1CNBO
045800021002     C     V1CKSC        READE     FIVAB01L                               30
045900021002      *
046000021002    5C     *IN30         DOWEQ     *OFF
046100021002     C     vabfgs        LOOKUP    L1NPPT                                 32
046200021002     C   32              LEAVE
046300021002     C     V1CKSC        READE     FIVAB01L                               30
046400021002    5C                   ENDDO
046500021002      *
046600021002    4C                   ENDIF
046700021002    3C                   ENDDO
046800021002      *
046900021002    2C                   ENDIF
047000021002    1C                   ENDIF
047100021002      **
047200021002     C                   ENDSR
047300021002     C**
047400981228     C* TRASMETTO I DATI RICHIESTI------------------------------------*
047500981228     C     ELAB          BEGSR
047600021002      *
047700021002     C                   SETOFF                                       3031
047800040604     C                   SETOFF                                       32
047900021001     C                   Z-ADD     1             NRR2
048000021002      *
048100021002      *
048200990107    1C     NRR2          DOWLE     13
048300021002      *
048400990107     C     NRR2          CHAIN     LV70S01                            31
048500990107    2C  N31V1CNUM        IFGT      0
048600020925      *
048700021002     C                   eval      *in34 = *off
048800021002      *
048900981228     C**
049000020926     C**  CERCO LE BOLLE DA TRASMETTERE O SCRIVERE/AGGIORNARE IN LOCALE
049100981228     C**
049200020926     C     KVAB          SETLL     FIVAB01L
049300020930     C     V1CKSC        READE     FIVAB01L                               30
049400020926      *
049500020926      * CERCO IL PRIMO RECORD CHE MI COMPETE
049600020926    3C     *IN30         DOWEQ     *OFF
049700021002     C     VABFGS        LOOKUP    L1NPPT                                 32
049800020926     C   32              LEAVE
049900020930     C     V1CKSC        READE     FIVAB01L                               30
050000020926    3C                   ENDDO
050100020926      *
050200020926      * PROSEGUO SE HO TROVATO UN RECORD VALIDO
050300020926    3C                   IF        *IN30=*OFF
050400020926    4C     *IN34         DOWEQ     *OFF
050500020926      *
050600020926    5C     V1CRMA        IFGT      0
050700981228     C     VABRMN        ANDGT     V1CRMA
050800021002     C                   SETON                                        3034
050900021002    5C                   ENDIF
051000021002    5C     V1CRMA        IFEQ      0
051100021002     C     V1CRMD        ANDGT     0
051200021002     C     VABRMN        ANDNE     V1CRMD
051300021002     C                   SETON                                        3034
051400021002    5C                   ENDIF
051500021002      *
051600021002    5C                   IF        *IN30 = *OFF
051700981228     C** TRASFERISCO ANCHE IL SUO DETTAGLIO SE C'E'
051800021002     C     KVAD          setll     FIVAD01L
051900021002     C     KVAD          READE     FIVAD01L                               31
052000981228     C**
052100020926    6C     *IN31         DOWEQ     *OFF
052200020930     C                   Z-ADD     V1CFIL        VADFGS
052300021002     C                   EXSR      GESLOC_VAD
052400981228     C**
052500020923     C     KVAD          READE     FIVAD01L                               31
052600020926    6C                   ENDDO
052700020424     C**
052800020424     C** TRASFERISCO ANCHE LA SUA ESTENSIONE-DETTAGLIO SE C'E'
052900021002     C     KVAD          setll     FIVAT01L
053000021002     C     KVAD          READE     FIVAT01L                               31
053100020424     C**
053200020926    6C     *IN31         DOWEQ     *OFF
053300020930     C                   Z-ADD     V1CFIL        VATFGS
053400021002     C                   EXSR      GESLOC_VAT
053500020424     C**
053600020923     C     KVAD          READE     FIVAT01L                               31
053700020926    6C                   ENDDO
053800040604     c**
053900040604     C** TRASFERISCO ANCHE LA SUA ESTENSIONE-TESTATA SE C'E'
054000040604     C     KVAD          setll     FIVAX01L
054100040604     C     KVAD          READE     FIVAX01L                               31
054200040604     C**
054300040604    6C     *IN31         DOWEQ     *OFF
054400040604     C                   Z-ADD     V1CFIL        VAXFGS
054500040604     C                   EXSR      GESLOC_VAX
054600040604     C**
054700040604     C     KVAD          READE     FIVAX01L                               31
054800040604    6C                   ENDDO
054801081020     C** TRASFERISCO ANCHE FIRAB
054802081020     C     KVAD          setll     FIRAB02L
054803081020     C     KVAD          READE     FIRAB02L                               31
054804081020     C**
054805081020    6C     *IN31         DOWEQ     *OFF
054806081020     C                   Z-ADD     V1CFIL        RABFGS
054807081020     C                   EXSR      GESLOC_RAB
054808081020     C**
054809081020     C     KVAD          READE     FIRAB02L                               31
054810081020    6C                   ENDDO
054900981228     C**
055000020930     C                   Z-ADD     V1CFIL        VABFGS
055100021002     C                   EXSR      GESLOC_VAB
055200981228     C**
055300020926    5C                   ENDIF
055400020924     C**
055500020926    5C     *IN34         DOWEQ     *OFF
055600020930     C     V1CKSC        READE     FIVAB01L                               34
055700021002     C     VABFGS        LOOKUP    L1NPPT                                 32
055800020926     C   32              LEAVE
055900020926    5C                   ENDDO
056000020926    4C                   ENDDO
056100020926    3C                   ENDIF
056200020926      *
056300021001    2C                   ENDIF
056400990107     C                   ADD       1             NRR2
056500021001    1C                   ENDDO
056600981228     C**
056700981228     C     ENDELA        ENDSR
056800020924     C**----------------------------------
056900020925     C** AGGIORNO/SCRIVO FILES VAD IN LOCALE -------------------------*
057000020924     C**----------------------------------
057100020925     C     GESLOC_VAD    BEGSR
057200020925     C                   IF        V1CANN = 'S'
057300020925     C                   UPDATE    FIVAD000
057400020925     C                   ELSE
057500020925     C                   WRITE     FIVAD000
057600020925     C                   ENDIF
057700020924     C                   ENDSR
057800020925     C**----------------------------------
057900020925     C** AGGIORNO/SCRIVO FILES VAT IN LOCALE -------------------------*
058000020925     C**----------------------------------
058100020925     C     GESLOC_VAT    BEGSR
058200020925     C                   IF        V1CANN = 'S'
058300020925     C                   UPDATE    FIVAT000
058400020925     C                   ELSE
058500020925     C                   WRITE     FIVAT000
058600020925     C                   ENDIF
058700020925     C                   ENDSR
058800040604     C**----------------------------------
058900040604     C** AGGIORNO/SCRIVO FILES VAX IN LOCALE -------------------------*
059000040604     C**----------------------------------
059100040604     C     GESLOC_VAX    BEGSR
059200040604     C                   IF        V1CANN = 'S'
059300040604     C                   UPDATE    FIVAX000
059400040604     C                   ELSE
059500040604     C                   WRITE     FIVAX000
059600040604     C                   ENDIF
059700040604     C                   ENDSR
059701081020     C**----------------------------------
059702081020     C** AGGIORNO/SCRIVO FILES RAB IN LOCALE -------------------------*
059703081020     C**----------------------------------
059704081020     C     GESLOC_RAB    BEGSR
059705081020     C                   IF        V1CANN = 'S'
059706081020     C                   UPDATE    FIRAB000
059707081020     C                   ELSE
059708081020     C                   WRITE     FIRAB000
059709081020     C                   ENDIF
059710081020     C                   ENDSR
059800020925     C**----------------------------------
059900020925     C** AGGIORNO/SCRIVO FILES VAB IN LOCALE -------------------------*
060000020925     C**----------------------------------
060100020925     C     GESLOC_VAB    BEGSR
060200020925     C                   IF        V1CANN = 'S'
060300020926     C                   UPDATE    FIVAB000
060400020925     C                   ELSE
060500020926     C                   WRITE     FIVAB000
060600020925     C                   ENDIF
060700020925     C                   ENDSR
060800990105     C**----------------------------------
060900020424**
061000981228Cliente inesistente                                                           1
061100981228Non esistono bolle da confermare per il cliente digitato                      2
061200981228Il numero riferimento mittente DAL deve essere minore del numero AL           3
061300981228P.O. da trasferire obbligatorio                                               4
061400981228P.O. da trasferire errato                                                     5
061500981228Non esiste la bolla con il riferimento immesso, per il cliente indicato       6
061600981228Non esistono bolle con i riferimenti immessi, per il cliente indicato         7
061700981228Se si trasferisce, indicare P.O. di altro terminal di partenza                8
061800040604                                                                              9
061900040604
062000990107Indicare almeno un codice mittente da trasferire
062100990107Immettere la chiave di ricerca alfabetica
062200021017Confermare con F10 la scelta di non cancellare le bolle dopo il trasferimento
062300021017Premere F6 per confermare
