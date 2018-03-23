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
001200081020     FFIRAB02L  UF A E           K DISK
001300021017     D MSG             S             78    DIM(14) CTDATA PERRCD(1)
001400920921     D*
001500981228     D L1              S              3  0 DIM(30)
001600020926     D L1NPPT          S              3  0 DIM(30)
001700990107     D KSM             S              7    DIM(20)
001800981228     D* DS PER TRUL06R - CARICAMENTO £X
001900981228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002000981228     D  LIN                    1     90  0
002100981228     D                                     DIM(30)
002200981228     D DSBS69        E DS                  EXTNAME(TIBS69DS)
002300981228     D DSACO         E DS                  EXTNAME(CNACO00F)
002400981228     D DSIND         E DS                  EXTNAME(CNIND00F)
002500981228     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002600981228     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002700900911     D KPJBA         E DS
002800020925     D OG143         E DS
002900990107     D                 DS
003000990107     D  V1CKLP                 1      3  0
003100990107     D  V1CNUM                 4      7  0
003200990107     D  V1CKSC                 1      7  0
003300020923      *------------
003400020923     D FNLSE5DS      e ds
003500020923      *------------
003600020923      * DS DATI UTENTE
003700020923      *------------
003800020923     D tibs34ds      e ds                  inz
003900020923     D ddatiute      e ds
004000020923     D azuteds       e ds                  extname(AZUTE00F)
004100051215     D tntb50ds      e ds
004200981228     C* ------------------------------------------------------------------
004300981228     C     *ENTRY        PLIST
004400981228     C                   PARM                    KPJBA
004500020925     C                   Z-ADD     1             CODUT             1 0
004600020923      *
004700020923     c     *dtaara       define    §azute        azuteds
004800020923     c     *dtaara       define    §datiute      ddatiute
004900020923     c                   in(E)     *dtaara
005000020923     c                   IF        %error  or RSUT = *blanks
005100020923     c                   CLEAR                   tibs34ds
005200020923     c                   CALL      'TIBS34R'
005300020923     c                   PARM                    tibs34ds
005400020923     c                   in        *dtaara
005500020923     c                   endif
005600020923     C***
005700981228     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
005800981228     C***
005900981228     C                   CLEAR                   L1
006000981228     C     SIMFEL        IFGT      0
006100981228     C                   CLEAR                   DSUL06
006200981228     C                   MOVE      '£1'          D06COD
006300981228     C                   MOVEL     SIMFEL        D06KEY
006400981228     C                   MOVEL     'L'           D06TLA
006500981228     C                   MOVEL     DSUL06        KPJBU
006600981228     C*
006700981228     C                   CALL      'TRUL06R'
006800981228     C                   PARM                    KPJBA
006900981228     C                   MOVEL     KPJBU         DSUL06
007000981228     C                   MOVEA     LIN           L1
007100981228     C                   ENDIF
007200020923     C***
007300021002     C* COSTRUISCO SCHIERE DA L1 SENZA I P.O POSTE
007400020923     C***
007500020924     C                   Z-ADD     1             Y                 2 0
007600020923     C     Y             DOWLT     30
007700020923     C     L1(Y)         ANDNE     *zeros
007800020923     C                   MOVE      L1(Y)         WORG              3 0
007900020923     C*
008000020923     C     WORG          CHAIN     AZORG01L                           37
008100020923     C                   IF        *IN37=*OFF AND ORGFVA = ' '
008200020923     C                   MOVEL     ORGDE3        OG143
008300020923     C                   ENDIF
008400020923     C     §OGNTW        IFNE      'PPT'
008500020924     C                   Z-ADD     L1(Y)         L1NPPT(Y)
008600020923     c                   ENDIF
008700020924     C                   ADD       1             Y
008800020923     C                   ENDDO
008900021002      *
009000021002      * Richiamo Pgm per spostare dati da FNVAB a FIVAB
009100020923     C                   CLEAR                   FNLSE5DS
009200020923     C                   Z-ADD     DUTPOU        IE5POU
009300020923     C                   CALL      'FNLSE5R'
009400020923     C                   PARM                    FNLSE5DS
009500020923     C
009600981228     C* ------------------------------------------------------------------
009700981228     C     KVAB          KLIST
009800981228     C                   KFLD                    V1CKSC
009900981228     C                   KFLD                    V1CRMD
010000981228     C     KVAD          KLIST
010100021002     C                   KFLD                    VABFGS
010200020923     C                   KFLD                    V1CKSC
010300981228     C                   KFLD                    VABAAS
010400981228     C                   KFLD                    VABLNP
010500981228     C                   KFLD                    VABNRS
010600981228     C                   KFLD                    VABNSP
010700981228     C**
010800981228     C     *LIKE         DEFINE    V1CKSC        SAVKSC
010900981228     C     *LIKE         DEFINE    V1CRMA        SAVRMA
011000981228     C     *LIKE         DEFINE    V1CRMD        SAVRMD
011100900911     C* ------------------------------------------------------------------
011200990107     C     INIZIO        TAG
011300981228     C                   SETOFF                                       01
011400981228     C                   CLEAR                   SAVRMD
011500981228     C                   CLEAR                   SAVRMA
011600981228     C                   CLEAR                   SAVKSC
011700990105     C                   CLEAR                   V1CKSC
011800990105     C                   CLEAR                   V1DKSC
011900990105     C                   CLEAR                   V1CRMD
012000990105     C                   CLEAR                   V1CRMA
012100990105     C                   CLEAR                   V1CFIL
012200990107     C                   CLEAR                   V1DFIL
012300021015     C                   move      'S'           V1CANN
012400990105     C                   CLEAR                   V1CNBO
012500990107     C                   MOVEL     SIMFEL        V1CKLP
012600020925     C     DUTLPO        IFEQ      '2'
012700020925     C     DUTLPO        OREQ      *BLANKS
012800020925     C                   MOVEL     DUTPOU        V1CKLP
012900990107     C                   END
013000990107     C*
013100990107     C                   SETON                                        35
013200990107     C                   WRITE     LV70C01
013300990107     C                   SETOFF                                       35
013400021002     C* SCRIVO LA PAGINA DI SFL
013500990107     C                   DO        13            NRR2              4 0
013600990107     C                   WRITE     LV70S01
013700990107     C                   ENDDO
013800990107     C                   Z-ADD     1             NRR2
013900981228     C     FOR01         TAG
014000990107     C                   WRITE     LV70Z01
014100990107     C                   EXFMT     LV70C01
014200021017     C                   SETOFF                                       3828
014300981228     C* F3 - FINE
014400981228     C   KC              GOTO      FINE
014500990105     C*
014600990105     C                   SETOFF                                       289040
014700990105     C                   SETOFF                                       414243
014800981228     C* CONTROLLI
014900981228     C                   EXSR      CTRD01
015000981228     C   90              GOTO      FOR01
015100981228     C**
015200021017     C  NKF
015300021017     CANNKJ              GOTO      FOR01
015400021017      **
015500021017      ** F6 - CONFERMA CON CANCELLAZIONE BOLLA,
015600021017      ** F10- CONFERMA SENZA CANCELLAZIONE BOLLA
015700021017      **
015800021017     C   KJ              DO
015900021017     C                   IF        V1CAnn = 'S'
016000021017     C                   MOVEL     MSG(14)       VIDMSG
016100021017     C                   SETON                                          28
016200021017     C                   SETOFF                                       38
016300021017     C                   GOTO      FOR01
016400021017     C                   ELSE
016500021017     C                   EXSR      ELAB
016600021017     C                   ENDIF
016700021017     C                   ENDDO
016800021017      *
016900021017     C   KF              DO
017000021017     C                   IF        V1CAnn = 'N'
017100021017     C                   MOVEL     MSG(13)       VIDMSG
017200021017     C                   SETON                                        3828
017300021017     C                   GOTO      FOR01
017400021017     C                   ELSE
017500021017     C                   EXSR      ELAB
017600021017     C                   ENDIF
017700021017     C                   ENDDO
017800990107     C   KF
017900990107     CAN 90              GOTO      FOR01
018000990107     C**
018100990107     C                   GOTO      INIZIO
018200981228     C**
018300981228     C     FINE          TAG
018400981228     C                   CLEAR                   DSBS69
018500981228     C                   MOVEL     'C'           I69TLA
018600981228     C                   CALL      'TIBS69R'
018700981228     C                   PARM                    DSBS69
018800981228     C                   PARM                    DSACO
018900981228     C                   PARM                    DSIND
019000981228     C                   PARM                    DSCLP
019100981228     C                   PARM                    DSCLS
019200900911     C*
019300900911     C                   SETON                                        LR
019400990107     C**
019500981228     C* CONTROLLI FORMATO 1 ------------------------------------------*
019600981228     C     CTRD01        BEGSR
019700021002     C* IL PO A CUI TRASFERIRE NON PUO' ESSERE IN L1
019800021002    1C     V1CFIL        IFEQ      0
019900990107     C                   MOVEL     MSG(4)        VIDMSG
020000990107     C                   SETON                                        422890
020100990107     C                   GOTO      ENDCTR
020200021002    1C                   ENDIF
020300990107     c*
020400990107     C     V1CFIL        CHAIN     AZORG01L                           30
020500021002    1C     *IN30         IFEQ      *ON
020600990107     C     ORGFVA        ORNE      ' '
020700990107     C     ORGFAG        ORNE      'A'
020800990107     C     ORGFAG        ANDNE     'F'
020900990107     C                   MOVEL     MSG(5)        VIDMSG
021000990107     C                   SETON                                        422890
021100990107     C                   GOTO      ENDCTR
021200021002    1C                   ENDIF
021300990107     C                   MOVEL     ORGDES        V1DFIL
021400990107     c*
021500990107     C     V1CFIL        LOOKUP    L1                                     30
021600021002    1C     *IN30         IFEQ      *ON
021700990107     C                   MOVEL     MSG(8)        VIDMSG
021800990107     C                   SETON                                        422890
021900990107     C                   GOTO      ENDCTR
022000021002    1C                   ENDIF
022100990107     C**
022200990107     C                   Z-ADD     1             NRR2
022300990107     C                   CLEAR                   WTROV
022400990107     C**
022500990107    1C     NRR2          DOWLE     13
022600990107     C     NRR2          CHAIN     LV70S01                            31
022700990107     C**
022800990107    2C     *IN31         IFEQ      *OFF
022900990107     C                   SETOFF                                       01
023000990107     C                   CLEAR                   V1CNBO
023100990107     C** RICERCA ALFABETICA
023200990107    3C   KGV1DKSC        IFNE      *BLANKS
023300990107     C     V1CNUM        ANDEQ     0
023400990107     C* DESCRIZIONE AZIENDA
023500990107     C                   MOVEL     RSUT          PAXDUT           30
023600990107     C* PAXSTA=9 ESCLUDO ANNULLATI
023700990107     C                   Z-ADD     9             PAXSTA
023800990107     C* CAPOCONTO CLIENTI
023900020925     C                   Z-ADD     DUTKCI        PAXCCC
024000990107     C                   MOVEL     V1DKSC        PAXDMT           48
024100990107     C                   CLEAR                   ACOKSC
024200021002    4C     V1CKLP        IFGT      0
024300990107     C                   MOVEL     V1CKLP        PAXFLR
024400021002    4C                   ENDIF
024500990107     C                   CLEAR                   PAXKSM
024600990107     C                   CLEAR                   PAXKCM
024700990107     C                   CLEAR                   PAXKDM
024800991203     C                   Z-ADD     13            PAXNUM
024900990107     C*
025000990107     C                   CALL      'XALFA3BR'
025100990107     C                   PARM                    PAXDUT
025200990107     C                   PARM                    CODUT
025300990107     C                   PARM                    PAXDMT
025400990107     C                   PARM                    PAXCCC            4 0
025500990107     C                   PARM                    PAXSTA            1 0
025600990107     C                   PARM                    PAXFLR           90
025700990107     C                   PARM                    PAXDIT            3
025800991203     C                   PARM                    PAXNUM            2 0
025900990107     C                   PARM                    PAXKCM           80
026000990107     C                   PARM                    PAXKSM          140
026100990107     C                   PARM                    PAXKDM           60
026200990107     C*
026300990107     C* NON TROVATO O PREMUTO CMD12
026400990107    4C     PAXSTA        IFGT      -1
026500990107     C* CODICI SELEZIONATI: IMPOSTO A VIDEO
026600990107     C                   MOVEA     PAXKSM        KSM
026700990107     C                   Z-ADD     NRR2          SAVNRR
026800990107     C     *LIKE         DEFINE    NRR2          SAVNRR
026900990107     C*
027000990107     C                   Z-ADD     1             BB                2 0
027100990107    5C     KSM(BB)       DOWNE     *BLANKS
027200990107     C     NRR2          ANDLE     13
027300990107    6C     BB            IFGT      1
027400990107     C                   UPDATE    LV70S01
027500990107     C                   ADD       1             NRR2
027600990107     C     NRR2          CHAIN     LV70S01                            32
027700990107    6C                   ENDIF
027800990107     C**
027900990107    6C     V1CNUM        IFEQ      0
028000990107     C                   MOVE      KSM(BB)       V1CKSC
028100990107     C                   ADD       1             BB
028200990107    6C                   ENDIF
028300990107     C**
028400990107    5C                   ENDDO
028500990107     C**
028600990107     C                   SETON                                        90
028700990107    5C     KSM(2)        IFNE      *BLANKS
028800990107     C                   UPDATE    LV70S01
028900990107     C**
029000990107     C                   Z-ADD     SAVNRR        NRR2
029100990107     C     NRR2          CHAIN     LV70S01                            31
029200990107    5C                   ENDIF
029300990107    4C                   ENDIF
029400990107    3C                   ENDIF
029500990107     C**
029600990107     C* ELABORO SOLO SE C'E' QUALCHE COSA
029700990107    3C     V1CNUM        IFGT      0
029800990107     C     V1CRMD        ORGT      0
029900990107     C     V1CRMA        ORGT      0
030000981228     C* CLIENTE
030100981228     C                   CLEAR                   DSBS69
030200020923     C                   MOVEL     DUTKCI        I69KCC
030300981228     C                   MOVEL     V1CKSC        I69KAC
030400981228     C                   MOVEL     KNSIF         I69SIF
030500981228     C                   CALL      'TIBS69R'
030600981228     C                   PARM                    DSBS69
030700981228     C                   PARM                    DSACO
030800981228     C                   PARM                    DSIND
030900981228     C                   PARM                    DSCLP
031000981228     C                   PARM                    DSCLS
031100981228     C* ERRORE
031200990107    4C     O69ERR        IFNE      ' '
031300981228     C                   MOVEL     MSG(1)        VIDMSG
031400981228     C                   SETON                                        402890
031500981228     C                   GOTO      ENDCT1
031600990107    4C                   ENDIF
031700990105     C                   MOVEL     ACORAG        V1DKSC
031800051215     c* il p.o. a cui trasferire deve essere abilitato come da tablla
031900051215     c* 3TR --> effettuo il controllo richiamando il pgm tntb51r
032000060518     c****               clear                   tntb50ds
032100060518     c****               movel     v1cksc        b50ke2
032200060518     c****               movel     v1cksc        b50ke1
032300060518     c****               movel     'C'           b50opz
032400060518     c****               movel     v1cfil        b50poa
032500060518     c****               call      'TNTB51R'
032600060518     c****               parm                    kpjba
032700060518     c****               parm                    tntb50ds
032800060518     c****
032900060518     c****               if        b50err<>' '
033000060518     c****               movel     b50msg        vidmsg
033100060518     c****               seton                                        402890
033200060518     c****               goto      endct1
033300060518     c****               endif
033400060518     C****
033500020924     c* NON ESISTONO BOLLE PER IL CLIENTE DIGITATO
033600020924     c* LE BOLLE VALIDE DEVONO INOLTRE AVERE IL P.O DI
033700020924     c* GESTIONE IN L1
033800020924     C     V1CKSC        SETLL     FIVAB01L
033900020924     C     V1CKSC        READE     FIVAB01L                               30
034000020924      *
034100021002    4C     *IN30         DOWEQ     *OFF
034200021002     C     VABFGS        LOOKUP    L1NPPT                                 32
034300020924     C   32              LEAVE
034400020924     C     V1CKSC        READE     FIVAB01L                               30
034500021002    4C                   ENDDO
034600020924      *
034700020924    4C     *IN30         IFEQ      *ON
034800981228     C                   MOVEL     MSG(2)        VIDMSG
034900981228     C                   SETON                                        402890
035000981228     C                   GOTO      ENDCT1
035100990107    4C                   ENDIF
035200981228     C* rif mitt al > dal
035300990107    4C     V1CRMA        IFGT      0
035400981228     C     V1CRMA        ANDLT     V1CRMD
035500981228     C                   MOVEL     MSG(3)        VIDMSG
035600981228     C                   SETON                                        412890
035700981228     C                   GOTO      ENDCT1
035800990107    4C                   ENDIF
035900990107     C**
036000981228     C* CONTROLLO SE ESISTE ALMENO UNA BOLLA
036100020924     c* LE BOLLE VALIDE DEVONO INOLTRE AVERE IL P.O DI
036200020924     c* GESTIONE IN L1
036300990107    4C     V1CRMD        IFGT      0
036400981228     C     V1CRMA        ANDEQ     0
036500020924     C     KVAB          SETLL     FIVAB01L
036600021002     C     KVAB          READE     FIVAB01L                               30
036700020924      *
036800021002    5C     *IN30         DOWEQ     *OFF
036900021002     C     VABFGS        LOOKUP    L1NPPT                                 32
037000020924     C   32              LEAVE
037100021002     C     KVAB          READE     FIVAB01L                               30
037200021002    5C                   ENDDO
037300020924      *
037400021002    5C     *IN30         IFEQ      *ON
037500981228     C                   MOVEL     MSG(6)        VIDMSG
037600990105     C                   SETON                                        412890
037700981228     C                   GOTO      ENDCT1
037800021002    5C                   ENDIF
037900990107    4C                   ENDIF
038000981228     c*
038100990107    4C     V1CRMA        IFGT      0
038200020923     C     KVAB          SETLL     FIVAB01L
038300020923     C     V1CKSC        READE     FIVAB01L                               30
038400020924      *
038500021002    5C     *IN30         DOWEQ     *OFF
038600021002     C     vabfgs        LOOKUP    L1NPPT                                 32
038700020924     C   32              LEAVE
038800020924     C     V1CKSC        READE     FIVAB01L                               30
038900021002    5C                   ENDDO
039000020924      *
039100990107    5C     *IN30         IFEQ      *OFF
039200981228     C     VABRMN        ANDGT     V1CRMA
039300021002     C                   eval      *in30 = *on
039400990107    5C                   ENDIF
039500990107    5C     *IN30         IFEQ      *ON
039600981228     C                   MOVEL     MSG(7)        VIDMSG
039700981228     C                   SETON                                        422890
039800981228     C                   GOTO      ENDCT1
039900990107    5C                   ENDIF
040000990107    4C                   ENDIF
040100990107     C*
040200990107     C                   MOVEL     'S'           WTROV             1
040300990107    3C                   ENDIF
040400981228     C**
040500990107     C     ENDCT1        TAG
040600990107     C**
040700990107     C* SE NON  CI SONO ERRORI CONTO LE BOLLE
040800021002    3C     *IN90         IFEQ      *OFF
040900021002     C     V1CNUM        ANDGT     0
041000021002     C                   EXSR      conta
041100021002     C                   eval      *in01 = *on
041200021002    3C                   ENDIF
041300990107     C*
041400990107     C                   UPDATE    LV70S01
041500021002    2C                   ENDIF
041600990107     C* SE C'E' ERRORE ESCO
041700021002    2C     *IN90         IFEQ      *ON
041800990107     C                   Z-ADD     14            NRR2
041900021002   x2C                   ELSE
042000990107     C                   ADD       1             NRR2
042100021002    2C                   ENDIF
042200021002      *
042300021002    1C                   ENDDO
042400990107     C**
042500990107     C* SE NON IMPOSTATO NEMMENO UN CODICE ERRORE
042600990107     C  N90WTROV         IFEQ      ' '
042700990107     C                   MOVEL     MSG(11)       VIDMSG
042800990107     C                   SETON                                        2890
042900990107     C                   ENDIF
043000021002      *
043100990107     C     ENDCTR        ENDSR
043200021002     C**
043300021002      * CONTO RECORD DA TRASMETTERE-----------------------------------*
043400021002     C     CONTA         BEGSR
043500021002      *
043600021002     C     KVAB          SETLL     FIVAB01L
043700021002     C     V1CKSC        READE     FIVAB01L                               30
043800021002      *
043900021002    1C     *IN30         DOWEQ     *OFF
044000021002     C     vabfgs        LOOKUP    L1NPPT                                 32
044100021002     C   32              LEAVE
044200021002     C     V1CKSC        READE     FIVAB01L                               30
044300021002    1C                   ENDDO
044400021002      *
044500021002    1C     *IN30         IFEQ      *OFF
044600021002      *
044700021002    2C     V1CRMA        IFEQ      *zeros
044800021002     C     V1CRMD        ANDGT     *zeros
044900021002     C                   Z-ADD     1             V1CNBO
045000021002   X2C                   ELSE
045100021002      *
045200021002    3C     *IN30         DOWEQ     *OFF
045300021002    4C     V1CRMA        IFGT      *zeros
045400021002     C     VABRMN        ANDGT     V1CRMA
045500021002     C                   eval      *in30 = *on
045600021002   x4C                   ELSE
045700021002      *
045800021002     C                   ADD       1             V1CNBO
045900021002     C     V1CKSC        READE     FIVAB01L                               30
046000021002      *
046100021002    5C     *IN30         DOWEQ     *OFF
046200021002     C     vabfgs        LOOKUP    L1NPPT                                 32
046300021002     C   32              LEAVE
046400021002     C     V1CKSC        READE     FIVAB01L                               30
046500021002    5C                   ENDDO
046600021002      *
046700021002    4C                   ENDIF
046800021002    3C                   ENDDO
046900021002      *
047000021002    2C                   ENDIF
047100021002    1C                   ENDIF
047200021002      **
047300021002     C                   ENDSR
047400021002     C**
047500981228     C* TRASMETTO I DATI RICHIESTI------------------------------------*
047600981228     C     ELAB          BEGSR
047700021002      *
047800021002     C                   SETOFF                                       3031
047900040604     C                   SETOFF                                       32
048000021001     C                   Z-ADD     1             NRR2
048100021002      *
048200021002      *
048300990107    1C     NRR2          DOWLE     13
048400021002      *
048500990107     C     NRR2          CHAIN     LV70S01                            31
048600990107    2C  N31V1CNUM        IFGT      0
048700020925      *
048800021002     C                   eval      *in34 = *off
048900021002      *
049000981228     C**
049100020926     C**  CERCO LE BOLLE DA TRASMETTERE O SCRIVERE/AGGIORNARE IN LOCALE
049200981228     C**
049300020926     C     KVAB          SETLL     FIVAB01L
049400020930     C     V1CKSC        READE     FIVAB01L                               30
049500020926      *
049600020926      * CERCO IL PRIMO RECORD CHE MI COMPETE
049700020926    3C     *IN30         DOWEQ     *OFF
049800021002     C     VABFGS        LOOKUP    L1NPPT                                 32
049900020926     C   32              LEAVE
050000020930     C     V1CKSC        READE     FIVAB01L                               30
050100020926    3C                   ENDDO
050200020926      *
050300020926      * PROSEGUO SE HO TROVATO UN RECORD VALIDO
050400020926    3C                   IF        *IN30=*OFF
050500020926    4C     *IN34         DOWEQ     *OFF
050600020926      *
050700020926    5C     V1CRMA        IFGT      0
050800981228     C     VABRMN        ANDGT     V1CRMA
050900021002     C                   SETON                                        3034
051000021002    5C                   ENDIF
051100021002    5C     V1CRMA        IFEQ      0
051200021002     C     V1CRMD        ANDGT     0
051300021002     C     VABRMN        ANDNE     V1CRMD
051400021002     C                   SETON                                        3034
051500021002    5C                   ENDIF
051600021002      *
051700021002    5C                   IF        *IN30 = *OFF
051800981228     C** TRASFERISCO ANCHE IL SUO DETTAGLIO SE C'E'
051900021002     C     KVAD          setll     FIVAD01L
052000021002     C     KVAD          READE     FIVAD01L                               31
052100981228     C**
052200020926    6C     *IN31         DOWEQ     *OFF
052300020930     C                   Z-ADD     V1CFIL        VADFGS
052400021002     C                   EXSR      GESLOC_VAD
052500981228     C**
052600020923     C     KVAD          READE     FIVAD01L                               31
052700020926    6C                   ENDDO
052800020424     C**
052900020424     C** TRASFERISCO ANCHE LA SUA ESTENSIONE-DETTAGLIO SE C'E'
053000021002     C     KVAD          setll     FIVAT01L
053100021002     C     KVAD          READE     FIVAT01L                               31
053200020424     C**
053300020926    6C     *IN31         DOWEQ     *OFF
053400020930     C                   Z-ADD     V1CFIL        VATFGS
053500021002     C                   EXSR      GESLOC_VAT
053600020424     C**
053700020923     C     KVAD          READE     FIVAT01L                               31
053800020926    6C                   ENDDO
053900040604     c**
054000040604     C** TRASFERISCO ANCHE LA SUA ESTENSIONE-TESTATA SE C'E'
054100040604     C     KVAD          setll     FIVAX01L
054200040604     C     KVAD          READE     FIVAX01L                               31
054300040604     C**
054400040604    6C     *IN31         DOWEQ     *OFF
054500040604     C                   Z-ADD     V1CFIL        VAXFGS
054600040604     C                   EXSR      GESLOC_VAX
054700040604     C**
054800040604     C     KVAD          READE     FIVAX01L                               31
054900040604    6C                   ENDDO
055000081020     C** TRASFERISCO ANCHE FIRAB
055100081020     C     KVAD          setll     FIRAB02L
055200081020     C     KVAD          READE     FIRAB02L                               31
055300081020     C**
055400081020    6C     *IN31         DOWEQ     *OFF
055500081020     C                   Z-ADD     V1CFIL        RABFGS
055600081020     C                   EXSR      GESLOC_RAB
055700081020     C**
055800081020     C     KVAD          READE     FIRAB02L                               31
055900081020    6C                   ENDDO
056000981228     C**
056100020930     C                   Z-ADD     V1CFIL        VABFGS
056200021002     C                   EXSR      GESLOC_VAB
056300981228     C**
056400020926    5C                   ENDIF
056500020924     C**
056600020926    5C     *IN34         DOWEQ     *OFF
056700020930     C     V1CKSC        READE     FIVAB01L                               34
056800021002     C     VABFGS        LOOKUP    L1NPPT                                 32
056900020926     C   32              LEAVE
057000020926    5C                   ENDDO
057100020926    4C                   ENDDO
057200020926    3C                   ENDIF
057300020926      *
057400021001    2C                   ENDIF
057500990107     C                   ADD       1             NRR2
057600021001    1C                   ENDDO
057700981228     C**
057800981228     C     ENDELA        ENDSR
057900020924     C**----------------------------------
058000020925     C** AGGIORNO/SCRIVO FILES VAD IN LOCALE -------------------------*
058100020924     C**----------------------------------
058200020925     C     GESLOC_VAD    BEGSR
058300020925     C                   IF        V1CANN = 'S'
058400020925     C                   UPDATE    FIVAD000
058500020925     C                   ELSE
058600020925     C                   WRITE     FIVAD000
058700020925     C                   ENDIF
058800020924     C                   ENDSR
058900020925     C**----------------------------------
059000020925     C** AGGIORNO/SCRIVO FILES VAT IN LOCALE -------------------------*
059100020925     C**----------------------------------
059200020925     C     GESLOC_VAT    BEGSR
059300020925     C                   IF        V1CANN = 'S'
059400020925     C                   UPDATE    FIVAT000
059500020925     C                   ELSE
059600020925     C                   WRITE     FIVAT000
059700020925     C                   ENDIF
059800020925     C                   ENDSR
059900040604     C**----------------------------------
060000040604     C** AGGIORNO/SCRIVO FILES VAX IN LOCALE -------------------------*
060100040604     C**----------------------------------
060200040604     C     GESLOC_VAX    BEGSR
060300040604     C                   IF        V1CANN = 'S'
060400040604     C                   UPDATE    FIVAX000
060500040604     C                   ELSE
060600040604     C                   WRITE     FIVAX000
060700040604     C                   ENDIF
060800040604     C                   ENDSR
060900081020     C**----------------------------------
061000081020     C** AGGIORNO/SCRIVO FILES RAB IN LOCALE -------------------------*
061100081020     C**----------------------------------
061200081020     C     GESLOC_RAB    BEGSR
061300081020     C                   IF        V1CANN = 'S'
061400081020     C                   UPDATE    FIRAB000
061500081020     C                   ELSE
061600081020     C                   WRITE     FIRAB000
061700081020     C                   ENDIF
061800081020     C                   ENDSR
061900020925     C**----------------------------------
062000020925     C** AGGIORNO/SCRIVO FILES VAB IN LOCALE -------------------------*
062100020925     C**----------------------------------
062200020925     C     GESLOC_VAB    BEGSR
062300020925     C                   IF        V1CANN = 'S'
062400020926     C                   UPDATE    FIVAB000
062500020925     C                   ELSE
062600020926     C                   WRITE     FIVAB000
062700020925     C                   ENDIF
062800020925     C                   ENDSR
062900990105     C**----------------------------------
063000020424**
063100981228Cliente inesistente                                                           1
063200981228Non esistono bolle da confermare per il cliente digitato                      2
063300981228Il numero riferimento mittente DAL deve essere minore del numero AL           3
063400121022Filiale da trasferire obbligatoria                                            4
063500121022Filiale da trasferire errata                                                  5
063600981228Non esiste la bolla con il riferimento immesso, per il cliente indicato       6
063700981228Non esistono bolle con i riferimenti immessi, per il cliente indicato         7
063800121022Se si trasferisce, indicare filiale di altro terminal di partenza             8
063900040604                                                                              9
064000040604
064100990107Indicare almeno un codice mittente da trasferire
064200990107Immettere la chiave di ricerca alfabetica
064300021017Confermare con F10 la scelta di non cancellare le bolle dopo il trasferimento
064400021017Premere F6 per confermare
