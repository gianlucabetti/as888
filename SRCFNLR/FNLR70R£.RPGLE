000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941109     H* FNLR70R *----------------------------------------------------*
000300970507     H*         QUADRATURA DISTINTA AUTOTRASPORTATORE FILTRO
000400000000     H*--------------------------------------------------------------*
000500941109     FFNLR70D   CF   E             WORKSTN
000600941109     FFNFVV01L  IF   E           K DISK
000700941109     FFNFVV02L  IF   E           K DISK
000800941109     F                                     RENAME(FNFVV000:FNFVV002)
000900000126     FFIBRV01L  IF   E           K DISK
001000020911     FFNARB78L  IF   E           K DISK
001100920831     D CNCR80        E DS
001200920831     D KPJBA         E DS
001300020429     D***                                     1   3 REM
001400020429     D***                                     4   6 REMFIL
001500941109     D***
001600941109     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
001700941109     D***
001800941109     D WLBDAT          DS                  INZ
001900941109     D  G02DAT                 1      8  0
002000941109     D  G02INV                 9     16  0
002100941109     D  G02ERR                17     17
002200941109     D  G02TGI                18     22  0
002300941109     D***
002400941109     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
002500941109     D***
002600941109     D WGIDAT          DS                  INZ
002700941109     D  GIODAT                 1      8  0
002800941109     D  GIOINV                 9     16  0
002900941109     D  GIOTGI                17     21  0
003000941109     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
003100941109     D DSLV50        E DS                  EXTNAME(FNLV50DS)
003200941109     D*
003300941109     D DSLR01        E DS
003400920831     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003500020911
003600020911     d Karbifp         s                   Like(ArbIfp)
003700020911     d Karbndc         s                   Like(ArbNdc)
003800020911
003900000000     I/SPACE 3
004000900702     C****************************************************************
004100900702     C*  RIEPILOGO INDICATORI
004200900702     C***************************************************************
004300920831     C* 01  -  NUMERO DISTINTA = 0  E  DATA DISTINTA >< 0
004400920831     C* 03  -  SONO UN REMOTO
004500920831     C* 04  -  CAMBIO FILIALE ATTIVATO
004600920831     C* 05  -  ESISTONO NUMERO DISTINTA DAL E NUMERO DISTINTA AL
004700920831     C* 09  -  LOKUP RIUSCITA
004800920831     C* 30/35 - DI COMODO
004900920522     C* 40  -  ERRORE : EFFETTUARE ALMENO UNA SCELTA
005000920522     C* 41  -  ERRORE : NUMERO DISTINTA NON UTILIZZABILE(ANNULLATO)
005100920522     C* 42  -  ERRORE : NON ESISTONO SPUNTE PER IL NUMERO DISTINTA
005200920421     C* 43  -  ERRORE : NUMERO DISTINTA INESISTENTE
005300920522     C* 45  -  ERRORE : DATA DISTINTA ERRATA
005400920522     C* 46  -  ERRORE : NUMERO DISTINTA NON APPARTENENTE ALLA DATA
005500920522     C* 47  -  ERRORE : NON ESISTONO BOLLE PER IL NUMERO DISTINTA
005600920528     C* 48  -  ERRORE : MANCA NUMERO DISTINTA DAL
005700920528     C* 49  -  ERRORE : NUM.DISTINTA AL DEVE ESSERE >= A NUMERO DAL
005800920831     C* 50/51  ERRORE : FILIALE DA GESTIRE
005900941109     C* 52  -  ERRORE : FOGLIO GIA' CHIUSO
006000950411     C* 53  -  ERRORE : NUM.DISTINTA INCONGRUENTE CON PRESTAZ.PADRONC.
006100950412     C* 54  -  SE NON E' STATA ESEGUITA ALCUNA SOTTOMISSIONE --> AVVISO
006200920421     C* 90  -  ERRORE GENERICO
006300900702     C*****************************************************************
006400000000     C     *ENTRY        PLIST
006500000000     C                   PARM                    KPJBA
006600941109     C*---------------------------------------------------------------*
006700950411     C*
006800950411     C     INIZIO        TAG
006900950411     C* PULIZIA INZIALE DEL VIDEO
007000950413     C                   Z-ADD     W0080         VIDDDC
007100950411     C                   MOVEL     *BLANKS       VIDNDD
007200950411     C                   MOVEL     *ZEROS        VIDNDA
007300950411     C                   MOVEL     *BLANKS       VIDDES
007400950411     C                   Z-ADD     4             COMNPG
007500950411     C                   CLEAR                   VIDFPP
007600950411     C*
007700900509     C     FOR01         TAG
007800941109     C                   EXFMT     LR70D01
007900941109     C**
008000941109     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
008100941109     C                   SETOFF                                       5028
008200941109     C** F3 - FINE LAVORO
008300900509     C   KC              GOTO      FINE
008400941109     C*
008500941109     C** F18- CAMBIO FILIALE
008600920831     C   KS              SETON                                        04
008700920831     C   KS              GOTO      FOR01
008800941109     C*
008900941109     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
009000941109     C   04              EXSR      CTRFGS
009100941109     C   04
009200941109     CAN 90              GOTO      FOR01
009300920831     C*
009400910801     C*  CONTROLLI
009500910801     C                   EXSR      CONTR
009600941109     C  NKF
009700910801     COR 90              GOTO      FOR01
009800941109     C**
009900941109     C* F6 - ELABORAZIONE
010000941109     C**
010100920528     C                   MOVEL     VIDNDD        NDD
010200941109     C                   CLEAR                   DSLR01
010300941109     C*
010400941109    1C     VIDDDC        IFEQ      *ZEROS
010500920528     C*
010600941109    2C     VIDNDA        IFEQ      0
010700941109     C                   Z-ADD     NDD           VIDNDA
010800941109    2C                   ENDIF
010900941109     C*
011000941109    2C     NDD           DOWLE     VIDNDA
011100920528     C                   MOVEL     NDD           COMNFV
011200920528     C*
011300941109     C     KFVV          CHAIN     FNFVV000                           32
011400941109     C*
011500950411     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
011600950411    3C  N32VIDFPP        IFNE      ' '
011700950411     C     VIDFPP        ANDNE     FVVFPP
011800950411     C                   SETON                                        32
011900950411    3C                   ENDIF
012000950411     C*
012100941109     C* SOTTOMETTO ELABORAZIONE SE NON E' ANNULLATA E CHIUSA
012200941109    3C  N32FVVATB        IFEQ      *BLANKS
012300941109     C     FVVFCF        ANDEQ     ' '
012400941109     C                   Z-ADD     4             DLRNPG
012500941109     C                   Z-ADD     NDD           DLRNFV
012600941109     C                   Z-ADD     VIDFIL        DLRFGS
012700941109     C*
012800941109     C                   MOVEL     DSLR01        KPJBU
012900941109     C                   MOVEL     'LR71'        KCOAZ
013000900702     C                   CALL      'BCH10'
013100900702     C                   PARM                    KPJBA
013200941109    3C                   ENDIF
013300920528     C*
013400920528     C                   ADD       1             NDD
013500941109    2C                   END
013600910320     C*
013700941109   X1C                   ELSE
013800950412     C*
013900950412     C* ACCENDO L'INDICATORE 54 PERCHE' SE SUCESSIVAMENTE NON VERRA'
014000950412     C*   SPENTO, LO UTILIZZO PER EMETTERE MESSAGGIO A VIDEO
014100950412     C                   SETON                                        54
014200941109     C*
014300941109     C* 01 ON - NUMERO DISTINTA = 0  E  DATA DISTINTA <> 0
014400941109     C* FIN TANTO CHE CI SONO FOGLI  CHE RIENTRANO NELLA
014500920522     C* DATA IMMESSA LANCIO L'AZIONE
014600941109     C     KFVV2         SETLL     FNFVV002
014700941109     C     KFVV2         READE     FNFVV002                               32
014800941109     C*
014900920522     C     *IN32         DOWEQ     '0'
015000941109     C*
015100920522     C* PRIMA DI LANCIARE CONTROLLO VALIDITA' DEL  F.V.
015200920522     C     FVVATB        IFEQ      *BLANKS
015300920528     C*
015400920528     C* NUMERO DISTINTA DAL SENZA AL
015500920717     C  N05NDD           IFGT      0
015600920702     C     FVVNFV        ANDNE     NDD
015700920528     C                   GOTO      LEGGI
015800920528     C                   END
015900941109     C*
016000920528     C* NUMERO DISTINTA DAL E NUMERO AL
016100920528     C   05FVVNFV        IFLT      NDD
016200920528     C                   GOTO      LEGGI
016300920528     C                   END
016400941109     C*
016500920528     C   05FVVNFV        IFGT      VIDNDA
016600920528     C                   GOTO      LEGGI
016700920528     C                   END
016800920528     C*
016900950411     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
017000950411     C     VIDFPP        IFNE      ' '
017100950411     C     VIDFPP        ANDNE     FVVFPP
017200950411     C                   GOTO      LEGGI
017300950411     C                   ENDIF
017400950411     C*
017500950412     C* 54 OFF - E' STATA ESEGUITA ALMENO UNA SOTTOMISSIONE
017600950412     C                   SETOFF                                       54
017700950412     C*
017800941109     C                   Z-ADD     4             DLRNPG
017900950123     C                   Z-ADD     FVVNFV        DLRNFV
018000950123     C                   Z-ADD     FVVFGS        DLRFGS
018100941109     C                   MOVEL     DSLR01        KPJBU
018200941109     C                   MOVEL     'LR71'        KCOAZ
018300920522     C                   CALL      'BCH10'
018400920522     C                   PARM                    KPJBA
018500920522     C                   END
018600920528     C*
018700920528     C     LEGGI         TAG
018800941109     C     KFVV2         READE     FNFVV002                               32
018900920522     C                   END
019000950412     C*
019100950412     C* SE NON E' STATA ESEGUITA ALCUNA SOOTOMISSIONE --> AVVISO
019200950412     C   54              GOTO      FOR01
019300950412    1C                   END
019400920522     C*
019500950411     C                   GOTO      INIZIO
019600000000     C     FINE          TAG
019700000000     C                   SETON                                        LR
019800941109     C*
019900941109     C*---------------------------------------------------------------*
020000941109     C     *INZSR        BEGSR
020100941109     C**
020200941109     C* KLIST
020300941109     C**
020400941109     C     KFVV          KLIST
020500941109     C                   KFLD                    COMNPG            1 0
020600941109     C                   KFLD                    COMNFV            5 0
020700941109     C                   KFLD                    VIDFIL
020800941109     C     KFVV2         KLIST
020900941109     C                   KFLD                    COMNPG
021000941109     C                   KFLD                    COMDFV            8 0
021100941109     C                   KFLD                    VIDFIL
021200000126     C     KBRV          KLIST
021300000126     C                   KFLD                    COMNPG            1 0
021400000126     C                   KFLD                    WNFV
021500000126     C                   KFLD                    VIDFIL
021600020911
021700020911     c     Karb78        Klist
021800020911     c                   Kfld                    Karbifp
021900020911     c                   Kfld                    Karbndc
022000000126     C*
022100000126     C     *LIKE         DEFINE    BRVNFV        WNFV
022200941109     C*
022300941109     C                   Z-ADD     1             CODUT
022400941109     C                   CALL      'X§PARUT'
022500941109     C                   PARM                    UT§DSE
022600941109     C                   MOVEL     REC80         CNCR80
022700941109     C                   MOVEL     RAGUT         VIDDSA
022800941109     C*
022900941109     C* CAMPI DI PASSAGGIO
023000941109     C                   CLEAR                   DSLR01
023100941109     C                   MOVEL     4             DLRNPG
023200941109     C                   MOVEL     'FNLR70R'     DLRPGM
023300941109     C                   MOVEL     'S'           DLRRIC
023400941109     C                   MOVEL     '2'           DLRTFV
023500941109     C*
023600950413     C* DATA DEL GIORNO
023700941109     C                   TIME                    W0140            14 0
023800941109     C                   MOVE      W0140         W0080             8 0
023900941109     C**
024000020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
024100941109     C**
024200020429     C***        REM       IFEQ 'REM'
024300020429     C***        REMFIL    ANDGT*ZEROS
024400020429     C***                  MOVELREMFIL    VIDFIL
024500020429     C***                  SETON                     03
024600020429     C***                  ELSE
024700020429     C***                  MOVELSIMFEL    VIDFIL
024800020429     C***                  END
024900020429     C                   Z-ADD     SIMPOU        VIDFIL
025000020429     C     SIMTPP        IFEQ      '2'
025100020429     C     SIMTPP        OREQ      *BLANKS
025200020429     C                   SETON                                        03
025300020429     C                   ENDIF
025400941109     C*
025500941109     C                   ENDSR
025600910320     C*
025700910320     C* CONTROLLO CAMPI -----------------------------------------------
025800920421     C*
025900910320     C     CONTR         BEGSR
026000920421     C*
026100920528     C                   SETOFF                                       019005
026200920521     C* SE NUMERO E DATA DISTINTA CONSEGNA = 0 : ERRORE
026300920528     C     VIDNDD        IFEQ      *BLANKS
026400920528     C     VIDNDD        OREQ      *ZEROS
026500920528     C     VIDNDA        IFNE      *ZEROS
026600920528     C                   SETON                                        4890
026700920528     C                   GOTO      ENDCT1
026800920528     C                   END
026900920521     C     VIDDDC        IFEQ      0
027000911015     C                   SETON                                        4090
027100911015     C                   GOTO      ENDCT1
027200920522     C                   END
027300920522     C                   END
027400920522     C* DATA DISTINTA >< 0
027500920522     C     VIDDDC        IFGT      0
027600920522     C* GIRO DATA DISTINTA
027700920522     C                   MOVE      VIDDDC        G02DAT
027800920522     C                   MOVEL     *BLANKS       G02ERR
027900941109     C                   CALL      'XSRDA8'
028000920522     C                   PARM                    WLBDAT
028100920522     C                   Z-ADD     G02INV        COMDFV
028200941109     C                   Z-ADD     G02DAT        VIDDDC
028300941109     C*
028400920522     C* CONTROLLO SE DATA DISTINTA ESISTE
028500941109     C     KFVV2         SETLL     FNFVV002                               31
028600941109     C     *IN31         IFEQ      *OFF
028700941109     C                   SETON                                        4590
028800941109     C                   GOTO      ENDCT1
028900941109     C                   ELSE
029000941109     C                   SETON                                        01
029100941109     C                   ENDIF
029200920521     C                   END
029300911015     C*
029400920522     C* NUMERO DISTINTA >< 0
029500920528     C     VIDNDD        IFNE      *BLANKS
029600920528     C     VIDNDD        ANDNE     *ZEROS
029700920423     C*
029800920423     C* RICERCA
029900941109     C     '?'           SCAN      VIDNDD                                 90
030000941109     C     *IN90         IFEQ      *ON
030100941109     C                   Z-ADD     0             DLRNFV
030200941109     C                   MOVEL     VIDFIL        DLRFGS
030300941109     C   01              Z-ADD     VIDDDC        DLRGDA
030400941109     C   01              Z-ADD     VIDDDC        DLRGAL
030500941109     C  N01              Z-ADD     0             DLRGDA
030600941109     C  N01              Z-ADD     311299        DLRGAL
030700941109     C                   MOVEL     DSLR01        KPJBU
030800941109     C                   CALL      'FNLR02R'
030900920423     C                   PARM                    KPJBA
031000941109     C                   MOVEL     KPJBU         DSLR01
031100941109     C                   MOVEL     DLRNFV        VIDNDD
031200920423     C*
031300920423     C                   GOTO      ENDCT1
031400920423     C                   END
031500941109     C**
031600920423     C*  C O N T R O L L I
031700920528     C                   TESTN                   VIDNDD               35
031800920522     C  N35              SETON                                        4390
031900920522     C  N35              GOTO      ENDCT1
032000920423     C*
032100920528     C                   MOVEL     VIDNDD        COMNFV
032200941109     C*
032300920421     C* CONTROLLO SE NUMERO DISTINTA ESISTE
032400941109     C     KFVV          CHAIN     FNFVV000                           30
032500941109    1C     *IN30         IFEQ      '0'
032600920421     C*
032700920522     C* SE NUMERO DISTINTA ANNULLATO : ERRORE
032800941109    2C     FVVATB        IFNE      *BLANKS
032900920421     C                   SETON                                        4190
033000920421     C                   GOTO      ENDCT1
033100920421     C*
033200941109   X2C                   ELSE
033300920522     C* CONTROLLO DATA SE IMMESSA
033400941109    3C   01FVVDFV        IFNE      COMDFV
033500941109     C                   SETON                                        4690
033600941109     C                   GOTO      ENDCT1
033700941109    3C                   END
033800941109     C* FOGLIO CHIUSO
033900941109     C     FVVFCF        IFNE      ' '
034000941109     C                   SETON                                        5290
034100941109     C                   GOTO      ENDCT1
034200941109    3C                   END
034300920528     C*
034400950411     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
034500950411     C     VIDFPP        IFNE      ' '
034600950411     C     VIDFPP        ANDNE     FVVFPP
034700950411     C                   SETON                                        5390
034800950411     C                   GOTO      ENDCT1
034900950411     C                   ENDIF
035000950411     C*
035100920528     C* 05 ON - ESISTONO NUMERO DISTINTA DAL E NUMERO AL
035200941109     C                   MOVEL     VIDNDD        NDD               5 0
035300941109    3C     VIDNDA        IFNE      *ZEROS
035400920528     C                   SETON                                        05
035500941109    4C     VIDNDA        IFLT      NDD
035600920528     C                   SETON                                        4990
035700920528     C                   GOTO      ENDCT1
035800941109    4C                   END
035900941109    3C                   END
036000941109     C*
036100920528     C* 05 OFF - NUMERO DISTINTA DAL SENZA AL
036200941109    3C  N05              DO
036300920421     C* CONTROLLO SE ESISTONO SPUNTE PER QUELLA DISTINTA
036400000126     C                   Z-ADD     COMNFV        WNFV
036500000126     C     KBRV          SETLL     FIBRV01L                               33
036600920522     C  N33              SETON                                        4290
036700941109     C  N33              GOTO      ENDCT1
036800941109     C*
036900920522     C* CONTROLLO SE ESISTE ALMENO UNA BOLLA
037000020911     c                   Z-Add     VidFil        Karbifp
037100020911     C**!!!              Z-ADD     COMNFV        COM               7 0
037200020911     c                   Z-Add     Comnfv        KarbNdc
037300020911     C     Karb78        SETLL     FNARB78L                               34
037400920522     C  N34              SETON                                        4790
037500920522     C   90              GOTO      ENDCT1
037600920831     C*
037700920831     C* DESCRIZIONE DISTINTA CONSEGNA
037800941109     C                   MOVEL     FVVDSF        VIDDES
037900941109    3C                   END
038000920528     C*
038100941109    2C                   END
038200920421     C*
038300941109   X1C                   ELSE
038400941109     C*
038500920421     C* 30 ON - NUMERO DISTINTA INESISTENTE
038600920421     C                   SETON                                        4390
038700920421     C                   GOTO      ENDCT1
038800920421     C                   END
038900920528     C*
039000920522     C                   END
039100920522     C*
039200911015     C     ENDCT1        ENDSR
039300941109     C*
039400941109     C* CONTROLLO LA FILIALE IN GESTIONE -----------------------------*
039500941109     C     CTRFGS        BEGSR
039600941109     C                   SETOFF                                       90
039700941109     C                   CLEAR                   DSLV50
039800941109     C                   MOVEL     KNMUS         D50PRU
039900941109     C                   MOVEL     VIDFIL        D50FGS
040000941109     C                   CALL      'FNLV50R'
040100941109     C                   PARM                    DSLV50
040200941109     C*
040300941109     C     D50ERR        IFNE      ' '
040400941109     C                   MOVEL     D50MSG        VIDMSG
040500941109     C                   SETON                                        509028
040600941109     C                   GOTO      ENDCTF
040700941109     C                   ENDIF
040800941109     C*
040900941109     C     ENDCTF        ENDSR
