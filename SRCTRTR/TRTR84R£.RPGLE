000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200991122     F* TRTR84R  *---------------------------------------------------*
000300000000     F*------------------*                                           *
000400900604     F* - RICEZIONE INCASSO CONTRASSEGNI                             *
000500000000     F*--------------------------------------------------------------*
000600991122     FFNBCO00R  UF   E             DISK    USROPN
000700941026     FTNCSB03L  UF   E           K DISK
000800900604     FCMRIC01L  UF   E           K DISK
000900940518     FRICCASS   O    F  132        PRINTER
001000900604     D*
001100900604     D SK              S             66    DIM(10) CTDATA PERRCD(1)
001200900604     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001300900604     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001400900604     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001500900604     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
001600900604     D                 DS
001700900604     D*-----INVERSIONE DATE
001800900604     D  ANNODS                 1      2  0
001900900604     D  MESEDS                 3      4  0
002000900604     D  GIORDS                 5      6  0
002100900604     D  DATAIN                 1      6  0
002200900604     D DISSAV          DS
002300900604     D  SAVNDS                 1      4P 0
002400900604     D  SAVDDS                 5      8P 0
002500900604     D DIS             DS
002600900604     D  BCONDS                 1      4P 0
002700900604     D  BCODDS                 5      8P 0
002800900604     D KPJBA         E DS
002900900604     D  LIBSYS                92    101
003000900604     D  LEGFIL               483    492
003100900604     D  MBRFIL               493    502
003200900604     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003300900604     D  TCU                  398    697
003400900604     D                                     DIM(50)                              TAB FLAG CNT
003500900604     D  KCU                  698    847P 0
003600900604     D                                     DIM(50)                              TAB CAPO CONTO
003700900604     D                                     PACKEVEN
003800900604     D  DTU                  848    895P 0
003900900604     D                                     DIM(12)                              DATE PARAM
004000900604     D                                     PACKEVEN
004100941026     D WLBDA8          DS
004200941026     D  G08DAT                 1      8  0
004300941026     D  G08INV                 9     16  0
004400941026     D  G08ERR                17     17
004500941026     D  G08TGI                18     22  0
004600900604     I/SPACE 3
004700900604     C*---------------------------------------------------------------*
004800900604     C     *ENTRY        PLIST
004900900604     C                   PARM                    KPJBA
005000900604     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
005100900604     C* ASSUMO IL PRIMO MEMBRO DEL FILE
005200900604     C     MBRFIL        IFEQ      *BLANK
005300900604     C                   MOVEL     '*FIRST'      MBRFIL
005400900604     C                   END
005500900604     C                   Z-ADD     1             CODUT
005600900604     C                   CALL      'XPARUT'
005700900604     C                   PARM                    UTEDSE
005800900604     C                   MOVEL     RAGUT         RSUT             20
005900900604     C                   Z-ADD     66            RIG               2 0
006000941026     C                   TIME                    WHHUDT           14 0
006100941026     C                   MOVEL     WHHUDT        ORA               6 0
006200941026     C                   MOVE      WHHUDT        G08DAT
006300941026     C                   MOVE      *ZEROS        G08INV
006400941026     C                   MOVEL     *BLANK        G08ERR
006500941026     C                   CALL      'XSRDA8'
006600941026     C                   PARM                    WLBDA8
006700941026     C                   Z-ADD     G08INV        AMGUDT            8 0
006800900604     C*---------------------------------------------------------------*
006900900604     C* APERTURA MEMBRO DA TRASCODIFICARE
007000900604     C*
007100900604     C                   EXSR      CAEXA
007200900604     C*---------------------------------------------------------------*
007300900604     C*---------------------------------------------------------------*
007400900604     C* DEFINIZIONE CHIAVI
007500900604     C*
007600900604     C     CHICON        KLIST
007700941026     C                   KFLD                    ANNO
007800941026     C                   KFLD                    BCOLNP
007900900604     C                   KFLD                    BCONRS
008000900604     C                   KFLD                    BCONSP
008100941026     C                   Z-ADD     1900          ANNO              4 0
008200900604     C*---------------------------------------------------------------*
008300900604     C                   DO        *HIVAL
008400991122     C                   READ      FNBCO00R                               80
008500900604     C  N80              DO
008600000113     C     BCOAAS        IFGE      40
008700000113     C                   Z-ADD     1900          ANNO
008800000113     C                   ELSE
008900000113     C                   Z-ADD     2000          ANNO
009000000113     C                   END
009100941026     C                   MOVE      BCOAAS        ANNO
009200900604     C                   ADD       1             NUREC             7 0
009300900604     C                   SETOFF                                       868721
009400900604     C                   SETOFF                                       222324
009500900604     C                   SETOFF                                       252630
009600900605     C* CONTROLLO SE SU BARTOLINI O AVANDERO
009700900605     C     BCOCOD        IFEQ      '1'                                          BARTOLINI
009800941026     C     CHICON        CHAIN     TNCSB000                           21
009900900604     C                   SETON                                        87
010000900604     C                   ELSE
010100941026     C     CHICON        CHAIN     TNCSB000                           21
010200900604     C                   SETON                                        86
010300900604     C                   END
010400900604     C*
010500900604     C* SE NON TROVATO E STAMPO
010600900604     C   21              EXSR      NONTRO
010700900604     C*
010800900604     C* SE TROVATO CONTROLLO SE TUTTI DATI UGUALI
010900900604     C  N21              DO
011000900604     C* CONTROLLO IMPORTO
011100941026     C     CSBCAS        IFNE      BCOIMA
011200991122     C     CSBVCA        ANDNE     BCOVCA
011300900604     C                   SETON                                        2325
011400900604     C                   END
011500900604     C* TROVATO MA CON DATI DISCORDANTI
011600900604     C   25              EXSR      NONTRO
011700900604     C   25              GOTO      SOTTO
011800900604     C*
011900900604     C* CONTROLLO RIFERIMENTI
012000941026     C                   MOVE      CSBRMN        RMN9              9 0
012100941026     C     RMN9          IFNE      BCORIF
012200900604     C                   SETON                                        2425
012300900604     C                   END
012400900604     C*
012500900604     C* SE TUTTO BENE AGGIORNO
012600900604     C                   EXSR      AGGIOR
012700900604     C     SOTTO         TAG
012800900604     C                   END
012900991122     C                   DELETE    FNBCO
013000900605     C                   END
013100900604     C  N80              END
013200900604     C*
013300900604     C* CHIUSURA E RIORGANIZZAZIONE MEMBRO TRASCODIFICATO
013400900604     C*
013500900604     C                   EXSR      CAEXB
013600900604     C*
013700900604     C* AGGIORNA ARCHIVIO DI LOG
013800900604     C*
013900900604     C     LEGFIL        CHAIN     CMRIC01L                           20
014000900604     C  N20              DO
014100900604     C                   Z-ADD     NUREC         RICNR1
014200900604     C                   MOVE      '1'           RICFL2                          AGG.TO
014300900604     C                   UPDATE    CMRIC000
014400900604     C                   MOVEL     RICPRR        LEGFIL
014500900604     C                   END
014600900604     C*
014700900713     C* STAMPA TOTALI SOLO SE CI SONO ANCHE ERRORI
014800900928     C* DAL 28/9/90 NON STAMPO PIU' I TOTALI
014900900713     C     CONERR        IFGT      *ZERO
015000900928     C   LR
015100900928     CANNLR              EXCEPT    LASTX
015200900713     C                   END
015300900605     C     STOP          TAG
015400900605     C                   SETON                                        LR
015500900605     C*
015600900604     C/SPACE 005
015700900604     C*--------------------------------------------
015800900604     C* NON TROVATO C/ASSEGNO IN BLCON  -----------
015900900604     C*--------------------------------------------
016000900604     C     NONTRO        BEGSR
016100900604     C* GIRO DATA DISTINTA
016200900604     C                   MOVE      BCODDS        DDS               6 0
016300900604     C                   MOVE      DDS           DATAIN
016400900604     C                   EXSR      ROVDAT
016500900604     C                   MOVE      DATAIN        DDS
016600900604     C* GIRO DATA INCASSO ASSEGNO
016700900604     C                   MOVE      BCODTA        DTA               6 0
016800900604     C                   MOVE      DTA           DATAIN
016900900604     C                   EXSR      ROVDAT
017000900604     C                   MOVE      DATAIN        DTA
017100900604     C*
017200900604     C                   MOVE      BCONDS        NDS               7 0
017300900604     C                   MOVE      BCONRA        NRA               7 0
017400900604     C*
017500900604     C* NUMERO DISTINTA NUOVO
017600900604     C     DIS           IFNE      DISSAV
017700900604     C     RIG           IFGT      58
017800900604     C                   Z-ADD     66            RIG
017900900604     C                   MOVEL     DIS           DISSAV
018000900604     C                   ELSE
018100900604     C                   EXCEPT    TES1
018200900604     C                   ADD       6             RIG
018300900604     C                   MOVEL     DIS           DISSAV
018400900604     C                   END
018500900604     C                   END
018600900604     C* SALTO PAGINA
018700900604     C     RIG           IFGT      64
018800900604     C                   EXCEPT    TES
018900900604     C                   EXCEPT    TES1
019000900604     C                   Z-ADD     9             RIG
019100900604     C                   END
019200900604     C                   EXCEPT    ERR
019300900605     C                   ADD       1             CONERR            7 0
019400900604     C                   ADD       2             RIG
019500900604     C                   ENDSR
019600900604     C/SPACE 005
019700900604     C*----------------------------------------------
019800900604     C* AGGIORNO FILE BLCON00F ----------------------
019900900604     C*----------------------------------------------
020000900604     C     AGGIOR        BEGSR
020100900604     C* SE GIA' INCASSATO SEGNALO ERRORE E NON AGGIORNO
020200910913     C     CSBDDC        IFGT      *ZEROS
020300910913     C     CSBNDT        ORGT      *ZEROS
020400900604     C                   SETON                                        26
020500900604     C                   END
020600900604     C   24
020700900604     COR 26              EXSR      NONTRO
020800900604     C   26              GOTO      ENDAGG
020900900604     C*
021000941026     C* FORZO SECOLO
021100000113     C     BCODTA        IFGT      0
021200000113     C     BCODTA        IFGT      400000
021300941026     C                   MOVEL     19            CSBDTE
021400000113     C                   ELSE
021500000113     C                   MOVEL     20            CSBDTE
021600941129     C                   ENDIF
021700000113     C                   ENDIF
021800910913     C                   MOVE      BCODTA        CSBDTE
021900910913     C                   MOVEL     BCOTIP        CSBTPI                         T.INT.=M
022000950127     C                   MOVEL     BCOTIP        CSBFUS                         T.INT.=M
022100910913     C* IMPONGO TIPO ASSEGNO BANCARIO
022200910913     C                   MOVEL     'B'           CSBTPA
022300910913     C                   MOVEL     BCOABI        CSBABI
022400910913     C                   MOVE      BCOCAB        CSBCAB
022500950713     C                   MOVE      BCONRA        WNRA              4
022600121210     C                   CLEAR                   CSBNRA
022700121210     C                   MOVEL     WNRA          CSBNRA
022800030926     C* METTO I RIFERIMENTI DELL'INCASSO SOLO SE NON HO I RIFERIMENTI
022900030926     C* DEL PAGAMENTO XCHÈ VORREBBE DIRE CHE L'HO ANTICIPATO
023000030926     C     CSBDDP        IFEQ      0
023100030926     C     CSBNDP        ANDEQ     0
023200941026     C* FORZO SECOLO
023300000120     C     BCODDS        IFGT      400000
023400941026     C                   MOVEL     19            CSBDDC
023500000113     C                   ELSE
023600000113     C                   MOVEL     20            CSBDDC
023700000113     C                   END
023800030926     C                   MOVE      BCODDS        CSBDDC
023900030926     C                   MOVE      BCONDS        CSBNDT
024000030926     C                   END
024100030926     C*
024200941026     C                   Z-ADD     AMGUDT        CSBUDA
024300900604     C*
024400910913     C     CSBSTA        IFEQ      2
024500941027     C     CSBSTA        OREQ      9
024600021202     C     CSBSTA        OREQ      3
024700021202     C     CSBSTA        OREQ      8
024800910913     C                   Z-ADD     0             CSBSTA
024900900604     C                   END
025000941026     C   87              UPDATE    TNCSB000
025100941026     C   86              UPDATE    TNCSB000
025200900604     C                   ADD       1             CONNBL            7 0
025300900604     C     ENDAGG        ENDSR
025400900604     C/SPACE 005
025500900604     C*----------------------------------------------
025600900604     C* ROVESCIO LE DATE    -------------------------
025700900604     C*----------------------------------------------
025800900604     C     ROVDAT        BEGSR
025900900604     C     GIORDS        SUB       ANNODS        COMOD1            2 0
026000900604     C     COMOD1        MULT      9999          COMOD2            6 0
026100900604     C     COMOD2        ADD       DATAIN        DATAIN
026200900604     C                   ENDSR
026300900604     C/SPACE
026400900604     C**---------------------------------------------------------
026500991122     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI FNBCO00R      -
026600900604     C**---------------------------------------------------------
026700900604     C     CAEXA         BEGSR
026800900604     C                   MOVE      ')'           VAR11            11
026900900604     C                   MOVEL     MBRFIL        VAR11
027000900604     C                   MOVE      '))'          VAR12            12
027100900604     C                   MOVEL     MBRFIL        VAR12
027200900604     C                   Z-ADD     55            LUNG             15 5
027300900604     C*
027400900604     C                   SETOFF                                       20
027500900604     C                   MOVE      VAR11         CMD1
027600900604     C                   MOVEL     *BLANKS       COMMAN
027700900604     C                   MOVEA     CMD1(1)       COMMAN           80
027800900604     C                   CALL      'QCMDEXC'                            20
027900900604     C                   PARM                    COMMAN
028000900604     C                   PARM                    LUNG
028100900604     C   20              GOTO      STOP
028200900604     C** CONTROLLO SE ESISTE IL MEMBRO
028300900604     C** SE NON ESISTE CHIUDO IL PROGRAMMA
028400900604     C                   SETOFF                                       20
028500900604     C                   MOVE      VAR12         CMD2
028600900604     C                   MOVEL     *BLANKS       COMMAN
028700900604     C                   MOVEA     CMD2(1)       COMMAN
028800900604     C                   CALL      'QCMDEXC'                            20
028900900604     C                   PARM                    COMMAN
029000900604     C                   PARM                    LUNG
029100900604     C   20              GOTO      STOP
029200900604     C** ALLOCO MEMBRO
029300900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
029400900604     C                   SETOFF                                       20
029500900604     C                   MOVE      VAR11         CMD3
029600900604     C                   MOVEL     *BLANKS       COMMAN
029700900604     C                   MOVEA     CMD3(1)       COMMAN
029800900604     C                   CALL      'QCMDEXC'                            20
029900900604     C                   PARM                    COMMAN
030000900604     C                   PARM                    LUNG
030100900604     C   20              GOTO      STOP
030200900604     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
030300900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
030400991122     C                   OPEN      FNBCO00R
030500900604     C                   ENDSR
030600900604     C/SPACE 005
030700900604     C**---------------------------------------------------------
030800991122     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI FNBCO00R      -
030900900604     C**---------------------------------------------------------
031000900604     C     CAEXB         BEGSR
031100900604     C                   MOVE      ')'           VAR11            11
031200900604     C                   MOVEL     MBRFIL        VAR11
031300900604     C                   MOVE      '))'          VAR12            12
031400900604     C                   MOVEL     MBRFIL        VAR12
031500900604     C                   Z-ADD     55            LUNG             15 5
031600900604     C*
031700991122     C                   CLOSE     FNBCO00R                             20
031800940825     C** RIORGANIZZA IL MEMBRO ELIMINATE SPECIF.
031900940826     C  N20              DO
032000900604     C                   MOVE      VAR12         CMD5
032100900604     C                   MOVEL     *BLANKS       COMMAN
032200900604     C                   MOVEA     CMD5(1)       COMMAN
032300900604     C                   CALL      'QCMDEXC'
032400900604     C                   PARM                    COMMAN
032500900604     C                   PARM                    LUNG
032600940826     C                   END
032700900604     C** DISALLOCA IL MEMBRO AGG.TO
032800900604     C                   ENDSR
032900000128     ORICCASS   E    LRNLR   TES            1 02
033000900604     O                       SK(1)               66
033100900604     O                       SK(2)              132
033200900604     O                       RSUT                20
033300900604     O                       UDATE              117 '  /  /  '
033400900604     O                       PAGE               128
033500000128     O          E    LRNLR   TES1        1
033600900604     O                       SK(3)               66
033700900604     O                       NDS           Z     23
033800900604     O                       DDS                 39 '  /  /  '
033900901123     O                       MBRFIL              99
034000000128     O          E    LRNLR   TES1        0  2
034100900604     O                       NDS           Z     23
034200900604     O                       DDS                 39 '  /  /  '
034300000128     O          E    LRNLR   TES1           1
034400900604     O                       SK(4)               66
034500900604     O                       SK(5)              132
034600000128     O          E    LRNLR   TES1           1
034700900604     O                       SK(6)               66
034800900604     O                       SK(7)              132
034900000128     O          E    LRNLR   TES1           1
035000900604     O                       SK(4)               66
035100900604     O                       SK(5)              132
035200000128     O          E    LRNLR   ERR            1
035300900605     O                       BCOLNP               4
035400900605     O                       BCOLNA               9
035500900605     O                       BCONRS        Z     14
035600900605     O                       BCONSP        Z     27
035700900605     O                       BCONRA        Z     39
035800991122     O                       BCOIMA        4     54
035900991122     O                       BCOVCA              58
036000991122     O                       BCOTIP              61
036100991122     O                       BCORIF        Z     72
036200991122     O                 N23N24                    96 'C/A INESISTENTE       '
036300991122     O                     26                    96 'C/A GIA INCASSATO     '
036400991122     O                     23                    98 'IMPORTO ASSEGNO DISCORDA'
036500991122     O                     23                   103 'NTE  '
036600991122     O                 N26 24                    98 'RIFERIMENTO MITT. DISCOR'
036700991122     O                 N26 24                   103 'DANTE'
036800000128     O          E    LRNLR 25ERR            1
036900941026     O                     23CSBCAS        2     54
037000941026     O                     24CSBRMN        Z   +  1
037100000128     O          E    LRNLR   LASTX            02
037200900604     O                       RSUT                20
037300900604     O                                           60 '**    CARICAMENTO   CONT'
037400900604     O                                           74 'RASSEGNI  DEL '
037500900604     O                       UDATE               83 '  /  /  '
037600900604     O                                           89 '***'
037700901123     O                       MBRFIL              99
037800000128     O                                          110 'TRTR84R'
037900900604     O                                          128 'PAG.'
038000900604     O                       PAGE          Z    132
038100000128     O          E    LRNLR   LASTX       3
038200900604     O                                         +  1 'N.ERRORI'
038300900604     O                       CONERR        2     50
038400000128     O          E    LRNLR   LASTX       2
038500900604     O                                         +  1 'N.BOLLE CARICATE'
038600900604     O                       CONNBL        2     50
038700000128     O          E    LRNLR   LASTX       2
038800900604     O                                         +  1 'N.RECORDS ELABORATI'
038900900605     O                       NUREC         2     50
039000991122**
039100900604                       ** ERRORI CARICAMENTO DISTINTE CONTRASSEGNI **
039200000128 **            TRTR84R
039300900604  DISTINTA N.RO XXXXXXX    DEL xx/xx/xx
039400900605+--------+-------------------+-----------+------------+---+-------
039500900604---+
039600991122+LNP  LNA+ Numero Spedizione +Num.Assegno+  Importo  +Vca+Ass+ Ri
039700991122f.Mitt +
039800900605
039900900514**         CMD1
040000991122CHKOBJ OBJ(FNBCO00R) OBJTYPE(*FILE)     MBR(M234567890)
040100900514**         CMD2
040200991122ALCOBJ OBJ((FNBCO00R *FILE *EXCL           M234567890))
040300900514**         CMD3
040400991122OVRDBF FILE(FNBCO00R)                   MBR(M234567890)
040500900514**         CMD5
040600991122DLCOBJ OBJ((FNBCO00R *FILE *EXCL           M234567890))
