000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941019     F* TRTR87R  *---------------------------------------------------*
000300000000     F*------------------*                                           *
000400941019     F* - RICEZIONE INCASSO CONTRASSEGNI RLS 11/94                   *
000500000000     F*--------------------------------------------------------------*
000600941019     FFNCAS00R  UF   E             DISK    USROPN
000700941019     FTNCSB03L  UF   E           K DISK
000800900604     FCMRIC01L  UF   E           K DISK
000900930927     FTABEL00F  IF   E           K DISK    USROPN
001000991021     FTNTBE01L  IF   E           K DISK    USROPN
001100900604     D*
001200951019     D BB              S              7  0 DIM(200)                             COD.CLI. ABILIT.BB
001300900604     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001400900604     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001500900604     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001600900604     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
001700941019     D WLBDA8          DS
001800941019     D  G08DAT                 1      8  0
001900941019     D  G08INV                 9     16  0
002000941019     D  G08ERR                17     17
002100941019     D  G08TGI                18     22  0
002200991021     D DGED          E DS
002300991021     D DSTM          E DS
002400950327     D DSCV          E DS
002500900604     D KPJBA         E DS
002600900604     D  LIBSYS                92    101
002700900604     D  LEGFIL               483    492
002800900604     D  MBRFIL               493    502
002900900604     C*---------------------------------------------------------------*
003000941019     C                   READ      FNCAS00R                               80
003100941019     C     *IN80         DOWEQ     '0'
003200911111     C*
003300991021     C                   ADD       1             NUMREC            7 0
003400941019     C     KCSB          CHAIN     TNCSB000                           21
003500991021     C* TROVATO CONTROLLO SE DATI CONGRUENTI
003600991021     C     *IN21         IFEQ      '0'
003700941019     C* CONTROLLO SE ANNULLATO
003800941019     C     CSBSTA        IFEQ      9
003900900604     C* CONTROLLO IMPORTO
004000991021     C     CSBCAS        ORNE      CASCAS
004100941019     C* SE GIA' INCASSATO
004200991021     C     CSBDDC        ORGT      *ZEROS
004300911112     C     CSBNDT        ORGT      *ZEROS
004400941019      *
004500941019     C                   ELSE
004600900604     C* SE TUTTO BENE AGGIORNO
004700900604     C                   EXSR      AGGIOR
004800941019     C                   DELETE    FNCAS000
004900941019     C                   ENDIF                                                  <- *IN24
005000941019     C                   ENDIF                                                  <- *IN21
005100941019      *
005200941019     C                   READ      FNCAS00R                               80
005300941019     C                   ENDDO
005400900604     C*
005500900604     C* CHIUSURA E RIORGANIZZAZIONE MEMBRO TRASCODIFICATO
005600900604     C*
005700900604     C                   EXSR      CAEXB
005800900604     C*
005900900604     C* AGGIORNA ARCHIVIO DI LOG
006000900604     C*
006100900604     C     LEGFIL        CHAIN     CMRIC01L                           20
006200900604     C  N20              DO
006300991021     C                   Z-ADD     NUMREC        RICNR1
006400900604     C                   MOVE      '1'           RICFL2                          AGG.TO
006500900604     C                   UPDATE    CMRIC000
006600900604     C                   MOVEL     RICPRR        LEGFIL
006700900604     C                   END
006800900604     C*
006900900605     C     STOP          TAG
007000900605     C                   SETON                                        LR
007100900605     C*
007200900604     C/SPACE 005
007300900604     C*----------------------------------------------
007400941019     C* AGGIORNO FILE TNCSB00F ----------------------
007500900604     C*----------------------------------------------
007600900604     C     AGGIOR        BEGSR
007700900604     C*
007800941019     C                   MOVE      CASFLE        CSBRGF
007901061031     c* se è un mittente e ho il pagamento anticipato non aggiorno
007902061031     c* i riferimenti dell'incasso
007903061031     c                   if        castpi <>'M' or
007904061031     c                             (castpi='M' and csbndp=0 and csbddp=0)
008000941019     C                   MOVEL     CASTPI        CSBTPI                         T.INT.=M
008100950127     C                   MOVEL     CASTPI        CSBFUS                         T.INT.=M
008200941019     C                   MOVEL     CASTPA        CSBTPA
008300941019     C                   MOVE      CASABI        CSBABI
008400941019     C                   MOVE      CASCAB        CSBCAB
008500941019     C                   MOVE      CASNRA        CSBNRA
008501061113     C                   MOVE      CASDTE        CSBDTE
008502061113     C                   MOVE      CASNRG        CSBNRI
008503061113     C                   MOVE      CASDRE        CSBDRI
008504061031     c                   end
008600941019     C                   MOVE      CASDDC        CSBDDC
008700941019     C                   MOVE      CASNDT        CSBNDT
008800941019     C                   MOVE      AMGUDT        CSBUDA
008900941019     C                   MOVE      CASVCA        CSBVCA
009000011019     C**                 MOVE      CASCMB        CSBCMB
009300911112     C*
009400911112     C* SE NON E DIRETTO E' CONFERMATO AUTOMATIC.
009500941019     C     CASTPI        IFNE      'M'
009600941019     C                   MOVE      AMGUDT        CSBDTA
009700930927     C                   ELSE
009800930927     C*
009900950327     C* CONTROLLO SE CONTRASSEGNO ESTERO
010000930927     C* SE E'DIRETTO CONTROLLO SE IL CLIENTE è ABILITATO ALL'INCASSO "BB"
010100930927     C* IN QUESTO CASO TOLGO IN TPI IL FLAG=M , QUINDI RISULTA INCASSATO
010200930927     C* CON ASSEGNO BARTOLINI CON DISTINTA DA CONFERMARE  (VECCHIA MANIERA)
010300991021     C     CSBVCA        IFEQ      *BLANKS
010400991021     C     CSBVCA        OREQ      §GEDCN
010500991021     C     CSBVCA        OREQ      DIVALT
010600930927     C                   Z-ADD     1             X
010700930927     C     CSBCDI        LOOKUP    BB(X)                                  30
010800930927     C   30              MOVEL     *BLANK        CSBTPI
010900950127     C   30              MOVEL     *BLANK        CSBFUS
011000950327     C                   END
011100930927     C*
011200930927     C                   END
011300900604     C*
011400910913     C     CSBSTA        IFEQ      2
011401021122     C     CSBSTA        orEQ      3
011500910913     C                   Z-ADD     0             CSBSTA
011600900604     C                   END
011700941019     C                   UPDATE    TNCSB000
011800900604     C                   ADD       1             CONNBL            7 0
011900900604     C     ENDAGG        ENDSR
012000900604     C/SPACE
012100900604     C**---------------------------------------------------------
012200941019     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI FNCAS00R      -
012300900604     C**---------------------------------------------------------
012400900604     C     CAEXA         BEGSR
012500900604     C                   MOVE      ')'           VAR11            11
012600900604     C                   MOVEL     MBRFIL        VAR11
012700900604     C                   MOVE      '))'          VAR12            12
012800900604     C                   MOVEL     MBRFIL        VAR12
012900900604     C                   Z-ADD     55            LUNG             15 5
013000900604     C*
013100900604     C                   SETOFF                                       20
013200900604     C                   MOVE      VAR11         CMD1
013300900604     C                   MOVEL     *BLANKS       COMMAN
013400900604     C                   MOVEA     CMD1(1)       COMMAN           80
013500900604     C                   CALL      'QCMDEXC'                            20
013600900604     C                   PARM                    COMMAN
013700900604     C                   PARM                    LUNG
013800900604     C   20              GOTO      STOP
013900900604     C** CONTROLLO SE ESISTE IL MEMBRO
014000900604     C** SE NON ESISTE CHIUDO IL PROGRAMMA
014100900604     C                   SETOFF                                       20
014200900604     C                   MOVE      VAR12         CMD2
014300900604     C                   MOVEL     *BLANKS       COMMAN
014400900604     C                   MOVEA     CMD2(1)       COMMAN
014500900604     C                   CALL      'QCMDEXC'                            20
014600900604     C                   PARM                    COMMAN
014700900604     C                   PARM                    LUNG
014800900604     C   20              GOTO      STOP
014900900604     C** ALLOCO MEMBRO
015000900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
015100900604     C                   SETOFF                                       20
015200900604     C                   MOVE      VAR11         CMD3
015300900604     C                   MOVEL     *BLANKS       COMMAN
015400900604     C                   MOVEA     CMD3(1)       COMMAN
015500900604     C                   CALL      'QCMDEXC'                            20
015600900604     C                   PARM                    COMMAN
015700900604     C                   PARM                    LUNG
015800900604     C   20              GOTO      STOP
015900900604     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
016000900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
016100941019     C                   OPEN      FNCAS00R
016200900604     C                   ENDSR
016300900604     C/SPACE 005
016400900604     C**---------------------------------------------------------
016500941019     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI FNCAS00R      -
016600900604     C**---------------------------------------------------------
016700900604     C     CAEXB         BEGSR
016800900604     C                   MOVE      ')'           VAR11            11
016900900604     C                   MOVEL     MBRFIL        VAR11
017000900604     C                   MOVE      '))'          VAR12            12
017100900604     C                   MOVEL     MBRFIL        VAR12
017200900604     C                   Z-ADD     55            LUNG             15 5
017300900604     C*
017400941019     C                   CLOSE     FNCAS00R                             20
017500940825     C** RIORGANIZZA IL MEMBRO ELIMINATE SPECIFICHE
017600940826     C  N20              DO
017700940826     C                   END
017800900604     C                   MOVE      VAR12         CMD5
017900900604     C                   MOVEL     *BLANKS       COMMAN
018000900604     C                   MOVEA     CMD5(1)       COMMAN
018100900604     C                   CALL      'QCMDEXC'
018200900604     C                   PARM                    COMMAN
018300900604     C                   PARM                    LUNG
018400900604     C** DISALLOCA IL MEMBRO AGG.TO
018500900604     C                   ENDSR
018600941019     C*---------------------------------------------------------------*
018700941019     C     *INZSR        BEGSR
018800941019     C*---------------------------------------------------------------*
018900941019     C     *ENTRY        PLIST
019000941019     C                   PARM                    KPJBA
019100941019     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
019200941019     C* ASSUMO IL PRIMO MEMBRO DEL FILE
019300941019     C     MBRFIL        IFEQ      *BLANK
019400941019     C                   MOVEL     '*FIRST'      MBRFIL
019500941019     C                   END
019600941019     C                   Z-ADD     66            RIG               2 0
019700941019     C*§§
019800941019     C                   TIME                    WHHUDT           14 0
019900941019     C                   MOVEL     WHHUDT        ORA               6 0
020000941019     C                   MOVE      WHHUDT        G08DAT
020100941019     C                   MOVE      *ZEROS        G08INV
020200941019     C                   MOVEL     *BLANK        G08ERR
020300941019     C                   CALL      'XSRDA8'
020400941019     C                   PARM                    WLBDA8
020500941019     C                   Z-ADD     G08INV        AMGUDT            8 0
020600941019      *
020700941019      * CARICO TUTTI I CLIENTI ABILITATI ALL' INCASSO 'BB'
020800941019      * CON AUTORIZZ. A VERSARE ASSEGNI INT. MITTENTE
020900941019      * PER TRASFORMARLI IN ASS.BARTOLINI
021000941019     C                   OPEN      TABEL00F
021100991021     C                   OPEN      TNTBE01L
021200991215     C                   Z-ADD     1             CODUT
021300941019     C                   MOVEL     'TM'          TBLCOD
021400941019     C                   MOVE      *BLANKS       TBLKEY
021500941019     C                   MOVE      *ZEROS        X                 3 0
021600941019     C     KTAB          SETLL     TABEL00F
021700941019     C     *IN21         DOUEQ     '1'
021800941019     C     KCOD          READE     TABEL00F                               21
021900941019     C  N21              MOVEL     TBLUNI        DSTM
022000941019     C  N21§TMFBB        IFEQ      'S'
022100941019     C     §TMFVE        ANDNE     'N'                                           AUT.VERS.MITT
022200941019     C                   ADD       1             X
022300941019     C                   MOVEL     TBLKEY        BB(X)
022400941019     C                   ENDIF
022500941019     C                   ENDDO
022600991021     C* CARICO DIVISA MONETA DI CONTO
022700991021     C                   MOVEL     'GED'         TBECOD
022800991021     C                   MOVEL     *BLANKS       TBEKE1
022900991021     C                   MOVEL     '1'           TBEKE1
023000991021     C     KTBE          CHAIN     TNTBE01L                           21
023100991021     C  N21              MOVEL     TBEUNI        DGED
023200991021     C* ... E MONETA ALTERNATIVA
023300950327     C                   MOVE      *BLANKS       TBLKEY
023400950327     C                   MOVE      *ZEROS        X                 3 0
023500950327     C     KTAB          SETLL     TABEL00F
023600950327     C     *IN21         DOUEQ     '1'
023700950327     C     KCOD          READE     TABEL00F                               21
023800950327     C     *IN21         IFEQ      '0'
023900950327     C     TBLFLG        ANDEQ     *BLANKS
024000950327     C                   MOVEL     TBLUNI        DSCV
024100991021     C                   MOVEL     TBLKEY        WDIV              3
024200991021     C     §CVDVA        IFEQ      'S'
024300991021     C     WDIV          ANDNE     §GEDCN
024400011220     C     WDIV          ANDNE     *BLANK
024500991021     C                   MOVEL     WDIV          DIVALT            3
024600991021     C                   END
024700950327     C                   ENDIF
024800950327     C                   ENDDO
024900941019     C                   CLOSE     TABEL00F
025000991021     C                   CLOSE     TNTBE01L
025100941019     C*---------------------------------------------------------------*
025200941019     C* APERTURA MEMBRO DA TRASCODIFICARE
025300941019     C*
025400941019     C                   EXSR      CAEXA
025500941019     C*---------------------------------------------------------------*
025600941019     C*---------------------------------------------------------------*
025700941019     C* DEFINIZIONE CHIAVI
025800941019     C*
025900941019     C     KCSB          KLIST
026000941019     C                   KFLD                    CASAAS
026100941025     C                   KFLD                    CASLNP
026200941019     C                   KFLD                    CASNRS
026300941019     C                   KFLD                    CASNSP
026400941019     C*
026500941019     C     KTAB          KLIST
026600991021     C                   KFLD                    CODUT             1 0
026700941019     C                   KFLD                    TBLCOD
026800941019     C                   KFLD                    TBLKEY
026900941019     C*
027000941019     C     KCOD          KLIST
027100941019     C                   KFLD                    CODUT
027200991021     C                   KFLD                    TBLCOD
027300991021     C     KTBE          KLIST
027400991021     C                   KFLD                    TBECOD
027500991021     C                   KFLD                    TBEKE1
027600941019      *
027700941019     C     *LIKE         DEFINE    CSBCAS        TOTASS
027800941019     C     *LIKE         DEFINE    CSBCAS        TOTBB
027900941019     C                   ENDSR
028000941019**         CMD1
028100941019CHKOBJ OBJ(FNCAS00R) OBJTYPE(*FILE)     MBR(M234567890)
028200900514**         CMD2
028300941019ALCOBJ OBJ((FNCAS00R *FILE *EXCL           M234567890))
028400900514**         CMD3
028500941019OVRDBF FILE(FNCAS00R)                   MBR(M234567890)
028600900514**         CMD5
028700941019DLCOBJ OBJ((FNCAS00R *FILE *EXCL           M234567890))
