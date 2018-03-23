000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960201     H* TRTR41R *-----------------------------------------------------*
000300960129     H*  RICEZIONE ESTENSIONE DESCRIZIONE FOGLIO VIAGGIO: FNFV400R    *
000400951113     H*---------------------------------------------------------------*
000500000626     FAZORG01L  IF   E           K DISK
000600011017     FTNFVS01L  IF   E           K DISK    USROPN
000700960129     FFNFV401L  UF A E           K DISK
000800960129     FFNFV400R  UF   E           K DISK    USROPN
000900960129     F                                     RENAME(FNFV4000:FNFV40RR)
001000960129     D* DEFINIZIONE SCHIERE
001100021003     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001200021003     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001300021003     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001400000626     D LPT             S              3  0 DIM(30)                              FIL GESTITE POSTE
001500950109     D*
001600000626     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001700000626     D  LIN                    1     90  0
001800000626     D                                     DIM(30)                              FIL GESTITE
001900960129     D                 DS
002000960129     D  GG                     1      2  0
002100960129     D  MM                     3      4  0
002200960129     D  AA                     5      8  0
002300960129     D  WDATA                  1      8  0
002400960129     D                 DS
002500960129     D  DSKEY                  1     27
002600960129     D  FV4LNP                 1      3  0
002700960129     D  FV4NFV                 4      8  0
002800960129     D  FV4EPA                 9      9
002900960129     D  FV4LAI                10     12  0
003000960129     D  FV4TRC                13     13
003100960129     D  FV4DTV                14     21  0
003200960129     D  FV4ORV                22     27  0
003300960129     D                 DS
003400960129     D  SAVKEY                 1     27
003500960129     D  SAVLNP                 1      3  0
003600960129     D  SAVNFV                 4      8  0
003700960129     D  SAVEPA                 9      9
003800960129     D  SAVLAI                10     12  0
003900960129     D  SAVTRC                13     13
004000960129     D  SAVDTV                14     21  0
004100960129     D  SAVORV                22     27  0
004200000000     D KPJBA         E DS
004300911001     D  LEGFIL               483    492
004400911001     D  MBRFIL               493    502
004500000626     D OG143         E DS
004600000626     D CNCR80        E DS
004700911001     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800960129     C*****************************************************************
004900960129     C*  RIEPILOGO INDICATORI
005000960129     C*****************************************************************
005100960129     C* 20    - NON RIUSCITA ALLOCAZIONE MEMBRO
005200960129     C* 30    - DI COMODO
005300960129     C*****************************************************************
005400000000     C     *ENTRY        PLIST
005500000000     C                   PARM                    KPJBA
005600950109     C*
005700000000     C                   Z-ADD     1             CODUT
005800911001     C                   CALL      'X§PARUT'
005900911001     C                   PARM                    UT§DSE
006000911001     C                   MOVEL     REC80         CNCR80
006100960129     C*
006200911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
006300911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
006400911001     C     MBRFIL        IFEQ      *BLANK
006500021003     C                   MOVEL     '*FIRST'      MBRFIL
006600911001     C                   END
006700911001     C* MEMBRO LUNGO 3
006800021003     C                   MOVEL     MBRFIL        COMODO            4
006900911001     C                   MOVE      COMODO        MBR               3 0
007000960129     C*
007100960129     C*---------------------------------------------------------------*
007200911001     C* ALLOCO MEMBRI
007300911001     C                   EXSR      CAEX
007400911001     C   20              GOTO      FINE
007500911001     C*
007600960129     C                   Z-ADD     0             NUMFV4            7 0
007700011017     C*
007800011017     C     SIMFEL        IFEQ      0
007900011017     C                   OPEN      TNFVS01L
008000011017     C                   ENDIF
008100960129     C* LETTURA FNFV400R
008200960129     C     *LOVAL        SETLL     FNFV400R
008300960129     C                   READ      FNFV400R                               30
008400960129     C*
008500960129    1C     *IN30         DOWEQ     *OFF
008600000626     C                   SETOFF                                       75        ON=TROVATO PT
008700000626      * .. PER SEDE CERCO FOGLIO VIAGGIO ANCHE CON LINEA POSTE DI LNP
008800000627      * .. SE NON TROVATO FOGLIO VIAGGIO CON LINEA DI PARTENZA
008900000626     C     SIMFEL        IFEQ      0
009000000626     C     KFVS1         CHAIN     TNFVS01L                           31
009100000626     C     *IN31         IFEQ      *ON
009200000626     C                   EXSR      CARLPT
009300000626     C     1             DO        30            PT                3 0
009400000626     C     LPT(PT)       IFNE      0
009500000626     C                   Z-ADD     LPT(PT)       LPTLNP
009600000626     C     LPTLNP        CHAIN     AZORG01L                           74
009700000626     C     *IN74         IFEQ      *OFF
009800000626     C                   MOVEL     ORGDE3        OG143
009900000626     C     §OGPT         IFEQ      'S'
010000000626      * - - - - - - - - - - - - - - - - - - - - - - - - -
010100000626     C     KFVS2         CHAIN     TNFVS01L                           31
010200000626     C  N31              SETON                                        75        TROVATO
010300000626     C  N31              LEAVE
010400000626      * - - - - - - - - - - - - - - - - - - - - - - - - -
010500000626     C                   ENDIF
010600000626     C                   ENDIF
010700000626     C                   ENDIF
010800000626     C                   ENDDO
010900000626     C                   ENDIF
011000000626     C                   ENDIF
011100960129     C* CANCELLO RECORDS DA FNFV400F A CAMBIO KEY
011200960129    2C     DSKEY         IFNE      COMKEY
011300960129     C                   CLEAR                   W001A
011400960129     C                   MOVEL     DSKEY         SAVKEY
011500960129     C*
011600000627     C  N75KFV4          SETLL     FNFV401L
011700000627     C  N75KFV4          READE     FNFV401L                               31
011800000627     C   75KFV42         SETLL     FNFV401L
011900000627     C   75KFV42         READE     FNFV401L                               31
012000960129    3C     *IN31         DOWEQ     *OFF
012100960129     C                   DELETE    FNFV4000
012200960129     C                   MOVEL     '1'           W001A             1
012300000627     C  N75KFV4          READE     FNFV401L                               31
012400000627     C   75KFV42         READE     FNFV401L                               31
012500960129    3C                   ENDDO
012600960129     C*
012700960129     C* W001A = "1" ---> SONO STATI DELETATI RECORD GIA' ESISTENTI:
012800960129     C*   RIESEGUO CHAIN SU FNFV400R PERCHE' HO PERSO IL POSIZIONAMENTO
012900960129    3C     W001A         IFEQ      '1'
013000960129     C     KSAV          CHAIN     FNFV400R                           30
013100960129    3C                   ENDIF
013200960129     C*
013300960129     C                   MOVEL     DSKEY         COMKEY
013400960129    2C                   ENDIF
013500960129     C*
013600960129     C* CARICO IL RECORD RICEVUTO SOLO SE NON E' ANNULLATO
013700960129    2C     FV4ATB        IFEQ      ' '
013800960129     C*
013900960129     C* AGGIORNO FLAG/DATA TRASMISSIONE
014000960129    3C     SIMFEL        IFGT      0
014100960129     C                   MOVEL     'R'           FV4FT1                         *FILIALE
014200960129     C                   MOVEL     DATEU         FV4DT1
014300960129   X3C                   ELSE
014400960129     C                   MOVEL     'R'           FV4FT2                         *SEDE
014500960129     C                   MOVEL     DATEU         FV4DT2
014600960129    3C                   ENDIF
014700960129     C*
014800000627     C   75              Z-ADD     LPTLNP        FV4LNP
014900960129     C                   WRITE     FNFV4000
015000960129    2C                   ENDIF
015100960129     C*
015200960129     C                   ADD       1             NUMFV4
015300960129     C                   DELETE    FNFV40RR
015400960129     C                   READ      FNFV400R                               30
015500960129    1C                   ENDDO
015600960129     C*
015700960129     C* CHIUSURA MEMBRO TRASCODIFICATO
015800960129     C                   CLOSE     FNFV400R
015900900911     C*
016000900409     C     FINE          TAG
016100060607     c
016200060607     c                   if        *in20
016300060607     C                   CALL      'BCH10'
016400060607     C                   PARM                    KPJBA
016500060607     c                   endif
016600000000     C                   SETON                                        LR
016700951114     C*
016800951114     C*--- ESEGUE QCAEXEC DI APERTURA SUI MEMBRI ---------------------*
016900911001     C     CAEX          BEGSR
017000950109     C*
017100021003     C                   MOVE      *BLANK        VAR11            11
017200021003     C                   MOVE      *BLANK        VAR12            12
017300911001     C                   MOVE      ')'           VAR11
017400021003     C                   MOVEL     MBRFIL        VAR11
017500911001     C                   MOVE      '))'          VAR12
017600021003     C                   MOVEL     MBRFIL        VAR12
017700021003     C                   Z-ADD     55            LUNG             15 5
017800911001     C*
017900911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
018000911001     C                   SETOFF                                       20
018100951114     C                   MOVE      VAR11         CMD1                            FNFVA01R
018200911001     C                   MOVEL     *BLANKS       COMMAN
018300911001     C                   MOVEA     CMD1(1)       COMMAN           80
018400911001     C                   CALL      'QCMDEXC'                            20
018500911001     C                   PARM                    COMMAN
018600911001     C                   PARM                    LUNG
018700911001     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
018800060607     c                   if        not *in20
018900060607     c* Per la sede non alloco
019000060607     c                   if        simfel>0
019100911001     C                   MOVE      VAR12         CMD2
019200911001     C                   MOVEL     *BLANKS       COMMAN
019300911001     C                   MOVEA     CMD2(1)       COMMAN
019400911001     C                   CALL      'QCMDEXC'                            20
019500911001     C                   PARM                    COMMAN
019600911001     C                   PARM                    LUNG
019700060607     c                   endif
019800060607     c
019900911001     C* OVRDBF
020000911001     C  N20              DO
020100911001     C                   SETOFF                                       20
020200960129     C                   MOVE      VAR11         CMD3
020300911001     C                   MOVEL     *BLANKS       COMMAN
020400960129     C                   MOVEA     CMD3(1)       COMMAN
020500911001     C                   CALL      'QCMDEXC'                            20
020600911001     C                   PARM                    COMMAN
020700911001     C                   PARM                    LUNG
020800911001     C*
020900960129     C  N20              OPEN      FNFV400R
021000911001     C                   END
021100911001     C                   END
021200950109     C*
021300911001     C                   ENDSR
021400000626     C**-------------------------------------------------------------**
021500000626     C**  ROUTINE DI CARICO LPT PER LINEE POSTE                      **
021600000626     C**-------------------------------------------------------------**
021700000626     C     CARLPT        BEGSR
021800000626     C                   CLEAR                   DSUL06
021900000626     C                   MOVE      '£1'          D06COD
022000000626     C                   MOVEL     FV4LNP        D06KEY
022100000626     C                   MOVEL     DSUL06        KPJBU
022200000626     C                   CALL      'TRUL06R'
022300000626     C                   PARM                    KPJBA
022400000626     C                   MOVEL     KPJBU         DSUL06
022500000626     C                   MOVEA     LIN           LPT
022600000626     C                   ENDSR
022700960129     C*
022800951114     C*--- SR INIZIALE -----------------------------------------------*
022900951114     C     *INZSR        BEGSR
023000951114     C*
023100951114     C****  KLIST  ****
023200960129     C* ACCESSO FNFV401L
023300960129     C     KFV4          KLIST
023400960129     C                   KFLD                    FV4LNP
023500960129     C                   KFLD                    FV4NFV
023600960129     C                   KFLD                    FV4EPA
023700960129     C                   KFLD                    FV4LAI
023800960129     C                   KFLD                    FV4TRC
023900000627     C     KFV42         KLIST
024000000627     C                   KFLD                    LPTLNP
024100000627     C                   KFLD                    FV4NFV
024200000627     C                   KFLD                    FV4EPA
024300000627     C                   KFLD                    FV4LAI
024400000627     C                   KFLD                    FV4TRC
024500960129     C* ACCESSO FNFV400R
024600960129     C     KSAV          KLIST
024700960129     C                   KFLD                    SAVEPA
024800960129     C                   KFLD                    SAVLNP
024900960129     C                   KFLD                    SAVNFV
025000960129     C                   KFLD                    SAVLAI
025100960129     C                   KFLD                    SAVTRC
025200960129     C                   KFLD                    SAVDTV
025300960129     C                   KFLD                    SAVORV
025400000626     C* ACCESSO TNFVS01L
025500000626     C     KFVS1         KLIST
025600000626     C                   KFLD                    FV4LNP
025700000626     C                   KFLD                    FV4NFV
025800000626     C     KFVS2         KLIST
025900000626     C                   KFLD                    LPTLNP            3 0
026000000626     C                   KFLD                    FV4NFV
026100960129     C*
026200960129     C****  DEFINIZIONE CAMPI  ****
026300960129     C     *LIKE         DEFINE    DSKEY         COMKEY
026400951114     C*
026500960129     C* GIRO LA DATA DEL GIORNO
026600951114     C                   TIME                    WTIME            14 0          ORA & DATA
026700960129     C                   MOVE      WTIME         UDATE8            8 0          GG/MM/AAAA
026800960129     C                   Z-ADD     UDATE8        WDATA
026900960129     C                   Z-ADD     GG            MMGG              4 0
027000960129     C                   MOVEL     MM            MMGG
027100960129     C                   Z-ADD     MMGG          DATEU             8 0
027200960129     C                   MOVEL     AA            DATEU
027300951114     C                   ENDSR
027400951114     C*
027500951114     C*---------------------------------------------------------------*
027600960129**         CMD1
027700021003CHKOBJ OBJ(FNFV400R) OBJTYPE(*FILE)     MBR(M234567890)
027800911001**         CMD2
027900021003ALCOBJ OBJ((FNFV400R *FILE *EXCL           M234567890))
028000960129**         CMD3
028100021003OVRDBF FILE(FNFV400R)                   MBR(M234567890)
