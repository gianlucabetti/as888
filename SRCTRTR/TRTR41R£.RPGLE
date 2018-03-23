000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960201     H* TRTR41R *-----------------------------------------------------*
000300960129     H*  RICEZIONE ESTENSIONE DESCRIZIONE FOGLIO VIAGGIO: FNFV400R    *
000400951113     H*---------------------------------------------------------------*
000500000626     FAZORG01L  IF   E           K DISK
000600011017     FTNFVS01L  IF   E           K DISK    USROPN
000700960129     FFNFV401L  UF A E           K DISK
000800960129     FFNFV400R  UF   E           K DISK    USROPN
000900960129     F                                     RENAME(FNFV4000:FNFV40RR)
001000911001     FCMRIC01L  UF   E           K DISK
001100950109     D*
001200960129     D* DEFINIZIONE SCHIERE
001300021003     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001400021003     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001500021003     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001600000626     D LPT             S              3  0 DIM(30)                              FIL GESTITE POSTE
001700950109     D*
001800000626     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001900000626     D  LIN                    1     90  0
002000000626     D                                     DIM(30)                              FIL GESTITE
002100960129     D                 DS
002200960129     D  GG                     1      2  0
002300960129     D  MM                     3      4  0
002400960129     D  AA                     5      8  0
002500960129     D  WDATA                  1      8  0
002600960129     D                 DS
002700960129     D  DSKEY                  1     27
002800960129     D  FV4LNP                 1      3  0
002900960129     D  FV4NFV                 4      8  0
003000960129     D  FV4EPA                 9      9
003100960129     D  FV4LAI                10     12  0
003200960129     D  FV4TRC                13     13
003300960129     D  FV4DTV                14     21  0
003400960129     D  FV4ORV                22     27  0
003500960129     D                 DS
003600960129     D  SAVKEY                 1     27
003700960129     D  SAVLNP                 1      3  0
003800960129     D  SAVNFV                 4      8  0
003900960129     D  SAVEPA                 9      9
004000960129     D  SAVLAI                10     12  0
004100960129     D  SAVTRC                13     13
004200960129     D  SAVDTV                14     21  0
004300960129     D  SAVORV                22     27  0
004400000000     D KPJBA         E DS
004500911001     D  LEGFIL               483    492
004600911001     D  MBRFIL               493    502
004700000626     D OG143         E DS
004800000626     D CNCR80        E DS
004900911001     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005000960129     C*****************************************************************
005100960129     C*  RIEPILOGO INDICATORI
005200960129     C*****************************************************************
005300960129     C* 20    - NON RIUSCITA ALLOCAZIONE MEMBRO
005400960129     C* 30    - DI COMODO
005500960129     C*****************************************************************
005600000000     C     *ENTRY        PLIST
005700000000     C                   PARM                    KPJBA
005800950109     C*
005900000000     C                   Z-ADD     1             CODUT
006000911001     C                   CALL      'X§PARUT'
006100911001     C                   PARM                    UT§DSE
006200911001     C                   MOVEL     REC80         CNCR80
006300960129     C*
006400911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
006500911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
006600911001     C     MBRFIL        IFEQ      *BLANK
006700021003     C                   MOVEL     '*FIRST'      MBRFIL
006800911001     C                   END
006900911001     C* MEMBRO LUNGO 3
007000021003     C                   MOVEL     MBRFIL        COMODO            4
007100911001     C                   MOVE      COMODO        MBR               3 0
007200911001     C* SE SONO ME STESSA NON SCRIVO CMRIC
007300911001     C     MBR           IFEQ      SIMFEL
007400911001     C                   SETON                                        03
007500911001     C                   END
007600960129     C*
007700960129     C*---------------------------------------------------------------*
007800911001     C* ALLOCO MEMBRI
007900911001     C                   EXSR      CAEX
008000911001     C   20              GOTO      FINE
008100911001     C*
008200960129     C                   Z-ADD     0             NUMFV4            7 0
008300011017     C*
008400011017     C     SIMFEL        IFEQ      0
008500011017     C                   OPEN      TNFVS01L
008600011017     C                   ENDIF
008700960129     C* LETTURA FNFV400R
008800960129     C     *LOVAL        SETLL     FNFV400R
008900960129     C                   READ      FNFV400R                               30
009000960129     C*
009100960129    1C     *IN30         DOWEQ     *OFF
009200000626     C                   SETOFF                                       75        ON=TROVATO PT
009300000626      * .. PER SEDE CERCO FOGLIO VIAGGIO ANCHE CON LINEA POSTE DI LNP
009400000627      * .. SE NON TROVATO FOGLIO VIAGGIO CON LINEA DI PARTENZA
009500000626     C     SIMFEL        IFEQ      0
009600000626     C     KFVS1         CHAIN     TNFVS01L                           31
009700000626     C     *IN31         IFEQ      *ON
009800000626     C                   EXSR      CARLPT
009900000626     C     1             DO        30            PT                3 0
010000000626     C     LPT(PT)       IFNE      0
010100000626     C                   Z-ADD     LPT(PT)       LPTLNP
010200000626     C     LPTLNP        CHAIN     AZORG01L                           74
010300000626     C     *IN74         IFEQ      *OFF
010400000626     C                   MOVEL     ORGDE3        OG143
010500000626     C     §OGPT         IFEQ      'S'
010600000626      * - - - - - - - - - - - - - - - - - - - - - - - - -
010700000626     C     KFVS2         CHAIN     TNFVS01L                           31
010800000626     C  N31              SETON                                        75        TROVATO
010900000626     C  N31              LEAVE
011000000626      * - - - - - - - - - - - - - - - - - - - - - - - - -
011100000626     C                   ENDIF
011200000626     C                   ENDIF
011300000626     C                   ENDIF
011400000626     C                   ENDDO
011500000626     C                   ENDIF
011600000626     C                   ENDIF
011700960129     C* CANCELLO RECORDS DA FNFV400F A CAMBIO KEY
011800960129    2C     DSKEY         IFNE      COMKEY
011900960129     C                   CLEAR                   W001A
012000960129     C                   MOVEL     DSKEY         SAVKEY
012100960129     C*
012200000627     C  N75KFV4          SETLL     FNFV401L
012300000627     C  N75KFV4          READE     FNFV401L                               31
012400000627     C   75KFV42         SETLL     FNFV401L
012500000627     C   75KFV42         READE     FNFV401L                               31
012600960129    3C     *IN31         DOWEQ     *OFF
012700960129     C                   DELETE    FNFV4000
012800960129     C                   MOVEL     '1'           W001A             1
012900000627     C  N75KFV4          READE     FNFV401L                               31
013000000627     C   75KFV42         READE     FNFV401L                               31
013100960129    3C                   ENDDO
013200960129     C*
013300960129     C* W001A = "1" ---> SONO STATI DELETATI RECORD GIA' ESISTENTI:
013400960129     C*   RIESEGUO CHAIN SU FNFV400R PERCHE' HO PERSO IL POSIZIONAMENTO
013500960129    3C     W001A         IFEQ      '1'
013600960129     C     KSAV          CHAIN     FNFV400R                           30
013700960129    3C                   ENDIF
013800960129     C*
013900960129     C                   MOVEL     DSKEY         COMKEY
014000960129    2C                   ENDIF
014100960129     C*
014200960129     C* CARICO IL RECORD RICEVUTO SOLO SE NON E' ANNULLATO
014300960129    2C     FV4ATB        IFEQ      ' '
014400960129     C*
014500960129     C* AGGIORNO FLAG/DATA TRASMISSIONE
014600960129    3C     SIMFEL        IFGT      0
014700960129     C                   MOVEL     'R'           FV4FT1                         *FILIALE
014800960129     C                   MOVEL     DATEU         FV4DT1
014900960129   X3C                   ELSE
015000960129     C                   MOVEL     'R'           FV4FT2                         *SEDE
015100960129     C                   MOVEL     DATEU         FV4DT2
015200960129    3C                   ENDIF
015300960129     C*
015400000627     C   75              Z-ADD     LPTLNP        FV4LNP
015500960129     C                   WRITE     FNFV4000
015600960129    2C                   ENDIF
015700960129     C*
015800960129     C                   ADD       1             NUMFV4
015900960129     C                   DELETE    FNFV40RR
016000960129     C                   READ      FNFV400R                               30
016100960129    1C                   ENDDO
016200960129     C*
016300960129     C* CHIUSURA MEMBRO TRASCODIFICATO
016400960129     C                   CLOSE     FNFV400R
016500911001     C*
016600911001     C* AGGIORNA ARCHIVIO DI LOG SE NON SONO ME STESSO
016700951114    1C  N03              DO
016800911001     C     LEGFIL        CHAIN     CMRIC01L                           20
016900960129     C  N20              Z-ADD     NUMFV4        RICNR1
017000911001     C  N20              MOVE      '1'           RICFL2                          AGG.TO
017100911001     C  N20              UPDATE    CMRIC000
017200951114    1C                   END
017300900911     C*
017400900409     C     FINE          TAG
017500060607     c
017600060607     c                   if        *in20
017700060607     C                   CALL      'BCH10'
017800060607     C                   PARM                    KPJBA
017900060607     c                   endif
018000000000     C                   SETON                                        LR
018100951114     C*
018200951114     C*--- ESEGUE QCAEXEC DI APERTURA SUI MEMBRI ---------------------*
018300911001     C     CAEX          BEGSR
018400950109     C*
018500021003     C                   MOVE      *BLANK        VAR11            11
018600021003     C                   MOVE      *BLANK        VAR12            12
018700911001     C                   MOVE      ')'           VAR11
018800021003     C                   MOVEL     MBRFIL        VAR11
018900911001     C                   MOVE      '))'          VAR12
019000021003     C                   MOVEL     MBRFIL        VAR12
019100021003     C                   Z-ADD     55            LUNG             15 5
019200911001     C*
019300911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
019400911001     C                   SETOFF                                       20
019500951114     C                   MOVE      VAR11         CMD1                            FNFVA01R
019600911001     C                   MOVEL     *BLANKS       COMMAN
019700911001     C                   MOVEA     CMD1(1)       COMMAN           80
019800911001     C                   CALL      'QCMDEXC'                            20
019900911001     C                   PARM                    COMMAN
020000911001     C                   PARM                    LUNG
020100911001     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
020200911001     C  N20              DO
020300911001     C                   SETOFF                                       20
020400911001     C                   MOVE      VAR12         CMD2
020500911001     C                   MOVEL     *BLANKS       COMMAN
020600911001     C                   MOVEA     CMD2(1)       COMMAN
020700911001     C                   CALL      'QCMDEXC'                            20
020800911001     C                   PARM                    COMMAN
020900911001     C                   PARM                    LUNG
021000911001     C* OVRDBF
021100911001     C  N20              DO
021200911001     C                   SETOFF                                       20
021300960129     C                   MOVE      VAR11         CMD3
021400911001     C                   MOVEL     *BLANKS       COMMAN
021500960129     C                   MOVEA     CMD3(1)       COMMAN
021600911001     C                   CALL      'QCMDEXC'                            20
021700911001     C                   PARM                    COMMAN
021800911001     C                   PARM                    LUNG
021900911001     C*
022000960129     C  N20              OPEN      FNFV400R
022100911001     C                   END
022200911001     C                   END
022300950109     C*
022400911001     C                   ENDSR
022500000626     C**-------------------------------------------------------------**
022600000626     C**  ROUTINE DI CARICO LPT PER LINEE POSTE                      **
022700000626     C**-------------------------------------------------------------**
022800000626     C     CARLPT        BEGSR
022900000626     C                   CLEAR                   DSUL06
023000000626     C                   MOVE      '£1'          D06COD
023100000626     C                   MOVEL     FV4LNP        D06KEY
023200000626     C                   MOVEL     DSUL06        KPJBU
023300000626     C                   CALL      'TRUL06R'
023400000626     C                   PARM                    KPJBA
023500000626     C                   MOVEL     KPJBU         DSUL06
023600000626     C                   MOVEA     LIN           LPT
023700000626     C                   ENDSR
023800960129     C*
023900951114     C*--- SR INIZIALE -----------------------------------------------*
024000951114     C     *INZSR        BEGSR
024100951114     C*
024200951114     C****  KLIST  ****
024300960129     C* ACCESSO FNFV401L
024400960129     C     KFV4          KLIST
024500960129     C                   KFLD                    FV4LNP
024600960129     C                   KFLD                    FV4NFV
024700960129     C                   KFLD                    FV4EPA
024800960129     C                   KFLD                    FV4LAI
024900960129     C                   KFLD                    FV4TRC
025000000627     C     KFV42         KLIST
025100000627     C                   KFLD                    LPTLNP
025200000627     C                   KFLD                    FV4NFV
025300000627     C                   KFLD                    FV4EPA
025400000627     C                   KFLD                    FV4LAI
025500000627     C                   KFLD                    FV4TRC
025600960129     C* ACCESSO FNFV400R
025700960129     C     KSAV          KLIST
025800960129     C                   KFLD                    SAVEPA
025900960129     C                   KFLD                    SAVLNP
026000960129     C                   KFLD                    SAVNFV
026100960129     C                   KFLD                    SAVLAI
026200960129     C                   KFLD                    SAVTRC
026300960129     C                   KFLD                    SAVDTV
026400960129     C                   KFLD                    SAVORV
026500000626     C* ACCESSO TNFVS01L
026600000626     C     KFVS1         KLIST
026700000626     C                   KFLD                    FV4LNP
026800000626     C                   KFLD                    FV4NFV
026900000626     C     KFVS2         KLIST
027000000626     C                   KFLD                    LPTLNP            3 0
027100000626     C                   KFLD                    FV4NFV
027200960129     C*
027300960129     C****  DEFINIZIONE CAMPI  ****
027400960129     C     *LIKE         DEFINE    DSKEY         COMKEY
027500951114     C*
027600960129     C* GIRO LA DATA DEL GIORNO
027700951114     C                   TIME                    WTIME            14 0          ORA & DATA
027800960129     C                   MOVE      WTIME         UDATE8            8 0          GG/MM/AAAA
027900960129     C                   Z-ADD     UDATE8        WDATA
028000960129     C                   Z-ADD     GG            MMGG              4 0
028100960129     C                   MOVEL     MM            MMGG
028200960129     C                   Z-ADD     MMGG          DATEU             8 0
028300960129     C                   MOVEL     AA            DATEU
028400951114     C                   ENDSR
028500951114     C*
028600951114     C*---------------------------------------------------------------*
028700960129**         CMD1
028800021003CHKOBJ OBJ(FNFV400R) OBJTYPE(*FILE)     MBR(M234567890)
028900911001**         CMD2
029000021003ALCOBJ OBJ((FNFV400R *FILE *EXCL           M234567890))
029100960129**         CMD3
029200021003OVRDBF FILE(FNFV400R)                   MBR(M234567890)
