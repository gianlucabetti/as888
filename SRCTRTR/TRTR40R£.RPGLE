000100000822     H DECEDIT('0,') DATEDIT(*DMY.)
000200960130     F* TRTR40R  *---------------------------------------------------*
000300950120     F* - RICEZIONE FNFGV00R E FNFVD00R  IN SEDE                     *
000400000000     F*--------------------------------------------------------------*
000500960130     FFNFGV01R  UF   E             DISK    USROPN
000600950120     F                                     RENAME(FNFGV000:FNFGVR)
000700960322     FFNFVD01R  UF   E             DISK    USROPN
000800950120     F                                     RENAME(FNFVD000:FNFVDR)
000900000502     FFNFGW00R  UF   E           K DISK    USROPN
001000941122     FFNFVD01L  UF A E           K DISK
001100951108     FTNFVS01L  UF A E           K DISK
001200000428     FTNFWS01L  UF A E           K DISK
001300951117     FFNFV201L  UF   E           K DISK
001400000628     FFNFV401L  UF   E           K DISK
001500900517     FCMRIC01L  UF   E           K DISK
001600951117     FTABEL00F  IF   E           K DISK
001700951120     FTNTST01L  UF   E           K DISK
001800960221     FAZORG01L  IF   E           K DISK
001900960320     FSYSPRT    O    F  132        PRINTER OFLIND(*INOF)
002000960320     F                                     USROPN
002100951117     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)              ALCOBJ FNFGV
002200951117     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
002300960322     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)              ALCOBJ FNFVD
002400951117     D CMD6            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
002500000502     D CMD7            S             55    DIM(1) CTDATA PERRCD(1)              ALCOBJ FNFGW
002600000502     D CMD8            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
002700960221     D PRT             S             66    DIM(4) CTDATA PERRCD(1)
002800960326     D CMD             S              1    DIM(67) CTDATA PERRCD(67)
002900000627     D LPT             S              3  0 DIM(30)                              FIL GESTITE POSTE
003000010822
003100010822     d w_importo       s                   like(fwsipt)
003200010822     d w_divisa        s                   like(fwsvip)
003300010926     d w_fwsipt        s                   like(fwsipt)
003400010926     d w_fwsies        s                   like(fwsies)
003500010926     d w_fwsvip        s                   like(fwsvip)
003600010926     d w_fwsves        s                   like(fwsves)
003700010822
003800941122     D*
003900941122     D                 DS
004000941122     D  GG                     1      2  0
004100941122     D  MM                     3      4  0
004200941122     D  AA                     5      8  0
004300941122     D  DATA                   1      8  0
004400010822
004500010822     D yeurcods      E DS
004600010822     D dscv          E DS
004700010822
004800941122     D*
004900000627     D OG143         E DS
005000000627     D DSTV          E DS
005100960221     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005200960322     D DSTL33        E DS                  EXTNAME(TNTL33DS)
005300000627     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005400000627     D  LIN                    1     90  0
005500000627     D                                     DIM(30)                              FIL GESTITE
005600000000     D KPJBA         E DS
005700930716     D  LIBSYS                92    101
005800900517     D  LEGFIL               483    492
005900900514     D  MBRFIL               493    502
006000951117     D* DS PER PASSAGGIO PARAMETRI A PGM DI SCRITTURA GRIGLIA TRAINI
006100951117     D DSTL25          DS
006200951117     D* Data traino
006300951117     D  D25DTN                 1      8  0
006400951117     D* Numero traino
006500951117     D  D25TRN                 9     15  0
006600951117     D* lnp n. foglio viaggio
006700951117     D  D25LNP                16     18  0
006800951117     D  D25NFV                19     23  0
006900951117     D  D25TFM                24     24
007000951117     D* ric = 'S' --> pgm richiamato
007100951117     D  D25RIC                25     25
007200951117     D* IN OUTPUT --> ERRORE
007300951117     D  D25ERR                26     26
007400951114     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL29R - SBINAMENTO F.V.
007500951114     D DSTL29          DS
007600951114     D  D29TLA                 1      1
007700951114     D  D29LNP                 2      4  0
007800951114     D  D29NFV                 5      9  0
007900951114     D  D29DPD                10     19
008000951114     D  D29PRT                20     20
008100951114     D  D29ERR                21     21
008200951120     D  D29TRN                22     28  0
008300951120     D  D29DTN                29     36  0
008400960221     D WLBDA8          DS
008500960221     D  G08DAT                 1      8  0
008600960221     D  G08INV                 9     16  0
008700960221     D  G08ERR                17     17
008800960221     D  G08TGI                18     22  0
008900950120     IFNFVDR
009000941122     I              FVDLNP                      RVDLNP
009100941122     I              FVDNFV                      RVDNFV
009200941122     I              FVDLNA                      RVDLNA
009300941122     I              FVDFLP                      RVDFLP
009400950120     I              FVDTRC                      RVDTRC
009500941122     I              FVDCLA                      RVDCLA
009600941122     I              FVDKFA                      RVDKFA
009700941122     I              FVDKOA                      RVDKOA
009800950120     I              FVDTS1                      RVDTS1
009900950120     I              FVDTS2                      RVDTS2
010000950120     I              FVDTS3                      RVDTS3
010100950120     I              FVDTS4                      RVDTS4
010200950120     I              FVDTS5                      RVDTS5
010300950120     I              FVDTS6                      RVDTS6
010400950120     I              FVDVL1                      RVDVL1
010500950120     I              FVDVL2                      RVDVL2
010600950120     I              FVDVL3                      RVDVL3
010700950120     I              FVDVL4                      RVDVL4
010800950120     I              FVDVL5                      RVDVL5
010900950120     I              FVDVL6                      RVDVL6
011000950120     I              FVDSNA                      RVDSNA
011100950120     IFNFGVR
011200950120     I              FGVATB                      RGVATB
011300950120     I              FGVLNP                      RGVLNP
011400950120     I              FGVNFV                      RGVNFV
011500950120     I              FGVDFV                      RGVDFV
011600950120     I              FGVFCP                      RGVFCP
011700950120     I              FGVPDR                      RGVPDR
011800950120     I              FGVDPD                      RGVDPD
011900950120     I              FGVCCN                      RGVCCN
012000950120     I              FGVDCN                      RGVDCN
012100950120     I              FGVTRM                      RGVTRM
012200950120     I              FGVTRR                      RGVTRR
012300950120     I              FGVTMZ                      RGVTMZ
012400950120     I              FGVTTR                      RGVTTR
012500950120     I              FGVFRQ                      RGVFRQ
012600950120     I              FGVIPT                      RGVIPT
012700950120     I              FGVSNP                      RGVSNP
012800950120     I              FGVLNA                      RGVLNA
012900950120     I              FGVFFV                      RGVFFV
013000950120     I              FGVFF2                      RGVFF2
013100950120     I              FGVFLP                      RGVFLP
013200950120     I              FGVFL2                      RGVFL2
013300950120     I              FGVDTP                      RGVDTP
013400950120     I              FGVHMP                      RGVHMP
013500960130     I              FGVRTP                      RGVRTP
013600950120     I              FGVNT1                      RGVNT1
013700950120     I              FGVNT2                      RGVNT2
013800950120     I              FGVPMB                      RGVPMB
013900950120     I              FGVTRN                      RGVTRN
014000951120     C****************************************************************
014100951120     C*  RIEPILOGO INDICATORI
014200951120     C***************************************************************
014300951120     c* 01    - ON=F.V GIA' ESISTENTE ABBINATO AUTOMATICAMENTE
014400951120     c* 02    - ON=F.V GIA' ESISTENTE ABBINATO MANUALMENTE OPPURE
014500951120     C*            NON UTILIZZABILE NEI CONTREOLLI DI SEDE
014600951120     C* 30    - COMODO
014700951120     C* 31    - COMODO
014800951120     C***************************************************************
014900930120     C     *ENTRY        PLIST
015000930120     C                   PARM                    KPJBA
015100960221     C*
015200960221     C                   Z-ADD     1             CODUT
015300960221     C                   CALL      'X§PARUT'
015400960221     C                   PARM                    UT§DSE
015500960221     C                   MOVEL     RAGUT         RSUT             20
015600930120     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
015700930120     C* ASSUMO IL PRIMO MEMBRO DEL FILE
015800930120     C     MBRFIL        IFEQ      *BLANK
015900930120     C                   MOVEL     '*FIRST'      MBRFIL
016000930120     C                   END
016100000627     C*
016200000627     C                   CLEAR                   SAVLNP
016300930120     C*---------------------------------------------------------------*
016400941122     C* ACCESSO FNFVD01L
016500930714     C     KFVD          KLIST
016600000627     C                   KFLD                    KLNP
016700930714     C                   KFLD                    RVDNFV
016800931119     C                   KFLD                    RVDLNA
016900950120     C                   KFLD                    RVDTRC
017000931119     C     KFVD2         KLIST
017100950120     C                   KFLD                    RGVLNP
017200950120     C                   KFLD                    RGVNFV
017300950120     C                   KFLD                    RGVLNA
017400941122     C                   KFLD                    WTRC
017500000627     C     KFVD3         KLIST
017600000627     C                   KFLD                    LPTLNP
017700000627     C                   KFLD                    RVDNFV
017800000627     C                   KFLD                    RVDLNA
017900000627     C                   KFLD                    RVDTRC
018000950120     C* ACCESSO FNFGV01L
018100960919     C***        KFGV      KLIST
018200960919     C***                  KFLD           RGVNFV
018300960919     C***                  KFLD           RGVLNP
018400951108     C* ACCESSO TNFVS01L
018500951108     C     KFVS          KLIST
018600951108     C                   KFLD                    RGVLNP
018700951108     C                   KFLD                    RGVNFV
018800000627     C*
018900000627     C     KFVS2         KLIST
019000000627     C                   KFLD                    LPTLNP
019100000627     C                   KFLD                    RGVNFV
019200960322     C*
019300960322     C     KFVS1         KLIST
019400960322     C                   KFLD                    WLNP
019500960322     C                   KFLD                    WNFV
019600000627     C     KFVS3         KLIST
019700000627     C                   KFLD                    KLNP
019800000627     C                   KFLD                    KNFV
019900951117     C* ACCESSO FNFV201L
020000951117     C     KFV2          KLIST
020100951117     C                   KFLD                    RGVLNP
020200951117     C                   KFLD                    RGVNFV
020300951117     C                   KFLD                    WEPA
020400000627     C     KFV3          KLIST
020500000627     C                   KFLD                    LPTLNP
020600000627     C                   KFLD                    RGVNFV
020700000627     C                   KFLD                    WEPA
020800951117     C                   MOVEL     'A'           WEPA
020900951117     C* ACCESSO TABEL00F
021000951117     C     KTAB          KLIST
021100951117     C                   KFLD                    KCUT
021200951117     C                   KFLD                    KCOD
021300951117     C                   KFLD                    KKEY
021400960130     C*
021500951117     C                   Z-ADD     1             KCUT
021600951120     C* ACCESSO TNTST01L
021700951120     C     KTST          KLIST
021800951120     C                   KFLD                    D29TRN
021900951120     C                   KFLD                    D29DTN
022000950120     C* DEFINIZIONE CAMPI
022100000627     C     *LIKE         DEFINE    FVDLNP        KLNP
022200000627     C     *LIKE         DEFINE    FVDNFV        KNFV
022300000627     C     *LIKE         DEFINE    FVDTRC        WTRC
022400951117     C     *LIKE         DEFINE    TBLKUT        KCUT
022500951117     C     *LIKE         DEFINE    TBLCOD        KCOD
022600951117     C     *LIKE         DEFINE    TBLKEY        KKEY
022700951117     C     *LIKE         DEFINE    RGVTTR        COMTTR
022800951117     C     *LIKE         DEFINE    FV2ABN        COMABN
022900951117     C     *LIKE         DEFINE    FV2EPA        WEPA
023000960312     C*****      *LIKE     DEFN FVSTFM    COMTFM
023100960209     C     *LIKE         DEFINE    FVSTRS        SAVTRS
023200960209     C     *LIKE         DEFINE    FVSABN        SAVABN
023300960221     C     *LIKE         DEFINE    FVSDFV        WDFV
023400960322     C     *LIKE         DEFINE    FVDLNP        WLNP
023500960322     C     *LIKE         DEFINE    FVDNFV        WNFV
023600930120     C*---------------------------------------------------------------*
023700941122     C** NUMERO TRASMISSIONE = UDATE USA
023800941122     C** LA DATA DEL GIORNO LLA PRENDO DA TIME
023900941122     C                   TIME                    W0140            14 0
024000941122     C                   MOVE      W0140         UDATE8            8 0
024100941122     C                   Z-ADD     UDATE8        DATA
024200941122     C                   Z-ADD     GG            MMGG              4 0
024300941122     C                   MOVEL     MM            MMGG
024400941122     C                   Z-ADD     MMGG          NUMTRA            8 0
024500941122     C                   MOVEL     AA            NUMTRA
024600960221     C                   MOVEL     W0140         WORA              6 0
024700960326     C* DETERMINO CODA PER INDIRIZZAMENTO STAMPE
024800960326     C                   MOVE      KNSIF         W004A             4
024900971117     C                   MOVEL     W004A         W001A             1
025000971117     C     W001A         IFEQ      'P'
025100960326     C                   MOVEL     'QPRINTS'     WOUTQ             7
025200960326     C                   ELSE
025300960731     C                   MOVEL     'PRTRAP1'     WOUTQ
025400960326     C                   END
025500960320     C* OVRPRTF SU FILE DI STAMPA
025600960320     C                   CLEAR                   COMMAN
025700960326     C                   MOVEA     WOUTQ         CMD(27)
025800960326     C                   MOVEA     CMD           COMMAN           80
025900960320     C                   Z-ADD     66            LUNG             15 5
026000960320     C                   CALL      'QCMDEXC'                                                A
026100960320     C                   PARM                    COMMAN
026200960320     C                   PARM                    LUNG
026300960320     C                   OPEN      SYSPRT
026400930716     C*
026500930716     C* CONTATORI
026600950120     C                   Z-ADD     0             NUMFGV            7 0
026700930716     C                   Z-ADD     0             NUMFVD            7 0
026800000428     C                   Z-ADD     0             NUMFVW            7 0
026900930716     C*
027000930120     C* APERTURA MEMBRO DA TRASCODIFICARE
027100930120     C                   EXSR      CAEXA
027200930120     C*
027300930120     C* 20 ON - CHIUDO IL PROGRAMMA
027400930120     C   20              GOTO      FINE
027500930714     C***
027600950120     C***   F N F G V
027700930714     C***
027800960130     C                   READ      FNFGV01R                               30
027900941122    1C     *IN30         DOWEQ     *OFF
028000930120     C*
028100000627     C* A S T E R I S C A M E N T O   S P E C I F I C H E
028200000627     C*
028300000627     C* SE ANNULLATO MA ESISTE IL RECORD IN FNFVD, RIPRISTINO IL RECORD
028400000627    2C***        RGVATB    IFNE ' '
028500000627     C* PROVO CON TIPO RECORD "1" ---> AREA
028600000627     C***                  MOVEL'1'       WTRC
028700000627     C***        KFVD2     SETLLFNFVD000                 32
028800000627     C* PROVO CON TIPO RECORD "2" ---> TRANSITI
028900000627     C***     N32          MOVEL'2'       WTRC
029000000627     C***     N32KFVD2     SETLLFNFVD000                 32
029100000627     C* PROVO CON TIPO RECORD "3" ---> TRANSITI PARTENZA SDI
029200000627     C***     N32          MOVEL'3'       WTRC
029300000627     C***     N32KFVD2     SETLLFNFVD000                 32
029400000627     C***      32          MOVEL' '       RGVATB
029500000627    2C***                  ENDIF
029600000627      *
029700951115     C* RICHIAMO ROUTINE DI SCRITTURA T N F V S 0 0 F
029800000628     C*
029900951108     C                   EXSR      SRFVS
030000000627     C*                    ----------
030100000628     C*
030200000628     C* RIALLINEAMENTO FILES DIPENDENTI SE HO FGS AL POSTO DI LNP
030300000628     C*
030400000628     C   75              EXSR      RIALLI
030500000628      *                    -----------
030600000628     C*
030700000428     C                   DELETE    FNFGVR
030800000428     C  N36              DELETE    FNFGW000
030900000502     C  N36              ADD       1             NUMFGW            7 0
031000000428     C*
031100950120     C                   ADD       1             NUMFGV
031200930120     C*
031300960130     C                   READ      FNFGV01R                               30
031400941122    1C                   ENDDO
031500000627     C***
031600000627     C***   F N F V D
031700000627     C***
031800000627     C                   CLEAR                   WLNP
031900000627     C                   CLEAR                   WNFV
032000000627     C*
032100000627     C                   READ      FNFVD01R                               30
032200000627    1C     *IN30         DOWEQ     *OFF
032300000627     C*
032400000627     C* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
032500000627     C*
032600000627     C                   SETOFF                                       75        ON=TROVATO PT
032700000627      * .. CERCO FOGLIO VIAGGIO ANCHE CON LINEA POSTE DI LNP
032800000627      * .. SE NON TROVATO FOGLIO VIAGGIO CON LINEA DI PARTENZA
032900000627     C                   Z-ADD     RVDLNP        KLNP
033000000627     C                   Z-ADD     RVDNFV        KNFV
033100000627     C     KFVS3         CHAIN     TNFVS01L                           31
033200000627     C     *IN31         IFEQ      *ON
033300000627     C                   EXSR      CARLPT
033400000627     C     1             DO        30            PT                3 0
033500000627     C     LPT(PT)       IFNE      0
033600000627     C                   Z-ADD     LPT(PT)       LPTLNP
033700000627     C     LPTLNP        CHAIN     AZORG01L                           74
033800000627     C     *IN74         IFEQ      *OFF
033900000627     C                   MOVEL     ORGDE3        OG143
034000000627     C     §OGPT         IFEQ      'S'
034100000627      * - - - - - - - - - - - - - - - - - - - - - - - - -
034200000627     C                   Z-ADD     LPTLNP        KLNP
034300000627     C     KFVS3         CHAIN     TNFVS01L                           31
034400000627     C  N31              SETON                                        75        TROVATO
034500000627     C  N31              LEAVE
034600000627      * - - - - - - - - - - - - - - - - - - - - - - - - -
034700000627     C                   ENDIF
034800000627     C                   ENDIF
034900000627     C                   ENDIF
035000000627     C                   ENDDO
035100000627     C                   ENDIF
035200000627     C* SE NON TROVATO RIMETTO FVDLNP
035300000627     C  N75              Z-ADD     RVDLNP        KLNP
035400000627     C*
035500000627     C*  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
035600000627     C*
035700000627     C*
035800000627     C     KFVD          CHAIN     FNFVD000                           31
035900000627     C   31              CLEAR                   FNFVD000
036000000627     C*
036100000627     C                   MOVEL     KLNP          FVDLNP
036200000627     C                   MOVEL     RVDNFV        FVDNFV
036300000627     C                   MOVEL     RVDLNA        FVDLNA
036400000627     C                   MOVEL     RVDFLP        FVDFLP
036500000627     C                   MOVEL     RVDTRC        FVDTRC
036600000627     C                   Z-ADD     RVDCLA        FVDCLA
036700000627     C                   Z-ADD     RVDKFA        FVDKFA
036800000627     C                   Z-ADD     RVDKOA        FVDKOA
036900000627     C                   MOVEL     RVDTS1        FVDTS1
037000000627     C                   MOVEL     RVDTS2        FVDTS2
037100000627     C                   MOVEL     RVDTS3        FVDTS3
037200000627     C                   MOVEL     RVDTS4        FVDTS4
037300000627     C                   MOVEL     RVDTS5        FVDTS5
037400000627     C                   MOVEL     RVDTS6        FVDTS6
037500000627     C                   Z-ADD     RVDVL1        FVDVL1
037600000627     C                   Z-ADD     RVDVL2        FVDVL2
037700000627     C                   Z-ADD     RVDVL3        FVDVL3
037800000627     C                   Z-ADD     RVDVL4        FVDVL4
037900000627     C                   Z-ADD     RVDVL5        FVDVL5
038000000627     C                   Z-ADD     RVDVL6        FVDVL6
038100000627     C                   Z-ADD     RVDSNA        FVDSNA
038200000627     C*
038300000627     C                   MOVEL     'R'           FVDFTR
038400000627     C                   MOVEL     NUMTRA        FVDDTR
038500000627     C*
038600000627     C   31              WRITE     FNFVD000
038700000627      *                    =============
038800000627     C  N31              UPDATE    FNFVD000
038900000627      *                    =============
039000000627     C*
039100000627     C                   DELETE    FNFVDR
039200000627     C                   ADD       1             NUMFVD
039300000627     C* A ROTTURA DI FOGLIO VIAGGIO RICHIAMO PGM DI AGGIORNAMENTO
039400000627     C* METRI CUBI REALI SU GRIGLIA
039500000627     C     FVDLNP        IFNE      WLNP
039600000627     C     FVDNFV        ORNE      WNFV
039700000627     C     WLNP          IFGT      *ZEROS
039800000627     C                   EXSR      BRKFVG
039900000627      *                    -----------
040000000627     C                   END
040100000627     C                   MOVE      FVDLNP        WLNP
040200000627     C                   MOVE      FVDNFV        WNFV
040300000627     C                   END
040400000627     C*
040500000627     C                   READ      FNFVD01R                               30
040600000627    1C                   ENDDO
040700000627     C*
040800000627     C     WLNP          IFGT      *ZEROS
040900000627     C                   EXSR      BRKFVG
041000000627      *                    -----------
041100000627     C                   END
041200000627      *
041300000627      * O P E R A Z I O N I   F I N A L I
041400000627      *
041500960221     C* CONTROLLO SE DEVO STAMPARE MSG DI FINE STAMPA
041600960221     C     WPRT          IFNE      *BLANKS
041700960221     C                   EXCEPT    FINES
041800960221     C                   END
041900900514     C*
042000900517     C* AGGIORNA ARCHIVIO DI LOG
042100951117     C     NUMFVD        COMP      0                                  43
042200951117     C     NUMFGV        COMP      0                                  44
042300000428     C     NUMFGW        COMP      0                                  45
042400951117     C*
042500900517     C     LEGFIL        CHAIN     CMRIC01L                           20
042600951117    1C  N20              SELECT
042700951117     C     *IN44         WHENEQ    '1'
042800951117     C                   Z-ADD     NUMFGV        RICNR1
042900951117     C     *IN43         WHENEQ    '1'
043000951117     C                   Z-ADD     NUMFVD        RICNR1
043100960130     C                   SETOFF                                       43
043200951117    1C                   ENDSL
043300960130     C*
043400951117     C  N20              MOVE      '1'           RICFL2                         *AGG.TO
043500951117     C  N20              UPDATE    CMRIC000
043600930120     C*
043700000428    1C  N20RICPRR        IFNE      *BLANKS
043800930715     C     RICPRR        CHAIN     CMRIC01L                           20
043900000428    2C  N20              SELECT
044000000428     C     *IN45         WHENEQ    '1'
044100000428     C                   Z-ADD     NUMFGW        RICNR1
044200000428     C     *IN43         WHENEQ    '1'
044300000428     C                   Z-ADD     NUMFVD        RICNR1
044400000428     C                   SETOFF                                       43
044500000428    2C                   ENDSL
044600951117     C  N20              MOVE      '1'           RICFL2                          AGG.TO
044700951117     C  N20              UPDATE    CMRIC000
044800951117    1C                   ENDIF
044900000428     C*
045000000428     C  N20
045100000428    1CAN 43RICPRR        IFNE      *BLANKS
045200000428     C     RICPRR        CHAIN     CMRIC01L                           20
045300000428     C  N20              Z-ADD     NUMFVD        RICNR1
045400000428     C  N20              MOVE      '1'           RICFL2                          AGG.TO
045500000428     C  N20              UPDATE    CMRIC000
045600000428    1C                   ENDIF
045700951114     C*
045800951127     C* SOTTOMISSIONE PGM DI ABBINAMENTO AUTOMATICO FV A GRIGLIA TRAINI
045900010822     C                   MOVEL     'TL45'        KCOAZ
046000960212     C                   CLEAR                   KPJBU
046100010822     C                   CALL      'TNTL45R'
046200960212     C                   PARM                    KPJBA
046300951127     C*
046400930120     C     FINE          TAG
046500060607     c
046600060607     c* Se non sono riuscita ad allocare i files, risottometto
046700060607     c                   if        *in20
046800060607     C                   CALL      'BCH10'
046900060607     C                   PARM                    KPJBA
047000060607     c                   endif
047100951114     C*
047200900514     C                   SETON                                        LR
047300951108     C**-------------------------------------------------------------**
047400951108     C**  SCRITTURA ARCHIVIO FOGLI VIAGGIO PARTENZA IN SEDE          **
047500951108     C**-------------------------------------------------------------**
047600951108     C     SRFVS         BEGSR
047700000627     C*
047800000627     C                   SETOFF                                       75        ON = USO FGS
047900000627     C*
048000000627     C     KFVS          CHAIN     FNFGW000                           36
048100000627     C  N36FGWFGS        IFGT      0
048200000627     C                   Z-ADD     FGWFGS        LPTLNP            3 0
048300000627     C                   SETON                                        75
048400000627     C                   ENDIF
048500951108     C*
048600000627     C   75KFVS2         CHAIN     TNFVS000                           31
048700000627     C  N75KFVS          CHAIN     TNFVS000                           31
048800951108     C   31              CLEAR                   TNFVS000
048900000428     C*
049000000627     C   75KFVS2         CHAIN     TNFWS000                           35
049100000627     C  N75KFVS          CHAIN     TNFWS000                           35
049200000428     C  N35              MOVEL     'S'           WESFWS            1
049300000428     C   35              MOVEL     ' '           WESFWS            1
049400960221     C* SE ERA ANNULLATO DA FILIALE ED ORA E' FV VALIDO --> CLEAR
049500960221     C* TRAINO DI SEDE PER PERMETTERE L'ABBINAMENTO
049600960301     C     FVSATB        IFEQ      '*'
049700951121     C     RGVATB        ANDEQ     ' '
049800951121     C                   CLEAR                   FVSTRS
049900960312     C                   CLEAR                   FVSTFS
050000951121     C                   ENDIF
050100960221     C* MANTENGO ANNULLAMENTO DEL FV SE FATTO DALLA SEDE (ATB='A')
050200960221     C* E STAMPO MESSAGGIO
050300960221    1C     FVSATB        IFEQ      'A'
050400960221     C                   EXSR      PRTERR
050500960221   X1C                   ELSE
050600951108     C                   MOVEL     RGVATB        FVSATB
050700960221    1C                   END
050800951108     C                   MOVEL     RGVNFV        FVSNFV
050900951108     C                   Z-ADD     RGVDFV        FVSDFV
051000951108     C                   MOVEL     RGVLNP        FVSLNP
051100960123     C                   MOVEL     RGVSNP        FVSBIS
051200951108     C                   MOVEL     RGVPDR        FVSPDR
051300951108     C                   MOVEL     RGVDPD        FVSDPD
051400951108     C                   MOVEL     RGVCCN        FVSCCN
051500951108     C                   MOVEL     RGVDCN        FVSDCN
051600951108     C                   MOVEL     RGVTRM        FVSTRM
051700951108     C                   MOVEL     RGVTRR        FVSTRR
051800951108     C                   MOVEL     RGVTMZ        FVSTMZ
051900951108     C                   MOVEL     RGVTTR        FVSTTR
052000951108     C                   MOVE      RGVIPT        FVSIPT
052100960919     C*****      FGVSNS    IFEQ 'N'
052200960919     C*****                MOVE *ZEROS    FVSIES
052300960919     C*****                ELSE
052400951108     C                   MOVE      RGVIPT        FVSIES
052500960919     C*****                END
052600960123     C*****                MOVELRGVSNP    FVSSNP
052700951108     C                   MOVEL     RGVLNA        FVSLNA
052800951108     C                   MOVEL     RGVFFV        FVSFFV
052900951108     C                   MOVEL     RGVFF2        FVSFF2
053000951108     C                   MOVEL     RGVFLP        FVSFLP
053100951108     C                   MOVEL     RGVFL2        FVSFL2
053200951108     C                   Z-ADD     RGVDTP        FVSDRP
053300951108     C                   MOVE      RGVHMP        FVSHRP
053400960130     C                   MOVE      RGVRTP        FVSRTP
053500951108     C                   MOVEL     RGVNT1        FVSNT1
053600951108     C                   MOVEL     RGVNT2        FVSNT2
053700951108     C                   MOVEL     RGVPMB        FVSPMB
053800960209     C                   MOVEL     'R'           FVSFTR
053900960209     C                   MOVEL     NUMTRA        FVSDTR
054000960312     C* prima di aggiornarlo, salvo il campo TFM, perchè mi serve
054100010822     C* per il richiamo di TNTL45R
054200960312     C**********           MOVELFVSTFM    COMTFM
054300960312     C***31                MOVELRGVFRQ    FVSTFM
054400960314     C*
054500960314     C* PRIMA DI AGGIORNARE FVSTRN ------->
054600960314     C* Se f.v. già esistente determino se abbinato automaticamente
054700960314     C* o manualmente
054800960314     C                   SETOFF                                       0102
054900960314     C*
055000960314    1C     *IN31         IFEQ      *OFF
055100960314     C     FVSTRS        ANDGT     *ZEROS
055200960314    2C     FVSTRS        IFEQ      FVSTRN
055300960314    3C     FVSTRS        IFGE      9000000
055400960314     C                   SETON                                        02        ECCEZIONALE
055500960314   X3C                   ELSE
055600960314     C                   SETON                                        01        AUTOMATICO
055700960314    3C                   END
055800960314   X2C                   ELSE
055900960314     C                   SETON                                        02        MANUALE
056000960314    2C                   END
056100960314    1C                   END
056200960314     C*
056300960314     C     *IN31         IFEQ      *ON
056400960314     C     FVSTRN        ORLT      9000000
056500960312     C                   MOVEL     RGVFRQ        FVSTFM
056600960209     C                   MOVE      RGVTRN        FVSTRN
056700960312     C                   END
056800960229     C*
056900000627     C   75              Z-ADD     LPTLNP        FVSLNP
057000000627     C*
057100960209     C   31              WRITE     TNFVS000
057200000627      *                    =============
057300960209     C  N31              UPDATE    TNFVS000
057400000627      *                    =============
057500000428     C*
057600000428     C*     ESISTE FWS DI RICEZIONE
057700000428     C     *IN36         IFEQ      *OFF
057800000428     C                   MOVEL     FGWATB        FWSATB
057900000428     C                   MOVEL     FGWNFV        FWSNFV
058000000428     C                   MOVEL     FGWLNP        FWSLNP
058100000428     C                   MOVEL     FGWFGS        FWSFGS
058200000428     C                   MOVEL     FGWIPT        FWSIPT
058300000428     C                   MOVEL     FGWVIP        FWSVIP
058400000428     C                   MOVEL     FGWIES        FWSIES
058500000428     C                   MOVEL     FGWVES        FWSVES
058600000428     C                   MOVEL     FGWFF3        FWSFF3
058700000502     C                   MOVEL     FGWFF4        FWSFF4
058800000428     C                   MOVEL     FGWFL3        FWSFL3
058900000428     C                   MOVEL     FGWFL4        FWSFL4
059000000627     C   75              Z-ADD     LPTLNP        FWSLNP
059100000428     C     WESFWS        IFEQ      'S'
059200000502     C                   UPDATE    TNFWS000
059300000428     C                   ELSE
059400000502     C                   WRITE     TNFWS000
059500000428     C                   ENDIF
059600000428     C                   ENDIF
059700000428     C* NON ESISTE FWA DI RICEZIONE
059800000428     C   36WESFWS        IFEQ      'S'
059900000502     C                   DELETE    TNFWS000
060000000428     C                   ENDIF
060100000428     C*
060200960221     C*
060300960221     C* SE FOGLIO VIAGGIO ANNULLATO DA SEDE NON DEVO FARE ALTRO
060400960221     C* PERCHE' FVSTRS ED FVSABN SONO GIA' IMPOSTATI BENE
060500960221   1AC     FVSATB        IFNE      'A'
060600960209     C                   MOVE      FVSTRS        SAVTRS
060700960209     C                   MOVE      FVSABN        SAVABN
060800960221     C* Chaino tabella tipi traino se ttr diverso da blank
060900960221    1C     FVSTTR        IFNE      *BLANKS
061000960221     C     FVSTTR        ANDNE     COMTTR
061100960221     C                   MOVE      RGVTTR        COMTTR
061200960221     C                   MOVEL     'TV'          KCOD
061300960221     C                   MOVEL     FVSTTR        KKEY
061400960221     C                   MOVE      'S'           §TVUTC
061500960221     C     KTAB          CHAIN     TABEL00F                           30
061600960221    2C     *IN30         IFEQ      *OFF
061700960221     C                   MOVEL     TBLUNI        DSTV
061800960221    2C                   END
061900960221    1C                   END
062000951121     C*
062100951120     C* I M P O S T O   C A M P O   F V S T R S
062200951122    1C                   SELECT
062300951122     C* fvstrs sarà: = 9999999 se ttr non utilizzabile nei controlli
062400951122     C* di sede o se f.v. annullato
062500951122     C*** FOGLIO VIAGGIO ANNULLATO
062600960221     C     FVSATB        WHENNE    *BLANKS
062700960209     C                   MOVE      *ALL'9'       SAVTRS
062800951122     C*** FOGLIO VIAGGIO RICEVUTO PER LA PRIMA VOLTA
062900951122     C     *IN31         WHENEQ    *ON
063000951122     C*** FOGLIO VIAGGIO NON ABBINATO
063100951121     C     FVSTRS        OREQ      0
063200951121     C*
063300951121    2C     FVSTTR        IFNE      *BLANKS
063400951120     C     §TVUTC        ANDNE     'S'
063500960209     C                   MOVE      *ALL'9'       SAVTRS
063600951121     C* richiamo routine per flaggare date/ore arrivi reali
063700951121     C  N31              EXSR      SRFV2
063800000627      *                    ----------
063900951121    2C                   ENDIF
064000951120     C*
064100951122     C*** FOGLIO VIAGGIO NON ABBINATO (RGVTRN=0) E PRECEDENTEMENTE GIA'
064200951122     C*** RICEVUTO (*IN31=OFF) E ABBINATO IN AUTOMATICO
064300951122     C     *IN31         WHENEQ    *OFF
064400951121     C     *IN01         ANDEQ     *ON
064500951121     C     RGVTRN        ANDEQ     *ZEROS
064600951121     C                   EXSR      SBINAM
064700000627      *                    -----------
064800951121     c**
064900951122     C*** FOGLIO VIAGGIO GIA' RICEVUTO IN PRECEDENZA E ABBINATO
065000960314     C*** MANUALMENTE
065100960221     C     *IN02         WHENEQ    *ON
065200951120     C* Utilizzabile in controlli di sede
065300951120    2C     FVSTTR        IFEQ      *BLANKS
065400951120     C     §TVUTC        OREQ      'S'
065500951120     C* Se abbinato manualmente richiamo pgm di aggiornamento griglia
065600951120    3C     FVSTRS        IFNE      9999999
065700951122    3C     FVSTRS        ANDNE     9999998
065800960209     C                   CLEAR                   DSTL25
065900951120     C                   MOVE      FVSTRS        D25TRN
066000951120     C                   MOVE      FVSLNP        D25LNP
066100951120     C                   MOVEL     FVSNFV        D25NFV
066200960312     C                   MOVEL     FVSTFS        D25TFM
066300951120     C                   MOVEL     'S'           D25RIC
066400010822     C                   MOVEL     DSTL25        KPJBU
066500010822     C                   CALL      'TNTL45R'
066600951120     C                   PARM                    KPJBA
066700951120   X3C                   ELSE
066800951122     C* Se prima non era utilizzabile in controlli di sede o se non
066900951122     C* era stato abbinato perchè relativo a traino fatturato, imposto
067000951122     C* trs=0 e richiamo routine per sflaggare date reali di arrivo
067100951122    4C     FVSTRS        IFEQ      9999999
067200951122     C     FVSTRS        OREQ      9999998
067300951122     C     FVSTRN        ANDEQ     *ZEROS
067400960209     C                   MOVE      *ZEROS        SAVTRS
067500951120     C                   EXSR      SRFV2
067600000627      *                    ----------
067700951122    4C                   END
067800951120    3C                   END
067900951121     C*
068000951120   X2C                   ELSE
068100951122     C* Non utilizzabile in sede
068200951121    3C     FVSTRS        IFNE      9999999
068300951122    3C     FVSTRS        ANDNE     9999998
068400951121     C                   EXSR      SBINAM
068500000627      *                    -----------
068600951122     C                   ELSE
068700951122    4C     FVSTRS        IFEQ      9999998
068800960209     C                   MOVE      *ALL'9'       SAVTRS
068900951122    4C                   END
069000951121    3C                   ENDIF
069100951121    2C                   ENDIF
069200951122    1C                   ENDSL
069300951120     C*
069400951120     C* I  M P O S T O   C A M P O   F V S A B N
069500951109     C* Flaggo record per impedire che venga abbinato in automatico
069600951109     C* nei seguenti casi:
069700951109     C* - numero traino della filiale = 0: è necessario un abbinamento
069800951109     C*   manuale
069900960108     C* - foglio viaggio annullato o non utilizzabile in controlli di
070000960209     C*   sede (SAVTRS=9999999)
070100951122     C* - foglio viaggio già abbinato manualmente
070200951122     C     FVSTRN        IFEQ      *ZEROS
070300960209     C     SAVTRS        OREQ      9999999
070400951120     C     *IN02         OREQ      *ON
070500960209     C                   MOVE      'S'           SAVABN
070600951108     C                   ELSE
070700960209     C                   MOVE      ' '           SAVABN
070800951108     C                   END
070900951108     C*
071000951108     C*
071100960209     C     FVSTRS        IFNE      SAVTRS
071200960209     C     FVSABN        ORNE      SAVABN
071300000627     C   75KFVS2         CHAIN     TNFVS000                           31
071400000627     C  N75KFVS          CHAIN     TNFVS000                           31
071500960209     C     *IN31         IFEQ      *OFF
071600960209     C                   MOVE      SAVTRS        FVSTRS
071700960209     C                   MOVE      SAVABN        FVSABN
071800960314     C     FVSTRS        IFEQ      *ZEROS
071900960314     C                   CLEAR                   FVSTFS
072000960314     C                   END
072100960209     C                   UPDATE    TNFVS000
072200000627      *                    =============
072300960209     C                   END
072400960209     C                   END
072500960221   1AC                   END
072600951108     C                   ENDSR
072700000628     C**-------------------------------------------------------------**
072800000628     C**  ROUTINE DI RIALLINEAMENTO FILES DIPENDENTI                 **
072900000628     C**  (FV4, FV2, FVD)                                            **
073000000628     C**-------------------------------------------------------------**
073100000628     C     RIALLI        BEGSR
073200000628     C     KFVS          SETLL     FNFV401L
073300000628     C                   DO        *HIVAL
073400000628     C     KFVS          READE     FNFV401L                               74
073500000628     C   74              LEAVE
073600000628     C                   Z-ADD     LPTLNP        FV4LNP
073700000628     C                   UPDATE    FNFV4000
073800000628     C                   ENDDO
073900000628     C     KFVS          SETLL     FNFV201L
074000000628     C                   DO        *HIVAL
074100000628     C     KFVS          READE     FNFV201L                               74
074200000628     C   74              LEAVE
074300000628     C                   Z-ADD     LPTLNP        FV2LNP
074400000628     C                   UPDATE    FNFV2000
074500000628     C                   ENDDO
074600000628     C     KFVS          SETLL     FNFVD01L
074700000628     C                   DO        *HIVAL
074800000628     C     KFVS          READE     FNFVD01L                               74
074900000628     C   74              LEAVE
075000000628     C                   Z-ADD     LPTLNP        FVDLNP
075100000628     C                   UPDATE    FNFVD000
075200000628     C                   ENDDO
075300000628     C                   ENDSR
075400000627     C**-------------------------------------------------------------**
075500000627     C**  ROUTINE DI CARICO LPT PER LINEE POSTE                      **
075600000627     C**-------------------------------------------------------------**
075700000627     C     CARLPT        BEGSR
075800000627     C     RVDLNP        IFNE      SAVLNP
075900000627     C                   CLEAR                   DSUL06
076000000627     C                   MOVE      '£1'          D06COD
076100000627     C                   MOVEL     RVDLNP        D06KEY
076200000627     C                   MOVEL     DSUL06        KPJBU
076300000627     C                   CALL      'TRUL06R'
076400000627     C                   PARM                    KPJBA
076500000627     C                   MOVEL     KPJBU         DSUL06
076600000627     C                   MOVEA     LIN           LPT
076700000627     C                   Z-ADD     RVDLNP        SAVLNP            3 0
076800000627     C                   ENDIF
076900000627     C                   ENDSR
077000951121     C**-------------------------------------------------------------**
077100951121     C**  SBINAMENTO DI UN FV
077200951121     C**-------------------------------------------------------------**
077300951121     C     SBINAM        BEGSR
077400951121     C* - Sbino f.v.
077500951121     C                   CLEAR                   D29ERR
077600951121     C                   MOVE      *BLANKS       D29TLA
077700951121     C                   MOVE      FVSLNP        D29LNP
077800951121     C                   MOVE      FVSNFV        D29NFV
077900951121     C                   MOVEL     FVSDPD        D29DPD
078000951121     C                   MOVEL     'S'           D29PRT
078100951121     C                   MOVEL     DSTL29        KPJBU
078200951121     C                   CALL      'TNTL29R'
078300960209     C                   PARM                    KPJBA
078400951121     C                   MOVEL     KPJBU         DSTL29
078500951121     C*
078600951121     C* - FVSTRS sarà: = 0 se TTR UTILIZZABILE in controlli di sede
078700951121     C*                = 9999999 se TTR NON UTILIZZABILE nei controlli
078800951121     C*                  di sede
078900951121    1C     D29ERR        IFEQ      *BLANKS
079000951121     C*
079100951121    2C     FVSTTR        IFEQ      *BLANKS
079200951121     C     §TVUTC        OREQ      'S'
079300960209     C                   MOVE      *ZEROS        SAVTRS
079400960314     C**********           CLEARFVSTFM
079500951121     C                   ELSE
079600960221     C                   MOVE      *ALL'9'       SAVTRS
079700951121     C* richiamo routine per flaggare date/ore arrivi reali
079800951121     C                   EXSR      SRFV2
079900000627      *                    ----------
080000951121    2C                   END
080100951121   X1C                   ELSE
080200960108     C* Se sbinamento non riuscito perchè testata già fatturata
080300960108     C* aggiorno su griglia IPT e IES
080400951121    2C     D29ERR        IFEQ      'E'
080500951121     C     KTST          CHAIN     TNTST01L                           30
080600951121    3C     *IN30         IFEQ      *OFF
080700010822
080800010926      * controllo se devo prendere i dati dal file principale o
080900010926      * dall'estensione
081000010926      * se l'importo dell'estensione è > 0 prendo quello che trovo
081100010926      * perchè vuol dire che stanno già girando i programmi nuovi modificati per l'euro
081200010926     c                   if        fwsipt > 0
081300010926     c                   eval      w_fwsvip = fwsvip
081400010926     c                   z-add     fwsipt        w_fwsipt
081500010926     c                   endif
081600010926     c                   if        fwsies > 0
081700010926     c                   eval      w_fwsves = fwsves
081800010926     c                   z-add     fwsies        w_fwsies
081900010926     c                   endif
082000010926      * se l'importo dell'estensione è = 0 e l'importo sul file
082100010926      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
082200010926      * la divisa è lire
082300010926     c                   if        fwsipt = *zeros and fvsipt <> 0
082400010926     c                   eval      w_fwsvip = 'ITL'
082500010926     c                   z-add     fvsipt        w_fwsipt
082600010926     c                   endif
082700010926     c                   if        fwsies = *zeros and fvsies <> 0
082800010926     c                   eval      w_fwsves = 'ITL'
082900010926     c                   z-add     fvsies        w_fwsies
083000010926     c                   endif
083100010926      * se l'importo dell'estensione è = 0 e l'importo sul file
083200010926      * principale è = 0, imposto la divisa a blank per poi prendere quella della
083300010926      * griglia, così non sto a convertire un importo a 0
083400010926     c                   if        fwsipt = *zeros and fvsipt = 0
083500010926     c                   z-add     fvsipt        w_fwsipt
083600010926     c                   clear                   w_fwsvip
083700010926     c                   endif
083800010926     c                   if        fwsies = *zeros and fvsies = 0
083900010926     c                   z-add     fvsies        w_fwsies
084000010926     c                   clear                   w_fwsves
084100010926     c                   endif
084200010926      * se ho la divisa di comodo del foglio viaggio a blank devo prendere quella
084300010926      * della griglia
084400010926     c                   if        w_fwsvip = *blanks
084500010926     c                   eval      w_fwsvip = tstdiv
084600010926     c                   endif
084700010926     c                   if        w_fwsves = *blanks
084800010926     c                   eval      w_fwsves = tstdiv
084900010926     c                   endif
085000010822      * aggancio la tabella CV per stabilire i decimali della divisa della
085100010822      * griglia
085200010822     c                   clear                   dscv
085300010822     c                   movel     'CV'          kcod
085400010822     c                   movel(p)  tstdiv        kkey
085500010822     c     ktab          chain     tabel00f
085600010822     c                   if        %found(tabel00f)
085700010822     c                   movel     tbluni        dscv
085800010822     c                   endif
085900010822      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
086000010822      * devo convertire fwsipt e fwsies nella divisa della griglia
086100010926     c                   eval      w_importo = w_fwsipt
086200010926     c                   if        w_fwsvip <> tstdiv
086300010926     c                   eval      w_divisa = w_fwsvip
086400010822     c                   exsr      Sr_Converti
086500010822     c                   endif
086600010822     c                   eval      tstipt = w_importo
086700010822     C**!!!              MOVE      FVSIPT        TSTIPT
086800010926     c                   eval      w_importo = w_fwsies
086900010926     c                   if        w_fwsves <> tstdiv
087000010926     c                   eval      w_divisa = w_fwsves
087100010822     c                   exsr      Sr_Converti
087200010822     c                   endif
087300010822     c                   eval      tsties = w_importo
087400010822     C**!!!              MOVE      FVSIES        TSTIES
087500951121     C                   UPDATE    TNTST000
087600951121    3C                   END
087700951121    2C                   END
087800951121    1C                   END
087900951121     C                   ENDSR
088000010822      *----------------------------------------------------------------
088100010822      * CONVERTI - Routine conversione importo
088200010822      *----------------------------------------------------------------
088300010822     c     Sr_Converti   begsr
088400010822
088500010822     c                   clear                   yeurcods
088600010822     c                   movel     w_divisa      yecdvi
088700010822     c                   movel     tstdiv        yecdvo
088800010822     c                   z-add     w_importo     yecimi
088900010822     c                   move      §cvdec        yecdco
089000010822     c                   call      'YEURCO'
089100010822     c                   parm                    yeurcods
089200010822    5c                   if        yecesi  = ' '
089300010822     c                   z-add     yecimo        w_importo
089400010822    5c                   endif
089500010822
089600010822     c                   endsr
089700010822
089800951117     C**-------------------------------------------------------------**
089900951117     C**  IMPOSTAZIONE FLAG FV2ABN SU DATE/ORE ARRIVO REALI DEL F.V. **
090000951117     C**-------------------------------------------------------------**
090100951117     C     SRFV2         BEGSR
090200951117     C* DETERMINO SE DEVO FLAGGARE O SFLAGGARE I RECORDS DI FNFV2
090300951121     C     FVSTTR        IFEQ      *BLANKS
090400951117     C     §TVUTC        OREQ      'S'
090500951117     C                   MOVE      *BLANKS       COMABN
090600951117     C                   ELSE
090700951117     C                   MOVEL     'S'           COMABN
090800951117     C                   END
090900951117     C* AGGIORNO FLAG SU FNFV2
091000000627     C   75KFV3          SETLL     FNFV201L
091100000627     C  N75KFV2          SETLL     FNFV201L
091200000627     C   75KFV3          READE     FNFV201L                               30
091300000627     C  N75KFV2          READE     FNFV201L                               30
091400951117    2C     *IN30         DOWEQ     *OFF
091500951117     C                   MOVEL     COMABN        FV2ABN
091600951117     C                   UPDATE    FNFV2000
091700000627     C   75KFV3          READE     FNFV201L                               30
091800000627     C  N75KFV2          READE     FNFV201L                               30
091900951117    2C                   ENDDO
092000951117     C                   ENDSR
092100960221     C**-------------------------------------------------------------**
092200960221     C**  STAMPA FOGLIO VIAGGIO GIA' ANNULLATO DA SEDE               **
092300960221     C**-------------------------------------------------------------**
092400960221     C     PRTERR        BEGSR
092500960221     C* Decodifico linea partenza
092600000627     C   75LPTLNP        CHAIN     AZORG01L                           30
092700000627     C  N75RGVLNP        CHAIN     AZORG01L                           30
092800960221     C  N30              MOVEL     ORGDES        WDLNP            10
092900960221     C   30              CLEAR                   WDLNP
093000960221     C* Rovescio data foglio viaggio
093100960221     C                   CLEAR                   WLBDA8
093200960221     C                   MOVE      RGVDFV        G08INV
093300960221     C                   MOVEL     '3'           G08ERR
093400960221     C                   CALL      'XSRDA8'
093500960221     C                   PARM                    WLBDA8
093600960221     C                   MOVE      G08DAT        WDFV
093700960221     C* Stampo intestazione la prima volta o per overflow
093800960221     C     WPRT          IFEQ      *BLANKS
093900960221     C     *INOF         OREQ      *ON
094000960221     C                   EXCEPT    TESTA
094100960221     C                   MOVE      '1'           WPRT              1
094200960221     C                   END
094300960221     C                   EXCEPT    RIGA
094400960221     C                   ENDSR
094500960322     C**-------------------------------------------------------------**
094600960322     C**  RICHIAMO PGM DI AGGIORNAMENTO METRI CUBI REALI             **
094700960322     C**-------------------------------------------------------------**
094800960322     C     BRKFVG        BEGSR
094900960322     C* CHAINO ARCHIVIO FOGLI VIAGGIO PER VEDERE SE RICEVUTO
095000960322     C     KFVS1         CHAIN(N)  TNFVS01L                           30
095100960322     C     *IN30         IFEQ      *OFF
095200960322     C     FVSTRS        ANDGT     *ZEROS
095300960322     C                   CLEAR                   DSTL33
095400960322     C                   MOVEL     'L'           D33TLA
095500960322     C                   MOVE      FVSLNP        D33LNP
095600960322     C                   MOVE      FVSNFV        D33NFV
095700960322     C                   MOVEL     DSTL33        KPJBU
095800960322     C                   CALL      'TNTL33R'
095900960322     C                   PARM                    KPJBA
096000960322     C                   END
096100960322     C                   ENDSR
096200930120     C**-------------------------------------------------------------**
096300930714     C**  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                      **
096400930120     C**-------------------------------------------------------------**
096500900514     C     CAEXA         BEGSR
096600900514     C                   MOVE      ')'           VAR11            11
096700900514     C                   MOVEL     MBRFIL        VAR11
096800900514     C                   MOVE      '))'          VAR12            12
096900900514     C                   MOVEL     MBRFIL        VAR12
097000900514     C                   Z-ADD     55            LUNG             15 5
097100900514     C*
097200930120     C** ALLOCO MEMBRO
097300930120     C** SE NON RIESCE CHIUDO IL PROGRAMMA
097400060607     C**                 SETOFF                                       20
097500060607     C**                 MOVE      VAR12         CMD2                           FNFGV01R
097600060607     C**                 MOVEL     *BLANKS       COMMAN
097700060607     C**                 MOVEA     CMD2(1)       COMMAN
097800060607     C**                 CALL      'QCMDEXC'                            20
097900060607     C**                 PARM                    COMMAN
098000060607     C**                 PARM                    LUNG
098100930120     C*
098200060607     C** 20              GOTO      ENDCAE
098300000502     C*
098400000502     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE LO CREO
098500060607     C                   SETOFF                                       20
098600060607     C                   MOVE      VAR11         CMD7                            FNFGW00R
098700060607     C                   MOVEL     *BLANKS       COMMAN
098800060607     C                   MOVEA     CMD7(1)       COMMAN           80
098900060607     C                   CALL      'QCMDEXC'                            20
099000060607     C                   PARM                    COMMAN
099100060607     C                   PARM                    LUNG
099200000502     C*
099300060607     C     *IN20         IFEQ      '1'
099400000502     C                   MOVEL     'FNFGW00R'    FISICO
099500000502     C                   EXSR      CRTMBR
099600000502     C                   END
099700060607     C** 20              GOTO      ENDCAE
099800951122     C*
099900960322     C** ALCOBJ --> CONTROLLO SE ESISTE IL MEMBRO
100000060607     C                   MOVE      VAR11         CMD5                           FNFVD01R
100100960322     C                   MOVEL     *BLANKS       COMMAN
100200960322     C                   MOVEA     CMD5(1)       COMMAN
100300960322     C                   CALL      'QCMDEXC'                            20
100400960322     C                   PARM                    COMMAN
100500960322     C                   PARM                    LUNG
100600930716     C*
100700960322     C* SE NON ESISTE: ADDLFM
100800960322     C     *IN20         IFEQ      '1'
100900950120     C                   MOVEL     'FNFVD00R'    FISICO
101000960322     C                   MOVEL     'FNFVD01R'    LOGICO
101100930716     C*
101200930716     C                   CALL      'TRUL50C'
101300930716     C                   PARM                    FISICO           10
101400930716     C                   PARM                    MBRFIL
101500930716     C                   PARM                    LIBSYS
101600930716     C                   PARM                    LOGICO           10
101700960322     C                   PARM                    MBRFIL
101800930716     C                   PARM                    ULFLG             1
101900960322     C* ALCOBJ --> CONTROLLA NUOVAMENTE L'ESISTENZA DEL MEMBRO
102000060607     C***                SETOFF                                       20
102100060607     C***                CALL      'QCMDEXC'                            20
102200060607     C***                PARM                    COMMAN
102300060607     C***                PARM                    LUNG
102400930716     C                   ENDIF
102500930716     C*
102600960322     C     *IN20         IFEQ      *OFF
102700930716     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
102800960130     C                   MOVE      VAR11         CMD3                           FNFGV01R
102900930716     C                   MOVEL     *BLANKS       COMMAN
103000930716     C                   MOVEA     CMD3(1)       COMMAN
103100930716     C                   CALL      'QCMDEXC'                            20
103200930716     C                   PARM                    COMMAN
103300930716     C                   PARM                    LUNG
103400930715     C*
103500951115     C   20              GOTO      ENDCAE
103600000502     C*
103700000502     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
103800000502     C                   MOVE      VAR11         CMD8                           FNFGW00R
103900000502     C                   MOVEL     *BLANKS       COMMAN
104000000502     C                   MOVEA     CMD8(1)       COMMAN
104100000502     C                   CALL      'QCMDEXC'                            20
104200000502     C                   PARM                    COMMAN
104300000502     C                   PARM                    LUNG
104400000502     C*
104500000502     C   20              GOTO      ENDCAE
104600930716     C*
104700960322     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO               FNFVD01R
104800930716     C                   MOVE      VAR11         CMD6
104900900514     C                   MOVEL     *BLANKS       COMMAN
105000930716     C                   MOVEA     CMD6(1)       COMMAN
105100900514     C                   CALL      'QCMDEXC'                            20
105200900514     C                   PARM                    COMMAN
105300900514     C                   PARM                    LUNG
105400951115     C*
105500951115     C   20              GOTO      ENDCAE
105600960322     C                   END
105700951115     C*
105800960130     C                   OPEN      FNFGV01R
105900000502     C                   OPEN      FNFGW00R
106000960322     C                   OPEN      FNFVD01R
106100060607     c                   setoff                                       20
106200930716     C     ENDCAE        ENDSR
106300000502     C**-----------------------------------------------*
106400000502     C     CRTMBR        BEGSR
106500000502     C*
106600000502     C                   MOVEL     MBRFIL        FISMBR
106700000502     C                   CALL      'TRUL50C'
106800000502     C                   PARM                    FISICO           10
106900000502     C                   PARM                    FISMBR           10
107000000502     C                   PARM                    KNSIF
107100000502     C                   PARM                    LOGICO           10
107200000502     C                   PARM                    LOGMBR           10
107300000502     C                   PARM                    ULFLG             1
107400000502     C* RITENTO L'ALLOCAZIONE
107500060607     C**                 CALL      'QCMDEXC'                            20
107600060607     C**                 PARM                    COMMAN
107700060607     C**                 PARM                    LUNG
107800060607     c                   setoff                                       20
107900000502     C*
108000000502     C                   ENDSR
108100930714     C*---------------------------------------------------------------*
108200960221     OSYSPRT    E            TESTA          1  1
108300960221     O                       PRT(1)              66
108400960221     O                       PRT(2)             132
108500960221     O                       RSUT                20
108600960221     O                       UDATE8        8    119
108700960221     O                       PAGE          Z    132
108800960221     O          E            TESTA          2
108900960221     O                       KNSIF               10
109000960221     O                       KNMUS               21
109100960221     O                       WORA               119 '  :  :  '
109200960221     O          E            RIGA           1
109300000627     O               75      LPTLNP        Z      3
109400000627     O              N75      RGVLNP        Z      3
109500960221     O                                            4 '/'
109600960221     O                       RGVNFV        Z      9
109700960221     O                       WDLNP               20
109800960221     O                                           25 'DEL'
109900960221     O                       WDFV          8     36
110000960221     O          E            FINES          1
110100960221     O                       PRT(4)              66
110200960326**         CMD2
110300960130ALCOBJ OBJ((FNFGV01R *FILE *EXCL           M234567890))
110400900514**         CMD3
110500060607OVRDBF FILE(FNFGV01R)                   MBR(M234567890)
110600930714**         CMD5
110700060607CHKOBJ OBJ(FNFVD01R) OBJTYPE(*FILE)     MBR(M         )
110800930714**         CMD6
110900960322OVRDBF FILE(FNFVD01R)                   MBR(M234567890)
111000000502**         CMD7
111100060607CHKOBJ OBJ(FNFGW00R) OBJTYPE(*FILE)     MBR(M         )
111200000502**         CMD8
111300000502OVRDBF FILE(FNFGW00R)                   MBR(M234567890)
111400960221**         PRT
111500960221XXXXXXXXXXXXXXXXXXXX  **  ELENCO FOGLI VIAGGIO RICEVUTI GIA' ANNUL  01
111600960221LATI DA SEDE  **               TRTR40R     XX/XX/XXXX    PAG. XXXX  02
111700960221XXX/XXXXX 1234567890  DEL XX/XX/XXXX                                03
111800960221                                         ***   FINE   STAMPA   ***  04
111900960320**         CMD
112000960326OVRPRTF FILE(SYSPRT) OUTQ(       ) SAVE(*YES) USRDTA('Ric.Variaz')
