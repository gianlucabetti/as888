000100000822     H DECEDIT('0,') DATEDIT(*DMY.)
000200960130     F* TRTR40R  *---------------------------------------------------*
000300950120     F* - RICEZIONE FNFGV00R E FNFVD00R  IN SEDE                     *
000400000000     F*--------------------------------------------------------------*
000500960130     FFNFGV01R  UF   E             DISK    USROPN
000600950120     F                                     RENAME(FNFGV000:FNFGVR)
000700960322     FFNFVD01R  UF   E             DISK    USROPN
000800950120     F                                     RENAME(FNFVD000:FNFVDR)
000900000502     FFNFGW00R  UF   E           K DISK    USROPN
001000121102     FFNFGs00R  UF   E           K DISK    USROPN prefix(RGS:3)
001100941122     FFNFVD01L  UF A E           K DISK
001200951108     FTNFVS01L  UF A E           K DISK
001300000428     FTNFWS01L  UF A E           K DISK
001400121102     FTNFGS01L  UF A E           K DISK    RENAME(FNFgs000:TNFGS000)
001500951117     FFNFV201L  UF   E           K DISK
001600000628     FFNFV401L  UF   E           K DISK
001700900517     FCMRIC01L  UF   E           K DISK
001800951117     FTABEL00F  IF   E           K DISK
001900951120     FTNTST01L  UF   E           K DISK
002000960221     FAZORG01L  IF   E           K DISK
002100960320     FSYSPRT    O    F  132        PRINTER OFLIND(*INOF)
002200960320     F                                     USROPN
002300951117     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)              ALCOBJ FNFGV
002400951117     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
002500960322     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)              ALCOBJ FNFVD
002600951117     D CMD6            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
002700000502     D CMD7            S             55    DIM(1) CTDATA PERRCD(1)              ALCOBJ FNFGW
002800000502     D CMD8            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
002900121102     D CMD9            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
003000121102     D CMD10           S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
003100960221     D PRT             S             66    DIM(4) CTDATA PERRCD(1)
003200960326     D CMD             S              1    DIM(67) CTDATA PERRCD(67)
003300000627     D LPT             S              3  0 DIM(30)                              FIL GESTITE POSTE
003400010822
003500010822     d w_importo       s                   like(fwsipt)
003600010822     d w_divisa        s                   like(fwsvip)
003700010926     d w_fwsipt        s                   like(fwsipt)
003800010926     d w_fwsies        s                   like(fwsies)
003900010926     d w_fwsvip        s                   like(fwsvip)
004000010926     d w_fwsves        s                   like(fwsves)
004100010822
004200941122     D*
004300941122     D                 DS
004400941122     D  GG                     1      2  0
004500941122     D  MM                     3      4  0
004600941122     D  AA                     5      8  0
004700941122     D  DATA                   1      8  0
004800010822
004900010822     D yeurcods      E DS
005000010822     D dscv          E DS
005100010822
005200941122     D*
005300000627     D OG143         E DS
005400000627     D DSTV          E DS
005500960221     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005600960322     D DSTL33        E DS                  EXTNAME(TNTL33DS)
005700000627     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005800000627     D  LIN                    1     90  0
005900000627     D                                     DIM(30)                              FIL GESTITE
006000000000     D KPJBA         E DS
006100930716     D  LIBSYS                92    101
006200900517     D  LEGFIL               483    492
006300900514     D  MBRFIL               493    502
006400951117     D* DS PER PASSAGGIO PARAMETRI A PGM DI SCRITTURA GRIGLIA TRAINI
006500951117     D DSTL25          DS
006600951117     D* Data traino
006700951117     D  D25DTN                 1      8  0
006800951117     D* Numero traino
006900951117     D  D25TRN                 9     15  0
007000951117     D* lnp n. foglio viaggio
007100951117     D  D25LNP                16     18  0
007200951117     D  D25NFV                19     23  0
007300951117     D  D25TFM                24     24
007400951117     D* ric = 'S' --> pgm richiamato
007500951117     D  D25RIC                25     25
007600951117     D* IN OUTPUT --> ERRORE
007700951117     D  D25ERR                26     26
007800951114     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL29R - SBINAMENTO F.V.
007900951114     D DSTL29          DS
008000951114     D  D29TLA                 1      1
008100951114     D  D29LNP                 2      4  0
008200951114     D  D29NFV                 5      9  0
008300951114     D  D29DPD                10     19
008400951114     D  D29PRT                20     20
008500951114     D  D29ERR                21     21
008600951120     D  D29TRN                22     28  0
008700951120     D  D29DTN                29     36  0
008800960221     D WLBDA8          DS
008900960221     D  G08DAT                 1      8  0
009000960221     D  G08INV                 9     16  0
009100960221     D  G08ERR                17     17
009200960221     D  G08TGI                18     22  0
009300950120     IFNFVDR
009400941122     I              FVDLNP                      RVDLNP
009500941122     I              FVDNFV                      RVDNFV
009600941122     I              FVDLNA                      RVDLNA
009700941122     I              FVDFLP                      RVDFLP
009800950120     I              FVDTRC                      RVDTRC
009900941122     I              FVDCLA                      RVDCLA
010000941122     I              FVDKFA                      RVDKFA
010100941122     I              FVDKOA                      RVDKOA
010200950120     I              FVDTS1                      RVDTS1
010300950120     I              FVDTS2                      RVDTS2
010400950120     I              FVDTS3                      RVDTS3
010500950120     I              FVDTS4                      RVDTS4
010600950120     I              FVDTS5                      RVDTS5
010700950120     I              FVDTS6                      RVDTS6
010800950120     I              FVDVL1                      RVDVL1
010900950120     I              FVDVL2                      RVDVL2
011000950120     I              FVDVL3                      RVDVL3
011100950120     I              FVDVL4                      RVDVL4
011200950120     I              FVDVL5                      RVDVL5
011300950120     I              FVDVL6                      RVDVL6
011400950120     I              FVDSNA                      RVDSNA
011500950120     IFNFGVR
011600950120     I              FGVATB                      RGVATB
011700950120     I              FGVLNP                      RGVLNP
011800950120     I              FGVNFV                      RGVNFV
011900950120     I              FGVDFV                      RGVDFV
012000950120     I              FGVFCP                      RGVFCP
012100950120     I              FGVPDR                      RGVPDR
012200950120     I              FGVDPD                      RGVDPD
012300950120     I              FGVCCN                      RGVCCN
012400950120     I              FGVDCN                      RGVDCN
012500950120     I              FGVTRM                      RGVTRM
012600950120     I              FGVTRR                      RGVTRR
012700950120     I              FGVTMZ                      RGVTMZ
012800950120     I              FGVTTR                      RGVTTR
012900950120     I              FGVFRQ                      RGVFRQ
013000950120     I              FGVIPT                      RGVIPT
013100950120     I              FGVSNP                      RGVSNP
013200950120     I              FGVLNA                      RGVLNA
013300950120     I              FGVFFV                      RGVFFV
013400950120     I              FGVFF2                      RGVFF2
013500950120     I              FGVFLP                      RGVFLP
013600950120     I              FGVFL2                      RGVFL2
013700950120     I              FGVDTP                      RGVDTP
013800950120     I              FGVHMP                      RGVHMP
013900960130     I              FGVRTP                      RGVRTP
014000950120     I              FGVNT1                      RGVNT1
014100950120     I              FGVNT2                      RGVNT2
014200950120     I              FGVPMB                      RGVPMB
014300950120     I              FGVTRN                      RGVTRN
014400951120     C****************************************************************
014500951120     C*  RIEPILOGO INDICATORI
014600951120     C***************************************************************
014700951120     c* 01    - ON=F.V GIA' ESISTENTE ABBINATO AUTOMATICAMENTE
014800951120     c* 02    - ON=F.V GIA' ESISTENTE ABBINATO MANUALMENTE OPPURE
014900951120     C*            NON UTILIZZABILE NEI CONTREOLLI DI SEDE
015000951120     C* 30    - COMODO
015100951120     C* 31    - COMODO
015200951120     C***************************************************************
015300930120     C     *ENTRY        PLIST
015400930120     C                   PARM                    KPJBA
015500960221     C*
015600960221     C                   Z-ADD     1             CODUT
015700960221     C                   CALL      'X§PARUT'
015800960221     C                   PARM                    UT§DSE
015900960221     C                   MOVEL     RAGUT         RSUT             20
016000930120     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
016100930120     C* ASSUMO IL PRIMO MEMBRO DEL FILE
016200930120     C     MBRFIL        IFEQ      *BLANK
016300930120     C                   MOVEL     '*FIRST'      MBRFIL
016400930120     C                   END
016500000627     C*
016600000627     C                   CLEAR                   SAVLNP
016700930120     C*---------------------------------------------------------------*
016800941122     C* ACCESSO FNFVD01L
016900930714     C     KFVD          KLIST
017000000627     C                   KFLD                    KLNP
017100930714     C                   KFLD                    RVDNFV
017200931119     C                   KFLD                    RVDLNA
017300950120     C                   KFLD                    RVDTRC
017400931119     C     KFVD2         KLIST
017500950120     C                   KFLD                    RGVLNP
017600950120     C                   KFLD                    RGVNFV
017700950120     C                   KFLD                    RGVLNA
017800941122     C                   KFLD                    WTRC
017900000627     C     KFVD3         KLIST
018000000627     C                   KFLD                    LPTLNP
018100000627     C                   KFLD                    RVDNFV
018200000627     C                   KFLD                    RVDLNA
018300000627     C                   KFLD                    RVDTRC
018400950120     C* ACCESSO FNFGV01L
018500960919     C***        KFGV      KLIST
018600960919     C***                  KFLD           RGVNFV
018700960919     C***                  KFLD           RGVLNP
018800951108     C* ACCESSO TNFVS01L
018900951108     C     KFVS          KLIST
019000951108     C                   KFLD                    RGVLNP
019100951108     C                   KFLD                    RGVNFV
019200000627     C*
019300000627     C     KFVS2         KLIST
019400000627     C                   KFLD                    LPTLNP
019500000627     C                   KFLD                    RGVNFV
019600960322     C*
019700960322     C     KFVS1         KLIST
019800960322     C                   KFLD                    WLNP
019900960322     C                   KFLD                    WNFV
020000000627     C     KFVS3         KLIST
020100000627     C                   KFLD                    KLNP
020200000627     C                   KFLD                    KNFV
020300951117     C* ACCESSO FNFV201L
020400951117     C     KFV2          KLIST
020500951117     C                   KFLD                    RGVLNP
020600951117     C                   KFLD                    RGVNFV
020700951117     C                   KFLD                    WEPA
020800000627     C     KFV3          KLIST
020900000627     C                   KFLD                    LPTLNP
021000000627     C                   KFLD                    RGVNFV
021100000627     C                   KFLD                    WEPA
021200951117     C                   MOVEL     'A'           WEPA
021300951117     C* ACCESSO TABEL00F
021400951117     C     KTAB          KLIST
021500951117     C                   KFLD                    KCUT
021600951117     C                   KFLD                    KCOD
021700951117     C                   KFLD                    KKEY
021800960130     C*
021900951117     C                   Z-ADD     1             KCUT
022000951120     C* ACCESSO TNTST01L
022100951120     C     KTST          KLIST
022200951120     C                   KFLD                    D29TRN
022300951120     C                   KFLD                    D29DTN
022400950120     C* DEFINIZIONE CAMPI
022500000627     C     *LIKE         DEFINE    FVDLNP        KLNP
022600000627     C     *LIKE         DEFINE    FVDNFV        KNFV
022700000627     C     *LIKE         DEFINE    FVDTRC        WTRC
022800951117     C     *LIKE         DEFINE    TBLKUT        KCUT
022900951117     C     *LIKE         DEFINE    TBLCOD        KCOD
023000951117     C     *LIKE         DEFINE    TBLKEY        KKEY
023100951117     C     *LIKE         DEFINE    RGVTTR        COMTTR
023200951117     C     *LIKE         DEFINE    FV2ABN        COMABN
023300951117     C     *LIKE         DEFINE    FV2EPA        WEPA
023400960312     C*****      *LIKE     DEFN FVSTFM    COMTFM
023500960209     C     *LIKE         DEFINE    FVSTRS        SAVTRS
023600960209     C     *LIKE         DEFINE    FVSABN        SAVABN
023700960221     C     *LIKE         DEFINE    FVSDFV        WDFV
023800960322     C     *LIKE         DEFINE    FVDLNP        WLNP
023900960322     C     *LIKE         DEFINE    FVDNFV        WNFV
024000930120     C*---------------------------------------------------------------*
024100941122     C** NUMERO TRASMISSIONE = UDATE USA
024200941122     C** LA DATA DEL GIORNO LLA PRENDO DA TIME
024300941122     C                   TIME                    W0140            14 0
024400941122     C                   MOVE      W0140         UDATE8            8 0
024500941122     C                   Z-ADD     UDATE8        DATA
024600941122     C                   Z-ADD     GG            MMGG              4 0
024700941122     C                   MOVEL     MM            MMGG
024800941122     C                   Z-ADD     MMGG          NUMTRA            8 0
024900941122     C                   MOVEL     AA            NUMTRA
025000960221     C                   MOVEL     W0140         WORA              6 0
025100960326     C* DETERMINO CODA PER INDIRIZZAMENTO STAMPE
025200960326     C                   MOVE      KNSIF         W004A             4
025300971117     C                   MOVEL     W004A         W001A             1
025400971117     C     W001A         IFEQ      'P'
025500960326     C                   MOVEL     'QPRINTS'     WOUTQ             7
025600960326     C                   ELSE
025700960731     C                   MOVEL     'PRTRAP1'     WOUTQ
025800960326     C                   END
025900960320     C* OVRPRTF SU FILE DI STAMPA
026000960320     C                   CLEAR                   COMMAN
026100960326     C                   MOVEA     WOUTQ         CMD(27)
026200960326     C                   MOVEA     CMD           COMMAN           80
026300960320     C                   Z-ADD     66            LUNG             15 5
026400960320     C                   CALL      'QCMDEXC'                                                A
026500960320     C                   PARM                    COMMAN
026600960320     C                   PARM                    LUNG
026700960320     C                   OPEN      SYSPRT
026800930716     C*
026900930716     C* CONTATORI
027000950120     C                   Z-ADD     0             NUMFGV            7 0
027100930716     C                   Z-ADD     0             NUMFVD            7 0
027200000428     C                   Z-ADD     0             NUMFVW            7 0
027300930716     C*
027400930120     C* APERTURA MEMBRO DA TRASCODIFICARE
027500930120     C                   EXSR      CAEXA
027600930120     C*
027700930120     C* 20 ON - CHIUDO IL PROGRAMMA
027800930120     C   20              GOTO      FINE
027900930714     C***
028000950120     C***   F N F G V
028100930714     C***
028200960130     C                   READ      FNFGV01R                               30
028300941122    1C     *IN30         DOWEQ     *OFF
028400930120     C*
028500951115     C* RICHIAMO ROUTINE DI SCRITTURA T N F V S 0 0 F
028600000628     C*
028700951108     C                   EXSR      SRFVS
028800000627     C*                    ----------
028900000628     C*
029000000628     C* RIALLINEAMENTO FILES DIPENDENTI SE HO FGS AL POSTO DI LNP
029100000628     C*
029200000628     C   75              EXSR      RIALLI
029300000628      *                    -----------
029400000628     C*
029500000428     C                   DELETE    FNFGVR
029600000428     C  N36              DELETE    FNFGW000
029700000502     C  N36              ADD       1             NUMFGW            7 0
029800121105     c
029900121105     C  N37              DELETE    FNFGs000
030000121105     C  N37              ADD       1             NUMFGs            7 0
030100000428     C*
030200950120     C                   ADD       1             NUMFGV
030300930120     C*
030400960130     C                   READ      FNFGV01R                               30
030500941122    1C                   ENDDO
030600000627     C***
030700000627     C***   F N F V D
030800000627     C***
030900000627     C                   CLEAR                   WLNP
031000000627     C                   CLEAR                   WNFV
031100000627     C*
031200000627     C                   READ      FNFVD01R                               30
031300000627    1C     *IN30         DOWEQ     *OFF
031400000627     C*
031500000627     C* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
031600000627     C*
031700000627     C                   SETOFF                                       75        ON=TROVATO PT
031800000627      * .. CERCO FOGLIO VIAGGIO ANCHE CON LINEA POSTE DI LNP
031900000627      * .. SE NON TROVATO FOGLIO VIAGGIO CON LINEA DI PARTENZA
032000000627     C                   Z-ADD     RVDLNP        KLNP
032100000627     C                   Z-ADD     RVDNFV        KNFV
032200000627     C     KFVS3         CHAIN     TNFVS01L                           31
032300000627     C     *IN31         IFEQ      *ON
032400000627     C                   EXSR      CARLPT
032500000627     C     1             DO        30            PT                3 0
032600000627     C     LPT(PT)       IFNE      0
032700000627     C                   Z-ADD     LPT(PT)       LPTLNP
032800000627     C     LPTLNP        CHAIN     AZORG01L                           74
032900000627     C     *IN74         IFEQ      *OFF
033000000627     C                   MOVEL     ORGDE3        OG143
033100000627     C     §OGPT         IFEQ      'S'
033200000627      * - - - - - - - - - - - - - - - - - - - - - - - - -
033300000627     C                   Z-ADD     LPTLNP        KLNP
033400000627     C     KFVS3         CHAIN     TNFVS01L                           31
033500000627     C  N31              SETON                                        75        TROVATO
033600000627     C  N31              LEAVE
033700000627      * - - - - - - - - - - - - - - - - - - - - - - - - -
033800000627     C                   ENDIF
033900000627     C                   ENDIF
034000000627     C                   ENDIF
034100000627     C                   ENDDO
034200000627     C                   ENDIF
034300000627     C* SE NON TROVATO RIMETTO FVDLNP
034400000627     C  N75              Z-ADD     RVDLNP        KLNP
034500000627     C*
034600000627     C*  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
034700000627     C*
034800000627     C*
034900000627     C     KFVD          CHAIN     FNFVD000                           31
035000000627     C   31              CLEAR                   FNFVD000
035100000627     C*
035200000627     C                   MOVEL     KLNP          FVDLNP
035300000627     C                   MOVEL     RVDNFV        FVDNFV
035400000627     C                   MOVEL     RVDLNA        FVDLNA
035500000627     C                   MOVEL     RVDFLP        FVDFLP
035600000627     C                   MOVEL     RVDTRC        FVDTRC
035700000627     C                   Z-ADD     RVDCLA        FVDCLA
035800000627     C                   Z-ADD     RVDKFA        FVDKFA
035900000627     C                   Z-ADD     RVDKOA        FVDKOA
036000000627     C                   MOVEL     RVDTS1        FVDTS1
036100000627     C                   MOVEL     RVDTS2        FVDTS2
036200000627     C                   MOVEL     RVDTS3        FVDTS3
036300000627     C                   MOVEL     RVDTS4        FVDTS4
036400000627     C                   MOVEL     RVDTS5        FVDTS5
036500000627     C                   MOVEL     RVDTS6        FVDTS6
036600000627     C                   Z-ADD     RVDVL1        FVDVL1
036700000627     C                   Z-ADD     RVDVL2        FVDVL2
036800000627     C                   Z-ADD     RVDVL3        FVDVL3
036900000627     C                   Z-ADD     RVDVL4        FVDVL4
037000000627     C                   Z-ADD     RVDVL5        FVDVL5
037100000627     C                   Z-ADD     RVDVL6        FVDVL6
037200000627     C                   Z-ADD     RVDSNA        FVDSNA
037300000627     C*
037400000627     C                   MOVEL     'R'           FVDFTR
037500000627     C                   MOVEL     NUMTRA        FVDDTR
037600000627     C*
037700000627     C   31              WRITE     FNFVD000
037800000627      *                    =============
037900000627     C  N31              UPDATE    FNFVD000
038000000627      *                    =============
038100000627     C*
038200000627     C                   DELETE    FNFVDR
038300000627     C                   ADD       1             NUMFVD
038400000627     C* A ROTTURA DI FOGLIO VIAGGIO RICHIAMO PGM DI AGGIORNAMENTO
038500000627     C* METRI CUBI REALI SU GRIGLIA
038600000627     C     FVDLNP        IFNE      WLNP
038700000627     C     FVDNFV        ORNE      WNFV
038800000627     C     WLNP          IFGT      *ZEROS
038900000627     C                   EXSR      BRKFVG
039000000627      *                    -----------
039100000627     C                   END
039200000627     C                   MOVE      FVDLNP        WLNP
039300000627     C                   MOVE      FVDNFV        WNFV
039400000627     C                   END
039500000627     C*
039600000627     C                   READ      FNFVD01R                               30
039700000627    1C                   ENDDO
039800000627     C*
039900000627     C     WLNP          IFGT      *ZEROS
040000000627     C                   EXSR      BRKFVG
040100000627      *                    -----------
040200000627     C                   END
040300000627      *
040400000627      * O P E R A Z I O N I   F I N A L I
040500000627      *
040600960221     C* CONTROLLO SE DEVO STAMPARE MSG DI FINE STAMPA
040700960221     C     WPRT          IFNE      *BLANKS
040800960221     C                   EXCEPT    FINES
040900960221     C                   END
041000900514     C*
041100900517     C* AGGIORNA ARCHIVIO DI LOG
041200951117     C     NUMFVD        COMP      0                                  43
041300951117     C     NUMFGV        COMP      0                                  44
041400000428     C     NUMFGW        COMP      0                                  45
041500121105     C     NUMFGs        COMP      0                                  46
041600951117     C*
041700900517     C     LEGFIL        CHAIN     CMRIC01L                           20
041800951117    1C  N20              SELECT
041900951117     C     *IN44         WHENEQ    '1'
042000951117     C                   Z-ADD     NUMFGV        RICNR1
042100951117     C     *IN43         WHENEQ    '1'
042200951117     C                   Z-ADD     NUMFVD        RICNR1
042300960130     C                   SETOFF                                       43
042400951117    1C                   ENDSL
042500960130     C*
042600951117     C  N20              MOVE      '1'           RICFL2                         *AGG.TO
042700951117     C  N20              UPDATE    CMRIC000
042800930120     C*
042900000428    1C  N20RICPRR        IFNE      *BLANKS
043000930715     C     RICPRR        CHAIN     CMRIC01L                           20
043100000428    2C  N20              SELECT
043200121105     C     *IN46         WHENEQ    '1'
043300121105     C                   Z-ADD     NUMFGs        RICNR1
043400121105     C     *IN45         WHENEQ    '1'
043500121105     C                   Z-ADD     NUMFGW        RICNR1
043600000428     C     *IN43         WHENEQ    '1'
043700000428     C                   Z-ADD     NUMFVD        RICNR1
043800000428     C                   SETOFF                                       43
043900000428    2C                   ENDSL
044000951117     C  N20              MOVE      '1'           RICFL2                          AGG.TO
044100951117     C  N20              UPDATE    CMRIC000
044200951117    1C                   ENDIF
044300121105     c
044400121105    1C  N20RICPRR        IFNE      *BLANKS
044500121105     C     RICPRR        CHAIN     CMRIC01L                           20
044600121105    2C  N20              SELECT
044700121105     C     *IN45         WHENEQ    '1'
044800121105     C                   Z-ADD     NUMFGW        RICNR1
044900121105     C     *IN43         WHENEQ    '1'
045000121105     C                   Z-ADD     NUMFVD        RICNR1
045100121105     C                   SETOFF                                       43
045200121105    2C                   ENDSL
045300121105     C  N20              MOVE      '1'           RICFL2                          AGG.TO
045400121105     C  N20              UPDATE    CMRIC000
045500121105    1C                   ENDIF
045600121105     C*
045700000428     C*
045800000428     C  N20
045900000428    1CAN 43RICPRR        IFNE      *BLANKS
046000000428     C     RICPRR        CHAIN     CMRIC01L                           20
046100000428     C  N20              Z-ADD     NUMFVD        RICNR1
046200000428     C  N20              MOVE      '1'           RICFL2                          AGG.TO
046300000428     C  N20              UPDATE    CMRIC000
046400000428    1C                   ENDIF
046500951114     C*
046600951127     C* SOTTOMISSIONE PGM DI ABBINAMENTO AUTOMATICO FV A GRIGLIA TRAINI
046700010822     C                   MOVEL     'TL45'        KCOAZ
046800960212     C                   CLEAR                   KPJBU
046900010822     C                   CALL      'TNTL45R'
047000960212     C                   PARM                    KPJBA
047100951127     C*
047200930120     C     FINE          TAG
047300060607     c
047400060607     c* Se non sono riuscita ad allocare i files, risottometto
047500060607     c                   if        *in20
047600060607     C                   CALL      'BCH10'
047700060607     C                   PARM                    KPJBA
047800060607     c                   endif
047900951114     C*
048000900514     C                   SETON                                        LR
048100951108     C**-------------------------------------------------------------**
048200951108     C**  SCRITTURA ARCHIVIO FOGLI VIAGGIO PARTENZA IN SEDE          **
048300951108     C**-------------------------------------------------------------**
048400951108     C     SRFVS         BEGSR
048500000627     C*
048600000627     C                   SETOFF                                       75        ON = USO FGS
048700000627     C*
048800000627     C     KFVS          CHAIN     FNFGW000                           36
048900000627     C  N36FGWFGS        IFGT      0
049000000627     C                   Z-ADD     FGWFGS        LPTLNP            3 0
049100000627     C                   SETON                                        75
049200000627     C                   ENDIF
049300121102     C     KFVS          CHAIN     FNFGs000                           37
049400951108     C*
049500000627     C   75KFVS2         CHAIN     TNFVS000                           31
049600000627     C  N75KFVS          CHAIN     TNFVS000                           31
049700951108     C   31              CLEAR                   TNFVS000
049800000428     C*
049900000627     C   75KFVS2         CHAIN     TNFWS000                           35
050000000627     C  N75KFVS          CHAIN     TNFWS000                           35
050100000428     C  N35              MOVEL     'S'           WESFWS            1
050200000428     C   35              MOVEL     ' '           WESFWS            1
050300121102     C*
050400121102     C   75KFVS2         CHAIN     TNFGS000                           35
050500121102     C  N75KFVS          CHAIN     TNFGS000                           35
050600121102     C  N35              MOVEL     'S'           WESFGS            1
050700121102     C   35              MOVEL     ' '           WESFGS            1
050800960221     C* SE ERA ANNULLATO DA FILIALE ED ORA E' FV VALIDO --> CLEAR
050900960221     C* TRAINO DI SEDE PER PERMETTERE L'ABBINAMENTO
051000960301     C     FVSATB        IFEQ      '*'
051100951121     C     RGVATB        ANDEQ     ' '
051200951121     C                   CLEAR                   FVSTRS
051300960312     C                   CLEAR                   FVSTFS
051400951121     C                   ENDIF
051500960221     C* MANTENGO ANNULLAMENTO DEL FV SE FATTO DALLA SEDE (ATB='A')
051600960221     C* E STAMPO MESSAGGIO
051700960221    1C     FVSATB        IFEQ      'A'
051800960221     C                   EXSR      PRTERR
051900960221   X1C                   ELSE
052000951108     C                   MOVEL     RGVATB        FVSATB
052100960221    1C                   END
052200951108     C                   MOVEL     RGVNFV        FVSNFV
052300951108     C                   Z-ADD     RGVDFV        FVSDFV
052400951108     C                   MOVEL     RGVLNP        FVSLNP
052500960123     C                   MOVEL     RGVSNP        FVSBIS
052600951108     C                   MOVEL     RGVPDR        FVSPDR
052700951108     C                   MOVEL     RGVDPD        FVSDPD
052800951108     C                   MOVEL     RGVCCN        FVSCCN
052900951108     C                   MOVEL     RGVDCN        FVSDCN
053000951108     C                   MOVEL     RGVTRM        FVSTRM
053100951108     C                   MOVEL     RGVTRR        FVSTRR
053200951108     C                   MOVEL     RGVTMZ        FVSTMZ
053300951108     C                   MOVEL     RGVTTR        FVSTTR
053400951108     C                   MOVE      RGVIPT        FVSIPT
053500960919     C*****      FGVSNS    IFEQ 'N'
053600960919     C*****                MOVE *ZEROS    FVSIES
053700960919     C*****                ELSE
053800951108     C                   MOVE      RGVIPT        FVSIES
053900960919     C*****                END
054000960123     C*****                MOVELRGVSNP    FVSSNP
054100951108     C                   MOVEL     RGVLNA        FVSLNA
054200951108     C                   MOVEL     RGVFFV        FVSFFV
054300951108     C                   MOVEL     RGVFF2        FVSFF2
054400951108     C                   MOVEL     RGVFLP        FVSFLP
054500951108     C                   MOVEL     RGVFL2        FVSFL2
054600951108     C                   Z-ADD     RGVDTP        FVSDRP
054700951108     C                   MOVE      RGVHMP        FVSHRP
054800960130     C                   MOVE      RGVRTP        FVSRTP
054900951108     C                   MOVEL     RGVNT1        FVSNT1
055000951108     C                   MOVEL     RGVNT2        FVSNT2
055100951108     C                   MOVEL     RGVPMB        FVSPMB
055200960209     C                   MOVEL     'R'           FVSFTR
055300960209     C                   MOVEL     NUMTRA        FVSDTR
055400960312     C* prima di aggiornarlo, salvo il campo TFM, perchè mi serve
055500010822     C* per il richiamo di TNTL45R
055600960312     C**********           MOVELFVSTFM    COMTFM
055700960312     C***31                MOVELRGVFRQ    FVSTFM
055800960314     C*
055900960314     C* PRIMA DI AGGIORNARE FVSTRN ------->
056000960314     C* Se f.v. già esistente determino se abbinato automaticamente
056100960314     C* o manualmente
056200960314     C                   SETOFF                                       0102
056300960314     C*
056400960314    1C     *IN31         IFEQ      *OFF
056500960314     C     FVSTRS        ANDGT     *ZEROS
056600960314    2C     FVSTRS        IFEQ      FVSTRN
056700960314    3C     FVSTRS        IFGE      9000000
056800960314     C                   SETON                                        02        ECCEZIONALE
056900960314   X3C                   ELSE
057000960314     C                   SETON                                        01        AUTOMATICO
057100960314    3C                   END
057200960314   X2C                   ELSE
057300960314     C                   SETON                                        02        MANUALE
057400960314    2C                   END
057500960314    1C                   END
057600960314     C*
057700960314     C     *IN31         IFEQ      *ON
057800960314     C     FVSTRN        ORLT      9000000
057900960312     C                   MOVEL     RGVFRQ        FVSTFM
058000960209     C                   MOVE      RGVTRN        FVSTRN
058100960312     C                   END
058200960229     C*
058300000627     C   75              Z-ADD     LPTLNP        FVSLNP
058400000627     C*
058500960209     C   31              WRITE     TNFVS000
058600000627      *                    =============
058700960209     C  N31              UPDATE    TNFVS000
058800000627      *                    =============
058900000428     C*
059000000428     C*     ESISTE FWS DI RICEZIONE
059100000428     C     *IN36         IFEQ      *OFF
059200000428     C                   MOVEL     FGWATB        FWSATB
059300000428     C                   MOVEL     FGWNFV        FWSNFV
059400000428     C                   MOVEL     FGWLNP        FWSLNP
059500000428     C                   MOVEL     FGWFGS        FWSFGS
059600000428     C                   MOVEL     FGWIPT        FWSIPT
059700000428     C                   MOVEL     FGWVIP        FWSVIP
059800000428     C                   MOVEL     FGWIES        FWSIES
059900000428     C                   MOVEL     FGWVES        FWSVES
060000000428     C                   MOVEL     FGWFF3        FWSFF3
060100000502     C                   MOVEL     FGWFF4        FWSFF4
060200000428     C                   MOVEL     FGWFL3        FWSFL3
060300000428     C                   MOVEL     FGWFL4        FWSFL4
060400000627     C   75              Z-ADD     LPTLNP        FWSLNP
060500000428     C     WESFWS        IFEQ      'S'
060600000502     C                   UPDATE    TNFWS000
060700000428     C                   ELSE
060800000502     C                   WRITE     TNFWS000
060900000428     C                   ENDIF
061000000428     C                   ENDIF
061100121102     C* NON ESISTE FGS DI RICEZIONE
061200121102     C   36WESFWS        IFEQ      'S'
061300121102     C                   DELETE    TNFwS000
061400000428     C                   ENDIF
061500000428     C*
061600121102     C*     ESISTE FGS DI RICEZIONE
061700121102     C     *IN37         IFEQ      *OFF
061800121102     C                   MOVEL     rGsATB        FgSATB
061900121102     C                   MOVEL     rGsNFV        FgSNFV
062000121102     C                   MOVEL     rGsLNP        FgSLNP
062100121102     C                   MOVEL     rGsFGS        FgSFGS
062200121102     C                   MOVEL     rGspsc        FgSpsc
062300121102     C                   MOVEL     rGsdgs        FgSdgs
062400121102     C                   MOVEL     rGshhs        FgShhs
062500121102     C                   MOVEL     rGstrns       FgStrns
062600121102     C                   MOVEL     rGslnar       FgSlnar
062700121102     C                   MOVEL     rGsdlar       FgSdlar
062800121102     C                   MOVEL     rGshlar       FgShlar
062900121102     C                   MOVEL     rGsflo        FgSflo
063000121102     C                   MOVEL     rGsnot        FgSnot
063100121102     C   75              Z-ADD     LPTLNP        FgSLNP
063200121105     C     WESFgS        IFEQ      'S'
063300121102     C                   UPDATE    TNFGS000
063400121102     C                   ELSE
063500121102     C                   WRITE     TNFGS000
063600121102     C                   ENDIF
063700121102     C                   ENDIF
063800121102     C* NON ESISTE FGS DI RICEZIONE
063900121102     C   37WESFgS        IFEQ      'S'
064000121108     C                   DELETE    TNFGS000                             99
064100121102     C                   ENDIF
064200960221     C*
064300960221     C* SE FOGLIO VIAGGIO ANNULLATO DA SEDE NON DEVO FARE ALTRO
064400960221     C* PERCHE' FVSTRS ED FVSABN SONO GIA' IMPOSTATI BENE
064500960221   1AC     FVSATB        IFNE      'A'
064600960209     C                   MOVE      FVSTRS        SAVTRS
064700960209     C                   MOVE      FVSABN        SAVABN
064800960221     C* Chaino tabella tipi traino se ttr diverso da blank
064900960221    1C     FVSTTR        IFNE      *BLANKS
065000960221     C     FVSTTR        ANDNE     COMTTR
065100960221     C                   MOVE      RGVTTR        COMTTR
065200960221     C                   MOVEL     'TV'          KCOD
065300960221     C                   MOVEL     FVSTTR        KKEY
065400960221     C                   MOVE      'S'           §TVUTC
065500960221     C     KTAB          CHAIN     TABEL00F                           30
065600960221    2C     *IN30         IFEQ      *OFF
065700960221     C                   MOVEL     TBLUNI        DSTV
065800960221    2C                   END
065900960221    1C                   END
066000951121     C*
066100951120     C* I M P O S T O   C A M P O   F V S T R S
066200951122    1C                   SELECT
066300951122     C* fvstrs sarà: = 9999999 se ttr non utilizzabile nei controlli
066400951122     C* di sede o se f.v. annullato
066500951122     C*** FOGLIO VIAGGIO ANNULLATO
066600960221     C     FVSATB        WHENNE    *BLANKS
066700960209     C                   MOVE      *ALL'9'       SAVTRS
066800951122     C*** FOGLIO VIAGGIO RICEVUTO PER LA PRIMA VOLTA
066900951122     C     *IN31         WHENEQ    *ON
067000951122     C*** FOGLIO VIAGGIO NON ABBINATO
067100951121     C     FVSTRS        OREQ      0
067200951121     C*
067300951121    2C     FVSTTR        IFNE      *BLANKS
067400951120     C     §TVUTC        ANDNE     'S'
067500960209     C                   MOVE      *ALL'9'       SAVTRS
067600951121     C* richiamo routine per flaggare date/ore arrivi reali
067700951121     C  N31              EXSR      SRFV2
067800000627      *                    ----------
067900951121    2C                   ENDIF
068000951120     C*
068100951122     C*** FOGLIO VIAGGIO NON ABBINATO (RGVTRN=0) E PRECEDENTEMENTE GIA'
068200951122     C*** RICEVUTO (*IN31=OFF) E ABBINATO IN AUTOMATICO
068300951122     C     *IN31         WHENEQ    *OFF
068400951121     C     *IN01         ANDEQ     *ON
068500951121     C     RGVTRN        ANDEQ     *ZEROS
068600951121     C                   EXSR      SBINAM
068700000627      *                    -----------
068800951121     c**
068900951122     C*** FOGLIO VIAGGIO GIA' RICEVUTO IN PRECEDENZA E ABBINATO
069000960314     C*** MANUALMENTE
069100960221     C     *IN02         WHENEQ    *ON
069200951120     C* Utilizzabile in controlli di sede
069300951120    2C     FVSTTR        IFEQ      *BLANKS
069400951120     C     §TVUTC        OREQ      'S'
069500951120     C* Se abbinato manualmente richiamo pgm di aggiornamento griglia
069600951120    3C     FVSTRS        IFNE      9999999
069700951122    3C     FVSTRS        ANDNE     9999998
069800960209     C                   CLEAR                   DSTL25
069900951120     C                   MOVE      FVSTRS        D25TRN
070000951120     C                   MOVE      FVSLNP        D25LNP
070100951120     C                   MOVEL     FVSNFV        D25NFV
070200960312     C                   MOVEL     FVSTFS        D25TFM
070300951120     C                   MOVEL     'S'           D25RIC
070400010822     C                   MOVEL     DSTL25        KPJBU
070500010822     C                   CALL      'TNTL45R'
070600951120     C                   PARM                    KPJBA
070700951120   X3C                   ELSE
070800951122     C* Se prima non era utilizzabile in controlli di sede o se non
070900951122     C* era stato abbinato perchè relativo a traino fatturato, imposto
071000951122     C* trs=0 e richiamo routine per sflaggare date reali di arrivo
071100951122    4C     FVSTRS        IFEQ      9999999
071200951122     C     FVSTRS        OREQ      9999998
071300951122     C     FVSTRN        ANDEQ     *ZEROS
071400960209     C                   MOVE      *ZEROS        SAVTRS
071500951120     C                   EXSR      SRFV2
071600000627      *                    ----------
071700951122    4C                   END
071800951120    3C                   END
071900951121     C*
072000951120   X2C                   ELSE
072100951122     C* Non utilizzabile in sede
072200951121    3C     FVSTRS        IFNE      9999999
072300951122    3C     FVSTRS        ANDNE     9999998
072400951121     C                   EXSR      SBINAM
072500000627      *                    -----------
072600951122     C                   ELSE
072700951122    4C     FVSTRS        IFEQ      9999998
072800960209     C                   MOVE      *ALL'9'       SAVTRS
072900951122    4C                   END
073000951121    3C                   ENDIF
073100951121    2C                   ENDIF
073200951122    1C                   ENDSL
073300951120     C*
073400951120     C* I  M P O S T O   C A M P O   F V S A B N
073500951109     C* Flaggo record per impedire che venga abbinato in automatico
073600951109     C* nei seguenti casi:
073700951109     C* - numero traino della filiale = 0: è necessario un abbinamento
073800951109     C*   manuale
073900960108     C* - foglio viaggio annullato o non utilizzabile in controlli di
074000960209     C*   sede (SAVTRS=9999999)
074100951122     C* - foglio viaggio già abbinato manualmente
074200951122     C     FVSTRN        IFEQ      *ZEROS
074300960209     C     SAVTRS        OREQ      9999999
074400951120     C     *IN02         OREQ      *ON
074500960209     C                   MOVE      'S'           SAVABN
074600951108     C                   ELSE
074700960209     C                   MOVE      ' '           SAVABN
074800951108     C                   END
074900951108     C*
075000951108     C*
075100960209     C     FVSTRS        IFNE      SAVTRS
075200960209     C     FVSABN        ORNE      SAVABN
075300000627     C   75KFVS2         CHAIN     TNFVS000                           31
075400000627     C  N75KFVS          CHAIN     TNFVS000                           31
075500960209     C     *IN31         IFEQ      *OFF
075600960209     C                   MOVE      SAVTRS        FVSTRS
075700960209     C                   MOVE      SAVABN        FVSABN
075800960314     C     FVSTRS        IFEQ      *ZEROS
075900960314     C                   CLEAR                   FVSTFS
076000960314     C                   END
076100960209     C                   UPDATE    TNFVS000
076200000627      *                    =============
076300960209     C                   END
076400960209     C                   END
076500960221   1AC                   END
076600951108     C                   ENDSR
076700000628     C**-------------------------------------------------------------**
076800000628     C**  ROUTINE DI RIALLINEAMENTO FILES DIPENDENTI                 **
076900000628     C**  (FV4, FV2, FVD)                                            **
077000000628     C**-------------------------------------------------------------**
077100000628     C     RIALLI        BEGSR
077200000628     C     KFVS          SETLL     FNFV401L
077300000628     C                   DO        *HIVAL
077400000628     C     KFVS          READE     FNFV401L                               74
077500000628     C   74              LEAVE
077600000628     C                   Z-ADD     LPTLNP        FV4LNP
077700000628     C                   UPDATE    FNFV4000
077800000628     C                   ENDDO
077900000628     C     KFVS          SETLL     FNFV201L
078000000628     C                   DO        *HIVAL
078100000628     C     KFVS          READE     FNFV201L                               74
078200000628     C   74              LEAVE
078300000628     C                   Z-ADD     LPTLNP        FV2LNP
078400000628     C                   UPDATE    FNFV2000
078500000628     C                   ENDDO
078600000628     C     KFVS          SETLL     FNFVD01L
078700000628     C                   DO        *HIVAL
078800000628     C     KFVS          READE     FNFVD01L                               74
078900000628     C   74              LEAVE
079000000628     C                   Z-ADD     LPTLNP        FVDLNP
079100000628     C                   UPDATE    FNFVD000
079200000628     C                   ENDDO
079300000628     C                   ENDSR
079400000627     C**-------------------------------------------------------------**
079500000627     C**  ROUTINE DI CARICO LPT PER LINEE POSTE                      **
079600000627     C**-------------------------------------------------------------**
079700000627     C     CARLPT        BEGSR
079800000627     C     RVDLNP        IFNE      SAVLNP
079900000627     C                   CLEAR                   DSUL06
080000000627     C                   MOVE      '£1'          D06COD
080100000627     C                   MOVEL     RVDLNP        D06KEY
080200000627     C                   MOVEL     DSUL06        KPJBU
080300000627     C                   CALL      'TRUL06R'
080400000627     C                   PARM                    KPJBA
080500000627     C                   MOVEL     KPJBU         DSUL06
080600000627     C                   MOVEA     LIN           LPT
080700000627     C                   Z-ADD     RVDLNP        SAVLNP            3 0
080800000627     C                   ENDIF
080900000627     C                   ENDSR
081000951121     C**-------------------------------------------------------------**
081100951121     C**  SBINAMENTO DI UN FV
081200951121     C**-------------------------------------------------------------**
081300951121     C     SBINAM        BEGSR
081400951121     C* - Sbino f.v.
081500951121     C                   CLEAR                   D29ERR
081600951121     C                   MOVE      *BLANKS       D29TLA
081700951121     C                   MOVE      FVSLNP        D29LNP
081800951121     C                   MOVE      FVSNFV        D29NFV
081900951121     C                   MOVEL     FVSDPD        D29DPD
082000951121     C                   MOVEL     'S'           D29PRT
082100951121     C                   MOVEL     DSTL29        KPJBU
082200951121     C                   CALL      'TNTL29R'
082300960209     C                   PARM                    KPJBA
082400951121     C                   MOVEL     KPJBU         DSTL29
082500951121     C*
082600951121     C* - FVSTRS sarà: = 0 se TTR UTILIZZABILE in controlli di sede
082700951121     C*                = 9999999 se TTR NON UTILIZZABILE nei controlli
082800951121     C*                  di sede
082900951121    1C     D29ERR        IFEQ      *BLANKS
083000951121     C*
083100951121    2C     FVSTTR        IFEQ      *BLANKS
083200951121     C     §TVUTC        OREQ      'S'
083300960209     C                   MOVE      *ZEROS        SAVTRS
083400960314     C**********           CLEARFVSTFM
083500951121     C                   ELSE
083600960221     C                   MOVE      *ALL'9'       SAVTRS
083700951121     C* richiamo routine per flaggare date/ore arrivi reali
083800951121     C                   EXSR      SRFV2
083900000627      *                    ----------
084000951121    2C                   END
084100951121   X1C                   ELSE
084200960108     C* Se sbinamento non riuscito perchè testata già fatturata
084300960108     C* aggiorno su griglia IPT e IES
084400951121    2C     D29ERR        IFEQ      'E'
084500951121     C     KTST          CHAIN     TNTST01L                           30
084600951121    3C     *IN30         IFEQ      *OFF
084700010822
084800010926      * controllo se devo prendere i dati dal file principale o
084900010926      * dall'estensione
085000010926      * se l'importo dell'estensione è > 0 prendo quello che trovo
085100010926      * perchè vuol dire che stanno già girando i programmi nuovi modificati per l'euro
085200010926     c                   if        fwsipt > 0
085300010926     c                   eval      w_fwsvip = fwsvip
085400010926     c                   z-add     fwsipt        w_fwsipt
085500010926     c                   endif
085600010926     c                   if        fwsies > 0
085700010926     c                   eval      w_fwsves = fwsves
085800010926     c                   z-add     fwsies        w_fwsies
085900010926     c                   endif
086000010926      * se l'importo dell'estensione è = 0 e l'importo sul file
086100010926      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
086200010926      * la divisa è lire
086300010926     c                   if        fwsipt = *zeros and fvsipt <> 0
086400010926     c                   eval      w_fwsvip = 'ITL'
086500010926     c                   z-add     fvsipt        w_fwsipt
086600010926     c                   endif
086700010926     c                   if        fwsies = *zeros and fvsies <> 0
086800010926     c                   eval      w_fwsves = 'ITL'
086900010926     c                   z-add     fvsies        w_fwsies
087000010926     c                   endif
087100010926      * se l'importo dell'estensione è = 0 e l'importo sul file
087200010926      * principale è = 0, imposto la divisa a blank per poi prendere quella della
087300010926      * griglia, così non sto a convertire un importo a 0
087400010926     c                   if        fwsipt = *zeros and fvsipt = 0
087500010926     c                   z-add     fvsipt        w_fwsipt
087600010926     c                   clear                   w_fwsvip
087700010926     c                   endif
087800010926     c                   if        fwsies = *zeros and fvsies = 0
087900010926     c                   z-add     fvsies        w_fwsies
088000010926     c                   clear                   w_fwsves
088100010926     c                   endif
088200010926      * se ho la divisa di comodo del foglio viaggio a blank devo prendere quella
088300010926      * della griglia
088400010926     c                   if        w_fwsvip = *blanks
088500010926     c                   eval      w_fwsvip = tstdiv
088600010926     c                   endif
088700010926     c                   if        w_fwsves = *blanks
088800010926     c                   eval      w_fwsves = tstdiv
088900010926     c                   endif
089000010822      * aggancio la tabella CV per stabilire i decimali della divisa della
089100010822      * griglia
089200010822     c                   clear                   dscv
089300010822     c                   movel     'CV'          kcod
089400010822     c                   movel(p)  tstdiv        kkey
089500010822     c     ktab          chain     tabel00f
089600010822     c                   if        %found(tabel00f)
089700010822     c                   movel     tbluni        dscv
089800010822     c                   endif
089900010822      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
090000010822      * devo convertire fwsipt e fwsies nella divisa della griglia
090100010926     c                   eval      w_importo = w_fwsipt
090200010926     c                   if        w_fwsvip <> tstdiv
090300010926     c                   eval      w_divisa = w_fwsvip
090400010822     c                   exsr      Sr_Converti
090500010822     c                   endif
090600010822     c                   eval      tstipt = w_importo
090700010822     C**!!!              MOVE      FVSIPT        TSTIPT
090800010926     c                   eval      w_importo = w_fwsies
090900010926     c                   if        w_fwsves <> tstdiv
091000010926     c                   eval      w_divisa = w_fwsves
091100010822     c                   exsr      Sr_Converti
091200010822     c                   endif
091300010822     c                   eval      tsties = w_importo
091400010822     C**!!!              MOVE      FVSIES        TSTIES
091500951121     C                   UPDATE    TNTST000
091600951121    3C                   END
091700951121    2C                   END
091800951121    1C                   END
091900951121     C                   ENDSR
092000010822      *----------------------------------------------------------------
092100010822      * CONVERTI - Routine conversione importo
092200010822      *----------------------------------------------------------------
092300010822     c     Sr_Converti   begsr
092400010822
092500010822     c                   clear                   yeurcods
092600010822     c                   movel     w_divisa      yecdvi
092700010822     c                   movel     tstdiv        yecdvo
092800010822     c                   z-add     w_importo     yecimi
092900010822     c                   move      §cvdec        yecdco
093000010822     c                   call      'YEURCO'
093100010822     c                   parm                    yeurcods
093200010822    5c                   if        yecesi  = ' '
093300010822     c                   z-add     yecimo        w_importo
093400010822    5c                   endif
093500010822
093600010822     c                   endsr
093700010822
093800951117     C**-------------------------------------------------------------**
093900951117     C**  IMPOSTAZIONE FLAG FV2ABN SU DATE/ORE ARRIVO REALI DEL F.V. **
094000951117     C**-------------------------------------------------------------**
094100951117     C     SRFV2         BEGSR
094200951117     C* DETERMINO SE DEVO FLAGGARE O SFLAGGARE I RECORDS DI FNFV2
094300951121     C     FVSTTR        IFEQ      *BLANKS
094400951117     C     §TVUTC        OREQ      'S'
094500951117     C                   MOVE      *BLANKS       COMABN
094600951117     C                   ELSE
094700951117     C                   MOVEL     'S'           COMABN
094800951117     C                   END
094900951117     C* AGGIORNO FLAG SU FNFV2
095000000627     C   75KFV3          SETLL     FNFV201L
095100000627     C  N75KFV2          SETLL     FNFV201L
095200000627     C   75KFV3          READE     FNFV201L                               30
095300000627     C  N75KFV2          READE     FNFV201L                               30
095400951117    2C     *IN30         DOWEQ     *OFF
095500951117     C                   MOVEL     COMABN        FV2ABN
095600951117     C                   UPDATE    FNFV2000
095700000627     C   75KFV3          READE     FNFV201L                               30
095800000627     C  N75KFV2          READE     FNFV201L                               30
095900951117    2C                   ENDDO
096000951117     C                   ENDSR
096100960221     C**-------------------------------------------------------------**
096200960221     C**  STAMPA FOGLIO VIAGGIO GIA' ANNULLATO DA SEDE               **
096300960221     C**-------------------------------------------------------------**
096400960221     C     PRTERR        BEGSR
096500960221     C* Decodifico linea partenza
096600000627     C   75LPTLNP        CHAIN     AZORG01L                           30
096700000627     C  N75RGVLNP        CHAIN     AZORG01L                           30
096800960221     C  N30              MOVEL     ORGDES        WDLNP            10
096900960221     C   30              CLEAR                   WDLNP
097000960221     C* Rovescio data foglio viaggio
097100960221     C                   CLEAR                   WLBDA8
097200960221     C                   MOVE      RGVDFV        G08INV
097300960221     C                   MOVEL     '3'           G08ERR
097400960221     C                   CALL      'XSRDA8'
097500960221     C                   PARM                    WLBDA8
097600960221     C                   MOVE      G08DAT        WDFV
097700960221     C* Stampo intestazione la prima volta o per overflow
097800960221     C     WPRT          IFEQ      *BLANKS
097900960221     C     *INOF         OREQ      *ON
098000960221     C                   EXCEPT    TESTA
098100960221     C                   MOVE      '1'           WPRT              1
098200960221     C                   END
098300960221     C                   EXCEPT    RIGA
098400960221     C                   ENDSR
098500960322     C**-------------------------------------------------------------**
098600960322     C**  RICHIAMO PGM DI AGGIORNAMENTO METRI CUBI REALI             **
098700960322     C**-------------------------------------------------------------**
098800960322     C     BRKFVG        BEGSR
098900960322     C* CHAINO ARCHIVIO FOGLI VIAGGIO PER VEDERE SE RICEVUTO
099000960322     C     KFVS1         CHAIN(N)  TNFVS01L                           30
099100960322     C     *IN30         IFEQ      *OFF
099200960322     C     FVSTRS        ANDGT     *ZEROS
099300960322     C                   CLEAR                   DSTL33
099400960322     C                   MOVEL     'L'           D33TLA
099500960322     C                   MOVE      FVSLNP        D33LNP
099600960322     C                   MOVE      FVSNFV        D33NFV
099700960322     C                   MOVEL     DSTL33        KPJBU
099800960322     C                   CALL      'TNTL33R'
099900960322     C                   PARM                    KPJBA
100000960322     C                   END
100100960322     C                   ENDSR
100200930120     C**-------------------------------------------------------------**
100300930714     C**  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                      **
100400930120     C**-------------------------------------------------------------**
100500900514     C     CAEXA         BEGSR
100600900514     C                   MOVE      ')'           VAR11            11
100700900514     C                   MOVEL     MBRFIL        VAR11
100800900514     C                   MOVE      '))'          VAR12            12
100900900514     C                   MOVEL     MBRFIL        VAR12
101000900514     C                   Z-ADD     55            LUNG             15 5
101100900514     C*
101200930120     C** ALLOCO MEMBRO
101300930120     C** SE NON RIESCE CHIUDO IL PROGRAMMA
101400060607     C**                 SETOFF                                       20
101500060607     C**                 MOVE      VAR12         CMD2                           FNFGV01R
101600060607     C**                 MOVEL     *BLANKS       COMMAN
101700060607     C**                 MOVEA     CMD2(1)       COMMAN
101800060607     C**                 CALL      'QCMDEXC'                            20
101900060607     C**                 PARM                    COMMAN
102000060607     C**                 PARM                    LUNG
102100930120     C*
102200060607     C** 20              GOTO      ENDCAE
102300000502     C*
102400000502     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE LO CREO
102500060607     C                   SETOFF                                       20
102600060607     C                   MOVE      VAR11         CMD7                            FNFGW00R
102700060607     C                   MOVEL     *BLANKS       COMMAN
102800060607     C                   MOVEA     CMD7(1)       COMMAN           80
102900060607     C                   CALL      'QCMDEXC'                            20
103000060607     C                   PARM                    COMMAN
103100060607     C                   PARM                    LUNG
103200000502     C*
103300060607     C     *IN20         IFEQ      '1'
103400000502     C                   MOVEL     'FNFGW00R'    FISICO
103500000502     C                   EXSR      CRTMBR
103600000502     C                   END
103700060607     C** 20              GOTO      ENDCAE
103800121102     c
103900121102     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE LO CREO
104000121102     C                   SETOFF                                       20
104100121102     C                   MOVE      VAR11         CMD9                            FNFGW00R
104200121102     C                   MOVEL     *BLANKS       COMMAN
104300121102     C                   MOVEA     CMD9(1)       COMMAN           80
104400121102     C                   CALL      'QCMDEXC'                            20
104500121102     C                   PARM                    COMMAN
104600121102     C                   PARM                    LUNG
104700121102     C*
104800121102     C     *IN20         IFEQ      '1'
104900121102     C                   MOVEL     'FNFGS00R'    FISICO
105000121102     C                   EXSR      CRTMBR
105100121102     C                   END
105200951122     C*
105300960322     C** ALCOBJ --> CONTROLLO SE ESISTE IL MEMBRO
105400060607     C                   MOVE      VAR11         CMD5                           FNFVD01R
105500960322     C                   MOVEL     *BLANKS       COMMAN
105600960322     C                   MOVEA     CMD5(1)       COMMAN
105700960322     C                   CALL      'QCMDEXC'                            20
105800960322     C                   PARM                    COMMAN
105900960322     C                   PARM                    LUNG
106000930716     C*
106100960322     C* SE NON ESISTE: ADDLFM
106200960322     C     *IN20         IFEQ      '1'
106300950120     C                   MOVEL     'FNFVD00R'    FISICO
106400960322     C                   MOVEL     'FNFVD01R'    LOGICO
106500930716     C*
106600930716     C                   CALL      'TRUL50C'
106700930716     C                   PARM                    FISICO           10
106800930716     C                   PARM                    MBRFIL
106900930716     C                   PARM                    LIBSYS
107000930716     C                   PARM                    LOGICO           10
107100960322     C                   PARM                    MBRFIL
107200930716     C                   PARM                    ULFLG             1
107300960322     C* ALCOBJ --> CONTROLLA NUOVAMENTE L'ESISTENZA DEL MEMBRO
107400060607     C***                SETOFF                                       20
107500060607     C***                CALL      'QCMDEXC'                            20
107600060607     C***                PARM                    COMMAN
107700060607     C***                PARM                    LUNG
107800930716     C                   ENDIF
107900930716     C*
108000960322     C     *IN20         IFEQ      *OFF
108100930716     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
108200960130     C                   MOVE      VAR11         CMD3                           FNFGV01R
108300930716     C                   MOVEL     *BLANKS       COMMAN
108400930716     C                   MOVEA     CMD3(1)       COMMAN
108500930716     C                   CALL      'QCMDEXC'                            20
108600930716     C                   PARM                    COMMAN
108700930716     C                   PARM                    LUNG
108800930715     C*
108900951115     C   20              GOTO      ENDCAE
109000000502     C*
109100000502     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
109200000502     C                   MOVE      VAR11         CMD8                           FNFGW00R
109300000502     C                   MOVEL     *BLANKS       COMMAN
109400000502     C                   MOVEA     CMD8(1)       COMMAN
109500000502     C                   CALL      'QCMDEXC'                            20
109600000502     C                   PARM                    COMMAN
109700000502     C                   PARM                    LUNG
109800000502     C*
109900000502     C   20              GOTO      ENDCAE
110000121102     c
110100121102     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
110200121102     C                   MOVE      VAR11         CMD10                          FNFGW00R
110300121102     C                   MOVEL     *BLANKS       COMMAN
110400121102     C                   MOVEA     CMD10(1)      COMMAN
110500121102     C                   CALL      'QCMDEXC'                            20
110600121102     C                   PARM                    COMMAN
110700121102     C                   PARM                    LUNG
110800121102     C*
110900121102     C   20              GOTO      ENDCAE
111000930716     C*
111100960322     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO               FNFVD01R
111200930716     C                   MOVE      VAR11         CMD6
111300900514     C                   MOVEL     *BLANKS       COMMAN
111400930716     C                   MOVEA     CMD6(1)       COMMAN
111500900514     C                   CALL      'QCMDEXC'                            20
111600900514     C                   PARM                    COMMAN
111700900514     C                   PARM                    LUNG
111800951115     C*
111900951115     C   20              GOTO      ENDCAE
112000960322     C                   END
112100951115     C*
112200960130     C                   OPEN      FNFGV01R
112300000502     C                   OPEN      FNFGW00R
112400121102     C                   OPEN      FNFGS00R
112500960322     C                   OPEN      FNFVD01R
112600060607     c                   setoff                                       20
112700930716     C     ENDCAE        ENDSR
112800000502     C**-----------------------------------------------*
112900000502     C     CRTMBR        BEGSR
113000000502     C*
113100000502     C                   MOVEL     MBRFIL        FISMBR
113200000502     C                   CALL      'TRUL50C'
113300000502     C                   PARM                    FISICO           10
113400000502     C                   PARM                    FISMBR           10
113500000502     C                   PARM                    KNSIF
113600000502     C                   PARM                    LOGICO           10
113700000502     C                   PARM                    LOGMBR           10
113800000502     C                   PARM                    ULFLG             1
113900000502     C* RITENTO L'ALLOCAZIONE
114000060607     C**                 CALL      'QCMDEXC'                            20
114100060607     C**                 PARM                    COMMAN
114200060607     C**                 PARM                    LUNG
114300060607     c                   setoff                                       20
114400000502     C*
114500000502     C                   ENDSR
114600930714     C*---------------------------------------------------------------*
114700960221     OSYSPRT    E            TESTA          1  1
114800960221     O                       PRT(1)              66
114900960221     O                       PRT(2)             132
115000960221     O                       RSUT                20
115100960221     O                       UDATE8        8    119
115200960221     O                       PAGE          Z    132
115300960221     O          E            TESTA          2
115400960221     O                       KNSIF               10
115500960221     O                       KNMUS               21
115600960221     O                       WORA               119 '  :  :  '
115700960221     O          E            RIGA           1
115800000627     O               75      LPTLNP        Z      3
115900000627     O              N75      RGVLNP        Z      3
116000960221     O                                            4 '/'
116100960221     O                       RGVNFV        Z      9
116200960221     O                       WDLNP               20
116300960221     O                                           25 'DEL'
116400960221     O                       WDFV          8     36
116500960221     O          E            FINES          1
116600960221     O                       PRT(4)              66
116700960326**         CMD2
116800960130ALCOBJ OBJ((FNFGV01R *FILE *EXCL           M234567890))
116900900514**         CMD3
117000060607OVRDBF FILE(FNFGV01R)                   MBR(M234567890)
117100930714**         CMD5
117200060607CHKOBJ OBJ(FNFVD01R) OBJTYPE(*FILE)     MBR(M         )
117300930714**         CMD6
117400960322OVRDBF FILE(FNFVD01R)                   MBR(M234567890)
117500000502**         CMD7
117600060607CHKOBJ OBJ(FNFGW00R) OBJTYPE(*FILE)     MBR(M         )
117700000502**         CMD8
117800000502OVRDBF FILE(FNFGW00R)                   MBR(M234567890)
117900121102**         CMD9
118000121102CHKOBJ OBJ(FNFGS00R) OBJTYPE(*FILE)     MBR(M         )
118100121102**         CMD10
118200121102OVRDBF FILE(FNFGS00R)                   MBR(M234567890)
118300960221**         PRT
118400960221XXXXXXXXXXXXXXXXXXXX  **  ELENCO FOGLI VIAGGIO RICEVUTI GIA' ANNUL  01
118500960221LATI DA SEDE  **               TRTR40R     XX/XX/XXXX    PAG. XXXX  02
118600960221XXX/XXXXX 1234567890  DEL XX/XX/XXXX                                03
118700960221                                         ***   FINE   STAMPA   ***  04
118800960320**         CMD
118900960326OVRPRTF FILE(SYSPRT) OUTQ(       ) SAVE(*YES) USRDTA('Ric.Variaz')
