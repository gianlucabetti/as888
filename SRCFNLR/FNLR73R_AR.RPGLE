000100070419     H DECEDIT('0,') DATEDIT(*yMd.)
000200941227      * FNLR73R *----------------------------------------------------*
000300980409      *        - RISTAMPA BOLLE E SEGNACOLLI IN ARRIVO
000400911205      *--------------------------------------------------------------*
000500970715     FAZORG01L  IF   E           K DISK
000600970715     FFNARB01L  IF   E           K DISK
000700090309     FFiART01L  IF   E           K DISK
000800941227     FFNARB27L  IF   E           K DISK
000900941227     F                                     RENAME(FNARB000:FNARB027)
001000090309     fFiart27l  if   e           k Disk    Rename(Fiart000:Fiart027)
001100070419     FFiAR401L  uF   E           K DISK
001200051013     FFIARS01L  IF   E           K DISK
001300971219     FTABEL00F  IF   E           K DISK
001400040519     FFNWlR73L  iF   E           K DISK    USROPN
001500050418     fFIAR501L  if   e           k disk    usropn
001600060615     Fdpcdp01l  IF   E           K DISK
001700060615     Fdpcsc01l  IF   E           K DISK
001800970715     FFNLV22P   O    F  132        PRINTER
001900980330     D*
002000980330     D W35             S              1    DIM(35)                              COMODO
002100000203      *  SCHIERA PER COMPORRE RMN IN STAMPA SEGNACOLLO DPD IMPORT
002200060518     D*W15             S              1    DIM(15)
002300911205      *
002400900523     D PARAM           DS
002500920422     D  VIDLNP                11     13  0
002600941227     D  DSD                   14     21  0
002700941227     D  DAAS                  14     17  0
002800941227     D  DMGS                  18     21  0
002900941227     D  DSA                   22     29  0
003000941227     D  VIDSED                30     31  0
003100941227     D  VIDNSD                32     38  0
003200941227     D  NUMD                  30     38  0
003300941227     D  VIDAAS                39     42  0
003400941227     D  VIDSEA                43     44  0
003500941227     D  VIDNSA                45     51  0
003600941227     D  NUMA                  43     51  0
003700941227     D  VIDLNA                52     54  0
003800970715     D  TIRIS                 55     55
003900971219     D  VIDSDI                56     56
004000980409     D  VIDDDT                57     57
004100030414     d  VidFls                58     60  0
004200030414     d  VidNrs                61     62  0
004300030414     d  VidNsc                63     69  0
004400030414     d  VidLnas               70     72  0
004500070419     d  wlnadstauto           73     73
004600030612     d* - parametri da TRUL90R:
004700030612     d* · pgm.   per stampa LDV
004800030612     d  wD90psl              157    166
004900030612     d* · pgm di stampa etichette segnacolli
005000030612     D  wD90pss              207    216
005100911211      *
005200920422     D                 DS
005300941227     D  ARBAAS                 1      4  0
005400941227     D  ARBMGS                 5      8  0
005500941227     D  ARBDSP                 1      8  0
005600060518     D**               DS
005700060518     D**DSRMN                  1     15
005800060518     D**DSRMN1                 4      6
005900060518     D**DSRMN2                 7     14
006000060518     D**DSRMN3                15     15
006100000203     D                 DS
006200000203     D  DPDPAR                 1     12
006300000203     D  DPD1                   1      3
006400000203     D  DPD2                   4     11
006500060518     D**DPD3                  12     12
006600000203     D                 DS
006700000203     D  DPDSTP                 1     14
006800000203     D  DPDS1                  1      3
006900000203     D  DPDV1                  4      4    INZ
007000000203     D  DPDS2                  5     12
007100000203     D  DPDV2                 13     13    INZ
007200000203     D  DPDS3                 14     14
007300000203     D* TELEFONO E FAX BARTOLINI DA STAMPARE PER EXPORT DPD
007400040524     D*TELFAX          DS
007500040524     D* DPDTEL                 1     15
007600040524     D* DPDFAX                16     30
007700941227     D*
007800000203     D* DS PER TIBS02R - GESTIONE TNTBE00F
007900000203     D DSBS02        E DS                  EXTNAME(TIBS02DS)
008000030612      * DS per pgm di STAMPA BOLLE
008100030613     d FNLSB5DS      e ds                  inz
008200030613      *
008300060518     D*DSBL4F        E DS
008400060518     D DSBL4I        E DS
008500970715     D DSBLD         E DS
008600971219     D DSBLDE        E DS
008700000201     D OG143         E DS
008800040518     D OG150         E DS
008900971219     D DS15          E DS
009000971219     D DS03          E DS
009100971219     D DS3A          E DS
009200990204     D DS3C          E DS
009300011011     D DS3I          E DS
009400940216     D KPJBA         E DS
009500031028     d dsbl4a        e ds
009600060615     d tisie2ds      e ds
009700060616     d trul28ds4     e ds
009800050418     d dAR5gen       e ds                  inz
009900060519     d w005a           s              5
010000060620     d w016a           s             16
010100060615     d wcalltisie2     s              1
010200060615     d ksrvc           s                   like(cscsrvc)
010300911206      ****************************************************************
010400911206      *  RIEPILOGO INDICATORI
010500911210      ****************************************************************
010600920422      * 01    - IMMESSO NUMERO SPEDIZIONE DAL
010700920422      * 02    - IMMESSO NUMERO SPEDIZIONE AL
010800920422      * 03    - IMMESSA DATA DAL
010900920422      * 04    - IMMESSA DATA DAL / AL
011000030414      * 05    - immessa lna
011100030414      * 06    - immesso segnacollo
011200920422      * 30    - LETTURA FILE
011300911206      *****************************************************************
011400000000     C     *ENTRY        PLIST
011500000000     C                   PARM                    KPJBA
011600911211     C                   MOVEL     KPJBU         PARAM
011700911211      *---------------------------------------------------------------*
011800911211      * CHIAVE X POSIZIONAMENTO SU FLARB35C
011900911211      *
012000911211     C     KARB          KLIST                                                  *
012100920422     C                   KFLD                    VIDAAS                         *
012200920422     C                   KFLD                    VIDLNP                         *
012300920422     C                   KFLD                    VIDSED                         *
012400920422     C                   KFLD                    VIDNSD                         *
012500920422     C     KARB2         KLIST                                                  *
012600920422     C                   KFLD                    VIDLNP                         *
012700920422     C                   KFLD                    DAAS                           *
012800920422     C                   KFLD                    DMGS                           *
012900970203     C     KAR42         KLIST
013000970203     C                   KFLD                    ARBAAS
013100970203     C                   KFLD                    ARBLNP
013200970203     C                   KFLD                    ARBNRS
013300970203     C                   KFLD                    ARBNSP
013400970203     C                   KFLD                    WTRC
013500970715     C     KBLD          KLIST                                                  *
013600970715     C                   KFLD                    ARBAAS                         *
013700970715     C                   KFLD                    ARBLNP                         *
013800970715     C                   KFLD                    ARBNRS                         *
013900970715     C                   KFLD                    ARBNSP                         *
014000971219     C     KTAB2         KLIST
014100971219     C                   KFLD                    CODUT             1 0
014200971219     C                   KFLD                    COD               2
014300971219     C                   KFLD                    KEY               8
014400971219     C                   Z-ADD     1             CODUT
014500980324     C     KARS          KLIST
014600980324     C                   KFLD                    ARBFLS
014700980324     C                   KFLD                    ARBLNA
014800980324     C                   KFLD                    ARBNRS
014900980324     C                   KFLD                    §LDNCD
015000980324     C                   KFLD                    WTRC
015100060615     C     KCDP          KLIST
015200060615     C                   KFLD                    sie2ver
015300060615     C                   KFLD                    §b4ida
015400060615     C     KCSC          KLIST
015500060615     C                   KFLD                    sie2ver
015600060615     C                   KFLD                    ksrvc
015700090309      * Fiart27L
015800030414     c     kArt27        Klist
015900030414     c                   Kfld                    VidFls
016000030414     c                   Kfld                    VidLnas
016100030414     c                   Kfld                    VidNrs
016200030414     c                   Kfld                    VidNsc
016300040519     c     kArt          Klist
016400040519     c                   Kfld                    wlrlnp
016500040519     c                   Kfld                    wlrLna
016600040519     c                   Kfld                    wlrNrs
016700040519     c                   Kfld                    wlrNsc
016800050418      * FIAR501L
016900050418     c     k05AR501      klist
017000050418     c                   kfld                    ARBaas
017100050418     c                   kfld                    ARBlnp
017200050418     c                   kfld                    ARBnrs
017300050418     c                   kfld                    ARBnsp
017400050418     c                   kfld                    AR5trd
017500050418     c                   eval      AR5trd = 'GEN'
017600970203     C* DEFINIZIONE CAMPI
017700020207     C     *LIKE         DEFINE    §OGNTW        LNANTW
017800020207     C     *LIKE         DEFINE    §OGNTW        LNPNTW
017900970203     C     *LIKE         DEFINE    AR4TRC        WTRC
018000011011     C     *LIKE         DEFINE    §3ISSA        WBART
018100000203      * AGGANCIO TABELLA "CHI" PER TEL E FAX PER EXPORT DPD
018200040524     C***                CLEAR                   DSBS02
018300040524     C***                MOVEL     'L'           T02TLA
018400040524     C***                MOVEL     'C'           T02MOD
018500040524     C***                MOVEL     KNSIF         T02SIF
018600040524     C***                MOVEL     'CHI'         T02COD
018700040524     C***                MOVEL     'TELFAX'      T02KE1
018800000203      *
018900040524     C***                CALL      'TIBS02R'
019000040524     C***                PARM                    KPJBA
019100040524     C***                PARM                    DSBS02
019200000203      *
019300040524     C***  T02ERR        IFEQ      *BLANKS
019400040524     C***                MOVEL     T02UNI        TELFAX
019500040524     C***                ELSE
019600040524     C***                MOVEL     *ALL'?'       TELFAX
019700040524     C***                ENDIF
019800011011     C* SE DEVO RISTAMPARE I SEGNACOLLI VADO SU TABELLA 3I PER VEDERE
019900040518     C* SE posso ristampare L'ETICHETTA BARTOLINI
020000030506     C**!!!TIRIS         IFEQ      'S'
020100030506     C     TIRIS         IFEQ      'D'
020200011011     C                   MOVEL     '3I'          COD
020300011011     C                   MOVEL(P)  '1'           KEY
020400011011     C     KTAB2         CHAIN     TABEL                              31
020500011011     C  N31              MOVEL     TBLUNI        DS3I
020600011011     C   31              CLEAR                   DS3I
020700011011     C                   MOVE      §3ISSA        WBART
020800011011     C                   ENDIF
020900040519     c* Se devo elaborare il file di work lo apro
021000040519     c     tiris         ifeq      'F'
021100040519     c                   open      fnwlr73l
021200040519     c                   endif
021300911206      *---------------------------------------------------------------*
021400920422     C*
021500920422     C* NUMERO DAL IMMESSO
021600920422     C     NUMD          IFGT      0
021700920422     C                   SETON                                        01
021800920422     C                   END
021900920422     C* NUMERO AL  IMMESSO
022000920422     C     NUMA          IFGT      0
022100920422     C                   SETON                                        02
022200920422     C                   END
022300920422     C* DATA DAL
022400920422     C     DSD           IFGT      0
022500920422     C                   SETON                                        03
022600920422     C                   END
022700920422     C* DATA AL
022800920422     C     DSA           IFGT      0
022900920422     C                   SETON                                        04
023000920422     C                   END
023100920422     C* IMMESSA LINEA DI ARRIVO
023200920422     C     VIDLNA        IFGT      0
023300920422     C                   SETON                                        05
023400920422     C                   END
023500030414      * Immesso il segnacollo
023600030414     c                   If        VidNsc > *Zeros
023700030414     c                   Eval      *In06 = *On
023800030414     c                   EndIf
023900920422     C*
024000920422     C* N U M E R O   S P E D I Z I O N E
024100920422     C*  SOLO DAL
024200970715     C   01
024300970715     CANN02TIRIS         IFEQ      'B'
024400980407     C* NON CONTROLLO SE LA BOLLA E' GIA' STATA STAMPATA PERCHE' LO
024500980407     C*  FACCIO NEL FILTRO
024600030613     c                   clear                   FNLSB5DS
024700941227     C                   MOVEL     VIDAAS        DB0AAS
024800941227     C                   MOVEL     VIDLNP        DB0LNP
024900941227     C                   MOVEL     VIDSED        DB0NRS
025000941227     C                   MOVEL     VIDNSD        DB0NSP
025100941227     C                   MOVEL     'A'           DB0TBO
025200941227     C                   MOVEL     'R'           DB0RIS
025300030612     c                   call      wD90psl
025400030613     c                   parm                    FNLSB5DS
025500070419      * aggiorno il flag ddt se mai stampata
025600070419     c                   if        wlnadstauto = *on
025700070419     c                   clear                   dsbl4a
025800070419     c                   movel     vidaas        arbaas
025900070419     c                   movel     vidlnp        arblnp
026000070419     c                   movel     vidsed        arbnrs
026100070419     c                   movel     vidnsd        arbnsp
026200070419     c                   eval      wtrc = 'A'
026300070419     c     kar42         chain     fiar401l                           31
026400070419     c  n31              eval      dsbl4a = ar4not
026500070419     c                   if        §b4abm = 'N'
026600070419     c                   eval      §b4abm = 'J'
026700070419     c                   z-add     *date         §b4asl
026800070419     c  n31              eval      ar4not = dsbl4a
026900070419     c  n31              update    fiar4000
027000070419     c                   endif
027100070419     c                   endif
027200970715     C                   ELSE
027300970715     C*
027400970715     C     KARB          CHAIN     FNARB000                           30
027500971219     C* SE BOLLA IMPORT O EXPORT LEGGO ESTENSIONE BOLLA TRC='E'
027600971219     C                   EXSR      LEGAR4
027700030414      * Non lo faccio più xchè se è una spedizione singola ho già controllato la
027800030414      * linea di arrivo nel filtro
027900030414     C**!!!WESTER        IFEQ      'S'
028000030414     C**!!!TIRIS         ORNE      'D'
028100030414     C**!!!WBART         ANDEQ     'S'
028200980324     C     ARBNCD        IFGT      0
028300970715     C                   EXSR      STASCI
028400970715     C                   ELSE
028500970715     C     ARBFNS        IFEQ      'S'
028600970715     C                   EXSR      STASCN
028700920422     C                   END
028800970715     C                   END
028900030414     C**!!!              END
029000970715     C                   END
029100920422     C**
029200920422     C*       DAL / AL
029300970715     C   01
029400970715    1CAN 02NUMD          DOWLE     NUMA
029500990318     C     NUMD          ANDGT     0
029600941227     C     KARB          CHAIN     FNARB000                           30
029700920422     C*
029800970715     C   05
029900970715    2CANN30VIDLNA        IFNE      ARBLNA
030000920422     C                   SETON                                        30
030100970715    2C                   END
030200970203     C*
030300970203     C* BOLLA ESISTENTE: CONTROLLO CHE SIA GIA' STATA STAMPATA
030400060216     C*   ALTRIMENTI NON LA STAMPO (SE NON TROVO IL RECORD IN FiAR4
030500970203     C*   CONSIDERO LA BOLLA COME SE FOSSE GIA' STATA STAMPATA)
030600980409     C*   CONTROLLO ANCHE LA PARZIALIZZAZIONE SI NO DDT
030700070419      *   se bolla mai stampata non la stampo se non c'è la procedura attiva
030800070419      *   della distinte automatiche...altrimenti la posso stampare
030900970715    2C     *IN30         IFEQ      *OFF
031000990204    3C     TIRIS         IFEQ      'B'
031100980409     C                   EXSR      CTRDDT
031200990204     C* CONTROLLO SE E' RISTAMPABILE LA BOLLA
031300000203     C  N28              EXSR      CTRRIS
031400000203     C*
031500000203    4C     *IN28         IFEQ      *OFF
031600000203     C     ARBLNA        CHAIN     AZORG01L                           31
031700000203     C* ESCLUDO LE BOLLE EXPORT DPD E I NO DDT EUROEXPRESS
031800000203     C                   MOVEL     ORGDE3        OG143
031900020207    5C     §OGNTW        IFEQ      'DPD'
032000000203     C                   SETON                                        28
032100000203    5C                   ENDIF
032200000203     C*
032300020207    5C     §OGNTW        IFEQ      'EEX'
032400000203    6C     WBAM          IFNE      'S'
032500000203     C     WBAM          ANDNE     'C'
032600031028     c     wbam          andne     'P'
032700031028     c     wbam          andne     'Q'
032800000203     C                   SETON                                        28
032900000203    6C                   ENDIF
033000000203    5C                   ENDIF
033100000203    4C                   ENDIF
033200970715     C*  BOLLE
033300990204    4C     *IN28         IFEQ      *OFF
033400030613     c                   clear                   FNLSB5DS
033500941227     C                   MOVEL     VIDAAS        DB0AAS
033600941227     C                   MOVEL     VIDLNP        DB0LNP
033700941227     C                   MOVEL     VIDSED        DB0NRS
033800941227     C                   MOVEL     VIDNSD        DB0NSP
033900941227     C                   MOVEL     'A'           DB0TBO
034000941227     C                   MOVEL     'R'           DB0RIS
034100030612     c                   call      wD90psl
034200030613     c                   parm                    FNLSB5DS
034300990204    4C                   ENDIF
034400990204    3C                   ENDIF
034500970715     C** SEGNACOLLI
034600990204    3C     TIRIS         IFEQ      'S'
034700030414     C**!!!TIRIS         OREQ      'D'
034800971219     C* SE BOLLA IMPORT O EXPORT LEGGO ESTENSIONE BOLLA TRC='E'
034900971219     C                   EXSR      LEGAR4
035000971219     C*
035100030414     C**!!!              EXSR      CHKEST
035200040527If  4c****               If        LnaNtw = 'DPD' or LnaNtw = 'FED' or
035300040527     c****                         LnaNtw = 'EEX'
035400040527If  4c                   If        LnaNtw = 'DPD'
035500030414    4C**!!!WESTER        IFEQ      'S'
035600030414     C**!!!TIRIS         ORNE      'D'
035700030414     C**!!!WBART         ANDEQ     'S'
035800990204    5C     ARBNCD        IFGT      0
035900970715     C                   EXSR      STASCI
036000970715     C                   ELSE
036100990204    6C     ARBFNS        IFEQ      'S'
036200970715     C                   EXSR      STASCN
036300990204    6C                   END
036400990204    5C                   END
036500990204    4C                   END
036600971219    3C                   END
036700970715    2C                   ENDIF
036800920422     C*
036900920422     C                   ADD       1             NUMD
037000970715    1C                   ENDDO
037100970715     C*
037200920422     C*
037300920422     C* D A T A   S P E D I Z I O N E
037400920422     C*
037500941227     C   03KARB2         SETLL     FNARB27L
037600941227     C   03
037700941227     CANN04KARB2         READE     FNARB27L                               30
037800941227     C   03
037900941227     CAN 04VIDLNP        READE     FNARB27L                               30
038000920422     C*
038100990204    1C   03*IN30         DOWEQ     *OFF
038200920422     C* SE DAL / AL CONTROLLO SE DATA MAGGIORE
038300990204    2C   04ARBDSP        IFGT      DSA
038400920422     C                   SETON                                        30
038500990204    2C                   END
038600920422     C                   SETOFF                                       29
038700920422     C* SE NON E' LA LINEA DI ARRIVO RICHIESTA NON STAMPO
038800990204     C   05
038900990204    2CANN30VIDLNA        IFNE      ARBLNA
039000920422     C                   SETON                                        29
039100990204    2C                   END
039200920422     C*
039300970203     C* BOLLA ESISTENTE: CONTROLLO CHE SIA GIA' STATA STAMPATA
039400060216     C*   ALTRIMENTI NON LA STAMPO (SE NON TROVO IL RECORD IN FiAR4
039500970203     C*   CONSIDERO LA BOLLA COME SE FOSSE GIA' STATA STAMPATA)
039600980409     C*   CONTROLLO ANCHE SE RICHIESTO SI NO DDT SE E' SECONDO LE RICH
039700990204    2C     *IN30         IFEQ      *OFF
039800970203     C     *IN29         ANDEQ     *OFF
039900990204     C* RISTAMPA BOLLE: CONTROLLO IL DDT
040000990204    3C     TIRIS         IFEQ      'B'
040100990204     C                   EXSR      CTRDDT
040200990204     C*                 CONTROLLO SE BOLLA RISTAMPABILE
040300990204     C  N28              EXSR      CTRRIS
040400000201     C*
040500000201    4C     *IN28         IFEQ      *OFF
040600000201     C     ARBLNA        CHAIN     AZORG01L                           31
040700000201     C* ESCLUDO LE BOLLE EXPORT DPD E I NO DDT EUROEXPRESS
040800000201     C                   MOVEL     ORGDE3        OG143
040900020207    5C     §OGNTW        IFEQ      'DPD'
041000000201     C                   SETON                                        28
041100000201    5C                   ENDIF
041200000201     C*
041300020207    5C***        ORGFL1    IFEQ 'E'
041400020207    5C     §OGNTW        IFEQ      'EEX'
041500000201    6C     WBAM          IFNE      'S'
041600000201     C     WBAM          ANDNE     'C'
041700031028     c     wbam          andne     'P'
041800031028     c     wbam          andne     'Q'
041900000201     C                   SETON                                        28
042000000201    6C                   ENDIF
042100000201    5C                   ENDIF
042200000201    4C                   ENDIF
042300970203     C*
042400990204    4C     *IN28         IFEQ      *OFF
042500030613     c                   clear                   FNLSB5DS
042600941227     C                   MOVEL     ARBAAS        DB0AAS
042700941227     C                   MOVEL     ARBLNP        DB0LNP
042800941227     C                   MOVEL     ARBNRS        DB0NRS
042900941227     C                   MOVEL     ARBNSP        DB0NSP
043000941227     C                   MOVEL     'A'           DB0TBO
043100941227     C                   MOVEL     'R'           DB0RIS
043200030612     c                   call      wD90psl
043300030613     c                   parm                    FNLSB5DS
043400030612    4C                   ENDIF
043500030612    3C                   ENDIF
043600970715     C*
043700990204    3C     TIRIS         IFEQ      'S'
043800030414     C**!!!TIRIS         OREQ      'D'
043900971219     C* SE BOLLA IMPORT O EXPORT LEGGO ESTENSIONE BOLLA TRC='E'
044000971219     C                   EXSR      LEGAR4
044100971219     C*
044200030414     C**!!!              EXSR      CHKEST
044300040527If  4c****               If        LnaNtw = 'DPD' or LnaNtw = 'FED' or
044400040527     c****                         LnaNtw = 'EEX'
044500040527If  4c                   If        LnaNtw = 'DPD'
044600030414    4C**!!!WESTER        IFEQ      'S'
044700030414     C**!!!TIRIS         ORNE      'D'
044800030414     C**!!!WBART         ANDEQ     'S'
044900990204    5C     ARBNCD        IFGT      0
045000970715     C                   EXSR      STASCI
045100970715     C                   ELSE
045200990204    6C     ARBFNS        IFEQ      'S'
045300970715     C                   EXSR      STASCN
045400990204    6C                   END
045500990204    5C                   END
045600990204    4C                   END
045700990204    3C                   ENDIF
045800990204    2C                   ENDIF
045900920422     C*
046000941227     C  N04KARB2         READE     FNARB27L                               30
046100941227     C   04
046200941227     CANN30VIDLNP        READE     FNARB27L                               30
046300990204    1C                   ENDDO
046400030414
046500030414
046600030414      * S E G N A C O L L O
046700030414
046800030414If  1c                   If        *In06
046900030414     c                   Clear                   Og143
047000030414     c     VidLnas       Chain     Azorg01l
047100030414If  2c                   If        %Found(Azorg01l)
047200030414     c                   Movel     Orgde3        Og143
047300040518     c                   Movel     Orgdf0        Og150
047400030414    2c                   Endif
047500030414      * se lna non è estera e richiesa la stampa con F8
047600040527If  2c***                If        §OgNtw <> 'DPD' and §OgNtw <> 'FED' and
047700040527     c***                          §OgNtw <> 'EEX' and
047800040527If  2c                   If        §OgNtw <> 'DPD' and
047900030414     c                             Tiris= 'D' and
048000030414      * deve essere abilitata
048100030414     c                             wbart <> 'S'
048200030414     c                   GoTo      nostp
048300030414    2c                   Endif
048400090309     c     kArt27        Chain     Fiart27l
048500090309If  2c                   If        %Found(Fiart27l)
048600030414     c                   Move      ArtAas        VidAas
048700030414     c                   Move      ArtLnp        VidLnp
048800030414     c                   Move      ArtNrs        VidSed
048900030414     c                   Move      ArtNsp        VidNsd
049000030414    2c                   EndIf
049100030414     c     kArb          Chain     Fnarb01l
049200030414If  2c                   If        %Found(Fnarb01l)
049300030414     c                   ExSr      LegAr4
049400030414     c                   ExSr      StaNsc
049500030414    2c                   EndIf
049600030414     c     nostp         tag
049700030414    1c                   EndIf
049800040519
049900040519      * F N W L R 7 3 L
050000040519
050100040519     c     tiris         ifeq      'F'
050200040519     c     *loval        setll     fnwlr73l
050300040519     c                   do        *hival
050400040519     c                   read      fnwlr73l
050500040519      * Fine File esco dal ciclo
050600040519     c                   If        %Eof(Fnwlr73l)
050700040519     c                   Leave
050800040519     c                   EndIf
050900090309     c     kArt          Chain     Fiart27l
051000090309If  2c                   If        %Found(Fiart27l)
051100040519     c                   Move      ArtAas        VidAas
051200040519     c                   Move      ArtLnp        VidLnp
051300040519     c                   Move      ArtNrs        VidSed
051400040519     c                   Move      ArtNsp        VidNsd
051500040519    2c                   EndIf
051600040519     c     kArb          Chain     Fnarb01l
051700040519If  2c                   If        %Found(Fnarb01l)
051800040519     c                   ExSr      LegAr4
051900040519     c                   ExSr      StaNsc
052000040519    2c                   EndIf
052100040519     c                   enddo
052200040519     c                   endif
052300911206      *
052400040519     c     tiris         ifeq      'F'
052500040519     c                   close     fnwlr73l
052600040519     c                   endif
052700040519     c*
052800060615     c                   if        wcalltisie2='S'
052900060615     c                   clear                   tisie2ds
053000060615     c                   eval      sie2tla='C'
053100060615     c                   call      'TISIE2R'
053200060615     c                   parm                    tisie2ds
053300060615     c                   endif
053400060615     c
053500911206     C                   SETON                                        LR
053600970715     C**
053700970715     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI SEQUENZIALI ---------*
053800970715     C     STASCI        BEGSR
053900970715     C                   Z-ADD     ARBNCD        COMSCD            7 0
054000970715     C     ARBNCA        IFEQ      0
054100970715     C                   Z-ADD     ARBNCD        ARBNCA
054200970715     C                   END
054300970715     C* IMPOSTO CAMPI DI STAMPA
054400970715     C                   EXSR      IMPO
054500970715     C*
054600970715     C     COMSCD        DOWLE     ARBNCA
054700990318     C     COMSCD        ANDGT     0
054800970715     C                   ADD       1             §LDNC1
054900970715     C* S T A M P A
055000970715     C                   MOVEL     COMSCD        §LDNCD
055100020218     c                   select
055200020218      * per segnacolli FedEx imposta §LDCDP
055300020218     c                   when      lnantw = 'FED'
055400020218     c                   exsr      sr_fedex
055500020218      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
055600040507     c****               when      lnpntw = 'EEX' or lnantw = 'EEX'
055700040507     c                   when      lnpntw = 'EEX'
055800980324     C                   EXSR      EUREXP
055900060615     c                   when      (lnpntw = 'DPD'  or lnantw='DPD')
056000060615     c                             and §b4ibi<>' ' and §DETST='EST'
056100060615     C                   movel(P)  dpdfax        §LDCDP
056200020218     c                   endsl
056300970715     C*
056400030612     C                   CALL      wD90pss
056500970715     C                   PARM                    DSBLD
056600971219     C                   PARM                    DSBLDE
056700970715     C*
056800970715     C                   ADD       1             COMSCD
056900970715     C*
057000970715     C                   END
057100970715     C*
057200970715     C                   ENDSR
057300970715     C**
057400970715     C* IMPOSTO CAMPI PER STAMPA IN IMMEDIATO ------------------------*
057500970715     C     IMPO          BEGSR
057600050418      *
057700050418      * Verifica se spedizione "di valore"
057800050418     c                   exsr      CtrSpeVal
057900970715     C*
058000970715     C                   Z-ADD     0             §LDNC1
058100970715     C                   MOVEL     ARBFLS        §LDLNP
058200970715     C     ARBFLS        CHAIN     AZORG                              30
058300970715     C  N30              MOVEL     ORGDES        §LDDLP
058400970715     C   30              MOVEL     *BLANKS       §LDDLP
058500970715     C                   MOVEL     ARBNRS        §LDNRS
058600970715     C                   MOVEL     ARBLNA        §LDLNA
058700970715     C     ARBLNA        CHAIN     AZORG                              30
058800970715     C  N30ORGDE5        IFNE      *BLANKS
058900970715     C                   MOVEL     ORGDE5        §LDDLA
059000970715     C                   ELSE
059100970715     C                   MOVEL     ORGDES        §LDDLA
059200970715     C                   END
059300970715     C   30              MOVEL     *BLANKS       §LDDLA
059400970715     C                   MOVEL     ARBTFA        §LDTNA
059500970715     C                   MOVEL     ARBTSP        §LDTSP
059600970715     C                   MOVEL     ARBZNC        §LDZSC
059700970715     C                   MOVE      ARBNCL        §LDNC2
059800970715     C                   MOVEL     ARBRSM        §LDRSM
059900970715     C                   MOVEL     ARBPRM        §LDPRM
060000970715     C                   MOVEL     ARBRSD        §LDRSD
060100970715     C                   MOVEL     ARBCAD        §LDCAD
060200970715     C                   MOVEL     ARBLOD        §LDLOD
060300970715     C                   MOVEL     ARBIND        §LDIND
060400970715     C                   MOVEL     ARBPRD        §LDPRD
060500970715     C                   MOVEL     *ZEROS        §LDNCP
060600970715     C                   MOVEL     *ZEROS        §LDNCU
060700050418if  1c                   if        §AR5bva = 'V'
060800050418     c                   movel     'X'           §LDrst
060900050418x   1c                   else
061000970715     C                   MOVEL     'R'           §LDRST
061100050418e   1c                   endif
061200970715     C                   Z-ADD     ARBPKF        §LDPKG
061300970715     C                   Z-ADD     ARBVLF        §LDVOL
061400970715     C                   MOVEL     ARBFVF        §LDFVR
061500970715     C                   MOVEL     ARBAAS        §LDAAS
061600970715     C                   MOVEL     ARBMGS        §LDMGS
061700971219     C*
061800060615      * La stampa ALL non è più prevista: o solo etichetta ITA-bartolini
061900060615      * stampo sia parte bartolini che paro solo etichetta EST-estera
062000040518If  1c                   If        tiris = 'S'
062100060615     c*****              If        §ogsea <> 'S'
062200060615     c*****              Eval      §Detst = 'ALL'
062300060615     c*****              else
062400040518     c                   Eval      §Detst = 'EST'
062500060615     c*****              endif
062600030414     c                   ExSr      EtiEst
062700030414    1c                   EndIf
062800060615     c
062900030414If  1c                   If        tiris = 'D'
063000060615If  2c***                If        (LnaNtw = 'DPD' or LnaNtw = 'FED' or
063100060615     c***                          LnaNtw = 'EEX') and §ogsea <> 'S'
063200060615     c***                Eval      §Detst = 'ALL'
063300060615     c***                ExSr      EtiEst
063400060615   x2c***                Else
063500030414     c                   Eval      §Detst = 'ITA'
063600060615    2c***                EndIf
063700030414    1c                   EndIf
063800040519     c* Se sto stampando i segnacolli in arrivo in base al file di work
063900040519     c* devo stampare solo l'etichetta estera
064000040519     c                   if        tiris='F'
064100040519     c                   Eval      §Detst = 'EST'
064200040519     c                   exsr      etiest
064300040519     c                   endif
064400971219     C*
064500970715     C                   ENDSR
064600970715     C**
064700970715     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI NON SEQUENZIALI -----*
064800970715     C     STASCN        BEGSR
064900970715     C*
065000970715     C                   EXSR      IMPO
065100970715     C*
065200090309     C     KBLD          SETLL     FiART01L
065300090309     C     KBLD          READE     FiART01L                               33
065400970715     C*
065500970715     C     *IN33         DOWEQ     *OFF
065600970715     C*
065700970715     C                   Z-ADD     ARTNSC        COMSCD
065800970715     C                   ADD       1             §LDNC1
065900970715     C* S T A M P A
066000970715     C                   MOVEL     COMSCD        §LDNCD
066100020218     c                   select
066200020218      * per segnacolli FedEx imposta §LDCDP
066300020218     c                   when      lnantw = 'FED'
066400020218     c                   exsr      sr_fedex
066500020218      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
066600040507     c***                when      lnpntw = 'EEX' or lnantw = 'EEX'
066700040507     c                   when      lnpntw = 'EEX'
066800980324     C                   EXSR      EUREXP
066900020218     c                   endsl
067000970715     C*
067100030612     C                   CALL      wD90pss
067200970715     C                   PARM                    DSBLD
067300971219     C                   PARM                    DSBLDE
067400970715     C*
067500090309     C  N33KBLD          READE     FiART01L                               33
067600970715     C                   ENDDO
067700970715     C*
067800970715     C                   ENDSR
067900030414      **
068000030414      *--- STAMPA SEGNACOLLO -----------------------------------------*
068100030414     c     StaNsc        BegSr
068200030414
068300030414     c                   ExSr      Impo
068400030414
068500030414     c                   Add       1             §LDNC1
068600030414     c                   Movel     ArtNsc        §LDNCD
068700030414
068800030414     c                   select
068900030414      * per segnacolli FedEx imposta §LDCDP
069000030414     c                   when      lnantw = 'FED'
069100030414     c                   exsr      sr_fedex
069200030414      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
069300040507     c***                when      lnpntw = 'EEX' or lnantw = 'EEX'
069400040507     c                   when      lnpntw = 'EEX'
069500030414     c                   ExSr      EurExp
069600030414     c                   endsl
069700030414
069800030612     c                   CALL      wD90pss
069900030414     c                   PARM                    DSBLD
070000030414     c                   PARM                    DSBLDE
070100030414
070200030414     c                   EndSr
070300030414
070400971219     C*--- REPERISCO ESTENZIONE BOLLA TIPO RECORD 'E' ----------------*
070500971219     C     LEGAR4        BEGSR
070600000203     C                   CLEAR                   DSBLDE
070700000203     C                   CLEAR                   DSBLD
070800060616     C*
070900060616     C* CHAIN SULLE NAZIONI
071000060616     C                   MOVEL     '15'          COD
071100060616     C                   MOVEL(P)  ARBNZD        KEY
071200060616     C     KTAB2         CHAIN     TABEL                              31
071300060616     C  N31TBLFLG        IFNE      ' '
071400060616     C                   SETON                                        31
071500060616     C                   END
071600060616     C**
071700060616     C  N31              MOVEL     TBLUNI        DS15
071800060616     C   31              CLEAR                   DS15
071900060616     c* Era già qu fuori dal ciclo DPD. forse serve ance per altre
072000060616     c*  etichette estere? ad ogni modo lo lasci qui. non serve
072100060616     c*  per DP3 ma solo per DP4
072200060616     C                   MOVEL     §15COD        §DEISA
072300971219     C*
072400971219     C                   MOVEL     ARBRMA        §LDCDP
072500971219     C*
072600971219     C     ARBLNA        IFNE      SAVLNA
072700971219     C     ARBLNA        CHAIN     AZORG01L                           31
072800020207     C                   MOVE      ORGDE3        OG143
072900040521     C                   MOVEl     ORGDf0        OG150
073000020207     C                   MOVE      §OGNTW        LNANTW
073100030414     C**!!!              MOVEL     §OGDPD        LNADPD            1
073200040524     c                   clear                   dpdtel           15
073300040524     c                   clear                   dpdfax           15
073400040524     c                   movel     orgtel        dpdtel
073500040524     c                   movel     orgfax        dpdfax
073600971219     C                   Z-ADD     ARBLNA        SAVLNA            3 0
073700971219     C                   END
073800971219     C*
073900971219     C     ARBLNP        CHAIN     AZORG01L                           31
074000020207     C                   MOVE      ORGDE3        OG143
074100020207     C                   MOVE      §OGNTW        LNPNTW
074200971219     C*
074300000207     C**
074400020218     C     lnaNTW        IFEQ      'EEX'
074600040507     C                   MOVEL     'EEX'         §DETSE
074700000207     C                   ENDIF
074800000207     C*
074900000203      * SE LNP DPD SCRIVO NUMERICO
075000060518     C***  LNPNTW        IFEQ      'DPD'
075100060518     C***                MOVEL     ARBRMN        DSRMN
075200060518     C***                CLEAR                   W15
075300060518     C***                MOVEA     DSRMN1        W15(1)
075400060518     C***                MOVEA     DSRMN2        W15(5)
075500060518     C***                MOVEA     DSRMN3        W15(14)
075600060518     C***                MOVEA     W15           §LDCDP
075700060518     C***                ENDIF
075800000203      **
075900060216      **  CHAIN Fiar4 PER EXPORT DPD
076000020207    1C     LNANTW        IFEQ      'DPD'
076100060518    1C     LNpNTW        oreQ      'DPD'
076200060615     c
076300060518     C                   MOVEL     'I'           WTRC
076400070419     C     KAR42         CHAIN(n)  FiAR401L                           39
076500060518     C  N39              MOVEL     AR4NOT        DSBL4I
076600060518     C   39              CLEAR                   DSBL4I
076700060615     c
076800060518    2C     §B4ipn        IFNE      *BLANKS
076900060615     c* per DP3 o DP4 ma parcel lungo 11
077000060615     c                   if        %subst(§b4ipn:12:1)=' '
077100060518     C                   MOVEL     §B4ipn        DPDPAR
077200000203     C                   MOVEL     DPD1          DPDS1
077300000203     C                   MOVEL     DPD2          DPDS2
077400060518     C                   MOVEL     §b4icd        DPDS3
077500000203     C                   MOVEL(P)  DPDSTP        §LDCDP
077600060615     c                   else
077700060615     c                   movel(P)  §b4ipn        §ldcdp
077800060615     c                   endif
077900060615     c
078000060518    2c                   endif
078100060518    1c                   endif
078200060615     c
078300011011     C* DEVO SEMPRE RISTAMPARE ANCHE IL CHI SONO
078400040518     c*
078500060518    1C     LNANTW        IFEQ      'DPD'
078600060518     C     §B4ipn        andne     *BLANKS
078700060615     c
078800060615     c***  §ogsea        ifne      'S'
078900060615     C***                MOVEL     'DP2'         §DETSE
079000060615     c***                else
079100040518     c     tiris         ifeq      'S'
079200040519     c     tiris         oreq      'F'
079300060616     c* DP3   - cappario dpd OLD
079400081008     c***                if        §b4ibi=' '
079500081008     c***                movel     'DP3'         §detse
079600081008     c***                else
079700060616     c* DP4   - cappario dpd NEW
079800060615     c                   movel     'DP4'         §detse
079900081008     c***                endif
080000060615     c
080100040518     c                   endif
080200060615     c****               endif
080300060615     c
080400000203     C     ARBPKF        IFLE      3
080500000203     C                   MOVEL     'X'           §DESSE
080600000203     C                   ENDIF
080700000203     C*
080800060615     c* reperisco versione cappario DPD
080900060615     c                   clear                   tisie2ds
081000060615     c                   z-add     arbmgs        sie2dri
081100060615     c                   movel     arbaas        sie2dri
081200060615     c                   call      'TISIE2R'
081300060615     c                   parm                    TIsie2ds
081400060615     c                   eval      wcalltisie2='S'
081500060615     c
081600060615     c* chain su file dpcdp01l per prendere nazione depot
081700060615     c     kcdp          chain     dpcdp01l
081800060615     c                   if        %found(dpcdp01l)
081900060615     c                   movel     cdpiso2       §DESIA
082000060615     c                   endif
082100061010     c*
082200061010     c                   eval      §deawb= §b4iat + §b4ipy
082300060615     c
082400060615     c                   eval      %subst(§deeur:1:1) = §b4ibi
082500060615     c* richiamo pgm per avere il cap da stampare come vuole dpd
082600060615     c                   clear                   outcapdpd
082700060615     c                   call      'XCAPDPD'
082800060615     c                   parm                    arbcad
082900060615     c                   parm                    outcapdpd         7
083000061013     c                   parm      arbnzd        inpnzd            3
083100060615     c
083200060615     c                   eval      %subst(§deeur:2:7) = %subst(outcapdpd:1:7)
083300060615     c* Se il numero parcel è lungo 11, devo portarlo a 14
083400060615     c                   if        %subst(§b4ipn:12:1)=' '
083500060615     c                   eval      %subst(§deeur:9:1) = '0'
083600060615     c                   eval      %subst(§deeur:10:3)=%subst(§b4ipn:1:3)
083700060615     c                   eval      %subst(§deeur:13:2)='99'
083800060615     c                   eval      %subst(§deeur:15:8)=%subst(§b4ipn:4:8)
083900060615     c                   else
084000060615     c                   eval      %subst(§deeur:9:14)=§b4ipn
084100060615     c                   endif
084200060615     c
084300060615     c                   eval      %subst(§deeur:23:3)=§b4isc
084400060615     c                   eval      %subst(§deeur:26:3)=%editc(§15cie:'X')
084500060616     c* §DESIP solo per DP4
084600060616     c* ci sono i chk digits
084700060616     c                   move      §b4icd        §DESIP
084800060616     c                   clear                   trul28ds4
084900060616     c                   eval      i284cod=%subst(§DEEUR:2:27)
085000060616     c                   call      'TRUL28R5'
085100060616     c                   parm                    trul28ds4
085200060616     c                   movel     o284ckd       §DESIP
085300060615     c
085400060615     c                   eval      %subst(§DEXX1:1:7)=sie2verd
085500060615     c                   eval      %subst(§DEXX1:8:4)=§b4ida
085600060615     c                   eval      %subst(§DEXX1:12:4)=§b4ids
085700060615     c
085800060615     C                   MOVEL     DPDTEL        §DEXX2
085900060615     c                   movel     §b4isc        ksrvc
086000060615     c     kcsc          chain     dpcsc01l
086100060615     c                   if        %found(dpcsc01l)
086200060620     c                   evalr     W016A = %TRIM(cscsrvd)
086300060620     c                   move      w016A         §DEXX2
086400060615     c                   endif
086500060615     c
086600060615     c
086700000203    1C                   ENDIF
086800971219     C                   ENDSR
086900990204     C**
087000971219     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
087100971219     C     ETIEST        BEGSR
087200971219     C**
087300971219     C                   MOVEL     '3A'          COD
087400971219     C                   MOVEL(P)  ARBCBO        KEY
087500971219     C     KTAB2         CHAIN     TABEL                              31
087600971219     C  N31TBLFLG        IFNE      ' '
087700971219     C                   SETON                                        31
087800971219     C                   END
087900971219     C**
088000971219     C                   MOVEL     TBLUNI        DS3A
088100971219     C                   MOVEL     §3ATB1        FLGTB1            1
088200971219     C**
088300971219    3C                   SELECT
088400971219     C     FLGTB1        WHENEQ    'A'
088500971219     C     §3AFCA        ANDEQ     0
088600971219     C                   MOVEL     'A'           §DEFCA
088700971219     C     FLGTB1        WHENEQ    'A'
088800971219     C     §3AFCA        ANDNE     0
088900971219     C                   MOVEL     'X'           §DEFCA
089000971219     C     §3AFCA        WHENNE    0
089100971219     C                   MOVEL     'S'           §DEFCA
089200971219    3C                   ENDSL
089300971219     C**
089400971219     C                   ENDSR
089500980324     C*--- IMPOSTO CAMPI SEGNAC.EUROEX DA PASSARE A PGM DI STAMPA ----*
089600980324     C     EUREXP        BEGSR
089700030414
089800980324     C                   MOVEL     'E'           WTRC
089900051013     C     KARS          CHAIN     FIARS01L                           31
090000980324    2C     *IN31         IFEQ      *OFF
090100980324     C* PASSO AL PGM DI STAMPA ETICHETTE IL N. SEGNACOLLO EUROEXPRESS
090200980324     C                   MOVEL     ARSNOT        §DEEUR
090300980324     C* SE IMPORT BARTOLINI METTO NEL CODICE PRODOTTO DELL'ETICHETTA
090400980324     C* ITALIA IL N.SEGNACOLLO EUROEXPRESS
090500020218    3C     LNPNTW        IFEQ      'EEX'
090600980330     C                   MOVEA     ARSNOT        W35
090700980330     C                   Z-ADD     1             BB                2 0
090800980330     C     ' '           LOOKUP    W35(BB)                                31
090900980330     C   31              MOVEA     *BLANKS       W35(BB)
091000980330     C                   MOVEA     W35(6)        §LDCDP
091100980324    3C                   ENDIF
091200980324    2C                   ENDIF
091300980324     C                   ENDSR
091400060615     c
091500020218      *----------------------------------------------------------------
091600020218      *--- IMPOSTO CAMPI SEGNACOLLO DA PASSARE A PGM DI STAMPA     ---*
091700020218      *---------------------------------------------------------------*
091800020218     c     sr_fedex      begsr
091900020218
092000020218     c                   eval      wtrc = 'F'
092100051013     c     kars          chain     fIars01l
092200051013     c                   if        %found(fIars01l) and arsnot <> *blanks
092300020218     c                   movel     arsnot        §ldcdp
092400020218     c                   endif
092500020218
092600020218     c                   endsr
092700980409     C**
092800980409     C*--- CONTROLLA SE E' SI NO DDT ---------------------------------*
092900980409     C     CTRDDT        BEGSR
093000980409     C                   SETOFF                                       28
093100031028     c                   Clear                   dsbl4a
093200980409     C                   MOVEL     'A'           WTRC
093300070419     C     KAR42         CHAIN(n)  FiAR401L                           31
093400070419     c*!n31              Movel     Ar4Not        §b4abm
093500070419     c  n31              Movel     Ar4Not        dsbl4a
093600031028     c  n31              Move      §b4abm        wbam              1
093700980409     C   31              MOVEL     'S'           WBAM
093800980409     C**
093900980409     C     WBAM          IFEQ      'N'
094000070419     c     wlnadstauto   andeq     *off
094100980409     C                   SETON                                        28
094200980409     C                   ENDIF
094300980409     C** RICHIESTO SI DDT
094400980409     C     VIDDDT        IFEQ      'S'
094500980409     C     WBAM          ANDNE     'C'
094600980409     C     WBAM          ANDNE     'S'
094700031028     c     wbam          andne     'P'
094800031028     c     wbam          andne     'Q'
094900980409     C                   SETON                                        28
095000980409     C                   ENDIF
095100980409     C** RICHIESTO NO DDT
095200980409     C     VIDDDT        IFEQ      'N'
095300980409     C     WBAM          IFEQ      'C'
095400980409     C     WBAM          OREQ      'S'
095500031028     c     wbam          oreq      'P'
095600031028     c     wbam          oreq      'Q'
095700980409     C                   SETON                                        28
095800980409     C                   ENDIF
095900980409     C                   ENDIF
096000980409     C**
096100980409     C                   ENDSR
096200990204     C**
096300990204     C* PER RISTAMPA BOLLE CONTROLLO SE E' RISTAMPABILE --------------*
096400990204     C     CTRRIS        BEGSR
096500990204     C**
096600990204     C* VEDO SE BOLLA FRANCO O ASSEGNATO
096700990204    3C     ARBNRS        IFGT      0
096800990204     C                   MOVEL     '3A'          COD
096900990204     C                   MOVEL(P)  ARBCBO        KEY
097000990204     C     KTAB2         CHAIN     TABEL                              30
097100990204    4C     *IN30         IFEQ      *OFF
097200990204     C                   MOVEL     TBLUNI        DS3A
097300990204     C                   ELSE
097400990204     C                   CLEAR                   DS3A
097500990204    4C                   ENDIF
097600990204     C                   MOVEL     §3ATB1        W001A             1
097700990204     C* ASSEGNATO
097800990204    4C     W001A         IFEQ      'A'
097900990204     C                   MOVEL(P)  ARBCCM        KEY
098000990204     C                   ELSE
098100990204     C* FRANCO
098200990204     C                   MOVEL(P)  ARBKSC        KEY
098300990204    4C                   ENDIF
098400990204     C**
098500990204     C                   MOVEL     '3C'          COD
098600990204     C     KTAB2         CHAIN     TABEL                              30
098700990204    4C     *IN30         IFEQ      *OFF
098800990204     C                   MOVEL     TBLUNI        DS3C
098900990204     C                   ELSE
099000990204     C                   CLEAR                   DS3C
099100990204    4C                   ENDIF
099200990204     C* NON RISTAMPABILE
099300990204    4C     §3CRSB        IFEQ      'N'
099400990204     C                   SETON                                        28
099500990204    4C                   ENDIF
099600990204    3C                   ENDIF
099700990204     C*
099800990204     C                   ENDSR
099900920422     C*
100000050418      *---------------------------------------------------------------*
100100050418      *  Verifica se spedizione "DI VALORE"                           *
100200050418      *---------------------------------------------------------------*
100300050418     c     CtrSpeVal     BEGSR
100400050418      *
100500050418if  1c                   if        NOT %open(FIAR501L)
100600050418     c                   open      FIAR501L
100700050418e   1c                   endif
100800050418      *
100900050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
101000050418if  1c                   if            ARBaas = AR5aas
101100050418     c                             and ARBlnp = AR5lnp
101200050418     c                             and ARBnrs = AR5nrs
101300050418     c                             and ARBnsp = AR5nsp
101400050418     c                   leavesr
101500050418e   1c                   endif
101600050418      *
101700050418      * Reperisce i dati da FIAR5
101800050418     c                   clear                   dAR5gen
101900050418     c     K05AR501      setgt     FIAR5000
102000050418do  1c                   dou            %eof(FIAR501L)
102100050418     c                             or   AR5atb =  *blanks
102200050418     c     K05AR501      readpe    FIAR5000
102300050418e   1c                   enddo
102400050418if  1c                   if             NOT %eof(FIAR501L)
102500050418     c                             and  AR5atb =  *blanks
102600050418     c                   movel     AR5uni        dAR5gen
102700050418e   1c                   endif
102800050418      *
102900050418     c                   ENDSR
103000050418      *
103100970715     OFNLV22P   T    LRNLR
