000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300070419     H DECEDIT('0,') DATEDIT(*yMd.)
000400941227      * FNLR73R *----------------------------------------------------*
000500980409      *        - RISTAMPA BOLLE E SEGNACOLLI IN ARRIVO
000600911205      *--------------------------------------------------------------*
000700970715     FAZORG01L  IF   E           K DISK
000800970715     FFNARB01L  IF   E           K DISK
000900970715     FFNART01L  IF   E           K DISK
001000941227     FFNARB27L  IF   E           K DISK
001100941227     F                                     RENAME(FNARB000:FNARB027)
001200030414     fFnart27l  if   e           k Disk    Rename(Fnart000:Fnart027)
001300070419     FFiAR401L  uF   E           K DISK
001301171020     FFipnd01L  iF   E           K DISK
001400051013     FFIARS01L  IF   E           K DISK
001500971219     FTABEL00F  IF   E           K DISK
001600040519     FFNWlR73L  iF   E           K DISK    USROPN
001700050418     fFIAR501L  if   e           k disk    usropn
001800060615     Fdpcdp01l  IF   E           K DISK
001900060615     Fdpcsc01l  IF   E           K DISK
002000970715     FFNLV22P   O    F  132        PRINTER
002100980330     D*
002200980330     D W35             S              1    DIM(35)                              COMODO
002300000203      *  SCHIERA PER COMPORRE RMN IN STAMPA SEGNACOLLO DPD IMPORT
002400060518     D*W15             S              1    DIM(15)
002500911205      *
002600900523     D PARAM           DS
002700920422     D  VIDLNP                11     13  0
002800941227     D  DSD                   14     21  0
002900941227     D  DAAS                  14     17  0
003000941227     D  DMGS                  18     21  0
003100941227     D  DSA                   22     29  0
003200941227     D  VIDSED                30     31  0
003300941227     D  VIDNSD                32     38  0
003400941227     D  NUMD                  30     38  0
003500941227     D  VIDAAS                39     42  0
003600941227     D  VIDSEA                43     44  0
003700941227     D  VIDNSA                45     51  0
003800941227     D  NUMA                  43     51  0
003900941227     D  VIDLNA                52     54  0
004000970715     D  TIRIS                 55     55
004100971219     D  VIDSDI                56     56
004200980409     D  VIDDDT                57     57
004300030414     d  VidFls                58     60  0
004400030414     d  VidNrs                61     62  0
004500030414     d  VidNsc                63     69  0
004600030414     d  VidLnas               70     72  0
004700120309     d* wlnadstauto           73     73
004800030612     d* - parametri da TRUL90R:
004900030612     d* · pgm.   per stampa LDV
005000030612     d  wD90psl              157    166
005100030612     d* · pgm di stampa etichette segnacolli
005200030612     D  wD90pss              207    216
005300911211      *
005400920422     D                 DS
005500941227     D  ARBAAS                 1      4  0
005600941227     D  ARBMGS                 5      8  0
005700941227     D  ARBDSP                 1      8  0
005800060518     D**               DS
005900060518     D**DSRMN                  1     15
006000060518     D**DSRMN1                 4      6
006100060518     D**DSRMN2                 7     14
006200060518     D**DSRMN3                15     15
006300171020     D****             DS
006400171020     D**DPDPAR                 1     12
006500171020     D**DPD1                   1      3
006600171020     D**DPD2                   4     11
006700060518     D**DPD3                  12     12
006800171020     D***              DS
006900171020     D**DPDSTP                 1     14
007000171020     D**DPDS1                  1      3
007100171020     D**DPDV1                  4      4    INZ
007200171020     D**DPDS2                  5     12
007300171020     D**DPDV2                 13     13    INZ
007400171020     D**DPDS3                 14     14
007500000203     D* TELEFONO E FAX BARTOLINI DA STAMPARE PER EXPORT DPD
007600040524     D*TELFAX          DS
007700040524     D* DPDTEL                 1     15
007800040524     D* DPDFAX                16     30
007900941227     D*
008000000203     D* DS PER TIBS02R - GESTIONE TNTBE00F
008100000203     D DSBS02        E DS                  EXTNAME(TIBS02DS)
008200030612      * DS per pgm di STAMPA BOLLE
008300030613     d FNLSB5DS      e ds                  inz
008400130314     d FNLSB6ds1     e ds                  inz
008500130314     d  DB6pdf       e                     inz('F')
008600030613      *
008700060518     D*DSBL4F        E DS
008800171020     D*DSBL4I        E DS
008801171020     d dpnddp4       E DS
008802171020     d dpnddp5       E DS
008900970715     D DSBLD         E DS
009000971219     D DSBLDE        E DS
009100000201     D OG143         E DS
009200040518     D OG150         E DS
009300971219     D DS15          E DS
009400971219     D DS03          E DS
009500971219     D DS3A          E DS
009600990204     D DS3C          E DS
009700011011     D DS3I          E DS
009800940216     D KPJBA         E DS
009900031028     d dsbl4a        e ds
010000060615     d tisie2ds      e ds
010100060616     d trul28ds4     e ds
010200050418     d dAR5gen       e ds                  inz
010300060519     d w005a           s              5
010400060620     d w016a           s             16
010500060615     d wcalltisie2     s              1
010600060615     d ksrvc           s                   like(cscsrvc)
010700911206      ****************************************************************
010800911206      *  RIEPILOGO INDICATORI
010900911210      ****************************************************************
011000920422      * 01    - IMMESSO NUMERO SPEDIZIONE DAL
011100920422      * 02    - IMMESSO NUMERO SPEDIZIONE AL
011200920422      * 03    - IMMESSA DATA DAL
011300920422      * 04    - IMMESSA DATA DAL / AL
011400030414      * 05    - immessa lna
011500030414      * 06    - immesso segnacollo
011600920422      * 30    - LETTURA FILE
011700911206      *****************************************************************
011800000000     C     *ENTRY        PLIST
011900000000     C                   PARM                    KPJBA
012000911211     C                   MOVEL     KPJBU         PARAM
012100911211      *---------------------------------------------------------------*
012200911211      * CHIAVE X POSIZIONAMENTO SU FLARB35C
012300911211      *
012400911211     C     KARB          KLIST                                                  *
012500920422     C                   KFLD                    VIDAAS                         *
012600920422     C                   KFLD                    VIDLNP                         *
012700920422     C                   KFLD                    VIDSED                         *
012800920422     C                   KFLD                    VIDNSD                         *
012900920422     C     KARB2         KLIST                                                  *
013000920422     C                   KFLD                    VIDLNP                         *
013100920422     C                   KFLD                    DAAS                           *
013200920422     C                   KFLD                    DMGS                           *
013300970203     C     KAR42         KLIST
013400970203     C                   KFLD                    ARBAAS
013500970203     C                   KFLD                    ARBLNP
013600970203     C                   KFLD                    ARBNRS
013700970203     C                   KFLD                    ARBNSP
013800970203     C                   KFLD                    WTRC
013900970715     C     KBLD          KLIST                                                  *
014000970715     C                   KFLD                    ARBAAS                         *
014100970715     C                   KFLD                    ARBLNP                         *
014200970715     C                   KFLD                    ARBNRS                         *
014300970715     C                   KFLD                    ARBNSP                         *
014400971219     C     KTAB2         KLIST
014500971219     C                   KFLD                    CODUT             1 0
014600971219     C                   KFLD                    COD               2
014700971219     C                   KFLD                    KEY               8
014800971219     C                   Z-ADD     1             CODUT
014900980324     C     KARS          KLIST
015000980324     C                   KFLD                    ARBFLS
015100980324     C                   KFLD                    ARBLNA
015200980324     C                   KFLD                    ARBNRS
015300980324     C                   KFLD                    §LDNCD
015400980324     C                   KFLD                    WTRC
015500060615     C     KCDP          KLIST
015600060615     C                   KFLD                    sie2ver
015700060615     C                   KFLD                    §b4ida
015800060615     C     KCSC          KLIST
015900060615     C                   KFLD                    sie2ver
016000060615     C                   KFLD                    ksrvc
016100030414      * Fnart27L
016200030414     c     kArt27        Klist
016300030414     c                   Kfld                    VidFls
016400030414     c                   Kfld                    VidLnas
016500030414     c                   Kfld                    VidNrs
016600030414     c                   Kfld                    VidNsc
016700040519     c     kArt          Klist
016800040519     c                   Kfld                    wlrlnp
016900040519     c                   Kfld                    wlrLna
017000040519     c                   Kfld                    wlrNrs
017100040519     c                   Kfld                    wlrNsc
017200050418      * FIAR501L
017300050418     c     k05AR501      klist
017400050418     c                   kfld                    ARBaas
017500050418     c                   kfld                    ARBlnp
017600050418     c                   kfld                    ARBnrs
017700050418     c                   kfld                    ARBnsp
017800050418     c                   kfld                    AR5trd
017900050418     c                   eval      AR5trd = 'GEN'
018000970203     C* DEFINIZIONE CAMPI
018100020207     C     *LIKE         DEFINE    §OGNTW        LNANTW
018200020207     C     *LIKE         DEFINE    §OGNTW        LNPNTW
018300970203     C     *LIKE         DEFINE    AR4TRC        WTRC
018400011011     C     *LIKE         DEFINE    §3ISSA        WBART
018500000203      * AGGANCIO TABELLA "CHI" PER TEL E FAX PER EXPORT DPD
018600040524     C***                CLEAR                   DSBS02
018700040524     C***                MOVEL     'L'           T02TLA
018800040524     C***                MOVEL     'C'           T02MOD
018900040524     C***                MOVEL     KNSIF         T02SIF
019000040524     C***                MOVEL     'CHI'         T02COD
019100040524     C***                MOVEL     'TELFAX'      T02KE1
019200000203      *
019300040524     C***                CALL      'TIBS02R'
019400040524     C***                PARM                    KPJBA
019500040524     C***                PARM                    DSBS02
019600000203      *
019700040524     C***  T02ERR        IFEQ      *BLANKS
019800040524     C***                MOVEL     T02UNI        TELFAX
019900040524     C***                ELSE
020000040524     C***                MOVEL     *ALL'?'       TELFAX
020100040524     C***                ENDIF
020200011011     C* SE DEVO RISTAMPARE I SEGNACOLLI VADO SU TABELLA 3I PER VEDERE
020300040518     C* SE posso ristampare L'ETICHETTA BARTOLINI
020400030506     C**!!!TIRIS         IFEQ      'S'
020500030506     C     TIRIS         IFEQ      'D'
020600011011     C                   MOVEL     '3I'          COD
020700011011     C                   MOVEL(P)  '1'           KEY
020800011011     C     KTAB2         CHAIN     TABEL                              31
020900011011     C  N31              MOVEL     TBLUNI        DS3I
021000011011     C   31              CLEAR                   DS3I
021100011011     C                   MOVE      §3ISSA        WBART
021200011011     C                   ENDIF
021300040519     c* Se devo elaborare il file di work lo apro
021400040519     c     tiris         ifeq      'F'
021500040519     c                   open      fnwlr73l
021600040519     c                   endif
021700911206      *---------------------------------------------------------------*
021800920422     C*
021900920422     C* NUMERO DAL IMMESSO
022000920422     C     NUMD          IFGT      0
022100920422     C                   SETON                                        01
022200920422     C                   END
022300920422     C* NUMERO AL  IMMESSO
022400920422     C     NUMA          IFGT      0
022500920422     C                   SETON                                        02
022600920422     C                   END
022700920422     C* DATA DAL
022800920422     C     DSD           IFGT      0
022900920422     C                   SETON                                        03
023000920422     C                   END
023100920422     C* DATA AL
023200920422     C     DSA           IFGT      0
023300920422     C                   SETON                                        04
023400920422     C                   END
023500920422     C* IMMESSA LINEA DI ARRIVO
023600920422     C     VIDLNA        IFGT      0
023700920422     C                   SETON                                        05
023800920422     C                   END
023900030414      * Immesso il segnacollo
024000030414     c                   If        VidNsc > *Zeros
024100030414     c                   Eval      *In06 = *On
024200030414     c                   EndIf
024300920422     C*
024400920422     C* N U M E R O   S P E D I Z I O N E
024500920422     C*  SOLO DAL
024600970715     C   01
024700970715     CANN02TIRIS         IFEQ      'B'
024800980407     C* NON CONTROLLO SE LA BOLLA E' GIA' STATA STAMPATA PERCHE' LO
024900980407     C*  FACCIO NEL FILTRO
025000030613     c                   clear                   FNLSB5DS
025100941227     C                   MOVEL     VIDAAS        DB0AAS
025200941227     C                   MOVEL     VIDLNP        DB0LNP
025300941227     C                   MOVEL     VIDSED        DB0NRS
025400941227     C                   MOVEL     VIDNSD        DB0NSP
025500941227     C                   MOVEL     'A'           DB0TBO
025600941227     C                   MOVEL     'R'           DB0RIS
025700030612     c                   call      wD90psl
025800030613     c                   parm                    FNLSB5DS
025900130314     c                   parm                    FNLSB6ds1
026000070419      * aggiorno il flag ddt se mai stampata
026100120309     c****               if        wlnadstauto = *on
026200070419     c                   clear                   dsbl4a
026300070419     c                   movel     vidaas        arbaas
026400070419     c                   movel     vidlnp        arblnp
026500070419     c                   movel     vidsed        arbnrs
026600070419     c                   movel     vidnsd        arbnsp
026700070419     c                   eval      wtrc = 'A'
026800070419     c     kar42         chain     fiar401l                           31
026900070419     c  n31              eval      dsbl4a = ar4not
027000070419     c                   if        §b4abm = 'N'
027100070419     c                   eval      §b4abm = 'J'
027200070419     c                   z-add     *date         §b4asl
027300070419     c  n31              eval      ar4not = dsbl4a
027400070419     c  n31              update    fiar4000
027500070419     c                   endif
027600120309     c****               endif
027700970715     C                   ELSE
027800970715     C*
027900970715     C     KARB          CHAIN     FNARB000                           30
028000971219     C* SE BOLLA IMPORT O EXPORT LEGGO ESTENSIONE BOLLA TRC='E'
028100971219     C                   EXSR      LEGAR4
028200030414      * Non lo faccio più xchè se è una spedizione singola ho già controllato la
028300030414      * linea di arrivo nel filtro
028400030414     C**!!!WESTER        IFEQ      'S'
028500030414     C**!!!TIRIS         ORNE      'D'
028600030414     C**!!!WBART         ANDEQ     'S'
028700980324     C     ARBNCD        IFGT      0
028800970715     C                   EXSR      STASCI
028900970715     C                   ELSE
029000970715     C     ARBFNS        IFEQ      'S'
029100970715     C                   EXSR      STASCN
029200920422     C                   END
029300970715     C                   END
029400030414     C**!!!              END
029500970715     C                   END
029600920422     C**
029700920422     C*       DAL / AL
029800970715     C   01
029900970715    1CAN 02NUMD          DOWLE     NUMA
030000990318     C     NUMD          ANDGT     0
030100941227     C     KARB          CHAIN     FNARB000                           30
030200920422     C*
030300970715     C   05
030400970715    2CANN30VIDLNA        IFNE      ARBLNA
030500920422     C                   SETON                                        30
030600970715    2C                   END
030700970203     C*
030800970203     C* BOLLA ESISTENTE: CONTROLLO CHE SIA GIA' STATA STAMPATA
030900060216     C*   ALTRIMENTI NON LA STAMPO (SE NON TROVO IL RECORD IN FiAR4
031000970203     C*   CONSIDERO LA BOLLA COME SE FOSSE GIA' STATA STAMPATA)
031100980409     C*   CONTROLLO ANCHE LA PARZIALIZZAZIONE SI NO DDT
031200070419      *   se bolla mai stampata non la stampo se non c'è la procedura attiva
031300070419      *   della distinte automatiche...altrimenti la posso stampare
031400970715    2C     *IN30         IFEQ      *OFF
031500990204    3C     TIRIS         IFEQ      'B'
031600980409     C                   EXSR      CTRDDT
031700990204     C* CONTROLLO SE E' RISTAMPABILE LA BOLLA
031800000203     C  N28              EXSR      CTRRIS
031900000203     C*
032000000203    4C     *IN28         IFEQ      *OFF
032100000203     C     ARBLNA        CHAIN     AZORG01L                           31
032200000203     C* ESCLUDO LE BOLLE EXPORT DPD E I NO DDT EUROEXPRESS
032300000203     C                   MOVEL     ORGDE3        OG143
032400020207    5C     §OGNTW        IFEQ      'DPD'
032500000203     C                   SETON                                        28
032600000203    5C                   ENDIF
032700000203     C*
032800020207    5C     §OGNTW        IFEQ      'EEX'
032900000203    6C     WBAM          IFNE      'S'
033000000203     C     WBAM          ANDNE     'C'
033100031028     c     wbam          andne     'P'
033200031028     c     wbam          andne     'Q'
033300000203     C                   SETON                                        28
033400000203    6C                   ENDIF
033500000203    5C                   ENDIF
033600000203    4C                   ENDIF
033700970715     C*  BOLLE
033800990204    4C     *IN28         IFEQ      *OFF
033900030613     c                   clear                   FNLSB5DS
034000941227     C                   MOVEL     VIDAAS        DB0AAS
034100941227     C                   MOVEL     VIDLNP        DB0LNP
034200941227     C                   MOVEL     VIDSED        DB0NRS
034300941227     C                   MOVEL     VIDNSD        DB0NSP
034400941227     C                   MOVEL     'A'           DB0TBO
034500941227     C                   MOVEL     'R'           DB0RIS
034600030612     c                   call      wD90psl
034700030613     c                   parm                    FNLSB5DS
034800130314     c                   parm                    FNLSB6ds1
034900990204    4C                   ENDIF
035000990204    3C                   ENDIF
035100970715     C** SEGNACOLLI
035200990204    3C     TIRIS         IFEQ      'S'
035300030414     C**!!!TIRIS         OREQ      'D'
035400971219     C* SE BOLLA IMPORT O EXPORT LEGGO ESTENSIONE BOLLA TRC='E'
035500971219     C                   EXSR      LEGAR4
035600971219     C*
035700030414     C**!!!              EXSR      CHKEST
035800040527If  4c****               If        LnaNtw = 'DPD' or LnaNtw = 'FED' or
035900040527     c****                         LnaNtw = 'EEX'
036000040527If  4c                   If        LnaNtw = 'DPD'
036100030414    4C**!!!WESTER        IFEQ      'S'
036200030414     C**!!!TIRIS         ORNE      'D'
036300030414     C**!!!WBART         ANDEQ     'S'
036400990204    5C     ARBNCD        IFGT      0
036500970715     C                   EXSR      STASCI
036600970715     C                   ELSE
036700990204    6C     ARBFNS        IFEQ      'S'
036800970715     C                   EXSR      STASCN
036900990204    6C                   END
037000990204    5C                   END
037100990204    4C                   END
037200971219    3C                   END
037300970715    2C                   ENDIF
037400920422     C*
037500920422     C                   ADD       1             NUMD
037600970715    1C                   ENDDO
037700970715     C*
037800920422     C*
037900920422     C* D A T A   S P E D I Z I O N E
038000920422     C*
038100941227     C   03KARB2         SETLL     FNARB27L
038200941227     C   03
038300941227     CANN04KARB2         READE     FNARB27L                               30
038400941227     C   03
038500941227     CAN 04VIDLNP        READE     FNARB27L                               30
038600920422     C*
038700990204    1C   03*IN30         DOWEQ     *OFF
038800920422     C* SE DAL / AL CONTROLLO SE DATA MAGGIORE
038900990204    2C   04ARBDSP        IFGT      DSA
039000920422     C                   SETON                                        30
039100990204    2C                   END
039200920422     C                   SETOFF                                       29
039300920422     C* SE NON E' LA LINEA DI ARRIVO RICHIESTA NON STAMPO
039400990204     C   05
039500990204    2CANN30VIDLNA        IFNE      ARBLNA
039600920422     C                   SETON                                        29
039700990204    2C                   END
039800920422     C*
039900970203     C* BOLLA ESISTENTE: CONTROLLO CHE SIA GIA' STATA STAMPATA
040000060216     C*   ALTRIMENTI NON LA STAMPO (SE NON TROVO IL RECORD IN FiAR4
040100970203     C*   CONSIDERO LA BOLLA COME SE FOSSE GIA' STATA STAMPATA)
040200980409     C*   CONTROLLO ANCHE SE RICHIESTO SI NO DDT SE E' SECONDO LE RICH
040300990204    2C     *IN30         IFEQ      *OFF
040400970203     C     *IN29         ANDEQ     *OFF
040500990204     C* RISTAMPA BOLLE: CONTROLLO IL DDT
040600990204    3C     TIRIS         IFEQ      'B'
040700990204     C                   EXSR      CTRDDT
040800990204     C*                 CONTROLLO SE BOLLA RISTAMPABILE
040900990204     C  N28              EXSR      CTRRIS
041000000201     C*
041100000201    4C     *IN28         IFEQ      *OFF
041200000201     C     ARBLNA        CHAIN     AZORG01L                           31
041300000201     C* ESCLUDO LE BOLLE EXPORT DPD E I NO DDT EUROEXPRESS
041400000201     C                   MOVEL     ORGDE3        OG143
041500020207    5C     §OGNTW        IFEQ      'DPD'
041600000201     C                   SETON                                        28
041700000201    5C                   ENDIF
041800000201     C*
041900020207    5C***        ORGFL1    IFEQ 'E'
042000020207    5C     §OGNTW        IFEQ      'EEX'
042100000201    6C     WBAM          IFNE      'S'
042200000201     C     WBAM          ANDNE     'C'
042300031028     c     wbam          andne     'P'
042400031028     c     wbam          andne     'Q'
042500000201     C                   SETON                                        28
042600000201    6C                   ENDIF
042700000201    5C                   ENDIF
042800000201    4C                   ENDIF
042900970203     C*
043000990204    4C     *IN28         IFEQ      *OFF
043100030613     c                   clear                   FNLSB5DS
043200941227     C                   MOVEL     ARBAAS        DB0AAS
043300941227     C                   MOVEL     ARBLNP        DB0LNP
043400941227     C                   MOVEL     ARBNRS        DB0NRS
043500941227     C                   MOVEL     ARBNSP        DB0NSP
043600941227     C                   MOVEL     'A'           DB0TBO
043700941227     C                   MOVEL     'R'           DB0RIS
043800030612     c                   call      wD90psl
043900030613     c                   parm                    FNLSB5DS
044000130314     c                   parm                    FNLSB6ds1
044100030612    4C                   ENDIF
044200030612    3C                   ENDIF
044300970715     C*
044400990204    3C     TIRIS         IFEQ      'S'
044500030414     C**!!!TIRIS         OREQ      'D'
044600971219     C* SE BOLLA IMPORT O EXPORT LEGGO ESTENSIONE BOLLA TRC='E'
044700971219     C                   EXSR      LEGAR4
044800971219     C*
044900030414     C**!!!              EXSR      CHKEST
045000040527If  4c****               If        LnaNtw = 'DPD' or LnaNtw = 'FED' or
045100040527     c****                         LnaNtw = 'EEX'
045200040527If  4c                   If        LnaNtw = 'DPD'
045300030414    4C**!!!WESTER        IFEQ      'S'
045400030414     C**!!!TIRIS         ORNE      'D'
045500030414     C**!!!WBART         ANDEQ     'S'
045600990204    5C     ARBNCD        IFGT      0
045700970715     C                   EXSR      STASCI
045800970715     C                   ELSE
045900990204    6C     ARBFNS        IFEQ      'S'
046000970715     C                   EXSR      STASCN
046100990204    6C                   END
046200990204    5C                   END
046300990204    4C                   END
046400990204    3C                   ENDIF
046500990204    2C                   ENDIF
046600920422     C*
046700941227     C  N04KARB2         READE     FNARB27L                               30
046800941227     C   04
046900941227     CANN30VIDLNP        READE     FNARB27L                               30
047000990204    1C                   ENDDO
047100030414
047200030414
047300030414      * S E G N A C O L L O
047400030414
047500030414If  1c                   If        *In06
047600030414     c                   Clear                   Og143
047700030414     c     VidLnas       Chain     Azorg01l
047800030414If  2c                   If        %Found(Azorg01l)
047900030414     c                   Movel     Orgde3        Og143
048000040518     c                   Movel     Orgdf0        Og150
048100030414    2c                   Endif
048200030414      * se lna non è estera e richiesa la stampa con F8
048300040527If  2c***                If        §OgNtw <> 'DPD' and §OgNtw <> 'FED' and
048400040527     c***                          §OgNtw <> 'EEX' and
048500040527If  2c                   If        §OgNtw <> 'DPD' and
048600030414     c                             Tiris= 'D' and
048700030414      * deve essere abilitata
048800030414     c                             wbart <> 'S'
048900030414     c                   GoTo      nostp
049000030414    2c                   Endif
049100030414     c     kArt27        Chain     Fnart27l
049200030414If  2c                   If        %Found(Fnart27l)
049300030414     c                   Move      ArtAas        VidAas
049400030414     c                   Move      ArtLnp        VidLnp
049500030414     c                   Move      ArtNrs        VidSed
049600030414     c                   Move      ArtNsp        VidNsd
049700030414    2c                   EndIf
049800030414     c     kArb          Chain     Fnarb01l
049900030414If  2c                   If        %Found(Fnarb01l)
050000030414     c                   ExSr      LegAr4
050100030414     c                   ExSr      StaNsc
050200030414    2c                   EndIf
050300030414     c     nostp         tag
050400030414    1c                   EndIf
050500040519
050600040519      * F N W L R 7 3 L
050700040519
050800040519     c     tiris         ifeq      'F'
050900040519     c     *loval        setll     fnwlr73l
051000040519     c                   do        *hival
051100040519     c                   read      fnwlr73l
051200040519      * Fine File esco dal ciclo
051300040519     c                   If        %Eof(Fnwlr73l)
051400040519     c                   Leave
051500040519     c                   EndIf
051600040519     c     kArt          Chain     Fnart27l
051700040519If  2c                   If        %Found(Fnart27l)
051800040519     c                   Move      ArtAas        VidAas
051900040519     c                   Move      ArtLnp        VidLnp
052000040519     c                   Move      ArtNrs        VidSed
052100040519     c                   Move      ArtNsp        VidNsd
052200040519    2c                   EndIf
052300040519     c     kArb          Chain     Fnarb01l
052400040519If  2c                   If        %Found(Fnarb01l)
052500040519     c                   ExSr      LegAr4
052600040519     c                   ExSr      StaNsc
052700040519    2c                   EndIf
052800040519     c                   enddo
052900040519     c                   endif
053000911206      *
053100040519     c     tiris         ifeq      'F'
053200040519     c                   close     fnwlr73l
053300040519     c                   endif
053400130314      *
053500130314      * Stampa eventuali documenti PDF abbinati alle spediz. stampate
053600130320if  1c                   if        DB6num > *zero
053700130314     c                   clear                   FNLSB5ds
053800130314     c                   eval      DB0tla = 'L'
053900130314     c                   eval      DB0fa4 = 'I'
054000130314     c                   eval      DB6pdf = 'S'
054100130314     c                   call      wD90psl
054200130314     c                   parm                    FNLSB5ds
054300130314     c                   parm                    FNLSB6ds1
054400130314x   1c                   else
054500130314     c                   clear                   FNLSB5ds
054600130314     c                   eval      DB0tla = 'C'
054700130314     c                   call      wD90psl
054800130314     C                   parm                    FNLSB5ds
054900130314e   1c                   endif
055000040519     c*
055100060615     c                   if        wcalltisie2='S'
055200060615     c                   clear                   tisie2ds
055300060615     c                   eval      sie2tla='C'
055400060615     c                   call      'TISIE2R'
055500060615     c                   parm                    tisie2ds
055600060615     c                   endif
055700130314     c*
055800911206     C                   SETON                                        LR
055900970715     C**
056000970715     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI SEQUENZIALI ---------*
056100970715     C     STASCI        BEGSR
056200970715     C                   Z-ADD     ARBNCD        COMSCD            7 0
056300970715     C     ARBNCA        IFEQ      0
056400970715     C                   Z-ADD     ARBNCD        ARBNCA
056500970715     C                   END
056600970715     C* IMPOSTO CAMPI DI STAMPA
056700970715     C                   EXSR      IMPO
056800970715     C*
056900970715     C     COMSCD        DOWLE     ARBNCA
057000990318     C     COMSCD        ANDGT     0
057100970715     C                   ADD       1             §LDNC1
057200970715     C* S T A M P A
057300970715     C                   MOVEL     COMSCD        §LDNCD
057400020218     c                   select
057500020218      * per segnacolli FedEx imposta §LDCDP
057600020218     c                   when      lnantw = 'FED'
057700020218     c                   exsr      sr_fedex
057800020218      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
057900040507     c****               when      lnpntw = 'EEX' or lnantw = 'EEX'
058000040507     c                   when      lnpntw = 'EEX'
058100980324     C                   EXSR      EUREXP
058200171020     c**                 when      (lnpntw = 'DPD'  or lnantw='DPD')
058300171020     c***                          and §b4ibi<>' ' and §DETST='EST'
058500020218     c                   endsl
058600970715     C*
058700030612     C                   CALL      wD90pss
058800970715     C                   PARM                    DSBLD
058900971219     C                   PARM                    DSBLDE
059000970715     C*
059100970715     C                   ADD       1             COMSCD
059200970715     C*
059300970715     C                   END
059400970715     C*
059500970715     C                   ENDSR
059600970715     C**
059700970715     C* IMPOSTO CAMPI PER STAMPA IN IMMEDIATO ------------------------*
059800970715     C     IMPO          BEGSR
059900050418      *
060000050418      * Verifica se spedizione "di valore"
060100050418     c                   exsr      CtrSpeVal
060200970715     C*
060300970715     C                   Z-ADD     0             §LDNC1
060400970715     C                   MOVEL     ARBFLS        §LDLNP
060500970715     C     ARBFLS        CHAIN     AZORG                              30
060600970715     C  N30              MOVEL     ORGDES        §LDDLP
060700970715     C   30              MOVEL     *BLANKS       §LDDLP
060800970715     C                   MOVEL     ARBNRS        §LDNRS
060900970715     C                   MOVEL     ARBLNA        §LDLNA
061000970715     C     ARBLNA        CHAIN     AZORG                              30
061100970715     C  N30ORGDE5        IFNE      *BLANKS
061200970715     C                   MOVEL     ORGDE5        §LDDLA
061300970715     C                   ELSE
061400970715     C                   MOVEL     ORGDES        §LDDLA
061500970715     C                   END
061600970715     C   30              MOVEL     *BLANKS       §LDDLA
061700970715     C                   MOVEL     ARBTFA        §LDTNA
061800970715     C                   MOVEL     ARBTSP        §LDTSP
061900970715     C                   MOVEL     ARBZNC        §LDZSC
062000970715     C                   MOVE      ARBNCL        §LDNC2
062100970715     C                   MOVEL     ARBRSM        §LDRSM
062200970715     C                   MOVEL     ARBPRM        §LDPRM
062300970715     C                   MOVEL     ARBRSD        §LDRSD
062400970715     C                   MOVEL     ARBCAD        §LDCAD
062500970715     C                   MOVEL     ARBLOD        §LDLOD
062600970715     C                   MOVEL     ARBIND        §LDIND
062700970715     C                   MOVEL     ARBPRD        §LDPRD
062800970715     C                   MOVEL     *ZEROS        §LDNCP
062900970715     C                   MOVEL     *ZEROS        §LDNCU
063000050418if  1c                   if        §AR5bva = 'V'
063100050418     c                   movel     'X'           §LDrst
063200050418x   1c                   else
063300970715     C                   MOVEL     'R'           §LDRST
063400050418e   1c                   endif
063500970715     C                   Z-ADD     ARBPKF        §LDPKG
063600970715     C                   Z-ADD     ARBVLF        §LDVOL
063700970715     C                   MOVEL     ARBFVF        §LDFVR
063800970715     C                   MOVEL     ARBAAS        §LDAAS
063900970715     C                   MOVEL     ARBMGS        §LDMGS
064000971219     C*
064100060615      * La stampa ALL non è più prevista: o solo etichetta ITA-bartolini
064200060615      * stampo sia parte bartolini che paro solo etichetta EST-estera
064300040518If  1c                   If        tiris = 'S'
064400060615     c*****              If        §ogsea <> 'S'
064500060615     c*****              Eval      §Detst = 'ALL'
064600060615     c*****              else
064700040518     c                   Eval      §Detst = 'EST'
064800060615     c*****              endif
064900030414     c                   ExSr      EtiEst
065000030414    1c                   EndIf
065100060615     c
065200030414If  1c                   If        tiris = 'D'
065300060615If  2c***                If        (LnaNtw = 'DPD' or LnaNtw = 'FED' or
065400060615     c***                          LnaNtw = 'EEX') and §ogsea <> 'S'
065500060615     c***                Eval      §Detst = 'ALL'
065600060615     c***                ExSr      EtiEst
065700060615   x2c***                Else
065800030414     c                   Eval      §Detst = 'ITA'
065900060615    2c***                EndIf
066000030414    1c                   EndIf
066100040519     c* Se sto stampando i segnacolli in arrivo in base al file di work
066200040519     c* devo stampare solo l'etichetta estera
066300040519     c                   if        tiris='F'
066400040519     c                   Eval      §Detst = 'EST'
066500040519     c                   exsr      etiest
066600040519     c                   endif
066700971219     C*
066800970715     C                   ENDSR
066900970715     C**
067000970715     C*--- STAMPA BOLLA IN IMMEDIATO: SEGNACOLLI NON SEQUENZIALI -----*
067100970715     C     STASCN        BEGSR
067200970715     C*
067300970715     C                   EXSR      IMPO
067400970715     C*
067500970715     C     KBLD          SETLL     FNART01L
067600970715     C     KBLD          READE     FNART01L                               33
067700970715     C*
067800970715     C     *IN33         DOWEQ     *OFF
067900970715     C*
068000970715     C                   Z-ADD     ARTNSC        COMSCD
068100970715     C                   ADD       1             §LDNC1
068200970715     C* S T A M P A
068300970715     C                   MOVEL     COMSCD        §LDNCD
068400020218     c                   select
068500020218      * per segnacolli FedEx imposta §LDCDP
068600020218     c                   when      lnantw = 'FED'
068700020218     c                   exsr      sr_fedex
068800020218      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
068900040507     c***                when      lnpntw = 'EEX' or lnantw = 'EEX'
069000040507     c                   when      lnpntw = 'EEX'
069100980324     C                   EXSR      EUREXP
069200020218     c                   endsl
069300970715     C*
069400030612     C                   CALL      wD90pss
069500970715     C                   PARM                    DSBLD
069600971219     C                   PARM                    DSBLDE
069700970715     C*
069800970715     C  N33KBLD          READE     FNART01L                               33
069900970715     C                   ENDDO
070000970715     C*
070100970715     C                   ENDSR
070200030414      **
070300030414      *--- STAMPA SEGNACOLLO -----------------------------------------*
070400030414     c     StaNsc        BegSr
070500030414
070600030414     c                   ExSr      Impo
070700030414
070800030414     c                   Add       1             §LDNC1
070900030414     c                   Movel     ArtNsc        §LDNCD
071000030414
071100030414     c                   select
071200030414      * per segnacolli FedEx imposta §LDCDP
071300030414     c                   when      lnantw = 'FED'
071400030414     c                   exsr      sr_fedex
071500030414      * per segnacolli EuroExpress imposta §DEEUR e §LDCDP
071600040507     c***                when      lnpntw = 'EEX' or lnantw = 'EEX'
071700040507     c                   when      lnpntw = 'EEX'
071800030414     c                   ExSr      EurExp
071900030414     c                   endsl
072000030414
072100030612     c                   CALL      wD90pss
072200030414     c                   PARM                    DSBLD
072300030414     c                   PARM                    DSBLDE
072400030414
072500030414     c                   EndSr
072600030414
072700971219     C*--- REPERISCO ESTENZIONE BOLLA TIPO RECORD 'E' ----------------*
072800971219     C     LEGAR4        BEGSR
072900000203     C                   CLEAR                   DSBLDE
073000000203     C                   CLEAR                   DSBLD
073100060616     C*
073200060616     C* CHAIN SULLE NAZIONI
073300060616     C                   MOVEL     '15'          COD
073400060616     C                   MOVEL(P)  ARBNZD        KEY
073500060616     C     KTAB2         CHAIN     TABEL                              31
073600060616     C  N31TBLFLG        IFNE      ' '
073700060616     C                   SETON                                        31
073800060616     C                   END
073900060616     C**
074000060616     C  N31              MOVEL     TBLUNI        DS15
074100060616     C   31              CLEAR                   DS15
074200060616     c* Era già qu fuori dal ciclo DPD. forse serve ance per altre
074300060616     c*  etichette estere? ad ogni modo lo lasci qui. non serve
074400060616     c*  per DP3 ma solo per DP4
074500060616     C                   MOVEL     §15COD        §DEISA
074600971219     C*
074700971219     C                   MOVEL     ARBRMA        §LDCDP
074800971219     C*
074900971219     C     ARBLNA        IFNE      SAVLNA
075000971219     C     ARBLNA        CHAIN     AZORG01L                           31
075100020207     C                   MOVE      ORGDE3        OG143
075200040521     C                   MOVEl     ORGDf0        OG150
075300020207     C                   MOVE      §OGNTW        LNANTW
075400030414     C**!!!              MOVEL     §OGDPD        LNADPD            1
075500040524     c                   clear                   dpdtel           15
075600040524     c                   clear                   dpdfax           15
075700040524     c                   movel     orgtel        dpdtel
075800040524     c                   movel     orgfax        dpdfax
075900971219     C                   Z-ADD     ARBLNA        SAVLNA            3 0
076000971219     C                   END
076100971219     C*
076200971219     C     ARBLNP        CHAIN     AZORG01L                           31
076300020207     C                   MOVE      ORGDE3        OG143
076400020207     C                   MOVE      §OGNTW        LNPNTW
076500971219     C*
076600000207     C**
076700020218     C     lnaNTW        IFEQ      'EEX'
076800040507     C                   MOVEL     'EEX'         §DETSE
076900000207     C                   ENDIF
077000000207     C*
078100171020      **  CHAIN dati dpd
078200020207    1C     LNANTW        IFEQ      'DPD'
078300060518    1C     LNpNTW        oreQ      'DPD'
078301171012
078302171012        chain (arbaas:arblnp:arbnrs:arbnsp) fipnd01l ;
078303171020        if not %found(fipnd01l)     ;
078304171020           clear pnddpe;
078305171020        else;
078306171020           §ldcdp=pndipn;
078307171012        endif;
078400060615     c
078500171012     C**                 MOVEL     'I'           WTRC
078600171012     C**   KAR42         CHAIN(n)  FiAR401L                           39
078700171012     C**N39              MOVEL     AR4NOT        DSBL4I
078800171012     C** 39              CLEAR                   DSBL4I
078900060615     c
079000171012    2C**   §B4ipn        IFNE      *BLANKS
079100060615     c* per DP3 o DP4 ma parcel lungo 11
079200171012     c*                  if        %subst(§b4ipn:12:1)=' '
079300171012     C*                  MOVEL     §B4ipn        DPDPAR
079400171012     C*                  MOVEL     DPD1          DPDS1
079500171012     C*                  MOVEL     DPD2          DPDS2
079600171012     C*                  MOVEL     §b4icd        DPDS3
079700171012     C*                  MOVEL(P)  DPDSTP        §LDCDP
079800171012     c*                  else
079900171012     c*                  movel(P)  §b4ipn        §ldcdp
080000171012     c*                  endif
080100060615     c
080200171012    2c*                  endif
080300060518    1c                   endif
080400060615     c
080500011011     C* DEVO SEMPRE RISTAMPARE ANCHE IL CHI SONO
080600040518     c*
080700060518    1C     LNANTW        IFEQ      'DPD'
080800171012     C***  §B4ipn        andne     *BLANKS
080900060615     c
081300040518     c     tiris         ifeq      'S'
081400040519     c     tiris         oreq      'F'
082001171012     c                   movel     PNDDPE        §detse
082002171020     c* Solo nell'etichetta estera DPD nel cdp mettiamo il FAX
082003171020     C                   movel(P)  dpdfax        §LDCDP
082200060615     c
082300040518     c                   endif
082301171012
082302171012     c                   select
082303171012     c* DP4
082304171012     c                   when      pnddpe='DP4'
082305171020     c                   eval      dpnddp4=pndetiu
082500060615     c
082600000203     C     ARBPKF        IFLE      3
082700000203     C                   MOVEL     'X'           §DESSE
082800000203     C                   ENDIF
082900000203     C*
083000060615     c* reperisco versione cappario DPD
083100060615     c                   clear                   tisie2ds
083200060615     c                   z-add     arbmgs        sie2dri
083300060615     c                   movel     arbaas        sie2dri
083400060615     c                   call      'TISIE2R'
083500060615     c                   parm                    TIsie2ds
083600060615     c                   eval      wcalltisie2='S'
083700060615     c
083800060615     c* chain su file dpcdp01l per prendere nazione depot
083900060615     c     kcdp          chain     dpcdp01l
084000060615     c                   if        %found(dpcdp01l)
084100060615     c                   movel     cdpiso2       §DESIA
084200060615     c                   endif
084300061010     c*
084400061010     c                   eval      §deawb= §b4iat + §b4ipy
084500060615     c
084600060615     c                   eval      %subst(§deeur:1:1) = §b4ibi
084700060615     c* richiamo pgm per avere il cap da stampare come vuole dpd
084800060615     c                   clear                   outcapdpd
084900060615     c                   call      'XCAPDPD'
085000060615     c                   parm                    arbcad
085100060615     c                   parm                    outcapdpd         7
085200061013     c                   parm      arbnzd        inpnzd            3
085300060615     c
085400060615     c                   eval      %subst(§deeur:2:7) = %subst(outcapdpd:1:7)
085500060615     c* Se il numero parcel è lungo 11, devo portarlo a 14
085600171020     c****               if        %subst(§b4ipn:12:1)=' '
085700171020     c****               eval      %subst(§deeur:9:1) = '0'
085800171020     c***                eval      %subst(§deeur:10:3)=%subst(§b4ipn:1:3)
085801171020     c***                eval      %subst(§deeur:10:3)=%subst(pndipn:1:3)
085900171020     c***                eval      %subst(§deeur:13:2)='99'
086000171020     c***                eval      %subst(§deeur:15:8)=%subst(§b4ipn:4:8)
086001171020     c***                eval      %subst(§deeur:15:8)=%subst(pndipn:4:8)
086100171020     c***                else
086200171020     c***                eval      %subst(§deeur:9:14)=§b4ipn
086201171020     c                   eval      %subst(§deeur:9:14)=pndipn
086300171020     c****               endif
086400060615     c
086500171020     c****               eval      %subst(§deeur:23:3)=§b4isc
086501171020     c                   eval      %subst(§deeur:23:3)=pndisc
086600091120     c* se service code blank vuol dire che è una bolla vecchia ancora col parcel
086700091120     c* a modo vecchio in questo caso imposto 000 per evitare che vada in errore il
086800091120     c* pgm di stampa etichette. E' successso in caso di lancio "Stampa segnacolli export
086900091120     c* DPD" (azione LRX5)
087000171020     c***                if        §b4isc=*blanks
087001171020     c                   if        pndisc=*blanks
087100091120     c                   eval      %subst(§deeur:23:3)='000'
087200091120     c                   endif
087300091120     c*
087400060615     c                   eval      %subst(§deeur:26:3)=%editc(§15cie:'X')
087500060616     c* §DESIP solo per DP4
087600060616     c* ci sono i chk digits
087700060616     c                   move      §b4icd        §DESIP
087800060616     c                   clear                   trul28ds4
087900060616     c                   eval      i284cod=%subst(§DEEUR:2:27)
088000060616     c                   call      'TRUL28R5'
088100060616     c                   parm                    trul28ds4
088200060616     c                   movel     o284ckd       §DESIP
088300060615     c
088400060615     c                   eval      %subst(§DEXX1:1:7)=sie2verd
088500060615     c                   eval      %subst(§DEXX1:8:4)=§b4ida
088600060615     c                   eval      %subst(§DEXX1:12:4)=§b4ids
088700060615     c
088800060615     C                   MOVEL     DPDTEL        §DEXX2
088900171020     c***                movel     §b4isc        ksrvc
088901171020     c                   movel     pndisc        ksrvc
089000060615     c     kcsc          chain     dpcsc01l
089100060615     c                   if        %found(dpcsc01l)
089200060620     c                   evalr     W016A = %TRIM(cscsrvd)
089300060620     c                   move      w016A         §DEXX2
089400060615     c                   endif
089401171012     c* DP5
089402171012     c                   when      pnddpe='DP5'
089403171012     c                   movel     PNDETIU       dpnddp5
089404171012     c                   eval      %subst(§deeur:1:1)=§PNDIBI
089405171012     c                   eval      %subst(§deeur:2:7) = §PNDCADP
089406171012     c                   eval      %subst(§deeur:9:14)=pndipn
089407171012     c                   eval      %subst(§deeur:23:3)=pndisc
089408171012     c                   eval      %subst(§deeur:26:3)=§pndNTWC
089409171012
089410171012     c                   eval      %subst(§deXX1:1:8)=§PNDVERD
089411171012     c                   eval      %subst(§deXX1:9:4)=§pndidast
089412171012
089413171012     c                   eval      §desse =§PNDDSRVM
089414171012
089415171012     c                   eval      %subst(§deawb:1:5)=§PNDSSORT
089416171012     c                   eval      %subst(§deawb:6:3)=§PNDBUCA
089417171012
089418171012     c                   clear                   §dexx2
089419171012     c                   eval      %subst(§deXX2:1:15)=dpdtel
089420171012     c                   eval      %subst(§deXX2:16:18)=§PNDDSRVX
089421171012
089422171012     c                   eval      §deisa =§PNDISO2
089423171012
089424171012     c                   eval      §desia =§PNDDDPTC
089425171012
089426171012     c                   eval      %subst(§desip:1:1) =§PNDICBCD
089427171012     c                   eval      %subst(§desip:3:1) =§PNDICD
089428171012     c
089430171012     c
089431171020     c****               eval      %subst(tr6dta:1:5)  =§PNDDSORT
089432171020     c****               eval      %subst(tr6dta:6:5)  =§PNDoSORT
089433171020     c****               eval      %subst(tr6dta:11:5) =§PNDcSORT
089434171012     c* additional service code ... previsto ma per ora vuoto
089435171020     c****               eval      %subst(tr6dta:16:6) =§PNDasco
089436171020     c****               eval      %subst(tr6dta:22:35) =§PNDref
089437171020     c****               eval      %subst(tr6dta:57:16) =§PNDteld
089438171020     c****               eval      %subst(tr6dta:73:40) =§PNDdsi
089439171012     c                   endsl
089600060615     c
089700000203    1C                   ENDIF
089800971219     C                   ENDSR
089900990204     C**
090000971219     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
090100971219     C     ETIEST        BEGSR
090200971219     C**
090300971219     C                   MOVEL     '3A'          COD
090400971219     C                   MOVEL(P)  ARBCBO        KEY
090500971219     C     KTAB2         CHAIN     TABEL                              31
090600971219     C  N31TBLFLG        IFNE      ' '
090700971219     C                   SETON                                        31
090800971219     C                   END
090900971219     C**
091000971219     C                   MOVEL     TBLUNI        DS3A
091100971219     C                   MOVEL     §3ATB1        FLGTB1            1
091200971219     C**
091300971219    3C                   SELECT
091400971219     C     FLGTB1        WHENEQ    'A'
091500971219     C     §3AFCA        ANDEQ     0
091600971219     C                   MOVEL     'A'           §DEFCA
091700971219     C     FLGTB1        WHENEQ    'A'
091800971219     C     §3AFCA        ANDNE     0
091900971219     C                   MOVEL     'X'           §DEFCA
092000971219     C     §3AFCA        WHENNE    0
092100971219     C                   MOVEL     'S'           §DEFCA
092200971219    3C                   ENDSL
092300971219     C**
092400971219     C                   ENDSR
092500980324     C*--- IMPOSTO CAMPI SEGNAC.EUROEX DA PASSARE A PGM DI STAMPA ----*
092600980324     C     EUREXP        BEGSR
092700030414
092800980324     C                   MOVEL     'E'           WTRC
092900051013     C     KARS          CHAIN     FIARS01L                           31
093000980324    2C     *IN31         IFEQ      *OFF
093100980324     C* PASSO AL PGM DI STAMPA ETICHETTE IL N. SEGNACOLLO EUROEXPRESS
093200980324     C                   MOVEL     ARSNOT        §DEEUR
093300980324     C* SE IMPORT BARTOLINI METTO NEL CODICE PRODOTTO DELL'ETICHETTA
093400980324     C* ITALIA IL N.SEGNACOLLO EUROEXPRESS
093500020218    3C     LNPNTW        IFEQ      'EEX'
093600980330     C                   MOVEA     ARSNOT        W35
093700980330     C                   Z-ADD     1             BB                2 0
093800980330     C     ' '           LOOKUP    W35(BB)                                31
093900980330     C   31              MOVEA     *BLANKS       W35(BB)
094000980330     C                   MOVEA     W35(6)        §LDCDP
094100980324    3C                   ENDIF
094200980324    2C                   ENDIF
094300980324     C                   ENDSR
094400060615     c
094500020218      *----------------------------------------------------------------
094600020218      *--- IMPOSTO CAMPI SEGNACOLLO DA PASSARE A PGM DI STAMPA     ---*
094700020218      *---------------------------------------------------------------*
094800020218     c     sr_fedex      begsr
094900020218
095000020218     c                   eval      wtrc = 'F'
095100051013     c     kars          chain     fIars01l
095200051013     c                   if        %found(fIars01l) and arsnot <> *blanks
095300020218     c                   movel     arsnot        §ldcdp
095400020218     c                   endif
095500020218
095600020218     c                   endsr
095700980409     C**
095800980409     C*--- CONTROLLA SE E' SI NO DDT ---------------------------------*
095900980409     C     CTRDDT        BEGSR
096000980409     C                   SETOFF                                       28
096100031028     c                   Clear                   dsbl4a
096200980409     C                   MOVEL     'A'           WTRC
096300070419     C     KAR42         CHAIN(n)  FiAR401L                           31
096400070419     c*!n31              Movel     Ar4Not        §b4abm
096500070419     c  n31              Movel     Ar4Not        dsbl4a
096600031028     c  n31              Move      §b4abm        wbam              1
096700980409     C   31              MOVEL     'S'           WBAM
096800980409     C**
096900120309     C**   WBAM          IFEQ      'N'
097000120309     c**   wlnadstauto   andeq     *off
097100120309     C***                SETON                                        28
097200120309     C***                ENDIF
097300101206     c* Se porto assegnato non tassato non si può mai ristampare anche se
097400101206     c* lna con procedura gestione distinte automatiche  attiva
097500101206     c                   if        arbaca=' '
097600101206     C                   SETON                                        28
097700101206     c                   endif
097800980409     C** RICHIESTO SI DDT
097900980409     C     VIDDDT        IFEQ      'S'
098000980409     C     WBAM          ANDNE     'C'
098100980409     C     WBAM          ANDNE     'S'
098200031028     c     wbam          andne     'P'
098300031028     c     wbam          andne     'Q'
098400980409     C                   SETON                                        28
098500980409     C                   ENDIF
098600980409     C** RICHIESTO NO DDT
098700980409     C     VIDDDT        IFEQ      'N'
098800980409     C     WBAM          IFEQ      'C'
098900980409     C     WBAM          OREQ      'S'
099000031028     c     wbam          oreq      'P'
099100031028     c     wbam          oreq      'Q'
099200980409     C                   SETON                                        28
099300980409     C                   ENDIF
099400980409     C                   ENDIF
099500980409     C**
099600980409     C                   ENDSR
099700990204     C**
099800990204     C* PER RISTAMPA BOLLE CONTROLLO SE E' RISTAMPABILE --------------*
099900990204     C     CTRRIS        BEGSR
100000990204     C**
100100990204     C* VEDO SE BOLLA FRANCO O ASSEGNATO
100200990204    3C     ARBNRS        IFGT      0
100300990204     C                   MOVEL     '3A'          COD
100400990204     C                   MOVEL(P)  ARBCBO        KEY
100500990204     C     KTAB2         CHAIN     TABEL                              30
100600990204    4C     *IN30         IFEQ      *OFF
100700990204     C                   MOVEL     TBLUNI        DS3A
100800990204     C                   ELSE
100900990204     C                   CLEAR                   DS3A
101000990204    4C                   ENDIF
101100990204     C                   MOVEL     §3ATB1        W001A             1
101200990204     C* ASSEGNATO
101300990204    4C     W001A         IFEQ      'A'
101400990204     C                   MOVEL(P)  ARBCCM        KEY
101500990204     C                   ELSE
101600990204     C* FRANCO
101700990204     C                   MOVEL(P)  ARBKSC        KEY
101800990204    4C                   ENDIF
101900990204     C**
102000990204     C                   MOVEL     '3C'          COD
102100990204     C     KTAB2         CHAIN     TABEL                              30
102200990204    4C     *IN30         IFEQ      *OFF
102300990204     C                   MOVEL     TBLUNI        DS3C
102400990204     C                   ELSE
102500990204     C                   CLEAR                   DS3C
102600990204    4C                   ENDIF
102700990204     C* NON RISTAMPABILE
102800990204    4C     §3CRSB        IFEQ      'N'
102900990204     C                   SETON                                        28
103000990204    4C                   ENDIF
103100990204    3C                   ENDIF
103200990204     C*
103300990204     C                   ENDSR
103400920422     C*
103500050418      *---------------------------------------------------------------*
103600050418      *  Verifica se spedizione "DI VALORE"                           *
103700050418      *---------------------------------------------------------------*
103800050418     c     CtrSpeVal     BEGSR
103900050418      *
104000050418if  1c                   if        NOT %open(FIAR501L)
104100050418     c                   open      FIAR501L
104200050418e   1c                   endif
104300050418      *
104400050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
104500050418if  1c                   if            ARBaas = AR5aas
104600050418     c                             and ARBlnp = AR5lnp
104700050418     c                             and ARBnrs = AR5nrs
104800050418     c                             and ARBnsp = AR5nsp
104900050418     c                   leavesr
105000050418e   1c                   endif
105100050418      *
105200050418      * Reperisce i dati da FIAR5
105300050418     c                   clear                   dAR5gen
105400050418     c     K05AR501      setgt     FIAR5000
105500050418do  1c                   dou            %eof(FIAR501L)
105600050418     c                             or   AR5atb =  *blanks
105700050418     c     K05AR501      readpe    FIAR5000
105800050418e   1c                   enddo
105900050418if  1c                   if             NOT %eof(FIAR501L)
106000050418     c                             and  AR5atb =  *blanks
106100050418     c                   movel     AR5uni        dAR5gen
106200050418e   1c                   endif
106300050418      *
106400050418     c                   ENDSR
106500050418      *
106600970715     OFNLV22P   T    LRNLR
