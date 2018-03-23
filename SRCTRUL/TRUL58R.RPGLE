000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200111020     H* trul58R *----------------------------------------------------*
000300111021     H*         - Aggiornamento Data Ritiro merce su bolle  - Utility
000400000000     H*--------------------------------------------------------------*
000500111020     Ftrul58d   CF   E             WORKSTN
000600910207     FAZORG01L  IF   E           K DISK
000700110704     Ffnorm01l  IF   E           K DISK
000800980130     FTABEL00F  IF   E           K DISK
000900021203     FFIAPD01L  IF   E           K DISK
001000910205     FCNACO00F  IF   E           K DISK
001100081203     FFNBLP70L  IF   E           K DISK    infds(fnblpinf)
001200081203     FFNBLP01L  UF   E           K DISK    rename(fnblp000:fnblp001)
001300110705     f                                     infds(blpnrr01)
001400111021     FFNarb01L  UF   E           K DISK
001500111021      * Bolle sede
001600111021     fTitas30c  uf   e           k Disk    UsrOpn extfile(wtas30)
001700111021
001800080331     FfIAR401L  UF   E           K DISK
001900111024     FFiar404L  IF   E           K DISK    RENAME(Fiar4000:Fiar404)
002000111024     Ffiftt01l  uf a e           k disk
002100111024     F                                     RENAME(FIFTT000:FIFTT001)
002200111021     f                                     infds(fttinfds)
002300111024     Ffifst01l  uf a e           k disk
002400111021     F                                     RENAME(FIFTT000:FIFST001)
002500111021     f                                     infds(fstinfds)
002600111021     Ffiftd05l  uf   e           k disk
002700111021     f                                      infds(ftdinfds)
002800111021     Ffifsd05l  uf   e           k disk
002900111021     F                                     RENAME(FIFTd000:FIFsd000)
003000111021     f                                      infds(fsdinfds)
003100020610
003200111026     D MSG             S             78    DIM(17) CTDATA PERRCD(1)
003300980202     D K31             S              1    DIM(31)
003400020610
003500020610     d lnp             s              3  0
003600020610     d xx              s              2  0
003700111018     d zz              s              2  0 inz
003800030619      *
003900060405     d data_eur        s               D   datfmt(*eur)
004000070612     d Dataiso         s               d   datfmt(*iso)
004100021203      *
004200111018     d
004300021203      * Ds esterna per ricerca codice padroncino
004400021203     D FNLV24DS      E DS
004500070612     d
004600070903     D og143         E DS
004700111021
004800111021     d fnlva7ds      e ds
004900111021
005000111021     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
005100111021     D PARAM3          DS
005200111021     D  PA3PDR                 1      7  0
005300111021     D  PA3TSR                 8      8
005400111021     D  PA3NDC                 9     15  0
005500111021     D  PA3FCT                16     16
005600111021     D  PA3FVD                17     17
005700111021     D  PA3FVT                18     18
005800111021     D  PA3DDC                19     26  0
005900111021     D  PA3sml               256    256
006000111021
006100081211     d
006200110704     D                 DS
006300110704     D  V1CPOE                 1      3  0
006400110704     D  V1CNSR                 4      5  0
006500110704     D  V1CNOR                 6     12  0
006600110704     D  V1CNRV                13     14  0
006700110704     D  SEL4                   1     14  0
006800081211     D***
006900941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
007000980202     D WGIDAT          DS                  INZ
007100980202     D  GIODAT                 1      8  0
007200980202     D  GIOINV                 9     16  0
007300980202     D  GIOTGI                17     21  0
007400941227     D***
007500941227     D WLBDAT          DS                  INZ
007600941227     D  G02DAT                 1      8  0
007700941227     D  G02INV                 9     16  0
007800941227     D  G02ERR                17     17
007900941227     D  G02TGI                18     22  0
008000060713     D                 DS
008100110704     D  blpaas                 1      4  0
008200110704     D  blpmgs                 5      8  0
008300110704     D  blpdsp                 1      8  0
008400060403
008500941116     D KPJBA         E DS
008600910205     D TCUDS           DS
008700910205     D  F1                     1      1
008800910205     D  F3                     3      3
008900910205     D  F2                     2      2
009000910205     D  F4                     4      4
009100910205     D  F56                    5      6
009200111020     D CNCR80        E DS
009300941227     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009400000000     D  TCU                  398    697
009500941116     D                                     DIM(50)
009600000000     D  KCU                  698    847P 0
009700000000     D                                     DIM(50)
009800000000     D                                     PACKEVEN
009900030619      *
010000081203     D fnblpinf        ds
010100110705     D  fnblpnrri            397    400i 0
010200110705     D blpnrr01        ds
010300110705     D  fnblpnrr01           397    400i 0
010400111021     d fttinfds        ds
010500111021     d  fttnrr               397    400i 0
010600111021     d fstinfds        ds
010700111021     d  fstnrr               397    400i 0
010800111021     d ftdinfds        ds
010900111021     d  ftdnrr               397    400i 0
011000111021     d fsdinfds        ds
011100111021     d  fsdnrr               397    400i 0
011200081203      * DS Esterna x gestione allocazione ed invio messaggi
011300081203     D TRUL82DS      E DS
011400980130     D DS3A          E DS
011500090427     D DS1B          E DS
011600021203
011700021203     D kpdpdr          S                   LIKE(APDpdr)
011800021203     D kpdtip          S                   LIKE(APDtip)
011900110704     D ktrcM           S                   LIKE(ar4trc) inz('M')
012000090126     D ntwlnp          S              1    inz
012100111024     D conteggi        S              1    inz
012200111026     D Aggior          S              1    inz
012300060926     D YY              S              3  0 inz
012400111021     D w0030           S              3  0 inz
012500111018     d O_testo         s            132    inz
012600111021     d w01tsr          s                   like(ftttsr)
012700111021     d w01fvl          s                   like(fttfvl)
012800111021     d w_ndc           s                   like(fttndc)
012900111021     d
013000111021     d WTAS30          s             21    inz('GAITRAGRU /TITAS30C')
013100111021     d
013200111021     D msgjob          C                   CONST('Si sta bloccando la manutenzi-
013300111021     D                                     one data spedizione:USCIRE un moment-
013400111021     D                                     o da questo lavoro')
013500111021     d
013600111021     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
013700111021     ITITAS000      22
013800111021     ITITAS010      23
013900910412     C****************************************************************
014000910412     C*  RIEPILOGO INDICATORI
014100910412     C***************************************************************
014200070612     C* 01    - possibilità di richidere bolle in sospensione x colli non
014300070612     c*         spuntati
014400070612     C* 02    -
014500070612     C* 03    - per colli non spuntati possibilità di modificare la consegna
014600070612     c*         richiesta da tabella CLI
014700910412     C* 30    - DI COMODO
014800980129     C* 40-50 - ERRORE
014900980130     C* 51-55 - GESTIONE CHIAVE LETTURA BOLLE
015000070612     C* 70    - posizionam.cursone su DCR
015100070612     C* 90    - ERRORE GENERICO
015200910412     C*****************************************************************
015300941116     C**
015400980130     C     *ENTRY        PLIST
015500980130     C                   PARM                    KPJBA
015600991206     C*
015700950411     C     INIZIO        TAG
015800980130     C                   EXSR      PUL01
015900950411     C*
016000980130     C                   MOVE      WDTGIO        WAAS              4 0
016100950411     C**
016200900509     C     FOR01         TAG
016300111020     C                   EXFMT     ul58d01
016400941227     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
016500110704     C                   SETOFF                                       902873
016600110704     C                   SETOFF                                       707172
016700111021     C                   SETOFF                                       747576
016800111021     C                   SETOFF                                       7760
016900081210     c                   clear                   v1cmsg
017000111020     C** F3 - FINE LAVORO
017100900509     C   KC              GOTO      FINE
017200910205     C* CONTROLLI FORMATO
017300980130     C                   EXSR      CNT01
017400111026     C   90              GOTO      FOR01
017500111026     c* se non ho premuto f6 verifico quante bolle si aggiornerebbero
017600111026     c
017700111026     c                   if        not *inkf
017800111026     c                   eval      Aggior ='N'
017900111026     c
018000111026     C                   EXSR      MOD01
018100111026     C                   GOTO      FOR01
018200111026     c                   endif
018300900523     C*
018400111026     C* F6 - M O D I F I C A   D A T A   S P E D I Z I O N E
018500111026     c                   if        *inkf
018600111026     c                   eval      Aggior ='S'
018700111026     c
018800111026     C                   EXSR      MOD01
018900111026     c                   endif
019000081211
019100910207     C*
019200950411     C                   GOTO      INIZIO
019300000000     C     FINE          TAG
019400111021     c* chiusura pgm
019500111021     C                   MOVEL     'C'           SA2TLA
019600111021     C                   CALL      'FNLSA2R3'
019700111021     C                   PARM                    SA2TLA
019800111021     C                   PARM                    SA2TRC
019900111021     C                   PARM      00000000      SA2DTS
020000111021     C                   PARM      000           WWLNA             3 0
020100111021
020200000000     C                   SETON                                        LR
020300941116     C*
020400980129     C*****************************************************************
020500910205     C* CONTROLLO FORMATO -------------------------------------------
020600980129     C*****************************************************************
020700980130     C     CNT01         BEGSR
020800910205     C                   SETOFF                                       90
020900051118     c
021000980129     C* CODICE CLIENTE
021100980130    1C     V1CKSC        IFEQ      0
021200980130    2C     V1DKSC        IFNE      *BLANKS
021300980129     C* PARSTA = 9  ESCLUDO ANNULLATI
021400980129     C                   Z-ADD     9             PARSTA
021500111020     C                   MOVEL     ragut         PARDUT           30
021600980129     C                   MOVEL     *BLANKS       DESCR            48
021700980130     C                   MOVEL     V1DKSC        DESCR
021800980129     C                   Z-ADD     KCI           CCC               4 0
021900981109     C                   CLEAR                   PARFLR
022000981109     C                   CLEAR                   PARDIT
022100000907     C                   Z-ADD     1             PAXNUM
022200000907     C                   CALL      'XALFA3BR'
022300980129     C                   PARM                    PARDUT
022400980129     C                   PARM                    CODUT
022500980129     C                   PARM                    DESCR
022600980129     C                   PARM                    CCC
022700980129     C                   PARM                    PARSTA            1 0
022800981109     C                   PARM                    PARFLR           90
022900981109     C                   PARM                    PARDIT            3
023000000907     C                   PARM                    PAXNUM            2 0
023100000907     C                   PARM                    PAXKCM           80
023200000907     C                   PARM                    PAXKSM          140
023300000907     C                   PARM                    PAXKDM           60
023400980129     C     PARSTA        IFNE      -1
023500000907     C                   MOVEL     PAXKSM        V1CKSC
023600980130     C                   MOVEL     DESCR         V1DKSC
023700980129     C                   ELSE
023800980202     C                   CLEAR                   V1DKSC
023900980129     C                   ENDIF
024000980129     C*
024100980129     C                   SETON                                        90
024200980129     C                   GOTO      ENDCTR
024300980129    2C                   ENDIF
024400980129   X1C                   ELSE
024500980129     C*
024600980129     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
024700980204     C                   Z-ADD     KCI           CCC
024800061003     c                   mllzo     1             v1cksc
024900980129     C     KACO          CHAIN     CNACO000                           41
025000980129    2C  N41ACOFLG        IFNE      ' '
025100980129     C                   SETON                                        41
025200980129    2C                   ENDIF
025300980129     C     *IN41         IFEQ      *OFF
025400980202     C                   MOVEL     ACORAG        V1DKSC
025500980129     C                   ELSE
025600980204     C                   CLEAR                   V1DKSC
025700980129     C                   SETON                                        90
025800980129     C                   GOTO      ENDCTR
025900980129     C                   END
026000980129    1C                   ENDIF
026100111020     c
026200111020     c* Data spedizione
026300111020     c                   clear                   wdspb
026400111021    1c                   if        v1cdspb>0
026500111020     C                   Z-ADD     V1cdspb       G02DAT
026600111020     C                   MOVEL     *BLANK        G02ERR
026700111020     C                   CALL      'XSRDA8'
026800111020     C                   PARM                    WLBDAT
026900111020     C* ERRATA
027000111021    2C     G02ERR        IFEQ      '1'
027100111020     C                   MOVEL     MSG(6)        V1CMSG
027200111020     C                   SETON                                        287590
027300111020     C                   GOTO      ENDCTR
027400111021    2C                   ENDIF
027500111020     C*
027600111020     C                   Z-ADD     G02DAT        V1cdspb
027700111020     C                   Z-ADD     G02inv        wdspb             8 0
027800111021     c
027900111021     c* La data deve essere compresa degli ultimi 10 giorni
028000111021    2c                   if        wdspb<datameno
028100111021     C                   MOVEL     MSG(9)        V1CMSG
028200111021     C                   SETON                                        287590
028300111021     C                   GOTO      ENDCTR
028400111021    2C                   ENDIF
028500111021     c                   if        wdspb>datapiu
028600111021     C                   MOVEL     MSG(10)       V1CMSG
028700111021     C                   SETON                                        287590
028800111021     C                   GOTO      ENDCTR
028900111021    2C                   ENDIF
029000111021    1c                   endif
029100980130     C*
029200111020     C* CONTROLLO Codice AUTOTRASPORTATORE
029300061003     c                   clear                   wcpdr
029400980130    1C     V1CPDR        IFNE      *BLANKS
029500980130     C     V1CPDR        ANDNE     *ZEROS
029600980130     C                   CLEAR                   V1DPDR
029700980130     C*
029800980130     C     '?'           SCAN      V1CPDR                                 30
029900980130     C*
030000980130     C     *IN30         IFEQ      *ON
030100021203      *
030200021203      * imposto i campi della ds esterna fnlv24ds
030300021203     C                   clear                   fnlv24ds
030400021203     C                   eval      d24fil   = v1clnp
030500021203     C                   eval      d24tip   = 'A'
030600021203     C                   eval      d24flg   = 'R'
030700021203     C                   movel     fnlv24ds      kpjbu
030800980130     C                   CALL      'FNLV24R'
030900980130     C                   PARM                    KPJBA
031000021203      *
031100021203      * valorizzo i campi a video
031200021203     C                   movel     kpjbu         fnlv24ds
031300021203     C                   move      d24pdr        v1cpdr
031400021203     C                   movel     d24rsc        v1dpdr
031500980130     C                   GOTO      ENDCTR
031600980130     C                   END
031700980130     C* ESEGUO TESTN
031800980130     C                   TESTN                   V1CPDR               30
031900980130     C                   MOVE      V1CPDR        W001A             1
032000980130     C     *IN30         IFEQ      *OFF
032100980130     C     W001A         ORLT      '0'
032200980205     C                   CLEAR                   V1CPDR
032300980205     C                   CLEAR                   V1DPDR
032400980130     C                   GOTO      ENDCTR
032500980130     C                   ENDIF
032600980130     C*
032700021204     C                   CLEAR                   V1DPDR
032800980130     C                   MOVE      V1CPDR        WCPDR             7 0
032900021203     C*    WCPDR         CHAIN     FNAPD000                           30
033000021204     C                   move      wcpdr         kpdpdr
033100021203     C                   move      'A'           kpdtip
033200021203     C     kfapd         chain     fiapd01l                           30
033300980130     C     *IN30         IFEQ      *ON
033400980205     C                   CLEAR                   WCPDR
033500980205     C                   CLEAR                   V1DPDR
033600980130     C                   SETON                                        4590
033700980130     C                   GOTO      ENDCTR
033800980204     C                   CLEAR                   V1DPDR
033900980204     C                   ELSE
034000980204     C                   MOVEL     APDRSC        V1DPDR
034100980130     C                   ENDIF
034200980130     C*
034300980205     C* RECUPERO LA LINEA DI PARTENZA DALLA PRIMA PARTE DELL'AUTOTRASPORTATORE
034400980130     C     V1CLNP        IFEQ      *ZEROS
034500980202     C                   MOVEL     WCPDR         V1CLNP
034600980205     C                   ELSE
034700111020     C* CONTROLLO LA LINEA DI PARTENZA CON QUELLA DELL'AUTOTRASPORATTORE
034800980205     C                   MOVEL     WCPDR         W0030
034900980205     C     V1CLNP        IFNE      W0030
035000980205     C     W0030         ANDNE     *ZEROS
035100980205     C                   SETON                                        56  90
035200980205     C                   Z-ADD     *ZEROS        W0030
035300980205     C                   GOTO      ENDCTR
035400980205     C                   ENDIF
035500980202     C                   ENDIF
035600111020     c
035700980205     C                   ENDIF
035800111020     C* LINEA DI PARTENZA
035900111020     c                   mllzo     1             v1clnp
036000111020     c                   clear                   ntwlnp
036100111020     C*
036200111020     c                   clear                   og143
036300111020     C     V1CLNP        CHAIN     AZORG01L                           30
036400111020     C   30              SETON                                        9042
036500111020     C   30              CLEAR                   V1DLNP
036600111020     C   30              GOTO      ENDCTR
036700111020     C  N30              MOVEL     ORGDES        V1DLNP
036800111020     c  n30              movel     orgde3        og143
036900111020     c                   if        §ogntw='DPD' or §ogntw='EEX' or
037000111020     c                             §ogntw='FED'
037100111020     c                   eval      ntwlnp='E'
037200111020     c                   endif
037300051118
037400110704     c* controllo numero ORM
037500111020    1c                   if        sel4>0
037600110704      * P.O. EMISSIONE
037700110704     C                   Z-ADD     V1CPOE        NUM3              3 0
037800110704     C     NUM3          CHAIN     AZORG01L                           30
037900110704     C     *IN30         IFEQ      *ON
038000110704    2C     ORGFVA        orne      *BLANKS
038100110704    2C     ORGFAG        ORNE      'F'
038200110704     C     ORGFAG        ANDNE     'A'
038300110704     C                   MOVEL     MSG(12)       V1CMSG
038400110704     C                   SETON                                        287290
038500110704     C                   GOTO      ENDCTR
038600110704     C                   ENDIF
038700110704      * N.ORM
038800110704     C     KFORM         CHAIN     FNORM01L                           30
038900110704     C     *IN30         IFEQ      *ON
039000110704     C                   MOVEL     MSG(13)       V1CMSG
039100110704     C                   SETON                                        287390
039200110704     C                   GOTO      ENDCTR
039300110704     C                   ENDIF
039400110704      *
039500110704     C                   SETON                                        06
039600110704      *
039700111020    1C                   ENDIF
039800111020     c
039900111020     c                   clear                   wdbr
040000111020     c                   if        v1cdbr>0
040100111020     C                   Z-ADD     V1cdbr        G02DAT
040200111020     C                   MOVEL     *BLANK        G02ERR
040300111020     C                   CALL      'XSRDA8'
040400111020     C                   PARM                    WLBDAT
040500111020     C* ERRATA
040600111020    1C     G02ERR        IFEQ      '1'
040700111020     C                   MOVEL     MSG(16)       V1CMSG
040800111020     C                   SETON                                        287490
040900111020     C                   GOTO      ENDCTR
041000111020    1C                   ENDIF
041100111020     C*
041200111020     C                   Z-ADD     G02DAT        V1cdbr
041300111020     C                   Z-ADD     G02inv        wdbr              8 0
041400111020     c                   endif
041500111020     c
041600111020     c* data spedizione o data borderò ci devono essere sempre
041700111021     c                   if        v1cdspb=0
041800111020     C                   MOVEL     MSG(7)        V1CMSG
041900111020     C                   SETON                                        287590
042000111020     C                   GOTO      ENDCTR
042100111020     c                   endif
042200111020     c* Accetto solo lnp solo se import altrimenti voglio altra selezione
042300111020     c
042400111020     c* Se immessa solo lnp, immettere anche cliente aut o ORM
042500111020     c                   if        v1clnp>0 and  ntwlnp<>'E'  and
042600111020     c                              v1cksc=0 and
042700111020     c                              v1cpdr<=*zeros and sel4=0
042800111020     C                   MOVEL     MSG(8)        V1CMSG
042900111020     C                   SETON                                        286090
043000111020     C                   GOTO      ENDCTR
043100111020     C                   ENDIF
043200111020     C*
043300111020     C* CONTROLLO DATA ritiro     DA MODIFICARE ----------------------*
043400980130     C*
043500111021     C                   Z-ADD     v10drt        G02DAT
043600980130     C                   MOVEL     *BLANK        G02ERR
043700980130     C                   CALL      'XSRDA8'
043800980130     C                   PARM                    WLBDAT
043900980130     C* ERRATA
044000980130    1C     G02ERR        IFEQ      '1'
044100980130     C                   SETON                                        46  90
044200980130     C                   GOTO      ENDCTR
044300980130    1C                   ENDIF
044400980130     C*
044500980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
044600980130     C*  IMMESSO DA VIDEO
044700111021     C                   Z-ADD     G02DAT        v10drt
044800980130     C*
044900980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
045000111021     C                   Z-ADD     G02INV        wolddrt           8 0
045100111021     c* degli ultimi 10 gg
045200111021     c                   if        wolddrt <datameno
045300111021     C                   MOVEL     MSG(9)        V1CMSG
045400111021     C                   SETON                                        287690
045500111021     C                   GOTO      ENDCTR
045600111021    1C                   ENDIF
045700111021     c                   if        wolddrt >datapiu
045800111021     C                   MOVEL     MSG(10)       V1CMSG
045900111021     C                   SETON                                        287690
046000111021     C                   GOTO      ENDCTR
046100111021    1C                   ENDIF
046200980130     C*
046300111020     C* CONTROLLO DATA ritiro     CHE SOSTITUISCE LA VECCHIA----------*
046400980130     C*
046500111021     C                   Z-ADD     v11drt        G02DAT
046600980130     C                   MOVEL     *BLANK        G02ERR
046700980130     C                   CALL      'XSRDA8'
046800980130     C                   PARM                    WLBDAT
046900980130     C* ERRATA
047000980130    1C     G02ERR        IFEQ      '1'
047100980130     C                   SETON                                        47  90
047200980130     C                   GOTO      ENDCTR
047300980130    1C                   ENDIF
047400980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
047500980130     C*  IMMESSO DA VIDEO
047600111021     C                   Z-ADD     G02DAT        v11drt
047700111020     C* CONTROLLO CHE LE DUE DATE non SIANO UGUALI
047800111021     C     v11drt        IFEQ      v10drt
047900980204     C                   SETON                                        5090
048000980202     C                   GOTO      ENDCTR
048100980202     C                   ENDIF
048200111021     c
048300111021     C                   MOVE      v11drt        WAASN             4 0
048400980130     C*
048500980130    2C     WAASN         IFNE      WAAS
048600980130     C                   SETON                                        4790
048700980130     C                   GOTO      ENDCTR
048800980130    2C                   ENDIF
048900980130     C*
049000111021     C                   Z-ADD     G02INV        wnewdrt           8 0
049100111021     c
049200111021     c                   if        wnewdrt <datameno
049300111021     C                   MOVEL     MSG(9)        V1CMSG
049400111021     C                   SETON                                        287790
049500111021     C                   GOTO      ENDCTR
049600111021    1C                   ENDIF
049700111021     c                   if        wnewdrt >datapiu
049800111021     C                   MOVEL     MSG(10)       V1CMSG
049900111021     C                   SETON                                        287790
050000111021     C                   GOTO      ENDCTR
050100111021    1C                   ENDIF
050200980130     C*
050300980130     C     ENDCTR        ENDSR
050400980130     C*****************************************************************
050500980130     C* ELABORAZIONE TESTATA E DETTAGLIO BOLLE ----------------------
050600980130     C*****************************************************************
050700980130     C     MOD01         BEGSR
050800111026     c                   clear                   bollema           9 0
050900111021     c
051000111021     c* Apro file titas per aggiornamento
051100111021     c                   if        %subst(knsif:7:1)='P'
051200111021     c                   eval      %subst(WTAS30:7:4)='GRPS'
051300111021     c                   endif
051400111021     c                   open      titas30c
051500980130     C*
051600980130     C                   SETOFF                                       515253
051700110704     C                   SETOFF                                       5455
051800111021     c                   clear                   Rielastat         1
051900980130     C* CONTROLLO CHE TIPO DI SELEZIONE E' STATA FATTA
052000980130     C* PER CLIENTE
052100980130     C     V1CKSC        COMP      *ZEROS                             5151
052200980130     C* PER LINEA PARTENZA
052300980130     C     V1CLNP        COMP      *ZEROS                             5252
052400980130     C* PER PADRONCINO
052500061003     C     WCPDR         COMP      *ZEROS                             55
052600060403     c
052700110704     c* se parzializzazione per numero ORM, leggo per ORM
052800110704     c*  che faccio prima
052900110704    0c                   if        sel4>0
053000110704     C                   MOVEL     SEL4          KN14
053100110704     C     Kar43         SETLL     Fiar404L
053200110704     C     Kar43         reade     Fiar404L
053300110704    1c                   dow       not %eof(fiar404l)
053400110704     c*
053500111021     c                   if        ar4lnp=v1clnp
053600110704
053700110704     c     kblpm         chain(n)  fnblp01l
053800110704
053900110704     c* Verifico anche la data spedizione
054000111021    3c                   if        blpdsp=wdspb
054100110704     c
054200110704     c                   exsr      Parzial
054300110704     c
054400111020    4c                   if        NoBolla=' '
054500110704     c
054600111026     c* Non devo aggiornare ma solo contare le bolle
054700111026     c                   if        Aggior='N'
054800111026     c                   add       1             bollema           9 0
054900111026     c                   else
055000110704     c                   exsr      Aggiorna
055100111026     c                   endif
055200110704     c*
055300110704    4c                   endif
055400110704    3c                   endif
055500110704    2c                   endif
055600110704     c
055700110704     C     Kar43         reade     Fiar404L
055800110704    1c                   enddo
055900110704      *
056000110704   x0c                   else
056100020610
056200020610      * elaboro la sk delle linee da stampare
056300020610
056400111021     c                   eval      lnp = v1clnp
056500111021     C                   MOVEL     wdspb         WAAS              4 0
056600111021     C                   MOVE      wdspb         WMGS              4 0
056700980130     C*
056800980130     C* LETTURA TESTATE BOLLE
056900111021     C***  N52KBLP1         SETLL     FNBLP70L
057000111021     C
057100111021     C  N55KBLP1         SETLL     FNBLP70L
057200020610     C   55KBLP3         SETLL     FNBLP70L
057300110704     c
057400980203    1C                   DO        *HIVAL
057500111021     C***  N52KBLP1         READE     FNBLP70L                               30
057600111021     C
057700111021     C  N55KBLP1         READE     FNBLP70L                               30
057800020610     C   55KBLP3         READE     FNBLP70L                               30
057900980203     C   30              LEAVE
058000110704     c
058100110704     c                   exsr      Parzial
058200110704     c
058300110704     c                   if        NoBolla='N'
058400110704     c                   iter
058500110704     c                   endif
058600110704     c
058700111026     c* Non devo aggiornare ma solo contare le bolle
058800111026     c                   if        Aggior='N'
058900111026     c                   add       1             bollema           9 0
059000111026     c                   else
059100110704     c                   exsr      Aggiorna
059200111026     c                   endif
059300980202     C*
059400980202    1C                   ENDDO
059500020610
059600110704    0c                   endif
059700111021
059800111026     c* Se aggiornata almeno una bolla -->
059900111021     c*    richiamo il pgm per la rielaborazione delle statistiche
060000111021     c                   if        Rielastat='S'
060100111021     c
060200111021     c                   z-add     500           st2ncl
060300111021     C                   MOVE      wolddrt       SA2DTS            8 0
060400111021     C                   MOVE      'P'           SA2TRC            1
060500111021     C                   exsr      sr_callsa2r3
060600111021     C                   MOVE      'T'           SA2TRC            1
060700111021     C                   exsr      sr_callsa2r3
060800111021     c
060900111021     C                   MOVE      wnewdrt       SA2DTS            8 0
061000111021     C                   MOVE      'P'           SA2TRC            1
061100111021     C                   exsr      sr_callsa2r3
061200111021     C                   MOVE      'T'           SA2TRC            1
061300111021     C                   exsr      sr_callsa2r3
061400111021     c                   endif
061500111018     c
061600060713     c* emetto msg di quante bolle manutenzionate
061700060713     c                   seton                                        28
061800111026     c                   if        Aggior='N'
061900111026     c                   movel     msg(17)       v1cmsg
062000111026     c                   else
062100081210     c                   movel     msg(5)        v1cmsg
062200111026     c                   endif
062300081211    2c                   if        bollema>0
062400081210     c                   eval      v1cmsg=%trim(v1cmsg)+' ' +%editc(bollema:'2')
062500081211   x2c                   else
062600081210     c                   eval      v1cmsg=%trim(v1cmsg)+' '
062700081210     c                             + %editw(bollema:'       0 ')
062800081211    2c                   endif
062900081210     c                   eval      v1cmsg=%trim(v1cmsg)+ ' Bolle.'
063000111021     c
063100111021     c                   close     titas30c
063200980130     C*
063300980202     C                   ENDSR
063400110704     C*----------------------------------------------------------------
063500110704     c     Parzial       BEGSR
063600110704     c                   clear                   Nobolla           1
063700110704      *
063800111020      *   CONTROLLO IL CODICE CLIENTE
063900110704     C     BLPCCM        IFNE      *ZEROS
064000110704     C                   Z-ADD     BLPCCM        WKSC              7 0
064100110704     C                   ELSE
064200110704     C                   Z-ADD     BLPKSC        WKSC
064300110704     C                   ENDIF
064400111020     c
064500110704    2C   51WKSC          IFNE      V1CKSC
064600110704     c                   eval      NoBolla='N'
064700110704     C                   leavesr
064800110704    2C                   END
064900110704     c*
065000110704      * SCARTO I PREPAGATI ANNULLATI
065100110704     c                   if        blpcbo='M '
065200110704     c                   eval      NoBolla='N'
065300110704     c                   leavesr
065400110704     c                   endif
065500110704      *
065600110704      ** CONTROLLO IL TIPO BOLLA
065700110704     C                   MOVEL     '3A'          COD
065800110704     C                   MOVEL(P)  BLPCBO        KEY
065900110704     C     KTAB2         CHAIN     TABEL00F                           31
066000110704     C   31              CLEAR                   DS3A
066100110704     C  N31              MOVEL     TBLUNI        DS3A
066200110704      *
066300110704      * SCARTO LE BOLLE DI RECUPERO
0664001107041   2C     §3ARBL        IFEQ      'R'
066500110704     c                   eval      NoBolla='N'
066600110704     C                   leavesr
066700110704    2C                   END
066800110704      * SCARTO LE BOLLE con cod trattamento merce che non prevede merce
066900110704     C                   MOVEL     '1B'          COD
067000110704     C                   MOVEL(P)  BLPctm        KEY
067100110704     C     KTAB2         CHAIN     TABEL00F                           31
067200110704     C   31              CLEAR                   DS1b
067300110704     C  N31              MOVEL     TBLUNI        DS1b
067400110704    2C                   IF        §1bfg8='N'
067500110704     c                   eval      NoBolla='N'
067600110704     C                   leavesr
067700110704    2C                   END
067800110704     c
067900111021     c* data ritiro = alla data vecchia
068000111021     c                   if        blpdrt<>wolddrt
068100111021     c                   eval      NoBolla='N'
068200111021     C                   leavesr
068300111021     c                   endif
068400111021     c
068500111021     c* Escludo bolle con ORM
068600111021     c     kar4m         chain     fiar401l
068700111021     c                   if        %found(fiar401l)
068800111021     c                   eval      NoBolla='N'
068900111021     C                   leavesr
069000111021     c                   endif
069100111021     c
069200110704     c                   ENDSR
069300110704     c*------------------------------------------------------------------------
069400110704     c     Aggiorna      BEGSR
069500110704     c* A questo punto visto che sono stati superati tutti i controlli
069600110704     c* chaino il record bolla in update
069700111021     c     kbolla        chain(E)  fnblp01l
069800111021     c
069900110704    1c                   if        %error
070000110704     C                   CLEAR                   TRUL82DS
070100110705     c                   if        sel4=0
070200110704     c                   eval      ul82§rrn = fnblpnrri
070300110705     c                   else
070400110705     c                   eval      ul82§rrn = fnblpnrr01
070500110705     c                   endif
070600110704     C                   EVAL      UL82§FIL = 'FNBLP00F'
070700110704     C                   EVAL      UL82§WIN = 'N'
070800110704     C                   EVAL      UL82§F7  = 'S'
070900110704     C                   EVAL      UL82§num = 1
071000110704     C                   EVAL      UL82§att = 30
071100110704     C                   EVAL      UL82§mss = msgjob
071200110704     C* Effettuo la chiamata al *PGM d utilità
071300110704     C                   CALL(e)   'TRUL82R'
071400110704     C                   PARM                    TRUL82DS
071500111021     C     kbolla        chain     fnblp01l
071600110704    1c                   endif
071700111020
071800111020    1c                   if        %found(fnblp01l)
071900110704      *
072000111021     C                   movel     wnewdrt       BLPdrt
072100111021     c
072200111021     c* Se borderizzata chain su titas per aggiornare anche TASDRT
072300111021    2c                   if        blpft2='T'
072400111021     c                   eval      Rielastat='S'
072500111021     c
072600111021     c                   setoff                                       2223
072700111021     C     kbolla        chain     titas30c
072800111021    3c                   if        %found(titas30c)
072900111021     C                   movel     wnewdrt       tasdrt
073000111021     c   22              except    agtas0
073100111021     c   23              except    agtas1
073200111021    3c                   endif
073300111021    c
073400111021      * Aggiorno i conteggi (richiamo due volte:la seconda per le simulazioni)
073500111024     c                   eval      w01tsr='R'
073600111024     c                   eval      conteggi='S'
073700111024     c
073800111024    2C     blpfpp        IFEQ      'M'
073900111024     C                   Z-ADD     1000          W_ndc
074000111024     C                   ELSE
074100111024     C                   Z-ADD     5000          W_ndc
074200111024    2C                   ENDIF
074300111024     C                   ADD       blpnrt        W_ndc
074400111021     c
074500111021     c                   setoff                                       17
074600111021    2c                   do        2
074700111021     c                   exsr      sr_aggaut
074800111021     c                   seton                                        17
074900111021    2c                   enddo
075000111021     c
075100111021     c* Ricalcolo affidabilità se bolla consegnata
075200111021     c                   exsr      sr_ricaff
075300111021    2c                   endif
075400111021     c*
075500111021     c                   except    aggblp
075600111021     c                   add       1             bollema           9 0
075700111021     c                   endif
075800110704     c
075900110704     c                   ENDSR
076000111021      *------------------------------------------------------------------------*
076100111021      * - Aggiornamento conteggi autotrasportatori
076200111021      *------------------------------------------------------------------------*
076300111021     c     Sr_AggAut     BegSr
076400111024   0ac                   if        conteggi='S'
076500111024     c* Prima veriifico che sia la nuova che la vecchia non siano già chiuse:
076600111024     c*  se chiuse non aggiorno i conteggi aut
076700111024     c
076800111024     c* VECCHIA DATA RITIRO
076900111024     c  n17kftt1old      chain(n)  fiftt01l                           32
077000111024     c   17kftt1old      chain(n)  fifst01l                           32
077100111024
077200111024    0c                   if        (not *in32  and fttfvl<>'C')
077300111024     c                             or  *in32
077400111024     c
077500111021     c* NUOVA DATA RITIRO
077600111024     c  n17kftt1         chain(E)  fiftt001                           32
077700111024     c   17kftt1         chain(E)  fifst001                           32
077800111024     c
077900111024     c* se errore ritento per una volta
078000111024    1c                   if        %error
078100111021     c* reperisco il RRN del file
078200111021     c  n17kftt1         chain(n)  fiftt001
078300111021     c     kftt1         chain(n)  fifst001
078400111021     C* Valorizzo la DS di procedura
078500111021     C                   CLEAR                   TRUL82DS
078600111021     c  n17              eval      ul82§rrn = fttnrr
078700111021     c   17              eval      ul82§rrn = fstnrr
078800111021     C  n17              EVAL      UL82§FIL = 'FIFTT00F'
078900111021     C   17              EVAL      UL82§FIL = 'FIFST00F'
079000111021     C                   EVAL      UL82§num = 1
079100111021     C                   EVAL      UL82§att = 2
079200111021     C* Effettuo la chiamata al *PGM d utilità
079300111021     C                   CALL(e)   'TRUL82R'
079400111021     C                   PARM                    TRUL82DS
079500111024     c  n17kftt1         chain     fiftt001                           32
079600111024     c   17kftt1         chain     fifst001                           32
079700111024    1c                   endif
079800111024     c
079900111024    1c                   if        (not *in32  and fttfvl<>'C')
080000111024     c                             or  *in32
080100111024     c
080200111021     c  n17kftd5         chain(E)  fiftd05l                           30
080300111021     c   17kftd5         chain(E)  fifsd05l                           30
080400111024    2c                   if        %error
080500111021     c* reperisco il RRN del file
080600111021     c  n17kftd5         chain(n)  fiftd05l
080700111021     c     kftd5         chain(n)  fifsd05l
080800111021     C* Valorizzo la DS di procedura
080900111021     C                   CLEAR                   TRUL82DS
081000111021     c  n17              eval      ul82§rrn = ftdnrr
081100111021     c   17              eval      ul82§rrn = fsdnrr
081200111021     C  n17              EVAL      UL82§FIL = 'FIFTD00F'
081300111021     C   17              EVAL      UL82§FIL = 'FIFSD00F'
081400111021     C                   EVAL      UL82§num = 1
081500111021     C                   EVAL      UL82§att = 2
081600111021     C* Effettuo la chiamata al *PGM d utilità
081700111021     C                   CALL(e)   'TRUL82R'
081800111021     C                   PARM                    TRUL82DS
081900111021     c  n17kftd5         chain     fiftd05l                           30
082000111021     c   17kftd5         chain     fifsd05l                           30
082100111024    2c                   endif
082200111021    2c                   if        *in30='0'
082300111021     c* numero di stop
082400111021     c                   clear                   fnlva7ds
082500111021     c                   eval      ia7pdr=ftdpdr
082600111021     c                   eval      ia7ndc=ftdndc
082700111021     c                   eval      ia7ddc=wnewdrt
082800111021     c                   eval      ia7ksc=ftdksc
082900111021     c                   eval      ia7rsm=ftdrsc
083000111021     c   17              eval      ia7sml='S'
083100111021     c                   call      'FNLVA7C'
083200111021     c                   parm                    kpjba
083300111021     c                   parm                    fnlva7ds
083400111021     c                   z-add     oa7stp        ftdstp
083500111021     c* data distinta
083600111021     c                   z-add     wnewdrt       ftdddc
083700111021     c  n17              update    fiftd000
083800111021     c   17              update    fifsd000
083900111021     c* numero spedizioni ritirate
084000111024    3c                   if        *in32
084100111024     c                   clear                   fiftt001
084200111024     c                   eval      fttset=1
084300111024     c                   eval      fttpdr=blppdr
084400111024     c                   movel     blppdr        fttfgs
084500111024     c                   eval      ftttsr=w01tsr
084600111024     c                   eval      fttndc=w_ndc
084700111024     c                   eval      fttddc=wnewdrt
084800111024     c                   eval      fttfpp=blpfpp
084900111024     C                   move      blppdr        kpdpdr
085000111024     C                   move      'A'           kpdtip
085100111024     C     kfapd         chain     fiapd01l
085200111024    4c                   if        %found(fiapd01l)
085300111024    5C     blpfpp        IFEQ      'M'
085400111024     C                   MOVEL     apdetm        FTTPEP
085500111024     c                   Movel     apdscm        FttLvl
085600111024     C                   ELSE
085700111024     C                   MOVEL     apdetp        FTTPEP
085800111024     c                   Movel     apdscp        FttLvl
085900111024    5c                   endif
086000111024    4c                   endif
086100111024    c
086200111024     c  n17              write     fiftt001
086300111024     c   17              write     fifst001
086400111024   x3c                   else
086500111024     c
086600111021     c                   eval      fttset=fttset+1
086700111021     c  n17              update    fiftt001
086800111021     c   17              update    fifst001
086900111024    3c                   endif
087000111024     c
087100111021     c* se conteggio valorizzato svalorizzo
087200111021    3C     fttfvl        ifeq      'V'
087300111021     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
087400111021     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
087500111021     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
087600111021     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
087700111021     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
087800111021     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
087900111021     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
088000111021     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
088100111021     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
088200111021     C                   MOVEL     PARAM3        KPJBU
088300111021     C  N17              CALL      'FICNE2R'
088400111021     C                   PARM                    KPJBA
088500111021     C   17              CALL      'FICNE9C'
088600111021     C                   PARM                    KPJBA
088700111021    3C                   endif
088800111021    2c                   endif
088900111021     c* VECCHIA DATA RITIRO
089000111024     c  n17kftt1old      chain(E)  fiftt01l                           32
089100111024     c   17kftt1old      chain(E)  fifst01l                           32
089200111024    2c                   if        %error
089300111021     c* reperisco il RRN del file
089400111021     c  n17kftt1old      chain(n)  fiftt001
089500111021     c     kftt1old      chain(n)  fifst001
089600111021     C* Valorizzo la DS di procedura
089700111021     C                   CLEAR                   TRUL82DS
089800111021     c  n17              eval      ul82§rrn = fttnrr
089900111021     c   17              eval      ul82§rrn = fstnrr
090000111021     C  n17              EVAL      UL82§FIL = 'FIFTT00F'
090100111021     C   17              EVAL      UL82§FIL = 'FIFST00F'
090200111021     C                   EVAL      UL82§num = 1
090300111021     C                   EVAL      UL82§att = 2
090400111021     C* Effettuo la chiamata al *PGM d utilità
090500111021     C                   CALL(e)   'TRUL82R'
090600111021     C                   PARM                    TRUL82DS
090700111024     c  n17kftt1old      chain     fiftt01l                           32
090800111024     c   17kftt1old      chain     fifst01l                           32
090900111024    2c                   endif
091000111024     c
091100111024    2c                   if        *in32='0'  and fttfvl<>'C'
091200111021     c                   select
091300111021     c*
091400111024    3c                   when      fttset-1=0 and ftttbs=0
091500111021     c  n17              delete    fiftt001
091600111021     c   17              delete    fifst001
091700111024    3c                   when      fttset-1>0 or fttset-1=0
091800111021     c                   eval      fttset=fttset-1
091900111021     c  n17              update    fiftt001
092000111021     c   17              update    fifst001
092100111021     c
092200111021     c* se conteggio valorizzato svalorizzo
092300111024    4C     fttfvl        ifeq      'V'
092400111021     C                   MOVEL     FTtPDR        PA3PDR                         *COD. PADRONCINO
092500111021     C                   MOVEL     FTtTSR        PA3TSR                         *TIPO PRESTAZIONE
092600111021     C                   Z-ADD     FTtNDC        PA3NDC                         *NUMERO DISTINTA
092700111021     C                   Z-ADD     FTtDDC        PA3DDC                         *DATA   DISTINTA
092800111021     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
092900111021     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
093000111021     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
093100111021     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
093200111021     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
093300111021     C                   MOVEL     PARAM3        KPJBU
093400111021     C  N17              CALL      'FICNE2R'
093500111021     C                   PARM                    KPJBA
093600111021     C   17              CALL      'FICNE9C'
093700111021     C                   PARM                    KPJBA
093800111024    4C                   endif
093900111024    3c                   endsl
094000111024    2c                   endif
094100111024     c
094200111024   x1c                   else
094300111024     c                   eval      conteggi='N'
094400111024    1c                   endif
094500111024   x0c                   else
094600111024     c                   eval      conteggi='N'
094700111024    0c                   endif
094800111024     c
094900111024     c  n17              unlock    fiftt01l
095000111024     c   17              unlock    fifst01l
095100111024   0ac                   endif
095200111024     c
095300111021     c                   endsr
095400111021      *------------------------------------------------------------------------*
095500111021      * - Aggiornamento bolle arrivi per rialcolo affidabilità
095600111021      *------------------------------------------------------------------------*
095700111021     c     Sr_RicAff     BegSr
095800111021     c     kbolla        chain(N)  fnarb01l
095900111021     c
096000111021     c                   if        %found(fnarb01l) and arbdcm>0
096100111021     c     kbolla        chain(E)  fnarb01l
096200111021     c
096300111021     c                   eval      arbfbs='S'
096400111021     c                   if        arbdce>0
096500111021     c                   clear                   arbdti
096600111021     c                   clear                   arbhti
096700111021     c                   endif
096800111021     c
096900111021     c                   update    fnarb000
097000111021     c                   endif
097100111021     c
097200111021     c                   endsr
097300980130     C*----------------------------------------------------------------
097400980130     C*
097500980130     C*--- OPERAZIONI INIZIALI ---------------------------------------*
097600980130     C     *INZSR        BEGSR
097700980130     C***
097800980130     C* KLIST
097900980130     C***
098000980130     C* ACCESSO   CNACO
098100980130     C     KACO          KLIST
098200980130     C                   KFLD                    CODUT
098300980130     C                   KFLD                    CCC
098400980130     C                   KFLD                    V1CKSC
098500111021     C     Kbolla        KLIST
098600980130     C                   KFLD                    BLPAAS
098700980130     C                   KFLD                    BLPLNP
098800980130     C                   KFLD                    BLPNRS
098900980130     C                   KFLD                    BLPNSP
099000110704     C* KEY PER AGGIIORNARE FLBLT
099100110704     C     KBLPM         KLIST
099200110704     C                   KFLD                    ar4AAS
099300110704     C                   KFLD                    ar4LNP
099400110704     C                   KFLD                    ar4NRS
099500110704     C                   KFLD                    ar4NSP
099600110704     C* KEY PER AGGIORNARE fiar4
099700080331     C     Kar4M         KLIST
099800080331     C                   KFLD                    BLPAAS
099900080331     C                   KFLD                    BLPLNP
100000080331     C                   KFLD                    BLPNRS
100100080331     C                   KFLD                    BLPNSP
100200110704     C                   KFLD                    ktrcM
100300111021     C* ACCESSO
100400980130     C     KBLP1         KLIST
100500020610     c                   kfld                    lnp
100600980130     C                   KFLD                    WAAS
100700980130     C                   KFLD                    WMGS
100800111021     C* ACCESSO   F
100900980130     C     KBLP3         KLIST
101000020610     c                   kfld                    lnp
101100980130     C                   KFLD                    WAAS
101200980130     C                   KFLD                    WMGS
101300980130     C                   KFLD                    WCPDR
101400980130     C* ACCESSO   TABEL
101500980130     C     KTAB2         KLIST
101600980130     C                   KFLD                    CODUT
101700980130     C                   KFLD                    COD               2
101800980130     C                   KFLD                    KEY               8
101900111020     c
102000021203     C     Kfapd         klist
102100021203     C                   kfld                    kpdtip
102200021203     C                   kfld                    kpdpdr
102300110704      *
102400110704     C     KFORM         KLIST
102500110704     C                   KFLD                    V1CPOE
102600110704     C                   KFLD                    V1CNSR
102700110704     C                   KFLD                    V1CNOR
102800110704     C                   KFLD                    V1CNRV
102900110704      *
103000110704     C     Kar43         KLIST
103100110704     C                   KFLD                    KTRCM
103200110704     C                   KFLD                    KN14             14
103300111021     C     kftt1         KLIST
103400111021     C                   KFLD                    blppdr
103500111021     C                   KFLD                    w01tsr
103600111021     C                   KFLD                    W_ndc
103700111021     C                   KFLD                    wnewdrt
103800111021     C     kftt1old      KLIST
103900111021     C                   KFLD                    blppdr
104000111021     C                   KFLD                    w01tsr
104100111021     C                   KFLD                    W_ndc
104200111021     C                   KFLD                    wolddrt
104300111021     c     kftd5         klist
104400111024     C                   KFLD                    blppdr
104500111024     C                   KFLD                    w01tsr
104600111024     C                   KFLD                    w_ndc
104700111021     C                   KFLD                    wolddrt
104800111021     c                   kfld                    blpaas
104900111021     c                   kfld                    blplnp
105000111021     c                   kfld                    blpnrs
105100111021     c                   kfld                    blpnsp
105200110704     C*
105300980130     C***
105400980130     C* DEFINIZIONE CAMPI
105500980130     C***
105600980130     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
105700980130     C***
105800980130     C                   CALL      'X§PARUT'
105900980130     C                   PARM                    UT§DSE
106000111020     C                   MOVEL     REC80         CNCR80
106100111020     c                   movel     ragut         rsut
106200980130     C*--- RICERCA CAPOCONTI
106300980130     C                   DO        50            X                 2 0
106400980130     C                   MOVE      TCU(X)        TCUDS
106500980130     C     F56           CABNE     'CG'          END1
106600980130     C     F4            COMP      '1'                                    21
106700980130     C     F4            COMP      '2'                                    22
106800980130     C     F4            COMP      '3'                                    23
106900980130     C     F4            COMP      '6'                                    27
107000980130     C** 1 CLIENTI   21
107100980130     C** 2 FORNITORI 22
107200980130     C** 3 AGENTI    23
107300980130     C     F3            COMP      '0'                                242425
107400980130     C     F3            COMP      'I'                                    26
107500980130     C** 0 ITALIA   25
107600980130     C** 1 ESTERO   24
107700980130     ** I CAPO CONTO IVA
107800980130     C   21
107900980130     CAN 24              Z-ADD     KCU(X)        KCE               4 0
108000980130     C   21
108100980130     CAN 25              Z-ADD     KCU(X)        KCI               4 0
108200980130     C   22
108300980130     CAN 24              Z-ADD     KCU(X)        KFE               4 0
108400980130     C   22
108500980130     CAN 25              Z-ADD     KCU(X)        KFI               4 0
108600980130     C   23
108700980130     CAN 24              Z-ADD     KCU(X)        KAE               4 0
108800980130     C   23
108900980130     CAN 25              Z-ADD     KCU(X)        KAI               4 0
109000980130     C   26              Z-ADD     KCU(X)        KIVA              4 0
109100980130     C   27              Z-ADD     KCU(X)        KBNA              4 0
109200980130     C     END1          TAG
109300980130     C                   END
109400980130     C                   SETOFF                                       212223
109500980130     C                   SETOFF                                       242526
109600980130     C                   SETOFF                                       27
109700980130     C*
109800111021     C* DATA ritiro entro 10 gg in meno o in più
109900980130     C                   TIME                    W0140            14 0
110000980130     C* UDATE IN GGMMAAAA
110100980130     C                   MOVE      W0140         WDTGIO            8 0
110200980130     C*
110300980130     C* UDATE IN AAAAMMGG
110400980130     C                   Z-ADD     WDTGIO        G02DAT
110500980130     C                   MOVEL     *BLANK        G02ERR
110600980130     C                   CALL      'XSRDA8'
110700980130     C                   PARM                    WLBDAT
110800980130     C                   MOVEL     G02INV        DATEU             8 0
110900111021     C     G02TGI        ADD       10            GIOTGI
111000111021     C                   CALL      'XSRGI8'
111100111021     C                   PARM                    WGIDAT
111200111021     C*
111300111021     C                   Z-ADD     GIOINV        DATApiu           8 0
111400111021     c
111500111021     C     G02TGI        sub       10            GIOTGI
111600111021     C                   CALL      'XSRGI8'
111700111021     C                   PARM                    WGIDAT
111800111021     C*
111900111021     C                   Z-ADD     GIOINV        DATAmeno          8 0
112000001120     C*
112100980130     C                   ENDSR
112200980130     C*****************************************************************
112300980130     C* PULIZIA   FORMATO -------------------------------------------
112400980130     C*****************************************************************
112500980130     C     PUL01         BEGSR
112600980130     C*
112700070614     c                   setoff                                       0103
112800980130     C                   MOVE      *ZEROS        V1CKSC
112900980130     C                   MOVE      *BLANKS       V1DKSC
113000980130     C                   MOVE      *BLANKS       V1CLNP
113100980130     C                   MOVE      *BLANKS       V1DLNP
113200980130     C                   MOVE      *BLANKS       V1CPDR
113300980130     C                   MOVE      *BLANKS       V1DPDR
113400111021     C                   Z-ADD     *ZEROS        v10drt
113500111021     C                   Z-ADD     *ZEROS        v11drt
113600111020     C                   Z-ADD     *ZEROS        V1cdspb
113700111020     c                   clear                   wdspb
113800111020     c                   clear                   wdbr
113900111020     c
114000111020     c                   clear                   v1cdbr
114100111020     c
114200060713     c                   clear                   bollema           9 0
114300110704     c                   clear                   v1cpoe
114400110704     c                   clear                   v1cnsr
114500110704     c                   clear                   v1cnor
114600110704     c                   clear                   v1cnrv
114700110704     c
114800980130     C                   ENDSR
114900111021      *--------------------------------------------------------------*
115000111021      * Richiamo pgm per rielaborazione statistiche partito e a terra
115100111021      *--------------------------------------------------------------*
115200111021     c     sr_callsa2r3  BegSr
115300111021     C                   CALL      'FNLSA2R3'
115400111021     C                   PARM                    SA2TLA            1
115500111021     C                   PARM                    SA2TRC
115600111021     C                   PARM                    SA2DTS
115700111021     C                   PARM                    WWLNA             3 0
115800111021     C                   PARM                    st2ncl            5 0
115900111021     c                   ENDSR
116000111021     c
116100111021      *--------------------------------------------------------------*
116200111021
116300111021     OFNBLP001  E            AGGBLP
116400111021     O                       BLPDRT
116500111021     OTITAS000  E            AGTAS0
116600111021     O                       TASDRT
116700111021     OTITAS010  E            AGTAS1
116800111021     O                       TASDRT
116900111018
117000060713**         MSG
117100111017MANUTENZIONE DATA SPED.BOLLE oltre xx giorni
117200111018Utente:  xxxxxxxxxx
117300060713Codice Cliente:  xxxxxxx   x
117400060713Bolle manutenzionate: xxx.xxx.xxx
117500081210Sono state manutenzionate
117600111020Data spedizione errata                                                         6
117700111021Data spedizione obbligatoria
117800111021Per lin.partenza Italia, indicare anche cliente o Aut                          8
117900111021Data di selezione troppo vecchia                                               9
118000111021Data troppo avanti                                                            10
118100090401Linea di partenza:  xxx
118200110704Immettere filiale di emissione valida                                         12
118300110704Immettere numero ORM esistente in archivio                                    13
118400111017Bolle immesse dal                                                             14
118500111020Immettere data borderizzazione solo se richieste bolle borderizzate           15
118600111020Data borderizzazione errata                                                   16
118700111026Verranno manutenzionate                                                       17
