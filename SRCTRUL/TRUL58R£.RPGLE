000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200980129     H* FNLS03R *----------------------------------------------------*
000300980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000400000000     H*--------------------------------------------------------------*
000500980129     FFNLS03D   CF   E             WORKSTN
000600060404     FFNLS03p   o    E             printer usropn  oflind(*in98)
000700910207     FAZORG01L  IF   E           K DISK
000800051212     Ftntbe02l  IF   E           K DISK
000900070117     FfNbrv07l  IF   E           K DISK
001000110704     Ffnorm01l  IF   E           K DISK
001100980130     FAZCAE01L  IF   E           K DISK
001200980202     FAZCLN01L  IF   E           K DISK
001300980130     FTABEL00F  IF   E           K DISK
001400021203     FFIAPD01L  IF   E           K DISK
001500910205     FCNACO00F  IF   E           K DISK
001600081203     FFNBLP70L  IF   E           K DISK    infds(fnblpinf)
001700081203     FFNBLP01L  UF   E           K DISK    rename(fnblp000:fnblp001)
001800110705     f                                     infds(blpnrr01)
001900980130     FFNBLT01L  UF   E           K DISK
002000010326     FFISGN05L  UF   E           K DISK
002100050318     FfIAR601L  UF   E           K DISK
002200080331     FfIAR401L  UF   E           K DISK
002300110704     FFiar404L  IF   E           K DISK    RENAME(Fiar4000:Fiar404)
002400050318     FfIAR701L  UF   E           K DISK
002500111018     FQSYSPRT   O    F  132        PRINTER OFLIND(*InOA)  usropn
002600111018
002700111018     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
002800020610
002900941227     D L1              S              3  0 DIM(30)
003000020610     d sklnp           s              3  0 dim(30)
003100020610
003200030619     d $Cmd            s             80    dim(3) ctdata perrcd(1)
003300060403     d $Cmd2           s             80    dim(1) ctdata perrcd(1)
003400061005     d $Cmd3           s             80    dim(1) ctdata perrcd(1)
003500061012     d DLTOVR          s             80    dim(1) ctdata perrcd(1)
003600111017     D MSG             S             78    DIM(14) CTDATA PERRCD(1)
003700980202     D K31             S              1    DIM(31)
003800980202     D*
003900980202     D KSS             S              7    DIM(12)
004000070612     D elabor          S             54    DIM(6)
004100111019     D invbo           S              5  0 DIM(51)
004200111019     D invcod          S             25    DIM(51)
004300111019     D invdim          S              8  0 DIM(51)
004400020610
004500020610     d lnp             s              3  0
004600020610     d xx              s              2  0
004700111018     d zz              s              2  0 inz
004800030619      *
004900030619     d Qlen            s             15  5 inz(150)
005000030619      *
005100030619     d Wfile           s             10    inz
005200030619     d Woutq           s             10    inz
005300030619     d Wform           s             10    inz
005400060405     d data_eur        s               D   datfmt(*eur)
005500070612     d Dataiso         s               d   datfmt(*iso)
005600070612     d woggidcgm1      s              8  0
005700070612     d woggidcgm2      s              8  0
005800070612     d s_woggidcg      s                   like(woggidcgm1)
005900070612     d s_vpodcg        s                   like(§vpodcgm1)
006000021203      *
006100111018
006200111018      // - Status
006300111018     d Psds           sds
006400111018     d   SDSpgm          *proc
006500111018     d   JobUser             254    263
006600111018
006700111018      // - Campi per QCMDEXC
006800111018     d Qcmd            s            512    inz
006900111018     d
007000111018      // - Esecuzione comando di sistema
007100111018     d qCmdExc         pr                  extpgm('QCMDEXC')
007200111018     d  Qcmd                        512    const  options(*varsize)
007300111018     d  Qlen                         15  5 const
007400111018
007500111018     d
007600021203      * Ds esterna per ricerca codice padroncino
007700021203     D FNLV24DS      E DS
007800051209     D* DS PER TIBS02R - GESTIONE TNTBE00F
007900051209     D TIBS02DS      E DS
008000111018      // - Tabella "MRA" = Bart-Mailing - Danni
008100111018     d dMRAdan       e ds                  inz
008200111018
008300070612     d
008400070612     D Dvpo          E DS
008500070903     D og143         E DS
008600081211     d
008700081211     dwidmsall         ds
008800081211     Dwidms1                         32
008900081211     Dwidms2                         32
009000081211     Dwidms3                         32
009100081211     Dwidms4                         32
009200110704     D                 DS
009300110704     D  V1CPOE                 1      3  0
009400110704     D  V1CNSR                 4      5  0
009500110704     D  V1CNOR                 6     12  0
009600110704     D  V1CNRV                13     14  0
009700110704     D  SEL4                   1     14  0
009800081211     D***
009900941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010000980202     D WGIDAT          DS                  INZ
010100980202     D  GIODAT                 1      8  0
010200980202     D  GIOINV                 9     16  0
010300980202     D  GIOTGI                17     21  0
010400941227     D***
010500941227     D WLBDAT          DS                  INZ
010600941227     D  G02DAT                 1      8  0
010700941227     D  G02INV                 9     16  0
010800941227     D  G02ERR                17     17
010900941227     D  G02TGI                18     22  0
011000980202     D                 DS
011100980202     D  WAA                    1      4  0
011200980202     D  WMM                    5      6  0
011300980202     D  WGG                    7      8  0
011400980202     D  WDATA                  1      8  0
011500060403
011600060403     d WDAT8           DS
011700060403     d  datada                 1      8  0
011800060403     d  dataa                  9     16  0
011900060403     d  ggl                   17     21  0
012000060713     D                 DS
012100110704     D  blpaas                 1      4  0
012200110704     D  blpmgs                 5      8  0
012300110704     D  blpdsp                 1      8  0
012400060926     d
012500060926     D                 DS
012600060926     D  v1cksc                 1      7  0
012700060926     D  v1clnp                 8     10  0
012800061003     D  wclna                 11     13  0
012900060926     D  v1ctfa                14     16  0
013000060926     D  v1cnos                17     17
013100060926     D  v1cggl                18     18  0
013200060926     D  v10dsp                19     26  0
013300060926     D  v11dsp                27     34  0
013400061012     D  Wcpdr                 35     41  0
013500070612     D  V1CTCR                42     42
013600070612     D  wdcr                  43     50  0
013700070612     D  V1CHCR                51     54  0
013800070612     D PARAM                   1     54
013900060403
014000941116     D KPJBA         E DS
014100910205     D TCUDS           DS
014200910205     D  F1                     1      1
014300910205     D  F3                     3      3
014400910205     D  F2                     2      2
014500910205     D  F4                     4      4
014600910205     D  F56                    5      6
014700941227     D CNCR80        E DS
014800941227     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
014900000000     D  TCU                  398    697
015000941116     D                                     DIM(50)
015100000000     D  KCU                  698    847P 0
015200000000     D                                     DIM(50)
015300000000     D                                     PACKEVEN
015400030619      * Parametri per stampa BOLLE ed ETICHETTE
015500030619     d TRUL90DS      e ds                  inz
015600030619     d   D90rse      e                     inz('N')
015700030619     d   D90rsb      e                     inz('S')
015800030619      * DS per pgm di STAMPA BOLLE
015900030619     d FNLSB5DS      e ds                  inz
016000081203      * Ds per pgm che mette in sospensione rinumerando la spedizione
016100081203     d FNLS65DS      e ds                  inz
016200030619      *
016300980202     D* DS PER TRUL06R - CARICAMENTO £X
016400941227     D DSUL06        E DS                  EXTNAME(TRUL06DS)
016500941227     D  LIN                    1     90  0
016600941227     D                                     DIM(30)
016700980129     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
016800980129     D DSLV55        E DS                  EXTNAME(FNLV55DS)
016900081203     D fnblpinf        ds
017000110705     D  fnblpnrri            397    400i 0
017100110705     D blpnrr01        ds
017200110705     D  fnblpnrr01           397    400i 0
017300081203      * DS Esterna x gestione allocazione ed invio messaggi
017400081203     D TRUL82DS      E DS
017500980130     D DS3A          E DS
017600090427     D DS1B          E DS
017700081203     D Ds3c          E DS
017800980202     D DS3I          E DS
017900051209     D DCLI          E DS
018000021203
018100021203     D kpdpdr          S                   LIKE(APDpdr)
018200021203     D kpdtip          S                   LIKE(APDtip)
018300051212     D kcod            S                   LIKE(tbecod) inz('CLI')
018400051212     D klin            S                   LIKE(tbelin) inz(' ')
018500051212     D ksif            S                   LIKE(tbesif)
018600110704     D ktrc            S                   LIKE(ar4trc)
018700110704     D ktrcM           S                   LIKE(ar4trc) inz('M')
018800081205     D wlpnsp          S                   LIKE(blpnsp)
018900081205     D wlpaas          S                   LIKE(blpaas)
019000081210     D wmsg            S                   LIKE(v1cmsg)
019100090126     D ntwlnp          S              1    inz
019200060926     D YY              S              3  0 inz
019300080620     D sosdaL1         S              1    inz
019400110704     D Indx            S              3  0 inz
019500111018     d O_testo         s            132    inz
019600060713     D* DEFINIZIONE COSTANTI
019700060404     D Ctitolo         C                   CONST('ELENCO SPEDIZIONI IN SOSPENSI-
019800060405     D                                     ONE DA OLTRE X GIORNI')
019900060713     D CUTI            C                   CONST('EDP*      ')
020000081203     D msgjob          C                   CONST('Si sta bloccando la manutenzi-
020100081203     D                                     one data spedizione:USCIRE un moment-
020200081203     D                                     o da questo lavoro')
020300081211     D ms2wdw          C                   CONST('Probabilmente in manutenzione-
020400081215     D                                        ')
020500081215     D ms3wdw          C                   CONST('da altro utente.             -
020600081211     D                                        ')
020700081215     D ms4wdw          C                   CONST('Rieseguire il lancio.        -
020800081211     D                                        ')
020900111018
021000111018      // - Comando di override al PrtF
021100111018     d C_CmdOvrPrtF    c                   const('OVRPRTF +
021200111018     d                                           file(PRTEMAIL) +
021300111018     d                                           pagesize(66 132) +
021400111018     d                                           lpi(6) cpi(10) +
021500111018     d                                           ovrscope(*actgrpdfn) +
021600111018     d                                           ')
021700111018     d C_CmdDltOvr     c                   const('DLTOVR +
021800111018     d                                            file(PRTEMAIL) +
021900111018     d                                            lvl(*actgrpdfn)')
022000111018      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
022100111018     d TRTCM1ds      e ds                  inz
022200111018      //   ·§CM1mitt = Indirizzo e-mail del mittente
022300111018     d   §CM1mitt    e                     inz('ced@bartolini.it')
022400111018      //   ·§CM1dst  = Indirizzo e-mail del destinatario
022500111018     d   §CM1dst     e                     inz('elisa.sanfelici@bartolini.it')
022600111018      //   ·§CM1tips = Tipo lettera via e-mail:
022700111018      //    "LET" = testo allegato in corpo con logo
022800111018      //            (richiede righe libere iniziali per il logo)
022900111018      //    "COR" = testo integrato senza logo
023000111018      //            (non consente né UNDERLINE né HIGHLIGHT)
023100111018     d   §CM1tips    e                     inz('COR')
023200111018      //   ·§CM1po   = Filiale
023300111018     d   §CM1po      e                     inz('046')
023400111018      //   ·§CM1var  = Oggetto e-mail
023500111018     d   §CM1var     e                     inz('*OBJM*+
023600111019     d                                     Manutenzione data spedizione+
023700111019     d                                      bolle ')
023800111018      //   ·§CM1sts  = Stato
023900111018     d   §CM1sts     e                     inz(*off)
024000111018      //   ·§CM1sts  = Id processo
024100111018     d   §CM1idp     e                     inz('2')
024200111018
024300111018
024400910412     C****************************************************************
024500910412     C*  RIEPILOGO INDICATORI
024600910412     C***************************************************************
024700070612     C* 01    - possibilità di richidere bolle in sospensione x colli non
024800070612     c*         spuntati
024900070612     C* 02    -
025000070612     C* 03    - per colli non spuntati possibilità di modificare la consegna
025100070612     c*         richiesta da tabella CLI
025200070612     C* 12    - PROTEZIONE CAMPO LINEA DI PARTENZA XCHE' SONO UN 2° LIVELLLO
025300910412     C* 30    - DI COMODO
025400980129     C* 40-50 - ERRORE
025500980130     C* 51-55 - GESTIONE CHIAVE LETTURA BOLLE
025600070612     C* 70    - posizionam.cursone su DCR
025700070612     C* 90    - ERRORE GENERICO
025800910412     C*****************************************************************
025900941116     C**
026000980130     C     *ENTRY        PLIST
026100980130     C                   PARM                    KPJBA
026200030619      *
026300030619      * Richiedo i dati per la stampa ed eseguo le override
026400030619     c                   exsr      SR_OvrPrtF
026500980130     C**
026600980130     C* F3 - FINE
026700030619     C     D90F3         IFEQ      '1'
026800980130     C                   GOTO      FINE
026900980130     C                   ENDIF
027000030619      * Errori nelle OVERRIDE -> FINE
027100030304     C   91              GOTO      FINE
027200991206     C*
027300950411     C     INIZIO        TAG
027400980130     C                   EXSR      PUL01
027500950411     C*
027600980130     C                   MOVE      WDTGIO        WAAS              4 0
027700980130     C                   MOVEL     CDU           V1CLNP
027800950411     C**
027900900509     C     FOR01         TAG
028000980129     C                   EXFMT     LS03D01
028100941227     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
028200110704     C                   SETOFF                                       902873
028300110704     C                   SETOFF                                       707172
028400081210     c                   clear                   v1cmsg
028500941116     C** CMD3 - FINE LAVORO
028600900509     C   KC              GOTO      FINE
028700910205     C* CONTROLLI FORMATO
028800980130     C                   EXSR      CNT01
028900980202     C  NKF
029000980202     CANNKH
029100941228     COR 90              GOTO      FOR01
029200900523     C*
029300980129     C* M O D I F I C A   D A T A   S P E D I Z I O N E
029400081203     C   KH
029500980130     COR KF              EXSR      MOD01
029600081211     c                   if        werror>0
029700081211     c                   do        *hival
029800081211     c                   exfmt     ls03w01
029900081215     c                   if        *inkl='1'
030000081211     c                   leave
030100081211     c                   endif
030200081211     c                   enddo
030300081211     c                   endif
030400081211
030500910207     C*
030600950411     C                   GOTO      INIZIO
030700000000     C     FINE          TAG
030800980202     C**
030900040409     C     D90F3         IFne      '1'
031000030619     c                   clear                   FNLSB5DS
031100030619     c                   movel     'C'           DB0TLA
031200030619     c                   call      D90psl
031300030619     c                   parm                    FNLSB5DS
031400040409     c                   endif
031500980202     C*
031600980202     C                   CLEAR                   DSLV55
031700980202     C                   MOVEL     'C'           D55TLA
031800980202     C                   CALL      'FNLV55R'
031900980202     C                   PARM                    DSLV55
032000081203     c*
032100081203     C                   CLEAR                   fnls65ds
032200081203     C                   MOVEL     'C'           i65tla
032300081211     c                   movel     '1'           I65STRCMT
032400081203     c                   movel     fnls65ds      kpjbu
032500081205     C                   CALL      'FNLS65R'
032600081203     C                   PARM                    kpjba
032700980202     C**
032800060926     c                   clear                   kpjbu
032900060926     c                   movea     elabor        kpjbu
033000060926     c                   if        kpjbu<>*blanks
033100060926     C                   MOVEL     'LS0G'        KCOAZ
033200060926     C                   CALL      'BCH10'
033300060926     C                   PARM                    KPJBA
033400060926     c                   endif
033500060926     c
033600061012     c
033700061012     c                   clear                   qcmd
033800061012     c                   eval      qcmd = dltovr(1)
033900061012     c                   call      'QCMDEXC'                            91
034000061012     c                   parm                    Qcmd
034100061012     c                   parm                    Qlen
034200061012     c
034300111018     c                   if        invbo(1)>0
034400111018     c                   exsr      Inv_email
034500111018     c                   endif
034600111018     c
034700000000     C                   SETON                                        LR
034800941116     C*
034900980129     C*****************************************************************
035000910205     C* CONTROLLO FORMATO -------------------------------------------
035100980129     C*****************************************************************
035200980130     C     CNT01         BEGSR
035300910205     C                   SETOFF                                       90
035400910412     C*
035500980129     C* SE TUTTE LE SELEZIONI SONO A BLANKS E' ERRORE
035600980130     C     V1CKSC        IFEQ      *ZEROS
035700980130     C     V1CLNP        ANDEQ     *ZEROS
035800980129     C                   SETON                                        4090
035900941116     C                   GOTO      ENDCTR
036000941116     C                   ENDIF
036100051118     c
036200980129     C* CODICE CLIENTE
036300980130    1C     V1CKSC        IFEQ      0
036400980130    2C     V1DKSC        IFNE      *BLANKS
036500980129     C* PARSTA = 9  ESCLUDO ANNULLATI
036600980129     C                   Z-ADD     9             PARSTA
036700980129     C                   MOVEL     RSUT          PARDUT           30
036800980129     C                   MOVEL     *BLANKS       DESCR            48
036900980130     C                   MOVEL     V1DKSC        DESCR
037000980129     C                   Z-ADD     KCI           CCC               4 0
037100981109     C                   CLEAR                   PARFLR
037200981109     C                   CLEAR                   PARDIT
037300000907     C                   Z-ADD     1             PAXNUM
037400000907     C                   CALL      'XALFA3BR'
037500980129     C                   PARM                    PARDUT
037600980129     C                   PARM                    CODUT
037700980129     C                   PARM                    DESCR
037800980129     C                   PARM                    CCC
037900980129     C                   PARM                    PARSTA            1 0
038000981109     C                   PARM                    PARFLR           90
038100981109     C                   PARM                    PARDIT            3
038200000907     C                   PARM                    PAXNUM            2 0
038300000907     C                   PARM                    PAXKCM           80
038400000907     C                   PARM                    PAXKSM          140
038500000907     C                   PARM                    PAXKDM           60
038600980129     C     PARSTA        IFNE      -1
038700000907     C                   MOVEL     PAXKSM        V1CKSC
038800980130     C                   MOVEL     DESCR         V1DKSC
038900980129     C                   ELSE
039000980202     C                   CLEAR                   V1DKSC
039100980129     C                   ENDIF
039200980129     C*
039300980129     C                   SETON                                        90
039400980129     C                   GOTO      ENDCTR
039500980129    2C                   ENDIF
039600980129   X1C                   ELSE
039700980129     C*
039800980129     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
039900980204     C                   Z-ADD     KCI           CCC
040000061003     c                   mllzo     1             v1cksc
040100980129     C     KACO          CHAIN     CNACO000                           41
040200980129    2C  N41ACOFLG        IFNE      ' '
040300980129     C                   SETON                                        41
040400980129    2C                   ENDIF
040500980129     C     *IN41         IFEQ      *OFF
040600980202     C                   MOVEL     ACORAG        V1DKSC
040700980129     C                   ELSE
040800980204     C                   CLEAR                   V1DKSC
040900980129     C                   SETON                                        90
041000980129     C                   GOTO      ENDCTR
041100980129     C                   END
041200980129    1C                   ENDIF
041300061003     c
041400980130     C* LINEA DI PARTENZA
041500061003     c                   mllzo     1             v1clnp
041600090126     c                   clear                   ntwlnp
041700980130    1C     V1CLNP        IFNE      *ZEROS
041800980130     C* CONTROLLO CHE SIA GESTIBILE
041900980130     C     V1CLNP        LOOKUP    L1                                     30
042000980130    2C     *IN30         IFEQ      *OFF
042100980130     C                   SETON                                        9042
042200980204     C                   CLEAR                   V1DLNP
042300980130     C                   GOTO      ENDCTR
042400980130    2C                   ENDIF
042500980130     C*
042600090126     c                   clear                   og143
042700980130     C     V1CLNP        CHAIN     AZORG01L                           30
042800980130     C   30              SETON                                        9042
042900980204     C   30              CLEAR                   V1DLNP
043000980130     C   30              GOTO      ENDCTR
043100980130     C  N30              MOVEL     ORGDES        V1DLNP
043200090126     c  n30              movel     orgde3        og143
043300090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
043400090126     c                             §ogntw='FED'
043500090126     c                   eval      ntwlnp='E'
043600090126     c                   endif
043700980130    1C                   ENDIF
043800980130     C* LINEA DI ARRIVO
043900061003     c                   clear                   wclna
044000980130    1C     V1CLNA        IFNE      *BLANKS
044100980130     C     V1CLNA        ANDNE     *ZEROS
044200980130     C*
044300980130     C     '?'           SCAN      V1CLNA                                 30
044400980130     C*
044500980130    2C     *IN30         IFEQ      *ON
044600980130     C                   MOVE      *BLANKS       §COD1
044700980130     C                   MOVE      *BLANKS       V1CLNA
044800980130     C                   CALL      'TNSD19R'
044900980130     C                   PARM                    §COD1             3
045000980130     C                   PARM                    §TIP              1
045100980202     C                   PARM                    §DES             25
045200980130     C                   MOVEL     §COD1         V1CLNA
045300980130     C                   MOVEL     §DES          V1DLNA
045400980130     C                   SETON                                        90
045500980130     C                   GOTO      ENDCTR
045600980130    2C                   ENDIF
045700980130     C* ESEGUO TESTN
045800980130     C                   TESTN                   V1CLNA               30
045900980130     C                   MOVE      V1CLNA        W001A
046000980130     C     *IN30         IFEQ      *OFF
046100980130     C     W001A         ORLT      '0'
046200980204     C                   CLEAR                   V1DLNA
046300980130     C                   GOTO      ENDCTR
046400980130     C                   ENDIF
046500980130     C*
046600980130     C*  CONTROLLO
046700980130     C*
046800980130     C                   MOVE      V1CLNA        WCLNA             3 0
046900980130     C     WCLNA         CHAIN     AZORG01L                           30
047000980205     C* CONTROLLO SE ANNULLATO
047100980205     C* CONSIDERO SOLO I TIPI "F" E "A"
047200980205    2C  N30ORGFVA        IFNE      *BLANKS
047300980205    2C     ORGFAG        ORNE      'F'
047400980205     C     ORGFAG        ANDNE     'A'
047500980205     C                   SETON                                        30
047600980205    2C                   END
047700980130     C   30              MOVEL     *BLANKS       V1DLNA
047800980130     C   30              SETON                                        9044
047900980130     C   30              GOTO      ENDCTR
048000980130     C  N30              MOVEL     ORGDES        V1DLNA
048100980130    1C                   ENDIF
048200980130     C*
048300980130     C* CONTROLLO CODICE AUTOTRASPORTATORE
048400061003     c                   clear                   wcpdr
048500980130    1C     V1CPDR        IFNE      *BLANKS
048600980130     C     V1CPDR        ANDNE     *ZEROS
048700980130     C                   CLEAR                   V1DPDR
048800980130     C*
048900980130     C     '?'           SCAN      V1CPDR                                 30
049000980130     C*
049100980130     C     *IN30         IFEQ      *ON
049200021203     C*                  MOVEL     *BLANKS       PA2DES
049300021203     C*                  Z-ADD     0             PA2PAD
049400021203     C*                  Z-ADD     V1CLNP        PA2FIL
049500021203     C*                  MOVEL     'R'           PA2FLG
049600021203     C*                  MOVEL     PARAM2        KPJBU
049700021203      *
049800021203      * imposto i campi della ds esterna fnlv24ds
049900021203     C                   clear                   fnlv24ds
050000021203     C                   eval      d24fil   = v1clnp
050100021203     C                   eval      d24tip   = 'A'
050200021203     C                   eval      d24flg   = 'R'
050300021203     C                   movel     fnlv24ds      kpjbu
050400980130     C                   CALL      'FNLV24R'
050500980130     C                   PARM                    KPJBA
050600021203     C*                  MOVEL     KPJBU         PARAM2
050700021203     C*                  MOVE      PA2PAD        V1CPDR
050800021203     C*                  MOVEL     PA2DES        V1DPDR
050900021203      *
051000021203      * valorizzo i campi a video
051100021203     C                   movel     kpjbu         fnlv24ds
051200021203     C                   move      d24pdr        v1cpdr
051300021203     C                   movel     d24rsc        v1dpdr
051400980130     C                   GOTO      ENDCTR
051500980130     C                   END
051600980130     C* ESEGUO TESTN
051700980130     C                   TESTN                   V1CPDR               30
051800980130     C                   MOVE      V1CPDR        W001A             1
051900980130     C     *IN30         IFEQ      *OFF
052000980130     C     W001A         ORLT      '0'
052100980205     C                   CLEAR                   V1CPDR
052200980205     C                   CLEAR                   V1DPDR
052300980130     C                   GOTO      ENDCTR
052400980130     C                   ENDIF
052500980130     C*
052600021204     C                   CLEAR                   V1DPDR
052700980130     C                   MOVE      V1CPDR        WCPDR             7 0
052800021203     C*    WCPDR         CHAIN     FNAPD000                           30
052900021204     C                   move      wcpdr         kpdpdr
053000021203     C                   move      'A'           kpdtip
053100021203     C     kfapd         chain     fiapd01l                           30
053200980130     C     *IN30         IFEQ      *ON
053300980205     C                   CLEAR                   WCPDR
053400980205     C                   CLEAR                   V1DPDR
053500980130     C                   SETON                                        4590
053600980130     C                   GOTO      ENDCTR
053700980204     C                   CLEAR                   V1DPDR
053800980204     C                   ELSE
053900980204     C                   MOVEL     APDRSC        V1DPDR
054000980130     C                   ENDIF
054100980130     C*
054200980205     C* RECUPERO LA LINEA DI PARTENZA DALLA PRIMA PARTE DELL'AUTOTRASPORTATORE
054300980130     C     V1CLNP        IFEQ      *ZEROS
054400980202     C                   MOVEL     WCPDR         V1CLNP
054500090126     c                   clear                   ntwlnp
054600090126     c                   clear                   og143
054700980130     C     V1CLNP        CHAIN     AZORG01L                           30
054800980130     C  N30              MOVEL     ORGDES        V1DLNP
054900090126     C  N30              MOVEL     ORGDE3        og143
055000090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
055100090126     c                             §ogntw='FED'
055200090126     c                   eval      ntwlnp='E'
055300090126     c                   endif
055400980204     C   30              MOVEL     *BLANKS       V1DLNP
055500980205     C                   ELSE
055600111018inzsrC* CONTROLLO LA LINEA DI PARTENZA CON QUELLA DELL'AUTOTRASPORATTORE
055700980205     C                   MOVEL     WCPDR         W0030
055800980205     C     V1CLNP        IFNE      W0030
055900980205     C     W0030         ANDNE     *ZEROS
056000980205     C                   SETON                                        56  90
056100980205     C                   Z-ADD     *ZEROS        W0030
056200980205     C                   GOTO      ENDCTR
056300980205     C                   ENDIF
056400980202     C                   ENDIF
056500980205     C                   ENDIF
056600051118
056700110704     c* controllo numero ORM
056800110704     c                   if        sel4>0
056900110704      * P.O. EMISSIONE
057000110704     C                   Z-ADD     V1CPOE        NUM3              3 0
057100110704     C     NUM3          CHAIN     AZORG01L                           30
057200110704     C     *IN30         IFEQ      *ON
057300110704    2C     ORGFVA        orne      *BLANKS
057400110704    2C     ORGFAG        ORNE      'F'
057500110704     C     ORGFAG        ANDNE     'A'
057600110704     C                   MOVEL     MSG(12)       V1CMSG
057700110704     C                   SETON                                        287290
057800110704     C                   GOTO      ENDCTR
057900110704     C                   ENDIF
058000110704      * N.ORM
058100110704     C     KFORM         CHAIN     FNORM01L                           30
058200110704     C     *IN30         IFEQ      *ON
058300110704     C                   MOVEL     MSG(13)       V1CMSG
058400110704     C                   SETON                                        287390
058500110704     C                   GOTO      ENDCTR
058600110704     C                   ENDIF
058700110704      *
058800110704     C                   SETON                                        06
058900110704      *
059000110704     C                   ENDIF
059100051118     c* Se immessa solo lnp, immettere anche cliente o linea arrivo o
059200051118     c*  terminal arrivo o autotrasportatore
059300051118     c                   if        v1clnp>0 and
059400060713     c                             (v1cksc=0 and v1clna<=*zeros and v1ctfa=0
059500110704     c                              and v1cpdr<=*zeros and v1cnos=' ' and
059600110704     c                              sel4=0)
059700051118     C                   SETON                                        6090
059800051118     C                   GOTO      ENDCTR
059900051118     C                   ENDIF
060000051209     c
060100080620     c                   if        not *in01
060200080620     c                   clear                   v1cnos
060300080620     c                   endif
060400080620     c
060500080620     c* Per mettere in sospensione  in base alle spunte devo essere
060600080620     c*  abilitato dalla  tabella   CLI
060700051209     C
060800080620     c                   clear                   dcli
060900080620    1c                   if        v1cksc>0
061000051209     c                   Clear                   tibs02ds
061100051209     c                   Movel     'C'           T02Mod
061200051209     c                   Movel     KNSIF         T02Sif
061300051209     c                   Movel     'CLI'         T02Cod
061400051209     c                   Movel     V1Cksc        T02Ke1
061500051209     c                   Call      'TIBS02R'
061600051209     c                   Parm                    Kpjba
061700051209     c                   Parm                    tibs02ds
061800070614    2c                   If        T02Err = *Blanks
061900051209     c                   Movel     T02Uni        Dcli
062000070614    2c                   endif
062100080620    1c                   endif
062200080620
062300080620     c                   if        not *in01 and §clisos='S'
062400080620     c                   seton                                        01
062500080620     C                   SETON                                        90
062600080620     C                   GOTO      ENDCTR
062700080620     c                   endif
062800080620     c                   if        sosdaL1=' 'and *in01 and §clisos=' '
062900080620     c                   setoff                                       01
063000080620     C                   SETON                                        90
063100080620     C                   GOTO      ENDCTR
063200080620     c                   endif
063300051209     c
063400080620    1c                   if        v1cnos='S'
063500090126    2c                   if        v1cKSC=0 and ntwlnp<>'E'
063600080620     C                   SETON                                        5790
063700080620     C                   GOTO      ENDCTR
063800080620    2C                   ENDIF
063900080620     c
064000090126    2c                   if        §clisos=' ' and ntwlnp<>'E'
064100051209     C                   SETON                                        58  90
064200051209     C                   GOTO      ENDCTR
064300070614   x2c                   else
064400060403     c* se numero giorni per stampa = 0 --> errore
064500070614    3c                   if        v1cggl = 0
064600060403     C                   SETON                                        59  90
064700070614    3c                   endif
064800070612     c* se posso emetto il campo della consegna richiesta
064900070614    3c                   if        §clisod='S' and not *in03
065000070612     c                   seton                                        0390
065100070612     c                   clear                   v1ctcr
065200070612     c                   clear                   v1cdcr
065300070612     c                   clear                   v1chcr
065400070612     C                   GOTO      ENDCTR
065500070614    3c                   endif
065600070612     c
065700070614    3c                   if        §clisod=' ' and *in03
065800070612     c                   seton                                        90
065900070612     c                   setoff                                       03
066000070612     c                   clear                   v1ctcr
066100070612     c                   clear                   v1cdcr
066200070612     c                   clear                   v1chcr
066300070612     C                   GOTO      ENDCTR
066400070614    3c                   endif
066500070612     c* Altrimenti controllo la consegna richiesta
066600070614    3c                   if        §clisod='S'
066700070612     c                   exsr      CtrCONSRICH
066800070614    4c                   if        *in90
066900070612     c                   goto      ENDCTR
067000070614    4c                   endif
067100070614    3c                   endif
067200070612     c
067300070614    2C                   ENDIF
067400070614   x1c                   else
067500070614     c* se cliente senza abilitazione bolla tolgo la consegna rich
067600070614    3c                   if        *in03
067700070614     c                   seton                                        90
067800070614     c                   setoff                                       03
067900070614     c                   clear                   v1ctcr
068000070614     c                   clear                   v1cdcr
068100070614     c                   clear                   v1chcr
068200070614     C                   GOTO      ENDCTR
068300070614    3c                   endif
068400070614    1C                   ENDIF
068500051209     c
068600980130     C* CONTROLLO DATA SPEDIZIONE DA MODIFICARE ----------------------*
068700980130     C*
068800980130     C                   Z-ADD     V10DSP        G02DAT
068900980130     C                   MOVEL     *BLANK        G02ERR
069000980130     C                   CALL      'XSRDA8'
069100980130     C                   PARM                    WLBDAT
069200980130     C* ERRATA
069300980130    1C     G02ERR        IFEQ      '1'
069400980130     C                   SETON                                        46  90
069500980130     C                   GOTO      ENDCTR
069600980130    1C                   ENDIF
069700980130     C*
069800980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
069900980130     C*  IMMESSO DA VIDEO
070000980130     C                   Z-ADD     G02DAT        V10DSP
070100980130     C*
070200980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
070300980130     C                   MOVE      V10DSP        WAAS              4 0
070400980202     C                   MOVE      G02INV        WMGS              4 0
070500980203     C                   Z-ADD     G02INV        WDTCAR            8 0
070600001120     C                   MOVEL     G02INV        WAMSPE            6 0
070700980130     C*
070800980130     C*
070900980130     C* CONTROLLO DATA SPEDIZIONE CHE SOSTITUISCE LA VECCHIA----------*
071000980130     C*
071100980130     C                   Z-ADD     V11DSP        G02DAT
071200980130     C                   MOVEL     *BLANK        G02ERR
071300980130     C                   CALL      'XSRDA8'
071400980130     C                   PARM                    WLBDAT
071500980130     C* ERRATA
071600980130    1C     G02ERR        IFEQ      '1'
071700980130     C                   SETON                                        47  90
071800980130     C                   GOTO      ENDCTR
071900980130    1C                   ENDIF
072000980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
072100980130     C*  IMMESSO DA VIDEO
072200980130     C                   Z-ADD     G02DAT        V11DSP
072300980202     C* CONTROLLO CHE LE DUE DATE SIANO UGUALI
072400980202     C     V11DSP        IFEQ      V10DSP
072500980204     C                   SETON                                        5090
072600980202     C                   GOTO      ENDCTR
072700980202     C                   ENDIF
072800980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
072900980130     C                   MOVE      V11DSP        WAASN             4 0
073000980130     C*
073100980130    2C     WAASN         IFNE      WAAS
073200081203     c                   if        v1cksc<>0
073300081203     C                   MOVEL(P)  '3C'          COD
073400081203     C                   MOVEL(p)  v1cksc        KEY
073500081203     C     ktab2         chain     tabel00f                           31
073600081203     C   31              clear                   ds3c
073700081203     C  N31              movel     tbluni        ds3c
073800081203     c                   endif
073900081203     c     v1cksc        ifeq      0
074000090126     c     ntwlnp        andne     'E'
074100090126     c     v1cksc        orgt      0
074200090126     c     §3cfsp        andeq     'S'
074300980130     C                   SETON                                        4790
074400980130     C                   GOTO      ENDCTR
074500081203     c                   endif
074600980130    2C                   ENDIF
074700980130     C*
074800980130     C                   Z-ADD     G02INV        WLPDSP            8 0
074900001120     C                   MOVEL     G02INV        WAMSOS            6 0
075000980130     C* DATE SPEDIZIONE NO > DATA MAX DI SOSPENSIONE
075100980130    2C     WLPDSP        IFGT      DATAPA
075200980130     C                   SETON                                        4990
075300980130     C                   GOTO      ENDCTR
075400980130    2C                   ENDIF
075500980130     C*
075600980130     C     WLPDSP        IFLT      DATEU
075700980130     C                   SETON                                        4890
075800980130     C                   GOTO      ENDCTR
075900980130     C                   ENDIF
076000070612     c* Se immessa la consegna richiesta, controllo che non sia
076100070612     c*  inferiore alla nuova data spedizione
076200070612     c                   if        *in03 and  v1cdcr>0  and wdcr<wlpdsp
076300070612     c                   seton                                        702890
076400070612     c                   movel     msg(9)        v1cmsg
076500070612     c                   goto      endctr
076600070612     c                   endif
076700980203     C*
076800980203     C* TERMINAL DI ARRIVO SOLO DOPO AVER INSERITO LA DATA DI SPEDIZIONE
076900980203    1C     V1CTFA        IFNE      *ZEROS
077000980203     C* VERIFICO SE E' CORRETTO
077100980203     C                   MOVE      '0'           CRISP             1
077200980203     C     'A'           SETLL     AZCAE01L
077300980203     C     'A'           READE     AZCAE01L                               30
077400980203    2C     *IN30         DOWEQ     *OFF
077500980203    3C     V1CTFA        IFEQ      CAETFE
077600980203     C     CAEATB        ANDEQ     *BLANKS
077700980203     C     WDTCAR        ANDGE     CAEDDE
077800980203     C     WDTCAR        ANDLE     CAEDSC
077900980203     C     CAETFP        IFEQ      0
078000980203     C     CAETFP        OREQ      SIMFEL
078100980203     C                   MOVE      '1'           CRISP
078200980203     C                   END
078300980203    3C                   END
078400980203     C     'A'           READE     AZCAE01L                               30
078500980203    2C                   ENDDO
078600980203     C* ERRORE
078700980203    2C     CRISP         IFEQ      '0'
078800980203     C                   SETON                                        9043
078900980203     C                   MOVE      *BLANKS       V1DTFA
079000980203     C                   GOTO      ENDCTR
079100980203   X2C                   ELSE
079200980203     C* DECODIFICA
079300980203     C     V1CTFA        CHAIN     AZORG01L                           30
079400980203     C  N30              MOVEL     ORGDES        V1DTFA
079500980203    2C                   END
079600980203    1C                   END
079700980130     C*
079800980204     C  NKF
079900980204     CANNKH              SETON                                        90
080000980130     C*
080100980130     C     ENDCTR        ENDSR
080200070612     C*****************************************************************
080300070612     C* controllo dati consegna richiesta      ----------------------
080400070612     C*****************************************************************
080500070612     C     CtrCONSRICH   BEGSR
080600070612      * tipo
080700070612    1c                   If        V1cTcr <> *Blanks   and
080800070612     c                             v1cDcr = *Zeros
080900070612     c                   Eval      *In28 = *On
081000070612     c                   Eval      *In70 = *On
081100070612     c                   Eval      *In90 = *On
081200070612     c                   Eval      v1cMsg = msg(6)
081300070612     c                   GoTo      EndCons
081400070612    1c                   EndIf
081500070612      * data
081600070614     c                   clear                   wdcr
081700070612    1c                   If        V1cDcr > *Zeros
081800070612      * controllo formale della data
081900070612     c                   Clear                   wlbdat
082000070612     c                   Z-Add     V1cDcr        G02Dat
082100070612     c                   Call      'XSRDA8'
082200070612     c                   Parm                    wlbdat
082300070612    2c                   If        G02Err = '1'
082400070612     c                   Eval      *In28 = *On
082500070612     c                   Eval      *In70 = *On
082600070612     c                   Eval      *In90 = *On
082700070612     c                   Eval      v1cMsg = msg(7)
082800070612     c                   GoTo      EndCons
082900070612    2c                   EndIf
083000070612     c
083100070612     c                   Movel     G02Dat        V1cDcr
083200070612     c                   Movel     G02Inv        wdcr
083300070612      * Non deve essere inferiore/uguale a oggi
083400070612    2c                   If        wdcr <= dateu
083500070612     c                   Eval      *In28 = *On
083600070612     c                   Eval      *In85 = *On
083700070612     c                   Eval      *In90 = *On
083800070612     c                   Eval      v1cMsg = msg(7)
083900070612     c                   GoTo      EndCons
084000070612    2c                   EndIf
084100070612     c
084200070612      * Non deve essere superiore a oggi + 5 gg.
084300070612    2c                   if        v1ctcr = 'P'
084400070612     c                   z-add     woggidcgm1    s_woggidcg
084500070612     c                   z-add     §vpodcgm1     s_vpodcg
084600070612     c                   else
084700070612     c                   z-add     woggidcgm2    s_woggidcg
084800070612     c                   z-add     §vpodcgm2     s_vpodcg
084900070612    2c                   endif
085000070612    2c                   If        wdcr > s_woggidcg
085100070612     c                   Eval      *In28 = *On
085200070612     c                   Eval      *In70 = *On
085300070612     c                   Eval      *In90 = *On
085400070612     c                   Eval      v1cMsg = msg(8)
085500070612     c                   Eval      v1cMsg = %trim(v1cmsg) + ' ' +
085600070612     c                             %trim(%editc(s_vpodcg:'Z'))  +
085700070612     c                             ' giorni dalla data odierna'
085800070612     c                   GoTo      EndCons
085900070612    2c                   EndIf
086000070612    1c                   EndIf
086100070614     c* Ora richiesta
086200070614     c                   if        v1chcr>0
086300070614     c                   movel     v1chcr        whcr              2 0
086400070614     c                   move      v1chcr        wmcr              2 0
086500070614    2C     WHCR          IFLT      0
086600070614     C     WHCR          ORGT      24
086700070614     C                   MOVEL     msg(10)       V1cMSG
086800070614     C                   SETON                                        719028
086900070614     C                   GOTO      ENDCons
087000070614    2C                   ENDIF
087100070614     C*
087200070614    2C     WMCR          IFLT      0
087300070614     C     WMCR          ORGT      59
087400070614     C                   MOVEL     msg(10)       V1cMSG
087500070614     C                   SETON                                        719028
087600070614     C                   GOTO      ENDCons
087700070614    2C                   ENDIF
087800070614    2C                   ENDIF
087900070612     c
088000070612     c     ENDCONS       ENDSR
088100980130     C*****************************************************************
088200980130     C* ELABORAZIONE TESTATA E DETTAGLIO BOLLE ----------------------
088300980130     C*****************************************************************
088400980130     C     MOD01         BEGSR
088500980130     C*
088600980130     C                   SETOFF                                       515253
088700110704     C                   SETOFF                                       5455
088800081205     c                   clear                   werror
088900081210     c                   clear                   wmsg
089000980130     C* CONTROLLO CHE TIPO DI SELEZIONE E' STATA FATTA
089100980130     C* PER CLIENTE
089200980130     C     V1CKSC        COMP      *ZEROS                             5151
089300980130     C* PER LINEA PARTENZA
089400980130     C     V1CLNP        COMP      *ZEROS                             5252
089500980130     C* PER TERMINAL DI ARRIVO
089600980130     C     V1CTFA        COMP      *ZEROS                             5353
089700980130     C* PER LINEA ARRIVO
089800061003     C     WCLNA         COMP      *ZEROS                             54
089900980130     C* PER PADRONCINO
090000061003     C     WCPDR         COMP      *ZEROS                             55
090100020610
090200020610      * se richiesta una linea di partenza imposto solo quella linea di
090300020610      * partenza nella sk delle linee da stampare
090400020610     c                   if        v1clnp <> *zeros
090500020610     c                   clear                   sklnp
090600020610     c                   eval      sklnp(1) = v1clnp
090700020610     c                   else
090800020610     c                   movea     lin           sklnp
090900020610     c                   endif
091000110704     c
091100060403     c* se richiesto aggiornamento su bolle senza colli spuntati stampo
091200060403     c* testata per elenco spedizioni in sospensione da x giorni
091300060403     c   01              if        v1cnos = 'S'
091400060403     c                   open      fnls03p
091500060404     c                   move      v1cggl        w_gg              1
091600060404     c                   eval      ptitolo = ctitolo
091700060404     c                   eval      ptitolo = %xlate('X':w_gg:ptitolo)
091800090127     c                   eval      *in13='0'
091900090127     c                   if        v1cksc>0
092000090127     c                   eval      *in13='1'
092100090127     c                   endif
092200060404     c                   z-add     v1cksc        p1cccm
092300060404     c                   movel     v1dksc        p1dccm
092400090127     c                   z-add     v1clnp        p1clnp
092500090127     c                   movel     v1dlnp        p1dlnp
092600060403     c                   write     testa
092700060403     c                   endif
092800061005     c
092900061005     c                   open      qsysprt
093000060403     c
093100110704     c* se parzializzazione per numero ORM, leggo per ORM
093200110704     c*  che faccio prima
093300110704    0c                   if        sel4>0
093400110704     C                   MOVEL     SEL4          KN14
093500110704     C     Kar43         SETLL     Fiar404L
093600110704     C     Kar43         reade     Fiar404L
093700110704    1c                   dow       not %eof(fiar404l)
093800110704     c
093900110704     c* controllo la linea di partenza immessa o dell'area
094000110704     c                   eval      Indx=%lookup(ar4lnp:sklnp)
094100110704     c*
094200110704    2c                   if        Indx>0
094300110704
094400110704     c     kblpm         chain(n)  fnblp01l
094500110704
094600110704     c* Verifico anche la data spedizione
094700110704     c                   if        blpdsp=wdtcar
094800110704     c
094900110704     c                   exsr      Parzial
095000110704     c
095100110704    3c                   if        NoBolla=' '
095200110704    4c                   if        waasn<>waas
095300110704     c                   exsr      sr_nuovonumero
095400110704   x4c                   else
095500110704     c
095600110704     c                   exsr      Aggiorna
095700110704     c*
095800110704    4c                   endif
095900110704    4c                   endif
096000110704    3c                   endif
096100110704    2c                   endif
096200110704     c
096300110704     C     Kar43         reade     Fiar404L
096400110704    1c                   enddo
096500110704      *
096600110704   x0c                   else
096700020610
096800020610      * elaboro la sk delle linee da stampare
096900110704   1ac                   do        30            xx
097000110704    1c                   if        sklnp(xx) = *zeros
097100020610     c                   leave
097200110704    1c                   endif
097300020610
097400020610     c                   eval      lnp = sklnp(xx)
097500980130     C*
097600980130     C* LETTURA TESTATE BOLLE
097700020610     C  N52KBLP1         SETLL     FNBLP70L
097800980130     C   52
097900020610     CANN55KBLP2         SETLL     FNBLP70L
098000020610     C   55KBLP3         SETLL     FNBLP70L
098100110704     c
098200980203    1C                   DO        *HIVAL
098300020610     C  N52KBLP1         READE     FNBLP70L                               30
098400980203     C   52
098500020610     CANN55KBLP2         READE     FNBLP70L                               30
098600020610     C   55KBLP3         READE     FNBLP70L                               30
098700980203     C   30              LEAVE
098800110704     c
098900110704     c                   exsr      Parzial
099000110704     c
099100110704     c                   if        NoBolla='N'
099200110704     c                   iter
099300110704     c                   endif
099400110704     c
099500081203     c* Se cambia anche l'anno richiamo pgm apposito che esegue anche la
099600081203     c* rinumerazione della spedizione
099700110704    2c                   if        waasn<>waas
099800081203     c                   exsr      sr_nuovonumero
099900081203     c                   iter
100000110704    2c                   endif
100100110704     c
100200110704     c                   exsr      Aggiorna
100300980202     C*
100400980202    1C                   ENDDO
100500020610
100600110704   1ac                   enddo
100700110704    0c                   endif
100800060403     c*
100900060403     c   01              if        v1cnos = 'S'
101000060404     c   98              write     testa
101100060404     c                   setoff                                       98
101200060404     c                   write     fines
101300060403     c                   close     fnls03p
101400060403     c                   endif
101500061005     c
101600061005     c                   close     qsysprt
101700060713     c
101800060713     c                   if        bollemsg>0
101900111018     c                   eval      zz=zz+1
102000111018     c                   eval      invbo(zz)=bollemsg
102100111018     c                   if        v1cksc>0
102200111018     c                   eval      invcod(zz)=%editc(v1cksc:'X')+'-'+
102300111018     c                             v1dksc
102400111018     c                   else
102500111018     c                   eval      invcod(zz)=%editc(v1clnp:'X')+'-'+
102600111018     c                             v1dlnp
102700111018     c                   endif
102800111018     c                   eval      invdim(zz)=savdim
102900111018     c
103000111019     c                   if        zz=50
103100111018     c                   exsr      Inv_email
103200111018     c                   clear                   invbo
103300111018     c                   clear                   invcod
103400111018     c                   clear                   invdim
103500111018     c                   endif
103600060713     c                   endif
103700111018     c
103800060713     c* emetto msg di quante bolle manutenzionate
103900081210     c****               movel     msg(5)        v1cmsg
104000081210     c****               eval      %subst(v1cmsg:27:11)= (%editc(bollema:'2'))
104100060713     c                   seton                                        28
104200081211    1c                   if        werror>0
104300081211    2c                   if        wmsg=*blanks
104400081215     c                   eval      widms1=%trim(widms1)+ 'ERRORE su '
104500081211     c                   eval      widms1=%trim(Widms1)+ (%editc(werror:'Z'))
104600081215     c                   eval      widms1=%trim(Widms1)+ ' bolle.'
104700081211     c                   eval      widms2=ms2wdw
104800081211     c                   eval      widms3=ms3wdw
104900081211     c                   eval      widms4=ms4wdw
105000081211   x2c                   else
105100081211     c                   eval      widmsall=wmsg
105200081211    2c                   endif
105300081211    1c                   endif
105400081211    1c                   if        wmsg=*blanks
105500081210     c                   movel     msg(5)        v1cmsg
105600081211    2c                   if        bollema>0
105700081210     c                   eval      v1cmsg=%trim(v1cmsg)+' ' +%editc(bollema:'2')
105800081211   x2c                   else
105900081210     c                   eval      v1cmsg=%trim(v1cmsg)+' '
106000081210     c                             + %editw(bollema:'       0 ')
106100081211    2c                   endif
106200081210     c                   eval      v1cmsg=%trim(v1cmsg)+ ' Bolle.'
106300081211    1c                   endif
106400060926     c*
106500061003     c                   if        yy<6
106600060926     c                   add       1             yy
106700060926     c                   movel     param         elabor(yy)
106800060926     c                   endif
106900980130     C*
107000980202     C                   ENDSR
107100110704     C*----------------------------------------------------------------
107200110704     c     Parzial       BEGSR
107300110704     c                   clear                   Nobolla           1
107400110704      *
107500110704      *  51 CONTROLLO IL CODICE CLIENTE
107600110704     C     BLPCCM        IFNE      *ZEROS
107700110704     C                   Z-ADD     BLPCCM        WKSC              7 0
107800110704     C                   ELSE
107900110704     C                   Z-ADD     BLPKSC        WKSC
108000110704     C                   ENDIF
108100110704    2C   51WKSC          IFNE      V1CKSC
108200110704     c                   if        blplnp=190 or blplnp=191
108300110704     c                   seton                                        80
108400110704     c                   except    det
108500110704     c                   setoff                                       80
108600110704    2C                   END
108700110704     c                   eval      NoBolla='N'
108800110704     C                   leavesr
108900110704    2C                   END
109000110704     c* Se richiesta sospensione a cavallo di anno per una lnp estera
109100110704     c* se il cliente mantiene il numero spedizione la scarto
109200110704     c                   if        not *in51 and ntwlnp='E'
109300110704     c                             and waasn<>waas
109400110704     C                   MOVEL(P)  '3C'          COD
109500110704     C                   MOVEL(p)  wksc          KEY
109600110704     C     ktab2         chain     tabel00f                           31
109700110704     C   31              clear                   ds3c
109800110704     C  N31              movel     tbluni        ds3c
109900110704     c     §3cfsp        ifeq      'S'
110000110704     c                   eval      NoBolla='N'
110100110704     C                   leavesr
110200110704     c                   endif
110300110704     c                   endif
110400110704      *
110500110704      *  53 CONTROLLO IL TERMINAL ARRIVO
110600110704    2C   53BLPTFA        IFNE      V1CTFA
110700110704     c                   if        blplnp=190 or blplnp=191
110800110704     c                   seton                                        81
110900110704     c                   except    det
111000110704     c                   setoff                                       81
111100110704    2C                   END
111200110704     c                   eval      NoBolla='N'
111300110704     C                   leavesr
111400110704    2C                   END
111500110704      *
111600110704      *  54 CONTROLLO LA LINEA DI ARRIVO
111700110704    2C   54BLPLNA        IFNE      WCLNA
111800110704     c                   if        blplnp=190 or blplnp=191
111900110704     c                   seton                                        82
112000110704     c                   except    det
112100110704     c                   setoff                                       82
112200110704    2C                   END
112300110704     c                   eval      NoBolla='N'
112400110704     C                   leavesr
112500110704    2C                   END
112600110704      *
112700110704      ** CONTROLLO CHIUSURA FOGLIO VIAGGIO DELLA BOLLA
112800110704    2C     BLPDT1        IFNE      *ZEROS
112900110704     c                   if        blplnp=190 or blplnp=191
113000110704     c                   seton                                        83
113100110704     c                   except    det
113200110704     c                   setoff                                       83
113300110704    2C                   END
113400110704     c                   eval      NoBolla='N'
113500110704     C                   leavesr
113600110704    2C                   ENDIF
113700110704     c
113800110704     c* Se richiesto, controllo se non ha spunte per tutti i colli della
113900110704     c*  sped
114000110704    1c                   if        v1cnos='S'
114100110704     c                   clear                   wsispu            1
114200110704     c     kblt          setll     fnblt01l
114300110704     c     kblt          reade     fnblt01l                               99
114400110704    2c                   dow       not *in99
114500110704     c
114600110704     c* Cerco una qualsiasi spunta
114700110704     c     kbrv          chain     fnbrv07l
114800110704    3c                   if        %found(fnbrv07l)
114900110704     c                   eval      wsispu='S'
115000110704     c                   seton                                        99
115100110704   x3c                   else
115200110704     c     kblt          reade     fnblt01l                               99
115300110704    3c                   endif
115400110704    2c                   enddo
115500110704     c
115600110704     c*Se non trovata nessuna spunta
115700110704    2c                   if        wsispu='S'
115800110704     c                   if        blplnp=190 or blplnp=191
115900110704     c                   seton                                        84
116000110704     c                   except    det
116100110704     c                   setoff                                       84
116200110704    2C                   END
116300110704     c                   eval      NoBolla='N'
116400110704     c                   leavesr
116500110704    2c                   endif
116600110704    1c                   endif
116700110704     c*
116800110704      * SCARTO I PREPAGATI ANNULLATI
116900110704     c                   if        blpcbo='M '
117000110704     c                   eval      NoBolla='N'
117100110704     c                   leavesr
117200110704     c                   endif
117300110704      *
117400110704      ** CONTROLLO IL TIPO BOLLA
117500110704     C                   MOVEL     '3A'          COD
117600110704     C                   MOVEL(P)  BLPCBO        KEY
117700110704     C     KTAB2         CHAIN     TABEL00F                           31
117800110704     C   31              CLEAR                   DS3A
117900110704     C  N31              MOVEL     TBLUNI        DS3A
118000110704      *
118100110704      * SCARTO LE BOLLE DI RECUPERO
1182001107041   2C     §3ARBL        IFEQ      'R'
118300110704     c                   if        blplnp=190 or blplnp=191
118400110704     c                   seton                                        85
118500110704     c                   except    det
118600110704     c                   setoff                                       85
118700110704    2C                   END
118800110704     c                   eval      NoBolla='N'
118900110704     C                   leavesr
119000110704    2C                   END
119100110704      * SCARTO LE BOLLE con cod trattamento merce che non prevede merce
119200110704     C                   MOVEL     '1B'          COD
119300110704     C                   MOVEL(P)  BLPctm        KEY
119400110704     C     KTAB2         CHAIN     TABEL00F                           31
119500110704     C   31              CLEAR                   DS1b
119600110704     C  N31              MOVEL     TBLUNI        DS1b
119700110704    2C                   IF        §1bfg8='N'
119800110704     c                   if        blplnp=190 or blplnp=191
119900110704     c                   seton                                        85
120000110704     c                   except    det
120100110704     c                   setoff                                       85
120200110704    2C                   END
120300110704     c                   eval      NoBolla='N'
120400110704     C                   leavesr
120500110704    2C                   END
120600110704      *
120700110704      * PER I PREPAGATI NON È CONSENTITO SPOSTARE LA DATA BOLLA AL MESE
120800110704      *  SUCCESSIVO
120900110704     C     WAMSOS        IFGT      WAMSPE
121000110704      *
121100110704    2C     BLPPDR        IFEQ      *ZEROS
121200110704     c                   if        blplnp=190 or blplnp=191
121300110704     c                   seton                                        86
121400110704     c                   except    det
121500110704     c                   setoff                                       86
121600110704    2C                   END
121700110704     c                   eval      NoBolla='N'
121800110704     C                   leavesr
121900110704    2C                   ELSE
122000110704     C                   MOVEL     '1'           KTRC
122100110704     C     KAR6          CHAIN(N)  FIAR6000                           31
122200110704    2C     *IN31         IFEQ      '0'
122300110704    2C     AR6NFT        ANDGT     *ZEROS
122400110704     c                   if        blplnp=190 or blplnp=191
122500110704     c                   seton                                        87
122600110704     c                   except    det
122700110704     c                   setoff                                       87
122800110704    2C                   END
122900110704     c                   eval      NoBolla='N'
123000110704     C                   leavesr
123100110704    2C                   ENDIF
123200110704    2C                   ENDIF
123300110704      *
123400110704    2C                   ENDIF
123500110704     c
123600110704     c                   ENDSR
123700110704     c*------------------------------------------------------------------------
123800110704     c     Aggiorna      BEGSR
123900110704     c* A questo punto visto che sono stati superati tutti i controlli
124000110704     c* chaino il record bolla in update
124100110704     c     kblt          chain(E)  fnblp01l
124200110704    1c                   if        %error
124300110704     C                   CLEAR                   TRUL82DS
124400110705     c                   if        sel4=0
124500110704     c                   eval      ul82§rrn = fnblpnrri
124600110705     c                   else
124700110705     c                   eval      ul82§rrn = fnblpnrr01
124800110705     c                   endif
124900110704     C                   EVAL      UL82§FIL = 'FNBLP00F'
125000110704     C                   EVAL      UL82§WIN = 'N'
125100110704     C                   EVAL      UL82§F7  = 'S'
125200110704     C                   EVAL      UL82§num = 1
125300110704     C                   EVAL      UL82§att = 30
125400110704     C                   EVAL      UL82§mss = msgjob
125500110704     C* Effettuo la chiamata al *PGM d utilità
125600110704     C                   CALL(e)   'TRUL82R'
125700110704     C                   PARM                    TRUL82DS
125800110704     C     kblt          chain     fnblp01l
125900110704    1c                   endif
126000110704    2c                   if        %found(fnblp01l)
126100110704      *
126200110704      ** CONTROLLO SE DATA BORDERO' MINORE DELLA NUOVA DATA DI SPEDIZIONE
126300110704      *  SBORDERIZZO
126400110704    2C     BLPDBR        IFLT      WLPDSP
126500110704      *
126600110704      * SBORDERIZZO
126700110704     C                   CLEAR                   BLPNFV
126800110704     C                   CLEAR                   BLPDBR
126900110704     C                   CLEAR                   BLPFLP
127000110704    2C                   ENDIF
127100110704     C* AGGIORNO LA NUOVA DATA SPEDIZIONE
127200110704     C                   Z-ADD     WAASN         BLPAAS
127300110704     C                   Z-ADD     WLPDSP        BLPMGS
127400110704     c* Aggiorno anche la data ritiro merce
127500110704     c*  solo per i clienti che spostanto in sospensione per mancanza di spu
127600110704     c*  nte
127700110704    2c                   if        v1cnos='S'
127800110704     C                   movel     WLPDSP        BLPdrt
127900110704     c* Se richiesto e se presente consegna richiesta, imposto la nuova data
128000110704    3c                   if        *in03 and v1cdcr>0 and blpdcr>0
128100110704     c                   movel     v1ctcr        blptcr
128200110704     c                   movel     wdcr          blpdcr
128300110704     c                   movel     v1chcr        blphcr
128400110704    3c                   endif
128500110704     c
128600110704   x2c                   else
128700110704     c* Se la data ritiro > della data spedizione nuova e non c'e' ORM
128800110704     c*  imposto SEMPRE la data ritiro= data spedizione
128900110704    3c                   if        blpdrt>wlpdsp
129000110704     c     kar4m         chain     fiar401l
129100110704    4c                   if        not %found(fiar401l)
129200110704     c                   eval      blpdrt=wlpdsp
129300110704    4c                   endif
129400110704    3c                   endif
129500110704     c
129600110704    2c                   endif
129700110704     c
129800110704     C* E RICACOLO I TERMINAL DI ARRIVO E FILIALE ELABORAZIONE ARRIVI
129900110704     C                   CLEAR                   DSLV55
130000110704     C                   MOVEL     ' '           D55TPT
130100110704     C                   MOVEL     BLPLNA        D55LIN
130200110704     C                   MOVEL     BLPLNP        D55LNP
130300110704     C                   MOVEL     WLPDSP        D55DRF
130400110704     C                   CALL      'FNLV55R'
130500110704     C                   PARM                    DSLV55
130600110704    2C     D55ERR        IFEQ      ' '
130700110704     C                   MOVEL     D55TFA        BLPTFA
130800110704     C                   MOVEL     D55TFP        BLPFEA
130900110704    2C                   ENDIF
131000110704     C* BLPTFP: TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
131100110704     C                   CLEAR                   DSLV55
131200110704     C                   MOVEL     'P'           D55TPT
131300110704     C                   MOVEL     BLPLNP        D55LIN
131400110704     C                   MOVEL     WLPDSP        D55DRF
131500110704     C                   CALL      'FNLV55R'
131600110704     C                   PARM                    DSLV55
131700110704    2C     D55ERR        IFEQ      ' '
131800110704     C                   MOVEL     D55TFP        BLPTFP
131900110704    2C                   ENDIF
132000110704     C*
132100110704     C                   UPDATE    FNBLP001
132200110704     c                   add       1             bollema           9 0
132300110704     c
132400110704     c* Se si tratta di assegnato S.F. tassato in partenza, cancello la
132500110704     c* tassazione
132600110704     c                   clear                   ktrc
132700110704    2c                   if        §3atb2<>*blanks
132800110704     c                   eval      ktrc='2'
132900110704   x2c                   else
133000110704     c                   movel     §3atb1        flgtb1            1
133100110704    3c                   if        flgtb1='A'
133200110704     c                   eval      ktrc='1'
133300110704    3c                   endif
133400110704    2c                   endif
133500110704     c
133600110704   1ac                   if        ktrc<>*blanks
133700110704     C     KAR6          CHAIN(N)  FIAR6000                           31
133800110704    2c                   if        not *in31
133900110704     c                   move      ar6ksc        fineksc           4 0
134000110704    3c                   if        fineksc<>0000 and fineksc<>9999
134100110704     c* deleto la tassazione
134200110704     c     kar6          setll     fiar701l
134300110704     c     kar6          reade     fiar701l
134400110704    4c                   dow       not %eof(fiar701l)
134500110704     c                   delete    fiar7000
134600110704     c
134700110704     c     kar6          reade     fiar701l
134800110704    4c                   enddo
134900110704     c
135000110704     c     kar6          delete    fiar6000
135100110704    3c                   endif
135200110704    2c                   endif
135300110704   1ac                   endif
135400110704     c
135500110704     C*
135600110704     C     KBLT          SETLL     FNBLT01L
135700110704    2C     *IN30         DOWEQ     *OFF
135800110704     C     KBLT          READE     FNBLT01L                               30
135900110704     C* SOLO SE NON C'E' GIA' LA DATA DI PARTENZA
136000110704    3C  N30BLTDFV        IFEQ      0
136100110704     C* E LA DATA BORDERO' DELLA TESTATA NON E' MAGGIORE ALLA NUOVA DATA SPED.
136200110704    2C     BLPDBR        ANDLT     WLPDSP
136300110704     C                   CLEAR                   BLTNFV
136400110704     C                   UPDATE    FNBLT000
136500110704   X3C                   ELSE
136600110704     C                   UNLOCK    FNBLT01L
136700110704    3C                   ENDIF
136800110704    2C                   ENDDO
136900110704     C* AGGIORNO ANCHE FISGN SE PRESENTE
137000110704     C     KFISGN        SETLL     FISGN05L
137100110704     C     KFISGN        READE     FISGN05L                               30
137200110704    2C     *IN30         DOWEQ     *OFF
137300110704     C                   Z-ADD     WLPDSP        SGNMGS
137400110704     C                   Z-ADD     BLPTFA        SGNTNA
137500110704     C                   Z-ADD     BLPTFP        SGNTNP
137600110704     C                   CLEAR                   SGNT6A
137700110704     C                   CLEAR                   SGNT6B
137800110704     C                   CLEAR                   SGNT6C
137900110704     C                   CLEAR                   SGNT6D
138000110704     C                   CLEAR                   SGNT6E
138100110704     C                   CLEAR                   SGNT6F
138200110704     c                   eval      sgndatora = %char(%timestamp:*iso0)
138300110704     C                   UPDATE    FISGN000
138400110704     C     KFISGN        READE     FISGN05L                               30
138500110704    2C                   ENDDO
138600110704     C*
138700110704     c* richiamo routine per gestione stampe
138800110704     c                   z-add     blpaas        wlpaas
138900110704     c                   z-add     blpnsp        wlpnsp
139000110704     c                   exsr      sr_stampe
139100110704     c*
139200110704    1c                   endif
139300110704     c
139400110704     c                   ENDSR
139500980130     C*----------------------------------------------------------------
139600980130     C*
139700980130     C*--- OPERAZIONI INIZIALI ---------------------------------------*
139800980130     C     *INZSR        BEGSR
139900980130     C***
140000980130     C* KLIST
140100980130     C***
140200980130     C* ACCESSO   CNACO
140300980130     C     KACO          KLIST
140400980130     C                   KFLD                    CODUT
140500980130     C                   KFLD                    CCC
140600980130     C                   KFLD                    V1CKSC
140700051212     C     Ktbe          KLIST
140800051212     C                   KFLD                    kcod
140900051212     C                   KFLD                    klin
141000051212     C                   KFLD                    ksif
141100110704     C*
141200980130     C     KBLT          KLIST
141300980130     C                   KFLD                    BLPAAS
141400980130     C                   KFLD                    BLPLNP
141500980130     C                   KFLD                    BLPNRS
141600980130     C                   KFLD                    BLPNSP
141700110704     C* KEY PER AGGIIORNARE FLBLT
141800110704     C     KBLPM         KLIST
141900110704     C                   KFLD                    ar4AAS
142000110704     C                   KFLD                    ar4LNP
142100110704     C                   KFLD                    ar4NRS
142200110704     C                   KFLD                    ar4NSP
142300110704     C* KEY PER AGGIORNARE fiar4
142400080331     C     Kar4M         KLIST
142500080331     C                   KFLD                    BLPAAS
142600080331     C                   KFLD                    BLPLNP
142700080331     C                   KFLD                    BLPNRS
142800080331     C                   KFLD                    BLPNSP
142900110704     C                   KFLD                    ktrcM
143000051209     C     KBrv          KLIST
143100051209     C                   KFLD                    bltfls
143200051209     C                   KFLD                    bltlna
143300051209     C                   KFLD                    bltNRS
143400051209     C                   KFLD                    bltnsc
143500001120     C* KEY PER AGGANCIARE  FIAR6
143600001120     C     KAR6          KLIST
143700001120     C                   KFLD                    BLPAAS
143800001120     C                   KFLD                    BLPLNP
143900001120     C                   KFLD                    BLPNRS
144000001120     C                   KFLD                    BLPNSP
144100001120     C                   KFLD                    KTRC              1
144200980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
144300980130     C     KBLP1         KLIST
144400020610     c                   kfld                    lnp
144500980130     C                   KFLD                    WAAS
144600980130     C                   KFLD                    WMGS
144700980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 2
144800980130     C     KBLP2         KLIST
144900020610     c                   kfld                    lnp
145000980130     C                   KFLD                    WAAS
145100980130     C                   KFLD                    WMGS
145200980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 3
145300980130     C     KBLP3         KLIST
145400020610     c                   kfld                    lnp
145500980130     C                   KFLD                    WAAS
145600980130     C                   KFLD                    WMGS
145700980130     C                   KFLD                    WCPDR
145800010326     C* ACCESSO   FILE FISGN PER NUMERO SPEDIZIONE
145900010326     C     KFISGN        KLIST
146000010326     C                   KFLD                    BLPAAS
146100010326     C                   KFLD                    BLPLNP
146200010326     C                   KFLD                    BLPNRS
146300010326     C                   KFLD                    BLPNSP
146400980130     C* ACCESSO   TABEL
146500980130     C     KTAB2         KLIST
146600980130     C                   KFLD                    CODUT
146700980130     C                   KFLD                    COD               2
146800980130     C                   KFLD                    KEY               8
146900980202     C* ACCESSO   AZCLN
147000980202     C     KCLN          KLIST
147100980202     C                   KFLD                    SIMFEL
147200980202     C                   KFLD                    W0030             3 0
147300980202     C                   KFLD                    WAA               4 0
147400980202     C                   KFLD                    WMM               2 0
147500021203     C     Kfapd         klist
147600021203     C                   kfld                    kpdtip
147700021203     C                   kfld                    kpdpdr
147800110704      *
147900110704     C     KFORM         KLIST
148000110704     C                   KFLD                    V1CPOE
148100110704     C                   KFLD                    V1CNSR
148200110704     C                   KFLD                    V1CNOR
148300110704     C                   KFLD                    V1CNRV
148400110704      *
148500110704     C     Kar43         KLIST
148600110704     C                   KFLD                    KTRCM
148700110704     C                   KFLD                    KN14             14
148800110704     C*
148900980130     C***
149000980130     C* DEFINIZIONE CAMPI
149100980130     C***
149200980130     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
149300980130     C***
149400980130     C                   CALL      'X§PARUT'
149500980130     C                   PARM                    UT§DSE
149600980202     C                   MOVEL     RAGUT         RSUT
149700980130     C                   MOVEL     REC80         CNCR80
149800980130     C*--- RICERCA CAPOCONTI
149900980130     C                   DO        50            X                 2 0
150000980130     C                   MOVE      TCU(X)        TCUDS
150100980130     C     F56           CABNE     'CG'          END1
150200980130     C     F4            COMP      '1'                                    21
150300980130     C     F4            COMP      '2'                                    22
150400980130     C     F4            COMP      '3'                                    23
150500980130     C     F4            COMP      '6'                                    27
150600980130     C** 1 CLIENTI   21
150700980130     C** 2 FORNITORI 22
150800980130     C** 3 AGENTI    23
150900980130     C     F3            COMP      '0'                                242425
151000980130     C     F3            COMP      'I'                                    26
151100980130     C** 0 ITALIA   25
151200980130     C** 1 ESTERO   24
151300980130     ** I CAPO CONTO IVA
151400980130     C   21
151500980130     CAN 24              Z-ADD     KCU(X)        KCE               4 0
151600980130     C   21
151700980130     CAN 25              Z-ADD     KCU(X)        KCI               4 0
151800980130     C   22
151900980130     CAN 24              Z-ADD     KCU(X)        KFE               4 0
152000980130     C   22
152100980130     CAN 25              Z-ADD     KCU(X)        KFI               4 0
152200980130     C   23
152300980130     CAN 24              Z-ADD     KCU(X)        KAE               4 0
152400980130     C   23
152500980130     CAN 25              Z-ADD     KCU(X)        KAI               4 0
152600980130     C   26              Z-ADD     KCU(X)        KIVA              4 0
152700980130     C   27              Z-ADD     KCU(X)        KBNA              4 0
152800980130     C     END1          TAG
152900980130     C                   END
153000980130     C                   SETOFF                                       212223
153100980130     C                   SETOFF                                       242526
153200980130     C                   SETOFF                                       27
153300980202     C***
153400980202     C* CARICO DATI VARIABILI PARTENZE
153500980202     C***
153600980202     C                   MOVEL     '3I'          COD
153700980202     C                   MOVEL     '1       '    KEY
153800980202     C     KTAB2         CHAIN     TABEL00F                           31
153900980202     C  N31              MOVEL     TBLUNI        DS3I
154000980202     C   31              CLEAR                   DS3I
154100980202     C***
154200980202     C* CARICO CODICI CLIENTI SDI PER I QUALI INSERISCO OBBLIGATORIAM
154300980202     C*  NUMERO AWB CON CONTROLLO CHECK DIGIT
154400980202     C***
154500980202     C                   MOVEL     '3I'          COD
154600980202     C                   MOVEL     'E       '    KEY
154700980202     C     KTAB2         CHAIN     TABEL00F                           31
154800980202     C  N31              MOVEA     TBLUNI        KSS
154900980202     C* 2 CIFRE INIZIALI FISSE PER NUMERO AWB
155000980202     C  N31              MOVE      TBLUNI        §3IFAW            2 0
155100980202     C   31              CLEAR                   KSS
155200980202     C   31              CLEAR                   §3IFAW
155300980130     C*
155400980130     C* DATA BOLLETTAZIONE:  TRA UDATE E DATA GIORNO + §3IGGS
155500980130     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
155600980130     C                   TIME                    W0140            14 0
155700980130     C* UDATE IN GGMMAAAA
155800980130     C                   MOVE      W0140         WDTGIO            8 0
155900060404     C                   MOVEl     W0140         utime
156000980130     C*
156100980130     C* UDATE IN AAAAMMGG
156200980130     C                   Z-ADD     WDTGIO        G02DAT
156300980130     C                   MOVEL     *BLANK        G02ERR
156400980130     C                   CALL      'XSRDA8'
156500980130     C                   PARM                    WLBDAT
156600980130     C                   MOVEL     G02INV        DATEU             8 0
156700980130     C* SE NON FOSSE PERMESSO BOLLETTARE IN SOSPENSIONE
156800980130     C*  SI PUO' BOLLETTARE SOLO FINO A OGGI
156900980130     C                   Z-ADD     G02INV        DATAPA            8 0
157000980130     C                   MOVEL     G02DAT        DATAPG            8
157100980130     C*
157200980130     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
157300980130     C                   Z-ADD     1             X
157400980130    1C     X             DOWLE     §3IGGS
157500980130     C*
157600980130     C                   MOVEL     'F'           WFEST             1
157700980130     C*
157800980130    2C     WFEST         DOWNE     ' '
157900980130     C     G02TGI        ADD       1             GIOTGI
158000980130     C                   ADD       1             G02TGI
158100980130     C                   CALL      'XSRGI8'
158200980130     C                   PARM                    WGIDAT
158300980130     C*
158400980130     C                   Z-ADD     GIOINV        DATAPA            8 0
158500980130     C                   MOVEL     GIODAT        DATAPG            8
158600980130     C*
158700980130     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
158800980130     C                   Z-ADD     GIOINV        WDATA
158900980130     C                   CLEAR                   W0030
159000980130     C* CHAIN SUL CALENDARIO BARTOLINI
159100980130     C     KCLN          CHAIN     AZCLN01L                           30
159200980130     C   30              CLEAR                   CLNPOM
159300980130     C                   MOVEA     CLNPOM        K31
159400980130     C                   MOVEL     K31(WGG)      WFEST
159500980130    2C                   ENDDO
159600980130     C*
159700980130     C                   ADD       1             X
159800980130    2C                   ENDDO
159900070612     C* reperisco da tabella vpo il numero giorni da sommare a udate per
160000070612     c* determinare data max per data consegna richiesta
160100070612     C                   CLEAR                   tibs02ds
160200070612     C                   MOVEL     'C'           T02MOD
160300070612     C                   MOVEL     KNSIF         T02SIF
160400070612     C                   MOVEL     'VPO'         T02COD
160500070612     C                   MOVEL(p)  'VPO'         T02KE1
160600070612     C                   CALL      'TIBS02R'
160700070612     C                   PARM                    KPJBA
160800070612     C                   PARM                    tibs02ds
160900070612    4C     T02ERR        IFEQ      *BLANKS
161000070612     C                   MOVEL     T02UNI        DVPO
161100070612     C                   ELSE
161200070612     C                   CLEAR                   DVPO
161300070612    4C                   ENDIF
161400070612    c
161500070612     c                   Move      Dateu         Dataiso
161600070612     c                   Adddur    §vpodcgm1:*d  Dataiso
161700070612     c                   Move      Dataiso       woggidcgm1
161800070612     c*
161900070612     c                   Move      Dateu         Dataiso
162000070612     c                   Adddur    §vpodcgm2:*d  Dataiso
162100070612     c                   Move      Dataiso       woggidcgm2
162200070612
162300980130     C***
162400980130     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
162500980130     C***
162600980130     C                   CLEAR                   DSUL06
162700980130     C                   MOVE      '£1'          D06COD
162800980130     C                   MOVEL     SIMFEL        D06KEY
162900980130     C                   MOVEL     'L'           D06TLA
163000980130     C                   MOVEL     DSUL06        KPJBU
163100980130     C*
163200980130     C                   CALL      'TRUL06R'
163300980130     C                   PARM                    KPJBA
163400980130     C                   MOVEL     KPJBU         DSUL06
163500980130     C                   MOVEA     LIN           L1
163600070903     c
163700090126     c* Verifico se il £1 c'e' almeno un p.o. estero
163800090126     c                   clear                   poestero          1
163900070903     c                   z-add     1             x
164000070903     c                   dow       L1(x)<>0
164100070903     c     l1(x)         chain     azorg01l
164200070903     c                   if        %found(azorg01l)
164300070903     c                   movel     orgde3        og143
164400090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
164500090126     c                             §ogntw='FED'
164600090126     c                   eval      poestero='S'
164700070903     c                   leave
164800070903     c                   endif
164900070903     c                   endif
165000070903     c                   add       1             x
165100070903     c                   enddo
165200980130     C***
165300020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
165400980130     C***
165500020429     C                   Z-ADD     SIMPOU        CDU               3 0
165600020429     C     SIMTPP        IFEQ      '2'
165700020429     C     SIMTPP        OREQ      *BLANKS
165800020429     C                   SETON                                        12
165900020429     C                   ENDIF
166000060926     c
166100111018     c                   clear                   elabor
166200111018     c
166300111018     c                   Clear                   tibs02ds
166400111018     c                   Clear                   dmradan
166500111018     c                   Movel     'C'           T02Mod
166600111018     c                   Movel     KNSIF         T02Sif
166700111018     c                   Movel     'MRA'         T02Cod
166800111018     c                   Movel     'FNLS03R'     T02Ke1
166900111018     c                   Call      'TIBS02R'
167000111018     c                   Parm                    Kpjba
167100111018     c                   Parm                    tibs02ds
167200111018    2c                   If        T02Err = *Blanks
167300111018     c                   Movel     T02Uni        Dmradan
167400111018    2c                   endif
167500001120     C*
167600980130     C                   ENDSR
167700980130     C*****************************************************************
167800980130     C* PULIZIA   FORMATO -------------------------------------------
167900980130     C*****************************************************************
168000980130     C     PUL01         BEGSR
168100980130     C*
168200070614     c                   setoff                                       0103
168300980130     C                   MOVE      *ZEROS        V1CKSC
168400980130     C                   MOVE      *BLANKS       V1DKSC
168500980130     C                   MOVE      *BLANKS       V1CLNP
168600980130     C                   MOVE      *BLANKS       V1DLNP
168700980130     C                   MOVE      *ZEROS        V1CTFA
168800980130     C                   MOVE      *BLANKS       V1DTFA
168900980130     C                   MOVE      *BLANKS       V1CLNA
169000980130     C                   MOVE      *BLANKS       V1DLNA
169100980130     C                   MOVE      *BLANKS       V1CPDR
169200980130     C                   MOVE      *BLANKS       V1DPDR
169300980130     C                   Z-ADD     *ZEROS        V10DSP
169400980130     C                   Z-ADD     *ZEROS        V11DSP
169500070612     C                   CLEAR                   v1cdcr
169600070612     C                   CLEAR                   v1ctcr
169700070612     C                   CLEAR                   v1chcr
169800070612     C                   CLEAR                   wdcr
169900051209     C                   move      *blanks       V1cnos
170000060403     C                   z-add     3             V1cggl
170100060713     c                   clear                   bollemsg
170200060713     c                   clear                   bollema           9 0
170300110704     c                   clear                   v1cpoe
170400110704     c                   clear                   v1cnsr
170500110704     c                   clear                   v1cnor
170600110704     c                   clear                   v1cnrv
170700111017     c                   clear                   savdim            8 0
170800110704     c
170900090126     c* Abilito l'opzione se c'e' un p.o. estero in £1
171000090126     c                   if        poestero='S'
171100070903     c                   seton                                        01
171200080624     c                   eval      SosdaL1='S'
171300070903     c                   else
171400070903     c
171500070903     c* Abilito l'opzione solo se almeno un cliente è abilitato
171600080620     c                   eval      SosdaL1=' '
171700051212     c                   eval      ksif=knsif
171800051212     c     ktbe          setll     tntbe02l
171900051212     c                   if        not %equal(tntbe02l)
172000051212     c                   clear                   ksif
172100051212     c     ktbe          setll     tntbe02l
172200051212     c                   endif
172300051212     c
172400051212    1c                   if        %equal(tntbe02l)
172500051212     c     ktbe          reade     tntbe02l
172600051212   1ac                   dow       not %eof(tntbe02l) and not *in01
172700051212    2c                   if        tbeatb=' '
172800051212     c                   movel     tbeuni        dcli
172900051212    3c                   if        §clisos='S'
173000070206     c                   movel     tbeke1        lnpcli            3 0
173100070206    4c***                if        *in12=*on  and lnpcli=cdu
173200070206     c***                seton                                        01
173300070206    4c***                endif
173400051212     c
173500070206    4c****               if        *in12=*off
173600051212     c     lnpcli        lookup    l1                                     30
173700051212    5c                   if        *in30
173800080620     c                   eval      SosdaL1='S'
173900051212     c                   seton                                        01
174000051212    5c                   endif
174100070206    4c****               endif
174200051212    3c                   endif
174300051212    2c                   endif
174400051212     c
174500051212     c     ktbe          reade     tntbe02l
174600051212   1ac                   enddo
174700051212    1c                   endif
174800070903     c
174900070903    1c                   endif
175000080616     c
175100080624     c*********          if        not *in01 and (cdu=118 or cdu=107)
175200080624     c*********          eval      *in01=*on
175300080624     c*********          endif
175400060403     c*
175500060403     c* se 01 acceso effettuo ovrprtf per mettere le stampe in save
175600060403     c                   if        *in01
175700060403     c                   clear                   qcmd
175800060403     c                   eval      qcmd = $cmd2(1)
175900060403     c                   call      'QCMDEXC'                            91
176000060403     c                   parm                    Qcmd
176100060403     c                   parm                    Qlen
176200060403     c                   endif
176300061005     c
176400061005     c                   clear                   qcmd
176500061005     c                   eval      qcmd = $cmd3(1)
176600061005     c                   call      'QCMDEXC'                            91
176700061005     c                   parm                    Qcmd
176800061005     c                   parm                    Qlen
176900061005     c
177000051212     c
177100051212     c
177200980130     C*
177300980130     C                   ENDSR
177400030619
177500030619      *---------------------------------------------------------------*
177600030619      * Richiedo i dati per la stampa   ed   Eseguo le override       *
177700030619      *---------------------------------------------------------------*
177800030619     c     SR_OvrPrtF    BEGSR
177900030619      *
178000030619     c                   call      'TRUL90R'
178100030619     c                   parm                    KPJBA
178200030619     c                   parm                    TRUL90DS
178300030619      * - F3 > Fine
178400030619     c     D90F3         cabeq     *on           End_Ovr
178500030619      *
178600030619      * - FNLSB5PA4 > bolle laser in formato A4
178700030619     c                   eval      wFILE = 'FNLSB5PA4 '
178800030619     c                   eval      wOUTQ = D90prb4
178900030619     c                   eval      wFORM = D90mdb4
179000030619     c                   exsr      SR_x_Cmd
179100030619     c     *in91         cabeq     *on           End_Ovr
179200030619      *
179300030619      * - FNLSB5PA5 > bolle laser in formato A5
179400030619     c                   eval      wFILE = 'FNLSB5PA5 '
179500030619     c                   eval      wOUTQ = D90prb5
179600030619     c                   eval      wFORM = D90mdb5
179700030619     c                   exsr      SR_x_Cmd
179800030619     c     *in91         cabeq     *on           End_Ovr
179900030619      *
180000030619     c     End_Ovr       ENDSR
180100030619
180200030619      *---------------------------------------------------------------*
180300030619
180400030619     c     SR_x_Cmd      BEGSR
180500030619      *
180600030619      * impostazione comando da eseguire
180700030619     c                   eval      Qcmd  = %trim($Cmd(1)) + ' '
180800030619     c                                   + %trim($Cmd(2))
180900030619      * - nome del file
181000030619     c                   eval      Qcmd = %replace(wFILE:Qcmd:
181100030619     c                                    %scan('&FILE':Qcmd))
181200030619      * - nome della coda di stampa
181300030619     c                   eval      Qcmd = %replace(wOUTQ:Qcmd:
181400030619     c                                    %scan('&OUTQ':Qcmd))
181500030619      * - tipo di modulo
181600030619     c                   eval      Qcmd = %replace(wFORM:Qcmd:
181700030619     c                                    %scan('&FORMTYPE':Qcmd))
181800030619      *
181900030619     c                   call      'QCMDEXC'                            91
182000030619     c                   parm                    Qcmd
182100030619     c                   parm                    Qlen
182200030619      *
182300030619     c                   ENDSR
182400081203      *---------------------------------------------------------------*
182500081203
182600081203     c     SR_stampe     BEGSR
182700081203     C* PER CMD8 STAMPO LA BOLLA
182800081203     C     *INKH         IFEQ      *ON
182900081203     C     BLPFST        ANDNE     'N'
183000081203     c                   clear                   FNLSB5DS
183100081203     C                   MOVEL     'V'           DB0RIS
183200081203     C                   MOVEL     'P'           DB0TBO
183300081205     C                   Z-ADD     wLPAAS        DB0AAS
183400081203     C                   Z-ADD     BLPLNP        DB0LNP
183500081203     C                   Z-ADD     BLPNRS        DB0NRS
183600081205     C                   Z-ADD     wLPNSP        DB0NSP
183700081203     c                   call      D90psl
183800081203     c                   parm                    FNLSB5DS
183900081203     C                   ENDIF
184000081203     c*
184100081203     c* GESTIONE STAMPA SPEDIZIONE
184200081203     c                   if        v1cnos='S'
184300081203     c                   if        v1cggl > 0
184400081203     c* calcolo giorni lavorativi fra data spedizione e data immissione
184500081203     c                   clear                   wdat8
184600081203     c                   z-add     blpdim        datada
184700081203     c                   z-add     wlpdsp        dataa
184800081203     c                   call      'XSRLAV8'
184900081203     c                   parm                    wdat8
185000081203     c                   if        (ggl-1) > v1cggl
185100081203     c                   if        blpfns = 'S'
185200081203     c                   seton                                        02
185300081203     c                   else
185400081203     c                   setoff                                       02
185500081203     c                   endif
185600081203     c* inverto data ritiro per stampa
185700081203     c     *iso          move      blpdim        data_eur
185800081203     c                   move      data_eur      p1cdim
185900081203     c   98              write     testa
186000081203     c                   setoff                                       98
186100081203     c                   write     rigsped
186200081203     c                   endif
186300081203     c                   endif
186400081203     C**
186500081203     c** Se passati xx giorni di sospensione, invio msg al CED
186600081203     c                   if        (ggl-1) > §3iggm
186700081203     c                   add       1             bollemsg          9 0
186800111017     c* memorizzo la data più bassa di immissione
186900111017     c                   if        savdim=0 or blpdim<savdim
187000111017     c                   eval      savdim=blpdim
187100111017     c                   endif
187200111017     c
187300081203     c                   endif
187400081203     c                   endif
187500081203     c                   endsr
187600081203      *---------------------------------------------------------------*
187700081203
187800081203     c     SR_nuovonumeroBEGSR
187900081203     c                   clear                   fnls65ds
188000081203     c                   z-add     waas          I65AAS
188100081203     c                   z-add     blplnp        I65LNP
188200081203     c                   z-add     blpnrs        I65NRS
188300081203     c                   z-add     blpnsp        I65NSP
188400081203     c                   z-add     waasn         I65AASN
188500081203     c                   z-add     wlpdsp        I65MGSN
188600081203     c                   if        v1cnos='S'
188700081203     c                   movel     'S'           I65AGDRT
188800081203     c                   if        *in03 and v1cdcr>0 and blpdcr>0
188900081203     c                   movel     'S'           I65AGDCR
189000081203     c                   move      v1ctcr        I65TCRN
189100081203     c                   move      wdcr          I65DCRN
189200081203     c                   move      v1chcr        I65HCRN
189300081203     c                   endif
189400081203     c                   endif
189500081203     c                   move      '1'           I65STRCMT
189600081203     c                   move      '1'           I65FCMT
189700081203     c                   move      '1'           I65CMTRLBK
189800081203     c                   movel     fnls65ds      kpjbu
189900081210     c                   call      'FNLS65R'
190000081203     c                   parm                    kpjba
190100081203     c                   movel     kpjbu         fnls65ds
190200081203     c                   if        o65err=*blanks
190300081203     c                   add       1             bollema           9 0
190400081205     c                   z-add     waasn         wlpaas
190500081205     c                   z-add     o65nsp        wlpnsp
190600081203     c                   exsr      sr_stampe
190700081204     c                   else
190800081205     c                   add       1             werror            6 0
190900081210     c                   if        o65err='1'
191000081210     c                   movel     o65msg        wmsg
191100081210     c                   endif
191200081203     c                   endif
191300081203     c                   endsr
191400111018     c*-------------------------------------------------------------------
191500111018     c     INV_email     BEGSR
191600111018     c
191700111018     c                   if        not %open(prtemail)
191800111018     C                   EXSR      SR_OPENPRTF
191900111018     C                   endif
192000111018     c
192100111018     c                   exsr      Routend
192200111018     c
192300111018     c                   ENDSR
192400111018
192500111018      /free
192600111018       //--------------------------------------------------------------
192700111018       //?Override al file di stampa per impostarvi i dati per
192800111018       //?  l'invio via e-mail   +   stampa inizio e-mail
192900111018       //--------------------------------------------------------------
193000111018       BEGSR sr_OpenPrtF;
193100111018
193200111018         // Override al file di stampa per impostarvi i dati per
193300111018         // l'invio via e-mail
193400111018           exsr sr_Override;
193500111018
193600111018           open PrtEMAIL;
193700111018
193800111018         // Stampa una testata se NON è richiesta la e-mail
193900111018           IF  §MRAdreg =  *blank;
194000111018             O_testo = JobUser + ' - ' + SDSpgm
194100111018                     + ' - ' + %editc( *date : 'Y' )
194200111018                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
194300111018             except PrtDet;
194400111018             clear O_testo;
194500111018             except PrtDet;
194600111018             except PrtDet;
194700111018           ENDIF;
194800111018
194900111018         // Stampa testo iniziale
195000111018           O_testo = 'E'' stata manutenzionata la DATA SPEDIZIONE +
195100111019                      di bolle immesse da almeno xx giorni.' ;
195200111019           %subst(O_testo:71:2) = (%editc(§3iggm: '4'))     ;
195300111018           except PrtDet;
195400111019           O_testo = 'Verificare col mittente o procedere alla lor+
195500111019                      o chiusura come "Merce MAI Affidata".' ;
195600111019           except PrtDet;
195700111018
195800111018         // Stampa una riga vuota
195900111018           clear O_testo;
196000111018           except PrtDet;
196100111018
196200111018         // Stampa intestazione
196300111019           O_testo = ' Bolle spostate in sospensione di          Data imm+
196400111019                     issione    Bolle da vericare elencate';
196500111018           except PrtDet;
196600111019           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
196700111019                     chia       in apposita stampa.Utente:';
196800111019           %subst(o_testo:90:10)= knmus     ;
196900111019           except PrtDet;
197000111018
197100111018         // Stampa una riga vuota
197200111018           clear O_testo;
197300111018           except PrtDet;
197400111019           zz = 1 ;
197500111019
197600111019          dow     invbo(zz)>0   ;
197700111019          dataiso=%date(invdim(zz))  ;
197800111019          data_eur=dataiso  ;
197900111019
198000111019           clear O_testo;
198100111019           %subst(O_testo:6:20) = invcod(zz)  ;
198200111019           %subst(O_testo:46:10) =%char(data_eur)  ;
198300111019           %subst(O_testo:64:7 ) =%editc(invbo(zz):'2')  ;
198400111019           except PrtDet;
198500111019
198600111019           zz=zz+1  ;
198700111019           enddo    ;
198800111018
198900111018       ENDSR;
199000111018
199100111018       //--------------------------------------------------------------
199200111018       //?Override al file di stampa per impostarvi i dati per
199300111018       //?  l'invio via e-mail   +   stampa inizio e-mail
199400111018       //--------------------------------------------------------------
199500111018       BEGSR sr_Override;
199600111018
199700111018         reset TRTCM1ds;
199800111018
199900111018         IF  §MRAdreg <> *blank;
200000111018           §CM1mitt = %trim(§MRAdmitt);
200100111018           §CM1dst  = %trim(§MRAddest);
200200111019
200300111019         // Valorizzo i campi della e-m@ail
200400111019          chain  simpou   azorg01l ;
200500111019
200600111019          if   %found(azorg01l) ;
200700111019           §cm1dst=%trim(§cm1dst) +'POC'+%editc(orgf70:'X')+'@bartolini.it;'  ;
200800111019          endif  ;
200900111019
201000111018           §CM1tips = §MRAdreg;
201100111018           §CM1po   = %editc(simfel:'X');
201200111019           §CM1var  = '*OBJM*' + 'Manutenzione DATA SPEDIZIONE bolle'   ;
201300111018           §CM1idp  = §MRAdidpro;
201400111018           Qcmd = C_CmdOvrPrtF
201500111018                + ' outq(' + %trim(§MRAdoutqi) + ')'
201600111018                + ' usrdfndta(''' + TRTCM1ds + ''')';
201700111018         ELSE;
201800111018           Qcmd = C_CmdOvrPrtF;
201900111018         ENDIF;
202000111018
202100111018         callp(e) qCmdExc(Qcmd : %size(Qcmd));
202200111018
202300111018       ENDSR;
202400111018
202500111018       //--------------------------------------------------------------
202600111018       //?Operazioni finali.
202700111018       //--------------------------------------------------------------
202800111018       BEGSR RoutEnd;
202900111018
203000111018         if %open(PrtEMAIL);
203100111018
203200111018         //?Chiusura dello spool?
203300111018           clear O_testo;
203400111018           except PrtDet;
203500111018           O_testo = '***   Fine Lista   ***';
203600111018           except PrtDet;
203700111018
203800111018           close PrtEMAIL;
203900111018
204000111018         //?Eliminazione overflow?
204100111018           Qcmd = C_CmdDltOvr;
204200111018           callp(e) qCmdExc(Qcmd : %size(Qcmd));
204300111018
204400111018         endif;
204500111018
204600111018       ENDSR;
204700111018
204800111018      /end-free
204900111018     c*-------------------------------------------------------------------
205000061117     Oqsysprt   E    80n80   DET         1  1
205100061005     o                                              'Bolla:'
205200061005     O                       BLPaas        Z   +  1
205300061005     O                       BLPlnp            +  1
205400061005     O                       BLPNRS        Z   +  1
205500061005     O                       BLPNSP        Z   +  1
205600061005     O                       BLPmgs            +  1 '  /  '
205700061005     o               80                          48 'Cliente    <>'
205800061005     O               80      wksc              +  1
205900061005     o               81                          48 'Terminal   <>'
206000061005     o               82                          48 'Lin Arrivo <>'
206100061005     o               83                          48 'BLPDT1  <>  0'
206200061005     o               84                          48 'Esiste spunta'
206300061005     o               85                          49 'Bolla recupero'
206400061005     o               86                          48 'Prepagato    '
206500061005     o               87                          55 'Prepagato con numfat'
206600030619
206700111018     o* ---------------------------------------------------------------
206800111018     o* ?Spool di stampa (per e-mail).
206900111018     o* ---------------------------------------------------------------
207000111018     oPrtEMAIL  e            PRTdet      1
207100111018     o                       O_testo
207200111018
207300030619**  $Cmd
207400030707OVRPRTF FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
207500030619           USRDTA('Manut.Boll') SHARE(*YES)
207600030619           COPIES(2)
207700060403**  $Cmd2
207800060403OVRPRTF FILE(FNLS03P) SAVE(*YES)
207900061005**  $Cmd3
208000061005OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES) OUTQ(LJ4200CED) USRDTA('EDPES')
208100061012**  DLTOVR
208200061012DLTOVR  FILE(QSYSPRT)
208300060713**         MSG
208400111017MANUTENZIONE DATA SPED.BOLLE oltre xx giorni
208500111018Utente:  xxxxxxxxxx
208600060713Codice Cliente:  xxxxxxx   x
208700060713Bolle manutenzionate: xxx.xxx.xxx
208800081210Sono state manutenzionate
208900070612Se immesso tipo immettere anche la data  Consegna Richiesta
209000070612Data cons.richiesta formalmente errata o inferiore/uguale alla data odierna    4
209100070612Data consegna richiesta non ammessa: oltre                                     5
209200070612Data cons.richiesta  inferiore  alla nuova data spedizione
209300070614Ora consegna richiesta errata
209400090401Linea di partenza:  xxx
209500110704Immettere filiale di emissione valida                                         12
209600110704Immettere numero ORM esistente in archivio                                    13
209700111017Bolle immesse dal                                                             14
