000100130313     /*PRM dbgview(*source)
000200130313     /*END
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400980129     H* FNLS03R *----------------------------------------------------*
000500980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000600111122     H***------------------------------------------------------------*
000700980129     FFNLS03D   CF   E             WORKSTN
000800060404     FFNLS03p   o    E             printer usropn  oflind(*in98)
000900910207     FAZORG01L  IF   E           K DISK
001000151207     F***tntbe02l  IF   E           K DISK
001100070117     FfNbrv07l  IF   E           K DISK
001200170502     Ffnlbl01l  IF   E           K DISK
001300110704     Ffnorm01l  IF   E           K DISK
001400980130     FAZCAE01L  IF   E           K DISK
001500980202     FAZCLN01L  IF   E           K DISK
001600980130     FTABEL00F  IF   E           K DISK
001700021203     FFIAPD01L  IF   E           K DISK
001800910205     FCNACO00F  IF   E           K DISK
001900081203     FFNBLP70L  IF   E           K DISK    infds(fnblpinf)
002000081203     FFNBLP01L  UF   E           K DISK    rename(fnblp000:fnblp001)
002100110705     f                                     infds(blpnrr01)
002200980130     FFNBLT01L  UF   E           K DISK
002300010326     FFISGN05L  UF   E           K DISK
002400050318     FfIAR601L  UF   E           K DISK
002500080331     FfIAR401L  UF   E           K DISK
002600110704     FFiar404L  IF   E           K DISK    RENAME(Fiar4000:Fiar404)
002700151204     Ffibsp02l  IF   E           K DISK
002800151207     Ffibsp03l  IF   E           K DISK    rename(fibsp000:fibsp03)
002900050318     FfIAR701L  UF   E           K DISK
003000160205     Ffnanm01l  iF   E           K DISK
003100111018     FQSYSPRT   O    F  132        PRINTER OFLIND(*InOA)  usropn
003200111018
003300111018     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003400020610
003500941227     D L1              S              3  0 DIM(30)
003600020610     d sklnp           s              3  0 dim(30)
003700020610
003800030619     d $Cmd            s             80    dim(3) ctdata perrcd(1)
003900060403     d $Cmd2           s             80    dim(1) ctdata perrcd(1)
004000061005     d $Cmd3           s             80    dim(1) ctdata perrcd(1)
004100061012     d DLTOVR          s             80    dim(1) ctdata perrcd(1)
004200130729     D MSG             S             78    DIM(16) CTDATA PERRCD(1)
004300980202     D K31             S              1    DIM(31)
004400980202     D*
004500980202     D KSS             S              7    DIM(12)
004600070612     D elabor          S             54    DIM(6)
004700111019     D invbo           S              5  0 DIM(51)
004800111019     D invcod          S             25    DIM(51)
004900111019     D invdim          S              8  0 DIM(51)
005000020610
005100020610     d lnp             s              3  0
005200020610     d xx              s              2  0
005300111018     d zz              s              2  0 inz
005400030619      *
005500030619     d Qlen            s             15  5 inz(150)
005600030619      *
005700130718     d Wbsp            s              1    inz
005800160111     d Wbspchf         s              1    inz
005900030619     d Wfile           s             10    inz
006000030619     d Woutq           s             10    inz
006100030619     d Wform           s             10    inz
006200060405     d data_eur        s               D   datfmt(*eur)
006300070612     d Dataiso         s               d   datfmt(*iso)
006400070612     d woggidcgm1      s              8  0
006500070612     d woggidcgm2      s              8  0
006600070612     d s_woggidcg      s                   like(woggidcgm1)
006700070612     d s_vpodcg        s                   like(§vpodcgm1)
006800021203      *
006900111018
007000111018      // - Status
007100111018     d Psds           sds
007200111018     d   SDSpgm          *proc
007300111018     d   JobUser             254    263
007400111018
007500111018      // - Campi per QCMDEXC
007600111018     d Qcmd            s            512    inz
007700111018     d
007800111018      // - Esecuzione comando di sistema
007900111018     d qCmdExc         pr                  extpgm('QCMDEXC')
008000111018     d  Qcmd                        512    const  options(*varsize)
008100111018     d  Qlen                         15  5 const
008200111018
008300021203      * Ds esterna per ricerca codice padroncino
008400021203     D FNLV24DS      E DS
008500051209     D* DS PER TIBS02R - GESTIONE TNTBE00F
008600051209     D TIBS02DS      E DS
008700111018      // - Tabella "MRA" = Bart-Mailing - Danni
008800111018     d dMRAdan       e ds                  inz
008900130726      * Ds esterna per ricerca cliente da tab. "BSP"
009000151201     d fnlsbsp1ds    e ds                  inz
009100111018
009200070612     d
009300070612     D Dvpo          E DS
009400070903     D og143         E DS
009500081211     d
009600081211     dwidmsall         ds
009700081211     Dwidms1                         32
009800081211     Dwidms2                         32
009900081211     Dwidms3                         32
010000081211     Dwidms4                         32
010100110704     D                 DS
010200110704     D  V1CPOE                 1      3  0
010300110704     D  V1CNSR                 4      5  0
010400110704     D  V1CNOR                 6     12  0
010500110704     D  V1CNRV                13     14  0
010600110704     D  SEL4                   1     14  0
010700081211     D***
010800941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010900980202     D WGIDAT          DS                  INZ
011000980202     D  GIODAT                 1      8  0
011100980202     D  GIOINV                 9     16  0
011200980202     D  GIOTGI                17     21  0
011300941227     D***
011400941227     D WLBDAT          DS                  INZ
011500941227     D  G02DAT                 1      8  0
011600941227     D  G02INV                 9     16  0
011700941227     D  G02ERR                17     17
011800941227     D  G02TGI                18     22  0
011900980202     D                 DS
012000980202     D  WAA                    1      4  0
012100980202     D  WMM                    5      6  0
012200980202     D  WGG                    7      8  0
012300980202     D  WDATA                  1      8  0
012400060403
012500060403     d WDAT8           DS
012600060403     d  datada                 1      8  0
012700060403     d  dataa                  9     16  0
012800060403     d  ggl                   17     21  0
012900060713     D                 DS
013000110704     D  blpaas                 1      4  0
013100110704     D  blpmgs                 5      8  0
013200110704     D  blpdsp                 1      8  0
013300060926     d
013400060926     D                 DS
013500130729     D  W1cksc                 1      7  0
013600060926     D  v1clnp                 8     10  0
013700061003     D  wclna                 11     13  0
013800060926     D  v1ctfa                14     16  0
013900060926     D  v1cnos                17     17
014000060926     D  v1cggl                18     18  0
014100060926     D  v10dsp                19     26  0
014200060926     D  v11dsp                27     34  0
014300061012     D  Wcpdr                 35     41  0
014400070612     D  V1CTCR                42     42
014500070612     D  wdcr                  43     50  0
014600070612     D  V1CHCR                51     54  0
014700070612     D PARAM                   1     54
014800130711
014900130711     D fnlsu3ds        DS
015000130711     D  ilsu3flg               1      1
015100130711     D  ilsu3aas               2      5
015200130711     D  ilsu3lnp               6      8
015300130711     D  ilsu3nrs               9     10
015400130711     D  ilsu3nsp              11     17
015500130711     D  ilsu3DSP_B            18     25
015600130711     D  ilsu3DSP_S            26     33
015700130711     D  ilsu3nos              34     34
015800130711     d
015900130711     d  ilsu3tcr              35     35
016000130711     d  ilsu3dcr              36     43
016100130711     d  ilsu3hcr              44     47
016200130711     D  olsu3err             100    101
016300130711     D  olsu3boagg           102    110
016400130830     D  olsu3bonagg          111    116
016500150703     D  olsu3msg             117    193
016600150703     D  olsu3sosp            194    194
016700170419     D  Ilsu3NOAGG           201    201
016701180110     D  Ilsu3Fcmt            202    202
016702180110     D  Ilsu3Flo             203    212
016800151207     d                 ds
016900151207     d bsplin
017000151207     d sbsp                           3  0 dim(30) overlay(bsplin)
017100130711     d
017200060403
017300941116     D KPJBA         E DS
017400130729
017500130729       // -?Parametri x Controllo profilo utenti?
017600130729     d TIBS34ds      e ds
017700130729       // -?Ds di riferimento al file esterno AZUTE00F?
017800130729     d AZUTEds       e ds                  extname(AZUTE00F)
017900130729       // -?Ds per dati organigramma?
018000130729     d dDATIUTE      e ds
018100130729
018200130327     *** * Parametri per stampa BOLLE ed ETICHETTE
018300130327     ***d TRUL90DS      e ds                  inz
018400130327     ***d   D90rse      e                     inz('N')
018500130327     ***d   D90rsb      e                     inz('S')
018600130327     *** * DS per pgm di STAMPA BOLLE
018700130327     ***d FNLSB5DS      e ds                  inz
018800130327     ***d FNLSB6ds1     e ds                  inz
018900130327     ***d  DB6pdf       e                     inz('F')
019000081203      * Ds per pgm che mette in sospensione rinumerando la spedizione
019100081203     d FNLS65DS      e ds                  inz
019200030619      *
019300980202     D* DS PER TRUL06R - CARICAMENTO £X
019400941227     D DSUL06        E DS                  EXTNAME(TRUL06DS)
019500941227     D  LIN                    1     90  0
019600941227     D                                     DIM(30)
019700980129     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
019800980129     D DSLV55        E DS                  EXTNAME(FNLV55DS)
019900081203     D fnblpinf        ds
020000110705     D  fnblpnrri            397    400i 0
020100110705     D blpnrr01        ds
020200110705     D  fnblpnrr01           397    400i 0
020300081203      * DS Esterna x gestione allocazione ed invio messaggi
020400081203     D TRUL82DS      E DS
020500980130     D DS3A          E DS
020600090427     D DS1B          E DS
020700081203     D Ds3c          E DS
020800980202     D DS3I          E DS
020900130717     D*DCLI          E DS
021000140205     D og140         E DS
021100021203
021200021203     D kpdpdr          S                   LIKE(APDpdr)
021300021203     D kpdtip          S                   LIKE(APDtip)
021400151207     D*kcod            S                   LIKE(tbecod) inz('BSP')
021500151207     D*klin            S                   LIKE(tbelin) inz(' ')
021600151207     D*ksif            S                   LIKE(tbesif)
021700110704     D ktrc            S                   LIKE(ar4trc)
021800110704     D ktrcM           S                   LIKE(ar4trc) inz('M')
021900081205     D wlpnsp          S                   LIKE(blpnsp)
022000081205     D wlpaas          S                   LIKE(blpaas)
022100081210     D wmsg            S                   LIKE(v1cmsg)
022200090126     D ntwlnp          S              1    inz
022300060926     D YY              S              3  0 inz
022400111214     D savtfp          S              3  0 inz
022500080620     D sosdaL1         S              1    inz
022600110704     D Indx            S              3  0 inz
022700111018     d O_testo         s            132    inz
022800151207     d IX              s              4  0
022900160205     d comcaa          s                   like(anmcaa)
023000060713     D* DEFINIZIONE COSTANTI
023100060404     D Ctitolo         C                   CONST('ELENCO SPEDIZIONI IN SOSPENSI-
023200060405     D                                     ONE DA OLTRE X GIORNI')
023300081203     D msgjob          C                   CONST('Si sta bloccando la manutenzi-
023400081203     D                                     one data spedizione:USCIRE un moment-
023500081203     D                                     o da questo lavoro')
023600130111     D*ms2wdw          C                   CONST('Probabilmente in manutenzione-
023700130111     D*                                       ')
023800130111     D*ms3wdw          C                   CONST('da altro utente.             -
023900130111     D*                                       ')
024000130111     D*ms4wdw          C                   CONST('Rieseguire il lancio.        -
024100130111     D*                                       ')
024200130111     D ms2wdw          C                   CONST('                             -
024300130111     D                                        ')
024400130111     D ms3wdw          C                   CONST('Segnalare al CED di verificar-
024500130111     D                                     e  ')
024600130111     D ms4wdw          C                   CONST('il JOBLOG                    -
024700130111     D                                        ')
024800130729
024900130729       // -?Costante per controllo "caratteri solo numerici"?
025000130729     d c_Digits        c                   const('0123456789')
025100111018
025200111018      // - Comando di override al PrtF
025300111018     d C_CmdOvrPrtF    c                   const('OVRPRTF +
025400111018     d                                           file(PRTEMAIL) +
025500111018     d                                           pagesize(66 132) +
025600111018     d                                           lpi(6) cpi(10) +
025700111018     d                                           ovrscope(*actgrpdfn) +
025800111018     d                                           ')
025900111018     d C_CmdDltOvr     c                   const('DLTOVR +
026000111018     d                                            file(PRTEMAIL) +
026100111018     d                                            lvl(*actgrpdfn)')
026200111018      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
026300111018     d TRTCM1ds      e ds                  inz
026400111018      //   ·§CM1mitt = Indirizzo e-mail del mittente
026500120820     d   §CM1mitt    e                     inz(' ed@brt.it')
026600111018      //   ·§CM1dst  = Indirizzo e-mail del destinatario
026700120302     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
026800111018      //   ·§CM1tips = Tipo lettera via e-mail:
026900111018      //    "LET" = testo allegato in corpo con logo
027000111018      //            (richiede righe libere iniziali per il logo)
027100120820      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
027200111018      //            (non consente né UNDERLINE né HIGHLIGHT)
027300120820     d   §CM1tips    e                     inz('COI')
027400111018      //   ·§CM1po   = Filiale
027500111018     d   §CM1po      e                     inz('046')
027600111018      //   ·§CM1var  = Oggetto e-mail
027700111018     d   §CM1var     e                     inz('*OBJM*+
027800111019     d                                     Manutenzione data spedizione+
027900111019     d                                      bolle ')
028000111018      //   ·§CM1sts  = Stato
028100111018     d   §CM1sts     e                     inz(*off)
028200111018      //   ·§CM1sts  = Id processo
028300111018     d   §CM1idp     e                     inz('2')
028400111018
028500111018
028600910412     C****************************************************************
028700910412     C*  RIEPILOGO INDICATORI
028800910412     C***************************************************************
028900070612     C* 01    - possibilità di richidere bolle in sospensione x colli non
029000070612     c*         spuntati
029100070612     C* 02    -
029200070612     C* 03    - per colli non spuntati possibilità di modificare la consegna
029300130717     c*         richiesta da tabella BSP
029400070612     C* 12    - PROTEZIONE CAMPO LINEA DI PARTENZA XCHE' SONO UN 2° LIVELLLO
029500910412     C* 30    - DI COMODO
029600980129     C* 40-50 - ERRORE
029700980130     C* 51-55 - GESTIONE CHIAVE LETTURA BOLLE
029800070612     C* 70    - posizionam.cursone su DCR
029900070612     C* 90    - ERRORE GENERICO
030000910412     C*****************************************************************
030100941116     C**
030200980130     C     *ENTRY        PLIST
030300980130     C                   PARM                    KPJBA
030400030619      *
030500130327     *** * Richiedo i dati per la stampa ed eseguo le override
030600130327     ***c                   exsr      SR_OvrPrtF
030700130327     ***C**
030800130327     ***C* F3 - FINE
030900130327     ***C     D90F3         IFEQ      '1'
031000130327     ***C                   GOTO      FINE
031100130327     ***C                   ENDIF
031200130327     *** * Errori nelle OVERRIDE -> FINE
031300130327     ***C   91              GOTO      FINE
031400991206     C*
031500950411     C     INIZIO        TAG
031600980130     C                   EXSR      PUL01
031700950411     C*
031800980130     C                   MOVE      WDTGIO        WAAS              4 0
031900980130     C                   MOVEL     CDU           V1CLNP
032000950411     C**
032100900509     C     FOR01         TAG
032200980129     C                   EXFMT     LS03D01
032300941227     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
032400110704     C                   SETOFF                                       902873
032500110704     C                   SETOFF                                       707172
032600130711     C                   SETOFF                                       74
032700081210     c                   clear                   v1cmsg
032800130711     C** F3 - FINE LAVORO
032900900509     C   KC              GOTO      FINE
033000130711     c
033100130726     c* F11-  seleziona  tabella sospensione senza codici senza spunte
033200130711     c*       tabella DSP
033300130711     c                   if        *inkk=*on
033400130729      * Sola visualizzazione di TUTTI i clienti (di qualsiasi filiale)
033500130729      *   in tab. "BSP"
033600151201     c***                clear                   tntbBSPds1
033700151201     c***                eval      iBSPmod = 'V'
033800151201     c***                eval      iBSPtfp = DUTtfp
033900151201     c***                eval      KPJBU = tntbBSPds1
034000151201     c***                call      'TNTBBSPR1'
034100151201     c***                parm                    KPJBA
034200151201     c***                eval      tntbBSPds1 = KPJBU
034300151201     c***                goto      for01
034400151216      * Sola visualizzazione  dei codici del terminal di partenza
034500151201     c                   clear                   fnlsbsp1ds
034600151201     c                   eval      iBSPmod = 'V'
034700151216     c                   eval      iBSPtfp = DUTtfp
034800151201     c                   eval      KPJBU = fnlsbsp1ds
034900151201     c                   call      'FNLSBSP1R'
035000151201     c                   parm                    KPJBA
035100151201     c                   eval      fnlsbsp1ds = KPJBU
035200151201     c                   goto      for01
035300151201     c                   endif
035400130711     c
035500910205     C* CONTROLLI FORMATO
035600980130     C                   EXSR      CNT01
035700980202     C  NKF
035800130711     CANNKH
035900941228     COR 90              GOTO      FOR01
036000130711     c
036100130711     c                   if        *inkh=*on
036200130711     c                   clear                   fnlsu3ds
036300130711     c                   eval      ilsu3dsp_B=%editc(wdtcar:'X')
036400130711     c                   eval      ilsu3dsp_S=%editc(wlpdsp:'X')
036500130711     c                   eval      ilsu3nos='S'
036600130711     c                   movel     fnlsu3ds      kpjbu
036700130711     C                   MOVEL     'LSU3'        KCOAZ
036800130711     C                   CALL      'FNLSU3R'
036900130711     C                   PARM                    KPJBA
037000130711     c*
037100130711     c                   movel     kpjbu         fnlsu3ds
037200130711
037300130711     c                   if        olsu3boagg>*zeros
037400130711     c                   movel     olsu3boagg    bollema
037500130830     c                   endif
037600130830     c                   if        olsu3bonagg>*zeros
037700130830     c                   movel     olsu3bonagg   werror
037800130830     c                   endif
037900130830     c                   eval      wmsg=olsu3msg
038000130711     c                   exsr      emis_msg
038100130830
038200130830     c                   if        werror>0
038300130830     c                   do        *hival
038400130830     c                   exfmt     ls03w01
038500130830     c                   if        *inkl='1'
038600130830     c                   leave
038700130830     c                   endif
038800130830     c                   enddo
038900130830     c                   endif
039000130711     c
039100130711     c                   goto      inizio
039200130711e   1C                   ENDIF
039300900523     C*
039400980129     C* M O D I F I C A   D A T A   S P E D I Z I O N E
039500130327     ***C   KH
039600130327     ***COR KF              EXSR      MOD01
039700130327     C   KF              EXSR      MOD01
039800130711     c
039900081211     c                   if        werror>0
040000081211     c                   do        *hival
040100081211     c                   exfmt     ls03w01
040200081215     c                   if        *inkl='1'
040300081211     c                   leave
040400081211     c                   endif
040500081211     c                   enddo
040600081211     c                   endif
040700081211
040800910207     C*
040900950411     C                   GOTO      INIZIO
041000000000     C     FINE          TAG
041100980202     C**
041200130327     ***C     D90F3         IFne      '1'
041300130327if  1***c                   if        DB6num > *zero
041400130327     ***c                   clear                   FNLSB5ds
041500130327     ***c                   eval      DB0tbo = 'P'
041600130327     ***c                   eval      DB0fa4 = 'I'
041700130327     ***c                   eval      DB6pdf = 'S'
041800130327     ***c                   call      D90psl
041900130327     ***c                   parm                    FNLSB5ds
042000130327     ***c                   parm                    FNLSB6ds1
042100130327e   1***c                   endif
042200130327     ***c                   clear                   FNLSB5DS
042300130327     ***c                   movel     'C'           DB0TLA
042400130327     ***c                   call      D90psl
042500130327     ***c                   parm                    FNLSB5DS
042600130327     ***c                   endif
042700980202     C*
042800980202     C                   CLEAR                   DSLV55
042900980202     C                   MOVEL     'C'           D55TLA
043000980202     C                   CALL      'FNLV55R'
043100980202     C                   PARM                    DSLV55
043200081203     c*
043300081203     C                   CLEAR                   fnls65ds
043400081203     C                   MOVEL     'C'           i65tla
043500081211     c                   movel     '1'           I65STRCMT
043600081203     c                   movel     fnls65ds      kpjbu
043700081205     C                   CALL      'FNLS65R'
043800081203     C                   PARM                    kpjba
043900980202     C**
044000060926     c                   clear                   kpjbu
044100060926     c                   movea     elabor        kpjbu
044200060926     c                   if        kpjbu<>*blanks
044300060926     C                   MOVEL     'LS0G'        KCOAZ
044400060926     C                   CALL      'BCH10'
044500060926     C                   PARM                    KPJBA
044600060926     c                   endif
044700060926     c
044800061012     c
044900061012     c                   clear                   qcmd
045000061012     c                   eval      qcmd = dltovr(1)
045100061012     c                   call      'QCMDEXC'                            91
045200061012     c                   parm                    Qcmd
045300061012     c                   parm                    Qlen
045400061012     c
045500111018     c                   if        invbo(1)>0
045600111018     c                   exsr      Inv_email
045700111018     c                   endif
045800111018     c
045900000000     C                   SETON                                        LR
046000941116     C*
046100980129     C*****************************************************************
046200910205     C* CONTROLLO FORMATO -------------------------------------------
046300980129     C*****************************************************************
046400980130     C     CNT01         BEGSR
046500910205     C                   SETOFF                                       90
046600130729      *
046700130729     c                   clear                   W1Cksc
046800130729     c                   Select
046900130729      * - Ricerca da tab. "BSP"
047000130729     c                   When      %scan( '?' : V1Cksc ) > *zero
047100130729     c                   clear                   V1Cksc
047200151201     c****               clear                   tntbBSPds1
047300151201     c****               eval      KPJBU = tntbBSPds1
047400151201     c****               call      'TNTBBSPR1'
047500151201     c****               parm                    KPJBA
047600151201     c****               eval      tntbBSPds1 = KPJBU
047700151216      *   Visualizzazione di tutti i cliente a prescindere da tfp
047800151216      *   esclusione dei codici che partono da + filiali
047900151201     c                   clear                   fnlsbsp1ds
048000151204     c                   eval      ibsptps='K'
048100160111     c                   eval      iBSPfpa = 'C'
048200151201     c                   eval      KPJBU = fnlsbsp1ds
048300151201     c                   call      'FNLSBSP1R'
048400151201     c                   parm                    KPJBA
048500151201     c                   eval      fnlsbsp1ds = KPJBU
048600130729     c                   if        oBSPfxx <= *zero  and
048700151204     c                             oBSPerr  = *blanks
048800130729     c                   eval      W1Cksc = oBSPcli
048900130729     c                   eval      V1Cksc = %editc( oBSPcli : 'X' )
049000130729     c                   eval      V1Dksc = oBSPdes
049100130729     c                   endif
049200130729     c                   eval      *in74 = *on
049300130729     c                   eval      *in90 = *on
049400130729     c                   leavesr
049500130729      * - Verifica se contiene solo caratteri numerici
049600130729     c                   When      V1Cksc <> *blanks  and
049700130729     c                             %checkR( c_Digits : V1Cksc ) > *zero
049800130729     c                   movel     Msg(16)       V1Cmsg
049900130729     c                   eval      *in28 = *on
050000130729     c                   eval      *in74 = *on
050100130729     c                   eval      *in90 = *on
050200130729     c                   leavesr
050300130729     c                   EndSl
050400130729      *
050500130729     c                   if        V1Cksc > *zeros
050600130729     c                   eval      W1Cksc = %int( V1Cksc )
050700130729     c                   endif
050800130729      *
050900130711     c* Se ho premuto F8 tutte le selezioni ddevono essere vuote
051000130711     c*  tranne le date
051100130711     c                   if        *inkh=*on
051200130711     c
051300130729     c                   if        W1cksc>*zeros or
051400130711     c                             v1clna>*zeros or
051500130711     c                             v1ctfa>*zeros or
051600130711     c                             v1cpdr >*zeros or
051700130711     c                             sel4>0
051800130711     C                   MOVEL     MSG(15)       V1CMSG
051900130711     C                   SETON                                        287490
052000130711     C                   GOTO      ENDCTR
052100130711     C                   ENDIF
052200130729     c*
052300130711     c                   else
052400910412     C*
052500980129     C* SE TUTTE LE SELEZIONI SONO A BLANKS E' ERRORE
052600130729     C     W1CKSC        IFEQ      *ZEROS
052700980130     C     V1CLNP        ANDEQ     *ZEROS
052800980129     C                   SETON                                        4090
052900941116     C                   GOTO      ENDCTR
053000941116     C                   ENDIF
053100051118     c
053200980129     C* CODICE CLIENTE
053300130729    1C     W1CKSC        IFEQ      0
053400980130    2C     V1DKSC        IFNE      *BLANKS
053500980129     C* PARSTA = 9  ESCLUDO ANNULLATI
053600980129     C                   Z-ADD     9             PARSTA
053700980129     C                   MOVEL     RSUT          PARDUT           30
053800980129     C                   MOVEL     *BLANKS       DESCR            48
053900980130     C                   MOVEL     V1DKSC        DESCR
054000130729     C                   Z-ADD     dutKCI        CCC               4 0
054100981109     C                   CLEAR                   PARFLR
054200981109     C                   CLEAR                   PARDIT
054300000907     C                   Z-ADD     1             PAXNUM
054400000907     C                   CALL      'XALFA3BR'
054500980129     C                   PARM                    PARDUT
054600980129     C                   PARM                    CODUT
054700980129     C                   PARM                    DESCR
054800980129     C                   PARM                    CCC
054900980129     C                   PARM                    PARSTA            1 0
055000981109     C                   PARM                    PARFLR           90
055100981109     C                   PARM                    PARDIT            3
055200000907     C                   PARM                    PAXNUM            2 0
055300000907     C                   PARM                    PAXKCM           80
055400000907     C                   PARM                    PAXKSM          140
055500000907     C                   PARM                    PAXKDM           60
055600980129     C     PARSTA        IFNE      -1
055700130729     C                   MOVEL     PAXKSM        W1CKSC
055800000907     C                   MOVEL     PAXKSM        V1CKSC
055900980130     C                   MOVEL     DESCR         V1DKSC
056000980129     C                   ELSE
056100980202     C                   CLEAR                   V1DKSC
056200980129     C                   ENDIF
056300980129     C*
056400980129     C                   SETON                                        90
056500980129     C                   GOTO      ENDCTR
056600980129    2C                   ENDIF
056700980129   X1C                   ELSE
056800980129     C*
056900980129     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
057000130729     C                   Z-ADD     dutKCI        CCC
057100130729     c                   mllzo     1             W1cksc
057200980129     C     KACO          CHAIN     CNACO000                           41
057300980129    2C  N41ACOFLG        IFNE      ' '
057400980129     C                   SETON                                        41
057500980129    2C                   ENDIF
057600980129     C     *IN41         IFEQ      *OFF
057700980202     C                   MOVEL     ACORAG        V1DKSC
057800980129     C                   ELSE
057900980204     C                   CLEAR                   V1DKSC
058000980129     C                   SETON                                        90
058100980129     C                   GOTO      ENDCTR
058200980129     C                   END
058300980129    1C                   ENDIF
058400130729     c*
058500980130     C* LINEA DI PARTENZA
058600061003     c                   mllzo     1             v1clnp
058700090126     c                   clear                   ntwlnp
058800980130    1C     V1CLNP        IFNE      *ZEROS
058900980130     C* CONTROLLO CHE SIA GESTIBILE
059000980130     C     V1CLNP        LOOKUP    L1                                     30
059100980130    2C     *IN30         IFEQ      *OFF
059200980130     C                   SETON                                        9042
059300980204     C                   CLEAR                   V1DLNP
059400980130     C                   GOTO      ENDCTR
059500980130    2C                   ENDIF
059600980130     C*
059700090126     c                   clear                   og143
059800980130     C     V1CLNP        CHAIN     AZORG01L                           30
059900980130     C   30              SETON                                        9042
060000980204     C   30              CLEAR                   V1DLNP
060100980130     C   30              GOTO      ENDCTR
060200980130     C  N30              MOVEL     ORGDES        V1DLNP
060300090126     c  n30              movel     orgde3        og143
060400090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
060500090126     c                             §ogntw='FED'
060600090126     c                   eval      ntwlnp='E'
060700090126     c                   endif
060800980130    1C                   ENDIF
060900980130     C* LINEA DI ARRIVO
061000061003     c                   clear                   wclna
061100980130    1C     V1CLNA        IFNE      *BLANKS
061200980130     C     V1CLNA        ANDNE     *ZEROS
061300980130     C*
061400980130     C     '?'           SCAN      V1CLNA                                 30
061500980130     C*
061600980130    2C     *IN30         IFEQ      *ON
061700980130     C                   MOVE      *BLANKS       §COD1
061800980130     C                   MOVE      *BLANKS       V1CLNA
061900980130     C                   CALL      'TNSD19R'
062000980130     C                   PARM                    §COD1             3
062100980130     C                   PARM                    §TIP              1
062200980202     C                   PARM                    §DES             25
062300980130     C                   MOVEL     §COD1         V1CLNA
062400980130     C                   MOVEL     §DES          V1DLNA
062500980130     C                   SETON                                        90
062600980130     C                   GOTO      ENDCTR
062700980130    2C                   ENDIF
062800980130     C* ESEGUO TESTN
062900980130     C                   TESTN                   V1CLNA               30
063000980130     C                   MOVE      V1CLNA        W001A
063100980130     C     *IN30         IFEQ      *OFF
063200980130     C     W001A         ORLT      '0'
063300980204     C                   CLEAR                   V1DLNA
063400980130     C                   GOTO      ENDCTR
063500980130     C                   ENDIF
063600980130     C*
063700980130     C*  CONTROLLO
063800980130     C*
063900980130     C                   MOVE      V1CLNA        WCLNA             3 0
064000980130     C     WCLNA         CHAIN     AZORG01L                           30
064100980205     C* CONTROLLO SE ANNULLATO
064200980205     C* CONSIDERO SOLO I TIPI "F" E "A"
064300980205    2C  N30ORGFVA        IFNE      *BLANKS
064400980205    2C     ORGFAG        ORNE      'F'
064500980205     C     ORGFAG        ANDNE     'A'
064600980205     C                   SETON                                        30
064700980205    2C                   END
064800980130     C   30              MOVEL     *BLANKS       V1DLNA
064900980130     C   30              SETON                                        9044
065000980130     C   30              GOTO      ENDCTR
065100980130     C  N30              MOVEL     ORGDES        V1DLNA
065200980130    1C                   ENDIF
065300980130     C*
065400980130     C* CONTROLLO CODICE AUTOTRASPORTATORE
065500061003     c                   clear                   wcpdr
065600980130    1C     V1CPDR        IFNE      *BLANKS
065700980130     C     V1CPDR        ANDNE     *ZEROS
065800980130     C                   CLEAR                   V1DPDR
065900980130     C*
066000980130     C     '?'           SCAN      V1CPDR                                 30
066100980130     C*
066200980130     C     *IN30         IFEQ      *ON
066300021203     C*                  MOVEL     *BLANKS       PA2DES
066400021203     C*                  Z-ADD     0             PA2PAD
066500021203     C*                  Z-ADD     V1CLNP        PA2FIL
066600021203     C*                  MOVEL     'R'           PA2FLG
066700021203     C*                  MOVEL     PARAM2        KPJBU
066800021203      *
066900021203      * imposto i campi della ds esterna fnlv24ds
067000021203     C                   clear                   fnlv24ds
067100021203     C                   eval      d24fil   = v1clnp
067200021203     C                   eval      d24tip   = 'A'
067300021203     C                   eval      d24flg   = 'R'
067400021203     C                   movel     fnlv24ds      kpjbu
067500980130     C                   CALL      'FNLV24R'
067600980130     C                   PARM                    KPJBA
067700021203     C*                  MOVEL     KPJBU         PARAM2
067800021203     C*                  MOVE      PA2PAD        V1CPDR
067900021203     C*                  MOVEL     PA2DES        V1DPDR
068000021203      *
068100021203      * valorizzo i campi a video
068200021203     C                   movel     kpjbu         fnlv24ds
068300021203     C                   move      d24pdr        v1cpdr
068400021203     C                   movel     d24rsc        v1dpdr
068500980130     C                   GOTO      ENDCTR
068600980130     C                   END
068700980130     C* ESEGUO TESTN
068800980130     C                   TESTN                   V1CPDR               30
068900980130     C                   MOVE      V1CPDR        W001A             1
069000980130     C     *IN30         IFEQ      *OFF
069100980130     C     W001A         ORLT      '0'
069200980205     C                   CLEAR                   V1CPDR
069300980205     C                   CLEAR                   V1DPDR
069400980130     C                   GOTO      ENDCTR
069500980130     C                   ENDIF
069600980130     C*
069700021204     C                   CLEAR                   V1DPDR
069800980130     C                   MOVE      V1CPDR        WCPDR             7 0
069900021203     C*    WCPDR         CHAIN     FNAPD000                           30
070000021204     C                   move      wcpdr         kpdpdr
070100021203     C                   move      'A'           kpdtip
070200021203     C     kfapd         chain     fiapd01l                           30
070300980130     C     *IN30         IFEQ      *ON
070400980205     C                   CLEAR                   WCPDR
070500980205     C                   CLEAR                   V1DPDR
070600980130     C                   SETON                                        4590
070700980130     C                   GOTO      ENDCTR
070800980204     C                   CLEAR                   V1DPDR
070900980204     C                   ELSE
071000980204     C                   MOVEL     APDRSC        V1DPDR
071100980130     C                   ENDIF
071200980130     C*
071300980205     C* RECUPERO LA LINEA DI PARTENZA DALLA PRIMA PARTE DELL'AUTOTRASPORTATORE
071400980130     C     V1CLNP        IFEQ      *ZEROS
071500980202     C                   MOVEL     WCPDR         V1CLNP
071600090126     c                   clear                   ntwlnp
071700090126     c                   clear                   og143
071800980130     C     V1CLNP        CHAIN     AZORG01L                           30
071900980130     C  N30              MOVEL     ORGDES        V1DLNP
072000090126     C  N30              MOVEL     ORGDE3        og143
072100090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
072200090126     c                             §ogntw='FED'
072300090126     c                   eval      ntwlnp='E'
072400090126     c                   endif
072500980204     C   30              MOVEL     *BLANKS       V1DLNP
072600980205     C                   ELSE
072700151207     C* CONTROLLO LA LINEA DI PARTENZA CON QUELLA DELL'AUTOTRASPORATTORE
072800980205     C                   MOVEL     WCPDR         W0030
072900980205     C     V1CLNP        IFNE      W0030
073000980205     C     W0030         ANDNE     *ZEROS
073100980205     C                   SETON                                        56  90
073200980205     C                   Z-ADD     *ZEROS        W0030
073300980205     C                   GOTO      ENDCTR
073400980205     C                   ENDIF
073500980202     C                   ENDIF
073600980205     C                   ENDIF
073700051118
073800110704     c* controllo numero ORM
073900110704     c                   if        sel4>0
074000110704      * P.O. EMISSIONE
074100110704     C                   Z-ADD     V1CPOE        NUM3              3 0
074200110704     C     NUM3          CHAIN     AZORG01L                           30
074300110704     C     *IN30         IFEQ      *ON
074400110704    2C     ORGFVA        orne      *BLANKS
074500110704    2C     ORGFAG        ORNE      'F'
074600110704     C     ORGFAG        ANDNE     'A'
074700110704     C                   MOVEL     MSG(12)       V1CMSG
074800110704     C                   SETON                                        287290
074900110704     C                   GOTO      ENDCTR
075000110704     C                   ENDIF
075100110704      * N.ORM
075200110704     C     KFORM         CHAIN     FNORM01L                           30
075300110704     C     *IN30         IFEQ      *ON
075400110704     C                   MOVEL     MSG(13)       V1CMSG
075500110704     C                   SETON                                        287390
075600110704     C                   GOTO      ENDCTR
075700110704     C                   ENDIF
075800110704      *
075900110704     C                   SETON                                        06
076000110704      *
076100110704     C                   ENDIF
076200051118     c* Se immessa solo lnp, immettere anche cliente o linea arrivo o
076300051118     c*  terminal arrivo o autotrasportatore
076400051118     c                   if        v1clnp>0 and
076500130729     c                             (W1cksc=0 and v1clna<=*zeros and v1ctfa=0
076600110704     c                              and v1cpdr<=*zeros and v1cnos=' ' and
076700110704     c                              sel4=0)
076800051118     C                   SETON                                        6090
076900051118     C                   GOTO      ENDCTR
077000051118     C                   ENDIF
077100051209     c
077200080620     c                   if        not *in01
077300080620     c                   clear                   v1cnos
077400080620     c                   endif
077500080620     c
077600080620     c* Per mettere in sospensione  in base alle spunte devo essere
077700151204     c*  abilitato (Fibsp00F)
077800051209     C
077900130718     c                   clear                   Wbsp
078000160111     c                   clear                   Wbspchf
078100130729    1c                   if        W1cksc>0
078200130718     c***                Clear                   tibs02ds
078300130718     c***                Movel     'C'           T02Mod
078400130718     c***                Movel     KNSIF         T02Sif
078500130718     c***                Movel     'CLI'         T02Cod
078600130729     c***                Movel     W1Cksc        T02Ke1
078700130718     c***                Call      'TIBS02R'
078800130718     c***                Parm                    Kpjba
078900130718     c***                Parm                    tibs02ds
079000130718    2c***                If        T02Err = *Blanks
079100130718     c***                Movel     T02Uni        Dcli
079200130718    2c**                 endif
079300130718     c* cerco il cliente in tabella bsp
079400130718     c                   exsr      cer_bsp
079500080620    1c                   endif
079600080620
079700130718     c                   if        not *in01 and wbsp='S'
079800080620     c                   seton                                        01
079900080620     C                   SETON                                        90
080000080620     C                   GOTO      ENDCTR
080100080620     c                   endif
080200130718     c                   if        sosdaL1=' 'and *in01 and wbsp =' '
080300080620     c                   setoff                                       01
080400080620     C                   SETON                                        90
080500080620     C                   GOTO      ENDCTR
080600080620     c                   endif
080700051209     c
080800080620    1c                   if        v1cnos='S'
080900130729    2c                   if        W1cKSC=0 and ntwlnp<>'E'
081000080620     C                   SETON                                        5790
081100080620     C                   GOTO      ENDCTR
081200080620    2C                   ENDIF
081300080620     c
081400130718    2c                   if        wbsp=' ' and ntwlnp<>'E'
081500160111     c                   if        wbspchf=' '
081600051209     C                   SETON                                        58  90
081700151209     c                   else
081800151209     C                   SETON                                        61  90
081900151209     c                   endif
082000051209     C                   GOTO      ENDCTR
082100070614   x2c                   else
082200060403     c* se numero giorni per stampa = 0 --> errore
082300070614    3c                   if        v1cggl = 0
082400060403     C                   SETON                                        59  90
082500070614    3c                   endif
082600070612     c* se posso emetto il campo della consegna richiesta
082700151204    3c                   if        BSPDCRP='S' and not *in03
082800070612     c                   seton                                        0390
082900070612     c                   clear                   v1ctcr
083000070612     c                   clear                   v1cdcr
083100070612     c                   clear                   v1chcr
083200070612     C                   GOTO      ENDCTR
083300070614    3c                   endif
083400070612     c
083500151204    3c                   if        BSPDCRP=' ' and *in03
083600070612     c                   seton                                        90
083700070612     c                   setoff                                       03
083800070612     c                   clear                   v1ctcr
083900070612     c                   clear                   v1cdcr
084000070612     c                   clear                   v1chcr
084100070612     C                   GOTO      ENDCTR
084200070614    3c                   endif
084300070612     c* Altrimenti controllo la consegna richiesta
084400151204    3c                   if        BSPDCRP='S'
084500070612     c                   exsr      CtrCONSRICH
084600070614    4c                   if        *in90
084700070612     c                   goto      ENDCTR
084800070614    4c                   endif
084900070614    3c                   endif
085000070612     c
085100070614    2C                   ENDIF
085200070614   x1c                   else
085300070614     c* se cliente senza abilitazione bolla tolgo la consegna rich
085400070614    3c                   if        *in03
085500070614     c                   seton                                        90
085600070614     c                   setoff                                       03
085700070614     c                   clear                   v1ctcr
085800070614     c                   clear                   v1cdcr
085900070614     c                   clear                   v1chcr
086000070614     C                   GOTO      ENDCTR
086100070614    3c                   endif
086200070614    1C                   ENDIF
086300130711     c
086400130711     c                   endif
086500051209     c
086600980130     C* CONTROLLO DATA SPEDIZIONE DA MODIFICARE ----------------------*
086700980130     C*
086800980130     C                   Z-ADD     V10DSP        G02DAT
086900980130     C                   MOVEL     *BLANK        G02ERR
087000980130     C                   CALL      'XSRDA8'
087100980130     C                   PARM                    WLBDAT
087200980130     C* ERRATA
087300980130    1C     G02ERR        IFEQ      '1'
087400980130     C                   SETON                                        46  90
087500980130     C                   GOTO      ENDCTR
087600980130    1C                   ENDIF
087700980130     C*
087800980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
087900980130     C*  IMMESSO DA VIDEO
088000980130     C                   Z-ADD     G02DAT        V10DSP
088100980130     C*
088200980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
088300980130     C                   MOVE      V10DSP        WAAS              4 0
088400980202     C                   MOVE      G02INV        WMGS              4 0
088500980203     C                   Z-ADD     G02INV        WDTCAR            8 0
088600001120     C                   MOVEL     G02INV        WAMSPE            6 0
088700980130     C*
088800980130     C*
088900980130     C* CONTROLLO DATA SPEDIZIONE CHE SOSTITUISCE LA VECCHIA----------*
089000980130     C*
089100980130     C                   Z-ADD     V11DSP        G02DAT
089200980130     C                   MOVEL     *BLANK        G02ERR
089300980130     C                   CALL      'XSRDA8'
089400980130     C                   PARM                    WLBDAT
089500980130     C* ERRATA
089600980130    1C     G02ERR        IFEQ      '1'
089700980130     C                   SETON                                        47  90
089800980130     C                   GOTO      ENDCTR
089900980130    1C                   ENDIF
090000980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
090100980130     C*  IMMESSO DA VIDEO
090200980130     C                   Z-ADD     G02DAT        V11DSP
090300980202     C* CONTROLLO CHE LE DUE DATE SIANO UGUALI
090400980202     C     V11DSP        IFEQ      V10DSP
090500980204     C                   SETON                                        5090
090600980202     C                   GOTO      ENDCTR
090700980202     C                   ENDIF
090800980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
090900980130     C                   MOVE      V11DSP        WAASN             4 0
091000980130     C*
091100140114     c* questo controllo non lo faccio se premuto f8-sosp automatica
091200140114    2C  nkhWAASN         IFNE      WAAS
091300130729     c                   if        W1cksc<>0
091400081203     C                   MOVEL(P)  '3C'          COD
091500130729     C                   MOVEL(p)  W1cksc        KEY
091600081203     C     ktab2         chain     tabel00f                           31
091700081203     C   31              clear                   ds3c
091800081203     C  N31              movel     tbluni        ds3c
091900081203     c                   endif
092000130729     c     W1cksc        ifeq      0
092100090126     c     ntwlnp        andne     'E'
092200130729     c     W1cksc        orgt      0
092300090126     c     §3cfsp        andeq     'S'
092400980130     C                   SETON                                        4790
092500980130     C                   GOTO      ENDCTR
092600081203     c                   endif
092700980130    2C                   ENDIF
092800160107     c* Anche se tecnicamente funziona,
092900160107     c* impedisco sospensione all'indietro a cavallo di anno (anche per sosp.
093000160107     c* automatica)   - per richiesta di Luciano
093100160107     c                   if        waasn<>waas and waasn<waas
093200160107     C                   SETON                                        6290
093300160107     C                   GOTO      ENDCTR
093400160107     c                   endif
093500980130     C*
093600980130     C                   Z-ADD     G02INV        WLPDSP            8 0
093700001120     C                   MOVEL     G02INV        WAMSOS            6 0
093800980130     C* DATE SPEDIZIONE NO > DATA MAX DI SOSPENSIONE
093900980130    2C     WLPDSP        IFGT      DATAPA
094000980130     C                   SETON                                        4990
094100980130     C                   GOTO      ENDCTR
094200980130    2C                   ENDIF
094300980130     C*
094400980130     C     WLPDSP        IFLT      DATEU
094500980130     C                   SETON                                        4890
094600980130     C                   GOTO      ENDCTR
094700980130     C                   ENDIF
094800070612     c* Se immessa la consegna richiesta, controllo che non sia
094900070612     c*  inferiore alla nuova data spedizione
095000070612     c                   if        *in03 and  v1cdcr>0  and wdcr<wlpdsp
095100070612     c                   seton                                        702890
095200070612     c                   movel     msg(9)        v1cmsg
095300070612     c                   goto      endctr
095400070612     c                   endif
095500980203     C*
095600980203     C* TERMINAL DI ARRIVO SOLO DOPO AVER INSERITO LA DATA DI SPEDIZIONE
095700980203    1C     V1CTFA        IFNE      *ZEROS
095800980203     C* VERIFICO SE E' CORRETTO
095900980203     C                   MOVE      '0'           CRISP             1
096000980203     C     'A'           SETLL     AZCAE01L
096100980203     C     'A'           READE     AZCAE01L                               30
096200980203    2C     *IN30         DOWEQ     *OFF
096300980203    3C     V1CTFA        IFEQ      CAETFE
096400980203     C     CAEATB        ANDEQ     *BLANKS
096500980203     C     WDTCAR        ANDGE     CAEDDE
096600980203     C     WDTCAR        ANDLE     CAEDSC
096700980203     C     CAETFP        IFEQ      0
096800980203     C     CAETFP        OREQ      SIMFEL
096900980203     C                   MOVE      '1'           CRISP
097000980203     C                   END
097100980203    3C                   END
097200980203     C     'A'           READE     AZCAE01L                               30
097300980203    2C                   ENDDO
097400980203     C* ERRORE
097500980203    2C     CRISP         IFEQ      '0'
097600980203     C                   SETON                                        9043
097700980203     C                   MOVE      *BLANKS       V1DTFA
097800980203     C                   GOTO      ENDCTR
097900980203   X2C                   ELSE
098000980203     C* DECODIFICA
098100980203     C     V1CTFA        CHAIN     AZORG01L                           30
098200980203     C  N30              MOVEL     ORGDES        V1DTFA
098300980203    2C                   END
098400980203    1C                   END
098500980130     C*
098600130327     ***C  NKF
098700130327     ***CANNKH              SETON                                        90
098800130711     C**  NKF              SETON                                        90
098900980130     C*
099000980130     C     ENDCTR        ENDSR
099100070612     C*****************************************************************
099200070612     C* controllo dati consegna richiesta      ----------------------
099300070612     C*****************************************************************
099400070612     C     CtrCONSRICH   BEGSR
099500070612      * tipo
099600070612    1c                   If        V1cTcr <> *Blanks   and
099700070612     c                             v1cDcr = *Zeros
099800070612     c                   Eval      *In28 = *On
099900070612     c                   Eval      *In70 = *On
100000070612     c                   Eval      *In90 = *On
100100070612     c                   Eval      v1cMsg = msg(6)
100200070612     c                   GoTo      EndCons
100300070612    1c                   EndIf
100400070612      * data
100500070614     c                   clear                   wdcr
100600070612    1c                   If        V1cDcr > *Zeros
100700070612      * controllo formale della data
100800070612     c                   Clear                   wlbdat
100900070612     c                   Z-Add     V1cDcr        G02Dat
101000070612     c                   Call      'XSRDA8'
101100070612     c                   Parm                    wlbdat
101200070612    2c                   If        G02Err = '1'
101300070612     c                   Eval      *In28 = *On
101400070612     c                   Eval      *In70 = *On
101500070612     c                   Eval      *In90 = *On
101600070612     c                   Eval      v1cMsg = msg(7)
101700070612     c                   GoTo      EndCons
101800070612    2c                   EndIf
101900070612     c
102000070612     c                   Movel     G02Dat        V1cDcr
102100070612     c                   Movel     G02Inv        wdcr
102200070612      * Non deve essere inferiore/uguale a oggi
102300070612    2c                   If        wdcr <= dateu
102400070612     c                   Eval      *In28 = *On
102500070612     c                   Eval      *In85 = *On
102600070612     c                   Eval      *In90 = *On
102700070612     c                   Eval      v1cMsg = msg(7)
102800070612     c                   GoTo      EndCons
102900070612    2c                   EndIf
103000070612     c
103100070612      * Non deve essere superiore a oggi + 5 gg.
103200070612    2c                   if        v1ctcr = 'P'
103300070612     c                   z-add     woggidcgm1    s_woggidcg
103400070612     c                   z-add     §vpodcgm1     s_vpodcg
103500070612     c                   else
103600070612     c                   z-add     woggidcgm2    s_woggidcg
103700070612     c                   z-add     §vpodcgm2     s_vpodcg
103800070612    2c                   endif
103900070612    2c                   If        wdcr > s_woggidcg
104000070612     c                   Eval      *In28 = *On
104100070612     c                   Eval      *In70 = *On
104200070612     c                   Eval      *In90 = *On
104300070612     c                   Eval      v1cMsg = msg(8)
104400070612     c                   Eval      v1cMsg = %trim(v1cmsg) + ' ' +
104500070612     c                             %trim(%editc(s_vpodcg:'Z'))  +
104600070612     c                             ' giorni dalla data odierna'
104700070612     c                   GoTo      EndCons
104800070612    2c                   EndIf
104900070612    1c                   EndIf
105000070614     c* Ora richiesta
105100070614     c                   if        v1chcr>0
105200070614     c                   movel     v1chcr        whcr              2 0
105300070614     c                   move      v1chcr        wmcr              2 0
105400070614    2C     WHCR          IFLT      0
105500070614     C     WHCR          ORGT      24
105600070614     C                   MOVEL     msg(10)       V1cMSG
105700070614     C                   SETON                                        719028
105800070614     C                   GOTO      ENDCons
105900070614    2C                   ENDIF
106000070614     C*
106100070614    2C     WMCR          IFLT      0
106200070614     C     WMCR          ORGT      59
106300070614     C                   MOVEL     msg(10)       V1cMSG
106400070614     C                   SETON                                        719028
106500070614     C                   GOTO      ENDCons
106600070614    2C                   ENDIF
106700070614    2C                   ENDIF
106800070612     c
106900070612     c     ENDCONS       ENDSR
107000980130     C*****************************************************************
107100980130     C* ELABORAZIONE TESTATA E DETTAGLIO BOLLE ----------------------
107200980130     C*****************************************************************
107300980130     C     MOD01         BEGSR
107400980130     C*
107500980130     C                   SETOFF                                       515253
107600110704     C                   SETOFF                                       5455
107700081205     c                   clear                   werror
107800081210     c                   clear                   wmsg
107900980130     C* CONTROLLO CHE TIPO DI SELEZIONE E' STATA FATTA
108000980130     C* PER CLIENTE
108100130729     C     W1CKSC        COMP      *ZEROS                             5151
108200980130     C* PER LINEA PARTENZA
108300980130     C     V1CLNP        COMP      *ZEROS                             5252
108400980130     C* PER TERMINAL DI ARRIVO
108500980130     C     V1CTFA        COMP      *ZEROS                             5353
108600980130     C* PER LINEA ARRIVO
108700061003     C     WCLNA         COMP      *ZEROS                             54
108800980130     C* PER PADRONCINO
108900061003     C     WCPDR         COMP      *ZEROS                             55
109000020610
109100020610      * se richiesta una linea di partenza imposto solo quella linea di
109200020610      * partenza nella sk delle linee da stampare
109300020610     c                   if        v1clnp <> *zeros
109400020610     c                   clear                   sklnp
109500020610     c                   eval      sklnp(1) = v1clnp
109600020610     c                   else
109700020610     c                   movea     lin           sklnp
109800020610     c                   endif
109900110704     c
110000060403     c* se richiesto aggiornamento su bolle senza colli spuntati stampo
110100060403     c* testata per elenco spedizioni in sospensione da x giorni
110200060403     c   01              if        v1cnos = 'S'
110300060403     c                   open      fnls03p
110400060404     c                   move      v1cggl        w_gg              1
110500060404     c                   eval      ptitolo = ctitolo
110600060404     c                   eval      ptitolo = %xlate('X':w_gg:ptitolo)
110700090127     c                   eval      *in13='0'
110800130729     c                   if        W1cksc>0
110900090127     c                   eval      *in13='1'
111000090127     c                   endif
111100130729     c                   z-add     W1cksc        p1cccm
111200060404     c                   movel     v1dksc        p1dccm
111300090127     c                   z-add     v1clnp        p1clnp
111400090127     c                   movel     v1dlnp        p1dlnp
111500060403     c                   write     testa
111600060403     c                   endif
111700061005     c
111800061005     c                   open      qsysprt
111900060403     c
112000170502     c* se §arzializzazione per numero ORM, leggo per ORM
112100110704     c*  che faccio prima
112200110704    0c                   if        sel4>0
112300110704     C                   MOVEL     SEL4          KN14
112400110704     C     Kar43         SETLL     Fiar404L
112500110704     C     Kar43         reade     Fiar404L
112600110704    1c                   dow       not %eof(fiar404l)
112700110704     c
112800110704     c* controllo la linea di partenza immessa o dell'area
112900110704     c                   eval      Indx=%lookup(ar4lnp:sklnp)
113000110704     c*
113100110704    2c                   if        Indx>0
113200110704
113300110704     c     kblpm         chain(n)  fnblp01l
113400110704
113500110704     c* Verifico anche la data spedizione
113600110704     c                   if        blpdsp=wdtcar
113700110704     c
113800110704     c                   exsr      Parzial
113900110704     c
114000110704    3c                   if        NoBolla=' '
114100110704    4c                   if        waasn<>waas
114200110704     c                   exsr      sr_nuovonumero
114300110704   x4c                   else
114400110704     c
114500110704     c                   exsr      Aggiorna
114600110704     c*
114700110704    4c                   endif
114800110704    4c                   endif
114900110704    3c                   endif
115000110704    2c                   endif
115100110704     c
115200110704     C     Kar43         reade     Fiar404L
115300110704    1c                   enddo
115400110704      *
115500110704   x0c                   else
115600020610
115700020610      * elaboro la sk delle linee da stampare
115800110704   1ac                   do        30            xx
115900110704    1c                   if        sklnp(xx) = *zeros
116000020610     c                   leave
116100110704    1c                   endif
116200020610
116300020610     c                   eval      lnp = sklnp(xx)
116400980130     C*
116500980130     C* LETTURA TESTATE BOLLE
116600020610     C  N52KBLP1         SETLL     FNBLP70L
116700980130     C   52
116800020610     CANN55KBLP2         SETLL     FNBLP70L
116900020610     C   55KBLP3         SETLL     FNBLP70L
117000110704     c
117100980203    1C                   DO        *HIVAL
117200020610     C  N52KBLP1         READE     FNBLP70L                               30
117300980203     C   52
117400020610     CANN55KBLP2         READE     FNBLP70L                               30
117500020610     C   55KBLP3         READE     FNBLP70L                               30
117600980203     C   30              LEAVE
117700110704     c
117800110704     c                   exsr      Parzial
117900110704     c
118000110704     c                   if        NoBolla='N'
118100110704     c                   iter
118200110704     c                   endif
118300110704     c
118400081203     c* Se cambia anche l'anno richiamo pgm apposito che esegue anche la
118500081203     c* rinumerazione della spedizione
118600110704    2c                   if        waasn<>waas
118700081203     c                   exsr      sr_nuovonumero
118800081203     c                   iter
118900110704    2c                   endif
119000110704     c
119100110704     c                   exsr      Aggiorna
119200980202     C*
119300980202    1C                   ENDDO
119400020610
119500110704   1ac                   enddo
119600110704    0c                   endif
119700060403     c*
119800060403     c   01              if        v1cnos = 'S'
119900060404     c   98              write     testa
120000060404     c                   setoff                                       98
120100060404     c                   write     fines
120200060403     c                   close     fnls03p
120300060403     c                   endif
120400061005     c
120500061005     c                   close     qsysprt
120600060713     c
120700060713     c                   if        bollemsg>0
120800111018     c                   eval      zz=zz+1
120900111018     c                   eval      invbo(zz)=bollemsg
121000130729     c                   if        W1cksc>0
121100130729     c                   eval      invcod(zz)=V1cksc + '-' + v1dksc
121200111018     c                   else
121300111018     c                   eval      invcod(zz)=%editc(v1clnp:'X')+'-'+
121400111018     c                             v1dlnp
121500111018     c                   endif
121600111018     c                   eval      invdim(zz)=savdim
121700111018     c
121800111019     c                   if        zz=50
121900111018     c                   exsr      Inv_email
122000111018     c                   clear                   invbo
122100111018     c                   clear                   invcod
122200111018     c                   clear                   invdim
122300111018     c                   endif
122400060713     c                   endif
122500111018     c
122600060713     c* emetto msg di quante bolle manutenzionate
122700130711     c                   exsr      emis_msg
122800130711
122900060926     c*
123000061003     c                   if        yy<6
123100060926     c                   add       1             yy
123200060926     c                   movel     param         elabor(yy)
123300060926     c                   endif
123400980130     C*
123500980202     C                   ENDSR
123600130711     C*----------------------------------------------------------------
123700130711     c     Emis_msg      BEGSR
123800130711     c****               movel     msg(5)        v1cmsg
123900130711     c****               eval      %subst(v1cmsg:27:11)= (%editc(bollema:'2'))
124000130711     c                   seton                                        28
124100130711    1c                   if        werror>0
124200130711    2c                   if        wmsg=*blanks
124300130711     c***                eval      widms1=%trim(widms1)+ 'ERRORE su '
124400130711     c***                eval      widms1=%trim(Widms1)+ (%editc(werror:'Z'))
124500130711     c****               eval      widms1=%trim(Widms1)+ ' bolle.'
124600130711     c                   eval      widms1=%trim(%editc(werror:'Z')) +
124700130711     c                             ' bolle non aggiornate.'
124800130711     c                   eval      widms2=ms2wdw
124900130711     c                   eval      widms3=ms3wdw
125000130711     c                   eval      widms4=ms4wdw
125100130711   x2c                   else
125200130711     c                   eval      widmsall=wmsg
125300130711    2c                   endif
125400130711    1c                   endif
125500130711    1c                   if        wmsg=*blanks
125600130711     c                   movel     msg(5)        v1cmsg
125700130711    2c                   if        bollema>0
125800130711     c                   eval      v1cmsg=%trim(v1cmsg)+' ' +%editc(bollema:'2')
125900130711   x2c                   else
126000130711     c                   eval      v1cmsg=%trim(v1cmsg)+' '
126100130711     c                             + %editw(bollema:'       0 ')
126200130711    2c                   endif
126300130711     c                   eval      v1cmsg=%trim(v1cmsg)+ ' Bolle.'
126400130711    1c                   endif
126500130711     c                   ENDSR
126600110704     C*----------------------------------------------------------------
126700110704     c     Parzial       BEGSR
126800110704     c                   clear                   Nobolla           1
126900110704      *
127000110704      *  51 CONTROLLO IL CODICE CLIENTE
127100110704     C     BLPCCM        IFNE      *ZEROS
127200110704     C                   Z-ADD     BLPCCM        WKSC              7 0
127300110704     C                   ELSE
127400110704     C                   Z-ADD     BLPKSC        WKSC
127500110704     C                   ENDIF
127600130729    2C   51WKSC          IFNE      W1CKSC
127700110704     c                   if        blplnp=190 or blplnp=191
127800110704     c                   seton                                        80
127900110704     c                   except    det
128000110704     c                   setoff                                       80
128100110704    2C                   END
128200110704     c                   eval      NoBolla='N'
128300110704     C                   leavesr
128400110704    2C                   END
128500110704     c* Se richiesta sospensione a cavallo di anno per una lnp estera
128600110704     c* se il cliente mantiene il numero spedizione la scarto
128700110704     c                   if        not *in51 and ntwlnp='E'
128800110704     c                             and waasn<>waas
128900110704     C                   MOVEL(P)  '3C'          COD
129000110704     C                   MOVEL(p)  wksc          KEY
129100110704     C     ktab2         chain     tabel00f                           31
129200110704     C   31              clear                   ds3c
129300110704     C  N31              movel     tbluni        ds3c
129400110704     c     §3cfsp        ifeq      'S'
129500110704     c                   eval      NoBolla='N'
129600110704     C                   leavesr
129700110704     c                   endif
129800110704     c                   endif
129900110704      *
130000110704      *  53 CONTROLLO IL TERMINAL ARRIVO
130100110704    2C   53BLPTFA        IFNE      V1CTFA
130200110704     c                   if        blplnp=190 or blplnp=191
130300110704     c                   seton                                        81
130400110704     c                   except    det
130500110704     c                   setoff                                       81
130600110704    2C                   END
130700110704     c                   eval      NoBolla='N'
130800110704     C                   leavesr
130900110704    2C                   END
131000110704      *
131100110704      *  54 CONTROLLO LA LINEA DI ARRIVO
131200110704    2C   54BLPLNA        IFNE      WCLNA
131300110704     c                   if        blplnp=190 or blplnp=191
131400110704     c                   seton                                        82
131500110704     c                   except    det
131600110704     c                   setoff                                       82
131700110704    2C                   END
131800110704     c                   eval      NoBolla='N'
131900110704     C                   leavesr
132000110704    2C                   END
132100110704      *
132200110704      ** CONTROLLO CHIUSURA FOGLIO VIAGGIO DELLA BOLLA
132300110704    2C     BLPDT1        IFNE      *ZEROS
132400110704     c                   if        blplnp=190 or blplnp=191
132500110704     c                   seton                                        83
132600110704     c                   except    det
132700110704     c                   setoff                                       83
132800110704    2C                   END
132900110704     c                   eval      NoBolla='N'
133000110704     C                   leavesr
133100110704    2C                   ENDIF
133200110704     c
133300110704     c* Se richiesto, controllo se non ha spunte per tutti i colli della
133400170502     c*  sped. Escludo le bolle legate
133500110704    1c                   if        v1cnos='S'
133600170502     c     kblt          chain     fnlbl01l
133700170502   1ac                   if        %found(fnlbl01l)
133800170502     c                   eval      NoBolla='N'
133900170502     c                   leavesr
134000170502  x1ac                   else
134100170502
134200110704     c                   clear                   wsispu            1
134300110704     c     kblt          setll     fnblt01l
134400110704     c     kblt          reade     fnblt01l                               99
134500110704    2c                   dow       not *in99
134600110704     c
134700110704     c* Cerco una qualsiasi spunta
134800110704     c     kbrv          chain     fnbrv07l
134900110704    3c                   if        %found(fnbrv07l)
135000110704     c                   eval      wsispu='S'
135100110704     c                   seton                                        99
135200110704   x3c                   else
135300110704     c     kblt          reade     fnblt01l                               99
135400110704    3c                   endif
135500110704    2c                   enddo
135600110704     c
135700110704     c*Se non trovata nessuna spunta
135800110704    2c                   if        wsispu='S'
135900110704     c                   if        blplnp=190 or blplnp=191
136000110704     c                   seton                                        84
136100110704     c                   except    det
136200110704     c                   setoff                                       84
136300110704    2C                   END
136400110704     c                   eval      NoBolla='N'
136500110704     c                   leavesr
136600110704    2c                   endif
136700160205     c*
136800160205     c* Se non trovate spunte controllo se presente anomalia 20
136900160205     C                   MOVEL     020           comcaa
137000160205     c     kanm01        chain     fnanm01l
137100160205     c                   if        %found(fnanm01l) and anmdch=0
137200160205     C                   eval      NoBolla='N'
137300160205     C                   leavesr
137400160205     c                   endif
137500160205
137600170502   1ac                   endif
137700110704    1c                   endif
137800110704     c*
137900110704      * SCARTO I PREPAGATI ANNULLATI
138000110704     c                   if        blpcbo='M '
138100110704     c                   eval      NoBolla='N'
138200110704     c                   leavesr
138300110704     c                   endif
138400110704      *
138500110704      ** CONTROLLO IL TIPO BOLLA
138600110704     C                   MOVEL     '3A'          COD
138700110704     C                   MOVEL(P)  BLPCBO        KEY
138800110704     C     KTAB2         CHAIN     TABEL00F                           31
138900110704     C   31              CLEAR                   DS3A
139000110704     C  N31              MOVEL     TBLUNI        DS3A
139100110704      *
139200110704      * SCARTO LE BOLLE DI RECUPERO
1393001107041   2C     §3ARBL        IFEQ      'R'
139400110704     c                   if        blplnp=190 or blplnp=191
139500110704     c                   seton                                        85
139600110704     c                   except    det
139700110704     c                   setoff                                       85
139800110704    2C                   END
139900110704     c                   eval      NoBolla='N'
140000110704     C                   leavesr
140100110704    2C                   END
140200110704      * SCARTO LE BOLLE con cod trattamento merce che non prevede merce
140300110704     C                   MOVEL     '1B'          COD
140400110704     C                   MOVEL(P)  BLPctm        KEY
140500110704     C     KTAB2         CHAIN     TABEL00F                           31
140600110704     C   31              CLEAR                   DS1b
140700110704     C  N31              MOVEL     TBLUNI        DS1b
140800110704    2C                   IF        §1bfg8='N'
140900110704     c                   if        blplnp=190 or blplnp=191
141000110704     c                   seton                                        85
141100110704     c                   except    det
141200110704     c                   setoff                                       85
141300110704    2C                   END
141400110704     c                   eval      NoBolla='N'
141500110704     C                   leavesr
141600110704    2C                   END
141700110704      *
141800110704      * PER I PREPAGATI NON È CONSENTITO SPOSTARE LA DATA BOLLA AL MESE
141900110704      *  SUCCESSIVO
142000110704     C     WAMSOS        IFGT      WAMSPE
142100110704      *
142200110704    2C     BLPPDR        IFEQ      *ZEROS
142300110704     c                   if        blplnp=190 or blplnp=191
142400110704     c                   seton                                        86
142500110704     c                   except    det
142600110704     c                   setoff                                       86
142700110704    2C                   END
142800110704     c                   eval      NoBolla='N'
142900110704     C                   leavesr
143000110704    2C                   ELSE
143100110704     C                   MOVEL     '1'           KTRC
143200110704     C     KAR6          CHAIN(N)  FIAR6000                           31
143300110704    2C     *IN31         IFEQ      '0'
143400110704    2C     AR6NFT        ANDGT     *ZEROS
143500110704     c                   if        blplnp=190 or blplnp=191
143600110704     c                   seton                                        87
143700110704     c                   except    det
143800110704     c                   setoff                                       87
143900110704    2C                   END
144000110704     c                   eval      NoBolla='N'
144100110704     C                   leavesr
144200110704    2C                   ENDIF
144300110704    2C                   ENDIF
144400110704      *
144500110704    2C                   ENDIF
144600110704     c
144700110704     c                   ENDSR
144800110704     c*------------------------------------------------------------------------
144900110704     c     Aggiorna      BEGSR
145000110704     c* A questo punto visto che sono stati superati tutti i controlli
145100110704     c* chaino il record bolla in update
145200110704     c     kblt          chain(E)  fnblp01l
145300110704    1c                   if        %error
145400110704     C                   CLEAR                   TRUL82DS
145500110705     c                   if        sel4=0
145600110704     c                   eval      ul82§rrn = fnblpnrri
145700110705     c                   else
145800110705     c                   eval      ul82§rrn = fnblpnrr01
145900110705     c                   endif
146000110704     C                   EVAL      UL82§FIL = 'FNBLP00F'
146100110704     C                   EVAL      UL82§WIN = 'N'
146200110704     C                   EVAL      UL82§F7  = 'S'
146300110704     C                   EVAL      UL82§num = 1
146400110704     C                   EVAL      UL82§att = 30
146500110704     C                   EVAL      UL82§mss = msgjob
146600110704     C* Effettuo la chiamata al *PGM d utilità
146700110704     C                   CALL(e)   'TRUL82R'
146800110704     C                   PARM                    TRUL82DS
146900110704     C     kblt          chain     fnblp01l
147000110704    1c                   endif
147100110704    2c                   if        %found(fnblp01l)
147200111214     c
147300111214     c* Mi salvo il terminal della bolla
147400111214     c                   eval      savtfp=blptfp
147500110704      *
147600110704      ** CONTROLLO SE DATA BORDERO' MINORE DELLA NUOVA DATA DI SPEDIZIONE
147700110704      *  SBORDERIZZO
147800110704    2C     BLPDBR        IFLT      WLPDSP
147900110704      *
148000110704      * SBORDERIZZO
148100110704     C                   CLEAR                   BLPNFV
148200110704     C                   CLEAR                   BLPDBR
148300110704     C                   CLEAR                   BLPFLP
148400110704    2C                   ENDIF
148500110704     C* AGGIORNO LA NUOVA DATA SPEDIZIONE
148600110704     C                   Z-ADD     WAASN         BLPAAS
148700110704     C                   Z-ADD     WLPDSP        BLPMGS
148800110704     c* Aggiorno anche la data ritiro merce
148900110704     c*  solo per i clienti che spostanto in sospensione per mancanza di spu
149000110704     c*  nte
149100110704    2c                   if        v1cnos='S'
149200110704     C                   movel     WLPDSP        BLPdrt
149300110704     c* Se richiesto e se presente consegna richiesta, imposto la nuova data
149400110704    3c                   if        *in03 and v1cdcr>0 and blpdcr>0
149500110704     c                   movel     v1ctcr        blptcr
149600110704     c                   movel     wdcr          blpdcr
149700110704     c                   movel     v1chcr        blphcr
149800110704    3c                   endif
149900110704     c
150000110704   x2c                   else
150100110704     c* Se la data ritiro > della data spedizione nuova e non c'e' ORM
150200110704     c*  imposto SEMPRE la data ritiro= data spedizione
150300110704    3c                   if        blpdrt>wlpdsp
150400110704     c     kar4m         chain     fiar401l
150500110704    4c                   if        not %found(fiar401l)
150600110704     c                   eval      blpdrt=wlpdsp
150700110704    4c                   endif
150800110704    3c                   endif
150900110704     c
151000110704    2c                   endif
151100110704     c
151200110704     C* E RICACOLO I TERMINAL DI ARRIVO E FILIALE ELABORAZIONE ARRIVI
151300110704     C                   CLEAR                   DSLV55
151400110704     C                   MOVEL     ' '           D55TPT
151500110704     C                   MOVEL     BLPLNA        D55LIN
151600110704     C                   MOVEL     BLPLNP        D55LNP
151700110704     C                   MOVEL     WLPDSP        D55DRF
151800110704     C                   CALL      'FNLV55R'
151900110704     C                   PARM                    DSLV55
152000110704    2C     D55ERR        IFEQ      ' '
152100110704     C                   MOVEL     D55TFA        BLPTFA
152200110704     C                   MOVEL     D55TFP        BLPFEA
152300110704    2C                   ENDIF
152400110704     C* BLPTFP: TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
152500110704     C                   CLEAR                   DSLV55
152600110704     C                   MOVEL     'P'           D55TPT
152700110704     C                   MOVEL     BLPLNP        D55LIN
152800110704     C                   MOVEL     WLPDSP        D55DRF
152900110704     C                   CALL      'FNLV55R'
153000110704     C                   PARM                    DSLV55
153100111214     c
153200111214     c
153300110704    2C     D55ERR        IFEQ      ' '
153400110704     C                   MOVEL     D55TFP        BLPTFP
153500111128     C                   MOVEL     D55TFP        BLPfle
153600110704    2C                   ENDIF
153700110704     C*
153800110704     C                   UPDATE    FNBLP001
153900110704     c                   add       1             bollema           9 0
154000110704     c
154100110704     c* Se si tratta di assegnato S.F. tassato in partenza, cancello la
154200110704     c* tassazione
154300110704     c                   clear                   ktrc
154400110704    2c                   if        §3atb2<>*blanks
154500110704     c                   eval      ktrc='2'
154600110704   x2c                   else
154700110704     c                   movel     §3atb1        flgtb1            1
154800110704    3c                   if        flgtb1='A'
154900110704     c                   eval      ktrc='1'
155000110704    3c                   endif
155100110704    2c                   endif
155200110704     c
155300110704   1ac                   if        ktrc<>*blanks
155400110704     C     KAR6          CHAIN(N)  FIAR6000                           31
155500110704    2c                   if        not *in31
155600110704     c                   move      ar6ksc        fineksc           4 0
155700110704    3c                   if        fineksc<>0000 and fineksc<>9999
155800110704     c* deleto la tassazione
155900110704     c     kar6          setll     fiar701l
156000110704     c     kar6          reade     fiar701l
156100110704    4c                   dow       not %eof(fiar701l)
156200110704     c                   delete    fiar7000
156300110704     c
156400110704     c     kar6          reade     fiar701l
156500110704    4c                   enddo
156600110704     c
156700110704     c     kar6          delete    fiar6000
156800110704    3c                   endif
156900110704    2c                   endif
157000110704   1ac                   endif
157100110704     c
157200110704     C*
157300110704     C     KBLT          SETLL     FNBLT01L
157400110704     C     KBLT          READE     FNBLT01L                               30
157500121107    2C     *IN30         DOWEQ     *OFF
157600110704     C* SOLO SE NON C'E' GIA' LA DATA DI PARTENZA
157700110704    3C  N30BLTDFV        IFEQ      0
157800110704     C* E LA DATA BORDERO' DELLA TESTATA NON E' MAGGIORE ALLA NUOVA DATA SPED.
157900110704    2C     BLPDBR        ANDLT     WLPDSP
158000110704     C                   CLEAR                   BLTNFV
158100110704     C                   UPDATE    FNBLT000
158200110704   X3C                   ELSE
158300110704     C                   UNLOCK    FNBLT01L
158400110704    3C                   ENDIF
158500121107     C     KBLT          READE     FNBLT01L                               30
158600110704    2C                   ENDDO
158700111214     c
158800110704     C* AGGIORNO ANCHE FISGN SE PRESENTE
158900110704     C     KFISGN        SETLL     FISGN05L
159000110704     C     KFISGN        READE     FISGN05L                               30
159100110704    2C     *IN30         DOWEQ     *OFF
159200110704     C                   Z-ADD     WLPDSP        SGNMGS
159300110704     C                   Z-ADD     BLPTFA        SGNTNA
159400111214     c
159500111214     c* Cambio il terminal solo se corrisponde a quello della bolla
159600111214     c*  se l'ho inviato ad aaltro terminal non modifico
159700130214     c**                 if        sgntnp=savtfp
159800130214     C**                 Z-ADD     BLPTFP        SGNTNP
159900130214     c**                 endif
160000111214     c
160100110704     C                   CLEAR                   SGNT6A
160200110704     C                   CLEAR                   SGNT6B
160300110704     C                   CLEAR                   SGNT6C
160400110704     C                   CLEAR                   SGNT6D
160500110704     C                   CLEAR                   SGNT6E
160600110704     C                   CLEAR                   SGNT6F
160700110704     c                   eval      sgndatora = %char(%timestamp:*iso0)
160800110704     C                   UPDATE    FISGN000
160900110704     C     KFISGN        READE     FISGN05L                               30
161000110704    2C                   ENDDO
161100110704     C*
161200110704     c* richiamo routine per gestione stampe
161300110704     c                   z-add     blpaas        wlpaas
161400110704     c                   z-add     blpnsp        wlpnsp
161500110704     c                   exsr      sr_stampe
161600110704     c*
161700110704    1c                   endif
161800110704     c
161900110704     c                   ENDSR
162000980130     C*----------------------------------------------------------------
162100980130     C*
162200980130     C*--- OPERAZIONI INIZIALI ---------------------------------------*
162300980130     C     *INZSR        BEGSR
162400980130     C***
162500980130     C* KLIST
162600980130     C***
162700980130     C* ACCESSO   CNACO
162800980130     C     KACO          KLIST
162900980130     C                   KFLD                    CODUT
163000980130     C                   KFLD                    CCC
163100130729     C                   KFLD                    W1CKSC
163200151207     C**   Ktbe          KLIST
163300151207     C**                 KFLD                    kcod
163400151207     C***                KFLD                    klin
163500151207     C***                KFLD                    ksif
163600110704     C*
163700980130     C     KBLT          KLIST
163800980130     C                   KFLD                    BLPAAS
163900980130     C                   KFLD                    BLPLNP
164000980130     C                   KFLD                    BLPNRS
164100980130     C                   KFLD                    BLPNSP
164200110704     C* KEY PER AGGIIORNARE FLBLT
164300110704     C     KBLPM         KLIST
164400110704     C                   KFLD                    ar4AAS
164500110704     C                   KFLD                    ar4LNP
164600110704     C                   KFLD                    ar4NRS
164700110704     C                   KFLD                    ar4NSP
164800110704     C* KEY PER AGGIORNARE fiar4
164900080331     C     Kar4M         KLIST
165000080331     C                   KFLD                    BLPAAS
165100080331     C                   KFLD                    BLPLNP
165200080331     C                   KFLD                    BLPNRS
165300080331     C                   KFLD                    BLPNSP
165400110704     C                   KFLD                    ktrcM
165500051209     C     KBrv          KLIST
165600051209     C                   KFLD                    bltfls
165700051209     C                   KFLD                    bltlna
165800051209     C                   KFLD                    bltNRS
165900051209     C                   KFLD                    bltnsc
166000001120     C* KEY PER AGGANCIARE  FIAR6
166100001120     C     KAR6          KLIST
166200001120     C                   KFLD                    BLPAAS
166300001120     C                   KFLD                    BLPLNP
166400001120     C                   KFLD                    BLPNRS
166500001120     C                   KFLD                    BLPNSP
166600001120     C                   KFLD                    KTRC              1
166700980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
166800980130     C     KBLP1         KLIST
166900020610     c                   kfld                    lnp
167000980130     C                   KFLD                    WAAS
167100980130     C                   KFLD                    WMGS
167200980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 2
167300980130     C     KBLP2         KLIST
167400020610     c                   kfld                    lnp
167500980130     C                   KFLD                    WAAS
167600980130     C                   KFLD                    WMGS
167700980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 3
167800980130     C     KBLP3         KLIST
167900020610     c                   kfld                    lnp
168000980130     C                   KFLD                    WAAS
168100980130     C                   KFLD                    WMGS
168200980130     C                   KFLD                    WCPDR
168300010326     C* ACCESSO   FILE FISGN PER NUMERO SPEDIZIONE
168400010326     C     KFISGN        KLIST
168500010326     C                   KFLD                    BLPAAS
168600010326     C                   KFLD                    BLPLNP
168700010326     C                   KFLD                    BLPNRS
168800010326     C                   KFLD                    BLPNSP
168900980130     C* ACCESSO   TABEL
169000980130     C     KTAB2         KLIST
169100980130     C                   KFLD                    CODUT
169200980130     C                   KFLD                    COD               2
169300980130     C                   KFLD                    KEY               8
169400980202     C* ACCESSO   AZCLN
169500980202     C     KCLN          KLIST
169600980202     C                   KFLD                    SIMFEL
169700980202     C                   KFLD                    W0030             3 0
169800980202     C                   KFLD                    WAA               4 0
169900980202     C                   KFLD                    WMM               2 0
170000021203     C     Kfapd         klist
170100021203     C                   kfld                    kpdtip
170200021203     C                   kfld                    kpdpdr
170300110704      *
170400110704     C     KFORM         KLIST
170500110704     C                   KFLD                    V1CPOE
170600110704     C                   KFLD                    V1CNSR
170700110704     C                   KFLD                    V1CNOR
170800110704     C                   KFLD                    V1CNRV
170900110704      *
171000110704     C     Kar43         KLIST
171100110704     C                   KFLD                    KTRCM
171200110704     C                   KFLD                    KN14             14
171300160205
171400160205     C     Kanm01        KLIST
171500160205     C                   KFLD                    BLPAAS
171600160205     C                   KFLD                    BLPLNP
171700160205     C                   KFLD                    BLPNRS
171800160205     C                   KFLD                    BLPNSP
171900160205     c                   kfld                    comcaa
172000110704     C*
172100980130     C***
172200980130     C* DEFINIZIONE CAMPI
172300980130     C***
172400980130     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
172500980130     C***
172600130729     c     *dtaara       define    §azute        AZUTEds
172700130729     c     *dtaara       define    §datiute      dDATIUTE
172800130729     c                   in(E)     *dtaara
172900130729     c                   if        %ERROR or RSUT = *blanks
173000130729     c                   clear                   Tibs34Ds
173100130729     c                   call      'TIBS34R'
173200130729     c                   parm                    Tibs34Ds
173300130729     c                   in        *dtaara
173400130729     c                   endif
173500130729      *
173600130729     c                   z-add     1             CodUt             1 0
173700980202     C***
173800980202     C* CARICO DATI VARIABILI PARTENZE
173900980202     C***
174000980202     C                   MOVEL     '3I'          COD
174100980202     C                   MOVEL     '1       '    KEY
174200980202     C     KTAB2         CHAIN     TABEL00F                           31
174300980202     C  N31              MOVEL     TBLUNI        DS3I
174400980202     C   31              CLEAR                   DS3I
174500980202     C***
174600980202     C* CARICO CODICI CLIENTI SDI PER I QUALI INSERISCO OBBLIGATORIAM
174700980202     C*  NUMERO AWB CON CONTROLLO CHECK DIGIT
174800980202     C***
174900980202     C                   MOVEL     '3I'          COD
175000980202     C                   MOVEL     'E       '    KEY
175100980202     C     KTAB2         CHAIN     TABEL00F                           31
175200980202     C  N31              MOVEA     TBLUNI        KSS
175300980202     C* 2 CIFRE INIZIALI FISSE PER NUMERO AWB
175400980202     C  N31              MOVE      TBLUNI        §3IFAW            2 0
175500980202     C   31              CLEAR                   KSS
175600980202     C   31              CLEAR                   §3IFAW
175700980130     C*
175800980130     C* DATA BOLLETTAZIONE:  TRA UDATE E DATA GIORNO + §3IGGS
175900980130     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
176000980130     C                   TIME                    W0140            14 0
176100980130     C* UDATE IN GGMMAAAA
176200980130     C                   MOVE      W0140         WDTGIO            8 0
176300060404     C                   MOVEl     W0140         utime
176400980130     C*
176500980130     C* UDATE IN AAAAMMGG
176600980130     C                   Z-ADD     WDTGIO        G02DAT
176700980130     C                   MOVEL     *BLANK        G02ERR
176800980130     C                   CALL      'XSRDA8'
176900980130     C                   PARM                    WLBDAT
177000980130     C                   MOVEL     G02INV        DATEU             8 0
177100980130     C* SE NON FOSSE PERMESSO BOLLETTARE IN SOSPENSIONE
177200980130     C*  SI PUO' BOLLETTARE SOLO FINO A OGGI
177300980130     C                   Z-ADD     G02INV        DATAPA            8 0
177400980130     C                   MOVEL     G02DAT        DATAPG            8
177500980130     C*
177600980130     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
177700130729     C                   Z-ADD     1             X                 2 0
177800980130    1C     X             DOWLE     §3IGGS
177900980130     C*
178000980130     C                   MOVEL     'F'           WFEST             1
178100980130     C*
178200980130    2C     WFEST         DOWNE     ' '
178300980130     C     G02TGI        ADD       1             GIOTGI
178400980130     C                   ADD       1             G02TGI
178500980130     C                   CALL      'XSRGI8'
178600980130     C                   PARM                    WGIDAT
178700980130     C*
178800980130     C                   Z-ADD     GIOINV        DATAPA            8 0
178900980130     C                   MOVEL     GIODAT        DATAPG            8
179000980130     C*
179100980130     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
179200980130     C                   Z-ADD     GIOINV        WDATA
179300980130     C                   CLEAR                   W0030
179400120302     C* CHAIN SUL CALENDARIO BRT
179500980130     C     KCLN          CHAIN     AZCLN01L                           30
179600980130     C   30              CLEAR                   CLNPOM
179700980130     C                   MOVEA     CLNPOM        K31
179800980130     C                   MOVEL     K31(WGG)      WFEST
179900980130    2C                   ENDDO
180000980130     C*
180100980130     C                   ADD       1             X
180200980130    2C                   ENDDO
180300070612     C* reperisco da tabella vpo il numero giorni da sommare a udate per
180400070612     c* determinare data max per data consegna richiesta
180500070612     C                   CLEAR                   tibs02ds
180600070612     C                   MOVEL     'C'           T02MOD
180700070612     C                   MOVEL     KNSIF         T02SIF
180800070612     C                   MOVEL     'VPO'         T02COD
180900070612     C                   MOVEL(p)  'VPO'         T02KE1
181000070612     C                   CALL      'TIBS02R'
181100070612     C                   PARM                    KPJBA
181200070612     C                   PARM                    tibs02ds
181300070612    4C     T02ERR        IFEQ      *BLANKS
181400070612     C                   MOVEL     T02UNI        DVPO
181500070612     C                   ELSE
181600070612     C                   CLEAR                   DVPO
181700070612    4C                   ENDIF
181800130327     c*
181900070612     c                   Move      Dateu         Dataiso
182000070612     c                   Adddur    §vpodcgm1:*d  Dataiso
182100070612     c                   Move      Dataiso       woggidcgm1
182200070612     c*
182300070612     c                   Move      Dateu         Dataiso
182400070612     c                   Adddur    §vpodcgm2:*d  Dataiso
182500070612     c                   Move      Dataiso       woggidcgm2
182600130327      *
182700980130     C***
182800980130     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
182900980130     C***
183000980130     C                   CLEAR                   DSUL06
183100980130     C                   MOVE      '£1'          D06COD
183200980130     C                   MOVEL     SIMFEL        D06KEY
183300980130     C                   MOVEL     'L'           D06TLA
183400980130     C                   MOVEL     DSUL06        KPJBU
183500980130     C*
183600980130     C                   CALL      'TRUL06R'
183700980130     C                   PARM                    KPJBA
183800980130     C                   MOVEL     KPJBU         DSUL06
183900980130     C                   MOVEA     LIN           L1
184000130327     c*
184100090126     c* Verifico se il £1 c'e' almeno un p.o. estero
184200151207     c***                clear                   poestero          1
184300151207     c***                z-add     1             x
184400151207     c***                dow       L1(x)<>0
184500151207     c***  l1(x)         chain     azorg01l
184600151207     c***                if        %found(azorg01l)
184700151207     c***                movel     orgde3        og143
184800151207     c***                if        §ogntw='DPD' or §ogntw='EEX' or
184900151207     c***                          §ogntw='FED'
185000151207     c***                eval      poestero='S'
185100151207     c***                leave
185200151207     c***                endif
185300151207     c***                endif
185400151207     c***                add       1             x
185500151207     c***                enddo
185600980130     C***
185700020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
185800980130     C***
185900130729     C                   Z-ADD     dutPOU        CDU               3 0
186000130729     C     DUTlpo        IFEQ      '2'
186100130729     C     DUTlpo        OREQ      *BLANKS
186200020429     C                   SETON                                        12
186300020429     C                   ENDIF
186400130327     c*
186500111018     c                   clear                   elabor
186600130327     c*
186700111018     c                   Clear                   tibs02ds
186800111018     c                   Clear                   dmradan
186900111018     c                   Movel     'C'           T02Mod
187000111018     c                   Movel     KNSIF         T02Sif
187100111018     c                   Movel     'MRA'         T02Cod
187200111018     c                   Movel     'FNLS03R'     T02Ke1
187300111018     c                   Call      'TIBS02R'
187400111018     c                   Parm                    Kpjba
187500111018     c                   Parm                    tibs02ds
187600111018    2c                   If        T02Err = *Blanks
187700111018     c                   Movel     T02Uni        Dmradan
187800111018    2c                   endif
187900001120     C*
188000980130     C                   ENDSR
188100980130     C*****************************************************************
188200980130     C* PULIZIA   FORMATO -------------------------------------------
188300980130     C*****************************************************************
188400980130     C     PUL01         BEGSR
188500980130     C*
188600070614     c                   setoff                                       0103
188700130729     C                   MOVE      *ZEROS        W1CKSC
188800130729     C                   clear                   V1CKSC
188900980130     C                   MOVE      *BLANKS       V1DKSC
189000980130     C                   MOVE      *BLANKS       V1CLNP
189100980130     C                   MOVE      *BLANKS       V1DLNP
189200980130     C                   MOVE      *ZEROS        V1CTFA
189300980130     C                   MOVE      *BLANKS       V1DTFA
189400980130     C                   MOVE      *BLANKS       V1CLNA
189500980130     C                   MOVE      *BLANKS       V1DLNA
189600980130     C                   MOVE      *BLANKS       V1CPDR
189700980130     C                   MOVE      *BLANKS       V1DPDR
189800980130     C                   Z-ADD     *ZEROS        V10DSP
189900980130     C                   Z-ADD     *ZEROS        V11DSP
190000070612     C                   CLEAR                   v1cdcr
190100070612     C                   CLEAR                   v1ctcr
190200070612     C                   CLEAR                   v1chcr
190300070612     C                   CLEAR                   wdcr
190400051209     C                   move      *blanks       V1cnos
190500160405     C                   z-add     §3igga        V1cggl
190600060713     c                   clear                   bollemsg
190700060713     c                   clear                   bollema           9 0
190800110704     c                   clear                   v1cpoe
190900110704     c                   clear                   v1cnsr
191000110704     c                   clear                   v1cnor
191100110704     c                   clear                   v1cnrv
191200111017     c                   clear                   savdim            8 0
191300151207
191400151207     c* Abilitazione sospensione per colli non spuntati
191500151207
191600151207     c     ' '           setll     fibsp03l
191700151207     c     ' '           reade     fibsp03l
191800151207     c                   dow       not %eof(fibsp03l) and *in01=*off
191900151207     c* record per lnp
192000151207     c                   if        bspfil>0
192100151207     c                   if        %lookup(bspfil:l1)>0
192200151207     c                   eval      SosdaL1='S'
192300151207     c                   seton                                        01
192400151207     c                   endif
192500151207     c                   else
192600151207     c* record per cliente
192700151207     c                   z-add     1             Ix
192800151207     c                   dow       Ix<=%elem(sbsp) and sbsp(Ix)>0
192900151207     c                             and *in01 = *off
193000151207     c                   if        %lookup(sbsp(Ix):L1)>0
193100151207     c                   eval      SosdaL1='S'
193200151207     c                   seton                                        01
193300151207     c                   endif
193400151207     c                   add       1             Ix
193500151207     c                   enddo
193600151207     c                   endif
193700151207     c     ' '           reade     fibsp03l
193800151207     c                   enddo
193900090126     c* Abilito l'opzione se c'e' un p.o. estero in £1
194000151207     c***                if        poestero='S'
194100151207     c***                seton                                        01
194200151207     c***                eval      SosdaL1='S'
194300151207     c****               else
194400130327     c*
194500070903     c* Abilito l'opzione solo se almeno un cliente è abilitato
194600151207     c***                eval      SosdaL1=' '
194700151207     c***                eval      ksif=knsif
194800151207     c***  ktbe          setll     tntbe02l
194900151207     c***                if        not %equal(tntbe02l)
195000151207     c***                clear                   ksif
195100151207     c***  ktbe          setll     tntbe02l
195200151207     c***                endif
195300130327     c*
195400151207    1c***                if        %equal(tntbe02l)
195500151207     c***  ktbe          reade     tntbe02l
195600151207   1ac***                dow       not %eof(tntbe02l) and not *in01
195700151207    2c***                if        tbeatb=' '
195800151207     c***                movel     tbeuni        dbsp
195900151207     c***                movel     tbeke1        lnpabi            3 0
196000130327     c*
196100151207     c***                if        lnpabi>0
196200151207     c***  lnpabi        lookup    l1                                     30
196300151207    5c***                if        *in30
196400151207     c***                eval      SosdaL1='S'
196500151207     c***                seton                                        01
196600151207    5c***                endif
196700151207     c***                endif
196800151207    2c***                endif
196900130327     c*
197000151207     c***  ktbe          reade     tntbe02l
197100151207   1ac***                enddo
197200151207    1c***                endif
197300130327     c*
197400151207    1c***                endif
197500130327     c*
197600080624     c*********          if        not *in01 and (cdu=118 or cdu=107)
197700080624     c*********          eval      *in01=*on
197800080624     c*********          endif
197900060403     c*
198000060403     c* se 01 acceso effettuo ovrprtf per mettere le stampe in save
198100060403     c                   if        *in01
198200060403     c                   clear                   qcmd
198300060403     c                   eval      qcmd = $cmd2(1)
198400060403     c                   call      'QCMDEXC'                            91
198500060403     c                   parm                    Qcmd
198600060403     c                   parm                    Qlen
198700060403     c                   endif
198800130327     c*
198900061005     c                   clear                   qcmd
199000061005     c                   eval      qcmd = $cmd3(1)
199100061005     c                   call      'QCMDEXC'                            91
199200061005     c                   parm                    Qcmd
199300061005     c                   parm                    Qlen
199400980130     C*
199500980130     C                   ENDSR
199600030619
199700130327     *** *---------------------------------------------------------------*
199800130327     *** * Richiedo i dati per la stampa   ed   Eseguo le override       *
199900130327     *** *---------------------------------------------------------------*
200000130327     ***c     SR_OvrPrtF    BEGSR
200100130327     *** *
200200130327     ***c                   call      'TRUL90R'
200300130327     ***c                   parm                    KPJBA
200400130327     ***c                   parm                    TRUL90DS
200500130327     *** * - F3 > Fine
200600130327     ***c     D90F3         cabeq     *on           End_Ovr
200700130327     *** *
200800130327     *** * - FNLSB5PA4 > bolle laser in formato A4
200900130327     ***c                   eval      wFILE = 'FNLSB5PA4 '
201000130327     ***c                   eval      wOUTQ = D90prb4
201100130327     ***c                   eval      wFORM = D90mdb4
201200130327     ***c                   exsr      SR_x_Cmd
201300130327     ***c     *in91         cabeq     *on           End_Ovr
201400130327     *** *
201500130327     *** * - FNLSB5PA5 > bolle laser in formato A5
201600130327     ***c                   eval      wFILE = 'FNLSB5PA5 '
201700130327     ***c                   eval      wOUTQ = D90prb5
201800130327     ***c                   eval      wFORM = D90mdb5
201900130327     ***c                   exsr      SR_x_Cmd
202000130327     ***c     *in91         cabeq     *on           End_Ovr
202100130327     *** *
202200130327     ***c     End_Ovr       ENDSR
202300130327     ***
202400130327     *** *---------------------------------------------------------------*
202500130327     ***
202600130327     ***c     SR_x_Cmd      BEGSR
202700130327     *** *
202800130327     *** * impostazione comando da eseguire
202900130327     ***c                   eval      Qcmd  = %trim($Cmd(1)) + ' '
203000130327     ***c                                   + %trim($Cmd(2))
203100130327     *** * - nome del file
203200130327     ***c                   eval      Qcmd = %replace(wFILE:Qcmd:
203300130327     ***c                                    %scan('&FILE':Qcmd))
203400130327     *** * - nome della coda di stampa
203500130327     ***c                   eval      Qcmd = %replace(wOUTQ:Qcmd:
203600130327     ***c                                    %scan('&OUTQ':Qcmd))
203700130327     *** * - tipo di modulo
203800130327     ***c                   eval      Qcmd = %replace(wFORM:Qcmd:
203900130327     ***c                                    %scan('&FORMTYPE':Qcmd))
204000130327     *** *
204100130327     ***c                   call      'QCMDEXC'                            91
204200130327     ***c                   parm                    Qcmd
204300130327     ***c                   parm                    Qlen
204400130327     *** *
204500130327     ***c                   ENDSR
204600130327
204700081203      *---------------------------------------------------------------*
204800081203
204900081203     c     SR_stampe     BEGSR
205000130327      *
205100130327     ***C* PER CMD8 STAMPO LA BOLLA
205200130327     ***C     *INKH         IFEQ      *ON
205300130327     ***C     BLPFST        ANDNE     'N'
205400130327     ***c                   clear                   FNLSB5DS
205500130327     ***C                   MOVEL     'V'           DB0RIS
205600130327     ***C                   MOVEL     'P'           DB0TBO
205700130327     ***C                   Z-ADD     wLPAAS        DB0AAS
205800130327     ***C                   Z-ADD     BLPLNP        DB0LNP
205900130327     ***C                   Z-ADD     BLPNRS        DB0NRS
206000130327     ***C                   Z-ADD     wLPNSP        DB0NSP
206100130327     ***c                   call      D90psl
206200130327     ***c                   parm                    FNLSB5DS
206300130327     ***c                   parm                    FNLSB6ds1
206400130327     ***C                   ENDIF
206500081203     c*
206600081203     c* GESTIONE STAMPA SPEDIZIONE
206700081203     c                   if        v1cnos='S'
206800081203     c                   if        v1cggl > 0
206900081203     c* calcolo giorni lavorativi fra data spedizione e data immissione
207000081203     c                   clear                   wdat8
207100081203     c                   z-add     blpdim        datada
207200081203     c                   z-add     wlpdsp        dataa
207300081203     c                   call      'XSRLAV8'
207400081203     c                   parm                    wdat8
207500081203     c                   if        (ggl-1) > v1cggl
207600081203     c                   if        blpfns = 'S'
207700081203     c                   seton                                        02
207800081203     c                   else
207900081203     c                   setoff                                       02
208000081203     c                   endif
208100081203     c* inverto data ritiro per stampa
208200081203     c     *iso          move      blpdim        data_eur
208300081203     c                   move      data_eur      p1cdim
208400081203     c   98              write     testa
208500081203     c                   setoff                                       98
208600081203     c                   write     rigsped
208700081203     c                   endif
208800081203     c                   endif
208900081203     C**
209000081203     c** Se passati xx giorni di sospensione, invio msg al CED
209100081203     c                   if        (ggl-1) > §3iggm
209200081203     c                   add       1             bollemsg          9 0
209300111017     c* memorizzo la data più bassa di immissione
209400111017     c                   if        savdim=0 or blpdim<savdim
209500111017     c                   eval      savdim=blpdim
209600111017     c                   endif
209700111017     c
209800081203     c                   endif
209900081203     c                   endif
210000081203     c                   endsr
210100081203      *---------------------------------------------------------------*
210200081203
210300081203     c     SR_nuovonumeroBEGSR
210400081203     c                   clear                   fnls65ds
210500081203     c                   z-add     waas          I65AAS
210600081203     c                   z-add     blplnp        I65LNP
210700081203     c                   z-add     blpnrs        I65NRS
210800081203     c                   z-add     blpnsp        I65NSP
210900081203     c                   z-add     waasn         I65AASN
211000081203     c                   z-add     wlpdsp        I65MGSN
211100081203     c                   if        v1cnos='S'
211200081203     c                   movel     'S'           I65AGDRT
211300081203     c                   if        *in03 and v1cdcr>0 and blpdcr>0
211400081203     c                   movel     'S'           I65AGDCR
211500081203     c                   move      v1ctcr        I65TCRN
211600081203     c                   move      wdcr          I65DCRN
211700081203     c                   move      v1chcr        I65HCRN
211800081203     c                   endif
211900081203     c                   endif
212000081203     c                   move      '1'           I65STRCMT
212100081203     c                   move      '1'           I65FCMT
212200081203     c                   move      '1'           I65CMTRLBK
212300081203     c                   movel     fnls65ds      kpjbu
212400081210     c                   call      'FNLS65R'
212500081203     c                   parm                    kpjba
212600081203     c                   movel     kpjbu         fnls65ds
212700081203     c                   if        o65err=*blanks
212800081203     c                   add       1             bollema           9 0
212900081205     c                   z-add     waasn         wlpaas
213000081205     c                   z-add     o65nsp        wlpnsp
213100081203     c                   exsr      sr_stampe
213200081204     c                   else
213300081205     c                   add       1             werror            6 0
213400081210     c                   if        o65err='1'
213500081210     c                   movel     o65msg        wmsg
213600081210     c                   endif
213700081203     c                   endif
213800081203     c                   endsr
213900111018     c*-------------------------------------------------------------------
214000111018     c     INV_email     BEGSR
214100111018     c
214200111018     c                   if        not %open(prtemail)
214300111018     C                   EXSR      SR_OPENPRTF
214400111018     C                   endif
214500111018     c
214600111018     c                   exsr      Routend
214700111018     c
214800111018     c                   ENDSR
214900111018
215000111018      /free
215100111018       //--------------------------------------------------------------
215200111018       //?Override al file di stampa per impostarvi i dati per
215300111018       //?  l'invio via e-mail   +   stampa inizio e-mail
215400111018       //--------------------------------------------------------------
215500111018       BEGSR sr_OpenPrtF;
215600111018
215700111018         // Override al file di stampa per impostarvi i dati per
215800111018         // l'invio via e-mail
215900111018           exsr sr_Override;
216000111018
216100111018           open PrtEMAIL;
216200111018
216300111018         // Stampa una testata se NON è richiesta la e-mail
216400111018           IF  §MRAdreg =  *blank;
216500111018             O_testo = JobUser + ' - ' + SDSpgm
216600111018                     + ' - ' + %editc( *date : 'Y' )
216700111018                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
216800111018             except PrtDet;
216900111018             clear O_testo;
217000111018             except PrtDet;
217100111018             except PrtDet;
217200111018           ENDIF;
217300111018
217400111018         // Stampa testo iniziale
217500111018           O_testo = 'E'' stata manutenzionata la DATA SPEDIZIONE +
217600111019                      di bolle immesse da almeno xx giorni.' ;
217700111019           %subst(O_testo:71:2) = (%editc(§3iggm: '4'))     ;
217800111018           except PrtDet;
217900111019           O_testo = 'Verificare col mittente o procedere alla lor+
218000111019                      o chiusura come "Merce MAI Affidata".' ;
218100111019           except PrtDet;
218200111018
218300111018         // Stampa una riga vuota
218400111018           clear O_testo;
218500111018           except PrtDet;
218600111018
218700111018         // Stampa intestazione
218800111019           O_testo = ' Bolle spostate in sospensione di          Data imm+
218900111019                     issione    Bolle da vericare elencate';
219000111018           except PrtDet;
219100111019           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
219200111019                     chia       in apposita stampa.Utente:';
219300111019           %subst(o_testo:90:10)= knmus     ;
219400111019           except PrtDet;
219500111018
219600111018         // Stampa una riga vuota
219700111018           clear O_testo;
219800111018           except PrtDet;
219900111019           zz = 1 ;
220000111019
220100111019          dow     invbo(zz)>0   ;
220200111019          dataiso=%date(invdim(zz))  ;
220300111019          data_eur=dataiso  ;
220400111019
220500111019           clear O_testo;
220600111019           %subst(O_testo:6:20) = invcod(zz)  ;
220700111019           %subst(O_testo:46:10) =%char(data_eur)  ;
220800111019           %subst(O_testo:64:7 ) =%editc(invbo(zz):'2')  ;
220900111019           except PrtDet;
221000111019
221100111019           zz=zz+1  ;
221200111019           enddo    ;
221300111018
221400111018       ENDSR;
221500111018
221600111018       //--------------------------------------------------------------
221700111018       //?Override al file di stampa per impostarvi i dati per
221800111018       //?  l'invio via e-mail   +   stampa inizio e-mail
221900111018       //--------------------------------------------------------------
222000111018       BEGSR sr_Override;
222100111018
222200111018         reset TRTCM1ds;
222300111018
222400111018         IF  §MRAdreg <> *blank;
222500111018           §CM1mitt = %trim(§MRAdmitt);
222600111018           §CM1dst  = %trim(§MRAddest);
222700111019
222800111019         // Valorizzo i campi della e-m@ail
222900130729          chain  DUTpou   azorg01l ;
223000111019
223100111019          if   %found(azorg01l) ;
223200140205           og140=orgde0  ;
223300140205           if §ogapo>*zeros  ;
223400140205           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
223500140205           else  ;
223600140205            §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
223700111019          endif  ;
223800140205          endif  ;
223900111019
224000111018           §CM1tips = §MRAdreg;
224100111018           §CM1po   = %editc(simfel:'X');
224200111019           §CM1var  = '*OBJM*' + 'Manutenzione DATA SPEDIZIONE bolle'   ;
224300111018           §CM1idp  = §MRAdidpro;
224400111018           Qcmd = C_CmdOvrPrtF
224500111018                + ' outq(' + %trim(§MRAdoutqi) + ')'
224600111018                + ' usrdfndta(''' + TRTCM1ds + ''')';
224700111018         ELSE;
224800111018           Qcmd = C_CmdOvrPrtF;
224900111018         ENDIF;
225000111018
225100111018         callp(e) qCmdExc(Qcmd : %size(Qcmd));
225200111018
225300111018       ENDSR;
225400111018
225500130718       //--------------------------------------------------------------
225600151204       //?Cerca cliente in FIBSP
225700130718       //--------------------------------------------------------------
225800130718       BEGSR cer_bsp;
225900151204       //ksif=knsif;
226000151204       //setll (kcod: klin: ksif) tntbe02l;
226100151204       //if not %equal(tntbe02l);
226200151204       //   clear ksif;
226300151204       //   setll (kcod: klin: ksif) tntbe02l;
226400151204       //endif;
226500151204       //if %equal(tntbe02l);
226600151204       //   reade (kcod: klin: ksif) tntbe02l;
226700151204       //   dow not %eof(tntbe02l) and wbsp=' ';
226800151204       //      if tbeatb=' ' and tbeke2=v1cksc;
226900151204       //         wbsp='S';
227000151204       //         dbsp=tbeuni;
227100151204       //      endif;
227200151204       //      reade (kcod: klin: ksif) tntbe02l;
227300151204       //   enddo;
227400151204       //endif;
227500151204
227600151207       chain w1Cksc fibsp02l;
227700151209       if %found(fibsp02l);
227800160111         if bspfpa=*blanks or bspaut='N';
227900151204          wbsp='S';
228000151209         else;
228100160111          wbspchf='S';
228200151209         endif  ;
228300151204       endif;
228400130718       endsr;
228500111018       //--------------------------------------------------------------
228600111018       //?Operazioni finali.
228700111018       //--------------------------------------------------------------
228800111018       BEGSR RoutEnd;
228900111018
229000111018         if %open(PrtEMAIL);
229100111018
229200111018         //?Chiusura dello spool?
229300111018           clear O_testo;
229400111018           except PrtDet;
229500111018           O_testo = '***   Fine Lista   ***';
229600111018           except PrtDet;
229700111018
229800111018           close PrtEMAIL;
229900111018
230000111018         //?Eliminazione overflow?
230100111018           Qcmd = C_CmdDltOvr;
230200111018           callp(e) qCmdExc(Qcmd : %size(Qcmd));
230300111018
230400111018         endif;
230500111018
230600111018       ENDSR;
230700111018
230800111018      /end-free
230900111018     c*-------------------------------------------------------------------
231000061117     Oqsysprt   E    80n80   DET         1  1
231100061005     o                                              'Bolla:'
231200061005     O                       BLPaas        Z   +  1
231300061005     O                       BLPlnp            +  1
231400061005     O                       BLPNRS        Z   +  1
231500061005     O                       BLPNSP        Z   +  1
231600061005     O                       BLPmgs            +  1 '  /  '
231700061005     o               80                          48 'Cliente    <>'
231800061005     O               80      wksc              +  1
231900061005     o               81                          48 'Terminal   <>'
232000061005     o               82                          48 'Lin Arrivo <>'
232100061005     o               83                          48 'BLPDT1  <>  0'
232200061005     o               84                          48 'Esiste spunta'
232300061005     o               85                          49 'Bolla recupero'
232400061005     o               86                          48 'Prepagato    '
232500061005     o               87                          55 'Prepagato con numfat'
232600030619
232700111018     o* ---------------------------------------------------------------
232800111018     o* ?Spool di stampa (per e-mail).
232900111018     o* ---------------------------------------------------------------
233000111018     oPrtEMAIL  e            PRTdet      1
233100111018     o                       O_testo
233200111018
233300030619**  $Cmd
233400030707OVRPRTF FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
233500030619           USRDTA('Manut.Boll') SHARE(*YES)
233600030619           COPIES(2)
233700060403**  $Cmd2
233800060403OVRPRTF FILE(FNLS03P) SAVE(*YES)
233900061005**  $Cmd3
234000061005OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES) OUTQ(LJ4200CED) USRDTA('EDPES')
234100061012**  DLTOVR
234200061012DLTOVR  FILE(QSYSPRT)
234300060713**         MSG
234400111017MANUTENZIONE DATA SPED.BOLLE oltre xx giorni
234500111018Utente:  xxxxxxxxxx
234600060713Codice Cliente:  xxxxxxx   x
234700060713Bolle manutenzionate: xxx.xxx.xxx
234800081210Sono state manutenzionate
234900070612Se immesso tipo immettere anche la data  Consegna Richiesta
235000070612Data cons.richiesta formalmente errata o inferiore/uguale alla data odierna    4
235100070612Data consegna richiesta non ammessa: oltre                                     5
235200070612Data cons.richiesta  inferiore  alla nuova data spedizione
235300070614Ora consegna richiesta errata
235400090401Linea di partenza:  xxx
235500110704Immettere filiale di emissione valida                                         12
235600110704Immettere numero ORM esistente in archivio                                    13
235700111017Bolle immesse dal                                                             14
235800130711Se richiesta la sospensione automatica, non effettuare alcuna parzializzaz.   15
235900130729Ammessi solo caratteri numerici                                               16
