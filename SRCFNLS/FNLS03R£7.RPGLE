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
001200110704     Ffnorm01l  IF   E           K DISK
001300980130     FAZCAE01L  IF   E           K DISK
001400980202     FAZCLN01L  IF   E           K DISK
001500980130     FTABEL00F  IF   E           K DISK
001600021203     FFIAPD01L  IF   E           K DISK
001700910205     FCNACO00F  IF   E           K DISK
001800081203     FFNBLP70L  IF   E           K DISK    infds(fnblpinf)
001900081203     FFNBLP01L  UF   E           K DISK    rename(fnblp000:fnblp001)
002000110705     f                                     infds(blpnrr01)
002100980130     FFNBLT01L  UF   E           K DISK
002200010326     FFISGN05L  UF   E           K DISK
002300050318     FfIAR601L  UF   E           K DISK
002400080331     FfIAR401L  UF   E           K DISK
002500110704     FFiar404L  IF   E           K DISK    RENAME(Fiar4000:Fiar404)
002600151204     Ffibsp02l  IF   E           K DISK
002700151207     Ffibsp03l  IF   E           K DISK    rename(fibsp000:fibsp03)
002800050318     FfIAR701L  UF   E           K DISK
002900160205     Ffnanm01l  iF   E           K DISK
003000111018     FQSYSPRT   O    F  132        PRINTER OFLIND(*InOA)  usropn
003100111018
003200111018     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003300020610
003400941227     D L1              S              3  0 DIM(30)
003500020610     d sklnp           s              3  0 dim(30)
003600020610
003700030619     d $Cmd            s             80    dim(3) ctdata perrcd(1)
003800060403     d $Cmd2           s             80    dim(1) ctdata perrcd(1)
003900061005     d $Cmd3           s             80    dim(1) ctdata perrcd(1)
004000061012     d DLTOVR          s             80    dim(1) ctdata perrcd(1)
004100130729     D MSG             S             78    DIM(16) CTDATA PERRCD(1)
004200980202     D K31             S              1    DIM(31)
004300980202     D*
004400980202     D KSS             S              7    DIM(12)
004500070612     D elabor          S             54    DIM(6)
004600111019     D invbo           S              5  0 DIM(51)
004700111019     D invcod          S             25    DIM(51)
004800111019     D invdim          S              8  0 DIM(51)
004900020610
005000020610     d lnp             s              3  0
005100020610     d xx              s              2  0
005200111018     d zz              s              2  0 inz
005300030619      *
005400030619     d Qlen            s             15  5 inz(150)
005500030619      *
005600130718     d Wbsp            s              1    inz
005700160111     d Wbspchf         s              1    inz
005800030619     d Wfile           s             10    inz
005900030619     d Woutq           s             10    inz
006000030619     d Wform           s             10    inz
006100060405     d data_eur        s               D   datfmt(*eur)
006200070612     d Dataiso         s               d   datfmt(*iso)
006300070612     d woggidcgm1      s              8  0
006400070612     d woggidcgm2      s              8  0
006500070612     d s_woggidcg      s                   like(woggidcgm1)
006600070612     d s_vpodcg        s                   like(§vpodcgm1)
006700021203      *
006800111018
006900111018      // - Status
007000111018     d Psds           sds
007100111018     d   SDSpgm          *proc
007200111018     d   JobUser             254    263
007300111018
007400111018      // - Campi per QCMDEXC
007500111018     d Qcmd            s            512    inz
007600111018     d
007700111018      // - Esecuzione comando di sistema
007800111018     d qCmdExc         pr                  extpgm('QCMDEXC')
007900111018     d  Qcmd                        512    const  options(*varsize)
008000111018     d  Qlen                         15  5 const
008100111018
008200021203      * Ds esterna per ricerca codice padroncino
008300021203     D FNLV24DS      E DS
008400051209     D* DS PER TIBS02R - GESTIONE TNTBE00F
008500051209     D TIBS02DS      E DS
008600111018      // - Tabella "MRA" = Bart-Mailing - Danni
008700111018     d dMRAdan       e ds                  inz
008800130726      * Ds esterna per ricerca cliente da tab. "BSP"
008900151201     d fnlsbsp1ds    e ds                  inz
009000111018
009100070612     d
009200070612     D Dvpo          E DS
009300070903     D og143         E DS
009400081211     d
009500081211     dwidmsall         ds
009600081211     Dwidms1                         32
009700081211     Dwidms2                         32
009800081211     Dwidms3                         32
009900081211     Dwidms4                         32
010000110704     D                 DS
010100110704     D  V1CPOE                 1      3  0
010200110704     D  V1CNSR                 4      5  0
010300110704     D  V1CNOR                 6     12  0
010400110704     D  V1CNRV                13     14  0
010500110704     D  SEL4                   1     14  0
010600081211     D***
010700941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010800980202     D WGIDAT          DS                  INZ
010900980202     D  GIODAT                 1      8  0
011000980202     D  GIOINV                 9     16  0
011100980202     D  GIOTGI                17     21  0
011200941227     D***
011300941227     D WLBDAT          DS                  INZ
011400941227     D  G02DAT                 1      8  0
011500941227     D  G02INV                 9     16  0
011600941227     D  G02ERR                17     17
011700941227     D  G02TGI                18     22  0
011800980202     D                 DS
011900980202     D  WAA                    1      4  0
012000980202     D  WMM                    5      6  0
012100980202     D  WGG                    7      8  0
012200980202     D  WDATA                  1      8  0
012300060403
012400060403     d WDAT8           DS
012500060403     d  datada                 1      8  0
012600060403     d  dataa                  9     16  0
012700060403     d  ggl                   17     21  0
012800060713     D                 DS
012900110704     D  blpaas                 1      4  0
013000110704     D  blpmgs                 5      8  0
013100110704     D  blpdsp                 1      8  0
013200060926     d
013300060926     D                 DS
013400130729     D  W1cksc                 1      7  0
013500060926     D  v1clnp                 8     10  0
013600061003     D  wclna                 11     13  0
013700060926     D  v1ctfa                14     16  0
013800060926     D  v1cnos                17     17
013900060926     D  v1cggl                18     18  0
014000060926     D  v10dsp                19     26  0
014100060926     D  v11dsp                27     34  0
014200061012     D  Wcpdr                 35     41  0
014300070612     D  V1CTCR                42     42
014400070612     D  wdcr                  43     50  0
014500070612     D  V1CHCR                51     54  0
014600070612     D PARAM                   1     54
014700130711
014800130711     D fnlsu3ds        DS
014900130711     D  ilsu3flg               1      1
015000130711     D  ilsu3aas               2      5
015100130711     D  ilsu3lnp               6      8
015200130711     D  ilsu3nrs               9     10
015300130711     D  ilsu3nsp              11     17
015400130711     D  ilsu3DSP_B            18     25
015500130711     D  ilsu3DSP_S            26     33
015600130711     D  ilsu3nos              34     34
015700130711     d
015800130711     d  ilsu3tcr              35     35
015900130711     d  ilsu3dcr              36     43
016000130711     d  ilsu3hcr              44     47
016100130711     D  olsu3err             100    101
016200130711     D  olsu3boagg           102    110
016300130830     D  olsu3bonagg          111    116
016400150703     D  olsu3msg             117    193
016500150703     D  olsu3sosp            194    194
016501170419     D  Ilsu3NOAGG           201    201
016600151207     d                 ds
016700151207     d bsplin
016800151207     d sbsp                           3  0 dim(30) overlay(bsplin)
016900130711     d
017000060403
017100941116     D KPJBA         E DS
017200130729
017300130729       // -?Parametri x Controllo profilo utenti?
017400130729     d TIBS34ds      e ds
017500130729       // -?Ds di riferimento al file esterno AZUTE00F?
017600130729     d AZUTEds       e ds                  extname(AZUTE00F)
017700130729       // -?Ds per dati organigramma?
017800130729     d dDATIUTE      e ds
017900130729
018000130327     *** * Parametri per stampa BOLLE ed ETICHETTE
018100130327     ***d TRUL90DS      e ds                  inz
018200130327     ***d   D90rse      e                     inz('N')
018300130327     ***d   D90rsb      e                     inz('S')
018400130327     *** * DS per pgm di STAMPA BOLLE
018500130327     ***d FNLSB5DS      e ds                  inz
018600130327     ***d FNLSB6ds1     e ds                  inz
018700130327     ***d  DB6pdf       e                     inz('F')
018800081203      * Ds per pgm che mette in sospensione rinumerando la spedizione
018900081203     d FNLS65DS      e ds                  inz
019000030619      *
019100980202     D* DS PER TRUL06R - CARICAMENTO £X
019200941227     D DSUL06        E DS                  EXTNAME(TRUL06DS)
019300941227     D  LIN                    1     90  0
019400941227     D                                     DIM(30)
019500980129     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
019600980129     D DSLV55        E DS                  EXTNAME(FNLV55DS)
019700081203     D fnblpinf        ds
019800110705     D  fnblpnrri            397    400i 0
019900110705     D blpnrr01        ds
020000110705     D  fnblpnrr01           397    400i 0
020100081203      * DS Esterna x gestione allocazione ed invio messaggi
020200081203     D TRUL82DS      E DS
020300980130     D DS3A          E DS
020400090427     D DS1B          E DS
020500081203     D Ds3c          E DS
020600980202     D DS3I          E DS
020700130717     D*DCLI          E DS
020800140205     D og140         E DS
020900021203
021000021203     D kpdpdr          S                   LIKE(APDpdr)
021100021203     D kpdtip          S                   LIKE(APDtip)
021200151207     D*kcod            S                   LIKE(tbecod) inz('BSP')
021300151207     D*klin            S                   LIKE(tbelin) inz(' ')
021400151207     D*ksif            S                   LIKE(tbesif)
021500110704     D ktrc            S                   LIKE(ar4trc)
021600110704     D ktrcM           S                   LIKE(ar4trc) inz('M')
021700081205     D wlpnsp          S                   LIKE(blpnsp)
021800081205     D wlpaas          S                   LIKE(blpaas)
021900081210     D wmsg            S                   LIKE(v1cmsg)
022000090126     D ntwlnp          S              1    inz
022100060926     D YY              S              3  0 inz
022200111214     D savtfp          S              3  0 inz
022300080620     D sosdaL1         S              1    inz
022400110704     D Indx            S              3  0 inz
022500111018     d O_testo         s            132    inz
022600151207     d IX              s              4  0
022700160205     d comcaa          s                   like(anmcaa)
022800060713     D* DEFINIZIONE COSTANTI
022900060404     D Ctitolo         C                   CONST('ELENCO SPEDIZIONI IN SOSPENSI-
023000060405     D                                     ONE DA OLTRE X GIORNI')
023100081203     D msgjob          C                   CONST('Si sta bloccando la manutenzi-
023200081203     D                                     one data spedizione:USCIRE un moment-
023300081203     D                                     o da questo lavoro')
023400130111     D*ms2wdw          C                   CONST('Probabilmente in manutenzione-
023500130111     D*                                       ')
023600130111     D*ms3wdw          C                   CONST('da altro utente.             -
023700130111     D*                                       ')
023800130111     D*ms4wdw          C                   CONST('Rieseguire il lancio.        -
023900130111     D*                                       ')
024000130111     D ms2wdw          C                   CONST('                             -
024100130111     D                                        ')
024200130111     D ms3wdw          C                   CONST('Segnalare al CED di verificar-
024300130111     D                                     e  ')
024400130111     D ms4wdw          C                   CONST('il JOBLOG                    -
024500130111     D                                        ')
024600130729
024700130729       // -?Costante per controllo "caratteri solo numerici"?
024800130729     d c_Digits        c                   const('0123456789')
024900111018
025000111018      // - Comando di override al PrtF
025100111018     d C_CmdOvrPrtF    c                   const('OVRPRTF +
025200111018     d                                           file(PRTEMAIL) +
025300111018     d                                           pagesize(66 132) +
025400111018     d                                           lpi(6) cpi(10) +
025500111018     d                                           ovrscope(*actgrpdfn) +
025600111018     d                                           ')
025700111018     d C_CmdDltOvr     c                   const('DLTOVR +
025800111018     d                                            file(PRTEMAIL) +
025900111018     d                                            lvl(*actgrpdfn)')
026000111018      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
026100111018     d TRTCM1ds      e ds                  inz
026200111018      //   ·§CM1mitt = Indirizzo e-mail del mittente
026300120820     d   §CM1mitt    e                     inz(' ed@brt.it')
026400111018      //   ·§CM1dst  = Indirizzo e-mail del destinatario
026500120302     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
026600111018      //   ·§CM1tips = Tipo lettera via e-mail:
026700111018      //    "LET" = testo allegato in corpo con logo
026800111018      //            (richiede righe libere iniziali per il logo)
026900120820      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
027000111018      //            (non consente né UNDERLINE né HIGHLIGHT)
027100120820     d   §CM1tips    e                     inz('COI')
027200111018      //   ·§CM1po   = Filiale
027300111018     d   §CM1po      e                     inz('046')
027400111018      //   ·§CM1var  = Oggetto e-mail
027500111018     d   §CM1var     e                     inz('*OBJM*+
027600111019     d                                     Manutenzione data spedizione+
027700111019     d                                      bolle ')
027800111018      //   ·§CM1sts  = Stato
027900111018     d   §CM1sts     e                     inz(*off)
028000111018      //   ·§CM1sts  = Id processo
028100111018     d   §CM1idp     e                     inz('2')
028200111018
028300111018
028400910412     C****************************************************************
028500910412     C*  RIEPILOGO INDICATORI
028600910412     C***************************************************************
028700070612     C* 01    - possibilità di richidere bolle in sospensione x colli non
028800070612     c*         spuntati
028900070612     C* 02    -
029000070612     C* 03    - per colli non spuntati possibilità di modificare la consegna
029100130717     c*         richiesta da tabella BSP
029200070612     C* 12    - PROTEZIONE CAMPO LINEA DI PARTENZA XCHE' SONO UN 2° LIVELLLO
029300910412     C* 30    - DI COMODO
029400980129     C* 40-50 - ERRORE
029500980130     C* 51-55 - GESTIONE CHIAVE LETTURA BOLLE
029600070612     C* 70    - posizionam.cursone su DCR
029700070612     C* 90    - ERRORE GENERICO
029800910412     C*****************************************************************
029900941116     C**
030000980130     C     *ENTRY        PLIST
030100980130     C                   PARM                    KPJBA
030200030619      *
030300130327     *** * Richiedo i dati per la stampa ed eseguo le override
030400130327     ***c                   exsr      SR_OvrPrtF
030500130327     ***C**
030600130327     ***C* F3 - FINE
030700130327     ***C     D90F3         IFEQ      '1'
030800130327     ***C                   GOTO      FINE
030900130327     ***C                   ENDIF
031000130327     *** * Errori nelle OVERRIDE -> FINE
031100130327     ***C   91              GOTO      FINE
031200991206     C*
031300950411     C     INIZIO        TAG
031400980130     C                   EXSR      PUL01
031500950411     C*
031600980130     C                   MOVE      WDTGIO        WAAS              4 0
031700980130     C                   MOVEL     CDU           V1CLNP
031800950411     C**
031900900509     C     FOR01         TAG
032000980129     C                   EXFMT     LS03D01
032100941227     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
032200110704     C                   SETOFF                                       902873
032300110704     C                   SETOFF                                       707172
032400130711     C                   SETOFF                                       74
032500081210     c                   clear                   v1cmsg
032600130711     C** F3 - FINE LAVORO
032700900509     C   KC              GOTO      FINE
032800130711     c
032900130726     c* F11-  seleziona  tabella sospensione senza codici senza spunte
033000130711     c*       tabella DSP
033100130711     c                   if        *inkk=*on
033200130729      * Sola visualizzazione di TUTTI i clienti (di qualsiasi filiale)
033300130729      *   in tab. "BSP"
033400151201     c***                clear                   tntbBSPds1
033500151201     c***                eval      iBSPmod = 'V'
033600151201     c***                eval      iBSPtfp = DUTtfp
033700151201     c***                eval      KPJBU = tntbBSPds1
033800151201     c***                call      'TNTBBSPR1'
033900151201     c***                parm                    KPJBA
034000151201     c***                eval      tntbBSPds1 = KPJBU
034100151201     c***                goto      for01
034200151216      * Sola visualizzazione  dei codici del terminal di partenza
034300151201     c                   clear                   fnlsbsp1ds
034400151201     c                   eval      iBSPmod = 'V'
034500151216     c                   eval      iBSPtfp = DUTtfp
034600151201     c                   eval      KPJBU = fnlsbsp1ds
034700151201     c                   call      'FNLSBSP1R'
034800151201     c                   parm                    KPJBA
034900151201     c                   eval      fnlsbsp1ds = KPJBU
035000151201     c                   goto      for01
035100151201     c                   endif
035200130711     c
035300910205     C* CONTROLLI FORMATO
035400980130     C                   EXSR      CNT01
035500980202     C  NKF
035600130711     CANNKH
035700941228     COR 90              GOTO      FOR01
035800130711     c
035900130711     c                   if        *inkh=*on
036000130711     c                   clear                   fnlsu3ds
036100130711     c                   eval      ilsu3dsp_B=%editc(wdtcar:'X')
036200130711     c                   eval      ilsu3dsp_S=%editc(wlpdsp:'X')
036300130711     c                   eval      ilsu3nos='S'
036400130711     c                   movel     fnlsu3ds      kpjbu
036500130711     C                   MOVEL     'LSU3'        KCOAZ
036600130711     C                   CALL      'FNLSU3R'
036700130711     C                   PARM                    KPJBA
036800130711     c*
036900130711     c                   movel     kpjbu         fnlsu3ds
037000130711
037100130711     c                   if        olsu3boagg>*zeros
037200130711     c                   movel     olsu3boagg    bollema
037300130830     c                   endif
037400130830     c                   if        olsu3bonagg>*zeros
037500130830     c                   movel     olsu3bonagg   werror
037600130830     c                   endif
037700130830     c                   eval      wmsg=olsu3msg
037800130711     c                   exsr      emis_msg
037900130830
038000130830     c                   if        werror>0
038100130830     c                   do        *hival
038200130830     c                   exfmt     ls03w01
038300130830     c                   if        *inkl='1'
038400130830     c                   leave
038500130830     c                   endif
038600130830     c                   enddo
038700130830     c                   endif
038800130711     c
038900130711     c                   goto      inizio
039000130711e   1C                   ENDIF
039100900523     C*
039200980129     C* M O D I F I C A   D A T A   S P E D I Z I O N E
039300130327     ***C   KH
039400130327     ***COR KF              EXSR      MOD01
039500130327     C   KF              EXSR      MOD01
039600130711     c
039700081211     c                   if        werror>0
039800081211     c                   do        *hival
039900081211     c                   exfmt     ls03w01
040000081215     c                   if        *inkl='1'
040100081211     c                   leave
040200081211     c                   endif
040300081211     c                   enddo
040400081211     c                   endif
040500081211
040600910207     C*
040700950411     C                   GOTO      INIZIO
040800000000     C     FINE          TAG
040900980202     C**
041000130327     ***C     D90F3         IFne      '1'
041100130327if  1***c                   if        DB6num > *zero
041200130327     ***c                   clear                   FNLSB5ds
041300130327     ***c                   eval      DB0tbo = 'P'
041400130327     ***c                   eval      DB0fa4 = 'I'
041500130327     ***c                   eval      DB6pdf = 'S'
041600130327     ***c                   call      D90psl
041700130327     ***c                   parm                    FNLSB5ds
041800130327     ***c                   parm                    FNLSB6ds1
041900130327e   1***c                   endif
042000130327     ***c                   clear                   FNLSB5DS
042100130327     ***c                   movel     'C'           DB0TLA
042200130327     ***c                   call      D90psl
042300130327     ***c                   parm                    FNLSB5DS
042400130327     ***c                   endif
042500980202     C*
042600980202     C                   CLEAR                   DSLV55
042700980202     C                   MOVEL     'C'           D55TLA
042800980202     C                   CALL      'FNLV55R'
042900980202     C                   PARM                    DSLV55
043000081203     c*
043100081203     C                   CLEAR                   fnls65ds
043200081203     C                   MOVEL     'C'           i65tla
043300081211     c                   movel     '1'           I65STRCMT
043400081203     c                   movel     fnls65ds      kpjbu
043500081205     C                   CALL      'FNLS65R'
043600081203     C                   PARM                    kpjba
043700980202     C**
043800060926     c                   clear                   kpjbu
043900060926     c                   movea     elabor        kpjbu
044000060926     c                   if        kpjbu<>*blanks
044100060926     C                   MOVEL     'LS0G'        KCOAZ
044200060926     C                   CALL      'BCH10'
044300060926     C                   PARM                    KPJBA
044400060926     c                   endif
044500060926     c
044600061012     c
044700061012     c                   clear                   qcmd
044800061012     c                   eval      qcmd = dltovr(1)
044900061012     c                   call      'QCMDEXC'                            91
045000061012     c                   parm                    Qcmd
045100061012     c                   parm                    Qlen
045200061012     c
045300111018     c                   if        invbo(1)>0
045400111018     c                   exsr      Inv_email
045500111018     c                   endif
045600111018     c
045700000000     C                   SETON                                        LR
045800941116     C*
045900980129     C*****************************************************************
046000910205     C* CONTROLLO FORMATO -------------------------------------------
046100980129     C*****************************************************************
046200980130     C     CNT01         BEGSR
046300910205     C                   SETOFF                                       90
046400130729      *
046500130729     c                   clear                   W1Cksc
046600130729     c                   Select
046700130729      * - Ricerca da tab. "BSP"
046800130729     c                   When      %scan( '?' : V1Cksc ) > *zero
046900130729     c                   clear                   V1Cksc
047000151201     c****               clear                   tntbBSPds1
047100151201     c****               eval      KPJBU = tntbBSPds1
047200151201     c****               call      'TNTBBSPR1'
047300151201     c****               parm                    KPJBA
047400151201     c****               eval      tntbBSPds1 = KPJBU
047500151216      *   Visualizzazione di tutti i cliente a prescindere da tfp
047600151216      *   esclusione dei codici che partono da + filiali
047700151201     c                   clear                   fnlsbsp1ds
047800151204     c                   eval      ibsptps='K'
047900160111     c                   eval      iBSPfpa = 'C'
048000151201     c                   eval      KPJBU = fnlsbsp1ds
048100151201     c                   call      'FNLSBSP1R'
048200151201     c                   parm                    KPJBA
048300151201     c                   eval      fnlsbsp1ds = KPJBU
048400130729     c                   if        oBSPfxx <= *zero  and
048500151204     c                             oBSPerr  = *blanks
048600130729     c                   eval      W1Cksc = oBSPcli
048700130729     c                   eval      V1Cksc = %editc( oBSPcli : 'X' )
048800130729     c                   eval      V1Dksc = oBSPdes
048900130729     c                   endif
049000130729     c                   eval      *in74 = *on
049100130729     c                   eval      *in90 = *on
049200130729     c                   leavesr
049300130729      * - Verifica se contiene solo caratteri numerici
049400130729     c                   When      V1Cksc <> *blanks  and
049500130729     c                             %checkR( c_Digits : V1Cksc ) > *zero
049600130729     c                   movel     Msg(16)       V1Cmsg
049700130729     c                   eval      *in28 = *on
049800130729     c                   eval      *in74 = *on
049900130729     c                   eval      *in90 = *on
050000130729     c                   leavesr
050100130729     c                   EndSl
050200130729      *
050300130729     c                   if        V1Cksc > *zeros
050400130729     c                   eval      W1Cksc = %int( V1Cksc )
050500130729     c                   endif
050600130729      *
050700130711     c* Se ho premuto F8 tutte le selezioni ddevono essere vuote
050800130711     c*  tranne le date
050900130711     c                   if        *inkh=*on
051000130711     c
051100130729     c                   if        W1cksc>*zeros or
051200130711     c                             v1clna>*zeros or
051300130711     c                             v1ctfa>*zeros or
051400130711     c                             v1cpdr >*zeros or
051500130711     c                             sel4>0
051600130711     C                   MOVEL     MSG(15)       V1CMSG
051700130711     C                   SETON                                        287490
051800130711     C                   GOTO      ENDCTR
051900130711     C                   ENDIF
052000130729     c*
052100130711     c                   else
052200910412     C*
052300980129     C* SE TUTTE LE SELEZIONI SONO A BLANKS E' ERRORE
052400130729     C     W1CKSC        IFEQ      *ZEROS
052500980130     C     V1CLNP        ANDEQ     *ZEROS
052600980129     C                   SETON                                        4090
052700941116     C                   GOTO      ENDCTR
052800941116     C                   ENDIF
052900051118     c
053000980129     C* CODICE CLIENTE
053100130729    1C     W1CKSC        IFEQ      0
053200980130    2C     V1DKSC        IFNE      *BLANKS
053300980129     C* PARSTA = 9  ESCLUDO ANNULLATI
053400980129     C                   Z-ADD     9             PARSTA
053500980129     C                   MOVEL     RSUT          PARDUT           30
053600980129     C                   MOVEL     *BLANKS       DESCR            48
053700980130     C                   MOVEL     V1DKSC        DESCR
053800130729     C                   Z-ADD     dutKCI        CCC               4 0
053900981109     C                   CLEAR                   PARFLR
054000981109     C                   CLEAR                   PARDIT
054100000907     C                   Z-ADD     1             PAXNUM
054200000907     C                   CALL      'XALFA3BR'
054300980129     C                   PARM                    PARDUT
054400980129     C                   PARM                    CODUT
054500980129     C                   PARM                    DESCR
054600980129     C                   PARM                    CCC
054700980129     C                   PARM                    PARSTA            1 0
054800981109     C                   PARM                    PARFLR           90
054900981109     C                   PARM                    PARDIT            3
055000000907     C                   PARM                    PAXNUM            2 0
055100000907     C                   PARM                    PAXKCM           80
055200000907     C                   PARM                    PAXKSM          140
055300000907     C                   PARM                    PAXKDM           60
055400980129     C     PARSTA        IFNE      -1
055500130729     C                   MOVEL     PAXKSM        W1CKSC
055600000907     C                   MOVEL     PAXKSM        V1CKSC
055700980130     C                   MOVEL     DESCR         V1DKSC
055800980129     C                   ELSE
055900980202     C                   CLEAR                   V1DKSC
056000980129     C                   ENDIF
056100980129     C*
056200980129     C                   SETON                                        90
056300980129     C                   GOTO      ENDCTR
056400980129    2C                   ENDIF
056500980129   X1C                   ELSE
056600980129     C*
056700980129     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
056800130729     C                   Z-ADD     dutKCI        CCC
056900130729     c                   mllzo     1             W1cksc
057000980129     C     KACO          CHAIN     CNACO000                           41
057100980129    2C  N41ACOFLG        IFNE      ' '
057200980129     C                   SETON                                        41
057300980129    2C                   ENDIF
057400980129     C     *IN41         IFEQ      *OFF
057500980202     C                   MOVEL     ACORAG        V1DKSC
057600980129     C                   ELSE
057700980204     C                   CLEAR                   V1DKSC
057800980129     C                   SETON                                        90
057900980129     C                   GOTO      ENDCTR
058000980129     C                   END
058100980129    1C                   ENDIF
058200130729     c*
058300980130     C* LINEA DI PARTENZA
058400061003     c                   mllzo     1             v1clnp
058500090126     c                   clear                   ntwlnp
058600980130    1C     V1CLNP        IFNE      *ZEROS
058700980130     C* CONTROLLO CHE SIA GESTIBILE
058800980130     C     V1CLNP        LOOKUP    L1                                     30
058900980130    2C     *IN30         IFEQ      *OFF
059000980130     C                   SETON                                        9042
059100980204     C                   CLEAR                   V1DLNP
059200980130     C                   GOTO      ENDCTR
059300980130    2C                   ENDIF
059400980130     C*
059500090126     c                   clear                   og143
059600980130     C     V1CLNP        CHAIN     AZORG01L                           30
059700980130     C   30              SETON                                        9042
059800980204     C   30              CLEAR                   V1DLNP
059900980130     C   30              GOTO      ENDCTR
060000980130     C  N30              MOVEL     ORGDES        V1DLNP
060100090126     c  n30              movel     orgde3        og143
060200090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
060300090126     c                             §ogntw='FED'
060400090126     c                   eval      ntwlnp='E'
060500090126     c                   endif
060600980130    1C                   ENDIF
060700980130     C* LINEA DI ARRIVO
060800061003     c                   clear                   wclna
060900980130    1C     V1CLNA        IFNE      *BLANKS
061000980130     C     V1CLNA        ANDNE     *ZEROS
061100980130     C*
061200980130     C     '?'           SCAN      V1CLNA                                 30
061300980130     C*
061400980130    2C     *IN30         IFEQ      *ON
061500980130     C                   MOVE      *BLANKS       §COD1
061600980130     C                   MOVE      *BLANKS       V1CLNA
061700980130     C                   CALL      'TNSD19R'
061800980130     C                   PARM                    §COD1             3
061900980130     C                   PARM                    §TIP              1
062000980202     C                   PARM                    §DES             25
062100980130     C                   MOVEL     §COD1         V1CLNA
062200980130     C                   MOVEL     §DES          V1DLNA
062300980130     C                   SETON                                        90
062400980130     C                   GOTO      ENDCTR
062500980130    2C                   ENDIF
062600980130     C* ESEGUO TESTN
062700980130     C                   TESTN                   V1CLNA               30
062800980130     C                   MOVE      V1CLNA        W001A
062900980130     C     *IN30         IFEQ      *OFF
063000980130     C     W001A         ORLT      '0'
063100980204     C                   CLEAR                   V1DLNA
063200980130     C                   GOTO      ENDCTR
063300980130     C                   ENDIF
063400980130     C*
063500980130     C*  CONTROLLO
063600980130     C*
063700980130     C                   MOVE      V1CLNA        WCLNA             3 0
063800980130     C     WCLNA         CHAIN     AZORG01L                           30
063900980205     C* CONTROLLO SE ANNULLATO
064000980205     C* CONSIDERO SOLO I TIPI "F" E "A"
064100980205    2C  N30ORGFVA        IFNE      *BLANKS
064200980205    2C     ORGFAG        ORNE      'F'
064300980205     C     ORGFAG        ANDNE     'A'
064400980205     C                   SETON                                        30
064500980205    2C                   END
064600980130     C   30              MOVEL     *BLANKS       V1DLNA
064700980130     C   30              SETON                                        9044
064800980130     C   30              GOTO      ENDCTR
064900980130     C  N30              MOVEL     ORGDES        V1DLNA
065000980130    1C                   ENDIF
065100980130     C*
065200980130     C* CONTROLLO CODICE AUTOTRASPORTATORE
065300061003     c                   clear                   wcpdr
065400980130    1C     V1CPDR        IFNE      *BLANKS
065500980130     C     V1CPDR        ANDNE     *ZEROS
065600980130     C                   CLEAR                   V1DPDR
065700980130     C*
065800980130     C     '?'           SCAN      V1CPDR                                 30
065900980130     C*
066000980130     C     *IN30         IFEQ      *ON
066100021203     C*                  MOVEL     *BLANKS       PA2DES
066200021203     C*                  Z-ADD     0             PA2PAD
066300021203     C*                  Z-ADD     V1CLNP        PA2FIL
066400021203     C*                  MOVEL     'R'           PA2FLG
066500021203     C*                  MOVEL     PARAM2        KPJBU
066600021203      *
066700021203      * imposto i campi della ds esterna fnlv24ds
066800021203     C                   clear                   fnlv24ds
066900021203     C                   eval      d24fil   = v1clnp
067000021203     C                   eval      d24tip   = 'A'
067100021203     C                   eval      d24flg   = 'R'
067200021203     C                   movel     fnlv24ds      kpjbu
067300980130     C                   CALL      'FNLV24R'
067400980130     C                   PARM                    KPJBA
067500021203     C*                  MOVEL     KPJBU         PARAM2
067600021203     C*                  MOVE      PA2PAD        V1CPDR
067700021203     C*                  MOVEL     PA2DES        V1DPDR
067800021203      *
067900021203      * valorizzo i campi a video
068000021203     C                   movel     kpjbu         fnlv24ds
068100021203     C                   move      d24pdr        v1cpdr
068200021203     C                   movel     d24rsc        v1dpdr
068300980130     C                   GOTO      ENDCTR
068400980130     C                   END
068500980130     C* ESEGUO TESTN
068600980130     C                   TESTN                   V1CPDR               30
068700980130     C                   MOVE      V1CPDR        W001A             1
068800980130     C     *IN30         IFEQ      *OFF
068900980130     C     W001A         ORLT      '0'
069000980205     C                   CLEAR                   V1CPDR
069100980205     C                   CLEAR                   V1DPDR
069200980130     C                   GOTO      ENDCTR
069300980130     C                   ENDIF
069400980130     C*
069500021204     C                   CLEAR                   V1DPDR
069600980130     C                   MOVE      V1CPDR        WCPDR             7 0
069700021203     C*    WCPDR         CHAIN     FNAPD000                           30
069800021204     C                   move      wcpdr         kpdpdr
069900021203     C                   move      'A'           kpdtip
070000021203     C     kfapd         chain     fiapd01l                           30
070100980130     C     *IN30         IFEQ      *ON
070200980205     C                   CLEAR                   WCPDR
070300980205     C                   CLEAR                   V1DPDR
070400980130     C                   SETON                                        4590
070500980130     C                   GOTO      ENDCTR
070600980204     C                   CLEAR                   V1DPDR
070700980204     C                   ELSE
070800980204     C                   MOVEL     APDRSC        V1DPDR
070900980130     C                   ENDIF
071000980130     C*
071100980205     C* RECUPERO LA LINEA DI PARTENZA DALLA PRIMA PARTE DELL'AUTOTRASPORTATORE
071200980130     C     V1CLNP        IFEQ      *ZEROS
071300980202     C                   MOVEL     WCPDR         V1CLNP
071400090126     c                   clear                   ntwlnp
071500090126     c                   clear                   og143
071600980130     C     V1CLNP        CHAIN     AZORG01L                           30
071700980130     C  N30              MOVEL     ORGDES        V1DLNP
071800090126     C  N30              MOVEL     ORGDE3        og143
071900090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
072000090126     c                             §ogntw='FED'
072100090126     c                   eval      ntwlnp='E'
072200090126     c                   endif
072300980204     C   30              MOVEL     *BLANKS       V1DLNP
072400980205     C                   ELSE
072500151207     C* CONTROLLO LA LINEA DI PARTENZA CON QUELLA DELL'AUTOTRASPORATTORE
072600980205     C                   MOVEL     WCPDR         W0030
072700980205     C     V1CLNP        IFNE      W0030
072800980205     C     W0030         ANDNE     *ZEROS
072900980205     C                   SETON                                        56  90
073000980205     C                   Z-ADD     *ZEROS        W0030
073100980205     C                   GOTO      ENDCTR
073200980205     C                   ENDIF
073300980202     C                   ENDIF
073400980205     C                   ENDIF
073500051118
073600110704     c* controllo numero ORM
073700110704     c                   if        sel4>0
073800110704      * P.O. EMISSIONE
073900110704     C                   Z-ADD     V1CPOE        NUM3              3 0
074000110704     C     NUM3          CHAIN     AZORG01L                           30
074100110704     C     *IN30         IFEQ      *ON
074200110704    2C     ORGFVA        orne      *BLANKS
074300110704    2C     ORGFAG        ORNE      'F'
074400110704     C     ORGFAG        ANDNE     'A'
074500110704     C                   MOVEL     MSG(12)       V1CMSG
074600110704     C                   SETON                                        287290
074700110704     C                   GOTO      ENDCTR
074800110704     C                   ENDIF
074900110704      * N.ORM
075000110704     C     KFORM         CHAIN     FNORM01L                           30
075100110704     C     *IN30         IFEQ      *ON
075200110704     C                   MOVEL     MSG(13)       V1CMSG
075300110704     C                   SETON                                        287390
075400110704     C                   GOTO      ENDCTR
075500110704     C                   ENDIF
075600110704      *
075700110704     C                   SETON                                        06
075800110704      *
075900110704     C                   ENDIF
076000051118     c* Se immessa solo lnp, immettere anche cliente o linea arrivo o
076100051118     c*  terminal arrivo o autotrasportatore
076200051118     c                   if        v1clnp>0 and
076300130729     c                             (W1cksc=0 and v1clna<=*zeros and v1ctfa=0
076400110704     c                              and v1cpdr<=*zeros and v1cnos=' ' and
076500110704     c                              sel4=0)
076600051118     C                   SETON                                        6090
076700051118     C                   GOTO      ENDCTR
076800051118     C                   ENDIF
076900051209     c
077000080620     c                   if        not *in01
077100080620     c                   clear                   v1cnos
077200080620     c                   endif
077300080620     c
077400080620     c* Per mettere in sospensione  in base alle spunte devo essere
077500151204     c*  abilitato (Fibsp00F)
077600051209     C
077700130718     c                   clear                   Wbsp
077800160111     c                   clear                   Wbspchf
077900130729    1c                   if        W1cksc>0
078000130718     c***                Clear                   tibs02ds
078100130718     c***                Movel     'C'           T02Mod
078200130718     c***                Movel     KNSIF         T02Sif
078300130718     c***                Movel     'CLI'         T02Cod
078400130729     c***                Movel     W1Cksc        T02Ke1
078500130718     c***                Call      'TIBS02R'
078600130718     c***                Parm                    Kpjba
078700130718     c***                Parm                    tibs02ds
078800130718    2c***                If        T02Err = *Blanks
078900130718     c***                Movel     T02Uni        Dcli
079000130718    2c**                 endif
079100130718     c* cerco il cliente in tabella bsp
079200130718     c                   exsr      cer_bsp
079300080620    1c                   endif
079400080620
079500130718     c                   if        not *in01 and wbsp='S'
079600080620     c                   seton                                        01
079700080620     C                   SETON                                        90
079800080620     C                   GOTO      ENDCTR
079900080620     c                   endif
080000130718     c                   if        sosdaL1=' 'and *in01 and wbsp =' '
080100080620     c                   setoff                                       01
080200080620     C                   SETON                                        90
080300080620     C                   GOTO      ENDCTR
080400080620     c                   endif
080500051209     c
080600080620    1c                   if        v1cnos='S'
080700130729    2c                   if        W1cKSC=0 and ntwlnp<>'E'
080800080620     C                   SETON                                        5790
080900080620     C                   GOTO      ENDCTR
081000080620    2C                   ENDIF
081100080620     c
081200130718    2c                   if        wbsp=' ' and ntwlnp<>'E'
081300160111     c                   if        wbspchf=' '
081400051209     C                   SETON                                        58  90
081500151209     c                   else
081600151209     C                   SETON                                        61  90
081700151209     c                   endif
081800051209     C                   GOTO      ENDCTR
081900070614   x2c                   else
082000060403     c* se numero giorni per stampa = 0 --> errore
082100070614    3c                   if        v1cggl = 0
082200060403     C                   SETON                                        59  90
082300070614    3c                   endif
082400070612     c* se posso emetto il campo della consegna richiesta
082500151204    3c                   if        BSPDCRP='S' and not *in03
082600070612     c                   seton                                        0390
082700070612     c                   clear                   v1ctcr
082800070612     c                   clear                   v1cdcr
082900070612     c                   clear                   v1chcr
083000070612     C                   GOTO      ENDCTR
083100070614    3c                   endif
083200070612     c
083300151204    3c                   if        BSPDCRP=' ' and *in03
083400070612     c                   seton                                        90
083500070612     c                   setoff                                       03
083600070612     c                   clear                   v1ctcr
083700070612     c                   clear                   v1cdcr
083800070612     c                   clear                   v1chcr
083900070612     C                   GOTO      ENDCTR
084000070614    3c                   endif
084100070612     c* Altrimenti controllo la consegna richiesta
084200151204    3c                   if        BSPDCRP='S'
084300070612     c                   exsr      CtrCONSRICH
084400070614    4c                   if        *in90
084500070612     c                   goto      ENDCTR
084600070614    4c                   endif
084700070614    3c                   endif
084800070612     c
084900070614    2C                   ENDIF
085000070614   x1c                   else
085100070614     c* se cliente senza abilitazione bolla tolgo la consegna rich
085200070614    3c                   if        *in03
085300070614     c                   seton                                        90
085400070614     c                   setoff                                       03
085500070614     c                   clear                   v1ctcr
085600070614     c                   clear                   v1cdcr
085700070614     c                   clear                   v1chcr
085800070614     C                   GOTO      ENDCTR
085900070614    3c                   endif
086000070614    1C                   ENDIF
086100130711     c
086200130711     c                   endif
086300051209     c
086400980130     C* CONTROLLO DATA SPEDIZIONE DA MODIFICARE ----------------------*
086500980130     C*
086600980130     C                   Z-ADD     V10DSP        G02DAT
086700980130     C                   MOVEL     *BLANK        G02ERR
086800980130     C                   CALL      'XSRDA8'
086900980130     C                   PARM                    WLBDAT
087000980130     C* ERRATA
087100980130    1C     G02ERR        IFEQ      '1'
087200980130     C                   SETON                                        46  90
087300980130     C                   GOTO      ENDCTR
087400980130    1C                   ENDIF
087500980130     C*
087600980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
087700980130     C*  IMMESSO DA VIDEO
087800980130     C                   Z-ADD     G02DAT        V10DSP
087900980130     C*
088000980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
088100980130     C                   MOVE      V10DSP        WAAS              4 0
088200980202     C                   MOVE      G02INV        WMGS              4 0
088300980203     C                   Z-ADD     G02INV        WDTCAR            8 0
088400001120     C                   MOVEL     G02INV        WAMSPE            6 0
088500980130     C*
088600980130     C*
088700980130     C* CONTROLLO DATA SPEDIZIONE CHE SOSTITUISCE LA VECCHIA----------*
088800980130     C*
088900980130     C                   Z-ADD     V11DSP        G02DAT
089000980130     C                   MOVEL     *BLANK        G02ERR
089100980130     C                   CALL      'XSRDA8'
089200980130     C                   PARM                    WLBDAT
089300980130     C* ERRATA
089400980130    1C     G02ERR        IFEQ      '1'
089500980130     C                   SETON                                        47  90
089600980130     C                   GOTO      ENDCTR
089700980130    1C                   ENDIF
089800980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
089900980130     C*  IMMESSO DA VIDEO
090000980130     C                   Z-ADD     G02DAT        V11DSP
090100980202     C* CONTROLLO CHE LE DUE DATE SIANO UGUALI
090200980202     C     V11DSP        IFEQ      V10DSP
090300980204     C                   SETON                                        5090
090400980202     C                   GOTO      ENDCTR
090500980202     C                   ENDIF
090600980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
090700980130     C                   MOVE      V11DSP        WAASN             4 0
090800980130     C*
090900140114     c* questo controllo non lo faccio se premuto f8-sosp automatica
091000140114    2C  nkhWAASN         IFNE      WAAS
091100130729     c                   if        W1cksc<>0
091200081203     C                   MOVEL(P)  '3C'          COD
091300130729     C                   MOVEL(p)  W1cksc        KEY
091400081203     C     ktab2         chain     tabel00f                           31
091500081203     C   31              clear                   ds3c
091600081203     C  N31              movel     tbluni        ds3c
091700081203     c                   endif
091800130729     c     W1cksc        ifeq      0
091900090126     c     ntwlnp        andne     'E'
092000130729     c     W1cksc        orgt      0
092100090126     c     §3cfsp        andeq     'S'
092200980130     C                   SETON                                        4790
092300980130     C                   GOTO      ENDCTR
092400081203     c                   endif
092500980130    2C                   ENDIF
092600160107     c* Anche se tecnicamente funziona,
092700160107     c* impedisco sospensione all'indietro a cavallo di anno (anche per sosp.
092800160107     c* automatica)   - per richiesta di Luciano
092900160107     c                   if        waasn<>waas and waasn<waas
093000160107     C                   SETON                                        6290
093100160107     C                   GOTO      ENDCTR
093200160107     c                   endif
093300980130     C*
093400980130     C                   Z-ADD     G02INV        WLPDSP            8 0
093500001120     C                   MOVEL     G02INV        WAMSOS            6 0
093600980130     C* DATE SPEDIZIONE NO > DATA MAX DI SOSPENSIONE
093700980130    2C     WLPDSP        IFGT      DATAPA
093800980130     C                   SETON                                        4990
093900980130     C                   GOTO      ENDCTR
094000980130    2C                   ENDIF
094100980130     C*
094200980130     C     WLPDSP        IFLT      DATEU
094300980130     C                   SETON                                        4890
094400980130     C                   GOTO      ENDCTR
094500980130     C                   ENDIF
094600070612     c* Se immessa la consegna richiesta, controllo che non sia
094700070612     c*  inferiore alla nuova data spedizione
094800070612     c                   if        *in03 and  v1cdcr>0  and wdcr<wlpdsp
094900070612     c                   seton                                        702890
095000070612     c                   movel     msg(9)        v1cmsg
095100070612     c                   goto      endctr
095200070612     c                   endif
095300980203     C*
095400980203     C* TERMINAL DI ARRIVO SOLO DOPO AVER INSERITO LA DATA DI SPEDIZIONE
095500980203    1C     V1CTFA        IFNE      *ZEROS
095600980203     C* VERIFICO SE E' CORRETTO
095700980203     C                   MOVE      '0'           CRISP             1
095800980203     C     'A'           SETLL     AZCAE01L
095900980203     C     'A'           READE     AZCAE01L                               30
096000980203    2C     *IN30         DOWEQ     *OFF
096100980203    3C     V1CTFA        IFEQ      CAETFE
096200980203     C     CAEATB        ANDEQ     *BLANKS
096300980203     C     WDTCAR        ANDGE     CAEDDE
096400980203     C     WDTCAR        ANDLE     CAEDSC
096500980203     C     CAETFP        IFEQ      0
096600980203     C     CAETFP        OREQ      SIMFEL
096700980203     C                   MOVE      '1'           CRISP
096800980203     C                   END
096900980203    3C                   END
097000980203     C     'A'           READE     AZCAE01L                               30
097100980203    2C                   ENDDO
097200980203     C* ERRORE
097300980203    2C     CRISP         IFEQ      '0'
097400980203     C                   SETON                                        9043
097500980203     C                   MOVE      *BLANKS       V1DTFA
097600980203     C                   GOTO      ENDCTR
097700980203   X2C                   ELSE
097800980203     C* DECODIFICA
097900980203     C     V1CTFA        CHAIN     AZORG01L                           30
098000980203     C  N30              MOVEL     ORGDES        V1DTFA
098100980203    2C                   END
098200980203    1C                   END
098300980130     C*
098400130327     ***C  NKF
098500130327     ***CANNKH              SETON                                        90
098600130711     C**  NKF              SETON                                        90
098700980130     C*
098800980130     C     ENDCTR        ENDSR
098900070612     C*****************************************************************
099000070612     C* controllo dati consegna richiesta      ----------------------
099100070612     C*****************************************************************
099200070612     C     CtrCONSRICH   BEGSR
099300070612      * tipo
099400070612    1c                   If        V1cTcr <> *Blanks   and
099500070612     c                             v1cDcr = *Zeros
099600070612     c                   Eval      *In28 = *On
099700070612     c                   Eval      *In70 = *On
099800070612     c                   Eval      *In90 = *On
099900070612     c                   Eval      v1cMsg = msg(6)
100000070612     c                   GoTo      EndCons
100100070612    1c                   EndIf
100200070612      * data
100300070614     c                   clear                   wdcr
100400070612    1c                   If        V1cDcr > *Zeros
100500070612      * controllo formale della data
100600070612     c                   Clear                   wlbdat
100700070612     c                   Z-Add     V1cDcr        G02Dat
100800070612     c                   Call      'XSRDA8'
100900070612     c                   Parm                    wlbdat
101000070612    2c                   If        G02Err = '1'
101100070612     c                   Eval      *In28 = *On
101200070612     c                   Eval      *In70 = *On
101300070612     c                   Eval      *In90 = *On
101400070612     c                   Eval      v1cMsg = msg(7)
101500070612     c                   GoTo      EndCons
101600070612    2c                   EndIf
101700070612     c
101800070612     c                   Movel     G02Dat        V1cDcr
101900070612     c                   Movel     G02Inv        wdcr
102000070612      * Non deve essere inferiore/uguale a oggi
102100070612    2c                   If        wdcr <= dateu
102200070612     c                   Eval      *In28 = *On
102300070612     c                   Eval      *In85 = *On
102400070612     c                   Eval      *In90 = *On
102500070612     c                   Eval      v1cMsg = msg(7)
102600070612     c                   GoTo      EndCons
102700070612    2c                   EndIf
102800070612     c
102900070612      * Non deve essere superiore a oggi + 5 gg.
103000070612    2c                   if        v1ctcr = 'P'
103100070612     c                   z-add     woggidcgm1    s_woggidcg
103200070612     c                   z-add     §vpodcgm1     s_vpodcg
103300070612     c                   else
103400070612     c                   z-add     woggidcgm2    s_woggidcg
103500070612     c                   z-add     §vpodcgm2     s_vpodcg
103600070612    2c                   endif
103700070612    2c                   If        wdcr > s_woggidcg
103800070612     c                   Eval      *In28 = *On
103900070612     c                   Eval      *In70 = *On
104000070612     c                   Eval      *In90 = *On
104100070612     c                   Eval      v1cMsg = msg(8)
104200070612     c                   Eval      v1cMsg = %trim(v1cmsg) + ' ' +
104300070612     c                             %trim(%editc(s_vpodcg:'Z'))  +
104400070612     c                             ' giorni dalla data odierna'
104500070612     c                   GoTo      EndCons
104600070612    2c                   EndIf
104700070612    1c                   EndIf
104800070614     c* Ora richiesta
104900070614     c                   if        v1chcr>0
105000070614     c                   movel     v1chcr        whcr              2 0
105100070614     c                   move      v1chcr        wmcr              2 0
105200070614    2C     WHCR          IFLT      0
105300070614     C     WHCR          ORGT      24
105400070614     C                   MOVEL     msg(10)       V1cMSG
105500070614     C                   SETON                                        719028
105600070614     C                   GOTO      ENDCons
105700070614    2C                   ENDIF
105800070614     C*
105900070614    2C     WMCR          IFLT      0
106000070614     C     WMCR          ORGT      59
106100070614     C                   MOVEL     msg(10)       V1cMSG
106200070614     C                   SETON                                        719028
106300070614     C                   GOTO      ENDCons
106400070614    2C                   ENDIF
106500070614    2C                   ENDIF
106600070612     c
106700070612     c     ENDCONS       ENDSR
106800980130     C*****************************************************************
106900980130     C* ELABORAZIONE TESTATA E DETTAGLIO BOLLE ----------------------
107000980130     C*****************************************************************
107100980130     C     MOD01         BEGSR
107200980130     C*
107300980130     C                   SETOFF                                       515253
107400110704     C                   SETOFF                                       5455
107500081205     c                   clear                   werror
107600081210     c                   clear                   wmsg
107700980130     C* CONTROLLO CHE TIPO DI SELEZIONE E' STATA FATTA
107800980130     C* PER CLIENTE
107900130729     C     W1CKSC        COMP      *ZEROS                             5151
108000980130     C* PER LINEA PARTENZA
108100980130     C     V1CLNP        COMP      *ZEROS                             5252
108200980130     C* PER TERMINAL DI ARRIVO
108300980130     C     V1CTFA        COMP      *ZEROS                             5353
108400980130     C* PER LINEA ARRIVO
108500061003     C     WCLNA         COMP      *ZEROS                             54
108600980130     C* PER PADRONCINO
108700061003     C     WCPDR         COMP      *ZEROS                             55
108800020610
108900020610      * se richiesta una linea di partenza imposto solo quella linea di
109000020610      * partenza nella sk delle linee da stampare
109100020610     c                   if        v1clnp <> *zeros
109200020610     c                   clear                   sklnp
109300020610     c                   eval      sklnp(1) = v1clnp
109400020610     c                   else
109500020610     c                   movea     lin           sklnp
109600020610     c                   endif
109700110704     c
109800060403     c* se richiesto aggiornamento su bolle senza colli spuntati stampo
109900060403     c* testata per elenco spedizioni in sospensione da x giorni
110000060403     c   01              if        v1cnos = 'S'
110100060403     c                   open      fnls03p
110200060404     c                   move      v1cggl        w_gg              1
110300060404     c                   eval      ptitolo = ctitolo
110400060404     c                   eval      ptitolo = %xlate('X':w_gg:ptitolo)
110500090127     c                   eval      *in13='0'
110600130729     c                   if        W1cksc>0
110700090127     c                   eval      *in13='1'
110800090127     c                   endif
110900130729     c                   z-add     W1cksc        p1cccm
111000060404     c                   movel     v1dksc        p1dccm
111100090127     c                   z-add     v1clnp        p1clnp
111200090127     c                   movel     v1dlnp        p1dlnp
111300060403     c                   write     testa
111400060403     c                   endif
111500061005     c
111600061005     c                   open      qsysprt
111700060403     c
111800110704     c* se parzializzazione per numero ORM, leggo per ORM
111900110704     c*  che faccio prima
112000110704    0c                   if        sel4>0
112100110704     C                   MOVEL     SEL4          KN14
112200110704     C     Kar43         SETLL     Fiar404L
112300110704     C     Kar43         reade     Fiar404L
112400110704    1c                   dow       not %eof(fiar404l)
112500110704     c
112600110704     c* controllo la linea di partenza immessa o dell'area
112700110704     c                   eval      Indx=%lookup(ar4lnp:sklnp)
112800110704     c*
112900110704    2c                   if        Indx>0
113000110704
113100110704     c     kblpm         chain(n)  fnblp01l
113200110704
113300110704     c* Verifico anche la data spedizione
113400110704     c                   if        blpdsp=wdtcar
113500110704     c
113600110704     c                   exsr      Parzial
113700110704     c
113800110704    3c                   if        NoBolla=' '
113900110704    4c                   if        waasn<>waas
114000110704     c                   exsr      sr_nuovonumero
114100110704   x4c                   else
114200110704     c
114300110704     c                   exsr      Aggiorna
114400110704     c*
114500110704    4c                   endif
114600110704    4c                   endif
114700110704    3c                   endif
114800110704    2c                   endif
114900110704     c
115000110704     C     Kar43         reade     Fiar404L
115100110704    1c                   enddo
115200110704      *
115300110704   x0c                   else
115400020610
115500020610      * elaboro la sk delle linee da stampare
115600110704   1ac                   do        30            xx
115700110704    1c                   if        sklnp(xx) = *zeros
115800020610     c                   leave
115900110704    1c                   endif
116000020610
116100020610     c                   eval      lnp = sklnp(xx)
116200980130     C*
116300980130     C* LETTURA TESTATE BOLLE
116400020610     C  N52KBLP1         SETLL     FNBLP70L
116500980130     C   52
116600020610     CANN55KBLP2         SETLL     FNBLP70L
116700020610     C   55KBLP3         SETLL     FNBLP70L
116800110704     c
116900980203    1C                   DO        *HIVAL
117000020610     C  N52KBLP1         READE     FNBLP70L                               30
117100980203     C   52
117200020610     CANN55KBLP2         READE     FNBLP70L                               30
117300020610     C   55KBLP3         READE     FNBLP70L                               30
117400980203     C   30              LEAVE
117500110704     c
117600110704     c                   exsr      Parzial
117700110704     c
117800110704     c                   if        NoBolla='N'
117900110704     c                   iter
118000110704     c                   endif
118100110704     c
118200081203     c* Se cambia anche l'anno richiamo pgm apposito che esegue anche la
118300081203     c* rinumerazione della spedizione
118400110704    2c                   if        waasn<>waas
118500081203     c                   exsr      sr_nuovonumero
118600081203     c                   iter
118700110704    2c                   endif
118800110704     c
118900110704     c                   exsr      Aggiorna
119000980202     C*
119100980202    1C                   ENDDO
119200020610
119300110704   1ac                   enddo
119400110704    0c                   endif
119500060403     c*
119600060403     c   01              if        v1cnos = 'S'
119700060404     c   98              write     testa
119800060404     c                   setoff                                       98
119900060404     c                   write     fines
120000060403     c                   close     fnls03p
120100060403     c                   endif
120200061005     c
120300061005     c                   close     qsysprt
120400060713     c
120500060713     c                   if        bollemsg>0
120600111018     c                   eval      zz=zz+1
120700111018     c                   eval      invbo(zz)=bollemsg
120800130729     c                   if        W1cksc>0
120900130729     c                   eval      invcod(zz)=V1cksc + '-' + v1dksc
121000111018     c                   else
121100111018     c                   eval      invcod(zz)=%editc(v1clnp:'X')+'-'+
121200111018     c                             v1dlnp
121300111018     c                   endif
121400111018     c                   eval      invdim(zz)=savdim
121500111018     c
121600111019     c                   if        zz=50
121700111018     c                   exsr      Inv_email
121800111018     c                   clear                   invbo
121900111018     c                   clear                   invcod
122000111018     c                   clear                   invdim
122100111018     c                   endif
122200060713     c                   endif
122300111018     c
122400060713     c* emetto msg di quante bolle manutenzionate
122500130711     c                   exsr      emis_msg
122600130711
122700060926     c*
122800061003     c                   if        yy<6
122900060926     c                   add       1             yy
123000060926     c                   movel     param         elabor(yy)
123100060926     c                   endif
123200980130     C*
123300980202     C                   ENDSR
123400130711     C*----------------------------------------------------------------
123500130711     c     Emis_msg      BEGSR
123600130711     c****               movel     msg(5)        v1cmsg
123700130711     c****               eval      %subst(v1cmsg:27:11)= (%editc(bollema:'2'))
123800130711     c                   seton                                        28
123900130711    1c                   if        werror>0
124000130711    2c                   if        wmsg=*blanks
124100130711     c***                eval      widms1=%trim(widms1)+ 'ERRORE su '
124200130711     c***                eval      widms1=%trim(Widms1)+ (%editc(werror:'Z'))
124300130711     c****               eval      widms1=%trim(Widms1)+ ' bolle.'
124400130711     c                   eval      widms1=%trim(%editc(werror:'Z')) +
124500130711     c                             ' bolle non aggiornate.'
124600130711     c                   eval      widms2=ms2wdw
124700130711     c                   eval      widms3=ms3wdw
124800130711     c                   eval      widms4=ms4wdw
124900130711   x2c                   else
125000130711     c                   eval      widmsall=wmsg
125100130711    2c                   endif
125200130711    1c                   endif
125300130711    1c                   if        wmsg=*blanks
125400130711     c                   movel     msg(5)        v1cmsg
125500130711    2c                   if        bollema>0
125600130711     c                   eval      v1cmsg=%trim(v1cmsg)+' ' +%editc(bollema:'2')
125700130711   x2c                   else
125800130711     c                   eval      v1cmsg=%trim(v1cmsg)+' '
125900130711     c                             + %editw(bollema:'       0 ')
126000130711    2c                   endif
126100130711     c                   eval      v1cmsg=%trim(v1cmsg)+ ' Bolle.'
126200130711    1c                   endif
126300130711     c                   ENDSR
126400110704     C*----------------------------------------------------------------
126500110704     c     Parzial       BEGSR
126600110704     c                   clear                   Nobolla           1
126700110704      *
126800110704      *  51 CONTROLLO IL CODICE CLIENTE
126900110704     C     BLPCCM        IFNE      *ZEROS
127000110704     C                   Z-ADD     BLPCCM        WKSC              7 0
127100110704     C                   ELSE
127200110704     C                   Z-ADD     BLPKSC        WKSC
127300110704     C                   ENDIF
127400130729    2C   51WKSC          IFNE      W1CKSC
127500110704     c                   if        blplnp=190 or blplnp=191
127600110704     c                   seton                                        80
127700110704     c                   except    det
127800110704     c                   setoff                                       80
127900110704    2C                   END
128000110704     c                   eval      NoBolla='N'
128100110704     C                   leavesr
128200110704    2C                   END
128300110704     c* Se richiesta sospensione a cavallo di anno per una lnp estera
128400110704     c* se il cliente mantiene il numero spedizione la scarto
128500110704     c                   if        not *in51 and ntwlnp='E'
128600110704     c                             and waasn<>waas
128700110704     C                   MOVEL(P)  '3C'          COD
128800110704     C                   MOVEL(p)  wksc          KEY
128900110704     C     ktab2         chain     tabel00f                           31
129000110704     C   31              clear                   ds3c
129100110704     C  N31              movel     tbluni        ds3c
129200110704     c     §3cfsp        ifeq      'S'
129300110704     c                   eval      NoBolla='N'
129400110704     C                   leavesr
129500110704     c                   endif
129600110704     c                   endif
129700110704      *
129800110704      *  53 CONTROLLO IL TERMINAL ARRIVO
129900110704    2C   53BLPTFA        IFNE      V1CTFA
130000110704     c                   if        blplnp=190 or blplnp=191
130100110704     c                   seton                                        81
130200110704     c                   except    det
130300110704     c                   setoff                                       81
130400110704    2C                   END
130500110704     c                   eval      NoBolla='N'
130600110704     C                   leavesr
130700110704    2C                   END
130800110704      *
130900110704      *  54 CONTROLLO LA LINEA DI ARRIVO
131000110704    2C   54BLPLNA        IFNE      WCLNA
131100110704     c                   if        blplnp=190 or blplnp=191
131200110704     c                   seton                                        82
131300110704     c                   except    det
131400110704     c                   setoff                                       82
131500110704    2C                   END
131600110704     c                   eval      NoBolla='N'
131700110704     C                   leavesr
131800110704    2C                   END
131900110704      *
132000110704      ** CONTROLLO CHIUSURA FOGLIO VIAGGIO DELLA BOLLA
132100110704    2C     BLPDT1        IFNE      *ZEROS
132200110704     c                   if        blplnp=190 or blplnp=191
132300110704     c                   seton                                        83
132400110704     c                   except    det
132500110704     c                   setoff                                       83
132600110704    2C                   END
132700110704     c                   eval      NoBolla='N'
132800110704     C                   leavesr
132900110704    2C                   ENDIF
133000110704     c
133100110704     c* Se richiesto, controllo se non ha spunte per tutti i colli della
133200110704     c*  sped
133300110704    1c                   if        v1cnos='S'
133400110704     c                   clear                   wsispu            1
133500110704     c     kblt          setll     fnblt01l
133600110704     c     kblt          reade     fnblt01l                               99
133700110704    2c                   dow       not *in99
133800110704     c
133900110704     c* Cerco una qualsiasi spunta
134000110704     c     kbrv          chain     fnbrv07l
134100110704    3c                   if        %found(fnbrv07l)
134200110704     c                   eval      wsispu='S'
134300110704     c                   seton                                        99
134400110704   x3c                   else
134500110704     c     kblt          reade     fnblt01l                               99
134600110704    3c                   endif
134700110704    2c                   enddo
134800110704     c
134900110704     c*Se non trovata nessuna spunta
135000110704    2c                   if        wsispu='S'
135100110704     c                   if        blplnp=190 or blplnp=191
135200110704     c                   seton                                        84
135300110704     c                   except    det
135400110704     c                   setoff                                       84
135500110704    2C                   END
135600110704     c                   eval      NoBolla='N'
135700110704     c                   leavesr
135800110704    2c                   endif
135900160205     c*
136000160205     c* Se non trovate spunte controllo se presente anomalia 20
136100160205     C                   MOVEL     020           comcaa
136200160205     c     kanm01        chain     fnanm01l
136300160205     c                   if        %found(fnanm01l) and anmdch=0
136400160205     C                   eval      NoBolla='N'
136500160205     C                   leavesr
136600160205     c                   endif
136700160205
136800110704    1c                   endif
136900110704     c*
137000110704      * SCARTO I PREPAGATI ANNULLATI
137100110704     c                   if        blpcbo='M '
137200110704     c                   eval      NoBolla='N'
137300110704     c                   leavesr
137400110704     c                   endif
137500110704      *
137600110704      ** CONTROLLO IL TIPO BOLLA
137700110704     C                   MOVEL     '3A'          COD
137800110704     C                   MOVEL(P)  BLPCBO        KEY
137900110704     C     KTAB2         CHAIN     TABEL00F                           31
138000110704     C   31              CLEAR                   DS3A
138100110704     C  N31              MOVEL     TBLUNI        DS3A
138200110704      *
138300110704      * SCARTO LE BOLLE DI RECUPERO
1384001107041   2C     §3ARBL        IFEQ      'R'
138500110704     c                   if        blplnp=190 or blplnp=191
138600110704     c                   seton                                        85
138700110704     c                   except    det
138800110704     c                   setoff                                       85
138900110704    2C                   END
139000110704     c                   eval      NoBolla='N'
139100110704     C                   leavesr
139200110704    2C                   END
139300110704      * SCARTO LE BOLLE con cod trattamento merce che non prevede merce
139400110704     C                   MOVEL     '1B'          COD
139500110704     C                   MOVEL(P)  BLPctm        KEY
139600110704     C     KTAB2         CHAIN     TABEL00F                           31
139700110704     C   31              CLEAR                   DS1b
139800110704     C  N31              MOVEL     TBLUNI        DS1b
139900110704    2C                   IF        §1bfg8='N'
140000110704     c                   if        blplnp=190 or blplnp=191
140100110704     c                   seton                                        85
140200110704     c                   except    det
140300110704     c                   setoff                                       85
140400110704    2C                   END
140500110704     c                   eval      NoBolla='N'
140600110704     C                   leavesr
140700110704    2C                   END
140800110704      *
140900110704      * PER I PREPAGATI NON È CONSENTITO SPOSTARE LA DATA BOLLA AL MESE
141000110704      *  SUCCESSIVO
141100110704     C     WAMSOS        IFGT      WAMSPE
141200110704      *
141300110704    2C     BLPPDR        IFEQ      *ZEROS
141400110704     c                   if        blplnp=190 or blplnp=191
141500110704     c                   seton                                        86
141600110704     c                   except    det
141700110704     c                   setoff                                       86
141800110704    2C                   END
141900110704     c                   eval      NoBolla='N'
142000110704     C                   leavesr
142100110704    2C                   ELSE
142200110704     C                   MOVEL     '1'           KTRC
142300110704     C     KAR6          CHAIN(N)  FIAR6000                           31
142400110704    2C     *IN31         IFEQ      '0'
142500110704    2C     AR6NFT        ANDGT     *ZEROS
142600110704     c                   if        blplnp=190 or blplnp=191
142700110704     c                   seton                                        87
142800110704     c                   except    det
142900110704     c                   setoff                                       87
143000110704    2C                   END
143100110704     c                   eval      NoBolla='N'
143200110704     C                   leavesr
143300110704    2C                   ENDIF
143400110704    2C                   ENDIF
143500110704      *
143600110704    2C                   ENDIF
143700110704     c
143800110704     c                   ENDSR
143900110704     c*------------------------------------------------------------------------
144000110704     c     Aggiorna      BEGSR
144100110704     c* A questo punto visto che sono stati superati tutti i controlli
144200110704     c* chaino il record bolla in update
144300110704     c     kblt          chain(E)  fnblp01l
144400110704    1c                   if        %error
144500110704     C                   CLEAR                   TRUL82DS
144600110705     c                   if        sel4=0
144700110704     c                   eval      ul82§rrn = fnblpnrri
144800110705     c                   else
144900110705     c                   eval      ul82§rrn = fnblpnrr01
145000110705     c                   endif
145100110704     C                   EVAL      UL82§FIL = 'FNBLP00F'
145200110704     C                   EVAL      UL82§WIN = 'N'
145300110704     C                   EVAL      UL82§F7  = 'S'
145400110704     C                   EVAL      UL82§num = 1
145500110704     C                   EVAL      UL82§att = 30
145600110704     C                   EVAL      UL82§mss = msgjob
145700110704     C* Effettuo la chiamata al *PGM d utilità
145800110704     C                   CALL(e)   'TRUL82R'
145900110704     C                   PARM                    TRUL82DS
146000110704     C     kblt          chain     fnblp01l
146100110704    1c                   endif
146200110704    2c                   if        %found(fnblp01l)
146300111214     c
146400111214     c* Mi salvo il terminal della bolla
146500111214     c                   eval      savtfp=blptfp
146600110704      *
146700110704      ** CONTROLLO SE DATA BORDERO' MINORE DELLA NUOVA DATA DI SPEDIZIONE
146800110704      *  SBORDERIZZO
146900110704    2C     BLPDBR        IFLT      WLPDSP
147000110704      *
147100110704      * SBORDERIZZO
147200110704     C                   CLEAR                   BLPNFV
147300110704     C                   CLEAR                   BLPDBR
147400110704     C                   CLEAR                   BLPFLP
147500110704    2C                   ENDIF
147600110704     C* AGGIORNO LA NUOVA DATA SPEDIZIONE
147700110704     C                   Z-ADD     WAASN         BLPAAS
147800110704     C                   Z-ADD     WLPDSP        BLPMGS
147900110704     c* Aggiorno anche la data ritiro merce
148000110704     c*  solo per i clienti che spostanto in sospensione per mancanza di spu
148100110704     c*  nte
148200110704    2c                   if        v1cnos='S'
148300110704     C                   movel     WLPDSP        BLPdrt
148400110704     c* Se richiesto e se presente consegna richiesta, imposto la nuova data
148500110704    3c                   if        *in03 and v1cdcr>0 and blpdcr>0
148600110704     c                   movel     v1ctcr        blptcr
148700110704     c                   movel     wdcr          blpdcr
148800110704     c                   movel     v1chcr        blphcr
148900110704    3c                   endif
149000110704     c
149100110704   x2c                   else
149200110704     c* Se la data ritiro > della data spedizione nuova e non c'e' ORM
149300110704     c*  imposto SEMPRE la data ritiro= data spedizione
149400110704    3c                   if        blpdrt>wlpdsp
149500110704     c     kar4m         chain     fiar401l
149600110704    4c                   if        not %found(fiar401l)
149700110704     c                   eval      blpdrt=wlpdsp
149800110704    4c                   endif
149900110704    3c                   endif
150000110704     c
150100110704    2c                   endif
150200110704     c
150300110704     C* E RICACOLO I TERMINAL DI ARRIVO E FILIALE ELABORAZIONE ARRIVI
150400110704     C                   CLEAR                   DSLV55
150500110704     C                   MOVEL     ' '           D55TPT
150600110704     C                   MOVEL     BLPLNA        D55LIN
150700110704     C                   MOVEL     BLPLNP        D55LNP
150800110704     C                   MOVEL     WLPDSP        D55DRF
150900110704     C                   CALL      'FNLV55R'
151000110704     C                   PARM                    DSLV55
151100110704    2C     D55ERR        IFEQ      ' '
151200110704     C                   MOVEL     D55TFA        BLPTFA
151300110704     C                   MOVEL     D55TFP        BLPFEA
151400110704    2C                   ENDIF
151500110704     C* BLPTFP: TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
151600110704     C                   CLEAR                   DSLV55
151700110704     C                   MOVEL     'P'           D55TPT
151800110704     C                   MOVEL     BLPLNP        D55LIN
151900110704     C                   MOVEL     WLPDSP        D55DRF
152000110704     C                   CALL      'FNLV55R'
152100110704     C                   PARM                    DSLV55
152200111214     c
152300111214     c
152400110704    2C     D55ERR        IFEQ      ' '
152500110704     C                   MOVEL     D55TFP        BLPTFP
152600111128     C                   MOVEL     D55TFP        BLPfle
152700110704    2C                   ENDIF
152800110704     C*
152900110704     C                   UPDATE    FNBLP001
153000110704     c                   add       1             bollema           9 0
153100110704     c
153200110704     c* Se si tratta di assegnato S.F. tassato in partenza, cancello la
153300110704     c* tassazione
153400110704     c                   clear                   ktrc
153500110704    2c                   if        §3atb2<>*blanks
153600110704     c                   eval      ktrc='2'
153700110704   x2c                   else
153800110704     c                   movel     §3atb1        flgtb1            1
153900110704    3c                   if        flgtb1='A'
154000110704     c                   eval      ktrc='1'
154100110704    3c                   endif
154200110704    2c                   endif
154300110704     c
154400110704   1ac                   if        ktrc<>*blanks
154500110704     C     KAR6          CHAIN(N)  FIAR6000                           31
154600110704    2c                   if        not *in31
154700110704     c                   move      ar6ksc        fineksc           4 0
154800110704    3c                   if        fineksc<>0000 and fineksc<>9999
154900110704     c* deleto la tassazione
155000110704     c     kar6          setll     fiar701l
155100110704     c     kar6          reade     fiar701l
155200110704    4c                   dow       not %eof(fiar701l)
155300110704     c                   delete    fiar7000
155400110704     c
155500110704     c     kar6          reade     fiar701l
155600110704    4c                   enddo
155700110704     c
155800110704     c     kar6          delete    fiar6000
155900110704    3c                   endif
156000110704    2c                   endif
156100110704   1ac                   endif
156200110704     c
156300110704     C*
156400110704     C     KBLT          SETLL     FNBLT01L
156500110704     C     KBLT          READE     FNBLT01L                               30
156600121107    2C     *IN30         DOWEQ     *OFF
156700110704     C* SOLO SE NON C'E' GIA' LA DATA DI PARTENZA
156800110704    3C  N30BLTDFV        IFEQ      0
156900110704     C* E LA DATA BORDERO' DELLA TESTATA NON E' MAGGIORE ALLA NUOVA DATA SPED.
157000110704    2C     BLPDBR        ANDLT     WLPDSP
157100110704     C                   CLEAR                   BLTNFV
157200110704     C                   UPDATE    FNBLT000
157300110704   X3C                   ELSE
157400110704     C                   UNLOCK    FNBLT01L
157500110704    3C                   ENDIF
157600121107     C     KBLT          READE     FNBLT01L                               30
157700110704    2C                   ENDDO
157800111214     c
157900110704     C* AGGIORNO ANCHE FISGN SE PRESENTE
158000110704     C     KFISGN        SETLL     FISGN05L
158100110704     C     KFISGN        READE     FISGN05L                               30
158200110704    2C     *IN30         DOWEQ     *OFF
158300110704     C                   Z-ADD     WLPDSP        SGNMGS
158400110704     C                   Z-ADD     BLPTFA        SGNTNA
158500111214     c
158600111214     c* Cambio il terminal solo se corrisponde a quello della bolla
158700111214     c*  se l'ho inviato ad aaltro terminal non modifico
158800130214     c**                 if        sgntnp=savtfp
158900130214     C**                 Z-ADD     BLPTFP        SGNTNP
159000130214     c**                 endif
159100111214     c
159200110704     C                   CLEAR                   SGNT6A
159300110704     C                   CLEAR                   SGNT6B
159400110704     C                   CLEAR                   SGNT6C
159500110704     C                   CLEAR                   SGNT6D
159600110704     C                   CLEAR                   SGNT6E
159700110704     C                   CLEAR                   SGNT6F
159800110704     c                   eval      sgndatora = %char(%timestamp:*iso0)
159900110704     C                   UPDATE    FISGN000
160000110704     C     KFISGN        READE     FISGN05L                               30
160100110704    2C                   ENDDO
160200110704     C*
160300110704     c* richiamo routine per gestione stampe
160400110704     c                   z-add     blpaas        wlpaas
160500110704     c                   z-add     blpnsp        wlpnsp
160600110704     c                   exsr      sr_stampe
160700110704     c*
160800110704    1c                   endif
160900110704     c
161000110704     c                   ENDSR
161100980130     C*----------------------------------------------------------------
161200980130     C*
161300980130     C*--- OPERAZIONI INIZIALI ---------------------------------------*
161400980130     C     *INZSR        BEGSR
161500980130     C***
161600980130     C* KLIST
161700980130     C***
161800980130     C* ACCESSO   CNACO
161900980130     C     KACO          KLIST
162000980130     C                   KFLD                    CODUT
162100980130     C                   KFLD                    CCC
162200130729     C                   KFLD                    W1CKSC
162300151207     C**   Ktbe          KLIST
162400151207     C**                 KFLD                    kcod
162500151207     C***                KFLD                    klin
162600151207     C***                KFLD                    ksif
162700110704     C*
162800980130     C     KBLT          KLIST
162900980130     C                   KFLD                    BLPAAS
163000980130     C                   KFLD                    BLPLNP
163100980130     C                   KFLD                    BLPNRS
163200980130     C                   KFLD                    BLPNSP
163300110704     C* KEY PER AGGIIORNARE FLBLT
163400110704     C     KBLPM         KLIST
163500110704     C                   KFLD                    ar4AAS
163600110704     C                   KFLD                    ar4LNP
163700110704     C                   KFLD                    ar4NRS
163800110704     C                   KFLD                    ar4NSP
163900110704     C* KEY PER AGGIORNARE fiar4
164000080331     C     Kar4M         KLIST
164100080331     C                   KFLD                    BLPAAS
164200080331     C                   KFLD                    BLPLNP
164300080331     C                   KFLD                    BLPNRS
164400080331     C                   KFLD                    BLPNSP
164500110704     C                   KFLD                    ktrcM
164600051209     C     KBrv          KLIST
164700051209     C                   KFLD                    bltfls
164800051209     C                   KFLD                    bltlna
164900051209     C                   KFLD                    bltNRS
165000051209     C                   KFLD                    bltnsc
165100001120     C* KEY PER AGGANCIARE  FIAR6
165200001120     C     KAR6          KLIST
165300001120     C                   KFLD                    BLPAAS
165400001120     C                   KFLD                    BLPLNP
165500001120     C                   KFLD                    BLPNRS
165600001120     C                   KFLD                    BLPNSP
165700001120     C                   KFLD                    KTRC              1
165800980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
165900980130     C     KBLP1         KLIST
166000020610     c                   kfld                    lnp
166100980130     C                   KFLD                    WAAS
166200980130     C                   KFLD                    WMGS
166300980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 2
166400980130     C     KBLP2         KLIST
166500020610     c                   kfld                    lnp
166600980130     C                   KFLD                    WAAS
166700980130     C                   KFLD                    WMGS
166800980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 3
166900980130     C     KBLP3         KLIST
167000020610     c                   kfld                    lnp
167100980130     C                   KFLD                    WAAS
167200980130     C                   KFLD                    WMGS
167300980130     C                   KFLD                    WCPDR
167400010326     C* ACCESSO   FILE FISGN PER NUMERO SPEDIZIONE
167500010326     C     KFISGN        KLIST
167600010326     C                   KFLD                    BLPAAS
167700010326     C                   KFLD                    BLPLNP
167800010326     C                   KFLD                    BLPNRS
167900010326     C                   KFLD                    BLPNSP
168000980130     C* ACCESSO   TABEL
168100980130     C     KTAB2         KLIST
168200980130     C                   KFLD                    CODUT
168300980130     C                   KFLD                    COD               2
168400980130     C                   KFLD                    KEY               8
168500980202     C* ACCESSO   AZCLN
168600980202     C     KCLN          KLIST
168700980202     C                   KFLD                    SIMFEL
168800980202     C                   KFLD                    W0030             3 0
168900980202     C                   KFLD                    WAA               4 0
169000980202     C                   KFLD                    WMM               2 0
169100021203     C     Kfapd         klist
169200021203     C                   kfld                    kpdtip
169300021203     C                   kfld                    kpdpdr
169400110704      *
169500110704     C     KFORM         KLIST
169600110704     C                   KFLD                    V1CPOE
169700110704     C                   KFLD                    V1CNSR
169800110704     C                   KFLD                    V1CNOR
169900110704     C                   KFLD                    V1CNRV
170000110704      *
170100110704     C     Kar43         KLIST
170200110704     C                   KFLD                    KTRCM
170300110704     C                   KFLD                    KN14             14
170400160205
170500160205     C     Kanm01        KLIST
170600160205     C                   KFLD                    BLPAAS
170700160205     C                   KFLD                    BLPLNP
170800160205     C                   KFLD                    BLPNRS
170900160205     C                   KFLD                    BLPNSP
171000160205     c                   kfld                    comcaa
171100110704     C*
171200980130     C***
171300980130     C* DEFINIZIONE CAMPI
171400980130     C***
171500980130     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
171600980130     C***
171700130729     c     *dtaara       define    §azute        AZUTEds
171800130729     c     *dtaara       define    §datiute      dDATIUTE
171900130729     c                   in(E)     *dtaara
172000130729     c                   if        %ERROR or RSUT = *blanks
172100130729     c                   clear                   Tibs34Ds
172200130729     c                   call      'TIBS34R'
172300130729     c                   parm                    Tibs34Ds
172400130729     c                   in        *dtaara
172500130729     c                   endif
172600130729      *
172700130729     c                   z-add     1             CodUt             1 0
172800980202     C***
172900980202     C* CARICO DATI VARIABILI PARTENZE
173000980202     C***
173100980202     C                   MOVEL     '3I'          COD
173200980202     C                   MOVEL     '1       '    KEY
173300980202     C     KTAB2         CHAIN     TABEL00F                           31
173400980202     C  N31              MOVEL     TBLUNI        DS3I
173500980202     C   31              CLEAR                   DS3I
173600980202     C***
173700980202     C* CARICO CODICI CLIENTI SDI PER I QUALI INSERISCO OBBLIGATORIAM
173800980202     C*  NUMERO AWB CON CONTROLLO CHECK DIGIT
173900980202     C***
174000980202     C                   MOVEL     '3I'          COD
174100980202     C                   MOVEL     'E       '    KEY
174200980202     C     KTAB2         CHAIN     TABEL00F                           31
174300980202     C  N31              MOVEA     TBLUNI        KSS
174400980202     C* 2 CIFRE INIZIALI FISSE PER NUMERO AWB
174500980202     C  N31              MOVE      TBLUNI        §3IFAW            2 0
174600980202     C   31              CLEAR                   KSS
174700980202     C   31              CLEAR                   §3IFAW
174800980130     C*
174900980130     C* DATA BOLLETTAZIONE:  TRA UDATE E DATA GIORNO + §3IGGS
175000980130     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
175100980130     C                   TIME                    W0140            14 0
175200980130     C* UDATE IN GGMMAAAA
175300980130     C                   MOVE      W0140         WDTGIO            8 0
175400060404     C                   MOVEl     W0140         utime
175500980130     C*
175600980130     C* UDATE IN AAAAMMGG
175700980130     C                   Z-ADD     WDTGIO        G02DAT
175800980130     C                   MOVEL     *BLANK        G02ERR
175900980130     C                   CALL      'XSRDA8'
176000980130     C                   PARM                    WLBDAT
176100980130     C                   MOVEL     G02INV        DATEU             8 0
176200980130     C* SE NON FOSSE PERMESSO BOLLETTARE IN SOSPENSIONE
176300980130     C*  SI PUO' BOLLETTARE SOLO FINO A OGGI
176400980130     C                   Z-ADD     G02INV        DATAPA            8 0
176500980130     C                   MOVEL     G02DAT        DATAPG            8
176600980130     C*
176700980130     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
176800130729     C                   Z-ADD     1             X                 2 0
176900980130    1C     X             DOWLE     §3IGGS
177000980130     C*
177100980130     C                   MOVEL     'F'           WFEST             1
177200980130     C*
177300980130    2C     WFEST         DOWNE     ' '
177400980130     C     G02TGI        ADD       1             GIOTGI
177500980130     C                   ADD       1             G02TGI
177600980130     C                   CALL      'XSRGI8'
177700980130     C                   PARM                    WGIDAT
177800980130     C*
177900980130     C                   Z-ADD     GIOINV        DATAPA            8 0
178000980130     C                   MOVEL     GIODAT        DATAPG            8
178100980130     C*
178200980130     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
178300980130     C                   Z-ADD     GIOINV        WDATA
178400980130     C                   CLEAR                   W0030
178500120302     C* CHAIN SUL CALENDARIO BRT
178600980130     C     KCLN          CHAIN     AZCLN01L                           30
178700980130     C   30              CLEAR                   CLNPOM
178800980130     C                   MOVEA     CLNPOM        K31
178900980130     C                   MOVEL     K31(WGG)      WFEST
179000980130    2C                   ENDDO
179100980130     C*
179200980130     C                   ADD       1             X
179300980130    2C                   ENDDO
179400070612     C* reperisco da tabella vpo il numero giorni da sommare a udate per
179500070612     c* determinare data max per data consegna richiesta
179600070612     C                   CLEAR                   tibs02ds
179700070612     C                   MOVEL     'C'           T02MOD
179800070612     C                   MOVEL     KNSIF         T02SIF
179900070612     C                   MOVEL     'VPO'         T02COD
180000070612     C                   MOVEL(p)  'VPO'         T02KE1
180100070612     C                   CALL      'TIBS02R'
180200070612     C                   PARM                    KPJBA
180300070612     C                   PARM                    tibs02ds
180400070612    4C     T02ERR        IFEQ      *BLANKS
180500070612     C                   MOVEL     T02UNI        DVPO
180600070612     C                   ELSE
180700070612     C                   CLEAR                   DVPO
180800070612    4C                   ENDIF
180900130327     c*
181000070612     c                   Move      Dateu         Dataiso
181100070612     c                   Adddur    §vpodcgm1:*d  Dataiso
181200070612     c                   Move      Dataiso       woggidcgm1
181300070612     c*
181400070612     c                   Move      Dateu         Dataiso
181500070612     c                   Adddur    §vpodcgm2:*d  Dataiso
181600070612     c                   Move      Dataiso       woggidcgm2
181700130327      *
181800980130     C***
181900980130     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
182000980130     C***
182100980130     C                   CLEAR                   DSUL06
182200980130     C                   MOVE      '£1'          D06COD
182300980130     C                   MOVEL     SIMFEL        D06KEY
182400980130     C                   MOVEL     'L'           D06TLA
182500980130     C                   MOVEL     DSUL06        KPJBU
182600980130     C*
182700980130     C                   CALL      'TRUL06R'
182800980130     C                   PARM                    KPJBA
182900980130     C                   MOVEL     KPJBU         DSUL06
183000980130     C                   MOVEA     LIN           L1
183100130327     c*
183200090126     c* Verifico se il £1 c'e' almeno un p.o. estero
183300151207     c***                clear                   poestero          1
183400151207     c***                z-add     1             x
183500151207     c***                dow       L1(x)<>0
183600151207     c***  l1(x)         chain     azorg01l
183700151207     c***                if        %found(azorg01l)
183800151207     c***                movel     orgde3        og143
183900151207     c***                if        §ogntw='DPD' or §ogntw='EEX' or
184000151207     c***                          §ogntw='FED'
184100151207     c***                eval      poestero='S'
184200151207     c***                leave
184300151207     c***                endif
184400151207     c***                endif
184500151207     c***                add       1             x
184600151207     c***                enddo
184700980130     C***
184800020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
184900980130     C***
185000130729     C                   Z-ADD     dutPOU        CDU               3 0
185100130729     C     DUTlpo        IFEQ      '2'
185200130729     C     DUTlpo        OREQ      *BLANKS
185300020429     C                   SETON                                        12
185400020429     C                   ENDIF
185500130327     c*
185600111018     c                   clear                   elabor
185700130327     c*
185800111018     c                   Clear                   tibs02ds
185900111018     c                   Clear                   dmradan
186000111018     c                   Movel     'C'           T02Mod
186100111018     c                   Movel     KNSIF         T02Sif
186200111018     c                   Movel     'MRA'         T02Cod
186300111018     c                   Movel     'FNLS03R'     T02Ke1
186400111018     c                   Call      'TIBS02R'
186500111018     c                   Parm                    Kpjba
186600111018     c                   Parm                    tibs02ds
186700111018    2c                   If        T02Err = *Blanks
186800111018     c                   Movel     T02Uni        Dmradan
186900111018    2c                   endif
187000001120     C*
187100980130     C                   ENDSR
187200980130     C*****************************************************************
187300980130     C* PULIZIA   FORMATO -------------------------------------------
187400980130     C*****************************************************************
187500980130     C     PUL01         BEGSR
187600980130     C*
187700070614     c                   setoff                                       0103
187800130729     C                   MOVE      *ZEROS        W1CKSC
187900130729     C                   clear                   V1CKSC
188000980130     C                   MOVE      *BLANKS       V1DKSC
188100980130     C                   MOVE      *BLANKS       V1CLNP
188200980130     C                   MOVE      *BLANKS       V1DLNP
188300980130     C                   MOVE      *ZEROS        V1CTFA
188400980130     C                   MOVE      *BLANKS       V1DTFA
188500980130     C                   MOVE      *BLANKS       V1CLNA
188600980130     C                   MOVE      *BLANKS       V1DLNA
188700980130     C                   MOVE      *BLANKS       V1CPDR
188800980130     C                   MOVE      *BLANKS       V1DPDR
188900980130     C                   Z-ADD     *ZEROS        V10DSP
189000980130     C                   Z-ADD     *ZEROS        V11DSP
189100070612     C                   CLEAR                   v1cdcr
189200070612     C                   CLEAR                   v1ctcr
189300070612     C                   CLEAR                   v1chcr
189400070612     C                   CLEAR                   wdcr
189500051209     C                   move      *blanks       V1cnos
189600160405     C                   z-add     §3igga        V1cggl
189700060713     c                   clear                   bollemsg
189800060713     c                   clear                   bollema           9 0
189900110704     c                   clear                   v1cpoe
190000110704     c                   clear                   v1cnsr
190100110704     c                   clear                   v1cnor
190200110704     c                   clear                   v1cnrv
190300111017     c                   clear                   savdim            8 0
190400151207
190500151207     c* Abilitazione sospensione per colli non spuntati
190600151207
190700151207     c     ' '           setll     fibsp03l
190800151207     c     ' '           reade     fibsp03l
190900151207     c                   dow       not %eof(fibsp03l) and *in01=*off
191000151207     c* record per lnp
191100151207     c                   if        bspfil>0
191200151207     c                   if        %lookup(bspfil:l1)>0
191300151207     c                   eval      SosdaL1='S'
191400151207     c                   seton                                        01
191500151207     c                   endif
191600151207     c                   else
191700151207     c* record per cliente
191800151207     c                   z-add     1             Ix
191900151207     c                   dow       Ix<=%elem(sbsp) and sbsp(Ix)>0
192000151207     c                             and *in01 = *off
192100151207     c                   if        %lookup(sbsp(Ix):L1)>0
192200151207     c                   eval      SosdaL1='S'
192300151207     c                   seton                                        01
192400151207     c                   endif
192500151207     c                   add       1             Ix
192600151207     c                   enddo
192700151207     c                   endif
192800151207     c     ' '           reade     fibsp03l
192900151207     c                   enddo
193000090126     c* Abilito l'opzione se c'e' un p.o. estero in £1
193100151207     c***                if        poestero='S'
193200151207     c***                seton                                        01
193300151207     c***                eval      SosdaL1='S'
193400151207     c****               else
193500130327     c*
193600070903     c* Abilito l'opzione solo se almeno un cliente è abilitato
193700151207     c***                eval      SosdaL1=' '
193800151207     c***                eval      ksif=knsif
193900151207     c***  ktbe          setll     tntbe02l
194000151207     c***                if        not %equal(tntbe02l)
194100151207     c***                clear                   ksif
194200151207     c***  ktbe          setll     tntbe02l
194300151207     c***                endif
194400130327     c*
194500151207    1c***                if        %equal(tntbe02l)
194600151207     c***  ktbe          reade     tntbe02l
194700151207   1ac***                dow       not %eof(tntbe02l) and not *in01
194800151207    2c***                if        tbeatb=' '
194900151207     c***                movel     tbeuni        dbsp
195000151207     c***                movel     tbeke1        lnpabi            3 0
195100130327     c*
195200151207     c***                if        lnpabi>0
195300151207     c***  lnpabi        lookup    l1                                     30
195400151207    5c***                if        *in30
195500151207     c***                eval      SosdaL1='S'
195600151207     c***                seton                                        01
195700151207    5c***                endif
195800151207     c***                endif
195900151207    2c***                endif
196000130327     c*
196100151207     c***  ktbe          reade     tntbe02l
196200151207   1ac***                enddo
196300151207    1c***                endif
196400130327     c*
196500151207    1c***                endif
196600130327     c*
196700080624     c*********          if        not *in01 and (cdu=118 or cdu=107)
196800080624     c*********          eval      *in01=*on
196900080624     c*********          endif
197000060403     c*
197100060403     c* se 01 acceso effettuo ovrprtf per mettere le stampe in save
197200060403     c                   if        *in01
197300060403     c                   clear                   qcmd
197400060403     c                   eval      qcmd = $cmd2(1)
197500060403     c                   call      'QCMDEXC'                            91
197600060403     c                   parm                    Qcmd
197700060403     c                   parm                    Qlen
197800060403     c                   endif
197900130327     c*
198000061005     c                   clear                   qcmd
198100061005     c                   eval      qcmd = $cmd3(1)
198200061005     c                   call      'QCMDEXC'                            91
198300061005     c                   parm                    Qcmd
198400061005     c                   parm                    Qlen
198500980130     C*
198600980130     C                   ENDSR
198700030619
198800130327     *** *---------------------------------------------------------------*
198900130327     *** * Richiedo i dati per la stampa   ed   Eseguo le override       *
199000130327     *** *---------------------------------------------------------------*
199100130327     ***c     SR_OvrPrtF    BEGSR
199200130327     *** *
199300130327     ***c                   call      'TRUL90R'
199400130327     ***c                   parm                    KPJBA
199500130327     ***c                   parm                    TRUL90DS
199600130327     *** * - F3 > Fine
199700130327     ***c     D90F3         cabeq     *on           End_Ovr
199800130327     *** *
199900130327     *** * - FNLSB5PA4 > bolle laser in formato A4
200000130327     ***c                   eval      wFILE = 'FNLSB5PA4 '
200100130327     ***c                   eval      wOUTQ = D90prb4
200200130327     ***c                   eval      wFORM = D90mdb4
200300130327     ***c                   exsr      SR_x_Cmd
200400130327     ***c     *in91         cabeq     *on           End_Ovr
200500130327     *** *
200600130327     *** * - FNLSB5PA5 > bolle laser in formato A5
200700130327     ***c                   eval      wFILE = 'FNLSB5PA5 '
200800130327     ***c                   eval      wOUTQ = D90prb5
200900130327     ***c                   eval      wFORM = D90mdb5
201000130327     ***c                   exsr      SR_x_Cmd
201100130327     ***c     *in91         cabeq     *on           End_Ovr
201200130327     *** *
201300130327     ***c     End_Ovr       ENDSR
201400130327     ***
201500130327     *** *---------------------------------------------------------------*
201600130327     ***
201700130327     ***c     SR_x_Cmd      BEGSR
201800130327     *** *
201900130327     *** * impostazione comando da eseguire
202000130327     ***c                   eval      Qcmd  = %trim($Cmd(1)) + ' '
202100130327     ***c                                   + %trim($Cmd(2))
202200130327     *** * - nome del file
202300130327     ***c                   eval      Qcmd = %replace(wFILE:Qcmd:
202400130327     ***c                                    %scan('&FILE':Qcmd))
202500130327     *** * - nome della coda di stampa
202600130327     ***c                   eval      Qcmd = %replace(wOUTQ:Qcmd:
202700130327     ***c                                    %scan('&OUTQ':Qcmd))
202800130327     *** * - tipo di modulo
202900130327     ***c                   eval      Qcmd = %replace(wFORM:Qcmd:
203000130327     ***c                                    %scan('&FORMTYPE':Qcmd))
203100130327     *** *
203200130327     ***c                   call      'QCMDEXC'                            91
203300130327     ***c                   parm                    Qcmd
203400130327     ***c                   parm                    Qlen
203500130327     *** *
203600130327     ***c                   ENDSR
203700130327
203800081203      *---------------------------------------------------------------*
203900081203
204000081203     c     SR_stampe     BEGSR
204100130327      *
204200130327     ***C* PER CMD8 STAMPO LA BOLLA
204300130327     ***C     *INKH         IFEQ      *ON
204400130327     ***C     BLPFST        ANDNE     'N'
204500130327     ***c                   clear                   FNLSB5DS
204600130327     ***C                   MOVEL     'V'           DB0RIS
204700130327     ***C                   MOVEL     'P'           DB0TBO
204800130327     ***C                   Z-ADD     wLPAAS        DB0AAS
204900130327     ***C                   Z-ADD     BLPLNP        DB0LNP
205000130327     ***C                   Z-ADD     BLPNRS        DB0NRS
205100130327     ***C                   Z-ADD     wLPNSP        DB0NSP
205200130327     ***c                   call      D90psl
205300130327     ***c                   parm                    FNLSB5DS
205400130327     ***c                   parm                    FNLSB6ds1
205500130327     ***C                   ENDIF
205600081203     c*
205700081203     c* GESTIONE STAMPA SPEDIZIONE
205800081203     c                   if        v1cnos='S'
205900081203     c                   if        v1cggl > 0
206000081203     c* calcolo giorni lavorativi fra data spedizione e data immissione
206100081203     c                   clear                   wdat8
206200081203     c                   z-add     blpdim        datada
206300081203     c                   z-add     wlpdsp        dataa
206400081203     c                   call      'XSRLAV8'
206500081203     c                   parm                    wdat8
206600081203     c                   if        (ggl-1) > v1cggl
206700081203     c                   if        blpfns = 'S'
206800081203     c                   seton                                        02
206900081203     c                   else
207000081203     c                   setoff                                       02
207100081203     c                   endif
207200081203     c* inverto data ritiro per stampa
207300081203     c     *iso          move      blpdim        data_eur
207400081203     c                   move      data_eur      p1cdim
207500081203     c   98              write     testa
207600081203     c                   setoff                                       98
207700081203     c                   write     rigsped
207800081203     c                   endif
207900081203     c                   endif
208000081203     C**
208100081203     c** Se passati xx giorni di sospensione, invio msg al CED
208200081203     c                   if        (ggl-1) > §3iggm
208300081203     c                   add       1             bollemsg          9 0
208400111017     c* memorizzo la data più bassa di immissione
208500111017     c                   if        savdim=0 or blpdim<savdim
208600111017     c                   eval      savdim=blpdim
208700111017     c                   endif
208800111017     c
208900081203     c                   endif
209000081203     c                   endif
209100081203     c                   endsr
209200081203      *---------------------------------------------------------------*
209300081203
209400081203     c     SR_nuovonumeroBEGSR
209500081203     c                   clear                   fnls65ds
209600081203     c                   z-add     waas          I65AAS
209700081203     c                   z-add     blplnp        I65LNP
209800081203     c                   z-add     blpnrs        I65NRS
209900081203     c                   z-add     blpnsp        I65NSP
210000081203     c                   z-add     waasn         I65AASN
210100081203     c                   z-add     wlpdsp        I65MGSN
210200081203     c                   if        v1cnos='S'
210300081203     c                   movel     'S'           I65AGDRT
210400081203     c                   if        *in03 and v1cdcr>0 and blpdcr>0
210500081203     c                   movel     'S'           I65AGDCR
210600081203     c                   move      v1ctcr        I65TCRN
210700081203     c                   move      wdcr          I65DCRN
210800081203     c                   move      v1chcr        I65HCRN
210900081203     c                   endif
211000081203     c                   endif
211100081203     c                   move      '1'           I65STRCMT
211200081203     c                   move      '1'           I65FCMT
211300081203     c                   move      '1'           I65CMTRLBK
211400081203     c                   movel     fnls65ds      kpjbu
211500081210     c                   call      'FNLS65R'
211600081203     c                   parm                    kpjba
211700081203     c                   movel     kpjbu         fnls65ds
211800081203     c                   if        o65err=*blanks
211900081203     c                   add       1             bollema           9 0
212000081205     c                   z-add     waasn         wlpaas
212100081205     c                   z-add     o65nsp        wlpnsp
212200081203     c                   exsr      sr_stampe
212300081204     c                   else
212400081205     c                   add       1             werror            6 0
212500081210     c                   if        o65err='1'
212600081210     c                   movel     o65msg        wmsg
212700081210     c                   endif
212800081203     c                   endif
212900081203     c                   endsr
213000111018     c*-------------------------------------------------------------------
213100111018     c     INV_email     BEGSR
213200111018     c
213300111018     c                   if        not %open(prtemail)
213400111018     C                   EXSR      SR_OPENPRTF
213500111018     C                   endif
213600111018     c
213700111018     c                   exsr      Routend
213800111018     c
213900111018     c                   ENDSR
214000111018
214100111018      /free
214200111018       //--------------------------------------------------------------
214300111018       //?Override al file di stampa per impostarvi i dati per
214400111018       //?  l'invio via e-mail   +   stampa inizio e-mail
214500111018       //--------------------------------------------------------------
214600111018       BEGSR sr_OpenPrtF;
214700111018
214800111018         // Override al file di stampa per impostarvi i dati per
214900111018         // l'invio via e-mail
215000111018           exsr sr_Override;
215100111018
215200111018           open PrtEMAIL;
215300111018
215400111018         // Stampa una testata se NON è richiesta la e-mail
215500111018           IF  §MRAdreg =  *blank;
215600111018             O_testo = JobUser + ' - ' + SDSpgm
215700111018                     + ' - ' + %editc( *date : 'Y' )
215800111018                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
215900111018             except PrtDet;
216000111018             clear O_testo;
216100111018             except PrtDet;
216200111018             except PrtDet;
216300111018           ENDIF;
216400111018
216500111018         // Stampa testo iniziale
216600111018           O_testo = 'E'' stata manutenzionata la DATA SPEDIZIONE +
216700111019                      di bolle immesse da almeno xx giorni.' ;
216800111019           %subst(O_testo:71:2) = (%editc(§3iggm: '4'))     ;
216900111018           except PrtDet;
217000111019           O_testo = 'Verificare col mittente o procedere alla lor+
217100111019                      o chiusura come "Merce MAI Affidata".' ;
217200111019           except PrtDet;
217300111018
217400111018         // Stampa una riga vuota
217500111018           clear O_testo;
217600111018           except PrtDet;
217700111018
217800111018         // Stampa intestazione
217900111019           O_testo = ' Bolle spostate in sospensione di          Data imm+
218000111019                     issione    Bolle da vericare elencate';
218100111018           except PrtDet;
218200111019           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
218300111019                     chia       in apposita stampa.Utente:';
218400111019           %subst(o_testo:90:10)= knmus     ;
218500111019           except PrtDet;
218600111018
218700111018         // Stampa una riga vuota
218800111018           clear O_testo;
218900111018           except PrtDet;
219000111019           zz = 1 ;
219100111019
219200111019          dow     invbo(zz)>0   ;
219300111019          dataiso=%date(invdim(zz))  ;
219400111019          data_eur=dataiso  ;
219500111019
219600111019           clear O_testo;
219700111019           %subst(O_testo:6:20) = invcod(zz)  ;
219800111019           %subst(O_testo:46:10) =%char(data_eur)  ;
219900111019           %subst(O_testo:64:7 ) =%editc(invbo(zz):'2')  ;
220000111019           except PrtDet;
220100111019
220200111019           zz=zz+1  ;
220300111019           enddo    ;
220400111018
220500111018       ENDSR;
220600111018
220700111018       //--------------------------------------------------------------
220800111018       //?Override al file di stampa per impostarvi i dati per
220900111018       //?  l'invio via e-mail   +   stampa inizio e-mail
221000111018       //--------------------------------------------------------------
221100111018       BEGSR sr_Override;
221200111018
221300111018         reset TRTCM1ds;
221400111018
221500111018         IF  §MRAdreg <> *blank;
221600111018           §CM1mitt = %trim(§MRAdmitt);
221700111018           §CM1dst  = %trim(§MRAddest);
221800111019
221900111019         // Valorizzo i campi della e-m@ail
222000130729          chain  DUTpou   azorg01l ;
222100111019
222200111019          if   %found(azorg01l) ;
222300140205           og140=orgde0  ;
222400140205           if §ogapo>*zeros  ;
222500140205           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
222600140205           else  ;
222700140205            §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
222800111019          endif  ;
222900140205          endif  ;
223000111019
223100111018           §CM1tips = §MRAdreg;
223200111018           §CM1po   = %editc(simfel:'X');
223300111019           §CM1var  = '*OBJM*' + 'Manutenzione DATA SPEDIZIONE bolle'   ;
223400111018           §CM1idp  = §MRAdidpro;
223500111018           Qcmd = C_CmdOvrPrtF
223600111018                + ' outq(' + %trim(§MRAdoutqi) + ')'
223700111018                + ' usrdfndta(''' + TRTCM1ds + ''')';
223800111018         ELSE;
223900111018           Qcmd = C_CmdOvrPrtF;
224000111018         ENDIF;
224100111018
224200111018         callp(e) qCmdExc(Qcmd : %size(Qcmd));
224300111018
224400111018       ENDSR;
224500111018
224600130718       //--------------------------------------------------------------
224700151204       //?Cerca cliente in FIBSP
224800130718       //--------------------------------------------------------------
224900130718       BEGSR cer_bsp;
225000151204       //ksif=knsif;
225100151204       //setll (kcod: klin: ksif) tntbe02l;
225200151204       //if not %equal(tntbe02l);
225300151204       //   clear ksif;
225400151204       //   setll (kcod: klin: ksif) tntbe02l;
225500151204       //endif;
225600151204       //if %equal(tntbe02l);
225700151204       //   reade (kcod: klin: ksif) tntbe02l;
225800151204       //   dow not %eof(tntbe02l) and wbsp=' ';
225900151204       //      if tbeatb=' ' and tbeke2=v1cksc;
226000151204       //         wbsp='S';
226100151204       //         dbsp=tbeuni;
226200151204       //      endif;
226300151204       //      reade (kcod: klin: ksif) tntbe02l;
226400151204       //   enddo;
226500151204       //endif;
226600151204
226700151207       chain w1Cksc fibsp02l;
226800151209       if %found(fibsp02l);
226900160111         if bspfpa=*blanks or bspaut='N';
227000151204          wbsp='S';
227100151209         else;
227200160111          wbspchf='S';
227300151209         endif  ;
227400151204       endif;
227500130718       endsr;
227600111018       //--------------------------------------------------------------
227700111018       //?Operazioni finali.
227800111018       //--------------------------------------------------------------
227900111018       BEGSR RoutEnd;
228000111018
228100111018         if %open(PrtEMAIL);
228200111018
228300111018         //?Chiusura dello spool?
228400111018           clear O_testo;
228500111018           except PrtDet;
228600111018           O_testo = '***   Fine Lista   ***';
228700111018           except PrtDet;
228800111018
228900111018           close PrtEMAIL;
229000111018
229100111018         //?Eliminazione overflow?
229200111018           Qcmd = C_CmdDltOvr;
229300111018           callp(e) qCmdExc(Qcmd : %size(Qcmd));
229400111018
229500111018         endif;
229600111018
229700111018       ENDSR;
229800111018
229900111018      /end-free
230000111018     c*-------------------------------------------------------------------
230100061117     Oqsysprt   E    80n80   DET         1  1
230200061005     o                                              'Bolla:'
230300061005     O                       BLPaas        Z   +  1
230400061005     O                       BLPlnp            +  1
230500061005     O                       BLPNRS        Z   +  1
230600061005     O                       BLPNSP        Z   +  1
230700061005     O                       BLPmgs            +  1 '  /  '
230800061005     o               80                          48 'Cliente    <>'
230900061005     O               80      wksc              +  1
231000061005     o               81                          48 'Terminal   <>'
231100061005     o               82                          48 'Lin Arrivo <>'
231200061005     o               83                          48 'BLPDT1  <>  0'
231300061005     o               84                          48 'Esiste spunta'
231400061005     o               85                          49 'Bolla recupero'
231500061005     o               86                          48 'Prepagato    '
231600061005     o               87                          55 'Prepagato con numfat'
231700030619
231800111018     o* ---------------------------------------------------------------
231900111018     o* ?Spool di stampa (per e-mail).
232000111018     o* ---------------------------------------------------------------
232100111018     oPrtEMAIL  e            PRTdet      1
232200111018     o                       O_testo
232300111018
232400030619**  $Cmd
232500030707OVRPRTF FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
232600030619           USRDTA('Manut.Boll') SHARE(*YES)
232700030619           COPIES(2)
232800060403**  $Cmd2
232900060403OVRPRTF FILE(FNLS03P) SAVE(*YES)
233000061005**  $Cmd3
233100061005OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES) OUTQ(LJ4200CED) USRDTA('EDPES')
233200061012**  DLTOVR
233300061012DLTOVR  FILE(QSYSPRT)
233400060713**         MSG
233500111017MANUTENZIONE DATA SPED.BOLLE oltre xx giorni
233600111018Utente:  xxxxxxxxxx
233700060713Codice Cliente:  xxxxxxx   x
233800060713Bolle manutenzionate: xxx.xxx.xxx
233900081210Sono state manutenzionate
234000070612Se immesso tipo immettere anche la data  Consegna Richiesta
234100070612Data cons.richiesta formalmente errata o inferiore/uguale alla data odierna    4
234200070612Data consegna richiesta non ammessa: oltre                                     5
234300070612Data cons.richiesta  inferiore  alla nuova data spedizione
234400070614Ora consegna richiesta errata
234500090401Linea di partenza:  xxx
234600110704Immettere filiale di emissione valida                                         12
234700110704Immettere numero ORM esistente in archivio                                    13
234800111017Bolle immesse dal                                                             14
234900130711Se richiesta la sospensione automatica, non effettuare alcuna parzializzaz.   15
235000130729Ammessi solo caratteri numerici                                               16
