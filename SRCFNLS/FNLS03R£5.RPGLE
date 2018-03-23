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
002501151204     Ffibsp02l  IF   E           K DISK
002502151207     Ffibsp03l  IF   E           K DISK    rename(fibsp000:fibsp03)
002600050318     FfIAR701L  UF   E           K DISK
002700111018     FQSYSPRT   O    F  132        PRINTER OFLIND(*InOA)  usropn
002800111018
002900111018     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003000020610
003100941227     D L1              S              3  0 DIM(30)
003200020610     d sklnp           s              3  0 dim(30)
003300020610
003400030619     d $Cmd            s             80    dim(3) ctdata perrcd(1)
003500060403     d $Cmd2           s             80    dim(1) ctdata perrcd(1)
003600061005     d $Cmd3           s             80    dim(1) ctdata perrcd(1)
003700061012     d DLTOVR          s             80    dim(1) ctdata perrcd(1)
003800130729     D MSG             S             78    DIM(16) CTDATA PERRCD(1)
003900980202     D K31             S              1    DIM(31)
004000980202     D*
004100980202     D KSS             S              7    DIM(12)
004200070612     D elabor          S             54    DIM(6)
004300111019     D invbo           S              5  0 DIM(51)
004400111019     D invcod          S             25    DIM(51)
004500111019     D invdim          S              8  0 DIM(51)
004600020610
004700020610     d lnp             s              3  0
004800020610     d xx              s              2  0
004900111018     d zz              s              2  0 inz
005000030619      *
005100030619     d Qlen            s             15  5 inz(150)
005200030619      *
005300130718     d Wbsp            s              1    inz
005301160111     d Wbspchf         s              1    inz
005400030619     d Wfile           s             10    inz
005500030619     d Woutq           s             10    inz
005600030619     d Wform           s             10    inz
005700060405     d data_eur        s               D   datfmt(*eur)
005800070612     d Dataiso         s               d   datfmt(*iso)
005900070612     d woggidcgm1      s              8  0
006000070612     d woggidcgm2      s              8  0
006100070612     d s_woggidcg      s                   like(woggidcgm1)
006200070612     d s_vpodcg        s                   like(§vpodcgm1)
006300021203      *
006400111018
006500111018      // - Status
006600111018     d Psds           sds
006700111018     d   SDSpgm          *proc
006800111018     d   JobUser             254    263
006900111018
007000111018      // - Campi per QCMDEXC
007100111018     d Qcmd            s            512    inz
007200111018     d
007300111018      // - Esecuzione comando di sistema
007400111018     d qCmdExc         pr                  extpgm('QCMDEXC')
007500111018     d  Qcmd                        512    const  options(*varsize)
007600111018     d  Qlen                         15  5 const
007700111018
007800021203      * Ds esterna per ricerca codice padroncino
007900021203     D FNLV24DS      E DS
008000051209     D* DS PER TIBS02R - GESTIONE TNTBE00F
008100051209     D TIBS02DS      E DS
008200111018      // - Tabella "MRA" = Bart-Mailing - Danni
008300111018     d dMRAdan       e ds                  inz
008400130726      * Ds esterna per ricerca cliente da tab. "BSP"
008500151201     d fnlsbsp1ds    e ds                  inz
008600111018
008700070612     d
008800070612     D Dvpo          E DS
008900070903     D og143         E DS
009000081211     d
009100081211     dwidmsall         ds
009200081211     Dwidms1                         32
009300081211     Dwidms2                         32
009400081211     Dwidms3                         32
009500081211     Dwidms4                         32
009600110704     D                 DS
009700110704     D  V1CPOE                 1      3  0
009800110704     D  V1CNSR                 4      5  0
009900110704     D  V1CNOR                 6     12  0
010000110704     D  V1CNRV                13     14  0
010100110704     D  SEL4                   1     14  0
010200081211     D***
010300941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010400980202     D WGIDAT          DS                  INZ
010500980202     D  GIODAT                 1      8  0
010600980202     D  GIOINV                 9     16  0
010700980202     D  GIOTGI                17     21  0
010800941227     D***
010900941227     D WLBDAT          DS                  INZ
011000941227     D  G02DAT                 1      8  0
011100941227     D  G02INV                 9     16  0
011200941227     D  G02ERR                17     17
011300941227     D  G02TGI                18     22  0
011400980202     D                 DS
011500980202     D  WAA                    1      4  0
011600980202     D  WMM                    5      6  0
011700980202     D  WGG                    7      8  0
011800980202     D  WDATA                  1      8  0
011900060403
012000060403     d WDAT8           DS
012100060403     d  datada                 1      8  0
012200060403     d  dataa                  9     16  0
012300060403     d  ggl                   17     21  0
012400060713     D                 DS
012500110704     D  blpaas                 1      4  0
012600110704     D  blpmgs                 5      8  0
012700110704     D  blpdsp                 1      8  0
012800060926     d
012900060926     D                 DS
013000130729     D  W1cksc                 1      7  0
013100060926     D  v1clnp                 8     10  0
013200061003     D  wclna                 11     13  0
013300060926     D  v1ctfa                14     16  0
013400060926     D  v1cnos                17     17
013500060926     D  v1cggl                18     18  0
013600060926     D  v10dsp                19     26  0
013700060926     D  v11dsp                27     34  0
013800061012     D  Wcpdr                 35     41  0
013900070612     D  V1CTCR                42     42
014000070612     D  wdcr                  43     50  0
014100070612     D  V1CHCR                51     54  0
014200070612     D PARAM                   1     54
014300130711
014400130711     D fnlsu3ds        DS
014500130711     D  ilsu3flg               1      1
014600130711     D  ilsu3aas               2      5
014700130711     D  ilsu3lnp               6      8
014800130711     D  ilsu3nrs               9     10
014900130711     D  ilsu3nsp              11     17
015000130711     D  ilsu3DSP_B            18     25
015100130711     D  ilsu3DSP_S            26     33
015200130711     D  ilsu3nos              34     34
015300130711     d
015400130711     d  ilsu3tcr              35     35
015500130711     d  ilsu3dcr              36     43
015600130711     d  ilsu3hcr              44     47
015700130711     D  olsu3err             100    101
015800130711     D  olsu3boagg           102    110
015900130830     D  olsu3bonagg          111    116
016000150703     D  olsu3msg             117    193
016001150703     D  olsu3sosp            194    194
016002151207     d                 ds
016003151207     d bsplin
016004151207     d sbsp                           3  0 dim(30) overlay(bsplin)
016100130711     d
016200060403
016300941116     D KPJBA         E DS
016400130729
016500130729       // -?Parametri x Controllo profilo utenti?
016600130729     d TIBS34ds      e ds
016700130729       // -?Ds di riferimento al file esterno AZUTE00F?
016800130729     d AZUTEds       e ds                  extname(AZUTE00F)
016900130729       // -?Ds per dati organigramma?
017000130729     d dDATIUTE      e ds
017100130729
017200130327     *** * Parametri per stampa BOLLE ed ETICHETTE
017300130327     ***d TRUL90DS      e ds                  inz
017400130327     ***d   D90rse      e                     inz('N')
017500130327     ***d   D90rsb      e                     inz('S')
017600130327     *** * DS per pgm di STAMPA BOLLE
017700130327     ***d FNLSB5DS      e ds                  inz
017800130327     ***d FNLSB6ds1     e ds                  inz
017900130327     ***d  DB6pdf       e                     inz('F')
018000081203      * Ds per pgm che mette in sospensione rinumerando la spedizione
018100081203     d FNLS65DS      e ds                  inz
018200030619      *
018300980202     D* DS PER TRUL06R - CARICAMENTO £X
018400941227     D DSUL06        E DS                  EXTNAME(TRUL06DS)
018500941227     D  LIN                    1     90  0
018600941227     D                                     DIM(30)
018700980129     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
018800980129     D DSLV55        E DS                  EXTNAME(FNLV55DS)
018900081203     D fnblpinf        ds
019000110705     D  fnblpnrri            397    400i 0
019100110705     D blpnrr01        ds
019200110705     D  fnblpnrr01           397    400i 0
019300081203      * DS Esterna x gestione allocazione ed invio messaggi
019400081203     D TRUL82DS      E DS
019500980130     D DS3A          E DS
019600090427     D DS1B          E DS
019700081203     D Ds3c          E DS
019800980202     D DS3I          E DS
019900130717     D*DCLI          E DS
020000130717     D Dbsp          E DS
020100140205     D og140         E DS
020200021203
020300021203     D kpdpdr          S                   LIKE(APDpdr)
020400021203     D kpdtip          S                   LIKE(APDtip)
020500151207     D*kcod            S                   LIKE(tbecod) inz('BSP')
020600151207     D*klin            S                   LIKE(tbelin) inz(' ')
020700151207     D*ksif            S                   LIKE(tbesif)
020800110704     D ktrc            S                   LIKE(ar4trc)
020900110704     D ktrcM           S                   LIKE(ar4trc) inz('M')
021000081205     D wlpnsp          S                   LIKE(blpnsp)
021100081205     D wlpaas          S                   LIKE(blpaas)
021200081210     D wmsg            S                   LIKE(v1cmsg)
021300090126     D ntwlnp          S              1    inz
021400060926     D YY              S              3  0 inz
021500111214     D savtfp          S              3  0 inz
021600080620     D sosdaL1         S              1    inz
021700110704     D Indx            S              3  0 inz
021800111018     d O_testo         s            132    inz
021801151207     d IX              s              4  0
021900060713     D* DEFINIZIONE COSTANTI
022000060404     D Ctitolo         C                   CONST('ELENCO SPEDIZIONI IN SOSPENSI-
022100060405     D                                     ONE DA OLTRE X GIORNI')
022200081203     D msgjob          C                   CONST('Si sta bloccando la manutenzi-
022300081203     D                                     one data spedizione:USCIRE un moment-
022400081203     D                                     o da questo lavoro')
022500130111     D*ms2wdw          C                   CONST('Probabilmente in manutenzione-
022600130111     D*                                       ')
022700130111     D*ms3wdw          C                   CONST('da altro utente.             -
022800130111     D*                                       ')
022900130111     D*ms4wdw          C                   CONST('Rieseguire il lancio.        -
023000130111     D*                                       ')
023100130111     D ms2wdw          C                   CONST('                             -
023200130111     D                                        ')
023300130111     D ms3wdw          C                   CONST('Segnalare al CED di verificar-
023400130111     D                                     e  ')
023500130111     D ms4wdw          C                   CONST('il JOBLOG                    -
023600130111     D                                        ')
023700130729
023800130729       // -?Costante per controllo "caratteri solo numerici"?
023900130729     d c_Digits        c                   const('0123456789')
024000111018
024100111018      // - Comando di override al PrtF
024200111018     d C_CmdOvrPrtF    c                   const('OVRPRTF +
024300111018     d                                           file(PRTEMAIL) +
024400111018     d                                           pagesize(66 132) +
024500111018     d                                           lpi(6) cpi(10) +
024600111018     d                                           ovrscope(*actgrpdfn) +
024700111018     d                                           ')
024800111018     d C_CmdDltOvr     c                   const('DLTOVR +
024900111018     d                                            file(PRTEMAIL) +
025000111018     d                                            lvl(*actgrpdfn)')
025100111018      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
025200111018     d TRTCM1ds      e ds                  inz
025300111018      //   ·§CM1mitt = Indirizzo e-mail del mittente
025400120820     d   §CM1mitt    e                     inz(' ed@brt.it')
025500111018      //   ·§CM1dst  = Indirizzo e-mail del destinatario
025600120302     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
025700111018      //   ·§CM1tips = Tipo lettera via e-mail:
025800111018      //    "LET" = testo allegato in corpo con logo
025900111018      //            (richiede righe libere iniziali per il logo)
026000120820      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
026100111018      //            (non consente né UNDERLINE né HIGHLIGHT)
026200120820     d   §CM1tips    e                     inz('COI')
026300111018      //   ·§CM1po   = Filiale
026400111018     d   §CM1po      e                     inz('046')
026500111018      //   ·§CM1var  = Oggetto e-mail
026600111018     d   §CM1var     e                     inz('*OBJM*+
026700111019     d                                     Manutenzione data spedizione+
026800111019     d                                      bolle ')
026900111018      //   ·§CM1sts  = Stato
027000111018     d   §CM1sts     e                     inz(*off)
027100111018      //   ·§CM1sts  = Id processo
027200111018     d   §CM1idp     e                     inz('2')
027300111018
027400111018
027500910412     C****************************************************************
027600910412     C*  RIEPILOGO INDICATORI
027700910412     C***************************************************************
027800070612     C* 01    - possibilità di richidere bolle in sospensione x colli non
027900070612     c*         spuntati
028000070612     C* 02    -
028100070612     C* 03    - per colli non spuntati possibilità di modificare la consegna
028200130717     c*         richiesta da tabella BSP
028300070612     C* 12    - PROTEZIONE CAMPO LINEA DI PARTENZA XCHE' SONO UN 2° LIVELLLO
028400910412     C* 30    - DI COMODO
028500980129     C* 40-50 - ERRORE
028600980130     C* 51-55 - GESTIONE CHIAVE LETTURA BOLLE
028700070612     C* 70    - posizionam.cursone su DCR
028800070612     C* 90    - ERRORE GENERICO
028900910412     C*****************************************************************
029000941116     C**
029100980130     C     *ENTRY        PLIST
029200980130     C                   PARM                    KPJBA
029300030619      *
029400130327     *** * Richiedo i dati per la stampa ed eseguo le override
029500130327     ***c                   exsr      SR_OvrPrtF
029600130327     ***C**
029700130327     ***C* F3 - FINE
029800130327     ***C     D90F3         IFEQ      '1'
029900130327     ***C                   GOTO      FINE
030000130327     ***C                   ENDIF
030100130327     *** * Errori nelle OVERRIDE -> FINE
030200130327     ***C   91              GOTO      FINE
030300991206     C*
030400950411     C     INIZIO        TAG
030500980130     C                   EXSR      PUL01
030600950411     C*
030700980130     C                   MOVE      WDTGIO        WAAS              4 0
030800980130     C                   MOVEL     CDU           V1CLNP
030900950411     C**
031000900509     C     FOR01         TAG
031100980129     C                   EXFMT     LS03D01
031200941227     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
031300110704     C                   SETOFF                                       902873
031400110704     C                   SETOFF                                       707172
031500130711     C                   SETOFF                                       74
031600081210     c                   clear                   v1cmsg
031700130711     C** F3 - FINE LAVORO
031800900509     C   KC              GOTO      FINE
031900130711     c
032000130726     c* F11-  seleziona  tabella sospensione senza codici senza spunte
032100130711     c*       tabella DSP
032200130711     c                   if        *inkk=*on
033000130729      * Sola visualizzazione di TUTTI i clienti (di qualsiasi filiale)
033100130729      *   in tab. "BSP"
033200151201     c***                clear                   tntbBSPds1
033300151201     c***                eval      iBSPmod = 'V'
033400151201     c***                eval      iBSPtfp = DUTtfp
033500151201     c***                eval      KPJBU = tntbBSPds1
033600151201     c***                call      'TNTBBSPR1'
033700151201     c***                parm                    KPJBA
033800151201     c***                eval      tntbBSPds1 = KPJBU
033900151201     c***                goto      for01
033901151216      * Sola visualizzazione  dei codici del terminal di partenza
033903151201     c                   clear                   fnlsbsp1ds
033904151201     c                   eval      iBSPmod = 'V'
033905151216     c                   eval      iBSPtfp = DUTtfp
033907151201     c                   eval      KPJBU = fnlsbsp1ds
033908151201     c                   call      'FNLSBSP1R'
033909151201     c                   parm                    KPJBA
033910151201     c                   eval      fnlsbsp1ds = KPJBU
033911151201     c                   goto      for01
034000151201     c                   endif
034100130711     c
034200910205     C* CONTROLLI FORMATO
034300980130     C                   EXSR      CNT01
034400980202     C  NKF
034500130711     CANNKH
034600941228     COR 90              GOTO      FOR01
034700130711     c
034800130711     c                   if        *inkh=*on
034900130711     c                   clear                   fnlsu3ds
035000130711     c                   eval      ilsu3dsp_B=%editc(wdtcar:'X')
035100130711     c                   eval      ilsu3dsp_S=%editc(wlpdsp:'X')
035200130711     c                   eval      ilsu3nos='S'
035300130711     c                   movel     fnlsu3ds      kpjbu
035400130711     C                   MOVEL     'LSU3'        KCOAZ
035500130711     C                   CALL      'FNLSU3R'
035600130711     C                   PARM                    KPJBA
035700130711     c*
035800130711     c                   movel     kpjbu         fnlsu3ds
035900130711
036000130711     c                   if        olsu3boagg>*zeros
036100130711     c                   movel     olsu3boagg    bollema
036200130830     c                   endif
036300130830     c                   if        olsu3bonagg>*zeros
036400130830     c                   movel     olsu3bonagg   werror
036500130830     c                   endif
036600130830     c                   eval      wmsg=olsu3msg
036700130711     c                   exsr      emis_msg
036800130830
036900130830     c                   if        werror>0
037000130830     c                   do        *hival
037100130830     c                   exfmt     ls03w01
037200130830     c                   if        *inkl='1'
037300130830     c                   leave
037400130830     c                   endif
037500130830     c                   enddo
037600130830     c                   endif
037700130711     c
037800130711     c                   goto      inizio
037900130711e   1C                   ENDIF
038000900523     C*
038100980129     C* M O D I F I C A   D A T A   S P E D I Z I O N E
038200130327     ***C   KH
038300130327     ***COR KF              EXSR      MOD01
038400130327     C   KF              EXSR      MOD01
038500130711     c
038600081211     c                   if        werror>0
038700081211     c                   do        *hival
038800081211     c                   exfmt     ls03w01
038900081215     c                   if        *inkl='1'
039000081211     c                   leave
039100081211     c                   endif
039200081211     c                   enddo
039300081211     c                   endif
039400081211
039500910207     C*
039600950411     C                   GOTO      INIZIO
039700000000     C     FINE          TAG
039800980202     C**
039900130327     ***C     D90F3         IFne      '1'
040000130327if  1***c                   if        DB6num > *zero
040100130327     ***c                   clear                   FNLSB5ds
040200130327     ***c                   eval      DB0tbo = 'P'
040300130327     ***c                   eval      DB0fa4 = 'I'
040400130327     ***c                   eval      DB6pdf = 'S'
040500130327     ***c                   call      D90psl
040600130327     ***c                   parm                    FNLSB5ds
040700130327     ***c                   parm                    FNLSB6ds1
040800130327e   1***c                   endif
040900130327     ***c                   clear                   FNLSB5DS
041000130327     ***c                   movel     'C'           DB0TLA
041100130327     ***c                   call      D90psl
041200130327     ***c                   parm                    FNLSB5DS
041300130327     ***c                   endif
041400980202     C*
041500980202     C                   CLEAR                   DSLV55
041600980202     C                   MOVEL     'C'           D55TLA
041700980202     C                   CALL      'FNLV55R'
041800980202     C                   PARM                    DSLV55
041900081203     c*
042000081203     C                   CLEAR                   fnls65ds
042100081203     C                   MOVEL     'C'           i65tla
042200081211     c                   movel     '1'           I65STRCMT
042300081203     c                   movel     fnls65ds      kpjbu
042400081205     C                   CALL      'FNLS65R'
042500081203     C                   PARM                    kpjba
042600980202     C**
042700060926     c                   clear                   kpjbu
042800060926     c                   movea     elabor        kpjbu
042900060926     c                   if        kpjbu<>*blanks
043000060926     C                   MOVEL     'LS0G'        KCOAZ
043100060926     C                   CALL      'BCH10'
043200060926     C                   PARM                    KPJBA
043300060926     c                   endif
043400060926     c
043500061012     c
043600061012     c                   clear                   qcmd
043700061012     c                   eval      qcmd = dltovr(1)
043800061012     c                   call      'QCMDEXC'                            91
043900061012     c                   parm                    Qcmd
044000061012     c                   parm                    Qlen
044100061012     c
044200111018     c                   if        invbo(1)>0
044300111018     c                   exsr      Inv_email
044400111018     c                   endif
044500111018     c
044600000000     C                   SETON                                        LR
044700941116     C*
044800980129     C*****************************************************************
044900910205     C* CONTROLLO FORMATO -------------------------------------------
045000980129     C*****************************************************************
045100980130     C     CNT01         BEGSR
045200910205     C                   SETOFF                                       90
045300130729      *
045400130729     c                   clear                   W1Cksc
045500130729     c                   Select
045600130729      * - Ricerca da tab. "BSP"
045700130729     c                   When      %scan( '?' : V1Cksc ) > *zero
045800130729     c                   clear                   V1Cksc
045900151201     c****               clear                   tntbBSPds1
046000151201     c****               eval      KPJBU = tntbBSPds1
046100151201     c****               call      'TNTBBSPR1'
046200151201     c****               parm                    KPJBA
046300151201     c****               eval      tntbBSPds1 = KPJBU
046301151216      *   Visualizzazione di tutti i cliente a prescindere da tfp
046302151216      *   esclusione dei codici che partono da + filiali
046303151201     c                   clear                   fnlsbsp1ds
046304151204     c                   eval      ibsptps='K'
046305160111     c                   eval      iBSPfpa = 'C'
046306151201     c                   eval      KPJBU = fnlsbsp1ds
046307151201     c                   call      'FNLSBSP1R'
046308151201     c                   parm                    KPJBA
046309151201     c                   eval      fnlsbsp1ds = KPJBU
046400130729     c                   if        oBSPfxx <= *zero  and
046500151204     c                             oBSPerr  = *blanks
046600130729     c                   eval      W1Cksc = oBSPcli
046700130729     c                   eval      V1Cksc = %editc( oBSPcli : 'X' )
046800130729     c                   eval      V1Dksc = oBSPdes
046900130729     c                   endif
047000130729     c                   eval      *in74 = *on
047100130729     c                   eval      *in90 = *on
047200130729     c                   leavesr
047300130729      * - Verifica se contiene solo caratteri numerici
047400130729     c                   When      V1Cksc <> *blanks  and
047500130729     c                             %checkR( c_Digits : V1Cksc ) > *zero
047600130729     c                   movel     Msg(16)       V1Cmsg
047700130729     c                   eval      *in28 = *on
047800130729     c                   eval      *in74 = *on
047900130729     c                   eval      *in90 = *on
048000130729     c                   leavesr
048100130729     c                   EndSl
048200130729      *
048300130729     c                   if        V1Cksc > *zeros
048400130729     c                   eval      W1Cksc = %int( V1Cksc )
048500130729     c                   endif
048600130729      *
048700130711     c* Se ho premuto F8 tutte le selezioni ddevono essere vuote
048800130711     c*  tranne le date
048900130711     c                   if        *inkh=*on
049000130711     c
049100130729     c                   if        W1cksc>*zeros or
049200130711     c                             v1clna>*zeros or
049300130711     c                             v1ctfa>*zeros or
049400130711     c                             v1cpdr >*zeros or
049500130711     c                             sel4>0
049600130711     C                   MOVEL     MSG(15)       V1CMSG
049700130711     C                   SETON                                        287490
049800130711     C                   GOTO      ENDCTR
049900130711     C                   ENDIF
050000130729     c*
050100130711     c                   else
050200910412     C*
050300980129     C* SE TUTTE LE SELEZIONI SONO A BLANKS E' ERRORE
050400130729     C     W1CKSC        IFEQ      *ZEROS
050500980130     C     V1CLNP        ANDEQ     *ZEROS
050600980129     C                   SETON                                        4090
050700941116     C                   GOTO      ENDCTR
050800941116     C                   ENDIF
050900051118     c
051000980129     C* CODICE CLIENTE
051100130729    1C     W1CKSC        IFEQ      0
051200980130    2C     V1DKSC        IFNE      *BLANKS
051300980129     C* PARSTA = 9  ESCLUDO ANNULLATI
051400980129     C                   Z-ADD     9             PARSTA
051500980129     C                   MOVEL     RSUT          PARDUT           30
051600980129     C                   MOVEL     *BLANKS       DESCR            48
051700980130     C                   MOVEL     V1DKSC        DESCR
051800130729     C                   Z-ADD     dutKCI        CCC               4 0
051900981109     C                   CLEAR                   PARFLR
052000981109     C                   CLEAR                   PARDIT
052100000907     C                   Z-ADD     1             PAXNUM
052200000907     C                   CALL      'XALFA3BR'
052300980129     C                   PARM                    PARDUT
052400980129     C                   PARM                    CODUT
052500980129     C                   PARM                    DESCR
052600980129     C                   PARM                    CCC
052700980129     C                   PARM                    PARSTA            1 0
052800981109     C                   PARM                    PARFLR           90
052900981109     C                   PARM                    PARDIT            3
053000000907     C                   PARM                    PAXNUM            2 0
053100000907     C                   PARM                    PAXKCM           80
053200000907     C                   PARM                    PAXKSM          140
053300000907     C                   PARM                    PAXKDM           60
053400980129     C     PARSTA        IFNE      -1
053500130729     C                   MOVEL     PAXKSM        W1CKSC
053600000907     C                   MOVEL     PAXKSM        V1CKSC
053700980130     C                   MOVEL     DESCR         V1DKSC
053800980129     C                   ELSE
053900980202     C                   CLEAR                   V1DKSC
054000980129     C                   ENDIF
054100980129     C*
054200980129     C                   SETON                                        90
054300980129     C                   GOTO      ENDCTR
054400980129    2C                   ENDIF
054500980129   X1C                   ELSE
054600980129     C*
054700980129     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
054800130729     C                   Z-ADD     dutKCI        CCC
054900130729     c                   mllzo     1             W1cksc
055000980129     C     KACO          CHAIN     CNACO000                           41
055100980129    2C  N41ACOFLG        IFNE      ' '
055200980129     C                   SETON                                        41
055300980129    2C                   ENDIF
055400980129     C     *IN41         IFEQ      *OFF
055500980202     C                   MOVEL     ACORAG        V1DKSC
055600980129     C                   ELSE
055700980204     C                   CLEAR                   V1DKSC
055800980129     C                   SETON                                        90
055900980129     C                   GOTO      ENDCTR
056000980129     C                   END
056100980129    1C                   ENDIF
056200130729     c*
056300980130     C* LINEA DI PARTENZA
056400061003     c                   mllzo     1             v1clnp
056500090126     c                   clear                   ntwlnp
056600980130    1C     V1CLNP        IFNE      *ZEROS
056700980130     C* CONTROLLO CHE SIA GESTIBILE
056800980130     C     V1CLNP        LOOKUP    L1                                     30
056900980130    2C     *IN30         IFEQ      *OFF
057000980130     C                   SETON                                        9042
057100980204     C                   CLEAR                   V1DLNP
057200980130     C                   GOTO      ENDCTR
057300980130    2C                   ENDIF
057400980130     C*
057500090126     c                   clear                   og143
057600980130     C     V1CLNP        CHAIN     AZORG01L                           30
057700980130     C   30              SETON                                        9042
057800980204     C   30              CLEAR                   V1DLNP
057900980130     C   30              GOTO      ENDCTR
058000980130     C  N30              MOVEL     ORGDES        V1DLNP
058100090126     c  n30              movel     orgde3        og143
058200090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
058300090126     c                             §ogntw='FED'
058400090126     c                   eval      ntwlnp='E'
058500090126     c                   endif
058600980130    1C                   ENDIF
058700980130     C* LINEA DI ARRIVO
058800061003     c                   clear                   wclna
058900980130    1C     V1CLNA        IFNE      *BLANKS
059000980130     C     V1CLNA        ANDNE     *ZEROS
059100980130     C*
059200980130     C     '?'           SCAN      V1CLNA                                 30
059300980130     C*
059400980130    2C     *IN30         IFEQ      *ON
059500980130     C                   MOVE      *BLANKS       §COD1
059600980130     C                   MOVE      *BLANKS       V1CLNA
059700980130     C                   CALL      'TNSD19R'
059800980130     C                   PARM                    §COD1             3
059900980130     C                   PARM                    §TIP              1
060000980202     C                   PARM                    §DES             25
060100980130     C                   MOVEL     §COD1         V1CLNA
060200980130     C                   MOVEL     §DES          V1DLNA
060300980130     C                   SETON                                        90
060400980130     C                   GOTO      ENDCTR
060500980130    2C                   ENDIF
060600980130     C* ESEGUO TESTN
060700980130     C                   TESTN                   V1CLNA               30
060800980130     C                   MOVE      V1CLNA        W001A
060900980130     C     *IN30         IFEQ      *OFF
061000980130     C     W001A         ORLT      '0'
061100980204     C                   CLEAR                   V1DLNA
061200980130     C                   GOTO      ENDCTR
061300980130     C                   ENDIF
061400980130     C*
061500980130     C*  CONTROLLO
061600980130     C*
061700980130     C                   MOVE      V1CLNA        WCLNA             3 0
061800980130     C     WCLNA         CHAIN     AZORG01L                           30
061900980205     C* CONTROLLO SE ANNULLATO
062000980205     C* CONSIDERO SOLO I TIPI "F" E "A"
062100980205    2C  N30ORGFVA        IFNE      *BLANKS
062200980205    2C     ORGFAG        ORNE      'F'
062300980205     C     ORGFAG        ANDNE     'A'
062400980205     C                   SETON                                        30
062500980205    2C                   END
062600980130     C   30              MOVEL     *BLANKS       V1DLNA
062700980130     C   30              SETON                                        9044
062800980130     C   30              GOTO      ENDCTR
062900980130     C  N30              MOVEL     ORGDES        V1DLNA
063000980130    1C                   ENDIF
063100980130     C*
063200980130     C* CONTROLLO CODICE AUTOTRASPORTATORE
063300061003     c                   clear                   wcpdr
063400980130    1C     V1CPDR        IFNE      *BLANKS
063500980130     C     V1CPDR        ANDNE     *ZEROS
063600980130     C                   CLEAR                   V1DPDR
063700980130     C*
063800980130     C     '?'           SCAN      V1CPDR                                 30
063900980130     C*
064000980130     C     *IN30         IFEQ      *ON
064100021203     C*                  MOVEL     *BLANKS       PA2DES
064200021203     C*                  Z-ADD     0             PA2PAD
064300021203     C*                  Z-ADD     V1CLNP        PA2FIL
064400021203     C*                  MOVEL     'R'           PA2FLG
064500021203     C*                  MOVEL     PARAM2        KPJBU
064600021203      *
064700021203      * imposto i campi della ds esterna fnlv24ds
064800021203     C                   clear                   fnlv24ds
064900021203     C                   eval      d24fil   = v1clnp
065000021203     C                   eval      d24tip   = 'A'
065100021203     C                   eval      d24flg   = 'R'
065200021203     C                   movel     fnlv24ds      kpjbu
065300980130     C                   CALL      'FNLV24R'
065400980130     C                   PARM                    KPJBA
065500021203     C*                  MOVEL     KPJBU         PARAM2
065600021203     C*                  MOVE      PA2PAD        V1CPDR
065700021203     C*                  MOVEL     PA2DES        V1DPDR
065800021203      *
065900021203      * valorizzo i campi a video
066000021203     C                   movel     kpjbu         fnlv24ds
066100021203     C                   move      d24pdr        v1cpdr
066200021203     C                   movel     d24rsc        v1dpdr
066300980130     C                   GOTO      ENDCTR
066400980130     C                   END
066500980130     C* ESEGUO TESTN
066600980130     C                   TESTN                   V1CPDR               30
066700980130     C                   MOVE      V1CPDR        W001A             1
066800980130     C     *IN30         IFEQ      *OFF
066900980130     C     W001A         ORLT      '0'
067000980205     C                   CLEAR                   V1CPDR
067100980205     C                   CLEAR                   V1DPDR
067200980130     C                   GOTO      ENDCTR
067300980130     C                   ENDIF
067400980130     C*
067500021204     C                   CLEAR                   V1DPDR
067600980130     C                   MOVE      V1CPDR        WCPDR             7 0
067700021203     C*    WCPDR         CHAIN     FNAPD000                           30
067800021204     C                   move      wcpdr         kpdpdr
067900021203     C                   move      'A'           kpdtip
068000021203     C     kfapd         chain     fiapd01l                           30
068100980130     C     *IN30         IFEQ      *ON
068200980205     C                   CLEAR                   WCPDR
068300980205     C                   CLEAR                   V1DPDR
068400980130     C                   SETON                                        4590
068500980130     C                   GOTO      ENDCTR
068600980204     C                   CLEAR                   V1DPDR
068700980204     C                   ELSE
068800980204     C                   MOVEL     APDRSC        V1DPDR
068900980130     C                   ENDIF
069000980130     C*
069100980205     C* RECUPERO LA LINEA DI PARTENZA DALLA PRIMA PARTE DELL'AUTOTRASPORTATORE
069200980130     C     V1CLNP        IFEQ      *ZEROS
069300980202     C                   MOVEL     WCPDR         V1CLNP
069400090126     c                   clear                   ntwlnp
069500090126     c                   clear                   og143
069600980130     C     V1CLNP        CHAIN     AZORG01L                           30
069700980130     C  N30              MOVEL     ORGDES        V1DLNP
069800090126     C  N30              MOVEL     ORGDE3        og143
069900090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
070000090126     c                             §ogntw='FED'
070100090126     c                   eval      ntwlnp='E'
070200090126     c                   endif
070300980204     C   30              MOVEL     *BLANKS       V1DLNP
070400980205     C                   ELSE
070500151207     C* CONTROLLO LA LINEA DI PARTENZA CON QUELLA DELL'AUTOTRASPORATTORE
070600980205     C                   MOVEL     WCPDR         W0030
070700980205     C     V1CLNP        IFNE      W0030
070800980205     C     W0030         ANDNE     *ZEROS
070900980205     C                   SETON                                        56  90
071000980205     C                   Z-ADD     *ZEROS        W0030
071100980205     C                   GOTO      ENDCTR
071200980205     C                   ENDIF
071300980202     C                   ENDIF
071400980205     C                   ENDIF
071500051118
071600110704     c* controllo numero ORM
071700110704     c                   if        sel4>0
071800110704      * P.O. EMISSIONE
071900110704     C                   Z-ADD     V1CPOE        NUM3              3 0
072000110704     C     NUM3          CHAIN     AZORG01L                           30
072100110704     C     *IN30         IFEQ      *ON
072200110704    2C     ORGFVA        orne      *BLANKS
072300110704    2C     ORGFAG        ORNE      'F'
072400110704     C     ORGFAG        ANDNE     'A'
072500110704     C                   MOVEL     MSG(12)       V1CMSG
072600110704     C                   SETON                                        287290
072700110704     C                   GOTO      ENDCTR
072800110704     C                   ENDIF
072900110704      * N.ORM
073000110704     C     KFORM         CHAIN     FNORM01L                           30
073100110704     C     *IN30         IFEQ      *ON
073200110704     C                   MOVEL     MSG(13)       V1CMSG
073300110704     C                   SETON                                        287390
073400110704     C                   GOTO      ENDCTR
073500110704     C                   ENDIF
073600110704      *
073700110704     C                   SETON                                        06
073800110704      *
073900110704     C                   ENDIF
074000051118     c* Se immessa solo lnp, immettere anche cliente o linea arrivo o
074100051118     c*  terminal arrivo o autotrasportatore
074200051118     c                   if        v1clnp>0 and
074300130729     c                             (W1cksc=0 and v1clna<=*zeros and v1ctfa=0
074400110704     c                              and v1cpdr<=*zeros and v1cnos=' ' and
074500110704     c                              sel4=0)
074600051118     C                   SETON                                        6090
074700051118     C                   GOTO      ENDCTR
074800051118     C                   ENDIF
074900051209     c
075000080620     c                   if        not *in01
075100080620     c                   clear                   v1cnos
075200080620     c                   endif
075300080620     c
075400080620     c* Per mettere in sospensione  in base alle spunte devo essere
075500151204     c*  abilitato (Fibsp00F)
075600051209     C
075800130718     c                   clear                   Wbsp
075801160111     c                   clear                   Wbspchf
075900130729    1c                   if        W1cksc>0
076000130718     c***                Clear                   tibs02ds
076100130718     c***                Movel     'C'           T02Mod
076200130718     c***                Movel     KNSIF         T02Sif
076300130718     c***                Movel     'CLI'         T02Cod
076400130729     c***                Movel     W1Cksc        T02Ke1
076500130718     c***                Call      'TIBS02R'
076600130718     c***                Parm                    Kpjba
076700130718     c***                Parm                    tibs02ds
076800130718    2c***                If        T02Err = *Blanks
076900130718     c***                Movel     T02Uni        Dcli
077000130718    2c**                 endif
077100130718     c* cerco il cliente in tabella bsp
077200130718     c                   exsr      cer_bsp
077300080620    1c                   endif
077400080620
077500130718     c                   if        not *in01 and wbsp='S'
077600080620     c                   seton                                        01
077700080620     C                   SETON                                        90
077800080620     C                   GOTO      ENDCTR
077900080620     c                   endif
078000130718     c                   if        sosdaL1=' 'and *in01 and wbsp =' '
078100080620     c                   setoff                                       01
078200080620     C                   SETON                                        90
078300080620     C                   GOTO      ENDCTR
078400080620     c                   endif
078500051209     c
078600080620    1c                   if        v1cnos='S'
078700130729    2c                   if        W1cKSC=0 and ntwlnp<>'E'
078800080620     C                   SETON                                        5790
078900080620     C                   GOTO      ENDCTR
079000080620    2C                   ENDIF
079100080620     c
079200130718    2c                   if        wbsp=' ' and ntwlnp<>'E'
079201160111     c                   if        wbspchf=' '
079300051209     C                   SETON                                        58  90
079301151209     c                   else
079302151209     C                   SETON                                        61  90
079303151209     c                   endif
079400051209     C                   GOTO      ENDCTR
079500070614   x2c                   else
079600060403     c* se numero giorni per stampa = 0 --> errore
079700070614    3c                   if        v1cggl = 0
079800060403     C                   SETON                                        59  90
079900070614    3c                   endif
080000070612     c* se posso emetto il campo della consegna richiesta
080100151204    3c                   if        BSPDCRP='S' and not *in03
080200070612     c                   seton                                        0390
080300070612     c                   clear                   v1ctcr
080400070612     c                   clear                   v1cdcr
080500070612     c                   clear                   v1chcr
080600070612     C                   GOTO      ENDCTR
080700070614    3c                   endif
080800070612     c
080900151204    3c                   if        BSPDCRP=' ' and *in03
081000070612     c                   seton                                        90
081100070612     c                   setoff                                       03
081200070612     c                   clear                   v1ctcr
081300070612     c                   clear                   v1cdcr
081400070612     c                   clear                   v1chcr
081500070612     C                   GOTO      ENDCTR
081600070614    3c                   endif
081700070612     c* Altrimenti controllo la consegna richiesta
081800151204    3c                   if        BSPDCRP='S'
081900070612     c                   exsr      CtrCONSRICH
082000070614    4c                   if        *in90
082100070612     c                   goto      ENDCTR
082200070614    4c                   endif
082300070614    3c                   endif
082400070612     c
082500070614    2C                   ENDIF
082600070614   x1c                   else
082700070614     c* se cliente senza abilitazione bolla tolgo la consegna rich
082800070614    3c                   if        *in03
082900070614     c                   seton                                        90
083000070614     c                   setoff                                       03
083100070614     c                   clear                   v1ctcr
083200070614     c                   clear                   v1cdcr
083300070614     c                   clear                   v1chcr
083400070614     C                   GOTO      ENDCTR
083500070614    3c                   endif
083600070614    1C                   ENDIF
083700130711     c
083800130711     c                   endif
083900051209     c
084000980130     C* CONTROLLO DATA SPEDIZIONE DA MODIFICARE ----------------------*
084100980130     C*
084200980130     C                   Z-ADD     V10DSP        G02DAT
084300980130     C                   MOVEL     *BLANK        G02ERR
084400980130     C                   CALL      'XSRDA8'
084500980130     C                   PARM                    WLBDAT
084600980130     C* ERRATA
084700980130    1C     G02ERR        IFEQ      '1'
084800980130     C                   SETON                                        46  90
084900980130     C                   GOTO      ENDCTR
085000980130    1C                   ENDIF
085100980130     C*
085200980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
085300980130     C*  IMMESSO DA VIDEO
085400980130     C                   Z-ADD     G02DAT        V10DSP
085500980130     C*
085600980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
085700980130     C                   MOVE      V10DSP        WAAS              4 0
085800980202     C                   MOVE      G02INV        WMGS              4 0
085900980203     C                   Z-ADD     G02INV        WDTCAR            8 0
086000001120     C                   MOVEL     G02INV        WAMSPE            6 0
086100980130     C*
086200980130     C*
086300980130     C* CONTROLLO DATA SPEDIZIONE CHE SOSTITUISCE LA VECCHIA----------*
086400980130     C*
086500980130     C                   Z-ADD     V11DSP        G02DAT
086600980130     C                   MOVEL     *BLANK        G02ERR
086700980130     C                   CALL      'XSRDA8'
086800980130     C                   PARM                    WLBDAT
086900980130     C* ERRATA
087000980130    1C     G02ERR        IFEQ      '1'
087100980130     C                   SETON                                        47  90
087200980130     C                   GOTO      ENDCTR
087300980130    1C                   ENDIF
087400980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
087500980130     C*  IMMESSO DA VIDEO
087600980130     C                   Z-ADD     G02DAT        V11DSP
087700980202     C* CONTROLLO CHE LE DUE DATE SIANO UGUALI
087800980202     C     V11DSP        IFEQ      V10DSP
087900980204     C                   SETON                                        5090
088000980202     C                   GOTO      ENDCTR
088100980202     C                   ENDIF
088200980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
088300980130     C                   MOVE      V11DSP        WAASN             4 0
088400980130     C*
088500140114     c* questo controllo non lo faccio se premuto f8-sosp automatica
088600140114    2C  nkhWAASN         IFNE      WAAS
088700130729     c                   if        W1cksc<>0
088800081203     C                   MOVEL(P)  '3C'          COD
088900130729     C                   MOVEL(p)  W1cksc        KEY
089000081203     C     ktab2         chain     tabel00f                           31
089100081203     C   31              clear                   ds3c
089200081203     C  N31              movel     tbluni        ds3c
089300081203     c                   endif
089400130729     c     W1cksc        ifeq      0
089500090126     c     ntwlnp        andne     'E'
089600130729     c     W1cksc        orgt      0
089700090126     c     §3cfsp        andeq     'S'
089800980130     C                   SETON                                        4790
089900980130     C                   GOTO      ENDCTR
090000081203     c                   endif
090100980130    2C                   ENDIF
090101160107     c* Anche se tecnicamente funziona,
090102160107     c* impedisco sospensione all'indietro a cavallo di anno (anche per sosp.
090103160107     c* automatica)   - per richiesta di Luciano
090104160107     c                   if        waasn<>waas and waasn<waas
090105160107     C                   SETON                                        6290
090106160107     C                   GOTO      ENDCTR
090107160107     c                   endif
090200980130     C*
090300980130     C                   Z-ADD     G02INV        WLPDSP            8 0
090400001120     C                   MOVEL     G02INV        WAMSOS            6 0
090500980130     C* DATE SPEDIZIONE NO > DATA MAX DI SOSPENSIONE
090600980130    2C     WLPDSP        IFGT      DATAPA
090700980130     C                   SETON                                        4990
090800980130     C                   GOTO      ENDCTR
090900980130    2C                   ENDIF
091000980130     C*
091100980130     C     WLPDSP        IFLT      DATEU
091200980130     C                   SETON                                        4890
091300980130     C                   GOTO      ENDCTR
091400980130     C                   ENDIF
091500070612     c* Se immessa la consegna richiesta, controllo che non sia
091600070612     c*  inferiore alla nuova data spedizione
091700070612     c                   if        *in03 and  v1cdcr>0  and wdcr<wlpdsp
091800070612     c                   seton                                        702890
091900070612     c                   movel     msg(9)        v1cmsg
092000070612     c                   goto      endctr
092100070612     c                   endif
092200980203     C*
092300980203     C* TERMINAL DI ARRIVO SOLO DOPO AVER INSERITO LA DATA DI SPEDIZIONE
092400980203    1C     V1CTFA        IFNE      *ZEROS
092500980203     C* VERIFICO SE E' CORRETTO
092600980203     C                   MOVE      '0'           CRISP             1
092700980203     C     'A'           SETLL     AZCAE01L
092800980203     C     'A'           READE     AZCAE01L                               30
092900980203    2C     *IN30         DOWEQ     *OFF
093000980203    3C     V1CTFA        IFEQ      CAETFE
093100980203     C     CAEATB        ANDEQ     *BLANKS
093200980203     C     WDTCAR        ANDGE     CAEDDE
093300980203     C     WDTCAR        ANDLE     CAEDSC
093400980203     C     CAETFP        IFEQ      0
093500980203     C     CAETFP        OREQ      SIMFEL
093600980203     C                   MOVE      '1'           CRISP
093700980203     C                   END
093800980203    3C                   END
093900980203     C     'A'           READE     AZCAE01L                               30
094000980203    2C                   ENDDO
094100980203     C* ERRORE
094200980203    2C     CRISP         IFEQ      '0'
094300980203     C                   SETON                                        9043
094400980203     C                   MOVE      *BLANKS       V1DTFA
094500980203     C                   GOTO      ENDCTR
094600980203   X2C                   ELSE
094700980203     C* DECODIFICA
094800980203     C     V1CTFA        CHAIN     AZORG01L                           30
094900980203     C  N30              MOVEL     ORGDES        V1DTFA
095000980203    2C                   END
095100980203    1C                   END
095200980130     C*
095300130327     ***C  NKF
095400130327     ***CANNKH              SETON                                        90
095500130711     C**  NKF              SETON                                        90
095600980130     C*
095700980130     C     ENDCTR        ENDSR
095800070612     C*****************************************************************
095900070612     C* controllo dati consegna richiesta      ----------------------
096000070612     C*****************************************************************
096100070612     C     CtrCONSRICH   BEGSR
096200070612      * tipo
096300070612    1c                   If        V1cTcr <> *Blanks   and
096400070612     c                             v1cDcr = *Zeros
096500070612     c                   Eval      *In28 = *On
096600070612     c                   Eval      *In70 = *On
096700070612     c                   Eval      *In90 = *On
096800070612     c                   Eval      v1cMsg = msg(6)
096900070612     c                   GoTo      EndCons
097000070612    1c                   EndIf
097100070612      * data
097200070614     c                   clear                   wdcr
097300070612    1c                   If        V1cDcr > *Zeros
097400070612      * controllo formale della data
097500070612     c                   Clear                   wlbdat
097600070612     c                   Z-Add     V1cDcr        G02Dat
097700070612     c                   Call      'XSRDA8'
097800070612     c                   Parm                    wlbdat
097900070612    2c                   If        G02Err = '1'
098000070612     c                   Eval      *In28 = *On
098100070612     c                   Eval      *In70 = *On
098200070612     c                   Eval      *In90 = *On
098300070612     c                   Eval      v1cMsg = msg(7)
098400070612     c                   GoTo      EndCons
098500070612    2c                   EndIf
098600070612     c
098700070612     c                   Movel     G02Dat        V1cDcr
098800070612     c                   Movel     G02Inv        wdcr
098900070612      * Non deve essere inferiore/uguale a oggi
099000070612    2c                   If        wdcr <= dateu
099100070612     c                   Eval      *In28 = *On
099200070612     c                   Eval      *In85 = *On
099300070612     c                   Eval      *In90 = *On
099400070612     c                   Eval      v1cMsg = msg(7)
099500070612     c                   GoTo      EndCons
099600070612    2c                   EndIf
099700070612     c
099800070612      * Non deve essere superiore a oggi + 5 gg.
099900070612    2c                   if        v1ctcr = 'P'
100000070612     c                   z-add     woggidcgm1    s_woggidcg
100100070612     c                   z-add     §vpodcgm1     s_vpodcg
100200070612     c                   else
100300070612     c                   z-add     woggidcgm2    s_woggidcg
100400070612     c                   z-add     §vpodcgm2     s_vpodcg
100500070612    2c                   endif
100600070612    2c                   If        wdcr > s_woggidcg
100700070612     c                   Eval      *In28 = *On
100800070612     c                   Eval      *In70 = *On
100900070612     c                   Eval      *In90 = *On
101000070612     c                   Eval      v1cMsg = msg(8)
101100070612     c                   Eval      v1cMsg = %trim(v1cmsg) + ' ' +
101200070612     c                             %trim(%editc(s_vpodcg:'Z'))  +
101300070612     c                             ' giorni dalla data odierna'
101400070612     c                   GoTo      EndCons
101500070612    2c                   EndIf
101600070612    1c                   EndIf
101700070614     c* Ora richiesta
101800070614     c                   if        v1chcr>0
101900070614     c                   movel     v1chcr        whcr              2 0
102000070614     c                   move      v1chcr        wmcr              2 0
102100070614    2C     WHCR          IFLT      0
102200070614     C     WHCR          ORGT      24
102300070614     C                   MOVEL     msg(10)       V1cMSG
102400070614     C                   SETON                                        719028
102500070614     C                   GOTO      ENDCons
102600070614    2C                   ENDIF
102700070614     C*
102800070614    2C     WMCR          IFLT      0
102900070614     C     WMCR          ORGT      59
103000070614     C                   MOVEL     msg(10)       V1cMSG
103100070614     C                   SETON                                        719028
103200070614     C                   GOTO      ENDCons
103300070614    2C                   ENDIF
103400070614    2C                   ENDIF
103500070612     c
103600070612     c     ENDCONS       ENDSR
103700980130     C*****************************************************************
103800980130     C* ELABORAZIONE TESTATA E DETTAGLIO BOLLE ----------------------
103900980130     C*****************************************************************
104000980130     C     MOD01         BEGSR
104100980130     C*
104200980130     C                   SETOFF                                       515253
104300110704     C                   SETOFF                                       5455
104400081205     c                   clear                   werror
104500081210     c                   clear                   wmsg
104600980130     C* CONTROLLO CHE TIPO DI SELEZIONE E' STATA FATTA
104700980130     C* PER CLIENTE
104800130729     C     W1CKSC        COMP      *ZEROS                             5151
104900980130     C* PER LINEA PARTENZA
105000980130     C     V1CLNP        COMP      *ZEROS                             5252
105100980130     C* PER TERMINAL DI ARRIVO
105200980130     C     V1CTFA        COMP      *ZEROS                             5353
105300980130     C* PER LINEA ARRIVO
105400061003     C     WCLNA         COMP      *ZEROS                             54
105500980130     C* PER PADRONCINO
105600061003     C     WCPDR         COMP      *ZEROS                             55
105700020610
105800020610      * se richiesta una linea di partenza imposto solo quella linea di
105900020610      * partenza nella sk delle linee da stampare
106000020610     c                   if        v1clnp <> *zeros
106100020610     c                   clear                   sklnp
106200020610     c                   eval      sklnp(1) = v1clnp
106300020610     c                   else
106400020610     c                   movea     lin           sklnp
106500020610     c                   endif
106600110704     c
106700060403     c* se richiesto aggiornamento su bolle senza colli spuntati stampo
106800060403     c* testata per elenco spedizioni in sospensione da x giorni
106900060403     c   01              if        v1cnos = 'S'
107000060403     c                   open      fnls03p
107100060404     c                   move      v1cggl        w_gg              1
107200060404     c                   eval      ptitolo = ctitolo
107300060404     c                   eval      ptitolo = %xlate('X':w_gg:ptitolo)
107400090127     c                   eval      *in13='0'
107500130729     c                   if        W1cksc>0
107600090127     c                   eval      *in13='1'
107700090127     c                   endif
107800130729     c                   z-add     W1cksc        p1cccm
107900060404     c                   movel     v1dksc        p1dccm
108000090127     c                   z-add     v1clnp        p1clnp
108100090127     c                   movel     v1dlnp        p1dlnp
108200060403     c                   write     testa
108300060403     c                   endif
108400061005     c
108500061005     c                   open      qsysprt
108600060403     c
108700110704     c* se parzializzazione per numero ORM, leggo per ORM
108800110704     c*  che faccio prima
108900110704    0c                   if        sel4>0
109000110704     C                   MOVEL     SEL4          KN14
109100110704     C     Kar43         SETLL     Fiar404L
109200110704     C     Kar43         reade     Fiar404L
109300110704    1c                   dow       not %eof(fiar404l)
109400110704     c
109500110704     c* controllo la linea di partenza immessa o dell'area
109600110704     c                   eval      Indx=%lookup(ar4lnp:sklnp)
109700110704     c*
109800110704    2c                   if        Indx>0
109900110704
110000110704     c     kblpm         chain(n)  fnblp01l
110100110704
110200110704     c* Verifico anche la data spedizione
110300110704     c                   if        blpdsp=wdtcar
110400110704     c
110500110704     c                   exsr      Parzial
110600110704     c
110700110704    3c                   if        NoBolla=' '
110800110704    4c                   if        waasn<>waas
110900110704     c                   exsr      sr_nuovonumero
111000110704   x4c                   else
111100110704     c
111200110704     c                   exsr      Aggiorna
111300110704     c*
111400110704    4c                   endif
111500110704    4c                   endif
111600110704    3c                   endif
111700110704    2c                   endif
111800110704     c
111900110704     C     Kar43         reade     Fiar404L
112000110704    1c                   enddo
112100110704      *
112200110704   x0c                   else
112300020610
112400020610      * elaboro la sk delle linee da stampare
112500110704   1ac                   do        30            xx
112600110704    1c                   if        sklnp(xx) = *zeros
112700020610     c                   leave
112800110704    1c                   endif
112900020610
113000020610     c                   eval      lnp = sklnp(xx)
113100980130     C*
113200980130     C* LETTURA TESTATE BOLLE
113300020610     C  N52KBLP1         SETLL     FNBLP70L
113400980130     C   52
113500020610     CANN55KBLP2         SETLL     FNBLP70L
113600020610     C   55KBLP3         SETLL     FNBLP70L
113700110704     c
113800980203    1C                   DO        *HIVAL
113900020610     C  N52KBLP1         READE     FNBLP70L                               30
114000980203     C   52
114100020610     CANN55KBLP2         READE     FNBLP70L                               30
114200020610     C   55KBLP3         READE     FNBLP70L                               30
114300980203     C   30              LEAVE
114400110704     c
114500110704     c                   exsr      Parzial
114600110704     c
114700110704     c                   if        NoBolla='N'
114800110704     c                   iter
114900110704     c                   endif
115000110704     c
115100081203     c* Se cambia anche l'anno richiamo pgm apposito che esegue anche la
115200081203     c* rinumerazione della spedizione
115300110704    2c                   if        waasn<>waas
115400081203     c                   exsr      sr_nuovonumero
115500081203     c                   iter
115600110704    2c                   endif
115700110704     c
115800110704     c                   exsr      Aggiorna
115900980202     C*
116000980202    1C                   ENDDO
116100020610
116200110704   1ac                   enddo
116300110704    0c                   endif
116400060403     c*
116500060403     c   01              if        v1cnos = 'S'
116600060404     c   98              write     testa
116700060404     c                   setoff                                       98
116800060404     c                   write     fines
116900060403     c                   close     fnls03p
117000060403     c                   endif
117100061005     c
117200061005     c                   close     qsysprt
117300060713     c
117400060713     c                   if        bollemsg>0
117500111018     c                   eval      zz=zz+1
117600111018     c                   eval      invbo(zz)=bollemsg
117700130729     c                   if        W1cksc>0
117800130729     c                   eval      invcod(zz)=V1cksc + '-' + v1dksc
117900111018     c                   else
118000111018     c                   eval      invcod(zz)=%editc(v1clnp:'X')+'-'+
118100111018     c                             v1dlnp
118200111018     c                   endif
118300111018     c                   eval      invdim(zz)=savdim
118400111018     c
118500111019     c                   if        zz=50
118600111018     c                   exsr      Inv_email
118700111018     c                   clear                   invbo
118800111018     c                   clear                   invcod
118900111018     c                   clear                   invdim
119000111018     c                   endif
119100060713     c                   endif
119200111018     c
119300060713     c* emetto msg di quante bolle manutenzionate
119400130711     c                   exsr      emis_msg
119500130711
119600060926     c*
119700061003     c                   if        yy<6
119800060926     c                   add       1             yy
119900060926     c                   movel     param         elabor(yy)
120000060926     c                   endif
120100980130     C*
120200980202     C                   ENDSR
120300130711     C*----------------------------------------------------------------
120400130711     c     Emis_msg      BEGSR
120500130711     c****               movel     msg(5)        v1cmsg
120600130711     c****               eval      %subst(v1cmsg:27:11)= (%editc(bollema:'2'))
120700130711     c                   seton                                        28
120800130711    1c                   if        werror>0
120900130711    2c                   if        wmsg=*blanks
121000130711     c***                eval      widms1=%trim(widms1)+ 'ERRORE su '
121100130711     c***                eval      widms1=%trim(Widms1)+ (%editc(werror:'Z'))
121200130711     c****               eval      widms1=%trim(Widms1)+ ' bolle.'
121300130711     c                   eval      widms1=%trim(%editc(werror:'Z')) +
121400130711     c                             ' bolle non aggiornate.'
121500130711     c                   eval      widms2=ms2wdw
121600130711     c                   eval      widms3=ms3wdw
121700130711     c                   eval      widms4=ms4wdw
121800130711   x2c                   else
121900130711     c                   eval      widmsall=wmsg
122000130711    2c                   endif
122100130711    1c                   endif
122200130711    1c                   if        wmsg=*blanks
122300130711     c                   movel     msg(5)        v1cmsg
122400130711    2c                   if        bollema>0
122500130711     c                   eval      v1cmsg=%trim(v1cmsg)+' ' +%editc(bollema:'2')
122600130711   x2c                   else
122700130711     c                   eval      v1cmsg=%trim(v1cmsg)+' '
122800130711     c                             + %editw(bollema:'       0 ')
122900130711    2c                   endif
123000130711     c                   eval      v1cmsg=%trim(v1cmsg)+ ' Bolle.'
123100130711    1c                   endif
123200130711     c                   ENDSR
123300110704     C*----------------------------------------------------------------
123400110704     c     Parzial       BEGSR
123500110704     c                   clear                   Nobolla           1
123600110704      *
123700110704      *  51 CONTROLLO IL CODICE CLIENTE
123800110704     C     BLPCCM        IFNE      *ZEROS
123900110704     C                   Z-ADD     BLPCCM        WKSC              7 0
124000110704     C                   ELSE
124100110704     C                   Z-ADD     BLPKSC        WKSC
124200110704     C                   ENDIF
124300130729    2C   51WKSC          IFNE      W1CKSC
124400110704     c                   if        blplnp=190 or blplnp=191
124500110704     c                   seton                                        80
124600110704     c                   except    det
124700110704     c                   setoff                                       80
124800110704    2C                   END
124900110704     c                   eval      NoBolla='N'
125000110704     C                   leavesr
125100110704    2C                   END
125200110704     c* Se richiesta sospensione a cavallo di anno per una lnp estera
125300110704     c* se il cliente mantiene il numero spedizione la scarto
125400110704     c                   if        not *in51 and ntwlnp='E'
125500110704     c                             and waasn<>waas
125600110704     C                   MOVEL(P)  '3C'          COD
125700110704     C                   MOVEL(p)  wksc          KEY
125800110704     C     ktab2         chain     tabel00f                           31
125900110704     C   31              clear                   ds3c
126000110704     C  N31              movel     tbluni        ds3c
126100110704     c     §3cfsp        ifeq      'S'
126200110704     c                   eval      NoBolla='N'
126300110704     C                   leavesr
126400110704     c                   endif
126500110704     c                   endif
126600110704      *
126700110704      *  53 CONTROLLO IL TERMINAL ARRIVO
126800110704    2C   53BLPTFA        IFNE      V1CTFA
126900110704     c                   if        blplnp=190 or blplnp=191
127000110704     c                   seton                                        81
127100110704     c                   except    det
127200110704     c                   setoff                                       81
127300110704    2C                   END
127400110704     c                   eval      NoBolla='N'
127500110704     C                   leavesr
127600110704    2C                   END
127700110704      *
127800110704      *  54 CONTROLLO LA LINEA DI ARRIVO
127900110704    2C   54BLPLNA        IFNE      WCLNA
128000110704     c                   if        blplnp=190 or blplnp=191
128100110704     c                   seton                                        82
128200110704     c                   except    det
128300110704     c                   setoff                                       82
128400110704    2C                   END
128500110704     c                   eval      NoBolla='N'
128600110704     C                   leavesr
128700110704    2C                   END
128800110704      *
128900110704      ** CONTROLLO CHIUSURA FOGLIO VIAGGIO DELLA BOLLA
129000110704    2C     BLPDT1        IFNE      *ZEROS
129100110704     c                   if        blplnp=190 or blplnp=191
129200110704     c                   seton                                        83
129300110704     c                   except    det
129400110704     c                   setoff                                       83
129500110704    2C                   END
129600110704     c                   eval      NoBolla='N'
129700110704     C                   leavesr
129800110704    2C                   ENDIF
129900110704     c
130000110704     c* Se richiesto, controllo se non ha spunte per tutti i colli della
130100110704     c*  sped
130200110704    1c                   if        v1cnos='S'
130300110704     c                   clear                   wsispu            1
130400110704     c     kblt          setll     fnblt01l
130500110704     c     kblt          reade     fnblt01l                               99
130600110704    2c                   dow       not *in99
130700110704     c
130800110704     c* Cerco una qualsiasi spunta
130900110704     c     kbrv          chain     fnbrv07l
131000110704    3c                   if        %found(fnbrv07l)
131100110704     c                   eval      wsispu='S'
131200110704     c                   seton                                        99
131300110704   x3c                   else
131400110704     c     kblt          reade     fnblt01l                               99
131500110704    3c                   endif
131600110704    2c                   enddo
131700110704     c
131800110704     c*Se non trovata nessuna spunta
131900110704    2c                   if        wsispu='S'
132000110704     c                   if        blplnp=190 or blplnp=191
132100110704     c                   seton                                        84
132200110704     c                   except    det
132300110704     c                   setoff                                       84
132400110704    2C                   END
132500110704     c                   eval      NoBolla='N'
132600110704     c                   leavesr
132700110704    2c                   endif
132800110704    1c                   endif
132900110704     c*
133000110704      * SCARTO I PREPAGATI ANNULLATI
133100110704     c                   if        blpcbo='M '
133200110704     c                   eval      NoBolla='N'
133300110704     c                   leavesr
133400110704     c                   endif
133500110704      *
133600110704      ** CONTROLLO IL TIPO BOLLA
133700110704     C                   MOVEL     '3A'          COD
133800110704     C                   MOVEL(P)  BLPCBO        KEY
133900110704     C     KTAB2         CHAIN     TABEL00F                           31
134000110704     C   31              CLEAR                   DS3A
134100110704     C  N31              MOVEL     TBLUNI        DS3A
134200110704      *
134300110704      * SCARTO LE BOLLE DI RECUPERO
1344001107041   2C     §3ARBL        IFEQ      'R'
134500110704     c                   if        blplnp=190 or blplnp=191
134600110704     c                   seton                                        85
134700110704     c                   except    det
134800110704     c                   setoff                                       85
134900110704    2C                   END
135000110704     c                   eval      NoBolla='N'
135100110704     C                   leavesr
135200110704    2C                   END
135300110704      * SCARTO LE BOLLE con cod trattamento merce che non prevede merce
135400110704     C                   MOVEL     '1B'          COD
135500110704     C                   MOVEL(P)  BLPctm        KEY
135600110704     C     KTAB2         CHAIN     TABEL00F                           31
135700110704     C   31              CLEAR                   DS1b
135800110704     C  N31              MOVEL     TBLUNI        DS1b
135900110704    2C                   IF        §1bfg8='N'
136000110704     c                   if        blplnp=190 or blplnp=191
136100110704     c                   seton                                        85
136200110704     c                   except    det
136300110704     c                   setoff                                       85
136400110704    2C                   END
136500110704     c                   eval      NoBolla='N'
136600110704     C                   leavesr
136700110704    2C                   END
136800110704      *
136900110704      * PER I PREPAGATI NON È CONSENTITO SPOSTARE LA DATA BOLLA AL MESE
137000110704      *  SUCCESSIVO
137100110704     C     WAMSOS        IFGT      WAMSPE
137200110704      *
137300110704    2C     BLPPDR        IFEQ      *ZEROS
137400110704     c                   if        blplnp=190 or blplnp=191
137500110704     c                   seton                                        86
137600110704     c                   except    det
137700110704     c                   setoff                                       86
137800110704    2C                   END
137900110704     c                   eval      NoBolla='N'
138000110704     C                   leavesr
138100110704    2C                   ELSE
138200110704     C                   MOVEL     '1'           KTRC
138300110704     C     KAR6          CHAIN(N)  FIAR6000                           31
138400110704    2C     *IN31         IFEQ      '0'
138500110704    2C     AR6NFT        ANDGT     *ZEROS
138600110704     c                   if        blplnp=190 or blplnp=191
138700110704     c                   seton                                        87
138800110704     c                   except    det
138900110704     c                   setoff                                       87
139000110704    2C                   END
139100110704     c                   eval      NoBolla='N'
139200110704     C                   leavesr
139300110704    2C                   ENDIF
139400110704    2C                   ENDIF
139500110704      *
139600110704    2C                   ENDIF
139700110704     c
139800110704     c                   ENDSR
139900110704     c*------------------------------------------------------------------------
140000110704     c     Aggiorna      BEGSR
140100110704     c* A questo punto visto che sono stati superati tutti i controlli
140200110704     c* chaino il record bolla in update
140300110704     c     kblt          chain(E)  fnblp01l
140400110704    1c                   if        %error
140500110704     C                   CLEAR                   TRUL82DS
140600110705     c                   if        sel4=0
140700110704     c                   eval      ul82§rrn = fnblpnrri
140800110705     c                   else
140900110705     c                   eval      ul82§rrn = fnblpnrr01
141000110705     c                   endif
141100110704     C                   EVAL      UL82§FIL = 'FNBLP00F'
141200110704     C                   EVAL      UL82§WIN = 'N'
141300110704     C                   EVAL      UL82§F7  = 'S'
141400110704     C                   EVAL      UL82§num = 1
141500110704     C                   EVAL      UL82§att = 30
141600110704     C                   EVAL      UL82§mss = msgjob
141700110704     C* Effettuo la chiamata al *PGM d utilità
141800110704     C                   CALL(e)   'TRUL82R'
141900110704     C                   PARM                    TRUL82DS
142000110704     C     kblt          chain     fnblp01l
142100110704    1c                   endif
142200110704    2c                   if        %found(fnblp01l)
142300111214     c
142400111214     c* Mi salvo il terminal della bolla
142500111214     c                   eval      savtfp=blptfp
142600110704      *
142700110704      ** CONTROLLO SE DATA BORDERO' MINORE DELLA NUOVA DATA DI SPEDIZIONE
142800110704      *  SBORDERIZZO
142900110704    2C     BLPDBR        IFLT      WLPDSP
143000110704      *
143100110704      * SBORDERIZZO
143200110704     C                   CLEAR                   BLPNFV
143300110704     C                   CLEAR                   BLPDBR
143400110704     C                   CLEAR                   BLPFLP
143500110704    2C                   ENDIF
143600110704     C* AGGIORNO LA NUOVA DATA SPEDIZIONE
143700110704     C                   Z-ADD     WAASN         BLPAAS
143800110704     C                   Z-ADD     WLPDSP        BLPMGS
143900110704     c* Aggiorno anche la data ritiro merce
144000110704     c*  solo per i clienti che spostanto in sospensione per mancanza di spu
144100110704     c*  nte
144200110704    2c                   if        v1cnos='S'
144300110704     C                   movel     WLPDSP        BLPdrt
144400110704     c* Se richiesto e se presente consegna richiesta, imposto la nuova data
144500110704    3c                   if        *in03 and v1cdcr>0 and blpdcr>0
144600110704     c                   movel     v1ctcr        blptcr
144700110704     c                   movel     wdcr          blpdcr
144800110704     c                   movel     v1chcr        blphcr
144900110704    3c                   endif
145000110704     c
145100110704   x2c                   else
145200110704     c* Se la data ritiro > della data spedizione nuova e non c'e' ORM
145300110704     c*  imposto SEMPRE la data ritiro= data spedizione
145400110704    3c                   if        blpdrt>wlpdsp
145500110704     c     kar4m         chain     fiar401l
145600110704    4c                   if        not %found(fiar401l)
145700110704     c                   eval      blpdrt=wlpdsp
145800110704    4c                   endif
145900110704    3c                   endif
146000110704     c
146100110704    2c                   endif
146200110704     c
146300110704     C* E RICACOLO I TERMINAL DI ARRIVO E FILIALE ELABORAZIONE ARRIVI
146400110704     C                   CLEAR                   DSLV55
146500110704     C                   MOVEL     ' '           D55TPT
146600110704     C                   MOVEL     BLPLNA        D55LIN
146700110704     C                   MOVEL     BLPLNP        D55LNP
146800110704     C                   MOVEL     WLPDSP        D55DRF
146900110704     C                   CALL      'FNLV55R'
147000110704     C                   PARM                    DSLV55
147100110704    2C     D55ERR        IFEQ      ' '
147200110704     C                   MOVEL     D55TFA        BLPTFA
147300110704     C                   MOVEL     D55TFP        BLPFEA
147400110704    2C                   ENDIF
147500110704     C* BLPTFP: TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
147600110704     C                   CLEAR                   DSLV55
147700110704     C                   MOVEL     'P'           D55TPT
147800110704     C                   MOVEL     BLPLNP        D55LIN
147900110704     C                   MOVEL     WLPDSP        D55DRF
148000110704     C                   CALL      'FNLV55R'
148100110704     C                   PARM                    DSLV55
148200111214     c
148300111214     c
148400110704    2C     D55ERR        IFEQ      ' '
148500110704     C                   MOVEL     D55TFP        BLPTFP
148600111128     C                   MOVEL     D55TFP        BLPfle
148700110704    2C                   ENDIF
148800110704     C*
148900110704     C                   UPDATE    FNBLP001
149000110704     c                   add       1             bollema           9 0
149100110704     c
149200110704     c* Se si tratta di assegnato S.F. tassato in partenza, cancello la
149300110704     c* tassazione
149400110704     c                   clear                   ktrc
149500110704    2c                   if        §3atb2<>*blanks
149600110704     c                   eval      ktrc='2'
149700110704   x2c                   else
149800110704     c                   movel     §3atb1        flgtb1            1
149900110704    3c                   if        flgtb1='A'
150000110704     c                   eval      ktrc='1'
150100110704    3c                   endif
150200110704    2c                   endif
150300110704     c
150400110704   1ac                   if        ktrc<>*blanks
150500110704     C     KAR6          CHAIN(N)  FIAR6000                           31
150600110704    2c                   if        not *in31
150700110704     c                   move      ar6ksc        fineksc           4 0
150800110704    3c                   if        fineksc<>0000 and fineksc<>9999
150900110704     c* deleto la tassazione
151000110704     c     kar6          setll     fiar701l
151100110704     c     kar6          reade     fiar701l
151200110704    4c                   dow       not %eof(fiar701l)
151300110704     c                   delete    fiar7000
151400110704     c
151500110704     c     kar6          reade     fiar701l
151600110704    4c                   enddo
151700110704     c
151800110704     c     kar6          delete    fiar6000
151900110704    3c                   endif
152000110704    2c                   endif
152100110704   1ac                   endif
152200110704     c
152300110704     C*
152400110704     C     KBLT          SETLL     FNBLT01L
152500110704     C     KBLT          READE     FNBLT01L                               30
152600121107    2C     *IN30         DOWEQ     *OFF
152700110704     C* SOLO SE NON C'E' GIA' LA DATA DI PARTENZA
152800110704    3C  N30BLTDFV        IFEQ      0
152900110704     C* E LA DATA BORDERO' DELLA TESTATA NON E' MAGGIORE ALLA NUOVA DATA SPED.
153000110704    2C     BLPDBR        ANDLT     WLPDSP
153100110704     C                   CLEAR                   BLTNFV
153200110704     C                   UPDATE    FNBLT000
153300110704   X3C                   ELSE
153400110704     C                   UNLOCK    FNBLT01L
153500110704    3C                   ENDIF
153600121107     C     KBLT          READE     FNBLT01L                               30
153700110704    2C                   ENDDO
153800111214     c
153900110704     C* AGGIORNO ANCHE FISGN SE PRESENTE
154000110704     C     KFISGN        SETLL     FISGN05L
154100110704     C     KFISGN        READE     FISGN05L                               30
154200110704    2C     *IN30         DOWEQ     *OFF
154300110704     C                   Z-ADD     WLPDSP        SGNMGS
154400110704     C                   Z-ADD     BLPTFA        SGNTNA
154500111214     c
154600111214     c* Cambio il terminal solo se corrisponde a quello della bolla
154700111214     c*  se l'ho inviato ad aaltro terminal non modifico
154800130214     c**                 if        sgntnp=savtfp
154900130214     C**                 Z-ADD     BLPTFP        SGNTNP
155000130214     c**                 endif
155100111214     c
155200110704     C                   CLEAR                   SGNT6A
155300110704     C                   CLEAR                   SGNT6B
155400110704     C                   CLEAR                   SGNT6C
155500110704     C                   CLEAR                   SGNT6D
155600110704     C                   CLEAR                   SGNT6E
155700110704     C                   CLEAR                   SGNT6F
155800110704     c                   eval      sgndatora = %char(%timestamp:*iso0)
155900110704     C                   UPDATE    FISGN000
156000110704     C     KFISGN        READE     FISGN05L                               30
156100110704    2C                   ENDDO
156200110704     C*
156300110704     c* richiamo routine per gestione stampe
156400110704     c                   z-add     blpaas        wlpaas
156500110704     c                   z-add     blpnsp        wlpnsp
156600110704     c                   exsr      sr_stampe
156700110704     c*
156800110704    1c                   endif
156900110704     c
157000110704     c                   ENDSR
157100980130     C*----------------------------------------------------------------
157200980130     C*
157300980130     C*--- OPERAZIONI INIZIALI ---------------------------------------*
157400980130     C     *INZSR        BEGSR
157500980130     C***
157600980130     C* KLIST
157700980130     C***
157800980130     C* ACCESSO   CNACO
157900980130     C     KACO          KLIST
158000980130     C                   KFLD                    CODUT
158100980130     C                   KFLD                    CCC
158200130729     C                   KFLD                    W1CKSC
158300151207     C**   Ktbe          KLIST
158400151207     C**                 KFLD                    kcod
158500151207     C***                KFLD                    klin
158600151207     C***                KFLD                    ksif
158700110704     C*
158800980130     C     KBLT          KLIST
158900980130     C                   KFLD                    BLPAAS
159000980130     C                   KFLD                    BLPLNP
159100980130     C                   KFLD                    BLPNRS
159200980130     C                   KFLD                    BLPNSP
159300110704     C* KEY PER AGGIIORNARE FLBLT
159400110704     C     KBLPM         KLIST
159500110704     C                   KFLD                    ar4AAS
159600110704     C                   KFLD                    ar4LNP
159700110704     C                   KFLD                    ar4NRS
159800110704     C                   KFLD                    ar4NSP
159900110704     C* KEY PER AGGIORNARE fiar4
160000080331     C     Kar4M         KLIST
160100080331     C                   KFLD                    BLPAAS
160200080331     C                   KFLD                    BLPLNP
160300080331     C                   KFLD                    BLPNRS
160400080331     C                   KFLD                    BLPNSP
160500110704     C                   KFLD                    ktrcM
160600051209     C     KBrv          KLIST
160700051209     C                   KFLD                    bltfls
160800051209     C                   KFLD                    bltlna
160900051209     C                   KFLD                    bltNRS
161000051209     C                   KFLD                    bltnsc
161100001120     C* KEY PER AGGANCIARE  FIAR6
161200001120     C     KAR6          KLIST
161300001120     C                   KFLD                    BLPAAS
161400001120     C                   KFLD                    BLPLNP
161500001120     C                   KFLD                    BLPNRS
161600001120     C                   KFLD                    BLPNSP
161700001120     C                   KFLD                    KTRC              1
161800980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
161900980130     C     KBLP1         KLIST
162000020610     c                   kfld                    lnp
162100980130     C                   KFLD                    WAAS
162200980130     C                   KFLD                    WMGS
162300980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 2
162400980130     C     KBLP2         KLIST
162500020610     c                   kfld                    lnp
162600980130     C                   KFLD                    WAAS
162700980130     C                   KFLD                    WMGS
162800980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 3
162900980130     C     KBLP3         KLIST
163000020610     c                   kfld                    lnp
163100980130     C                   KFLD                    WAAS
163200980130     C                   KFLD                    WMGS
163300980130     C                   KFLD                    WCPDR
163400010326     C* ACCESSO   FILE FISGN PER NUMERO SPEDIZIONE
163500010326     C     KFISGN        KLIST
163600010326     C                   KFLD                    BLPAAS
163700010326     C                   KFLD                    BLPLNP
163800010326     C                   KFLD                    BLPNRS
163900010326     C                   KFLD                    BLPNSP
164000980130     C* ACCESSO   TABEL
164100980130     C     KTAB2         KLIST
164200980130     C                   KFLD                    CODUT
164300980130     C                   KFLD                    COD               2
164400980130     C                   KFLD                    KEY               8
164500980202     C* ACCESSO   AZCLN
164600980202     C     KCLN          KLIST
164700980202     C                   KFLD                    SIMFEL
164800980202     C                   KFLD                    W0030             3 0
164900980202     C                   KFLD                    WAA               4 0
165000980202     C                   KFLD                    WMM               2 0
165100021203     C     Kfapd         klist
165200021203     C                   kfld                    kpdtip
165300021203     C                   kfld                    kpdpdr
165400110704      *
165500110704     C     KFORM         KLIST
165600110704     C                   KFLD                    V1CPOE
165700110704     C                   KFLD                    V1CNSR
165800110704     C                   KFLD                    V1CNOR
165900110704     C                   KFLD                    V1CNRV
166000110704      *
166100110704     C     Kar43         KLIST
166200110704     C                   KFLD                    KTRCM
166300110704     C                   KFLD                    KN14             14
166400110704     C*
166500980130     C***
166600980130     C* DEFINIZIONE CAMPI
166700980130     C***
166800980130     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
166900980130     C***
167000130729     c     *dtaara       define    §azute        AZUTEds
167100130729     c     *dtaara       define    §datiute      dDATIUTE
167200130729     c                   in(E)     *dtaara
167300130729     c                   if        %ERROR or RSUT = *blanks
167400130729     c                   clear                   Tibs34Ds
167500130729     c                   call      'TIBS34R'
167600130729     c                   parm                    Tibs34Ds
167700130729     c                   in        *dtaara
167800130729     c                   endif
167900130729      *
168000130729     c                   z-add     1             CodUt             1 0
168100980202     C***
168200980202     C* CARICO DATI VARIABILI PARTENZE
168300980202     C***
168400980202     C                   MOVEL     '3I'          COD
168500980202     C                   MOVEL     '1       '    KEY
168600980202     C     KTAB2         CHAIN     TABEL00F                           31
168700980202     C  N31              MOVEL     TBLUNI        DS3I
168800980202     C   31              CLEAR                   DS3I
168900980202     C***
169000980202     C* CARICO CODICI CLIENTI SDI PER I QUALI INSERISCO OBBLIGATORIAM
169100980202     C*  NUMERO AWB CON CONTROLLO CHECK DIGIT
169200980202     C***
169300980202     C                   MOVEL     '3I'          COD
169400980202     C                   MOVEL     'E       '    KEY
169500980202     C     KTAB2         CHAIN     TABEL00F                           31
169600980202     C  N31              MOVEA     TBLUNI        KSS
169700980202     C* 2 CIFRE INIZIALI FISSE PER NUMERO AWB
169800980202     C  N31              MOVE      TBLUNI        §3IFAW            2 0
169900980202     C   31              CLEAR                   KSS
170000980202     C   31              CLEAR                   §3IFAW
170100980130     C*
170200980130     C* DATA BOLLETTAZIONE:  TRA UDATE E DATA GIORNO + §3IGGS
170300980130     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
170400980130     C                   TIME                    W0140            14 0
170500980130     C* UDATE IN GGMMAAAA
170600980130     C                   MOVE      W0140         WDTGIO            8 0
170700060404     C                   MOVEl     W0140         utime
170800980130     C*
170900980130     C* UDATE IN AAAAMMGG
171000980130     C                   Z-ADD     WDTGIO        G02DAT
171100980130     C                   MOVEL     *BLANK        G02ERR
171200980130     C                   CALL      'XSRDA8'
171300980130     C                   PARM                    WLBDAT
171400980130     C                   MOVEL     G02INV        DATEU             8 0
171500980130     C* SE NON FOSSE PERMESSO BOLLETTARE IN SOSPENSIONE
171600980130     C*  SI PUO' BOLLETTARE SOLO FINO A OGGI
171700980130     C                   Z-ADD     G02INV        DATAPA            8 0
171800980130     C                   MOVEL     G02DAT        DATAPG            8
171900980130     C*
172000980130     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
172100130729     C                   Z-ADD     1             X                 2 0
172200980130    1C     X             DOWLE     §3IGGS
172300980130     C*
172400980130     C                   MOVEL     'F'           WFEST             1
172500980130     C*
172600980130    2C     WFEST         DOWNE     ' '
172700980130     C     G02TGI        ADD       1             GIOTGI
172800980130     C                   ADD       1             G02TGI
172900980130     C                   CALL      'XSRGI8'
173000980130     C                   PARM                    WGIDAT
173100980130     C*
173200980130     C                   Z-ADD     GIOINV        DATAPA            8 0
173300980130     C                   MOVEL     GIODAT        DATAPG            8
173400980130     C*
173500980130     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
173600980130     C                   Z-ADD     GIOINV        WDATA
173700980130     C                   CLEAR                   W0030
173800120302     C* CHAIN SUL CALENDARIO BRT
173900980130     C     KCLN          CHAIN     AZCLN01L                           30
174000980130     C   30              CLEAR                   CLNPOM
174100980130     C                   MOVEA     CLNPOM        K31
174200980130     C                   MOVEL     K31(WGG)      WFEST
174300980130    2C                   ENDDO
174400980130     C*
174500980130     C                   ADD       1             X
174600980130    2C                   ENDDO
174700070612     C* reperisco da tabella vpo il numero giorni da sommare a udate per
174800070612     c* determinare data max per data consegna richiesta
174900070612     C                   CLEAR                   tibs02ds
175000070612     C                   MOVEL     'C'           T02MOD
175100070612     C                   MOVEL     KNSIF         T02SIF
175200070612     C                   MOVEL     'VPO'         T02COD
175300070612     C                   MOVEL(p)  'VPO'         T02KE1
175400070612     C                   CALL      'TIBS02R'
175500070612     C                   PARM                    KPJBA
175600070612     C                   PARM                    tibs02ds
175700070612    4C     T02ERR        IFEQ      *BLANKS
175800070612     C                   MOVEL     T02UNI        DVPO
175900070612     C                   ELSE
176000070612     C                   CLEAR                   DVPO
176100070612    4C                   ENDIF
176200130327     c*
176300070612     c                   Move      Dateu         Dataiso
176400070612     c                   Adddur    §vpodcgm1:*d  Dataiso
176500070612     c                   Move      Dataiso       woggidcgm1
176600070612     c*
176700070612     c                   Move      Dateu         Dataiso
176800070612     c                   Adddur    §vpodcgm2:*d  Dataiso
176900070612     c                   Move      Dataiso       woggidcgm2
177000130327      *
177100980130     C***
177200980130     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
177300980130     C***
177400980130     C                   CLEAR                   DSUL06
177500980130     C                   MOVE      '£1'          D06COD
177600980130     C                   MOVEL     SIMFEL        D06KEY
177700980130     C                   MOVEL     'L'           D06TLA
177800980130     C                   MOVEL     DSUL06        KPJBU
177900980130     C*
178000980130     C                   CALL      'TRUL06R'
178100980130     C                   PARM                    KPJBA
178200980130     C                   MOVEL     KPJBU         DSUL06
178300980130     C                   MOVEA     LIN           L1
178400130327     c*
178500090126     c* Verifico se il £1 c'e' almeno un p.o. estero
178600151207     c***                clear                   poestero          1
178700151207     c***                z-add     1             x
178800151207     c***                dow       L1(x)<>0
178900151207     c***  l1(x)         chain     azorg01l
179000151207     c***                if        %found(azorg01l)
179100151207     c***                movel     orgde3        og143
179200151207     c***                if        §ogntw='DPD' or §ogntw='EEX' or
179300151207     c***                          §ogntw='FED'
179400151207     c***                eval      poestero='S'
179500151207     c***                leave
179600151207     c***                endif
179700151207     c***                endif
179800151207     c***                add       1             x
179900151207     c***                enddo
180000980130     C***
180100020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
180200980130     C***
180300130729     C                   Z-ADD     dutPOU        CDU               3 0
180400130729     C     DUTlpo        IFEQ      '2'
180500130729     C     DUTlpo        OREQ      *BLANKS
180600020429     C                   SETON                                        12
180700020429     C                   ENDIF
180800130327     c*
180900111018     c                   clear                   elabor
181000130327     c*
181100111018     c                   Clear                   tibs02ds
181200111018     c                   Clear                   dmradan
181300111018     c                   Movel     'C'           T02Mod
181400111018     c                   Movel     KNSIF         T02Sif
181500111018     c                   Movel     'MRA'         T02Cod
181600111018     c                   Movel     'FNLS03R'     T02Ke1
181700111018     c                   Call      'TIBS02R'
181800111018     c                   Parm                    Kpjba
181900111018     c                   Parm                    tibs02ds
182000111018    2c                   If        T02Err = *Blanks
182100111018     c                   Movel     T02Uni        Dmradan
182200111018    2c                   endif
182300001120     C*
182400980130     C                   ENDSR
182500980130     C*****************************************************************
182600980130     C* PULIZIA   FORMATO -------------------------------------------
182700980130     C*****************************************************************
182800980130     C     PUL01         BEGSR
182900980130     C*
183000070614     c                   setoff                                       0103
183100130729     C                   MOVE      *ZEROS        W1CKSC
183200130729     C                   clear                   V1CKSC
183300980130     C                   MOVE      *BLANKS       V1DKSC
183400980130     C                   MOVE      *BLANKS       V1CLNP
183500980130     C                   MOVE      *BLANKS       V1DLNP
183600980130     C                   MOVE      *ZEROS        V1CTFA
183700980130     C                   MOVE      *BLANKS       V1DTFA
183800980130     C                   MOVE      *BLANKS       V1CLNA
183900980130     C                   MOVE      *BLANKS       V1DLNA
184000980130     C                   MOVE      *BLANKS       V1CPDR
184100980130     C                   MOVE      *BLANKS       V1DPDR
184200980130     C                   Z-ADD     *ZEROS        V10DSP
184300980130     C                   Z-ADD     *ZEROS        V11DSP
184400070612     C                   CLEAR                   v1cdcr
184500070612     C                   CLEAR                   v1ctcr
184600070612     C                   CLEAR                   v1chcr
184700070612     C                   CLEAR                   wdcr
184800051209     C                   move      *blanks       V1cnos
184900060403     C                   z-add     3             V1cggl
185000060713     c                   clear                   bollemsg
185100060713     c                   clear                   bollema           9 0
185200110704     c                   clear                   v1cpoe
185300110704     c                   clear                   v1cnsr
185400110704     c                   clear                   v1cnor
185500110704     c                   clear                   v1cnrv
185600111017     c                   clear                   savdim            8 0
185601151207
185700151207     c* Abilitazione sospensione per colli non spuntati
185701151207
185702151207     c     ' '           setll     fibsp03l
185703151207     c     ' '           reade     fibsp03l
185704151207     c                   dow       not %eof(fibsp03l) and *in01=*off
185705151207     c* record per lnp
185706151207     c                   if        bspfil>0
185707151207     c                   if        %lookup(bspfil:l1)>0
185708151207     c                   eval      SosdaL1='S'
185709151207     c                   seton                                        01
185710151207     c                   endif
185711151207     c                   else
185712151207     c* record per cliente
185713151207     c                   z-add     1             Ix
185714151207     c                   dow       Ix<=%elem(sbsp) and sbsp(Ix)>0
185715151207     c                             and *in01 = *off
185716151207     c                   if        %lookup(sbsp(Ix):L1)>0
185717151207     c                   eval      SosdaL1='S'
185718151207     c                   seton                                        01
185719151207     c                   endif
185720151207     c                   add       1             Ix
185721151207     c                   enddo
185722151207     c                   endif
185723151207     c     ' '           reade     fibsp03l
185724151207     c                   enddo
185800090126     c* Abilito l'opzione se c'e' un p.o. estero in £1
185900151207     c***                if        poestero='S'
186000151207     c***                seton                                        01
186100151207     c***                eval      SosdaL1='S'
186200151207     c****               else
186300130327     c*
186400070903     c* Abilito l'opzione solo se almeno un cliente è abilitato
186500151207     c***                eval      SosdaL1=' '
186600151207     c***                eval      ksif=knsif
186700151207     c***  ktbe          setll     tntbe02l
186800151207     c***                if        not %equal(tntbe02l)
186900151207     c***                clear                   ksif
187000151207     c***  ktbe          setll     tntbe02l
187100151207     c***                endif
187200130327     c*
187300151207    1c***                if        %equal(tntbe02l)
187400151207     c***  ktbe          reade     tntbe02l
187500151207   1ac***                dow       not %eof(tntbe02l) and not *in01
187600151207    2c***                if        tbeatb=' '
187700151207     c***                movel     tbeuni        dbsp
187900151207     c***                movel     tbeke1        lnpabi            3 0
188300130327     c*
188500151207     c***                if        lnpabi>0
188600151207     c***  lnpabi        lookup    l1                                     30
188700151207    5c***                if        *in30
188800151207     c***                eval      SosdaL1='S'
188900151207     c***                seton                                        01
189000151207    5c***                endif
189100151207     c***                endif
189400151207    2c***                endif
189500130327     c*
189600151207     c***  ktbe          reade     tntbe02l
189700151207   1ac***                enddo
189800151207    1c***                endif
189900130327     c*
190000151207    1c***                endif
190100130327     c*
190200080624     c*********          if        not *in01 and (cdu=118 or cdu=107)
190300080624     c*********          eval      *in01=*on
190400080624     c*********          endif
190500060403     c*
190600060403     c* se 01 acceso effettuo ovrprtf per mettere le stampe in save
190700060403     c                   if        *in01
190800060403     c                   clear                   qcmd
190900060403     c                   eval      qcmd = $cmd2(1)
191000060403     c                   call      'QCMDEXC'                            91
191100060403     c                   parm                    Qcmd
191200060403     c                   parm                    Qlen
191300060403     c                   endif
191400130327     c*
191500061005     c                   clear                   qcmd
191600061005     c                   eval      qcmd = $cmd3(1)
191700061005     c                   call      'QCMDEXC'                            91
191800061005     c                   parm                    Qcmd
191900061005     c                   parm                    Qlen
192000980130     C*
192100980130     C                   ENDSR
192200030619
192300130327     *** *---------------------------------------------------------------*
192400130327     *** * Richiedo i dati per la stampa   ed   Eseguo le override       *
192500130327     *** *---------------------------------------------------------------*
192600130327     ***c     SR_OvrPrtF    BEGSR
192700130327     *** *
192800130327     ***c                   call      'TRUL90R'
192900130327     ***c                   parm                    KPJBA
193000130327     ***c                   parm                    TRUL90DS
193100130327     *** * - F3 > Fine
193200130327     ***c     D90F3         cabeq     *on           End_Ovr
193300130327     *** *
193400130327     *** * - FNLSB5PA4 > bolle laser in formato A4
193500130327     ***c                   eval      wFILE = 'FNLSB5PA4 '
193600130327     ***c                   eval      wOUTQ = D90prb4
193700130327     ***c                   eval      wFORM = D90mdb4
193800130327     ***c                   exsr      SR_x_Cmd
193900130327     ***c     *in91         cabeq     *on           End_Ovr
194000130327     *** *
194100130327     *** * - FNLSB5PA5 > bolle laser in formato A5
194200130327     ***c                   eval      wFILE = 'FNLSB5PA5 '
194300130327     ***c                   eval      wOUTQ = D90prb5
194400130327     ***c                   eval      wFORM = D90mdb5
194500130327     ***c                   exsr      SR_x_Cmd
194600130327     ***c     *in91         cabeq     *on           End_Ovr
194700130327     *** *
194800130327     ***c     End_Ovr       ENDSR
194900130327     ***
195000130327     *** *---------------------------------------------------------------*
195100130327     ***
195200130327     ***c     SR_x_Cmd      BEGSR
195300130327     *** *
195400130327     *** * impostazione comando da eseguire
195500130327     ***c                   eval      Qcmd  = %trim($Cmd(1)) + ' '
195600130327     ***c                                   + %trim($Cmd(2))
195700130327     *** * - nome del file
195800130327     ***c                   eval      Qcmd = %replace(wFILE:Qcmd:
195900130327     ***c                                    %scan('&FILE':Qcmd))
196000130327     *** * - nome della coda di stampa
196100130327     ***c                   eval      Qcmd = %replace(wOUTQ:Qcmd:
196200130327     ***c                                    %scan('&OUTQ':Qcmd))
196300130327     *** * - tipo di modulo
196400130327     ***c                   eval      Qcmd = %replace(wFORM:Qcmd:
196500130327     ***c                                    %scan('&FORMTYPE':Qcmd))
196600130327     *** *
196700130327     ***c                   call      'QCMDEXC'                            91
196800130327     ***c                   parm                    Qcmd
196900130327     ***c                   parm                    Qlen
197000130327     *** *
197100130327     ***c                   ENDSR
197200130327
197300081203      *---------------------------------------------------------------*
197400081203
197500081203     c     SR_stampe     BEGSR
197600130327      *
197700130327     ***C* PER CMD8 STAMPO LA BOLLA
197800130327     ***C     *INKH         IFEQ      *ON
197900130327     ***C     BLPFST        ANDNE     'N'
198000130327     ***c                   clear                   FNLSB5DS
198100130327     ***C                   MOVEL     'V'           DB0RIS
198200130327     ***C                   MOVEL     'P'           DB0TBO
198300130327     ***C                   Z-ADD     wLPAAS        DB0AAS
198400130327     ***C                   Z-ADD     BLPLNP        DB0LNP
198500130327     ***C                   Z-ADD     BLPNRS        DB0NRS
198600130327     ***C                   Z-ADD     wLPNSP        DB0NSP
198700130327     ***c                   call      D90psl
198800130327     ***c                   parm                    FNLSB5DS
198900130327     ***c                   parm                    FNLSB6ds1
199000130327     ***C                   ENDIF
199100081203     c*
199200081203     c* GESTIONE STAMPA SPEDIZIONE
199300081203     c                   if        v1cnos='S'
199400081203     c                   if        v1cggl > 0
199500081203     c* calcolo giorni lavorativi fra data spedizione e data immissione
199600081203     c                   clear                   wdat8
199700081203     c                   z-add     blpdim        datada
199800081203     c                   z-add     wlpdsp        dataa
199900081203     c                   call      'XSRLAV8'
200000081203     c                   parm                    wdat8
200100081203     c                   if        (ggl-1) > v1cggl
200200081203     c                   if        blpfns = 'S'
200300081203     c                   seton                                        02
200400081203     c                   else
200500081203     c                   setoff                                       02
200600081203     c                   endif
200700081203     c* inverto data ritiro per stampa
200800081203     c     *iso          move      blpdim        data_eur
200900081203     c                   move      data_eur      p1cdim
201000081203     c   98              write     testa
201100081203     c                   setoff                                       98
201200081203     c                   write     rigsped
201300081203     c                   endif
201400081203     c                   endif
201500081203     C**
201600081203     c** Se passati xx giorni di sospensione, invio msg al CED
201700081203     c                   if        (ggl-1) > §3iggm
201800081203     c                   add       1             bollemsg          9 0
201900111017     c* memorizzo la data più bassa di immissione
202000111017     c                   if        savdim=0 or blpdim<savdim
202100111017     c                   eval      savdim=blpdim
202200111017     c                   endif
202300111017     c
202400081203     c                   endif
202500081203     c                   endif
202600081203     c                   endsr
202700081203      *---------------------------------------------------------------*
202800081203
202900081203     c     SR_nuovonumeroBEGSR
203000081203     c                   clear                   fnls65ds
203100081203     c                   z-add     waas          I65AAS
203200081203     c                   z-add     blplnp        I65LNP
203300081203     c                   z-add     blpnrs        I65NRS
203400081203     c                   z-add     blpnsp        I65NSP
203500081203     c                   z-add     waasn         I65AASN
203600081203     c                   z-add     wlpdsp        I65MGSN
203700081203     c                   if        v1cnos='S'
203800081203     c                   movel     'S'           I65AGDRT
203900081203     c                   if        *in03 and v1cdcr>0 and blpdcr>0
204000081203     c                   movel     'S'           I65AGDCR
204100081203     c                   move      v1ctcr        I65TCRN
204200081203     c                   move      wdcr          I65DCRN
204300081203     c                   move      v1chcr        I65HCRN
204400081203     c                   endif
204500081203     c                   endif
204600081203     c                   move      '1'           I65STRCMT
204700081203     c                   move      '1'           I65FCMT
204800081203     c                   move      '1'           I65CMTRLBK
204900081203     c                   movel     fnls65ds      kpjbu
205000081210     c                   call      'FNLS65R'
205100081203     c                   parm                    kpjba
205200081203     c                   movel     kpjbu         fnls65ds
205300081203     c                   if        o65err=*blanks
205400081203     c                   add       1             bollema           9 0
205500081205     c                   z-add     waasn         wlpaas
205600081205     c                   z-add     o65nsp        wlpnsp
205700081203     c                   exsr      sr_stampe
205800081204     c                   else
205900081205     c                   add       1             werror            6 0
206000081210     c                   if        o65err='1'
206100081210     c                   movel     o65msg        wmsg
206200081210     c                   endif
206300081203     c                   endif
206400081203     c                   endsr
206500111018     c*-------------------------------------------------------------------
206600111018     c     INV_email     BEGSR
206700111018     c
206800111018     c                   if        not %open(prtemail)
206900111018     C                   EXSR      SR_OPENPRTF
207000111018     C                   endif
207100111018     c
207200111018     c                   exsr      Routend
207300111018     c
207400111018     c                   ENDSR
207500111018
207600111018      /free
207700111018       //--------------------------------------------------------------
207800111018       //?Override al file di stampa per impostarvi i dati per
207900111018       //?  l'invio via e-mail   +   stampa inizio e-mail
208000111018       //--------------------------------------------------------------
208100111018       BEGSR sr_OpenPrtF;
208200111018
208300111018         // Override al file di stampa per impostarvi i dati per
208400111018         // l'invio via e-mail
208500111018           exsr sr_Override;
208600111018
208700111018           open PrtEMAIL;
208800111018
208900111018         // Stampa una testata se NON è richiesta la e-mail
209000111018           IF  §MRAdreg =  *blank;
209100111018             O_testo = JobUser + ' - ' + SDSpgm
209200111018                     + ' - ' + %editc( *date : 'Y' )
209300111018                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
209400111018             except PrtDet;
209500111018             clear O_testo;
209600111018             except PrtDet;
209700111018             except PrtDet;
209800111018           ENDIF;
209900111018
210000111018         // Stampa testo iniziale
210100111018           O_testo = 'E'' stata manutenzionata la DATA SPEDIZIONE +
210200111019                      di bolle immesse da almeno xx giorni.' ;
210300111019           %subst(O_testo:71:2) = (%editc(§3iggm: '4'))     ;
210400111018           except PrtDet;
210500111019           O_testo = 'Verificare col mittente o procedere alla lor+
210600111019                      o chiusura come "Merce MAI Affidata".' ;
210700111019           except PrtDet;
210800111018
210900111018         // Stampa una riga vuota
211000111018           clear O_testo;
211100111018           except PrtDet;
211200111018
211300111018         // Stampa intestazione
211400111019           O_testo = ' Bolle spostate in sospensione di          Data imm+
211500111019                     issione    Bolle da vericare elencate';
211600111018           except PrtDet;
211700111019           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
211800111019                     chia       in apposita stampa.Utente:';
211900111019           %subst(o_testo:90:10)= knmus     ;
212000111019           except PrtDet;
212100111018
212200111018         // Stampa una riga vuota
212300111018           clear O_testo;
212400111018           except PrtDet;
212500111019           zz = 1 ;
212600111019
212700111019          dow     invbo(zz)>0   ;
212800111019          dataiso=%date(invdim(zz))  ;
212900111019          data_eur=dataiso  ;
213000111019
213100111019           clear O_testo;
213200111019           %subst(O_testo:6:20) = invcod(zz)  ;
213300111019           %subst(O_testo:46:10) =%char(data_eur)  ;
213400111019           %subst(O_testo:64:7 ) =%editc(invbo(zz):'2')  ;
213500111019           except PrtDet;
213600111019
213700111019           zz=zz+1  ;
213800111019           enddo    ;
213900111018
214000111018       ENDSR;
214100111018
214200111018       //--------------------------------------------------------------
214300111018       //?Override al file di stampa per impostarvi i dati per
214400111018       //?  l'invio via e-mail   +   stampa inizio e-mail
214500111018       //--------------------------------------------------------------
214600111018       BEGSR sr_Override;
214700111018
214800111018         reset TRTCM1ds;
214900111018
215000111018         IF  §MRAdreg <> *blank;
215100111018           §CM1mitt = %trim(§MRAdmitt);
215200111018           §CM1dst  = %trim(§MRAddest);
215300111019
215400111019         // Valorizzo i campi della e-m@ail
215500130729          chain  DUTpou   azorg01l ;
215600111019
215700111019          if   %found(azorg01l) ;
215800140205           og140=orgde0  ;
215900140205           if §ogapo>*zeros  ;
216000140205           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
216100140205           else  ;
216200140205            §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
216300111019          endif  ;
216400140205          endif  ;
216500111019
216600111018           §CM1tips = §MRAdreg;
216700111018           §CM1po   = %editc(simfel:'X');
216800111019           §CM1var  = '*OBJM*' + 'Manutenzione DATA SPEDIZIONE bolle'   ;
216900111018           §CM1idp  = §MRAdidpro;
217000111018           Qcmd = C_CmdOvrPrtF
217100111018                + ' outq(' + %trim(§MRAdoutqi) + ')'
217200111018                + ' usrdfndta(''' + TRTCM1ds + ''')';
217300111018         ELSE;
217400111018           Qcmd = C_CmdOvrPrtF;
217500111018         ENDIF;
217600111018
217700111018         callp(e) qCmdExc(Qcmd : %size(Qcmd));
217800111018
217900111018       ENDSR;
218000111018
218100130718       //--------------------------------------------------------------
218200151204       //?Cerca cliente in FIBSP
218300130718       //--------------------------------------------------------------
218400130718       BEGSR cer_bsp;
218500151204       //ksif=knsif;
218600151204       //setll (kcod: klin: ksif) tntbe02l;
218700151204       //if not %equal(tntbe02l);
218800151204       //   clear ksif;
218900151204       //   setll (kcod: klin: ksif) tntbe02l;
219000151204       //endif;
219100151204       //if %equal(tntbe02l);
219200151204       //   reade (kcod: klin: ksif) tntbe02l;
219300151204       //   dow not %eof(tntbe02l) and wbsp=' ';
219400151204       //      if tbeatb=' ' and tbeke2=v1cksc;
219500151204       //         wbsp='S';
219600151204       //         dbsp=tbeuni;
219700151204       //      endif;
219800151204       //      reade (kcod: klin: ksif) tntbe02l;
219900151204       //   enddo;
220000151204       //endif;
220001151204
220002151207       chain w1Cksc fibsp02l;
220003151209       if %found(fibsp02l);
220004160111         if bspfpa=*blanks or bspaut='N';
220005151204          wbsp='S';
220006151209         else;
220007160111          wbspchf='S';
220008151209         endif  ;
220009151204       endif;
220100130718       endsr;
220200111018       //--------------------------------------------------------------
220300111018       //?Operazioni finali.
220400111018       //--------------------------------------------------------------
220500111018       BEGSR RoutEnd;
220600111018
220700111018         if %open(PrtEMAIL);
220800111018
220900111018         //?Chiusura dello spool?
221000111018           clear O_testo;
221100111018           except PrtDet;
221200111018           O_testo = '***   Fine Lista   ***';
221300111018           except PrtDet;
221400111018
221500111018           close PrtEMAIL;
221600111018
221700111018         //?Eliminazione overflow?
221800111018           Qcmd = C_CmdDltOvr;
221900111018           callp(e) qCmdExc(Qcmd : %size(Qcmd));
222000111018
222100111018         endif;
222200111018
222300111018       ENDSR;
222400111018
222500111018      /end-free
222600111018     c*-------------------------------------------------------------------
222700061117     Oqsysprt   E    80n80   DET         1  1
222800061005     o                                              'Bolla:'
222900061005     O                       BLPaas        Z   +  1
223000061005     O                       BLPlnp            +  1
223100061005     O                       BLPNRS        Z   +  1
223200061005     O                       BLPNSP        Z   +  1
223300061005     O                       BLPmgs            +  1 '  /  '
223400061005     o               80                          48 'Cliente    <>'
223500061005     O               80      wksc              +  1
223600061005     o               81                          48 'Terminal   <>'
223700061005     o               82                          48 'Lin Arrivo <>'
223800061005     o               83                          48 'BLPDT1  <>  0'
223900061005     o               84                          48 'Esiste spunta'
224000061005     o               85                          49 'Bolla recupero'
224100061005     o               86                          48 'Prepagato    '
224200061005     o               87                          55 'Prepagato con numfat'
224300030619
224400111018     o* ---------------------------------------------------------------
224500111018     o* ?Spool di stampa (per e-mail).
224600111018     o* ---------------------------------------------------------------
224700111018     oPrtEMAIL  e            PRTdet      1
224800111018     o                       O_testo
224900111018
225000030619**  $Cmd
225100030707OVRPRTF FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
225200030619           USRDTA('Manut.Boll') SHARE(*YES)
225300030619           COPIES(2)
225400060403**  $Cmd2
225500060403OVRPRTF FILE(FNLS03P) SAVE(*YES)
225600061005**  $Cmd3
225700061005OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES) OUTQ(LJ4200CED) USRDTA('EDPES')
225800061012**  DLTOVR
225900061012DLTOVR  FILE(QSYSPRT)
226000060713**         MSG
226100111017MANUTENZIONE DATA SPED.BOLLE oltre xx giorni
226200111018Utente:  xxxxxxxxxx
226300060713Codice Cliente:  xxxxxxx   x
226400060713Bolle manutenzionate: xxx.xxx.xxx
226500081210Sono state manutenzionate
226600070612Se immesso tipo immettere anche la data  Consegna Richiesta
226700070612Data cons.richiesta formalmente errata o inferiore/uguale alla data odierna    4
226800070612Data consegna richiesta non ammessa: oltre                                     5
226900070612Data cons.richiesta  inferiore  alla nuova data spedizione
227000070614Ora consegna richiesta errata
227100090401Linea di partenza:  xxx
227200110704Immettere filiale di emissione valida                                         12
227300110704Immettere numero ORM esistente in archivio                                    13
227400111017Bolle immesse dal                                                             14
227500130711Se richiesta la sospensione automatica, non effettuare alcuna parzializzaz.   15
227600130729Ammessi solo caratteri numerici                                               16
