000100130313     /*PRM dbgview(*source)
000200130313     /*END
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400980129     H* FNLS03R *----------------------------------------------------*
000500980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000600111122     H***------------------------------------------------------------*
000700980129     FFNLS03D   CF   E             WORKSTN
000800060404     FFNLS03p   o    E             printer usropn  oflind(*in98)
000900910207     FAZORG01L  IF   E           K DISK
001000051212     Ftntbe02l  IF   E           K DISK
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
008500130726     d tntbBSPds1    e ds                  inz
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
016000130830     D  olsu3msg             117    194
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
020100021203
020200021203     D kpdpdr          S                   LIKE(APDpdr)
020300021203     D kpdtip          S                   LIKE(APDtip)
020400130717     D kcod            S                   LIKE(tbecod) inz('BSP')
020500051212     D klin            S                   LIKE(tbelin) inz(' ')
020600051212     D ksif            S                   LIKE(tbesif)
020700110704     D ktrc            S                   LIKE(ar4trc)
020800110704     D ktrcM           S                   LIKE(ar4trc) inz('M')
020900081205     D wlpnsp          S                   LIKE(blpnsp)
021000081205     D wlpaas          S                   LIKE(blpaas)
021100081210     D wmsg            S                   LIKE(v1cmsg)
021200090126     D ntwlnp          S              1    inz
021300060926     D YY              S              3  0 inz
021400111214     D savtfp          S              3  0 inz
021500080620     D sosdaL1         S              1    inz
021600110704     D Indx            S              3  0 inz
021700111018     d O_testo         s            132    inz
021800060713     D* DEFINIZIONE COSTANTI
021900060404     D Ctitolo         C                   CONST('ELENCO SPEDIZIONI IN SOSPENSI-
022000060405     D                                     ONE DA OLTRE X GIORNI')
022100081203     D msgjob          C                   CONST('Si sta bloccando la manutenzi-
022200081203     D                                     one data spedizione:USCIRE un moment-
022300081203     D                                     o da questo lavoro')
022400130111     D*ms2wdw          C                   CONST('Probabilmente in manutenzione-
022500130111     D*                                       ')
022600130111     D*ms3wdw          C                   CONST('da altro utente.             -
022700130111     D*                                       ')
022800130111     D*ms4wdw          C                   CONST('Rieseguire il lancio.        -
022900130111     D*                                       ')
023000130111     D ms2wdw          C                   CONST('                             -
023100130111     D                                        ')
023200130111     D ms3wdw          C                   CONST('Segnalare al CED di verificar-
023300130111     D                                     e  ')
023400130111     D ms4wdw          C                   CONST('il JOBLOG                    -
023500130111     D                                        ')
023600130729
023700130729       // -?Costante per controllo "caratteri solo numerici"?
023800130729     d c_Digits        c                   const('0123456789')
023900111018
024000111018      // - Comando di override al PrtF
024100111018     d C_CmdOvrPrtF    c                   const('OVRPRTF +
024200111018     d                                           file(PRTEMAIL) +
024300111018     d                                           pagesize(66 132) +
024400111018     d                                           lpi(6) cpi(10) +
024500111018     d                                           ovrscope(*actgrpdfn) +
024600111018     d                                           ')
024700111018     d C_CmdDltOvr     c                   const('DLTOVR +
024800111018     d                                            file(PRTEMAIL) +
024900111018     d                                            lvl(*actgrpdfn)')
025000111018      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
025100111018     d TRTCM1ds      e ds                  inz
025200111018      //   ·§CM1mitt = Indirizzo e-mail del mittente
025300120820     d   §CM1mitt    e                     inz(' ed@brt.it')
025400111018      //   ·§CM1dst  = Indirizzo e-mail del destinatario
025500120302     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
025600111018      //   ·§CM1tips = Tipo lettera via e-mail:
025700111018      //    "LET" = testo allegato in corpo con logo
025800111018      //            (richiede righe libere iniziali per il logo)
025900120820      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
026000111018      //            (non consente né UNDERLINE né HIGHLIGHT)
026100120820     d   §CM1tips    e                     inz('COI')
026200111018      //   ·§CM1po   = Filiale
026300111018     d   §CM1po      e                     inz('046')
026400111018      //   ·§CM1var  = Oggetto e-mail
026500111018     d   §CM1var     e                     inz('*OBJM*+
026600111019     d                                     Manutenzione data spedizione+
026700111019     d                                      bolle ')
026800111018      //   ·§CM1sts  = Stato
026900111018     d   §CM1sts     e                     inz(*off)
027000111018      //   ·§CM1sts  = Id processo
027100111018     d   §CM1idp     e                     inz('2')
027200111018
027300111018
027400910412     C****************************************************************
027500910412     C*  RIEPILOGO INDICATORI
027600910412     C***************************************************************
027700070612     C* 01    - possibilità di richidere bolle in sospensione x colli non
027800070612     c*         spuntati
027900070612     C* 02    -
028000070612     C* 03    - per colli non spuntati possibilità di modificare la consegna
028100130717     c*         richiesta da tabella BSP
028200070612     C* 12    - PROTEZIONE CAMPO LINEA DI PARTENZA XCHE' SONO UN 2° LIVELLLO
028300910412     C* 30    - DI COMODO
028400980129     C* 40-50 - ERRORE
028500980130     C* 51-55 - GESTIONE CHIAVE LETTURA BOLLE
028600070612     C* 70    - posizionam.cursone su DCR
028700070612     C* 90    - ERRORE GENERICO
028800910412     C*****************************************************************
028900941116     C**
029000980130     C     *ENTRY        PLIST
029100980130     C                   PARM                    KPJBA
029200030619      *
029300130327     *** * Richiedo i dati per la stampa ed eseguo le override
029400130327     ***c                   exsr      SR_OvrPrtF
029500130327     ***C**
029600130327     ***C* F3 - FINE
029700130327     ***C     D90F3         IFEQ      '1'
029800130327     ***C                   GOTO      FINE
029900130327     ***C                   ENDIF
030000130327     *** * Errori nelle OVERRIDE -> FINE
030100130327     ***C   91              GOTO      FINE
030200991206     C*
030300950411     C     INIZIO        TAG
030400980130     C                   EXSR      PUL01
030500950411     C*
030600980130     C                   MOVE      WDTGIO        WAAS              4 0
030700980130     C                   MOVEL     CDU           V1CLNP
030800950411     C**
030900900509     C     FOR01         TAG
031000980129     C                   EXFMT     LS03D01
031100941227     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
031200110704     C                   SETOFF                                       902873
031300110704     C                   SETOFF                                       707172
031400130711     C                   SETOFF                                       74
031500081210     c                   clear                   v1cmsg
031600130711     C** F3 - FINE LAVORO
031700900509     C   KC              GOTO      FINE
031800130711     c
031900130726     c* F11-  seleziona  tabella sospensione senza codici senza spunte
032000130711     c*       tabella DSP
032100130711     c                   if        *inkk=*on
032200130726     ***c                   Clear                   tibs02ds
032300130726     ***c                   Eval      T02Mod = 'R'
032400130726     ***c                   Eval      T02Sif = Knsif
032500130726     ***c                   Eval      T02Cod = 'BSP'
032600130726     ***c                   Call      'TIBS02R'
032700130726     ***c                   Parm                    Kpjba
032800130726     ***c                   Parm                    tibs02ds
032900130729      * Sola visualizzazione di TUTTI i clienti (di qualsiasi filiale)
033000130729      *   in tab. "BSP"
033100130726     c                   clear                   tntbBSPds1
033200130731     c                   eval      iBSPtfp = DUTtfp
033300130726     c                   eval      KPJBU = tntbBSPds1
033400130726     c                   call      'TNTBBSPR1'
033500130726     c                   parm                    KPJBA
033600130726     c                   eval      tntbBSPds1 = KPJBU
033700130711     c                   goto      for01
033800130711     c                   endif
033900130711     c
034000910205     C* CONTROLLI FORMATO
034100980130     C                   EXSR      CNT01
034200980202     C  NKF
034300130711     CANNKH
034400941228     COR 90              GOTO      FOR01
034500130711     c
034600130711     c                   if        *inkh=*on
034700130711     c                   clear                   fnlsu3ds
034800130711     c                   eval      ilsu3dsp_B=%editc(wdtcar:'X')
034900130711     c                   eval      ilsu3dsp_S=%editc(wlpdsp:'X')
035000130711     c                   eval      ilsu3nos='S'
035100130711     c                   movel     fnlsu3ds      kpjbu
035200130711     C                   MOVEL     'LSU3'        KCOAZ
035300130711     C                   CALL      'FNLSU3R'
035400130711     C                   PARM                    KPJBA
035500130711     c*
035600130711     c                   movel     kpjbu         fnlsu3ds
035700130711
035800130711     c                   if        olsu3boagg>*zeros
035900130711     c                   movel     olsu3boagg    bollema
036000130830     c                   endif
036100130830     c                   if        olsu3bonagg>*zeros
036200130830     c                   movel     olsu3bonagg   werror
036300130830     c                   endif
036400130830     c                   eval      wmsg=olsu3msg
036500130711     c                   exsr      emis_msg
036600130830
036700130830     c                   if        werror>0
036800130830     c                   do        *hival
036900130830     c                   exfmt     ls03w01
037000130830     c                   if        *inkl='1'
037100130830     c                   leave
037200130830     c                   endif
037300130830     c                   enddo
037400130830     c                   endif
037500130711     c
037600130711     c                   goto      inizio
037700130711e   1C                   ENDIF
037800900523     C*
037900980129     C* M O D I F I C A   D A T A   S P E D I Z I O N E
038000130327     ***C   KH
038100130327     ***COR KF              EXSR      MOD01
038200130327     C   KF              EXSR      MOD01
038300130711     c
038400081211     c                   if        werror>0
038500081211     c                   do        *hival
038600081211     c                   exfmt     ls03w01
038700081215     c                   if        *inkl='1'
038800081211     c                   leave
038900081211     c                   endif
039000081211     c                   enddo
039100081211     c                   endif
039200081211
039300910207     C*
039400950411     C                   GOTO      INIZIO
039500000000     C     FINE          TAG
039600980202     C**
039700130327     ***C     D90F3         IFne      '1'
039800130327if  1***c                   if        DB6num > *zero
039900130327     ***c                   clear                   FNLSB5ds
040000130327     ***c                   eval      DB0tbo = 'P'
040100130327     ***c                   eval      DB0fa4 = 'I'
040200130327     ***c                   eval      DB6pdf = 'S'
040300130327     ***c                   call      D90psl
040400130327     ***c                   parm                    FNLSB5ds
040500130327     ***c                   parm                    FNLSB6ds1
040600130327e   1***c                   endif
040700130327     ***c                   clear                   FNLSB5DS
040800130327     ***c                   movel     'C'           DB0TLA
040900130327     ***c                   call      D90psl
041000130327     ***c                   parm                    FNLSB5DS
041100130327     ***c                   endif
041200980202     C*
041300980202     C                   CLEAR                   DSLV55
041400980202     C                   MOVEL     'C'           D55TLA
041500980202     C                   CALL      'FNLV55R'
041600980202     C                   PARM                    DSLV55
041700081203     c*
041800081203     C                   CLEAR                   fnls65ds
041900081203     C                   MOVEL     'C'           i65tla
042000081211     c                   movel     '1'           I65STRCMT
042100081203     c                   movel     fnls65ds      kpjbu
042200081205     C                   CALL      'FNLS65R'
042300081203     C                   PARM                    kpjba
042400980202     C**
042500060926     c                   clear                   kpjbu
042600060926     c                   movea     elabor        kpjbu
042700060926     c                   if        kpjbu<>*blanks
042800060926     C                   MOVEL     'LS0G'        KCOAZ
042900060926     C                   CALL      'BCH10'
043000060926     C                   PARM                    KPJBA
043100060926     c                   endif
043200060926     c
043300061012     c
043400061012     c                   clear                   qcmd
043500061012     c                   eval      qcmd = dltovr(1)
043600061012     c                   call      'QCMDEXC'                            91
043700061012     c                   parm                    Qcmd
043800061012     c                   parm                    Qlen
043900061012     c
044000111018     c                   if        invbo(1)>0
044100111018     c                   exsr      Inv_email
044200111018     c                   endif
044300111018     c
044400000000     C                   SETON                                        LR
044500941116     C*
044600980129     C*****************************************************************
044700910205     C* CONTROLLO FORMATO -------------------------------------------
044800980129     C*****************************************************************
044900980130     C     CNT01         BEGSR
045000910205     C                   SETOFF                                       90
045100130729      *
045200130729     c                   clear                   W1Cksc
045300130729     c                   Select
045400130729      * - Ricerca da tab. "BSP"
045500130729     c                   When      %scan( '?' : V1Cksc ) > *zero
045600130729     c                   clear                   V1Cksc
045700130729     c                   clear                   tntbBSPds1
045800130729     c                   eval      KPJBU = tntbBSPds1
045900130729     c                   call      'TNTBBSPR1'
046000130729     c                   parm                    KPJBA
046100130729     c                   eval      tntbBSPds1 = KPJBU
046200130729     c                   if        oBSPfxx <= *zero  and
046300130729     c                             oBSPerr  = *off
046400130729     c                   eval      W1Cksc = oBSPcli
046500130729     c                   eval      V1Cksc = %editc( oBSPcli : 'X' )
046600130729     c                   eval      V1Dksc = oBSPdes
046700130729     c                   endif
046800130729     c                   eval      *in74 = *on
046900130729     c                   eval      *in90 = *on
047000130729     c                   leavesr
047100130729      * - Verifica se contiene solo caratteri numerici
047200130729     c                   When      V1Cksc <> *blanks  and
047300130729     c                             %checkR( c_Digits : V1Cksc ) > *zero
047400130729     c                   movel     Msg(16)       V1Cmsg
047500130729     c                   eval      *in28 = *on
047600130729     c                   eval      *in74 = *on
047700130729     c                   eval      *in90 = *on
047800130729     c                   leavesr
047900130729     c                   EndSl
048000130729      *
048100130729     c                   if        V1Cksc > *zeros
048200130729     c                   eval      W1Cksc = %int( V1Cksc )
048300130729     c                   endif
048400130729      *
048500130711     c* Se ho premuto F8 tutte le selezioni ddevono essere vuote
048600130711     c*  tranne le date
048700130711     c                   if        *inkh=*on
048800130711     c
048900130729     c                   if        W1cksc>*zeros or
049000130711     c                             v1clna>*zeros or
049100130711     c                             v1ctfa>*zeros or
049200130711     c                             v1cpdr >*zeros or
049300130711     c                             sel4>0
049400130711     C                   MOVEL     MSG(15)       V1CMSG
049500130711     C                   SETON                                        287490
049600130711     C                   GOTO      ENDCTR
049700130711     C                   ENDIF
049800130729     c*
049900130711     c                   else
050000910412     C*
050100980129     C* SE TUTTE LE SELEZIONI SONO A BLANKS E' ERRORE
050200130729     C     W1CKSC        IFEQ      *ZEROS
050300980130     C     V1CLNP        ANDEQ     *ZEROS
050400980129     C                   SETON                                        4090
050500941116     C                   GOTO      ENDCTR
050600941116     C                   ENDIF
050700051118     c
050800980129     C* CODICE CLIENTE
050900130729    1C     W1CKSC        IFEQ      0
051000980130    2C     V1DKSC        IFNE      *BLANKS
051100980129     C* PARSTA = 9  ESCLUDO ANNULLATI
051200980129     C                   Z-ADD     9             PARSTA
051300980129     C                   MOVEL     RSUT          PARDUT           30
051400980129     C                   MOVEL     *BLANKS       DESCR            48
051500980130     C                   MOVEL     V1DKSC        DESCR
051600130729     C                   Z-ADD     dutKCI        CCC               4 0
051700981109     C                   CLEAR                   PARFLR
051800981109     C                   CLEAR                   PARDIT
051900000907     C                   Z-ADD     1             PAXNUM
052000000907     C                   CALL      'XALFA3BR'
052100980129     C                   PARM                    PARDUT
052200980129     C                   PARM                    CODUT
052300980129     C                   PARM                    DESCR
052400980129     C                   PARM                    CCC
052500980129     C                   PARM                    PARSTA            1 0
052600981109     C                   PARM                    PARFLR           90
052700981109     C                   PARM                    PARDIT            3
052800000907     C                   PARM                    PAXNUM            2 0
052900000907     C                   PARM                    PAXKCM           80
053000000907     C                   PARM                    PAXKSM          140
053100000907     C                   PARM                    PAXKDM           60
053200980129     C     PARSTA        IFNE      -1
053300130729     C                   MOVEL     PAXKSM        W1CKSC
053400000907     C                   MOVEL     PAXKSM        V1CKSC
053500980130     C                   MOVEL     DESCR         V1DKSC
053600980129     C                   ELSE
053700980202     C                   CLEAR                   V1DKSC
053800980129     C                   ENDIF
053900980129     C*
054000980129     C                   SETON                                        90
054100980129     C                   GOTO      ENDCTR
054200980129    2C                   ENDIF
054300980129   X1C                   ELSE
054400980129     C*
054500980129     C* CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
054600130729     C                   Z-ADD     dutKCI        CCC
054700130729     c                   mllzo     1             W1cksc
054800980129     C     KACO          CHAIN     CNACO000                           41
054900980129    2C  N41ACOFLG        IFNE      ' '
055000980129     C                   SETON                                        41
055100980129    2C                   ENDIF
055200980129     C     *IN41         IFEQ      *OFF
055300980202     C                   MOVEL     ACORAG        V1DKSC
055400980129     C                   ELSE
055500980204     C                   CLEAR                   V1DKSC
055600980129     C                   SETON                                        90
055700980129     C                   GOTO      ENDCTR
055800980129     C                   END
055900980129    1C                   ENDIF
056000130729     c*
056100980130     C* LINEA DI PARTENZA
056200061003     c                   mllzo     1             v1clnp
056300090126     c                   clear                   ntwlnp
056400980130    1C     V1CLNP        IFNE      *ZEROS
056500980130     C* CONTROLLO CHE SIA GESTIBILE
056600980130     C     V1CLNP        LOOKUP    L1                                     30
056700980130    2C     *IN30         IFEQ      *OFF
056800980130     C                   SETON                                        9042
056900980204     C                   CLEAR                   V1DLNP
057000980130     C                   GOTO      ENDCTR
057100980130    2C                   ENDIF
057200980130     C*
057300090126     c                   clear                   og143
057400980130     C     V1CLNP        CHAIN     AZORG01L                           30
057500980130     C   30              SETON                                        9042
057600980204     C   30              CLEAR                   V1DLNP
057700980130     C   30              GOTO      ENDCTR
057800980130     C  N30              MOVEL     ORGDES        V1DLNP
057900090126     c  n30              movel     orgde3        og143
058000090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
058100090126     c                             §ogntw='FED'
058200090126     c                   eval      ntwlnp='E'
058300090126     c                   endif
058400980130    1C                   ENDIF
058500980130     C* LINEA DI ARRIVO
058600061003     c                   clear                   wclna
058700980130    1C     V1CLNA        IFNE      *BLANKS
058800980130     C     V1CLNA        ANDNE     *ZEROS
058900980130     C*
059000980130     C     '?'           SCAN      V1CLNA                                 30
059100980130     C*
059200980130    2C     *IN30         IFEQ      *ON
059300980130     C                   MOVE      *BLANKS       §COD1
059400980130     C                   MOVE      *BLANKS       V1CLNA
059500980130     C                   CALL      'TNSD19R'
059600980130     C                   PARM                    §COD1             3
059700980130     C                   PARM                    §TIP              1
059800980202     C                   PARM                    §DES             25
059900980130     C                   MOVEL     §COD1         V1CLNA
060000980130     C                   MOVEL     §DES          V1DLNA
060100980130     C                   SETON                                        90
060200980130     C                   GOTO      ENDCTR
060300980130    2C                   ENDIF
060400980130     C* ESEGUO TESTN
060500980130     C                   TESTN                   V1CLNA               30
060600980130     C                   MOVE      V1CLNA        W001A
060700980130     C     *IN30         IFEQ      *OFF
060800980130     C     W001A         ORLT      '0'
060900980204     C                   CLEAR                   V1DLNA
061000980130     C                   GOTO      ENDCTR
061100980130     C                   ENDIF
061200980130     C*
061300980130     C*  CONTROLLO
061400980130     C*
061500980130     C                   MOVE      V1CLNA        WCLNA             3 0
061600980130     C     WCLNA         CHAIN     AZORG01L                           30
061700980205     C* CONTROLLO SE ANNULLATO
061800980205     C* CONSIDERO SOLO I TIPI "F" E "A"
061900980205    2C  N30ORGFVA        IFNE      *BLANKS
062000980205    2C     ORGFAG        ORNE      'F'
062100980205     C     ORGFAG        ANDNE     'A'
062200980205     C                   SETON                                        30
062300980205    2C                   END
062400980130     C   30              MOVEL     *BLANKS       V1DLNA
062500980130     C   30              SETON                                        9044
062600980130     C   30              GOTO      ENDCTR
062700980130     C  N30              MOVEL     ORGDES        V1DLNA
062800980130    1C                   ENDIF
062900980130     C*
063000980130     C* CONTROLLO CODICE AUTOTRASPORTATORE
063100061003     c                   clear                   wcpdr
063200980130    1C     V1CPDR        IFNE      *BLANKS
063300980130     C     V1CPDR        ANDNE     *ZEROS
063400980130     C                   CLEAR                   V1DPDR
063500980130     C*
063600980130     C     '?'           SCAN      V1CPDR                                 30
063700980130     C*
063800980130     C     *IN30         IFEQ      *ON
063900021203     C*                  MOVEL     *BLANKS       PA2DES
064000021203     C*                  Z-ADD     0             PA2PAD
064100021203     C*                  Z-ADD     V1CLNP        PA2FIL
064200021203     C*                  MOVEL     'R'           PA2FLG
064300021203     C*                  MOVEL     PARAM2        KPJBU
064400021203      *
064500021203      * imposto i campi della ds esterna fnlv24ds
064600021203     C                   clear                   fnlv24ds
064700021203     C                   eval      d24fil   = v1clnp
064800021203     C                   eval      d24tip   = 'A'
064900021203     C                   eval      d24flg   = 'R'
065000021203     C                   movel     fnlv24ds      kpjbu
065100980130     C                   CALL      'FNLV24R'
065200980130     C                   PARM                    KPJBA
065300021203     C*                  MOVEL     KPJBU         PARAM2
065400021203     C*                  MOVE      PA2PAD        V1CPDR
065500021203     C*                  MOVEL     PA2DES        V1DPDR
065600021203      *
065700021203      * valorizzo i campi a video
065800021203     C                   movel     kpjbu         fnlv24ds
065900021203     C                   move      d24pdr        v1cpdr
066000021203     C                   movel     d24rsc        v1dpdr
066100980130     C                   GOTO      ENDCTR
066200980130     C                   END
066300980130     C* ESEGUO TESTN
066400980130     C                   TESTN                   V1CPDR               30
066500980130     C                   MOVE      V1CPDR        W001A             1
066600980130     C     *IN30         IFEQ      *OFF
066700980130     C     W001A         ORLT      '0'
066800980205     C                   CLEAR                   V1CPDR
066900980205     C                   CLEAR                   V1DPDR
067000980130     C                   GOTO      ENDCTR
067100980130     C                   ENDIF
067200980130     C*
067300021204     C                   CLEAR                   V1DPDR
067400980130     C                   MOVE      V1CPDR        WCPDR             7 0
067500021203     C*    WCPDR         CHAIN     FNAPD000                           30
067600021204     C                   move      wcpdr         kpdpdr
067700021203     C                   move      'A'           kpdtip
067800021203     C     kfapd         chain     fiapd01l                           30
067900980130     C     *IN30         IFEQ      *ON
068000980205     C                   CLEAR                   WCPDR
068100980205     C                   CLEAR                   V1DPDR
068200980130     C                   SETON                                        4590
068300980130     C                   GOTO      ENDCTR
068400980204     C                   CLEAR                   V1DPDR
068500980204     C                   ELSE
068600980204     C                   MOVEL     APDRSC        V1DPDR
068700980130     C                   ENDIF
068800980130     C*
068900980205     C* RECUPERO LA LINEA DI PARTENZA DALLA PRIMA PARTE DELL'AUTOTRASPORTATORE
069000980130     C     V1CLNP        IFEQ      *ZEROS
069100980202     C                   MOVEL     WCPDR         V1CLNP
069200090126     c                   clear                   ntwlnp
069300090126     c                   clear                   og143
069400980130     C     V1CLNP        CHAIN     AZORG01L                           30
069500980130     C  N30              MOVEL     ORGDES        V1DLNP
069600090126     C  N30              MOVEL     ORGDE3        og143
069700090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
069800090126     c                             §ogntw='FED'
069900090126     c                   eval      ntwlnp='E'
070000090126     c                   endif
070100980204     C   30              MOVEL     *BLANKS       V1DLNP
070200980205     C                   ELSE
070300111018inzsrC* CONTROLLO LA LINEA DI PARTENZA CON QUELLA DELL'AUTOTRASPORATTORE
070400980205     C                   MOVEL     WCPDR         W0030
070500980205     C     V1CLNP        IFNE      W0030
070600980205     C     W0030         ANDNE     *ZEROS
070700980205     C                   SETON                                        56  90
070800980205     C                   Z-ADD     *ZEROS        W0030
070900980205     C                   GOTO      ENDCTR
071000980205     C                   ENDIF
071100980202     C                   ENDIF
071200980205     C                   ENDIF
071300051118
071400110704     c* controllo numero ORM
071500110704     c                   if        sel4>0
071600110704      * P.O. EMISSIONE
071700110704     C                   Z-ADD     V1CPOE        NUM3              3 0
071800110704     C     NUM3          CHAIN     AZORG01L                           30
071900110704     C     *IN30         IFEQ      *ON
072000110704    2C     ORGFVA        orne      *BLANKS
072100110704    2C     ORGFAG        ORNE      'F'
072200110704     C     ORGFAG        ANDNE     'A'
072300110704     C                   MOVEL     MSG(12)       V1CMSG
072400110704     C                   SETON                                        287290
072500110704     C                   GOTO      ENDCTR
072600110704     C                   ENDIF
072700110704      * N.ORM
072800110704     C     KFORM         CHAIN     FNORM01L                           30
072900110704     C     *IN30         IFEQ      *ON
073000110704     C                   MOVEL     MSG(13)       V1CMSG
073100110704     C                   SETON                                        287390
073200110704     C                   GOTO      ENDCTR
073300110704     C                   ENDIF
073400110704      *
073500110704     C                   SETON                                        06
073600110704      *
073700110704     C                   ENDIF
073800051118     c* Se immessa solo lnp, immettere anche cliente o linea arrivo o
073900051118     c*  terminal arrivo o autotrasportatore
074000051118     c                   if        v1clnp>0 and
074100130729     c                             (W1cksc=0 and v1clna<=*zeros and v1ctfa=0
074200110704     c                              and v1cpdr<=*zeros and v1cnos=' ' and
074300110704     c                              sel4=0)
074400051118     C                   SETON                                        6090
074500051118     C                   GOTO      ENDCTR
074600051118     C                   ENDIF
074700051209     c
074800080620     c                   if        not *in01
074900080620     c                   clear                   v1cnos
075000080620     c                   endif
075100080620     c
075200080620     c* Per mettere in sospensione  in base alle spunte devo essere
075300130717     c*  abilitato dalla  tabella   BSP
075400051209     C
075500130718     c                   clear                   dbsp
075600130718     c                   clear                   Wbsp
075700130729    1c                   if        W1cksc>0
075800130718     c***                Clear                   tibs02ds
075900130718     c***                Movel     'C'           T02Mod
076000130718     c***                Movel     KNSIF         T02Sif
076100130718     c***                Movel     'CLI'         T02Cod
076200130729     c***                Movel     W1Cksc        T02Ke1
076300130718     c***                Call      'TIBS02R'
076400130718     c***                Parm                    Kpjba
076500130718     c***                Parm                    tibs02ds
076600130718    2c***                If        T02Err = *Blanks
076700130718     c***                Movel     T02Uni        Dcli
076800130718    2c**                 endif
076900130718     c* cerco il cliente in tabella bsp
077000130718     c                   exsr      cer_bsp
077100080620    1c                   endif
077200080620
077300130718     c                   if        not *in01 and wbsp='S'
077400080620     c                   seton                                        01
077500080620     C                   SETON                                        90
077600080620     C                   GOTO      ENDCTR
077700080620     c                   endif
077800130718     c                   if        sosdaL1=' 'and *in01 and wbsp =' '
077900080620     c                   setoff                                       01
078000080620     C                   SETON                                        90
078100080620     C                   GOTO      ENDCTR
078200080620     c                   endif
078300051209     c
078400080620    1c                   if        v1cnos='S'
078500130729    2c                   if        W1cKSC=0 and ntwlnp<>'E'
078600080620     C                   SETON                                        5790
078700080620     C                   GOTO      ENDCTR
078800080620    2C                   ENDIF
078900080620     c
079000130718    2c                   if        wbsp=' ' and ntwlnp<>'E'
079100051209     C                   SETON                                        58  90
079200051209     C                   GOTO      ENDCTR
079300070614   x2c                   else
079400060403     c* se numero giorni per stampa = 0 --> errore
079500070614    3c                   if        v1cggl = 0
079600060403     C                   SETON                                        59  90
079700070614    3c                   endif
079800070612     c* se posso emetto il campo della consegna richiesta
079900130718    3c                   if        §bspsod='S' and not *in03
080000070612     c                   seton                                        0390
080100070612     c                   clear                   v1ctcr
080200070612     c                   clear                   v1cdcr
080300070612     c                   clear                   v1chcr
080400070612     C                   GOTO      ENDCTR
080500070614    3c                   endif
080600070612     c
080700130718    3c                   if        §bspsod=' ' and *in03
080800070612     c                   seton                                        90
080900070612     c                   setoff                                       03
081000070612     c                   clear                   v1ctcr
081100070612     c                   clear                   v1cdcr
081200070612     c                   clear                   v1chcr
081300070612     C                   GOTO      ENDCTR
081400070614    3c                   endif
081500070612     c* Altrimenti controllo la consegna richiesta
081600130718    3c                   if        §bspsod='S'
081700070612     c                   exsr      CtrCONSRICH
081800070614    4c                   if        *in90
081900070612     c                   goto      ENDCTR
082000070614    4c                   endif
082100070614    3c                   endif
082200070612     c
082300070614    2C                   ENDIF
082400070614   x1c                   else
082500070614     c* se cliente senza abilitazione bolla tolgo la consegna rich
082600070614    3c                   if        *in03
082700070614     c                   seton                                        90
082800070614     c                   setoff                                       03
082900070614     c                   clear                   v1ctcr
083000070614     c                   clear                   v1cdcr
083100070614     c                   clear                   v1chcr
083200070614     C                   GOTO      ENDCTR
083300070614    3c                   endif
083400070614    1C                   ENDIF
083500130711     c
083600130711     c                   endif
083700051209     c
083800980130     C* CONTROLLO DATA SPEDIZIONE DA MODIFICARE ----------------------*
083900980130     C*
084000980130     C                   Z-ADD     V10DSP        G02DAT
084100980130     C                   MOVEL     *BLANK        G02ERR
084200980130     C                   CALL      'XSRDA8'
084300980130     C                   PARM                    WLBDAT
084400980130     C* ERRATA
084500980130    1C     G02ERR        IFEQ      '1'
084600980130     C                   SETON                                        46  90
084700980130     C                   GOTO      ENDCTR
084800980130    1C                   ENDIF
084900980130     C*
085000980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
085100980130     C*  IMMESSO DA VIDEO
085200980130     C                   Z-ADD     G02DAT        V10DSP
085300980130     C*
085400980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
085500980130     C                   MOVE      V10DSP        WAAS              4 0
085600980202     C                   MOVE      G02INV        WMGS              4 0
085700980203     C                   Z-ADD     G02INV        WDTCAR            8 0
085800001120     C                   MOVEL     G02INV        WAMSPE            6 0
085900980130     C*
086000980130     C*
086100980130     C* CONTROLLO DATA SPEDIZIONE CHE SOSTITUISCE LA VECCHIA----------*
086200980130     C*
086300980130     C                   Z-ADD     V11DSP        G02DAT
086400980130     C                   MOVEL     *BLANK        G02ERR
086500980130     C                   CALL      'XSRDA8'
086600980130     C                   PARM                    WLBDAT
086700980130     C* ERRATA
086800980130    1C     G02ERR        IFEQ      '1'
086900980130     C                   SETON                                        47  90
087000980130     C                   GOTO      ENDCTR
087100980130    1C                   ENDIF
087200980130     C* REIMPOSTO LA DATA SPEDIZIONE GGMMAA PRENDENDO L'ANNO A 4 SE NON
087300980130     C*  IMMESSO DA VIDEO
087400980130     C                   Z-ADD     G02DAT        V11DSP
087500980202     C* CONTROLLO CHE LE DUE DATE SIANO UGUALI
087600980202     C     V11DSP        IFEQ      V10DSP
087700980204     C                   SETON                                        5090
087800980202     C                   GOTO      ENDCTR
087900980202     C                   ENDIF
088000980130     C* IM MANUTENZIONE NON SI PUO' CAMBIARE L'ANNO DI SPEDIZIONE
088100980130     C                   MOVE      V11DSP        WAASN             4 0
088200980130     C*
088300140114     c* questo controllo non lo faccio se premuto f8-sosp automatica
088400140114    2C  nkhWAASN         IFNE      WAAS
088500130729     c                   if        W1cksc<>0
088600081203     C                   MOVEL(P)  '3C'          COD
088700130729     C                   MOVEL(p)  W1cksc        KEY
088800081203     C     ktab2         chain     tabel00f                           31
088900081203     C   31              clear                   ds3c
089000081203     C  N31              movel     tbluni        ds3c
089100081203     c                   endif
089200130729     c     W1cksc        ifeq      0
089300090126     c     ntwlnp        andne     'E'
089400130729     c     W1cksc        orgt      0
089500090126     c     §3cfsp        andeq     'S'
089600980130     C                   SETON                                        4790
089700980130     C                   GOTO      ENDCTR
089800081203     c                   endif
089900980130    2C                   ENDIF
090000980130     C*
090100980130     C                   Z-ADD     G02INV        WLPDSP            8 0
090200001120     C                   MOVEL     G02INV        WAMSOS            6 0
090300980130     C* DATE SPEDIZIONE NO > DATA MAX DI SOSPENSIONE
090400980130    2C     WLPDSP        IFGT      DATAPA
090500980130     C                   SETON                                        4990
090600980130     C                   GOTO      ENDCTR
090700980130    2C                   ENDIF
090800980130     C*
090900980130     C     WLPDSP        IFLT      DATEU
091000980130     C                   SETON                                        4890
091100980130     C                   GOTO      ENDCTR
091200980130     C                   ENDIF
091300070612     c* Se immessa la consegna richiesta, controllo che non sia
091400070612     c*  inferiore alla nuova data spedizione
091500070612     c                   if        *in03 and  v1cdcr>0  and wdcr<wlpdsp
091600070612     c                   seton                                        702890
091700070612     c                   movel     msg(9)        v1cmsg
091800070612     c                   goto      endctr
091900070612     c                   endif
092000980203     C*
092100980203     C* TERMINAL DI ARRIVO SOLO DOPO AVER INSERITO LA DATA DI SPEDIZIONE
092200980203    1C     V1CTFA        IFNE      *ZEROS
092300980203     C* VERIFICO SE E' CORRETTO
092400980203     C                   MOVE      '0'           CRISP             1
092500980203     C     'A'           SETLL     AZCAE01L
092600980203     C     'A'           READE     AZCAE01L                               30
092700980203    2C     *IN30         DOWEQ     *OFF
092800980203    3C     V1CTFA        IFEQ      CAETFE
092900980203     C     CAEATB        ANDEQ     *BLANKS
093000980203     C     WDTCAR        ANDGE     CAEDDE
093100980203     C     WDTCAR        ANDLE     CAEDSC
093200980203     C     CAETFP        IFEQ      0
093300980203     C     CAETFP        OREQ      SIMFEL
093400980203     C                   MOVE      '1'           CRISP
093500980203     C                   END
093600980203    3C                   END
093700980203     C     'A'           READE     AZCAE01L                               30
093800980203    2C                   ENDDO
093900980203     C* ERRORE
094000980203    2C     CRISP         IFEQ      '0'
094100980203     C                   SETON                                        9043
094200980203     C                   MOVE      *BLANKS       V1DTFA
094300980203     C                   GOTO      ENDCTR
094400980203   X2C                   ELSE
094500980203     C* DECODIFICA
094600980203     C     V1CTFA        CHAIN     AZORG01L                           30
094700980203     C  N30              MOVEL     ORGDES        V1DTFA
094800980203    2C                   END
094900980203    1C                   END
095000980130     C*
095100130327     ***C  NKF
095200130327     ***CANNKH              SETON                                        90
095300130711     C**  NKF              SETON                                        90
095400980130     C*
095500980130     C     ENDCTR        ENDSR
095600070612     C*****************************************************************
095700070612     C* controllo dati consegna richiesta      ----------------------
095800070612     C*****************************************************************
095900070612     C     CtrCONSRICH   BEGSR
096000070612      * tipo
096100070612    1c                   If        V1cTcr <> *Blanks   and
096200070612     c                             v1cDcr = *Zeros
096300070612     c                   Eval      *In28 = *On
096400070612     c                   Eval      *In70 = *On
096500070612     c                   Eval      *In90 = *On
096600070612     c                   Eval      v1cMsg = msg(6)
096700070612     c                   GoTo      EndCons
096800070612    1c                   EndIf
096900070612      * data
097000070614     c                   clear                   wdcr
097100070612    1c                   If        V1cDcr > *Zeros
097200070612      * controllo formale della data
097300070612     c                   Clear                   wlbdat
097400070612     c                   Z-Add     V1cDcr        G02Dat
097500070612     c                   Call      'XSRDA8'
097600070612     c                   Parm                    wlbdat
097700070612    2c                   If        G02Err = '1'
097800070612     c                   Eval      *In28 = *On
097900070612     c                   Eval      *In70 = *On
098000070612     c                   Eval      *In90 = *On
098100070612     c                   Eval      v1cMsg = msg(7)
098200070612     c                   GoTo      EndCons
098300070612    2c                   EndIf
098400070612     c
098500070612     c                   Movel     G02Dat        V1cDcr
098600070612     c                   Movel     G02Inv        wdcr
098700070612      * Non deve essere inferiore/uguale a oggi
098800070612    2c                   If        wdcr <= dateu
098900070612     c                   Eval      *In28 = *On
099000070612     c                   Eval      *In85 = *On
099100070612     c                   Eval      *In90 = *On
099200070612     c                   Eval      v1cMsg = msg(7)
099300070612     c                   GoTo      EndCons
099400070612    2c                   EndIf
099500070612     c
099600070612      * Non deve essere superiore a oggi + 5 gg.
099700070612    2c                   if        v1ctcr = 'P'
099800070612     c                   z-add     woggidcgm1    s_woggidcg
099900070612     c                   z-add     §vpodcgm1     s_vpodcg
100000070612     c                   else
100100070612     c                   z-add     woggidcgm2    s_woggidcg
100200070612     c                   z-add     §vpodcgm2     s_vpodcg
100300070612    2c                   endif
100400070612    2c                   If        wdcr > s_woggidcg
100500070612     c                   Eval      *In28 = *On
100600070612     c                   Eval      *In70 = *On
100700070612     c                   Eval      *In90 = *On
100800070612     c                   Eval      v1cMsg = msg(8)
100900070612     c                   Eval      v1cMsg = %trim(v1cmsg) + ' ' +
101000070612     c                             %trim(%editc(s_vpodcg:'Z'))  +
101100070612     c                             ' giorni dalla data odierna'
101200070612     c                   GoTo      EndCons
101300070612    2c                   EndIf
101400070612    1c                   EndIf
101500070614     c* Ora richiesta
101600070614     c                   if        v1chcr>0
101700070614     c                   movel     v1chcr        whcr              2 0
101800070614     c                   move      v1chcr        wmcr              2 0
101900070614    2C     WHCR          IFLT      0
102000070614     C     WHCR          ORGT      24
102100070614     C                   MOVEL     msg(10)       V1cMSG
102200070614     C                   SETON                                        719028
102300070614     C                   GOTO      ENDCons
102400070614    2C                   ENDIF
102500070614     C*
102600070614    2C     WMCR          IFLT      0
102700070614     C     WMCR          ORGT      59
102800070614     C                   MOVEL     msg(10)       V1cMSG
102900070614     C                   SETON                                        719028
103000070614     C                   GOTO      ENDCons
103100070614    2C                   ENDIF
103200070614    2C                   ENDIF
103300070612     c
103400070612     c     ENDCONS       ENDSR
103500980130     C*****************************************************************
103600980130     C* ELABORAZIONE TESTATA E DETTAGLIO BOLLE ----------------------
103700980130     C*****************************************************************
103800980130     C     MOD01         BEGSR
103900980130     C*
104000980130     C                   SETOFF                                       515253
104100110704     C                   SETOFF                                       5455
104200081205     c                   clear                   werror
104300081210     c                   clear                   wmsg
104400980130     C* CONTROLLO CHE TIPO DI SELEZIONE E' STATA FATTA
104500980130     C* PER CLIENTE
104600130729     C     W1CKSC        COMP      *ZEROS                             5151
104700980130     C* PER LINEA PARTENZA
104800980130     C     V1CLNP        COMP      *ZEROS                             5252
104900980130     C* PER TERMINAL DI ARRIVO
105000980130     C     V1CTFA        COMP      *ZEROS                             5353
105100980130     C* PER LINEA ARRIVO
105200061003     C     WCLNA         COMP      *ZEROS                             54
105300980130     C* PER PADRONCINO
105400061003     C     WCPDR         COMP      *ZEROS                             55
105500020610
105600020610      * se richiesta una linea di partenza imposto solo quella linea di
105700020610      * partenza nella sk delle linee da stampare
105800020610     c                   if        v1clnp <> *zeros
105900020610     c                   clear                   sklnp
106000020610     c                   eval      sklnp(1) = v1clnp
106100020610     c                   else
106200020610     c                   movea     lin           sklnp
106300020610     c                   endif
106400110704     c
106500060403     c* se richiesto aggiornamento su bolle senza colli spuntati stampo
106600060403     c* testata per elenco spedizioni in sospensione da x giorni
106700060403     c   01              if        v1cnos = 'S'
106800060403     c                   open      fnls03p
106900060404     c                   move      v1cggl        w_gg              1
107000060404     c                   eval      ptitolo = ctitolo
107100060404     c                   eval      ptitolo = %xlate('X':w_gg:ptitolo)
107200090127     c                   eval      *in13='0'
107300130729     c                   if        W1cksc>0
107400090127     c                   eval      *in13='1'
107500090127     c                   endif
107600130729     c                   z-add     W1cksc        p1cccm
107700060404     c                   movel     v1dksc        p1dccm
107800090127     c                   z-add     v1clnp        p1clnp
107900090127     c                   movel     v1dlnp        p1dlnp
108000060403     c                   write     testa
108100060403     c                   endif
108200061005     c
108300061005     c                   open      qsysprt
108400060403     c
108500110704     c* se parzializzazione per numero ORM, leggo per ORM
108600110704     c*  che faccio prima
108700110704    0c                   if        sel4>0
108800110704     C                   MOVEL     SEL4          KN14
108900110704     C     Kar43         SETLL     Fiar404L
109000110704     C     Kar43         reade     Fiar404L
109100110704    1c                   dow       not %eof(fiar404l)
109200110704     c
109300110704     c* controllo la linea di partenza immessa o dell'area
109400110704     c                   eval      Indx=%lookup(ar4lnp:sklnp)
109500110704     c*
109600110704    2c                   if        Indx>0
109700110704
109800110704     c     kblpm         chain(n)  fnblp01l
109900110704
110000110704     c* Verifico anche la data spedizione
110100110704     c                   if        blpdsp=wdtcar
110200110704     c
110300110704     c                   exsr      Parzial
110400110704     c
110500110704    3c                   if        NoBolla=' '
110600110704    4c                   if        waasn<>waas
110700110704     c                   exsr      sr_nuovonumero
110800110704   x4c                   else
110900110704     c
111000110704     c                   exsr      Aggiorna
111100110704     c*
111200110704    4c                   endif
111300110704    4c                   endif
111400110704    3c                   endif
111500110704    2c                   endif
111600110704     c
111700110704     C     Kar43         reade     Fiar404L
111800110704    1c                   enddo
111900110704      *
112000110704   x0c                   else
112100020610
112200020610      * elaboro la sk delle linee da stampare
112300110704   1ac                   do        30            xx
112400110704    1c                   if        sklnp(xx) = *zeros
112500020610     c                   leave
112600110704    1c                   endif
112700020610
112800020610     c                   eval      lnp = sklnp(xx)
112900980130     C*
113000980130     C* LETTURA TESTATE BOLLE
113100020610     C  N52KBLP1         SETLL     FNBLP70L
113200980130     C   52
113300020610     CANN55KBLP2         SETLL     FNBLP70L
113400020610     C   55KBLP3         SETLL     FNBLP70L
113500110704     c
113600980203    1C                   DO        *HIVAL
113700020610     C  N52KBLP1         READE     FNBLP70L                               30
113800980203     C   52
113900020610     CANN55KBLP2         READE     FNBLP70L                               30
114000020610     C   55KBLP3         READE     FNBLP70L                               30
114100980203     C   30              LEAVE
114200110704     c
114300110704     c                   exsr      Parzial
114400110704     c
114500110704     c                   if        NoBolla='N'
114600110704     c                   iter
114700110704     c                   endif
114800110704     c
114900081203     c* Se cambia anche l'anno richiamo pgm apposito che esegue anche la
115000081203     c* rinumerazione della spedizione
115100110704    2c                   if        waasn<>waas
115200081203     c                   exsr      sr_nuovonumero
115300081203     c                   iter
115400110704    2c                   endif
115500110704     c
115600110704     c                   exsr      Aggiorna
115700980202     C*
115800980202    1C                   ENDDO
115900020610
116000110704   1ac                   enddo
116100110704    0c                   endif
116200060403     c*
116300060403     c   01              if        v1cnos = 'S'
116400060404     c   98              write     testa
116500060404     c                   setoff                                       98
116600060404     c                   write     fines
116700060403     c                   close     fnls03p
116800060403     c                   endif
116900061005     c
117000061005     c                   close     qsysprt
117100060713     c
117200060713     c                   if        bollemsg>0
117300111018     c                   eval      zz=zz+1
117400111018     c                   eval      invbo(zz)=bollemsg
117500130729     c                   if        W1cksc>0
117600130729     c                   eval      invcod(zz)=V1cksc + '-' + v1dksc
117700111018     c                   else
117800111018     c                   eval      invcod(zz)=%editc(v1clnp:'X')+'-'+
117900111018     c                             v1dlnp
118000111018     c                   endif
118100111018     c                   eval      invdim(zz)=savdim
118200111018     c
118300111019     c                   if        zz=50
118400111018     c                   exsr      Inv_email
118500111018     c                   clear                   invbo
118600111018     c                   clear                   invcod
118700111018     c                   clear                   invdim
118800111018     c                   endif
118900060713     c                   endif
119000111018     c
119100060713     c* emetto msg di quante bolle manutenzionate
119200130711     c                   exsr      emis_msg
119300130711
119400060926     c*
119500061003     c                   if        yy<6
119600060926     c                   add       1             yy
119700060926     c                   movel     param         elabor(yy)
119800060926     c                   endif
119900980130     C*
120000980202     C                   ENDSR
120100130711     C*----------------------------------------------------------------
120200130711     c     Emis_msg      BEGSR
120300130711     c****               movel     msg(5)        v1cmsg
120400130711     c****               eval      %subst(v1cmsg:27:11)= (%editc(bollema:'2'))
120500130711     c                   seton                                        28
120600130711    1c                   if        werror>0
120700130711    2c                   if        wmsg=*blanks
120800130711     c***                eval      widms1=%trim(widms1)+ 'ERRORE su '
120900130711     c***                eval      widms1=%trim(Widms1)+ (%editc(werror:'Z'))
121000130711     c****               eval      widms1=%trim(Widms1)+ ' bolle.'
121100130711     c                   eval      widms1=%trim(%editc(werror:'Z')) +
121200130711     c                             ' bolle non aggiornate.'
121300130711     c                   eval      widms2=ms2wdw
121400130711     c                   eval      widms3=ms3wdw
121500130711     c                   eval      widms4=ms4wdw
121600130711   x2c                   else
121700130711     c                   eval      widmsall=wmsg
121800130711    2c                   endif
121900130711    1c                   endif
122000130711    1c                   if        wmsg=*blanks
122100130711     c                   movel     msg(5)        v1cmsg
122200130711    2c                   if        bollema>0
122300130711     c                   eval      v1cmsg=%trim(v1cmsg)+' ' +%editc(bollema:'2')
122400130711   x2c                   else
122500130711     c                   eval      v1cmsg=%trim(v1cmsg)+' '
122600130711     c                             + %editw(bollema:'       0 ')
122700130711    2c                   endif
122800130711     c                   eval      v1cmsg=%trim(v1cmsg)+ ' Bolle.'
122900130711    1c                   endif
123000130711     c                   ENDSR
123100110704     C*----------------------------------------------------------------
123200110704     c     Parzial       BEGSR
123300110704     c                   clear                   Nobolla           1
123400110704      *
123500110704      *  51 CONTROLLO IL CODICE CLIENTE
123600110704     C     BLPCCM        IFNE      *ZEROS
123700110704     C                   Z-ADD     BLPCCM        WKSC              7 0
123800110704     C                   ELSE
123900110704     C                   Z-ADD     BLPKSC        WKSC
124000110704     C                   ENDIF
124100130729    2C   51WKSC          IFNE      W1CKSC
124200110704     c                   if        blplnp=190 or blplnp=191
124300110704     c                   seton                                        80
124400110704     c                   except    det
124500110704     c                   setoff                                       80
124600110704    2C                   END
124700110704     c                   eval      NoBolla='N'
124800110704     C                   leavesr
124900110704    2C                   END
125000110704     c* Se richiesta sospensione a cavallo di anno per una lnp estera
125100110704     c* se il cliente mantiene il numero spedizione la scarto
125200110704     c                   if        not *in51 and ntwlnp='E'
125300110704     c                             and waasn<>waas
125400110704     C                   MOVEL(P)  '3C'          COD
125500110704     C                   MOVEL(p)  wksc          KEY
125600110704     C     ktab2         chain     tabel00f                           31
125700110704     C   31              clear                   ds3c
125800110704     C  N31              movel     tbluni        ds3c
125900110704     c     §3cfsp        ifeq      'S'
126000110704     c                   eval      NoBolla='N'
126100110704     C                   leavesr
126200110704     c                   endif
126300110704     c                   endif
126400110704      *
126500110704      *  53 CONTROLLO IL TERMINAL ARRIVO
126600110704    2C   53BLPTFA        IFNE      V1CTFA
126700110704     c                   if        blplnp=190 or blplnp=191
126800110704     c                   seton                                        81
126900110704     c                   except    det
127000110704     c                   setoff                                       81
127100110704    2C                   END
127200110704     c                   eval      NoBolla='N'
127300110704     C                   leavesr
127400110704    2C                   END
127500110704      *
127600110704      *  54 CONTROLLO LA LINEA DI ARRIVO
127700110704    2C   54BLPLNA        IFNE      WCLNA
127800110704     c                   if        blplnp=190 or blplnp=191
127900110704     c                   seton                                        82
128000110704     c                   except    det
128100110704     c                   setoff                                       82
128200110704    2C                   END
128300110704     c                   eval      NoBolla='N'
128400110704     C                   leavesr
128500110704    2C                   END
128600110704      *
128700110704      ** CONTROLLO CHIUSURA FOGLIO VIAGGIO DELLA BOLLA
128800110704    2C     BLPDT1        IFNE      *ZEROS
128900110704     c                   if        blplnp=190 or blplnp=191
129000110704     c                   seton                                        83
129100110704     c                   except    det
129200110704     c                   setoff                                       83
129300110704    2C                   END
129400110704     c                   eval      NoBolla='N'
129500110704     C                   leavesr
129600110704    2C                   ENDIF
129700110704     c
129800110704     c* Se richiesto, controllo se non ha spunte per tutti i colli della
129900110704     c*  sped
130000110704    1c                   if        v1cnos='S'
130100110704     c                   clear                   wsispu            1
130200110704     c     kblt          setll     fnblt01l
130300110704     c     kblt          reade     fnblt01l                               99
130400110704    2c                   dow       not *in99
130500110704     c
130600110704     c* Cerco una qualsiasi spunta
130700110704     c     kbrv          chain     fnbrv07l
130800110704    3c                   if        %found(fnbrv07l)
130900110704     c                   eval      wsispu='S'
131000110704     c                   seton                                        99
131100110704   x3c                   else
131200110704     c     kblt          reade     fnblt01l                               99
131300110704    3c                   endif
131400110704    2c                   enddo
131500110704     c
131600110704     c*Se non trovata nessuna spunta
131700110704    2c                   if        wsispu='S'
131800110704     c                   if        blplnp=190 or blplnp=191
131900110704     c                   seton                                        84
132000110704     c                   except    det
132100110704     c                   setoff                                       84
132200110704    2C                   END
132300110704     c                   eval      NoBolla='N'
132400110704     c                   leavesr
132500110704    2c                   endif
132600110704    1c                   endif
132700110704     c*
132800110704      * SCARTO I PREPAGATI ANNULLATI
132900110704     c                   if        blpcbo='M '
133000110704     c                   eval      NoBolla='N'
133100110704     c                   leavesr
133200110704     c                   endif
133300110704      *
133400110704      ** CONTROLLO IL TIPO BOLLA
133500110704     C                   MOVEL     '3A'          COD
133600110704     C                   MOVEL(P)  BLPCBO        KEY
133700110704     C     KTAB2         CHAIN     TABEL00F                           31
133800110704     C   31              CLEAR                   DS3A
133900110704     C  N31              MOVEL     TBLUNI        DS3A
134000110704      *
134100110704      * SCARTO LE BOLLE DI RECUPERO
1342001107041   2C     §3ARBL        IFEQ      'R'
134300110704     c                   if        blplnp=190 or blplnp=191
134400110704     c                   seton                                        85
134500110704     c                   except    det
134600110704     c                   setoff                                       85
134700110704    2C                   END
134800110704     c                   eval      NoBolla='N'
134900110704     C                   leavesr
135000110704    2C                   END
135100110704      * SCARTO LE BOLLE con cod trattamento merce che non prevede merce
135200110704     C                   MOVEL     '1B'          COD
135300110704     C                   MOVEL(P)  BLPctm        KEY
135400110704     C     KTAB2         CHAIN     TABEL00F                           31
135500110704     C   31              CLEAR                   DS1b
135600110704     C  N31              MOVEL     TBLUNI        DS1b
135700110704    2C                   IF        §1bfg8='N'
135800110704     c                   if        blplnp=190 or blplnp=191
135900110704     c                   seton                                        85
136000110704     c                   except    det
136100110704     c                   setoff                                       85
136200110704    2C                   END
136300110704     c                   eval      NoBolla='N'
136400110704     C                   leavesr
136500110704    2C                   END
136600110704      *
136700110704      * PER I PREPAGATI NON È CONSENTITO SPOSTARE LA DATA BOLLA AL MESE
136800110704      *  SUCCESSIVO
136900110704     C     WAMSOS        IFGT      WAMSPE
137000110704      *
137100110704    2C     BLPPDR        IFEQ      *ZEROS
137200110704     c                   if        blplnp=190 or blplnp=191
137300110704     c                   seton                                        86
137400110704     c                   except    det
137500110704     c                   setoff                                       86
137600110704    2C                   END
137700110704     c                   eval      NoBolla='N'
137800110704     C                   leavesr
137900110704    2C                   ELSE
138000110704     C                   MOVEL     '1'           KTRC
138100110704     C     KAR6          CHAIN(N)  FIAR6000                           31
138200110704    2C     *IN31         IFEQ      '0'
138300110704    2C     AR6NFT        ANDGT     *ZEROS
138400110704     c                   if        blplnp=190 or blplnp=191
138500110704     c                   seton                                        87
138600110704     c                   except    det
138700110704     c                   setoff                                       87
138800110704    2C                   END
138900110704     c                   eval      NoBolla='N'
139000110704     C                   leavesr
139100110704    2C                   ENDIF
139200110704    2C                   ENDIF
139300110704      *
139400110704    2C                   ENDIF
139500110704     c
139600110704     c                   ENDSR
139700110704     c*------------------------------------------------------------------------
139800110704     c     Aggiorna      BEGSR
139900110704     c* A questo punto visto che sono stati superati tutti i controlli
140000110704     c* chaino il record bolla in update
140100110704     c     kblt          chain(E)  fnblp01l
140200110704    1c                   if        %error
140300110704     C                   CLEAR                   TRUL82DS
140400110705     c                   if        sel4=0
140500110704     c                   eval      ul82§rrn = fnblpnrri
140600110705     c                   else
140700110705     c                   eval      ul82§rrn = fnblpnrr01
140800110705     c                   endif
140900110704     C                   EVAL      UL82§FIL = 'FNBLP00F'
141000110704     C                   EVAL      UL82§WIN = 'N'
141100110704     C                   EVAL      UL82§F7  = 'S'
141200110704     C                   EVAL      UL82§num = 1
141300110704     C                   EVAL      UL82§att = 30
141400110704     C                   EVAL      UL82§mss = msgjob
141500110704     C* Effettuo la chiamata al *PGM d utilità
141600110704     C                   CALL(e)   'TRUL82R'
141700110704     C                   PARM                    TRUL82DS
141800110704     C     kblt          chain     fnblp01l
141900110704    1c                   endif
142000110704    2c                   if        %found(fnblp01l)
142100111214     c
142200111214     c* Mi salvo il terminal della bolla
142300111214     c                   eval      savtfp=blptfp
142400110704      *
142500110704      ** CONTROLLO SE DATA BORDERO' MINORE DELLA NUOVA DATA DI SPEDIZIONE
142600110704      *  SBORDERIZZO
142700110704    2C     BLPDBR        IFLT      WLPDSP
142800110704      *
142900110704      * SBORDERIZZO
143000110704     C                   CLEAR                   BLPNFV
143100110704     C                   CLEAR                   BLPDBR
143200110704     C                   CLEAR                   BLPFLP
143300110704    2C                   ENDIF
143400110704     C* AGGIORNO LA NUOVA DATA SPEDIZIONE
143500110704     C                   Z-ADD     WAASN         BLPAAS
143600110704     C                   Z-ADD     WLPDSP        BLPMGS
143700110704     c* Aggiorno anche la data ritiro merce
143800110704     c*  solo per i clienti che spostanto in sospensione per mancanza di spu
143900110704     c*  nte
144000110704    2c                   if        v1cnos='S'
144100110704     C                   movel     WLPDSP        BLPdrt
144200110704     c* Se richiesto e se presente consegna richiesta, imposto la nuova data
144300110704    3c                   if        *in03 and v1cdcr>0 and blpdcr>0
144400110704     c                   movel     v1ctcr        blptcr
144500110704     c                   movel     wdcr          blpdcr
144600110704     c                   movel     v1chcr        blphcr
144700110704    3c                   endif
144800110704     c
144900110704   x2c                   else
145000110704     c* Se la data ritiro > della data spedizione nuova e non c'e' ORM
145100110704     c*  imposto SEMPRE la data ritiro= data spedizione
145200110704    3c                   if        blpdrt>wlpdsp
145300110704     c     kar4m         chain     fiar401l
145400110704    4c                   if        not %found(fiar401l)
145500110704     c                   eval      blpdrt=wlpdsp
145600110704    4c                   endif
145700110704    3c                   endif
145800110704     c
145900110704    2c                   endif
146000110704     c
146100110704     C* E RICACOLO I TERMINAL DI ARRIVO E FILIALE ELABORAZIONE ARRIVI
146200110704     C                   CLEAR                   DSLV55
146300110704     C                   MOVEL     ' '           D55TPT
146400110704     C                   MOVEL     BLPLNA        D55LIN
146500110704     C                   MOVEL     BLPLNP        D55LNP
146600110704     C                   MOVEL     WLPDSP        D55DRF
146700110704     C                   CALL      'FNLV55R'
146800110704     C                   PARM                    DSLV55
146900110704    2C     D55ERR        IFEQ      ' '
147000110704     C                   MOVEL     D55TFA        BLPTFA
147100110704     C                   MOVEL     D55TFP        BLPFEA
147200110704    2C                   ENDIF
147300110704     C* BLPTFP: TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
147400110704     C                   CLEAR                   DSLV55
147500110704     C                   MOVEL     'P'           D55TPT
147600110704     C                   MOVEL     BLPLNP        D55LIN
147700110704     C                   MOVEL     WLPDSP        D55DRF
147800110704     C                   CALL      'FNLV55R'
147900110704     C                   PARM                    DSLV55
148000111214     c
148100111214     c
148200110704    2C     D55ERR        IFEQ      ' '
148300110704     C                   MOVEL     D55TFP        BLPTFP
148400111128     C                   MOVEL     D55TFP        BLPfle
148500110704    2C                   ENDIF
148600110704     C*
148700110704     C                   UPDATE    FNBLP001
148800110704     c                   add       1             bollema           9 0
148900110704     c
149000110704     c* Se si tratta di assegnato S.F. tassato in partenza, cancello la
149100110704     c* tassazione
149200110704     c                   clear                   ktrc
149300110704    2c                   if        §3atb2<>*blanks
149400110704     c                   eval      ktrc='2'
149500110704   x2c                   else
149600110704     c                   movel     §3atb1        flgtb1            1
149700110704    3c                   if        flgtb1='A'
149800110704     c                   eval      ktrc='1'
149900110704    3c                   endif
150000110704    2c                   endif
150100110704     c
150200110704   1ac                   if        ktrc<>*blanks
150300110704     C     KAR6          CHAIN(N)  FIAR6000                           31
150400110704    2c                   if        not *in31
150500110704     c                   move      ar6ksc        fineksc           4 0
150600110704    3c                   if        fineksc<>0000 and fineksc<>9999
150700110704     c* deleto la tassazione
150800110704     c     kar6          setll     fiar701l
150900110704     c     kar6          reade     fiar701l
151000110704    4c                   dow       not %eof(fiar701l)
151100110704     c                   delete    fiar7000
151200110704     c
151300110704     c     kar6          reade     fiar701l
151400110704    4c                   enddo
151500110704     c
151600110704     c     kar6          delete    fiar6000
151700110704    3c                   endif
151800110704    2c                   endif
151900110704   1ac                   endif
152000110704     c
152100110704     C*
152200110704     C     KBLT          SETLL     FNBLT01L
152300110704     C     KBLT          READE     FNBLT01L                               30
152400121107    2C     *IN30         DOWEQ     *OFF
152500110704     C* SOLO SE NON C'E' GIA' LA DATA DI PARTENZA
152600110704    3C  N30BLTDFV        IFEQ      0
152700110704     C* E LA DATA BORDERO' DELLA TESTATA NON E' MAGGIORE ALLA NUOVA DATA SPED.
152800110704    2C     BLPDBR        ANDLT     WLPDSP
152900110704     C                   CLEAR                   BLTNFV
153000110704     C                   UPDATE    FNBLT000
153100110704   X3C                   ELSE
153200110704     C                   UNLOCK    FNBLT01L
153300110704    3C                   ENDIF
153400121107     C     KBLT          READE     FNBLT01L                               30
153500110704    2C                   ENDDO
153600111214     c
153700110704     C* AGGIORNO ANCHE FISGN SE PRESENTE
153800110704     C     KFISGN        SETLL     FISGN05L
153900110704     C     KFISGN        READE     FISGN05L                               30
154000110704    2C     *IN30         DOWEQ     *OFF
154100110704     C                   Z-ADD     WLPDSP        SGNMGS
154200110704     C                   Z-ADD     BLPTFA        SGNTNA
154300111214     c
154400111214     c* Cambio il terminal solo se corrisponde a quello della bolla
154500111214     c*  se l'ho inviato ad aaltro terminal non modifico
154600130214     c**                 if        sgntnp=savtfp
154700130214     C**                 Z-ADD     BLPTFP        SGNTNP
154800130214     c**                 endif
154900111214     c
155000110704     C                   CLEAR                   SGNT6A
155100110704     C                   CLEAR                   SGNT6B
155200110704     C                   CLEAR                   SGNT6C
155300110704     C                   CLEAR                   SGNT6D
155400110704     C                   CLEAR                   SGNT6E
155500110704     C                   CLEAR                   SGNT6F
155600110704     c                   eval      sgndatora = %char(%timestamp:*iso0)
155700110704     C                   UPDATE    FISGN000
155800110704     C     KFISGN        READE     FISGN05L                               30
155900110704    2C                   ENDDO
156000110704     C*
156100110704     c* richiamo routine per gestione stampe
156200110704     c                   z-add     blpaas        wlpaas
156300110704     c                   z-add     blpnsp        wlpnsp
156400110704     c                   exsr      sr_stampe
156500110704     c*
156600110704    1c                   endif
156700110704     c
156800110704     c                   ENDSR
156900980130     C*----------------------------------------------------------------
157000980130     C*
157100980130     C*--- OPERAZIONI INIZIALI ---------------------------------------*
157200980130     C     *INZSR        BEGSR
157300980130     C***
157400980130     C* KLIST
157500980130     C***
157600980130     C* ACCESSO   CNACO
157700980130     C     KACO          KLIST
157800980130     C                   KFLD                    CODUT
157900980130     C                   KFLD                    CCC
158000130729     C                   KFLD                    W1CKSC
158100051212     C     Ktbe          KLIST
158200051212     C                   KFLD                    kcod
158300051212     C                   KFLD                    klin
158400051212     C                   KFLD                    ksif
158500110704     C*
158600980130     C     KBLT          KLIST
158700980130     C                   KFLD                    BLPAAS
158800980130     C                   KFLD                    BLPLNP
158900980130     C                   KFLD                    BLPNRS
159000980130     C                   KFLD                    BLPNSP
159100110704     C* KEY PER AGGIIORNARE FLBLT
159200110704     C     KBLPM         KLIST
159300110704     C                   KFLD                    ar4AAS
159400110704     C                   KFLD                    ar4LNP
159500110704     C                   KFLD                    ar4NRS
159600110704     C                   KFLD                    ar4NSP
159700110704     C* KEY PER AGGIORNARE fiar4
159800080331     C     Kar4M         KLIST
159900080331     C                   KFLD                    BLPAAS
160000080331     C                   KFLD                    BLPLNP
160100080331     C                   KFLD                    BLPNRS
160200080331     C                   KFLD                    BLPNSP
160300110704     C                   KFLD                    ktrcM
160400051209     C     KBrv          KLIST
160500051209     C                   KFLD                    bltfls
160600051209     C                   KFLD                    bltlna
160700051209     C                   KFLD                    bltNRS
160800051209     C                   KFLD                    bltnsc
160900001120     C* KEY PER AGGANCIARE  FIAR6
161000001120     C     KAR6          KLIST
161100001120     C                   KFLD                    BLPAAS
161200001120     C                   KFLD                    BLPLNP
161300001120     C                   KFLD                    BLPNRS
161400001120     C                   KFLD                    BLPNSP
161500001120     C                   KFLD                    KTRC              1
161600980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
161700980130     C     KBLP1         KLIST
161800020610     c                   kfld                    lnp
161900980130     C                   KFLD                    WAAS
162000980130     C                   KFLD                    WMGS
162100980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 2
162200980130     C     KBLP2         KLIST
162300020610     c                   kfld                    lnp
162400980130     C                   KFLD                    WAAS
162500980130     C                   KFLD                    WMGS
162600980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 3
162700980130     C     KBLP3         KLIST
162800020610     c                   kfld                    lnp
162900980130     C                   KFLD                    WAAS
163000980130     C                   KFLD                    WMGS
163100980130     C                   KFLD                    WCPDR
163200010326     C* ACCESSO   FILE FISGN PER NUMERO SPEDIZIONE
163300010326     C     KFISGN        KLIST
163400010326     C                   KFLD                    BLPAAS
163500010326     C                   KFLD                    BLPLNP
163600010326     C                   KFLD                    BLPNRS
163700010326     C                   KFLD                    BLPNSP
163800980130     C* ACCESSO   TABEL
163900980130     C     KTAB2         KLIST
164000980130     C                   KFLD                    CODUT
164100980130     C                   KFLD                    COD               2
164200980130     C                   KFLD                    KEY               8
164300980202     C* ACCESSO   AZCLN
164400980202     C     KCLN          KLIST
164500980202     C                   KFLD                    SIMFEL
164600980202     C                   KFLD                    W0030             3 0
164700980202     C                   KFLD                    WAA               4 0
164800980202     C                   KFLD                    WMM               2 0
164900021203     C     Kfapd         klist
165000021203     C                   kfld                    kpdtip
165100021203     C                   kfld                    kpdpdr
165200110704      *
165300110704     C     KFORM         KLIST
165400110704     C                   KFLD                    V1CPOE
165500110704     C                   KFLD                    V1CNSR
165600110704     C                   KFLD                    V1CNOR
165700110704     C                   KFLD                    V1CNRV
165800110704      *
165900110704     C     Kar43         KLIST
166000110704     C                   KFLD                    KTRCM
166100110704     C                   KFLD                    KN14             14
166200110704     C*
166300980130     C***
166400980130     C* DEFINIZIONE CAMPI
166500980130     C***
166600980130     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
166700980130     C***
166800130729     c     *dtaara       define    §azute        AZUTEds
166900130729     c     *dtaara       define    §datiute      dDATIUTE
167000130729     c                   in(E)     *dtaara
167100130729     c                   if        %ERROR or RSUT = *blanks
167200130729     c                   clear                   Tibs34Ds
167300130729     c                   call      'TIBS34R'
167400130729     c                   parm                    Tibs34Ds
167500130729     c                   in        *dtaara
167600130729     c                   endif
167700130729      *
167800130729     c                   z-add     1             CodUt             1 0
167900980202     C***
168000980202     C* CARICO DATI VARIABILI PARTENZE
168100980202     C***
168200980202     C                   MOVEL     '3I'          COD
168300980202     C                   MOVEL     '1       '    KEY
168400980202     C     KTAB2         CHAIN     TABEL00F                           31
168500980202     C  N31              MOVEL     TBLUNI        DS3I
168600980202     C   31              CLEAR                   DS3I
168700980202     C***
168800980202     C* CARICO CODICI CLIENTI SDI PER I QUALI INSERISCO OBBLIGATORIAM
168900980202     C*  NUMERO AWB CON CONTROLLO CHECK DIGIT
169000980202     C***
169100980202     C                   MOVEL     '3I'          COD
169200980202     C                   MOVEL     'E       '    KEY
169300980202     C     KTAB2         CHAIN     TABEL00F                           31
169400980202     C  N31              MOVEA     TBLUNI        KSS
169500980202     C* 2 CIFRE INIZIALI FISSE PER NUMERO AWB
169600980202     C  N31              MOVE      TBLUNI        §3IFAW            2 0
169700980202     C   31              CLEAR                   KSS
169800980202     C   31              CLEAR                   §3IFAW
169900980130     C*
170000980130     C* DATA BOLLETTAZIONE:  TRA UDATE E DATA GIORNO + §3IGGS
170100980130     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
170200980130     C                   TIME                    W0140            14 0
170300980130     C* UDATE IN GGMMAAAA
170400980130     C                   MOVE      W0140         WDTGIO            8 0
170500060404     C                   MOVEl     W0140         utime
170600980130     C*
170700980130     C* UDATE IN AAAAMMGG
170800980130     C                   Z-ADD     WDTGIO        G02DAT
170900980130     C                   MOVEL     *BLANK        G02ERR
171000980130     C                   CALL      'XSRDA8'
171100980130     C                   PARM                    WLBDAT
171200980130     C                   MOVEL     G02INV        DATEU             8 0
171300980130     C* SE NON FOSSE PERMESSO BOLLETTARE IN SOSPENSIONE
171400980130     C*  SI PUO' BOLLETTARE SOLO FINO A OGGI
171500980130     C                   Z-ADD     G02INV        DATAPA            8 0
171600980130     C                   MOVEL     G02DAT        DATAPG            8
171700980130     C*
171800980130     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
171900130729     C                   Z-ADD     1             X                 2 0
172000980130    1C     X             DOWLE     §3IGGS
172100980130     C*
172200980130     C                   MOVEL     'F'           WFEST             1
172300980130     C*
172400980130    2C     WFEST         DOWNE     ' '
172500980130     C     G02TGI        ADD       1             GIOTGI
172600980130     C                   ADD       1             G02TGI
172700980130     C                   CALL      'XSRGI8'
172800980130     C                   PARM                    WGIDAT
172900980130     C*
173000980130     C                   Z-ADD     GIOINV        DATAPA            8 0
173100980130     C                   MOVEL     GIODAT        DATAPG            8
173200980130     C*
173300980130     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
173400980130     C                   Z-ADD     GIOINV        WDATA
173500980130     C                   CLEAR                   W0030
173600120302     C* CHAIN SUL CALENDARIO BRT
173700980130     C     KCLN          CHAIN     AZCLN01L                           30
173800980130     C   30              CLEAR                   CLNPOM
173900980130     C                   MOVEA     CLNPOM        K31
174000980130     C                   MOVEL     K31(WGG)      WFEST
174100980130    2C                   ENDDO
174200980130     C*
174300980130     C                   ADD       1             X
174400980130    2C                   ENDDO
174500070612     C* reperisco da tabella vpo il numero giorni da sommare a udate per
174600070612     c* determinare data max per data consegna richiesta
174700070612     C                   CLEAR                   tibs02ds
174800070612     C                   MOVEL     'C'           T02MOD
174900070612     C                   MOVEL     KNSIF         T02SIF
175000070612     C                   MOVEL     'VPO'         T02COD
175100070612     C                   MOVEL(p)  'VPO'         T02KE1
175200070612     C                   CALL      'TIBS02R'
175300070612     C                   PARM                    KPJBA
175400070612     C                   PARM                    tibs02ds
175500070612    4C     T02ERR        IFEQ      *BLANKS
175600070612     C                   MOVEL     T02UNI        DVPO
175700070612     C                   ELSE
175800070612     C                   CLEAR                   DVPO
175900070612    4C                   ENDIF
176000130327     c*
176100070612     c                   Move      Dateu         Dataiso
176200070612     c                   Adddur    §vpodcgm1:*d  Dataiso
176300070612     c                   Move      Dataiso       woggidcgm1
176400070612     c*
176500070612     c                   Move      Dateu         Dataiso
176600070612     c                   Adddur    §vpodcgm2:*d  Dataiso
176700070612     c                   Move      Dataiso       woggidcgm2
176800130327      *
176900980130     C***
177000980130     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
177100980130     C***
177200980130     C                   CLEAR                   DSUL06
177300980130     C                   MOVE      '£1'          D06COD
177400980130     C                   MOVEL     SIMFEL        D06KEY
177500980130     C                   MOVEL     'L'           D06TLA
177600980130     C                   MOVEL     DSUL06        KPJBU
177700980130     C*
177800980130     C                   CALL      'TRUL06R'
177900980130     C                   PARM                    KPJBA
178000980130     C                   MOVEL     KPJBU         DSUL06
178100980130     C                   MOVEA     LIN           L1
178200130327     c*
178300090126     c* Verifico se il £1 c'e' almeno un p.o. estero
178400090126     c                   clear                   poestero          1
178500070903     c                   z-add     1             x
178600070903     c                   dow       L1(x)<>0
178700070903     c     l1(x)         chain     azorg01l
178800070903     c                   if        %found(azorg01l)
178900070903     c                   movel     orgde3        og143
179000090126     c                   if        §ogntw='DPD' or §ogntw='EEX' or
179100090126     c                             §ogntw='FED'
179200090126     c                   eval      poestero='S'
179300070903     c                   leave
179400070903     c                   endif
179500070903     c                   endif
179600070903     c                   add       1             x
179700070903     c                   enddo
179800980130     C***
179900020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
180000980130     C***
180100130729     C                   Z-ADD     dutPOU        CDU               3 0
180200130729     C     DUTlpo        IFEQ      '2'
180300130729     C     DUTlpo        OREQ      *BLANKS
180400020429     C                   SETON                                        12
180500020429     C                   ENDIF
180600130327     c*
180700111018     c                   clear                   elabor
180800130327     c*
180900111018     c                   Clear                   tibs02ds
181000111018     c                   Clear                   dmradan
181100111018     c                   Movel     'C'           T02Mod
181200111018     c                   Movel     KNSIF         T02Sif
181300111018     c                   Movel     'MRA'         T02Cod
181400111018     c                   Movel     'FNLS03R'     T02Ke1
181500111018     c                   Call      'TIBS02R'
181600111018     c                   Parm                    Kpjba
181700111018     c                   Parm                    tibs02ds
181800111018    2c                   If        T02Err = *Blanks
181900111018     c                   Movel     T02Uni        Dmradan
182000111018    2c                   endif
182100001120     C*
182200980130     C                   ENDSR
182300980130     C*****************************************************************
182400980130     C* PULIZIA   FORMATO -------------------------------------------
182500980130     C*****************************************************************
182600980130     C     PUL01         BEGSR
182700980130     C*
182800070614     c                   setoff                                       0103
182900130729     C                   MOVE      *ZEROS        W1CKSC
183000130729     C                   clear                   V1CKSC
183100980130     C                   MOVE      *BLANKS       V1DKSC
183200980130     C                   MOVE      *BLANKS       V1CLNP
183300980130     C                   MOVE      *BLANKS       V1DLNP
183400980130     C                   MOVE      *ZEROS        V1CTFA
183500980130     C                   MOVE      *BLANKS       V1DTFA
183600980130     C                   MOVE      *BLANKS       V1CLNA
183700980130     C                   MOVE      *BLANKS       V1DLNA
183800980130     C                   MOVE      *BLANKS       V1CPDR
183900980130     C                   MOVE      *BLANKS       V1DPDR
184000980130     C                   Z-ADD     *ZEROS        V10DSP
184100980130     C                   Z-ADD     *ZEROS        V11DSP
184200070612     C                   CLEAR                   v1cdcr
184300070612     C                   CLEAR                   v1ctcr
184400070612     C                   CLEAR                   v1chcr
184500070612     C                   CLEAR                   wdcr
184600051209     C                   move      *blanks       V1cnos
184700060403     C                   z-add     3             V1cggl
184800060713     c                   clear                   bollemsg
184900060713     c                   clear                   bollema           9 0
185000110704     c                   clear                   v1cpoe
185100110704     c                   clear                   v1cnsr
185200110704     c                   clear                   v1cnor
185300110704     c                   clear                   v1cnrv
185400111017     c                   clear                   savdim            8 0
185500130327     c*
185600090126     c* Abilito l'opzione se c'e' un p.o. estero in £1
185700090126     c                   if        poestero='S'
185800070903     c                   seton                                        01
185900080624     c                   eval      SosdaL1='S'
186000070903     c                   else
186100130327     c*
186200070903     c* Abilito l'opzione solo se almeno un cliente è abilitato
186300080620     c                   eval      SosdaL1=' '
186400051212     c                   eval      ksif=knsif
186500051212     c     ktbe          setll     tntbe02l
186600051212     c                   if        not %equal(tntbe02l)
186700051212     c                   clear                   ksif
186800051212     c     ktbe          setll     tntbe02l
186900051212     c                   endif
187000130327     c*
187100051212    1c                   if        %equal(tntbe02l)
187200051212     c     ktbe          reade     tntbe02l
187300051212   1ac                   dow       not %eof(tntbe02l) and not *in01
187400051212    2c                   if        tbeatb=' '
187500130718     c                   movel     tbeuni        dbsp
187600130718    3c****               if        §clisos='S'
187700130718     c                   movel     tbeke1        lnpabi            3 0
187800070206    4c***                if        *in12=*on  and lnpcli=cdu
187900070206     c***                seton                                        01
188000070206    4c***                endif
188100130327     c*
188200070206    4c****               if        *in12=*off
188300130718     c                   if        lnpabi>0
188400130718     c     lnpabi        lookup    l1                                     30
188500051212    5c                   if        *in30
188600080620     c                   eval      SosdaL1='S'
188700051212     c                   seton                                        01
188800051212    5c                   endif
188900130718     c                   endif
189000070206    4c****               endif
189100130718    3c***                endif
189200051212    2c                   endif
189300130327     c*
189400051212     c     ktbe          reade     tntbe02l
189500051212   1ac                   enddo
189600051212    1c                   endif
189700130327     c*
189800070903    1c                   endif
189900130327     c*
190000080624     c*********          if        not *in01 and (cdu=118 or cdu=107)
190100080624     c*********          eval      *in01=*on
190200080624     c*********          endif
190300060403     c*
190400060403     c* se 01 acceso effettuo ovrprtf per mettere le stampe in save
190500060403     c                   if        *in01
190600060403     c                   clear                   qcmd
190700060403     c                   eval      qcmd = $cmd2(1)
190800060403     c                   call      'QCMDEXC'                            91
190900060403     c                   parm                    Qcmd
191000060403     c                   parm                    Qlen
191100060403     c                   endif
191200130327     c*
191300061005     c                   clear                   qcmd
191400061005     c                   eval      qcmd = $cmd3(1)
191500061005     c                   call      'QCMDEXC'                            91
191600061005     c                   parm                    Qcmd
191700061005     c                   parm                    Qlen
191800980130     C*
191900980130     C                   ENDSR
192000030619
192100130327     *** *---------------------------------------------------------------*
192200130327     *** * Richiedo i dati per la stampa   ed   Eseguo le override       *
192300130327     *** *---------------------------------------------------------------*
192400130327     ***c     SR_OvrPrtF    BEGSR
192500130327     *** *
192600130327     ***c                   call      'TRUL90R'
192700130327     ***c                   parm                    KPJBA
192800130327     ***c                   parm                    TRUL90DS
192900130327     *** * - F3 > Fine
193000130327     ***c     D90F3         cabeq     *on           End_Ovr
193100130327     *** *
193200130327     *** * - FNLSB5PA4 > bolle laser in formato A4
193300130327     ***c                   eval      wFILE = 'FNLSB5PA4 '
193400130327     ***c                   eval      wOUTQ = D90prb4
193500130327     ***c                   eval      wFORM = D90mdb4
193600130327     ***c                   exsr      SR_x_Cmd
193700130327     ***c     *in91         cabeq     *on           End_Ovr
193800130327     *** *
193900130327     *** * - FNLSB5PA5 > bolle laser in formato A5
194000130327     ***c                   eval      wFILE = 'FNLSB5PA5 '
194100130327     ***c                   eval      wOUTQ = D90prb5
194200130327     ***c                   eval      wFORM = D90mdb5
194300130327     ***c                   exsr      SR_x_Cmd
194400130327     ***c     *in91         cabeq     *on           End_Ovr
194500130327     *** *
194600130327     ***c     End_Ovr       ENDSR
194700130327     ***
194800130327     *** *---------------------------------------------------------------*
194900130327     ***
195000130327     ***c     SR_x_Cmd      BEGSR
195100130327     *** *
195200130327     *** * impostazione comando da eseguire
195300130327     ***c                   eval      Qcmd  = %trim($Cmd(1)) + ' '
195400130327     ***c                                   + %trim($Cmd(2))
195500130327     *** * - nome del file
195600130327     ***c                   eval      Qcmd = %replace(wFILE:Qcmd:
195700130327     ***c                                    %scan('&FILE':Qcmd))
195800130327     *** * - nome della coda di stampa
195900130327     ***c                   eval      Qcmd = %replace(wOUTQ:Qcmd:
196000130327     ***c                                    %scan('&OUTQ':Qcmd))
196100130327     *** * - tipo di modulo
196200130327     ***c                   eval      Qcmd = %replace(wFORM:Qcmd:
196300130327     ***c                                    %scan('&FORMTYPE':Qcmd))
196400130327     *** *
196500130327     ***c                   call      'QCMDEXC'                            91
196600130327     ***c                   parm                    Qcmd
196700130327     ***c                   parm                    Qlen
196800130327     *** *
196900130327     ***c                   ENDSR
197000130327
197100081203      *---------------------------------------------------------------*
197200081203
197300081203     c     SR_stampe     BEGSR
197400130327      *
197500130327     ***C* PER CMD8 STAMPO LA BOLLA
197600130327     ***C     *INKH         IFEQ      *ON
197700130327     ***C     BLPFST        ANDNE     'N'
197800130327     ***c                   clear                   FNLSB5DS
197900130327     ***C                   MOVEL     'V'           DB0RIS
198000130327     ***C                   MOVEL     'P'           DB0TBO
198100130327     ***C                   Z-ADD     wLPAAS        DB0AAS
198200130327     ***C                   Z-ADD     BLPLNP        DB0LNP
198300130327     ***C                   Z-ADD     BLPNRS        DB0NRS
198400130327     ***C                   Z-ADD     wLPNSP        DB0NSP
198500130327     ***c                   call      D90psl
198600130327     ***c                   parm                    FNLSB5DS
198700130327     ***c                   parm                    FNLSB6ds1
198800130327     ***C                   ENDIF
198900081203     c*
199000081203     c* GESTIONE STAMPA SPEDIZIONE
199100081203     c                   if        v1cnos='S'
199200081203     c                   if        v1cggl > 0
199300081203     c* calcolo giorni lavorativi fra data spedizione e data immissione
199400081203     c                   clear                   wdat8
199500081203     c                   z-add     blpdim        datada
199600081203     c                   z-add     wlpdsp        dataa
199700081203     c                   call      'XSRLAV8'
199800081203     c                   parm                    wdat8
199900081203     c                   if        (ggl-1) > v1cggl
200000081203     c                   if        blpfns = 'S'
200100081203     c                   seton                                        02
200200081203     c                   else
200300081203     c                   setoff                                       02
200400081203     c                   endif
200500081203     c* inverto data ritiro per stampa
200600081203     c     *iso          move      blpdim        data_eur
200700081203     c                   move      data_eur      p1cdim
200800081203     c   98              write     testa
200900081203     c                   setoff                                       98
201000081203     c                   write     rigsped
201100081203     c                   endif
201200081203     c                   endif
201300081203     C**
201400081203     c** Se passati xx giorni di sospensione, invio msg al CED
201500081203     c                   if        (ggl-1) > §3iggm
201600081203     c                   add       1             bollemsg          9 0
201700111017     c* memorizzo la data più bassa di immissione
201800111017     c                   if        savdim=0 or blpdim<savdim
201900111017     c                   eval      savdim=blpdim
202000111017     c                   endif
202100111017     c
202200081203     c                   endif
202300081203     c                   endif
202400081203     c                   endsr
202500081203      *---------------------------------------------------------------*
202600081203
202700081203     c     SR_nuovonumeroBEGSR
202800081203     c                   clear                   fnls65ds
202900081203     c                   z-add     waas          I65AAS
203000081203     c                   z-add     blplnp        I65LNP
203100081203     c                   z-add     blpnrs        I65NRS
203200081203     c                   z-add     blpnsp        I65NSP
203300081203     c                   z-add     waasn         I65AASN
203400081203     c                   z-add     wlpdsp        I65MGSN
203500081203     c                   if        v1cnos='S'
203600081203     c                   movel     'S'           I65AGDRT
203700081203     c                   if        *in03 and v1cdcr>0 and blpdcr>0
203800081203     c                   movel     'S'           I65AGDCR
203900081203     c                   move      v1ctcr        I65TCRN
204000081203     c                   move      wdcr          I65DCRN
204100081203     c                   move      v1chcr        I65HCRN
204200081203     c                   endif
204300081203     c                   endif
204400081203     c                   move      '1'           I65STRCMT
204500081203     c                   move      '1'           I65FCMT
204600081203     c                   move      '1'           I65CMTRLBK
204700081203     c                   movel     fnls65ds      kpjbu
204800081210     c                   call      'FNLS65R'
204900081203     c                   parm                    kpjba
205000081203     c                   movel     kpjbu         fnls65ds
205100081203     c                   if        o65err=*blanks
205200081203     c                   add       1             bollema           9 0
205300081205     c                   z-add     waasn         wlpaas
205400081205     c                   z-add     o65nsp        wlpnsp
205500081203     c                   exsr      sr_stampe
205600081204     c                   else
205700081205     c                   add       1             werror            6 0
205800081210     c                   if        o65err='1'
205900081210     c                   movel     o65msg        wmsg
206000081210     c                   endif
206100081203     c                   endif
206200081203     c                   endsr
206300111018     c*-------------------------------------------------------------------
206400111018     c     INV_email     BEGSR
206500111018     c
206600111018     c                   if        not %open(prtemail)
206700111018     C                   EXSR      SR_OPENPRTF
206800111018     C                   endif
206900111018     c
207000111018     c                   exsr      Routend
207100111018     c
207200111018     c                   ENDSR
207300111018
207400111018      /free
207500111018       //--------------------------------------------------------------
207600111018       //?Override al file di stampa per impostarvi i dati per
207700111018       //?  l'invio via e-mail   +   stampa inizio e-mail
207800111018       //--------------------------------------------------------------
207900111018       BEGSR sr_OpenPrtF;
208000111018
208100111018         // Override al file di stampa per impostarvi i dati per
208200111018         // l'invio via e-mail
208300111018           exsr sr_Override;
208400111018
208500111018           open PrtEMAIL;
208600111018
208700111018         // Stampa una testata se NON è richiesta la e-mail
208800111018           IF  §MRAdreg =  *blank;
208900111018             O_testo = JobUser + ' - ' + SDSpgm
209000111018                     + ' - ' + %editc( *date : 'Y' )
209100111018                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
209200111018             except PrtDet;
209300111018             clear O_testo;
209400111018             except PrtDet;
209500111018             except PrtDet;
209600111018           ENDIF;
209700111018
209800111018         // Stampa testo iniziale
209900111018           O_testo = 'E'' stata manutenzionata la DATA SPEDIZIONE +
210000111019                      di bolle immesse da almeno xx giorni.' ;
210100111019           %subst(O_testo:71:2) = (%editc(§3iggm: '4'))     ;
210200111018           except PrtDet;
210300111019           O_testo = 'Verificare col mittente o procedere alla lor+
210400111019                      o chiusura come "Merce MAI Affidata".' ;
210500111019           except PrtDet;
210600111018
210700111018         // Stampa una riga vuota
210800111018           clear O_testo;
210900111018           except PrtDet;
211000111018
211100111018         // Stampa intestazione
211200111019           O_testo = ' Bolle spostate in sospensione di          Data imm+
211300111019                     issione    Bolle da vericare elencate';
211400111018           except PrtDet;
211500111019           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
211600111019                     chia       in apposita stampa.Utente:';
211700111019           %subst(o_testo:90:10)= knmus     ;
211800111019           except PrtDet;
211900111018
212000111018         // Stampa una riga vuota
212100111018           clear O_testo;
212200111018           except PrtDet;
212300111019           zz = 1 ;
212400111019
212500111019          dow     invbo(zz)>0   ;
212600111019          dataiso=%date(invdim(zz))  ;
212700111019          data_eur=dataiso  ;
212800111019
212900111019           clear O_testo;
213000111019           %subst(O_testo:6:20) = invcod(zz)  ;
213100111019           %subst(O_testo:46:10) =%char(data_eur)  ;
213200111019           %subst(O_testo:64:7 ) =%editc(invbo(zz):'2')  ;
213300111019           except PrtDet;
213400111019
213500111019           zz=zz+1  ;
213600111019           enddo    ;
213700111018
213800111018       ENDSR;
213900111018
214000111018       //--------------------------------------------------------------
214100111018       //?Override al file di stampa per impostarvi i dati per
214200111018       //?  l'invio via e-mail   +   stampa inizio e-mail
214300111018       //--------------------------------------------------------------
214400111018       BEGSR sr_Override;
214500111018
214600111018         reset TRTCM1ds;
214700111018
214800111018         IF  §MRAdreg <> *blank;
214900111018           §CM1mitt = %trim(§MRAdmitt);
215000111018           §CM1dst  = %trim(§MRAddest);
215100111019
215200111019         // Valorizzo i campi della e-m@ail
215300130729          chain  DUTpou   azorg01l ;
215400111019
215500111019          if   %found(azorg01l) ;
215600120802           §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
215700111019          endif  ;
215800111019
215900111018           §CM1tips = §MRAdreg;
216000111018           §CM1po   = %editc(simfel:'X');
216100111019           §CM1var  = '*OBJM*' + 'Manutenzione DATA SPEDIZIONE bolle'   ;
216200111018           §CM1idp  = §MRAdidpro;
216300111018           Qcmd = C_CmdOvrPrtF
216400111018                + ' outq(' + %trim(§MRAdoutqi) + ')'
216500111018                + ' usrdfndta(''' + TRTCM1ds + ''')';
216600111018         ELSE;
216700111018           Qcmd = C_CmdOvrPrtF;
216800111018         ENDIF;
216900111018
217000111018         callp(e) qCmdExc(Qcmd : %size(Qcmd));
217100111018
217200111018       ENDSR;
217300111018
217400130718       //--------------------------------------------------------------
217500130718       //?Cerca cliente in tabella BSP
217600130718       //--------------------------------------------------------------
217700130718       BEGSR cer_bsp;
217800130718       ksif=knsif;
217900130718       setll (kcod: klin: ksif) tntbe02l;
218000130718       if not %equal(tntbe02l);
218100130718          clear ksif;
218200130718          setll (kcod: klin: ksif) tntbe02l;
218300130718       endif;
218400130718       if %equal(tntbe02l);
218500130718          reade (kcod: klin: ksif) tntbe02l;
218600130718          dow not %eof(tntbe02l) and wbsp=' ';
218700130729             if tbeatb=' ' and tbeke2=v1cksc;
218800130718                wbsp='S';
218900130718                dbsp=tbeuni;
219000130718             endif;
219100130718             reade (kcod: klin: ksif) tntbe02l;
219200130718          enddo;
219300130718       endif;
219400130718       endsr;
219500111018       //--------------------------------------------------------------
219600111018       //?Operazioni finali.
219700111018       //--------------------------------------------------------------
219800111018       BEGSR RoutEnd;
219900111018
220000111018         if %open(PrtEMAIL);
220100111018
220200111018         //?Chiusura dello spool?
220300111018           clear O_testo;
220400111018           except PrtDet;
220500111018           O_testo = '***   Fine Lista   ***';
220600111018           except PrtDet;
220700111018
220800111018           close PrtEMAIL;
220900111018
221000111018         //?Eliminazione overflow?
221100111018           Qcmd = C_CmdDltOvr;
221200111018           callp(e) qCmdExc(Qcmd : %size(Qcmd));
221300111018
221400111018         endif;
221500111018
221600111018       ENDSR;
221700111018
221800111018      /end-free
221900111018     c*-------------------------------------------------------------------
222000061117     Oqsysprt   E    80n80   DET         1  1
222100061005     o                                              'Bolla:'
222200061005     O                       BLPaas        Z   +  1
222300061005     O                       BLPlnp            +  1
222400061005     O                       BLPNRS        Z   +  1
222500061005     O                       BLPNSP        Z   +  1
222600061005     O                       BLPmgs            +  1 '  /  '
222700061005     o               80                          48 'Cliente    <>'
222800061005     O               80      wksc              +  1
222900061005     o               81                          48 'Terminal   <>'
223000061005     o               82                          48 'Lin Arrivo <>'
223100061005     o               83                          48 'BLPDT1  <>  0'
223200061005     o               84                          48 'Esiste spunta'
223300061005     o               85                          49 'Bolla recupero'
223400061005     o               86                          48 'Prepagato    '
223500061005     o               87                          55 'Prepagato con numfat'
223600030619
223700111018     o* ---------------------------------------------------------------
223800111018     o* ?Spool di stampa (per e-mail).
223900111018     o* ---------------------------------------------------------------
224000111018     oPrtEMAIL  e            PRTdet      1
224100111018     o                       O_testo
224200111018
224300030619**  $Cmd
224400030707OVRPRTF FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
224500030619           USRDTA('Manut.Boll') SHARE(*YES)
224600030619           COPIES(2)
224700060403**  $Cmd2
224800060403OVRPRTF FILE(FNLS03P) SAVE(*YES)
224900061005**  $Cmd3
225000061005OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES) OUTQ(LJ4200CED) USRDTA('EDPES')
225100061012**  DLTOVR
225200061012DLTOVR  FILE(QSYSPRT)
225300060713**         MSG
225400111017MANUTENZIONE DATA SPED.BOLLE oltre xx giorni
225500111018Utente:  xxxxxxxxxx
225600060713Codice Cliente:  xxxxxxx   x
225700060713Bolle manutenzionate: xxx.xxx.xxx
225800081210Sono state manutenzionate
225900070612Se immesso tipo immettere anche la data  Consegna Richiesta
226000070612Data cons.richiesta formalmente errata o inferiore/uguale alla data odierna    4
226100070612Data consegna richiesta non ammessa: oltre                                     5
226200070612Data cons.richiesta  inferiore  alla nuova data spedizione
226300070614Ora consegna richiesta errata
226400090401Linea di partenza:  xxx
226500110704Immettere filiale di emissione valida                                         12
226600110704Immettere numero ORM esistente in archivio                                    13
226700111017Bolle immesse dal                                                             14
226800130711Se richiesta la sospensione automatica, non effettuare alcuna parzializzaz.   15
226900130729Ammessi solo caratteri numerici                                               16
