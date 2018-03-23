000100130313     /*PRM dbgview(*source)
000200130313     /*END
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400130610     H* FNLSU3R *----------------------------------------------------*
000500980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000600130610     h*           passo un numero di spedizione per controllo/aggiornamento
000700111122     H***------------------------------------------------------------*
000800130710     FFNLSU3P   o    E             printer usropn  oflind(*in98)
000900910207     FAZORG01L  IF   E           K DISK
001000151207     F*tntbe02l  IF   E           K DISK
001100151207     Ffibsp03l  IF   E           K DISK
001200170502     Ffnlbl01l  IF   E           K DISK
001300070117     FfNbrv07l  IF   E           K DISK
001400980130     FTABEL00F  IF   E           K DISK
001500910205     FCNACO00F  IF   E           K DISK
001600130715     FFNBLP70L  IF   E           K DISK    infds(fnblpinf0)
001700130715     f                                     rename(fnblp000:fnblp007)
001800081203     FFNBLP01L  UF   E           K DISK    rename(fnblp000:fnblp001)
001900110705     f                                     infds(blpnrr01)
002000130705     FFNBLT01L  iF   E           K DISK
002100010326     FFISGN05L  UF   E           K DISK
002200050318     FfIAR601L  UF   E           K DISK
002300130710     FfIAR401L  iF   E           K DISK
002400050318     FfIAR701L  UF   E           K DISK
002500111018     FQSYSPRT   O    F  132        PRINTER OFLIND(*InOA)  usropn
002600111018
002700111018     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
002800020610
002900020610     d sklnp           s              3  0 dim(30)
003000020610
003100130715     D L1              S              3  0 DIM(30)
003200130715
003300030619     d $Cmd            s             80    dim(3) ctdata perrcd(1)
003400060403     d $Cmd2           s             80    dim(1) ctdata perrcd(1)
003500061005     d $Cmd3           s             80    dim(1) ctdata perrcd(1)
003600061012     d DLTOVR          s             80    dim(1) ctdata perrcd(1)
003700980202     D*
003800130705     D BSPlnp          S              3  0 DIM(599)
003900130705     D BSPlnpBO        S              7  0 DIM(599)
004000130705     D BSPlnpD         S              8  0 DIM(599)
004100130705     d
004200151209     D BSPkscC         S              7  0 DIM(799)
004300130705     D BSPkscBO        S              7  0 DIM(799)
004400150703     D BSPkscD         S              8  0 DIM(799)
004500130717
004600130717     D WrkStringaSql   S           4500
004700130717     D                                     VARYING
004800130717     d $Finerec        s              1    inz(*off)
004900130717     d rrnblp          s             15  0
005000020610
005100130717     d blp_mm          s              2  0
005200020610     d lnp             s              3  0
005300020610     d xx              s              2  0
005400130705     d YY              s              4  0 inz
005500130705     d zz              s              4  0 inz
005600130705     d x               s              4  0 inz
005700130705     d y               s              4  0 inz
005800130705     D Indx            S              3  0 inz
005900130705     D Writerig        S              4  0 inz
006000151207     d ix              s              2  0
006100030619      *
006200030619     d Qlen            s             15  5 inz(150)
006300030619      *
006400030619     d Wfile           s             10    inz
006500030619     d Woutq           s             10    inz
006600030619     d Wform           s             10    inz
006700060405     d data_eur        s               D   datfmt(*eur)
006800070612     d Dataiso         s               d   datfmt(*iso)
006900130702     d
007000130705     D DSPsosp         S              8  0
007100130705     D AAAsosp         S              4  0
007200021203      *
007300111018
007400111018      // - Status
007500111018     d Psds           sds
007600111018     d   SDSpgm          *proc
007700111018     d   JobUser             254    263
007800111018
007900111018      // - Campi per QCMDEXC
008000111018     d Qcmd            s            512    inz
008100111018     d
008200111018      // - Esecuzione comando di sistema
008300111018     d qCmdExc         pr                  extpgm('QCMDEXC')
008400111018     d  Qcmd                        512    const  options(*varsize)
008500111018     d  Qlen                         15  5 const
008600111018
008700130715     D trul06ds      E DS
008800130715     D  LIN                    1     90  0
008900130715     D                                     DIM(30)
009000051209     D* DS PER TIBS02R - GESTIONE TNTBE00F
009100051209     D TIBS02DS      E DS
009200111018      // - Tabella "MRA" = Bart-Mailing - Danni
009300111018     d dMRAdan       e ds                  inz
009400111018
009500070903     D og143         E DS
009600081211     d
009700081211     dwidmsall         ds
009800081211     Dwidms1                         32
009900081211     Dwidms2                         32
010000081211     Dwidms3                         32
010100081211     Dwidms4                         32
010200081211     D***
010300130717     d fnblpds       e ds                  extname(fnblp00f) inz
010400941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010500980202     D WGIDAT          DS                  INZ
010600980202     D  GIODAT                 1      8  0
010700980202     D  GIOINV                 9     16  0
010800980202     D  GIOTGI                17     21  0
010900941227     D***
011000941227     D WLBDAT          DS                  INZ
011100941227     D  G02DAT                 1      8  0
011200941227     D  G02INV                 9     16  0
011300941227     D  G02ERR                17     17
011400941227     D  G02TGI                18     22  0
011500980202     D                 DS
011600980202     D  WAA                    1      4  0
011700980202     D  WMM                    5      6  0
011800980202     D  WGG                    7      8  0
011900980202     D  WDATA                  1      8  0
012000060403
012100060403     d WDAT8           DS
012200060403     d  datada                 1      8  0
012300060403     d  dataa                  9     16  0
012400060403     d  ggl                   17     21  0
012500130717     D***              DS
012600130717     D**blpaas                 1      4  0
012700130717     D**blpmgs                 5      8  0
012800130717     D**blpaamm                1      6  0
012900130717     D**blpdsp                 1      8  0
013000060403
013100941116     D KPJBA         E DS
013200910205     D TCUDS           DS
013300910205     D  F1                     1      1
013400910205     D  F3                     3      3
013500910205     D  F2                     2      2
013600910205     D  F4                     4      4
013700910205     D  F56                    5      6
013800941227     D CNCR80        E DS
013900941227     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
014000000000     D  TCU                  398    697
014100941116     D                                     DIM(50)
014200000000     D  KCU                  698    847P 0
014300000000     D                                     DIM(50)
014400000000     D                                     PACKEVEN
014500081203      * Ds per pgm che mette in sospensione rinumerando la spedizione
014600081203     d FNLS65DS      e ds                  inz
014700030619      *
014800980129     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
014900980129     D DSLV55        E DS                  EXTNAME(FNLV55DS)
015000130717     D*fnblpinf        ds
015100130717     D* fnblpnrri            397    400i 0
015200130715     D fnblpinf0       ds
015300130715     D  fnblpnrr0            397    400i 0
015400110705     D blpnrr01        ds
015500110705     D  fnblpnrr01           397    400i 0
015600081203      * DS Esterna x gestione allocazione ed invio messaggi
015700081203     D TRUL82DS      E DS
015800130702     d
015900130702     D trul13ds      E DS
016000130702     d
016100980130     D DS3A          E DS
016200090427     D DS1B          E DS
016300081203     D Ds3c          E DS
016400980202     D DS3I          E DS
016500151207     D*DBSP          E DS
016600140205     D og140         E DS
016700130702
016800130702     D fnlsu3ds        DS
016900151006     d* FLG='R' --> pgm richiamato dal ripristino spedizioni chiuse per
017000151006     d*             merce mai affidata
017100130702     D  ilsu3flg               1      1
017200130702     D  ilsu3aas               2      5
017300130702     D  ilsu3lnp               6      8
017400130702     D  ilsu3nrs               9     10
017500130702     D  ilsu3nsp              11     17
017600130702     D  ilsu3DSP_B            18     25
017700151006     D  ilsu3DSP_S            26     33
017800130705     D  ilsu3nos              34     34
017900130705     d
018000130705     d  ilsu3tcr              35     35
018100130705     d  ilsu3dcr              36     43
018200130705     d  ilsu3hcr              44     47
018300130705     D  olsu3err             100    101
018400130711     D  olsu3boagg           102    110
018500130830     D  olsu3bonagg          111    116
018600150703     D  olsu3msg             117    193
018700150703     D  olsu3sosp            194    194
018800130702     d
018900130710     D  Ilsu3NOAGG           201    201
019000021203
019100151209     D*kcod1           S                   LIKE(tbecod)
019200151209     D*klin            S                   LIKE(tbelin) inz(' ')
019300151209     D*ksif            S                   LIKE(tbesif)
019400110704     D ktrc            S                   LIKE(ar4trc)
019500110704     D ktrcM           S                   LIKE(ar4trc) inz('M')
019600081205     D wlpnsp          S                   LIKE(blpnsp)
019700081205     D wlpaas          S                   LIKE(blpaas)
019800130715     D Klnp            S                   LIKE(blplnp)
019900130715     D Kaas            S                   LIKE(blpaas)
020000130715     D Kmgs            S                   LIKE(blpmgs)
020100130705     D wmsg            S             80    inz
020200090126     D ntwlnp          S              1    inz
020300111214     D savtfp          S              3  0 inz
020400080620     D sosdaL1         S              1    inz
020500111018     d O_testo         s            132    inz
020600130705     d Tiposos         s              1    inz
020700130705     d Unasospesa      s              1    inz
020800130710     d savdsp          s              8  0 inz
020900130715     d wtiBLP          s              1    inz
021000130705     d
021100060713     D* DEFINIZIONE COSTANTI
021200130710     D Ctitolo         C                   CONST('Elenco spedizioni in SOSPENSI-
021300130710     D                                     ONE AUTOMATICA da oltre X GIORNI')
021400081203     D msgjob          C                   CONST('Si sta bloccando la manutenzi-
021500081203     D                                     one data spedizione:USCIRE un moment-
021600081203     D                                     o da questo lavoro')
021700130111     D*ms2wdw          C                   CONST('Probabilmente in manutenzione-
021800130111     D*                                       ')
021900130111     D*ms3wdw          C                   CONST('da altro utente.             -
022000130111     D*                                       ')
022100130111     D*ms4wdw          C                   CONST('Rieseguire il lancio.        -
022200130111     D*                                       ')
022300130111     D ms2wdw          C                   CONST('                             -
022400130111     D                                        ')
022500130111     D ms3wdw          C                   CONST('Segnalare al CED di verificar-
022600130111     D                                     e  ')
022700130111     D ms4wdw          C                   CONST('il JOBLOG                    -
022800130111     D                                        ')
022900111018
023000111018      // - Comando di override al PrtF
023100111018     d C_CmdOvrPrtF    c                   const('OVRPRTF +
023200111018     d                                           file(PRTEMAIL) +
023300111018     d                                           pagesize(66 132) +
023400111018     d                                           lpi(6) cpi(10) +
023500111018     d                                           ovrscope(*actgrpdfn) +
023600111018     d                                           ')
023700111018     d C_CmdDltOvr     c                   const('DLTOVR +
023800111018     d                                            file(PRTEMAIL) +
023900111018     d                                            lvl(*actgrpdfn)')
024000111018      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
024100111018     d TRTCM1ds      e ds                  inz
024200111018      //   ·§CM1mitt = Indirizzo e-mail del mittente
024300120820     d   §CM1mitt    e                     inz(' ed@brt.it')
024400111018      //   ·§CM1dst  = Indirizzo e-mail del destinatario
024500120302     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
024600111018      //   ·§CM1tips = Tipo lettera via e-mail:
024700111018      //    "LET" = testo allegato in corpo con logo
024800111018      //            (richiede righe libere iniziali per il logo)
024900120820      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
025000111018      //            (non consente né UNDERLINE né HIGHLIGHT)
025100120820     d   §CM1tips    e                     inz('COI')
025200111018      //   ·§CM1po   = Filiale
025300111018     d   §CM1po      e                     inz('046')
025400111018      //   ·§CM1var  = Oggetto e-mail
025500111018     d   §CM1var     e                     inz('*OBJM*+
025600111019     d                                     Manutenzione data spedizione+
025700111019     d                                      bolle ')
025800111018      //   ·§CM1sts  = Stato
025900111018     d   §CM1sts     e                     inz(*off)
026000111018      //   ·§CM1sts  = Id processo
026100111018     d   §CM1idp     e                     inz('2')
026200151209     d                 ds
026300151209     d bsplin
026400151209     d sbsp                           3  0 dim(30) overlay(bsplin)
026500111018
026600111018
026700910412     C****************************************************************
026800910412     C*  RIEPILOGO INDICATORI
026900910412     C***************************************************************
027000070612     C* 01    - possibilità di richidere bolle in sospensione x colli non
027100070612     c*         spuntati
027200070612     C* 02    -
027300070612     C* 03    - per colli non spuntati possibilità di modificare la consegna
027400070612     c*         richiesta da tabella CLI
027500070612     C* 12    - PROTEZIONE CAMPO LINEA DI PARTENZA XCHE' SONO UN 2° LIVELLLO
027600910412     C* 30    - DI COMODO
027700980129     C* 40-50 - ERRORE
027800980130     C* 51-55 - GESTIONE CHIAVE LETTURA BOLLE
027900070612     C* 70    - posizionam.cursone su DCR
028000070612     C* 90    - ERRORE GENERICO
028100910412     C*****************************************************************
028200941116     C**
028300980130     C     *ENTRY        PLIST
028400980130     C                   PARM                    KPJBA
028500130702     c                   movel     kpjbu         fnlsu3ds
028600130717      /free
028700130717         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028800130717      /end-free
028900130702     c
029000130702     c                   exsr      CartaB
029100130705     c
029200130705     c                   if        olsu3err=*blanks
029300130702     c
029400130702     C* SE PASSATO UN NUMERO DI SPEDIZIONE --> metto in sospensione la bolla
029500130702     c*                         altrimenti elaboro da tabella BSP
029600130702     c
029700150703    2c                   if        ilsu3nsp>*zeros
029800150703    3c                   if        ilsu3aas<=*zeros
029900130702     c                   eval      olsu3err='E1'
030000150604     c                   eval      olsu3msg='Key sped. incompleto o mancante'
030100150703    3c                   endif
030200150703    3c                   if        ilsu3lnp<=*zeros
030300150703     c                   eval      olsu3err='E1'
030400150604     c                   eval      olsu3msg='Key sped. incompleto o mancante'
030500150703    3c                   endif
030600150703    3c                   if        ilsu3nrs<*zeros
030700150703     c                   eval      olsu3err='E1'
030800150604     c                   eval      olsu3msg='Key sped. incompleto o mancante'
030900150703    3c                   endif
031000130702     c
031100150703    3c                   if        olsu3err=*blanks
031200130705     c                   movel     ilsu3nsp      blpnsp
031300130705     c                   movel     ilsu3aas      blpaas
031400130705     c                   movel     ilsu3lnp      blplnp
031500130705     c                   movel     ilsu3nrs      blpnrs
031600150603     c     kblt          chain(n)  fnblp01l
031700150703    4c                   if        %found(fnblp01l)
031800150703     c                   clear                   bollema
031900150603     c                   exsr      elabora
032000150703     c* IMPOSTAZIONE EVENTUALE CODICE DI ERRORE
032100150703     c* Errori di ritorno da pgm di attribuzione nuovo numero sped. per
032200150703     c* sospensione a cavallo di anno (FNLS65R)
032300150703    5c                   if        werror>0
032400150703     c                   move      'E3'          olsu3err
032500150703     c                   move      werror        olsu3bonagg
032600150703     c                   move      wmsg          olsu3msg
032700150703    5c                   endif
032800150703     c* se bolla in sospensione da oltre x giorni restituisco al chiamante
032900150703    5c                   if        unasospesa='S'
033000150703     c                   move      '1'           olsu3sosp
033100150703    5c                   endif
033200150703   x4c                   else
033300150703     c                   move      'E4'          olsu3err
033400150703     c                   eval      olsu3msg='Spedizione non trovata         '
033500150703    4c                   endif
033600130702     c
033700150703    3c                   endif
033800150703   x2c                   else
033900130702     c                   exsr      LeggiBLP
034000130830     c* IMPOSTAZIONE EVENTUALE CODICE DI ERRORE
034100130830     c* Errori di ritorno da pgm di attribuzione nuovo numero sped. per
034200130830     c* sospensione a cavallo di anno (FNLS65R)
034300130830     c                   if        werror>0
034400130830     c                   move      'E3'          olsu3err
034500130830     c                   move      werror        olsu3bonagg
034600130830     c                   move      wmsg          olsu3msg
034700130830     c                   endif
034800130702     c                   endif
034900980202     C*
035000980202     C                   CLEAR                   DSLV55
035100980202     C                   MOVEL     'C'           D55TLA
035200980202     C                   CALL      'FNLV55R'
035300980202     C                   PARM                    DSLV55
035400081203     c*
035500081203     C                   CLEAR                   fnls65ds
035600081203     C                   MOVEL     'C'           i65tla
035700081211     c                   movel     '1'           I65STRCMT
035800081203     c                   movel     fnls65ds      kpjbu
035900081205     C                   CALL      'FNLS65R'
036000081203     C                   PARM                    kpjba
036100980202     C**
036200061012     c
036300061012     c                   clear                   qcmd
036400061012     c                   eval      qcmd = dltovr(1)
036500061012     c                   call      'QCMDEXC'                            91
036600061012     c                   parm                    Qcmd
036700061012     c                   parm                    Qlen
036800130705     C**
036900130705     c                   endif
037000111018     c
037100130711     c*
037200130711     c                   if        bollema>0
037300130711     c                   movel     bollema       olsu3boagg
037400130830     c***                movel     fnlsu3ds      kpjbu
037500130711     c                   endif
037600130830
037700130830     c                   movel     fnlsu3ds      kpjbu
037800130830
037900130830     c* sottometto azione a titolo di documentazione di sospensione
038000130830     c* lanciata
038100150610     c                   if        ilsu3nsp<=*zeros
038200130830     C                   MOVEL     'LS0U'        KCOAZ
038300130830     C                   CALL      'BCH10'
038400130830     C                   PARM                    KPJBA
038500150610     c                   endif
038600130711     c
038700000000     C                   SETON                                        LR
038800941116     C*
038900980130     C*****************************************************************
039000980130     C* ELABORAZIONE TESTATA E DETTAGLIO BOLLE ----------------------
039100980130     C*****************************************************************
039200130702     C     LeggiBLP      BEGSR
039300980130     C*
039400081205     c                   clear                   werror
039500081210     c                   clear                   wmsg
039600130705     c                   clear                   bollema
039700130705     c* Imposto la "S" di sospensione atomatica per spunte
039800130705     c                   eval      ilsu3nos='S'
039900130702     c
040000151207     c* Carico da FIBSP00F le linee / ksc che devo elaborare
040100151207     C****               movel     'BSP'         kcod1
040200130702     C                   Z-ADD     1             X
040300130702     C                   Z-ADD     1             y
040400151207     c     ' '           setll     fibsp03l
040500151207     c     ' '           reade     fibsp03l
040600151207     c                   dow       not %eof(fibsp03l)
040700151207     c* elaboro solo se non escluso dalla elaborazione automatica
040800151207     c                   if        bspaut<>'N'
040900151207     c                   if        bspfil>0
041000151207     c                   if        %lookup(bspfil:L1)>0
041100151207     c                   eval      BSPlnp(y)=bspfil
041200151207     c                   add       1             y
041300151207     c                   endif
041400151207     c                   else
041500151207     c                   z-add     1             Ix
041600151207     c                   dow       Ix<=%elem(sbsp) and sbsp(Ix)>0
041700151207     c                   if        %lookup(sbsp(Ix):L1)>0
041800151209     c                   eval      BSPkscC(x)=bspksc
041900151207     c                   add       1             x
042000151207     c                   eval      ix=%elem(sbsp)
042100151207     c                   endif
042200151207     c                   add       1             Ix
042300151207     c                   enddo
042400151207     c                   endif
042500151207     c                   endif
042600151207     c     ' '           reade     fibsp03l
042700151207     c                   enddo
042800151207
042900151207     C***  kcod1         setll     tntbe02l
043000151207     C***  kcod1         reade     tntbe02l
043100130702     c
043200151207    1c***                dow       not %eof(tntbe02l)
043300151207    2c***                if        tbeatb=' '
043400140703     c* elabolo solo se non escluso dalla elaborazione automatica
043500151207     c***                eval      dbsp=tbeuni
043600151207   2ac***                if        §bspaut<>'N'
043700130702     c* carico solo il terminal
043800151207    3c***                if        tbeke1='000            '
043900151207    4c***                if        %subst(tbeke2:1:3)>*zeros
044000151207     c***                movel     tbeke2        w0030
044100151207     c***                eval      Indx=%lookup(w0030:l1)
044200151207    5c***                if        indx>0
044300151207     c***                eval      BSPlnp(y)=%int(%subst(tbeke2:1:3))
044400151207     c***                add       1             y
044500151207    5c***                endif
044600151207    4c****               endif
044700130715     c
044800151207   x3c****               else
044900130715     c
045000151207     c***                movel     tbeke1        w0030             3 0
045100130702     c
045200151207    4c****               if        simfel=w0030
045300151207    5c***                if        %subst(tbeke2:1:7)>*zeros
045400151207     c****               eval      BSPksc(x)=%int(%subst(tbeke2:1:7))
045500151207     c***                add       1             x
045600151207    5c****               endif
045700130702     c
045800151207    4c****               endif
045900151207    3c****               endif
046000151207   2ac***                endif
046100151207    2c***                endif
046200130702     c
046300151207     C***  kcod1         reade     tntbe02l
046400151207    2c***                enddo
046500130702     c
046600130702     c* Se nessuna linea/ksc trovata --> esco dalla elaborazione
046700151209     c                   if        BSPlnp(1)=0 and BSPkscC(1)=0
046800130702     c                   leavesr
046900130702     c                   endif
047000130702     C*
047100130710     c                   open      fnlsU3p
047200130702     c
047300130705     c                   move      wGGL          w_gg              1
047400060404     c                   eval      ptitolo = ctitolo
047500060404     c                   eval      ptitolo = %xlate('X':w_gg:ptitolo)
047600090127     c                   eval      *in13='0'
047700130708     c* Imposto nella testa della stampa il terminal
047800130708     c                   eval      *in14='1'
047900130708     c                   eval      p1clnp=simfel
048000130708     c     simfel        chain     azorg01l
048100130708     c                   if        %found(azorg01l)
048200130708     c                   movel     orgdes        p1dlnp
048300130708     c                   endif
048400130708     c
048500060403     c                   write     testa
048600061005     c
048700061005     c                   open      qsysprt
048800130715     c
048900130715     c* Elaboro prima le bolle per linea di partenza
049000130715     c*  da mettere ibn sospensione
049100130715     c                   z-add     1             x
049200130715     c                   eval      tiposos='L'
049300130715     c
049400130715     c                   dow       bsplnp(X)>0
049500130715     c                   eval      klnp=BSPlnp(X)
049600130715     c                   eval      indx=X
049700130715     c
049800130715     c     kblp70        setll     fnblp70l
049900130715     c     kblp70        reade     fnblp70l
050000130715     c
050100130715     c                   dow       not %eof(fnblp70l)
050200130715     c                   exsr      Elabora
050300130715
050400130715     c     kblp70        reade     fnblp70l
050500130715     c                   enddo
050600130715     c
050700130715     c                   seton                                        98
050800130715     c                   add       1             x
050900130715     c                   enddo
051000130702     c
051100130702     c* Elaboro tutte le bolle ancora da borderizzare
051200130715     c* Per cod cliente
051300130702     c*
051400130715     c                   eval      tiposos='K'
051500130717     c                   eval      indx=1
051600151209     c                   dow       bspkscC(indx)>0 and indx<=799
051700130717      /free
051800130717       WrkStringaSql='select rrn(fnblp00f) as RRNBLP, fnblp00f.* +
051900130717        from fnblp00f +
052000151209        where (blpccm=' + %editc(bspkscC(indx):'X') + ' or blpksc=' +
052100151209        %editc(bspkscC(indx):'X') +  ') and blptfp=' + %editc(simfel:'X') +
052200130717        ' and blpnfv=0';
052300130717
052400130717        $finerec=*off;
052500130717        exec sql prepare STRINGASQL from :WrkStringaSql;
052600130717        exec sql declare BLPCsr cursor for StringaSql;
052700130717        exec sql open BLPCSR;
052800130717        dow $finerec=*off;
052900130717           exec sql Fetch BLPcsr into :rrnblp, :fnblpds;
053000130717           if sqlcod=100 or sqlcod<0;
053100130717              $finerec = *on;
053200130717              leave;
053300130717           endif;
053400130717           exsr elabora;
053500130717        enddo;
053600130717        exec sql close BLPcsr;
053700130717      /end-free
053800130717     c                   add       1             indx
053900130717     c                   enddo
054000130717     c**                 clear                   knfv              5 0
054100130717     c**   kblp77        setll     fnblp77l
054200130717     c**   kblp77        reade     fnblp77l
054300130717     c**
054400130717     c**                 dow       not %eof(fnblp77l)
054500130717     c**                 exsr      Elabora
054600980202     C*
054700130717     c**   kblp77        reade     fnblp77l
054800130717   1ac**                 enddo
054900060403     c*
055000060404     c   98              write     testa
055100060404     c                   setoff                                       98
055200060404     c                   write     fines
055300130710     c                   close     fnlsU3p
055400061005     c
055500061005     c                   close     qsysprt
055600060713     c
055700130705     c                   if        Unasospesa='S'
055800111018     c                   exsr      Inv_email
055900130705     c                   endif
056000111018     c
056100060713     c* emetto msg di quante bolle manutenzionate
056200060713     c                   seton                                        28
056300980130     C*
056400980202     C                   ENDSR
056500110704     C*----------------------------------------------------------------
056600130715     c     Elabora       BEGSR
056700130715     c
056800130715     c                   exsr      Parzial
056900130715     c
057000130715     c                   if        NoBolla=' '
057100130715     c
057200130715     c* Se cambia anche l'anno richiamo pgm apposito che esegue anche la
057300130715     c* rinumerazione della spedizione
057400160107
057500160107     c* Anche se tecnicamente funziona,
057600160107     c* la sospensione all'indietro a cavallo di anno è stata impedita
057700160107     c* per richiesta di Luciano. (V.di controllo nel fnls03r)
057800160107    2c                   if        aaasosp<>blpaas
057900130715     c                   exsr      sr_nuovonumero
058000130715     c                   else
058100130715     c
058200130715     c                   exsr      Aggiorna
058300130715    2c                   endif
058400130715     c                   endif
058500130715     c                   ENDSR
058600130715     C*----------------------------------------------------------------
058700110704     c     Parzial       BEGSR
058800110704     c                   clear                   Nobolla           1
058900130702     c
059000130702     c* Escludo le bolle con data spedizione > data passata
059100130717    1c****               if        blpdsp>wdtcar
059200130717     c                   if        (blpaas*10000)+blpmgs>wdtcar
059300130702     c                   eval      NoBolla='N'
059400130702     C                   leavesr
059500130702    1c                   endif
059600130702      *
059700130702      ** CONTROLLO se bolla trasmessa
059800130702    2C     BLPDT1        IFNE      *ZEROS
059900130702     c                   eval      NoBolla='N'
060000130702     C                   leavesr
060100130702    2C                   ENDIF
060200130702     c*
060300130702      * SCARTO I PREPAGATI ANNULLATI
060400130702     c                   if        blpcbo='M '
060500130702     c                   eval      NoBolla='N'
060600130702     c                   leavesr
060700130702     c                   endif
060800130702     c
060900130702      *  CONTROLLO IL CODICE CLIENTE
061000130702     C     BLPCCM        IFNE      *ZEROS
061100130702     C                   Z-ADD     BLPCCM        WKSC              7 0
061200130702     C                   ELSE
061300130702     C                   Z-ADD     BLPKSC        WKSC
061400130702     C                   ENDIF
061500130715     c**
061600130702     c* controllo se linea tutta da mettere in sospensione
061700130715     c**                 eval      tiposos='L'
061800130715     c**                 eval      indx=%lookup(blplnp:BSPlnp)
061900130715     c**
062000130715    1c**                 if        Indx=0
062100130715     c**
062200130715     c**                 eval      Indx=%lookup(wksc:BSPksc)
062300130715    2c**                 if        Indx=0
062400130715     c**                 eval      NoBolla='N'
062500130715     C**                 leavesr
062600130715     c**                 else
062700130715     c**                 eval      tiposos='K'
062800130715    2C**                 endif
062900130715    1C**                 endif
063000130715     c
063100130717    1c***                if        tiposos='K'
063200130717     c***                eval      Indx=1
063300130717     c***                eval      Indx=%lookup(wksc:BSPksc)
063400130717    2c***                if        Indx=0
063500130717     c***                eval      NoBolla='N'
063600130717     C***                leavesr
063700130717    2C***                endif
063800130717    1C***                endif
063900130702     c
064000130702     c                   clear                   ntwlnp
064100130702     c     blplnp        chain     azorg01l
064200130702     c                   if        %found(azorg01l)
064300130702     c                   movel     orgde3        og143
064400130702     c                   if        §ogntw='DPD' or §ogntw='EEX' or
064500130702     c                             §ogntw='FED'
064600130702     c                   eval      ntwlnp='E'
064700130702     c                   endif
064800130702    1C                   ENDIF
064900130702     c
065000140114     c* Se richiesta sospensione a cavallo di anno
065100140114     c* sia per linea estera che italia
065200110704     c* se il cliente mantiene il numero spedizione la scarto
065300140114     c*****              if        ntwlnp='E' and AAAsosp > blpaas
065400140114     c                   if        AAAsosp > blpaas
065500110704     C                   MOVEL(P)  '3C'          COD
065600110704     C                   MOVEL(p)  wksc          KEY
065700110704     C     ktab2         chain     tabel00f                           31
065800110704     C   31              clear                   ds3c
065900110704     C  N31              movel     tbluni        ds3c
066000110704     c     §3cfsp        ifeq      'S'
066100110704     c                   eval      NoBolla='N'
066200110704     C                   leavesr
066300110704     c                   endif
066400110704     c                   endif
066500110704      *
066600110704     c                   clear                   wsispu            1
066700110704     c     kblt          setll     fnblt01l
066800110704     c     kblt          reade     fnblt01l                               99
066900110704    2c                   dow       not *in99
067000110704     c
067100110704     c* Cerco una qualsiasi spunta
067200110704     c     kbrv          chain     fnbrv07l
067300110704    3c                   if        %found(fnbrv07l)
067400110704     c                   eval      wsispu='S'
067500110704     c                   seton                                        99
067600110704   x3c                   else
067700110704     c     kblt          reade     fnblt01l                               99
067800110704    3c                   endif
067900110704    2c                   enddo
068000110704     c
068100130702     c*Se trovata  spunta   --> non posso spostare in sospensione
068200151006     c                   if        ilsu3flg<>'R'
068300110704    2c                   if        wsispu='S'
068400110704     c                   eval      NoBolla='N'
068500110704     c                   leavesr
068600110704    2c                   endif
068700151006     c                   endif
068800110704      *
068900110704      ** CONTROLLO IL TIPO BOLLA
069000110704     C                   MOVEL     '3A'          COD
069100110704     C                   MOVEL(P)  BLPCBO        KEY
069200110704     C     KTAB2         CHAIN     TABEL00F                           31
069300110704     C   31              CLEAR                   DS3A
069400110704     C  N31              MOVEL     TBLUNI        DS3A
069500110704      *
069600110704      * SCARTO LE BOLLE DI RECUPERO
0697001107041   2C     §3ARBL        IFEQ      'R'
069800110704     c                   eval      NoBolla='N'
069900110704     C                   leavesr
070000110704    2C                   END
070100170509     c* scarto le bolle legate ma solo se flag <> da "R"
070200170509     c* ( no per ripristino e richiamo da chiusura FGV)
070300170509     c                   if        ilsu3flg<>'R'
070400170502     c     kblt          chain     fnlbl01l
070500170502     c                   if        %found(fnlbl01l)
070600170502     c                   eval      NoBolla='N'
070700170502     C                   leavesr
070800170502    2C                   endif
070900170509    2C                   endif
071000130702     c
071100110704      * SCARTO LE BOLLE con cod trattamento merce che non prevede merce
071200110704     C                   MOVEL     '1B'          COD
071300110704     C                   MOVEL(P)  BLPctm        KEY
071400110704     C     KTAB2         CHAIN     TABEL00F                           31
071500110704     C   31              CLEAR                   DS1b
071600110704     C  N31              MOVEL     TBLUNI        DS1b
071700110704    2C                   IF        §1bfg8='N'
071800110704     c                   eval      NoBolla='N'
071900110704     C                   leavesr
072000110704    2C                   END
072100110704      *
072200110704      * PER I PREPAGATI NON È CONSENTITO SPOSTARE LA DATA BOLLA AL MESE
072300110704      *  SUCCESSIVO
072400130717     C***  WAMSOS        IFGT      blpaamm
072500130717     c                   movel     blpmgs        blp_mm
072600130717     c                   if        wamsos>(blpaas*100)+blp_mm
072700110704      *
072800110704    2C     BLPPDR        IFEQ      *ZEROS
072900110704     c                   eval      NoBolla='N'
073000110704     C                   leavesr
073100110704    2C                   ELSE
073200110704     C                   MOVEL     '1'           KTRC
073300110704     C     KAR6          CHAIN(N)  FIAR6000                           31
073400110704    2C     *IN31         IFEQ      '0'
073500110704    2C     AR6NFT        ANDGT     *ZEROS
073600110704     c                   eval      NoBolla='N'
073700110704     C                   leavesr
073800110704    2C                   ENDIF
073900110704    2C                   ENDIF
074000110704      *
074100110704    2C                   ENDIF
074200110704     c
074300110704     c                   ENDSR
074400110704     c*------------------------------------------------------------------------
074500110704     c     Aggiorna      BEGSR
074600110704     c* A questo punto visto che sono stati superati tutti i controlli
074700110704     c* chaino il record bolla in update
074800110704     c     kblt          chain(E)  fnblp01l
074900110704    1c                   if        %error
075000110704     C                   CLEAR                   TRUL82DS
075100151006     c                   select
075200151006     c                   when      ilsu3nsp>*zeros
075300151006     c                   eval      ul82§rrn = fnblpnrr01
075400151006     c                   when      tiposos='L'
075500130715     c                   eval      ul82§rrn = fnblpnrr0
075600151006     c                   other
075700130717     c****               eval      ul82§rrn = fnblpnrri
075800130717     c                   eval      ul82§rrn = RRNBLP
075900151006     c                   endsl
076000110704     C                   EVAL      UL82§FIL = 'FNBLP00F'
076100110704     C                   EVAL      UL82§WIN = 'N'
076200110704     C                   EVAL      UL82§F7  = 'S'
076300110704     C                   EVAL      UL82§num = 1
076400110704     C                   EVAL      UL82§att = 30
076500110704     C                   EVAL      UL82§mss = msgjob
076600110704     C* Effettuo la chiamata al *PGM d utilità
076700110704     C                   CALL(e)   'TRUL82R'
076800110704     C                   PARM                    TRUL82DS
076900110704     C     kblt          chain     fnblp01l
077000110704    1c                   endif
077100130705     c
077200130705    1c                   if        %found(fnblp01l)
077300111214     c
077400110704      *
077500110704      ** CONTROLLO SE DATA BORDERO' MINORE DELLA NUOVA DATA DI SPEDIZIONE
077600110704      *  SBORDERIZZO
077700130705    2C***  BLPDBR        IFLT      dspsosp
077800110704      *
077900130705   2ac                   if        blpdbr=0
078000110704     C                   CLEAR                   BLPNFV
078100110704     C                   CLEAR                   BLPDBR
078200110704     C                   CLEAR                   BLPFLP
078300130705     c
078400130710     c                   z-add     blpmgs        savdsp
078500160107     c                   movel     blpaas        savdsp
078600110704     C* AGGIORNO LA NUOVA DATA SPEDIZIONE
078700160107     C*****              Z-ADD     AAAsosp       BLPAAS
078800130702     C                   Z-ADD     dspsosp       BLPMGS
078900110704     c* Aggiorno anche la data ritiro merce
079000110704     c*  solo per i clienti che spostanto in sospensione per mancanza di spu
079100110704     c*  nte
079200130705    2c                   if        ilsu3nos='S'
079300130702     C                   movel     dspsosp       BLPdrt
079400110704     c* Se richiesto e se presente consegna richiesta, imposto la nuova data
079500130705    3c                   if        ilsu3dcr>*zeros and blpdcr>0
079600130705     c                   movel     ilsu3tcr      blptcr
079700130705     c                   movel     ilsu3dcr      blpdcr
079800130705     c                   movel     ilsu3hcr      blphcr
079900110704    3c                   endif
080000110704     c
080100110704   x2c                   else
080200110704     c* Se la data ritiro > della data spedizione nuova e non c'e' ORM
080300110704     c*  imposto SEMPRE la data ritiro= data spedizione
080400130702    3c                   if        blpdrt>dspsosp
080500110704     c     kar4m         chain     fiar401l
080600110704    4c                   if        not %found(fiar401l)
080700130702     c                   eval      blpdrt=DSPsosp
080800110704    4c                   endif
080900110704    3c                   endif
081000110704     c
081100110704    2c                   endif
081200110704     c
081300110704     C* E RICACOLO I TERMINAL DI ARRIVO E FILIALE ELABORAZIONE ARRIVI
081400110704     C                   CLEAR                   DSLV55
081500110704     C                   MOVEL     ' '           D55TPT
081600110704     C                   MOVEL     BLPLNA        D55LIN
081700110704     C                   MOVEL     BLPLNP        D55LNP
081800130702     C                   MOVEL     DSPsosp       D55DRF
081900110704     C                   CALL      'FNLV55R'
082000110704     C                   PARM                    DSLV55
082100110704    2C     D55ERR        IFEQ      ' '
082200110704     C                   MOVEL     D55TFA        BLPTFA
082300110704     C                   MOVEL     D55TFP        BLPFEA
082400110704    2C                   ENDIF
082500110704     C* BLPTFP: TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
082600110704     C                   CLEAR                   DSLV55
082700110704     C                   MOVEL     'P'           D55TPT
082800110704     C                   MOVEL     BLPLNP        D55LIN
082900130702     C                   MOVEL     DSPsosp       D55DRF
083000110704     C                   CALL      'FNLV55R'
083100110704     C                   PARM                    DSLV55
083200111214     c
083300111214     c
083400110704    2C     D55ERR        IFEQ      ' '
083500110704     C                   MOVEL     D55TFP        BLPTFP
083600111128     C                   MOVEL     D55TFP        BLPfle
083700110704    2C                   ENDIF
083800110704     C*
083900130710     c                   if        ilsu3NOAGG=' '
084000110704     C                   UPDATE    FNBLP001
084100130710     c                   else
084200130710     c                   unlock    fnblp01l
084300130710     C                   except    stabolla
084400130710     c                   endif
084500130710     c
084600110704     c                   add       1             bollema           9 0
084700110704     c
084800110704     c* Se si tratta di assegnato S.F. tassato in partenza, cancello la
084900110704     c* tassazione
085000130710     c                   if        ilsu3NOAGG=' '
085100130710     c
085200130710     c                   clear                   ktrc
085300110704    2c                   if        §3atb2<>*blanks
085400110704     c                   eval      ktrc='2'
085500110704   x2c                   else
085600110704     c                   movel     §3atb1        flgtb1            1
085700110704    3c                   if        flgtb1='A'
085800110704     c                   eval      ktrc='1'
085900110704    3c                   endif
086000110704    2c                   endif
086100110704     c
086200110704   1ac                   if        ktrc<>*blanks
086300110704     C     KAR6          CHAIN(N)  FIAR6000                           31
086400110704    2c                   if        not *in31
086500110704     c                   move      ar6ksc        fineksc           4 0
086600110704    3c                   if        fineksc<>0000 and fineksc<>9999
086700110704     c* deleto la tassazione
086800110704     c     kar6          setll     fiar701l
086900110704     c     kar6          reade     fiar701l
087000110704    4c                   dow       not %eof(fiar701l)
087100110704     c                   delete    fiar7000
087200110704     c
087300110704     c     kar6          reade     fiar701l
087400110704    4c                   enddo
087500110704     c
087600110704     c     kar6          delete    fiar6000
087700110704    3c                   endif
087800110704    2c                   endif
087900110704   1ac                   endif
088000130710
088100130710     c                   endif
088200110704     c
088300130705     C***
088400130705     C***  KBLT          SETLL     FNBLT01L
088500130705     C***  KBLT          READE     FNBLT01L                               30
088600130705    2C***  *IN30         DOWEQ     *OFF
088700110704     C* SOLO SE NON C'E' GIA' LA DATA DI PARTENZA
088800130705    3C**N30BLTDFV        IFEQ      0
088900110704     C* E LA DATA BORDERO' DELLA TESTATA NON E' MAGGIORE ALLA NUOVA DATA SPED.
089000130705    2C***  BLPDBR        ANDLT     DSPsosp
089100130705     C***                CLEAR                   BLTNFV
089200130705     C***                UPDATE    FNBLT000
089300130705   X3C***                ELSE
089400130705     C***                UNLOCK    FNBLT01L
089500130705    3C***                ENDIF
089600130705     C***  KBLT          READE     FNBLT01L                               30
089700130705    2C***                ENDDO
089800111214     c
089900110704     C* AGGIORNO ANCHE FISGN SE PRESENTE
090000110704     C     KFISGN        SETLL     FISGN05L
090100110704     C     KFISGN        READE     FISGN05L                               30
090200110704    2C     *IN30         DOWEQ     *OFF
090300130702     C                   Z-ADD     DSPsosp       SGNMGS
090400110704     C                   Z-ADD     BLPTFA        SGNTNA
090500111214     c
090600110704     C                   CLEAR                   SGNT6A
090700110704     C                   CLEAR                   SGNT6B
090800110704     C                   CLEAR                   SGNT6C
090900110704     C                   CLEAR                   SGNT6D
091000110704     C                   CLEAR                   SGNT6E
091100110704     C                   CLEAR                   SGNT6F
091200110704     c                   eval      sgndatora = %char(%timestamp:*iso0)
091300130710     c                   if        ilsu3NOAGG=' '
091400110704     C                   UPDATE    FISGN000
091500130710     c                   else
091600130710     c                   unlock    fisgn05l
091700130710     C                   except    stasgn
091800130710     c                   endif
091900110704     C     KFISGN        READE     FISGN05L                               30
092000110704    2C                   ENDDO
092100110704     C*
092200110704     c* richiamo routine per gestione stampe
092300110704     c                   z-add     blpaas        wlpaas
092400110704     c                   z-add     blpnsp        wlpnsp
092500150604
092600150702     c****               if        ilsu3nsp<=*zeros
092700110704     c                   exsr      sr_stampe
092800150702     c****               endif
092900110704     c*
093000130705   2ac                   endif
093100130705    1c                   endif
093200110704     c
093300110704     c                   ENDSR
093400980130     C*----------------------------------------------------------------
093500980130     C*
093600980130     C*--- OPERAZIONI INIZIALI ---------------------------------------*
093700980130     C     *INZSR        BEGSR
093800980130     C***
093900980130     C* KLIST
094000130705     C*
094100130715     C     KBLP70        KLIST
094200130715     c                   kfld                    klnp
094300130715     C                   KFLD                    kAAS
094400130715     C                   KFLD                    kMGS
094500130705     C     KBLT          KLIST
094600130705     C                   KFLD                    BLPAAS
094700130705     C                   KFLD                    BLPLNP
094800130705     C                   KFLD                    BLPNRS
094900130705     C                   KFLD                    BLPNSP
095000110704     C*
095100110704     C* KEY PER AGGIORNARE fiar4
095200080331     C     Kar4M         KLIST
095300080331     C                   KFLD                    BLPAAS
095400080331     C                   KFLD                    BLPLNP
095500080331     C                   KFLD                    BLPNRS
095600080331     C                   KFLD                    BLPNSP
095700110704     C                   KFLD                    ktrcM
095800051209     C     KBrv          KLIST
095900051209     C                   KFLD                    bltfls
096000051209     C                   KFLD                    bltlna
096100051209     C                   KFLD                    bltNRS
096200051209     C                   KFLD                    bltnsc
096300001120     C* KEY PER AGGANCIARE  FIAR6
096400001120     C     KAR6          KLIST
096500001120     C                   KFLD                    BLPAAS
096600001120     C                   KFLD                    BLPLNP
096700001120     C                   KFLD                    BLPNRS
096800001120     C                   KFLD                    BLPNSP
096900001120     C                   KFLD                    KTRC              1
097000980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 2
097100130717     C**   KBLP77        KLIST
097200130717     c**                 kfld                    simfel
097300130717     C**                 KFLD                    knfv
097400010326     C* ACCESSO   FILE FISGN PER NUMERO SPEDIZIONE
097500010326     C     KFISGN        KLIST
097600010326     C                   KFLD                    BLPAAS
097700010326     C                   KFLD                    BLPLNP
097800010326     C                   KFLD                    BLPNRS
097900010326     C                   KFLD                    BLPNSP
098000980130     C* ACCESSO   TABEL
098100980130     C     KTAB2         KLIST
098200980130     C                   KFLD                    CODUT
098300980130     C                   KFLD                    COD               2
098400980130     C                   KFLD                    KEY               8
098500980130     C***
098600980130     C* DEFINIZIONE CAMPI
098700980130     C***
098800980130     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
098900980130     C***
099000980130     C                   CALL      'X§PARUT'
099100980130     C                   PARM                    UT§DSE
099200980202     C                   MOVEL     RAGUT         RSUT
099300980130     C                   MOVEL     REC80         CNCR80
099400980130     C*--- RICERCA CAPOCONTI
099500130705     C                   DO        50            X
099600980130     C                   MOVE      TCU(X)        TCUDS
099700980130     C     F56           CABNE     'CG'          END1
099800980130     C     F4            COMP      '1'                                    21
099900980130     C     F4            COMP      '2'                                    22
100000980130     C     F4            COMP      '3'                                    23
100100980130     C     F4            COMP      '6'                                    27
100200980130     C** 1 CLIENTI   21
100300980130     C** 2 FORNITORI 22
100400980130     C** 3 AGENTI    23
100500980130     C     F3            COMP      '0'                                242425
100600980130     C     F3            COMP      'I'                                    26
100700980130     C** 0 ITALIA   25
100800980130     C** 1 ESTERO   24
100900980130     ** I CAPO CONTO IVA
101000980130     C   21
101100980130     CAN 24              Z-ADD     KCU(X)        KCE               4 0
101200980130     C   21
101300980130     CAN 25              Z-ADD     KCU(X)        KCI               4 0
101400980130     C   22
101500980130     CAN 24              Z-ADD     KCU(X)        KFE               4 0
101600980130     C   22
101700980130     CAN 25              Z-ADD     KCU(X)        KFI               4 0
101800980130     C   23
101900980130     CAN 24              Z-ADD     KCU(X)        KAE               4 0
102000980130     C   23
102100980130     CAN 25              Z-ADD     KCU(X)        KAI               4 0
102200980130     C   26              Z-ADD     KCU(X)        KIVA              4 0
102300980130     C   27              Z-ADD     KCU(X)        KBNA              4 0
102400980130     C     END1          TAG
102500980130     C                   END
102600980130     C                   SETOFF                                       212223
102700980130     C                   SETOFF                                       242526
102800980130     C                   SETOFF                                       27
102900980202     C***
103000980202     C* CARICO DATI VARIABILI PARTENZE
103100980202     C***
103200980202     C                   MOVEL     '3I'          COD
103300980202     C                   MOVEL     '1       '    KEY
103400980202     C     KTAB2         CHAIN     TABEL00F                           31
103500980202     C  N31              MOVEL     TBLUNI        DS3I
103600980202     C   31              CLEAR                   DS3I
103700130705     c*
103800130705     c                   z-add     3             wGGL              1 0
103900980130     C*
104000980130     C* DATA BOLLETTAZIONE:  TRA UDATE E DATA GIORNO + §3IGGS
104100980130     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
104200980130     C                   TIME                    W0140            14 0
104300980130     C* UDATE IN GGMMAAAA
104400980130     C                   MOVE      W0140         WDTGIO            8 0
104500060404     C                   MOVEl     W0140         utime
104600980130     C*
104700980130     C* UDATE IN AAAAMMGG
104800980130     C                   Z-ADD     WDTGIO        G02DAT
104900980130     C                   MOVEL     *BLANK        G02ERR
105000980130     C                   CALL      'XSRDA8'
105100980130     C                   PARM                    WLBDAT
105200980130     C                   MOVEL     G02INV        DATEU             8 0
105300980130     C*
105400130702     c                   ENDSR
105500130702     c*
105600130702     C*****************************************************************
105700130702     c     CarTAB        BEGSR
105800130702     C*****************************************************************
105900130327     c*
106000020429     C                   Z-ADD     SIMPOU        CDU               3 0
106100130327     c*
106200130702     c                   clear                   bollema           9 0
106300130711     c                   clear                   olsu3boagg
106400130830     c                   clear                   olsu3bonagg
106500130830     c                   clear                   olsu3msg
106600130830     c                   clear                   olsu3err
106700150703     c                   clear                   olsu3sosp
106800130327     c*
106900111018     c                   Clear                   tibs02ds
107000111018     c                   Clear                   dmradan
107100111018     c                   Movel     'C'           T02Mod
107200111018     c                   Movel     KNSIF         T02Sif
107300111018     c                   Movel     'MRA'         T02Cod
107400130710     c                   Movel     'FNLSU3R'     T02Ke1
107500111018     c                   Call      'TIBS02R'
107600111018     c                   Parm                    Kpjba
107700111018     c                   Parm                    tibs02ds
107800111018    2c                   If        T02Err = *Blanks
107900111018     c                   Movel     T02Uni        Dmradan
108000111018    2c                   endif
108100060403     c*
108200060403     c                   clear                   qcmd
108300060403     c                   eval      qcmd = $cmd2(1)
108400060403     c                   call      'QCMDEXC'                            91
108500060403     c                   parm                    Qcmd
108600060403     c                   parm                    Qlen
108700130327     c*
108800061005     c                   clear                   qcmd
108900061005     c                   eval      qcmd = $cmd3(1)
109000061005     c                   call      'QCMDEXC'                            91
109100061005     c                   parm                    Qcmd
109200061005     c                   parm                    Qlen
109300130702     c
109400130702     c* Se non passata la data spedizione prendo udate
109500130705    1c                   if        ilsu3dsp_B<=*zeros
109600130702     c                   movel     dateu         Wdtcar
109700130702     c                   else
109800130702     c                   movel     ilsu3dsp_B    wdtcar            8 0
109900130705    1c                   endif
110000130715     c                   movel     wdtcar        kaas
110100130715     c                   move      wdtcar        kmgs
110200130702     c
110300130702     c* Calcolo la data sospensione
110400130705    1c                   if        ilsu3dsp_S<=*zeros
110500130702     c                   clear                   trul13ds
110600130705     c                   Eval      I13Tla = 'L'
110700130702     c                   Eval      I13Mod = 'P'
110800130702     c                   Eval      I13Fil = simfel
110900130705     c                   Eval      I13Din = wdtcar
111000130702     c                   Eval      I13gio = 1
111100130702     c                   Call      'TRUL13R'
111200130702     c                   Parm                    Trul13ds
111300130702     c* se da errore non posso spostare in sospensione
111400130705    2c                   if        o13err<>' '
111500130705     c                   eval      olsu3err='E2'
111600130702     c                   else
111700130702     c                   eval      dspsosp=o13dfi
111800130702     c                   movel     o13dfi        aaasosp
111900130702     c                   movel     o13dfi        Wamsos            6 0
112000130705    2c                   endif
112100130705     c
112200130705   x1c                   else
112300130702     c* data passata
112400130702     c                   movel     ilsu3dsp_s    DSPsosp
112500130702     c                   movel     ilsu3dsp_s    AAAsosp
112600130702     c                   movel     ilsu3dsp_s    wamsos
112700130705    1c                   endif
112800130715     c* Carico la £1 alla data spedizione
112900130715     c                   clear                   trul06ds
113000130715     C                   MOVE      '£1'          D06COD
113100130715     C                   MOVEL     SIMFEL        D06KEY
113200130715     c                   move      'S'           d06key
113300130715     c                   movel     wdtcar        d06drf
113400130715     C                   MOVEL     'L'           D06TLA
113500130715     C                   MOVEL     trul06ds      KPJBU
113600130715     C*
113700130715     C                   CALL      'TRUL06R'
113800130715     C                   PARM                    KPJBA
113900130715     C                   MOVEL     KPJBU         trul06ds
114000130715     C                   MOVEA     LIN           L1
114100980130     C*
114200980130     C                   ENDSR
114300081203      *---------------------------------------------------------------*
114400081203
114500081203     c     SR_stampe     BEGSR
114600081203     c*
114700081203     c* GESTIONE STAMPA SPEDIZIONE
114800130705     c                   if        ilsu3nos='S'
114900130705     c                   if        wggl > 0
115000081203     c* calcolo giorni lavorativi fra data spedizione e data immissione
115100081203     c                   clear                   wdat8
115200081203     c                   z-add     blpdim        datada
115300130702     c                   z-add     DSPsosp       dataa
115400081203     c                   call      'XSRLAV8'
115500081203     c                   parm                    wdat8
115600150702     c                   if        ilsu3nsp<=*zeros
115700130705     c                   if        (ggl-1) > wggl
115800081203     c                   if        blpfns = 'S'
115900081203     c                   seton                                        02
116000081203     c                   else
116100081203     c                   setoff                                       02
116200081203     c                   endif
116300081203     c* inverto data ritiro per stampa
116400081203     c     *iso          move      blpdim        data_eur
116500081203     c                   move      data_eur      p1cdim
116600130710     c
116700130710     c     *iso          move      savdsp        data_eur
116800130710     c                   move      data_eur      v11dsp
116900130710     c* Imposto mittente
117000130710     c                   eval      p1crsm=%editc(wksc:'X')+'-'+blprsm
117100081203     c   98              write     testa
117200081203     c                   setoff                                       98
117300081203     c                   write     rigsped
117400081203     c                   endif
117500150702     c                   endif
117600081203     c                   endif
117700081203     C**
117800130705     c** Se passati xx giorni di sospensione, invio msg
117900081203     c                   if        (ggl-1) > §3iggm
118000130705     c
118100130705     c* Memorizzo che ne ho sospensa alemeno una da altro xx giorni
118200130705     c                   eval      Unasospesa='S'
118300130705     c
118400150702     c                   if        ilsu3nsp<=*zeros
118500130705     c                   if        tiposos='L'
118600130705     c                   add       1             BSPlnpBO(indx)
118700130705     c* memorizzo la data più bassa di immissione
118800130705     c                   if        BSPlnpD(indx)=0 or
118900130705     c                             blpdim>BSPlnpD(indx)
119000130705     c                   eval      BSPlnpD(indx)=blpdim
119100130705     c                   endif
119200130705     c
119300130705     c                   else
119400130705     c                   add       1             BSPkscBO(indx)
119500130705     c* memorizzo la data più bassa di immissione
119600130705     c                   if        BSPkscD(indx)=0 or
119700130705     c                             blpdim>BSPkscD(indx)
119800130705     c                   eval      BSPkscD(indx)=blpdim
119900130705     c                   endif
120000130705     c                   endif
120100150702     c                   endif
120200130705     c
120300111017     c
120400081203     c                   endif
120500081203     c                   endif
120600081203     c                   endsr
120700081203      *---------------------------------------------------------------*
120800081203
120900081203     c     SR_nuovonumeroBEGSR
121000130710     c
121100130710     c                   z-add     blpmgs        savdsp
121200130710     c                   movel     blpaas        savdsp
121300130710
121400081203     c                   clear                   fnls65ds
121500130708     c                   z-add     blpaas        I65AAS
121600081203     c                   z-add     blplnp        I65LNP
121700081203     c                   z-add     blpnrs        I65NRS
121800081203     c                   z-add     blpnsp        I65NSP
121900130702     c                   z-add     AAAsosp       I65AASN
122000130702     c                   z-add     DSPsosp       I65MGSN
122100130705     c                   if        ilsu3nos='S'
122200081203     c                   movel     'S'           I65AGDRT
122300130705     c                   if        ilsu3dcr>*zeros  and blpdcr>0
122400130705     c                   movel     'S'           I65AGDCR
122500130705     c                   move      ilsu3tcr      I65TCRN
122600130705     c                   move      ilsu3dcr      I65DCRN
122700130705     c                   move      ilsu3hcr      I65HCRN
122800130705     c                   endif
122900130705     c                   endif
123000130705     c
123100081203     c                   move      '1'           I65STRCMT
123200081203     c                   move      '1'           I65FCMT
123300081203     c                   move      '1'           I65CMTRLBK
123400081203     c                   movel     fnls65ds      kpjbu
123500081210     c                   call      'FNLS65R'
123600081203     c                   parm                    kpjba
123700081203     c                   movel     kpjbu         fnls65ds
123800081203     c                   if        o65err=*blanks
123900081203     c                   add       1             bollema           9 0
124000130705     c                   z-add     AAAsosp       wlpaas
124100081205     c                   z-add     o65nsp        wlpnsp
124200081203     c                   exsr      sr_stampe
124300081204     c                   else
124400081205     c                   add       1             werror            6 0
124500081210     c                   if        o65err='1'
124600081210     c                   movel     o65msg        wmsg
124700081210     c                   endif
124800081203     c                   endif
124900081203     c                   endsr
125000111018     c*-------------------------------------------------------------------
125100111018     c     INV_email     BEGSR
125200111018     c
125300130705     c                   clear                   writerig
125400111018     c                   if        not %open(prtemail)
125500111018     C                   EXSR      SR_OPENPRTF
125600111018     C                   endif
125700111018     c
125800111018     c                   exsr      Routend
125900111018     c
126000111018     c                   ENDSR
126100111018
126200111018      /free
126300111018       //--------------------------------------------------------------
126400111018       //?Override al file di stampa per impostarvi i dati per
126500111018       //?  l'invio via e-mail   +   stampa inizio e-mail
126600111018       //--------------------------------------------------------------
126700111018       BEGSR sr_OpenPrtF;
126800111018
126900111018         // Override al file di stampa per impostarvi i dati per
127000111018         // l'invio via e-mail
127100111018           exsr sr_Override;
127200111018
127300130705
127400111019           zz = 1 ;
127500111019
127600130705          dow     BSPlnp(zz)>0  ;
127700130705
127800130705          if      BSPlnpBO(ZZ)>0 ;
127900130705          dataiso=%date(BSPlnpD(zz)) ;
128000111019          data_eur=dataiso  ;
128100111019
128200111019           clear O_testo;
128300130705           chain  BSPlnp(ZZ)   azorg01l  ;
128400130705           if not %found(azorg01l)  ;
128500130705           eval orgdes=*all'?'  ;
128600130705           endif  ;
128700130705           %subst(O_testo:6:20) = %editc(BSPlnp(zz):'X')+'-'+orgdes  ;
128800111019           %subst(O_testo:46:10) =%char(data_eur)  ;
128900130708           %subst(O_testo:64:9 ) =%editc(BSPlnpBO(zz):'2')  ;
129000111019           except PrtDet;
129100130705           writerig=writerig+1  ;
129200130705           endif  ;
129300130705
129400130705           // se ne ho scritte 50 di righe devo inviare
129500130705           if writerig>=50  ;
129600130705             exsr      Routend  ;
129700130705             clear writerig                  ;
129800130705
129900130705             exsr sr_Override;
130000130705
130100130705           endif  ;
130200111019
130300111019           zz=zz+1  ;
130400111019           enddo    ;
130500130705
130600130705          // Stesso ciclo sui cod clienti
130700130705          zz = 1      ;
130800151209          dow     BSPkscC(zz)>0  ;
130900130705
131000130705          if      BSPkscBO(ZZ)>0 ;
131100130705          dataiso=%date(BSPkscD(zz)) ;
131200130705          data_eur=dataiso  ;
131300130705
131400130705           clear O_testo;
131500151209           chain  (1:kci:BSPkscC(ZZ)) cnaco00f  ;
131600130705           if not %found(cnaco00f)  ;
131700130705           eval acorag=*all'?'  ;
131800130705           endif  ;
131900151209           %subst(O_testo:6:20) = %editc(BSPkscC(zz):'X')+'-'+acorag  ;
132000130705           %subst(O_testo:46:10) =%char(data_eur)  ;
132100130708           %subst(O_testo:64:9 ) =%editc(BSPkscBO(zz):'2')  ;
132200130705           except PrtDet;
132300130705           writerig=writerig+1  ;
132400130705           endif  ;
132500130705
132600130705           // se ne ho scritte 50 di righe devo inviare
132700130705           if writerig>=50  ;
132800130705             exsr      Routend  ;
132900130705             clear writerig                  ;
133000130705
133100130705             exsr sr_Override;
133200130705
133300130705           endif  ;
133400130705
133500130705           zz=zz+1  ;
133600130705           enddo    ;
133700111018
133800130705             exsr      Routend  ;
133900111018       ENDSR;
134000111018
134100111018       //--------------------------------------------------------------
134200111018       //?Override al file di stampa per impostarvi i dati per
134300111018       //?  l'invio via e-mail   +   stampa inizio e-mail
134400111018       //--------------------------------------------------------------
134500111018       BEGSR sr_Override;
134600111018
134700111018         reset TRTCM1ds;
134800111018
134900111018         IF  §MRAdreg <> *blank;
135000111018           §CM1mitt = %trim(§MRAdmitt);
135100111018           §CM1dst  = %trim(§MRAddest);
135200111019
135300111019         // Valorizzo i campi della e-m@ail
135400111019          chain  simpou   azorg01l ;
135500111019
135600111019          if   %found(azorg01l) ;
135700140205           og140=orgde0  ;
135800140205           if §ogapo>*zeros  ;
135900140205           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
136000140205           else  ;
136100120802           §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
136200111019          endif  ;
136300140205          endif  ;
136400111019
136500111018           §CM1tips = §MRAdreg;
136600111018           §CM1po   = %editc(simfel:'X');
136700111019           §CM1var  = '*OBJM*' + 'Manutenzione DATA SPEDIZIONE bolle'   ;
136800111018           §CM1idp  = §MRAdidpro;
136900111018           Qcmd = C_CmdOvrPrtF
137000111018                + ' outq(' + %trim(§MRAdoutqi) + ')'
137100111018                + ' usrdfndta(''' + TRTCM1ds + ''')';
137200111018         ELSE;
137300111018           Qcmd = C_CmdOvrPrtF;
137400111018         ENDIF;
137500111018
137600111018         callp(e) qCmdExc(Qcmd : %size(Qcmd));
137700111018
137800130708           open PrtEMAIL;
137900130705
138000130705         // Stampa una testata se NON è richiesta la e-mail
138100130705           IF  §MRAdreg =  *blank;
138200130705             O_testo = JobUser + ' - ' + SDSpgm
138300130705                     + ' - ' + %editc( *date : 'Y' )
138400130705                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
138500130705             except PrtDet;
138600130705             clear O_testo;
138700130705             except PrtDet;
138800130705             except PrtDet;
138900130705           ENDIF;
139000130705
139100130705         // Stampa testo iniziale
139200130705           O_testo = 'E'' stata manutenzionata la DATA SPEDIZIONE +
139300130705                      di bolle immesse da almeno xx giorni.' ;
139400130705           %subst(O_testo:71:2) = (%editc(§3iggm: '4'))     ;
139500130705           except PrtDet;
139600130705           O_testo = 'Verificare col mittente o procedere alla lor+
139700130705                      o chiusura come "Merce MAI Affidata".' ;
139800130705           except PrtDet;
139900130705
140000130705         // Stampa una riga vuota
140100130705           clear O_testo;
140200130705           except PrtDet;
140300130705
140400130705         // Stampa intestazione
140500130705           O_testo = ' Bolle spostate in sospensione di          Data imm+
140600130705                     issione    Bolle da vericare elencate';
140700130705           except PrtDet;
140800130705           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
140900130705                     chia       in apposita stampa.Utente:';
141000130705           %subst(o_testo:90:10)= knmus     ;
141100130705           except PrtDet;
141200130705
141300130705         // Stampa una riga vuota
141400130705           clear O_testo;
141500130705           except PrtDet;
141600111018       ENDSR;
141700111018
141800111018       //--------------------------------------------------------------
141900111018       //?Operazioni finali.
142000111018       //--------------------------------------------------------------
142100111018       BEGSR RoutEnd;
142200111018
142300111018         if %open(PrtEMAIL);
142400111018
142500111018         //?Chiusura dello spool?
142600111018           clear O_testo;
142700111018           except PrtDet;
142800111018           O_testo = '***   Fine Lista   ***';
142900111018           except PrtDet;
143000111018
143100111018           close PrtEMAIL;
143200111018
143300111018         //?Eliminazione overflow?
143400111018           Qcmd = C_CmdDltOvr;
143500111018           callp(e) qCmdExc(Qcmd : %size(Qcmd));
143600111018
143700111018         endif;
143800111018
143900111018       ENDSR;
144000111018
144100111018      /end-free
144200111018     o* ---------------------------------------------------------------
144300111018     o* ?Spool di stampa (per e-mail).
144400111018     o* ---------------------------------------------------------------
144500111018     oPrtEMAIL  e            PRTdet      1
144600111018     o                       O_testo
144700130710     o* ---------------------------------------------------------------
144800130710     Oqsysprt   E            stabolla    1
144900130710     o                                              'Bolla:'
145000130710     O                       BLPaas        Z   +  1
145100130710     O                       BLPlnp            +  1
145200130710     O                       BLPNRS        Z   +  1
145300130710     O                       BLPNSP        Z   +  1
145400130710     O                       BLPmgs            +  1 '  /  '
145500130710     o                                         +  5 'Cliente'
145600130710     O                       wksc              +  1
145700130710     O                       BLPrsm            +  1
145800130710     Oqsysprt   E            stasgn      1
145900130710     o                                              'Chi sono'
146000130710     O                       SGNWHO            +  1
146100130710     O                       sgnksc            +  1
146200130710     O                       sgnNRS        Z   +  1
146300130710     O                       sgnNSP        Z   +  1
146400130710     O                       sgnmgs            +  1 '  /  '
146500130710     O                       sgntna            +  1
146600111018
146700030619**  $Cmd
146800030707OVRPRTF FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
146900030619           USRDTA('Manut.Boll') SHARE(*YES)
147000030619           COPIES(2)
147100060403**  $Cmd2
147200130710OVRPRTF FILE(FNLSU3P) SAVE(*YES)
147300061005**  $Cmd3
147400061005OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES) OUTQ(LJ4200CED) USRDTA('EDPES')
147500061012**  DLTOVR
147600061012DLTOVR  FILE(QSYSPRT)
