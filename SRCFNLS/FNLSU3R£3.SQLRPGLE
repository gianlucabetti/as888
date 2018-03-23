000100130313     /*PRM dbgview(*source)
000200130313     /*END
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400130610     H* FNLSU3R *----------------------------------------------------*
000500980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000600130610     h*           passo un numero di spedizione per controllo/aggiornamento
000700111122     H***------------------------------------------------------------*
000800130710     FFNLSU3P   o    E             printer usropn  oflind(*in98)
000900910207     FAZORG01L  IF   E           K DISK
001000051212     Ftntbe02l  IF   E           K DISK
001100070117     FfNbrv07l  IF   E           K DISK
001200980130     FTABEL00F  IF   E           K DISK
001300910205     FCNACO00F  IF   E           K DISK
001400130715     FFNBLP70L  IF   E           K DISK    infds(fnblpinf0)
001500130715     f                                     rename(fnblp000:fnblp007)
001600081203     FFNBLP01L  UF   E           K DISK    rename(fnblp000:fnblp001)
001700110705     f                                     infds(blpnrr01)
001800130705     FFNBLT01L  iF   E           K DISK
001900010326     FFISGN05L  UF   E           K DISK
002000050318     FfIAR601L  UF   E           K DISK
002100130710     FfIAR401L  iF   E           K DISK
002200050318     FfIAR701L  UF   E           K DISK
002300111018     FQSYSPRT   O    F  132        PRINTER OFLIND(*InOA)  usropn
002400111018
002500111018     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
002600020610
002700020610     d sklnp           s              3  0 dim(30)
002800020610
002900130715     D L1              S              3  0 DIM(30)
003000130715
003100030619     d $Cmd            s             80    dim(3) ctdata perrcd(1)
003200060403     d $Cmd2           s             80    dim(1) ctdata perrcd(1)
003300061005     d $Cmd3           s             80    dim(1) ctdata perrcd(1)
003400061012     d DLTOVR          s             80    dim(1) ctdata perrcd(1)
003500980202     D*
003600130705     D BSPlnp          S              3  0 DIM(599)
003700130705     D BSPlnpBO        S              7  0 DIM(599)
003800130705     D BSPlnpD         S              8  0 DIM(599)
003900130705     d
004000130705     D BSPksc          S              7  0 DIM(799)
004100130705     D BSPkscBO        S              7  0 DIM(799)
004200130705     D BSPkscD         S              8  0 DIM(599)
004300130717
004400130717     D WrkStringaSql   S           4500
004500130717     D                                     VARYING
004600130717     d $Finerec        s              1    inz(*off)
004700130717     d rrnblp          s             15  0
004800020610
004900130717     d blp_mm          s              2  0
005000020610     d lnp             s              3  0
005100020610     d xx              s              2  0
005200130705     d YY              s              4  0 inz
005300130705     d zz              s              4  0 inz
005400130705     d x               s              4  0 inz
005500130705     d y               s              4  0 inz
005600130705     D Indx            S              3  0 inz
005700130705     D Writerig        S              4  0 inz
005800030619      *
005900030619     d Qlen            s             15  5 inz(150)
006000030619      *
006100030619     d Wfile           s             10    inz
006200030619     d Woutq           s             10    inz
006300030619     d Wform           s             10    inz
006400060405     d data_eur        s               D   datfmt(*eur)
006500070612     d Dataiso         s               d   datfmt(*iso)
006600130702     d
006700130705     D DSPsosp         S              8  0
006800130705     D AAAsosp         S              4  0
006900021203      *
007000111018
007100111018      // - Status
007200111018     d Psds           sds
007300111018     d   SDSpgm          *proc
007400111018     d   JobUser             254    263
007500111018
007600111018      // - Campi per QCMDEXC
007700111018     d Qcmd            s            512    inz
007800111018     d
007900111018      // - Esecuzione comando di sistema
008000111018     d qCmdExc         pr                  extpgm('QCMDEXC')
008100111018     d  Qcmd                        512    const  options(*varsize)
008200111018     d  Qlen                         15  5 const
008300111018
008400130715     D trul06ds      E DS
008500130715     D  LIN                    1     90  0
008600130715     D                                     DIM(30)
008700051209     D* DS PER TIBS02R - GESTIONE TNTBE00F
008800051209     D TIBS02DS      E DS
008900111018      // - Tabella "MRA" = Bart-Mailing - Danni
009000111018     d dMRAdan       e ds                  inz
009100111018
009200070903     D og143         E DS
009300081211     d
009400081211     dwidmsall         ds
009500081211     Dwidms1                         32
009600081211     Dwidms2                         32
009700081211     Dwidms3                         32
009800081211     Dwidms4                         32
009900081211     D***
010000130717     d fnblpds       e ds                  extname(fnblp00f) inz
010100941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010200980202     D WGIDAT          DS                  INZ
010300980202     D  GIODAT                 1      8  0
010400980202     D  GIOINV                 9     16  0
010500980202     D  GIOTGI                17     21  0
010600941227     D***
010700941227     D WLBDAT          DS                  INZ
010800941227     D  G02DAT                 1      8  0
010900941227     D  G02INV                 9     16  0
011000941227     D  G02ERR                17     17
011100941227     D  G02TGI                18     22  0
011200980202     D                 DS
011300980202     D  WAA                    1      4  0
011400980202     D  WMM                    5      6  0
011500980202     D  WGG                    7      8  0
011600980202     D  WDATA                  1      8  0
011700060403
011800060403     d WDAT8           DS
011900060403     d  datada                 1      8  0
012000060403     d  dataa                  9     16  0
012100060403     d  ggl                   17     21  0
012200130717     D***              DS
012300130717     D**blpaas                 1      4  0
012400130717     D**blpmgs                 5      8  0
012500130717     D**blpaamm                1      6  0
012600130717     D**blpdsp                 1      8  0
012700060403
012800941116     D KPJBA         E DS
012900910205     D TCUDS           DS
013000910205     D  F1                     1      1
013100910205     D  F3                     3      3
013200910205     D  F2                     2      2
013300910205     D  F4                     4      4
013400910205     D  F56                    5      6
013500941227     D CNCR80        E DS
013600941227     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
013700000000     D  TCU                  398    697
013800941116     D                                     DIM(50)
013900000000     D  KCU                  698    847P 0
014000000000     D                                     DIM(50)
014100000000     D                                     PACKEVEN
014200081203      * Ds per pgm che mette in sospensione rinumerando la spedizione
014300081203     d FNLS65DS      e ds                  inz
014400030619      *
014500980129     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
014600980129     D DSLV55        E DS                  EXTNAME(FNLV55DS)
014700130717     D*fnblpinf        ds
014800130717     D* fnblpnrri            397    400i 0
014900130715     D fnblpinf0       ds
015000130715     D  fnblpnrr0            397    400i 0
015100110705     D blpnrr01        ds
015200110705     D  fnblpnrr01           397    400i 0
015300081203      * DS Esterna x gestione allocazione ed invio messaggi
015400081203     D TRUL82DS      E DS
015500130702     d
015600130702     D trul13ds      E DS
015700130702     d
015800980130     D DS3A          E DS
015900090427     D DS1B          E DS
016000081203     D Ds3c          E DS
016100980202     D DS3I          E DS
016200140703     D DBSP          E DS
016300140205     D og140         E DS
016400130702
016500130702     D fnlsu3ds        DS
016600130702     D  ilsu3flg               1      1
016700130702     D  ilsu3aas               2      5
016800130702     D  ilsu3lnp               6      8
016900130702     D  ilsu3nrs               9     10
017000130702     D  ilsu3nsp              11     17
017100130702     D  ilsu3DSP_B            18     25
017200130702     D  ilsu3DSP_S            26     33
017300130705     D  ilsu3nos              34     34
017400130705     d
017500130705     d  ilsu3tcr              35     35
017600130705     d  ilsu3dcr              36     43
017700130705     d  ilsu3hcr              44     47
017800130705     D  olsu3err             100    101
017900130711     D  olsu3boagg           102    110
018000130830     D  olsu3bonagg          111    116
018100130830     D  olsu3msg             117    194
018200130702     d
018300130710     D  Ilsu3NOAGG           201    201
018400021203
018500130702     D kcod1           S                   LIKE(tbecod)
018600051212     D klin            S                   LIKE(tbelin) inz(' ')
018700051212     D ksif            S                   LIKE(tbesif)
018800110704     D ktrc            S                   LIKE(ar4trc)
018900110704     D ktrcM           S                   LIKE(ar4trc) inz('M')
019000081205     D wlpnsp          S                   LIKE(blpnsp)
019100081205     D wlpaas          S                   LIKE(blpaas)
019200130715     D Klnp            S                   LIKE(blplnp)
019300130715     D Kaas            S                   LIKE(blpaas)
019400130715     D Kmgs            S                   LIKE(blpmgs)
019500130705     D wmsg            S             80    inz
019600090126     D ntwlnp          S              1    inz
019700111214     D savtfp          S              3  0 inz
019800080620     D sosdaL1         S              1    inz
019900111018     d O_testo         s            132    inz
020000130705     d Tiposos         s              1    inz
020100130705     d Unasospesa      s              1    inz
020200130710     d savdsp          s              8  0 inz
020300130715     d wtiBLP          s              1    inz
020400130705     d
020500060713     D* DEFINIZIONE COSTANTI
020600130710     D Ctitolo         C                   CONST('Elenco spedizioni in SOSPENSI-
020700130710     D                                     ONE AUTOMATICA da oltre X GIORNI')
020800081203     D msgjob          C                   CONST('Si sta bloccando la manutenzi-
020900081203     D                                     one data spedizione:USCIRE un moment-
021000081203     D                                     o da questo lavoro')
021100130111     D*ms2wdw          C                   CONST('Probabilmente in manutenzione-
021200130111     D*                                       ')
021300130111     D*ms3wdw          C                   CONST('da altro utente.             -
021400130111     D*                                       ')
021500130111     D*ms4wdw          C                   CONST('Rieseguire il lancio.        -
021600130111     D*                                       ')
021700130111     D ms2wdw          C                   CONST('                             -
021800130111     D                                        ')
021900130111     D ms3wdw          C                   CONST('Segnalare al CED di verificar-
022000130111     D                                     e  ')
022100130111     D ms4wdw          C                   CONST('il JOBLOG                    -
022200130111     D                                        ')
022300111018
022400111018      // - Comando di override al PrtF
022500111018     d C_CmdOvrPrtF    c                   const('OVRPRTF +
022600111018     d                                           file(PRTEMAIL) +
022700111018     d                                           pagesize(66 132) +
022800111018     d                                           lpi(6) cpi(10) +
022900111018     d                                           ovrscope(*actgrpdfn) +
023000111018     d                                           ')
023100111018     d C_CmdDltOvr     c                   const('DLTOVR +
023200111018     d                                            file(PRTEMAIL) +
023300111018     d                                            lvl(*actgrpdfn)')
023400111018      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
023500111018     d TRTCM1ds      e ds                  inz
023600111018      //   ·§CM1mitt = Indirizzo e-mail del mittente
023700120820     d   §CM1mitt    e                     inz(' ed@brt.it')
023800111018      //   ·§CM1dst  = Indirizzo e-mail del destinatario
023900120302     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
024000111018      //   ·§CM1tips = Tipo lettera via e-mail:
024100111018      //    "LET" = testo allegato in corpo con logo
024200111018      //            (richiede righe libere iniziali per il logo)
024300120820      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
024400111018      //            (non consente né UNDERLINE né HIGHLIGHT)
024500120820     d   §CM1tips    e                     inz('COI')
024600111018      //   ·§CM1po   = Filiale
024700111018     d   §CM1po      e                     inz('046')
024800111018      //   ·§CM1var  = Oggetto e-mail
024900111018     d   §CM1var     e                     inz('*OBJM*+
025000111019     d                                     Manutenzione data spedizione+
025100111019     d                                      bolle ')
025200111018      //   ·§CM1sts  = Stato
025300111018     d   §CM1sts     e                     inz(*off)
025400111018      //   ·§CM1sts  = Id processo
025500111018     d   §CM1idp     e                     inz('2')
025600111018
025700111018
025800910412     C****************************************************************
025900910412     C*  RIEPILOGO INDICATORI
026000910412     C***************************************************************
026100070612     C* 01    - possibilità di richidere bolle in sospensione x colli non
026200070612     c*         spuntati
026300070612     C* 02    -
026400070612     C* 03    - per colli non spuntati possibilità di modificare la consegna
026500070612     c*         richiesta da tabella CLI
026600070612     C* 12    - PROTEZIONE CAMPO LINEA DI PARTENZA XCHE' SONO UN 2° LIVELLLO
026700910412     C* 30    - DI COMODO
026800980129     C* 40-50 - ERRORE
026900980130     C* 51-55 - GESTIONE CHIAVE LETTURA BOLLE
027000070612     C* 70    - posizionam.cursone su DCR
027100070612     C* 90    - ERRORE GENERICO
027200910412     C*****************************************************************
027300941116     C**
027400980130     C     *ENTRY        PLIST
027500980130     C                   PARM                    KPJBA
027600130702     c                   movel     kpjbu         fnlsu3ds
027700130717      /free
027800130717         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
027900130717      /end-free
028000130702     c
028100130702     c                   exsr      CartaB
028200130705     c
028300130705     c                   if        olsu3err=*blanks
028400130702     c
028500130702     C* SE PASSATO UN NUMERO DI SPEDIZIONE --> metto in sospensione la bolla
028600130702     c*                         altrimenti elaboro da tabella BSP
028700130702     c
028800130702     c                   if        ilsu3nsp>*zeros
028900130702     c                   if        ilsu3aas<=*zeros
029000130702     c                   eval      olsu3err='E1'
029100130702     c                   endif
029200130702     c                   if        ilsu3lnp<=*zeros
029300130702     c                   eval      olsu3err='E1'
029400130702     c                   endif
029500130702     c                   if        ilsu3nrs<=*zeros
029600130702     c                   eval      olsu3err='E1'
029700130702     c                   endif
029800130702     c
029900130705     c                   movel     ilsu3nsp      blpnsp
030000130705     c                   movel     ilsu3aas      blpaas
030100130705     c                   movel     ilsu3lnp      blplnp
030200130705     c                   movel     ilsu3nrs      blpnrs
030300130702     c
030400130702     c                   else
030500130702     c                   exsr      LeggiBLP
030600130830     c* IMPOSTAZIONE EVENTUALE CODICE DI ERRORE
030700130830     c* Errori di ritorno da pgm di attribuzione nuovo numero sped. per
030800130830     c* sospensione a cavallo di anno (FNLS65R)
030900130830     c                   if        werror>0
031000130830     c                   move      'E3'          olsu3err
031100130830     c                   move      werror        olsu3bonagg
031200130830     c                   move      wmsg          olsu3msg
031300130830     c                   endif
031400130702     c                   endif
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
032800061012     c
032900061012     c                   clear                   qcmd
033000061012     c                   eval      qcmd = dltovr(1)
033100061012     c                   call      'QCMDEXC'                            91
033200061012     c                   parm                    Qcmd
033300061012     c                   parm                    Qlen
033400130705     C**
033500130705     c                   endif
033600111018     c
033700130711     c*
033800130711     c                   if        bollema>0
033900130711     c                   movel     bollema       olsu3boagg
034000130830     c***                movel     fnlsu3ds      kpjbu
034100130711     c                   endif
034200130830
034300130830     c                   movel     fnlsu3ds      kpjbu
034400130830
034500130830     c* sottometto azione a titolo di documentazione di sospensione
034600130830     c* lanciata
034700130830     C                   MOVEL     'LS0U'        KCOAZ
034800130830     C                   CALL      'BCH10'
034900130830     C                   PARM                    KPJBA
035000130711     c
035100000000     C                   SETON                                        LR
035200941116     C*
035300980130     C*****************************************************************
035400980130     C* ELABORAZIONE TESTATA E DETTAGLIO BOLLE ----------------------
035500980130     C*****************************************************************
035600130702     C     LeggiBLP      BEGSR
035700980130     C*
035800081205     c                   clear                   werror
035900081210     c                   clear                   wmsg
036000130705     c                   clear                   bollema
036100130705     c* Imposto la "S" di sospensione atomatica per spunte
036200130705     c                   eval      ilsu3nos='S'
036300130702     c
036400130702     c* Carico da tabella BSP le linee / ksc che devo elaborare
036500130702     C                   movel     'BSP'         kcod1
036600130702     C                   Z-ADD     1             X
036700130702     C                   Z-ADD     1             y
036800130702     C     kcod1         setll     tntbe02l
036900130702     C     kcod1         reade     tntbe02l
037000130702     c
037100130715    1c                   dow       not %eof(tntbe02l)
037200130715    2c                   if        tbeatb=' '
037300140703     c* elabolo solo se non escluso dalla elaborazione automatica
037400140703     c                   eval      dbsp=tbeuni
037500140703   2ac                   if        §bspaut<>'N'
037600130702     c* carico solo il terminal
037700130715    3c                   if        tbeke1='000            '
037800130715    4c                   if        %subst(tbeke2:1:3)>*zeros
037900130715     c                   movel     tbeke2        w0030
038000130715     c                   eval      Indx=%lookup(w0030:l1)
038100130715    5c                   if        indx>0
038200130715     c                   eval      BSPlnp(y)=%int(%subst(tbeke2:1:3))
038300130715     c                   add       1             y
038400130715    5c                   endif
038500130715    4c                   endif
038600130715     c
038700130715   x3c                   else
038800130715     c
038900130702     c                   movel     tbeke1        w0030             3 0
039000130702     c
039100130715    4c                   if        simfel=w0030
039200130702     c* in TBEKE2 ci può essere lnp o ksc
039300130715     c**                 if        %subst(tbeke2:1:3)>*zeros and
039400130715     c**                           %subst(tbeke2:4:4)=*blanks
039500130715     c**                 eval      BSPlnp(y)=%int(%subst(tbeke2:1:3))
039600130715     c**                 add       1             y
039700130715     c**                 else
039800130715    5c                   if        %subst(tbeke2:1:7)>*zeros
039900130705     c                   eval      BSPksc(x)=%int(%subst(tbeke2:1:7))
040000130702     c                   add       1             x
040100130715    5c                   endif
040200130715     c**                 endif
040300130702     c
040400130715    4c                   endif
040500130715    3c                   endif
040600140703   2ac                   endif
040700140703    2c                   endif
040800130702     c
040900130705     C     kcod1         reade     tntbe02l
041000130715    2c                   enddo
041100130702     c
041200130702     c* Se nessuna linea/ksc trovata --> esco dalla elaborazione
041300130705     c                   if        BSPlnp(1)=0 and BSPksc(1)=0
041400130702     c                   leavesr
041500130702     c                   endif
041600130702     C*
041700130710     c                   open      fnlsU3p
041800130702     c
041900130705     c                   move      wGGL          w_gg              1
042000060404     c                   eval      ptitolo = ctitolo
042100060404     c                   eval      ptitolo = %xlate('X':w_gg:ptitolo)
042200090127     c                   eval      *in13='0'
042300130708     c* Imposto nella testa della stampa il terminal
042400130708     c                   eval      *in14='1'
042500130708     c                   eval      p1clnp=simfel
042600130708     c     simfel        chain     azorg01l
042700130708     c                   if        %found(azorg01l)
042800130708     c                   movel     orgdes        p1dlnp
042900130708     c                   endif
043000130708     c
043100060403     c                   write     testa
043200061005     c
043300061005     c                   open      qsysprt
043400130715     c
043500130715     c* Elaboro prima le bolle per linea di partenza
043600130715     c*  da mettere ibn sospensione
043700130715     c                   z-add     1             x
043800130715     c                   eval      tiposos='L'
043900130715     c
044000130715     c                   dow       bsplnp(X)>0
044100130715     c                   eval      klnp=BSPlnp(X)
044200130715     c                   eval      indx=X
044300130715     c
044400130715     c     kblp70        setll     fnblp70l
044500130715     c     kblp70        reade     fnblp70l
044600130715     c
044700130715     c                   dow       not %eof(fnblp70l)
044800130715     c                   exsr      Elabora
044900130715
045000130715     c     kblp70        reade     fnblp70l
045100130715     c                   enddo
045200130715     c
045300130715     c                   seton                                        98
045400130715     c                   add       1             x
045500130715     c                   enddo
045600130702     c
045700130702     c* Elaboro tutte le bolle ancora da borderizzare
045800130715     c* Per cod cliente
045900130702     c*
046000130715     c                   eval      tiposos='K'
046100130717     c                   eval      indx=1
046200130717     c                   dow       bspksc(indx)>0 and indx<=799
046300130717      /free
046400130717       WrkStringaSql='select rrn(fnblp00f) as RRNBLP, fnblp00f.* +
046500130717        from fnblp00f +
046600140114        where (blpccm=' + %editc(bspksc(indx):'X') + ' or blpksc=' +
046700140114        %editc(bspksc(indx):'X') +  ') and blptfp=' + %editc(simfel:'X') +
046800130717        ' and blpnfv=0';
046900130717
047000130717        $finerec=*off;
047100130717        exec sql prepare STRINGASQL from :WrkStringaSql;
047200130717        exec sql declare BLPCsr cursor for StringaSql;
047300130717        exec sql open BLPCSR;
047400130717        dow $finerec=*off;
047500130717           exec sql Fetch BLPcsr into :rrnblp, :fnblpds;
047600130717           if sqlcod=100 or sqlcod<0;
047700130717              $finerec = *on;
047800130717              leave;
047900130717           endif;
048000130717           exsr elabora;
048100130717        enddo;
048200130717        exec sql close BLPcsr;
048300130717      /end-free
048400130717     c                   add       1             indx
048500130717     c                   enddo
048600130717     c**                 clear                   knfv              5 0
048700130717     c**   kblp77        setll     fnblp77l
048800130717     c**   kblp77        reade     fnblp77l
048900130717     c**
049000130717     c**                 dow       not %eof(fnblp77l)
049100130717     c**                 exsr      Elabora
049200980202     C*
049300130717     c**   kblp77        reade     fnblp77l
049400130717   1ac**                 enddo
049500060403     c*
049600060404     c   98              write     testa
049700060404     c                   setoff                                       98
049800060404     c                   write     fines
049900130710     c                   close     fnlsU3p
050000061005     c
050100061005     c                   close     qsysprt
050200060713     c
050300130705     c                   if        Unasospesa='S'
050400111018     c                   exsr      Inv_email
050500130705     c                   endif
050600111018     c
050700060713     c* emetto msg di quante bolle manutenzionate
050800060713     c                   seton                                        28
050900980130     C*
051000980202     C                   ENDSR
051100110704     C*----------------------------------------------------------------
051200130715     c     Elabora       BEGSR
051300130715     c
051400130715     c                   exsr      Parzial
051500130715     c
051600130715     c                   if        NoBolla=' '
051700130715     c
051800130715     c* Se cambia anche l'anno richiamo pgm apposito che esegue anche la
051900130715     c* rinumerazione della spedizione
052000130715    2c                   if        aaasosp>blpaas
052100130715     c                   exsr      sr_nuovonumero
052200130715     c                   else
052300130715     c
052400130715     c                   exsr      Aggiorna
052500130715    2c                   endif
052600130715     c                   endif
052700130715     c                   ENDSR
052800130715     C*----------------------------------------------------------------
052900110704     c     Parzial       BEGSR
053000110704     c                   clear                   Nobolla           1
053100130702     c
053200130702     c* Escludo le bolle con data spedizione > data passata
053300130717    1c****               if        blpdsp>wdtcar
053400130717     c                   if        (blpaas*10000)+blpmgs>wdtcar
053500130702     c                   eval      NoBolla='N'
053600130702     C                   leavesr
053700130702    1c                   endif
053800130702      *
053900130702      ** CONTROLLO se bolla trasmessa
054000130702    2C     BLPDT1        IFNE      *ZEROS
054100130702     c                   eval      NoBolla='N'
054200130702     C                   leavesr
054300130702    2C                   ENDIF
054400130702     c*
054500130702      * SCARTO I PREPAGATI ANNULLATI
054600130702     c                   if        blpcbo='M '
054700130702     c                   eval      NoBolla='N'
054800130702     c                   leavesr
054900130702     c                   endif
055000130702     c
055100130702      *  CONTROLLO IL CODICE CLIENTE
055200130702     C     BLPCCM        IFNE      *ZEROS
055300130702     C                   Z-ADD     BLPCCM        WKSC              7 0
055400130702     C                   ELSE
055500130702     C                   Z-ADD     BLPKSC        WKSC
055600130702     C                   ENDIF
055700130715     c**
055800130702     c* controllo se linea tutta da mettere in sospensione
055900130715     c**                 eval      tiposos='L'
056000130715     c**                 eval      indx=%lookup(blplnp:BSPlnp)
056100130715     c**
056200130715    1c**                 if        Indx=0
056300130715     c**
056400130715     c**                 eval      Indx=%lookup(wksc:BSPksc)
056500130715    2c**                 if        Indx=0
056600130715     c**                 eval      NoBolla='N'
056700130715     C**                 leavesr
056800130715     c**                 else
056900130715     c**                 eval      tiposos='K'
057000130715    2C**                 endif
057100130715    1C**                 endif
057200130715     c
057300130717    1c***                if        tiposos='K'
057400130717     c***                eval      Indx=1
057500130717     c***                eval      Indx=%lookup(wksc:BSPksc)
057600130717    2c***                if        Indx=0
057700130717     c***                eval      NoBolla='N'
057800130717     C***                leavesr
057900130717    2C***                endif
058000130717    1C***                endif
058100130702     c
058200130702     c                   clear                   ntwlnp
058300130702     c     blplnp        chain     azorg01l
058400130702     c                   if        %found(azorg01l)
058500130702     c                   movel     orgde3        og143
058600130702     c                   if        §ogntw='DPD' or §ogntw='EEX' or
058700130702     c                             §ogntw='FED'
058800130702     c                   eval      ntwlnp='E'
058900130702     c                   endif
059000130702    1C                   ENDIF
059100130702     c
059200140114     c* Se richiesta sospensione a cavallo di anno
059300140114     c* sia per linea estera che italia
059400110704     c* se il cliente mantiene il numero spedizione la scarto
059500140114     c*****              if        ntwlnp='E' and AAAsosp > blpaas
059600140114     c                   if        AAAsosp > blpaas
059700110704     C                   MOVEL(P)  '3C'          COD
059800110704     C                   MOVEL(p)  wksc          KEY
059900110704     C     ktab2         chain     tabel00f                           31
060000110704     C   31              clear                   ds3c
060100110704     C  N31              movel     tbluni        ds3c
060200110704     c     §3cfsp        ifeq      'S'
060300110704     c                   eval      NoBolla='N'
060400110704     C                   leavesr
060500110704     c                   endif
060600110704     c                   endif
060700110704      *
060800110704     c                   clear                   wsispu            1
060900110704     c     kblt          setll     fnblt01l
061000110704     c     kblt          reade     fnblt01l                               99
061100110704    2c                   dow       not *in99
061200110704     c
061300110704     c* Cerco una qualsiasi spunta
061400110704     c     kbrv          chain     fnbrv07l
061500110704    3c                   if        %found(fnbrv07l)
061600110704     c                   eval      wsispu='S'
061700110704     c                   seton                                        99
061800110704   x3c                   else
061900110704     c     kblt          reade     fnblt01l                               99
062000110704    3c                   endif
062100110704    2c                   enddo
062200110704     c
062300130702     c*Se trovata  spunta   --> non posso spostare in sospensione
062400110704    2c                   if        wsispu='S'
062500110704     c                   eval      NoBolla='N'
062600110704     c                   leavesr
062700110704    2c                   endif
062800110704      *
062900110704      ** CONTROLLO IL TIPO BOLLA
063000110704     C                   MOVEL     '3A'          COD
063100110704     C                   MOVEL(P)  BLPCBO        KEY
063200110704     C     KTAB2         CHAIN     TABEL00F                           31
063300110704     C   31              CLEAR                   DS3A
063400110704     C  N31              MOVEL     TBLUNI        DS3A
063500110704      *
063600110704      * SCARTO LE BOLLE DI RECUPERO
0637001107041   2C     §3ARBL        IFEQ      'R'
063800110704     c                   eval      NoBolla='N'
063900110704     C                   leavesr
064000110704    2C                   END
064100130702     c
064200110704      * SCARTO LE BOLLE con cod trattamento merce che non prevede merce
064300110704     C                   MOVEL     '1B'          COD
064400110704     C                   MOVEL(P)  BLPctm        KEY
064500110704     C     KTAB2         CHAIN     TABEL00F                           31
064600110704     C   31              CLEAR                   DS1b
064700110704     C  N31              MOVEL     TBLUNI        DS1b
064800110704    2C                   IF        §1bfg8='N'
064900110704     c                   eval      NoBolla='N'
065000110704     C                   leavesr
065100110704    2C                   END
065200110704      *
065300110704      * PER I PREPAGATI NON È CONSENTITO SPOSTARE LA DATA BOLLA AL MESE
065400110704      *  SUCCESSIVO
065500130717     C***  WAMSOS        IFGT      blpaamm
065600130717     c                   movel     blpmgs        blp_mm
065700130717     c                   if        wamsos>(blpaas*100)+blp_mm
065800110704      *
065900110704    2C     BLPPDR        IFEQ      *ZEROS
066000110704     c                   eval      NoBolla='N'
066100110704     C                   leavesr
066200110704    2C                   ELSE
066300110704     C                   MOVEL     '1'           KTRC
066400110704     C     KAR6          CHAIN(N)  FIAR6000                           31
066500110704    2C     *IN31         IFEQ      '0'
066600110704    2C     AR6NFT        ANDGT     *ZEROS
066700110704     c                   eval      NoBolla='N'
066800110704     C                   leavesr
066900110704    2C                   ENDIF
067000110704    2C                   ENDIF
067100110704      *
067200110704    2C                   ENDIF
067300110704     c
067400110704     c                   ENDSR
067500110704     c*------------------------------------------------------------------------
067600110704     c     Aggiorna      BEGSR
067700110704     c* A questo punto visto che sono stati superati tutti i controlli
067800110704     c* chaino il record bolla in update
067900110704     c     kblt          chain(E)  fnblp01l
068000110704    1c                   if        %error
068100110704     C                   CLEAR                   TRUL82DS
068200130715     c                   if        tiposos='L'
068300130715     c                   eval      ul82§rrn = fnblpnrr0
068400130715     c                   else
068500130717     c****               eval      ul82§rrn = fnblpnrri
068600130717     c                   eval      ul82§rrn = RRNBLP
068700130715     c                   endif
068800110704     C                   EVAL      UL82§FIL = 'FNBLP00F'
068900110704     C                   EVAL      UL82§WIN = 'N'
069000110704     C                   EVAL      UL82§F7  = 'S'
069100110704     C                   EVAL      UL82§num = 1
069200110704     C                   EVAL      UL82§att = 30
069300110704     C                   EVAL      UL82§mss = msgjob
069400110704     C* Effettuo la chiamata al *PGM d utilità
069500110704     C                   CALL(e)   'TRUL82R'
069600110704     C                   PARM                    TRUL82DS
069700110704     C     kblt          chain     fnblp01l
069800110704    1c                   endif
069900130705     c
070000130705    1c                   if        %found(fnblp01l)
070100111214     c
070200110704      *
070300110704      ** CONTROLLO SE DATA BORDERO' MINORE DELLA NUOVA DATA DI SPEDIZIONE
070400110704      *  SBORDERIZZO
070500130705    2C***  BLPDBR        IFLT      dspsosp
070600110704      *
070700130705   2ac                   if        blpdbr=0
070800110704     C                   CLEAR                   BLPNFV
070900110704     C                   CLEAR                   BLPDBR
071000110704     C                   CLEAR                   BLPFLP
071100130705     c
071200130710     c                   z-add     blpmgs        savdsp
071300130710     c                   movel     blpaas        savdsp
071400110704     C* AGGIORNO LA NUOVA DATA SPEDIZIONE
071500130702     C                   Z-ADD     AAAsosp       BLPAAS
071600130702     C                   Z-ADD     dspsosp       BLPMGS
071700110704     c* Aggiorno anche la data ritiro merce
071800110704     c*  solo per i clienti che spostanto in sospensione per mancanza di spu
071900110704     c*  nte
072000130705    2c                   if        ilsu3nos='S'
072100130702     C                   movel     dspsosp       BLPdrt
072200110704     c* Se richiesto e se presente consegna richiesta, imposto la nuova data
072300130705    3c                   if        ilsu3dcr>*zeros and blpdcr>0
072400130705     c                   movel     ilsu3tcr      blptcr
072500130705     c                   movel     ilsu3dcr      blpdcr
072600130705     c                   movel     ilsu3hcr      blphcr
072700110704    3c                   endif
072800110704     c
072900110704   x2c                   else
073000110704     c* Se la data ritiro > della data spedizione nuova e non c'e' ORM
073100110704     c*  imposto SEMPRE la data ritiro= data spedizione
073200130702    3c                   if        blpdrt>dspsosp
073300110704     c     kar4m         chain     fiar401l
073400110704    4c                   if        not %found(fiar401l)
073500130702     c                   eval      blpdrt=DSPsosp
073600110704    4c                   endif
073700110704    3c                   endif
073800110704     c
073900110704    2c                   endif
074000110704     c
074100110704     C* E RICACOLO I TERMINAL DI ARRIVO E FILIALE ELABORAZIONE ARRIVI
074200110704     C                   CLEAR                   DSLV55
074300110704     C                   MOVEL     ' '           D55TPT
074400110704     C                   MOVEL     BLPLNA        D55LIN
074500110704     C                   MOVEL     BLPLNP        D55LNP
074600130702     C                   MOVEL     DSPsosp       D55DRF
074700110704     C                   CALL      'FNLV55R'
074800110704     C                   PARM                    DSLV55
074900110704    2C     D55ERR        IFEQ      ' '
075000110704     C                   MOVEL     D55TFA        BLPTFA
075100110704     C                   MOVEL     D55TFP        BLPFEA
075200110704    2C                   ENDIF
075300110704     C* BLPTFP: TERMINAL DI PARTENZA DELLA LINEA DI PARTENZA
075400110704     C                   CLEAR                   DSLV55
075500110704     C                   MOVEL     'P'           D55TPT
075600110704     C                   MOVEL     BLPLNP        D55LIN
075700130702     C                   MOVEL     DSPsosp       D55DRF
075800110704     C                   CALL      'FNLV55R'
075900110704     C                   PARM                    DSLV55
076000111214     c
076100111214     c
076200110704    2C     D55ERR        IFEQ      ' '
076300110704     C                   MOVEL     D55TFP        BLPTFP
076400111128     C                   MOVEL     D55TFP        BLPfle
076500110704    2C                   ENDIF
076600110704     C*
076700130710     c                   if        ilsu3NOAGG=' '
076800110704     C                   UPDATE    FNBLP001
076900130710     c                   else
077000130710     c                   unlock    fnblp01l
077100130710     C                   except    stabolla
077200130710     c                   endif
077300130710     c
077400110704     c                   add       1             bollema           9 0
077500110704     c
077600110704     c* Se si tratta di assegnato S.F. tassato in partenza, cancello la
077700110704     c* tassazione
077800130710     c                   if        ilsu3NOAGG=' '
077900130710     c
078000130710     c                   clear                   ktrc
078100110704    2c                   if        §3atb2<>*blanks
078200110704     c                   eval      ktrc='2'
078300110704   x2c                   else
078400110704     c                   movel     §3atb1        flgtb1            1
078500110704    3c                   if        flgtb1='A'
078600110704     c                   eval      ktrc='1'
078700110704    3c                   endif
078800110704    2c                   endif
078900110704     c
079000110704   1ac                   if        ktrc<>*blanks
079100110704     C     KAR6          CHAIN(N)  FIAR6000                           31
079200110704    2c                   if        not *in31
079300110704     c                   move      ar6ksc        fineksc           4 0
079400110704    3c                   if        fineksc<>0000 and fineksc<>9999
079500110704     c* deleto la tassazione
079600110704     c     kar6          setll     fiar701l
079700110704     c     kar6          reade     fiar701l
079800110704    4c                   dow       not %eof(fiar701l)
079900110704     c                   delete    fiar7000
080000110704     c
080100110704     c     kar6          reade     fiar701l
080200110704    4c                   enddo
080300110704     c
080400110704     c     kar6          delete    fiar6000
080500110704    3c                   endif
080600110704    2c                   endif
080700110704   1ac                   endif
080800130710
080900130710     c                   endif
081000110704     c
081100130705     C***
081200130705     C***  KBLT          SETLL     FNBLT01L
081300130705     C***  KBLT          READE     FNBLT01L                               30
081400130705    2C***  *IN30         DOWEQ     *OFF
081500110704     C* SOLO SE NON C'E' GIA' LA DATA DI PARTENZA
081600130705    3C**N30BLTDFV        IFEQ      0
081700110704     C* E LA DATA BORDERO' DELLA TESTATA NON E' MAGGIORE ALLA NUOVA DATA SPED.
081800130705    2C***  BLPDBR        ANDLT     DSPsosp
081900130705     C***                CLEAR                   BLTNFV
082000130705     C***                UPDATE    FNBLT000
082100130705   X3C***                ELSE
082200130705     C***                UNLOCK    FNBLT01L
082300130705    3C***                ENDIF
082400130705     C***  KBLT          READE     FNBLT01L                               30
082500130705    2C***                ENDDO
082600111214     c
082700110704     C* AGGIORNO ANCHE FISGN SE PRESENTE
082800110704     C     KFISGN        SETLL     FISGN05L
082900110704     C     KFISGN        READE     FISGN05L                               30
083000110704    2C     *IN30         DOWEQ     *OFF
083100130702     C                   Z-ADD     DSPsosp       SGNMGS
083200110704     C                   Z-ADD     BLPTFA        SGNTNA
083300111214     c
083400110704     C                   CLEAR                   SGNT6A
083500110704     C                   CLEAR                   SGNT6B
083600110704     C                   CLEAR                   SGNT6C
083700110704     C                   CLEAR                   SGNT6D
083800110704     C                   CLEAR                   SGNT6E
083900110704     C                   CLEAR                   SGNT6F
084000110704     c                   eval      sgndatora = %char(%timestamp:*iso0)
084100130710     c                   if        ilsu3NOAGG=' '
084200110704     C                   UPDATE    FISGN000
084300130710     c                   else
084400130710     c                   unlock    fisgn05l
084500130710     C                   except    stasgn
084600130710     c                   endif
084700110704     C     KFISGN        READE     FISGN05L                               30
084800110704    2C                   ENDDO
084900110704     C*
085000110704     c* richiamo routine per gestione stampe
085100110704     c                   z-add     blpaas        wlpaas
085200110704     c                   z-add     blpnsp        wlpnsp
085300110704     c                   exsr      sr_stampe
085400110704     c*
085500130705   2ac                   endif
085600130705    1c                   endif
085700110704     c
085800110704     c                   ENDSR
085900980130     C*----------------------------------------------------------------
086000980130     C*
086100980130     C*--- OPERAZIONI INIZIALI ---------------------------------------*
086200980130     C     *INZSR        BEGSR
086300980130     C***
086400980130     C* KLIST
086500130705     C*
086600130715     C     KBLP70        KLIST
086700130715     c                   kfld                    klnp
086800130715     C                   KFLD                    kAAS
086900130715     C                   KFLD                    kMGS
087000130705     C     KBLT          KLIST
087100130705     C                   KFLD                    BLPAAS
087200130705     C                   KFLD                    BLPLNP
087300130705     C                   KFLD                    BLPNRS
087400130705     C                   KFLD                    BLPNSP
087500110704     C*
087600110704     C* KEY PER AGGIORNARE fiar4
087700080331     C     Kar4M         KLIST
087800080331     C                   KFLD                    BLPAAS
087900080331     C                   KFLD                    BLPLNP
088000080331     C                   KFLD                    BLPNRS
088100080331     C                   KFLD                    BLPNSP
088200110704     C                   KFLD                    ktrcM
088300051209     C     KBrv          KLIST
088400051209     C                   KFLD                    bltfls
088500051209     C                   KFLD                    bltlna
088600051209     C                   KFLD                    bltNRS
088700051209     C                   KFLD                    bltnsc
088800001120     C* KEY PER AGGANCIARE  FIAR6
088900001120     C     KAR6          KLIST
089000001120     C                   KFLD                    BLPAAS
089100001120     C                   KFLD                    BLPLNP
089200001120     C                   KFLD                    BLPNRS
089300001120     C                   KFLD                    BLPNSP
089400001120     C                   KFLD                    KTRC              1
089500980130     C* ACCESSO   FILE BOLLE VISTA LOGICA 2
089600130717     C**   KBLP77        KLIST
089700130717     c**                 kfld                    simfel
089800130717     C**                 KFLD                    knfv
089900010326     C* ACCESSO   FILE FISGN PER NUMERO SPEDIZIONE
090000010326     C     KFISGN        KLIST
090100010326     C                   KFLD                    BLPAAS
090200010326     C                   KFLD                    BLPLNP
090300010326     C                   KFLD                    BLPNRS
090400010326     C                   KFLD                    BLPNSP
090500980130     C* ACCESSO   TABEL
090600980130     C     KTAB2         KLIST
090700980130     C                   KFLD                    CODUT
090800980130     C                   KFLD                    COD               2
090900980130     C                   KFLD                    KEY               8
091000980130     C***
091100980130     C* DEFINIZIONE CAMPI
091200980130     C***
091300980130     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
091400980130     C***
091500980130     C                   CALL      'X§PARUT'
091600980130     C                   PARM                    UT§DSE
091700980202     C                   MOVEL     RAGUT         RSUT
091800980130     C                   MOVEL     REC80         CNCR80
091900980130     C*--- RICERCA CAPOCONTI
092000130705     C                   DO        50            X
092100980130     C                   MOVE      TCU(X)        TCUDS
092200980130     C     F56           CABNE     'CG'          END1
092300980130     C     F4            COMP      '1'                                    21
092400980130     C     F4            COMP      '2'                                    22
092500980130     C     F4            COMP      '3'                                    23
092600980130     C     F4            COMP      '6'                                    27
092700980130     C** 1 CLIENTI   21
092800980130     C** 2 FORNITORI 22
092900980130     C** 3 AGENTI    23
093000980130     C     F3            COMP      '0'                                242425
093100980130     C     F3            COMP      'I'                                    26
093200980130     C** 0 ITALIA   25
093300980130     C** 1 ESTERO   24
093400980130     ** I CAPO CONTO IVA
093500980130     C   21
093600980130     CAN 24              Z-ADD     KCU(X)        KCE               4 0
093700980130     C   21
093800980130     CAN 25              Z-ADD     KCU(X)        KCI               4 0
093900980130     C   22
094000980130     CAN 24              Z-ADD     KCU(X)        KFE               4 0
094100980130     C   22
094200980130     CAN 25              Z-ADD     KCU(X)        KFI               4 0
094300980130     C   23
094400980130     CAN 24              Z-ADD     KCU(X)        KAE               4 0
094500980130     C   23
094600980130     CAN 25              Z-ADD     KCU(X)        KAI               4 0
094700980130     C   26              Z-ADD     KCU(X)        KIVA              4 0
094800980130     C   27              Z-ADD     KCU(X)        KBNA              4 0
094900980130     C     END1          TAG
095000980130     C                   END
095100980130     C                   SETOFF                                       212223
095200980130     C                   SETOFF                                       242526
095300980130     C                   SETOFF                                       27
095400980202     C***
095500980202     C* CARICO DATI VARIABILI PARTENZE
095600980202     C***
095700980202     C                   MOVEL     '3I'          COD
095800980202     C                   MOVEL     '1       '    KEY
095900980202     C     KTAB2         CHAIN     TABEL00F                           31
096000980202     C  N31              MOVEL     TBLUNI        DS3I
096100980202     C   31              CLEAR                   DS3I
096200130705     c*
096300130705     c                   z-add     3             wGGL              1 0
096400980130     C*
096500980130     C* DATA BOLLETTAZIONE:  TRA UDATE E DATA GIORNO + §3IGGS
096600980130     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
096700980130     C                   TIME                    W0140            14 0
096800980130     C* UDATE IN GGMMAAAA
096900980130     C                   MOVE      W0140         WDTGIO            8 0
097000060404     C                   MOVEl     W0140         utime
097100980130     C*
097200980130     C* UDATE IN AAAAMMGG
097300980130     C                   Z-ADD     WDTGIO        G02DAT
097400980130     C                   MOVEL     *BLANK        G02ERR
097500980130     C                   CALL      'XSRDA8'
097600980130     C                   PARM                    WLBDAT
097700980130     C                   MOVEL     G02INV        DATEU             8 0
097800980130     C*
097900130702     c                   ENDSR
098000130702     c*
098100130702     C*****************************************************************
098200130702     c     CarTAB        BEGSR
098300130702     C*****************************************************************
098400130327     c*
098500020429     C                   Z-ADD     SIMPOU        CDU               3 0
098600130327     c*
098700130702     c                   clear                   bollema           9 0
098800130711     c                   clear                   olsu3boagg
098900130830     c                   clear                   olsu3bonagg
099000130830     c                   clear                   olsu3msg
099100130830     c                   clear                   olsu3err
099200130327     c*
099300111018     c                   Clear                   tibs02ds
099400111018     c                   Clear                   dmradan
099500111018     c                   Movel     'C'           T02Mod
099600111018     c                   Movel     KNSIF         T02Sif
099700111018     c                   Movel     'MRA'         T02Cod
099800130710     c                   Movel     'FNLSU3R'     T02Ke1
099900111018     c                   Call      'TIBS02R'
100000111018     c                   Parm                    Kpjba
100100111018     c                   Parm                    tibs02ds
100200111018    2c                   If        T02Err = *Blanks
100300111018     c                   Movel     T02Uni        Dmradan
100400111018    2c                   endif
100500060403     c*
100600060403     c                   clear                   qcmd
100700060403     c                   eval      qcmd = $cmd2(1)
100800060403     c                   call      'QCMDEXC'                            91
100900060403     c                   parm                    Qcmd
101000060403     c                   parm                    Qlen
101100130327     c*
101200061005     c                   clear                   qcmd
101300061005     c                   eval      qcmd = $cmd3(1)
101400061005     c                   call      'QCMDEXC'                            91
101500061005     c                   parm                    Qcmd
101600061005     c                   parm                    Qlen
101700130702     c
101800130702     c* Se non passata la data spedizione prendo udate
101900130705    1c                   if        ilsu3dsp_B<=*zeros
102000130702     c                   movel     dateu         Wdtcar
102100130702     c                   else
102200130702     c                   movel     ilsu3dsp_B    wdtcar            8 0
102300130705    1c                   endif
102400130715     c                   movel     wdtcar        kaas
102500130715     c                   move      wdtcar        kmgs
102600130702     c
102700130702     c* Calcolo la data sospensione
102800130705    1c                   if        ilsu3dsp_S<=*zeros
102900130702     c                   clear                   trul13ds
103000130705     c                   Eval      I13Tla = 'L'
103100130702     c                   Eval      I13Mod = 'P'
103200130702     c                   Eval      I13Fil = simfel
103300130705     c                   Eval      I13Din = wdtcar
103400130702     c                   Eval      I13gio = 1
103500130702     c                   Call      'TRUL13R'
103600130702     c                   Parm                    Trul13ds
103700130702     c* se da errore non posso spostare in sospensione
103800130705    2c                   if        o13err<>' '
103900130705     c                   eval      olsu3err='E2'
104000130702     c                   else
104100130702     c                   eval      dspsosp=o13dfi
104200130702     c                   movel     o13dfi        aaasosp
104300130702     c                   movel     o13dfi        Wamsos            6 0
104400130705    2c                   endif
104500130705     c
104600130705   x1c                   else
104700130702     c* data passata
104800130702     c                   movel     ilsu3dsp_s    DSPsosp
104900130702     c                   movel     ilsu3dsp_s    AAAsosp
105000130702     c                   movel     ilsu3dsp_s    wamsos
105100130705    1c                   endif
105200130715     c* Carico la £1 alla data spedizione
105300130715     c                   clear                   trul06ds
105400130715     C                   MOVE      '£1'          D06COD
105500130715     C                   MOVEL     SIMFEL        D06KEY
105600130715     c                   move      'S'           d06key
105700130715     c                   movel     wdtcar        d06drf
105800130715     C                   MOVEL     'L'           D06TLA
105900130715     C                   MOVEL     trul06ds      KPJBU
106000130715     C*
106100130715     C                   CALL      'TRUL06R'
106200130715     C                   PARM                    KPJBA
106300130715     C                   MOVEL     KPJBU         trul06ds
106400130715     C                   MOVEA     LIN           L1
106500980130     C*
106600980130     C                   ENDSR
106700081203      *---------------------------------------------------------------*
106800081203
106900081203     c     SR_stampe     BEGSR
107000081203     c*
107100081203     c* GESTIONE STAMPA SPEDIZIONE
107200130705     c                   if        ilsu3nos='S'
107300130705     c                   if        wggl > 0
107400081203     c* calcolo giorni lavorativi fra data spedizione e data immissione
107500081203     c                   clear                   wdat8
107600081203     c                   z-add     blpdim        datada
107700130702     c                   z-add     DSPsosp       dataa
107800081203     c                   call      'XSRLAV8'
107900081203     c                   parm                    wdat8
108000130705     c                   if        (ggl-1) > wggl
108100081203     c                   if        blpfns = 'S'
108200081203     c                   seton                                        02
108300081203     c                   else
108400081203     c                   setoff                                       02
108500081203     c                   endif
108600081203     c* inverto data ritiro per stampa
108700081203     c     *iso          move      blpdim        data_eur
108800081203     c                   move      data_eur      p1cdim
108900130710     c
109000130710     c     *iso          move      savdsp        data_eur
109100130710     c                   move      data_eur      v11dsp
109200130710     c* Imposto mittente
109300130710     c                   eval      p1crsm=%editc(wksc:'X')+'-'+blprsm
109400081203     c   98              write     testa
109500081203     c                   setoff                                       98
109600081203     c                   write     rigsped
109700081203     c                   endif
109800081203     c                   endif
109900081203     C**
110000130705     c** Se passati xx giorni di sospensione, invio msg
110100081203     c                   if        (ggl-1) > §3iggm
110200130705     c
110300130705     c* Memorizzo che ne ho sospensa alemeno una da altro xx giorni
110400130705     c                   eval      Unasospesa='S'
110500130705     c
110600130705     c                   if        tiposos='L'
110700130705     c                   add       1             BSPlnpBO(indx)
110800130705     c* memorizzo la data più bassa di immissione
110900130705     c                   if        BSPlnpD(indx)=0 or
111000130705     c                             blpdim>BSPlnpD(indx)
111100130705     c                   eval      BSPlnpD(indx)=blpdim
111200130705     c                   endif
111300130705     c
111400130705     c                   else
111500130705     c                   add       1             BSPkscBO(indx)
111600130705     c* memorizzo la data più bassa di immissione
111700130705     c                   if        BSPkscD(indx)=0 or
111800130705     c                             blpdim>BSPkscD(indx)
111900130705     c                   eval      BSPkscD(indx)=blpdim
112000130705     c                   endif
112100130705     c                   endif
112200130705     c
112300111017     c
112400081203     c                   endif
112500081203     c                   endif
112600081203     c                   endsr
112700081203      *---------------------------------------------------------------*
112800081203
112900081203     c     SR_nuovonumeroBEGSR
113000130710     c
113100130710     c                   z-add     blpmgs        savdsp
113200130710     c                   movel     blpaas        savdsp
113300130710
113400081203     c                   clear                   fnls65ds
113500130708     c                   z-add     blpaas        I65AAS
113600081203     c                   z-add     blplnp        I65LNP
113700081203     c                   z-add     blpnrs        I65NRS
113800081203     c                   z-add     blpnsp        I65NSP
113900130702     c                   z-add     AAAsosp       I65AASN
114000130702     c                   z-add     DSPsosp       I65MGSN
114100130705     c                   if        ilsu3nos='S'
114200081203     c                   movel     'S'           I65AGDRT
114300130705     c                   if        ilsu3dcr>*zeros  and blpdcr>0
114400130705     c                   movel     'S'           I65AGDCR
114500130705     c                   move      ilsu3tcr      I65TCRN
114600130705     c                   move      ilsu3dcr      I65DCRN
114700130705     c                   move      ilsu3hcr      I65HCRN
114800130705     c                   endif
114900130705     c                   endif
115000130705     c
115100081203     c                   move      '1'           I65STRCMT
115200081203     c                   move      '1'           I65FCMT
115300081203     c                   move      '1'           I65CMTRLBK
115400081203     c                   movel     fnls65ds      kpjbu
115500081210     c                   call      'FNLS65R'
115600081203     c                   parm                    kpjba
115700081203     c                   movel     kpjbu         fnls65ds
115800081203     c                   if        o65err=*blanks
115900081203     c                   add       1             bollema           9 0
116000130705     c                   z-add     AAAsosp       wlpaas
116100081205     c                   z-add     o65nsp        wlpnsp
116200081203     c                   exsr      sr_stampe
116300081204     c                   else
116400081205     c                   add       1             werror            6 0
116500081210     c                   if        o65err='1'
116600081210     c                   movel     o65msg        wmsg
116700081210     c                   endif
116800081203     c                   endif
116900081203     c                   endsr
117000111018     c*-------------------------------------------------------------------
117100111018     c     INV_email     BEGSR
117200111018     c
117300130705     c                   clear                   writerig
117400111018     c                   if        not %open(prtemail)
117500111018     C                   EXSR      SR_OPENPRTF
117600111018     C                   endif
117700111018     c
117800111018     c                   exsr      Routend
117900111018     c
118000111018     c                   ENDSR
118100111018
118200111018      /free
118300111018       //--------------------------------------------------------------
118400111018       //?Override al file di stampa per impostarvi i dati per
118500111018       //?  l'invio via e-mail   +   stampa inizio e-mail
118600111018       //--------------------------------------------------------------
118700111018       BEGSR sr_OpenPrtF;
118800111018
118900111018         // Override al file di stampa per impostarvi i dati per
119000111018         // l'invio via e-mail
119100111018           exsr sr_Override;
119200111018
119300130705
119400111019           zz = 1 ;
119500111019
119600130705          dow     BSPlnp(zz)>0  ;
119700130705
119800130705          if      BSPlnpBO(ZZ)>0 ;
119900130705          dataiso=%date(BSPlnpD(zz)) ;
120000111019          data_eur=dataiso  ;
120100111019
120200111019           clear O_testo;
120300130705           chain  BSPlnp(ZZ)   azorg01l  ;
120400130705           if not %found(azorg01l)  ;
120500130705           eval orgdes=*all'?'  ;
120600130705           endif  ;
120700130705           %subst(O_testo:6:20) = %editc(BSPlnp(zz):'X')+'-'+orgdes  ;
120800111019           %subst(O_testo:46:10) =%char(data_eur)  ;
120900130708           %subst(O_testo:64:9 ) =%editc(BSPlnpBO(zz):'2')  ;
121000111019           except PrtDet;
121100130705           writerig=writerig+1  ;
121200130705           endif  ;
121300130705
121400130705           // se ne ho scritte 50 di righe devo inviare
121500130705           if writerig>=50  ;
121600130705             exsr      Routend  ;
121700130705             clear writerig                  ;
121800130705
121900130705             exsr sr_Override;
122000130705
122100130705           endif  ;
122200111019
122300111019           zz=zz+1  ;
122400111019           enddo    ;
122500130705
122600130705          // Stesso ciclo sui cod clienti
122700130705          zz = 1      ;
122800130705          dow     BSPksc(zz)>0  ;
122900130705
123000130705          if      BSPkscBO(ZZ)>0 ;
123100130705          dataiso=%date(BSPkscD(zz)) ;
123200130705          data_eur=dataiso  ;
123300130705
123400130705           clear O_testo;
123500130705           chain  (1:kci:BSPksc(ZZ)) cnaco00f  ;
123600130705           if not %found(cnaco00f)  ;
123700130705           eval acorag=*all'?'  ;
123800130705           endif  ;
123900130705           %subst(O_testo:6:20) = %editc(BSPksc(zz):'X')+'-'+acorag  ;
124000130705           %subst(O_testo:46:10) =%char(data_eur)  ;
124100130708           %subst(O_testo:64:9 ) =%editc(BSPkscBO(zz):'2')  ;
124200130705           except PrtDet;
124300130705           writerig=writerig+1  ;
124400130705           endif  ;
124500130705
124600130705           // se ne ho scritte 50 di righe devo inviare
124700130705           if writerig>=50  ;
124800130705             exsr      Routend  ;
124900130705             clear writerig                  ;
125000130705
125100130705             exsr sr_Override;
125200130705
125300130705           endif  ;
125400130705
125500130705           zz=zz+1  ;
125600130705           enddo    ;
125700111018
125800130705             exsr      Routend  ;
125900111018       ENDSR;
126000111018
126100111018       //--------------------------------------------------------------
126200111018       //?Override al file di stampa per impostarvi i dati per
126300111018       //?  l'invio via e-mail   +   stampa inizio e-mail
126400111018       //--------------------------------------------------------------
126500111018       BEGSR sr_Override;
126600111018
126700111018         reset TRTCM1ds;
126800111018
126900111018         IF  §MRAdreg <> *blank;
127000111018           §CM1mitt = %trim(§MRAdmitt);
127100111018           §CM1dst  = %trim(§MRAddest);
127200111019
127300111019         // Valorizzo i campi della e-m@ail
127400111019          chain  simpou   azorg01l ;
127500111019
127600111019          if   %found(azorg01l) ;
127700140205           og140=orgde0  ;
127800140205           if §ogapo>*zeros  ;
127900140205           §cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
128000140205           else  ;
128100120802           §cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
128200111019          endif  ;
128300140205          endif  ;
128400111019
128500111018           §CM1tips = §MRAdreg;
128600111018           §CM1po   = %editc(simfel:'X');
128700111019           §CM1var  = '*OBJM*' + 'Manutenzione DATA SPEDIZIONE bolle'   ;
128800111018           §CM1idp  = §MRAdidpro;
128900111018           Qcmd = C_CmdOvrPrtF
129000111018                + ' outq(' + %trim(§MRAdoutqi) + ')'
129100111018                + ' usrdfndta(''' + TRTCM1ds + ''')';
129200111018         ELSE;
129300111018           Qcmd = C_CmdOvrPrtF;
129400111018         ENDIF;
129500111018
129600111018         callp(e) qCmdExc(Qcmd : %size(Qcmd));
129700111018
129800130708           open PrtEMAIL;
129900130705
130000130705         // Stampa una testata se NON è richiesta la e-mail
130100130705           IF  §MRAdreg =  *blank;
130200130705             O_testo = JobUser + ' - ' + SDSpgm
130300130705                     + ' - ' + %editc( *date : 'Y' )
130400130705                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
130500130705             except PrtDet;
130600130705             clear O_testo;
130700130705             except PrtDet;
130800130705             except PrtDet;
130900130705           ENDIF;
131000130705
131100130705         // Stampa testo iniziale
131200130705           O_testo = 'E'' stata manutenzionata la DATA SPEDIZIONE +
131300130705                      di bolle immesse da almeno xx giorni.' ;
131400130705           %subst(O_testo:71:2) = (%editc(§3iggm: '4'))     ;
131500130705           except PrtDet;
131600130705           O_testo = 'Verificare col mittente o procedere alla lor+
131700130705                      o chiusura come "Merce MAI Affidata".' ;
131800130705           except PrtDet;
131900130705
132000130705         // Stampa una riga vuota
132100130705           clear O_testo;
132200130705           except PrtDet;
132300130705
132400130705         // Stampa intestazione
132500130705           O_testo = ' Bolle spostate in sospensione di          Data imm+
132600130705                     issione    Bolle da vericare elencate';
132700130705           except PrtDet;
132800130705           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
132900130705                     chia       in apposita stampa.Utente:';
133000130705           %subst(o_testo:90:10)= knmus     ;
133100130705           except PrtDet;
133200130705
133300130705         // Stampa una riga vuota
133400130705           clear O_testo;
133500130705           except PrtDet;
133600111018       ENDSR;
133700111018
133800111018       //--------------------------------------------------------------
133900111018       //?Operazioni finali.
134000111018       //--------------------------------------------------------------
134100111018       BEGSR RoutEnd;
134200111018
134300111018         if %open(PrtEMAIL);
134400111018
134500111018         //?Chiusura dello spool?
134600111018           clear O_testo;
134700111018           except PrtDet;
134800111018           O_testo = '***   Fine Lista   ***';
134900111018           except PrtDet;
135000111018
135100111018           close PrtEMAIL;
135200111018
135300111018         //?Eliminazione overflow?
135400111018           Qcmd = C_CmdDltOvr;
135500111018           callp(e) qCmdExc(Qcmd : %size(Qcmd));
135600111018
135700111018         endif;
135800111018
135900111018       ENDSR;
136000111018
136100111018      /end-free
136200111018     o* ---------------------------------------------------------------
136300111018     o* ?Spool di stampa (per e-mail).
136400111018     o* ---------------------------------------------------------------
136500111018     oPrtEMAIL  e            PRTdet      1
136600111018     o                       O_testo
136700130710     o* ---------------------------------------------------------------
136800130710     Oqsysprt   E            stabolla    1
136900130710     o                                              'Bolla:'
137000130710     O                       BLPaas        Z   +  1
137100130710     O                       BLPlnp            +  1
137200130710     O                       BLPNRS        Z   +  1
137300130710     O                       BLPNSP        Z   +  1
137400130710     O                       BLPmgs            +  1 '  /  '
137500130710     o                                         +  5 'Cliente'
137600130710     O                       wksc              +  1
137700130710     O                       BLPrsm            +  1
137800130710     Oqsysprt   E            stasgn      1
137900130710     o                                              'Chi sono'
138000130710     O                       SGNWHO            +  1
138100130710     O                       sgnksc            +  1
138200130710     O                       sgnNRS        Z   +  1
138300130710     O                       sgnNSP        Z   +  1
138400130710     O                       sgnmgs            +  1 '  /  '
138500130710     O                       sgntna            +  1
138600111018
138700030619**  $Cmd
138800030707OVRPRTF FILE(&FILE     ) OUTQ(&OUTQ     ) FORMTYPE('&FORMTYPE ')
138900030619           USRDTA('Manut.Boll') SHARE(*YES)
139000030619           COPIES(2)
139100060403**  $Cmd2
139200130710OVRPRTF FILE(FNLSU3P) SAVE(*YES)
139300061005**  $Cmd3
139400061005OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES) OUTQ(LJ4200CED) USRDTA('EDPES')
139500061012**  DLTOVR
139600061012DLTOVR  FILE(QSYSPRT)
