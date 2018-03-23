000100050705     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050705
000300050705      *------------------------------------------------------------------------*
000400050705      *                                                                        *
000500050705      *                 COPIA TARIFFA/OFFERTA              ?                   *
000600050705      *                                                                        *
000700050705      *------------------------------------------------------------------------*
000800050705
000900150210     fTntam01l  if   e           k disk
001000150210     fTNTAM00L  uf   e           k disk    rename(TNTAM000:TNTAM0L)
001100150210     fTNTAM04L  if   e           k disk    rename(TNTAM000:TNTAM04)
001200150219     f                                     prefix(w_)
001300050712     fTitad04l  if   e           k disk
001400050712     fTitpt01l  if   e           k disk
001500050728     fTitpd01l  if   e           k disk
001600050712     fTitgc01l  if   e           k disk
001700050707     fTnofm01l  if   e           k disk    usropn
001800050711     f                                     rename(tntam000:tnofm)
001900050706     fTiofd01l  if   e           k disk    usropn
002000050711     f                                     rename(titad000:tiofd)
002100050707     fTiopt01l  if   e           k disk    usropn
002200050711     f                                     rename(titpt000:tiopt)
002300050728     fTiopd01l  if   e           k disk    usropn
002400050728     f                                     rename(titpd000:tiopd)
002500050729     fTiogc01l  if   e           k disk    usropn
002600050729     f                                     rename(titgc000:tiogc)
002700050728     fTeetc01l  if   e           k disk
002800140128     fTIACB01L  if   e           k disk
002900050705     fTabel00f  if   e           k disk
003000130807     fAZCMM01L  if   e           k disk
003100090915     fTivis05l  if   e           k disk    usropn
003200060307     fcnclp00f  if   e           k disk    usropn prefix(w_)
003300060307     ftfclp00f  if   e           k disk    usropn prefix(w_)
003400060307     f                                     rename(cnclp000:tfclp000)
003500140115     fTIPMG01L  if   e           k disk
003600150209     fTITAS41C  if   e           k disk    usropn extfile(wFLib)
003700050707     fAzorg01l  if   e           k disk
003800050919     fTnta24D   cf   e             workstn sfile(ta24s02:nrr2)
003900050919     f                                     sfile(ta24s03:nrr3)
004000050706     f                                     sfile(ta24s04:nrr4)
004100050706     f                                     sfile(ta24s05:nrr5)
004200050705
004300050705      *------------------------------------------------------------------------*
004400050705      *  RIEPILOGO INDICATORI
004500050705      *------------------------------------------------------------------------*
004600050727      * 01 - Pgm richiamato per copia su offerta
004700050727      * 02 - Utente non abilitato - esce dal pgm
004800050727      * 03 - Copia da Offerta - N03 Copia da Tariffa
004900050727      * 04 - Copia su tariffa già esistente
005000050727      * 05 - Importi tariffa (old) in percentuale
005100050727      * 06 - Visualizzo 'M' x indicare presenza del minimo o del massimo
005200050727      * 07 - Tariffa particolare (old) assicurazione per conto
005300050727      * 08 - Tariffa particolare (old) RCV
005400050727      * 09 - Tariffa particolare (old) a valore
005500050728      * 10 - Proteggo campo scelta nel dettaglio tariffe particolari
005600050729      * 11 - Visualizzo arrotondamenti nelle tariffe particolari
005700050705      * 12 - Sede - N12 Filiale
005800050729      * 13 - Visualizzo rapporto peso volume nelle tariffe particolari
005900050729      * 14 - Visualizzo valore merce nelle tariffe particolari
006000050729      * 15 - Visualizzo tipo mandato nelle tariffe particolari
006100050729      * 16 - Visualizzo tipo applicazione nelle tariffe particolari
006200050711      * 17 - Chiave di ricerca nel subfile dettaglio tariffario
006300050714      * 18 - Posizionamento subfile x errore
006400050909      * 19 - Proteggo tipo tariffa se non è copia da tariffa di cartello
006500050705      * 20 - GESTIONE SUBFILE
006600050705      * 21 - GESTIONE SUBFILE
006700050705      * 22 - GESTIONE SUBFILE
006800050713      * 23 - ROLLUP
006900090915      * 24 - Visualizza data riferimento fuel
007000050729      * 25 - Visualizzo il minimo nelle tariffe particolari
007100050729      * 26 - Visualizzo il massimo nelle tariffe particolari
007200050729      * 27 - Visualizzo inoltro nelle tariffe particolari
007300050705      * 28 - ERRORE GENERICO DSPF
007400050921      * 29 - ERRORE Copia tariffa
007500050705      * 30 - Comodo
007600050714      * 31 - Fine file
007700050714      * 32 - Fine subfile
007800050727      * 33 - Fine file
007900050727      * 34 - Fine subfile
008000150219      * 35 - Cliente a cui copio presente in tab. VMA
008100050705      * 40 - Cliente/Offerta da cui copiare
008200050705      * 41 - Tariffa da cui copiare
008300050707      * 42 - Progressivo da cui copiare
008400050707      * 43 - Cliente a cui copiare
008500050707      * 44 - Tariffa a cui copiare
008600050707      * 45 - Progressivo a cui copiare
008700050707      * 46 - Dettaglio tariffario
008800050909      * 47 - Tariffe particolari
008900050909      * 48 - Tariffe di giacenza
009000050909      * 49 - Dati tipo
009100050909      * 50 - Tipo tariffa I/E
009200050909      * 51 - Linee di partenza
009300050909      * 52 - Codici di tassazione
009400050909      * 53 - Scaglione
009500120112      * 54 - Copia su offerta tipo trattativa "NUOVA" x AC BASE
009600050705      * 90 - Riemetto la videata
009700050705      *------------------------------------------------------------------------*
009800050705
009900050705      *   V A R I A B I L I
010000050728     d comlnp          s                   like(tadlnp)
010100050729     d datadmy         s               d   datfmt(*eur)
010200060802     d dataymd         s               d   datfmt(*iso)
010300050711     d kcap            s                   like(tadcap)
010400050705     d kcod            s                   like(tblcod)
010500050728     d kcto            s                   like(etccto)
010600050705     d kctr            s                   like(tamctr)
010700050801     d kdsf            s                   like(etcdsf)
010800050705     d kkey            s                   like(tblkey)
010900050705     d kksc            s                   like(tamksc)
011000050705     d kkut            s                   like(tblkut) inz(1)
011100050728     d kftc            s                   like(tpdftc)
011200050711     d klnp            s                   like(tadlnp)
011300050711     d knaz            s                   like(tadnaz)
011400050711     d korp            s                   like(tadorp)
011500140227     d kpmg            s                   like(TPTdpb)
011600050706     d kprg            s                   like(tamprg)
011700050711     d ksgl            s                   like(tadsgl)
011800050919     d nrr2            s                   like(recsf2)
011900050714     d nrr3            s                   like(recsf3)
012000050714     d nrr4            s                   like(recsf4)
012100050714     d nrr5            s                   like(recsf5)
012200050708     d ntwksc          s                   like(§ogntw)
012300090414     d oldkscn         s                   like(v1kscn)
012400050706     d oldlnp          s                   like(tadlnp)
012500050706     d oldcts          s                   like(tadcts)
012600050714     d pagine          s              4  0
012700050714     d resto           s              3  0
012800150210     d sav_TAMksc      s                   like(TAMksc) inz
012900150210     d sav_TAMctr      s                   like(TAMctr) inz
013000150210     d sav_TAMprg      s                   like(TAMprg) inz
013100150210     d sav_TAMddt      s                   like(TAMddt) inz
013200150210     d sav_TAMdst      s                   like(TAMdst) inz
013300050714     d savcap          s                   like(tadcap)
013400050729     d savcat          s                   like(ta25cat)
013500060307     d savclva         s                   like(clpclv)
013600050729     d savctr          s                   like(tamctr)
013700060705     d savfabitra      s                   like(d§fabitr)
013800120112     d oldfabitra      s                   like(d§fabitr)
013900060307     d savfmp          s                   like(tamfmp)
014000050714     d savksc          s                   like(tamksc)
014100050714     d savlnp          s                   like(tadlnp)
014200050714     d savnaz          s                   like(tadnaz)
014300050727     d savnrr          s                   like(nrr3)
014400050714     d savorp          s                   like(tadorp)
014500050714     d savprg          s                   like(tamprg)
014600050714     d savsgl          s                   like(tadsgl)
014700050729     d savtad          s                   like(ta25tad)
014800050729     d savtgc          s                   like(ta25tgc)
014900050729     d savtpt          s                   like(ta25tpt)
015000050711     d wabi            s                   like(UteAut)
015100060307     d wacbase         s              1    inz('0')
015200060705     d wacbasefmp      s              1    inz('0')
015300050705     d wcmm            s                   like(clpage)
015400050712     d wcopiatad       s              1    inz('0')
015500050729     d wcopiatgc       s              1    inz('0')
015600050728     d wcopiatpd       s              1    inz('0')
015700050728     d wcopiatpt       s              1    inz('0')
015800050912     d wctrn           s              2  0
015900050912     d wctro           s              2  0
016000050707     d wcts            s             20
016100150210     d wDataScadNw     s                   like(TAMdst)
016200150210     d wDataSP         s                   like(TAMddt)
016300150219     d wData           s                   like(TAMddt)
016400050706     d wdpd            s                   like(§tadpd)
016500050708     d wdst            s                   like(tamdst)
016600050706     d wfed            s                   like(§tafed)
016700050706     d wfie            s                   like(tamfie)
016800150210     d wFineW02        s               n   inz
016900060307     d wforzaacbase    s              1    inz('0')
017000060307     d wforzaclv       s              1    inz('0')
017100050708     d wforzaftc       s              1    inz('0')
017200050922     d wforzalnp       s              1    inz('0')
017300060130     d wforzapar       s              1    inz('0')
017400050708     d wftc            s                   like(tptftc)
017500050708     d wftceli         s              1    inz('0')
017600150210     d wInzW02         s               n   inz
017700050707     d wlnp            s              5  0
017800050714     d wmaxnrr         s              4  0
017900050707     d wmat            s                   like(§taf02)
018000050715     d wnewcar         s              1    inz('0')
018100050923     d wnewlnp         s                   like(tadlnp)
018200050727     d wnrr            s              4  0
018300050727     d wnrr1           s              4  0
018400150210     d wokAGGdst       s               n   inz
018500050711     d wokcts          s              1    inz('0')
018600090414     d wokkscn         s               n
018700050711     d woklnp          s              1    inz('0')
018800050728     d wokrec          s              1    inz('0')
018900050711     d woksgl          s              1    inz('0')
019000050707     d woktad          s              1    inz('0')
019100050707     d woktpt          s              1    inz('0')
019200050805     d woldcar         s              1    inz('0')
019300050714     d wpagsf3         s                   like(recsf3)
019400050705     d wporagru        s              3
019500050714     d wprg            s              3
019600050909     d wselt           s              1    inz('0')
019700050711     d wslnp           s              1    inz('0')
019800050711     d wscts           s              1    inz('0')
019900050711     d wssgl           s              1    inz('0')
020000050706     d wtad            s              5
020100050705     d wtibs69         s              1    inz('0')
020200050708     d wtsp            s                   like(tamtsp)
020300050705     d w001a           s              1
020400050707     d w002a           s              2
020500050819     d w003a           s              3
020600050714     d w005a           s              5
020700050708     d w0030           s              3  0
020800050705     d w0070           s              7  0
020900050711     d w0080           s              8  0
021000120119     d W_v1ksco        s                   like(v1ksco)
021100120119     d W_v1nrvot       s                   like(v1nrvot)
021200120119     d W_v1ctro        s                   like(v1ctro)
021300120119     d W_v1prgo        s                   like(v1prgo)
021400050707     d xx              s              3  0
021500140128     d dataoggi        s              8s 0
021600150209
021700150209       // -?Campi libreria e file per TITAS41C
021800150209     d wFLib           s             21
021900150209     d wLib            s             10
022000050819
022100050705      *   S C H I E R E
022200090415     d ksa             s              4    Dim(30)
022300090415     d ksc             s              7  0 Dim(30)
022400150210     d msg             s             78    Dim(65) ctdata perrcd(1)
022500050920     d sklnpo          s              3    Dim(50)
022600050920     d sklnpn          s              3    Dim(50)
022700050705     d skpog           s              3    Dim(250) inz(*Zeros)
022800050705     d skpogc          s              3    Dim(250) inz(*Zeros)
022900050707     d sktad           s              5    Dim(500)
023000050921     d sktpt           s              2    Dim(50)
023100080609      * schiera filiali abilitate al lasciato avviso
023200080609     d sklav           s              3  0 dim(999)
023300050705
023400050705      *   D S   I N T E R N E / E S T E R N E
023500090415     d                 ds
023600090415     d dsksn                   1      4p 0
023700090415     d dsksa                   1      4
023800090415
023900050705     d parmta36        ds
024000050705     d  ta36ksc                6     12  0
024100050705     d  ta36ctr               13     15
024200050705     d  ta36flg               16     16
024300051121     d  ta36vpr               17     17
024400050705     d  ta36kcc               18     21  0
024500050705     d  ta36prg               37     39
024600050817
024700050707     d                 ds
024800050908     d v1clnp1                 1      3  0
024900050908     d v1clnp2                 4      6  0
025000050908     d v1clnp3                 7      9  0
025100050908     d v1clnp4                10     12  0
025200050908     d v1clnp5                13     15  0
025300050921     d sklnp                   1     15  0 dim(5)
025400050707
025500050707     d                 ds
025600050908     d v1ccts1                 1      2
025700050908     d v1ccts2                 3      4
025800050908     d v1ccts3                 5      6
025900050908     d v1ccts4                 7      8
026000050908     d v1ccts5                 9     10
026100050908     d v1ccts6                11     12
026200050908     d v1ccts7                13     14
026300050909     d skcts                   1     14    dim(7)
026400150210
026500150210     d wlbdat          ds                  inz
026600150210     d  g02dat                 1      8  0
026700150210     d  g02inv                 9     16  0
026800150210     d  g02err                17     17
026900150210     d  g02tgi                18     22  0
027000050705
027100050705     d                sds
027200050705     d  Vtcpgm                 1     10
027300050705
027400050705     d Azuteds       e ds                  Extname(Azute00f)
027500050705     d dDatiute      e ds
027600050912     d ddfe          e ds
027700060705     d dfab          e ds
027800050912     d dlat          e ds
027900050912     d dsct          e ds
028000050706     d dsta01        e ds
028100050705     d dstr          e ds
028200050708     d ds1p          e ds
028300050711     d ds15          e ds
028400050729     d ds2f          e ds
028500050729     d ds2h          e ds
028600050708     d ds5e          e ds
028700050705     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
028800050705     d ds_cnind      e ds                  inz  extname(Cnind00f)
028900050705     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
029000050705     d ds_fncls      e ds                  inz  extname(Fncls00f)
029100140115
029200140115       // -?DS campo TPTflo
029300140115     d dTPT01        e ds
029400140115
029500050705     d dute01        e ds
029600050705     d Kpjba         e ds
029700050708     d og143         e ds
029800050705     d Tibs02ds      e ds
029900050705     d Tibs34ds      e ds
030000050705     d Tibs69ds      e ds
030100050705     d Tnta24ds      e ds
030200050708     d Tnta25ds      e ds
030300050712     d Tnta25tad     e ds                  inz
030400050712     d Tnta25tgc     e ds                  inz
030500050712     d Tnta25tpd     e ds                  inz
030600050712     d Tnta25tpt     e ds                  inz
030700120712     d Trtb09ds      e ds
030800050705     d Trul31ds      e ds
030900080609     d dlav          e ds
031000080609     d tntbeds       e ds                  extname(tntbe00f)
031100090415     d xclirds       e ds
031200150219     d dVMA          e ds
031300050705
031400050705      *   C O S T A N T I
031500051014      * titolo videata (lunghezza massima 24)
031600051014     d VtcTit          C                   CONST('* DUPLICAZIONE TARIFFA *')
031700050714      * nr. di rercord per pagina del subfile dettaglio tariffario
031800050714     d sflpag3         C                   CONST(14)
031900091021      * per controllo campi numerici
032000091021     d Digits          c                   const('0123456789')
032100150210
032200150210      //---------------------------------------------------------------
032300150210      //?Prototipi.
032400150210      //---------------------------------------------------------------
032500150219      /copy gaitrasrc/srcprotopr,TIBS02R
032600150210      /copy gaitrasrc/srcprotopr,xsrda8
032700160211
032800160211     d TNTAT1ds      e ds                  inz
032900160211      /copy gaitrasrc/srcProtoPR,TNTAT1R
033000150217
033100150217      //---------------------------------------------------------------
033200090915
033300090915     itivis000
033400090915     i              visesi                      wisesi
033500050705
033600050705      *------------------------------------------------------------------------*
033700080609
033800080609     c/exec sql
033900080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
034000080609     c/end-exec
034100080609
034200080609      * carico tabella filiali attive al lasciato avviso
034300080609     c                   exsr      carlav
034400050819
034500050705     c                   ExSr      Sr_Puld01
034600050706     c                   ExSr      Sr_Pult02
034700050706
034800050707      * --> Prima videata
034900050707     c     Emid01        Tag
035000050706      * Pulisco la videata
035100050705     c                   If        Not *In28 and Not *In90
035200050705     c                   ExSr      Sr_Puld01
035300050708     c                   ExSr      Sr_Pult02
035400050705     c                   EndIf
035500050706      * Emetto la videata
035600050705     c                   Write     Ta24t01
035700050705     c                   Exfmt     Ta24d01
035800050819     c                   Eval      *In28 = *Off
035900050705     c                   Eval      *In90 = *Off
036000050705     c                   Clear                   v1cmsg
036100050819      * F3=Fine
036200050819     c                   If        *InKc or
036300050727      * 02=utente non abilitato
036400050727     c                             *In02
036500050727     c                   If        *In01 and *Inkc
036600050711     c                   Eval      ta24f03 = '1'
036700050711     c                   EndIf
036800050707     c                   Goto      Fine
036900050819     c                   EndIf
037000050819      * Controllo
037100050705     c                   ExSr      Sr_Ctrd01
037200050705     c                   If        *In28 or *In90
037300050707     c                   Goto      Emid01
037400050705     c                   EndIf
037500050712      * per enter devo restare nella videata
037600050712     c                   If        Not *Inkf
037700050712      * accendo il 90 così non pulisco il video
037800050712     c                   Eval      *In90 = *On
037900050909     c                   Goto      Emid01
038000050712     c                   EndIf
038100150209
038200050711      * F6=Conferma
038300050729     c   kf              ExSr      Sr_Conferma
038400050708
038500050729      * --> Videata di Riepilogo
038600050729     c     Riepilogo     Tag
038700050713      * Emetto la videta del riepilogo di ciò che è stato fatto
038800050713     c                   Exfmt     ta24d07
038900050803     c                   Eval      *In28 = *Off
039000050803     c                   Clear                   v7cmsg
039100050713      * F3=Fine
039200050713     c                   If        *InKc
039300050801      * prima di uscire integro la triffa con i dati eventualmente
039400050801      * mancanti
039500050909      * se ho copiato qualcosa
039600050913     c                   If        wcopiatad = *On or wcopiatpt = *On or
039700050922     c                             wcopiatpd = *On or wcopiatgc = *On
039800050801     c                   ExSr      Sr_Integra
039900080610      * visto che sto integrando quindi richiamo la inglo, devo anche creare
040000080610      * la tariffa particolare 'f' = Fuel
040100080610      * se non già presente, se nuova tariffa/offerta
040200080610      * ma solo se sto copiando da cartello o verso una cartello
040300080610     c                   if        wnewcar = *off and woldcar = *on
040400080610     c                             and not *in04
040500080610     c                   exsr      sr_fuel
040600080610     c                   endif
040700050909     c                   EndIf
040800050803      * esco dal pgm senza gestire nessun errore xchè diamo x scontato
040900050803      * che la cartello vada bene
041000050713     c                   Goto      Fine
041100050713     c                   EndIf
041200050713     c                   Goto      Riepilogo
041300050707
041400050819     c     Fine          Tag
041500050819
041600050819      * Chiude i file dei pgm richiamati
041700050819     c                   If        wtibs69 = *On
041800050819     c                   Eval      i69tla = 'C'
041900050819     c                   Call      'TIBS69R'
042000050819     c                   Parm                    Tibs69ds
042100050819     c                   EndIf
042200050819
042300050819     c                   Eval      *InLr = *On
042400050819
042500050705      *------------------------------------------------------------------------*
042600050705      * PULIZIA PRIMA VIDEATA
042700050705      *------------------------------------------------------------------------*
042800050705     c     Sr_Puld01     BegSr
042900050705
043000060307     c                   eval      wforzaacbase = *off
043100060307     c                   eval      wforzaclv = *off
043200050708     c                   Eval      wforzaftc = *Off
043300050922     c                   Eval      wforzalnp = *Off
043400060130     c                   Eval      wforzapar = *Off
043500050714     c                   Clear                   wprg
043600050711     c                   Clear                   v1ksco
043700091118     c                   Clear                   v1nrvot
043800090408     c                   clear                   v1dksco
043900050705     c                   Clear                   v1ctro
044000050711     c                   Clear                   v1prgo
044100050711     c                   Clear                   v1kscn
044200091118     c                   Clear                   v1nrvnt
044300090408     c                   clear                   v1dkscn
044400050705     c                   Clear                   v1ctrn
044500050705     c                   Clear                   v1prgn
044600050922     c                   Eval      v1ctad = 'T'
044700050922     c                   Eval      v1ctpt = 'T'
044800050922     c                   Eval      v1ctgc = 'T'
044900050705     c                   Eval      v1ccat = 'N'
045000050705
045100050705      * se richiamato imposto i dati ricevuti
045200050727     c                   If        *In01
045300091118     c                   Eval      v1nrvnt= ta24nrv
045400050711     c                   Move      ta24ctr       v1ctrn
045500050923     c                   Move      ta24prg       v1prgn
045600050705     c                   EndIf
045700050908
045800050908     c                   Clear                   v1cfie
045900050908     c                   Clear                   v1csad
046000050908     c                   Clear                   v1cdec
046100050908     c                   Eval      v1cio1 = 'O'
046200050921     c                   Clear                   sklnp
046300050908     c                   Eval      v1cio2 = 'O'
046400050908     c                   Clear                   skcts
046500050908     c                   Eval      v1cio3 = 'O'
046600050908     c                   Clear                   v1csgl
046700050705
046800050705     c                   EndSr
046900050706
047000050706      *------------------------------------------------------------------------*
047100050706      * PULIZIA VIDEATA DEI DATI DELLA TARIFFA/OFFERTA DA COPIARE
047200050706      *------------------------------------------------------------------------*
047300050706     c     Sr_Pult02     BegSr
047400050706
047500050706     c                   Clear                   v2cksc
047600090916     c                   Clear                   v2cnrvt
047700050706     c                   Clear                   v2dksc
047800050706     c                   Clear                   v2cprg
047900050706     c                   Clear                   v2cdiv
048000050706     c                   Clear                   v2cctr
048100050706     c                   Clear                   v2dctr
048200050706     c                   Clear                   v2cdcv
048300050706
048400050706     c                   EndSr
048500050705
048600050819      *------------------------------------------------------------------------*
048700050705      * CONTROLLO LA PRIMA VIDEATA
048800050819      *------------------------------------------------------------------------*
048900050705     c     Sr_Ctrd01     BegSr
049000050707
049100050727     c                   Eval      *In04 = *Off
049200050912     c                   Eval      *In19 = *Off
049300050707     c                   Eval      woktad = *Off
049400050715     c                   Eval      wnewcar = *Off
049500050805     c                   Eval      woldcar = *Off
049600050805     c                   Clear                   comlnp
049700050922     c                   Eval      wslnp = *Off
049800050922     c                   Eval      wscts = *Off
049900050922     c                   Eval      wssgl = *Off
050000050711
050100050711      * Spengo gli indicatori di posizionamento cursore
050200050711     c                   Setoff                                       404142
050300050711     c                   Setoff                                       434445
050400050728     c                   Setoff                                       464748
050500050728     c                   Setoff                                       495051
050600160304     c                   Setoff                                       525354
050700120119      * se in caso di precedente uscita del msg di ac base incongruente
050800120119      * verifico se impostata una tariffa o una offerta diversa da prima
050900120119      * spendo la fprzatura ac base
051000120119     c                   If        wforzaclv = *on   and
051100120119     c                             (w_v1ksco <> v1ksco or
051200120119     c                              w_v1nrvot <> v1nrvot or
051300120119     c                              w_v1ctro <> v1ctro or
051400120119     c                              w_v1prgo <> v1prgo)
051500120119     c                   eval      wforzaclv = *off
051600120119     c                   endif
051700050708
051800050708      * COPIA DA TARIFFA
051900050727if  1c                   If        Not *In03
052000050705
052100050708      * --> Cliente da cui copiare
052200050705      *     obbligatorio
052300050708if  2c                   If        v1ksco = *Zeros
052400090408     c                             or v1ksco = *blanks
052500050819     c                   Eval      *In28 = *On
052600050819     c                   Eval      *In40 = *On
052700050705     c                   Eval      v1cmsg = msg(02)
052800050712     c                   Eval      %subst(v1cmsg:11:10) = 'il Cliente'
052900050819     c                   Leavesr
053000050708    2c                   EndIf
053100090414      *     ricerca cliente
053200090408     c                   if        %scan('?':v1ksco) > *zeros
053300090415     c                   clear                   xclirds
053400090415     c                   eval      dxcaut = §utectc
053500090408     c                   exsr      sr_ricksc
053600090415     c                   move      ksc(1)        v1ksco
053700090408     c                   eval      *in90 = *on
053800090408     c                   leavesr
053900090408     c                   endif
054000091021      *     deve essere numerico
054100091021     c                   if        %check(digits:v1ksco) > *zeros
054200091021     c                   eval      *in28 = *on
054300091021     c                   eval      *in40 = *on
054400091021     c                   eval      v1cmsg = msg(59)
054500091021     c                   leavesr
054600091021     c                   endif
054700050705      *         deve esistere
054800050705     c                   Move      v1ksco        w0070
054900050705     c                   ExSr      Sr_Chkcli
055000050705if  2c                   If        O69err = *On
055100050705     c                   Eval      *In28 = *On
055200050705     c                   Eval      *In40 = *On
055300050705     c                   Eval      v1cmsg = o69msg
055400050705     c                   Leavesr
055500050706    2c                   EndIf
055600120112      *     cerco l'importo per la forzatura AC BASE (tab. FAB)
055700120112     c                   clear                   tibs02ds
055800120112     c                   clear                   dfab
055900120112     c                   eval      t02mod = 'C'
056000120112     c                   eval      t02sif = knsif
056100120112     c                   eval      t02cod = 'FAB'
056200120112     c                   movel     v1ksco        t02ke1
056300120112     c                   call      'TIBS02R'
056400120112     c                   parm                    kpjba
056500120112     c                   parm                    tibs02ds
056600120112     c                   if        t02err = *blanks
056700120112     c                   eval      dfab = t02uni
056800120112     c                   endif
056900120112     c                   eval      oldfabitra = d§fabitr
057000120112
057100090408     c                   eval      v1dksco = acorag
057200050705      *         deve essere gestibile dall'utente
057300050705     c                   Movel     v1ksco        w003a
057400050715     c                   Movel     v1ksco        w005a
057500050705      *         se non è tariffa di cartello
057600050715if  2c                   If        w005a <> '88888'
057700050923     c                   Eval      woldcar = *Off
057800050705     c     w003a         Lookup    skpogc                                 30
057900050705      *         non trovo nella schiera dei p.o. gestibili
058000050705if  3c                   If        Not *In30
058100050705      *         non c'è il commerciale azzero il campo del raggruppamento p.o.
058200050705if  4c                   If        clpage = *Zeros
058300050705     c                   Clear                   wporagru
058400050705      *         c'è il commerciale cerco il raggruppamento p.o.
058500050705   x4c                   Else
058600050705     c                   Eval      wcmm = clpage
058700050705     c                   ExSr      Sr_Cercmm
058800050705    4c                   EndIf
058900050705     c     wporagru      Lookup    skpogc                                 30
059000050705if  4c                   If        Not *In30
059100050705     c                   Eval      *In28 = *On
059200050705     c                   Eval      *In40 = *On
059300050705     c                   Eval      v1cmsg = msg(03)
059400050705     c                   Leavesr
059500050705    4c                   EndIf
059600050705    3c                   EndIf
059700050805      *         se è tariffa di cartello
059800050805   x2c                   Else
059900050805     c                   Eval      woldcar = *On
060000050705    2c                   EndIf
060100090408     c                   move      v1ksco        v2cksc
060200050706      *     decodifico il cliente
060300050706     c                   Eval      v2dksc = acorag
060400050711    1c                   EndIf
060500050708
060600050708      * COPIA DA OFFERTA
060700050727if  1c                   If        *In03
060800050708
060900050708      * --> Offerta da cui copiare
061000050708      *     obbligatoria
061100101222if  2c                   if        v1nrvot= *Zeros
061200050708     c                   Eval      *In28 = *On
061300050708     c                   Eval      *In40 = *On
061400050708     c                   Eval      v1cmsg = msg(02)
061500050712     c                   Eval      %subst(v1cmsg:11:10) = 'l''Offerta'
061600050708     c                   Leavesr
061700050708    2c                   EndIf
061800081215      *     converto se inferiore a zero
061900091118     c                   if        v1nrvot< *zeros
062000091118     c                   mllzo     1             v1nrvot
062100090915     c                   endif
062200050706      *         deve esistere
062300101222     c     v1nrvot       Chain     Tivis05l
062400090915if  2c                   If        Not %Found
062500050705     c                   Eval      *In28 = *On
062600050705     c                   Eval      *In40 = *On
062700050705     c                   Eval      v1cmsg = msg(04)
062800050705     c                   Leavesr
062900050705    2c                   EndIf
063000050705      *         offerta di cliente codificato
063100050705if  2c                   If        visksc > *Zeros
063200120112      *     cerco l'importo per la forzatura AC BASE (tab. FAB)
063300120112     c                   clear                   tibs02ds
063400120112     c                   clear                   dfab
063500120112     c                   eval      t02mod = 'C'
063600120112     c                   eval      t02sif = knsif
063700120112     c                   eval      t02cod = 'FAB'
063800120112     c                   movel     visksc        t02ke1
063900120112     c                   call      'TIBS02R'
064000120112     c                   parm                    kpjba
064100120112     c                   parm                    tibs02ds
064200120112     c                   if        t02err = *blanks
064300120112     c                   eval      dfab = t02uni
064400120112     c                   endif
064500120112     c                   eval      oldfabitra = d§fabitr
064600120112
064700050705     c                   Movel     visksc        w003a
064800050705      *         offerta di cliente non codificato
064900050705   x2c                   Else
065000050705     c                   Movel     viscmm        w003a
065100120112     c                   clear                   oldfabitra
065200050705    2c                   EndIf
065300050705      *         deve essere gestibile dall'utente
065400050705     c     w003a         Lookup    skpogc                                 30
065500050705      *         non trovo nella schiera dei p.o. gestibili
065600050705if  2c                   If        Not *In30
065700050706      *         cerco il raggruppamento p.o.
065800050705     c                   Eval      wcmm = viscmm
065900050705     c                   ExSr      Sr_Cercmm
066000050705     c     wporagru      Lookup    skpogc                                 30
066100050705if  3c                   If        Not *In30
066200050705     c                   Eval      *In28 = *On
066300050705     c                   Eval      *In40 = *On
066400050705     c                   Eval      v1cmsg = msg(05)
066500050705     c                   Leavesr
066600050705    3c                   EndIf
066700050705    2c                   EndIf
066800091118     c                   Eval      v2cnrvt = v1nrvot
066900050706      *     decodifico il cliente
067000050706     c                   Eval      v2dksc = visrag
067100060307      *     cerco il cod.importanza cliente
067200060307     c                   clear                   w_clpclv
067300101222     c                   move      v1nrvot       kksc
067400060307     c     kclp          chain     tfclp00f
067500060307if  2c                   if        not %found(tfclp00f) and
067600060307     c                             visksc > *zeros
067700060307     c                   move      visksc        kksc
067800060307     c     kclp          chain     cnclp00f
067900060307    2c                   endif
068000050705    1c                   EndIf
068100050705
068200050705      * --> Tariffa da cui copiare
068300090414      *     obbligatoria
068400090414if  1c                   If        v1ctro = *blanks
068500090414     c                   Eval      *In28 = *On
068600090414     c                   Eval      *In41 = *On
068700090414     c                   Eval      v1cmsg = msg(02)
068800090414     c                   Eval      %subst(v1cmsg:11:10) = 'la Tariffa'
068900090414     c                   Leavesr
069000090414    1c                   EndIf
069100090414      *     ricerca tariffa
069200090414     c                   if        %scan('?':v1ctro) > *zeros
069300090414     c                   clear                   parmta36
069400090414     c  n03              move      v1ksco        kksc
069500101222     c
069600101222     c   03              move      v1nrvot       kksc
069700090414     c                   exsr      sr_ricctr
069800090414     c   28              eval      *in41 = *on
069900090414     c                   eval      *in90 = *on
070000090414     c                   eval      v1ctro = ta36ctr
070100101222     c                   move      ta36prg       v1prgo
070200090414     c                   leavesr
070300090414     c                   endif
070400091021      *     deve essere numerico
070500091021     c                   if        %check(digits:v1ctro) > *zeros
070600091021     c                   eval      *in28 = *on
070700091021     c                   eval      *in41 = *on
070800091021     c                   eval      v1cmsg = msg(59)
070900091021     c                   leavesr
071000091021     c                   endif
071100090414      *     immessa
071200050711if  1c                   If        v1ctro <> *Blanks
071300050705      *     controllo che esista il codice tariffa nella tabella TR
071400050705     c                   Eval      w001a = %subst(v1ctro:1:1)
071500050705     c                   ExSr      Sr_Chkctr
071600050705if  2c                   If        *In28
071700050705     c                   Eval      *In41 = *On
071800050711     c                   Eval      v1cmsg = msg(06)
071900050705     c                   Leavesr
072000050705    2c                   EndIf
072100050705      *     --> se devo copiare su offerta
072200050817if  2c                   If        *In01 and
072300050705      *         controllo la congruenza con il codice tariffa da creare
072400160624      *     se copia delle SOLE tariffe particolari non controllo
072500160624      *     o se non sto copiando da una tariffa di tipo 4 o 5
072600160624      *     o verso una tariffa di tipo 4 o 5
072700160704     c                             ((V1Ctad <> 'N' and V1Ctpt <> 'D' and
072800160624     c                              V1Ctgc <> 'N') or
072900160624     c                             %subst(V1Ctro:1:1) = '4' or
073000160624     c                             %subst(V1Ctro:1:1) = '5' or
073100160624     c                             %subst(V1Ctrn:1:1) = '4' or
073200160704     c                             %subst(V1Ctrn:1:1) = '5') and
073300050817     c                             %subst(v1ctro:1:1) <> %subst(v1ctrn:1:1)
073400050705     c                   Eval      *In28 = *On
073500050705     c                   Eval      *In41 = *On
073600050711     c                   Eval      v1cmsg = msg(07)
073700050705     c                   Leavesr
073800050705    2c                   EndIf
073900050705      *     --> se copia da tariffa
074000050727if  2c                   If        Not *In03
074100050705      *         deve esistere almeno una tariffa
074200090408     c                   move      v1ksco        kksc
074300050711     c                   Move      v1ctro        kctr
074400050705     c     kTntam        Setgt     Tntam01l                           30
074500050711     c     kTntam        Readpe    Tntam01l                               30
074600050705do  3c                   Dow       Not *In30 and tamatb <> *Blanks
074700050711     c     kTntam        Readpe    Tntam01l                               30
074800050705    3c                   EndDo
074900050705if  3c                   If        *In30
075000050705     c                   Eval      *In28 = *On
075100050705     c                   Eval      *In41 = *On
075200050711     c                   Eval      v1cmsg = msg(08)
075300050705     c                   Leavesr
075400050705    3c                   EndIf
075500050705    2c                   EndIf
075600050705      *     --> se copia da offerta
075700050727if  2c                   If        *In03
075800050705      *         deve esistere almeno un'offerta
075900101222     c                   Eval      kksc = v1nrvot
076000050711     c                   Move      v1ctro        kctr
076100050705     c     kTntam        Setgt     Tnofm01l                           30
076200050705     c     kTntam        Readpe    Tnofm01l                               30
076300050705do  3c                   Dow       Not *In30 and tamatb <> *Blanks
076400050705     c     kTntam        Readpe    Tnofm01l                               30
076500050705    3c                   EndDo
076600050705if  3c                   If        *In30
076700050705     c                   Eval      *In28 = *On
076800050705     c                   Eval      *In41 = *On
076900050711     c                   Eval      v1cmsg = msg(10)
077000050705     c                   Leavesr
077100050705    3c                   EndIf
077200050705    2c                   EndIf
077300050711     c                   Move      v1ctro        v2cctr
077400050711     c                   Move      v1prgo        v2cprg
077500050706      *     decodifico la tariffa
077600050706     c                   Eval      v2dctr = §trdes
077700050706     c                   Eval      v2cdcv = tamdcv
077800050711      *     imposto se gli importi della tariffa sono gestiti in percentuale
077900050727     c                   Eval      *In05 = (§trtap = 'S')
078000050705    1c                   EndIf
078100050707
078200050707      * --> Progressivo tariffa da cui copiare
078300050707      *     obbligatorio
078400050707if  1c                   If        v1prgo = *Blanks
078500050707     c                   Eval      *In28 = *On
078600050711     c                   Eval      *In42 = *On
078700050711     c                   Eval      v1cmsg = msg(10)
078800050707     c                   Leavesr
078900050923      *     immesso
079000050707   x1c                   Else
079100050707      *     --> se copia da tariffa
079200050727if  2c                   If        Not *In03
079300050707      *         deve esistere la tariffa
079400090408     c                   move      v1ksco        kksc
079500050711     c                   Move      v1ctro        kctr
079600050711     c                   Move      v1prgo        kprg
079700050711     c     kTntam01      Chain     Tntam01l                           30
079800050707do  3c                   Dow       Not *In30 and tamatb <> *Blanks
079900050711     c     kTntam01      Reade     Tntam01l                               30
080000050707    3c                   EndDo
080100050707if  3c                   If        *In30
080200050707     c                   Eval      *In28 = *On
080300050707     c                   Eval      *In42 = *On
080400050711     c                   Eval      v1cmsg = msg(08)
080500050707     c                   Leavesr
080600050707    3c                   EndIf
080700050707    2c                   EndIf
080800050707      *     --> se copia da offerta
080900050727if  2c                   If        *In03
081000050707      *         deve esistere l'offerta
081100101222     c                   Eval      kksc = v1nrvot
081200050711     c                   Move      v1ctro        kctr
081300050711     c                   Move      v1prgo        kprg
081400050711     c     kTntam01      Chain     Tnofm01l                           30
081500050707do  3c                   Dow       Not *In30 and tamatb <> *Blanks
081600050711     c     kTntam01      Reade     Tnofm01l                               30
081700050707    3c                   EndDo
081800050707if  3c                   If        *In30
081900050707     c                   Eval      *In28 = *On
082000050707     c                   Eval      *In42 = *On
082100050711     c                   Eval      v1cmsg = msg(09)
082200050707     c                   Leavesr
082300050707    3c                   EndIf
082400050707    2c                   EndIf
082500050707    1c                   EndIf
082600050912      * Se il tipo tariffa è diverso dal tipo tariffa precedente pulisco
082700050912      * il campo e spengo l'indicatore
082800050912     c                   If        tamfie <> wfie
082900050912     c                   Clear                   v1cfie
083000050912     c                   Eval      *In19 = *Off
083100050912     c                   EndIf
083200050706
083300050706      * Salvo il tipo tariffa I/E della tariffa/offerta da cui copiare
083400050706     c                   Eval      wfie = tamfie
083500050706      * Salvo i netwrok della tariffa/offerta da cui copiare
083600050706     c                   Eval      dsta01 = tamflo
083700050706     c                   Eval      wdpd = §tadpd
083800050706     c                   Eval      wfed = §tafed
083900050707      * Salvo l'applicazione tariffa della tariffa/offerta da cui copiare
084000050707     c                   Eval      wmat = §taf02
084100050706      * Imposto la divisa della tariffa/offerta da cui copiare
084200050706     c                   Eval      v2cdiv = §tadiv
084300050708      * Salvo la data scadenza della tariffa/offerta da cui copiare
084400050708     c                   Eval      wdst = tamdst
084500050708      * Salvo il tipo servizio della tariffa/offerta da cui copiare
084600050708     c                   Eval      wtsp = tamtsp
084700050708     c                   ExSr      Sr_Chktsp
084800050706
084900050708      * Se non è copia su offerta
085000050727if  1c                   If        Not *In01
085100050707
085200050706      * --> Cliente a cui copiare
085300050706      *     obbligatorio
085400050706if  2c                   If        v1kscn = *Zeros
085500090408     c                             or v1kscn = *blanks
085600050706     c                   Eval      *In28 = *On
085700050707     c                   Eval      *In43 = *On
085800050711     c                   Eval      v1cmsg = msg(11)
085900050706     c                   Leavesr
086000050706    2c                   EndIf
086100090408      * ricerca cliente
086200090408     c                   if        %scan('?':v1kscn) > *zeros
086300090415     c                   clear                   xclirds
086400090415     c                   eval      dxcaut = 'AZ'
086500090408     c                   exsr      sr_ricksc
086600090415     c                   move      ksc(1)        v1kscn
086700090408     c                   eval      *in90 = *on
086800090408     c                   leavesr
086900090408     c                   endif
087000091021      *     deve essere numerico
087100091021     c                   if        %check(digits:v1kscn) > *zeros
087200091021     c                   eval      *in28 = *on
087300091021     c                   eval      *in43 = *on
087400091021     c                   eval      v1cmsg = msg(59)
087500091021     c                   leavesr
087600091021     c                   endif
087700090414
087800090414     c                   if        v1kscn <> oldkscn
087900090414     c                   eval      wokkscn = *off
088000090414     c                   eval      oldkscn = v1kscn
088100090414     c                   endif
088200090415
088300050706      *     deve esistere
088400050708     c                   Move      v1kscn        w0070
088500050706     c                   ExSr      Sr_Chkcli
088600050706if  2c                   If        O69err = *On
088700050706     c                   Eval      *In28 = *On
088800050707     c                   Eval      *In43 = *On
088900050706     c                   Eval      v1cmsg = o69msg
089000050706     c                   Leavesr
089100050706    2c                   EndIf
089200090408     c                   eval      v1dkscn = acorag
089300050714      *     se non autorizzato non posso copiare verso una tariffa di cartello
089400050714     c                   Movel     v1kscn        w005a
089500050922if  2c                   If        w005a = '88888' and §utecar <> 'S'
089600050714     c                   Eval      *In28 = *On
089700050714     c                   Eval      *In43 = *On
089800050714     c                   Eval      v1cmsg = msg(12)
089900050714     c                   Leavesr
090000050714    2c                   EndIf
090100050922      *     se non sto copiando da cartello non posso andare verso una cartello
090200050922if  2c                   If        w005a = '88888' and woldcar = *Off
090300050922     c                   Eval      *In28 = *On
090400050922     c                   Eval      *In43 = *On
090500050922     c                   Eval      v1cmsg = msg(52)
090600050922     c                   Leavesr
090700050922    2c                   EndIf
090800050712      *     deve essere gestibile dall'utente
090900050712     c                   Movel     v1kscn        w003a
091000050715     c                   Movel     v1kscn        w005a
091100050712      *     se non è tariffa di cartello
091200050715if  2c                   If        w005a <> '88888'
091300050923     c                   Eval      wnewcar = *Off
091400050712     c     w003a         Lookup    skpog                                  30
091500050712      *     non trovo nella schiera dei p.o. gestibili
091600050712if  3c                   If        Not *In30
091700050712      *     non c'è il commerciale azzero il campo del raggruppamento p.o.
091800050712if  4c                   If        clpage = *Zeros
091900050712     c                   Clear                   wporagru
092000050712      *     c'è il commerciale cerco il raggruppamento p.o.
092100050712   x4c                   Else
092200050712     c                   Eval      wcmm = clpage
092300050712     c                   ExSr      Sr_Cercmm
092400050712    4c                   EndIf
092500050712     c     wporagru      Lookup    skpog                                  30
092600090423      * se non gestibile dall'utente
092700090423      * e utente non autorizzato --> errore
092800090423if  4c                   If        Not *In30 and §utedupcli <> 'S'
092900090423     c                   Eval      *In28 = *On
093000090423     c                   Eval      *In43 = *On
093100090423     c                   Eval      v1cmsg = msg(03)
093200090423     c                   Leavesr
093300090423    4c                   EndIf
093400090414if  4c                   If        Not *In30 and wokkscn = *off
093500090414      * se non gestibile dall'utente window di avviso con richiesta
093600090414      * di proseguire
093700090423      * e utente autorizzato
093800090423if  4c                             and §utedupcli = 'S'
093900090414      * se proseguo posso solo andare su una nuova tariffa
094000090414      * non posso integrare
094100090414     c                   exsr      sr_geswin
094200090414     c   90              leavesr
094300050712    4c                   EndIf
094400050712    3c                   EndIf
094500050805     c                   Movel     v1kscn        comlnp
094600050909      *     copio verso una nuova tariffa di cartello
094700050715   x2c                   Else
094800050715     c                   Eval      wnewcar = *On
094900050712    2c                   EndIf
095000060307      *     salvo il cod.importanza cliente
095100060307     c                   eval      savclva = clpclv
095200060705      *     cerco l'importo per la forzatura AC BASE (tab. FAB)
095300060705     c                   clear                   tibs02ds
095400060705     c                   clear                   dfab
095500060705     c                   eval      t02mod = 'C'
095600060705     c                   eval      t02sif = knsif
095700060705     c                   eval      t02cod = 'FAB'
095800060705     c                   movel     v1kscn        t02ke1
095900060705     c                   call      'TIBS02R'
096000060705     c                   parm                    kpjba
096100060705     c                   parm                    tibs02ds
096200060705     c                   if        t02err = *blanks
096300060705     c                   eval      dfab = t02uni
096400060705     c                   endif
096500060705     c                   eval      savfabitra = d§fabitr
096600050708      *     recupero il network cliente
096700050708     c                   Movel     v1kscn        w0030
096800050708     c                   Clear                   og143
096900050708     c     w0030         Chain     Azorg01l
097000050708     c                   If        %Found(Azorg01l)
097100050708     c                   Eval      og143 = orgde3
097200050708     c                   EndIf
097300050708     c                   Eval      ntwksc = §ogntw
097400050708      *     controllo il tipo servizio
097500050708      *     tariffa da cui copiare con tipo servizio poste
097600050708      *     il cliente di arrivo deve essere un cliente poste
097700050708if  2c                   If        §5euso = 'P' and ntwksc <> 'PPT'
097800050708     c                   Eval      *In28 = *On
097900050708     c                   Eval      *In43 = *On
098000050714     c                   Eval      v1cmsg = msg(13)
098100050727     c  n03              Eval      %subst(v1cmsg:1:7) = 'Tariffa'
098200050727     c   03              Eval      %subst(v1cmsg:1:7) = 'Offerta'
098300050708     c                   Leavesr
098400050708    2c                   EndIf
098500050708      *     tariffa da cui copiare con tipo servizio NON poste
098600050708      *     il cliente di arrivo NON deve essere un cliente poste
098700050708if  2c                   If        §5euso = 'E' and ntwksc = 'PPT'
098800050708     c                   Eval      *In28 = *On
098900050708     c                   Eval      *In43 = *On
099000050714     c                   Eval      v1cmsg = msg(14)
099100050727     c  n03              Eval      %subst(v1cmsg:1:7) = 'Tariffa'
099200050727     c   03              Eval      %subst(v1cmsg:1:7) = 'Offerta'
099300050708     c                   Leavesr
099400050708    2c                   EndIf
099500050913      * Se è copia su offerta
099600050913   x1c                   Else
099700120112     c                   eval      *in54 = *off
099800081215      *     converto se inferiore a zero
099900091118     c                   if        v1nrvnt < *zeros
100000091118     c                   mllzo     1             v1nrvnt
100100091118     c                   endif
100200101222     c     v1nrvnt       Chain     Tivis05l
100300090916if  2c                   If        %Found
100400120112      *         verifico se tarttativa di tipo "N" = NUOVA
100500120112     c                   If        vistpv = 'N'
100600120112     c                   eval      *in54 = *on
100700120112     c                   Endif
100800050913      *         offerta di cliente codificato
100900050913if  3c                   If        visksc > *Zeros
101000050913     c                   Movel     visksc        comlnp
101100060705      *         cerco l'importo per la forzatura AC BASE (tab. FAB)
101200060705     c                   clear                   tibs02ds
101300060705     c                   clear                   dfab
101400060705     c                   eval      t02mod = 'C'
101500060705     c                   eval      t02sif = knsif
101600060705     c                   eval      t02cod = 'FAB'
101700060705     c                   movel     visksc        t02ke1
101800060705     c                   call      'TIBS02R'
101900060705     c                   parm                    kpjba
102000060705     c                   parm                    tibs02ds
102100060705     c                   if        t02err = *blanks
102200060705     c                   eval      dfab = t02uni
102300060705     c                   endif
102400060705     c                   eval      savfabitra = d§fabitr
102500050913      *         offerta di cliente non codificato
102600050913   x3c                   Else
102700050913     c                   Movel     viscmm        comlnp
102800060705     c                   clear                   savfabitra
102900050913    3c                   EndIf
103000060307      *     cerco il cod.importanza cliente
103100060307     c                   clear                   w_clpclv
103200101222     c                   move      v1nrvnt       kksc
103300060307     c     kclp          chain     tfclp00f
103400060307if  3c                   if        not %found(tfclp00f) and
103500060307     c                             visksc > *zeros
103600060307     c                   move      visksc        kksc
103700060307     c     kclp          chain     cnclp00f
103800060307    3c                   endif
103900060307      *     salvo il cod.importanza cliente
104000060307     c                   eval      savclva = w_clpclv
104100050913    2c                   EndIf
104200050913    1c                   EndIf
104300050707
104400050706      * --> Tariffa a cui copiare
104500050706      *     obbligatoria
104600050913if  1c                   If        v1ctrn = *Blanks
104700050706     c                   Eval      *In28 = *On
104800050707     c                   Eval      *In44 = *On
104900050714     c                   Eval      v1cmsg = msg(15)
105000050706     c                   Leavesr
105100050913    1c                   EndIf
105200090414      *     ricerca tariffa
105300090414     c                   if        %scan('?':v1ctrn) > *zeros
105400090414     c                   clear                   parmta36
105500090414     c  n03              move      v1kscn        kksc
105600101222     c   03              move      v1nrvnt       kksc
105700090414     c                   exsr      sr_ricctr
105800090414     c   28              eval      *in43 = *on
105900090414     c                   eval      *in90 = *on
106000090414     c                   eval      v1ctrn = ta36ctr
106100101222     c                   move      ta36prg       v1prgn
106200090414     c                   leavesr
106300090414     c                   endif
106400091021      *     deve essere numerico
106500091021     c                   if        %check(digits:v1ctrn) > *zeros
106600091021     c                   eval      *in28 = *on
106700091021     c                   eval      *in44 = *on
106800091021     c                   eval      v1cmsg = msg(59)
106900091021     c                   leavesr
107000091021     c                   endif
107100050706      *     congruente con la tariffa da cui copiare
107200160517      *     se copia delle SOLE tariffe particolari non controllo
107300160519      *     o se non sto copiando da una tariffa di tipo 4 o 5
107400160519      *     o verso una tariffa di tipo 4 o 5
107500160519     c                   IF        (V1Ctad <> 'N' and V1Ctpt <> 'D' and
107600160519     c                              V1Ctgc <> 'N') or
107700160519     c                             %subst(V1Ctro:1:1) = '4' or
107800160519     c                             %subst(V1Ctro:1:1) = '5' or
107900160519     c                             %subst(V1Ctrn:1:1) = '4' or
108000160519     c                             %subst(V1Ctrn:1:1) = '5'
108100050913if  1c                   If        %subst(v1ctro:1:1) <> %subst(v1ctrn:1:1)
108200050706     c                   Eval      *In28 = *On
108300050707     c                   Eval      *In44 = *On
108400050714     c                   Eval      v1cmsg = msg(16)
108500050706     c                   Leavesr
108600050913    1c                   EndIf
108700160517     c                   ENDIF
108800050913      *     Se sto copiando da una tariffa/Offerta Fedex
108900050913      *     deve essere congruente anche il range per distinguere
109000050913      *     se tariffa Merci o Documenti
109100050913if  1c                   If        wfed = 'S'
109200050912     c                   Move      v1ctro        wctro
109300050912     c                   Move      v1ctrn        wctrn
109400050912     c                   Select
109500050912     c                   When      wctro >= §dfedalm and wctro <= §dfealm and
109600050912     c                             wctrn >= §dfedalm and wctrn <= §dfealm
109700050912     c                   When      wctro >= §dfedald and wctro <= §dfeald and
109800050912     c                             wctrn >= §dfedald and wctrn <= §dfeald
109900050912     c                   Other
110000160517      *     se copia delle SOLE tariffe particolari non controllo
110100160517     c                   IF        V1Ctad <> 'N' and V1Ctpt <> 'D' and
110200160517     c                             V1Ctgc <> 'N'
110300050912     c                   Eval      *In28 = *On
110400050912     c                   Eval      *In44 = *On
110500050912     c                   Eval      v1cmsg = msg(16)
110600050912     c                   Leavesr
110700160517     c                   ENDIF
110800050912     c                   EndSl
110900050913    1c                   EndIf
111000050706
111100050706      * --> Progressivo a cui copiare
111200090414      * non posso andare in aggiunta ad una tariffa esistente se il cliente
111300090414      * non è di mia gestione
111400090414     c                   if        v1prgn <> *blanks and wokkscn
111500090414     c                   eval      *in28 = *on
111600090414     c                   eval      *in45 = *on
111700090414     c                   eval      v1cmsg = msg(58)
111800090414     c                   leavesr
111900090414     c                   endif
112000090414      *     se immesso vuol dire che voglio implementare una tariffa/offerta
112100090414      *     già esistente quindi controllo che effettivamente esista
112200090414      *     la tariffa/offerta
112300050706if  1c                   If        v1prgn <> *Blanks
112400050727     c                   Eval      *In04 = *Off
112500090408     c  n01              move      v1kscn        kksc
112600101222     c   01              Eval      kksc = v1nrvnt
112700050711     c                   Move      v1ctrn        kctr
112800050711     c                   Move      v1prgn        kprg
112900050923     c  n01kTntam01      Setll     Tntam01l
113000050923     c   01kTntam01      Setll     Tnofm01l
113100050923if  2c                   If        Not %Equal
113200050706     c                   Eval      *In28 = *On
113300050707     c                   Eval      *In45 = *On
113400050714     c                   Eval      v1cmsg = msg(17)
113500050706     c                   Leavesr
113600050706    2c                   EndIf
113700050706do  2c                   Do        *Hival
113800050923     c  n01kTntam01      Reade     Tntam01l
113900050923     c   01kTntam01      Reade     Tnofm01l
114000050923     c                   If        %Eof
114100050706     c                   Leave
114200050706     c                   EndIf
114300050706     c                   If        tamatb <> *Blanks
114400050706     c                   Iter
114500050706     c                   EndIf
114600050727     c                   Eval      *In04 = *On
114700050706     c                   Eval      dsta01 = tamflo
114800050706    2c                   EndDo
114900050727if  2c                   If        Not *In04
115000050706     c                   Eval      *In28 = *On
115100050707     c                   Eval      *In45 = *On
115200050714     c                   Eval      v1cmsg = msg(17)
115300050706     c                   Leavesr
115400050706    2c                   EndIf
115500050706      *     I/E congruente con la tariffa da cui copiare
115600050707if  2c                   If        wfie <> tamfie
115700050805      *     se non sto creando una nuova tariffa di cartello
115800050805      *     e se non sto copiando da una tariffa di cartello in questo caso
115900050805      *     solo 8888830 xchè è l'unica che non ha il tamfie impostato
116000050805     c                             and wnewcar = *Off
116100050805     c                             and (woldcar = *On and wfie <> *Blanks)
116200050706     c                   Eval      *In28 = *On
116300050707     c                   Eval      *In45 = *On
116400050714     c                   Eval      v1cmsg = msg(18)
116500050711     c                   Eval      %subst(v1cmsg:14:1) = tamfie
116600050706     c                   Leavesr
116700050706    2c                   EndIf
116800050706      *     network incongruente con la tariffa da cui copiare
116900160519if  2c                   If        (wdpd = 'S' or §tadpd = 'S')
117000050706     c                              and wdpd <> §tadpd
117100050706     c                   Eval      *In28 = *On
117200050707     c                   Eval      *In45 = *On
117300050714     c                   Eval      v1cmsg = msg(19)
117400050707     c                   Eval      %subst(v1cmsg:9:3) = 'DPD'
117500050706     c                   Leavesr
117600050706    2c                   EndIf
117700050706if  2c                   If        (wfed = 'S' or §tafed = 'S')
117800050706     c                              and wfed <> §tafed
117900050706     c                   Eval      *In28 = *On
118000050707     c                   Eval      *In45 = *On
118100050714     c                   Eval      v1cmsg = msg(19)
118200050707     c                   Eval      %subst(v1cmsg:9:5) = 'FedEx'
118300050707     c                   Leavesr
118400050706    2c                   EndIf
118500050707      *     modo applicazione tariffa incongruente con la tariffa da cui copiare
118600160519      *     lo controllo solo se ho chiesto la copia del dettaglio tariffario
118700050707if  2c                   If        wmat <> §taf02
118800160519     c                             and V1Ctad <> 'N'
118900050707     c                   Eval      *In28 = *On
119000050707     c                   Eval      *In45 = *On
119100050714     c                   Eval      v1cmsg = msg(20)
119200050707     c                   Eval      %subst(v1cmsg:27:1) = §taf02
119300050707     c                   Leavesr
119400050707    2c                   EndIf
119500060307      *     salvo il flag di rinuncia AC BASE della tariffa a cui copiare
119600060307     c                   eval      savfmp = tamfmp
119700050706    1c                   EndIf
119800050912
119900050912      * Se è copia verso tariffa esistente proteggo il campo v1cfie
120000050912     c   04              Eval      *In19 = *On
120100050912      * Se non è copia da tariffa di cartello proteggo il campo v1cfie
120200050912     c                   If        woldcar = *Off
120300050912     c                   Eval      *In19 = *On
120400050912     c                   EndIf
120500050912
120600050912      * Se vuoto imposto il campo v1cfie
120700050912     c                   If        v1cfie = *Blanks
120800050912      * Tariffa esistente imposto il tipo tariffa I/E della tariffa
120900050912      * a cui copiare
121000050912     c   04              Eval      v1cfie = tamfie
121100050912      * Tariffa nuova imposto il tipo tariffa I/E dalla tariffa da cui
121200050912      * copiare
121300050912     c  n04              Eval      v1cfie = wfie
121400050912     c                   EndIf
121500060130
121600060130      * Msg info per avvisare che si sta copiando da una tariffa/offerta
121700060130      * dove sono presenti delle abilitazioni particolari
121800060130      * se sto copiando verso una nuova tariffa/offerta
121900060130    1c                   If        (tamgca <> *Blanks or tamgga <> *Blanks or
122000060130     c                              tamgma <> *Blanks or tamgva <> *Blanks) and
122100060130     c                              wforzapar = *Off and Not *In04
122200060130     c                   Eval      *In28 = *On
122300060130     c                   Eval      *In40 = *On
122400060130     c                   Eval      v1cmsg = msg(53)
122500060130     c                   Eval      wforzapar = *On
122600060130     c                   Leavesr
122700060130    1c                   EndIf
122800060130
122900150219      /free
123000150219       //?Se sto andando verso un nuovo progressivo
123100150219         IF  not *in04;
123200150219         //?Se sto copiando verso un'offerta
123300150219         //?devo tener buona come date le data decorrenza
123400150219         //?della tariffa/offerta dalla quale sto copiando
123500150219         //?sto copiando
123600150219           IF  *in01;
123700150219             wData = TAMddt;
123800150219           ENDIF;
123900150219         //?Se sto copiando verso una tariffa
124000150219         //?devo prendere la data scadenza dell'ultimo progressivo presente
124100150219           IF  not *in01;
124200150219             sav_TAMksc = %dec(V1kscn:7:0);
124300150219             sav_TAMctr = %dec(V1ctrn:3:0);
124400150219             chain (sav_TAMksc:sav_TAMctr) TNTAM04L;
124500150219             IF  %found(TNTAM04L);
124600150219               wData = w_TAMdst;
124700150219             ELSE;
124800150219               wData = TAMdst;
124900150219             ENDIF;
125000150219         //?a questo punto calcolo la nuova data decorrenza tariffa
125100150219             dataymd = %date(wData) + %days(1);
125200150219             wData = %dec(dataymd);
125300150219           ENDIF;
125400150219         ENDIF;
125500150219       //?Se sto andando verso un progressivo già esistente
125600150219         IF  *in04;
125700150219           wData = TAMddt;
125800150219         ENDIF;
125900150219       //?Controllo se il cliente al quale sto creando la tariffa/offerta
126000150219       //?è presente in tabella VMA e la decorrenza della nuova
126100150219       //?tariffa/offerta è compresa nel periodo in cui il VMA va negato
126200150219         *in35 = *off;
126300150219         clear TIBS02DS;
126400150219         clear dVMA;
126500150219         T02mod = 'C';
126600150219         T02cod = 'VMA';
126700150219       //?copia verso tariffa
126800150219         IF  not *in01;
126900150219           T02ke1 = V1kscn;
127000150219         ENDIF;
127100150219       //?copia verso offerta di cliente codificato
127200150219         IF  *in01;
127300150219           T02ke1 = %editc(VISksc:'X');
127400150219         ENDIF;
127500150219         T02sif = KNSIF;
127600150219         TNTBE_RicercaControllo  (kpjba : tibs02ds);
127700150219         IF  T02err = *blanks;
127800150219           dVMA = T02uni;
127900150219           IF  wData >= §VMAddt and
128000150219               wData <= §VMAdst;
128100150219             *in35 = *on;
128200150219           ENDIF;
128300150219         ENDIF;
128400150219      /end-free
128500050909
128600050706      * --> Almeno una scelta deve essere fatta
128700050706     c                   If        v1ctad = 'N' and v1ctpt = 'N' and
128800050908     c                             v1ctgc = 'N'
128900050706     c                   Eval      *In28 = *On
129000050707     c                   Eval      *In46 = *On
129100050714     c                   Eval      v1cmsg = msg(21)
129200050706     c                   Leavesr
129300050706     c                   EndIf
129400050801
129500050801      * --> Dati tipo
129600050801      *     errore se richiesta la copia ma non è copia totale
129700050801      *     della tariffa/offerta
129800050801if  1c                   If         v1ccat = 'S' and
129900051005     c                              (v1ctad <> 'T' or  v1ctpt <> 'T' or
130000051005     c                               v1ctgc <> 'T')
130100050801     c                   Eval      *In28 = *On
130200050909     c                   Eval      *In49 = *On
130300050801     c                   Eval      v1cmsg = msg(29)
130400050801     c                   Leavesr
130500050801    1c                   EndIf
130600050920      *     errore se richiesta la copia e sto copiando da una tariffa
130700050920      *     di cartello
130800050920if  1c                   If         v1ccat = 'S' and woldcar = *On
130900050920     c                   Eval      *In28 = *On
131000050920     c                   Eval      *In49 = *On
131100050920     c                   Eval      v1cmsg = msg(49)
131200050920     c                   Leavesr
131300050920    1c                   EndIf
131400050711
131500050728      * --> Se copia dati tipo devono esistere
131600050801if  1c                   If        v1ccat = 'S'
131700050801      *     se copia da tariffa
131800050801if  2c                   If        Not *In03
131900050801     c                   Eval      kcto = 'T'
132000090408     c                   move      v1ksco        kksc
132100050801      *     se copia da offerta
132200050801   x2c                   Else
132300050801     c                   Eval      kcto = 'O'
132400101222     c                   Eval      kksc = v1nrvot
132500050801    2c                   EndIf
132600061009     c                   Eval      kdsf = 'S'
132700050728     c                   Move      v1ctro        kctr
132800050728     c                   Move      v1prgo        kprg
132900050728     c     kTeetc        Chain     Teetc01l
133000050728     c                   If        Not %Found(Teetc01l)
133100050711     c                   Eval      *In28 = *On
133200050909     c                   Eval      *In49 = *On
133300050714     c                   Eval      v1cmsg = msg(23)
133400050711     c                   Leavesr
133500050711     c                   EndIf
133600050801    1c                   EndIf
133700050909
133800050909      * --> Tipo tariffa I/E
133900050909      *     obbligatorio
134000050909if  1c                   If        v1cfie = *Blanks
134100050909      *     se non sto creando una nuova tariffa di cartello
134200050909      *     e se non sto copiando da una tariffa di cartello in questo caso
134300050909      *     solo 8888830 xchè è l'unica che non ha il tamfie impostato
134400050909     c                             and  wnewcar = *Off
134500050909     c                             and (woldcar = *On and wfie = *Blanks)
134600050909     c                   Eval      *In28 = *On
134700050909     c                   Eval      *In50 = *On
134800050909     c                   Eval      v1cmsg = msg(30)
134900050909     c                   Leavesr
135000050909    1c                   EndIf
135100050909      *     I/E congruente con la tariffa da cui copiare
135200050909      *     controllo anche se wfie <> ' ' xchè nella cartello 8888830
135300050909      *     il tamfie non è impostato ed è l'unico caso in cui non è da
135400050909      *     impostare
135500050922if  1c                   If        wfie <> v1cfie and wfie <> *Blanks
135600050909     c                   Eval      *In28 = *On
135700050909     c                   Eval      *In50 = *On
135800050909     c                   Eval      v1cmsg = msg(18)
135900050909     c                   Eval      %subst(v1cmsg:14:1) = v1cfie
136000050909     c                   Leavesr
136100050922    1c                   EndIf
136200160225      *
136300160211      /free
136400160225
136500160225         // -?SE copia in Tariffa (NOT *in01)?
136600160225         //   ?o copia in Offerta (*in01) per una trattativa di un?
136700160225         //   ?cliente NON nuovo  (NOT *in54)?
136800160225         //*IF  %parms() = 1  or  VIStpv <> 'N';
136900160225         IF  NOT *in01  or  Not *in54;
137000160225
137100160225           clear  TNTAT1ds;
137200160225
137300160225           // -?SE si sta tentando di copiare su una NUOVA TAR./OFF.?
137400160225           //  ?a fronte di una TRATTATIVA su cliente già ESISTENTE?
137500160225           //  ?(Tipo Trattativa diversa da "N"):?
137600160225           //  ?Controllo che all'interno della stessa Tar./Off. NON?
137700160225           //  ?ci siano già progressivi con tipologia differente da?
137800160225           //  ?quello della Tariffa/Offerta che sto copiando?
137900160225           Select;
138000160225             //  ?SE *pgm richiamato => è per copia in Offerta?
138100160225             //  ?(e NON deve essere per una NUOVA trattativa)?
138200160225             //*When  %parms() > 1  and  VIStpv <> 'N';
138300160225             When  *in01  and  Not *in54;
138400160225               iTAT1ntc  = VISnrv;
138500160225               iTAT1ksc  = VISksc;
138600160225             //  ?Altrimenti => è per copia in Tariffa?
138700160304             //*When  %parms() = 1;
138800160225             When  NOT *in01;
138900160225               iTAT1ksc  = %int( V1kscN );
139000160225           EndSl;
139100160225
139200160225           iTAT1tipo = *blank;
139300160225           iTAT1ctr  = %int( V1Ctrn );
139400160304           iTAT1ntw = V1Cfie;
139500160304           // -?SE sto copiando in un nuovo progressivo di una TARIFFA?
139600160304           //  ?già esistente, controllo che l'ultimo progressivo di?
139700160304           //  ?questa tariffa già esistente abbia lo stesso Network?
139800160304           //  ?della tariffa che sto copiando?
139900160304           If  *in01  = *off  and  V1prgN = *blank;
140000160304             select;
140100160304               when  wDPD = 'S';
140200160304                 iTAT1ntw = 'DPD';
140300160304               when  wFED = 'S';
140400160304                 iTAT1ntw = 'FED';
140500160304               when  V1Cfie = 'E';
140600160304                 iTAT1ntw = 'EEX';
140700160304               other;
140800160304                 clear  iTAT1ntw;
140900160304             endsl;
141000160304           EndIf;
141100160225
141200160225           KPJBU    = TNTAT1ds;
141300160225           tntaT1r ( kpjba );
141400160225           TNTAT1ds = KPJBU;
141500160225
141600160225           if  oTAT1err = *on;
141700160304             //*if  %parms() > 1;
141800160304             if  *in01;        //?(copia offerta)?
141900160304               *in41  = *on;
142000160304               *in54  = *on;
142100160304             else;             //?(copia tariffa)?
142200160304               *in44  = *on;
142300160304             endif;
142400160225             *in28  = *on;
142500160225             V1Cmsg = oTAT1msg;
142600160225             leavesr;
142700160225           endif;
142800160225
142900160225         ENDIF;
143000160225
143100160211      /end-free
143200160225      *
143300050909      * --> Linee di partenza
143400050909      *     errore se non è stata immessa nessuna linea di partenza
143500050920     c                   Eval      wlnp = v1clnp1 + v1clnp2 +
143600050920     c                             v1clnp3 + v1clnp4 + v1clnp5
143700050909if  1c                   If        v1cio1 = 'I' and wlnp = *Zeros
143800050909     c                   Eval      *In28 = *On
143900050909     c                   Eval      *In51 = *On
144000050909     c                   Eval      v1cmsg = msg(31)
144100050909     c                   Leavesr
144200050909    1c                   EndIf
144300050922      *     Copia da tariffa di cartello verso tariffa/offerta
144400050922      *     non di cartello
144500050922      *     richiedo obbligatoria l'inclusione di almeno una lnp
144600050922if  1c                   If        woldcar = *On and wnewcar = *Off and
144700050922     c                             v1cio1 <> 'I'
144800050922     c                   Eval      *In28 = *On
144900050922     c                   Eval      *In51 = *On
145000050922     c                   Eval      v1cmsg = msg(50)
145100050922     c                   Leavesr
145200050922    1c                   EndIf
145300050909      *     errore se linea di partenza non valida
145400050909do  1c                   Do        5             xx
145500050909if  2c                   If        sklnp(xx) <> *Zeros
145600050921     c     sklnp(xx)     Chain     Azorg01l
145700050909if  3c                   If        Not %Found(Azorg01l) or
145800050909     c                             orgfva <> *Blanks or
145900050909     c                             (orgfag <> 'A' and orgfag <> 'F' and
146000050909     c                              orgfl2 <> 'S')
146100050909     c                   Eval      *In28 = *On
146200050909     c                   Eval      *In51 = *On
146300050909     c                   Eval      v1cmsg = msg(32)
146400050921     c                   Eval      %subst(v1cmsg:19:3) = %editc(sklnp(xx):'1')
146500050909     c                   Leavesr
146600050909    3c                   EndIf
146700050909    2c                   EndIf
146800050909    1c                   EndDo
146900050923      *     Copia da tariffa di cartello verso tariffa/offerta
147000050923      *     non di cartello
147100050923      *     se non richiesta la lnp del cliente msg. info
147200050923if  1c                   If        woldcar = *On and wnewcar = *Off and
147300050923     c                             wforzalnp = *Off
147400050923     c     comlnp        Lookup    sklnp                                  30
147500050923     c                   If        Not *In30
147600050923     c                   Eval      *In28 = *On
147700050923     c                   Eval      *In51 = *On
147800050923     c                   Eval      v1cmsg = msg(51)
147900050923     c                   Eval      wforzalnp = *On
148000050923     c                   Leavesr
148100050923     c                   EndIf
148200050923    1c                   EndIf
148300050909     c                   If        v1cio1 = 'O' and wlnp > *Zeros
148400050909     c                   Eval      wslnp = *On
148500050909     c                   EndIf
148600050909
148700050909      * --> Codici di tassazione
148800050909      *     errore se non inserito nessun codice di tassazione
148900050909     c                   Movea     skcts         wcts
149000050909if  1c                   If        v1cio2 = 'I' and wcts = *Blanks
149100050909     c                   Eval      *In28 = *On
149200050909     c                   Eval      *In52 = *On
149300050909     c                   Eval      v1cmsg = msg(33)
149400050909     c                   Leavesr
149500050909    1c                   EndIf
149600050909      *     errore se codice di tassazione non valido
149700050909do  1c                   Do        7             xx
149800050909if  2c                   If        skcts(xx) <> *Blanks
149900050909     c                   Eval      w002a = skcts(xx)
150000050909     c                   ExSr      Sr_Chkcts
150100120712     c                   eval      skcts(xx) = w002a
150200120723     c                   IF        skcts(xx) = *blanks
150300120723     c                   leavesr
150400120723     c                   ENDIF
150500050909if  3c                   If        *In28
150600050909     c                   Eval      *In52 = *On
150700050909     c                   Eval      v1cmsg = msg(34)
150800050909     c                   Eval      %subst(v1cmsg:19:2) = skcts(xx)
150900050909     c                   Leavesr
151000050909    3c                   EndIf
151100050909      *     errore se codice tassazione non utilizzabile in tariffa
151200050909if  3c                   If        §ctuta = 'N'
151300050909     c                   Eval      *In28 = *On
151400050909     c                   Eval      *In52 = *On
151500050909     c                   Eval      v1cmsg = msg(34)
151600050909     c                   Eval      %subst(v1cmsg:19:2) = skcts(xx)
151700050909     c                   Leavesr
151800050909    3c                   EndIf
151900050909      *     errore se tariffa FedEx e codice tassazione non utlizzabile x FedEx
152000050909if   c                   If        wfed = 'S' and §ctutf <> 'S'
152100050909     c                   Eval      *In28 = *On
152200050909     c                   Eval      *In52 = *On
152300050909     c                   Eval      v1cmsg = msg(35)
152400050909     c                   Eval      %subst(v1cmsg:19:2) = skcts(xx)
152500050909     c                   Leavesr
152600050909    3c                   EndIf
152700050909      *     errore se tariffa no FedEx e codice tassazione utilizzabile x FedEx
152800050909if  3c                   If        wfed <> 'S' and §ctutf = 'S'
152900050909     c                   Eval      *In28 = *On
153000050909     c                   Eval      *In52 = *On
153100050909     c                   Eval      v1cmsg = msg(36)
153200050909     c                   Eval      %subst(v1cmsg:19:2) = skcts(xx)
153300050909     c                   Leavesr
153400050909    3c                   EndIf
153500050909      *     errore se tariffa Estera e codice di tassazione non estero
153600050909if  3c                   If        wfie = 'E' and §ctest = *Blanks
153700050909     c                   Eval      *In28 = *On
153800050909     c                   Eval      *In52 = *On
153900050909     c                   Eval      v1cmsg = msg(37)
154000050909     c                   Eval      %subst(v1cmsg:19:2) = skcts(xx)
154100050909     c                   Leavesr
154200050909    3c                   EndIf
154300050909      *     errore se tariffa DPD e codice di tassazione non estero
154400050909      *     escluso IT che per le tariffe DPD è valido
154500050909if  3c                   If        wdpd = 'S' and §ctest = *Blanks and
154600050909     c                             skcts(xx) <> 'IT'
154700050909     c                   Eval      *In28 = *On
154800050909     c                   Eval      *In52 = *On
154900050909     c                   Eval      v1cmsg = msg(37)
155000050909     c                   Eval      %subst(v1cmsg:19:2) = skcts(xx)
155100050909     c                   Leavesr
155200050909    3c                   EndIf
155300050909    2c                   EndIf
155400050909    1c                   EndDo
155500050909     c                   If        v1cio2 = 'O' and wcts <> *Blanks
155600050909     c                   Eval      wscts = *On
155700050909     c                   EndIf
155800050909
155900050909      * --> Scaglione
156000050909      *     errore se non è stata immesso lo scaglione
156100050909if  1c                   If        v1cio3 = 'I' and v1csgl = *Zeros
156200050909     c                   Eval      *In28 = *On
156300050909     c                   Eval      *In53 = *On
156400050909     c                   Eval      v1cmsg = msg(38)
156500050909     c                   Leavesr
156600050909    1c                   EndIf
156700050909      *     errore se la tariffa prevede lo scaglione senza decimali
156800050909      *     e inseriti decimali
156900050909if  1c                   If        v1csgl <> *Zeros and §trsap = *Blanks
157000050909     c                   Move      v1csgl        w0030
157100050909if  2c                   If        w0030 <> *Zeros
157200050909     c                   Eval      *In28 = *On
157300050909     c                   Eval      *In53 = *On
157400050909     c                   Eval      v1cmsg = msg(39)
157500050909     c                   Leavesr
157600050909    2c                   EndIf
157700050909    1c                   EndIf
157800050909     c                   If        v1cio3 = 'O' and v1csgl > *Zeros
157900050909     c                   Eval      wssgl = *On
158000050909     c                   EndIf
158100050909
158200050920      * --> Copia del dettaglio tariffario
158300050920      *     carico le sk x i successivi controlli
158400050909      *     errore se non trovo nessun dettaglio da copiare
158500050909if  1c                   If        v1ctad <> 'N'
158600050920     c                   ExSr      Sr_Carsktad
158700050909     c   28              Leavesr
158800050909    1c                   EndIf
158900050909
159000050920      * --> Copia delle tariffe particolari
159100050909      *     errore se non trovo nessuna tariffa particolare
159200050909if  1c                   If        v1ctpt <> 'N'
159300050909     c                   ExSr      Sr_Ctrtpt
159400050909     c   28              Leavesr
159500050909      *     errore forzabile se trovo una tariffa particolare in scadenza
159600050909if  2c                   If        wftceli = *On and wforzaftc = *Off
159700050909     c                   Eval      *In28 = *On
159800050909     c                   Eval      *In47 = *On
159900050909     c                   Eval      v1cmsg = msg(26)
160000050909     c                   Eval      %subst(v1cmsg:21:1) = wftc
160100050909     c                   Move      datadmy       w0080
160200050909     c                   Eval      %subst(v1cmsg:43:10) = %editw(w0080:'  /  / -
160300050909     c                                ')
160400050909     c                   Eval      wforzaftc = *On
160500050909     c                   Leavesr
160600050909    2c                   EndIf
160700050909      *     errore se trovo nella tariffa a cui copiare
160800050909      *     una tariffa particolare uguale
160900050909if  2c                   If        woktpt = *On
161000050909     c                   Eval      *In28 = *On
161100050909     c                   Eval      *In47 = *On
161200050909     c                   Eval      v1cmsg = msg(22)
161300050909     c                   Leavesr
161400050909    2c                   EndIf
161500060307      *     errore forzabile se la tariffa a cui copiare ha la rinuncia AC BASE
161600060307      *     e c'è la tariffa particolare 'd ' nella tariffa da cui copiare
161700060307if  2c                   if        savfmp = 'S' and wforzaacbase = *off
161800060705     c                             and wacbasefmp = *on
161900060307     c                   eval      *in28 = *on
162000060307     c                   eval      *in47 = *on
162100060307     c                   eval      v1cmsg = msg(54)
162200060307     c                   eval      wforzaacbase = *on
162300060307     c                   leavesr
162400060307    2c                   endif
162500060705      *     errore forzabile se l'importo della tariffa particolare 'd ' AC BASE
162600060705      *     da cui copiare è minore rispetto a quello che dovrebbe essere
162700060705      *     per la tariffa a cui copiare
162800060307if  2c                   if        wforzaclv = *off and wacbase = *on
162900060307     c                   eval      *in28 = *on
163000060307     c                   eval      *in47 = *on
163100060307     c                   eval      v1cmsg = msg(57)
163200060307     c                   eval      wforzaclv = *on
163300120119      * salvo i dati che mi hanno scatenato l'errore di ac base incongruente
163400120119     c                   eval      w_v1ksco = v1ksco
163500120119     c                   eval      w_v1nrvot = v1nrvot
163600120119     c                   eval      w_v1ctro = v1ctro
163700120119     c                   eval      w_v1prgo = v1prgo
163800060307     c                   leavesr
163900060307    2c                   endif
164000050909    1c                   EndIf
164100050909
164200050909      * --> Controllo se esiste già qualcosa nella tariffa a cui copiare
164300050923      *     se copia verso tariffa/offerta esistente
164400050805if  1c                   If        *In04
164500050707      *     --> La tariffa di giacenza
164600050707      *         errore se trovo nella tariffa a cui copiare
164700050707      *         la tariffa di giacenza
164800050909if  2c                   If        v1ctgc <> 'N'
164900090408     c  n01              move      v1kscn        kksc
165000101222     c   01              Eval      kksc = v1nrvnt
165100050711     c                   Move      v1ctrn        kctr
165200050711     c                   Move      v1prgn        kprg
165300050729     c  n01kTntam01      Chain     Titgc01l
165400050729     c   01kTntam01      Chain     Tiogc01l
165500060307if  3c                   If        %Found and tgcatb = *blanks
165600050707     c                   Eval      *In28 = *On
165700050909     c                   Eval      *In48 = *On
165800050714     c                   Eval      v1cmsg = msg(27)
165900050707     c                   Leavesr
166000050805    3c                   EndIf
166100050805    2c                   EndIf
166200050801
166300050801      *     --> Dati tipo
166400050801      *         errore se trovo nella tariffa a cui copiare
166500050801      *         i dati tipo
166600050805if  2c                   If        v1ccat = 'S'
166700050805if  3c                   If        Not *In01
166800050801     c                   Eval      kcto = 'T'
166900090408     c                   move      v1kscn        kksc
167000050805   x3c                   Else
167100050801     c                   Eval      kcto = 'O'
167200101222     c                   Eval      kksc = v1nrvnt
167300050805    3c                   EndIf
167400061009     c                   Eval      kdsf = 'S'
167500050801     c                   Move      v1ctrn        kctr
167600050801     c                   Move      v1prgn        kprg
167700050801     c     kTeetc        Chain     Teetc01l
167800050805if  3c                   If        %Found(Teetc01l)
167900050801     c                   Eval      *In28 = *On
168000050909     c                   Eval      *In49 = *On
168100050801     c                   Eval      v1cmsg = msg(28)
168200050801     c                   Leavesr
168300050805    3c                   EndIf
168400050805    2c                   EndIf
168500050801    1c                   EndIf
168600050707
168700050819     c                   EndSr
168800050729      *------------------------------------------------------------------------*
168900050729      * CONFERMA
169000050729      *------------------------------------------------------------------------*
169100050729     c     Sr_Conferma   BegSr
169200150210
169300150210     c                   eval      wokAGGdst = *off
169400150210
169500150210      //?In caso di copia da tariffa a tariffa su progressivo nuovo
169600150210      //?devo verificare la data scadenza
169700150210      //?dell'ultimo progressivo della nuova tariffa
169800150210      //?con data ultima spedizione fatturata
169900150210     c                   IF        not *in03 and
170000150210     c                             not *in01 and
170100150210     c                             not *in04
170200150210     c                   exsr      CercaDate
170300150210       //?Se la data scadenza della tariffa è inferiore alla data
170400150210       //?ultima spedizione fatturata devo emettere la win x poter
170500150210       //?modificare la data scadenza
170600150210     c                   IF        sav_TAMdst < wDataSP
170700150210     c                   eval      wInzW02 = *on
170800150210     c                   exsr      GesW02
170900150210     c                   IF        *In90
171000150210     c                   Goto      Emid01
171100150210     c                   ENDIF
171200150210     c                   ENDIF
171300150210     c                   ENDIF
171400050919
171500050919      * Se ho richiesto la copia del dettaglio tariffario devo prima
171600050919      * emettere un subfile per la richiesta della nuova lnp della tariffa
171700050919      * da creare
171800050923      * se non sto copiando verso una cartello
171900050923if  1c                   If        v1ctad <> 'N' and wnewcar = *Off
172000050919     c                   ExSr      Sr_Newlnp
172100050919    1c                   EndIf
172200050922      * se copio da cartello a cartello in modo parziale devo emettere
172300050922      * il subfile
172400050922if  1c                   If        v1ctad = 'D' and woldcar = *On and
172500050922     c                             wnewcar = *On
172600050922     c                   ExSr      Sr_Newlnp
172700050922    1c                   EndIf
172800050729
172900050729      * --> Copia tariffa TOTALE
173000050909if  1c                   If        v1ctad = 'T' and
173100050909     c                             v1ctpt = 'T' and
173200050909     c                             v1ctgc = 'T'
173300050729     c                   ExSr      Sr_Cards
173400050729     c  n04              Eval      ta25tam = 'S'
173500050729     c                   Eval      ta25tad = 'T'
173600050729     c                   Eval      ta25tpt = 'T'
173700050803     c                   Eval      ta25tpd = 'N'
173800050729     c                   Eval      ta25tgc = 'T'
173900050804     c                   Eval      ta25cat = v1ccat
174000050729     c                   ExSr      Sr_Copia
174100050729     c                   Eval      savtad = ta25tad
174200050729     c                   Eval      savtpt = ta25tpt
174300050729     c                   Eval      savtgc = ta25tgc
174400050729     c                   Eval      savcat = ta25cat
174500050922      * Imposto almeno un flag di copia x poi richiamare la inglo
174600050922     c                   If        savtad <> 'N' or savtpt <> 'N' or
174700050922     c                             savtgc <> 'N'
174800050922     c                   Eval      wcopiatad = *On
174900050922     c                   EndIf
175000050729      * Dopo aver richiamato il pgm di copia faccio vedere
175100050729      * quello che è successo nella copia totale
175200050729     c                   ExSr      Sr_Card07
175300050729     c                   Leavesr
175400050729    1c                   EndIf
175500050729
175600050729      * --> Copia tariffa DETTAGLIO
175700050729      *     imposto già i campi nella ds esterna x la copia
175800050729     c                   Clear                   Tnta25ds
175900050729     c                   ExSr      Sr_Cards
176000050803
176100050729      *     --> Copia solo il dettaglio tariffario
176200050909if  1c                   If        v1ctad = 'T'
176300050729     c  n04              Eval      ta25tam = 'S'
176400050729     c                   Eval      ta25tad = 'T'
176500050729     c                   Eval      ta25tpt = 'N'
176600050729     c                   Eval      ta25tpd = 'N'
176700050729     c                   Eval      ta25tgc = 'N'
176800050729     c                   Eval      ta25cat = 'N'
176900050729     c                   ExSr      Sr_Copia
177000050729     c                   Eval      savtad = ta25tad
177100050729     c                   Eval      savtpt = ta25tpt
177200050729     c                   Eval      savtgc = ta25tgc
177300050729     c                   Eval      savcat = ta25cat
177400050729      *         se la copia non è andata a buon fine faccio vedere
177500050729      *         quello che è successo nella copia del dettaglio
177600050729      *         e interrompo la copia
177700050729if  2c                   If        ta25err <> *Blanks
177800050729     c                   ExSr      Sr_Card07
177900050729     c                   Leavesr
178000050729    2c                   EndIf
178100050922      *         se la copia è andata a buon fine imposto il flag
178200050922      *         di copia x richiamare la inglo
178300050922     c                   Eval      wcopiatad = *On
178400050729    1c                   EndIf
178500050729      *     --> Visualizza dettaglio tariffario
178600050909if  1c                   If        v1ctad = 'D'
178700050729     c                   ExSr      Sr_Titad
178800050729    1c                   EndIf
178900050803
179000050729      *     --> Copia solo le tariffe particolari
179100050909if  1c                   If        v1ctpt = 'T'
179200050802     c                   If        Not *In04 and wprg = *Blanks
179300050729     c                   Eval      ta25tam = 'S'
179400050908     c                   Else
179500050908     c                   Eval      ta25tam = 'N'
179600050729     c                   EndIf
179700050729     c                   Eval      ta25tad = 'N'
179800050729     c                   Eval      ta25tpt = 'T'
179900050729     c                   Eval      ta25tpd = 'N'
180000050729     c                   Eval      ta25tgc = 'N'
180100050729     c                   Eval      ta25cat = 'N'
180200050729     c                   ExSr      Sr_Copia
180300050729     c                   Eval      savtpt = ta25tpt
180400050729     c                   Eval      savtgc = ta25tgc
180500050729     c                   Eval      savcat = ta25cat
180600050729      *         se la copia non è andata a buon fine faccio vedere
180700050729      *         quello che è successo nella copia delle tariffe particolari
180800050729      *         e interrompo la copia
180900050729if  2c                   If        ta25err <> *Blanks
181000050729     c                   ExSr      Sr_Card07
181100050729     c                   Leavesr
181200050729    2c                   EndIf
181300050922      *         se la copia è andata a buon fine imposto il flag
181400050922      *         di copia x richiamare la inglo
181500050922     c                   Eval      wcopiatpt = *On
181600050729    1c                   EndIf
181700050729      *     --> Visualizza tariffe particolari
181800050909if  1c                   If        v1ctpt = 'D'
181900050729     c                   ExSr      Sr_Titpt
182000050729    1c                   EndIf
182100050803
182200050729      *     --> Copia solo le tariffe di giacenza
182300050909if  1c                   If        v1ctgc = 'T'
182400050802     c                   If        Not *In04 and wprg = *Blanks
182500050729     c                   Eval      ta25tam = 'S'
182600050908     c                   Else
182700050908     c                   Eval      ta25tam = 'N'
182800050729     c                   EndIf
182900050729     c                   Eval      ta25tad = 'N'
183000050729     c                   Eval      ta25tpt = 'N'
183100050729     c                   Eval      ta25tpd = 'N'
183200050729     c                   Eval      ta25tgc = 'T'
183300050729     c                   Eval      ta25cat = 'N'
183400050729     c                   ExSr      Sr_Copia
183500050729     c                   Eval      savtgc = ta25tgc
183600050729     c                   Eval      savcat = ta25cat
183700050729      *         se la copia non è andata a buon fine faccio vedere
183800050729      *         quello che è successo nella copia delle tariffe di giacenza
183900050729      *         e interrompo la copia
184000050729if  2c                   If        ta25err <> *Blanks
184100050729     c                   ExSr      Sr_Card07
184200050729     c                   Leavesr
184300050729    2c                   EndIf
184400050922      *         se la copia è andata a buon fine imposto il flag
184500050922      *         di copia x richiamare la inglo
184600050922     c                   Eval      wcopiatgc = *On
184700050729    1c                   EndIf
184800050729      *     --> Visualizza tariffe di giacenza
184900050909if  1c                   If        v1ctgc = 'D'
185000050729     c                   ExSr      Sr_Titgc
185100050729    1c                   EndIf
185200050803
185300050729      *     --> Copia solo i dati tipo
185400050801      *         non possibile
185500050801      * Se arrivo qua vuol dire che sono riuscita a copiare tutto
185600050801      * e faccio vedere quello che è successo nella copia
185700050729     c                   ExSr      Sr_Card07
185800050729
185900050729     c                   EndSr
186000150210
186100150210      /free
186200150210       //--------------------------------------------------------------
186300150210       //?Cerco le date tariffa e ultima sped.fatturata.
186400150210       //--------------------------------------------------------------
186500150210       BEGSR CercaDate;
186600150210
186700150210       //?cerco l'ultimo progressivo della tariffa nuova
186800150210         clear wDataScadNw;
186900150210         sav_TAMksc = %dec(V1kscn:7:0);
187000150210         sav_TAMctr = %dec(V1ctrn:3:0);
187100150210         chain (sav_TAMksc:sav_TAMctr) TNTAM04L;
187200150210         IF  not %found(TNTAM04L);
187300150210           leavesr;
187400150210         ENDIF;
187500150219         sav_TAMprg = w_TAMprg;
187600150219         sav_TAMddt = w_TAMddt;
187700150219         sav_TAMdst = w_TAMdst;
187800150210
187900150210       //?Cerco la data ultima spedizione fatturata con la tariffa
188000150210       //?che andrò a creare
188100150210         clear wDataSP;
188200150210         clear TASaas;
188300150210         clear TASmgs;
188400150210         setgt  (sav_TAMksc:sav_TAMctr) TITAS41C;
188500150210         readpe (sav_TAMksc:sav_TAMctr) TITAS41C;
188600150210         wDataSP = TASaas * 10000 + TASmgs;
188700150210
188800150210       ENDSR;
188900150210       //--------------------------------------------------------------
189000150210       //?WIN per incongruenza date deorrenza nuova tariffa.
189100150210       //--------------------------------------------------------------
189200150210       BEGSR GesW02;
189300150210
189400150210         wFineW02 = *off;
189500150210
189600150210       //?Inizializzo la videata
189700150210         IF  wInzW02;
189800150210           exsr InzW02;
189900150210           wInzW02 = *off;
190000150210         ENDIF;
190100150210
190200150210       //?Emetto la videata
190300150210         DOW  not wFineW02;
190400150210           exfmt  TA24W02;
190500150210           *in28 = *off;
190600150210           clear W02msg;
190700150210
190800150210           SELECT;
190900150210
191000150210         //?- F12=Ritorno
191100150210           WHEN  *inkl;
191200150210             *in90 = *on;
191300150210             wFineW02 = *on;
191400150210             leavesr;
191500150210
191600150210         //?- F06=Modifica tariffa
191700150210           WHEN  *inkf;
191800150210             exsr ContrW02;
191900150210             IF  *in28;
192000150210               iter;
192100150210             ENDIF;
192200150210           //?Imposto il flag per aggiornare la data scadenza
192300150210           //?ultimo progressivo della tariffa nuova da creare
192400150210             wokAGGdst = *on;
192500150210             wFineW02 = *on;
192600150210             leavesr;
192700150210
192800150210         //?- Enter
192900150210           OTHER;
193000150210             exsr ContrW02;
193100150210             IF  *in28;
193200150210               iter;
193300150210             ENDIF;
193400150210
193500150210           ENDSL;
193600150210
193700150210         ENDDO;
193800150210
193900150210       ENDSR;
194000150210
194100150210       //--------------------------------------------------------------
194200150210       //?Inizializzo la WIN.
194300150210       //--------------------------------------------------------------
194400150210       BEGSR InzW02 ;
194500150210
194600150210         clear TA24W02;
194700150210         W02ksc = sav_TAMksc;
194800150210         W02ctr = sav_TAMctr;
194900150210         W02prg = sav_TAMprg;
195000150210         w0070 = W02ksc;
195100150210         exsr sr_Chkcli;
195200150210         W02rag = ACOrag;
195300150210         *in55 = *off;
195400150210
195500150210         datadmy = %date(sav_TAMddt:*iso);
195600150210         W02ddt = %dec(datadmy);
195700150210
195800150210         datadmy = %date(sav_TAMdst:*iso);
195900150210         W02dst = %dec(datadmy);
196000150210
196100150210       //?Imposto già l'errore
196200150210         *in28 = *on;
196300150210         *in55 = *on;
196400150210         W02msg = 'Scadenza Tariffa < Ultima Sped. Fatt. ' +
196500150210                   %subst(%editc(wDataSP:'X'):7:2) + '/' +
196600150210                   %subst(%editc(wDataSP:'X'):5:2) + '/' +
196700150210                   %subst(%editc(wDataSP:'X'):1:4);
196800150210
196900150210       ENDSR;
197000150210
197100150210       //--------------------------------------------------------------
197200150210       //?Controllo i dati della WIN.
197300150210       //--------------------------------------------------------------
197400150210       BEGSR ContrW02;
197500150210
197600150210         *in55 = *off;
197700150210
197800150210       //?Controllo data scadenza tariffa
197900150210         IF  W02dst = 0;
198000150210           *in28 = *on;
198100150210           *in55 = *on;
198200150210           W02msg = msg(61);
198300150210           leavesr;
198400150210         ENDIF;
198500150210
198600150210         clear wlbdat;
198700150210         G02dat = W02dst;
198800150210         xsrda8(wlbdat);
198900150210         IF  G02err = '1';
199000150210           *in28 = *on;
199100150210           *in55 = *on;
199200150210           W02msg = msg(61);
199300150210           leavesr;
199400150210         ENDIF;
199500150210
199600150210         W02dst = G02dat;
199700150210         wDataScadNw = G02inv;
199800150210
199900150210       //?se < data ultima spedizione fatturata
200000150210       //?errore
200100150210         IF  wDataScadNw < wDataSP;
200200150210           *in28 = *on;
200300150210           *in55 = *on;
200400150210           W02msg = 'Scadenza Tariffa < Ultima Sped. Fatt. ' +
200500150210                    %subst(%editc(wDataSP:'X'):7:2) + '/' +
200600150210                    %subst(%editc(wDataSP:'X'):5:2) + '/' +
200700150210                    %subst(%editc(wDataSP:'X'):1:4);
200800150210           leavesr;
200900150210         ENDIF;
201000150210
201100150210         IF  wDataScadNw < sav_TAMdst;
201200150210           *in28 = *on;
201300150210           *in55 = *on;
201400150210           W02msg = 'Scadenza Tariffa < ' +
201500150210                    %subst(%editc(wDataSP:'X'):7:2) + '/' +
201600150210                    %subst(%editc(wDataSP:'X'):5:2) + '/' +
201700150210                    %subst(%editc(wDataSP:'X'):1:4) + ' ' +
201800150210                    'non si può anticipare.';
201900150210           leavesr;
202000150210         ENDIF;
202100150210
202200150210       ENDSR;
202300150210
202400150210      /end-free
202500050728
202600050728      *------------------------------------------------------------------------*
202700050920      * CARICO LE SCHIERE PER I CONTROLLI SUL DETTAGLIO TARIFFARIO
202800050728      *------------------------------------------------------------------------*
202900050920     c     Sr_Carsktad   BegSr
203000050728
203100050728     c                   Clear                   oldlnp
203200050728     c                   Clear                   oldcts
203300050728     c                   Clear                   sktad
203400050728     c                   Eval      woktad = *Off
203500050728     c                   Clear                   sktpt
203600050920     c                   Movea     *Zeros        sklnpo
203700050728
203800050728      * Leggo dettaglio tariffa/offerta da cui copiare
203900090408     c  n03              move      v1ksco        kksc
204000101222     c   03              Eval      kksc = v1nrvot
204100050728     c                   Move      v1ctro        kctr
204200050728     c                   Move      v1prgo        kprg
204300050728     c  n03kTntam01      Setll     Titad04l
204400050728     c   03kTntam01      Setll     Tiofd01l
204500050728      * se non trovo il dettaglio tariffario errore
204600050728if  1c                   If        Not %Equal
204700050728     c                   Eval      *In28 = *On
204800050913     c                   Eval      *In46 = *On
204900050909     c                   Eval      v1cmsg = msg(43)
205000050909     c                   Eval      %subst(v1cmsg:32:7) = 'copiare'
205100050728     c                   Leavesr
205200050728    1c                   EndIf
205300050728do  1c                   Do        *Hival
205400050728     c  n03kTntam01      Reade     Titad04l
205500050728     c   03kTntam01      Reade     Tiofd01l
205600050728     c                   If        %Eof
205700050728     c                   Leave
205800050728     c                   EndIf
205900050728     c                   If        tadatb <> *Blanks
206000050728     c                   Iter
206100050728     c                   EndIf
206200050728      * Controllo le inclusioni/omissioni
206300050728     c                   ExSr      Sr_Seltad
206400050728     c                   If        wokrec = *On
206500050728     c                   Iter
206600050728     c                   EndIf
206700050728     c                   If        tadlnp = oldlnp and tadcts = oldcts
206800050728     c                   Iter
206900050728     c                   EndIf
207000050728      * a rottura di LNP/CTS memorizzo in sk
207100050919      * prima memorizzo la sk delle lnp presenti in tariffa
207200050922      * se non è una copia totale da cartello a cartello
207300050922if  2c                   If        wnewcar = *Off or
207400050922     c                             (wnewcar = *On and v1ctad = 'D')
207500050920     c                   Move      tadlnp        w003a
207600050920     c     w003a         Lookup    sklnpo                                 30
207700050922if  3c                   If        Not *In30
207800050919     c                   Eval      xx = 1
207900050919     c     *Zeros        Lookup    sklnpo(xx)                             30
208000050922if  4c                   If        *In30
208100050920     c                   Eval      sklnpo(xx) = w003a
208200050920      * se supero le 50 linee di partenza devo dare errore
208300050920      * xchè nella ds ho solo 50 linee di partenza
208400050922   x4c                   Else
208500050919     c                   Eval      *In28 = *On
208600050922     c                   Eval      *In51 = *On
208700050919     c                   Eval      v1cmsg = msg(46)
208800050919     c                   Leavesr
208900050922    4c                   EndIf
209000050922    3c                   EndIf
209100050919    2c                   EndIf
209200050919      * poi memorizzo la sk delle lnp/cts
209300050728     c                   Movel     tadlnp        wtad
209400050728     c                   Move      tadcts        wtad
209500050728     c     wtad          Lookup    sktad                                  30
209600050728if  2c                   If        Not *In30
209700050728     c                   Eval      xx = 1
209800050728     c     *Blanks       Lookup    sktad(xx)                              30
209900050728     c                   If        *In30
210000050728     c                   Eval      sktad(xx) = wtad
210100050728     c                   EndIf
210200050728    2c                   EndIf
210300050728     c                   Eval      oldlnp = tadlnp
210400050728     c                   Eval      oldcts = tadcts
210500050728    1c                   EndDo
210600050728      * se alla fine di tutto quello che ho letto non ho caricato la sk
210700050728      * vuol dire che non ho trovato niente
210800050728      * se non trovo il dettaglio tariffario errore
210900050728if  1c                   If        sktad(1) = *Blanks
211000050728     c                   Eval      *In28 = *On
211100050913     c                   Eval      *In46 = *On
211200050909     c                   Eval      v1cmsg = msg(43)
211300050909     c                   Eval      %subst(v1cmsg:32:7) = 'copiare'
211400050728     c                   Leavesr
211500050728    1c                   EndIf
211600050920
211700050920     c                   EndSr
211800050920
211900050920      *------------------------------------------------------------------------*
212000050920      * CONTROLLO I DUE DETTAGLI TARIFFARI
212100050920      *------------------------------------------------------------------------*
212200050920     c     Sr_Ctrtad     BegSr
212300050921
212400050921     c                   Eval      woktad = *Off
212500050728
212600050728      * Copia del dettaglio tariffario
212700050728      * controllo se trovo nella tariffa a cui copiare
212800050728      * dettaglio uguale (LNP CTS)
212900050908      * ma solo se la tariffa non è nuova
213000050909if  1c                   If        v1ctad = 'T' and *In04
213100050728     c                   Clear                   oldlnp
213200050728     c                   Clear                   oldcts
213300050728
213400050728      * Leggo dettaglio tariffa a cui copiare
213500090408     c  n01              move      v1kscn        kksc
213600101222     c   01              Eval      kksc = v1nrvnt
213700050728     c                   Move      v1ctrn        kctr
213800050728     c                   Move      v1prgn        kprg
213900050729     c  n01kTntam01      Setll     Titad04l
214000050729     c   01kTntam01      Setll     Tiofd01l
214100050728do  2c                   Do        *Hival
214200050729     c  n01kTntam01      Reade     Titad04l
214300050729     c   01kTntam01      Reade     Tiofd01l
214400050729     c                   If        %Eof
214500050728     c                   Leave
214600050728     c                   EndIf
214700050728     c                   If        tadatb <> *Blanks
214800050728     c                   Iter
214900050728     c                   EndIf
215000050728     c                   If        tadlnp = oldlnp and tadcts = oldcts
215100050728     c                   Iter
215200050728     c                   EndIf
215300050921      * Cerco la nuova linea di partenza
215400050920     c                   Eval      xx = 1
215500050920     c                   Move      tadlnp        w003a
215600050921     c     w003a         Lookup    sklnpn(xx)                             30
215700050921      * Non la trovo torno a leggere vuol dire che nelle vecchia tariffa
215800050921      * non c'era o non è stata richiesta per la copia
215900050921     c                   If        Not *In30
216000050921     c                   Iter
216100050921     c                   EndIf
216200050921      * La trovo recupero la vecchia linea di partenza x controllare
216300050921      * che non ci sia già lo stesso codice di tassazione
216400050921     c                   Movel     sklnpo(xx)    wtad
216500050728     c                   Move      tadcts        wtad
216600050920      * Controllo se già esiste
216700050728     c     wtad          Lookup    sktad                                  30
216800050728     c                   If        *In30
216900050728     c                   Eval      woktad = *On
217000050728     c                   Leave
217100050728     c                   EndIf
217200050728     c                   Eval      oldlnp = tadlnp
217300050728     c                   Eval      oldcts = tadcts
217400050728    2c                   EndDo
217500050728    1c                   EndIf
217600050728
217700050728     c                   EndSr
217800121001      /free
217900121001       //----------------------------------------------------------------------
218000121001       //?Controllo se Dettaglio OK x tariffa estero.
218100121001       //----------------------------------------------------------------------
218200121001       BEGSR CTRtadE;
218300121001
218400121001         woktad = *off;
218500121001
218600121001       //?Leggo il dettaglio tariffario con la vecchia LNP
218700121001         IF  not *in03;
218800121001           kksc = %dec(V1ksco:7:0);
218900121001         ELSE;
219000121001           kksc = V1nrvot;
219100121001         ENDIF;
219200121001         kctr  = %dec(V1ctro:3:0);
219300121001         kprg  = %dec(V1prgo:3:0);
219400121001         w0030 = %dec(V2slnpo:3:0);
219500121001
219600121001         IF not *in03;
219700121001           setll (kksc:kctr:kprg:w0030) TITAD04L;
219800121001           reade (kksc:kctr:kprg:w0030) TITAD04L;
219900121001         ELSE;
220000121001           setll (kksc:kctr:kprg:w0030) TIOFD01L;
220100121001           reade (kksc:kctr:kprg:w0030) TIOFD01L;
220200121001         ENDIF;
220300121001
220400121001         DOW not %eof;
220500121001         //?Controllo il codice di tassazione deve essere estero
220600121001           w002a = TADcts;
220700121001           exsr Sr_Chkcts;
220800121001           IF  §CTest <> 'E';
220900121001             woktad = *on;
221000121001             leave;
221100121001           ENDIF;
221200121001
221300121001           IF not *in03;
221400121001             reade (kksc:kctr:kprg:w0030) TITAD04L;
221500121001           ELSE;
221600121001             reade (kksc:kctr:kprg:w0030) TIOFD01L;
221700121001           ENDIF;
221800121001         ENDDO;
221900121001
222000121001       ENDSR;
222100121001      /end-free
222200050920
222300050728      *------------------------------------------------------------------------*
222400050728      * CONTROLLO LE DUE TARIFFE PARTICOLARI
222500050728      *------------------------------------------------------------------------*
222600050728     c     Sr_Ctrtpt     BegSr
222700050728
222800050728     c                   Clear                   sktpt
222900050728     c                   Eval      woktpt = *Off
223000060307     c                   eval      wacbase = *off
223100060705     c                   eval      wacbasefmp = *off
223200050728
223300050728      * Leggo le tariffe particolari da cui copiare
223400090408     c  n03              move      v1ksco        kksc
223500101222     c   03              Eval      kksc = v1nrvot
223600050729     c                   Move      v1ctro        kctr
223700050729     c                   Move      v1prgo        kprg
223800050728     c  n03kTntam01      Setll     Titpt01l
223900050728     c   03kTntam01      Setll     Tiopt01l
224000050919      * se non trovo tariffe particolari errore
224100050728if  1c                   If        Not %Equal
224200050728     c                   Eval      *In28 = *On
224300050913     c                   Eval      *In47 = *On
224400050909     c                   Eval      v1cmsg = msg(44)
224500050909     c                   Eval      %subst(v1cmsg:32:7) = 'copiare'
224600050728     c                   Leavesr
224700050728    1c                   EndIf
224800050728do  1c                   Do        *Hival
224900050728     c  n03kTntam01      Reade     Titpt01l
225000050728     c   03kTntam01      Reade     Tiopt01l
225100050728     c                   If        %Eof
225200050728     c                   Leave
225300050728     c                   EndIf
225400050728     c                   If        tptatb <> *Blanks
225500050728     c                   Iter
225600050728     c                   EndIf
225700050728      * controllo se la tariffa/offerta da cui copiare ha delle tariffe
225800050728      * particolari in scadenza
225900050728     c                   Eval      w002a = tptftc
226000050728     c                   ExSr      Sr_Chktpt
226100050728      * se tariffa particolare in scadenza esco
226200050728     c                   If        wftceli = *On and wforzaftc = *Off
226300050728     c                   Leavesr
226400050728     c                   EndIf
226500051019      * se tariffa particolare lasciato avviso 'c ' e non sto copiando
226600080609      * verso una cartello o verso un cliente relativo ad una delle
226700080609      * filiali abilitate torno a leggere
226800080609     c                   If        tptftc = 'c ' and wnewcar = *Off
226900080609     c                   if        %lookup(comlnp:sklav) = *zeros
227000080609     c                             and %lookup(999:sklav) = *zeros
227100051019     c                   Iter
227200080609     c                   endif
227300051019     c                   EndIf
227400060705      * se tariffa particolare 'd ' AC BASE
227500060705     c                   if        tptftc = 'd '
227600060705      * controllo se la tariffa a cui copiare ha il flag di rinuncia AC BASE
227700060705     c                   if        savfmp = 'S'
227800060705     c                   eval      wacbasefmp = *on
227900060307     c                   endif
228000111130      * se codice importanza a blank imposto "C"
228100111130     c                   If        savclva = ' '
228200111130     c                   eval      savclva = 'C'
228300111130     c                   endif
228400060705      * non controllo più il cod.importanza cliente ma direttamente l'importo
228500060705      * con quello che posso inserire per il codice a cui sto copiando
228600060705      * in questo modo posso copiare se la tariffa 'd ' ha un importo
228700060705      * più alto
228800060705      * cerco l'importo della tariffa particolare
228900060705     c                   eval      kftc = tptftc
229000060705     c  n03ktitpt        chain     titpd01l
229100060705     c   03ktitpt        chain     tiopd01l
229200060705     c                   if        %found
229300060705     c                   select
229400060705      * se il codice a cui copiare è presente in tabella il controllo lo
229500060705      * faccio con la forzatura inserita nella tabella
229600060705     c                   when      savfabitra <> *zeros and tpditr < savfabitra
229700060307     c                   eval      wacbase = *on
229800120112      * se il codice da cui copio  è presente in tabella ed è diverso dalla
229900120112      * forzatura inserita nella tabella del codice cliente a cui copio errore
230000120112     c                   when      savfabitra <> oldfabitra and oldfabitra > 0
230100120112     c                   eval      wacbase = *on
230200060705      * controllo con i valori fissi scritti in DV 1463 in base al codice
230300060705      * importanza cliente
230400120112      *** In base ai nuovi valori fissati il 30/11/2011 dv 2191 l'ac base
230500120112      *** avrà dei nuovi valori in base alla categoria dei clienti ma solo
230600120112      *** se si tratta di copia su offerta di una trattativa di tipo
230700120112      *** "NUOVO" indicatore 54 acceso per gli altri tipi di copie non ci sono
230800120112      *** controlli e vengono presi i dati di provenienza
230900140128
231000140128     c                   other
231100140128       //?Controllo con i valori presenti sul file TIACB
231200140128     c     kACB          setll     TIACB01L
231300140128     c     savclva       reade     TIACB01L
231400140128     c                   if        %found(TIACB01L) and
231500140128     c                             TPDitr < ACBitr and *in54
231600140128     c                   eval      wacbase = *on
231700140128     c                   ENDIF
231800060705     c                   endsl
231900060705     c                   endif
232000060705     c                   if        wacbasefmp = *on or wacbase = *on
232100060705     c                   iter
232200060705     c                   endif
232300060307     c                   endif
232400050728      * memorizzo in sk le tariffe particolari
232500050728     c     tptftc        Lookup    sktpt                                  30
232600050728if  2c                   If        Not *In30
232700050728     c                   Eval      xx = 1
232800050728     c     *Blanks       Lookup    sktpt(xx)                              30
232900050728     c                   If        *In30
233000050728     c                   Eval      sktpt(xx) = tptftc
233100050728     c                   EndIf
233200050728    2c                   EndIf
233300050728    1c                   EndDo
233400050728      * se alla fine di tutto quello che ho letto non ho caricato la sk
233500050728      * vuol dire che non ho trovato niente
233600050919      * se non trovo tariffe particolari errore
233700050728if  1c                   If        sktpt(1) = *Blanks
233800050728     c                   Eval      *In28 = *On
233900050913     c                   Eval      *In47 = *On
234000050909     c                   Eval      v1cmsg = msg(44)
234100050909     c                   Eval      %subst(v1cmsg:32:7) = 'copiare'
234200050728     c                   Leavesr
234300050728    1c                   EndIf
234400050728
234500050728      * controllo se trovo nella tariffa a cui copiare
234600050728      * una tariffa particolare uguale
234700050908      * ma solo se la tariffa non è nuova
234800050909if  1c                   If        v1ctpt = 'T' and *In04
234900050728      * Leggo tariffe particolari a cui copiare
235000090408     c  n01              move      v1kscn        kksc
235100101222     c   01              Eval      kksc = v1nrvnt
235200050728     c                   Move      v1ctrn        kctr
235300050728     c                   Move      v1prgn        kprg
235400050729     c  n01kTntam01      Setll     Titpt01l
235500050729     c   01kTntam01      Setll     Tiopt01l
235600050728do  2c                   Do        *Hival
235700050729     c  n01kTntam01      Reade     Titpt01l
235800050729     c   01kTntam01      Reade     Tiopt01l
235900050729     c                   If        %Eof
236000050728     c                   Leave
236100050728     c                   EndIf
236200050728     c                   If        tptatb <> *Blanks
236300050728     c                   Iter
236400050728     c                   EndIf
236500050728     c     tptftc        Lookup    sktpt                                  30
236600050728     c                   If        *In30
236700050728     c                   Eval      woktpt = *On
236800050728     c                   Leave
236900050728     c                   EndIf
237000050728    2c                   EndDo
237100050728    1c                   EndIf
237200050728
237300050728     c                   EndSr
237400050919
237500050919      *------------------------------------------------------------------------*
237600050919      * VISUALIZZA LE LINEE DI PARTENZA PRESENTI NELLA TARIFFA
237700050919      *------------------------------------------------------------------------*
237800050919     c     Sr_Newlnp     BegSr
237900050919
238000050919      * Pulisco il subfile
238100050919     c                   Clear                   nrr2
238200050919     c                   Eval      *In20 = *Off
238300050919     c                   Eval      *In21 = *Off
238400050919     c                   Write     ta24c02
238500050919     c                   Eval      *In21 = *On
238600050919     c                   Eval      *In20 = *On
238700050919
238800050919     c                   Eval      recsf2 = 1
238900050919     c                   Eval      *In31 = *Off
239000050919
239100050919      * Carico il subfile in base alle lnp che ho caricato nella sk
239200050920do  1c                   Do        50            xx
239300050920     c                   If        sklnpo(xx) > *Zeros
239400050919     c                   Clear                   v2slnpo
239500050919     c                   Clear                   v2dlnpo
239600050919     c                   Clear                   v2slnpn
239700050919     c                   Clear                   v2dlnpn
239800050920     c                   Move      sklnpo(xx)    v2slnpo
239900050919     c     v2slnpo       Chain     Azorg01l
240000050919     c                   If        %Found(Azorg01l)
240100050919     c                   Eval      v2dlnpo = orgdes
240200050919     c                   EndIf
240300050919     c                   Add       1             nrr2
240400050919     c                   Write     ta24s02
240500050920     c                   EndIf
240600050919    1c                   EndDo
240700050920     c                   Eval      *In31 = *On
240800050923     c                   Clear                   wnewlnp
240900050923
241000050923      * se ho caricato 1 sola linea di partenza
241100050923if  1c                   If        nrr2 = 1
241200050923      * se non è uguale alla lnp del cliente a cui copiare
241300050923      * imposto la nuova linea di partenza = alla lnp del cliente
241400050920     c                   Move      comlnp        w003a
241500050920if  2c                   If        sklnpo(1) <> w003a
241600050923     c                   Eval      wnewlnp = comlnp
241700050923      * se è uguale alla lnp del cliente a cui copiare
241800050923      * imposto la nuova linea di partenza = alla lnp caricata
241900050923   x2c                   Else
242000050923     c                   Move      sklnpo(1)     wnewlnp
242100050923    2c                   EndIf
242200050923      * aggiorno il subfile
242300050920     c     nrr2          Chain     ta24s02                            32
242400050923if  2c                   If        Not *In32
242500050920     c                   Eval      recsf2 = nrr2
242600050923     c                   Eval      v2slnpn = wnewlnp
242700050920     c     v2slnpn       Chain     Azorg01l
242800050920     c                   If        %Found(Azorg01l)
242900050920     c                   Eval      v2dlnpn = orgdes
243000050920     c                   Update    ta24s02
243100050920     c                   EndIf
243200050923    2c                   EndIf
243300050920    1c                   EndIf
243400050919
243500050919      * Emetto il subfile
243600050919     c     Emis02        Tag
243700050919
243800050920     c                   Movea     *Zeros        sklnpn
243900050919     c                   If        recsf2 > nrr2
244000050919     c                   Z-add     1             recsf2
244100050919     c                   EndIf
244200050919
244300050919     c                   Write     ta24t02
244400050919     c                   Write     ta24z02
244500050919     c                   Exfmt     ta24c02
244600050920
244700050920     c                   Eval      *In28 = *Off
244800050920     c                   Clear                   v2cmsg
244900050919
245000050919      * F12=Ritorno
245100050919     c                   If        *Inkl
245200050919      * accendo il 90 così non pulisco il video
245300050919     c                   Eval      *In90 = *On
245400050919     c                   Goto      Emid01
245500050919     c                   EndIf
245600050919
245700050919      * Controllo i dati immessi nel subfile
245800050919     c                   Clear                   nrr2
245900050919do  1c                   Do        *Hival
246000050919     c                   Add       1             nrr2
246100050919     c     nrr2          Chain     ta24s02                            32
246200050919      * esco se fine sfl
246300050919     c                   If        *In32
246400050919     c                   Leave
246500050919     c                   EndIf
246600050919
246700050919     c                   Eval      recsf2 = nrr2
246800050919     c                   setoff                                       18
246900050919
247000050919      * La nuova linea di partenza deve essere presente
247100050919if  2c                   If        v2slnpn = *Zeros
247200050919     c                   Clear                   v2dlnpn
247300050919     c                   Eval      *In18 = *On
247400050919     c                   Eval      *In28 = *On
247500050919     c                   Eval      v2cmsg = msg(47)
247600050919     c                   Update    ta24s02
247700050919     c                   Goto      Emis02
247800050919    2c                   EndIf
247900050919      * deve essere valida
248000050922     c                   Clear                   og143
248100050919     c     v2slnpn       Chain     Azorg01l
248200050919if  2c                   If        Not %Found(Azorg01l) or
248300050919     c                             orgfva <> *Blanks or
248400050919     c                             (orgfag <> 'A' and orgfag <> 'F' and
248500050919     c                              orgfl2 <> 'S')
248600050919     c                   Clear                   v2dlnpn
248700050919     c                   Eval      *In18 = *On
248800050919     c                   Eval      *In28 = *On
248900050919     c                   Eval      v2cmsg = msg(32)
249000050919     c                   Eval      %subst(v2cmsg:19:3) = %editc(v2slnpn:'1')
249100050919     c                   Update    ta24s02
249200050919     c                   Goto      Emis02
249300050919    2c                   EndIf
249400050919      * decodifico la nuova linea
249500050919     c                   Eval      v2dlnpn = orgdes
249600050922      * network nuova linea di partenza
249700050922     c                   Eval      og143 = orgde3
249800050922      * se tariffa italia non posso inserire lnp estere
249900050922if  2c                   If        v1cfie = 'I' and wdpd = *Blanks and
250000050922     c                             wfed = *Blanks and
250100050922     c                             (§ogntw = 'EEX' or §ogntw = 'EUP' or
250200050922     c                              §ogntw = 'FED')
250300050922     c                   Eval      *In18 = *On
250400050922     c                   Eval      *In28 = *On
250500050922     c                   Eval      v2cmsg = msg(24)
250600050922     c                   Update    ta24s02
250700050922     c                   Goto      Emis02
250800050922    2c                   EndIf
250900050922      * se la nuova linea di partenza è 950 non devo avere un codice di
251000050922      * tassazione IT o EE
251100050922if  2c                   If        v2slnpn = 950
251200050922     c                   Movel     v2slnpo       wtad
251300050922     c                   Move      'IT'          wtad
251400050922     c     wtad          Lookup    sktad                                  30
251500050922     c  n30              Move      'EE'          wtad
251600050922     c  n30wtad          Lookup    sktad                                  30
251700050922if  3c                   If        *In30
251800050922     c                   Eval      *In18 = *On
251900050922     c                   Eval      *In28 = *On
252000050922     c                   Eval      v2cmsg = msg(25)
252100050922     c                   Eval      %subst(v2cmsg:36:3) = %editc(v2slnpo:'1')
252200050922     c                   Update    ta24s02
252300050922     c                   Goto      Emis02
252400050922    3c                   EndIf
252500050922    2c                   EndIf
252600050919      * non deve essere uguale ad una nuova lnp già immessa a video
252700050920     c                   Move      v2slnpn       w003a
252800050920     c     w003a         Lookup    sklnpn                                 30
252900050920if  2c                   If        *In30
253000050919     c                   Eval      *In18 = *On
253100050919     c                   Eval      *In28 = *On
253200050919     c                   Eval      v2cmsg = msg(48)
253300050919     c                   Update    ta24s02
253400050919     c                   Goto      Emis02
253500050919    2c                   EndIf
253600121001      /free
253700121001       //?Se copia verso tariffa Estera e LNP nuova non è estera
253800121001       //?devo controllare che il CTS sia estero se no errore
253900121002       //?e se non sto copiando da tariffa di cartello
254000121001         IF  V1Cfie = 'E' and §OGntw <> 'EEX' and
254100121002             §OGntw <> 'FED' and §OGntw <> 'EUP' and
254200121002             woldcar = *off;
254300121001           exsr CTRtadE;
254400121001           IF  woktad = *on;
254500121001             *in18 = *on;
254600121001             *in28 = *on;
254700121001             V2Cmsg = msg(60);
254800121001             UPDATE TA24S02;
254900121001      /end-free
255000121001     c                   Goto      Emis02
255100121001      /free
255200121001           ENDIF;
255300121001         ENDIF;
255400121001      /end-free
255500050919
255600050919     c                   Update    ta24s02
255700050919      * memorizzo in sk
255800050920     c                   Move      v2slnpn       sklnpn(nrr2)
255900050919    1c                   EndDo
256000050920
256100050920      * Controllo se esiste già qualcosa nella tariffa a cui copiare
256200050920     c                   ExSr      Sr_Ctrtad
256300050920      * errore se trovo nella tariffa a cui copiare
256400050920      * dettaglio uguale (LNP CTS)
256500050920if  1c                   If        woktad = *on
256600050920     c     xx            Chain     ta24s02                            32
256700050920if  2c                   If        Not *In32
256800050920     c                   Eval      recsf2 = xx
256900050920     c                   Eval      *In18 = *On
257000050920     c                   Eval      *In28 = *On
257100050920     c                   Eval      v2cmsg = msg(40)
257200050920     c                   Update    ta24s02
257300050920    2c                   EndIf
257400050920     c                   Goto      Emis02
257500050920    1c                   EndIf
257600050919
257700050919      * F6=Conferma
257800050919     c                   If        Not *InKf
257900050920     c                   Eval      recsf2 = 1
258000050919     c                   Goto      Emis02
258100050919     c                   EndIf
258200050919
258300050919     c                   Eval      *In31 = *Off
258400050919
258500050919     c                   EndSr
258600050708
258700050708      *------------------------------------------------------------------------*
258800050708      * VISUALIZZA IL DETTAGLIO TARIFFARIO
258900050708      *------------------------------------------------------------------------*
259000050708     c     Sr_Titad      BegSr
259100050711
259200050712     c                   Eval      wcopiatad = *Off
259300050909     c                   Eval      wselt = *Off
259400050708
259500050708      * Pulisco il subfile
259600050711     c     Cars03        Tag
259700050712     c                   Clear                   v3clnp
259800050712     c                   Clear                   v3ccts
259900050708     c                   ExSr      Sr_Puls03
260000050714      * Leggo dettaglio tariffa/offerta da cui copiare
260100090408     c  n03              move      v1ksco        kksc
260200101222     c   03              Eval      kksc = v1nrvot
260300050714     c                   Move      v1ctro        kctr
260400050714     c                   Move      v1prgo        kprg
260500050714     c                   ExSr      Sr_Setll03
260600050714      * Carico il subfile
260700050708     c                   ExSr      Sr_Cars03
260800050728      * se non ho caricato nessun dettaglio tariffario emetto errore
260900050909      * e torno alla prima videata
261000050728     c                   If        nrr3 = *Zeros
261100050728     c                   Eval      *In28 = *On
261200050909     c                   Eval      v1cmsg = msg(43)
261300050909     c                   Eval      %subst(v1cmsg:32:12) = 'visualizzare'
261400050909     c                   Goto      Emid01
261500050728     c                   EndIf
261600050708      * Emetto il subfile
261700050711     c     Emis03        Tag
261800050711     c                   ExSr      Sr_Emis03
261900050711
262000050711      * Controllo se è stata immessa una lnp come ricerca
262100050711if  1c                   If        v3clnp <> *Zeros
262200050711     c                   Eval      *In17 = *On
262300050711     c                   Eval      klnp = v3clnp
262400050711      * Controllo se è stato immesso un codice di tassazione come ricerca
262500050711     c                   Clear                   korp
262600050711     c                   If        v3ccts <> *Blanks
262700050711     c                   Eval      w002a = v3ccts
262800050711     c                   ExSr      Sr_Chkcts
262900120712     c                   eval      v3ccts = w002a
263000050711     c   28              Clear                   v3ccts
263100050712     c  n28              Move(p)   §ctcor        korp
263200050711     c                   EndIf
263300050711     c                   Goto      Cars03
263400050711   x1c                   Else
263500050711     c                   Eval      *In17 = *Off
263600050711    1c                   EndIf
263700050711
263800050711      * F1= Selezione totale
263900050711     c                   If        *Inka
264000050714     c                   ExSr      Sr_Selts03
264100050711     c                   Goto      Emis03
264200050919     c                   Else
264300050919     c                   Eval      wselt = *Off
264400050711     c                   EndIf
264500050909      * F3=Fine copia
264600050909     c                   If        *InKc
264700050909     c                   Leavesr
264800050711     c                   EndIf
264900050712      * F6= Conferma
265000050715if  1c                   If        *Inkf
265100050714     c                   ExSr      Sr_Opzs03
265200050714     c                   Goto      Emis03
265300050715    1c                   EndIf
265400050713      * Rollup
265500050714      * imposto l'ultima chiave del subfile caricato e carico nuova pagina
265600050714     c                   If        *In23
265700050714     c                   Eval      kksc = savksc
265800050714     c                   Eval      kctr = savctr
265900050714     c                   Eval      kprg = savprg
266000050714     c                   Eval      klnp = savlnp
266100050714     c                   Eval      korp = savorp
266200050714     c                   Eval      knaz = savnaz
266300050714     c                   Eval      kcap = savcap
266400050714     c                   Eval      ksgl = savsgl
266500050714     c                   ExSr      Sr_Setll03
266600050714      * Carico il subfile
266700050714     c                   ExSr      Sr_Cars03
266800050714     c                   Goto      Emis03
266900050714     c                   EndIf
267000050711      * Controllo il dettaglio tariffario da copiare che non esista già
267100050714     c                   ExSr      Sr_Opzs03
267200050712     c                   Goto      Emis03
267300050708
267400050708     c                   EndSr
267500050711
267600050711      *------------------------------------------------------------------------*
267700050711      * PULIZIA SUBFILE DETTAGLIO TARIFFARIO
267800050711      *------------------------------------------------------------------------*
267900050711     c     Sr_Puls03     BegSr
268000050711
268100050714     c                   Clear                   wmaxnrr
268200050714     c                   Clear                   savnrr
268300050714     c                   Clear                   nrr3
268400050714     c                   Eval      *In20 = *Off
268500050711     c                   Eval      *In21 = *Off
268600050711     c                   Write     ta24c03
268700050711     c                   Eval      *In21 = *On
268800050714     c                   Eval      *In20 = *On
268900050714
269000050714     c                   Eval      wpagsf3 = sflpag3
269100050711
269200050711     c                   EndSr
269300050714
269400050714      *------------------------------------------------------------------------*
269500050714      * POSIZIONAMENTO INIZIALE SU DETTAGLIO TARIFFARIO
269600050714      *------------------------------------------------------------------------*
269700050714     c     Sr_Setll03    BegSr
269800050714
269900050714     c                   Select
270000050714      * Leggo dettaglio tariffa/offerta da cui copiare
270100050714      * senza chiave di ricerca e no rollup
270200050909     c                   When      Not *In17 and Not *In23
270300050727     c  n03kTntam01      Setll     Titad04l
270400050727     c   03kTntam01      Setll     Tiofd01l
270500050714      * con chiave di ricerca
270600050714     c                   When      *In17
270700050805     c  n03kTitad01      Setll     Titad04l
270800050805     c   03kTitad01      Setll     Tiofd01l
270900050714      * con rollup
271000050714     c                   When      *In23
271100050727     c  n03kTitad04      Setgt     Titad04l
271200050727     c   03kTitad04      Setgt     Tiofd01l
271300050714     c                   EndSl
271400050714
271500050714     c                   EndSr
271600050711
271700050711      *------------------------------------------------------------------------*
271800050711      * CARICO SUBFILE DETTAGLIO TARIFFARIO
271900050711      *------------------------------------------------------------------------*
272000050711     c     Sr_Cars03     BegSr
272100050714
272200050714     c                   Clear                   savnrr
272300050714     c                   Eval      nrr3 = wmaxnrr
272400050714
272500050711do  1c                   Do        *Hival
272600050727     c  n03kTntam01      Reade     Titad04l                               31
272700050727     c   03kTntam01      Reade     Tiofd01l                               31
272800050714      * Fine file
272900050714     c                   If        *In31
273000050711     c                   Leave
273100050711     c                   EndIf
273200050714      * carico solo fino al n. max di record x pagina
273300050714     c                   If        savnrr >= wpagsf3
273400050714     c                   Leave
273500050714     c                   EndIf
273600050714      * escludo annullati
273700050711     c                   If        tadatb <> *Blanks
273800050711     c                   Iter
273900050711     c                   EndIf
274000050728      * Controllo le inclusioni/omissioni
274100050728     c                   ExSr      Sr_Seltad
274200050728     c                   If        wokrec = *On
274300050728     c                   Iter
274400050728     c                   EndIf
274500050711      * visulizzo 'M' x indicare presenza del minimo o del massimo
274600050711     c                   If        tadmax <> *Zeros or tadmin <> *Zeros
274700050727     c                   Eval      *In06 = *On
274800050711     c                   Else
274900050727     c                   Eval      *In06 = *Off
275000050711     c                   EndIf
275100050711      * imposto i campi del file a video
275200050711     c                   Eval      v3slnp = tadlnp
275300050711     c                   Eval      v3scts = tadcts
275400050711     c                   Eval      v3scap = tadcap
275500050711     c                   Eval      v3ssgl = tadsgl
275600050711     c                   Eval      v3sitr = taditr
275700050711     c                   Eval      v3sino = tadino
275800050711     c                   Eval      v3srpv = tadrpv
275900050711     c                   Eval      v3snaz = tadnaz
276000050711     c                   Eval      v3smin = tadmin
276100050711     c                   Eval      v3smax = tadmax
276200050711     c                   Eval      v3sorp = tadorp
276300050711      * decodifico codice tassazione
276400050711     c                   Eval      w002a = tadcts
276500050711     c                   ExSr      Sr_Chkcts
276600050711     c                   Eval      v3sdcts = §ctdes
276700050711      * decodifico nazione
276800050711     c                   Eval      w003a = tadnaz
276900050711     c                   ExSr      Sr_Chknaz
277000050711     c                   Eval      v3sdnaz = §15des
277100050920      * cerco la nuova linea di partenza
277200050920     c                   Eval      xx = 1
277300050920     c                   Move      tadlnp        w003a
277400050920     c     w003a         Lookup    sklnpo(xx)                             30
277500050920     c                   If        *In30
277600050920     c                   Move      sklnpn(xx)    v3slnpn
277700050920     c                   Else
277800050920     c                   Move      tadlnp        v3slnpn
277900050920     c                   EndIf
278000050714      * salvo i dati della chiave x prossimo caricamento
278100050714     c                   Eval      savksc = tadksc
278200050714     c                   Eval      savctr = tadctr
278300050714     c                   Eval      savprg = tadprg
278400050714     c                   Eval      savlnp = tadlnp
278500050714     c                   Eval      savorp = tadorp
278600050714     c                   Eval      savnaz = tadnaz
278700050714     c                   Eval      savcap = tadcap
278800050714     c                   Eval      savsgl = tadsgl
278900050711
279000050714     c                   Add       1             savnrr
279100050714     c                   Add       1             nrr3
279200050711     c                   Clear                   v3sopz
279300050711     c                   Write     ta24s03
279400050711    1c                   EndDo
279500050714
279600050714     c                   Eval      wmaxnrr = nrr3
279700050711
279800050711     c                   EndSr
279900050711
280000050711      *------------------------------------------------------------------------*
280100050711      * EMISSIONE SUBFILE DETTAGLIO TARIFFARIO
280200050711      *------------------------------------------------------------------------*
280300050711     c     Sr_Emis03     BegSr
280400050714
280500050714      * Mi posiziono al 1° record della pagina
280600050909      * se non ho rischiesto la selezione totale
280700050909     c                   If        wselt = *Off
280800050714     c     nrr3          Div       sflpag3       pagine
280900050714     c                   Mvr                     resto
281000050714     c     pagine        Mult      sflpag3       recsf3
281100050714     c                   If        resto > *Zeros
281200050714     c                   Add       1             recsf3
281300050714     c                   Else
281400050714     c                   Sub       sflpag3       recsf3
281500050714     c                   Add       1             recsf3
281600050714     c                   EndIf
281700050909      * se ho rischiesto la selezione totale mi posizione al primo record
281800050909      * caricato
281900050909     c                   Else
282000050909     c                   Z-add     1             recsf3
282100050909     c                   EndIf
282200050711
282300050711     c                   Write     ta24t02
282400050711     c                   Write     ta24z03
282500050711     c                   Exfmt     ta24c03
282600050711
282700050711     c                   Eval      *In28 = *Off
282800050711     c                   Clear                   v3cmsg
282900050711
283000050711     c                   EndSr
283100050711
283200050711      *------------------------------------------------------------------------*
283300050909      * SELEZIONE TOTALE DEL DETTAGLIO TARIFFARIO
283400050711      *------------------------------------------------------------------------*
283500050714     c     Sr_Selts03    BegSr
283600050909
283700050909     c                   Eval      wselt = *On
283800050909      * Pulisco il subfile xchè lo devo ricaricare ma questa volta carico tutta
283900050909      * la tariffa
284000050909     c                   ExSr      Sr_Puls03
284100050912      * Reimposto la chiave
284200090408     c  n03              move      v1ksco        kksc
284300101222     c   03              Eval      kksc = v1nrvot
284400050912     c                   Move      v1ctro        kctr
284500050912     c                   Move      v1prgo        kprg
284600050909      * Leggo dettaglio tariffa/offerta da cui copiare
284700050909     c                   ExSr      Sr_Setll03
284800050909
284900050909     c                   Clear                   savnrr
285000050909
285100050909do  1c                   Do        *Hival
285200050909     c  n03kTntam01      Reade     Titad04l                               31
285300050909     c   03kTntam01      Reade     Tiofd01l                               31
285400050909      * Fine file
285500050909     c                   If        *In31
285600050909     c                   Leave
285700050909     c                   EndIf
285800050909      * escludo annullati
285900050909     c                   If        tadatb <> *Blanks
286000050909     c                   Iter
286100050909     c                   EndIf
286200050909      * Controllo le inclusioni/omissioni
286300050909     c                   ExSr      Sr_Seltad
286400050909     c                   If        wokrec = *On
286500050909     c                   Iter
286600050909     c                   EndIf
286700050909      * visulizzo 'M' x indicare presenza del minimo o del massimo
286800050909     c                   If        tadmax <> *Zeros or tadmin <> *Zeros
286900050909     c                   Eval      *In06 = *On
287000050909     c                   Else
287100050909     c                   Eval      *In06 = *Off
287200050909     c                   EndIf
287300050909      * imposto i campi del file a video
287400050909     c                   Eval      v3slnp = tadlnp
287500050909     c                   Eval      v3scts = tadcts
287600050909     c                   Eval      v3scap = tadcap
287700050909     c                   Eval      v3ssgl = tadsgl
287800050909     c                   Eval      v3sitr = taditr
287900050909     c                   Eval      v3sino = tadino
288000050909     c                   Eval      v3srpv = tadrpv
288100050909     c                   Eval      v3snaz = tadnaz
288200050909     c                   Eval      v3smin = tadmin
288300050909     c                   Eval      v3smax = tadmax
288400050909     c                   Eval      v3sorp = tadorp
288500050909      * decodifico codice tassazione
288600050909     c                   Eval      w002a = tadcts
288700050909     c                   ExSr      Sr_Chkcts
288800050909     c                   Eval      v3sdcts = §ctdes
288900050909      * decodifico nazione
289000050909     c                   Eval      w003a = tadnaz
289100050909     c                   ExSr      Sr_Chknaz
289200050909     c                   Eval      v3sdnaz = §15des
289300050930      * cerco la nuova linea di partenza
289400050930     c                   Eval      xx = 1
289500050930     c                   Move      tadlnp        w003a
289600050930     c     w003a         Lookup    sklnpo(xx)                             30
289700050930     c                   If        *In30
289800050930     c                   Move      sklnpn(xx)    v3slnpn
289900050930     c                   Else
290000050930     c                   Move      tadlnp        v3slnpn
290100050930     c                   EndIf
290200050909      * salvo i dati della chiave x prossimo caricamento
290300050909     c                   Eval      savksc = tadksc
290400050909     c                   Eval      savctr = tadctr
290500050909     c                   Eval      savprg = tadprg
290600050909     c                   Eval      savlnp = tadlnp
290700050909     c                   Eval      savorp = tadorp
290800050909     c                   Eval      savnaz = tadnaz
290900050909     c                   Eval      savcap = tadcap
291000050909     c                   Eval      savsgl = tadsgl
291100050909
291200050909     c                   Add       1             savnrr
291300050909     c                   Add       1             nrr3
291400050909     c                   Eval      v3sopz = '3'
291500050909     c                   Write     ta24s03
291600050909    1c                   EndDo
291700050909
291800050909     c                   Eval      wmaxnrr = nrr3
291900050711
292000050711     c                   EndSr
292100050708
292200050714      *------------------------------------------------------------------------*
292300050714      * LEGGO IL SUBFILE DETTAGLIO TARIFFARIO
292400050714      *------------------------------------------------------------------------*
292500050714     c     Sr_Opzs03     BegSr
292600050714
292700050909     c                   Clear                   nrr3
292800050919     c                   Eval      recsf3 = 1
292900050714do  1c                   Do        *Hival
293000050909     c                   Add       1             nrr3
293100050909     c     nrr3          Chain     ta24s03                            32
293200050714      * esco se fine sfl
293300050714     c                   If        *In32
293400050715     c                   Leave
293500050714     c                   EndIf
293600050714
293700050714      * riposiziono il cursore sull'ultimo record modificato
293800050912     c                   If        v3sopz <> *Blanks
293900050909     c                   Eval      recsf3 = nrr3
294000050912     c                   EndIf
294100050909
294200050909     c                   setoff                                       18
294300050714
294400050714      * Controllo
294500050908      * ma solo se non è una nuova tariffa o se non è il primo rcd che copio
294600050908      * sulla nuova tariffa
294700050908if  2c                   If        v3sopz = '3' and v1prgn <> *Blanks
294800090408     c  n01              move      v1kscn        kksc
294900101222     c   01              Eval      kksc = v1nrvnt
295000050714     c                   Move      v1ctrn        kctr
295100050817     c                   Move      v1prgn        kprg
295200050920     c                   Eval      klnp = v3slnpn
295300050714     c                   Eval      korp = v3sorp
295400050714     c                   Eval      knaz = v3snaz
295500050714     c                   Eval      kcap = v3scap
295600050714     c                   Eval      ksgl = v3ssgl
295700050729     c  n01kTitad04      Chain     Titad04l
295800050729     c   01kTitad04      Chain     Tiofd01l
295900050922if  3c                   If        %Found and tadatb = *Blanks
296000050714     c                   Eval      *In18 = *On
296100050714     c                   Eval      *In28 = *On
296200050728     c                   Eval      v3cmsg = msg(40)
296300050714     c                   Clear                   v3sopz
296400050714     c                   Update    ta24s03
296500050714     c                   Leave
296600050714    3c                   EndIf
296700050714    2c                   EndIf
296800050714      * Copio
296900050714if  2c                   If        *Inkf and v3sopz = '3'
297000050714     c                   ExSr      Sr_Cards
297100050714     c                   ExSr      Sr_Cardstad
297200050727     c                   If        Not *In04 and wprg = *Blanks
297300050714     c                   Eval      ta25tam = 'S'
297400050714     c                   EndIf
297500050714     c                   Eval      ta25tad = 'D'
297600050714     c                   Eval      ta25tpt = 'N'
297700050714     c                   Eval      ta25tpd = 'N'
297800050714     c                   Eval      ta25tgc = 'N'
297900050714     c                   Eval      ta25cat = 'N'
298000050714     c                   ExSr      Sr_Copia
298100050729     c                   Eval      savtad = ta25tad
298200050729     c                   Eval      savtpt = ta25tpt
298300050729     c                   Eval      savtgc = ta25tgc
298400050729     c                   Eval      savcat = ta25cat
298500050805     c                   If        wprg <> *Blanks
298600050805     c                   Move      wprg          v1prgn
298700050805     c                   EndIf
298800050714if  3c                   If        ta25err <> *Blanks
298900050714     c                   Eval      *In28 = *On
299000050714     c                   Eval      v3cmsg = ta25msg
299100050729     c                   Clear                   v3sopz
299200050714     c                   Update    ta24s03
299300050913      * pulisco il flag e il msg di errore della ds
299400050913     c                   Clear                   ta25err
299500050913     c                   Clear                   ta25msg
299600050714     c                   Leave
299700050714    3c                   EndIf
299800050714     c                   Clear                   v3sopz
299900050715     c                   Eval      wcopiatad = *On
300000050729     c                   Eval      savtad = 'D'
300100050714    2c                   EndIf
300200050714
300300050714     c                   Update    ta24s03
300400050714    1c                   EndDo
300500050714
300600050919     c   32              Eval      nrr3 = recsf3
300700050714     c   32              Eval      wpagsf3 = sflpag3
300800050919
300900050714
301000050714     c                   EndSr
301100050714
301200050708      *------------------------------------------------------------------------*
301300050708      * VISUALIZZA LE TARIFFE PARTICOLARI
301400050708      *------------------------------------------------------------------------*
301500050708     c     Sr_Titpt      BegSr
301600050727
301700050727     c                   Eval      wcopiatpt = *Off
301800050805     c                   Eval      wcopiatpd = *Off
301900050805     c                   Eval      woktpt = *Off
302000050727
302100050727      * Pulisco il subfile
302200050727     c                   ExSr      Sr_Puls04
302300050727      * Leggo tariffe particolari/offerta da cui copiare
302400090408     c  n03              move      v1ksco        kksc
302500101222     c   03              Eval      kksc = v1nrvot
302600050727     c                   Move      v1ctro        kctr
302700050727     c                   Move      v1prgo        kprg
302800050727     c                   ExSr      Sr_Setll04
302900050727      * Carico il subfile
303000050727     c                   ExSr      Sr_Cars04
303100050728      * se non ho caricato nessuna tariffa particolare emetto errore
303200050728      * e torno alla seconda videata
303300050728     c                   If        nrr4 = *Zeros
303400050728     c                   Eval      *In28 = *On
303500050909     c                   Eval      v1cmsg = msg(44)
303600050909     c                   Eval      %subst(v1cmsg:32:12) = 'visualizzare'
303700050909     c                   Goto      Emid01
303800050728     c                   EndIf
303900050727      * Emetto il subfile
304000050727     c     Emis04        Tag
304100050727     c                   ExSr      Sr_Emis04
304200050727
304300050727      * F1= Selezione totale
304400050727     c                   If        *Inka
304500050727     c                   ExSr      Sr_Selts04
304600050727     c                   Goto      Emis04
304700050727     c                   EndIf
304800050909      * F3=Fine copia
304900050909     c                   If        *InKc
305000050909     c                   Leavesr
305100050727     c                   EndIf
305200050727      * F6= Conferma
305300050727if  1c                   If        *Inkf
305400050912     c                   ExSr      Sr_Copia04
305500050727     c                   Goto      Emis04
305600050727    1c                   EndIf
305700050912      * Controllo la scelta fatta sulla tariffa particolare
305800050727     c                   ExSr      Sr_Opzs04
305900050912     c                   Goto      Emis04
306000050727
306100050727     c                   EndSr
306200050727
306300050727      *------------------------------------------------------------------------*
306400050727      * PULIZIA SUBFILE TARIFFE PARTICOLARI
306500050727      *------------------------------------------------------------------------*
306600050727     c     Sr_Puls04     BegSr
306700050727
306800050727     c                   Clear                   nrr4
306900050727     c                   Eval      *In20 = *Off
307000050727     c                   Eval      *In21 = *Off
307100050727     c                   Write     ta24c04
307200050727     c                   Eval      *In21 = *On
307300050727     c                   Eval      *In20 = *On
307400050727
307500050912     c                   Eval      recsf4 = 1
307600050727
307700050727     c                   EndSr
307800050727
307900050727      *------------------------------------------------------------------------*
308000050727      * POSIZIONAMENTO INIZIALE SULLE TARIFFE PARTICOLARI
308100050727      *------------------------------------------------------------------------*
308200050727     c     Sr_Setll04    BegSr
308300050727
308400050727      * Leggo tariffe particolari/offerta da cui copiare
308500050727     c  n03kTntam01      Setll     Titpt01l
308600050727     c   03kTntam01      Setll     Tiopt01l
308700050727
308800050727     c                   EndSr
308900050727
309000050727      *------------------------------------------------------------------------*
309100050727      * CARICO SUBFILE TARIFFE PARTICOLARI
309200050727      *------------------------------------------------------------------------*
309300050727     c     Sr_Cars04     BegSr
309400050727
309500050912     c                   Clear                   nrr4
309600050727
309700050727do  1c                   Do        *Hival
309800050727     c  n03kTntam01      Reade     Titpt01l                               31
309900050727     c   03kTntam01      Reade     Tiopt01l                               31
310000050727      * Fine file
310100050727     c                   If        *In31
310200050727     c                   Leave
310300050727     c                   EndIf
310400050727      * escludo annullati
310500050727     c                   If        tptatb <> *Blanks
310600050727     c                   Iter
310700050727     c                   EndIf
310800050728      * escludo le tariffe particolari in scadenza
310900050728     c                   Eval      w002a = tptftc
311000050728     c                   ExSr      Sr_Chktpt
311100050728     c                   If        wftceli = *On
311200050728     c                   Iter
311300050728     c                   EndIf
311400080609      * escludo se tariffa particolare 'c ' lasciato avviso
311500080609      * e non è copia verso cartello e non è un cliente realtivo ad
311600080609      * una delle filiali abilitate
311700080609     c                   If        tptftc = 'c ' and wnewcar = *Off
311800080609     c                   if        %lookup(comlnp:sklav) = *zeros
311900080609     c                             and %lookup(999:sklav) = *zeros
312000051019     c                   Iter
312100080609     c                   endif
312200051019     c                   EndIf
312300060307      * escludo AC BASE se tariffa a cui copiare ha rinuncia AC BASE
312400060307     c                   If        tptftc = 'd ' and savfmp = 'S'
312500060307     c                   Iter
312600060307     c                   EndIf
312700060705      * escludo
312800060705      * se tariffa particolare 'd ' AC BASE della tariffa da cui copiare
312900060705      * ha l'importo più basso rispetto a quanto dovrebbe essere impostato
313000060705      * per la tariffa a cui copiare
313100060705     c                   if        tptftc = 'd '
313200060705     c                   eval      kftc = tptftc
313300060705     c  n03ktitpt        chain     titpd01l
313400060705     c   03ktitpt        chain     tiopd01l
313500060705     c                   if        %found
313600060705     c                   select
313700060705     c                   when      savfabitra <> *zeros and tpditr < savfabitra
313800060307     c                   iter
313900120112      * se il codice da cui copio  è presente in tabella ed è diverso dalla
314000120112      * forzatura inserita nella tabella del codice cliente a cui copio errore
314100120112     c                   when      savfabitra <> oldfabitra and oldfabitra > 0
314200120112     c                   eval      wacbase = *on
314300120112      *** In base ai nuovi valori fissati il 30/11/2011 dv 2191 l'ac base
314400120112      *** avrà dei nuovi valori in base alla categoria dei clienti ma solo
314500120112      *** se si tratta di copia su offerta di una trattativa di tipo
314600120112      *** "NUOVO" indicatore 54 acceso x il resto non ci sono controlli
314700140128
314800140128     c                   other
314900140128       //?Controllo con i valori presenti sul file TIACB
315000140128     c     kACB          setll     TIACB01L
315100140128     c     savclva       reade     TIACB01L
315200140128     c                   if        %found(TIACB01L) and
315300140128     c                             TPDitr < ACBitr and *in54
315400140128     c                   iter
315500140128     c                   ENDIF
315600060705     c                   endsl
315700060307     c                   endif
315800060705     c                   endif
315900050727      * imposto i campi del file a video
316000050727     c                   Eval      v4sftc = tptftc
316100050727     c                   Eval      v4stpg = tpttpg
316200050727     c                   Eval      v4sarl = tptarl
316300050727     c                   Eval      v4sarf = tptarf
316400050727     c                   Eval      v4saro = tptaro
316500050727     c                   Eval      v4srpv = tptrpv
316600050727     c                   Eval      v4svlm = tptvlm
316700050727     c                   Eval      v4svvm = tptvvm
316800050727     c                   Eval      v4sfvm = tptfvm
316900050727     c                   Eval      v4stma = tpttma
317000050727     c                   Eval      v4sapl = tptapl
317100050727      * decodifico codice tariffa
317200050727     c                   Eval      v4sdftc = §1pdes
317300050727      * decodifico tipo tariffa
317400050727     c                   Eval      w001a = tpttpg
317500050727     c                   ExSr      Sr_Chkctr
317600050727     c                   Eval      v4sdtpg = §trdes
317700050727      * giro la data ultima variazione
317800050727     c     *iso          Move      tptduv        datadmy
317900050727     c                   Move      datadmy       v4sduv
318000060802      * giro la data riferimento fuel
318100060803     c                   if        tptdpb > *zeros
318200060802     c     *iso          move      tptdpb        datadmy
318300060802     c                   move      datadmy       v4sdpb
318400060803     c                   else
318500060803     c                   clear                   v4sdpb
318600060803     c                   endif
318700060803      * data inserimento tariffa particolare
318800060803     c                   eval      v4sdit = tptdit
318900060802      * imposto anche il flo..
319000060802     c                   eval      v4sflo = tptflo
319100150219     c                   eval      dTPT01 = V4sflo
319200150219     c                   IF        *in35 and v4sftc = 'f '
319300150219     c                   eval      §TPTvma = 'N'
319400150219     c                   ELSE
319500150219     c                   clear                   §TPTvma
319600150219     c                   ENDIF
319700150219     c                   eval      V4sflo = dTPT01
319800050727
319900050727     c                   Add       1             nrr4
320000050727     c                   Clear                   v4sopz
320100050727     c                   Write     ta24s04
320200050727    1c                   EndDo
320300050727
320400050727     c                   EndSr
320500050727
320600050727      *------------------------------------------------------------------------*
320700050727      * EMISSIONE SUBFILE TARIFFE PARTICOLARI
320800050727      *------------------------------------------------------------------------*
320900050727     c     Sr_Emis04     BegSr
321000050727
321100050912     c                   If        recsf4 > nrr4
321200050912     c                   Z-add     1             recsf4
321300050912     c                   EndIf
321400050912
321500050727     c                   Write     ta24t02
321600050913     c                   Write     ta24z04
321700050727     c                   Exfmt     ta24c04
321800050727
321900050727     c                   Eval      *In28 = *Off
322000050727     c                   Clear                   v4cmsg
322100050727
322200050727     c                   EndSr
322300050727
322400050727      *------------------------------------------------------------------------*
322500050727      * SELEZIONE TOTALE SUL SUBFILE CARICATO DELLE TARIFFE PARTICOLARI
322600050727      *------------------------------------------------------------------------*
322700050727     c     Sr_Selts04    BegSr
322800050727
322900050727     c                   Clear                   wnrr
323000050727do  1c                   Do        *Hival
323100050727     c                   Add       1             wnrr
323200050727     c     wnrr          Chain     ta24s04
323300050727     c                   If        Not %Found
323400050727     c                   Leave
323500050727     c                   EndIf
323600050727     c                   Eval      v4sopz = '3'
323700050727     c                   Update    ta24s04
323800050727    1c                   EndDo
323900050912
324000050912     c                   Clear                   nrr4
324100050727
324200050727     c                   EndSr
324300050727
324400050727      *------------------------------------------------------------------------*
324500050727      * LEGGO IL SUBFILE TARIFFE PARTICOLARI
324600050727      *------------------------------------------------------------------------*
324700050727     c     Sr_Opzs04     BegSr
324800050727
324900050912     c                   Clear                   nrr4
325000050727do  1c                   Do        *Hival
325100050912     c                   Add       1             nrr4
325200050912     c     nrr4          Chain     ta24s04                            32
325300050727      * esco se fine sfl
325400050727     c                   If        *In32
325500050727     c                   Leave
325600050727     c                   EndIf
325700050805
325800050805     c                   Eval      woktpt = *Off
325900050727
326000050727      * riposiziono il cursore sull'ultimo record modificato
326100050912     c                   If        v4sopz <> *Blanks
326200050727     c                   Eval      recsf4 = nrr4
326300050912     c                   EndIf
326400050912
326500050912     c                   setoff                                       18
326600050727
326700050727      * Scelta per entrare nel dettaglio
326800050727if  2c                   If        v4sopz = '1'
326900050728     c                   Eval      *In10 = *Off
327000050727     c                   ExSr      Sr_Titpd
327100050729     c                   Clear                   v4sopz
327200050729      * nessun rcd trovato
327300050729if  3c                   If        *In28
327400050729     c                   Eval      *In18 = *On
327500050729     c                   Eval      v4cmsg = msg(45)
327600050729     c                   Update    ta24s04
327700050912     c                   Leave
327800050912    3c                   EndIf
327900050727    2c                   EndIf
328000050727
328100050727      * Visualizza
328200050727if  2c                   If        v4sopz = '5'
328300050728     c                   Eval      *In10 = *On
328400050727     c                   ExSr      Sr_Titpd
328500050729     c                   Clear                   v4sopz
328600050729      * nessun rcd trovato
328700050729if  3c                   If        *In28
328800050729     c                   Eval      *In18 = *On
328900050729     c                   Eval      v4cmsg = msg(45)
329000050728     c                   Update    ta24s04
329100050912     c                   Leave
329200050729    3c                   EndIf
329300050727    2c                   EndIf
329400050727
329500050727      * Controllo
329600050908      * ma solo se non è una nuova tariffa o se non è il primo rcd che copio
329700050908      * sulla nuova tariffa
329800050908if  2c                   If        v4sopz = '3' and v1prgn <> *Blanks
329900090408     c  n01              move      v1kscn        kksc
330000101222     c   01              Eval      kksc = v1nrvnt
330100050727     c                   Move      v1ctrn        kctr
330200050817     c                   Move      v1prgn        kprg
330300050728     c                   Eval      kftc = v4sftc
330400050729     c  n01kTitpt        Chain     Titpt01l
330500050729     c   01kTitpt        Chain     Tiopt01l
330600050922if  3c                   If        %Found and tptatb = *Blanks
330700050727     c                   Eval      *In18 = *On
330800050727     c                   Eval      *In28 = *On
330900050728     c                   Eval      v4cmsg = msg(41)
331000050727     c                   Clear                   v4sopz
331100050727     c                   Update    ta24s04
331200050727     c                   Leave
331300050727    3c                   EndIf
331400050727    2c                   EndIf
331500060307      * se sto copiando la tariffa particolare AC BASE controllo se nella
331600060307      * tariffa a cui copiare è presente la tariffa particola AC PLUS reale e
331700060307      * con applicazione completa
331800060307if  2c                   if        v4sopz = '3' and v4sftc = 'd '
331900090408     c  n01              move      v1kscn        kksc
332000101222     c   01              eval      kksc = v1nrvnt
332100060307     c                   move      v1ctrn        kctr
332200060307     c                   move      v1prgn        kprg
332300060307     c                   eval      kftc = 'C '
332400060307     c  n01kTitpt        chain     titpt01l
332500060307     c   01kTitpt        chain     tiopt01l
332600060307if  3c                   if        %found and tptatb = *blanks and
332700060307     c                             tpttma = *blanks and tptapl = *blanks
332800060307     c                   eval      *in18 = *on
332900060307     c                   eval      *in28 = *on
333000060307     c                   eval      v4cmsg = msg(55)
333100060307     c                   clear                   v4sopz
333200060307     c                   update    ta24s04
333300060307     c                   leave
333400060307    3c                   endif
333500060307    2c                   endif
333600060307      * se sto copiando la tariffa particolare AC PLUS reale con applicazione
333700060307      * completa controllo se nella tariffa a cui copiare è presente la tariffa
333800060307      * particolare AC BASE
333900060307if  2c                   if        v4sopz = '3' and v4sftc = 'C ' and
334000060307     c                             v4stma = *blanks and v4sapl = *blanks
334100090408     c  n01              move      v1kscn        kksc
334200101222     c   01              eval      kksc = v1nrvnt
334300060307     c                   move      v1ctrn        kctr
334400060307     c                   move      v1prgn        kprg
334500060307     c                   eval      kftc = 'd '
334600060307     c  n01kTitpt        chain     titpt01l
334700060307     c   01kTitpt        chain     tiopt01l
334800060307if  3c                   if        %found and tptatb = *blanks
334900060307     c                   eval      *in18 = *on
335000060307     c                   eval      *in28 = *on
335100060307     c                   eval      v4cmsg = msg(56)
335200060307     c                   clear                   v4sopz
335300060307     c                   update    ta24s04
335400060307     c                   leave
335500060307    3c                   endif
335600060307    2c                   endif
335700050727
335800050727     c                   Update    ta24s04
335900050727    1c                   EndDo
336000050727
336100050727     c                   EndSr
336200050912
336300050912      *------------------------------------------------------------------------*
336400050912      * COPIO LE TARIFFE PARTICOLARI
336500050912      *------------------------------------------------------------------------*
336600050912     c     Sr_Copia04    BegSr
336700050912
336800050912     c                   Clear                   nrr4
336900050912do  1c                   Do        *Hival
337000050912     c                   Add       1             nrr4
337100050912     c     nrr4          Chain     ta24s04                            32
337200050912      * esco se fine sfl
337300050912     c                   If        *In32
337400050912     c                   Leave
337500050912     c                   EndIf
337600050912
337700050912     c                   Eval      woktpt = *Off
337800050912
337900050912     c                   setoff                                       18
338000050912      * Copio
338100050912if  2c                   If        v4sopz = '3'
338200050912     c                   Clear                   v4sopz
338300050912     c                   Eval      recsf4 = nrr4
338400050912     c                   ExSr      Sr_Cards
338500050912     c                   ExSr      Sr_Cardstpt
338600050912     c                   If        Not *In04 and wprg = *Blanks
338700050912     c                   Eval      ta25tam = 'S'
338800050912     c                   EndIf
338900050912     c                   Eval      ta25tad = 'N'
339000050912     c                   Eval      ta25tpt = 'D'
339100050912     c                   Eval      ta25tpd = 'N'
339200050912     c                   Eval      ta25tgc = 'N'
339300050912     c                   Eval      ta25cat = 'N'
339400050912     c                   ExSr      Sr_Copia
339500050912     c                   Eval      savtpt = ta25tpt
339600050912     c                   Eval      savtgc = ta25tgc
339700050912     c                   Eval      savcat = ta25cat
339800050912     c                   If        wprg <> *Blanks
339900050912     c                   Move      wprg          v1prgn
340000050912     c                   EndIf
340100050912if  3c                   If        ta25err <> *Blanks
340200050912     c                   Eval      *In18 = *On
340300050912     c                   Eval      *In28 = *On
340400050912     c                   Eval      v4cmsg = ta25msg
340500050912     c                   Update    ta24s04
340600050913      * pulisco il flag e il msg di errore della ds
340700050913     c                   Clear                   ta25err
340800050913     c                   Clear                   ta25msg
340900050912     c                   Leave
341000050912    3c                   EndIf
341100050912     c                   Eval      wcopiatpt = *On
341200050912     c                   Eval      savtpt = 'D'
341300050912    2c                   EndIf
341400050912
341500050912     c                   Update    ta24s04
341600050912    1c                   EndDo
341700050912
341800050912     c                   EndSr
341900050727
342000050727      *------------------------------------------------------------------------*
342100050727      * VISUALIZZA DETTAGLIO TARIFFE PARTICOLARI
342200050727      *------------------------------------------------------------------------*
342300050727     c     Sr_Titpd      BegSr
342400050727
342500050805     c                   Eval      woktpt = *Off
342600050727
342700050727      * Pulisco il subfile
342800050727     c                   ExSr      Sr_Puls05
342900050729      * Controllo la tariffa particolare
343000050729     c                   Eval      w002a = v4sftc
343100050729     c                   ExSr      Sr_Chktpt
343200050727      * Leggo tariffe particolari/offerta da cui copiare
343300090408     c  n03              move      v1ksco        kksc
343400101222     c   03              Eval      kksc = v1nrvot
343500050727     c                   Move      v1ctro        kctr
343600050727     c                   Move      v1prgo        kprg
343700050727     c                   Move      v4sftc        kftc
343800050727     c                   ExSr      Sr_Setll05
343900050727      * Carico il subfile
344000050727     c                   ExSr      Sr_Cars05
344100050728      * se non ho caricato nessun dettaglio tariffario emetto errore
344200050728      * e torno al subfile delle tariffe particolari
344300050729     c                   If        nrr5 = *Zeros
344400050728     c                   Eval      *In28 = *On
344500050728     c                   Leavesr
344600050728     c                   EndIf
344700050727      * Imposto i campi della tariffa particolare
344800050727     c                   ExSr      Sr_Carc05
344900050727      * Emetto il subfile
345000050727     c     Emis05        Tag
345100050727     c                   ExSr      Sr_Emis05
345200050727
345300050727      * F1= Selezione totale
345400050727     c                   If        *Inka
345500050727     c                   ExSr      Sr_Selts05
345600050727     c                   Goto      Emis05
345700050727     c                   EndIf
345800050913      * F12=Fine copia
345900050913     c                   If        *InKl
346000050912     c                   Leavesr
346100050912     c                   EndIf
346200050727      * F6= Conferma
346300050727if  1c                   If        *Inkf
346400050727     c                   ExSr      Sr_Opzs05
346500050727     c                   Goto      Emis05
346600050727    1c                   EndIf
346700050727      * Controllo il dett. della tariffa partic. da copiare che non esista già
346800050727     c                   ExSr      Sr_Opzs05
346900050727     c                   Goto      Emis05
347000050727
347100050727     c                   EndSr
347200050727
347300050727      *------------------------------------------------------------------------*
347400050727      * PULIZIA SUBFILE DETTAGLIO TARIFFE PARTICOLARI
347500050727      *------------------------------------------------------------------------*
347600050727     c     Sr_Puls05     BegSr
347700050727
347800050727     c                   Clear                   nrr5
347900050727     c                   Eval      *In20 = *Off
348000050727     c                   Eval      *In21 = *Off
348100050727     c                   Write     ta24c05
348200050727     c                   Eval      *In21 = *On
348300050727     c                   Eval      *In20 = *On
348400050912
348500050912     c                   Eval      recsf5 = 1
348600050727
348700050727     c                   EndSr
348800050727
348900050727      *------------------------------------------------------------------------*
349000050727      * POSIZIONAMENTO INIZIALE SUL DETTAGLIO TARIFFE PARTICOLARI
349100050727      *------------------------------------------------------------------------*
349200050727     c     Sr_Setll05    BegSr
349300050727
349400050727      * Leggo tariffe particolari/offerta da cui copiare
349500050727     c  n03kTitpt        Setll     Titpd01l
349600050727     c   03kTitpt        Setll     Tiopd01l
349700050727
349800050727     c                   EndSr
349900050727
350000050727      *------------------------------------------------------------------------*
350100050727      * CARICO SUBFILE DETTAGLIO TARIFFE PARTICOLARI
350200050727      *------------------------------------------------------------------------*
350300050727     c     Sr_Cars05     BegSr
350400050727
350500050912     c                   Clear                   nrr5
350600050727
350700050727do  1c                   Do        *Hival
350800050727     c  n03kTitpt        Reade     Titpd01l                               33
350900050727     c   03kTitpt        Reade     Tiopd01l                               33
351000050727      * Fine file
351100050727     c                   If        *In33
351200050727     c                   Leave
351300050727     c                   EndIf
351400050727      * escludo annullati
351500050727     c                   If        tpdatb <> *Blanks
351600050727     c                   Iter
351700050728     c                   EndIf
351800050728      * Controllo le inclusioni/omissioni
351900050728     c                   ExSr      Sr_Seltpd
352000050728     c                   If        wokrec = *On
352100050728     c                   Iter
352200050728     c                   EndIf
352300050727      * imposto i campi del file a video
352400050727     c                   Eval      v5scts = tpdcts
352500050727     c                   Eval      v5sorp = tpdorp
352600050727     c                   Eval      v5ssgl = tpdsgl
352700050727     c                   Eval      v5sitr = tpditr
352800050727     c                   Eval      v5smin = tpdmin
352900050728     c                   Eval      v5smax = tpdmax
353000050728     c                   Eval      v5sain = tpdain
353100050729      * Imposto gli indicatori per visualizzare o meno i campi
353200050729     c                   Eval      *In25 = (§1pvmn = 'S')
353300050729     c                   Eval      *In26 = (§1pvmx = 'S')
353400050729     c                   Eval      *In27 = (§1pain = 'S')
353500050912      * decodifico il codice di tassazione
353600050912     c                   Eval      w002a = tpdcts
353700050912     c                   ExSr      Sr_Chkcts
353800050912     c                   Eval      v5sdcts = §ctdes
353900050727
354000050728     c                   Add       1             nrr5
354100050728     c                   Clear                   v5sopz
354200050728     c                   Write     ta24s05
354300050727    1c                   EndDo
354400050727
354500050727     c                   EndSr
354600050727
354700050727      *------------------------------------------------------------------------*
354800050727      * CARICO DATI DELLA TARIFFA PARTICOLARE
354900050727      *------------------------------------------------------------------------*
355000050727     c     Sr_Carc05     BegSr
355100050729
355200050729     c                   Eval      v5cftc = v4sftc
355300050729     c                   Eval      v5dftc = v4sdftc
355400050729     c                   Eval      v5ctpg = v4stpg
355500050729     c                   Eval      v5dtpg = v4sdtpg
355600050729
355700050729      * Imposto indicatori per visualizzare i campi a video
355800050729     c                   Eval      *In07 = (§1pasc = 'S')
355900050729     c                   Eval      *In09 = (§1prcv = 'S')
356000050729     c                   Eval      *In14 = (§1pvvm = 'S')
356100050729     c                   Eval      *In16 = (§1pvca = 'S')
356200050729     c                   Eval      *In15 = (v5cftc = 'C ')
356300060802     c                   Eval      *In24 = (§1pdrf = 'S')
356400050729
356500050729      * Controllo se la tariffa particolare
356600050729     c                   Eval      w001a = v5ctpg
356700050729     c                   ExSr      Sr_Chkctr
356800050729      * Imposto indicatori per visualizzare i campi a video
356900050729     c                   Eval      *In09 = (§trtap = 'S')
357000050729     c                   Eval      *In11 = (§trarr = 'S')
357100050729     c                   Eval      *In13 = (§trrpv = 'S')
357200050727
357300050727     c                   Eval      v5carl = v4sarl
357400050727     c                   Eval      v5carf = v4sarf
357500050727     c                   Eval      v5caro = v4saro
357600050727     c                   Eval      v5crpv = v4srpv
357700050727     c                   Eval      v5cvlm = v4svlm
357800050727     c                   Eval      v5cfvm = v4sfvm
357900050727     c                   Eval      v5ctma = v4stma
358000050727     c                   Eval      v5capl = v4sapl
358100050805     c                   Eval      v5cvvm = v4svvm
358200060802     c                   eval      v5cdpb = v4sdpb
358300060803      * data inserimento tariffa particolare
358400060803     c                   eval      v5cdit = v4sdit
358500060802      * imposto anche il flo..
358600060802     c                   eval      v5cflo = v4sflo
358700140227     c                   If        v5cftc = 'f '
358800140115      * FUEL
358900140115     c                   eval      dTPT01 = V4sflo
359000140115     c                   IF        %check(Digits:%subst(V4Sflo:1:
359100140115     c                             %len(§TPTpma))) = *zeros
359200140115     c                   eval      V5Cpma = §TPTpma
359300140115     c                   ENDIF
359400150217     c                   eval      V5Cvma = §TPTvma
359500140115      * recupero il prezzo del gasolio corrispondente
359600140227     c     *eur          move      V5Cdpb        dataymd
359700140227     c                   move      dataymd       kpmg
359800140227     c     kpmg          setll     tipmg01l
359900140115     c                   read      tipmg01l
360000140115     c                   if        %found(tipmg01l)
360100140115     c                   eval      V5Cpmg = PMGpmg
360200140227     c                   eval      V5Csca = PMGsca
360300140115     c                   endif
360400140227     c                   endif
360500050727
360600050727      * Decodifico il tipo valore
360700050729     c                   If        *In14 and v5cfvm <> *Blanks
360800050727     c                   Eval      w001a = v5cfvm
360900050727     c                   ExSr      Sr_Chkctr
361000050727     c                   Eval      v5dfvm = §trdes
361100050729     c                   EndIf
361200050727
361300050727     c                   EndSr
361400050727
361500050727      *------------------------------------------------------------------------*
361600050728      * EMISSIONE SUBFILE DETTAGLIO TARIFFE PARTICOLARI
361700050727      *------------------------------------------------------------------------*
361800050728     c     Sr_Emis05     BegSr
361900050912
362000050912     c                   If        recsf5 > nrr5
362100050912     c                   Z-add     1             recsf5
362200050912     c                   EndIf
362300050727
362400050727     c                   Write     ta24t02
362500050728     c                   Write     ta24d05
362600050913     c                   Write     ta24z05
362700050728     c                   Exfmt     ta24c05
362800050727
362900050727     c                   Eval      *In28 = *Off
363000050728     c                   Clear                   v5cmsg
363100050727
363200050727     c                   EndSr
363300050727
363400050727      *------------------------------------------------------------------------*
363500050728      * SELEZIONE TOTALE SUL SUBFILE CARICATO DEL DETTAGLIO DELLE TARIFFE PART.
363600050727      *------------------------------------------------------------------------*
363700050728     c     Sr_Selts05    BegSr
363800050727
363900050728     c                   Clear                   wnrr1
364000050727do  1c                   Do        *Hival
364100050728     c                   Add       1             wnrr1
364200050728     c     wnrr1         Chain     ta24s05
364300050727     c                   If        Not %Found
364400050727     c                   Leave
364500050727     c                   EndIf
364600050728     c                   Eval      v5sopz = '3'
364700050728     c                   Update    ta24s05
364800050727    1c                   EndDo
364900050727
365000050727     c                   EndSr
365100050727
365200050727      *------------------------------------------------------------------------*
365300050728      * LEGGO IL SUBFILE DEL DETTAGLIO DELLE TARIFFE PARTICOLARI
365400050727      *------------------------------------------------------------------------*
365500050728     c     Sr_Opzs05     BegSr
365600050727
365700050912     c                   Clear                   nrr5
365800050727do  1c                   Do        *Hival
365900050912     c                   Add       1             nrr5
366000050912     c     nrr5          Chain     ta24s05                            34
366100050727      * esco se fine sfl
366200050728     c                   If        *In34
366300050727     c                   Leave
366400050727     c                   EndIf
366500050727
366600050727      * riposiziono il cursore sull'ultimo record modificato
366700050912     c                   If        v5sopz <> *Blanks
366800050728     c                   Eval      recsf5 = nrr5
366900050912     c                   EndIf
367000050912
367100050912     c                   setoff                                       18
367200050727
367300050727      * Controllo
367400050908      * ma solo se non è una nuova tariffa o se non è il primo rcd che copio
367500050908      * sulla nuova tariffa
367600050908if  2c                   If        v5sopz = '3' and v1prgn <> *Blanks
367700090408     c  n01              move      v1kscn        kksc
367800101222     c   01              Eval      kksc = v1nrvnt
367900050727     c                   Move      v1ctrn        kctr
368000050817     c                   Move      v1prgn        kprg
368100050728     c                   Eval      kftc = v5cftc
368200050728     c                   Eval      korp = v5sorp
368300050728     c                   Eval      ksgl = v5ssgl
368400050729     c  n01kTitpd        Chain     Titpd01l
368500050729     c   01kTitpd        Chain     Tiopd01l
368600050817if  3c                   If        %Found and tpdatb = *Blanks
368700050727     c                   Eval      *In18 = *On
368800050727     c                   Eval      *In28 = *On
368900050728     c                   Eval      v5cmsg = msg(42)
369000050728     c                   Clear                   v5sopz
369100050728     c                   Update    ta24s05
369200050727     c                   Leave
369300050727    3c                   EndIf
369400050727    2c                   EndIf
369500060307      * se sto copiando la tariffa particolare AC BASE controllo se nella
369600060307      * tariffa a cui copiare è presente la tariffa particola AC PLUS reale e
369700060307      * con applicazione completa
369800060307if  2c                   if        v5sopz = '3' and v5cftc = 'd '
369900090408     c  n01              move      v1kscn        kksc
370000101222     c   01              eval      kksc = v1nrvnt
370100060307     c                   move      v1ctrn        kctr
370200060307     c                   move      v1prgn        kprg
370300060307     c                   eval      kftc = 'C '
370400060307     c  n01kTitpt        chain     titpt01l
370500060307     c   01kTitpt        chain     tiopt01l
370600060307if  3c                   if        %found and tptatb = *blanks and
370700060307     c                             tpttma = *blanks and tptapl = *blanks
370800060307     c                   eval      *in18 = *on
370900060307     c                   eval      *in28 = *on
371000060307     c                   eval      v5cmsg = msg(55)
371100060307     c                   clear                   v5sopz
371200060307     c                   update    ta24s05
371300060307     c                   leave
371400060307    3c                   endif
371500060307    2c                   endif
371600060307      * se sto copiando la tariffa particolare AC PLUS reale con applicazione
371700060307      * completa controllo se nella tariffa a cui copiare è presente la tariffa
371800060307      * particolare AC BASE
371900060307if  2c                   if        v5sopz = '3' and v5cftc = 'C ' and
372000060307     c                             v5ctma = *blanks and v5capl = *blanks
372100090408     c  n01              move      v1kscn        kksc
372200101222     c   01              eval      kksc = v1nrvnt
372300060307     c                   move      v1ctrn        kctr
372400060307     c                   move      v1prgn        kprg
372500060307     c                   eval      kftc = 'd '
372600060307     c  n01kTitpt        chain     titpt01l
372700060307     c   01kTitpt        chain     tiopt01l
372800060307if  3c                   if        %found and tptatb = *blanks
372900060307     c                   eval      *in18 = *on
373000060307     c                   eval      *in28 = *on
373100060307     c                   eval      v5cmsg = msg(56)
373200060307     c                   clear                   v5sopz
373300060307     c                   update    ta24s05
373400060307     c                   leave
373500060307    3c                   endif
373600060307    2c                   endif
373700050727      * Copio
373800050728if  2c                   If        *Inkf and v5sopz = '3'
373900050727     c                   ExSr      Sr_Cards
374000050805      * se la testata della tariffa particolare non esiste carico
374100050805      * anche la tnta25tpt
374200050908      * ma solo se non è una nuova tariffa o se non è il primo rcd che copio
374300050908      * sulla nuova tariffa
374400050908if  3c                   If        v1prgn <> *Blanks
374500090408     c  n01              move      v1kscn        kksc
374600101222     c   01              Eval      kksc = v1nrvnt
374700050805     c                   Move      v1ctrn        kctr
374800050817     c                   Move      v1prgn        kprg
374900050805     c                   Eval      kftc = v5cftc
375000050805     c  n01kTitpt        Chain     Titpt01l
375100050805     c   01kTitpt        Chain     Tiopt01l
375200050908if  4c                   If        Not %Found or tptatb = 'A'
375300050805     c                   Eval      woktpt = *On
375400050805     c                   ExSr      Sr_Cardstpt
375500050908   x4c                   Else
375600050817     c                   Eval      woktpt = *Off
375700050908    4c                   EndIf
375800050908      * se tariffa nuova la testata della tariffa di sicuro non esiste quindi
375900050908      * carico anche la tnta25tpt
376000050908   x3c                   Else
376100050908     c                   Eval      woktpt = *On
376200050908     c                   ExSr      Sr_Cardstpt
376300050908    3c                   EndIf
376400050728     c                   ExSr      Sr_Cardstpd
376500050727     c                   If        Not *In04 and wprg = *Blanks
376600050727     c                   Eval      ta25tam = 'S'
376700050727     c                   EndIf
376800050728     c                   Eval      ta25tad = 'N'
376900050805     c                   If        woktpt = *Off
377000050727     c                   Eval      ta25tpt = 'N'
377100050805     c                   Else
377200050805     c                   Eval      ta25tpt = 'D'
377300050805     c                   EndIf
377400050728     c                   Eval      ta25tpd = 'D'
377500050727     c                   Eval      ta25tgc = 'N'
377600050727     c                   Eval      ta25cat = 'N'
377700050727     c                   ExSr      Sr_Copia
377800050729     c                   Eval      savtgc = ta25tgc
377900050729     c                   Eval      savcat = ta25cat
378000050805     c                   If        wprg <> *Blanks
378100050805     c                   Move      wprg          v1prgn
378200050805     c                   EndIf
378300050727if  3c                   If        ta25err <> *Blanks
378400050912     c                   Eval      *In18 = *On
378500050727     c                   Eval      *In28 = *On
378600050728     c                   Eval      v5cmsg = ta25msg
378700050729     c                   Clear                   v5sopz
378800050728     c                   Update    ta24s05
378900050913      * pulisco il flag e il msg di errore della ds
379000050913     c                   Clear                   ta25err
379100050913     c                   Clear                   ta25msg
379200050727     c                   Leave
379300050727    3c                   EndIf
379400050728     c                   Clear                   v5sopz
379500050728     c                   Eval      wcopiatpd = *On
379600050729     c                   Eval      savtpt = 'D'
379700050727    2c                   EndIf
379800050727
379900050728     c                   Update    ta24s05
380000050727    1c                   EndDo
380100050727
380200050727     c                   EndSr
380300050727
380400050708      *------------------------------------------------------------------------*
380500050708      * VISUALIZZA LE TARIFFE DI GIACENZA
380600050708      *------------------------------------------------------------------------*
380700050708     c     Sr_Titgc      BegSr
380800050729
380900050729     c                   Eval      wcopiatgc = *Off
381000050729
381100050729     c                   Clear                   v6dtcm
381200050729     c                   Clear                   v6dtfg
381300050729
381400050729      * Leggo tariffe di giacenza/offerta da cui copiare
381500090408     c  n03              move      v1ksco        kksc
381600101222     c   03              Eval      kksc = v1nrvot
381700050729     c                   Move      v1ctro        kctr
381800050729     c                   Move      v1prgo        kprg
381900050729     c  n03kTntam01      Chain     Titgc01l
382000050729     c   03kTntam01      Chain     Tiogc01l
382100050729if  1c                   If        %Found
382200050729      * Carico i dati a video
382300050729     c                   Eval      v6csgv = tgcsgv
382400050729     c                   Eval      v6csgr = tgcsgr
382500050729     c                   Eval      v6csgd = tgcsgd
382600050729     c                   Eval      v6csgp = tgcsgp
382700050729     c                   Eval      v6csg1 = tgcsg1
382800050729     c                   Eval      v6csg2 = tgcsg2
382900050729     c                   Eval      v6csg3 = tgcsg3
383000050729     c                   Eval      v6cst1 = tgcst1
383100050729     c                   Eval      v6cst2 = tgcst2
383200050729     c                   Eval      v6cst3 = tgcst3
383300050729     c                   Eval      v6cstm = tgcstm
383400050729     c                   Eval      v6crpv = tgcrpv
383500050729     c                   Eval      v6cfaf = tgcfaf
383600050729     c                   Eval      v6csgf = tgcsgf
383700050729     c                   Eval      v6cggr = tgcggr
383800050729     c                   Eval      v6ctcm = tgctcm
383900050729     c                   Eval      v6ctfg = tgctfg
384000050729      * Decodifico il tipo comunicazione al mittente
384100050729     c                   Eval      w001a = v6ctcm
384200050729     c                   ExSr      Sr_Chktcm
384300050729     c                   Eval      v6dtcm = §2fdes
384400050729      * Decodifico il tipo comunicazione fine giacenza
384500050729     c                   Eval      w001a = v6ctfg
384600050729     c                   ExSr      Sr_Chktfg
384700050729     c                   Eval      v6dtfg = §2hdes
384800050729    1c                   EndIf
384900050729
385000050729      * Emetto la videata
385100050729     c     Emid06        Tag
385200050729     c                   Write     ta24t02
385300050729     c                   Exfmt     ta24d06
385400050729     c                   Eval      *In28 = *Off
385500050729     c                   Clear                   v6cmsg
385600050912      * F3=Fine copia
385700050912     c                   If        *InKc
385800050912     c                   Leavesr
385900050729     c                   EndIf
386000050729      * Controllo che non esista già la tariffa di giacenza
386100050908      * ma solo se non è una nuova tariffa o se non è il primo rcd che copio
386200050908      * sulla nuova tariffa
386300050908if  1c                   If        v1prgn <> *Blanks
386400090408     c  n01              move      v1kscn        kksc
386500101222     c   01              Eval      kksc = v1nrvnt
386600050729     c                   Move      v1ctrn        kctr
386700050817     c                   Move      v1prgn        kprg
386800050729     c  n01kTntam01      Chain     Titgc01l
386900050729     c   01kTntam01      Chain     Tiogc01l
387000060307if  2c                   If        %Found and tgcatb = *blanks
387100050729     c                   Eval      *In18 = *On
387200050729     c                   Eval      *In28 = *On
387300050729     c                   Eval      v6cmsg = msg(27)
387400050729     c                   Goto      emid06
387500050908    2c                   EndIf
387600050908    1c                   EndIf
387700050729      * F6= Conferma
387800050729if  1c                   If        *Inkf
387900050729     c                   ExSr      Sr_Cards
388000050729     c                   ExSr      Sr_Cardstgc
388100050729     c                   If        Not *In04 and wprg = *Blanks
388200050729     c                   Eval      ta25tam = 'S'
388300050729     c                   EndIf
388400050729     c                   Eval      ta25tad = 'N'
388500050729     c                   Eval      ta25tpt = 'N'
388600050729     c                   Eval      ta25tpd = 'N'
388700050729     c                   Eval      ta25tgc = 'D'
388800050729     c                   Eval      ta25cat = 'N'
388900050729     c                   ExSr      Sr_Copia
389000050729     c                   Eval      savtgc = ta25tgc
389100050729     c                   Eval      savcat = ta25cat
389200050805     c                   If        wprg <> *Blanks
389300050805     c                   Move      wprg          v1prgn
389400050805     c                   EndIf
389500050729if  2c                   If        ta25err <> *Blanks
389600050729     c                   Eval      *In28 = *On
389700050729     c                   Eval      v6cmsg = ta25msg
389800050913      * pulisco il flag e il msg di errore della ds
389900050913     c                   Clear                   ta25err
390000050913     c                   Clear                   ta25msg
390100050729     c                   Goto      emid06
390200050729    2c                   EndIf
390300050729     c                   Eval      wcopiatgc = *On
390400050729     c                   Eval      savtgc = 'D'
390500050729     c                   Leavesr
390600050729    1c                   EndIf
390700050729
390800050729      * Per enter riemetto la videata
390900050729     c                   Goto      emid06
391000050708
391100050708     c                   EndSr
391200050712
391300050712      *------------------------------------------------------------------------*
391400050712      * CARICO DATI NELLA DS X LA COPIA
391500050712      *------------------------------------------------------------------------*
391600050712     c     Sr_Cards      BegSr
391700050712
391800050712     c                   Clear                   Tnta25ds
391900050727     c  n03              Eval      ta25tipo = 'T'
392000050727     c   03              Eval      ta25tipo = 'O'
392100090408     c  n03              move      v1ksco        ta25ksco
392200101222     c   03              Eval      ta25ksco = v1nrvot
392300050712     c                   Move      v1ctro        ta25ctro
392400050712     c                   Move      v1prgo        ta25prgo
392500050727     c  n01              Eval      ta25tipn = 'T'
392600050727     c   01              Eval      ta25tipn = 'O'
392700090408     c  n01              move      v1kscn        ta25kscn
392800101222     c   01              Eval      ta25kscn = v1nrvnt
392900050712     c                   Move      v1ctrn        ta25ctrn
393000050727     c   04              Move      v1prgn        ta25prgn
393100050713     c                   Eval      ta25tam = 'N'
393200050909     c                   Eval      ta25fie = v1cfie
393300050909     c                   Eval      ta25sad = v1csad
393400050909     c                   Eval      ta25dec = v1cdec
393500050909     c                   Eval      ta25io2 = v1cio2
393600050920     c                   Movea     skcts         ta25cts
393700050909     c                   Eval      ta25io3 = v1cio3
393800050909     c                   Eval      ta25sgl = v1csgl
393900050801     c                   Eval      ta25inglo = 'N'
394000050919      * carico le vecchie linee di partenza e le nuove
394100050920     c                   Movea     sklnpo        ta25lnpo
394200050920     c                   Movea     sklnpn        ta25lnpn
394300050921      * carico la sk delle tariffe particolari da copiare
394400050921     c                   Movea     sktpt         ta25tpto
394500050714      * se ho già creato il nuovo progressivo lo passo
394600050817      * se non è copia su progressivo già esistente
394700050817     c                   If        Not *In04 and wprg <> *Blanks
394800050714     c                   Move      wprg          ta25prgn
394900050714     c                   EndIf
395000050712
395100050712     c                   EndSr
395200050712
395300050712      *------------------------------------------------------------------------*
395400050712      * CARICO DATI NELLA DS X LA COPIA DI TITAD
395500050712      *------------------------------------------------------------------------*
395600050712     c     Sr_Cardstad   BegSr
395700050712
395800050712     c                   Clear                   Tnta25tad
395900050712     c                   Clear                   Tnta25tgc
396000050712     c                   Clear                   Tnta25tpd
396100050712     c                   Clear                   Tnta25tpt
396200050712     c                   Eval      ta25lnpr = v3slnp
396300050712     c                   Eval      ta25ctsr = v3scts
396400050712     c                   Eval      ta25capr = v3scap
396500050712     c                   Eval      ta25sglr = v3ssgl
396600050712     c                   Eval      ta25itrr = v3sitr
396700050712     c                   Eval      ta25inor = v3sino
396800050712     c                   Eval      ta25rpvr = v3srpv
396900050712     c                   Eval      ta25nazr = v3snaz
397000050712     c                   Eval      ta25minr = v3smin
397100050712     c                   Eval      ta25maxr = v3smax
397200050712     c                   Eval      ta25orpr = v3sorp
397300050920     c                   Eval      ta25lnprn = v3slnpn
397400050712
397500050712     c                   EndSr
397600050727
397700050727      *------------------------------------------------------------------------*
397800050727      * CARICO DATI NELLA DS X LA COPIA DI TITPT
397900050727      *------------------------------------------------------------------------*
398000050727     c     Sr_Cardstpt   BegSr
398100050727
398200050727     c                   Clear                   Tnta25tad
398300050727     c                   Clear                   Tnta25tgc
398400050727     c                   Clear                   Tnta25tpd
398500050727     c                   Clear                   Tnta25tpt
398600050805      * se copia a dettaglio da subfile delle tariffe particolari
398700050805      * imposto i campi della subfile
398800050805     c                   If        woktpt = *Off
398900050727     c                   Eval      ta25ftcr = v4sftc
399000050727     c                   Eval      ta25tpgr = v4stpg
399100050727     c                   Eval      ta25arlr = v4sarl
399200050727     c                   Eval      ta25arfr = v4sarf
399300050727     c                   Eval      ta25aror = v4saro
399400050727     c                   Eval      ta25rpvrt = v4srpv
399500050727     c                   Eval      ta25vlmr = v4svlm
399600050727     c                   Eval      ta25vvmr = v4svvm
399700050727     c                   Eval      ta25fvmr = v4sfvm
399800050727     c                   Eval      ta25tmar = v4stma
399900050727     c                   Eval      ta25aplr = v4sapl
400000060803     c                   if        v4sdpb > *zeros
400100060802     c     *eur          move      v4sdpb        dataymd
400200060802     c                   move      dataymd       ta25dpbr
400300060803     c                   else
400400060803     c                   clear                   ta25dpbr
400500060803     c                   endif
400600060803     c                   eval      ta25ditr = v4sdit
400700060802     c                   eval      ta25flor = v4sflo
400800050805     c                   EndIf
400900050805      * se copia a dettaglio da subfile del dettaglio tariffe particolari
401000050817      * e la testata non è già presente imposto i campi della videata
401100050805     c                   If        woktpt = *On
401200050805     c                   Eval      ta25ftcr = v5cftc
401300050805     c                   Eval      ta25tpgr = v5ctpg
401400050805     c                   Eval      ta25arlr = v5carl
401500050805     c                   Eval      ta25arfr = v5carf
401600050805     c                   Eval      ta25aror = v5caro
401700050805     c                   Eval      ta25rpvrt = v5crpv
401800050805     c                   Eval      ta25vlmr = v5cvlm
401900050805     c                   Eval      ta25vvmr = v5cvvm
402000050805     c                   Eval      ta25fvmr = v5cfvm
402100050805     c                   Eval      ta25tmar = v5ctma
402200050805     c                   Eval      ta25aplr = v5capl
402300060803     c                   if        v5cdpb > *zeros
402400060802     c     *eur          move      v5cdpb        dataymd
402500060802     c                   move      dataymd       ta25dpbr
402600060803     c                   else
402700060803     c                   clear                   ta25dpbr
402800060803     c                   endif
402900060803     c                   eval      ta25ditr = v5cdit
403000060802     c                   eval      ta25flor = v5cflo
403100050805     c                   EndIf
403200050727
403300050727     c                   EndSr
403400050728
403500050728      *------------------------------------------------------------------------*
403600050728      * CARICO DATI NELLA DS X LA COPIA DI TITPD
403700050728      *------------------------------------------------------------------------*
403800050728     c     Sr_Cardstpd   BegSr
403900050728
404000050728     c                   Clear                   Tnta25tad
404100050728     c                   Clear                   Tnta25tgc
404200050728     c                   Clear                   Tnta25tpd
404300050805     c                   If        woktpt = *Off
404400050728     c                   Clear                   Tnta25tpt
404500050805     c                   EndIf
404600050728     c                   Eval      ta25ftcrd = v5cftc
404700050728     c                   Eval      ta25ctsrd = v5scts
404800050728     c                   Eval      ta25sglrd = v5ssgl
404900050728     c                   Eval      ta25itrrd = v5sitr
405000050728     c                   Eval      ta25orprd = v5sorp
405100050728     c                   Eval      ta25minrd = v5smin
405200050728     c                   Eval      ta25maxrd = v5smax
405300050728     c                   Eval      ta25ainr = v5sain
405400050728
405500050728     c                   EndSr
405600050819
405700050729      *------------------------------------------------------------------------*
405800050729      * CARICO DATI NELLA DS X LA COPIA DI TITGC
405900050729      *------------------------------------------------------------------------*
406000050729     c     Sr_Cardstgc   BegSr
406100050729
406200050729     c                   Clear                   Tnta25tad
406300050729     c                   Clear                   Tnta25tgc
406400050729     c                   Clear                   Tnta25tpd
406500050729     c                   Clear                   Tnta25tpt
406600050729     c                   Eval      ta25sgvr = v6csgv
406700050729     c                   Eval      ta25sgrr = v6csgr
406800050729     c                   Eval      ta25sgpr = v6csgp
406900050729     c                   Eval      ta25sgdr = v6csgd
407000050729     c                   Eval      ta25sg1r = v6csg1
407100050729     c                   Eval      ta25sg2r = v6csg2
407200050729     c                   Eval      ta25sg3r = v6csg3
407300050729     c                   Eval      ta25st1r = v6cst1
407400050729     c                   Eval      ta25st2r = v6cst2
407500050729     c                   Eval      ta25st3r = v6cst3
407600050729     c                   Eval      ta25stmr = v6cstm
407700050729     c                   Eval      ta25rpvrg = v6crpv
407800050729     c                   Eval      ta25fafr = v6cfaf
407900050729     c                   Eval      ta25sgfr = v6csgf
408000050729     c                   Eval      ta25ggrr = v6cggr
408100050729     c                   Eval      ta25tcmr = v6ctcm
408200050729     c                   Eval      ta25tfgr = v6ctfg
408300050729
408400050729     c                   EndSr
408500150210
408600050819      *------------------------------------------------------------------------*
408700050707      * COPIA TARIFFA INTERA
408800050819      *------------------------------------------------------------------------*
408900050707     c     Sr_Copia      BegSr
409000150210
409100150210      //?prima di richiamare il pgm di copia verifico se devo aggiornare
409200150210      //?la data scadenza dell'ultimo progressivo
409300150210     c                   IF        wokAGGdst
409400150210     c                   exsr      AggDataDst
409500150210     c                   ENDIF
409600090916
409700090916      * imposto se da trattativa
409800101222     c                   eval      %subst(kpjbu:1:1) = 'T'
409900050708
410000050708     c                   Call      'TNTA25R'
410100050713     c                   Parm                    kpjba
410200050708     c                   Parm                    Tnta25ds
410300050712     c                   Parm                    Tnta25tad
410400050712     c                   Parm                    Tnta25tgc
410500050712     c                   Parm                    Tnta25tpd
410600050712     c                   Parm                    Tnta25tpt
410700050714
410800050714     c                   If        ta25tam = 'S' and ta25prg <> *Blanks
410900050714     c                   Eval      wprg = ta25prg
411000050714     c                   EndIf
411100050819
411200050819     c                   EndSr
411300150210
411400150210      /free
411500150210       //--------------------------------------------------------------
411600150210       //?Aggiorno data scadenza ultimo progressivo tariffa.
411700150210       //--------------------------------------------------------------
411800150210       BEGSR AggDataDst;
411900150210
412000150210      //?Aggancio la tariffa da variare
412100150210         chain(e) (sav_TAMksc:sav_TAMctr:sav_TAMprg) TNTAM00L;
412200150210         IF  %error;
412300150210           *in28 = *on;
412400150210           W02msg = msg(62);
412500150210           exsr GesW02;
412600150210      /end-free
412700150210     c                   IF        *in90
412800150210     c                   goto      EmiD01
412900150210     c                   ENDIF
413000150210      /free
413100150210         ENDIF;
413200150210         IF  not %found(TNTAM00L);
413300150210           leavesr;
413400150210         ENDIF;
413500150210      //?se modificata la data scadenza tariffa aggiorno TNTAM
413600150210         IF  TAMdst <> wDataScadNw;
413700150210           TAMdst = wDataScadNw;
413800150210           TAMdtr = %dec(%date());
413900150210           clear TAMftr;
414000150210           TAMduv = %dec(%date());
414100150210           update  TNTAM0L;
414200150210         ENDIF;
414300150210
414400150210       ENDSR;
414500150210
414600150210      /end-free
414700050713
414800050713      *------------------------------------------------------------------------*
414900050713      * CARICO I DATI NELLA VIDEATA DI RIEPILOGO COPIA
415000050713      *------------------------------------------------------------------------*
415100050713     c     Sr_Card07     BegSr
415200050713
415300050713     c                   Clear                   v7kscn
415400090916     c                   Clear                   v7nrvnt
415500050728     c                   Clear                   v7dksc
415600050728     c                   Clear                   v7dctr
415700050728     c                   Clear                   v7cdcv
415800050921     c                   Eval      *In29 = *Off
415900050713
416000050715      * Imposto i dati del cliente
416100050715      * non è copia su offerta
416200050727     c                   If        Not *In01
416300050713     c                   Eval      v7kscn = ta25kscn
416400050713     c                   Move      v7kscn        w0070
416500050713     c                   ExSr      Sr_Chkcli
416600050713     c                   Eval      v7dksc = acorag
416700050713     c                   EndIf
416800050715      * è copia su offerta
416900050727     c                   If        *In01
417000090916     c                   Eval      v7nrvnt = ta25kscn
417100101222     c     v7nrvnt       Chain     Tivis05l
417200090916     c                   If        %Found
417300050715     c                   Eval      v7dksc = visrag
417400050713     c                   EndIf
417500050713     c                   EndIf
417600050715      * Imposto la tariffa
417700050713     c                   Move      ta25ctrn      v7ctrn
417800050715      * Imposto il progressivo
417900050802     c                   Move      ta25prgn      v7prgn
418000090916      * se nuovo progressivo imposto quello
418100090916     c                   if        wprg <> *blanks
418200090916     c                   Move      wprg          v7prgn
418300090916     c                   endif
418400050728      *     decodifico la tariffa
418500050728     c                   Eval      v7dctr = v2dctr
418600050728     c                   Eval      v7cdcv = v2cdcv
418700050728     c                   Eval      v7cdiv = v2cdiv
418800050713
418900050713      * Imposto quello che è stato fatto
419000050729     c                   If        savtad = 'T' or
419100050729     c                             (savtad = 'D' and wcopiatad = *On)
419200050713     c                   Eval      v7ctad = 'SI'
419300050713     c                   EndIf
419400050805     c                   If        savtad = 'N' or savtad = *Blanks
419500050713     c                   Eval      v7ctad = 'NO'
419600050713     c                   EndIf
419700050802     c                   If        savtpt = 'T' or
419800050802     c                             (savtpt = 'D' and wcopiatpt = *On) or
419900050802     c                             (savtpt = 'D' and wcopiatpd = *On)
420000050713     c                   Eval      v7ctpt = 'SI'
420100050713     c                   EndIf
420200050805     c                   If        savtpt = 'N' or savtpt = *Blanks
420300050713     c                   Eval      v7ctpt = 'NO'
420400050713     c                   EndIf
420500050729     c                   If        savtgc = 'T' or
420600050729     c                             (savtgc = 'D' and wcopiatgc = *On)
420700050713     c                   Eval      v7ctgc = 'SI'
420800050713     c                   EndIf
420900050805     c                   If        savtgc = 'N' or savtgc = *Blanks
421000050713     c                   Eval      v7ctgc = 'NO'
421100050713     c                   EndIf
421200050729     c                   If        savcat = 'S'
421300050713     c                   Eval      v7ccat = 'SI'
421400050713     c                   EndIf
421500050805     c                   If        savcat = 'N' or savcat = *Blanks
421600050713     c                   Eval      v7ccat = 'NO'
421700050803    1c                   EndIf
421800050713
421900050713      * La copia non è andata a buon fine
422000050715if  1c                   If        ta25err <> *Blanks
422100050713     c                   Eval      v7cmsg = ta25msg
422200050803     c                   Eval      *In28 = *On
422300050921     c                   Eval      *In29 = *On
422400050715    1c                   EndIf
422500050713
422600050713     c                   EndSr
422700050801
422800050801      *------------------------------------------------------------------------*
422900050801      * INTEGRO I DATI NELLA TARIFFA/OFFERTA NUOVA
423000050801      *------------------------------------------------------------------------*
423100050801     c     Sr_Integra    BegSr
423200050801
423300050801     c                   ExSr      Sr_Cards
423400050801     c                   Eval      ta25tam = 'N'
423500050801     c                   Eval      ta25tad = 'N'
423600050801     c                   Eval      ta25tpt = 'N'
423700050801     c                   Eval      ta25tpd = 'N'
423800050801     c                   Eval      ta25tgc = 'N'
423900050801     c                   Eval      ta25cat = 'N'
424000050801     c                   Eval      ta25inglo = 'S'
424100111031      * in caso di copia su offerta nuova imposto il falg di ricerca data
424200111031      * riferimento della cartello maggiore tra data del giorno e data
424300111031      * decorrenza della nuova offerta
424400111031     c                   If        *in01 and not *in04
424500111031     c                   Eval      Ta25drc = 'S'
424600111031     c                   else
424700111031     c                   clear                   Ta25drc
424800111031     c                   Endif
424900111031
425000050801     c                   ExSr      Sr_Copia
425100050801
425200050801     c                   EndSr
425300080610
425400080610      *-------------------------------------------------------------------*
425500080610      * Integro la tariffa con la tariffa 'f' Fuel
425600080610      *-------------------------------------------------------------------*
425700080610     c     sr_fuel       begsr
425800080610
425900080610     c                   exsr      sr_cards
426000080610     c                   eval      ta25tam = 'N'
426100080610     c                   eval      ta25tad = 'N'
426200080610     c                   eval      ta25tpt = 'N'
426300080610     c                   eval      ta25tpd = 'N'
426400080610     c                   eval      ta25tgc = 'N'
426500080610     c                   eval      ta25cat = 'N'
426600080610     c                   eval      ta25inglo = 'f'
426700080610     c                   exsr      sr_copia
426800080610
426900080610     c                   endsr
427000090408
427100090408      *------------------------------------------------------------------------*
427200090408      * Ricerca cliente
427300090408      *------------------------------------------------------------------------*
427400090408     c     sr_ricksc     begsr
427500090415
427600090415     c                   Eval      DxcCap = DutKci
427700090415     c                   Call      'XCLIR'
427800090415     c                   Parm                    Xclirds
427900090415     c                   movea     dxcksc        ksa
428000090415     c                   move      ksa(1)        dsksa
428100090415     c                   z-add     dsksn         ksc(1)
428200090408
428300090408     c                   endsr
428400050711
428500050711      *------------------------------------------------------------------------*
428600050711      * CONTROLLO IL CLIENTE
428700050711      *------------------------------------------------------------------------*
428800050711     c     Sr_Chkcli     BegSr
428900050711
429000050711     c                   Clear                   Tibs69ds
429100050711     c                   Move      w0070         I69kac
429200050711     c                   Move      w0070         I69kcp
429300050711     c                   Call      'TIBS69R'
429400050711     c                   Parm                    Tibs69ds
429500050711     c                   Parm                    ds_cnaco
429600050711     c                   Parm                    ds_cnind
429700050711     c                   Parm                    ds_cnclp
429800050711     c                   Parm                    ds_fncls
429900050711     c                   Eval      wtibs69 = *On
430000050711
430100050711     c                   EndSr
430200050711
430300050711      *------------------------------------------------------------------------*
430400050711      * CERCO IL COMMERCIALE IN TABELLA
430500050711      *------------------------------------------------------------------------*
430600050711     c     Sr_Cercmm     BegSr
430700050711
430800130807     c                   Clear                   wporagru
430900130807      *
431000130807     c     Wcmm          Chain     AZCMM000
431100130807     c                   If        %Found(AZCMM01L)
431200130807     c                   Movel     CMMuni        wporagru
431300050711     c                   EndIf
431400050711
431500050711     c                   EndSr
431600090414
431700090414      *------------------------------------------------------------------------*
431800090414      * Gestione Window di richiesta ok su cliente non in gestione
431900090414      *------------------------------------------------------------------------*
432000090414     c     sr_geswin     begsr
432100090414
432200090414     c                   eval      w01sino = 'NO'
432300090414     c                   do        *hival
432400090414     c                   exfmt     ta24w01
432500090414     c   kf              leave
432600090414     c                   enddo
432700090414
432800090414      * imposto che ho scelto di proseguire
432900090414     c                   if        w01sino = 'SI'
433000090414     c                   eval      wokkscn = *on
433100090414     c                   endif
433200090414      * se non proseguo riemetto la videata
433300090414     c                   if        w01sino = 'NO'
433400090414     c                   eval      *in90 = *on
433500090414     c                   endif
433600090414
433700090414     c                   endsr
433800090414
433900090414      *------------------------------------------------------------------------*
434000090414      * Ricerca tariffa
434100090414      *------------------------------------------------------------------------*
434200090414     c     sr_ricctr     begsr
434300090414
434400090414      *     --> se copia da tariffa
434500090414    1c                   If        Not *In03
434600090414      *         deve esistere almeno una tariffa
434700090414     c     kksc          Chain     Tntam01l                           30
434800090414    2c                   Dow       Not *In30 and tamatb <> *Blanks
434900090414     c     kksc          Reade     Tntam01l                               30
435000090414    2c                   EndDo
435100090414    2c                   If        *In30
435200090414     c                   Eval      *In28 = *On
435300090414     c                   Eval      v1cmsg = msg(08)
435400090414     c                   Leavesr
435500090414    2c                   EndIf
435600090414      *         richiamo il programma x la scelta delle testate tariffa
435700090414     c                   move      kksc          ta36ksc
435800090414     c                   Clear                   ta36ctr
435900090414     c                   Eval      ta36flg = '1'
436000090414     c                   Eval      ta36vpr = '2'
436100090414     c                   Eval      ta36kcc = dutkci
436200090414     c                   Eval      kpjbu = parmta36
436300090414     c                   Call      'TNTA36R'
436400090414     c                   Parm                    kpjba
436500090414     c                   Eval      parmta36 = kpjbu
436600090414     c                   Leavesr
436700090414    1c                   EndIf
436800090414      *     --> se copia da offerta
436900090414if  1c                   If        *In03
437000090414      *         deve esistere almeno un'offerta
437100090414     c     kksc          Chain     Tnofm01l                           30
437200090414do  2c                   Dow       Not *In30 and tamatb <> *Blanks
437300090414     c     kksc          Reade     Tnofm01l                               30
437400090414    2c                   EndDo
437500090414if  2c                   If        *In30
437600090414     c                   Eval      *In28 = *On
437700090414     c                   Eval      v1cmsg = msg(10)
437800090414     c                   Leavesr
437900090414    2c                   EndIf
438000090414      *         richiamo il programma x la scelta delle testate offerte
438100090414     c                   move      kksc          ta36ksc
438200090414     c                   Clear                   ta36ctr
438300101222     c                   Eval      ta36flg = '3'
438400090414     c                   Clear                   ta36vpr
438500090414     c                   Eval      ta36kcc = dutkci
438600090414     c                   Eval      kpjbu = parmta36
438700090414     c                   Call      'TNTA36C'
438800090414     c                   Parm                    kpjba
438900090414     c                   Eval      parmta36 = kpjbu
439000090414     c                   Leavesr
439100090414    1c                   EndIf
439200090414
439300090414     c                   endsr
439400050711
439500050711      *------------------------------------------------------------------------*
439600050711      * CONTROLLO IL CODICE TARIFFA IN TABELLA
439700050711      *------------------------------------------------------------------------*
439800050711     c     Sr_Chkctr     BegSr
439900050711
440000050711     c                   Clear                   dstr
440100050711     c                   Eval      kcod = 'TR'
440200050711     c                   Eval      kkey = w001a
440300050711     c     kTabel        Chain     Tabel00f
440400050711     c                   If        %Found(Tabel00f) and tblflg = *Blanks
440500050711     c                   Eval      dstr = tbluni
440600050711     c                   Else
440700050711     c                   Eval      *In28 = *On
440800050711     c                   EndIf
440900050711
441000050711     c                   EndSr
441100050711
441200050711      *------------------------------------------------------------------------*
441300050711      * CONTROLLO IL TIPO SERVIZIO IN TABELLA
441400050711      *------------------------------------------------------------------------*
441500050711     c     Sr_Chktsp     BegSr
441600050711
441700050711     c                   Clear                   ds5e
441800050711     c                   Eval      kcod = '5E'
441900050711     c                   Eval      kkey = wtsp
442000050711     c     kTabel        Chain     Tabel00f
442100050711     c                   If        %Found(Tabel00f) and tblflg = *Blanks
442200050711     c                   Eval      ds5e = tbluni
442300050711     c                   EndIf
442400050711
442500050711     c                   EndSr
442600050711
442700050711      *------------------------------------------------------------------------*
442800050711      * CONTROLLO LE TARIFFE PARICOLARI IN TABELLA
442900050711      *------------------------------------------------------------------------*
443000050711     c     Sr_Chktpt     BegSr
443100050711
443200050711     c                   Eval      wftceli = *Off
443300050711     c                   Clear                   ds1P
443400050711     c                   Eval      kcod = '1P'
443500050711     c                   Eval      kkey = w002a
443600050711     c     kTabel        Chain     Tabel00f
443700050711     c                   If        %Found(Tabel00f) and tblflg = *Blanks
443800050711     c                   Eval      ds1p = tbluni
443900050711     c                   EndIf
444000050711     c                   If        §1peli <> *Blanks
444100050711     c                   Move      §1peli        w0080
444200050711     c                   If        wdst >= w0080
444300050711     c     *iso          Move      w0080         datadmy
444400050711     c                   Eval      wftceli = *On
444500050711     c                   Eval      wftc = w002a
444600050711     c                   EndIf
444700050711     c                   EndIf
444800050711
444900050711     c                   EndSr
445000050711
445100050711      *------------------------------------------------------------------------*
445200050711      * CONTROLLO IL CODICE TASSAZIONE IN TABELLA
445300050711      *------------------------------------------------------------------------*
445400050711     c     Sr_Chkcts     BegSr
445500050711
445600120712      * Ricerca
445700120712     c                   IF        %scan('?':w002a) > *zeros
445800120712     c                   clear                   TRTB09DS
445900120712     c                   eval      TB09fun = '1'
446000120712     c                   eval      TB09ord = '1'
446100120713     c                   IF        wdpd <> 'S'
446200120713     c                   eval      TB09est = wfie
446300120713     c                   ELSE
446400120713     c                   eval      TB09est = 'E'
446500120713     c                   ENDIF
446600120713     c                   eval      TB09uta = 'S'
446700120713     c                   IF        wfed = 'S'
446800120713     c                   eval      TB09utf = wfed
446900120713     c                   ELSE
447000120713     c                   eval      TB09utf = 'N'
447100120713     c                   ENDIF
447200120712     c                   eval      kpjbu = TRTB09DS
447300120712     c                   call      'TRTB09R'
447400120712     c                   parm                    kpjba
447500120712     c                   eval      TRTB09DS = kpjbu
447600120712     c                   IF        TB09cod <> *blanks
447700120712     c                   eval      w002a = TB09cod
447800120723     c                   else
447900120723     c                   clear                   w002a
448000120723     c                   leavesr
448100120712     c                   ENDIF
448200120712     c                   ENDIF
448300120712
448400050711     c                   Clear                   dsct
448500050711     c                   Eval      kcod = 'CT'
448600050711     c                   Eval      kkey = w002a
448700050711     c     kTabel        Chain     Tabel00f
448800050711     c                   If        %Found(Tabel00f) and tblflg = *Blanks
448900050711     c                   Eval      dsct = tbluni
449000050711     c                   Else
449100050711     c                   Eval      *In28 = *On
449200050711     c                   EndIf
449300050711
449400050711     c                   EndSr
449500050711
449600050711      *------------------------------------------------------------------------*
449700050711      * CONTROLLO LA NAZIONE IN TABELLA
449800050711      *------------------------------------------------------------------------*
449900050711     c     Sr_Chknaz     BegSr
450000050711
450100050711     c                   Clear                   ds15
450200050711     c                   Eval      kcod = '15'
450300050711     c                   Eval      kkey = w003a
450400050711     c     kTabel        Chain     Tabel00f
450500050711     c                   If        %Found(Tabel00f) and tblflg = *Blanks
450600050711     c                   Eval      ds15 = tbluni
450700050711     c                   EndIf
450800050711
450900050711     c                   EndSr
451000050729
451100050729      *------------------------------------------------------------------------*
451200050729      * CONTROLLO IL TIPO COMUNICAZIONE AL MITTENTE
451300050729      *------------------------------------------------------------------------*
451400050729     c     Sr_Chktcm     BegSr
451500050729
451600050729     c                   Clear                   ds2f
451700050729     c                   Eval      kcod = '2F'
451800050729     c                   Eval      kkey = w001a
451900050729     c     kTabel        Chain     Tabel00f
452000050729     c                   If        %Found(Tabel00f) and tblflg = *Blanks
452100050729     c                   Eval      dstr = tbluni
452200050729     c                   EndIf
452300050729
452400050729     c                   EndSr
452500050729
452600050729      *------------------------------------------------------------------------*
452700050729      * CONTROLLO IL TIPO COMUNICAZIONE FINE GIACENZA
452800050729      *------------------------------------------------------------------------*
452900050729     c     Sr_Chktfg     BegSr
453000050729
453100050729     c                   Clear                   ds2h
453200050729     c                   Eval      kcod = '2H'
453300050729     c                   Eval      kkey = w001a
453400050729     c     kTabel        Chain     Tabel00f
453500050729     c                   If        %Found(Tabel00f) and tblflg = *Blanks
453600050729     c                   Eval      dstr = tbluni
453700050729     c                   EndIf
453800050729
453900050729     c                   EndSr
454000050728
454100050728      *------------------------------------------------------------------------*
454200050728      * CONTROLLO INCLUSIONI/OMISSIONI DEL DETTAGLIO TARIFFARIO
454300050728      *------------------------------------------------------------------------*
454400050728     c     Sr_Seltad     BegSr
454500050728
454600050728     c                   Eval      wokrec = *Off
454700050729     c                   Eval      woklnp = *Off
454800050729     c                   Eval      wokcts = *Off
454900050729     c                   Eval      woksgl = *Off
455000050728
455100050728      * Controllo le inclusioni
455200050728      * --> Linee di partenza
455300050728if  1c                   If        wlnp > *Zeros
455400050921     c     tadlnp        Lookup    sklnp                                  30
455500050728      *     inclusa
455600050728if  2c                   If        *In30
455700050728     c                   Eval      woklnp = *On
455800050728   x2c                   Else
455900050728      *     non inclusa
456000050909if  3c                   If        Not *In30 and v1cio1 = 'I'
456100050728     c                   Eval      wokrec = *On
456200050728    3c                   EndIf
456300050728    2c                   EndIf
456400050728    1c                   EndIf
456500050728      * --> Codici di tassazione
456600050728if  1c                   If        wcts <> *Blanks
456700050728     c     tadcts        Lookup    skcts                                  30
456800050728      *     incluso
456900050728if  2c                   If        *In30
457000050728     c                   Eval      wokcts = *On
457100050728   x2c                   Else
457200050728      *     non incluso
457300050909if  3c                   If        Not *In30 and v1cio2 = 'I'
457400050728     c                   Eval      wokrec = *On
457500050728    3c                   EndIf
457600050728    2c                   EndIf
457700050728    1c                   EndIf
457800050728      * --> Scaglione
457900050909if  1c                   If        v1csgl > *Zeros
458000050728      *     incluso
458100050909if  2c                   If        tadsgl = v1csgl
458200050728     c                   Eval      woksgl = *On
458300050728   x2c                   Else
458400050728      *     non incluso
458500050909if  3c                   If        v1csgl <> tadsgl and v1cio3 = 'I'
458600050728     c                   Eval      wokrec = *On
458700050728    3c                   EndIf
458800050728    2c                   EndIf
458900050728    1c                   EndIf
459000050728      * Controllo le omissioni
459100050728      * --> Linee di partenza
459200050728if  1c                   If        wslnp = *On and woklnp = *On
459300050728     c                   Eval      wokrec = *On
459400050728    1c                   EndIf
459500050728      * --> Codici di tassazione
459600050729if  1c                   If        wscts = *On and wokcts = *On
459700050728     c                   Eval      wokrec = *On
459800050728    1c                   EndIf
459900050728      * --> Scaglione
460000050728if  1c                   If        wssgl = *On and woksgl = *On
460100050728     c                   Eval      wokrec = *On
460200050728    1c                   EndIf
460300050728
460400050728     c                   If        wokrec = *On
460500050728     c                   Leavesr
460600050728     c                   EndIf
460700050728
460800050728      * Se copia da tariffa
460900050728if  1c                   If        Not *In03
461000050728      * e copia da tariffa di cartello
461100050805if  2c                   If        woldcar = *On
461200050728      * non DPD o FedEx (quindi dalla 8888830)
461300050728      * dove sono presenti sia codici di tassazione Italia che Estero
461400050728      * (non ho messo fisso 8888830 xchè se un domani cambia....)
461500050728     c                             and wdpd <> 'S' and wfed <> 'S'
461600050728      * e non sto creando una nuova tariffa di cartello
461700050805if  3c                   If        wnewcar = *Off
461800050728      * controllo i codici di tassazione da includere
461900050728     c                   Eval      w002a = tadcts
462000050728     c                   ExSr      Sr_Chkcts
462100050728s   4c                   Select
462200050728      * Se tariffa estero solo estero
462300050909     c                   When      v1cfie = 'E' and §ctest = *Blanks
462400050728     c                   Eval      wokrec = *On
462500050728      * Se tariffa italia solo italia
462600050909     c                   When      v1cfie = 'I' and §ctest <> *Blanks
462700050728     c                   Eval      wokrec = *On
462800050728    4c                   EndSl
462900050728if  4c                   If        wokrec = *On
463000050728     c                   Leavesr
463100050728    4c                   EndIf
463200050728    3c                   EndIf
463300050728    2c                   EndIf
463400050728    1c                   EndIf
463500050728
463600050728     c                   EndSr
463700050728
463800050728      *------------------------------------------------------------------------*
463900050728      * CONTROLLO INCLUSIONI/OMISSIONI DEL DETTAGLIO DELLE TARIFFE PARTICOLARI
464000050728      *------------------------------------------------------------------------*
464100050728     c     Sr_Seltpd     BegSr
464200050728
464300050728     c                   Eval      wokrec = *Off
464400050729     c                   Eval      woklnp = *Off
464500050729     c                   Eval      wokcts = *Off
464600050729     c                   Eval      woksgl = *Off
464700050728
464800050728      * Controllo le inclusioni
464900050728      * --> Codici di tassazione
465000050728if  1c                   If        wcts <> *Blanks
465100050728     c     tpdcts        Lookup    skcts                                  30
465200050728      *     incluso
465300050728if  2c                   If        *In30
465400050728     c                   Eval      wokcts = *On
465500050728   x2c                   Else
465600050728      *     non incluso
465700050909if  3c                   If        Not *In30 and v1cio2 = 'I'
465800050728     c                   Eval      wokrec = *On
465900050728    3c                   EndIf
466000050728    2c                   EndIf
466100050728    1c                   EndIf
466200050728      * --> Scaglione
466300050909if  1c                   If        v1csgl > *Zeros
466400050728      *     incluso
466500050909if  2c                   If        tpdsgl = v1csgl
466600050728     c                   Eval      woksgl = *On
466700050728   x2c                   Else
466800050728      *     non incluso
466900050909if  3c                   If        v1csgl <> tpdsgl and v1cio3 = 'I'
467000050728     c                   Eval      wokrec = *On
467100050728    3c                   EndIf
467200050728    2c                   EndIf
467300050728    1c                   EndIf
467400050728      * Controllo le omissioni
467500050728      * --> Codici di tassazione
467600050729if  1c                   If        wscts = *On and wokcts = *On
467700050728     c                   Eval      wokrec = *On
467800050728    1c                   EndIf
467900050728      * --> Scaglione
468000050728if  1c                   If        wssgl = *On and woksgl = *On
468100050728     c                   Eval      wokrec = *On
468200050728    1c                   EndIf
468300050728
468400050728     c                   If        wokrec = *On
468500050728     c                   Leavesr
468600050728     c                   EndIf
468700050728
468800050728      * Se copia da tariffa
468900050728if  1c                   If        Not *In03
469000050728      * e copia da tariffa di cartello
469100050805if  2c                   If        woldcar = *On
469200050728      * non DPD o FedEx (quindi dalla 8888830)
469300050728      * dove sono presenti sia codici di tassazione Italia che Estero
469400050728      * (non ho messo fisso 8888830 xchè se un domani cambia....)
469500050728     c                             and wdpd <> 'S' and wfed <> 'S'
469600050728      * e non sto creando una nuova tariffa di cartello
469700050805if  3c                   If        wnewcar = *Off
469800050728      * controllo i codici di tassazione da includere
469900050805     c                   Eval      w002a = tpdcts
470000050728     c                   ExSr      Sr_Chkcts
470100050728s   4c                   Select
470200050728      * Se tariffa estero solo estero
470300050909     c                   When      v1cfie = 'E' and §ctest = *Blanks
470400050728     c                   Eval      wokrec = *On
470500050728      * Se tariffa italia solo italia
470600050909     c                   When      v1cfie = 'I' and §ctest <> *Blanks
470700050728     c                   Eval      wokrec = *On
470800050728    4c                   EndSl
470900050728if  4c                   If        wokrec = *On
471000050728     c                   Leavesr
471100050728    4c                   EndIf
471200050728    3c                   EndIf
471300050728    2c                   EndIf
471400050728    1c                   EndIf
471500050728
471600050728     c                   EndSr
471700080609
471800080609      *------------------------------------------------------------------------*
471900080609      * Carico filiali abilitate al lasciato avviso
472000080609      *------------------------------------------------------------------------*
472100080609     c     carlav        begsr
472200080609
472300080609     c                   clear                   sklav
472400080609     c                   clear                   xx
472500080609
472600080609     c/exec sql
472700080609     c+ declare lav cursor for select tntbe01l.*
472800080609     c+ from tntbe01l
472900080609     c+ where tbecod='LAV' and tbeatb = ''
473000080609     c/end-exec
473100080609
473200080609     c/exec sql
473300080609     c+ open lav
473400080609     c/end-exec
473500080609
473600080609     c                   do        *hival
473700080609
473800080609     c/exec sql
473900080609     c+ fetch next from lav into :tntbeds
474000080609     c/end-exec
474100080609
474200080609     c                   select
474300080609     c                   when      sqlcod = 100
474400080609     c                   leave
474500080609     c                   when      sqlcod < 0
474600080609     c                   other
474700080609     c                   eval      dlav = tbeuni
474800080609      * carico la schiera della filiali abilitate
474900080616     c                   if        d§lavtar > *date
475000080609     c                   iter
475100080609     c                   endif
475200080609     c                   eval      xx = xx + 1
475300080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
475400080609
475500080609     c                   endsl
475600080609
475700080609     c                   enddo
475800080609
475900080609     c/exec sql
476000080609     c+ close lav
476100080609     c/end-exec
476200080609
476300080609     c                   endsr
476400050819
476500050705      *------------------------------------------------------------------------*
476600050705      * ROUTINE INIZIALE
476700050705      *------------------------------------------------------------------------*
476800050705     c     *Inzsr        BegSr
476900050705
477000050705     c     *Entry        Plist
477100050705     c                   Parm                    Kpjba
477200050705     c                   Parm                    Tnta24ds
477300090915
477400090915      * controllo se richiamato da trattativa
477500050705
477600050707     c                   If        %Parms > 1
477700050727     c                   Eval      *In01 = *On
477800050705     c                   Else
477900050727     c                   Eval      *In01 = *Off
478000050705     c                   EndIf
478100050705
478200050708      * se richiamato controllo se devo copiare da Tariffa o da Offerta
478300050727     c   01              Eval      *In03 = (ta24tip = 'O')
478400050819
478500050705     c     *dtaara       define    §azute        azuteds
478600050705     c     *dtaara       define    §datiute      ddatiute
478700050705     c                   in(E)     *dtaara
478800050705     c                   If        %error  or RSUT = *blanks
478900050705     c                   Clear                   Tibs34ds
479000050705     c                   Call      'TIBS34R'
479100050705     c                   Parm                    Tibs34ds
479200050705     c                   In        *dtaara
479300050705     c                   EndIf
479400050705
479500050705     c                   Clear                   wabi
479600050705     c                   Clear                   dLat
479700050705
479800050705      * Verifica errori e autorità profilo
479900050705s   1c                   Select
480000050705      * se ho errori nei dati utente esco dal pgm
480100050705w   1c                   When      DutErr = 'E'
480200050727     c                   Eval      *In02 = *On
480300050705     c                   Eval      *In28 = *On
480400050705     c                   Eval      v1cmsg = msg(01)
480500050705      * se richiamato passo il messaggio
480600050727     c                   If        *In01
480700050705     c                   Eval      ta24msg = v1cmsg
480800050705     c                   Eval      ta24err = '1'
480900050705     c                   EndIf
481000050705     c                   Leavesr
481100050705      * se non c'è l'abilitazione
481200050705      * --> se 1° livello, abilitazioni al terminal
481300050705      *     se 2° livello, abilitazioni al punto operativo
481400050705      *     se sede è impossibile ma se capita mando a fine il pgm
481500050705w   1c                   When      UteAut = *Blanks
481600050705if  2c                   If        DutLpo = '1'
481700050705     c                   Eval      wabi   = 'TP'
481800050705e   2c                   EndIf
481900050705if  2c                   If        DutLpo = '2'
482000050705     c                   Eval      wabi   = 'PO'
482100050705e   2c                   EndIf
482200050705if  2c                   If        DutLpo = 'S'
482300050727     c                   Eval      *In02 = *On
482400050705     c                   Eval      *In28 = *On
482500050705     c                   Eval      v1cmsg = msg(01)
482600050705      * se richiamato passo il messaggio
482700050727     c                   If        *In01
482800050705     c                   Eval      ta24msg = v1cmsg
482900050705     c                   Eval      ta24err = '1'
483000050705     c                   EndIf
483100050705     c                   Leavesr
483200050705e   2c                   EndIf
483300050705
483400050705      * carica le abilitazioni del profilo
483500050705      * alla gestione tariffe clienti
483600050705x   1c                   Other
483700050705     c                   Movel     UteFaf        Dute01
483800050705if  2c                   If        §UteGtc <> *Blanks
483900050705     c                   Eval      wabi = §UteGtc
484000050705   x2c                   Else
484100050705     c                   Eval      wabi = UteAut
484200050705e   2c                   EndIf
484300050705e   1c                   EndSl
484400050705
484500050705      * controllo se ok l'abilitazione dell'utente
484600050705      * alla gestione tariffe clienti
484700050705     c                   Clear                   Tibs02ds
484800050705     c                   Eval      T02Mod = 'C'
484900050705     c                   Eval      T02Sif = knsif
485000050705     c                   Eval      T02Cod = 'LAT'
485100050705     c                   Movel(p)  wabi          T02Ke1
485200050705     c                   Call      'TIBS02R'
485300050705     c                   Parm                    kpjba
485400050705     c                   Parm                    Tibs02ds
485500050705if  1c                   If        T02Err = *Blanks
485600050705     c                   Eval      dLat = T02Uni
485700050705e   1c                   EndIf
485800050705      * errore o non abilitato
485900050705if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
486000050727     c                   Eval      *In02 = *On
486100050705     c                   Eval      *In28 = *On
486200050705     c                   Eval      v1cmsg = msg(01)
486300050705      * se richiamato passo il messaggio
486400050727     c                   If        *In01
486500050705     c                   Eval      ta24msg = v1cmsg
486600050705     c                   Eval      ta24err = '1'
486700050705     c                   EndIf
486800050705     c                   Leavesr
486900050705e   1c                   EndIf
487000050705
487100050705      * Reperimento dei P.O. gestibili dall'utente
487200050705      * per la gestione delle tariffe
487300050705     c                   Clear                   Trul31ds
487400050705     c                   Eval      I31abi = wabi
487500050705     c                   Eval      I31cdi = DUTdis
487600050705     c                   Eval      I31car = DUTare
487700050705     c                   Eval      I31cpo = DUTpou
487800050705     c                   Call      'TRUL31R'
487900050705     c                   Parm                    KPJBA
488000050705     c                   Parm                    Trul31ds
488100050705if  1c                   If        O31pog > *zeros
488200050705     c                   Movea     O31pog        skpog
488300050705x   1c                   Else
488400050727     c                   Eval      *In02 = *On
488500050705     c                   Eval      *In28 = *On
488600050705     c                   Eval      v1cmsg = msg(01)
488700050705      * se richiamato passo il messaggio
488800050727     c                   If        *In01
488900050705     c                   Eval      ta24msg = v1cmsg
489000050705     c                   Eval      ta24err = '1'
489100050705     c                   EndIf
489200050705     c                   Leavesr
489300050705e   1c                   EndIf
489400050705
489500050705      * carica le abilitazioni del profilo
489600050705      * per i clienti da cui copiare le tariffe
489700050705     c                   If        §UteCtc <> *Blanks
489800050705     c                   Eval      wabi = §UteCtc
489900050705     c                   Else
490000050705     c                   Eval      wabi = UteAut
490100050705     c                   EndIf
490200050705
490300050705      * controllo se ok l'abilitazione dell'utente
490400050705      * per i clienti da cui copiare le tariffe
490500050705     c                   Clear                   Tibs02ds
490600050705     c                   Eval      T02Mod = 'C'
490700050705     c                   Eval      T02Sif = knsif
490800050705     c                   Eval      T02Cod = 'LAT'
490900050705     c                   Movel(p)  wabi          T02Ke1
491000050705     c                   Call      'TIBS02R'
491100050705     c                   Parm                    kpjba
491200050705     c                   Parm                    Tibs02ds
491300050705if  1c                   If        T02Err = *Blanks
491400050705     c                   Eval      dLat = T02Uni
491500050705e   1c                   EndIf
491600050705      * errore o non abilitato
491700050705if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
491800050727     c                   Eval      *In02 = *On
491900050705     c                   Eval      *In28 = *On
492000050705     c                   Eval      v1cmsg = msg(01)
492100050705      * se richiamato passo il messaggio
492200050727     c                   If        *In01
492300050705     c                   Eval      ta24msg = v1cmsg
492400050705     c                   Eval      ta24err = '1'
492500050705     c                   EndIf
492600050705     c                   Leavesr
492700050705e   1c                   EndIf
492800050705
492900050705      * Reperimento dei P.O. gestibili dall'utente
493000050705      * per i clienti da cui copiare le tariffe
493100050705     c                   Clear                   Trul31ds
493200050705     c                   Eval      I31abi = §UteCtc
493300050705     c                   Eval      I31cdi = DUTdis
493400050705     c                   Eval      I31car = DUTare
493500050705     c                   Eval      I31cpo = DUTpou
493600050705     c                   Call      'TRUL31R'
493700050705     c                   Parm                    KPJBA
493800050705     c                   Parm                    Trul31ds
493900050705if  1c                   If        O31pog > *zeros
494000050705     c                   Movea     O31pog        skpogc
494100050705x   1c                   Else
494200050727     c                   Eval      *In02 = *On
494300050705     c                   Eval      *In28 = *On
494400050705     c                   Eval      v1cmsg = msg(01)
494500050705      * se richiamato passo il messaggio
494600050727     c                   If        *In01
494700050705     c                   Eval      ta24msg = v1cmsg
494800050705     c                   Eval      ta24err = '1'
494900050705     c                   EndIf
495000050705     c                   Leavesr
495100050705e   1c                   EndIf
495200050705
495300050705      * Controllo se sono in sede
495400050705     c                   Eval      *In12 = (simfel = *zeros)
495500050705
495600050707      * Apro i file delle visite/offerte se non sono in sede
495700050705     c                   If        Not *In12
495800060307     c                   open      tfclp00f
495900060307     c                   open      cnclp00f
496000050706     c                   Open      Tnofm01l
496100050706     c                   Open      Tiofd01l
496200050707     c                   Open      Tiopt01l
496300050728     c                   Open      Tiopd01l
496400050729     c                   Open      Tiogc01l
496500101222     c                   open      tivis05l
496600050705     c                   EndIf
496700050912
496800050912      * Recupero range tariffe di cartello FedEx
496900050912     c                   Clear                   Tibs02ds
497000050912     c                   Clear                   ddfe
497100050912     c                   Eval      T02Mod = 'C'
497200050912     c                   Eval      T02Sif = knsif
497300050912     c                   Eval      T02Cod = 'DFE'
497400050912     c                   Movel(p)  'FED'         T02Ke1
497500050912     c                   Call      'TIBS02R'
497600050912     c                   Parm                    kpjba
497700050912     c                   Parm                    Tibs02ds
497800050912if  1c                   If        T02Err = *Blanks
497900050912     c                   Eval      ddfe = T02Uni
498000050912e   1c                   EndIf
498100140128
498200140128      /free
498300140128       //?Data del giorno
498400140128         dataoggi = %dec(%date());
498500150209
498600150209       //?Imposto la libreria per il TITAS
498700150209         IF  %subst(knsif : 7 : 1) = 'P';
498800150209           wLib = 'GAITRAGRPS';
498900150209         ELSE;
499000150209           wLib = 'GAITRAGRU';
499100150209         ENDIF;
499200150209       //?Apro il file
499300150209         wFLib = %trim(wLib) + '/TITAS41C';
499400150209         open  TITAS41C;
499500140128      /end-free
499600050705
499700050705      * KLIST
499800050705     c     kTabel        klist
499900050705     c                   kfld                    kkut
500000050705     c                   kfld                    kcod
500100050705     c                   kfld                    kkey
500200060307
500300060307     c     kclp          klist
500400060307     c                   kfld                    kkut
500500060307     c                   kfld                    dutkci
500600060307     c                   kfld                    kksc
500700050705
500800050705     c     kTntam        klist
500900050705     c                   kfld                    kksc
501000050705     c                   kfld                    kctr
501100050706
501200050706     c     kTntam01      klist
501300050706     c                   kfld                    kksc
501400050706     c                   kfld                    kctr
501500050706     c                   kfld                    kprg
501600050711
501700050711     c     kTitad        klist
501800050711     c                   kfld                    kksc
501900050711     c                   kfld                    kctr
502000050711     c                   kfld                    kprg
502100050711     c                   kfld                    klnp
502200050805
502300050805     c     kTitad01      klist
502400050805     c                   kfld                    kksc
502500050805     c                   kfld                    kctr
502600050805     c                   kfld                    kprg
502700050805     c                   kfld                    klnp
502800050805     c                   kfld                    korp
502900050711
503000050711     c     kTitad04      klist
503100050711     c                   kfld                    kksc
503200050711     c                   kfld                    kctr
503300050711     c                   kfld                    kprg
503400050711     c                   kfld                    klnp
503500050711     c                   kfld                    korp
503600050711     c                   kfld                    knaz
503700050711     c                   kfld                    kcap
503800050711     c                   kfld                    ksgl
503900050727
504000050727     c     kTitpt        klist
504100050727     c                   kfld                    kksc
504200050727     c                   kfld                    kctr
504300050727     c                   kfld                    kprg
504400050727     c                   kfld                    kftc
504500050727
504600050727     c     kTitpd        klist
504700050727     c                   kfld                    kksc
504800050727     c                   kfld                    kctr
504900050727     c                   kfld                    kprg
505000050727     c                   kfld                    kftc
505100050727     c                   kfld                    korp
505200050727     c                   kfld                    ksgl
505300050728
505400050728     c     kTeetc        klist
505500050728     c                   kfld                    kdsf
505600050728     c                   kfld                    kcto
505700050728     c                   kfld                    kksc
505800050728     c                   kfld                    kctr
505900050728     c                   kfld                    kprg
506000140128
506100140128     c     kACB          klist
506200140128     c                   kfld                    savclva
506300140128     c                   kfld                    dataoggi
506400050705
506500050705     c                   EndSr
506600050705      *------------------------------------------------------------------------*
506700050705
506800050705** MSG  Lungh. 78                                                            *
506900050705Utente non autorizzato alla Copia Tariffe                                     01
507000050712Immettere            da cui copiare                                           02
507100090423Cliente non in gestione all'utente                                            03
507200050705Offerta errata                                                                04
507300090423Offerta non in gestione all'utente                                            05
507400050705Codice tariffa non codificato                                                 06
507500050705Codice tariffa incongruente con Codice tariffa a cui copiare                  07
507600050705Tariffa inesistente                                                           08
507700050705Offerta inesistente                                                           09
507800050707Immettere il progressivo da cui copiare                                       10
507900050707Immettere il Cliente a cui copiare                                            11
508000050714Utente non abilitato alla gestione delle tariffe di cartello                  12
508100050714        duplicabile solo per Clienti Poste                                    13
508200050714        non duplicabile per un Cliente Poste                                  14
508300050714Immettere la tariffa a cui copiare                                            15
508400050714Codice tariffa incongruente con Codice tariffa da cui copiare                 16
508500050714Tariffa a cui copiare inesistente                                             17
508600050714Tipo tariffa   incongruente con la tariffa da cui copiare                     18
508700050714Network       incongruente con la tariffa da cui copiare                      19
508800050714Modo applizazione tariffa   incongruente con la tariffa da cui copiare        20
508900050714Effettuare almeno una scelta                                                  21
509000050909Tariffe particolari già presenti nella tariffa a cui copiare                  22
509100050728Nessuna dato tipo da copiare                                                  23
509200050922Impossibile inserire linea di partenza estera                                 24
509300050922Non inserire LNP 950 perchè la LNP     ha codici di tassazione 'IT' o 'EE'    25
509400050714Tariffa particolare    da eliminare entro           : non verrà copiata       26
509500050714Tariffa di giacenza già presente nella tariffa a cui copiare                  27
509600050714Dati tipo già presenti nella tariffa a cui copiare                            28
509700050714Copia dati tipo possibile solo se copia totale della tariffa/offerta          29
509800050714Immettere tipo tariffa Italia/Estero                                          30
509900050714Immettere almeno una linea di partenza                                        31
510000050714Linea di partenza     inesistente                                             32
510100050714Immettere almeno un codice di tassazione                                      33
510200050714Codice tassazione    errato                                                   34
510300050728Codice tassazione    non utilizzabile per tariffa FedEx                       35
510400050714Codice tassazione    utlizzabile solo per tariffa FedEx                       36
510500050728Codice tassazione    non utilizzabile per tariffe estere o DPD                37
510600050728Immettere lo scaglione                                                        38
510700050728Impossibile inserire scaglione con decimali                                   39
510800050728Dettaglio tariffario già presente nella tariffa a cui copiare                 40
510900050728Tariffa particolare già presente nella tariffa a cui copiare                  41
511000050728Dettaglio della tariffa particolare già presente nella tariffa a cui copiare  42
511100050728Nessun dettaglio tariffario da                                                43
511200050728Nessuna tariffa particolare da                                                44
511300050728Nessun dettaglio della tariffa particolare da visualizzare                    45
511400050922Presenti più di 50 linee di partenza! Selezionare le linee interessate        46
511500050919Immettere la nuova linea di partenza                                          47
511600050919Linea di partenza già presente                                                48
511700050920Copia dati tipo non possibile se copia da tariffa di cartello                 49
511800050922Per copia da tariffa di cartello includere almeno una linea di partenza       50
511900050922Copia da tariffa di cartello, LNP del cliente non inclusa. ENTER X FORZARE!   51
512000050922Copia VERSO una tariffa di cartello possibile solo se si copia DA cartello    52
512100060130Attenzione!! Nella tariffa da cui si copia esistono particolarità. VERIFICARE 53
512200060307Tariffa a cui copiare con rinuncia AC BASE, la tar.part. non verrà copiata    54
512300060307Impossibile copiare AC BASE. Presente AC PLUS reale con applicazione completa 55
512400060307Impossibile copiare AC PLUS reale con applicazione completa. Presente AC BASE 56
512500060705La tar.part. AC BASE non verrà copiata. Importo non congruente                57
512600090415Se Cod.Cliente non in gestione è possibile SOLO creare una nuova tariffa      58
512700091021Dati errati!!! Il campo contiene caratteri NON numerici                       59
512800121001Impossibile duplicare dettaglio con LNP e codice tassazione italiani          60
512900150210Data Errata.                                                                  61
513000150210Tariffa in modifica da altro utente riprovare più tardi.                      62
