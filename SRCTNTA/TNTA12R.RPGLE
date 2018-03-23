000100051201     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200051201
000300051201      *------------------------------------------------------------------------*
000400051201      *                                                                        *
000500170928      *                 GESTIONE CONTATTI                 ?                    *
000600051201      *                                                                        *
000700051201      *------------------------------------------------------------------------*
000800051201
000900051205     fTfntc00f  uf a e             disk    rename(tfntc:tfntc0)
001000051205     fTfntc01l  if   e           k disk    infds(ntcds)
001100051202     fTncpo01l  if   e           k disk
001200051201     fTabel00f  if   e           k disk
001300140722     fTNTBE01L  if   e           k disk
001400160614     fAZORG01L  if   e           k disk
001500140722      *
001600051206     fTnta12D   cf   e             workstn sfile(ta12s02:nrr1)
001700051206     f                                     sfile(ta12s03:nrr2)
001800140722     f                                     sfile(TA12S04:S04nrr)
001900051201
002000051201      *------------------------------------------------------------------------*
002100051201      *  RIEPILOGO INDICATORI
002200051201      *------------------------------------------------------------------------*
002300051201      * 01 - Pgm richiamato
002400051201      * 02 - Utente non abilitato
002500051201      * 03 - Cliente
002600051201      * 04 - Potenziale
002700051202      * 06 - Seconda parte della prima videata
002800051202      * 07 - Proteggo campo nota a video
002900140530      * 08 - Eccezione per la modifica del contatto in Rubrica
003000140530      *      (altrimenti SOLO visualizzabile)
003100051207      * 09 - Solo interrogazione
003200051207      * 10 - Sottolineo campo nota sel sbfl 02 x interrogazione
003300090915      * 11 - Richiamato per trattativa
003400051201      * 12 - Sede - N12 Filiale
003500051201      * 20 - GESTIONE SUBFILE
003600051201      * 21 - GESTIONE SUBFILE
003700051201      * 22 - GESTIONE SUBFILE
003800051201      * 28 - ERRORE GENERICO DSPF
003900051201      * 30 - Comodo
004000051202      * 31 - Fine File
004100051202      * 32 - Fine Subfile
004200140722      * 33 - SubFileEND per S04/C04
004300051201      * 40 - Cliente/Potenziale/Visita
004400051201      * 41 - Contatto
004500051202      * 42 - Nota
004600051201      * 90 - Riemetto la videata
004700051201      *------------------------------------------------------------------------*
004800051201
004900051201      *   V A R I A B I L I
005000051201     d comando         s              1
005001170928     d IndicePOG       s              3  0 inz
005002170928     d IndiceskPOG     s              3  0 inz
005100051202     d kapl            s                   like(ntcapl)
005200051202     d kcod            s                   like(tblcod)
005300051201     d kkey            s                   like(tblkey)
005400051201     d kkut            s                   like(tblkut) inz(1)
005500051202     d knk1            s                   like(ntcnk1)
005600051202     d knk2            s                   like(ntcnk2)
005700051202     d ktnt            s                   like(ntctnt)
005800051206     d nrr1            s                   like(recsf1)
005900051206     d nrr2            s                   like(recsf2)
006000140722     d S04nrr          s                   like(C04rcd) inz
006100140722     d SavS04csr       s                   like(C04rcd) inz
006200051201     d ordina          s              1
006300051202     d riga            s                   like(§1trig)
006400051201     d tiporich        s              1    inz('C')
006600051201     d wpos            s              2  0
006800051201     d wtibs69         s              1    inz('0')
006900160614     d w0030           s              3  0 inz
007200051202     d xx              s              3  0
007300051202     d yy              s              3  0
007400090406     d zz              s              3  0
007500140722     d $InzS04         s               n   inz(*on)
007501170928     d Logistica       s               n   inz(*off)
007502170929     d sav_kpjbu       s                   like(kpjbu)
007600051201
007700051201      *   S C H I E R E
007800051201     d msg             s             78    Dim(60) ctdata perrcd(1)
007900051206     d skceml          s              1    Dim(99)
008000051206     d skdtnt          s             25    Dim(99)
008100060329     d skmult          s              1    Dim(99)
008200051201     d sktnt           s              2    Dim(99)
008300051202     d sktnt1          s              2    Dim(99)
008400090406     d skord           s              5    dim(99)
008500090415     d skord1          s              5    dim(99)
008600140722     d sk_Prg          s                   dim(99)  like(§1Tprg)
008700140722     d sk_Rgr          s                   dim(99)  like(§1Trgr)
008800140722     d sk_Tot          s              4    dim(99)
008801170928     d skPOG           s              3  0 dim(250) inz
008900051201
009000051201      *   D S   I N T E R N E / E S T E R N E
009100051205     d ntcds           ds
009200051205     d ntcnrr                397    400B 0
009300051202
009400051201     d                 ds
009500051202     d v1cnrv1                 1      6  0
009600051202     d v1cnrv2                 7      7  0
009700051202     d v1cnrv                  1      7  0
009800051202     d                 ds
009900051202     d v2cnrv1                 1      6  0
010000051202     d v2cnrv2                 7      7  0
010100051202     d v2cnrv                  1      7  0
010200051206     d                 ds
010300051206     d v3cnrv1                 1      6  0
010400051206     d v3cnrv2                 7      7  0
010500051206     d v3cnrv                  1      7  0
010600051201
010700051201     d                sds
010800051201     d  Vtcpgm                 1     10
010900051201
011000051201     d Azuteds       e ds                  Extname(Azute00f)
011100051201     d dDatiute      e ds
011200051202     d dsemail       e ds
011300051201     d ds1t          e ds
011400051201     d ds1u          e ds
011500140722     d d1TE          e ds                  inz
011600051201     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
011700051201     d ds_cnind      e ds                  inz  extname(Cnind00f)
011800051201     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
011900051201     d ds_fncls      e ds                  inz  extname(Fncls00f)
012000051201     d Kpjba         e ds
012100051201     d Tnta12ds      e ds
012200051201     d Tibs02ds      e ds
012300051201     d Tibs34ds      e ds
012400051201     d Tibs69ds      e ds
012500090630     d tntaa1ds      e ds
012600160614     d OG143         e ds
012601170928     d TRUL31DS      e ds
012602170928     d  pog                   10    759    dim(250)
012700051201
012800051201      *   C O S T A N T I
012900051201      * titolo videata (lunghezza massima 21)
013000051201     d VtcTit          C                   CONST('* GESTIONE CONTATTI *')
013100051201
013101170928      //---------------------------------------------------------------
013102170928      //?Definizione procedure utilizzate.
013103170928      //---------------------------------------------------------------
013107170928      // - Carica filiali abilitate all'utente
013108170928     d TRUL31R         pr                  extpgm('TRUL31R')
013109170928     d  KPJUBA                             likeds(KPJBA)
013110170928     d  TRUL31DS                           likeds(TRUL31DS)
013119170928
013200051201      *------------------------------------------------------------------------*
013300051201
013400051202     c                   Clear                   v1cksc
013500051202     c                   Clear                   v1cpot
013600051202     c                   Clear                   v1cnrv
013700051202     c                   Clear                   v1dksc
013800051202     c                   Clear                   v1dpot
013900051202     c                   Clear                   v1dnrv
014000051202     c                   Clear                   v1ctnt
014100090707     c                   Clear                   v1dtnt
014200051202
014201170929       // controllo se da menù per Rubrica Clienti Logistica
014202170929         IF  %subst(sav_kpjbu:1:1) = 'L';
014203170929           Logistica = *on;
014204170929         ENDIF;
014205170929
014300051202      * Se richiamato imposto i dati e vado subito al subfile
014400060216      * se ho i dati validi altrimenti vado in seconda videata
014500051202if  1c                   If        *In01
014600051202     c                   Eval      v1capl = ta12apl
014700051202     c                   Eval      *In03 = (v1capl = 'C')
014800051202     c                   Eval      *In04 = (v1capl = 'P')
014900091112     c                   Eval      *In11 = (v1capl = 'T')
015000060216      * se non c'è la chiave impostata vado in seconda videata
015100060216     c                   If        *In03 and ta12ksc = *Zeros
015200060216     c                   Goto      vid02
015300060216     c                   EndIf
015400060216     c                   If        *In04 and ta12pot = *Zeros
015500060216     c                   Goto      vid02
015600060216     c                   EndIf
015700091112     c                   If        *In11 and ta12nrv = *Zeros
015800091112     c                   Goto      vid02
015900091112     c                   EndIf
016000051202     c   03              Eval      v1cksc = ta12ksc
016100051202     c   04              Eval      v1cpot = ta12pot
016200091112     c   11              Eval      v1cnrv = ta12nrv
016300051202     c                   Eval      v1ctnt = ta12tnt
016400051202      * Decodifico i dati passati
016500051202     c                   Eval      *In06 = *On
016600090701     c                   ExSr      Sr_Ctraut
016700100331     c                   IF        not *in02
016800100331     c                   ExSr      Sr_Ctrd01
016900100331     c                   ENDIF
017000090701     C
017100060215     c                   If        *In28
017200060215     c                   Eval      ta12err = '1'
017300060215     c                   Eval      ta12msg = v1cmsg
017400060215     c                   Goto      Fine
017500060215     c                   EndIf
017600051202     c                   Goto      carsfl
017700101019    1c                   EndIf
017800051201
017900051201      * --> Prima videata
018000051201     c     emid01        Tag
018100051201      * Pulisco la videata
018200051201     c                   If        Not *In28 and Not *In90
018300101019     c                   Setoff                                       0306
018400051201     c                   Eval      v1capl = 'C'
018500101020     c                   EndIf
018600051201
018700051202      * --> Seconda parte
018800051201     c     vid02         Tag
018900051202     c                   Eval      *In06 = *On
019000051201     c                   Eval      *In03 = (v1capl = 'C')
019100051201     c                   Eval      *In04 = (v1capl = 'P')
019200091112     c                   Eval      *In11 = (v1capl = 'T')
019300051202     c                   Clear                   v1cksc
019400051202     c                   Clear                   v1cpot
019500051202     c                   Clear                   v1cnrv
019600051202     c                   Clear                   v1dksc
019700051202     c                   Clear                   v1dpot
019800051202     c                   Clear                   v1dnrv
019900051202     c                   Clear                   v1ctnt
020000090707     c                   Clear                   v1dtnt
020100090701      * Controllo se utente abilitato
020200090701     c                   ExSr      Sr_Ctraut
020300051201
020400051201      * Emetto la videata
020500051201     c     emid02        Tag
020600051201     c                   Write     Ta12t01
020700051202     c                   Exfmt     Ta12d01
020800090630     c                   Eval      *In28 = *Off
020900090630     c                   Eval      *In90 = *Off
021000090630     c                   Clear                   v1cmsg
021100051201
021200051201      * F3=Fine
021300090701      * *IN02 - esco lo stesso dal pgm perchè utente non abilitato
021400090701     c   02
021500090701     cor kc              Goto      fine
021600051201
021700051201      * F12=Ritorno
021800051201     c                   If        *Inkl
021900060216      * se richiamato da gestione note esco
022000060216     c                   If        *in01 and ta12note = 'S'
022100060216     c                   Goto      fine
022200060216     c                   EndIf
022201170929     c                   If        not *in01
022202170929     c                   Goto      fine
022203170929     c                   EndIf
022300051201     c                   Setoff                                       2890
022400051201     c                   Goto      emid01
022500051201     c                   EndIf
022600051201
022700051201      * Controllo i dati della videata
022800051202     c                   ExSr      Sr_Ctrd01
022900051201     c   28
023000051201     cor 90              Goto      emid02
023100051206
023200051206      * F10=Inserimento Multiplo E-Mail
023300051206     c                   If        *Inkj
023400051206     c                   ExSr      Sr_F10
023500051206     c                   Goto      emid02
023600051206     c                   EndIf
023700051201
023800051201      * --> Subfile
023900051201     c     Carsfl        Tag
024000140722     c                   if            %parms() > 1
024100140722     c                             and  TA12tnt = *blanks
024200140722     c                             and  TA12rgr = *blanks
024300140722     c                             and  TA12tot = *blanks
024400140722     c                   exsr      sr_GesS04
024500140722     c                   goto      CarSfl
024600140722     c                   endif
024700051201     c                   ExSr      Sr_Pulsfl
024800051201     c                   ExSr      Sr_Carsfl
024900051201     c                   ExSr      Sr_Emisfl
025000051201
025100051206     c                   Goto      vid02
025200051201
025300051201     c     Fine          Tag
025400051201
025500051201      * Chiude i file dei pgm richiamati
025600051201     c                   If        wtibs69 = *On
025700051201     c                   Eval      i69tla = 'C'
025800051201     c                   Call      'TIBS69R'
025900051201     c                   Parm                    tibs69ds
026000051201     c                   EndIf
026100090630     C*
026200090630     C                   clear                   tntaa1ds
026300090630     C                   movel     'C'           Itaa1Tla
026400090630     C                   movel(p)  tntaa1ds      kpjbu
026500090703     C                   CALL      'TNTAA1C'
026600090630     C                   PARM                    KPJBA
026700090630     c
026800051201
026900051201     c                   Eval      *InLr = *On
027000051201
027100051201      *------------------------------------------------------------------------*
027200051201      * CONTROLLO LA PRIMA VIDEATA
027300051201      *------------------------------------------------------------------------*
027400051201     c     Sr_Ctrd01     BegSr
027500051202
027600051202      * Spengo indicatori di posizionamento cursore
027700140722     c                   Setoff                                       4041
027800051201
027900051202if  1c                   If        Not *In06
028000051202
028100051202   x1c                   Else
028200051202
028300051202      * --> Codice Cliente/Potenziale/Nr.Visita
028400051202      * Cliente
028500051202     c   03              ExSr      Sr_Ctrksc
028600051202      * Potenziale
028700051202     c   04              ExSr      Sr_Ctrpot
028800090915      * Trattativa
028900090915     c                   if        *in11
029000090915     c                   eval      v1dnrv = ta12rag
029100090915     c                   endif
029200051202
029300051202      * --> Tipo contatto
029400090707     c                   clear                   v1dtnt
029500140722sel 2c                   Select
029600140722      * - - Ricevuto un Raggruppamento Rubrica
029700140722w   2c                   When           %parms() > 1
029800140722     c                             and  TA12rgr <> *blank
029900140722     c                   exsr      sr_RGRnot
030000140722      * - - Ricevuto un Codice Contatto
030100140722w   2c                   When      v1ctnt <> *Blanks
030200051202     c                   ExSr      Sr_Ctrtnt
030300140722      * - - Visualizzazione di tutti i Contatti
030400140722x   2c                   Other
030500051202     c                   Movea     sktnt         sktnt1
030600090415     c                   Movea     skord         skord1
030700140722     c                   sorta     skord1
030800140722e   2c                   EndSl
030900140722      *
031000140722     c                   sorta     skord
031100051202
031200051202    1c                   EndIf
031300051201
031400051201     c                   EndSr
031500051201
031600051202      *------------------------------------------------------------------------*
031700051202      * CONTROLLO CLIENTE
031800051202      *------------------------------------------------------------------------*
031900051202     c     Sr_Ctrksc     BegSr
032000051202
032100051202     c                   If        v1cksc = *Zeros
032200051202     c                   Eval      *In28 = *On
032300051202     c                   Eval      *In40 = *On
032400051202     c                   Eval      v1cmsg = msg(04)
032500051202     c                   Leavesr
032600051202     c                   EndIf
032700051202
032800060216      * se richiamato da immissione cliente non controllo
032900060216     c                   If        *In01 and
033000060216     c                             ta12gcli = '1' and ta12icli = '1'
033100060217      * imposto la ragione sociale
033200060217     c                   Eval      v1dksc = ta12rag
033300060216     c                   goto      noksc
033400060216     c                   endif
033500060216
033600051202     c                   Clear                   Tibs69ds
033700051202     c                   Move      v1cksc        I69kac
033800051202     c                   Call      'TIBS69R'
033900051202     c                   Parm                    Tibs69ds
034000051202     c                   Parm                    ds_cnaco
034100051202     c                   Parm                    ds_cnind
034200051202     c                   Parm                    ds_cnclp
034300051202     c                   Parm                    ds_fncls
034400051202     c                   Eval      wtibs69 = *On
034500051202     c                   If        o69err = *Blanks
034600051202     c                   Eval      v1dksc = acorag
034700051202     c                   Else
034800051202     c                   Eval      *In28 = *On
034900051202     c                   Eval      *In40 = *On
035000051202     c                   Eval      v1cmsg = msg(04)
035100051202     c                   Leavesr
035200051202     c                   EndIf
035300060216     c     noksc         tag
035400051202
035500051202      * Controllo se utente può gestire il cliente
035600051207      * se non richiamato
035700160614      *?Salto questo controllo
035800160614      *?SOLO se utente logistica (Cappellini)
035900160614     c                   clear                   OG143
036000160614     c     DUTpou        chain     AZORG01L
036100160614     c                   IF        %found(AZORG01L)
036200160614     c                   eval      OG143 = ORGde3
036300160614     c                   ENDIF
036400160614     c                   IF        §OGntw = 'LOG'
036500160614     c                   goto      dopotaa1
036600160614     c                   ENDIF
036601170929
036602170929      // se Rubrica Clienti Logistica controllo con sk caricata per Logistica
036603170929       IF  Logistica;
036604170929         w0030 = %dec(%subst(%editc(V1Cksc:'X'):1:3):3:0);
036605170929         IF  %lookup (w0030:skPOG) = 0;
036606170929           *in28 = *on;
036607170929           *in40 = *on;
036608170929           V1Cmsg = msg(08);
036609170929           leavesr;
036610170929         ENDIF;
036611170929         leavesr;
036612170929       ENDIF;
036613170928
036700051207if  1c                   If        Not *In01
036800090630     c                   clear                   tntaa1ds
036900090630     c                   eval      itaa1caut='§utecli'
037000090630     c                   eval      itaa1ksc=v1cksc
037100090630     c                   clear                   kpjbu
037200090630     c                   movel     tntaa1ds      kpjbu
037300090703     c                   call      'TNTAA1C'
037400090630     c                   parm                    kpjba
037500090630     c                   movel     kpjbu         tntaa1ds
037600090630     c
037700090630    1c                   if        otaa1fabi='N'
037800051202     c                   Eval      *In28 = *On
037900051202     c                   Eval      *In40 = *On
038000051202     c                   Eval      v1cmsg = msg(05)
038100051202     c                   Leavesr
038200051207    2c                   EndIf
038300051207    1c                   EndIf
038400160614     c                   leavesr
038401170928
038500160614
038600160614     c     dopotaa1      tag
038700170929
038800160614       //?Visto che questa parte viene fatta solo da utente logistica
038900170929       //?verifico che il cliente inserito sia un cliente con ntw LOG
039000160614         w0030 = %dec(%subst(%editc(V1Cksc:'X'):1:3):3:0);
039100160614         clear OG143;
039200160614         chain w0030 AZORG01L;
039300160614         IF  %found(AZORG01L);
039400160614           OG143 = ORGde3;
039500160614         ENDIF;
039600160614         IF  §OGntw <> 'LOG';
039700160614           *in28 = *on;
039800160614           *in40 = *on;
039900160614           V1Cmsg = msg(08);
040000160614           leavesr;
040100160614         ENDIF;
040200170929
040300051202     c                   EndSr
040400051202
040500051202      *------------------------------------------------------------------------*
040600051202      * CONTROLLO POTENZIALE
040700051202      *------------------------------------------------------------------------*
040800051202     c     Sr_Ctrpot     BegSr
040900051202
041000051202     c                   If        v1cpot = *Zeros
041100051202     c                   Eval      *In28 = *On
041200051202     c                   Eval      *In40 = *On
041300051202     c                   Eval      v1cmsg = msg(06)
041400051202     c                   Leavesr
041500051202     c                   EndIf
041600051202
041700060216      * se richiamato da immissione cliente non controllo
041800060216     c                   If        *In01 and
041900060216     c                             ta12gcli = '1' and ta12icli = '1'
042000060217      * imposto la ragione sociale
042100060217     c                   Eval      v1dpot = ta12rag
042200060216     c                   goto      nopot
042300060216     c                   endif
042400060216
042500051202     c     v1cpot        Chain     Tncpo01l
042600051202     c                   If        %Found(Tncpo01l) and cpoatb = *Blanks
042700060217     c                   Eval      v1dpot = cporag
042800051202     c                   Else
042900051202     c                   Eval      *In28 = *On
043000051202     c                   Eval      *In40 = *On
043100051202     c                   Eval      v1cmsg = msg(06)
043200051202     c                   Leavesr
043300051202     c                   EndIf
043400060216     c     nopot         tag
043500051202
043600051202      * Controllo se utente può gestire il potenziale
043700100331      * se non richiamato
043800100331if  1c                   If        Not *In01
043900090630     c                   clear                   tntaa1ds
044000090702     c                   eval      itaa1cpo =cpocpo
044100090630     c                   eval      itaa1caut='§UTEPOT'
044200090630     c                   clear                   kpjbu
044300090630     c                   movel     tntaa1ds      kpjbu
044400090703     c                   call      'TNTAA1C'
044500090630     c                   parm                    kpjba
044600090630     c                   movel     kpjbu         tntaa1ds
044700090630     c
044800090630     c                   if        otaa1fabi='N'
044900051202     c                   Eval      *In28 = *On
045000051202     c                   Eval      *In40 = *On
045100051202     c                   Eval      v1cmsg = msg(07)
045200051202     c                   Leavesr
045300051202     c                   EndIf
045400100331     c                   endif
045500051202
045600051202     c                   EndSr
045700051202
045800051202      *------------------------------------------------------------------------*
045900051202      * CONTROLLO TIPO CONTATTO
046000051202      *------------------------------------------------------------------------*
046100051202     c     Sr_Ctrtnt     BegSr
046200051202
046300051202      * Ricerca
046400051202     c                   Eval      wpos = %scan('?':v1ctnt)
046500051202     c                   If        wpos > 0
046600051202     c                   Eval      kcod = '1T'
046700051202     c                   Call      'TRTB15R'
046800051202     c                   Parm                    kcod
046900051202     c                   Parm                    ordina
047000051202     c                   Parm                    tiporich
047100051202     c                   Parm                    kkey
047200051202     c                   Parm                    comando
047300051202     c                   Eval      v1ctnt = kkey
047400051202     c                   Eval      *In41 = *On
047500051202     c                   Eval      *In90 = *On
047600051202     c                   EndIf
047700051202
047800051202      * Controllo
047900051202     c                   Clear                   v1dtnt
048000051202     c                   Clear                   ds1t
048100051202     c                   Eval      kcod = '1T'
048200051202     c                   Eval      kkey = v1ctnt
048300051202     c     kTabel        Chain     Tabel00f
048400051202     c                   If        Not %Found(Tabel00f) or
048500051202     c                             tblflg <> *Blanks
048600051202     c                   Eval      *In28 = *On
048700051202     c                   Eval      *In41 = *On
048800051202     c                   Eval      v1cmsg = msg(10)
048900051202     c                   Leavesr
049000051202     c                   EndIf
049100051202
049200051202     c                   Eval      ds1t = tbluni
049300051202     c                   Eval      v1dtnt = §1tdes
049301170929
049302170929      // se Rubrica Clienti Logistica
049304170928      // posso utilizzare solo note abilitate da tabella
049305170929       IF  Logistica and §1Toklog <> 'L';
049306170928         *in28 = *on;
049307170928         *in41 = *on;
049308170928         V1Cmsg = msg(09);
049309170928         leavesr;
049310170928       ENDIF;
049400051202     c                   Clear                   sktnt1
049500090415     c                   clear                   skord1
049600051202     c                   Eval      sktnt1(1) = v1ctnt
049700090415     c                   eval      %subst(skord1(1):1:3) = %editc(§1tprg:'X')
049800090415     c                   eval      %subst(skord1(1):4:2) = sktnt1(1)
049900051202
050000051202     c                   EndSr
050100140722      *
050200140722      *------------------------------------------------------------------------*
050300140722      * Caricamento Contatti (Tipi Note) del singolo Raggruppamento Rubrica
050400140722      *------------------------------------------------------------------------*
050500140722     c     sr_RGRnot     BegSr
050600140722      *
050700140722     c                   clear                   skTnt1
050800140722     c                   clear                   skOrd1
050900140722     c                   clear                   zz
051000140722      *
051100140722     c                   For       yy = 1  To %elem(skTnt1)
051200140722      *
051300140722     c                   if        sk_Rgr(yy) = TA12rgr
051400140722      *
051500140722     c                   eval      zz += 1
051600140722     c                   eval      skTnt1(zz) = skTnt(yy)
051700140722     c                   eval      skOrd1(zz) = %editc( sk_Prg(yy) : 'X' )
051800140722     c                                        + skTnt(yy)
051900140722      *
052000140722     c                   endif
052100140722      *
052200140722     c                   EndFor
052300140722      *
052400140722     c                   sorta     skOrd1
052500140722      *
052600140722     c                   EndSr
052700140722      *
052800140722      *---------------------------------------------------------------*
052900140722      *?Gestione subfile S04                                         ?*
053000140722      *---------------------------------------------------------------*
053100140722     c     sr_GesS04     BEGSR
053200140722      *
053300140722      * -?Inizializzazione della videata?
053400140722if  1c                   if        $InzS04 = *on
053500140722     c                   exsr      sr_InzS04
053600140722     c                   eval      $InzS04 = *off
053700140722e   1c                   endif
053800140722      *
053900140722      * -?Posizionameno del cursore?
054000140722if  1c                   if        C04csr  > *zeros
054100140722     c                   z-add     C04csr        C04rcd
054200140722x   1c                   else
054300140722     c                   z-add     1             C04rcd
054400140722e   1c                   endif
054500140722      *
054600140722      * -?Emissione Testata e Piede con tasti funzionali abilitati?
054700140722     c                   write     TA12T01
054800140722     c                   write     TA12Z04
054900140722      * -?Emissione videata?
055000140722     c                   exfmt     TA12C04
055100140722     c                   setoff                                       28  90
055200140722     c                   clear                   V4Dmsg
055300140722      *
055400140722sel 1c                   SELECT
055500140722      * -?F3=Fine?
055600140722w   1c                   WHEN      *inKC
055700140722     c                   goto      Fine
055800140722      * -?F10=Inserimento?
055900140722w   1c                   WHEN      *inKJ
056000140722     c                   exsr      sr_F10
056100140722     c                   if        *in28
056200140722     c                   eval      V4Dmsg = V2Cmsg
056300140722     c                   else
056400140722     c                   eval      $InzS04 = *on
056500140722     c*//                goto      CarSfl
056600140722     c                   endif
056700140722      * -?F12=Ritorno?
056800140722w   1c                   WHEN      *inKL
056900140722     c   01              goto      Fine
057000140722     c  n01              goto      Emid02
057100140722      *
057200140722x   1c                   OTHER
057300140722      * -?Gestione Opzioni?
057400140722     c                   exsr      sr_OpzS04
057500140722if  2c                   if        *in90
057600140722     c                   leavesr
057700140722e   2c                   endif
057800140722      *
057900140722e   1c                   ENDSL
058000140722      *
058100140722     c                   ENDSR
058200140722      *
058300140722      *---------------------------------------------------------------*
058400140722      *?Inizializzazione videata C04/S04                             ?*
058500140722      *---------------------------------------------------------------*
058600140722     c     sr_InzS04     BEGSR
058700140722      *
058800140722      * -?Pulizia subfile?
058900140722     c                   setoff                                       2223
059000140722     c                   write     TA12C04
059100140722     c                   seton                                        2223
059200140722     c                   eval      *in33 = *off
059300140722      *
059400140722     c                   clear                   C04rcd
059500140722     c                   clear                   C04csr
059600140722     c                   clear                   S04nrr
059700140722     c                   clear                   V4Dmsg
059800140722     c                   setoff                                       28  90
059900140722      *
060000140722      * Imposto i dati del control
060100140722     c                   eval      V4Cksc = V1Cksc
060200140722     c                   eval      V4Cpot = V1Cpot
060300140722     c                   eval      V4Cnrv = V1Cnrv
060400140722     c                   eval      V4Dksc = V1Dksc
060500140722     c                   eval      V4Dpot = V1Dpot
060600140722     c                   eval      V4Dnrv = V1Dnrv
060700140722      *
060800140722     c                   eval      *in42 = *off
060900140722      *
061000140722      * Imposto la chiave per tfntc
061100140722     c                   eval      Kapl = V1Capl
061200140722     c   03              movel     DUTkci        Knk1
061300140722     c   03              move      V4Cksc        Knk1
061400140722     c   04              move      V4Cpot        Knk1
061500140722     c   11              movel     DUTkci        Knk1
061600140722     c   11              move      V4Cnrv        Knk1
061700140722     c                   clear                   Knk2
061800140722      *
061900140722      * -?Caricamento dati dalle tab. "1TE"/"1T"?
062000140722for 1c                   For       yy = 1  To %elem(sk_Tot)
062100140722      *
062200140722if  2c                   if        sk_Tot(yy) = *blanks
062300140722     c                   iter
062400140722e   2c                   endif
062500140722      *
062600140722     c                   exsr      sr_WrtS04
062700140722      *
062800140722e   1c                   EndFor
062900140722      *
063000140722      * -?Visualizzazione del SFL (se ci sono dati)?
063100140722     c                   eval      *in22   = (S04nrr > *zeros)
063200140722      *
063300140722      * -?Attivazione del SFLEND?
063400140722     c                   eval      *in33   = *on
063500140722      *
063600140722      * -?Posizionamento cursore al 1° rcd del subfile?
063700140722if  1c                   if        S04nrr  > *zeros
063800140722     c                   eval      C04rcd  = 1
063900140722x   1c                   else
064000140722     c                   clear                   C04rcd
064100140722e   1c                   endif
064200140722     c                   eval      C04csr  = C04rcd
064300140722      *
064400140722     c                   ENDSR
064500140722      *
064600140722      *---------------------------------------------------------------*
064700140722      *?Impostazione dati in singolo record del sfl S04              ?*
064800140722      *---------------------------------------------------------------*
064900140722     c     sr_WrtS04     BEGSR
065000140722      *
065100140722     c                   clear                   TA12S04
065200140722     c                   eval      V4rgr  = %subst( sk_Tot(yy) : 3 )
065300140722      *
065400140722      * -?Verifica esistenza dati per il singolo tipo nota in elaboraz.?
065500140722for 1c                   For       zz = 1  To %elem(skTnt)
065600140722      *
065700140722if  2c                   if        sk_Rgr(zz) = V4rgr
065800140722      *
065900140722     c                   add       1             V4max
066000140722     c                   eval      kTnt = skTnt(zz)
066100140722     c     kTFntc        setll     TFNTC
066200140722      * Trovo il contatto lo conteggio nel subfile
066300140722if  3c                   if        %equal(TFNTC01L)
066400140722     c                   add       1             V4tot
066500140722e   3c                   endif
066600140722      *
066700140722e   2c                   endif
066800140722      *
066900140722e   1c                   EndFor
067000140722      *
067100140722      * -?Scrittura record nel subfile?
067200140722if  1c                   If        V4max  > *zero
067300140722      *
067400140722     c                   clear                   d1TE
067500140722     c                   eval      TBEcod = '1TE'
067600140722     c                   eval      TBEke1 = V4rgr
067700140722     c                   clear                   TBEke2
067800140722     c     k03tntbe01    chain     TNTBE000
067900140722      *
068000140722if  2c                   if        %found(TNTBE01L)
068100140722     c                   eval      d1TE = TBEuni
068200140722e   2c                   endif
068300140722      *
068400140722     c                   eval      V4ord    = §1TEord
068500140722     c                   eval      V4rgrDes = V4rgr + ' - '
068600140722     c                                      + §1TEdes
068700140722      *
068800140722     c                   add       1             S04nrr
068900140722     c                   write     TA12S04
069000140722      *
069100140722e   1c                   EndIf
069200140722      *
069300140722     c                   ENDSR
069400140722      *
069500140722      *---------------------------------------------------------------*
069600140722      *?Impostazione dati in singolo record del sfl S04              ?*
069700140722      *---------------------------------------------------------------*
069800140722     c     sr_OpzS04     BEGSR
069900140722      *
070000140722     c                   clear                   SavS04csr
070100140722      *
070200140722for 1c                   FOR       S04nrr = 1  To *hival
070300140722      *
070400140722     c     S04nrr        chain     TA12S04
070500140722      * Esco se fine sfl
070600140722     c                   if        Not  %found(TNTA12D)
070700140722     c                   leave
070800140722     c                   endif
070900140722      *
071000140722     c                   z-add     S04nrr        C04rcd
071100140722      *
071200140722sel 2c                   SELECT
071300140722      * -?Nessuna opzione?
071400140722w   2c                   WHEN      V4opz   = *blanks
071500140722      * -?X = Dettaglio Raggruppamento?
071600140722w   2c                   WHEN      V4opz   = 'X'
071700140722     c                   exsr      sr_OpzS4_X
071800140722e   2c                   ENDSL
071900140722      *
072000140722      * -?Memorizzazione nrr del sfl con la 1ª opzione?
072100140722      *  ?per riposizionarvici il cursore dopo il tasto "Invio"?
072200140722if  2c                   if             V4opz    <> *blanks
072300140722     c                             and (SavS04csr = *zeros
072400140722     c                              or  SavS04csr < C04rcd)
072500140722     c                   eval      SavS04csr   = C04rcd
072600140722e   2c                   endif
072700140722      *
072800140722      * -?Aggiornamento sfl?
072900140722sel 2c                   select
073000140722w   2c                   when      *in28
073100140722     c                   z-add     C04rcd        C04csr
073200140722w   2c                   when      *in90       = *on
073300140722     c                   z-add     C04rcd        C04csr
073400140722     c                   clear                   V4opz
073500140722x   2c                   other
073600140722     c                   clear                   V4opz
073700140722e   2c                   endsl
073800140722     c                   UPDATE    TA12S04
073900140722if  2c                   if        *in28  OR  *in90
074000140722     c                   leave
074100140722e   2c                   endif
074200140722      *
074300140722e   1c                   ENDFOR
074400140722      *
074500140722      * -?Riposiziono il cursore sul record contenente la prima opzione?
074600140722      *  ?(se non sono stati rilevati errori)?
074700140722if  1c                   if        NOT *in28  and  NOT *in90
074800140722     c                             and SavS04csr  > *zeros
074900140722     c                   z-add     SavS04csr     C04csr
075000140722e   1c                   endif
075100140722      *
075200140722     c                   ENDSR
075300140722      *
075400140722      *---------------------------------------------------------------*
075500140722      *?S4_Opz.X = Dettaglio singolo Raggruppamento Rubrica          ?*
075600140722      *---------------------------------------------------------------*
075700140722     c     sr_OpzS4_X    BEGSR
075800140722      *
075900140722     c                   eval      TA12rgr = V4rgr
076000140722     c                   exsr      sr_RGRnot
076100140722     c                   clear                   TA12rgr
076200140722      *
076300140722     c                   exsr      sr_PulSfl
076400140722     c                   exsr      sr_CarSfl
076500140722     c                   exsr      sr_EmiSfl
076600140722      *
076700140722     c                   ENDSR
076800051201
076900051201      *------------------------------------------------------------------------*
077000051201      * PULISCO IL SUBFILE
077100051201      *------------------------------------------------------------------------*
077200051201     c     Sr_Pulsfl     BegSr
077300051201
077400051206     c                   Clear                   nrr1
077500051202     c                   Clear                   v2cksc
077600051202     c                   Clear                   v2cpot
077700051202     c                   Clear                   v2cnrv
077800051201     c                   Eval      *In20 = *Off
077900051201     c                   Eval      *In21 = *Off
078000051202     c                   Write     ta12c02
078100051201     c                   Eval      *In21 = *On
078200051201     c                   Eval      *In20 = *On
078300051201
078400051206     c                   Eval      recsf1 = 1
078500051201     c                   Eval      *In31 = *Off
078600051201
078700051201     c                   EndSr
078800051201
078900051201      *------------------------------------------------------------------------*
079000051201      * CARICO IL SUBFILE
079100051201      *------------------------------------------------------------------------*
079200051201     c     Sr_Carsfl     BegSr
079300051202
079400051202      * Imposto i dati del control
079500051202     c                   Eval      v2cksc = v1cksc
079600051202     c                   Eval      v2cpot = v1cpot
079700051202     c                   Eval      v2cnrv = v1cnrv
079800051202     c                   Eval      v2dksc = v1dksc
079900051202     c                   Eval      v2dpot = v1dpot
080000051202     c                   Eval      v2dnrv = v1dnrv
080100051206
080200051206     c                   Eval      *In42 = *Off
080300051201
080400051202      * Imposto la chiave per tfntc
080500051202     c                   Eval      kapl = v1capl
080600051202     c                   Clear                   knk2
080700051202     c   03              Movel     dutkci        knk1
080800051202     c   03              Move      v2cksc        knk1
080900051202     c   04              Move      v2cpot        knk1
081000091112     c   11              Movel     dutkci        knk1
081100091112     c   11              Move      v2cnrv        knk1
081200051202
081300051202      * Carico il subfile (ciclo per i tipi contatti caricati in sk)
081400051202     c                   Eval      xx = 1
081500090406     c                   Eval      zz = 1
081600090406for 1c                   For       zz by 1 to 99
081700090415     c                   if        skord1(zz) = *blanks
081800090406     c                   iter
081900051202    2c                   EndIf
082000090406      * recupero la sk della nota
082100090415     c                   eval      xx = %lookup(%subst(skord1(zz):4:2):sktnt1)
082200051202     c                   Clear                   riga
082300051202      * Cerco i dati del tipo contatto in tabella 1T
082400051202     c                   Clear                   ds1t
082500051202     c                   Eval      kcod = '1T'
082600051202     c                   Eval      kkey = sktnt1(xx)
082700051202     c     kTabel        Chain     Tabel00f
082800051202if  2c                   If        Not %Found(Tabel00f) or tblflg <> *Blanks
082900051202     c                   Iter
083000051202    2c                   EndIf
083100051202     c                   Eval      ds1t = tbluni
083200140722     c                   eval      V2notaD = %subst(tblkey:1:2) + ' - '
083300140722     c                   eval      V2nota  = §1tdes
083400051202     c                   Clear                   v2email
083500051205     c                   Clear                   v2cann
083600051205     c                   Clear                   v2nrrr
083700051206     c                   Clear                   v2csns
083800090406     c                   clear                   v2prg
083900051205     c                   Eval      v2cmod = 'S'
084000051202     c                   Eval      *In07 = *On
084100051207     c                   Eval      *In10 = *Off
084200051206     c                   Add       1             nrr1
084300051202     c                   Write     ta12s02
084400051202     c                   Eval      ktnt = sktnt1(xx)
084500051202     c     kTfntc        Setll     Tfntc01l
084600051202      * Trovo il contatto lo scrivo nel subfile
084700051202if  2c                   If        %Equal
084800051202do  3c                   Do        *Hival
084900051205     c     kTfntc        Reade     Tfntc01l
085000051202     c                   If        %Eof(Tfntc01l)
085100051202     c                   Leave
085200051202     c                   EndIf
085300140722     c                   clear                   V2notaD
085400051202     c                   Eval      v2nota = ntcrnt
085500051202     c                   Eval      v2email = §1teml
085600051205     c                   Eval      v2ctnt = ntctnt
085700051205     c                   Eval      v2cann = ntcflt
085800051205     c                   Eval      v2nrrr = ntcnrr
085900051206     c                   Eval      v2csns = ntcsns
086000090406     c                   eval      v2prg  = §1tprg
086100051205     c                   Clear                   v2cmod
086200051202     c                   Eval      *In07 = *Off
086300051202      * Proteggo nota se richiesto da tabella 1T
086400051207      * o se sola interrogazione
086500140530     c                   If        (§1tpro  =  'S'   and
086600140530     c                             (Not *in08        or
086700140530     c                             (§1TnPro = *blank and *in08)))
086800140530     c                                               or  *In09
086900051202     c                   Eval      *In07 = *On
087000051207     c   09              Eval      *In10 = *On
087100051205     c                   Eval      v2cmod = 'S'
087200051202     c                   EndIf
087300051206     c                   Add       1             nrr1
087400051202     c                   Add       1             riga
087500051202     c                   Write     ta12s02
087600051202    3c                   EndDo
087700051202      * Carico n righe vuote in base a quante ne ho già scritte
087800051207      * se non sono protette
087900051207      * o se non è sola interrogazione
088000051207if  3c                   If        riga < §1trig and
088100140603     c                             (§1tpro  <> 'S'    or
088200140603     c                             (§1TnPro <> *blank or Not *in08))
088300140603     c                                                or  *In09
088400051202for 2c                   For       riga = riga + 1 by 1 to §1trig
088500140722     c                   clear                   V2notaD
088600051202     c                   Clear                   v2nota
088700051202     c                   Eval      v2email = §1teml
088800051205     c                   Eval      v2ctnt = sktnt1(xx)
088900051205     c                   Clear                   v2cann
089000051205     c                   Clear                   v2nrrr
089100051205     c                   Clear                   v2cmod
089200090406     c                   eval      v2prg  = §1tprg
089300051206     c                   Eval      v2csns = 'S'
089400051202     c                   Eval      *In07 = *Off
089500051206     c                   Add       1             nrr1
089600051202     c                   Write     ta12s02
089700051202    2c                   EndFor
089800051202    3c                   EndIf
089900051202      * Non trovo il contatto scrivo righe vuote nel subfile
090000051202   x2c                   Else
090100051202      * Scrivo righe vuote solo se non è richiesta la protezione da tabella 1T
090200051207      * o se non è solo interrogazione
090300140530     c                   If        (§1tpro  =  'S'   and
090400140530     c                             (Not *in08        or
090500140530     c                             (§1TnPro = *blank and *in08)))
090600140530     c                                               or  *In09
090700051202     c                   Iter
090800051202    3c                   EndIf
090900051202     c                   Eval      yy = 1
091000051202for 3c                   For       yy by 1 to §1trig
091100140722     c                   clear                   V2notaD
091200051202     c                   Clear                   v2nota
091300051202     c                   Eval      v2email = §1teml
091400051205     c                   Eval      v2ctnt = sktnt1(xx)
091500051205     c                   Clear                   v2cann
091600051205     c                   Clear                   v2nrrr
091700051205     c                   Clear                   v2cmod
091800090406     c                   eval      v2prg  = §1tprg
091900051206     c                   Eval      v2csns = 'S'
092000051202     c                   Eval      *In07 = *Off
092100051206     c                   Add       1             nrr1
092200051202     c                   Write     ta12s02
092300051202    3c                   EndFor
092400051202    2c                   EndIf
092500051202    1c                   EndFor
092600090406
092700051201     c                   Eval      *In31 = *On
092800051201
092900051202     c                   EndSr
093000051202
093100051202      *------------------------------------------------------------------------*
093200051202      * EMETTO IL SUBFILE
093300051202      *------------------------------------------------------------------------*
093400051202     c     Sr_Emisfl     BegSr
093500051201
093600051202do  1c                   Do        *Hival
093700051205
093800051206     c  n28              Z-add     1             recsf1
093900051206
094000051202     c                   Write     ta12t01
094100051202     c                   Write     ta12z02
094200051202     c                   Exfmt     ta12c02
094300051201
094400051201     c                   Eval      *In28 = *Off
094500051202     c                   Clear                   v2cmsg
094600051201
094700051202      * F3=Fine
094800051202     c   kc              Goto      fine
094900051202
095000051201      * F12=Ritorno
095100051207     c                   If        *InKl
095200140722     c                   if            %parms() > 1
095300140722     c                             and  TA12tnt = *blanks
095400140722     c                             and  TA12rgr = *blanks
095500140722     c                             and  TA12tot = *blanks
095600140722     c                   leavesr
095700140722     c                   endif
095800051207     c   01              Goto      fine
095900051207     c  n01              Goto      emid02
096000051207     c                   EndIf
096100051206
096200051206      * F10=Inserimento Multiplo E-Mail
096300051206     c                   If        *Inkj
096400051206     c                   ExSr      Sr_F10
096500140722     c                   if            %parms() > 1
096600140722     c                             and  TA12tnt = *blanks
096700140722     c                             and  TA12rgr = *blanks
096800140722     c                             and  TA12tot = *blanks
096900140722     c                             and  *in28
097000140722     c                   iter
097100140722     c                   endif
097200140722     c                   eval      $InzS04 = *on
097300051206     c                   Goto      carsfl
097400051206     c                   EndIf
097500051201
097600051201      * Controllo i dati immessi nel subfile
097700051202     c                   ExSr      Sr_Ctrsfl
097800051202     c   28              Iter
097900051202
098000051202      * F6=Conferma
098100051205     c                   If        *Inkf
098200051205     c                   ExSr      Sr_Conferma
098300140722     c                   eval      $InzS04 = *on
098400140722     c                   if            %parms() > 1
098500140722     c                             and  TA12tnt = *blanks
098600140722     c                             and  TA12rgr = *blanks
098700140722     c                             and  TA12tot = *blanks
098800140722     c                   leavesr
098900140722     c                   endif
099000051207     c                   If        *In01
099100051207     c                   Goto      fine
099200051207     c                   EndIf
099300051202     c                   Leave
099400051205     c                   EndIf
099500051205
099600051202    1c                   EndDo
099700051201
099800051201     c                   EndSr
099900051202
100000051202      *------------------------------------------------------------------------*
100100051202      * CONTROLLO I DATI DEL SUBFILE
100200051202      *------------------------------------------------------------------------*
100300051202     c     Sr_Ctrsfl     BegSr
100400051202
100500051206     c                   Clear                   nrr1
100600051202do  1c                   Do        *Hival
100700051206     c                   Add       1             nrr1
100800051206     c     nrr1          Chain     ta12s02                            32
100900051205      * Esco se fine sfl
101000051202     c                   If        *In32
101100051202     c                   Leave
101200051202     c                   EndIf
101300051206      * Non controllo se riga protetta
101400051206     c                   If        v2cmod = 'S'
101500051206     c                   Iter
101600051206     c                   EndIf
101700051202
101800051202     c                   setoff                                       42
101900051205
102000051205      * Se prima la nota c'era ora non posso cancellarla
102100051205if  2c                   If        v2nrrr <> *Zeros and v2nota = *Blanks
102200051205     c                   Eval      *In28 = *On
102300051205     c                   Eval      *In42 = *On
102400051205     c                   Eval      v2cmsg = msg(11)
102500051206     c                   Eval      recsf1 = nrr1
102600051205     c                   Update    ta12s02
102700051205     c                   Leave
102800051205    2c                   EndIf
102900051202
103000051202      * Controllo se indirizzo e-mail
103100051202if  2c                   If        v2email = 'S' and v2nota <> *Blanks
103200051202     c                   Clear                   dsemail
103300051202     c                   Eval      §emlindi = v2nota
103400051202     c                   Call      'XEMAIL'
103500051202     c                   Parm                    dsemail
103600051202      * Errore
103700051202if  3c                   If        §emlerro <> '0'
103800051202     c                   Eval      *In28 = *On
103900051202     c                   Eval      *In42 = *On
104000051202     c                   Eval      v2cmsg = §emlmsgo
104100051206     c                   Eval      recsf1 = nrr1
104200051205     c                   Update    ta12s02
104300051202     c                   Leave
104400051202   x3c                   else
104500140722     c                   clear                   V2notaD
104600051202     c                   Eval      v2nota = §emlindo
104700051202    3c                   EndIf
104800051202    2c                   EndIf
104900051202
105000051206     c                   Update    ta12s02
105100051202    1c                   EndDo
105200051202
105300051202     c                   EndSr
105400051202
105500051202      *------------------------------------------------------------------------*
105600051202      * CONFERMA
105700051202      *------------------------------------------------------------------------*
105800051202     c     Sr_Conferma   BegSr
105900051205
106000051206     c                   Clear                   nrr1
106100051205do  1c                   Do        *Hival
106200051206     c                   Add       1             nrr1
106300051206     c     nrr1          Chain     ta12s02                            32
106400051205      * Esco se fine sfl
106500051205     c                   If        *In32
106600051205     c                   Leave
106700051205     c                   EndIf
106800051205      * Torno a leggere se nota non modificabile
106900051205     c                   If        v2cmod = 'S'
107000051205     c                   Iter
107100051205     c                   EndIf
107200051205      * Torno a leggere se non immessa la nota
107300051205     c                   If        v2nota = *Blanks
107400051205     c                   Iter
107500051205     c                   EndIf
107600051205
107700051205      * C'è il nrr nel subfile contatto già esistente aggiorno/deleto
107800051205if  2c                   If        v2nrrr <> *Zeros
107900051205     c                   Eval      ntcnrr = v2nrrr
108000051205     c     ntcnrr        Chain     tfntc00f
108100051205if  3c                   If        %Found(tfntc00f)
108200051205     c                   Eval      ntcrnt = v2nota
108300051205     c                   Move      *date         ntcntr
108400051205     c                   Eval      ntcflt = v2cann
108500051206     c                   Eval      ntcsns = v2csns
108600051205      * Aggiorno
108700051205if  4c                   If        ntcflt = *Blanks
108800051205     c                   Update    tfntc0
108900051205      * Deleto
109000051205   x4c                   Else
109100051205     c                   Delete    tfntc0
109200051205    4c                   EndIf
109300051205    3c                   EndIf
109400051205
109500051205      * Non c'è il nrr sul subfile è rcd nuovo scrivo
109600051205   x2c                   Else
109700051205if  3c                   If        v2cann = *Blanks
109800090806     c                   Clear                   tfntc0
109900051205     c                   Eval      ntcapl = kapl
110000051205     c                   Eval      ntcnk1 = knk1
110100051205     c                   Eval      ntctnt = v2ctnt
110200051205     c                   Eval      ntcrnt = v2nota
110300051206     c                   Eval      ntcsns = v2csns
110400051205     c                   Move      *date         ntcntr
110500051205      * Scrivo il contatto
110600051205     c                   Write     tfntc0
110700051205    3c                   EndIf
110800051205    2c                   EndIf
110900051205
111000051205    1c                   EndDo
111100051202
111200051202     c                   EndSr
111300051206
111400051206      *------------------------------------------------------------------------*
111500051206      * TASTO FUNZIONE F10 - INSERIMENTO MULTIPLO MAIL
111600051206      *------------------------------------------------------------------------*
111700051206     c     Sr_F10        BegSr
111800051206
111900051206      * Pulisco il subfile
112000051206     c                   Clear                   nrr2
112100051206     c                   Clear                   v3cksc
112200051206     c                   Clear                   v3cpot
112300051206     c                   Clear                   v3cnrv
112400051206     c                   Eval      *In20 = *Off
112500051206     c                   Eval      *In21 = *Off
112600051206     c                   Write     ta12c03
112700051206     c                   Eval      *In21 = *On
112800051206     c                   Eval      *In20 = *On
112900051206
113000051206     c                   Eval      recsf2 = 1
113100051206     c                   Eval      *In31 = *Off
113200051206
113300051206      * Imposto i dati del control
113400051206     c                   Eval      v3cksc = v1cksc
113500051206     c                   Eval      v3cpot = v1cpot
113600051206     c                   Eval      v3cnrv = v1cnrv
113700051206     c                   Eval      v3dksc = v1dksc
113800051206     c                   Eval      v3dpot = v1dpot
113900051206     c                   Eval      v3dnrv = v1dnrv
114000051206     c                   Clear                   v3email
114100051206
114200051206      * Imposto la chiave per tfntc
114300051206     c                   Eval      kapl = v1capl
114400051206     c                   Clear                   knk2
114500051206     c   03              Movel     dutkci        knk1
114600051206     c   03              Move      v3cksc        knk1
114700051206     c   04              Move      v3cpot        knk1
114800091112     c   11              Movel     dutkci        knk1
114900091112     c   11              Move      v3cnrv        knk1
115000051206
115100051206      * Carico il subfile in base alla sk dei tipi nota che prevedono e-mail
115200060329      * e che sono gestibili dall'immissione multipla
115300051206     c                   Eval      xx = 1
115400090406     c                   Eval      zz = 1
115500090406for 1c                   For       zz by 1 to 99
115600090406     c                   if        skord(zz) = *blanks
115700090406     c                   iter
115800051206     c                   EndIf
115900090406      * recupero la sk della nota
116000090406     c                   eval      xx = %lookup(%subst(skord(zz):4:2):sktnt)
116100051206      * Solo le note e-mail
116200051206     c                   If        skceml(xx) = *Blanks
116300051206     c                   Iter
116400051206     c                   EndIf
116500060329      * Solo le gestibili
116600060329     c                   If        skmult(xx) <> *Blanks
116700060329     c                   Iter
116800060329     c                   EndIf
116900051206      * Se non esistono scrivo subfile
117000051206     c                   Eval      ktnt = sktnt(xx)
117100051206     c     kTfntc        Chain     Tfntc01l
117200051206     c                   If        %Found(Tfntc01l) and ntcflt <> 'A'
117300051206     c                   Iter
117400051206     c                   EndIf
117500051206     c                   Eval      v3nota = sktnt(xx) + ' - ' + skdtnt(xx)
117600051206     c                   Clear                   v3scelta
117700051206     c                   Eval      v3ctnt = sktnt(xx)
117800051206     c                   Add       1             nrr2
117900051206     c                   Write     ta12s03
118000051206    1c                   EndFor
118100051206     c                   Eval      *In31 = *On
118200051206
118300051206      * Se non ho caricato nessuna e-mail non emetto la videata
118400051206     c                   If        nrr2 = *Zeros
118500060329     c                   Eval      v2cmsg = 'già inserite tutte le e-mail gesti-
118600060329     c                             bili'
118700051206     c                   Eval      *In28 = *On
118800140722     c                   if            %parms() > 1
118900140722     c                             and  TA12tnt = *blanks
119000140722     c                             and  TA12rgr = *blanks
119100140722     c                             and  TA12tot = *blanks
119200140722     c                   leavesr
119300140722     c                   else
119400051206     c                   Goto      carsfl
119500140722     c                   endif
119600051206     c                   EndIf
119700051206
119800051206      * Emetto il subfile
119900051206     c     emisfl2       Tag
120000051206do  1c                   Do        *Hival
120100051206
120200051206     c  n28              Z-add     1             recsf2
120300051206
120400051206     c                   Write     ta12t01
120500051206     c                   Write     ta12z03
120600051206     c                   Exfmt     ta12c03
120700051206
120800051206     c                   Eval      *In28 = *Off
120900051206     c                   Clear                   v3cmsg
121000051206
121100051206      * F3=Fine
121200051206     c   kc              Goto      fine
121300051206
121400051206      * F12=Ritorno
121500051206     c   kl              Goto      carsfl
121600051206
121700051206      * Controllo che sia inserita l'e-mail da creare
121800051206     c                   If        v3email = *Blanks
121900051206     c                   Eval      *In28 = *On
122000051206     c                   Eval      v3cmsg = msg(12)
122100051206     c                   Goto      emisfl2
122200051206     c                   EndIf
122300051206
122400051206      * Controllo che l'e-mail inserita sia valida
122500051206if  2c                   If        v3email <> *Blanks
122600051206     c                   Clear                   dsemail
122700051206     c                   Eval      §emlindi = v3email
122800051206     c                   Call      'XEMAIL'
122900051206     c                   Parm                    dsemail
123000051206if  3c                   If        §emlerro <> '0'
123100051206     c                   Eval      *In28 = *On
123200051206     c                   Eval      v3cmsg = §emlmsgo
123300051206     c                   Goto      emisfl2
123400051206   x3c                   Else
123500051206     c                   Eval      v3email = §emlindo
123600051206    3c                   EndIf
123700051206     c  nkf              Goto      emisfl2
123800051206    2c                   EndIf
123900051206      * F6=Conferma
124000051206if  2c                   If        *Inkf
124100051206     c                   Clear                   nrr2
124200051206do  3c                   Do        *Hival
124300051206     c                   Add       1             nrr2
124400051206     c     nrr2          Chain     ta12s03                            32
124500051206      * Esco se fine sfl
124600051206     c                   If        *In32
124700051206     c                   Leave
124800051206     c                   EndIf
124900051206      * Torno a leggere se non è stata scelta
125000051206     c                   If        v3scelta = *Blanks
125100051206     c                   Iter
125200051206     c                   EndIf
125300051206      * Scrivo le note
125400051206     c                   Clear                   tfntc
125500051206     c                   Eval      ntcapl = kapl
125600051206     c                   Eval      ntcnk1 = knk1
125700051206     c                   Eval      ntctnt = v3ctnt
125800051206     c                   Eval      ntcrnt = v3email
125900051206     c                   Eval      ntcsns = 'S'
126000051206     c                   Move      *date         ntcntr
126100051206      * Scrivo il contatto
126200051206     c                   Write     tfntc0
126300051206    3c                   EndDo
126400051206     c                   Leave
126500051206    2c                   EndIf
126600051206
126700051206    1c                   EndDo
126800051206
126900051206     c                   EndSr
127000051202
127100051201      *------------------------------------------------------------------------*
127200051201      * ROUTINE INIZIALE
127300051201      *------------------------------------------------------------------------*
127400051201     c     *Inzsr        BegSr
127500051201
127600051201     c     *Entry        Plist
127700051201     c                   Parm                    kpjba
127800051201     c                   Parm                    tnta12ds
127801170929
127802170929       sav_kpjbu = kpjbu;
127900051206
128000051206      * Controllo se richiamato e come
128100051206if  1c                   If        %Parms > 1
128200101019
128300051206if  2c                   If        ta12apl <> *Blanks
128400051201     c                   Eval      *In01 = *On
128500051206   x2c                   Else
128600051206     c                   Eval      *In01 = *Off
128700051206    2c                   EndIf
128800051207      * Chiamato per sola interrogazione
128900051207     c                   If        ta12ric = 'V'
129000051207     c                   Eval      *In09 = *On
129100051207     c                   Else
129200051207     c                   Eval      *In09 = *Off
129300051207     c                   EndIf
129400140530      * Chiamato dal *pgm Gestione del Credito
129500140530     c                   eval      *in08 = (TA12ric = 'C')
129600101019
129700101019      * da menù --> non richiamato
129800051206   x1c                   Else
129900051201     c                   Eval      *In01 = *Off
130000140530     c                   eval      *in08 = *off
130100051206    1c                   EndIf
130200051206
130300051201     c     *dtaara       define    §azute        azuteds
130400051201     c     *dtaara       define    §datiute      ddatiute
130500051201     c                   in(E)     *dtaara
130600051201     c                   If        %error  or rsut = *blanks
130700051201     c                   Clear                   tibs34ds
130800051201     c                   Call      'TIBS34R'
130900051201     c                   Parm                    tibs34ds
131000051201     c                   In        *dtaara
131100051201     c                   EndIf
131200051201
131300051201      * Controllo se sono in sede
131400051201     c                   Eval      *In12 = (simfel = *zeros)
131500051201
131600051201      * Carico tutti i contatti
131700051201     c                   Eval      kcod = '1T'
131800051201     c                   Clear                   xx
131900051201     c     kTabel01      Setll     Tabel00f
132000051201     c                   Do        *Hival
132100051201     c     kTabel01      Reade     Tabel00f
132200051201     c                   If        %Eof(Tabel00f)
132300051201     c                   Leave
132400051201     c                   EndIf
132500051201     c                   If        tblflg <> *Blanks
132600051201     c                   Iter
132700051201     c                   EndIf
132800051201     c                   Eval      ds1t = tbluni
132900051201     c                   If        §1trich <> 'C'
133000051201     c                   Iter
133100051201     c                   EndIf
133101170929       // se Rubrica Clienti Logistica carico solo note con 'L' in tabella
133103170929        IF  %subst(sav_kpjbu:1:3) = 'L' and §1Toklog <> 'L';
133104170928          iter;
133105170928        ENDIF;
133200051201     c                   Add       1             xx
133300051206     c                   Eval      ds1t = tbluni
133400051201     c                   Eval      sktnt(xx) = tblkey
133500051206     c                   Eval      skdtnt(xx) = §1tdes
133600051206     c                   Eval      skceml(xx) = §1teml
133700060329     c                   Eval      skmult(xx) = §1tmult
133800140722     c                   eval      sk_Prg(xx) = §1Tprg
133900140722     c                   eval      sk_Rgr(xx) = §1Trgr
134000090406     c                   eval      %subst(skord(xx):1:3) = %editc(§1tprg:'X')
134100090406     c                   eval      %subst(skord(xx):4:2) = sktnt(xx)
134200051201     c                   EndDo
134300140722      *
134400140722if  1c                   IF            %parms() > 1
134500140722     c                             and  TA12tnt = *blanks
134600140722     c                             and  TA12rgr = *blanks
134700140722     c                             and  TA12tot = *blanks
134800140722     c                   clear                   xx
134900140722     c                   movel     *hival        sk_Tot
135000140722     c     '1TE'         setll     TNTBE000
135100140722     c     '1TE'         reade     TNTBE000
135200140722do  2c                   DoW       Not %eof(TNTBE01L)
135300140722if  3c                   if        TBEatb = *blanks
135400140722     c                   eval      d1TE = TBEuni
135500140722     c                   eval      xx += 1
135600140722     c                   eval      sk_Tot(xx) = %editc( §1TEord : 'X' )
135700140722     c                                        + TBEke1
135800140722e   3c                   endif
135900140722     c     '1TE'         reade     TNTBE000
136000140722e   2c                   EndDo
136100140722     c                   sorta     sk_Tot
136200140722     c                   eval      xx += 1
136300140722     c                   movea     *blanks       sk_Tot(xx)
136400140722e   1c                   ENDIF
136500051201
136600051201      * KLIST
136700051201     c     kTabel        klist
136800051201     c                   kfld                    kkut
136900051201     c                   kfld                    kcod
137000051201     c                   kfld                    kkey
137100051201
137200051201     c     kTabel01      klist
137300051201     c                   kfld                    kkut
137400051201     c                   kfld                    kcod
137500140722
137600140722     c     k03tntbe01    klist
137700140722     c                   kfld                    TBEcod
137800140722     c                   kfld                    TBEke1
137900140722     c                   kfld                    TBEke2
138000051202
138100051202     c     kTfntc        klist
138200051202     c                   kfld                    kapl
138300051202     c                   kfld                    knk1
138400051202     c                   kfld                    knk2
138500051202     c                   kfld                    ktnt
138600051201
138700051201     c                   EndSr
138800090701      *------------------------------------------------------------------------*
138900090701      * CONTROLLO SE UTENTE ABILITATO
139000090701      *------------------------------------------------------------------------*
139100090701     c     Sr_Ctraut     BegSr
139101170929
139102170929      // per Rubrica Clienti Logistica abilitato solo utente RAM
139103170929       IF  Logistica and %subst(knmus:1:3) <> 'RAM';
139108170929         *in02 = *on;
139109170929         *in28 = *on;
139110170929         V1Cmsg = msg(03);
139111170929         V1Cmsg = %trim(V1Cmsg) + ' I CLIENTI';
139112170929         leavesr;
139114170929       ENDIF;
139146170928     c
139200090701     c                   clear                   tntaa1ds
139300090701     c   03              eval      itaa1caut='§utecli'
139400090701     c   04              eval      itaa1caut='§utepot'
139500091112     c   11              eval      itaa1caut='§utegtc'
139600090701     c                   clear                   kpjbu
139700090701     c                   movel     tntaa1ds      kpjbu
139800090703     c                   call      'TNTAA1C'
139900090701     c                   parm                    kpjba
140000090701     c                   movel     kpjbu         tntaa1ds
140100090701     c
140200090701    1c                   if        otaa1fabi='N'
140300090701     c                   Eval      *In02 = *On
140400090701     c                   Eval      *In28 = *On
140500090701     c                   Eval      v1cmsg = msg(03)
140600090701     c   03              eval      v1cmsg=%trim(v1cmsg)+' I CLIENTI   '
140700090701     c   04              eval      v1cmsg=%trim(v1cmsg)+' I POTENZIALI'
140800091112     c   11              eval      v1cmsg=%trim(v1cmsg)+' LE TRATTATIVE'
140900090701     c                   endif
140901170929
140902170929      // se Rubrica Clienti Logistica devo caricare le filiali logistica
140903170929      // abilitate all'utente
140904170929       IF  Logistica;
140905170929         clear TRUL31DS;
140906171113         I31abi = 'RL';
140907170929         I31cdi = DUTdis;
140908170929         I31car = DUTare;
140909170929         I31cpo = DUTpou;
140910170929         trul31r (kpjba:TRUL31DS);
140911170929         IF  O31pog > *zeros;
140912170929         // Carico in schiera solo le filiali di logistica abilitate all'utente
140913170929           IndicePOG = 1;
140914170929           clear IndiceskPOG;
140915170929           FOR IndicePOG by 1 to %elem(pog);
140916170929             IF  pog(IndicePOG) <> *zeros;
140917170929               w0030 = %dec(pog(IndicePOG):3:0);
140918170929               clear OG143;
140919170929               chain w0030 AZORG01L;
140920170929               IF  %found(AZORG01L);
140921170929                 OG143 = ORGde3;
140922170929               ENDIF;
140923170929               IF  §OGntw = 'LOG';
140924170929                 IndiceskPOG += 1;
140925170929                 skPOG(IndiceskPOG) = w0030;
140926170929               ENDIF;
140927170929             ENDIF;
140928170929           ENDFOR;
140929170929         ENDIF;
140930170929         leavesr;
140931170929       ENDIF;
141000090701     c
141100090701     c                   EndSr
141200090701
141300051201      *------------------------------------------------------------------------*
141400051201
141500051201** MSG  Lungh. 78                                                            *
141600051201Tipo applicazione errato                                                      01
141700051201Tipo applicazione non utlizzabile da utenti di SEDE                           02
141800090701UTENTE NON ABILITATO ALLA GESTIONE CONTATTI PER                               03
141900051202Cliente errato                                                                04
142000090630Cliente non in gestione all'utente                                            05
142100051202Potenziale errato                                                             06
142200090630Potenziale non in gestione all'utente                                         07
142300160614Il cliente immesso non è un cliente Logistica                                 08
142400170928Tipo contatto non utilizzabile per clienti Logistica                          09
142500051202Contatto errato                                                               10
142600051205Manca riga contatto                                                           11
142700051206Manca e-mail                                                                  12
