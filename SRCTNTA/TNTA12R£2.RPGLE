000100140722     /*PRM  dbgview(*source)
000200140722     /*END
000300140722
000400051201     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500051201
000600051201      *------------------------------------------------------------------------*
000700051201      *                                                                        *
000800051201      *                 GESTIONE CONTATTI                  ?                   *
000900051201      *                                                                        *
001000051201      *------------------------------------------------------------------------*
001100051201
001200051205     fTfntc00f  uf a e             disk    rename(tfntc:tfntc0)
001300051205     fTfntc01l  if   e           k disk    infds(ntcds)
001400051202     fTncpo01l  if   e           k disk
001500051201     fTabel00f  if   e           k disk
001600140722     fTNTBE01L  if   e           k disk
001700140722      *
001800051206     fTnta12D   cf   e             workstn sfile(ta12s02:nrr1)
001900051206     f                                     sfile(ta12s03:nrr2)
002000140722     f                                     sfile(TA12S04:S04nrr)
002100051201
002200051201      *------------------------------------------------------------------------*
002300051201      *  RIEPILOGO INDICATORI
002400051201      *------------------------------------------------------------------------*
002500051201      * 01 - Pgm richiamato
002600051201      * 02 - Utente non abilitato
002700051201      * 03 - Cliente
002800051201      * 04 - Potenziale
002900051202      * 06 - Seconda parte della prima videata
003000051202      * 07 - Proteggo campo nota a video
003100140530      * 08 - Eccezione per la modifica del contatto in Rubrica
003200140530      *      (altrimenti SOLO visualizzabile)
003300051207      * 09 - Solo interrogazione
003400051207      * 10 - Sottolineo campo nota sel sbfl 02 x interrogazione
003500090915      * 11 - Richiamato per trattativa
003600051201      * 12 - Sede - N12 Filiale
003700051201      * 20 - GESTIONE SUBFILE
003800051201      * 21 - GESTIONE SUBFILE
003900051201      * 22 - GESTIONE SUBFILE
004000051201      * 28 - ERRORE GENERICO DSPF
004100051201      * 30 - Comodo
004200051202      * 31 - Fine File
004300051202      * 32 - Fine Subfile
004400140722      * 33 - SubFileEND per S04/C04
004500051201      * 40 - Cliente/Potenziale/Visita
004600051201      * 41 - Contatto
004700051202      * 42 - Nota
004800051201      * 90 - Riemetto la videata
004900051201      *------------------------------------------------------------------------*
005000051201
005100051201      *   V A R I A B I L I
005200051201     d comando         s              1
005300051202     d kapl            s                   like(ntcapl)
005400051202     d kcod            s                   like(tblcod)
005500051201     d kkey            s                   like(tblkey)
005600051201     d kkut            s                   like(tblkut) inz(1)
005700051202     d knk1            s                   like(ntcnk1)
005800051202     d knk2            s                   like(ntcnk2)
005900051202     d ktnt            s                   like(ntctnt)
006000051206     d nrr1            s                   like(recsf1)
006100051206     d nrr2            s                   like(recsf2)
006200140722     d S04nrr          s                   like(C04rcd) inz
006300140722     d SavS04csr       s                   like(C04rcd) inz
006400051201     d ordina          s              1
006500051202     d riga            s                   like(§1trig)
006600051201     d tiporich        s              1    inz('C')
006700051202     d wcmm            s                   like(clpage)
006800051201     d wpos            s              2  0
006900051202     d wporagru        s              3
007000051201     d wtibs69         s              1    inz('0')
007100051202     d w003a           s              3
007200140722     d ww              s              3  0 inz
007300051202     d xx              s              3  0
007400051202     d yy              s              3  0
007500090406     d zz              s              3  0
007600140722     d $InzS04         s               n   inz(*on)
007700051201
007800051201      *   S C H I E R E
007900051201     d msg             s             78    Dim(60) ctdata perrcd(1)
008000051206     d skceml          s              1    Dim(99)
008100051206     d skdtnt          s             25    Dim(99)
008200060329     d skmult          s              1    Dim(99)
008300051201     d sktnt           s              2    Dim(99)
008400051202     d sktnt1          s              2    Dim(99)
008500090406     d skord           s              5    dim(99)
008600090415     d skord1          s              5    dim(99)
008700140722     d sk_Prg          s                   dim(99)  like(§1Tprg)
008800140722     d sk_Rgr          s                   dim(99)  like(§1Trgr)
008900140722     d sk_Tot          s              4    dim(99)
009000051201
009100051201      *   D S   I N T E R N E / E S T E R N E
009200051205     d ntcds           ds
009300051205     d ntcnrr                397    400B 0
009400051202
009500051201     d                 ds
009600051202     d v1cnrv1                 1      6  0
009700051202     d v1cnrv2                 7      7  0
009800051202     d v1cnrv                  1      7  0
009900051202     d                 ds
010000051202     d v2cnrv1                 1      6  0
010100051202     d v2cnrv2                 7      7  0
010200051202     d v2cnrv                  1      7  0
010300051206     d                 ds
010400051206     d v3cnrv1                 1      6  0
010500051206     d v3cnrv2                 7      7  0
010600051206     d v3cnrv                  1      7  0
010700051201
010800051201     d                sds
010900051201     d  Vtcpgm                 1     10
011000051201
011100051201     d Azuteds       e ds                  Extname(Azute00f)
011200051201     d dDatiute      e ds
011300051202     d dsemail       e ds
011400051201     d ds1t          e ds
011500051201     d ds1u          e ds
011600140722     d d1TE          e ds                  inz
011700051201     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
011800051201     d ds_cnind      e ds                  inz  extname(Cnind00f)
011900051201     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
012000051201     d ds_fncls      e ds                  inz  extname(Fncls00f)
012100051201     d Kpjba         e ds
012200051201     d Tnta12ds      e ds
012300051201     d Tibs02ds      e ds
012400051201     d Tibs34ds      e ds
012500051201     d Tibs69ds      e ds
012600090630     d tntaa1ds      e ds
012700051201
012800051201      *   C O S T A N T I
012900051201      * titolo videata (lunghezza massima 21)
013000051201     d VtcTit          C                   CONST('* GESTIONE CONTATTI *')
013100051201
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
029500140722if  2c*//                If        v1ctnt <> *Blanks
029600140722sel 2c                   Select
029700140722      * - - Ricevuto un Raggruppamento Rubrica
029800140722w   2c                   When           %parms() > 1
029900140722     c                             and  TA12rgr <> *blank
030000140722     c                   exsr      sr_RGRnot
030100140722      * - - Ricevuto un Codice Contatto
030200140722w   2c                   When      v1ctnt <> *Blanks
030300051202     c                   ExSr      Sr_Ctrtnt
030400140722     /*/ * - - Richiesto l'elenco dei Raggruppamenti
030500140722w   2/*/c                   When           %parms() > 1
030600140722     /*/c                             and  TA12tot  = *blank
030700140722     /*/c                   exsr      sr_TOTnot
030800140722      * - - Visualizzazione di tutti i Contatti
030900140722x   2c                   Other
031000140722     c*//                Else
031100051202     c                   Movea     sktnt         sktnt1
031200090415     c                   Movea     skord         skord1
031300140722     c                   sorta     skord1
031400140722    2c*//                EndIf
031500140722e   2c                   EndSl
031600140722      *
031700140722     c                   sorta     skord
031800051202
031900051202    1c                   EndIf
032000051201
032100051201     c                   EndSr
032200051201
032300051202      *------------------------------------------------------------------------*
032400051202      * CONTROLLO CLIENTE
032500051202      *------------------------------------------------------------------------*
032600051202     c     Sr_Ctrksc     BegSr
032700051202
032800051202     c                   If        v1cksc = *Zeros
032900051202     c                   Eval      *In28 = *On
033000051202     c                   Eval      *In40 = *On
033100051202     c                   Eval      v1cmsg = msg(04)
033200051202     c                   Leavesr
033300051202     c                   EndIf
033400051202
033500060216      * se richiamato da immissione cliente non controllo
033600060216     c                   If        *In01 and
033700060216     c                             ta12gcli = '1' and ta12icli = '1'
033800060217      * imposto la ragione sociale
033900060217     c                   Eval      v1dksc = ta12rag
034000060216     c                   goto      noksc
034100060216     c                   endif
034200060216
034300051202     c                   Clear                   Tibs69ds
034400051202     c                   Move      v1cksc        I69kac
034500051202     c                   Call      'TIBS69R'
034600051202     c                   Parm                    Tibs69ds
034700051202     c                   Parm                    ds_cnaco
034800051202     c                   Parm                    ds_cnind
034900051202     c                   Parm                    ds_cnclp
035000051202     c                   Parm                    ds_fncls
035100051202     c                   Eval      wtibs69 = *On
035200051202     c                   If        o69err = *Blanks
035300051202     c                   Eval      v1dksc = acorag
035400051202     c                   Else
035500051202     c                   Eval      *In28 = *On
035600051202     c                   Eval      *In40 = *On
035700051202     c                   Eval      v1cmsg = msg(04)
035800051202     c                   Leavesr
035900051202     c                   EndIf
036000060216     c     noksc         tag
036100051202
036200051202      * Controllo se utente può gestire il cliente
036300051207      * se non richiamato
036400051207if  1c                   If        Not *In01
036500090630     c                   clear                   tntaa1ds
036600090630     c                   eval      itaa1caut='§utecli'
036700090630     c                   eval      itaa1ksc=v1cksc
036800090630     c                   clear                   kpjbu
036900090630     c                   movel     tntaa1ds      kpjbu
037000090703     c                   call      'TNTAA1C'
037100090630     c                   parm                    kpjba
037200090630     c                   movel     kpjbu         tntaa1ds
037300090630     c
037400090630    1c                   if        otaa1fabi='N'
037500051202     c                   Eval      *In28 = *On
037600051202     c                   Eval      *In40 = *On
037700051202     c                   Eval      v1cmsg = msg(05)
037800051202     c                   Leavesr
037900051207    2c                   EndIf
038000051207    1c                   EndIf
038100051202
038200051202     c                   EndSr
038300051202
038400051202      *------------------------------------------------------------------------*
038500051202      * CONTROLLO POTENZIALE
038600051202      *------------------------------------------------------------------------*
038700051202     c     Sr_Ctrpot     BegSr
038800051202
038900051202     c                   If        v1cpot = *Zeros
039000051202     c                   Eval      *In28 = *On
039100051202     c                   Eval      *In40 = *On
039200051202     c                   Eval      v1cmsg = msg(06)
039300051202     c                   Leavesr
039400051202     c                   EndIf
039500051202
039600060216      * se richiamato da immissione cliente non controllo
039700060216     c                   If        *In01 and
039800060216     c                             ta12gcli = '1' and ta12icli = '1'
039900060217      * imposto la ragione sociale
040000060217     c                   Eval      v1dpot = ta12rag
040100060216     c                   goto      nopot
040200060216     c                   endif
040300060216
040400051202     c     v1cpot        Chain     Tncpo01l
040500051202     c                   If        %Found(Tncpo01l) and cpoatb = *Blanks
040600060217     c                   Eval      v1dpot = cporag
040700051202     c                   Else
040800051202     c                   Eval      *In28 = *On
040900051202     c                   Eval      *In40 = *On
041000051202     c                   Eval      v1cmsg = msg(06)
041100051202     c                   Leavesr
041200051202     c                   EndIf
041300060216     c     nopot         tag
041400051202
041500051202      * Controllo se utente può gestire il potenziale
041600100331      * se non richiamato
041700100331if  1c                   If        Not *In01
041800090630     c                   clear                   tntaa1ds
041900090702     c                   eval      itaa1cpo =cpocpo
042000090630     c                   eval      itaa1caut='§UTEPOT'
042100090630     c                   clear                   kpjbu
042200090630     c                   movel     tntaa1ds      kpjbu
042300090703     c                   call      'TNTAA1C'
042400090630     c                   parm                    kpjba
042500090630     c                   movel     kpjbu         tntaa1ds
042600090630     c
042700090630     c                   if        otaa1fabi='N'
042800051202     c                   Eval      *In28 = *On
042900051202     c                   Eval      *In40 = *On
043000051202     c                   Eval      v1cmsg = msg(07)
043100051202     c                   Leavesr
043200051202     c                   EndIf
043300100331     c                   endif
043400051202
043500051202     c                   EndSr
043600051202
043700051202      *------------------------------------------------------------------------*
043800051202      * CONTROLLO TIPO CONTATTO
043900051202      *------------------------------------------------------------------------*
044000051202     c     Sr_Ctrtnt     BegSr
044100051202
044200051202      * Ricerca
044300051202     c                   Eval      wpos = %scan('?':v1ctnt)
044400051202     c                   If        wpos > 0
044500051202     c                   Eval      kcod = '1T'
044600051202     c                   Call      'TRTB15R'
044700051202     c                   Parm                    kcod
044800051202     c                   Parm                    ordina
044900051202     c                   Parm                    tiporich
045000051202     c                   Parm                    kkey
045100051202     c                   Parm                    comando
045200051202     c                   Eval      v1ctnt = kkey
045300051202     c                   Eval      *In41 = *On
045400051202     c                   Eval      *In90 = *On
045500051202     c                   EndIf
045600051202
045700051202      * Controllo
045800051202     c                   Clear                   v1dtnt
045900051202     c                   Clear                   ds1t
046000051202     c                   Eval      kcod = '1T'
046100051202     c                   Eval      kkey = v1ctnt
046200051202     c     kTabel        Chain     Tabel00f
046300051202     c                   If        Not %Found(Tabel00f) or
046400051202     c                             tblflg <> *Blanks
046500051202     c                   Eval      *In28 = *On
046600051202     c                   Eval      *In41 = *On
046700051202     c                   Eval      v1cmsg = msg(10)
046800051202     c                   Leavesr
046900051202     c                   EndIf
047000051202
047100051202     c                   Eval      ds1t = tbluni
047200051202     c                   Eval      v1dtnt = §1tdes
047300051202     c                   Clear                   sktnt1
047400090415     c                   clear                   skord1
047500051202     c                   Eval      sktnt1(1) = v1ctnt
047600090415     c                   eval      %subst(skord1(1):1:3) = %editc(§1tprg:'X')
047700090415     c                   eval      %subst(skord1(1):4:2) = sktnt1(1)
047800051202
047900051202     c                   EndSr
048000140722      *
048100140722      *------------------------------------------------------------------------*
048200140722      * Caricamento Contatti (Tipi Note) del singolo Raggruppamento Rubrica
048300140722      *------------------------------------------------------------------------*
048400140722     c     sr_RGRnot     BegSr
048500140722      *
048600140722     c                   clear                   skTnt1
048700140722     c                   clear                   skOrd1
048800140722     c                   clear                   zz
048900140722      *
049000140722     c                   For       yy = 1  To %elem(skTnt1)
049100140722      *
049200140722     c                   if        sk_Rgr(yy) = TA12rgr
049300140722      *
049400140722     c                   eval      zz += 1
049500140722     c                   eval      skTnt1(zz) = skTnt(yy)
049600140722     c*//                eval      skOrd1(zz) = skOrd(yy)
049700140722     c                   eval      skOrd1(zz) = %editc( sk_Prg(yy) : 'X' )
049800140722     c                                        + skTnt(yy)
049900140722      *
050000140722     c                   endif
050100140722      *
050200140722     c                   EndFor
050300140722      *
050400140722     c                   sorta     skOrd1
050500140722      *
050600140722     c                   EndSr
050700140722      *
050800140722      *---------------------------------------------------------------*
050900140722      *?Gestione subfile S04                                         ?*
051000140722      *---------------------------------------------------------------*
051100140722     c     sr_GesS04     BEGSR
051200140722      *
051300140722      * -?Inizializzazione della videata?
051400140722if  1c                   if        $InzS04 = *on
051500140722     c                   exsr      sr_InzS04
051600140722     c                   eval      $InzS04 = *off
051700140722e   1c                   endif
051800140722      *
051900140722      * -?Posizionameno del cursore?
052000140722if  1c                   if        C04csr  > *zeros
052100140722     c                   z-add     C04csr        C04rcd
052200140722x   1c                   else
052300140722     c                   z-add     1             C04rcd
052400140722e   1c                   endif
052500140722      *
052600140722      * -?Emissione Testata e Piede con tasti funzionali abilitati?
052700140722     c                   write     TA12T01
052800140722     c                   write     TA12Z04
052900140722      * -?Emissione videata?
053000140722     c                   exfmt     TA12C04
053100140722     c                   setoff                                       28  90
053200140722     c                   clear                   V4Dmsg
053300140722      *
053400140722sel 1c                   SELECT
053500140722      * -?F3=Fine?
053600140722w   1c                   WHEN      *inKC
053700140722     c                   goto      Fine
053800140722      * -?F10=Inserimento?
053900140722w   1c                   WHEN      *inKJ
054000140722     c                   exsr      sr_F10
054100140722     c                   if        *in28
054200140722     c                   eval      V4Dmsg = V2Cmsg
054300140722     c                   else
054400140722     c                   eval      $InzS04 = *on
054500140722     c*//                goto      CarSfl
054600140722     c                   endif
054700140722      * -?F12=Ritorno?
054800140722w   1c                   WHEN      *inKL
054900140722     c   01              goto      Fine
055000140722     c  n01              goto      Emid02
055100140722      *
055200140722x   1c                   OTHER
055300140722      * -?Gestione Opzioni?
055400140722     c                   exsr      sr_OpzS04
055500140722if  2c                   if        *in90
055600140722     c                   leavesr
055700140722e   2c                   endif
055800140722      *
055900140722e   1c                   ENDSL
056000140722      *
056100140722     c                   ENDSR
056200140722      *
056300140722      *---------------------------------------------------------------*
056400140722      *?Inizializzazione videata C04/S04                             ?*
056500140722      *---------------------------------------------------------------*
056600140722     c     sr_InzS04     BEGSR
056700140722      *
056800140722      * -?Pulizia subfile?
056900140722     c                   setoff                                       2223
057000140722     c                   write     TA12C04
057100140722     c                   seton                                        2223
057200140722     c                   eval      *in33 = *off
057300140722      *
057400140722     c                   clear                   C04rcd
057500140722     c                   clear                   C04csr
057600140722     c                   clear                   S04nrr
057700140722     c                   clear                   V4Dmsg
057800140722     c                   setoff                                       28  90
057900140722      *
058000140722      * Imposto i dati del control
058100140722     c                   eval      V4Cksc = V1Cksc
058200140722     c                   eval      V4Cpot = V1Cpot
058300140722     c                   eval      V4Cnrv = V1Cnrv
058400140722     c                   eval      V4Dksc = V1Dksc
058500140722     c                   eval      V4Dpot = V1Dpot
058600140722     c                   eval      V4Dnrv = V1Dnrv
058700140722      *
058800140722     c                   eval      *in42 = *off
058900140722      *
059000140722      * Imposto la chiave per tfntc
059100140722     c                   eval      Kapl = V1Capl
059200140722     c   03              movel     DUTkci        Knk1
059300140722     c   03              move      V4Cksc        Knk1
059400140722     c   04              move      V4Cpot        Knk1
059500140722     c   11              movel     DUTkci        Knk1
059600140722     c   11              move      V4Cnrv        Knk1
059700140722     c                   clear                   Knk2
059800140722      *
059900140722      * -?Caricamento dati dalle tab. "1TE"/"1T"?
060000140722for 1c                   For       yy = 1  To %elem(sk_Tot)
060100140722      *
060200140722if  2c                   if        sk_Tot(yy) = *blanks
060300140722     c                   iter
060400140722e   2c                   endif
060500140722      *
060600140722     c                   exsr      sr_WrtS04
060700140722      *
060800140722e   1c                   EndFor
060900140722      *
061000140722      * -?Visualizzazione del SFL (se ci sono dati)?
061100140722     c                   eval      *in22   = (S04nrr > *zeros)
061200140722      *
061300140722      * -?Attivazione del SFLEND?
061400140722     c                   eval      *in33   = *on
061500140722      *
061600140722      * -?Posizionamento cursore al 1° rcd del subfile?
061700140722if  1c                   if        S04nrr  > *zeros
061800140722     c                   eval      C04rcd  = 1
061900140722x   1c                   else
062000140722     c                   clear                   C04rcd
062100140722e   1c                   endif
062200140722     c                   eval      C04csr  = C04rcd
062300140722      *
062400140722     c                   ENDSR
062500140722      *
062600140722      *---------------------------------------------------------------*
062700140722      *?Impostazione dati in singolo record del sfl S04              ?*
062800140722      *---------------------------------------------------------------*
062900140722     c     sr_WrtS04     BEGSR
063000140722      *
063100140722     c                   clear                   TA12S04
063200140722     c                   eval      V4rgr  = %subst( sk_Tot(yy) : 3 )
063300140722      *
063400140722      * -?Verifica esistenza dati per il singolo tipo nota in elaboraz.?
063500140722for 1c                   For       zz = 1  To %elem(skTnt)
063600140722      *
063700140722if  2c                   if        sk_Rgr(zz) = V4rgr
063800140722      *
063900140722     c                   add       1             V4max
064000140722     c                   eval      kTnt = skTnt(zz)
064100140722     c     kTFntc        setll     TFNTC
064200140722      * Trovo il contatto lo conteggio nel subfile
064300140722if  3c                   if        %equal(TFNTC01L)
064400140722     c                   add       1             V4tot
064500140722e   3c                   endif
064600140722      *
064700140722e   2c                   endif
064800140722      *
064900140722e   1c                   EndFor
065000140722      *
065100140722      * -?Scrittura record nel subfile?
065200140722if  1/*/c                   If        V4tot  > *zero
065300140722if  1c                   If        V4max  > *zero
065400140722      *
065500140722     c                   clear                   d1TE
065600140722     c                   eval      TBEcod = '1TE'
065700140722     c                   eval      TBEke1 = V4rgr
065800140722     c                   clear                   TBEke2
065900140722     c     k03tntbe01    chain     TNTBE000
066000140722      *
066100140722if  2c                   if        %found(TNTBE01L)
066200140722     c                   eval      d1TE = TBEuni
066300140722e   2c                   endif
066400140722      *
066500140722     c                   eval      V4ord    = §1TEord
066600140722     c                   eval      V4rgrDes = V4rgr + ' - '
066700140722     c                                      + §1TEdes
066800140722      *
066900140722     c                   add       1             S04nrr
067000140722     c                   write     TA12S04
067100140722      *
067200140722e   1c                   EndIf
067300140722      *
067400140722     c                   ENDSR
067500140722      *
067600140722      *---------------------------------------------------------------*
067700140722      *?Impostazione dati in singolo record del sfl S04              ?*
067800140722      *---------------------------------------------------------------*
067900140722     c     sr_OpzS04     BEGSR
068000140722      *
068100140722     c                   clear                   SavS04csr
068200140722      *
068300140722for 1c                   FOR       S04nrr = 1  To *hival
068400140722      *
068500140722     c     S04nrr        chain     TA12S04
068600140722      * Esco se fine sfl
068700140722     c                   if        Not  %found(TNTA12D)
068800140722     c                   leave
068900140722     c                   endif
069000140722      *
069100140722     c                   z-add     S04nrr        C04rcd
069200140722      *
069300140722sel 2c                   SELECT
069400140722      * -?Nessuna opzione?
069500140722w   2c                   WHEN      V4opz   = *blanks
069600140722      * -?X = Dettaglio Raggruppamento?
069700140722w   2c                   WHEN      V4opz   = 'X'
069800140722     c                   exsr      sr_OpzS4_X
069900140722e   2c                   ENDSL
070000140722      *
070100140722      * -?Memorizzazione nrr del sfl con la 1ª opzione?
070200140722      *  ?per riposizionarvici il cursore dopo il tasto "Invio"?
070300140722if  2c                   if             V4opz    <> *blanks
070400140722     c                             and (SavS04csr = *zeros
070500140722     c                              or  SavS04csr < C04rcd)
070600140722     c                   eval      SavS04csr   = C04rcd
070700140722e   2c                   endif
070800140722      *
070900140722      * -?Aggiornamento sfl?
071000140722sel 2c                   select
071100140722w   2c                   when      *in28
071200140722     c                   z-add     C04rcd        C04csr
071300140722w   2c                   when      *in90       = *on
071400140722     c                   z-add     C04rcd        C04csr
071500140722     c                   clear                   V4opz
071600140722x   2c                   other
071700140722     c                   clear                   V4opz
071800140722e   2c                   endsl
071900140722     c                   UPDATE    TA12S04
072000140722if  2c                   if        *in28  OR  *in90
072100140722     c                   leave
072200140722e   2c                   endif
072300140722      *
072400140722e   1c                   ENDFOR
072500140722      *
072600140722      * -?Riposiziono il cursore sul record contenente la prima opzione?
072700140722      *  ?(se non sono stati rilevati errori)?
072800140722if  1c                   if        NOT *in28  and  NOT *in90
072900140722     c                             and SavS04csr  > *zeros
073000140722     c                   z-add     SavS04csr     C04csr
073100140722e   1c                   endif
073200140722      *
073300140722     c                   ENDSR
073400140722      *
073500140722      *---------------------------------------------------------------*
073600140722      *?S4_Opz.X = Dettaglio singolo Raggruppamento Rubrica          ?*
073700140722      *---------------------------------------------------------------*
073800140722     c     sr_OpzS4_X    BEGSR
073900140722      *
074000140722     c                   eval      TA12rgr = V4rgr
074100140722     c                   exsr      sr_RGRnot
074200140722     c                   clear                   TA12rgr
074300140722      *
074400140722     c                   exsr      sr_PulSfl
074500140722     c                   exsr      sr_CarSfl
074600140722     c                   exsr      sr_EmiSfl
074700140722      *
074800140722     c                   ENDSR
074900051201
075000051201      *------------------------------------------------------------------------*
075100051201      * PULISCO IL SUBFILE
075200051201      *------------------------------------------------------------------------*
075300051201     c     Sr_Pulsfl     BegSr
075400051201
075500051206     c                   Clear                   nrr1
075600051202     c                   Clear                   v2cksc
075700051202     c                   Clear                   v2cpot
075800051202     c                   Clear                   v2cnrv
075900051201     c                   Eval      *In20 = *Off
076000051201     c                   Eval      *In21 = *Off
076100051202     c                   Write     ta12c02
076200051201     c                   Eval      *In21 = *On
076300051201     c                   Eval      *In20 = *On
076400051201
076500051206     c                   Eval      recsf1 = 1
076600051201     c                   Eval      *In31 = *Off
076700051201
076800051201     c                   EndSr
076900051201
077000051201      *------------------------------------------------------------------------*
077100051201      * CARICO IL SUBFILE
077200051201      *------------------------------------------------------------------------*
077300051201     c     Sr_Carsfl     BegSr
077400051202
077500051202      * Imposto i dati del control
077600051202     c                   Eval      v2cksc = v1cksc
077700051202     c                   Eval      v2cpot = v1cpot
077800051202     c                   Eval      v2cnrv = v1cnrv
077900051202     c                   Eval      v2dksc = v1dksc
078000051202     c                   Eval      v2dpot = v1dpot
078100051202     c                   Eval      v2dnrv = v1dnrv
078200051206
078300051206     c                   Eval      *In42 = *Off
078400051201
078500051202      * Imposto la chiave per tfntc
078600051202     c                   Eval      kapl = v1capl
078700051202     c                   Clear                   knk2
078800051202     c   03              Movel     dutkci        knk1
078900051202     c   03              Move      v2cksc        knk1
079000051202     c   04              Move      v2cpot        knk1
079100091112     c   11              Movel     dutkci        knk1
079200091112     c   11              Move      v2cnrv        knk1
079300051202
079400051202      * Carico il subfile (ciclo per i tipi contatti caricati in sk)
079500051202     c                   Eval      xx = 1
079600090406     c                   Eval      zz = 1
079700090406for 1c                   For       zz by 1 to 99
079800090415     c                   if        skord1(zz) = *blanks
079900090406     c                   iter
080000051202    2c                   EndIf
080100090406      * recupero la sk della nota
080200090415     c                   eval      xx = %lookup(%subst(skord1(zz):4:2):sktnt1)
080300051202     c                   Clear                   riga
080400051202      * Cerco i dati del tipo contatto in tabella 1T
080500051202     c                   Clear                   ds1t
080600051202     c                   Eval      kcod = '1T'
080700051202     c                   Eval      kkey = sktnt1(xx)
080800051202     c     kTabel        Chain     Tabel00f
080900051202if  2c                   If        Not %Found(Tabel00f) or tblflg <> *Blanks
081000051202     c                   Iter
081100051202    2c                   EndIf
081200051202     c                   Eval      ds1t = tbluni
081300140722     c*//                Eval      v2nota = %subst(tblkey:1:2) + ' - ' + §1tdes
081400140722     c                   eval      V2notaD = %subst(tblkey:1:2) + ' - '
081500140722     c                   eval      V2nota  = §1tdes
081600051202     c                   Clear                   v2email
081700051205     c                   Clear                   v2cann
081800051205     c                   Clear                   v2nrrr
081900051206     c                   Clear                   v2csns
082000090406     c                   clear                   v2prg
082100051205     c                   Eval      v2cmod = 'S'
082200051202     c                   Eval      *In07 = *On
082300051207     c                   Eval      *In10 = *Off
082400051206     c                   Add       1             nrr1
082500051202     c                   Write     ta12s02
082600051202     c                   Eval      ktnt = sktnt1(xx)
082700051202     c     kTfntc        Setll     Tfntc01l
082800051202      * Trovo il contatto lo scrivo nel subfile
082900051202if  2c                   If        %Equal
083000051202do  3c                   Do        *Hival
083100051205     c     kTfntc        Reade     Tfntc01l
083200051202     c                   If        %Eof(Tfntc01l)
083300051202     c                   Leave
083400051202     c                   EndIf
083500140722     c                   clear                   V2notaD
083600051202     c                   Eval      v2nota = ntcrnt
083700051202     c                   Eval      v2email = §1teml
083800051205     c                   Eval      v2ctnt = ntctnt
083900051205     c                   Eval      v2cann = ntcflt
084000051205     c                   Eval      v2nrrr = ntcnrr
084100051206     c                   Eval      v2csns = ntcsns
084200090406     c                   eval      v2prg  = §1tprg
084300051205     c                   Clear                   v2cmod
084400051202     c                   Eval      *In07 = *Off
084500051202      * Proteggo nota se richiesto da tabella 1T
084600051207      * o se sola interrogazione
084700140530     c*//                If        §1tpro = 'S' or *In09
084800140530     c                   If        (§1tpro  =  'S'   and
084900140530     c                             (Not *in08        or
085000140530     c                             (§1TnPro = *blank and *in08)))
085100140530     c                                               or  *In09
085200051202     c                   Eval      *In07 = *On
085300051207     c   09              Eval      *In10 = *On
085400051205     c                   Eval      v2cmod = 'S'
085500051202     c                   EndIf
085600051206     c                   Add       1             nrr1
085700051202     c                   Add       1             riga
085800051202     c                   Write     ta12s02
085900051202    3c                   EndDo
086000051202      * Carico n righe vuote in base a quante ne ho già scritte
086100051207      * se non sono protette
086200051207      * o se non è sola interrogazione
086300051207if  3c                   If        riga < §1trig and
086400140603     c                             (§1tpro  <> 'S'    or
086500140603     c                             (§1TnPro <> *blank or Not *in08))
086600140603     c                                                or  *In09
086700140530     c*//                           §1tpro <> 'S' and Not *In09
086800051202for 2c                   For       riga = riga + 1 by 1 to §1trig
086900140722     c                   clear                   V2notaD
087000051202     c                   Clear                   v2nota
087100051202     c                   Eval      v2email = §1teml
087200051205     c                   Eval      v2ctnt = sktnt1(xx)
087300051205     c                   Clear                   v2cann
087400051205     c                   Clear                   v2nrrr
087500051205     c                   Clear                   v2cmod
087600090406     c                   eval      v2prg  = §1tprg
087700051206     c                   Eval      v2csns = 'S'
087800051202     c                   Eval      *In07 = *Off
087900051206     c                   Add       1             nrr1
088000051202     c                   Write     ta12s02
088100051202    2c                   EndFor
088200051202    3c                   EndIf
088300051202      * Non trovo il contatto scrivo righe vuote nel subfile
088400051202   x2c                   Else
088500051202      * Scrivo righe vuote solo se non è richiesta la protezione da tabella 1T
088600051207      * o se non è solo interrogazione
088700140530if  3c*//                If        §1tpro = 'S' or *In09
088800140530     c                   If        (§1tpro  =  'S'   and
088900140530     c                             (Not *in08        or
089000140530     c                             (§1TnPro = *blank and *in08)))
089100140530     c                                               or  *In09
089200051202     c                   Iter
089300051202    3c                   EndIf
089400051202     c                   Eval      yy = 1
089500051202for 3c                   For       yy by 1 to §1trig
089600140722     c                   clear                   V2notaD
089700051202     c                   Clear                   v2nota
089800051202     c                   Eval      v2email = §1teml
089900051205     c                   Eval      v2ctnt = sktnt1(xx)
090000051205     c                   Clear                   v2cann
090100051205     c                   Clear                   v2nrrr
090200051205     c                   Clear                   v2cmod
090300090406     c                   eval      v2prg  = §1tprg
090400051206     c                   Eval      v2csns = 'S'
090500051202     c                   Eval      *In07 = *Off
090600051206     c                   Add       1             nrr1
090700051202     c                   Write     ta12s02
090800051202    3c                   EndFor
090900051202    2c                   EndIf
091000051202    1c                   EndFor
091100090406
091200051201     c                   Eval      *In31 = *On
091300051201
091400051202     c                   EndSr
091500051202
091600051202      *------------------------------------------------------------------------*
091700051202      * EMETTO IL SUBFILE
091800051202      *------------------------------------------------------------------------*
091900051202     c     Sr_Emisfl     BegSr
092000051201
092100051202do  1c                   Do        *Hival
092200051205
092300051206     c  n28              Z-add     1             recsf1
092400051206
092500051202     c                   Write     ta12t01
092600051202     c                   Write     ta12z02
092700051202     c                   Exfmt     ta12c02
092800051201
092900051201     c                   Eval      *In28 = *Off
093000051202     c                   Clear                   v2cmsg
093100051201
093200051202      * F3=Fine
093300051202     c   kc              Goto      fine
093400051202
093500051201      * F12=Ritorno
093600051207     c                   If        *InKl
093700140722     c                   if            %parms() > 1
093800140722     c                             and  TA12tnt = *blanks
093900140722     c                             and  TA12rgr = *blanks
094000140722     c                             and  TA12tot = *blanks
094100140722     c                   leavesr
094200140722     c                   endif
094300051207     c   01              Goto      fine
094400051207     c  n01              Goto      emid02
094500051207     c                   EndIf
094600051206
094700051206      * F10=Inserimento Multiplo E-Mail
094800051206     c                   If        *Inkj
094900051206     c                   ExSr      Sr_F10
095000140722     c                   if            %parms() > 1
095100140722     c                             and  TA12tnt = *blanks
095200140722     c                             and  TA12rgr = *blanks
095300140722     c                             and  TA12tot = *blanks
095400140722     c                             and  *in28
095500140722     c                   iter
095600140722     c                   endif
095700140722     c                   eval      $InzS04 = *on
095800051206     c                   Goto      carsfl
095900051206     c                   EndIf
096000051201
096100051201      * Controllo i dati immessi nel subfile
096200051202     c                   ExSr      Sr_Ctrsfl
096300051202     c   28              Iter
096400051202
096500051202      * F6=Conferma
096600051205     c                   If        *Inkf
096700051205     c                   ExSr      Sr_Conferma
096800140722     c                   eval      $InzS04 = *on
096900140722     c                   if            %parms() > 1
097000140722     c                             and  TA12tnt = *blanks
097100140722     c                             and  TA12rgr = *blanks
097200140722     c                             and  TA12tot = *blanks
097300140722     c                   leavesr
097400140722     c                   endif
097500051207     c                   If        *In01
097600051207     c                   Goto      fine
097700051207     c                   EndIf
097800051202     c                   Leave
097900051205     c                   EndIf
098000051205
098100051202    1c                   EndDo
098200051201
098300051201     c                   EndSr
098400051202
098500051202      *------------------------------------------------------------------------*
098600051202      * CONTROLLO I DATI DEL SUBFILE
098700051202      *------------------------------------------------------------------------*
098800051202     c     Sr_Ctrsfl     BegSr
098900051202
099000051206     c                   Clear                   nrr1
099100051202do  1c                   Do        *Hival
099200051206     c                   Add       1             nrr1
099300051206     c     nrr1          Chain     ta12s02                            32
099400051205      * Esco se fine sfl
099500051202     c                   If        *In32
099600051202     c                   Leave
099700051202     c                   EndIf
099800051206      * Non controllo se riga protetta
099900051206     c                   If        v2cmod = 'S'
100000051206     c                   Iter
100100051206     c                   EndIf
100200051202
100300051202     c                   setoff                                       42
100400051205
100500051205      * Se prima la nota c'era ora non posso cancellarla
100600051205if  2c                   If        v2nrrr <> *Zeros and v2nota = *Blanks
100700051205     c                   Eval      *In28 = *On
100800051205     c                   Eval      *In42 = *On
100900051205     c                   Eval      v2cmsg = msg(11)
101000051206     c                   Eval      recsf1 = nrr1
101100051205     c                   Update    ta12s02
101200051205     c                   Leave
101300051205    2c                   EndIf
101400051202
101500051202      * Controllo se indirizzo e-mail
101600051202if  2c                   If        v2email = 'S' and v2nota <> *Blanks
101700051202     c                   Clear                   dsemail
101800051202     c                   Eval      §emlindi = v2nota
101900051202     c                   Call      'XEMAIL'
102000051202     c                   Parm                    dsemail
102100051202      * Errore
102200051202if  3c                   If        §emlerro <> '0'
102300051202     c                   Eval      *In28 = *On
102400051202     c                   Eval      *In42 = *On
102500051202     c                   Eval      v2cmsg = §emlmsgo
102600051206     c                   Eval      recsf1 = nrr1
102700051205     c                   Update    ta12s02
102800051202     c                   Leave
102900051202   x3c                   else
103000140722     c                   clear                   V2notaD
103100051202     c                   Eval      v2nota = §emlindo
103200051202    3c                   EndIf
103300051202    2c                   EndIf
103400051202
103500051206     c                   Update    ta12s02
103600051202    1c                   EndDo
103700051202
103800051202     c                   EndSr
103900051202
104000051202      *------------------------------------------------------------------------*
104100051202      * CONFERMA
104200051202      *------------------------------------------------------------------------*
104300051202     c     Sr_Conferma   BegSr
104400051205
104500051206     c                   Clear                   nrr1
104600051205do  1c                   Do        *Hival
104700051206     c                   Add       1             nrr1
104800051206     c     nrr1          Chain     ta12s02                            32
104900051205      * Esco se fine sfl
105000051205     c                   If        *In32
105100051205     c                   Leave
105200051205     c                   EndIf
105300051205      * Torno a leggere se nota non modificabile
105400051205     c                   If        v2cmod = 'S'
105500051205     c                   Iter
105600051205     c                   EndIf
105700051205      * Torno a leggere se non immessa la nota
105800051205     c                   If        v2nota = *Blanks
105900051205     c                   Iter
106000051205     c                   EndIf
106100051205
106200051205      * C'è il nrr nel subfile contatto già esistente aggiorno/deleto
106300051205if  2c                   If        v2nrrr <> *Zeros
106400051205     c                   Eval      ntcnrr = v2nrrr
106500051205     c     ntcnrr        Chain     tfntc00f
106600051205if  3c                   If        %Found(tfntc00f)
106700051205     c                   Eval      ntcrnt = v2nota
106800051205     c                   Move      *date         ntcntr
106900051205     c                   Eval      ntcflt = v2cann
107000051206     c                   Eval      ntcsns = v2csns
107100051205      * Aggiorno
107200051205if  4c                   If        ntcflt = *Blanks
107300051205     c                   Update    tfntc0
107400051205      * Deleto
107500051205   x4c                   Else
107600051205     c                   Delete    tfntc0
107700051205    4c                   EndIf
107800051205    3c                   EndIf
107900051205
108000051205      * Non c'è il nrr sul subfile è rcd nuovo scrivo
108100051205   x2c                   Else
108200051205if  3c                   If        v2cann = *Blanks
108300090806     c                   Clear                   tfntc0
108400051205     c                   Eval      ntcapl = kapl
108500051205     c                   Eval      ntcnk1 = knk1
108600051205     c                   Eval      ntctnt = v2ctnt
108700051205     c                   Eval      ntcrnt = v2nota
108800051206     c                   Eval      ntcsns = v2csns
108900051205     c                   Move      *date         ntcntr
109000051205      * Scrivo il contatto
109100051205     c                   Write     tfntc0
109200051205    3c                   EndIf
109300051205    2c                   EndIf
109400051205
109500051205    1c                   EndDo
109600051202
109700051202     c                   EndSr
109800051206
109900051206      *------------------------------------------------------------------------*
110000051206      * TASTO FUNZIONE F10 - INSERIMENTO MULTIPLO MAIL
110100051206      *------------------------------------------------------------------------*
110200051206     c     Sr_F10        BegSr
110300051206
110400051206      * Pulisco il subfile
110500051206     c                   Clear                   nrr2
110600051206     c                   Clear                   v3cksc
110700051206     c                   Clear                   v3cpot
110800051206     c                   Clear                   v3cnrv
110900051206     c                   Eval      *In20 = *Off
111000051206     c                   Eval      *In21 = *Off
111100051206     c                   Write     ta12c03
111200051206     c                   Eval      *In21 = *On
111300051206     c                   Eval      *In20 = *On
111400051206
111500051206     c                   Eval      recsf2 = 1
111600051206     c                   Eval      *In31 = *Off
111700051206
111800051206      * Imposto i dati del control
111900051206     c                   Eval      v3cksc = v1cksc
112000051206     c                   Eval      v3cpot = v1cpot
112100051206     c                   Eval      v3cnrv = v1cnrv
112200051206     c                   Eval      v3dksc = v1dksc
112300051206     c                   Eval      v3dpot = v1dpot
112400051206     c                   Eval      v3dnrv = v1dnrv
112500051206     c                   Clear                   v3email
112600051206
112700051206      * Imposto la chiave per tfntc
112800051206     c                   Eval      kapl = v1capl
112900051206     c                   Clear                   knk2
113000051206     c   03              Movel     dutkci        knk1
113100051206     c   03              Move      v3cksc        knk1
113200051206     c   04              Move      v3cpot        knk1
113300091112     c   11              Movel     dutkci        knk1
113400091112     c   11              Move      v3cnrv        knk1
113500051206
113600051206      * Carico il subfile in base alla sk dei tipi nota che prevedono e-mail
113700060329      * e che sono gestibili dall'immissione multipla
113800051206     c                   Eval      xx = 1
113900090406     c                   Eval      zz = 1
114000090406for 1c                   For       zz by 1 to 99
114100090406     c                   if        skord(zz) = *blanks
114200090406     c                   iter
114300051206     c                   EndIf
114400090406      * recupero la sk della nota
114500090406     c                   eval      xx = %lookup(%subst(skord(zz):4:2):sktnt)
114600051206      * Solo le note e-mail
114700051206     c                   If        skceml(xx) = *Blanks
114800051206     c                   Iter
114900051206     c                   EndIf
115000060329      * Solo le gestibili
115100060329     c                   If        skmult(xx) <> *Blanks
115200060329     c                   Iter
115300060329     c                   EndIf
115400051206      * Se non esistono scrivo subfile
115500051206     c                   Eval      ktnt = sktnt(xx)
115600051206     c     kTfntc        Chain     Tfntc01l
115700051206     c                   If        %Found(Tfntc01l) and ntcflt <> 'A'
115800051206     c                   Iter
115900051206     c                   EndIf
116000051206     c                   Eval      v3nota = sktnt(xx) + ' - ' + skdtnt(xx)
116100051206     c                   Clear                   v3scelta
116200051206     c                   Eval      v3ctnt = sktnt(xx)
116300051206     c                   Add       1             nrr2
116400051206     c                   Write     ta12s03
116500051206    1c                   EndFor
116600051206     c                   Eval      *In31 = *On
116700051206
116800051206      * Se non ho caricato nessuna e-mail non emetto la videata
116900051206     c                   If        nrr2 = *Zeros
117000060329     c                   Eval      v2cmsg = 'già inserite tutte le e-mail gesti-
117100060329     c                             bili'
117200051206     c                   Eval      *In28 = *On
117300140722     c                   if            %parms() > 1
117400140722     c                             and  TA12tnt = *blanks
117500140722     c                             and  TA12rgr = *blanks
117600140722     c                             and  TA12tot = *blanks
117700140722     c                   leavesr
117800140722     c                   else
117900051206     c                   Goto      carsfl
118000140722     c                   endif
118100051206     c                   EndIf
118200051206
118300051206      * Emetto il subfile
118400051206     c     emisfl2       Tag
118500051206do  1c                   Do        *Hival
118600051206
118700051206     c  n28              Z-add     1             recsf2
118800051206
118900051206     c                   Write     ta12t01
119000051206     c                   Write     ta12z03
119100051206     c                   Exfmt     ta12c03
119200051206
119300051206     c                   Eval      *In28 = *Off
119400051206     c                   Clear                   v3cmsg
119500051206
119600051206      * F3=Fine
119700051206     c   kc              Goto      fine
119800051206
119900051206      * F12=Ritorno
120000051206     c   kl              Goto      carsfl
120100051206
120200051206      * Controllo che sia inserita l'e-mail da creare
120300051206     c                   If        v3email = *Blanks
120400051206     c                   Eval      *In28 = *On
120500051206     c                   Eval      v3cmsg = msg(12)
120600051206     c                   Goto      emisfl2
120700051206     c                   EndIf
120800051206
120900051206      * Controllo che l'e-mail inserita sia valida
121000051206if  2c                   If        v3email <> *Blanks
121100051206     c                   Clear                   dsemail
121200051206     c                   Eval      §emlindi = v3email
121300051206     c                   Call      'XEMAIL'
121400051206     c                   Parm                    dsemail
121500051206if  3c                   If        §emlerro <> '0'
121600051206     c                   Eval      *In28 = *On
121700051206     c                   Eval      v3cmsg = §emlmsgo
121800051206     c                   Goto      emisfl2
121900051206   x3c                   Else
122000051206     c                   Eval      v3email = §emlindo
122100051206    3c                   EndIf
122200051206     c  nkf              Goto      emisfl2
122300051206    2c                   EndIf
122400051206      * F6=Conferma
122500051206if  2c                   If        *Inkf
122600051206     c                   Clear                   nrr2
122700051206do  3c                   Do        *Hival
122800051206     c                   Add       1             nrr2
122900051206     c     nrr2          Chain     ta12s03                            32
123000051206      * Esco se fine sfl
123100051206     c                   If        *In32
123200051206     c                   Leave
123300051206     c                   EndIf
123400051206      * Torno a leggere se non è stata scelta
123500051206     c                   If        v3scelta = *Blanks
123600051206     c                   Iter
123700051206     c                   EndIf
123800051206      * Scrivo le note
123900051206     c                   Clear                   tfntc
124000051206     c                   Eval      ntcapl = kapl
124100051206     c                   Eval      ntcnk1 = knk1
124200051206     c                   Eval      ntctnt = v3ctnt
124300051206     c                   Eval      ntcrnt = v3email
124400051206     c                   Eval      ntcsns = 'S'
124500051206     c                   Move      *date         ntcntr
124600051206      * Scrivo il contatto
124700051206     c                   Write     tfntc0
124800051206    3c                   EndDo
124900051206     c                   Leave
125000051206    2c                   EndIf
125100051206
125200051206    1c                   EndDo
125300051206
125400051206     c                   EndSr
125500051202
125600051201      *------------------------------------------------------------------------*
125700051201      * ROUTINE INIZIALE
125800051201      *------------------------------------------------------------------------*
125900051201     c     *Inzsr        BegSr
126000051201
126100051201     c     *Entry        Plist
126200051201     c                   Parm                    kpjba
126300051201     c                   Parm                    tnta12ds
126400051206
126500051206      * Controllo se richiamato e come
126600051206if  1c                   If        %Parms > 1
126700101019
126800051206if  2c                   If        ta12apl <> *Blanks
126900051201     c                   Eval      *In01 = *On
127000051206   x2c                   Else
127100051206     c                   Eval      *In01 = *Off
127200051206    2c                   EndIf
127300051207      * Chiamato per sola interrogazione
127400051207     c                   If        ta12ric = 'V'
127500051207     c                   Eval      *In09 = *On
127600051207     c                   Else
127700051207     c                   Eval      *In09 = *Off
127800051207     c                   EndIf
127900140530      * Chiamato dal *pgm Gestione del Credito
128000140530     c                   eval      *in08 = (TA12ric = 'C')
128100101019
128200101019      * da menù --> non richiamato
128300051206   x1c                   Else
128400051201     c                   Eval      *In01 = *Off
128500140530     c                   eval      *in08 = *off
128600051206    1c                   EndIf
128700051206
128800051201     c     *dtaara       define    §azute        azuteds
128900051201     c     *dtaara       define    §datiute      ddatiute
129000051201     c                   in(E)     *dtaara
129100051201     c                   If        %error  or rsut = *blanks
129200051201     c                   Clear                   tibs34ds
129300051201     c                   Call      'TIBS34R'
129400051201     c                   Parm                    tibs34ds
129500051201     c                   In        *dtaara
129600051201     c                   EndIf
129700051201
129800051201      * Controllo se sono in sede
129900051201     c                   Eval      *In12 = (simfel = *zeros)
130000051201
130100051201      * Carico tutti i contatti
130200051201     c                   Eval      kcod = '1T'
130300051201     c                   Clear                   xx
130400051201     c     kTabel01      Setll     Tabel00f
130500051201     c                   Do        *Hival
130600051201     c     kTabel01      Reade     Tabel00f
130700051201     c                   If        %Eof(Tabel00f)
130800051201     c                   Leave
130900051201     c                   EndIf
131000051201     c                   If        tblflg <> *Blanks
131100051201     c                   Iter
131200051201     c                   EndIf
131300051201     c                   Eval      ds1t = tbluni
131400051201     c                   If        §1trich <> 'C'
131500051201     c                   Iter
131600051201     c                   EndIf
131700051201     c                   Add       1             xx
131800051206     c                   Eval      ds1t = tbluni
131900051201     c                   Eval      sktnt(xx) = tblkey
132000051206     c                   Eval      skdtnt(xx) = §1tdes
132100051206     c                   Eval      skceml(xx) = §1teml
132200060329     c                   Eval      skmult(xx) = §1tmult
132300140722     c                   eval      sk_Prg(xx) = §1Tprg
132400140722     c                   eval      sk_Rgr(xx) = §1Trgr
132500090406     c                   eval      %subst(skord(xx):1:3) = %editc(§1tprg:'X')
132600090406     c                   eval      %subst(skord(xx):4:2) = sktnt(xx)
132700051201     c                   EndDo
132800140722      *//* ordino la schiera
132900140722     c*//                sorta     skord
133000140722      *
133100140722if  1c                   IF            %parms() > 1
133200140722     c                             and  TA12tnt = *blanks
133300140722     c                             and  TA12rgr = *blanks
133400140722     c                             and  TA12tot = *blanks
133500140722     c                   clear                   xx
133600140722     c                   movel     *hival        sk_Tot
133700140722     c     '1TE'         setll     TNTBE000
133800140722     c     '1TE'         reade     TNTBE000
133900140722do  2c                   DoW       Not %eof(TNTBE01L)
134000140722if  3c                   if        TBEatb = *blanks
134100140722     c                   eval      d1TE = TBEuni
134200140722     c                   eval      xx += 1
134300140722     c                   eval      sk_Tot(xx) = %editc( §1TEord : 'X' )
134400140722     c                                        + TBEke1
134500140722e   3c                   endif
134600140722     c     '1TE'         reade     TNTBE000
134700140722e   2c                   EndDo
134800140722     c                   sorta     sk_Tot
134900140722     c                   eval      xx += 1
135000140722     c                   movea     *blanks       sk_Tot(xx)
135100140722e   1c                   ENDIF
135200051201
135300051201      * KLIST
135400051201     c     kTabel        klist
135500051201     c                   kfld                    kkut
135600051201     c                   kfld                    kcod
135700051201     c                   kfld                    kkey
135800051201
135900051201     c     kTabel01      klist
136000051201     c                   kfld                    kkut
136100051201     c                   kfld                    kcod
136200140722
136300140722     c     k03tntbe01    klist
136400140722     c                   kfld                    TBEcod
136500140722     c                   kfld                    TBEke1
136600140722     c                   kfld                    TBEke2
136700051202
136800051202     c     kTfntc        klist
136900051202     c                   kfld                    kapl
137000051202     c                   kfld                    knk1
137100051202     c                   kfld                    knk2
137200051202     c                   kfld                    ktnt
137300051201
137400051201     c                   EndSr
137500090701      *------------------------------------------------------------------------*
137600090701      * CONTROLLO SE UTENTE ABILITATO
137700090701      *------------------------------------------------------------------------*
137800090701     c     Sr_Ctraut     BegSr
137900090701     c                   clear                   tntaa1ds
138000090701     c   03              eval      itaa1caut='§utecli'
138100090701     c   04              eval      itaa1caut='§utepot'
138200091112     c   11              eval      itaa1caut='§utegtc'
138300090701     c                   clear                   kpjbu
138400090701     c                   movel     tntaa1ds      kpjbu
138500090703     c                   call      'TNTAA1C'
138600090701     c                   parm                    kpjba
138700090701     c                   movel     kpjbu         tntaa1ds
138800090701     c
138900090701    1c                   if        otaa1fabi='N'
139000090701     c                   Eval      *In02 = *On
139100090701     c                   Eval      *In28 = *On
139200090701     c                   Eval      v1cmsg = msg(03)
139300090701     c   03              eval      v1cmsg=%trim(v1cmsg)+' I CLIENTI   '
139400090701     c   04              eval      v1cmsg=%trim(v1cmsg)+' I POTENZIALI'
139500091112     c   11              eval      v1cmsg=%trim(v1cmsg)+' LE TRATTATIVE'
139600090701     c                   endif
139700090701     c
139800090701     c                   EndSr
139900090701
140000051201      *------------------------------------------------------------------------*
140100051201
140200051201** MSG  Lungh. 78                                                            *
140300051201Tipo applicazione errato                                                      01
140400051201Tipo applicazione non utlizzabile da utenti di SEDE                           02
140500090701UTENTE NON ABILITATO ALLA GESTIONE CONTATTI PER                               03
140600051202Cliente errato                                                                04
140700090630Cliente non in gestione all'utente                                            05
140800051202Potenziale errato                                                             06
140900090630Potenziale non in gestione all'utente                                         07
141000101019                                                                              08
141100101019                                                                              09
141200051202Contatto errato                                                               10
141300051205Manca riga contatto                                                           11
141400051206Manca e-mail                                                                  12
