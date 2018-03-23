000100130723       //==============================================================
000200150521       //?Selezione cliente da FIBSP00F
000300150521       //?            (Abilitazione allo spostamento in sospensione se ?
000400150521       //?             non spuntati i colli)                           ?
000500130723       //==============================================================
000600130723
000700130723       //--------------------------------------------------------------
000800130723       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900130723       //--------------------------------------------------------------
001000130723
001100130723     /*PRM  dbgview(*source)
001200130723     /*END
001300130723
001400130723       //--------------------------------------------------------------
001500130723       //?Specifiche di controllo.                                     ?
001600130723       //--------------------------------------------------------------
001700130723
001800130723     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900130723
002000130723       //--------------------------------------------------------------
002100130723       //?Dichiarazione file.                                          ?
002200130723       //--------------------------------------------------------------
002300130723
002400130723
002500151216     fazorg01l  if   e           k disk
002600150518       // -?File clienti/filiali abilitati
002700150521     fFIBSP01L  if   e           k disk
002800150521     fFIBSP02L  if   e           k disk    rename(fibsp000:fibsp02)
002900150505
003000130723
003100130723       // -?Video?
003200150525     ffnlsBSP1d cf   e             workstn sfile(tbbsps01:s01nrr)
003300130723     f                                     indds(IndDspF)
003400130723     f                                     infds(InfDspF)
003500130723
003600130723       //--------------------------------------------------------------
003700130723       //?Definizione costanti.                                        ?
003800130723       //--------------------------------------------------------------
003900130723
004000130723       // -?Costante per controllo "caratteri solo numerici"?
004100130723     d c_Digits        c                   const('0123456789')
004200131212
004300131212       // -?Costanti per la definizione delle schiere con i nomi?
004400130723
004500130723       // -?Tasti funzionali a video?
004600130723     d c_F01           c                   const(x'31')
004700130723     d c_F02           c                   const(x'32')
004800130723     d c_F03           c                   const(x'33')
004900130723     d c_F04           c                   const(x'34')
005000130723     d c_F05           c                   const(x'35')
005100130723     d c_F06           c                   const(x'36')
005200130723     d c_F07           c                   const(x'37')
005300130723     d c_F08           c                   const(x'38')
005400130723     d c_F09           c                   const(x'39')
005500130723     d c_F10           c                   const(x'3A')
005600130723     d c_F11           c                   const(x'3B')
005700130723     d c_F12           c                   const(x'3C')
005800130723     d c_F13           c                   const(x'B1')
005900130723     d c_F14           c                   const(x'B2')
006000130723     d c_F15           c                   const(x'B3')
006100130723     d c_F16           c                   const(x'B4')
006200130723     d c_F17           c                   const(x'B5')
006300130723     d c_F18           c                   const(x'B6')
006400130723     d c_F19           c                   const(x'B7')
006500130723     d c_F20           c                   const(x'B8')
006600130723     d c_F21           c                   const(x'B9')
006700130723     d c_F22           c                   const(x'BA')
006800130723     d c_F23           c                   const(x'BB')
006900130723     d c_F24           c                   const(x'BC')
007000130723     d c_Enter         c                   const(x'F1')
007100130723     d c_RollDown      c                   const(x'F4')
007200130723     d c_RollUp        c                   const(x'F5')
007300130723
007400130723       //--------------------------------------------------------------
007500130723       //?Definizione schiere.                                         ?
007600130723       //--------------------------------------------------------------
007700130723
007800130723       // -?Messaggi di errore?
007900130723
008000130723       //--------------------------------------------------------------
008100130723       //?Definizione aree dati.                                       ?
008200130723       //--------------------------------------------------------------
008300130723
008400130723       // -?Dati utente?
008500130723     d §AzUte        e ds                  extname(AZUTE00F)
008600130723     d                                     dtaara
008700130723     d §DatiUte      e ds                  extname(dDatiUte)
008800130723     d                                     dtaara
008900130723
009000130723       //--------------------------------------------------------------
009100130723       //?Definizione strutture dati.                                  ?
009200130723       //--------------------------------------------------------------
009300130723
009400130723       // -?Status ds?
009500130723     d Status         sds
009600130723     d   SDSpgm          *proc
009700130723
009800130723       // -?InfDS?
009900130723     d InfDspF         ds
010000130723     d   dsp_aid             369    369a
010100130723
010200130723       // -?Indicatori su DspF?
010300130723     d IndDspF         ds                  inz
010400130723         // -?Abilitazione tasti funzionali?
010500150526     d   F8Attivo                      n   overlay(IndDspF : 08)
010600150520         // -?Indicatori di gestione del subfile?
010700150520     d   SflDsp_N                      n   overlay(IndDspF : 23)
010800150520     d   SflDspCtl_N                   n   overlay(IndDspF : 20)
010900150520     d   SflNxtChg                     n   overlay(IndDspF : 22)
011000150520     d   SflEnd                        n   overlay(IndDspF : 21)
011100130723         // -?Emissione messaggio di errore?
011200130723         // -?Indicatori per Attibuti di visualizzazione?
011300150525     d   DisabilitOpz                  n   overlay(IndDspF : 40)
011400150526     d   Ord_x_Des                     n   overlay(IndDspF : 41)
011500150526     d*campolps01      ds            42
011600150526     d* S1cLP1
011700150526     d* S1cLP2
011800150526     d* S1cLP3
011900150526     d* S1cLP4
012000150526     d* S1cLP5
012100150526     d* S1cLP6
012200150526     d* S1cLP7
012300150526     d* S1cLP8
012400150526     d* S1cLP9
012500150526     d* S1cLp10
012600150526     d* S1cLp11
012700150526     d* S1cLp12
012800150526     d* S1cLp13
012900150526     d* S1cLp14
013000130723
013100130723       // -?Parametri ricevuti?
013200130723     d KPJBA         e ds
013300150525     d fnlsbsp1ds    e ds
013400151216       // - Caricamento Filiali in £1
013500151216     d TRUL06DS      e ds
013600151216     d  L1                     1     90  0 dim(30)
013700151215     d                 ds
013800151215     d bsplin
013900151215     d sbsp                           3  0 dim(30) overlay(bsplin)
014000150520
014100130723
014200130723       //--------------------------------------------------------------
014300130723       //?Definizione variabili globali.                               ?
014400130723       //--------------------------------------------------------------
014500130723
014600130723
014700150526       // -?Flags booleani?
014800150525     d $Fine           s               n   inz
014900130723
015000130723       // -?Campi di comodo?
015100131212     d wDate           s              8  0 inz
015200150520     d s01nrr          s              4  0
015300151216     d welabora        s              1
015400151216     d wtrov           s              1
015500151216     d i               s              3  0
015600130723
015700150526       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
015800150526
015900150526       //--------------------------------------------------------------
016000150526       // -?Constants?
016100150526       //--------------------------------------------------------------
016200150526       // -?MaxKey - Maximum number of key fields allowed by this program?
016300150526     d c_MaxKey        c                   const(3)
016400150526       // -?Sort order:?
016500150526       //  ?1 = Sort in an ascending sequence?
016600150526       //  ?2 = Sort in a descending sequence?
016700150526     d c_Ascendente    c                   const(1)
016800150526     d c_Discendente   c                   const(2)
016900150526       // -?Key data type:?
017000150526       //  ? 0 = Signed binary?
017100150526       //  ? 2 = Signed zoned decimal?
017200150526       //  ? 3 = Signed packed decimal?
017300150526       //  ? 6 = Character with no national language sort sequence applied,?
017400150526       //  ?     if specified?
017500150526       //  ? 7 = Unsigned packed decimal?
017600150526       //  ?     All numbers will have the sign forced positive ('F'X).?
017700150526       //  ? 8 = Unsigned zoned decimal?
017800150526       //  ?     All numbers will have the sign forced positive ('F'X).?
017900150526       //  ? 9 = Unsigned binary?
018000150526       //  ?14 = Date in form of DD/MM/YY?
018100150526       //  ?15 = Date in form of DD.MM.YYYY?
018200150526     d c_Numero        c                   const(2)
018300150526     d c_Carattere     c                   const(6)
018400150526     d c_NumIntero     c                   const(8)
018500150526       //
018600150526     d c_Put           c                   const(1)
018700150526     d c_EndPut        c                   const(2)
018800150526     d c_Get           c                   const(3)
018900150526
019000150526       //--------------------------------------------------------------
019100150526       // -?Data Structures?
019200150526       //  ?SflRcd     - The entire subfile record to pass to the sort?
019300150526       //  ?QLGSCB     - The sort request block for the QLGSORT API?
019400150526       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
019500150526       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
019600150526       //  ?QUSEC      - Error structure for the QLGSORT API?
019700150526       //--------------------------------------------------------------
019800150526     d SflRcd          ds                  inz
019900150526     d   S1opzione                         inz
020000150526     d   s1htps                            inz
020100150526     d   S1Ccod                            inz
020200150526     d   S1cdes                            inz
020300150526     d   S1Hlnp                            inz
020400150526     d   S1Hksc                            inz
020500150526     d   S1Cfpa                            inz
020600150526     d   S1Caut                            inz
020700150526     d   s1cdlin                           inz
020800150526     d   campolps01                  42    inz
020900150526     d   s1clp1                            inz overlay(campolps01:1)
021000150526     d   s1clp2                            inz overlay(campolps01:4)
021100150526     d   s1clp3                            inz overlay(campolps01:7)
021200150526     d   s1clp4                            inz overlay(campolps01:10)
021300150526     d   s1clp5                            inz overlay(campolps01:13)
021400150526     d   s1clp6                            inz overlay(campolps01:16)
021500150526     d   s1clp7                            inz overlay(campolps01:19)
021600150526     d   s1clp8                            inz overlay(campolps01:22)
021700150526     d   s1clp9                            inz overlay(campolps01:25)
021800150526     d   s1clp10                           inz overlay(campolps01:28)
021900150526     d   s1clp11                           inz overlay(campolps01:31)
022000150526     d   s1clp12                           inz overlay(campolps01:34)
022100150526     d   s1clp13                           inz overlay(campolps01:37)
022200150526     d   s1clp14                           inz overlay(campolps01:40)
022300150526     d   s1cpiu                            inz
022400150526     d   s1hdes                            inz
022500150526     d   Selected                     1a   inz
022600150526      /copy QSYSINC/QRPGLESRC,QLGSORT
022700150526     d QLGKL                         16    dim(c_MaxKey)
022800150526     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
022900150526     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
023000150526     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
023100150526     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
023200150526      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023300150526      /copy QSYSINC/QRPGLESRC,QUSEC
023400150526
023500150526       //--------------------------------------------------------------
023600150526       // -?Standalone fields?
023700150526       //  ?Nrr        - Relative record number for subfile?
023800150526       //  ?SizeList   - Size of list?
023900150526       //  ?ReturnSize - Size of list returned by sort API?
024000150526       //--------------------------------------------------------------
024100150526     d NotUsed         s             16a   inz
024200170208     d ReturnSize      s             10i 0 inz
024300170208     d SizeList        s             10i 0 inz
024400150526     d RrnLast         s              5i 0 inz
024500150526     d Nrr             s              5i 0 inz
024600130723       //--------------------------------------------------------------
024700130723       //?Definizione prototipi procedure.                             ?
024800130723       //--------------------------------------------------------------
024900130723
025000130723       // -?Reperimento dati utente?
025100130723     d TIBS34ds      e ds
025200130723      /copy gaitrasrc/srcProtoPR,TIBS34R
025300130723
025400150526       // -?Ordinamento subfile?
025500150526      /copy gaitrasrc/srcProtoPr,QLGSRTIO
025600151216       // - Caricamento Filiali in £1
025700151216     d TRUL06R         pr                  extpgm('TRUL06R')
025800151216     d  kpjba                              likeds(kpjba)
025900130724
026000130723
026100130723       //--------------------------------------------------------------
026200130723       //?Definizione key-list.                                        ?
026300130723       //--------------------------------------------------------------
026400130723
026500130723
026600130723       //--------------------------------------------------------------
026700130723       //?Riepilogo indicatori utilizzati.                             ?
026800130723       //--------------------------------------------------------------
026900130723       //--------------------------------------------------------------
027000130723
027100130723       //--------------------------------------------------------------
027200130723       //?M A I N - L I N E                                            ?
027300130723       //--------------------------------------------------------------
027400130723
027500130723     c     *Entry        plist
027600130723     c                   parm                    KPJBA
027700150525     c                   movel     kpjbu         fnlsbsp1ds
027800150525     c* pulizia campi di output
027900150525     c                   clear                   OBSPTPS
028000150525     c                   clear                   OBSPCLI
028100150525     c                   clear                   OBSPLIN
028200150525     c                   clear                   OBSPDES
028300150525     c                   clear                   OBSPSOD
028400150525     c                   clear                   OBSPFXX
028500150525     c                   clear                   OBSPERR
028600150525     c                   clear                   OBSPMSG
028700130723      /free
028800130723
028900130723       // -?Operazioni iniziali?
029000130723       exsr  sr_RoutInz;
029100150521
029200150521       exsr sr_gesc01;
029300130723
029400130723       // -?Operazioni finali?
029500130723       exsr  sr_RoutEnd;
029600130723
029700130723       //--------------------------------------------------------------
029800130723       //?Operazioni iniziali.                                         ?
029900130723       //--------------------------------------------------------------
030000130723       BEGSR  sr_RoutInz;
030100130723
030200130723         *inLR = *on;
030300130723
030400130723         // -?Reperimento dati job?
030500130723         exsr  sr_DatiJob;
030600130723
030700130723         // -?Impostazione nome programma a video?
030800130723         V1Tpgm = SDSpgm;
030900131212
031000131212         // -?Reperimento data odierna?
031100131212         wDate = %int( %subst( %char( %dec( %timestamp() ) )
031200131212                               : 1 : 8 ) );
031300160111         // -?Caricamento L1 se ricevuto il TFP
031400160111         if ibsptfp>0;
031500160111                clear trul06ds ;
031600160111                d06cod='£1';
031700160111                d06key=%editc(ibsptfp:'X');
031800160111                d06tla='L' ;
031900160111                kpjbu=trul06ds;
032000160111                trul06r(kpjba) ;
032100160111                TRUL06DS = kpjbu;
032200160111         endif;
032300130723
032400130723       ENDSR;
032500130723
032600130723       //--------------------------------------------------------------
032700130723       //?Reperimento Dati del job (Utente/Operativi).                 ?
032800130723       //--------------------------------------------------------------
032900130723       BEGSR  sr_DatiJob;
033000130723
033100130723         in(E) §AzUte;
033200130723         if NOT %error;
033300130723           in(E) §DatiUte;
033400130723         endif;
033500130723         if %error or RSut = *blanks;
033600130723           clear TIBS34ds;
033700130723           tibs34r ( tibs34ds );
033800130723           in §AzUte;
033900130723           in §DatiUte;
034000130723         endif;
034100130723
034200130723       ENDSR;
034300130723
034400150519       //--------------------------------------------------------------
034500150520       //?Gestione SFL di scelta record da FIBSP per lnp o ksc         ?
034600150519       //--------------------------------------------------------------
034700150519       BEGSR  sr_gesc01 ;
034800150520         // -?Inizializzazione Sfl    ?
034900150520         s01nrr = 0;
035000150526         clear  C1RcdNbr;
035100150526         clear  C1CsrRrn;
035200150520         sfldspCtl_N = *on;
035300150520         write tbbspc01;
035400150520         sfldsp_N = *off;
035500150520         sfldspCtl_N = *off;
035600150520         sflend = *off;
035700150520         sflnxtchg = *off;
035800150520         clear  C1RcdNbr;
035900150525         select;
036000150525         when ibsptps='P';
036100150525            s1htps='P' ;
036200150525         when ibsptps='C';
036300150525            s1htps='C' ;
036400150525         when ibsptps=' ';
036500150525            s1htps='P' ;
036600150525         endsl;
036700150525         // -?Caricamento Sfl         ?
036800150525         exsr carsfl;
036900150525       // SE richiesto entrambi i tipi (linea di partenza e cliente)
037000150525       // se ho elaborato i "P" ripeto ciclo per "C"
037100150525         if ibsptps=' ' and s1htps='P';
037200150525            s1htps='C';
037300150525            exsr carsfl;
037400150525         endif;
037500150525
037600150520       // se scritto almeno un record
037700150520        if s01nrr > 0;
037800150520       // indicatore di sflend
037900150520         eval sflend = *on;
038000150520         C1RcdNbr  = 1;
038100150520        endif;
038200150520
038300150520       // se non scritto neanche un record
038400150520        if s01nrr = 0;
038500150520       // indicatore di sfldsp
038600150520         eval sfldsp_N = *on;
038700150520         clear C1RcdNbr;
038800150520        endif;
038900150520         // -?Emissione Videata       ?
039000150525         dow not $fine=*on;
039100150526            write tbbspp03;
039200150520            exfmt tbbspc01;
039300150520       // controllo tasti funzionali del subfile
039400150520            select;
039500150520
039600150526           // -?F3=Fine?
039700150520            when dsp_aid=c_F03;
039800150525               obspfxx='03';
039900150520               $fine=*on;
040000150526           // -?F12=Ritorno?
040100150520            when dsp_aid=c_F12;
040200150525               $fine=*on;
040300150525               obspfxx='12';
040400150526           // -?F8=Cambio ordinamento?
040500150526           WHEN  dsp_aid = c_F08;
040600150526             exsr  sr_F08S01;
040700150520            other;
040800150525             if ibspmod=*blanks;
040900150520               readc tbbsps01;
041000150525               dow not %eof and not $fine = *on;
041100150520              // se è stata immessa un'opzione
041200150526                  C1RcdNbr = S01nrr;
041300150520                  if s1opzione <> *zeros;
041400150520
041500150525                     select;
041600150525                     when s1htps='P'  ;
041700150525                        obsplin=s1hlnp;
041800150525                     when s1htps='C'  ;
041900150525                        obspcli=s1hksc;
042000150525                     endsl;
042100150525                     obspdes=s1hdes;
042200150525                     obsptps=s1htps;
042300150525                     $fine=*on;
042400150520
042500150520                  endif;
042600150520
042700150520                  readc tbbsps01;
042800150520               enddo;
042900150525             endif;
043000150520
043100150520
043200150520            endsl;
043300150525           enddo;
043400150519       ENDSR;
043500150525       //--------------------------------------------------------------
043600150525       //?Caricamento sfl                                              ?
043700150525       //--------------------------------------------------------------
043800150525       BEGSR  carsfl    ;
043900150525         If  s1htps='P'                     ;
044000150525            setll (*loval) fibsp01l;
044100150525            read fibsp01l;
044200150525            s1htps='P' ;
044300150525         else;
044400150525            setll (*loval) fibsp02l;
044500150525            read fibsp02l;
044600150525            s1htps='C' ;
044700150525         endif;
044800150525         DisabilitOpz = (iBSPmod = 'V');
044900150526         Ord_x_Des = *off;
045000151216         C1Ctfp = iBSPtfp;
045100151216         if  iBSPtfp = *zero;
045200151216           C1Dtfp = 'TUTTI';
045300151216         else;
045400151216           chain  (C1Ctfp)  AZORG;
045500151216           if  %found(AZORG01L);
045600151216             C1Dtfp = ORGdes;
045700151216           else;
045800151216             C1Dtfp = *all'? ';
045900151216           endif;
046000151216         endif;
046100150525         // -?Caricamento      Sfl    ?
046200150525         dow not %eof;
046300150525
046400151209            if ibspfpa= ' ' or
046500151209               (ibspfpa='E' and bspfpa=' ') or
046600160111               (ibspfpa='S' and bspfpa='S') or
046700160111               (ibspfpa='C' and ((bspfpa='S' and bspaut='N') or
046800160111                bspfpa=' '));
046900151216         // - Se richiesta parzializzazione per tfp verifico
047000151216         //   se record da caricare
047100151216             clear welabora;
047200151216             if ibsptfp>0;
047300151216                exsr sr_tfp;
047400151216             endif;
047500151216          if ibsptfp=0 or welabora=*blanks;
047600150525            clear s1opzione;
047700150525            clear s1hksc;
047800150525            clear s1hlnp;
047900150525            clear s1cfpa;
048000150526            campolps01 = *zeros;
048100150525            clear s1cdlin;
048200150525            clear s1cpiu ;
048300150525            clear s1caut ;
048400150526            s1cdes= bspdes;
048500150525
048600150525            If  S1HTPS ='P'                  ;
048700150526               s1ccod= %editc(bspfil:'X');
048800150525               s1hlnp=bspfil;
048900150525            else ;
049000150526               s1ccod=%editc(bspksc:'X');
049100150525               s1hksc=bspksc;
049200150525            endif;
049300150525         // Flag "Part. da + fil"  e elenco filiali abilitate
049400150525            if bspfpa='S';
049500150525               s1cfpa='Si';
049600150525               s1cdlin='FilAbi:' ;
049700150525               campolps01=bsplin;
049800150525            else;
049900150525               if s1hksc>0;
050000150525         // Se record "cliente" --> elenco ter-part. abilitati
050100150525                  s1cdlin='TerPar:';
050200150525                  campolps01=bsplin;
050300150525               endif;
050400150525            endif;
050500150525            if campolps01>*zeros   and %subst(bsplin:43:3)>*zeros;
050600150525               s1cpiu='+';
050700150525            endif;
050800150525            if bspaut='N';
050900150525               s1caut='No';
051000150525            endif;
051100150525            s1hdes=bspdes;
051200150525            s01nrr+= 1;
051300150525            write tbbsps01;
051400151215          endif;
051500151209            endif;
051600150525            If  S1HTPS ='P'                  ;
051700150525               read fibsp01l;
051800150525            else;
051900150525               read fibsp02l;
052000150525            endif;
052100150525         enddo;
052200150526         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
052300150526         RrnLast  = S01nrr;
052400150526
052500150526         // -?Impaginazione subfile?
052600150526         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
052700150526         if S01nrr > *zero;
052800150526           C1RcdNbr  = 1;
052900150526           C1CsrRrn  = 1;
053000150526         else;
053100150526           clear C1RcdNbr;
053200150526         endif;
053300150526
053400150526         // -?Ordinamento subfile (se premuto F8)?
053500150526         if  Ord_x_Des  and  S01nrr > 1;
053600150526           exsr  sr_SortSfl;
053700150526         endif;
053800150526
053900150526         // -?(Dis)Abilitazione tasti funzionali?
054000150526         F8Attivo  = (S01nrr > *zero); // -?F8 = Ordinamento x ...?
054100150525       ENDSR;
054200150526       //--------------------------------------------------------------
054300150526       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
054400150526       //?F08-Cambia ordinamento sfl                                   ?
054500150526       //--------------------------------------------------------------
054600150526       BEGSR  sr_F08S01;
054700150526
054800150526         if  Not Ord_x_Des;
054900150526             Ord_x_Des = *on;
055000150526         else;
055100150526           Ord_x_Des = *off;
055200150526         endif;
055300150526
055400150526         // -?SubFile già completamente caricato: basta riordinarlo?
055500150526         exsr  sr_SortSfl;
055600150526
055700150526       ENDSR;
055800150526
055900150526       //--------------------------------------------------------------
056000150526       //?Ordinamento subfile                                          ?
056100150526       //?  This subroutine sorts the subfile records.                 ?
056200150526       //--------------------------------------------------------------
056300150526       BEGSR  sr_SortSfl;
056400150526
056500150526         // -?Impostazione NRR dell'ultimo record del subfile          ?
056600150526         //  ?(già fatto in sr_InzS01:                                 ?
056700150526         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
056800150526         //RrnLast  = S01nrr;
056900150526
057000150526         C1RcdNbr = 1;
057100150526         clear  C1CsrRrn;
057200150526         clear  S01nrr;
057300150526         SflNxtChg   = *off;
057400150526
057500150526         //?___________________________________________________________?
057600150526         //?Initialize the key fields to sort on.?
057700150526         //?There is one extra field not in the subfile -- Selected --?
057800150526         //?that is added to the record. This field is used to place?
057900150526         //?selected records in front of nonselected records.?
058000150526         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
058100150526
058200150526         clear  QLGKL;
058300150526         clear  QLGSKL;
058400150526         clear  QLGSCB;
058500150526         clear  QLGSCB00;
058600150526
058700150526         // -?Gestione della scelta ordinamento:?
058800150526         Select;
058900150526
059000150526           // -?Ordinamento per Codice Cliente?
059100150526           when  Not Ord_x_Des;
059200150526             // -?1 campi chiave?
059300150526             QLGNBRK  = 2;
059400150526             // -?1° campo: tipologia (cliente/lnp)?
059500150526             //            ?a posizone    2,  1 byte, char,  Discendente
059600150526             QLGSP    = %size(S1opzione) + 1;
059700150526             QLGSS    = %size(S1htps);
059800150526             QLGDT    = c_Carattere;
059900150526             QLGSO    = c_discendente;
060000150526             QLGKL(1) = QLGSKL;
060100150526             // -?2° campo: codice  (S1Ccod)?
060200150526             //            ?a posizone    3,  7 byte, char,  ascending.?
060300150526             QLGSP    = %size(S1opzione) + %size(s1htps) + 1;
060400150526             QLGSS    = %size(S1Ccod);
060500150526             QLGDT    = c_Carattere;
060600150526             QLGSO    = c_Ascendente;
060700150526             QLGKL(2) = QLGSKL;
060800150526
060900150526           // -?Ordinamento per Descrizione Cliente?
061000150526           when  Ord_x_Des;
061100150526             // -?3 campi chiave?
061200150526             QLGNBRK  = 3;
061300150526             // -?1° campo: tipologia      (cliente/lnp)
061400150526             //            ?a posizione   2 , 1  byte, char, Discendente
061500150526             QLGSP    = %size(S1opzione) +  1;
061600150526             QLGSS    = %size(S1htps);
061700150526             QLGDT    = c_Carattere;
061800150526             QLGSO    = c_Discendente;
061900150526             QLGKL(1) = QLGSKL;
062000150526             // -?2° campo: descr.  (S1cdes)?
062100150526             //            ?a posizone   10, 40 byte, char, ascending.?
062200150526             QLGSP    = %size(S1opzione) + %size(S1htps) + %size(s1ccod) + 1;
062300150526             QLGSS    = %size(S1cdes);
062400150526             QLGDT    = c_Carattere;
062500150526             QLGSO    = c_Ascendente;
062600150526             QLGKL(2) = QLGSKL;
062700150526             // -?2° campo: codice  (S1Ccod)?
062800150526             //            ?a posizione    3,  7 byte, char,  ascending.?
062900150526             QLGSP    = %size(S1opzione) + %size(s1htps) + 1;
063000150526             QLGSS    = %size(S1Ccod);
063100150526             QLGDT    = c_Carattere;
063200150526             QLGSO    = c_Ascendente;
063300150526             QLGKL(3) = QLGSKL;
063400150526
063500150526         EndSl;
063600150526
063700150526         // -?Load other sort parameters.?
063800150526         QLGLB   = 80 + 16 * c_MaxKey;
063900150526         QLGRL   = %size(SflRcd) - 1;
064000150526         QLGRT   = 8;
064100150526         QLGOKL  = 80;
064200150526         QLGLKE  = 16;
064300150526         QLGLSS  = 290;
064400150526         // -?Initialize Sort I/O API fields.?
064500150526         QLGRL00 = QLGRL;
064600150526         QLGRC00 = 1;
064700150526         clear  QUSEI;
064800150526         QUSBPRV = %size(QUSEC);
064900150526
065000150526         // -?First step - Initialize the sort routine.?
065100150526         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
065200150526                      SizeList : ReturnSize : QUSEC );
065300150526
065400150526         // -?Next step - Write records to I/O routine.?
065500150526         QLGRT00  = c_Put;
065600150526         For  Nrr = 1  To  RrnLast;
065700150526           chain  Nrr  TBBSPS01;
065800150526           // -?Solo le righe con Selected = 'Y' sono riordinate,?
065900150526           //  ?quindi per fare un ordinamento di tutte le righe?
066000150526           //  ?metto 'Y' sempre.?
066100150526           Selected = 'Y';
066200150526           clear  QUSEI;
066300150526           QUSBPRV  = %size(QUSEC);
066400150526           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
066500150526                         SizeList : NotUsed : QUSEC );
066600150526         EndFor;
066700150526
066800150526         // -?Next step - Signal end of input, clear subfile for reload.?
066900150526         QLGRT00 = c_EndPut;
067000150526         clear  QUSEI;
067100150526         QUSBPRV = %size(QUSEC);
067200150526         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
067300150526                       SizeList : NotUsed : QUSEC );
067400150526
067500150526         // -?Pulizia subfile?
067600150526         SflDsp_N    = *on;
067700150526         SflDspCtl_N = *on;
067800150526         write  TBBSPC01;
067900150526         SflDspCtl_N = *off;
068000150526         SflEnd      = *off;
068100150526
068200150526         // -?Final step - Write the records back to the subfile.?
068300150526         QLGRT00  = c_Get;
068400150526         For  Nrr = 1  To RrnLast;
068500150526           clear  QUSEI;
068600150526           QUSBPRV = %size(QUSEC);
068700150526           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
068800150526                         QLGRL00  : NotUsed : QUSEC );
068900150526           // -?Ripristino indicatori utilizzati nel sfl rec?
069000150526           SflNxtChg = (S1opzione <> *zero);
069100150526           // -?e aggiorno?
069200150526           S01nrr = Nrr;
069300150526           write  TBBSPS01;
069400150526         EndFor;
069500150526
069600150526         C1CsrRrn = 1;
069700150526
069800150526         // -?Visualizzazione del SFL (se ci sono dati)?
069900150526         SflDsp_N = (S01nrr <= *zero);
070000150526
070100150526         // -?Attivazione del SFLEND?
070200150526         SflEnd = *on           ;
070300150526
070400150526       ENDSR;
070500151216       //--------------------------------------------------------------
070600151216       //?verifica parzializzazione per terminal di partenza           ?
070700151216       //--------------------------------------------------------------
070800151216       BEGSR  sr_tfp;
070900151216       // non in partenza da più filiali: la sbsp contiene elenco dei TERMINAL
071000160111       // se record per cliente, se record per filiale, invece verifico che
071100160111       // sia nella £1 del TFP
071200151216             if bspfpa=' ' ;
071300160111              if s1htps='P';
071400160111                if %lookup(bspfil:l1)<=0;
071500160111                   welabora='N';
071600160111                endif;
071700160111              else;
071800151216                if %lookup(ibsptfp:sbsp)<=0;
071900151216                   welabora='N';
072000151216                endif;
072100160111              endif;
072200151216             else;
072300160111       //     in partenza da più filiali: la sbsp contiene elenco delle FILIALI
072400160111       //     quindi carico record solo se filiali presenti nella £1 del TFP
072500151216                clear wtrov;
072600151216                for i=1 to %elem(l1);
072700151216                   if l1(i)=0 or wtrov='S';
072800151216                      leave;
072900151216                   endif;
073000151216                   if %lookup(l1(i):sbsp)>0    ;
073100151216                      wtrov='S';
073200151216                   endif;
073300151216                endfor ;
073400151216                if wtrov=*blanks;
073500151216                   welabora='N';
073600151216                endif;
073700151216             endif;
073800151216       ENDSR;
073900130723       //--------------------------------------------------------------
074000130723       //?Operazioni finali.                                           ?
074100130723       //--------------------------------------------------------------
074200130723       BEGSR  sr_RoutEnd;
074300130723
074400130723
074500130723         //  ?Uscita?
074600150525         kpjbu=fnlsbsp1ds;
074700130723         return;
074800130723
074900130723       ENDSR;
075000130723
075100130723      /end-free
075200130723
075300130723       //--------------------------------------------------------------
075400130723       //?Definizione schiere a tempo di compilazione.                 ?
075500130723       //--------------------------------------------------------------
