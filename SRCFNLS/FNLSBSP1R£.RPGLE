000200130723       //==============================================================
000300150521       //?Selezione cliente da FIBSP00F
000301150521       //?            (Abilitazione allo spostamento in sospensione se ?
000302150521       //?             non spuntati i colli)                           ?
000400130723       //==============================================================
000500130723
000600130723       //--------------------------------------------------------------
000700130723       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800130723       //--------------------------------------------------------------
000900130723
001000130723     /*PRM  dbgview(*source)
001100130723     /*END
001200130723
001300130723       //--------------------------------------------------------------
001400130723       //?Specifiche di controllo.                                     ?
001500130723       //--------------------------------------------------------------
001600130723
001700130723     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000130723
002100130723       //--------------------------------------------------------------
002200130723       //?Dichiarazione file.                                          ?
002300130723       //--------------------------------------------------------------
002400130723
002700130723
002701151216     fazorg01l  if   e           k disk
002702150518       // -?File clienti/filiali abilitati
002703150521     fFIBSP01L  if   e           k disk
002704150521     fFIBSP02L  if   e           k disk    rename(fibsp000:fibsp02)
002902150505
003100130723
003200130723       // -?Video?
003300150525     ffnlsBSP1d cf   e             workstn sfile(tbbsps01:s01nrr)
003400130723     f                                     indds(IndDspF)
003500130723     f                                     infds(InfDspF)
003600130723
003700130723       //--------------------------------------------------------------
003800130723       //?Definizione costanti.                                        ?
003900130723       //--------------------------------------------------------------
004300130723
004400130723       // -?Costante per controllo "caratteri solo numerici"?
004500130723     d c_Digits        c                   const('0123456789')
004600131212
004700131212       // -?Costanti per la definizione delle schiere con i nomi?
005100130723
005200130723       // -?Tasti funzionali a video?
005300130723     d c_F01           c                   const(x'31')
005400130723     d c_F02           c                   const(x'32')
005500130723     d c_F03           c                   const(x'33')
005600130723     d c_F04           c                   const(x'34')
005700130723     d c_F05           c                   const(x'35')
005800130723     d c_F06           c                   const(x'36')
005900130723     d c_F07           c                   const(x'37')
006000130723     d c_F08           c                   const(x'38')
006100130723     d c_F09           c                   const(x'39')
006200130723     d c_F10           c                   const(x'3A')
006300130723     d c_F11           c                   const(x'3B')
006400130723     d c_F12           c                   const(x'3C')
006500130723     d c_F13           c                   const(x'B1')
006600130723     d c_F14           c                   const(x'B2')
006700130723     d c_F15           c                   const(x'B3')
006800130723     d c_F16           c                   const(x'B4')
006900130723     d c_F17           c                   const(x'B5')
007000130723     d c_F18           c                   const(x'B6')
007100130723     d c_F19           c                   const(x'B7')
007200130723     d c_F20           c                   const(x'B8')
007300130723     d c_F21           c                   const(x'B9')
007400130723     d c_F22           c                   const(x'BA')
007500130723     d c_F23           c                   const(x'BB')
007600130723     d c_F24           c                   const(x'BC')
007700130723     d c_Enter         c                   const(x'F1')
007800130723     d c_RollDown      c                   const(x'F4')
007900130723     d c_RollUp        c                   const(x'F5')
008000130723
008100130723       //--------------------------------------------------------------
008200130723       //?Definizione schiere.                                         ?
008300130723       //--------------------------------------------------------------
009200130723
009300130723       // -?Messaggi di errore?
009500130723
009600130723       //--------------------------------------------------------------
009700130723       //?Definizione aree dati.                                       ?
009800130723       //--------------------------------------------------------------
009900130723
010000130723       // -?Dati utente?
010100130723     d §AzUte        e ds                  extname(AZUTE00F)
010200130723     d                                     dtaara
010300130723     d §DatiUte      e ds                  extname(dDatiUte)
010400130723     d                                     dtaara
010500130723
010600130723       //--------------------------------------------------------------
010700130723       //?Definizione strutture dati.                                  ?
010800130723       //--------------------------------------------------------------
010900130723
011000130723       // -?Status ds?
011100130723     d Status         sds
011200130723     d   SDSpgm          *proc
011300130723
011400130723       // -?InfDS?
011500130723     d InfDspF         ds
011600130723     d   dsp_aid             369    369a
011700130723
011800130723       // -?Indicatori su DspF?
011900130723     d IndDspF         ds                  inz
012000130723         // -?Abilitazione tasti funzionali?
012001150526     d   F8Attivo                      n   overlay(IndDspF : 08)
012401150520         // -?Indicatori di gestione del subfile?
012402150520     d   SflDsp_N                      n   overlay(IndDspF : 23)
012403150520     d   SflDspCtl_N                   n   overlay(IndDspF : 20)
012404150520     d   SflNxtChg                     n   overlay(IndDspF : 22)
012405150520     d   SflEnd                        n   overlay(IndDspF : 21)
012500130723         // -?Emissione messaggio di errore?
012700130723         // -?Indicatori per Attibuti di visualizzazione?
012701150525     d   DisabilitOpz                  n   overlay(IndDspF : 40)
012702150526     d   Ord_x_Des                     n   overlay(IndDspF : 41)
014034150526     d*campolps01      ds            42
014035150526     d* S1cLP1
014036150526     d* S1cLP2
014037150526     d* S1cLP3
014038150526     d* S1cLP4
014039150526     d* S1cLP5
014040150526     d* S1cLP6
014041150526     d* S1cLP7
014042150526     d* S1cLP8
014043150526     d* S1cLP9
014044150526     d* S1cLp10
014045150526     d* S1cLp11
014046150526     d* S1cLp12
014047150526     d* S1cLp13
014048150526     d* S1cLp14
014100130723
014200130723       // -?Parametri ricevuti?
014300130723     d KPJBA         e ds
014301150525     d fnlsbsp1ds    e ds
014302151216       // - Caricamento Filiali in £1
014303151216     d TRUL06DS      e ds
014304151216     d  L1                     1     90  0 dim(30)
014305151215     d                 ds
014306151215     d bsplin
014307151215     d sbsp                           3  0 dim(30) overlay(bsplin)
015501150520
015600130723
015700130723       //--------------------------------------------------------------
015800130723       //?Definizione variabili globali.                               ?
015900130723       //--------------------------------------------------------------
016000130723
017100130723
017101150526       // -?Flags booleani?
017102150525     d $Fine           s               n   inz
017700130723
017800130723       // -?Campi di comodo?
018100131212     d wDate           s              8  0 inz
018208150520     d s01nrr          s              4  0
018209151216     d welabora        s              1
018210151216     d wtrov           s              1
018211151216     d i               s              3  0
018300130723
018301150526       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
018302150526
018303150526       //--------------------------------------------------------------
018304150526       // -?Constants?
018305150526       //--------------------------------------------------------------
018306150526       // -?MaxKey - Maximum number of key fields allowed by this program?
018307150526     d c_MaxKey        c                   const(3)
018308150526       // -?Sort order:?
018309150526       //  ?1 = Sort in an ascending sequence?
018310150526       //  ?2 = Sort in a descending sequence?
018311150526     d c_Ascendente    c                   const(1)
018312150526     d c_Discendente   c                   const(2)
018313150526       // -?Key data type:?
018314150526       //  ? 0 = Signed binary?
018315150526       //  ? 2 = Signed zoned decimal?
018316150526       //  ? 3 = Signed packed decimal?
018317150526       //  ? 6 = Character with no national language sort sequence applied,?
018318150526       //  ?     if specified?
018319150526       //  ? 7 = Unsigned packed decimal?
018320150526       //  ?     All numbers will have the sign forced positive ('F'X).?
018321150526       //  ? 8 = Unsigned zoned decimal?
018322150526       //  ?     All numbers will have the sign forced positive ('F'X).?
018323150526       //  ? 9 = Unsigned binary?
018324150526       //  ?14 = Date in form of DD/MM/YY?
018325150526       //  ?15 = Date in form of DD.MM.YYYY?
018326150526     d c_Numero        c                   const(2)
018327150526     d c_Carattere     c                   const(6)
018328150526     d c_NumIntero     c                   const(8)
018329150526       //
018330150526     d c_Put           c                   const(1)
018331150526     d c_EndPut        c                   const(2)
018332150526     d c_Get           c                   const(3)
018333150526
018334150526       //--------------------------------------------------------------
018335150526       // -?Data Structures?
018336150526       //  ?SflRcd     - The entire subfile record to pass to the sort?
018337150526       //  ?QLGSCB     - The sort request block for the QLGSORT API?
018338150526       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
018339150526       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
018340150526       //  ?QUSEC      - Error structure for the QLGSORT API?
018341150526       //--------------------------------------------------------------
018342150526     d SflRcd          ds                  inz
018343150526     d   S1opzione                         inz
018344150526     d   s1htps                            inz
018345150526     d   S1Ccod                            inz
018346150526     d   S1cdes                            inz
018347150526     d   S1Hlnp                            inz
018348150526     d   S1Hksc                            inz
018349150526     d   S1Cfpa                            inz
018350150526     d   S1Caut                            inz
018351150526     d   s1cdlin                           inz
018352150526     d   campolps01                  42    inz
018353150526     d   s1clp1                            inz overlay(campolps01:1)
018354150526     d   s1clp2                            inz overlay(campolps01:4)
018355150526     d   s1clp3                            inz overlay(campolps01:7)
018356150526     d   s1clp4                            inz overlay(campolps01:10)
018357150526     d   s1clp5                            inz overlay(campolps01:13)
018358150526     d   s1clp6                            inz overlay(campolps01:16)
018359150526     d   s1clp7                            inz overlay(campolps01:19)
018360150526     d   s1clp8                            inz overlay(campolps01:22)
018361150526     d   s1clp9                            inz overlay(campolps01:25)
018362150526     d   s1clp10                           inz overlay(campolps01:28)
018363150526     d   s1clp11                           inz overlay(campolps01:31)
018364150526     d   s1clp12                           inz overlay(campolps01:34)
018365150526     d   s1clp13                           inz overlay(campolps01:37)
018366150526     d   s1clp14                           inz overlay(campolps01:40)
018367150526     d   s1cpiu                            inz
018368150526     d   s1hdes                            inz
018369150526     d   Selected                     1a   inz
018370150526      /copy QSYSINC/QRPGLESRC,QLGSORT
018371150526     d QLGKL                         16    dim(c_MaxKey)
018372150526     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
018373150526     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
018374150526     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
018375150526     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
018376150526      /copy QSYSINC/QRPGLESRC,QLGSRTIO
018377150526      /copy QSYSINC/QRPGLESRC,QUSEC
018378150526
018379150526       //--------------------------------------------------------------
018380150526       // -?Standalone fields?
018381150526       //  ?Nrr        - Relative record number for subfile?
018382150526       //  ?SizeList   - Size of list?
018383150526       //  ?ReturnSize - Size of list returned by sort API?
018384150526       //--------------------------------------------------------------
018385150526     d NotUsed         s             16a   inz
018386150526     d ReturnSize      s              9b 0 inz
018387150526     d SizeList        s              9b 0 inz
018388150526     d RrnLast         s              5i 0 inz
018389150526     d Nrr             s              5i 0 inz
018400130723       //--------------------------------------------------------------
018500130723       //?Definizione prototipi procedure.                             ?
018600130723       //--------------------------------------------------------------
019200130723
019300130723       // -?Reperimento dati utente?
019400130723     d TIBS34ds      e ds
019500130723      /copy gaitrasrc/srcProtoPR,TIBS34R
019600130723
019601150526       // -?Ordinamento subfile?
019602150526      /copy gaitrasrc/srcProtoPr,QLGSRTIO
019603151216       // - Caricamento Filiali in £1
019604151216     d TRUL06R         pr                  extpgm('TRUL06R')
019605151216     d  kpjba                              likeds(kpjba)
020600130724
021900130723
022000130723       //--------------------------------------------------------------
022100130723       //?Definizione key-list.                                        ?
022200130723       //--------------------------------------------------------------
022300130723
022700130723
022800130723       //--------------------------------------------------------------
022900130723       //?Riepilogo indicatori utilizzati.                             ?
023000130723       //--------------------------------------------------------------
023100130723       //--------------------------------------------------------------
023200130723
023300130723       //--------------------------------------------------------------
023400130723       //?M A I N - L I N E                                            ?
023500130723       //--------------------------------------------------------------
023600130723
023700130723     c     *Entry        plist
023800130723     c                   parm                    KPJBA
023801150525     c                   movel     kpjbu         fnlsbsp1ds
023802150525     c* pulizia campi di output
023803150525     c                   clear                   OBSPTPS
023804150525     c                   clear                   OBSPCLI
023805150525     c                   clear                   OBSPLIN
023806150525     c                   clear                   OBSPDES
023807150525     c                   clear                   OBSPSOD
023808150525     c                   clear                   OBSPFXX
023810150525     c                   clear                   OBSPERR
023900150525     c                   clear                   OBSPMSG
024000130723      /free
024100130723
024200130723       // -?Operazioni iniziali?
024300130723       exsr  sr_RoutInz;
024301150521
024400150521       exsr sr_gesc01;
026700130723
026800130723       // -?Operazioni finali?
026900130723       exsr  sr_RoutEnd;
027000130723
027100130723       //--------------------------------------------------------------
027200130723       //?Operazioni iniziali.                                         ?
027300130723       //--------------------------------------------------------------
027400130723       BEGSR  sr_RoutInz;
027500130723
027600130723         *inLR = *on;
027700130723
027800130723         // -?Reperimento dati job?
027900130723         exsr  sr_DatiJob;
028000130723
028100130723         // -?Impostazione nome programma a video?
028200130723         V1Tpgm = SDSpgm;
032600131212
032700131212         // -?Reperimento data odierna?
032800131212         wDate = %int( %subst( %char( %dec( %timestamp() ) )
032900131212                               : 1 : 8 ) );
032901160111         // -?Caricamento L1 se ricevuto il TFP
032902160111         if ibsptfp>0;
032903160111                clear trul06ds ;
032904160111                d06cod='£1';
032905160111                d06key=%editc(ibsptfp:'X');
032906160111                d06tla='L' ;
032907160111                kpjbu=trul06ds;
032908160111                trul06r(kpjba) ;
032909160111                TRUL06DS = kpjbu;
032910160111         endif;
033000130723
033100130723       ENDSR;
033200130723
033300130723       //--------------------------------------------------------------
033400130723       //?Reperimento Dati del job (Utente/Operativi).                 ?
033500130723       //--------------------------------------------------------------
033600130723       BEGSR  sr_DatiJob;
033700130723
033800130723         in(E) §AzUte;
033900130723         if NOT %error;
034000130723           in(E) §DatiUte;
034100130723         endif;
034200130723         if %error or RSut = *blanks;
034300130723           clear TIBS34ds;
034400130723           tibs34r ( tibs34ds );
034500130723           in §AzUte;
034600130723           in §DatiUte;
034700130723         endif;
034800130723
034900130723       ENDSR;
035000130723
094758150519       //--------------------------------------------------------------
094759150520       //?Gestione SFL di scelta record da FIBSP per lnp o ksc         ?
094760150519       //--------------------------------------------------------------
094761150519       BEGSR  sr_gesc01 ;
094762150520         // -?Inizializzazione Sfl    ?
094763150520         s01nrr = 0;
094764150526         clear  C1RcdNbr;
094765150526         clear  C1CsrRrn;
094766150520         sfldspCtl_N = *on;
094767150520         write tbbspc01;
094768150520         sfldsp_N = *off;
094769150520         sfldspCtl_N = *off;
094770150520         sflend = *off;
094771150520         sflnxtchg = *off;
094772150520         clear  C1RcdNbr;
094777150525         select;
094778150525         when ibsptps='P';
094780150525            s1htps='P' ;
094781150525         when ibsptps='C';
094783150525            s1htps='C' ;
094784150525         when ibsptps=' ';
094786150525            s1htps='P' ;
094787150525         endsl;
094788150525         // -?Caricamento Sfl         ?
094789150525         exsr carsfl;
094839150525       // SE richiesto entrambi i tipi (linea di partenza e cliente)
094840150525       // se ho elaborato i "P" ripeto ciclo per "C"
094841150525         if ibsptps=' ' and s1htps='P';
094842150525            s1htps='C';
094843150525            exsr carsfl;
094844150525         endif;
094845150525
094846150520       // se scritto almeno un record
094847150520        if s01nrr > 0;
094848150520       // indicatore di sflend
094849150520         eval sflend = *on;
094850150520         C1RcdNbr  = 1;
094851150520        endif;
094852150520
094853150520       // se non scritto neanche un record
094854150520        if s01nrr = 0;
094855150520       // indicatore di sfldsp
094856150520         eval sfldsp_N = *on;
094857150520         clear C1RcdNbr;
094858150520        endif;
094859150520         // -?Emissione Videata       ?
094862150525         dow not $fine=*on;
094863150526            write tbbspp03;
094864150520            exfmt tbbspc01;
094865150520       // controllo tasti funzionali del subfile
094866150520            select;
094867150520
094868150526           // -?F3=Fine?
094869150520            when dsp_aid=c_F03;
094870150525               obspfxx='03';
094871150520               $fine=*on;
094872150526           // -?F12=Ritorno?
094874150520            when dsp_aid=c_F12;
094875150525               $fine=*on;
094876150525               obspfxx='12';
094877150526           // -?F8=Cambio ordinamento?
094878150526           WHEN  dsp_aid = c_F08;
094879150526             exsr  sr_F08S01;
094880150520            other;
094881150525             if ibspmod=*blanks;
094882150520               readc tbbsps01;
094883150525               dow not %eof and not $fine = *on;
094884150520              // se è stata immessa un'opzione
094885150526                  C1RcdNbr = S01nrr;
094886150520                  if s1opzione <> *zeros;
094887150520
094888150525                     select;
094889150525                     when s1htps='P'  ;
094890150525                        obsplin=s1hlnp;
094891150525                     when s1htps='C'  ;
094892150525                        obspcli=s1hksc;
094893150525                     endsl;
094894150525                     obspdes=s1hdes;
094895150525                     obsptps=s1htps;
094896150525                     $fine=*on;
094897150520
094898150520                  endif;
094899150520
094900150520                  readc tbbsps01;
094901150520               enddo;
094902150525             endif;
094903150520
094904150520
094905150520            endsl;
094906150525           enddo;
094907150519       ENDSR;
094908150525       //--------------------------------------------------------------
094909150525       //?Caricamento sfl                                              ?
094910150525       //--------------------------------------------------------------
094911150525       BEGSR  carsfl    ;
094912150525         If  s1htps='P'                     ;
094913150525            setll (*loval) fibsp01l;
094914150525            read fibsp01l;
094915150525            s1htps='P' ;
094916150525         else;
094917150525            setll (*loval) fibsp02l;
094918150525            read fibsp02l;
094919150525            s1htps='C' ;
094920150525         endif;
094921150525         DisabilitOpz = (iBSPmod = 'V');
094922150526         Ord_x_Des = *off;
094923151216         C1Ctfp = iBSPtfp;
094924151216         if  iBSPtfp = *zero;
094925151216           C1Dtfp = 'TUTTI';
094926151216         else;
094927151216           chain  (C1Ctfp)  AZORG;
094928151216           if  %found(AZORG01L);
094929151216             C1Dtfp = ORGdes;
094930151216           else;
094931151216             C1Dtfp = *all'? ';
094932151216           endif;
094933151216         endif;
094934150525         // -?Caricamento      Sfl    ?
094935150525         dow not %eof;
094936150525
094937151209            if ibspfpa= ' ' or
094938151209               (ibspfpa='E' and bspfpa=' ') or
094939160111               (ibspfpa='S' and bspfpa='S') or
094940160111               (ibspfpa='C' and ((bspfpa='S' and bspaut='N') or
094941160111                bspfpa=' '));
094942151216         // - Se richiesta parzializzazione per tfp verifico
094943151216         //   se record da caricare
094944151216             clear welabora;
094945151216             if ibsptfp>0;
094946151216                exsr sr_tfp;
094947151216             endif;
094961151216          if ibsptfp=0 or welabora=*blanks;
094962150525            clear s1opzione;
094963150525            clear s1hksc;
094964150525            clear s1hlnp;
094965150525            clear s1cfpa;
094966150526            campolps01 = *zeros;
094967150525            clear s1cdlin;
094968150525            clear s1cpiu ;
094969150525            clear s1caut ;
094970150526            s1cdes= bspdes;
094971150525
094972150525            If  S1HTPS ='P'                  ;
094973150526               s1ccod= %editc(bspfil:'X');
094974150525               s1hlnp=bspfil;
094975150525            else ;
094976150526               s1ccod=%editc(bspksc:'X');
094977150525               s1hksc=bspksc;
094978150525            endif;
094979150525         // Flag "Part. da + fil"  e elenco filiali abilitate
094980150525            if bspfpa='S';
094981150525               s1cfpa='Si';
094982150525               s1cdlin='FilAbi:' ;
094983150525               campolps01=bsplin;
094984150525            else;
094985150525               if s1hksc>0;
094986150525         // Se record "cliente" --> elenco ter-part. abilitati
094987150525                  s1cdlin='TerPar:';
094988150525                  campolps01=bsplin;
094989150525               endif;
094990150525            endif;
094991150525            if campolps01>*zeros   and %subst(bsplin:43:3)>*zeros;
094992150525               s1cpiu='+';
094993150525            endif;
094994150525            if bspaut='N';
094995150525               s1caut='No';
094996150525            endif;
094997150525            s1hdes=bspdes;
094998150525            s01nrr+= 1;
094999150525            write tbbsps01;
095000151215          endif;
095001151209            endif;
095002150525            If  S1HTPS ='P'                  ;
095003150525               read fibsp01l;
095004150525            else;
095005150525               read fibsp02l;
095006150525            endif;
095007150525         enddo;
095008150526         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
095009150526         RrnLast  = S01nrr;
095010150526
095011150526         // -?Impaginazione subfile?
095012150526         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
095013150526         if S01nrr > *zero;
095014150526           C1RcdNbr  = 1;
095015150526           C1CsrRrn  = 1;
095016150526         else;
095017150526           clear C1RcdNbr;
095018150526         endif;
095019150526
095020150526         // -?Ordinamento subfile (se premuto F8)?
095021150526         if  Ord_x_Des  and  S01nrr > 1;
095022150526           exsr  sr_SortSfl;
095023150526         endif;
095024150526
095025150526         // -?(Dis)Abilitazione tasti funzionali?
095026150526         F8Attivo  = (S01nrr > *zero); // -?F8 = Ordinamento x ...?
095027150525       ENDSR;
095028150526       //--------------------------------------------------------------
095029150526       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
095030150526       //?F08-Cambia ordinamento sfl                                   ?
095031150526       //--------------------------------------------------------------
095032150526       BEGSR  sr_F08S01;
095033150526
095034150526         if  Not Ord_x_Des;
095035150526             Ord_x_Des = *on;
095036150526         else;
095037150526           Ord_x_Des = *off;
095038150526         endif;
095039150526
095040150526         // -?SubFile già completamente caricato: basta riordinarlo?
095041150526         exsr  sr_SortSfl;
095042150526
095043150526       ENDSR;
095044150526
095045150526       //--------------------------------------------------------------
095046150526       //?Ordinamento subfile                                          ?
095047150526       //?  This subroutine sorts the subfile records.                 ?
095048150526       //--------------------------------------------------------------
095049150526       BEGSR  sr_SortSfl;
095050150526
095051150526         // -?Impostazione NRR dell'ultimo record del subfile          ?
095052150526         //  ?(già fatto in sr_InzS01:                                 ?
095053150526         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
095054150526         //RrnLast  = S01nrr;
095055150526
095056150526         C1RcdNbr = 1;
095057150526         clear  C1CsrRrn;
095058150526         clear  S01nrr;
095059150526         SflNxtChg   = *off;
095060150526
095061150526         //?___________________________________________________________?
095062150526         //?Initialize the key fields to sort on.?
095063150526         //?There is one extra field not in the subfile -- Selected --?
095064150526         //?that is added to the record. This field is used to place?
095065150526         //?selected records in front of nonselected records.?
095066150526         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
095067150526
095068150526         clear  QLGKL;
095069150526         clear  QLGSKL;
095070150526         clear  QLGSCB;
095071150526         clear  QLGSCB00;
095072150526
095073150526         // -?Gestione della scelta ordinamento:?
095074150526         Select;
095075150526
095076150526           // -?Ordinamento per Codice Cliente?
095077150526           when  Not Ord_x_Des;
095078150526             // -?1 campi chiave?
095079150526             QLGNBRK  = 2;
095080150526             // -?1° campo: tipologia (cliente/lnp)?
095081150526             //            ?a posizone    2,  1 byte, char,  Discendente
095082150526             QLGSP    = %size(S1opzione) + 1;
095083150526             QLGSS    = %size(S1htps);
095084150526             QLGDT    = c_Carattere;
095085150526             QLGSO    = c_discendente;
095086150526             QLGKL(1) = QLGSKL;
095087150526             // -?2° campo: codice  (S1Ccod)?
095088150526             //            ?a posizone    3,  7 byte, char,  ascending.?
095089150526             QLGSP    = %size(S1opzione) + %size(s1htps) + 1;
095090150526             QLGSS    = %size(S1Ccod);
095091150526             QLGDT    = c_Carattere;
095092150526             QLGSO    = c_Ascendente;
095093150526             QLGKL(2) = QLGSKL;
095094150526
095095150526           // -?Ordinamento per Descrizione Cliente?
095096150526           when  Ord_x_Des;
095097150526             // -?3 campi chiave?
095098150526             QLGNBRK  = 3;
095099150526             // -?1° campo: tipologia      (cliente/lnp)
095100150526             //            ?a posizione   2 , 1  byte, char, Discendente
095101150526             QLGSP    = %size(S1opzione) +  1;
095102150526             QLGSS    = %size(S1htps);
095103150526             QLGDT    = c_Carattere;
095104150526             QLGSO    = c_Discendente;
095105150526             QLGKL(1) = QLGSKL;
095106150526             // -?2° campo: descr.  (S1cdes)?
095107150526             //            ?a posizone   10, 40 byte, char, ascending.?
095108150526             QLGSP    = %size(S1opzione) + %size(S1htps) + %size(s1ccod) + 1;
095109150526             QLGSS    = %size(S1cdes);
095110150526             QLGDT    = c_Carattere;
095111150526             QLGSO    = c_Ascendente;
095112150526             QLGKL(2) = QLGSKL;
095113150526             // -?2° campo: codice  (S1Ccod)?
095114150526             //            ?a posizione    3,  7 byte, char,  ascending.?
095115150526             QLGSP    = %size(S1opzione) + %size(s1htps) + 1;
095116150526             QLGSS    = %size(S1Ccod);
095117150526             QLGDT    = c_Carattere;
095118150526             QLGSO    = c_Ascendente;
095119150526             QLGKL(3) = QLGSKL;
095120150526
095121150526         EndSl;
095122150526
095123150526         // -?Load other sort parameters.?
095124150526         QLGLB   = 80 + 16 * c_MaxKey;
095125150526         QLGRL   = %size(SflRcd) - 1;
095126150526         QLGRT   = 8;
095127150526         QLGOKL  = 80;
095128150526         QLGLKE  = 16;
095129150526         QLGLSS  = 290;
095130150526         // -?Initialize Sort I/O API fields.?
095131150526         QLGRL00 = QLGRL;
095132150526         QLGRC00 = 1;
095133150526         clear  QUSEI;
095134150526         QUSBPRV = %size(QUSEC);
095135150526
095136150526         // -?First step - Initialize the sort routine.?
095137150526         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
095138150526                      SizeList : ReturnSize : QUSEC );
095139150526
095140150526         // -?Next step - Write records to I/O routine.?
095141150526         QLGRT00  = c_Put;
095142150526         For  Nrr = 1  To  RrnLast;
095143150526           chain  Nrr  TBBSPS01;
095144150526           // -?Solo le righe con Selected = 'Y' sono riordinate,?
095145150526           //  ?quindi per fare un ordinamento di tutte le righe?
095146150526           //  ?metto 'Y' sempre.?
095147150526           Selected = 'Y';
095148150526           clear  QUSEI;
095149150526           QUSBPRV  = %size(QUSEC);
095150150526           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
095151150526                         SizeList : NotUsed : QUSEC );
095152150526         EndFor;
095153150526
095154150526         // -?Next step - Signal end of input, clear subfile for reload.?
095155150526         QLGRT00 = c_EndPut;
095156150526         clear  QUSEI;
095157150526         QUSBPRV = %size(QUSEC);
095158150526         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
095159150526                       SizeList : NotUsed : QUSEC );
095160150526
095161150526         // -?Pulizia subfile?
095162150526         SflDsp_N    = *on;
095163150526         SflDspCtl_N = *on;
095164150526         write  TBBSPC01;
095165150526         SflDspCtl_N = *off;
095166150526         SflEnd      = *off;
095167150526
095168150526         // -?Final step - Write the records back to the subfile.?
095169150526         QLGRT00  = c_Get;
095170150526         For  Nrr = 1  To RrnLast;
095171150526           clear  QUSEI;
095172150526           QUSBPRV = %size(QUSEC);
095173150526           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
095174150526                         QLGRL00  : NotUsed : QUSEC );
095175150526           // -?Ripristino indicatori utilizzati nel sfl rec?
095176150526           SflNxtChg = (S1opzione <> *zero);
095177150526           // -?e aggiorno?
095178150526           S01nrr = Nrr;
095179150526           write  TBBSPS01;
095180150526         EndFor;
095181150526
095182150526         C1CsrRrn = 1;
095183150526
095184150526         // -?Visualizzazione del SFL (se ci sono dati)?
095185150526         SflDsp_N = (S01nrr <= *zero);
095186150526
095187150526         // -?Attivazione del SFLEND?
095188150526         SflEnd = *on           ;
095189150526
095190150526       ENDSR;
095191151216       //--------------------------------------------------------------
095192151216       //?verifica parzializzazione per terminal di partenza           ?
095193151216       //--------------------------------------------------------------
095194151216       BEGSR  sr_tfp;
095195151216       // non in partenza da più filiali: la sbsp contiene elenco dei TERMINAL
095196160111       // se record per cliente, se record per filiale, invece verifico che
095197160111       // sia nella £1 del TFP
095198151216             if bspfpa=' ' ;
095199160111              if s1htps='P';
095200160111                if %lookup(bspfil:l1)<=0;
095201160111                   welabora='N';
095202160111                endif;
095203160111              else;
095204151216                if %lookup(ibsptfp:sbsp)<=0;
095205151216                   welabora='N';
095206151216                endif;
095207160111              endif;
095208151216             else;
095209160111       //     in partenza da più filiali: la sbsp contiene elenco delle FILIALI
095210160111       //     quindi carico record solo se filiali presenti nella £1 del TFP
095215151216                clear wtrov;
095216151216                for i=1 to %elem(l1);
095217151216                   if l1(i)=0 or wtrov='S';
095218151216                      leave;
095219151216                   endif;
095220151216                   if %lookup(l1(i):sbsp)>0    ;
095221151216                      wtrov='S';
095222151216                   endif;
095223151216                endfor ;
095224151216                if wtrov=*blanks;
095225151216                   welabora='N';
095226151216                endif;
095227151216             endif;
095228151216       ENDSR;
111500130723       //--------------------------------------------------------------
111600130723       //?Operazioni finali.                                           ?
111700130723       //--------------------------------------------------------------
111800130723       BEGSR  sr_RoutEnd;
112400130723
112800130723
112900130723         //  ?Uscita?
112901150525         kpjbu=fnlsbsp1ds;
113000130723         return;
113100130723
113200130723       ENDSR;
113300130723
113400130723      /end-free
113500130723
113600130723       //--------------------------------------------------------------
113700130723       //?Definizione schiere a tempo di compilazione.                 ?
113800130723       //--------------------------------------------------------------
