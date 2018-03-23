000100020322     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020322      *------------------------------------------------------------------------*
000300171114      * Creaz. bolle + stampa etichetta segnacollo per invio materiale IT ai P.O
000400020322      *------------------------------------------------------------------------*
000700020528     fazorg01l  if   e           k disk
000701171114     ftntbe01l  iF   e           k disk
000702171115     ftabel00f  iF   e           k disk
000703171116     ftnsdutd   CF   E             workstn
001600171114     FFIVAB50F  O  A E             DISK
001601171115     FFIVAT50F  O  A E             DISK
001800171115     FFNLV22P   O    F  132        PRINTER usropn
003100020528
003201171114      * DS PER TRUL90DS - RICHIESTA STAMPANTI
003202171114     d trul90ds      e ds
003300131118     d
003400941114     D KPJBA         E DS
006700020920
006701171117     d tnsdmagds     e ds
006702171115     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006703171115     D fivabds       E DS                  EXTNAME(fivab50f)
006704171115     d trul33ds      e ds
007300171115     D tisi95ds      E DS
007400171115     D fnlv13ds      E DS
007401171115     d Tibs34ds      e ds                  inz
007402171115     d §azute        e ds                  Extname(Azute00f)
007403171115     d                                     dtaara
007404171115     d §Datiute      e ds                  EXTname(ddatiute)
007405171115     d                                     dtaara
007500131203     D DSBLD         E DS
007501171115     D DSBLDE        E DS                  INZ
007700171115     D* DS ESTERNA PER  STAMPA BOLLE
007800131203     d Fnlsb5ds      e ds
007900131203     d
008100080924     d
008700131203     D DSACO         E DS                  EXTNAME(CNACO00F)
008800131203     D DSIND         E DS                  EXTNAME(CNIND00F)
008900131203     D DSCLP         E DS                  EXTNAME(CNCLP00F)
009000131203     D DSCLS         E DS                  EXTNAME(FNCLS00F)
009100131203     D DSBS69        E DS                  EXTNAME(TIBS69DS)
009101171115
009102171117     d og148         E DS
009103171117     d og143         E DS
009104171117     d dvmg          E DS
009105171115     d ds3a          E DS
009200120322     d
009201171114     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
009202171115     D msg             S             78    DIM(5) CTDATA PERRCD(1)
009203171114     d datacur         s               d   datfmt(*iso)
009204171114     d wmese           s              2s 0
009205171114     d wgiorno         s              2s 0
009206171115     d k_key           s                   like(tblkey)
009207171117     d widlna          s                   like(vidlna)
009208171117     d wtipo           s              1
009209171115     d $Fine           s               n   inz(*off)
009210171115     d $Finw1          s               n   inz(*off)
009211171115     d comman          s            150
009212171115     d lung            s             15  5
009213171115      // ? PROTOTIPI ?
009214171115      /copy gaitrasrc/srcprotopr,tibs34r
009215171117      /copy gaitrasrc/srcprotopr,tnsdMAGr
010400941115     C*------------------------------------------------------------------------*
010500941115     C* INDICATORI
010600941115     C*------------------------------------------------------------------------*
011300941114     C*------------------------------------------------------------------------*
011400941114     C* MAIN LINE
011500941114     C*------------------------------------------------------------------------*
011801171115
011802171115     C* RICEVIMENTO PARAMETRI
011803171115     C     *ENTRY        PLIST
011804171115     C                   PARM                    KPJBA
011922171114     C*
011923171115       //?operazioni iniziali
011924171115         exsr routinz;
011925171115
011926171115       //?gestione videata
011927171115
011934171114         clear vidlna;
011935171114         clear vidrma;
011936171114         $fine = *off;
012101171114         dow  $fine = *off;
012102171116         //  *inkf = *off;
012200171116             exfmt sdutd01;
012201171115           if  vidmsg=msg(1);
012202171115               *inlr = *on;
012203171115               return;
012204171115           endif;
012205171114             clear vidmsg;
012206171114             *in28=*off;
012207171114             *in90=*off;
012208171114             select;
012209171114             when *inkc=*on;
012210171114                $fine=*on ;
012211171114             other ;
012212171114                exsr ctrd01;
012213171116         //     if not *in90 and *inkf;
012214171116                if not *in90  ;
012215171115       //?elaborazione richiesta
012216171114                   exsr elabora;
012217171115                   vidlna = 0;
012218171115                   vidrma = *blanks;
012219171114                endif;
012220171114             endsl;
012300171115         enddo;
012400171114         eval *inlr=*on;
012401171115       //-------------------------------------------------------------*
012402171115       // CONTROLLO DATI  D01                                         *
012403171115       //-------------------------------------------------------------*
012404171115       Begsr CTRD01   ;
012405171115          *in40 = *off;
012406171115          *in41 = *off;
012407171116         // Non faccio controlli finchè non sono stati immessi entrambi i campi
012408171116          if vidlna >0 and vidrma<> *blanks;
012417171117          // Deve essere un magazzino valido
012418171117          //chain vidlna azorg01l ;
012421171117          //if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A');
012422171117             clear tnsdmagds  ;
012423171117             §MAGMAG=%editc(vidlna:'X');
012424171117             %subst(§IMAGTORG:1:1)='F';
012425171117             tnsdmagr(kpjba:tnsdmagds);
012426171117          if §omagerr='E';
012427171115             vidmsg=msg(2);
012428171115             *in28 = *on;
012429171115             *in90 = *on;
012430171115             leavesr;
012431171115          endif ;
012432171117          // RMA obbligatorio deve iniziare per sped
012433171117            if %subst(vidrma:1:4)<>'SPED'  ;
012434171117               vidmsg=msg(3);
012435171117               *in28 = *on;
012436171117             *in90 = *on;
012437171117             *in41 = *on;
012438171117             leavesr;
012439171117            endif;
012440171116          else;
012441171116          // se dati mancanti accendo 90 per rimanere in videata
012442171116            *in90 = *on;
012443171116          // posizionamento del cursore sul primo campo vuoto
012444171116            if vidlna=0;
012445171116              *in40=*on;
012446171116            else ;
012447171116              *in41=*on;
012448171116            endif   ;
012449171116        endif;
012450171115       endsr;
012451171114       //-------------------------------------------------------------*
012452171114       // Elaborazione richiesta                                      *
012453171114       //-------------------------------------------------------------*
012454171114       Begsr elabora  ;
012455171114       // Reperisco la data corrente
012456171114          datacur=(%date());
012457171117       // reperisco la linea di arrivo da utilizzare
012458171117       // memorizzo se trattasi di primo o secondo livello o di logistica
012459171117          widlna=vidlna;
012460171117          clear wtipo     ;
012461171117          chain vidlna azorg01l;
012462171117          if %found(azorg01l);
012463171117            og148 = orgde8;
012465171117            if §ogfiue > *zeros;
012466171117               widlna=%dec(§ogfiue:3:0);
012467171117            endif;
012468171117            og143 = orgde3;
012469171117            if §ogntw='LOG';
012470171117               wtipo='L';
012471171117            else;
012472171117               wtipo=§oglpo;
012473171117            endif    ;
012474171117          endif;
012475171115       // Se record già presente su fivab viene richiesto se si vuole ristampare
012476171115       // l'etichetta
012477171115       EXEC SQL
012478171115        SELECT  *  into :fivabds
012479171117          FROM  FIVAB50F   WHERE  VABlna = :widlna and vabrma= :vidrma
012480171115           fetch first row only  ;
012481171115       // Trovato il record corrispondente
012482171115            if sqlcod = 0   ;
012483171117              widris=' ';
012484171116       //     *inkf = *off;
012485171115              $finw1 = *off;
012486171115              dow $finw1 = *off    ;
012487171116                exfmt sdutw01;
012488171116       //        if *inkf = *on;
012489171116                 if widris <> *blanks;
012490171115                     $finw1 = *on;
012491171115                 endif;
012492171115              enddo;
012493171115         // Stampa Etichetta segnacollo - La ristampa solo se richiesta
012494171115              if  widris = 'S';
012495171115                 exsr GESSNC   ;
012496171115              endif;
012497171115            else  ;
012498171115         // Creazione fivab50f + stampa etichetta segnacollo solo se record non presente
012499171115               exsr sr_vab50 ;
012500171115               exsr GESSNC   ;
012501171115            endif;
012502171114
012503171114       endsr;
012504171114       //-------------------------------------------------------------*
012505171114       // Scrittura fivab50F                                          *
012506171114       //-------------------------------------------------------------*
012507171114       Begsr sr_vab50 ;
012508171115        // lettura tabella VMG per recuperare dati della bolla
012509171114        CLEAR FIVAB000   ;
012510171114        vabaas=%subdt(datacur:*years);
012511171114        wmese   = %subdt(datacur:*months);
012512171114        wgiorno = %subdt(datacur:*days);
012513171114        vabmgs  = wmese * 100;
012514171114        vabmgs+= wgiorno;
012515171117        chain widlna azorg01l ;
012516171114        if %found(azorg01l);
012517171114           vabrsd=orgdes;
012518171114           vabind=orgind;
012519171115           vabcad=%editc(orgcpf:'X');
012520171114           vablod=orgloc;
012521171114           vabprd=orgpro;
012522171114        endif;
012523171115        VABCCM  = §VMGCCM ;
012524171115        VABLNP  = §VMGLNP ;
012525171115        VABFGS  = vablnp  ;
012526171115        VABNRS  = §VMGNRS ;
012527171115        VABCBO  = §VMGCBO ;
012528171115        clear ds3a;
012529171115        k_key = vabcbo;
012530171115        chain (1:'3A':k_key) tabel00f ;
012531171115        if %found(tabel00f);
012532171115          ds3a = tbluni;
012533171115        else;
012534171115          §3atb1 = 'F';
012535171115        endif;
012536171117        VABLNA  = widlna  ;
012537171115        VABTSP  = §VMGTSP ;
012538171115        VABNAS  = §VMGNAS ;
012539171115        VABNCL  = §VMGNCL ;
012540171115        VABPKB  = §VMGPKB ;
012541171115     C*  PRELEVO IL NUMERO DEL C/S
012542171115     c* lo prendo da AZNUM con trul33r
012543171115     C                   CLEAR                   TRUL33DS
012544171115     C                   Z-ADD     50            I33CNU
012545171115     C                   Z-ADD     1             I33NUM
012546171115     c                   movel(p)  trul33ds      kpjbu
012547171115     c                   call      'TRUL33R'
012548171115     c                   parm                    kpjba
012549171115     c                   movel     kpjbu         trul33ds
012550171115    3C                   IF        O33ERR = 0
012551171115     C                   Z-ADD     O33Nrf        bolla             7 0
012552171115     c                   else
012553171115     c                   clear                   bolla
012554171115     c                   endif
012555171115        VABRMN  = bolla   ;
012556171115        VABRMA  = vidrma  ;
012558171117        select ;
012559171117        when wtipo='L';
012560171117           VABNOT  = §VMGNOTL;
012561171117        when wtipo='1';
012562171117           VABNOT  = §VMGNOT1;
012563171117        when wtipo='2';
012564171117           VABNOT  = §VMGNOT2;
012565171117        endsl      ;
012566171115        VABZNC  = §vmgznc ;
012567171115        VABCTM  = §VMGCTM ;
012568171115        VABFFD  = §VMGFFD ;
012569171115
012570171114     C* NUMERO SPEDIZIONE
012571171114     c* lo prendo da AZNUM con trul33r
012572171114     C                   CLEAR                   TRUL33DS
012573171114     C                   Z-ADD     103           I33CNU
012574171114     C                   Z-ADD     vablnp        I33PO1
012575171114     C                   Z-ADD     1             I33NUM
012576171114     c                   movel(p)  trul33ds      kpjbu
012577171114     c                   call      'TRUL33R'
012578171114     c                   parm                    kpjba
012579171114     c                   movel     kpjbu         trul33ds
012580171114     C                   IF        O33ERR = 0
012581171114     C                   Z-ADD     O33Nrf        vabnsp
012582171114     C                   ELSE
012583171114     C                   CLEAR                   VABNSP
012584171114     C                   ENDIF
012585171114     C
012586171114     C* RICERCO IL NUMERO SEGNACOLLI E LANCIO IL PROGRAMMA STAMPA
012587171114     C* SEGNACOLLI
012588171114     c* lo prendo da AZNUM con trul33r
012589171114     C                   CLEAR                   TRUL33DS
012590171114     C                   Z-ADD     109           I33CNU
012591171114     C                   Z-ADD     vablnp        I33PO1
012592171114     C                   Z-ADD     vabncl        I33NUM
012593171114     c                   movel(p)  trul33ds      kpjbu
012594171114     c                   call      'TRUL33R'
012595171114     c                   parm                    kpjba
012596171114     c                   movel     kpjbu         trul33ds
012597171114     C                   IF        O33ERR<> 0
012598171114     C*
012599171114     C                   Z-ADD     0             VABNCD
012600171114     C                   Z-ADD     0             VABNCA
012601171114     C                   ELSE
012602171114     c                   z-add     o33nri        vabncd
012603171114     c                   z-add     o33nrf        vabnca
012604171114     C                   END
012605171114     C*
012606171114     C* SE AL = DAL AZZERO IL CAMPO
012607171114     C     VABNCA        IFEQ      VABNCD
012608171114     C                   Z-ADD     0             VABNCA
012609171114     C                   END
012610171115        exsr chkcap;
012611171115        VABCTS  = o95cts  ;
012612171115
012613171115        // FIVAT per alert  di affidamento spedizione
012614171115         clear fivat000 ;
012615171115         VATFGS = vabfgs;
012616171115         VATCCM = vabccm;
012617171115         VATLNP = vablnp;
012618171115         VATAAS = vabaas;
012619171115         VATNRS = vabnrs;
012620171115         VATNSP = vabnsp;
012621171115         VATTRC = 'I';
012622171117       //  VATNOT = %trim(§vmgmai) + '@BRT.it' ;
012623171117         VATNOT = §vmgmai ;
012624171115         write  fivat000      ;
012625171115
012626171114         write fivab000;
012627171114       endsr ;
012628171115     C*------------------------------------------------------------------------*
012629171115     C*  CHKCAP - CONTROLLO FORMALE DEL CAP
012630171115     C*------------------------------------------------------------------------*
012631171115     C     CHKCAP        BEGSR
012632171115     C*
012633171115     C* IMPOSTO CAMPI DS EST. E LANCIO PGM PER CONTROLLO
012634171115     C                   CLEAR                   tisi95ds
012635171115     C                   CLEAR                   fnlv13ds
012636171115     C* CONTROLLO: 1-FORMALE  2-SU CAPPARIO 3-CONGRUENZA CON PROVINC
012637171115     C                   MOVEL     '3'           I95TCN                                     LLO FORM
012638171115     C                   MOVEL     VABCAD        I95CAP                         *CAP
012639171115     C                   MOVEL     VABPRD        I95PRV
012640171115     C                   MOVEL     VABLOD        I95LOC
012641171115     C                   Z-ADD     vabpkb        I95LKG
012642171115     C                   Z-ADD     vabvlb        I95LMC
012643171115     C                   MOVEL     VABFFD        I95FFD
012644171115     C                   Z-ADD     VABMGS        I95DAT
012645171115     C                   MOVEL     VABAAS        I95DAT
012646171115     C                   MOVEL     'S'           I13AF0
012647171115     C                   MOVEL     'S'           I13CNG
012648171115     C     §3AFCA        IFNE      0
012649171115     C                   MOVEL     'S'           I95FCA
012650171115     C                   ENDIF
012651171115     C                   MOVEL     §3ATB1        I95TPO
012652171115     C                   MOVEL     'S'           I95FRE
012653171115     C                   MOVEL     VABTSP        I95TSP
012654171115     C**
012655171115     C**  TERMINAL DI PARTENZA DELLA LNP
012656171115     C                   CLEAR                   DSLV55
012657171115     C                   MOVEL     'P'           D55TPT
012658171115     C                   MOVE      VABLNP        D55LIN
012659171115     C                   MOVE      VABMGS        D55DRF
012660171115     C                   MOVEL     VABAAS        D55DRF
012661171115     C                   CALL      'FNLV55R'
012662171115     C                   PARM                    DSLV55
012663171115     C**
012664171115     C                   MOVEL     D55TFP        I95TFP
012665171115     C*
012666171115     C                   CALL      'FNLV13R'
012667171115     C                   PARM                    KPJBA
012668171115     C                   PARM                    fnlv13ds
012669171115     C                   PARM                    tisi95ds
012670171115     C*
012671171115     C                   ENDSR
012672171115     C*---------------------------------------------------------------
012673171115     C*   GESSNC  GESTIONE STAMPA SEGNACOLLI
012674171115     C*---------------------------------------------------------------
012675171115     C*
012676171115     C     GESSNC        BEGSR
012677171115     C*
012678171115     C                   Z-ADD     VABNCD        CO2SCD            7 0
012679171115     C     VABNCA        IFEQ      0
012680171115     C                   Z-ADD     VABNCD        VABNCA
012681171115     C                   END
012682171115     C*
012683171115     C* VALORIZZO LA DS PARAMETRO PASSANTE PER PGM STAMPA SEGNACOLLI
012684171115     C*
012685171115     C                   Z-ADD     0             §LDNC1
012686171115     C                   MOVEL     VABLNP        §LDLNP
012687171115     C     VABLNP        CHAIN     AZORG                              56
012688171115    1C     *IN56         IFEQ      *OFF
012689171115    2C     ORGDE5        IFNE      *BLANKS
012690171115     C                   MOVEL     ORGDE5        §LDDLP
012691171115   X2C                   ELSE
012692171115     C                   MOVEL     ORGDES        §LDDLP
012693171115    2C                   END
012694171115   X1C                   ELSE
012695171115     C                   MOVEL     *BLANKS       §LDDLP
012696171115    1C                   END
012697171115     C                   MOVEL     *BLANK        §LDCDP
012698171115     C                   MOVEL     VABNRS        §LDNRS
012699171115     C                   MOVEL     VABLNA        §LDLNA
012700171115     C     VABLNA        CHAIN     AZORG                              56
012701171115     C   56              MOVEL     *BLANKS       §LDDLA
012702171115    1C  N56ORGDE5        IFNE      *BLANKS
012703171115     C                   MOVEL     ORGDE5        §LDDLA
012704171115     C                   ELSE
012705171115     C                   MOVEL     ORGDES        §LDDLA
012706171115    1C                   END
012707171115     C                   CLEAR                   DSLV55
012708171115     C                   MOVEL     'A'           D55TPT
012709171115     C                   MOVE      VABLNA        D55LIN
012710171115     C                   MOVE      VABMGS        D55DRF
012711171115     C                   MOVEL     VABAAS        D55DRF
012712171115     C                   CALL      'FNLV55R'
012713171115     C                   PARM                    DSLV55
012714171115     C                   MOVEL     D55TFA        §LDTNA
012715171115     C                   MOVEL     VABTSP        §LDTSP
012716171115     C                   MOVEL     VABZNC        §LDZSC
012717171115     C                   MOVE      VABNCL        §LDNC2
012718171115     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
012719171115     C                   CLEAR                   DSBS69
012720171115     C                   Z-ADD     VABCCM        I69KAC
012721171115     C                   Z-ADD     VABCCM        I69KIN
012722171115     C                   MOVEL     KNSIF         I69SIF
012723171115     C                   CALL      'TIBS69R'
012724171115     C                   PARM                    DSBS69
012725171115     C                   PARM                    DSACO
012726171115     C                   PARM                    DSIND
012727171115     C                   PARM                    DSCLP
012728171115     C                   PARM                    DSCLS
012729171115     C     O69ERR        IFNE      '1'
012730171115     C     ACOFLG        IFEQ      '*'
012731171115     C                   MOVEL     *BLANKS       §LDRSM
012732171115     C                   ELSE
012733171115     C                   MOVEL     ACORAG        §LDRSM
012734171115     C                   END
012735171115     C                   MOVEL     INDPRV        §LDPRM
012736171115     C                   END
012737171115     C                   MOVEL     VABRSD        §LDRSD
012738171115     C                   MOVEL     VABCAD        §LDCAD
012739171115     C                   MOVEL     VABLOD        §LDLOD
012740171115     C                   MOVEL     VABIND        §LDIND
012741171115     C                   MOVEL     VABPRD        §LDPRD
012742171115     C                   MOVEL     999           §LDCPD
012743171115     C                   MOVEL     *ZEROS        §LDNCP
012744171115     C                   MOVEL     *ZEROS        §LDNCU
012745171115     C     VABVLB        IFGT      0
012746171115     C                   MOVEL     'R'           §LDFVR
012747171115     C                   ELSE
012748171115     C                   CLEAR                   §LDFVR
012749171115     C                   ENDIF
012750171115     C                   Z-ADD     VABVLB        §LDVOL
012751171115     C                   Z-ADD     VABPKB        §LDPKG
012752171115     C                   Z-ADD     VABAAS        §LDAAS
012753171115     C                   Z-ADD     VABMGS        §LDMGS
012754171115     C                   MOVEL     'ITA'         §DETST
012755171117         open  fnlv22p;
012756171115     C     CO2SCD        DOWLE     VABNCA
012757171115     C                   ADD       1             §LDNC1
012758171115     C                   Z-ADD     CO2SCD        §LDNCD
012759171115     C*
012760171115     c                   Call      d90pss
012761171115     C                   PARM                    DSBLD
012762171115     C                   PARM                    DSBLDE
012763171115     C*
012764171115     C                   ADD       1             CO2SCD
012765171115     C*
012766171115     C                   END
012767171117         close fnlv22p;
012768171115     C*
012769171115     C                   ENDSR
012770171115       //-------------------------------------------------------------*
012771171115       //Routine iniziale                                             *
012772171115       //-------------------------------------------------------------*
012773171115       Begsr routinz;
012774171115
012775171115       //?Dati utente
012776171115          in(e) §azute;
012777171115          if not %error;
012778171115           in(e) §datiute;
012779171115          endif;
012780171115          if %error or rsut = *blanks;
012781171115           tibs34r(tibs34ds);
012782171115           in §azute;
012783171115           in §datiute;
012784171115          endif;
012785171115
012786171115     C* RICHIESTA STAMPANTI
012787171115     C                   CLEAR                   trul90ds
012788171115     C*
012789171115     C                   MOVEL     'S'           D90RSE
012790171115     C                   CALL      'TRUL90R'
012791171115     C                   PARM                    KPJBA
012792171115     C                   PARM                    trul90ds
012793171115     C     D90F3         IFne      '1'
012794171115     C* OVRPRTF SEGNACOLLI
012795171115     C                   MOVEA     D90PRE        CM3(28)
012796171115     C                   MOVEA     D90MDE        CM3(50)
012797171115     C*
012798171115     C                   Z-ADD     103           LUNG
012799171115     C                   CLEAR                   COMMAN
012800171115     C                   MOVEA     CM3           COMMAN
012801171115     C                   CALL      'QCMDEXC'                            91
012802171115     C                   PARM                    COMMAN
012803171115     C                   PARM                    LUNG
012804171115     c                   if        *in91
012805171115     c                   eval      *inlr = *on
012806171115     c                   return
012807171115     c                   endif
012808171115     c                   else
012809171115     c                   eval      *inlr = *on
012810171115     c                   return
012811171115     c                   endif
012812171115
012813171115        // lettura tabella VMG per recuperare dati della bolla
012814171115        chain ('VMG':'TLC') tntbe01l;
012815171115        if not %found(tntbe01l);
012816171115           vidmsg=msg(1) ;
012817171115           *in28 = *on;
012818171115        else;
012819171115           dvmg=tbeuni;
012820171115        endif;
012821171115
012822171115         endsr;
012823171114**  cm3
012824171114OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
012825171114 USRDTA('Segnacolli')
012826171115**  msg
012900171115Non presenti dati per la creazione bolla - Tabella "VMG"
013000171116Linea di arrivo errata
013100171117Rif.Mitt. Alfabetico errato
