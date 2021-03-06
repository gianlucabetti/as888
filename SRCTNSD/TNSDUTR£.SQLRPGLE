000100020322     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020322      *------------------------------------------------------------------------*
000300171114      * Creaz. bolle + stampa etichetta segnacollo per invio materiale IT ai P.O
000400020322      *------------------------------------------------------------------------*
000700020528     fazorg01l  if   e           k disk
000701171114     ftntbe01l  iF   e           k disk
000702171115     ftabel00f  iF   e           k disk
000703171116     ftnsdutd1  CF   E             workstn
001600171114     FFIVAB50F  O  A E             DISK
001601171115     FFIVAT50F  O  A E             DISK
001800171115     FFNLV22P   O    F  132        PRINTER usropn
003100020528
003201171114      * DS PER TRUL90DS - RICHIESTA STAMPANTI
003202171114     d trul90ds      e ds
003300131118     d
003400941114     D KPJBA         E DS
006700020920
006701171115     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006702171115     D fivabds       E DS                  EXTNAME(fivab50f)
006703171115     d trul33ds      e ds
007300171115     D tisi95ds      E DS
007400171115     D fnlv13ds      E DS
007401171115     d Tibs34ds      e ds                  inz
007402171115     d �azute        e ds                  Extname(Azute00f)
007403171115     d                                     dtaara
007404171115     d �Datiute      e ds                  EXTname(ddatiute)
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
009102171115     d dvmg          E DS
009103171115     d ds3a          E DS
009200120322     d
009201171114     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
009202171115     D msg             S             78    DIM(5) CTDATA PERRCD(1)
009203171114     d datacur         s               d   datfmt(*iso)
009204171114     d wmese           s              2s 0
009205171114     d wgiorno         s              2s 0
009206171115     d k_key           s                   like(tblkey)
009207171115     d $Fine           s               n   inz(*off)
009208171115     d $Finw1          s               n   inz(*off)
009209171115     d comman          s            150
009210171115     d lung            s             15  5
009211171115      // ? PROTOTIPI ?
009212171115      /copy gaitrasrc/srcprotopr,tibs34r
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
012102171115             *inkf = *off;
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
012213171115                if not *in90 and *inkf;
012214171115       //?elaborazione richiesta
012215171114                   exsr elabora;
012216171115                   vidlna = 0;
012217171115                   vidrma = *blanks;
012218171114                endif;
012219171114             endsl;
012300171115         enddo;
012400171114         eval *inlr=*on;
012401171115       //-------------------------------------------------------------*
012402171115       // CONTROLLO DATI  D01                                         *
012403171115       //-------------------------------------------------------------*
012404171115       Begsr CTRD01   ;
012405171115          *in40 = *off;
012406171115          *in41 = *off;
012407171116          if vidlna >0 and vidrma<> *blanks;
012408171115          // Linea di arrivo  obbligatoria
012409171116          //if vidlna = 0 ;
012410171116          //   vidmsg=msg(2);
012411171116          //   *in28 = *on;
012412171116          //   *in90 = *on;
012413171116          //   *in40 = *on;
012414171116          //   leavesr;
012415171116          //endif;
012416171115          // Deve essere una filiale valida
012417171115          chain vidlna azorg01l ;
012418171115          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A');
012419171115             vidmsg=msg(2);
012420171115             *in28 = *on;
012421171115             *in90 = *on;
012422171115             leavesr;
012423171115          endif ;
012424171115          // RMA obbligatorio
012425171116          //if vidrma = *blanks;
012426171116          //   vidmsg=msg(3);
012427171116          //   *in28 = *on;
012428171116          // *in90 = *on;
012429171116          // *in41 = *on;
012430171116          // leavesr;
012431171116        //endif;
012432171116        endif;
012433171115       endsr;
012434171114       //-------------------------------------------------------------*
012435171114       // Elaborazione richiesta                                      *
012436171114       //-------------------------------------------------------------*
012437171114       Begsr elabora  ;
012438171114       // Reperisco la data corrente
012439171114          datacur=(%date());
012440171115       // Se record gi� presente su fivab viene richiesto se si vuole ristampare
012441171115       // l'etichetta
012442171115       EXEC SQL
012443171115        SELECT  *  into :fivabds
012444171115          FROM  FIVAB50F   WHERE  VABlna = :vidlna and vabrma= :vidrma
012445171115           fetch first row only  ;
012446171115       // Trovato il record corrispondente
012447171115            if sqlcod = 0   ;
012448171115              *inkf = *off;
012449171115              $finw1 = *off;
012450171115              dow $finw1 = *off    ;
012451171116                exfmt sdutw01;
012452171115                 if *inkf = *on;
012453171115                     $finw1 = *on;
012454171115                 endif;
012455171115              enddo;
012456171115         // Stampa Etichetta segnacollo - La ristampa solo se richiesta
012457171115              if  widris = 'S';
012458171115                 exsr GESSNC   ;
012459171115              endif;
012460171115            else  ;
012461171115         // Creazione fivab50f + stampa etichetta segnacollo solo se record non presente
012462171115               exsr sr_vab50 ;
012463171115               exsr GESSNC   ;
012464171115            endif;
012465171114
012466171114       endsr;
012467171114       //-------------------------------------------------------------*
012468171114       // Scrittura fivab50F                                          *
012469171114       //-------------------------------------------------------------*
012470171114       Begsr sr_vab50 ;
012471171115        // lettura tabella VMG per recuperare dati della bolla
012472171114        CLEAR FIVAB000   ;
012473171114        vabaas=%subdt(datacur:*years);
012474171114        wmese   = %subdt(datacur:*months);
012475171114        wgiorno = %subdt(datacur:*days);
012476171114        vabmgs  = wmese * 100;
012477171114        vabmgs+= wgiorno;
012478171114        chain vidlna azorg01l ;
012479171114        if %found(azorg01l);
012480171114           vabrsd=orgdes;
012481171114           vabind=orgind;
012482171115           vabcad=%editc(orgcpf:'X');
012483171114           vablod=orgloc;
012484171114           vabprd=orgpro;
012485171114        endif;
012486171115        VABCCM  = �VMGCCM ;
012487171115        VABLNP  = �VMGLNP ;
012488171115        VABFGS  = vablnp  ;
012489171115        VABNRS  = �VMGNRS ;
012490171115        VABCBO  = �VMGCBO ;
012491171115        clear ds3a;
012492171115        k_key = vabcbo;
012493171115        chain (1:'3A':k_key) tabel00f ;
012494171115        if %found(tabel00f);
012495171115          ds3a = tbluni;
012496171115        else;
012497171115          �3atb1 = 'F';
012498171115        endif;
012499171115        VABLNA  = vidlna  ;
012500171115        VABTSP  = �VMGTSP ;
012501171115        VABNAS  = �VMGNAS ;
012502171115        VABNCL  = �VMGNCL ;
012503171115        VABPKB  = �VMGPKB ;
012504171115     C*  PRELEVO IL NUMERO DEL C/S
012505171115     c* lo prendo da AZNUM con trul33r
012506171115     C                   CLEAR                   TRUL33DS
012507171115     C                   Z-ADD     50            I33CNU
012508171115     C                   Z-ADD     1             I33NUM
012509171115     c                   movel(p)  trul33ds      kpjbu
012510171115     c                   call      'TRUL33R'
012511171115     c                   parm                    kpjba
012512171115     c                   movel     kpjbu         trul33ds
012513171115    3C                   IF        O33ERR = 0
012514171115     C                   Z-ADD     O33Nrf        bolla             7 0
012515171115     c                   else
012516171115     c                   clear                   bolla
012517171115     c                   endif
012518171115        VABRMN  = bolla   ;
012519171115        VABRMA  = vidrma  ;
012520171115        VABNOT  = �VMGNOT ;
012521171115        VABZNC  = �vmgznc ;
012522171115        VABCTM  = �VMGCTM ;
012523171115        VABFFD  = �VMGFFD ;
012524171115
012525171114     C* NUMERO SPEDIZIONE
012526171114     c* lo prendo da AZNUM con trul33r
012527171114     C                   CLEAR                   TRUL33DS
012528171114     C                   Z-ADD     103           I33CNU
012529171114     C                   Z-ADD     vablnp        I33PO1
012530171114     C                   Z-ADD     1             I33NUM
012531171114     c                   movel(p)  trul33ds      kpjbu
012532171114     c                   call      'TRUL33R'
012533171114     c                   parm                    kpjba
012534171114     c                   movel     kpjbu         trul33ds
012535171114     C                   IF        O33ERR = 0
012536171114     C                   Z-ADD     O33Nrf        vabnsp
012537171114     C                   ELSE
012538171114     C                   CLEAR                   VABNSP
012539171114     C                   ENDIF
012540171114     C
012541171114     C* RICERCO IL NUMERO SEGNACOLLI E LANCIO IL PROGRAMMA STAMPA
012542171114     C* SEGNACOLLI
012543171114     c* lo prendo da AZNUM con trul33r
012544171114     C                   CLEAR                   TRUL33DS
012545171114     C                   Z-ADD     109           I33CNU
012546171114     C                   Z-ADD     vablnp        I33PO1
012547171114     C                   Z-ADD     vabncl        I33NUM
012548171114     c                   movel(p)  trul33ds      kpjbu
012549171114     c                   call      'TRUL33R'
012550171114     c                   parm                    kpjba
012551171114     c                   movel     kpjbu         trul33ds
012552171114     C                   IF        O33ERR<> 0
012553171114     C*
012554171114     C                   Z-ADD     0             VABNCD
012555171114     C                   Z-ADD     0             VABNCA
012556171114     C                   ELSE
012557171114     c                   z-add     o33nri        vabncd
012558171114     c                   z-add     o33nrf        vabnca
012559171114     C                   END
012560171114     C*
012561171114     C* SE AL = DAL AZZERO IL CAMPO
012562171114     C     VABNCA        IFEQ      VABNCD
012563171114     C                   Z-ADD     0             VABNCA
012564171114     C                   END
012565171115        exsr chkcap;
012566171115        VABCTS  = o95cts  ;
012567171115
012568171115        // FIVAT per alert  di affidamento spedizione
012569171115         clear fivat000 ;
012570171115         VATFGS = vabfgs;
012571171115         VATCCM = vabccm;
012572171115         VATLNP = vablnp;
012573171115         VATAAS = vabaas;
012574171115         VATNRS = vabnrs;
012575171115         VATNSP = vabnsp;
012576171115         VATTRC = 'I';
012577171115         VATNOT = %trim(�vmgmai) + '@BRT.it' ;
012578171115         write  fivat000      ;
012579171115
012580171114         write fivab000;
012581171114       endsr ;
012582171115     C*------------------------------------------------------------------------*
012583171115     C*  CHKCAP - CONTROLLO FORMALE DEL CAP
012584171115     C*------------------------------------------------------------------------*
012585171115     C     CHKCAP        BEGSR
012586171115     C*
012587171115     C* IMPOSTO CAMPI DS EST. E LANCIO PGM PER CONTROLLO
012588171115     C                   CLEAR                   tisi95ds
012589171115     C                   CLEAR                   fnlv13ds
012590171115     C* CONTROLLO: 1-FORMALE  2-SU CAPPARIO 3-CONGRUENZA CON PROVINC
012591171115     C                   MOVEL     '3'           I95TCN                                     LLO FORM
012592171115     C                   MOVEL     VABCAD        I95CAP                         *CAP
012593171115     C                   MOVEL     VABPRD        I95PRV
012594171115     C                   MOVEL     VABLOD        I95LOC
012595171115     C                   Z-ADD     vabpkb        I95LKG
012596171115     C                   Z-ADD     vabvlb        I95LMC
012597171115     C                   MOVEL     VABFFD        I95FFD
012598171115     C                   Z-ADD     VABMGS        I95DAT
012599171115     C                   MOVEL     VABAAS        I95DAT
012600171115     C                   MOVEL     'S'           I13AF0
012601171115     C                   MOVEL     'S'           I13CNG
012602171115     C     �3AFCA        IFNE      0
012603171115     C                   MOVEL     'S'           I95FCA
012604171115     C                   ENDIF
012605171115     C                   MOVEL     �3ATB1        I95TPO
012606171115     C                   MOVEL     'S'           I95FRE
012607171115     C                   MOVEL     VABTSP        I95TSP
012608171115     C**
012609171115     C**  TERMINAL DI PARTENZA DELLA LNP
012610171115     C                   CLEAR                   DSLV55
012611171115     C                   MOVEL     'P'           D55TPT
012612171115     C                   MOVE      VABLNP        D55LIN
012613171115     C                   MOVE      VABMGS        D55DRF
012614171115     C                   MOVEL     VABAAS        D55DRF
012615171115     C                   CALL      'FNLV55R'
012616171115     C                   PARM                    DSLV55
012617171115     C**
012618171115     C                   MOVEL     D55TFP        I95TFP
012619171115     C*
012620171115     C                   CALL      'FNLV13R'
012621171115     C                   PARM                    KPJBA
012622171115     C                   PARM                    fnlv13ds
012623171115     C                   PARM                    tisi95ds
012624171115     C*
012625171115     C                   ENDSR
012626171115     C*---------------------------------------------------------------
012627171115     C*   GESSNC  GESTIONE STAMPA SEGNACOLLI
012628171115     C*---------------------------------------------------------------
012629171115     C*
012630171115     C     GESSNC        BEGSR
012631171115     C*
012632171115     C                   Z-ADD     VABNCD        CO2SCD            7 0
012633171115     C     VABNCA        IFEQ      0
012634171115     C                   Z-ADD     VABNCD        VABNCA
012635171115     C                   END
012636171115     C*
012637171115     C* VALORIZZO LA DS PARAMETRO PASSANTE PER PGM STAMPA SEGNACOLLI
012638171115     C*
012639171115     C                   Z-ADD     0             �LDNC1
012640171115     C                   MOVEL     VABLNP        �LDLNP
012641171115     C     VABLNP        CHAIN     AZORG                              56
012642171115    1C     *IN56         IFEQ      *OFF
012643171115    2C     ORGDE5        IFNE      *BLANKS
012644171115     C                   MOVEL     ORGDE5        �LDDLP
012645171115   X2C                   ELSE
012646171115     C                   MOVEL     ORGDES        �LDDLP
012647171115    2C                   END
012648171115   X1C                   ELSE
012649171115     C                   MOVEL     *BLANKS       �LDDLP
012650171115    1C                   END
012651171115     C                   MOVEL     *BLANK        �LDCDP
012652171115     C                   MOVEL     VABNRS        �LDNRS
012653171115     C                   MOVEL     VABLNA        �LDLNA
012654171115     C     VABLNA        CHAIN     AZORG                              56
012655171115     C   56              MOVEL     *BLANKS       �LDDLA
012656171115    1C  N56ORGDE5        IFNE      *BLANKS
012657171115     C                   MOVEL     ORGDE5        �LDDLA
012658171115     C                   ELSE
012659171115     C                   MOVEL     ORGDES        �LDDLA
012660171115    1C                   END
012661171115     C                   CLEAR                   DSLV55
012662171115     C                   MOVEL     'A'           D55TPT
012663171115     C                   MOVE      VABLNA        D55LIN
012664171115     C                   MOVE      VABMGS        D55DRF
012665171115     C                   MOVEL     VABAAS        D55DRF
012666171115     C                   CALL      'FNLV55R'
012667171115     C                   PARM                    DSLV55
012668171115     C                   MOVEL     D55TFA        �LDTNA
012669171115     C                   MOVEL     VABTSP        �LDTSP
012670171115     C                   MOVEL     VABZNC        �LDZSC
012671171115     C                   MOVE      VABNCL        �LDNC2
012672171115     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
012673171115     C                   CLEAR                   DSBS69
012674171115     C                   Z-ADD     VABCCM        I69KAC
012675171115     C                   Z-ADD     VABCCM        I69KIN
012676171115     C                   MOVEL     KNSIF         I69SIF
012677171115     C                   CALL      'TIBS69R'
012678171115     C                   PARM                    DSBS69
012679171115     C                   PARM                    DSACO
012680171115     C                   PARM                    DSIND
012681171115     C                   PARM                    DSCLP
012682171115     C                   PARM                    DSCLS
012683171115     C     O69ERR        IFNE      '1'
012684171115     C     ACOFLG        IFEQ      '*'
012685171115     C                   MOVEL     *BLANKS       �LDRSM
012686171115     C                   ELSE
012687171115     C                   MOVEL     ACORAG        �LDRSM
012688171115     C                   END
012689171115     C                   MOVEL     INDPRV        �LDPRM
012690171115     C                   END
012691171115     C                   MOVEL     VABRSD        �LDRSD
012692171115     C                   MOVEL     VABCAD        �LDCAD
012693171115     C                   MOVEL     VABLOD        �LDLOD
012694171115     C                   MOVEL     VABIND        �LDIND
012695171115     C                   MOVEL     VABPRD        �LDPRD
012696171115     C                   MOVEL     999           �LDCPD
012697171115     C                   MOVEL     *ZEROS        �LDNCP
012698171115     C                   MOVEL     *ZEROS        �LDNCU
012699171115     C     VABVLB        IFGT      0
012700171115     C                   MOVEL     'R'           �LDFVR
012701171115     C                   ELSE
012702171115     C                   CLEAR                   �LDFVR
012703171115     C                   ENDIF
012704171115     C                   Z-ADD     VABVLB        �LDVOL
012705171115     C                   Z-ADD     VABPKB        �LDPKG
012706171115     C                   Z-ADD     VABAAS        �LDAAS
012707171115     C                   Z-ADD     VABMGS        �LDMGS
012708171115     C                   MOVEL     'ITA'         �DETST
012709171115     C     CO2SCD        DOWLE     VABNCA
012710171115     C                   ADD       1             �LDNC1
012711171115     C                   Z-ADD     CO2SCD        �LDNCD
012712171115     C*
012713171115     c                   Call      d90pss
012714171115     C                   PARM                    DSBLD
012715171115     C                   PARM                    DSBLDE
012716171115     C*
012717171115     C                   ADD       1             CO2SCD
012718171115     C*
012719171115     C                   END
012720171115     C*
012721171115     C                   ENDSR
012722171115       //-------------------------------------------------------------*
012723171115       //Routine iniziale                                             *
012724171115       //-------------------------------------------------------------*
012725171115       Begsr routinz;
012728171115
012730171115       //?Dati utente
012731171115          in(e) �azute;
012732171115          if not %error;
012733171115           in(e) �datiute;
012734171115          endif;
012735171115          if %error or rsut = *blanks;
012736171115           tibs34r(tibs34ds);
012737171115           in �azute;
012738171115           in �datiute;
012739171115          endif;
012740171115
012741171115     C* RICHIESTA STAMPANTI
012742171115     C                   CLEAR                   trul90ds
012743171115     C*
012744171115     C                   MOVEL     'S'           D90RSE
012745171115     C                   CALL      'TRUL90R'
012746171115     C                   PARM                    KPJBA
012747171115     C                   PARM                    trul90ds
012748171115     C     D90F3         IFne      '1'
012749171115     C* OVRPRTF SEGNACOLLI
012750171115     C                   MOVEA     D90PRE        CM3(28)
012751171115     C                   MOVEA     D90MDE        CM3(50)
012752171115     C*
012753171115     C                   Z-ADD     103           LUNG
012754171115     C                   CLEAR                   COMMAN
012755171115     C                   MOVEA     CM3           COMMAN
012756171115     C                   CALL      'QCMDEXC'                            91
012757171115     C                   PARM                    COMMAN
012758171115     C                   PARM                    LUNG
012759171115     c                   if        *in91
012760171115     c                   eval      *inlr = *on
012761171115     c                   return
012762171115     c                   endif
012763171115     c                   else
012764171115     c                   eval      *inlr = *on
012765171115     c                   return
012766171115     c                   endif
012767171115         open  fnlv22p;
012768171115
012769171115        // lettura tabella VMG per recuperare dati della bolla
012770171115        chain ('VMG':'TLC') tntbe01l;
012771171115        if not %found(tntbe01l);
012772171115           vidmsg=msg(1) ;
012773171115           *in28 = *on;
012774171115        else;
012775171115           dvmg=tbeuni;
012776171115        endif;
012777171115
012778171115         endsr;
012779171114**  cm3
012780171114OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
012781171114 USRDTA('Segnacolli')
012800171115**  msg
012900171115Non presenti dati per la creazione bolla - Tabella "VMG"
013000171116Linea di arrivo errata
013100171115Rif.Mitt. Alfabetico mancante
