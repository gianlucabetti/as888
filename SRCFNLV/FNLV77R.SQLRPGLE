000002110218      *PARMS CLOSQLCSR(*ENDMOD)
000003110218      *PARMS DYNUSRPRF(*OWNER)
000100000713     H DECEDIT('0,') DATEDIT(*YMD) DATFMT(*ISO)
000200110217      * FNLV77R ******************************************************
000300110217      *         CONTROLLO CAP spedizioni clienti         - BATCH -   *
000400000711      *         -------------------------                ---------   *
000500000704      ****************************************************************
000501110225     ftitas33c  if   e           k disk    usropn
000502110225     F                                     extfile(wlibsede)
000503110225     ftita430c  if   e           k disk    usropn
000504110225     F                                     extfile(wlibsede)
000505110217     fwfeib00f  o  a e             disk
000506110308
000507110309     D ERR             S             78    DIM(11) CTDATA PERRCD(1)
000508110309     d erc             s              2    dim(11) alt(err)
005000000713      *  DS PER RICHIAMO FNLV13R (CONTROLLO CAP)
005100000713     D DSSI95        E DS                  EXTNAME(TISI95DS)
005200000713     D DSLV13        E DS                  EXTNAME(FNLV13DS)
005201110217      *
005202110217     D fnlv76ds      E DS
005203110218     d tibs10ds      e ds
005204110218     d skc                           11    DIM(500) overlay(d10skc)
005205110218     d TIBS69DS      E DS                  INZ
005206110218     d DS_cnaco      E DS                  extname(CNACO00F)
005207110218     d DS_cnind      E DS                  extname(CNIND00F)
005208110218     d DS_cnclp      E DS                  extname(CNCLP00F)
005209110218     d DS_fncls      E DS                  extname(FNCLS00F)
005210110217      *
005300000713      *  DS KPJBA
005400000711     D KPJBA         E DS
007600100429     d
007601110218     D DTA4A         E DS
007602110218     D KTRC            S                   LIKE(TA4trc)
007603110217     d k_cli           s                   like(tasccm)
007604110217     d k_aas           s                   Like(tasaas)
007605110217     d k_mgs           s                   Like(tasmgs)
007606110713     d w_o95lia        s                   Like(o95lia)
007607110309     d n               s              2  0
007608110225     D Wlibsede        S             21
007609110225     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS33C')
007610110225     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS33C')
007611110225     D WlibsedeP4      S             21    inz('GAITRAGRPS/TITA430C')
007612110225     D WlibsedeB4      S             21    INZ('GAITRAGRU /TITA430C')
009500000705      ****************************************************************
009600000705      * RIEPILOGO INDICATORI:                                        *
009700000705      ****************************************************************
012000000704      *
012100000704     C     *ENTRY        PLIST
012200000704     C                   PARM                    KPJBA
012300110217     C                   MOVEL     KPJBU         fnlv76ds
012301110217      *
012302110217     c     kt33          klist
012303110217     c                   kfld                    k_cli
012304110217     c                   kfld                    k_aas
012305110217     c                   kfld                    k_mgs
012306110218     C     ktastrc       KLIST
012307110218     C                   KFLD                    tasAAS
012308110218     C                   KFLD                    tasLNP
012309110218     C                   KFLD                    tasNRS
012310110218     C                   KFLD                    tasNSP
012311110218     C                   KFLD                    Ktrc
024100020417
024101110225     c* pulisco file di work
024102110225     C/EXEC SQL
024103110225     C+ DELETE FROM WFEIB00F
024104110225     C/END-EXEC
024106110225     c                   If        %subst(knsif:7:1) = 'P'
024107110225     c                   eval      wlibsede=wlibsedep
024108110225     c                   else
024109110225     c                   eval      wlibsede=wlibsedeb
024110110225     c                   endif
024111110225     c                   open      titas33c
024112110225     c                   If        %subst(knsif:7:1) = 'P'
024113110225     c                   eval      wlibsede=wlibsedep4
024114110225     c                   else
024115110225     c                   eval      wlibsede=wlibsedeb4
024116110225     c                   endif
024117110225     c                   open      tita430c
024118110218     c* cliente da elaborare: singolo o unificante
024119110221     c                   if        lv76kscu='S'
024120110217     c                   z-add     lv76ksc       k_cli
024121110218     c                   exsr      sr_elabora
024122110218     c                   else
024123110218     c* verifico se codice è figlio
024124110218     c                   clear                   tibs10ds
024125110218     c                   eval      d10tle='ST'
024126110218     c                   eval      d10paf='P'
024127110218     c                   eval      d10cod=lv76ksc
024128110218     c                   call      'TIBS10R'
024129110218     c                   parm                    tibs10ds
024130110218     c                   if        d10cop=0
024131110218     c* se non è figlio verifico se è padre
024132110218     c                   clear                   tibs10ds
024133110218     c                   eval      d10tle='ST'
024134110218     c                   eval      d10paf='F'
024135110218     c                   eval      d10cod=lv76ksc
024136110218     c                   call      'TIBS10R'
024137110218     c                   parm                    tibs10ds
024138110218     c                   endif
024139110218     c                   if        d10cop>0
024140110218     C     1             DO        500           H                 3 0
024141110218     C     SKC(H)        IFeq      *ZEROS
024142110218     c                   leave
024143110218     c                   endif
024144110218     c                   move      skc(H)        k_cli
024145110218     c                   exsr      sr_elabora
024146110218     c                   enddo
024147110218     c                   else
024148110223     c* se non è padre e non è figlio è un cliente singolo
024149110218     c                   z-add     lv76ksc       k_cli
024150110223     c                   exsr      sr_elabora
024151110218     c                   endif
024152110218     c                   endif
024153110218      *
024200000712      *
024201110217     c* chiudo pgm
024202110217     c                   clear                   dslv13
024203110217     c                   clear                   dssi95
024204110217     c                   movel     'C'           i13tla
024205110217     C                   CALL      'FNLV13R'
024206110217     C                   PARM                    KPJBA
024207110217     C                   PARM                    DSLV13
024208110217     C                   PARM                    DSSI95
024209110217     C                   PARM                    flgbac            1
024210110217      *
024211110217     C                   SETON                                        LR
024212110218      *--------------------------------------------------------------*
024213110218      * Elaborazione bolle di un ccm                                 *
024214110218      *--------------------------------------------------------------*
024215110218     c     sr_elabora    begsr
024216110218     c                   z-add     lv76aas       k_aas
024217110218     c                   z-add     lv76mgs       k_mgs
024222110218     c* richiamo tibs69r per recuperare la ragione sociale del cliente
024223110218     c                   clear                   DS_cnaco
024224110218     c                   clear                   DS_cnind
024225110218     c                   clear                   DS_cnclp
024226110218     c                   clear                   DS_fncls
024227110218     c                   z-add     k_cli         i69kac
024228110218     c                   call      'TIBS69R'
024229110218     c                   parm                    tibs69ds
024230110218     c                   parm                    ds_cnaco
024231110218     c                   parm                    ds_cnind
024232110218     c                   parm                    ds_cnclp
024233110218     c                   parm                    ds_fncls
024234110218      *
024235110218     c     kt33          setll     titas33c
024236110218     c                   do        *hival
024237110218     c     k_cli         reade     titas33c
024238110218     c* Fine lettura per fine file o superamento data max
024239110218     c                   if        %eof(titas33c) or
024240110218     c                             tasaas>lv76aasa or
024241110218     c                             tasmgs>lv76mgsa
024242110218     c                   leave
024243110218     c                   endif
024244110712     c                   clear                   w_o95lia
024245110218     c                   exsr      chkcap
024246110218     c                   enddo
024247110218     c                   endsr
033000000713      *
088700000712      *--------------------------------------------------------------*
088800000712      * CHKCAP - SUBROUTINE CONTROLLO CAP                            *
088900000712      *--------------------------------------------------------------*
089000000713      * NB: VIENE RICHIAMATO "FNLV13R" ED IN BASE AL RITORNO DEI     *
089100000713      *     PARAMETRI STAMPO ERRORE OPPURE NO;                       *
089200000713      *     POSSONO ESSERE EFFETTUATI 2 TENTATIVI DI CONTROLLO:      *
089300000713      *     IL PRIMO: NORMALE                                        *
089400070116      *     IL SECONDO1: SE HO TROVATO CAP OBSOLETO AL PRIMO         *
089500070116      *     IL SECONDO2: SE ERRORE DI PROVINCIA AL PRIMO             *
089600000713      *---------------------------------------------------------------
089700000713      *
089800000712     C     CHKCAP        BEGSR
089900000713      *
090000000713      * INIZIALIZZO TENTATIVI
090100000713      *
090200110217     C                   MOVEL(P)  'PRIMO'       TENTATIVO         8
090300101122     c                   clear                   obslna            3 0
090400101122     c                   clear                   obslocnor        35
090500000713      *
090600000713      * PULISCO DS CHE SERVONO A "FNLV13R"
090700000713      *
090800000713     C     RITENT        TAG
090900000713      *
091000000713     C                   CLEAR                   DSLV13
091100000713     C                   CLEAR                   DSSI95
091200000713      *
091300110217      * PASSO PARAMETRI A "FNLV13R"
091400000713      *
091500000713     C                   MOVEL     '7'           I95TCN
091600000713     C                   MOVEL     'S'           I13AF0
091700000713     C                   MOVEL     'S'           I13CNV
091800000713     C                   MOVEL     'S'           I13LA3
091900000721     c                   movel     'S'           flgbac
093100000713     C                   EVAL      I95DAT = *DATE
093800000713      *
094100110217     C                   MOVEL     taslod        I95LOC
094200070116     C     TENTATIVO     IFEQ      'PRIMO'
094300110217     C                   MOVEL     tascad        I95CAP
094400000713     C                   ELSE
094500070116     C     TENTATIVO     IFEQ      'SECONDO1'
094600000713     C                   CLEAR                   I95CAP
094700070116     C                   ELSE
094800110217     C                   MOVEL     tascad        I95CAP
094900070116     C                   MOVEL     '4'           I95TCN
095000070116     C                   ENDIF
095100000713     C                   ENDIF
095200110217     C                   MOVEL     tasprd        I95PRV
095300110217     C                   MOVEL     tasnzd        I95NAR
095500000713      *
102800101122     c
102900101122     c* Per secondo tentativo per sicurezza usola località noramlizzata
103000101122     C     TENTATIVO     IFEQ      'SECONDO1'
103100101122     c                   movel     obslocnor     i95loc
103200101122     c                   endif
103300000713      *
103400000713      * LANCIO "FNLV13R"
103500000713      *
103600000713      *             --------------------------------------
103700000713     C                   CALL      'FNLV13R'
103800000713     C                   PARM                    KPJBA
103900000713     C                   PARM                    DSLV13
104000000713     C                   PARM                    DSSI95
104100000721     C                   PARM                    flgbac            1
104200000713      *             --------------------------------------
104300101122     c* Se mi è stato dato errore per secondo tentativo ma
104400101122     c*  trova i dati univoci per localit,. se corrisponde la lna
104500101122     c*  prima e dopo, tratto come se ok
104600101122     c                   if        TENTATIVO='SECONDO1 '
104700101122     C                             AND O95LNA=obslna
104800101122     c                             and %subst(O95MSG:78:1) = '9'
104900101122     c                             and o95err=' '
105100101122     c                   eval      o13rop='S'
105200101122     c                   eval      o13roc='S'
105300110713     c**                 clear                   o95msg
105400110712     c**                 eval      o95msg='          cambiata anche PROV.+
105500110712     c**                           x località UNIVOCA'
105600101122     c                   endif
105700000713      *
105800000713      * SE VI E' ERRORE STAMPO ED EVENTUALMENTE AGGIORNO
105801110303      * non vado in segnalazione se  errore "E" perchè è previsto un secondo
105802110303      *  tentativo il "SECONDO2"
105900000713      *
106000070116     C     O95ERR        IFNE      *BLANKS
106001110303     C     O95ERR        andne     'E'
106100110120     c
106200070116     C     TENTATIVO     ORNE      'PRIMO'
106300110120     c
106700101209     c*
106900110217     C                   EXSR      w_file
107100101209     c
107200110120     c                   leavesr
107300000713      *                  ----------------
107400000713     C                   ENDIF
107500000713      *
107800090408     c
107900000713      * SE NON VI E' ERRORE MA CAP = OBSOLETO RITENTO CLEARANDO IL CAP
108000000713      *
108400101124     c
108500101124     c                   if        o95err=*blanks and
108600101124     c                             tentativo='PRIMO ' and
108700101124     c                             (o95cf1<>*blanks or o95lf1<>*blanks)
108800000713      *
108900070116     C                   MOVEL     'SECONDO1'    TENTATIVO
109000101122     c* Mi salvo la linea di arrivo del cap obsoleto
109100101122     c                   eval      obslna=o95lna
109200101122     c* Mi salvo anche la località normalizzata per la seconda ricerca
109300101122     c                   eval      obslocnor=o95loc
109301110712     c                   eval      w_o95lia='O'
110200000713      *
110400000713     C                   GOTO      RITENT
110600101123     c
110700000713      *                  °°°°°°°°°°°°°°°°
110800000713      *
110900000713     C                   ENDIF
111000070116      *
111100070116      * SE VI E' ERRORE DI PROVINCIA = RITENTO CON TIPO CONTROLLO 4
111200070116      * E LIVELLO DATI >= 3 E LIVELLO AFFIDABILITA >= 2
111300070116      *
111400070116     C                   IF        (%subst(O95MSG:78:1) = '9'  OR
111500070116     C                              %subst(O95MSG:78:1) = 'E') AND
111600070116     C                             TENTATIVO = 'PRIMO'
111700070116      *
111800070116     C                   MOVEL     'SECONDO2'    TENTATIVO
111801110713     c                   eval      w_o95lia=o95lia
111900070116      *
112000070116     C                   GOTO      RITENT
112100070116      *                  °°°°°°°°°°°°°°°°
112200070116      *
112300070116     C                   ENDIF
112400110120     c
112500110120     c* Se il tisi95 non da errore ma lo da FNLV13, stampo lo stesso
112600110120     C     O95ERR        ifeq      *BLANKS
112700110120     C     TENTATIVO     andeq     'PRIMO'
112800110120     c     o13err        andne     *blanks
113000110120     c*
113100110217     C                   EXSR      w_file
113200110120     c                   endif
113300110120     c
113500000713      *
113600000712     C                   ENDSR
116800000713      *
116900000713      *--------------------------------------------------------------*
117000110217      * w_file - SUBROUTINE scrittura errori in work-file            *
117100000713      *--------------------------------------------------------------*
117200000713      *
117300110217     C     w_file        BEGSR
120300090410     c
120301110217     c                   clear                   wfeib000
126000000713      *
127000000713      * .. MSG ERRORE
127100110217     C                   MOVEL     O13MSG        eiberr
127101110308     C                   MOVEL     O13err        eibcod
127102110712     c                   if        w_o95lia<>*blANKS
127103110712     C                   MOVEL     w_o95lia      eibliv
127104110712     c                   else
127105110712     c                   movel     o95lia        eibliv
127106110712     c                   endif
127200000713      *
127300000713      * ALTRI DATI:
127400000713      *
127401110218     c                   z-add     tasccm        eibccm
127403110218     c                   movel     acorag        eibrsc
127404110218     c                   z-add     tasaas        eibaas
127405110217     c                   z-add     taslnp        eiblnp
127406110217     c                   z-add     tasnrs        eibnrs
127407110217     c                   z-add     tasnsp        eibnsp
127408110218     c                   z-add     tasrmn        eibrmn
127411110218     c                   eval      ktrc = 'A'
127412110218     c     KTAStrc       CHAIN     TITA430C
127413110218     c                   If        %FOUND
127414110218     c                   movel     TA4not        DTA4A
127415110218     c                   movel     §TA4ARMA      eibrma
127416110218     c                   endif
127417110218     c                   eval      eibpda=(lv76aas*10000)+lv76mgs
127418110218     c                   eval      eibpal=(lv76aasa*10000)+lv76mgsa
127419110225     c****               eval      eiblan=*date
127420110223     c                   eval      eibccu=lv76uni
127700000713      *
127701110712     c                   movel     tasrsd        eibrsd
127702110712     c                   movel     tasind        eibind
127703110217     C                   MOVEL     tasnzd        EIBNzd
128100110217     C                   MOVEL     tascad        eibcad
128200110217     C                   MOVEL     taslod        EIBLOD
128300110217     C                   MOVEL     tasprd        EIBPRD
134600000713      *
134700000713      * SE NON SONO RIUSCITO A CORREGGERE PULISCO
134800000713      * I DATI IN SOSTITUZIONE, AGGIORNO MESSAGGIO DI ERRORE
134900000713      * PER SECONDO TENTATIVO
135000000713      *
135100000713     C     O13ROP        IFNE      'S'
135200000713     C     O13ROC        ANDNE     'S'
135300110217     C     O13ROL        ANDNE     'S'
135400000713     C     O13RON        ANDNE     'S'
135500110217     C                   CLEAR                   EIBNAR
135600110217     C                   CLEAR                   EIBCAP
135700110217     C                   CLEAR                   EIBLOC
135800110217     C                   CLEAR                   EIBPRV
135801110713     c                   if        eibliv=*blanks or eibliv = *zeros
135802110308     c                   eval      eibliv='E'
135803110308     c                   endif
135900070116     C     TENTATIVO     IFEQ      'SECONDO1'
136000110217     C                   CLEAR                   eiberr
136100110217     C                   EVAL      eiberr='C.A.P. o località obsoleti'
136200110712     c***                if        o95msg<>*blanks
136300110712     c***                eval      eiberr=%trim(eiberr)+ '-' +
136400110712     c***                          %subst(o95msg:10:60)
136500110712     c***                endif
136600110107     C                   ENDIF
137400101122     c
137500000713     C                   ELSE
137600000713      *
137700110217      * ALTRIMENTI MUOVO CAMPI DA DS A FILE DI work
137900000713      * AGGIORNO MESSAGGIO DI ERRORE PER SECONDO TENTATIVO
138000000713      *
138100070116     C     TENTATIVO     IFEQ      'SECONDO1'
138200110217     C                   CLEAR                   eiberr
138300110712     C                   EVAL      eiberR='C.A.P. obsoleto'
138500110712     c*                  if        o95msg<>*blanks
138600110712     c*                  eval      eiberr=%trim(eiberr)+ '-' +
138700110712     c*                            %subst(o95msg:10:60)
138800110712     c*                  endif
138900000713     C                   ENDIF
139000070116      *
139100070116     C     TENTATIVO     IFEQ      'SECONDO2'
139200110217     C                   CLEAR                   eiberr
139300110713     C                   EVAL      eiberr='Provincia/CAP errato'
139500070116     C                   ENDIF
139600110119     c
139700110217     C     TENTATIVO     IFEQ      'PRIMO   '
139800110713     C                   EVAL      eiberR='CAP errato'
139900110119     c                   endif
140000000713      *
140100000713     C     O13ROP        IFEQ      'S'
140200110217     C                   MOVEL     O95PRV        eibprv
140500000713     C                   ENDIF
140600000713      *
140700000713     C     O13ROC        IFEQ      'S'
140800110217     C                   MOVEL     O95CAP        eibcap
141100000713     C                   ENDIF
141200000713      *
141300000713     C     O13ROL        IFEQ      'S'
141400110217     C                   MOVEL     O95LOC        eibloc
141700000713     C                   ENDIF
141800000713      *
141900000713     C     O13RON        IFEQ      'S'
142000110217     C                   MOVEL     O95NAR        eibnar
142300000713     C                   ENDIF
150700000713      *
150800000713      *
150900000713     C                   ENDIF
151000000713      *
151100000713      * STAMPO DETTAGLIO ERRORI
151101110308      * completo il cod messaggio in base al messaggio
151102110309     c                   eval      n=%lookup(eiberr:err)
151103110308     c                   if        n>0
151104110309     c                   eval      eibcod=erc(n)
151105110308     c                   endif
151200000713      *
151300110217     C                   WRITE     wfeib000
151400000713      *                  ==============
151500000713      *
151600110217     C                   ENDSR
151700110308**
151800110309C.A.P. incongruente con provincia e/o localita'                               50
151900110309C.A.P. inesistente                                                            51
152300110712C.A.P. obsoleto                                                               52
152500110712CAP errato                                                                    54
152600110309Inserito un CAP generico di citta' che ha il Viario                           55
152800110712C.A.P. o località obsoleti                                                    31
153000110713Provincia/CAP errato                                                          40
