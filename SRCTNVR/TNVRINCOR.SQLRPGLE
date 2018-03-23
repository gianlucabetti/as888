000100131001      //---------------------------------------------------------------
000200131001      //
000300131001      //---------------------------------------------------------------
000400131001
000500080520     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000600040511
000700131001      //---------------------------------------------------------------
000800131001      //?Dichiarazione file.
000900131001      //---------------------------------------------------------------
001000180131     FtnvrINCOD CF   E             WORKSTN
001100180131     Fazorg01l  IF   E           K DISK
001200150911
001300150911      // -?File di stampa    ?
001400150911     fPRTF198   o    f  198        printer oflind(*inof)
001500180131     fPrtEMAIL  o    f  132        printer  oflind(*inOe)  usropn
001600040511
001700131001      //---------------------------------------------------------------
001800150909      // -?Parametri ricevuti?
001900131001     d kpjba         e ds
002000180131      // - Tabella "MRA" = Bart-Mailing - Danni
002100180131     d dMRAdan       e ds                  inz
002200180131     D* DS PER TIBS02R - GESTIONE TNTBE00F
002300180131     D TIBS02DS      E DS
002400180131      // - Campi per QCMDEXC
002500180131     d Qcmd            s            512    inz
002600180131     d
002700180131      // - Esecuzione comando di sistema
002800180131     d qCmdExc         pr                  extpgm('QCMDEXC')
002900180131     d  Qcmd                        512    const  options(*varsize)
003000180131     d  Qlen                         15  5 const
003100150909
003200131001
003300180131     d C_CmdOvrPrtF    c                   const('OVRPRTF +
003400180131     d                                           file(PRTEMAIL) +
003500180131     d                                           pagesize(66 132) +
003600180131     d                                           lpi(6) cpi(10) +
003700180131     d                                           ovrscope(*actgrpdfn) +
003800180131     d                                           ')
003900180131     d C_CmdDltOvr     c                   const('DLTOVR +
004000180131     d                                            file(PRTEMAIL) +
004100180131     d                                            lvl(*actgrpdfn)')
004200180131      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
004300180131     d TRTCM1ds      e ds                  inz
004400180131      //   ·§CM1mitt = Indirizzo e-mail del mittente
004500180131     d   §CM1mitt    e                     inz(' ed@brt.it')
004600180131      //   ·§CM1dst  = Indirizzo e-mail del destinatario
004700180131     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
004800180131      //   ·§CM1tips = Tipo lettera via e-mail:
004900180131      //    "LET" = testo allegato in corpo con logo
005000180131      //            (richiede righe libere iniziali per il logo)
005100180131      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
005200180131      //            (non consente né UNDERLINE né HIGHLIGHT)
005300180131     d   §CM1tips    e                     inz('COI')
005400180131      //   ·§CM1po   = Filiale
005500180131     d   §CM1po      e                     inz('046')
005600180131      //   ·§CM1var  = Oggetto e-mail
005700180131     d   §CM1var     e                     inz('*OBJM*+
005800180131     d                                     Totale colli incompatibili')
005900180131      //   ·§CM1sts  = Stato
006000180131     d   §CM1sts     e                     inz(*off)
006100180131      //   ·§CM1sts  = Id processo
006200180131     d   §CM1idp     e                     inz('2')
006300131001      //---------------------------------------------------------------
006400131001      //?Definizione variabili globali.
006500131001      //---------------------------------------------------------------
006600131001
006700131001      // -?Flag booleani
006800131001     d wEoF            s               n   inz(*off)
006900131001
007000131001      // -?Campi di comodo
007100180131     d TOT_col         s             15s 0
007200180131     d TOT_inc         s             15s 0
007300180201     d TOT_incs        s             15s 0
007400180201     d TOT_incj        s             15s 0
007500180201     d incs            s              7s 0
007600180201     d incj            s              7s 0
007700180201     d col             s              9s 0
007800180201     d inc             s              9s 0
007900180131     d Datasys         s               d   datfmt(*iso) inz(*sys)
008000180131     d Dataiso         s               d   datfmt(*iso)
008100180131     d Dataeur         s               d   datfmt(*eur)
008200180131     d amgdri          s              8  0
008300180131     d amgprima        s              8  0
008400180131     d wdat            s              8  0
008500180131     d Kdat            s              8  0
008600180131     d wdata           s              8  0
008700180131     d amgdat          s              8  0
008800180131     d dateu           s              8  0
008900180131     d O_testo         s            132    inz
009000180131     d perc            s              5  2 inz
009100180131     D Writerig        S              4  0 inz
009200180131     d Trul09ds      e ds
009300180131     d  T09                   19    108  0 dim(30)
009400131001
009500131001      //---------------------------------------------------------------
009600131001
009700131001     c     *Entry        plist
009800131001     c                   parm                    kpjba
009900180131     C                   eval      DATAeur=datasys
010000180131     c                   movel     dataeur       v1cdri
010100180201     c                   clear                   v1cmail
010200180131
010300180131     c     for01         tag
010400180131     C                   EXFMT     vrINCOD
010500180131     c                   setoff                                       2890
010600180131     c
010700180131     c* F3 - Fine
010800180131     c   kc              goto      fine
010900180131
011000180131     c                   eval      wdat=v1cdri
011100180131     c                   exsr      CTRDEV
011200180131     c                   eval      v1cdri=wdat
011300180131
011400180131     c                   if        *in90
011500180131     c                   seton                                        28
011600180131     C                   EVAL      V1cMSG='DATA ERRATA'
011700180131     c                   goto      FOR01
011800180131    1c                   endif
011900180131
012000180131     C                   IF        NOT *INKF
012100180131     C                   GOTO      FOR01
012200180131     C                   ENDIF
012300180131     C
012400180131     c                   eval      KDAT  =amgdat
012500180131     C*
012600180131     c                   movel     kdat          kdatalfa          8
012700180131
012800180131       //?Operazioni iniziali
012900180131       exsr RoutInz;
013000180131     c*
013100180131     c                   Clear                   tibs02ds
013200180131     c                   Clear                   dmradan
013300180131     c                   Movel     'C'           T02Mod
013400180131     c                   Movel     KNSIF         T02Sif
013500180131     c                   Movel     'MRA'         T02Cod
013600180131     c                   Movel     'TNVRINCOR'   T02Ke1
013700180131     c                   Call      'TIBS02R'
013800180131     c                   Parm                    Kpjba
013900180131     c                   Parm                    tibs02ds
014000180131    2c                   If        T02Err = *Blanks
014100180131     c                   Movel     T02Uni        Dmradan
014200180131    2c                   endif
014300180131
014400180202       if v1cmail<>*blaNKS ;
014500180131       exsr sr_Override;
014600180202       ENDIF  ;
014700180131
014800180131     c* Cerco tutti i terminal di arrivo
014900180131     c     *loval        setll     azorg01l
015000180131     c                   read      azorg01l
015100180131     c                   dow       not %eof(azorg01l)
015200180131     c
015300180131     c                   if        orgfag='F' or orgfag='A'
015400180131     c                   clear                   trul09ds
015500180131     c                   eval      i09mod='TFA'
015600180131     c                   eval      i09tfa=orgfil
015700180131     c                   eval      i09dta=kdat
015800180131     c                   call      'TRUL09R'
015900180131     c                   parm                    trul09ds
016000180131     c
016100180131     c                   if        (o09err=' '  and t09(1)>000)
016200180206     c                   if        kdat>=20180211  or
016300180131     c                              orgfil=001 or orgfil=275
016400180131     c
016500131001      /free
016600131001
016700131001       //?Elabora file
016800131001       exsr Elabora;
016900180131     c                   endif
017000180131     c                   endif
017100180131     c                   endif
017200180131
017300180131     c                   read      azorg01l
017400180131     c                   enddo
017500180202
017600180202     C                   IF        V1CMAIL<>*BLANKS
017700180131     c                   exsr      routend
017800180202     C                   ENDIF
017900180131     C
018000180131     C     FINE          TAG
018100131001
018200131001       //?Operazioni finali
018300180131            *inlr='1'       ;
018400131001
018500131001       //--------------------------------------------------------------
018600131001       //?Operazioni iniziali.
018700131001       //--------------------------------------------------------------
018800131001       BEGSR RoutInz;
018900131001
019000131001       //?Impostazione opzioni per SQL?
019100150909         exec sql set option DynUsrPrf = *Owner,
019200150909                             CloSqlCsr = *EndMod;
019300150909
019400131001
019500131001       ENDSR;
019600131001
019700131001       //--------------------------------------------------------------
019800131001       //?Elabora
019900131001       //--------------------------------------------------------------
020000150909       BEGSR Elabora;
020100131001
020200131001         wEoF = *off;
020300180131         clear TOT_col;
020400180131         clear TOT_inc;
020500180201         clear TOT_incs;
020600180201         clear TOT_incj;
020700180201         clear incs;
020800180201         clear incj;
020900180201         clear inc ;
021000180201         clear col ;
021100131001
021200180131       //?
021300131001         exec sql
021400180131         with sel1 as(
021500180131         select *        from filtra201/fnart00f where artdam=:kdat)
021600180131         select count(*) into :tot_col  from sel1, filtra201/fnarb00f
021700180131         where artaas=
021800180131         arbaas and artlnp=arblnp and artnrs=arbnrs and artnsp=arbnsp
021900180131         and arbtfa=:orgfil;
022000180131
022100180131         exec sql
022200180131         with sel2 as(
022300180131         select * from gaitragru/titah00f
022400180131                   where tahtrc='J' and substr(tahnot, 5,
022500180131         8 )=:kdatalfa )
022600180201         select count(*) into :tot_inc from sel2,
022700180131                   filtra201/fnarb00f  where tahaas=
022800180131         arbaas and tahlnp=arblnp and tahnrs=arbnrs and tahnsp=arbnsp
022900180131         and arbtfa=:orgfil;
023000180131
023100180131
023200180131       clear perc ;
023300180131       if tot_inc>0 and tot_col>0  ;
023400180131       eval(h) perc   = (100*tot_inc)/tot_col     ;
023500180131       endif ;
023600180201
023700180201         exec sql
023800180201         with sel3 as(
023900180201         select * from gaitragru/titah00f
024000180201                   where tahtrc='J' and substr(tahnot, 5,
024100180201         8 )=:kdatalfa and substr(tahnot, 15, 1)='S')
024200180201         select count(*) into :tot_incs from sel3,
024300180201                   filtra201/fnarb00f  where tahaas=
024400180201         arbaas and tahlnp=arblnp and tahnrs=arbnrs and tahnsp=arbnsp
024500180201         and arbtfa=:orgfil;
024600180201
024700180201         tot_incJ=tot_inc-tot_incs  ;
024800180201         col =tot_col  ;
024900180201         inc =tot_inc  ;
025000180201         incs=tot_incs ;
025100180201         incj=tot_incj ;
025200180201
025300180131         except tot1;
025400180202
025500180202         if  v1cmail<>*blanks  ;
025600180131         EXSR      SR_OPENPRTF   ;
025700180202         endif  ;
025800180131
025900131001       ENDSR;
026000180131       //--------------------------------------------------------------
026100180131       BEGSR sr_OpenPrtF;
026200180131
026300180131           clear O_testo;
026400180201           %subst(O_testo:1:20) = %editc(orgfil:'X')+'-'+orgdes ;
026500180201           %subst(O_testo:22:5) = 'colli'                  ;
026600180201           %subst(O_testo:28:11) =  %editc(col:'2')    ;
026700180201           %subst(O_testo:40:14) = 'di cui INCOMP.'      ;
026800180201           %subst(O_testo:54:11) =  %editc(inc:'2')    ;
026900180201           %subst(O_testo:66:6) =  %editc(perc:'4')    ;
027000180201           %subst(O_testo:72:2) = '%|'                   ;
027100180201           %subst(O_testo:74:13) = 'con spunta"S"'      ;
027200180201           %subst(O_testo:87:9) =  %editc(incs:'2')    ;
027300180201           %subst(O_testo:97:13) = 'con foglioINC'      ;
027400180201           %subst(O_testo:111:9) =  %editc(incj:'2')    ;
027500180131           except PrtDet;
027600180131           writerig=writerig+1  ;
027700180131
027800180131           // se ne ho scritte 50 di righe devo inviare
027900180131           if writerig>=50  ;
028000180131             exsr      Routend  ;
028100180131             clear writerig                  ;
028200180131
028300180131             exsr sr_Override;
028400180131
028500180131           endif  ;
028600180131
028700180131       ENDSR;
028800180131
028900180131       //--------------------------------------------------------------
029000180131       //?Override al file di stampa per impostarvi i dati per
029100180131       //?  l'invio via e-mail   +   stampa inizio e-mail
029200180131       //--------------------------------------------------------------
029300180131       BEGSR sr_Override;
029400180131
029500180131         reset TRTCM1ds;
029600180131
029700180131         IF  §MRAdreg <> *blank;
029800180131           §CM1mitt = %trim(§MRAdmitt);
029900180131           §CM1dst  = %trim(§MRAddest);
030000180201           if v1cmail<>*blanks   ;
030100180201           §cm1dst=%trim(v1cmail)+'@brt.it' ;
030200180201           endif ;
030300180131
030400180131
030500180131           §CM1tips = §MRAdreg;
030600180131           §CM1po   = '046'             ;
030700180131           §CM1var  = '*OBJM*' + 'totale colli incompatibili per TFA'   ;
030800180131           §CM1idp  = §MRAdidpro;
030900180131           Qcmd = C_CmdOvrPrtF
031000180131                + ' outq(' + %trim(§MRAdoutqi) + ')'
031100180131                + ' usrdfndta(''' + TRTCM1ds + ''')';
031200180131         ELSE;
031300180131           Qcmd = C_CmdOvrPrtF;
031400180131         ENDIF;
031500180131
031600180131         callp(e) qCmdExc(Qcmd : %size(Qcmd));
031700180131
031800180131           open PrtEMAIL;
031900180131           o_testo='Data arrivo  ' + %editw(v1cdri: '  /  /    ') ;
032000180131           except PrtDet;
032100180131           clear o_testo ;
032200180131           except PrtDet;
032300180131
032400180131       ENDSR;
032500180131
032600180131       //--------------------------------------------------------------
032700180131       BEGSR RoutEnd;
032800180131
032900180131         if %open(PrtEMAIL);
033000180131
033100180131         //?Chiusura dello spool?
033200180131           clear O_testo;
033300180131           except PrtDet;
033400180131           O_testo = '***   Fine Lista   ***';
033500180131           except PrtDet;
033600180131
033700180131           close PrtEMAIL;
033800180131
033900180131         //?Eliminazione overflow?
034000180131           Qcmd = C_CmdDltOvr;
034100180131           callp(e) qCmdExc(Qcmd : %size(Qcmd));
034200180131
034300180131         endif;
034400180131
034500180131       ENDSR;
034600180131     c     CTRDEV        BEGSR
034700180131    1c                   if        wdat>0
034800180131     c                   movel     wdat          w002a             2
034900180131     c     *eur          test(d)                 wdat                   30
035000180131    2c                   if        *in30  and w002a='00'
035100180131     c     *dmy          test(d)                 wdat                   30
035200180131     c                   endif
035300180131     c* Errore
035400180131    3c                   if        *in30
035500180131     c                   seton                                        9028
035600180131     c                   goto      endctdev
035700180131     c                   endif
035800180131     c
035900180131     c                   if        w002a='00'
036000180131     c     *dmy          move      wdat          dataeur
036100180131     c                   else
036200180131     c     *eur          move      wdat          dataeur
036300180131     c                   endif
036400180131     c
036500180131     c                   else
036600180131     c                   seton                                        9028
036700180131     c                   goto      endctdev
036800180131     c                   endif
036900180131     c* la trasformo in AAAAAMMGG - iso
037000180131     c                   move      dataeur       wdat
037100180131     c                   movel     dataeur       dataiso
037200180131     c     *iso          movel     dataiso       amgdat            8 0
037300180131     c
037400180131     c
037500180131     c     endctdev      ENDSR
037600180131
037700180131
037800131001
037900180131     oprtf198   e            tot1           2
038000180131     o                                         +  0 'Colli arrivati termArr'
038100180131     o                       orgfil            +  1
038200180131     o                       orgdes            +  1
038300180201     o                       col           2   +  1
038400180201     o                                         +  1 'di cui incomp.'
038500180201     o                       inc           2   +  1
038600180201     o                       perc          4   +  1
038700180201     o                                         +  1 '%|'
038800180201     o                                         +  1 'da spunta"S"'
038900180201     o                       incs          2   +  1
039000180201     o                                         +  1 'da foglio INC'
039100180201     o                       incj          2   +  1
039200180131     o* ---------------------------------------------------------------
039300180131     oPrtEMAIL  e            PRTdet      1
039400180131     o                       O_testo
