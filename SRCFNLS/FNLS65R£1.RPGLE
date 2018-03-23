000100081114     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200081114      * FNLS65R *-----------------------------------------------------*
000300081114      *            Cambia numero spedizione bolla partenza            *
000400000731      *---------------------------------------------------------------*
000500081114      * Il programma attribuisce un nuovo numero ad una spedizione in *
000600081114      * partenza. Gestisce sia un semplice cambio di numero sia un    *
000700081114      * cambio di numero con modifica data spedizione anche a cavallo *
000800081114      * di anno                                                       *
000900081114      * E' a cura del chiamante verificare che sia possibile effettuare
001000081114      * la rinumerazione in partenza                                  *
001100150506      * 06/05/2015 - aggiunta la possibilità di attribuire anche nuova*
001200150514      *              LINEA DI PARTENZA. Utilizzata per                *
001300150506      *              "Clienti con colli in part. da più Filiali"      *
001400150527      *              (v.di file FIBSP).                               *
001500000731      *---------------------------------------------------------------*
001600081202     FFNBLP01L  iF   E           K DISK    infds(fnblpinf)
001700081202     f                                     rename(fnblp000:fnblp001)
001800081202     FFNBLP00F  UF   E             DISK    commit(i65fcmt) usropn
001900081209     f                                     infsr(*pssr)
002000081128     FFiar401L  UF   E           K DISK    commit(i65fcmt) usropn
002100081201     f                                     infsr(*pssr)
002200081128     FFIAR501L  UF   E           K DISK    commit(i65fcmt) usropn
002300081201     f                                     infsr(*pssr)
002400151020     FFIAR531c  UF   E           K DISK    extfile(war5) commit(i65fcmt) usropn
002500151020     f                                     infsr(*pssr)
002600151020     f                                     rename(fiar5000:fiar500s)
002700151020     f                                     ignore(fiar5p00)
002800081128     FFIAR601L  UF   E           K DISK    commit(i65fcmt) usropn
002900081201     f                                     infsr(*pssr)
003000081128     FFIAR701L  UF   E           K DISK    commit(i65fcmt) usropn
003100081201     f                                     infsr(*pssr)
003200081128     FFIAR801L  UF   E           K DISK    commit(i65fcmt) usropn
003300081201     f                                     infsr(*pssr)
003400081128     FFiar901L  UF   E           K DISK    commit(i65fcmt) usropn
003500081201     f                                     infsr(*pssr)
003600081128     FFNBLT01L  UF   E           K DISK    commit(i65fcmt) usropn
003700081201     f                                     infsr(*pssr)
003800140102     FFIART01L  UF   E           K DISK    commit(i65fcmt) usropn
003900140102     f                                     infsr(*pssr)
004000131014     FFIARBF2C  UF   E           K DISK    commit(i65fcmt) usropn
004100081201     f                                     infsr(*pssr)
004200081128     FFIARBU1L  UF   E           K DISK    commit(i65fcmt) usropn
004300081201     f                                     infsr(*pssr)
004400081128     FFNEVB01L  UF   E           K DISK    commit(i65fcmt) usropn
004500081201     f                                     infsr(*pssr)
004600081128     FFNBLD01L  UF   E           K DISK    commit(i65fcmt) usropn
004700081201     f                                     infsr(*pssr)
004800081128     FFiarg01l  UF   E           K DISK    commit(i65fcmt) usropn
004900081201     f                                     infsr(*pssr)
005000081128     FFISGN05L  UF   E           K DISK    commit(i65fcmt) usropn
005100081201     f                                     infsr(*pssr)
005200081202     Ffirab01l  UF   E           K DISK    commit(i65fcmt) usropn
005300081202     f                                     infsr(*pssr)
005400081128     Ftidp301l  UF   E           K DISK    commit(i65fcmt) usropn
005500081201     f                                     infsr(*pssr)
005600081202     Ffnanm01l  UF   E           K DISK    commit(i65fcmt) usropn
005700081202     f                                     infsr(*pssr)
005800081209     Ffnlbl01l  UF   E           K DISK    commit(i65fcmt) usropn
005900081209     f                                     infsr(*pssr)
006000160401     Ftiidc01l  UF   E           K DISK    commit(i65fcmt) usropn
006100160401     f                                     infsr(*pssr)
006200081117     Ftabel00f  iF   E           K DISK
006300150511     Ffibsp01l  iF   E           K DISK
006400150511     Ffibsp02l  iF   E           K DISK    rename(fibsp000:fibsp02)
006500151211     Ffibsp11l  iF   E           K DISK
006600150514     Ffiapd01l  iF   E           K DISK
006700150514     Ffnbrv07l  iF   E           K DISK
006800150514     FAZORG01l  iF   E           K DISK
006900081119     Ftitas30c  iF   E           K DISK    extfile(wtitas) usropn
007000081119     FFLNUF01L  UF   E           K DISK
007100000731      *
007200081118
007300081119     d Wtitas          s             21    inz('GAITRAGRU /TITAS30C')
007400151020     d War5            s             21    inz('GAITRAGRU /FIAR531C')
007500081118     d W_chgdsp        s              1
007600081118     d W_dlttas        s              1
007700081118     d Wnsp_new        s                   like(blpnsp)
007800081118     d Werrore         s              1
007900081118     d ktrc            s                   like(ar6trc)
008000081118     d wtfp            s                   like(d55tfp)
008100081118     d wtfa            s                   like(d55tfa)
008200081118     d wfea            s                   like(d55tfp)
008300150506     d wlnp            s                   like(blplnp)
008400150512     d wdse            s                   like(bltdse)
008500150514     d wpdr            s                   like(apdpdr)
008600150514     d wtrov           s              1
008700150514     d wls45           s              1
008800150623     d codnum          s              3s 0
008900081201     d comando         s            200a
009000081201     d cmdlen          s             15p 5
009100151211     d PDR_daBs1       s                   like(bs1pdr)
009200081117      // - Reperimento terminal di arrivo/partenza
009300081117     d fnlv55r         pr                  extpgm('FNLV55R')
009400081117     d  fnlv55ds                           likeds(FNLV55ds)
009500081119     d fnlvcnr         pr                  extpgm('FNLVCNR')
009600150623     d codnum                         3s 0
009700081124     d trul82r         pr                  extpgm('TRUL82R')
009800081124     d  trul82ds                           likeds(TRUL82DS)
009900081128     d qcmdexc         pr                  extpgm('QCMDEXC')
010000081201     d comando                      200a   options(*varsize) const
010100081201     d cmdlen                        15p 5 const
010200150514
010300150514     d fnls45r         pr                  extpgm('FNLS45R')
010400150514     d fnls45ds                            likeds(FNLS45DS)
010500150514     d dsbrv                               likeds(DSBRV)
010600000731      *
010700081128     Dcmd              s             40    inz('STRCMTCTL LCKLVL(*CHG) CMTSCOPE-
010800081128     d                                     (*ACTGRP)')
010900081128     Dcmd1             s              9    inz('ENDCMTCTL')
011000081128
011100150514     D DSBRV         E DS                  EXTNAME(FNBRV00F)
011200081119     D fnlv55ds      E DS
011300081124     D trul82ds      E DS
011400081119     D fnls65ds      E DS
011500081114     d  i65aasn2                      2s 0 overlay(i65aasn:3)
011600081118     d  i65dspn               13     20s 0
011700150514     D fnls45ds      E DS
011800000731      *
011900081124     D fnblpinf        ds
012000081202     D  blpnrr               397    400i 0
012100000731      *
012200081117     d                 ds
012300081117     d w_ksc                          7s 0
012400081119     d w_ksc4                         4  0 overlay(w_ksc:4)
012500000731     D KPJBA         E DS
012600081117     d ds3a          E ds
012700150514     d og143         E ds
012800081120
012900081124     D c_msg1          C                   CONST('Manca numeratore bolle: tele-
013000081120     d                                     fonare in Sede')
013100081120     D c_msg2          C                   CONST('Numeratore bolle non disponib-
013200081120     d                                     ile: ritentare')
013300081201     D c_msg3          C                   CONST('Aggiornamento non eseguito')
013400150506     D c_msg4          C                   CONST('Aggiornamento non eseguito: -
013500150511     D                                     manca record in FIBSP')
013600150604     D c_msg5          C                   CONST('Aggiornamento non eseguito: -
013700150604     D                                     bolla già partita')
013800150609     D c_msg6          C                   CONST('Aggiornamento non eseguito: -
013900150609     D                                     bolla allocata da altro lavoro')
014000081209     D msgjob          C                   CONST('Uscire subito da questo lavor-
014100081209     D                                     o per due minuti. Grazie')
014200081120
014300940929     IFNARBD00      01
014400940929     IFNARBK00      02
014500991104     IFIARBT00      03
014600940929     IFNARBG00      05
014700950607     IFNARBM00      06
014800950607     IFNARBV00      07
014900991104     IFNARBP00      04
015000131014     IFNARBN00      08
015100000731      *
015200000731      *---------------------------------------------------------------*
015300000731      *                                                               *
015400000731      *   O P E R A Z I O N I   I N I Z I A L I                       *
015500000731      *                                                               *
015600000731      *---------------------------------------------------------------*
015700000731      *
015800000731     C     *ENTRY        PLIST
015900000731     C                   PARM                    KPJBA
016000081114     C                   MOVEL     KPJBU         fnls65ds
016100081114
016200081114     c*  pulizia campi di output
016300081119     c                   clear                   o65err
016400081119     c                   clear                   o65msg
016500081203     c                   clear                   o65nsp
016600150514     c                   clear                   wls45
016700081119     c
016800081119     c                   Open(e)   titas30c
016900081119     c                   if        not %open(titas30c)
017000081119     c                   eval      %subst(Wtitas:7:4)='GRPS'
017100081119     c                   Open      titas30c
017200081119     c                   endif
017300081114
017400081114      * TIPO LANCIO, I65TLA "C" -           CHIUSO CON LR
017500081114      * TIPO LANCIO, I65TLA "L" - ELABORO E CHIUDO CON LR
017600081114      * TIPO LANCIO, I65TLA " " - ELABORO E CHIUDO IN RETRN
017700081114
017800081114      /free
017900081114       If i65tla<>'C';
018000081202           if not %open (fnblp00f);
018100081128              if I65STRCMT='1';
018200081128                 comando=cmd;
018300081201                 callp qcmdexc(comando : %size(comando));
018400081128              endif;
018500081128              exsr sr_openf;
018600081128           endif;
018700081114           exsr sr_elabora;
018800081114       endif;
018900081114
019000081114       kpjbu=fnls65ds;
019100081114
019200081114       if i65tla=' ';
019300081114           *inrt=*on;
019400081114       else;
019500081202           if %open(fnblp00f);
019600081128              exsr sr_closef;
019700081128              if I65STRCMT='1';
019800081201                 comando=cmd1;
019900081201                 callp qcmdexc(comando : %size(comando));
020000081128              endif;
020100081128           endif;
020200081128           if %open(titas30c);
020300081128              close titas30c;
020400081128           endif;
020500081114           *inlr=*on;
020600081114       endif;
020700081114       //-----------------------------------------------------------------
020800081114       // Cambia numero spedizione su blp e archivi collegati
020900081114       //-----------------------------------------------------------------
021000081114       begsr sr_elabora;
021100081118         clear w_chgdsp;
021200081202         CHAIN (i65aas: i65lnp: i65nrs: i65nsp) fnblp01l;
021300081202         if %found(fnblp01l);
021400081202            CHAIN(E) blpnrr fnblp00f;
021500081202            if %error;
021600150609       // Se bolla allocata e pgm richiamato per aggiornare anche la lnp della
021700150609       // bolla restituisco errore di allocazione e vado a fine
021800150609               if i65lnpn>0;
021900150609                 o65err='6';
022000150609                 o65msg=c_msg6;
022100150609                 leavesr;
022200150609               else;
022300081202               clear trul82ds;
022400081202               ul82§rrn=blpnrr;
022500081202               ul82§fil='FNBLP00F';
022600081202               ul82§win='N';
022700081209               ul82§f7='S';
022800081209               ul82§num=1;
022900081209               ul82§att=30;
023000081202               ul82§mss=msgjob;
023100081202               callp(E) trul82r(trul82ds);
023200081202               CHAIN blpnrr fnblp00f;
023300150609               endif;
023400081202            endif;
023500151022            if %found(fnblp00f)     ;
023600151022       // Proseguo con l'elaborazione solo se la bolla chainata ha
023700151022       // lo stesso numero della bolla ricevuta in input
023800151022       // Nel frattempo la bolla potrebbe essere già stata aggiornata da
023900151022       // un altro lavoro!!! E in questo caso succederebbe che andrei
024000151022       // a rinumerare nuovamente BLP ma NON i suoi collegati in quanto
024100151022       // questi ultimi non vengono chainati per nrr ma per key spedizione
024200151022             if  blpaas=i65aas and
024300151022                 blplnp=i65lnp and
024400151022                 blpnrs=i65nrs and
024500151022                 blpnsp=i65nsp     ;
024600150604              if blpdt1>0;
024700150604                 unlock fnblp00f;
024800150604                 o65err='5';
024900150604                 o65msg=c_msg5;
025000150604                 leavesr;
025100150604              endif;
025200150506                  wlnp=blplnp;
025300150511       // Routine di reperimento dati da FIBSP se c'è da cambiare anche la Lnp
025400150506               if i65lnpn>0;
025500150512       // Verifico se presente data entrata: se presente non elaboro
025600150512                  if blpdse=0;
025700150512                     exsr sr_dse_t;
025800150512                  endif;
025900150512                  if blpdse>0 or wdse>0;
026000150512                     unlock fnblp00f;
026100150512                     leavesr;
026200150512                  endif;
026300150506                  exsr sr_lnpn;
026400150506                  if o65err<>' ';
026500150512                     unlock fnblp00f;
026600150506                     leavesr;
026700150506                  endif;
026800150506                  wlnp=i65lnpn;
026900150506               endif;
027000081118       // memorizzo in campo di comodo se richiesta variazione di data sped.
027100081202               if i65aas<>i65aasn or blpmgs<>i65mgsn;
027200081202                  w_chgdsp='S';
027300150506               endif;
027400150506       // se bisogna variare la data e/o la lnp sped. ricalcolo TFP e TFA
027500150506               if w_chgdsp='S' or i65lnpn>0;
027600081202                  exsr sr_terlna;
027700081202                  exsr sr_terlnp;
027800081202               endif;
027900081118       // Stacco nuovo numero di spedizione da numeratore
028000081202               exsr sr_numeronew;
028100081202               if o65err<>' ';
028200150512                  unlock fnblp00f;
028300081202                  leavesr;
028400081202               endif;
028500081117
028600081202               exsr sr_3a;
028700081117
028800081117       // FIAR9 - Contrassegno
028900081202               CHAIN (i65aas: i65lnp: i65nrs: i65nsp) fiar901l;
029000081202               if %found(fiar901l);
029100081202                  ar9nsp=wnsp_new;
029200081202                  if w_chgdsp='S';
029300081202                     ar9aas=i65aasn;
029400081202                  endif;
029500150506                  if i65lnpn>0 ;
029600150506                     ar9lnp=i65lnpn;
029700150506                  endif;
029800081202                  update fiar9000;
029900081202               endif;
030000081117       // FIAR6/7 - tassazione
030100081117       // Se si tratta di assegnato segue fattura tassato in partenza
030200081117       // e cambia la data, cancello la tassazione. In caso contrario
030300081117       // la aggiorno
030400081202               clear ktrc;
030500081118
030600081202               if w_chgdsp='S';
030700081202                  if  §3atb2<>*blanks;
030800081202                     ktrc='2';
030900081202                  else;
031000081202                     if %subst(§3atb1: 1: 1)='A';
031100081202                        ktrc='1';
031200081202                     endif;
031300081202                  endif;
031400081202               endif;
031500081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar601l;
031600081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar601l;
031700081202               dow not %eof(fiar601l);
031800081202                  clear w_dlttas;
031900081202                  w_ksc=ar6ksc;
032000081202                  if ar6trc=ktrc and w_ksc4<>0000 and w_ksc4<>9999;
032100081202                     w_dlttas='S';
032200081202                  endif;
032300081202                  setll (ar6aas: ar6lnp: ar6nrs: ar6nsp: ar6trc) fiar701l;
032400081202                  reade (ar6aas: ar6lnp: ar6nrs: ar6nsp: ar6trc) fiar701l;
032500081202                  dow not %eof(fiar701l);
032600081202                     if w_dlttas='S';
032700081202                        delete fiar7000;
032800081202                     else;
032900081202                     if w_chgdsp='S';
033000081202                        ar7aas=i65aasn;
033100081202                     endif;
033200150506                     if i65lnpn>0 ;
033300150506                        ar7lnp=i65lnpn;
033400150506                     endif;
033500081202                        ar7nsp=wnsp_new;
033600081202                        update fiar7000;
033700081202                     endif;
033800081202                     reade (ar6aas: ar6lnp: ar6nrs: ar6nsp: ar6trc) fiar701l;
033900081202                  enddo;
034000081202                  if w_dlttas='S';
034100081202                     delete fiar6000;
034200081202                  else;
034300081202                  if w_chgdsp='S';
034400081202                     ar6aas=i65aasn;
034500081202                  endif;
034600150506                     if i65lnpn>0 ;
034700150506                        ar6lnp=i65lnpn;
034800150506                     endif;
034900081202                     ar6nsp=wnsp_new;
035000081202                     update fiar6000;
035100081202                  endif;
035200081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar601l;
035300081202               enddo;
035400131014       // FIARBF2C - variazioni bolla
035500081210       // Non dovrebbero mai essercene... ma ormai l'avevo messo...
035600081202               exsr sr_varbol;
035700081117       // FIAR4 - Estensione descrizioni
035800081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar401l;
035900081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar401l;
036000081202               dow not %eof(fiar401l);
036100081202                  if w_chgdsp='S';
036200081202                     ar4aas=i65aasn;
036300081202                  endif;
036400150506                  if i65lnpn>0 ;
036500150506                     ar4lnp=i65lnpn;
036600150506                  endif;
036700081202                  ar4nsp=wnsp_new;
036800081202                  update fiar4000;
036900081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar401l;
037000081202               enddo;
037100081117       // FIAR5 - Estensione testata bolla
037200081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar501l;
037300081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar501l;
037400081202               dow not %eof(fiar501l);
037500081202                  if w_chgdsp='S';
037600081202                     ar5aas=i65aasn;
037700081202                  endif;
037800150506                  if i65lnpn>0 ;
037900150506                     ar5lnp=i65lnpn;
038000150506                  endif;
038100081202                  ar5nsp=wnsp_new;
038200081202                  update fiar5000;
038300081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar501l;
038400081202               enddo;
038500151020       // FIAR5 di sede - Estensione testata bolla - Per record FAT
038600151020               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar531c;
038700151020               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar531c;
038800151020               dow not %eof(fiar531c);
038900151020                  if w_chgdsp='S';
039000151020                     ar5aas=i65aasn;
039100151020                  endif;
039200151020                  if i65lnpn>0 ;
039300151020                     ar5lnp=i65lnpn;
039400151020                  endif;
039500151020                  ar5nsp=wnsp_new;
039600151020                  update fiar500s;
039700151020                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar531c;
039800151020               enddo;
039900081117       // FIAR8 - Estensione DDT cliente
040000081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar801l;
040100081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar801l;
040200081202               dow not %eof(fiar801l);
040300081202                  if w_chgdsp='S';
040400081202                     ar8aas=i65aasn;
040500081202                  endif;
040600150506                  if i65lnpn>0 ;
040700150506                     ar8lnp=i65lnpn;
040800150506                  endif;
040900081202                  ar8nsp=wnsp_new;
041000081202                  update fiar8000;
041100081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar801l;
041200081202               enddo;
041300081117       //  FNEVB - Eventi
041400081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fnevb01l;
041500081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fnevb01l;
041600081202               dow not %eof(fnevb01l);
041700081202                  if w_chgdsp='S';
041800081202                     evbaas=i65aasn;
041900081202                  endif;
042000150506                  if i65lnpn>0 ;
042100150506                     evblnp=i65lnpn;
042200150506                  endif;
042300081202                  evbnsp=wnsp_new;
042400081202                  update fnevb000;
042500081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnevb01l;
042600081202               enddo;
042700081118       //  FNBLT - Dettaglio colli
042800081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
042900081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
043000081202               dow not %eof(fnblt01l);
043100081202                  if w_chgdsp='S';
043200081202                     bltaas=i65aasn;
043300081202                     if bltdfv=0 and blpdbr<i65dspn;
043400081202                        clear bltnfv;
043500081202                     endif;
043600081202                  endif;
043700150506                  if i65lnpn>0 ;
043800150506                     bltlnp=i65lnpn;
043900150506                  endif;
044000081202                  bltnsp=wnsp_new;
044100081202                  update fnblt000;
044200081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
044300081202               enddo;
044400140102       //  FIART - Dettaglio colli
044500140102               setll (i65aas: i65lnp: i65nrs: i65nsp) fiart01l;
044600140102               reade (i65aas: i65lnp: i65nrs: i65nsp) fiart01l;
044700140102               dow not %eof(fiart01l);
044800140102                  if w_chgdsp='S';
044900140102                     artaas=i65aasn;
045000140102                  endif;
045100150506                  if i65lnpn>0 ;
045200150506                     artlnp=i65lnpn;
045300150506                  endif;
045400140102                  artnsp=wnsp_new;
045500140102                  update fiart000;
045600140102                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiart01l;
045700140102               enddo;
045800081117       //  FNBLD - Dettaglio per stampa segnacolli
045900081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fnbld01l;
046000081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fnbld01l;
046100081202               dow not %eof(fnbld01l);
046200081202                  if w_chgdsp='S';
046300081202                     bldaas=i65aasn;
046400081202                  endif;
046500150506                  if i65lnpn>0 ;
046600150506                     bldlnp=i65lnpn;
046700150506                  endif;
046800081202                  bldnsp=wnsp_new;
046900081202                  update fnbld000;
047000081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnbld01l;
047100081202               enddo;
047200081117       //  FISGN - Colli per disk "C"
047300081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fisgn05l;
047400081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fisgn05l;
047500081202               dow not %eof(fisgn05l);
047600081202                  if w_chgdsp='S';
047700081202                     sgnaas=i65aasn;
047800081202                     sgnmgs=i65mgsn;
047900081202                     sgntna=wtfa;
048000130108       // Non serve più anzi crea problemi nel caso di fisgn duplicato su altro
048100130108       // tfp.
048200130114       //            sgntnp=wtfp;
048300081202                  endif;
048400150506                  if i65lnpn>0 ;
048500150506                     sgnlnp=i65lnpn;
048600150512                     sgntna=wtfa;
048700150506                  endif;
048800081202                  sgnnsp=wnsp_new;
048900081202                  clear sgnt6a;
049000081202                  clear sgnt6b;
049100081202                  clear sgnt6c;
049200081202                  clear sgnt6d;
049300081202                  clear sgnt6e;
049400081202                  clear sgnt6f;
049500081202                  sgndatora = %char(%timestamp:*iso0);
049600081202                  update FISGN000;
049700081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fisgn05l;
049800081202               enddo;
049900081118
050000081118       //  FIARG -
050100081202               chain (i65aas: i65lnp: i65nrs: i65nsp) fiarg01l;
050200081202               if %found(fiarg01l);
050300081202                  if w_chgdsp='S';
050400081202                     argaas=i65aasn;
050500081202                  endif;
050600150506                  if i65lnpn>0 ;
050700150506                     arglnp=i65lnpn;
050800150506                  endif;
050900081118                  argnsp=wnsp_new;
051000081202                  update fiarg000;
051100081202               endif;
051200081202       //  FIRAB -
051300081202               chain (i65aas: i65lnp: i65nrs: i65nsp) firab01l;
051400081202               if %found(firab01l);
051500081202                  if w_chgdsp='S';
051600081202                     rabaas=i65aasn;
051700081202                  endif;
051800150506                  if i65lnpn>0 ;
051900150506                     rablnp=i65lnpn;
052000150506                  endif;
052100081202                  rabnsp=wnsp_new;
052200081202                  update firab000;
052300081202               endif;
052400081118       //  TIDP3 - Scan DPD
052500081202               setll (i65aas: i65lnp: i65nrs: i65nsp) tidp301l;
052600081202               reade (i65aas: i65lnp: i65nrs: i65nsp) tidp301l;
052700081202               dow not %eof(tidp301l);
052800081202                  if w_chgdsp='S';
052900081202                     dp3aas=i65aasn;
053000151211                     dp3dtspe= i65aasn*10000 +i65mgsn ;
053100081202                  endif;
053200150513                  if i65lnpn>0 ;
053300150513                     dp3lnp=i65lnpn;
053400150513                  endif;
053500081202                  dp3nsp=wnsp_new;
053600081202                  update tidp3000;
053700081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) tidp301l;
053800081202               enddo;
053900081202       //  FNANM - Anomalie
054000081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fnanm01l;
054100081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fnanm01l;
054200081202               dow not %eof(fnanm01l);
054300081202                  if w_chgdsp='S';
054400081202                     anmaas=i65aasn;
054500081202                  endif;
054600150506                  if i65lnpn>0 ;
054700150506                     anmlnp=i65lnpn;
054800150513       //          aggiorno anche anmcdu se = al vecchio terminal della bolla
054900150513                     if blptfp=anmcdu;
055000150513                        anmcdu=wtfp;
055100150513                     endif;
055200150506                  endif;
055300081202                  anmnsp=wnsp_new;
055400081202                  update fnanm000;
055500081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnanm01l;
055600081202               enddo;
055700081209       //  FNLBL - Legami bolla. Aggiorno solo la key figlia in quanto
055800081209       //  questo pgm non gesitsce la rinumerazione di bolle già
055900081209       //  trasmesse ma solo quelle ancora da partire
056000081209               setll (i65aas: i65lnp: i65nrs: i65nsp) fnlbl01l;
056100081209               reade (i65aas: i65lnp: i65nrs: i65nsp) fnlbl01l;
056200081209               dow not %eof(fnlbl01l);
056300081209                  if w_chgdsp='S';
056400081209                     lblaan=i65aasn;
056500081210                     lblmgn=i65mgsn;
056600081209                  endif;
056700150506                  if i65lnpn>0 ;
056800150506                     lbllpn=i65lnpn;
056900150506                  endif;
057000081209                  lblnsn=wnsp_new;
057100081209                  update fnlbl000;
057200081209                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnlbl01l;
057300081209               enddo;
057400160401       // TIIDC - Disposizioni di consegna (serve in particolare per le
057500160401       //                                   dispo di reso)
057600160401               setll (i65aas: i65lnp: i65nrs: i65nsp) tiidc01l;
057700160401               reade (i65aas: i65lnp: i65nrs: i65nsp) tiidc01l;
057800160401               dow not %eof(tiidc01l);
057900160401                  if w_chgdsp='S';
058000160401                     idcaas=i65aasn;
058100160401                  endif;
058200160401                  if i65lnpn>0 ;
058300160401                     idclnp=i65lnpn;
058400160401                  endif;
058500160401                  idcnsp=wnsp_new;
058600160401                  update tiidc000;
058700160401                  reade (i65aas: i65lnp: i65nrs: i65nsp) tiidc01l;
058800160401               enddo;
058900081118
059000081118       // Imposto campi di fnblp da aggiornare
059100081202               exsr sr_impblp;
059200081202               update fnblp000;
059300081202                  if i65cmtrlbk='1';
059400081202                     commit;
059500081202                  endif;
059600150514       // Se cambiata la lnp bolla richiamo aggiornamento data entrata
059700150514                  if i65lnpn>0;
059800150514                     exsr sr_dtent;
059900150514                  endif;
060000081203               o65nsp=wnsp_new;
060100151022            else;
060200151022                  unlock fnblp00f;
060300151022                  leavesr;
060400151022            endif;
060500081202            endif;
060600081202         endif;
060700081117       endsr;
060800081114       //-----------------------------------------------------------------
060900081114       // Stacca nuovo numero spedizione da numeratore
061000081114       //-----------------------------------------------------------------
061100081114       begsr sr_numeronew;
061200081114       wnsp_new=0;
061300081120       dow wnsp_new=0;
061400150506       // CHAIN(E)(i65aasn2: 3: i65lnp: 0) flnuf01l;
061500150506          CHAIN(E)(i65aasn2: 3: wlnp  : 0) flnuf01l;
061600150824       // numeratore allocato: esco
061700150824          if %error;
061800150824              o65err='2';
061900150824              o65msg=c_msg2;
062000150824              leave;
062100150824          endif;
062200081114       // Numeratore non trovato: prova a crearlo
062300081120          if not %found(flnuf01l);
062400081120              codnum=3;
062500081120              callp fnlvcnr(codnum);
062600150506       //     CHAIN(E)(i65aasn2: 3: i65lnp: 0) flnuf01l;
062700150824              CHAIN   (i65aasn2: 3: wlnp  : 0) flnuf01l;
062800081120              if not %found(flnuf01l);
062900081120                  o65err='1';
063000081120                  o65msg=c_msg1;
063100081120                  leave;
063200081120              endif;
063300081120          endif;
063400081114       // numeratore trovato verifico che non esista già una bolla
063500081120          nufnum=nufnum+1;
063600081120          update flnuf;
063700150506       // setll (i65aasn: i65lnp: i65nrs: nufnum) fnblp01l;
063800150506          setll (i65aasn: wlnp  : i65nrs: nufnum) fnblp01l;
063900081120              if %equal(fnblp01l);
064000081120                 iter;
064100081120              else;
064200150506       //        setll (i65aasn: i65lnp: i65nrs: nufnum) titas30c;
064300150506                 setll (i65aasn: wlnp  : i65nrs: nufnum) titas30c;
064400081120                 if %equal(titas30c);
064500081120                    iter;
064600081120                 endif;
064700081120              endif;
064800081120          wnsp_new=nufnum;
064900081120       enddo;
065000081114       endsr;
065100081118       //-----------------------------------------------------------------
065200081118       // Aggiornamento variazioni bolle
065300081118       //-----------------------------------------------------------------
065400081118       begsr Sr_varbol;
065500081118          *in01=*off;
065600081118          *in02=*off;
065700081118          *in03=*off;
065800081118          *in04=*off;
065900081118          *in05=*off;
066000081118          *in06=*off;
066100081118          *in07=*off;
066200131014          *in08=*off;
066300131014          setll (i65aas: i65lnp: i65nrs: i65nsp) fiarbf2c;
066400131014          reade (i65aas: i65lnp: i65nrs: i65nsp) fiarbf2c;
066500131014          dow not %eof(fiarbf2c);
066600081118             if w_chgdsp='S';
066700081118                arbaas=i65aasn;
066800081118             endif;
066900150514             if i65lnpn>0 ;
067000150514                arblnp=i65lnpn;
067100150514             endif;
067200081119             arbnsp=wnsp_new;
067300081118             if *in01;
067400081118               update fnarbd00;
067500081118             endif;
067600081118             if *in02;
067700081118               update fnarbk00;
067800081118             endif;
067900081118             if *in03;
068000081118             setll (i65aas: i65lnp: i65nrs: i65nsp) fiarbu1l;
068100081118             reade (i65aas: i65lnp: i65nrs: i65nsp) fiarbu1l;
068200081120                dow not %eof(fiarbu1l);
068300081118                   if w_chgdsp='S';
068400081118                      arbaas=i65aasn;
068500081118                   endif;
068600150514                   if i65lnpn>0 ;
068700150514                      arblnp=i65lnpn;
068800150514                   endif;
068900081119                   arbnsp=wnsp_new;
069000081118                   update fiarbu00;
069100081118                   reade (i65aas: i65lnp: i65nrs: i65nsp) fiarbu1l;
069200081118                enddo;
069300081202                update fiarbt00;
069400081118             endif;
069500081118             if *in04;
069600081202                update fnarbp00;
069700081118             endif;
069800081118             if *in05;
069900081202                update fnarbg00;
070000081118             endif;
070100081118             if *in06;
070200081202                update fnarbm00;
070300081118             endif;
070400081118             if *in07;
070500081202                update fnarbv00;
070600081118             endif;
070700131014             if *in08;
070800131014                update fnarbn00;
070900131014             endif;
071000081118             *in01=*off;
071100081118             *in02=*off;
071200081118             *in03=*off;
071300081118             *in04=*off;
071400081118             *in05=*off;
071500081118             *in06=*off;
071600081118             *in07=*off;
071700131014             *in08=*off;
071800131014          reade (i65aas: i65lnp: i65nrs: i65nsp) fiarbf2c;
071900081118          enddo;
072000081118       endsr;
072100081117       //-----------------------------------------------------------------
072200081117       // Impostazione campi di fnblp da aggiornare
072300081117       //-----------------------------------------------------------------
072400081117       begsr sr_impblp;
072500081118
072600081118       // NUMERO SPEDIZIONE
072700081118
072800081118          blpnsp=wnsp_new;
072900081118
073000081118          if w_chgdsp='S';
073100081118       // DATA SPEDIZIONE
073200081118
073300081118             blpaas=i65aasn;
073400081118             blpmgs=i65mgsn;
073500081118
073600081118       // TERMINAL TARTENZA/ARRIVO/FILIALE ELABORATORE ARRIVO
073700081118
073800081118             blptfa=wtfa;
073900081118             blpfea=wfea;
074000081118             blptfp=wtfp;
074100081118
074200081118       // SBORDERIZZAZIONE
074300081118
074400081118             if blpdbr<i65dspn;
074500081118                clear blpnfv;
074600081118                clear blpdbr;
074700081118                clear blpflp;
074800081118             endif;
074900081118          endif;
075000150514
075100150514          if i65lnpn>0;
075200150514             blplnp=i65lnpn;
075300150514             blpflb=i65lnpn;
075400150515             blptfa=wtfa;
075500150515             blpfea=wfea;
075600150515             blptfp=wtfp;
075700150514             if %subst(§3atb1: 1: 1)='A';
075800150514      /end-free
075900150514     c                   movel     i65lnpn       blpksc
076000150514      /free
076100150514             endif;
076200151211             if PDR_daBs1=0;
076300151211                wpdr = i65lnpn*10000+bsppdr;
076400151211             else;
076500151211                wpdr = PDR_daBs1;
076600151211             endif ;
076700150514             chain ('A' : wpdr) fiapd01l;
076800150514             if %found(fiapd01l);
076900150514                blppdr=wpdr;
077000150515                blpfdn=apdfcm;
077100150702             else;
077200150702                wpdr=i65lnpn*10000+0999;
077300150702                chain ('A' : wpdr) fiapd01l;
077400150702                blppdr=wpdr;
077500150702                if %found(fiapd01l);
077600150702                   blpfdn=apdfcm;
077700150702                endif;
077800150514             endif;
077900150514          endif;
078000081118
078100081118       // DATA RITIRO
078200081118
078300081118          if i65agdrt='S';
078400081118             blpdrt=i65dspn;
078500081118          else;
078600081118       // Se non richiesto aggiornamento data ritiro la devo comunque
078700081118       // aggiornare nel caso in cui la data ritiro sia maggiore della
078800081118       // nuova data spedizione e la spedizione non è legata a nessun
078900081118       // ORM
079000081119             if w_chgdsp='S' and blpdrt>i65dspn;
079100081118                setll (i65aas: i65lnp: i65nrs: i65nsp: 'M') fiar401l;
079200081118                if not %equal(fiar401l);
079300081119                   blpdrt=i65dspn;
079400081118                endif;
079500081118             endif;
079600081118          endif;
079700081118
079800081118       // TIPO, DATA E ORA CONSEGNA RICHIESTA
079900081118
080000081118          if i65agdcr='S';
080100081118             blptcr=i65tcrn;
080200081118             blpdcr=i65dcrn;
080300081118             blphcr=i65hcrn;
080400081118          endif;
080500081117       endsr;
080600150506       //-----------------------------------------------------------------
080700150512       // Reperimento rec FIBSP se richiesta la variazione della lnp bolla
080800150506       //-----------------------------------------------------------------
080900150506       begsr sr_lnpn;
081000151211        clear PDR_daBs1;
081100150511       // cerco record corrispondente per recuperare il cod.aut
081200150506       // con cui aggiornare la bolla.
081300150506       // La ricerca è per lnp bolla se ntw dpd, altrimenti per codice cliente
081400150506        clear og143;
081500150506        chain i65lnp azorg01l;
081600150506        if %found(azorg01l);
081700150506           og143=orgde3;
081800150506        endif;
081900150506        if §ogntw='DPD' ;
082000150511           chain i65lnp fibsp01l;
082100150506        else;
082200150511           chain blpksc fibsp02l;
082300150506        endif;
082400150506       // Non trovato record BSP --> errore: non è possibile continuare
082500150511        if not %found;
082600150506           o65err='4';
082700150506           o65msg=c_msg4;
082800150506        endif;
082900151211       // se record per cliente cerco se presente attribuzione AUT specifico
083000151211       // per il cliente/filiale
083100151211       if §ogntw<>'DPD';
083200151211          chain (i65lnpn:blpksc) fibsp11l;
083300151211          if %found(fibsp11l);
083400151211             PDR_daBs1=bs1pdr;
083500151211          endif;
083600151211       endif;
083700150506       endsr;
083800081114       //-----------------------------------------------------------------
083900081114       // 3A del codice bolla
084000081114       //-----------------------------------------------------------------
084100081114       begsr sr_3a;
084200081117            clear ds3a;
084300081117            CHAIN (1: '3A': blpcbo) tabel00f;
084400081117            if %found(tabel00f);
084500081117            ds3a=tbluni;
084600081117            endif;
084700081114       endsr;
084800081118       //-----------------------------------------------------------------
084900081118       // Terminal di partenza e arrivo della linea di arrivo
085000081118       //-----------------------------------------------------------------
085100081118       begsr sr_terlna;
085200081118             clear wtfa;
085300081118             clear wfea;
085400081118             reset fnlv55ds;
085500081118             D55lin = blplna ;
085600150506             D55lnp = wlnp   ;
085700081118             d55drf = i65dspn;
085800081118             callp fnlv55r(fnlv55ds);
085900081118             if d55err = ' ';
086000081118                wtfa=d55tfA;
086100081118                wfea=d55tfp;
086200081118             endif;
086300081118       endsr;
086400081118       //-----------------------------------------------------------------
086500081118       // Terminal di partenza della lineda di partenza
086600081118       //-----------------------------------------------------------------
086700081118       begsr sr_terlnp;
086800081118             clear wtfp;
086900081118             reset fnlv55ds;
087000150506             D55lin = wlnp   ;
087100081118             d55drf = i65dspn;
087200081118             d55tpt = 'P';
087300081118             callp fnlv55r(fnlv55ds);
087400081118             if d55err = ' ';
087500081118                wtfp=d55tfp;
087600081118             endif;
087700081118       endsr;
087800081128       //-----------------------------------------------------------------
087900081128       // Open dei file
088000081128       //-----------------------------------------------------------------
088100081128       begsr sr_openf;
088200081202              open fnblp00f;
088300081128              open Fiar401L;
088400081128              open FIAR501L;
088500081128              open FIAR601L;
088600081128              open FIAR701L;
088700081128              open FIAR801L;
088800081128              open Fiar901L;
088900081128              open FNBLT01L;
089000140102              open FIART01L;
089100131014              open FIARBF2C;
089200081128              open FIARBU1L;
089300081128              open FNEVB01L;
089400081128              open FNBLD01L;
089500081128              open Fiarg01l;
089600081128              open FISGN05L;
089700081202              open firab01l;
089800081128              open tidp301l;
089900081202              open fnanm01l;
090000081209              open fnlbl01l;
090100160401              open tiidc01l;
090200151020              Open(e)   fiar531c;
090300151020              if not %open(fiar531c);
090400151020                 %subst(war5:7:4)='GRPS';
090500151020                 Open      fiar531c;
090600151020              endif;
090700081128       endsr;
090800081128       //-----------------------------------------------------------------
090900081128       // Close dei file
091000081128       //-----------------------------------------------------------------
091100081128       begsr sr_closef;
091200081202              close fnblp00f;
091300081128              close Fiar401L;
091400081128              close FIAR501L;
091500081128              close FIAR601L;
091600081128              close FIAR701L;
091700081128              close FIAR801L;
091800081128              close Fiar901L;
091900081128              close FNBLT01L;
092000140102              close FIART01L;
092100131014              close FIARBF2C;
092200081128              close FIARBU1L;
092300081128              close FNEVB01L;
092400081128              close FNBLD01L;
092500081128              close Fiarg01l;
092600081128              close FISGN05L;
092700081202              close firab01l;
092800081128              close tidp301l;
092900081202              close fnanm01l;
093000081209              close fnlbl01l;
093100160401              close tiidc01l;
093200151020              close fiar531c;
093300081128       endsr;
093400150512       //-----------------------------------------------------------------
093500150512       // Verifica presenza data entrata sul dettaglio colli
093600150512       //-----------------------------------------------------------------
093700150512       begsr sr_dse_t;
093800150512          clear wdse;
093900150512               setll (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
094000150512               reade (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
094100150512               dow not %eof(fnblt01l) and wdse=0 ;
094200150512                  if bltdse>0;
094300150512                     wdse=bltdse;
094400150512                  endif;
094500150512                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
094600150512               enddo                 ;
094700150512       endsr;
094800150514       //-----------------------------------------------------------------
094900150514       // Lettura dettaglio colli della bolla per aggior. data di entrata
095000150514       //-----------------------------------------------------------------
095100150514       begsr sr_dtent;
095200150514               setll (blpaas: blplnp: blpnrs: blpnsp) fnblt01l;
095300150515               reade(n) (blpaas: blplnp: blpnrs: blpnsp) fnblt01l;
095400150514               dow not %eof(fnblt01l)            ;
095500150514                  if bltdse=0;
095600150514                     clear wtrov;
095700150514                     setll (bltfls:bltlna:bltnrs:bltnsc) fnbrv07l;
095800150514                     reade (bltfls:bltlna:bltnrs:bltnsc) fnbrv07l;
095900150514                     dow not  %eof(fnbrv07l) and wtrov=*blank ;
096000150824                         if (brvdcs>0 or brvnps=00) and
096100150824                            (brvfgs=blptfp or brvfgs=blplnp) ;
096200150514                            clear fnls45ds;
096300150514                            Dls45ric='S';
096400150514                            dls45pes=%editc(brvpes:'X');
096500150514                            FNLS45R(fnls45ds:dsbrv);
096600150514                            wtrov='1' ;
096700150514                            wls45='1' ;
096800150514                         endif;
096900150824                         if wtrov=*blanks;
097000150824                            reade (bltfls:bltlna:bltnrs:bltnsc) fnbrv07l;
097100150824                         endif;
097200150514                     enddo;
097300150514                  endif;
097400150515                  reade(n) (blpaas: blplnp: blpnrs: blpnsp) fnblt01l;
097500150514               enddo                 ;
097600150514
097700150514               if wls45='1';
097800150514                  dls45flg='C';
097900150514                  FNLS45R(fnls45ds:dsbrv);
098000150514               endif;
098100150514       endsr;
098200081201       //-----------------------------------------------------------------
098300081201       // Gestione errore generico
098400081201       //-----------------------------------------------------------------
098500081201       begsr *pssr;
098600081201       o65err='3';
098700081201       o65msg=c_msg3;
098800081201       if i65cmtrlbk='1';
098900081205          rolbk(e);
099000081201       endif;
099100081201       kpjbu=fnls65ds;
099200081210       exsr sr_closef;
099300081210       if I65STRCMT='1';
099400081210          comando=cmd1;
099500081210          callp(e) qcmdexc(comando : %size(comando));
099600081210       endif;
099700081210       *inlr=*on;
099800081210       return;
099900081210       endsr '*CANCL';
