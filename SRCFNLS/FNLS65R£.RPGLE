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
001901170915     FFipnd01L  UF   E           K DISK    commit(i65fcmt) usropn
001902170915     f                                     infsr(*pssr)
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
005901170221     Ffnlbl02l  iF   E           K DISK    rename(fnlbl000:fnlbl2)
006000160401     Ftiidc01l  UF   E           K DISK    commit(i65fcmt) usropn
006100160401     f                                     infsr(*pssr)
006200160503     Ffitgd32l  UF   E           K DISK    commit(i65fcmt) usropn
006300160503     f                                     infsr(*pssr)
006400081117     Ftabel00f  iF   E           K DISK
006500150511     Ffibsp01l  iF   E           K DISK
006600150511     Ffibsp02l  iF   E           K DISK    rename(fibsp000:fibsp02)
006700151211     Ffibsp11l  iF   E           K DISK
006800150514     Ffiapd01l  iF   E           K DISK
006900150514     Ffnbrv07l  iF   E           K DISK
007000150514     FAZORG01l  iF   E           K DISK
007100081119     Ftitas30c  iF   E           K DISK    extfile(wtitas) usropn
007200081119     FFLNUF01L  UF   E           K DISK
007300000731      *
007400081118
007500081119     d Wtitas          s             21    inz('GAITRAGRU /TITAS30C')
007600151020     d War5            s             21    inz('GAITRAGRU /FIAR531C')
007700081118     d W_chgdsp        s              1
007800081118     d W_dlttas        s              1
007900081118     d Wnsp_new        s                   like(blpnsp)
008000081118     d Werrore         s              1
008100081118     d ktrc            s                   like(ar6trc)
008200081118     d wtfp            s                   like(d55tfp)
008300081118     d wtfa            s                   like(d55tfa)
008400081118     d wfea            s                   like(d55tfp)
008500150506     d wlnp            s                   like(blplnp)
008600150512     d wdse            s                   like(bltdse)
008700150514     d wpdr            s                   like(apdpdr)
008800160503     d w_ogg           s                   like(tgdogg)
008900150514     d wtrov           s              1
009000150514     d wls45           s              1
009100150623     d codnum          s              3s 0
009200081201     d comando         s            200a
009300081201     d cmdlen          s             15p 5
009400151211     d PDR_daBs1       s                   like(bs1pdr)
009401170221     d wRiprBolla      s              1a
009500081117      // - Reperimento terminal di arrivo/partenza
009600081117     d fnlv55r         pr                  extpgm('FNLV55R')
009700081117     d  fnlv55ds                           likeds(FNLV55ds)
009800081119     d fnlvcnr         pr                  extpgm('FNLVCNR')
009900150623     d codnum                         3s 0
010000081124     d trul82r         pr                  extpgm('TRUL82R')
010100081124     d  trul82ds                           likeds(TRUL82DS)
010200081128     d qcmdexc         pr                  extpgm('QCMDEXC')
010300081201     d comando                      200a   options(*varsize) const
010400081201     d cmdlen                        15p 5 const
010500150514
010600150514     d fnls45r         pr                  extpgm('FNLS45R')
010700150514     d fnls45ds                            likeds(FNLS45DS)
010800150514     d dsbrv                               likeds(DSBRV)
010900000731      *
011000081128     Dcmd              s             40    inz('STRCMTCTL LCKLVL(*CHG) CMTSCOPE-
011100081128     d                                     (*ACTGRP)')
011200081128     Dcmd1             s              9    inz('ENDCMTCTL')
011300081128
011400150514     D DSBRV         E DS                  EXTNAME(FNBRV00F)
011500081119     D fnlv55ds      E DS
011600081124     D trul82ds      E DS
011700081119     D fnls65ds      E DS
011800081114     d  i65aasn2                      2s 0 overlay(i65aasn:3)
011900081118     d  i65dspn               13     20s 0
012000150514     D fnls45ds      E DS
012100000731      *
012200081124     D fnblpinf        ds
012300081202     D  blpnrr               397    400i 0
012400000731      *
012500081117     d                 ds
012600081117     d w_ksc                          7s 0
012700081119     d w_ksc4                         4  0 overlay(w_ksc:4)
012800000731     D KPJBA         E DS
012900081117     d ds3a          E ds
013000150514     d og143         E ds
013100081120
013200081124     D c_msg1          C                   CONST('Manca numeratore bolle: tele-
013300081120     d                                     fonare in Sede')
013400081120     D c_msg2          C                   CONST('Numeratore bolle non disponib-
013500081120     d                                     ile: ritentare')
013600081201     D c_msg3          C                   CONST('Aggiornamento non eseguito')
013700150506     D c_msg4          C                   CONST('Aggiornamento non eseguito: -
013800150511     D                                     manca record in FIBSP')
013900150604     D c_msg5          C                   CONST('Aggiornamento non eseguito: -
014000150604     D                                     bolla già partita')
014100150609     D c_msg6          C                   CONST('Aggiornamento non eseguito: -
014200150609     D                                     bolla allocata da altro lavoro')
014300081209     D msgjob          C                   CONST('Uscire subito da questo lavor-
014400081209     D                                     o per due minuti. Grazie')
014500081120
014600940929     IFNARBD00      01
014700940929     IFNARBK00      02
014800991104     IFIARBT00      03
014900940929     IFNARBG00      05
015000950607     IFNARBM00      06
015100950607     IFNARBV00      07
015200991104     IFNARBP00      04
015300131014     IFNARBN00      08
015400000731      *
015500000731      *---------------------------------------------------------------*
015600000731      *                                                               *
015700000731      *   O P E R A Z I O N I   I N I Z I A L I                       *
015800000731      *                                                               *
015900000731      *---------------------------------------------------------------*
016000000731      *
016100000731     C     *ENTRY        PLIST
016200000731     C                   PARM                    KPJBA
016300081114     C                   MOVEL     KPJBU         fnls65ds
016400081114
016500081114     c*  pulizia campi di output
016600081119     c                   clear                   o65err
016700081119     c                   clear                   o65msg
016800081203     c                   clear                   o65nsp
016900150514     c                   clear                   wls45
017000081119     c
017100081119     c                   Open(e)   titas30c
017200081119     c                   if        not %open(titas30c)
017300081119     c                   eval      %subst(Wtitas:7:4)='GRPS'
017400081119     c                   Open      titas30c
017500081119     c                   endif
017600081114
017700081114      * TIPO LANCIO, I65TLA "C" -           CHIUSO CON LR
017800081114      * TIPO LANCIO, I65TLA "L" - ELABORO E CHIUDO CON LR
017900081114      * TIPO LANCIO, I65TLA " " - ELABORO E CHIUDO IN RETRN
018000081114
018100081114      /free
018200081114       If i65tla<>'C';
018300081202           if not %open (fnblp00f);
018400081128              if I65STRCMT='1';
018500081128                 comando=cmd;
018600081201                 callp qcmdexc(comando : %size(comando));
018700081128              endif;
018800081128              exsr sr_openf;
018900081128           endif;
019000081114           exsr sr_elabora;
019100081114       endif;
019200081114
019300081114       kpjbu=fnls65ds;
019400081114
019500081114       if i65tla=' ';
019600081114           *inrt=*on;
019700081114       else;
019800081202           if %open(fnblp00f);
019900081128              exsr sr_closef;
020000081128              if I65STRCMT='1';
020100081201                 comando=cmd1;
020200081201                 callp qcmdexc(comando : %size(comando));
020300081128              endif;
020400081128           endif;
020500081128           if %open(titas30c);
020600081128              close titas30c;
020700081128           endif;
020800081114           *inlr=*on;
020900081114       endif;
021000081114       //-----------------------------------------------------------------
021100081114       // Cambia numero spedizione su blp e archivi collegati
021200081114       //-----------------------------------------------------------------
021300081114       begsr sr_elabora;
021400081118         clear w_chgdsp;
021500081202         CHAIN (i65aas: i65lnp: i65nrs: i65nsp) fnblp01l;
021600081202         if %found(fnblp01l);
021700081202            CHAIN(E) blpnrr fnblp00f;
021800081202            if %error;
021900150609       // Se bolla allocata e pgm richiamato per aggiornare anche la lnp della
022000150609       // bolla restituisco errore di allocazione e vado a fine
022100150609               if i65lnpn>0;
022200150609                 o65err='6';
022300150609                 o65msg=c_msg6;
022400150609                 leavesr;
022500150609               else;
022600081202               clear trul82ds;
022700081202               ul82§rrn=blpnrr;
022800081202               ul82§fil='FNBLP00F';
022900081202               ul82§win='N';
023000081209               ul82§f7='S';
023100081209               ul82§num=1;
023200081209               ul82§att=30;
023300081202               ul82§mss=msgjob;
023400081202               callp(E) trul82r(trul82ds);
023500081202               CHAIN blpnrr fnblp00f;
023600150609               endif;
023700081202            endif;
023800151022            if %found(fnblp00f)     ;
023900151022       // Proseguo con l'elaborazione solo se la bolla chainata ha
024000151022       // lo stesso numero della bolla ricevuta in input
024100151022       // Nel frattempo la bolla potrebbe essere già stata aggiornata da
024200151022       // un altro lavoro!!! E in questo caso succederebbe che andrei
024300151022       // a rinumerare nuovamente BLP ma NON i suoi collegati in quanto
024400151022       // questi ultimi non vengono chainati per nrr ma per key spedizione
024500151022             if  blpaas=i65aas and
024600151022                 blplnp=i65lnp and
024700151022                 blpnrs=i65nrs and
024800151022                 blpnsp=i65nsp     ;
024801170221       // Se richiesta la modifica della lnp bolla
024802170221       // verifico se da ripristinare la spedizione se chiusa in partenza per
024803170221       // "Merce mai Affidata"
024804170221                 eval wRiprBolla=' ';
024805170221               if i65lnpn>0 and blpcca='7' and blpft1='N';
024806170221       // la bolla non deve avere legami
024807170221                   setll (blpaas:blplnp:blpnrs:blpnsp) FNLBL02L;
024808170221                   setll (blpaas:blplnp:blpnrs:blpnsp) FNLBL01L;
024809170221                   if not %equal(fnlbl02l) and not %equal(fnlbl01l);
024810170221                      eval wRiprBolla='S';
024811170221                   endif;
024812170221               endif;
024900170221              if blpdt1>0 and wRiprBolla=' ';
025000150604                 unlock fnblp00f;
025100150604                 o65err='5';
025200150604                 o65msg=c_msg5;
025300150604                 leavesr;
025400150604              endif;
025500150506                  wlnp=blplnp;
025600150511       // Routine di reperimento dati da FIBSP se c'è da cambiare anche la Lnp
025700150506               if i65lnpn>0;
025800150512       // Verifico se presente data entrata: se presente non elaboro
025900150512                  if blpdse=0;
026000150512                     exsr sr_dse_t;
026100150512                  endif;
026200150512                  if blpdse>0 or wdse>0;
026300150512                     unlock fnblp00f;
026400150512                     leavesr;
026500150512                  endif;
026600150506                  exsr sr_lnpn;
026700150506                  if o65err<>' ';
026800150512                     unlock fnblp00f;
026900150506                     leavesr;
027000150506                  endif;
027100150506                  wlnp=i65lnpn;
027200150506               endif;
027300081118       // memorizzo in campo di comodo se richiesta variazione di data sped.
027400081202               if i65aas<>i65aasn or blpmgs<>i65mgsn;
027500081202                  w_chgdsp='S';
027600150506               endif;
027700150506       // se bisogna variare la data e/o la lnp sped. ricalcolo TFP e TFA
027800150506               if w_chgdsp='S' or i65lnpn>0;
027900081202                  exsr sr_terlna;
028000081202                  exsr sr_terlnp;
028100081202               endif;
028200081118       // Stacco nuovo numero di spedizione da numeratore
028300081202               exsr sr_numeronew;
028400081202               if o65err<>' ';
028500150512                  unlock fnblp00f;
028600081202                  leavesr;
028700081202               endif;
028800081117
028900081202               exsr sr_3a;
029000081117
029100081117       // FIAR9 - Contrassegno
029200081202               CHAIN (i65aas: i65lnp: i65nrs: i65nsp) fiar901l;
029300081202               if %found(fiar901l);
029400081202                  ar9nsp=wnsp_new;
029500081202                  if w_chgdsp='S';
029600081202                     ar9aas=i65aasn;
029700081202                  endif;
029800150506                  if i65lnpn>0 ;
029900150506                     ar9lnp=i65lnpn;
030000150506                  endif;
030100081202                  update fiar9000;
030200081202               endif;
030300081117       // FIAR6/7 - tassazione
030400081117       // Se si tratta di assegnato segue fattura tassato in partenza
030500081117       // e cambia la data, cancello la tassazione. In caso contrario
030600081117       // la aggiorno
030700081202               clear ktrc;
030800081118
030900081202               if w_chgdsp='S';
031000081202                  if  §3atb2<>*blanks;
031100081202                     ktrc='2';
031200081202                  else;
031300081202                     if %subst(§3atb1: 1: 1)='A';
031400081202                        ktrc='1';
031500081202                     endif;
031600081202                  endif;
031700081202               endif;
031800081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar601l;
031900081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar601l;
032000081202               dow not %eof(fiar601l);
032100081202                  clear w_dlttas;
032200081202                  w_ksc=ar6ksc;
032300081202                  if ar6trc=ktrc and w_ksc4<>0000 and w_ksc4<>9999;
032400081202                     w_dlttas='S';
032500081202                  endif;
032600081202                  setll (ar6aas: ar6lnp: ar6nrs: ar6nsp: ar6trc) fiar701l;
032700081202                  reade (ar6aas: ar6lnp: ar6nrs: ar6nsp: ar6trc) fiar701l;
032800081202                  dow not %eof(fiar701l);
032900081202                     if w_dlttas='S';
033000081202                        delete fiar7000;
033100081202                     else;
033200081202                     if w_chgdsp='S';
033300081202                        ar7aas=i65aasn;
033400081202                     endif;
033500150506                     if i65lnpn>0 ;
033600150506                        ar7lnp=i65lnpn;
033700150506                     endif;
033800081202                        ar7nsp=wnsp_new;
033900081202                        update fiar7000;
034000081202                     endif;
034100081202                     reade (ar6aas: ar6lnp: ar6nrs: ar6nsp: ar6trc) fiar701l;
034200081202                  enddo;
034300081202                  if w_dlttas='S';
034400081202                     delete fiar6000;
034500081202                  else;
034600081202                  if w_chgdsp='S';
034700081202                     ar6aas=i65aasn;
034800081202                  endif;
034900150506                     if i65lnpn>0 ;
035000150506                        ar6lnp=i65lnpn;
035100150506                     endif;
035200081202                     ar6nsp=wnsp_new;
035300081202                     update fiar6000;
035400081202                  endif;
035500081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar601l;
035600081202               enddo;
035700131014       // FIARBF2C - variazioni bolla
035800081210       // Non dovrebbero mai essercene... ma ormai l'avevo messo...
035900081202               exsr sr_varbol;
035901170915       // FIPND - Estensione bolla DPD
035902170915               chain (i65aas: i65lnp: i65nrs: i65nsp) fipnd01l;
035904170915               if  %found(fipnd01l);
035905170915                  if w_chgdsp='S';
035906170915                     pndaas=i65aasn;
035907170915                  endif;
035908170915                  if i65lnpn>0 ;
035909170915                     pndlnp=i65lnpn;
035910170915                  endif;
035911170915                  pndnsp=wnsp_new;
035912170915                  update fipnd000;
035914170915               endif;
036000081117       // FIAR4 - Estensione descrizioni
036100081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar401l;
036200081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar401l;
036300081202               dow not %eof(fiar401l);
036400081202                  if w_chgdsp='S';
036500081202                     ar4aas=i65aasn;
036600081202                  endif;
036700150506                  if i65lnpn>0 ;
036800150506                     ar4lnp=i65lnpn;
036900150506                  endif;
037000081202                  ar4nsp=wnsp_new;
037100081202                  update fiar4000;
037200081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar401l;
037300081202               enddo;
037400081117       // FIAR5 - Estensione testata bolla
037500081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar501l;
037600081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar501l;
037700081202               dow not %eof(fiar501l);
037800081202                  if w_chgdsp='S';
037900081202                     ar5aas=i65aasn;
038000081202                  endif;
038100150506                  if i65lnpn>0 ;
038200150506                     ar5lnp=i65lnpn;
038300150506                  endif;
038400081202                  ar5nsp=wnsp_new;
038500081202                  update fiar5000;
038600081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar501l;
038700081202               enddo;
038800151020       // FIAR5 di sede - Estensione testata bolla - Per record FAT
038900151020               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar531c;
039000151020               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar531c;
039100151020               dow not %eof(fiar531c);
039200151020                  if w_chgdsp='S';
039300151020                     ar5aas=i65aasn;
039400151020                  endif;
039500151020                  if i65lnpn>0 ;
039600151020                     ar5lnp=i65lnpn;
039700151020                  endif;
039800151020                  ar5nsp=wnsp_new;
039900151020                  update fiar500s;
040000151020                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar531c;
040100151020               enddo;
040200081117       // FIAR8 - Estensione DDT cliente
040300081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fiar801l;
040400081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fiar801l;
040500081202               dow not %eof(fiar801l);
040600081202                  if w_chgdsp='S';
040700081202                     ar8aas=i65aasn;
040800081202                  endif;
040900150506                  if i65lnpn>0 ;
041000150506                     ar8lnp=i65lnpn;
041100150506                  endif;
041200081202                  ar8nsp=wnsp_new;
041300081202                  update fiar8000;
041400081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiar801l;
041500081202               enddo;
041600081117       //  FNEVB - Eventi
041700081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fnevb01l;
041800081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fnevb01l;
041900081202               dow not %eof(fnevb01l);
042000081202                  if w_chgdsp='S';
042100081202                     evbaas=i65aasn;
042200081202                  endif;
042300150506                  if i65lnpn>0 ;
042400150506                     evblnp=i65lnpn;
042500150506                  endif;
042600081202                  evbnsp=wnsp_new;
042700081202                  update fnevb000;
042800081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnevb01l;
042900081202               enddo;
043000081118       //  FNBLT - Dettaglio colli
043100081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
043200081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
043300081202               dow not %eof(fnblt01l);
043400081202                  if w_chgdsp='S';
043500081202                     bltaas=i65aasn;
043600081202                     if bltdfv=0 and blpdbr<i65dspn;
043700081202                        clear bltnfv;
043800081202                     endif;
043900081202                  endif;
044000150506                  if i65lnpn>0 ;
044100150506                     bltlnp=i65lnpn;
044200150506                  endif;
044201170221                   if wRiprBolla='S';
044202170221                     clear  BLTDFV;
044203170221                     clear  BLTNFV;
044204170221                     clear  BLTNPP;
044205170221                     clear  BLTDCM;
044206170221                     clear  BLTDTR;
044207170221                     clear  BLTFTR;
044208170221                     clear  BLTFL1;
044209170221                   endif;
044300081202                  bltnsp=wnsp_new;
044400081202                  update fnblt000;
044500081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
044600081202               enddo;
044700140102       //  FIART - Dettaglio colli
044800140102               setll (i65aas: i65lnp: i65nrs: i65nsp) fiart01l;
044900140102               reade (i65aas: i65lnp: i65nrs: i65nsp) fiart01l;
045000140102               dow not %eof(fiart01l);
045100140102                  if w_chgdsp='S';
045200140102                     artaas=i65aasn;
045300140102                  endif;
045400150506                  if i65lnpn>0 ;
045500150506                     artlnp=i65lnpn;
045600150506                  endif;
045601170221                   if wRiprBolla='S';
045602170221                     clear  artDFV;
045603170221                     clear  artNFV;
045604170221                     clear  artNPP;
045605170221                     clear  artDCM;
045606170224                     clear  artDTC;
045608170224                     clear  artcca;
045609170221                   endif;
045700140102                  artnsp=wnsp_new;
045800140102                  update fiart000;
045900140102                  reade (i65aas: i65lnp: i65nrs: i65nsp) fiart01l;
046000140102               enddo;
046100081117       //  FNBLD - Dettaglio per stampa segnacolli
046200081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fnbld01l;
046300081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fnbld01l;
046400081202               dow not %eof(fnbld01l);
046500081202                  if w_chgdsp='S';
046600081202                     bldaas=i65aasn;
046700081202                  endif;
046800150506                  if i65lnpn>0 ;
046900150506                     bldlnp=i65lnpn;
047000150506                  endif;
047100081202                  bldnsp=wnsp_new;
047200081202                  update fnbld000;
047300081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnbld01l;
047400081202               enddo;
047500081117       //  FISGN - Colli per disk "C"
047600081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fisgn05l;
047700081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fisgn05l;
047800081202               dow not %eof(fisgn05l);
047900081202                  if w_chgdsp='S';
048000081202                     sgnaas=i65aasn;
048100081202                     sgnmgs=i65mgsn;
048200081202                     sgntna=wtfa;
048300130108       // Non serve più anzi crea problemi nel caso di fisgn duplicato su altro
048400130108       // tfp.
048500130114       //            sgntnp=wtfp;
048600081202                  endif;
048700150506                  if i65lnpn>0 ;
048800150506                     sgnlnp=i65lnpn;
048900150512                     sgntna=wtfa;
049000150506                  endif;
049100081202                  sgnnsp=wnsp_new;
049200081202                  clear sgnt6a;
049300081202                  clear sgnt6b;
049400081202                  clear sgnt6c;
049500081202                  clear sgnt6d;
049600081202                  clear sgnt6e;
049700081202                  clear sgnt6f;
049800081202                  sgndatora = %char(%timestamp:*iso0);
049900081202                  update FISGN000;
050000081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fisgn05l;
050100081202               enddo;
050200081118
050300081118       //  FIARG -
050400081202               chain (i65aas: i65lnp: i65nrs: i65nsp) fiarg01l;
050500081202               if %found(fiarg01l);
050600081202                  if w_chgdsp='S';
050700081202                     argaas=i65aasn;
050800081202                  endif;
050900150506                  if i65lnpn>0 ;
051000150506                     arglnp=i65lnpn;
051100150506                  endif;
051200081118                  argnsp=wnsp_new;
051300081202                  update fiarg000;
051400081202               endif;
051500081202       //  FIRAB -
051600081202               chain (i65aas: i65lnp: i65nrs: i65nsp) firab01l;
051700081202               if %found(firab01l);
051800081202                  if w_chgdsp='S';
051900081202                     rabaas=i65aasn;
052000081202                  endif;
052100150506                  if i65lnpn>0 ;
052200150506                     rablnp=i65lnpn;
052300150506                  endif;
052400081202                  rabnsp=wnsp_new;
052500081202                  update firab000;
052600081202               endif;
052700081118       //  TIDP3 - Scan DPD
052800081202               setll (i65aas: i65lnp: i65nrs: i65nsp) tidp301l;
052900081202               reade (i65aas: i65lnp: i65nrs: i65nsp) tidp301l;
053000081202               dow not %eof(tidp301l);
053100081202                  if w_chgdsp='S';
053200081202                     dp3aas=i65aasn;
053300151211                     dp3dtspe= i65aasn*10000 +i65mgsn ;
053400081202                  endif;
053500150513                  if i65lnpn>0 ;
053600150513                     dp3lnp=i65lnpn;
053700150513                  endif;
053800081202                  dp3nsp=wnsp_new;
053900081202                  update tidp3000;
054000081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) tidp301l;
054100081202               enddo;
054200081202       //  FNANM - Anomalie
054300081202               setll (i65aas: i65lnp: i65nrs: i65nsp) fnanm01l;
054400081202               reade (i65aas: i65lnp: i65nrs: i65nsp) fnanm01l;
054500081202               dow not %eof(fnanm01l);
054600081202                  if w_chgdsp='S';
054700081202                     anmaas=i65aasn;
054800081202                  endif;
054900150506                  if i65lnpn>0 ;
055000150506                     anmlnp=i65lnpn;
055100150513       //          aggiorno anche anmcdu se = al vecchio terminal della bolla
055200150513                     if blptfp=anmcdu;
055300150513                        anmcdu=wtfp;
055400150513                     endif;
055500150506                  endif;
055600081202                  anmnsp=wnsp_new;
055700081202                  update fnanm000;
055800081202                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnanm01l;
055900081202               enddo;
056000081209       //  FNLBL - Legami bolla. Aggiorno solo la key figlia in quanto
056100081209       //  questo pgm non gesitsce la rinumerazione di bolle già
056200081209       //  trasmesse ma solo quelle ancora da partire
056300081209               setll (i65aas: i65lnp: i65nrs: i65nsp) fnlbl01l;
056400081209               reade (i65aas: i65lnp: i65nrs: i65nsp) fnlbl01l;
056500081209               dow not %eof(fnlbl01l);
056600081209                  if w_chgdsp='S';
056700081209                     lblaan=i65aasn;
056800081210                     lblmgn=i65mgsn;
056900081209                  endif;
057000150506                  if i65lnpn>0 ;
057100150506                     lbllpn=i65lnpn;
057200150506                  endif;
057300081209                  lblnsn=wnsp_new;
057400081209                  update fnlbl000;
057500081209                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnlbl01l;
057600081209               enddo;
057700160401       // TIIDC - Disposizioni di consegna (serve in particolare per le
057800160401       //                                   dispo di reso)
057900160401               setll (i65aas: i65lnp: i65nrs: i65nsp) tiidc01l;
058000160401               reade (i65aas: i65lnp: i65nrs: i65nsp) tiidc01l;
058100160401               dow not %eof(tiidc01l);
058200160401                  if w_chgdsp='S';
058300160401                     idcaas=i65aasn;
058400160401                  endif;
058500160401                  if i65lnpn>0 ;
058600160401                     idclnp=i65lnpn;
058700160401                  endif;
058800160401                  idcnsp=wnsp_new;
058801170331                  if i65aas=idcwaas and
058802170331                     i65lnp=idcwlnp and
058803170331                     i65nrs=idcwnrs and
058804170331                     i65nsp=idcwnsp      ;
058805170331                        idcwaas=idcaas;
058806170331                        idcwlnp=idclnp;
058807170331                        idcwnsp=idcnsp;
058808170331                  endif;
058900160401                  update tiidc000;
059000160401                  reade (i65aas: i65lnp: i65nrs: i65nsp) tiidc01l;
059100160401               enddo;
059200160503       // FITGD - R.A.  (serve in particolare per le dispo di reso
059300160503               w_ogg  =%editc(i65lnp:'X')+%editc(i65nrs:'X')+
059400160503                                   %editc(i65nsp:'X')+%editc(i65aas:'X');
059500160503               setll ('S': w_ogg) fitgd32l;
059600160503               reade ('S': w_ogg) fitgd32l;
059700160503               dow not %eof(fitgd32l);
059800160503                  if w_chgdsp='S';
059900160503                     %subst(tgdogg:13:4)=%editc(i65aasn:'X');
060000160503                  endif;
060100160503                  if i65lnpn>0 ;
060200160503                     %subst(tgdogg:1:3)=%editc(i65lnpn:'X');
060300160503                  endif;
060400160503                  %subst(tgdogg:6:7)=%editc(wnsp_new:'X');
060500160503                  update fitgd000;
060600160503                  reade ('S': W_ogg) fitgd32l;
060700160503               enddo;
060800081118
060900081118       // Imposto campi di fnblp da aggiornare
061000081202               exsr sr_impblp;
061100081202               update fnblp000;
061200081202                  if i65cmtrlbk='1';
061300081202                     commit;
061400081202                  endif;
061500150514       // Se cambiata la lnp bolla richiamo aggiornamento data entrata
061600150514                  if i65lnpn>0;
061700150514                     exsr sr_dtent;
061800150514                  endif;
061900081203               o65nsp=wnsp_new;
062000151022            else;
062100151022                  unlock fnblp00f;
062200151022                  leavesr;
062300151022            endif;
062400081202            endif;
062500081202         endif;
062600081117       endsr;
062700081114       //-----------------------------------------------------------------
062800081114       // Stacca nuovo numero spedizione da numeratore
062900081114       //-----------------------------------------------------------------
063000081114       begsr sr_numeronew;
063100081114       wnsp_new=0;
063200081120       dow wnsp_new=0;
063300150506       // CHAIN(E)(i65aasn2: 3: i65lnp: 0) flnuf01l;
063400150506          CHAIN(E)(i65aasn2: 3: wlnp  : 0) flnuf01l;
063500150824       // numeratore allocato: esco
063600150824          if %error;
063700150824              o65err='2';
063800150824              o65msg=c_msg2;
063900150824              leave;
064000150824          endif;
064100081114       // Numeratore non trovato: prova a crearlo
064200081120          if not %found(flnuf01l);
064300081120              codnum=3;
064400081120              callp fnlvcnr(codnum);
064500150506       //     CHAIN(E)(i65aasn2: 3: i65lnp: 0) flnuf01l;
064600150824              CHAIN   (i65aasn2: 3: wlnp  : 0) flnuf01l;
064700081120              if not %found(flnuf01l);
064800081120                  o65err='1';
064900081120                  o65msg=c_msg1;
065000081120                  leave;
065100081120              endif;
065200081120          endif;
065300081114       // numeratore trovato verifico che non esista già una bolla
065400081120          nufnum=nufnum+1;
065500081120          update flnuf;
065600150506       // setll (i65aasn: i65lnp: i65nrs: nufnum) fnblp01l;
065700150506          setll (i65aasn: wlnp  : i65nrs: nufnum) fnblp01l;
065800081120              if %equal(fnblp01l);
065900081120                 iter;
066000081120              else;
066100150506       //        setll (i65aasn: i65lnp: i65nrs: nufnum) titas30c;
066200150506                 setll (i65aasn: wlnp  : i65nrs: nufnum) titas30c;
066300081120                 if %equal(titas30c);
066400081120                    iter;
066500081120                 endif;
066600081120              endif;
066700081120          wnsp_new=nufnum;
066800081120       enddo;
066900081114       endsr;
067000081118       //-----------------------------------------------------------------
067100081118       // Aggiornamento variazioni bolle
067200081118       //-----------------------------------------------------------------
067300081118       begsr Sr_varbol;
067400081118          *in01=*off;
067500081118          *in02=*off;
067600081118          *in03=*off;
067700081118          *in04=*off;
067800081118          *in05=*off;
067900081118          *in06=*off;
068000081118          *in07=*off;
068100131014          *in08=*off;
068200131014          setll (i65aas: i65lnp: i65nrs: i65nsp) fiarbf2c;
068300131014          reade (i65aas: i65lnp: i65nrs: i65nsp) fiarbf2c;
068400131014          dow not %eof(fiarbf2c);
068500081118             if w_chgdsp='S';
068600081118                arbaas=i65aasn;
068700081118             endif;
068800150514             if i65lnpn>0 ;
068900150514                arblnp=i65lnpn;
069000150514             endif;
069100081119             arbnsp=wnsp_new;
069200081118             if *in01;
069300081118               update fnarbd00;
069400081118             endif;
069500081118             if *in02;
069600081118               update fnarbk00;
069700081118             endif;
069800081118             if *in03;
069900081118             setll (i65aas: i65lnp: i65nrs: i65nsp) fiarbu1l;
070000081118             reade (i65aas: i65lnp: i65nrs: i65nsp) fiarbu1l;
070100081120                dow not %eof(fiarbu1l);
070200081118                   if w_chgdsp='S';
070300081118                      arbaas=i65aasn;
070400081118                   endif;
070500150514                   if i65lnpn>0 ;
070600150514                      arblnp=i65lnpn;
070700150514                   endif;
070800081119                   arbnsp=wnsp_new;
070900081118                   update fiarbu00;
071000081118                   reade (i65aas: i65lnp: i65nrs: i65nsp) fiarbu1l;
071100081118                enddo;
071200081202                update fiarbt00;
071300081118             endif;
071400081118             if *in04;
071500081202                update fnarbp00;
071600081118             endif;
071700081118             if *in05;
071800081202                update fnarbg00;
071900081118             endif;
072000081118             if *in06;
072100081202                update fnarbm00;
072200081118             endif;
072300081118             if *in07;
072400081202                update fnarbv00;
072500081118             endif;
072600131014             if *in08;
072700131014                update fnarbn00;
072800131014             endif;
072900081118             *in01=*off;
073000081118             *in02=*off;
073100081118             *in03=*off;
073200081118             *in04=*off;
073300081118             *in05=*off;
073400081118             *in06=*off;
073500081118             *in07=*off;
073600131014             *in08=*off;
073700131014          reade (i65aas: i65lnp: i65nrs: i65nsp) fiarbf2c;
073800081118          enddo;
073900081118       endsr;
074000081117       //-----------------------------------------------------------------
074100081117       // Impostazione campi di fnblp da aggiornare
074200081117       //-----------------------------------------------------------------
074300081117       begsr sr_impblp;
074400081118
074500081118       // NUMERO SPEDIZIONE
074600081118
074700081118          blpnsp=wnsp_new;
074800081118
074900081118          if w_chgdsp='S';
075000081118       // DATA SPEDIZIONE
075100081118
075200081118             blpaas=i65aasn;
075300081118             blpmgs=i65mgsn;
075400081118
075500081118       // TERMINAL TARTENZA/ARRIVO/FILIALE ELABORATORE ARRIVO
075600081118
075700081118             blptfa=wtfa;
075800081118             blpfea=wfea;
075900081118             blptfp=wtfp;
076000160509             blpfle=wtfp;
076100081118
076200081118       // SBORDERIZZAZIONE
076300081118
076400081118             if blpdbr<i65dspn;
076500081118                clear blpnfv;
076600081118                clear blpdbr;
076700081118                clear blpflp;
076800081118             endif;
076900081118          endif;
077000150514
077100150514          if i65lnpn>0;
077200150514             blplnp=i65lnpn;
077300150514             blpflb=i65lnpn;
077400150515             blptfa=wtfa;
077500150515             blpfea=wfea;
077600150515             blptfp=wtfp;
077700160509             blpfle=wtfp;
077800150514             if %subst(§3atb1: 1: 1)='A';
077900150514      /end-free
078000150514     c                   movel     i65lnpn       blpksc
078100150514      /free
078200150514             endif;
078300151211             if PDR_daBs1=0;
078400151211                wpdr = i65lnpn*10000+bsppdr;
078500151211             else;
078600151211                wpdr = PDR_daBs1;
078700151211             endif ;
078800150514             chain ('A' : wpdr) fiapd01l;
078900150514             if %found(fiapd01l);
079000150514                blppdr=wpdr;
079100150515                blpfdn=apdfcm;
079200150702             else;
079300150702                wpdr=i65lnpn*10000+0999;
079400150702                chain ('A' : wpdr) fiapd01l;
079500150702                blppdr=wpdr;
079600150702                if %found(fiapd01l);
079700150702                   blpfdn=apdfcm;
079800150702                endif;
079900150514             endif;
080000150514          endif;
080001170221             if wRiprBolla='S';
080002170221                clear blpdce  ;
080003170221                clear blpnpi;
080004170221                clear blpnci;
080005170221                clear blpnrc;
080006170221                clear blpdbr;
080007170221                clear BLPDPC;
080008170221                clear BLPDUC;
080009170221                clear BLPDDC;
080010170221                clear BLPDCM;
080011170221                clear BLPHMC;
080012170221                clear BLPNFV;
080013170221                clear BLPNDC;
080014170221                clear BLPDT1;
080015170221                clear BLPDT2;
080016170221                clear BLPFT1;
080017170221                clear BLPFT2;
080018170221                clear BLPCCA;
080019170221                clear BLPDT3;
080020170221                clear BLPFT3;
080021170221             endif;
080100081118
080200081118       // DATA RITIRO
080300081118
080400081118          if i65agdrt='S';
080500081118             blpdrt=i65dspn;
080600081118          else;
080700081118       // Se non richiesto aggiornamento data ritiro la devo comunque
080800081118       // aggiornare nel caso in cui la data ritiro sia maggiore della
080900081118       // nuova data spedizione e la spedizione non è legata a nessun
081000081118       // ORM
081100081119             if w_chgdsp='S' and blpdrt>i65dspn;
081200081118                setll (i65aas: i65lnp: i65nrs: i65nsp: 'M') fiar401l;
081300081118                if not %equal(fiar401l);
081400081119                   blpdrt=i65dspn;
081500081118                endif;
081600081118             endif;
081700081118          endif;
081800081118
081900081118       // TIPO, DATA E ORA CONSEGNA RICHIESTA
082000081118
082100081118          if i65agdcr='S';
082200081118             blptcr=i65tcrn;
082300081118             blpdcr=i65dcrn;
082400081118             blphcr=i65hcrn;
082500081118          endif;
082600081117       endsr;
082700150506       //-----------------------------------------------------------------
082800150512       // Reperimento rec FIBSP se richiesta la variazione della lnp bolla
082900150506       //-----------------------------------------------------------------
083000150506       begsr sr_lnpn;
083100151211        clear PDR_daBs1;
083200150511       // cerco record corrispondente per recuperare il cod.aut
083300150506       // con cui aggiornare la bolla.
083400150506       // La ricerca è per lnp bolla se ntw dpd, altrimenti per codice cliente
083500150506        clear og143;
083600150506        chain i65lnp azorg01l;
083700150506        if %found(azorg01l);
083800150506           og143=orgde3;
083900150506        endif;
084000150506        if §ogntw='DPD' ;
084100150511           chain i65lnp fibsp01l;
084200150506        else;
084300150511           chain blpksc fibsp02l;
084400150506        endif;
084500150506       // Non trovato record BSP --> errore: non è possibile continuare
084600150511        if not %found;
084700150506           o65err='4';
084800150506           o65msg=c_msg4;
084900150506        endif;
085000151211       // se record per cliente cerco se presente attribuzione AUT specifico
085100151211       // per il cliente/filiale
085200151211       if §ogntw<>'DPD';
085300151211          chain (i65lnpn:blpksc) fibsp11l;
085400151211          if %found(fibsp11l);
085500151211             PDR_daBs1=bs1pdr;
085600151211          endif;
085700151211       endif;
085800150506       endsr;
085900081114       //-----------------------------------------------------------------
086000081114       // 3A del codice bolla
086100081114       //-----------------------------------------------------------------
086200081114       begsr sr_3a;
086300081117            clear ds3a;
086400081117            CHAIN (1: '3A': blpcbo) tabel00f;
086500081117            if %found(tabel00f);
086600081117            ds3a=tbluni;
086700081117            endif;
086800081114       endsr;
086900081118       //-----------------------------------------------------------------
087000081118       // Terminal di partenza e arrivo della linea di arrivo
087100081118       //-----------------------------------------------------------------
087200081118       begsr sr_terlna;
087300081118             clear wtfa;
087400081118             clear wfea;
087500081118             reset fnlv55ds;
087600081118             D55lin = blplna ;
087700150506             D55lnp = wlnp   ;
087800081118             d55drf = i65dspn;
087900081118             callp fnlv55r(fnlv55ds);
088000170727       //    if d55err = ' ';
088100081118                wtfa=d55tfA;
088200081118                wfea=d55tfp;
088300170727       //    endif;
088400081118       endsr;
088500081118       //-----------------------------------------------------------------
088600081118       // Terminal di partenza della lineda di partenza
088700081118       //-----------------------------------------------------------------
088800081118       begsr sr_terlnp;
088900081118             clear wtfp;
089000081118             reset fnlv55ds;
089100150506             D55lin = wlnp   ;
089200081118             d55drf = i65dspn;
089300081118             d55tpt = 'P';
089400081118             callp fnlv55r(fnlv55ds);
089500170727       //    if d55err = ' ';
089600170727                wtfp=d55tfp;
089700170727       //    endif;
089800081118       endsr;
089900081128       //-----------------------------------------------------------------
090000081128       // Open dei file
090100081128       //-----------------------------------------------------------------
090200081128       begsr sr_openf;
090300081202              open fnblp00f;
090301170915              open Fipnd01L;
090400081128              open Fiar401L;
090500081128              open FIAR501L;
090600081128              open FIAR601L;
090700081128              open FIAR701L;
090800081128              open FIAR801L;
090900081128              open Fiar901L;
091000081128              open FNBLT01L;
091100140102              open FIART01L;
091200131014              open FIARBF2C;
091300081128              open FIARBU1L;
091400081128              open FNEVB01L;
091500081128              open FNBLD01L;
091600081128              open Fiarg01l;
091700081128              open FISGN05L;
091800081202              open firab01l;
091900081128              open tidp301l;
092000081202              open fnanm01l;
092100081209              open fnlbl01l;
092200160401              open tiidc01l;
092300160503              open fitgd32l;
092400151020              Open(e)   fiar531c;
092500151020              if not %open(fiar531c);
092600151020                 %subst(war5:7:4)='GRPS';
092700151020                 Open      fiar531c;
092800151020              endif;
092900081128       endsr;
093000081128       //-----------------------------------------------------------------
093100081128       // Close dei file
093200081128       //-----------------------------------------------------------------
093300081128       begsr sr_closef;
093400081202              close fnblp00f;
093401170915              close fipnd01l;
093500081128              close Fiar401L;
093600081128              close FIAR501L;
093700081128              close FIAR601L;
093800081128              close FIAR701L;
093900081128              close FIAR801L;
094000081128              close Fiar901L;
094100081128              close FNBLT01L;
094200140102              close FIART01L;
094300131014              close FIARBF2C;
094400081128              close FIARBU1L;
094500081128              close FNEVB01L;
094600081128              close FNBLD01L;
094700081128              close Fiarg01l;
094800081128              close FISGN05L;
094900081202              close firab01l;
095000081128              close tidp301l;
095100081202              close fnanm01l;
095200081209              close fnlbl01l;
095300160401              close tiidc01l;
095400160503              close fitgd32l;
095500151020              close fiar531c;
095600081128       endsr;
095700150512       //-----------------------------------------------------------------
095800150512       // Verifica presenza data entrata sul dettaglio colli
095900150512       //-----------------------------------------------------------------
096000150512       begsr sr_dse_t;
096100150512          clear wdse;
096200150512               setll (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
096300150512               reade (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
096400150512               dow not %eof(fnblt01l) and wdse=0 ;
096500150512                  if bltdse>0;
096600150512                     wdse=bltdse;
096700150512                  endif;
096800150512                  reade (i65aas: i65lnp: i65nrs: i65nsp) fnblt01l;
096900150512               enddo                 ;
097000150512       endsr;
097100150514       //-----------------------------------------------------------------
097200150514       // Lettura dettaglio colli della bolla per aggior. data di entrata
097300150514       //-----------------------------------------------------------------
097400150514       begsr sr_dtent;
097500150514               setll (blpaas: blplnp: blpnrs: blpnsp) fnblt01l;
097600150515               reade(n) (blpaas: blplnp: blpnrs: blpnsp) fnblt01l;
097700150514               dow not %eof(fnblt01l)            ;
097800150514                  if bltdse=0;
097900150514                     clear wtrov;
098000150514                     setll (bltfls:bltlna:bltnrs:bltnsc) fnbrv07l;
098100150514                     reade (bltfls:bltlna:bltnrs:bltnsc) fnbrv07l;
098200150514                     dow not  %eof(fnbrv07l) and wtrov=*blank ;
098300150824                         if (brvdcs>0 or brvnps=00) and
098400150824                            (brvfgs=blptfp or brvfgs=blplnp) ;
098500150514                            clear fnls45ds;
098600150514                            Dls45ric='S';
098700150514                            dls45pes=%editc(brvpes:'X');
098800150514                            FNLS45R(fnls45ds:dsbrv);
098900150514                            wtrov='1' ;
099000150514                            wls45='1' ;
099100150514                         endif;
099200150824                         if wtrov=*blanks;
099300150824                            reade (bltfls:bltlna:bltnrs:bltnsc) fnbrv07l;
099400150824                         endif;
099500150514                     enddo;
099600150514                  endif;
099700150515                  reade(n) (blpaas: blplnp: blpnrs: blpnsp) fnblt01l;
099800150514               enddo                 ;
099900150514
100000150514               if wls45='1';
100100150514                  dls45flg='C';
100200150514                  FNLS45R(fnls45ds:dsbrv);
100300150514               endif;
100400150514       endsr;
100500081201       //-----------------------------------------------------------------
100600081201       // Gestione errore generico
100700081201       //-----------------------------------------------------------------
100800081201       begsr *pssr;
100900081201       o65err='3';
101000081201       o65msg=c_msg3;
101100081201       if i65cmtrlbk='1';
101200081205          rolbk(e);
101300081201       endif;
101400081201       kpjbu=fnls65ds;
101500081210       exsr sr_closef;
101600081210       if I65STRCMT='1';
101700081210          comando=cmd1;
101800081210          callp(e) qcmdexc(comando : %size(comando));
101900081210       endif;
102000081210       *inlr=*on;
102100081210       return;
102200081210       endsr '*CANCL';
