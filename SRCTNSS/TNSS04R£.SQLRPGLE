000100110114       //==============================================================
000200110118       //?Statistica Anomalie Impianti Smistacolli (batch)             ?
000300110114       //==============================================================
000400110120       //? Il programma estrae dal file FLTR700F:                      ?
000500110120       //? - i record con dati disallineati ("|" in un qualche campo)  ?
000600110120       //?   nel file GAITRAAZM/WFTR7_1D - interrogabile               ?
000700110120       //? - i record con dati errati       ("!" in un qualche campo)  ?
000800110126       //?   solo per totalizzare in stampa                            ?
000900110120       //? e totalizza per filiale (solo in stampa):                   ?
001000110120       //? - i record con dati disallineati ("|" in un qualche campo)  ?
001100110120       //? e totalizza per filiale e postazione:                       ?
001200110120       //? - i record con dati errati       ("!" in un qualche campo)  ?
001300110120       //==============================================================
001400110114
001500110114       //--------------------------------------------------------------
001600110114       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001700110114       //--------------------------------------------------------------
001800110114
001900110114     /*PRM dbgview(*source)
002000110120     /*CMD ovrdbf file(FLTR700F) +
002100110120     /*CMD        tofile(FILTRAPRD/FLTR700F) +
002200110120     /*CMD        ovrscope(*calllvl)
002300110120     /*CMD ovrdbf file(WFTR7_1D) +
002400110120     /*CMD        tofile(GAITRAAZP/WFTR7_1D) +
002500110120     /*CMD        ovrscope(*calllvl)
002600110120     /*CMD ovrprtf file(TNSS04DISP) +
002700110120     /*CMD         tofile(*libl/TNSS04P) +
002800110120     /*CMD         ovrscope(*calllvl)
002900110120     /*CMD ovrprtf file(TNSS04ERRP) +
003000110120     /*CMD         tofile(*libl/TNSS04P) +
003100110120     /*CMD         ovrscope(*calllvl)
003200110120     /*END dltovr file(*all) lvl(*)
003300110114     /*END
003400110114
003500110114       //--------------------------------------------------------------
003600110114       //?Specifiche di controllo.                                     ?
003700110114       //--------------------------------------------------------------
003800110114
003900110114     h decedit('0,') datedit(*dmy/) option(*nodebugio)
004000110114     h dftactgrp(*no)
004100110120     h bnddir('UBBNDDIR')
004200110114     h alwnull(*inputonly)
004300110114
004400110114       //--------------------------------------------------------------
004500110114       //?Dichiarazione file.                                          ?
004600110114       //--------------------------------------------------------------
004700110118
004800110118       // -?Tabelle?
004900110121     fTNTBE01L  Uf A e           k disk    usropn
005000110120     f                                     extfile(wLibTNTBE)
005100110120
005200110120       // -?Dettaglio segnacolli Disallineati
005300110120     fWFTR7_1D  o    e             disk    usropn
005400110120     f                                     extfile(wLibWF_1D)
005500110120     f                                     rename(FLTR7000:WFTR7100)
005600110114
005700110120       // -?File di Stampa x Disallineati?
005800110120     fTNSS04DISPo    e             printer usropn
005900110121     f                                     oflind(*in25)
006000110120     f                                     rename(SS04txt   :SS041txt  )
006100110120     f                                     rename(SS04end   :SS041end  )
006200110120     f                                     ignore(SS04space)
006300110120     f                                     ignore(SS04tChaEr)
006400110120     f                                     ignore(SS04dChaEr)
006500110120       // -?File di Stampa x Con Caratteri Errati?
006600110120     fTNSS04ERRPo    e             printer usropn
006700110120     f                                     oflind(*in26)
006800110120     f                                     rename(SS04txt   :SS042txt  )
006900110120     f                                     rename(SS04end   :SS042end  )
007000110120     f                                     ignore(SS04tDisal)
007100110120     f                                     ignore(SS04dDisal)
007200110114
007300110114       //--------------------------------------------------------------
007400110114       //?Definizione costanti.                                        ?
007500110114       //--------------------------------------------------------------
007600110114
007700110114
007800110114       //--------------------------------------------------------------
007900110114       //?Definizione schiere.                                         ?
008000110114       //--------------------------------------------------------------
008100110114
008200110120       // -?Segnacolli con caratteri errati (!):?
008300110120       //   ·?postazione?
008400110120     d $Pep            s                   like(TR7pep)
008500110120     d                                     dim(999)  inz(*hival)
008600110120       //   ·?tot. rec. elaborati x postazione?
008700110120     d $PepRec         s              9  0 dim(999)  inz
008800110120       //   ·?tot. rec. errati x postazione?
008900110120     d $PepErr         s              9  0 dim(999)  inz
009000110121
009100110120       // -?Comodo per ordinamento?
009200110121     d $Comodo         s                   like(wComodo_ds)
009300110121     d                                     dim(999)  inz(*loval)
009400110121     d                                     descend
009500110114
009600110114       //--------------------------------------------------------------
009700110114       //?Definizione aree dati.                                       ?
009800110114       //--------------------------------------------------------------
009900110114
010000110114       // -?Dati utente?
010100110114     d §AzUte        e ds                  extname(AZUTE00F)
010200110114     d                                     dtaara
010300110114     d §DatiUte      e ds                  extname(dDatiUte)
010400110114     d                                     dtaara
010500110114
010600110114       //--------------------------------------------------------------
010700110114       //?Definizione strutture dati.                                  ?
010800110114       //--------------------------------------------------------------
010900110114
011000110114       // -?Status ds?
011100110114     d Status         sds
011200110114     d  SDSpgm           *proc
011300110114
011400110114       // -?Parametri ricevuti?
011500110120     d Kpjba         e ds
011600110114     d TNSS04ds      e ds                  inz
011700110118
011800110118       // -?Tabella "MVI"/"FLTR7"?
011900110118     d DmviFLTR7     e ds                  inz
012000110120
012100110120       // -?Dati estratti via SQL: spunte da VDL?
012200110120     d FLTR7ds       e ds                  extname(FLTR700F)  inz
012300110121
012400110121       // -?Dati per ordinamento elenco in stampa?
012500110121     d wComodo_ds      ds                  inz
012600110121     d   wPerc                             like(PDCpepPrc)    inz
012700110121     d   wPepRec                           like(PDCpepRec)    inz
012800110121     d   wPep                              like(PDCpep)       inz
012900110121     d   wXx                               like(xx)           inz
013000110114
013100110114       //--------------------------------------------------------------
013200110114       //?Definizione variabili globali.                               ?
013300110114       //--------------------------------------------------------------
013400110121
013500110121       // -?Flags booleani?
013600110121     d $First          s               n   inz(*on)
013700110120
013800110120       // -?Indici di schiera?
013900110121     d xx              s              4  0 inz
014000110114
014100110114       // -?Stringhe SQL da eseguire?
014200110120     d wSql            s           1024    inz  varying
014300110120
014400110120       // -?Nomi estesi libreria/file dei file?
014500110120     d wLibFLTR7       s             21a   inz
014600110120     d wLibWF_1D       s             21a   inz
014700110120     d wLibWF_2T       s             21a   inz
014800110120     d wLibTNTBE       s             21a   inz
014900110118
015000110118       // -?Campi di comodo?
015100110120     d SAVfil          s                   like(TR7fil)    inz(*hival)
015200110118     d wDate_Eur       s               d   inz  datfmt(*eur)
015300110120     d wDate_Iso       s               d   inz  datfmt(*iso)
015400110120     d wMaxDateA       s              8    inz(*hival)
015500110114
015600110114       //--------------------------------------------------------------
015700110114       //?Definizione prototipi procedure.                             ?
015800110114       //--------------------------------------------------------------
015900110114
016000110114       // -?Reperimento dati utente?
016100110114     d TIBS34ds      e ds                  inz
016200110114      /copy gaitrasrc/srcProtoPr,TIBS34R
016300110120
016400110120       // -?Reperimento NETA sistema AS/400 corrente?
016500110120     d currSysNeta     s              8a   inz
016600110120      /copy gaitrasrc/srcProtoPr,UBRTVNETA
016700110114
016800110114       // -?Parametri API QCAPCMD (Process Commands)?
016900110114     d Qcmd            s           2048    inz  varying
017000110114      /copy qSysInc/qRpgleSrc,QCAPCMD
017100110114       // -?API QCAPCMD (Process Commands)?
017200110114      /copy gaitrasrc/srcProtoPR,QCAPCMD
017300110114
017400110114       // -?Parametri gestione errori API.?
017500110114      /copy qsysinc/qrpglesrc,QUSEC
017600110114
017700110114       //--------------------------------------------------------------
017800110114       //?Definizione key-list.                                        ?
017900110114       //--------------------------------------------------------------
018000110114
018100110118       // -?File TNTBE01L?
018200110118     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
018300110118     d                                     prefix(k_)   inz
018400110114
018500110117       //--------------------------------------------------------------
018600110117       //?Indicatori utilizzati.                                       ?
018700110117       //--------------------------------------------------------------
018800110117
018900110117       //?*in25?- OverFlow PrtF TNSS04P.
019000110117
019100110114       //--------------------------------------------------------------
019200110114       //?M A I N - L I N E                                            ?
019300110114       //--------------------------------------------------------------
019400110114
019500110114     c     *Entry        plist
019600110114     c                   parm                    KPJBA
019700110114
019800110114      /free
019900110114
020000110114       // -?Operazioni iniziali?
020100110114       exsr  sr_RoutInz;
020200110114
020300110120       // -?1° ciclo di elaborazione:?
020400110120       //   ·?lettura FLTR700F?
020500110120       //   ·?scrittura WFTR7_1D?
020600110121       //   ·?stampa totali disallineati per filiale?
020700110121       //   ·?preparazione totali con caratteri errati per filiale?
020800110120       exsr  sr_OpenCursor;
020900110114
021000110120       DoU  sqlCode <> *zero;
021100110120         exsr  sr_ReadCursor;
021200110114       EndDo;
021300110114
021400110120       exsr  sr_CloseCursor;
021500110114
021600110114       // -?Fine programma?
021700110114       exsr  sr_RoutEnd;
021800110114
021900110114       //--------------------------------------------------------------
022000110114       //?Operazioni iniziali.                                         ?
022100110114       //--------------------------------------------------------------
022200110114       BEGSR  sr_RoutInz;
022300110114
022400110114         *inLR = *on;
022500110114
022600110114         // -?Impostazione opzioni per SQL?
022700110114         exec sql  set  option  DynUsrPrf = *Owner,
022800110114                                CloSqlCsr = *EndMod;
022900110114
023000110114         // -?Reperimento dati job?
023100110114         exsr  sr_DatiJob;
023200110114
023300110114         // -?Impostazione parametri ricevuti?
023400110114         TNSS04ds = KPJBU;
023500110121
023600110121         // -?Override ai printer files?
023700110121         Qcmd = 'OVRPRTF file(TNSS04DISP) tofile(TNSS04P) +
023800110121                         usrdta(''AnImSmiDIS'')';
023900110121         exsr  sr_ExecCmd;
024000110121         if  Qusei <> *blank;
024100110121           exsr  sr_PrintErr;
024200110121         endif;
024300110121         Qcmd = 'OVRPRTF file(TNSS04ERRP) tofile(TNSS04P) +
024400110121                         usrdta(''AnImSmiERR'')';
024500110121         exsr  sr_ExecCmd;
024600110121         if  Qusei <> *blank;
024700110121           exsr  sr_PrintErr;
024800110121         endif;
024900110120
025000110120         // -?Verifica del sistema AS/400 corrente?
025100110120         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
025200110120           return;
025300110120         endif;
025400110120
025500110120         // -?Impostazione libreria per archivi in gestione?
025600110120         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
025700110120           wLibTNTBE = 'GAITRAGRU/TNTBE01L';
025800110120           wLibFLTR7 = 'FILTRA201/FLTR701L';
025900110120           wLibWF_1D = 'GAITRAAZM/WFTR7_1D';
026000110120         else;
026100110120           wLibTNTBE = 'GAITRAGRPS/TNTBE01L';
026200110120           wLibFLTR7 = 'FILTRAPRD/FLTR701L';
026300110120           wLibWF_1D = 'GAITRAAZP/WFTR7_1D';
026400110120         endif;
026500110120
026600110120         // -?Pulizia del work-file da rigenerare?
026700110120         Qcmd = 'CLRPFM file(' + %trimr(wLibWF_1D) + ')';
026800110120         exsr  sr_ExecCmd;
026900110120         if  Qusei <> *blank;
027000110120           exsr  sr_PrintErr;
027100110120         endif;
027200110120
027300110120         // -?Apertura archivi in gestione?
027400110120         //open  TNTBE01L;       ?(Dopo: in sr_RoutEnd!)?
027500110120         open  WFTR7_1D;
027600110120         open  TNSS04DisP;
027700110120         open  TNSS04ErrP;
027800110120
027900110120         // -?Impostazione iniziale della struttura dati "MVI"/"FLTR7"?
028000110120         clear  DmviFLTR7;
028100110120         §MVITR7ela = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
028200110120         §MVITR7pri = *hival;
028300110120         §MVITR7ult = *loval;
028400110120
028500110120         // -?Impostazione dati in testata?
028600110120         T1Cpgm = SDSpgm;
028700110121         wDate_Eur = %date();
028800110121         T1Cdta = %dec( wDate_Eur );
028900110120         T1Cora = %int( %subst( %char( %dec( %timestamp() ) ) :9 :6 ) );
029000110120
029100110120         // -?Impostazione separatore in stampa?
029200110120         PSX01 = *all'-';
029300110120
029400110120         // -?Stampa 1ª testata?
029500110120         T1Ctxt = '      SEGNACOLLI  DISALLINEATI (|)      ';
029600110120         write  SS041txt;
029700110120         write  SS04tDisal;
029800110120         *in25 = *off;
029900110120
030000110120         // -?Stampa 2ª testata?
030100110120         T1Ctxt = '  SEGNACOLLI  CON CARATTERI ERRATI (!)  ';
030200110120         write  SS042txt;
030300110120         write  SS04tChaEr;
030400110120         *in26 = *off;
030500110120
030600110120       ENDSR;
030700110114
030800110114       //--------------------------------------------------------------
030900110114       //?Reperimento Dati del job (Utente/Operativi).                 ?
031000110114       //--------------------------------------------------------------
031100110114       BEGSR  sr_DatiJob;
031200110114
031300110114         in(E) §AzUte;
031400110114         if NOT %error;
031500110114           in(E) §DatiUte;
031600110114         endif;
031700110114         if %error or RSut = *blanks;
031800110114           clear TIBS34ds;
031900110114           tibs34r ( tibs34ds );
032000110114           in §AzUte;
032100110114           in §DatiUte;
032200110114         endif;
032300110114
032400110114       ENDSR;
032500110114
032600110114       //--------------------------------------------------------------
032700110114       //?Apertura cursore C1.                                         ?
032800110114       //--------------------------------------------------------------
032900110120       BEGSR  sr_OpenCursor;
033000110120
033100110120         // -?Impostazione ultima data elaborabile (alfanumerica)?
033200110120         wDate_Iso = %date() - %days(1);
033300110120         wMaxDateA = %editc( %dec( wDate_Iso ) : 'X' );
033400110114
033500110114         // -?Preparazione stringa SQL?
033600110120         wSQL = 'select * from ' + %trimr( wLibFLTR7 ) +
033700110120                ' where substr(TR7tim, 1, 8) <= ''' + wMaxDateA +
033800110120              ''' order by TR7fil, TR7tim, TR7mil';
033900110114
034000110114         // -?Dichiarazione cursore?
034100110120         exec sql  prepare S1  from :wSQL;
034200110114         exec sql  declare C1  cursor for S1;
034300110114
034400110114         // -?Apertura del cursore?
034500110114         exec sql  open C1;
034600110114
034700110114         if  SQLcode < *zero;
034800110114           exsr  sr_PrintErr;
034900110114         endif;
035000110114
035100110114       ENDSR;
035200110114
035300110114       //--------------------------------------------------------------
035400110120       //?Lettura cursore C1 (file FLTR700F).                          ?
035500110114       //--------------------------------------------------------------
035600110120       BEGSR  sr_ReadCursor;
035700110114
035800110120         clear  FLTR7ds;
035900110114
036000110114         // -?Lettura cursore?
036100110120         exec sql  fetch next  from C1  into :FLTR7ds;
036200110114
036300110114         select;
036400110114           // -?Fine flusso?
036500110114           when  SQLcode = 100;
036600110114           // -?Errore?
036700110114           when  SQLcode < *zero;
036800110114             exsr  sr_PrintErr;
036900110114           // -?Elaborazione dati estratti?
037000110114           other;
037100110120             exsr  sr_Elab_FLTR7;
037200110114         endsl;
037300110114
037400110114       ENDSR;
037500110114
037600110114       //--------------------------------------------------------------
037700110114       //?Chiusura cursore C1.                                         ?
037800110114       //--------------------------------------------------------------
037900110120       BEGSR  sr_CloseCursor;
038000110120
038100110120         // -?Stampa ultimi totali?
038200110120         exsr  sr_Stampa_Disall;
038300110120         exsr  sr_Stampa_CharErr;
038400110114
038500110120         // -?Stampa "Fine Lista"?
038600110121         if  §MVITR7pri > *zero  and  §MVITR7pri < *hival;
038700110121           wDate_Eur = %date( §MVITR7pri : *iso );
038800110121           P1Cpri = %dec( wDate_Eur );
038900110121         endif;
039000110121         if  §MVITR7ult > *zero  and  §MVITR7ult < *hival;
039100110121           wDate_Eur = %date( §MVITR7ult : *iso );
039200110121           P1Cult = %dec( wDate_Eur );
039300110121         endif;
039400110120         write  SS041end;
039500110120         write  SS042end;
039600110114
039700110114         // -?Chiusura del cursore?
039800110114         exec sql  close C1;
039900110114
040000110114       ENDSR;
040100110120
040200110120       //--------------------------------------------------------------
040300110120       //?Elaborazione singolo record del file FLTR700F                ?
040400110120       //--------------------------------------------------------------
040500110120       BEGSR  sr_Elab_FLTR7;
040600110120
040700110120         // -?Controllo rotture di livello?
040800110120         if  TR7fil <> SAVfil;
040900110120           if  SAVfil <> *hival;
041000110120             exsr  sr_Stampa_Disall;
041100110120             exsr  sr_Stampa_CharErr;
041200110121             clear  SS04dDisal;
041300110121             clear  SS04dChaEr;
041400110121             reset  $Pep;
041500110121             clear  $PepRec;
041600110121             clear  $PepErr;
041700110121             clear  xx;
041800110120           endif;
041900110120           SAVfil = TR7fil;
042000110120         endif;
042100110120
042200110120         // -?Memorizzazione prima ed ultima data elaborata?
042300110120         if  %subst( TR7tim : 1 : 4 ) >= '2010'  and
042400110120             %subst( TR7tim : 1 : 4 ) <= '2060'  and
042500110120             %subst( TR7tim : 5 : 2 ) >= '01'    and
042600110120             %subst( TR7tim : 5 : 2 ) <= '12'    and
042700110120             §MVITR7pri > %int( %trim( %subst( TR7tim : 1 : 8 ) ) );
042800110120           §MVITR7pri = %int( %trim( %subst( TR7tim : 1 : 8 ) ) );
042900110120         endif;
043000110120         if  %subst( TR7tim : 1 : 4 ) >= '2010'  and
043100110120             %subst( TR7tim : 1 : 4 ) <='2060'  and
043200110120             %subst( TR7tim : 5 : 2 ) >= '01'    and
043300110120             %subst( TR7tim : 5 : 2 ) <= '12'    and
043400110120             §MVITR7ult < %int( %trim( %subst( TR7tim : 1 : 8 ) ) );
043500110120           §MVITR7ult = %int( %trim( %subst( TR7tim : 1 : 8 ) ) );
043600110120         endif;
043700110120
043800110120         // -?Conteggio generale records (per filiale)?
043900110120         PDDtotRec += 1;
044000110120         PDCtotRec += 1;
044100110120
044200110120         // -?Conteggio records disallineati (per filiale)?
044300110120         IF  %scan('|':TR7pic) > *zero   or +
044400110120             %scan('|':TR7pgr) > *zero   or +
044500110120             %scan('|':TR7pod) > *zero   or +
044600110120             %scan('|':TR7ods) > *zero   or +
044700110120             %scan('|':TR7pad) > *zero   or +
044800110120             %scan('|':TR7dts) > *zero   or +
044900110120             %scan('|':TR7ps1) > *zero   or +
045000110120             %scan('|':TR7pn1) > *zero   or +
045100110120           //%scan('|':TR7pd1) > *zero   or +
045200110120             %scan('|':TR7ps2) > *zero   or +
045300110120             %scan('|':TR7pn2) > *zero   or +
045400110120           //%scan('|':TR7pd2) > *zero   or +
045500110120             %scan('|':TR7ps3) > *zero   or +
045600110120             %scan('|':TR7pn3) > *zero   or +
045700110120             %scan('|':TR7pd3) > *zero   or +
045800110120             %scan('|':TR7ps4) > *zero   or +
045900110120             %scan('|':TR7pn4) > *zero   or +
046000110315             //%scan('|':TR7pd4) > *zero   or +
046100110315             %scan('|':%subst(TR7pd4:1:%len(TR7pd4)-2)) > *zero   or +
046200110120             %scan('|':TR7pep) > *zero   or +
046300110120             %scan('|':TR7pes) > *zero   or +
046400110120             %scan('|':TR7pup) > *zero   or +
046500110120             %scan('|':TR7pus) > *zero   or +
046600110120             %scan('|':TR7crc) > *zero   or +
046700110120             %scan('|':TR7vol) > *zero   or +
046800110120             %scan('|':TR7bar) > *zero;
046900110120
047000110120           write  WFTR7100;
047100110120           PDDdisall += 1;
047200110120
047300110120         ENDIF;
047400110120
047500110120         // -?Conteggio records elaborati (per postazione)?
047600110121         xx = %lookup( TR7pep : $Pep );
047700110120         If  xx = *zero;
047800110121           xx  = %lookup( *hival : $Pep );
047900110120           if  xx = *zero;
048000110120             xx  = %elem($Pep);
048100110120           endif;
048200110120         EndIf;
048300110120         $Pep(xx)     = TR7pep;
048400110120         $PepRec(xx) += 1;
048500110120
048600110120         // -?Conteggio records errati (per postazione)?
048700110120         //  ?(il nuovo incubatore prevede caratteri particolari nella?
048800110120         //   ?penultima posizione del campo TR7PD4...)?
048900110120         IF  %scan('!':TR7pic) > *zero   or +
049000110120             %scan('!':TR7pgr) > *zero   or +
049100110120             %scan('!':TR7pod) > *zero   or +
049200110120             %scan('!':TR7ods) > *zero   or +
049300110120             %scan('!':TR7pad) > *zero   or +
049400110120             %scan('!':TR7dts) > *zero   or +
049500110120             %scan('!':TR7ps1) > *zero   or +
049600110120             %scan('!':TR7pn1) > *zero   or +
049700110120             %scan('!':TR7pd1) > *zero   or +
049800110120             %scan('!':TR7ps2) > *zero   or +
049900110120             %scan('!':TR7pn2) > *zero   or +
050000110120             %scan('!':TR7pd2) > *zero   or +
050100110120             %scan('!':TR7ps3) > *zero   or +
050200110120             %scan('!':TR7pn3) > *zero   or +
050300110120             %scan('!':TR7pd3) > *zero   or +
050400110120             %scan('!':TR7ps4) > *zero   or +
050500110120             %scan('!':TR7pn4) > *zero   or +
050600110120             //%scan('!':TR7pd4) > *zero   or +
050700110120             %scan('!':%subst(TR7pd4:1:%len(TR7pd4)-2)) > *zero   or +
050800110120             %scan('!':TR7pep) > *zero   or +
050900110120             %scan('!':TR7pes) > *zero   or +
051000110120             %scan('!':TR7pup) > *zero   or +
051100110120             %scan('!':TR7pus) > *zero   or +
051200110120             %scan('!':TR7crc) > *zero   or +
051300110120             %scan('!':TR7vol) > *zero   or +
051400110120             %scan('!':TR7bar) > *zero;
051500110120
051600110120           PDCtotErr   += 1;
051700110120           $PepErr(xx) += 1;
051800110120
051900110120         ENDIF;
052000110120
052100110120       ENDSR;
052200110114
052300110114       //--------------------------------------------------------------
052400110120       //?Stampa dati di segnacolli con disallineati x filiale         ?
052500110114       //--------------------------------------------------------------
052600110120       BEGSR  sr_Stampa_Disall;
052700110120
052800110120         // -?Nessun segnacollo disallineato rilevato?
052900110120         if  PDDdisall = *zero;
053000110120           leavesr;
053100110120         endif;
053200110120
053300110120         // -?Calcolo percentuale disallineati (per filiale)?
053400110120         if  PDDtotRec > *zero;
053500110120           eval(h) PDDperc = (PDDdisall * 100) / PDDtotRec;
053600110120         else;
053700110120           clear   PDDperc;
053800110120         endif;
053900110120
054000110121         // -?Test percentuale rispetto al limite richiesto?
054100110121         If  PDDperc >= D04prc1;
054200110114
054300110121           // -?Test OverFlow per stampa testata?
054400110121           if  *in25;
054500110121             write  SS041txt;
054600110121             write  SS04tDisal;
054700110121             *in25 = *off;
054800110121           endif;
054900110121
055000110121           // -?Valorizzazione altri campi di output?
055100110121           PDDfil = SAVfil;
055200110121
055300110121           // -?Stampa del record per filiale?
055400110121           write  SS04dDisal;
055500110121
055600110121         EndIf;
055700110114
055800110114       ENDSR;
055900110120
056000110120       //--------------------------------------------------------------
056100110120       //?Stampa dati di segnacolli con caretteri errati x fil./baia   ?
056200110120       //--------------------------------------------------------------
056300110120       BEGSR  sr_Stampa_CharErr;
056400110120
056500110120         // -?Nessun segnacollo con catatteri errati rilevato?
056600110120         if  PDCtotRec = *zero;
056700110120           leavesr;
056800110120         endif;
056900110121
057000110121         // -?Ordinamento dati in stampa (per baia)?
057100110121         //  ?[Ordinamento per percentuale/baia]?
057200110121         reset  $Comodo;
057300110121
057400110121         For  xx = 1  To  %elem($Pep);
057500110121
057600110121           if  $Pep(xx) = *loval  and  $PepRec(xx) = *zero;
057700110121             leave;
057800110121           endif;
057900110121
058000110121           if  $PepErr(xx) = *zero;
058100110121             iter;
058200110121           endif;
058300110121
058400110121           clear  wComodo_ds;
058500110121           wXx     = xx;
058600110121           wPep    = $Pep(xx);
058700110121           wPepRec = $PepRec(xx);
058800110121           // -?Calcolo percentuale con caratteri errati (per baia)?
058900110121           if  $PepRec(xx) > *zero;
059000110121             eval(h) wPerc = ($PepErr(xx) * 100) / $PepRec(xx);
059100110121           else;
059200110121             clear   wPerc;
059300110121           endif;
059400110121
059500110121           // -?Memorizzazione elementi base per ordinamento?
059600110121           if  wPepRec >= D04min2  and  wPerc >= D04prc2;
059700110121             $Comodo(xx) = wComodo_ds;
059800110121           endif;
059900110121
060000110121         EndFor;
060100110121
060200110121         // -?Ordinamento dati da stampare?
060300110121         sorta  $Comodo;
060400110121
060500110121         // -?Verifica se NESSUNA baia da stampare?
060600110121         If  $Comodo(1) = *loval;
060700110121           leavesr;
060800110121         EndIf;
060900110121
061000110121         // -?Salto di una riga a "rottura" di filiale?
061100110121         if  Not  $First;
061200110121           write SS04space;
061300110121         endif;
061400110121
061500110121         // -?Valorizzazione campi di output?
061600110121         PDCfil = SAVfil;
061700110121
061800110121         // -?Calcolo percentuale con caratteri errati (per filiale)?
061900110121         if  PDCtotRec > *zero;
062000110121           eval(h) PDCtotPrc = (PDCtotErr * 100) / PDCtotRec;
062100110121         else;
062200110121           clear   PDCtotPrc;
062300110121         endif;
062400110120
062500110121         // -?Ciclo di stampa dati (per baia)?
062600110121         For  xx = 1  To  %elem($Comodo);
062700110121
062800110121           if  $Comodo(xx) = *loval;
062900110121             leave;
063000110121           endif;
063100110121
063200110121           wComodo_ds = $Comodo(xx);
063300110120
063400110121           PDCpep    = $Pep(wXx);
063500110121           PDCpepRec = $PepRec(wXx);
063600110121           PDCpepErr = $PepErr(wXx);
063700110121           // -?Calcolo percentuale con caratteri errati (per baia)?
063800110121           if  PDCpepRec > *zero;
063900110121             eval(h) PDCpepPrc = (PDCpepErr * 100) / PDCpepRec;
064000110121           else;
064100110121             clear   PDCpepPrc;
064200110121           endif;
064300110121
064400110121           // -?Test OverFlow per stampa testata?
064500110121           if  *in25;
064600110121             write  SS042txt;
064700110121             write  SS04tChaEr;
064800110121             *in25 = *off;
064900110121           endif;
065000110121
065100110121           // -?Stampa del dettaglio per baia?
065200110121           write  SS04dChaEr;
065300110121           $First = *off;
065400110120
065500110121         EndFor;
065600110120
065700110120       ENDSR;
065800110114
065900110114       //--------------------------------------------------------------
066000110114       //?Stampa segnalazione dell'errore rilevato.                    ?
066100110114       //--------------------------------------------------------------
066200110114       BEGSR  sr_PrintErr;
066300110114
066400110114         // -?Stampa del Dump?
066500110114         Dump(A);
066600110114
066700110114         // -?Stampa del Job-Log?
066800110114         Qcmd = 'DSPJOBLOG job(*) output(*print)';
066900110114         exsr  sr_ExecCmd;
067000110114
067100110114         // -?Chiusura del programma?
067200110114         exsr  sr_RoutEnd;
067300110114
067400110114       ENDSR;
067500110114
067600110114       //--------------------------------------------------------------
067700110114       //?Esecuzione del comando (già impostato).                      ?
067800110114       //--------------------------------------------------------------
067900110114       BEGSR  sr_ExecCmd;
068000110114
068100110114         clear Qcap0100;
068200110114         Qcabcsdh = *off;
068300110114         Qcapa    = *off;
068400110114         Qcacmdss = *off;
068500110114         Qcaerved = *allX'00';
068600110114
068700110114         clear Qusec;
068800110114         Qusbprv  = %size(Qusec);
068900110114
069000110114         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
069100110114                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
069200110114                           0 : 0 : Qusec);
069300110114
069400110114         //if  Qusei <> *blank;
069500110114         //  ...
069600110114         //endif;
069700110114
069800110114       ENDSR;
069900110114
070000110114       //--------------------------------------------------------------
070100110114       //?Operazioni finali.                                           ?
070200110114       //--------------------------------------------------------------
070300110114       BEGSR  sr_RoutEnd;
070400110120
070500110120         // -?Apertura archivio tabelle "MVI" di Sede?
070600110120         open  TNTBE01L;
070700110120
070800110120         // -?Aggiornamento tab. "MVI"/"FLTR7"?
070900110120         clear  k05tntbe01;
071000110120         k_TBEcod = 'MVI';
071100110120         k_TBEke1 = 'FLTR7';
071200110120         chain  %kds( k05tntbe01 )  TNTBE000;
071300110120         If  %found(TNTBE01L);
071400110120           clear  TBEatb;
071500110120           TBEuni = DmviFLTR7;
071600110120           update  TNTBE000;
071700110120         Else;
071800110120           clear  TNTBE000;
071900110120           TBEcod = k_TBEcod;
072000110120           TBEke1 = k_TBEke1;
072100110120           TBEapl = 'MS';
072200110120           TBEuni = DmviFLTR7;
072300110120           write  TNTBE000;
072400110120         EndIf;
072500110120
072600110120         // -?Chiusura archivi?
072700110120         close  TNTBE01L;
072800110120         close  WFTR7_1D;
072900110121         close  TNSS04DISP;
073000110121         close  TNSS04ERRP;
073100110114
073200110117         // -?Uscita dal *pgm?
073300110114         return;
073400110114
073500110114       ENDSR;
073600110114
073700110114      /end-free
