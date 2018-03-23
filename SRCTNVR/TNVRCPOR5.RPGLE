000100140721       //==============================================================
000200140721       //?TNVRCPOR5 * Nuovo WrkF clienti potenziali                    ?
000300140721       //==============================================================
000400140721
000500140721       //--------------------------------------------------------------
000600140721       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140721       //--------------------------------------------------------------
000800140721
000900140721     /*PRM  dbgview(*source)
001000140721     /*END
001100140721
001200140721       //--------------------------------------------------------------
001300140721       //?Specifiche di controllo.                                     ?
001400140721       //--------------------------------------------------------------
001500140721
001600140721     h decedit('0,') datedit(*ymd.) option(*nodebugio)
001700140721
001800140721       //--------------------------------------------------------------
001900140721       //?Dichiarazione file.                                          ?
002000140721       //--------------------------------------------------------------
002100140721
002200140721       // -?Anagrafica Clienti Potenziali?
002300140722     fTNCPO01L  Uf   e           k disk
002400140721
002500140721       // -?Trattative a Clienti?
002600140722     fTNVIS05L  if   e           k disk
002700140721
002800140721       // -?Anagrafica P.d.C.?
002900140721     fCNACO16L  if   e           k disk    rename(CNACO000:CNACO016)
003000140722     fCNACO00F  Uf   e           k disk    prefix(x)
003100140721
003200140721       // -?Anagrafica P.d.C. - Provvisoria per Tariffe?
003300140721     fTFACO16L  if   e           k disk    rename(CNACO000:TFACO016)
003400140722     fTFACO00F  Uf   e           k disk    prefix(x)
003500140721     f                                     rename(CNACO000:TFACO000)
003600140721
003700140721       // -?Anagrafica Particolarità Clienti?
003800140721     fCNCLP00F  if   e           k disk
003900140721       // -?Anagrafica Particolarità Clienti per Tariffe?
004000140721     fTFCLP00F  if   e           k disk    rename(CNCLP000:TFCLP000)
004100140721       // -?Anagrafica Indirizzi e Condizioni di Pagamento?
004200140721     fCNIND00F  if   e           k disk
004300140721       // -?Anagrafica Indirizzi e Condizioni di Pagamento per Offerte?
004400140721     fTFIND00F  if   e           k disk    rename(CNIND000:TFIND000)
004500140721
004600140721       // -?Work-File Anagrafico Clienti Potenziali - da CNACO?
004700140722     fWFCPOCNF  Uf A e           k disk    rename(TNCPO000:WFCPOCN0)
004800140722     f*//                                  usropn
004900140721       // -?Work-File Anagrafico Clienti Potenziali - da TFACO?
005000140722     fWFCPOTFF  Uf A e           k disk    rename(TNCPO000:WFCPOTF0)
005100140722     f*//                                  usropn
005200140721
005300140721       //--------------------------------------------------------------
005400140721       //?Definizione costanti.                                        ?
005500140721       //--------------------------------------------------------------
005600140721
005700140722       // -?Costanti per anagrafica clienti?
005800140722     d c_kut           c                   const(1)
005900140722     d c_kcc           c                   const(0151)
006000140722
006100140721       // -?Costante per controllo "caratteri solo numerici"?
006200140721     d c_Digits        c                   const('0123456789')
006300140722
006400140722       // -?Numero massimo di elementi per le schiere sottostanti?
006500140724     d c_MaxElem       c                   const(750)
006600140721
006700140721       //--------------------------------------------------------------
006800140721       //?Definizione schiere.                                         ?
006900140721       //--------------------------------------------------------------
007000140721
007100140722       // -?Province del cliente?
007200140722     d sk_PRValt       s              2    dim(15)  ctdata  perrcd(15)
007300140722
007400140722       // -?Codice Cliente?
007500140722     d sk_Ksc          s                   dim(c_MaxElem)  like(ACOksc)
007600140722       // -?Blocco cliente?
007700140722     d sk_Blo          s                   dim(c_MaxElem)  like(ACOabl)
007800140722       // -?Sottoconto intestazione Fattura?
007900140722     d sk_Scf          s                   dim(c_MaxElem)  like(CLPscf)
008000140722       // -?Data Ultima Spedizione Fatturata?
008100140722     d sk_Dus          s            + 2    dim(c_MaxElem)  like(CLPdus)
008200140722       // -?Codice Commerciale?
008300140722     d sk_Cmm          s                   dim(c_MaxElem)  like(VIScmm)
008400140722       // -?Data Visita?
008500140722     d sk_Dvi          s                   dim(c_MaxElem)  like(VISdvi)
008600140722
008700140722       // -?POTENZIALE: Codice Cliente?
008800140722     d sk_pKsc         s                   dim(c_MaxElem)  like(ACOksc)
008900140722       // -?POTENZIALE: Blocco cliente?
009000140722     d sk_pBlo         s                   dim(c_MaxElem)  like(ACOabl)
009100140722       // -?POTENZIALE: Sottoconto intestazione Fattura?
009200140722     d sk_pScf         s                   dim(c_MaxElem)  like(CLPscf)
009300140722       // -?POTENZIALE: Data Ultima Spedizione Fatturata?
009400140722     d sk_pDus         s            + 2    dim(c_MaxElem)  like(CLPdus)
009500140721
009600140721       //--------------------------------------------------------------
009700140721       //?Definizione aree dati.                                       ?
009800140721       //--------------------------------------------------------------
009900140721
010000140721       // -?Dati utente?
010100140721     d §AzUte        e ds                  extname(AZUTE00F)
010200140721     d                                     dtaara
010300140721     d §DatiUte      e ds                  extname(dDatiUte)
010400140721     d                                     dtaara
010500140721
010600140721       //--------------------------------------------------------------
010700140721       //?Definizione strutture dati.                                  ?
010800140721       //--------------------------------------------------------------
010900140721
011000140722       // -?Parametri?
011100140721     d KPJBA         e ds
011200140721
011300140721       //--------------------------------------------------------------
011400140721       //?Definizione variabili globali.                               ?
011500140721       //--------------------------------------------------------------
011600140721
011700140722       // -?Indici di schiera?
011800140722     d kk              s              5  0 inz
011900140722     d yy              s              5  0 inz
012000140722     d SavYY           s              5  0 inz
012100140722     d SavKK           s              5  0 inz
012200140722     d IndX            s              3  0 inz
012300140722
012400140722       // -?Flag Booleani?
012500140722     d $Registrato     s               n   inz
012600140722
012700140722       // -?Campi di lavoro?
012800140722     d wDataSYS        s               d   inz(*sys) datfmt(*iso)
012900140722     d wTipo           s              1    inz
013000140722     d wUnicoBlo       s              1    inz
013100140722     d wUnicoScf       s                   inz  like(CLPscf)
013200140722     d wDUS            s            + 2    inz  like(CLPdus)
013300140722     d SavCPO          s                   inz  like(CPOcpo)
013400140724     d SavDUS          s            + 2    inz  like(CLPdus)
013500140722     d SavDVI          s                   inz  like(VISdvi)
013600140722     d kKSC            s                   inz  like(ACOksc)
013700140722     d kSCF            s                   inz  like(CLPscf)
013800140721
013900140721       //--------------------------------------------------------------
014000140721       //?Definizione prototipi procedure.                             ?
014100140721       //--------------------------------------------------------------
014200140721
014300140722       // -?Pretrullo TISI95R (Controlla CAP/Località)?
014400140722     d FNLV13ds      e ds                  inz
014500140722     d TISI95ds      e ds                  inz
014600140721     d FNLV13R         pr                  extpgm('FNLV13R')
014700140722     d   KPJBA                             likeds(KPJBA)
014800140722     d   FNLV13ds                          likeds(FNLV13ds)
014900140722     d   TISI95ds                          likeds(TISI95ds)
015000140721
015100140721       //--------------------------------------------------------------
015200140721       //?Definizione key-list.                                        ?
015300140721       //--------------------------------------------------------------
015400140721
015500140721
015600140721       //--------------------------------------------------------------
015700140721       //?M A I N - L I N E                                            ?
015800140721       //--------------------------------------------------------------
015900140721
016000140721     c*//  *Entry        plist
016100140721     c*//                parm                    KPJBA
016200140721
016300080212      /free
016400140721
016500140721       *inLR = *on;
016600080212
016700140722       //?1) registro potenziali con anagrafica clienti?
016800140722       //   ?-> indirizzo completo, commerciale, cat.merceologica fil.?
016900140722       exsr  sr_Elab_Clienti;
017000080211
017100140722       //?2) registro potenziali con anagrafica clienti provvisoria?
017200140722       //   ?non legata a clienti esistenti?
017300140722       //   ?-> indirizzo completo, commerciale, cat.merceologica fil.?
017400140722       exsr  sr_Elab_CliPROV;
017500080215
017600140722       //--------------------------------------------------------------
017700140722       //?1) Registro potenziali in anagrafica clienti                 ?
017800140722       //?   -> indirizzo completo, commerciale, cat.merceologica fil. ?
017900140722       //--------------------------------------------------------------
018000140722       BEGSR  sr_Elab_Clienti;
018100140722
018200140722         wTipo = 'C';
018300140722
018400140722         clear  sk_Ksc;
018500140722         clear  sk_Scf;
018600140722         clear  sk_Dus;
018700140722         clear  sk_Blo;
018800140722         clear  kk;
018900140722         clear  yy;
019000140722
019100140722         clear  SavCPO;
019200140722
019300140722         ACOlib = 1;
019400140722         setll  ACOlib  CNACO016;
019500140722         read  CNACO016;
019600140722
019700140722         DoW  NOT %eof(CNACO16L);
019800140722
019900140722           IF  ACOlib <> SavCPO;
020000140722
020100140722             // -?Memorizzazione dati del Potenziale precedente?
020200140722             if  SavCPO > *zero;
020300140722               exsr  sr_CambioPOT;
020400140722               clear  SavCPO;
020500140722             endif;
020600140722
020700140722             // -?Elaborazione nuovo Potenziale?
020800140722             chain  ACOlib  TNCPO000;
020900140722             If  NOT  %found(TNCPO01L);
021000140722               chain  (c_kut : c_kcc : ACOksc)  CNACO000;
021100140722               clear  ACOlib;
021200140722               clear  xACOlib;
021300140722               clear  xACOdtr;
021400140722               clear  xACOftr;
021500140722               //_______________
021600140722               UPDATE  CNACO000;
021700140722               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
021800140722             Else;
021900140722               SavCPO = ACOlib;
022000140722               clear  sk_Ksc;
022100140722               clear  sk_pKsc;
022200140722               clear  sk_Scf;
022300140722               clear  sk_pScf;
022400140722               clear  sk_Dus;
022500140722               clear  sk_pDus;
022600140722               clear  sk_Blo;
022700140722               clear  sk_pBlo;
022800140722               clear  kk;
022900140722               clear  yy;
023000140722             EndIf;
023100140722
023200140722           ENDIF;
023300140722
023400140722           IF  ACOlib > *zero;
023500140722
023600140722             kk +=1;
023700140722             sk_Ksc(kk) = ACOksc;
023800140722             sk_Blo(kk) = ACOabl;
023900140722             chain  (ACOkut : ACOkcc : ACOksc)  CNCLP000;
024000140722
024100140722             If  %found(CNCLP00F);
024200140722
024300140722               sk_Scf(kk) = CLPscf;
024400140722               if  CLPdus > *zero;
024500140724                 if  CLPdus > 400000;
024600140722                   wDUS = 19000000;
024700140722                 else ;
024800140722                   wDUS = 20000000;
024900140722                 endif;
025000140722                 wDUS += CLPdus;
025100140722               endif;
025200140724               //sk_Dus(kk) = %int( %subst( %editc( wDUS : 'X' ) : 1 : 6 ) );
025300140724               sk_Dus(kk) = wDUS;
025400140722
025500140722             EndIf;
025600140722
025700140722             chain  (ACOkut : ACOkcc : ACOksc)  CNIND000;
025800140722             //if  Not %found(CNIND00F);     ?(impossibile)?
025900140722             //  ...
026000140722             //endif;
026100140722
026200140722             // -?Se stessa provincia del CPO memorizzo a parte?
026300140722             //  ?(nelle schiere SK_Pxxx)?
026400140722             if  INDprv <> CPOprv;
026500140722               IndX = %lookup( INDprv : sk_PRValt );
026600140722               if IndX > *zero;
026700140722                 exsr  sr_CallTISIind;
026800140722               else;
026900140722                 O95err = 'E';
027000140722               endif;
027100140722             endif;
027200140722
027300140722             If  (INDprv = CPOprv)  or
027400140722                 (O95err = *blank  and  (CPOprv = O95prv  or
027500140722                                         CPOprv = O95pra));
027600140722               yy += 1;
027700140722               sk_pKsc(yy) = sk_Ksc(kk);
027800140722               sk_pScf(yy) = sk_Scf(kk);
027900140722               sk_pDus(yy) = sk_Dus(kk);
028000140722               sk_pBlo(yy) = sk_Blo(kk);
028100140722             EndIf;
028200140722
028300140722           ENDIF;
028400140722
028500140722           read  CNACO016;
028600140722
028700140722         EndDo;
028800140722
028900140722       ENDSR;
029000140722
029100140722       //--------------------------------------------------------------
029200140722       //?2) Registro potenziali in anagrafica clienti provvisoria     ?
029300140722       //?   non legata a clienti esistenti                            ?
029400140722       //?   -> indirizzo completo, commerciale, cat.merceologica fil. ?
029500140722       //--------------------------------------------------------------
029600140722       BEGSR  sr_Elab_CliPROV;
029700140722
029800140722         wTipo = 'P';
029900140722
030000140722         clear  sk_Ksc;
030100140722         clear  sk_Scf;
030200140722         clear  sk_Dus;
030300140722         clear  sk_Dvi;
030400140722         clear  sk_Cmm;
030500140722         clear  kk;
030600140722
030700140722         clear  SavCPO;
030800140722
030900140722         ACOlib = 1;
031000140722         setll  ACOlib  TFACO016;
031100140722         read  TFACO016;
031200140722
031300140722         DoW  NOT %eof(TFACO16L);
031400140722
031500140722           chain  ACOksc  TNVIS000;
031600140722
031700140722           IF  VISksc = *zero;
031800140722
031900140722             // -?Se già cliente lo salto?
032000140722             chain  ACOlib  CNACO016;
032100140722
032200140722             If  NOT %found(CNACO16L);
032300140722
032400140722               If  ACOlib <> SavCPO;
032500140722
032600140722                 // -?Memorizzazione dati del Potenziale precedente?
032700140722                 if  SavCPO > *zero;
032800140722                   exsr  sr_CambioPOTprov;
032900140722                   clear  SavCPO;
033000140722                 endif;
033100140722
033200140722                 // -?Elaborazione nuovo Potenziale?
033300140722                 chain  ACOlib  TNCPO000;
033400140722                 If  NOT  %found(TNCPO01L);
033500140722                   chain  (c_kut : c_kcc : ACOksc)  TFACO000;
033600140722                   clear  ACOlib;
033700140722                   clear  xACOlib;
033800140722                   clear  xACOdtr;
033900140722                   clear  xACOftr;
034000140722                   //_______________
034100140722                   UPDATE  TFACO000;
034200140722                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
034300140722                 Else;
034400140722                   SavCPO = ACOlib;
034500140724                   clear  sk_Ksc;
034600140724                   clear  sk_Dvi;
034700140724                   clear  sk_Cmm;
034800140722                   clear  kk;
034900140722                 EndIf;
035000140722
035100140722               EndIf;
035200140722
035300140722               if ACOlib > *zero;
035400140722                 kk += 1;
035500140724                 sk_Ksc(kk) = ACOksc;
035600140724                 sk_Dvi(kk) = VISdvi;
035700140724                 sk_Cmm(kk) = VIScmm;
035800140722               endif;
035900140722
036000140722             EndIf;
036100140722
036200140722           ENDIF;
036300140722
036400140722           read  TFACO016;
036500140722
036600140722         EndDo;
036700140722
036800140722       ENDSR;
036900140722
037000140722       //--------------------------------------------------------------
037100140722       //?Scrittura potenziale nel work-file WFCPOCNF.                 ?
037200140722       //--------------------------------------------------------------
037300140722       BEGSR  sr_CambioPOT;
037400140722
037500140722         $Registrato = *off;
037600140722
037700140722
037800140722         // -?Se rapporto 1 a 1 cioè un'unica anagrafica legata al?
037900140722         //  ?potenziale => la aggiorno e la registro nel WF?
038000140722
038100140722         IF  kk = 1;
038200140724           kKSC = sk_Ksc(kk);
038300140724           kSCF = sk_Scf(kk);
038400140722           exsr  sr_WriteWFCN;
038500140722         ENDIF;
038600140722
038700140722
038800140722         // -?Se più codici legati, guardo alla provincia: se unica o?
038900140722         //  ?uno solo non bloccato => registro nel WF?
039000140722
039100140722         IF  Not $Registrato;
039200140722
039300140722           If   yy = 1;
039400140722
039500140722             kKSC = sk_pKsc(yy);
039600140722             kSCF = sk_pScf(yy);
039700140722             exsr  sr_WriteWFCN;
039800140722
039900140722           Else;
040000140722
040100140722             clear  wUnicoBlo;
040200140722             yy = 1;
040300140722
040400140722             DoW  sk_pKsc(yy) > *zero  and  wUnicoBlo <> '9';
040500140722               if  sk_pBlo(yy) = *blank;
040600140722                 if  wUnicoBlo = *blank;
040700140722                   wUnicoBlo = 'N';
040800140722                   SavYY     = yy;
040900140722                 else;
041000140722                   wUnicoBlo = '9';
041100140722                 endif;
041200140722               endif;
041300140722               yy += 1;
041400140722             EndDo;
041500140722
041600140722             if  wUnicoBlo = 'N';
041700140722               kKSC = sk_pKsc(SavYY);
041800140722               kSCF = sk_pScf(SavYY);
041900140722               exsr  sr_WriteWFCN;
042000140722             endif;
042100140722
042200140722           EndIf;
042300140722
042400140722         ENDIF;
042500140722
042600140722
042700140722         // -?Se più codici legati (e neanche la provincia è unica),?
042800140722         //  ?guardo al sottoconto int. fattura: se è unico, uno dei?
042900140722         //  ?codici memorizzati e non bloccato => registro nel WF?
043000140722
043100140722         IF  Not $Registrato;
043200140722
043300140722           wUnicoScf = sk_Scf(1);
043400140722           kk = 2;
043500140724           DoW  sk_Scf(kk) > *zero  and  wUnicoScf > *zero;
043600140724             if  wUnicoScf <> sk_Scf(kk);
043700140722               // -?Trovato un altro sottoconto intestazione fattura?
043800140722               clear wUnicoScf;
043900140722             endif;
044000140722             kk += 1;
044100140722           EndDo;
044200140722
044300140722           If  wUnicoScf > *zero;
044400140722             IndX = %lookup( wUnicoScf : sk_Ksc );
044500140722             if  IndX > *zero;
044600140722               // -?Verifico che non sia bloccato?
044700140722               chain  ( c_kut : c_kcc : wUnicoScf )  CNACO000;
044800140722               if  %found(CNACO00F)  and  ACOabl = *blank;
044900140722                 kKSC = wUnicoScf;
045000140722                 kSCF = *zero;
045100140722                 exsr  sr_WriteWFCN;
045200140722               endif;
045300140722             endif;
045400140722           EndIf;
045500140722
045600140722         ENDIF;
045700140722
045800140722
045900140722         // -?Atrimenti l'ultimo che ha spedito con la stessa provincia?
046000140722         //  ?di TNCPO se trovato, altrimenti l'ultimo in assoluto?
046100140722
046200140722         IF  Not $Registrato;
046300140722
046400140722           IF sk_pKsc(2) > *zero;
046500140722
046600140722             SavDUS = sk_pDus(1);
046700140722             SavYY  = 1;
046800140722             yy     = 2;
046900140722
047000140722             DoW  sk_pDus(yy) > *zero;
047100140722
047200140722               If  sk_pDus(yy) > SavDUS;
047300140722                 SavDUS = sk_pDus(yy);
047400140722                 SavYY  = yy;
047500140722               Else;
047600140722                 // -?Se uguali tengo il primo non bloccato?
047700140722                 if  sk_pDus(yy)    =  SavDUS  and
047800140722                     sk_pBlo(yy)    =  *blank  and
047900140722                     sk_pBlo(SavYY) <> *blank;
048000140722                   SavDUS = sk_pDus(yy);
048100140722                   SavYY  = yy;
048200140722                 endif;
048300140722               EndIf;
048400140722               yy += 1;
048500140722
048600140722             EndDo;
048700140722
048800140722             kKSC = sk_pKsc(SavYY);
048900140722             kSCF = sk_pScf(SavYY);
049000140722             exsr  sr_WriteWFCN;
049100140722
049200140722           ELSE;
049300140722
049400140722             SavDUS = sk_Dus(1);
049500140722             SavKK  = 1;
049600140722             kk     = 2;
049700140722
049800140722             DoW  sk_Dus(kk) > *zero;
049900140722
050000140722               If  sk_dus(kk) > SavDUS;
050100140722                 SavDUS = sk_Dus(kk);
050200140722                 SavKK  = kk;
050300140722               Else;
050400140722                 // -?Se uguali tengo il primo non bloccato?
050500140722                 if  sk_Dus(kk)    =  SavDUS  and
050600140722                     sk_Blo(kk)    =  *blank  and
050700140722                     sk_Blo(SavKK) <> *blank;
050800140722                   SavDUS = sk_Dus(kk);
050900140722                   SavKK  = kk;
051000140722                 endif;
051100140722               EndIf;
051200140722               kk += 1;
051300140722
051400140722             EndDo;
051500140722
051600140722             kKSC = sk_Ksc(SavKK);
051700140722             kSCF = sk_Scf(SavKK);
051800140722             exsr  sr_WriteWFCN;
051900140722
052000140722           ENDIF;
052100140722
052200140722         ENDIF;
052300140722
052400140722       ENDSR;
052500140722
052600140722       //--------------------------------------------------------------
052700140722       //?Scrittura potenziale nel work-file WFCPOTFF.                 ?
052800140722       //--------------------------------------------------------------
052900140722       BEGSR  sr_CambioPOTprov;
053000140722
053100140722         clear  kSCF;
053200080219
053300140722
053400140722         // -?Se rapporto 1 a 1 cioè un'unica anagrafica legata al?
053500140722         //  ?potenziale => la aggiorno e la registro nel WF?
053600140722         IF   kk = 1;
053700140722
053800140722           kKSC = sk_Ksc(kk);
053900140722           exsr  sr_WriteWFCN;
054000140722
054100140722         ELSE;
054200140722
054300140722           // -?Se più codici legati, guardo alla data visita => prendo?
054400140722           //  ?la più recente?
054500140722           SavDVI = sk_Dvi(1);
054600140722           SavKK  = 1;
054700140722           kk     = 2;
054800140722
054900140722           DoW  sk_Dvi(kk) > *zero;
055000140722             if  sk_Dvi(kk) > SavDVI;
055100140722               SavDVI = sk_Dvi(kk);
055200140722               SavKK  = kk;
055300140722             endif;
055400140722             kk += 1;
055500140722           EndDo;
055600080211
055700140722           kKSC = sk_Ksc(SavKK);
055800140722           exsr  sr_WriteWFCN;
055900140722         endif;
056000080211
056100140722       ENDSR;
056200080211
056300140722       //--------------------------------------------------------------
056400140722       //?Aggiornamento dati in TNCPO00F  e  scrittura in WFCPOcn/tfF. ?
056500140722       //--------------------------------------------------------------
056600140722       BEGSR  sr_WriteWFCN;
056700140722
056800140722         $Registrato = *on;
056900140722
057000140722         If  wTipo = 'C';
057100140722           chain(N) ( c_kut : c_kcc : kKSC )  CNACO000;
057200140722           chain    ( c_kut : c_kcc : kKSC )  CNIND000;
057300140722           chain    ( c_kut : c_kcc : kKSC )  CNCLP000;
057400140722         Else;
057500140722           chain(N) ( c_kut : c_kcc : kKSC )  TFACO000;
057600140722           chain    ( c_kut : c_kcc : kKSC )  TFIND000;
057700140722           chain    ( c_kut : c_kcc : kKSC )  TFCLP000;
057800140722         EndIf;
057900140722
058000140722         // -?Aggiorno l'indirizzo solo se le provincie corrispondono?
058100140722         if INDprv <> CPOprv;
058200140722           IndX = %lookup( INDprv : sk_PRValt );
058300140722           if IndX > *zero;
058400140722             exsr  sr_CallTISIind;
058500140722           else;
058600140722             O95err = 'E';
058700140722           endif;
058800140722         endif;
058900140722
059000140722         IF  (INDprv = CPOprv)  or
059100140722             (O95err = *blank  and  (CPOprv = O95prv  or
059200140722                                     CPOprv = O95pra));
059300140722
059400140722           %subst( CPOrst : 1 : 1 ) = 'A';
059500140722
059600140722           CPOrag = xACOrag;
059700140722           CPOvia = INDvia;
059800140722           CPOcit = INDcit;
059900140722           CPOcap = INDcae;
060000140722           CPOprv = INDprv;
060100140722           CPOnaz = INDsta;
060200140722           if  INDtel <> *blank;
060300140722             CPOtel = INDtel;
060400140722           endif;
060500140722           if  INDtlf <> *blank;
060600140722             CPOfax = INDtlf;
060700140722           endif;
060800140722
060900140722           // -?P.Iva e Cod.Fisc. li aggiorno se non c'è l'Int.Fattura?
061000140722           //  ?(solo per clienti)?
061100140722           CPOcdf = INDcdf;
061200140722           CPOpiv = INDiva;
061300140722
061400140722           if  kSCF > *zero  and  kSCF <> kKSC;
061500140722             chain  ( c_kut : c_kcc : kSCF )  CNIND000;
061600140722             if  %found(CNIND00F);
061700140722               CPOcdf = INDcdf;
061800140722               CPOpiv = INDiva;
061900140722             endif;
062000140722           endif;
062100140722
062200140722         ENDIF;
062300140722
062400140722
062500140722         // -?Categoria Merceologica?
062600140722         CPOsct = xACOitc;
062700140722
062800140722         CPOdtr = %dec( wDataSys : *iso );
062900140722
063000140722         // -?Aggiornamento TNCPO00F?
063100140722         //_______________
063200140722         UPDATE  TNCPO000;
063300140722         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063400140722
063500140722         If  wTipo = 'C';
063600140722           // -?Scrittura WFCPOCNF?
063700140722           //______________
063800140722           WRITE  WFCPOCN0;
063900140722           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064000140722         Else;
064100140722           // -?Scrittura WFCPOTFF?
064200140722           //______________
064300140722           WRITE  WFCPOTF0;
064400140722           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064500140722         EndIf;
064600140722
064700140722       ENDSR;
064800080220
064900140722       //--------------------------------------------------------------
065000140722       //?Controllo Indirizzo.                                         ?
065100140722       //--------------------------------------------------------------
065200140722       BEGSR  sr_CallTISIind;
065300080226
065400140722         clear  TISI95ds;
065500140722         I95tcn = '7';
065600140722         I95cap = INDcae;
065700140722         I95loc = INDcit;
065800140722         I95prv = INDprv;
065900140722         I95dat = %dec( wDataSYS : *iso );
066000140722
066100140722         clear  FNLV13ds;
066200140722         I13af0 = 'S';
066300140722         I13cnv = ' ';
066400140722         I13af1 = ' ';
066500140722         I13la3 = 'S';
066600140722
066700140722         FNLV13R ( kpjba : FNLV13ds : TISI95ds );
066800080226
066900140722       ENDSR;
067000071130
067100071116      /end-free
067200140722
067300140722       //--------------------------------------------------------------
067400140722       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
067500140722       //--------------------------------------------------------------
067600140722
067700140722** --?sk_PrvAlt?-?Province del cliente?
067800080226FOFCOTSSORNUCAOGCIVSSVCNIMPUPS
