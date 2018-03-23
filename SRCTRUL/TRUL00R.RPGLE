000100110221       //==============================================================
000200110221       //?TRUL00R // Modifica Filiale associata al Profilo Utente.
000300110221       //==============================================================
000400110221
000500110221       //--------------------------------------------------------------
000600110221       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110221       //--------------------------------------------------------------
000800110221
000900110221     /*PRM dbgview(*source)
001000110221     /*END
001100110221
001200110221       //--------------------------------------------------------------
001300110221       //?Specifiche di controllo.                                     ?
001400110221       //--------------------------------------------------------------
001500110221
001600110221     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110221     h dftactgrp(*no)
001800110221     h bnddir('UBBNDDIR')
001900110221
002000110221       //--------------------------------------------------------------
002100110221       //?Dichiarazione file.                                          ?
002200110221       //--------------------------------------------------------------
002300110221
002400110221       // -?Profili Utente?
002500110221     fAZUTE02L  if   e           k disk
002600110221     f                                     rename( AZUTE000 : AZUTE002 )
002700110221     fAZUTE01L  Uf   e           k disk
002800110221
002900110221       //--------------------------------------------------------------
003000110221       //?Definizione costanti.                                        ?
003100110221       //--------------------------------------------------------------
003200110221
003300110221
003400110221       //--------------------------------------------------------------
003500110221       //?Definizione schiere.                                         ?
003600110221       //--------------------------------------------------------------
003700110221
003800110221
003900110221       //--------------------------------------------------------------
004000110221       //?Definizione aree dati.                                       ?
004100110221       //--------------------------------------------------------------
004200110221
004300110221
004400110221       //--------------------------------------------------------------
004500110221       //?Definizione strutture dati.                                  ?
004600110221       //--------------------------------------------------------------
004700110221
004800110221       // -?Status ds?
004900110221     d Status         sds
005000110224     d  SDSdta               191    198
005100110224     d  SDSjob               244    253
005200110221     d  SDSusr               254    263
005300110224     d  SDSjnr               264    269  0
005400110221
005500110221       //--------------------------------------------------------------
005600110221       //?Definizione variabili globali.                               ?
005700110221       //--------------------------------------------------------------
005800110221
005900110221       // -?Parametri ricevuti?
006000110224     d prm_Fil         s              3  0
006100110221
006200110221       //--------------------------------------------------------------
006300110221       //?Definizione prototipi procedure.                             ?
006400110221       //--------------------------------------------------------------
006500110221
006600110221       // -?Reperimento NETA sistema AS/400 corrente?
006700110221     d currSysNeta     s              8a   inz
006800110221      /copy gaitrasrc/srcProtoPr,UBRTVNETA
006900110224
007000110224       // -?Parametri API QUSROBJD (Retrieve Object Description)?
007100110224      /copy qSysInc/qRpgleSrc,QUSROBJD
007200110224      /copy GaiTraSrc/srcProtoPr,QUSROBJD
007300110221
007400110221       // -?Parametri API QCAPCMD (Process Commands)?
007500110221     d Qcmd            s           1024    inz  varying
007600110221      /copy qSysInc/qRpgleSrc,QCAPCMD
007700110221      /copy gaitrasrc/srcProtoPR,QCAPCMD
007800110221
007900110221       // -?Parametri gestione errori API.?
008000110221      /copy qSysInc/qRpgleSrc,QUSEC
008100110221
008200110221       //--------------------------------------------------------------
008300110221       //?Definizione key-list.                                        ?
008400110221       //--------------------------------------------------------------
008500110221
008600110221       // -?File AZUTE01L?
008700110221     d k01azute01    e ds                  extname(AZUTE01L : *key)
008800110221     d                                     prefix(k_)   inz
008900110221
009000110221       // -?File AZUTE02L?
009100110221     d k02azute02    e ds                  extname(AZUTE02L : *key)
009200110221     d                                     prefix(k_)   inz
009300110221
009400110221       //==============================================================
009500110221       //?M A I N - L I N E                                            ?
009600110221       //==============================================================
009700110221
009800110221       // -?Parametri (obbligatori):?
009900110221       //   ·?Filiale da impostare?
010000110221     c     *Entry        plist
010100110224     c                   parm                    prm_Fil
010200110221
010300110221      /free
010400110221
010500110221       // -?Operazioni iniziali?
010600110221       exsr  sr_RoutInz;
010700110221
010800110221       // -?Aggiornamento filiale del profilo utente in AZUTE01L?
010900110221       exsr  sr_UpdAZUTE;
011000110221
011100110221       // -?Operazioni finali?
011200110221       exsr  sr_RoutEnd;
011300110221
011400110221       //--------------------------------------------------------------
011500110221       //?Operazioni iniziali.                                         ?
011600110221       //--------------------------------------------------------------
011700110221       begsr  sr_RoutInz;
011800110221       //     ¯¯¯¯¯¯¯¯¯¯
011900110221
012000110221         *inLR = *on;
012100110221
012200110221         // -?Verifica del sistema AS/400 corrente?
012300110221         select;
012400110221           when  UBRTVNETA_Rtv( currSysNeta ) <> *zero;
012500110221             return;
012600110221           when  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
012700110221             return;
012800110221         endsl;
012900110224
013000110224         // -?Reperimento dati (libreria) del file AZUTE00F?
013100110224         clear QUSD0100;
013200110224         clear QUSEC;
013300110224         Qusbprv = %size(Qusec);
013400110224         RetrieveObjectDescription( QUSD0100 :
013500110224                                    %len(QUSD0100) :
013600110224                                    'OBJD0100' :
013700110224                                    'AZUTE00F  *LIBL     ':
013800110224                                    '*FILE' :
013900110224                                    qUsec );
014000110224         select;
014100110224           // -?Fallito il reperimento dati?
014200110224           when  qUsei <> *blank;
014300110224             *inH1 = *on;
014400110224             return;
014500110224           // -?Richiesta impostazione 046 (sede) in s.i. di filiale?
014600110224           when  %subst( qUsrL01 : 1 : 3 ) = 'FIL'  and  prm_Fil = 046;
014700110224             return;
014800110224         endsl;
014900110221
015000110221       endsr;
015100110221
015200110221       //--------------------------------------------------------------
015300110221       //?Aggiornamento filiale del profilo utente in AZUTE01L.        ?
015400110221       //--------------------------------------------------------------
015500110221       begsr  sr_UpdAZUTE;
015600110221       //     ¯¯¯¯¯¯¯¯¯¯¯
015700110221
015800110221         // -?Aggancio record da modificare?
015900110221         chain  (SDSusr)  AZUTE000;
016000110223
016100110223         select;
016200110224           // -?Utente NON trovato nel file AZUTE01L?
016300110223           when  Not  %found(AZUTE01L);
016400110224             return;
016500110223           // -?Utente già della filiale ricevuta?
016600110224           when  UTEfil = prm_Fil;
016700110224             unlock  AZUTE01L;
016800110414             // ·?NON esce per cancellare COMUNQUE le aree dati?
016900110414             //return;
017000110223         endsl;
017100110221
017200110414         IF  UTEfil <> prm_Fil;
017300110414
017400110414           // -?Verifica se chiave duplicata sulla nuova filiale;?
017500110414           //  ?eventualmente ricerca un codice utente disponibile?
017600110414           k_UTEfil = prm_Fil;
017700110414           k_UTEcdu = UTEcdu;
017800110414           setll  %kds( k02azute02 )  AZUTE002;
017900110414           if  %equal(AZUTE02L);
018000110414             exsr  sr_FndNewCDU;
018100110414           endif;
018200110221
018300110414           // -?Aggiornamento profilo utente?
018400110414           UTEfil = k_UTEfil;
018500110414           //______________
018600110414           update  AZUTE000;
018700110414           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
018800110414
018900110414         ENDIF;
019000110221
019100110221         // -?Cancellazione eventuali aree dati già in QTEMP?
019200110221         Qcmd = 'DLTDTAARA dtaara(QTEMP/§AZUTE)';
019300110221         exsr  sr_ExecCmd;
019400110330         if  Qusei <> *blank  and  Qusei <> 'CPF2105';
019500110330           Qcmd = 'SNDMSG msg(''Non riuscita la cancellazione +
019600110330                                dell''''area dati QTEMP/§AZUTE.'') +
019700110330                          tomsgq(' + %trimr( SDSjob ) + ') +
019800110330                          msgtype(*info)';
019900110330           exsr  sr_ExecCmd;
020000110330           return;
020100110330         endif;
020200110330
020300110221         Qcmd = 'DLTDTAARA dtaara(QTEMP/§DATIUTE)';
020400110221         exsr  sr_ExecCmd;
020500110330         if  Qusei <> *blank  and  Qusei <> 'CPF2105';
020600110330           Qcmd = 'SNDMSG msg(''Non riuscita la cancellazione +
020700110330                                dell''''area dati QTEMP/§DATIUTE.'') +
020800110330                          tomsgq(' + %trimr( SDSjob ) + ') +
020900110330                          msgtype(*info)';
021000110330           exsr  sr_ExecCmd;
021100110330           return;
021200110330         endif;
021300110221
021400110221       endsr;
021500110221
021600110221       //--------------------------------------------------------------
021700110221       //?Reperimento codice utente libero per la nuova filiale.       ?
021800110221       //--------------------------------------------------------------
021900110221       begsr  sr_FndNewCDU;
022000110221       //     ¯¯¯¯¯¯¯¯¯¯¯¯
022100110221
022200110224         k_UTEfil = prm_Fil;
022300110221
022400110221         For  k_UTEcdu = 1  To  999;
022500110221
022600110221           setll  %kds( k02azute02 )  AZUTE002;
022700110221
022800110221           if  Not  %equal(AZUTE02L);
022900110221             UTEcdu = k_UTEcdu;
023000110221             leave;
023100110221           endif;
023200110221
023300110221         EndFor;
023400110221
023500110221       endsr;
023600110221
023700110221       //--------------------------------------------------------------
023800110221       //?Operazioni finali.                                           ?
023900110221       //--------------------------------------------------------------
024000110221       begsr  sr_RoutEnd;
024100110221       //     ¯¯¯¯¯¯¯¯¯¯
024200110221
024300110224         // -?Invio *msg di chiusura (se avvenuta modifica)?
024400110224         Qcmd = 'SNDMSG msg(''Modificata la filiale del profilo +
024500110224                              utente in ' + %char(prm_Fil) + '.'') +
024600110224                        tomsgq(' + %trimr( SDSjob ) + ') +
024700110224                        msgtype(*info)';
024800110224         exsr  sr_ExecCmd;
024900110224
025000110221         // -?Uscita dal *pgm?
025100110221         return;
025200110221
025300110221       endsr;
025400110221
025500110221       //--------------------------------------------------------------
025600110221       //?Esecuzione del comando (già impostato).                      ?
025700110221       //--------------------------------------------------------------
025800110221       begsr  sr_ExecCmd;
025900110221       //     ¯¯¯¯¯¯¯¯¯¯
026000110221
026100110221         clear Qcap0100;
026200110221         Qcabcsdh = *off;
026300110221         Qcapa    = *off;
026400110221         Qcacmdss = *off;
026500110221         Qcaerved = *allX'00';
026600110221
026700110221         clear Qusec;
026800110221         Qusbprv  = %size(Qusec);
026900110221
027000110221         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
027100110221                           %size(Qcap0100) : 'CPOP0100' : *omit :
027200110221                           0 : 0 : Qusec);
027300110221
027400110221         //if  Qusei <> *blank;
027500110222         //  ...;
027600110221         //endif;
027700110221
027800110221       endsr;
027900110221
028000110221      /end-free
