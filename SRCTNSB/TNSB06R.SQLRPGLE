000100110721       //---------------------------------------------------------------
000200110721       //?TNSB06R - Crea file elenco Pod Image per il cliente
000300110721       //---------------------------------------------------------------
000400090127
000500071210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040720
000700110721       //---------------------------------------------------------------
000800110721       //?Dichiarazione file.
000900110721       //---------------------------------------------------------------
001000110721
001100110721       // -?File bolle di sede
001200110721     fTitas30c  if   e           k disk
001300110721       // -?WrkF per creazione elenco Pod Image
001400110721     fWflac00f  o    e             disk    usropn
001500110721     f                                     extfile(wFLib)
001600110721
001700110721       //--------------------------------------------------------------
001800110721       //?Definizione costanti.                                        ?
001900110721       //--------------------------------------------------------------
002000110721
002100110721       //--------------------------------------------------------------
002200110721       //?Definizione schiere.                                         ?
002300110721       //--------------------------------------------------------------
002400110721
002500110721       //--------------------------------------------------------------
002600110721       //?Definizione aree dati.                                       ?
002700110721       //--------------------------------------------------------------
002800110721
002900110721       // -?Dati utente?
003000110721     d §AzUte        e ds                  extname(AZUTE00F)
003100110721     d                                     dtaara
003200110721     d §DatiUte      e ds                  extname(dDatiUte)
003300110721     d                                     dtaara
003400110721
003500110721       //--------------------------------------------------------------
003600110721       //?Definizione strutture dati.                                  ?
003700110721       //--------------------------------------------------------------
003800110721
003900110721       // -?Status ds?
004000110721     d Status         sds
004100110721     d  SDSpgm           *proc
004200110721     d  SDSjob               244    253
004300110721     d  SDSusr               254    263
004400110721     d  SDSnbr               264    269s 0
004500110721
004600110721       // -?Parametri ricevuti
004700110721     d KPJBA         e ds
004800110721     d TNSB06ds      e ds
004900110721
005000110721       // -?Parametri per Reperimento dati utente?
005100110721     d TIBS34ds      e ds
005200110721
005300110721       // -?File immagini estratte
005400110721     d TILAC00F      e ds                  extname(TILAC00F)
005500110721
005600110721       //--------------------------------------------------------------
005700110721       //?Definizione variabili globali.                               ?
005800110721       //--------------------------------------------------------------
005900110721
006000110721       // -?Flags booleani
006100110721     d $End            s               n   inz(*off)
006200110721     d $Fine           s               n   inz(*off)
006300110721
006400110721       // -?Nome libreria del work-file WFLAC00F?
006500110721     d wFLib           s             21    inz
006600110721
006700110721       // -?Campi per richiamo QCMDEXC
006800110721     d Qcmd            s            128    inz
006900110721
007000110721       // -?Stringa SQL da eseguire
007100110721     d wSQL            s           2048    Varying        inz
007200110721
007300110721       // -?Campi di comodo
007400110721     d wprv            s                   like(TASprd)
007500110801     d wcostante       s             10    inz('S09')
007600110721
007700110721       // -?Campi per chiamata a pgm esterno
007800110721     d  iJob           s             26a
007900110721     d  iOra           s              6a
008000110721     d  iLib           s             10a
008100110721     d  iFile          s             10a
008200110721     d  iMbr           s             10a
008300110721     d  iDir           s             30a
008400110721     d  iNFile         s            128a
008500110721     d  iEste          s              5a
008600110721     d  oEsito         s              1s 0
008700110721
008800110721       //--------------------------------------------------------------
008900110721       //?Definizione prototipi.
009000110721       //--------------------------------------------------------------
009100110721
009200110721       // -?Esecuzione comando di sistema
009300110721      /copy gaitrasrc/srcProtoPR,QCMDEXC
009400110721
009500110721       // -?Reperimento dati utente?
009600110721      /copy gaitrasrc/srcProtoPR,TIBS34R
009700110721
009800110721       //--------------------------------------------------------------
009900110721       //?Definizione procedure.
010000110721       //--------------------------------------------------------------
010100110721
010200110721       // -?Pgm creazione file CSV
010300110721     d TRBMF2N0R       pr                  extpgm('TRBMF2N0R')
010400110721     d  iJob                         26a
010500110721     d  iOra                          6a
010600110721     d  iLib                         10a
010700110721     d  iFile                        10a
010800110721     d  iMbr                         10a
010900110721     d  iDir                         30a
011000110721     d  iNFile                      128a
011100110721     d  iEste                         5a
011200110721     d  oEsito                        1s 0
011300110721
011400110721       //--------------------------------------------------------------
011500110721       //?Definizione key-list.                                        ?
011600110721       //--------------------------------------------------------------
011700110721
011800110721       //--------------------------------------------------------------
011900110721       //?M A I N - L I N E                                            ?
012000110721       //--------------------------------------------------------------
012100110721
012200110721     c     *Entry        plist
012300110721     c                   parm                    KPJBA
012400110721     c                   parm                    TNSB06DS
012500110721
012600110721      /free
012700110721
012800110721       // -?Operazioni iniziali?
012900110721       exsr RoutInz;
013000110721
013100110721       // -?Elaboro?
013200110721       IF  not $Fine;
013300110721         exsr Elabora;
013400110721       ENDIF;
013500110721
013600110721       // -?Operazioni finali?
013700110721       exsr RoutEnd;
013800110721
013900110721       //--------------------------------------------------------------
014000110721       //?Operazioni iniziali.                                         ?
014100110721       //--------------------------------------------------------------
014200110721       BEGSR  RoutInz;
014300110721
014400110721       // -?Reperimento dati job?
014500110721         exsr  DatiJob;
014600110721
014700110721       // -?Controllo i dati ricevuti?
014800110721         OSB06esito = 0;
014900110721         IF  ISB06ksu = 0 or ISB06idjob = *blanks or
015000110721             ISB06dir = *blanks;
015100110721           OSB06esito = 2;
015200110721           $Fine = *on;
015300110721           leavesr;
015400110721         ENDIF;
015500110721
015600110721       // -?Pulizia ed apertura WrkF?
015700110721         Qcmd = 'CRTDUPOBJ OBJ(WFLAC00F) FROMLIB(';
015800110721         IF  %subst(knsif : 7 : 1) = 'P';
015900110721           Qcmd = %trimr(Qcmd) + 'GAITRAAZP)';
016000110721         ELSE;
016100110721           Qcmd = %trimr(Qcmd) + 'GAITRAAZM)';
016200110721         ENDIF;
016300110721         Qcmd = %trimr(Qcmd) + ' OBJTYPE(*FILE) TOLIB(QTEMP)';
016400110721         callp(e) ExecuteCommand ( Qcmd : %size(Qcmd));
016500110721         IF  %error;
016600110721           Qcmd = 'CLRPFM QTEMP/WFLAC00F';
016700110721           ExecuteCommand ( Qcmd : %size(Qcmd));
016800110721         ENDIF;
016900110721         wFLib = 'QTEMP/WFLAC00F';
017000110721         open  WFLAC00F;
017100110721
017200110721         exec sql set option dynusrprf = *owner,
017300110721                             closqlcsr = *endmod;
017400110721
017500110721       ENDSR;
017600110721
017700110721       //--------------------------------------------------------------
017800110721       //?Reperimento Dati del job (Utente/Operativi).                 ?
017900110721       //--------------------------------------------------------------
018000110721       BEGSR  DatiJob;
018100110721
018200110721         in(E) §AzUte;
018300110721         IF  NOT %error;
018400110721           in(E) §DatiUte;
018500110721         ENDIF;
018600110721         IF  %error or RSut = *blanks;
018700110721           clear TIBS34ds;
018800110721           tibs34r ( tibs34ds );
018900110721           in §AzUte;
019000110721           in §DatiUte;
019100110721         ENDIF;
019200110721
019300110721       ENDSR;
019400110721
019500110721       //--------------------------------------------------------------
019600110721       //?Elaborazione.
019700110721       //--------------------------------------------------------------
019800110721       BEGSR  Elabora;
019900110721
020000110721         $End = *off;
020100110721
020200110721       // -?Leggo file TILAC00F?
020300110721         exec sql
020400110721         DECLARE TILAC cursor for
020500110721         SELECT LACaas, LAClnp, LACnrs, LACnsp, LACnim
020600110721         from TILAC00F
020700110721         WHERE LACidjob = :ISB06idjob and LACksu = :ISB06ksu
020800110721               and LACela not in('33', '34');
020900110721
021000110721         exec sql
021100110721           open TILAC;
021200110721           IF  sqlcode < 0;
021300110721             $End = *on;
021400110721           ENDIF;
021500110721
021600110721         DOW  not $End;
021700110721           exec sql
021800110721             FETCH next from TILAC into :LACaas, :LAClnp,
021900110721                                        :LACnrs, :LACnsp,
022000110721                                        :LACnim;
022100110721             IF  sqlcod = 100 or sqlcod < 0;
022200110721               $End = *on;
022300110721               leave;
022400110721             ENDIF;
022500110721
022600110721           exsr Scrivi;
022700110721
022800110721         ENDDO;
022900110721
023000110721         exec sql close TILAC;
023100110721
023200110721       // -?Chiudo il file di work?
023300110721         close WFLAC00F;
023400110721
023500110721       // -?Richiamo pgm che genera file CSV?
023600110721         exsr CreaFile;
023700110721
023800110721       ENDSR;
023900110721
024000110721       //--------------------------------------------------------------
024100110721       //?Scrive file di work.                                         ?
024200110721       //--------------------------------------------------------------
024300110721       BEGSR  Scrivi;
024400110721
024500110721       // -?Aggancio la bolla?
024600110721         clear wprv;
024700110721         TASaas  = %dec(LACaas:4:0);
024800110721         TASaas += 2000;
024900110721         TASlnp  = %dec(LAClnp:3:0);
025000110721         TASnrs  = %dec(LACnrs:2:0);
025100110721         TASnsp  = %dec(LACnsp:7:0);
025200110721         chain (TASaas:TASlnp:TASnrs:TASnsp) TITAS30C;
025300110721         IF  %found(TITAS30C);
025400110721           wprv = TASprd;
025500110721         ENDIF;
025600110721
025700110721       // -?Scrivo il file di Work?
025800110721         clear WFLAC000;
025900110721         WLACrec = %trim(LACnim) + '.TIF' + ';' +
026000110721                   %trim(wprv)      + ';' +
026100110721                   %trim(wcostante) + ';';
026200110721
026300110721         write WFLAC000;
026400110721
026500110721       ENDSR;
026600110721
026700110721       //--------------------------------------------------------------
026800110721       //?Crea File CSV.                                               ?
026900110721       //--------------------------------------------------------------
027000110721       BEGSR  CreaFile;
027100110721
027200110721       // -?Imposto i dati da passare al pgm?
027300110721          iJob = %trim(SDSjob) + %trim(SDSusr) + %trim(%editc(SDSnbr:'X'));
027400110721          iOra   = %editc(%dec(%time()):'X');
027500110721          iLib   = 'QTEMP';
027600110721          iFile  = 'WFLAC00F';
027700110721          clear iMbr;
027800110721          iDir   = ISB06dir;
027900110721          iNFile = 'ElencoPod_' + %char(%dec(%timestamp()));
028000110721          iEste  = '.CSV';
028100110721          clear oEsito;
028200110721
028300110721          TRBMF2N0R (iJob:iOra:iLib:iFile:iMbr:iDir:iNFile:iEste:oEsito);
028400110721
028500110721          IF  oEsito <> 0;
028600110721            OSB06esito = oEsito;
028700110721          ENDIF;
028800110721
028900110721       ENDSR;
029000110721
029100110721       //--------------------------------------------------------------
029200110721       //?Operazioni finali.                                           ?
029300110721       //--------------------------------------------------------------
029400110721       BEGSR  RoutEnd;
029500110721
029600110721         *inLR = *on;
029700110721
029800110721       // -?Uscita dal pgm?
029900110721         return;
030000110721
030100110721       ENDSR;
030200110721
030300110721      /end-free
