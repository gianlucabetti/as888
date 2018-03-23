000100090609     H BNDDIR('UBRTVNETA')
000200090609     H NOMAIN
000300071128     ***********************************************************************************************
000400100827     **
000500090609     ** PARAMETRI DELLA SOTTOPROCEDURA UBUTISGI_RtvSrvSts
000600071128     **
000700071128     ** Posizione Uso Tipo      Descrizione
000800071128     ** --------- --- --------- ------------------------------
000900090609     ** 1         IN  CHARACTER Strategi User Number
001000090609     ** 2         IN  CHARACTER Cliente unificante
001100090611     ** 3         IN  CHARACTER Tipo procedura legata
001200100827     ** 4         OUT CHARACTER Esito (N=No Srv 0=No srv attivi 1=Sì srv attivi)
001300071128     ** --------- --- --------- ------------------------------
001400090610     **
001500090610     ** PARAMETRI DELLA SOTTOPROCEDURA UBUTISGI_DisableSgiUsr
001600090610     **
001700090610     ** Posizione Uso Tipo      Descrizione
001800090610     ** --------- --- --------- ------------------------------
001900090610     ** 1         IN  CHARACTER Strategi User Number
002000090610     ** 2         IN  CHARACTER Cliente unificante
002100090610     ** 4         OUT CHARACTER Esito (N=Usr nn trovato  E=Errore  1=Ok usr disabilitato)
002200090610     ** --------- --- --------- ------------------------------
002300100827     **
002400100827     ** PARAMETRI DELLA SOTTOPROCEDURA UBUTISGI_DeleteSgiUsr
002500100827     **
002600100827     ** Posizione Uso Tipo      Descrizione
002700100827     ** --------- --- --------- ------------------------------
002800100827     ** 1         IN  CHARACTER Strategi User Number
002900100827     ** 2         OUT CHARACTER Esito (N=Usr nn trovato  E=Errore  1=Ok usr disabilitato)
003000100827     ** --------- --- --------- ------------------------------
003100100827     **
003200071129     **
003300071128     **
003400071128     ** ESEMPI DI CHIAMATA
003500071128     **
003600090609     ** if UBUTISGI_RtvSrvSts(pInSUN:pInKSU:pInRGR:pOutEsito) = 0
003700090610     ** if UBUTISGI_DisableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0
003800100827     ** if UBUTISGI_DeleteSgiUsr(pInSUN:pOutEsito) = 0
003900071128     **
004000071128     ** ISTRUZIONI PER LA COMPILAZIONE
004100071128     **
004200090609     ** 1. Creare il modulo UBUTISGI (Opz 15 PDM)
004300090609     ** 2. Creare/Aggiornare il programma di servizio UBUTISGI (CRTSRVPGM / UPDSRVPGM)
004400090609     ** 3. Creare il programma UBUTISGI dal modulo (CRTPGM) - solo se struttura anche *pgm (*ENTRY)
004500080610     ** 4. Cancellare il modulo.
004600071128     **
004700071128     ***********************************************************************************************
004800090609
004900090609      //********************************************************************************************
005000090609      //
005100090609      // Definizione file
005200090609      //
005300090609      //********************************************************************************************
005400090610     Ftivss03l  IF   E           K DISK    extfile(LibFile01)        usropn
005500090610     Ftntbe01l  IF   E           K DISK    extfile(LibFile02)        usropn
005600071128
005700071128      //********************************************************************************************
005800071128      //
005900071128      // Definizione prototipi procedure.
006000071128      //
006100071128      //********************************************************************************************
006200090609     D/COPY GAITRASRC/SRCPROTOPI,UBUTISGI
006300090609     D/COPY GAITRASRC/SRCPROTOPR,UBUTISGI
006400090609     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
006500090609     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
006600071128
006700071128      //********************************************************************************************
006800071128      //
006900071128      // Definizione variabili globali.
007000071128      //
007100071128      //********************************************************************************************
007200090609     D InitDone        S               N   INZ(*off)
007300090609     D LibFile01       S             21A   INZ
007400090609     D LibFile02       S             21A   INZ
007500090610     D gCurrSysNeta    S              8A   INZ
007600090609
007700071127
007800071128      //********************************************************************************************
007900071128      //
008000071128      // Dichiarazione parametri.
008100071128      //
008200071128      //********************************************************************************************
008300090609     C***  *ENTRY        PLIST
008400090609     C***                PARM                    pInPar001...
008500090609
008600090609
008700090609     P*--------------------------------------------------
008800090609     P* Procedure name: UBUTISGI_Init
008900090609     P* Purpose:        Inizializzazioni
009000090609     P* Returns:        Integer (Status_Code)
009100090609     P*--------------------------------------------------
009200090609     P*
009300090609     P UBUTISGI_Init...
009400090609     P                 B                   EXPORT
009500090609     D*
009600090609     D UBUTISGI_Init...
009700090609     D                 PI            10I 0
009800090609     D*
009900090609      //********************************************************************************************
010000090609      //
010100090609      // Definizione variabili work (locali)
010200090609      //
010300090609      //********************************************************************************************
010400090609     D locMethodCode   S             10I 0
010500090609     D currSysNeta     S              8A
010600090609
010700090609      /FREE
010800090609        // Inizializzo opcode di metodo
010900090609        locMethodCode = -1;
011000090609        InitDone = *off;
011100090609
011200090609        // Verifico il sistema AS/400 corrente
011300090609        if UBRTVNETA_Rtv(currSysNeta) = 0;
011400090610           gCurrSysNeta = currSysNeta;
011500090609           if %subst(currSysNeta:1:6) = 'SETRAS';
011600090609              LibFile01 = 'UNITRAGRU/TIVSS03L';
011700090609              LibFile02 = 'GAITRAGRU/TNTBE01L';
011800090609           else;
011900090609              LibFile01 = 'UNITRAGRP/TIVSS03L';
012000090609              LibFile02 = 'GAITRAGRPS/TNTBE01L';
012100090609           endif;
012200090609
012300090609           // Se necessario apro i file
012400090609           if not %open(tivss03l);
012500090609              open tivss03l;
012600090609           endif;
012700090609           if not %open(tntbe01l);
012800090609              open tntbe01l;
012900090609           endif;
013000090609
013100090609
013200090609           // Valorizzo opcode di metodo
013300090609           locMethodCode = 0;
013400090609           InitDone = *on;
013500090609
013600090609        endif;
013700090609
013800090609        return locMethodCode;
013900090609
014000090609      /END-FREE
014100090609
014200090609     P UBUTISGI_Init...
014300090609     P                 E
014400090609
014500090609
014600090609
014700090609     P*--------------------------------------------------
014800090609     P* Procedure name: UBUTISGI_Finalize
014900090609     P* Purpose:        Finalizzazioni
015000090609     P* Returns:        Integer (Status_Code)
015100090609     P*--------------------------------------------------
015200090609     P*
015300090609     P UBUTISGI_Finalize...
015400090609     P                 B                   EXPORT
015500090609     D*
015600090609     D UBUTISGI_Finalize...
015700090609     D                 PI            10I 0
015800090609     D*
015900090609      //********************************************************************************************
016000090609      //
016100090609      // Definizione variabili work (locali)
016200090609      //
016300090609      //********************************************************************************************
016400090609     D locMethodCode   S             10I 0
016500090609
016600090609      /FREE
016700090609        // Inizializzo opcode di metodo
016800090609        locMethodCode = -1;
016900090610        InitDone = *off;
017000090610
017100090609
017200090609        // Se necessario chiudo i file
017300090609        if %open(tivss03l);
017400090609           close tivss03l;
017500090609        endif;
017600090609        if %open(tntbe01l);
017700090609           close tntbe01l;
017800090609        endif;
017900090609
018000090609
018100090609        // Valorizzo opcode di metodo
018200090609        locMethodCode = 0;
018300090609
018400090609        return locMethodCode;
018500090609
018600090609      /END-FREE
018700090609
018800090609     P UBUTISGI_Finalize...
018900090609     P                 E
019000090609
019100090609
019200071129
019300071128
019400071127     P*--------------------------------------------------
019500090609     P* Procedure name: UBUTISGI_RtvSrvSts
019600090611     P* Purpose:        Verifica esistenza servizi attivi del tipo procedura legata
019700090609     P* Returns:        Integer
019800090609     P* Parameter:      pInSUN        => Strategi User Number
019900090609     P* Parameter:      pInKSU        => Cliente Unificante
020000090611     P* Parameter:      pInRGR        => Tipo procedura legata
020100090609     P* Parameter:      pOutEsito     => Esito (N=No Srv 0=No srv attivi 1=Sì srv attivi) attivi)
020200071127     P*--------------------------------------------------
020300090609     P UBUTISGI_RtvSrvSts...
020400071129     P                 B                   EXPORT
020500071129     D*
020600090609     D UBUTISGI_RtvSrvSts...
020700071129     D                 PI            10I 0
020800090609     D  pInSUN                        9A   CONST
020900090609     D  pInKSU                        8A   CONST
021000090609     D  pInRGR                        1A   CONST
021100090609     D  pOutEsito                     1A   OPTIONS(*NOPASS)
021200090609
021300090609
021400071129      //********************************************************************************************
021500071129      //
021600071129      // Definizione variabili work.
021700071129      //
021800071129      //********************************************************************************************
021900090609     D skISV           S              2    DIM(999)
022000090609     D jISV            S              3  0 INZ
022100090609     D disv          E DS                  INZ
022200090609     D wChkISV         S               N
022300090610     D locMethodCode   S             10I 0
022400090610
022500090610      /FREE
022600090610        // Inizializzo opcode di metodo
022700090610        locMethodCode = -1;
022800090610
022900090610
023000090610        // Verifico se già evocato metodo Init
023100090610        if not InitDone;
023200090610           UBUTISGI_Init();
023300090610        endif;
023400090610
023500090609
023600090609        // Inizializzo a falso il flag "presenti servizi attivi"
023700090609        wChkISV  = *off;
023800090609        pOutEsito = '0';
023900090609
024000090610
024100090611        // Carico i servizi del tipo procedura legata richiesta
024200090609        setll  ('ISV') tntbe01l;
024300090609        if %equal(tntbe01l);
024400090609           reade  ('ISV') tntbe01l;
024500090609           dow not %eof(tntbe01l);
024600090609               disv = tbeuni;
024700090611               if §isvproc = pInRGR;
024800090609                  jISV = jISV + 1;
024900090609                  skISV(jISV) = tbeKE1;
025000090609               endif;
025100090609               reade  ('ISV') tntbe01l;
025200090609           enddo;
025300090609        endif;
025400090609
025500071129
025600090609        // Verifica servizi attivi su utente/cliente
025700090609        setll  (pInKSU:pInSUN) tivss03l;
025800090609        if %equal(tivss03l);
025900090609           reade  (pInKSU:pInSUN) tivss03l;
026000090609           dow not %eof(tivss03l);
026100090609               eval jISV = %lookup(vssISV:skISV);
026200090609               if jISV > *zeros;
026300090609                  wChkISV  = *on;
026400090609                  if vssdde <= %dec(%date()) and vssdsc >= %dec(%date());
026500090609                     pOutEsito = '1';
026600090609                     Leave;
026700090609                  endif;
026800090609               endif;
026900090609               reade  (pInKSU:pInSUN) tivss03l;
027000090609           enddo;
027100090609        endif;
027200090610
027300090609
027400090609        // Verifico condizioni attuali
027500090609        if wChkISV = *off;
027600090609           pOutEsito = 'N';
027700071205        endif;
027800071127
027900090610
028000090610        // Valorizzo opcode di metodo
028100090610        locMethodCode = 0;
028200090610
028300090610
028400090610        return locMethodCode;
028500071127
028600071127      /END-FREE
028700071129
028800090609     P UBUTISGI_RtvSrvSts...
028900071129     P                 E
029000090610
029100090610
029200090610
029300090610
029400090610     P*--------------------------------------------------
029500090610     P* Procedure name: UBUTISGI_DisableSgiUsr
029600090610     P* Purpose:        Disabilita l'utente Strategi richiesto
029700090610     P* Returns:        Integer
029800090610     P* Parameter:      pInSUN        => Strategi User Number
029900090610     P* Parameter:      pInKSU        => Cliente Unificante
030000090610     P* Parameter:      pOutEsito     => Esito (N=Usr nn trovato  E=Errore  1=Ok usr disabilitato)
030100090610     P*--------------------------------------------------
030200090610     P UBUTISGI_DisableSgiUsr...
030300090610     P                 B                   EXPORT
030400090610     D*
030500090610     D UBUTISGI_DisableSgiUsr...
030600090610     D                 PI            10I 0
030700090610     D  pInSUN                        9A   CONST
030800090610     D  pInKSU                        8A   CONST
030900090610     D  pOutEsito                     1A   OPTIONS(*NOPASS)
031000090610
031100090610
031200090610      //********************************************************************************************
031300090610      //
031400090610      // Definizione variabili work.
031500090610      //
031600090610      //********************************************************************************************
031700090610     D locMethodCode   S             10I 0
031800090610     D locEsito        S             10I 0
031900090610     D wInSUN          S                   like(pInSUN)
032000100921     D wInKSU          S             40
032100090610     D wEsito          S                   like(pOutEsito) inz('N')
032200090610     D cmd             S            500
032300090610     D cmdl            S             15  5 inz(%size(cmd))
032400100921     D VAS777          S             10    inz('SGISECOFR')
032500100921     D wwwVAS          S             10    inz('uffa')
032600090610
032700090610
032800090610      /FREE
032900090610        // Inizializzo opcode di metodo
033000090610        locMethodCode = -1;
033100090610        locEsito = -1;
033200090610
033300090610
033400090610        // Verifico se già evocato metodo Init
033500090610        if not InitDone;
033600090610           UBUTISGI_Init();
033700090610        endif;
033800090610
033900090610
034000090610        // Inizializzo a ko l'esito richiesta
034100090610        pOutEsito = 'E';
034200090610
034300090610
034400090610        // Monitorizzo tutto quanto segue
034500090610        Monitor;
034600090610
034700090610
034800090610        // Via SQL mi connetto al db remoto AS777 ma SOLO se sono su SETRAS
034900090610        if %subst(gCurrSysNeta:1:6) = 'SETRAS';
035000090610           // X zelo disconnetto preventivamente il db remoto AS777
035100090610           exec sql
035200090610                disconnect AS777;
035300090610
035400090610           // Quindi eseguo la connessione al db remoto AS777
035500090610           exec sql
035600090610                connect to AS777 user :VAS777 using :wwwVAS;
035700090610
035800090610           // Eseguo *pgm remoto x sincerarmi esistenza utente della richiesta corrente
035900090610           wInSUN = pInSUN;
036000090610           wInKSU = %subst(pInKSU:2);
036100090610           exec sql
036200090610                call STRATEOBJ/TIS180R ('U', :wInSUN, :wInKSU, :wEsito);
036300090610
036400090610
036500090610           // Se tutto ok => procedo
036600090610           if sqlcod < *zeros;
036700090610           else;
036800090610              if wEsito = 'S';
036900090610                 // Disabilito l'utente STRATEGI
037000090610                 cmd = 'STRATEGI/CHGSGIUSR ' +
037100090610                       pInSUN + ' STATUS(*DISABLED) ';
037200090610
037300090610                 exec sql
037400090610                      call qsys/qcmdexc (:cmd, :cmdl);
037500090610
037600090610                 // Verifico esito disabilitazione
037700090610                if sqlcod < *zeros;
037800090610                else;
037900090610                   pOutEsito = '1';
038000090610                endif;
038100090610              else;
038200090610                 pOutEsito = 'N';
038300090610              endif;
038400090610           endif;
038500090610        endif;
038600090610
038700090610        locEsito = 0;
038800090610
038900090610        On-Error;
039000090610           locEsito = -1;
039100090610        EndMon;
039200090610
039300090610
039400090610        // Al termine ripristino assolutamente il db su *local
039500090610        exec sql
039600090610             disconnect AS777;
039700090610
039800090610        exec sql
039900090610             connect reset;
040000090610
040100090610        exec sql
040200090610             set connection :gCurrSysNeta;
040300090610
040400090610
040500090610        // Valorizzo opcode di metodo
040600090610        locMethodCode = locEsito;
040700090610
040800090610
040900090610        return locMethodCode;
041000090610
041100090610      /END-FREE
041200090610
041300090610     P UBUTISGI_DisableSgiUsr...
041400090610     P                 E
041500090610
041600090610
041700090610
041800090610
041900090610     P*--------------------------------------------------
042000090610     P* Procedure name: UBUTISGI_EnableSgiUsr
042100090610     P* Purpose:        Abilita l'utente Strategi richiesto
042200090610     P* Returns:        Integer
042300090610     P* Parameter:      pInSUN        => Strategi User Number
042400090610     P* Parameter:      pInKSU        => Cliente Unificante
042500090610     P* Parameter:      pOutEsito     => Esito (N=Usr nn trovato  E=Errore  1=Ok usr abilitato)
042600090610     P*--------------------------------------------------
042700090610     P UBUTISGI_EnableSgiUsr...
042800090610     P                 B                   EXPORT
042900090610     D*
043000090610     D UBUTISGI_EnableSgiUsr...
043100090610     D                 PI            10I 0
043200090610     D  pInSUN                        9A   CONST
043300090610     D  pInKSU                        8A   CONST
043400090610     D  pOutEsito                     1A   OPTIONS(*NOPASS)
043500090610
043600090610
043700090610      //********************************************************************************************
043800090610      //
043900090610      // Definizione variabili work.
044000090610      //
044100090610      //********************************************************************************************
044200090610     D locMethodCode   S             10I 0
044300090610     D locEsito        S             10I 0
044400090610     D wInSUN          S                   like(pInSUN)
044500100921     D wInKSU          S             40
044600090610     D wEsito          S                   like(pOutEsito) inz('N')
044700090610     D cmd             S            500
044800090610     D cmdl            S             15  5 inz(%size(cmd))
044900090610     D VAS777          S             10    inz('VAS777')
045000090610     D wwwVAS          S             10    inz('wwwVAS')
045100090610
045200090610
045300090610      /FREE
045400090610        // Inizializzo opcode di metodo
045500090610        locMethodCode = -1;
045600090610        locEsito = -1;
045700090610
045800090610
045900090610        // Verifico se già evocato metodo Init
046000090610        if not InitDone;
046100090610           UBUTISGI_Init();
046200090610        endif;
046300090610
046400090610
046500090610        // Inizializzo a ko l'esito richiesta
046600090610        pOutEsito = 'E';
046700090610
046800090610
046900090610        // Monitorizzo tutto quanto segue
047000090610        Monitor;
047100090610
047200090610
047300090610        // Via SQL mi connetto al db remoto AS777 ma SOLO se sono su SETRAS
047400090610        if %subst(gCurrSysNeta:1:6) = 'SETRAS';
047500090610           // X zelo disconnetto preventivamente il db remoto AS777
047600090610           exec sql
047700090610                disconnect AS777;
047800090610
047900090610           // Quindi eseguo la connessione al db remoto AS777
048000090610           exec sql
048100090610                connect to AS777 user :VAS777 using :wwwVAS;
048200090610
048300090610           // Eseguo *pgm remoto x sincerarmi esistenza utente della richiesta corrente
048400090610           wInSUN = pInSUN;
048500090610           wInKSU = %subst(pInKSU:2);
048600090610           exec sql
048700090610                call STRATEOBJ/TIS180R ('U', :wInSUN, :wInKSU, :wEsito);
048800090610
048900090610
049000090610           // Se tutto ok => procedo
049100090610           if sqlcod < *zeros;
049200090610           else;
049300090610              if wEsito = 'S';
049400090610                 // Disabilito l'utente STRATEGI
049500090610                 cmd = 'STRATEGI/CHGSGIUSR ' +
049600090610                       pInSUN + ' STATUS(*ENABLED) ';
049700090610
049800090610                 exec sql
049900090610                      call qsys/qcmdexc (:cmd, :cmdl);
050000090610
050100090610                 // Verifico esito abilitazione
050200090610                if sqlcod < *zeros;
050300090610                else;
050400090610                   pOutEsito = '1';
050500090610                endif;
050600090610              else;
050700090610                 pOutEsito = 'N';
050800090610              endif;
050900090610           endif;
051000090610        endif;
051100090610
051200090610        locEsito = 0;
051300090610
051400090610        On-Error;
051500090610           locEsito = -1;
051600090610        EndMon;
051700090610
051800090610
051900090610        // Al termine ripristino assolutamente il db su *local
052000090610        exec sql
052100090610             disconnect AS777;
052200090610
052300090610        exec sql
052400090610             connect reset;
052500090610
052600090610        exec sql
052700090610             set connection :gCurrSysNeta;
052800090610
052900090610
053000090610        // Valorizzo opcode di metodo
053100090610        locMethodCode = locEsito;
053200090610
053300090610
053400090610        return locMethodCode;
053500090610
053600090610      /END-FREE
053700090610
053800090610     P UBUTISGI_EnableSgiUsr...
053900090610     P                 E
054000100827
054100100827
054200100827
054300100827
054400100827     P*--------------------------------------------------
054500100827     P* Procedure name: UBUTISGI_DeleteSgiUsr
054600100827     P* Purpose:        Elimina l'utente Strategi richiesto
054700100827     P* Returns:        Integer
054800100827     P* Parameter:      pInSUN        => Strategi User Number
054900100827     P* Parameter:      pOutEsito     => Esito (N=Usr nn trovato  E=Errore  1=Ok usr eliminato)
055000100827     P*--------------------------------------------------
055100100827     P UBUTISGI_DeleteSgiUsr...
055200100827     P                 B                   EXPORT
055300100827     D*
055400100827     D UBUTISGI_DeleteSgiUsr...
055500100827     D                 PI            10I 0
055600100827     D  pInSUN                        9A   CONST
055700100827     D  pOutEsito                     1A   OPTIONS(*NOPASS)
055800100827
055900100827
056000100827      //********************************************************************************************
056100100827      //
056200100827      // Definizione variabili work.
056300100827      //
056400100827      //********************************************************************************************
056500100827     D locMethodCode   S             10I 0
056600100827     D locEsito        S             10I 0
056700100827     D wInSUN          S                   like(pInSUN)
056800100921     D wInKSU          S             40
056900100827     D wEsito          S                   like(pOutEsito) inz('N')
057000100827     D cmd             S            500
057100100827     D cmdl            S             15  5 inz(%size(cmd))
057200100827     D VAS777          S             10    inz('VAS777')
057300100827     D wwwVAS          S             10    inz('wwwVAS')
057400100827
057500100827
057600100827      /FREE
057700100827        // Inizializzo opcode di metodo
057800100827        locMethodCode = -1;
057900100827        locEsito = -1;
058000100827
058100100827
058200100827        // Verifico se già evocato metodo Init
058300100827        if not InitDone;
058400100827           UBUTISGI_Init();
058500100827        endif;
058600100827
058700100827
058800100827        // Inizializzo a ko l'esito richiesta
058900100827        pOutEsito = 'E';
059000100827
059100100827
059200100827        // Monitorizzo tutto quanto segue
059300100827        Monitor;
059400100827
059500100827
059600100827        // Via SQL mi connetto al db remoto AS777 ma SOLO se sono su SETRAS
059700100827        if %subst(gCurrSysNeta:1:6) = 'SETRAS';
059800100827           // X zelo disconnetto preventivamente il db remoto AS777
059900100827           exec sql
060000100827                disconnect AS777;
060100100827
060200100827           // Quindi eseguo la connessione al db remoto AS777
060300100827           exec sql
060400100827                connect to AS777 user :VAS777 using :wwwVAS;
060500100827
060600100827           // Eseguo *pgm remoto x sincerarmi esistenza utente della richiesta corrente
060700100827           wInSUN = pInSUN;
060800100827           wInKSU = *blanks;
060900100827           exec sql
061000100827                call STRATEOBJ/TIS180R ('S', :wInSUN, :wInKSU, :wEsito);
061100100827
061200100827
061300100827           // Se tutto ok => procedo
061400100827           if sqlcod < *zeros;
061500100827           else;
061600100827              if wEsito = 'S';
061700100827                 // Elimino l'utente STRATEGI
061800100827                 cmd = 'STRATEGI/DLTSGIUSR ' + pInSUN;
061900100827
062000100827                 exec sql
062100100827                      call qsys/qcmdexc (:cmd, :cmdl);
062200100827
062300100827                 // Verifico esito disabilitazione
062400100827                if sqlcod < *zeros;
062500100827                else;
062600100827                   pOutEsito = '1';
062700100827                endif;
062800100827              else;
062900100827                 pOutEsito = 'N';
063000100827              endif;
063100100827           endif;
063200100827        endif;
063300100827
063400100827        locEsito = 0;
063500100827
063600100827        On-Error;
063700100827           locEsito = -1;
063800100827        EndMon;
063900100827
064000100827
064100100827        // Al termine ripristino assolutamente il db su *local
064200100827        exec sql
064300100827             disconnect AS777;
064400100827
064500100827        exec sql
064600100827             connect reset;
064700100827
064800100827        exec sql
064900100827             set connection :gCurrSysNeta;
065000100827
065100100827
065200100827        // Valorizzo opcode di metodo
065300100827        locMethodCode = locEsito;
065400100827
065500100827
065600100827        return locMethodCode;
065700100827
065800100827      /END-FREE
065900100827
066000100827     P UBUTISGI_DeleteSgiUsr...
066100100827     P                 E
