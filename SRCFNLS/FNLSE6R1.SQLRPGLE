000100101210      *PARMS DBGVIEW(*SOURCE)
000200101210     /*PRM dbgview(*source)
000300101210     /*CMD ovrdbf file(WFNBL00F) tofile(GAITRAAZP/WFNBL00F) +
000400101210     /*CMD        ovrscope(*calllvl)
000500101210     /*END dltovr file(WFNBL00F) lvl(*)
000600101210     /*END
000700070119
000800070123      *---------------------------------------------------------------*
000900070124      *?STATISTICA NUMERO LDV "A4" ED "A5" PER DATA/LNP/LNA - BATCH  ?*
001000070123      *---------------------------------------------------------------*
001100070119
001200070119     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001300070119     h alwnull(*inputonly)
001400070119
001500070119      *---------------------------------------------------------------*
001600070119
001700080212     fAZORG01L  if   e           k disk
001800090512     fTABEL00F  if   e           k disk
001900080212      *
002000070119     fWFNBL00F  o    e             disk
002100070119
002200070119      *---------------------------------------------------------------*
002300070119
002400070119      *
002500070119      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002600070119      *
002700080212     d C_ForFetch      c                   const('FOR FETCH ONLY')
002800070123      *
002900070119      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
003000070119      *
003100080212      * - Stringhe SQL da eseguire
003200080212     d $Sql            s             70    dim(06) ctdata  perrcd(1)
003300090512      *
003400090512      * - Filiali e relativi pgm di stampa da tab. "5P"/"B"
003500090512     d $5PBk           s              3    dim(999)  inz
003600090512     d $5PBp           s             10    dim(999)  inz
003700070119      *
003800090512      *?  S T R U T T U R E - D A T I   - - - - - - - - - - - - - - -?*
003900070119      *
004000070119      * - Parametri
004100090512     d KPJBA         e ds
004200070119     d FNLSE6ds        ds                  inz
004300070119     d  DE6dti                        8  0 inz(*loval)
004400070119     d  DE6dtf                        8  0 inz(*hival)
004500070119      *
004600070119      * - Parametri x Controllo profilo utenti
004700070119     d TIBS34ds      e ds                  inz
004800090126      *
004900090126      * - Reperimento nome pgm di stampa LdV
005000090512     d ds5PB         e ds                  inz
005100080212      *
005200080212      * - 143ª descrizione dell'organigramma
005300080212     d Og143         e ds                  inz
005400070123      *
005500070123      * - Parametri x Stampa LdV
005600070123     d FNLSB5ds      e ds                  inz
005700070123     d  DB0tbo       e                     inz('A')
005800070123     d  DB0ris       e                     inz('R')
005900070123     d  DB0fl9       e                     inz(*on)
006000070123      *
006100080212      * - Struttura dati per la definizione dei campi del file FNARB
006200080212     d FNARB00X      e ds                  extname(FNARB00F)
006300080212     d                                     prefix(X_)     inz
006400080212      * - Dati estratti via SQL
006500080212     d FNARBds         ds                  inz
006600080212     d   ARBaas                            like(X_ARBaas) inz
006700080212     d   ARBmgs                            like(X_ARBmgs) inz
006800080212     d   ARBlnp                            like(X_ARBlnp) inz
006900080212     d   ARBnrs                            like(X_ARBnrs) inz
007000080212     d   ARBnsp                            like(X_ARBnsp) inz
007100080212     d   ARBlna                            like(X_ARBlna) inz
007200070119      *
007300070123      * - Status ds
007400070119     d Status         sds
007500070119     d  SDSpgm           *proc
007600070119     d  SDSprm           *parms
007700070119      *
007800070119      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007900070119      *
008000070119      * - Flags booleani
008100070119     d $EoF            s              1    inz(*off)
008200070119     d $Err            s              1    inz(*off)
008300090512      * - Indici di schiera
008400090512     d xx              s              3  0 inz
008500090512     d yy              s              3  0 inz
008600070123      * - Contatori
008700070123     d wNrA4           s                   like(WNBna4)   inz
008800070123     d wNrA5           s                   like(WNBna5)   inz
008900080212      * - Stringa SQL da eseguire
009000080212     d wSql            s           5000    varying        inz
009100070123      * - Dati per controllo "rotture di livello"
009200070123     d SAVdts          s                   like(WNBdsp)   inz(*loval)
009300070123     d SAVlnp          s                   like(WNBlnp)   inz(*loval)
009400070123     d SAVlna          s                   like(WNBlna)   inz(*loval)
009500070119      * - Campi di comodo
009600080212     d wLnaExp         s            500    varying        inz
009700070123     d wDate_Amg       s              8  0
009800090512
009900090512       //--------------------------------------------------------------
010000090512       //?Definizione aree dati.                                       ?
010100090512       //--------------------------------------------------------------
010200090512
010300090512       // - Dati utente
010400090512     d §AzUte        e ds                  extname(AZUTE00F)
010500090512     d                                     dtaara
010600090512     d §DatiUte      e ds                  extname(dDatiUte)
010700090512     d                                     dtaara
010800090512
010900090512       //--------------------------------------------------------------
011000090512       //?Definizione prototipi procedure.                             ?
011100090512       //--------------------------------------------------------------
011200090512
011300090512       // - Reperimento dati utente
011400090512      /copy gaitrasrc/srcProtoPR,TIBS34R
011500090512
011600090512       //--------------------------------------------------------------
011700090512       //?Definizione key-list.                                        ?
011800090512       //--------------------------------------------------------------
011900070119
012000090512       // - File TABEL00F
012100090512     d k03tabel00    e ds                  extname(TABEL00F : *key)
012200090512     d                                     prefix(k_)   inz
012300090512
012400070119      *---------------------------------------------------------------*
012500070119      *? RIEPILOGO INDICATORI                                        ?*
012600070119      *---------------------------------------------------------------*
012700070119      * 10   - Comodo                                                 *
012800070119      *---------------------------------------------------------------*
012900070119      *
013000070119     c     *Entry        plist
013100090512     c                   parm                    KPJBA
013200070119      *
013300090512     c                   exsr      sr_RoutInz
013400080212      *
013500080212      * Preparazione stringa SQL
013600080212     c                   exsr      PrepSQL
013700070119      *
013800070119      * Ciclo di elaborazione
013900070119     c                   exsr      OpenCursor
014000070119      *
014100070119     c                   dou       $Eof   = *on
014200070119     c                   exsr      ReadCursor
014300070119     c                   enddo
014400070119      *
014500070119     c                   exsr      CloseCursor
014600070119      *
014700070119      * Fine Programma
014800090512     c                   exsr      sr_RoutEnd
014900090512
015000090512      /free
015100090512
015200090512       //--------------------------------------------------------------
015300090512       //?Operazioni iniziali                                          ?
015400090512       //--------------------------------------------------------------
015500090512       BEGSR  sr_RoutInz;
015600090512
015700090512         *inLR = *on;
015800090512
015900090512         // Reperimento dati job
016000090512         exsr  sr_DatiJob;
016100090512
016200090512         // Reperimento eventuali parametri
016300090512         if  SDSprm > *zeros   and
016400090512             KPJBU <> *blanks;
016500090512           FNLSE6ds = KPJBU;
016600090512         endif;
016700090512
016800090512         // Reperimento data odierna
016900090512         wDate_Amg = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
017000090514
017100090514         // Caricamento tab. 5P di tutte le filiali
017200090514         exsr  sr_InTab5P;
017300090512
017400090512       ENDSR;
017500090512
017600090512       //--------------------------------------------------------------
017700090512       //?Reperimento Dati del job (Utente/Operativi).                 ?
017800090512       //--------------------------------------------------------------
017900090512       BEGSR  sr_DatiJob;
018000090512
018100090512         in(E) §AzUte;
018200090512         if NOT %error;
018300090512           in(E) §DatiUte;
018400090512         endif;
018500090512         if %error or RSut = *blanks;
018600090512           clear TIBS34ds;
018700090512           tibs34r ( tibs34ds );
018800090512           in §AzUte;
018900090512           in §DatiUte;
019000090512         endif;
019100090512
019200090512       ENDSR;
019300090512
019400090512       //--------------------------------------------------------------
019500090512       //?Operazioni finali                                            ?
019600090512       //--------------------------------------------------------------
019700090512       BEGSR  sr_RoutEnd;
019800090512
019900090512         return;
020000090512
020100090512       ENDSR;
020200090512
020300090512       //--------------------------------------------------------------
020400090512       //?Intabellamento pgm di stampa LdV per filiale (da tab. "5P/O")?
020500090512       //--------------------------------------------------------------
020600090512       BEGSR  sr_InTab5P;
020700090512
020800090512         //?Intabellamento filiali & relativi pgm di stampa da tab. "5P"?
020900090512         k_TBLkut = 1;
021000090512         k_TBLcod = '5P';
021100090512         k_TBLkey = 'B   ';
021200090512         setll  %kds(k03tabel00)  TABEL;
021300090512         reade  %kds(k03tabel00 : 2)  TABEL;
021400090512         DoW  Not %eof(TABEL00F);
021500090512           select;
021600090512             when  %subst(TBLkey : 1 : 1) <> 'B';
021700090512               leave;
021800090512             when  TBLflg = *blank;
021900090512               xx   += 1;
022000090512               ds5PB = TBLuni;
022100090512               $5PBk(xx) = %subst(TBLkey : 2 : 3);
022200090512               $5PBp(xx) = §5Pbpl;
022300090512           endsl;
022400090512           reade  %kds(k03tabel00 : 2)  TABEL;
022500090512         EndDo;
022600090512
022700090512         yy = xx + 1;
022800090512         $5PBk(yy) = *loval;
022900090512         $5PBp(yy) = 'FNLSB6R';
023000090512
023100090512       ENDSR;
023200090512
023300090512       //--------------------------------------------------------------
023400090512       //?Reperimento pgm di stampa LdV da tab. "5P"/"B" per la LNA    ?
023500090512       //--------------------------------------------------------------
023600090512       BEGSR  sr_Rep5PxLNA;
023700090512
023800090512         xx = %lookup( %editc(ARBlna : 'X') : $5PBk );
023900090512
024000090512         // Se filiale non reperita, forzo "FNLSB6R"
024100090512         //   [memorizzato in $5PBp(yy)]
024200090512         if  xx = *zero;
024300090512           xx = yy;
024400090512         endif;
024500090512
024600090512       ENDSR;
024700090512
024800090512      /end-free
024900080212      *
025000080212      *---------------------------------------------------------------*
025100080212      *?Preparazione stringa SQL                                     ?*
025200080212      *---------------------------------------------------------------*
025300080212     c     PrepSQL       BEGSR
025400080212      *
025500080212      * Intabellamento lna export
025600080212     c                   clear                   wLnaExp
025700080212     c     *loval        setll     AZORG
025800080212     c                   read      AZORG
025900080212do  1c                   dow       NOT %eof(AZORG01L)
026000080212     c                   eval      Og143 = ORGde3
026100080212if  2c                   if             §OGntw = 'DPD'
026200080212     c                             or   §OGntw = 'EEX'
026300080212     c                             or   §OGntw = 'EUP'
026400080212     c                             or   §OGntw = 'FED'
026500080212     c                   if        wLnaExp  = *blanks
026600080212     c                   eval      wLnaExp  = %char(ORGfil)
026700080212     c                   else
026800080212     c                   eval      wLnaExp  = wLnaExp + ', '
026900080212     c                                      + %char(ORGfil)
027000080212     c                   endif
027100080212e   2c                   endif
027200080212     c                   read      AZORG
027300080212e   1c                   enddo
027400080212      *
027500080212      * Impostazione parametri ricevuti come parzializzazioni
027600080212      *   nella stringa SQL
027700080212      * - date dal - al
027800080212     c                   eval      $Sql(04) = %trimr($Sql(04))
027900080212     c                                      + ' '
028000080212     c                                      + %char(DE6dti)
028100080212     c                                      + ' and '
028200080212     c                                      + %char(DE6dtf)
028300080212      *
028400080212      *? E S E M P I O : ?
028500080212      *
028600080212      *  select  ARBaas, ARBmgs, ARBlnp, ARBnrs, ARBnsp, ARBlna
028700080212      *  from    FNARB00F
028800080212      *  where  (ARBncd > 0   or   ARBfns = 'S')
028900080212      *    and ((ARBaas*10000)+ARBmgs)  between  :DE6dti and :DE6dtf
029000080212      *    and   ARBlna not in (:wLnaExp)
029100080212      *  order   by  ARBaas, ARBmgs, ARBlnp, ARBlna
029200080212      *  for     FETCH  only
029300080212      *
029400080212      *? N.B. ?· Seleziona le LdV del periodo richiesto
029500080212      *        · Seleziona le LdV con segnacolli
029600080212      *        · Scarta    le LdV con LNA export
029700080212      *
029800080212     c                   clear                   wSql
029900080212     c                   eval      wSql     =  %trim($Sql(01)) + ' '
030000080212     c                                      +  %trim($Sql(02)) + ' '
030100080212     c                                      +  %trim($Sql(03)) + ' '
030200080212     c                                      +  %trim($Sql(04))
030300080212      *
030400080212     c                   if        wLnaExp <> *blanks
030500080212     c                   eval      wSql     =  wSql            + ' '
030600080212     c                                      +  %trim($Sql(05))
030700080212     c                                      +  wLnaExp         + ')'
030800080212     c                   endif
030900080212      *
031000080212     c                   eval      wSql     =  wSql            + ' '
031100080212     c                                      +  %trim($Sql(06)) + ' '
031200080212      *
031300080212      *?for fetch only?
031400080212     c                                      +  C_ForFetch
031500080212      *
031600080212     c                   ENDSR
031700070119      *
031800070119      *---------------------------------------------------------------*
031900070119      *?Apertura cursore.                                            ?*
032000070119      *---------------------------------------------------------------*
032100070119     c     OpenCursor    BEGSR
032200070122      *
032300070122      * Impostazione opzioni SQL
032400070122      *
032500070122     c/exec sql
032600070123     c+     SET  OPTION  DYNUSRPRF = *OWNER,
032700070122     c+                  CLOSQLCSR = *ENDMOD
032800070122     c/end-exec
032900070119      *
033000070119      * Dichiarazione cursore
033100070119      *
033200080212     c/exec sql
033300080212     c+     PREPARE S1 FROM :wSql
033400080212     c/end-exec
033500080212     c*
033600080212     c/exec sql
033700080212     c+     DECLARE C1 CURSOR FOR S1
033800080212     c/end-exec
033900070119      *
034000070119      * Apertura del cursore
034100070119      *
034200070119     c/exec sql
034300070123     c+     OPEN C1
034400070119     c/end-exec
034500070119      *
034600070119     c                   ENDSR
034700070119      *
034800070119      *---------------------------------------------------------------*
034900070119      *?Chiusura cursore.                                            ?*
035000070119      *---------------------------------------------------------------*
035100070119     c     CloseCursor   BEGSR
035200070119      *
035300070123      * Registrazione totali dell'ultimo giorno/lnp/lna
035400070123if  1c                   if            wNrA4 <> *zeros
035500070123     c                             or  wNrA5 <> *zeros
035600070122     c                   exsr      Wrt_WFNBL
035700070123e   1c                   endif
035800070123      *
035900090514      * Richiamo dei pgm. di stampa LdV per chiusura
036000070123     c                   clear                   FNLSB5ds
036100070123     c                   eval      DB0tla = 'C'
036200101210     c                   call      $5PBp(yy)
036300070123     c                   parm                    FNLSB5ds
036400070119      *
036500070119     c/exec sql
036600070119     c+     CLOSE C1
036700070119     c/end-exec
036800070119      *
036900070119     c                   ENDSR
037000070119      *
037100070119      *---------------------------------------------------------------*
037200070119      *?Lettura cursore.                                             ?*
037300070119      *---------------------------------------------------------------*
037400070119     c     ReadCursor    BEGSR
037500070119      *
037600070122     c                   clear                   FNARBds
037700070119      *
037800070119     c/exec sql
037900070123     c+     FETCH NEXT FROM C1 INTO :FNARBds
038000070119     c/end-exec
038100070119      *
038200070119sel 1c                   select
038300070119w   1c                   when      SQLcod = 100
038400070119     c                   eval      $EoF   = *on
038500070119w   1c                   when      SQLcod < *zeros
038600070119     c                   eval      $Err   = *on
038700070119     c                   eval      $Eof   = *on
038800070119x   1c                   other
038900070123if  2c                   if             SAVdts <> (ARBaas*10000)+ARBmgs
039000070123     c                             or   SAVlnp <>  ARBlnp
039100070123     c                             or   SAVlna <>  ARBlna
039200070123     c                   exsr      sr_Write
039300070123e   2c                   endif
039400070119     c                   exsr      sr_Elab
039500070119e   1c                   endsl
039600070119      *
039700070119     c                   ENDSR
039800070119      *
039900070119      *---------------------------------------------------------------*
040000070123      *?Elaborazione singolo record da FNARB00F                      ?*
040100070119      *---------------------------------------------------------------*
040200070119     c     sr_Elab       BEGSR
040300090514      *
040400090514      * Reperimento pgm di stampa LdV per la LNA della spedizione
040500090514     c                   exsr      sr_Rep5PxLNA
040600070123      *
040700090126      * Richiamo del pgm. di stampa LdV?SOLO?per ricavarne il
040800070123      *   tipo modulo (A4/A5) richiesto dalla LdV in esame
040900070123     c                   reset                   FNLSB5ds
041000070123     c                   eval      DB0aas = ARBaas
041100070123     c                   eval      DB0lnp = ARBlnp
041200070123     c                   eval      DB0nrs = ARBnrs
041300070123     c                   eval      DB0nsp = ARBnsp
041400070123     c*** già così:      eval      DB0tbo = 'A'
041500070123     c*** già così:      eval      DB0ris = 'R'
041600070123     c*** già così:      eval      DB0fl9 = *on
041700090512     c                   call      $5PBp(xx)
041800070123     c                   parm                    FNLSB5ds
041900070123      *
042000070123      * Conteggio LdV per tipo modulo
042100070123     c                   if        DB0tms = '4'
042200070123     c                   add       1             wNrA4
042300070123     c                   else
042400070123     c                   add       1             wNrA5
042500070123     c                   endif
042600070123      *
042700070123     c                   ENDSR
042800070119      *
042900070119      *---------------------------------------------------------------*
043000070123      *?Registrazione record nel WorkFile WFNBL00F                   ?*
043100070119      *---------------------------------------------------------------*
043200070119     c     sr_Write      BEGSR
043300070119      *
043400070123      * Scrittura a rottura di data/lnp/lna
043500070123if  1c                   if            wNrA4 <> *zeros
043600070123     c                             or  wNrA5 <> *zeros
043700070123     c                   exsr      Wrt_WFNBL
043800070123e   1c                   endif
043900070123      *
044000070123      * Azzeramento dei totali
044100070123     c                   clear                   wNrA4
044200070123     c                   clear                   wNrA5
044300070119      *
044400070123      * Memorizzazione dei campi di "rottura"
044500070123     c                   eval      SAVdts  = (ARBaas * 10000) + ARBmgs
044600070123     c                   eval      SAVlnp  = ARBlnp
044700070123     c                   eval      SAVlna  = ARBlna
044800070119      *
044900070119     c                   ENDSR
045000070119      *
045100070119      *---------------------------------------------------------------*
045200070123      *?Scrittura del singolo record nel WorkFile WFNBL00F           ?*
045300070119      *---------------------------------------------------------------*
045400070123     c     Wrt_WFNBL     BEGSR
045500070119      *
045600070123     c                   clear                   WFNBL000
045700070123     c                   eval      WNBeut  = DUTute
045800070123     c                   eval      WNBedt  = wDate_Amg
045900070123     c                   eval      WNBdti  = DE6dti
046000070123     c                   eval      WNBdtf  = DE6dtf
046100070123     c                   eval      WNBdsp  = SAVdts
046200070123     c                   eval      WNBlnp  = SAVlnp
046300070123     c                   eval      WNBlna  = SAVlna
046400070123     c                   eval      WNBna4  = wNrA4
046500070123     c                   eval      WNBna5  = wNrA5
046600070119      *                  __________________
046700070123     c                   WRITE     WFNBL000
046800070119      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
046900070119     c                   ENDSR
047000080212** - $Sql -----------------------------------------------------------*
047100080212select  ARBaas, ARBmgs, ARBlnp, ARBnrs, ARBnsp, ARBlna                  1
047200080212from    FNARB00F                                                        2
047300080212where  (ARBncd > 0   or   ARBfns = 'S')                                 3
047400080212  and ((ARBaas*10000)+ARBmgs)  between                                  4
047500080212  and   ARBlna not in (                                                 5
047600080212order   by  ARBaas, ARBmgs, ARBlnp, ARBlna                              6
