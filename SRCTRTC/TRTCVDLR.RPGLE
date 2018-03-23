000100080219     h decedit('0,') option(*nodebugio)
000200080219      *--------------------------------------------------------------------------------------------*
000300080219      * Pgm autostartjob che per ogni p.o. sottomette                                              *
000400090220      * un JOB per la ricezione dati da Positrova                                                  *
000500080219      *--------------------------------------------------------------------------------------------*
000600080219     fazorg01l  if   e           k disk
000700080219     fazute01l  if   e           k disk
000800090302     fPrtEMAIL  O    f  132        printer  oflind(*inOF)  usropn
000900080219
001000120521     d
001100120521     d tibs40ds      e ds
001200120521     D wfil                    9   1808  0 DIM(600)
001300120521     D wlib                 1809   7808    DIM(600)
001400120521
001500120514      *
001600120514     d XX              s              3  0
001700120514     d parpo           s              3
001800080219     d profilo         s             10
001900080219     d nomejob         s             10
002000080219     d WCurlib         s             10
002100080219     d WCurlibDFT      s             10    inz('*CRTDFT')
002200080219     d comman          S            100                                         *comando CLP
002300080219     d lenght          S             15  5 INZ(100)                             *lunghezza comando
002400090302     d C_Sede          c                   const('046')
002500090302      // - Comando di override al PrtF
002600090302     d C_CmdOvrPrtF    c                   const('OVRPRTF +
002700090302     d                                           file(PRTEMAIL) +
002800090302     d                                           pagesize(66 132) +
002900090302     d                                           lpi(6) cpi(10) +
003000090302     d                                           ovrscope(*actgrpdfn) +
003100090302     d                                           ')
003200090302     d C_CmdDltOvr     c                   const('DLTOVR +
003300090302     d                                            file(PRTEMAIL) +
003400090302     d                                            lvl(*actgrpdfn)')
003500090302
003600120309     D dataiso         s               d   datfmt(*iso)
003700120309     D dateu           s              8  0
003800090302      // - Status
003900090302     d Psds           sds
004000090302     d   SDSpgm          *proc
004100090302     d   JobUser             254    263                                         User name
004200090302      // - Tabella "MRA" = Bart-Mailing -
004300090302     d dMRAdan       e ds                  inz
004400090302      // - Gestione tabelle: controllo e ricerca
004500090302     d TIBS02ds      e ds                  inz
004600090302     d   T02tla      e                     inz('L')
004700090302     d   T02mod      e                     inz('C')
004800090302     d   T02cod      e                     inz('MRA')
004900090302      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
005000090302     d TRTCM1ds      e ds                  inz
005100090302      //   ·§CM1sts  = Stato
005200090302     d   §CM1sts     e                     inz(*off)
005300090303      // Parametri
005400090303     d kpjba         e ds
005500090302      // - Campi per QCMDEXC
005600090302     d Qcmd            s            512    inz
005700090302      // - Campi di stampa
005800090302     d O_testo         s            132    inz
005900080219
006000090302      //---------------------------------------------------------------
006100090302      //?Definizione procedure esterne.
006200090302      //---------------------------------------------------------------
006300090302
006400090302      // - Esecuzione comando di sistema
006500090302     d qCmdExc         pr                  extpgm('QCMDEXC')
006600090302     d  Qcmd                        512    const  options(*varsize)
006700090302     d  Qlen                         15  5 const
006800090302      // - Gestione tabelle
006900090302     d tibs02r         pr                  extpgm('TIBS02R')
007000090302     d  kpjba                              likeds(KPJBA)    const
007100090302     d  tibs02ds                           likeds(TIBS02ds)
007200080219      *--------------------------------------------------------------------------------------------*
007300120309     c                   time                    dataiso
007400120309     C                   move      dataiso       dateu
007500080219
007600090302     c                   exsr      routinz
007700080219      * elimino eventuale libreria corrente
007800080219     c                   eval      Wcurlib = Wcurlibdft
007900080219     C                   exsr      VarCurLib
008000080219     c
008100080219
008200080219     c     *loval        setll     azorg01l
008300080219     c                   do        *hival
008400080219     c                   read      azorg01l
008500080219
008600080219      * fine file
008700080219    1c                   if        %eof(azorg01l)
008800080219     c                   leave
008900080219     c                   endif
009000080219
009100080219      * escludo gli annullati e se non è Filiale o Agenzia
009200080219     c                   if        orgfva <> *blanks or
009300080219     c                             (orgfag <> 'F' and orgfag <> 'A')
009400080219     c                   iter
009500080219     c                   endif
009600080219
009700080219     c                   move      orgfil        parpo
009800080219
009900080219      * verifico esistenza profilo
010000090220     C                   eval      profilo = 'VDL' + parpo
010100080219     c     profilo       chain     azute000
010200120309     c                   if        %found  and dateu<=utedsc
010300080219
010400080219      * verifico se impostare libreria personalizzazioni
010500120514     c**                 clear                   dcur
010600120514     c**                 movel(p)  parpo         kke1
010700120514     c**   Ktbe          chain     TNTBE000
010800120514if  1c**                 if             %found(TNTBE01L)
010900120514     c**                           and  TBEatb = *blanks
011000120514     c**                 movel     TBEuni        dcur
011100080219      *
011200080219      *  imposto libreria personalizzazioni
011300120514     c**                 if        §curlib <> *blanks
011400120514     c**                 eval      Wcurlib = §curlib
011500120514     C**                 exsr      VarCurLib
011600120514     c**                 endif
011700120514     c**                 endif
011800120514     c
011900120514     c* ES --> cambiato uso della tabella CUR caricate le filiali in skiera
012000120514     c                   clear                   xx
012100120514     c                   eval      XX  =%lookup(orgfil:wfil)
012200120514     c                   if        xx  >0
012300120514     c                   eval      Wcurlib = wlib(XX)
012400120514     C                   exsr      VarCurLib
012500120514     c                   endif
012600080219
012700080219      * sottometto JOB
012800090302     c                   call      'TRTCVDLC'
012900090302     c                   parm                    parpo
013000090302     c                   parm                    profilo
013100090302     c                   parm      ' '           esito             1
013200090302     c                   if        esito='E'
013300090302     c                   exsr      sr_PrtEmail
013400090302     c                   endif
013500080219
013600080219     c                   endif
013700080219
013800080219      * REimposto "NESSUNA" libreria corrente
013900080219     c                   if        Wcurlib <> Wcurlibdft
014000080219     c                   eval      Wcurlib = Wcurlibdft
014100080219     C                   exsr      VarCurLib
014200080219     c                   endif
014300080219
014400080219     c                   enddo
014500080219
014600080219     c                   eval      *inlr = *on
014700080219      *--------------------------------------------------------------------------------------------*
014800080219      * modifico libreria corrente
014900080219      *--------------------------------------------------------------------------------------------*
015000080219     C     VarCurLib     begsr
015100080219      *
015200120514     c                   eval      lenght=100
015300080219     c                   eval      comman =
015400080219     c                             'chgcurlib curlib(' + Wcurlib + ')'
015500080219
015600080219     c                   call      'QCMDEXC'
015700080219     c                   parm                    comman
015800080219     c                   parm                    lenght
015900080219
016000080219     c                   endsr
016100090302      *--------------------------------------------------------------------------------------------*
016200090302      * Invio E_mail per errore sbmjob
016300090302      *--------------------------------------------------------------------------------------------*
016400090302     C     Sr_PrtEmail   begsr
016500090302     c                   exsr      sr_override
016600090302     c                   open      PrtEMAIL
016700090302     c                   if        §mradreg=*blanks
016800090302     c                   eval      O_testo=jobuser+ ' - ' + SDSpgm +
016900090302     c                             ' - *REM* ' + %subst(§CM1var:7 :70)
017000090302     c                   except    prtdet
017100090302     c                   clear                   o_testo
017200090302     c                   except    prtdet
017300090302     c                   except    prtdet
017400090302     c                   endif
017500090304     c                   eval      o_testo='Profilo utente ' + profilo
017600090304     c                   eval      o_testo=%trim(o_testo) +
017700090304     c                             ' non trovato. Lavoro non sottomesso'
017800090302     c                   except    prtdet
017900090302     c* stampa una riga vuota
018000090302     c                   clear                   o_testo
018100090302     c                   except    prtdet
018200090302     c* chiusura dello spool
018300090302     c                   eval      o_testo=sdspgm
018400090302     c                   except    PrtDet
018500090302     c                   clear                   o_testo
018600090302     c                   except    PrtDet
018700090302     c                   eval      o_testo='***   Fine lista   ***'
018800090302     c                   except    PrtDet
018900090302     c                   close     prtemail
019000090302     c                   eval      qcmd=C_CmdDltOvr
019100090302     c                   eval      lenght=%size(qcmd)
019200090302     c                   call      'QCMDEXC'
019300090302     c                   parm                    qcmd
019400090302     c                   parm                    lenght
019500090302     c                   endsr
019600090302      /free
019700090302       //--------------------------------------------------------------
019800090302       //?Override al file di stampa per impostarvi i dati per
019900090302       //?  l'invio via e-mail   +   stampa inizio e-mail
020000090302       //--------------------------------------------------------------
020100090302       BEGSR sr_Override;
020200090302
020300090302         reset TRTCM1ds;
020400090302
020500090302
020600090302         IF  §MRAdreg <> *blank;
020700090302           §CM1mitt = %trim(§MRAdmitt);
020800090302           §CM1dst  = %trim(§MRAddest);
020900090302           §CM1tips = §MRAdreg;
021000090302           §CM1po   = C_Sede;
021100090302           §CM1var  = '*OBJM*' + §MRAddes;
021200090302           §CM1idp  = §MRAdidpro;
021300090302           Qcmd = C_CmdOvrPrtF
021400090302                + ' outq(' + %trim(§MRAdoutqi) + ')'
021500090302                + ' usrdfndta(''' + TRTCM1ds + ''')';
021600090302         ELSE;
021700090302           Qcmd = C_CmdOvrPrtF;
021800090302         ENDIF;
021900090302
022000090302         callp(e) qCmdExc(Qcmd : %size(Qcmd));
022100090302       ENDSR;
022200090302
022300090302       //--------------------------------------------------------------
022400090302       //?Operazioni iniziali.
022500090302       //--------------------------------------------------------------
022600090302       BEGSR RoutInz;
022700090302
022800090302       // Reperimento tab. "MRA"
022900090302         clear dMRAdan;
023000090302         clear kpjbu;
023100090302         reset tibs02ds;
023200090302         T02ke1 = SDSpgm;
023300090302         tibs02r(kpjba:tibs02ds);
023400090302         if T02err = *blank;
023500090302           dMRAdan = T02uni;
023600090302         endif;
023700090302      /end-free
023800120514     c
023900120514     c* Carico tabella CUR per sapere le filiali che hanno la CUR in lista
024000120521     c                   clear                   tibs40ds
024100120521     c                   call      'TIBS40R'
024200120521     c                   parm                    kpjba
024300120521     c                   parm                    tibs40ds
024400120514     c                   ENDSR
024500090302
024600090302     oPrtEMAIL  e            PRTdet      1
024700090302     o                       O_testo
