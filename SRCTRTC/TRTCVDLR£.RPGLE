000600080219     h decedit('0,') option(*nodebugio)
000700080219      *--------------------------------------------------------------------------------------------*
000701080219      * Pgm autostartjob che per ogni p.o. sottomette                                              *
000702090220      * un JOB per la ricezione dati da Positrova                                                  *
001000080219      *--------------------------------------------------------------------------------------------*
001100080219     fazorg01l  if   e           k disk
001200080219     fazute01l  if   e           k disk
001300080219     ftntbe01l  if   e           k disk
001301090302     fPrtEMAIL  O    f  132        printer  oflind(*inOF)  usropn
001400080219
001500080219     d parpo           s              3
001600080219     d profilo         s             10
001700080219     d nomejob         s             10
001800080219     d WCurlib         s             10
001900080219     d WCurlibDFT      s             10    inz('*CRTDFT')
002000080219     d comman          S            100                                         *comando CLP
002100080219     d lenght          S             15  5 INZ(100)                             *lunghezza comando
002200080219     d kcod            s              3    inz('CUR')
002300080219     d kke1            s             15
002301090302     d C_Sede          c                   const('046')
002302090302      // - Comando di override al PrtF
002303090302     d C_CmdOvrPrtF    c                   const('OVRPRTF +
002304090302     d                                           file(PRTEMAIL) +
002305090302     d                                           pagesize(66 132) +
002306090302     d                                           lpi(6) cpi(10) +
002307090302     d                                           ovrscope(*actgrpdfn) +
002308090302     d                                           ')
002309090302     d C_CmdDltOvr     c                   const('DLTOVR +
002310090302     d                                            file(PRTEMAIL) +
002311090302     d                                            lvl(*actgrpdfn)')
002312090302
002400080219     d dcur          e ds
002401120309     D dataiso         s               d   datfmt(*iso)
002402120309     D dateu           s              8  0
002403090302      // - Status
002404090302     d Psds           sds
002405090302     d   SDSpgm          *proc
002406090302     d   JobUser             254    263                                         User name
002407090302      // - Tabella "MRA" = Bart-Mailing -
002408090302     d dMRAdan       e ds                  inz
002409090302      // - Gestione tabelle: controllo e ricerca
002410090302     d TIBS02ds      e ds                  inz
002411090302     d   T02tla      e                     inz('L')
002412090302     d   T02mod      e                     inz('C')
002413090302     d   T02cod      e                     inz('MRA')
002414090302      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
002415090302     d TRTCM1ds      e ds                  inz
002426090302      //   ·§CM1sts  = Stato
002427090302     d   §CM1sts     e                     inz(*off)
002428090303      // Parametri
002429090303     d kpjba         e ds
002430090302      // - Campi per QCMDEXC
002431090302     d Qcmd            s            512    inz
002432090302      // - Campi di stampa
002433090302     d O_testo         s            132    inz
002500080219
002501090302      //---------------------------------------------------------------
002502090302      //?Definizione procedure esterne.
002503090302      //---------------------------------------------------------------
002504090302
002505090302      // - Esecuzione comando di sistema
002506090302     d qCmdExc         pr                  extpgm('QCMDEXC')
002507090302     d  Qcmd                        512    const  options(*varsize)
002508090302     d  Qlen                         15  5 const
002509090302      // - Gestione tabelle
002510090302     d tibs02r         pr                  extpgm('TIBS02R')
002511090302     d  kpjba                              likeds(KPJBA)    const
002512090302     d  tibs02ds                           likeds(TIBS02ds)
002600080219      *--------------------------------------------------------------------------------------------*
002700080219     c     Ktbe          klist                                                  *tntbe01l
002800080219     c                   kfld                    kcod                            -tabella
002900080219     c                   kfld                    kke1                            -chiave uno
003000080219      *--------------------------------------------------------------------------------------------*
003001120309     c                   time                    dataiso
003002120309     C                   move      dataiso       dateu
003100080219
003101090302     c                   exsr      routinz
003200080219      * elimino eventuale libreria corrente
003300080219     c                   eval      Wcurlib = Wcurlibdft
003400080219     C                   exsr      VarCurLib
003500080219     c
003600080219
003700080219     c     *loval        setll     azorg01l
003800080219     c                   do        *hival
003900080219     c                   read      azorg01l
004000080219
004100080219      * fine file
004200080219    1c                   if        %eof(azorg01l)
004300080219     c                   leave
004400080219     c                   endif
004500080219
004600080219      * escludo gli annullati e se non è Filiale o Agenzia
004700080219     c                   if        orgfva <> *blanks or
004800080219     c                             (orgfag <> 'F' and orgfag <> 'A')
004900080219     c                   iter
005000080219     c                   endif
005100080219
005200080219     c                   move      orgfil        parpo
005300080219
005400080219      * verifico esistenza profilo
005500090220     C                   eval      profilo = 'VDL' + parpo
005700080219     c     profilo       chain     azute000
005800120309     c                   if        %found  and dateu<=utedsc
005900080219
006000080219      * verifico se impostare libreria personalizzazioni
006100080219     c                   clear                   dcur
006200080219     c                   movel(p)  parpo         kke1
006300080219     c     Ktbe          chain     TNTBE000
006400080219if  1c                   if             %found(TNTBE01L)
006500080219     c                             and  TBEatb = *blanks
006600080219     c                   movel     TBEuni        dcur
006700080219      *
006800080219      *  imposto libreria personalizzazioni
006900080219     c                   if        §curlib <> *blanks
007000080219     c                   eval      Wcurlib = §curlib
007100080219     C                   exsr      VarCurLib
007200080219     c                   endif
007300080219     c                   endif
007400080219
007500080219      * sottometto JOB
007501090302     c                   call      'TRTCVDLC'
007502090302     c                   parm                    parpo
007503090302     c                   parm                    profilo
007504090302     c                   parm      ' '           esito             1
007505090302     c                   if        esito='E'
007506090302     c                   exsr      sr_PrtEmail
007507090302     c                   endif
008400080219
008500080219     c                   endif
008600080219
008700080219      * REimposto "NESSUNA" libreria corrente
008800080219     c                   if        Wcurlib <> Wcurlibdft
008900080219     c                   eval      Wcurlib = Wcurlibdft
009000080219     C                   exsr      VarCurLib
009100080219     c                   endif
009200080219
009300080219     c                   enddo
009400080219
009500080219     c                   eval      *inlr = *on
009600080219      *--------------------------------------------------------------------------------------------*
009700080219      * modifico libreria corrente
009800080219      *--------------------------------------------------------------------------------------------*
009900080219     C     VarCurLib     begsr
010000080219      *
010100080219     c                   eval      comman =
010200080219     c                             'chgcurlib curlib(' + Wcurlib + ')'
010300080219
010400080219     c                   call      'QCMDEXC'
010500080219     c                   parm                    comman
010600080219     c                   parm                    lenght
010700080219
010800080219     c                   endsr
010900090302      *--------------------------------------------------------------------------------------------*
011000090302      * Invio E_mail per errore sbmjob
011100090302      *--------------------------------------------------------------------------------------------*
011200090302     C     Sr_PrtEmail   begsr
011201090302     c                   exsr      sr_override
011202090302     c                   open      PrtEMAIL
011203090302     c                   if        §mradreg=*blanks
011204090302     c                   eval      O_testo=jobuser+ ' - ' + SDSpgm +
011205090302     c                             ' - *REM* ' + %subst(§CM1var:7 :70)
011206090302     c                   except    prtdet
011207090302     c                   clear                   o_testo
011208090302     c                   except    prtdet
011209090302     c                   except    prtdet
011210090302     c                   endif
011211090304     c                   eval      o_testo='Profilo utente ' + profilo
011212090304     c                   eval      o_testo=%trim(o_testo) +
011213090304     c                             ' non trovato. Lavoro non sottomesso'
011214090302     c                   except    prtdet
011215090302     c* stampa una riga vuota
011216090302     c                   clear                   o_testo
011217090302     c                   except    prtdet
011218090302     c* chiusura dello spool
011219090302     c                   eval      o_testo=sdspgm
011220090302     c                   except    PrtDet
011221090302     c                   clear                   o_testo
011222090302     c                   except    PrtDet
011223090302     c                   eval      o_testo='***   Fine lista   ***'
011224090302     c                   except    PrtDet
011225090302     c                   close     prtemail
011226090302     c                   eval      qcmd=C_CmdDltOvr
011227090302     c                   eval      lenght=%size(qcmd)
011228090302     c                   call      'QCMDEXC'
011229090302     c                   parm                    qcmd
011230090302     c                   parm                    lenght
011300090302     c                   endsr
011301090302      /free
011302090302       //--------------------------------------------------------------
011303090302       //?Override al file di stampa per impostarvi i dati per
011304090302       //?  l'invio via e-mail   +   stampa inizio e-mail
011305090302       //--------------------------------------------------------------
011306090302       BEGSR sr_Override;
011307090302
011309090302         reset TRTCM1ds;
011310090302
011313090302
011316090302         IF  §MRAdreg <> *blank;
011317090302           §CM1mitt = %trim(§MRAdmitt);
011318090302           §CM1dst  = %trim(§MRAddest);
011319090302           §CM1tips = §MRAdreg;
011320090302           §CM1po   = C_Sede;
011321090302           §CM1var  = '*OBJM*' + §MRAddes;
011322090302           §CM1idp  = §MRAdidpro;
011323090302           Qcmd = C_CmdOvrPrtF
011324090302                + ' outq(' + %trim(§MRAdoutqi) + ')'
011325090302                + ' usrdfndta(''' + TRTCM1ds + ''')';
011326090302         ELSE;
011327090302           Qcmd = C_CmdOvrPrtF;
011328090302         ENDIF;
011329090302
011330090302         callp(e) qCmdExc(Qcmd : %size(Qcmd));
011400090302       ENDSR;
011401090302
011402090302       //--------------------------------------------------------------
011403090302       //?Operazioni iniziali.
011404090302       //--------------------------------------------------------------
011405090302       BEGSR RoutInz;
011406090302
011409090302       // Reperimento tab. "MRA"
011410090302         clear dMRAdan;
011411090302         clear kpjbu;
011412090302         reset tibs02ds;
011413090302         T02ke1 = SDSpgm;
011414090302         tibs02r(kpjba:tibs02ds);
011415090302         if T02err = *blank;
011416090302           dMRAdan = T02uni;
011417090302         endif;
011418090302       endsr;
011419090302      /end-free
011420090302
011421090302     oPrtEMAIL  e            PRTdet      1
011500090302     o                       O_testo
