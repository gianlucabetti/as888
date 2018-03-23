000100080912     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200011203      *****************************************************************
000300011114      *
000400040722      * Gestione Profili utente - Modulo base e AS400
000500011114      *
000600011115      *****************************************************************
000700011115      *  RIEPILOGO INDICATORI
000800011115      ****************************************************************
000900040727      * 01      - Immissione
001000040727      * 02      - Modifica
001100040727      * 05      - Visualizzazione
001200040728      * 07      - Profilo utente esistente su AS/400
001300040728      * 28      - Errore generico
001400040830      * 30-39   - Riservati a gestione video (posizionamento cursore)
001500040830      * 50-59   - Riservati a gestione video (protezione campi)
001600040830      *
001700011114      *--------------------------------------------------------------------------------------------*
001800011114      * Data base                                                                                  *
001900011114      *--------------------------------------------------------------------------------------------*
002000040830     fKpPrf01l  Uf a e           k disk
002100081125     fAzUte01l  Uf   e           k disk
002200040830     fKpMnt01l  If   e           k disk
002300040830     fKfSif01l  If   e           k disk
002400120515     ftntbe01l  If   e           k disk
002500161116     fazorg01l  If   e           k disk
002600040830     fTibs32d   Cf   e             workstn
002700040830      *
002800011114      *--------------------------------------------------------------------------------------------*
002900011114      * Data structure                                                                             *
003000011114      *--------------------------------------------------------------------------------------------*
003100081125      *__________
003200040830      * Costanti
003300081125      *¯¯¯¯¯¯¯¯¯¯
003400040830     d C_CrtDft        c                   const('*CRTDFT   ')
003500081125     d c_DataIsoHiVal  c                   const(20391231)
003600081125
003700081125       // - Attributi di visualizzazione
003800081125     d c_DspA_Norm     c                   const(x'A0')
003900081125     d c_DspA_RI       c                   const(x'A1')
004000081125     d c_DspA_HI       c                   const(x'A2')
004100081125     d c_DspA_HIRI     c                   const(x'A3')
004200081125     d c_DspA_UL       c                   const(x'A4')
004300081125     d c_DspA_ULRI     c                   const(x'A5')
004400081125     d c_DspA_ULHI     c                   const(x'A6')
004500081125     d c_DspA_ND       c                   const(x'A7')
004600081125     d c_DspA_Red      c                   const(x'A8')
004700081125     d c_DspA_RedRI    c                   const(x'A9')
004800081125     d c_DspA_RedBL    c                   const(x'AA')
004900081125     d c_DspA_RedBLRI  c                   const(x'AB')
005000081125     d c_DspA_RedUL    c                   const(x'AC')
005100081125     d c_DspA_RedULRI  c                   const(x'AD')
005200081125     d c_DspA_RedULBL  c                   const(x'AE')
005300111205
005400111205       // -?Caratteri minuscoli?
005500111205     d c_Minusc        c                   const('abcdefghijklm+
005600111205     d                                            nopqrstuvwxyz+
005700111205     d                                            àáâãäå+
005800111205     d                                            æç+
005900111205     d                                            èéêë+
006000111205     d                                            ìíîï+
006100111205     d                                            òóôõö+
006200111205     d                                            ùúûü')
006300111205
006400111205       // -?Caratteri maiuscoli?
006500111205     d c_Maiusc        c                   const('ABCDEFGHIJKLM+
006600111205     d                                            NOPQRSTUVWXYZ+
006700111205     d                                            ÀÁÂÃÄÅ+
006800111205     d                                            ÆÇ+
006900111205     d                                            ÈÉÊË+
007000111205     d                                            ÌÍÎÏ+
007100111205     d                                            ÒÓÔÕÖ+
007200111205     d                                            ÙÚÛÜ')
007300081125      *_________________________________
007400081125      * Schiere a tempo di compilazione
007500081125      *¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
007600111205     d sk_alfab        s              1    dim(26) ctdata perrcd(26)
007700111205      *
007800071005     d Msg             s             78    dim(13) ctdata perrcd(1)             *Messaggi video
007900081125      *________________
008000081125      * Strutture dati
008100081125      *¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
008200040830     d Kpjba         e ds                                                       *architettura
008300040830     d UT§DSE0F      e ds
008400040830     d Tibs32Ds      e ds
008500160822     D Tibs31Ds2     e ds                  qualified
008600040830     d Tibs02Ds      e ds                  inz
008700040830     d dCUR          e ds                  inz
008800130702     d dUte02        e ds                  inz
008900120515?    D*
009000120515     D* DS PER TRUL06R - CARICAMENTO £X
009100120515     D trul06ds      E DS
009200120515     D  LIN                    1     90  0
009300120515     D                                     DIM(30)                              P.O. COMODO
009400080915
009500080915       // - Status
009600080915     d Psds           sds
009700080915     d   SDSpgm          *proc
009800160822     d   SDSprm          *parms
009900081125      *___________
010000081125      * Variabili
010100081125      *¯¯¯¯¯¯¯¯¯¯¯
010200081125     d $Fine           s               n   inz(*off)                            *fine programma
010300081125     d $InzD02         s               n   inz(*on)
010400081125     d $InzD03         s               n   inz(*on)
010500160822     d $in28           s               n   inz
010600081125      *
010700120515     d kcod            s              3    inz('CUR')
010800111207     d wPassword       s                   like(V2CpswN)  inz
010900111205     d xx              s              7  0 inz
011000040830     d w32Ctela        s              3    inz
011100040830     d w32Cmoba        s              1    inz
011200040830     d wAStext         s             50    inz
011300040830     d wASerr          s              1    inz
011400040830     d wEsiAS          s              1    inz
011500040830     d wEsiMB          s              1    inz
011600040830     d Comman          s             80    inz
011700040830     d Lenght          s             15  5 inz(80)
011800081125     d wDate           s               d   inz      datfmt(*eur)
011900120515     d dftcurlib       s             10    inz(C_CrtDft)
012000120515     d Dataiso         s               d   datfmt(*iso)
012100120515     d WCurlib         s             10
012200161116     d curcar          s                   like(orgcar)
012300040727
012400080912       //--------------------------------------------------------------
012500080912       //?Definizione procedure usate.
012600080912       //--------------------------------------------------------------
012700080912
012800080912      // - Esecuzione comando di sistema
012900080912     d qCmdExc         pr                  extpgm('QCMDEXC')
013000080912     d  Qcmd                        512    const  options(*varsize)
013100080912     d  Qlen                         15  5 const
013200040728      *
013300011114      *--------------------------------------------------------------------------------------------*
013400011114      * Main lines
013500011114      *--------------------------------------------------------------------------------------------*
013600040723      *
013700040826     c                   clear                   O32tfu
013800040826     c                   clear                   O32amb
013900040826     c                   clear                   O32apa
014000040826     c                   clear                   O32err
014100040826     c                   clear                   O32msg
014200120515     c
014300120515     c                   Move      *date         Dataiso
014400120515     c                   Move      Dataiso       Dateu             8 0
014500080912      /free
014600081125       DA01 = c_DspA_ND;
014700081125       clear  V1Tope;
014800080912       clear  BS32d02;
014900081125       clear  BS32d03;
015000080915       $InzD02  = *on;
015100081125       $InzD03  = *on;
015200080915
015300080915       V1Tpgm = SDSpgm;
015400080912      /end-free
015500040826      *
015600040727      * Imposto campi a video
015700081125      /free
015800081125       if  I32opz <> '7'   and   I32opz <> '8';
015900081125      /end-free
016000081125     c     I32UTE        Chain(n)  AZUTE000
016100081125      /free
016200081125       else;
016300081125         chain  (I32ute)  AZUTE000;
016400081125       endif;
016500081125      /end-free
016600040723     c                   eval      v1cUTE = I32UTE
016700040727     c                   eval      v1cFIL = uteFIL
016800130702     c                   eval      dUte02 = UTEfa1
016900040830      *
017000040830      *  Reperisco libreria corrente per p.o.
017100120515     c
017200120515     c                   clear                   wcurlib
017300120515     c                   eval      kcod='CUR'
017400120515     c     kcod          setll     tntbe01l
017500120515     c     kcod          reade     tntbe01l
017600120515    1c                   dow       not %eof(tntbe01l)
017700120515    2c                   if        tbeatb=' '
017800120515     c                   movel     tbeuni        dcur
017900120515     c*
018000120515    3c                   if        §curdde<=dateu
018100161116     c* verifico se caricamento per AREA
018200161116   3ac                   if        §curapl='A'
018300161116     c     §curfil       chain     azorg01l
018400161116    4c                   if        %found(azorg01l)
018500161116     c                   eval      curcar=orgcar
018600161116     c     v1cfil        chain     azorg01l
018700161116    5c                   if        %found(azorg01l) and orgcar=curcar
018800161116     c                   eval      wcurlib =tbeke1
018900161116    5c                   endif
019000161116    4c                   endif
019100161116 x3a c                   else
019200120515     c
019300120515     c* carico i secondi livello a oggi
019400120515     C                   CLEAR                   trul06ds
019500120515     c                   eval      D06tla='L'
019600120522    4c                   if        §curapl='T'
019700120515     C                   MOVE      '£1'          D06COD
019800120522     c                   else
019900120522     C                   MOVE      '£6'          D06COD
020000120522    4c                   endif
020100120515     C                   MOVEL     §curfil       D06KEY
020200120515     C                   MOVEL     §curdde       D06drf
020300120515     C                   MOVEL     trul06ds      KPJBU
020400120515     C*
020500120515     C                   CALL      'TRUL06R'
020600120515     C                   PARM                    KPJBA
020700120515     C                   MOVEL     KPJBU         trul06ds
020800120515
020900120515     c                   eval      xx=1
021000120515     c                   eval      xx=%lookup(v1cfil:lin)
021100120522    4c                   if        xx>0
021200120515     c                   eval      wcurlib =tbeke1
021300120515     c                   leave
021400120515    4c                   endif
021500120515     c
021600161116   3ac                   endif
021700161116    3c                   endif
021800120515    2c                   endif
021900120515     c*
022000120515     c     kcod          reade     tntbe01l
022100120515    1c                   enddo
022200120515     c
022300120515     c                   if        wcurlib=*blanks
022400120515     c                   eval      wcurlib=dftcurlib
022500120515     c                   endif
022600040723      *
022700040723      * DATI MODULO BASE
022800040826     c                   if        I32opz = '3'
022900040826     c     I32uco        chain(N)  KPPRF000
023000040826     c                   else
023100040830     c     v1cUTE        Chain(N)  KpPrf000
023200040826     c                   endif
023300040723      *   Profilo non iscritto
023400070530if  1c                   IF        not %FOUND(KpPrf01L)
023500040830     c                   eval      wESIMB = 'N'
023600040723     c                   eval      v1cTXT = *blanks
023700160823     c                   if        I32opz = '3'  and
023800160823     c                             SDSprm > 1
023900160823     c                   eval      V1Ctxt = TIBS31ds2.I31txt
024000160823     c                   endif
024100040728     c                   eval      v1cNSI = KNSIF
024200040830     c                   eval      v1cMSU = '1'
024300040830     c                   eval      v1cMST = '1'
024400040830     c                   eval      v1cAZA = '2'
024500040830     c                   eval      v1cAAU = '1'
024600070531     c                   eval      prfAAU = v1cAAU
024700070531     c                   eval      *in50  = (%subst(V1Cute:1:3)<>'EDP'
024800070531     c                                       and V1Cute<>'PROBAS    '
024900070531     c                                       and V1Cute<>'PROJMASTER')
025000070530x   1c                   ELSE
025100040723      *   Profilo iscritto
025200040830     c                   eval      wESIMB = 'S'
025300040723     c                   eval      v1cTXT = prfTXT
025400160823     c                   if        I32opz = '3'  and
025500160823     c                             SDSprm > 1
025600160823     c                   eval      V1Ctxt = TIBS31ds2.I31txt
025700160823     c                   endif
025800040723     c                   eval      v1cNSI = prfNSI
025900040830     c                   eval      v1cNMI = prfNMI
026000040830     c                   eval      v1cMSU = prfMSU
026100040830     c                   eval      v1cMST = prfMST
026200070530sel 2c                   select
026300070530w   2c                   when      %subst(V1Cute:1:3) = 'EDP'
026400070531     c                             or     V1Cute      = 'PROBAS    '
026500070531     c                             or     V1Cute      = 'PROJMASTER'
026600040830     c                   eval      v1cAZA = prfAZA
026700040830     c                   eval      *in50  = *off
026800070530w   2c                   when      I32opz = '5'
026900070530     c                   eval      v1cAZA = prfAZA
027000070530     c                   eval      *in50  = *on
027100070530x   2c                   other
027200040830     c                   eval      v1cAZA = '2'
027300040830     c                   eval      *in50  = *on
027400070530e   2c                   endsl
027500070531     c                   eval      v1cAAU = prfAAU
027600070530e   1c                   ENDIF
027700120523     c
027800040830      *   Libreria corrente proposta
027900120523     c* l'ambito non dipende più da FILTRA/GAITRA ma dalla filiale utente
028000120523     c
028100120523     c**                 select
028200120523     c**                 when      not %found(KPPRF01L)
028300120523     c**                 eval      v1cAMB = *blanks
028400120523     c**                 when      %subst(v1cNSI:1:6) = 'FILTRA'
028500120523     c**                 eval      v1cAMB = 'P'
028600120523     c**                 other
028700120523     c**                 eval      v1cAMB = 'S'
028800120523     c**                 endsl
028900120523     c
029000120523     c                   if        v1cfil=046
029100120523     c                   eval      v1cAMB = 'S'
029200120523     c                   else
029300120523     c                   eval      v1cAMB = 'P'
029400120523     c                   endif
029500120523     c
029600040726     c                   eval      w32Cmoba = wESIMB
029700130702
029800130702      *  imposto costante per STATO *enabled/*disabled
029900130702     c                   IF        I32opz = '1' and §UT2mopr = 'S'
030000130702     c                   eval      V1Csts = '*DISABLED'
030100130702     c                   ELSE
030200130702     c                   eval      V1Csts = '*ENABLED'
030300160822     c                   if        I32opz = '3'  and
030400160822     c                             SDSprm > 1
030500160822     c                   eval      V1Csts = TIBS31ds2.I31sts
030600160822     c                   endif
030700130702     c                   ENDIF
030800130708
030900130708      * se copia e il profilo non è uno STD proteggo Ambiente utilizzo e S.Informativo
031000130708     c                   IF        I32opz = '3' and §UT2mopr <> 'S'
031100130708     c                   eval      *in51 = *on
031200130708     c                   ELSE
031300130708     c                   eval      *in51 = *off
031400130708     c                   ENDIF
031500040723      *
031600040728      * DATI AS/400
031700040723      *  Passo i default che restano impostati se profilo non trovato
031800130702
031900040830     c                   call      'TIBS32C'
032000040830     c                   parm      'CHK'         w32Ctela
032100040830     c                   parm                    w32Cmoba
032200040830     c                   parm                    v1cUTE
032300040830     c                   parm      *blanks       wAStext
032400120515     c                   parm      wCURlib       v1cCUR
032500040830     c                   parm      'TNSY87C'     v1cPAT
032600040830     c                   parm      'GAITRAOBJ'   v1cLBA
032700040830     c                   parm      '*YES'        v1cSCP
032800040830     c                   parm      *blanks       v1cPSW
032900130702     c                   parm                    v1cSTS
033000040830     c                   parm      *blanks       wASerr
033100040723      *
033200040723     c                   IF        WASerr = 'E'
033300040830     c                   eval      wESIAS = 'N'
033400040729     c                   if        I32opz = '5'
033500040729     c                   clear                   v1cPAT
033600040729     c                   clear                   v1cLBA
033700040729     c                   clear                   v1cSTS
033800040729     c                   clear                   v1cSCP
033900040729     c                   clear                   v1cPSW
034000040729     c                   clear                   v1cCUR
034100040729     c                   else
034200111207     c*//                eval      v1cPSW = v1cUTE
034300111207     c* -?Impostazione nuova password?
034400160822     c                   if        I32opz = '3'  and
034500160822     c                             SDSprm > 1
034600160822     c                   eval      V1Cscp = TIBS31ds2.I31scp
034700160822     c                   eval      V1Cpsw = TIBS31ds2.I31psw
034800160822     c                   else
034900111207     c                   exsr      sr_NewPsw
035000111207     c                   eval      V1Cpsw = wPassword
035100160822     c                   endif
035200040729     c                   endif
035300040723     c                   ELSE
035400040830     c                   eval      wESIAS = 'S'
035500040830     c                   eval      v1cPSW = '*SAME'
035600040727     c                   IF        wAStext <> *blanks and v1cTXT = *blanks
035700040727     c                   eval      v1ctxt = wAStext
035800040723     c                   ENDIF
035900040727     c                   ENDIF
036000040826      *
036100040826      * Tento l'aggancio del record in gestione in caso di COPIA
036200040826     c                   IF        I32Opz = '3'
036300040826     c                   clear                   v1cTXT
036400160823     c                   if        I32opz = '3'  and
036500160823     c                             SDSprm > 1
036600160823     c                   eval      V1Ctxt = TIBS31ds2.I31txt
036700160823     c                   endif
036800040826     c     V1Cute        chain(N)  KPPRF000
036900040826     c                   if        not %FOUND(KpPrf01L)
037000040826     c                   eval      wESIMB = 'N'
037100040826     c                   endif
037200040826     c                   call      'TIBS32C'
037300040826     c                   parm      'CHK'         w32Ctela
037400040826     c                   parm                    w32Cmoba
037500040826     c                   parm                    v1cUTE
037600040826     c                   parm                    wAStext
037700040826     c                   parm                    v1cCUR
037800040826     c                   parm                    v1cPAT
037900040826     c                   parm                    v1cLBA
038000040826     c                   parm                    v1cSCP
038100040826     c                   parm                    v1cPSW
038200040826     c                   parm                    v1cSTS
038300040826     c                   parm      *blanks       wASerr
038400040826     c                   if        wASerr = 'E'
038500040826     c                   eval      wESIAS = 'N'
038600040826     c                   endif
038700040826     c                   ENDIF
038800040727      *
038900040826      * Imposto altri campi descrittivi / indicatori
039000040727     c                   IF        wESIMB = 'S'
039100040830     c                   eval      v1cPMB = '   Profilo presente   '
039200040727     c                   ELSE
039300040830     c                   eval      v1cPMB = ' Profilo NON presente '
039400040727     c                   ENDIF
039500040727      *
039600040727     c                   IF        wESIAS = 'S'
039700040830     c                   eval      v1cPAS = '   Profilo presente   '
039800040728     c                   eval      *in07  = *on
039900040727     c                   ELSE
040000040830     c                   eval      v1cPAS = ' Profilo NON presente '
040100040728     c                   eval      *in07  = *off
040200040727     c                   ENDIF
040300040723      *
040400040723      * Imposto campi a video a seconda dell'opzione della DS
040500040830     c                   SELECT
040600040723      * Inserimento
040700040830     c     I32Opz        Wheneq    '1'
040800040830     c     I32Opz        oreq      '3'
040900040830     c                   eval      *in01 = *on
041000040723     c                   eval      v1cMOB = 'S'
041100040723     c                   eval      v1cAS4 = 'S'
041200040723      * Modifica
041300040830     c     I32Opz        Wheneq    '2'
041400040830     c                   eval      *in02 = *on
041500040723     c                   eval      v1cMOB = wESIMB
041600040723     c                   eval      v1cAS4 = wESIAS
041700040723      * Visualizzazione
041800040830     c     I32Opz        Wheneq    '5'
041900040830     c                   eval      *in05 = *on
042000080912      /free
042100081125         //?Riabilitazione?
042200080912         when  I32opz = '7';
042300080912           *in02  = *on;
042400080912           v1cAS4 = wESIAS;
042500081125
042600081125         //?Disabilitazione?
042700081125         when  I32opz = '8';
042800081125           *in02  = *on;
042900081125           //V1Tope = '  DISABILITAZIONE   ';
043000081125           //DA01   = c_DspA_RedRI;
043100081125           v1cMOB = wESIMB;
043200081125           v1cAS4 = wESIAS;
043300080912      /end-free
043400040830     c                   ENDSL
043500040726      *
043600040726      * Gestione video
043700081125     c                   eval      $Fine = *off
043800081125     c                   dow       $Fine = *off
043900080912      /free
044000081125         select;
044100081125
044200081125           //?Riabilitazione profilo utente?
044300081125           when  I32opz = '7';
044400081125             exsr  sr_GesD02;
044500081125
044600081125           //?Disabilitazione profilo utente?
044700081125           when  I32opz = '8';
044800081125             exsr  sr_GesD03;
044900081125
045000081125           //?Visualizzazione/Modifica/Copia profilo utente?
045100081125           other;
045200080912      /end-free
045300040726     c                   exsr      GESVID
045400080912      /free
045500081125
045600081125         endsl;
045700080912      /end-free
045800040830     c                   enddo
045900040726      *
046000040830     c                   eval      *inLR = *on
046100040726      *--------------------------------------------------------------------------------------------*
046200040726      * GesVid - Gestione videata
046300040726      *--------------------------------------------------------------------------------------------*
046400040830     c     GESVID        BEGSR
046500160822      *
046600160823if  1c                   IF        SDSprm <= 1  or  $in28 = *on
046700040723      *
046800040726      * Emissione video
046900040830     c                   WRITE     Bs32T01
047000011114      *
047100011114      * Visualizzazione
047200040830     c     I32Opz        Ifeq      '5'
047300040830     c                   WRITE     Bs32D01
047400040830     c                   EXFMT     PROTECT
047500040830     c                   else
047600040726      * Immissione/Modifica
047700040830     c                   EXFMT     Bs32D01
047800040830     c                   endif
047900011114      *
048000040726      * F12=Ritorno
048100040830     c                   IF        *inkl = *on
048200040830     c                   clear                   o32Err
048300040830     c                   clear                   o32Msg
048400040830     c                   eval      o32Tfu = 'F12'
048500081125     c                   eval      $Fine  = *on                                 *fine pgm
048600040726     c                   ENDIF
048700040726      *
048800040726      * F07=Visualizza profilo
048900040830     c                   IF        *inkg = *on
049000040726     c                   eval      comman = 'DSPUSRPRF ' + v1cUTE
049100040726     c                   Call      'QCMDEXC'                            99
049200040726     c                   Parm                    Comman
049300040726     c                   Parm                    Lenght
049400040726     c                   ENDIF
049500160822      *
049600160822e   1c                   ENDIF
049700011122      *
049800040830     c                   IF        *inkl = *off  AND  *inkg = *off
049900040830     c                                           AND  *in05 = *off
050000011114      *
050100040726      * Controlli e decodifiche
050200040830     c                   EXSR      CtrVid
050300160822      *
050400160822      * -?Memorizza che è stato rilevato un errore in copia?
050500160822if  1c                   if        SDSprm > 1     and
050600160822     c                             I32opz = '3'   and
050700160822     c                             *in28  = *on
050800160822     c                   eval      $in28  = *on
050900160822e   1c                   endif
051000160822      * -?Procede alla conferma se richiamato per copia e?
051100160822      *  ?non rilevati errori?
051200160822if  1c                   if        SDSprm > 1     and
051300160822     c                             I32opz = '3'   and
051400160822     c                             *in28  = *off  and
051500160822     c                             $in28  = *off
051600160822     c                   eval      *inKF  = *on
051700160822e   1c                   endif
051800011114      *
051900040726      * Conferma
052000040830     c                   IF        *inKF = *ON  AND  *in28 = *off
052100040830     c                   EXSR      Conferma
052200160822     c                   if        wASerr <> *blank  or
052300160822     c                             *in28  =  *on
052400160822     c                   eval      $in28  =  *on
052500160822     c                   endif
052600040830     c                   if        wASerr = *blanks
052700040830     c                   clear                   o32Err
052800040830     c                   clear                   o32Msg
052900040830     c                   eval      o32Tfu = 'F06'
053000081125     c                   eval      $Fine  = *on                                 *fine pgm
053100040830     c                   endif
053200040830     c                   ENDIF
053300011114      *
053400040830     c                   ENDIF
053500011114      *
053600040729     c                   movel(p)  TIBS32ds      KPJBU
053700011114      *
053800040830     c                   endsr
053900011121      *-----------------------------------------------------------
054000040726      * Controllo videata
054100011121      *-----------------------------------------------------------
054200040830     c     CtrVid        BEGSR
054300011122      *
054400040830     c                   eval      *in28 = *off
054500040830     c                   clear                   V1CMsg
054600040830     c                   movea     '0000000000'  *in(30)
054700040726      *
054800040726      * Se non previsti aggiornamenti non eseguo controlli
054900040726      *
055000040830     c                   if        v1cMOB = 'S'
055100040726     c                   exsr      ctrMOB
055200040830     c                   endif
055300040726      *
055400040830     c                   if        v1cAS4 = 'S'  and  *in28 = *off
055500040726     c                   exsr      ctrAS4
055600040830     c                   endif
055700040726      *
055800040830     c                   ENDSR
055900040726      *-----------------------------------------------------------
056000040726      * Controllo dati MODULO BASE
056100040726      *-----------------------------------------------------------
056200040830     c     CtrMOB        BEGSR
056300040726      *
056400040726      * Descrizione
056500040830     c                   IF        v1ctxt = *blanks
056600040830     c                   seton                                        28  30
056700040830     c                   eval      V1CMsg = Msg(1)
056800040830     c                   ENDIF
056900040726     c   28              goto      EndCtrMob
057000011130      *
057100040726      * Sistema Informativo
057200040830     c                   IF        v1cNSI = *blanks
057300040830     c                   seton                                        28  32
057400040830     c                   eval      V1CMsg = Msg(2)
057500040830     c                   ELSE
057600040728     c     v1cNSI        chain     KfSif00
057700040830     c                   IF        NOT %FOUND(KfSif01L) or  ATKI1 <> *blanks
057800040830     c                   seton                                        28  32
057900040830     c                   eval      V1CMsg = Msg(3)
058000040830     c                   ENDIF
058100040830     c                   ENDIF
058200040726     c   28              goto      EndCtrMob
058300040726      *
058400040726      * Menù iniziale
058500040830     c                   IF        v1cnmi = *blanks
058600040830     c                   seton                                        28  33
058700040830     c                   eval      V1CMsg = Msg(4)
058800040830     c                   ELSE
058900040728     c     v1cNMI        chain     KpMnt000
059000040830     c                   IF        NOT %FOUND(KpMnt01L)
059100040830     c                   seton                                        28  33
059200040830     c                   eval      V1CMsg = Msg(5)
059300040830     c                   ENDIF
059400040830     c                   ENDIF
059500040726     c   28              goto      EndCtrMob
059600040726      *
059700040726      * Ambiente utilizzo
059800120523     c**                 IF        (%subst(v1cnsi:1:6) = 'FILTRA'
059900120523     c**                            AND v1cAMB <> 'P')   OR
060000120523     c**                           (%subst(v1cnsi:1:6) = 'GAITRA'
060100120523     c**                            AND v1cAMB <> 'S')
060200120523     c**                 seton                                        28  31
060300120523     c**                 eval      V1CMsg = Msg(6)
060400120523     c**                 ENDIF
060500040726     c   28              goto      EndCtrMob
060600020315      *
060700040830     c     EndCtrMob     ENDSR
060800040726      *-----------------------------------------------------------
060900040726      * Controllo dati PROFILO AS400
061000040726      *-----------------------------------------------------------
061100040830     c     CtrAS4        BEGSR
061200040726      *
061300040726      * Descrizione
061400040830     c                   IF        v1ctxt = *blanks
061500040830     c                   seton                                        28  30
061600040830     c                   eval      V1CMsg = Msg(1)
061700040830     c                   ENDIF
061800040727     c   28              goto      EndCtrAs4
061900040726      *
062000040830      * Libreria PGM tasto Sys.Attn
062100040830     c                   IF            v1cLBA =  *blanks
062200040830     c                             and v1cPAT <> *blanks
062300040830     c                             and v1cPAT <> '*NONE     '
062400040830     c                             and v1cPAT <> '*SYSVAL   '
062500040830     c                             and v1cPAT <> '*ASSIST   '
062600040830     c                   seton                                        28  35
062700040830     c                   eval      V1CMsg = Msg(7)
062800040830     c                   ENDIF
062900040727     c   28              goto      EndCtrAs4
063000040726      *
063100040830      * PGM tasto Sys.Attn
063200040830     c                   SELECT
063300040830     c                   When          v1cPAT =  '*NONE     '
063400040830     c                             or  v1cPAT =  '*SYSVAL   '
063500040830     c                             or  v1cPAT =  '*ASSIST   '
063600040830     c                   When      v1cPAT = *blanks
063700040830     c                   seton                                        28  34
063800040830     c                   eval      V1CMsg = Msg(8)
063900040830     c                   Other
064000040830     c                   call      'TIBS32C1'
064100040726     c                   parm                    V1CPAT
064200040726     c                   parm                    V1CLBA
064300040727     c                   parm      *blanks       W32c1ERR          1
064400040830     c                   if        W32c1ERR = 'E'
064500040830     c                   seton                                        28  34
064600040830     c                   eval      V1CMsg = Msg(9)
064700040830     c                   endif
064800040830     c                   ENDSL
064900040727     c   28              goto      EndCtrAs4
065000040726      *
065100040726      * PASSWORD
065200040729     c                   if        v1cPSW = *blanks
065300040830     c                   seton                                        28  36
065400040728     c                   eval      V1CMsg = Msg(10)
065500040729     c                   endif
065600040727     c   28              goto      EndCtrAs4
065700040726      *
065800040830      * (Re)Imposto libreria corrente (CURLIB) a seconda dell'ambiente
065900071005     c                   select
066000120522     c* le CURLIB che iniziamo per EDP non le tocco
066100071005     c                   when           V1Camb =  'P'
066200120515     c                             and  V1Ccur <> wCURlib
066300120522     c                             and  %subst(v1ccur:1:3)<>'EDP'
066400071005     c                   seton                                        28  37
066500071005     c                   eval      V1Cmsg =  %trimr(Msg(13))
066600071005     c                                    +  ' ' + V1Ccur
066700120515     c                   eval      V1Ccur =  wCURlib
066800071005     c                   when           V1Camb =  'S'
066900071005     c                             and  V1Ccur <> C_CrtDft
067000120522     c                             and  %subst(v1ccur:1:3)<>'EDP'
067100071005     c                   seton                                        28  37
067200071005     c                   eval      V1Cmsg =  %trimr(Msg(13))
067300071005     c                                    +  ' ' + V1Ccur
067400071005     c                   eval      V1Ccur =  C_CrtDft
067500071005     c                   other
067600120522     c
067700120522     c                   if        %subst(v1ccur:1:3)<>'EDP'
067800040830     c                   if        V1Camb =  'P'
067900120515     c                   eval      V1Ccur =  wCURlib
068000040830     c                   else
068100071005     c                   eval      V1Ccur =  C_CrtDft
068200040830     c                   endif
068300120522     c                   endif
068400120522     c
068500071005     c                   endsl
068600040726      * Ambiente utilizzo
068700040830     c                   IF        v1cCUR <> C_CrtDft  and  v1cAMB = 'S'
068800120522     c                             and  %subst(v1ccur:1:3)<>'EDP'
068900040830     c                   seton                                        28  31
069000040830     c                   eval      V1CMsg = Msg(11)
069100040830     c                   ENDIF
069200040727     c   28              goto      EndCtrAs4
069300040726      *
069400040830     c     EndCtrAs4     ENDSR
069500040726      *-----------------------------------------------------------
069600040726      * Scrittura/aggiornamento profili
069700040726      *-----------------------------------------------------------
069800040830     c     Conferma      BEGSR
069900040726      *
070000040830     c                   IF        v1cMOB = 'S'
070100040830     c     v1cUTE        chain     KPPRF000
070200040826     c                   if        NOT %found(KPPRF01L)
070300040826     c                   clear                   KPPRF000
070400040826     c                   eval      prfNMU = v1cUTE
070500040826     c                   endif
070600040830     c                   eval      prfNMI = v1cNMI
070700040726     c                   eval      prfNSI = v1cNSI
070800040826     c                   eval      prfTXT = v1cTXT
070900040830     c                   eval      prfAZA = v1cAZA
071000040830     c                   eval      prfMSU = v1cMSU
071100040830     c                   eval      prfMST = v1cMST
071200040830     c                   eval      prfAAU = v1cAAU
071300040726      *
071400040830     c                   if        %found(KPPRF01L)
071500040830     c                   UPDATE    KPPRF000
071600040830     c                   else
071700040830     c                   WRITE     KPPRF000
071800040830     c                   endif
071900040830     c                   ENDIF
072000040726      *
072100040729     c                   clear                   wASerr
072200040830     c                   IF        v1cAS4 = 'S'
072300040729      * Imposto se profilo da creare o da modificare
072400040830     c                   if        wESIAS = 'N' and v1cAS4 = 'S'
072500040729     c                   eval      w32Ctela = 'CRT'
072600040830     c                   else
072700040729     c                   eval      w32Ctela = 'CHG'
072800040830     c                   endif
072900040726      * Imposto se profilo iscritto a Modulo base
073000040830     c                   if        wESIMB = 'S' or v1cMOB = 'S'
073100040726     c                   eval      w32cmoba = 'S'
073200040830     c                   else
073300040726     c                   clear                   w32cmoba
073400040830     c                   endif
073500040726      *  Passo i default che restano impostati se profilo non trovato
073600040728     c                   movel(p)  v1ctxt        Wastext
073700040830     c                   call      'TIBS32C'
073800040830     c                   parm                    w32Ctela
073900040830     c                   parm                    w32Cmoba
074000040830     c                   parm                    v1cUTE
074100040830     c                   parm                    wAStext
074200040830     c                   parm                    v1cCUR
074300040830     c                   parm                    v1cPAT
074400040830     c                   parm                    v1cLBA
074500040830     c                   parm                    v1cSCP
074600040830     c                   parm                    v1cPSW
074700040830     c                   parm                    v1cSTS
074800040830     c                   parm      *blanks       wASerr
074900040728     c                   if        wASerr = 'E'
075000040830     c                   seton                                        28  36
075100040729     c                   eval      V1CMsg = Msg(12)
075200081125     c                   leavesr
075300040728     c                   endif
075400081125      /free
075500081125         // -?Aggiornamento AZUTE00F (impostaz. data fine validità a?
075600081125         //                          ?ieri  o  ad *hival)?
075700081125         if  I32opz = '8'   or   I32opz = '7';
075800081125           UPDATE AZUTE000;
075900081125         endif;
076000081125      /end-free
076100040830     c                   ENDIF
076200040726      *
076300040830     c                   ENDSR
076400080912      /free
076500080912       //--------------------------------------------------------------
076600081125       //?Gestione videata per la riabilitazione del profilo utente
076700080912       //--------------------------------------------------------------
076800080912       BEGSR  sr_GesD02;
076900080912
077000081125         //?Inizializzazione videata?
077100080912         if  $InzD02 = *on;
077200080912           exsr  sr_InzD02;
077300080912         endif;
077400080912
077500081125         //?Emissione window?
077600080912         //write  BS32T01;
077700080912         //write  BS32D01;
077800081125         //write  Protect;
077900080912         exfmt  BS32D02;
078000080912
078100080912         select;
078200080912
078300081125           //?F12=Ritorno?
078400080912           when *inKL;
078500080912             clear  O32err;
078600080912             clear  O32msg;
078700080912             O32tfu = 'F12';
078800081125             $Fine  = *on;
078900080912
079000081125           //?F7=Visualizzazione profilo?
079100080912           when *inKG;
079200080912             Comman = 'DSPUSRPRF ' + V2Cute;
079300080912             callp(e)  qcmdexc(Comman : Lenght);
079400080912
079500081125           //?Enter o F6=Conferma?
079600080912           other;
079700080912             //exsr  sr_CtrD02;
079800080912             if  *inKF   and   NOT *in28;
079900080912               exsr  sr_F06D02;
080000080912               if  wASerr = *blank;
080100080912                 clear  O32err;
080200080912                 clear  O32msg;
080300080912                 O32tfu = 'F06';
080400081125                 $Fine  = *on;
080500080912               endif;
080600080912             endif;
080700080912
080800080912         endsl;
080900080912
081000080912         KPJBU = TIBS32ds;
081100080912
081200080912       ENDSR;
081300080912
081400080912       //--------------------------------------------------------------
081500081125       //?Inizializzazione videata x la riabilitaz. del profilo utente
081600080912       //--------------------------------------------------------------
081700080912       BEGSR  sr_InzD02;
081800080912
081900080912         //clear  BS32d02;         (già fatto)
082000080912
082100080912         V2Cute  = V1Cute;
082200080912         V2Cfil  = V1Cfil;
082300080912         V2Ctxt  = V1Ctxt;
082400080912
082500080912         V2Cstso = V1Csts;
082600080912         V2Cstsn = '*ENABLED  ';
082700080912         V2Cscpo = V1Cscp;
082800080912         V2Cscpn = '*YES';
082900080912         V2Cpswo = V1Cpsw;
083000111205         //V2Cpswn = V1Cute;
083100111205
083200111205         // -?Impostazione nuova password?
083300111207         exsr  sr_NewPsw;
083400111207         V2Cpswn = wPassword;
083500080912
083600080912         $InzD02 = *off;
083700080912
083800080912       ENDSR;
083900080912
084000080912       //--------------------------------------------------------------
084100081125       //?Gestione conferma per la riabilitazione del profilo utente
084200080912       //--------------------------------------------------------------
084300080912       BEGSR  sr_F06D02;
084400080912
084500080912         V1Csts  = V2CstsN;
084600080912         V1Cscp  = V2CscpN;
084700080912         V1Cpsw  = V2CpswN;
084800081125
084900081125         UTEdsc = c_DataIsoHiVal;
085000080912
085100080912         exsr  Conferma;
085200080912
085300080912       ENDSR;
085400111207
085500111207       //--------------------------------------------------------------
085600111207       //?Creazione nuova password per l'utente in gestione
085700111207       //--------------------------------------------------------------
085800111207       BEGSR  sr_NewPsw;
085900111207
086000111207         clear  wPassword;
086100111207
086200111207         // ·?1° carattere = lettera reperita in base alla 1ª del nominativo?
086300111207         xx = %scan( %subst( %xlate( c_Minusc : c_Maiusc : V1Ctxt ) : 1 : 1 ) :
086400111207                     c_Maiusc );
086500111207         xx += 8;
086600111207         if  xx > %elem( sk_alfab );
086700111207           xx = %rem( xx : %elem( sk_alfab ) );
086800111207         endif;
086900111207         wPassword = sk_alfab(xx);
087000111207         // ·?2° carattere = lettera reperita in base alla 2ª del nominativo?
087100111207         xx = %scan( %subst( %xlate( c_Minusc : c_Maiusc : V1Ctxt ) : 2 : 1 ) :
087200111207                     c_Maiusc );
087300111207         xx -= 8;
087400111207         select;
087500111207           when  xx < 1;
087600111207             xx += %elem( sk_alfab );
087700111207           when  xx > %elem( sk_alfab );
087800111207             xx = %rem( xx : %elem( sk_alfab ) );
087900111207         endsl;
088000111207         wPassword = %trimr( wPassword )
088100111207                   + sk_alfab(xx);
088200111207         // ·?3°-5° carattere = millisecondi attuali?
088300111207         //        ?(eventuale sostituzione della prima cifra SE *zero)?
088400111207         // ·?6°-9° carattere = secondi attuali elevati al quadrato?
088500111207         xx      = ( %subdt( %timestamp() : *ms ) * 10 )
088600111207                 + ( %subdt( %timestamp() : *s ) *
088700111207                     %subdt( %timestamp() : *s ) );
088800111207         if  xx  < 1000000;
088900111207           xx += 9000000;
089000111207         endif;
089100111207         wPassword = %trimr( wPassword )
089200111207                   + %editc( xx : 'X' );
089300111207
089400111207       ENDSR;
089500081125
089600081125       //--------------------------------------------------------------
089700081125       //?Gestione videata per la disabilitazione del profilo utente
089800081125       //--------------------------------------------------------------
089900081125       BEGSR  sr_GesD03;
090000081125
090100081125         //?Inizializzazione videata?
090200081125         if  $InzD03 = *on;
090300081125           exsr  sr_InzD03;
090400081125         endif;
090500081125
090600081125         //?Emissione window?
090700081125         //write  BS32T01;
090800081125         //write  BS32D01;
090900081125         //write  Protect;
091000081125         exfmt  BS32D03;
091100081125
091200081125         select;
091300081125
091400081125           //?F12=Ritorno?
091500081125           when *inKL;
091600081125             clear  O32err;
091700081125             clear  O32msg;
091800081125             O32tfu = 'F12';
091900081125             $Fine  = *on;
092000081125
092100081125           //?F7=Visualizzazione profilo?
092200081125           when *inKG;
092300081125             Comman = 'DSPUSRPRF ' + V3Cute;
092400081125             callp(e)  qcmdexc(Comman : Lenght);
092500081125
092600081125           //?Enter o F6=Conferma?
092700081125           other;
092800081125             //exsr  sr_CtrD03;
092900081125             if  *inKF   and   NOT *in28;
093000081125               exsr  sr_F06D03;
093100081125               if  wASerr = *blank;
093200081125                 clear  O32err;
093300081125                 clear  O32msg;
093400081125                 O32tfu = 'F06';
093500081125                 $Fine  = *on;
093600081125               endif;
093700081125             endif;
093800081125
093900081125         endsl;
094000081125
094100081125         KPJBU = TIBS32ds;
094200081125
094300081125       ENDSR;
094400081125
094500081125       //--------------------------------------------------------------
094600081125       //?Inizializzazione videata x la disabilitaz. del profilo utente
094700081125       //--------------------------------------------------------------
094800081125       BEGSR  sr_InzD03;
094900081125
095000081125         //clear  BS32d03;         (già fatto)
095100081125
095200081125         V3Cute  = V1Cute;
095300081125         V3Cfil  = V1Cfil;
095400081125         V3Ctxt  = V1Ctxt;
095500081125
095600081125         V3Cstso = V1Csts;
095700081125         V3Cstsn = '*DISABLED ';
095800081125
095900081125         wDate   = %date( %dec(UTEdsc) : *ISO );
096000081125         V3CDSCO = %dec( wdate );
096100081125         wDate   = %date(*date) - %days(1);
096200081125         V3CDSCN = %dec( wDate );
096300081125
096400081125         $InzD03 = *off;
096500081125
096600081125       ENDSR;
096700081125
096800081125       //--------------------------------------------------------------
096900081125       //?Gestione conferma per la riabilitazione del profilo utente
097000081125       //--------------------------------------------------------------
097100081125       BEGSR  sr_F06D03;
097200081125
097300081125         V1Csts  = V3CstsN;
097400081125
097500081125         UTEdsc = %dec( %date( *date ) - %days(1) );
097600081125
097700081125         exsr  Conferma;
097800081125
097900081125       ENDSR;
098000080912      /end-free
098100011114      *--------------------------------------------------------------------------------------------*
098200011119      * Operazioni iniziali
098300011114      *--------------------------------------------------------------------------------------------*
098400040830     c     *inzsr        BEGSR
098500011114      *
098600011114      * Ricevimento parametri
098700040830     c     *ENTRY        PLIST
098800040830     c                   PARM                    kpjba
098900160822     c                   parm                    TIBS31ds2
099000040722      *
099100040830     c                   movel     kpjbu         tibs32ds
099200020408      *
099300040830     c                   z-add     1             CODUT
099400040830     c                   call      'X§PARUT'
099500040830     c                   parm                    UT§DSE0F
099600040830     c                   movel     RAGUT         RSUT
099700011114      *
099800040830     c                   ENDSR
099900040728      *--------------------------------------------------------------------------------------------*
100000111205** - sk_alfab +....2....+.
100100111205ABCDEFGHIJKLMNOPQRSTUVWXYZ
100200040726** -MSG-                                                                     *
100300040726TIBS32R- Descrizione obbligatoria                                               01
100400040726TIBS32R- Sistema informativo obbligatorio                                       02
100500040726TIBS32R- Sistema informativo inesistente o annullato                            03
100600040726TIBS32R- Menù iniziale obbligatorio                                             04
100700040726TIBS32R- Menù iniziale inesistente                                              05
100800040726TIBS32R- Ambiente utilizzo incongruente con Sistema Informativo                 06
100900040726TIBS32R- Indicare libreria PGM iniziale                                         07
101000040726TIBS32R- Indicare PGM iniziale o *NONE per nessun PGM iniziale                  08
101100040726TIBS32R- PGM inesistente nella libreria indicata                                09
101200040726TIBS32R- Immettere PASSWORD                                                     10
101300040726TIBS32R- Ambiente utilizzo incongruente con Libreria Corrente                   11
101400040729TIBS32R- Creazione/Manutenzione del profilo utente non riuscita - vedi JOBLOG   12
101500120522TIBS32R- con F6 verrà sistemata la libreria corrente: era                       13
