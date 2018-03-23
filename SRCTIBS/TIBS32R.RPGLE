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
017100170315     c* non lo faccio se utente della 046 o edpcedxxx
017200120515     c                   clear                   wcurlib
017300170315     c
017400170315    0c                   if        utefil<>46 and uteute<>'EDPCEDXXX'
017500120515     c                   eval      kcod='CUR'
017600120515     c     kcod          setll     tntbe01l
017700120515     c     kcod          reade     tntbe01l
017800120515    1c                   dow       not %eof(tntbe01l)
017900120515    2c                   if        tbeatb=' '
018000120515     c                   movel     tbeuni        dcur
018100120515     c*
018200120515    3c                   if        §curdde<=dateu
018300161116     c* verifico se caricamento per AREA
018400161116   3ac                   if        §curapl='A'
018500161116     c     §curfil       chain     azorg01l
018600161116    4c                   if        %found(azorg01l)
018700161116     c                   eval      curcar=orgcar
018800161116     c     v1cfil        chain     azorg01l
018900161116    5c                   if        %found(azorg01l) and orgcar=curcar
019000161116     c                   eval      wcurlib =tbeke1
019100161116    5c                   endif
019200161116    4c                   endif
019300161116 x3a c                   else
019400120515     c
019500120515     c* carico i secondi livello a oggi
019600120515     C                   CLEAR                   trul06ds
019700120515     c                   eval      D06tla='L'
019800120522    4c                   if        §curapl='T'
019900120515     C                   MOVE      '£1'          D06COD
020000120522     c                   else
020100120522     C                   MOVE      '£6'          D06COD
020200120522    4c                   endif
020300120515     C                   MOVEL     §curfil       D06KEY
020400120515     C                   MOVEL     §curdde       D06drf
020500120515     C                   MOVEL     trul06ds      KPJBU
020600120515     C*
020700120515     C                   CALL      'TRUL06R'
020800120515     C                   PARM                    KPJBA
020900120515     C                   MOVEL     KPJBU         trul06ds
021000120515
021100120515     c                   eval      xx=1
021200120515     c                   eval      xx=%lookup(v1cfil:lin)
021300120522    4c                   if        xx>0
021400120515     c                   eval      wcurlib =tbeke1
021500120515     c                   leave
021600120515    4c                   endif
021700120515     c
021800161116   3ac                   endif
021900161116    3c                   endif
022000120515    2c                   endif
022100120515     c*
022200120515     c     kcod          reade     tntbe01l
022300120515    1c                   enddo
022400170315    0c                   endif
022500120515     c
022600120515     c                   if        wcurlib=*blanks
022700120515     c                   eval      wcurlib=dftcurlib
022800120515     c                   endif
022900040723      *
023000040723      * DATI MODULO BASE
023100040826     c                   if        I32opz = '3'
023200040826     c     I32uco        chain(N)  KPPRF000
023300040826     c                   else
023400040830     c     v1cUTE        Chain(N)  KpPrf000
023500040826     c                   endif
023600040723      *   Profilo non iscritto
023700070530if  1c                   IF        not %FOUND(KpPrf01L)
023800040830     c                   eval      wESIMB = 'N'
023900040723     c                   eval      v1cTXT = *blanks
024000160823     c                   if        I32opz = '3'  and
024100160823     c                             SDSprm > 1
024200160823     c                   eval      V1Ctxt = TIBS31ds2.I31txt
024300160823     c                   endif
024400040728     c                   eval      v1cNSI = KNSIF
024500040830     c                   eval      v1cMSU = '1'
024600040830     c                   eval      v1cMST = '1'
024700040830     c                   eval      v1cAZA = '2'
024800040830     c                   eval      v1cAAU = '1'
024900070531     c                   eval      prfAAU = v1cAAU
025000070531     c                   eval      *in50  = (%subst(V1Cute:1:3)<>'EDP'
025100070531     c                                       and V1Cute<>'PROBAS    '
025200070531     c                                       and V1Cute<>'PROJMASTER')
025300070530x   1c                   ELSE
025400040723      *   Profilo iscritto
025500040830     c                   eval      wESIMB = 'S'
025600040723     c                   eval      v1cTXT = prfTXT
025700160823     c                   if        I32opz = '3'  and
025800160823     c                             SDSprm > 1
025900160823     c                   eval      V1Ctxt = TIBS31ds2.I31txt
026000160823     c                   endif
026100040723     c                   eval      v1cNSI = prfNSI
026200040830     c                   eval      v1cNMI = prfNMI
026300040830     c                   eval      v1cMSU = prfMSU
026400040830     c                   eval      v1cMST = prfMST
026500070530sel 2c                   select
026600070530w   2c                   when      %subst(V1Cute:1:3) = 'EDP'
026700070531     c                             or     V1Cute      = 'PROBAS    '
026800070531     c                             or     V1Cute      = 'PROJMASTER'
026900040830     c                   eval      v1cAZA = prfAZA
027000040830     c                   eval      *in50  = *off
027100070530w   2c                   when      I32opz = '5'
027200070530     c                   eval      v1cAZA = prfAZA
027300070530     c                   eval      *in50  = *on
027400070530x   2c                   other
027500040830     c                   eval      v1cAZA = '2'
027600040830     c                   eval      *in50  = *on
027700070530e   2c                   endsl
027800070531     c                   eval      v1cAAU = prfAAU
027900070530e   1c                   ENDIF
028000120523     c
028100040830      *   Libreria corrente proposta
028200120523     c* l'ambito non dipende più da FILTRA/GAITRA ma dalla filiale utente
028300120523     c
028400120523     c**                 select
028500120523     c**                 when      not %found(KPPRF01L)
028600120523     c**                 eval      v1cAMB = *blanks
028700120523     c**                 when      %subst(v1cNSI:1:6) = 'FILTRA'
028800120523     c**                 eval      v1cAMB = 'P'
028900120523     c**                 other
029000120523     c**                 eval      v1cAMB = 'S'
029100120523     c**                 endsl
029200120523     c
029300120523     c                   if        v1cfil=046
029400120523     c                   eval      v1cAMB = 'S'
029500120523     c                   else
029600120523     c                   eval      v1cAMB = 'P'
029700120523     c                   endif
029800120523     c
029900040726     c                   eval      w32Cmoba = wESIMB
030000130702
030100130702      *  imposto costante per STATO *enabled/*disabled
030200130702     c                   IF        I32opz = '1' and §UT2mopr = 'S'
030300130702     c                   eval      V1Csts = '*DISABLED'
030400130702     c                   ELSE
030500130702     c                   eval      V1Csts = '*ENABLED'
030600160822     c                   if        I32opz = '3'  and
030700160822     c                             SDSprm > 1
030800160822     c                   eval      V1Csts = TIBS31ds2.I31sts
030900160822     c                   endif
031000130702     c                   ENDIF
031100130708
031200130708      * se copia e il profilo non è uno STD proteggo Ambiente utilizzo e S.Informativo
031300130708     c                   IF        I32opz = '3' and §UT2mopr <> 'S'
031400130708     c                   eval      *in51 = *on
031500130708     c                   ELSE
031600130708     c                   eval      *in51 = *off
031700130708     c                   ENDIF
031800040723      *
031900040728      * DATI AS/400
032000040723      *  Passo i default che restano impostati se profilo non trovato
032100130702
032200040830     c                   call      'TIBS32C'
032300040830     c                   parm      'CHK'         w32Ctela
032400040830     c                   parm                    w32Cmoba
032500040830     c                   parm                    v1cUTE
032600040830     c                   parm      *blanks       wAStext
032700120515     c                   parm      wCURlib       v1cCUR
032800040830     c                   parm      'TNSY87C'     v1cPAT
032900040830     c                   parm      'GAITRAOBJ'   v1cLBA
033000040830     c                   parm      '*YES'        v1cSCP
033100040830     c                   parm      *blanks       v1cPSW
033200130702     c                   parm                    v1cSTS
033300040830     c                   parm      *blanks       wASerr
033400040723      *
033500040723     c                   IF        WASerr = 'E'
033600040830     c                   eval      wESIAS = 'N'
033700040729     c                   if        I32opz = '5'
033800040729     c                   clear                   v1cPAT
033900040729     c                   clear                   v1cLBA
034000040729     c                   clear                   v1cSTS
034100040729     c                   clear                   v1cSCP
034200040729     c                   clear                   v1cPSW
034300040729     c                   clear                   v1cCUR
034400040729     c                   else
034500111207     c*//                eval      v1cPSW = v1cUTE
034600111207     c* -?Impostazione nuova password?
034700160822     c                   if        I32opz = '3'  and
034800160822     c                             SDSprm > 1
034900160822     c                   eval      V1Cscp = TIBS31ds2.I31scp
035000160822     c                   eval      V1Cpsw = TIBS31ds2.I31psw
035100160822     c                   else
035200111207     c                   exsr      sr_NewPsw
035300111207     c                   eval      V1Cpsw = wPassword
035400160822     c                   endif
035500040729     c                   endif
035600040723     c                   ELSE
035700040830     c                   eval      wESIAS = 'S'
035800040830     c                   eval      v1cPSW = '*SAME'
035900040727     c                   IF        wAStext <> *blanks and v1cTXT = *blanks
036000040727     c                   eval      v1ctxt = wAStext
036100040723     c                   ENDIF
036200040727     c                   ENDIF
036300040826      *
036400040826      * Tento l'aggancio del record in gestione in caso di COPIA
036500040826     c                   IF        I32Opz = '3'
036600040826     c                   clear                   v1cTXT
036700160823     c                   if        I32opz = '3'  and
036800160823     c                             SDSprm > 1
036900160823     c                   eval      V1Ctxt = TIBS31ds2.I31txt
037000160823     c                   endif
037100040826     c     V1Cute        chain(N)  KPPRF000
037200040826     c                   if        not %FOUND(KpPrf01L)
037300040826     c                   eval      wESIMB = 'N'
037400040826     c                   endif
037500040826     c                   call      'TIBS32C'
037600040826     c                   parm      'CHK'         w32Ctela
037700040826     c                   parm                    w32Cmoba
037800040826     c                   parm                    v1cUTE
037900040826     c                   parm                    wAStext
038000040826     c                   parm                    v1cCUR
038100040826     c                   parm                    v1cPAT
038200040826     c                   parm                    v1cLBA
038300040826     c                   parm                    v1cSCP
038400040826     c                   parm                    v1cPSW
038500040826     c                   parm                    v1cSTS
038600040826     c                   parm      *blanks       wASerr
038700040826     c                   if        wASerr = 'E'
038800040826     c                   eval      wESIAS = 'N'
038900040826     c                   endif
039000040826     c                   ENDIF
039100040727      *
039200040826      * Imposto altri campi descrittivi / indicatori
039300040727     c                   IF        wESIMB = 'S'
039400040830     c                   eval      v1cPMB = '   Profilo presente   '
039500040727     c                   ELSE
039600040830     c                   eval      v1cPMB = ' Profilo NON presente '
039700040727     c                   ENDIF
039800040727      *
039900040727     c                   IF        wESIAS = 'S'
040000040830     c                   eval      v1cPAS = '   Profilo presente   '
040100040728     c                   eval      *in07  = *on
040200040727     c                   ELSE
040300040830     c                   eval      v1cPAS = ' Profilo NON presente '
040400040728     c                   eval      *in07  = *off
040500040727     c                   ENDIF
040600040723      *
040700040723      * Imposto campi a video a seconda dell'opzione della DS
040800040830     c                   SELECT
040900040723      * Inserimento
041000040830     c     I32Opz        Wheneq    '1'
041100040830     c     I32Opz        oreq      '3'
041200040830     c                   eval      *in01 = *on
041300040723     c                   eval      v1cMOB = 'S'
041400040723     c                   eval      v1cAS4 = 'S'
041500040723      * Modifica
041600040830     c     I32Opz        Wheneq    '2'
041700040830     c                   eval      *in02 = *on
041800040723     c                   eval      v1cMOB = wESIMB
041900040723     c                   eval      v1cAS4 = wESIAS
042000040723      * Visualizzazione
042100040830     c     I32Opz        Wheneq    '5'
042200040830     c                   eval      *in05 = *on
042300080912      /free
042400081125         //?Riabilitazione?
042500080912         when  I32opz = '7';
042600080912           *in02  = *on;
042700080912           v1cAS4 = wESIAS;
042800081125
042900081125         //?Disabilitazione?
043000081125         when  I32opz = '8';
043100081125           *in02  = *on;
043200081125           //V1Tope = '  DISABILITAZIONE   ';
043300081125           //DA01   = c_DspA_RedRI;
043400081125           v1cMOB = wESIMB;
043500081125           v1cAS4 = wESIAS;
043600080912      /end-free
043700040830     c                   ENDSL
043800040726      *
043900040726      * Gestione video
044000081125     c                   eval      $Fine = *off
044100081125     c                   dow       $Fine = *off
044200080912      /free
044300081125         select;
044400081125
044500081125           //?Riabilitazione profilo utente?
044600081125           when  I32opz = '7';
044700081125             exsr  sr_GesD02;
044800081125
044900081125           //?Disabilitazione profilo utente?
045000081125           when  I32opz = '8';
045100081125             exsr  sr_GesD03;
045200081125
045300081125           //?Visualizzazione/Modifica/Copia profilo utente?
045400081125           other;
045500080912      /end-free
045600040726     c                   exsr      GESVID
045700080912      /free
045800081125
045900081125         endsl;
046000080912      /end-free
046100040830     c                   enddo
046200040726      *
046300040830     c                   eval      *inLR = *on
046400040726      *--------------------------------------------------------------------------------------------*
046500040726      * GesVid - Gestione videata
046600040726      *--------------------------------------------------------------------------------------------*
046700040830     c     GESVID        BEGSR
046800160822      *
046900160823if  1c                   IF        SDSprm <= 1  or  $in28 = *on
047000040723      *
047100040726      * Emissione video
047200040830     c                   WRITE     Bs32T01
047300011114      *
047400011114      * Visualizzazione
047500040830     c     I32Opz        Ifeq      '5'
047600040830     c                   WRITE     Bs32D01
047700040830     c                   EXFMT     PROTECT
047800040830     c                   else
047900040726      * Immissione/Modifica
048000040830     c                   EXFMT     Bs32D01
048100040830     c                   endif
048200011114      *
048300040726      * F12=Ritorno
048400040830     c                   IF        *inkl = *on
048500040830     c                   clear                   o32Err
048600040830     c                   clear                   o32Msg
048700040830     c                   eval      o32Tfu = 'F12'
048800081125     c                   eval      $Fine  = *on                                 *fine pgm
048900040726     c                   ENDIF
049000040726      *
049100040726      * F07=Visualizza profilo
049200040830     c                   IF        *inkg = *on
049300040726     c                   eval      comman = 'DSPUSRPRF ' + v1cUTE
049400040726     c                   Call      'QCMDEXC'                            99
049500040726     c                   Parm                    Comman
049600040726     c                   Parm                    Lenght
049700040726     c                   ENDIF
049800160822      *
049900160822e   1c                   ENDIF
050000011122      *
050100040830     c                   IF        *inkl = *off  AND  *inkg = *off
050200040830     c                                           AND  *in05 = *off
050300011114      *
050400040726      * Controlli e decodifiche
050500040830     c                   EXSR      CtrVid
050600160822      *
050700160822      * -?Memorizza che è stato rilevato un errore in copia?
050800160822if  1c                   if        SDSprm > 1     and
050900160822     c                             I32opz = '3'   and
051000160822     c                             *in28  = *on
051100160822     c                   eval      $in28  = *on
051200160822e   1c                   endif
051300160822      * -?Procede alla conferma se richiamato per copia e?
051400160822      *  ?non rilevati errori?
051500160822if  1c                   if        SDSprm > 1     and
051600160822     c                             I32opz = '3'   and
051700160822     c                             *in28  = *off  and
051800160822     c                             $in28  = *off
051900160822     c                   eval      *inKF  = *on
052000160822e   1c                   endif
052100011114      *
052200040726      * Conferma
052300040830     c                   IF        *inKF = *ON  AND  *in28 = *off
052400040830     c                   EXSR      Conferma
052500160822     c                   if        wASerr <> *blank  or
052600160822     c                             *in28  =  *on
052700160822     c                   eval      $in28  =  *on
052800160822     c                   endif
052900040830     c                   if        wASerr = *blanks
053000040830     c                   clear                   o32Err
053100040830     c                   clear                   o32Msg
053200040830     c                   eval      o32Tfu = 'F06'
053300081125     c                   eval      $Fine  = *on                                 *fine pgm
053400040830     c                   endif
053500040830     c                   ENDIF
053600011114      *
053700040830     c                   ENDIF
053800011114      *
053900040729     c                   movel(p)  TIBS32ds      KPJBU
054000011114      *
054100040830     c                   endsr
054200011121      *-----------------------------------------------------------
054300040726      * Controllo videata
054400011121      *-----------------------------------------------------------
054500040830     c     CtrVid        BEGSR
054600011122      *
054700040830     c                   eval      *in28 = *off
054800040830     c                   clear                   V1CMsg
054900040830     c                   movea     '0000000000'  *in(30)
055000040726      *
055100040726      * Se non previsti aggiornamenti non eseguo controlli
055200040726      *
055300040830     c                   if        v1cMOB = 'S'
055400040726     c                   exsr      ctrMOB
055500040830     c                   endif
055600040726      *
055700040830     c                   if        v1cAS4 = 'S'  and  *in28 = *off
055800040726     c                   exsr      ctrAS4
055900040830     c                   endif
056000040726      *
056100040830     c                   ENDSR
056200040726      *-----------------------------------------------------------
056300040726      * Controllo dati MODULO BASE
056400040726      *-----------------------------------------------------------
056500040830     c     CtrMOB        BEGSR
056600040726      *
056700040726      * Descrizione
056800040830     c                   IF        v1ctxt = *blanks
056900040830     c                   seton                                        28  30
057000040830     c                   eval      V1CMsg = Msg(1)
057100040830     c                   ENDIF
057200040726     c   28              goto      EndCtrMob
057300011130      *
057400040726      * Sistema Informativo
057500040830     c                   IF        v1cNSI = *blanks
057600040830     c                   seton                                        28  32
057700040830     c                   eval      V1CMsg = Msg(2)
057800040830     c                   ELSE
057900040728     c     v1cNSI        chain     KfSif00
058000040830     c                   IF        NOT %FOUND(KfSif01L) or  ATKI1 <> *blanks
058100040830     c                   seton                                        28  32
058200040830     c                   eval      V1CMsg = Msg(3)
058300040830     c                   ENDIF
058400040830     c                   ENDIF
058500040726     c   28              goto      EndCtrMob
058600040726      *
058700040726      * Menù iniziale
058800040830     c                   IF        v1cnmi = *blanks
058900040830     c                   seton                                        28  33
059000040830     c                   eval      V1CMsg = Msg(4)
059100040830     c                   ELSE
059200040728     c     v1cNMI        chain     KpMnt000
059300040830     c                   IF        NOT %FOUND(KpMnt01L)
059400040830     c                   seton                                        28  33
059500040830     c                   eval      V1CMsg = Msg(5)
059600040830     c                   ENDIF
059700040830     c                   ENDIF
059800040726     c   28              goto      EndCtrMob
059900040726      *
060000040726      * Ambiente utilizzo
060100120523     c**                 IF        (%subst(v1cnsi:1:6) = 'FILTRA'
060200120523     c**                            AND v1cAMB <> 'P')   OR
060300120523     c**                           (%subst(v1cnsi:1:6) = 'GAITRA'
060400120523     c**                            AND v1cAMB <> 'S')
060500120523     c**                 seton                                        28  31
060600120523     c**                 eval      V1CMsg = Msg(6)
060700120523     c**                 ENDIF
060800040726     c   28              goto      EndCtrMob
060900020315      *
061000040830     c     EndCtrMob     ENDSR
061100040726      *-----------------------------------------------------------
061200040726      * Controllo dati PROFILO AS400
061300040726      *-----------------------------------------------------------
061400040830     c     CtrAS4        BEGSR
061500040726      *
061600040726      * Descrizione
061700040830     c                   IF        v1ctxt = *blanks
061800040830     c                   seton                                        28  30
061900040830     c                   eval      V1CMsg = Msg(1)
062000040830     c                   ENDIF
062100040727     c   28              goto      EndCtrAs4
062200040726      *
062300040830      * Libreria PGM tasto Sys.Attn
062400040830     c                   IF            v1cLBA =  *blanks
062500040830     c                             and v1cPAT <> *blanks
062600040830     c                             and v1cPAT <> '*NONE     '
062700040830     c                             and v1cPAT <> '*SYSVAL   '
062800040830     c                             and v1cPAT <> '*ASSIST   '
062900040830     c                   seton                                        28  35
063000040830     c                   eval      V1CMsg = Msg(7)
063100040830     c                   ENDIF
063200040727     c   28              goto      EndCtrAs4
063300040726      *
063400040830      * PGM tasto Sys.Attn
063500040830     c                   SELECT
063600040830     c                   When          v1cPAT =  '*NONE     '
063700040830     c                             or  v1cPAT =  '*SYSVAL   '
063800040830     c                             or  v1cPAT =  '*ASSIST   '
063900040830     c                   When      v1cPAT = *blanks
064000040830     c                   seton                                        28  34
064100040830     c                   eval      V1CMsg = Msg(8)
064200040830     c                   Other
064300040830     c                   call      'TIBS32C1'
064400040726     c                   parm                    V1CPAT
064500040726     c                   parm                    V1CLBA
064600040727     c                   parm      *blanks       W32c1ERR          1
064700040830     c                   if        W32c1ERR = 'E'
064800040830     c                   seton                                        28  34
064900040830     c                   eval      V1CMsg = Msg(9)
065000040830     c                   endif
065100040830     c                   ENDSL
065200040727     c   28              goto      EndCtrAs4
065300040726      *
065400040726      * PASSWORD
065500040729     c                   if        v1cPSW = *blanks
065600040830     c                   seton                                        28  36
065700040728     c                   eval      V1CMsg = Msg(10)
065800040729     c                   endif
065900040727     c   28              goto      EndCtrAs4
066000040726      *
066100040830      * (Re)Imposto libreria corrente (CURLIB) a seconda dell'ambiente
066200071005     c                   select
066300120522     c* le CURLIB che iniziamo per EDP non le tocco
066400071005     c                   when           V1Camb =  'P'
066500120515     c                             and  V1Ccur <> wCURlib
066600120522     c                             and  %subst(v1ccur:1:3)<>'EDP'
066700071005     c                   seton                                        28  37
066800071005     c                   eval      V1Cmsg =  %trimr(Msg(13))
066900071005     c                                    +  ' ' + V1Ccur
067000120515     c                   eval      V1Ccur =  wCURlib
067100071005     c                   when           V1Camb =  'S'
067200071005     c                             and  V1Ccur <> C_CrtDft
067300120522     c                             and  %subst(v1ccur:1:3)<>'EDP'
067400071005     c                   seton                                        28  37
067500071005     c                   eval      V1Cmsg =  %trimr(Msg(13))
067600071005     c                                    +  ' ' + V1Ccur
067700071005     c                   eval      V1Ccur =  C_CrtDft
067800071005     c                   other
067900120522     c
068000120522     c                   if        %subst(v1ccur:1:3)<>'EDP'
068100040830     c                   if        V1Camb =  'P'
068200120515     c                   eval      V1Ccur =  wCURlib
068300040830     c                   else
068400071005     c                   eval      V1Ccur =  C_CrtDft
068500040830     c                   endif
068600120522     c                   endif
068700120522     c
068800071005     c                   endsl
068900040726      * Ambiente utilizzo
069000040830     c                   IF        v1cCUR <> C_CrtDft  and  v1cAMB = 'S'
069100120522     c                             and  %subst(v1ccur:1:3)<>'EDP'
069200040830     c                   seton                                        28  31
069300040830     c                   eval      V1CMsg = Msg(11)
069400040830     c                   ENDIF
069500040727     c   28              goto      EndCtrAs4
069600040726      *
069700040830     c     EndCtrAs4     ENDSR
069800040726      *-----------------------------------------------------------
069900040726      * Scrittura/aggiornamento profili
070000040726      *-----------------------------------------------------------
070100040830     c     Conferma      BEGSR
070200040726      *
070300040830     c                   IF        v1cMOB = 'S'
070400040830     c     v1cUTE        chain     KPPRF000
070500040826     c                   if        NOT %found(KPPRF01L)
070600040826     c                   clear                   KPPRF000
070700040826     c                   eval      prfNMU = v1cUTE
070800040826     c                   endif
070900040830     c                   eval      prfNMI = v1cNMI
071000040726     c                   eval      prfNSI = v1cNSI
071100040826     c                   eval      prfTXT = v1cTXT
071200040830     c                   eval      prfAZA = v1cAZA
071300040830     c                   eval      prfMSU = v1cMSU
071400040830     c                   eval      prfMST = v1cMST
071500040830     c                   eval      prfAAU = v1cAAU
071600040726      *
071700040830     c                   if        %found(KPPRF01L)
071800040830     c                   UPDATE    KPPRF000
071900040830     c                   else
072000040830     c                   WRITE     KPPRF000
072100040830     c                   endif
072200040830     c                   ENDIF
072300040726      *
072400040729     c                   clear                   wASerr
072500040830     c                   IF        v1cAS4 = 'S'
072600040729      * Imposto se profilo da creare o da modificare
072700040830     c                   if        wESIAS = 'N' and v1cAS4 = 'S'
072800040729     c                   eval      w32Ctela = 'CRT'
072900040830     c                   else
073000040729     c                   eval      w32Ctela = 'CHG'
073100040830     c                   endif
073200040726      * Imposto se profilo iscritto a Modulo base
073300040830     c                   if        wESIMB = 'S' or v1cMOB = 'S'
073400040726     c                   eval      w32cmoba = 'S'
073500040830     c                   else
073600040726     c                   clear                   w32cmoba
073700040830     c                   endif
073800040726      *  Passo i default che restano impostati se profilo non trovato
073900040728     c                   movel(p)  v1ctxt        Wastext
074000040830     c                   call      'TIBS32C'
074100040830     c                   parm                    w32Ctela
074200040830     c                   parm                    w32Cmoba
074300040830     c                   parm                    v1cUTE
074400040830     c                   parm                    wAStext
074500040830     c                   parm                    v1cCUR
074600040830     c                   parm                    v1cPAT
074700040830     c                   parm                    v1cLBA
074800040830     c                   parm                    v1cSCP
074900040830     c                   parm                    v1cPSW
075000040830     c                   parm                    v1cSTS
075100040830     c                   parm      *blanks       wASerr
075200040728     c                   if        wASerr = 'E'
075300040830     c                   seton                                        28  36
075400040729     c                   eval      V1CMsg = Msg(12)
075500081125     c                   leavesr
075600040728     c                   endif
075700081125      /free
075800081125         // -?Aggiornamento AZUTE00F (impostaz. data fine validità a?
075900081125         //                          ?ieri  o  ad *hival)?
076000081125         if  I32opz = '8'   or   I32opz = '7';
076100081125           UPDATE AZUTE000;
076200081125         endif;
076300081125      /end-free
076400040830     c                   ENDIF
076500040726      *
076600040830     c                   ENDSR
076700080912      /free
076800080912       //--------------------------------------------------------------
076900081125       //?Gestione videata per la riabilitazione del profilo utente
077000080912       //--------------------------------------------------------------
077100080912       BEGSR  sr_GesD02;
077200080912
077300081125         //?Inizializzazione videata?
077400080912         if  $InzD02 = *on;
077500080912           exsr  sr_InzD02;
077600080912         endif;
077700080912
077800081125         //?Emissione window?
077900080912         //write  BS32T01;
078000080912         //write  BS32D01;
078100081125         //write  Protect;
078200080912         exfmt  BS32D02;
078300080912
078400080912         select;
078500080912
078600081125           //?F12=Ritorno?
078700080912           when *inKL;
078800080912             clear  O32err;
078900080912             clear  O32msg;
079000080912             O32tfu = 'F12';
079100081125             $Fine  = *on;
079200080912
079300081125           //?F7=Visualizzazione profilo?
079400080912           when *inKG;
079500080912             Comman = 'DSPUSRPRF ' + V2Cute;
079600080912             callp(e)  qcmdexc(Comman : Lenght);
079700080912
079800081125           //?Enter o F6=Conferma?
079900080912           other;
080000080912             //exsr  sr_CtrD02;
080100080912             if  *inKF   and   NOT *in28;
080200080912               exsr  sr_F06D02;
080300080912               if  wASerr = *blank;
080400080912                 clear  O32err;
080500080912                 clear  O32msg;
080600080912                 O32tfu = 'F06';
080700081125                 $Fine  = *on;
080800080912               endif;
080900080912             endif;
081000080912
081100080912         endsl;
081200080912
081300080912         KPJBU = TIBS32ds;
081400080912
081500080912       ENDSR;
081600080912
081700080912       //--------------------------------------------------------------
081800081125       //?Inizializzazione videata x la riabilitaz. del profilo utente
081900080912       //--------------------------------------------------------------
082000080912       BEGSR  sr_InzD02;
082100080912
082200080912         //clear  BS32d02;         (già fatto)
082300080912
082400080912         V2Cute  = V1Cute;
082500080912         V2Cfil  = V1Cfil;
082600080912         V2Ctxt  = V1Ctxt;
082700080912
082800080912         V2Cstso = V1Csts;
082900080912         V2Cstsn = '*ENABLED  ';
083000080912         V2Cscpo = V1Cscp;
083100080912         V2Cscpn = '*YES';
083200080912         V2Cpswo = V1Cpsw;
083300111205         //V2Cpswn = V1Cute;
083400111205
083500111205         // -?Impostazione nuova password?
083600111207         exsr  sr_NewPsw;
083700111207         V2Cpswn = wPassword;
083800080912
083900080912         $InzD02 = *off;
084000080912
084100080912       ENDSR;
084200080912
084300080912       //--------------------------------------------------------------
084400081125       //?Gestione conferma per la riabilitazione del profilo utente
084500080912       //--------------------------------------------------------------
084600080912       BEGSR  sr_F06D02;
084700080912
084800080912         V1Csts  = V2CstsN;
084900080912         V1Cscp  = V2CscpN;
085000080912         V1Cpsw  = V2CpswN;
085100081125
085200081125         UTEdsc = c_DataIsoHiVal;
085300080912
085400080912         exsr  Conferma;
085500080912
085600080912       ENDSR;
085700111207
085800111207       //--------------------------------------------------------------
085900111207       //?Creazione nuova password per l'utente in gestione
086000111207       //--------------------------------------------------------------
086100111207       BEGSR  sr_NewPsw;
086200111207
086300111207         clear  wPassword;
086400111207
086500111207         // ·?1° carattere = lettera reperita in base alla 1ª del nominativo?
086600111207         xx = %scan( %subst( %xlate( c_Minusc : c_Maiusc : V1Ctxt ) : 1 : 1 ) :
086700111207                     c_Maiusc );
086800111207         xx += 8;
086900111207         if  xx > %elem( sk_alfab );
087000111207           xx = %rem( xx : %elem( sk_alfab ) );
087100111207         endif;
087200111207         wPassword = sk_alfab(xx);
087300111207         // ·?2° carattere = lettera reperita in base alla 2ª del nominativo?
087400111207         xx = %scan( %subst( %xlate( c_Minusc : c_Maiusc : V1Ctxt ) : 2 : 1 ) :
087500111207                     c_Maiusc );
087600111207         xx -= 8;
087700111207         select;
087800111207           when  xx < 1;
087900111207             xx += %elem( sk_alfab );
088000111207           when  xx > %elem( sk_alfab );
088100111207             xx = %rem( xx : %elem( sk_alfab ) );
088200111207         endsl;
088300111207         wPassword = %trimr( wPassword )
088400111207                   + sk_alfab(xx);
088500111207         // ·?3°-5° carattere = millisecondi attuali?
088600111207         //        ?(eventuale sostituzione della prima cifra SE *zero)?
088700111207         // ·?6°-9° carattere = secondi attuali elevati al quadrato?
088800111207         xx      = ( %subdt( %timestamp() : *ms ) * 10 )
088900111207                 + ( %subdt( %timestamp() : *s ) *
089000111207                     %subdt( %timestamp() : *s ) );
089100111207         if  xx  < 1000000;
089200111207           xx += 9000000;
089300111207         endif;
089400111207         wPassword = %trimr( wPassword )
089500111207                   + %editc( xx : 'X' );
089600111207
089700111207       ENDSR;
089800081125
089900081125       //--------------------------------------------------------------
090000081125       //?Gestione videata per la disabilitazione del profilo utente
090100081125       //--------------------------------------------------------------
090200081125       BEGSR  sr_GesD03;
090300081125
090400081125         //?Inizializzazione videata?
090500081125         if  $InzD03 = *on;
090600081125           exsr  sr_InzD03;
090700081125         endif;
090800081125
090900081125         //?Emissione window?
091000081125         //write  BS32T01;
091100081125         //write  BS32D01;
091200081125         //write  Protect;
091300081125         exfmt  BS32D03;
091400081125
091500081125         select;
091600081125
091700081125           //?F12=Ritorno?
091800081125           when *inKL;
091900081125             clear  O32err;
092000081125             clear  O32msg;
092100081125             O32tfu = 'F12';
092200081125             $Fine  = *on;
092300081125
092400081125           //?F7=Visualizzazione profilo?
092500081125           when *inKG;
092600081125             Comman = 'DSPUSRPRF ' + V3Cute;
092700081125             callp(e)  qcmdexc(Comman : Lenght);
092800081125
092900081125           //?Enter o F6=Conferma?
093000081125           other;
093100081125             //exsr  sr_CtrD03;
093200081125             if  *inKF   and   NOT *in28;
093300081125               exsr  sr_F06D03;
093400081125               if  wASerr = *blank;
093500081125                 clear  O32err;
093600081125                 clear  O32msg;
093700081125                 O32tfu = 'F06';
093800081125                 $Fine  = *on;
093900081125               endif;
094000081125             endif;
094100081125
094200081125         endsl;
094300081125
094400081125         KPJBU = TIBS32ds;
094500081125
094600081125       ENDSR;
094700081125
094800081125       //--------------------------------------------------------------
094900081125       //?Inizializzazione videata x la disabilitaz. del profilo utente
095000081125       //--------------------------------------------------------------
095100081125       BEGSR  sr_InzD03;
095200081125
095300081125         //clear  BS32d03;         (già fatto)
095400081125
095500081125         V3Cute  = V1Cute;
095600081125         V3Cfil  = V1Cfil;
095700081125         V3Ctxt  = V1Ctxt;
095800081125
095900081125         V3Cstso = V1Csts;
096000081125         V3Cstsn = '*DISABLED ';
096100081125
096200081125         wDate   = %date( %dec(UTEdsc) : *ISO );
096300081125         V3CDSCO = %dec( wdate );
096400081125         wDate   = %date(*date) - %days(1);
096500081125         V3CDSCN = %dec( wDate );
096600081125
096700081125         $InzD03 = *off;
096800081125
096900081125       ENDSR;
097000081125
097100081125       //--------------------------------------------------------------
097200081125       //?Gestione conferma per la riabilitazione del profilo utente
097300081125       //--------------------------------------------------------------
097400081125       BEGSR  sr_F06D03;
097500081125
097600081125         V1Csts  = V3CstsN;
097700081125
097800081125         UTEdsc = %dec( %date( *date ) - %days(1) );
097900081125
098000081125         exsr  Conferma;
098100081125
098200081125       ENDSR;
098300080912      /end-free
098400011114      *--------------------------------------------------------------------------------------------*
098500011119      * Operazioni iniziali
098600011114      *--------------------------------------------------------------------------------------------*
098700040830     c     *inzsr        BEGSR
098800011114      *
098900011114      * Ricevimento parametri
099000040830     c     *ENTRY        PLIST
099100040830     c                   PARM                    kpjba
099200160822     c                   parm                    TIBS31ds2
099300040722      *
099400040830     c                   movel     kpjbu         tibs32ds
099500020408      *
099600040830     c                   z-add     1             CODUT
099700040830     c                   call      'X§PARUT'
099800040830     c                   parm                    UT§DSE0F
099900040830     c                   movel     RAGUT         RSUT
100000011114      *
100100040830     c                   ENDSR
100200040728      *--------------------------------------------------------------------------------------------*
100300111205** - sk_alfab +....2....+.
100400111205ABCDEFGHIJKLMNOPQRSTUVWXYZ
100500040726** -MSG-                                                                     *
100600040726TIBS32R- Descrizione obbligatoria                                               01
100700040726TIBS32R- Sistema informativo obbligatorio                                       02
100800040726TIBS32R- Sistema informativo inesistente o annullato                            03
100900040726TIBS32R- Menù iniziale obbligatorio                                             04
101000040726TIBS32R- Menù iniziale inesistente                                              05
101100040726TIBS32R- Ambiente utilizzo incongruente con Sistema Informativo                 06
101200040726TIBS32R- Indicare libreria PGM iniziale                                         07
101300040726TIBS32R- Indicare PGM iniziale o *NONE per nessun PGM iniziale                  08
101400040726TIBS32R- PGM inesistente nella libreria indicata                                09
101500040726TIBS32R- Immettere PASSWORD                                                     10
101600040726TIBS32R- Ambiente utilizzo incongruente con Libreria Corrente                   11
101700040729TIBS32R- Creazione/Manutenzione del profilo utente non riuscita - vedi JOBLOG   12
101800120522TIBS32R- con F6 verrà sistemata la libreria corrente: era                       13
