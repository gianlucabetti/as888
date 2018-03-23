000100110427       //==============================================================
000200110427       //?FIOR18R *   Stampa 4 Ordini Ritiro Merce in foglio A4        ?
000300110427       //==============================================================
000400110427       //
000500110427       //?A T T E N Z I O N E :                                        ?
000600110427       //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯                                        ?
000700110427       // Il programma ha 3 printer file esterni:
000800110427       //   FIOR18P    per O.R.M. nazionale (4 O.R.M. x A4)
000900110427       //   FIOR16P2   per O.R.M. estero
001000110427       //   ORMest     per O.R.M. estero da inviare via fax
001100110427       //              uguale al FIOR16P2, ma senza lettere accentate
001200110427       //
001300110427       // Gli ORM della FedEx (se mai dovessero essere immessi):
001400110427       //   non vanno con il fax ma esce il modulo ORM
001500110427       //
001600110427       //--------------------------------------------------------------
001700110427
001800110427       //--------------------------------------------------------------
001900110427       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
002000110427       //--------------------------------------------------------------
002100110427
002200110427     /*PRM dftactgrp(*no) actgrp(*caller) dbgview(*source)
002300110427     /*END
002400110427
002500110427       //--------------------------------------------------------------
002600110427       //?Specifiche di controllo.                                     ?
002700110427       //--------------------------------------------------------------
002800031117
002900081210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003000081210     h dftactgrp(*no) actgrp(*caller)
003100031117
003200110427       //--------------------------------------------------------------
003300110427       //?Dichiarazione file.                                          ?
003400110427       //--------------------------------------------------------------
003500031117
003600031117     fFNORM06L  if   e           k disk    usropn
003700031117     f                                     rename(FNORM000:FNORM06)
003800031117     fFNORM11L  if   e           k disk    usropn
003900031117     f                                     rename(FNORM000:FNORM11)
004000031117      *
004100081209     fFNORM01L  if   e           k disk    infds(ORMinf)
004200140127     fFNORT11L  if   e           k disk
004300131129     fFNORE01L  if   e           k disk
004400081209     fFIAR404L  if   e           k disk
004500081210     fFIAR601L  if   e           k disk
004600120612     ffidst01l  if   e           k disk
004700031117     fFIAPD01L  if   e           k disk
004800031117     fFNSPE01L  if   e           k disk
004900060123     fFNSP201L  if   e           k disk
005000031117     fEDTAB01L  if   e           k disk
005100081209     fTFNTC01L  if   e           k disk
005200081209      *
005300081209     fFIDPO01L  if   e           k disk
005400031117      *
005500031117     fAZORG01L  if   e           k disk
005600031117     fTABEL00F  if   e           k disk
005700130118     fTNTBE01L  if   e           k disk
005800031117      *
005900081209     fFNORM00F  Uf   e             disk    rename(FNORM000 : FNORM00)
006000081210      *
006100081209     fFNORF00F  o    e           k disk
006200031117      *
006300081210     fFIOR18P   o    e             printer usropn
006400081209     fFIOR16P2  o    e             printer usropn
006500081209     fORMest    o    e             printer usropn
006600031117
006700110427       //--------------------------------------------------------------
006800110427       //?Definizione costanti.                                        ?
006900110427       //--------------------------------------------------------------
007000031117
007100110427       // -?Numero dei Box per Pagina "A4"?
007200081210     d $BxP            c                   const(4)
007300110427
007400110427       // -?Spessore delle linee in inches/pollici (*memo)?
007500081222     d c_LineNarrow    c                   const(0.008)
007600081222     d c_LineMedium    c                   const(0.017)
007700081222     d c_LineWide      c                   const(0.025)
007800110427
007900120330       // -?Dimensioni base per il simil-logo BRT?
008000120330      /copy gaitrasrc/srcTNSY,LogoBRT_R1
008100110427
008200110427       // -?Posizioni "base" per:?
008300120330       //  ?- logo Brt in Pagina "A4"?
008400110428       //  ?· posizione superiore?
008500110428     d $PS0            c                   const(0.260)
008600110428       //  ?· posizione centrale?
008700110428     d $PS0_c          c                   const(0.350)
008800110428       //  ?· posizione inferiore?
008900110428     d $PS0_i          c                   const(0.440)
009000110427       //  ?- barcode?
009100031215     d $PS1            c                   const(2.350)
009200110427       //  ?- linee orizzontali (alta e bassa) del singolo box?
009300031117     d $LOA            c                   const(0.200)
009400031127     d $LOB            c                   const(2.350)
009500031127     d $LOC            c                   const(2.342)
009600031211     d $LOD            c                   const(2.825)
009700110427       //  ?- varie linee orizzontali interne al box?
009800031117     d $LO1            c                   const(0.525)
009900031117     d $LO2            c                   const(0.850)
010000031117     d $LO3            c                   const(1.500)
010100031117     d $LO4            c                   const(1.840)
010200110502
010300110502       // -?Intestazioni in stampa?
010400110502     d $BRT            c                   const('BRT S.p.A.')
010500110427
010600110427       // -?Messaggio di avvertimento per allocazione?
010700081209     d MsgJob          c                   const('Si sta bloccando la stampa de-
010800081209     d                                     gli ORM: USCIRE SUBITO da questo lav-
010900081209     d                                     oro!')
011000130118
011100130118      // - Tabella OSR
011200130118     d c_tabosr        c                   const('OSR')
011300110427
011400110427       //--------------------------------------------------------------
011500110427       //?Definizione schiere.                                         ?
011600110427       //--------------------------------------------------------------
011700110427
011800110427       // -?Schiere x Partner?
011900031117     d $Ksc            s                   like(§PTksc)  dim(999)
012000031117     d $Lnp            s                   like(§PTlnp)  dim(999)
012100031117     d $KeyPT          s                   like(TABkey)  dim(999)
012200130509     d skPGMTR         s                   like(§POpgmtrso) dim(999)
012300130522     d skSIelenco      s                   like(§POsndprtf) dim(999)
012400110427       // -?Schiera x P.O. della £6?
012500031117     d $L6             s                   like(R16poe)  dim(30) inz
012600110427       // -?Note?
012700140127     d $Note           s                   like(ORMno1)  dim(04) inz
012800110427       // -?Quantità (editate) - a sx - e Unità di misura - a dx -?
012900031128     d $QUM            s             20    dim(3)  inz
013000151126
013100151126       // -?Schiere x Nuovo Invio File ai Partner?
013200151126     d skPOR           s                   like(ORMpor)  dim(999)
013300151126     d skKSU           s                   like(§NASksu) dim(999)
013400151201     d skFIE           s                   like(§NASfie) dim(999)
013500110427
013600110427       //--------------------------------------------------------------
013700110427       //?Definizione strutture dati.                                  ?
013800110427       //--------------------------------------------------------------
013900110427
014000110427       // -?Parametri ricevuti?
014100031117     d KPJBA         e ds
014200031117     d FIOR16DS      e ds                  inz
014300151126
014400151126       // -?Ds file FNORM da passare al TIS7TBOR
014500151126     d FNORMDS       e ds                  extname(FNORM00F)
014600110427
014700110427       // -?Parametri x Controllo profilo utenti?
014800050307     d TIBS34ds      e ds
014900110427       // -?Ds di riferimento al file esterno AZUTE00F?
015000050307     d AZUTEds       e ds                  extname(AZUTE00F)
015100110427       // -?Ds per dati organigramma?
015200050307     d dDATIUTE      e ds
015300090129
015400110427       // -?Parametri per override ad un file di stampa?
015500090129     d trulOvrPds    e ds                  inz
015600110427
015700110427       // -?Dati da AZORG00F?
015800071106     d Og143         e ds                  inz
015900071106     d Og147         e ds                  inz
016000110427
016100110427       // -?Flag operativi di FNORM00F?
016200110427     d dORM01        e ds                  inz
016300131129
016400131129       // -?Dati per trc "O " del file FNORE (orari apertura)?
016500131129     d dOREorari     e ds
016600161116
016700161116       // -?Dati per trc "DT" del file FNORE (data pronta merce)?
016800161116     d dOREdt        e ds
016900110427
017000110427       // -?Parametri per TRUL06R - caricamento £x?
017100050307     d TRUL06ds      e ds                  inz
017200031117     d  Lin                    1     90  0 dim(30)
017300110427
017400110427       // -?Controllo/Decodifica codice cliente?
017500050307     d TIBS69ds      e ds                  inz
017600031117     d ds_CNACO      e ds                  inz  extname(CNACO00F)
017700031117     d ds_CNIND      e ds                  inz  extname(CNIND00F)
017800031117     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
017900031117     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
018000110427
018100130508       // -?Tabelle PT e PU = Partner EDI estero e PR - PO
018200130508     d EDIdsPO       e ds
018300130508     d EDIdsPR       e ds
018400110427     d EDIdsPT       e ds
018500110427     d EDIdsPU       e ds
018600110427       // -?Tabella 15 = Nazioni?
018700031117     d ds15          e ds                  inz
018800110427       // -?Tabella 3I/DPD = Variabili x DPD?
018900081209     d ds3Idp        e ds
019000110427       // -?Tabella FAR = Fasi O.R.M.?
019100031117     d dFAR          e ds                  inz
019200110427       // -?Tabella OES = Dati standard per O.R.M. Estero?
019300031117     d dOES          e ds                  inz
019400110427       // -?Tabella OSR = Gestione Serie O.R.M.?
019500031117     d dOSR          e ds                  inz
019600151126
019700151126       // -?Tabella ALP - Abbinamento Linea/Partner
019800151126     d dALP          e ds                  inz
019900151126
020000110427       // -?Interrogazione/Controllo tabelle?
020100110427     d TIBS02ds      e ds                  inz
020200110427
020300110427       // -?Calcolo instradamento cappario DPD GeoPost?
020400081209     d TISIE3ds      e ds                  inz
020500110427       // -?Reperimento cod.cliente di un depot DPD?
020600081209     d TRULDEPds     e ds                  inz
020700110427       // -?Scrittura del FIDPO00F?
020800081209     d FIEU49ds      e ds                  inz
020900110427
021000110427       // -?Reperimento USRDFNDTA x comunicazioni via e-mail?
021100081209     d TRUL44ds      e ds                  inz
021200110427       // -?Ridefinizione dati utenti estesi x mailing PDF?
021300081209     d TRTCM1ds      e ds                  inz
021400110427
021500110427       // -?Gestione rcd allocato?
021600081209     d TRUL82ds      e ds
021700130509
021800130509       // -?DS per passaggio parametri x pgm trasmissione ORM a partner
021900130520     d TRTCTORMds    e ds                  inz
022000110427
022100110427       // -?InfDS?
022200081209     d ORMinf          ds
022300081209     d  ORMnrr               397    400i 0
022400110427
022500110427       // -?Controllo/Inversione date di 8 cifre?
022600031117     d WLBDAT          ds                  inz
022700031117     d  G02dat                 1      8  0 inz
022800031117     d  G02inv                 9     16  0 inz
022900031117     d  G02err                17     17    inz
023000031117     d  G02tgi                18     22  0 inz
023100110427
023200110427       // -?Codice a Barre della LDV?
023300081209     d BARcodeDS       ds            14    inz
023400081209     d  DSkeyOSR               1      5  0 inz
023500151126     d   wORMpoe               1      3  0 inz
023600151126     d   wORMnsr               4      5  0 inz
023700151126     d  wORMnor                6     12  0 inz
023800151126     d  wORMnrv               13     14  0 inz
023900110427
024000110427       // -?Scomposizione codice cliente?
024100031117     d DS_ksc          ds             7    inz
024200031117     d  DsKsc_POR              1      3  0 inz
024300060921     d  dsksc_e3ddep           4      7  0 inz
024400110427
024500110427       //--------------------------------------------------------------
024600110427       //?Definizione variabili globali.                               ?
024700110427       //--------------------------------------------------------------
024800151126
024900151126       // -?Parametri per pgm. "TIS7TBOR"?
025000151126     d pin_Opz         s              5a
025100151126     d pin_KSU         s              8a   inz('00000000')
025200151126     d pOut_Esito      s              1a
025300110427
025400110427       // -?Parametri per pgm. "TRULOUTQ"?
025500040218     d PrmFine         s              1
025600040218     d PrmFil          s              3s 0
025700040218     d PrmTpOutQ       s              1
025800040218     d PrmOutQ         s             10
025900040218     d PrmOutQLib      s             10
026000040218     d PrmEsito        s              1
026100110427
026200110427       // -?Parametri per pgm. "QCMDEXC"?
026300031117     d Qcmd            s            156    inz
026400031117     d Qlen            s             15  5 inz(156)
026500060123     d Qcmdm           s            395    inz
026600060123     d Qlenm           s             15  5 inz(395)
026700110427
026800110427       // -?Indici di schiera?
026900031117     d XX              s              3  0 inz
027000040713     d XY              s              3  0 inz
027100031117     d YY              s              3  0 inz
027200130520     d zz              s              4  0 inz
027300110427
027400110427       // -?Flags booleani?
027500110427     d $Estero         s               n   inz
027600110427     d $DPD            s               n   inz
027700110427     d $PrtNote        s               n   inz
027800130509     d wOKtraduttore   s               n   inz
027900130522     d wSIelenco       s               n   inz
028000151126     d NewTradPartner  s               n   inz
028100160413     d wOkTrul44       s               n   inz
028200110427
028300110427       // -?Campi chiave x archivi?
028400031117     d kSPEfls         s                   like(SPEfls)  inz('L')
028500031117     d kSPEcli         s                   like(SPEcli)  inz
028600031117     d kSPEcod         s                   like(SPEcod)  inz('002')
028700060123     d kSP2tpe         s                   like(SP2tpe)  inz('EM')
028800060214     d kAR4trc         s                   like(AR4trc)  inz('M')
028900060214     d kAR4n14         s                   like(AR4n14)  inz
029000110427     d kNTCapl         s                   like(NTCapl)  inz('C')
029100110427     d kNTCnk1         s                   like(NTCnk1)  inz
029200110427     d kNTCnk2         s                   like(NTCnk2)  inz
029300110427     d kNTCtnt         s                   like(NTCtnt)  inz('83')
029400140127     d kORTgst         s                   like(ORTgst)  inz('S')
029500110427
029600110427       // -?Dati del giorno?
029700031117     d w0140           s             14  0
029800031117     d DateU           s              8  0
029900071106     d Data_Org        s              8  0 inz
030000110427
030100110427       // -?Contatori: da stampare, in fase di stampa, posiz. in "A4"?
030200031117     d wPage           s              5  0 inz
030300031117     d wBoxP           s              1  0 inz
030400110427       // -?Posizione iniziale nel calcolo delle posizioni per box?
030500031117     d wPosIni_B       s              5  3 inz
030600110427
030700110427       // -?Campi di comodo?
030800130521     d wfcs            s                   like(ORMfcs)
030900081209     d w0040           s              4  0 inz
031000031117     d w0112           s             11  2 inz
031100081209     d wMail           s             60    inz
031200081209     d Pormdpd         s             12    inz
031300081209     d SavORMdst       s                   like(ORMdst)  inz
031400130118      // - Campo di comodo per Ordinante ORM lungo 7
031500130118     d wORMcor         s              7s 0 inz
031600090129
031700090423       //--------------------------------------------------------------
031800131129       //?Definizione procedure usate.                                 ?
031900090423       //--------------------------------------------------------------
032000090129
032100110427       // -?Override ai PrtF?
032200090129     d trulOvrPr       pr                  extpgm('TRULOVRPR')
032300090129     d  trulOvrPds                         likeds(trulOvrPds)
032400151126
032500151126       // -?Invio ORM ai Partner via File
032600151126     d tis7tbor        pr                  extpgm('TIS7TBOR')
032700151126     d  pin_Opz                       5a
032800151126     d  pin_KSU                       8a
032900151126     d  fnormds                            likeds(FNORMDS)
033000151126     d  pOut_Esito                    1a
033100131129
033200131129       //--------------------------------------------------------------
033300131129       //?Definizione key-list.                                        ?
033400131129       //--------------------------------------------------------------
033500131129
033600131129       // -?File FNORE01L?
033700131129     d keyFNORE01    e ds                  extname(FNORE01L:*key)
033800131129     d                                     prefix(k_)
033900131129     d                                     inz
034000041122
034100031117      *===============================================================*
034200081209      *? RIEPILOGO INDICATORI                                        ?*
034300031117      *---------------------------------------------------------------*
034400110502      *?04   ?- Stampa Euroexpress                                    *
034500110502      *?05   ?- Stampa DPD                                            *
034600110502      *?06   ?- SOLLECITO                                             *
034700110502      *?10   ?- ORM ESTERO VIA MAIL                                   *
034800110502      *?11   ?- ORM ESTERO VIA FILE                                   *
034900110502      *?20   ?- Condiziona "grassetto" in stampa su FIOR16P2          *
035000110502      *?21   ?- Comodo                                                *
035100110502      *?41   ?- Posizionamento nel 1° ¼ del foglio A4                 *
035200110502      *?42   ?- Posizionamento nel 2° ¼ del foglio A4                 *
035300110502      *?43   ?- Posizionamento nel 3° ¼ del foglio A4                 *
035400110502      *?44   ?- Posizionamento nel 4° ¼ del foglio A4                 *
035500110502      *?50   ?- Stampa BarCode                                        *
035600110502      *?60   ?- *Date >= §OGDDAO    (nuova stampa "aut + d:xxxxx")    *
035700110502      *?61   ?- Ora ritiro <= 12:30 (stampata in grassetto)           *
035800110502      *?62   ?- Stampa dati ORM per Ritiro Contestuale in grassetto   *
035900110502      *?70   ?- Stampa Rimesso Mittente: 1ª fase (stampa O.R.M.)      *
036000110502      *?71   ?- Stampa Rimesso Mittente: 2ª fase (dopo O.R.M.)        *
036100110502      *?90   ?- Errore QCmdExc                                        *
036200031117      *===============================================================*
036300110427
036400110427       //--------------------------------------------------------------
036500110427       //?Specifiche di calcolo.                                       ?
036600110427       //--------------------------------------------------------------
036700031117
036800031117     c     *Entry        plist
036900031117     c                   parm                    KPJBA
037000110427
037100031117     c                   movel     KPJBU         FIOR16ds
037200110427
037300110427       // -?Tipo lancio (DB0tla):?"C" -           Chiudo con LR?
037400110427       //                        ?"L" - Elaboro e chiudo con LR?
037500110427       //                        ?" " - Elaboro e chiudo in RETRN?
037600031117     c     R16tla        cabeq     'C'           Fine
037700110427
037800110427       // -?Operazioni iniziali?
037900110427     c                   exsr      sr_Start
038000031117     c     *in90         cabeq     *on           Fine
038100110427
038200110427       // -?Stampa:?
038300031117if  1c                   if        R16nor > *zeros
038400110427       //   ·?Singolo ORM richiesto?
038500031117     c                   exsr      sr_ORM_1
038600031117x   1c                   else
038700110427       //   ·?Tutti gli ORM della £6?
038800031117     c                   exsr      sr_ORM_2
038900031117e   1c                   endif
039000110427
039100110427       // -?Operazioni finali?
039200031117     c     Fine          tag
039300110427     c                   exsr      sr_End
039400031117      *
039500031117      *---------------------------------------------------------------*
039600081209      *?*INZSR     * Routine iniziale                                ?*
039700031117      *---------------------------------------------------------------*
039800031117     c     *InzSr        BEGSR
039900031204      *
040000031204     c                   if        %subst(KPJBU:1:1) = 'C'
040100110427     c                   leavesr
040200031204     c                   endif
040300031117      *
040400050307      * Reperisco dati aziendali
040500050307     c     *dtaara       define    §azute        AZUTEds
040600050307     c     *dtaara       define    §datiute      dDATIUTE
040700050307     c                   in(E)     *dtaara
040800050307     c                   if        %ERROR or RSUT = *blanks
040900050307     c                   clear                   Tibs34Ds
041000050307     c                   call      'TIBS34R'
041100050307     c                   parm                    Tibs34Ds
041200050307     c                   in        *dtaara
041300050307     c                   endif
041400050307      *
041500050307     c                   eval      TBLkut = 1
041600031117      *
041700031117      * Carico tabella "PU" - SOLO PARTNER
041800031117     c                   clear                   XX
041900031117     c                   movel     'PU'          TABcod
042000031117     c     TABcod        setll     EDTAB000
042100031117     c     TABcod        reade     EDTAB000
042200031117do  1c                   DOW       NOT %eof(EDTAB01L)
042300031117if  2c                   if        TABflg = *blanks
042400031117     C                   movel     TABuni        EDIdsPU
042500031117if  3c                   if        §PUtpc = 'P'
042600031117     c                   add       1             xx
042700031117     C                   movel     TABkey        $KeyPT(xx)
042800031117e   3C                   endif
042900031117e   2C                   endif
043000031117     C     TABcod        reade     EDTAB000
043100031117e   1C                   ENDDO
043200031117      *
043300031117      * Carico tabella "PT"
043400031117     c                   movel     'PT'          TABcod
043500031117do  1c                   DO        xx            yy
043600031117     c                   movel     $KeyPT(yy)    TABkey
043700031117     c     kEDTAB        chain     EDTAB000
043800031117if  2c                   if            %found(EDTAB01L)
043900031117     c                             and TABflg = *blanks
044000031117     c                   movel     TABuni        EDIdsPT
044100031117     c                   z-add     §PTksc        $Ksc(yy)
044200031117     c                   z-add     §PTlnp        $Lnp(yy)
044300031117e   2c                   endif
044400031117e   1c                   ENDDO
044500130508
044600130508      /free
044700130508       //?Per ogni partner caricato aggangio anche la tabella "PO"
044800130508       //?mi serve per controllare se invio automativo al partner
044900130520         zz = 1;
045000130520         FOR zz by 1 to %elem($keyPT);
045100130521           IF  $keyPT(zz) <> *blanks;
045200130508             clear EDIdsPO;
045300130508             TABcod = 'PO';
045400130520             TABkey = $keyPT(zz);
045500130508             chain (TABcod:TABkey) EDTAB01L;
045600130508             IF  %found(EDTAB01L);
045700130508               EDIdsPO = TABuni;
045800130508             ENDIF;
045900130522             skPGMTR(zz)    = §POpgmtrso;
046000130522             skSIelenco(zz) = §POsndprtf;
046100130508           ENDIF;
046200130508         ENDFOR;
046300130508      /end-free
046400081209      *
046500110427      * carico tabella 3I/DPD
046600081209     c                   clear                   ds3Idp
046700081209     c                   movel     '3I'          TBLcod
046800081209     c                   movel(p)  'DPD'         TBLkey
046900081209     c     ktabel        chain     TABEL
047000081209     c                   if        %found(TABEL00F) and TBLflg = *blanks
047100081209     c                   eval      ds3Idp = TBLuni
047200060921     c                   endif
047300040218      *
047400040218      * Cerco il nome della coda del fax
047500040218     c                   clear                   PRMFine
047600050307     c                   eval      PRMFil    = DUTpou
047700040218     c                   eval      PRMTpOutq = '2'
047800040218     c                   clear                   PRMOutQ
047900040218     c                   clear                   PRMOutQLib
048000040218     c                   clear                   PRMEsito
048100040218     c                   call      'TRULOUTQ'
048200040218     c                   parm                    PRMFine
048300040218     c                   parm                    PRMFil
048400040218     c                   parm                    PRMTpOutQ
048500040218     c                   parm                    PRMOutQ
048600040218     c                   parm                    PRMOutQLib
048700040218     c                   parm                    PRMEsito
048800040218if  1c                   if            PRMEsito <> *off
048900040218     c                             or  PRMOutQ  =  *blanks
049000040218     c                   movel(p)  '*JOB'        PRMoutq
049100040218e   1c                   endif
049200031117      *
049300031117      * Inverto la *Date
049400031117     c                   time                    w0140
049500031117     c                   clear                   WLBdat
049600031117     c                   move      w0140         G02dat
049700031117     c                   call      'XSRDA8'
049800031117     c                   parm                    WLBdat
049900031117     c                   z-add     G02inv        DateU
050000031117      *
050100031117      * Azzeramento dei campi generici/"totali"
050200031117     c*** non serve:     clear                   wPage
050300031117     c*** non serve:     clear                   wBoxP
050400031117     c*** non serve:     clear                   wPosIni_B
050500151126
050600151126      /free
050700151126       //?Carico in sk la tabella ALP - Abbinamento Linea/Partner
050800151126       //?per invio a modo nuovo dei dati al partner
050900151126         clear dALP;
051000151126         clear xx;
051100151126         TBEcod = 'ALP';
051200151126         setll (TBEcod) TNTBE01L;
051300151126         reade (TBEcod) TNTBE01L;
051400151126         DOW  not %eof(TNTBE01L);
051500151211           IF  TBEke2 = *blanks;
051600151211             dALP = TBEuni;
051700151211             xx += 1;
051800151211             skPOR(xx) = %dec(%subst(TBEke1:1:3):3:0);
051900151211             skKSU(xx) = §NASksu;
052000151211             skFIE(xx) = §NASfie;
052100151211           ENDIF;
052200151126           reade (TBEcod) TNTBE01L;
052300151126         ENDDO;
052400151126      /end-free
052500031117      *
052600110427     c                   ENDSR
052700031117      *
052800031117      *---------------------------------------------------------------*
052900110427      *?sr_START   * Operazioni iniziali                             ?*
053000031117      *---------------------------------------------------------------*
053100110427     c     sr_Start      BEGSR
053200031117      *
053300031117      * Carica tutte le filiali gestite dalla filiale di gestione (£6)
053400031117if  1c                   IF        R16nor = *zeros
053500031117     c                   clear                   $L6
053600031117     c                   clear                   TRUL06DS
053700031117     c                   move      '£6'          D06cod
053800031117     c                   movel     'L'           D06tla
053900031117     c                   movel     R16por        D06key
054000031117     c                   movel     TRUL06DS      KPJBU
054100031117     c                   call      'TRUL06R'
054200031117     c                   parm                    KPJBA
054300031117     c                   movel     KPJBU         TRUL06DS
054400031117     c                   movea     Lin           $L6
054500130509e   1c                   ENDIF
054600031117      *
054700031117      * Esegue la override al file di stampa
054800081209if  1c                   if        NOT %open(FIOR18P)
054900031117if  2c                   if        R16mdo = *blanks
055000081211     c                   eval      R16mdo = '*ORMA4    '
055100031117e   2c                   endif
055200090129      /free
055300090129         clear  TrulOvrPds;
055400090930         uopToDo   = 'A';
055500090129         uopPrtF   = 'FIOR18P   ';
055600090129         uopOutQ   = R16oqo;
055700090129         uopFrmTyp = R16mdo;
055800090129         uopSave   = '*NO ';
055900090129         trulOvrPr ( trulOvrPds );
056000090129         Qcmd  = uopCmd;
056100090129      /end-free
056200031117     c                   call      'QCMDEXC'                            90
056300031117     c                   parm                    Qcmd
056400031117     c                   parm                    Qlen
056500081209     c                   OPEN      FIOR18P
056600031117e   1c                   endif
056700031117      *
056800031117      * Apre i file di input a seconda della richiesta
056900081210if  1c                   IF        R16ris = 'R' or R16ris = 'X'
057000031117      * - Se richiesta la ristampa apro il file FNORM06L
057100061006      *   e sollecito
057200031117if  2c                   if        NOT %open(FNORM06L)
057300031117     c                   open      FNORM06L
057400031117e   2c                   endif
057500031117x   1c                   else
057600031117      * - Se richiesta la   stampa apro il file FNORM11L
057700031117if  2c                   if        NOT %open(FNORM11L)
057800031117     c                   open      FNORM11L
057900031117e   2c                   endif
058000031117e   1c                   endif
058100031117      *
058200031117     c                   ENDSR
058300031117      *
058400031117      *---------------------------------------------------------------*
058500081209      *?sr_ORM_1   * Stampa singolo O.R.M.                           ?*
058600031117      *---------------------------------------------------------------*
058700031117     c     sr_ORM_1      BEGSR
058800031117      *
058900031117     c     k04orm01      chain     FNORM000
059000031117      *
059100031117if  1c                   IF        %found(FNORM01L)
059200081209      *
059300090423      * Gestisco allocazione del record con invio del msg all'utente
059400090423      *   che blocca il rcd
059500081209     c     ORMnrr        chain(e)  FNORM00
059600081209if  2c                   if        %error
059700081209     c                   clear                   TRUL82ds
059800081209     c                   eval      UL82§rrn = ORMnrr
059900081209     c                   eval      UL82§fil = 'FNORM00F'
060000081209     c                   eval      UL82§win = 'N'
060100081209     c                   eval      UL82§F7  = 'S'
060200081209     c                   eval      UL82§num = 5
060300081209     c                   eval      UL82§att = 60
060400081209     c                   eval      UL82§mss = msgjob
060500041122      * Effettuo la chiamata al *PGM d utilità
060600081209     c                   call(e)   'TRUL82R'
060700081209     c                   parm                    TRUL82ds
060800081209     c     ORMnrr        chain     FNORM00
060900041122e   2c                   endif
061000031117      *
061100031117     c                   eval      dORM01 = ORMflo
061200090423      * Reperimento tab. OSR per Controllo se serie ORM non stampabile
061300031117     c                   exsr      Tab_OSR
061400090423      * Reperimento tab. FAR per Controllo se fase ORM non stampabile
061500031117     c                   exsr      Tab_FAR
061600031117      *
061700031117if  2c                   IF            d§OSRstp <> 'S'
061800031117     c                             and d§FARstp =  'S'
061900031117      *
062000031117      * Controllo se p.o. ritiro Italia, Estero o DPD
062100031117      *   (no FedEx)
062200031117     c                   exsr      Ctrl_POR
062300031117      *
062400031117      * Pulizia dei PRTF
062500031117     c                   exsr      ClrPRTF
062600031117      *
062700031117      * Stampa
062800031117if  3c                   if        $Estero  = *off
062900031117     c                             and $DPD = *off
063000031117     c                   exsr      StampaORM
063100031117x   3c                   else
063200031117     c                   exsr      StampaORMe
063300031117e   3c                   endif
063400031117      *
063500080326      * Aggiorna dati se NON è RIstampa o sollecito o orm con allegati con distinta su PDA
063600081210if  3c                   if        R16ris  <> 'R' and R16ris <> 'X'
063700080326     c                             and R16ris <> 'A'
063800031117      * - O.R.M.
063900031117     c                   exsr      UpdatFNORM
064000031117      * - Storico Fasi
064100031117     c                   exsr      WriteFNORF
064200031117e   3c                   endif
064300031117      *
064400031117e   2c                   ENDIF
064500031117      *
064600031117e   1c                   ENDIF
064700031117      *
064800031117     c                   ENDSR
064900031117      *
065000031117      *---------------------------------------------------------------*
065100081209      *?sr_ORM_2   * Stampa di TUTTI gli O.R.M. della £6             ?*
065200031117      *---------------------------------------------------------------*
065300031117     c     sr_ORM_2      BEGSR
065400031117      *
065500031117do  1c                   DO        30            xx
065600031117      *
065700031117if  2c                   if        $L6(xx) <= 0
065800031117     c                   leave
065900031117e   2c                   endif
066000151126      /free
066100151126       //?Controllo se devo inviare i dati al partner a modo nuovo
066200151126         exsr NewTraduttore;
066300151126      /end-free
066400151126
066500130509       //?Cerco eventuale traduttore
066600151126     c                   IF        not NewTradPartner
066700130509     c                   exsr      CercaTrad
066800151126     c                   ENDIF
066900031117      *
067000031117      * Posizionamento per P.O. di ritiro
067100031117sel 2c                   select
067200031117w   2c                   when          R16ris =  'R'
067300031117     c                             and R16ndc =  *zeros
067400031117     c     $L6(xx)       setll     FNORM06
067500031117w   2c                   when          R16ris =  'R'
067600031117     c                             and R16ndc >  *zeros
067700031117     c     k02orm06      setll     FNORM06
067800081210w   2c                   when          R16ris <> 'R' and R16ris <> 'X'
067900031117     c                             and R16ndc =  *zeros
068000031117     c     $L6(xx)       setll     FNORM11
068100081210w   2c                   when          R16ris <> 'R' and R16ris <> 'X'
068200031117     c                             and R16ndc >  *zeros
068300031117     c     k02orm06      setll     FNORM11
068400031117e   2c                   endsl
068500031117      *
068600031117      * Ciclo di lettura/elaborazione per P.O. di ritiro
068700031117do  2c                   DO        *hival
068800031117      *
068900031117sel 3c                   select
069000031117w   3c                   when          R16ris =  'R'
069100031117     c                             and R16ndc =  *zeros
069200031117     c     $L6(xx)       reade     FNORM06
069300031117w   3c                   when          R16ris =  'R'
069400031117     c                             and R16ndc >  *zeros
069500031117     c     k02orm06      reade     FNORM06
069600081210w   3c                   when          R16ris <> 'R' and R16ris <> 'X'
069700031117     c                             and R16ndc =  *zeros
069800031117     c     $L6(xx)       reade     FNORM11
069900081210w   3c                   when          R16ris <> 'R' and R16ris <> 'X'
070000031117     c                             and R16ndc >  *zeros
070100031117     c     k02orm06      reade     FNORM11
070200031117e   3c                   endsl
070300031117      *
070400031117if  3c                   if        %eof
070500031117     c                   leave
070600031117e   3c                   endif
070700031117      *
070800031117      *
070900031117     c                   eval      dORM01 = ORMflo
071000031117      * - Controllo se serie ORM non stampabile
071100031117     c                   exsr      Tab_OSR
071200031117      * - Controllo se fase ORM non stampabile
071300031117     c                   exsr      Tab_FAR
071400031117      *
071500031117      * - Annullamento
071600031117if  3c                   if            ORMatb   <> *blanks
071700031117      *   Serie non stampabile
071800031117     c                             or  d§OSRstp =  'S'
071900031117      *   Fase non stampabile
072000031117     c                             or  d§FARstp <> 'S'
072100031117      *   Se tipo elaborazione RISTAMPA  deve essere già stato stampato
072200031117     c                             or (R16ris =  'R'
072300031117     c                             and ORMdst =  *zeros)
072400031117      *   Se tipo elaborazione RISTAMPA  deve essere già stato stampato
072500031117      *   nella data richiesta
072600031117     c                             or (R16ris =  'R'
072700031117     c                             and R16dst <> *zeros
072800031117     c                             and ORMdst <> R16dst)
072900031117      *   Data distinta
073000031117     c                             or (R16ddc <> *zeros
073100031117     c                             and ORMddc <> R16ddc)
073200031117     c                   iter
073300031117e   3c                   endif
073400031117      *
073500031117      * - Data ritiro
073600081210if  3c                   IF            R16ris <> 'R' and R16ris <> 'X'
073700031117     c                             and R16dar <> *zeros
073800031117      *   dal 19/09/2001 devo stampare anche gli ORM con data ritiro
073900031117      *   inferiore alla data richiesta, se non sono mai stati stampati
074000031117      *   da considerare solo in STAMPA
074100031117if  4c                   if            ORMdar   >  R16dar
074200031117      * - - Solo Già Assegnati
074300031117     c                             or (R16ass =  'S'
074400031117     c                             and ORMfao <> 400)
074500031117     c                   iter
074600031117e   4c                   endif
074700031117e   3c                   ENDIF
074800031117      *
074900031117      * > Controllo se p.o. ritiro Italia, Estero o DPD
075000031117      *   (no FedEx)
075100031117     c                   exsr      Ctrl_POR
075200031117      *
075300031117      * > Pulizia del PRTF
075400031117     c                   exsr      ClrPRTF
075500151126
075600151126      /free
075700151126       //?Se ORM Estero e Nuovo traduttore attivo devo preparare il file
075800151126       //?richiamando il nuovo programma con la funzione EXEC
075900151126       //?solo se siamo in STAMPA
076000151126         IF  $Estero and NewTradPartner and R16ris = 'S';
076100151126           pin_Opz = 'WRITE';
076200151126           pin_KSU = skKSU(zz);
076300151126           clear pOut_Esito;
076400151126           tis7tbor (pin_Opz:pin_KSU:FNORMDS:pOut_Esito);
076500151126         //?Per errore leggo altro ORM
076600151126           IF  pOut_Esito <> '1';
076700151126             iter;
076800151126           ENDIF;
076900151126         ENDIF;
077000151126      /end-free
077100130509
077200130513       //?Se ORM Estero (no DPD) e traduttore attivo devo preparare il file
077300130509       //?richiamando il traduttore con la funzione EXEC
077400130509       //?solo se siamo in STAMPA
077500151126       //?e non ho il nuovo traduttore attivo
077600130509     c                   IF        $Estero = *on and wOKtraduttore and
077700130509     c                             R16ris = 'S'
077800151126     c                             and not NewTradPartner
077900130520     c                   eval      ORMfunzion = 'EXEC'
078000130520     c                   eval      ORMormpoe = ORMpoe
078100130520     c                   eval      ORMormnsr = ORMnsr
078200130520     c                   eval      ORMormnor = ORMnor
078300130520     c                   eval      ORMormnrv = ORMnrv
078400130520     c                   eval      kpjbu = TRTCTORMds
078500130509     c                   call      §prtradex
078600130509     c                   parm                    KPJBA
078700130509       //?se torna esito 1 qualcosa è andato storto nel traduttore
078800130509       //?leggo altro ORM
078900130520     c                   IF        ORMesito = '1'
079000130510     c                   eval      R16ass = 'E'
079100130509     c                   iter
079200130509     c                   ENDIF
079300130509     c                   ENDIF
079400031117      *
079500031117      * > Stampa
079600031117     c                   eval      R16poe = ORMpoe
079700031117     c                   eval      R16nsr = ORMnsr
079800031117     c                   eval      R16nor = ORMnor
079900031117     c                   eval      R16nrv = ORMnrv
080000031117if  3c                   if            $Estero = *off
080100031117     c                             and $DPD    = *off
080200031117     c                   exsr      StampaORM
080300040218x   3c                   else
080400031117     c                   exsr      StampaORMe
080500031117e   3c                   endif
080600031117      *
080700031117      * > Aggiornamento dati se NON è ristampa
080800081210if  3c                   IF        R16ris <> 'R' and R16ris <> 'X'
080900031117      *
081000031117     c     k04orm01      chain     FNORM000
081100031117if  4c                   if            %found(FNORM01L)
081200031117     c                             and ORMdst = *zeros
081300081209      *
081400041122      * Gestisco allocazione del record con invio del msg all'utente che blocca il rcd
081500081209     c     ormnrr        chain(e)  FNORM00
081600081209if  5c                   if        %Error
081700081209     c                   clear                   TRUL82ds
081800081209     c                   eval      UL82§rrn = ORMnrr
081900081209     c                   eval      UL82§FIL = 'FNORM00F'
082000081209     c                   eval      UL82§WIN = 'N'
082100081209     c                   eval      UL82§F7  = 'S'
082200081209     c                   eval      UL82§num = 5
082300081209     c                   eval      UL82§att = 60
082400081209     c                   eval      UL82§mss = MsgJob
082500041122      * Effettuo la chiamata al *PGM d utilità
082600081209     c                   call(e)   'TRUL82R'
082700081209     c                   parm                    TRUL82ds
082800081209     c     ORMnrr        chain     FNORM00
082900041122e   5c                   endif
083000031117      * - O.R.M.
083100031117     c                   exsr      UpdatFNORM
083200031117      * - Storico Fasi
083300031117     c                   exsr      WriteFNORF
083400031117e   4c                   endif
083500031117      *
083600031117e   3c                   ENDIF
083700031117      *
083800031117e   2c                   ENDDO
083900151126      /free
084000151126       //?Per Nuovo Traduttore devo richiamare il programma con CLOSE
084100151126         IF  NewTradPartner;
084200151126           pin_Opz = 'CLOSE';
084300151126           reset pin_KSU;
084400151126           clear pOut_Esito;
084500151126           tis7tbor (pin_Opz:pin_KSU:FNORMDS:pOut_Esito);
084600151126         ENDIF;
084700151126      /end-free
084800151126
084900130509       //?Richiamo il traduttore con la funzione CLOSE
085000130509     c                   IF        wOKtraduttore
085100130520     c                   eval      ORMfunzion = 'CLOSE'
085200130520     c                   clear                   ORMormpoe
085300130520     c                   clear                   ORMormnsr
085400130520     c                   clear                   ORMormnor
085500130520     c                   clear                   ORMormnrv
085600130520     c                   eval      kpjbu = TRTCTORMds
085700130509     c                   call      §prtradex
085800130509     c                   parm                    KPJBA
085900130509     c                   ENDIF
086000031117      *
086100031117e   1c                   ENDDO
086200031117      *
086300031117     c                   ENDSR
086400130509
086500130509      /free
086600151126       //-----------------------------------------------------------------
086700151126       //?Verifico se la filiale ritiro è un Partner con nuovo traduttore.
086800151126       //------------------------------------------------------------------
086900151126       BEGSR NewTraduttore;
087000151126
087100151126         NewTradPartner = *off;
087200151201         wSIelenco = *off;
087300151126         clear zz;
087400151126         clear yy;
087500151201
087600151201       //?Se filiale ritiro è nella tabella partner
087700151201         IF  %lookup($L6(xx):$lnp) = 0;
087800151201           leavesr;
087900151201         ENDIF;
088000151126
088100151126       //?Se la filiale ritiro è presente in tabella ALP
088200151126         zz =  %lookup($L6(xx):skPOR);
088300151126         IF  zz = 0;
088400151126           leavesr;
088500151126         ENDIF;
088600151126         NewTradPartner = *on;
088700151201
088800151201       //?imposto se devo mandare lo stesso MAIL/FAX oltre al FILE
088900151201         wSIelenco = (skFIE(zz) = 'S');
089000151126
089100151126       //?Se nuovo invio richiamo il pgm con la funzione OPEN
089200151126         pin_Opz = 'OPEN';
089300151126         reset pin_KSU;
089400151126         clear FNORMDS;
089500151126         clear pOut_Esito;
089600151126         tis7tbor (pin_Opz:pin_KSU:FNORMDS:pOut_Esito);
089700151126         IF  pOut_Esito <> '1';
089800151126           NewTradPartner = *off;
089900151126           wSIelenco = *off;
090000151126         ENDIF;
090100151126
090200151126       ENDSR;
090300130509       //--------------------------------------------------------------
090400130509       //?Cerco eventuale traduttore previsto per la filiale ritiro
090500130509       //--------------------------------------------------------------
090600130509       BEGSR CercaTrad;
090700130509
090800130509         wOKtraduttore = *off;
090900130522         wSIelenco     = *off;
091000130509
091100130509       //?Se filiale ritiro è partner
091200130509         clear yy;
091300130509         yy = %lookup($L6(xx):$lnp);
091400130509         IF  yy > 0 and skPGMTR(yy) <> *blanks;
091500130522       //?imposto se devo mandare lo stesso MAIL/FAX oltre al FILE
091600130522           wSIelenco = (skSIelenco(yy) = 'S');
091700130509       //?aggangio la tabella PR in caso di invio automatico
091800130509           clear EDIdsPR;
091900130509           TABcod = 'PR';
092000130509           TABkey = skPGMTR(yy);
092100130509           chain (TABcod:TABkey) EDTAB01L;
092200130509           IF  %found(EDTAB01L);
092300130509             EDIdsPR = TABuni;
092400130509           ENDIF;
092500130509           IF  §PRtradex <> *blanks;
092600130509             wOKtraduttore = *on;
092700130509         //?devo richiamare il pgm con la funzione OPEN
092800130520             clear TRTCTORMds;
092900130520             ORMcliente = $ksc(yy);
093000130520             ORMlinea   = $lnp(yy);
093100130520             ORMfunzion = 'OPEN';
093200130520             kpjbu = TRTCTORMds;
093300130509      /end-free
093400130509     c                   call      §prtradex
093500130509     c                   parm                    KPJBA
093600130509      /free
093700130509           ENDIF;
093800130509         ENDIF;
093900130509
094000130509       ENDSR;
094100130509      /end-free
094200031117      *
094300031117      *---------------------------------------------------------------*
094400081209      *?Tab_OSR    * Reperimento dati tab. OSR                       ?*
094500031117      *---------------------------------------------------------------*
094600031117     c     Tab_OSR       BEGSR
094700031117      *
094800031117      * Caricamento impedimenti relativi alla serie ORM:
094900031117     c                   reset                   dOSR
095000031117      *
095100031117      *  ORM senza serie: risulta sempre stampabile
095200130118     c                   IF        ORMnsr = *zeros
095300130118     c                   leavesr
095400130118     c                   ENDIF
095500130118
095600031117      *  ORM  con  serie: controllo l'impedimento da tab.OSR
095700110427      * - Se flag R16ris = 'C' => forzo che ORM stampabile
095800110427if  2c                   If        R16ris = 'C'
095900100927     c                   clear                   d§OSRstp
096000130118     c                   leavesr
096100130118     c                   ENDIF
096200130118
096300130118      /free
096400130118       //?Controllo se il cliente è in tabella OSR
096500130118         wORMcor = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
096600130118       //?leggo tutta la tabella OSR
096700130118         setll c_tabosr tntbe01l;
096800130118         reade c_tabosr tntbe01l;
096900130118
097000130118         DOW not %eof(tntbe01l);
097100130122           IF  TBEatb = *blanks;
097200130118           dosr = tbeuni;
097300130118           IF d§osrcli  = wORMcor or d§OSRcl2  = wORMcor or
097400130118              d§osrcl3  = wORMcor or d§OSRcl4  = wORMcor or
097500130118              d§osrcl5  = wORMcor or d§OSRcl6  = wORMcor or
097600130118              d§osrcl7  = wORMcor or d§OSRcl8  = wORMcor or
097700130118              d§osrcl9  = wORMcor or d§OSRcl10 = wORMcor or
097800130118              d§osrcl11 = wORMcor or d§OSRcl12 = wORMcor or
097900130118              d§osrcl13 = wORMcor or d§OSRcl14 = wORMcor or
098000130118              d§osrcl15 = wORMcor or d§OSRcl16 = wORMcor;
098100130118             leavesr;
098200130118           ENDIF;
098300130122           ENDIF;
098400130121           reade c_tabosr tntbe01l;
098500130118         ENDDO;
098600130118
098700130118       //?Se arrivo qua vuol dire che ho la serie e che non ho trovato il cliente
098800130118       //?in tabella quindi imposto il flag a NON stampabile
098900130118         d§OSRstp = 'S';
099000130118      /end-free
099100031117      *
099200031117     c                   ENDSR
099300031117      *
099400031117      *---------------------------------------------------------------*
099500081209      *?Tab_FAR    * Reperimento dati tab. FAR                       ?*
099600031117      *---------------------------------------------------------------*
099700031117     c     Tab_FAR       BEGSR
099800031117      *
099900031117      * Caricamento impedimenti relativi alla fase ORM:
100000031117     c                   reset                   dFAR
100100031117      *
100200031117     c                   clear                   TIBS02DS
100300031117     c                   movel     'C'           T02mod
100400031117     c                   movel     KNSIF         T02sif
100500031117     c                   movel     'FAR'         T02cod
100600031117     c                   movel(p)  ORMfao        T02ke1
100700031117     c                   call      'TIBS02R'
100800031117     c                   parm                    KPJBA
100900031117     c                   parm                    TIBS02DS
101000031117if  1c                   If        T02err   = *blanks
101100031117     c                   movel     T02uni        dFAR
101200031117e   1c                   EndIf
101300081209      *
101400031117     c                   ENDSR
101500031117      *
101600031117      *---------------------------------------------------------------*
101700081209      *?Ctrl_POR   * Verifica se P.O. ritiro Italia, Estero o DPD    ?*
101800081209      *?             ed esecuzione delle opporttune override         ?*
101900031117      *---------------------------------------------------------------*
102000031117     c     Ctrl_POR      BEGSR
102100031117      *
102200031117     c                   reset                   $Estero
102300031117     c                   reset                   $DPD
102400031117      *
102500031117if  1c                   IF        ORMpor <> ORGfil
102600071106     c                   clear                   Og143
102700071106     c                   clear                   Og147
102800031117     c                   clear                   ORGfl1
102900031117     c     ORMpor        chain     AZORG
103000031117if  2c                   if        %found(AZORG01L)
103100071106     c                   movel     ORGde3        Og143
103200071106     c                   movel     ORGde7        Og147
103300031117e   2c                   endif
103400031117e   1c                   ENDIF
103500070926      * data partenza procedura in organigramma
103600071106     c                   clear                   Data_Org
103700071106     c                   if        §OGddao > *zeros
103800071106     c                   move      §OGddao       Data_Org
103900070926     c                   endif
104000031117      *
104100031117      * - $Estero = flag O.R.M. Estero ma NON per FedEx
104200031117if  1c                   if        §OGntw  <> 'FED' and ORGfl1 = 'E'
104300031117     c                   eval      $Estero =  *on
104400031117e   1c                   endif
104500031117      * - $DPD    = flag O.R.M. DPD
104600031117if  1c                   if        §OGntw  =  'DPD'
104700031117     c                   eval      $DPD    =  *on
104800031117e   1c                   endif
104900031117      *
105000031117      *
105100031117if  1c                   IF            $Estero = *on
105200031117     c                             or  $DPD    = *on
105300031117      *
105400031204if  2c                   IF        NOT %open(FIOR16P2)
105500031117      * 1ª override
105600090129      /free
105700090129         clear  TrulOvrPds;
105800090930         uopToDo   = 'A';
105900090129         uopPrtF   = 'FIOR16P2  ';
106000090129         uopOutQ   = R16oqo;
106100090129         uopSave   = '*NO ';
106200090129         trulOvrPr ( trulOvrPds );
106300090129         Qcmd  = uopCmd;
106400090129      /end-free
106500031117     c                   call      'QCMDEXC'                            90
106600031117     c                   parm                    Qcmd
106700031117     c                   parm                    Qlen
106800031117      *
106900031204     c                   open      FIOR16P2
107000031117e   2c                   ENDIF
107100031117      *
107200031117if  2c                   IF        NOT %open(ORMest)
107300031117      * 2ª override
107400090129      /free
107500090129         clear  TrulOvrPds;
107600090930         uopToDo   = 'A';
107700090129         uopPrtF   = 'ORMEST    ';
107800090129         uopOutQ   = PRMoutq;
107900090129         uopSave   = '*YES';
108000090129         trulOvrPr ( trulOvrPds );
108100090129         Qcmd  = uopCmd;
108200090129      /end-free
108300031117     c                   call      'QCMDEXC'                            90
108400031117     c                   parm                    Qcmd
108500031117     c                   parm                    Qlen
108600031117      *
108700031117     c                   open      ORMest
108800031117e   2c                   ENDIF
108900031117      *
109000031117e   1c                   ENDIF
109100031117      *
109200031117     c                   ENDSR
109300031117      *
109400031117      *---------------------------------------------------------------*
109500081209      *?ClrPRTF    * Pulizia dei campi di stampa                     ?*
109600031117      *---------------------------------------------------------------*
109700031117     c     ClrPRTF       BEGSR
109800031117      *
109900120330     c                   clear                   OR18logBRT
110000090423     c                   clear                   OR18box
110100090423     c                   clear                   OR18pos
110200090423     c                   clear                   OR18orm
110300090423     c                   clear                   OR18not
110400090423     c*** non ha campi   clear                   OR18nul
110500031117      *
110600031117     c                   clear                   OR16est
110700031117      *
110800031117     c*** non qui:       reset                   $Estero
110900031117     c*** non qui:       reset                   $DPD
111000031117     c                   reset                   $PrtNote
111100031117      *
111200031117     c                   clear                   $Note
111300031117     c                   clear                   $QUM
111400031117      *
111500031117     c                   movea     *zeros        *in(01)
111600031117      *
111700031117     c                   ENDSR
111800031117      *
111900031117      *---------------------------------------------------------------*
112000081209      *?StampaORM  * Stampa singolo O.R.M.                           ?*
112100031117      *---------------------------------------------------------------*
112200031117     c     StampaORM     BEGSR
112300071106      *
112400071106      * *in60 = Stampa Distinte ORM automatiche "attiva"
112500071106     c                   eval      *in60  =  (DateU >= Data_Org  and
112600071106     c                                        Data_Org > *zeros)
112700050126      *
112800050126      * *in70 = O.R.M. con Rimesso Mittente
112900050126if  1c                   eval      *in70  =  (§ORsrm = 'S')
113000050126      * *in71 = Stampa Ricevuta di ritiro (non adesso: DOPO!)
113100050126     c*** (già così)     eval      *in71  =  *off
113200031117      *
113300031117      * Imposto i campi in stampa:
113400031117      *
113500050429      * - Numero ORM
113600031117     c                   eval      Ppoe   =  ORMpoe
113700031117     c                   eval      Pnsr   =  ORMnsr
113800031117     c                   eval      Pnor   =  ORMnor
113900031117     c                   eval      Pnrv   =  ORMnrv
114000050429if  1c                   if        *in70
114100050429     c                   eval      Ppoe2  =  ORMpoe
114200050429     c                   eval      Pnsr2  =  ORMnsr
114300050429     c                   eval      Pnor2  =  ORMnor
114400050429     c                   eval      Pnrv2  =  ORMnrv
114500050429e   1c                   endif
114600031117      *
114700031117      * - Data emissione ORM
114800031117     c                   clear                   WLBdat
114900031117     c                   move      ORMdao        G02inv
115000031117     c                   movel     '3'           G02err
115100031117     c                   call      'XSRDA8'
115200031117     c                   parm                    WLBdat
115300031117     c                   z-add     G02dat        Pdao
115400031117      *
115500031117      * - Ora emissione ORM ("hh:mm", senza ":ss")
115600031117     c                   movel     ORMoao        Poao
115700071107      *
115800071107      * - Numero distinta consegna
115900071107     c                   eval      Pndc   =  ORMndc
116000031117      *
116100071107      * - Padroncino: codice e nominativo
116200120612     c                   z-add     ORMnpg        DSTnpg
116300031117     c                   z-add     ORMndc        DSTnfv
116400031117     c                   z-add     ORMpor        DSTfgs
116500120612     c     kfidst        chain     fidst000
116600120612if  1c                   if        %found(Fidst01l)
116700031117     c                             and    DSTatb = *blanks
116800071107if  2c                   if        *in60
116900071107     c                   movel     DSTpdr        Ppdr
117000071107e   2c                   endif
117100031117     c                   movel     'A'           APDtip
117200031117     c                   z-add     DSTpdr        APDpdr
117300031117     c     kFIAPD        chain     FIAPD000
117400071107if  2c                   if        %found(FIAPD01L)
117500071107if  3c                   if        *in60
117600071107     c                   eval      Ppdr   =  %editc(DSTpdr : 'Z')
117700071107     c                                    +  ' '
117800071107     c                                    +  %subst(APDrsc : 1 :
117900071107     c                                       %len(Ppdr) - 1 -
118000071107     c                                         %len( %trim(
118100071107     c                                         %editw( DSTpdr :
118200071107     c                                                '0       ' ))))
118300071107x   3c                   else
118400071107     c                   movel     APDrsc        Ppdr
118500071107     c                   eval      Ppdr   =  %subst( APDrsc : 1 :
118600071107     c                                               %len( Ppdr ) - 5 )
118700071107e   3c                   endif
118800071107e   2c                   endif
118900031117e   1c                   endif
119000071107      *
119100071107      * - Zona ritiro
119200071107     c                   if        NOT  *in60
119300071107     c                   eval      Pzor   =  ORMzor
119400071107     c                   endif
119500031117      *
119600031117      * - P.O. ritiro
119700031117     c                   eval      Ppor   =  ORMpor
119800031117     c                   eval      Ppod   =  ORGdes
119900031117      *
120000031117      * - Data ritiro
120100031117     c                   clear                   WLBdat
120200031117     c                   z-add     ORMdar        G02inv
120300031117     c                   movel     '3'           G02err
120400031117     c                   call      'XSRDA8'
120500031117     c                   parm                    WLBdat
120600031117     c                   z-add     G02dat        Pdar
120700031117      *
120800031117      * - Ora ritiro
120900031117     c                   eval      Porr   =  ORMorr
121000071106      * - - Ora ritiro <= 12:30 (stampata in "grassetto")
121100071106     c                   eval      *in61  =  (ORMorr <= 1230)
121200161116      /free
121300161116       //?Cerco la data pronta merce
121400161116       //?se data ritiro > data pronta merce non stampo l'ora pronta merce
121500161116        clear dOREdt;
121600161116        OREtrc = 'DT';
121700161116        chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
121800161116        IF  %found(FNORE01L) and %subst(OREdati:1:8) > *zeros;
121900161116          dOREdt = OREdati;
122000161116        ENDIF;
122100161116        IF  ORMdar > §OREdpm and §OREdpm > *zeros;
122200161116          clear Porr;
122300161116          *in61 = *off;
122400161116        ENDIF;
122500161116      /end-free
122600031117      *
122700031117      * - Priorità (lungo 1/0)
122800031117     c                   z-add     ORMsto        Psto
122900050307      *
123000050307      * - O.R.M. commissionato
123100050307if  1c                   if        §ORcom =  'S'
123200050307     c                   eval      Pcom   =  'C'
123300050307e   1c                   endif
123400031117      *
123500031117      * - Mittente
123600031117     c                   movel     ORMrsr        Prsr
123700031117     c                   movel     ORMinr        Pinr
123800031117     c                   movel     ORMcar        Pcar
123900031117     c                   movel     ORMlor        Plor
124000031117     c                   movel     ORMprr        Pprr
124100031117     c                   movel     ORMnar        Pnar
124200031117      *
124300031117      * - Codice ritiro
124400031117     c                   eval      Pcra   =  ORMcra
124500031117      *
124600031117      * - Referente
124700081215if  1c                   if        ORMnsr =  *zeros
124800031117     c                   eval      Pter   =  ORMter
124900081215e   1c                   endif
125000031117     c                   eval      Prer   =  ORMrer
125100081210     c                   eval      *in62  =  (ORMnsr <> *zeros)
125200031117      *
125300031117      * - Riferimento (NO se call center)
125400031117     c                   if        ORMtco <> 'C'
125500031117     c                   movel     ORMrfa        Prfa
125600031117     c                   endif
125700031117      *
125800031117      * - Indicazione "Sponda Idraulica"
125900031117if  1c                   if        ORMspi <> *blanks
126000031117     c                   eval      Pspi   =  'Sponda Idraulica'
126100031117e   1c                   endif
126200031117      *
126300031117      * - Quantità e Unità di Misura
126400031117     c                   exsr      QUM_FnOrm
126500031117     c                   eval      Pqum   =  %trim($Qum(1)) + '   '
126600031117     c                                    +  %trim($Qum(2)) + '   '
126700031128     c                                    +  %trim($Qum(3))
126800031117      *
126900031117      * - Natura della merce
127000031117     c                   movel     ORMnam        Pnam
127100031117      *
127200031117      * - "Porto assegnato" o "Prepagato"
127300031117sel 1c                   select
127400031117w   1c                   when      ORMtor =  'P'
127500031117     c                   eval      Ptor   =  'Prepagato '
127600031117w   1c                   when      §ORpfb =  'A'
127700031117     c                   eval      Ptor   =  'Assegnato '
127800050125e   1c                   endsl
127900031117      *
128000031117      * - Assegnazione al padroncino
128100031117     c                   movel     ORMtap        Ptap
128200031117      *
128300031117      * - Tipo comunicazione ORM
128400031117     c                   movel     ORMtco        Ptco
128500031117      *
128600081210      * - Riferimento spedizione (se ORM prepagato)
128700031117if  1c                   if        ORMtor =  'P'
128800151126     c                   eval      wORMpoe = ORMpoe
128900151126     c                   eval      wORMnsr = ORMnsr
129000151126     c                   eval      wORMnor = ORMnor
129100151126     c                   eval      wORMnrv = ORMnrv
129200060214     c                   movel     BARcodeDS     kAR4n14
129300060214     c     kFiar4        chain     FIAR4000
129400060214if  2c                   if        NOT %found(FIAR404L)
129500060214     c                   clear                   AR4aas
129600060214     c                   clear                   AR4lnp
129700060214     c                   clear                   AR4nrs
129800060214     c                   clear                   AR4nsp
129900031117x   2c                   else
130000081209     c                   eval      Par4   =  'Spedizione: '
130100060214     c                                    +  %editc(AR4aas : 'Z')
130200081209     c                                    +  ' '
130300081209     c                                    +  %triml(%editw(AR4lnp : '0   '))
130400031117     c                                    +  ' '
130500060214     c                                    +  %editc(AR4nrs : 'Z')
130600031117     c                                    +  ' '
130700060214     c                                    +  %editc(AR4nsp : 'Z')
130800081210     c                   exsr      sr_ImpFatt
130900031117e   2c                   endif
131000031117e   1c                   endif
131100031117      *
131200031117      * - Ordinante
131300031117if  1c                   if        ORMrso <> *blanks
131400031117     c                   eval      Pkor   =  'ORDINANTE'
131500031117     c                   movel     ORMrso        Prso
131600031117if  2c                   if        ORMnao =  *blanks
131700031117     c                   eval      Pclo   =  %trim(ORMcao) + ' '
131800031117     c                                    +  %trim(ORMloo) + ' '
131900031117     c***                                 +  %trim(ORMpro)
132000031117     c                   move      '   '         Pclo
132100031117     c                   move      ORMpro        Pclo
132200031117x   2c                   else
132300031117     c                   eval      Pclo   =  %trim(ORMcao) + ' '
132400031117     c                                    +  %trim(%subst(ORMloo:1:32))
132500031117     c                                    +  ' ' + %trim(ORMpro)
132600031117     c                                    +  ' ' + %trim(ORMnao)
132700031117e   2c                   endif
132800031117e   1c                   endif
132900031117      *
133000031117      * - Destinatario
133100031117if  1c                   if        ORMrsc <> *blanks
133200031117     c                   eval      Pkde   =  'DESTINATARIO'
133300031117     c                   movel     ORMrsc        Prsc
133400031117if  2c                   if        ORMnac =  *blanks
133500031117     c                   eval      Pclc   =  %trim(ORMcac) + ' '
133600031117     c                                    +  %trim(ORMloc) + ' '
133700031117     c***                                 +  %trim(ORMprc)
133800031117     c                   move      '   '         Pclc
133900031117     c                   move      ORMprc        Pclc
134000031117x   2c                   else
134100031117     c                   eval      Pclc   =  %trim(ORMcac) + ' '
134200031117     c                                    +  %trim(%subst(ORMloc:1:32))
134300031117     c                                    +  ' ' + %trim(ORMprc)
134400031117     c                                    +  ' ' + %trim(ORMnac)
134500031117e   2c                   endif
134600031117e   1c                   endif
134700031117      *
134800031117      * - Note
134900140127if  1c                   if           ORMno1 <> *blanks
135000031117     c                             or ORMno2 <> *blanks
135100031127     c                   movel     ORMno1        Pno1
135200031127     c                   movel     ORMno2        Pno2
135300140127e   1c                   endif
135400031117      * - Note aggiuntive
135500140127     c                   exsr      Sch_NoteAUT
135600031117if  1c                   IF        YY > *zeros
135700031117sel 2c                   SELECT
135800031117      * - - disposizione tra ordinante (Prso) e destinatario (Prsc)
135900031117      *     e note (Pno1/Pno2)
136000031117w   2c                   WHEN          YY    <= 2
136100031117     c                             and Pno1  =  *blanks
136200031117     c                             and Pno2  =  *blanks
136300031117     c                   eval      Pno1   =  $Note(1)
136400031117     c                   eval      Pno2   =  $Note(2)
136500031117     c                   clear                   YY
136600031117w   2c                   WHEN          YY    <= 2
136700031117     c                             and Prso  =  *blanks
136800031117     c                             and Prsc  <> *blanks
136900031117     c                   eval      Pkor   =  'NOTE'
137000031117     c                   eval      Prso   =  $Note(1)
137100031117     c                   eval      Pclo   =  $Note(2)
137200031117     c                   clear                   YY
137300031117w   2c                   WHEN          YY    <= 2
137400031117     c                             and Prso  <> *blanks
137500031117     c                             and Prsc  =  *blanks
137600031117     c                   eval      Pkde   =  'NOTE'
137700031117     c                   eval      Prsc   =  $Note(1)
137800031117     c                   eval      Pclc   =  $Note(2)
137900031117     c                   clear                   YY
138000031117w   2c                   WHEN          YY    <= 4
138100031117     c                             and Prso  =  *blanks
138200031117     c                             and Prsc  =  *blanks
138300031117     c                   eval      Pkor   =  'NOTE'
138400031117     c                   eval      Pkde   =  'NOTE'
138500031117     c                   eval      Prso   =  $Note(1)
138600031117     c                   eval      Pclo   =  $Note(2)
138700031117     c                   eval      Prsc   =  $Note(3)
138800031117     c                   eval      Pclc   =  $Note(4)
138900031117     c                   clear                   YY
139000031117w   2c                   WHEN          YY    <= 4
139100031117     c                             and Prso  =  *blanks
139200031117     c                             and Prsc  <> *blanks
139300031117     c                             and Pno1  =  *blanks
139400031117     c                             and Pno2  =  *blanks
139500031117     c                   eval      Pkor   =  'NOTE'
139600031117     c                   eval      Prso   =  $Note(1)
139700031117     c                   eval      Pclo   =  $Note(2)
139800031117     c                   eval      Pno1   =  $Note(3)
139900031117     c                   eval      Pno2   =  $Note(4)
140000031117     c                   clear                   YY
140100031117w   2c                   WHEN          YY    <= 4
140200031117     c                             and Prso  <> *blanks
140300031117     c                             and Prsc  =  *blanks
140400031117     c                             and Pno1  =  *blanks
140500031117     c                             and Pno2  =  *blanks
140600031117     c                   eval      Pkde   =  'NOTE'
140700031117     c                   eval      Prsc   =  $Note(1)
140800031117     c                   eval      Pclc   =  $Note(2)
140900031117     c                   eval      Pno1   =  $Note(3)
141000031117     c                   eval      Pno2   =  $Note(4)
141100031117     c                   clear                   YY
141200140127w   2c*//                WHEN          YY    >  *zeros
141300140127w   2c*//                          and Pno1  =  *blanks
141400140127     c*//                          and Pno2  =  *blanks
141500140127     c*//                eval      Pno1   =  $Note(1)
141600140127     c*//                eval      Pno2   =  $Note(2)
141700140127     c*//                clear                   YY
141800031117      * - - stampa in nuova pagina (dopo la stampa dell'O.R.M.)
141900031117     c***                OTHER
142000050125     c***                exsr      Prt_Note
142100031117e   2c                   ENDSL
142200031117e   1c                   ENDIF
142300131129      *
142400131129      * -?Orari Apertura?
142500131129     c                   exsr      sr_OrariApert
142600031117      *
142700031117      * - Codice a barre: POE+NSR+NOR+NRV
142800031215     *** *   (chiamo il pgm TRUL28R per ricavarne il check digit)
142900031215     ***c                   reset                   TRUL28ds
143000031215     ***c                   movel     BarCodeDS     I28cod
143100031215     ***c                   call      'TRUL28R1'
143200031215     ***c                   parm                    TRUL28DS
143300031215if  1***c                   if        O28err =  *blanks
143400031215     ***c                   movel     O28cod        Tbcd
143500031215e   1***c                   endif
143600031215     c                   movel     BarCodeDS     Tbcd
143700031117      *
143800031117      * - - Verifico se codice a barre da stampare
143900031117      *     (O28err = *blanks)
144000031117     c                   eval      *in50  =  (Tbcd <> *zeros)
144100031117      *
144200031117      *
144300031117      * Stampo O.R.M.
144400031117      *
144500081211      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
144600031117     c                   exsr      Posiz_Box
144700031117      * - O.R.M.
144800090423     c                   WRITE     OR18orm
144900031117      *
145000031117      *
145100050125      * -Se?O.R.M.?senza?Rimesso Mittente:
145200050125      *   Stampo eventuali Note Aggiuntive in pagina successiva
145300050125if  1c                   if        §ORsrm <> 'S'
145400081211     c                   exsr      Prt_Note
145500050125      * -Altrimenti?:
145600050125      *   Stampo Rimesso Mittente in pagina successiva
145700050125x   1c                   else
145800050125     c                   exsr      Prt_RimMitt
145900050125e   1c                   endif
146000050125      *
146100050125     c                   ENDSR
146200081210      /free
146300081210
146400081210       //--------------------------------------------------------------
146500081210       //?Reperimento Importo Totale Fattura (se O.R.M. PrePagato)     ?
146600081210       //--------------------------------------------------------------
146700081210       BEGSR  sr_ImpFatt;
146800081210
146900081210         // Reperimento importo prepagato
147000081210         chain (AR4aas : AR4lnp : AR4nrs : AR4nsp)  FIAR6000;
147100081210
147200081210         if  %found(FIAR601L);
147300081210           w0112 = AR6ift;
147400131129           Pimp  = 'Tot. Fatt.: '
147500131129                 + %triml( %editw( w0112 : '       . 0 ,  -') )
147600081210                 + AR6div;
147700081210         endif;
147800081210
147900081210       ENDSR;
148000131129
148100131129       //--------------------------------------------------------------
148200131129       //?Reperimento ed impostazione degli Orari di Apertura          ?
148300131129       //--------------------------------------------------------------
148400131129       BEGSR  sr_OrariApert;
148500131129
148600131129         clear  dOREorari;
148700131129         *in61 = *off;
148800131129
148900131129         // -?Recupero dati da estensione FNORE (trc "O ")?
149000131129         k_OREpoe = ORMpoe;
149100131129         k_OREnsr = ORMnsr;
149200131129         k_OREnor = ORMnor;
149300131129         k_OREnrv = ORMnrv;
149400131129         k_OREtrc = 'O ';
149500131129         chain  %kds(keyFNORE01)  FNORE000;
149600131129
149700131129         // -?...NON trovati => uscita dalla subroutine?
149800131129         if  Not  %found(FNORE01L);
149900131129           leavesr;
150000131129         endif;
150100131129
150200131129         // -?...Trovati => verifica degli orari?
150300131129         dOREorari = OREdati;
150400131129
150500131129         Select;
150600131129
150700131129           // -?C'è un solo orario "ALLE"?
150800131129           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
150900131129                 §OREoraPda = *zero  and  §OREoraPa = *zero;
151000131129             POA1 = 'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
151100131129           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
151200131129                 §OREoraPda = *zero  and  §OREoraPa > *zero;
151300131129             POA1 = 'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
151400131129
151500131129           // -?C'è un solo orario completo (per 1°)?
151600131129           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
151700131129                 §OREoraPda = *zero  and  §OREoraPa = *zero;
151800131129             POA1 = %editw( §OREoraMda : ' 0:  ' ) + '  ' +
151900131129                    %editw( §OREoraMa  : ' 0:  ' );
152000131129
152100131129           // -?Ci sono due orari completi?
152200131129           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
152300131129                 §OREoraPda > *zero  and  §OREoraPa > *zero;
152400131129             POA1 = %editw( §OREoraMda : ' 0:  ' ) + '  ' +
152500131129                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
152600131129                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
152700131129                    %editw( §OREoraPa  : ' 0:  ' );
152800131129
152900131129         EndSl;
153000131129
153100131129         *in61 = (POA1 <> *blanks);
153200131129
153300131129       ENDSR;
153400131129
153500081210      /end-free
153600050125      *
153700050125      *---------------------------------------------------------------*
153800081209      *?Stampa delle note aggiuntive in pagina successiva            ?*
153900050125      *---------------------------------------------------------------*
154000050125     c     Prt_Note      BEGSR
154100050125      *
154200050125if  1c                   if        YY     >  *zeros
154300031117     c                   eval      $PrtNote  = *on
154400081211      * - - Posizionamento sul foglio A4 in base al n° pagina in stampa
154500031117     c                   exsr      Posiz_Box
154600031117      * - - intestazione
154700090423     c                   write     OR18nul
154800031117     c                   eval      Pnot   =  '            '
154900031117     c                                    +  '(Seguito)     '
155000031117     c                                    +  'O.R.M. numero '
155100031117     c                                    +   %editw(ORMpoe : '0   ')
155200031117     c                                    +   ' '
155300031117     c                                    +   %editc(ORMnsr : 'Z')
155400031117     c                                    +   ' '
155500031117     c                                    +   %editc(ORMnor : 'Z')
155600031117     c                                    +   ' '
155700031117     c                                    +   %editc(ORMnrv : 'Z')
155800090423     c                   WRITE     OR18not
155900090423     c                   write     OR18nul
156000031117     c                   eval      Pnot   =  '            '
156100031117     c                                    +  'NOTE:'
156200090423     c                   WRITE     OR18not
156300090423     c                   write     OR18nul
156400031117e   1c                   endif
156500040713     c                   eval      xy     =  1
156600040713do  1c                   dow       XY     <= YY
156700040713     c                   eval      Pnot   =  $Note(xy) + ' '
156800040713     c                                    +  $Note(xy+1)
156900040713     c                   add       2             xy
157000090423     c                   WRITE     OR18not
157100031117      * - - Avendo massimo 6 righe di note: posso lasciare una riga
157200031117      *     vuota tra una nota e l'altra
157300031215if  2c                   if        YY     <= 6
157400090423     c                   write     OR18nul
157500031117e   2c                   endif
157600031117e   1c                   enddo
157700031117     c                   reset                   $PrtNote
157800031117      *
157900031117     c                   ENDSR
158000050125      *
158100050125      *---------------------------------------------------------------*
158200081209      *?Stampa del Rimesso Mittente                                  ?*
158300050125      *---------------------------------------------------------------*
158400050125     c     Prt_RimMitt   BEGSR
158500050125      *
158600050126     c*** (già così)     eval      *in70 = *on
158700050126     c                   eval      *in71 = *on
158800050125      *
158900050125      * Tolgo impostazione di stampa del codice a barre
159000050125     c                   eval      *in50 = *off
159100110502      *
159200110502      /free
159300120330
159400120330         // -?Impostazione "BRT" per ritiro della merce?
159500120330         Pbrt  = $Brt;
159600110502
159700110502      /end-free
159800050125      *
159900081211      * Posizionamento sul foglio A4 in base al n° pagina in stampa
160000050125     c                   exsr      Posiz_Box
160100050125      *
160200050125      * - RIMESSO MITTENTE
160300090423     c                   WRITE     OR18orm
160400050125      *
160500050125     c                   ENDSR
160600031117      *
160700031117      *---------------------------------------------------------------*
160800081209      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
160900031117      *---------------------------------------------------------------*
161000031117     c     QUM_FNORM     BEGSR
161100031117      *
161200031117     c*** (già fatto:)   clear                   $Qum
161300031117     c                   clear                   YY
161400031117     c                   eval      w0112    = (ORMblc + ORMmtc + ORMatt)
161500031117      * Colli
161600031117if  1c                   if        ORMncl   > *zeros
161700031117     c                             and YY   < %elem($Qum)
161800031117     c                   add       1             yy
161900031117     c                   eval      $Qum(yy) = %editc(ORMncl : '4')
162000031117     c                                      + ' Cl'
162100031117e   1c                   endif
162200031117      * Peso
162300031117if  1c                   if        ORMpkg   > *zeros
162400031117     c                             and YY   < %elem($Qum)
162500031117     c                   add       1             yy
162600031117     c                   eval      $Qum(yy) = %editc(ORMpkg : '4')
162700031117     c                                      + ' Kg'
162800031117e   1c                   endif
162900031117      * Volume
163000031117      * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
163100031117if  1c                   if        ORMvlm   > *zeros
163200031117     c                             and (YY  < %elem($Qum) - 1
163300031117     c                              or  YY  = %elem($Qum) - 1
163400031117     c                              and w0112 = *zeros)
163500031117     c                   add       1             yy
163600031117     c                   eval      $Qum(yy) = %editc(ORMvlm : '4')
163700031117     c                                      + ' Vl'
163800031117e   1c                   endif
163900031117      * Bancali
164000031117      * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
164100031117      * (in ultimo posto da preferire al VOLUME - come da FIOR09R)
164200031117if  1c                   if        ORMbnc   > *zeros
164300031117     c                             and (YY  < %elem($Qum) - 1
164400031117     c                              or  YY >= %elem($Qum) - 1
164500031117     c                              and w0112 = *zeros)
164600031117if  1c                   if        YY       < %elem($Qum)
164700031117     c                   add       1             yy
164800031117e   1c                   endif
164900031117     c                   eval      $Qum(yy) = %editc(ORMbnc : '4')
165000031117     c                                      + ' Bn'
165100031117e   1c                   endif
165200031117      * Bilici
165300031117if  1c                   if        ORMblc  > *zeros
165400031117     c                             and YY  < %elem($Qum)
165500031117     c                   add       1             yy
165600031117     c                   eval      $Qum(yy) = %editc(ORMblc : '4')
165700031117     c                                      + ' Bl'
165800031117e   1c                   endif
165900031117      * Motrici
166000031117if  1c                   if        ORMmtc  > *zeros
166100031117     c                             and YY  < %elem($Qum)
166200031117     c                   add       1             yy
166300031117     c                   eval      $Qum(yy) = %editc(ORMmtc : '4')
166400031117     c                                      + ' Mt'
166500031117e   1c                   endif
166600031117      * Autotreni
166700031117if  1c                   if        ORMatt  > *zeros
166800031117     c                             and YY  < %elem($Qum)
166900031117     c                   add       1             yy
167000031117     c                   eval      $Qum(yy) = %editc(ORMatt : '4')
167100031117     c                                      + ' Au'
167200031117e   1c                   endif
167300031117      *
167400031117     c                   ENDSR
167500140127      *
167600140127      *---------------------------------------------------------------*
167700140127      *?Sch_NoteAUT * Caricamento delle note da FNORT in schiera     ?*
167800140127      *---------------------------------------------------------------*
167900140127     c     Sch_NoteAUT   BEGSR
168000140127      *
168100140127     c*** (già fatto:)   clear                   $Note
168200140127     c                   clear                   YY
168300140127      *
168400140127     c*//?(già così:)?   eval      kORTgst = 'S'
168500140127     c     k05ort11      setll     FNORT000
168600140127     c     k05ort11      reade     FNORT000
168700140127      *
168800140127do  1c                   DOW       NOT %eof(FNORT11L)
168900140127      *
169000140127      * Solo quelle da stampare (già filtrate dalla v.l.)
169100140127      * - Carica la schiera
169200140127if  2c                   if        %subst( ORTnob : 1 :
169300140127     c                                     %len( $Note(1) ) ) <> *blanks
169400140127     c                   add       1             YY
169500140127     c                   eval      $Note(yy) = %subst( ORTnob : 1 :
169600140127     c                                           %len( $Note(1) ) )
169700140127e   2c                   endif
169800140127if  2c                   if        %subst( ORTnob :
169900140127     c                                     %len($Note(1)) + 1 ) <> *blanks
170000140127     c                   add       1             YY
170100140127     c                   eval      $Note(yy) = %subst( ORTnob :
170200140127     c                                           %len( $Note(1) ) + 1 )
170300140127e   2c                   endif
170400140127if  2c                   if        YY = %elem($Note)
170500140127     c                   leave
170600140127e   2c                   endif
170700140127      *
170800140127     c     k05ort11      reade     FNORT000
170900140127      *
171000140127e   1c                   ENDDO
171100140127      *
171200140127     c                   ENDSR
171300031117      *
171400031117      *---------------------------------------------------------------*
171500081209      *?Posiz_BOX  * Posizionamento del box all'interno della pag.A4 ?*
171600031117      *---------------------------------------------------------------*
171700031117     c     Posiz_BOX     BEGSR
171800031117      *
171900031117     c                   add       1             wPage
172000031117      *
172100081211      * - Calcolo la posizione x box all'interno della pagina "A4"
172200031117     c     wPage         div       $BxP          wBoxP
172300031117     c                   mvr                     wBoxP
172400031117if  1c                   if        wBoxP = *zeros
172500081210      * - - Se 4° ORM nella pagina, ma ORM con Rimesso Mittente:
172600081210      *     devo saltare alla pag. A4 successiva: l'ORM in esame
172700081209      *     dev'essere SEMPRE seguito dal rimesso mittente corrispon-
172800081209      *     dente (senza note aggiuntive).
172900050125sel 2c                   select
173000050125w   2c                   when          §ORsrm <> 'S'
173100050125     c                             or  §ORsrm  = 'S'
173200081210     c                             and *in71   = *on
173300031117     c                   eval      wBoxP = $BxP
173400050125w   2c                   when          §ORsrm  = 'S'
173500081210w   2c                             and *in71   = *off
173600050125     c                   add       1             wPage
173700050125     c                   z-add     1             wBoxP
173800050125e   2c                   endsl
173900031117e   1c                   endif
174000031117      *
174100031117      * - Imposto le posizioni nella pagina in corso di stampa
174200081210     c                   eval      wPosIni_B = (2.835 * (wBoxP - 1))
174300081210      /free
174400081210         if  wBoxP > 2;
174500081210           wPosIni_B += 0.167;
174600081210         endif;
174700110427
174800120330         // -?Simil-Logo BRT?
174900120330         §PlblS  = wPosIni_B + $PS0 - 0.010;
175000120330         §PlblVS = 7.541;
175100120330         exsr  sr_SimilLogoBRT_;
175200120330
175300081210      /end-free
175400031117     c                   eval      §PS1 = wPosIni_B + $PS1
175500031117     c                   eval      §LOA = wPosIni_B + $LOA
175600050125if  1c                   if            §ORsrm <> 'S'
175700050125     c                             or  §ORsrm  = 'S'
175800050126     c                             and *in71   = *off
175900031117     c                   eval      §LOB = wPosIni_B + $LOB
176000031127     c                   eval      §LOC = wPosIni_B + $LOC
176100031127     c                   eval      §LOD = wPosIni_B + $LOD
176200050125x   1c                   else
176300050125     c                   eval      §LOB = wPosIni_B + $LOD
176400050125e   1c                   endif
176500031117     c                   eval      §LO1 = wPosIni_B + $LO1
176600031117     c                   eval      §LO2 = wPosIni_B + $LO2
176700031117     c                   eval      §LO3 = wPosIni_B + $LO3
176800031117     c                   eval      §LO4 = wPosIni_B + $LO4
176900131129if  1c                   if        *in61
177000131129     c                   eval      §LO5 = wPosIni_B + $LOB
177100131129e   1c                   endif
177200050126if  1c                   if        §ORsrm   = 'S'
177300050125     c                   eval      §LO5 = wPosIni_B + $LOB
177400050126if  2c                   if        NOT *in71
177500050126     c                   eval      §LO6 = §LOD - 0,10
177600050126x   2c                   else
177700050125     c                   eval      §LO6 = §LOB - 0,10
177800050126e   2c                   endif
177900050126e   1c                   endif
178000031117      *
178100081209      * - Posizionamento nella pagina A4 in base al n° foglio in stampa
178200031117     c                   eval      *in41 = (wBoxP = 1)
178300031117     c                   eval      *in42 = (wBoxP = 2)
178400081210     c                   eval      *in43 = (wBoxP = 3)
178500081210     c                   eval      *in44 = (wBoxP = 4)
178600090423     c                   WRITE     OR18pos
178700031117      * - box / barcode
178800031117if  1c                   if        $PrtNote = *off
178900090423     c                   WRITE     OR18box
179000031117e   1c                   endif
179100120330      * - logo BRT
179200120330     c                   WRITE     OR18logBRT
179300031117      *
179400031117     c                   ENDSR
179500120330
179600120330       //--------------------------------------------------------------
179700120330       //?Posizionamento del simil-logo BRT (unico)                    ?
179800120330       //--------------------------------------------------------------
179900120330      /copy gaitrasrc/srcTNSY,LogoBRT_R2
180000110427
180100031117      *---------------------------------------------------------------*
180200081209      *?StampaOrmE * Stampa singolo O.R.M. ESTERO                    ?*
180300031117      *---------------------------------------------------------------*
180400031117     c     StampaORMe    BEGSR
180500031117      *
180600031117     c                   clear                   OR16est
180700060123     c                   eval      *in10 = *off
180800061003     c                   eval      *in11 = *off
180900081209      *
181000061006      * controllo se sollecito
181100081209     c                   eval      *in06 = (R16ris = 'X')
181200031117      *
181300031117      * Network
181400031117     c                   movea     '00'          *in(04)
181500031117sel 1c                   select
181600031117w   1c                   when      $DPD    = *on
181700031117     c                   movea     '01'          *in(04)
181800031117w   1c                   when      $Estero = *on
181900031117     c                   movea     '10'          *in(04)
182000031117e   1c                   endsl
182100031117      *
182200031117     c                   reset                   DS_ksc
182300031117      * Cerco da cappario DPD
182400081209      * quello nuovo (ora il depot è lungo 4 e non più 3)
182500081209if  1c                   IF        $DPD = *on
182600081209     c                   clear                   TISIE3ds
182700081209     c                   eval      Isie3dri = DateU
182800081209     c                   eval      Isie3dsp = ORMdar
182900060921     c                   time                    isie3hsp
183000081209     c                   eval      Isie3nzd = ORMnar
183100081209     c                   eval      Isie3cad = ORMcar
183200081209if  2c                   if        ORMpkg   > §3ilmp
183300081209     c                   eval      Isie3srv = 101
183400081209x   2c                   else
183500081209     c                   eval      Isie3srv = 136
183600081209e   2c                   endif
183700081209     c                   eval      Isie3lnp = ORMpoe
183800060921     c                   call      'TISIE3R'
183900081209     c                   parm                    TISIE3ds
184000081209if  2c                   if        Osie3err = *blanks
184100081209     c                   movel     Osie3ddep     dsKsc_e3ddep
184200081209     c                   move      Osie3ddep     Pdep
184300060921e   2c                   endif
184400031117      * - Cerco il P.O. di ritiro dalla tabella delle nazioni
184500031117     c                   clear                   ds15
184600031117     c                   movel     '15'          TBLcod
184700031117     c                   movel(p)  ORMnar        TBLkey
184800031117     c     kTABEL        chain     TABEL
184900031117if  2c                   if        %found(TABEL00F)
185000031117     c                   movel     TBLuni        ds15
185100031117e   2c                   endif
185200081209     c                   z-add     §15lad        dsKsc_POR
185300081209      * - Con il depot richiamo pgm per sapere se depot automatizzato
185400081209      *     o meno e mi torna il codice cliente
185500081209     c                   clear                   TRULDEPds
185600081209     c                   eval      Idepdpc = Osie3ddep
185700081209     c                   eval      Idepdrf = ORMdar
185800061003     c                   call      'TRULDEPR'
185900081209     c                   parm                    TRULDEPds
186000081209     c                   if        Odeperr = *blanks
186100081209     c                   move      Odepksc       ds_ksc
186200061003     c                   endif
186300081209     c                   eval      *in11 = (Odeporma = 'S')
186400081209e   1c                   ENDIF
186500031117      *
186600031117      * Cerco dati da tabella Partner
186700031117if  1c                   if        $Estero = *on
186800040713     c                   z-add     1             xy
186900040713     c     ORMpor        lookup    $LNP(xy)                               21
187000031117if  2c                   if        *in21   = *on
187100040713     c                   move      $ksc(xy)      DS_ksc
187200031117e   2c                   endif
187300031117e   1c                   endif
187400081209      *
187500081209     c                   clear                   wMail
187600081209      *
187700081209      * Se depot automatico
187800081209      *   devo scrivere il file e basta se non è ristampa
187900081209      *                                  e non è sollecito
188000081209if  1c                   if             *in11
188100081209     c                             and  R16ris <> 'R'
188200081209     c                             and  R16ris <> 'X'
188300081209     c                   clear                   FIEU49ds
188400081209     c                   eval      Id49poe = ORMpoe
188500081209     c                   eval      Id49nrs = ORMnsr
188600081209     c                   eval      Id49nor = ORMnor
188700081209     c                   eval      Id49nrv = ORMnrv
188800081209     c                   eval      Id49dao = ORMdao
188900061005     c                   call      'FIEU49R'
189000081209     c                   parm                    FIEU49ds
189100081209     c                   if        Od49esito = '0'
189200061005     c                   leavesr
189300061005     c                   endif
189400081209e   1c                   endif
189500081209      *
189600031117      * Controllo se c'è un luogo di spedizione per il fax
189700081209if  1c                   IF        ds_ksc <> *zeros
189800081209     c                   move      ds_ksc        kSPEcli
189900031117     c     kFNSPE        chain     FNSPE000
190000081209if  2c                   If            %found(FNSPE01L)
190100031117     c                             and SPEflg =  *blanks
190200031117     c                   movel     SPErag        Pdepd
190300031117     c                   movel     SPEfax        Pfax
190400081209      * - controllo se oltre al fax c'è anche la relativa e-mail
190500060123     c     kFNSP2        chain     FNSP2000
190600081209if  3c                   if            %found(FNSP201L)
190700060123     c                             and SP2flg =  *blanks
190800081209     c                             and SP2est <> *blank
190900081209     c                   eval      wMail = SP2est
191000081209e   3c                   endif
191100081209x   2c                   Else
191200081209      * - non ho trovato il luogo cerco se c'è la mail sulla nota 83
191300081209     c                   movel(p)  DUTkci        kntcnk1
191400081209     c                   move      ds_ksc        kntcnk1
191500081209     c     kTfntc        chain     TFNTC
191600081209if  3c                   if        %found(TFNTC01L)
191700081209     c                   eval      wMail = NTCrnt
191800081209e   3c                   endif
191900031117      *   Ragione sociale e fax
192000031117     c                   clear                   TIBS69DS
192100031117     c                   move      DS_ksc        I69kac
192200031117     c                   move      DS_ksc        I69kin
192300031117     c                   call      'TIBS69R'
192400081209     c                   parm                    TIBS69ds
192500081209     c                   parm                    ds_CNACO
192600081209     c                   parm                    ds_CNIND
192700081209     c                   parm                    ds_CNCLP
192800081209     c                   parm                    ds_FNCLS
192900081209if  3c                   if        O69err <> *on
193000031117     c                   movel     ACOrag        Pdepd
193100031117     c                   movel     INDtlf        Pfax
193200081209e   3c                   endif
193300081209e   2c                   EndIf
193400081209e   1c                   ENDIF
193500031117      *
193600031117      * Dati del ritirante (mittente)
193700031117     c                   eval      Prsr  =  ORMrsr
193800031117     c                   eval      Pinr  =  ORMinr
193900031117     c                   eval      Pcar  =  ORMcar
194000031117     c                   eval      Plpr  =  %trim(ORMlor) + '  '
194100031117     c                                   +  %trim(ORMprr)
194200031117     c                                   +  %trim(ORMnar)
194300031117     c                   movel     ORMrer        Prer2
194400031117     c                   movel     ORMter        Pter
194500031117      *
194600031117      * Numero colli e peso
194700031117     c                   eval      Pncl  =  ORMncl
194800031117     c                   eval      Ppkg  =  ORMpkg
194900121120      * Bancali
195000121120     c                   eval      Pbnc  =  ORMbnc
195100120925      *
195200120925      * - Natura della merce
195300120925     c                   movel     ORMnam        Pnam
195400160413      * - Note
195500160413     c                   movel     ORMno1        Pno1
195600160413     c                   movel     ORMno2        Pno2
195700031117      *
195800031117      * Dati del consegnante (destinatario)
195900031117     c                   eval      Prsc  =  ORMrsc
196000031117     c                   eval      Pinc  =  ORMinc
196100031117     c                   eval      Pcac  =  ORMcac
196200031117     c                   eval      Plpc  =  %trim(ORMloc) + '  '
196300031117     c                                   +  %trim(ORMprc)
196400031117     c                                   +  %trim(ORMnac)
196500031117      *
196600031117      * Inversione data ritiro
196700031117     c                   clear                   WLBdat
196800031117     c                   z-add     ORMdar        G02inv
196900031117     c                   movel     '3'           G02err
197000031117     c                   call      'XSRDA8'
197100031117     c                   parm                    WLBdat
197200031117     c                   z-add     G02dat        Pdar
197300031117      *
197400031117      * Numero ORM
197500031117     c                   eval      Porm  =  %trim(
197600031117     c                                        %editw(ORMpoe:'0   '))
197700031117     c                                   +  '-'
197800031117     c                                   +  %trim(
197900031117     c                                        %editc(ORMnsr:'Z'))
198000031117     c                                   +  %trim(
198100031117     c                                        %editc(ORMnor:'Z'))
198200031117     c                                   +  '-'
198300031117     c                                   +  %trim(
198400031117     c                                        %editw(ORMnrv:'0  '))
198500160415     c                   eval      pormrmd = porm
198600031117      *
198700031117      * Depot
198800031117     c                   clear                   dOES
198900031117if  1c                   if        $Estero = *on
199000031117     c                             or $DPD = *on
199100031117     c                   clear                   TIBS02DS
199200031117     c                   movel     'C'           T02mod
199300031117     c                   movel     KNSIF         T02sif
199400031117     c                   movel     'OES'         T02cod
199500031117sel 2c                   select
199600031117w   2c                   when      $DPD    = *on
199700060502     c                   movel(p)  §OGdp1        T02ke1
199800031117w   2c                   when      $Estero = *on
199900081209     c                   move      ORMpor        w0040
200000060503     c                   movel(p)  w0040         T02ke1
200100031117e   2c                   endsl
200200031117     c                   call      'TIBS02R'
200300031117     c                   parm                    KPJBA
200400031117     c                   parm                    TIBS02DS
200500031117     c                   movel     T02uni        dOES
200600031117e   1c                   endif
200700031117     c                   movel     d§OESdes      Poesd
200800160413     c*****              movel     d§OESfax      Poesf
200900160413     c*****              movel     d§OESnom      Poesn
201000031117      *
201100031117      * Data odierna
201200031117     c                   z-add     w0140         Pdat
201300130521
201400130521      * Imposto in stampa il tipo invio al partner
201500130521     c                   clear                   wfcs
201600130521     c                   select
201700130521     c                   when      R16ris = 'R' or R16ris = 'X'
201800130521     c                   eval      wfcs = ORMfcs                                da ORM
201900130521     c                   when      *in11 or wOKtraduttore
202000151126     c                             or NewTradPartner
202100130521     c                   eval      wfcs = 'F'                                   File
202200130521     c                   when      wmail <> *blanks
202300130521     c                   eval      wfcs = 'M'                                   Mail
202400130521     c                   other
202500130521     c                   eval      wfcs = 'M'                                   Mail
202600130521     c                   endsl
202700130521     c                   clear                   pfcs
202800130521     c                   clear                   TIBS02DS
202900130521     c                   movel     'C'           T02mod
203000130521     c                   movel     KNSIF         T02sif
203100130521     c                   movel     'MIP'         T02cod
203200130521     c                   movel(p)  wfcs          T02ke1
203300130521     c                   call      'TIBS02R'
203400130521     c                   parm                    KPJBA
203500130521     c                   parm                    TIBS02DS
203600130521if  1c                   If        T02err   = *blanks
203700130521     c                   movel     T02uni        pfcs
203800130521e   1c                   EndIf
203900160415      *
204000160415      * SE ORM Export DPD recupero il numero ORM che mandiamo a DPD
204100160415      * ma solo se sollecito per poi impostarlo nell'oggetto della mail
204200160516      * recupero anche il n. parcel
204300160415     c                   clear                   Pormdpd
204400160516     c                   clear                   PParcel
204500160415     c                   if        $dpd = *on and *in06
204600160415     c     kfidpo        chain     FIDPO000
204700160415     c                   if        %found(FIDPO01L)
204800160415     c                   eval      Pormdpd = DPOdep + '/' +
204900160415     c                             %editc(DPOord:'X')
205000160516     c                   IF        DPOnrp <> *blanks
205100160516     c                   eval      pparcel = 'Parcel Reference ' +
205200160516     c                             DPOnrp
205300160516     c                   ENDIF
205400160415     c                   endif
205500160415     c                   endif
205600160413      /free
205700160413       //?Richiamo in questo punto il TRUL44R
205800160413       //?in modo da avere l'indirizzo mail del mittente da stampare
205900160413         exsr CallTrul44;
206000160413         pmail = d44mit;
206100160413      /end-free
206200031117      *
206300031117      *
206400031117      * Stampa "Estero"
206500031117     c                   eval      *in20   = *on
206600031117     c                   write     OR16est
206700130521
206800130605      * se è stampa ORM e NON sollecito
206900130605     c                   IF        not *in06
207000130521      * SE ORM INVIATO AL PARTENER PER FILE NON VADO AVANTI CON FAX/MAIL
207100130521     c                   IF        wOKtraduttore or ORMfcs = 'F'
207200151126     c                             or NewTradPartner
207300130521     c                   eval      *in11 = *on
207400130522      * a meno che non è richiesto da tabella di inviare anche mail/fax
207500130522     c                   IF        not wSIelenco
207600130521     c                   leavesr
207700130522     c                   ENDIF
207800130521     c                   ENDIF
207900130605     c                   ENDIF
208000130521
208100060123      * se c'è la mail devo inviare l'orm estero via mail
208200060123if  1c                   if        wmail <> *Blanks
208300060123     c                   eval      *in10 = *on
208400060123      * chiudo il printer file già aperto
208500060123if  2c                   if        %open(ORMest)
208600060123     c                   close     ORMest
208700090129      /free
208800090129         clear  TrulOvrPds;
208900090930         uopToDo   = 'B';
209000090129         uopPrtF   = 'ORMEST    ';
209100090129         trulOvrPr ( trulOvrPds );
209200090129         Qcmd  = uopCmd;
209300090129      /end-free
209400060123     c                   call      'QCMDEXC'                            90
209500060123     c                   parm                    Qcmd
209600060123     c                   parm                    Qlen
209700060123e   2c                   endif
209800081209      *
209900160413    2c                   IF        wOkTrul44
210000060123if  3c                   IF        NOT %open(ORMest)
210100090129      /free
210200090129         clear  TrulOvrPds;
210300090930         uopToDo   = 'A';
210400090129         uopPrtF   = 'ORMEST    ';
210500090129         uopOutQ   = D44outq;
210600090129         uopSave   = '*YES';
210700090129         trulOvrPr ( trulOvrPds );
210800110427         Qcmdm = uopCmd + ' usrdfndta(''' + %trimr(D44dta) + ''')';
210900090129      /end-free
211000060123     c                   call      'QCMDEXC'                            90
211100060123     c                   parm                    Qcmdm
211200060123     c                   parm                    Qlenm
211300060123      *
211400060123     c                   open      ORMest
211500060123e   3c                   endif
211600060123e   2c                   endif
211700060123    1c                   endif
211800081209      *
211900081209     c  n10              write     ORMeff
212000081209     c   10              write     ORMefm
212100031117      *
212200031117      *
212300031117      * Stampa "Estero-Fax" (senza parole accentate)
212400081209if  1c                   if            *in05    = *on
212500081209     c                             and Osie3err = *blanks
212600081209     c                   movel     Osie3ddep     Ecoddep
212700031117e   1c                   endif
212800130508     c                   eval      Einvprtn   = pfcs
212900031117     c                   eval      Edeposito  = Pdepd
213000031117     c                   eval      Efaxdep    = Pfax
213100031117     c                   eval      Elocalitar = Plpr
213200041213     c                   eval      Eormrer    = Prer2
213300031117     c                   eval      Elocalitac = Plpc
213400031117     c                   eval      Eormdar    = Pdar
213500031117     c                   eval      Enumorm    = Porm
213600031117     c                   eval      Wdtgio     = Pdat
213700160413     c*****              eval      Eorgnom    = Poesn
213800160413     c                   eval      email      = pmail
213900031117     c                   write     ORMef
214000031117      *
214100031117      * "Libera" subito la pagina in stampa
214200031117     c                   close     ORMest
214300090129      /free
214400090129         clear  TrulOvrPds;
214500090930         uopToDo   = 'B';
214600090129         uopPrtF   = 'ORMEST    ';
214700090129         trulOvrPr ( trulOvrPds );
214800090129         Qcmd  = uopCmd;
214900090129      /end-free
215000040218     c                   call      'QCMDEXC'                            90
215100040218     c                   parm                    Qcmd
215200040218     c                   parm                    Qlen
215300031117      *
215400031117     c                   ENDSR
215500031117      *
215600031117      *---------------------------------------------------------------*
215700081209      *?UpdatFNORM * Aggiornamento Ordine Ritiro Merce (FNORM00F)    ?*
215800031117      *---------------------------------------------------------------*
215900031117     c     UpdatFNORM    BEGSR
216000031117      *
216100031117      * Data di stampa
216200031117     c                   z-add     DateU         ORMdst
216300081209     c                   z-add     DateU         SavORMdst
216400070924      * scrivo la fase se non attivo con la precedura GEO ORM
216500081209     c                   if        Data_Org > ORMdst  or
216600081209     c                             Data_Org = *zeros
216700031117      * Fase
216800031117if  1c                   if        ORMfao < 300
216900031117     c                   eval      ORMfao = 300
217000031117     c                   z-add     DateU         ORMdfo
217100031117     c                   movel     w0140         ORMofo
217200081210     c                   exsr      Tab_FAR
217300081210     c                   eval      ORMeti = d§FARass
217400031117e   1c                   endif
217500070924e   1c                   endif
217600060921      * modalità invio al partner
217700060925if  3c                   if        $Estero  = *on
217800060925     c                             or $DPD = *on
217900081210     c   11              eval      ORMfcs = 'F'                                 File
218000061005     c  n11              do
218100081210     c   10              eval      ORMfcs = 'M'                                 Mail
218200081210     c  n10              eval      ORMfcs = 'X'                                 Fax
218300061005     c                   enddo
218400060925     c                   endif
218500031117      *
218600071106     c                   z-add     DateU         ORMdtt
218700081209      *
218800081209      * SE ORM non in distinta pulisco la data di stampa
218900081209      *   (solo se procedura GEO ORM attiva)
219000081209     c                   if             ORMndc   = *zeros
219100081209     c                             and  ORMdst  >= Data_Org
219200081209     c                             and  Data_Org > *zeros
219300081209     c                   clear                   ORMdst
219400070924     c                   endif
219500081209      *
219600041122     c                   UPDATE    FNORM00
219700031117      *
219800031117     c                   ENDSR
219900031117      *
220000031117      *---------------------------------------------------------------*
220100081209      *?WriteFNORF * Scrittura del file "Storico fasi" (FNORF00F)    ?*
220200031117      *---------------------------------------------------------------*
220300031117     c     WriteFNORF    BEGSR
220400081209      *
220500070924      * scrivo la fase se non attivo con la precedura GEO ORM
220600081209     c                   if        Data_Org > SavORMdst or
220700081209     c                             Data_Org = *zeros
220800031117      *
220900031117     c                   clear                   FNORF000
221000031117      *
221100031117     c                   eval      ORFpoe = ORMpoe
221200031117     c                   eval      ORFnsr = ORMnsr
221300031117     c                   eval      ORFnor = ORMnor
221400031117     c                   eval      ORFnrv = ORMnrv
221500031117     c                   eval      ORFpog = R16por
221600031117     c                   eval      ORFdae = DateU
221700031117     c                   movel     w0140         ORFore
221800031117     c                   eval      ORFfar = 300
221900031117     c                   eval      ORFpue = KNMUS
222000040914      * riporto i dati della distinta se già abbinato l'orm
222100081209     c                   if        ORMndc > *zeros
222200081209     c                   eval      ORFfgs = ORMpor
222300081209     c                   eval      ORFndc = ORMndc
222400081209     c                   eval      ORFddc = ORMddc
222500081209     c                   endif
222600031117      *
222700031117     c                   WRITE     FNORF000
222800081209      *
222900070924     c                   endif
223000031117      *
223100031117     c                   ENDSR
223200110427      *
223300160413       //-----------------------------------------------------------------
223400160413       //?Richiamo il TRUL44R.
223500160413       //------------------------------------------------------------------
223600160413     c     CallTrul44    BEGSR
223700160413
223800160413     c                   eval      wOkTrul44 = *on
223900160413     c                   clear                   TRUL44ds
224000160413     c                   eval      D44pgm = 'FIOR16R'
224100160413     c                   z-add     ORMpor        D44po
224200160413     c                   eval      D44des = wMail
224300160413     c  n06              eval      §CM1var  = '*OBJM*' + 'COLLECTION -
224400160413     c                             REQUEST n.' + Porm
224500160413     c   06              eval      §CM1var  = '*OBJM*' + 'REMINDER -
224600160413     c                             COLLECTION REQUEST n.' + Porm
224700160413     c                             + ' DPD ' + Pormdpd
224800160413     c                   call      'TRUL44R'
224900160413     c                   parm                    TRUL44ds
225000160413     c                   parm                    TRTCM1ds
225100160413     c                   IF        D44err <> *blanks
225200160413     c                   eval      wOkTrul44 = *off
225300160413     c                   ENDIF
225400160413
225500160413     c                   ENDSR
225600110427      *---------------------------------------------------------------*
225700110427      *?OPERAZIONI FINALI                                            ?*
225800110427      *---------------------------------------------------------------*
225900110427     c     sr_End        BEGSR
226000110427      *
226100110427      *
226200110427if  1c                   IF        R16tla  = *blanks
226300110427     c                   eval      *inRT   = *on
226400110427x   1c                   ELSE
226500110427      * - Chiudo i file aperti
226600110427      *   e Cancello le eventuali override ai prtf
226700110427     c                   eval      PrmFine = 'L'
226800110427     c                   clear                   PrmFil
226900110427     c                   clear                   PrmTpOutq
227000110427     c                   clear                   PrmOutQ
227100110427     c                   clear                   PrmOutQLib
227200110427     c                   clear                   PrmEsito
227300110427     c                   call      'TRULOUTQ'
227400110427     c                   parm                    PrmFine
227500110427     c                   parm                    PrmFil
227600110427     c                   parm                    PrmTpOutQ
227700110427     c                   parm                    PrmOutQ
227800110427     c                   parm                    PrmOutQLib
227900110427     c                   parm                    PrmEsito
228000110427if  2c                   if        %open(FIOR18P)
228100110427     c                   close     FIOR18P
228200110427      /free
228300110427         clear  TrulOvrPds;
228400110427         uopToDo   = 'B';
228500110427         uopPrtF   = 'FIOR18P   ';
228600110427         trulOvrPr ( trulOvrPds );
228700110427         Qcmd  = uopCmd;
228800110427      /end-free
228900110427     c                   call      'QCMDEXC'                            90
229000110427     c                   parm                    Qcmd
229100110427     c                   parm                    Qlen
229200110427e   2c                   endif
229300110427if  2c                   if        %open(FIOR16P2)
229400110427     c                   close     FIOR16P2
229500110427      /free
229600110427         clear  TrulOvrPds;
229700110427         uopToDo   = 'B';
229800110427         uopPrtF   = 'FIOR16P2  ';
229900110427         trulOvrPr ( trulOvrPds );
230000110427         Qcmd  = uopCmd;
230100110427      /end-free
230200110427     c                   call      'QCMDEXC'                            90
230300110427     c                   parm                    Qcmd
230400110427     c                   parm                    Qlen
230500110427e   2c                   endif
230600110427if  2c                   if        %open(ORMest)
230700110427     c                   close     ORMest
230800110427      /free
230900110427         clear  TrulOvrPds;
231000110427         uopToDo   = 'B';
231100110427         uopPrtF   = 'ORMest    ';
231200110427         trulOvrPr ( trulOvrPds );
231300110427         Qcmd  = uopCmd;
231400110427      /end-free
231500110427     c                   call      'QCMDEXC'                            90
231600110427     c                   parm                    Qcmd
231700110427     c                   parm                    Qlen
231800110427e   2c                   endif
231900110427     c                   clear                   TIBS02ds
232000110427     c                   eval      T02tla = 'C'
232100110427     c                   call      'TIBS02R'
232200110427     c                   parm                    KPJBA
232300110427     c                   parm                    TIBS02ds
232400110427     c                   eval      *inLR   = *on
232500110427e   1c                   ENDIF
232600130510
232700130510     c                   eval      kpjbu = FIOR16ds
232800110427      *
232900110427     c                   return
233000110427      *
233100110427     c                   ENDSR
233200031117      *
233300031117      *---------------------------------------------------------------*
233400081209      *?DEFINIZIONE KEY-LIST                                         ?*
233500031117      *---------------------------------------------------------------*
233600031117     c     DefKLIST      BEGSR
233700031117      *
233800140127      * FNORM01L
233900031117     c     k04orm01      klist
234000031117     c                   kfld                    R16poe
234100031117     c                   kfld                    R16nsr
234200031117     c                   kfld                    R16nor
234300031117     c                   kfld                    R16nrv
234400140127      * FNORT11L
234500140127     c     k05ort11      klist
234600140127     c                   kfld                    R16poe
234700140127     c                   kfld                    R16nsr
234800140127     c                   kfld                    R16nor
234900140127     c                   kfld                    R16nrv
235000140127     c                   kfld                    kORTgst
235100031117      * FNORM06L / FNORM11L
235200031117     c     k02orm06      klist
235300031117     c                   kfld                    $L6(xx)
235400031117     c                   kfld                    R16ndc
235500031117      * FNSPE01L
235600031117     c     kFNSPE        klist
235700031117     c                   kfld                    kSPEfls
235800031117     c                   kfld                    kSPEcli
235900031117     c                   kfld                    kSPEcod
236000060123      * FNSP201L
236100060123     c     kFNSP2        klist
236200060123     c                   kfld                    kSPEcli
236300060123     c                   kfld                    kSPEcod
236400060123     c                   kfld                    kSP2tpe
236500060214      * FIAR404L
236600060214     c     kFIAR4        klist
236700060214     c                   kfld                    kAR4trc
236800060214     c                   kfld                    kAR4n14
236900120612      * fidst01l
237000120612     c     kfidst        klist
237100120612     c                   kfld                    DSTnpg
237200031117     c                   kfld                    DSTnfv
237300031117     c                   kfld                    DSTfgs
237400031117      * FIAPD01L
237500031117     c     kFIAPD        klist
237600031117     c                   kfld                    APDtip
237700031117     c                   kfld                    APDpdr
237800081209      * TFNTC01L
237900081209     c     kTFNTC        klist
238000081209     c                   kfld                    kNTCapl
238100081209     c                   kfld                    kNTCnk1
238200081209     c                   kfld                    kNTCnk2
238300081209     c                   kfld                    kNTCtnt
238400081209      * FIDPO01L
238500081209     c     kFIDPO        klist
238600081209     c                   kfld                    ORMpoe
238700081209     c                   kfld                    ORMnsr
238800081209     c                   kfld                    ORMnor
238900081209     c                   kfld                    ORMnrv
239000031117      * EDTAB01L
239100031117     c     kEDTAB        klist
239200031117     c                   kfld                    TABcod
239300031117     c                   kfld                    TABkey
239400031117      * TABEL00F
239500031117     c     kTABEL        klist
239600050307     c                   kfld                    TBLkut
239700031117     c                   kfld                    TBLcod
239800031117     c                   kfld                    TBLkey
239900031117      *
240000031117     c                   ENDSR
