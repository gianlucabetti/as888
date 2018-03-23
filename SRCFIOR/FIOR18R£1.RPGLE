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
016600110427
016700110427       // -?Parametri per TRUL06R - caricamento £x?
016800050307     d TRUL06ds      e ds                  inz
016900031117     d  Lin                    1     90  0 dim(30)
017000110427
017100110427       // -?Controllo/Decodifica codice cliente?
017200050307     d TIBS69ds      e ds                  inz
017300031117     d ds_CNACO      e ds                  inz  extname(CNACO00F)
017400031117     d ds_CNIND      e ds                  inz  extname(CNIND00F)
017500031117     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
017600031117     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
017700110427
017800130508       // -?Tabelle PT e PU = Partner EDI estero e PR - PO
017900130508     d EDIdsPO       e ds
018000130508     d EDIdsPR       e ds
018100110427     d EDIdsPT       e ds
018200110427     d EDIdsPU       e ds
018300110427       // -?Tabella 15 = Nazioni?
018400031117     d ds15          e ds                  inz
018500110427       // -?Tabella 3I/DPD = Variabili x DPD?
018600081209     d ds3Idp        e ds
018700110427       // -?Tabella FAR = Fasi O.R.M.?
018800031117     d dFAR          e ds                  inz
018900110427       // -?Tabella OES = Dati standard per O.R.M. Estero?
019000031117     d dOES          e ds                  inz
019100110427       // -?Tabella OSR = Gestione Serie O.R.M.?
019200031117     d dOSR          e ds                  inz
019300151126
019400151126       // -?Tabella ALP - Abbinamento Linea/Partner
019500151126     d dALP          e ds                  inz
019600151126
019700110427       // -?Interrogazione/Controllo tabelle?
019800110427     d TIBS02ds      e ds                  inz
019900110427
020000110427       // -?Calcolo instradamento cappario DPD GeoPost?
020100081209     d TISIE3ds      e ds                  inz
020200110427       // -?Reperimento cod.cliente di un depot DPD?
020300081209     d TRULDEPds     e ds                  inz
020400110427       // -?Scrittura del FIDPO00F?
020500081209     d FIEU49ds      e ds                  inz
020600110427
020700110427       // -?Reperimento USRDFNDTA x comunicazioni via e-mail?
020800081209     d TRUL44ds      e ds                  inz
020900110427       // -?Ridefinizione dati utenti estesi x mailing PDF?
021000081209     d TRTCM1ds      e ds                  inz
021100110427
021200110427       // -?Gestione rcd allocato?
021300081209     d TRUL82ds      e ds
021400130509
021500130509       // -?DS per passaggio parametri x pgm trasmissione ORM a partner
021600130520     d TRTCTORMds    e ds                  inz
021700110427
021800110427       // -?InfDS?
021900081209     d ORMinf          ds
022000081209     d  ORMnrr               397    400i 0
022100110427
022200110427       // -?Controllo/Inversione date di 8 cifre?
022300031117     d WLBDAT          ds                  inz
022400031117     d  G02dat                 1      8  0 inz
022500031117     d  G02inv                 9     16  0 inz
022600031117     d  G02err                17     17    inz
022700031117     d  G02tgi                18     22  0 inz
022800110427
022900110427       // -?Codice a Barre della LDV?
023000081209     d BARcodeDS       ds            14    inz
023100081209     d  DSkeyOSR               1      5  0 inz
023200151126     d   wORMpoe               1      3  0 inz
023300151126     d   wORMnsr               4      5  0 inz
023400151126     d  wORMnor                6     12  0 inz
023500151126     d  wORMnrv               13     14  0 inz
023600110427
023700110427       // -?Scomposizione codice cliente?
023800031117     d DS_ksc          ds             7    inz
023900031117     d  DsKsc_POR              1      3  0 inz
024000060921     d  dsksc_e3ddep           4      7  0 inz
024100110427
024200110427       //--------------------------------------------------------------
024300110427       //?Definizione variabili globali.                               ?
024400110427       //--------------------------------------------------------------
024500151126
024600151126       // -?Parametri per pgm. "TIS7TBOR"?
024700151126     d pin_Opz         s              5a
024800151126     d pin_KSU         s              8a   inz('00000000')
024900151126     d pOut_Esito      s              1a
025000110427
025100110427       // -?Parametri per pgm. "TRULOUTQ"?
025200040218     d PrmFine         s              1
025300040218     d PrmFil          s              3s 0
025400040218     d PrmTpOutQ       s              1
025500040218     d PrmOutQ         s             10
025600040218     d PrmOutQLib      s             10
025700040218     d PrmEsito        s              1
025800110427
025900110427       // -?Parametri per pgm. "QCMDEXC"?
026000031117     d Qcmd            s            156    inz
026100031117     d Qlen            s             15  5 inz(156)
026200060123     d Qcmdm           s            395    inz
026300060123     d Qlenm           s             15  5 inz(395)
026400110427
026500110427       // -?Indici di schiera?
026600031117     d XX              s              3  0 inz
026700040713     d XY              s              3  0 inz
026800031117     d YY              s              3  0 inz
026900130520     d zz              s              4  0 inz
027000110427
027100110427       // -?Flags booleani?
027200110427     d $Estero         s               n   inz
027300110427     d $DPD            s               n   inz
027400110427     d $PrtNote        s               n   inz
027500130509     d wOKtraduttore   s               n   inz
027600130522     d wSIelenco       s               n   inz
027700151126     d NewTradPartner  s               n   inz
027800160413     d wOkTrul44       s               n   inz
027900110427
028000110427       // -?Campi chiave x archivi?
028100031117     d kSPEfls         s                   like(SPEfls)  inz('L')
028200031117     d kSPEcli         s                   like(SPEcli)  inz
028300031117     d kSPEcod         s                   like(SPEcod)  inz('002')
028400060123     d kSP2tpe         s                   like(SP2tpe)  inz('EM')
028500060214     d kAR4trc         s                   like(AR4trc)  inz('M')
028600060214     d kAR4n14         s                   like(AR4n14)  inz
028700110427     d kNTCapl         s                   like(NTCapl)  inz('C')
028800110427     d kNTCnk1         s                   like(NTCnk1)  inz
028900110427     d kNTCnk2         s                   like(NTCnk2)  inz
029000110427     d kNTCtnt         s                   like(NTCtnt)  inz('83')
029100140127     d kORTgst         s                   like(ORTgst)  inz('S')
029200110427
029300110427       // -?Dati del giorno?
029400031117     d w0140           s             14  0
029500031117     d DateU           s              8  0
029600071106     d Data_Org        s              8  0 inz
029700110427
029800110427       // -?Contatori: da stampare, in fase di stampa, posiz. in "A4"?
029900031117     d wPage           s              5  0 inz
030000031117     d wBoxP           s              1  0 inz
030100110427       // -?Posizione iniziale nel calcolo delle posizioni per box?
030200031117     d wPosIni_B       s              5  3 inz
030300110427
030400110427       // -?Campi di comodo?
030500130521     d wfcs            s                   like(ORMfcs)
030600081209     d w0040           s              4  0 inz
030700031117     d w0112           s             11  2 inz
030800081209     d wMail           s             60    inz
030900081209     d Pormdpd         s             12    inz
031000081209     d SavORMdst       s                   like(ORMdst)  inz
031100130118      // - Campo di comodo per Ordinante ORM lungo 7
031200130118     d wORMcor         s              7s 0 inz
031300090129
031400090423       //--------------------------------------------------------------
031500131129       //?Definizione procedure usate.                                 ?
031600090423       //--------------------------------------------------------------
031700090129
031800110427       // -?Override ai PrtF?
031900090129     d trulOvrPr       pr                  extpgm('TRULOVRPR')
032000090129     d  trulOvrPds                         likeds(trulOvrPds)
032100151126
032200151126       // -?Invio ORM ai Partner via File
032300151126     d tis7tbor        pr                  extpgm('TIS7TBOR')
032400151126     d  pin_Opz                       5a
032500151126     d  pin_KSU                       8a
032600151126     d  fnormds                            likeds(FNORMDS)
032700151126     d  pOut_Esito                    1a
032800131129
032900131129       //--------------------------------------------------------------
033000131129       //?Definizione key-list.                                        ?
033100131129       //--------------------------------------------------------------
033200131129
033300131129       // -?File FNORE01L?
033400131129     d keyFNORE01    e ds                  extname(FNORE01L:*key)
033500131129     d                                     prefix(k_)
033600131129     d                                     inz
033700041122
033800031117      *===============================================================*
033900081209      *? RIEPILOGO INDICATORI                                        ?*
034000031117      *---------------------------------------------------------------*
034100110502      *?04   ?- Stampa Euroexpress                                    *
034200110502      *?05   ?- Stampa DPD                                            *
034300110502      *?06   ?- SOLLECITO                                             *
034400110502      *?10   ?- ORM ESTERO VIA MAIL                                   *
034500110502      *?11   ?- ORM ESTERO VIA FILE                                   *
034600110502      *?20   ?- Condiziona "grassetto" in stampa su FIOR16P2          *
034700110502      *?21   ?- Comodo                                                *
034800110502      *?41   ?- Posizionamento nel 1° ¼ del foglio A4                 *
034900110502      *?42   ?- Posizionamento nel 2° ¼ del foglio A4                 *
035000110502      *?43   ?- Posizionamento nel 3° ¼ del foglio A4                 *
035100110502      *?44   ?- Posizionamento nel 4° ¼ del foglio A4                 *
035200110502      *?50   ?- Stampa BarCode                                        *
035300110502      *?60   ?- *Date >= §OGDDAO    (nuova stampa "aut + d:xxxxx")    *
035400110502      *?61   ?- Ora ritiro <= 12:30 (stampata in grassetto)           *
035500110502      *?62   ?- Stampa dati ORM per Ritiro Contestuale in grassetto   *
035600110502      *?70   ?- Stampa Rimesso Mittente: 1ª fase (stampa O.R.M.)      *
035700110502      *?71   ?- Stampa Rimesso Mittente: 2ª fase (dopo O.R.M.)        *
035800110502      *?90   ?- Errore QCmdExc                                        *
035900031117      *===============================================================*
036000110427
036100110427       //--------------------------------------------------------------
036200110427       //?Specifiche di calcolo.                                       ?
036300110427       //--------------------------------------------------------------
036400031117
036500031117     c     *Entry        plist
036600031117     c                   parm                    KPJBA
036700110427
036800031117     c                   movel     KPJBU         FIOR16ds
036900110427
037000110427       // -?Tipo lancio (DB0tla):?"C" -           Chiudo con LR?
037100110427       //                        ?"L" - Elaboro e chiudo con LR?
037200110427       //                        ?" " - Elaboro e chiudo in RETRN?
037300031117     c     R16tla        cabeq     'C'           Fine
037400110427
037500110427       // -?Operazioni iniziali?
037600110427     c                   exsr      sr_Start
037700031117     c     *in90         cabeq     *on           Fine
037800110427
037900110427       // -?Stampa:?
038000031117if  1c                   if        R16nor > *zeros
038100110427       //   ·?Singolo ORM richiesto?
038200031117     c                   exsr      sr_ORM_1
038300031117x   1c                   else
038400110427       //   ·?Tutti gli ORM della £6?
038500031117     c                   exsr      sr_ORM_2
038600031117e   1c                   endif
038700110427
038800110427       // -?Operazioni finali?
038900031117     c     Fine          tag
039000110427     c                   exsr      sr_End
039100031117      *
039200031117      *---------------------------------------------------------------*
039300081209      *?*INZSR     * Routine iniziale                                ?*
039400031117      *---------------------------------------------------------------*
039500031117     c     *InzSr        BEGSR
039600031204      *
039700031204     c                   if        %subst(KPJBU:1:1) = 'C'
039800110427     c                   leavesr
039900031204     c                   endif
040000031117      *
040100050307      * Reperisco dati aziendali
040200050307     c     *dtaara       define    §azute        AZUTEds
040300050307     c     *dtaara       define    §datiute      dDATIUTE
040400050307     c                   in(E)     *dtaara
040500050307     c                   if        %ERROR or RSUT = *blanks
040600050307     c                   clear                   Tibs34Ds
040700050307     c                   call      'TIBS34R'
040800050307     c                   parm                    Tibs34Ds
040900050307     c                   in        *dtaara
041000050307     c                   endif
041100050307      *
041200050307     c                   eval      TBLkut = 1
041300031117      *
041400031117      * Carico tabella "PU" - SOLO PARTNER
041500031117     c                   clear                   XX
041600031117     c                   movel     'PU'          TABcod
041700031117     c     TABcod        setll     EDTAB000
041800031117     c     TABcod        reade     EDTAB000
041900031117do  1c                   DOW       NOT %eof(EDTAB01L)
042000031117if  2c                   if        TABflg = *blanks
042100031117     C                   movel     TABuni        EDIdsPU
042200031117if  3c                   if        §PUtpc = 'P'
042300031117     c                   add       1             xx
042400031117     C                   movel     TABkey        $KeyPT(xx)
042500031117e   3C                   endif
042600031117e   2C                   endif
042700031117     C     TABcod        reade     EDTAB000
042800031117e   1C                   ENDDO
042900031117      *
043000031117      * Carico tabella "PT"
043100031117     c                   movel     'PT'          TABcod
043200031117do  1c                   DO        xx            yy
043300031117     c                   movel     $KeyPT(yy)    TABkey
043400031117     c     kEDTAB        chain     EDTAB000
043500031117if  2c                   if            %found(EDTAB01L)
043600031117     c                             and TABflg = *blanks
043700031117     c                   movel     TABuni        EDIdsPT
043800031117     c                   z-add     §PTksc        $Ksc(yy)
043900031117     c                   z-add     §PTlnp        $Lnp(yy)
044000031117e   2c                   endif
044100031117e   1c                   ENDDO
044200130508
044300130508      /free
044400130508       //?Per ogni partner caricato aggangio anche la tabella "PO"
044500130508       //?mi serve per controllare se invio automativo al partner
044600130520         zz = 1;
044700130520         FOR zz by 1 to %elem($keyPT);
044800130521           IF  $keyPT(zz) <> *blanks;
044900130508             clear EDIdsPO;
045000130508             TABcod = 'PO';
045100130520             TABkey = $keyPT(zz);
045200130508             chain (TABcod:TABkey) EDTAB01L;
045300130508             IF  %found(EDTAB01L);
045400130508               EDIdsPO = TABuni;
045500130508             ENDIF;
045600130522             skPGMTR(zz)    = §POpgmtrso;
045700130522             skSIelenco(zz) = §POsndprtf;
045800130508           ENDIF;
045900130508         ENDFOR;
046000130508      /end-free
046100081209      *
046200110427      * carico tabella 3I/DPD
046300081209     c                   clear                   ds3Idp
046400081209     c                   movel     '3I'          TBLcod
046500081209     c                   movel(p)  'DPD'         TBLkey
046600081209     c     ktabel        chain     TABEL
046700081209     c                   if        %found(TABEL00F) and TBLflg = *blanks
046800081209     c                   eval      ds3Idp = TBLuni
046900060921     c                   endif
047000040218      *
047100040218      * Cerco il nome della coda del fax
047200040218     c                   clear                   PRMFine
047300050307     c                   eval      PRMFil    = DUTpou
047400040218     c                   eval      PRMTpOutq = '2'
047500040218     c                   clear                   PRMOutQ
047600040218     c                   clear                   PRMOutQLib
047700040218     c                   clear                   PRMEsito
047800040218     c                   call      'TRULOUTQ'
047900040218     c                   parm                    PRMFine
048000040218     c                   parm                    PRMFil
048100040218     c                   parm                    PRMTpOutQ
048200040218     c                   parm                    PRMOutQ
048300040218     c                   parm                    PRMOutQLib
048400040218     c                   parm                    PRMEsito
048500040218if  1c                   if            PRMEsito <> *off
048600040218     c                             or  PRMOutQ  =  *blanks
048700040218     c                   movel(p)  '*JOB'        PRMoutq
048800040218e   1c                   endif
048900031117      *
049000031117      * Inverto la *Date
049100031117     c                   time                    w0140
049200031117     c                   clear                   WLBdat
049300031117     c                   move      w0140         G02dat
049400031117     c                   call      'XSRDA8'
049500031117     c                   parm                    WLBdat
049600031117     c                   z-add     G02inv        DateU
049700031117      *
049800031117      * Azzeramento dei campi generici/"totali"
049900031117     c*** non serve:     clear                   wPage
050000031117     c*** non serve:     clear                   wBoxP
050100031117     c*** non serve:     clear                   wPosIni_B
050200151126
050300151126      /free
050400151126       //?Carico in sk la tabella ALP - Abbinamento Linea/Partner
050500151126       //?per invio a modo nuovo dei dati al partner
050600151126         clear dALP;
050700151126         clear xx;
050800151126         TBEcod = 'ALP';
050900151126         setll (TBEcod) TNTBE01L;
051000151126         reade (TBEcod) TNTBE01L;
051100151126         DOW  not %eof(TNTBE01L);
051200151211           IF  TBEke2 = *blanks;
051300151211             dALP = TBEuni;
051400151211             xx += 1;
051500151211             skPOR(xx) = %dec(%subst(TBEke1:1:3):3:0);
051600151211             skKSU(xx) = §NASksu;
051700151211             skFIE(xx) = §NASfie;
051800151211           ENDIF;
051900151126           reade (TBEcod) TNTBE01L;
052000151126         ENDDO;
052100151126      /end-free
052200031117      *
052300110427     c                   ENDSR
052400031117      *
052500031117      *---------------------------------------------------------------*
052600110427      *?sr_START   * Operazioni iniziali                             ?*
052700031117      *---------------------------------------------------------------*
052800110427     c     sr_Start      BEGSR
052900031117      *
053000031117      * Carica tutte le filiali gestite dalla filiale di gestione (£6)
053100031117if  1c                   IF        R16nor = *zeros
053200031117     c                   clear                   $L6
053300031117     c                   clear                   TRUL06DS
053400031117     c                   move      '£6'          D06cod
053500031117     c                   movel     'L'           D06tla
053600031117     c                   movel     R16por        D06key
053700031117     c                   movel     TRUL06DS      KPJBU
053800031117     c                   call      'TRUL06R'
053900031117     c                   parm                    KPJBA
054000031117     c                   movel     KPJBU         TRUL06DS
054100031117     c                   movea     Lin           $L6
054200130509e   1c                   ENDIF
054300031117      *
054400031117      * Esegue la override al file di stampa
054500081209if  1c                   if        NOT %open(FIOR18P)
054600031117if  2c                   if        R16mdo = *blanks
054700081211     c                   eval      R16mdo = '*ORMA4    '
054800031117e   2c                   endif
054900090129      /free
055000090129         clear  TrulOvrPds;
055100090930         uopToDo   = 'A';
055200090129         uopPrtF   = 'FIOR18P   ';
055300090129         uopOutQ   = R16oqo;
055400090129         uopFrmTyp = R16mdo;
055500090129         uopSave   = '*NO ';
055600090129         trulOvrPr ( trulOvrPds );
055700090129         Qcmd  = uopCmd;
055800090129      /end-free
055900031117     c                   call      'QCMDEXC'                            90
056000031117     c                   parm                    Qcmd
056100031117     c                   parm                    Qlen
056200081209     c                   OPEN      FIOR18P
056300031117e   1c                   endif
056400031117      *
056500031117      * Apre i file di input a seconda della richiesta
056600081210if  1c                   IF        R16ris = 'R' or R16ris = 'X'
056700031117      * - Se richiesta la ristampa apro il file FNORM06L
056800061006      *   e sollecito
056900031117if  2c                   if        NOT %open(FNORM06L)
057000031117     c                   open      FNORM06L
057100031117e   2c                   endif
057200031117x   1c                   else
057300031117      * - Se richiesta la   stampa apro il file FNORM11L
057400031117if  2c                   if        NOT %open(FNORM11L)
057500031117     c                   open      FNORM11L
057600031117e   2c                   endif
057700031117e   1c                   endif
057800031117      *
057900031117     c                   ENDSR
058000031117      *
058100031117      *---------------------------------------------------------------*
058200081209      *?sr_ORM_1   * Stampa singolo O.R.M.                           ?*
058300031117      *---------------------------------------------------------------*
058400031117     c     sr_ORM_1      BEGSR
058500031117      *
058600031117     c     k04orm01      chain     FNORM000
058700031117      *
058800031117if  1c                   IF        %found(FNORM01L)
058900081209      *
059000090423      * Gestisco allocazione del record con invio del msg all'utente
059100090423      *   che blocca il rcd
059200081209     c     ORMnrr        chain(e)  FNORM00
059300081209if  2c                   if        %error
059400081209     c                   clear                   TRUL82ds
059500081209     c                   eval      UL82§rrn = ORMnrr
059600081209     c                   eval      UL82§fil = 'FNORM00F'
059700081209     c                   eval      UL82§win = 'N'
059800081209     c                   eval      UL82§F7  = 'S'
059900081209     c                   eval      UL82§num = 5
060000081209     c                   eval      UL82§att = 60
060100081209     c                   eval      UL82§mss = msgjob
060200041122      * Effettuo la chiamata al *PGM d utilità
060300081209     c                   call(e)   'TRUL82R'
060400081209     c                   parm                    TRUL82ds
060500081209     c     ORMnrr        chain     FNORM00
060600041122e   2c                   endif
060700031117      *
060800031117     c                   eval      dORM01 = ORMflo
060900090423      * Reperimento tab. OSR per Controllo se serie ORM non stampabile
061000031117     c                   exsr      Tab_OSR
061100090423      * Reperimento tab. FAR per Controllo se fase ORM non stampabile
061200031117     c                   exsr      Tab_FAR
061300031117      *
061400031117if  2c                   IF            d§OSRstp <> 'S'
061500031117     c                             and d§FARstp =  'S'
061600031117      *
061700031117      * Controllo se p.o. ritiro Italia, Estero o DPD
061800031117      *   (no FedEx)
061900031117     c                   exsr      Ctrl_POR
062000031117      *
062100031117      * Pulizia dei PRTF
062200031117     c                   exsr      ClrPRTF
062300031117      *
062400031117      * Stampa
062500031117if  3c                   if        $Estero  = *off
062600031117     c                             and $DPD = *off
062700031117     c                   exsr      StampaORM
062800031117x   3c                   else
062900031117     c                   exsr      StampaORMe
063000031117e   3c                   endif
063100031117      *
063200080326      * Aggiorna dati se NON è RIstampa o sollecito o orm con allegati con distinta su PDA
063300081210if  3c                   if        R16ris  <> 'R' and R16ris <> 'X'
063400080326     c                             and R16ris <> 'A'
063500031117      * - O.R.M.
063600031117     c                   exsr      UpdatFNORM
063700031117      * - Storico Fasi
063800031117     c                   exsr      WriteFNORF
063900031117e   3c                   endif
064000031117      *
064100031117e   2c                   ENDIF
064200031117      *
064300031117e   1c                   ENDIF
064400031117      *
064500031117     c                   ENDSR
064600031117      *
064700031117      *---------------------------------------------------------------*
064800081209      *?sr_ORM_2   * Stampa di TUTTI gli O.R.M. della £6             ?*
064900031117      *---------------------------------------------------------------*
065000031117     c     sr_ORM_2      BEGSR
065100031117      *
065200031117do  1c                   DO        30            xx
065300031117      *
065400031117if  2c                   if        $L6(xx) <= 0
065500031117     c                   leave
065600031117e   2c                   endif
065700151126      /free
065800151126       //?Controllo se devo inviare i dati al partner a modo nuovo
065900151126         exsr NewTraduttore;
066000151126      /end-free
066100151126
066200130509       //?Cerco eventuale traduttore
066300151126     c                   IF        not NewTradPartner
066400130509     c                   exsr      CercaTrad
066500151126     c                   ENDIF
066600031117      *
066700031117      * Posizionamento per P.O. di ritiro
066800031117sel 2c                   select
066900031117w   2c                   when          R16ris =  'R'
067000031117     c                             and R16ndc =  *zeros
067100031117     c     $L6(xx)       setll     FNORM06
067200031117w   2c                   when          R16ris =  'R'
067300031117     c                             and R16ndc >  *zeros
067400031117     c     k02orm06      setll     FNORM06
067500081210w   2c                   when          R16ris <> 'R' and R16ris <> 'X'
067600031117     c                             and R16ndc =  *zeros
067700031117     c     $L6(xx)       setll     FNORM11
067800081210w   2c                   when          R16ris <> 'R' and R16ris <> 'X'
067900031117     c                             and R16ndc >  *zeros
068000031117     c     k02orm06      setll     FNORM11
068100031117e   2c                   endsl
068200031117      *
068300031117      * Ciclo di lettura/elaborazione per P.O. di ritiro
068400031117do  2c                   DO        *hival
068500031117      *
068600031117sel 3c                   select
068700031117w   3c                   when          R16ris =  'R'
068800031117     c                             and R16ndc =  *zeros
068900031117     c     $L6(xx)       reade     FNORM06
069000031117w   3c                   when          R16ris =  'R'
069100031117     c                             and R16ndc >  *zeros
069200031117     c     k02orm06      reade     FNORM06
069300081210w   3c                   when          R16ris <> 'R' and R16ris <> 'X'
069400031117     c                             and R16ndc =  *zeros
069500031117     c     $L6(xx)       reade     FNORM11
069600081210w   3c                   when          R16ris <> 'R' and R16ris <> 'X'
069700031117     c                             and R16ndc >  *zeros
069800031117     c     k02orm06      reade     FNORM11
069900031117e   3c                   endsl
070000031117      *
070100031117if  3c                   if        %eof
070200031117     c                   leave
070300031117e   3c                   endif
070400031117      *
070500031117      *
070600031117     c                   eval      dORM01 = ORMflo
070700031117      * - Controllo se serie ORM non stampabile
070800031117     c                   exsr      Tab_OSR
070900031117      * - Controllo se fase ORM non stampabile
071000031117     c                   exsr      Tab_FAR
071100031117      *
071200031117      * - Annullamento
071300031117if  3c                   if            ORMatb   <> *blanks
071400031117      *   Serie non stampabile
071500031117     c                             or  d§OSRstp =  'S'
071600031117      *   Fase non stampabile
071700031117     c                             or  d§FARstp <> 'S'
071800031117      *   Se tipo elaborazione RISTAMPA  deve essere già stato stampato
071900031117     c                             or (R16ris =  'R'
072000031117     c                             and ORMdst =  *zeros)
072100031117      *   Se tipo elaborazione RISTAMPA  deve essere già stato stampato
072200031117      *   nella data richiesta
072300031117     c                             or (R16ris =  'R'
072400031117     c                             and R16dst <> *zeros
072500031117     c                             and ORMdst <> R16dst)
072600031117      *   Data distinta
072700031117     c                             or (R16ddc <> *zeros
072800031117     c                             and ORMddc <> R16ddc)
072900031117     c                   iter
073000031117e   3c                   endif
073100031117      *
073200031117      * - Data ritiro
073300081210if  3c                   IF            R16ris <> 'R' and R16ris <> 'X'
073400031117     c                             and R16dar <> *zeros
073500031117      *   dal 19/09/2001 devo stampare anche gli ORM con data ritiro
073600031117      *   inferiore alla data richiesta, se non sono mai stati stampati
073700031117      *   da considerare solo in STAMPA
073800031117if  4c                   if            ORMdar   >  R16dar
073900031117      * - - Solo Già Assegnati
074000031117     c                             or (R16ass =  'S'
074100031117     c                             and ORMfao <> 400)
074200031117     c                   iter
074300031117e   4c                   endif
074400031117e   3c                   ENDIF
074500031117      *
074600031117      * > Controllo se p.o. ritiro Italia, Estero o DPD
074700031117      *   (no FedEx)
074800031117     c                   exsr      Ctrl_POR
074900031117      *
075000031117      * > Pulizia del PRTF
075100031117     c                   exsr      ClrPRTF
075200151126
075300151126      /free
075400151126       //?Se ORM Estero e Nuovo traduttore attivo devo preparare il file
075500151126       //?richiamando il nuovo programma con la funzione EXEC
075600151126       //?solo se siamo in STAMPA
075700151126         IF  $Estero and NewTradPartner and R16ris = 'S';
075800151126           pin_Opz = 'WRITE';
075900151126           pin_KSU = skKSU(zz);
076000151126           clear pOut_Esito;
076100151126           tis7tbor (pin_Opz:pin_KSU:FNORMDS:pOut_Esito);
076200151126         //?Per errore leggo altro ORM
076300151126           IF  pOut_Esito <> '1';
076400151126             iter;
076500151126           ENDIF;
076600151126         ENDIF;
076700151126      /end-free
076800130509
076900130513       //?Se ORM Estero (no DPD) e traduttore attivo devo preparare il file
077000130509       //?richiamando il traduttore con la funzione EXEC
077100130509       //?solo se siamo in STAMPA
077200151126       //?e non ho il nuovo traduttore attivo
077300130509     c                   IF        $Estero = *on and wOKtraduttore and
077400130509     c                             R16ris = 'S'
077500151126     c                             and not NewTradPartner
077600130520     c                   eval      ORMfunzion = 'EXEC'
077700130520     c                   eval      ORMormpoe = ORMpoe
077800130520     c                   eval      ORMormnsr = ORMnsr
077900130520     c                   eval      ORMormnor = ORMnor
078000130520     c                   eval      ORMormnrv = ORMnrv
078100130520     c                   eval      kpjbu = TRTCTORMds
078200130509     c                   call      §prtradex
078300130509     c                   parm                    KPJBA
078400130509       //?se torna esito 1 qualcosa è andato storto nel traduttore
078500130509       //?leggo altro ORM
078600130520     c                   IF        ORMesito = '1'
078700130510     c                   eval      R16ass = 'E'
078800130509     c                   iter
078900130509     c                   ENDIF
079000130509     c                   ENDIF
079100031117      *
079200031117      * > Stampa
079300031117     c                   eval      R16poe = ORMpoe
079400031117     c                   eval      R16nsr = ORMnsr
079500031117     c                   eval      R16nor = ORMnor
079600031117     c                   eval      R16nrv = ORMnrv
079700031117if  3c                   if            $Estero = *off
079800031117     c                             and $DPD    = *off
079900031117     c                   exsr      StampaORM
080000040218x   3c                   else
080100031117     c                   exsr      StampaORMe
080200031117e   3c                   endif
080300031117      *
080400031117      * > Aggiornamento dati se NON è ristampa
080500081210if  3c                   IF        R16ris <> 'R' and R16ris <> 'X'
080600031117      *
080700031117     c     k04orm01      chain     FNORM000
080800031117if  4c                   if            %found(FNORM01L)
080900031117     c                             and ORMdst = *zeros
081000081209      *
081100041122      * Gestisco allocazione del record con invio del msg all'utente che blocca il rcd
081200081209     c     ormnrr        chain(e)  FNORM00
081300081209if  5c                   if        %Error
081400081209     c                   clear                   TRUL82ds
081500081209     c                   eval      UL82§rrn = ORMnrr
081600081209     c                   eval      UL82§FIL = 'FNORM00F'
081700081209     c                   eval      UL82§WIN = 'N'
081800081209     c                   eval      UL82§F7  = 'S'
081900081209     c                   eval      UL82§num = 5
082000081209     c                   eval      UL82§att = 60
082100081209     c                   eval      UL82§mss = MsgJob
082200041122      * Effettuo la chiamata al *PGM d utilità
082300081209     c                   call(e)   'TRUL82R'
082400081209     c                   parm                    TRUL82ds
082500081209     c     ORMnrr        chain     FNORM00
082600041122e   5c                   endif
082700031117      * - O.R.M.
082800031117     c                   exsr      UpdatFNORM
082900031117      * - Storico Fasi
083000031117     c                   exsr      WriteFNORF
083100031117e   4c                   endif
083200031117      *
083300031117e   3c                   ENDIF
083400031117      *
083500031117e   2c                   ENDDO
083600151126      /free
083700151126       //?Per Nuovo Traduttore devo richiamare il programma con CLOSE
083800151126         IF  NewTradPartner;
083900151126           pin_Opz = 'CLOSE';
084000151126           reset pin_KSU;
084100151126           clear pOut_Esito;
084200151126           tis7tbor (pin_Opz:pin_KSU:FNORMDS:pOut_Esito);
084300151126         ENDIF;
084400151126      /end-free
084500151126
084600130509       //?Richiamo il traduttore con la funzione CLOSE
084700130509     c                   IF        wOKtraduttore
084800130520     c                   eval      ORMfunzion = 'CLOSE'
084900130520     c                   clear                   ORMormpoe
085000130520     c                   clear                   ORMormnsr
085100130520     c                   clear                   ORMormnor
085200130520     c                   clear                   ORMormnrv
085300130520     c                   eval      kpjbu = TRTCTORMds
085400130509     c                   call      §prtradex
085500130509     c                   parm                    KPJBA
085600130509     c                   ENDIF
085700031117      *
085800031117e   1c                   ENDDO
085900031117      *
086000031117     c                   ENDSR
086100130509
086200130509      /free
086300151126       //-----------------------------------------------------------------
086400151126       //?Verifico se la filiale ritiro è un Partner con nuovo traduttore.
086500151126       //------------------------------------------------------------------
086600151126       BEGSR NewTraduttore;
086700151126
086800151126         NewTradPartner = *off;
086900151201         wSIelenco = *off;
087000151126         clear zz;
087100151126         clear yy;
087200151201
087300151201       //?Se filiale ritiro è nella tabella partner
087400151201         IF  %lookup($L6(xx):$lnp) = 0;
087500151201           leavesr;
087600151201         ENDIF;
087700151126
087800151126       //?Se la filiale ritiro è presente in tabella ALP
087900151126         zz =  %lookup($L6(xx):skPOR);
088000151126         IF  zz = 0;
088100151126           leavesr;
088200151126         ENDIF;
088300151126         NewTradPartner = *on;
088400151201
088500151201       //?imposto se devo mandare lo stesso MAIL/FAX oltre al FILE
088600151201         wSIelenco = (skFIE(zz) = 'S');
088700151126
088800151126       //?Se nuovo invio richiamo il pgm con la funzione OPEN
088900151126         pin_Opz = 'OPEN';
089000151126         reset pin_KSU;
089100151126         clear FNORMDS;
089200151126         clear pOut_Esito;
089300151126         tis7tbor (pin_Opz:pin_KSU:FNORMDS:pOut_Esito);
089400151126         IF  pOut_Esito <> '1';
089500151126           NewTradPartner = *off;
089600151126           wSIelenco = *off;
089700151126         ENDIF;
089800151126
089900151126       ENDSR;
090000130509       //--------------------------------------------------------------
090100130509       //?Cerco eventuale traduttore previsto per la filiale ritiro
090200130509       //--------------------------------------------------------------
090300130509       BEGSR CercaTrad;
090400130509
090500130509         wOKtraduttore = *off;
090600130522         wSIelenco     = *off;
090700130509
090800130509       //?Se filiale ritiro è partner
090900130509         clear yy;
091000130509         yy = %lookup($L6(xx):$lnp);
091100130509         IF  yy > 0 and skPGMTR(yy) <> *blanks;
091200130522       //?imposto se devo mandare lo stesso MAIL/FAX oltre al FILE
091300130522           wSIelenco = (skSIelenco(yy) = 'S');
091400130509       //?aggangio la tabella PR in caso di invio automatico
091500130509           clear EDIdsPR;
091600130509           TABcod = 'PR';
091700130509           TABkey = skPGMTR(yy);
091800130509           chain (TABcod:TABkey) EDTAB01L;
091900130509           IF  %found(EDTAB01L);
092000130509             EDIdsPR = TABuni;
092100130509           ENDIF;
092200130509           IF  §PRtradex <> *blanks;
092300130509             wOKtraduttore = *on;
092400130509         //?devo richiamare il pgm con la funzione OPEN
092500130520             clear TRTCTORMds;
092600130520             ORMcliente = $ksc(yy);
092700130520             ORMlinea   = $lnp(yy);
092800130520             ORMfunzion = 'OPEN';
092900130520             kpjbu = TRTCTORMds;
093000130509      /end-free
093100130509     c                   call      §prtradex
093200130509     c                   parm                    KPJBA
093300130509      /free
093400130509           ENDIF;
093500130509         ENDIF;
093600130509
093700130509       ENDSR;
093800130509      /end-free
093900031117      *
094000031117      *---------------------------------------------------------------*
094100081209      *?Tab_OSR    * Reperimento dati tab. OSR                       ?*
094200031117      *---------------------------------------------------------------*
094300031117     c     Tab_OSR       BEGSR
094400031117      *
094500031117      * Caricamento impedimenti relativi alla serie ORM:
094600031117     c                   reset                   dOSR
094700031117      *
094800031117      *  ORM senza serie: risulta sempre stampabile
094900130118     c                   IF        ORMnsr = *zeros
095000130118     c                   leavesr
095100130118     c                   ENDIF
095200130118
095300031117      *  ORM  con  serie: controllo l'impedimento da tab.OSR
095400110427      * - Se flag R16ris = 'C' => forzo che ORM stampabile
095500110427if  2c                   If        R16ris = 'C'
095600100927     c                   clear                   d§OSRstp
095700130118     c                   leavesr
095800130118     c                   ENDIF
095900130118
096000130118      /free
096100130118       //?Controllo se il cliente è in tabella OSR
096200130118         wORMcor = %dec(%subst(%editc(ORMcor:'X'):1:7):7:0);
096300130118       //?leggo tutta la tabella OSR
096400130118         setll c_tabosr tntbe01l;
096500130118         reade c_tabosr tntbe01l;
096600130118
096700130118         DOW not %eof(tntbe01l);
096800130122           IF  TBEatb = *blanks;
096900130118           dosr = tbeuni;
097000130118           IF d§osrcli  = wORMcor or d§OSRcl2  = wORMcor or
097100130118              d§osrcl3  = wORMcor or d§OSRcl4  = wORMcor or
097200130118              d§osrcl5  = wORMcor or d§OSRcl6  = wORMcor or
097300130118              d§osrcl7  = wORMcor or d§OSRcl8  = wORMcor or
097400130118              d§osrcl9  = wORMcor or d§OSRcl10 = wORMcor or
097500130118              d§osrcl11 = wORMcor or d§OSRcl12 = wORMcor or
097600130118              d§osrcl13 = wORMcor or d§OSRcl14 = wORMcor or
097700130118              d§osrcl15 = wORMcor or d§OSRcl16 = wORMcor;
097800130118             leavesr;
097900130118           ENDIF;
098000130122           ENDIF;
098100130121           reade c_tabosr tntbe01l;
098200130118         ENDDO;
098300130118
098400130118       //?Se arrivo qua vuol dire che ho la serie e che non ho trovato il cliente
098500130118       //?in tabella quindi imposto il flag a NON stampabile
098600130118         d§OSRstp = 'S';
098700130118      /end-free
098800031117      *
098900031117     c                   ENDSR
099000031117      *
099100031117      *---------------------------------------------------------------*
099200081209      *?Tab_FAR    * Reperimento dati tab. FAR                       ?*
099300031117      *---------------------------------------------------------------*
099400031117     c     Tab_FAR       BEGSR
099500031117      *
099600031117      * Caricamento impedimenti relativi alla fase ORM:
099700031117     c                   reset                   dFAR
099800031117      *
099900031117     c                   clear                   TIBS02DS
100000031117     c                   movel     'C'           T02mod
100100031117     c                   movel     KNSIF         T02sif
100200031117     c                   movel     'FAR'         T02cod
100300031117     c                   movel(p)  ORMfao        T02ke1
100400031117     c                   call      'TIBS02R'
100500031117     c                   parm                    KPJBA
100600031117     c                   parm                    TIBS02DS
100700031117if  1c                   If        T02err   = *blanks
100800031117     c                   movel     T02uni        dFAR
100900031117e   1c                   EndIf
101000081209      *
101100031117     c                   ENDSR
101200031117      *
101300031117      *---------------------------------------------------------------*
101400081209      *?Ctrl_POR   * Verifica se P.O. ritiro Italia, Estero o DPD    ?*
101500081209      *?             ed esecuzione delle opporttune override         ?*
101600031117      *---------------------------------------------------------------*
101700031117     c     Ctrl_POR      BEGSR
101800031117      *
101900031117     c                   reset                   $Estero
102000031117     c                   reset                   $DPD
102100031117      *
102200031117if  1c                   IF        ORMpor <> ORGfil
102300071106     c                   clear                   Og143
102400071106     c                   clear                   Og147
102500031117     c                   clear                   ORGfl1
102600031117     c     ORMpor        chain     AZORG
102700031117if  2c                   if        %found(AZORG01L)
102800071106     c                   movel     ORGde3        Og143
102900071106     c                   movel     ORGde7        Og147
103000031117e   2c                   endif
103100031117e   1c                   ENDIF
103200070926      * data partenza procedura in organigramma
103300071106     c                   clear                   Data_Org
103400071106     c                   if        §OGddao > *zeros
103500071106     c                   move      §OGddao       Data_Org
103600070926     c                   endif
103700031117      *
103800031117      * - $Estero = flag O.R.M. Estero ma NON per FedEx
103900031117if  1c                   if        §OGntw  <> 'FED' and ORGfl1 = 'E'
104000031117     c                   eval      $Estero =  *on
104100031117e   1c                   endif
104200031117      * - $DPD    = flag O.R.M. DPD
104300031117if  1c                   if        §OGntw  =  'DPD'
104400031117     c                   eval      $DPD    =  *on
104500031117e   1c                   endif
104600031117      *
104700031117      *
104800031117if  1c                   IF            $Estero = *on
104900031117     c                             or  $DPD    = *on
105000031117      *
105100031204if  2c                   IF        NOT %open(FIOR16P2)
105200031117      * 1ª override
105300090129      /free
105400090129         clear  TrulOvrPds;
105500090930         uopToDo   = 'A';
105600090129         uopPrtF   = 'FIOR16P2  ';
105700090129         uopOutQ   = R16oqo;
105800090129         uopSave   = '*NO ';
105900090129         trulOvrPr ( trulOvrPds );
106000090129         Qcmd  = uopCmd;
106100090129      /end-free
106200031117     c                   call      'QCMDEXC'                            90
106300031117     c                   parm                    Qcmd
106400031117     c                   parm                    Qlen
106500031117      *
106600031204     c                   open      FIOR16P2
106700031117e   2c                   ENDIF
106800031117      *
106900031117if  2c                   IF        NOT %open(ORMest)
107000031117      * 2ª override
107100090129      /free
107200090129         clear  TrulOvrPds;
107300090930         uopToDo   = 'A';
107400090129         uopPrtF   = 'ORMEST    ';
107500090129         uopOutQ   = PRMoutq;
107600090129         uopSave   = '*YES';
107700090129         trulOvrPr ( trulOvrPds );
107800090129         Qcmd  = uopCmd;
107900090129      /end-free
108000031117     c                   call      'QCMDEXC'                            90
108100031117     c                   parm                    Qcmd
108200031117     c                   parm                    Qlen
108300031117      *
108400031117     c                   open      ORMest
108500031117e   2c                   ENDIF
108600031117      *
108700031117e   1c                   ENDIF
108800031117      *
108900031117     c                   ENDSR
109000031117      *
109100031117      *---------------------------------------------------------------*
109200081209      *?ClrPRTF    * Pulizia dei campi di stampa                     ?*
109300031117      *---------------------------------------------------------------*
109400031117     c     ClrPRTF       BEGSR
109500031117      *
109600120330     c                   clear                   OR18logBRT
109700090423     c                   clear                   OR18box
109800090423     c                   clear                   OR18pos
109900090423     c                   clear                   OR18orm
110000090423     c                   clear                   OR18not
110100090423     c*** non ha campi   clear                   OR18nul
110200031117      *
110300031117     c                   clear                   OR16est
110400031117      *
110500031117     c*** non qui:       reset                   $Estero
110600031117     c*** non qui:       reset                   $DPD
110700031117     c                   reset                   $PrtNote
110800031117      *
110900031117     c                   clear                   $Note
111000031117     c                   clear                   $QUM
111100031117      *
111200031117     c                   movea     *zeros        *in(01)
111300031117      *
111400031117     c                   ENDSR
111500031117      *
111600031117      *---------------------------------------------------------------*
111700081209      *?StampaORM  * Stampa singolo O.R.M.                           ?*
111800031117      *---------------------------------------------------------------*
111900031117     c     StampaORM     BEGSR
112000071106      *
112100071106      * *in60 = Stampa Distinte ORM automatiche "attiva"
112200071106     c                   eval      *in60  =  (DateU >= Data_Org  and
112300071106     c                                        Data_Org > *zeros)
112400050126      *
112500050126      * *in70 = O.R.M. con Rimesso Mittente
112600050126if  1c                   eval      *in70  =  (§ORsrm = 'S')
112700050126      * *in71 = Stampa Ricevuta di ritiro (non adesso: DOPO!)
112800050126     c*** (già così)     eval      *in71  =  *off
112900031117      *
113000031117      * Imposto i campi in stampa:
113100031117      *
113200050429      * - Numero ORM
113300031117     c                   eval      Ppoe   =  ORMpoe
113400031117     c                   eval      Pnsr   =  ORMnsr
113500031117     c                   eval      Pnor   =  ORMnor
113600031117     c                   eval      Pnrv   =  ORMnrv
113700050429if  1c                   if        *in70
113800050429     c                   eval      Ppoe2  =  ORMpoe
113900050429     c                   eval      Pnsr2  =  ORMnsr
114000050429     c                   eval      Pnor2  =  ORMnor
114100050429     c                   eval      Pnrv2  =  ORMnrv
114200050429e   1c                   endif
114300031117      *
114400031117      * - Data emissione ORM
114500031117     c                   clear                   WLBdat
114600031117     c                   move      ORMdao        G02inv
114700031117     c                   movel     '3'           G02err
114800031117     c                   call      'XSRDA8'
114900031117     c                   parm                    WLBdat
115000031117     c                   z-add     G02dat        Pdao
115100031117      *
115200031117      * - Ora emissione ORM ("hh:mm", senza ":ss")
115300031117     c                   movel     ORMoao        Poao
115400071107      *
115500071107      * - Numero distinta consegna
115600071107     c                   eval      Pndc   =  ORMndc
115700031117      *
115800071107      * - Padroncino: codice e nominativo
115900120612     c                   z-add     ORMnpg        DSTnpg
116000031117     c                   z-add     ORMndc        DSTnfv
116100031117     c                   z-add     ORMpor        DSTfgs
116200120612     c     kfidst        chain     fidst000
116300120612if  1c                   if        %found(Fidst01l)
116400031117     c                             and    DSTatb = *blanks
116500071107if  2c                   if        *in60
116600071107     c                   movel     DSTpdr        Ppdr
116700071107e   2c                   endif
116800031117     c                   movel     'A'           APDtip
116900031117     c                   z-add     DSTpdr        APDpdr
117000031117     c     kFIAPD        chain     FIAPD000
117100071107if  2c                   if        %found(FIAPD01L)
117200071107if  3c                   if        *in60
117300071107     c                   eval      Ppdr   =  %editc(DSTpdr : 'Z')
117400071107     c                                    +  ' '
117500071107     c                                    +  %subst(APDrsc : 1 :
117600071107     c                                       %len(Ppdr) - 1 -
117700071107     c                                         %len( %trim(
117800071107     c                                         %editw( DSTpdr :
117900071107     c                                                '0       ' ))))
118000071107x   3c                   else
118100071107     c                   movel     APDrsc        Ppdr
118200071107     c                   eval      Ppdr   =  %subst( APDrsc : 1 :
118300071107     c                                               %len( Ppdr ) - 5 )
118400071107e   3c                   endif
118500071107e   2c                   endif
118600031117e   1c                   endif
118700071107      *
118800071107      * - Zona ritiro
118900071107     c                   if        NOT  *in60
119000071107     c                   eval      Pzor   =  ORMzor
119100071107     c                   endif
119200031117      *
119300031117      * - P.O. ritiro
119400031117     c                   eval      Ppor   =  ORMpor
119500031117     c                   eval      Ppod   =  ORGdes
119600031117      *
119700031117      * - Data ritiro
119800031117     c                   clear                   WLBdat
119900031117     c                   z-add     ORMdar        G02inv
120000031117     c                   movel     '3'           G02err
120100031117     c                   call      'XSRDA8'
120200031117     c                   parm                    WLBdat
120300031117     c                   z-add     G02dat        Pdar
120400031117      *
120500031117      * - Ora ritiro
120600031117     c                   eval      Porr   =  ORMorr
120700071106      * - - Ora ritiro <= 12:30 (stampata in "grassetto")
120800071106     c                   eval      *in61  =  (ORMorr <= 1230)
120900031117      *
121000031117      * - Priorità (lungo 1/0)
121100031117     c                   z-add     ORMsto        Psto
121200050307      *
121300050307      * - O.R.M. commissionato
121400050307if  1c                   if        §ORcom =  'S'
121500050307     c                   eval      Pcom   =  'C'
121600050307e   1c                   endif
121700031117      *
121800031117      * - Mittente
121900031117     c                   movel     ORMrsr        Prsr
122000031117     c                   movel     ORMinr        Pinr
122100031117     c                   movel     ORMcar        Pcar
122200031117     c                   movel     ORMlor        Plor
122300031117     c                   movel     ORMprr        Pprr
122400031117     c                   movel     ORMnar        Pnar
122500031117      *
122600031117      * - Codice ritiro
122700031117     c                   eval      Pcra   =  ORMcra
122800031117      *
122900031117      * - Referente
123000081215if  1c                   if        ORMnsr =  *zeros
123100031117     c                   eval      Pter   =  ORMter
123200081215e   1c                   endif
123300031117     c                   eval      Prer   =  ORMrer
123400081210     c                   eval      *in62  =  (ORMnsr <> *zeros)
123500031117      *
123600031117      * - Riferimento (NO se call center)
123700031117     c                   if        ORMtco <> 'C'
123800031117     c                   movel     ORMrfa        Prfa
123900031117     c                   endif
124000031117      *
124100031117      * - Indicazione "Sponda Idraulica"
124200031117if  1c                   if        ORMspi <> *blanks
124300031117     c                   eval      Pspi   =  'Sponda Idraulica'
124400031117e   1c                   endif
124500031117      *
124600031117      * - Quantità e Unità di Misura
124700031117     c                   exsr      QUM_FnOrm
124800031117     c                   eval      Pqum   =  %trim($Qum(1)) + '   '
124900031117     c                                    +  %trim($Qum(2)) + '   '
125000031128     c                                    +  %trim($Qum(3))
125100031117      *
125200031117      * - Natura della merce
125300031117     c                   movel     ORMnam        Pnam
125400031117      *
125500031117      * - "Porto assegnato" o "Prepagato"
125600031117sel 1c                   select
125700031117w   1c                   when      ORMtor =  'P'
125800031117     c                   eval      Ptor   =  'Prepagato '
125900031117w   1c                   when      §ORpfb =  'A'
126000031117     c                   eval      Ptor   =  'Assegnato '
126100050125e   1c                   endsl
126200031117      *
126300031117      * - Assegnazione al padroncino
126400031117     c                   movel     ORMtap        Ptap
126500031117      *
126600031117      * - Tipo comunicazione ORM
126700031117     c                   movel     ORMtco        Ptco
126800031117      *
126900081210      * - Riferimento spedizione (se ORM prepagato)
127000031117if  1c                   if        ORMtor =  'P'
127100151126     c                   eval      wORMpoe = ORMpoe
127200151126     c                   eval      wORMnsr = ORMnsr
127300151126     c                   eval      wORMnor = ORMnor
127400151126     c                   eval      wORMnrv = ORMnrv
127500060214     c                   movel     BARcodeDS     kAR4n14
127600060214     c     kFiar4        chain     FIAR4000
127700060214if  2c                   if        NOT %found(FIAR404L)
127800060214     c                   clear                   AR4aas
127900060214     c                   clear                   AR4lnp
128000060214     c                   clear                   AR4nrs
128100060214     c                   clear                   AR4nsp
128200031117x   2c                   else
128300081209     c                   eval      Par4   =  'Spedizione: '
128400060214     c                                    +  %editc(AR4aas : 'Z')
128500081209     c                                    +  ' '
128600081209     c                                    +  %triml(%editw(AR4lnp : '0   '))
128700031117     c                                    +  ' '
128800060214     c                                    +  %editc(AR4nrs : 'Z')
128900031117     c                                    +  ' '
129000060214     c                                    +  %editc(AR4nsp : 'Z')
129100081210     c                   exsr      sr_ImpFatt
129200031117e   2c                   endif
129300031117e   1c                   endif
129400031117      *
129500031117      * - Ordinante
129600031117if  1c                   if        ORMrso <> *blanks
129700031117     c                   eval      Pkor   =  'ORDINANTE'
129800031117     c                   movel     ORMrso        Prso
129900031117if  2c                   if        ORMnao =  *blanks
130000031117     c                   eval      Pclo   =  %trim(ORMcao) + ' '
130100031117     c                                    +  %trim(ORMloo) + ' '
130200031117     c***                                 +  %trim(ORMpro)
130300031117     c                   move      '   '         Pclo
130400031117     c                   move      ORMpro        Pclo
130500031117x   2c                   else
130600031117     c                   eval      Pclo   =  %trim(ORMcao) + ' '
130700031117     c                                    +  %trim(%subst(ORMloo:1:32))
130800031117     c                                    +  ' ' + %trim(ORMpro)
130900031117     c                                    +  ' ' + %trim(ORMnao)
131000031117e   2c                   endif
131100031117e   1c                   endif
131200031117      *
131300031117      * - Destinatario
131400031117if  1c                   if        ORMrsc <> *blanks
131500031117     c                   eval      Pkde   =  'DESTINATARIO'
131600031117     c                   movel     ORMrsc        Prsc
131700031117if  2c                   if        ORMnac =  *blanks
131800031117     c                   eval      Pclc   =  %trim(ORMcac) + ' '
131900031117     c                                    +  %trim(ORMloc) + ' '
132000031117     c***                                 +  %trim(ORMprc)
132100031117     c                   move      '   '         Pclc
132200031117     c                   move      ORMprc        Pclc
132300031117x   2c                   else
132400031117     c                   eval      Pclc   =  %trim(ORMcac) + ' '
132500031117     c                                    +  %trim(%subst(ORMloc:1:32))
132600031117     c                                    +  ' ' + %trim(ORMprc)
132700031117     c                                    +  ' ' + %trim(ORMnac)
132800031117e   2c                   endif
132900031117e   1c                   endif
133000031117      *
133100031117      * - Note
133200140127if  1c                   if           ORMno1 <> *blanks
133300031117     c                             or ORMno2 <> *blanks
133400031127     c                   movel     ORMno1        Pno1
133500031127     c                   movel     ORMno2        Pno2
133600140127e   1c                   endif
133700031117      * - Note aggiuntive
133800140127     c                   exsr      Sch_NoteAUT
133900031117if  1c                   IF        YY > *zeros
134000031117sel 2c                   SELECT
134100031117      * - - disposizione tra ordinante (Prso) e destinatario (Prsc)
134200031117      *     e note (Pno1/Pno2)
134300031117w   2c                   WHEN          YY    <= 2
134400031117     c                             and Pno1  =  *blanks
134500031117     c                             and Pno2  =  *blanks
134600031117     c                   eval      Pno1   =  $Note(1)
134700031117     c                   eval      Pno2   =  $Note(2)
134800031117     c                   clear                   YY
134900031117w   2c                   WHEN          YY    <= 2
135000031117     c                             and Prso  =  *blanks
135100031117     c                             and Prsc  <> *blanks
135200031117     c                   eval      Pkor   =  'NOTE'
135300031117     c                   eval      Prso   =  $Note(1)
135400031117     c                   eval      Pclo   =  $Note(2)
135500031117     c                   clear                   YY
135600031117w   2c                   WHEN          YY    <= 2
135700031117     c                             and Prso  <> *blanks
135800031117     c                             and Prsc  =  *blanks
135900031117     c                   eval      Pkde   =  'NOTE'
136000031117     c                   eval      Prsc   =  $Note(1)
136100031117     c                   eval      Pclc   =  $Note(2)
136200031117     c                   clear                   YY
136300031117w   2c                   WHEN          YY    <= 4
136400031117     c                             and Prso  =  *blanks
136500031117     c                             and Prsc  =  *blanks
136600031117     c                   eval      Pkor   =  'NOTE'
136700031117     c                   eval      Pkde   =  'NOTE'
136800031117     c                   eval      Prso   =  $Note(1)
136900031117     c                   eval      Pclo   =  $Note(2)
137000031117     c                   eval      Prsc   =  $Note(3)
137100031117     c                   eval      Pclc   =  $Note(4)
137200031117     c                   clear                   YY
137300031117w   2c                   WHEN          YY    <= 4
137400031117     c                             and Prso  =  *blanks
137500031117     c                             and Prsc  <> *blanks
137600031117     c                             and Pno1  =  *blanks
137700031117     c                             and Pno2  =  *blanks
137800031117     c                   eval      Pkor   =  'NOTE'
137900031117     c                   eval      Prso   =  $Note(1)
138000031117     c                   eval      Pclo   =  $Note(2)
138100031117     c                   eval      Pno1   =  $Note(3)
138200031117     c                   eval      Pno2   =  $Note(4)
138300031117     c                   clear                   YY
138400031117w   2c                   WHEN          YY    <= 4
138500031117     c                             and Prso  <> *blanks
138600031117     c                             and Prsc  =  *blanks
138700031117     c                             and Pno1  =  *blanks
138800031117     c                             and Pno2  =  *blanks
138900031117     c                   eval      Pkde   =  'NOTE'
139000031117     c                   eval      Prsc   =  $Note(1)
139100031117     c                   eval      Pclc   =  $Note(2)
139200031117     c                   eval      Pno1   =  $Note(3)
139300031117     c                   eval      Pno2   =  $Note(4)
139400031117     c                   clear                   YY
139500140127w   2c*//                WHEN          YY    >  *zeros
139600140127w   2c*//                          and Pno1  =  *blanks
139700140127     c*//                          and Pno2  =  *blanks
139800140127     c*//                eval      Pno1   =  $Note(1)
139900140127     c*//                eval      Pno2   =  $Note(2)
140000140127     c*//                clear                   YY
140100031117      * - - stampa in nuova pagina (dopo la stampa dell'O.R.M.)
140200031117     c***                OTHER
140300050125     c***                exsr      Prt_Note
140400031117e   2c                   ENDSL
140500031117e   1c                   ENDIF
140600131129      *
140700131129      * -?Orari Apertura?
140800131129     c                   exsr      sr_OrariApert
140900031117      *
141000031117      * - Codice a barre: POE+NSR+NOR+NRV
141100031215     *** *   (chiamo il pgm TRUL28R per ricavarne il check digit)
141200031215     ***c                   reset                   TRUL28ds
141300031215     ***c                   movel     BarCodeDS     I28cod
141400031215     ***c                   call      'TRUL28R1'
141500031215     ***c                   parm                    TRUL28DS
141600031215if  1***c                   if        O28err =  *blanks
141700031215     ***c                   movel     O28cod        Tbcd
141800031215e   1***c                   endif
141900031215     c                   movel     BarCodeDS     Tbcd
142000031117      *
142100031117      * - - Verifico se codice a barre da stampare
142200031117      *     (O28err = *blanks)
142300031117     c                   eval      *in50  =  (Tbcd <> *zeros)
142400031117      *
142500031117      *
142600031117      * Stampo O.R.M.
142700031117      *
142800081211      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
142900031117     c                   exsr      Posiz_Box
143000031117      * - O.R.M.
143100090423     c                   WRITE     OR18orm
143200031117      *
143300031117      *
143400050125      * -Se?O.R.M.?senza?Rimesso Mittente:
143500050125      *   Stampo eventuali Note Aggiuntive in pagina successiva
143600050125if  1c                   if        §ORsrm <> 'S'
143700081211     c                   exsr      Prt_Note
143800050125      * -Altrimenti?:
143900050125      *   Stampo Rimesso Mittente in pagina successiva
144000050125x   1c                   else
144100050125     c                   exsr      Prt_RimMitt
144200050125e   1c                   endif
144300050125      *
144400050125     c                   ENDSR
144500081210      /free
144600081210
144700081210       //--------------------------------------------------------------
144800081210       //?Reperimento Importo Totale Fattura (se O.R.M. PrePagato)     ?
144900081210       //--------------------------------------------------------------
145000081210       BEGSR  sr_ImpFatt;
145100081210
145200081210         // Reperimento importo prepagato
145300081210         chain (AR4aas : AR4lnp : AR4nrs : AR4nsp)  FIAR6000;
145400081210
145500081210         if  %found(FIAR601L);
145600081210           w0112 = AR6ift;
145700131129           Pimp  = 'Tot. Fatt.: '
145800131129                 + %triml( %editw( w0112 : '       . 0 ,  -') )
145900081210                 + AR6div;
146000081210         endif;
146100081210
146200081210       ENDSR;
146300131129
146400131129       //--------------------------------------------------------------
146500131129       //?Reperimento ed impostazione degli Orari di Apertura          ?
146600131129       //--------------------------------------------------------------
146700131129       BEGSR  sr_OrariApert;
146800131129
146900131129         clear  dOREorari;
147000131129         *in61 = *off;
147100131129
147200131129         // -?Recupero dati da estensione FNORE (trc "O ")?
147300131129         k_OREpoe = ORMpoe;
147400131129         k_OREnsr = ORMnsr;
147500131129         k_OREnor = ORMnor;
147600131129         k_OREnrv = ORMnrv;
147700131129         k_OREtrc = 'O ';
147800131129         chain  %kds(keyFNORE01)  FNORE000;
147900131129
148000131129         // -?...NON trovati => uscita dalla subroutine?
148100131129         if  Not  %found(FNORE01L);
148200131129           leavesr;
148300131129         endif;
148400131129
148500131129         // -?...Trovati => verifica degli orari?
148600131129         dOREorari = OREdati;
148700131129
148800131129         Select;
148900131129
149000131129           // -?C'è un solo orario "ALLE"?
149100131129           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
149200131129                 §OREoraPda = *zero  and  §OREoraPa = *zero;
149300131129             POA1 = 'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
149400131129           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
149500131129                 §OREoraPda = *zero  and  §OREoraPa > *zero;
149600131129             POA1 = 'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
149700131129
149800131129           // -?C'è un solo orario completo (per 1°)?
149900131129           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
150000131129                 §OREoraPda = *zero  and  §OREoraPa = *zero;
150100131129             POA1 = %editw( §OREoraMda : ' 0:  ' ) + '  ' +
150200131129                    %editw( §OREoraMa  : ' 0:  ' );
150300131129
150400131129           // -?Ci sono due orari completi?
150500131129           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
150600131129                 §OREoraPda > *zero  and  §OREoraPa > *zero;
150700131129             POA1 = %editw( §OREoraMda : ' 0:  ' ) + '  ' +
150800131129                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
150900131129                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
151000131129                    %editw( §OREoraPa  : ' 0:  ' );
151100131129
151200131129         EndSl;
151300131129
151400131129         *in61 = (POA1 <> *blanks);
151500131129
151600131129       ENDSR;
151700131129
151800081210      /end-free
151900050125      *
152000050125      *---------------------------------------------------------------*
152100081209      *?Stampa delle note aggiuntive in pagina successiva            ?*
152200050125      *---------------------------------------------------------------*
152300050125     c     Prt_Note      BEGSR
152400050125      *
152500050125if  1c                   if        YY     >  *zeros
152600031117     c                   eval      $PrtNote  = *on
152700081211      * - - Posizionamento sul foglio A4 in base al n° pagina in stampa
152800031117     c                   exsr      Posiz_Box
152900031117      * - - intestazione
153000090423     c                   write     OR18nul
153100031117     c                   eval      Pnot   =  '            '
153200031117     c                                    +  '(Seguito)     '
153300031117     c                                    +  'O.R.M. numero '
153400031117     c                                    +   %editw(ORMpoe : '0   ')
153500031117     c                                    +   ' '
153600031117     c                                    +   %editc(ORMnsr : 'Z')
153700031117     c                                    +   ' '
153800031117     c                                    +   %editc(ORMnor : 'Z')
153900031117     c                                    +   ' '
154000031117     c                                    +   %editc(ORMnrv : 'Z')
154100090423     c                   WRITE     OR18not
154200090423     c                   write     OR18nul
154300031117     c                   eval      Pnot   =  '            '
154400031117     c                                    +  'NOTE:'
154500090423     c                   WRITE     OR18not
154600090423     c                   write     OR18nul
154700031117e   1c                   endif
154800040713     c                   eval      xy     =  1
154900040713do  1c                   dow       XY     <= YY
155000040713     c                   eval      Pnot   =  $Note(xy) + ' '
155100040713     c                                    +  $Note(xy+1)
155200040713     c                   add       2             xy
155300090423     c                   WRITE     OR18not
155400031117      * - - Avendo massimo 6 righe di note: posso lasciare una riga
155500031117      *     vuota tra una nota e l'altra
155600031215if  2c                   if        YY     <= 6
155700090423     c                   write     OR18nul
155800031117e   2c                   endif
155900031117e   1c                   enddo
156000031117     c                   reset                   $PrtNote
156100031117      *
156200031117     c                   ENDSR
156300050125      *
156400050125      *---------------------------------------------------------------*
156500081209      *?Stampa del Rimesso Mittente                                  ?*
156600050125      *---------------------------------------------------------------*
156700050125     c     Prt_RimMitt   BEGSR
156800050125      *
156900050126     c*** (già così)     eval      *in70 = *on
157000050126     c                   eval      *in71 = *on
157100050125      *
157200050125      * Tolgo impostazione di stampa del codice a barre
157300050125     c                   eval      *in50 = *off
157400110502      *
157500110502      /free
157600120330
157700120330         // -?Impostazione "BRT" per ritiro della merce?
157800120330         Pbrt  = $Brt;
157900110502
158000110502      /end-free
158100050125      *
158200081211      * Posizionamento sul foglio A4 in base al n° pagina in stampa
158300050125     c                   exsr      Posiz_Box
158400050125      *
158500050125      * - RIMESSO MITTENTE
158600090423     c                   WRITE     OR18orm
158700050125      *
158800050125     c                   ENDSR
158900031117      *
159000031117      *---------------------------------------------------------------*
159100081209      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
159200031117      *---------------------------------------------------------------*
159300031117     c     QUM_FNORM     BEGSR
159400031117      *
159500031117     c*** (già fatto:)   clear                   $Qum
159600031117     c                   clear                   YY
159700031117     c                   eval      w0112    = (ORMblc + ORMmtc + ORMatt)
159800031117      * Colli
159900031117if  1c                   if        ORMncl   > *zeros
160000031117     c                             and YY   < %elem($Qum)
160100031117     c                   add       1             yy
160200031117     c                   eval      $Qum(yy) = %editc(ORMncl : '4')
160300031117     c                                      + ' Cl'
160400031117e   1c                   endif
160500031117      * Peso
160600031117if  1c                   if        ORMpkg   > *zeros
160700031117     c                             and YY   < %elem($Qum)
160800031117     c                   add       1             yy
160900031117     c                   eval      $Qum(yy) = %editc(ORMpkg : '4')
161000031117     c                                      + ' Kg'
161100031117e   1c                   endif
161200031117      * Volume
161300031117      * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
161400031117if  1c                   if        ORMvlm   > *zeros
161500031117     c                             and (YY  < %elem($Qum) - 1
161600031117     c                              or  YY  = %elem($Qum) - 1
161700031117     c                              and w0112 = *zeros)
161800031117     c                   add       1             yy
161900031117     c                   eval      $Qum(yy) = %editc(ORMvlm : '4')
162000031117     c                                      + ' Vl'
162100031117e   1c                   endif
162200031117      * Bancali
162300031117      * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
162400031117      * (in ultimo posto da preferire al VOLUME - come da FIOR09R)
162500031117if  1c                   if        ORMbnc   > *zeros
162600031117     c                             and (YY  < %elem($Qum) - 1
162700031117     c                              or  YY >= %elem($Qum) - 1
162800031117     c                              and w0112 = *zeros)
162900031117if  1c                   if        YY       < %elem($Qum)
163000031117     c                   add       1             yy
163100031117e   1c                   endif
163200031117     c                   eval      $Qum(yy) = %editc(ORMbnc : '4')
163300031117     c                                      + ' Bn'
163400031117e   1c                   endif
163500031117      * Bilici
163600031117if  1c                   if        ORMblc  > *zeros
163700031117     c                             and YY  < %elem($Qum)
163800031117     c                   add       1             yy
163900031117     c                   eval      $Qum(yy) = %editc(ORMblc : '4')
164000031117     c                                      + ' Bl'
164100031117e   1c                   endif
164200031117      * Motrici
164300031117if  1c                   if        ORMmtc  > *zeros
164400031117     c                             and YY  < %elem($Qum)
164500031117     c                   add       1             yy
164600031117     c                   eval      $Qum(yy) = %editc(ORMmtc : '4')
164700031117     c                                      + ' Mt'
164800031117e   1c                   endif
164900031117      * Autotreni
165000031117if  1c                   if        ORMatt  > *zeros
165100031117     c                             and YY  < %elem($Qum)
165200031117     c                   add       1             yy
165300031117     c                   eval      $Qum(yy) = %editc(ORMatt : '4')
165400031117     c                                      + ' Au'
165500031117e   1c                   endif
165600031117      *
165700031117     c                   ENDSR
165800140127      *
165900140127      *---------------------------------------------------------------*
166000140127      *?Sch_NoteAUT * Caricamento delle note da FNORT in schiera     ?*
166100140127      *---------------------------------------------------------------*
166200140127     c     Sch_NoteAUT   BEGSR
166300140127      *
166400140127     c*** (già fatto:)   clear                   $Note
166500140127     c                   clear                   YY
166600140127      *
166700140127     c*//?(già così:)?   eval      kORTgst = 'S'
166800140127     c     k05ort11      setll     FNORT000
166900140127     c     k05ort11      reade     FNORT000
167000140127      *
167100140127do  1c                   DOW       NOT %eof(FNORT11L)
167200140127      *
167300140127      * Solo quelle da stampare (già filtrate dalla v.l.)
167400140127      * - Carica la schiera
167500140127if  2c                   if        %subst( ORTnob : 1 :
167600140127     c                                     %len( $Note(1) ) ) <> *blanks
167700140127     c                   add       1             YY
167800140127     c                   eval      $Note(yy) = %subst( ORTnob : 1 :
167900140127     c                                           %len( $Note(1) ) )
168000140127e   2c                   endif
168100140127if  2c                   if        %subst( ORTnob :
168200140127     c                                     %len($Note(1)) + 1 ) <> *blanks
168300140127     c                   add       1             YY
168400140127     c                   eval      $Note(yy) = %subst( ORTnob :
168500140127     c                                           %len( $Note(1) ) + 1 )
168600140127e   2c                   endif
168700140127if  2c                   if        YY = %elem($Note)
168800140127     c                   leave
168900140127e   2c                   endif
169000140127      *
169100140127     c     k05ort11      reade     FNORT000
169200140127      *
169300140127e   1c                   ENDDO
169400140127      *
169500140127     c                   ENDSR
169600031117      *
169700031117      *---------------------------------------------------------------*
169800081209      *?Posiz_BOX  * Posizionamento del box all'interno della pag.A4 ?*
169900031117      *---------------------------------------------------------------*
170000031117     c     Posiz_BOX     BEGSR
170100031117      *
170200031117     c                   add       1             wPage
170300031117      *
170400081211      * - Calcolo la posizione x box all'interno della pagina "A4"
170500031117     c     wPage         div       $BxP          wBoxP
170600031117     c                   mvr                     wBoxP
170700031117if  1c                   if        wBoxP = *zeros
170800081210      * - - Se 4° ORM nella pagina, ma ORM con Rimesso Mittente:
170900081210      *     devo saltare alla pag. A4 successiva: l'ORM in esame
171000081209      *     dev'essere SEMPRE seguito dal rimesso mittente corrispon-
171100081209      *     dente (senza note aggiuntive).
171200050125sel 2c                   select
171300050125w   2c                   when          §ORsrm <> 'S'
171400050125     c                             or  §ORsrm  = 'S'
171500081210     c                             and *in71   = *on
171600031117     c                   eval      wBoxP = $BxP
171700050125w   2c                   when          §ORsrm  = 'S'
171800081210w   2c                             and *in71   = *off
171900050125     c                   add       1             wPage
172000050125     c                   z-add     1             wBoxP
172100050125e   2c                   endsl
172200031117e   1c                   endif
172300031117      *
172400031117      * - Imposto le posizioni nella pagina in corso di stampa
172500081210     c                   eval      wPosIni_B = (2.835 * (wBoxP - 1))
172600081210      /free
172700081210         if  wBoxP > 2;
172800081210           wPosIni_B += 0.167;
172900081210         endif;
173000110427
173100120330         // -?Simil-Logo BRT?
173200120330         §PlblS  = wPosIni_B + $PS0 - 0.010;
173300120330         §PlblVS = 7.541;
173400120330         exsr  sr_SimilLogoBRT_;
173500120330
173600081210      /end-free
173700031117     c                   eval      §PS1 = wPosIni_B + $PS1
173800031117     c                   eval      §LOA = wPosIni_B + $LOA
173900050125if  1c                   if            §ORsrm <> 'S'
174000050125     c                             or  §ORsrm  = 'S'
174100050126     c                             and *in71   = *off
174200031117     c                   eval      §LOB = wPosIni_B + $LOB
174300031127     c                   eval      §LOC = wPosIni_B + $LOC
174400031127     c                   eval      §LOD = wPosIni_B + $LOD
174500050125x   1c                   else
174600050125     c                   eval      §LOB = wPosIni_B + $LOD
174700050125e   1c                   endif
174800031117     c                   eval      §LO1 = wPosIni_B + $LO1
174900031117     c                   eval      §LO2 = wPosIni_B + $LO2
175000031117     c                   eval      §LO3 = wPosIni_B + $LO3
175100031117     c                   eval      §LO4 = wPosIni_B + $LO4
175200131129if  1c                   if        *in61
175300131129     c                   eval      §LO5 = wPosIni_B + $LOB
175400131129e   1c                   endif
175500050126if  1c                   if        §ORsrm   = 'S'
175600050125     c                   eval      §LO5 = wPosIni_B + $LOB
175700050126if  2c                   if        NOT *in71
175800050126     c                   eval      §LO6 = §LOD - 0,10
175900050126x   2c                   else
176000050125     c                   eval      §LO6 = §LOB - 0,10
176100050126e   2c                   endif
176200050126e   1c                   endif
176300031117      *
176400081209      * - Posizionamento nella pagina A4 in base al n° foglio in stampa
176500031117     c                   eval      *in41 = (wBoxP = 1)
176600031117     c                   eval      *in42 = (wBoxP = 2)
176700081210     c                   eval      *in43 = (wBoxP = 3)
176800081210     c                   eval      *in44 = (wBoxP = 4)
176900090423     c                   WRITE     OR18pos
177000031117      * - box / barcode
177100031117if  1c                   if        $PrtNote = *off
177200090423     c                   WRITE     OR18box
177300031117e   1c                   endif
177400120330      * - logo BRT
177500120330     c                   WRITE     OR18logBRT
177600031117      *
177700031117     c                   ENDSR
177800120330
177900120330       //--------------------------------------------------------------
178000120330       //?Posizionamento del simil-logo BRT (unico)                    ?
178100120330       //--------------------------------------------------------------
178200120330      /copy gaitrasrc/srcTNSY,LogoBRT_R2
178300110427
178400031117      *---------------------------------------------------------------*
178500081209      *?StampaOrmE * Stampa singolo O.R.M. ESTERO                    ?*
178600031117      *---------------------------------------------------------------*
178700031117     c     StampaORMe    BEGSR
178800031117      *
178900031117     c                   clear                   OR16est
179000060123     c                   eval      *in10 = *off
179100061003     c                   eval      *in11 = *off
179200081209      *
179300061006      * controllo se sollecito
179400081209     c                   eval      *in06 = (R16ris = 'X')
179500031117      *
179600031117      * Network
179700031117     c                   movea     '00'          *in(04)
179800031117sel 1c                   select
179900031117w   1c                   when      $DPD    = *on
180000031117     c                   movea     '01'          *in(04)
180100031117w   1c                   when      $Estero = *on
180200031117     c                   movea     '10'          *in(04)
180300031117e   1c                   endsl
180400031117      *
180500031117     c                   reset                   DS_ksc
180600031117      * Cerco da cappario DPD
180700081209      * quello nuovo (ora il depot è lungo 4 e non più 3)
180800081209if  1c                   IF        $DPD = *on
180900081209     c                   clear                   TISIE3ds
181000081209     c                   eval      Isie3dri = DateU
181100081209     c                   eval      Isie3dsp = ORMdar
181200060921     c                   time                    isie3hsp
181300081209     c                   eval      Isie3nzd = ORMnar
181400081209     c                   eval      Isie3cad = ORMcar
181500081209if  2c                   if        ORMpkg   > §3ilmp
181600081209     c                   eval      Isie3srv = 101
181700081209x   2c                   else
181800081209     c                   eval      Isie3srv = 136
181900081209e   2c                   endif
182000081209     c                   eval      Isie3lnp = ORMpoe
182100060921     c                   call      'TISIE3R'
182200081209     c                   parm                    TISIE3ds
182300081209if  2c                   if        Osie3err = *blanks
182400081209     c                   movel     Osie3ddep     dsKsc_e3ddep
182500081209     c                   move      Osie3ddep     Pdep
182600060921e   2c                   endif
182700031117      * - Cerco il P.O. di ritiro dalla tabella delle nazioni
182800031117     c                   clear                   ds15
182900031117     c                   movel     '15'          TBLcod
183000031117     c                   movel(p)  ORMnar        TBLkey
183100031117     c     kTABEL        chain     TABEL
183200031117if  2c                   if        %found(TABEL00F)
183300031117     c                   movel     TBLuni        ds15
183400031117e   2c                   endif
183500081209     c                   z-add     §15lad        dsKsc_POR
183600081209      * - Con il depot richiamo pgm per sapere se depot automatizzato
183700081209      *     o meno e mi torna il codice cliente
183800081209     c                   clear                   TRULDEPds
183900081209     c                   eval      Idepdpc = Osie3ddep
184000081209     c                   eval      Idepdrf = ORMdar
184100061003     c                   call      'TRULDEPR'
184200081209     c                   parm                    TRULDEPds
184300081209     c                   if        Odeperr = *blanks
184400081209     c                   move      Odepksc       ds_ksc
184500061003     c                   endif
184600081209     c                   eval      *in11 = (Odeporma = 'S')
184700081209e   1c                   ENDIF
184800031117      *
184900031117      * Cerco dati da tabella Partner
185000031117if  1c                   if        $Estero = *on
185100040713     c                   z-add     1             xy
185200040713     c     ORMpor        lookup    $LNP(xy)                               21
185300031117if  2c                   if        *in21   = *on
185400040713     c                   move      $ksc(xy)      DS_ksc
185500031117e   2c                   endif
185600031117e   1c                   endif
185700081209      *
185800081209     c                   clear                   wMail
185900081209      *
186000081209      * Se depot automatico
186100081209      *   devo scrivere il file e basta se non è ristampa
186200081209      *                                  e non è sollecito
186300081209if  1c                   if             *in11
186400081209     c                             and  R16ris <> 'R'
186500081209     c                             and  R16ris <> 'X'
186600081209     c                   clear                   FIEU49ds
186700081209     c                   eval      Id49poe = ORMpoe
186800081209     c                   eval      Id49nrs = ORMnsr
186900081209     c                   eval      Id49nor = ORMnor
187000081209     c                   eval      Id49nrv = ORMnrv
187100081209     c                   eval      Id49dao = ORMdao
187200061005     c                   call      'FIEU49R'
187300081209     c                   parm                    FIEU49ds
187400081209     c                   if        Od49esito = '0'
187500061005     c                   leavesr
187600061005     c                   endif
187700081209e   1c                   endif
187800081209      *
187900031117      * Controllo se c'è un luogo di spedizione per il fax
188000081209if  1c                   IF        ds_ksc <> *zeros
188100081209     c                   move      ds_ksc        kSPEcli
188200031117     c     kFNSPE        chain     FNSPE000
188300081209if  2c                   If            %found(FNSPE01L)
188400031117     c                             and SPEflg =  *blanks
188500031117     c                   movel     SPErag        Pdepd
188600031117     c                   movel     SPEfax        Pfax
188700081209      * - controllo se oltre al fax c'è anche la relativa e-mail
188800060123     c     kFNSP2        chain     FNSP2000
188900081209if  3c                   if            %found(FNSP201L)
189000060123     c                             and SP2flg =  *blanks
189100081209     c                             and SP2est <> *blank
189200081209     c                   eval      wMail = SP2est
189300081209e   3c                   endif
189400081209x   2c                   Else
189500081209      * - non ho trovato il luogo cerco se c'è la mail sulla nota 83
189600081209     c                   movel(p)  DUTkci        kntcnk1
189700081209     c                   move      ds_ksc        kntcnk1
189800081209     c     kTfntc        chain     TFNTC
189900081209if  3c                   if        %found(TFNTC01L)
190000081209     c                   eval      wMail = NTCrnt
190100081209e   3c                   endif
190200031117      *   Ragione sociale e fax
190300031117     c                   clear                   TIBS69DS
190400031117     c                   move      DS_ksc        I69kac
190500031117     c                   move      DS_ksc        I69kin
190600031117     c                   call      'TIBS69R'
190700081209     c                   parm                    TIBS69ds
190800081209     c                   parm                    ds_CNACO
190900081209     c                   parm                    ds_CNIND
191000081209     c                   parm                    ds_CNCLP
191100081209     c                   parm                    ds_FNCLS
191200081209if  3c                   if        O69err <> *on
191300031117     c                   movel     ACOrag        Pdepd
191400031117     c                   movel     INDtlf        Pfax
191500081209e   3c                   endif
191600081209e   2c                   EndIf
191700081209e   1c                   ENDIF
191800031117      *
191900031117      * Dati del ritirante (mittente)
192000031117     c                   eval      Prsr  =  ORMrsr
192100031117     c                   eval      Pinr  =  ORMinr
192200031117     c                   eval      Pcar  =  ORMcar
192300031117     c                   eval      Plpr  =  %trim(ORMlor) + '  '
192400031117     c                                   +  %trim(ORMprr)
192500031117     c                                   +  %trim(ORMnar)
192600031117     c                   movel     ORMrer        Prer2
192700031117     c                   movel     ORMter        Pter
192800031117      *
192900031117      * Numero colli e peso
193000031117     c                   eval      Pncl  =  ORMncl
193100031117     c                   eval      Ppkg  =  ORMpkg
193200121120      * Bancali
193300121120     c                   eval      Pbnc  =  ORMbnc
193400120925      *
193500120925      * - Natura della merce
193600120925     c                   movel     ORMnam        Pnam
193700160413      * - Note
193800160413     c                   movel     ORMno1        Pno1
193900160413     c                   movel     ORMno2        Pno2
194000031117      *
194100031117      * Dati del consegnante (destinatario)
194200031117     c                   eval      Prsc  =  ORMrsc
194300031117     c                   eval      Pinc  =  ORMinc
194400031117     c                   eval      Pcac  =  ORMcac
194500031117     c                   eval      Plpc  =  %trim(ORMloc) + '  '
194600031117     c                                   +  %trim(ORMprc)
194700031117     c                                   +  %trim(ORMnac)
194800031117      *
194900031117      * Inversione data ritiro
195000031117     c                   clear                   WLBdat
195100031117     c                   z-add     ORMdar        G02inv
195200031117     c                   movel     '3'           G02err
195300031117     c                   call      'XSRDA8'
195400031117     c                   parm                    WLBdat
195500031117     c                   z-add     G02dat        Pdar
195600031117      *
195700031117      * Numero ORM
195800031117     c                   eval      Porm  =  %trim(
195900031117     c                                        %editw(ORMpoe:'0   '))
196000031117     c                                   +  '-'
196100031117     c                                   +  %trim(
196200031117     c                                        %editc(ORMnsr:'Z'))
196300031117     c                                   +  %trim(
196400031117     c                                        %editc(ORMnor:'Z'))
196500031117     c                                   +  '-'
196600031117     c                                   +  %trim(
196700031117     c                                        %editw(ORMnrv:'0  '))
196800160415     c                   eval      pormrmd = porm
196900031117      *
197000031117      * Depot
197100031117     c                   clear                   dOES
197200031117if  1c                   if        $Estero = *on
197300031117     c                             or $DPD = *on
197400031117     c                   clear                   TIBS02DS
197500031117     c                   movel     'C'           T02mod
197600031117     c                   movel     KNSIF         T02sif
197700031117     c                   movel     'OES'         T02cod
197800031117sel 2c                   select
197900031117w   2c                   when      $DPD    = *on
198000060502     c                   movel(p)  §OGdp1        T02ke1
198100031117w   2c                   when      $Estero = *on
198200081209     c                   move      ORMpor        w0040
198300060503     c                   movel(p)  w0040         T02ke1
198400031117e   2c                   endsl
198500031117     c                   call      'TIBS02R'
198600031117     c                   parm                    KPJBA
198700031117     c                   parm                    TIBS02DS
198800031117     c                   movel     T02uni        dOES
198900031117e   1c                   endif
199000031117     c                   movel     d§OESdes      Poesd
199100160413     c*****              movel     d§OESfax      Poesf
199200160413     c*****              movel     d§OESnom      Poesn
199300031117      *
199400031117      * Data odierna
199500031117     c                   z-add     w0140         Pdat
199600130521
199700130521      * Imposto in stampa il tipo invio al partner
199800130521     c                   clear                   wfcs
199900130521     c                   select
200000130521     c                   when      R16ris = 'R' or R16ris = 'X'
200100130521     c                   eval      wfcs = ORMfcs                                da ORM
200200130521     c                   when      *in11 or wOKtraduttore
200300151126     c                             or NewTradPartner
200400130521     c                   eval      wfcs = 'F'                                   File
200500130521     c                   when      wmail <> *blanks
200600130521     c                   eval      wfcs = 'M'                                   Mail
200700130521     c                   other
200800130521     c                   eval      wfcs = 'M'                                   Mail
200900130521     c                   endsl
201000130521     c                   clear                   pfcs
201100130521     c                   clear                   TIBS02DS
201200130521     c                   movel     'C'           T02mod
201300130521     c                   movel     KNSIF         T02sif
201400130521     c                   movel     'MIP'         T02cod
201500130521     c                   movel(p)  wfcs          T02ke1
201600130521     c                   call      'TIBS02R'
201700130521     c                   parm                    KPJBA
201800130521     c                   parm                    TIBS02DS
201900130521if  1c                   If        T02err   = *blanks
202000130521     c                   movel     T02uni        pfcs
202100130521e   1c                   EndIf
202200160415      *
202300160415      * SE ORM Export DPD recupero il numero ORM che mandiamo a DPD
202400160415      * ma solo se sollecito per poi impostarlo nell'oggetto della mail
202500160415     c                   clear                   Pormdpd
202600160415     c                   if        $dpd = *on and *in06
202700160415     c     kfidpo        chain     FIDPO000
202800160415     c                   if        %found(FIDPO01L)
202900160415     c                   eval      Pormdpd = DPOdep + '/' +
203000160415     c                             %editc(DPOord:'X')
203100160415     c                   endif
203200160415     c                   endif
203300160413      /free
203400160413       //?Richiamo in questo punto il TRUL44R
203500160413       //?in modo da avere l'indirizzo mail del mittente da stampare
203600160413         exsr CallTrul44;
203700160413         pmail = d44mit;
203800160413      /end-free
203900031117      *
204000031117      *
204100031117      * Stampa "Estero"
204200031117     c                   eval      *in20   = *on
204300031117     c                   write     OR16est
204400130521
204500130605      * se è stampa ORM e NON sollecito
204600130605     c                   IF        not *in06
204700130521      * SE ORM INVIATO AL PARTENER PER FILE NON VADO AVANTI CON FAX/MAIL
204800130521     c                   IF        wOKtraduttore or ORMfcs = 'F'
204900151126     c                             or NewTradPartner
205000130521     c                   eval      *in11 = *on
205100130522      * a meno che non è richiesto da tabella di inviare anche mail/fax
205200130522     c                   IF        not wSIelenco
205300130521     c                   leavesr
205400130522     c                   ENDIF
205500130521     c                   ENDIF
205600130605     c                   ENDIF
205700130521
205800060123      * se c'è la mail devo inviare l'orm estero via mail
205900060123if  1c                   if        wmail <> *Blanks
206000060123     c                   eval      *in10 = *on
206100060123      * chiudo il printer file già aperto
206200060123if  2c                   if        %open(ORMest)
206300060123     c                   close     ORMest
206400090129      /free
206500090129         clear  TrulOvrPds;
206600090930         uopToDo   = 'B';
206700090129         uopPrtF   = 'ORMEST    ';
206800090129         trulOvrPr ( trulOvrPds );
206900090129         Qcmd  = uopCmd;
207000090129      /end-free
207100060123     c                   call      'QCMDEXC'                            90
207200060123     c                   parm                    Qcmd
207300060123     c                   parm                    Qlen
207400060123e   2c                   endif
207500081209      *
207600160413    2c                   IF        wOkTrul44
207700060123if  3c                   IF        NOT %open(ORMest)
207800090129      /free
207900090129         clear  TrulOvrPds;
208000090930         uopToDo   = 'A';
208100090129         uopPrtF   = 'ORMEST    ';
208200090129         uopOutQ   = D44outq;
208300090129         uopSave   = '*YES';
208400090129         trulOvrPr ( trulOvrPds );
208500110427         Qcmdm = uopCmd + ' usrdfndta(''' + %trimr(D44dta) + ''')';
208600090129      /end-free
208700060123     c                   call      'QCMDEXC'                            90
208800060123     c                   parm                    Qcmdm
208900060123     c                   parm                    Qlenm
209000060123      *
209100060123     c                   open      ORMest
209200060123e   3c                   endif
209300060123e   2c                   endif
209400060123    1c                   endif
209500081209      *
209600081209     c  n10              write     ORMeff
209700081209     c   10              write     ORMefm
209800031117      *
209900031117      *
210000031117      * Stampa "Estero-Fax" (senza parole accentate)
210100081209if  1c                   if            *in05    = *on
210200081209     c                             and Osie3err = *blanks
210300081209     c                   movel     Osie3ddep     Ecoddep
210400031117e   1c                   endif
210500130508     c                   eval      Einvprtn   = pfcs
210600031117     c                   eval      Edeposito  = Pdepd
210700031117     c                   eval      Efaxdep    = Pfax
210800031117     c                   eval      Elocalitar = Plpr
210900041213     c                   eval      Eormrer    = Prer2
211000031117     c                   eval      Elocalitac = Plpc
211100031117     c                   eval      Eormdar    = Pdar
211200031117     c                   eval      Enumorm    = Porm
211300031117     c                   eval      Wdtgio     = Pdat
211400160413     c*****              eval      Eorgnom    = Poesn
211500160413     c                   eval      email      = pmail
211600031117     c                   write     ORMef
211700031117      *
211800031117      * "Libera" subito la pagina in stampa
211900031117     c                   close     ORMest
212000090129      /free
212100090129         clear  TrulOvrPds;
212200090930         uopToDo   = 'B';
212300090129         uopPrtF   = 'ORMEST    ';
212400090129         trulOvrPr ( trulOvrPds );
212500090129         Qcmd  = uopCmd;
212600090129      /end-free
212700040218     c                   call      'QCMDEXC'                            90
212800040218     c                   parm                    Qcmd
212900040218     c                   parm                    Qlen
213000031117      *
213100031117     c                   ENDSR
213200031117      *
213300031117      *---------------------------------------------------------------*
213400081209      *?UpdatFNORM * Aggiornamento Ordine Ritiro Merce (FNORM00F)    ?*
213500031117      *---------------------------------------------------------------*
213600031117     c     UpdatFNORM    BEGSR
213700031117      *
213800031117      * Data di stampa
213900031117     c                   z-add     DateU         ORMdst
214000081209     c                   z-add     DateU         SavORMdst
214100070924      * scrivo la fase se non attivo con la precedura GEO ORM
214200081209     c                   if        Data_Org > ORMdst  or
214300081209     c                             Data_Org = *zeros
214400031117      * Fase
214500031117if  1c                   if        ORMfao < 300
214600031117     c                   eval      ORMfao = 300
214700031117     c                   z-add     DateU         ORMdfo
214800031117     c                   movel     w0140         ORMofo
214900081210     c                   exsr      Tab_FAR
215000081210     c                   eval      ORMeti = d§FARass
215100031117e   1c                   endif
215200070924e   1c                   endif
215300060921      * modalità invio al partner
215400060925if  3c                   if        $Estero  = *on
215500060925     c                             or $DPD = *on
215600081210     c   11              eval      ORMfcs = 'F'                                 File
215700061005     c  n11              do
215800081210     c   10              eval      ORMfcs = 'M'                                 Mail
215900081210     c  n10              eval      ORMfcs = 'X'                                 Fax
216000061005     c                   enddo
216100060925     c                   endif
216200031117      *
216300071106     c                   z-add     DateU         ORMdtt
216400081209      *
216500081209      * SE ORM non in distinta pulisco la data di stampa
216600081209      *   (solo se procedura GEO ORM attiva)
216700081209     c                   if             ORMndc   = *zeros
216800081209     c                             and  ORMdst  >= Data_Org
216900081209     c                             and  Data_Org > *zeros
217000081209     c                   clear                   ORMdst
217100070924     c                   endif
217200081209      *
217300041122     c                   UPDATE    FNORM00
217400031117      *
217500031117     c                   ENDSR
217600031117      *
217700031117      *---------------------------------------------------------------*
217800081209      *?WriteFNORF * Scrittura del file "Storico fasi" (FNORF00F)    ?*
217900031117      *---------------------------------------------------------------*
218000031117     c     WriteFNORF    BEGSR
218100081209      *
218200070924      * scrivo la fase se non attivo con la precedura GEO ORM
218300081209     c                   if        Data_Org > SavORMdst or
218400081209     c                             Data_Org = *zeros
218500031117      *
218600031117     c                   clear                   FNORF000
218700031117      *
218800031117     c                   eval      ORFpoe = ORMpoe
218900031117     c                   eval      ORFnsr = ORMnsr
219000031117     c                   eval      ORFnor = ORMnor
219100031117     c                   eval      ORFnrv = ORMnrv
219200031117     c                   eval      ORFpog = R16por
219300031117     c                   eval      ORFdae = DateU
219400031117     c                   movel     w0140         ORFore
219500031117     c                   eval      ORFfar = 300
219600031117     c                   eval      ORFpue = KNMUS
219700040914      * riporto i dati della distinta se già abbinato l'orm
219800081209     c                   if        ORMndc > *zeros
219900081209     c                   eval      ORFfgs = ORMpor
220000081209     c                   eval      ORFndc = ORMndc
220100081209     c                   eval      ORFddc = ORMddc
220200081209     c                   endif
220300031117      *
220400031117     c                   WRITE     FNORF000
220500081209      *
220600070924     c                   endif
220700031117      *
220800031117     c                   ENDSR
220900110427      *
221000160413       //-----------------------------------------------------------------
221100160413       //?Richiamo il TRUL44R.
221200160413       //------------------------------------------------------------------
221300160413     c     CallTrul44    BEGSR
221400160413
221500160413     c                   eval      wOkTrul44 = *on
221600160413     c                   clear                   TRUL44ds
221700160413     c                   eval      D44pgm = 'FIOR16R'
221800160413     c                   z-add     ORMpor        D44po
221900160413     c                   eval      D44des = wMail
222000160413     c  n06              eval      §CM1var  = '*OBJM*' + 'COLLECTION -
222100160413     c                             REQUEST n.' + Porm
222200160413     c   06              eval      §CM1var  = '*OBJM*' + 'REMINDER -
222300160413     c                             COLLECTION REQUEST n.' + Porm
222400160413     c                             + ' DPD ' + Pormdpd
222500160413     c                   call      'TRUL44R'
222600160413     c                   parm                    TRUL44ds
222700160413     c                   parm                    TRTCM1ds
222800160413     c                   IF        D44err <> *blanks
222900160413     c                   eval      wOkTrul44 = *off
223000160413     c                   ENDIF
223100160413
223200160413     c                   ENDSR
223300110427      *---------------------------------------------------------------*
223400110427      *?OPERAZIONI FINALI                                            ?*
223500110427      *---------------------------------------------------------------*
223600110427     c     sr_End        BEGSR
223700110427      *
223800110427      *
223900110427if  1c                   IF        R16tla  = *blanks
224000110427     c                   eval      *inRT   = *on
224100110427x   1c                   ELSE
224200110427      * - Chiudo i file aperti
224300110427      *   e Cancello le eventuali override ai prtf
224400110427     c                   eval      PrmFine = 'L'
224500110427     c                   clear                   PrmFil
224600110427     c                   clear                   PrmTpOutq
224700110427     c                   clear                   PrmOutQ
224800110427     c                   clear                   PrmOutQLib
224900110427     c                   clear                   PrmEsito
225000110427     c                   call      'TRULOUTQ'
225100110427     c                   parm                    PrmFine
225200110427     c                   parm                    PrmFil
225300110427     c                   parm                    PrmTpOutQ
225400110427     c                   parm                    PrmOutQ
225500110427     c                   parm                    PrmOutQLib
225600110427     c                   parm                    PrmEsito
225700110427if  2c                   if        %open(FIOR18P)
225800110427     c                   close     FIOR18P
225900110427      /free
226000110427         clear  TrulOvrPds;
226100110427         uopToDo   = 'B';
226200110427         uopPrtF   = 'FIOR18P   ';
226300110427         trulOvrPr ( trulOvrPds );
226400110427         Qcmd  = uopCmd;
226500110427      /end-free
226600110427     c                   call      'QCMDEXC'                            90
226700110427     c                   parm                    Qcmd
226800110427     c                   parm                    Qlen
226900110427e   2c                   endif
227000110427if  2c                   if        %open(FIOR16P2)
227100110427     c                   close     FIOR16P2
227200110427      /free
227300110427         clear  TrulOvrPds;
227400110427         uopToDo   = 'B';
227500110427         uopPrtF   = 'FIOR16P2  ';
227600110427         trulOvrPr ( trulOvrPds );
227700110427         Qcmd  = uopCmd;
227800110427      /end-free
227900110427     c                   call      'QCMDEXC'                            90
228000110427     c                   parm                    Qcmd
228100110427     c                   parm                    Qlen
228200110427e   2c                   endif
228300110427if  2c                   if        %open(ORMest)
228400110427     c                   close     ORMest
228500110427      /free
228600110427         clear  TrulOvrPds;
228700110427         uopToDo   = 'B';
228800110427         uopPrtF   = 'ORMest    ';
228900110427         trulOvrPr ( trulOvrPds );
229000110427         Qcmd  = uopCmd;
229100110427      /end-free
229200110427     c                   call      'QCMDEXC'                            90
229300110427     c                   parm                    Qcmd
229400110427     c                   parm                    Qlen
229500110427e   2c                   endif
229600110427     c                   clear                   TIBS02ds
229700110427     c                   eval      T02tla = 'C'
229800110427     c                   call      'TIBS02R'
229900110427     c                   parm                    KPJBA
230000110427     c                   parm                    TIBS02ds
230100110427     c                   eval      *inLR   = *on
230200110427e   1c                   ENDIF
230300130510
230400130510     c                   eval      kpjbu = FIOR16ds
230500110427      *
230600110427     c                   return
230700110427      *
230800110427     c                   ENDSR
230900031117      *
231000031117      *---------------------------------------------------------------*
231100081209      *?DEFINIZIONE KEY-LIST                                         ?*
231200031117      *---------------------------------------------------------------*
231300031117     c     DefKLIST      BEGSR
231400031117      *
231500140127      * FNORM01L
231600031117     c     k04orm01      klist
231700031117     c                   kfld                    R16poe
231800031117     c                   kfld                    R16nsr
231900031117     c                   kfld                    R16nor
232000031117     c                   kfld                    R16nrv
232100140127      * FNORT11L
232200140127     c     k05ort11      klist
232300140127     c                   kfld                    R16poe
232400140127     c                   kfld                    R16nsr
232500140127     c                   kfld                    R16nor
232600140127     c                   kfld                    R16nrv
232700140127     c                   kfld                    kORTgst
232800031117      * FNORM06L / FNORM11L
232900031117     c     k02orm06      klist
233000031117     c                   kfld                    $L6(xx)
233100031117     c                   kfld                    R16ndc
233200031117      * FNSPE01L
233300031117     c     kFNSPE        klist
233400031117     c                   kfld                    kSPEfls
233500031117     c                   kfld                    kSPEcli
233600031117     c                   kfld                    kSPEcod
233700060123      * FNSP201L
233800060123     c     kFNSP2        klist
233900060123     c                   kfld                    kSPEcli
234000060123     c                   kfld                    kSPEcod
234100060123     c                   kfld                    kSP2tpe
234200060214      * FIAR404L
234300060214     c     kFIAR4        klist
234400060214     c                   kfld                    kAR4trc
234500060214     c                   kfld                    kAR4n14
234600120612      * fidst01l
234700120612     c     kfidst        klist
234800120612     c                   kfld                    DSTnpg
234900031117     c                   kfld                    DSTnfv
235000031117     c                   kfld                    DSTfgs
235100031117      * FIAPD01L
235200031117     c     kFIAPD        klist
235300031117     c                   kfld                    APDtip
235400031117     c                   kfld                    APDpdr
235500081209      * TFNTC01L
235600081209     c     kTFNTC        klist
235700081209     c                   kfld                    kNTCapl
235800081209     c                   kfld                    kNTCnk1
235900081209     c                   kfld                    kNTCnk2
236000081209     c                   kfld                    kNTCtnt
236100081209      * FIDPO01L
236200081209     c     kFIDPO        klist
236300081209     c                   kfld                    ORMpoe
236400081209     c                   kfld                    ORMnsr
236500081209     c                   kfld                    ORMnor
236600081209     c                   kfld                    ORMnrv
236700031117      * EDTAB01L
236800031117     c     kEDTAB        klist
236900031117     c                   kfld                    TABcod
237000031117     c                   kfld                    TABkey
237100031117      * TABEL00F
237200031117     c     kTABEL        klist
237300050307     c                   kfld                    TBLkut
237400031117     c                   kfld                    TBLcod
237500031117     c                   kfld                    TBLkey
237600031117      *
237700031117     c                   ENDSR
