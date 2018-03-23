000100970526     H*------------------------------------------------------------------------*
000200160705     H* Lancio Stampa Etichetta BRT
000300970526     H*------------------------------------------------------------------------*
000400161107     H DECEDIT('0,') DATEDIT(*DMY.) dftactgrp(*no) actgrp('QILE')
000500170801     H bnddir('UBBNDDIR':'UBBRTETI')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900160705     FUBBRTETID1cf   e             WORKSTN INFDS(DEVDS1)
001000160705     F                                     SFILE(BRTETIDMS:MSGULT)
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D* MESSAGGI
001500970526     D*------------
001600000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001700970526     D*------------
001800970526     D* DESCRIZIONI TASTI FUNZIONALI
001900970526     D*------------
002000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* FUNZIONI DA CARICARE A VIDEO
002300970526     D*------------
002400970526     D FUV             S             19    DIM(24)
002500970526     D*------------------------------------------------------------------------*
002600970526     D* INPUT
002700970526     D*------------------------------------------------------------------------*
002800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
002900970526     D*------------
003000970526     D FUDDS           DS
003100970526     D  FUDDE1                 1      1
003200970526     D  FUDFUN                 2      3
003300970526     D  FUDDE2                 4     19
003400970526     D  FUDFI0                20     34
003500970526     D  FUDATT                35     35
003600970526     D  FUDFI1                36     80
003700970526     D  FUDDES                 1     19
003800970526     D  FUDRIG                 1     80
003900970526     D*------------
004000970526     D* DS GESTIONE TASTI FUNZIONALI
004100970526     D*------------
004200970526     D DSKEY         E DS
004300970526     D DEVDS1          DS
004400970526     D  KEY                  369    369
004500970526     D*------------
004600970526     D* DS "XMSG" - GESTIONE MESSAGGI
004700970526     D*------------
004800970526     D DSMSG         E DS
004900970528     D*------------------
005000970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005100970528     D*------------------
005200970528     D WLBDA8          DS                  INZ
005300970528     D  G08DAT                 1      8  0
005400970528     D  G08INV                 9     16  0
005500970528     D  G08ERR                17     17
005600970528     D  G08TGI                18     22  0
005700970526     D*------------
005800970526     D* ARCHITETTURA
005900970526     D*------------
006000970526     D KPJBA         E DS
006100161107       // parametro d'ingresso UBBRTETIR
006200161107     D UBBRTETIDS    e ds
006300020923     D*-------------------
006400020923     D* DS REPERIMENTO DATI UTENTE
006500020923     D*-------------------
006600020923     D TIBS34DS      E DS                                                       *Profili utente
006700020923     D DDATIUTE      E DS                                                       *Dati utente
006800020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900120927     D* DS REPERIMENTO PADRE/FIGLI
007000120927     D TIBS10DS      E DS
007100120928     D*-------------------
007200120928
007300121003     d Status         sds
007400121003     d  SDSpgm           *proc
007500121003     d  SDSprm           *parms
007600121003     d  SDSuser              254    263
007700121003
007800160705       // - Parametri da passare
007900160705       // tipo lancio: 'C'=LR   *blank=RT
008000161107     d*pIn_TLA         s              1a
008100160705       // offset
008200161107     d*pIn_OfsX        s              8s 3
008300161107     d*pIn_OfsY        s              8s 3
008400160705       // stampa bordo esterno etichetta SI/NO(' ')
008500161107     d*pIn_Bordo       s              1a
008600160705       // stampa logo SI(' ')/NO
008700161107     d*pIn_Logo        s              1a
008800160705       // stampa riga di contrasto SI/NO(' ')
008900161107     d*pIn_Row         s              1a
009000160705       // tipo etichetta ITA(' ')/DPD/EEX/FED
009100161107     d*pIn_TiE         s              3a
009200160705       // ristampa
009300161107     d*pIn_Rst         s              1a
009400160705       // anno/mese (se vuoti prendo i dati di oggi)
009500161107     d*pIn_AAS         s              4s 0
009600161107     d*pIn_MGS         s              4s 0
009700160705       // tipo servizio ' ' (stampa il box ma non la lettera)/C/E/H/D (non stampa nulla)
009800161107     d*pIn_TSP         s              1a
009900160705       // codice prodotto
010000161107     d*pIn_CDP         s             15a
010100160705       // MITTENTE: ragione sociale - provincia - nazione (' '=Italia)
010200161107     d*pIn_RSM         s             35a
010300161107     d*pIn_PRM         s              2a
010400161107     d*pIn_NZM         s              3a
010500160705       // DESTINATARIO: ragione sociale - indirizzo - CAP - località - provincia -
010600160705       //               nazione (' '=Italia)
010700161107     d*pIn_RSD         s             35a
010800161107     d*pIn_IND         s             35a
010900161107     d*pIn_CAD         s              9a
011000161107     d*pIn_LOD         s             35a
011100161107     d*pIn_PRD         s              2a
011200161107     d*pIn_NZD         s              3a
011300160705       //  Cod. ISO nazione arrivo
011400161107     d*pIn_ISA         s             35a
011500161107     d*pIn_FLS         s              3s 0
011600161107     d*pIn_NRS         s              2s 0
011700161107     d*pIn_NCD         s              7s 0
011800161107     d*pIn_NCA         s              7s 0
011900161107     d*pIn_LNA         s              3s 0
012000161107     d*pIn_TMA         s              3s 0
012100161107     d*pIn_ZNC         s              2s 0
012200161107     d*pIn_NC1         s              5s 0
012300161107     d*pIn_NC2         s              5s 0
012400161107     d*pIn_PKG         s              7s 1
012500161107     d*pIn_VOL         s              5s 3
012600160705
012700160705       //  Spool name - Job name - User name - Jon number - File number
012800161107     d*pOut_FNam       s             10a
012900161107     d*pOut_JNam       s             10a
013000161107     d*pOut_UNam       s             10a
013100161107     d*pOut_JNum       s              6a
013200161107     d*pOut_Fnum       s              6a
013300160705       // Esito 0=OK 1=errore durante la stampa 2=parametri di ingresso errati o mancanti
013400161107     d*pOut_Esito      s              3s 0
013500130116
013600130116     d ora             s               t
013700161003     d wX              s              2s 0
013800161003     d wPgm            s             10a
013900170406     D wFnumDS         ds
014000170406     D  wFnum                         6a   dim(100)
014100170421     D wUDtaDS         ds
014200170421     D  wUDta                        10a   dim(100)
014300120928
014400161107       //--------------------------------------------------------------
014500161107       // Definizione prototipi procedure.                             ?
014600161107       //--------------------------------------------------------------
014700161107
014800161107       /copy gaitrasrc/srcProtoPR,UBBRTETIWR
014900161107
015000970526     C*------------------------------------------------------------------------*
015100000303     C* MAIN LINES
015200970526     C*------------------------------------------------------------------------*
015300970526     C*
015400120927     C* metto i default su D03
015500120927     C                   EXSR      InzD03
015600120927     C*
015700120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015800991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016100970526     C*
016200970526     C* EMETTE FORMATO VIDEO
016300970526     C                   EXSR      BUTFMR
016400970526     C*
016500970526     C* GESTIONE TASTI FUNZIONALI
016600970526     C                   EXSR      FUNCON
016700970526E   2C                   ENDDO
016800970526     C*
016900970526     C* FINE PGM
017000970526     C                   SETON                                        LR
017100970526     C*------------------------------------------------------------------------*
017200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
017300970526     C*------------------------------------------------------------------------*
017400970526     C     BUTFMR        BEGSR
017500970526     C*
017600970526     C* EMETTE LA TESTATA
017700160705     C                   WRITE     BRTETID01
017800970526     C*
017900970526     C* SPEDISCE EVENTUALE MESSAGGIO
018000970526     C                   EXSR      SNDMSG
018100970526     C*
018200970526     C* EMETTE LE FUNZIONI
018300160705     C                   WRITE     BRTETID02
018400970526     C*
018500970526     C* EMETTE IL VIDEO
018600160705     C                   EXFMT     BRTETID03
018700970526     C*
018800991214     C* REIMPOSTA FLAG DI ERRORE
018900991214     C                   MOVEL     '0'           ERRL00
019000970526     C*
019100970526     C* REIMPOSTA INDICATORI DI ERRORE
019200020611     C                   SETOFF                                       555556
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600120927     C* InzD03 - Inizializzo D03
019700970526     C*------------------------------------------------------------------------*
019800120927     C     InzD03        BEGSR
019900120927     C*
020000161003     C                   EVAL      VIDTpStp   ='Z'
020100160718     c* metto un po' di default per fare in fretta i test
020200170419     C                   EVAL      VIDOFSX    = 0
020300170419     C                   EVAL      VIDOFSY    = 0
020400160718     C                   EVAL      VIDBordo   = 'S'
020500160718     C                   EVAL      VIDLogo    = 'S'
020600160718     C                   EVAL      VIDRow     = 'S'
020700160718     C                   EVAL      VIDTiE     = *blank
020800160720     C                   EVAL      VIDRst     = 'R'
020900160718     C                   EVAL      VIDAAS     =
021000160718     C                              %dec(%subst(%char(%date()) : 1 : 4) : 4 : 0)
021100160718     C                   EVAL      VIDMGS     =
021200160718     C                              %dec(%subst(%char(%date()) : 6 : 2) +
021300160718     C                                   %subst(%char(%date()) : 9 : 2) : 4 : 0)
021400160718     C                   EVAL      VIDTSP     = *blank
021500160718     C                   EVAL      VIDCDP     = VIDCDP
021600160718     C                   EVAL      VIDRSM     = 'MITTENTE'
021700160718     C                   EVAL      VIDPRM     = 'BO'
021800160718     C                   EVAL      VIDNZM     = *blank
021900160718     C                   EVAL      VIDRSD     = 'DESTINATARIO'
022000160718     C                   EVAL      VIDIND     = 'VIA TEST'
022100160718     C                   EVAL      VIDCAD     = '02000'
022200160718     C                   EVAL      VIDLOD     = 'MILANO'
022300160718     C                   EVAL      VIDPRD     = 'MI'
022400160718     C                   EVAL      VIDNZD     = *blank
022500160718     C                   EVAL      VIDISA     = *blank
022600160718     C                   EVAL      VIDFLS     = 001
022700160720     C                   EVAL      VIDNRS     = 22
022800160718     C                   EVAL      VIDNCD     = 1
022900160718     C                   EVAL      VIDNCA     = 3
023000160718     C                   EVAL      VIDLNA     = 002
023100160718     C                   EVAL      VIDTMA     = 076
023200160718     C                   EVAL      VIDZNC     = 14
023300160718     C                   EVAL      VIDNC1     = 0
023400160718     C                   EVAL      VIDNC2     = 3
023500160718     C                   EVAL      VIDPKG     = 1
023600160718     C                   EVAL      VIDVOL     = 1
023700170208     C                   EVAL      VIDSegnErr = *blank
023800170208     C                   EVAL      VIDMsgErT = *blank
023900170208     C                   EVAL      VIDMsgEr1 = *blank
024000170208     C                   EVAL      VIDMsgEr2 = *blank
024100170208     C                   EVAL      VIDMsgEr3 = *blank
024200170208     C                   EVAL      VIDMsgEr4 = *blank
024300170209     C                   EVAL      VIDMsgEr5 = *blank
024400170209     C                   EVAL      VIDMsgEr6 = *blank
024500170209     C                   EVAL      VIDMsgEr7 = *blank
024600130108     C*
024700120927     C                   ENDSR
024800120927     C*------------------------------------------------------------------------*
024900120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
025000120927     C*------------------------------------------------------------------------*
025100120927     C     SNDMSG        BEGSR
025200120927     C*
025300970526     C* PULISCE IL SFILE MESSAGGI
025400970526     C                   MOVEL     '1'           *IN40
025500160705     C                   WRITE     BRTETIDMC
025600970526     C                   MOVEL     '0'           *IN40
025700970526     C*
025800970526     C* VISUALIZZA IL MESSAGGIO
025900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
026000970526     C                   CALL      'XMSG'        XMSGPA
026100020611     C     DSMSER        IFEQ      *ZEROS
026200970526     C                   MOVEL     DSMSKE        MSGKEY
026300020611     C                   ENDIF
026400970526     C                   Z-ADD     1             MSGULT
026500160705     C                   WRITE     BRTETIDMS
026600160705     C                   WRITE     BRTETIDMC
026700970526     C*
026800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
026900970526     C                   MOVEL     *BLANKS       DSMSMS
027000970526     C*
027100970526     C                   ENDSR
027200970526     C*------------------------------------------------------------------------*
027300970526     C* ENTGES - GESTIONE ENTER -> INVIO
027400970526     C*------------------------------------------------------------------------*
027500970526     C     ENTGES        BEGSR
027600970526     C*
027700970526     C* CONTROLLA I CAMPI IMMESSI
027800970526     C                   EXSR      DECODI
027900970526     C*
028000970526     C                   ENDSR
028100970526     C*------------------------------------------------------------------------*
028200970526     C* F03GES - GESTIONE F03 -> FINE
028300970526     C*------------------------------------------------------------------------*
028400970526     C     F03GES        BEGSR
028500970526     C*
028600970526     C* IMPOSTA "PREMUTO F03"
028700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
028800970526     C*
028900970526     C                   ENDSR
029000970526     C*------------------------------------------------------------------------*
029100991214     C* F06GES - GESTIONE F06 -> CONFERMA
029200970526     C*------------------------------------------------------------------------*
029300970526     C     F06GES        BEGSR
029400970526     C*
029500991214     C* CONTROLLA I CAMPI IMMESSI
029600991214     C                   EXSR      DECODI
029700020611     C*
029800991214     C* ESEGUE ROUTINE DI CONFERMA
029900020611     C     ERRL00        IFEQ      '0'
030000991214     C                   EXSR      ESEGUI
030100130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
030200121109     C                   MOVEL     MSG(20)       DSMSMS
030300121109     C* non esco mai dal pgm
030400121109     C***                MOVEL     '1'           FINL00
030500121108     C                   ENDIF
030600970526     C*
030700991214     C                   ENDSR
030800970526     C*------------------------------------------------------------------------*
030900130109     C* F07GES - GESTIONE F07 -> STAMPA
031000970526     C*------------------------------------------------------------------------*
031100130109     C     F07GES        BEGSR
031200970526     C*
031300140818     C                   EXSR      F06GES
031400130110     C*
031500970526     C                   ENDSR
031600130109     C*------------------------------------------------------------------------*
031700130109     C* F12GES - GESTIONE F12 -> RITORNO
031800130109     C*------------------------------------------------------------------------*
031900130109     C     F12GES        BEGSR
032000130109     C*
032100130109     C* IMPOSTA "PREMUTO F12"
032200130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
032300130109     C*
032400130109     C                   ENDSR
032500970526     C*------------------------------------------------------------------------*
032600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
032700970526     C*------------------------------------------------------------------------*
032800970526     C     F24GES        BEGSR
032900970526     C*
033000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
033100970526     C*
033200970526     C                   ENDSR
033300970526     C*------------------------------------------------------------------------*
033400140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
033500970526     C*------------------------------------------------------------------------*
033600991214     C     ESEGUI        BEGSR
033700140120     C*
033800140120     C* creo il file temporaneo con elenco dei membri
033900160705     C                   EVAL      pIn_TLA     = VIDTLA
034000160705     C                   EVAL      pIn_OFSX    = VIDOFSX
034100160705     C                   EVAL      pIn_OFSY    = VIDOFSY
034200160705     C                   EVAL      pIn_Bordo   = VIDBordo
034300160705     C                   EVAL      pIn_Logo    = VIDLogo
034400160705     C                   EVAL      pIn_Row     = VIDRow
034500160705     C                   EVAL      pIn_TiE     = VIDTiE
034600160705     C                   EVAL      pIn_Rst     = VIDRst
034700160705     C                   EVAL      pIn_AAS     = VIDAAS
034800160705     C                   EVAL      pIn_MGS     = VIDMGS
034900160705     C                   EVAL      pIn_TSP     = VIDTSP
035000160705     C                   EVAL      pIn_CDP     = VIDCDP
035100160705     C                   EVAL      pIn_RSM     = VIDRSM
035200160705     C                   EVAL      pIn_PRM     = VIDPRM
035300160705     C                   EVAL      pIn_NZM     = VIDNZM
035400160705     C                   EVAL      pIn_RSD     = VIDRSD
035500160705     C                   EVAL      pIn_IND     = VIDIND
035600160705     C                   EVAL      pIn_CAD     = VIDCAD
035700160705     C                   EVAL      pIn_LOD     = VIDLOD
035800160705     C                   EVAL      pIn_PRD     = VIDPRD
035900160705     C                   EVAL      pIn_NZD     = VIDNZD
036000160705     C                   EVAL      pIn_ISA     = VIDISA
036100160705     C                   EVAL      pIn_FLS     = VIDFLS
036200160705     C                   EVAL      pIn_NRS     = VIDNRS
036300160705     C                   EVAL      pIn_NCD     = VIDNCD
036400160705     C                   EVAL      pIn_NCA     = VIDNCA
036500160705     C                   EVAL      pIn_LNA     = VIDLNA
036600160705     C                   EVAL      pIn_TMA     = VIDTMA
036700160705     C                   EVAL      pIn_ZNC     = VIDZNC
036800160705     C                   EVAL      pIn_NC1     = VIDNC1
036900160705     C                   EVAL      pIn_NC2     = VIDNC2
037000160705     C                   EVAL      pIn_PKG     = VIDPKG
037100160705     C                   EVAL      pIn_VOL     = VIDVOL
037200170406     C                   EVAL      pIn_1SplXC  = VID1SplXC
037300140819     c                   clear                   pOut_Esito
037400170406     c                   clear                   pOut_FNam
037500170406     c                   clear                   pOut_JNam
037600170406     c                   clear                   pOut_UNam
037700170406     c                   clear                   pOut_JNum
037800170406     c                   clear                   pOut_Fnum
037900170421     c                   clear                   pOut_UDta
038000161003
038100161107     C                   SELECT
038200161107     C                   WHEN      VIDTpStp = 'A'
038300161107     C                   EVAL      pIn_TpStp = 'A'
038400161107     C                   WHEN      VIDTpStp = 'Z'
038500161107     C                   EVAL      pIn_TpStp = 'Z'
038600161107     C                   ENDSL
038700170208     C                   EVAL      pIn_SegErr = VIDSegnErr
038800170208     C                   EVAL      pIn_MsgErT = VIDMsgErT
038900170209     C                   EVAL      %subst(pIn_MsgErr:1:20) = VIDMsgEr1
039000170209     C                   EVAL      %subst(pIn_MsgErr:21:20) = VIDMsgEr2
039100170209     C                   EVAL      %subst(pIn_MsgErr:41:20) = VIDMsgEr3
039200170209     C                   EVAL      %subst(pIn_MsgErr:61:40) = VIDMsgEr4
039300170209     C                   EVAL      %subst(pIn_MsgErr:101:40) = VIDMsgEr5
039400170209     C                   EVAL      %subst(pIn_MsgErr:141:40) = VIDMsgEr6
039500170209     C                   EVAL      %subst(pIn_MsgErr:181:40) = VIDMsgEr7
039600170310     C                   EVAL      pIn_OutQ    = VIDOutQ
039700170310     C                   EVAL      pIn_UsrDta  = VIDUsrDta
039800170208
039900121108     C* chiamo
040000161107
040100161107       ubbrtetiwr(ubbrtetids);
040200141113
040300140109     c                   EVAL      VIDEsito = pOut_Esito
040400160718     c                   EVAL      VIDFNam  = pOut_FNam
040500160718     c                   EVAL      VIDJNam  = pOut_JNam
040600160718     c                   EVAL      VIDUNam  = pOut_UNam
040700160718     c                   EVAL      VIDJNum  = pOut_JNum
040800170406     c                   EVAL      wFnumDS = pOut_Fnum
040900170406     c                   EVAL      VIDFnum_01 = wFnum(1)
041000170406     c                   EVAL      VIDFnum_02 = wFnum(2)
041100170406     c                   EVAL      VIDFnum_03 = wFnum(3)
041200170406     c                   EVAL      VIDFnum_04 = wFnum(4)
041300170406     c                   EVAL      VIDFnum_05 = wFnum(5)
041400170406     c                   EVAL      VIDFnum_06 = wFnum(6)
041500170406     c                   EVAL      VIDFnum_07 = wFnum(7)
041600170421     c                   EVAL      wUDtaDS = pOut_UDta
041700170421     c                   EVAL      VIDUDta_01 = wUDta(1)
041800170421     c                   EVAL      VIDUDta_02 = wUDta(2)
041900170421     c                   EVAL      VIDUDta_03 = wUDta(3)
042000170421     c                   EVAL      VIDUDta_04 = wUDta(4)
042100170421     c                   EVAL      VIDUDta_05 = wUDta(5)
042200170421     c                   EVAL      VIDUDta_06 = wUDta(6)
042300170421     c                   EVAL      VIDUDta_07 = wUDta(7)
042400140120     C*
042500970526     C                   ENDSR
042600970526     C*------------------------------------------------------------------------*
042700991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
042800970526     C*------------------------------------------------------------------------*
042900970526     C     DECODI        BEGSR
043000121019     C*
043100120927     C                   SETOFF                                       505152
043200140120     c                   EVAL      VIDMSG    = *blank
043300120927     C*
043400970528     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
043700970526     C*------------------------------------------------------------------------*
043800970526     C     AZZVID        BEGSR
043900970526     C* CAMPI
044000970526     C*
044100970526     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
044400970526     C*------------------------------------------------------------------------*
044500970526     C     IMPFUN        BEGSR
044600970526     C*
044700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
044800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
044900970526     C                   EXSR      FUNGES
045000970526     C*
045100970526     C                   ENDSR
045200020923     C*--------------------------------------------------------------------------------------------*
045300020923     C* REPERISCE I DATI UTENTE
045400020923     C*--------------------------------------------------------------------------------------------*
045500020923     C     REPDATIUTE    BEGSR
045600020923     C*
045700020923     C* INIZIALIZZA VARIABILI DI WRK
045800020923     C                   CLEAR                   TIBS34DS
045900020923     C                   CLEAR                   AZUTEDS
046000020923     C                   CLEAR                   DDATIUTE
046100020923     C*
046200020923     C     *DTAARA       DEFINE    §azute        azuteds
046300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
046400020923     C                   IN(E)     *DTAARA
046500020923if  1C                   IF        %Error
046600020923     c                   EVAL      I34Tla = 'L'
046700020923     C                   CALL      'TIBS34R'
046800020923     C                   PARM                    Tibs34Ds
046900020923     C                   IN        *DTAARA
047000020923e   1C                   ENDIF
047100020923     C*
047200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
047300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
047400020923     C*
047500020923     C                   ENDSR
047600970526     C*------------------------------------------------------------------------*
047700970526     C* *INZSR - OPERAZIONI INIZIALI
047800970526     C*------------------------------------------------------------------------*
047900970526     C     *INZSR        BEGSR
048000991217     C*
048100991217     C     *ENTRY        PLIST
048200991217     C                   PARM                    KPJBA
048300020923     C*
048400020923     C* REPERISCE I DATI DELL'UTENTE
048500020923     C                   EXSR      REPDATIUTE
048600970526     C*
048700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
048800970526     C     XMSGPA        PLIST
048900970526     C                   PARM                    DSMSMS
049000970526     C                   PARM                    DSMSPG
049100970526     C                   PARM                    DSMSKE
049200970526     C                   PARM                    DSMSER
049300970526     C*
049400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
049500970526     C                   CALL      'XKEY'
049600970526     C                   PARM                    DSKEY
049700970526     C*---
049800970526     C* VARIABILI NON RIFERITE AL DATA BASE
049900970526     C*---
050000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
050100991214     C                   MOVEL     *BLANKS       DEPOP1            3
050200970526     C*---
050300970526     C* VARIABILI DI CONTROLLO
050400970526     C*---
050500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
050600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
050700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
050800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
050900970526     C*
051000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
051100160705     C                   MOVEL     'UBBRTETIR1'  DSMSPG
051200160705     C                   MOVEL     'UBBRTETIR1'  MSGQUE
051300970526     C                   MOVEL     *BLANKS       MSGKEY
051400970526     C                   MOVEL     *BLANKS       DSMSMS
051500970528     C*
051600970528     C* CALCOLA LA DATA CORRENTE
051700970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
051800970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
051900970528     C                   Z-ADD     WN8           G08DAT
052000970528     C                   Z-ADD     *ZEROS        G08INV
052100970528     C                   MOVEL     '0'           G08ERR
052200970528     C                   CALL      'XSRDA8'
052300970528     C                   PARM                    WLBDA8
052400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
052500000310     C*
052600000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
052700000310     C                   EXSR      AZZVID
052800970526     C*
052900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
053000970526     C                   EXSR      IMPFUN
053100970526     C*
053200970526     C                   ENDSR
053300970526     C*------------------------------------------------------------------------*
053400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
053500970526     C*------------------------------------------------------------------------*
053600970526     C     FUNGES        BEGSR
053700970526     C*
053800970526     C* PULISCO CAMPI DI LAVORO
053900970526     C                   Z-ADD     0             W                 5 0
054000970526     C*
054100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
054200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
054300970526     C     1             DO        24            I
054400970526     C                   MOVEL     FUD(I)        FUDRIG
054500970526     C*
054600970526     C* PULIZIA FUNZIONI PRECEDENTI
054700970526     C                   MOVEL     *BLANKS       FUV(I)
054800970526     C*
054900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
055000970526     C* (SALTA F24)
055100970526     C     FUDATT        IFEQ      'S'
055200970526     C     FUDFUN        ANDNE     '24'
055300970526     C                   ADD       1             W
055400970526     C                   MOVEL     FUDDES        FUV(W)
055500970526     C                   END
055600970526     C                   END                                                    DO WHILE
055700970526     C*
055800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
055900970526     C* ALTRIMENTI DISATTIVO F24
056000970526     C     1             DO        24            I
056100970526     C                   MOVEL     FUD(I)        FUDRIG
056200970526     C*
056300970526     C     FUDFUN        IFEQ      '24'
056400970526     C     W             IFGT      6
056500970526     C                   MOVEL     'S'           FUDATT
056600970526     C                   ELSE
056700970526     C                   MOVEL     *BLANKS       FUDATT
056800970526     C                   END                                                    W GT 8
056900970526     C                   MOVEL     FUDRIG        FUD(I)
057000970526     C                   END
057100970526     C                   END                                                    DO WHILE
057200970526     C*
057300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
057400970526     C     W             DIV       6             I
057500970526     C                   MVR                     Z
057600970526     C*
057700970526     C     Z             IFNE      0
057800970526     C     I             ADD       1             Z
057900970526     C                   ELSE
058000970526     C                   Z-ADD     I             Z                 5 0
058100970526     C                   END
058200970526     C*
058300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
058400970526     C                   ADD       1             FUNGRU
058500970526     C     FUNGRU        IFGT      Z
058600970526     C     FUNGRU        ORLT      1
058700970526     C                   Z-ADD     1             FUNGRU
058800970526     C                   END
058900970526     C*
059000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
059100970526     C     FUNGRU        IFEQ      1
059200970526     C                   MOVEL     FUV(1)        FUN001
059300970526     C                   MOVEL     FUV(2)        FUN002
059400970526     C                   MOVEL     FUV(3)        FUN003
059500970526     C                   MOVEL     FUV(4)        FUN004
059600970526     C                   MOVEL     FUV(5)        FUN005
059700970526     C                   MOVEL     FUV(6)        FUN006
059800970526     C                   END
059900970526     C*
060000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
060100970526     C     FUNGRU        IFEQ      2
060200970526     C                   MOVEL     FUV(7)        FUN001
060300970526     C                   MOVEL     FUV(8)        FUN002
060400970526     C                   MOVEL     FUV(9)        FUN003
060500970526     C                   MOVEL     FUV(10)       FUN004
060600970526     C                   MOVEL     FUV(11)       FUN005
060700970526     C                   MOVEL     FUV(12)       FUN006
060800970526     C                   END
060900970526     C*
061000970526     C* TERZO GRUPPO DI 6 FUNZIONI
061100970526     C     FUNGRU        IFEQ      3
061200970526     C                   MOVEL     FUV(13)       FUN001
061300970526     C                   MOVEL     FUV(14)       FUN002
061400970526     C                   MOVEL     FUV(15)       FUN003
061500970526     C                   MOVEL     FUV(16)       FUN004
061600970526     C                   MOVEL     FUV(17)       FUN005
061700970526     C                   MOVEL     FUV(18)       FUN006
061800970526     C                   END
061900970526     C*
062000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
062100970526     C     FUNGRU        IFEQ      4
062200970526     C                   MOVEL     FUV(19)       FUN001
062300970526     C                   MOVEL     FUV(20)       FUN002
062400970526     C                   MOVEL     FUV(21)       FUN003
062500970526     C                   MOVEL     FUV(22)       FUN004
062600970526     C                   MOVEL     *BLANKS       FUN005
062700970526     C                   MOVEL     *BLANKS       FUN006
062800970526     C                   END
062900970526     C*
063000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
063100970526     C                   MOVEL     *BLANKS       FUN007
063200970526     C                   MOVEL     *BLANKS       FUN008
063300970526     C*
063400970526     C     1             DO        24            I
063500970526     C                   MOVEL     FUD(I)        FUDRIG
063600970526     C     FUDFUN        IFEQ      '24'
063700970526     C     FUDATT        ANDEQ     'S'
063800970526     C                   MOVEL     FUDDES        FUN008
063900970526     C                   END
064000970526     C                   END                                                    DO
064100970526     C*
064200970526     C                   ENDSR
064300970526     C*------------------------------------------------------------------------*
064400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
064500970526     C*------------------------------------------------------------------------*
064600970526     C     FUNCON        BEGSR
064700970526     C*
064800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
064900970526     C                   EXSR      FUNVAR
065000970526     C*
065100970526     C* PULISCO CAMPI DI LAVORO
065200970526     C                   Z-ADD     0             I                 5 0
065300970526     C                   Z-ADD     0             W                 5 0
065400970526     C*
065500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
065600970526     C     FUNKEY        IFNE      *BLANKS
065700970526     C     1             DO        24            I
065800970526     C                   MOVEL     FUD(I)        FUDRIG
065900970526     C*
066000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
066100970526     C     FUNKEY        IFEQ      FUDFUN
066200970526     C     FUDATT        ANDEQ     'S'
066300970526     C                   Z-ADD     I             W
066400970526     C                   END
066500970526     C                   END                                                    DO WHILE
066600970526     C*
066700970526     C* FUNZIONE NON ATTIVA (ERRORE)
066800970526     C     W             IFEQ      0
066900970526     C*
067000970526     C* EMISSIONE MESSAGGIO DI ERRORE
067100970526     C                   MOVEL     MSG(1)        DSMSMS
067200970526     C*
067300970526     C                   END                                                    W=0
067400970526     C                   END                                                    FUNKEY NE *BLANKS
067500970526     C*
067600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
067700970526     C     W             IFNE      0
067800970526     C     FUNKEY        OREQ      *BLANKS
067900970526     C     FUNKEY        IFEQ      *BLANKS
068000970526     C                   EXSR      ENTGES
068100970526     C                   END
068200970526     C     FUNKEY        IFEQ      '03'
068300970526     C                   EXSR      F03GES
068400970526     C                   END
068500970526     C     FUNKEY        IFEQ      '06'
068600970526     C                   EXSR      F06GES
068700970526     C                   END
068800130109     C     FUNKEY        IFEQ      '07'
068900130109     C                   EXSR      F07GES
069000130109     C                   END
069100970526     C     FUNKEY        IFEQ      '12'
069200970526     C                   EXSR      F12GES
069300970526     C                   END
069400970526     C     FUNKEY        IFEQ      '24'
069500970526     C                   EXSR      F24GES
069600970526     C                   END
069700970526     C                   END
069800970526     C*
069900970526     C                   ENDSR
070000970526     C*------------------------------------------------------------------------*
070100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
070200970526     C*------------------------------------------------------------------------*
070300970526     C     FUNVAR        BEGSR
070400970526     C*
070500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
070600970526     C*
070700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
070800970526     C     KEY           IFEQ      ENTER
070900970526     C                   MOVEL     *BLANKS       FUNKEY
071000970526     C                   END
071100970526     C     KEY           IFEQ      F1
071200970526     C                   MOVEL     '01'          FUNKEY
071300970526     C                   END
071400970526     C     KEY           IFEQ      F2
071500970526     C                   MOVEL     '02'          FUNKEY
071600970526     C                   END
071700970526     C     KEY           IFEQ      F3
071800970526     C                   MOVEL     '03'          FUNKEY
071900970526     C                   END
072000970526     C     KEY           IFEQ      F4
072100970526     C                   MOVEL     '04'          FUNKEY
072200970526     C                   END
072300970526     C     KEY           IFEQ      F5
072400970526     C                   MOVEL     '05'          FUNKEY
072500970526     C                   END
072600970526     C     KEY           IFEQ      F6
072700970526     C                   MOVEL     '06'          FUNKEY
072800970526     C                   END
072900970526     C     KEY           IFEQ      F7
073000970526     C                   MOVEL     '07'          FUNKEY
073100970526     C                   END
073200970526     C     KEY           IFEQ      F8
073300970526     C                   MOVEL     '08'          FUNKEY
073400970526     C                   END
073500970526     C     KEY           IFEQ      F9
073600970526     C                   MOVEL     '09'          FUNKEY
073700970526     C                   END
073800970526     C     KEY           IFEQ      F10
073900970526     C                   MOVEL     '10'          FUNKEY
074000970526     C                   END
074100970526     C     KEY           IFEQ      F11
074200970526     C                   MOVEL     '11'          FUNKEY
074300970526     C                   END
074400970526     C     KEY           IFEQ      F12
074500970526     C                   MOVEL     '12'          FUNKEY
074600970526     C                   END
074700970526     C     KEY           IFEQ      F13
074800970526     C                   MOVEL     '13'          FUNKEY
074900970526     C                   END
075000970526     C     KEY           IFEQ      F14
075100970526     C                   MOVEL     '14'          FUNKEY
075200970526     C                   END
075300970526     C     KEY           IFEQ      F15
075400970526     C                   MOVEL     '15'          FUNKEY
075500970526     C                   END
075600970526     C     KEY           IFEQ      F16
075700970526     C                   MOVEL     '16'          FUNKEY
075800970526     C                   END
075900970526     C     KEY           IFEQ      F17
076000970526     C                   MOVEL     '17'          FUNKEY
076100970526     C                   END
076200970526     C     KEY           IFEQ      F18
076300970526     C                   MOVEL     '18'          FUNKEY
076400970526     C                   END
076500970526     C     KEY           IFEQ      F19
076600970526     C                   MOVEL     '19'          FUNKEY
076700970526     C                   END
076800970526     C     KEY           IFEQ      F20
076900970526     C                   MOVEL     '20'          FUNKEY
077000970526     C                   END
077100970526     C     KEY           IFEQ      F21
077200970526     C                   MOVEL     '21'          FUNKEY
077300970526     C                   END
077400970526     C     KEY           IFEQ      F22
077500970526     C                   MOVEL     '22'          FUNKEY
077600970526     C                   END
077700970526     C     KEY           IFEQ      F23
077800970526     C                   MOVEL     '23'          FUNKEY
077900970526     C                   END
078000970526     C     KEY           IFEQ      F24
078100970526     C                   MOVEL     '24'          FUNKEY
078200970526     C                   END
078300970526     C     KEY           IFEQ      ROLLUP
078400970526     C                   MOVEL     'RU'          FUNKEY
078500970526     C                   END
078600970526     C     KEY           IFEQ      HELP
078700970526     C                   MOVEL     'HP'          FUNKEY
078800970526     C                   END
078900970526     C                   ENDSR
079000970526     C*------------------------------------------------------------------------*
079100970526     C* NOTE COSTRUZIONE SCHIERE
079200970526     C*------------------------------------------------------------------------*
079300970526     C*
079400970526     C* -MSG-MESSAGGI
079500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
079600970526     C* (NON TOCCARE MSG,1 E MSG,2)
079700970526     C*
079800970526     C* -CMD-COMANDI
079900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
080000970526     C*
080100970526     C* -OPD-OPZIONI
080200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
080300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
080400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
080500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
080600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
080700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
080800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
080900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
081000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
081100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
081200970526     C*
081300970526     C* -FUD-FUNZIONI
081400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
081500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
081600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
081700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
081800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
081900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
082000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
082100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
082200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
082300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
082400970526** MSG - MESSAGGI
082500120927Tasto funzionale non gestito                                                   1
082600160705                                                                               2
082700160705                                                                               3
082800140819                                                                               4
082900130108                                                                               5
083000130108Campo obbligatorio                                                             6
083100130108                                                                               7
083200130108                                                                               8
083300120927                                                                               9
083400120927                                                                               10
083500120927                                                                               11
083600120927                                                                               12
083700120927                                                                               13
083800120927Data errata                                                                    14
083900140116                                                                               15
084000120927                                                                               16
084100120927                                                                               17
084200120927                                                                               18
084300120927                                                                               19
084400160705Reperimento concluso - controllare l'esito                                     20
084500120927Scelta obbligatoria                                                            21
084600130110Eseguita la stampa                                                             22
084700121108                                                                               23
084800121108                                                                               24
084900970526** FUD - FUNZIONI
085000970526                   -        (S/N)=N                        funz.01¬
085100970526                   -        (S/N)=N                        funz.02¬
085200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
085300970526                   -        (S/N)=N                        funz.04¬
085400970526                   -        (S/N)=N                        funz.05¬
085500991214F06=Conferma       -        (S/N)=S                        funz.06¬
085600140819                   -        (S/N)=N                        funz.07¬
085700970527                   -        (S/N)=N                        funz.08¬
085800970527                   -        (S/N)=N                        funz.09¬
085900970527                   -        (S/N)=N                        funz.10¬
086000970526                   -        (S/N)=N                        funz.11¬
086100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
086200970526                   -        (S/N)=N                        funz.13¬
086300970526                   -        (S/N)=N                        funz.14¬
086400970526                   -        (S/N)=N                        funz.15¬
086500970526                   -        (S/N)=N                        funz.16¬
086600970526                   -        (S/N)=N                        funz.17¬
086700970526                   -        (S/N)=N                        funz.18¬
086800970526                   -        (S/N)=N                        funz.19¬
086900970526                   -        (S/N)=N                        funz.20¬
087000970526                   -        (S/N)=N                        funz.21¬
087100970526                   -        (S/N)=N                        funz.22¬
087200970526                   -        (S/N)=N                        funz.23¬
087300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
