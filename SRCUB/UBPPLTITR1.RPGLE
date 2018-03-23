000100970526     H*------------------------------------------------------------------------*
000200140625     H* Lancio Diagnostica formati TIT per ottenere la prima posizione libera
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800140625     FUBPPLTITD1cf   e             WORKSTN INFDS(DEVDS1)
000900140625     F                                     SFILE(PPLTITDMS:MSGULT)
001000140120     FWFTIDTMP  UF   E             DISK    usropn
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
006100020923     D*-------------------
006200020923     D* DS REPERIMENTO DATI UTENTE
006300020923     D*-------------------
006400020923     D TIBS34DS      E DS                                                       *Profili utente
006500020923     D DDATIUTE      E DS                                                       *Dati utente
006600020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006700120927     D* DS REPERIMENTO PADRE/FIGLI
006800120927     D TIBS10DS      E DS
006900120928     D*-------------------
007000120928
007100121003     d Status         sds
007200121003     d  SDSpgm           *proc
007300121003     d  SDSprm           *parms
007400121003     d  SDSuser              254    263
007500121003
007600140116     d pIn_Lib         s             10a
007700140116     d pIn_File        s             10a
007800140625     d pIn_Mbr         s             10a
007900140116     d pIn_SelTxt      s             10a
008000140117     d pOut_Esito      s              2  0
008100130116
008200130116     d ora             s               t
008300140117     d wX              s              2s 0
008400120928
008500970526     C*------------------------------------------------------------------------*
008600000303     C* MAIN LINES
008700970526     C*------------------------------------------------------------------------*
008800970526     C*
008900120927     C* metto i default su D03
009000120927     C                   EXSR      InzD03
009100120927     C*
009200120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009300991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009600970526     C*
009700970526     C* EMETTE FORMATO VIDEO
009800970526     C                   EXSR      BUTFMR
009900970526     C*
010000970526     C* GESTIONE TASTI FUNZIONALI
010100970526     C                   EXSR      FUNCON
010200970526E   2C                   ENDDO
010300970526     C*
010400970526     C* FINE PGM
010500970526     C                   SETON                                        LR
010600970526     C*------------------------------------------------------------------------*
010700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010800970526     C*------------------------------------------------------------------------*
010900970526     C     BUTFMR        BEGSR
011000970526     C*
011100970526     C* EMETTE LA TESTATA
011200140625     C                   WRITE     PPLTITD01
011300970526     C*
011400970526     C* SPEDISCE EVENTUALE MESSAGGIO
011500970526     C                   EXSR      SNDMSG
011600970526     C*
011700970526     C* EMETTE LE FUNZIONI
011800140625     C                   WRITE     PPLTITD02
011900970526     C*
012000970526     C* EMETTE IL VIDEO
012100140625     C                   EXFMT     PPLTITD03
012200970526     C*
012300991214     C* REIMPOSTA FLAG DI ERRORE
012400991214     C                   MOVEL     '0'           ERRL00
012500970526     C*
012600970526     C* REIMPOSTA INDICATORI DI ERRORE
012700020611     C                   SETOFF                                       555556
012800970526     C*
012900970526     C                   ENDSR
013000970526     C*------------------------------------------------------------------------*
013100120927     C* InzD03 - Inizializzo D03
013200970526     C*------------------------------------------------------------------------*
013300120927     C     InzD03        BEGSR
013400120927     C*
013500140117     C                   EVAL      VIDLib = 'GAITRASRC'
013600140117     C                   EVAL      VIDFile= 'SRCTIT'
013700140625     C                   EVAL      VIDMbr = 'TIT'
013800130108     C*
013900120927     C                   ENDSR
014000120927     C*------------------------------------------------------------------------*
014100120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014200120927     C*------------------------------------------------------------------------*
014300120927     C     SNDMSG        BEGSR
014400120927     C*
014500970526     C* PULISCE IL SFILE MESSAGGI
014600970526     C                   MOVEL     '1'           *IN40
014700140625     C                   WRITE     PPLTITDMC
014800970526     C                   MOVEL     '0'           *IN40
014900970526     C*
015000970526     C* VISUALIZZA IL MESSAGGIO
015100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015200970526     C                   CALL      'XMSG'        XMSGPA
015300020611     C     DSMSER        IFEQ      *ZEROS
015400970526     C                   MOVEL     DSMSKE        MSGKEY
015500020611     C                   ENDIF
015600970526     C                   Z-ADD     1             MSGULT
015700140625     C                   WRITE     PPLTITDMS
015800140625     C                   WRITE     PPLTITDMC
015900970526     C*
016000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016100970526     C                   MOVEL     *BLANKS       DSMSMS
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* ENTGES - GESTIONE ENTER -> INVIO
016600970526     C*------------------------------------------------------------------------*
016700970526     C     ENTGES        BEGSR
016800970526     C*
016900970526     C* CONTROLLA I CAMPI IMMESSI
017000970526     C                   EXSR      DECODI
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F03GES - GESTIONE F03 -> FINE
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F03GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F03"
017900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300991214     C* F06GES - GESTIONE F06 -> CONFERMA
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F06GES        BEGSR
018600970526     C*
018700991214     C* CONTROLLA I CAMPI IMMESSI
018800991214     C                   EXSR      DECODI
018900020611     C*
019000991214     C* ESEGUE ROUTINE DI CONFERMA
019100020611     C     ERRL00        IFEQ      '0'
019200991214     C                   EXSR      ESEGUI
019300130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
019400121109     C                   MOVEL     MSG(20)       DSMSMS
019500121109     C* non esco mai dal pgm
019600121109     C***                MOVEL     '1'           FINL00
019700121108     C                   ENDIF
019800970526     C*
019900991214     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100130109     C* F07GES - GESTIONE F07 -> STAMPA
020200970526     C*------------------------------------------------------------------------*
020300130109     C     F07GES        BEGSR
020400970526     C*
020500130110     C*
020600970526     C                   ENDSR
020700130109     C*------------------------------------------------------------------------*
020800130109     C* F12GES - GESTIONE F12 -> RITORNO
020900130109     C*------------------------------------------------------------------------*
021000130109     C     F12GES        BEGSR
021100130109     C*
021200130109     C* IMPOSTA "PREMUTO F12"
021300130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021400130109     C*
021500130109     C                   ENDSR
021600970526     C*------------------------------------------------------------------------*
021700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021800970526     C*------------------------------------------------------------------------*
021900970526     C     F24GES        BEGSR
022000970526     C*
022100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022200970526     C*
022300970526     C                   ENDSR
022400970526     C*------------------------------------------------------------------------*
022500140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
022600970526     C*------------------------------------------------------------------------*
022700991214     C     ESEGUI        BEGSR
022800140120     C*
022900140120     C* creo il file temporaneo con elenco dei membri
023000140116     C                   EVAL      pIn_Lib     = VIDLib
023100140116     C                   EVAL      pIn_File    = VIDFile
023200140625     C                   EVAL      pIn_Mbr     = VIDMbr
023300140116     C                   EVAL      pIn_SelTxt  = VIDSelTxt
023400140109     c                   clear                   pOut_Esito
023500121108     C* chiamo
023600140625     C                   CALL      'UBPPLTITC2'
023700140117     c                   parm                    pIn_Lib
023800140117     c                   parm                    pIn_File
023900140625     c                   parm                    pIn_Mbr
024000140116     c                   parm                    pIn_SelTxt
024100140116     c                   parm                    pOut_Esito
024200121108     C*
024300140109     c                   EVAL      VIDEsito = pOut_Esito
024400140116     C*
024500140116     C* se l'esito è 0 (cioè non ci sono stati errori che hanno bloccato la diagnostica)
024600140116     C* leggo il file WFTIDTMP per sapere il risultato
0247001401171    c                   IF        pOut_Esito = 0
024800140116     C                   OPEN      WFTIDTMP
0249001401202    C                   EVAL      wX = 0
0250001401202    C                   DOU       %eof(WFTIDTMP)
025100140117     C                   READ      WFTID000
0252001401173    C                   IF        %eof
025300140117     C                   LEAVE
0254001401173-   C                   ENDIF
025500140625     C* se è stato trovato qualcosa
0256001406253    C                   IF        TIDResult<>*blank
0257001401202    C                   EVAL      wX = wX + 1
025800140626     C* siccome arriva l'ultima posizione usata x la sua lunghezza,
025900140626     C* la prima posizione libera è quella dopo la lunghezza
0260001401203    C                   SELECT
026100140117     C                   WHEN      WX = 1
026200140116     C                   EVAL      VIDUti01 = TIDNome
026300140901     C                   EVAL      VIDUPU01 = %dec(%subst(TIDResult:1:4):4:0)
026400140901     C                   EVAL      VIDULU01 = %dec(%subst(TIDResult:5:4):4:0)
026500140626     C                   EVAL      VIDPPL01 = %dec(%subst(TIDResult:1:4):4:0) +
026600140626     C                                        %dec(%subst(TIDResult:5:4):4:0)
026700140117     C                   WHEN      WX = 2
026800140117     C                   EVAL      VIDUti02 = TIDNome
026900140901     C                   EVAL      VIDUPU02 = %dec(%subst(TIDResult:1:4):4:0)
027000140901     C                   EVAL      VIDULU02 = %dec(%subst(TIDResult:5:4):4:0)
027100140626     C                   EVAL      VIDPPL02 = %dec(%subst(TIDResult:1:4):4:0) +
027200140626     C                                        %dec(%subst(TIDResult:5:4):4:0)
027300140117     C                   WHEN      WX = 3
027400140117     C                   EVAL      VIDUti03 = TIDNome
027500140901     C                   EVAL      VIDUPU03 = %dec(%subst(TIDResult:1:4):4:0)
027600140901     C                   EVAL      VIDULU03 = %dec(%subst(TIDResult:5:4):4:0)
027700140626     C                   EVAL      VIDPPL03 = %dec(%subst(TIDResult:1:4):4:0) +
027800140626     C                                        %dec(%subst(TIDResult:5:4):4:0)
027900140117     C                   WHEN      WX = 4
028000140117     C                   EVAL      VIDUti04 = TIDNome
028100140901     C                   EVAL      VIDUPU04 = %dec(%subst(TIDResult:1:4):4:0)
028200140901     C                   EVAL      VIDULU04 = %dec(%subst(TIDResult:5:4):4:0)
028300140626     C                   EVAL      VIDPPL04 = %dec(%subst(TIDResult:1:4):4:0) +
028400140626     C                                        %dec(%subst(TIDResult:5:4):4:0)
028500140117     C                   WHEN      WX = 5
028600140117     C                   EVAL      VIDUti05 = TIDNome
028700140901     C                   EVAL      VIDUPU05 = %dec(%subst(TIDResult:1:4):4:0)
028800140901     C                   EVAL      VIDULU05 = %dec(%subst(TIDResult:5:4):4:0)
028900140626     C                   EVAL      VIDPPL05 = %dec(%subst(TIDResult:1:4):4:0) +
029000140626     C                                        %dec(%subst(TIDResult:5:4):4:0)
029100140117     C                   WHEN      WX = 6
029200140901     C                   EVAL      VIDUti06 = TIDNome
029300140901     C                   EVAL      VIDUPU06 = %dec(%subst(TIDResult:1:4):4:0)
029400140901     C                   EVAL      VIDULU06 = %dec(%subst(TIDResult:5:4):4:0)
029500140626     C                   EVAL      VIDPPL06 = %dec(%subst(TIDResult:1:4):4:0) +
029600140626     C                                        %dec(%subst(TIDResult:5:4):4:0)
029700140117     C                   WHEN      WX = 7
029800140117     C                   EVAL      VIDUti07 = TIDNome
029900140901     C                   EVAL      VIDUPU07 = %dec(%subst(TIDResult:1:4):4:0)
030000140901     C                   EVAL      VIDULU07 = %dec(%subst(TIDResult:5:4):4:0)
030100140626     C                   EVAL      VIDPPL07 = %dec(%subst(TIDResult:1:4):4:0) +
030200140626     C                                        %dec(%subst(TIDResult:5:4):4:0)
030300140120     C* per ora ho solo 7 campi risultato
030400140120     C                   WHEN      WX > 7
030500140120     C                   EVAL      VIDMSG = 'Sono più di 7'
0306001401173-   C                   ENDSL
030700140312     C***                DELETE    WFTID000
0308001401203-   C                   ENDIF
030900140312     C                   DELETE    WFTID000
0310001401202-   C                   ENDDO
031100140120     C                   UNLOCK    WFTIDTMP
031200140120     C                   CLOSE     WFTIDTMP
031300140120     C*
031400140120     C* disalloco il file temporaneo dei risultati copiato in QTEMP
031500140120     C***                EVAL      Cmd = 'DLCOBJ OBJ((QTEMP/WFTIDTMP *FILE +
031600140120     C***                                *SHRRD))'
031700140120      *
031800140120     C*                  Call      'QCMDEXC'
031900140120     C*                  Parm                    Cmd             140
032000140120     C*                  Parm      50            Len              15 5
0321001401201-   C                   ENDIF
032200991214     C*
032300970526     C                   ENDSR
032400970526     C*------------------------------------------------------------------------*
032500991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
032600970526     C*------------------------------------------------------------------------*
032700970526     C     DECODI        BEGSR
032800121019     C*
032900120927     C                   SETOFF                                       505152
033000140109     c                   EVAL      VIDUTI01  = *blank
033100140109     c                   EVAL      VIDUTI02  = *blank
033200140109     c                   EVAL      VIDUTI03  = *blank
033300140109     c                   EVAL      VIDUTI04  = *blank
033400140109     c                   EVAL      VIDUTI05  = *blank
033500140109     c                   EVAL      VIDUTI06  = *blank
033600140109     c                   EVAL      VIDUTI07  = *blank
033700140109     c                   EVAL      VIDUTI08  = *blank
033800140109     c                   EVAL      VIDUTI09  = *blank
033900140109     c                   EVAL      VIDUTI10  = *blank
034000140625     c                   EVAL      VIDPPL01  = *zeros
034100140625     c                   EVAL      VIDPPL02  = *zeros
034200140625     c                   EVAL      VIDPPL03  = *zeros
034300140625     c                   EVAL      VIDPPL04  = *zeros
034400140625     c                   EVAL      VIDPPL05  = *zeros
034500140625     c                   EVAL      VIDPPL06  = *zeros
034600140625     c                   EVAL      VIDPPL07  = *zeros
034700140901     c                   EVAL      VIDUPU01  = *zeros
034800140901     c                   EVAL      VIDUPU02  = *zeros
034900140901     c                   EVAL      VIDUPU03  = *zeros
035000140901     c                   EVAL      VIDUPU04  = *zeros
035100140901     c                   EVAL      VIDUPU05  = *zeros
035200140901     c                   EVAL      VIDUPU06  = *zeros
035300140901     c                   EVAL      VIDUPU07  = *zeros
035400140901     c                   EVAL      VIDULU01  = *zeros
035500140901     c                   EVAL      VIDULU02  = *zeros
035600140901     c                   EVAL      VIDULU03  = *zeros
035700140901     c                   EVAL      VIDULU04  = *zeros
035800140901     c                   EVAL      VIDULU05  = *zeros
035900140901     c                   EVAL      VIDULU06  = *zeros
036000140901     c                   EVAL      VIDULU07  = *zeros
036100140120     c                   EVAL      VIDMSG    = *blank
036200121001     C*
036300140625     C* Lib e File sono obbligatori
036400121001     C     ERRL00        IFEQ      '0'
036500140116     C                   IF        VIDLib = *blank or VIDFile = *blank
036600120927     C                   MOVEL     '1'           ERRL00
036700130108     C                   SETON                                        51
036800120927     C                   MOVEL     MSG(3)        DSMSMS
036900120927     C                   ENDIF
037000140116     C                   ENDIF
037100140625     C*
037200140625     C* Se presente il membro non si può richiedere una selezione sulla descrizione
037300140625     C     ERRL00        IFEQ      '0'
037400140625     C                   IF        VIDMbr <> *blank and VIDSelTxt <> *blank
037500140625     C                   MOVEL     '1'           ERRL00
037600140625     C                   SETON                                        5152
037700140625     C                   MOVEL     MSG(4)        DSMSMS
037800140625     C                   ENDIF
037900140625     C                   ENDIF
038000120927     C*
038100970528     C                   ENDSR
038200970526     C*------------------------------------------------------------------------*
038300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
038400970526     C*------------------------------------------------------------------------*
038500970526     C     AZZVID        BEGSR
038600970526     C* CAMPI
038700970526     C*
038800970526     C                   ENDSR
038900970526     C*------------------------------------------------------------------------*
039000020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
039100970526     C*------------------------------------------------------------------------*
039200970526     C     IMPFUN        BEGSR
039300970526     C*
039400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
039500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
039600970526     C                   EXSR      FUNGES
039700970526     C*
039800970526     C                   ENDSR
039900020923     C*--------------------------------------------------------------------------------------------*
040000020923     C* REPERISCE I DATI UTENTE
040100020923     C*--------------------------------------------------------------------------------------------*
040200020923     C     REPDATIUTE    BEGSR
040300020923     C*
040400020923     C* INIZIALIZZA VARIABILI DI WRK
040500020923     C                   CLEAR                   TIBS34DS
040600020923     C                   CLEAR                   AZUTEDS
040700020923     C                   CLEAR                   DDATIUTE
040800020923     C*
040900020923     C     *DTAARA       DEFINE    §azute        azuteds
041000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
041100020923     C                   IN(E)     *DTAARA
041200020923if  1C                   IF        %Error
041300020923     c                   EVAL      I34Tla = 'L'
041400020923     C                   CALL      'TIBS34R'
041500020923     C                   PARM                    Tibs34Ds
041600020923     C                   IN        *DTAARA
041700020923e   1C                   ENDIF
041800020923     C*
041900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
042000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
042100020923     C*
042200020923     C                   ENDSR
042300970526     C*------------------------------------------------------------------------*
042400970526     C* *INZSR - OPERAZIONI INIZIALI
042500970526     C*------------------------------------------------------------------------*
042600970526     C     *INZSR        BEGSR
042700991217     C*
042800991217     C     *ENTRY        PLIST
042900991217     C                   PARM                    KPJBA
043000020923     C*
043100020923     C* REPERISCE I DATI DELL'UTENTE
043200020923     C                   EXSR      REPDATIUTE
043300970526     C*
043400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
043500970526     C     XMSGPA        PLIST
043600970526     C                   PARM                    DSMSMS
043700970526     C                   PARM                    DSMSPG
043800970526     C                   PARM                    DSMSKE
043900970526     C                   PARM                    DSMSER
044000970526     C*
044100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
044200970526     C                   CALL      'XKEY'
044300970526     C                   PARM                    DSKEY
044400970526     C*---
044500970526     C* VARIABILI NON RIFERITE AL DATA BASE
044600970526     C*---
044700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
044800991214     C                   MOVEL     *BLANKS       DEPOP1            3
044900970526     C*---
045000970526     C* VARIABILI DI CONTROLLO
045100970526     C*---
045200970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
045300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
045400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
045500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
045600970526     C*
045700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
045800140625     C                   MOVEL     'UBPPLTITR1'  DSMSPG
045900140625     C                   MOVEL     'UBPPLTITR1'  MSGQUE
046000970526     C                   MOVEL     *BLANKS       MSGKEY
046100970526     C                   MOVEL     *BLANKS       DSMSMS
046200970528     C*
046300970528     C* CALCOLA LA DATA CORRENTE
046400970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
046500970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
046600970528     C                   Z-ADD     WN8           G08DAT
046700970528     C                   Z-ADD     *ZEROS        G08INV
046800970528     C                   MOVEL     '0'           G08ERR
046900970528     C                   CALL      'XSRDA8'
047000970528     C                   PARM                    WLBDA8
047100970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
047200000310     C*
047300000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
047400000310     C                   EXSR      AZZVID
047500970526     C*
047600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
047700970526     C                   EXSR      IMPFUN
047800970526     C*
047900970526     C                   ENDSR
048000970526     C*------------------------------------------------------------------------*
048100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
048200970526     C*------------------------------------------------------------------------*
048300970526     C     FUNGES        BEGSR
048400970526     C*
048500970526     C* PULISCO CAMPI DI LAVORO
048600970526     C                   Z-ADD     0             W                 5 0
048700970526     C*
048800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
048900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
049000970526     C     1             DO        24            I
049100970526     C                   MOVEL     FUD(I)        FUDRIG
049200970526     C*
049300970526     C* PULIZIA FUNZIONI PRECEDENTI
049400970526     C                   MOVEL     *BLANKS       FUV(I)
049500970526     C*
049600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
049700970526     C* (SALTA F24)
049800970526     C     FUDATT        IFEQ      'S'
049900970526     C     FUDFUN        ANDNE     '24'
050000970526     C                   ADD       1             W
050100970526     C                   MOVEL     FUDDES        FUV(W)
050200970526     C                   END
050300970526     C                   END                                                    DO WHILE
050400970526     C*
050500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
050600970526     C* ALTRIMENTI DISATTIVO F24
050700970526     C     1             DO        24            I
050800970526     C                   MOVEL     FUD(I)        FUDRIG
050900970526     C*
051000970526     C     FUDFUN        IFEQ      '24'
051100970526     C     W             IFGT      6
051200970526     C                   MOVEL     'S'           FUDATT
051300970526     C                   ELSE
051400970526     C                   MOVEL     *BLANKS       FUDATT
051500970526     C                   END                                                    W GT 8
051600970526     C                   MOVEL     FUDRIG        FUD(I)
051700970526     C                   END
051800970526     C                   END                                                    DO WHILE
051900970526     C*
052000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
052100970526     C     W             DIV       6             I
052200970526     C                   MVR                     Z
052300970526     C*
052400970526     C     Z             IFNE      0
052500970526     C     I             ADD       1             Z
052600970526     C                   ELSE
052700970526     C                   Z-ADD     I             Z                 5 0
052800970526     C                   END
052900970526     C*
053000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
053100970526     C                   ADD       1             FUNGRU
053200970526     C     FUNGRU        IFGT      Z
053300970526     C     FUNGRU        ORLT      1
053400970526     C                   Z-ADD     1             FUNGRU
053500970526     C                   END
053600970526     C*
053700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
053800970526     C     FUNGRU        IFEQ      1
053900970526     C                   MOVEL     FUV(1)        FUN001
054000970526     C                   MOVEL     FUV(2)        FUN002
054100970526     C                   MOVEL     FUV(3)        FUN003
054200970526     C                   MOVEL     FUV(4)        FUN004
054300970526     C                   MOVEL     FUV(5)        FUN005
054400970526     C                   MOVEL     FUV(6)        FUN006
054500970526     C                   END
054600970526     C*
054700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
054800970526     C     FUNGRU        IFEQ      2
054900970526     C                   MOVEL     FUV(7)        FUN001
055000970526     C                   MOVEL     FUV(8)        FUN002
055100970526     C                   MOVEL     FUV(9)        FUN003
055200970526     C                   MOVEL     FUV(10)       FUN004
055300970526     C                   MOVEL     FUV(11)       FUN005
055400970526     C                   MOVEL     FUV(12)       FUN006
055500970526     C                   END
055600970526     C*
055700970526     C* TERZO GRUPPO DI 6 FUNZIONI
055800970526     C     FUNGRU        IFEQ      3
055900970526     C                   MOVEL     FUV(13)       FUN001
056000970526     C                   MOVEL     FUV(14)       FUN002
056100970526     C                   MOVEL     FUV(15)       FUN003
056200970526     C                   MOVEL     FUV(16)       FUN004
056300970526     C                   MOVEL     FUV(17)       FUN005
056400970526     C                   MOVEL     FUV(18)       FUN006
056500970526     C                   END
056600970526     C*
056700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
056800970526     C     FUNGRU        IFEQ      4
056900970526     C                   MOVEL     FUV(19)       FUN001
057000970526     C                   MOVEL     FUV(20)       FUN002
057100970526     C                   MOVEL     FUV(21)       FUN003
057200970526     C                   MOVEL     FUV(22)       FUN004
057300970526     C                   MOVEL     *BLANKS       FUN005
057400970526     C                   MOVEL     *BLANKS       FUN006
057500970526     C                   END
057600970526     C*
057700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
057800970526     C                   MOVEL     *BLANKS       FUN007
057900970526     C                   MOVEL     *BLANKS       FUN008
058000970526     C*
058100970526     C     1             DO        24            I
058200970526     C                   MOVEL     FUD(I)        FUDRIG
058300970526     C     FUDFUN        IFEQ      '24'
058400970526     C     FUDATT        ANDEQ     'S'
058500970526     C                   MOVEL     FUDDES        FUN008
058600970526     C                   END
058700970526     C                   END                                                    DO
058800970526     C*
058900970526     C                   ENDSR
059000970526     C*------------------------------------------------------------------------*
059100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
059200970526     C*------------------------------------------------------------------------*
059300970526     C     FUNCON        BEGSR
059400970526     C*
059500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
059600970526     C                   EXSR      FUNVAR
059700970526     C*
059800970526     C* PULISCO CAMPI DI LAVORO
059900970526     C                   Z-ADD     0             I                 5 0
060000970526     C                   Z-ADD     0             W                 5 0
060100970526     C*
060200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
060300970526     C     FUNKEY        IFNE      *BLANKS
060400970526     C     1             DO        24            I
060500970526     C                   MOVEL     FUD(I)        FUDRIG
060600970526     C*
060700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
060800970526     C     FUNKEY        IFEQ      FUDFUN
060900970526     C     FUDATT        ANDEQ     'S'
061000970526     C                   Z-ADD     I             W
061100970526     C                   END
061200970526     C                   END                                                    DO WHILE
061300970526     C*
061400970526     C* FUNZIONE NON ATTIVA (ERRORE)
061500970526     C     W             IFEQ      0
061600970526     C*
061700970526     C* EMISSIONE MESSAGGIO DI ERRORE
061800970526     C                   MOVEL     MSG(1)        DSMSMS
061900970526     C*
062000970526     C                   END                                                    W=0
062100970526     C                   END                                                    FUNKEY NE *BLANKS
062200970526     C*
062300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
062400970526     C     W             IFNE      0
062500970526     C     FUNKEY        OREQ      *BLANKS
062600970526     C     FUNKEY        IFEQ      *BLANKS
062700970526     C                   EXSR      ENTGES
062800970526     C                   END
062900970526     C     FUNKEY        IFEQ      '03'
063000970526     C                   EXSR      F03GES
063100970526     C                   END
063200970526     C     FUNKEY        IFEQ      '06'
063300970526     C                   EXSR      F06GES
063400970526     C                   END
063500130109     C     FUNKEY        IFEQ      '07'
063600130109     C                   EXSR      F07GES
063700130109     C                   END
063800970526     C     FUNKEY        IFEQ      '12'
063900970526     C                   EXSR      F12GES
064000970526     C                   END
064100970526     C     FUNKEY        IFEQ      '24'
064200970526     C                   EXSR      F24GES
064300970526     C                   END
064400970526     C                   END
064500970526     C*
064600970526     C                   ENDSR
064700970526     C*------------------------------------------------------------------------*
064800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
064900970526     C*------------------------------------------------------------------------*
065000970526     C     FUNVAR        BEGSR
065100970526     C*
065200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
065300970526     C*
065400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
065500970526     C     KEY           IFEQ      ENTER
065600970526     C                   MOVEL     *BLANKS       FUNKEY
065700970526     C                   END
065800970526     C     KEY           IFEQ      F1
065900970526     C                   MOVEL     '01'          FUNKEY
066000970526     C                   END
066100970526     C     KEY           IFEQ      F2
066200970526     C                   MOVEL     '02'          FUNKEY
066300970526     C                   END
066400970526     C     KEY           IFEQ      F3
066500970526     C                   MOVEL     '03'          FUNKEY
066600970526     C                   END
066700970526     C     KEY           IFEQ      F4
066800970526     C                   MOVEL     '04'          FUNKEY
066900970526     C                   END
067000970526     C     KEY           IFEQ      F5
067100970526     C                   MOVEL     '05'          FUNKEY
067200970526     C                   END
067300970526     C     KEY           IFEQ      F6
067400970526     C                   MOVEL     '06'          FUNKEY
067500970526     C                   END
067600970526     C     KEY           IFEQ      F7
067700970526     C                   MOVEL     '07'          FUNKEY
067800970526     C                   END
067900970526     C     KEY           IFEQ      F8
068000970526     C                   MOVEL     '08'          FUNKEY
068100970526     C                   END
068200970526     C     KEY           IFEQ      F9
068300970526     C                   MOVEL     '09'          FUNKEY
068400970526     C                   END
068500970526     C     KEY           IFEQ      F10
068600970526     C                   MOVEL     '10'          FUNKEY
068700970526     C                   END
068800970526     C     KEY           IFEQ      F11
068900970526     C                   MOVEL     '11'          FUNKEY
069000970526     C                   END
069100970526     C     KEY           IFEQ      F12
069200970526     C                   MOVEL     '12'          FUNKEY
069300970526     C                   END
069400970526     C     KEY           IFEQ      F13
069500970526     C                   MOVEL     '13'          FUNKEY
069600970526     C                   END
069700970526     C     KEY           IFEQ      F14
069800970526     C                   MOVEL     '14'          FUNKEY
069900970526     C                   END
070000970526     C     KEY           IFEQ      F15
070100970526     C                   MOVEL     '15'          FUNKEY
070200970526     C                   END
070300970526     C     KEY           IFEQ      F16
070400970526     C                   MOVEL     '16'          FUNKEY
070500970526     C                   END
070600970526     C     KEY           IFEQ      F17
070700970526     C                   MOVEL     '17'          FUNKEY
070800970526     C                   END
070900970526     C     KEY           IFEQ      F18
071000970526     C                   MOVEL     '18'          FUNKEY
071100970526     C                   END
071200970526     C     KEY           IFEQ      F19
071300970526     C                   MOVEL     '19'          FUNKEY
071400970526     C                   END
071500970526     C     KEY           IFEQ      F20
071600970526     C                   MOVEL     '20'          FUNKEY
071700970526     C                   END
071800970526     C     KEY           IFEQ      F21
071900970526     C                   MOVEL     '21'          FUNKEY
072000970526     C                   END
072100970526     C     KEY           IFEQ      F22
072200970526     C                   MOVEL     '22'          FUNKEY
072300970526     C                   END
072400970526     C     KEY           IFEQ      F23
072500970526     C                   MOVEL     '23'          FUNKEY
072600970526     C                   END
072700970526     C     KEY           IFEQ      F24
072800970526     C                   MOVEL     '24'          FUNKEY
072900970526     C                   END
073000970526     C     KEY           IFEQ      ROLLUP
073100970526     C                   MOVEL     'RU'          FUNKEY
073200970526     C                   END
073300970526     C     KEY           IFEQ      HELP
073400970526     C                   MOVEL     'HP'          FUNKEY
073500970526     C                   END
073600970526     C                   ENDSR
073700970526     C*------------------------------------------------------------------------*
073800970526     C* NOTE COSTRUZIONE SCHIERE
073900970526     C*------------------------------------------------------------------------*
074000970526     C*
074100970526     C* -MSG-MESSAGGI
074200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
074300970526     C* (NON TOCCARE MSG,1 E MSG,2)
074400970526     C*
074500970526     C* -CMD-COMANDI
074600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
074700970526     C*
074800970526     C* -OPD-OPZIONI
074900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
075000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
075100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
075200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
075300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
075400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
075500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
075600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
075700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
075800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
075900970526     C*
076000970526     C* -FUD-FUNZIONI
076100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
076200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
076300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
076400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
076500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
076600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
076700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
076800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
076900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
077000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
077100970526** MSG - MESSAGGI
077200120927Tasto funzionale non gestito                                                   1
077300120927Numero chiamate obbligatorio                                                   2
077400140625Libreria/file sorgente obbligatori                                             3
077500140625Se presente il membro non si può richiedere una selezione sulla descrizione    4
077600130108                                                                               5
077700130108Campo obbligatorio                                                             6
077800130108                                                                               7
077900130108                                                                               8
078000120927                                                                               9
078100120927                                                                               10
078200120927                                                                               11
078300120927                                                                               12
078400120927                                                                               13
078500120927Data errata                                                                    14
078600140116                                                                               15
078700120927                                                                               16
078800120927                                                                               17
078900120927                                                                               18
079000120927                                                                               19
079100140116Diagnostica conclusa - controllare l'esito                                     20
079200120927Scelta obbligatoria                                                            21
079300130110Eseguita la stampa                                                             22
079400121108                                                                               23
079500121108                                                                               24
079600970526** FUD - FUNZIONI
079700970526                   -        (S/N)=N                        funz.01¬
079800970526                   -        (S/N)=N                        funz.02¬
079900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
080000970526                   -        (S/N)=N                        funz.04¬
080100970526                   -        (S/N)=N                        funz.05¬
080200991214F06=Conferma       -        (S/N)=S                        funz.06¬
080300140109                   -        (S/N)=N                        funz.07¬
080400970527                   -        (S/N)=N                        funz.08¬
080500970527                   -        (S/N)=N                        funz.09¬
080600970527                   -        (S/N)=N                        funz.10¬
080700970526                   -        (S/N)=N                        funz.11¬
080800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
080900970526                   -        (S/N)=N                        funz.13¬
081000970526                   -        (S/N)=N                        funz.14¬
081100970526                   -        (S/N)=N                        funz.15¬
081200970526                   -        (S/N)=N                        funz.16¬
081300970526                   -        (S/N)=N                        funz.17¬
081400970526                   -        (S/N)=N                        funz.18¬
081500970526                   -        (S/N)=N                        funz.19¬
081600970526                   -        (S/N)=N                        funz.20¬
081700970526                   -        (S/N)=N                        funz.21¬
081800970526                   -        (S/N)=N                        funz.22¬
081900970526                   -        (S/N)=N                        funz.23¬
082000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
