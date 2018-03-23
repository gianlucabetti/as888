000100970526     H*------------------------------------------------------------------------*
000200140625     H* Lancio Diagnostica formati TIT per ottenere la prima posizione libera
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800160909     FTNVRVMSD1 cf   e             WORKSTN INFDS(DEVDS1)
000900160909     F                                     SFILE(VRVMSDMS:MSGULT)
001000160909     FTIVMS01I  IF   E           K DISK    rename(TIVMS000:TIVMS01)
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
006100160912     D*------------
006200160912     D TIS7VMS2DS    E DS
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
007800140116     d pIn_Lib         s             10a
007900140116     d pIn_File        s             10a
008000140625     d pIn_Mbr         s             10a
008100160909     d pIn_DS_Sel      ds
008200160909     d  pIn_TpSk                      1
008300160909     d  pIn_Sk1                      10
008400160909     d  pIn_Sk2                      10
008500160909     d  pIn_Sk3                      10
008600160912     d  pIn_Proc                     10
008700160912     d  pIn_KSU                       8
008800160912     d  pIn_DDE                       8s 0
008900160912     d  pIn_DSC                       8s 0
009000140117     d pOut_Esito      s              2  0
009100130116
009200130116     d ora             s               t
009300140117     d wX              s              2s 0
009400160909     D*------------
009500160909     D* campi tipo data per le date a video
009600160909     D DDEISO          s               d
009700160909     D DSCISO          s               d
009800120928
009900970526     C*------------------------------------------------------------------------*
010000000303     C* MAIN LINES
010100970526     C*------------------------------------------------------------------------*
010200970526     C*
010300120927     C* metto i default su D03
010400120927     C                   EXSR      InzD03
010500120927     C*
010600120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011000970526     C*
011100970526     C* EMETTE FORMATO VIDEO
011200970526     C                   EXSR      BUTFMR
011300970526     C*
011400970526     C* GESTIONE TASTI FUNZIONALI
011500970526     C                   EXSR      FUNCON
011600970526E   2C                   ENDDO
011700970526     C*
011800970526     C* FINE PGM
011900970526     C                   SETON                                        LR
012000970526     C*------------------------------------------------------------------------*
012100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012200970526     C*------------------------------------------------------------------------*
012300970526     C     BUTFMR        BEGSR
012400970526     C*
012500970526     C* EMETTE LA TESTATA
012600160909     C                   WRITE     VRVMSD01
012700970526     C*
012800970526     C* SPEDISCE EVENTUALE MESSAGGIO
012900970526     C                   EXSR      SNDMSG
013000970526     C*
013100970526     C* EMETTE LE FUNZIONI
013200160909     C                   WRITE     VRVMSD02
013300970526     C*
013400970526     C* EMETTE IL VIDEO
013500160909     C                   EXFMT     VRVMSD03
013600970526     C*
013700991214     C* REIMPOSTA FLAG DI ERRORE
013800991214     C                   MOVEL     '0'           ERRL00
013900970526     C*
014000970526     C* REIMPOSTA INDICATORI DI ERRORE
014100020611     C                   SETOFF                                       555556
014200970526     C*
014300970526     C                   ENDSR
014400970526     C*------------------------------------------------------------------------*
014500120927     C* InzD03 - Inizializzo D03
014600970526     C*------------------------------------------------------------------------*
014700120927     C     InzD03        BEGSR
014800120927     C*
014900140117     C                   EVAL      VIDLib = 'GAITRASRC'
015000160909     C                   EVAL      VIDFile= 'SRC'
015100160909     C                   EVAL      VIDMbr = *blank
015200160909     C                   EVAL      VIDTpSk = '1'
015300160909     C                   EVAL      VIDSk1 = 'WCAU'
015400160909     C                   EVAL      VIDSk2 = 'WDCT'
015500160909     C                   EVAL      VIDSk3 = 'WTIS9'
015600160909     C                   EVAL      VIDProc = *blank
015700160909     C                   EVAL      VIDKSU  = *blank
015800160909     C                   EVAL      VIDDDE  = 0
015900160909     C                   EVAL      VIDDSC  = 0
016000130108     C*
016100120927     C                   ENDSR
016200120927     C*------------------------------------------------------------------------*
016300120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016400120927     C*------------------------------------------------------------------------*
016500120927     C     SNDMSG        BEGSR
016600120927     C*
016700970526     C* PULISCE IL SFILE MESSAGGI
016800970526     C                   MOVEL     '1'           *IN40
016900160909     C                   WRITE     VRVMSDMC
017000970526     C                   MOVEL     '0'           *IN40
017100970526     C*
017200970526     C* VISUALIZZA IL MESSAGGIO
017300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017400970526     C                   CALL      'XMSG'        XMSGPA
017500020611     C     DSMSER        IFEQ      *ZEROS
017600970526     C                   MOVEL     DSMSKE        MSGKEY
017700020611     C                   ENDIF
017800970526     C                   Z-ADD     1             MSGULT
017900160909     C                   WRITE     VRVMSDMS
018000160909     C                   WRITE     VRVMSDMC
018100970526     C*
018200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018300970526     C                   MOVEL     *BLANKS       DSMSMS
018400970526     C*
018500970526     C                   ENDSR
018600970526     C*------------------------------------------------------------------------*
018700970526     C* ENTGES - GESTIONE ENTER -> INVIO
018800970526     C*------------------------------------------------------------------------*
018900970526     C     ENTGES        BEGSR
019000970526     C*
019100970526     C* CONTROLLA I CAMPI IMMESSI
019200970526     C                   EXSR      DECODI
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* F03GES - GESTIONE F03 -> FINE
019700970526     C*------------------------------------------------------------------------*
019800970526     C     F03GES        BEGSR
019900970526     C*
020000970526     C* IMPOSTA "PREMUTO F03"
020100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500991214     C* F06GES - GESTIONE F06 -> CONFERMA
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F06GES        BEGSR
020800970526     C*
020900991214     C* CONTROLLA I CAMPI IMMESSI
021000991214     C                   EXSR      DECODI
021100020611     C*
021200991214     C* ESEGUE ROUTINE DI CONFERMA
021300020611     C     ERRL00        IFEQ      '0'
021400991214     C                   EXSR      ESEGUI
021500130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
021600121109     C                   MOVEL     MSG(20)       DSMSMS
021700121109     C* non esco mai dal pgm
021800121109     C***                MOVEL     '1'           FINL00
021900121108     C                   ENDIF
022000970526     C*
022100991214     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300130109     C* F07GES - GESTIONE F07 -> STAMPA
022400970526     C*------------------------------------------------------------------------*
022500130109     C     F07GES        BEGSR
022600970526     C*
022700130110     C*
022800970526     C                   ENDSR
022900160912     C*------------------------------------------------------------------------*
023000160912     C* F08GES - chiama l'anagrafica passando i dati del video
023100160912     C*------------------------------------------------------------------------*
023200160912     C     F08GES        BEGSR
023300160912     C*
023400160912     C* CONTROLLA I CAMPI IMMESSI
023500160912     C                   EXSR      DECODI
023600160912     C*
023700160915     C* chiama l'anagrafica
023800160912     C     ERRL00        IFEQ      '0'
023900160912     C                   clear                   TIS7VMS2DS
024000160912     C                   EVAL      VMS2PROC = VIDProc
024100160913     C                   EVAL      VMS2KSU = VIDKSU
024200160915     C                   EVAL      VMS2DDE = %editc(%dec(DDEISO):'X')
024300160912     C                   MOVEL     'TIS7VMS1R'   WPGM             10
024400160912     C                   EXSR      LANPGM                                       *LANCIA PGM
024500160912     C                   ENDIF
024600160912     C*
024700160912     C                   ENDSR
024800130109     C*------------------------------------------------------------------------*
024900130109     C* F12GES - GESTIONE F12 -> RITORNO
025000130109     C*------------------------------------------------------------------------*
025100130109     C     F12GES        BEGSR
025200130109     C*
025300130109     C* IMPOSTA "PREMUTO F12"
025400130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025500130109     C*
025600130109     C                   ENDSR
025700970526     C*------------------------------------------------------------------------*
025800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025900970526     C*------------------------------------------------------------------------*
026000970526     C     F24GES        BEGSR
026100970526     C*
026200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026300970526     C*
026400970526     C                   ENDSR
026500970526     C*------------------------------------------------------------------------*
026600140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
026700970526     C*------------------------------------------------------------------------*
026800991214     C     ESEGUI        BEGSR
026900140120     C*
027000140120     C* creo il file temporaneo con elenco dei membri
027100140116     C                   EVAL      pIn_Lib     = VIDLib
027200140116     C                   EVAL      pIn_File    = VIDFile
027300140625     C                   EVAL      pIn_Mbr     = VIDMbr
027400160909     c                   EVAL      pIn_TpSk =     VIDTpSk
027500160909     c                   EVAL      pIn_Sk1  =     VIDSk1
027600160909     c                   EVAL      pIn_Sk2  =     VIDSk2
027700160909     c                   EVAL      pIn_Sk3  =     VIDSk3
027800160909     c                   EVAL      pIn_Proc =     VIDProc
027900160909     c                   EVAL      pIn_KSU  =     VIDKSU
028000160912     c                   EVAL      pIn_DDE  =     %dec(DDEISO)
028100160912     c                   EVAL      pIn_DSC  =     %dec(DSCISO)
028200140109     c                   clear                   pOut_Esito
028300121108     C* chiamo
028400160909     C                   CALL      'TNVRVMSC2'
028500140117     c                   parm                    pIn_Lib
028600140117     c                   parm                    pIn_File
028700140625     c                   parm                    pIn_Mbr
028800160909     c                   parm                    pIn_DS_Sel
028900140116     c                   parm                    pOut_Esito
029000121108     C*
029100140109     c                   EVAL      VIDEsito = pOut_Esito
029200140116     C*
029300160909     C* se l'esito è 0 (cioè non ci sono stati errori che hanno bloccato la scrittura)
0294001401171    c                   IF        pOut_Esito = 0
0295001401201-   C                   ENDIF
029600991214     C*
029700970526     C                   ENDSR
029800970526     C*------------------------------------------------------------------------*
029900991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
030000970526     C*------------------------------------------------------------------------*
030100970526     C     DECODI        BEGSR
030200121019     C*
030300120927     C                   SETOFF                                       505152
030400160909     C                   SETOFF                                       535455
030500160909     C                   SETOFF                                       606162
030600160909     C                   SETOFF                                       636465
030700140120     c                   EVAL      VIDMSG    = *blank
030800121001     C*
030900160909     C* Lib, File e Mbr sono obbligatori
031000121001     C     ERRL00        IFEQ      '0'
031100160909     C                   IF        VIDLib = *blank or VIDFile = *blank or
031200160909     C                             VIDMbr = *blank
031300120927     C                   MOVEL     '1'           ERRL00
031400130108     C                   SETON                                        51
031500120927     C                   MOVEL     MSG(3)        DSMSMS
031600120927     C                   ENDIF
031700140116     C                   ENDIF
031800160909     C*
031900160909     C* ID shiere sono obbligatori
032000160909     C     ERRL00        IFEQ      '0'
032100160909     C                   IF        VIDSk1 = *blank
032200160909     C                   MOVEL     '1'           ERRL00
032300160909     C                   SETON                                        52
032400160909     C                   MOVEL     MSG(4)        DSMSMS
032500160909     C                   ENDIF
032600160909     C                   ENDIF
032700160909     C     ERRL00        IFEQ      '0'
032800160909     C                   IF        VIDSk2 = *blank
032900160909     C                   MOVEL     '1'           ERRL00
033000160909     C                   SETON                                        53
033100160909     C                   MOVEL     MSG(4)        DSMSMS
033200160909     C                   ENDIF
033300160909     C                   ENDIF
033400160909     C     ERRL00        IFEQ      '0'
033500160909     C                   IF        VIDSk3 = *blank
033600160909     C                   MOVEL     '1'           ERRL00
033700160909     C                   SETON                                        54
033800160909     C                   MOVEL     MSG(4)        DSMSMS
033900160909     C                   ENDIF
034000160909     C                   ENDIF
034100160909     C*
034200160909     C* Campi chiave TIVMS obbligatori
034300160909     C*
034400160909     C     ERRL00        IFEQ      '0'
034500160909IF  2C                   IF        VIDProc = *blank
034600160909     C                   MOVEL     '1'           ERRL00
034700160909     C                   SETON                                        60
034800160909     C                   MOVEL     MSG(6)        DSMSMS
034900160909E   1C                   ENDIF
035000160909E   1C                   ENDIF
035100160909     C     ERRL00        IFEQ      '0'
035200160909IF  2C                   IF        VIDDDE = 0
035300160909     C                   MOVEL     '1'           ERRL00
035400160909     C                   MOVEL     MSG(6)        DSMSMS
035500160909     C                   SETON                                        61
035600160909E   2C                   ENDIF
035700160909E   1C                   ENDIF
035800160909     C     ERRL00        IFEQ      '0'
035900160909IF  2C                   IF        VIDDSC = 0
036000160909     C                   MOVEL     '1'           ERRL00
036100160909     C                   MOVEL     MSG(6)        DSMSMS
036200160909     C                   SETON                                        62
036300160909E   2C                   ENDIF
036400160909E   1C                   ENDIF
036500160909     C*---
036600160909     C* DECORRENZA
036700160909     C*---
036800160909     C     ERRL00        IFEQ      '0'
036900160909     C                   MONITOR
037000160909     C* se il valore della data è > 6 cifre
037100160909     C                   IF        %len(%trim(%char(VIDDDE))) > 6
037200160909     C* presumo sia scritta in ggmmaaaa
037300160909IF  5C                   EVAL      DDEISO = %date(VIDDDE:*eur)
037400160909     C* se il valore della data è <= 6 cifre
037500160909     C                   ELSE
037600160909     C* presumo sia scritta in ggmmaa
037700160909IF  5C                   EVAL      DDEISO = %date(VIDDDE:*dmy)
037800160909     C                   ENDIF
037900160909IF  2C                   ON-ERROR
038000160909     C                   MOVEL     '1'           ERRL00
038100160909     C                   SETON                                        61
038200160909     C                   MOVEL     MSG(7)        DSMSMS
038300160909     C                   ENDMON
038400160909     C* rivalorizzo il video con la data in ggmmaaaa
038500160909     C                   EVAL      VIDDDE = %dec(DDEISO:*eur)
038600160909E   1C                   ENDIF
038700160909     C*---
038800160909     C* SCADENZA
038900160909     C*---
039000160909     C     ERRL00        IFEQ      '0'
039100160909     C                   MONITOR
039200160909     C* se il valore della data è > 6 cifre
039300160909     C                   IF        %len(%trim(%char(VIDDSC))) > 6
039400160909     C* presumo sia scritta in ggmmaaaa
039500160909IF  5C                   EVAL      DSCISO = %date(VIDDSC:*eur)
039600160909     C* se il valore della data è <= 6 cifre
039700160909     C                   ELSE
039800160909     C* presumo sia scritta in ggmmaa
039900160909IF  5C                   EVAL      DSCISO = %date(VIDDSC:*dmy)
040000160909     C                   ENDIF
040100160909IF  2C                   ON-ERROR
040200160909     C                   MOVEL     '1'           ERRL00
040300160909     C                   SETON                                        62
040400160909     C                   MOVEL     MSG(7)        DSMSMS
040500160909     C                   ENDMON
040600160909     C* rivalorizzo il video con la data in ggmmaaaa
040700160909     C                   EVAL      VIDDSC = %dec(DSCISO:*eur)
040800160909E   1C                   ENDIF
040900160909     C*---
041000160909     C* la SCADENZA deve essere > DECORRENZA (errore se decorrenza >= scadenza)
041100160909     C     ERRL00        IFEQ      '0'
041200160909IF  2C                   IF        DDEISO >= DSCISO
041300160909     C                   SETON                                        6162
041400160909     C                   MOVEL     MSG(8)        DSMSMS
041500160909     C                   MOVEL     '1'           ERRL00
041600160909E   2C                   ENDIF
041700160909E   1C                   ENDIF
041800160909     C* la SCADENZA deve essere >= DATA CORRENTE (errore se scadeza < oggi)
041900160909     C     ERRL00        IFEQ      '0'
042000160909IF  2C                   IF        DSCISO < %date()
042100160909     C                   SETON                                        62
042200160909     C                   MOVEL     MSG(12)       DSMSMS
042300160909     C                   MOVEL     '1'           ERRL00
042400160909E   2C                   ENDIF
042500160909E   1C                   ENDIF
042600160909     C*---
042700160909     C* ACCAVALLAMENTO DATI
042800160916     C* casi di accavallamento
042900160916     C*                   **********
043000160916     C*                +++++       |
043100160916     C*                   ++++     |
043200160916     C*                   |  +++++ |
043300160916     C*                   |     ++++
043400160916     C*                   |      ++++++++
043500160916     C*              +++++++++++++++++++
043600160916     C*                   +++++++++++++++
043700160916     C*            +++++++++++++++++
043800160916     C* casi di NON accavallamento
043900160916     C*                   **********
044000160916     C*          +++++++  |        |
044100160916     C*                   |        |  +++++++
044200160916     C*                   ++++++++++
044300160916     C*
044400160909     C     ERRL00        IFEQ      '0'
044500160909     C     K02VMS01      CHAIN(N)  TIVMS01I                           99
044600160909     C* se non esistono rcd con Procedura/Cliente, allora non ci può essere accavallamento
044700160909DO  2C                   DOW       *in99 = *off
044800160909
044900160909     C* la duplica parte sicuramente da un rcd NON annullato
045000160909     C                   EVAL      VMSPROC   = VIDPROC
045100160909     C                   EVAL      VMSKSU    = VIDKSU
045200160909
045300160916     C* se non accade quanto sotto, siamo in accavallamento
045400160916     C* se la decorrenza a video è > della scadenza del rcd letto     oppure
045500160916IF  5C                   IF        not(
045600160916IF  5C                             DDEISO > %date(VMSDSC)         OR
045700160916     C*    la scadenza a video è < della decorrenza del rcd letto     oppure
045800160916     C                             DSCISO <= %date(VMSDDE)        OR
045900160916     C*    decorrenza e scadenza a video sono le stesse del rcd letto
046000160916     C                             DDEISO = %date(VMSDDE)     AND
046100160916     C                             DSCISO = %date(VMSDSC)
046200160916     C                             )
046300160909     C                   SETON                                        606162
046400160909     C                   SETON                                        63
046500160909     C                   MOVEL     MSG(11)       DSMSMS
046600160909     C                   MOVEL     '1'           ERRL00
046700160909     C                   LEAVE
046800160909E   3C                   ENDIF
046900160909
047000160909     C     K02VMS01      READE(N)  TIVMS01I                               99
047100160909E   2C                   ENDDO
047200160909E   1C                   ENDIF
047300120927     C*
047400970528     C                   ENDSR
047500970526     C*------------------------------------------------------------------------*
047600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
047700970526     C*------------------------------------------------------------------------*
047800970526     C     AZZVID        BEGSR
047900970526     C* CAMPI
048000970526     C*
048100970526     C                   ENDSR
048200970526     C*------------------------------------------------------------------------*
048300020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
048400970526     C*------------------------------------------------------------------------*
048500970526     C     IMPFUN        BEGSR
048600970526     C*
048700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
048800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
048900970526     C                   EXSR      FUNGES
049000970526     C*
049100970526     C                   ENDSR
049200020923     C*--------------------------------------------------------------------------------------------*
049300020923     C* REPERISCE I DATI UTENTE
049400020923     C*--------------------------------------------------------------------------------------------*
049500020923     C     REPDATIUTE    BEGSR
049600020923     C*
049700020923     C* INIZIALIZZA VARIABILI DI WRK
049800020923     C                   CLEAR                   TIBS34DS
049900020923     C                   CLEAR                   AZUTEDS
050000020923     C                   CLEAR                   DDATIUTE
050100020923     C*
050200020923     C     *DTAARA       DEFINE    §azute        azuteds
050300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
050400020923     C                   IN(E)     *DTAARA
050500020923if  1C                   IF        %Error
050600020923     c                   EVAL      I34Tla = 'L'
050700020923     C                   CALL      'TIBS34R'
050800020923     C                   PARM                    Tibs34Ds
050900020923     C                   IN        *DTAARA
051000020923e   1C                   ENDIF
051100020923     C*
051200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
051300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
051400020923     C*
051500020923     C                   ENDSR
051600970526     C*------------------------------------------------------------------------*
051700970526     C* *INZSR - OPERAZIONI INIZIALI
051800970526     C*------------------------------------------------------------------------*
051900970526     C     *INZSR        BEGSR
052000991217     C*
052100991217     C     *ENTRY        PLIST
052200991217     C                   PARM                    KPJBA
052300160909     C*
052400160909     C*--- campi chiave
052500160909     C*
052600160909     C* LETTURA TIVMS01I - parziale
052700160909     C     K02VMS01      KLIST
052800160909     C                   KFLD                    VMSProc
052900160909     C                   KFLD                    VMSKSU
053000020923     C*
053100020923     C* REPERISCE I DATI DELL'UTENTE
053200020923     C                   EXSR      REPDATIUTE
053300970526     C*
053400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
053500970526     C     XMSGPA        PLIST
053600970526     C                   PARM                    DSMSMS
053700970526     C                   PARM                    DSMSPG
053800970526     C                   PARM                    DSMSKE
053900970526     C                   PARM                    DSMSER
054000970526     C*
054100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
054200970526     C                   CALL      'XKEY'
054300970526     C                   PARM                    DSKEY
054400970526     C*---
054500970526     C* VARIABILI NON RIFERITE AL DATA BASE
054600970526     C*---
054700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
054800991214     C                   MOVEL     *BLANKS       DEPOP1            3
054900970526     C*---
055000970526     C* VARIABILI DI CONTROLLO
055100970526     C*---
055200970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
055300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
055500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
055600970526     C*
055700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
055800160909     C                   MOVEL     'TNVRVMSR1'   DSMSPG
055900160909     C                   MOVEL     'TNVRVMSR1'   MSGQUE
056000970526     C                   MOVEL     *BLANKS       MSGKEY
056100970526     C                   MOVEL     *BLANKS       DSMSMS
056200970528     C*
056300970528     C* CALCOLA LA DATA CORRENTE
056400970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
056500970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
056600970528     C                   Z-ADD     WN8           G08DAT
056700970528     C                   Z-ADD     *ZEROS        G08INV
056800970528     C                   MOVEL     '0'           G08ERR
056900970528     C                   CALL      'XSRDA8'
057000970528     C                   PARM                    WLBDA8
057100970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
057200000310     C*
057300000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
057400000310     C                   EXSR      AZZVID
057500970526     C*
057600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
057700970526     C                   EXSR      IMPFUN
057800970526     C*
057900970526     C                   ENDSR
058000970526     C*------------------------------------------------------------------------*
058100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
058200970526     C*------------------------------------------------------------------------*
058300970526     C     FUNGES        BEGSR
058400970526     C*
058500970526     C* PULISCO CAMPI DI LAVORO
058600970526     C                   Z-ADD     0             W                 5 0
058700970526     C*
058800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
058900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
059000970526     C     1             DO        24            I
059100970526     C                   MOVEL     FUD(I)        FUDRIG
059200970526     C*
059300970526     C* PULIZIA FUNZIONI PRECEDENTI
059400970526     C                   MOVEL     *BLANKS       FUV(I)
059500970526     C*
059600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
059700970526     C* (SALTA F24)
059800970526     C     FUDATT        IFEQ      'S'
059900970526     C     FUDFUN        ANDNE     '24'
060000970526     C                   ADD       1             W
060100970526     C                   MOVEL     FUDDES        FUV(W)
060200970526     C                   END
060300970526     C                   END                                                    DO WHILE
060400970526     C*
060500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
060600970526     C* ALTRIMENTI DISATTIVO F24
060700970526     C     1             DO        24            I
060800970526     C                   MOVEL     FUD(I)        FUDRIG
060900970526     C*
061000970526     C     FUDFUN        IFEQ      '24'
061100970526     C     W             IFGT      6
061200970526     C                   MOVEL     'S'           FUDATT
061300970526     C                   ELSE
061400970526     C                   MOVEL     *BLANKS       FUDATT
061500970526     C                   END                                                    W GT 8
061600970526     C                   MOVEL     FUDRIG        FUD(I)
061700970526     C                   END
061800970526     C                   END                                                    DO WHILE
061900970526     C*
062000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
062100970526     C     W             DIV       6             I
062200970526     C                   MVR                     Z
062300970526     C*
062400970526     C     Z             IFNE      0
062500970526     C     I             ADD       1             Z
062600970526     C                   ELSE
062700970526     C                   Z-ADD     I             Z                 5 0
062800970526     C                   END
062900970526     C*
063000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
063100970526     C                   ADD       1             FUNGRU
063200970526     C     FUNGRU        IFGT      Z
063300970526     C     FUNGRU        ORLT      1
063400970526     C                   Z-ADD     1             FUNGRU
063500970526     C                   END
063600970526     C*
063700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
063800970526     C     FUNGRU        IFEQ      1
063900970526     C                   MOVEL     FUV(1)        FUN001
064000970526     C                   MOVEL     FUV(2)        FUN002
064100970526     C                   MOVEL     FUV(3)        FUN003
064200970526     C                   MOVEL     FUV(4)        FUN004
064300970526     C                   MOVEL     FUV(5)        FUN005
064400970526     C                   MOVEL     FUV(6)        FUN006
064500970526     C                   END
064600970526     C*
064700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
064800970526     C     FUNGRU        IFEQ      2
064900970526     C                   MOVEL     FUV(7)        FUN001
065000970526     C                   MOVEL     FUV(8)        FUN002
065100970526     C                   MOVEL     FUV(9)        FUN003
065200970526     C                   MOVEL     FUV(10)       FUN004
065300970526     C                   MOVEL     FUV(11)       FUN005
065400970526     C                   MOVEL     FUV(12)       FUN006
065500970526     C                   END
065600970526     C*
065700970526     C* TERZO GRUPPO DI 6 FUNZIONI
065800970526     C     FUNGRU        IFEQ      3
065900970526     C                   MOVEL     FUV(13)       FUN001
066000970526     C                   MOVEL     FUV(14)       FUN002
066100970526     C                   MOVEL     FUV(15)       FUN003
066200970526     C                   MOVEL     FUV(16)       FUN004
066300970526     C                   MOVEL     FUV(17)       FUN005
066400970526     C                   MOVEL     FUV(18)       FUN006
066500970526     C                   END
066600970526     C*
066700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
066800970526     C     FUNGRU        IFEQ      4
066900970526     C                   MOVEL     FUV(19)       FUN001
067000970526     C                   MOVEL     FUV(20)       FUN002
067100970526     C                   MOVEL     FUV(21)       FUN003
067200970526     C                   MOVEL     FUV(22)       FUN004
067300970526     C                   MOVEL     *BLANKS       FUN005
067400970526     C                   MOVEL     *BLANKS       FUN006
067500970526     C                   END
067600970526     C*
067700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
067800970526     C                   MOVEL     *BLANKS       FUN007
067900970526     C                   MOVEL     *BLANKS       FUN008
068000970526     C*
068100970526     C     1             DO        24            I
068200970526     C                   MOVEL     FUD(I)        FUDRIG
068300970526     C     FUDFUN        IFEQ      '24'
068400970526     C     FUDATT        ANDEQ     'S'
068500970526     C                   MOVEL     FUDDES        FUN008
068600970526     C                   END
068700970526     C                   END                                                    DO
068800970526     C*
068900970526     C                   ENDSR
069000970526     C*------------------------------------------------------------------------*
069100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
069200970526     C*------------------------------------------------------------------------*
069300970526     C     FUNCON        BEGSR
069400970526     C*
069500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
069600970526     C                   EXSR      FUNVAR
069700970526     C*
069800970526     C* PULISCO CAMPI DI LAVORO
069900970526     C                   Z-ADD     0             I                 5 0
070000970526     C                   Z-ADD     0             W                 5 0
070100970526     C*
070200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
070300970526     C     FUNKEY        IFNE      *BLANKS
070400970526     C     1             DO        24            I
070500970526     C                   MOVEL     FUD(I)        FUDRIG
070600970526     C*
070700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
070800970526     C     FUNKEY        IFEQ      FUDFUN
070900970526     C     FUDATT        ANDEQ     'S'
071000970526     C                   Z-ADD     I             W
071100970526     C                   END
071200970526     C                   END                                                    DO WHILE
071300970526     C*
071400970526     C* FUNZIONE NON ATTIVA (ERRORE)
071500970526     C     W             IFEQ      0
071600970526     C*
071700970526     C* EMISSIONE MESSAGGIO DI ERRORE
071800970526     C                   MOVEL     MSG(1)        DSMSMS
071900970526     C*
072000970526     C                   END                                                    W=0
072100970526     C                   END                                                    FUNKEY NE *BLANKS
072200970526     C*
072300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
072400970526     C     W             IFNE      0
072500970526     C     FUNKEY        OREQ      *BLANKS
072600970526     C     FUNKEY        IFEQ      *BLANKS
072700970526     C                   EXSR      ENTGES
072800970526     C                   END
072900970526     C     FUNKEY        IFEQ      '03'
073000970526     C                   EXSR      F03GES
073100970526     C                   END
073200970526     C     FUNKEY        IFEQ      '06'
073300970526     C                   EXSR      F06GES
073400970526     C                   END
073500130109     C     FUNKEY        IFEQ      '07'
073600130109     C                   EXSR      F07GES
073700130109     C                   END
073800160912     C     FUNKEY        IFEQ      '08'
073900160912     C                   EXSR      F08GES
074000160912     C                   END
074100970526     C     FUNKEY        IFEQ      '12'
074200970526     C                   EXSR      F12GES
074300970526     C                   END
074400970526     C     FUNKEY        IFEQ      '24'
074500970526     C                   EXSR      F24GES
074600970526     C                   END
074700970526     C                   END
074800970526     C*
074900970526     C                   ENDSR
075000970526     C*------------------------------------------------------------------------*
075100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
075200970526     C*------------------------------------------------------------------------*
075300970526     C     FUNVAR        BEGSR
075400970526     C*
075500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
075600970526     C*
075700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
075800970526     C     KEY           IFEQ      ENTER
075900970526     C                   MOVEL     *BLANKS       FUNKEY
076000970526     C                   END
076100970526     C     KEY           IFEQ      F1
076200970526     C                   MOVEL     '01'          FUNKEY
076300970526     C                   END
076400970526     C     KEY           IFEQ      F2
076500970526     C                   MOVEL     '02'          FUNKEY
076600970526     C                   END
076700970526     C     KEY           IFEQ      F3
076800970526     C                   MOVEL     '03'          FUNKEY
076900970526     C                   END
077000970526     C     KEY           IFEQ      F4
077100970526     C                   MOVEL     '04'          FUNKEY
077200970526     C                   END
077300970526     C     KEY           IFEQ      F5
077400970526     C                   MOVEL     '05'          FUNKEY
077500970526     C                   END
077600970526     C     KEY           IFEQ      F6
077700970526     C                   MOVEL     '06'          FUNKEY
077800970526     C                   END
077900970526     C     KEY           IFEQ      F7
078000970526     C                   MOVEL     '07'          FUNKEY
078100970526     C                   END
078200970526     C     KEY           IFEQ      F8
078300970526     C                   MOVEL     '08'          FUNKEY
078400970526     C                   END
078500970526     C     KEY           IFEQ      F9
078600970526     C                   MOVEL     '09'          FUNKEY
078700970526     C                   END
078800970526     C     KEY           IFEQ      F10
078900970526     C                   MOVEL     '10'          FUNKEY
079000970526     C                   END
079100970526     C     KEY           IFEQ      F11
079200970526     C                   MOVEL     '11'          FUNKEY
079300970526     C                   END
079400970526     C     KEY           IFEQ      F12
079500970526     C                   MOVEL     '12'          FUNKEY
079600970526     C                   END
079700970526     C     KEY           IFEQ      F13
079800970526     C                   MOVEL     '13'          FUNKEY
079900970526     C                   END
080000970526     C     KEY           IFEQ      F14
080100970526     C                   MOVEL     '14'          FUNKEY
080200970526     C                   END
080300970526     C     KEY           IFEQ      F15
080400970526     C                   MOVEL     '15'          FUNKEY
080500970526     C                   END
080600970526     C     KEY           IFEQ      F16
080700970526     C                   MOVEL     '16'          FUNKEY
080800970526     C                   END
080900970526     C     KEY           IFEQ      F17
081000970526     C                   MOVEL     '17'          FUNKEY
081100970526     C                   END
081200970526     C     KEY           IFEQ      F18
081300970526     C                   MOVEL     '18'          FUNKEY
081400970526     C                   END
081500970526     C     KEY           IFEQ      F19
081600970526     C                   MOVEL     '19'          FUNKEY
081700970526     C                   END
081800970526     C     KEY           IFEQ      F20
081900970526     C                   MOVEL     '20'          FUNKEY
082000970526     C                   END
082100970526     C     KEY           IFEQ      F21
082200970526     C                   MOVEL     '21'          FUNKEY
082300970526     C                   END
082400970526     C     KEY           IFEQ      F22
082500970526     C                   MOVEL     '22'          FUNKEY
082600970526     C                   END
082700970526     C     KEY           IFEQ      F23
082800970526     C                   MOVEL     '23'          FUNKEY
082900970526     C                   END
083000970526     C     KEY           IFEQ      F24
083100970526     C                   MOVEL     '24'          FUNKEY
083200970526     C                   END
083300970526     C     KEY           IFEQ      ROLLUP
083400970526     C                   MOVEL     'RU'          FUNKEY
083500970526     C                   END
083600970526     C     KEY           IFEQ      HELP
083700970526     C                   MOVEL     'HP'          FUNKEY
083800970526     C                   END
083900970526     C                   ENDSR
084000160912     C*------------------------------------------------------------------------*
084100160912     C* LANPGM - LANCIA UN PROGRAMMA
084200160912     C*------------------------------------------------------------------------*
084300160912     C     LANPGM        BEGSR
084400160912     C*
084500160912     C* IMPOSTA LA DS DI PROCEDURA
084600160912     C                   MOVEL     'R02'         VMS2OP0                         *LIVELLO PROCEDURA
084700160912     C                   MOVEL     'O02'         VMS2OP1                         *LIVELLO PGM
084800160912     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
084900160912     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
085000160912     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
085100160912     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
085200160912     C                   EVAL      VMS2ATB    = *blank
085300160912     C                   EVAL      kpjbu      = TIS7VMS2DS
085400160912     C*
085500160912     C                   CALL      WPGM
085600160912     C                   PARM                    KPJBA
085700160912     C***                PARM                    TIS7VMS2DS
085800160912     C*
085900160912     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
086000160912IF  1C     VMS2F03       IFEQ      '1'                                          *PREMUTO F03
086100160912     C                   MOVEL     '1'           ERRL00                         *ERRORE
086200160912     C                   MOVEL     '1'           F03L00                         *FINE PGM
086300160912X   1C                   ELSE                                                   *NON PREMUTO F03
086400160912     C                   MOVEL     VMS2MSG       DSMSMS                         *MESSAGGIO DI RIT
086500160912E   1C                   ENDIF
086600160912     C*
086700160912     C                   ENDSR
086800970526     C*------------------------------------------------------------------------*
086900970526     C* NOTE COSTRUZIONE SCHIERE
087000970526     C*------------------------------------------------------------------------*
087100970526     C*
087200970526     C* -MSG-MESSAGGI
087300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
087400970526     C* (NON TOCCARE MSG,1 E MSG,2)
087500970526     C*
087600970526     C* -CMD-COMANDI
087700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
087800970526     C*
087900970526     C* -OPD-OPZIONI
088000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
088100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
088200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
088300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
088400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
088600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
088700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
088800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
088900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
089000970526     C*
089100970526     C* -FUD-FUNZIONI
089200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
089300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
089400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
089500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
089600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
089700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
089800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
089900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
090000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
090100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
090200970526** MSG - MESSAGGI
090300120927Tasto funzionale non gestito                                                   1
090400160909                                                                               2
090500160912Libreria/file/membro sorgente obbligatori                                      3
090600160909ID schiera obbligatorio                                                        4
090700130108                                                                               5
090800130108Campo obbligatorio                                                             6
090900160909Data errata                                                                    7
091000160909La data scadenza deve essere maggiore della decorrenza                         8
091100120927                                                                               9
091200120927                                                                               10
091300160909La versione non può sovrapporsi ad una già in decorrenza                       11
091400160909La versione non può scadere in un giorno già trascorso                         12
091500120927                                                                               13
091600160909                                                                               14
091700140116                                                                               15
091800120927                                                                               16
091900120927                                                                               17
092000120927                                                                               18
092100120927                                                                               19
092200160909Elaborazione conclusa - controllare l'esito e TIVMS00F                         20
092300160909                                                                               21
092400160909                                                                               22
092500121108                                                                               23
092600121108                                                                               24
092700970526** FUD - FUNZIONI
092800970526                   -        (S/N)=N                        funz.01¬
092900970526                   -        (S/N)=N                        funz.02¬
093000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
093100970526                   -        (S/N)=N                        funz.04¬
093200970526                   -        (S/N)=N                        funz.05¬
093300991214F06=Conferma       -        (S/N)=S                        funz.06¬
093400160912                   -        (S/N)=N                        funz.07¬
093500160912F08=Anagrafica     -        (S/N)=S                        funz.08¬
093600970527                   -        (S/N)=N                        funz.09¬
093700970527                   -        (S/N)=N                        funz.10¬
093800970526                   -        (S/N)=N                        funz.11¬
093900970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
094000970526                   -        (S/N)=N                        funz.13¬
094100970526                   -        (S/N)=N                        funz.14¬
094200970526                   -        (S/N)=N                        funz.15¬
094300970526                   -        (S/N)=N                        funz.16¬
094400970526                   -        (S/N)=N                        funz.17¬
094500970526                   -        (S/N)=N                        funz.18¬
094600970526                   -        (S/N)=N                        funz.19¬
094700970526                   -        (S/N)=N                        funz.20¬
094800970526                   -        (S/N)=N                        funz.21¬
094900970526                   -        (S/N)=N                        funz.22¬
095000970526                   -        (S/N)=N                        funz.23¬
095100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
