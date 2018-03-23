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
008600160909     d  pIn_Proc                           like(VIDProc)
008700160909     d  pIn_KSU                            like(VIDKSU)
008800160909     d  pIn_DDE                            like(VIDDDE)
008900160909     d  pIn_DSC                            like(VIDDSC)
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
023700160912     C* ESEGUE ROUTINE DI CONFERMA
023800160912     C     ERRL00        IFEQ      '0'
023900160912     C                   clear                   TIS7VMS2DS
024000160912     C                   EVAL      VMS2PROC = VIDProc
024100160912     C                   EVAL      VMS2KSU = VIDKSU
024200160912     C                   EVAL      VMS2DDE = VIDDDE
024300160912     C                   EVAL      VMS2DSC = VIDDSC
024400160912     C                   MOVEL     'TIS7VMS2R'   WPGM             10
024500160912     C                   EXSR      LANPGM                                       *LANCIA PGM
024600160912     C                   ENDIF
024700160912     C*
024800160912     C                   ENDSR
024900130109     C*------------------------------------------------------------------------*
025000130109     C* F12GES - GESTIONE F12 -> RITORNO
025100130109     C*------------------------------------------------------------------------*
025200130109     C     F12GES        BEGSR
025300130109     C*
025400130109     C* IMPOSTA "PREMUTO F12"
025500130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025600130109     C*
025700130109     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026000970526     C*------------------------------------------------------------------------*
026100970526     C     F24GES        BEGSR
026200970526     C*
026300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026400970526     C*
026500970526     C                   ENDSR
026600970526     C*------------------------------------------------------------------------*
026700140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
026800970526     C*------------------------------------------------------------------------*
026900991214     C     ESEGUI        BEGSR
027000140120     C*
027100140120     C* creo il file temporaneo con elenco dei membri
027200140116     C                   EVAL      pIn_Lib     = VIDLib
027300140116     C                   EVAL      pIn_File    = VIDFile
027400140625     C                   EVAL      pIn_Mbr     = VIDMbr
027500160909     c                   EVAL      pIn_TpSk =     VIDTpSk
027600160909     c                   EVAL      pIn_Sk1  =     VIDSk1
027700160909     c                   EVAL      pIn_Sk2  =     VIDSk2
027800160909     c                   EVAL      pIn_Sk3  =     VIDSk3
027900160909     c                   EVAL      pIn_Proc =     VIDProc
028000160909     c                   EVAL      pIn_KSU  =     VIDKSU
028100160909     c                   EVAL      pIn_DDE  =     VIDDDE
028200160909     c                   EVAL      pIn_DSC  =     VIDDSC
028300140109     c                   clear                   pOut_Esito
028400121108     C* chiamo
028500160909     C                   CALL      'TNVRVMSC2'
028600140117     c                   parm                    pIn_Lib
028700140117     c                   parm                    pIn_File
028800140625     c                   parm                    pIn_Mbr
028900160909     c                   parm                    pIn_DS_Sel
029000140116     c                   parm                    pOut_Esito
029100121108     C*
029200140109     c                   EVAL      VIDEsito = pOut_Esito
029300140116     C*
029400160909     C* se l'esito è 0 (cioè non ci sono stati errori che hanno bloccato la scrittura)
0295001401171    c                   IF        pOut_Esito = 0
0296001401201-   C                   ENDIF
029700991214     C*
029800970526     C                   ENDSR
029900970526     C*------------------------------------------------------------------------*
030000991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
030100970526     C*------------------------------------------------------------------------*
030200970526     C     DECODI        BEGSR
030300121019     C*
030400120927     C                   SETOFF                                       505152
030500160909     C                   SETOFF                                       535455
030600160909     C                   SETOFF                                       606162
030700160909     C                   SETOFF                                       636465
030800140120     c                   EVAL      VIDMSG    = *blank
030900121001     C*
031000160909     C* Lib, File e Mbr sono obbligatori
031100121001     C     ERRL00        IFEQ      '0'
031200160909     C                   IF        VIDLib = *blank or VIDFile = *blank or
031300160909     C                             VIDMbr = *blank
031400120927     C                   MOVEL     '1'           ERRL00
031500130108     C                   SETON                                        51
031600120927     C                   MOVEL     MSG(3)        DSMSMS
031700120927     C                   ENDIF
031800140116     C                   ENDIF
031900160909     C*
032000160909     C* ID shiere sono obbligatori
032100160909     C     ERRL00        IFEQ      '0'
032200160909     C                   IF        VIDSk1 = *blank
032300160909     C                   MOVEL     '1'           ERRL00
032400160909     C                   SETON                                        52
032500160909     C                   MOVEL     MSG(4)        DSMSMS
032600160909     C                   ENDIF
032700160909     C                   ENDIF
032800160909     C     ERRL00        IFEQ      '0'
032900160909     C                   IF        VIDSk2 = *blank
033000160909     C                   MOVEL     '1'           ERRL00
033100160909     C                   SETON                                        53
033200160909     C                   MOVEL     MSG(4)        DSMSMS
033300160909     C                   ENDIF
033400160909     C                   ENDIF
033500160909     C     ERRL00        IFEQ      '0'
033600160909     C                   IF        VIDSk3 = *blank
033700160909     C                   MOVEL     '1'           ERRL00
033800160909     C                   SETON                                        54
033900160909     C                   MOVEL     MSG(4)        DSMSMS
034000160909     C                   ENDIF
034100160909     C                   ENDIF
034200160909     C*
034300160909     C* Campi chiave TIVMS obbligatori
034400160909     C*
034500160909     C     ERRL00        IFEQ      '0'
034600160909IF  2C                   IF        VIDProc = *blank
034700160909     C                   MOVEL     '1'           ERRL00
034800160909     C                   SETON                                        60
034900160909     C                   MOVEL     MSG(6)        DSMSMS
035000160909E   1C                   ENDIF
035100160909E   1C                   ENDIF
035200160909     C     ERRL00        IFEQ      '0'
035300160909IF  2C                   IF        VIDDDE = 0
035400160909     C                   MOVEL     '1'           ERRL00
035500160909     C                   MOVEL     MSG(6)        DSMSMS
035600160909     C                   SETON                                        61
035700160909E   2C                   ENDIF
035800160909E   1C                   ENDIF
035900160909     C     ERRL00        IFEQ      '0'
036000160909IF  2C                   IF        VIDDSC = 0
036100160909     C                   MOVEL     '1'           ERRL00
036200160909     C                   MOVEL     MSG(6)        DSMSMS
036300160909     C                   SETON                                        62
036400160909E   2C                   ENDIF
036500160909E   1C                   ENDIF
036600160909     C*---
036700160909     C* DECORRENZA
036800160909     C*---
036900160909     C     ERRL00        IFEQ      '0'
037000160909     C                   MONITOR
037100160909     C* se il valore della data è > 6 cifre
037200160909     C                   IF        %len(%trim(%char(VIDDDE))) > 6
037300160909     C* presumo sia scritta in ggmmaaaa
037400160909IF  5C                   EVAL      DDEISO = %date(VIDDDE:*eur)
037500160909     C* se il valore della data è <= 6 cifre
037600160909     C                   ELSE
037700160909     C* presumo sia scritta in ggmmaa
037800160909IF  5C                   EVAL      DDEISO = %date(VIDDDE:*dmy)
037900160909     C                   ENDIF
038000160909IF  2C                   ON-ERROR
038100160909     C                   MOVEL     '1'           ERRL00
038200160909     C                   SETON                                        61
038300160909     C                   MOVEL     MSG(7)        DSMSMS
038400160909     C                   ENDMON
038500160909     C* rivalorizzo il video con la data in ggmmaaaa
038600160909     C                   EVAL      VIDDDE = %dec(DDEISO:*eur)
038700160909E   1C                   ENDIF
038800160909     C*---
038900160909     C* SCADENZA
039000160909     C*---
039100160909     C     ERRL00        IFEQ      '0'
039200160909     C                   MONITOR
039300160909     C* se il valore della data è > 6 cifre
039400160909     C                   IF        %len(%trim(%char(VIDDSC))) > 6
039500160909     C* presumo sia scritta in ggmmaaaa
039600160909IF  5C                   EVAL      DSCISO = %date(VIDDSC:*eur)
039700160909     C* se il valore della data è <= 6 cifre
039800160909     C                   ELSE
039900160909     C* presumo sia scritta in ggmmaa
040000160909IF  5C                   EVAL      DSCISO = %date(VIDDSC:*dmy)
040100160909     C                   ENDIF
040200160909IF  2C                   ON-ERROR
040300160909     C                   MOVEL     '1'           ERRL00
040400160909     C                   SETON                                        62
040500160909     C                   MOVEL     MSG(7)        DSMSMS
040600160909     C                   ENDMON
040700160909     C* rivalorizzo il video con la data in ggmmaaaa
040800160909     C                   EVAL      VIDDSC = %dec(DSCISO:*eur)
040900160909E   1C                   ENDIF
041000160909     C*---
041100160909     C* la SCADENZA deve essere > DECORRENZA (errore se decorrenza >= scadenza)
041200160909     C     ERRL00        IFEQ      '0'
041300160909IF  2C                   IF        DDEISO >= DSCISO
041400160909     C                   SETON                                        6162
041500160909     C                   MOVEL     MSG(8)        DSMSMS
041600160909     C                   MOVEL     '1'           ERRL00
041700160909E   2C                   ENDIF
041800160909E   1C                   ENDIF
041900160909     C* la SCADENZA deve essere >= DATA CORRENTE (errore se scadeza < oggi)
042000160909     C     ERRL00        IFEQ      '0'
042100160909IF  2C                   IF        DSCISO < %date()
042200160909     C                   SETON                                        62
042300160909     C                   MOVEL     MSG(12)       DSMSMS
042400160909     C                   MOVEL     '1'           ERRL00
042500160909E   2C                   ENDIF
042600160909E   1C                   ENDIF
042700160909     C*---
042800160909     C* ACCAVALLAMENTO DATI
042900160909     C     ERRL00        IFEQ      '0'
043000160909     C     K02VMS01      CHAIN(N)  TIVMS01I                           99
043100160909     C* se non esistono rcd con Procedura/Cliente, allora non ci può essere accavallamento
043200160909DO  2C                   DOW       *in99 = *off
043300160909
043400160909     C* la duplica parte sicuramente da un rcd NON annullato
043500160909     C                   EVAL      VMSPROC   = VIDPROC
043600160909     C                   EVAL      VMSKSU    = VIDKSU
043700160909
043800160909     C* se la decorrenza a video è > della decorrenza del rcd letto
043900160909IF  3C                   IF        DDEISO > %date(VMSDDE)      AND
044000160909     C*  E la decorrenza a video è <= alla scadenza del rcd letto
044100160909     C                             DDEISO <= %date(VMSDSC)        OR
044200160909     C* oppure la scadenza a video è > della decorrenza del rcd letto
044300160909     C                             DSCISO > %date(VMSDDE)      AND
044400160909     C*  E la scadenza   a video è <= della scadenza del rcd letto
044500160909     C                             DSCISO <= %date(VMSDSC)        OR
044600160909     C* oppure la decorrenza del rcd letto è > della decorrenza a video
044700160909     C                             %date(VMSDDE) > DDEISO      AND
044800160909     C*  E la scadenza del rcd letto è <= della scadenza a video
044900160909     C                             %date(VMSDSC) <= DSCISO
045000160909     C                   SETON                                        606162
045100160909     C                   SETON                                        63
045200160909     C                   MOVEL     MSG(11)       DSMSMS
045300160909     C                   MOVEL     '1'           ERRL00
045400160909     C                   LEAVE
045500160909E   3C                   ENDIF
045600160909
045700160909     C     K02VMS01      READE(N)  TIVMS01I                               99
045800160909E   2C                   ENDDO
045900160909E   1C                   ENDIF
046000120927     C*
046100970528     C                   ENDSR
046200970526     C*------------------------------------------------------------------------*
046300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
046400970526     C*------------------------------------------------------------------------*
046500970526     C     AZZVID        BEGSR
046600970526     C* CAMPI
046700970526     C*
046800970526     C                   ENDSR
046900970526     C*------------------------------------------------------------------------*
047000020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
047100970526     C*------------------------------------------------------------------------*
047200970526     C     IMPFUN        BEGSR
047300970526     C*
047400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
047500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
047600970526     C                   EXSR      FUNGES
047700970526     C*
047800970526     C                   ENDSR
047900020923     C*--------------------------------------------------------------------------------------------*
048000020923     C* REPERISCE I DATI UTENTE
048100020923     C*--------------------------------------------------------------------------------------------*
048200020923     C     REPDATIUTE    BEGSR
048300020923     C*
048400020923     C* INIZIALIZZA VARIABILI DI WRK
048500020923     C                   CLEAR                   TIBS34DS
048600020923     C                   CLEAR                   AZUTEDS
048700020923     C                   CLEAR                   DDATIUTE
048800020923     C*
048900020923     C     *DTAARA       DEFINE    §azute        azuteds
049000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
049100020923     C                   IN(E)     *DTAARA
049200020923if  1C                   IF        %Error
049300020923     c                   EVAL      I34Tla = 'L'
049400020923     C                   CALL      'TIBS34R'
049500020923     C                   PARM                    Tibs34Ds
049600020923     C                   IN        *DTAARA
049700020923e   1C                   ENDIF
049800020923     C*
049900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
050000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
050100020923     C*
050200020923     C                   ENDSR
050300970526     C*------------------------------------------------------------------------*
050400970526     C* *INZSR - OPERAZIONI INIZIALI
050500970526     C*------------------------------------------------------------------------*
050600970526     C     *INZSR        BEGSR
050700991217     C*
050800991217     C     *ENTRY        PLIST
050900991217     C                   PARM                    KPJBA
051000160909     C*
051100160909     C*--- campi chiave
051200160909     C*
051300160909     C* LETTURA TIVMS01I - parziale
051400160909     C     K02VMS01      KLIST
051500160909     C                   KFLD                    VMSProc
051600160909     C                   KFLD                    VMSKSU
051700020923     C*
051800020923     C* REPERISCE I DATI DELL'UTENTE
051900020923     C                   EXSR      REPDATIUTE
052000970526     C*
052100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
052200970526     C     XMSGPA        PLIST
052300970526     C                   PARM                    DSMSMS
052400970526     C                   PARM                    DSMSPG
052500970526     C                   PARM                    DSMSKE
052600970526     C                   PARM                    DSMSER
052700970526     C*
052800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
052900970526     C                   CALL      'XKEY'
053000970526     C                   PARM                    DSKEY
053100970526     C*---
053200970526     C* VARIABILI NON RIFERITE AL DATA BASE
053300970526     C*---
053400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
053500991214     C                   MOVEL     *BLANKS       DEPOP1            3
053600970526     C*---
053700970526     C* VARIABILI DI CONTROLLO
053800970526     C*---
053900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
054000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
054100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
054200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
054300970526     C*
054400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
054500160909     C                   MOVEL     'TNVRVMSR1'   DSMSPG
054600160909     C                   MOVEL     'TNVRVMSR1'   MSGQUE
054700970526     C                   MOVEL     *BLANKS       MSGKEY
054800970526     C                   MOVEL     *BLANKS       DSMSMS
054900970528     C*
055000970528     C* CALCOLA LA DATA CORRENTE
055100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
055200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
055300970528     C                   Z-ADD     WN8           G08DAT
055400970528     C                   Z-ADD     *ZEROS        G08INV
055500970528     C                   MOVEL     '0'           G08ERR
055600970528     C                   CALL      'XSRDA8'
055700970528     C                   PARM                    WLBDA8
055800970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
055900000310     C*
056000000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
056100000310     C                   EXSR      AZZVID
056200970526     C*
056300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
056400970526     C                   EXSR      IMPFUN
056500970526     C*
056600970526     C                   ENDSR
056700970526     C*------------------------------------------------------------------------*
056800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
056900970526     C*------------------------------------------------------------------------*
057000970526     C     FUNGES        BEGSR
057100970526     C*
057200970526     C* PULISCO CAMPI DI LAVORO
057300970526     C                   Z-ADD     0             W                 5 0
057400970526     C*
057500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
057600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
057700970526     C     1             DO        24            I
057800970526     C                   MOVEL     FUD(I)        FUDRIG
057900970526     C*
058000970526     C* PULIZIA FUNZIONI PRECEDENTI
058100970526     C                   MOVEL     *BLANKS       FUV(I)
058200970526     C*
058300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
058400970526     C* (SALTA F24)
058500970526     C     FUDATT        IFEQ      'S'
058600970526     C     FUDFUN        ANDNE     '24'
058700970526     C                   ADD       1             W
058800970526     C                   MOVEL     FUDDES        FUV(W)
058900970526     C                   END
059000970526     C                   END                                                    DO WHILE
059100970526     C*
059200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
059300970526     C* ALTRIMENTI DISATTIVO F24
059400970526     C     1             DO        24            I
059500970526     C                   MOVEL     FUD(I)        FUDRIG
059600970526     C*
059700970526     C     FUDFUN        IFEQ      '24'
059800970526     C     W             IFGT      6
059900970526     C                   MOVEL     'S'           FUDATT
060000970526     C                   ELSE
060100970526     C                   MOVEL     *BLANKS       FUDATT
060200970526     C                   END                                                    W GT 8
060300970526     C                   MOVEL     FUDRIG        FUD(I)
060400970526     C                   END
060500970526     C                   END                                                    DO WHILE
060600970526     C*
060700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
060800970526     C     W             DIV       6             I
060900970526     C                   MVR                     Z
061000970526     C*
061100970526     C     Z             IFNE      0
061200970526     C     I             ADD       1             Z
061300970526     C                   ELSE
061400970526     C                   Z-ADD     I             Z                 5 0
061500970526     C                   END
061600970526     C*
061700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
061800970526     C                   ADD       1             FUNGRU
061900970526     C     FUNGRU        IFGT      Z
062000970526     C     FUNGRU        ORLT      1
062100970526     C                   Z-ADD     1             FUNGRU
062200970526     C                   END
062300970526     C*
062400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
062500970526     C     FUNGRU        IFEQ      1
062600970526     C                   MOVEL     FUV(1)        FUN001
062700970526     C                   MOVEL     FUV(2)        FUN002
062800970526     C                   MOVEL     FUV(3)        FUN003
062900970526     C                   MOVEL     FUV(4)        FUN004
063000970526     C                   MOVEL     FUV(5)        FUN005
063100970526     C                   MOVEL     FUV(6)        FUN006
063200970526     C                   END
063300970526     C*
063400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
063500970526     C     FUNGRU        IFEQ      2
063600970526     C                   MOVEL     FUV(7)        FUN001
063700970526     C                   MOVEL     FUV(8)        FUN002
063800970526     C                   MOVEL     FUV(9)        FUN003
063900970526     C                   MOVEL     FUV(10)       FUN004
064000970526     C                   MOVEL     FUV(11)       FUN005
064100970526     C                   MOVEL     FUV(12)       FUN006
064200970526     C                   END
064300970526     C*
064400970526     C* TERZO GRUPPO DI 6 FUNZIONI
064500970526     C     FUNGRU        IFEQ      3
064600970526     C                   MOVEL     FUV(13)       FUN001
064700970526     C                   MOVEL     FUV(14)       FUN002
064800970526     C                   MOVEL     FUV(15)       FUN003
064900970526     C                   MOVEL     FUV(16)       FUN004
065000970526     C                   MOVEL     FUV(17)       FUN005
065100970526     C                   MOVEL     FUV(18)       FUN006
065200970526     C                   END
065300970526     C*
065400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
065500970526     C     FUNGRU        IFEQ      4
065600970526     C                   MOVEL     FUV(19)       FUN001
065700970526     C                   MOVEL     FUV(20)       FUN002
065800970526     C                   MOVEL     FUV(21)       FUN003
065900970526     C                   MOVEL     FUV(22)       FUN004
066000970526     C                   MOVEL     *BLANKS       FUN005
066100970526     C                   MOVEL     *BLANKS       FUN006
066200970526     C                   END
066300970526     C*
066400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
066500970526     C                   MOVEL     *BLANKS       FUN007
066600970526     C                   MOVEL     *BLANKS       FUN008
066700970526     C*
066800970526     C     1             DO        24            I
066900970526     C                   MOVEL     FUD(I)        FUDRIG
067000970526     C     FUDFUN        IFEQ      '24'
067100970526     C     FUDATT        ANDEQ     'S'
067200970526     C                   MOVEL     FUDDES        FUN008
067300970526     C                   END
067400970526     C                   END                                                    DO
067500970526     C*
067600970526     C                   ENDSR
067700970526     C*------------------------------------------------------------------------*
067800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
067900970526     C*------------------------------------------------------------------------*
068000970526     C     FUNCON        BEGSR
068100970526     C*
068200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
068300970526     C                   EXSR      FUNVAR
068400970526     C*
068500970526     C* PULISCO CAMPI DI LAVORO
068600970526     C                   Z-ADD     0             I                 5 0
068700970526     C                   Z-ADD     0             W                 5 0
068800970526     C*
068900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
069000970526     C     FUNKEY        IFNE      *BLANKS
069100970526     C     1             DO        24            I
069200970526     C                   MOVEL     FUD(I)        FUDRIG
069300970526     C*
069400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
069500970526     C     FUNKEY        IFEQ      FUDFUN
069600970526     C     FUDATT        ANDEQ     'S'
069700970526     C                   Z-ADD     I             W
069800970526     C                   END
069900970526     C                   END                                                    DO WHILE
070000970526     C*
070100970526     C* FUNZIONE NON ATTIVA (ERRORE)
070200970526     C     W             IFEQ      0
070300970526     C*
070400970526     C* EMISSIONE MESSAGGIO DI ERRORE
070500970526     C                   MOVEL     MSG(1)        DSMSMS
070600970526     C*
070700970526     C                   END                                                    W=0
070800970526     C                   END                                                    FUNKEY NE *BLANKS
070900970526     C*
071000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
071100970526     C     W             IFNE      0
071200970526     C     FUNKEY        OREQ      *BLANKS
071300970526     C     FUNKEY        IFEQ      *BLANKS
071400970526     C                   EXSR      ENTGES
071500970526     C                   END
071600970526     C     FUNKEY        IFEQ      '03'
071700970526     C                   EXSR      F03GES
071800970526     C                   END
071900970526     C     FUNKEY        IFEQ      '06'
072000970526     C                   EXSR      F06GES
072100970526     C                   END
072200130109     C     FUNKEY        IFEQ      '07'
072300130109     C                   EXSR      F07GES
072400130109     C                   END
072500160912     C     FUNKEY        IFEQ      '08'
072600160912     C                   EXSR      F08GES
072700160912     C                   END
072800970526     C     FUNKEY        IFEQ      '12'
072900970526     C                   EXSR      F12GES
073000970526     C                   END
073100970526     C     FUNKEY        IFEQ      '24'
073200970526     C                   EXSR      F24GES
073300970526     C                   END
073400970526     C                   END
073500970526     C*
073600970526     C                   ENDSR
073700970526     C*------------------------------------------------------------------------*
073800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
073900970526     C*------------------------------------------------------------------------*
074000970526     C     FUNVAR        BEGSR
074100970526     C*
074200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
074300970526     C*
074400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
074500970526     C     KEY           IFEQ      ENTER
074600970526     C                   MOVEL     *BLANKS       FUNKEY
074700970526     C                   END
074800970526     C     KEY           IFEQ      F1
074900970526     C                   MOVEL     '01'          FUNKEY
075000970526     C                   END
075100970526     C     KEY           IFEQ      F2
075200970526     C                   MOVEL     '02'          FUNKEY
075300970526     C                   END
075400970526     C     KEY           IFEQ      F3
075500970526     C                   MOVEL     '03'          FUNKEY
075600970526     C                   END
075700970526     C     KEY           IFEQ      F4
075800970526     C                   MOVEL     '04'          FUNKEY
075900970526     C                   END
076000970526     C     KEY           IFEQ      F5
076100970526     C                   MOVEL     '05'          FUNKEY
076200970526     C                   END
076300970526     C     KEY           IFEQ      F6
076400970526     C                   MOVEL     '06'          FUNKEY
076500970526     C                   END
076600970526     C     KEY           IFEQ      F7
076700970526     C                   MOVEL     '07'          FUNKEY
076800970526     C                   END
076900970526     C     KEY           IFEQ      F8
077000970526     C                   MOVEL     '08'          FUNKEY
077100970526     C                   END
077200970526     C     KEY           IFEQ      F9
077300970526     C                   MOVEL     '09'          FUNKEY
077400970526     C                   END
077500970526     C     KEY           IFEQ      F10
077600970526     C                   MOVEL     '10'          FUNKEY
077700970526     C                   END
077800970526     C     KEY           IFEQ      F11
077900970526     C                   MOVEL     '11'          FUNKEY
078000970526     C                   END
078100970526     C     KEY           IFEQ      F12
078200970526     C                   MOVEL     '12'          FUNKEY
078300970526     C                   END
078400970526     C     KEY           IFEQ      F13
078500970526     C                   MOVEL     '13'          FUNKEY
078600970526     C                   END
078700970526     C     KEY           IFEQ      F14
078800970526     C                   MOVEL     '14'          FUNKEY
078900970526     C                   END
079000970526     C     KEY           IFEQ      F15
079100970526     C                   MOVEL     '15'          FUNKEY
079200970526     C                   END
079300970526     C     KEY           IFEQ      F16
079400970526     C                   MOVEL     '16'          FUNKEY
079500970526     C                   END
079600970526     C     KEY           IFEQ      F17
079700970526     C                   MOVEL     '17'          FUNKEY
079800970526     C                   END
079900970526     C     KEY           IFEQ      F18
080000970526     C                   MOVEL     '18'          FUNKEY
080100970526     C                   END
080200970526     C     KEY           IFEQ      F19
080300970526     C                   MOVEL     '19'          FUNKEY
080400970526     C                   END
080500970526     C     KEY           IFEQ      F20
080600970526     C                   MOVEL     '20'          FUNKEY
080700970526     C                   END
080800970526     C     KEY           IFEQ      F21
080900970526     C                   MOVEL     '21'          FUNKEY
081000970526     C                   END
081100970526     C     KEY           IFEQ      F22
081200970526     C                   MOVEL     '22'          FUNKEY
081300970526     C                   END
081400970526     C     KEY           IFEQ      F23
081500970526     C                   MOVEL     '23'          FUNKEY
081600970526     C                   END
081700970526     C     KEY           IFEQ      F24
081800970526     C                   MOVEL     '24'          FUNKEY
081900970526     C                   END
082000970526     C     KEY           IFEQ      ROLLUP
082100970526     C                   MOVEL     'RU'          FUNKEY
082200970526     C                   END
082300970526     C     KEY           IFEQ      HELP
082400970526     C                   MOVEL     'HP'          FUNKEY
082500970526     C                   END
082600970526     C                   ENDSR
082700160912     C*------------------------------------------------------------------------*
082800160912     C* LANPGM - LANCIA UN PROGRAMMA
082900160912     C*------------------------------------------------------------------------*
083000160912     C     LANPGM        BEGSR
083100160912     C*
083200160912     C* IMPOSTA LA DS DI PROCEDURA
083300160912     C                   MOVEL     *blank        VMS2OP0                         *LIVELLO PROCEDURA
083400160912     C                   MOVEL     *blank        VMS2OP1                         *LIVELLO PGM
083500160912     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
083600160912     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
083700160912     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
083800160912     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
083900160912     C                   EVAL      VMS2ATB    = *blank
084000160912     C*
084100160912     C                   CALL      WPGM
084200160912     C                   PARM                    KPJBA
084300160912     C                   PARM                    TIS7VMS2DS
084400160912     C*
084500160912     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
084600160912IF  1C     VMS2F03       IFEQ      '1'                                          *PREMUTO F03
084700160912     C                   MOVEL     '1'           ERRL00                         *ERRORE
084800160912     C                   MOVEL     '1'           F03L00                         *FINE PGM
084900160912X   1C                   ELSE                                                   *NON PREMUTO F03
085000160912     C                   MOVEL     VMS2MSG       DSMSMS                         *MESSAGGIO DI RIT
085100160912E   1C                   ENDIF
085200160912     C*
085300160912     C                   ENDSR
085400970526     C*------------------------------------------------------------------------*
085500970526     C* NOTE COSTRUZIONE SCHIERE
085600970526     C*------------------------------------------------------------------------*
085700970526     C*
085800970526     C* -MSG-MESSAGGI
085900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
086000970526     C* (NON TOCCARE MSG,1 E MSG,2)
086100970526     C*
086200970526     C* -CMD-COMANDI
086300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
086400970526     C*
086500970526     C* -OPD-OPZIONI
086600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
086700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
086800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
086900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
087000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
087100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
087200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
087300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
087400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
087500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
087600970526     C*
087700970526     C* -FUD-FUNZIONI
087800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
087900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
088000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
088200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
088300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
088400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
088500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
088600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
088700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
088800970526** MSG - MESSAGGI
088900120927Tasto funzionale non gestito                                                   1
089000160909                                                                               2
089100140625Libreria/file sorgente obbligatori                                             3
089200160909ID schiera obbligatorio                                                        4
089300130108                                                                               5
089400130108Campo obbligatorio                                                             6
089500160909Data errata                                                                    7
089600160909La data scadenza deve essere maggiore della decorrenza                         8
089700120927                                                                               9
089800120927                                                                               10
089900160909La versione non può sovrapporsi ad una già in decorrenza                       11
090000160909La versione non può scadere in un giorno già trascorso                         12
090100120927                                                                               13
090200160909                                                                               14
090300140116                                                                               15
090400120927                                                                               16
090500120927                                                                               17
090600120927                                                                               18
090700120927                                                                               19
090800160909Elaborazione conclusa - controllare l'esito e TIVMS00F                         20
090900160909                                                                               21
091000160909                                                                               22
091100121108                                                                               23
091200121108                                                                               24
091300970526** FUD - FUNZIONI
091400970526                   -        (S/N)=N                        funz.01¬
091500970526                   -        (S/N)=N                        funz.02¬
091600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
091700970526                   -        (S/N)=N                        funz.04¬
091800970526                   -        (S/N)=N                        funz.05¬
091900991214F06=Conferma       -        (S/N)=S                        funz.06¬
092000160912                   -        (S/N)=N                        funz.07¬
092100160912F08=Anagrafica     -        (S/N)=S                        funz.08¬
092200970527                   -        (S/N)=N                        funz.09¬
092300970527                   -        (S/N)=N                        funz.10¬
092400970526                   -        (S/N)=N                        funz.11¬
092500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
092600970526                   -        (S/N)=N                        funz.13¬
092700970526                   -        (S/N)=N                        funz.14¬
092800970526                   -        (S/N)=N                        funz.15¬
092900970526                   -        (S/N)=N                        funz.16¬
093000970526                   -        (S/N)=N                        funz.17¬
093100970526                   -        (S/N)=N                        funz.18¬
093200970526                   -        (S/N)=N                        funz.19¬
093300970526                   -        (S/N)=N                        funz.20¬
093400970526                   -        (S/N)=N                        funz.21¬
093500970526                   -        (S/N)=N                        funz.22¬
093600970526                   -        (S/N)=N                        funz.23¬
093700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
