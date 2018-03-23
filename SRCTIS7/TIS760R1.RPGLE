000100970526     H*------------------------------------------------------------------------*
000200140627     H* Lancio verifica log upload in errore
000300970526     H*------------------------------------------------------------------------*
000400140627     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500140627     h dftactgrp(*no) bnddir('UBBNDDIR')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900140627     FTIS760D1  cf   e             WORKSTN INFDS(DEVDS1)
001000140627     F                                     SFILE(S760DMS:MSGULT)
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
006900140630     D*-------------------
007000140630     D* TABELLE "XCLIR"- RICERCA CLIENTI
007100140630     D ksc             s              7  0 dim(30)
007200140630     D ksa             s              4    dim(30)
007300140630     D isvkun          s              2    dim(100)
007400140630     D isvtt           s              2    dim(100)
007500140630     D isvftp          s              2    dim(100)
007600140630     D isvKEYS         s              2    dim(100)
007700140630     D isvCTRL         s              4    dim(100)
007800140630     D isvPGMAFT       s              3    dim(100)
007900140630     D*-------------------
008000140630     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
008100140630     D*-------------------
008200140630     DDSXC           e DS                  extname(XCLIRDS)
008300140630     D*-------------------
008400140630     D                 DS
008500140630     D dsksn                   1      4p 0
008600140630     D dsksa                   1      4
008700140630     D*-------------------
008800140630     D TIBS02DS      E DS
008900120928     D*-------------------
009000120928
009100121003     d Status         sds
009200121003     d  SDSpgm           *proc
009300121003     d  SDSprm           *parms
009400121003     d  SDSuser              254    263
009500121003
009600140627     d DS760R2         ds
009700140701     d pIn_DatDa                      8s 0
009800140701     d pIn_DatA                       8s 0
009900140701     d pIn_Ksc                        8
010000140701     d pIn_Tip                        2
010100140701     d pIn_Perc                       3s 0
010200140702     d pIn_Log                        3s 0
010300140701     d pIn_Modo                       1
010400140701     d pOut_Esito                     2  0
010500130116
010600130116     d ora             s               t
010700140117     d wX              s              2s 0
010800140627     d wData08         s              8s 0
010900140627     d wDataDa         s              8s 0
011000140627     d wDataA          s              8s 0
011100120928
011200140627      /COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
011300140627
011400970526     C*------------------------------------------------------------------------*
011500000303     C* MAIN LINES
011600970526     C*------------------------------------------------------------------------*
011700970526     C*
011800120927     C* metto i default su D03
011900120927     C                   EXSR      InzD03
012000120927     C*
012100120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012200991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012500970526     C*
012600970526     C* EMETTE FORMATO VIDEO
012700970526     C                   EXSR      BUTFMR
012800970526     C*
012900970526     C* GESTIONE TASTI FUNZIONALI
013000970526     C                   EXSR      FUNCON
013100970526E   2C                   ENDDO
013200970526     C*
013300970526     C* FINE PGM
013400970526     C                   SETON                                        LR
013500970526     C*------------------------------------------------------------------------*
013600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013700970526     C*------------------------------------------------------------------------*
013800970526     C     BUTFMR        BEGSR
013900970526     C*
014000970526     C* EMETTE LA TESTATA
014100140627     C                   WRITE     S760D01
014200970526     C*
014300970526     C* SPEDISCE EVENTUALE MESSAGGIO
014400970526     C                   EXSR      SNDMSG
014500970526     C*
014600970526     C* EMETTE LE FUNZIONI
014700140627     C                   WRITE     S760D02
014800970526     C*
014900970526     C* EMETTE IL VIDEO
015000140627     C                   EXFMT     S760D03
015100970526     C*
015200991214     C* REIMPOSTA FLAG DI ERRORE
015300991214     C                   MOVEL     '0'           ERRL00
015400970526     C*
015500970526     C* REIMPOSTA INDICATORI DI ERRORE
015600140702     C                   SETOFF                                       515253
015700140702     C                   SETOFF                                       545556
015800970526     C*
015900970526     C                   ENDSR
016000970526     C*------------------------------------------------------------------------*
016100120927     C* InzD03 - Inizializzo D03
016200970526     C*------------------------------------------------------------------------*
016300120927     C     InzD03        BEGSR
016400120927     C*
016500140627     C                   EVAL      VIDDataDa = 0
016600140627     C                   EVAL      VIDDataA = 0
016700140627     C                   EVAL      VIDKsc= *blank
016800140627     C                   EVAL      VIDTip= *blank
016900140627     C                   EVAL      VIDPerc = 0
017000130108     C*
017100120927     C                   ENDSR
017200120927     C*------------------------------------------------------------------------*
017300120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017400120927     C*------------------------------------------------------------------------*
017500120927     C     SNDMSG        BEGSR
017600120927     C*
017700970526     C* PULISCE IL SFILE MESSAGGI
017800970526     C                   MOVEL     '1'           *IN40
017900140627     C                   WRITE     S760DMC
018000970526     C                   MOVEL     '0'           *IN40
018100970526     C*
018200970526     C* VISUALIZZA IL MESSAGGIO
018300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018400970526     C                   CALL      'XMSG'        XMSGPA
018500020611     C     DSMSER        IFEQ      *ZEROS
018600970526     C                   MOVEL     DSMSKE        MSGKEY
018700020611     C                   ENDIF
018800970526     C                   Z-ADD     1             MSGULT
018900140627     C                   WRITE     S760DMS
019000140627     C                   WRITE     S760DMC
019100970526     C*
019200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019300970526     C                   MOVEL     *BLANKS       DSMSMS
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* ENTGES - GESTIONE ENTER -> INVIO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     ENTGES        BEGSR
020000970526     C*
020100970526     C* CONTROLLA I CAMPI IMMESSI
020200970526     C                   EXSR      DECODI
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F03GES - GESTIONE F03 -> FINE
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F03GES        BEGSR
020900970526     C*
021000970526     C* IMPOSTA "PREMUTO F03"
021100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500140701     C* F06GES - GESTIONE F06 -> Estrazione interattiva
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F06GES        BEGSR
021800970526     C*
021900991214     C* CONTROLLA I CAMPI IMMESSI
022000991214     C                   EXSR      DECODI
022100020611     C*
022200991214     C* ESEGUE ROUTINE DI CONFERMA
022300020611     C     ERRL00        IFEQ      '0'
022400140701     C* interattivo
022500140701     C                   EVAL      pIn_Modo    = 'I'
022600991214     C                   EXSR      ESEGUI
022700140702     C* il messaggio di fine lo do sempre, a prescindere dagli esiti
022800121109     C                   MOVEL     MSG(20)       DSMSMS
022900121109     C* non esco mai dal pgm
023000121109     C***                MOVEL     '1'           FINL00
023100121108     C                   ENDIF
023200970526     C*
023300991214     C                   ENDSR
023400140701     C*------------------------------------------------------------------------*
023500140701     C* F07GES - GESTIONE F07 -> Estrazione in batch
023600140701     C*------------------------------------------------------------------------*
023700140701     C     F07GES        BEGSR
023800140701     C*
023900140701     C* CONTROLLA I CAMPI IMMESSI
024000140701     C                   EXSR      DECODI
024100140701     C*
024200140701     C* ESEGUE ROUTINE DI CONFERMA
024300140701     C     ERRL00        IFEQ      '0'
024400140701     C* batch
024500140701     C                   EVAL      pIn_Modo    = 'B'
024600140701     C                   EXSR      ESEGUI
024700140702     C* il messaggio di fine lo do sempre, a prescindere dagli esiti
024800140701     C                   MOVEL     MSG(20)       DSMSMS
024900140701     C* esco dal pgm
025000140701     C                   MOVEL     '1'           FINL00
025100140701     C                   ENDIF
025200140701     C*
025300140701     C                   ENDSR
025400130109     C*------------------------------------------------------------------------*
025500130109     C* F12GES - GESTIONE F12 -> RITORNO
025600130109     C*------------------------------------------------------------------------*
025700130109     C     F12GES        BEGSR
025800130109     C*
025900130109     C* IMPOSTA "PREMUTO F12"
026000130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026100130109     C*
026200130109     C                   ENDSR
026300970526     C*------------------------------------------------------------------------*
026400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026500970526     C*------------------------------------------------------------------------*
026600970526     C     F24GES        BEGSR
026700970526     C*
026800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026900970526     C*
027000970526     C                   ENDSR
027100970526     C*------------------------------------------------------------------------*
027200140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
027300970526     C*------------------------------------------------------------------------*
027400991214     C     ESEGUI        BEGSR
027500140120     C*
027600140627     C                   EVAL      pIn_DatDa   = wDataDa
027700140627     C                   EVAL      pIn_DatA    = wDataA
027800140627     C                   EVAL      pIn_Ksc     = VIDKsc
027900140627     C                   EVAL      pIn_Tip     = VIDTip
028000140627     C                   EVAL      pIn_Perc    = VIDPerc
028100140702     C                   EVAL      pIn_Log     = VIDLog
028200140627     C                   EVAL      KPJBU = DS760R2
028300140701     C*
028400140702     C* interattivo: estraggo i dati scrivendoli su WFLUE00F e mostro il risultato
028500140702     C* batch      : estraggo i dati scrivendoli su WFLUE00F e li spedisco al CED
028600140630     C                   CALL      'TIS760R3'
028700140701     C                   parm                    KPJBA
028800991214     C*
028900970526     C                   ENDSR
029000970526     C*------------------------------------------------------------------------*
029100991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029200970526     C*------------------------------------------------------------------------*
029300970526     C     DECODI        BEGSR
029400121019     C*
029500140702     C                   SETOFF                                       515253
029600140702     C                   SETOFF                                       545556
029700140627     C                   EVAL      VIDMSG    = *blank
029800140627     C                   EVAL      wDataDa = 0
029900140627     C                   EVAL      wDataA = 99999999
030000140627     C*
030100140627     C     ERRL00        IFEQ      '0'
030200140627     C* se scritta, la data DA deve essere corretta
030300140627     C                   IF        VIDDataDa>0
030400140627     C* se ci sono più di 6 caratteri deve essere una data in formato ggmmaaaa
030500140627     C                   IF        VIDDataDa>999999
030600140627     C                   EVAL      wData08 = %dec(%subst(
030700140627     C                              UBFMTDATE_Convert(
030800140627     C                              %editc(VIDDataDa:'X'):
030900140627     C                              'DDMMYYYY':
031000140627     C                              'YYYYMMDD') :
031100140627     C                              1 : 8) : 8 : 0)
031200140627     C                   IF        wData08 = VIDDataDa
031300140627     C                   MOVEL     '1'           ERRL00
031400140627     C                   SETON                                        51
031500140701     C                   MOVEL     MSG(14)       DSMSMS
031600140627     C                   ELSE
031700140627     C                   EVAL      wDataDa = wData08
031800140627     C                   ENDIF
031900140627     C                   ELSE
032000140627     C                   EVAL      wData08 = %dec(%subst(
032100140627     C                              UBFMTDATE_Convert(
032200140627     C                              %editc(%dec(VIDDataDa:6:0):'X'):
032300140627     C                              'DDMMYY':
032400140627     C                              'YYYYMMDD') :
032500140627     C                              1 : 8) : 8 : 0)
032600140627     C                   IF        wData08 = VIDDataDa
032700140627     C                   MOVEL     '1'           ERRL00
032800140627     C                   SETON                                        51
032900140627     C                   MOVEL     MSG(14)       DSMSMS
033000140627     C                   ELSE
033100140627     C* imposto a video la data con l'anno di 4
033200140627     C                   EVAL      wDataDa = wData08
033300140627     C                   EVAL      VIDDataDa = %dec(
033400140627     C                              %subst(%editc(wData08:'X'):7:2) +
033500140627     C                              %subst(%editc(wData08:'X'):5:2) +
033600140627     C                              %subst(%editc(wData08:'X'):1:4):8:0)
033700140627     C                   ENDIF
033800140627     C                   ENDIF
033900140627     C                   ENDIF
034000140627     C*
034100140627     C                   ENDIF
034200140627     C*
034300140627     C     ERRL00        IFEQ      '0'
034400140627     C* se scritta, la data A deve essere corretta
034500140627     C                   IF        VIDDataA>0
034600140627     C* se ci sono più di 6 caratteri deve essere una data in formato ggmmaaaa
034700140627     C                   IF        VIDDataA>999999
034800140627     C                   EVAL      wData08 = %dec(%subst(
034900140627     C                              UBFMTDATE_Convert(
035000140701     C                              %editc(VIDDataA:'X'):
035100140627     C                              'DDMMYYYY':
035200140627     C                              'YYYYMMDD') :
035300140627     C                              1 : 8) : 8 : 0)
035400140627     C                   IF        wData08 = VIDDataA
035500140627     C                   MOVEL     '1'           ERRL00
035600140701     C                   SETON                                        52
035700140701     C                   MOVEL     MSG(14)       DSMSMS
035800140627     C                   ELSE
035900140627     C                   EVAL      wDataA = wData08
036000140627     C                   ENDIF
036100140627     C                   ELSE
036200140627     C                   EVAL      wData08 = %dec(%subst(
036300140627     C                              UBFMTDATE_Convert(
036400140627     C                              %editc(%dec(VIDDataA:6:0):'X'):
036500140627     C                              'DDMMYY':
036600140627     C                              'YYYYMMDD') :
036700140627     C                              1 : 8) : 8 : 0)
036800140627     C                   IF        wData08 = VIDDataA
036900140627     C                   MOVEL     '1'           ERRL00
037000140701     C                   SETON                                        52
037100140627     C                   MOVEL     MSG(14)       DSMSMS
037200140627     C                   ELSE
037300140627     C* imposto a video la data con l'anno di 4
037400140627     C                   EVAL      wDataA = wData08
037500140701     C                   EVAL      VIDDataA = %dec(
037600140627     C                              %subst(%editc(wData08:'X'):7:2) +
037700140627     C                              %subst(%editc(wData08:'X'):5:2) +
037800140627     C                              %subst(%editc(wData08:'X'):1:4):8:0)
037900140627     C                   ENDIF
038000140627     C                   ENDIF
038100140627     C                   ENDIF
038200140627     C*
038300140627     C                   ENDIF
038400140627     C*
038500140627     C     ERRL00        IFEQ      '0'
038600140627     C* controllo che la data A sia >= data DA
038700140627     C                   IF        wDataA < wDataDa
038800140627     C                   MOVEL     '1'           ERRL00
038900140627     C                   MOVEL     MSG(15)       DSMSMS
039000140627     C                   SETON                                        5152
039100140627     C                   ENDIF
039200140627     C*
039300140627     C                   ENDIF
039400140630     C*
039500140630     C     ERRL00        IFEQ      '0'
039600140630     C* se richiesto elenco clienti
039700140630     C                   SETOFF                                           28
039800140630     C     '?'           SCAN      VIDKsc                                 28
039900140630IF  1C     *IN28         IFEQ      *ON
040000140630     C* RICHIESTO ELENCO CODICI
040100140630     C* CLIENTI
040200140630     C                   CLEAR                   DSXC
040300140630     C                   Z-ADD     DUTKCI        DXCCAP
040400140630     C                   CALL      'XCLIR'
040500140630     C                   PARM                    DSXC
040600140630     C* SCOMPATTA
040700140630     C                   MOVEA     DXCKSC        KSA                        ALF
040800140630DO  2C     1             DO        30            J                 3 0
040900140630     C                   MOVE      KSA(J)        DSKSA                      F
041000140630     C                   Z-ADD     DSKSN         KSC(J)                     M
041100140630E   2C                   ENDDO
041200140630     C                   MOVEL     KSC(1)        WKSC              7
041300140630     C                   MOVEL     *all'0'       VIDKsc
041400140630     C                   MOVE      WKSC          VIDKsc
041500140630E   1C                   ENDIF
041600140630     C*
041700140630     C* CODICE CLIENTE NUMERICO
041800140630IF  1C     WKSC          IFLT      *ZEROS
041900140630     C                   MOVEL     '1'           WERR              1
042000140630X   1C                   ELSE
042100140630     C*
042200140630     C* CONTROLLA ESISTENZA CODICE
042300140630     C* nessun controllo
042400140630E   1C                   ENDIF
042500120927     C*
042600140630E   1C                   ENDIF
042700140630     C*
042800140630     C     ERRL00        IFEQ      '0'
042900140630     C* controllo esistenza tipo file
043000140630     C                   IF        VIDTip <> *blank
043100140630E   1C                   EXSR      CntTip
043200140630E   1C                   ENDIF
043300140630E   1C*
043400140630E   1C                   ENDIF
043500140701     C*
043600140701     C     ERRL00        IFEQ      '0'
043700140701     C* la percentuale di errore non può essere > 100
043800140701     C                   IF        VIDPerc > 100
043900140701     C                   MOVEL     '1'           ERRL00
044000140701     C                   MOVEL     MSG(16)       DSMSMS
044100140702     C                   SETON                                        55
044200140701E   1C                   ENDIF
044300140701E   1C*
044400140701E   1C                   ENDIF
044500140701E   1C*
044600140701     C                   IF        VIDPerc = 0
044700140701     C                   EVAL      VIDPerc = 100
044800140701E   1C                   ENDIF
044900140630     C*
045000970528     C                   ENDSR
045100140630     C*------------------------------------------------------------------------*
045200140630     C* Cnttip - CONTROLLA codice tipo legame
045300140630     C*------------------------------------------------------------------------*
045400140630     C     cnttip        begsr
045500140630     C                   clear                   TIBS02DS
045600140630     C*
045700140630     C     '?'           SCAN      VIDTip                                 30
045800140630     C*  Ricerca
045900140630     C                   eval      t02tla = 'L'
046000140630    1C                   IF        *IN30 = *ON
046100140630     C                   movel     'R'           t02mod
046200140630     C                   else
046300140630     C                   movel     'C'           t02mod
046400140630     C                   movel(p)  VIDTip        t02ke1
046500140630     C                   end
046600140630     C                   movel     'ITF'         t02cod
046700140630     C                   movel     knsif         t02sif
046800140630     C                   CALL      'TIBS02R'
046900140630     C                   parm                    KPJBA
047000140630     C                   parm                    TIBS02DS
047100140630     C
047200140630     C*
047300140630    2C                   IF        T02err = *blanks
047400140630     C                   movel     T02ke1        VIDTip
047500140630     C                   else
047600140630     C                   movel     '1'           errl00
047700140630     C                   MOVEL     msg(11)       DSMSMS
047800140630     C                   SETON                                        54
047900140630     C                   ENDIF
048000140630     C
048100140630     C                   endsr
048200970526     C*------------------------------------------------------------------------*
048300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
048400970526     C*------------------------------------------------------------------------*
048500970526     C     AZZVID        BEGSR
048600970526     C* CAMPI
048700970526     C*
048800970526     C                   ENDSR
048900970526     C*------------------------------------------------------------------------*
049000020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
049100970526     C*------------------------------------------------------------------------*
049200970526     C     IMPFUN        BEGSR
049300970526     C*
049400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
049500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
049600970526     C                   EXSR      FUNGES
049700970526     C*
049800970526     C                   ENDSR
049900020923     C*--------------------------------------------------------------------------------------------*
050000020923     C* REPERISCE I DATI UTENTE
050100020923     C*--------------------------------------------------------------------------------------------*
050200020923     C     REPDATIUTE    BEGSR
050300020923     C*
050400020923     C* INIZIALIZZA VARIABILI DI WRK
050500020923     C                   CLEAR                   TIBS34DS
050600020923     C                   CLEAR                   AZUTEDS
050700020923     C                   CLEAR                   DDATIUTE
050800020923     C*
050900020923     C     *DTAARA       DEFINE    §azute        azuteds
051000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
051100020923     C                   IN(E)     *DTAARA
051200020923if  1C                   IF        %Error
051300020923     c                   EVAL      I34Tla = 'L'
051400020923     C                   CALL      'TIBS34R'
051500020923     C                   PARM                    Tibs34Ds
051600020923     C                   IN        *DTAARA
051700020923e   1C                   ENDIF
051800020923     C*
051900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
052000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
052100020923     C*
052200020923     C                   ENDSR
052300970526     C*------------------------------------------------------------------------*
052400970526     C* *INZSR - OPERAZIONI INIZIALI
052500970526     C*------------------------------------------------------------------------*
052600970526     C     *INZSR        BEGSR
052700991217     C*
052800991217     C     *ENTRY        PLIST
052900991217     C                   PARM                    KPJBA
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
055800140627     C                   MOVEL     'TIS760R1'    DSMSPG
055900140627     C                   MOVEL     'TIS760R1'    MSGQUE
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
073800970526     C     FUNKEY        IFEQ      '12'
073900970526     C                   EXSR      F12GES
074000970526     C                   END
074100970526     C     FUNKEY        IFEQ      '24'
074200970526     C                   EXSR      F24GES
074300970526     C                   END
074400970526     C                   END
074500970526     C*
074600970526     C                   ENDSR
074700970526     C*------------------------------------------------------------------------*
074800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
074900970526     C*------------------------------------------------------------------------*
075000970526     C     FUNVAR        BEGSR
075100970526     C*
075200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
075300970526     C*
075400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
075500970526     C     KEY           IFEQ      ENTER
075600970526     C                   MOVEL     *BLANKS       FUNKEY
075700970526     C                   END
075800970526     C     KEY           IFEQ      F1
075900970526     C                   MOVEL     '01'          FUNKEY
076000970526     C                   END
076100970526     C     KEY           IFEQ      F2
076200970526     C                   MOVEL     '02'          FUNKEY
076300970526     C                   END
076400970526     C     KEY           IFEQ      F3
076500970526     C                   MOVEL     '03'          FUNKEY
076600970526     C                   END
076700970526     C     KEY           IFEQ      F4
076800970526     C                   MOVEL     '04'          FUNKEY
076900970526     C                   END
077000970526     C     KEY           IFEQ      F5
077100970526     C                   MOVEL     '05'          FUNKEY
077200970526     C                   END
077300970526     C     KEY           IFEQ      F6
077400970526     C                   MOVEL     '06'          FUNKEY
077500970526     C                   END
077600970526     C     KEY           IFEQ      F7
077700970526     C                   MOVEL     '07'          FUNKEY
077800970526     C                   END
077900970526     C     KEY           IFEQ      F8
078000970526     C                   MOVEL     '08'          FUNKEY
078100970526     C                   END
078200970526     C     KEY           IFEQ      F9
078300970526     C                   MOVEL     '09'          FUNKEY
078400970526     C                   END
078500970526     C     KEY           IFEQ      F10
078600970526     C                   MOVEL     '10'          FUNKEY
078700970526     C                   END
078800970526     C     KEY           IFEQ      F11
078900970526     C                   MOVEL     '11'          FUNKEY
079000970526     C                   END
079100970526     C     KEY           IFEQ      F12
079200970526     C                   MOVEL     '12'          FUNKEY
079300970526     C                   END
079400970526     C     KEY           IFEQ      F13
079500970526     C                   MOVEL     '13'          FUNKEY
079600970526     C                   END
079700970526     C     KEY           IFEQ      F14
079800970526     C                   MOVEL     '14'          FUNKEY
079900970526     C                   END
080000970526     C     KEY           IFEQ      F15
080100970526     C                   MOVEL     '15'          FUNKEY
080200970526     C                   END
080300970526     C     KEY           IFEQ      F16
080400970526     C                   MOVEL     '16'          FUNKEY
080500970526     C                   END
080600970526     C     KEY           IFEQ      F17
080700970526     C                   MOVEL     '17'          FUNKEY
080800970526     C                   END
080900970526     C     KEY           IFEQ      F18
081000970526     C                   MOVEL     '18'          FUNKEY
081100970526     C                   END
081200970526     C     KEY           IFEQ      F19
081300970526     C                   MOVEL     '19'          FUNKEY
081400970526     C                   END
081500970526     C     KEY           IFEQ      F20
081600970526     C                   MOVEL     '20'          FUNKEY
081700970526     C                   END
081800970526     C     KEY           IFEQ      F21
081900970526     C                   MOVEL     '21'          FUNKEY
082000970526     C                   END
082100970526     C     KEY           IFEQ      F22
082200970526     C                   MOVEL     '22'          FUNKEY
082300970526     C                   END
082400970526     C     KEY           IFEQ      F23
082500970526     C                   MOVEL     '23'          FUNKEY
082600970526     C                   END
082700970526     C     KEY           IFEQ      F24
082800970526     C                   MOVEL     '24'          FUNKEY
082900970526     C                   END
083000970526     C     KEY           IFEQ      ROLLUP
083100970526     C                   MOVEL     'RU'          FUNKEY
083200970526     C                   END
083300970526     C     KEY           IFEQ      HELP
083400970526     C                   MOVEL     'HP'          FUNKEY
083500970526     C                   END
083600970526     C                   ENDSR
083700970526     C*------------------------------------------------------------------------*
083800970526     C* NOTE COSTRUZIONE SCHIERE
083900970526     C*------------------------------------------------------------------------*
084000970526     C*
084100970526     C* -MSG-MESSAGGI
084200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
084300970526     C* (NON TOCCARE MSG,1 E MSG,2)
084400970526     C*
084500970526     C* -CMD-COMANDI
084600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
084700970526     C*
084800970526     C* -OPD-OPZIONI
084900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
085000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
085100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
085200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
085300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
085400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
085500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
085600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
085700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
085800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
085900970526     C*
086000970526     C* -FUD-FUNZIONI
086100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
086200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
086300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
086400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
086500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
086600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
086700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
086800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
086900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
087000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
087100970526** MSG - MESSAGGI
087200120927Tasto funzionale non gestito                                                   1
087300120927Numero chiamate obbligatorio                                                   2
087400140116Libreria e file sorgente obbligatori                                           3
087500140312Sono obbligatorie le posizioni di RSD-IND-LOD                                  4
087600130108                                                                               5
087700130108Campo obbligatorio                                                             6
087800130108                                                                               7
087900130108                                                                               8
088000120927                                                                               9
088100120927                                                                               10
088200140630Codice tipo file inesistente in tabella                                        11
088300120927                                                                               12
088400120927                                                                               13
088500120927Data errata                                                                    14
088600140627Data DA > data A                                                               15
088700140701Massima Percentuale di errore = 100                                            16
088800120927                                                                               17
088900120927                                                                               18
089000120927                                                                               19
089100140702Estrazione conclusa                                                            20
089200120927Scelta obbligatoria                                                            21
089300130110Eseguita la stampa                                                             22
089400121108                                                                               23
089500121108                                                                               24
089600970526** FUD - FUNZIONI
089700970526                   -        (S/N)=N                        funz.01¬
089800970526                   -        (S/N)=N                        funz.02¬
089900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
090000970526                   -        (S/N)=N                        funz.04¬
090100970526                   -        (S/N)=N                        funz.05¬
090200140701F06=Estr.interatt. -        (S/N)=S                        funz.06¬
090300140701F07=Estr.batch     -        (S/N)=S                        funz.07¬
090400970527                   -        (S/N)=N                        funz.08¬
090500970527                   -        (S/N)=N                        funz.09¬
090600970527                   -        (S/N)=N                        funz.10¬
090700970526                   -        (S/N)=N                        funz.11¬
090800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
090900970526                   -        (S/N)=N                        funz.13¬
091000970526                   -        (S/N)=N                        funz.14¬
091100970526                   -        (S/N)=N                        funz.15¬
091200970526                   -        (S/N)=N                        funz.16¬
091300970526                   -        (S/N)=N                        funz.17¬
091400970526                   -        (S/N)=N                        funz.18¬
091500970526                   -        (S/N)=N                        funz.19¬
091600970526                   -        (S/N)=N                        funz.20¬
091700970526                   -        (S/N)=N                        funz.21¬
091800970526                   -        (S/N)=N                        funz.22¬
091900970526                   -        (S/N)=N                        funz.23¬
092000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
