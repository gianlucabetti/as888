000100970526     H*------------------------------------------------------------------------*
000200140627     H* Lancio verifica log upload in errore
000300970526     H*------------------------------------------------------------------------*
000400140627     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500140627     h dftactgrp(*no) bnddir('UBBNDDIR')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900140904     FTIS7GI1D1 cf   e             WORKSTN INFDS(DEVDS1)
001000140904     F                                     SFILE(S7GIDMS:MSGULT)
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
009600140904     d DS7GIR2         ds
009700140701     d pIn_DatDa                      8s 0
009800140701     d pIn_DatA                       8s 0
009900140701     d pIn_Ksc                        8
010000140701     d pIn_Tip                        2
010100140904     d pIn_ISV                        2
010200140701     d pIn_Modo                       1
010300140701     d pOut_Esito                     2  0
010400130116
010500130116     d ora             s               t
010600140117     d wX              s              2s 0
010700140627     d wData08         s              8s 0
010800140627     d wDataDa         s              8s 0
010900140627     d wDataA          s              8s 0
011000120928
011100140627      /COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
011200140627
011300970526     C*------------------------------------------------------------------------*
011400000303     C* MAIN LINES
011500970526     C*------------------------------------------------------------------------*
011600970526     C*
011700120927     C* metto i default su D03
011800120927     C                   EXSR      InzD03
011900120927     C*
012000120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012100991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012400970526     C*
012500970526     C* EMETTE FORMATO VIDEO
012600970526     C                   EXSR      BUTFMR
012700970526     C*
012800970526     C* GESTIONE TASTI FUNZIONALI
012900970526     C                   EXSR      FUNCON
013000970526E   2C                   ENDDO
013100970526     C*
013200970526     C* FINE PGM
013300970526     C                   SETON                                        LR
013400970526     C*------------------------------------------------------------------------*
013500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013600970526     C*------------------------------------------------------------------------*
013700970526     C     BUTFMR        BEGSR
013800970526     C*
013900970526     C* EMETTE LA TESTATA
014000140904     C                   WRITE     S7GID01
014100970526     C*
014200970526     C* SPEDISCE EVENTUALE MESSAGGIO
014300970526     C                   EXSR      SNDMSG
014400970526     C*
014500970526     C* EMETTE LE FUNZIONI
014600140904     C                   WRITE     S7GID02
014700970526     C*
014800970526     C* EMETTE IL VIDEO
014900140904     C                   EXFMT     S7GID03
015000970526     C*
015100991214     C* REIMPOSTA FLAG DI ERRORE
015200991214     C                   MOVEL     '0'           ERRL00
015300970526     C*
015400970526     C* REIMPOSTA INDICATORI DI ERRORE
015500140702     C                   SETOFF                                       515253
015600140702     C                   SETOFF                                       545556
015700970526     C*
015800970526     C                   ENDSR
015900970526     C*------------------------------------------------------------------------*
016000120927     C* InzD03 - Inizializzo D03
016100970526     C*------------------------------------------------------------------------*
016200120927     C     InzD03        BEGSR
016300120927     C*
016400140627     C                   EVAL      VIDDataDa = 0
016500140627     C                   EVAL      VIDDataA = 0
016600140627     C                   EVAL      VIDKsc= *blank
016700140627     C                   EVAL      VIDTip= *blank
016800140904     C                   EVAL      VIDISV= *blank
016900130108     C*
017000120927     C                   ENDSR
017100120927     C*------------------------------------------------------------------------*
017200120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017300120927     C*------------------------------------------------------------------------*
017400120927     C     SNDMSG        BEGSR
017500120927     C*
017600970526     C* PULISCE IL SFILE MESSAGGI
017700970526     C                   MOVEL     '1'           *IN40
017800140904     C                   WRITE     S7GIDMC
017900970526     C                   MOVEL     '0'           *IN40
018000970526     C*
018100970526     C* VISUALIZZA IL MESSAGGIO
018200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018300970526     C                   CALL      'XMSG'        XMSGPA
018400020611     C     DSMSER        IFEQ      *ZEROS
018500970526     C                   MOVEL     DSMSKE        MSGKEY
018600020611     C                   ENDIF
018700970526     C                   Z-ADD     1             MSGULT
018800140904     C                   WRITE     S7GIDMS
018900140904     C                   WRITE     S7GIDMC
019000970526     C*
019100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019200970526     C                   MOVEL     *BLANKS       DSMSMS
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* ENTGES - GESTIONE ENTER -> INVIO
019700970526     C*------------------------------------------------------------------------*
019800970526     C     ENTGES        BEGSR
019900970526     C*
020000970526     C* CONTROLLA I CAMPI IMMESSI
020100970526     C                   EXSR      DECODI
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F03GES - GESTIONE F03 -> FINE
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F03GES        BEGSR
020800970526     C*
020900970526     C* IMPOSTA "PREMUTO F03"
021000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400140701     C* F06GES - GESTIONE F06 -> Estrazione interattiva
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F06GES        BEGSR
021700970526     C*
021800991214     C* CONTROLLA I CAMPI IMMESSI
021900991214     C                   EXSR      DECODI
022000020611     C*
022100991214     C* ESEGUE ROUTINE DI CONFERMA
022200020611     C     ERRL00        IFEQ      '0'
022300140701     C* interattivo
022400140701     C                   EVAL      pIn_Modo    = 'I'
022500991214     C                   EXSR      ESEGUI
022600140702     C* il messaggio di fine lo do sempre, a prescindere dagli esiti
022700121109     C                   MOVEL     MSG(20)       DSMSMS
022800121109     C* non esco mai dal pgm
022900121109     C***                MOVEL     '1'           FINL00
023000121108     C                   ENDIF
023100970526     C*
023200991214     C                   ENDSR
023300140701     C*------------------------------------------------------------------------*
023400140701     C* F07GES - GESTIONE F07 -> Estrazione in batch
023500140701     C*------------------------------------------------------------------------*
023600140701     C     F07GES        BEGSR
023700140701     C*
023800140701     C* CONTROLLA I CAMPI IMMESSI
023900140701     C                   EXSR      DECODI
024000140701     C*
024100140701     C* ESEGUE ROUTINE DI CONFERMA
024200140701     C     ERRL00        IFEQ      '0'
024300140701     C* batch
024400140701     C                   EVAL      pIn_Modo    = 'B'
024500140701     C                   EXSR      ESEGUI
024600140702     C* il messaggio di fine lo do sempre, a prescindere dagli esiti
024700140701     C                   MOVEL     MSG(20)       DSMSMS
024800140701     C* esco dal pgm
024900140701     C                   MOVEL     '1'           FINL00
025000140701     C                   ENDIF
025100140701     C*
025200140701     C                   ENDSR
025300130109     C*------------------------------------------------------------------------*
025400130109     C* F12GES - GESTIONE F12 -> RITORNO
025500130109     C*------------------------------------------------------------------------*
025600130109     C     F12GES        BEGSR
025700130109     C*
025800130109     C* IMPOSTA "PREMUTO F12"
025900130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026000130109     C*
026100130109     C                   ENDSR
026200970526     C*------------------------------------------------------------------------*
026300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026400970526     C*------------------------------------------------------------------------*
026500970526     C     F24GES        BEGSR
026600970526     C*
026700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026800970526     C*
026900970526     C                   ENDSR
027000970526     C*------------------------------------------------------------------------*
027100140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
027200970526     C*------------------------------------------------------------------------*
027300991214     C     ESEGUI        BEGSR
027400140120     C*
027500140627     C                   EVAL      pIn_DatDa   = wDataDa
027600140627     C                   EVAL      pIn_DatA    = wDataA
027700141010     C                   EVAL      pIn_Ksc     = VIDKsc
027800141010     C                   IF        pIn_Ksc     = *zeros
027900141010     C                   EVAL      pIn_Ksc     = *blank
028000141010     C                   ENDIF
028100140627     C                   EVAL      pIn_Tip     = VIDTip
028200140904     C                   EVAL      pIn_ISV     = VIDISV
028300140904     C                   EVAL      KPJBU = DS7GIR2
028400140701     C*
028500140904     C* interattivo: estraggo i dati scrivendoli su WFLDE00F e mostro il risultato
028600140904     C* batch      : estraggo i dati scrivendoli su WFLDE00F e li spedisco al CED
028700140904     C                   CALL      'TIS7GI1R3'
028800140701     C                   parm                    KPJBA
028900991214     C*
029000970526     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029300970526     C*------------------------------------------------------------------------*
029400970526     C     DECODI        BEGSR
029500121019     C*
029600140702     C                   SETOFF                                       515253
029700140702     C                   SETOFF                                       545556
029800140627     C                   EVAL      VIDMSG    = *blank
029900140627     C                   EVAL      wDataDa = 0
030000140627     C                   EVAL      wDataA = 99999999
030100140627     C*
030200140627     C     ERRL00        IFEQ      '0'
030300140627     C* se scritta, la data DA deve essere corretta
030400140627     C                   IF        VIDDataDa>0
030500140627     C* se ci sono più di 6 caratteri deve essere una data in formato ggmmaaaa
030600140627     C                   IF        VIDDataDa>999999
030700140627     C                   EVAL      wData08 = %dec(%subst(
030800140627     C                              UBFMTDATE_Convert(
030900140627     C                              %editc(VIDDataDa:'X'):
031000140627     C                              'DDMMYYYY':
031100140627     C                              'YYYYMMDD') :
031200140627     C                              1 : 8) : 8 : 0)
031300140627     C                   IF        wData08 = VIDDataDa
031400140627     C                   MOVEL     '1'           ERRL00
031500140627     C                   SETON                                        51
031600140701     C                   MOVEL     MSG(14)       DSMSMS
031700140627     C                   ELSE
031800140627     C                   EVAL      wDataDa = wData08
031900140627     C                   ENDIF
032000140627     C                   ELSE
032100140627     C                   EVAL      wData08 = %dec(%subst(
032200140627     C                              UBFMTDATE_Convert(
032300140627     C                              %editc(%dec(VIDDataDa:6:0):'X'):
032400140627     C                              'DDMMYY':
032500140627     C                              'YYYYMMDD') :
032600140627     C                              1 : 8) : 8 : 0)
032700140627     C                   IF        wData08 = VIDDataDa
032800140627     C                   MOVEL     '1'           ERRL00
032900140627     C                   SETON                                        51
033000140627     C                   MOVEL     MSG(14)       DSMSMS
033100140627     C                   ELSE
033200140627     C* imposto a video la data con l'anno di 4
033300140627     C                   EVAL      wDataDa = wData08
033400140627     C                   EVAL      VIDDataDa = %dec(
033500140627     C                              %subst(%editc(wData08:'X'):7:2) +
033600140627     C                              %subst(%editc(wData08:'X'):5:2) +
033700140627     C                              %subst(%editc(wData08:'X'):1:4):8:0)
033800140627     C                   ENDIF
033900140627     C                   ENDIF
034000140627     C                   ENDIF
034100140627     C*
034200140627     C                   ENDIF
034300140627     C*
034400140627     C     ERRL00        IFEQ      '0'
034500140627     C* se scritta, la data A deve essere corretta
034600140627     C                   IF        VIDDataA>0
034700140627     C* se ci sono più di 6 caratteri deve essere una data in formato ggmmaaaa
034800140627     C                   IF        VIDDataA>999999
034900140627     C                   EVAL      wData08 = %dec(%subst(
035000140627     C                              UBFMTDATE_Convert(
035100140701     C                              %editc(VIDDataA:'X'):
035200140627     C                              'DDMMYYYY':
035300140627     C                              'YYYYMMDD') :
035400140627     C                              1 : 8) : 8 : 0)
035500140627     C                   IF        wData08 = VIDDataA
035600140627     C                   MOVEL     '1'           ERRL00
035700140701     C                   SETON                                        52
035800140701     C                   MOVEL     MSG(14)       DSMSMS
035900140627     C                   ELSE
036000140627     C                   EVAL      wDataA = wData08
036100140627     C                   ENDIF
036200140627     C                   ELSE
036300140627     C                   EVAL      wData08 = %dec(%subst(
036400140627     C                              UBFMTDATE_Convert(
036500140627     C                              %editc(%dec(VIDDataA:6:0):'X'):
036600140627     C                              'DDMMYY':
036700140627     C                              'YYYYMMDD') :
036800140627     C                              1 : 8) : 8 : 0)
036900140627     C                   IF        wData08 = VIDDataA
037000140627     C                   MOVEL     '1'           ERRL00
037100140701     C                   SETON                                        52
037200140627     C                   MOVEL     MSG(14)       DSMSMS
037300140627     C                   ELSE
037400140627     C* imposto a video la data con l'anno di 4
037500140627     C                   EVAL      wDataA = wData08
037600140701     C                   EVAL      VIDDataA = %dec(
037700140627     C                              %subst(%editc(wData08:'X'):7:2) +
037800140627     C                              %subst(%editc(wData08:'X'):5:2) +
037900140627     C                              %subst(%editc(wData08:'X'):1:4):8:0)
038000140627     C                   ENDIF
038100140627     C                   ENDIF
038200140627     C                   ENDIF
038300140627     C*
038400140627     C                   ENDIF
038500140627     C*
038600140627     C     ERRL00        IFEQ      '0'
038700140627     C* controllo che la data A sia >= data DA
038800140627     C                   IF        wDataA < wDataDa
038900140627     C                   MOVEL     '1'           ERRL00
039000140627     C                   MOVEL     MSG(15)       DSMSMS
039100140627     C                   SETON                                        5152
039200140627     C                   ENDIF
039300140627     C*
039400140627     C                   ENDIF
039500140630     C*
039600140630     C     ERRL00        IFEQ      '0'
039700140630     C* se richiesto elenco clienti
039800140630     C                   SETOFF                                           28
039900140630     C     '?'           SCAN      VIDKsc                                 28
040000140630IF  1C     *IN28         IFEQ      *ON
040100140630     C* RICHIESTO ELENCO CODICI
040200140630     C* CLIENTI
040300140630     C                   CLEAR                   DSXC
040400140630     C                   Z-ADD     DUTKCI        DXCCAP
040500140630     C                   CALL      'XCLIR'
040600140630     C                   PARM                    DSXC
040700140630     C* SCOMPATTA
040800140630     C                   MOVEA     DXCKSC        KSA                        ALF
040900140630DO  2C     1             DO        30            J                 3 0
041000140630     C                   MOVE      KSA(J)        DSKSA                      F
041100140630     C                   Z-ADD     DSKSN         KSC(J)                     M
041200140630E   2C                   ENDDO
041300140630     C                   MOVEL     KSC(1)        WKSC              7
041400140630     C                   MOVEL     *all'0'       VIDKsc
041500140630     C                   MOVE      WKSC          VIDKsc
041600140630E   1C                   ENDIF
041700140630     C*
041800140630     C* CODICE CLIENTE NUMERICO
041900140630IF  1C     WKSC          IFLT      *ZEROS
042000140630     C                   MOVEL     '1'           WERR              1
042100140630X   1C                   ELSE
042200140630     C*
042300140630     C* CONTROLLA ESISTENZA CODICE
042400140630     C* nessun controllo
042500140630E   1C                   ENDIF
042600120927     C*
042700140630E   1C                   ENDIF
042800140630     C*
042900140630     C     ERRL00        IFEQ      '0'
043000140630     C* controllo esistenza tipo file
043100140630     C                   IF        VIDTip <> *blank
043200140630E   1C                   EXSR      CntTip
043300140630E   1C                   ENDIF
043400140630E   1C*
043500140630E   1C                   ENDIF
043600140701     C*
043700140701     C     ERRL00        IFEQ      '0'
043800140701     C* la percentuale di errore non può essere > 100
043900140701     C                   IF        VIDPerc > 100
044000140701     C                   MOVEL     '1'           ERRL00
044100140701     C                   MOVEL     MSG(16)       DSMSMS
044200140702     C                   SETON                                        55
044300140701E   1C                   ENDIF
044400140701E   1C*
044500140701E   1C                   ENDIF
044600140701E   1C*
044700140701     C                   IF        VIDPerc = 0
044800140701     C                   EVAL      VIDPerc = 100
044900140701E   1C                   ENDIF
045000140630     C*
045100970528     C                   ENDSR
045200140630     C*------------------------------------------------------------------------*
045300140630     C* Cnttip - CONTROLLA codice tipo legame
045400140630     C*------------------------------------------------------------------------*
045500140630     C     cnttip        begsr
045600140630     C                   clear                   TIBS02DS
045700140630     C*
045800140630     C     '?'           SCAN      VIDTip                                 30
045900140630     C*  Ricerca
046000140630     C                   eval      t02tla = 'L'
046100140630    1C                   IF        *IN30 = *ON
046200140630     C                   movel     'R'           t02mod
046300140630     C                   else
046400140630     C                   movel     'C'           t02mod
046500140630     C                   movel(p)  VIDTip        t02ke1
046600140630     C                   end
046700140630     C                   movel     'ITF'         t02cod
046800140630     C                   movel     knsif         t02sif
046900140630     C                   CALL      'TIBS02R'
047000140630     C                   parm                    KPJBA
047100140630     C                   parm                    TIBS02DS
047200140630     C
047300140630     C*
047400140630    2C                   IF        T02err = *blanks
047500140630     C                   movel     T02ke1        VIDTip
047600140630     C                   else
047700140630     C                   movel     '1'           errl00
047800140630     C                   MOVEL     msg(11)       DSMSMS
047900140630     C                   SETON                                        54
048000140630     C                   ENDIF
048100140630     C
048200140630     C                   endsr
048300970526     C*------------------------------------------------------------------------*
048400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
048500970526     C*------------------------------------------------------------------------*
048600970526     C     AZZVID        BEGSR
048700970526     C* CAMPI
048800970526     C*
048900970526     C                   ENDSR
049000970526     C*------------------------------------------------------------------------*
049100020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
049200970526     C*------------------------------------------------------------------------*
049300970526     C     IMPFUN        BEGSR
049400970526     C*
049500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
049600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
049700970526     C                   EXSR      FUNGES
049800970526     C*
049900970526     C                   ENDSR
050000020923     C*--------------------------------------------------------------------------------------------*
050100020923     C* REPERISCE I DATI UTENTE
050200020923     C*--------------------------------------------------------------------------------------------*
050300020923     C     REPDATIUTE    BEGSR
050400020923     C*
050500020923     C* INIZIALIZZA VARIABILI DI WRK
050600020923     C                   CLEAR                   TIBS34DS
050700020923     C                   CLEAR                   AZUTEDS
050800020923     C                   CLEAR                   DDATIUTE
050900020923     C*
051000020923     C     *DTAARA       DEFINE    §azute        azuteds
051100020923     C     *DTAARA       DEFINE    §datiute      ddatiute
051200020923     C                   IN(E)     *DTAARA
051300020923if  1C                   IF        %Error
051400020923     c                   EVAL      I34Tla = 'L'
051500020923     C                   CALL      'TIBS34R'
051600020923     C                   PARM                    Tibs34Ds
051700020923     C                   IN        *DTAARA
051800020923e   1C                   ENDIF
051900020923     C*
052000020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
052100020923     C                   MOVEL(P)  RSUT          DSFIRS           20
052200020923     C*
052300020923     C                   ENDSR
052400970526     C*------------------------------------------------------------------------*
052500970526     C* *INZSR - OPERAZIONI INIZIALI
052600970526     C*------------------------------------------------------------------------*
052700970526     C     *INZSR        BEGSR
052800991217     C*
052900991217     C     *ENTRY        PLIST
053000991217     C                   PARM                    KPJBA
053100020923     C*
053200020923     C* REPERISCE I DATI DELL'UTENTE
053300020923     C                   EXSR      REPDATIUTE
053400970526     C*
053500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
053600970526     C     XMSGPA        PLIST
053700970526     C                   PARM                    DSMSMS
053800970526     C                   PARM                    DSMSPG
053900970526     C                   PARM                    DSMSKE
054000970526     C                   PARM                    DSMSER
054100970526     C*
054200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
054300970526     C                   CALL      'XKEY'
054400970526     C                   PARM                    DSKEY
054500970526     C*---
054600970526     C* VARIABILI NON RIFERITE AL DATA BASE
054700970526     C*---
054800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
054900991214     C                   MOVEL     *BLANKS       DEPOP1            3
055000970526     C*---
055100970526     C* VARIABILI DI CONTROLLO
055200970526     C*---
055300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
055400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
055600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
055700970526     C*
055800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
055900140904     C                   MOVEL     'TIS7GI1R1'   DSMSPG
056000140904     C                   MOVEL     'TIS7GI1R1'   MSGQUE
056100970526     C                   MOVEL     *BLANKS       MSGKEY
056200970526     C                   MOVEL     *BLANKS       DSMSMS
056300970528     C*
056400970528     C* CALCOLA LA DATA CORRENTE
056500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
056600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
056700970528     C                   Z-ADD     WN8           G08DAT
056800970528     C                   Z-ADD     *ZEROS        G08INV
056900970528     C                   MOVEL     '0'           G08ERR
057000970528     C                   CALL      'XSRDA8'
057100970528     C                   PARM                    WLBDA8
057200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
057300000310     C*
057400000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
057500000310     C                   EXSR      AZZVID
057600970526     C*
057700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
057800970526     C                   EXSR      IMPFUN
057900970526     C*
058000970526     C                   ENDSR
058100970526     C*------------------------------------------------------------------------*
058200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
058300970526     C*------------------------------------------------------------------------*
058400970526     C     FUNGES        BEGSR
058500970526     C*
058600970526     C* PULISCO CAMPI DI LAVORO
058700970526     C                   Z-ADD     0             W                 5 0
058800970526     C*
058900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
059000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
059100970526     C     1             DO        24            I
059200970526     C                   MOVEL     FUD(I)        FUDRIG
059300970526     C*
059400970526     C* PULIZIA FUNZIONI PRECEDENTI
059500970526     C                   MOVEL     *BLANKS       FUV(I)
059600970526     C*
059700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
059800970526     C* (SALTA F24)
059900970526     C     FUDATT        IFEQ      'S'
060000970526     C     FUDFUN        ANDNE     '24'
060100970526     C                   ADD       1             W
060200970526     C                   MOVEL     FUDDES        FUV(W)
060300970526     C                   END
060400970526     C                   END                                                    DO WHILE
060500970526     C*
060600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
060700970526     C* ALTRIMENTI DISATTIVO F24
060800970526     C     1             DO        24            I
060900970526     C                   MOVEL     FUD(I)        FUDRIG
061000970526     C*
061100970526     C     FUDFUN        IFEQ      '24'
061200970526     C     W             IFGT      6
061300970526     C                   MOVEL     'S'           FUDATT
061400970526     C                   ELSE
061500970526     C                   MOVEL     *BLANKS       FUDATT
061600970526     C                   END                                                    W GT 8
061700970526     C                   MOVEL     FUDRIG        FUD(I)
061800970526     C                   END
061900970526     C                   END                                                    DO WHILE
062000970526     C*
062100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
062200970526     C     W             DIV       6             I
062300970526     C                   MVR                     Z
062400970526     C*
062500970526     C     Z             IFNE      0
062600970526     C     I             ADD       1             Z
062700970526     C                   ELSE
062800970526     C                   Z-ADD     I             Z                 5 0
062900970526     C                   END
063000970526     C*
063100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
063200970526     C                   ADD       1             FUNGRU
063300970526     C     FUNGRU        IFGT      Z
063400970526     C     FUNGRU        ORLT      1
063500970526     C                   Z-ADD     1             FUNGRU
063600970526     C                   END
063700970526     C*
063800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
063900970526     C     FUNGRU        IFEQ      1
064000970526     C                   MOVEL     FUV(1)        FUN001
064100970526     C                   MOVEL     FUV(2)        FUN002
064200970526     C                   MOVEL     FUV(3)        FUN003
064300970526     C                   MOVEL     FUV(4)        FUN004
064400970526     C                   MOVEL     FUV(5)        FUN005
064500970526     C                   MOVEL     FUV(6)        FUN006
064600970526     C                   END
064700970526     C*
064800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
064900970526     C     FUNGRU        IFEQ      2
065000970526     C                   MOVEL     FUV(7)        FUN001
065100970526     C                   MOVEL     FUV(8)        FUN002
065200970526     C                   MOVEL     FUV(9)        FUN003
065300970526     C                   MOVEL     FUV(10)       FUN004
065400970526     C                   MOVEL     FUV(11)       FUN005
065500970526     C                   MOVEL     FUV(12)       FUN006
065600970526     C                   END
065700970526     C*
065800970526     C* TERZO GRUPPO DI 6 FUNZIONI
065900970526     C     FUNGRU        IFEQ      3
066000970526     C                   MOVEL     FUV(13)       FUN001
066100970526     C                   MOVEL     FUV(14)       FUN002
066200970526     C                   MOVEL     FUV(15)       FUN003
066300970526     C                   MOVEL     FUV(16)       FUN004
066400970526     C                   MOVEL     FUV(17)       FUN005
066500970526     C                   MOVEL     FUV(18)       FUN006
066600970526     C                   END
066700970526     C*
066800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
066900970526     C     FUNGRU        IFEQ      4
067000970526     C                   MOVEL     FUV(19)       FUN001
067100970526     C                   MOVEL     FUV(20)       FUN002
067200970526     C                   MOVEL     FUV(21)       FUN003
067300970526     C                   MOVEL     FUV(22)       FUN004
067400970526     C                   MOVEL     *BLANKS       FUN005
067500970526     C                   MOVEL     *BLANKS       FUN006
067600970526     C                   END
067700970526     C*
067800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
067900970526     C                   MOVEL     *BLANKS       FUN007
068000970526     C                   MOVEL     *BLANKS       FUN008
068100970526     C*
068200970526     C     1             DO        24            I
068300970526     C                   MOVEL     FUD(I)        FUDRIG
068400970526     C     FUDFUN        IFEQ      '24'
068500970526     C     FUDATT        ANDEQ     'S'
068600970526     C                   MOVEL     FUDDES        FUN008
068700970526     C                   END
068800970526     C                   END                                                    DO
068900970526     C*
069000970526     C                   ENDSR
069100970526     C*------------------------------------------------------------------------*
069200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
069300970526     C*------------------------------------------------------------------------*
069400970526     C     FUNCON        BEGSR
069500970526     C*
069600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
069700970526     C                   EXSR      FUNVAR
069800970526     C*
069900970526     C* PULISCO CAMPI DI LAVORO
070000970526     C                   Z-ADD     0             I                 5 0
070100970526     C                   Z-ADD     0             W                 5 0
070200970526     C*
070300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
070400970526     C     FUNKEY        IFNE      *BLANKS
070500970526     C     1             DO        24            I
070600970526     C                   MOVEL     FUD(I)        FUDRIG
070700970526     C*
070800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
070900970526     C     FUNKEY        IFEQ      FUDFUN
071000970526     C     FUDATT        ANDEQ     'S'
071100970526     C                   Z-ADD     I             W
071200970526     C                   END
071300970526     C                   END                                                    DO WHILE
071400970526     C*
071500970526     C* FUNZIONE NON ATTIVA (ERRORE)
071600970526     C     W             IFEQ      0
071700970526     C*
071800970526     C* EMISSIONE MESSAGGIO DI ERRORE
071900970526     C                   MOVEL     MSG(1)        DSMSMS
072000970526     C*
072100970526     C                   END                                                    W=0
072200970526     C                   END                                                    FUNKEY NE *BLANKS
072300970526     C*
072400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
072500970526     C     W             IFNE      0
072600970526     C     FUNKEY        OREQ      *BLANKS
072700970526     C     FUNKEY        IFEQ      *BLANKS
072800970526     C                   EXSR      ENTGES
072900970526     C                   END
073000970526     C     FUNKEY        IFEQ      '03'
073100970526     C                   EXSR      F03GES
073200970526     C                   END
073300970526     C     FUNKEY        IFEQ      '06'
073400970526     C                   EXSR      F06GES
073500970526     C                   END
073600130109     C     FUNKEY        IFEQ      '07'
073700130109     C                   EXSR      F07GES
073800130109     C                   END
073900970526     C     FUNKEY        IFEQ      '12'
074000970526     C                   EXSR      F12GES
074100970526     C                   END
074200970526     C     FUNKEY        IFEQ      '24'
074300970526     C                   EXSR      F24GES
074400970526     C                   END
074500970526     C                   END
074600970526     C*
074700970526     C                   ENDSR
074800970526     C*------------------------------------------------------------------------*
074900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
075000970526     C*------------------------------------------------------------------------*
075100970526     C     FUNVAR        BEGSR
075200970526     C*
075300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
075400970526     C*
075500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
075600970526     C     KEY           IFEQ      ENTER
075700970526     C                   MOVEL     *BLANKS       FUNKEY
075800970526     C                   END
075900970526     C     KEY           IFEQ      F1
076000970526     C                   MOVEL     '01'          FUNKEY
076100970526     C                   END
076200970526     C     KEY           IFEQ      F2
076300970526     C                   MOVEL     '02'          FUNKEY
076400970526     C                   END
076500970526     C     KEY           IFEQ      F3
076600970526     C                   MOVEL     '03'          FUNKEY
076700970526     C                   END
076800970526     C     KEY           IFEQ      F4
076900970526     C                   MOVEL     '04'          FUNKEY
077000970526     C                   END
077100970526     C     KEY           IFEQ      F5
077200970526     C                   MOVEL     '05'          FUNKEY
077300970526     C                   END
077400970526     C     KEY           IFEQ      F6
077500970526     C                   MOVEL     '06'          FUNKEY
077600970526     C                   END
077700970526     C     KEY           IFEQ      F7
077800970526     C                   MOVEL     '07'          FUNKEY
077900970526     C                   END
078000970526     C     KEY           IFEQ      F8
078100970526     C                   MOVEL     '08'          FUNKEY
078200970526     C                   END
078300970526     C     KEY           IFEQ      F9
078400970526     C                   MOVEL     '09'          FUNKEY
078500970526     C                   END
078600970526     C     KEY           IFEQ      F10
078700970526     C                   MOVEL     '10'          FUNKEY
078800970526     C                   END
078900970526     C     KEY           IFEQ      F11
079000970526     C                   MOVEL     '11'          FUNKEY
079100970526     C                   END
079200970526     C     KEY           IFEQ      F12
079300970526     C                   MOVEL     '12'          FUNKEY
079400970526     C                   END
079500970526     C     KEY           IFEQ      F13
079600970526     C                   MOVEL     '13'          FUNKEY
079700970526     C                   END
079800970526     C     KEY           IFEQ      F14
079900970526     C                   MOVEL     '14'          FUNKEY
080000970526     C                   END
080100970526     C     KEY           IFEQ      F15
080200970526     C                   MOVEL     '15'          FUNKEY
080300970526     C                   END
080400970526     C     KEY           IFEQ      F16
080500970526     C                   MOVEL     '16'          FUNKEY
080600970526     C                   END
080700970526     C     KEY           IFEQ      F17
080800970526     C                   MOVEL     '17'          FUNKEY
080900970526     C                   END
081000970526     C     KEY           IFEQ      F18
081100970526     C                   MOVEL     '18'          FUNKEY
081200970526     C                   END
081300970526     C     KEY           IFEQ      F19
081400970526     C                   MOVEL     '19'          FUNKEY
081500970526     C                   END
081600970526     C     KEY           IFEQ      F20
081700970526     C                   MOVEL     '20'          FUNKEY
081800970526     C                   END
081900970526     C     KEY           IFEQ      F21
082000970526     C                   MOVEL     '21'          FUNKEY
082100970526     C                   END
082200970526     C     KEY           IFEQ      F22
082300970526     C                   MOVEL     '22'          FUNKEY
082400970526     C                   END
082500970526     C     KEY           IFEQ      F23
082600970526     C                   MOVEL     '23'          FUNKEY
082700970526     C                   END
082800970526     C     KEY           IFEQ      F24
082900970526     C                   MOVEL     '24'          FUNKEY
083000970526     C                   END
083100970526     C     KEY           IFEQ      ROLLUP
083200970526     C                   MOVEL     'RU'          FUNKEY
083300970526     C                   END
083400970526     C     KEY           IFEQ      HELP
083500970526     C                   MOVEL     'HP'          FUNKEY
083600970526     C                   END
083700970526     C                   ENDSR
083800970526     C*------------------------------------------------------------------------*
083900970526     C* NOTE COSTRUZIONE SCHIERE
084000970526     C*------------------------------------------------------------------------*
084100970526     C*
084200970526     C* -MSG-MESSAGGI
084300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
084400970526     C* (NON TOCCARE MSG,1 E MSG,2)
084500970526     C*
084600970526     C* -CMD-COMANDI
084700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
084800970526     C*
084900970526     C* -OPD-OPZIONI
085000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
085100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
085200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
085300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
085400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
085500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
085600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
085700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
085800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
085900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
086000970526     C*
086100970526     C* -FUD-FUNZIONI
086200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
086300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
086400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
086500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
086600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
086700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
086800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
086900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
087000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
087100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
087200970526** MSG - MESSAGGI
087300120927Tasto funzionale non gestito                                                   1
087400120927Numero chiamate obbligatorio                                                   2
087500140116Libreria e file sorgente obbligatori                                           3
087600140312Sono obbligatorie le posizioni di RSD-IND-LOD                                  4
087700130108                                                                               5
087800130108Campo obbligatorio                                                             6
087900130108                                                                               7
088000130108                                                                               8
088100120927                                                                               9
088200120927                                                                               10
088300140630Codice tipo file inesistente in tabella                                        11
088400120927                                                                               12
088500120927                                                                               13
088600120927Data errata                                                                    14
088700140627Data DA > data A                                                               15
088800140701Massima Percentuale di errore = 100                                            16
088900120927                                                                               17
089000120927                                                                               18
089100120927                                                                               19
089200140702Estrazione conclusa                                                            20
089300120927Scelta obbligatoria                                                            21
089400130110Eseguita la stampa                                                             22
089500121108                                                                               23
089600121108                                                                               24
089700970526** FUD - FUNZIONI
089800970526                   -        (S/N)=N                        funz.01¬
089900970526                   -        (S/N)=N                        funz.02¬
090000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
090100970526                   -        (S/N)=N                        funz.04¬
090200970526                   -        (S/N)=N                        funz.05¬
090300140701F06=Estr.interatt. -        (S/N)=S                        funz.06¬
090400140701F07=Estr.batch     -        (S/N)=S                        funz.07¬
090500970527                   -        (S/N)=N                        funz.08¬
090600970527                   -        (S/N)=N                        funz.09¬
090700970527                   -        (S/N)=N                        funz.10¬
090800970526                   -        (S/N)=N                        funz.11¬
090900970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
091000970526                   -        (S/N)=N                        funz.13¬
091100970526                   -        (S/N)=N                        funz.14¬
091200970526                   -        (S/N)=N                        funz.15¬
091300970526                   -        (S/N)=N                        funz.16¬
091400970526                   -        (S/N)=N                        funz.17¬
091500970526                   -        (S/N)=N                        funz.18¬
091600970526                   -        (S/N)=N                        funz.19¬
091700970526                   -        (S/N)=N                        funz.20¬
091800970526                   -        (S/N)=N                        funz.21¬
091900970526                   -        (S/N)=N                        funz.22¬
092000970526                   -        (S/N)=N                        funz.23¬
092100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
