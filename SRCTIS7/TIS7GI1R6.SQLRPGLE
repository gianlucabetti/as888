000100970526     H*------------------------------------------------------------------------*
000200141009     H* Lancio cancellazione log download in errore
000300970526     H*------------------------------------------------------------------------*
000400140627     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500140627     h dftactgrp(*no) bnddir('UBBNDDIR')
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900141009     FTIS7GI1D6 cf   e             WORKSTN INFDS(DEVDS1)
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
002700141009     D*------------
002800141009     D* PASSAGGIO PARAMETRI IN INGRESSO
002900141009     D*------------
003000141009     D PAR_INP         DS
003100141009     D  DEPOP0                        3
003200141009     D  DEPKSC                        8
003300141009     D  DEPTIP                        2
003400141009     D  DEPISV                        2
003500141009     D*------------------------------------------------------------------------*
003600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003700970526     D*------------
003800970526     D FUDDS           DS
003900970526     D  FUDDE1                 1      1
004000970526     D  FUDFUN                 2      3
004100970526     D  FUDDE2                 4     19
004200970526     D  FUDFI0                20     34
004300970526     D  FUDATT                35     35
004400970526     D  FUDFI1                36     80
004500970526     D  FUDDES                 1     19
004600970526     D  FUDRIG                 1     80
004700970526     D*------------
004800970526     D* DS GESTIONE TASTI FUNZIONALI
004900970526     D*------------
005000970526     D DSKEY         E DS
005100970526     D DEVDS1          DS
005200970526     D  KEY                  369    369
005300970526     D*------------
005400970526     D* DS "XMSG" - GESTIONE MESSAGGI
005500970526     D*------------
005600970526     D DSMSG         E DS
005700970528     D*------------------
005800970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005900970528     D*------------------
006000970528     D WLBDA8          DS                  INZ
006100970528     D  G08DAT                 1      8  0
006200970528     D  G08INV                 9     16  0
006300970528     D  G08ERR                17     17
006400970528     D  G08TGI                18     22  0
006500970526     D*------------
006600970526     D* ARCHITETTURA
006700970526     D*------------
006800970526     D KPJBA         E DS
006900020923     D*-------------------
007000020923     D* DS REPERIMENTO DATI UTENTE
007100020923     D*-------------------
007200020923     D TIBS34DS      E DS                                                       *Profili utente
007300020923     D DDATIUTE      E DS                                                       *Dati utente
007400020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007500120927     D* DS REPERIMENTO PADRE/FIGLI
007600120927     D TIBS10DS      E DS
007700140630     D*-------------------
007800140630     D* TABELLE "XCLIR"- RICERCA CLIENTI
007900140630     D ksc             s              7  0 dim(30)
008000140630     D ksa             s              4    dim(30)
008100140630     D isvkun          s              2    dim(100)
008200140630     D isvtt           s              2    dim(100)
008300140630     D isvftp          s              2    dim(100)
008400140630     D isvKEYS         s              2    dim(100)
008500140630     D isvCTRL         s              4    dim(100)
008600140630     D isvPGMAFT       s              3    dim(100)
008700140630     D*-------------------
008800140630     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
008900140630     D*-------------------
009000140630     DDSXC           e DS                  extname(XCLIRDS)
009100140630     D*-------------------
009200140630     D                 DS
009300140630     D dsksn                   1      4p 0
009400140630     D dsksa                   1      4
009500140630     D*-------------------
009600140630     D TIBS02DS      E DS
009700120928     D*-------------------
009800120928
009900121003     d Status         sds
010000121003     d  SDSpgm           *proc
010100121003     d  SDSprm           *parms
010200121003     d  SDSuser              254    263
010300121003
010400141009     d DS7GIR7         ds
010500140701     d pIn_Ksc                        8
010600140701     d pIn_Tip                        2
010700140904     d pIn_ISV                        2
010800141009     d pIn_Dat                        8s 0
010900130116
011000130116     d ora             s               t
011100140117     d wX              s              2s 0
011200140627     d wData08         s              8s 0
011300141009     d wDat            s              8s 0
011400141009       // Stringa SQL da eseguire
011500141009     d wSQL            s           2048    inz  varying
011600141009
011700120928
011800140627      /COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
011900140627
012000970526     C*------------------------------------------------------------------------*
012100000303     C* MAIN LINES
012200970526     C*------------------------------------------------------------------------*
012300970526     C*
012400120927     C* metto i default su D03
012500120927     C                   EXSR      InzD03
012600120927     C*
012700120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012800991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013100141010     C*
013200141010     C* chiamato
013300141010     C                   IF        DepOp0 <> *blank
013400141010     C* proteggo i campi di selezione tranne la data
013500141010     C                   SETON                                        30
013600141010     C                   ENDIF
013700970526     C*
013800970526     C* EMETTE FORMATO VIDEO
013900970526     C                   EXSR      BUTFMR
014000970526     C*
014100970526     C* GESTIONE TASTI FUNZIONALI
014200970526     C                   EXSR      FUNCON
014300970526E   2C                   ENDDO
014400970526     C*
014500970526     C* FINE PGM
014600970526     C                   SETON                                        LR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014900970526     C*------------------------------------------------------------------------*
015000970526     C     BUTFMR        BEGSR
015100970526     C*
015200970526     C* EMETTE LA TESTATA
015300140904     C                   WRITE     S7GID01
015400970526     C*
015500970526     C* SPEDISCE EVENTUALE MESSAGGIO
015600970526     C                   EXSR      SNDMSG
015700970526     C*
015800970526     C* EMETTE LE FUNZIONI
015900140904     C                   WRITE     S7GID02
016000970526     C*
016100970526     C* EMETTE IL VIDEO
016200140904     C                   EXFMT     S7GID03
016300970526     C*
016400991214     C* REIMPOSTA FLAG DI ERRORE
016500991214     C                   MOVEL     '0'           ERRL00
016600970526     C*
016700970526     C* REIMPOSTA INDICATORI DI ERRORE
016800140702     C                   SETOFF                                       515253
016900140702     C                   SETOFF                                       545556
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300120927     C* InzD03 - Inizializzo D03
017400970526     C*------------------------------------------------------------------------*
017500120927     C     InzD03        BEGSR
017600120927     C*
017700141009     C                   EVAL      VIDDat = 0
017800140627     C                   EVAL      VIDKsc= *blank
017900140627     C                   EVAL      VIDTip= *blank
018000140904     C                   EVAL      VIDISV= *blank
018100141009     C* chiamato
018200141009     C                   IF        DepOp0 <> *blank
018300141009     C                   EVAL      VIDKsc= DepKsc
018400141009     C                   EVAL      VIDTip= DepTip
018500141009     C                   EVAL      VIDISV= DepISV
018600141010     C* proteggo i campi di selezione tranne la data
018700141009     C                   SETON                                        30
018800141009     C                   ENDIF
018900130108     C*
019000120927     C                   ENDSR
019100120927     C*------------------------------------------------------------------------*
019200120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
019300120927     C*------------------------------------------------------------------------*
019400120927     C     SNDMSG        BEGSR
019500120927     C*
019600970526     C* PULISCE IL SFILE MESSAGGI
019700970526     C                   MOVEL     '1'           *IN40
019800140904     C                   WRITE     S7GIDMC
019900970526     C                   MOVEL     '0'           *IN40
020000970526     C*
020100970526     C* VISUALIZZA IL MESSAGGIO
020200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
020300970526     C                   CALL      'XMSG'        XMSGPA
020400020611     C     DSMSER        IFEQ      *ZEROS
020500970526     C                   MOVEL     DSMSKE        MSGKEY
020600020611     C                   ENDIF
020700970526     C                   Z-ADD     1             MSGULT
020800140904     C                   WRITE     S7GIDMS
020900140904     C                   WRITE     S7GIDMC
021000970526     C*
021100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
021200970526     C                   MOVEL     *BLANKS       DSMSMS
021300970526     C*
021400970526     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600970526     C* ENTGES - GESTIONE ENTER -> INVIO
021700970526     C*------------------------------------------------------------------------*
021800970526     C     ENTGES        BEGSR
021900970526     C*
022000970526     C* CONTROLLA I CAMPI IMMESSI
022100970526     C                   EXSR      DECODI
022200970526     C*
022300970526     C                   ENDSR
022400970526     C*------------------------------------------------------------------------*
022500970526     C* F03GES - GESTIONE F03 -> FINE
022600970526     C*------------------------------------------------------------------------*
022700970526     C     F03GES        BEGSR
022800970526     C*
022900970526     C* IMPOSTA "PREMUTO F03"
023000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400140701     C* F06GES - GESTIONE F06 -> Estrazione interattiva
023500970526     C*------------------------------------------------------------------------*
023600970526     C     F06GES        BEGSR
023700970526     C*
023800991214     C* CONTROLLA I CAMPI IMMESSI
023900991214     C                   EXSR      DECODI
024000020611     C*
024100991214     C* ESEGUE ROUTINE DI CONFERMA
024200020611     C     ERRL00        IFEQ      '0'
024300991214     C                   EXSR      ESEGUI
024400121109     C* non esco mai dal pgm
024500121109     C***                MOVEL     '1'           FINL00
024600121108     C                   ENDIF
024700970526     C*
024800991214     C                   ENDSR
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
026700141009     C* ESEGUI - Eliminazione fisica log in errore
026800970526     C*------------------------------------------------------------------------*
026900991214     C     ESEGUI        BEGSR
027000141009     C*
027100141009     C                   DO        *hival
027200141009     C                   EXFMT     S7GIW01
027300141009     C                   SELECT
027400141009     C* F12=Annulla
027500141009     C                   WHEN      *inkl
027600141009     C                   LEAVESR
027700141009     C* F6=Conferma
027800141009     C                   WHEN      *inkf
027900141009     C                   EXSR      AnnullaVLT
028000141009     C                   LEAVESR
028100141009     C                   ENDSL
028200141009     C                   ENDDO
028300991214     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600141009     C* AnnullaVLT - annulla log
028700970526     C*------------------------------------------------------------------------*
028800141009     C     AnnullaVLT    BEGSR
028900121019     C*
029000141009        /free
029100141009         // preparazione estrazione
029200141009         // base è l'SQL su TIVLT che estrae per i parametri indicati
029300141009         wSQL = 'delete +
029400141009                 from TIVLT00F ';
029500141009         wSQL = %trimr(wSQL) + ' where';
029600141009         if VidDat > 0;
029700141009           wSQL = %trimr(wSQL) + ' VLTDAT between 0 and ' +
029800141010                  %char(wDat) + ' and ';
029900141009         endif;
030000141009         //cliente
030100141009         wSQL = %trimr(wSQL) + ' VLTKSC = ''' +
030200141009                  VidKsc + '''';
030300141009         //tipo file
030400141009         wSQL = %trimr(wSQL) + ' and';
030500141009         wSQL = %trimr(wSQL) + ' VLTTIP = ''' +
030600141009                VidTip + '''';
030700141009         //tipo servizio
030800141009         wSQL = %trimr(wSQL) + ' and';
030900141009         wSQL = %trimr(wSQL) + ' VLTISV = ''' +
031000141009                VidISV + '''';
031100141009         wSQL = %trimr(wSQL) + ' and';
031200141009         wSQL = %trimr(wSQL) + ' VLTSTS in (''2'', ''4'')';
031300141009
031400141009         exec sql   execute immediate  :wSQL;
031500141009
031600141009         // emetto messaggio a seconda del risultato
031700141009         select;
031800141009           when sqlcod < 0;
031900141009             dsmsms = msg(18);
032000141009           when sqlcod = 100;
032100141009             dsmsms = msg(19);
032200141009           other;
032300141009             dsmsms = msg(20);
032400141009         endsl;
032500141009        /end-free
032600141009     C*
032700141009     C                   ENDSR
032800141009     C*------------------------------------------------------------------------*
032900141009     C* DECODI - CONTROLLA CAMPI E DECODIFICA
033000141009     C*------------------------------------------------------------------------*
033100141009     C     DECODI        BEGSR
033200141009     C*
033300140702     C                   SETOFF                                       515253
033400140702     C                   SETOFF                                       545556
033500140627     C                   EVAL      VIDMSG    = *blank
033600141009     C                   EVAL      wDat = 0
033700140630     C*
033800140630     C     ERRL00        IFEQ      '0'
033900141009     C*
034000141009     C* campo obbligatorio
034100141009     C                   IF        VidKsc = *blank
034200141009     C                   movel     '1'           errl00
034300141009     C                   seton                                        52
034400141009     C                   MOVEL     MSG(6)        DSMSMS
034500141009     C                   LEAVESR
034600141009     C                   ELSE
034700140630     C* se richiesto elenco clienti
034800140630     C                   SETOFF                                           28
034900140630     C     '?'           SCAN      VIDKsc                                 28
035000140630IF  1C     *IN28         IFEQ      *ON
035100140630     C* RICHIESTO ELENCO CODICI
035200140630     C* CLIENTI
035300140630     C                   CLEAR                   DSXC
035400140630     C                   Z-ADD     DUTKCI        DXCCAP
035500140630     C                   CALL      'XCLIR'
035600140630     C                   PARM                    DSXC
035700140630     C* SCOMPATTA
035800140630     C                   MOVEA     DXCKSC        KSA                        ALF
035900140630DO  2C     1             DO        30            J                 3 0
036000140630     C                   MOVE      KSA(J)        DSKSA                      F
036100140630     C                   Z-ADD     DSKSN         KSC(J)                     M
036200140630E   2C                   ENDDO
036300140630     C                   MOVEL     KSC(1)        WKSC              7
036400140630     C                   MOVEL     *all'0'       VIDKsc
036500140630     C                   MOVE      WKSC          VIDKsc
036600140630E   1C                   ENDIF
036700140630     C*
036800140630     C* CODICE CLIENTE NUMERICO
036900140630IF  1C     WKSC          IFLT      *ZEROS
037000140630     C                   MOVEL     '1'           WERR              1
037100140630X   1C                   ELSE
037200140630     C*
037300140630     C* CONTROLLA ESISTENZA CODICE
037400140630     C* nessun controllo
037500140630E   1C                   ENDIF
037600141009E   1C                   ENDIF
037700120927     C*
037800140630E   1C                   ENDIF
037900140630     C*
038000140630     C     ERRL00        IFEQ      '0'
038100141015     C* se il dato non mi è arrivato
038200141015     C                   IF        DEPOP0 = *blank
038300140630     C* controllo esistenza tipo file
038400140630     C                   IF        VIDTip <> *blank
038500140630E   1C                   EXSR      CntTip
038600141009     C                   ELSE
038700141009     C* campo obbligatorio
038800141009     C                   movel     '1'           errl00
038900141009     C                   seton                                        54
039000141009     C                   MOVEL     MSG(6)        DSMSMS
039100140630E   1C                   ENDIF
039200141015E   1C                   ENDIF
039300140630E   1C*
039400140630E   1C                   ENDIF
039500141009     C*
039600141009     C     ERRL00        IFEQ      '0'
039700141015     C* se il dato non mi è arrivato
039800141015     C                   IF        DEPOP0 = *blank
039900141009     C* controllo esistenza tipo servizio
040000141009     C                   IF        VIDISV <> *blank
040100141009E   1C                   EXSR      CntIsv
040200141009     C                   ELSE
040300141009     C* campo obbligatorio
040400141009     C                   movel     '1'           errl00
040500141009     C                   seton                                        53
040600141009     C                   MOVEL     MSG(6)        DSMSMS
040700141009E   1C                   ENDIF
040800141015E   1C                   ENDIF
040900141009E   1C*
041000141009E   1C                   ENDIF
041100141009     C*
041200141009     C     ERRL00        IFEQ      '0'
041300141009     C* se scritta, la data deve essere corretta
041400141009     C                   IF        VIDDat>0
041500141009     C* se ci sono più di 6 caratteri deve essere una data in formato ggmmaaaa
041600141009     C                   IF        VIDDat>999999
041700141009     C                   EVAL      wData08 = %dec(%subst(
041800141009     C                              UBFMTDATE_Convert(
041900141009     C                              %editc(VIDDat:'X'):
042000141009     C                              'DDMMYYYY':
042100141009     C                              'YYYYMMDD') :
042200141009     C                              1 : 8) : 8 : 0)
042300141009     C                   IF        wData08 = VIDDat
042400141009     C                   MOVEL     '1'           ERRL00
042500141009     C                   SETON                                        51
042600141009     C                   MOVEL     MSG(14)       DSMSMS
042700141009     C                   ELSE
042800141009     C                   EVAL      wDat = wData08
042900141009     C                   ENDIF
043000141009     C                   ELSE
043100141009     C                   EVAL      wData08 = %dec(%subst(
043200141009     C                              UBFMTDATE_Convert(
043300141009     C                              %editc(%dec(VIDDat:6:0):'X'):
043400141009     C                              'DDMMYY':
043500141009     C                              'YYYYMMDD') :
043600141009     C                              1 : 8) : 8 : 0)
043700141009     C                   IF        wData08 = VIDDat
043800141009     C                   MOVEL     '1'           ERRL00
043900141009     C                   SETON                                        51
044000141009     C                   MOVEL     MSG(14)       DSMSMS
044100141009     C                   ELSE
044200141009     C* imposto a video la data con l'anno di 4
044300141009     C                   EVAL      wDat = wData08
044400141009     C                   EVAL      VIDDat = %dec(
044500141009     C                              %subst(%editc(wData08:'X'):7:2) +
044600141009     C                              %subst(%editc(wData08:'X'):5:2) +
044700141009     C                              %subst(%editc(wData08:'X'):1:4):8:0)
044800141009     C                   ENDIF
044900141009     C                   ENDIF
045000141009     C                   ENDIF
045100141009     C*
045200141009     C                   ENDIF
045300140630     C*
045400970528     C                   ENDSR
045500140630     C*------------------------------------------------------------------------*
045600140630     C* Cnttip - CONTROLLA codice tipo legame
045700140630     C*------------------------------------------------------------------------*
045800140630     C     cnttip        begsr
045900141009     C*
046000140630     C                   clear                   TIBS02DS
046100140630     C*
046200140630     C     '?'           SCAN      VIDTip                                 30
046300140630     C*  Ricerca
046400140630     C                   eval      t02tla = 'L'
046500140630    1C                   IF        *IN30 = *ON
046600140630     C                   movel     'R'           t02mod
046700140630     C                   else
046800140630     C                   movel     'C'           t02mod
046900140630     C                   movel(p)  VIDTip        t02ke1
047000140630     C                   end
047100140630     C                   movel     'ITF'         t02cod
047200140630     C                   movel     knsif         t02sif
047300140630     C                   CALL      'TIBS02R'
047400140630     C                   parm                    KPJBA
047500140630     C                   parm                    TIBS02DS
047600140630     C
047700140630     C*
047800140630    2C                   IF        T02err = *blanks
047900140630     C                   movel     T02ke1        VIDTip
048000140630     C                   else
048100140630     C                   movel     '1'           errl00
048200140630     C                   MOVEL     msg(11)       DSMSMS
048300140630     C                   SETON                                        54
048400140630     C                   ENDIF
048500140630     C
048600140630     C                   endsr
048700141009     C*------------------------------------------------------------------------*
048800141009     C* Cntisv - CONTROLLA codice tipo legame
048900141009     C*------------------------------------------------------------------------*
049000141009     C     cntisv        begsr
049100141009     C*
049200141009     C                   clear                   TIBS02DS
049300141009     C*
049400141009     C     '?'           SCAN      VIDISV                                 30
049500141009     C*  Ricerca
049600141009     C                   eval      t02tla = 'L'
049700141009    1C                   IF        *IN30 = *ON
049800141009     C                   movel     'R'           t02mod
049900141009     C                   else
050000141009     C                   movel     'C'           t02mod
050100141009     C                   movel(p)  VIDISV        t02ke1
050200141009     C                   end
050300141009     C                   movel     'ISV'         t02cod
050400141009     C                   movel     knsif         t02sif
050500141009     C                   CALL      'TIBS02R'
050600141009     C                   parm                    KPJBA
050700141009     C                   parm                    TIBS02DS
050800141009     C
050900141009     C*
051000141009    2C                   IF        T02err = *blanks
051100141009     C                   movel     T02ke1        VIDISV
051200141009     C                   else
051300141009     C                   movel     '1'           errl00
051400141009     C                   seton                                        53
051500141009     C                   MOVEL     MSG(9)        DSMSMS
051600141009     C                   ENDIF
051700141009     C*
051800141009     C                   endsr
051900970526     C*------------------------------------------------------------------------*
052000970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
052100970526     C*------------------------------------------------------------------------*
052200970526     C     AZZVID        BEGSR
052300970526     C* CAMPI
052400970526     C*
052500970526     C                   ENDSR
052600970526     C*------------------------------------------------------------------------*
052700020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
052800970526     C*------------------------------------------------------------------------*
052900970526     C     IMPFUN        BEGSR
053000970526     C*
053100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
053200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
053300970526     C                   EXSR      FUNGES
053400970526     C*
053500970526     C                   ENDSR
053600020923     C*--------------------------------------------------------------------------------------------*
053700020923     C* REPERISCE I DATI UTENTE
053800020923     C*--------------------------------------------------------------------------------------------*
053900020923     C     REPDATIUTE    BEGSR
054000020923     C*
054100020923     C* INIZIALIZZA VARIABILI DI WRK
054200020923     C                   CLEAR                   TIBS34DS
054300020923     C                   CLEAR                   AZUTEDS
054400020923     C                   CLEAR                   DDATIUTE
054500020923     C*
054600020923     C     *DTAARA       DEFINE    §azute        azuteds
054700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
054800020923     C                   IN(E)     *DTAARA
054900020923if  1C                   IF        %Error
055000020923     c                   EVAL      I34Tla = 'L'
055100020923     C                   CALL      'TIBS34R'
055200020923     C                   PARM                    Tibs34Ds
055300020923     C                   IN        *DTAARA
055400020923e   1C                   ENDIF
055500020923     C*
055600020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
055700020923     C                   MOVEL(P)  RSUT          DSFIRS           20
055800020923     C*
055900020923     C                   ENDSR
056000970526     C*------------------------------------------------------------------------*
056100970526     C* *INZSR - OPERAZIONI INIZIALI
056200970526     C*------------------------------------------------------------------------*
056300970526     C     *INZSR        BEGSR
056400991217     C*
056500991217     C     *ENTRY        PLIST
056600991217     C                   PARM                    KPJBA
056700141009     C                   MOVEL     KPJBU         PAR_INP
056800020923     C*
056900020923     C* REPERISCE I DATI DELL'UTENTE
057000020923     C                   EXSR      REPDATIUTE
057100970526     C*
057200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
057300970526     C     XMSGPA        PLIST
057400970526     C                   PARM                    DSMSMS
057500970526     C                   PARM                    DSMSPG
057600970526     C                   PARM                    DSMSKE
057700970526     C                   PARM                    DSMSER
057800970526     C*
057900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
058000970526     C                   CALL      'XKEY'
058100970526     C                   PARM                    DSKEY
058200970526     C*---
058300970526     C* VARIABILI NON RIFERITE AL DATA BASE
058400970526     C*---
058500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
058600991214     C                   MOVEL     *BLANKS       DEPOP1            3
058700970526     C*---
058800970526     C* VARIABILI DI CONTROLLO
058900970526     C*---
059000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
059100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
059200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
059300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
059400970526     C*
059500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
059600140904     C                   MOVEL     'TIS7GI1R1'   DSMSPG
059700140904     C                   MOVEL     'TIS7GI1R1'   MSGQUE
059800970526     C                   MOVEL     *BLANKS       MSGKEY
059900970526     C                   MOVEL     *BLANKS       DSMSMS
060000970528     C*
060100970528     C* CALCOLA LA DATA CORRENTE
060200970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
060300970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
060400970528     C                   Z-ADD     WN8           G08DAT
060500970528     C                   Z-ADD     *ZEROS        G08INV
060600970528     C                   MOVEL     '0'           G08ERR
060700970528     C                   CALL      'XSRDA8'
060800970528     C                   PARM                    WLBDA8
060900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
061000000310     C*
061100000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
061200000310     C                   EXSR      AZZVID
061300970526     C*
061400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
061500970526     C                   EXSR      IMPFUN
061600970526     C*
061700970526     C                   ENDSR
061800970526     C*------------------------------------------------------------------------*
061900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
062000970526     C*------------------------------------------------------------------------*
062100970526     C     FUNGES        BEGSR
062200970526     C*
062300970526     C* PULISCO CAMPI DI LAVORO
062400970526     C                   Z-ADD     0             W                 5 0
062500970526     C*
062600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
062700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
062800970526     C     1             DO        24            I
062900970526     C                   MOVEL     FUD(I)        FUDRIG
063000970526     C*
063100970526     C* PULIZIA FUNZIONI PRECEDENTI
063200970526     C                   MOVEL     *BLANKS       FUV(I)
063300970526     C*
063400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
063500970526     C* (SALTA F24)
063600970526     C     FUDATT        IFEQ      'S'
063700970526     C     FUDFUN        ANDNE     '24'
063800970526     C                   ADD       1             W
063900970526     C                   MOVEL     FUDDES        FUV(W)
064000970526     C                   END
064100970526     C                   END                                                    DO WHILE
064200970526     C*
064300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
064400970526     C* ALTRIMENTI DISATTIVO F24
064500970526     C     1             DO        24            I
064600970526     C                   MOVEL     FUD(I)        FUDRIG
064700970526     C*
064800970526     C     FUDFUN        IFEQ      '24'
064900970526     C     W             IFGT      6
065000970526     C                   MOVEL     'S'           FUDATT
065100970526     C                   ELSE
065200970526     C                   MOVEL     *BLANKS       FUDATT
065300970526     C                   END                                                    W GT 8
065400970526     C                   MOVEL     FUDRIG        FUD(I)
065500970526     C                   END
065600970526     C                   END                                                    DO WHILE
065700970526     C*
065800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
065900970526     C     W             DIV       6             I
066000970526     C                   MVR                     Z
066100970526     C*
066200970526     C     Z             IFNE      0
066300970526     C     I             ADD       1             Z
066400970526     C                   ELSE
066500970526     C                   Z-ADD     I             Z                 5 0
066600970526     C                   END
066700970526     C*
066800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
066900970526     C                   ADD       1             FUNGRU
067000970526     C     FUNGRU        IFGT      Z
067100970526     C     FUNGRU        ORLT      1
067200970526     C                   Z-ADD     1             FUNGRU
067300970526     C                   END
067400970526     C*
067500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
067600970526     C     FUNGRU        IFEQ      1
067700970526     C                   MOVEL     FUV(1)        FUN001
067800970526     C                   MOVEL     FUV(2)        FUN002
067900970526     C                   MOVEL     FUV(3)        FUN003
068000970526     C                   MOVEL     FUV(4)        FUN004
068100970526     C                   MOVEL     FUV(5)        FUN005
068200970526     C                   MOVEL     FUV(6)        FUN006
068300970526     C                   END
068400970526     C*
068500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
068600970526     C     FUNGRU        IFEQ      2
068700970526     C                   MOVEL     FUV(7)        FUN001
068800970526     C                   MOVEL     FUV(8)        FUN002
068900970526     C                   MOVEL     FUV(9)        FUN003
069000970526     C                   MOVEL     FUV(10)       FUN004
069100970526     C                   MOVEL     FUV(11)       FUN005
069200970526     C                   MOVEL     FUV(12)       FUN006
069300970526     C                   END
069400970526     C*
069500970526     C* TERZO GRUPPO DI 6 FUNZIONI
069600970526     C     FUNGRU        IFEQ      3
069700970526     C                   MOVEL     FUV(13)       FUN001
069800970526     C                   MOVEL     FUV(14)       FUN002
069900970526     C                   MOVEL     FUV(15)       FUN003
070000970526     C                   MOVEL     FUV(16)       FUN004
070100970526     C                   MOVEL     FUV(17)       FUN005
070200970526     C                   MOVEL     FUV(18)       FUN006
070300970526     C                   END
070400970526     C*
070500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
070600970526     C     FUNGRU        IFEQ      4
070700970526     C                   MOVEL     FUV(19)       FUN001
070800970526     C                   MOVEL     FUV(20)       FUN002
070900970526     C                   MOVEL     FUV(21)       FUN003
071000970526     C                   MOVEL     FUV(22)       FUN004
071100970526     C                   MOVEL     *BLANKS       FUN005
071200970526     C                   MOVEL     *BLANKS       FUN006
071300970526     C                   END
071400970526     C*
071500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
071600970526     C                   MOVEL     *BLANKS       FUN007
071700970526     C                   MOVEL     *BLANKS       FUN008
071800970526     C*
071900970526     C     1             DO        24            I
072000970526     C                   MOVEL     FUD(I)        FUDRIG
072100970526     C     FUDFUN        IFEQ      '24'
072200970526     C     FUDATT        ANDEQ     'S'
072300970526     C                   MOVEL     FUDDES        FUN008
072400970526     C                   END
072500970526     C                   END                                                    DO
072600970526     C*
072700970526     C                   ENDSR
072800970526     C*------------------------------------------------------------------------*
072900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
073000970526     C*------------------------------------------------------------------------*
073100970526     C     FUNCON        BEGSR
073200970526     C*
073300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
073400970526     C                   EXSR      FUNVAR
073500970526     C*
073600970526     C* PULISCO CAMPI DI LAVORO
073700970526     C                   Z-ADD     0             I                 5 0
073800970526     C                   Z-ADD     0             W                 5 0
073900970526     C*
074000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
074100970526     C     FUNKEY        IFNE      *BLANKS
074200970526     C     1             DO        24            I
074300970526     C                   MOVEL     FUD(I)        FUDRIG
074400970526     C*
074500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
074600970526     C     FUNKEY        IFEQ      FUDFUN
074700970526     C     FUDATT        ANDEQ     'S'
074800970526     C                   Z-ADD     I             W
074900970526     C                   END
075000970526     C                   END                                                    DO WHILE
075100970526     C*
075200970526     C* FUNZIONE NON ATTIVA (ERRORE)
075300970526     C     W             IFEQ      0
075400970526     C*
075500970526     C* EMISSIONE MESSAGGIO DI ERRORE
075600970526     C                   MOVEL     MSG(1)        DSMSMS
075700970526     C*
075800970526     C                   END                                                    W=0
075900970526     C                   END                                                    FUNKEY NE *BLANKS
076000970526     C*
076100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
076200970526     C     W             IFNE      0
076300970526     C     FUNKEY        OREQ      *BLANKS
076400970526     C     FUNKEY        IFEQ      *BLANKS
076500970526     C                   EXSR      ENTGES
076600970526     C                   END
076700970526     C     FUNKEY        IFEQ      '03'
076800970526     C                   EXSR      F03GES
076900970526     C                   END
077000970526     C     FUNKEY        IFEQ      '06'
077100970526     C                   EXSR      F06GES
077200970526     C                   END
077300970526     C     FUNKEY        IFEQ      '12'
077400970526     C                   EXSR      F12GES
077500970526     C                   END
077600970526     C     FUNKEY        IFEQ      '24'
077700970526     C                   EXSR      F24GES
077800970526     C                   END
077900970526     C                   END
078000970526     C*
078100970526     C                   ENDSR
078200970526     C*------------------------------------------------------------------------*
078300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
078400970526     C*------------------------------------------------------------------------*
078500970526     C     FUNVAR        BEGSR
078600970526     C*
078700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
078800970526     C*
078900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
079000970526     C     KEY           IFEQ      ENTER
079100970526     C                   MOVEL     *BLANKS       FUNKEY
079200970526     C                   END
079300970526     C     KEY           IFEQ      F1
079400970526     C                   MOVEL     '01'          FUNKEY
079500970526     C                   END
079600970526     C     KEY           IFEQ      F2
079700970526     C                   MOVEL     '02'          FUNKEY
079800970526     C                   END
079900970526     C     KEY           IFEQ      F3
080000970526     C                   MOVEL     '03'          FUNKEY
080100970526     C                   END
080200970526     C     KEY           IFEQ      F4
080300970526     C                   MOVEL     '04'          FUNKEY
080400970526     C                   END
080500970526     C     KEY           IFEQ      F5
080600970526     C                   MOVEL     '05'          FUNKEY
080700970526     C                   END
080800970526     C     KEY           IFEQ      F6
080900970526     C                   MOVEL     '06'          FUNKEY
081000970526     C                   END
081100970526     C     KEY           IFEQ      F7
081200970526     C                   MOVEL     '07'          FUNKEY
081300970526     C                   END
081400970526     C     KEY           IFEQ      F8
081500970526     C                   MOVEL     '08'          FUNKEY
081600970526     C                   END
081700970526     C     KEY           IFEQ      F9
081800970526     C                   MOVEL     '09'          FUNKEY
081900970526     C                   END
082000970526     C     KEY           IFEQ      F10
082100970526     C                   MOVEL     '10'          FUNKEY
082200970526     C                   END
082300970526     C     KEY           IFEQ      F11
082400970526     C                   MOVEL     '11'          FUNKEY
082500970526     C                   END
082600970526     C     KEY           IFEQ      F12
082700970526     C                   MOVEL     '12'          FUNKEY
082800970526     C                   END
082900970526     C     KEY           IFEQ      F13
083000970526     C                   MOVEL     '13'          FUNKEY
083100970526     C                   END
083200970526     C     KEY           IFEQ      F14
083300970526     C                   MOVEL     '14'          FUNKEY
083400970526     C                   END
083500970526     C     KEY           IFEQ      F15
083600970526     C                   MOVEL     '15'          FUNKEY
083700970526     C                   END
083800970526     C     KEY           IFEQ      F16
083900970526     C                   MOVEL     '16'          FUNKEY
084000970526     C                   END
084100970526     C     KEY           IFEQ      F17
084200970526     C                   MOVEL     '17'          FUNKEY
084300970526     C                   END
084400970526     C     KEY           IFEQ      F18
084500970526     C                   MOVEL     '18'          FUNKEY
084600970526     C                   END
084700970526     C     KEY           IFEQ      F19
084800970526     C                   MOVEL     '19'          FUNKEY
084900970526     C                   END
085000970526     C     KEY           IFEQ      F20
085100970526     C                   MOVEL     '20'          FUNKEY
085200970526     C                   END
085300970526     C     KEY           IFEQ      F21
085400970526     C                   MOVEL     '21'          FUNKEY
085500970526     C                   END
085600970526     C     KEY           IFEQ      F22
085700970526     C                   MOVEL     '22'          FUNKEY
085800970526     C                   END
085900970526     C     KEY           IFEQ      F23
086000970526     C                   MOVEL     '23'          FUNKEY
086100970526     C                   END
086200970526     C     KEY           IFEQ      F24
086300970526     C                   MOVEL     '24'          FUNKEY
086400970526     C                   END
086500970526     C     KEY           IFEQ      ROLLUP
086600970526     C                   MOVEL     'RU'          FUNKEY
086700970526     C                   END
086800970526     C     KEY           IFEQ      HELP
086900970526     C                   MOVEL     'HP'          FUNKEY
087000970526     C                   END
087100970526     C                   ENDSR
087200970526     C*------------------------------------------------------------------------*
087300970526     C* NOTE COSTRUZIONE SCHIERE
087400970526     C*------------------------------------------------------------------------*
087500970526     C*
087600970526     C* -MSG-MESSAGGI
087700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
087800970526     C* (NON TOCCARE MSG,1 E MSG,2)
087900970526     C*
088000970526     C* -CMD-COMANDI
088100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
088200970526     C*
088300970526     C* -OPD-OPZIONI
088400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
088500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
088600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
088700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
088800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
089000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
089100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
089200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
089300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
089400970526     C*
089500970526     C* -FUD-FUNZIONI
089600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
089700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
089800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
089900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
090000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
090100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
090200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
090300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
090400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
090500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
090600970526** MSG - MESSAGGI
090700120927Tasto funzionale non gestito                                                   1
090800120927Numero chiamate obbligatorio                                                   2
090900140116Libreria e file sorgente obbligatori                                           3
091000141009                                                                               4
091100130108                                                                               5
091200130108Campo obbligatorio                                                             6
091300130108                                                                               7
091400130108                                                                               8
091500141009Codice tipo servizio inesistente in tabella                                    9
091600120927                                                                               10
091700140630Codice tipo file inesistente in tabella                                        11
091800120927                                                                               12
091900120927                                                                               13
092000120927Data errata                                                                    14
092100141009                                                                               15
092200141009                                                                               16
092300120927                                                                               17
092400141009Cancellazione andata in errore SQL                                             18
092500141009Non trovati rcd da cancellare                                                  19
092600141009Cancellazione eseguita                                                         20
092700120927Scelta obbligatoria                                                            21
092800130110Eseguita la stampa                                                             22
092900121108                                                                               23
093000121108                                                                               24
093100970526** FUD - FUNZIONI
093200970526                   -        (S/N)=N                        funz.01¬
093300970526                   -        (S/N)=N                        funz.02¬
093400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
093500970526                   -        (S/N)=N                        funz.04¬
093600970526                   -        (S/N)=N                        funz.05¬
093700141009F06=Conferma       -        (S/N)=S                        funz.06¬
093800141009                   -        (S/N)=S                        funz.07¬
093900970527                   -        (S/N)=N                        funz.08¬
094000970527                   -        (S/N)=N                        funz.09¬
094100970527                   -        (S/N)=N                        funz.10¬
094200970526                   -        (S/N)=N                        funz.11¬
094300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
094400970526                   -        (S/N)=N                        funz.13¬
094500970526                   -        (S/N)=N                        funz.14¬
094600970526                   -        (S/N)=N                        funz.15¬
094700970526                   -        (S/N)=N                        funz.16¬
094800970526                   -        (S/N)=N                        funz.17¬
094900970526                   -        (S/N)=N                        funz.18¬
095000970526                   -        (S/N)=N                        funz.19¬
095100970526                   -        (S/N)=N                        funz.20¬
095200970526                   -        (S/N)=N                        funz.21¬
095300970526                   -        (S/N)=N                        funz.22¬
095400970526                   -        (S/N)=N                        funz.23¬
095500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
