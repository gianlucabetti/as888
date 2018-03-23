000100970526     H*------------------------------------------------------------------------*
000200140701     H* Gestione log upload in errore
000300970526     H*------------------------------------------------------------------------*
000400140703
000500140703      * per compilare questo pgm bisogna avere in lista di librerie il file WFLUE00F che viene,
000600140703      * però, creato e distrutto da questi pgm (è una table SQL).
000700140703      * quindi crearlo secondo lo statement in SRCTNDB nella propria libreria e mettere questa
000800140703      * in lista temporaneamente, ricordandosi di toglierla prima di lanciare l'esecuzione.
000900140703
001000140704     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR')
001100970526     H DECEDIT('0,') DATEDIT(*DMY.)
001200040121     F*------------------------------------------------------------------------*
001300970526     F* DATA BASE
001400970526     F*------------------------------------------------------------------------*
001500140701     Ftianv01l  IF   E           K DISK
001600140701     Ftntbe02l  IF   E           K DISK
001700140701     FTIS760D4  CF   E             WORKSTN INFDS(DEVDS1)
001800991105     F                                     SFILE(s760D05:SFLULT)
001900991105     F                                     SFILE(s760DMS:MSGULT)
002000970526     D*------------------------------------------------------------------------*
002100970526     D* SCHIERE
002200970526     D*------------------------------------------------------------------------*
002300970526     D*------------
002400970526     D* DECODIFICHE
002500970526     D*------------
002600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* MESSAGGI
002900970526     D*------------
003000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003100970528     D*------------
003200970528     D* COMANDI
003300970528     D*------------
003400970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* DESCRIZIONI OPZIONI
003700970526     D*------------
003800000707     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* DESCRIZIONI TASTI FUNZIONALI
004100970526     D*------------
004200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004300970526     D*------------
004400970526     D* OPZIONI DA CARICARE A VIDEO
004500970526     D*------------
004600970526     D OPV             S             19    DIM(24)
004700970526     D*------------
004800970526     D* FUNZIONI DA CARICARE A VIDEO
004900970526     D*------------
005000970527     D FUV             S             19    DIM(24)
005100991108     D*------------
005200991108     D* CODICI DI STATO DEI RECORD DI LOG
005300991108     D*------------
005400991108     D FLG             S              1    DIM(10) CTDATA PERRCD(1)
005500991108     D*------------
005600991108     D* DESCRIZIONI STATO DEI RECORD DI LOG
005700991108     D*------------
005800991112     D DES             S              5    DIM(10) CTDATA PERRCD(1)
005900970526     D*------------------------------------------------------------------------*
006000970526     D* INPUT
006100970526     D*------------------------------------------------------------------------*
006200970526     D*------------
006300970526     D* RIDEFINIZIONE OPZIONI VIDEO
006400970526     D*------------
006500970526     D OPDDS           DS
006600970526     D  OPDOPZ                 1      2
006700970526     D  OPDDE1                 3     19
006800970526     D  OPDOP1                21     22
006900970526     D  OPDFI0                23     34
007000970526     D  OPDATT                35     35
007100970526     D  OPDFI1                36     80
007200970526     D  OPDDES                 1     19
007300970526     D  OPDRIG                 1     80
007400970526     D*------------
007500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007600970526     D*------------
007700970526     D FUDDS           DS
007800970526     D  FUDDE1                 1      1
007900970526     D  FUDFUN                 2      3
008000970526     D  FUDDE2                 4     19
008100970526     D  FUDFI0                20     34
008200970526     D  FUDATT                35     35
008300970526     D  FUDFI1                36     80
008400970526     D  FUDDES                 1     19
008500970526     D  FUDRIG                 1     80
008600970526     D*------------
008700970526     D* DS GESTIONE TASTI FUNZIONALI
008800970526     D*------------
008900970526     D DSKEY         E DS
009000970526     D DEVDS1          DS
009100970526     D  KEY                  369    369
009200970526     D*------------
009300970526     D* DS "XMSG" - GESTIONE MESSAGGI
009400970526     D*------------
009500970526     D DSMSG         E DS
009600970526     D*------------
009700020916     D* DS REPERIMENTO DATI UTENTE
009800970526     D*------------
009900020916     D TIBS34DS      E DS                                                       *Profili utente
010000020916     D DDATIUTE      E DS                                                       *Dati utente
010100020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010200020218     D*------------
010300020218     D* DS "ITF" - TABELLA SCAMBIO TIPI FILE
010400020218     D*------------
010500020218     D DITF          E DS
010600970527     D*------------------
010700991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
010800970527     D*------------------
010900970527     D WLBDA8          DS                  INZ
011000970527     D  G08DAT                 1      8  0
011100970527     D  G08INV                 9     16  0
011200970527     D  G08ERR                17     17
011300970527     D  G08TGI                18     22  0
011400970526     D*------------
011500970526     D* ARCHITETTURA
011600970526     D*------------
011700970526     D KPJBA         E DS
011800021119     D*------------
011900021119     D* DS RIDEFINIZIONE CAMPO FLAG OPERATIVI (VLRFL3) DI TIVLR00F
012000021119     D*------------
012100021119     D DSVLR3        E DS
012200991108     D*------------------
012300140701     D* DS - dati per fetch
012400140701     D WFLUEDS       E DS                  extname(WFLUE00F)
012500140701     D DSFetch         DS
012600140701     D  KSC                                like(LUEKsc)
012700140701     D  TIP                                like(LUETip)
012800140701     D  LOG                                like(LUECntLog)
012900140701     D  ERR                                like(LUECntErr)
013000140702     D  PERC                          5s 2
013100140701     D*------------------
013200140701     D* DS - CONTROLLO ED INVERSIONE VELOCE DATA (8)
013300991108     D*------------------
013400991108     D                 DS                  INZ
013500991108     D  GG                     1      2  0
013600991108     D  MM                     3      4  0
013700991108     D  AAAA                   5      8  0
013800991108     D  CHKDATA                1      8  0
013900991108     D*------------------
014000991108     D                 DS                  INZ
014100991108     D  AAINV                  1      4  0
014200991108     D  MMINV                  5      6  0
014300991108     D  GGINV                  7      8  0
014400991108     D  INVDATA                1      8  0
014500991110     D*------------------
014600991110     D* DS - CONVERSIONE DATA IN FORMATO ALFA  (10)
014700991110     D*------------------
014800991110     D                 DS                  INZ
014900991110     D  GGA                    1      2  0
015000991110     D  SEP1                   3      3
015100991110     D  MMA                    4      5  0
015200991110     D  SEP2                   6      6
015300991110     D  AAAAA                  7     10  0
015400991110     D  DATAA                  1     10
015500991110     D*------------------
015600991110     D* DS - CONVERSIONE ORA IN FORMATO ALFA  (8)
015700991110     D*------------------
015800991110     D                 DS                  INZ
015900991110     D  HH                     1      2  0
016000991110     D  PP                     3      4  0
016100991110     D  SS                     5      6  0
016200991110     D  ORAN                   1      6  0
016300991110     D*------------------
016400991110     D                 DS                  INZ
016500991110     D  HHA                    1      2  0
016600991110     D  SEP3                   3      3
016700991110     D  PPA                    4      5  0
016800991110     D  SEP4                   6      6
016900991110     D  SSA                    7      8  0
017000991110     D  ORAA                   1      8
017100991110     D*------------------
017200991110     D* DS - COMPOSIZIONE NOME MEMBRO
017300991110     D*------------------
017400991110     D                 DS                  INZ
017500991110     D  M                      1      1
017600991110     D  HDL                    1     10
017700991110     D*------------------
017800991110     D* DS - SCOMPOSIZIONE MESSAGGIO A VIDEO
017900991110     D*------------------
018000991110     D                 DS                  INZ
018100001215     D  MS1                    1     42
018200001215     D  MS2                   43     80
018300991110     D  MESS                   1     80
018400991108     D*------------------
018500991108     D* DS REPERIMENTO DATI UTENTE
018600991108     D*-------------------
018700991108     DBS69DS         E DS                  extname(TIBS69DS)
018800991108     DACODS          E DS                  extname(CNACO00F)
018900991108     DINDDS          E DS                  extname(CNIND00F)
019000991108     DCLPDS          E DS                  extname(CNCLP00F)
019100991108     DCLSDS          E DS                  extname(FNCLS00F)
019200090519     D*------------
019300090519     D* VARIABILI D WRK
019400090519     D*------------
019500090519     D wrkprg          s              8  0 inz(*zeros)
019600090519     D uplprg          s             10    inz(*all'0')
019700991110     D*------------
019800140702     D* PASSAGGIO PARAMETRI IN INGRESSO
019900140702     D*------------
020000140702     D PARAM           DS
020100140702     D  DEPOP0                        3
020200140702     D  KSC760                        8
020300140702     D  TIP760                        2
020400140702     D*------------
020500140701     D* Stringa SQL da eseguire
020600140701     d wSQL            s           2048    inz  varying
020700140701     D* appoggio ragione sociale cliente
020800140701     d wRag            s             30    inz
020900051013     D*------------
021000051013     D* PASSAGGIO PARAMETRI IN INGRESSO
021100051013     D*------------
021200140701     d DS760R2         ds
021300140701     d pIn_DatDa                      8s 0
021400140701     d pIn_DatA                       8s 0
021500140701     d pIn_Ksc                        8
021600140701     d pIn_Tip                        2
021700140701     d pIn_Perc                       3s 0
021800140702     d pIn_Log                        3s 0
021900140701     d pIn_Modo                       1
022000140701     d pOut_Esito                     2  0
022100080320     D*---
022200080320     D* Costanti
022300080320     D*---
022400080320     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
022500080320     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
022600101005     D*-------------------
022700101005     D* DEFINIZIONI PROCEDURE ESTERNE
022800101005     D*-------------------
022900101005     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
023000101005     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
023100970526     C*------------------------------------------------------------------------*
023200970526     C* MAIN LINES
023300970526     C*------------------------------------------------------------------------*
023400970526     C*
023500970526     C* SE NON CI SONO ERRORI, PROSEGUE
023600970526IF  1C     ERRL00        IFNE      '1'
023700970526     C*
023800970526     C* CARICA IL SFILE
023900970526     C                   EXSR      POSSFL
024000970526     C*
024100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
024200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
024300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
024400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
024500970526     C*
024600970526     C* REIMPOSTA ERRORE DI PROCERURA
024700970526     C                   MOVEL     '0'           ERRL00
024800970526     C*
024900970526     C* EMETTE IL SFILE
025000970526     C                   EXSR      OUTSFL
025100970526     C*
025200970526     C* GESTIONE TASTI FUNZIONALI
025300970526     C                   EXSR      FUNCON
025400970526     C*
025500970526E   2C                   ENDDO
025600970527     C*
025700970526E   1C                   ENDIF
025800991108     C*
025900991108     C                   EXSR      FINSR
026000970526     C* FINE PGM
026100970526     C                   SETON                                        LR
026200970526     C*------------------------------------------------------------------------*
026300970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
026400970526     C*------------------------------------------------------------------------*
026500970526     C     F03GES        BEGSR
026600970526     C*
026700970526     C* IMPOSTA "PREMUTO F03"
026800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
026900970526     C*
027000970526     C                   ENDSR
027100970526     C*------------------------------------------------------------------------*
027200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
027300970526     C*------------------------------------------------------------------------*
027400970526     C     F06GES        BEGSR
027500970526     C*
027600991108     C* NON GESTITO
027700970526     C*
027800970526     C                   ENDSR
027900970526     C*------------------------------------------------------------------------*
028000991108     C* F12GES - GESTIONE F12 -> RITORNO
028100970526     C*------------------------------------------------------------------------*
028200970526     C     F12GES        BEGSR
028300970526     C*
028400970526     C* IMPOSTA "PREMUTO F12"
028500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
028600970526     C*
028700970526     C                   ENDSR
028800970528     C*------------------------------------------------------------------------*
028900970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
029000970528     C*------------------------------------------------------------------------*
029100970528     C     F21GES        BEGSR
029200970612     C*
029300991117     C* NON GESTITA
029400970528     C*
029500970528     C                   ENDSR
029600970526     C*------------------------------------------------------------------------*
029700970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
029800970526     C*------------------------------------------------------------------------*
029900970526     C     F23GES        BEGSR
030000970526     C*
030100970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
030200970526     C*
030300970526     C                   ENDSR
030400970526     C*------------------------------------------------------------------------*
030500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
030600970526     C*------------------------------------------------------------------------*
030700970526     C     F24GES        BEGSR
030800970526     C*
030900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
031000970526     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
031400970526     C*------------------------------------------------------------------------*
031500970526     C     RUPGES        BEGSR
031600970526     C*
031700970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
031800970526     C*
031900970526     C                   ENDSR
032000970526     C*------------------------------------------------------------------------*
032100991108     C* ENTGES - GESTIONE ENTER -> INVIO
032200970526     C*------------------------------------------------------------------------*
032300970526     C     ENTGES        BEGSR
032400991108     C*
032500140702IF  3C     SFLULT        IFNE      0
032600140702     C                   EXSR      ENTATT                                       *GESTIONE ENTER
032700140702     C*
032800140702     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
032900140702IF  4C     SWMODI        IFNE      0
033000140702     C                   EXSR      POSSFL
033100140702E   4C                   ENDIF
033200140702E   3C                   ENDIF
033300991108     C*
033400970526     C                   ENDSR
033500970526     C*------------------------------------------------------------------------*
033600140701     C* O10GES - Gestione Log
033700970526     C*------------------------------------------------------------------------*
033800140701     C     O10GES        BEGSR
033900970526     C*
034000140702     C                   EVAL      DEPOP0 = 'P02'
034100140702     C                   EVAL      KSC760 = D05KSC
034200140702     C                   EVAL      TIP760 = D05TIP
034300140702     C                   EVAL      KPJBU = PARAM
034400140702     C                   CALL      'TIS760R'
034500140702     C                   PARM                    KPJBA
034600140702     C*
034700140702     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
034800140702     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
034900140702     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
035000140702     C                   Z-ADD     0             SWMODI            1 0
035100970526     C*
035200970526     C                   ENDSR
035300140701     C*------------------------------------------------------------------------*
035400140701     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
035500140701     C*------------------------------------------------------------------------*
035600140701     C     NEWPOS        BEGSR
035700140701     C*
035800140701     C*
035900140701     C                   ENDSR
036000970526     C*------------------------------------------------------------------------*
036100970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
036200970526     C*------------------------------------------------------------------------*
036300970528     C     SETFIL        BEGSR
036400970526     C*
036500140702     C* leggo tutti i rcd a seconda dei parametri ricevuti dal lancio
036600140701     C                   EVAL      wSQL = 'select +
036700140701     C                                    lueksc, luetip, +
036800140701     C                                    sum(luecntlog), sum(luecnterr), +
036900140702     C                                    dec( +
037000140707     C                                    dec(sum(luecnterr) , 9 , 2)/ +
037100140707     C                                    dec(sum(luecntlog) , 9 , 2)*100 +
037200140702     C                                    , 3 , 0) +
037300140701     C                                    from WFLUE00F +
037400140701     C                                    group by lueksc, luetip +
037500140701     C                                    having sum(luecnterr) > 0 +
037600140702     C                                    and sum(luecntlog) >=' +
037700140702     C                                    %char(pIn_Log) +
037800140702     C                                    ' and dec( +
037900140707     C                                    dec(sum(luecnterr) , 9 , 2)/ +
038000140707     C                                    dec(sum(luecntlog) , 9 , 2)*100 +
038100140702     C                                    , 3 , 0) >=' +
038200140702     C                                    %char(pIn_Perc) +
038300140702     C                                    ' order by 5 desc , 4 desc , 1 , 2 '
038400140701     C*
038500140701     C/EXEC SQL
038600140701     C+ prepare S1 from :wSQL
038700140701     C/END-EXEC
038800140701     C*
038900140701     C*
039000140701     C/EXEC SQL
039100140701     C+ declare C1 cursor for S1
039200140701     C/END-EXEC
039300140701     C*
039400140701     C*
039500140701     C/EXEC SQL
039600140701     C+ open C1
039700140701     C/END-EXEC
039800140701     C*
039900970526     C                   ENDSR
040000970526     C*------------------------------------------------------------------------*
040100970526     C* LETFIL - LETTURA RECORD
040200970526     C*------------------------------------------------------------------------*
040300970526     C     LETFIL        BEGSR
040400970526     C*
040500970526     C* LEGGE FINO A:
040600970526     C*     - FINE FILE
040700970526     C*     - TROVATO RECORD VALIDO
040800140701     C                   MOVEL     'N'           WRECOK                         *RCD NON VALIDO
040900140701     C                   EVAL      SFLEOF = 0
041000140701
041100140701     C     SFLEOF        DOWEQ     0
041200970526     C     WRECOK        ANDEQ     'N'
041300140701     C*
041400140701     C/EXEC SQL
041500140701     C+ Fetch C1 into :DSFetch
041600140701     C/END-EXEC
041700970528     C*
041800140702     C                   SELECT
041900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
042000140702     C                   WHEN      SQLCod = 100                                 *FINE FILE
042100140702     C                   Z-ADD     1             SFLEOF
042200140702     C* SE Rcd letto, lo controllo
042300140702     C                   WHEN      SQLCod >= 0
042400970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
042500140702     C* SE l'SQL si è rotto, esco avvertendo il chiamante
042600140702     C                   WHEN      SQLCod < 0
042700140702     C                   Z-ADD     1             SFLEOF
042800140702     C                   EVAL      pOut_Esito = 4
042900140702     C                   ENDSL
043000001215     C                   ENDDO
043100970526     C*
043200970526     C                   ENDSR
043300970526     C*------------------------------------------------------------------------*
043400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
043500970526     C*------------------------------------------------------------------------*
043600970526     C     CHKREC        BEGSR
043700970526     C*
043800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
043900001215     C*
044000970526     C                   ENDSR
044100010423     C*------------------------------------------------------------------------*
044200010423     C* dectib - Controlla esistenza codice cliente
044300010423     C*------------------------------------------------------------------------*
044400010423     C     dectib        begsr
044500010913     C*
044600140701     C     ANVCli        chain     tianv01l
044700010913     C                   if        %found(tianv01l)
044800140701     C                   movel(p)  anvdes        wRag
044900010913     C                   else
045000010423     C                   CLEAR                   BS69DS
045100010423     C                   CLEAR                   ACODS
045200010423     C                   MOVEL     KNSIF         I69SIF
045300140702     C                   CLEAR                   wRag
045400140701     C                   MONITOR
045500140701     C                   EVAL      I69KAC = %dec(%subst(ANVCli:2:7):7:0)
045600140701     C                   ON-ERROR  *all
045700140701     C                   MOVEL     *ALL'*'       wRag
045800140701     C                   ENDMON
045900140701     C*
046000140701     C                   IF        wRag <> *all'*'
046100010423     C                   CALL      'TIBS69R'
046200010423     C                   PARM                    BS69DS
046300010423     C                   PARM                    ACODS
046400010423     C                   PARM                    INDDS
046500010423     C                   PARM                    CLPDS
046600010423     C                   PARM                    CLSDS
046700010423     C     O69ERR        IFNE      '1'
046800140701     C                   MOVEL     ACORAG        wRag
046900010423     C                   ELSE
047000140701     C                   MOVEL     *ALL'*'       wRag
047100010913     C                   MOVE      '1'           errl00
047200010913     C                   ENDIF
047300140701     C                   ENDIF
047400140701     C*
047500140701     C                   ENDIF
047600010913     C*
047700010913     C                   endsr
047800970526     C*------------------------------------------------------------------------*
047900970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
048000970526     C*------------------------------------------------------------------------*
048100970526     C     IRESFL        BEGSR
048200010329     C*
048300010612     C                   CLEAR                   S760D05
048400010612     C*
048500970526     C                   MOVEL     *BLANKS       OPZKEY
048600140701     C                   EVAL      D05KSC = KSC
048700140701     C                   EVAL      ANVCli = D05Ksc
048800140701     C                   EXSR      DecTib
048900140701     C                   MOVEL     wRag          D05Rag
049000140701     C*
049100140701     C                   EVAL      D05TIP = TIP
049200140701     C* reperimento descrizione tipi file
049300140701     C                   MOVEL     'ITF'         KBEcod                         *tabella
049400140701     C                   MOVEL     *BLANKS       KBElin                         *tabella
049500140701     C                   MOVEL     *BLANKS       KBEsif                         *tabella
049600140701     C                   MOVEL     D05Tip        KBEke1                         *tabella
049700140701     C                   MOVEL     *BLANKS       KBEke2                         *tabella
049800140701     C     KE2tbe        CHAIN     tntbe02l
049900140701     C                   MOVEL     tbeuni        DITF
050000140701     C                   MOVEL     §ITFDESC      D05TipD                        *tabella
050100140701     C*
050200140701     C                   EVAL      D05Log = Log
050300140701     C                   EVAL      D05Err = Err
050400140701     C                   EVAL      D05Perc = Perc
050500991108     C*
050600970526     C                   ENDSR
050700970526     C*------------------------------------------------------------------------*
050800991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
050900970526     C*------------------------------------------------------------------------*
051000970526     C     CHKPAR        BEGSR
051100970528     C*
051200140701     C* non c'è richiesta ordinamento perché legge un file già ordinato via SQL
051300991012      *
051400970526     C                   ENDSR
051500970526     C*------------------------------------------------------------------------*
051600970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
051700970526     C*------------------------------------------------------------------------*
051800970526     C     CHKFOP        BEGSR
051900991112     C*
052000991112     C* OPZIONI
052100970626     C*
052200140702     C* l'utente può fare tutte le opzioni/funzioni perché entra in un solo modo
052300140702     C                   MOVEL     OPD(10)       OPDRIG                         *Gestione LOG
052400970626     C                   MOVEL     'S'           OPDATT
052500140702     C                   MOVEL     OPDRIG        OPD(10)
052600970626     C*
052700970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
052800020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
052900970626E   1C                   ENDIF
053000970526     C*
053100970526     C* GESTIONE OPZIONI A VIDEO
053200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
053300970526     C                   EXSR      OPZGES
053400970526     C*
053500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
053600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
053700970526     C                   EXSR      FUNGES
053800970526     C*
053900970526     C                   ENDSR
054000030617     C*------------------------------------------------------------------------*
054100120112     C*CONTA0B - CONTA I RECORD DA ELABORARE (STATO = 0 / " ")
054200030617     C*------------------------------------------------------------------------*
054300101018     C     CONTA0B       BEGSR
054400030617     C*
054500030617     C*
054600030617     C                   ENDSR
054700991108     C*------------------------------------------------------------------------*
054800991108     C* FINSR - OPERAZIONI FINALI
054900991108     C*------------------------------------------------------------------------*
055000991108     C     FINSR         BEGSR
055100991108     C*
055200991108     C* CHIUDE I FILE DEI PGM CHIAMATI
055300991108     C                   CLEAR                   BS69DS
055400991108     C                   CLEAR                   ACODS
055500991108     C                   CLEAR                   INDDS
055600991108     C                   CLEAR                   CLPDS
055700991108     C                   CLEAR                   CLSDS
055800991108     C                   MOVEL     'C'           I69TLA
055900991108     C                   MOVEL     KNSIF         I69SIF
056000991108     C                   CALL      'TIBS69R'
056100991108     C                   PARM                    BS69DS
056200991108     C                   PARM                    ACODS
056300991108     C                   PARM                    INDDS
056400991108     C                   PARM                    CLPDS
056500991108     C                   PARM                    CLSDS
056600140701     C*
056700140701     C/EXEC SQL
056800140701     C+ close C1
056900140701     C/END-EXEC
057000991108     C*
057100140702     C* cancello i file di appoggio in QTEMP (dovrebbe sempre funzionare, per cui non testo
057200140702     C* l'SQLCOD
057300140702     C                   EVAL      wSQL = 'drop   table QTEMP/WFLUE00F'
057400140702     C/EXEC SQL
057500140702     C+ execute immediate  :wSQL
057600140702     C/END-EXEC
057700140702     C*
057800991108     C                   ENDSR
057900020916     C*--------------------------------------------------------------------------------------------*
058000020916     C* REPERISCE I DATI UTENTE
058100020916     C*--------------------------------------------------------------------------------------------*
058200020916     C     REPDATIUTE    BEGSR
058300020916     C*
058400020916     C* INIZIALIZZA VARIABILI DI WRK
058500020916     C                   CLEAR                   TIBS34DS
058600020916     C                   CLEAR                   AZUTEDS
058700020916     C                   CLEAR                   DDATIUTE
058800020916     C*
058900020916     C     *DTAARA       DEFINE    §azute        azuteds
059000020916     C     *DTAARA       DEFINE    §datiute      ddatiute
059100020916     C                   IN(E)     *DTAARA
059200020916if  1C                   IF        %Error
059300020916     c                   EVAL      I34Tla = 'L'
059400020916     C                   CALL      'TIBS34R'
059500020916     C                   PARM                    Tibs34Ds
059600020916     C                   IN        *DTAARA
059700020916e   1C                   ENDIF
059800020916     C*
059900020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
060000020916     C                   MOVEL(P)  RSUT          DSFIRS           20
060100020916     C*
060200020916     C                   ENDSR
060300130731     C*----------------------------------------------------*
060400130731     C*  CONTROLLO NUMERICITA' CAMPI
060500130731     C*----------------------------------------------------*
060600130731     C     CHKNUM        BEGSR
060700130731     C*
060800130731     C                   IF        PiDecChr = *blanks
060900130731     C                   EVAL      PiDecChr = '.'
061000130731     C                   ENDIF
061100130731     C*
061200130731     C                   CALL(e)   'ISNUMERIC'
061300130731     C                   PARM                    PiStr            30
061400130731     C                   PARM                    PiDecChr          1
061500130731     C                   PARM      *ZEROS        PiVal            30 9
061600130731     C                   PARM      '0'           PiInt             1
061700130731     C                   PARM      '0'           PiNum             1
061800130731     C                   IF        %error
061900130731     C                   EVAL      PiNum=*off
062000130731     C                   ENDIF
062100130731     C*
062200130731     C                   ENDSR
062300130731     C***
062400970526     C*------------------------------------------------------------------------*
062500970526     C* *INZSR - OPERAZIONI INIZIALI
062600970526     C*------------------------------------------------------------------------*
062700970526     C     *INZSR        BEGSR
062800970526     C*---
062900970526     C* RICEVIMENTO PARAMETRI
063000970526     C*---
063100970526     C     *ENTRY        PLIST
063200970612     C                   PARM                    KPJBA
063300140701     C                   MOVEL     KPJBU         DS760R2
063400080319     C*
063500080319     C* SE UTENTE NO EDP* => ACCENDO *IN55
063600080319     C                   SETOFF                                       55
063700101014     C***                IF        %subst(KNMUS:1:3) <> 'EDP'
063800101014     C***                SETON                                        55
063900101014     C***                ENDIF
064000970526     C*
064100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
064200970526     C     XMSGPA        PLIST
064300970526     C                   PARM                    DSMSMS
064400970526     C                   PARM                    DSMSPG
064500970526     C                   PARM                    DSMSKE
064600970526     C                   PARM                    DSMSER
064700970526     C*
064800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
064900970526     C                   CALL      'XKEY'
065000970526     C                   PARM                    DSKEY
065100970526     C*---
065200970526     C* VARIABILI NON RIFERITE AL DATA BASE
065300970526     C*---
065400970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
065500090921     C***  *LIKE         DEFINE    SFLPAG        WFLPAG
065600970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
065700970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
065800970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
065900970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
066000970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
066100970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
066200140701     C     *LIKE         DEFINE    FUDFUN        FUNKEY
066300140701     C*---
066400140701     C     *LIKE         DEFINE    tbecod        kbecod
066500140701     C     *LIKE         DEFINE    tbelin        kbelin
066600140701     C     *LIKE         DEFINE    tbesif        kbesif
066700140701     C     *LIKE         DEFINE    tbeke1        kbeke1
066800140701     C     *LIKE         DEFINE    tbeke2        kbeke2
066900140701     C*---
067000140701     C* VARIABILI DI CONTROLLO
067100140701     C*---
067200140701     C                   MOVEL     '0'           FINL00            1            *FINE PGM
067300140701     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
067400140701     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
067500140701     C                   MOVEL     '0'           ERRL00            1            *ERRORE
067600140701     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
067700140701     C                   Z-ADD     9999          SFLPAG                         *carico tutto
067800140701     C*---
067900991108     C* LETTURA "NUOVE" TABELLE
068000010524     C     KE2tbe        KLIST                                                  *TNTBE02L
068100991108     C                   KFLD                    KBEcod                          -tabella
068200991108     C                   KFLD                    KBElin                          -lingua
068300991108     C                   KFLD                    KBEsif                          -s.informativo
068400991108     C                   KFLD                    KBEke1                          -lingua
068500991108     C                   KFLD                    KBEke2                          -s.informativo
068600991108     C*
068700970527     C* REPERIMENTO DATI UTENTE
068800020916     C                   EXSR      REPDATIUTE
068900970526     C*
069000970526     C* CALCOLA LA DATA CORRENTE
069100120112     C                   Z-ADD     0             DATCOR            8 0          *DATA CORRENTE AA/M/
069200120112     C                   EVAL      DatCor = %dec(%date : *ISO)
069300010314     C*
069400970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
069500970526     C                   EXSR      CHKFOP
069600051013     C*
069700051013     C* EFFETTUA PRIMA VERIFICA DEI PARAMETRI RICEVUTI
069800051013     C                   EXSR      CHKPAR
069900970526     C*
070000970526     C                   ENDSR
070100970526     C*------------------------------------------------------------------------*
070200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070400970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
070500970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
070600970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
070700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070900970526     C*------------------------------------------------------------------------*
071000970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
071100970526     C*------------------------------------------------------------------------*
071200970526     C     POSSFL        BEGSR
071300970526     C*
071400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
071500970526     C                   EXSR      CLRSFL
071600970526     C*
071700970526     C                   EXSR      CARSFL
071800970526     C*
071900970526     C                   ENDSR
072000970526     C*------------------------------------------------------------------------*
072100970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
072200970526     C*------------------------------------------------------------------------*
072300970526     C     ENTATT        BEGSR
072400970526     C*
072500970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
072600970526     C                   Z-ADD     0             OPZOK             1 0
072700970526     C*
072800970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
072900970526     C                   Z-ADD     0             SFLVAR            5 0
073000970526     C*
073100970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
073200970526     C                   Z-ADD     0             SFLERR
073300970526     C*
073400970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
073500970526     C                   Z-ADD     SFLULT        WFLULT
073600970526     C*
073700970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
073800991105     C                   READC     s760D05                                98
073900970526     C     *IN98         DOWEQ     '0'
074000970526     C*
074100970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
074200970526     C                   ADD       1             SFLVAR
074300970526     C*
074400970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
074500970526     C     OPZOK         IFEQ      0
074600970526     C*
074700970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
074800970526     C     OPZKEY        IFNE      *BLANKS
074900970526     C                   EXSR      OPZCON
075000970526     C                   END
075100970526     C*
075200970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
075300970526     C*            2. PULIZIA SCELTA IN SFL
075400970526     C     OPZOK         IFEQ      0
075500970526     C                   MOVEL     '0'           *IN41
075600970526     C                   MOVEL     *BLANKS       OPZKEY
075700970526     C                   ELSE
075800970526     C*
075900970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
076000970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
076100970526     C                   MOVEL     '1'           *IN41
076200970526     C                   Z-ADD     SFLULT        SFLERR
076300970526     C                   END
076400970526     C                   ELSE
076500970526     C*
076600970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
076700970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
076800970526     C                   MOVEL     '1'           *IN41
076900970526     C                   END                                                    OPZOK=0
077000970526     C*
077100970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
077200970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
077300970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
077400970526     C     OPZOK         IFNE      0
077500970526     C                   Z-ADD     SFLERR        SFLNBR
077600970526     C                   ELSE
077700970526     C                   Z-ADD     SFLULT        SFLNBR
077800970526     C                   END
077900970526     C*
078000970526     C* AGGIORNO RECORD SFL
078100020212     C                   EVAL      *IN50 = HIN50
078200020212     C                   EVAL      *IN51 = HIN51
078300991105     C                   UPDATE    s760D05
078400970526     C*
078500991105     C                   READC     s760D05                                98
078600970526     C                   END                                                    DO WHILE
078700970526     C*
078800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
078900970526     C                   Z-ADD     WFLULT        SFLULT
079000970526     C*
079100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
079200970526     C     SFLVAR        IFEQ      0
079300970526     C*---                 MOVEL'1'       FINL00
079400970526     C                   END
079500991112     C*
079600970526     C                   ENDSR
079700970526     C*------------------------------------------------------------------------*
079800970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
079900970526     C*------------------------------------------------------------------------*
080000970526     C     CLRSFL        BEGSR
080100970526     C*
080200970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
080300970526     C* (0=NON FINE FILE  1=FINE FILE)
080400970526     C                   Z-ADD     0             SFLEOF
080500970526     C*
080600970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
080700970526     C                   Z-ADD     0             SFLULT
080800970526     C*
080900970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
081000970526     C                   MOVEL     '1'           *IN40
081100991105     C                   WRITE     s760D06
081200970526     C                   MOVEL     '0'           *IN40
081300970526     C*
081400970526     C                   ENDSR
081500970526     C*------------------------------------------------------------------------*
081600970526     C* CARSFL - CARICAMENTO SFL
081700970526     C*------------------------------------------------------------------------*
081800970526     C     CARSFL        BEGSR
081900970526     C*
082000140701     C* valorizzo i campi di testata con i parametri
082100140701     C                   EVAL      VIDKsc = pIn_Ksc
082200140701     C                   IF        VIDKsc <> *blank
082300140701     C                   EVAL      ANVCli = VIDKsc
082400140701     C                   EXSR      DecTib
082500140701     C                   MOVEL     wRag          VIDRag
082600140701     C                   ENDIF
082700140701     C                   EVAL      VIDTip = pIn_Tip
082800140702     C                   EVAL      VIDPerc= pIn_Perc
082900140702     C                   EVAL      VIDLog = pIn_Log
083000140701     C*
083100970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
083200970526     C     SFLULT        IFEQ      0
083300970526     C                   EXSR      SETFIL
083400970526     C                   END
083500970526     C*
083600970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
083700970526     C                   Z-ADD     SFLULT        WFLULT
083800140701     C*
083900140701     C* LETTURA FILE DA CARICARE
084000140701     C                   EXSR      LETFIL
084100970526     C*
084200970526     C* CICLO:
084300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
084400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
084500970526     C                   Z-ADD     1             SFLRIG
084600970526     C     SFLRIG        DOWLE     SFLPAG
084700970526     C     SFLEOF        ANDEQ     0
084800970526     C*
084900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
085000970526     C                   MOVEL     *BLANKS       OPZKEY
085100970526     C*
085200970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
085300970526     C                   EXSR      IRESFL
085400970526     C*
085500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
085600970526     C                   ADD       1             SFLULT
085700970526     C*
085800970526     C* SCRITTURA RECORD SFL
085900020212     C                   EVAL      HIN50 = *IN50
086000020212     C                   EVAL      HIN51 = *IN51
086100991105     C                   WRITE     s760D05
086200970526     C                   ADD       1             SFLRIG
086300970526     C*
086400970526     C* LETTURA FILE DA CARICARE
086500970526     C                   EXSR      LETFIL
086600970526     C                   END                                                    DO WHILE
086700970526     C*
086800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
086900970526     C     WFLULT        IFNE      SFLULT
087000970526     C                   Z-ADD     WFLULT        SFLNBR
087100970526     C                   ADD       1             SFLNBR
087200140701     C* mi salvo il posizionamento effettuato
087300140701     C* --- nessun posizionamento ---
087400970526     C                   END
087500970526     C*
087600970526     C                   ENDSR
087700970526     C*------------------------------------------------------------------------*
087800970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
087900970526     C*------------------------------------------------------------------------*
088000970526     C     OUTSFL        BEGSR
088100970526     C*
088200970526     C* WRITE RECORD TESTATA VIDEO
088300991105     C                   WRITE     s760D01
088400970526     C*
088500970526     C* VISUALIZZAZIONE MESSAGGIO
088600970526     C                   EXSR      SNDMSG
088700970526     C*
088800970526     C* WRITE RECORD OPZIONI VIDEO
088900991105     C                   WRITE     s760D02
089000030617     C*
089100030617     C* CONTO I RECORD DA ELABORARE
089200120112     C                   EXSR      CONTA0B
089300970526     C*
089400970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
089500991105     C                   WRITE     s760D03
089600970526     C*
089700970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
089800991105     C                   WRITE     s760D04
089900970526     C*
090000970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
090100970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
090200970526     C     SFLEOF        IFEQ      0
090300970526     C                   MOVEL     '0'           *IN42
090400970526     C                   ELSE
090500970526     C*
090600970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
090700970526     C                   MOVEL     '1'           *IN42
090800970526     C                   END
090900970526     C*
091000970526     C* WRITE RECORD TASTI FUNZIONALI
091100991105     C                   WRITE     s760D09
091200970526     C*
091300970526     C* CASO DI SFL VUOTO
091400970526     C     SFLULT        IFLE      0
091500970526     C*
091600970526     C*    - WRITE RECORD VUOTO
091700991105     C                   WRITE     s760D0A
091800991105     C                   READ      s760D0A                                99
091900970526     C*
092000970526     C                   ELSE
092100970526     C*
092200970526     C* CASO DI SFL CON RECORDS
092300970526     C*
092400970526     C*    - WRITE RECORD CONTROLLO SFL
092500991105     C                   WRITE     s760D06
092600991105     C                   READ      s760D06                                99
092700970526     C                   END
092800970526     C*
092900970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
093000991105     C                   READ      s760D03                                99
093100970526     C*
093200970526     C                   ENDSR
093300970526     C*------------------------------------------------------------------------*
093400970526     C* SNDMSG - MANDA MESSAGGIO
093500970526     C*------------------------------------------------------------------------*
093600970526     C     SNDMSG        BEGSR
093700970526     C*
093800970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
093900970526     C                   MOVEL     '1'           *IN40
094000991105     C                   WRITE     s760DMC
094100970526     C                   MOVEL     '0'           *IN40
094200970526     C*
094300970526     C* VISUALIZZAZIONE MESSAGGIO
094400970526     C                   Z-ADD     0             MSGULT
094500970526     C                   CALL      'XMSG'        XMSGPA
094600970526     C     DSMSER        IFEQ      0
094700970526     C                   MOVEL     DSMSKE        MSGKEY
094800970526     C                   END
094900970526     C                   Z-ADD     1             MSGULT
095000991105     C                   WRITE     s760DMS                                      SFL MESS
095100991105     C                   WRITE     s760DMC                                      CONTR MESS
095200970526     C*
095300970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
095400970526     C                   MOVEL     *BLANKS       DSMSMS
095500970526     C*
095600970526     C                   ENDSR
095700970526     C*------------------------------------------------------------------------*
095800970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
095900970526     C*------------------------------------------------------------------------*
096000970526     C     FUNCON        BEGSR
096100970526     C*
096200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
096300970526     C                   EXSR      FUNVAR
096400970526     C*
096500970526     C* PULISCO CAMPI DI LAVORO
096600970526     C                   Z-ADD     0             I                 5 0
096700970526     C                   Z-ADD     0             W                 5 0
096800970526     C*
096900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
097000970526     C     FUNKEY        IFNE      *BLANKS
097100970526     C     1             DO        24            I
097200970526     C                   MOVEL     FUD(I)        FUDRIG
097300970526     C*
097400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
097500970526     C     FUNKEY        IFEQ      FUDFUN
097600970526     C     FUDATT        ANDEQ     'S'
097700970526     C                   Z-ADD     I             W
097800970526     C                   END
097900970526     C                   END                                                    DO WHILE
098000970526     C*
098100970526     C* FUNZIONE NON ATTIVA (ERRORE)
098200970526     C     W             IFEQ      0
098300970526     C*
098400970526     C* EMISSIONE MESSAGGIO DI ERRORE
098500970526     C                   MOVEL     MSG(2)        DSMSMS
098600970526     C                   END                                                    W=0
098700970526     C                   END                                                    FUNKEY NE *BLANKS
098800970526     C*
098900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
099000970526     C     W             IFNE      0
099100970526     C     FUNKEY        OREQ      *BLANKS
099200970526     C*
099300970526     C     FUNKEY        IFEQ      *BLANKS
099400970526     C                   EXSR      ENTGES
099500970526     C                   END
099600970526     C     FUNKEY        IFEQ      '03'
099700970526     C                   EXSR      F03GES
099800970526     C                   END
099900970526     C     FUNKEY        IFEQ      '06'
100000970526     C                   EXSR      F06GES
100100970526     C                   END
100200970526     C     FUNKEY        IFEQ      '12'
100300970526     C                   EXSR      F12GES
100400970526     C                   END
100500970526     C     FUNKEY        IFEQ      '23'
100600970526     C                   EXSR      F23GES
100700970526     C                   END
100800970526     C     FUNKEY        IFEQ      '24'
100900970526     C                   EXSR      F24GES
101000970526     C                   END
101100970526     C     FUNKEY        IFEQ      'RU'
101200970526     C                   EXSR      RUPGES
101300970526     C                   END
101400970526     C*
101500970526     C                   END
101600970526     C*
101700970526     C                   ENDSR
101800970526     C*------------------------------------------------------------------------*
101900970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
102000970526     C*------------------------------------------------------------------------*
102100970526     C     FUNVAR        BEGSR
102200970526     C*
102300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
102400970526     C     KEY           IFEQ      ENTER
102500970526     C                   MOVEL     *BLANKS       FUNKEY
102600970526     C                   END
102700970526     C     KEY           IFEQ      F1
102800970526     C                   MOVEL     '01'          FUNKEY
102900970526     C                   END
103000970526     C     KEY           IFEQ      F2
103100970526     C                   MOVEL     '02'          FUNKEY
103200970526     C                   END
103300970526     C     KEY           IFEQ      F3
103400970526     C                   MOVEL     '03'          FUNKEY
103500970526     C                   END
103600970526     C     KEY           IFEQ      F4
103700970526     C                   MOVEL     '04'          FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F5
104000970526     C                   MOVEL     '05'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F6
104300970526     C                   MOVEL     '06'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F7
104600970526     C                   MOVEL     '07'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F8
104900970526     C                   MOVEL     '08'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      F9
105200970526     C                   MOVEL     '09'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      F10
105500970526     C                   MOVEL     '10'          FUNKEY
105600970526     C                   END
105700970526     C     KEY           IFEQ      F11
105800970526     C                   MOVEL     '11'          FUNKEY
105900970526     C                   END
106000970526     C     KEY           IFEQ      F12
106100970526     C                   MOVEL     '12'          FUNKEY
106200970526     C                   END
106300970526     C     KEY           IFEQ      F13
106400970526     C                   MOVEL     '13'          FUNKEY
106500970526     C                   END
106600970526     C     KEY           IFEQ      F14
106700970526     C                   MOVEL     '14'          FUNKEY
106800970526     C                   END
106900970526     C     KEY           IFEQ      F15
107000970526     C                   MOVEL     '15'          FUNKEY
107100970526     C                   END
107200970526     C     KEY           IFEQ      F16
107300970526     C                   MOVEL     '16'          FUNKEY
107400970526     C                   END
107500970526     C     KEY           IFEQ      F17
107600970526     C                   MOVEL     '17'          FUNKEY
107700970526     C                   END
107800970526     C     KEY           IFEQ      F18
107900970526     C                   MOVEL     '18'          FUNKEY
108000970526     C                   END
108100970526     C     KEY           IFEQ      F19
108200970526     C                   MOVEL     '19'          FUNKEY
108300970526     C                   END
108400970526     C     KEY           IFEQ      F20
108500970526     C                   MOVEL     '20'          FUNKEY
108600970526     C                   END
108700970526     C     KEY           IFEQ      F21
108800970526     C                   MOVEL     '21'          FUNKEY
108900970526     C                   END
109000970526     C     KEY           IFEQ      F22
109100970526     C                   MOVEL     '22'          FUNKEY
109200970526     C                   END
109300970526     C     KEY           IFEQ      F23
109400970526     C                   MOVEL     '23'          FUNKEY
109500970526     C                   END
109600970526     C     KEY           IFEQ      F24
109700970526     C                   MOVEL     '24'          FUNKEY
109800970526     C                   END
109900970526     C     KEY           IFEQ      ROLLUP
110000970526     C                   MOVEL     'RU'          FUNKEY
110100970526     C                   END
110200970526     C     KEY           IFEQ      HELP
110300970526     C                   MOVEL     'HP'          FUNKEY
110400970526     C                   END
110500970526     C*
110600970526     C                   ENDSR
110700970526     C*------------------------------------------------------------------------*
110800970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
110900970526     C*------------------------------------------------------------------------*
111000970526     C     FUNGES        BEGSR
111100970526     C*
111200970526     C* PULISCO CAMPI DI LAVORO
111300970526     C                   Z-ADD     0             W                 5 0
111400970526     C*
111500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
111600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
111700970526     C     1             DO        24            I
111800970526     C                   MOVEL     FUD(I)        FUDRIG
111900970526     C*
112000970526     C* PULIZIA FUNZIONI PRECEDENTI
112100970526     C                   MOVEL     *BLANKS       FUV(I)
112200970526     C*
112300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
112400970526     C* (SALTA F23 E F24)
112500970526     C     FUDATT        IFEQ      'S'
112600970526     C     FUDFUN        ANDNE     '23'
112700970526     C     FUDFUN        ANDNE     '24'
112800970526     C                   ADD       1             W
112900970526     C                   MOVEL     FUDDES        FUV(W)
113000970526     C                   END
113100970526     C                   END                                                    DO WHILE
113200970526     C*
113300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
113400970526     C* ALTRIMENTI DISATTIVO F24
113500970526     C     1             DO        24            I
113600970526     C                   MOVEL     FUD(I)        FUDRIG
113700970526     C*
113800970526     C     FUDFUN        IFEQ      '24'
113900970526     C     W             IFGT      6
114000970526     C                   MOVEL     'S'           FUDATT
114100970526     C                   ELSE
114200970526     C                   MOVEL     *BLANKS       FUDATT
114300970526     C                   END                                                    W GT 8
114400970526     C                   MOVEL     FUDRIG        FUD(I)
114500970526     C                   END
114600970526     C                   END                                                    DO WHILE
114700970526     C*
114800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
114900970526     C     W             DIV       6             I
115000970526     C                   MVR                     Z
115100970526     C*
115200970526     C     Z             IFNE      0
115300970526     C     I             ADD       1             Z
115400970526     C                   ELSE
115500970526     C                   Z-ADD     I             Z
115600970526     C                   END
115700970526     C*
115800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
115900970526     C                   ADD       1             FUNGRU
116000970526     C     FUNGRU        IFGT      Z
116100970526     C     FUNGRU        ORLT      1
116200970526     C                   Z-ADD     1             FUNGRU
116300970526     C                   END
116400970526     C*
116500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
116600970526     C     FUNGRU        IFEQ      1
116700970526     C                   MOVEL     FUV(1)        FUN001
116800970526     C                   MOVEL     FUV(2)        FUN002
116900970526     C                   MOVEL     FUV(3)        FUN003
117000970526     C                   MOVEL     FUV(4)        FUN004
117100970526     C                   MOVEL     FUV(5)        FUN005
117200970526     C                   MOVEL     FUV(6)        FUN006
117300970526     C                   END
117400970526     C*
117500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
117600970526     C     FUNGRU        IFEQ      2
117700970526     C                   MOVEL     FUV(7)        FUN001
117800970526     C                   MOVEL     FUV(8)        FUN002
117900970526     C                   MOVEL     FUV(9)        FUN003
118000970526     C                   MOVEL     FUV(10)       FUN004
118100970526     C                   MOVEL     FUV(11)       FUN005
118200970526     C                   MOVEL     FUV(12)       FUN006
118300970526     C                   END
118400970526     C*
118500970526     C* TERZO GRUPPO DI 6 FUNZIONI
118600970526     C     FUNGRU        IFEQ      3
118700970526     C                   MOVEL     FUV(13)       FUN001
118800970526     C                   MOVEL     FUV(14)       FUN002
118900970526     C                   MOVEL     FUV(15)       FUN003
119000970526     C                   MOVEL     FUV(16)       FUN004
119100970526     C                   MOVEL     FUV(17)       FUN005
119200970526     C                   MOVEL     FUV(18)       FUN006
119300970526     C                   END
119400970526     C*
119500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
119600970526     C     FUNGRU        IFEQ      4
119700970526     C                   MOVEL     FUV(19)       FUN001
119800970526     C                   MOVEL     FUV(20)       FUN002
119900970526     C                   MOVEL     FUV(21)       FUN003
120000970526     C                   MOVEL     FUV(22)       FUN004
120100970526     C                   MOVEL     *BLANKS       FUN005
120200970526     C                   MOVEL     *BLANKS       FUN006
120300970526     C                   END
120400970526     C*
120500970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
120600970526     C                   MOVEL     *BLANKS       FUN007
120700970526     C                   MOVEL     *BLANKS       FUN008
120800970526     C*
120900970526     C     1             DO        24            I
121000970526     C                   MOVEL     FUD(I)        FUDRIG
121100970526     C     FUDFUN        IFEQ      '23'
121200970526     C     FUDATT        ANDEQ     'S'
121300970526     C                   MOVEL     FUDDES        FUN007
121400970526     C                   END
121500970526     C     FUDFUN        IFEQ      '24'
121600970526     C     FUDATT        ANDEQ     'S'
121700970526     C                   MOVEL     FUDDES        FUN008
121800970526     C                   END
121900970526     C                   END                                                    DO
122000970526     C*
122100970526     C                   ENDSR
122200970526     C*------------------------------------------------------------------------*
122300970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
122400970526     C*------------------------------------------------------------------------*
122500970526     C     OPZGES        BEGSR
122600970526     C*
122700970526     C* PULISCO CAMPI DI LAVORO
122800970526     C                   Z-ADD     0             W                 5 0
122900970526     C*
123000970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
123100970526     C* E CARICO TABELLA OPZIONI ATTIVE
123200970526     C     1             DO        24            I
123300970526     C                   MOVEL     OPD(I)        OPDRIG
123400970526     C*
123500970526     C* PULIZIA OPZIONI PRECEDENTI
123600970526     C                   MOVEL     *BLANKS       OPV(I)
123700970526     C*
123800970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
123900970526     C     OPDATT        IFEQ      'S'
124000970526     C                   ADD       1             W
124100970526     C                   MOVEL     OPDDES        OPV(W)
124200970526     C                   END
124300970526     C                   END                                                    DO WHILE
124400970526     C*
124500970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
124600970526     C* ALTRIMENTI DISATTIVO F23
124700970526     C     1             DO        24            I
124800970526     C                   MOVEL     FUD(I)        FUDRIG
124900970526     C*
125000970526     C     FUDFUN        IFEQ      '23'
125100970526     C     W             IFGT      8
125200970526     C                   MOVEL     'S'           FUDATT
125300970526     C                   ELSE
125400970526     C                   MOVEL     *BLANKS       FUDATT
125500970526     C                   END                                                    W GT 8
125600970526     C                   MOVEL     FUDRIG        FUD(I)
125700970526     C                   END
125800970526     C                   END                                                    DO WHILE
125900970526     C*
126000970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
126100970526     C     W             DIV       8             I
126200970526     C                   MVR                     Z                 5 0
126300970526     C*
126400970526     C     Z             IFNE      0
126500970526     C     I             ADD       1             Z
126600970526     C                   ELSE
126700970526     C                   Z-ADD     I             Z
126800970526     C                   END
126900970526     C*
127000970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
127100970526     C                   ADD       1             OPZGRU
127200970526     C     OPZGRU        IFGT      Z
127300970526     C     OPZGRU        ORLT      1
127400970526     C                   Z-ADD     1             OPZGRU
127500970526     C                   END
127600970526     C*
127700970526     C* PRIMO GRUPPO DI 8 OPZIONI
127800970526     C     OPZGRU        IFEQ      1
127900970526     C                   MOVEL     OPV(1)        OPZ001
128000970526     C                   MOVEL     OPV(2)        OPZ002
128100970526     C                   MOVEL     OPV(3)        OPZ003
128200970526     C                   MOVEL     OPV(4)        OPZ004
128300970526     C                   MOVEL     OPV(5)        OPZ005
128400970526     C                   MOVEL     OPV(6)        OPZ006
128500970526     C                   MOVEL     OPV(7)        OPZ007
128600970526     C                   MOVEL     OPV(8)        OPZ008
128700970526     C                   END
128800970526     C*
128900970526     C* SECONDO GRUPPO DI 8 OPZIONI
129000970526     C     OPZGRU        IFEQ      2
129100970526     C                   MOVEL     OPV(9)        OPZ001
129200970526     C                   MOVEL     OPV(10)       OPZ002
129300970526     C                   MOVEL     OPV(11)       OPZ003
129400970526     C                   MOVEL     OPV(12)       OPZ004
129500970526     C                   MOVEL     OPV(13)       OPZ005
129600970526     C                   MOVEL     OPV(14)       OPZ006
129700970526     C                   MOVEL     OPV(15)       OPZ007
129800970526     C                   MOVEL     OPV(16)       OPZ008
129900970526     C                   END
130000970526     C*
130100970526     C* TERZO GRUPPO DI 8 OPZIONI
130200970526     C     OPZGRU        IFEQ      3
130300970526     C                   MOVEL     OPV(17)       OPZ001
130400970526     C                   MOVEL     OPV(18)       OPZ002
130500970526     C                   MOVEL     OPV(19)       OPZ003
130600970526     C                   MOVEL     OPV(20)       OPZ004
130700970526     C                   MOVEL     OPV(21)       OPZ005
130800970526     C                   MOVEL     OPV(22)       OPZ006
130900970526     C                   MOVEL     OPV(23)       OPZ007
131000970526     C                   MOVEL     OPV(24)       OPZ008
131100970526     C                   END
131200970526     C*
131300970526     C                   ENDSR
131400970526     C*------------------------------------------------------------------------*
131500970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
131600970526     C*------------------------------------------------------------------------*
131700970526     C     OPZCON        BEGSR
131800970526     C*
131900970526     C* PULISCO CAMPI DI LAVORO
132000970526     C                   Z-ADD     0             I                 5 0
132100970526     C                   Z-ADD     0             W                 5 0
132200970526     C*
132300970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
132400970526     C     1             DO        24            I
132500970526     C                   MOVEL     OPD(I)        OPDRIG
132600970526     C*
132700970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
132800970526     C     OPZKEY        IFEQ      OPDOPZ
132900970526     C     OPZKEY        OREQ      OPDOP1
133000970526     C*
133100970526     C     OPDATT        IFEQ      'S'
133200970526     C                   MOVEL     OPDOPZ        OPZKEY
133300970526     C                   Z-ADD     I             W
133400970526     C                   END
133500970526     C                   END
133600970526     C*
133700970526     C                   END                                                    DO WHILE
133800970526     C*
133900970526     C* OPZIONE NON ATTIVA (ERRORE)
134000970526     C     W             IFEQ      0
134100970526     C*
134200970526     C* OPZIONE NON OK
134300970526     C                   Z-ADD     1             OPZOK
134400970526     C*
134500970526     C* EMISSIONE MESSAGGIO DI ERRORE
134600970526     C                   MOVEL     MSG(1)        DSMSMS
134700970526     C                   ELSE
134800970526     C*
134900970526     C* OPZIONE ATTIVA (PROCEDI ...)
135000010314     C*
135100140701     C     OPZKEY        IFEQ      '10'
135200140701     C                   EXSR      O10GES
135300140701     C                   ENDIF
135400140701     C                   ENDIF                                                  OPZ.NON ATTIVA
135500970526     C*
135600970526     C                   ENDSR
135700970526     C*------------------------------------------------------------------------*
135800970526     C* NOTE COSTRUZIONE SCHIERE
135900970526     C*------------------------------------------------------------------------*
136000970526     C*
136100970526     C* -MSG-MESSAGGI
136200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
136300970526     C* (NON TOCCARE MSG,1 E MSG,2)
136400970526     C*
136500970526     C* -CMD-COMANDI
136600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
136700970526     C*
136800970526     C* -OPD-OPZIONI
136900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
137000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
137100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
137200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
137300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
137400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
137500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
137600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
137700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
137800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
137900970526     C*
138000970526     C* -FUD-FUNZIONI
138100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
138200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
138300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
138400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
138500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
138600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
138700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
138800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
138900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
139000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
139100991108     C*
139200991110     C* -FLG-CODICI STATI DEI RECORD DI LOG
139300991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
139400991110     C* ================> IMPORTANTE <=================
139500991110     C* per codici di errore in -FLG- indicare in -DES- sempre
139600991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
139700991110     C* ERR
139800991108     C*
139900970526     O*------------------------------------------------------------------------*
140000970526** DEC - DECODIFICHE
140100970526    GESTIONE                                                     1
140200970526INTERROGAZIONE                                                   2
140300970530     SCELTA                                                      3
1404009705264
1405009705265
140600970526** MSG - MESSAGGI
140700991105TIS760R- Opzione non gestita                                                   1
140800991105TIS760R- Tasto funzionale non gestito                                          2
140900021119TIS760R- Storicizzazione già effettuata impossibile eseguire opzione richiesta.3
141000080118TIS760R- Elaborazione non effettuata, impossibile visualizzare storico.        4
141100010314TIS760R-                                                                       5
141200991105TIS760R- Accettato solo un parametro di inizio lista                           6
141300991105TIS760R- 7                                                                     7
141400991105TIS760R- 8                                                                     8
141500991105TIS760R- 9                                                                     9
141600991105TIS760R- 10                                                                   10
141700991105TIS760R- 11
141800991105TIS760R- Data errata                                                          12
141900000707TIS760R- Opzione valida per record di log con stato diverso da '0'            13
142000010315TIS760R- Per il cliente scelto non è consentita l'operazione richiesta.       14
142100060104TIS760R- L'opzione richiesta non è consentita per log a stato 'M'.            15
142200101005TIS760R- Effettuare selezione                                                 16
142300991105TIS760R- 17                                                                   17
142400991105TIS760R- 18                                                                   18
142500991105TIS760R- 19                                                                   19
142600991105TIS760R- 20                                                                   20
142700970528** CMD - COMANDI CL
142800991110DSPPFM FILE(
142900991110) MBR(
143000991110
143100970528
143200970528
143300970526** OPD - OPZIONI
143400140701                   -  -     (S/N)=N                        opz. 01¬
143500991110                   -  -     (S/N)=N                        opz. 02¬
143600991110                   -  -     (S/N)=N                        opz. 03¬
143700140701                   -  -     (S/N)=N                        opz. 04¬
143800140701                   -  -     (S/N)=N                        opz. 05¬
143900991110                   -  -     (S/N)=N                        opz. 06¬
144000991110                   -  -     (S/N)=N                        opz. 07¬
144100991110                   -  -     (S/N)=N                        opz. 08¬
144200991110                   -  -     (S/N)=N                        opz. 09¬
14430014070210=Gestione Log    -10-     (S/N)=S                        opz. 10¬
144400140701                   -  -     (S/N)=N                        opz. 11¬
144500970526                   -  -     (S/N)=N                        opz. 12¬
144600970526                   -  -     (S/N)=N                        opz. 13¬
144700140701                   -  -     (S/N)=N                        opz. 14¬
144800140701                   -  -     (S/N)=N                        opz. 15¬
144900970526                   -  -     (S/N)=N                        opz. 16¬
145000970526                   -  -     (S/N)=N                        opz. 17¬
145100101005                   -  -     (S/N)=N                        opz. 18¬
145200970526                   -  -     (S/N)=N                        opz. 19¬
145300140701                   -  -     (S/N)=N                        opz. 20¬
145400970526                   -  -     (S/N)=N                        opz. 21¬
145500970526                   -  -     (S/N)=N                        opz. 22¬
145600970526                   -  -     (S/N)=N                        opz. 23¬
145700140701                   -  -     (S/N)=N                        opz. 25¬
145800970526** FUD - FUNZIONI
145900970526                   -        (S/N)=N                        funz.01¬
146000970526                   -        (S/N)=N                        funz.02¬
146100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
146200970526                   -        (S/N)=N                        funz.04¬
146300140701                   -        (S/N)=N                        funz.05¬
146400991108                   -        (S/N)=N                        funz.06¬
146500970526                   -        (S/N)=N                        funz.07¬
146600970527                   -        (S/N)=N                        funz.08¬
146700970527                   -        (S/N)=N                        funz.09¬
146800970527                   -        (S/N)=N                        funz.10¬
146900970526                   -        (S/N)=N                        funz.11¬
147000970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
147100970526                   -        (S/N)=N                        funz.13¬
147200970526                   -        (S/N)=N                        funz.14¬
147300970526                   -        (S/N)=N                        funz.15¬
147400970526                   -        (S/N)=N                        funz.16¬
147500970526                   -        (S/N)=N                        funz.17¬
147600140701                   -        (S/N)=N                        funz.18¬
147700970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
147800970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
147900140701                   -        (S/N)=N                        funz.21¬
148000140701                   -        (S/N)=N                        funz.22¬
148100991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
148200991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
148300991108** FLG - STATI DEI RECORD DI LOG
1484009911080
1485009911081
1486009911082
1487009911083
1488009911084
148900060104M
149000991108
149100991108
149200991108
149300991108
149400991108** DES - STATI DEI RECORD DI LOG
149500991112Da El
149600991108OK
149700991108Err
149800991112R-OK
149900991112R-Err
150000060104Temp
150100101018Init
150200101018Init
150300101018Init
150400101018Init
