000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500100709      *  Da UPLOAD    DEVE CHIAMARE il TRADUTTORE per imPORTARE gli IFCSUM      *
000600100709????  *  oppure GLI IFTSTA                                                      *
000700100709????  *      al momento gli IFCSUM vers S93A   oppure IFTSTA D94A               *
000800060609      **?************************************************************************
000900100709     Ftivin00r  iF   E             DISK    usropn  INFDS(VINRECDS)
001000101019     Fedtab01l  iF   E           k DISK
001100100709      *----------------------------------------------------*
001200100709      * Stringhe per conversione alfanumerico/numerico N.Documento
001300100709     D CA              S             78    DIM(1) CTDATA PERRCD(1)
001400100709     D CN              S             78    DIM(1) CTDATA PERRCD(1)
001500100709      **?************************************************************************
001600090213      * numero relativo di record
001700090213     D VINRECDS        DS
001800090213     D  VNREC                397    400B 0
001900100701      *----------------------------------------------------*
002000090205      **  Elenco DS di tutti i Segmenti da tradurre
002100100701      *----------------------------------------------------*
002200101005???? D edS00UNB      E DS
002300101005???? D edS00UNH      E DS
002400101019???? D edidspr       E DS
002500101019???? D edidspz       E DS
002600100701      *----------------------------------------------------*
002700090209     D Valori_inErr    ds
002800090209     D  SKout_Errori                  1    DIM(50)
002900100630      **
003000090209     D Descr_Errore    ds
003100090209     D  SKout_DesErr                 50    DIM(50)
003200090205      *----------------------------------------------------*
003300091016     D EoFTivin        s              1
003400091016      *----------------------------------------------------*
003500101005     D Quale_File      S              6
003600100709     D x_quale_versione...
003700100709     D                 s              3
003800100709     D x_quale_release...
003900100709     D                 s              3
004000100712     D Agency          s              3
004100100712     D Association     s              6
004200100709      *----------------------------------------------------*
004300090205     D Tipo_seg        S              3
004400100716      *
004500100716     d keyUNBCLI       s             35
004600100716     d keyTIPOMSG      s              6
004700100716     d keyVERSION      s              3
004800100716     d keyRELEASE      s              3
004900100716     d keyAGENCY       s              3
005000100716     d keyASSOCIA      s              6
005100100716      *
005200090209     D segmento        s           2048
005300090209     D Errore          s              1
005400101015      *
005500101015     D W0140           S             14  0
005600101015     D DataGGMMAAA     S              8  0
005700101015     D DATEU_AMG       S              8  0
005800101015     D DATA_eur        S               D   DATFMT(*eur)
005900101015      *
006000100318      *------------------------------------------
006100100701???? D Segm_Testata_Dettagli...
006200100701???? D                 s              3    INZ('UNH')
006300100709      *------------------------------------------
006400090209      *  Invio E-mail
006500090209     D Indirizzi       s            253
006600090209     D Oggetto         s             44
006700090209     D Messaggio       s           5000
006800090209      *
006900090209     d   DSmail        ds
007000090209     D Mail_Ind                      44
007100090209     d   IND_char                     1    dim(44)
007200090209      *
007300100630     D Lunghezza_Indirizzo...
007400100630     D                 s              5u 0
007500090209      *
007600100630     D dalCarattere    s              5u 0
007700100630     D xTOTcaratteri   s              5u 0
007800090209     C*---------------------------------------------------------------*
007900090209     D MsgDSP          S            256                                         Testo generico
008000090209     C*---------------------------------------------------------------*
008100090209     D SndMg01         S             10                                         Message type
008200090209     D                                     INZ('*INFO')
008300090209     D SndMg02         S             10                                         Deluvery mode
008400090209     D                                     INZ('*BREAK')
008500090209     D SndMg03         S            256                                         Message text
008600090209     D SndMg04         S             10I 0                                      Length of text
008700090209     D                                     INZ(%SIZE(SndMg03))
008800090209     D SndMg05         S             10                                         User profile
008900090209     D                                     DIM(299)
009000090209     D SndMg06         S             10I 0                                      Number of user
009100090209     D SndMg07         S             10I 0                                      Message sent indic.
009200090209     D SndMg08         S             10I 0                                      Function requested
009300090209     D SndMg10         S              1                                         Show display
009400090209     D                                     INZ('N')
009500090209     D SndMg11         S             20                                         Qualified MSGQ name
009600090209     D SndMg12         S              4                                         Name type indicator
009700090209     D                                     INZ('*USR')
009800090209      * indice di scihera
009900090209     D Jx              S              5I 0
010000090209      *
010100090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
010200090209      *
010300090209     d bart_it         C                   CONST('@Bartolini.it;')
010400090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
010500060612      * ?================================================================== */
010600060612      * ?   * Campi x decodifica * (INPUT  del Record)
010700100319      * ?================================================================== */
010800090130     D  Dati           s           2048
010900060612      * ?   *--------------------------------------------------------------*
011000060612     D  se_errore      s              1    inz(' ')
011100060612      * ?* ------------------------------------------------------ *
011200060710     D Digits          C                   '0123456789'
011300091019     D*------------------
011400091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
011500091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011600060612      * ?================================================================== */
011700060612      *   Ciclo principale
011800090205      * ?================================================================== */
011900100709      *
012000100709     c     *ENTRY        PLIST
012100100709     C                   parm                    esito             1
012200100927     C                   parm                    TIPO_FILE         6
012300100927     c                   parm                    Versione          3
012400100927     c                   parm                    release           3
012500100927     c                   parm                    Agenzia           3
012600100927     c                   parm                    Associazione      6
012700101005     c                   parm                    PGM_traduz       10
012800101015      *
012900101015      * Recupero data e ora
013000101015     C                   TIME                    W0140
013100101015      * UDATE IN GGMMAAAA
013200101015     C                   MOVE      W0140         DataGGMMAAA
013300101015      * UDATE IN AAAAMMGG
013400101015     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
013500101015     C     *iso          MOVEL     DATA_eur      dateu_AMG
013600100726      *
013700100727     C                   eval      esito = '0'
013800100726     C                   clear                   TIPO_FILE
013900100927     C                   clear                   Versione
014000100927     C                   clear                   release
014100100927     C                   clear                   Agenzia
014200100927     C                   clear                   Associazione
014300101005     C                   clear                   PGM_traduz
014400100709      *
014500060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
014600060612     C*
014700101005     c                   eval       Quale_File       = *blank
014800100709     c                   eval       x_quale_versione = *blank
014900100709     c                   eval       x_quale_release  = *blank
015000100712     c                   eval       Agency           = *blank
015100100712     c                   eval       Association      = *blank
015200100709     C*
015300060612     C                   if        not %open(tivin00r)
015400060612     C                   open      tivin00r
015500060612     C                   endif
015600060612      **
015700100709     c     *start        setll     Tivin00r
015800100709
015900100709      * ?              /*---------------------- */
016000100709     c                   EXSR      LEGGE_TIVIN_R
016100100709      * ?              /*---------------------- */
016200100726      **
016300100726     c                   if        EoFTivin = 'S'
016400100928     C                   eval        TIPO_FILE   = 'FINE_F'
016500100726     C                   endif
016600100726      **
016700100709     c                   dow       EoFTivin <> 'S'
016800100709
016900100709     c                   IF        vinFLG = *blank and vinDTA <> *blank
017000101005      ** Decodifica record UNB
017100101005      * ?              /*---------------------- */
017200101005     c                   If        tipo_seg = 'UNB'
017300101005     C                   clear                   keyUNBCLI
017400101005     C                   clear                   keyTIPOMSG
017500101005     C                   clear                   keyVERSION
017600101005     C                   clear                   keyRELEASE
017700101005     C                   clear                   keyAGENCY
017800101005     C                   clear                   keyASSOCIA
017900101005      *
018000101005     c                   call      'TRTCTUNBR'
018100101005     c                   parm                    tipo_seg
018200101005     c                   parm                    segmento
018300101005     C                   parm                    keyUNBCLI
018400101005     C                   parm                    keyTIPOMSG
018500101005     C                   parm                    keyVERSION
018600101005     C                   parm                    keyRELEASE
018700101005     C                   parm                    keyAGENCY
018800101005     C                   parm                    keyASSOCIA
018900101005     c                   parm                    Errore
019000101005     c                   parm                    EDS00UNB
019100101005     c                   parm                    Valori_inErr
019200101005     c                   parm                    Descr_Errore
019300101005      * dopo aver decodificato
019400101005      * ?              /*---------------------- */
019500101005     C                   EXSR      DATI_UNB
019600101005      * ?              /*---------------------- */
019700101005     c                   end
019800101005      *
019900100709      ** Decodifica record UNH
020000100709      * ?              /*---------------------- */
020100100709     c                   If        tipo_seg = 'UNH'
020200100716      *
020300100716     C                   clear                   keyUNBCLI
020400100716     C                   clear                   keyTIPOMSG
020500100716     C                   clear                   keyVERSION
020600100716     C                   clear                   keyRELEASE
020700100716     C                   clear                   keyAGENCY
020800100716     C                   clear                   keyASSOCIA
020900100716      *
021000100709     c                   call      'TRTCTUNHR'
021100100709     c                   parm                    tipo_seg
021200100709     c                   parm                    segmento
021300100716     C                   parm                    keyUNBCLI
021400100716     C                   parm                    keyTIPOMSG
021500100716     C                   parm                    keyVERSION
021600100716     C                   parm                    keyRELEASE
021700100716     C                   parm                    keyAGENCY
021800100716     C                   parm                    keyASSOCIA
021900100709     c                   parm                    Errore
022000100709     c                   parm                    EDS00UNH
022100100709     c                   parm                    Valori_inErr
022200100709     c                   parm                    Descr_Errore
022300100709      * dopo aver decodificato
022400100709      * ?              /*---------------------- */
022500100709     C                   EXSR      DATI_UNH
022600100709      * ?              /*---------------------- */
022700100709      * esce per chiamre il traduttore
022800100709     c                   leave
022900100709     c                   end
023000100709     c                   endIF
023100100709
023200100709      * ?              /*---------------------- */
023300100709     c                   EXSR      LEGGE_TIVIN_R
023400100709      * ?              /*---------------------- */
023500100709     C                   ENDdo
023600100708      * chiude TIVIN
023700100708     C                   if        %open(tivin00r)
023800100708     C                   close     tivin00r
023900100708     C                   endif
024000060614      *
024100100709      *
024200100709      *  programmi di traduzione
024300100928     C                   if          TIPO_FILE  <> 'FINE_F'
024400101005     C                   eval        TIPO_FILE   = Quale_File
024500100927     c                   eval        Versione    = x_quale_versione
024600100927     c                   eval        release     = x_quale_release
024700100927     c                   eval        Agenzia     = Agency
024800100927     c                   eval        Associazione= Association
024900100928     C                   endif
025000100709      *
025100100709      *
025200101005     c                   if        Quale_File   <> 'IFCSUM' and
025300101005     c                             Quale_File   <> 'IFTMIN' and
025400101005     c                             Quale_File   <> 'IFTSTA' and
025500100928     C                               TIPO_FILE  <> 'FINE_F'
025600101123     c                             or
025700101123     c                              pgm_traduz = *blank
025800100709     c                   exsr      Invio_Mail
025900100709     C                   endif
026000100709      *
026100060612     c                   SETON                                        LR
026200060612      * ?================================================================== */
026300100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
026400100709      * ?================================================================== */
026500100709     C     LEGGE_TIVIN_R BEGSR
026600100709      *
026700100727     c     ri_Legge      tag
026800100709     c                   clear                   EoFTivin
026900100709      *  Lettura sequenziale
027000100709     c                   read      Tivin00r
027100100709
027200100709     c                   if        %Eof(tivin00r)
027300100709     c                   move      'S'           EoFTivin
027400100709     c                   else
027500100727      *
027600100727      * nuova lettura x salto riga già utilizzata
027700100727     c                   if        vinFLG <> *blank
027800100727     c                   goto      ri_Legge
027900100727     c                   end
028000100709      *
028100100709      * ?  imposta il tipo di segmento:
028200100709     c                   clear                   dati
028300100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
028400100728     c                   eval      segmento = %trimL(dati)
028500100728     c                   eval      tipo_seg = %subst(segmento:1:3)
028600100709      **
028700100709     c                   end
028800100709      *
028900100709     C                   ENDSR
029000101005      * ?================================================================== */
029100101005     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
029200101005      * ?================================================================== */
029300101005     C     DATI_UNB      BEGSR
029400101005      *
029500101005      *  Tipo di messaggio
029600101005     c                   clear                   UNB_mittente     35
029700101005     c                   eval       UNB_mittente = UNB0004
029800101005     c                   move      UNB000720     UNB_mittente
029900101005      **
030000101005     c     end_UNB       endSR
030100090227      * ?================================================================== */
030200090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
030300090227      * ?================================================================== */
030400090227     C     DATI_UNH      BEGSR
030500100316      *
030600100709      *  Tipo di messaggio
030700101005     c                   eval       Quale_File       = UNH0065
030800100709     c                   eval       x_quale_versione = UNH0052
030900100709     c                   eval       x_quale_release  = UNH0054
031000100712     c                   eval       Agency           = UNH0051
031100100712     c                   eval       Association      = UNH0057
031200101005      *
031300101019     c     keytab        klist
031400101019     c                   kfld                    tabcod
031500101019     c                   kfld                    tabkey
031600101019      *
031700101019     c                   movel     'PZ'          tabcod
031800101019     c                   eval       tabkey = %subst(UNB_mittente:1:31)
031900101019     c                   clear                   pgm_traduz
032000101019      *
032100101019     c     keytab        setll     edTAB01l
032200101019     c     tabcod        reade     edTAB01l
032300101015      **
032400101019     c                   dow       not %Eof(edTAB01l)
032500101015      ** Basta il Parziale
032600101019     c                   if         %subst(tabKEY:1:31)
032700101006     c                               = %subst(UNB_mittente:1:31)
032800101019      *   Valorizza la DS
032900101021     c                   eval      edidsPZ = tabUNI
033000101019      * Imposta la DS
033100101019      *   per agganciare il pgm di traduzione
033200101019     c                   movel     'PR'          tabcod
033300101019     c                   clear                   tabkey
033400101019      * Bolle
033500101015     c                   if         Quale_File  =  'IFCSUM'
033600101019     c                   eval      tabkey = §pzPGMrczB
033700101005      * Stati di consegna
033800101015     c                   elseIf     Quale_File  =  'IFTSTA'
033900101019     c                   eval      tabkey = §pzPGMrczS
034000101019     c                   end
034100101019      *
034200101019     c     keytab        chain     edTAB01l
034300101019      *  Nome del programma di traduzione
034400101019     c                   if        %Found(edTAB01l)
034500101021     c                   eval      edidsPR = tabUNI
034600101019     c                   eval       pgm_traduz = §PRTRADUT
034700101005     c                   end
034800101019      **
034900101019      ** Comunque esce dal ciclo
035000101019     c                   leave
035100101005     c                   end
035200101015      **
035300101019     c     tabcod        reade     edTAB01l
035400101015     c                   endDo
035500101123      **
035600101123      ** se non ha trovato la tabella PT.
035700101123     c                   if         pgm_traduz = *blank
035800090303      **
035900101123      **
036000101123     c                   end
036100101123      **
036200090210     c     end_UNH       endSR
036300100709      * ?================================================================== */
036400090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
036500100709      * ?================================================================== */
036600090205     C     *pssr         BEGSR
036700090205     C
036800060710     C                   eval      esito = '2'
036900100709???? C                   EVAL      Oggetto ='Problemi UPLOAD TRADUZIONE EDI !!'
037000090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
037100100709     C                             NON tradotto EDI ricevuto su UPLOAD.'
037200100709     c                   exsr      Invio_Mail
037300060621     C
037400090213     C                   ENDSR
037500090213     c*==================================================================*
037600090213      * Manda un Msg x E-mail
037700090213     c*==================================================================*
037800090213     c     Invio_Mail    begsr
037900090213      *
038000100709     c                   exsr      SEND_MSG_EDP
038100100709      *
038200090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
038300090213     C* Inizializzo variabili
038400090213     C                   movel     *blanks       wrkEml          253
038500090213     C                   movel     *blanks       wrkMsg         5000
038600090213     C                   movel     *blanks       wrkOgg           44
038700090213      *
038800090213     C* Valorizzo i campi della e-m@ail - indirizzo
038900100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
039000100709     C                   eval      wrkEml = CED_BART
039100090213     C                   eval      wrkOgg = Oggetto
039200090213     C                   eval      wrkMsg = Messaggio
039300100319      *
039400090213     C                   call(e)   'TIS701C'
039500090213     C                   parm                    wrkEml
039600090213     C                   parm                    wrkOgg
039700090213     C                   parm                    wrkMsg
039800090213     C*
039900090213     C                   ENDSR
040000090603      *
040100090211     ***********************************************************************
040200090211     **
040300090211     ** Send Message (QEZSNDMG) API
040400090211     **
040500090211     ***********************************************************************
040600090211     C     CalQEZSNDMG   BEGSR
040700090211      *
040800090211     ** Invio messaggio all'utente.
040900090211     C                   EVAL      SndMg03 = msgDSP
041000090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
041100100709     C                   EVAL      SndMg05(Jx) = CED_BART
041200090211     C                   EVAL      SndMg06 = Jx
041300090211     C                   CLEAR                   QUSEC
041400090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
041500090211
041600090211     C                   CALL      'QEZSNDMG'
041700090211     C                   PARM                    SndMg01
041800090211     C                   PARM                    SndMg02
041900090211     C                   PARM                    SndMg03
042000090211     C                   PARM                    SndMg04
042100090211     C                   PARM                    SndMg05
042200090211     C                   PARM                    SndMg06
042300090211     C                   PARM                    SndMg07
042400090211     C                   PARM                    SndMg08
042500090211     C                   PARM                    Qusec
042600090211     C                   PARM                    SndMg10
042700090211     C                   PARM                    SndMg11
042800090211     C                   PARM                    SndMg12
042900090211      *
043000090211     C                   ENDSR
043100100709     c*==================================================================*
043200100709      * Manda un Msg in Posta AS Sede a EDP*
043300100709     c*==================================================================*
043400100709     c     SEND_MSG_EDP  begsr
043500100709      *
043600100709      * INVIA MESSAGGIO ad un Utente --> EDPAB
043700100709      *   Lo manda una sola volta per lavoro
043800100709     c                   z-add     1             Jx
043900100709     C                   exsr      CalQEZSNDMG
044000100709      *
044100100709     c                   endsr
044200100708      * ?------------------------------------------------------------------ */
044300090211     O*****************************************************************
044400090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
044500090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
044600090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
044700090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
