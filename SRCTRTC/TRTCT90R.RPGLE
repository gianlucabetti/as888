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
010400141002     d CED_Bart        C                   CONST('CedAlert@Brt.it;')
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
012800141002      *
012900141002     C                   seton                                        LR
013000101015      *
013100101015      * Recupero data e ora
013200101015     C                   TIME                    W0140
013300101015      * UDATE IN GGMMAAAA
013400101015     C                   MOVE      W0140         DataGGMMAAA
013500101015      * UDATE IN AAAAMMGG
013600101015     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
013700101015     C     *iso          MOVEL     DATA_eur      dateu_AMG
013800100726      *
013900100727     C                   eval      esito = '0'
014000100726     C                   clear                   TIPO_FILE
014100100927     C                   clear                   Versione
014200100927     C                   clear                   release
014300100927     C                   clear                   Agenzia
014400100927     C                   clear                   Associazione
014500101005     C                   clear                   PGM_traduz
014600100709      *
014700060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
014800060612     C*
014900101005     c                   eval       Quale_File       = *blank
015000100709     c                   eval       x_quale_versione = *blank
015100100709     c                   eval       x_quale_release  = *blank
015200100712     c                   eval       Agency           = *blank
015300100712     c                   eval       Association      = *blank
015400100709     C*
015500060612     C                   if        not %open(tivin00r)
015600060612     C                   open      tivin00r
015700060612     C                   endif
015800060612      **
015900100709     c     *start        setll     Tivin00r
016000100709
016100100709      * ?              /*---------------------- */
016200100709     c                   EXSR      LEGGE_TIVIN_R
016300100709      * ?              /*---------------------- */
016400100726      **
016500100726     c                   if        EoFTivin = 'S'
016600100928     C                   eval        TIPO_FILE   = 'FINE_F'
016700100726     C                   endif
016800100726      **
016900100709     c                   dow       EoFTivin <> 'S'
017000100709
017100100709     c                   IF        vinFLG = *blank and vinDTA <> *blank
017200101005      ** Decodifica record UNB
017300101005      * ?              /*---------------------- */
017400101005     c                   If        tipo_seg = 'UNB'
017500101005     C                   clear                   keyUNBCLI
017600101005     C                   clear                   keyTIPOMSG
017700101005     C                   clear                   keyVERSION
017800101005     C                   clear                   keyRELEASE
017900101005     C                   clear                   keyAGENCY
018000101005     C                   clear                   keyASSOCIA
018100101005      *
018200101005     c                   call      'TRTCTUNBR'
018300101005     c                   parm                    tipo_seg
018400101005     c                   parm                    segmento
018500101005     C                   parm                    keyUNBCLI
018600101005     C                   parm                    keyTIPOMSG
018700101005     C                   parm                    keyVERSION
018800101005     C                   parm                    keyRELEASE
018900101005     C                   parm                    keyAGENCY
019000101005     C                   parm                    keyASSOCIA
019100101005     c                   parm                    Errore
019200101005     c                   parm                    EDS00UNB
019300101005     c                   parm                    Valori_inErr
019400101005     c                   parm                    Descr_Errore
019500101005      * dopo aver decodificato
019600101005      * ?              /*---------------------- */
019700101005     C                   EXSR      DATI_UNB
019800101005      * ?              /*---------------------- */
019900101005     c                   end
020000101005      *
020100100709      ** Decodifica record UNH
020200100709      * ?              /*---------------------- */
020300100709     c                   If        tipo_seg = 'UNH'
020400100716      *
020500100716     C                   clear                   keyUNBCLI
020600100716     C                   clear                   keyTIPOMSG
020700100716     C                   clear                   keyVERSION
020800100716     C                   clear                   keyRELEASE
020900100716     C                   clear                   keyAGENCY
021000100716     C                   clear                   keyASSOCIA
021100100716      *
021200100709     c                   call      'TRTCTUNHR'
021300100709     c                   parm                    tipo_seg
021400100709     c                   parm                    segmento
021500100716     C                   parm                    keyUNBCLI
021600100716     C                   parm                    keyTIPOMSG
021700100716     C                   parm                    keyVERSION
021800100716     C                   parm                    keyRELEASE
021900100716     C                   parm                    keyAGENCY
022000100716     C                   parm                    keyASSOCIA
022100100709     c                   parm                    Errore
022200100709     c                   parm                    EDS00UNH
022300100709     c                   parm                    Valori_inErr
022400100709     c                   parm                    Descr_Errore
022500100709      * dopo aver decodificato
022600100709      * ?              /*---------------------- */
022700100709     C                   EXSR      DATI_UNH
022800100709      * ?              /*---------------------- */
022900100709      * esce per chiamre il traduttore
023000100709     c                   leave
023100100709     c                   end
023200100709     c                   endIF
023300100709
023400100709      * ?              /*---------------------- */
023500100709     c                   EXSR      LEGGE_TIVIN_R
023600100709      * ?              /*---------------------- */
023700100709     C                   ENDdo
023800100708      * chiude TIVIN
023900100708     C                   if        %open(tivin00r)
024000100708     C                   close     tivin00r
024100100708     C                   endif
024200060614      *
024300100709      *
024400100709      *  programmi di traduzione
024500100928     C                   if          TIPO_FILE  <> 'FINE_F'
024600101005     C                   eval        TIPO_FILE   = Quale_File
024700100927     c                   eval        Versione    = x_quale_versione
024800100927     c                   eval        release     = x_quale_release
024900100927     c                   eval        Agenzia     = Agency
025000100927     c                   eval        Associazione= Association
025100100928     C                   endif
025200100709      *
025300100709      *
025400101005     c                   if        Quale_File   <> 'IFCSUM' and
025500101005     c                             Quale_File   <> 'IFTMIN' and
025600101005     c                             Quale_File   <> 'IFTSTA' and
025700100928     C                               TIPO_FILE  <> 'FINE_F'
025800101123     c                             or
025900101123     c                              pgm_traduz = *blank
026000100709     c                   exsr      Invio_Mail
026100100709     C                   endif
026200100709      *
026300141002     c                   return
026400141002     c********           SETON                                        LR
026500060612      * ?================================================================== */
026600100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
026700100709      * ?================================================================== */
026800100709     C     LEGGE_TIVIN_R BEGSR
026900100709      *
027000100727     c     ri_Legge      tag
027100100709     c                   clear                   EoFTivin
027200100709      *  Lettura sequenziale
027300100709     c                   read      Tivin00r
027400100709
027500100709     c                   if        %Eof(tivin00r)
027600100709     c                   move      'S'           EoFTivin
027700100709     c                   else
027800100727      *
027900100727      * nuova lettura x salto riga già utilizzata
028000100727     c                   if        vinFLG <> *blank
028100100727     c                   goto      ri_Legge
028200100727     c                   end
028300100709      *
028400100709      * ?  imposta il tipo di segmento:
028500100709     c                   clear                   dati
028600100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
028700100728     c                   eval      segmento = %trimL(dati)
028800100728     c                   eval      tipo_seg = %subst(segmento:1:3)
028900100709      **
029000100709     c                   end
029100100709      *
029200100709     C                   ENDSR
029300101005      * ?================================================================== */
029400101005     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
029500101005      * ?================================================================== */
029600101005     C     DATI_UNB      BEGSR
029700101005      *
029800101005      *  Tipo di messaggio
029900101005     c                   clear                   UNB_mittente     35
030000101005     c                   eval       UNB_mittente = UNB0004
030100101005     c                   move      UNB000720     UNB_mittente
030200101005      **
030300101005     c     end_UNB       endSR
030400090227      * ?================================================================== */
030500090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
030600090227      * ?================================================================== */
030700090227     C     DATI_UNH      BEGSR
030800100316      *
030900100709      *  Tipo di messaggio
031000101005     c                   eval       Quale_File       = UNH0065
031100100709     c                   eval       x_quale_versione = UNH0052
031200100709     c                   eval       x_quale_release  = UNH0054
031300100712     c                   eval       Agency           = UNH0051
031400100712     c                   eval       Association      = UNH0057
031500101005      *
031600101019     c     keytab        klist
031700101019     c                   kfld                    tabcod
031800101019     c                   kfld                    tabkey
031900101019      *
032000101019     c                   movel     'PZ'          tabcod
032100101019     c                   eval       tabkey = %subst(UNB_mittente:1:31)
032200101019     c                   clear                   pgm_traduz
032300101019      *
032400101019     c     keytab        setll     edTAB01l
032500101019     c     tabcod        reade     edTAB01l
032600101015      **
032700101019     c                   dow       not %Eof(edTAB01l)
032800101015      ** Basta il Parziale
032900101019     c                   if         %subst(tabKEY:1:31)
033000101006     c                               = %subst(UNB_mittente:1:31)
033100101019      *   Valorizza la DS
033200101021     c                   eval      edidsPZ = tabUNI
033300101019      * Imposta la DS
033400101019      *   per agganciare il pgm di traduzione
033500101019     c                   movel     'PR'          tabcod
033600101019     c                   clear                   tabkey
033700101019      * Bolle
033800101015     c                   if         Quale_File  =  'IFCSUM'
033900101019     c                   eval      tabkey = §pzPGMrczB
034000101005      * Stati di consegna
034100101015     c                   elseIf     Quale_File  =  'IFTSTA'
034200101019     c                   eval      tabkey = §pzPGMrczS
034300101019     c                   end
034400101019      *
034500101019     c     keytab        chain     edTAB01l
034600101019      *  Nome del programma di traduzione
034700101019     c                   if        %Found(edTAB01l)
034800101021     c                   eval      edidsPR = tabUNI
034900101019     c                   eval       pgm_traduz = §PRTRADUT
035000101005     c                   end
035100101019      **
035200101019      ** Comunque esce dal ciclo
035300101019     c                   leave
035400101005     c                   end
035500101015      **
035600101019     c     tabcod        reade     edTAB01l
035700101015     c                   endDo
035800101123      **
035900101123      ** se non ha trovato la tabella PT.
036000101123     c                   if         pgm_traduz = *blank
036100090303      **
036200101123      **
036300101123     c                   end
036400101123      **
036500090210     c     end_UNH       endSR
036600100709      * ?================================================================== */
036700090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
036800100709      * ?================================================================== */
036900090205     C     *pssr         BEGSR
037000090205     C
037100060710     C                   eval      esito = '2'
037200100709???? C                   EVAL      Oggetto ='Problemi UPLOAD TRADUZIONE EDI !!'
037300090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
037400100709     C                             NON tradotto EDI ricevuto su UPLOAD.'
037500100709     c                   exsr      Invio_Mail
037600141002      *
037700141002     c*****              return
037800141002      *
037900141002     C                   ENDSR     '*CANCL'
038000090213     c*==================================================================*
038100090213      * Manda un Msg x E-mail
038200090213     c*==================================================================*
038300090213     c     Invio_Mail    begsr
038400090213      *
038500141002      * Non serve più:
038600141002     c********           exsr      SEND_MSG_EDP
038700100709      *
038800090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
038900090213     C* Inizializzo variabili
039000090213     C                   movel     *blanks       wrkEml          253
039100090213     C                   movel     *blanks       wrkMsg         5000
039200090213     C                   movel     *blanks       wrkOgg           44
039300090213      *
039400090213     C* Valorizzo i campi della e-m@ail - indirizzo
039500100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
039600100709     C                   eval      wrkEml = CED_BART
039700090213     C                   eval      wrkOgg = Oggetto
039800090213     C                   eval      wrkMsg = Messaggio
039900100319      *
040000110203      ********
040100110203     C********           call(e)   'TIS701C'
040200110203     C********           parm                    wrkEml
040300110203     C********           parm                    wrkOgg
040400110203     C********           parm                    wrkMsg
040500110203      ** *****
040600110203     C                   call(e)   'TRTCT00R2'
040700110203     C                   parm                    wrkEml
040800110203     C                   parm                    wrkOgg
040900110203     C                   parm                    wrkMsg
041000110203     C*
041100090213     C*
041200090213     C                   ENDSR
041300090603      *
041400090211     ***********************************************************************
041500090211     **
041600090211     ** Send Message (QEZSNDMG) API
041700090211     **
041800090211     ***********************************************************************
041900090211     C     CalQEZSNDMG   BEGSR
042000090211      *
042100090211     ** Invio messaggio all'utente.
042200090211     C                   EVAL      SndMg03 = msgDSP
042300090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
042400100709     C                   EVAL      SndMg05(Jx) = CED_BART
042500090211     C                   EVAL      SndMg06 = Jx
042600090211     C                   CLEAR                   QUSEC
042700090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
042800090211
042900090211     C                   CALL      'QEZSNDMG'
043000090211     C                   PARM                    SndMg01
043100090211     C                   PARM                    SndMg02
043200090211     C                   PARM                    SndMg03
043300090211     C                   PARM                    SndMg04
043400090211     C                   PARM                    SndMg05
043500090211     C                   PARM                    SndMg06
043600090211     C                   PARM                    SndMg07
043700090211     C                   PARM                    SndMg08
043800090211     C                   PARM                    Qusec
043900090211     C                   PARM                    SndMg10
044000090211     C                   PARM                    SndMg11
044100090211     C                   PARM                    SndMg12
044200090211      *
044300090211     C                   ENDSR
044400100709     c*==================================================================*
044500100709      * Manda un Msg in Posta AS Sede a EDP*
044600100709     c*==================================================================*
044700100709     c     SEND_MSG_EDP  begsr
044800100709      *
044900100709      * INVIA MESSAGGIO ad un Utente --> EDPAB
045000100709      *   Lo manda una sola volta per lavoro
045100100709     c                   z-add     1             Jx
045200100709     C                   exsr      CalQEZSNDMG
045300100709      *
045400100709     c                   endsr
045500100708      * ?------------------------------------------------------------------ */
045600090211     O*****************************************************************
045700090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
045800090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
045900090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
046000090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
