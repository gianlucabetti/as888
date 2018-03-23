000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500110513      *  Da MANIFEST  DEVE CHIAMARE il TRADUTTORE per esPORTARE gli IFCSUM      *
000600060609      **?************************************************************************
000700110513     FEDIfcsum  iF   E             DISK    usropn  INFDS(SuMRECDS)
000800101019     Fedtab01l  iF   E           k DISK
000900110513     I*---------------------------------------------------------------------*
001000100709      * Stringhe per conversione alfanumerico/numerico N.Documento
001100100709     D CA              S             78    DIM(1) CTDATA PERRCD(1)
001200100709     D CN              S             78    DIM(1) CTDATA PERRCD(1)
001300100709      **?************************************************************************
001400090213      * numero relativo di record
001500110513     D SuMRECDS        DS
001600090213     D  VNREC                397    400B 0
001700110715     D*
001800110715     D pgmstatus      SDS
001900110715     D  JOB_NAME             244    253                                         * Job name
002000110715     D  USER                 254    263                                         * User name
002100110715     D  JOB_NUM              264    269                                         * Job number
002200100701      *----------------------------------------------------*
002300090205      **  Elenco DS di tutti i Segmenti da tradurre
002400100701      *----------------------------------------------------*
002500110513???? D edTT00ds      E DS
002600110513      *----------------------------------------------------*
002700101019???? D edidspr       E DS
002800101019???? D edidspz       E DS
002900110516???? D edidspt       E DS
003000100701      *----------------------------------------------------*
003100090209     D Valori_inErr    ds
003200090209     D  SKout_Errori                  1    DIM(50)
003300100630      **
003400090209     D Descr_Errore    ds
003500090209     D  SKout_DesErr                 50    DIM(50)
003600090205      *----------------------------------------------------*
003700110513     D EoFEdSUM        s              1
003800091016      *----------------------------------------------------*
003900110526     D Tipo_seg        S              4
004000100716      *
004100090209     D segmento        s           2048
004200090209     D Errore          s              1
004300101015      *
004400101015     D W0140           S             14  0
004500101015     D DataGGMMAAA     S              8  0
004600101015     D DATEU_AMG       S              8  0
004700101015     D DATA_eur        S               D   DATFMT(*eur)
004800101015      *
004900100318      *------------------------------------------
005000110513???? D Segm_Testata...
005100110513???? D                 s              4    INZ('TT00')
005200100709      *------------------------------------------
005300090209      *  Invio E-mail
005400090209     D Indirizzi       s            253
005500090209     D Oggetto         s             44
005600090209     D Messaggio       s           5000
005700090209      *
005800090209     d   DSmail        ds
005900090209     D Mail_Ind                      44
006000090209     d   IND_char                     1    dim(44)
006100090209      *
006200100630     D Lunghezza_Indirizzo...
006300100630     D                 s              5u 0
006400090209      *
006500100630     D dalCarattere    s              5u 0
006600100630     D xTOTcaratteri   s              5u 0
006700090209     C*---------------------------------------------------------------*
006800090209     D MsgDSP          S            256                                         Testo generico
006900090209     C*---------------------------------------------------------------*
007000090209     D SndMg01         S             10                                         Message type
007100090209     D                                     INZ('*INFO')
007200090209     D SndMg02         S             10                                         Deluvery mode
007300090209     D                                     INZ('*BREAK')
007400090209     D SndMg03         S            256                                         Message text
007500090209     D SndMg04         S             10I 0                                      Length of text
007600090209     D                                     INZ(%SIZE(SndMg03))
007700090209     D SndMg05         S             10                                         User profile
007800090209     D                                     DIM(299)
007900090209     D SndMg06         S             10I 0                                      Number of user
008000090209     D SndMg07         S             10I 0                                      Message sent indic.
008100090209     D SndMg08         S             10I 0                                      Function requested
008200090209     D SndMg10         S              1                                         Show display
008300090209     D                                     INZ('N')
008400090209     D SndMg11         S             20                                         Qualified MSGQ name
008500090209     D SndMg12         S              4                                         Name type indicator
008600090209     D                                     INZ('*USR')
008700090209      * indice di scihera
008800090209     D Jx              S              5I 0
008900090209      *
009000090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
009100090209      *
009200090209     d bart_it         C                   CONST('@Bartolini.it;')
009300110516     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
009400060612      * ?================================================================== */
009500060612      * ?   * Campi x decodifica * (INPUT  del Record)
009600100319      * ?================================================================== */
009700090130     D  Dati           s           2048
009800060612      * ?   *--------------------------------------------------------------*
009900060612     D  se_errore      s              1    inz(' ')
010000060612      * ?* ------------------------------------------------------ *
010100060710     D Digits          C                   '0123456789'
010200091019     D*------------------
010300091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010400091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010500110513      *----------------------------------------------------*
010600110513     I* FILE
010700110513     I*---------------------------------------------------------------------*
010800110513     IRRIFCSUM
010900110513     I              RRIFCSUM                    SUMDAT
011000060612      * ?================================================================== */
011100060612      *   Ciclo principale
011200090205      * ?================================================================== */
011300110516      *
011400110516     c     keytab        klist
011500110516     c                   kfld                    tabcod
011600110516     c                   kfld                    tabkey
011700100709      *
011800100709     c     *ENTRY        PLIST
011900100709     C                   parm                    esito             1
012000110516     C                   parm                    aINTESA           1
012100110513     c                   parm                    PGM_traduz       10
012200110516     C                   parm                    KscTabPT          7 0
012300101015      *
012400101015      * Recupero data e ora
012500101015     C                   TIME                    W0140
012600101015      * UDATE IN GGMMAAAA
012700101015     C                   MOVE      W0140         DataGGMMAAA
012800101015      * UDATE IN AAAAMMGG
012900101015     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
013000101015     C     *iso          MOVEL     DATA_eur      dateu_AMG
013100100726      *
013200100727     C                   eval      esito = '0'
013300110516     C                   move      'S'           aINTESA
013400101005     C                   clear                   PGM_traduz
013500110516     C                   clear                   KscTabPT
013600110715     C                   clear                   edidspr
013700110715     C                   clear                   edidspz
013800110715     C                   clear                   edidspt
013900110715     C                   clear                   MOTIVO           35
014000100709      *
014100110513      **  da EDIfcsum esegue la pretraduzione portando su DDS ogni record
014200110513     C                   if        not %open(EDIfcsum)
014300110513     C                   open      EDIfcsum
014400060612     C                   endif
014500060612      **
014600110513     c     *start        setll     EDIfcsum
014700100709      * ?              /*---------------------- */
014800110513     c                   EXSR      LEGGE_EdSUM_R
014900100709      * ?              /*---------------------- */
015000110715      **
015100110715      **  se proprio non ci sono dati nel file
015200110715     c                   if        EoFEdSUM = 'S'
015300110715     c                   exsr      msg_vuoto
015400110715     c                   end
015500100726      **
015600110513     c                   dow       EoFEdSUM <> 'S'
015700110516
015800110513     c                   If        tipo_seg = Segm_Testata
015900110516      ** Decodifica record TT00
016000100709      * ?              /*---------------------- */
016100110513     C                   EXSR      DATI_TT00
016200100709      * ?              /*---------------------- */
016300100709      * esce per chiamre il traduttore
016400100709     c                   leave
016500100709     c                   end
016600100709
016700100709      * ?              /*---------------------- */
016800110513     c                   EXSR      LEGGE_EdSUM_R
016900100709      * ?              /*---------------------- */
017000100709     C                   ENDdo
017100110513      * chiude EdSUM
017200110513     C                   if        %open(EDIfcsum)
017300110513     C                   close     EDIfcsum
017400100708     C                   endif
017500060614      *
017600100709      *  programmi di traduzione
017700110516     c                   if          pgm_traduz = *blank
017800110516     c                             or
017900110516     c                               EoFEdSUM = 'S'
018000100709     c                   exsr      Invio_Mail
018100100709     C                   endif
018200100709      *
018300060612     c                   SETON                                        LR
018400060612      * ?================================================================== */
018500110513      *?    Legge il EdSUM eseguendo subito delle operazioni Essenziali
018600100709      * ?================================================================== */
018700110513     C     LEGGE_EdSUM_R BEGSR
018800100709      *
018900110513     c                   clear                   EoFEdSUM
019000100709      *  Lettura sequenziale
019100110513     c                   read      EDIfcsum
019200100709
019300110513     c                   if        %Eof(EDIfcsum)
019400110513     c                   move      'S'           EoFEdSUM
019500100709     c                   else
019600100709      *
019700100709      * ?  imposta il tipo di segmento:
019800100709     c                   clear                   dati
019900110513     C     minu:maiu     XLATE     SuMDAT        dati                           *Minus -> Maiuscolo
020000100728     c                   eval      segmento = %trimL(dati)
020100110513     c                   eval      tipo_seg = %subst(segmento:5:4)
020200100709     c                   end
020300100709      *
020400100709     C                   ENDSR
020500090227      * ?================================================================== */
020600090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
020700090227      * ?================================================================== */
020800110513     C     DATI_TT00     BEGSR
020900110526      **
021000110526     c                   move      *blank        PGM_traduz       10
021100110526     C                   move      '0'           esito             1
021200110526     C                   move      'N'           aINTESA           1
021300101005      *
021400110516      *  imposta i dati del flat sul segmento
021500110516     c                   eval      edTT00ds = segmento
021600101019      *
021700101019     c                   movel     'PZ'          tabcod
021800110526     c                   eval       tabkey = %subst(TA0010:1:31)
021900130522     c*********          move      TAA007        tabkey
022000101019     c                   clear                   pgm_traduz
022100101019      *
022200101019     c     keytab        setll     edTAB01l
022300101019     c     tabcod        reade     edTAB01l
022400101015      **
022500101019     c                   dow       not %Eof(edTAB01l)
022600101015      ** Basta il Parziale
022700101019     c                   if         %subst(tabKEY:1:31)
022800110526     c                               = %subst(TA0010:1:31)
022900101019      *   Valorizza la DS
023000101021     c                   eval      edidsPZ = tabUNI
023100110526      *
023200110516     c                   movel     'PT'          tabcod
023300110516     c     keytab        chain     edTAB01l
023400110516     c                   if        %Found(edTAB01l)
023500110516     c                   eval      edidsPT = tabUNI
023600110516     c                   eval       kscTabPT = §PTksc
023700110516     c                   end
023800110516      **
023900101019      * Imposta la DS
024000101019      *   per agganciare il pgm di traduzione
024100101019     c                   movel     'PR'          tabcod
024200101019     c                   clear                   tabkey
024300110513     c                   eval      tabkey = §pzPGMtrsB
024400101019     c     keytab        chain     edTAB01l
024500101019      *  Nome del programma di traduzione
024600101019     c                   if        %Found(edTAB01l)
024700101021     c                   eval      edidsPR = tabUNI
024800110526     c                   eval       pgm_traduz = §PRTRADEX
024900101005     c                   end
025000101019      **
025100101019      ** Comunque esce dal ciclo
025200101019     c                   leave
025300101005     c                   end
025400101015      **
025500101019     c     tabcod        reade     edTAB01l
025600101015     c                   endDo
025700110516      **
025800180129      ** giro INTESA non esiste più
025900180129     C                   move      'N'           aINTESA
026000180129      **
026100101123      ** se non ha trovato la tabella PT.
026200101123     c                   if         pgm_traduz = *blank
026300110516     C                   move      '1'           esito
026400101123     c                   end
026500101123      **
026600090210     c     end_UNH       endSR
026700100709      * ?================================================================== */
026800090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
026900100709      * ?================================================================== */
027000090205     C     *pssr         BEGSR
027100090205     C
027200110715     c*
027300110715     c                   DUMP(A)
027400110715     c*
027500060710     C                   eval      esito = '2'
027600110516???? C                   EVAL      Oggetto ='Problemi DOWNLD MANIFEST EDI !!'
027700110707     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
027800090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
027900110707     C                             MANIFEST NON tradotto in DOWNLD. -
028000110707     C                             Controllare il Download del cod.Cliente:'
028100110707     C                   EVAL      Messaggio = %trim(Messaggio) +
028200110715     C                                         %editc(§PTksc:'X') + motivo +
028300110715     c                             ' - Verificare dump del job: '        +
028400110715     c                             %trim(JOB_NUM)+'/'+%trim(USER)+'/'    +
028500110715     c                             %trim(JOB_NAME)
028600110715     c
028700100709     c                   exsr      Invio_Mail
028800060621     C
028900090213     C                   ENDSR
029000090213     c*==================================================================*
029100110715     C     msg_vuoto     begSR
029200110715      *
029300110715     C                   eval       MOTIVO = ' < NON ci sono Bolle sul -
029400110715     c                              Manifest >'
029500110715      *
029600110715     C                   ENDSR
029700110715     c*==================================================================*
029800090213      * Manda un Msg x E-mail
029900090213     c*==================================================================*
030000090213     c     Invio_Mail    begsr
030100110516      *
030200110516???? C                   EVAL      Oggetto ='Problemi DOWNLD MANIFEST EDI !!'
030300110707     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
030400110516     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
030500110707     C                             MANIFEST NON tradotto in DOWNLD al cliente. -
030600110707     C                             Controllare il Download del Codice:'
030700110707     C                   EVAL      Messaggio = %trim(Messaggio) +
030800110715     C                                         %editc(§PTksc:'X') + Motivo
030900090213      *
031000110824      *  se non ci sono bolle sul manifest non aggancia la PT
031100110824      *   quindi non si deve inviare alcun messaggio
031200110824     c                   if        §ptKSC > 0
031300100709     c                   exsr      SEND_MSG_EDP
031400110909     c*******            end
031500100709      *
031600110516     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
031700090213     C* Inizializzo variabili
031800090213     C                   movel     *blanks       wrkEml          253
031900090213     C                   movel     *blanks       wrkMsg         5000
032000090213     C                   movel     *blanks       wrkOgg           44
032100090213      *
032200090213     C* Valorizzo i campi della e-m@ail - indirizzo
032300100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
032400100709     C                   eval      wrkEml = CED_BART
032500090213     C                   eval      wrkOgg = Oggetto
032600090213     C                   eval      wrkMsg = Messaggio
032700100319      *
032800110203     C                   call(e)   'TRTCT00R2'
032900110203     C                   parm                    wrkEml
033000110203     C                   parm                    wrkOgg
033100110203     C                   parm                    wrkMsg
033200110203     C*
033300110909     c                   end
033400090213     C*
033500090213     C                   ENDSR
033600090603      *
033700090211     ***********************************************************************
033800090211     **
033900090211     ** Send Message (QEZSNDMG) API
034000090211     **
034100090211     ***********************************************************************
034200090211     C     CalQEZSNDMG   BEGSR
034300090211      *
034400090211     ** Invio messaggio all'utente.
034500090211     C                   EVAL      SndMg03 = msgDSP
034600090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
034700100709     C                   EVAL      SndMg05(Jx) = CED_BART
034800090211     C                   EVAL      SndMg06 = Jx
034900090211     C                   CLEAR                   QUSEC
035000090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
035100090211
035200090211     C                   CALL      'QEZSNDMG'
035300090211     C                   PARM                    SndMg01
035400090211     C                   PARM                    SndMg02
035500090211     C                   PARM                    SndMg03
035600090211     C                   PARM                    SndMg04
035700090211     C                   PARM                    SndMg05
035800090211     C                   PARM                    SndMg06
035900090211     C                   PARM                    SndMg07
036000090211     C                   PARM                    SndMg08
036100090211     C                   PARM                    Qusec
036200090211     C                   PARM                    SndMg10
036300090211     C                   PARM                    SndMg11
036400090211     C                   PARM                    SndMg12
036500090211      *
036600090211     C                   ENDSR
036700100709     c*==================================================================*
036800100709      * Manda un Msg in Posta AS Sede a EDP*
036900100709     c*==================================================================*
037000100709     c     SEND_MSG_EDP  begsr
037100100709      *
037200100709      * INVIA MESSAGGIO ad un Utente --> EDPAB
037300100709      *   Lo manda una sola volta per lavoro
037400100709     c                   z-add     1             Jx
037500100709     C                   exsr      CalQEZSNDMG
037600100709      *
037700100709     c                   endsr
037800100708      * ?------------------------------------------------------------------ */
037900090211     O*****************************************************************
038000090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
038100090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
038200090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
038300090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
