000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600100712????  *  TRASCODIFICA : MESSAGGI EDI  -  STATI BOLLE EXPORT   IFTSTA vers D94A  *
000700060612      **?************************************************************************
000800100712      * Il pgm crea:  RCVIFTSTA                                                 *
000900060609      **?************************************************************************
001000100723     Ftivin00r  uF   E             DISK    INFDS(VINRECDS) usropn
001100100722     FEDTAB01L  IF   E           K DISK
001200101018     FtrtcT94W  O  A E           K DISK    INFDS(SRECDS)
001300100701      *----------------------------------------------------*
001400100701      * Stringhe per conversione alfanumerico/numerico N.Documento
001500100701     D CA              S             78    DIM(1) CTDATA PERRCD(1)
001600100701     D CN              S             78    DIM(1) CTDATA PERRCD(1)
001700940321      *----------------------------------------------------*
001800090209      * numero relativo di record
001900090209     D SRECDS          DS
002000090209     D  NRREC                397    400B 0
002100090209      *
002200090213      * numero relativo di record
002300090213     D VINRECDS        DS
002400090213     D  VNREC                397    400B 0
002500100701      * ---------------------------------------------------
002600100701     D KPJBA         E DS
002700100701     D     svkpjbu     s                   like(kpjbu)
002800100701     D UTEDSE0F      E DS
002900100701     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003000100701     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003100100701      * Ds scomposzione tipo capoconti
003200100701     D TCUDS           DS
003300100701     D  F34                    3      4
003400100701     D  F56                    5      6
003500100722      *----------------------------------------------------*
003600100701     D EDIDSPT       E DS
003700100701     D EDIDSPU       E DS
003800100701     D EDIDSPV       E DS
003900100722     D EDIDSMS       E DS
004000100701      **
004100100701     D WLBDA8          DS
004200100701     D  G02DAT                 1      8  0
004300100701     D  G02INV                 9     16  0
004400100701     D  G02ERR                17     17
004500100701     D  G02TGI                18     22  0
004600100712     D*---------------------------------------------------------------------*
004700100712     D* DS
004800100712     D*---------------------------------------------------------------------*
004900100712     D* .. per scompozione dati da spedire a seconda del tipo record
005000100712     D EDST00        E DS                  EXTNAME(EDST00DS)
005100100712     D EDST05        E DS                  EXTNAME(EDST05DS)
005200100712     D EDST10        E DS                  EXTNAME(EDST10DS)
005300100712     D EDST15        E DS                  EXTNAME(EDST15DS)
005400100712     D EDSD00        E DS                  EXTNAME(EDSD00DS)
005500100712     D EDSD05        E DS                  EXTNAME(EDSD05DS)
005600100712     D EDSD10        E DS                  EXTNAME(EDSD10DS)
005700100712     D EDSD15        E DS                  EXTNAME(EDSD15DS)
005800100712     D EDSD20        E DS                  EXTNAME(EDSD20DS)
005900100712     D*---------------------------------------------------------------------*
006000100712     D* .. per reperimento tipo record di EDIFTSTA da scrivere
006100100712     D*                    .. 5 - 8  : tipo record
006200100712     D WDAT            DS          1950
006300100712     D  STAPRG                 1      4  0
006400100712     D  STATPR                 5      8
006500100712     D*---------------------------------------------------------------------*
006600090213      *
006700100701      *----------------------------------------------------*
006800090205      **  Elenco DS di tutti i Segmenti da tradurre
006900100701      *----------------------------------------------------*
007000100712???? D edS00UNA      E DS
007100100712???? D edS00UNB      E DS
007200100712???? D edS00UNH      E DS
007300100712???? D edS00UNT      E DS
007400100712???? D edS00UNZ      E DS
007500091019???? D edS00BGM      E DS
007600100316???? D edS00CNI      E DS
007700091019???? D edS00DTM      E DS
007800100628???? D edS00FTX      E DS
007900091019???? D edS00GID      E DS
008000100316???? D edS00LOC      E DS
008100091019???? D edS00NAD      E DS
008200091019???? D edS00PCI      E DS
008300091019???? D edS00RFF      E DS
008400100712???? D edS00STS      E DS
008500100722      **---------------------------------------------------*
008600090209     D Valori_inErr    ds
008700090209     D  SKout_Errori                  1    DIM(50)
008800100630      **
008900090209     D Descr_Errore    ds
009000090209     D  SKout_DesErr                 50    DIM(50)
009100090205      *----------------------------------------------------*
009200091016     D EoFTivin        s              1
009300091016      *----------------------------------------------------*
009400090205     D Tipo_seg        S              3
009500100721     D Trovato_seg     S              1
009600100716      *
009700100716     d keyUNBCLI       s             35
009800100716     d keyTIPOMSG      s              6
009900100716     d keyVERSION      s              3
010000100716     d keyRELEASE      s              3
010100100716     d keyAGENCY       s              3
010200100716     d keyASSOCIA      s              6
010300100716      *
010400100721     D DsGenerica      s           2048
010500090209     D segmento        s           2048
010600090209     D Errore          s              1
010700100722      *------------------------------------------
010800000223     D W0140           S             14  0
010900991129     D WORA            S              6  0
011000100701     D DataGGMMAAA     S              8  0
011100100701     D DATEU           S              8  0
011200100701     D DATA_eur        S               D   DATFMT(*eur)
011300100723     D Work_Data__35   S             35
011400100723     D Work_Format_3   S              3
011500100722      *------------------------------------------
011600100708     D UNT_record      s                   LIKE(vnREC)
011700100708     D IniDET_RECord   s                   LIKE(vnREC)
011800100708     D FinDET_RECord   s                   LIKE(vnREC)
011900090213     D Last_RECord     s                   LIKE(vnREC)
012000110110     D UNZ_SEGMENTO    s              1
012100100722      *------------------------------------------
012200100722     D nad3035SF       s                   LIKE(NAD3035)
012300100723     D nad3036SF       s                   LIKE(NAD30361)
012400100722     D nad3039SF       s                   LIKE(NAD3039)
012500100722     D nad3035ST       s                   LIKE(NAD3035)
012600100723     D nad3036ST       s                   LIKE(NAD30361)
012700100722     D nad3039ST       s                   LIKE(NAD3039)
012800100722      *
012900100722     D LOC3227t1       s                   LIKE(LOC3227)
013000100722     D LOC3225t1       s                   LIKE(LOC3225)
013100100722     D LOC3227t2       s                   LIKE(LOC3227)
013200100722     D LOC3225t2       s                   LIKE(LOC3225)
013300100722     D dtm2005te       s                   LIKE(dtm2005)
013400100722     D dtm2380te       s                   LIKE(dtm2380)
013500100722     D dtm2379te       s                   LIKE(dtm2379)
013600100722     D rff1153te       s                   LIKE(rff1153)
013700100722     D rff1154te       s                   LIKE(rff1154)
013800100318      *------------------------------------------
013900100318      **  Definizione Qualificatori
014000100318      *------------------------------------------
014100100705???? D Data_senza_ora...
014200100628???? D                 s              3    INZ('102')
014300100705???? D Data_con_ora...
014400100628???? D                 s              3    INZ('203')
014500100705???? D Data_con_i_secondi...
014600100705???? D                 s              3    INZ('204')
014700100316      *---------------------------------------------------------------------*
014800100316      **  segmenti Identificativi parti specifiche del messaggio
014900100316      *---------------------------------------------------------------------*
015000100701???? D Segm_Inizio_Messaggio...
015100100701???? D                 s              3    INZ('UNB')
015200100701???? D Segm_Testata_Dettagli...
015300100701???? D                 s              3    INZ('UNH')
015400100701???? D Segm_Fine_messaggio...
015500100316???? D                 s              3    INZ('UNZ')
015600100722???? D Segm_Inizio_Dettaglio...
015700100722???? D                 s              3    INZ('CNI')
015800100722???? D Segm_Fine_Dettagli...
015900100722???? D                 s              3    INZ('UNT')
016000100722???? D Segm_Data_Dettaglio...
016100100722???? D                 s              3    INZ('DTM')
016200100722???? D Segm_Address_Dettaglio...
016300100722???? D                 s              3    INZ('NAD')
016400100722???? D Segm_Text_Dettaglio...
016500100722???? D                 s              3    INZ('FTX')
016600100722???? D Segm_Parcels_Dettaglio...
016700100722???? D                 s              3    INZ('PCI')
016800100701???? D seg_imprevisto...
016900100701???? D                 s              1
017000100723???? D eseguire_ROLBACK...
017100100723???? D                 s              1
017200100726???? D Forza_Uscita...
017300100726???? D                 s              1
017400090209      *---------------------------------------------------------------------*
017500100701???? D Contatore_Dettagli...
017600100701???? D                 s              5s 0 INZ(0)
017700100701      *---------------------------------------------------------------------*
017800100628      *
017900100701      *---------------------------------------------------------------------*
018000100630     d UNB_diWork      s                   like(UNB0004)
018100100628     d  UNB_Mittente   s                   like(UNB0004)
018200100712     d UNB_parziale    s              1    inz('N')
018300100712     d UNB_Parz        s             35    inz(' ')
018400100712     d UNB_Generico    s             35    inz(' ')
018500100712     d UNB_NrTrasmiss  s             14    inz(' ')
018600100712      *
018700100701      * Tabella partner
018800121105     D PT_key          S             35    DIM(200)
018900121105     D PT_Parz         S             35    DIM(%elem(PT_key))
019000121105     D PT_KSC          S              7    DIM(%elem(PT_key))
019100121105     D PT_uni          S             90    DIM(%elem(PT_key))
019200121105     D PU_uni          S             90    DIM(%elem(PT_key))
019300121105     D PV_uni          S             90    DIM(%elem(PT_key))
019400121105     D TRUL0SDS      E DS
019500100701      *
019600100701     d PT_KeyXsav      s              3  0 inz(0)
019700100701     D PT_trovata      s              1    inz('N')
019800100701     D PU_key          S             35
019900100701     D PV_key          S             35
020000100701     D Nr_PT           S              3  0 INZ
020100100701      *
020200100702     d  CNI_Identificativo_Bolla...
020300100702     d                 s             35A
020400090209      *---------------------------------------------------------------------*
020500090209      * Schiere
020600090209      *---------------------------------------------------------------------*
020700090209      * Nr. documento alfanumerico da decodificare
020800090209     D NDA             S              1    DIM(35)
020900100630      *
021000090209      * Nr. documento alfanumerico da già decodificato
021100090209     D NDN             S              1    DIM(15)
021200060608      **?------------------------------------------------------------------ */
021300090209      *  Invio E-mail
021400090209     D Indirizzi       s            253
021500090209     D Oggetto         s             44
021600090209     D Messaggio       s           5000
021700090209      *
021800090209     d   DSmail        ds
021900090209     D Mail_Ind                      44
022000090209     d   IND_char                     1    dim(44)
022100090209      *
022200100630     D Lunghezza_Indirizzo...
022300100630     D                 s              5u 0
022400090209      *
022500100630     D dalCarattere    s              5u 0
022600100630     D xTOTcaratteri   s              5u 0
022700090209      *
022800090209     C*---------------------------------------------------------------*
022900090209     D MsgDSP          S            256                                         Testo generico
023000090209     C*---------------------------------------------------------------*
023100090209     D SndMg01         S             10                                         Message type
023200090209     D                                     INZ('*INFO')
023300090209     D SndMg02         S             10                                         Deluvery mode
023400090209     D                                     INZ('*BREAK')
023500090209     D SndMg03         S            256                                         Message text
023600090209     D SndMg04         S             10I 0                                      Length of text
023700090209     D                                     INZ(%SIZE(SndMg03))
023800090209     D SndMg05         S             10                                         User profile
023900090209     D                                     DIM(299)
024000090209     D SndMg06         S             10I 0                                      Number of user
024100090209     D SndMg07         S             10I 0                                      Message sent indic.
024200090209     D SndMg08         S             10I 0                                      Function requested
024300090209     D SndMg10         S              1                                         Show display
024400090209     D                                     INZ('N')
024500090209     D SndMg11         S             20                                         Qualified MSGQ name
024600090209     D SndMg12         S              4                                         Name type indicator
024700090209     D                                     INZ('*USR')
024800090209      * indice di scihera
024900090209     D Jx              S              5I 0
025000090209      *
025100090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
025200090209      *
025300130313     d bart_it         C                   CONST('@Brt.it;')
025400130313     d CED_Bart        C                   CONST('CEDALERT@Brt.it;')
025500060612      * ?================================================================== */
025600060612      * ?   * Campi x decodifica * (INPUT  del Record)
025700100319      * ?================================================================== */
025800090130     D  Dati           s           2048
025900100722      *
026000060612     D  se_errore      s              1    inz(' ')
026100060612      * ?* ------------------------------------------------------ *
026200060710     D Digits          C                   '0123456789'
026300091019     D*------------------
026400091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
026500091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
026600060612      * ?================================================================== */
026700060612      *   Ciclo principale
026800090205      * ?================================================================== */
026900060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
027000060612     C*
027100060612     C                   if        not %open(tivin00r)
027200060612     C                   open      tivin00r
027300060612     C                   endif
027400060612      **
027500060612      * ?------------------------------------------------------------------ */
027600100712     C*  Controllo dati arrivati
027700060612      * ?------------------------------------------------------------------ */
027800060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
027900060612      * ?- controllando che la trasmissione sia completa.
028000090205      **
028100060612      * ?              /*---------------------- */
028200090205     c                   goto      nonFare_adesso
028300060612     c                   exsr      check_Trasm
028400090205     c     nonFare_adessotag
028500060612      * ?              /*---------------------- */
028600090205      **
028700060613      **  Errore di trasmissione x tutti i records
028800060613      **   --> file in errore
028900090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
029000100712     c                   if        se_errore = 'S'
029100090205     c                   exsr      Msg_errato
029200060612     c                   else
029300060614      **
029400060612      * ?------------------------------------------------------------------ */
029500100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
029600060612      * ?------------------------------------------------------------------ */
029700100722      *
029800060612      * ?              /*---------------------- */
029900060612     c                   exsr      Importa_Msg
030000060612      * ?              /*---------------------- */
030100060614     c                   end
030200060612      *
030300100723     C                   if          eseguire_ROLBACK = 'S'
030400130313     c                   ROLBK
030500100723     C*
030600100723     C                   eval      esito = '2'
030700100723???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
030800100723     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
030900100723     C                             STATI IFTSTA - messaggio -
031000100723     C                             EDI ricevuto errato su UPLOAD.'
031100100723     c                   if        §PVinv <> 'N'
031200100723     c                   exsr      invio_mail
031300100723     c                   end
031400100723     C
031500100723      *
031600100723     c                   else
031700060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
031800090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
031900090225     c                             esito ='0'
032000060710     C                   eval      esito ='0'
032100090225      *
032200130313     c                   COMMIT
032300090224      *
032400090213     c                   if        almeno_un_due ='S'
032500090213     C                   eval      esito ='1'
032600060614     C                   endif
032700090213     C                   endif
032800100723     C                   end
032900100708      * chiude TIVIN
033000100708     C                   if        %open(tivin00r)
033100100708     C                   close     tivin00r
033200100708     C                   endif
033300060614      *
033400060612     c                   SETON                                        LR
033500060612      * ?================================================================== */
033600100708      *? Importa i records della trasmissione
033700060612      * ?------------------------------------------------------------------ */
033800060612     c     Importa_Msg   Begsr
033900060614
034000100726     c                   clear                   Forza_Uscita
034100060612     c     *start        setll     Tivin00r
034200100708
034300100708     c                   EXSR      LEGGE_TIVIN_R
034400100708     c                   dow       EoFTivin <> 'S'
034500060614
034600060614      * solo i record sflaggati da rielaborare
034700110107     c                   IF        vinFLG = *blank
034800110107      *** ++++++
034900090211      *  Sempre Record OK
035000090211      ** Controlli formali sui campi
035100090211     C                   eval      vinFLG = '1'
035200060612     c                   clear                   se_errore
035300110107      *** >>>>>>
035400110107     c                   IF        vinDTA <> *blank
035500110107      *** >>>>>>
035600100723      **
035700100723      **  Ad ogni inizio Dettaglio incrementa il contatore dei dettagli
035800100723     c                   If           tipo_seg = Segm_Inizio_Dettaglio
035900100723     c                   eval        Contatore_Dettagli = Contatore_Dettagli + 1
036000100723      **
036100100723      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
036200100723     c                   eval      FinDET_RECord = vnrec
036300100723      **
036400100723     c*  Scrivere la Testata
036500100723      * SOLO ALLA SCRITTURA del 1° dettaglio
036600100723     c                   If           Contatore_Dettagli = 1
036700100723     C                   EXSR      TESTAMSG
036800100723     c                   end
036900100723     c                   endIF
037000060612
037100060612      ** Decodifica record a campi variabili
037200060612      * ?              /*---------------------- */
037300090205     c                   exsr      Decod_Segmento
037400090209      * ?              /*---------------------- */
037500060612
037600060621      *  x errore bloccante nella composizione del VAB/VAT
037700060621     c                   if        err_bloccante ='S'
037800060621     C                   eval      vinFLG = '2'
037900090211     c                   eval      esito  = '2'
038000100712???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
038100090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
038200100712     C                             STATI su Bolle export - messaggio -
038300090603     C                             con UNB non presente in tabella PT controlla-
038400090603     C                             re EDI ricevuto su UPLOAD.'
038500100319     c                   if        §PVinv <> 'N'
038600100319     c                   exsr      invio_mail
038700060621     c                   end
038800100319     c                   end
038900060612
039000110107      *** >>>>>>
039100110107     c                   endIF
039200110107      *** >>>>>>
039300110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
039400110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
039500110107      *         un erroneo messaggio
039600110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
039700060612     c                   update    Tivin000
039800090211
039900090211      *** se non è riconosciuto l'UNB deve uscire direttamente
040000090211     c                   if        se_errore ='S'  and
040100100708     c                             tipo_seg = Segm_Inizio_Messaggio
040200090213     c                   eval      err_bloccante = 'S'
040300090211     c                   exsr      Msg_errato
040400090211     c                   LEAVE
040500090211     c                   endIF
040600110107      ***
040700110107      *** ++++++
040800110107     c                   endIF
040900060612
041000100708     c                   EXSR      LEGGE_TIVIN_R
041100060612     C                   ENDdo
041200060612      **
041300060612     c                   endSR
041400100709      * ?------------------------------------------------------------------ */
041500100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
041600100709      * ?------------------------------------------------------------------ */
041700100709     C     LEGGE_TIVIN_R BEGSR
041800100709      *
041900100709     c                   clear                   EoFTivin
042000100709      *  Lettura sequenziale
042100100709     c                   read      Tivin00r
042200100709
042300100709     c                   if        %Eof(tivin00r)
042400100709     c                   move      'S'           EoFTivin
042500100709     c                   else
042600100709      *
042700100709      * ?  imposta il tipo di segmento:
042800100709     c                   clear                   dati
042900100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
043000100728     c                   eval      segmento = %trimL(dati)
043100100728     c                   eval      tipo_seg = %subst(segmento:1:3)
043200100709      **
043300100709     c                   end
043400100709      *
043500100709     C                   ENDSR
043600100120      * ?------------------------------------------------------------------ */
043700100120      *?    Decodifica il segmento ed importa i campi in formato DS
043800100120      * ?------------------------------------------------------------------ */
043900100120     c     Decod_SegmentoBegsr
044000100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
044100100708      **?              /*---------------------- */
044200100722     c                   exsr      Pulizia_campi
044300100708      **?              /*---------------------- */
044400100722      *
044500100706      *** ------------------------------------
044600100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
044700100120      * ?  Anche se non utilizzati.
044800100712      *** ------------------------------------
044900100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
045000100317     c                   clear                   seg_imprevisto
045100100721     c                   clear                   trovato_seg
045200100712      *** ------------------------------------
045300100319      *
045400100319      *  Contatore delle righe ossia dei segmenti del messaggio
045500100319     c                   add       1             Conta_segmenti
045600100708      *
045700100716     C                   clear                   keyUNBCLI
045800100716     C                   clear                   keyTIPOMSG
045900100716     C                   clear                   keyVERSION
046000100716     C                   clear                   keyRELEASE
046100100716     C                   clear                   keyAGENCY
046200100716     C                   clear                   keyASSOCIA
046300100716      *
046400100721      **--------
046500100721      *  Ritorna la DS GENERICA oppure l'errore
046600100721      **--------
046700100721      *  Chiamata generica x decodificare il singolo segmento letto
046800100721     c                   call      'TRTCT00R1'
046900100721      * input
047000100721     c                   parm                    tipo_seg
047100100721     c                   parm                    segmento
047200100721     C                   parm                    keyUNBCLI
047300100721     C                   parm                    keyTIPOMSG
047400100721     C                   parm                    keyVERSION
047500100721     C                   parm                    keyRELEASE
047600100721     C                   parm                    keyAGENCY
047700100721     C                   parm                    keyASSOCIA
047800100721      *output
047900100721     c                   parm                    Errore
048000100721     c                   parm                    DsGenerica
048100100721     c                   parm                    Valori_inErr
048200100721     c                   parm                    Descr_Errore
048300100721     c                   parm                    trovato_seg
048400100721      *
048500100708      *** ------------------------------------
048600100708      *   Decodifica il singolo segmento LETTO
048700100708      *** ------------------------------------
048800100120     c                   select
048900100120      *
049000110110     c                   when      trovato_seg ='N' and unz_segmento <>'S'
049100100723     C                   eval        eseguire_ROLBACK = 'S'
049200100721     c                   move      'S'           seg_imprevisto
049300100721     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
049400100721     c                   exsr      Error_DET
049500100721     c                   goto      end_Decod
049600100721      **--------
049700100723     c                   when      errore <> ' '
049800100723     C                   eval        eseguire_ROLBACK = 'S'
049900100723     C                   eval      vinMSG = tipo_seg +Valori_inErr +Descr_Errore
050000100723     c                   exsr      Error_DET
050100100723     c                   goto      end_Decod
050200100723      **--------
050300100120     c                   when      tipo_seg = 'UNA'
050400100721     c                   movel(p)  DsGenerica    EDS00UNA
050500100712      * --------
050600100120     c                   when      tipo_seg = 'UNB'
050700100721     c                   movel(p)  DsGenerica    EDS00UNB
050800100120     C                   EXSR      DATI_UNB
050900100120      * --------
051000100120     c                   when      tipo_seg = 'UNH'
051100100721     c                   movel(p)  DsGenerica    EDS00UNH
051200100120     C                   EXSR      DATI_UNH
051300100120      * --------
051400100120     c                   when      tipo_seg = 'BGM'
051500100721     c                   movel(p)  DsGenerica    EDS00BGM
051600100316     C                   EXSR      DATI_BGM
051700100120      **--------
051800100120     c                   when      tipo_seg = 'DTM'
051900100721     c                   movel(p)  DsGenerica    EDS00DTM
052000100120     C                   EXSR      DATI_DTM
052100100120      * --------
052200100712     c                   when      tipo_seg = 'STS'
052300100721     c                   movel(p)  DsGenerica    EDS00STS
052400100712     C                   EXSR      DATI_STS
052500100120      **--------
052600100120     c                   when      tipo_seg = 'RFF'
052700100721     c                   movel(p)  DsGenerica    EDS00RFF
052800100120     C                   EXSR      DATI_RFF
052900100316      **--------
053000100316     c                   when      tipo_seg = 'LOC'
053100100721     c                   movel(p)  DsGenerica    EDS00LOC
053200100316     C                   EXSR      DATI_LOC
053300100120      **--------
053400100120     c                   when      tipo_seg = 'FTX'
053500100721     c                   movel(p)  DsGenerica    EDS00FTX
053600100120     C                   EXSR      DATI_FTX
053700100120      **--------
053800100120     c                   when      tipo_seg = 'NAD'
053900100721     c                   movel(p)  DsGenerica    EDS00NAD
054000100120     C                   EXSR      DATI_NAD
054100100316      **--------
054200100316     c                   when      tipo_seg = 'CNI'
054300100721     c                   movel(p)  DsGenerica    EDS00CNI
054400100316     C                   EXSR      DATI_CNI
054500100120      **--------
054600100120     c                   when      tipo_seg = 'GID'
054700100721     c                   movel(p)  DsGenerica    EDS00GID
054800100120     C                   EXSR      DATI_GID
054900100120      **--------
055000100120     c                   when      tipo_seg = 'PCI'
055100100721     c                   movel(p)  DsGenerica    EDS00PCI
055200100120     C                   EXSR      DATI_PCI
055300100120      **--------
055400100120     c                   when      tipo_seg = 'UNT'
055500100721     c                   movel(p)  DsGenerica    EDS00UNT
055600100120     C                   EXSR      DATI_UNT
055700100120      **--------
055800100120     c                   when      tipo_seg = 'UNZ'
055900100721     c                   movel(p)  DsGenerica    EDS00UNZ
056000100120     C                   EXSR      DATI_UNZ
056100110110     c                   move      'S'           unz_segmento
056200110110      **--------
056300110110     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
056400100120      **--------
056500100120     c                   OTHER
056600100120      **--------
056700110110     c                   if           unz_segmento <>'S'
056800100723     C                   eval        eseguire_ROLBACK = 'S'
056900100120     c                   move      'S'           seg_imprevisto
057000100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
057100100120     c                   exsr      Error_DET
057200100120     c                   goto      end_Decod
057300110110     c                   end
057400100120      **--------
057500100120     c                   endSL
057600100120      **
057700100120     c     end_Decod     endSR
057800100701      * ?================================================================== */
057900100319     C*? Azzera_MSG   - Azzera dati messaggio
058000100701      * ?================================================================== */
058100100708     C     Scrive_Dettag BEGSR
058200100701      **
058300100708      *  Deve flaggare il dettaglio con dei problemi
058400100712      * Imposta le righe in errore solo sullo specifico dettaglio
058500100708     C                   if        Err_in_detta = 'S'
058600100708      *  rilascia il record
058700100708     c                   update    tivin000
058800100708      * ?  Scrive su tutti i records il tipo di errore
058900100708     c     IniDET_RECord setll     tivin00r
059000100708      **?              /*---------------------- */
059100100708     c                   exsr      DETta_Errato
059200100708      **?              /*---------------------- */
059300100708     c                   end
059400100708      *
059500100708      **  Se presente un errore nel record emette una segnalazione msg
059600100712      *     altrimenti scrive il FLAT
059700100708???? c                   if        se_errore <> 'S'
059800100708      * ?              /*---------------------- */
059900100712     c                   exsr      Scrive_Flat
060000100708      * ?              /*---------------------- */
060100100708     c                   else
060200100708      *
060300100712      *  Problemi nella decodifica dei campi
060400100708     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
060500100712     c                   eval      errori_in_Wrt = 'S'
060600100708     c                   exsr      Error_DET
060700100708     c                   end
060800100708      *
060900100708     c     end_WRidett   endSR
061000100722      * ?================================================================== */
061100100722     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
061200100722      * ?================================================================== */
061300100722     C     Scrive_FLAT   BEGSR
061400100722      *
061500100722      *  dopo il CNI
061600100722     c                   IF           tipo_seg = Segm_Inizio_Dettaglio  and
061700100722     c                                Contatore_Dettagli > 0
061800100722     C                   CLEAR                   EDSD00
061900100722     C                   MOVEL     CNI1490       DA1490
062000100722     C                   MOVEL     CNI1004       DA1004
062100100722     C                   MOVEL     CNI1312       DA1312
062200101018     C                   CLEAR                   TRTCT94R
062300100722     C                   clear                   WDAT
062400100722     C                   MOVEL     EDSD00        WDAT
062500100722     C                   MOVEL     'SD00'        STATPR
062600100723     C                   MOVEL     WDAT          FLATREC
062700101018     C                   WRITE     TRTCT94R                             99
062800100722     c                   end
062900100722      * STS/RFF/DTM
063000100722     c                   IF           tipo_seg = Segm_Data_Dettaglio  and
063100100722     c                                Contatore_Dettagli > 0
063200100722     C                   CLEAR                   EDSD05
063300100722     C                   MOVEL     STS9011       DB9011
063400100722     C                   MOVEL     STS113120     DB1131
063500100722     C                   MOVEL     STS901330     DB9013
063600100722     C                   MOVEL     STS113130     DBA131
063700100722     C                   MOVEL     RFF1153       DB1153
063800100722     C                   MOVEL     RFF1154       DB1154
063900100722     C                   MOVEL     DTM2005       DB2005
064000100722     C                   MOVEL     DTM2380       DB2380
064100100722     C                   MOVEL     DTM2379       DB2379
064200101018     C                   CLEAR                   TRTCT94R
064300100722     C                   clear                   WDAT
064400100722     C                   MOVEL     EDSD05        WDAT
064500100722     C                   MOVEL     'SD05'        STATPR
064600100723     C                   MOVEL     WDAT          FLATREC
064700101018     C                   WRITE     TRTCT94R                             99
064800100722     c                   end
064900100722      *  NAD
065000100722     c                   IF           tipo_seg = Segm_Address_Dettaglio  and
065100100722     c                                Contatore_Dettagli > 0
065200100722     C                   CLEAR                   EDSD10
065300100723     C                   MOVEL     NAD3035       DC3035
065400100722     C                   MOVEL     NAD30361      DC3036
065500101018     C                   CLEAR                   TRTCT94R
065600100723     C                   clear                   wdat
065700100722     C                   MOVEL     EDSD10        WDAT
065800100722     C                   MOVEL     'SD10'        STATPR
065900100723     C                   MOVEL     WDAT          FLATREC
066000101018     C                   WRITE     TRTCT94R                             99
066100100722     c                   end
066200100722      *  FTX
066300100722     c                   IF           tipo_seg = Segm_Text_Dettaglio  and
066400100722     c                                Contatore_Dettagli > 0
066500100722     C                   CLEAR                   EDSD15
066600100722     C                   MOVEL     FTX4451       DD4451
066700100722     C                   MOVEL     FTX44401      DD4440
066800100722     C                   MOVEL     FTX44402      DDA440
066900100722     C                   MOVEL     FTX44403      DDB440
067000100722     C                   MOVEL     FTX44404      DDC440
067100100722     C                   MOVEL     FTX44405      DDD440
067200101018     C                   CLEAR                   TRTCT94R
067300100722     C                   clear                   WDAT
067400100722     C                   MOVEL     EDSD15        WDAT
067500100722     C                   MOVEL     'SD15'        STATPR
067600100723     C                   MOVEL     WDAT          FLATREC
067700101018     C                   WRITE     TRTCT94R                             99
067800100722     c                   end
067900100722      *  PCI
068000100722     c                   IF           tipo_seg = Segm_Parcels_Dettaglio  and
068100100722     c                                Contatore_Dettagli > 0
068200100722     C                   CLEAR                   EDSD20
068300100722      *
068400100722     C                   MOVEL     GID1496       DE1496
068500100722     C                   MOVEL     GID722420     DE7224
068600100722     C                   MOVEL     GID706520     DE7065
068700100722      *
068800100722     C                   MOVEL     PCI4233       DE4233
068900100722     C                   MOVEL     PCI71021      DE7102
069000100722     C                   MOVEL     PCI71022      DEA102
069100100722     C                   MOVEL     PCI71023      DEB102
069200100722     C                   MOVEL     PCI71024      DEC102
069300100722     C                   MOVEL     PCI71025      DED102
069400100722     C                   MOVEL     PCI71026      DEE102
069500100722     C                   MOVEL     PCI71027      DEF102
069600100722     C                   MOVEL     PCI71028      DEG102
069700100722     C                   MOVEL     PCI71029      DEH102
069800100722     C                   MOVEL     PCI710210     DEI102
069900100722      *
070000101018     C                   CLEAR                   TRTCT94R
070100100722     C                   clear                   WDAT
070200100722     C                   MOVEL     EDSD20        WDAT
070300100722     C                   MOVEL     'SD20'        STATPR
070400100723     C                   MOVEL     WDAT          FLATREC
070500101018     C                   WRITE     TRTCT94R                             99
070600100722      *
070700100722     C                   clear                   GID1496
070800100722     C                   clear                   GID722420
070900100722     C                   clear                   GID706520
071000100722     c                   end
071100100722      *
071200100722     c                   clear                   err_bloccante     1
071300100722     c                   move      'S'           Almeno_Uno
071400100722     c   99              eval      errori_in_Wrt = 'S'
071500100722     C* >>>>>>>>>>>>
071600100722     C                   if        se_errore  = 'S'
071700100722     c                   move      'S'           err_bloccante
071800100722     c                   goto      Error_Flat
071900100722     c                   end
072000100722     C*
072100100722     C     Error_Flat    Endsr
072200100708      * ?================================================================== */
072300100722     C*? Pulizia campi -
072400100708      * ?================================================================== */
072500100722     C     Pulizia_campi BEGSR
072600100708      **
072700100712      **
072800100712      * inzio messaggio azzera tutte le variabili generali del messaggio
072900100712      * ?                         --------------
073000100712     c                   If        tipo_seg = Segm_Inizio_Messaggio
073100100712      * ?                         --------------
073200100723     C                   eval        eseguire_ROLBACK = ' '
073300100712      *  INIZIALIZZA  dati fondamentali messaggio
073400100712     C                   clear                   EDIDSPT
073500100712     C                   clear                   EDIDSPU
073600100712     C                   clear                   EDIDSPV
073700100722     C                   clear                   EDIDSMS
073800100712     C                   move      'N'           PT_trovata
073900100712     c                   clear                   UNB_Generico
074000100712     c                   clear                   UNB_Parz
074100100712     c                   clear                   UNB_NrTrasmiss
074200100712      * per controllare se almeno un record è stato importato sul VAB
074300100712     c                   clear                   Almeno_Uno        1
074400100712     c                   clear                   Almeno_Un_Due     1
074500100712     c                   clear                   IniDET_RECord
074600100712     c                   clear                   FinDET_RECord
074700100712     c                   eval      esito  = '0'
074800100722     c                   clear                   EDST00
074900100722     c                   clear                   EDST05
075000100722     c                   clear                   EDST10
075100100722     c                   clear                   EDST15
075200100722     C                   clear                   nad3035SF
075300100722     C                   clear                   nad3039SF
075400100722     C                   clear                   nad3035ST
075500100722     C                   clear                   nad3039ST
075600100722     C                   clear                   LOC3227t1
075700100722     C                   clear                   LOC3225t1
075800100722     C                   clear                   LOC3227t2
075900100722     C                   clear                   LOC3225t2
076000100722     C                   clear                   dtm2005te
076100100722     C                   clear                   dtm2380te
076200100722     C                   clear                   dtm2379te
076300100722     C                   clear                   rff1153te
076400100722     C                   clear                   rff1154te
076500100712      *
076600100712      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
076700100712      * ?                         --------------
076800100712     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
076900100712      * ?                         --------------
077000100712      *   Contatore dei dettagli
077100100712     c                   eval      Contatore_Dettagli = 0
077200100712      **
077300100712      *   Inizia il conteggio delle righe
077400100712     c                   z-add     0             Conta_segmenti    5 0
077500100712      *
077600100712      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
077700100712      *  Prepara una nuova spedizione
077800100712      * ?                         --------------
077900100712     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
078000100712      * ?                         --------------
078100100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
078200100708      *  Prepara una nuova spedizione
078300100712     c                   clear                   EDSD00
078400100712     c                   clear                   EDSD05
078500100712     c                   clear                   EDSD10
078600100712     c                   clear                   EDSD15
078700100712     c                   clear                   EDSD20
078800100712      **
078900100712     c                   z-add     vnrec         IniDET_RECord
079000100722      **
079100100712      * Un errore nel dettaglio
079200100712     C                   clear                   Err_in_detta      1
079300100712     c                   clear                   errori_in_Wrt     1
079400100712      *
079500100723     c                   endIF
079600100708      **
079700100708     c     end_clear     endSR
079800100701      * ?================================================================== */
079900090210      *? DECODIFICA DATI UNB
080000100712      * ?================================================================== */
080100090210     C     DATI_UNB      BEGSR
080200090210      *
080300100701      * identificativo Messaggio
080400100701     C                   eval      UNB_NrTrasmiss = unb0020
080500100701      *
080600100712      * quindi Controlla esistenza UNB ricevuto del mittente se presente
080700100712      *  anche se parziale.
080800100630     C                   MOVEL     unb0004       UNB_diWork
080900100630     C                   MOVE      unb000720     UNB_diWork
081000100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
081100100712     c                   clear                   UNB_parziale
081200100630     C                   Z-ADD     1             Nr_PT
081300100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
081400100712      * Poi occorre verificare se il codice UNB è fra quelli parzializzati
081500100317     C                   If        *IN32 = *off
081600100630     C                   eval      Nr_PT = 1
081700100630     c                   clear                   UNB_diWork
081800100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
081900100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
082000100712     c   32              eval      UNB_parziale = 'S'
082100100712     c   32              eval      UNB_Generico = UNB_diWork
082200100712     c                   end
082300100317      *
082400100712     C                   If        *IN32 = *off
082500100319      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
082600130313      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Brt.it
082700090209      * Msg di allerta Traduzione
082800090209     C                   EVAL      Indirizzi = CED_Bart
082900130313???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
083000090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
083100090209     C                             Bolle import in filiale - messaggio -
083200090209     C                             con UNB non presente in tabella PT controlla-
083300090209     C                             re EDI ricevuto su UPLOAD.'
083400090209      *
083500090209     c                   exsr      invio_mail
083600090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
083700090210     C                   eval      vinFLG = '2'
083800090210     C                   eval      se_errore   = 'S'
083900090210     c                   goto      end_UNB
084000090209      *
084100100630     C                   move      'N'           PT_trovata
084200090209      *
084300090209      *             *------------------------------*
084400090209     C                   eLSe
084500090209      *             *------------------------------*
084600100712      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
084700100712     c                   movel     UNB_diWork    UNB_Parz
084800100712      **
084900090209      * Se tutto OK su tab.PT:
085000100630     C                   move      'S'           PT_trovata
085100100630     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
085200100712     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
085300100712     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
085400091016     c                   clear                   Mittente         35
085500091016     c                   clear                   Mitt_Qualifier    4
085600091016     c                   clear                   Id_Messaggio     14
085700090226     C                   eval      Mittente       = unb0004
085800090226     C                   eval      Mitt_qualifier = unb000720
085900090226     C                   eval      Id_Messaggio   = UNB0022
086000090209      *
086100090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
086200090209      *  ricevuti:
086300090209     c                   movel     §PVema        mail_ind         44
086400090209      *
086500090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
086600090209     c                   if        mail_ind <> *blank
086700090209     c                   exsr      imp_ADDR_mail
086800090209     c                   end
086900100712      *
087000090209     c                   Endif
087100090209      **
087200090210     c     end_UNB       endSR
087300090227      * ?================================================================== */
087400090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
087500090227      * ?================================================================== */
087600090227     C     DATI_UNH      BEGSR
087700100316      *
087800100701      *
087900090210     c     end_UNH       endSR
088000100701      * ?================================================================== */
088100100701      *? dati iniziali   BGM
088200100701      * ?================================================================== */
088300100701     C     DATI_BGM      BEGSR
088400100701      *
088500100712      *
088600100701     c                   endSR
088700100705      * ?================================================================== */
088800100705     C*? Imposta la Località
088900100705      * ?================================================================== */
089000100705     C     DATI_LOC      BEGSR
089100100705     C*
089200100722     c                   If        LOC3227t1 = *blank
089300100722      *
089400100722     c                   eval      LOC3227t1 = LOC3227
089500100722     c                   eval      LOC3225t1 = LOC3225
089600100722      *
089700100722     c                   else
089800100722      *
089900100722     c                   eval      LOC3227t2 = LOC3227
090000100722     c                   eval      LOC3225t2 = LOC3225
090100100722      *
090200100722     c                   end
090300100705      **
090400100705     c     end_LOC       endSR
090500100316      * ?================================================================== */
090600100316     C*? Imposta l'inizio del Dettaglio CNI
090700100316      * ?================================================================== */
090800100316     C     DATI_CNI      BEGSR
090900100316     C*
091000100702      ** Inizio Dettaglio riferimento iniziale
091100100702     c                   If        CNI1004 <> *blank
091200100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
091300100702     c                   end
091400100316      **
091500100722      **?              /*========================= */
091600100722     c                   exsr      Scrive_Dettag
091700100722      **?              /*========================= */
091800100722      **
091900100316     c     end_CNI       endSR
092000100722      * ?================================================================== */
092100100722     C*? Imposta informazioni sullo STATO di Consegna
092200100722      * ?================================================================== */
092300100722     C     DATI_STS      BEGSR
092400100722     C*
092500100722      **
092600100722     c     end_STS       endSR
092700100722      * ?================================================================== */
092800100722     C*?  il Riferimento della spedizione
092900100722      * ?================================================================== */
093000100722     C     DATI_RFF      BEGSR
093100100722      **
093200100722      *
093300100722     c     end_RFF       endSR
093400100722      * ?================================================================== */
093500100722     C*? Imposta la data dell'evento
093600100722      * ?================================================================== */
093700100722     C     DATI_DTM      BEGSR
093800100722      *
093900100723     c                   If           Contatore_Dettagli > 0
094000100722      **?              /*========================= */
094100100722     c                   exsr      Scrive_Dettag
094200100722      **?              /*========================= */
094300100723     c                   end
094400100722      *
094500100722     c     end_DTM       endSR
094600100705      * ?================================================================== */
094700100705     C*?  FREE TEXT
094800100705      * ?================================================================== */
094900100705     C     DATI_FTX      BEGSR
095000100705      *
095100100723     c                   If           Contatore_Dettagli > 0
095200100722      **?              /*========================= */
095300100722     c                   exsr      Scrive_Dettag
095400100722      **?              /*========================= */
095500100723     c                   end
095600100721      *
095700100705     c     end_FTX       endSR
095800090213      * ?================================================================== */
095900090213     C*?  Anagrafica del mittente e del destinatario
096000090213      * ?================================================================== */
096100091016     C     DATI_NAD      BEGSR
096200100705      *
096300100722     c                   if        NAD3035 = 'SF '
096400100722      *
096500100722     c                   eval      nad3035SF = NAD3035
096600100722     c                   eval      nad3039SF = NAD3039
096700100722      *
096800100722     c                   elseIf    NAD3035 = 'ST '
096900100722      *
097000100722     c                   eval      nad3035ST = NAD3035
097100100722     c                   eval      nad3039ST = NAD3039
097200100722      *
097300100722     c                   end
097400100722      **
097500100723     c                   If           Contatore_Dettagli > 0
097600100722      **?              /*========================= */
097700100722     c                   exsr      Scrive_Dettag
097800100722      **?              /*========================= */
097900100723     c                   end
098000090210      *
098100090210     c     end_NAD       endSR
098200090210      * ?================================================================== */
098300090210     C*?  Dati di dettaglio della spedizione e dei colli
098400090210      * ?================================================================== */
098500090210     C     DATI_GID      BEGSR
098600100706      *
098700090210      *
098800090210     c     end_GID       endSR
098900090210      * ?================================================================== */
099000090210     C*?  Dati di dettaglio Codice a barre Segnacolli
099100090210      * ?================================================================== */
099200090210     C     DATI_PCI      BEGSR
099300090210      *
099400100723     c                   If           Contatore_Dettagli > 0
099500100722      **?              /*========================= */
099600100722     c                   exsr      Scrive_Dettag
099700100722      **?              /*========================= */
099800100723     c                   end
099900090210      *
100000090210     c     end_PCI       endSR
100100090211     C*----------------------------------------------------------------
100200090211      *? dati finali del dettaglio    UNT
100300090211     C*----------------------------------------------------------------
100400090211     C     DATI_UNT      BEGSR
100500110324      *
100600110324      * Non deve dare messaggio
100700110324     c                   goto      No_UNTctrl
100800090211      *
100900100708     c                   eval      UNT_record = vnREC
101000090213     c                   z-add     vnrec         last_RECord
101100090211      **
101200100319      **   verifica la quadratura di tutti i segmenti ricevuti
101300100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
101400100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
101500100319     C     UNT0074       IFne      Conta_segmenti
101600100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
101700100319     c                             trollare il Messaggio ricevuto'
101800100319      *
101900130313???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFTSTA :'
102000100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
102100100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
102200100319     C                             Bolle import in filiale - messaggio -
102300100319     C                             con quadratura UNT errata controllare EDI -
102400100319     C                             ricevuto su UPLOAD x il cliente :'
102500100319     C                   EVAL      Messaggio = %trim(Messaggio) +
102600100319     C                                         %editc(§PTksc:'X')
102700100319     C                   EVAL      Messaggio = %trim(Messaggio) +
102800100319     C                             ' i segmenti contati sono :' +
102900100319     C                             %editc(Conta_segmenti:'Z')
103000100319     c                   if        §PVinv <> 'N'
103100100319     c                   exsr      invio_mail
103200100319     c                   end
103300100319     c                   end
103400110324     c     No_UNTctrl    tag
103500100319      **
103600100319     c     End_UNT       endSR
103700100316     C*----------------------------------------------------------------
103800100316      *? dati finali     UNT
103900100316     C*----------------------------------------------------------------
104000100316     C     DATI_UNZ      BEGSR
104100100316      *
104200100726     c                   move      'S'           Forza_Uscita
104300100316      **
104400100316     c                   endSR
104500090210     C*----------------------------------------------------------------
104600051205      *   DEFINIZIONE CHIAVI                               *
104700100712     C*----------------------------------------------------------------
104800051205     C     *INZSR        BEGSR
104900051205      *
105000991129     c     *ENTRY        PLIST
105100060613     C                   parm                    esito             1
105200090209      *
105300090209     C* RECUPERO DATI DELL'UTENTE
105400090209     C                   Z-ADD     1             CODUT             1 0
105500090209     C                   CALL      'XPARUT'
105600090209     C                   PARM                    UTEDSE0F
105700090209     C                   MOVEL     RAGUT         RSUT             20
105800090209     C*
105900090209     C* RICERCA CAPOCONTI
106000090209     C                   DO        50            X
106100090209     C                   MOVE      TCU(X)        TCUDS
106200090209     C     F56           IFEQ      'CG'
106300090209     C     F34           ANDEQ     '01'
106400090209     C                   Z-ADD     KCU(X)        KCI               4 0
106500090209     C                   END
106600090209     C                   END
106700100722     C*
106800090211     c                   movel(p)  'EDPAB'       User_Msg         10
106900090211     C*
107000090209     C* IMPOSTO A ZERO VARIABILI DI PGM
107100090209     c                   move      *blank        Snd_Msg_alert     1
107200090209     C                   MOVEL     CA(1)         WCA              78
107300090209     C                   MOVEL     CN(1)         WCN              78
107400090209     C*
107500090209     C* RECUPERO DATA E ORA
107600100630     C                   TIME                    Ora_Data         14 0
107700100630     C                   MOVE      Ora_Data      G02DAT
107800100630     C                   MOVE      Ora_Data      WDATA8            8 0
107900100630     C                   MOVE      WDATA8        Anno2             2 0
108000090209     C                   MOVEL     WDATA8        DATA              6 0
108100100630     C                   MOVE      Anno2         DATA
108200090209     C                   MOVEL     *BLANK        G02ERR
108300090209     C                   CALL      'XSRDA8'
108400090209     C                   PARM                    WLBDA8
108500090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
108600100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
108700090209     C                   TIME                    WORA              6 0
108800090209      * Recupero data e ora
108900090209     C                   TIME                    W0140
109000090209      * UDATE IN GGMMAAAA
109100100630     C                   MOVE      W0140         DataGGMMAAA
109200090209      * UDATE IN AAAAMMGG
109300100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
109400090209     C     *iso          MOVEL     DATA_eur      dateu
109500090209      *
109600090209      * ?              /*--------------------------- */
109700090209     c                   exsr      CARICA_TABELLE
109800090209      * ?              /*--------------------------- */
109900090209      *
110000090209     C                   MOVEL     *ALL'-'       CMP132          132
110100991124      *
110200050414      *------------------
110300991124     C                   ENDSR
110400060621      * ?------------------------------------------------------------------ */
110500090209      *?    CARICA TABELLE SU SCHIERE
110600060621      * ?------------------------------------------------------------------ */
110700090209     C     CARICA_TABELLEBEGSR
110800090209      *
110900090209      * Caricamento Tabella Partner esteri
111000090209     C                   Z-ADD     0             X
111100090209     C                   MOVEL     'PT'          TABCOD
111200090209     C     TABCOD        CHAIN     EDTAB01L                           30
111300090209     C     *IN30         DOWEQ     '0'
111400090209      *
111500090209     C                   SELECT
111600090209     C     TABFLG        WHENNE    *BLANKS
111700090209      *
111800090209     C                   OTHER
111900090209     C                   ADD       1             X
112000100630     C                   MOVEL     TABKEY        PT_key(X)
112100100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
112200100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
112300100630     C                   MOVEL     TABUNI        PT_uni(X)
112400090209     C                   ENDSL
112500090209      *
112600090209     C     TABCOD        READE     EDTAB01L                               30
112700090209     C                   END
112800121105      *
112900121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
113000121105     c                   exsr      ChecK_PT
113100121105      *
113200090209      * salva quanti Codici UNB ha caricato
113300100630     c                   z-add     x             PT_KeyXsav
113400090209      *
113500090209     C                   MOVEL     'PU'          TABCOD
113600090209     C     TABCOD        CHAIN     EDTAB01L                           30
113700090209     C     *IN30         DOWEQ     '0'
113800090209      *
113900090209     C                   SELECT
114000090209     C     TABFLG        WHENNE    *BLANKS
114100090209      *
114200090209     C                   OTHER
114300100630     C                   MOVEL     TABKEY        PU_key
114400100723     C                   Z-ADD     1             X                 3 0
114500100630     C     PU_key        LOOKUP    PT_key(X)                              32
114600100630     C   32              MOVEL     TABUNI        PU_uni(X)
114700090209     C                   ENDSL
114800090209      *
114900090209     C     TABCOD        READE     EDTAB01L                               30
115000090209     C                   END
115100090209      *
115200090209      * Prosegue tab.PT e PU
115300090209     C                   MOVEL     'PV'          TABCOD
115400090209     C     TABCOD        CHAIN     EDTAB01L                           30
115500090209     C     *IN30         DOWEQ     '0'
115600090209      *
115700090209     C                   SELECT
115800090209     C     TABFLG        WHENNE    *BLANKS
115900090209      *
116000090209     C                   OTHER
116100100630     C                   MOVEL     TABKEY        PV_key
116200090209     C                   Z-ADD     1             X
116300100630     C     PV_key        LOOKUP    PT_key(X)                              32
116400100630     C   32              MOVEL     TABUNI        PV_uni(X)
116500090209     C                   ENDSL
116600090209      *
116700090209     C     TABCOD        READE     EDTAB01L                               30
116800090209     C                   END
116900100722      *
117000090209     C                   ENDSR
117100090205      * ?------------------------------------------------------------------ */
117200090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
117300090205      * ?------------------------------------------------------------------ */
117400090205     C     *pssr         BEGSR
117500090205     C
117600060710     C                   eval      esito = '2'
117700100712???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
117800090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
117900100712     C                             STATI IFTSTA - messaggio -
118000090603     C                             EDI ricevuto errato su UPLOAD.'
118100100319     c                   if        §PVinv <> 'N'
118200100319     c                   exsr      invio_mail
118300100319     c                   end
118400060621     C
118500090213     C                   ENDSR
118600090213      * ?------------------------------------------------------------------ */
118700090213      *?      Segnala Errore su TIVIN
118800090213      * ?------------------------------------------------------------------ */
118900090213     C     Error_DET     BEGSR
119000090213     C
119100090213     C                   eval      vinFLG = '2'
119200090213     c                   eval      Almeno_Un_Due ='S'
119300090213     c                   eval      esito  = '1'
119400090213     c                   eval      se_errore ='S'
119500090213     C                   eval      Err_in_detta = 'S'
119600090213     C
119700090213     C                   ENDSR
119800100712      *----------------------------------------------------------------
119900100712      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO
120000100712      *----------------------------------------------------------------
120100100712     C     TESTAMSG      BEGSR
120200100712      *
120300100722      * Eseguo scrittura dati record ST00   (UNB/BGM/DTM)
120400100712     C                   EXSR      WRST00
120500100722      * Eseguo scrittura dati record ST05   (NAD)
120600100712     C                   EXSR      WRST05
120700100722      * Eseguo scrittura dati record ST10   (RFF/LOC)
120800100712     C***                EXSR      WRST10
120900100722      * Eseguo scrittura dati record ST15   (FTX)
121000100712     C                   EXSR      WRST15
121100100712      *
121200100712     C                   ENDSR
121300100712      *----------------------------------------------------------------
121400100712      *? WRST00 - Scrittura record partner
121500100712      *----------------------------------------------------------------
121600100712     C     WRST00        BEGSR
121700100712      *
121800100712      *  Pulisco dati DS
121900100712     C                   CLEAR                   EDST00
122000100712      *
122100100712     C                   eval        TA0004 = UNB0004
122200100712     C                   eval        TA0007 = UNB000720
122300100712      *
122400100712     C                   eval        TA0010 = UNB0010
122500100712     C                   eval        TAA007 = UNB000730
122600100712      *
122700100712      *  Data/Ora invio messagio:
122800100712     C                   eval        TA0017 = UNB0017
122900100723     C                   move      UNB0019       TA0019
123000100712      *
123100100712      *  Codice documento - nome documento
123200100712     C                   eval        TA1001 = BGM1001
123300100712     C                   eval        TA1000 = BGM1000
123400100712     C                   eval        TA1004 = BGM1004
123500100712     C                   eval        TA1225 = BGM1225
123600100712      *  Data messaggio:
123700100712     C                   eval        TA2005 = DTM2005
123800100712     C                   eval        TA2380 = DTM2380
123900100712     C                   eval        TA2379 = DTM2379
124000100712      *
124100100712      *  Scivo record IFTSTA
124200101018     C                   CLEAR                   TRTCT94R
124300100712     C                   CLEAR                   WDAT
124400100712     C                   MOVEL     EDST00        WDAT
124500100712     C                   MOVEL     'ST00'        STATPR
124600100723     C                   MOVEL     WDAT          FLATREC
124700101018     C                   WRITE     TRTCT94R                             99
124800100712      *
124900100712     C                   ENDSR
125000100712      *----------------------------------------------------------------
125100100712      *? WRST05 - Scrittura record partner
125200100712      *----------------------------------------------------------------
125300100712     C     WRST05        BEGSR
125400100712      *
125500100712      *  Ship from:
125600100722     C                   CLEAR                   EDST05
125700100722     C                   MOVEL     nad3035SF     TE3035
125800100723     C                   MOVEL     nad3039SF     TE3036
125900101018     C                   CLEAR                   TRTCT94R
126000100722     C                   CLEAR                   WDAT
126100100712     C                   MOVEL     EDST05        WDAT
126200100712     C                   MOVEL     'ST05'        STATPR
126300100723     C                   MOVEL     WDAT          FLATREC
126400101018     C                   WRITE     TRTCT94R
126500100712      *  Ship to:
126600100712     C                   CLEAR                   EDST05
126700100722     C                   MOVEL     nad3035ST     TE3035
126800100723     C                   MOVEL     nad3039ST     TE3036
126900101018     C                   CLEAR                   TRTCT94R
127000100722     C                   CLEAR                   WDAT
127100100712     C                   MOVEL     EDST05        WDAT
127200100712     C                   MOVEL     'ST05'        STATPR
127300100723     C                   MOVEL     WDAT          FLATREC
127400101018     C                   WRITE     TRTCT94R                             99
127500100712      *
127600100712     C                   ENDSR
127700100712      *----------------------------------------------------------------
127800100712      *? WRST10 - Scrittura record partner
127900100712      *----------------------------------------------------------------
128000100712     C     WRST10        BEGSR
128100100712      *
128200100722      *  scrittura condizionata
128300100722     C                   if        rff1153 <> *blank or loc3227t1 <> *blank
128400100712      *  Pulisco dati DS
128500100712     C                   CLEAR                   EDST10
128600100722     C                   MOVEL     rff1153te     TB1153
128700100722     C                   MOVEL     rff1154te     TB1154
128800100722     C                   MOVEL     LOC3227t1     TB3227
128900100722     C                   MOVEL     LOC3225t1     TB3225
129000100722     C                   MOVEL     LOC3227t2     TBA227
129100100722     C                   MOVEL     LOC3225t2     TBA225
129200101018     C                   CLEAR                   TRTCT94R
129300100722     C                   clear                   WDAT
129400100712     C                   MOVEL     EDST10        WDAT
129500100712     C                   MOVEL     'ST10'        STATPR
129600100723     C                   MOVEL     WDAT          FLATREC
129700101018     C                   WRITE     TRTCT94R                             99
129800100722     c                   end
129900100712      *
130000100712     C                   ENDSR
130100100712      *----------------------------------------------------------------
130200100712      *? WRST15 - Scrittura record partner
130300100712      *----------------------------------------------------------------
130400100712     C     WRST15        BEGSR
130500100712      *
130600100722      *  scrittura condizionata
130700100722     C                   if         FTX44401 <> *blank
130800100712      *  Pulisco dati DS
130900100712     C                   CLEAR                   EDST15
131000100722     C                   MOVEL     ftx4451       TC4451
131100100722     C                   MOVEL     FTX44401      TC4440
131200100722     C                   MOVEL     FTX44402      TCA440
131300100722     C                   MOVEL     FTX44403      TCB440
131400100722     C                   MOVEL     FTX44404      TCC440
131500100722     C                   MOVEL     FTX44405      TCD440
131600101018     C                   CLEAR                   TRTCT94R
131700100722     C                   clear                   WDAT
131800100722     C                   MOVEL     EDST15        WDAT
131900100722     C                   MOVEL     'ST15'        STATPR
132000100723     C                   MOVEL     WDAT          FLATREC
132100101018     C                   WRITE     TRTCT94R                             99
132200100722     c                   end
132300100712      *
132400100712     C                   ENDSR
132500090211     c*==================================================================*
132600090211      * Manda un Msg in Posta AS Sede a EDP*
132700090211     c*==================================================================*
132800090211     c     SEND_MSG_EDP  begsr
132900090211      *
133000090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
133100090211      *   Lo manda una sola volta per lavoro
133200090211     c                   IF        Snd_Msg_alert  <> 'N' and
133300090211     c                             User_msg <> *blank
133400090211     c                   z-add     1             Jx
133500090211     C                   exsr      CalQEZSNDMG
133600090211     c                   end
133700090211      *
133800090211     c                   endsr
133900090213     c*==================================================================*
134000090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
134100090213     c*==================================================================*
134200090213     c     Imp_ADDR_mail begsr
134300090213      *
134400090213     c                   clear                   Indirizzi
134500090213      *
134600090213      *  Lunghezza indirizzi presenti
134700100630     c                   eval      Lunghezza_Indirizzo =
134800100630     c                                       %Len(%TrimR(Mail_Ind))
134900090213     c                   z-add     1             al_char           3 0
135000090213     c                   z-add     1             dal_char          3 0
135100090213     c                   z-add     1             tot_char          3 0
135200090213      *
135300090213     c     successivo    tag
135400090213      *
135500090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
135600090213      *   e aggiunge il dominio Bartolini + il (;) separatore
135700090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
135800090213     c                   eval      tot_char = al_char - dal_char
135900100630     c                   z-add     dal_char      dalCarattere
136000100630     c                   z-add     tot_char      xTOTcaratteri
136100090213      *
136200090213     c                   clear                   Indir_Bartol     40
136300090213     c                   clear                   Indir_Parz       20
136400100630     c                   eval      Indir_Parz =
136500100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
136600100630      *
136700090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
136800090213     c                             %Trim(bart_it)
136900090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
137000090213      *
137100100630     c                   if        al_char = Lunghezza_Indirizzo
137200090213     c                   goto      fine_indmail
137300090213     c                   else
137400090213     c                   eval      dal_char = ( al_char + 1 )
137500090213     c                   goto      successivo
137600090213     c                   end
137700090213      *
137800090213     C     fine_indmail  TAG
137900090213      *
138000090213     C                   ENDSR
138100090213     c*==================================================================*
138200090213      * Manda un Msg x E-mail
138300090213     c*==================================================================*
138400090213     c     Invio_Mail    begsr
138500090213      *
138600130313     C*   Alert_mail : invio Mail a CEDALERT@Brt.it                  *
138700090213     C* Inizializzo variabili
138800090213     C                   movel     *blanks       wrkEml          253
138900090213     C                   movel     *blanks       wrkMsg         5000
139000090213     C                   movel     *blanks       wrkOgg           44
139100090213      *
139200090213     C* Valorizzo i campi della e-m@ail - indirizzo
139300130313     C*******            eval      wrkEml = 'Andrea.Bertocchi@Brt.it'
139400090213     C                   eval      wrkEml = Indirizzi
139500090213     C                   eval      wrkOgg = Oggetto
139600090213     C                   eval      wrkMsg = Messaggio
139700110203      ********
139800110203     C********           call(e)   'TIS701C'
139900110203     C********           parm                    wrkEml
140000110203     C********           parm                    wrkOgg
140100110203     C********           parm                    wrkMsg
140200110203      ** *****
140300110203     C                   call(e)   'TRTCT00R2'
140400110203     C                   parm                    wrkEml
140500110203     C                   parm                    wrkOgg
140600110203     C                   parm                    wrkMsg
140700110203     C*
140800090213     C*
140900090213     C                   ENDSR
141000090603      *
141100090211     ***********************************************************************
141200090211     **
141300090211     ** Send Message (QEZSNDMG) API
141400090211     **
141500090211     ***********************************************************************
141600090211     C     CalQEZSNDMG   BEGSR
141700090211      *
141800090211     ** Invio messaggio all'utente.
141900090211     C                   EVAL      SndMg03 = msgDSP
142000090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
142100090211     C                   EVAL      SndMg05(Jx) = User_msg
142200090211     C                   EVAL      SndMg06 = Jx
142300090211     C                   CLEAR                   QUSEC
142400090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
142500090211
142600090211     C                   CALL      'QEZSNDMG'
142700090211     C                   PARM                    SndMg01
142800090211     C                   PARM                    SndMg02
142900090211     C                   PARM                    SndMg03
143000090211     C                   PARM                    SndMg04
143100090211     C                   PARM                    SndMg05
143200090211     C                   PARM                    SndMg06
143300090211     C                   PARM                    SndMg07
143400090211     C                   PARM                    SndMg08
143500090211     C                   PARM                    Qusec
143600090211     C                   PARM                    SndMg10
143700090211     C                   PARM                    SndMg11
143800090211     C                   PARM                    SndMg12
143900090211      *
144000090211     C                   ENDSR
144100100708      * ?------------------------------------------------------------------ */
144200100708      *  Controlla la trasmissione   se completa
144300100708      * ?------------------------------------------------------------------ */
144400100708     c     Check_Trasm   Begsr
144500100708
144600100708     C                   clear                   se_errore
144700100708     C                   clear                   Riga_iNIZIo       1
144800100708     C                   clear                   Riga_fINe         1
144900100708      ** primo  record
145000100708     c     *start        setll     tivin00r
145100100708      *
145200100708     c     rileggi       tag
145300100708     c                   EXSR      LEGGE_TIVIN_R
145400100708      *
145500100708     c                   if        EoFTivin <> 'S'
145600100708      *
145700100708     c                   if        dati = *blank
145800100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
145900100708      * le leggo e le escludo.
146000100708     C                   eval      vinFLG = '1'
146100100708     c                   update    Tivin000
146200100708     c                   goto      rileggi
146300100708     c                   end
146400100708      * ?              /*---------------------- */
146500100708     c                   exsr      Decod_Segmento
146600100708      * ?              /*---------------------- */
146700100708     c                   endif
146800100708      **
146900100708      ** ultimo record
147000100708     c     *hival        setll     tivin00r
147100100708     c     leggiAncora   tag
147200100708     c                   readp     tivin00r
147300100708     c                   if        %Eof(tivin00r)
147400100708     c                   move      'S'           EoFTivin
147500100708     c                   else
147600100708     c                   clear                   dati
147700100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
147800100708      *
147900100708     c                   if        dati = *blank
148000100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
148100100708      * le leggo e le escludo.
148200100708     C                   eval      vinFLG = '1'
148300100708     c                   update    Tivin000
148400100708     c                   goto      leggiAncora
148500100708     c                   end
148600100708      * ?              /*---------------------- */
148700100708     c                   exsr      Decod_Segmento
148800100708      * ?              /*---------------------- */
148900100708     c                   endif
149000100708
149100100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
149200100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
149300100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
149400100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
149500100708      * ?-----> Errore
149600100708     C                   eval      se_errore = 'S'
149700100708     c                   end
149800100708
149900100708     c                   endSR
150000100712     c*==================================================================*
150100100712     C*? CNVDAT - DECODIFICA LA DATA
150200100712     C* INPUT:  - WFORM  (FORMATO DATA : 102)
150300100712     C*         - WDATA  (DATA ALFANUMERICA)
150400100712     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
150500100712     C*         - WHMS   (ORA NUMERICA)
150600100712     C*----------------------------------------------------------------
150700100712     C     Converte_Data BEGSR
150800100712     C*
150900100712     C                   MOVEL     ' '           WERRO             1
151000100712     C                   Z-ADD     *ZEROS        WCAMG             8 0
151100100712     C                   Z-ADD     *ZEROS        WCAMGora         14 0
151200100712     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
151300100712     C                   Z-ADD     *ZEROS        WHMS              6 0
151400100712     C*
151500100712     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
151600100712     C                   MOVEL     Work_Data__35 WCOMO8            8
151700100712     C                   TESTN                   WCOMO8               99
151800100712     C   99              MOVE      WCOMO8        WCAMG                          *DATA
151900100712     C  N99              MOVEL     'S'           WERRO             1
152000100712     C                   END
152100100712     C*
152200100712     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
152300100712     C                   MOVEL     Work_Data__35 WCOMO12          12
152400100712     C                   TESTN                   WCOMO12              99
152500100712     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
152600100712     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
152700100712     C   99              MOVE      WCAMGORA      WHMS                           *DATA
152800100712     C  N99              MOVEL     'S'           WERRO             1
152900100712     C                   END
153000100712     C*
153100100712     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
153200100712     C                   MOVEL     Work_Data__35 WCOMO14          14
153300100712     C                   TESTN                   WCOMO14              99
153400100712     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
153500100712     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
153600100712     C   99              MOVE      WCAMGORA      WHMS                           *DATA
153700100712     C  N99              MOVEL     'S'           WERRO             1
153800100712     C                   END
153900100712     C*
154000100712     C                   ENDSR
154100100712      * ?------------------------------------------------------------------ */
154200100712      *?    Flagga il TIVIN come messaggio completamente ERRATO
154300100712      * ?------------------------------------------------------------------ */
154400100712     C     MSG_Errato    BEGSR
154500100712      *
154600100712      * ?  Scrive su tutti i records il tipo di errore
154700100712     c     *start        setll     tivin00r
154800100712     c
154900100712     c                   EXSR      LEGGE_TIVIN_R
155000100712     c                   dow       EoFTivin <> 'S'
155100100712     c
155200100712     c                   if        vinMSG  = *blank
155300100712     C                   evalR     vinMSG  = 'MSG IFTSTA ricevuto Anomalo +
155400100712     C                               >> Controllare i DATI su UPLOAD !!'
155500100712     c                   end
155600100712     C                   eval      vinFLG = '2'
155700100712     c                   eval      Almeno_Un_Due ='S'
155800100712     c                   eval      esito  = '2'
155900100712     c                   eval      se_errore ='S'
156000100712     c                   eval      err_bloccante ='S'
156100100712      *
156200100712     c                   update    tivin000
156300100712     c                   EXSR      LEGGE_TIVIN_R
156400100712     c                   endDO
156500100712      *
156600100712     C                   ENDSR
156700100712      * ?------------------------------------------------------------------ */
156800100712      *?    Flagga il TIVIN come messaggio completamente ERRATO
156900100712      * ?------------------------------------------------------------------ */
157000100712     C     DETta_Errato  BEGSR
157100100712      *
157200100712     c                   EXSR      LEGGE_TIVIN_R
157300100712     c                   dow       EoFTivin <> 'S'
157400100712      *
157500100712      * Se ha letto il successivo record
157600100712      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
157700100712     c                   if        VNREC = UNT_record  or
157800100712     c                             VNREC = finDET_RECord
157900100712     c                   leave
158000100712     c                   end
158100100712      *
158200100712     c                   exsr      Error_DET
158300100712      *
158400100712     c                   if        vinMSG  = *blank
158500100712     C                   evalR     vinMSG  = 'Dettaglio IFTSTA ERRATO -
158600100712     C                             >> Controllare i DATI di ogni Segmento <<'
158700100712     c                   end
158800100712      *
158900100712     c                   update    tivin000
159000100712     c                   EXSR      LEGGE_TIVIN_R
159100100712     c                   endDO
159200100712      * ------
159300100712???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
159400100712     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
159500100712???? C                             STATI IFTSTA non totalmente tradotto. -
159600100712     C                             Controllare UPLOAD del cliente:'
159700100712     C                   EVAL      Messaggio = %trim(Messaggio) +
159800100712     C                                         %editc(§PTksc:'X')
159900100712     c                   if        §PVinv <> 'N'
160000100712     c                   exsr      invio_mail
160100100712     c                   end
160200100712      *
160300100712      * ripulisce per un nuovo giro
160400100712     C                   clear                   Err_in_detta
160500100712      *
160600100712     C                   ENDSR
160700121105      *---------------------------------------------------------------*
160800121105      * CTRL caricamento schiere    PT                               -*
160900121105      *---------------------------------------------------------------*
161000121105     C     Check_PT      begsr
161100121105     C*
161200121105     c* Controllo riempimento schiera
161300121105     c                   clear                   trul0sds
161400121105     c                   eval      i0sski='PT_key'
161500121105     c                   eval      i0sele=%elem(PT_key)
161600121105     c                   eval      i0spie=x
161700121105     c                   eval      i0sfile='EDTAB00F'
161800121105     c                   eval      i0ssif=knsif
161900121105     c                   eval      i0spgm='TRTCT94R'
162000121112     c                   move      kpjbu         SvKpjbu
162100121112     c                   clear                   kpjbu
162200121105     c                   movel     trul0sds      kpjbu
162300121105     c                   call      'TRUL0SR'
162400121105     c                   parm                    kpjba
162500121112     c                   eval      kpjbu = SvKpjbu
162600121105     C*
162700121105     C                   ENDSR
162800100712      * ?------------------------------------------------------------------ */
162900090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
163000090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
163100090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
163200090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
