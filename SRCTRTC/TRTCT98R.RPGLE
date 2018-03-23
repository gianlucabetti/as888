000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600130410????  *  TRASCODIFICA : MESSAGGI EDI  -   O.R.M.IMPORT   con  IFTMBP vers D99B  *
000700060612      **?************************************************************************
000800991124      * Il pgm crea:                                                            *
000900130410      *             FIVAO00F file degli ORM da confermare                       *
001000060609      **?************************************************************************
001100090213     Ftivin00r  uF   E             DISK    usropn  INFDS(VINRECDS)
001200130410      *---------------------------------------------------------------------*
001300130410     FFnVAO00F  O  a E             DISK    commit
001400130424     Ftiori00f  O  a E             DISK    commit
001500130410      *---------------------------------------------------------------------*
001600130410     ftabel00f  if   E           K DISK
001700130410     ffnacr01l  if   E           K DISK
001800130410     fcnaco00f  if   E           K DISK
001900090209     FCNIND00F  IF   E           K DISK
002000130410     fedtab01l  if   E           K DISK
002100090210      *
002200091019???? Fedstbl01l IF   E           K DISK
002300130410      **?************************************************************************
002400130410      * Stringhe per conversione alfanumerico/numerico N.Documento
002500130410     D CA              S             78    DIM(1) CTDATA PERRCD(1)
002600130410     D CN              S             78    DIM(1) CTDATA PERRCD(1)
002700130410      **?************************************************************************
002800130410      **  Elenco DS di tutti i Segmenti da tradurre
002900130410      **?************************************************************************
003000130410???? D edS00BGM      E DS
003100130410???? D edS00CNI      E DS
003200130410???? D edS00COM      E DS
003300130410???? D edS00CTA      E DS
003400130410???? D edS00DTM      E DS
003500130410???? D edS00FTX      E DS
003600130410???? D  D05                   16    365    DIM(5)
003700130410???? D edS00GID      E DS
003800130410???? D edS00LOC      E DS
003900130410???? D edS00MEA      E DS
004000130410???? D edS00NAD      E DS
004100130410???? D edS00RFF      E DS
004200130410???? D edS00TDT      E DS
004300130410???? D edS00TSR      E DS
004400130410???? D edS00UNA      E DS
004500130410???? D edS00UNB      E DS
004600130410???? D edS00UNH      E DS
004700130410???? D edS00UNT      E DS
004800130410???? D edS00UNZ      E DS
004900130410      **?************************************************************************
005000130410     D EDIDSPT       E DS
005100130410     D EDIDSPU       E DS
005200130410     D EDIDSPV       E DS
005300130410     D EDIDSPZ       E DS
005400130410     D EDIDSCL       E DS
005500130410     D EDIDSSS       E DS
005600130410     D EDIDSTC       E DS
005700130410      **?************************************************************************
005800130410     D KPJBA         E DS
005900130410     D     svkpjbu     s                   like(kpjbu)
006000130424     D trul33ds      e ds
006100130410      **?************************************************************************
006200130410     D* DS PER FNLV13R - CONTROLLO DI UN CAP
006300130410     D DSLV13        E DS                  EXTNAME(FNLV13DS)
006400130410     D* DS PER TISI95R - CONTROLLO DI CAP
006500130410     D DSSI95        E DS                  EXTNAME(TISI95DS)
006600130410      *---------------------------------------------------------------------*
006700130410     D UTEDSE0F      E DS
006800130410     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
006900130410     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
007000130410      * Ds scomposzione tipo capoconti
007100130410     D TCUDS           DS
007200130410     D  F34                    3      4
007300130410     D  F56                    5      6
007400940321      *----------------------------------------------------*
007500090209      * numero relativo di record
007600090209     D SRECDS          DS
007700090209     D  NRREC                397    400B 0
007800090209      *
007900090213      * numero relativo di record
008000090213     D VINRECDS        DS
008100090213     D  VNREC                397    400B 0
008200130410      **---------------------------------------------------*
008300100701     D WLBDA8          DS
008400100701     D  G02DAT                 1      8  0
008500100701     D  G02INV                 9     16  0
008600100701     D  G02ERR                17     17
008700100701     D  G02TGI                18     22  0
008800100701      *----------------------------------------------------*
008900090209     D Valori_inErr    ds
009000090209     D  SKout_Errori                  1    DIM(50)
009100100630      **
009200090209     D Descr_Errore    ds
009300090209     D  SKout_DesErr                 50    DIM(50)
009400090205      *----------------------------------------------------*
009500091016     D EoFTivin        s              1
009600091016      *----------------------------------------------------*
009700090205     D Tipo_seg        S              3
009800100720     D Trovato_seg     S              1
009900100716      *
010000100716     d keyUNBCLI       s             35
010100100716     d keyTIPOMSG      s              6
010200100716     d keyVERSION      s              3
010300100716     d keyRELEASE      s              3
010400100716     d keyAGENCY       s              3
010500100716     d keyASSOCIA      s              6
010600100716      *
010700100720     D DsGenerica      s           2048
010800090209     D segmento        s           2048
010900090209     D Errore          s              1
011000130411     D err_bloccante   s              1
011100100701      *
011200000223     D W0140           S             14  0
011300991129     D WORA            S              6  0
011400100701     D DataGGMMAAA     S              8  0
011500100701     D DATEU           S              8  0
011600100701     D DATA_eur        S               D   DATFMT(*eur)
011700100701      *
011800100628     D XX              S              3  0 INZ
011900110328     d Key_Generica35  s             35
012000100708     D IniDET_RECord   s                   LIKE(vnREC)
012100100708     D FinDET_RECord   s                   LIKE(vnREC)
012200100318      *------------------------------------------
012300100318      **  Definizione Qualificatori
012400100318      *------------------------------------------
012500130410???? D Tipo_Messaggio...
012600130410???? D                 s              3    INZ('335')
012700130411???? D Riferimento_Ordine...
012800130411???? D                 s              3    INZ('CR ')
012900130411???? D Riferimento_Destinatario...
013000130411???? D                 s              3    INZ('ANT')
013100130411???? D Riferimento_Cliente_FF...
013200100318???? D                 s              3    INZ('FF ')
013300100318???? D Telephone_Number...
013400100318???? D                 s              3    INZ('TE ')
013500100318???? D TeleFax_Number...
013600100318???? D                 s              3    INZ('TX ')
013700130410???? D Volume...
013800130410???? D                 s              3    INZ('VOL')
013900100705???? D Weight...
014000100705???? D                 s              3    INZ('WT ')
014100100319???? D G_Weight...
014200100319???? D                 s              3    INZ('G  ')
014300100318???? D Gross_Weight...
014400100318???? D                 s              3    INZ('AAG')
014500100706???? D Gross_Weight_E...
014600100706???? D                 s              3    INZ('AAE')
014700110512???? D Gross_Weight_D...
014800110512???? D                 s              3    INZ('AAD')
014900100706???? D N_Weight...
015000100706???? D                 s              3    INZ('N  ')
015100100706???? D Net_Weight...
015200100706???? D                 s              3    INZ('AAF')
015300100318???? D Kilograms...
015400100318???? D                 s              3    INZ('KGM')
015500100319???? D Grams...
015600100319???? D                 s              3    INZ('163')
015700100318???? D Meters...
015800100318???? D                 s              3    INZ('MTR')
015900100318???? D Cubic_Meters...
016000100318???? D                 s              3    INZ('MTQ')
016100100705???? D Data_senza_ora...
016200100628???? D                 s              3    INZ('102')
016300100705???? D Data_con_ora...
016400100628???? D                 s              3    INZ('203')
016500100705???? D Data_con_i_secondi...
016600100705???? D                 s              3    INZ('204')
016700120613     D controlla_COD_FTX...
016800120613???? D                 s              3
016900130411     d  Natura_Merce...
017000130411     d                 s              3    inz('AAA')
017100100316      *---------------------------------------------------------------------*
017200100316      **  segmenti Identificativi parti specifiche del messaggio
017300100316      *---------------------------------------------------------------------*
017400100701???? D seg_imprevisto...
017500100701???? D                 s              1
017600100701      *---------------------------------------------------------------------*
017700130411???? D NAD_Destinatario...
017800100701???? D                 s              1
017900130411???? D NAD_Mittente...
018000100701???? D                 s              1
018100130411???? D NAD_Ordinante...
018200130411???? D                 s              1
018300130411???? D NAD_HI          s              6    inZ('VAORSO')
018400130411???? D NAD_CZ          s              6    inZ('VABRMO')
018500130411???? D NAD_CN          s              6    inZ('VABRSD')
018600111121???? D NAD_Lunghezza_Indir_Aggiuntivo...
018700111121???? D                 s              3s 0
018800111121???? D NAD_Lunghezza_Indirizzo...
018900111121???? D                 s              3s 0
019000130411???? D NAD_Lunghezza_ragSociale...
019100111121???? D                 s              3s 0
019200100701      *
019300090209      *---------------------------------------------------------------------*
019400100701???? D Contatore_Dettagli...
019500100701???? D                 s              5s 0 INZ(0)
019600100728???? D Segmento_in_errore...
019700100728???? D                 s              5s 0 INZ(0)
019800130424     D §numori         s              7  0 inz
019900100701      *---------------------------------------------------------------------*
020000100630     d UNB_diWork      s                   like(UNB0004)
020100100628     d  UNB_Mittente   s                   like(UNB0004)
020200100706     d BGM_Id_Testata  s             35A   inz(' ')
020300130411     d  RFF_RifRitiro  s             35A
020400100701     d UNB_parziale    s              1    inz('N')
020500100813     d UNB_Generico    s             31    inz(' ')
020600100701     d UNB_NrTrasmiss  s             14    inz(' ')
020700100701     d UNZ_NrTrasmiss  s             14    inz(' ')
020800100701     d UNH_Rifer_Msg   s             14    inz(' ')
020900130410      *
021000130411     D TRUL0SDS      E DS
021100130411      *
021200100701      * Tabella partner
021300121105     D PT_key          S             35    DIM(200)
021400121105     D PT_Parz         S             35    DIM(%elem(PT_key))
021500121105     D PT_KSC          S              7    DIM(%elem(PT_key))
021600121105     D PT_uni          S             90    DIM(%elem(PT_key))
021700121105     D PU_uni          S             90    DIM(%elem(PT_key))
021800121105     D PV_uni          S             90    DIM(%elem(PT_key))
021900121105     D PZ_uni          S             90    DIM(%elem(PT_key))
022000100701      *
022100130411     D Nr_PT           S              3  0 INZ
022200130411     D PT_key_savXX    S              3  0 INZ
022300130411     D  PT_poe         s                   LIKE(§PTlnp)
022400130411     D  PT_cor         s                   LIKE(§PTksc)
022500130411     d  PT_KeyXsav     s              3  0 inz(0)
022600130411     D  PT_trovata     s              1    inz('N')
022700130411     D  PT_con_piu_Nazioni...
022800130411     d                 s              1
022900100701     D PU_key          S             35
023000130411     D  PU_TipoServ    s              2a
023100130411     D  PU_SpecServ    s              2a
023200130424     D  PU_ORMAKSC     s              7a
023300130424     D  PU_ORMACTR     s              3a
023400100701     D PV_key          S             35
023500120629     D PZ_key          S             35
023600130410      *
023700100701      *
023800100701     d  GID_aTotale    s              1A
023900100705     d  GID_ColliInAdd...
024000100705     d                 s                   like(gid722420)
024100090209      *---------------------------------------------------------------------*
024200090209      * Schiere
024300090209      *---------------------------------------------------------------------*
024400090209      * Nr. documento alfanumerico da decodificare
024500090209     D NDA             S              1    DIM(35)
024600100630      *
024700090209      * Nr. documento alfanumerico da già decodificato
024800090209     D NDN             S              1    DIM(15)
024900100630      *
025000090209      * Messaggi di errore
025100090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
025200100630      *
025300130410      * ?================================================================== */
025400130410      * Tabella codici nazioni
025500130410     D Ds15          e DS
025600130410     D  Naz03          s              3    DIM(999)
025700130410     D  Iso03          s              3    DIM(999)
025800130410     D  Cpf03          S              2  0 DIM(999)
025900130410      * ?================================================================== */
026000130410     D Dorm01        e DS
026100100630      *
026200090209      * Tabella CL --> codici clienti - tariffa
026300130305     D Cod_Tb_CL       S             35    DIM(999)
026400130305     D Des_Tb_CL       S             90    DIM(999)
026500100630      *
026600100318      * Priorità trasporto
026700100705     D Key_TipServ     S             35    DIM(100)
026800100702     D TipoServizio    S              1    DIM(100)
026900100702      *
027000100702      * Decodifiche servizi speciali
027100100702     D SpecialServ     S             35    DIM(100)
027200100702     D Key_ServSpec    S              3    DIM(100)
027300100702     D UNI_ServSpec    S             90    DIM(100)
027400100630      *
027500090209      * Schiere x routine di conversione CAP
027600090209     D CAP5            S              1    DIM(5)
027700090209     D CAP9            S              1    DIM(9)
027800090209     D CAPM            S              1    DIM(9)
027900100630      *
028000090209      * Schiera per memorizzazione FTX ricevuti
028100090209     D QUA             S              3    DIM(100)
028200090209     D FTX             S             70    DIM(100)
028300100630      *
028400090209      * Tabelle capiconto
028500090209      * Schiere x località
028600090209     D LOD             S              1    DIM(35)
028700060608      **?------------------------------------------------------------------ */
028800090209     C* Definizione campi di work
028900090209     D kKUT            s                   LIKE(TBLKUT)
029000090209     D kCOD            s                   LIKE(TBLCOD)
029100090209     D kKCC            s                   LIKE(ACOKCC)
029200090209     D kKSC            s                   LIKE(ACOKSC)
029300130410      **?------------------------------------------------------------------ */
029400090209      *  Invio E-mail
029500090209     D Indirizzi       s            253
029600090209     D Oggetto         s             44
029700090209     D Messaggio       s           5000
029800090209     d   DSmail        ds
029900090209     D Mail_Ind                      44
030000090209     d   IND_char                     1    dim(44)
030100090209      *
030200100630     D Lunghezza_Indirizzo...
030300100630     D                 s              5u 0
030400100630     D dalCarattere    s              5u 0
030500100630     D xTOTcaratteri   s              5u 0
030600090209      *
030700130410     d bart_it         C                   CONST('@Brt.it;')
030800130410     d CED_Bart        C                   CONST('CED@Brt.it;')
030900060612      * ?================================================================== */
031000060612      * ?   * Campi x decodifica * (INPUT  del Record)
031100100319      * ?================================================================== */
031200090130     D  Dati           s           2048
031300060612      * ?   *--------------------------------------------------------------*
031400060612     D  se_errore      s              1    inz(' ')
031500060612      * ?* ------------------------------------------------------ *
031600130410     d minu            c                   const('qwertyuiopasdfghjklzxcvbnmàáâ-*alfabeto
031700130410     d                                     ãäåèéêëìíîïòóôõöùúûüæçðñßÑÀÁÂÃÄÅÆÈÉÊ-
031800130410     d                                     ËÌÍÎÏÒÓÔÕÖÙÚÛÜ')
031900130410     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMAAA-*ALFABETO
032000130410     d                                     AAAEEEEIIIIOOOOOUUUUACANSNAAAAAAAEEE-
032100130410     d                                     EIIIIOOOOOUUUU')
032200130410     D Digits          C                   '0123456789'
032300060612      * ?================================================================== */
032400060612      *   Ciclo principale
032500090205      * ?================================================================== */
032600060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
032700060612     C                   if        not %open(tivin00r)
032800060612     C                   open      tivin00r
032900060612     C                   endif
033000130410     C*
033100060612      * ?------------------------------------------------------------------ */
033200130410      *? Importa i records della trasmissione
033300130410      * ?------------------------------------------------------------------ */
033400130410     c     *start        setll     Tivin00r
033500130410
033600130410     c                   EXSR      LEGGE_TIVIN_R
033700130410     c                   dow       EoFTivin <> 'S'
033800130410
033900130410      * solo i record sflaggati da rielaborare
034000130410     c                   IF        vinFLG = *blank
034100130410     C                   eval      vinFLG = '1'                                 Sempre Record OK
034200130410     c                   clear                   se_errore
034300130410      *** >>>>>>
034400130410     c                   IF        vinDTA <> *blank
034500130410      *** >>>>>>
034600130410      ** Decodifica record a campi variabili
034700130410      * ?              /*---------------------- */
034800130410     c                   exsr      Decod_Segmento
034900130410      * ?              /*---------------------- */
035000130410
035100130410      *  x errore bloccante nella preparazione del VAO
035200130410     c                   if        err_bloccante ='S'
035300130410     C                   eval      vinFLG = '2'
035400130410     c                   eval      Almeno_Un_Due ='S'
035500130410     c                   eval      esito  = '2'
035600130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFTMBP - ORM'
035700130506      *
035800130410     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
035900130410     C                             ORM import - messaggio -
036000130410     C                             con UNB non presente in tabella PT controlla-
036100130410     C                             re EDI ricevuto su UPLOAD.'
036200130410     c                   if        §PVinv <> 'N'
036300130410     c                   exsr      invio_mail
036400130410     c                   end
036500130410     c                   end
036600130410      *** >>>>>>
036700130410     c                   endIF
036800130410      *** >>>>>>
036900130410      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
037000130410      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
037100130410      *         un erroneo messaggio
037200130410      *     di errore come se tutto il messaggio fosse ricevuto errato.
037300130410     c                   update    Tivin000
037400130410
037500130410     c                   endIF
037600130410
037700130410     c                   EXSR      LEGGE_TIVIN_R
037800130410     C                   ENDdo
037900130410      **
038000060612      * ?              /*---------------------- */
038100060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
038200090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
038300090225     c                             esito ='0'
038400060710     C                   eval      esito ='0'
038500090225      *
038600090225     c                   COMMIT
038700110208     C                   endif
038800130410      * ?              /*---------------------- */
038900090224      *
039000090213     c                   if        almeno_un_due ='S'
039100110208     C                   eval      esito ='1'
039200060614     C                   endif
039300100708      * chiude TIVIN
039400100708     C                   if        %open(tivin00r)
039500100708     C                   close     tivin00r
039600100708     C                   endif
039700060614      *
039800060612     c                   SETON                                        LR
039900100709      * ?------------------------------------------------------------------ */
040000100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
040100100709      * ?------------------------------------------------------------------ */
040200100709     C     MSG_Errato    BEGSR
040300100709      *
040400100709      * ?  Scrive su tutti i records il tipo di errore
040500100709     c     *start        setll     tivin00r
040600100709     c
040700100709     c                   EXSR      LEGGE_TIVIN_R
040800100709     c                   dow       EoFTivin <> 'S'
040900100709     c
041000100709     c                   if        vinMSG  = *blank
041100100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
041200100709     C                               >> Controllare i DATI su UPLOAD !!'
041300100709     c                   end
041400100709     C                   eval      vinFLG = '2'
041500100709     c                   eval      Almeno_Un_Due ='S'
041600100709     c                   eval      esito  = '2'
041700100709     c                   eval      se_errore ='S'
041800100709     c                   eval      err_bloccante ='S'
041900100709      *
042000100709     c                   update    tivin000
042100100709     c                   EXSR      LEGGE_TIVIN_R
042200100709     c                   endDO
042300100709      *
042400100709     C                   ENDSR
042500100709      * ?------------------------------------------------------------------ */
042600100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
042700100709      * ?------------------------------------------------------------------ */
042800100709     C     DETta_Errato  BEGSR
042900100709      *
043000100709     c                   EXSR      LEGGE_TIVIN_R
043100100709     c                   dow       EoFTivin <> 'S'
043200100709      *
043300100709      * Se ha letto il successivo record
043400100709      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
043500130410     c                   if        VNREC = finDET_RECord
043600100709     c                   leave
043700100709     c                   end
043800100709      *
043900100709     c                   exsr      Error_DET
044000100709      *
044100100709     c                   if        vinMSG  = *blank
044200100709     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
044300100709     C                             >> Controllare i DATI di ogni Segmento <<'
044400100709     c                   end
044500100709      *
044600100709     c                   update    tivin000
044700100709     c                   EXSR      LEGGE_TIVIN_R
044800100709     c                   endDO
044900100709      * ------
045000130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFTMBP - ORM'
045100130506      *
045200100709     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
045300130410???? C                             ORM IFTMBP non totalmente tradotto. -
045400100709     C                             Controllare UPLOAD del cliente:'
045500100709     C                   EVAL      Messaggio = %trim(Messaggio) +
045600100709     C                                         %editc(§PTksc:'X')
045700100728     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
045800100728     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
045900100709     c                   if        §PVinv <> 'N'
046000100709     c                   exsr      invio_mail
046100100709     c                   end
046200100709      *
046300100709     C                   ENDSR
046400130410      * ?------------------------------------------------------------------ */
046500130410      *?      X non bloccare in nessun caso il traduttore CLIENTI
046600130410      * ?------------------------------------------------------------------ */
046700130410     C     *pssr         BEGSR
046800130410     C
046900130410     C                   eval      esito = '2'
047000130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFTMBP - ORM'
047100130506      *
047200130410     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
047300130410     C                             O.R.M. import  - messaggio -
047400130506     C                             EDI ricevuto errato su UPLOAD ' +
047500130506     C                             'del Cliente/Partner: ' + %editc(§PTksc:'X')
047600130506     c                             + ' verificare il segmento -> ' + dati
047700130506      *
047800130410     c                   if        §PVinv <> 'N'
047900130410     c                   exsr      invio_mail
048000130410     c                   end
048100130506     C
048200130506     c                   seton                                        LR
048300130506     c                   return
048400130410     C
048500130506     C                   ENDSR     '*CANCL'
048600130410      * ?------------------------------------------------------------------ */
048700130410      *?      Segnala Errore su TIVIN
048800130410      * ?------------------------------------------------------------------ */
048900130410     C     Error_DET     BEGSR
049000130410     C
049100130410     c                   eval      Segmento_in_errore = Conta_segmTOT
049200130410     C                   eval      vinFLG = '2'
049300130410     c                   eval      Almeno_Un_Due ='S'
049400130410     c                   eval      esito  = '1'
049500130410     c                   eval      se_errore ='S'
049600130410     C                   eval      Err_in_detta = 'S'
049700130410     C
049800130410     C                   ENDSR
049900100709      * ?------------------------------------------------------------------ */
050000100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
050100100709      * ?------------------------------------------------------------------ */
050200100709     C     LEGGE_TIVIN_R BEGSR
050300100709      *
050400100709     c                   clear                   EoFTivin
050500100709      *  Lettura sequenziale
050600100709     c                   read      Tivin00r
050700100709
050800100709     c                   if        %Eof(tivin00r)
050900100709     c                   move      'S'           EoFTivin
051000100709     c                   else
051100100709      *
051200100709      * ?  imposta il tipo di segmento:
051300100709     c                   clear                   dati
051400100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
051500100728     c                   eval      segmento = %trimL(dati)
051600100728     c                   eval      tipo_seg = %subst(segmento:1:3)
051700100709      **
051800100709     c                   end
051900100709      *
052000100709     C                   ENDSR
052100100120      * ?------------------------------------------------------------------ */
052200100120      *?    Decodifica il segmento ed importa i campi in formato DS
052300100120      * ?------------------------------------------------------------------ */
052400100120     c     Decod_SegmentoBegsr
052500130410      *
052600130410      *  Contatore delle righe ossia dei segmenti del messaggio
052700130410     c                   add       1             Conta_segmenti
052800130410     c                   add       1             Conta_segmTot
052900100813      *
053000100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
053100100120      * ?  Anche se non utilizzati.
053200100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
053300100317     c                   clear                   seg_imprevisto
053400100720     c                   clear                   trovato_seg
053500100716     C                   clear                   keyUNBCLI
053600100716     C                   clear                   keyTIPOMSG
053700100716     C                   clear                   keyVERSION
053800100716     C                   clear                   keyRELEASE
053900100716     C                   clear                   keyAGENCY
054000100716     C                   clear                   keyASSOCIA
054100110530      **--------
054200110530      **--   PASSA sempre l'UNB mittente se ci fossero delle eccezioni da considerare
054300110530     c                   movel     UNB_Mittente  keyUNBCLI
054400100720      **--------
054500100720      *  Ritorna la DS GENERICA oppure l'errore
054600100720      *  Chiamata generica x decodificare il singolo segmento letto
054700100720     c                   call      'TRTCT00R1'
054800100720      * input
054900100720     c                   parm                    tipo_seg
055000100720     c                   parm                    segmento
055100100720     C                   parm                    keyUNBCLI
055200100720     C                   parm                    keyTIPOMSG
055300100720     C                   parm                    keyVERSION
055400100720     C                   parm                    keyRELEASE
055500100720     C                   parm                    keyAGENCY
055600100720     C                   parm                    keyASSOCIA
055700100720      *output
055800100720     c                   parm                    Errore
055900100720     c                   parm                    DsGenerica
056000100720     c                   parm                    Valori_inErr
056100100720     c                   parm                    Descr_Errore
056200100720     c                   parm                    trovato_seg
056300120424      *
056400120424      *>>>--->>>                                                    <<<---<<<
056500120424      *   Se ci sono dei Segmenti Particolari da NON considerare
056600130410     c                   IF        trovato_seg ='N'
056700120424      *
056800120424      *   ("SGM" -> NON ESISTE NELL'EDI  come TIPO SEGMENTO)
056900120424     C                   movel(p)  'SGM'         etbSGM
057000120424     C                   movel(p)  Tipo_Seg      etbVALSGM
057100120424     c                   exsr      In_TAB_EDSTBL
057200120424      *
057300120424     C*  Quindi sono dichiarati nel file delle eccezioni
057400130410     C                   If        Quale_campo ='TIPSGM' and trovata_EDSTBL ='S'
057500120424     c                   eval      trovato_seg ='S'
057600120424      * serve per bypassare eventuali records  NON in mappatura e non segnalare
057700120424      * errori sul TIVIN.
057800120424     c                   endIf
057900130410     c
058000120424     c                   end
058100120424      *>>>--->>>                                                    <<<---<<<
058200100708      *** ------------------------------------
058300100708      *   Decodifica il singolo segmento LETTO
058400100708      *** ------------------------------------
058500100120     c                   select
058600100120      *
058700130410     c                   when      trovato_seg ='N'
058800100720     c                   move      'S'           seg_imprevisto
058900100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
059000100720     c                   exsr      Error_DET
059100100720     c                   goto      end_Decod
059200100720      **--------
059300100120     c                   when      tipo_seg = 'UNA'
059400100720     c                   movel(p)  DsGenerica    EDS00UNA
059500130410     C                   EXSR      DATI_UNA
059600130410      **--------
059700100120     c                   when      tipo_seg = 'UNB'
059800100720     c                   movel(p)  DsGenerica    EDS00UNB
059900100120     C                   EXSR      DATI_UNB
060000100120      * --------
060100100120     c                   when      tipo_seg = 'UNH'
060200100720     c                   movel(p)  DsGenerica    EDS00UNH
060300100120     C                   EXSR      DATI_UNH
060400100120      * --------
060500100120     c                   when      tipo_seg = 'BGM'
060600100720     c                   movel(p)  DsGenerica    EDS00BGM
060700100316     C                   EXSR      DATI_BGM
060800100120      **--------
060900100120     c                   when      tipo_seg = 'DTM'
061000100720     c                   movel(p)  DsGenerica    EDS00DTM
061100100120     C                   EXSR      DATI_DTM
061200100120      * --------
061300100120     c                   when      tipo_seg = 'TSR'
061400100720     c                   movel(p)  DsGenerica    EDS00TSR
061500100316     C                   EXSR      DATI_TSR
061600100120      **--------
061700100120     c                   when      tipo_seg = 'RFF'
061800100720     c                   movel(p)  DsGenerica    EDS00RFF
061900100120     C                   EXSR      DATI_RFF
062000100316      **--------
062100100316     c                   when      tipo_seg = 'LOC'
062200100720     c                   movel(p)  DsGenerica    EDS00LOC
062300100316     C                   EXSR      DATI_LOC
062400100316      **--------
062500100120     c                   when      tipo_seg = 'TDT'
062600100720     c                   movel(p)  DsGenerica    EDS00TDT
062700100316     C                   EXSR      DATI_TDT
062800100120      **--------
062900100120     c                   when      tipo_seg = 'FTX'
063000100720     c                   movel(p)  DsGenerica    EDS00FTX
063100100120     C                   EXSR      DATI_FTX
063200100120      **--------
063300100120     c                   when      tipo_seg = 'NAD'
063400100720     c                   movel(p)  DsGenerica    EDS00NAD
063500100120     C                   EXSR      DATI_NAD
063600100120      **--------
063700100120     c                   when      tipo_seg = 'CTA'
063800100720     c                   movel(p)  DsGenerica    EDS00CTA
063900100120     C                   EXSR      DATI_CTA
064000100120      **--------
064100100120     c                   when      tipo_seg = 'COM'
064200100720     c                   movel(p)  DsGenerica    EDS00COM
064300100120     C                   EXSR      DATI_COM
064400100120      **--------
064500100120     c                   when      tipo_seg = 'GID'
064600100720     c                   movel(p)  DsGenerica    EDS00GID
064700100120     C                   EXSR      DATI_GID
064800100120      **--------
064900100120     c                   when      tipo_seg = 'MEA'
065000100720     c                   movel(p)  DsGenerica    EDS00MEA
065100100120     C                   EXSR      DATI_MEA
065200100120      **--------
065300100120     c                   when      tipo_seg = 'UNT'
065400100720     c                   movel(p)  DsGenerica    EDS00UNT
065500100120     C                   EXSR      DATI_UNT
065600100120      **--------
065700100120     c                   when      tipo_seg = 'UNZ'
065800100720     c                   movel(p)  DsGenerica    EDS00UNZ
065900100120     C                   EXSR      DATI_UNZ
066000110105      **--------
066100130410     c                   When      tipo_seg = *blank
066200100120      **--------
066300100120     c                   OTHER
066400120424      **--------
066500130410     c                   if        trovata_EDSTBL <>'S'
066600100120     c                   move      'S'           seg_imprevisto
066700100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
066800100120     c                   exsr      Error_DET
066900100120     c                   goto      end_Decod
067000110110     c                   end
067100100120      **--------
067200100120     c                   endSL
067300100120      **
067400100120     c     end_Decod     endSR
067500090210     C*----------------------------------------------------------------
067600130410      *? DECODIFICA DATI UNA
067700090210     C*----------------------------------------------------------------
067800130410     C     DATI_UNA      BEGSR
067900130410      *
068000130410     c                   z-add     vnrec         IniDET_RECord
068100130410     c                   clear                   FinDET_RECord
068200130410      *
068300130410     c                   endSR
068400130410     C*----------------------------------------------------------------
068500130410      *? DECODIFICA DATI UNB
068600130410     C*----------------------------------------------------------------
068700130410     C     DATI_UNB      BEGSR
068800130410      *
068900130411     C                   clear                   PT_key_savXX
069000130411     C                   clear                   PT_poe
069100130411     C                   clear                   PT_cor
069200130410     C                   eval      PT_con_piu_Nazioni = *blank
069300130410     c                   move      *blank        Naz_salvata       3
069400130410     C*
069500130410     c                   clear                   UNB_Generico
069600130410     c                   clear                   UNB_NrTrasmiss
069700130410     c                   clear                   UNZ_NrTrasmiss
069800090210      *
069900100701      * identificativo Messaggio
070000100701     C                   eval      UNB_NrTrasmiss = unb0020
070100100701      *
070200100813     C                   MOVEL     unb0017       Anno2             2 0
070300100813     C                   z-add     2000          Anno4             4 0
070400100813     C                   add       Anno2         Anno4
070500100813     C                   MOVE      unb0017       WGG               2 0
070600100813     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
070700100813     C                   MOVE      Anno2         Data_messag
070800100813     C                   MOVEL     WGG           Data_messag
070900100813     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
071000100813     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
071100100813     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
071200100813     c****
071300100813     c                   clear                   Mittente         35
071400100813     c                   clear                   Mitt_Qualifier    4
071500100813     c                   clear                   Id_Messaggio     14
071600100813     C                   eval      Mittente       = unb0004
071700100813     C                   eval      Mitt_qualifier = unb000720
071800100813     C                   eval      Id_Messaggio   = UNB0022
071900100813      *
072000090209      ** ---------------------------------------------------------------
072100090209      * quindi 1° cosa:
072200090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
072300090209      *     senza questo il messaggio NON è trascodificabile
072400100630     C                   MOVEL     unb0004       UNB_diWork
072500100630     C                   MOVE      unb000720     UNB_diWork
072600100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
072700100630     C                   Z-ADD     1             Nr_PT
072800100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
072900090209      *
073000130410      * Poi verifica se UNB è fra quelli parzializzati con la Nazione:
073100100317     C                   If        *IN32 = *off
073200100630     C                   eval      Nr_PT = 1
073300100630     c                   clear                   UNB_diWork
073400100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
073500100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
073600100317      *
073700100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
073800100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
073900100317      *  per cercare di individuare con certezza il codice della tabella PT in base
074000100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
074100100317     C                   If        *IN32 = *on
074200100317      *
074300100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
074400100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
074500100317      * ?  su Partner che hanno + linee con + nazioni.
074600100813      *  se è considerato Parziale
074700100813      *   Salva il codice generico
074800100630     c                   eval      UNB_parziale = 'S'
074900100813     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
075000100813      *             *------------------------------*
075100100813     c                   exsr      cerca_Mittente
075200100813      *             *------------------------------*
075300100813     C                   eval          Naz_salvata = Naz_mittente
075400100813     c                   Endif
075500100813     c                   Endif
075600100813      *
075700100813      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
075800100813      *             *------------------------------*
075900100813     c                   exsr      esito_tab_PT
076000100813      *             *------------------------------*
076100100813      **
076200100813      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
076300100813     C                   If        PT_trovata = 'S'
076400100813      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
076500100813      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
076600100813      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
076700100813      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
076800100813      * ?- Salva l'indice delle schiere per riaggancarla in seguito        ------ */
076900130411     C                   z-add     Nr_PT         PT_key_savXX
077000100813      * ?- Se ha più LINEE da gestire                                      ------ */
077100100813     C                   eval      PT_con_piu_Nazioni = §puPLN
077200100813     c                   end
077300090209      **
077400090210     c     end_UNB       endSR
077500130410     C*----------------------------------------------------------------
077600130410      *? cerca Nazione del primo Mittente nella sequenza dettagli
077700130410     C*----------------------------------------------------------------
077800130410     C     cerca_MittenteBEGSR
077900130410      *
078000130410     c                   z-add     vnrec         rec_posiz         9 0
078100130410     c                   move      *blank        Naz_mittente      3
078200130410     c                   movel(p)  UNB_Generico  PT_Parziale      35
078300130410     c                   move      'N'           OK_Nazione        1
078400130410      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
078500130410     c                   call      'TRTCT98R1'
078600130410     c                   parm                    rec_posiz
078700130410     c                   parm                    PT_Parziale
078800130410     c                   parm                    Naz_mittente
078900130410     c                   parm                    Ok_Nazione
079000130410     c                   parm                    User_msg
079100130410     C                   parm                    keyUNBCLI
079200130410     C                   parm                    keyTIPOMSG
079300130410     C                   parm                    keyVERSION
079400130410     C                   parm                    keyRELEASE
079500130410     C                   parm                    keyAGENCY
079600130410     C                   parm                    keyASSOCIA
079700130410      *
079800130410      *  Re-inizializza l'indicatore di trovato
079900130410     c                   setoff                                       32
080000130410      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
080100130410      *   da decodificare come UNB.
080200130410     c                   if        Ok_Nazione = 'S'
080300130410     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
080400130410     C                   eval      Nr_PT = 1
080500130410     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
080600130410     c                   Endif
080700130410      *
080800130410     c                   endSR
080900130410     C*----------------------------------------------------------------
081000130410      *? esito ricerca tabella PT decodificata oppure NO
081100130410     C*----------------------------------------------------------------
081200130410     C     esito_tab_PT  BEGSR
081300130410     C*
081400130410      *  INIZIALIZZA  dati fondamentali messaggio
081500130410     C                   clear                   EDIDSPT
081600130410     C                   clear                   EDIDSPU
081700130410     C                   clear                   EDIDSPV
081800130410     C                   clear                   EDIDSPZ
081900130410      *
082000130410     C                   move      'N'           PT_trovata
082100130410     C   32              move      'S'           PT_trovata
082200130410      *
082300130410      * ******  Errore generale sul Messaggio
082400130410      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
082500130410      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Brt.it
082600130410     C                   If        PT_trovata = 'N'
082700130410      * Msg di allerta Traduzione
082800130410     C                   EVAL      Indirizzi = CED_Bart
082900130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFTMBP - ORM'
083000130506      *
083100130410     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
083200130410     C                             IFTMBP D99B import O.R.M. EDI - messaggio -
083300130410     C                             con UNB non presente in tabella PT controlla-
083400130410     C                             re EDI ricevuto su UPLOAD.'
083500130410      *
083600130410     c                   exsr      invio_mail
083700130410     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
083800130410     C                   eval      vinFLG = '2'
083900130410     c                   eval      Almeno_Un_Due ='S'
084000130410     C                   eval      se_errore   = 'S'
084100130410      *
084200130410      *             *------------------------------*
084300130410     C                   eLSe
084400130410      *             *------------------------------*
084500130410     c                   exsr      Se_Trovata_PT
084600130410     c                   Endif
084700130410      *
084800130410     c                   endSR
084900130410     C*----------------------------------------------------------------
085000130410      *? DECODIFICA DATI UNB
085100130410     C*----------------------------------------------------------------
085200130410     C     Se_Trovata_PT BEGSR
085300130410      *
085400130410      * Se tutto OK su tab.PT:
085500130410     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
085600130410     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
085700130410     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
085800130410     C                   MOVEL     PZ_uni(Nr_PT) EDIDSPZ
085900130410      *
086000130410      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
086100130410      *  ricevuti:
086200130410     c                   movel     §PVema        mail_ind         44
086300130410      *
086400130410      * ?imposta gli indirizzi mail se interni a BRT o esterni
086500130410     c                   if        mail_ind <> *blank
086600130410     c                   exsr      imp_ADDR_mail
086700130410     c                   end
086800130410      *
086900130410     C                   move      §puTS         PU_TipoServ
087000130410     C                   move      §puSS         PU_SpecServ
087100130424     C                   move      §PUORMAKSC    PU_ORMAKSC
087200130424     C                   move      §PUORMACTR    PU_ORMACTR
087300130410      *
087400130411     C                   Z-ADD     §PTKSC        PT_cor
087500130411     C                   z-add     §PTLNP        PT_poe
087600130410      *
087700130410      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
087800130410     C                   clear                   ACORAG
087900130410     C                   clear                   INDCAE
088000130410     C                   clear                   INDCAP
088100130410     C                   clear                   INDSTA
088200130410      * Decodifico partner
088300130410     C                   Z-ADD     1             KKUT
088400130410     C                   Z-ADD     KCI           KKCC
088500130410     C                   MOVE      §PTKSC        KKSC
088600130410     C     KACO          CHAIN     CNACO00F                           31
088700130410     C     KIND          CHAIN     CNIND00F                           31
088800130410      *
088900130410     c                   endSR
089000130410     c*==================================================================*
089100130410      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
089200130410     c*==================================================================*
089300130410     c     Imp_ADDR_mail begsr
089400130410      *
089500130410     c                   clear                   Indirizzi
089600130410      *
089700130410      *  Lunghezza indirizzi presenti
089800130410     c                   eval      Lunghezza_Indirizzo =
089900130410     c                                       %Len(%TrimR(Mail_Ind))
090000130410     c                   z-add     1             al_char           3 0
090100130410     c                   z-add     1             dal_char          3 0
090200130410     c                   z-add     1             tot_char          3 0
090300130410      *
090400130410     c     successivo    tag
090500130410      *
090600130410      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
090700130410      *   e aggiunge il dominio Brt + il (;) separatore
090800130410     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
090900130410     c                   eval      tot_char = al_char - dal_char
091000130410     c                   z-add     dal_char      dalCarattere
091100130410     c                   z-add     tot_char      xTOTcaratteri
091200130410      *
091300130410     c                   clear                   Indir_BRT        40
091400130410     c                   clear                   Indir_Parz       20
091500130410     c                   eval      Indir_Parz =
091600130410     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
091700130410      *
091800130410     c                   eval      Indir_BRT    = %Trim(Indir_Parz) +
091900130410     c                             %Trim(bart_it)
092000130410     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_BRT
092100130410      *
092200130410     c                   if        al_char = Lunghezza_Indirizzo
092300130410     c                   goto      fine_indmail
092400130410     c                   else
092500130410     c                   eval      dal_char = ( al_char + 1 )
092600130410     c                   goto      successivo
092700130410     c                   end
092800130410      *
092900130410     C     fine_indmail  TAG
093000130410      *
093100130410     C                   ENDSR
093200130410     c*==================================================================*
093300130410      * Manda un Msg x E-mail
093400130410     c*==================================================================*
093500130410     c     Invio_Mail    begsr
093600130410      *
093700130410     C*   Alert_mail : invio Mail a CED@Brt.it                  *
093800130410     C* Inizializzo variabili
093900130410     C                   movel     *blanks       wrkEml          253
094000130410     C                   movel     *blanks       wrkMsg         5000
094100130410     C                   movel     *blanks       wrkOgg           44
094200130410      *
094300130410     C* Valorizzo i campi della e-m@ail - indirizzo
094400130410     C*******            eval      wrkEml = 'Andrea.Bertocchi@Brt.it'
094500130410     C                   eval      wrkEml = Indirizzi
094600130410     C                   eval      wrkOgg = Oggetto
094700130410     C                   eval      wrkMsg = Messaggio
094800130410      ** *****
094900130410     C                   call(e)   'TRTCT00R2'
095000130410     C                   parm                    wrkEml
095100130410     C                   parm                    wrkOgg
095200130410     C                   parm                    wrkMsg
095300130410     C*
095400130410     C                   ENDSR
095500090227      * ?================================================================== */
095600090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
095700090227      * ?================================================================== */
095800090227     C     DATI_UNH      BEGSR
095900100316      *
096000130410      * x singolo Dettaglio pulisce i campi
096100130410      **?              /*---------------------- */
096200130410     c                   exsr      INIT_dettaglio
096300130410      **?              /*---------------------- */
096400100701      *
096500090211      ** Riferimento del cliente
096600100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
096700100701      * ?                                       ------------
096800100701      * ?                                       ------------
096900100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
097000100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
097100100701     C                   SUB       1             lunghezza_num
097200100701     C                   EXSR      Se_Numerico
097300100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
097400090211      * ?                                       --------
097500090211      * ?                                       --------
097600090211     C                   END
097700090303      **
097800090210     c     end_UNH       endSR
097900130410      * ?================================================================== */
098000130410     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
098100130410      * ?================================================================== */
098200130410     C     INIT_dettaglioBEGSR
098300130410      **
098400130411      *  Pulizia dei records            *-----------------------------*
098500130411      * ?                               *-----------------------------*
098600130411     C                   CLEAR                   FnVAO000
098700130411      * ?                               *-----------------------------*
098800130411      *                                 *-----------------------------*
098900130410      **
099000130410      *   Inizia il conteggio delle righe e dei dettagli
099100130410     c                   z-add     1             Conta_segmenti    5 0
099200130410     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
099300130410      *
099400130410     C*  Inizializza variabili
099500130410      *   Codici identificativi della Testata del messaggio
099600130410     c                   clear                   UNH_Rifer_Msg
099700130410     c                   clear                   BGM_Id_Testata
099800130410      *
099900130410     ** Inizializza campo di work
100000130410     c                   eval      GID_aTotale = 'N'
100100130424     c                   move      *zeros        vaoCOR
100200130424     C                   movel     PT_cor        vaoCOR
100300130424      *** Cod.TASSAZZ su PT
100400130424     c                   if        PU_ORMAKSC = *blank
100500130424     C                   movel     PT_cor        vaoKSC
100600130424     c                   else
100700130424     C                   movel     PU_ORMAKSC    vaoKSC
100800130424     c                   end
100900130424      *** Cod.Tariffa su PT
101000130424     c                   if        PU_ORMACTR  <>*blank
101100130424     C                   movel     PU_ORMACTR    vaoCTr
101200130424     c                   end
101300130424      ***
101400130411     C                   z-add     PT_poe        vaoPOE
101500130422     C                   z-add     PT_poe        vaoPOC
101600130424     C* ?------------------
101700130424     c                   z-add     0             cod10n           10 0
101800130424     c                   z-add     0             cod07n            7 0
101900130424     c                   movel     vaoCOR        cod10n
102000130424     c                   movel     vaoCOR        cod07n
102100130424     c                   movel     'N'           ordinante_ok      1
102200130424      **
102300130424     C* ?Decodifica  Codice Ordinante su Anagrafica clienti di Ritiro
102400130424     C* ?------------------
102500130424      *?    DA SOSTITUIRE APPENA pronta utility per reperire da DPPDC00F
102600130424     C     cod10n        setll     Fnacr01l
102700130424     C                   read      Fnacr01l
102800130424      **
102900130424     c                   Dow       not %EoF(Fnacr01l)
103000130424     c                   movel     ACRCRO        par07n            7 0
103100130424      **
103200130424      **  Se diverso deve uscire immediatamente
103300130424     c                   If        par07n <> cod07n
103400130424     c                   leave
103500130424     c                   Else
103600130424      **  Se uguale invece deve essere non bloccato (M)
103700130424     c                   iF        ACRTCR <> 'M'
103800130424     c                   movel     'S'           ordinante_ok
103900130424     c                   leave
104000130424     c                   endiF
104100130424     c                   endIf
104200130424      **
104300130424     C                   read      Fnacr01l
104400130424     c                   endDO
104500130424     C* ?------------------
104600130424     c                   if        ordinante_ok <>'S'
104700130424     c                   z-add     1             KKUT
104800130424     c                   z-add     151           KKCC
104900130424     c                   z-add     Cod07n        KKSC
105000130424     C     KAco          chain     Cnaco00F
105100130424     c                   if        %found(Cnaco00F)
105200130424      ** se ha decodificato l'ordinante cerca sull'anagrafica dei ritiri
105300130424     c                   movel     'S'           ordinante_ok      1
105400130424     C                   End
105500130424     C                   End
105600130424      ** se non è codificato invia una mail di errore e salta
105700130424      ** il record
105800130424     c                   if        ordinante_ok <> 'S'
105900130424     C                   eval      vinMSG = 'UNB Ordinante NON CODIFICATO su -
106000130424     C                             FNACR00F '
106100130424     c                   exsr      Error_DET
106200130424      * ------
106300130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFTMBP - ORM'
106400130506      *
106500130424     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
106600130424???? C                             ORM IFTMBP NON CODIFICATO Partner Ordinante.-
106700130424     C                             Controllare UPLOAD del cliente:'
106800130424     C                   EVAL      Messaggio = %trim(Messaggio) +
106900130424     C                                         %editc(§PTksc:'X')
107000130424     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
107100130424     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
107200130424     c                   if        §PVinv <> 'N'
107300130424     c                   exsr      invio_mail
107400130424     c                   end
107500130424      *
107600130424     C                   End
107700130424     C* ?------------------
107800130411     C                   clear                   RFF_RifRitiro
107900130411     C                   clear                   Rifer_Alfanum    35
108000130411     C                   clear                   Rifer_Cliente    35
108100130411     C                   clear                   Rifer_Numeric    35
108200130411     C                   clear                   Tipo_Rifer        3
108300130410      *
108400130410     C*  Azzero codice cliente - tariffa
108500130411     C                   clear                   RFF_FF_ksc
108600130411     C                   clear                   RFF_FF_tar
108700130411     C                   clear                   RFF_FF_lnp
108800130411     C                   clear                   RFF_FF_nrs
108900130411     C                   clear                   RFF_FF_ctm
109000130411     C                   clear                   RFF_FF_bs1
109100130411     C                   clear                   RFF_FF_nsp                     *blk/S
109200130411     C                   clear                   RFF_FF_tic
109300130411     C*  Azzero codice cliente - tariffa
109400130411     C                   Z-ADD     0             ToT_Colli        16 0
109500130411     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
109600130411     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
109700130410      *
109800130410     C                   clear                   Note_1           35
109900130410     C                   clear                   Note_2           35
110000130410      *
110100130410      * Un errore nel dettaglio
110200130410     C                   clear                   Err_in_detta      1
110300130410     c                   clear                   errori_in_Wrt     1
110400130410      *
110500130410     c                   endSR
110600100701      * ?================================================================== */
110700100701      *? dati iniziali   BGM
110800100701      * ?================================================================== */
110900100701     C     DATI_BGM      BEGSR
111000100701      *
111100100701      *  identificativo testata
111200100706     c                   eval       BGM_Id_Testata = BGM1004
111300100701      **
111400100701     c                   endSR
111500130411      * ?================================================================== */
111600130411     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
111700130411      * ?================================================================== */
111800130411     C     DATI_DTM      BEGSR
111900130411      *
112000130411      *   Cerca la decodifica del dato in TABELLA
112100130411     C                   movel(p)  Tipo_Seg      etbSGM
112200130411     C                   movel(p)  dtm2005       etbVALSGM
112300130411     c                   exsr      In_TAB_EDSTBL
112400130411      *
112500130411     C*  Data Richiesta RITIRO
112600130411     C                   If        Quale_campo ='VAODAR'
112700130411     c                              and dtm2380 <> *blank
112800130411      **
112900130411     C                   MOVEL     dtm2380       Work_Data__35    35            Data
113000130411     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
113100130411     C                   EXSR      Converte_Data
113200130411     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
113300130411     C                   MOVEL     WCAMG         VAoDAR
113400130411     C                   MOVEL     WHMS          VAoORR
113500130422      ** imposta obbligatoriamente un'ora se non viene passata dal Partner
113600130422     c                   if        vaoORR = 0
113700130422     c                   z-add     1200          vaoORR
113800130422     c                   end
113900130411     C     WERRO         IFEQ      'S'
114000130411     C                   eval      vinMSG = 'DTM ( Data_Rich.Ritiro ' +
114100130411     C                             dtm2005  +
114200130411     C                              ') data errata'
114300130411     c                   exsr      Error_DET
114400130411     c                   goto      end_DTM
114500130411     C                   END
114600130411     C                   END
114700130411     C*
114800130411     C* Data Documento
114900130424     C****************   If        Quale_campo ='DATDOC'
115000130424     c****************              and dtm2380 <> *blank
115100130424      ****************
115200130424     C****************   MOVEL     dtm2380       Work_Data__35                  Data
115300130424     C****************   MOVEL     dtm2379       Work_Format_3                  Formato
115400130424     C****************   EXSR      Converte_Data
115500130424     C****************   MOVEL     WCAMG         vaodao                         Data
115600130424     C****************   MOVEL     WHMS          vaooao                         Ora
115700130424     c****************   if        vaooao = 0
115800130424     c****************   eval      vaooao = 1200
115900130424     c****************   end
116000130424      ****************
116100130424     C**** WERRO         IFEQ      'S'
116200130424     C****************   eval      vinMSG = 'DTM ( Data Documento ' +
116300130424     C****************             dtm2005  +
116400130424     C****************              ') data errata'
116500130424     c****************   exsr      Error_DET
116600130424     c****************   goto      end_DTM
116700130424     C****************   END
116800130424     C****************   END
116900130411      **
117000130411     c     end_DTM       endSR
117100130411      * ?================================================================== */
117200130411     C*? Imposta informazioni sul dettaglio del trasporto
117300130411      * ?================================================================== */
117400130411     C     DATI_TDT      BEGSR
117500130411     C*
117600130411      **
117700130411     c     end_TDT       endSR
117800130411      * ?================================================================== */
117900130411     C*?  Tipo Servizio
118000130411      * ?================================================================== */
118100130411     C     DATI_TSR      BEGSR
118200130411      ***
118300130411     C*  Reperisco priorità spedizione:
118400130411     C                   Z-ADD     1             X
118500130411      ***
118600130411     c                   movel(p)  TSR4219       Work_Data__35
118700130411     c                   move      PU_TipoServ   Work_Data__35
118800130411     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
118900130411     C     *IN32         IFEQ      '1'
119000130411      ***
119100130411      ***  non serve decodificare  il tipo di trasporto (è solo via camion)
119200130411      ***
119300130411     C                   ELSE
119400130411      **  Il tipo bolla non è presente
119500130411     C                   eval      vinMSG = 'TSR non in tabella'
119600130411     c*********          exsr      Error_DET
119700130411     c*********          goto      end_TSR
119800130411     C                   END
119900130411     C*
120000130411     C*--------------------------------
120100130411     C*  Decodifico servizi speciali
120200130411     C*--------------------------------
120300130411     C     TSR7085       IFNE      *BLANKS
120400130411     C     TSR7085       ANDNE     *LOVAL
120500130411      *
120600130411     C                   Z-ADD     1             X
120700130411     c                   movel(p)  TSR7085       Work_Data__35
120800130411     c                   move      PU_SpecServ   Work_Data__35
120900130411     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
121000130411     C     *IN32         IFEQ      '1'
121100130411     C                   eval       EDIDSSS = UNI_ServSpec(X)
121200130411      *
121300130411      *
121400130411      * x FERMO DEPOSITO  --> codice "GWC" - GOODS WAIT COLLECTION
121500130411     C     §SSTPS        IFEQ      'F'
121600130411     C                   MOVEL     'S'           VAoFFD
121700130411     C                   END
121800130411      *
121900130411     C                   ELSE
122000130411      *
122100130411      **  Il tipo bolla non è presente
122200130411     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
122300130411     c*******            exsr      Error_DET
122400130411     c*******            goto      end_TSR
122500130411      *
122600130411     C                   END
122700130411      *
122800130411     C                   END
122900130411      **
123000130411     c     end_TSR       endSR
123100130411      * ?================================================================== */
123200130411     C*? Imposta la Località
123300130411      * ?================================================================== */
123400130411     C     DATI_LOC      BEGSR
123500130411     C*
123600130411      **
123700130411     c     end_LOC       endSR
123800100701      * ?================================================================== */
123900100701     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
124000100701      * ?================================================================== */
124100100701     C     DATI_RFF      BEGSR
124200120424      *
124300130411      *   Cerca la decodifica del dato in TABELLA
124400120424     C                   movel(p)  Tipo_Seg      etbSGM
124500120424     C                   movel(p)  RFF1153       etbVALSGM
124600120424     c                   exsr      In_TAB_EDSTBL
124700100701      **
124800120629      * ?Codice Cliente di riferimento in Testata
124900130411     C                   if        RFF1153 = Riferimento_Cliente_FF
125000120629     C                              and RFF1154 <> *BLANKS
125100120629     c                               or
125200120629     C                             Quale_campo ='RFFCLI'  and
125300120629     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
125400120629      *>>>>>>>>
125500120629     C*------------------
125600120629     C                   movel     RFF1154       Rifer_Cliente
125700120629      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
125800120629     c                   exsr      CL_Particolare
125900120629     C*------------------
126000120629      *>>>>>>>>
126100120629     C                   ENDIF
126200100701      **
126300100701      * ?   se in Dettaglio   Riferimento singola spedizione
126400100701      *  Trascodifico riferimento spedizione
126500120424      **  o con altro riferimento diverso dall'AGE che indica la stessa cosa
126600130411     c                   if        RFF1153 = Riferimento_Ordine      and
126700100701     c                                RFF1154 <> *BLANKS
126800120424     c                               or
126900130411     C                             Quale_campo ='RFFCR '  and
127000120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
127100130411      * Solo il primo riferimento
127200130411     C                   if           Rifer_Alfanum = *blank
127300130411     C                   movel(p)  RFF1154       Rifer_Alfanum
127400130411     C                   movel(p)  RFF1154       VAoRFA
127500130411     c                   end
127600100701     c                   end
127700100701      *
127800100701     c     end_RFF       endSR
127900130411      * ?------------------------------------------------------------------ */
128000130411     C*  Ricerca dell RFF+FF: su tabella CL per impostare altro codice Cliente
128100130411      * ?------------------------------------------------------------------ */
128200130411     C     CL_ParticolarebegSR
128300130411      *
128400130411     C     Rifer_Cliente IFNE      *BLANKS
128500130411      * ------>>>
128600130411     C                   eval      XX = 1
128700130411     C                   movel     §PTKSC        Key_CL           35
128800130411     C                   movel     Rifer_Cliente WCOM28           28
128900130411     C                   move      WCOM28        Key_CL
129000130411      *
129100130411     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
129200130411     C     *IN34         IFEQ      '1'
129300130411     C                   movel     Des_Tb_CL(XX) EDIDSCL
129400130411      *
129500130411      * in dettaglio Messaggio
129600130411     C                   z-add     §CLksc        RFF_FF_ksc        7 0
129700130411     C                   movel     §CLtar        RFF_FF_tar        3
129800130411     C                   z-add     §CLlnp        RFF_FF_lnp        3 0
129900130411     C                   z-add     §CLNRS        RFF_FF_nrs        2 0
130000130411     C                   movel     §CLCTM        RFF_FF_ctm        2
130100130411     C                   movel     §CLBS1        RFF_FF_bs1        1
130200130411     C                   movel     §CLnsp        RFF_FF_nsp        1            *blk/S
130300130411     C                   movel     §CLtic        RFF_FF_tic        2
130400130411     c                   end
130500130411      *
130600130411     C                   ENDIF
130700130411      * ------>>>
130800130411      * in Dettaglio  preimposto i campi rilevati dalle tabelle
130900130411      *   per clienti particolari (CL)
131000130411      *   se aveva un RFF+FF in dettaglio
131100130411     C                   if          RFF_FF_ksc > 0
131200130424     C* ?------------------
131300130424     c                   z-add     0             cod10n           10 0
131400130424     c                   z-add     0             cod07n            7 0
131500130424     c                   movel     RFF_FF_ksc    cod10n
131600130424     c                   movel     RFF_FF_ksc    cod07n
131700130424     c                   movel     'N'           ordinante_okFF    1
131800130424      **
131900130424     C* ?Decodifica  Codice Ordinante su Anagrafica clienti di Ritiro
132000130424     C* ?------------------
132100130424      *?    DA SOSTITUIRE APPENA pronta utility per reperire da DPPDC00F
132200130424     C     cod10n        setll     Fnacr01l
132300130424     C                   read      Fnacr01l
132400130424      **
132500130424     c                   Dow       not %EoF(Fnacr01l)
132600130424     c                   movel     ACRCRO        par07n            7 0
132700130424      **
132800130424      **  Se diverso deve uscire immediatamente
132900130424     c                   If        par07n <> cod07n
133000130424     c                   leave
133100130424     c                   Else
133200130424      **  Se uguale invece deve essere non bloccato (M)
133300130424     c                   iF        ACRTCR <> 'M'
133400130424     c                   movel     'S'           ordinante_okFF
133500130424     c                   leave
133600130424     c                   endiF
133700130424     c                   endIf
133800130424      **
133900130424     C                   read      Fnacr01l
134000130424     c                   endDO
134100130424     C* ?------------------
134200130424     c                   if        ordinante_okFF <>'S'
134300130424      ** se NON ha decodificato l'ordinante
134400130424     c                   z-add     1             KKUT
134500130424     c                   z-add     151           KKCC
134600130424     c                   z-add     Cod07n        KKSC
134700130424     C     KAco          chain     Cnaco00F
134800130424     c                   if        %found(Cnaco00F)
134900130424     c                   movel     'S'           ordinante_okFF    1
135000130424     C                   End
135100130424     C                   End
135200130424      ** se non è codificato invia una mail di errore e salta
135300130424      ** il record
135400130424     c                   if        ordinante_ok = 'S'
135500130424     c                   move      *zeros        vaoCOR
135600130424     C                   movel     RFF_FF_ksc    vaoCOR
135700130424     C                   movel     RFF_FF_lnp    VAoPOE
135800130424     C                   movel     RFF_FF_lnp    VAoPOC
135900130424     C                   End
136000130424     C* ?------------------
136100130411      *
136200130411     c                   end
136300130411     C*
136400130411     C                   ENDSR
136500100705      * ?================================================================== */
136600100705     C*?  FREE TEXT
136700100705      * ?================================================================== */
136800100705     C     DATI_FTX      BEGSR
136900100705      *
137000100705      *   Cerca la decodifica del dato in TABELLA
137100120424     C                   movel(p)  Tipo_Seg      etbSGM
137200100705     C                   movel(p)  ftx4451       etbVALSGM
137300100705     c                   exsr      In_TAB_EDSTBL
137400100705      *
137500130411      * ? solo se si tratta di note descrittive da comunicare
137600130411     c                   if         quale_campo = 'VAONOT'
137700100705     C                   DO        4             X
137800100705      *
137900100705     C     D05(X)        IFNE      *BLANKS
138000100705      *
138100100705     C     Note_1        IFEQ      *BLANKS
138200100705     C                   MOVEL     D05(X)        Note_1
138300100705     C                   MOVE      D05(X)        Note_2
138400100705     C                   ELSE
138500100705     C     Note_2        IFEQ      *BLANKS
138600100705     C                   MOVEL     D05(X)        Note_2
138700100705     C                   END
138800100705     C                   END
138900100705      *
139000100705     C                   ENDif
139100100705      *
139200100705     C                   ENDdo
139300100705     C                   end
139400110907     C*------>>
139500130411      * ?Se Natura Merce
139600130411     c                   if         quale_campo = 'VABNAS'
139700130411     C                   MOVEL     D05(1)        VAoNAM
139800100705     c                   End
139900100705      *
140000130411     C*------>>
140100120613      * ?Consegne Particolari
140200120613      *
140300100705     c     end_FTX       endSR
140400090213      * ?================================================================== */
140500090213     C*?  Anagrafica del mittente e del destinatario
140600090213      * ?================================================================== */
140700091016     C     DATI_NAD      BEGSR
140800130411      *
140900130411      *   per sapere quale è stato decodificato
141000130411     c                   eval      NAD_Destinatario  = *blank
141100130411     c                   eval      NAD_Mittente      = *blank
141200130411     c                   eval      NAD_Ordinante     = *blank
141300100705      *
141400100705      *   Cerca la decodifica del dato in TABELLA
141500120424     C                   movel(p)  Tipo_Seg      etbSGM
141600100705     C                   movel(p)  nad3035       etbVALSGM
141700100705     c                   exsr      In_TAB_EDSTBL
141800130410      *-------
141900130410      * Dati Ordinante
142000100708      *-------
142100130411     C                   IF        Quale_campo = NAD_HI
142200130411     c                   eval       NAD_Ordinante  ='S'
142300130422      *  N O N
142400130411     C* Imposto dati Ordinante
142500130422      *  perchè avendo messo il CODICE ORDINANTE (preso dalla tabella PT)
142600130422      *   i campi descrittivi l'ordinante non devono essere riempiti
142700130424      *
142800130422     C*************      MOVEL     nad30361      VAoRSO
142900130422     C*************      if        nad30362 <> *LOVAL
143000130422     c*************      eval      VAoRSO = %Trim(VAoRSO) +' '+ nad30362
143100130422     C*************      endif
143200130422      *************
143300130422???? c*************      if        VAoRSO = *blank
143400130422???? C*************      MOVEL     nad31241      VAoRSO
143500130422???? C*************      if        nad31242 <> *LOVAL
143600130422     c*************      eval      VAoRSO = %Trim(VAoRSO) +' '+ nad31242
143700130422???? C*************      endif
143800130422     c*************      end
143900130422      *************
144000130411      *   Indirizzo  città/Località
144100130422     C*************      MOVEL     nad30421      VAoINO
144200130422     C*************      MOVEL     nad3164       VAoLOO
144300130422      *************
144400130411     C* Reperisco nazione corrispondente destinatario
144500130422     C*************      Z-ADD     1             naz               3 0
144600130422     C*************      MOVEL     *BLANKS       VAoNAO
144700130422     C*************      Z-ADD     0             WFLCPF
144800130422     c*************      eval      *in32 = *off
144900130422     C*************      if        nad3207 <> *BLANKS
145000130422     C***  nad3207       LOOKUP    Iso03(naz)                             32
145100130422     C*************      If        *in32 = *on
145200130411      * per reperire il CAP corretto
145300130422     C*************      MOVEL     Naz03(naz)    VAoNAO
145400130422     C*************      MOVEL     cpf03(naz)    WFLCPF            2 0
145500130422     c*************      Endif
145600130422     C*************      endif
145700130422      *************
145800130422     c*************      eval      NAD_Lunghezza_Indir_Aggiuntivo =
145900130422     c*************                            %len(%trim(NAD30422))
146000130422???? c*************      eval      NAD_Lunghezza_Indirizzo =
146100130422     c*************                            %len(%trim(VAoINO))
146200130422???? c*************      eval      NAD_Lunghezza_ragSociale =
146300130422     c*************                            %len(%trim(VAoRSO))
146400130422      *************
146500130411      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
146600130411      *  lo aggiunge nel proseguimento della Ragione Sociale
146700130422     c*************      if        NAD_Lunghezza_Indir_Aggiuntivo > 0
146800130422      *************
146900130422     c*************                and  %len(VAoINO) <
147000130422     c*************                     NAD_Lunghezza_Indirizzo +
147100130422     c*************                     NAD_Lunghezza_Indir_Aggiuntivo
147200130411      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
147300130411      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
147400130422     c*************                and  %len(VAoRSO) -
147500130422     c*************                     NAD_Lunghezza_ragSociale -
147600130422     c*************                     NAD_Lunghezza_Indir_Aggiuntivo
147700130422     c*************                     >= 0
147800130411      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
147900130422     c*************                and  NAD_Lunghezza_ragSociale <
148000130422     c*************                     NAD_Lunghezza_Indirizzo
148100130422      *************
148200130422     C***  '    '        SCAN      VAoRSO        WI
148300130422     c*************      if        %Found
148400130422     C*************      MOVEA     VAoRSO        LOD
148500130422     C*************      ADD       1             WI
148600130422     C*************      MOVEA     NAD30422      LOD(WI)
148700130422     C*************      MOVEA     LOD           VAoRSO
148800130422     c*************      end
148900130422      *************
149000130411      * Altrimenti lo aggiunge direttamente sulla VIA
149100130422     c*************      else
149200130422      *************
149300130411      * Se NAD30422  non è vuoto
149400130411      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
149500130422      *************
149600130411     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
149700130422     c*************      if        NAD30422 <> *blanks and NAD30422 <> *loval
149800130411     C* Verifico se nella località c'è dello spazio
149900130422     C***  '    '        SCAN      VAoINO        WI
150000130422     c*************      if        %Found
150100130422     C*************      MOVEA     VAoINO        LOD
150200130422     C*************      ADD       1             WI
150300130422     C*************      MOVEA     NAD30422      LOD(WI)
150400130422     C*************      MOVEA     LOD           VAoINO
150500130422     c*************      else
150600130411      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
150700130422     C***  '    '        SCAN      VAoLOO        WI                4 0
150800130422     c*************      if        %Found
150900130422     C*************      MOVEA     VAoLOO        LOD
151000130422     C*************      ADD       1             WI
151100130422     C*************      MOVEA     NAD30422      LOD(WI)
151200130422     C*************      MOVEA     LOD           VAoLOO
151300130422     C*************      ENDif
151400130422     C*************      endif
151500130422     C*************      ENDif
151600130411      *-------
151700130422     c*************      end
151800130422      *************
151900130411      * Converto CAP
152000130422     C*************      EXSR      Converte_CAP
152100130422     c*************      eval      VAoCAO = WCAP
152200130422      *************
152300130411     C* CAP OBBLIGATORIO : CONTROLLO
152400130422     C*************      CLEAR                   DSSI95
152500130422     C*************      CLEAR                   DSLV13
152600130422     C*************      MOVEL     '3'           I95TCN
152700130422     C*************      MOVEL     VAoCAO        I95CAP
152800130422     C*************      MOVEL     VAoLOO        I95LOC
152900130411     C* ERRORE AFFIDABILITA = 0
153000130422     C*************      MOVEL     'S'           I13AF0
153100130422     C*************      MOVEL     'S'           I13AF1
153200130422     C*************      z-add     1             E13FZ1
153300130424     c*************      move      kpjbu         SvKpjbu
153400130424     C*************      clear                   KPJBU
153500130422     C*************      CALL      'FNLV13R'
153600130422     C*************      PARM                    KPJBA
153700130422     C*************      PARM                    DSLV13
153800130422     C*************      PARM                    DSSI95
153900130424     c*************      eval      kpjbu   =     SvKpjbu
154000130411     C* ERRORE
154100130422     C***  O13ERR        IFEQ      *BLANKS
154200130422     C*************      MOVEL     O95PRV        VAoPRO
154300130422    3C*************      ENDIF
154400130422     C*************
154500130411     C*    SEGNALA
154600130422     C*************      if          vaoRSO = *blank
154700130422     C*************      eval      vinMSG='NAD Ragione Soc.Ordinante assente'
154800130411     c******             exsr      Error_DET
154900130411     c******             goto      end_NAD
155000130422     C*************      elseIF      vaoINO = *blank
155100130422     C*************      eval      vinMSG = 'NAD Indirizzo Ordinante assente'
155200130411     c*********          exsr      Error_DET
155300130411     c*********          goto      end_NAD
155400130422     C*************      elseIF      vaoLOO = *blank
155500130422     C*************      eval      vinMSG = 'NAD Località Ordinante assente'
155600130411     c*********          exsr      Error_DET
155700130411     c*********          goto      end_NAD
155800130422     C*************      elseIF      vaoCAO = *blank
155900130422     C*************      eval      vinMSG = 'MEA (CAP) non trovato'
156000130411     c*********          exsr      Error_DET
156100130411     C*********          goto      end_MEA
156200130422     C*************      ENDIF
156300120629      *-------
156400130410      *-------
156500130411      * Dati Mittente
156600130410      *-------
156700130411     C                   elseIF    Quale_campo = NAD_CZ
156800130411     c                   eval       NAD_Mittente   ='S'
156900130411     C* Imposto dati Mittente
157000130411     C                   MOVEL     nad30361      VAoRSR
157100130411     C                   if        nad30362 <> *LOVAL
157200130411     c                   eval      VAoRSR = %Trim(VAoRSR) +' '+ nad30362
157300130411     C                   endif
157400130411      *
157500130411???? c                   if        VAoRSR = *blank
157600130411???? C                   MOVEL     nad31241      VAoRSR
157700130411???? C                   if        nad31242 <> *LOVAL
157800130411     c                   eval      VAoRSR = %Trim(VAoRSR) +' '+ nad31242
157900130411???? C                   endif
158000130411     c                   end
158100130411      *
158200130411      *   Indirizzo  città/Località
158300130411     C                   MOVEL     nad30421      VAoINR
158400130411     C                   MOVEL     nad3164       VAoLOR
158500130411     C*
158600130411     C* Reperisco nazione corrispondente destinatario
158700130411     C                   Z-ADD     1             naz               3 0
158800130411     C                   MOVEL     *BLANKS       VAoNAR
158900130411     C                   Z-ADD     0             WFLCPF
159000130411     c                   eval      *in32 = *off
159100130411     C                   if        nad3207 <> *BLANKS
159200130411     C     nad3207       LOOKUP    Iso03(naz)                             32
159300130411     C                   If        *in32 = *on
159400130411      * per reperire il CAP corretto
159500130411     C                   MOVEL     Naz03(naz)    VAoNAR
159600130411     C                   MOVEL     cpf03(naz)    WFLCPF            2 0
159700130411     c                   Endif
159800130411     C                   endif
159900130411      *
160000130411     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
160100130411     c                                         %len(%trim(NAD30422))
160200130411???? c                   eval      NAD_Lunghezza_Indirizzo =
160300130411     c                                         %len(%trim(VAoINR))
160400130411???? c                   eval      NAD_Lunghezza_ragSociale =
160500130411     c                                         %len(%trim(VAoRSR))
160600130411      *
160700130411      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
160800130411      *  lo aggiunge nel proseguimento della Ragione Sociale
160900130411     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
161000130411      **
161100130411     c                             and  %len(VAoINR) <
161200130411     c                                  NAD_Lunghezza_Indirizzo +
161300130411     c                                  NAD_Lunghezza_Indir_Aggiuntivo
161400130411      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
161500130411      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
161600130411     c                             and  %len(VAoRSR) -
161700130411     c                                  NAD_Lunghezza_ragSociale -
161800130411     c                                  NAD_Lunghezza_Indir_Aggiuntivo
161900130411     c                                  >= 0
162000130411      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
162100130411     c                             and  NAD_Lunghezza_ragSociale <
162200130411     c                                  NAD_Lunghezza_Indirizzo
162300130411      *
162400130411     C     '    '        SCAN      VAoRSR        WI
162500130411     c                   if        %Found
162600130411     C                   MOVEA     VAoRSR        LOD
162700130411     C                   ADD       1             WI
162800130411     C                   MOVEA     NAD30422      LOD(WI)
162900130411     C                   MOVEA     LOD           VAoRSR
163000130411     c                   end
163100130411      *
163200130411      * Altrimenti lo aggiunge direttamente sulla VIA
163300130411     c                   else
163400130411      *
163500130411      * Se NAD30422  non è vuoto
163600130411      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
163700130411      *
163800130411     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
163900130411     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
164000130411     C* Verifico se nella località c'è dello spazio
164100130411     C     '    '        SCAN      VAoINR        WI
164200130411     c                   if        %Found
164300130411     C                   MOVEA     VAoINR        LOD
164400130411     C                   ADD       1             WI
164500130411     C                   MOVEA     NAD30422      LOD(WI)
164600130411     C                   MOVEA     LOD           VAoINR
164700130411     c                   else
164800130411      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
164900130411     C     '    '        SCAN      VAoLOR        WI                4 0
165000130411     c                   if        %Found
165100130411     C                   MOVEA     VAoLOR        LOD
165200130411     C                   ADD       1             WI
165300130411     C                   MOVEA     NAD30422      LOD(WI)
165400130411     C                   MOVEA     LOD           VAoLOR
165500130411     C                   ENDif
165600130411     C                   endif
165700130411     C                   ENDif
165800130411      *-------
165900130411     c                   end
166000130411      * Converto CAP
166100130411     C                   EXSR      Converte_CAP
166200130411     c                   eval      VAoCAR = WCAP
166300130411      *
166400130411     C* CAP OBBLIGATORIO : CONTROLLO
166500130411     C                   CLEAR                   DSSI95
166600130411     C                   CLEAR                   DSLV13
166700130411     C                   MOVEL     '3'           I95TCN
166800130411     C                   MOVEL     vaocar        I95CAP
166900130411     C                   MOVEL     vaolor        I95LOC
167000130411     C* ERRORE AFFIDABILITA = 0
167100130411     C                   MOVEL     'S'           I13AF0
167200130411     C                   MOVEL     'S'           I13AF1
167300130411     C                   z-add     1             E13FZ1
167400130424     c                   move      kpjbu         SvKpjbu
167500130424     C                   clear                   KPJBU
167600130411     C                   CALL      'FNLV13R'
167700130411     C                   PARM                    KPJBA
167800130411     C                   PARM                    DSLV13
167900130411     C                   PARM                    DSSI95
168000130424     c                   eval      kpjbu   =     SvKpjbu
168100130411     C* ERRORE
168200130411     C     O13ERR        IFEQ      *BLANKS
168300130411     C                   MOVEL     O95PRV        VAoPRR
168400130411    3C                   ENDIF
168500130411     C*
168600130411     C*    SEGNALA
168700130411     C                   if          vaoRSR = *blank
168800130411     C                   eval      vinMSG='NAD Ragione Soc.Mittente assente'
168900130411     c******             exsr      Error_DET
169000130411     c******             goto      end_NAD
169100130411     C                   elseIF      vaoINR = *blank
169200130411     C                   eval      vinMSG = 'NAD Indirizzo Mittente assente'
169300130411     c*********          exsr      Error_DET
169400130411     c*********          goto      end_NAD
169500130411     C                   elseIF      vaoLOR = *blank
169600130411     C                   eval      vinMSG = 'NAD Località Mittente assente'
169700130411     c*********          exsr      Error_DET
169800130411     c*********          goto      end_NAD
169900130411     C                   elseIF      vaoCAR = *blank
170000130411     C                   eval      vinMSG = 'MEA (CAP) non trovato'
170100130411     c*********          exsr      Error_DET
170200130411     C*********          goto      end_MEA
170300130411     C                   ENDIF
170400100705      *-------
170500130422      *-------
170600090210      * Dati destinatario
170700100705      *-------
170800130411     C                   elseIF    Quale_campo =  NAD_CN
170900130411     c                   eval       NAD_Destinatario ='S'
171000130411      *
171100130411     C* Imposto dati destinatario
171200130411     C                   MOVEL     nad30361      VAoRSC
171300130411     C                   if        nad30362 <> *LOVAL
171400130411     c                   eval      VAoRSC = %Trim(VAoRSC) +' '+ nad30362
171500130411     C                   endif
171600130411      *
171700130411???? c                   if        VAoRSC = *blank
171800130411???? C                   MOVEL     nad31241      VAoRSC
171900130411???? C                   if        nad31242 <> *LOVAL
172000130411     c                   eval      VAoRSC = %Trim(VAoRSC) +' '+ nad31242
172100130411???? C                   endif
172200130411     c                   end
172300130411      *
172400130411      *   Indirizzo  città/Località
172500130411     C                   MOVEL     nad30421      VAoINC
172600130411     C                   MOVEL     nad3164       VAoLOC
172700090210      *
172800090210     C* Reperisco nazione corrispondente destinatario
172900100705     C                   Z-ADD     1             naz               3 0
173000130411     C                   MOVEL     *BLANKS       VAoNAC
173100100705     C                   Z-ADD     0             WFLCPF
173200090210     c                   eval      *in32 = *off
173300090210     C                   if        nad3207 <> *BLANKS
173400130410     C     nad3207       LOOKUP    Iso03(naz)                             32
173500100705     C                   If        *in32 = *on
173600100705      * per reperire il CAP corretto
173700130411     C                   MOVEL     Naz03(naz)    VAoNAC
173800130410     C                   MOVEL     cpf03(naz)    WFLCPF            2 0
173900100705     c                   Endif
174000100705     C                   endif
174100111121      *
174200111121     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
174300111121     c                                         %len(%trim(NAD30422))
174400111121???? c                   eval      NAD_Lunghezza_Indirizzo =
174500130411     c                                         %len(%trim(VAoINC))
174600130411???? c                   eval      NAD_Lunghezza_ragSociale =
174700130411     c                                         %len(%trim(VAoRSC))
174800111116      *
174900111121      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
175000111121      *  lo aggiunge nel proseguimento della Ragione Sociale
175100111121     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
175200111121      **
175300130411     c                             and  %len(VAoINC) <
175400111121     c                                  NAD_Lunghezza_Indirizzo +
175500111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
175600111121      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
175700111121      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
175800130411     c                             and  %len(VAoRSC) -
175900130411     c                                  NAD_Lunghezza_ragSociale -
176000111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
176100111121     c                                  >= 0
176200111121      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
176300130411     c                             and  NAD_Lunghezza_ragSociale <
176400111121     c                                  NAD_Lunghezza_Indirizzo
176500111121      *
176600130411     C     '    '        SCAN      VAoRSC        WI
176700111121     c                   if        %Found
176800130411     C                   MOVEA     VAoRSC        LOD
176900111121     C                   ADD       1             WI
177000111121     C                   MOVEA     NAD30422      LOD(WI)
177100130411     C                   MOVEA     LOD           VAoRSC
177200111121     c                   end
177300111121      *
177400111121      * Altrimenti lo aggiunge direttamente sulla VIA
177500111121     c                   else
177600130411      *
177700100705      * Se NAD30422  non è vuoto
177800111116      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
177900111116      *
178000111116     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
178100090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
178200100705     C* Verifico se nella località c'è dello spazio
178300130411     C     '    '        SCAN      VAoINC        WI
178400100705     c                   if        %Found
178500130411     C                   MOVEA     VAoINC        LOD
178600090210     C                   ADD       1             WI
178700090210     C                   MOVEA     NAD30422      LOD(WI)
178800130411     C                   MOVEA     LOD           VAoINC
178900100705     c                   else
179000111121      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
179100130411     C     '    '        SCAN      VAoLOC        WI                4 0
179200100705     c                   if        %Found
179300130411     C                   MOVEA     VAoLOC        LOD
179400100705     C                   ADD       1             WI
179500100705     C                   MOVEA     NAD30422      LOD(WI)
179600130411     C                   MOVEA     LOD           VAoLOC
179700100705     C                   ENDif
179800130411     C                   endif
179900100705     C                   ENDif
180000130411      *-------
180100111121     c                   end
180200111116      *
180300090210      * Converto CAP
180400100705     C                   EXSR      Converte_CAP
180500130411     c                   eval      VAoCAC = WCAP
180600130411      *
180700130411     C* CAP OBBLIGATORIO : CONTROLLO
180800130411     C                   CLEAR                   DSSI95
180900130411     C                   CLEAR                   DSLV13
181000130411     C                   MOVEL     '3'           I95TCN
181100130411     C                   MOVEL     VAoCAC        I95CAP
181200130411     C                   MOVEL     VAoLOC        I95LOC
181300130411     C* ERRORE AFFIDABILITA = 0
181400130411     C                   MOVEL     'S'           I13AF0
181500130411     C                   MOVEL     'S'           I13AF1
181600130411     C                   z-add     1             E13FZ1
181700130424     c                   move      kpjbu         SvKpjbu
181800130424     C                   clear                   KPJBU
181900130411     C                   CALL      'FNLV13R'
182000130411     C                   PARM                    KPJBA
182100130411     C                   PARM                    DSLV13
182200130411     C                   PARM                    DSSI95
182300130424     c                   eval      kpjbu     =   SvKpjbu
182400130411     C* ERRORE
182500130411     C     O13ERR        IFEQ      *BLANKS
182600130411     C                   MOVEL     O95PRV        VAoPRC
182700130411    3C                   ENDIF
182800130411     C*
182900130411     C*    SEGNALA
183000130411     C                   if          vaoRSC = *blank
183100100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
183200130315     c******             exsr      Error_DET
183300130315     c******             goto      end_NAD
183400130411     C                   elseIF      vaoINC = *blank
183500100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
183600101006     c*********          exsr      Error_DET
183700101006     c*********          goto      end_NAD
183800130411     C                   elseIF      vaoLOC = *blank
183900100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
184000101006     c*********          exsr      Error_DET
184100101006     c*********          goto      end_NAD
184200130411     C                   elseIF      vaoCAC = *blank
184300130411     C                   eval      vinMSG = 'MEA (CAP) non trovato'
184400130411     c*********          exsr      Error_DET
184500130411     C*********          goto      end_MEA
184600100728     C                   ENDIF
184700090210      *
184800090210     C                   END
184900090210      *
185000090210     c     end_NAD       endSR
185100090210      * ?================================================================== */
185200090210     C*?  Contatto a cui far riferimento
185300090210      * ?================================================================== */
185400090210     C     DATI_CTA      BEGSR
185500130411      *
185600130411     C     cta3412       ifne      *BLANKS
185700130411      *
185800130411     c                   if        NAD_Destinatario  = 'S'
185900130411     c                   elseif    NAD_Mittente      = 'S'
186000130411     c                   eval          VAoRER = cta3412
186100130411     c                   elseif    NAD_Ordinante     = 'S'
186200130411     c                   end
186300090210      *
186400100701     c                   end
186500090210      *
186600090210     c                   endSR
186700090210      * ?================================================================== */
186800090210     C*?  Contatto telefonico  a cui far riferimento
186900090210      * ?================================================================== */
187000090210     C     DATI_COM      BEGSR
187100090210      *
187200100705      *  memorizza il riferimento di consegna nr.telefonico
187300100705     C                   if        com3148 <> *BLANKS    and
187400100705     C                              com3155 = Telephone_Number
187500130411      *
187600130411     c                   if        NAD_Destinatario  = 'S'
187700130411     c                   elseif    NAD_Mittente      = 'S'
187800130411     c                   eval          VAoTER = com3148
187900130411     c                   elseif    NAD_Ordinante     = 'S'
188000130411     c                   end
188100130411      *
188200130411     c                   end
188300090210      *
188400090209     C                   ENDSR
188500090210      * ?================================================================== */
188600090210     C*?  Dati di dettaglio della spedizione e dei colli
188700090210      * ?================================================================== */
188800090210     C     DATI_GID      BEGSR
188900090210      *
189000130411     C* Se ho totali per spedizione metto nei campi
189100090210     C     gid1496       IFEQ      99999
189200090210     C     gid1496       orEQ      9999
189300100630     c                   eval      GID_aTotale = 'S'
189400130411     C                   z-add     gid722420     VAoNCL
189500090210     C                   ELSE
189600130411      *
189700100630     c                   If        GID_aTotale = 'N'
189800100705     C                   z-add     gid722420     GID_ColliInAdd
189900130411     C                   eval      VAoNCL = VAoNCL + GID_ColliInAdd
190000090210     C                   END
190100090210     C                   END
190200090212      *
190300130411     C                   ADD       VAoNCL        ToT_Colli        16 0
190400090210      *
190500090210     c     end_GID       endSR
190600090210      * ?================================================================== */
190700090210     C*?  Dati di dettaglio misure spedizione e colli
190800090210      * ?================================================================== */
190900090210     C     DATI_MEA      BEGSR
191000090210      *
191100090210     C*  Peso
191200100319     C     mea6311       IFEQ      Weight
191300100319     C     mea6411       ANDEQ     Kilograms
191400090210      *
191500090210     C*  Controllo se ho valore totale o parziale
191600090210      *   sempre il Lordo
191700100319???? C     mea6313       IFEQ      G_Weight
191800100319???? C     mea6313       oreq      Gross_Weight
191900100706???? C     mea6313       oreq      Gross_Weight_E
192000110512???? C     mea6313       oreq      Gross_Weight_D
192100101027???? C     mea6313       oreq      *blank
192200100630     C                   IF        GID_aTotale = 'S'
192300091019???? C                   move      mea6314       mea6314_2        18 2          Peso
192400130411???? C                   Z-ADD     mea6314_2     VAoPKg                         Peso
192500090210     C                   ELSE
192600091019???? C                   move      mea6314       mea6314_2                      Peso
192700130411???? C                   ADD       mea6314_2     VAoPKg                         Peso
192800090210     C                   END
192900091203     C                   END
193000090210      *
193100100319???? C     mea6313       IFEQ      G_Weight
193200100319???? C     mea6313       oreq      Gross_Weight
193300100706???? C     mea6313       oreq      Gross_Weight_E
193400110512???? C     mea6313       oreq      Gross_Weight_D
193500101027???? C     mea6313       oreq      *blank
193600091019???? C                   move      mea6314       mea6314_2                      Peso
193700100701???? C                   ADD       mea6314_2     ToT_PesoLordo
193800090210     C                   END
193900100706???? C     mea6313       IFEQ      N_Weight
194000100706???? C     mea6313       oreq      Net_Weight
194100100706???? C     mea6313       oreq      Gross_Weight_E
194200110512???? C     mea6313       oreq      Gross_Weight_D
194300101027???? C     mea6313       oreq      *blank
194400091019???? C                   move      mea6314       mea6314_2                      Peso
194500100701???? C                   ADD       mea6314_2     ToT_PesoNetto
194600090210     C                   END
194700090210     C                   END
194800090210      ***
194900090210     C*  Peso in Grammi  se abilitato a utilizzarlo
195000090210      ***
195100100319     C     mea6311       IFEQ      Weight
195200100319     C     mea6411       ANDEQ     Grams
195300090210      *
195400090210     C*  Controllo se ho valore totale o parziale
195500100319???? C     mea6313       IFEQ      G_Weight
195600100319???? C     mea6313       oreq      Gross_Weight
195700100706???? C     mea6313       oreq      Gross_Weight_E
195800110512???? C     mea6313       oreq      Gross_Weight_D
195900101027???? C     mea6313       oreq      *blank
196000100630     c                   If        GID_aTotale = 'S'
196100091019???? c                   move      mea6314       mea6314_2
196200130411???? C     mea6314_2     div       1000          VAoPKg                         Peso
196300090210     C                   ELSE
196400091019???? c                   move      mea6314       mea6314_2
196500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
196600130411???? C                   add       Peso_inKG     VAoPKg                         Peso
196700090210     C                   END
196800090210     C                   END
196900090210      *
197000100319???? C     mea6313       IFEQ      G_Weight
197100100319???? C     mea6313       oreq      Gross_Weight
197200100706???? C     mea6313       oreq      Gross_Weight_E
197300110512???? C     mea6313       oreq      Gross_Weight_D
197400101027???? C     mea6313       oreq      *blank
197500091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
197600100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
197700090210     C                   END
197800100706???? C     mea6313       IFEQ      N_Weight
197900100706???? C     mea6313       oreq      Net_Weight
198000100706???? C     mea6313       oreq      Gross_Weight_E
198100110512???? C     mea6313       oreq      Gross_Weight_D
198200101027???? C     mea6313       oreq      *blank
198300091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
198400100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
198500090210     C                   END
198600090210     C                   END
198700090210      *  Volume
198800100319???? C     mea6311       IFEQ      Volume
198900100319???? C     mea6411       ANDEQ     Cubic_Meters
199000090210      *  Controllo se ho valore totale o parziale
199100100630     c                   If        GID_aTotale = 'S'
199200091019???? C                   move      mea6314       mea6314_2        18 2          Volume
199300130411???? C                   Z-ADD     mea6314_2     VAoVLm                         Volume
199400090210     C                   ELSE
199500091019???? C                   move      mea6314       mea6314_2        18 2          Volume
199600130411???? C                   ADD       mea6314_2     VAoVLm                         Volume
199700090210     C                   END
199800090210     C                   END
199900100705      *
200000090210     c     end_MEA       endSR
200100090211     C*----------------------------------------------------------------
200200090211      *? dati finali del dettaglio    UNT
200300090211     C*----------------------------------------------------------------
200400090211     C     DATI_UNT      BEGSR
200500130410      **
200600130410      *  Ad ogni fine dettaglio scrive il VAO.
200700130410      *  DETTAGLIO inizia dal segmento UNH e si chiude con UNT.
200800130410      *
200900130410      *
201000130410      **?              /*---------------------- */
201100130410     c                   exsr      Scrive_Dettag
201200130410      **?              /*---------------------- */
201300130410      *
201400130410      *   Ogni nuovo Dettaglio,
201500130410      *    per quei Partner che gestiscono più LInee
201600130410      *    Mediante il Mittente del dettaglio
201700130410     c                   if        UNB_parziale ='S' and
201800130410     c                              PT_con_piu_Nazioni = 'S' and
201900130410     C                                 Naz_salvata = Naz_mittente
202000130410      * ?ReImposta i dati da tabella PT
202100130410      * ?  se fossero stati cambiati dal precedente dettaglio:
202200130411     C                   z-add     PT_key_savXX  Nr_PT
202300130410     c                   exsr      Se_Trovata_PT
202400130410     c                   endIF
202500130410      *>>>>>>>>>>>>>>>
202600110324      * Non deve dare messaggio
202700110324     c                   goto      No_UNTctrl
202800090211      *
202900100319      **   verifica la quadratura di tutti i segmenti ricevuti
203000100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
203100100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
203200100728     C     UNT0074       IFne      Conta_segmenti
203300100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
203400100319     c                             trollare il Messaggio ricevuto'
203500100319      *
203600130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFTMBP - ORM'
203700130506      *
203800100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
203900100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
204000130410     C                             O.R.M.import IFTMBP - messaggio -
204100100319     C                             con quadratura UNT errata controllare EDI -
204200100319     C                             ricevuto su UPLOAD x il cliente :'
204300100319     C                   EVAL      Messaggio = %trim(Messaggio) +
204400100319     C                                         %editc(§PTksc:'X')
204500100319     C                   EVAL      Messaggio = %trim(Messaggio) +
204600100319     C                             ' i segmenti contati sono :' +
204700100319     C                             %editc(Conta_segmenti:'Z')
204800100319     c                   if        §PVinv <> 'N'
204900100319     c                   exsr      invio_mail
205000100319     c                   end
205100100319     c                   end
205200110321     c     No_UNTctrl    tag
205300100319      **
205400100319     c     End_UNT       endSR
205500100316     C*----------------------------------------------------------------
205600100316      *? dati finali     UNT
205700100316     C*----------------------------------------------------------------
205800100316     C     DATI_UNZ      BEGSR
205900100316      **
206000130410      ** <Memorizza>
206100130410      **  dove deve aggiornare il singolo dettaglio:
206200130410     c                   eval      FinDET_RECord = vnrec
206300130410      **
206400100316     c                   endSR
206500130410     C*----------------------------------------------------------------
206600051205      *   DEFINIZIONE CHIAVI                               *
206700130410     C*----------------------------------------------------------------
206800051205     C     *INZSR        BEGSR
206900051205      *
207000991129     c     *ENTRY        PLIST
207100060613     C                   parm                    esito             1
207200971215      *
207300050112     C     KTABel        KLIST
207400050112     C                   KFLD                    TBLKUT
207500050112     C                   KFLD                    TBLCOD
207600050112     C                   KFLD                    TBLKEY
207700090210      *
207800090210     C     K_TAB1L       KLIST
207900090213     C                   KFLD                    etbUNB
208000090213     C                   KFLD                    etbSGM
208100090213     C                   KFLD                    etbVALSGM
208200090209     C*
208300090209     C* Definisco chiavi di accesso
208400090209     C     KTAB1         KLIST
208500090209     C                   KFLD                    KKUT
208600090209     C                   KFLD                    KCOD
208700090209      *
208800090209     C     KACO          KLIST
208900090209     C                   KFLD                    KKUT
209000090209     C                   KFLD                    KKCC
209100090209     C                   KFLD                    KKSC
209200090209      *
209300090209     C     KIND          KLIST
209400090209     C                   KFLD                    KKUT
209500090209     C                   KFLD                    KKCC
209600090209     C                   KFLD                    KKSC
209700111027      *
209800090209     C* RECUPERO DATI DELL'UTENTE
209900090209     C                   Z-ADD     1             CODUT             1 0
210000090209     C                   CALL      'XPARUT'
210100090209     C                   PARM                    UTEDSE0F
210200090209     C                   MOVEL     RAGUT         RSUT             20
210300090209     C*
210400090209     C* RICERCA CAPOCONTI
210500090209     C                   DO        50            X
210600090209     C                   MOVE      TCU(X)        TCUDS
210700090209     C     F56           IFEQ      'CG'
210800090209     C     F34           ANDEQ     '01'
210900090209     C                   Z-ADD     KCU(X)        KCI               4 0
211000090209     C                   END
211100090209     C                   END
211200090209     C*
211300090211     c                   movel(p)  'EDPAB'       User_Msg         10
211400090211     C*
211500090209     C* IMPOSTO A ZERO VARIABILI DI PGM
211600090209     c                   move      *blank        Snd_Msg_alert     1
211700090209     C                   MOVEL     CA(1)         WCA              78
211800090209     C                   MOVEL     CN(1)         WCN              78
211900090209     C*
212000090209     C* RECUPERO DATA E ORA
212100100630     C                   TIME                    Ora_Data         14 0
212200100630     C                   MOVE      Ora_Data      G02DAT
212300100630     C                   MOVE      Ora_Data      WDATA8            8 0
212400100630     C                   MOVE      WDATA8        Anno2             2 0
212500090209     C                   MOVEL     WDATA8        DATA              6 0
212600100630     C                   MOVE      Anno2         DATA
212700090209     C                   MOVEL     *BLANK        G02ERR
212800090209     C                   CALL      'XSRDA8'
212900090209     C                   PARM                    WLBDA8
213000090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
213100100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
213200090209     C                   TIME                    WORA              6 0
213300090209      * Recupero data e ora
213400090209     C                   TIME                    W0140
213500090209      * UDATE IN GGMMAAAA
213600100630     C                   MOVE      W0140         DataGGMMAAA
213700090209      * UDATE IN AAAAMMGG
213800100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
213900090209     C     *iso          MOVEL     DATA_eur      dateu
214000090209      *
214100090209      * ?              /*--------------------------- */
214200090209     c                   exsr      CARICA_TABELLE
214300090209      * ?              /*--------------------------- */
214400090209      *
214500090209     C                   MOVEL     *ALL'-'       CMP132          132
214600130410      *
214700130410      * variabili di messaggio
214800130410     c                   clear                   Almeno_Uno        1
214900130410     c                   clear                   Almeno_Un_Due     1
215000130410     c                   eval      esito  = '0'
215100130410     c                   eval      Segmento_in_errore = 0
215200130410     c                   z-add     0             Conta_segmTot     5 0
215300130410      *
215400991124     C                   ENDSR
215500060621      * ?------------------------------------------------------------------ */
215600090209      *?    CARICA TABELLE SU SCHIERE
215700060621      * ?------------------------------------------------------------------ */
215800130410     C     CARICA_tabelleBEGSR
215900090205      *
216000090209     C* Caricamento Tabella 15
216100130410      * Nazione di consegna x P.O. di Consegna ed Emittente
216200130410     C                   z-add     0             Nz                3 0
216300130410     c                   clear                   Naz03
216400130410     c                   clear                   Iso03
216500130411     c                   clear                   Cpf03
216600130410     C                   z-add     1             KKUT
216700130410     C                   MOVEL(p)  '15'          KCOD
216800130410     C     KTAB1         setll     TABEL00F
216900130410     C     KTAB1         reade     TABEL00F
217000130410DO  1C                   Dow       not %EoF(TABEL00F)
217100130410DO  1C                   if         tblflg = *blank
217200130410     C                   MOVEL     TBLuni        DS15
217300130410     C                   add       1             Nz
217400130410     C                   MOVEL     tblKEY        Naz03(Nz)
217500130410     C                   MOVEL     §15COD        Iso03(Nz)
217600130410     C                   MOVEL     §15ECF        CPF03(Nz)
217700130410     C                   MOVE      §15PCF        CPF03(Nz)
217800130411E   1C                   End
217900130410     C     KTAB1         reade     TABEL00F
218000130410E   1C                   End
218100090216      *
218200100702     C* Caricamento Tabella Priorità trasporto
218300130410     C                   Z-ADD     0             X                 4 0
218400100702     C                   MOVEL     'TS'          TABCOD
218500100702     C     TABCOD        CHAIN     EDTAB01L                           30
218600100702     C     *IN30         DOWEQ     '0'
218700100702     C     TABFLG        IFEQ      *BLANKS
218800100702     C                   ADD       1             X
218900100702     C                   eval      TipoServizio(X) = TABUNI
219000100705     C                   eval      Key_TipServ(X)  = TABKEY
219100100702     C                   END
219200100702     C     TABCOD        READE     EDTAB01L                               30
219300100702     C                   END
219400090209      *
219500090209      * Caricamento Tabella Partner esteri
219600090209     C                   Z-ADD     0             X
219700090209     C                   MOVEL     'PT'          TABCOD
219800090209     C     TABCOD        CHAIN     EDTAB01L                           30
219900090209     C     *IN30         DOWEQ     '0'
220000090209     C                   SELECT
220100090209     C     TABFLG        WHENNE    *BLANKS
220200090209     C                   OTHER
220300090209     C                   ADD       1             X
220400100630     C                   MOVEL     TABKEY        PT_key(X)
220500100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
220600100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
220700100630     C                   MOVEL     TABUNI        PT_uni(X)
220800090209     C                   ENDSL
220900090209     C     TABCOD        READE     EDTAB01L                               30
221000090209     C                   END
221100121105      *
221200121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
221300121105     c                   exsr      ChecK_PT
221400121105      *
221500090209      * salva quanti Codici UNB ha caricato
221600100630     c                   z-add     x             PT_KeyXsav
221700090209      *
221800090209     C                   MOVEL     'PU'          TABCOD
221900090209     C     TABCOD        CHAIN     EDTAB01L                           30
222000090209     C     *IN30         DOWEQ     '0'
222100090209     C                   SELECT
222200090209     C     TABFLG        WHENNE    *BLANKS
222300090209     C                   OTHER
222400100630     C                   MOVEL     TABKEY        PU_key
222500090209     C                   Z-ADD     1             X
222600100630     C     PU_key        LOOKUP    PT_key(X)                              32
222700100630     C   32              MOVEL     TABUNI        PU_uni(X)
222800090209     C                   ENDSL
222900090209     C     TABCOD        READE     EDTAB01L                               30
223000090209     C                   END
223100090209      *
223200090209      * Prosegue tab.PT e PU
223300090209     C                   MOVEL     'PV'          TABCOD
223400090209     C     TABCOD        CHAIN     EDTAB01L                           30
223500090209     C     *IN30         DOWEQ     '0'
223600090209     C                   SELECT
223700090209     C     TABFLG        WHENNE    *BLANKS
223800090209     C                   OTHER
223900100630     C                   MOVEL     TABKEY        PV_key
224000090209     C                   Z-ADD     1             X
224100100630     C     PV_key        LOOKUP    PT_key(X)                              32
224200100630     C   32              MOVEL     TABUNI        PV_uni(X)
224300090209     C                   ENDSL
224400090209     C     TABCOD        READE     EDTAB01L                               30
224500090209     C                   END
224600120629      *
224700120629      * Prosegue tab.PT e PU e PV
224800120629     C                   MOVEL     'PZ'          TABCOD
224900120629     C     TABCOD        CHAIN     EDTAB01L                           30
225000120629     C     *IN30         DOWEQ     '0'
225100120629     C                   SELECT
225200120629     C     TABFLG        WHENNE    *BLANKS
225300120629     C                   OTHER
225400120629     C                   MOVEL     TABKEY        PZ_key
225500120629     C                   Z-ADD     1             X
225600120629     C     PZ_key        LOOKUP    PT_key(X)                              32
225700120629     C   32              MOVEL     TABUNI        PZ_uni(X)
225800120629     C                   ENDSL
225900120629     C     TABCOD        READE     EDTAB01L                               30
226000120629     C                   END
226100090209      *
226200090209     C* Caricamento Tabella codici clienti - tariffe
226300090209     C                   Z-ADD     0             X
226400090209     C                   MOVEL     'CL'          TABCOD
226500090209     C     TABCOD        CHAIN     EDTAB01L                           30
226600090209     C     *IN30         DOWEQ     '0'
226700090209     C     TABFLG        IFEQ      *BLANKS
226800090209     C                   ADD       1             X
226900100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
227000100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
227100090209     C                   END
227200090209     C     TABCOD        READE     EDTAB01L                               30
227300090209     C                   END
227400090209      *
227500090209     C* Caricamento Serivizi speciali
227600090209     C                   Z-ADD     0             X
227700090209     C                   MOVEL     'SS'          TABCOD
227800090209     C     TABCOD        CHAIN     EDTAB01L                           30
227900090209     C     *IN30         DOWEQ     '0'
228000090209     C     TABFLG        IFEQ      *BLANKS
228100090209     C                   ADD       1             X
228200100702     C                   eval       Key_ServSpec(X) = TABKEY
228300100702     C                   eval       SpecialServ(X)  = TABKEY
228400100702     C                   eval       UNI_ServSpec(X) = TABUNI
228500090209     C                   END
228600090209     C     TABCOD        READE     EDTAB01L                               30
228700090209     C                   END
228800090209      *
228900090209     C                   ENDSR
229000100705      * ?------------------------------------------------------------------ */
229100100705      *?   Controlla e decodifica Valori VALUE dei segmenti
229200100705      * ?------------------------------------------------------------------ */
229300100705     C     In_TAB_EDSTBL BEGSR
229400100705      *
229500100705      *   Cerca la decodifica del dato in TABELLA
229600100705     c                   clear                   trovata_EDSTBL    1
229700100705     c                   clear                   quale_campo       6
229800100705     c                   clear                   campo_dati_70A   70
229900100705     C                   movel     UNB_Mittente  etbUNB
230000100705     c     K_TAB1L       chain     edstbl01l
230100100705      *
230200100705      * prova quindi senza l'UNB specifico del cliente
230300100705     c                   if        not %Found(edstbl01l)
230400100705     C                   clear                   etbUNB
230500100705     c     K_TAB1L       chain     edstbl01l
230600100705     c                   end
230700100705      *
230800100705      *  Traduce i testi descrittivi se PRESENTE in tabella
230900100705      *   il campo di destinazione è definito sulla destra della descrizione
231000130411      *     ed è lungo 6 come i nomi definiti nei VAB o VAO
231100100705     c                   if        %Found(edstbl01l)
231200100705     c                   move(p)   etbDESCRI     quale_campo
231300100705     c                   movel(p)  etbDATI       campo_dati_70A
231400100705     c                   move      'S'           trovata_EDSTBL
231500100705     c                   end
231600100705      *
231700100705     C                   ENDSR
231800100705     c*==================================================================*
231900090209     C*? CNVDAT - DECODIFICA LA DATA
232000090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
232100090209     C*         - WDATA  (DATA ALFANUMERICA)
232200090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
232300090209     C*         - WHMS   (ORA NUMERICA)
232400090209     C*----------------------------------------------------------------
232500100702     C     Converte_Data BEGSR
232600090209     C*
232700090209     C                   MOVEL     ' '           WERRO             1
232800090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
232900090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
233000100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
233100090209     C                   Z-ADD     *ZEROS        WHMS              6 0
233200090209     C*
233300120905     C     Work_Format_3 IFEQ      *blank
233400120905     c                   if        %Len(%trim(Work_Data__35)) = 8
233500120905     C                   eval         Work_Format_3 = Data_senza_ora
233600120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 12
233700120905     C                   eval         Work_Format_3 = Data_con_ora
233800120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 14
233900120905     C                   eval         Work_Format_3 = Data_con_i_secondi
234000120905     c                   end
234100120905     C                   END
234200120905     C*
234300120905     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
234400100702     C                   MOVEL     Work_Data__35 WCOMO8            8
234500090209     C                   TESTN                   WCOMO8               99
234600090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
234700090209     C  N99              MOVEL     'S'           WERRO             1
234800090209     C                   END
234900090209     C*
235000100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
235100100702     C                   MOVEL     Work_Data__35 WCOMO12          12
235200100120     C                   TESTN                   WCOMO12              99
235300100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
235400100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
235500100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
235600090209     C  N99              MOVEL     'S'           WERRO             1
235700090209     C                   END
235800100120     C*
235900100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
236000100702     C                   MOVEL     Work_Data__35 WCOMO14          14
236100100120     C                   TESTN                   WCOMO14              99
236200100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
236300100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
236400100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
236500100120     C  N99              MOVEL     'S'           WERRO             1
236600100120     C                   END
236700090209     C*
236800090209     C                   ENDSR
236900090210     C*----------------------------------------------------------------
237000100701     C*? Se_Numerico       - CONTROLLO NUMERICO
237100100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
237200100701     C*         - lunghezza_num   (Lunghezza campo output)
237300100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
237400090210     C*----------------------------------------------------------------
237500100701     C     Se_Numerico   BEGSR
237600090210     C*
237700090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
237800100701     C                   MOVEL     'N'           Campo_Numerico    1
237900100701     C                   MOVEL     *BLANKS       Numero_OK        15
238000110224     C*
238100090210     C                   Z-ADD     15            IN                3 0
238200100701     C                   Z-ADD     lunghezza_num IX                3 0
238300110224     C*
238400090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
238500100701     C                   MOVEA     test_num      NDA
238600090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
238700090210     C                   MOVEA     *ALL'0'       NDN
238800100630      *
238900090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
239000100701     C                   Z-ADD     lunghezza_num I                 3 0
239100110224     C*
239200090210     C     I             DOWGT     0                                            --- 1 -->
239300110224     C*
239400090210     C* ESTRAE IL CARATTERE DA ESAMINARE
239500100701     C     1             SUBST     test_num:I    WTEM01            1
239600110224     C*
239700090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
239800090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
239900090210     C     WTEM01        SCAN      WCA                                    99
240000110224     C*
240100090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
240200090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
240300110224     C*
240400090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
240500090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
240600110224     C*
240700090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
240800090210     C* DA DESTRA VERSO SINISTRA
240900090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
241000090210     C                   MOVEL     WTEMP1        NDN(IN)
241100090210     C                   SUB       1             IN
241200090210     C                   END                                                    <-- 3 ---
241300110224     C*
241400090210     C                   END                                                    <-- 2 ---
241500090210     C                   SUB       1             I
241600110224     C*
241700090210     C                   END                                                    <-- 1 ---
241800090210     C*
241900090210     C                   MOVEA     NDN           WNDN             15
242000110224     C*
242100090210     C     WNDN          IFNE      *ZEROS
242200100701     C                   MOVEA     NDN           Numero_OK        15
242300100701     C                   MOVEL     'S'           Campo_Numerico    1
242400090210     C                   END
242500090210     C*
242600090210     C                   ENDSR
242700090210     C*----------------------------------------------------------------
242800090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
242900090210     C*----------------------------------------------------------------
243000100705     C     Converte_CAP  BEGSR
243100090210     C*
243200090210     C                   MOVEL     *BLANKS       WCAP              9
243300090210     C     nad3251       IFNE      '0'
243400090210     C     '  '          SCAN      nad3251       LENGHT            2 0
243500090210     C                   SUB       1             LENGHT
243600090210     C     LENGHT        IFLE      5
243700090210     C     LENGHT        ANDGT     0
243800090210     C                   Z-ADD     LENGHT        T                 2 0
243900090210     C                   Z-ADD     5             S                 2 0
244000090210     C                   MOVEA     nad3251       CAP9
244100090210     C                   MOVEL     *ZEROS        CAP5
244200090210     C                   DO        LENGHT
244300090210     C                   MOVE      CAP9(T)       CAP5(S)
244400090210     C                   SUB       1             S
244500090210     C                   SUB       1             T
244600090210     C                   END
244700090210     C                   MOVEA     CAP5          WCAP5             5
244800090210     C     WCAP5         IFEQ      '0'
244900090210     C                   MOVEL     *BLANKS       WCAP
245000090210     C                   ELSE
245100090210     C                   MOVEA     CAP5          WCAP
245200090210     C                   END
245300090210     C*
245400090210     C                   ELSE
245500090210     C                   MOVEL     nad3251       WCAP
245600090210     C                   END
245700090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
245800090210     C     WCAP          IFNE      *BLANKS
245900090210     C     WFLCPF        ANDNE     0
246000090210     C                   MOVEL     WFLCPF        WELE              1 0
246100090210     C                   MOVE      WFLCPF        WPOS              1 0
246200090210     C                   MOVEL     *BLANK        WCPF
246300090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
246400090210     C                   ELSE
246500090210     C                   MOVEL     *BLANKS       WCPF
246600090210     C                   END
246700090210     C                   END
246800090210     C*
246900090210     C                   ENDSR
247000130410      * ?================================================================== */
247100130410     C*? Azzera_MSG   - Azzera dati messaggio
247200130410      * ?================================================================== */
247300130410     C     Scrive_Dettag BEGSR
247400130410      **
247500130410      *  Deve flaggare il dettaglio che ha dei problemi
247600130410      * Impostando le righe in errore
247700130410     C                   if        Err_in_detta = 'S'
247800130410      * prima di rieseguire il ciclo
247900130410      *  rilascia il record
248000130410     c                   update    tivin000
248100130410      **
248200130410      * ?  Scrive su tutti i records il tipo di errore
248300130410     c     IniDET_RECord setll     tivin00r
248400130410      **?              /*---------------------- */
248500130410     c                   exsr      DETta_Errato
248600130410      **?              /*---------------------- */
248700130410      *
248800130411???? c                   else
248900130411      *
249000130410      *  se è arrivato in fondo al dettaglio scrive VAO
249100130410      * ?              /*---------------------- */
249200130410     c                   exsr      Scrive_VAO
249300130410      * ?              /*---------------------- */
249400130411      *
249500130410      *  Problemi nella decodifica dei campi VAO
249600130410     c                   if        errori_in_Wrt = 'S'
249700130410     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAO'
249800130410     c                   exsr      Error_DET
249900130410     c                   ROLBK
250000130410     c                   else
250100130410     c                   COMMIT
250200130410     c                   end
250300130410      *
250400130410     c                   end
250500130410      **
250600130410     c     end_WRidett   endSR
250700090211      * ?================================================================== */
250800090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
250900090211      * ?================================================================== */
251000130410     C     Scrive_VAO    BEGSR
251100090211      *
251200090211     c                   move      'S'           Almeno_Uno
251300130424     C                   MOVEL     Woggi         vaodao                         Data
251400130424     C                   MOVEL     Wora          vaooao                         Ora
251500130424     c                   if        vaooao = 0
251600130424     c                   eval      vaooao = 1200
251700130424     c                   end
251800090211      *
251900130411      ** tipo comunicazione ORM
252000130423     C                   eval      vaotco = 'F'
252100130424      *
252200130424      ** stampa DDT ('S')
252300130424     C                   eval      vaoDDT = 'S'
252400130424      *
252500130411      * Imposta le NOte
252600130411     C     VAoNO1        IFEQ      *BLANKS
252700130411     C                   MOVEL     Note_1        VAoNO1
252800130411     C                   MOVEL     Note_2        VAoNO2
252900130411     C                   ELSE
253000130411     C                   MOVEL     Note_1        VAoNO2
253100130411     C                   END
253200130411      **
253300130411      **   tipo ORM
253400130422     C                   eval      vaotor = 'M'
253500130422     c                   IF           NAD_Destinatario ='S'
253600130411     C                   eval      vaotor = 'S'
253700130422     c                   END
253800130422      **
253900130411     C                   eval      VAOPAG = 'O'
254000130411     C* ?------------------
254100130411     c                   eval        Dorm01 = vaoFLO
254200130423      * sempre commissionato
254300130411     c                   move      'S'           §ormFD
254400130411     c                   move      'S'           §ormFR
254500130411     c                   eval        vaoFLO = Dorm01
254600130424      *
254700130424      **  scrive il TIORI di sede
254800130424     c                   exsr      WRIORI
254900130411      **
255000130411     C                   WRITE     FnVAO000                             99
255100130411     c   99              eval      errori_in_Wrt = 'S'
255200130424      **
255300130410     C     Error_VAO     Endsr
255400130424     C*------------------------------------------------------------------------*
255500130424     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
255600130424     C*------------------------------------------------------------------------*
255700130424     C     WRIORI        BEGSR
255800130424     C*
255900130424     C* Come prima cosa stacco un numeratore da AZNUM
256000130424     C                   clear                   trul33ds
256100130424     C                   eval      I33OPE = *zeros
256200130424     C                   eval      I33CNU = 600
256300130424     C                   eval      I33NUM = 1
256400130424     c                   move      kpjbu         SvKpjbu
256500130424     C                   clear                   KPJBU
256600130424     C                   movel     TRUL33DS      KPJBU
256700130424     C                   call      'TRUL33R'
256800130424     C                   parm                    KPJBA
256900130424     C                   movel     KPJBU         TRUL33DS
257000130424     c                   eval      kpjbu   =     SvKpjbu
257100130424     C                   if        O33ERR = *zeros
257200130424     C                   z-add     O33NRF        §numori
257300130424     c                   else
257400130424     c                   eval      §numori = *all'9'
257500130424     C                   endif
257600130424     C*
257700130424     C* Quindi imposto i campi "extra-VAO"
257800130424     C                   eval      oriPRG = §numori
257900130424     C                   eval      oriDER = vaoDAO
258000130424     C                   eval      oriOER = vaoOAO
258100130424     C                   eval      oriPOE = vaoPOE
258200130424     C                   eval      oriNSR = vaoNSR
258300130424     C                   eval      oriNOR = vaoNOR
258400130424     C                   eval      oriNRV = vaoNRV
258500130424     C                   eval      oriFLO = *blanks
258600130424     C*
258700130424     C* Infine valorizzo la chiave esterna sul file FNVAO
258800130424     C                   eval      dorm01 = vaoFLO
258900130424     C                   movel     §numori       §ormpg
259000130424     C                   movel     'S'           §ormfd
259100130424     C                   movel     'S'           §ormfr
259200130424     C**************     movel     'S'           §ormks
259300130424     C                   movel     *blank        §ORMIC
259400130424     C                   movel     *blank        §ORCOM
259500130424     C                   eval      vaoFLO = dorm01
259600130424     C*
259700130424     C* ...quindi scrivo il file TIORI00F
259800130424     C                   WRITE     TIORI000
259900130424     C*
260000130424     C                   ENDSR
260100130410      * ?------------------------------------------------------------------ */
260200130424      * CTRL caricamento schiere    PT                                     -*
260300130411      * ?------------------------------------------------------------------ */
260400121105     C     Check_PT      begsr
260500121105     C*
260600121105     c* Controllo riempimento schiera
260700121105     c                   clear                   trul0sds
260800121105     c                   eval      i0sski='PT_key'
260900121105     c                   eval      i0sele=%elem(PT_key)
261000121105     c                   eval      i0spie=x
261100121105     c                   eval      i0sfile='EDTAB00F'
261200121105     c                   eval      i0ssif=knsif
261300121105     c                   eval      i0spgm='TRTCT93R'
261400121112     c                   move      kpjbu         SvKpjbu
261500121112     c                   clear                   kpjbu
261600121105     c                   movel     trul0sds      kpjbu
261700121105     c                   call      'TRUL0SR'
261800121105     c                   parm                    kpjba
261900121112     c                   eval      kpjbu = SvKpjbu
262000121105     C*
262100121105     C                   ENDSR
262200120629      * ?------------------------------------------------------------------ */
262300090211     O*****************************************************************
262400090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
262500090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
262600090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
262700090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
