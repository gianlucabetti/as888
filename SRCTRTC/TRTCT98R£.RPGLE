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
001400130410      *---------------------------------------------------------------------*
001500130410     ftabel00f  if   E           K DISK
001600130410     ffnacr01l  if   E           K DISK
001700130410     fcnaco00f  if   E           K DISK
001800090209     FCNIND00F  IF   E           K DISK
001900130410     fedtab01l  if   E           K DISK
002000090210      *
002100091019???? Fedstbl01l IF   E           K DISK
002200130410      **?************************************************************************
002300130410      * Stringhe per conversione alfanumerico/numerico N.Documento
002400130410     D CA              S             78    DIM(1) CTDATA PERRCD(1)
002500130410     D CN              S             78    DIM(1) CTDATA PERRCD(1)
002600130410      **?************************************************************************
002700130410      **  Elenco DS di tutti i Segmenti da tradurre
002800130410      **?************************************************************************
002900130410???? D edS00BGM      E DS
003000130410???? D edS00CNI      E DS
003100130410???? D edS00COM      E DS
003200130410???? D edS00CTA      E DS
003300130410???? D edS00DTM      E DS
003400130410???? D edS00FTX      E DS
003500130410???? D  D05                   16    365    DIM(5)
003600130410???? D edS00GID      E DS
003700130410???? D edS00LOC      E DS
003800130410???? D edS00MEA      E DS
003900130410???? D edS00NAD      E DS
004000130410???? D edS00RFF      E DS
004100130410???? D edS00TDT      E DS
004200130410???? D edS00TSR      E DS
004300130410???? D edS00UNA      E DS
004400130410???? D edS00UNB      E DS
004500130410???? D edS00UNH      E DS
004600130410???? D edS00UNT      E DS
004700130410???? D edS00UNZ      E DS
004800130410      **?************************************************************************
004900130410     D EDIDSPT       E DS
005000130410     D EDIDSPU       E DS
005100130410     D EDIDSPV       E DS
005200130410     D EDIDSPZ       E DS
005300130410     D EDIDSCL       E DS
005400130410     D EDIDSSS       E DS
005500130410     D EDIDSTC       E DS
005600130410      **?************************************************************************
005700130410     D KPJBA         E DS
005800130410     D     svkpjbu     s                   like(kpjbu)
005900130410      **?************************************************************************
006000130410     D* DS PER FNLV13R - CONTROLLO DI UN CAP
006100130410     D DSLV13        E DS                  EXTNAME(FNLV13DS)
006200130410     D* DS PER TISI95R - CONTROLLO DI CAP
006300130410     D DSSI95        E DS                  EXTNAME(TISI95DS)
006400130410      *---------------------------------------------------------------------*
006500130410     D UTEDSE0F      E DS
006600130410     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
006700130410     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
006800130410      * Ds scomposzione tipo capoconti
006900130410     D TCUDS           DS
007000130410     D  F34                    3      4
007100130410     D  F56                    5      6
007200940321      *----------------------------------------------------*
007300090209      * numero relativo di record
007400090209     D SRECDS          DS
007500090209     D  NRREC                397    400B 0
007600090209      *
007700090213      * numero relativo di record
007800090213     D VINRECDS        DS
007900090213     D  VNREC                397    400B 0
008000130410      **---------------------------------------------------*
008100100701     D WLBDA8          DS
008200100701     D  G02DAT                 1      8  0
008300100701     D  G02INV                 9     16  0
008400100701     D  G02ERR                17     17
008500100701     D  G02TGI                18     22  0
008600100701      *----------------------------------------------------*
008700090209     D Valori_inErr    ds
008800090209     D  SKout_Errori                  1    DIM(50)
008900100630      **
009000090209     D Descr_Errore    ds
009100090209     D  SKout_DesErr                 50    DIM(50)
009200090205      *----------------------------------------------------*
009300091016     D EoFTivin        s              1
009400091016      *----------------------------------------------------*
009500090205     D Tipo_seg        S              3
009600100720     D Trovato_seg     S              1
009700100716      *
009800100716     d keyUNBCLI       s             35
009900100716     d keyTIPOMSG      s              6
010000100716     d keyVERSION      s              3
010100100716     d keyRELEASE      s              3
010200100716     d keyAGENCY       s              3
010300100716     d keyASSOCIA      s              6
010400100716      *
010500100720     D DsGenerica      s           2048
010600090209     D segmento        s           2048
010700090209     D Errore          s              1
010800130411     D err_bloccante   s              1
010900100701      *
011000000223     D W0140           S             14  0
011100991129     D WORA            S              6  0
011200100701     D DataGGMMAAA     S              8  0
011300100701     D DATEU           S              8  0
011400100701     D DATA_eur        S               D   DATFMT(*eur)
011500100701      *
011600100628     D XX              S              3  0 INZ
011700110328     d Key_Generica35  s             35
011800100708     D IniDET_RECord   s                   LIKE(vnREC)
011900100708     D FinDET_RECord   s                   LIKE(vnREC)
012000100318      *------------------------------------------
012100100318      **  Definizione Qualificatori
012200100318      *------------------------------------------
012300130410???? D Tipo_Messaggio...
012400130410???? D                 s              3    INZ('335')
012500130411???? D Riferimento_Ordine...
012600130411???? D                 s              3    INZ('CR ')
012700130411???? D Riferimento_Destinatario...
012800130411???? D                 s              3    INZ('ANT')
012900130411???? D Riferimento_Cliente_FF...
013000100318???? D                 s              3    INZ('FF ')
013100100318???? D Telephone_Number...
013200100318???? D                 s              3    INZ('TE ')
013300100318???? D TeleFax_Number...
013400100318???? D                 s              3    INZ('TX ')
013500130410???? D Volume...
013600130410???? D                 s              3    INZ('VOL')
013700100705???? D Weight...
013800100705???? D                 s              3    INZ('WT ')
013900100319???? D G_Weight...
014000100319???? D                 s              3    INZ('G  ')
014100100318???? D Gross_Weight...
014200100318???? D                 s              3    INZ('AAG')
014300100706???? D Gross_Weight_E...
014400100706???? D                 s              3    INZ('AAE')
014500110512???? D Gross_Weight_D...
014600110512???? D                 s              3    INZ('AAD')
014700100706???? D N_Weight...
014800100706???? D                 s              3    INZ('N  ')
014900100706???? D Net_Weight...
015000100706???? D                 s              3    INZ('AAF')
015100100318???? D Kilograms...
015200100318???? D                 s              3    INZ('KGM')
015300100319???? D Grams...
015400100319???? D                 s              3    INZ('163')
015500100318???? D Meters...
015600100318???? D                 s              3    INZ('MTR')
015700100318???? D Cubic_Meters...
015800100318???? D                 s              3    INZ('MTQ')
015900100705???? D Data_senza_ora...
016000100628???? D                 s              3    INZ('102')
016100100705???? D Data_con_ora...
016200100628???? D                 s              3    INZ('203')
016300100705???? D Data_con_i_secondi...
016400100705???? D                 s              3    INZ('204')
016500120613     D controlla_COD_FTX...
016600120613???? D                 s              3
016700130411     d  Natura_Merce...
016800130411     d                 s              3    inz('AAA')
016900100316      *---------------------------------------------------------------------*
017000100316      **  segmenti Identificativi parti specifiche del messaggio
017100100316      *---------------------------------------------------------------------*
017200100701???? D seg_imprevisto...
017300100701???? D                 s              1
017400100701      *---------------------------------------------------------------------*
017500130411???? D NAD_Destinatario...
017600100701???? D                 s              1
017700130411???? D NAD_Mittente...
017800100701???? D                 s              1
017900130411???? D NAD_Ordinante...
018000130411???? D                 s              1
018100130411???? D NAD_HI          s              6    inZ('VAORSO')
018200130411???? D NAD_CZ          s              6    inZ('VABRMO')
018300130411???? D NAD_CN          s              6    inZ('VABRSD')
018400111121???? D NAD_Lunghezza_Indir_Aggiuntivo...
018500111121???? D                 s              3s 0
018600111121???? D NAD_Lunghezza_Indirizzo...
018700111121???? D                 s              3s 0
018800130411???? D NAD_Lunghezza_ragSociale...
018900111121???? D                 s              3s 0
019000100701      *
019100090209      *---------------------------------------------------------------------*
019200100701???? D Contatore_Dettagli...
019300100701???? D                 s              5s 0 INZ(0)
019400100728???? D Segmento_in_errore...
019500100728???? D                 s              5s 0 INZ(0)
019600100701      *---------------------------------------------------------------------*
019700100630     d UNB_diWork      s                   like(UNB0004)
019800100628     d  UNB_Mittente   s                   like(UNB0004)
019900100706     d BGM_Id_Testata  s             35A   inz(' ')
020000130411     d  RFF_RifRitiro  s             35A
020100100701     d UNB_parziale    s              1    inz('N')
020200100813     d UNB_Generico    s             31    inz(' ')
020300100701     d UNB_NrTrasmiss  s             14    inz(' ')
020400100701     d UNZ_NrTrasmiss  s             14    inz(' ')
020500100701     d UNH_Rifer_Msg   s             14    inz(' ')
020600130410      *
020700130411     D TRUL0SDS      E DS
020800130411      *
020900100701      * Tabella partner
021000121105     D PT_key          S             35    DIM(200)
021100121105     D PT_Parz         S             35    DIM(%elem(PT_key))
021200121105     D PT_KSC          S              7    DIM(%elem(PT_key))
021300121105     D PT_uni          S             90    DIM(%elem(PT_key))
021400121105     D PU_uni          S             90    DIM(%elem(PT_key))
021500121105     D PV_uni          S             90    DIM(%elem(PT_key))
021600121105     D PZ_uni          S             90    DIM(%elem(PT_key))
021700100701      *
021800130411     D Nr_PT           S              3  0 INZ
021900130411     D PT_key_savXX    S              3  0 INZ
022000130411     D  PT_poe         s                   LIKE(§PTlnp)
022100130411     D  PT_cor         s                   LIKE(§PTksc)
022200130411     d  PT_KeyXsav     s              3  0 inz(0)
022300130411     D  PT_trovata     s              1    inz('N')
022400130411     D  PT_con_piu_Nazioni...
022500130411     d                 s              1
022600100701     D PU_key          S             35
022700130411     D  PU_TipoServ    s              2a
022800130411     D  PU_SpecServ    s              2a
022900100701     D PV_key          S             35
023000120629     D PZ_key          S             35
023100130410      *
023200100701      *
023300100701     d  GID_aTotale    s              1A
023400100705     d  GID_ColliInAdd...
023500100705     d                 s                   like(gid722420)
023600090209      *---------------------------------------------------------------------*
023700090209      * Schiere
023800090209      *---------------------------------------------------------------------*
023900090209      * Nr. documento alfanumerico da decodificare
024000090209     D NDA             S              1    DIM(35)
024100100630      *
024200090209      * Nr. documento alfanumerico da già decodificato
024300090209     D NDN             S              1    DIM(15)
024400100630      *
024500090209      * Messaggi di errore
024600090209     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
024700100630      *
024800130410      * ?================================================================== */
024900130410      * Tabella codici nazioni
025000130410     D Ds15          e DS
025100130410     D  Naz03          s              3    DIM(999)
025200130410     D  Iso03          s              3    DIM(999)
025300130410     D  Cpf03          S              2  0 DIM(999)
025400130410      * ?================================================================== */
025500130410     D Dorm01        e DS
025600100630      *
025700090209      * Tabella CL --> codici clienti - tariffa
025800130305     D Cod_Tb_CL       S             35    DIM(999)
025900130305     D Des_Tb_CL       S             90    DIM(999)
026000100630      *
026100100318      * Priorità trasporto
026200100705     D Key_TipServ     S             35    DIM(100)
026300100702     D TipoServizio    S              1    DIM(100)
026400100702      *
026500100702      * Decodifiche servizi speciali
026600100702     D SpecialServ     S             35    DIM(100)
026700100702     D Key_ServSpec    S              3    DIM(100)
026800100702     D UNI_ServSpec    S             90    DIM(100)
026900100630      *
027000090209      * Schiere x routine di conversione CAP
027100090209     D CAP5            S              1    DIM(5)
027200090209     D CAP9            S              1    DIM(9)
027300090209     D CAPM            S              1    DIM(9)
027400100630      *
027500090209      * Schiera per memorizzazione FTX ricevuti
027600090209     D QUA             S              3    DIM(100)
027700090209     D FTX             S             70    DIM(100)
027800100630      *
027900090209      * Tabelle capiconto
028000090209      * Schiere x località
028100090209     D LOD             S              1    DIM(35)
028200060608      **?------------------------------------------------------------------ */
028300090209     C* Definizione campi di work
028400090209     D kKUT            s                   LIKE(TBLKUT)
028500090209     D kCOD            s                   LIKE(TBLCOD)
028600090209     D kKCC            s                   LIKE(ACOKCC)
028700090209     D kKSC            s                   LIKE(ACOKSC)
028800130410      **?------------------------------------------------------------------ */
028900090209      *  Invio E-mail
029000090209     D Indirizzi       s            253
029100090209     D Oggetto         s             44
029200090209     D Messaggio       s           5000
029300090209     d   DSmail        ds
029400090209     D Mail_Ind                      44
029500090209     d   IND_char                     1    dim(44)
029600090209      *
029700100630     D Lunghezza_Indirizzo...
029800100630     D                 s              5u 0
029900100630     D dalCarattere    s              5u 0
030000100630     D xTOTcaratteri   s              5u 0
030100090209      *
030200130410     d bart_it         C                   CONST('@Brt.it;')
030300130410     d CED_Bart        C                   CONST('CED@Brt.it;')
030400060612      * ?================================================================== */
030500060612      * ?   * Campi x decodifica * (INPUT  del Record)
030600100319      * ?================================================================== */
030700090130     D  Dati           s           2048
030800060612      * ?   *--------------------------------------------------------------*
030900060612     D  se_errore      s              1    inz(' ')
031000060612      * ?* ------------------------------------------------------ *
031100130410     d minu            c                   const('qwertyuiopasdfghjklzxcvbnmàáâ-*alfabeto
031200130410     d                                     ãäåèéêëìíîïòóôõöùúûüæçðñßÑÀÁÂÃÄÅÆÈÉÊ-
031300130410     d                                     ËÌÍÎÏÒÓÔÕÖÙÚÛÜ')
031400130410     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMAAA-*ALFABETO
031500130410     d                                     AAAEEEEIIIIOOOOOUUUUACANSNAAAAAAAEEE-
031600130410     d                                     EIIIIOOOOOUUUU')
031700130410     D Digits          C                   '0123456789'
031800060612      * ?================================================================== */
031900060612      *   Ciclo principale
032000090205      * ?================================================================== */
032100060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
032200060612     C                   if        not %open(tivin00r)
032300060612     C                   open      tivin00r
032400060612     C                   endif
032500130410     C*
032600060612      * ?------------------------------------------------------------------ */
032700130410      *? Importa i records della trasmissione
032800130410      * ?------------------------------------------------------------------ */
032900130410     c     *start        setll     Tivin00r
033000130410
033100130410     c                   EXSR      LEGGE_TIVIN_R
033200130410     c                   dow       EoFTivin <> 'S'
033300130410
033400130410      * solo i record sflaggati da rielaborare
033500130410     c                   IF        vinFLG = *blank
033600130410     C                   eval      vinFLG = '1'                                 Sempre Record OK
033700130410     c                   clear                   se_errore
033800130410      *** >>>>>>
033900130410     c                   IF        vinDTA <> *blank
034000130410      *** >>>>>>
034100130410      ** Decodifica record a campi variabili
034200130410      * ?              /*---------------------- */
034300130410     c                   exsr      Decod_Segmento
034400130410      * ?              /*---------------------- */
034500130410
034600130410      *  x errore bloccante nella preparazione del VAO
034700130410     c                   if        err_bloccante ='S'
034800130410     C                   eval      vinFLG = '2'
034900130410     c                   eval      Almeno_Un_Due ='S'
035000130410     c                   eval      esito  = '2'
035100130410???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTMBP'
035200130410     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
035300130410     C                             ORM import - messaggio -
035400130410     C                             con UNB non presente in tabella PT controlla-
035500130410     C                             re EDI ricevuto su UPLOAD.'
035600130410     c                   if        §PVinv <> 'N'
035700130410     c                   exsr      invio_mail
035800130410     c                   end
035900130410     c                   end
036000130410      *** >>>>>>
036100130410     c                   endIF
036200130410      *** >>>>>>
036300130410      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
036400130410      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
036500130410      *         un erroneo messaggio
036600130410      *     di errore come se tutto il messaggio fosse ricevuto errato.
036700130410     c                   update    Tivin000
036800130410
036900130410     c                   endIF
037000130410
037100130410     c                   EXSR      LEGGE_TIVIN_R
037200130410     C                   ENDdo
037300130410      **
037400060612      * ?              /*---------------------- */
037500060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
037600090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
037700090225     c                             esito ='0'
037800060710     C                   eval      esito ='0'
037900090225      *
038000090225     c                   COMMIT
038100110208     C                   endif
038200130410      * ?              /*---------------------- */
038300090224      *
038400090213     c                   if        almeno_un_due ='S'
038500110208     C                   eval      esito ='1'
038600060614     C                   endif
038700100708      * chiude TIVIN
038800100708     C                   if        %open(tivin00r)
038900100708     C                   close     tivin00r
039000100708     C                   endif
039100060614      *
039200060612     c                   SETON                                        LR
039300100709      * ?------------------------------------------------------------------ */
039400100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
039500100709      * ?------------------------------------------------------------------ */
039600100709     C     MSG_Errato    BEGSR
039700100709      *
039800100709      * ?  Scrive su tutti i records il tipo di errore
039900100709     c     *start        setll     tivin00r
040000100709     c
040100100709     c                   EXSR      LEGGE_TIVIN_R
040200100709     c                   dow       EoFTivin <> 'S'
040300100709     c
040400100709     c                   if        vinMSG  = *blank
040500100709     C                   evalR     vinMSG  = 'MSG ricevuto Anomalo +
040600100709     C                               >> Controllare i DATI su UPLOAD !!'
040700100709     c                   end
040800100709     C                   eval      vinFLG = '2'
040900100709     c                   eval      Almeno_Un_Due ='S'
041000100709     c                   eval      esito  = '2'
041100100709     c                   eval      se_errore ='S'
041200100709     c                   eval      err_bloccante ='S'
041300100709      *
041400100709     c                   update    tivin000
041500100709     c                   EXSR      LEGGE_TIVIN_R
041600100709     c                   endDO
041700100709      *
041800100709     C                   ENDSR
041900100709      * ?------------------------------------------------------------------ */
042000100709      *?    Flagga il TIVIN come messaggio completamente ERRATO
042100100709      * ?------------------------------------------------------------------ */
042200100709     C     DETta_Errato  BEGSR
042300100709      *
042400100709     c                   EXSR      LEGGE_TIVIN_R
042500100709     c                   dow       EoFTivin <> 'S'
042600100709      *
042700100709      * Se ha letto il successivo record
042800100709      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
042900130410     c                   if        VNREC = finDET_RECord
043000100709     c                   leave
043100100709     c                   end
043200100709      *
043300100709     c                   exsr      Error_DET
043400100709      *
043500100709     c                   if        vinMSG  = *blank
043600100709     C                   evalR     vinMSG  = 'Dettaglio ERRATO -
043700100709     C                             >> Controllare i DATI di ogni Segmento <<'
043800100709     c                   end
043900100709      *
044000100709     c                   update    tivin000
044100100709     c                   EXSR      LEGGE_TIVIN_R
044200100709     c                   endDO
044300100709      * ------
044400130410???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTMBP'
044500100709     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
044600130410???? C                             ORM IFTMBP non totalmente tradotto. -
044700100709     C                             Controllare UPLOAD del cliente:'
044800100709     C                   EVAL      Messaggio = %trim(Messaggio) +
044900100709     C                                         %editc(§PTksc:'X')
045000100728     C                   EVAL      Messaggio = %trim(Messaggio) + ' alla riga s-
045100100728     C                             egmento: ' + %editc(Segmento_in_errore:'Z')
045200100709     c                   if        §PVinv <> 'N'
045300100709     c                   exsr      invio_mail
045400100709     c                   end
045500100709      *
045600100709     C                   ENDSR
045700130410      * ?------------------------------------------------------------------ */
045800130410      *?      X non bloccare in nessun caso il traduttore CLIENTI
045900130410      * ?------------------------------------------------------------------ */
046000130410     C     *pssr         BEGSR
046100130410     C
046200130410     C                   eval      esito = '2'
046300130410???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTMBP'
046400130410     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
046500130410     C                             O.R.M. import  - messaggio -
046600130410     C                             EDI ricevuto errato su UPLOAD.'
046700130410     c                   if        §PVinv <> 'N'
046800130410     c                   exsr      invio_mail
046900130410     c                   end
047000130410     C
047100130410     C                   ENDSR
047200130410      * ?------------------------------------------------------------------ */
047300130410      *?      Segnala Errore su TIVIN
047400130410      * ?------------------------------------------------------------------ */
047500130410     C     Error_DET     BEGSR
047600130410     C
047700130410     c                   eval      Segmento_in_errore = Conta_segmTOT
047800130410     C                   eval      vinFLG = '2'
047900130410     c                   eval      Almeno_Un_Due ='S'
048000130410     c                   eval      esito  = '1'
048100130410     c                   eval      se_errore ='S'
048200130410     C                   eval      Err_in_detta = 'S'
048300130410     C
048400130410     C                   ENDSR
048500100709      * ?------------------------------------------------------------------ */
048600100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
048700100709      * ?------------------------------------------------------------------ */
048800100709     C     LEGGE_TIVIN_R BEGSR
048900100709      *
049000100709     c                   clear                   EoFTivin
049100100709      *  Lettura sequenziale
049200100709     c                   read      Tivin00r
049300100709
049400100709     c                   if        %Eof(tivin00r)
049500100709     c                   move      'S'           EoFTivin
049600100709     c                   else
049700100709      *
049800100709      * ?  imposta il tipo di segmento:
049900100709     c                   clear                   dati
050000100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
050100100728     c                   eval      segmento = %trimL(dati)
050200100728     c                   eval      tipo_seg = %subst(segmento:1:3)
050300100709      **
050400100709     c                   end
050500100709      *
050600100709     C                   ENDSR
050700100120      * ?------------------------------------------------------------------ */
050800100120      *?    Decodifica il segmento ed importa i campi in formato DS
050900100120      * ?------------------------------------------------------------------ */
051000100120     c     Decod_SegmentoBegsr
051100130410      *
051200130410      *  Contatore delle righe ossia dei segmenti del messaggio
051300130410     c                   add       1             Conta_segmenti
051400130410     c                   add       1             Conta_segmTot
051500100813      *
051600100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
051700100120      * ?  Anche se non utilizzati.
051800100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
051900100317     c                   clear                   seg_imprevisto
052000100720     c                   clear                   trovato_seg
052100100716     C                   clear                   keyUNBCLI
052200100716     C                   clear                   keyTIPOMSG
052300100716     C                   clear                   keyVERSION
052400100716     C                   clear                   keyRELEASE
052500100716     C                   clear                   keyAGENCY
052600100716     C                   clear                   keyASSOCIA
052700110530      **--------
052800110530      **--   PASSA sempre l'UNB mittente se ci fossero delle eccezioni da considerare
052900110530     c                   movel     UNB_Mittente  keyUNBCLI
053000100720      **--------
053100100720      *  Ritorna la DS GENERICA oppure l'errore
053200100720      *  Chiamata generica x decodificare il singolo segmento letto
053300100720     c                   call      'TRTCT00R1'
053400100720      * input
053500100720     c                   parm                    tipo_seg
053600100720     c                   parm                    segmento
053700100720     C                   parm                    keyUNBCLI
053800100720     C                   parm                    keyTIPOMSG
053900100720     C                   parm                    keyVERSION
054000100720     C                   parm                    keyRELEASE
054100100720     C                   parm                    keyAGENCY
054200100720     C                   parm                    keyASSOCIA
054300100720      *output
054400100720     c                   parm                    Errore
054500100720     c                   parm                    DsGenerica
054600100720     c                   parm                    Valori_inErr
054700100720     c                   parm                    Descr_Errore
054800100720     c                   parm                    trovato_seg
054900120424      *
055000120424      *>>>--->>>                                                    <<<---<<<
055100120424      *   Se ci sono dei Segmenti Particolari da NON considerare
055200130410     c                   IF        trovato_seg ='N'
055300120424      *
055400120424      *   ("SGM" -> NON ESISTE NELL'EDI  come TIPO SEGMENTO)
055500120424     C                   movel(p)  'SGM'         etbSGM
055600120424     C                   movel(p)  Tipo_Seg      etbVALSGM
055700120424     c                   exsr      In_TAB_EDSTBL
055800120424      *
055900120424     C*  Quindi sono dichiarati nel file delle eccezioni
056000130410     C                   If        Quale_campo ='TIPSGM' and trovata_EDSTBL ='S'
056100120424     c                   eval      trovato_seg ='S'
056200120424      * serve per bypassare eventuali records  NON in mappatura e non segnalare
056300120424      * errori sul TIVIN.
056400120424     c                   endIf
056500130410     c
056600120424     c                   end
056700120424      *>>>--->>>                                                    <<<---<<<
056800100708      *** ------------------------------------
056900100708      *   Decodifica il singolo segmento LETTO
057000100708      *** ------------------------------------
057100100120     c                   select
057200100120      *
057300130410     c                   when      trovato_seg ='N'
057400100720     c                   move      'S'           seg_imprevisto
057500100720     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
057600100720     c                   exsr      Error_DET
057700100720     c                   goto      end_Decod
057800100720      **--------
057900100120     c                   when      tipo_seg = 'UNA'
058000100720     c                   movel(p)  DsGenerica    EDS00UNA
058100130410     C                   EXSR      DATI_UNA
058200130410      **--------
058300100120     c                   when      tipo_seg = 'UNB'
058400100720     c                   movel(p)  DsGenerica    EDS00UNB
058500100120     C                   EXSR      DATI_UNB
058600100120      * --------
058700100120     c                   when      tipo_seg = 'UNH'
058800100720     c                   movel(p)  DsGenerica    EDS00UNH
058900100120     C                   EXSR      DATI_UNH
059000100120      * --------
059100100120     c                   when      tipo_seg = 'BGM'
059200100720     c                   movel(p)  DsGenerica    EDS00BGM
059300100316     C                   EXSR      DATI_BGM
059400100120      **--------
059500100120     c                   when      tipo_seg = 'DTM'
059600100720     c                   movel(p)  DsGenerica    EDS00DTM
059700100120     C                   EXSR      DATI_DTM
059800100120      * --------
059900100120     c                   when      tipo_seg = 'TSR'
060000100720     c                   movel(p)  DsGenerica    EDS00TSR
060100100316     C                   EXSR      DATI_TSR
060200100120      **--------
060300100120     c                   when      tipo_seg = 'RFF'
060400100720     c                   movel(p)  DsGenerica    EDS00RFF
060500100120     C                   EXSR      DATI_RFF
060600100316      **--------
060700100316     c                   when      tipo_seg = 'LOC'
060800100720     c                   movel(p)  DsGenerica    EDS00LOC
060900100316     C                   EXSR      DATI_LOC
061000100316      **--------
061100100120     c                   when      tipo_seg = 'TDT'
061200100720     c                   movel(p)  DsGenerica    EDS00TDT
061300100316     C                   EXSR      DATI_TDT
061400100120      **--------
061500100120     c                   when      tipo_seg = 'FTX'
061600100720     c                   movel(p)  DsGenerica    EDS00FTX
061700100120     C                   EXSR      DATI_FTX
061800100120      **--------
061900100120     c                   when      tipo_seg = 'NAD'
062000100720     c                   movel(p)  DsGenerica    EDS00NAD
062100100120     C                   EXSR      DATI_NAD
062200100120      **--------
062300100120     c                   when      tipo_seg = 'CTA'
062400100720     c                   movel(p)  DsGenerica    EDS00CTA
062500100120     C                   EXSR      DATI_CTA
062600100120      **--------
062700100120     c                   when      tipo_seg = 'COM'
062800100720     c                   movel(p)  DsGenerica    EDS00COM
062900100120     C                   EXSR      DATI_COM
063000100120      **--------
063100100120     c                   when      tipo_seg = 'GID'
063200100720     c                   movel(p)  DsGenerica    EDS00GID
063300100120     C                   EXSR      DATI_GID
063400100120      **--------
063500100120     c                   when      tipo_seg = 'MEA'
063600100720     c                   movel(p)  DsGenerica    EDS00MEA
063700100120     C                   EXSR      DATI_MEA
063800100120      **--------
063900100120     c                   when      tipo_seg = 'UNT'
064000100720     c                   movel(p)  DsGenerica    EDS00UNT
064100100120     C                   EXSR      DATI_UNT
064200100120      **--------
064300100120     c                   when      tipo_seg = 'UNZ'
064400100720     c                   movel(p)  DsGenerica    EDS00UNZ
064500100120     C                   EXSR      DATI_UNZ
064600110105      **--------
064700130410     c                   When      tipo_seg = *blank
064800100120      **--------
064900100120     c                   OTHER
065000120424      **--------
065100130410     c                   if        trovata_EDSTBL <>'S'
065200100120     c                   move      'S'           seg_imprevisto
065300100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
065400100120     c                   exsr      Error_DET
065500100120     c                   goto      end_Decod
065600110110     c                   end
065700100120      **--------
065800100120     c                   endSL
065900100120      **
066000100120     c     end_Decod     endSR
066100090210     C*----------------------------------------------------------------
066200130410      *? DECODIFICA DATI UNA
066300090210     C*----------------------------------------------------------------
066400130410     C     DATI_UNA      BEGSR
066500130410      *
066600130410     c                   z-add     vnrec         IniDET_RECord
066700130410     c                   clear                   FinDET_RECord
066800130410      *
066900130410     c                   endSR
067000130410     C*----------------------------------------------------------------
067100130410      *? DECODIFICA DATI UNB
067200130410     C*----------------------------------------------------------------
067300130410     C     DATI_UNB      BEGSR
067400130410      *
067500130411     C                   clear                   PT_key_savXX
067600130411     C                   clear                   PT_poe
067700130411     C                   clear                   PT_cor
067800130410     C                   eval      PT_con_piu_Nazioni = *blank
067900130410     c                   move      *blank        Naz_salvata       3
068000130410     C*
068100130410     c                   clear                   UNB_Generico
068200130410     c                   clear                   UNB_NrTrasmiss
068300130410     c                   clear                   UNZ_NrTrasmiss
068400090210      *
068500100701      * identificativo Messaggio
068600100701     C                   eval      UNB_NrTrasmiss = unb0020
068700100701      *
068800100813     C                   MOVEL     unb0017       Anno2             2 0
068900100813     C                   z-add     2000          Anno4             4 0
069000100813     C                   add       Anno2         Anno4
069100100813     C                   MOVE      unb0017       WGG               2 0
069200100813     C                   MOVE      unb0017       Data_messag       6 0          * WDTMSG
069300100813     C                   MOVE      Anno2         Data_messag
069400100813     C                   MOVEL     WGG           Data_messag
069500100813     C                   MOVE      unb0017       Data_prepara      8 0          * WDTPRE
069600100813     C                   MOVEL     '20'          Data_prepara                   * WDTPRE
069700100813     C                   MOVE      unb0019       Ora__prepara      4 0          * WHMPRE
069800100813     c****
069900100813     c                   clear                   Mittente         35
070000100813     c                   clear                   Mitt_Qualifier    4
070100100813     c                   clear                   Id_Messaggio     14
070200100813     C                   eval      Mittente       = unb0004
070300100813     C                   eval      Mitt_qualifier = unb000720
070400100813     C                   eval      Id_Messaggio   = UNB0022
070500100813      *
070600090209      ** ---------------------------------------------------------------
070700090209      * quindi 1° cosa:
070800090209      *  trascodifica il Codice UNB del Mittente da codice + qualificatore
070900090209      *     senza questo il messaggio NON è trascodificabile
071000100630     C                   MOVEL     unb0004       UNB_diWork
071100100630     C                   MOVE      unb000720     UNB_diWork
071200100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
071300100630     C                   Z-ADD     1             Nr_PT
071400100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
071500090209      *
071600130410      * Poi verifica se UNB è fra quelli parzializzati con la Nazione:
071700100317     C                   If        *IN32 = *off
071800100630     C                   eval      Nr_PT = 1
071900100630     c                   clear                   UNB_diWork
072000100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
072100100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
072200100317      *
072300100317      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
072400100317      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
072500100317      *  per cercare di individuare con certezza il codice della tabella PT in base
072600100317      *  all'UNB specifico. (es.: A02YR    ES/PT)
072700100317     C                   If        *IN32 = *on
072800100317      *
072900100317      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
073000100317      * ?a decodificare il mittente per attribuire la giusta linea/cliente
073100100317      * ?  su Partner che hanno + linee con + nazioni.
073200100813      *  se è considerato Parziale
073300100813      *   Salva il codice generico
073400100630     c                   eval      UNB_parziale = 'S'
073500100813     c                   eval      UNB_Generico = PT_Parz(Nr_PT)
073600100813      *             *------------------------------*
073700100813     c                   exsr      cerca_Mittente
073800100813      *             *------------------------------*
073900100813     C                   eval          Naz_salvata = Naz_mittente
074000100813     c                   Endif
074100100813     c                   Endif
074200100813      *
074300100813      *  Esito ricerca della tabella PT se Partner decodificato oppure NON Trovato
074400100813      *             *------------------------------*
074500100813     c                   exsr      esito_tab_PT
074600100813      *             *------------------------------*
074700100813      **
074800100813      **  Memorizza per reimpostare in seguito se necessario dopo un cambiamento
074900100813     C                   If        PT_trovata = 'S'
075000100813      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
075100100813      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
075200100813      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
075300100813      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
075400100813      * ?- Salva l'indice delle schiere per riaggancarla in seguito        ------ */
075500130411     C                   z-add     Nr_PT         PT_key_savXX
075600100813      * ?- Se ha più LINEE da gestire                                      ------ */
075700100813     C                   eval      PT_con_piu_Nazioni = §puPLN
075800100813     c                   end
075900090209      **
076000090210     c     end_UNB       endSR
076100130410     C*----------------------------------------------------------------
076200130410      *? cerca Nazione del primo Mittente nella sequenza dettagli
076300130410     C*----------------------------------------------------------------
076400130410     C     cerca_MittenteBEGSR
076500130410      *
076600130410     c                   z-add     vnrec         rec_posiz         9 0
076700130410     c                   move      *blank        Naz_mittente      3
076800130410     c                   movel(p)  UNB_Generico  PT_Parziale      35
076900130410     c                   move      'N'           OK_Nazione        1
077000130410      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
077100130410     c                   call      'TRTCT98R1'
077200130410     c                   parm                    rec_posiz
077300130410     c                   parm                    PT_Parziale
077400130410     c                   parm                    Naz_mittente
077500130410     c                   parm                    Ok_Nazione
077600130410     c                   parm                    User_msg
077700130410     C                   parm                    keyUNBCLI
077800130410     C                   parm                    keyTIPOMSG
077900130410     C                   parm                    keyVERSION
078000130410     C                   parm                    keyRELEASE
078100130410     C                   parm                    keyAGENCY
078200130410     C                   parm                    keyASSOCIA
078300130410      *
078400130410      *  Re-inizializza l'indicatore di trovato
078500130410     c                   setoff                                       32
078600130410      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
078700130410      *   da decodificare come UNB.
078800130410     c                   if        Ok_Nazione = 'S'
078900130410     C                   eval      UNB_diWork = UNB_generico + Naz_mittente
079000130410     C                   eval      Nr_PT = 1
079100130410     C     UNB_diWork    lookup    PT_key(Nr_PT)                          32
079200130410     c                   Endif
079300130410      *
079400130410     c                   endSR
079500130410     C*----------------------------------------------------------------
079600130410      *? esito ricerca tabella PT decodificata oppure NO
079700130410     C*----------------------------------------------------------------
079800130410     C     esito_tab_PT  BEGSR
079900130410     C*
080000130410      *  INIZIALIZZA  dati fondamentali messaggio
080100130410     C                   clear                   EDIDSPT
080200130410     C                   clear                   EDIDSPU
080300130410     C                   clear                   EDIDSPV
080400130410     C                   clear                   EDIDSPZ
080500130410      *
080600130410     C                   move      'N'           PT_trovata
080700130410     C   32              move      'S'           PT_trovata
080800130410      *
080900130410      * ******  Errore generale sul Messaggio
081000130410      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
081100130410      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Brt.it
081200130410     C                   If        PT_trovata = 'N'
081300130410      * Msg di allerta Traduzione
081400130410     C                   EVAL      Indirizzi = CED_Bart
081500130410???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTMBP'
081600130410     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
081700130410     C                             IFTMBP D99B import O.R.M. EDI - messaggio -
081800130410     C                             con UNB non presente in tabella PT controlla-
081900130410     C                             re EDI ricevuto su UPLOAD.'
082000130410      *
082100130410     c                   exsr      invio_mail
082200130410     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
082300130410     C                   eval      vinFLG = '2'
082400130410     c                   eval      Almeno_Un_Due ='S'
082500130410     C                   eval      se_errore   = 'S'
082600130410      *
082700130410      *             *------------------------------*
082800130410     C                   eLSe
082900130410      *             *------------------------------*
083000130410     c                   exsr      Se_Trovata_PT
083100130410     c                   Endif
083200130410      *
083300130410     c                   endSR
083400130410     C*----------------------------------------------------------------
083500130410      *? DECODIFICA DATI UNB
083600130410     C*----------------------------------------------------------------
083700130410     C     Se_Trovata_PT BEGSR
083800130410      *
083900130410      * Se tutto OK su tab.PT:
084000130410     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
084100130410     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
084200130410     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
084300130410     C                   MOVEL     PZ_uni(Nr_PT) EDIDSPZ
084400130410      *
084500130410      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
084600130410      *  ricevuti:
084700130410     c                   movel     §PVema        mail_ind         44
084800130410      *
084900130410      * ?imposta gli indirizzi mail se interni a BRT o esterni
085000130410     c                   if        mail_ind <> *blank
085100130410     c                   exsr      imp_ADDR_mail
085200130410     c                   end
085300130410      *
085400130410     C                   move      §puTS         PU_TipoServ
085500130410     C                   move      §puSS         PU_SpecServ
085600130410      *
085700130411     C                   Z-ADD     §PTKSC        PT_cor
085800130411     C                   z-add     §PTLNP        PT_poe
085900130410      *
086000130410      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
086100130410     C                   clear                   ACORAG
086200130410     C                   clear                   INDCAE
086300130410     C                   clear                   INDCAP
086400130410     C                   clear                   INDSTA
086500130410      * Decodifico partner
086600130410     C                   Z-ADD     1             KKUT
086700130410     C                   Z-ADD     KCI           KKCC
086800130410     C                   MOVE      §PTKSC        KKSC
086900130410     C     KACO          CHAIN     CNACO00F                           31
087000130410     C     KIND          CHAIN     CNIND00F                           31
087100130410      *
087200130410     c                   endSR
087300130410     c*==================================================================*
087400130410      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
087500130410     c*==================================================================*
087600130410     c     Imp_ADDR_mail begsr
087700130410      *
087800130410     c                   clear                   Indirizzi
087900130410      *
088000130410      *  Lunghezza indirizzi presenti
088100130410     c                   eval      Lunghezza_Indirizzo =
088200130410     c                                       %Len(%TrimR(Mail_Ind))
088300130410     c                   z-add     1             al_char           3 0
088400130410     c                   z-add     1             dal_char          3 0
088500130410     c                   z-add     1             tot_char          3 0
088600130410      *
088700130410     c     successivo    tag
088800130410      *
088900130410      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
089000130410      *   e aggiunge il dominio Brt + il (;) separatore
089100130410     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
089200130410     c                   eval      tot_char = al_char - dal_char
089300130410     c                   z-add     dal_char      dalCarattere
089400130410     c                   z-add     tot_char      xTOTcaratteri
089500130410      *
089600130410     c                   clear                   Indir_BRT        40
089700130410     c                   clear                   Indir_Parz       20
089800130410     c                   eval      Indir_Parz =
089900130410     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
090000130410      *
090100130410     c                   eval      Indir_BRT    = %Trim(Indir_Parz) +
090200130410     c                             %Trim(bart_it)
090300130410     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_BRT
090400130410      *
090500130410     c                   if        al_char = Lunghezza_Indirizzo
090600130410     c                   goto      fine_indmail
090700130410     c                   else
090800130410     c                   eval      dal_char = ( al_char + 1 )
090900130410     c                   goto      successivo
091000130410     c                   end
091100130410      *
091200130410     C     fine_indmail  TAG
091300130410      *
091400130410     C                   ENDSR
091500130410     c*==================================================================*
091600130410      * Manda un Msg x E-mail
091700130410     c*==================================================================*
091800130410     c     Invio_Mail    begsr
091900130410      *
092000130410     C*   Alert_mail : invio Mail a CED@Brt.it                  *
092100130410     C* Inizializzo variabili
092200130410     C                   movel     *blanks       wrkEml          253
092300130410     C                   movel     *blanks       wrkMsg         5000
092400130410     C                   movel     *blanks       wrkOgg           44
092500130410      *
092600130410     C* Valorizzo i campi della e-m@ail - indirizzo
092700130410     C*******            eval      wrkEml = 'Andrea.Bertocchi@Brt.it'
092800130410     C                   eval      wrkEml = Indirizzi
092900130410     C                   eval      wrkOgg = Oggetto
093000130410     C                   eval      wrkMsg = Messaggio
093100130410      ** *****
093200130410     C                   call(e)   'TRTCT00R2'
093300130410     C                   parm                    wrkEml
093400130410     C                   parm                    wrkOgg
093500130410     C                   parm                    wrkMsg
093600130410     C*
093700130410     C                   ENDSR
093800090227      * ?================================================================== */
093900090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
094000090227      * ?================================================================== */
094100090227     C     DATI_UNH      BEGSR
094200100316      *
094300130410      * x singolo Dettaglio pulisce i campi
094400130410      **?              /*---------------------- */
094500130410     c                   exsr      INIT_dettaglio
094600130410      **?              /*---------------------- */
094700100701      *
094800090211      ** Riferimento del cliente
094900100701     C                   MOVEL     unh0062       UNH_Rifer_Msg
095000100701      * ?                                       ------------
095100100701      * ?                                       ------------
095200100701     C                   MOVEL     UNH_Rifer_Msg test_num         35
095300100701     C     ' '           SCAN      UNH_Rifer_Msg lunghezza_num     3 0
095400100701     C                   SUB       1             lunghezza_num
095500100701     C                   EXSR      Se_Numerico
095600100701     C                   If          Campo_Numerico = 'S'                          Ctrl numerico
095700090211      * ?                                       --------
095800090211      * ?                                       --------
095900090211     C                   END
096000090303      **
096100090210     c     end_UNH       endSR
096200130410      * ?================================================================== */
096300130410     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
096400130410      * ?================================================================== */
096500130410     C     INIT_dettaglioBEGSR
096600130410      **
096700130411      *  Pulizia dei records            *-----------------------------*
096800130411      * ?                               *-----------------------------*
096900130411     C                   CLEAR                   FnVAO000
097000130411      * ?                               *-----------------------------*
097100130411      *                                 *-----------------------------*
097200130410      **
097300130410      *   Inizia il conteggio delle righe e dei dettagli
097400130410     c                   z-add     1             Conta_segmenti    5 0
097500130410     c                   eval      Contatore_Dettagli = 1 + Contatore_Dettagli
097600130410      *
097700130410     C*  Inizializza variabili
097800130410      *   Codici identificativi della Testata del messaggio
097900130410     c                   clear                   UNH_Rifer_Msg
098000130410     c                   clear                   BGM_Id_Testata
098100130410      *
098200130410     ** Inizializza campo di work
098300130410     c                   eval      GID_aTotale = 'N'
098400130410     c                   move      *zeros        vaoCOR
098500130411     C                   movel     PT_cor        vaoCOR
098600130411     C                   z-add     PT_poe        vaoPOE
098700130411      *
098800130411     C                   clear                   RFF_RifRitiro
098900130411     C                   clear                   Rifer_Alfanum    35
099000130411     C                   clear                   Rifer_Cliente    35
099100130411     C                   clear                   Rifer_Numeric    35
099200130411     C                   clear                   Tipo_Rifer        3
099300130410      *
099400130410     C*  Azzero codice cliente - tariffa
099500130411     C                   clear                   RFF_FF_ksc
099600130411     C                   clear                   RFF_FF_tar
099700130411     C                   clear                   RFF_FF_lnp
099800130411     C                   clear                   RFF_FF_nrs
099900130411     C                   clear                   RFF_FF_ctm
100000130411     C                   clear                   RFF_FF_bs1
100100130411     C                   clear                   RFF_FF_nsp                     *blk/S
100200130411     C                   clear                   RFF_FF_tic
100300130411     C*  Azzero codice cliente - tariffa
100400130411     C                   Z-ADD     0             ToT_Colli        16 0
100500130411     C                   Z-ADD     0             ToT_PesoLordo    16 2          ???
100600130411     C                   Z-ADD     0             ToT_PesoNetto    16 2          ???
100700130410      *
100800130410     C                   clear                   Note_1           35
100900130410     C                   clear                   Note_2           35
101000130410      *
101100130410      * Un errore nel dettaglio
101200130410     C                   clear                   Err_in_detta      1
101300130410     c                   clear                   errori_in_Wrt     1
101400130410      *
101500130410     c                   endSR
101600100701      * ?================================================================== */
101700100701      *? dati iniziali   BGM
101800100701      * ?================================================================== */
101900100701     C     DATI_BGM      BEGSR
102000100701      *
102100100701      *  identificativo testata
102200100706     c                   eval       BGM_Id_Testata = BGM1004
102300100701      **
102400100701     c                   endSR
102500130411      * ?================================================================== */
102600130411     C*? Imposta i campi del VAB e del VAT da scrivere a rottura dettaglio
102700130411      * ?================================================================== */
102800130411     C     DATI_DTM      BEGSR
102900130411      *
103000130411      *   Cerca la decodifica del dato in TABELLA
103100130411     C                   movel(p)  Tipo_Seg      etbSGM
103200130411     C                   movel(p)  dtm2005       etbVALSGM
103300130411     c                   exsr      In_TAB_EDSTBL
103400130411      *
103500130411     C*  Data Richiesta RITIRO
103600130411     C                   If        Quale_campo ='VAODAR'
103700130411     c                              and dtm2380 <> *blank
103800130411      **
103900130411     C                   MOVEL     dtm2380       Work_Data__35    35            Data
104000130411     C                   MOVEL     dtm2379       Work_Format_3     3            Formato
104100130411     C                   EXSR      Converte_Data
104200130411     C                   MOVEL     WCAMG         WOggi_CMR         8 0          Data CMR
104300130411     C                   MOVEL     WCAMG         VAoDAR
104400130411     C                   MOVEL     WHMS          VAoORR
104500130411     C     WERRO         IFEQ      'S'
104600130411     C                   eval      vinMSG = 'DTM ( Data_Rich.Ritiro ' +
104700130411     C                             dtm2005  +
104800130411     C                              ') data errata'
104900130411     c                   exsr      Error_DET
105000130411     c                   goto      end_DTM
105100130411     C                   END
105200130411     C                   END
105300130411     C*
105400130411     C* Data Documento
105500130411     C                   If        Quale_campo ='DATDOC'
105600130411     c                              and dtm2380 <> *blank
105700130411      **
105800130411     C                   MOVEL     dtm2380       Work_Data__35                  Data
105900130411     C                   MOVEL     dtm2379       Work_Format_3                  Formato
106000130411     C                   EXSR      Converte_Data
106100130411     C                   MOVEL     WCAMG         vaodao                         Data
106200130411     C                   MOVEL     WHMS          vaooao                         Ora
106300130412     c                   if        vaooao = 0
106400130412     c                   eval      vaooao = 1200
106500130412     c                   end
106600130411      **
106700130411     C     WERRO         IFEQ      'S'
106800130411     C                   eval      vinMSG = 'DTM ( Data Documento ' +
106900130411     C                             dtm2005  +
107000130411     C                              ') data errata'
107100130411     c                   exsr      Error_DET
107200130411     c                   goto      end_DTM
107300130411     C                   END
107400130411     C                   END
107500130411      **
107600130411     c     end_DTM       endSR
107700130411      * ?================================================================== */
107800130411     C*? Imposta informazioni sul dettaglio del trasporto
107900130411      * ?================================================================== */
108000130411     C     DATI_TDT      BEGSR
108100130411     C*
108200130411      **
108300130411     c     end_TDT       endSR
108400130411      * ?================================================================== */
108500130411     C*?  Tipo Servizio
108600130411      * ?================================================================== */
108700130411     C     DATI_TSR      BEGSR
108800130411      ***
108900130411     C*  Reperisco priorità spedizione:
109000130411     C                   Z-ADD     1             X
109100130411      ***
109200130411     c                   movel(p)  TSR4219       Work_Data__35
109300130411     c                   move      PU_TipoServ   Work_Data__35
109400130411     C     Work_Data__35 LOOKUP    Key_TipServ(X)                         32
109500130411     C     *IN32         IFEQ      '1'
109600130411      ***
109700130411      ***  non serve decodificare  il tipo di trasporto (è solo via camion)
109800130411      ***
109900130411     C                   ELSE
110000130411      **  Il tipo bolla non è presente
110100130411     C                   eval      vinMSG = 'TSR non in tabella'
110200130411     c*********          exsr      Error_DET
110300130411     c*********          goto      end_TSR
110400130411     C                   END
110500130411     C*
110600130411     C*--------------------------------
110700130411     C*  Decodifico servizi speciali
110800130411     C*--------------------------------
110900130411     C     TSR7085       IFNE      *BLANKS
111000130411     C     TSR7085       ANDNE     *LOVAL
111100130411      *
111200130411     C                   Z-ADD     1             X
111300130411     c                   movel(p)  TSR7085       Work_Data__35
111400130411     c                   move      PU_SpecServ   Work_Data__35
111500130411     C     Work_Data__35 LOOKUP    SpecialServ(X)                         32
111600130411     C     *IN32         IFEQ      '1'
111700130411     C                   eval       EDIDSSS = UNI_ServSpec(X)
111800130411      *
111900130411      * x Appuntamento   (Booking Ins)
112000130411     C     §SSNOT        IFEQ      'A'
112100130411     C                   eval      dORM01 = VAoFLO
112200130411      * ORM  commissionato
112300130411     C                   MOVEL     'S'           §orCOMC
112400130411     C                   movel     dORM01        VAoFLO
112500130411     C                   END
112600130411      *
112700130411      * x PreNoon - Mattina
112800130411     C     §SSNOT        IFEQ      'S'
112900130411     C                   MOVEL     'M'           VAoRMP
113000130411     C                   END
113100130411      *
113200130411      * x FERMO DEPOSITO  --> codice "GWC" - GOODS WAIT COLLECTION
113300130411     C     §SSTPS        IFEQ      'F'
113400130411     C                   MOVEL     'S'           VAoFFD
113500130411     C                   END
113600130411      *
113700130411     C                   ELSE
113800130411      *
113900130411      **  Il tipo bolla non è presente
114000130411     C                   eval      vinMSG = 'TSR ServSpeciali non in tab.SS'
114100130411     c*******            exsr      Error_DET
114200130411     c*******            goto      end_TSR
114300130411      *
114400130411     C                   END
114500130411      *
114600130411     C                   END
114700130411      **
114800130411     c     end_TSR       endSR
114900130411      * ?================================================================== */
115000130411     C*? Imposta la Località
115100130411      * ?================================================================== */
115200130411     C     DATI_LOC      BEGSR
115300130411     C*
115400130411      **
115500130411     c     end_LOC       endSR
115600100701      * ?================================================================== */
115700100701     C*?  i Termini di spedizione in FRANCO o ASSEGNATO
115800100701      * ?================================================================== */
115900100701     C     DATI_RFF      BEGSR
116000120424      *
116100130411      *   Cerca la decodifica del dato in TABELLA
116200120424     C                   movel(p)  Tipo_Seg      etbSGM
116300120424     C                   movel(p)  RFF1153       etbVALSGM
116400120424     c                   exsr      In_TAB_EDSTBL
116500100701      **
116600120629      * ?Codice Cliente di riferimento in Testata
116700130411     C                   if        RFF1153 = Riferimento_Cliente_FF
116800120629     C                              and RFF1154 <> *BLANKS
116900120629     c                               or
117000120629     C                             Quale_campo ='RFFCLI'  and
117100120629     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
117200120629      *>>>>>>>>
117300120629     C*------------------
117400120629     C                   movel     RFF1154       Rifer_Cliente
117500120629      * Se ho RFF+FF e trovo il cod. cliente prendo LE DEFINIZIONI del Cliente
117600120629     c                   exsr      CL_Particolare
117700120629     C*------------------
117800120629      *>>>>>>>>
117900120629     C                   ENDIF
118000100701      **
118100100701      * ?   se in Dettaglio   Riferimento singola spedizione
118200100701      *  Trascodifico riferimento spedizione
118300120424      **  o con altro riferimento diverso dall'AGE che indica la stessa cosa
118400130411     c                   if        RFF1153 = Riferimento_Ordine      and
118500100701     c                                RFF1154 <> *BLANKS
118600120424     c                               or
118700130411     C                             Quale_campo ='RFFCR '  and
118800120424     c                                RFF1153 <> *blank and RFF1154 <> *BLANKS
118900130411      * Solo il primo riferimento
119000130411     C                   if           Rifer_Alfanum = *blank
119100130411     C                   movel(p)  RFF1154       Rifer_Alfanum
119200130411     C                   movel(p)  RFF1154       VAoRFA
119300130411     c                   end
119400100701     c                   end
119500100701      *
119600100701     c     end_RFF       endSR
119700130411      * ?------------------------------------------------------------------ */
119800130411     C*  Ricerca dell RFF+FF: su tabella CL per impostare altro codice Cliente
119900130411      * ?------------------------------------------------------------------ */
120000130411     C     CL_ParticolarebegSR
120100130411      *
120200130411     C     Rifer_Cliente IFNE      *BLANKS
120300130411      * ------>>>
120400130411     C                   eval      XX = 1
120500130411     C                   movel     §PTKSC        Key_CL           35
120600130411     C                   movel     Rifer_Cliente WCOM28           28
120700130411     C                   move      WCOM28        Key_CL
120800130411      *
120900130411     C     Key_CL        lookup    Cod_Tb_CL(XX)                          34
121000130411     C     *IN34         IFEQ      '1'
121100130411     C                   movel     Des_Tb_CL(XX) EDIDSCL
121200130411      *
121300130411      * in dettaglio Messaggio
121400130411     C                   z-add     §CLksc        RFF_FF_ksc        7 0
121500130411     C                   movel     §CLtar        RFF_FF_tar        3
121600130411     C                   z-add     §CLlnp        RFF_FF_lnp        3 0
121700130411     C                   z-add     §CLNRS        RFF_FF_nrs        2 0
121800130411     C                   movel     §CLCTM        RFF_FF_ctm        2
121900130411     C                   movel     §CLBS1        RFF_FF_bs1        1
122000130411     C                   movel     §CLnsp        RFF_FF_nsp        1            *blk/S
122100130411     C                   movel     §CLtic        RFF_FF_tic        2
122200130411     c                   end
122300130411      *
122400130411     C                   ENDIF
122500130411      * ------>>>
122600130411      *
122700130411      * in Dettaglio  preimposto i campi rilevati dalle tabelle
122800130411      *   per clienti particolari (CL)
122900130411      *   se aveva un RFF+FF in dettaglio
123000130411     C                   if          RFF_FF_ksc > 0
123100130411      *
123200130411     c                   move      *zeros        vaoCOR
123300130411     C                   movel     RFF_FF_ksc    vaoCOR
123400130411     C                   movel     RFF_FF_lnp    VAoPOE
123500130411      *
123600130411     c                   end
123700130411     C*
123800130411     C                   ENDSR
123900100705      * ?================================================================== */
124000100705     C*?  FREE TEXT
124100100705      * ?================================================================== */
124200100705     C     DATI_FTX      BEGSR
124300100705      *
124400100705      *   Cerca la decodifica del dato in TABELLA
124500120424     C                   movel(p)  Tipo_Seg      etbSGM
124600100705     C                   movel(p)  ftx4451       etbVALSGM
124700100705     c                   exsr      In_TAB_EDSTBL
124800100705      *
124900130411      * ? solo se si tratta di note descrittive da comunicare
125000130411     c                   if         quale_campo = 'VAONOT'
125100100705     C                   DO        4             X
125200100705      *
125300100705     C     D05(X)        IFNE      *BLANKS
125400100705      *
125500100705     C     Note_1        IFEQ      *BLANKS
125600100705     C                   MOVEL     D05(X)        Note_1
125700100705     C                   MOVE      D05(X)        Note_2
125800100705     C                   ELSE
125900100705     C     Note_2        IFEQ      *BLANKS
126000100705     C                   MOVEL     D05(X)        Note_2
126100100705     C                   END
126200100705     C                   END
126300100705      *
126400100705     C                   ENDif
126500100705      *
126600100705     C                   ENDdo
126700100705     C                   end
126800110907     C*------>>
126900130411      * ?Se Natura Merce
127000130411     c                   if         quale_campo = 'VABNAS'
127100130411     C                   MOVEL     D05(1)        VAoNAM
127200100705     c                   End
127300100705      *
127400130411     C*------>>
127500120613      * ?Consegne Particolari
127600120613      *
127700100705     c     end_FTX       endSR
127800090213      * ?================================================================== */
127900090213     C*?  Anagrafica del mittente e del destinatario
128000090213      * ?================================================================== */
128100091016     C     DATI_NAD      BEGSR
128200130411      *
128300130411      *   per sapere quale è stato decodificato
128400130411     c                   eval      NAD_Destinatario  = *blank
128500130411     c                   eval      NAD_Mittente      = *blank
128600130411     c                   eval      NAD_Ordinante     = *blank
128700100705      *
128800100705      *   Cerca la decodifica del dato in TABELLA
128900120424     C                   movel(p)  Tipo_Seg      etbSGM
129000100705     C                   movel(p)  nad3035       etbVALSGM
129100100705     c                   exsr      In_TAB_EDSTBL
129200130410      *-------
129300130410      * Dati Ordinante
129400100708      *-------
129500130411     C                   IF        Quale_campo = NAD_HI
129600130411     c                   eval       NAD_Ordinante  ='S'
129700130411      *
129800130411     C* Imposto dati Ordinante
129900130411     C                   MOVEL     nad30361      VAoRSO
130000130411     C                   if        nad30362 <> *LOVAL
130100130411     c                   eval      VAoRSO = %Trim(VAoRSO) +' '+ nad30362
130200130411     C                   endif
130300130411      *
130400130411???? c                   if        VAoRSO = *blank
130500130411???? C                   MOVEL     nad31241      VAoRSO
130600130411???? C                   if        nad31242 <> *LOVAL
130700130411     c                   eval      VAoRSO = %Trim(VAoRSO) +' '+ nad31242
130800130411???? C                   endif
130900130411     c                   end
131000130411      *
131100130411      *   Indirizzo  città/Località
131200130411     C                   MOVEL     nad30421      VAoINO
131300130411     C                   MOVEL     nad3164       VAoLOO
131400130411      *
131500130411     C* Reperisco nazione corrispondente destinatario
131600130411     C                   Z-ADD     1             naz               3 0
131700130411     C                   MOVEL     *BLANKS       VAoNAO
131800130411     C                   Z-ADD     0             WFLCPF
131900130411     c                   eval      *in32 = *off
132000130411     C                   if        nad3207 <> *BLANKS
132100130411     C     nad3207       LOOKUP    Iso03(naz)                             32
132200130411     C                   If        *in32 = *on
132300130411      * per reperire il CAP corretto
132400130411     C                   MOVEL     Naz03(naz)    VAoNAO
132500130411     C                   MOVEL     cpf03(naz)    WFLCPF            2 0
132600130411     c                   Endif
132700130411     C                   endif
132800130411      **
132900130411     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
133000130411     c                                         %len(%trim(NAD30422))
133100130411???? c                   eval      NAD_Lunghezza_Indirizzo =
133200130411     c                                         %len(%trim(VAoINO))
133300130411???? c                   eval      NAD_Lunghezza_ragSociale =
133400130411     c                                         %len(%trim(VAoRSO))
133500130411      *
133600130411      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
133700130411      *  lo aggiunge nel proseguimento della Ragione Sociale
133800130411     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
133900130411      **
134000130411     c                             and  %len(VAoINO) <
134100130411     c                                  NAD_Lunghezza_Indirizzo +
134200130411     c                                  NAD_Lunghezza_Indir_Aggiuntivo
134300130411      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
134400130411      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
134500130411     c                             and  %len(VAoRSO) -
134600130411     c                                  NAD_Lunghezza_ragSociale -
134700130411     c                                  NAD_Lunghezza_Indir_Aggiuntivo
134800130411     c                                  >= 0
134900130411      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
135000130411     c                             and  NAD_Lunghezza_ragSociale <
135100130411     c                                  NAD_Lunghezza_Indirizzo
135200130411      *
135300130411     C     '    '        SCAN      VAoRSO        WI
135400130411     c                   if        %Found
135500130411     C                   MOVEA     VAoRSO        LOD
135600130411     C                   ADD       1             WI
135700130411     C                   MOVEA     NAD30422      LOD(WI)
135800130411     C                   MOVEA     LOD           VAoRSO
135900130411     c                   end
136000130411      *
136100130411      * Altrimenti lo aggiunge direttamente sulla VIA
136200130411     c                   else
136300130411      *
136400130411      * Se NAD30422  non è vuoto
136500130411      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
136600130411      *
136700130411     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
136800130411     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
136900130411     C* Verifico se nella località c'è dello spazio
137000130411     C     '    '        SCAN      VAoINO        WI
137100130411     c                   if        %Found
137200130411     C                   MOVEA     VAoINO        LOD
137300130411     C                   ADD       1             WI
137400130411     C                   MOVEA     NAD30422      LOD(WI)
137500130411     C                   MOVEA     LOD           VAoINO
137600130411     c                   else
137700130411      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
137800130411     C     '    '        SCAN      VAoLOO        WI                4 0
137900130411     c                   if        %Found
138000130411     C                   MOVEA     VAoLOO        LOD
138100130411     C                   ADD       1             WI
138200130411     C                   MOVEA     NAD30422      LOD(WI)
138300130411     C                   MOVEA     LOD           VAoLOO
138400130411     C                   ENDif
138500130411     C                   endif
138600130411     C                   ENDif
138700130411      *-------
138800130411      *-------
138900130411     c                   end
139000130411      *
139100130411      * Converto CAP
139200130411     C                   EXSR      Converte_CAP
139300130411     c                   eval      VAoCAO = WCAP
139400130411      *
139500130411     C* CAP OBBLIGATORIO : CONTROLLO
139600130411     C                   CLEAR                   DSSI95
139700130411     C                   CLEAR                   DSLV13
139800130411     C                   MOVEL     '3'           I95TCN
139900130411     C                   MOVEL     VAoCAO        I95CAP
140000130411     C                   MOVEL     VAoLOO        I95LOC
140100130411     C* ERRORE AFFIDABILITA = 0
140200130411     C                   MOVEL     'S'           I13AF0
140300130411     C                   MOVEL     'S'           I13AF1
140400130411     C                   z-add     1             E13FZ1
140500130411     C                   CALL      'FNLV13R'
140600130411     C                   PARM                    KPJBA
140700130411     C                   PARM                    DSLV13
140800130411     C                   PARM                    DSSI95
140900130411     C* ERRORE
141000130411     C     O13ERR        IFEQ      *BLANKS
141100130411     C                   MOVEL     O95PRV        VAoPRO
141200130411    3C                   ENDIF
141300130411     C*
141400130411     C*    SEGNALA
141500130411     C                   if          vaoRSO = *blank
141600130411     C                   eval      vinMSG='NAD Ragione Soc.Ordinante assente'
141700130411     c******             exsr      Error_DET
141800130411     c******             goto      end_NAD
141900130411     C                   elseIF      vaoINO = *blank
142000130411     C                   eval      vinMSG = 'NAD Indirizzo Ordinante assente'
142100130411     c*********          exsr      Error_DET
142200130411     c*********          goto      end_NAD
142300130411     C                   elseIF      vaoLOO = *blank
142400130411     C                   eval      vinMSG = 'NAD Località Ordinante assente'
142500130411     c*********          exsr      Error_DET
142600130411     c*********          goto      end_NAD
142700130411     C                   elseIF      vaoCAO = *blank
142800130411     C                   eval      vinMSG = 'MEA (CAP) non trovato'
142900130411     c*********          exsr      Error_DET
143000130411     C*********          goto      end_MEA
143100130411     C                   ENDIF
143200130411      *-------
143300130411      *-------
143400120629      *-------
143500130410      *-------
143600130411      * Dati Mittente
143700130410      *-------
143800130411     C                   elseIF    Quale_campo = NAD_CZ
143900130411     c                   eval       NAD_Mittente   ='S'
144000130411      *
144100130411     C* Imposto dati Mittente
144200130411     C                   MOVEL     nad30361      VAoRSR
144300130411     C                   if        nad30362 <> *LOVAL
144400130411     c                   eval      VAoRSR = %Trim(VAoRSR) +' '+ nad30362
144500130411     C                   endif
144600130411      *
144700130411???? c                   if        VAoRSR = *blank
144800130411???? C                   MOVEL     nad31241      VAoRSR
144900130411???? C                   if        nad31242 <> *LOVAL
145000130411     c                   eval      VAoRSR = %Trim(VAoRSR) +' '+ nad31242
145100130411???? C                   endif
145200130411     c                   end
145300130411      *
145400130411      *   Indirizzo  città/Località
145500130411     C                   MOVEL     nad30421      VAoINR
145600130411     C                   MOVEL     nad3164       VAoLOR
145700130411     C*
145800130411     C* Reperisco nazione corrispondente destinatario
145900130411     C                   Z-ADD     1             naz               3 0
146000130411     C                   MOVEL     *BLANKS       VAoNAR
146100130411     C                   Z-ADD     0             WFLCPF
146200130411     c                   eval      *in32 = *off
146300130411     C                   if        nad3207 <> *BLANKS
146400130411     C     nad3207       LOOKUP    Iso03(naz)                             32
146500130411     C                   If        *in32 = *on
146600130411      * per reperire il CAP corretto
146700130411     C                   MOVEL     Naz03(naz)    VAoNAR
146800130411     C                   MOVEL     cpf03(naz)    WFLCPF            2 0
146900130411     c                   Endif
147000130411     C                   endif
147100130411      *
147200130411     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
147300130411     c                                         %len(%trim(NAD30422))
147400130411???? c                   eval      NAD_Lunghezza_Indirizzo =
147500130411     c                                         %len(%trim(VAoINR))
147600130411???? c                   eval      NAD_Lunghezza_ragSociale =
147700130411     c                                         %len(%trim(VAoRSR))
147800130411      *
147900130411      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
148000130411      *  lo aggiunge nel proseguimento della Ragione Sociale
148100130411     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
148200130411      **
148300130411     c                             and  %len(VAoINR) <
148400130411     c                                  NAD_Lunghezza_Indirizzo +
148500130411     c                                  NAD_Lunghezza_Indir_Aggiuntivo
148600130411      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
148700130411      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
148800130411     c                             and  %len(VAoRSR) -
148900130411     c                                  NAD_Lunghezza_ragSociale -
149000130411     c                                  NAD_Lunghezza_Indir_Aggiuntivo
149100130411     c                                  >= 0
149200130411      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
149300130411     c                             and  NAD_Lunghezza_ragSociale <
149400130411     c                                  NAD_Lunghezza_Indirizzo
149500130411      *
149600130411     C     '    '        SCAN      VAoRSR        WI
149700130411     c                   if        %Found
149800130411     C                   MOVEA     VAoRSR        LOD
149900130411     C                   ADD       1             WI
150000130411     C                   MOVEA     NAD30422      LOD(WI)
150100130411     C                   MOVEA     LOD           VAoRSR
150200130411     c                   end
150300130411      *
150400130411      * Altrimenti lo aggiunge direttamente sulla VIA
150500130411     c                   else
150600130411      *
150700130411      * Se NAD30422  non è vuoto
150800130411      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
150900130411      *
151000130411     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
151100130411     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
151200130411     C* Verifico se nella località c'è dello spazio
151300130411     C     '    '        SCAN      VAoINR        WI
151400130411     c                   if        %Found
151500130411     C                   MOVEA     VAoINR        LOD
151600130411     C                   ADD       1             WI
151700130411     C                   MOVEA     NAD30422      LOD(WI)
151800130411     C                   MOVEA     LOD           VAoINR
151900130411     c                   else
152000130411      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
152100130411     C     '    '        SCAN      VAoLOR        WI                4 0
152200130411     c                   if        %Found
152300130411     C                   MOVEA     VAoLOR        LOD
152400130411     C                   ADD       1             WI
152500130411     C                   MOVEA     NAD30422      LOD(WI)
152600130411     C                   MOVEA     LOD           VAoLOR
152700130411     C                   ENDif
152800130411     C                   endif
152900130411     C                   ENDif
153000130411      *-------
153100130411      *-------
153200130411     c                   end
153300130411      * Converto CAP
153400130411     C                   EXSR      Converte_CAP
153500130411     c                   eval      VAoCAR = WCAP
153600130411      *
153700130411     C* CAP OBBLIGATORIO : CONTROLLO
153800130411     C                   CLEAR                   DSSI95
153900130411     C                   CLEAR                   DSLV13
154000130411     C                   MOVEL     '3'           I95TCN
154100130411     C                   MOVEL     vaocar        I95CAP
154200130411     C                   MOVEL     vaolor        I95LOC
154300130411     C* ERRORE AFFIDABILITA = 0
154400130411     C                   MOVEL     'S'           I13AF0
154500130411     C                   MOVEL     'S'           I13AF1
154600130411     C                   z-add     1             E13FZ1
154700130411     C                   CALL      'FNLV13R'
154800130411     C                   PARM                    KPJBA
154900130411     C                   PARM                    DSLV13
155000130411     C                   PARM                    DSSI95
155100130411     C* ERRORE
155200130411     C     O13ERR        IFEQ      *BLANKS
155300130411     C                   MOVEL     O95PRV        VAoPRR
155400130411    3C                   ENDIF
155500130411     C*
155600130411     C*    SEGNALA
155700130411     C                   if          vaoRSR = *blank
155800130411     C                   eval      vinMSG='NAD Ragione Soc.Mittente assente'
155900130411     c******             exsr      Error_DET
156000130411     c******             goto      end_NAD
156100130411     C                   elseIF      vaoINR = *blank
156200130411     C                   eval      vinMSG = 'NAD Indirizzo Mittente assente'
156300130411     c*********          exsr      Error_DET
156400130411     c*********          goto      end_NAD
156500130411     C                   elseIF      vaoLOR = *blank
156600130411     C                   eval      vinMSG = 'NAD Località Mittente assente'
156700130411     c*********          exsr      Error_DET
156800130411     c*********          goto      end_NAD
156900130411     C                   elseIF      vaoCAR = *blank
157000130411     C                   eval      vinMSG = 'MEA (CAP) non trovato'
157100130411     c*********          exsr      Error_DET
157200130411     C*********          goto      end_MEA
157300130411     C                   ENDIF
157400130411      *-------
157500130410      *-------
157600100705      *-------
157700090210      * Dati destinatario
157800100705      *-------
157900130411     C                   elseIF    Quale_campo =  NAD_CN
158000130411     c                   eval       NAD_Destinatario ='S'
158100130411      *
158200130411     C* Imposto dati destinatario
158300130411     C                   MOVEL     nad30361      VAoRSC
158400130411     C                   if        nad30362 <> *LOVAL
158500130411     c                   eval      VAoRSC = %Trim(VAoRSC) +' '+ nad30362
158600130411     C                   endif
158700130411      *
158800130411???? c                   if        VAoRSC = *blank
158900130411???? C                   MOVEL     nad31241      VAoRSC
159000130411???? C                   if        nad31242 <> *LOVAL
159100130411     c                   eval      VAoRSC = %Trim(VAoRSC) +' '+ nad31242
159200130411???? C                   endif
159300130411     c                   end
159400130411      *
159500130411      *   Indirizzo  città/Località
159600130411     C                   MOVEL     nad30421      VAoINC
159700130411     C                   MOVEL     nad3164       VAoLOC
159800090210      *
159900090210     C* Reperisco nazione corrispondente destinatario
160000100705     C                   Z-ADD     1             naz               3 0
160100130411     C                   MOVEL     *BLANKS       VAoNAC
160200100705     C                   Z-ADD     0             WFLCPF
160300090210     c                   eval      *in32 = *off
160400090210     C                   if        nad3207 <> *BLANKS
160500130410     C     nad3207       LOOKUP    Iso03(naz)                             32
160600100705     C                   If        *in32 = *on
160700100705      * per reperire il CAP corretto
160800130411     C                   MOVEL     Naz03(naz)    VAoNAC
160900130410     C                   MOVEL     cpf03(naz)    WFLCPF            2 0
161000100705     c                   Endif
161100100705     C                   endif
161200111121      *
161300111121     c                   eval      NAD_Lunghezza_Indir_Aggiuntivo =
161400111121     c                                         %len(%trim(NAD30422))
161500111121???? c                   eval      NAD_Lunghezza_Indirizzo =
161600130411     c                                         %len(%trim(VAoINC))
161700130411???? c                   eval      NAD_Lunghezza_ragSociale =
161800130411     c                                         %len(%trim(VAoRSC))
161900111116      *
162000111121      *  se la parte di indirizzo aggiuntivo NON ci sta accodandolo all'indirizzo,
162100111121      *  lo aggiunge nel proseguimento della Ragione Sociale
162200111121     c                   if        NAD_Lunghezza_Indir_Aggiuntivo > 0
162300111121      **
162400130411     c                             and  %len(VAoINC) <
162500111121     c                                  NAD_Lunghezza_Indirizzo +
162600111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
162700111121      *   Quindi se nel campo della VIA non ci sta tutto dell'indicazione aggiuntiva
162800111121      *    dell'indirizzo....allora lo mettiamo accodato alla 2°ragione sociale.
162900130411     c                             and  %len(VAoRSC) -
163000130411     c                                  NAD_Lunghezza_ragSociale -
163100111121     c                                  NAD_Lunghezza_Indir_Aggiuntivo
163200111121     c                                  >= 0
163300111121      *  e se ho più spazio sul prosieguo della 2°Rag.sociale anzichè sulla VIA
163400130411     c                             and  NAD_Lunghezza_ragSociale <
163500111121     c                                  NAD_Lunghezza_Indirizzo
163600111121      *
163700130411     C     '    '        SCAN      VAoRSC        WI
163800111121     c                   if        %Found
163900130411     C                   MOVEA     VAoRSC        LOD
164000111121     C                   ADD       1             WI
164100111121     C                   MOVEA     NAD30422      LOD(WI)
164200130411     C                   MOVEA     LOD           VAoRSC
164300111121     c                   end
164400111121      *
164500111121      * Altrimenti lo aggiunge direttamente sulla VIA
164600111121     c                   else
164700130411      *
164800100705      * Se NAD30422  non è vuoto
164900111116      * si è invertita la vecchia logica ossia prima si tenta di aggiungere alla Via
165000111116      *
165100111116     C*  aggiungo all'INDIRIZZO  quanto di indirizzo ce n'è in più
165200090210     c                   if        NAD30422 <> *blanks and NAD30422 <> *loval
165300100705     C* Verifico se nella località c'è dello spazio
165400130411     C     '    '        SCAN      VAoINC        WI
165500100705     c                   if        %Found
165600130411     C                   MOVEA     VAoINC        LOD
165700090210     C                   ADD       1             WI
165800090210     C                   MOVEA     NAD30422      LOD(WI)
165900130411     C                   MOVEA     LOD           VAoINC
166000100705     c                   else
166100111121      *  Altrimenti se non c'è spazio dopo la via prova a concatenarlo alla località
166200130411     C     '    '        SCAN      VAoLOC        WI                4 0
166300100705     c                   if        %Found
166400130411     C                   MOVEA     VAoLOC        LOD
166500100705     C                   ADD       1             WI
166600100705     C                   MOVEA     NAD30422      LOD(WI)
166700130411     C                   MOVEA     LOD           VAoLOC
166800100705     C                   ENDif
166900130411     C                   endif
167000100705     C                   ENDif
167100130411      *-------
167200130411      *-------
167300111121     c                   end
167400111116      *
167500090210      * Converto CAP
167600100705     C                   EXSR      Converte_CAP
167700130411     c                   eval      VAoCAC = WCAP
167800130411      *
167900130411     C* CAP OBBLIGATORIO : CONTROLLO
168000130411     C                   CLEAR                   DSSI95
168100130411     C                   CLEAR                   DSLV13
168200130411     C                   MOVEL     '3'           I95TCN
168300130411     C                   MOVEL     VAoCAC        I95CAP
168400130411     C                   MOVEL     VAoLOC        I95LOC
168500130411     C* ERRORE AFFIDABILITA = 0
168600130411     C                   MOVEL     'S'           I13AF0
168700130411     C                   MOVEL     'S'           I13AF1
168800130411     C                   z-add     1             E13FZ1
168900130411     C                   CALL      'FNLV13R'
169000130411     C                   PARM                    KPJBA
169100130411     C                   PARM                    DSLV13
169200130411     C                   PARM                    DSSI95
169300130411     C* ERRORE
169400130411     C     O13ERR        IFEQ      *BLANKS
169500130411     C                   MOVEL     O95PRV        VAoPRC
169600130411    3C                   ENDIF
169700130411     C*
169800130411     C*    SEGNALA
169900130411     C                   if          vaoRSC = *blank
170000100728     C                   eval      vinMSG='NAD Ragione Soc.destinatario assente'
170100130315     c******             exsr      Error_DET
170200130315     c******             goto      end_NAD
170300130411     C                   elseIF      vaoINC = *blank
170400100728     C                   eval      vinMSG = 'NAD Indirizzo destinatario assente'
170500101006     c*********          exsr      Error_DET
170600101006     c*********          goto      end_NAD
170700130411     C                   elseIF      vaoLOC = *blank
170800100728     C                   eval      vinMSG = 'NAD Località destinatario assente'
170900101006     c*********          exsr      Error_DET
171000101006     c*********          goto      end_NAD
171100130411     C                   elseIF      vaoCAC = *blank
171200130411     C                   eval      vinMSG = 'MEA (CAP) non trovato'
171300130411     c*********          exsr      Error_DET
171400130411     C*********          goto      end_MEA
171500100728     C                   ENDIF
171600090210      *
171700090210     C                   END
171800090210      *
171900090210     c     end_NAD       endSR
172000090210      * ?================================================================== */
172100090210     C*?  Contatto a cui far riferimento
172200090210      * ?================================================================== */
172300090210     C     DATI_CTA      BEGSR
172400130411      *
172500130411     C     cta3412       ifne      *BLANKS
172600130411      *
172700130411     c                   if        NAD_Destinatario  = 'S'
172800130411     c                   elseif    NAD_Mittente      = 'S'
172900130411     c                   eval          VAoRER = cta3412
173000130411     c                   elseif    NAD_Ordinante     = 'S'
173100130411     c                   end
173200090210      *
173300100701     c                   end
173400090210      *
173500090210     c                   endSR
173600090210      * ?================================================================== */
173700090210     C*?  Contatto telefonico  a cui far riferimento
173800090210      * ?================================================================== */
173900090210     C     DATI_COM      BEGSR
174000090210      *
174100100705      *  memorizza il riferimento di consegna nr.telefonico
174200100705     C                   if        com3148 <> *BLANKS    and
174300100705     C                              com3155 = Telephone_Number
174400130411      *
174500130411     c                   if        NAD_Destinatario  = 'S'
174600130411     c                   elseif    NAD_Mittente      = 'S'
174700130411     c                   eval          VAoTER = com3148
174800130411     c                   elseif    NAD_Ordinante     = 'S'
174900130411     c                   end
175000130411      *
175100130411     c                   end
175200090210      *
175300090209     C                   ENDSR
175400090210      * ?================================================================== */
175500090210     C*?  Dati di dettaglio della spedizione e dei colli
175600090210      * ?================================================================== */
175700090210     C     DATI_GID      BEGSR
175800090210      *
175900130411     C* Se ho totali per spedizione metto nei campi
176000090210     C     gid1496       IFEQ      99999
176100090210     C     gid1496       orEQ      9999
176200100630     c                   eval      GID_aTotale = 'S'
176300130411     C                   z-add     gid722420     VAoNCL
176400090210     C                   ELSE
176500130411      *
176600100630     c                   If        GID_aTotale = 'N'
176700100705     C                   z-add     gid722420     GID_ColliInAdd
176800130411     C                   eval      VAoNCL = VAoNCL + GID_ColliInAdd
176900090210     C                   END
177000090210     C                   END
177100090212      *
177200130411     C                   ADD       VAoNCL        ToT_Colli        16 0
177300090210      *
177400090210     c     end_GID       endSR
177500090210      * ?================================================================== */
177600090210     C*?  Dati di dettaglio misure spedizione e colli
177700090210      * ?================================================================== */
177800090210     C     DATI_MEA      BEGSR
177900090210      *
178000090210     C*  Peso
178100100319     C     mea6311       IFEQ      Weight
178200100319     C     mea6411       ANDEQ     Kilograms
178300090210      *
178400090210     C*  Controllo se ho valore totale o parziale
178500090210      *   sempre il Lordo
178600100319???? C     mea6313       IFEQ      G_Weight
178700100319???? C     mea6313       oreq      Gross_Weight
178800100706???? C     mea6313       oreq      Gross_Weight_E
178900110512???? C     mea6313       oreq      Gross_Weight_D
179000101027???? C     mea6313       oreq      *blank
179100100630     C                   IF        GID_aTotale = 'S'
179200091019???? C                   move      mea6314       mea6314_2        18 2          Peso
179300130411???? C                   Z-ADD     mea6314_2     VAoPKg                         Peso
179400090210     C                   ELSE
179500091019???? C                   move      mea6314       mea6314_2                      Peso
179600130411???? C                   ADD       mea6314_2     VAoPKg                         Peso
179700090210     C                   END
179800091203     C                   END
179900090210      *
180000100319???? C     mea6313       IFEQ      G_Weight
180100100319???? C     mea6313       oreq      Gross_Weight
180200100706???? C     mea6313       oreq      Gross_Weight_E
180300110512???? C     mea6313       oreq      Gross_Weight_D
180400101027???? C     mea6313       oreq      *blank
180500091019???? C                   move      mea6314       mea6314_2                      Peso
180600100701???? C                   ADD       mea6314_2     ToT_PesoLordo
180700090210     C                   END
180800100706???? C     mea6313       IFEQ      N_Weight
180900100706???? C     mea6313       oreq      Net_Weight
181000100706???? C     mea6313       oreq      Gross_Weight_E
181100110512???? C     mea6313       oreq      Gross_Weight_D
181200101027???? C     mea6313       oreq      *blank
181300091019???? C                   move      mea6314       mea6314_2                      Peso
181400100701???? C                   ADD       mea6314_2     ToT_PesoNetto
181500090210     C                   END
181600090210     C                   END
181700090210      ***
181800090210     C*  Peso in Grammi  se abilitato a utilizzarlo
181900090210      ***
182000100319     C     mea6311       IFEQ      Weight
182100100319     C     mea6411       ANDEQ     Grams
182200090210      *
182300090210     C*  Controllo se ho valore totale o parziale
182400100319???? C     mea6313       IFEQ      G_Weight
182500100319???? C     mea6313       oreq      Gross_Weight
182600100706???? C     mea6313       oreq      Gross_Weight_E
182700110512???? C     mea6313       oreq      Gross_Weight_D
182800101027???? C     mea6313       oreq      *blank
182900100630     c                   If        GID_aTotale = 'S'
183000091019???? c                   move      mea6314       mea6314_2
183100130411???? C     mea6314_2     div       1000          VAoPKg                         Peso
183200090210     C                   ELSE
183300091019???? c                   move      mea6314       mea6314_2
183400091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
183500130411???? C                   add       Peso_inKG     VAoPKg                         Peso
183600090210     C                   END
183700090210     C                   END
183800090210      *
183900100319???? C     mea6313       IFEQ      G_Weight
184000100319???? C     mea6313       oreq      Gross_Weight
184100100706???? C     mea6313       oreq      Gross_Weight_E
184200110512???? C     mea6313       oreq      Gross_Weight_D
184300101027???? C     mea6313       oreq      *blank
184400091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
184500100701???? C                   ADD       Peso_inKG     ToT_PesoLordo
184600090210     C                   END
184700100706???? C     mea6313       IFEQ      N_Weight
184800100706???? C     mea6313       oreq      Net_Weight
184900100706???? C     mea6313       oreq      Gross_Weight_E
185000110512???? C     mea6313       oreq      Gross_Weight_D
185100101027???? C     mea6313       oreq      *blank
185200091019???? C     mea6314_2     div       1000          Peso_inKG        12 3          Peso
185300100701???? C                   ADD       Peso_inKG     ToT_PesoNetto
185400090210     C                   END
185500090210     C                   END
185600090210      *  Volume
185700100319???? C     mea6311       IFEQ      Volume
185800100319???? C     mea6411       ANDEQ     Cubic_Meters
185900090210      *  Controllo se ho valore totale o parziale
186000100630     c                   If        GID_aTotale = 'S'
186100091019???? C                   move      mea6314       mea6314_2        18 2          Volume
186200130411???? C                   Z-ADD     mea6314_2     VAoVLm                         Volume
186300090210     C                   ELSE
186400091019???? C                   move      mea6314       mea6314_2        18 2          Volume
186500130411???? C                   ADD       mea6314_2     VAoVLm                         Volume
186600090210     C                   END
186700090210     C                   END
186800100705      *
186900090210     c     end_MEA       endSR
187000090211     C*----------------------------------------------------------------
187100090211      *? dati finali del dettaglio    UNT
187200090211     C*----------------------------------------------------------------
187300090211     C     DATI_UNT      BEGSR
187400130410      **
187500130410      *  Ad ogni fine dettaglio scrive il VAO.
187600130410      *  DETTAGLIO inizia dal segmento UNH e si chiude con UNT.
187700130410      *
187800130410      *
187900130410      **?              /*---------------------- */
188000130410     c                   exsr      Scrive_Dettag
188100130410      **?              /*---------------------- */
188200130410      *
188300130410      *   Ogni nuovo Dettaglio,
188400130410      *    per quei Partner che gestiscono più LInee
188500130410      *    Mediante il Mittente del dettaglio
188600130410     c                   if        UNB_parziale ='S' and
188700130410     c                              PT_con_piu_Nazioni = 'S' and
188800130410     C                                 Naz_salvata = Naz_mittente
188900130410      * ?ReImposta i dati da tabella PT
189000130410      * ?  se fossero stati cambiati dal precedente dettaglio:
189100130411     C                   z-add     PT_key_savXX  Nr_PT
189200130410     c                   exsr      Se_Trovata_PT
189300130410     c                   endIF
189400130410      *>>>>>>>>>>>>>>>
189500110324      * Non deve dare messaggio
189600110324     c                   goto      No_UNTctrl
189700090211      *
189800100319      **   verifica la quadratura di tutti i segmenti ricevuti
189900100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
190000100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
190100100728     C     UNT0074       IFne      Conta_segmenti
190200100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
190300100319     c                             trollare il Messaggio ricevuto'
190400100319      *
190500130410???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFTMBP :'
190600100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
190700100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
190800130410     C                             O.R.M.import IFTMBP - messaggio -
190900100319     C                             con quadratura UNT errata controllare EDI -
191000100319     C                             ricevuto su UPLOAD x il cliente :'
191100100319     C                   EVAL      Messaggio = %trim(Messaggio) +
191200100319     C                                         %editc(§PTksc:'X')
191300100319     C                   EVAL      Messaggio = %trim(Messaggio) +
191400100319     C                             ' i segmenti contati sono :' +
191500100319     C                             %editc(Conta_segmenti:'Z')
191600100319     c                   if        §PVinv <> 'N'
191700100319     c                   exsr      invio_mail
191800100319     c                   end
191900100319     c                   end
192000110321     c     No_UNTctrl    tag
192100100319      **
192200100319     c     End_UNT       endSR
192300100316     C*----------------------------------------------------------------
192400100316      *? dati finali     UNT
192500100316     C*----------------------------------------------------------------
192600100316     C     DATI_UNZ      BEGSR
192700100316      **
192800130410      ** <Memorizza>
192900130410      **  dove deve aggiornare il singolo dettaglio:
193000130410     c                   eval      FinDET_RECord = vnrec
193100130410      **
193200100316     c                   endSR
193300130410     C*----------------------------------------------------------------
193400051205      *   DEFINIZIONE CHIAVI                               *
193500130410     C*----------------------------------------------------------------
193600051205     C     *INZSR        BEGSR
193700051205      *
193800991129     c     *ENTRY        PLIST
193900060613     C                   parm                    esito             1
194000971215      *
194100050112     C     KTABel        KLIST
194200050112     C                   KFLD                    TBLKUT
194300050112     C                   KFLD                    TBLCOD
194400050112     C                   KFLD                    TBLKEY
194500090210      *
194600090210     C     K_TAB1L       KLIST
194700090213     C                   KFLD                    etbUNB
194800090213     C                   KFLD                    etbSGM
194900090213     C                   KFLD                    etbVALSGM
195000090209     C*
195100090209     C* Definisco chiavi di accesso
195200090209     C     KTAB1         KLIST
195300090209     C                   KFLD                    KKUT
195400090209     C                   KFLD                    KCOD
195500090209      *
195600090209     C     KACO          KLIST
195700090209     C                   KFLD                    KKUT
195800090209     C                   KFLD                    KKCC
195900090209     C                   KFLD                    KKSC
196000090209      *
196100090209     C     KIND          KLIST
196200090209     C                   KFLD                    KKUT
196300090209     C                   KFLD                    KKCC
196400090209     C                   KFLD                    KKSC
196500111027      *
196600090209     C* RECUPERO DATI DELL'UTENTE
196700090209     C                   Z-ADD     1             CODUT             1 0
196800090209     C                   CALL      'XPARUT'
196900090209     C                   PARM                    UTEDSE0F
197000090209     C                   MOVEL     RAGUT         RSUT             20
197100090209     C*
197200090209     C* RICERCA CAPOCONTI
197300090209     C                   DO        50            X
197400090209     C                   MOVE      TCU(X)        TCUDS
197500090209     C     F56           IFEQ      'CG'
197600090209     C     F34           ANDEQ     '01'
197700090209     C                   Z-ADD     KCU(X)        KCI               4 0
197800090209     C                   END
197900090209     C                   END
198000090209     C*
198100090211     c                   movel(p)  'EDPAB'       User_Msg         10
198200090211     C*
198300090209     C* IMPOSTO A ZERO VARIABILI DI PGM
198400090209     c                   move      *blank        Snd_Msg_alert     1
198500090209     C                   MOVEL     CA(1)         WCA              78
198600090209     C                   MOVEL     CN(1)         WCN              78
198700090209     C*
198800090209     C* RECUPERO DATA E ORA
198900100630     C                   TIME                    Ora_Data         14 0
199000100630     C                   MOVE      Ora_Data      G02DAT
199100100630     C                   MOVE      Ora_Data      WDATA8            8 0
199200100630     C                   MOVE      WDATA8        Anno2             2 0
199300090209     C                   MOVEL     WDATA8        DATA              6 0
199400100630     C                   MOVE      Anno2         DATA
199500090209     C                   MOVEL     *BLANK        G02ERR
199600090209     C                   CALL      'XSRDA8'
199700090209     C                   PARM                    WLBDA8
199800090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
199900100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
200000090209     C                   TIME                    WORA              6 0
200100090209      * Recupero data e ora
200200090209     C                   TIME                    W0140
200300090209      * UDATE IN GGMMAAAA
200400100630     C                   MOVE      W0140         DataGGMMAAA
200500090209      * UDATE IN AAAAMMGG
200600100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
200700090209     C     *iso          MOVEL     DATA_eur      dateu
200800090209      *
200900090209      * ?              /*--------------------------- */
201000090209     c                   exsr      CARICA_TABELLE
201100090209      * ?              /*--------------------------- */
201200090209      *
201300090209     C                   MOVEL     *ALL'-'       CMP132          132
201400130410      *
201500130410      * variabili di messaggio
201600130410     c                   clear                   Almeno_Uno        1
201700130410     c                   clear                   Almeno_Un_Due     1
201800130410     c                   eval      esito  = '0'
201900130410     c                   eval      Segmento_in_errore = 0
202000130410     c                   z-add     0             Conta_segmTot     5 0
202100130410      *
202200991124     C                   ENDSR
202300060621      * ?------------------------------------------------------------------ */
202400090209      *?    CARICA TABELLE SU SCHIERE
202500060621      * ?------------------------------------------------------------------ */
202600130410     C     CARICA_tabelleBEGSR
202700090205      *
202800090209     C* Caricamento Tabella 15
202900130410      * Nazione di consegna x P.O. di Consegna ed Emittente
203000130410     C                   z-add     0             Nz                3 0
203100130410     c                   clear                   Naz03
203200130410     c                   clear                   Iso03
203300130411     c                   clear                   Cpf03
203400130410     C                   z-add     1             KKUT
203500130410     C                   MOVEL(p)  '15'          KCOD
203600130410     C     KTAB1         setll     TABEL00F
203700130410     C     KTAB1         reade     TABEL00F
203800130410DO  1C                   Dow       not %EoF(TABEL00F)
203900130410DO  1C                   if         tblflg = *blank
204000130410     C                   MOVEL     TBLuni        DS15
204100130410     C                   add       1             Nz
204200130410     C                   MOVEL     tblKEY        Naz03(Nz)
204300130410     C                   MOVEL     §15COD        Iso03(Nz)
204400130410     C                   MOVEL     §15ECF        CPF03(Nz)
204500130410     C                   MOVE      §15PCF        CPF03(Nz)
204600130411E   1C                   End
204700130410     C     KTAB1         reade     TABEL00F
204800130410E   1C                   End
204900090216      *
205000100702     C* Caricamento Tabella Priorità trasporto
205100130410     C                   Z-ADD     0             X                 4 0
205200100702     C                   MOVEL     'TS'          TABCOD
205300100702     C     TABCOD        CHAIN     EDTAB01L                           30
205400100702     C     *IN30         DOWEQ     '0'
205500100702     C     TABFLG        IFEQ      *BLANKS
205600100702     C                   ADD       1             X
205700100702     C                   eval      TipoServizio(X) = TABUNI
205800100705     C                   eval      Key_TipServ(X)  = TABKEY
205900100702     C                   END
206000100702     C     TABCOD        READE     EDTAB01L                               30
206100100702     C                   END
206200090209      *
206300090209      * Caricamento Tabella Partner esteri
206400090209     C                   Z-ADD     0             X
206500090209     C                   MOVEL     'PT'          TABCOD
206600090209     C     TABCOD        CHAIN     EDTAB01L                           30
206700090209     C     *IN30         DOWEQ     '0'
206800090209     C                   SELECT
206900090209     C     TABFLG        WHENNE    *BLANKS
207000090209     C                   OTHER
207100090209     C                   ADD       1             X
207200100630     C                   MOVEL     TABKEY        PT_key(X)
207300100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
207400100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
207500100630     C                   MOVEL     TABUNI        PT_uni(X)
207600090209     C                   ENDSL
207700090209     C     TABCOD        READE     EDTAB01L                               30
207800090209     C                   END
207900121105      *
208000121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
208100121105     c                   exsr      ChecK_PT
208200121105      *
208300090209      * salva quanti Codici UNB ha caricato
208400100630     c                   z-add     x             PT_KeyXsav
208500090209      *
208600090209     C                   MOVEL     'PU'          TABCOD
208700090209     C     TABCOD        CHAIN     EDTAB01L                           30
208800090209     C     *IN30         DOWEQ     '0'
208900090209     C                   SELECT
209000090209     C     TABFLG        WHENNE    *BLANKS
209100090209     C                   OTHER
209200100630     C                   MOVEL     TABKEY        PU_key
209300090209     C                   Z-ADD     1             X
209400100630     C     PU_key        LOOKUP    PT_key(X)                              32
209500100630     C   32              MOVEL     TABUNI        PU_uni(X)
209600090209     C                   ENDSL
209700090209     C     TABCOD        READE     EDTAB01L                               30
209800090209     C                   END
209900090209      *
210000090209      * Prosegue tab.PT e PU
210100090209     C                   MOVEL     'PV'          TABCOD
210200090209     C     TABCOD        CHAIN     EDTAB01L                           30
210300090209     C     *IN30         DOWEQ     '0'
210400090209     C                   SELECT
210500090209     C     TABFLG        WHENNE    *BLANKS
210600090209     C                   OTHER
210700100630     C                   MOVEL     TABKEY        PV_key
210800090209     C                   Z-ADD     1             X
210900100630     C     PV_key        LOOKUP    PT_key(X)                              32
211000100630     C   32              MOVEL     TABUNI        PV_uni(X)
211100090209     C                   ENDSL
211200090209     C     TABCOD        READE     EDTAB01L                               30
211300090209     C                   END
211400120629      *
211500120629      * Prosegue tab.PT e PU e PV
211600120629     C                   MOVEL     'PZ'          TABCOD
211700120629     C     TABCOD        CHAIN     EDTAB01L                           30
211800120629     C     *IN30         DOWEQ     '0'
211900120629     C                   SELECT
212000120629     C     TABFLG        WHENNE    *BLANKS
212100120629     C                   OTHER
212200120629     C                   MOVEL     TABKEY        PZ_key
212300120629     C                   Z-ADD     1             X
212400120629     C     PZ_key        LOOKUP    PT_key(X)                              32
212500120629     C   32              MOVEL     TABUNI        PZ_uni(X)
212600120629     C                   ENDSL
212700120629     C     TABCOD        READE     EDTAB01L                               30
212800120629     C                   END
212900090209      *
213000090209     C* Caricamento Tabella codici clienti - tariffe
213100090209     C                   Z-ADD     0             X
213200090209     C                   MOVEL     'CL'          TABCOD
213300090209     C     TABCOD        CHAIN     EDTAB01L                           30
213400090209     C     *IN30         DOWEQ     '0'
213500090209     C     TABFLG        IFEQ      *BLANKS
213600090209     C                   ADD       1             X
213700100701     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
213800100701     C                   MOVEL     TABUNI        Des_Tb_CL(X)
213900090209     C                   END
214000090209     C     TABCOD        READE     EDTAB01L                               30
214100090209     C                   END
214200090209      *
214300090209     C* Caricamento Serivizi speciali
214400090209     C                   Z-ADD     0             X
214500090209     C                   MOVEL     'SS'          TABCOD
214600090209     C     TABCOD        CHAIN     EDTAB01L                           30
214700090209     C     *IN30         DOWEQ     '0'
214800090209     C     TABFLG        IFEQ      *BLANKS
214900090209     C                   ADD       1             X
215000100702     C                   eval       Key_ServSpec(X) = TABKEY
215100100702     C                   eval       SpecialServ(X)  = TABKEY
215200100702     C                   eval       UNI_ServSpec(X) = TABUNI
215300090209     C                   END
215400090209     C     TABCOD        READE     EDTAB01L                               30
215500090209     C                   END
215600090209      *
215700090209     C                   ENDSR
215800100705      * ?------------------------------------------------------------------ */
215900100705      *?   Controlla e decodifica Valori VALUE dei segmenti
216000100705      * ?------------------------------------------------------------------ */
216100100705     C     In_TAB_EDSTBL BEGSR
216200100705      *
216300100705      *   Cerca la decodifica del dato in TABELLA
216400100705     c                   clear                   trovata_EDSTBL    1
216500100705     c                   clear                   quale_campo       6
216600100705     c                   clear                   campo_dati_70A   70
216700100705     C                   movel     UNB_Mittente  etbUNB
216800100705     c     K_TAB1L       chain     edstbl01l
216900100705      *
217000100705      * prova quindi senza l'UNB specifico del cliente
217100100705     c                   if        not %Found(edstbl01l)
217200100705     C                   clear                   etbUNB
217300100705     c     K_TAB1L       chain     edstbl01l
217400100705     c                   end
217500100705      *
217600100705      *  Traduce i testi descrittivi se PRESENTE in tabella
217700100705      *   il campo di destinazione è definito sulla destra della descrizione
217800130411      *     ed è lungo 6 come i nomi definiti nei VAB o VAO
217900100705     c                   if        %Found(edstbl01l)
218000100705     c                   move(p)   etbDESCRI     quale_campo
218100100705     c                   movel(p)  etbDATI       campo_dati_70A
218200100705     c                   move      'S'           trovata_EDSTBL
218300100705     c                   end
218400100705      *
218500100705     C                   ENDSR
218600100705     c*==================================================================*
218700090209     C*? CNVDAT - DECODIFICA LA DATA
218800090209     C* INPUT:  - WFORM  (FORMATO DATA : 102)
218900090209     C*         - WDATA  (DATA ALFANUMERICA)
219000090209     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
219100090209     C*         - WHMS   (ORA NUMERICA)
219200090209     C*----------------------------------------------------------------
219300100702     C     Converte_Data BEGSR
219400090209     C*
219500090209     C                   MOVEL     ' '           WERRO             1
219600090209     C                   Z-ADD     *ZEROS        WCAMG             8 0
219700090209     C                   Z-ADD     *ZEROS        WCAMGora         14 0
219800100120     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
219900090209     C                   Z-ADD     *ZEROS        WHMS              6 0
220000090209     C*
220100120905     C     Work_Format_3 IFEQ      *blank
220200120905     c                   if        %Len(%trim(Work_Data__35)) = 8
220300120905     C                   eval         Work_Format_3 = Data_senza_ora
220400120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 12
220500120905     C                   eval         Work_Format_3 = Data_con_ora
220600120905     c                   elseIF    %Len(%trim(Work_Data__35)) = 14
220700120905     C                   eval         Work_Format_3 = Data_con_i_secondi
220800120905     c                   end
220900120905     C                   END
221000120905     C*
221100120905     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
221200100702     C                   MOVEL     Work_Data__35 WCOMO8            8
221300090209     C                   TESTN                   WCOMO8               99
221400090209     C   99              MOVE      WCOMO8        WCAMG                          *DATA
221500090209     C  N99              MOVEL     'S'           WERRO             1
221600090209     C                   END
221700090209     C*
221800100705     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
221900100702     C                   MOVEL     Work_Data__35 WCOMO12          12
222000100120     C                   TESTN                   WCOMO12              99
222100100120     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
222200100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
222300100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
222400090209     C  N99              MOVEL     'S'           WERRO             1
222500090209     C                   END
222600100120     C*
222700100705     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
222800100702     C                   MOVEL     Work_Data__35 WCOMO14          14
222900100120     C                   TESTN                   WCOMO14              99
223000100120     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
223100100120     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
223200100120     C   99              MOVE      WCAMGORA      WHMS                           *DATA
223300100120     C  N99              MOVEL     'S'           WERRO             1
223400100120     C                   END
223500090209     C*
223600090209     C                   ENDSR
223700090210     C*----------------------------------------------------------------
223800100701     C*? Se_Numerico       - CONTROLLO NUMERICO
223900100701     C* INPUT:  - test_num        (Numero ALFABETICO) (35)
224000100701     C*         - lunghezza_num   (Lunghezza campo output)
224100100701     C* OUTPUT: - Numero_OK       (Numero NUMERICO) (15)
224200090210     C*----------------------------------------------------------------
224300100701     C     Se_Numerico   BEGSR
224400090210     C*
224500090210     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
224600100701     C                   MOVEL     'N'           Campo_Numerico    1
224700100701     C                   MOVEL     *BLANKS       Numero_OK        15
224800110224     C*
224900090210     C                   Z-ADD     15            IN                3 0
225000100701     C                   Z-ADD     lunghezza_num IX                3 0
225100110224     C*
225200090210     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
225300100701     C                   MOVEA     test_num      NDA
225400090210     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
225500090210     C                   MOVEA     *ALL'0'       NDN
225600100630      *
225700090210     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
225800100701     C                   Z-ADD     lunghezza_num I                 3 0
225900110224     C*
226000090210     C     I             DOWGT     0                                            --- 1 -->
226100110224     C*
226200090210     C* ESTRAE IL CARATTERE DA ESAMINARE
226300100701     C     1             SUBST     test_num:I    WTEM01            1
226400110224     C*
226500090210     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
226600090210     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
226700090210     C     WTEM01        SCAN      WCA                                    99
226800110224     C*
226900090210     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
227000090210     C     *IN99         IFEQ      '1'                                          --- 2 -->
227100110224     C*
227200090210     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
227300090210     C     IN            IFGT      *ZEROS                                       --- 3 -->
227400110224     C*
227500090210     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
227600090210     C* DA DESTRA VERSO SINISTRA
227700090210     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
227800090210     C                   MOVEL     WTEMP1        NDN(IN)
227900090210     C                   SUB       1             IN
228000090210     C                   END                                                    <-- 3 ---
228100110224     C*
228200090210     C                   END                                                    <-- 2 ---
228300090210     C                   SUB       1             I
228400110224     C*
228500090210     C                   END                                                    <-- 1 ---
228600090210     C*
228700090210     C                   MOVEA     NDN           WNDN             15
228800110224     C*
228900090210     C     WNDN          IFNE      *ZEROS
229000100701     C                   MOVEA     NDN           Numero_OK        15
229100100701     C                   MOVEL     'S'           Campo_Numerico    1
229200090210     C                   END
229300090210     C*
229400090210     C                   ENDSR
229500090210     C*----------------------------------------------------------------
229600090210     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
229700090210     C*----------------------------------------------------------------
229800100705     C     Converte_CAP  BEGSR
229900090210     C*
230000090210     C                   MOVEL     *BLANKS       WCAP              9
230100090210     C     nad3251       IFNE      '0'
230200090210     C     '  '          SCAN      nad3251       LENGHT            2 0
230300090210     C                   SUB       1             LENGHT
230400090210     C     LENGHT        IFLE      5
230500090210     C     LENGHT        ANDGT     0
230600090210     C                   Z-ADD     LENGHT        T                 2 0
230700090210     C                   Z-ADD     5             S                 2 0
230800090210     C                   MOVEA     nad3251       CAP9
230900090210     C                   MOVEL     *ZEROS        CAP5
231000090210     C                   DO        LENGHT
231100090210     C                   MOVE      CAP9(T)       CAP5(S)
231200090210     C                   SUB       1             S
231300090210     C                   SUB       1             T
231400090210     C                   END
231500090210     C                   MOVEA     CAP5          WCAP5             5
231600090210     C     WCAP5         IFEQ      '0'
231700090210     C                   MOVEL     *BLANKS       WCAP
231800090210     C                   ELSE
231900090210     C                   MOVEA     CAP5          WCAP
232000090210     C                   END
232100090210     C*
232200090210     C                   ELSE
232300090210     C                   MOVEL     nad3251       WCAP
232400090210     C                   END
232500090210     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
232600090210     C     WCAP          IFNE      *BLANKS
232700090210     C     WFLCPF        ANDNE     0
232800090210     C                   MOVEL     WFLCPF        WELE              1 0
232900090210     C                   MOVE      WFLCPF        WPOS              1 0
233000090210     C                   MOVEL     *BLANK        WCPF
233100090210     C     WELE          SUBST     WCAP:WPOS     WCPF              9
233200090210     C                   ELSE
233300090210     C                   MOVEL     *BLANKS       WCPF
233400090210     C                   END
233500090210     C                   END
233600090210     C*
233700090210     C                   ENDSR
233800130410      * ?================================================================== */
233900130410     C*? Azzera_MSG   - Azzera dati messaggio
234000130410      * ?================================================================== */
234100130410     C     Scrive_Dettag BEGSR
234200130410      **
234300130410      *  Deve flaggare il dettaglio che ha dei problemi
234400130410      * Impostando le righe in errore
234500130410     C                   if        Err_in_detta = 'S'
234600130410      * prima di rieseguire il ciclo
234700130410      *  rilascia il record
234800130410     c                   update    tivin000
234900130410      **
235000130410      * ?  Scrive su tutti i records il tipo di errore
235100130410     c     IniDET_RECord setll     tivin00r
235200130410      **?              /*---------------------- */
235300130410     c                   exsr      DETta_Errato
235400130410      **?              /*---------------------- */
235500130410      *
235600130411???? c                   else
235700130411      *
235800130410      *  se è arrivato in fondo al dettaglio scrive VAO
235900130410      * ?              /*---------------------- */
236000130410     c                   exsr      Scrive_VAO
236100130410      * ?              /*---------------------- */
236200130411      *
236300130410      *  Problemi nella decodifica dei campi VAO
236400130410     c                   if        errori_in_Wrt = 'S'
236500130410     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAO'
236600130410     c                   exsr      Error_DET
236700130410     c                   ROLBK
236800130410     c                   else
236900130410     c                   COMMIT
237000130410     c                   end
237100130410      *
237200130410     c                   end
237300130410      **
237400130410     c     end_WRidett   endSR
237500090211      * ?================================================================== */
237600090211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
237700090211      * ?================================================================== */
237800130410     C     Scrive_VAO    BEGSR
237900090211      *
238000090211     c                   move      'S'           Almeno_Uno
238100090211      *
238200130411      ** tipo comunicazione ORM
238300130411     C                   eval      vaotco = 'F'
238400130411      * Imposta le NOte
238500130411     C     VAoNO1        IFEQ      *BLANKS
238600130411     C                   MOVEL     Note_1        VAoNO1
238700130411     C                   MOVEL     Note_2        VAoNO2
238800130411     C                   ELSE
238900130411     C                   MOVEL     Note_1        VAoNO2
239000130411     C                   END
239100130411      **
239200130411      **   tipo ORM
239300130411     C                   eval      vaotor = 'S'
239400130411     C                   eval      VAOPAG = 'O'
239500130411     C* ?------------------
239600130411     c                   eval        Dorm01 = vaoFLO
239700130411      **  ORM Commissionato
239800130411     c                   move      'S'           §orComC
239900130411     c                   move      'S'           §ormFD
240000130411     c                   move      'S'           §ormFR
240100130411     c                   eval        vaoFLO = Dorm01
240200130411      **
240300130411     C                   WRITE     FnVAO000                             99
240400130411     c   99              eval      errori_in_Wrt = 'S'
240500130411      *
240600130410     C     Error_VAO     Endsr
240700130410      * ?------------------------------------------------------------------ */
240800121105      * CTRL caricamento schiere    PT                               -*
240900130411      * ?------------------------------------------------------------------ */
241000121105     C     Check_PT      begsr
241100121105     C*
241200121105     c* Controllo riempimento schiera
241300121105     c                   clear                   trul0sds
241400121105     c                   eval      i0sski='PT_key'
241500121105     c                   eval      i0sele=%elem(PT_key)
241600121105     c                   eval      i0spie=x
241700121105     c                   eval      i0sfile='EDTAB00F'
241800121105     c                   eval      i0ssif=knsif
241900121105     c                   eval      i0spgm='TRTCT93R'
242000121112     c                   move      kpjbu         SvKpjbu
242100121112     c                   clear                   kpjbu
242200121105     c                   movel     trul0sds      kpjbu
242300121105     c                   call      'TRUL0SR'
242400121105     c                   parm                    kpjba
242500121112     c                   eval      kpjbu = SvKpjbu
242600121105     C*
242700121105     C                   ENDSR
242800120629      * ?------------------------------------------------------------------ */
242900090211     O*****************************************************************
243000090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
243100090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
243200090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
243300090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
