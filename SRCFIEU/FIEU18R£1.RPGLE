000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200060707      *-------------------------------------------------------------------------*
000300060707      ***************************************************************************
000400000117      *  CONVERSIONE SCAN RICEVUTI DA DPD                                       *
000500971216      ***************************************************************************
000600010219     FFNARB01L  UF   E           K DISK
000700170922      *--
000800170926     FfiPND03L  iF   E           K DISK
000900170922      *--
001000000201     FFNLBL02L  IF   E           K DISK
001100090831      *
001200090831     FfnLBL01L  IF   E           K DISK    rename(FNLBL000:FNLBL01)
001300090831     Fazorg01L  IF   E           K DISK
001400090831      *
001500060630     FTNTBE01L  IF   E           K DISK
001600971216      *
001700060719     FTidp501L  uF a E           K DISK
001800060719      *
001900000117     FFNEVB01L  UF A E           K DISK
002000000117     FFNCDE02L  UF A E           K DISK
002100051017     FFIARS01L  UF A E           K DISK
002200060615      **?------------------------------------------------------------------ */
002300060615     C*? Ds Decodifica dei campi DS PASSATA COME PARAMETRO
002400060719      **?------------------------------------------------------------------ */
002500161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
002600160907     D FNARB00       E DS                  EXTNAME(FNARB00F)
002700060719      **?------------------------------------------------------------------ */
002800000131     D KDIR            S             30    DIM(900)  inz                        CODICE NAZIONE
002900000131     D DIR             S            256    DIM(900)  inz                        CODICE NAZIONE
003000000221     D KC2E            S              3    DIM(900)  inz                        CODICE NAZIONE
003100060615     D ser             S              3  0 DIM(3)    inz                        CODICE NAZIONE
003200160929     D Kcbr            S              6    DIM(100)  inz                        BOL RECUP.xDIROTTA
003300160929     D KcbrCEV         S              3    DIM(100)  inz                        evento Bolla Recup
003400000131      *----------------------------------------------------*
003500010219     D  MSGDS          S             80
003600010219     D  LENGH          S             15  5
003700010219     D Wdat5Dsc        S               D   DATFMT(*iso)
003800010219      *
003900000203     D W030A           S             30
004000000203     D W002A           S             30
004100000201     D Writeevb        S              1
004200000204     D W0140           S             14  0
004300991129     D WORA            S              6  0
004400160928     D dd              S              3  0
004500000221     D YY              S              3  0
004600000221     D XX              S              3  0
004700000218     D XX1             S              3  0
004800000131     D XX2             S              3  0
004900000131     D XX3             S              3  0
005000991129     D WDTGIO          S              8  0
005100991129     D DATEU           S              8  0
005200060721     D Wdata_gg_inpiu  S              8  0
005300000131     D Wdataoggi       S               D   DATFMT(*iso)
005400000203     D Wdata           S               D   DATFMT(*iso)
005500170925      *--
005600170925     D Ktrcs           S              1    INZ('E')
005700170925     D Kaas            S                   LIKE(arbaas)
005800170925     D Klnp            S                   LIKE(arblnp)
005900170925     D Knrs            S                   LIKE(arbnrs)
006000170925     D Knsp            S                   LIKE(arbnsp)
006100170925     D KFaas           S                   LIKE(arbaas)
006200170925     D KFlnp           S                   LIKE(arblnp)
006300170925     D KFnrs           S                   LIKE(arbnrs)
006400170925     D KFnsp           S                   LIKE(arbnsp)
006500170925     D  Kaas_P         S                   LIKE(arbaas)
006600170925     D  Klnp_P         S                   LIKE(arblnp)
006700170925     D  Knrs_P         S                   LIKE(arbnrs)
006800170925     D  Knsp_P         S                   LIKE(arbnsp)
006900170925      *--
007000060616     D Kndc            S                   LIKE(CDendc)  INZ(*zeros)
007100060721     D Kndc0           S                   LIKE(CDendc)  INZ(*zeros)
007200000202     D Wcdecod         S                   LIKE(cdecod)
007300000202     D Wcdests         S                   LIKE(cdests)
007400000203     D wARBfls         S                   LIKE(arbfls)
007500000203     D wARBlna         S                   LIKE(arblna)
007600000203     D wARBnrs         S                   LIKE(arbnrs)
007700000203     D wARBncd         S                   LIKE(arbncd)
007800090721     D Data_Consegna_ARB...
007900090721     D                 S                   LIKE(arbDCM)
008000090828     D Bolla_chiu_con_RESO...
008100090824     D                 S              1
008200090831     D Linea_ARR_DPD   S              1
008300170925     D trovato_PND     S              1
008400020716      *
008500090831     D og143         E DS
008600000131     D DDIR          E DS
008700170922     D dPNDflo       E DS
008800021114     D Tibs36ds      E DS
008900060628     D Devb01        E DS
009000991129     D KPJBA         E DS
009100160928     D fieu18r1DS    E DS
009200060720      **
009300060720     D Parcel11        S             11
009400060720     D Parcel14        S             14
009500060720     D Lung_Parcel     S              3  0
009600060720     D  position       s              3  0 INZ(0)
009700060720
009800060720     D Digits          C                   '0123456789'
009900911213      *----------------------------------------------------*
010000060615     c     *ENTRY        PLIST
010100161130     c                   parm                    FIEU18
010200060616     c                   parm                    ok_18             1
010300060630     c                   parm                    msgerr           80
010400010219      *
010500060719      **?se ok_18 ='A' è richiamato per elaborare gli SCAN dal giro in attesa
010600060720     c                   clear                   ciclo_attesa      1
010700060719     c                   if        ok_18 ='A'
010800060720     c                   move      'S'           ciclo_attesa
010900060719     c                   clear                   ok_18
011000060719     c                   end
011100060719      *
011200000201     C                   clear                   kAAS
011300000201     C                   clear                   kLNP
011400000201     C                   clear                   kNRS
011500000201     C                   clear                   kNSP
011600000201     C                   clear                   kfAAS
011700000201     C                   clear                   kfLNP
011800000201     C                   clear                   kfNRS
011900000201     C                   clear                   kfNSP
012000060630      *
012100060630     C                   movel     i5dattim      wi5dat            8 0
012200060630     C                   move      i5dattim      wi5ora            6 0
012300060630     c                   clear                   si_Elabora        1
012400000201      *
012500000203     C                   exsr      AGGarb
012600971215      *
012700060719      **?Se non trovo la spedizione scrivo file errori, forzo msg 001
012800000201     c                   IF        KNSP = *zeros
012900060620     c                   eval      ok_18 = 'S'
013000060703     c                   eval      msgerr = 'Parcel >' +  I5PARCEL +
013100060703     c                                      '< Non trovata spedizione '
013200000131     C                   ELSE
013300000203      *
013400060719      **?Aggancio tabella "DIR" e seguo indicazioni per elaborare il record
013500000204     C                   exsr      AGGtabella
013600000203     c                   ENDIF
013700000204      *
013800060719      **?In ultimo controllo se aggiornare ARBDAM
013900060719      **? se non è stata spuntata la merce e inviato lo scan a DPD
014000060719      **?  per mandare lo Scan di defluenza a DPD occorre aggiornare
014100060719      **?   questa data sul FNARB00F
014200060630     c                   if        si_Elabora = 'S'
014300010219     C                   Exsr      AGG_ARBDAM
014400160928      *
014500160928     C                   Exsr      AGG_PESO
014600060630     C                   End
014700010219      *
014800060615     C                   eval      *inrt = *on
014900000203      *----------------------------------------------------------------
015000000203      * AGGANCIO SPEDIZIONE
015100000203      *----------------------------------------------------------------
015200000203     C     AGGARB        BEGSR
015300000131      *
015400090721     c                   eval      Data_Consegna_ARB = 0
015500090828     c                   eval      Bolla_chiu_con_RESO = *blank
015600090831     c                   eval      Linea_ARR_DPD = *blank
015700170925     c                   eval      trovato_PND = 'N'
015800090721      *
015900000203      * Cerco in AR4 con il "chi sono"
016000090901      *
016100090901      *  agganciando AR4 con tipo record "I" e Parcel nei primi 14 caratteri,
016200090901      *   se il parcel fosse presente su + record, prenderebbe sempre quello
016300090901      *   della bolla MAMMA ossia il primo scritto nel file.
016400170925      **  agganciava il Fisico  (SOSTITUITO con FIPND00F)
016500170922      **
016600170922      **  il flag è stato spostato sul FIPND quindi non si deve più agganciare BL4I
016700170925     c     i5Parcel      chain     fipnd03l
016800170925     c                   if        %Found(fipnd03l)
016900170925     c                   eval      trovato_PND = 'S'
017000170922     c                   movel     pndFLO        dPNDflo
017100161014      **
017200000203      * Se trovato aggancio ARB
017300170925     C                   eval      Kaas = PNDaas
017400170925     C                   eval      Klnp = PNDlnp
017500170925     C                   eval      Knrs = PNDnrs
017600170925     C                   eval      Knsp = PNDnsp
017700170925      *
017800090901      *  quindi aggancia sempre la bolla MAMMA
017900090901      *   eccetto il caso in cui
018000090901      *  potrebbe essere stata pulita poichè passati i 15gg. in ambiente di Filiale
018100090901      *=======
018200060721     c     KSPE          chain     FNARB01l
018300090901      *=======
018400090831      *
018500090831      * Controlla se la spedizione è un Export in base alla linea di arrivo
018600090831     C                   If         %found(FNARB01l)
018700090831     c     arbLNA        chain     azORG01l
018800090831     C                   If         %found(azORG01l)
018900090831     C                   MOVEL     orgDE3        Og143
019000090831      * controlla il Network se DPD
019100090831     c                   if        §ogNTW = 'DPD'
019200090831     c                   eval      Linea_ARR_DPD  = 'S'
019300090831     c                   end
019400090831     c                   end
019500090831     c                   end
019600090901      *=======
019700090901      ***    quindi :
019800090901      ***  X non trovata   O   se la LNA NON è DPD:
019900090901      ***   verifica che sia FIGLIA di una bolla export DPD
020000090901      ***------**
020100090831     C                   If        NOT %found(FNARB01l) or
020200090831     C                             %found(FNARB01l) and Linea_ARR_DPD <> 'S'
020300090901     c                   exsr      cerca_mamma
020400090831      ***------**
020500000203     c                   Else
020600090831      ***------**
020700090901     c                   eval      wARBfls = ARBfls
020800090901     c                   eval      wARBlna = ARBlna
020900090901     c                   eval      wARBnrs = ARBnrs
021000090901     c                   eval      wARBncd = ARBncd
021100090901      *
021200090721      * Data Consegna Bolla
021300090831      *  Si memorizza la Data consegna se la bolla è già stata data x consegnata
021400100311      *     o si tratta di un RESO (2) o RESO x AVARIA (6)
021500090721     c                   eval      data_Consegna_ARB  = arbDCM
021600100311     c                   if        arbCCA = '2' or arbCCA = '6'
021700090828     c                   eval      Bolla_chiu_con_RESO  = 'S'
021800090824     c                   end
021900090828      *
022000090901      *  solo se non è un Reso
022100090901     c                   if        Bolla_chiu_con_RESO <> 'S'
022200090901     c                   exsr      cerca_figlia
022300090901     C                   End
022400090901      ***------**
022500090901     C                   EndIF
022600090831      ***------**
022700171030     c                   end
022800090831      ***
022900000203     C                   ENDSR
023000060719      **?------------------------------------------------------------------ */
023100090901      ***  Cerca se esiste una bolla mamma altrimenti resetta i campi
023200090901      **?------------------------------------------------------------------ */
023300090901     C     cerca_mamma   begSR
023400090901      ***
023500090901      * chiave della figlia x prendere la mamma
023600090901     C     Kspe          chain     FNLBL01L
023700090901      ***
023800090901      *               Se non trova pulisce i campi chiave
023900090901     c                   if        Not %Found(FNLBL01L)
024000090901     C                   clear                   Knsp
024100090901     c                   else
024200090901      ***
024300090901     C                   eval      Kaas_P = LBLaaP
024400090901     C                   eval      Klnp_P = LBLlpP
024500090901     C                   eval      Knrs_P = LBLnrP
024600090901     C                   eval      Knsp_P = LBLnsP
024700090901      ***
024800090901      * Ha trovato la MAMMA prova a cercare ARB
024900090901      *  ma attenzione che, causa pulizie, dopo X gg.,
025000090901      *  potrebbe non trovarlo + !!!!
025100090901     c     KFSPE         chain     FNARB01l
025200090901      ***
025300090901      *               Se non trova pulisce i campi chiave
025400090901     c                   if        NOT %Found(FNARB01l)
025500090901     C                   clear                   Knsp
025600090901     c                   else
025700090901      *
025800090901      * Quindi della MAMMA si memorizza
025900090901      *  la Data consegna se data già x consegnata
026000100311      *   e se è un RESO (2) o RESO x AVARIA (6)
026100090901     c                   eval      data_Consegna_ARB  = arbDCM
026200100311     c                   if        arbCCA = '2' or arbCCA = '6'
026300090901     c                   eval      Bolla_chiu_con_RESO  = 'S'
026400090901     c                   end
026500090901      *
026600090901      *  altri campi da riportare  su ARS e LNA su EVB.
026700090901     c                   eval      wARBfls = ARBfls
026800090901     c                   eval      wARBlna = ARBlna
026900090901     c                   eval      wARBnrs = ARBnrs
027000090901     c                   eval      wARBncd = ARBncd
027100090901     c                   end
027200090901      *
027300090901     c                   endIf
027400090901      ***
027500090901     C                   ENDSR
027600090901      **?------------------------------------------------------------------ */
027700090901      ***  Cerca se esiste una bolla mamma altrimenti resetta i campi
027800090901      **?------------------------------------------------------------------ */
027900090901     C     cerca_figlia  begSR
028000090901      ***
028100090901      *>>>>>>>>>>
028200090901      * CERCO se la spedizione ha una figlia utilizzando la stessa chiave
028300090901      *  con cui ha agganciato ARB.
028400090901     C     Kspe          SETGT     FNLBL000
028500090901     C     Kspe          READPE    FNLBL000                               33
028600090901      *
028700090901      *  x il cambio di Porto o per il Dirottamento deve agire sulla figlia
028800090901      *   al contrario dei Resi che deve invece agire sulla Mamma.
028900090901     C******             If        *in33 = *off  and  LBLlan = LBLlap
029000090901     c                   if        not %EoF(FNLBL02L)  and
029100090901     c                              (arbCCA = '1' or arbCCA = '9')
029200090901      *
029300090901     C                   eval      KFaas = LBLaan
029400090901     C                   eval      KFlnp = LBLlpn
029500090901     C                   eval      KFnrs = LBLnrn
029600090901     C                   eval      KFnsp = LBLnsn
029700090901      *
029800090901     c     KFSPE         chain     FNARB01l
029900090901     C                   If        NOT %found
030000090901     C                   clear                   KFnsp
030100090901     C                   Else
030200090901     c                   eval      data_Consegna_ARB  = arbDCM
030300090901     C                   Endif
030400090901      *
030500090901     C                   Endif
030600090901      *>>>>>>>>>>
030700090901      ***
030800090901     C                   ENDSR
030900090901      **?------------------------------------------------------------------ */
031000000204      * ELABORO LO SCAN PILOTATO DALLA TABELLA
031100060719      **?------------------------------------------------------------------ */
031200000204     C     AGGtabella    BEGSR
031300000204      *
031400060719      **?Aggancio tabella
031500060719      **?  XX1 = SCAN + COD.ADDIZIONALE
031600060719      **?  XX2 = COD.ADDIZIONALE
031700060719      **?  XX3 = SCAN
031800060615     c                   movel     i5addser1     ser(1)
031900060615     c                   movel     i5addser2     ser(2)
032000060615     c                   movel     i5addser3     ser(3)
032100060630     c                   clear                   ScanCod           3
032200060630     c                   clear                   Agg_Cod           3
032300060703     c                   movel     i5scant       ScanCod
032400060615     c
032500060615     c                   do        3             sr                1 0
032600060719      **?  secondo e terzo giro solo se valorizzati
032700060615     c                   if        sr > 1 and ser(sr) = 0
032800060615     c                   iter
032900060615     c                   endif
033000060615
033100000204     c                   clear                   XX1
033200000204     c                   clear                   XX2
033300000204     c                   clear                   XX3
033400000204      *
033500060619     c                   movel     i5scant       w030a
033600060615     c                   movel     ser(sr)       W002A
033700000221     c                   eval      %subst(W030a:16) = W002a
033800000204     c                   eval      XX1 = 1
033900000204     c     W030a         lookup    KDIR(xx1)                              31
034000060630     c   31              movel     i5scant       ScanCod
034100060630     c   31              movel     ser(sr)       Agg_Cod
034200000204      *
034300000204     C                   IF        *IN31 = *off
034400000204     c                   clear                   XX1
034500000204      *
034600000204     c                   clear                   W030A
034700060615     c                   movel     ser(sr)       W002A
034800000221     c                   eval      %subst(W030a:16) = W002a
034900000221     c                   eval      XX2 = 1
035000000221     c     W030a         lookup    KDIR(xx2)                              31
035100060630     c   31              movel     ser(sr)       Agg_Cod
035200000221     c  n31              clear                   XX2
035300000204      *
035400060719      **? solo per il primo giro altrimenti ripeterebbe la ricerca solo con lo
035500060719      **?  scan e l'operazione è già stata fatta la prima volta
035600060615     c                   if        sr = 1
035700060623     c                   movel(p)  i5scant       w030a
035800000221     c                   eval      XX3 = 1
035900000221     c     W030a         lookup    KDIR(xx3)                              31
036000060630     c   31              movel     i5scant       ScanCod
036100000221     c  n31              clear                   XX3
036200060615     c                   else
036300060615     c                   clear                   XX3
036400060615     c                   endif
036500000221      *
036600000204     C                   ENDIF
036700000204      *
036800060719      **? Se non trovo la tabella scrivo file errori, cod. 002, cancello Scan
036900060630     C                   IF        XX1 = *zero and XX2 = *zero and XX3 = *zero
037000060630     C                                          and ser(sr)<> *zero  or
037100060630      *
037200060630     C                             XX1 = *zero and XX2 = *zero and XX3 = *zero
037300060630     C                             and ser(1) = *zero and ser(2) = *zero
037400060630     C                             and ser(3) = *zero
037500060630      *
037600060620     c                   eval      ok_18 = 'S'
037700060703     c                   eval      msgerr =  'Scan >' + %Trim(ScanCod) +
037800060703     c                                       '< Non trovata tabella DIR'
037900000218      *
038000000204     c                   ELSE
038100000204      *
038200060719      **? SCAN + CODICE ADDIZIONALE
038300000204     C                   IF        XX1 > *zeros
038400000204     C                   movel     DIR(XX1)      DDIR
038500000204     c                   exsr      ELABORA
038600000204     c                   ELSE
038700000204      *
038800060719      **? CODICE ADDIZIONALE
038900000204     C                   IF        XX2 > *zeros
039000000204     C                   movel     DIR(XX2)      DDIR
039100000204     c                   exsr      ELABORA
039200000204     C                   clear                   XX2
039300000204     c                   ENDIF
039400000204      *
039500060719      **? SCAN
039600000204     C                   IF        XX3 > *zeros
039700000204     C                   movel     DIR(XX3)      DDIR
039800000204     c                   exsr      ELABORA
039900000204     c                   ENDIF
040000000204      *
040100000204     c                   ENDIF
040200000204     c                   ENDIF
040300000204      *
040400060615     c                   enddo
040500060615
040600000204     c                   ENDSR
040700060719      **?------------------------------------------------------------------ */
040800000203      * ELABORA
040900060719      **?------------------------------------------------------------------ */
041000000203     C     ELABORA       BEGSR
041100000203      *
041200060630     c                   move      'S'           si_Elabora
041300060616     C                   move      wi5dat        Wdata
041400000131     C                   adddur    §DIRgga:*d    wdata
041500060721     C     *iso          MOVEL     Wdata         Wdata_gg_inpiu
041600000131      *
041700060719      **? Scrivo/Aggiorno FIARS se impostato il flag
041800000131     c                   If        §DIRarr = 'S'  or  §DIRcan <> *blanks
041900000131     c                   Exsr      ELAB_ars
042000000131     c                   Endif
042100160909      *
042200160909      *  =============================================================
042300160909      **?  QUESTA Routine DEVE essere fatta prima di quella che GENERA EVB
042400160928      **? altrimenti NON FUNZIONA ........poichè testa gli EVENTI PRESENTI
042500160909      *  =============================================================
042600160909      **? Creo Bolla di recupero al cliente su DIROTTAMENTO estero segnalato
042700160909      **?  da DPD che DPD addebita sul Clearing a noi.
042800160909     c                   If        §DIRcbR = 'S'
042900160909      **?  Occorre verificare se si vuole avere il recupero una sola volta
043000160909     c                   Exsr      ELAB_cbR
043100160909      *
043200160909     c                   Endif
043300000131      *
043400060719      **? Scrivo FNEVB se impostato il flag
043500060628     c                   If        §DIReve = 'S'
043600000131     c                   Exsr      ELAB_evb
043700000131     c                   Endif
043800000202      *
043900060719      **? DPD ha cambiato le regole x il POD nr. -> lo invia sullo SCAN 13
044000060719      **? Occorre portarlo quindi sul nostro evento MIC se questo è l'ultimo
044100060719      **?  evento rilevato nel nostro sistema.
044200060628     c                   If        §DIRmic = 'S'
044300060628     c                   Exsr      MIC_evb_aggio
044400060628     c                   Endif
044500060628      *
044600060719      **? Elaboro per distinta se impostato il flag e se trascorsi i gg attesa
044700000203     c                   IF        §DIRdis = 'S'  and  Wdataoggi >= Wdata
044800090723      **
044900100920     c**********         iF        I5SCANT <> 13
045000100920      **   sostituito anche per tutti gli altri SCAN che non siano la consegna x i resi
045100100920     c                   iF        I5SCANT <> 13 and Bolla_chiu_con_RESO <> 'S'
045200090824     c                              or
045300090721     c                             I5SCANT = 13 and Wi5DAT <> Data_Consegna_ARB
045400090828     c                                       and Bolla_chiu_con_RESO <> 'S'
045500090824     c                              or
045600090828     c                             I5SCANT = 13 and Wi5DAT  < Data_Consegna_ARB
045700090828     c                                       and Bolla_chiu_con_RESO  = 'S'
045800090824      *
045900090824      *  Si è deciso di non ricevere più volte la Delivery (SCAN 13) se la bolla è già
046000090824      *  data x consegnata su ARB con la stessa Data.
046100090824      **  e se trattandosi di RESO la data ricevuta è successiva alla data Consegna
046200090824      *
046300000202     c                   Exsr      ELAB_cde
046400090721     C                   endiF
046500000202     C                   ENDIF
046600000218      *
046700060720      **?Se richiesta scrittura SCAN in Attesa con il nuovo codice SCAN
046800060720      **? solo se il pgm non è chiamato dal giro di Attesa ossia
046900060720      **?  x elaborare gli SCAN che nel giro precedente sono stati scritti
047000060720     c                   If        XX3 > *zeros and ciclo_attesa = *blank
047100060720     c                             and msgerr = *blank
047200091203     c                             or I5SCANT = 3 and ciclo_attesa = *blank
047300091203     c                             and XX3 > *zeros
047400060720     c                   Exsr      SCAN_Attesa
047500060720     c                   end
047600060720      *
047700060719      **? Scrivo il file errori se impostato il flag
047800000218     c                   IF        §DIRerr = 'S'
047900060620     c                   eval      ok_18 = 'S'
048000060630     C                   movel     'DER'         tbeCOD
048100060630     C                   movel(p)  §dircer       tbeKE1
048200060630     c     ktbe          chain     tntbe01l
048300060630     c                   if        %Found(tntbe01l)
048400060720     c                   eval      msgerr = 'Scan>' + %Trim(ScanCod) + '/'
048500060720     c                             + Agg_Cod + '<' + %Trim(TbeUNI)
048600060630     c                   end
048700000218     C                   ENDIF
048800060719      *
048900000221     C                   ENDSR
049000060719      **?------------------------------------------------------------------ */
049100161130      * Scrive/Cancella lo scan di attesa sul TIDP500R
049200060719      **?------------------------------------------------------------------ */
049300060719     C     SCAN_Attesa   BEGSR
049400061103      *
049500061103      **? se gli SCAN non sono fra quelli da considerare
049600061107     c                   if        I5SCANT = 18
049700061103     c                   goto      salta_attesa
049800061103     c                   end
049900060719      *
050000060719      **? il problema di gestire lo SCAN di attesa da un precedente SCAN nasce
050100060719      **? poichè non tutti i DEPOT DPD mandano lo SCAN 13 di Merce Consegnata.
050200060719      **? Occorre quindi dare x consegnato il Parcel dopo x giorni (impostati su
050300060719      **? tabella DIR).
050400060719      **? >es.: SCAN 03 di MIC genera SCAN 13 con 5 gg. di attessa  -
050500060719      **? allo scadere della data calcolata se non si sono ricevuti SCAN che
050600060719      **? informano sulla consegna o non consegna, il parcel verrà dato x consegnato<
050700060719      **? MA....Se sono stati inviati degli SCAN durante il periodo di attesa occorre
050800060719      **? eliminare dal file di attesa lo SCAN in maturazione.
050900060719      *
051000060719     c                   clear                   in_attesa         1
051100060719      *
051200060719      **?Controlla se ci sono degli SCANs in Attesa da eliminare o da scrivere
051300060719      *  Se c'è una Bolla legata, Utilizzo spedizione FNLBL
051400060719     c                   IF        KFnsp > *zeros
051500060719     c     KFspe         chain     Tidp501L
051600060719     c                   ELSE
051700060719      *  altrimenti direttamente con la Bolla
051800060719     c     Kspe          chain     Tidp501L
051900060719     c                   ENDIF
052000060719      **? x trovato:
052100060719      **?   occorre controllare se lo SCAN in elaborazione è temporalmente precedente
052200060719      **?   o successivo quindi eliminare o meno lo SCAN in attesa.
052300060719      *
052400060719      **?Scan in attesa successivo a scan elaborato o contemporaneo a scan 10 elaborato
052500060719      **?   Non deve essere toccato lo SCAN in attesa
052600060719     c                   if        %found(Tidp501L)
052700060719      *
052800060719     c                   if        (DP5dattim > i5DATTIM)  or
052900060719     c                             (DP5dattim = i5DATTIM and I5SCANT = 10)
053000100312     c                              or
053100100312     c                             (DP5dattim = i5DATTIM and I5SCANT = 2)
053200060719     c                   move      'S'           in_attesa         1
053300060719     c                   else
053400060719      **?   Altrimenti deve essere eliminato lo SCAN in attesa poichè è stato
053500060719      **?   inviato un evento successivo a quello in attesa .
053600061103      *
053700060719     c                   delete    Tidp500r
053800061103      *
053900061103      *
054000060719     c                   endif
054100060719      *
054200060719     c                   end
054300060719      *
054400060719      **? ----------------------->
054500060719      **?non presente sul file di attesa e nuovo SCAN impostato in tabella
054600060719      **?  scrive lo SCAN in attesa.
054700060719     c                   if        in_attesa = *blank and §DIRasc <> *blank
054800060719      *
054900060719     c                   clear                   TIDP500r
055000060719      *
055100060719      *   Chiave bolla da LBL se presente
055200060719     C                   If        KFnsp <> *zeros
055300060719     C                   eval       DP5aas = KFaas
055400060719     C                   eval       DP5lnp = KFlnp
055500060719     C                   eval       DP5nrs = KFnrs
055600060719     C                   eval       DP5nsp = KFnsp
055700060719     C                   Else
055800060721      *   Chiave bolla da ARB
055900060719     C                   eval       DP5aas = Kaas
056000060719     C                   eval       DP5lnp = Klnp
056100060719     C                   eval       DP5nrs = Knrs
056200060719     C                   eval       DP5nsp = Knsp
056300060719     C                   Endif
056400060721      *
056500060721      **? Attenzione però:
056600060721      **? se DPD invia sulla stessa trasmissione relativamente allo  stesso Parcel
056700060721      **?  gli SCAN non nel corretto ordine di DATETIME nel caso della  "CONSEGNA" e del "MIC"
056800060721      **?    potremmo avere questa situazione:
056900060721      **  84482305625;13;0413;Depot 0413;20060720153400;0000;1;01;101;000;;;;;;;5322;GUIBERT;;
057000060721      **  84482305625;03;0413;Depot 0413;20060720135400;0000;1;01;101;000;;;;;;;;;;
057100060721      **?Trattandosi di scrittura da SCAN 03 dello SCAN 13:
057200060721      **? poichè l'ora del MIC è antecedente alla Delivery, non deve essere scritto nulla.
057300060721      **?  in attesa.
057400060721     c                   if        §DIRasc = '13 ' and i5scant = 3
057500060721      **?OCCORRE verificare comnunque se su FNCDE non sia presente un record da elaborare
057600060721      **? appena scritto con SCAN 13 per riportare su ARB la Data Consegna.
057700060721     C     KCDEdp5       chain     FNCDE02L
057800060721     c                   if        %Found(FNCDE02L) and cdeSTS ='13 '
057900060721     c                             and cdeDCM < wdata_gg_inpiu
058000060721     c                   goto      No_Write
058100060721     c                   end
058200060721      **?OCCORRE verificare comnunque su ARB che non sia già stata data x Consegnata.
058300060721     C     KARBdp5       chain     FNarb01L
058400060721     c                   if        %Found(FNARB01L) and arbDCM > 0
058500060721     c                             and arbDCM < wdata_gg_inpiu
058600060721     c                   goto      No_Write
058700060721     c                   end
058800060721     c                   end
058900060721      **?Passato il controllo può SCRIVERE il record.
059000060719      *
059100060719     c                   eval      DP5GGATT     =  §DIRgga
059200060719     c                   eval      DP5DATAGG    =  I5DATTIM
059300060721     c                   movel     wdata_gg_inpiuDP5DATAGG
059400060719     c                   eval      DP5PARCEL    =  I5PARCEL
059500060719     c                   movel     §DIRasc       DP5SCANT
059600060719     c                   eval      DP5DEPCODE   =  I5DEPCODE
059700060719     c                   eval      DP5DEPNAME   =  I5DEPNAME
059800060719     c                   eval      DP5DATTIM    =  I5DATTIM
059900060719     c                   eval      DP5ROUTE     =  I5ROUTE
060000060719     c                   eval      DP5TOUR      =  I5TOUR
060100060719     c                   eval      DP5PCODE     =  I5PCODE
060200060719     c                   eval      DP5SERVICE   =  I5SERVICE
060300060719     c                   eval      DP5COUNTRY   =  I5CCOUNTRY
060400060719     c                   eval      DP5CONSZIP   =  I5CONSZIP
060500060719     c                   if        §DIRaca ='S'
060600060719     c                   eval      DP5ADDSER1   =  I5ADDSER1
060700060719     c                   eval      DP5ADDSER2   =  I5ADDSER2
060800060719     c                   eval      DP5ADDSER3   =  I5ADDSER3
060900060719     c                   end
061000060719     c                   eval      DP5WEIGHT    =  I5WEIGHT
061100060719     c                   eval      DP5CUSTREF   =  I5CUSTREF
061200060719     c                   eval      DP5PODIMAG   =  I5PODIMAG
061300060719     c                   eval      DP5RECNAME   =  I5RECNAME
061400060719     c                   eval      DP5INFOTXT   =  I5INFOTXT
061500060719      *
061600060719     c                   WRITE     TIDP500r
061700060719     C                   feod      TIDP501L
061800060721     c     No_write      tag
061900060721      *
062000060719     c                   End
062100060719      **? ----------------------->
062200060719      *
062300061103     c     salta_attesa  tag
062400061103      *
062500060719     C                   ENDSR
062600060719      **?------------------------------------------------------------------ */
062700000201      * Scrivo/aggiorno DATA ARRIVO - Utilizzo spedizione FNARB
062800060719      **?------------------------------------------------------------------ */
062900000201     C     ELAB_ars      BEGSR
063000000201      *
063100051017     C                   CLEAR                   FIARS000
063200000201      *
063300051017     c     Kars          chain     FIARS000
063400000201      *
063500000201     c                   If        §DIRcan <> *blanks
063600000201     c                   eval      ARScan = §dircan
063700000201     c                   Endif
063800000201      *
063900060616     c                   If        ARSdat > wi5dat  or  ARSdat = *zeros
064000060616     c                   eval      ARSdat = wi5dat
064100000201     c                   Endif
064200000201      *
064300060620     c                   IF        NOT %FOUND(fiars01l)
064400000203     c                   z-add     wARBfls       ARSfls
064500000203     c                   z-add     wARBlna       ARSlna
064600000203     c                   z-add     wARBnrs       ARSnrs
064700000203     c                   z-add     wARBncd       ARSnsc
064800000201     c                   movel     Ktrcs         ARStrc
064900000201      *
065000051017     C                   WRITE     FIARS000
065100051017     C                   FEOD      FIARS01L
065200000201      *
065300000201     C                   ELSE
065400051017     C                   UPDATE    FIARS000
065500000201     c                   ENDIF
065600000201      *
065700000201     C                   ENDSR
065800060719      **?------------------------------------------------------------------ */
065900060628      * Aggiorna l'ultimo MIC su EVB con la distinta e il Depot
066000060719      **?------------------------------------------------------------------ */
066100060628     C     MIC_evb_aggio BEGSR
066200000131      *
066300060628     c     Kspe          setgt     FNEVB000
066400060628     c     Kspe          readPE    FNEVB000
066500060628     C                   DOW       NOT %EOF(fnevb01l)
066600060628      *
066700060719      **? Solo sull'ultimo evento MIC : serve per agganciare Track & Trace
066800060719      **?    e Firma su Internet
066900060628     c                   IF        evbCEV = 'MIC'
067000060628     c                   movel     EVBnot        Devb01
067100060707     c                   if        i5podImag = 0 and §NOTndc = *blank
067200060707     c                   move      *zeros        §NOTndc
067300060707     c                   end
067400060628     c                   if        i5podImag > 0
067500060628     c                   move      *zeros        §NOTndc
067600060628     c                   move      i5podIMAG     §NOTndc
067700060628     c                   end
067800060628     c                   if        i5depCODE <> *blank
067900060628     c                   movel     i5depCODE     §NOTdep
068000060628     c                   end
068100060628     c                   movel     Devb01        EVBnot
068200060628     c                   update    fnevb000
068300060628      * poi esce dal giro
068400060628     c                   leave
068500060628      *
068600060628     c                   ENDIF
068700060628      *
068800060628     c     Kspe          readPE    FNEVB000
068900060628     C                   ENDDO
069000060628      *
069100060628     C                   ENDSR
069200060719      **?------------------------------------------------------------------ */
069300060628      * Scrivo EVENTO - Utilizzo spedizione FNARB
069400060719      **?------------------------------------------------------------------ */
069500060628     C     ELAB_evb      BEGSR
069600060628      *
069700000201     c                   eval      Writeevb = 'S'
069800000201      *
069900060719      **? Se evento "UNICO" controllo che non sia già stato scritto
070000060719      **?  in assoluto o nella data
070100000209     c                   IF        §DIRevu <> *blanks
070200000201     c     Kspe          setll     FNEVB000
070300000201     c     Kspe          reade     FNEVB000
070400060620     C                   DOW       NOT %EOF(fnevb01l)
070500000209      *
070600000209     C                   SELECT
070700000209     c                   WHEN      §DIRcev <> EVBcev
070800160414      *** in assoluto
070900000209     c                   WHEN      §DIRevu =  'S'
071000000209     c                   eval        Writeevb = 'N'
071100160414      *** a giornata
071200060616     c                   WHEN      EVBdev = wi5dat
071300160414     c                              and §DIRevu = 'D'
071400000209     c                   eval        Writeevb = 'N'
071500000209     c                   ENDSL
071600000209      *
071700160414      * se NON deve scriverlo esce subito  (era dal 2000/02 che il test era invertito!!)
071800160414     c****               IF        Writeevb = 'S'
071900160414     c                   IF        Writeevb = 'N'
072000000209     c                   leave
072100000209     c                   ENDIF
072200000209      *
072300000201     c     Kspe          reade     FNEVB000
072400000201     C                   ENDDO
072500000201     c                   ENDIF
072600000131      *
072700000201     c                   IF        Writeevb = 'S'
072800000201     c                   clear                   FNEVB000
072900000201     c                   z-add     dateu         EVBdtv
073000000201     c                   z-add     wora          EVBorv
073100020910      *
073200020910      * P.O.Utente e Codice Utente e non più la LNA
073300021114     C                   Z-ADD     O36pou        EVBFLE
073400021114     C                   Z-ADD     O36cou        EVBCDU
073500050509      *
073600050509      *  Se sede prende la LNA
073700050509     c                   if        evbfle = 46
073800050509     c                   z-add     WarbLNA       EVBfle
073900050509     c                   end
074000000908      * -----------------                  -------------------
074100000908      *
074200000201     c                   z-add     Klnp          EVBlnp
074300000201     c                   z-add     Kaas          EVBaas
074400000201     c                   z-add     Knrs          EVBnrs
074500000201     c                   z-add     Knsp          EVBnsp
074600060616     c                   z-add     wi5dat        EVBdev
074700060616     c                   if        wi5ora = *All'0'
074800031205     c                   z-add     0001          EVBhev
074900060710     c                   else
075000060710     c                   movel     wi5ora        EVBhev
075100031205     c                   end
075200000203     c                   movel     §DIRcev       EVBcev
075300060628     c                   z-add     dateu         EVBdtc
075400060628      *
075500060628      *  Memorizza comunque il Depot che ha fatto lo SCAN
075600060707      *******
075700060707     C                   if        EVBCEV <> 'MIC'
075800060628     c                   if        i5depCODE <> *blank
075900131031     c                   movel     EVBnot        Devb01
076000131031     c*****  Prima veniva allineato a sinistra,
076100131031     c*****    adesso lo imposta sempre sul suo campo
076200131112      **
076300131112      ****** DA ELIMINARE in seguito dopo il test
076400131112     c                   movel(p)  i5depCODE     §NOTndc
076500131112      ****** DA ELIMINARE in seguito dopo il test
076600131112      **
076700131112      *
076800131031     c                   movel     i5depCODE     §NOTdep
076900131031     c                   movel     Devb01        EVBnot
077000060628     c                   end
077100060707     c                   end
077200000204      *
077300060623      * Alla scrittura del "MIC" adesso memorizzaimo solo il numero distinta POD
077400060628      *  ed il Depot a parte seguendo la nuova DS
077500060623     C                   if        EVBCEV = 'MIC'
077600060628     c                   clear                   Devb01
077700060628     c                   move      *zeros        §NOTndc
077800060628     c                   move      i5podIMAG     §NOTndc
077900060628     c                   movel     i5depCODE     §NOTdep
078000060628     c                   movel     Devb01        EVBnot
078100060623     C                   end
078200010717      *
078300000201     C                   WRITE     FNEVB000
078400000210     C                   FEOD      FNEVB01L
078500000201      *
078600000201     c                   ENDIF
078700000201      *
078800000131     C                   ENDSR
078900060719      **?------------------------------------------------------------------ */
079000000202      * Scrivo FNCDE - Utilizzo spedizione FNLBL se esiste
079100060719      **?------------------------------------------------------------------ */
079200000202     C     ELAB_cde      BEGSR
079300000202      *
079400000202     C                   clear                   FNCDE000
079500000202     C                   clear                   WCDESTS
079600000202     C                   clear                   WCDECOD
079700170310     C                   movel     'S'           Write_CDE         1
079800000202      *
079900000202     c                   IF        KFnsp > *zeros
080000000202     c     KFcde         chain     FNCDE000
080100000202     c                   ELSE
080200000202     c     Kcde          chain     FNCDE000
080300000202     c                   ENDIF
080400000202      *
080500060620     C                   IF        %FOUND(fncde02l)
080600000202     C                   eval      Wcdests = cdests
080700000202     C                   eval      Wcdecod = cdecod
080800000202     c                   ENDIF
080900000202      *
081000060719      **? Elaboro se la data evento è maggiore di quelle presenti nel file
081100060616b2   c                   IF        wi5dat >= CDEDCM  and  wi5dat >= CDEDAG
081200060616     c                                               and  wi5dat >= CDEDLA
081300000203      *
081400060719      **? Imposto evento mancata consegna
081500060616b5   c                   IF           (§DIRtpe <> 'CO' and wi5dat <> CDEDAG)
081600000202     c                             or (§DIRtpe <> 'LA' and §DIRtpe <> 'CO')
081700000203     C                   eval      CDEcmc = §DIRcev
081800000202e5   C                   ENDIF
081900000202      *
082000060719      **? Imposto date evento
082100000202     C                   SELECT
082200060719      **? CONSEGNATA
082300060616     C                   WHEN      §DIRTPE = 'CO'  and  CDEdcm < wi5dat
082400060616     C                   eval      CDEDCM = wi5dat
082500060616     c                   if        wi5ora = *All'0'
082600031205     C                   eval      CDEHMC = 0001
082700031205     c                   else
082800060710     c                   movel     wi5ora        CDEHMC
082900031205     c                   end
083000140410      * imposta la Firma di chi ha ritirato la spedizione
083100140410     c                   if        i5recNAME <> *blank
083200140410     c                   eval      cdeNO2 = i5recNAME
083300140410     c                   end
083400140410      *
083500060719      **? RISERVA
083600000202     C                   WHEN      §DIRtpe = 'RI'
083700060616     C                   eval      CDEDRC = wi5dat
083800060616     c                   if        wi5ora = *All'0'
083900031205     C                   eval      CDEHMC = 0001
084000031205     c                   else
084100060710     c                   movel     wi5ora        CDEHMC
084200031205     c                   end
084300060719      **? LASCIATO AVVISO
084400000202     C                   WHEN      §DIRtpe = 'LA'
084500060616     C                   eval      CDEDLA = wi5dat
084600060616     c                   if        wi5ora = *All'0'
084700031205     C                   eval      CDEHMC = 0001
084800031205     c                   else
084900060710     c                   movel     wi5ora        CDEHMC
085000031205     c                   end
085100060719      **? GIACENZA
085200000202     C                   WHEN      §DIRtpe = 'GI'
085300060616     C                   eval      CDEDAG = wi5dat
085400060616     c                   if        wi5ora = *All'0'
085500031205     C                   eval      CDEHMC = 0001
085600031205     c                   else
085700060710     c                   movel     wi5ora        CDEHMC
085800031205     c                   end
085900060719      **? DANNI
086000000202     C                   WHEN      §DIRtpe = 'DA'
086100060719      **? ARRIVO
086200000202     C                   WHEN      §DIRtpe = 'AR'
086300060719      **? PARTICOLARITA' CONSEGNA
086400000209     C                   WHEN      §DIRtpe = 'CP'
086500170310     C                   movel     'N'           Write_CDE         1
086600000202      *
086700000202     C                   ENDSL
086800000202      *
086900060719      **? Blocco in distinta
087000000203     C                   eval      CDEerr = §DIRbld
087100000203      *
087200060719      **? Descrizione
087300000203     C                   eval      CDEmot = §DIRdes
087400000209      *
087500060719      **? Status
087600000209     C                   SELECT
087700000209     C                   WHEN      XX1 > *zeros
087800060619     c                   movel     i5scant       cdests
087900060616     C                   movel     ser(sr)       cdecod
088000000221     C                   WHEN      XX2 > *zeros
088100000209     C                   clear                   cdests
088200060616     C                   movel     ser(sr)       cdecod
088300000221     C                   WHEN      XX3 > *zeros
088400060619     c                   movel     i5scant       cdests
088500000221     C                   clear                   cdecod
088600000209     C                   ENDSL
088700000209      *
088800060719      **? Consegna particolare
088900000210     C                   IF        §DIRtc1 <> *blanks and CDEtc1 = *blanks
089000170310      ** attenzione se si tratta del FUORI MISURA
089100170310      ** e la spedizione è sotto i 3 kg. --> NON DEVE avere il FUORI MISURA
089200170310     c                   if        §dirtc1 <> 'F'
089300170310     c                              or
089400170310     c                             §dirtc1  = 'F' and (arbpkc>3 or arbpkb>3)
089500000210     c                   eval      cdetc1 = §dirtc1
089600170310     C                   movel     'S'           Write_CDE         1
089700170310     c                   end
089800000210     c                   ENDIF
089900000203      *
090000060719      **? Se ho impostato data consegna e data giacenza abblenco la giacenza
090100000203     C                   IF        CDEDCM >  CDEDAG
090200000203     C                   clear                   CDEDAG
090300000203     C                   IF        CDECMC <> 'MAN' and CDECMC <> 'P  ' and
090400000203     C                             CDECMC <> 'RES' and CDECMC <> 'IDD' and
090500000203     C                             CDECMC <> 'RIT'
090600000203     C                   clear                   CDECMC
090700000203     C                   ENDIF
090800000203     C                   ENDIF
090900000203      *
091000060719      **? Se ho impostato data consegna e data lasc.avviso abblenco lasc.avv.
091100000203     C                   IF        CDEDCM > CDEDLA
091200000203     C                   clear                   CDEDLA
091300000203     C                   IF        %subst(CDECMC:1:2) = 'AV'
091400000203     C                   clear                   CDECMC
091500000203     C                   ENDIF
091600000203     C                   ENDIF
091700000203      *
091800060719      **? Aggiorno FNCDE
091900000203     C                   IF        %FOUND(fncde02l)
092000090721      *
092100000203     C                   update    FNCDE000
092200090721      *
092300000203     C                   ELSE
092400090721      *
092500000203     C                   eval      CDEaas = ARBaas
092600000203     C                   eval      CDElnp = ARBlnp
092700000203     C                   eval      CDEnrs = ARBnrs
092800000203     C                   eval      CDEnsp = ARBnsp
092900000203     C                   eval      CDElna = ARBlna
093000060720      ***
093100060616     C                   clear                   CDErmn
093200060720      ***
093300060720      ** controlla lunghezza del Parcel
093400060720     c                   clear                   Lung_Parcel
093500060720     c                   eval      Lung_Parcel = %Len(%trim(i5PARCEL))
093600060720      ***
093700060720      * ?   ( Il parcel non è + numerico )
093800060720      *  se comunque il Parcel ricevuto è numerico impostiamo nel riferimento
093900060720      *  numerico il nr.Parcel come avveniva prima che diventasse alfa il campo
094000060720     c     digits        check     i5PARCEL      position
094100060720      **
094200060720      **  presenti campi non numerici: (Ossia trovati)
094300060720     c                   iF        %Found
094400060720      **  comunque lo inposto a (1)
094500060720     c                   z-add     1             cdermn
094600060720      **  per impostare il riferimento numerico (cdeRMN):
094700060720      **    è importante che siano oltre il limite della lunghezza del Parcel
094800060720      **    in tal caso sono solo i blanks alla destra del numero
094900060720     c                   if        Lung_Parcel = 11 and position > 11
095000060720     C                   MOVEL     i5PARCEL      parcel11
095100060720     c                   move      parcel11      cdermn
095200060720     c                   end
095300060720      **
095400060720     c                   if        Lung_Parcel = 14 and position > 14
095500060720     C                   MOVEL     i5PARCEL      parcel14
095600060720     c                   move      parcel14      cdermn
095700060720     c                   end
095800060720      **
095900060720     c                   elsE
096000060720      ** è solo numerico
096100060720     c                   if        Lung_Parcel = 11
096200060720     C                   MOVEL     i5PARCEL      parcel11
096300060720     c                   move      parcel11      cdermn
096400060720     c                   end
096500060720     c                   if        Lung_Parcel = 14
096600060720     C                   MOVEL     i5PARCEL      parcel14
096700060720     c                   move      parcel14      cdermn
096800060720     c                   end
096900060720      **
097000060720     c                   enD
097100060720      **
097200170310      ** condizionato per i Fuori Misura
097300170310     C                   if         Write_CDE =  'S'
097400000203     C                   write     FNCDE000
097500000204     c                   feod      FNCDE02L
097600170310     c                   end
097700090721      *
097800000203     C                   ENDIF
097900000203      *
098000000203     C                   ELSE
098100000203     C                   ENDIF
098200000203      *
098300000202     C                   ENDSR
098400060719      **?------------------------------------------------------------------ */
098500060719      *   DEFINIZIONE CHIAVI
098600060719      **?------------------------------------------------------------------ */
098700940321     C     *INZSR        BEGSR
098800940321      *
098900991129      *
099000000201     C     KARS          KLIST
099100000203     C                   KFLD                    wARBFLS
099200000203     C                   KFLD                    wARBLNA
099300000203     C                   KFLD                    wARBNRS
099400000203     C                   KFLD                    wARBNCD
099500000201     C                   KFLD                    ktrcs
099600991129      *
099700000131     C     KSPE          KLIST
099800000201     C                   KFLD                    kAAS
099900000201     C                   KFLD                    kLNP
100000000201     C                   KFLD                    kNRS
100100000201     C                   KFLD                    kNSP
100200000203      *
100300000203     C     KFSPE         KLIST
100400000203     C                   KFLD                    kfAAS
100500000203     C                   KFLD                    kfLNP
100600000203     C                   KFLD                    kfNRS
100700000203     C                   KFLD                    kfNSP
100800090831      *
100900090831     C     KFSPE_P       KLIST
101000090831     C                   KFLD                    kAAS_P
101100090831     C                   KFLD                    kLNP_P
101200090831     C                   KFLD                    kNRS_P
101300090831     C                   KFLD                    kNSP_P
101400000201      *
101500000202     C     KFCDE         KLIST
101600000202     C                   KFLD                    KNDC
101700000201     C                   KFLD                    kfAAS
101800000201     C                   KFLD                    kfLNP
101900000201     C                   KFLD                    kfNRS
102000000201     C                   KFLD                    kfNSP
102100000202      *
102200000202     C     KCDE          KLIST
102300000202     C                   KFLD                    KNDC
102400000202     C                   KFLD                    kAAS
102500000202     C                   KFLD                    kLNP
102600000202     C                   KFLD                    kNRS
102700000202     C                   KFLD                    kNSP
102800060721      *
102900060721     C     KCDEdp5       KLIST
103000060721     C                   KFLD                    KNDC0
103100060721     C                   KFLD                    dp5AAS
103200060721     C                   KFLD                    dp5LNP
103300060721     C                   KFLD                    dp5NRS
103400060721     C                   KFLD                    dp5NSP
103500060721      *
103600060721     C     KARBdp5       KLIST
103700060721     C                   KFLD                    dp5AAS
103800060721     C                   KFLD                    dp5LNP
103900060721     C                   KFLD                    dp5NRS
104000060721     C                   KFLD                    dp5NSP
104100160909      *
104200160909     C     KARB          KLIST
104300160909     C                   KFLD                    arbAAS
104400160909     C                   KFLD                    arbLNP
104500160909     C                   KFLD                    arbNRS
104600160909     C                   KFLD                    arbNSP
104700170922      *
104800060630     C     KTBE          KLIST
104900060630     C                   KFLD                    tbeCOD
105000060630     C                   KFLD                    tbeKE1
105100020924      *
105200020924      *  Decodifica il profilo generico EDPCED in base alla LNA e quindi al Terminal
105300020924      *   EDPCEDxxx dove xxx è il Terminal della LNA
105400021114     C                   CLEAR                   Tibs36Ds
105500060707     c                   EVAL      I36ute = 'EDPCED'
105600021114     c                   EVAL      I36Tla = 'L'
105700021114     C                   CALL      'TIBS36R'
105800021114     C                   PARM                    Tibs36Ds
105900991124      *
106000000221      * Caricamento Tabella DIR e schiera a parte per cancellazione scan
106100000218     c                   clear                   xx
106200000221     c                   clear                   yy
106300160928     c                   clear                   dd
106400000131     c     'DIR'         chain     tntbe000                           31
106500991124      *
106600991124     c                   DOW       *in31 = *off
106700991124      * Se il S.I. è indicato deve essere uguale al mio
106800991124     c                   IF        TBEatb = *BLANKS  and
106900991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
107000000218     c                   add       1             xx
107100000218     c                   eval      Kdir(XX) = TBEke1 + TBEke2
107200000218     c                   movel     TBEuni        Dir(XX)
107300000221     c                   movel     TBEuni        DDIR
107400000221      *
107500000221     C                   if        §DIRc2e = 'S'
107600000221     c                   add       1             YY
107700000221     c                   movel     TBEke1        Kc2e(YY)
107800991124     c                   endif
107900160928      **
108000160928      * deve generare Bolla di recupero x dirottamento
108100160928     C                   if        §DIRcbr = 'S'
108200160928     c                   add       1             dd
108300160929     c                   eval      Kcbr(dd) =
108400160929     c                              %subst(TBEke1:1:3) + %subst(TBEke2:1:3)
108500160929     c                   eval      KcbrCEV(dd) = §DIRCEV
108600160928     c                   endif
108700160928      *
108800000221     c                   endif
108900000131     c     'DIR'         reade     tntbe000                               31
109000991124     c                   enddo
109100000908      *
109200971216      * Recupero data e ora
109300971216     C                   TIME                    WORA
109400991124     C                   TIME                    W0140
109500991124      * UDATE IN GGMMAAAA
109600991124     C                   MOVE      W0140         WDTGIO
109700991124      * UDATE IN AAAAMMGG
109800000131     C     *eur          MOVEL     WDTGIO        Wdataoggi
109900000131     C     *iso          MOVEL     Wdataoggi     dateu
110000010219      *
110100060616     C                   ENDSR
110200060719      **?------------------------------------------------------------------ */
110300010219      *  Giro di aggiornamento del FNARB00F per data Arrivo Merce
110400060719      **?------------------------------------------------------------------ */
110500010219     C     AGG_ARBDAM    BEGSR
110600060630      *
110700060630      *   Attenzione: questa routine deve essere eseguita sempre in base
110800060630      *    a scan che indicano che il Pacco è stato lavorato da DPD.
110900060630      *  Eventuali altri SCAN che hanno altro tipo di significato (Vedi ORM Scan 18.....)
111000060630      *   non devono aggiornare i nostri archivi.
111100010219      *
111200060616     C                   move      wi5dat        Wdat5Dsc
111300060616     C     Wdat5dsc      subdur    1:*d          Wdat5Dsc
111400010220      * pulizia flag promemoria
111500010220      *
111600010219      *  Aggiorna ARB se manca la data arrivo merce.
111700010219      *   viene aggiornata con un giorno di anticipo rispetto allo scan DPD
111800010219     C                   If        %found(FNARB01L)
111900010219     C                             and ARBDAM=0 and ARBDTI=0
112000010220      * serve per controllare quali records sono stati aggiornati
112100060616     C                   MOVE      wdat5dsc      ARBDAM
112200010219     C                   UPDATE    FNARB000
112300010219      *
112400010219     C                   ENDIF
112500060630      *
112600160907     C                   ENDSR
112700160907      **?------------------------------------------------------------------ */
112800160907      *  Crea Bolla di recupero
112900160907      **?------------------------------------------------------------------ */
113000160907     C     Elab_CBR      BEGSR
113100160907      *
113200161017      **?   Se trovato ar4 con il flag BOLLA di RECUPERO deve essere fatta
113300161017      **?    una sola bolla di recupero quindi controlla tramite il flag
113400161017      **?     che non sia stata già fatta.
113500170925     c                   if        trovato_PND = 'S' and
113600170922     c                             §PNDRECDIR <> *blank
113700161014      * Se c'era già una bolla di recupero          va a fine SR
113800161017      *     e NON esegue un'altra bolle di recupero
113900161014     c                   leavesr
114000161014     c                   end
114100160909      *
114200160909      * Scrive una bolla di recupero su DIROTTAMENTI segnalati da DPD
114300160909      * che verranno poi addebitati sul clearing
114400160907     c                   call      'FIEU18R2C'
114500161130     c                   parm                    FIEU18
114600160907     c                   parm                    fnarb00
114700160907     c                   parm                    KPJBA
114800160907      *
114900010219     C                   ENDSR
115000160928      **?------------------------------------------------------------------ */
115100160928      *  Crea Bolla di recupero
115200160928      **?------------------------------------------------------------------ */
115300160928     C     AGG_PESO      BEGSR
115400160928      *
115500160928      *  se c'è un peso sullo SCAN
115600170504      *    aggiunto il controllo che deve essere diverso da 9 kg.
115700170504      *    causa HUB Francia 1043 che imposta 900 di default (sembrerebbe)
115800170504     C                   if        I5WEIGHT  > 0 and I5WEIGHT <> 9
115900160928     c                   unlock    fnarb01l                             99
116000160928     c                   clear                   fieu18r1DS
116100160928     c                   eval         I18AAS     = arbAAS
116200160928     c                   eval         I18LNP     = arbLNP
116300160928     c                   eval         I18NRS     = arbNRS
116400160928     c                   eval         I18NSP     = arbNSP
116500160928     c                   z-add     I5WEIGHT      I18WEIGHT
116600160928      **
116700161003     c                   call      'FIEU18R1C'
116800160928     c                   parm                    fieu18r1ds
116900160928     c                   end
117000160928      *
117100160928     C                   ENDSR
117200060719      **?------------------------------------------------------------------ */
